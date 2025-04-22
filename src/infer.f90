!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2025
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.


! =============================================================
! Inter-procedure feed-forward type inference
! - determine type of each variable (incl. temporaries)
! - flag recursive procedures
! - propagate 'taints' such as impurity
! - create a vector of integer resolution values
!   for each procedure s.t. vect[index] contains resolution
!   information for node (block, call or variable)
!   associated with that index
! =============================================================

module pm_infer
  use pm_sysdep
  use pm_compbase
  use pm_kinds
  use pm_memory
  use pm_hash
  use pm_lib
  use pm_symbol
  use pm_types
  use pm_cnodes
  use pm_codegen
  use pm_vmdefs
  implicit none

  ! Print compiler debugging messages
  logical,parameter:: debug_inference=.false.
  
  ! Maximum times a procedure template can call itself with
  ! *different* arguments types each time
  integer,parameter:: max_recur=32
 
  ! Special types
  integer,parameter:: undefined=-1
  integer,parameter:: error_type=-2

  ! Parallel modes
  integer,parameter:: par_mode_outer=1
  integer,parameter:: par_mode_multi_node=2
  integer,parameter:: par_mode_single_node=3
  integer,parameter:: par_mode_conc=4
  integer,parameter:: par_mode_inner=5

  private:: get_var_type,get_arg_type

contains
  
  !============================================================
  ! The following routines process the intermediate code tree 
  ! applying type inference and resolving polymorphic procedure 
  ! calls at compile time
  !=============================================================

  !==============================
  ! Type-infer main program
  !============================== 
  subroutine inf_prog(coder)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: cnode
    integer:: i

    if(debug_inference) write(*,*) 'INF PROG>'

    coder%flag_recursion=.false.
    coder%trace_depth=0
    coder%poly_cache=pm_dict_new(coder%context,32_pm_ln)
    coder%first_pass=.true.
   
    do
       coder%top=1
       coder%wtop=1
       coder%types_finished=.true.
       coder%redo_calls=.false.
       coder%incomplete=.false.
       coder%taints=0
       
       coder%proc_cache=pm_dict_new(coder%context,32_pm_ln)
      
       ! Setup resolution stack block
       call create_stack_frame(coder,0,coder%index,0)
       
       ! Process program code
       call inf_cblock(coder,top_code(coder))

       ! Uncaught break implies infinite recursion
       if(coder%incomplete) then
          if(coder%num_errors==0) then
             call more_error(coder%context,&
                  'Error: A procedure in this program has infinite recursion')
             coder%flag_recursion=.true.
             call inf_cblock(coder,top_code(coder))
             call pm_stop('Program contains infinite recursion')
          endif
       endif
       if(debug_inference) write(*,*) 'FULL PASS FINISHED>',coder%types_finished
       if(coder%types_finished) exit
       coder%first_pass=.false.
    enddo
    
    ! Create resolved code object
    call code_int_vec(coder,coder%stack,coder%base,coder%top)
    call code_num(coder,coder%stack(2))
    call make_code(coder,pm_null_obj,cnode_is_resolved_proc,3)

    if(debug_inference) write(*,*) 'END OF PROG> vtop=',coder%vtop

  contains
    include 'fnewnc.inc'
    include 'ftiny.inc'
  end subroutine  inf_prog

  ! ====================================================
  ! Type-infer procedure
  ! Returns signature index as tiny int in on vstack
  ! ====================================================
  function inf_proc(coder,procnode,callnode,atype,ptype,nret,nkeys,&
       keynames,keybase,proc_nkeys,nomatch,only_when) result(rtype)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: procnode,callnode
    integer,intent(in):: atype,ptype
    integer,intent(in):: nret,nkeys,keybase,proc_nkeys
    logical,intent(in):: only_when
    logical,intent(out):: nomatch
    type(pm_ptr),intent(in):: keynames
    type(pm_ptr):: cnode,cac
    integer:: rtype
    integer:: at
    integer,dimension(4+proc_nkeys):: key
    integer:: i,j,keysize,nk,tno
    integer(pm_ln):: k
    logical:: save_redo_calls,save_incomplete
    integer:: taints,save_taints
    integer:: keypartyp,keyargtyp,last_key_index,sp_code
    type(pm_ptr):: save_procnode, keys
    logical:: iscomm
    
    taints=0

    if(pm_debug_checks) then
       if(cnode_get_kind(procnode)/=cnode_is_proc) then
          call pm_panic('procnode-proc procnode not proc')
       endif
    endif

    ! If this is an abstract proc then raise an error
    if(cnode_flags_set(procnode,pr_flags,proc_is_abstract)) then
       call inf_error(coder,callnode,&
            'Abstract procedure needs to be implemented for the given argument list')
       call inf_error(coder,procnode,&
            'Abstract procedure definition referenced in the above error')
       call inf_trace(coder)
       rtype=error_type
       return
    endif

    iscomm=cnode_flags_set(procnode,pr_flags,proccall_is_comm)

    ! Dictionary entries in coder%proc_cache:
    ! Key is proc and argument types 
    ! Value is tiny int with procedure return type (if >0)
    ! or (-1) sp_in_process in process of resolution
    ! or (-2) sp_recursive called recursively
    ! or (-3) sp_break  breaking (or previously broke) out of inference

    ! Is this combination already cached?
    key(1)=cnode_get_num(procnode,pr_id)
    key(2)=atype
    keysize=2
        
    ! Process keyword arguments - they form part of the hash key
    last_key_index=0
    if(proc_nkeys>0) then
       keys=cnode_get(procnode,pr_keys)
       last_key_index=keys%data%i(keys%offset+pm_fast_esize(keys))
       call new_stack_frame(coder,cnode_get_num(procnode,pr_max_index))
       call init_stack_frame(coder,coder%base,1,coder%base+last_key_index,atype,0)
       call  inf_key_args(coder,callnode,procnode,atype,&
            nkeys,keynames,keybase,key(3:),nk)
       keysize=keysize+nk
    elseif(nkeys>0) then
       call inf_error(coder,callnode,&
            'Keyword arguments in call to procedure that does not take any')
       call inf_error_with_trace(coder,procnode,&
            'Procedure definition corresponding to the above error')
    endif

    ! Process when expression
    nomatch=.false.
    if(.not.pm_fast_isnull(cnode_get(procnode,pr_when))) then
       if(proc_nkeys==0) call new_stack_frame(coder,cnode_get_num(procnode,pr_max_index))
       call init_stack_frame(coder,coder%base,1,coder%base+last_key_index,atype,0)
       call inf_arg_types(coder,procnode,atype)
       call inf_cblock(coder,cnode_get(procnode,pr_when))
       tno=get_arg_type(coder,callnode,cnode_get(procnode,pr_whenvar))
       if(tno==coder%false_fix.or.tno==coder%false_literal) then
          call pop_stack_frame(coder)
          nomatch=.true.
          return
       elseif(tno/=coder%true_fix.and.tno/=coder%true_literal) then
          call inf_error(coder,procnode,&
               '"when" expression must have a fixed or literal bool value')
          call more_error(coder%context,'Type of expression is: '//&
               trim(pm_type_as_string(coder%context,tno)))
       endif
    endif

    if(only_when) then
       call pop_stack_frame(coder)
       nomatch=.false.
       return
    endif

    ! Lookup combination of proc, arg types and all key types
    ! defined for the procedure (including defaults)
    k=pm_ivect_lookup(coder%context,coder%proc_cache,key,keysize)

    if(debug_inference) then
       write(*,*) 'INF PROC>',key(1),key(2),k,&
            trim(pm_name_as_string(coder%context,&
            cnode_get_name(procnode,pr_name))),&
            trim(pm_type_as_string(coder%context,atype))
    endif

    
    ! This combination already cached
    if(k>0) then
       cnode=pm_dict_val(coder%context,coder%proc_cache,k)
       
       if(debug_inference) then
          write(*,*) 'FOUND',k,'-->',key(1:keysize)
          write(*,*) 'CACHED>',k,cnode%data%vkind,cnode%offset,&
               trim(pm_name_as_string(coder%context,&
               cnode_get_name(procnode,pr_name))),sp_sig_recursive,sp_sig_in_process
       endif

       ! One of the special in-progress codes
       if(pm_fast_istiny(cnode)) then
          sp_code=cnode%offset
          if(sp_code==sp_sig_break) then
             goto 10
          elseif(sp_code==sp_sig_recursive) then
             if(coder%flag_recursion) then
                call inf_error(coder,procnode,'Recursive call to: '//&
                     trim(pm_name_as_string(coder%context,&
                     cnode_get_name(procnode,pr_name))))
                call inf_trace(coder)
                coder%flag_recursion=.false.
             endif
             coder%incomplete=.true.
             rtype=error_type
          elseif(sp_code==sp_sig_in_process) then
             call pm_dict_set_val(coder%context,coder%proc_cache,&
                  k,pm_fast_tinyint(coder%context,sp_sig_recursive))
             if(coder%flag_recursion) then
                call inf_error(coder,procnode,'Recursive call to: '//&
                     trim(pm_name_as_string(coder%context,&
                     cnode_get_name(procnode,pr_name))))
                call inf_trace(coder)
                coder%flag_recursion=.false.
             endif
             coder%incomplete=.true.
             rtype=error_type
          elseif(sp_code<0) then
             ! Another special sig
             rtype=atype
             call code_num(coder,int(sp_code))
          else
             ! Return type
             rtype=sp_code
             if(debug_inference) write(*,*) 'CACHED RETURN>',rtype
             call code_num(coder,int(k))
          endif
       else

          ! Not a special code so have a fully inferred procedure

          ! Pass out taints
          taints=cnode_num_arg(cnode,3)
          coder%taints=ior(coder%taints,iand(taints,proc_taints))

          ! Push signature
          call code_num(coder,int(k))

          ! Get return type
          cnode=cnode_arg(cnode,2)
          rtype=cnode%data%i(cnode%offset)
          if(nret==0) rtype=0
          if(debug_inference) write(*,*) 'CACHED RTYPE>',rtype
       endif
       if(proc_nkeys>0) call pop_stack_frame(coder)
       return
    endif

10  continue


    ! Proc is not (or not yet fully) inferred
    at=atype

    ! Check for infinite recursion with changing arg types
    if(cnode_get_num(procnode,pr_recurse)>max_recur) then
       call inf_error_with_trace(coder,procnode,&
            'Recursion appears to require infinite types')
       call code_num(coder,0)
       return
    endif

20  continue

    ! Flag call to check for recursion
    call cnode_incr_num(procnode,pr_recurse,1)
    k=pm_idict_add(coder%context,coder%proc_cache,&
         key,keysize,pm_fast_tinyint(coder%context,sp_sig_in_process))

    ! Repeatedly type infer until complete
    save_incomplete=coder%incomplete
    save_taints=coder%taints

    if(proc_nkeys==0.and.pm_fast_isnull(cnode_get(procnode,pr_when))) then
       call new_stack_frame(coder,cnode_get_num(procnode,pr_max_index))
    endif
    
    do
       if(debug_inference) write(*,*) 'TRY>',key(1),key(2),rtype

       call init_stack_frame(coder,coder%base,last_key_index+1,coder%top,at,taints)

       ! Process code
       coder%incomplete=.false.
       coder%taints=taints
       save_procnode=coder%proc
       coder%proc=procnode
       call inf_cblock(coder,cnode_get(procnode,pr_cblock))
       coder%proc=save_procnode

       ! Check  procedure record for recursion/completion
       cnode=pm_dict_val(coder%context,coder%proc_cache,k)
       if(.not.pm_fast_istiny(cnode)) then
          write(*,*) cnode%data%vkind,k
          call pm_panic('procnode-proc bad cache')
       endif

       if(debug_inference) then
          write(*,*) 'TRY COMPLETE>',cnode%offset,&
               coder%stack(coder%base),coder%stack(coder%base-1),nret
       endif

       sp_code=cnode%offset
       if(sp_code==sp_sig_in_process) then
          ! Not recursively called
          rtype=coder%stack(coder%base)
          if(nret==0) rtype=0
          if(debug_inference) write(*,*) 'NOT RECURSIVE>',rtype,coder%incomplete
          exit
       else if(sp_code<=sp_sig_recursive) then
          ! Recursively called
          if(nret==0) coder%stack(coder%base)=0

          if(coder%stack(coder%base)<0) then
             ! No resolved type yet 
             ! flag cache entry
             ! and break out
             call pop_stack_frame(coder)
             sp_code=sp_sig_break
             call pm_dict_set_val(coder%context,&
                  coder%proc_cache,k,cnode)
             coder%incomplete=.true.
             coder%taints=save_taints
             rtype=error_type
             if(debug_inference) write(*,*) 'NOT RESOLVED>'
             return
          endif

          ! Flag procedure as recursive
          coder%taints=ior(coder%taints,proc_is_recursive)

          ! Cache resolved return type
          sp_code=coder%stack(coder%base)
          call pm_dict_set_val(coder%context,coder%proc_cache,k,cnode)
       else
          ! Recursive call for which we 
          ! already have a return type
          ! check against type just returned
          if(debug_inference) write(*,*) 'RT>',rtype,coder%stack(coder%base)
          rtype=sp_code

          if(debug_inference) write(*,*) 'RECURSIVE WITH TYPE>',&
               trim(pm_type_as_string(coder%context,rtype)),&
               trim(pm_name_as_string(coder%context,cnode_get_num(procnode,pr_name)))

          ! This error should not happen
          !(implies compiler bug as proc output type determined by args)
          if(nret>0.and.rtype/=coder%stack(coder%base)) then
             call inf_error_with_trace(coder,procnode,&
                  'Internal Compiler Error: Procedure return type changed')
          endif

          ! Flag procedure as recursive
          coder%taints=ior(coder%taints,proc_is_recursive)
          exit
       endif
    enddo

    if(debug_inference) then
       write(*,*) 'COMPLETED>',coder%stack(coder%base),&
            coder%stack(coder%base-1),coder%base
    endif

    ! Pass a break out
    if(coder%incomplete) then
       call pop_stack_frame(coder)
       ! clear cache entry
       cnode%offset=sp_sig_break
       call pm_dict_set_val(coder%context,&
            coder%proc_cache,k,cnode)
       if(rtype>=0) then
          call code_num(coder,int(k))
       endif
       return
    endif

    coder%incomplete=save_incomplete

    ! Flag recursive calls with taints or keyword args as unfinished
    taints=iand(coder%taints,proc_taints)

    ! Create record of type-annotated code
    call code_val(coder,procnode)
    call code_int_vec(coder,coder%stack,coder%base,coder%top)
    call code_num(coder,&
         ior(iand(cnode_get_num(procnode,pr_flags),&
         proccall_is_comm+proccall_is_inline+proccall_is_no_inline),&
         coder%taints))
    call make_code(coder,pm_null_obj,cnode_is_resolved_proc,3)
    cnode=top_code(coder)
    if(debug_inference) then
       write(*,*) 'CACHE AS>',key(1:keysize),'>',cnode%offset
    endif
    k=pm_idict_add(coder%context,coder%proc_cache,&
         key,keysize,cnode)
    call drop_code(coder)

    call code_num(coder,int(k))
    call pop_stack_frame(coder)
    call cnode_incr_num(procnode,pr_recurse,-1)

    ! Pass out taint information
    coder%proc_taints=iand(coder%taints,proc_taints)
    coder%taints=ior(save_taints,coder%proc_taints)

    if(pm_debug_level>3) then
       write(*,*) 'ENDPROCNODE>',key(1),key(2),key(3),key(4),k
    endif

  contains
    include 'fnewnc.inc'
    include 'fistiny.inc'
    include 'ftiny.inc'
    include 'fvkind.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end function inf_proc

  subroutine inf_arg_types(coder,procnode,atype)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: procnode
    integer,intent(in):: atype
    type(pm_ptr):: arglist,tv
    integer:: nargs,totargs,i,j
    arglist=cnode_get(procnode,pr_argcall)
    if(.not.pm_fast_isnull(arglist)) then
       nargs=cnode_numargs(arglist)
       tv=pm_type_vect(coder%context,atype)
       totargs=pm_tv_numargs(tv)
       do i=1,nargs
          call set_var_type(coder,cnode_arg(arglist,i),pm_tv_arg(tv,i))
       enddo
       if(totargs>nargs) then
          call push_word(coder,pm_type_is_tuple)
          call push_word(coder,0)
          j=0
          do i=nargs,totargs
             call push_word(coder,pm_tv_arg(tv,i))
             j=j+1
          enddo
          call make_type(coder,j+2)
          call set_var_type(coder,cnode_arg(arglist,nargs),pop_word(coder))
       endif
    endif
  contains
    include 'fisnull.inc'
  end subroutine inf_arg_types
  
  !=======================================================================
  ! Process keyword arguments
  ! This requires type inference of default expressions before checking
  ! and converting the arguments
  !=======================================================================
  subroutine inf_key_args(coder,callnode,procnode,atype,nkeys,call_keys,key_base,&
       keytypes,n)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,procnode,call_keys
    integer,intent(in):: atype,nkeys,key_base
    integer,intent(out):: keytypes(*),n
    integer i,j,cname,pname,ctype,ptype,dtype,pdtype,mtype
    logical:: nomatch,error
    type(pm_ptr):: callkeys,proc_keys,arglist,tv
    integer:: nargs,totargs,tno
    
    proc_keys=cnode_get(procnode,pr_keys)

    ! Need to infer standard arguments in case they are
    ! used in default expressions
    call inf_arg_types(coder,procnode,atype)
    
    arglist=cnode_get(procnode,pr_keycall)
    n=pm_fast_esize(proc_keys)/2
    keytypes(1:n)=undefined
    callkeys=pm_name_val(coder%context,int(call_keys%offset))

    ! Find matching keyword parameter for each call keyword argument
    ! and set the type for that parameter
    outer: do i=0,nkeys-1
       cname=callkeys%data%i(callkeys%offset+i)
       ctype=coder%wstack(key_base+1+i)
       do j=1,n
          pname=proc_keys%data%i(proc_keys%offset+j-1)
          if(cname==pname) then
             keytypes(j)=ctype
             cycle outer
          endif
       enddo
       call inf_error(coder,callnode,'Keyword argument "'//&
            trim(pm_name_as_string(coder%context,cname))//&
            '" does not match a keyword in the procedure definition')
       call inf_error(coder,procnode,&
            'Procedure definition corresponding to the above error')
    enddo outer
    
    ! For each keyword parameter, infer the default type and then
    ! determine keyword argument matching with any required conversions
    ! -  match_arg will leave conversion records on the vstack
    do i=1,n
       call inf_cblock(coder,cnode_arg(arglist,i*2-1+n+n))

       dtype = pm_type_strip_literal(coder%context,&
            get_arg_type(coder,callnode,cnode_arg(arglist,i*2+n+n)))
  
       if(keytypes(i)>=0) then
          ptype=proc_keys%data%i(proc_keys%offset+i-1+n)
          pdtype=merge(ptype,dtype,ptype>=0)
          mtype=match_arg(coder,callnode,procnode,&
               pdtype,keytypes(i),-i,3,nomatch,error)
          if(nomatch) then
             if(ptype>=0) then
                call inf_error(coder,callnode,'Keyword argument "'//&
                     trim(pm_name_as_string(coder%context,proc_keys%data%i(proc_keys%offset)))//&
                     '" does not conform to the parameter type constraint')
             else
               call inf_error(coder,callnode,'Keyword argument "'//&
                     trim(pm_name_as_string(coder%context,proc_keys%data%i(proc_keys%offset)))//&
                     '" does not have the same type as the default value in the procedure definition')
            endif
            call more_error(coder%context,'Expected: '//trim(pm_type_as_string(coder%context,pdtype)))
            call more_error(coder%context,'Got:      '//&
                 trim(pm_type_as_string(coder%context,keytypes(i))))
            call inf_error(coder,procnode,'Definition corresponding to the above error')
            call inf_trace(coder)
          elseif(error) then
             exit
          else
             keytypes(i)=mtype
          endif
       else
          keytypes(i)=dtype
       endif
       call set_var_type(coder,cnode_arg(arglist,i),keytypes(i))
       call set_var_type(coder,cnode_arg(arglist,i+n),keytypes(i))
    enddo
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine inf_key_args
    
  ! ==================================================
  ! Type infer builtin procedure
  ! ===================================================
  function inf_builtin(coder,procnode,callnode,atype,ptype) result(rtype)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: procnode,callnode
    integer,intent(in):: atype,ptype
    integer:: rtype,mode,atype1
    integer,dimension(1):: key
    integer:: k,t1,n
    type(pm_ptr):: tv,v
    logical:: isstatic,iscomm

    if(debug_inference) then
       write(*,*) 'BUILTIN>',&
            trim(pm_name_as_string(coder%context,cnode_get_num(procnode,pr_name)))
    endif
    
    rtype=cnode_get_num(procnode,pr_rtype)
    if(rtype<0) then
       ! Cached concrete return type
       rtype=-rtype
    else
       ! Convert type to concrete only representation and cache it
       if(rtype/=0) then
          rtype=pm_type_as_concrete(coder%context,rtype,coder%wstack,&
               isstatic)
       endif
    endif

    if(cnode_flags_set(procnode,pr_flags,proccall_is_comm)) then
       atype1=pm_type_arg(coder%context,atype,1+num_comm_args)
    else
       atype1=pm_type_strip_mode(coder%context,&
            pm_type_arg(coder%context,atype,2),mode)
    endif

    ! special handling of return types for some operations
    select case(cnode_get_num(procnode,bi_opcode))
    case(first_fold:last_fold)
       rtype=fold(coder,procnode,atype,rtype)
       call code_num(coder,sp_sig_setval)
       goto 10
    case(op_clone_var)
       k=cnode_get_num(procnode,bi_opcode2)
       rtype=atype1
       if(k/=0) rtype=pm_type_replace_mode(coder%context,rtype,k)
       call code_num(coder,sp_sig_dup)
       goto 10
    case(op_extractelm)
       rtype=pm_type_arg(coder%context,atype1,1)
    case(op_get_dom)
       rtype=pm_type_arg(coder%context,atype1,2)
    case(op_as,op_get_poly_or)
       rtype=pm_type_arg(coder%context,atype,3)
    case(op_import_varg,op_broadcast_val,&
         op_get_rf)
       rtype=atype1
    case(op_clone)
       k=cnode_get_num(procnode,bi_opcode2)
       rtype=atype1
       if(k/=0) rtype=pm_type_replace_mode(coder%context,rtype,k)
    case(op_elem)
       n=cnode_get_num(procnode,bi_opcode2)
       if(n/=0) then
          tv=pm_type_vect(coder%context,atype1)
          k=pm_tv_kind(tv)
          if(k==pm_type_is_vect) then
             t1=pm_tv_arg(tv,1)
             tv=pm_type_vect(coder%context,t1)
             rtype=pm_type_strip_mode(coder%context,pm_tv_arg(tv,n-1),mode)
             if(mode<sym_invar.and.pm_tv_name(tv)/=sym_pling) then
                rtype=pm_new_vect_type(coder%context,rtype)
             endif
          else
             rtype=pm_tv_arg(tv,n-1)
          endif
       else
          tv=pm_type_vect(coder%context,atype)
          t1=pm_type_strip_mode(coder%context,pm_tv_arg(tv,8),mode)
          v=pm_type_val(coder%context,t1)
          n=v%data%ln(v%offset)
          t1=pm_type_strip_mode(coder%context,pm_tv_arg(tv,7),mode)
          tv=pm_type_vect(coder%context,t1)
          k=pm_tv_kind(tv)
          if(k/=pm_type_is_rec.and.k/=pm_type_is_tuple) then
             call inf_error_with_trace(coder,callnode,&
                  'Cannot apply ".element_at_index" to: '//&
                  trim(pm_type_as_string(coder%context,t1)))
             rtype=error_type
          elseif(n<1.or.n>pm_tv_numargs(tv)) then
             call inf_error(coder,callnode,&
                  '".element_at_index": index out of bounds: '//&
                  trim(pm_int_as_string(n)))
             call more_error(coder%context,'Type being indexed: '//&
                  trim(pm_type_as_string(coder%context,t1)))
             call inf_trace(coder)
             rtype=error_type
          else
             rtype=pm_tv_arg(tv,n)
          endif
       endif
    case(op_make_array,op_pack)
       rtype=pm_new_arr_type(coder%context,sym_const,atype1,&
            pm_type_arg(coder%context,atype,2),int(pm_long))
    case(op_var_array)
       rtype=pm_new_arr_type(coder%context,sym_var,atype1,&
            pm_type_arg(coder%context,atype,2),int(pm_long))
    case(op_redim)
       tv=pm_type_vect(coder%context,atype1)
       rtype=pm_new_arr_type(coder%context,pm_tv_name(tv),&
            pm_tv_arg(tv,1),&
            pm_type_arg(coder%context,atype,3),int(pm_long))
    case(op_make_type_val)
       rtype=pm_new_type_type(coder%context,atype1)
    case(op_import_dref)
       rtype=pm_type_strip_mode(coder%context,&
            atype1,mode)
       tv=pm_type_vect(coder%context,rtype)
       if(pm_tv_kind(tv)==pm_type_is_vect) then
          rtype=pm_tv_arg(tv,1)
       endif
    case(op_import_val)
       t1=pm_type_strip_mode(coder%context,atype1,mode)
       if(mode==sym_shared) then
          rtype=pm_error_type_from_string(coder%context,'Cannot import '//&
               '"shrd" value into a new parallel context')
       else
          rtype=pm_type_add_mode(coder%context,t1,&
               merge(sym_shared,sym_invar,&
               iand(pm_type_flags(coder%context,t1),pm_type_has_distributed)/=0))
       endif
    case(op_list_concat)
       call infer_list_concat
    case(op_list_splice)
       call infer_list_splice
    end select

    ! Create cache entry
    key(1)=-cnode_get_num(procnode,bi_id)-1
    k=pm_idict_add(coder%context,&
         coder%proc_cache,key,1,procnode)
    call code_num(coder,k)

10  continue
    
    ! Pass out taint information
    coder%proc_taints=iand(proc_taints,cnode_get_num(procnode,pr_flags))
    coder%taints=ior(coder%taints,coder%proc_taints)

    return
    
  contains
    include 'fisnull.inc'
    include 'fistiny.inc'
    include 'fnew.inc'
    include 'fvkind.inc'

    subroutine infer_list_concat
      call push_word(coder,pm_type_new_tuple)
      call push_word(coder,0)
      n=coder%wtop
      call push_word(coder,pm_type_new_tuple+pm_type_is_list)
      call push_word(coder,0)
      tv=pm_type_vect(coder%context,atype1)
      do k=1,pm_tv_numargs(tv)
         call push_word(coder,pm_tv_arg(tv,k))
      enddo
      t1=pm_type_strip_mode(coder%context,&
           pm_type_arg(coder%context,atype,3),mode)
      tv=pm_type_vect(coder%context,t1)
      do k=1,pm_tv_numargs(tv)
         call push_word(coder,pm_tv_arg(tv,k))
      enddo
      call make_type_if_possible(coder,coder%wtop-n)
      call make_type_if_possible(coder,3)
      rtype=pop_word(coder)
    end subroutine infer_list_concat

    subroutine infer_list_splice
      type(pm_ptr):: tv,tv2,arg
      integer:: k,base,i,siz,t1
      arg=pm_type_val(coder%context,&
           pm_type_strip_mode(coder%context,&
           pm_type_arg(coder%context,atype,4),mode))
      i=arg%data%ln(arg%offset)
      arg=pm_type_val(coder%context,&
           pm_type_strip_mode(coder%context,&
           pm_type_arg(coder%context,atype,5),mode))
      siz=arg%data%ln(arg%offset)
      tv=pm_type_vect(coder%context,atype1)
      if(i<0.or.i>=pm_tv_numargs(tv)) then
         call inf_error(coder,callnode,&
              'Internal error: Bad index in splice_list: '//&
              trim(pm_int_as_string(i))//' with list size='//&
              trim(pm_int_as_string(pm_tv_numargs(tv))))
      endif
      if(siz<0.or.i+siz>=pm_tv_numargs(tv)) then
         call inf_error(coder,callnode,&
              'Internal error: Bad size in splice_list: '//&
              trim(pm_int_as_string(siz))//&
              ' with index='//trim(pm_int_as_string(i))//' and list size='//&
              trim(pm_int_as_string(pm_tv_numargs(tv))))
      endif
      call push_word(coder,pm_type_new_tuple)
      call push_word(coder,0)
      base=coder%wtop
      call push_word(coder,pm_type_new_tuple+pm_type_is_list)
      call push_word(coder,0)
 
      do k=1,i
         call push_word(coder,pm_tv_arg(tv,k))
      enddo
      t1=pm_type_strip_mode(coder%context,&
           pm_type_arg(coder%context,atype,3),mode)
      tv2=pm_type_vect(coder%context,t1)
      do k=1,pm_tv_numargs(tv2)
         call push_word(coder,pm_tv_arg(tv2,k))
      enddo
      do k=i+2+siz,pm_tv_numargs(tv)
         call push_word(coder,pm_tv_arg(tv,k))
      enddo
      call make_type_if_possible(coder,coder%wtop-base)
      call make_type_if_possible(coder,3)
      rtype=pop_word(coder)
    end subroutine infer_list_splice
    
  end function inf_builtin

  !==========================================
  ! Type infer code block
  !==========================================
  subroutine inf_cblock(coder,cblock)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    integer:: nvars,i,newbase
    type(pm_ptr):: p
    if(pm_fast_isnull(cblock)) return
    p=cnode_get(cblock,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       call inf_call(coder,cblock,p)      
       p=cnode_get(p,call_link)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine inf_cblock

  !=======================================================
  ! Type infer general calls
  ! (which include control structures as a special case)
  !========================================================
  subroutine inf_call(coder,cblock,callnode)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,cblock
    integer:: sig
    integer:: tno,tno2,tno3,tno4,name,off,flags,mode,mode2
    type(pm_ptr):: args,t,t2,tv,list,list2,namep
    integer:: i,j,n,nret,nargs,slot,slot2,tbase,wbase
    integer:: vbase_check,tbase_check,counter,nerrors
    integer,dimension(2):: key
    logical:: ok,isstatic,mayfail,undef_arg,cond
    integer(pm_ln):: k
    character(len=100):: str

    if(pm_debug_checks) then
       vbase_check=coder%vtop
       tbase_check=coder%wtop
    endif
    
    nret=cnode_get_num(callnode,call_nret)
    sig=-cnode_get_num(callnode,call_sig)
    args=cnode_get(callnode,call_args)
    nargs=cnode_numargs(args)-nret
    if(sig>0) then
       if(debug_inference) then
          write(*,*) 'PROCESS CALL>',sym_names(sig),&
               'ttop=',coder%wtop,'vtop=',coder%vtop
       endif
       ! Negative signatures indicate a control structure/special case
       ! call (with symbol sig)
       select case(sig)
       case(sym_while,sym_while_invar)
          call check_loop_writes(4)
          list=cnode_arg(args,1)
          list2=cnode_arg(args,3)
          counter=0
          do
             call clear_cblock_mark(list)
             call clear_cblock_mark(list2)
             call inf_cblock(coder,list)
             call check_logical(2,sig==sym_while_invar)
             if(arg_type(2)==coder%false_fix) return
             call inf_cblock(coder,list2)
             if(.not.(cblock_marked(list).or.&
                  cblock_marked(list2))) exit
             counter=counter+1
             if(counter>max_recur) then
                call inf_error_with_trace(coder,args,&
                     '"while" appears to lead to infinite types')
                exit
             endif
          enddo
          if(sig/=sym_while) call mark_loop_cond(5)
       case(sym_until,sym_until_invar,sym_each)
          call check_loop_writes(3)
          list=cnode_arg(args,1)
          counter=0
          do 
             call clear_cblock_mark(list)
             call inf_cblock(coder,list)
             if(.not.cblock_marked(list)) exit
             counter=counter+1
             if(counter>max_recur) then
                call inf_error_with_trace(coder,args,&
                     trim(sym_names(sig))//' appears to lead to infinite types')
                exit
             endif
          enddo
          call check_logical(2,sig==sym_until_invar)
          if(sig/=sym_until) call mark_loop_cond(5)
       case(sym_if,sym_if_invar)
          call inf_if(count_updates(cnode_arg(args,4),2),sig==sym_if_invar)
       case(sym_pm_for,sym_pm_over)
          call inf_cblock(coder,cnode_arg(args,2))
       case(sym_task)
          do i=1,nargs,3
             call inf_cblock(coder,cnode_arg(args,i+1))
             call inf_cblock(coder,cnode_arg(args,i+2))
          enddo
       case(sym_do,sym_pm_shared,sym_pm_shared_always,sym_pm_chan,sym_pm_chan_always)
          call inf_cblock(coder,cnode_arg(args,1))
       case(sym_sync)
          call inf_cblock(coder,cnode_arg(args,2))
       case(sym_pct)
          call inf_cblock(coder,cnode_arg(args,2))
       case(sym_null)
          do i=1,nret
             coder%stack(get_slot(i))=pm_null
          enddo
       case(sym_pm_send:sym_pm_serve)
          call check_long(5)
          coder%taints=ior(coder%taints,proc_is_impure)
          coder%stack(get_slot(1))=pm_long
          tno=pm_type_strip_mode_and_vect(coder%context,arg_type(4))
          t=pm_type_vect(coder%context,tno)
          if(pm_tv_kind(t)/=pm_type_is_dref) then
             call inf_error(coder,callnode,'Internal compiler error: Not a d-ref.')
          endif
          coder%stack(get_slot(2))=pm_type_strip_mode_and_vect(coder%context,arg_type(4))
          if(sig==sym_pm_send.or.sig==sym_pm_collect) then
             coder%stack(get_slot(3))=pm_type_strip_mode_and_vect(coder%context,arg_type(6))
          else
             coder%stack(get_slot(3))=pm_type_strip_mode(coder%context,pm_tv_arg(t,1),mode)
          endif
          call inf_cblock(coder,cnode_arg(args,8))
       case(sym_pm_bcast)
          coder%taints=ior(coder%taints,proc_is_impure)
          coder%stack(get_slot(1))=arg_type(3)
          coder%stack(get_slot(2))=arg_type(4)
          call check_long(5)
          call inf_cblock(coder,cnode_arg(args,6))
       case(sym_pm_recv_req)
          coder%taints=ior(coder%taints,proc_is_impure)
          coder%stack(get_slot(1))=pm_long
          coder%stack(get_slot(2))=pm_type_strip_mode_and_vect(coder%context,arg_type(3))
          call inf_cblock(coder,cnode_arg(args,5))
       case(sym_pm_recv_assn)
          coder%taints=ior(coder%taints,proc_is_impure)
          coder%stack(get_slot(1))=pm_long
          coder%stack(get_slot(2))=pm_type_strip_mode_and_vect(coder%context,arg_type(4))
          coder%stack(get_slot(3))=pm_type_strip_mode_and_vect(coder%context,arg_type(5))
          call inf_cblock(coder,cnode_arg(args,7))
       case(sym_pm_do,sym_pm_do_at)
          do i=merge(1,3,sig==sym_pm_do),nargs-1,2
             coder%stack(get_slot(i))=pm_type_strip_mode_and_vect(coder%context,arg_type(i+1))
          enddo
          call inf_cblock(coder,cnode_arg(args,nargs))
       case(sym_pm_head_node)
          call inf_cblock(coder,cnode_arg(args,1))
       case(sym_pm_dref:sym_pm_ref)
          call push_word(coder,pm_type_new_dref)
          slot=coder%wtop
          call push_word(coder,sym_pm_dref-sig-1)
          if(nargs==3) then
             t=pm_type_vect(coder%context,arg_type(3))
             call push_word(coder,arg_type_with_mode(2))
             call push_word(coder,arg_type_with_mode(3))
             call push_word(coder,arg_type_with_mode(4))
             call push_word(coder,pm_tv_arg(t,4))
             call push_word(coder,pm_tv_arg(t,5))
             tno=0
             tno2=pm_tv_flags(t)
             coder%wstack(slot)=ior(coder%wstack(slot),tno)
             call make_type_if_possible(coder,7)
          else
             do i=1,nargs
                call push_word(coder,arg_type_with_mode(i+1))
             enddo
             if(debug_inference) then
                do i=4,0,-1
                   write(*,*) 'DREF[',i,']',&
                        trim(pm_type_as_string(coder%context,coder%wstack(coder%wtop-i)))
                enddo
             endif
             call make_type_if_possible(coder,nargs+2)
          endif
          if(debug_inference) write(*,*) 'DREF=',&
               trim(pm_type_as_string(coder%context,top_word(coder)))
          coder%stack(get_slot(1))=pop_word(coder)
       case(sym_pm_each_index)
          call inf_each_index
       case(sym_pm_set_dotdotdot)
          coder%stack(get_slot(1))=arg_type(2)
       case(sym_rec)
          t=cnode_arg(args,2)
          t=cnode_arg(t,1)
          if(cnode_num_arg(args,3)>=0) then
             tno=pm_user_type_body(coder%context,cnode_num_arg(args,3))
             t2=pm_type_vect(coder%context,tno)
          else
             tno=t%data%i(t%offset+1)
             t2=pm_type_vect(coder%context,pm_tv_arg(pm_type_vect(coder%context,tno),1))
          endif

          name=t%data%i(t%offset+2)
          call push_word(coder,pm_type_new_rec+t%data%i(t%offset+4))
          call push_word(coder,t%data%i(t%offset))
          do i=1,nargs-3
             call push_word(coder,arg_type_with_mode(i+4))
          enddo
          mode=pm_type_combine_modes(coder%context,&
               coder%wstack(coder%wtop-nargs+4:coder%wtop),&
               cnode_flags_set(callnode,call_flags,call_is_cond).or.arg_type(4)==pm_logical,&
               .false.)
          if(mode<0) then
             if(mode>-1000) then
                namep=pm_name_val(coder%context,pm_tv_name(t2))
                call inf_error_with_trace(coder,callnode,&
                     'Cannot use a shared value'//&
                     ' in "new" to initialise: '//&
                     trim(pm_name_as_string(coder%context,&
                     namep%data%i(namep%offset-mode))))
             else
                call inf_error_with_trace(coder,callnode,&
                     'Cannot use a "prtl" value in "new" '//&
                     'in a "cplt" context to initialise: '//&
                     trim(pm_name_as_string(coder%context,&
                     namep%data%i(namep%offset-mode))))
             endif
             mode=sym_invar
          endif
          do i=1,nargs-3
             tno2=pm_type_strip_mode(coder%context,coder%wstack(coder%wtop-nargs+3+i),mode2)
             tno3=pm_tv_arg(t2,i)
             if(tno2==pm_tiny_int) then
                tno2=tno3
                if(tno2==0.or.iand(pm_type_flags(coder%context,tno2),&
                     pm_type_has_storage)/=0) then
                   namep=pm_name_val(coder%context,pm_tv_name(t2))
                   call inf_error(coder,callnode,'Element "'//&
                        trim(pm_name_as_string(coder%context,&
                        namep%data%i(namep%offset+i)))//&
                        ':'//trim(pm_type_as_string(coder%context,tno2))//'" of "'//&
                        trim(pm_name_as_string(coder%context,name))//&
                        '" needs to be initialised')
                endif
             endif
             coder%wstack(coder%wtop-nargs+3+i)=tno2
          enddo
          call make_type_if_possible(coder,nargs-1)
          tno2=pop_word(coder)
          if(tno2>0) then
             if(.not.pm_type_includes(coder%context,tno,tno2,&
                  pm_type_incl_val)) then
                 call inf_error(coder,callnode,&
                      '"'//trim(sym_names(sig))//&
                      '" initial expression has wrong type for: ',&
                      pm_fast_name(coder%context,name))
                 call inf_trace(coder)
                 tno2=error_type
              endif
           endif
           tno2=pm_type_add_mode(coder%context,tno2,mode)
           call combine_types(cnode_arg(args,1),tno2)
        case(sym_pm_list)
           call push_word(coder,pm_type_new_tuple+pm_type_is_list)
           call push_word(coder,0)
           do i=1,nargs
              call push_word(coder,arg_type_with_mode(i+1))
           enddo
           mode=pm_type_mix_modes(coder%context,&
                coder%wstack(coder%wtop-nargs+1:coder%wtop))
           call make_type_if_possible(coder,nargs+2)
           tno2=pop_word(coder)
           tno2=pm_type_add_mode(coder%context,tno2,mode)
           call combine_types(cnode_arg(args,1),tno2)
        case(sym_dot,sym_dot_ref,sym_get_dot,sym_get_dot_ref)
          if(sig==sym_get_dot.or.sig==sym_get_dot_ref) then
             tno=arg_type(3)
             if(tno/=error_type) then
                namep=pm_type_vect(coder%context,arg_type(3))
                name=pm_tv_name(namep)
                namep=pm_fast_name(coder%context,name)
             else
                call set_arg_to_error_type(1)
                return
             endif
          else
             namep=cnode_arg(cnode_arg(args,3),1)
             name=namep%offset
          endif
          tno=arg_type_with_mode(2)
          if(tno==error_type) then
             call set_arg_to_error_type(1)
          else
             tno=pm_type_strip_mode(coder%context,&
                  tno,mode)
             if(tno>0) then
                call set_call_sig(resolve_elem(cnode_arg(args,2),tno,name,&
                     sig==sym_dot_ref.or.sig==sym_get_dot_ref,.false.,tno2))
                call combine_types(cnode_arg(args,1),&
                     pm_type_add_mode(coder%context,tno2,mode))
             else
                call set_arg_to_error_type(1)
             endif
          endif
       case(sym_cast)
          ! Arg 3 is type to cast to (-ve if in a conditional context)
          tno=arg_type(3)
          if(tno==error_type) then
             call set_arg_to_error_type(1)
             return
          endif
          if(pm_type_kind(coder%context,tno)==pm_type_is_type) then
             tno=pm_type_arg(coder%context,tno,1)
          else
             call inf_error_with_trace(coder,callnode,&
                  '"as" second argument is not a type')
             call set_arg_to_error_type(1)
             return
          endif
          tno2=pm_type_strip_mode(coder%context,&
               arg_type_with_mode(2),mode)
          k=inf_cast(coder,callnode,tno,tno2,.true.)
          call set_call_sig(int(k))
          call combine_types(cnode_arg(args,1),&
               pm_type_add_mode(coder%context,tno2,mode))
       case(sym_var_set_mode)
          mode2=cnode_num_arg(args,2)
          coder%stack(get_slot(1))=pm_type_add_mode(coder%context,&
               pm_type_strip_mode(coder%context,&
               arg_type_with_mode(1),mode),mode2)
          if(mode2>=sym_invar.and.mode<sym_invar) then
             call inf_error_with_trace(coder,callnode,&
                  'Cannot initialise "'//&
                  trim(sym_names(mode2))//'" variable with "'//&
                  trim(sym_names(mode))//'" value')
          endif
       case(sym_private)
          coder%stack(get_slot(1))=pm_type_replace_mode(coder%context,&
               arg_type_with_mode(1),sig)
       case(sym_set_mode)
          mode=cnode_num_arg(args,2)
          coder%stack(get_slot(1))=pm_type_replace_mode(coder%context,&
               arg_type_with_mode(1),mode)
       case(sym_change_mode)
          mode=cnode_num_arg(args,3)
          coder%stack(get_slot(1))=pm_type_replace_mode(coder%context,&
               arg_type_with_mode(2),mode)
       case(sym_invar)
          tno=pm_type_strip_mode(coder%context,arg_type_with_mode(1),mode)
          if(mode<sym_invar) then
             call inf_error_with_trace(coder,callnode,&
                  'Expression must be invariant instead of: '//trim(sym_names(mode)))
          endif
       case(sym_var)
          tno=cnode_num_arg(args,nargs+nret)
          do i=1,nret
             call set_arg_to_type(i,tno)
          enddo
       case(sym_const)
          tno=arg_type(1)
          if(pm_type_kind(coder%context,tno)/=pm_type_is_uninitialised) then
             call inf_error(coder,callnode,&
                  'Cannot initialise constant twice in succession: ',&
                  cnode_get(cnode_arg(args,1),var_name))
          endif
       case(sym_assignment)
          tno=pm_type_get_mode(coder%context,arg_type_with_mode(1))
          if(tno>=sym_invar) then
             call inf_error(coder,callnode,&
                  'Assignments to "'//trim(sym_names(tno))//&
                  '" variables are not allowed outside of a "sync" statement') 
          elseif(tno>=sym_chan) then
             call inf_error(coder,callnode,&
                  'Assignments to "'//trim(sym_names(tno))//&
                  '" variables must be labelled in a conditional context') 
          endif
       case(sym_type_val)
          tno=cnode_num_arg(cnode_arg(args,2),1)
          call combine_types(cnode_arg(args,1),&
               pm_new_type_type(coder%context,tno))
       case(sym_any)
          call inf_any(count_updates(cnode_arg(args,5),2))
       case(sym_test)
          call inf_cblock(coder,cnode_arg(args,1))
       case(sym_check)
          if(.not.pm_fast_isnull(cnode_arg(args,2))) then
             call inf_cblock(coder,cnode_arg(args,2))
          endif
          call inf_cblock(coder,cnode_arg(args,4))
          tno=arg_type(3)
          if(pm_type_strip_to_basic(coder%context,arg_type(1))/=pm_string_type&
               .and.arg_type(1)/=error_type) then
             call inf_error_with_trace(coder,cnode_arg(args,1),&
                  'Check message is not a string, got:'//&
                  trim(pm_type_as_string(coder%context,arg_type(1))))
          elseif(tno==coder%false_fix) then
             if(cnode_get_kind(cnode_arg(args,1))==cnode_is_const) then
                call pm_strval(cnode_arg(cnode_arg(args,1),1),str)
                call inf_error_with_trace(coder,callnode,str(1:len_trim(str)))
             else
                call inf_error_with_trace(coder,callnode,&
                     'Check condition will always fail') 
             endif
          elseif(tno/=coder%true_fix) then
             call check_logical(3,.false.)
             coder%stack(coder%base-2)=ior(coder%stack(coder%base-2),proc_is_impure)
          endif
       case(sym_fix,sym_literal)
          tno=arg_type(2)
          t=pm_type_vect(coder%context,tno)
          if(iand(pm_tv_flags(t),pm_type_has_storage)/=0) then
             call inf_error_with_trace(coder,callnode,&
                  'Value in "'//trim(sym_names(sig))//'" cannot be determined at compile time: '//&
                  trim(pm_type_as_string(coder%context,tno)))
          endif
          if(pm_tv_kind(t)==pm_type_is_literal_value) then
             tno=pm_new_fix_value_type(coder%context,pm_type_val(coder%context,tno),&
                  pm_tv_name(t))
          endif
          coder%stack(get_slot(1))=tno
       case(sym_dcaret)
          coder%stack(get_slot(1))=pm_type_add_mode(coder%context,&
               pm_new_vect_type(coder%context,arg_type(2)),sym_shared)
       case(sym_open)
          if(nargs>0) then
             t=pm_type_vect(coder%context,coder%stack(coder%base))
             n=pm_tv_numargs(t)
             do i=1,nargs
                slot=get_slot(i)
                coder%stack(slot)=pm_tv_arg(t,i)
                if(debug_inference) &
                     write(*,*) 'PARAM>',i,slot,&
                     pm_tv_arg(t,i+n-nargs),pm_tv_numargs(t)
             enddo
             if(n>nargs) then
                call push_word(coder,pm_type_is_tuple)
                call push_word(coder,0)
                j=0
                do i=nargs,n
                   call push_word(coder,pm_tv_arg(t,i))
                   j=j+1
                enddo
                call make_type(coder,j+2)
                slot=get_slot(nargs)
                coder%stack(slot)=pop_word(coder)
             endif
          endif
          coder%stack(coder%base)=undefined  
       case(sym_key)
          ! This is inferred in trav_proc
          continue
       case(sym_present)
          call combine_types(cnode_arg(args,1),int(pm_logical))
       case(sym_result)
          call get_arg_types_and_modes
          call make_type_if_possible(coder,nargs+2)
          coder%stack(coder%base)=pop_word(coder)
       case(sym_start_loop)
          coder%stack(get_slot(2))=pm_logical
       case(sym_underscore,sym_colon,sym_end_loop)
          continue
       case(first_pragma:last_pragma)
          if(sig==sym_infer_type.or.sig==sym_infer_type_and_stack) then
             call inf_cblock(coder,cnode_arg(args,1))
          endif
          if(sig==sym_infer_stack) then
             call cnode_error(coder,callnode,'Type inference stack trace:',warn=.true.)
          endif
          if(sig==sym_infer_type_and_stack.or.sig==sym_infer_stack) then
             call inf_trace(coder)
          endif
       case(sym_pm_dump)
          if(coder%first_pass) then
             if(arg_type_with_mode(1)>0) then
                call cnode_error(coder,callnode,'Type inference gives: '//&
                     trim(pm_type_as_string(coder%context,arg_type_with_mode(1))),warn=.true.)
             else
                call cnode_error(coder,callnode,'Type inference fails',warn=.true.)
             endif
          endif
       case default
          if(sig>=0.and.sig<=num_sym) then
             write(*,*) sym_names(sig)
          else
             write(*,*) 'Sym no:',sig
             write(*,*) trim(pm_name_as_string(coder%context,sig))
          endif
          call pm_panic('Unexpected call symbol')
       end select
    else
       ! A positive signature (so sig<0) is a conventional procedure call
       call inf_proc_call(coder,cblock,callnode,-sig,args,nargs,nret)
    endif

    ! Check stacks are in proper state (no stack leaks)
    if(pm_debug_checks) then
       if(vbase_check/=coder%vtop) then
          if(sig>0) write(*,*) 'in',sym_names(sig)
          write(*,*) 'MISMATCH-vstack',coder%vtop,vbase_check
          call pm_panic('inf_call')
       endif
       if(tbase_check/=coder%wtop) then
          if(sig>0) write(*,*) 'in',sym_names(sig)
          write(*,*) 'MISMATCH-wstack',coder%wtop,tbase_check
          call pm_panic('inf_call')
       endif
    endif
    
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'fname.inc'
    include 'ftiny.inc'
    include 'ftypeno.inc'

    subroutine inf_if(nupdates,isinvar)
      integer,intent(in):: nupdates
      logical,intent(in):: isinvar
      integer,dimension(nupdates):: save_var_types
      integer:: i,tno,typ
      type(pm_ptr):: changelist,writelist,p,var
      call check_logical(1,isinvar)
      tno=arg_type(1)
      changelist=cnode_arg(args,4)
      writelist=cnode_arg(changelist,2)
      if(tno/=coder%false_fix) then
         if(tno==coder%true_fix.or.pm_fast_isnull(cnode_arg(args,3))) then
            call inf_cblock(coder,cnode_arg(args,2))
         else
            i=1
            p=writelist
            do while(.not.pm_fast_isnull(p))
               var=p%data%ptr(p%offset)
               save_var_types(i)=get_var_type(coder,callnode,var)
               p=p%data%ptr(p%offset+1)
               i=i+1
            end do
            call inf_cblock(coder,cnode_arg(args,2))
            i=1
            p=writelist
            do while(.not.pm_fast_isnull(p))
               var=p%data%ptr(p%offset)
               typ=save_var_types(i)
               save_var_types(i)=get_var_type(coder,callnode,var)
               call set_var_type(coder,var,typ)
               p=p%data%ptr(p%offset+1)
               i=i+1
            end do
            call inf_cblock(coder,cnode_arg(args,3))
            i=1
            p=writelist
            do while(.not.pm_fast_isnull(p))
               var=p%data%ptr(p%offset)
               call combine_var_type(coder,callnode,var,save_var_types(i))
               p=p%data%ptr(p%offset+1)
               i=i+1
            end do
         endif
      else
         call inf_cblock(coder,cnode_arg(args,3))
      endif
    end subroutine inf_if

    subroutine inf_any(nupdates)
      integer,intent(in):: nupdates
      integer,dimension(nupdates):: init_var_types,final_var_types
      integer:: i,j,slot,slot2
      type(pm_ptr):: changelist,writelist,list,list2,var,p
      list2=cnode_arg(args,4)
      list2=cnode_arg(list2,1)
      changelist=cnode_arg(args,5)
      writelist=cnode_arg(changelist,2)
      slot=list2%data%i(list2%offset)
      slot2=list2%data%i(list2%offset+1)
      tno=pm_type_strip_mode(coder%context,arg_type(3),mode)
      t=check_poly(coder,tno)
      if(tno/=error_type.and..not.pm_fast_isnull(t)) then
         n=pm_set_size(coder%context,t)
         j=1
         p=writelist
         do while(.not.pm_fast_isnull(p))
            var=p%data%ptr(p%offset)
            init_var_types(j)=get_var_type(coder,callnode,var)
            p=p%data%ptr(p%offset+1)
            j=j+1
         end do
         do i=1,n
            j=1
            p=writelist
            do while(.not.pm_fast_isnull(p))
               var=p%data%ptr(p%offset)
               call set_var_type(coder,var,init_var_types(j))
               p=p%data%ptr(p%offset+1)
               j=j+1
            end do
            list=pm_set_key(coder%context,t,int(i,pm_ln))
            tno=list%data%i(list%offset)
            coder%stack(coder%base+slot:coder%base+slot2)=undefined
            coder%stack(get_slot(1))=&
                 pm_type_add_mode(coder%context,tno,mode)
            call inf_cblock(coder,cnode_arg(args,2))
            call code_int_vec(coder,coder%stack,coder%base+slot,coder%base+slot2)
            
            if(i>1) then
               j=1
               p=writelist
               do while(.not.pm_fast_isnull(p))
                  var=p%data%ptr(p%offset)
                  call combine_var_type(coder,callnode,var,final_var_types(j))
                  p=p%data%ptr(p%offset+1)
                  j=j+1
               end do
            endif
            j=1
            p=writelist
            do while(.not.pm_fast_isnull(p))
               var=p%data%ptr(p%offset)
               final_var_types(j)=get_var_type(coder,callnode,var)
               p=p%data%ptr(p%offset+1)
               j=j+1
            end do
         enddo
         call make_code(coder,pm_null_obj,cnode_is_any_sig,n)
         if(.not.coder%incomplete) then
            list=top_code(coder)
            key(1)=pm_dict_size(coder%context,coder%proc_cache)
            k=pm_idict_add(coder%context,coder%proc_cache,&
                 key,1,list)
            call set_call_sig(int(k))
         endif
         call drop_code(coder)
      else
         coder%stack(coder%base+slot:coder%base+slot2)=undefined
         call set_arg_to_error_type(1)
         call inf_cblock(coder,cnode_arg(args,2))
      endif
    end subroutine inf_any

    subroutine inf_each_index()
      type(pm_ptr):: p,tv
      integer:: start,finish,tno,tno2,i,n,k,key(1)
!!! need to handle modes
 
      p=cnode_arg(args,nret+3)
      p=cnode_arg(p,1)
      start=p%data%i(p%offset)
      finish=p%data%i(p%offset+1)
      tno=arg_type(nret+1)
      if(tno>0) then
         k=pm_type_kind(coder%context,tno)
         if(k==pm_type_is_literal_value.or.k==pm_type_is_fix_value) then
            p=pm_type_val(coder%context,tno)
            n=p%data%ln(p%offset)
         else
            call inf_error(coder,callnode,'Internal error: PM__each_index: not a literal or fix int parameter')
         endif
      else
         n=1
      endif
      if(nret>1) then
         call push_word(coder,pm_type_new_tuple)
         call push_word(coder,0)
      endif
      do i=1,n
         coder%stack(coder%base+start:coder%base+finish)=undefined
         coder%temp=pm_fast_newnc(coder%context,pm_long,1)
         coder%temp%data%ln(coder%temp%offset)=i
         coder%stack(get_slot(nret))=pm_new_fix_value_type(coder%context,coder%temp)
         coder%temp=pm_null_obj
         call inf_cblock(coder,cnode_arg(args,nret+2))
         call code_int_vec(coder,coder%stack,coder%base+start,coder%base+finish)
         if(nret>1) then
            call push_word(coder,arg_type(nret+4))
         endif
      enddo
      if(nret>1) then
         call make_type_if_possible(coder,n+2)
         coder%stack(get_slot(1))=pop_word(coder)
      endif
      call make_code(coder,pm_null_obj,cnode_is_any_sig,n)
      p=pop_code(coder)
      key(1)=pm_dict_size(coder%context,coder%proc_cache)
      k=pm_idict_add(coder%context,coder%proc_cache,&
           key,1,p)
      call set_call_sig(k)
    end subroutine inf_each_index

    !===================================================================
    ! Push argument types with modes for all arguments
    !==================================================================
    subroutine get_arg_types_and_modes
      integer:: i,j
      type(pm_ptr):: v
      if(coder%wtop+nargs+2>max_code_stack) then
         call pm_panic('Program too complex')
      endif
      coder%wstack(coder%wtop+1)=pm_type_is_tuple
      coder%wstack(coder%wtop+2)=0
      do i=1,nargs
         coder%wstack(coder%wtop+i+2)=arg_type_with_mode(nret+i)
      enddo
      coder%wtop=coder%wtop+nargs+2
    end subroutine get_arg_types_and_modes

    !===================================================================
    ! Return the type with for argument m (errors are checked)
    !==================================================================
    function arg_type_with_mode(m) result(tno)
      integer,intent(in):: m
      integer:: tno
!!$      integer:: slot
      tno=get_arg_type(coder,callnode,cnode_arg(args,m))
!!$      slot=get_slot_or_type(m)
!!$      if(slot<0) then
!!$         tno=-slot
!!$      else
!!$         tno=coder%stack(slot)
!!$         if(pm_debug_checks) then
!!$            if(tno==undefined) then
!!$               call qdump_code_tree(coder,pm_null_obj,6,&
!!$                    cnode_arg(args,m),2)
!!$               call inf_error_with_trace(coder,args,&
!!$                    'Internal Compiler Error: Broken type resulution::')
!!$               !!call pm_panic('Broken type resolution chain')
!!$            endif
!!$         endif
!!$      endif
    end function arg_type_with_mode

    !===================================================================
    ! Return the type for argument m (errors are checked)
    !==================================================================
    function arg_type(m) result(tno)
      integer,intent(in):: m
      integer:: tno
      integer:: mode
      tno=pm_type_strip_mode(coder%context,arg_type_with_mode(m),mode)
    end function arg_type
    
!!$    !===================================================================
!!$    ! Return the type and mode for arguement m (no error check)
!!$    !==================================================================
!!$    function arg_type_noerr(m) result(tno)
!!$      integer,intent(in):: m
!!$      integer:: tno
!!$      integer:: slot
!!$      slot=get_slot_or_type(m)
!!$      if(slot<0) then
!!$         tno=-slot
!!$      else
!!$         tno=coder%stack(slot)
!!$      endif
!!$    end function arg_type_noerr

!!$    !===================================================================
!!$    ! Return the slot for arguement m (or -ve of typeno for a constant)
!!$    !==================================================================
!!$    function get_slot_or_type(m) result(slotno)
!!$      integer,intent(in):: m
!!$      integer:: slotno
!!$      type(pm_ptr):: v
!!$      v=cnode_arg(args,m)
!!$
!!$      if(cnode_get_kind(v)==cnode_is_const) then
!!$         slotno=-cnode_num_arg(v,2)
!!$      else
!!$         slotno=cnode_get_num(v,var_index)+coder%base
!!$      endif
!!$    end function get_slot_or_type

!!$    !===================================================================
!!$    ! Return the slot and type for arguement m
!!$    ! - slot will be -ve for a constant
!!$    !==================================================================
!!$    subroutine get_slot_and_type(m,slot,tno)
!!$      integer,intent(in):: m
!!$      integer,intent(out):: slot
!!$      integer,intent(out):: tno
!!$      slot=get_slot_or_type(m)
!!$      if(slot<0) then
!!$         tno=-slot
!!$      else
!!$         tno=coder%stack(slot)
!!$         if(pm_debug_checks) then
!!$            if(tno==undefined) then
!!$               call inf_error_with_trace(coder,args,&
!!$                    'Internal Compiler Error: Broken type resulution::')
!!$            endif
!!$         endif
!!$      endif
!!$    end subroutine get_slot_and_type

    !===================================================================
    ! Return the slot for arguement m (which must be a var)
    !==================================================================
    function get_slot(m) result(slotno)
      integer,intent(in):: m
      integer:: slotno
      type(pm_ptr):: v
      v=cnode_arg(args,m)
      if(pm_debug_checks) then
         if(cnode_get_kind(v)/=cnode_is_var) &
              call pm_panic('get_slot')
      endif
      slotno=cnode_get_num(v,var_index)+coder%base
    end function get_slot

    !==================================================================
    ! Check if argument m has logical type (bool in PM)
    !==================================================================
    subroutine check_logical(m,isinvar)
      integer,intent(in):: m
      logical,intent(in):: isinvar
      integer:: tno,mode
      tno=arg_type_with_mode(m)
      if(tno/=error_type) then
         tno=pm_type_strip_mode(coder%context,tno,mode)
         if(tno/=pm_logical.and.tno/=coder%true_literal.and.tno/=coder%false_literal.and.&
              tno/=coder%true_fix.and.tno/=coder%false_fix) then
            call inf_error_with_trace(coder,callnode,&
                 'Expecting boolean expression, got: '//&
                 trim(pm_type_as_string(coder%context,tno)))
         endif
         if(isinvar.and.mode/=sym_invar) then
            call inf_error_with_trace(coder,callnode,&
                 'Expecting "invar" expression, got: '//&
                 trim(sym_names(mode)))
         endif
      endif
    end subroutine check_logical

        
    !==================================================================
    ! Set loop call signature to 1 if it is in a conditional
    ! (incling masked) context
    !==================================================================
    subroutine mark_loop_cond(m)
      integer,intent(in):: m
      integer:: tno,mark
      tno=arg_type(m)
      if(tno==pm_logical) then
         mark=1
      elseif(cnode_flags_set(callnode,call_flags,call_is_cond)) then
         mark=1
      else
         mark=0
      endif
      call set_call_sig(mark)
    end subroutine mark_loop_cond
    
    !==================================================================
    ! Check if argument m has long type (int type in PM)
    !==================================================================
    subroutine check_long(m)
      integer,intent(in):: m
      integer:: slt
      integer:: ty
      integer:: i
      type(pm_ptr):: tv
      integer:: tno
      tno=arg_type(m)
      if(tno/=error_type) then
         if(tno/=pm_long) then
            call inf_error_with_trace(coder,callnode,&
                 'Expecting long expression, got: '//&
                 trim(pm_type_as_string(coder%context,tno)))
         endif
      endif
    end subroutine check_long

    !=======================================================================
    ! Check that variables updated in a loop call are not uninitialised
    ! Arg #arg must contain the changelist
    !=======================================================================
    subroutine check_loop_writes(arg)
      integer,intent(in):: arg
      type(pm_ptr):: changelists,p,var
      changelists=cnode_arg(args,arg)
      p=cnode_arg(changelists,2)
      do while(.not.pm_fast_isnull(p))
         var=p%data%ptr(p%offset)
         tno=get_var_type(coder,callnode,var)
         p=p%data%ptr(p%offset+1)
      enddo
    end subroutine check_loop_writes
    
    subroutine clear_cblock_mark(list)
      type(pm_ptr),intent(in):: list
      integer:: slot
      slot=coder%base+cnode_get_num(list,cblock_index)
      coder%stack(slot)=0
    end subroutine clear_cblock_mark

    function cblock_marked(list) result(marked)
      type(pm_ptr),intent(in):: list
      logical:: marked
      integer:: slot
      slot=coder%base+cnode_get_num(list,cblock_index)
      marked=coder%stack(slot)/=0
    end function cblock_marked

    !==================================================================
    ! Flag if an import or export option actually
    ! needs to do any work
    ! Sets signature 0=no 1=yes
    !==================================================================
    subroutine flag_import_export(tno)
      integer,intent(in):: tno
      integer:: tkind
      tkind=pm_type_kind(coder%context,tno)
      call set_call_sig(&
           merge(1,0,tkind/=pm_type_is_dref.and.tkind/=pm_type_is_vect))
    end subroutine flag_import_export

    !==================================================================
    ! Set the signature of the current call to k
    !==================================================================
    subroutine set_call_sig(k)
      integer,intent(in):: k
      coder%stack(coder%base+cnode_get_num(callnode,call_index))=k
    end subroutine set_call_sig

    !==================================================================
    ! Resolve signature for item.name
    !==================================================================
    recursive function resolve_elem(var,tno,name,isref,isopt,elem_type) result(sig)
      type(pm_ptr),intent(in):: var
      integer,intent(in):: tno,name
      logical,intent(in):: isref,isopt
      integer,intent(out):: elem_type
      integer:: sig,tk
      type(pm_ptr):: svec

      sig=pm_type_find_elem(coder%context,tno,name,isref,&
           elem_type)
      if(sig==0) then
         if(.not.isopt) then
            tk=pm_type_kind(coder%context,tno)
            if(tk==pm_type_is_error) then
               call inf_type_error(coder,callnode,tno,var)
               coder%stack(cnode_get_num(var,var_index)+coder%base)=error_type
            elseif(tk==pm_type_is_uninitialised) then
               call inf_error(coder,callnode,&
                    'Cannot take an element of an uninitialised value: ',&
                    cnode_get(var,var_name))
               coder%stack(cnode_get_num(var,var_index)+coder%base)=error_type
            elseif(tk/=pm_type_is_rec.and.tk/=pm_type_is_tuple) then
               call inf_error(coder,callnode,&
                    'Cannot take an element of a value of type: "'//&
                    trim(pm_type_as_string(coder%context,tno))//'": ',&
                    cnode_get(var,var_name))
            else
               sig=pm_type_find_elem(coder%context,tno,name,.false.,&
                    elem_type)
               if(sig==0) then
                  call inf_error_with_trace(coder,callnode,&
                       'Type "'//trim(pm_type_as_string(coder%context,tno))//'"'//&
                       ' does not have an element named "'//&
                       trim(pm_name_as_string(coder%context,name))//'" in: ',&
                       cnode_get(var,var_name))
               else
                  call inf_error_with_trace(coder,callnode,&
                       'Cannot modify element "'//&
                       trim(pm_name_as_string(coder%context,name))//&
                       '" of type "'//&
                       trim(pm_type_as_string(coder%context,tno))//'" in: ',&
                       cnode_get(var,var_name))
                  sig=0
               endif
            endif
         endif
         elem_type=error_type
      endif
    end function resolve_elem

    !==================================================================
    ! Set argument m (which must be a var) to have type t
    ! - overwrites original -- generally use combine_types
    !==================================================================
    subroutine set_arg_to_type(m,t)
      integer:: m,t
      integer:: slot
      type(pm_ptr):: ptr
      slot=get_slot(m)
      coder%stack(slot)=t
    end subroutine set_arg_to_type

    !==================================================================
    ! Set argument m (which must be a var) to have error type
    !==================================================================
    subroutine set_arg_to_error_type(m)
      integer:: m
      integer:: slot
      type(pm_ptr):: ptr
      slot=get_slot(m)
      coder%stack(slot)=error_type
    end subroutine set_arg_to_error_type

    !==================================================================
    ! Augment the type stored in a given variable vararg by adding typ
    !==================================================================
    subroutine combine_arg_types(m,typ,no_init)
      integer,intent(in):: m,typ
      logical,intent(in),optional:: no_init
      call combine_types(cnode_arg(args,m),typ,no_init=no_init)
    end subroutine combine_arg_types
    
    !===================================================================
    ! Augment the type stored in a given variable vararg by adding typ
    !==================================================================
    subroutine combine_types(vararg,typ,no_init)
      type(pm_ptr),intent(in)::vararg
      integer,intent(in):: typ
      logical,intent(in),optional:: no_init
      call combine_var_type(coder,cblock,vararg,typ,no_init=no_init)
    end subroutine combine_types
    
    
  end subroutine inf_call

  !==================================================================
  ! Conventional procedure call
  !==================================================================
  subroutine inf_proc_call(coder,cblock,callnode,sig,args,num_args,nret)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,cblock,args
    integer,intent(in):: sig,num_args,nret
    logical:: is_comm,is_cond,is_unlabelled,ignore_rules
    integer:: name,mode,mode2,i,j,tno,tno2,slot,flags
    integer:: nargs,nkey,keybase,ressig,amps
    logical:: undef_arg
    type(pm_ptr):: arg,keys,keynames,amplocs,proclist,t,tv
    
    nargs=num_args

    if(debug_inference) then
       write(*,*) 'PROCESS PROC CALL>',&
            trim(sig_name_str(coder,int(sig))),'@',&
            callnode%data%ptr(callnode%offset+cnode_lineno)%offset
       if(cnode_get_kind(args)/=cnode_is_arglist) call pm_panic('not arglist')
       !call qdump_code_tree(coder,pm_null_obj,6,callnode,2)
    endif

    ! extract characteristics of call
    amps=cnode_get_num(callnode,call_amp)
    flags=cnode_get_num(callnode,call_flags)
    is_comm=iand(flags,proccall_is_comm)/=0
    is_cond=iand(flags,call_is_cond)/=0
    is_unlabelled=iand(flags,call_is_unlabelled)/=0
    ignore_rules=cnode_flags_set(callnode,call_flags,call_ignore_rules)
    if(sig/=0) then
       proclist=pm_dict_val(coder%context,coder%sig_cache,int(sig,pm_ln))
    endif

    undef_arg=.false.
    
    keys=cnode_get(callnode,call_keys)
    keynames=cnode_get(callnode,call_key_names)
    keybase=coder%wtop
    if(.not.pm_fast_isnull(keys)) then
       nkey=cnode_numargs(keys)
       do i=1,nkey
          tno=get_arg_type(coder,callnode,cnode_arg(keys,i))
          call push_word(coder,tno)
          undef_arg=undef_arg.or.tno<=0
       enddo
    else
       nkey=0
    endif

    ! Push arguments types to stack
    call push_word(coder,pm_type_is_tuple)
    call push_word(coder,amps)
    call check_wstack(coder,nargs)

    do i=1,nargs
       tno=get_arg_type(coder,callnode,cnode_arg(args,i+nret),&
            init=flags)
       coder%wstack(coder%wtop+i)=tno
       undef_arg=undef_arg.or.tno<=0
    enddo

    if(is_comm) then 
       if(is_cond) then
          coder%wstack(coder%wtop+num_comm_args)=pm_logical
       else
          if(coder%wstack(coder%wtop+num_comm_args)/=pm_logical) then
             is_cond=.false.
          endif
       endif
    endif

    ! Error return for error argument 
    if(undef_arg) then
       do i=1,nret
          call set_arg_to_error_type(i)
       enddo
       coder%wtop=coder%wtop-2-nkey
       if(debug_inference) then
          write(*,*) 'END PROC CALL (FAILED ERR ARG)>',&
               trim(sig_name_str(coder,int(sig))),coder%stack(4),coder%vtop
       endif
       return
    endif
    
    ! Standard calls
    if(.not.is_comm) then
       
       if(debug_inference) then
          do i=1,nargs
             write(*,*) 'PRE-STRIPPED',&
                  trim(pm_type_as_string(coder%context,coder%wstack(coder%wtop+i)))
          enddo
       endif
       
       ! Suspend 'no shared import' rule in system module code
       ignore_rules=ignore_rules.or.&
            cnode_get_name(callnode,cnode_modl_name)==sym_pm_system
       
       ! Implement mode combination rule for standard procedures
       mode=pm_type_combine_modes(coder%context,&
            coder%wstack(coder%wtop+1:coder%wtop+nargs),is_cond,&
            ignore_rules)
       if(mode<0) then
          call call_error('Cannot pass a shared value to a standard procedure')
          call inf_error_with_trace(coder,cnode_arg(args,nret-mode),&
               'Cannot pass a shared value to a standard procedure')
          mode=sym_private
       endif
       
       ! Rules for "&" arguments
!!! -- Need better error positioning
       if(amps/=0.and..not.ignore_rules) then
          amplocs=pm_name_val(coder%context,amps)
          do i=0,pm_fast_esize(amplocs)
             tno2=pm_type_strip_mode(coder%context,&
                  coder%wstack(coder%wtop+amplocs%data%i(amplocs%offset+i)+nkey),mode2)
             if(tno2>0) then
                tv=pm_type_vect(coder%context,tno2)
                if(pm_tv_kind(tv)==pm_type_is_dref) then
                   do while(pm_tv_name(tv)>0)
                      tno2=pm_tv_arg(tv,2)
                      tv=pm_type_vect(coder%context,tno2)
                   enddo
                   if(pm_tv_kind(tv)==pm_type_is_dref.and.&
                        pm_tv_name(tv)/=pm_dref_is_ref) then
                      call call_error(&
                           'Cannot pass a mixed-mode reference as an "&" argument - must use "&&"')
                      coder%wstack(coder%wtop+amplocs%data%i(amplocs%offset+i)+nkey)=pm_tv_arg(tv,1)
                   endif
                endif
             endif
             if(mode2/=sym_private.and.(mode2/=sym_chan.or.is_unlabelled)) then
                if(mode2==sym_chan) then
                   call call_error('Cannot change "chan" variable in an unlabelled conditional context')
                else
                   call call_error('Cannot change "'//trim(sym_names(mode2))//&
                        '" "&" variable outside of a "sync" statement')
                endif
             endif
          enddo
       endif
       
       ! As this is standard call strip argument modes before passing
       do i=1,nargs
          coder%wstack(coder%wtop+i)=&
               pm_type_strip_mode(coder%context,coder%wstack(coder%wtop+i),mode2)
       enddo
    endif
    
    ! Move stack top to end of args (args were above stack top)
    coder%wtop=coder%wtop+nargs
    
    ! Deal with arg...
    if(cnode_flags_set(callnode,call_flags,call_is_vararg)) then
       if(top_word(coder)>0) then
          t=pm_type_vect(coder%context,top_word(coder))
          if(pm_tv_kind(t)==pm_type_is_tuple) then
             call drop_word(coder)
             do i=1,pm_tv_numargs(t)
                tno2=pm_tv_arg(t,i)
                if(tno2/=pm_tiny_int) then
                   call push_word(coder,tno2)
                   nargs=nargs+1
                endif
             enddo
             nargs=nargs-1
          endif
       endif
    endif

    ! Now run the call itself
    do j=1,nret
       coder%stack(get_slot(j))=undefined
    enddo
    if(sig==0) then
       ressig=var_call(callnode)
    else
       ressig=simple_proc_call(sig,proclist)
    endif
    
    if(debug_inference) then
       write(*,*) 'RESSIG>',ressig,coder%incomplete,&
            'for', trim(sig_name_str(coder,int(sig)))
    endif
    
    ! Standard procedure return modes
    if(.not.is_comm) then
       
       ! Apply return mode to returned values
       if(mode/=sym_private) then
          do j=1,nret
             coder%stack(get_slot(j))=pm_type_replace_mode(coder%context,&
                  coder%stack(get_slot(j)),mode)
          enddo
       endif
       
    endif
    
  
    if(debug_inference) then
       do j=1,nret
          write(*,*) 'RETURN',j,&
               trim(pm_type_as_string(coder%context,coder%stack(get_slot(j))))
       enddo
    endif

    ! Tidy up
    coder%wtop=coder%wtop-nargs-nkey-2
    slot=coder%base+cnode_get_num(callnode,call_index)
    coder%stack(slot)=ressig

    if(debug_inference) then
       write(*,*) 'END PROC CALL>',&
            trim(sig_name_str(coder,int(sig))),coder%stack(4),coder%vtop,ressig,slot,coder%base
    endif
    
  contains

    include 'fesize.inc'
    include 'fisnull.inc'
    include 'ftiny.inc'

    !========================================================================
    ! Procedure call for which signature has been resolved
    ! (either simple in the first place or an option for a vcall)
    ! - If err is present then no error messages - set err to true instead
    ! - If sig_start is present then disable visibility rule (for "." call)
    !========================================================================
    function simple_proc_call(sig,procs,err,sig_start) result(ressig)
      integer,intent(in):: sig
      type(pm_ptr),intent(in):: procs
      logical,intent(out),optional:: err
      integer,intent(in),optional:: sig_start
      integer:: ressig
      
      integer:: h,i,j,m,start,slot,pcheck,nkey_sig,jpass,nconsidered
      integer:: vbase,wbase
      type(pm_ptr):: tv,v,proc,match_proc,rtvect
      integer:: rt,rt2,pars,mpars,apars,tno,match_pars,pflags
      logical:: ok,found,visible,found_has_no_rtypes,when_no_match
      integer,dimension(1):: key
      integer:: memo
    
      if(present(err)) err=.false.
      start=coder%vtop
      if(present(sig_start)) start=sig_start

      ! For procedure signature "." call then don't check visibility
      ! .. also do not check visibility for yield(...) call
      visible=present(sig_start).or.iand(flags,proccall_is_yield)/=0

      ! Find matching signature
      ! This is done in multiple passes with increasingly broader matching
      ! allowed in passes 1..3

      if(debug_inference) write(*,*) 'Checking',cnode_numargs(procs),' sigs'
      found=.false.
      apars=0

      outer: do jpass=0,3
         if(debug_inference) write(*,*) 'MATCH PASS> ',jpass
         nconsidered=0
         do i=1,cnode_numargs(procs)
            proc=cnode_arg(procs,i)
            if(debug_inference) write(*,*) 'CHECK nret',cnode_get_num(proc,pr_nret),nret,&
                 'amps',cnode_get_num(proc,pr_amps),amps,&
                 'comm',cnode_flags_set(proc,pr_flags,proccall_is_comm),is_comm,&
                 'cflags',iand(cnode_get_num(proc,pr_flags),proccall_is_comm+proccall_is_ref+proccall_is_general),&
                 iand(flags,proccall_is_comm+proccall_is_ref+proccall_is_general)
!!$            call pm_dump_tree(coder%context,6,pm_name_val(coder%context,cnode_get_num(proc,pr_amps)),2)
!!$            call pm_dump_tree(coder%context,6,pm_name_val(coder%context,amps),2)
            
            if(cnode_get_num(proc,pr_nret)/=nret) cycle
            if(cnode_get_num(proc,pr_amps)/=amps) cycle
            pflags=cnode_get_num(proc,pr_flags)
            if(iand(pflags,proccall_is_comm+proccall_is_ref+proccall_is_general)/=&
                 iand(flags,proccall_is_comm+proccall_is_ref+proccall_is_general)) cycle
            if(iand(pflags,proc_is_cond)/=0.and..not.is_cond.or.&
                 iand(pflags,proc_is_uncond)/=0.and.is_cond) cycle
            nconsidered=nconsidered+1
            
            pars=cnode_get_num(proc,pr_ptype)
            
            if(debug_inference) then
               write(*,*) 'CHECKING SIG',(i-1)/2,&
                    ' OF ',(cnode_numargs(procs)-2)/2,&
                    ' FOR> ',trim(sig_name_str(coder,int(sig)))
               write(*,*) '>> ',trim(pm_type_as_string(coder%context,pars))
            endif

            wbase=coder%wtop
            vbase=coder%vtop
            apars=match_call_sig(coder,callnode,proc,&
                 pars,nargs,call_flags,jpass)
            
            if(apars>=0) then

               ! Check for a visible match
               if(is_visible(coder,callnode,proc)) visible=.true.

               !!$               ! If this is a second (or later) match, then check for compatibility
               if(found) then
                  if(debug_inference) write(*,*) 'SECOND MATCH>',&
                       trim(pm_type_as_string(coder%context,pars)),'AFTER>',&
                       trim(pm_type_as_string(coder%context,match_pars))
                  if(pm_type_includes(coder%context,pars,&
                       match_pars,pm_type_incl_type)) then
                     coder%wtop=wbase
                     coder%vtop=vbase
                     ! Have to also check compatibility of return types
                     ! in the case where the enclosing procedure defines return types
                     ! and the first-match procedure does not define them
                     if(nret>0.and.rt>0.and.found_has_no_rtypes) then
                        rt2=abs(cnode_get_num(proc,pr_rtype))
                        if(pm_type_kind(coder%context,rt2)/=pm_type_is_undef_result) then
                           if(.not.pm_type_includes(coder%context,rt2,rt,pm_type_incl_type)) then
                              call inf_error(coder,proc,&
                                   'Procedure returns type(s) not compatible'//&
                                   ' with an enclosing procedure to which it conforms')
                              call inf_error(coder,cnode_arg(procs,i+1),&
                                   'Enclosing procedure referenced in above error')
                              call more_error(coder%context,' ')
                              call print_call_details(coder,callnode,keybase,nargs)
                              call inf_trace(coder)
                           endif
                        endif
                     endif
                     if(pm_type_has_when(coder%context,pars)) then
                        if(pm_type_includes(coder%context,match_pars,&
                             pars,pm_type_incl_type)) then
                           ! Two equally specific when procs - the second must have when(false)
                           coder%trace_depth=coder%trace_depth+1
                           if(coder%trace_depth<max_trace_depth) then
                              coder%trace(coder%trace_depth)=callnode
                              coder%trace_keys(coder%trace_depth)=keybase
                           endif
                           rt=inf_proc(coder,proc,callnode,apars,pars,nret,nkey,&
                                keynames,keybase,&
                                int(pm_fast_esize(cnode_get(proc,pr_keys))+1)/2,when_no_match,.true.)
                           coder%trace_depth=coder%trace_depth-1
                           if(.not.when_no_match) then
                              call inf_error(coder,callnode,&
                                   'Ambiguous call to: '//trim(sig_name_str(coder,int(sig))))
                              call more_error(coder%context,&
                                   'Two or more equivalent procedures have "when" expressions evaluating to "true"')
                              call print_call_details(coder,callnode,keybase,nargs)
                              call print_proc_details(coder,match_proc)
                              call print_proc_details(coder,proc)
                           endif
                        endif
                     endif
                     cycle
                  else
                     if(.not.present(err)) then
                        call inf_error(coder,callnode,&
                             'Ambiguous call to: '//trim(sig_name_str(coder,int(sig))))
                        call print_call_details(coder,callnode,keybase,nargs)
                        call print_proc_details(coder,match_proc)
                        call print_proc_details(coder,proc)
                     else
                        err=.true.
                     endif
                     coder%wtop=wbase
                     exit
                  endif
               endif
               
               ! A good match has been found
               ! infer the associated procedure
               if(cnode_get_kind(proc)==cnode_is_builtin) then
                  rt=inf_builtin(coder,proc,callnode,apars,pars)
               else
                  
                  pcheck=coder%vtop
                  
                  ! Traceback record 
                  ! of calls being processed
                  coder%trace_depth=coder%trace_depth+1
                  if(coder%trace_depth<max_trace_depth) then
                     coder%trace(coder%trace_depth)=callnode
                     coder%trace_keys(coder%trace_depth)=keybase
                  endif

                  rt=inf_proc(coder,proc,callnode,apars,pars,nret,nkey,&
                       keynames,keybase,&
                       int(pm_fast_esize(cnode_get(proc,pr_keys))+1)/2,when_no_match,.false.)
                  coder%trace_depth=coder%trace_depth-1
                  if(when_no_match) then
                     cycle
                  endif
                  if(cnode_get_name(callnode,cnode_modl_name)/=sym_pm_system) then
                     coder%supress_errors=.false.
                  endif

                  if(rt<0) then
                     if(debug_inference) then
                        write(*,*) 'INCOMPLETE PROC>',coder%vtop,start,coder%incomplete
                     endif
                     do j=1,nret
                        call set_arg_to_error_type(j)
                     enddo
                     coder%vtop=start
                     coder%wtop=wbase
                     ressig=undefined
                     return
                  else
                     !if(coder%vtop/=pcheck+1) call pm_panic('pcheck mismatch')
                  endif
               endif

               found=.true.
               match_pars=pars
               match_proc=proc
               found_has_no_rtypes=&
                    pm_type_kind(coder%context,cnode_get_num(match_proc,pr_rtype))==&
                    pm_type_is_undef_result
               
               if(nret>0) then
                  if(rt>0) then
                     rtvect=pm_type_vect(coder%context,rt)
                     if(pm_tv_kind(rtvect)==pm_type_is_tuple.and.&
                          iand(pm_tv_flags(rtvect),pm_type_is_list)==0) then
                        do j=1,nret
                           v=cnode_arg(args,j)
                           call combine_types(v,&
                                pm_tv_arg(rtvect,j))
                        enddo
                     else
                        call combine_types(cnode_arg(args,1),rt)
                     endif
                  else
                     do j=1,nret
                        call set_arg_to_error_type(j)
                     enddo
                  endif
               endif
            elseif(apars==error_type) then
               if(debug_inference) write(*,*) 'TERMINATED>'
               exit outer
            else
               ! Not this one - keep looking
               coder%vtop=vbase
               if(debug_inference) write(*,*) 'REJECTED>'
            endif
            coder%wtop=wbase
            if(debug_inference) write(*,*) 'CHECKED SIG'
         enddo
         if(found) exit
      enddo outer
      
      if(debug_inference) then
         write(*,*) 'ALL SIGS CHECKED>',trim(sig_name_str(coder,int(sig)))
      endif

      if(apars==error_type) then
         do i=1,nret
            call set_arg_to_error_type(i)
         enddo
         ressig=undefined
      elseif(.not.found.or..not.visible) then
         ! If nothing found print error message
         ! or return error flag
         if(present(err)) then
            err=.true.
         elseif(iand(flags,proccall_is_yield)==0) then
            if(.not.found) then
               call cnode_error(coder,callnode,&
                    'No matching procedure returning '//trim(pm_int_as_string(nret))//' value'//&
                    merge(': ','s:',nret==1))
            else
               call cnode_error(coder,callnode,&
                    'No matching procedure is visible to the module containing the call')
               call more_error(coder%context,'(expecting '//trim(pm_int_as_string(nret))//' return value'//&
                     merge(': ','s:',nret==1))
            endif
            m=coder%wtop
            call make_type(coder,nargs+2)
            call print_call_details(coder,callnode,keybase,nargs)
            coder%wtop=m
               call more_error(coder%context,'Procedures considered:')
               do m=1,cnode_numargs(procs)
                  proc=cnode_arg(procs,m)
                  if(nconsidered/=0) then
                     if(cnode_get_num(proc,pr_nret)/=nret) cycle
                     if(cnode_get_num(proc,pr_amps)/=amps) cycle
                     if(iand(cnode_get_num(proc,pr_flags),proccall_is_comm+proccall_is_ref+proccall_is_general)/=&
                          iand(flags,proccall_is_comm+proccall_is_ref+proccall_is_general)) cycle
                  endif
                  pars=cnode_get_num(proc,pr_ptype)
                  call print_proc_details(coder,proc)
                  if(m>pm_opts%proc_list.and..not.pm_opts%see_all_procs) then
                     call more_error(coder%context,&
                          '... (to see all procedures use -fsee-all-procs)')
                     exit
                  endif
               enddo
            
            call inf_trace(coder)
            do i=1,nret
               call set_arg_to_error_type(i)
            enddo
            ressig=undefined
         else
            call cnode_error(coder,callnode,'Yield statement does not conform to supplied block')
            call print_call_details(coder,callnode,keybase,nargs)
            call inf_trace(coder)
         endif
      else
         ! Otherwise create resolved procedure cnode
         if(coder%vtop>start+1) then
            call make_code(coder,pm_null_obj,cnode_is_autoconv_sig,&
                 coder%vtop-start)
            key(1)=pm_dict_size(coder%context,coder%proc_cache)
            ressig=pm_idict_add(coder%context,coder%proc_cache,key,1,top_code(coder))
         else
            ressig=coder%vstack(coder%vtop)%offset
         endif
      endif

      ! Tidy up
      coder%vtop=start
       
    end function  simple_proc_call

    !================================================
    ! Call with variable procedure name: v.(args)
    !================================================
    function var_call(callnode) result(ressig)
      type(pm_ptr),intent(in):: callnode
      integer:: ressig
      integer:: i,sig,rsig,apars
      type(pm_ptr):: pr,var,tv,tv2
      integer:: proctyp,tno,name,start,arg(1)
      logical:: err

      ! Get value for procedure name (actually its type)
      var=cnode_get(callnode,call_var)
      if(cnode_get_kind(var)==cnode_is_var) then
         proctyp=coder%stack(cnode_get_num(var,var_index)+coder%base)
      else
         proctyp=cnode_num_arg(var,2)
      endif
      if(proctyp==error_type) then
         goto 999
      endif
      tv=pm_type_vect(coder%context,proctyp)
      if(pm_tv_kind(tv)==pm_type_is_par_kind) then
         proctyp=pm_tv_arg(tv,1)
         tv=pm_type_vect(coder%context,proctyp)
      endif
      coder%wstack(coder%wtop-nargs)=proctyp

      if(pm_tv_kind(tv)/=pm_type_is_proc) then
         call inf_error_with_trace(coder,callnode,&
              'Value for ".()" call does not hold proc name, got: '//&
              trim(pm_type_as_string(coder%context,proctyp)))
         goto 999
      endif
      name=abs(pm_tv_name(tv))
      arg(1)=name
      sig=pm_ivect_lookup(coder%context,coder%sig_cache,arg,1)
      if(sig>0) then
         pr=pm_dict_val(coder%context,coder%sig_cache,int(sig,pm_ln))
         if(pm_tv_name(tv)>=0) then
            start=coder%vtop
            ressig=simple_proc_call(sig,pr,sig_start=start)
         else
            tno=pm_tv_arg(tv,1)
            tv2=pm_type_vect(coder%context,tno)
            call check_call_against_sig(proctyp,tv,callnode)
            start=coder%vtop
            apars=match_call_sig(coder,callnode,callnode,pm_tv_arg(tv2,1),nargs,&
                 cnode_get_num(callnode,call_flags),3,issig=.true.)
            if(apars>0) then
               ressig=simple_proc_call(sig,pr,sig_start=start)
               if(ressig>0) call check_returns_against_sig(proctyp,tv,callnode)
               ! Pop off extra set of arg types pushed by match_call_sig
               coder%wtop=coder%wtop-nargs-2
            else
               call inf_error(coder,callnode,&
                    'Call arguments do not match procedure type'//&
                    ' and no conversions are available: '//&
                    pm_type_as_string(coder%context,proctyp))
               goto 999
            endif
         endif
         return
      else
         call inf_error_with_trace(coder,callnode,&
              'No match found for ".()" call using procedure name: '//&
              trim(pm_name_as_string(coder%context,name)))
      endif
999   continue
      do i=1,nret
         call set_arg_to_error_type(i)
      enddo
      ressig=undefined

      write(*,*) 'var call end>',coder%wtop
    end function  var_call

    !=======================================================
    ! If a call is v.(args) with v of a signature type
    ! then it is necessary to check call characteristics 
    ! against the signature
    !=======================================================
    subroutine check_call_against_sig(tno,tvp,callnode)
      integer,intent(in):: tno
      type(pm_ptr),intent(in):: tvp,callnode
      type(pm_ptr):: tv
      integer:: flags,kind

      tv=pm_type_vect(coder%context,pm_tv_arg(tvp,1))
      
      ! Get information on call
      flags=cnode_get_num(callnode,call_flags)
      if(iand(flags,proccall_is_ref)/=0) then
         kind=sym_dot
      elseif(iand(flags,proccall_is_general)/=0) then
         kind=sym_dash
      elseif(iand(flags,proccall_is_comm)/=0) then
         kind=sym_pct
      else
         kind=sym_proc
      endif
      
      if(pm_tv_name(tv)/=kind) then
         call inf_error(coder,callnode,&
              'Call does not match procedure type ("'//&
              trim(pm_name_as_string(coder%context,pm_tv_name(tv)))//'" vs "'//&
              trim(pm_name_as_string(coder%context,kind))//'"):'//&
              trim(pm_type_as_string(coder%context,tno)))
      endif

      if(iand(pm_tv_flags(tv),pm_type_is_yield)/=0.neqv.&
           cnode_flags_set(callnode,call_flags,proccall_is_block)) then
         call inf_error(coder,callnode,&
              'Call does not match procedure type ("yield")'//&
              trim(pm_type_as_string(coder%context,tno)))
      endif
      
    end subroutine check_call_against_sig

    !=======================================================
    ! If a call is v.(args) with v of a signature type
    ! then it is necessary to check the returned values against
    ! the signature
    !=======================================================
    subroutine check_returns_against_sig(tno,tvp,callnode)
      integer,intent(in):: tno
      type(pm_ptr),intent(in):: tvp,callnode
      type(pm_ptr):: tv2
      integer:: tno2,nret
      integer:: tno3,i,k,n,at

      nret=cnode_get_num(callnode,call_nret)
      
      ! Check returns
      tno2=pm_type_arg(coder%context,pm_tv_arg(tvp,1),2)
      tv2=pm_type_vect(coder%context,tno2)
      if(pm_tv_kind(tv2)==pm_type_is_undef_result) then
         if(nret/=pm_tv_name(tv2)) then
            call inf_error(coder,callnode,&
                 'Call does not match procedure type: '//&
                 pm_type_as_string(coder%context,tno))
            call more_error(coder%context,'Call has a different number of return values: '//&
                 trim(pm_int_as_string(nret))//' vs '//trim(pm_int_as_string(pm_tv_name(tv2))))
            return
         endif
      else
         n=pm_tv_numargs(tv2)
         if(nret/=n) then
            call inf_error(coder,callnode,&
              'Call does not match procedure type: '//&
              pm_type_as_string(coder%context,tno))
            call more_error(coder%context,'Different number of return values: '//&
                 trim(pm_int_as_string(nret))//' vs '//trim(pm_int_as_string(n)))
            return
         endif
         
         do i=1,n
            at=get_var_type(coder,callnode,cnode_arg(args,i))
            if(.not.pm_type_includes(coder%context,pm_tv_arg(tv2,i),&
                 at,pm_type_incl_val)) then
               
               call inf_error(coder,callnode,&
                    'Call does not match procedure type: '//&
                    pm_type_as_string(coder%context,tno))
               call more_error(coder%context,&
                    'Return type mismatch: '//&
                    trim(pm_type_as_string(coder%context,pm_tv_arg(tv2,i)))//&
                    ' vs: '//&
                    trim(pm_type_as_string(coder%context,at)))
            endif
         enddo
      endif
      return
10    continue
     
      call inf_error(coder,callnode,&
           'Call does not match procedure type: '//&
           pm_type_as_string(coder%context,tno))
      call inf_trace(coder)
      
    end subroutine check_returns_against_sig

!!$    !===================================================================
!!$    ! Return type, without mode, for argument m
!!$    !==================================================================
!!$    function arg_type(m) result(tno)
!!$      integer,intent(in):: m
!!$      integer:: tno
!!$      integer:: mode
!!$      tno=pm_type_strip_mode(coder%context,arg_type_with_mode(m),mode)
!!$    end function arg_type
!!$
!!$    !===================================================================
!!$    ! Return type and mode for arguement m 
!!$    !==================================================================
!!$    function arg_type_with_mode(m) result(tno)
!!$      integer,intent(in):: m
!!$      integer:: tno
!!$      integer:: slot
!!$      slot=get_slot_or_type(m)
!!$      if(slot<0) then
!!$         tno=-slot
!!$      else
!!$         tno=coder%stack(slot)
!!$         if(pm_debug_checks) then
!!$            if(tno==undefined) then
!!$               write(*,*) m,slot,pm_main_process
!!$               call qdump_code_tree(coder,pm_null_obj,6,&
!!$                    cnode_arg(args,m),2)
!!$               call inf_error_with_trace(coder,args,'Broken::')
!!$               !call pm_panic('Broken type resolution chain')
!!$            endif
!!$         endif
!!$      endif
!!$    end function arg_type_with_mode
!!$
    !===================================================================
    ! Return the slot for arguement n (which must be a var)
    !==================================================================
    function get_slot(m) result(slotno)
      integer,intent(in):: m
      integer:: slotno
      type(pm_ptr):: v
      v=cnode_arg(args,m)
      if(pm_debug_checks) then
         if(cnode_get_kind(v)/=cnode_is_var) &
              call pm_panic('get_slot')
      endif
      slotno=cnode_get_num(v,var_index)+coder%base
    end function get_slot
!!$
!!$    !===================================================================
!!$    ! Return the slot for arguement n (or -ve of typeno for a constant)
!!$    !==================================================================
!!$    function get_slot_or_type(m) result(slotno)
!!$      integer,intent(in):: m
!!$      integer:: slotno
!!$      type(pm_ptr):: v
!!$      v=cnode_arg(args,m)
!!$
!!$      if(cnode_get_kind(v)==cnode_is_const) then
!!$         slotno=-cnode_num_arg(v,2)
!!$      else
!!$         slotno=cnode_get_num(v,var_index)+coder%base
!!$      endif
!!$    end function get_slot_or_type

    !===================================================================
    ! Set argument m to the error type
    !==================================================================
    subroutine set_arg_to_error_type(m)
      integer:: m
      integer:: slot
      type(pm_ptr):: ptr
      slot=get_slot(m)
      coder%stack(slot)=error_type
    end subroutine set_arg_to_error_type

    !===================================================================
    ! Augment the type stored in a given variable vararg by adding typ
    !==================================================================
    subroutine combine_types(vararg,typ,no_init)
      type(pm_ptr),intent(in)::vararg
      integer,intent(in):: typ
      logical,intent(in),optional:: no_init
      call combine_var_type(coder,cblock,vararg,typ,no_init=no_init)
    end subroutine combine_types
   
    !=========================================
    ! Print error message for a call
    !=========================================
    subroutine call_error(str)
      character(len=*):: str
      call inf_error(coder,callnode,str)
      call print_call_details(coder,callnode,coder%wtop,nargs)
      call inf_trace(coder)
    end subroutine call_error
    
  end subroutine inf_proc_call

  !====================================================
  ! Check if a procedure matches a given call signature
  ! - defined by parameter tuple type pars
  ! Call argument types must be nargs entries on wstack
  ! Returns tuple of argument types (after any conversion)
  ! Conversions are covered by ipass (see match_arg)
  ! 
  !====================================================
  function match_call_sig(coder,callnode,procnode,pars,&
       nargs,flags,ipass,issig) result(argtyp)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,procnode
    integer,intent(in):: pars
    integer,intent(in):: nargs,flags,ipass
    logical,intent(in),optional:: issig
    integer:: argtyp
    
    logical:: nomatch,error
    integer:: i,n,at,at2,pt,pk,wbase
    type(pm_ptr):: pv
 
    if(pars==error_type) then
       argtyp=undefined
       return
    endif
    pv=pm_type_vect(coder%context,pars)
    pk=pm_tv_kind(pv)
    n=pm_tv_numargs(pv)

    if(n>nargs) then
       argtyp=undefined
       return
    endif

    if(pm_debug_checks) then
       if(pk/=pm_type_is_tuple.and.&
            pk/=pm_type_is_vtuple) &
            call pm_panic('check-sig')
    endif
 
    if(debug_inference) then
       write(*,*) 'Check call sig: [ipass=',ipass,'] ('
       write(*,*) pars,' ',trim(pm_type_as_string(coder%context,pars))
       write(*,*) '----'
       do i=1,nargs
          at=coder%wstack(coder%wtop-nargs+i)
          write(*,*) at,' ',trim(pm_type_as_string(coder%context,at))
       enddo
       write(*,*) ')'
    endif
    
    ! Allocate space for new argument types on wstack
    wbase=coder%wtop
    if(coder%wtop+nargs+2>max_code_stack) then
       call pm_panic('Program too complex (match-sig)')
    endif
    coder%wtop=coder%wtop+nargs+2
    coder%wstack(wbase+1)=pm_type_is_tuple
    coder%wstack(wbase+2)=0

    ! Process each argument, converting if required
    do i=1,nargs
       at=coder%wstack(wbase-nargs+i)
       if(at==undefined) call pm_panic('broken type resolution chain')
       if(at==error_type) then
          pt=0
          cycle
       endif
       if(i>n) then
          if(pk/=pm_type_is_vtuple) then
             argtyp=undefined
             goto 10
          endif
       else
          pt=pm_tv_arg(pv,i)
       endif

       at2=match_arg(coder,callnode,procnode,pt,at,i,ipass,nomatch,error)
       if(error.or.nomatch) then
          argtyp=undefined
          goto 10
       endif
       coder%wstack(wbase+i+2)=at2
       
    enddo

    if(.not.present(issig)) then
    
       ! Bundle arguments into a single type
       argtyp=pm_new_type(coder%context,coder%wstack(wbase+1:&
            wbase+nargs+2))

    else
       argtyp=1
       return

    endif

    ! Error exit point
10  continue

    ! Tidy up
    coder%wtop=wbase
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'fesize.inc'
  end function match_call_sig

  !================================================================
  ! Match a single argument of type at to parameter type pt
  ! applying automatic conversions as required
  ! Returns converted argument type
  !  nomatch - match failed
  !  error   - actual error raised (such as ambiguous match)
  ! Any conversions will result in conversion record pushed on vstack
  !  convesion record will refer to argument #ielem
  ! Conversions applied are determined by ipass
  !  0 -- none
  !  1 -- lexical to basic
  !  2 -- proc type conversion
  !  3 -- convert to poly type
  !================================================================
  function match_arg(coder,callnode,procnode,pt,old_at,ielem,ipass,nomatch,error) result(new_at)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,procnode
    integer,intent(in):: pt,old_at,ielem,ipass
    logical,intent(out):: nomatch,error
    integer:: new_at
    integer:: at,pt2,at2,base,status,flags
    at=old_at
    nomatch=.false.
    error=.false.
    flags=cnode_get_num(callnode,call_flags)
    if(iand(flags,call_is_fixed)==0) then
       at2=pm_type_convert(coder%context,pt,at,.true.,ipass>=2,.false.)
       if(at2>0) at=at2
    endif
    if(pm_type_includes(coder%context,&
         pt,at,pm_type_incl_val)) then
       if(debug_inference) then
          write(*,*) 'Match',trim(pm_type_as_string(coder%context,pt)),'<>',&
               trim(pm_type_as_string(coder%context,at))
       endif
       new_at=at
       return
    else
       at2=pm_type_convert(coder%context,pt,at,.true.,ipass>=2,.false.)
       if(at2>0) then
          new_at=at2
          return
       elseif(ipass==3) then
          ! On third pass check for poly conversions
          at2=convert_poly(coder,pt,at,.false.)
          if(at2/=-1) then
             base=coder%wtop
             call push_word(coder,ielem)
             call push_word(coder,at2)
             call code_int_vec(coder,coder%wstack,base+1,coder%wtop)
             ! Correct parameter type to post-conversion value
             coder%wtop=base
             new_at=at2
             return
          endif
       endif
       
       ! No match found
       if(debug_inference) then
          write(*,*) 'Does not include',&
               trim(pm_type_as_string(coder%context,pt)),'<>',&
               trim(pm_type_as_string(coder%context,at))
       endif
       nomatch=.true.
       !coder%wtop=base
       return
    endif
  end function  match_arg
  
  
  !=================================================================
  ! Is procnode directly visible from module containing callnode?
  !=================================================================
  function is_visible(coder,callnode,procnode) result(ok)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,procnode
    logical:: ok
    integer:: callmodule,procmodule
    integer:: key(2)
    integer(pm_ln):: j
    callmodule=cnode_get_name(callnode,cnode_modl_name)
    procmodule=cnode_get_name(procnode,cnode_modl_name)
    if(callmodule==procmodule.or.procmodule==sym_pm_system) then
       ok=.true.
    else
       key(1)=callmodule
       key(2)=procmodule
       j=pm_ivect_lookup(coder%context,coder%visibility,key,2)
       ok=j>0
    endif
  end function is_visible

  subroutine new_stack_frame(coder,max_index)
    type(code_state),intent(inout):: coder
    integer,intent(in):: max_index
    coder%stack(coder%top+1)=coder%base
    coder%base=coder%top+4
    coder%top=coder%base+max_index
    if(coder%top>max_code_stack) &
         call pm_panic('Program too complex (nested calls)')
  end subroutine new_stack_frame

  
  ! ================================================================================
  ! Set up type inference frame
  ! Three control slots:
  !  coder%base-2   == taints for current procedure
  !  coder%base-1   == break value -- flags changing types, resolution not complete if /= 0
  !  coder%base     == argument (on entry) return (on exit) types
  ! Remaining slots:
  !  coder%base+index == resolution information according to var or call index
  ! =================================================================================
  subroutine create_stack_frame(coder,argtype,max_index,init_taints) 
    type(code_state),intent(inout):: coder
    integer,intent(in):: argtype,max_index,init_taints
    coder%stack(coder%top+1)=coder%base
    coder%base=coder%top+4
    coder%top=coder%base+max_index
    if(coder%top>max_code_stack) &
         call pm_panic('Program too complex (nested calls)')
    call init_stack_frame(coder,coder%base,1,coder%top,argtype,init_taints)
  end subroutine create_stack_frame

  !===============================================================
  ! (Re)initialise current stack frame
  !===============================================================
  subroutine init_stack_frame(coder,base,first,last,argtype,init_taints) 
    type(code_state),intent(inout):: coder
    integer,intent(in):: base,first,last,argtype,init_taints
    integer:: i
    coder%stack(base-2)=init_taints
    coder%stack(base-1)=0
    coder%stack(base)=argtype
    do i=base+first,last
       coder%stack(i)=undefined
    enddo
  end subroutine init_stack_frame

  !===============================================================
  ! Pop off current stack frame
  !===============================================================
  subroutine pop_stack_frame(coder)
    type(code_state),intent(inout):: coder
    coder%top=coder%base-4
    coder%base=coder%stack(coder%base-3)
    if(coder%base==0) call pm_panic('xxx')
  end subroutine pop_stack_frame

  !===============================================================
  ! Perform poly type conversion from typ2 to typ1 if possible
  ! Return converted type or -1 on failure
  !===============================================================
  function convert_poly(coder,typ1,typ2,conv_poly) result(typ3)
    type(code_state),intent(inout):: coder
    integer,intent(in):: typ1,typ2
    logical,intent(in):: conv_poly
    integer:: typ3
    type(pm_ptr):: tv1,tv2
    if(typ1<=0) return
    typ3=-1
    tv1=pm_type_vect(coder%context,typ1)
    tv2=pm_type_vect(coder%context,typ2)
    if(pm_tv_kind(tv1)==pm_type_is_poly) then
       if(pm_tv_kind(tv2)==pm_type_is_poly) then
          if(conv_poly.and.pm_type_includes(coder%context,&
               pm_tv_arg(tv1,1),pm_tv_arg(tv2,1),&
               pm_type_incl_type)) then
             if(add_poly_to_poly(coder,typ1,typ2)) then
                coder%types_finished=.false.
             endif
             typ3=typ1
          endif
       else
         if(pm_type_includes(coder%context,&
               pm_tv_arg(tv1,1),typ2,&
               pm_type_incl_type)) then
            if(add_type_to_poly(coder,typ1,typ2)) then
               coder%types_finished=.false.
            endif
             typ3=typ1
          endif
       endif
    endif
  end function convert_poly

  !==============================================================
  ! Return the working set for a given poly type
  ! Returns a set type
  !==============================================================
  function check_poly(coder,poly_type) result(ptr)
    type(code_state),intent(inout):: coder
    integer,intent(in):: poly_type
    type(pm_ptr):: ptr
    integer(pm_ln):: j
    integer,dimension(1):: key
    key(1)=poly_type
    j=pm_ivect_lookup(coder%context,coder%poly_cache,key,1)
    if(j==0) then
       ptr=pm_null_obj
    else
       ptr=pm_dict_val(coder%context,coder%poly_cache,j)
    endif
  end function check_poly

  !=======================================================
  ! Add a type to the working set for a given poly type
  ! Return whether working set has changed
  !======================================================
  function add_type_to_poly(coder,poly_type,mtyp) result(changed)
    type(code_state),intent(inout):: coder
    integer,intent(in):: poly_type,mtyp
    logical:: changed
    integer,dimension(1):: key
    integer(pm_ln):: j
    type(pm_ptr):: v
    key(1)=poly_type
    j=pm_ivect_lookup(coder%context,coder%poly_cache,key,1)
    if(j==0) then
       coder%temp=pm_set_new(coder%context,32_pm_ln)
       j=pm_idict_add(coder%context,&
            coder%poly_cache,&
            key,1,coder%temp)
       key(1)=mtyp
       j=pm_iset_add(coder%context,&
            coder%temp,key,1)
       changed=.true.
    else
       key(1)=mtyp
       v=pm_dict_val(coder%context,coder%poly_cache,j)
       j=pm_iset_add(coder%context,v,key,1,changed)
    endif
  end function add_type_to_poly
  
  !=======================================================
  ! Add all types in poly_type2 to the working set for
  ! poly type poly_type
  ! Return whether working set has changed
  !======================================================
  function add_poly_to_poly(coder,poly_type,poly_type2) result(changed)
    type(code_state),intent(inout):: coder
    integer,intent(in):: poly_type,poly_type2
    logical:: changed
    type(pm_ptr):: typeset1,typeset2,type_entry
    integer(pm_ln):: i,j,n
    integer,dimension(1):: key
    changed=.false.
    typeset2=check_poly(coder,poly_type2)
    if(pm_fast_isnull(typeset2)) return
    typeset1=check_poly(coder,poly_type)
    if(pm_fast_isnull(typeset1)) then
       coder%temp=pm_set_new(coder%context,32_pm_ln)
       key(1)=poly_type
       j=pm_idict_add(coder%context,&
            coder%poly_cache,&
            key,1,coder%temp)
       typeset1=coder%temp
    endif
    call pm_set_merge(coder%context,typeset1,typeset2,changed)
  contains
    include 'fisnull.inc'
  end function add_poly_to_poly

  !=================================================
  ! Get currently resolved type (&mode) for argument
  ! (variable or constant)
  !=================================================
  function get_arg_type(coder,callnode,arg,init) result(tno)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,arg
    integer,intent(in),optional:: init
    integer:: tno
    if(cnode_get_kind(arg)==cnode_is_var) then
       tno=get_var_type(coder,callnode,arg,init)
    else
       if(pm_debug_checks) then
          if(cnode_get_kind(arg)/=cnode_is_const) then
             call pm_panic('get_arg_type')
          endif
       endif
       tno=cnode_num_arg(arg,2)
    endif
  end function get_arg_type
  
  !============================================================================
  ! Get currently resolved type (&mode) for variable
  ! Can pass call flags through init parameter
  !  - call_takes_init       - no error for unitialised value
  !  - call_converts_uninit  - Convert uninitialsed value to a type value
  !============================================================================
  function get_var_type(coder,callnode,var,init) result(tno)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,var
    integer,intent(in),optional:: init
    integer:: tno
    integer:: tk
    tno=coder%stack(cnode_get_num(var,var_index)+coder%base)
!!$    if(tno==undefined) then
!!$       call cnode_error(coder,callnode,&
!!$            'Internal error: broken type resolution chain')
!!$       write(*,*) '###',cnode_get_num(var,var_index),cnode_get_num(var,var_index)+coder%base
!!$       tno=error_type
!!$       return
!!$    endif
    tk=pm_type_kind(coder%context,tno)
    if(tk==pm_type_is_uninitialised) then
       if(present(init)) then
          if(iand(init,call_takes_uninit)/=0) then
             if(iand(init,call_converts_uninit)/=0) then
                tno=pm_type_arg(coder%context,tno,1)
             endif
             return
          endif
       endif
       call cnode_error(coder,callnode,&
            'Attempt to use "var" or "const" value before it is initialised: ',&
            cnode_get(var,var_name))
       call cnode_error(coder,var,&
            'Definition statement relating to above error')
       coder%stack(cnode_get_num(var,var_index)+coder%base)=error_type
       tno=error_type
    elseif(tk==pm_type_is_error) then
       call inf_type_error(coder,callnode,tno,var)
       coder%stack(cnode_get_num(var,var_index)+coder%base)=error_type
       tno=error_type
    endif
  end function get_var_type

  !===============================================
  ! Set resolved type (& mode) for variable
  !===============================================
  subroutine set_var_type(coder,var,tno)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: var
    integer:: tno
    coder%stack(cnode_get_num(var,var_index)+coder%base)=tno
  end subroutine set_var_type

  !===============================================
  ! Return count of entries in an update list
  !===============================================
  function count_updates(changelist,listno) result(n)
    type(pm_ptr),intent(in):: changelist
    integer,intent(in):: listno
    integer:: n
    type(pm_ptr):: p
    p=cnode_arg(changelist,listno)
    n=0
    do while(.not.pm_fast_isnull(p))
       n=n+1
       p=p%data%ptr(p%offset+1)
    enddo
  contains
    include 'fisnull.inc'
  end function count_updates

  !===============================================
  ! Combine a new type into the type recorded
  ! for a given variable
  !===============================================
  subroutine combine_var_type(coder,cnode,var,typ,no_init)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cnode,var
    integer,intent(in):: typ
    logical,intent(in),optional:: no_init
    integer:: typ0,typ2
    typ0=get_var_type(coder,cnode,var)
    typ2=typ0
    if(typ/=typ0) then
       if(typ0<=0) then
          typ2=typ
       elseif(typ>0) then
          if(present(no_init).or.&
               pm_type_kind(coder%context,typ0)/=pm_type_is_uninitialised) then
             if(pm_type_kind(coder%context,typ0)==pm_type_is_uninitialised.or.&
                  pm_type_kind(coder%context,typ)==pm_type_is_uninitialised) then
                call cnode_error(coder,cnode,&
                     'Variable/constant is not intialised in '//&
                     ' all branches of a conditional statment:',&
                     cnode_get(var,var_name))
             else
                call cnode_error(coder,var,'Value does not have consistent type:',&
                     cnode_get(var,var_name))
                call more_error(coder%context,&
                     'First:  '//trim(pm_type_as_string(coder%context,typ0)))
                call more_error(coder%context,&
                     'Then:   '//trim(pm_type_as_string(coder%context,typ)))
                if(present(no_init)) then
                   call cnode_error(coder,cnode,&
                        'Above type is inconsistent between branches of this statement')
                else
                   call cnode_error(coder,cnode,'Type inconsistency occurs here')
                endif
             endif
          endif
          typ2=error_type
       endif
    endif
    call set_var_type(coder,var,typ2)
  end subroutine combine_var_type
  

  !===========================================================
  ! Type constraint / Cast
  !===========================================================
  function inf_cast(coder,node,tno1,tno2,isvar) result(k)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: tno1
    integer,intent(inout):: tno2
    logical,intent(in):: isvar
    integer:: k
    logical:: ok
    integer:: tno3,base,key(1)
    k=0
    if(tno1<0.or.tno2<=0) then
       return
    endif
    ok=pm_type_includes(coder%context,tno1,tno2,pm_type_incl_val)
    if(.not.ok) then
       tno3=pm_type_convert(coder%context,tno1,tno2,.true.,.true.,.false.)
       if(tno3==undefined) then
          base=coder%wtop
          tno3=convert_poly(coder,tno1,tno2,.true.)
          if(tno3/=-1) then
             k=tno3
             tno2=tno3
             ok=.true.
          endif
          coder%wtop=base
       else
          tno2=tno3
          ok=.true.
       endif
    endif
    if(.not.ok) then
       call inf_error(coder,node,&
            'Value cannot be cast to the given type')
       call inf_trace(coder)
    endif
  contains
    include 'fisnull.inc'
  end function inf_cast

  !=============================================
  ! Compile time computation of expressions
  ! atype - tuple of argument types
  ! rstypes - typle of declared result types
  ! rtype - actual result type
  !=============================================
  function fold(coder,procnode,atype,rstype) result(rtype)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: procnode
    integer,intent(in):: atype,rstype
    integer:: rtype
    integer:: i,n,opcode,errno,tno,tk,num_elem
    type(pm_ptr):: tv,arg1,arg2
    type(pm_ptr):: result
    logical:: ok
    character(len=100):: emess
    type(pm_ptr):: rtv
    integer:: rtyp

    tv=pm_type_vect(coder%context,atype)
    n=pm_tv_numargs(tv)-1
    opcode=cnode_get_num(procnode,bi_opcode)
    if(opcode==op_num_elems_fold) then
       coder%temp=pm_fast_newnc(coder%context,pm_long,1)
       tno=pm_tv_arg(tv,2)
       tk=pm_type_kind(coder%context,tno)
       if(tk/=pm_type_is_rec.and.tk/=pm_type_is_tuple) then
          call inf_error_with_trace(coder,procnode,&
               'Can only apply "num_elements" to a "rec", not: '//&
               pm_type_as_string(coder%context,tno))
          num_elem=1
       else
          num_elem=pm_type_numargs(coder%context,tno)
       endif
       coder%temp%data%ln(coder%temp%offset)=num_elem
       rtype=pm_new_fix_value_type(coder%context,coder%temp)
       return
    elseif(opcode==op_type_include_fold) then
       ok=pm_type_includes(coder%context,&
            pm_type_arg(coder%context,&
            pm_type_arg(coder%context,atype,2),1),&
            pm_type_arg(coder%context,&
            pm_type_arg(coder%context,atype,3),1),&
            pm_type_incl_type)
       if(ok) then
          rtype=coder%true_literal
       else
          rtype=coder%false_literal
       endif
       return
    endif
    arg1=pm_dict_val(coder%context,coder%context%tcache,&
         int(pm_tv_arg(tv,2),pm_ln))
    if(n>1) then
       arg2=pm_dict_val(coder%context,coder%context%tcache,&
            int(pm_tv_arg(tv,3),pm_ln))
    endif
    rtyp=pm_type_strip_to_basic(coder%context,pm_type_arg(coder%context,rstype,1))
    
    !write(*,*) rtyp,'rtyp=',trim(pm_type_as_string(coder%context,rtyp))

    rtv=pm_type_vect(coder%context,rtyp)
    rtyp=pm_type_strip_to_basic(coder%context,pm_tv_arg(rtv,1))
    if(rtyp==pm_long) then
       coder%temp=pm_fast_newnc(coder%context,pm_long,1)
       call fold_value(opcode,coder%temp,arg1,arg2,ok,emess)
       if(.not.ok) then
          call inf_error_with_trace(coder,procnode,&
               'Cannot combine run time values: '//trim(emess))
       elseif(pm_tv_kind(rtv)==pm_type_is_literal) then 
          rtype=pm_new_literal_value_type(coder%context,coder%temp)
       else
          rtype=pm_new_fix_value_type(coder%context,coder%temp)
       endif
    elseif(rtyp==pm_string_type) then
       call fold_string(coder,opcode,arg1,arg2,coder%temp)
       if(pm_tv_kind(rtv)==pm_type_is_literal) then 
          rtype=pm_new_literal_value_type(coder%context,coder%temp)
       else
          rtype=pm_new_fix_value_type(coder%context,coder%temp)
       endif
    else
       if(opcode==op_eq_fold.or.opcode==op_ne_fold) then
          ok=pm_type_name(coder%context,pm_tv_arg(tv,2))==&
               pm_type_name(coder%context,pm_tv_arg(tv,3))
          if(opcode==op_ne_fold) ok=.not.ok
       else
          call fold_comparison(opcode,arg1,arg2,ok)
       endif
       if(pm_tv_kind(rtv)==pm_type_is_literal) then
          if(ok) then
             rtype=coder%true_literal
          else
             rtype=coder%false_literal
          endif
       else
          if(ok) then
             rtype=coder%true_fix
          else
             rtype=coder%false_fix
          endif
       endif
    endif
    coder%temp=pm_null_obj
  contains
    include "fnewnc.inc"
  end function fold

  
  !===========================================================
  ! Calculate and arithmetic operation on integer constants
  !===========================================================
  subroutine fold_value(op,a,b,c,ok,emess)
    integer,intent(in):: op
    type(pm_ptr),intent(in):: a,b,c
    logical,intent(out):: ok
    character(len=*),intent(out):: emess
    ok=.true.
    select case(op)
    case(op_uminus_fold)
       a%data%ln(a%offset)=-b%data%ln(b%offset)
    case(op_add_fold)
       a%data%ln(a%offset)=b%data%ln(b%offset)+c%data%ln(c%offset)
    case(op_sub_fold)
       a%data%ln(a%offset)=b%data%ln(b%offset)-c%data%ln(c%offset)
    case(op_mult_fold)
       a%data%ln(a%offset)=b%data%ln(b%offset)*c%data%ln(c%offset)
    case(op_divide_fold)
       if(c%data%ln(c%offset)/=0) then
          a%data%ln(a%offset)=b%data%ln(b%offset)/c%data%ln(c%offset)
       else
          ok=.false.
          emess='division by zero'
       endif
    case(op_mod_fold)
       if(c%data%ln(c%offset)/=0) then
          a%data%ln(a%offset)=modulo(b%data%ln(b%offset),c%data%ln(c%offset))
       else
          ok=.false.
          emess='modulo zero'
       endif
    case(op_pow_fold)
       a%data%ln(a%offset)=b%data%ln(b%offset)**c%data%ln(c%offset)
    case(op_max_fold)
       a%data%ln(a%offset)=max(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_min_fold)
       a%data%ln(a%offset)=min(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_abs_fold)
       a%data%ln(a%offset)=abs(b%data%ln(b%offset))
    case(op_band_fold)
       a%data%ln(a%offset)=iand(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_bor_fold)
       a%data%ln(a%offset)=ior(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_bxor_fold)
       a%data%ln(a%offset)=ieor(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_bshift_fold)
       a%data%ln(a%offset)=ishft(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_bnot_fold)
       a%data%ln(a%offset)=not(b%data%ln(b%offset))
    case(op_pdiff_fold)
       a%data%ln(a%offset)=dim(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_sign_fold)
       a%data%ln(a%offset)=sign(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_modulo_fold)
        if(c%data%ln(c%offset)/=0) then
          a%data%ln(a%offset)=mod(b%data%ln(b%offset),c%data%ln(c%offset))
       else
          ok=.false.
          emess='modulo zero'
       endif
    end select
  end subroutine fold_value

  !=================================================
  ! Calculate operations returning string constants
  !=================================================
  subroutine fold_string(coder,op,a,b,c)
    type(code_state),intent(inout):: coder
    integer,intent(in):: op
    type(pm_ptr),intent(in):: a,b
    type(pm_ptr),intent(out):: c
    character(len=100):: str
    select case(op)
    case(op_string_fold)
       str=pm_number_as_string(coder%context,a,0_pm_ln)
       c=pm_new_string(coder%context,trim(str))
    case(op_concat_fold)
       c=pm_concat_string(coder%context,a,b)
    end select
  end subroutine fold_string

  !===============================================
  ! Calculate operations returning bool constants
  !===============================================
  subroutine fold_comparison(op,a,b,ok)
    integer,intent(in):: op
    type(pm_ptr),intent(in):: a,b
    logical,intent(out):: ok
    select case(op)
    case(op_gt_fold)
       ok=a%data%ln(a%offset)>b%data%ln(b%offset)
    case(op_ge_fold)
       ok=a%data%ln(a%offset)>=b%data%ln(b%offset)
    case(op_eq_fold)
       ok=a%data%ln(a%offset)>=b%data%ln(b%offset)
    case(op_ne_fold)
       ok=a%data%ln(a%offset)/=b%data%ln(b%offset)
    case(op_and_fold)
       ok=a%data%l(a%offset).and.b%data%l(b%offset)
    case(op_or_fold)
       ok=a%data%l(a%offset).or.b%data%l(b%offset)
    case(op_except_fold)
       ok=a%data%l(a%offset).and..not.b%data%l(b%offset)
    end select
  end subroutine fold_comparison

  !========================================================
  ! Build a type from top n entries on wstack
  ! - return error type if any type argument is error_type
  !========================================================
  subroutine make_type_if_possible(coder,n)
    type(code_state),intent(inout):: coder
    integer,intent(in):: n
    integer:: i

    do i=n,3,-1
       if(coder%wstack(coder%wtop-n+i)==error_type) then
          coder%wtop=coder%wtop-n+1
          coder%wstack(coder%wtop)=error_type
          return
       endif
    enddo
    call make_type(coder,n)
  end subroutine make_type_if_possible

  !===================================================
  ! Dump resolved proc signatures (debugging)
  !===================================================
  subroutine dump_res_sigs(coder,iunit)
    type(code_state),intent(in):: coder
    integer,intent(in):: iunit
    integer(pm_ln):: i,n
    n=pm_dict_size(coder%context,coder%proc_cache)
    do i=1,n
       write(iunit,*) 'Resolved Signature',i,'of',n,'('
       call qdump_code_tree(coder,pm_null_obj,iunit,&
            pm_dict_val(coder%context,coder%proc_cache,i),&
            2)
       write(iunit,*) ')'
    enddo
  end subroutine dump_res_sigs

  !===============================================
  ! Output error message followed by call trace
  !===============================================
  subroutine inf_error_with_trace(coder,node,message,name)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    character(len=*):: message
    type(pm_ptr),intent(in),optional:: name
    logical:: save_supress,current_supress
    current_supress=coder%supress_errors
    call inf_error(coder,node,message,name)
    save_supress=coder%supress_errors
    coder%supress_errors=current_supress
    call inf_trace(coder)
    coder%supress_errors=save_supress
  end subroutine inf_error_with_trace

  !=====================================
  ! Output error message
  !=====================================
  subroutine inf_error(coder,node,message,name)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    character(len=*):: message
    type(pm_ptr),intent(in),optional:: name
    character(len=250):: str
    type(pm_ptr):: modname
    integer:: i,modl_name,lineno,charno
    if(coder%supress_errors) return
    ! Do not repeat errors on the same node
    ! (only really a problem with inference)
    do i=1,min(max_error_nodes,coder%num_errors)
       if(coder%error_nodes(i)==node) return
    enddo
    coder%error_nodes(min(coder%num_errors+1,max_error_nodes))=node
    if(pm_main_process) then
       write(*,*)
       modl_name=cnode_get_name(node,cnode_modl_name)
       lineno=cnode_get_name(node,cnode_lineno)
       charno=cnode_get_name(node,cnode_charno)
       if(modl_name==sym_pm_system.and.pm_opts%hide_sysmod) then
          ! Search call stack for source outside of the system module
          ! (note- par/import stack is misused here)
          do i=coder%trace_depth,1,-1
             modl_name=cnode_get_name(coder%trace(i),cnode_modl_name)
             if(modl_name/=sym_pm_system) then
                lineno=cnode_get_name(coder%trace(i),cnode_lineno)
                charno=cnode_get_name(coder%trace(i),cnode_charno)
                exit
             endif
          enddo
       endif
       call pm_error_header(coder%context,&
            modl_name,&
            lineno,&
            charno)
       if(present(name)) then
          if(.not.pm_fast_isnull(name)) then
             call pm_name_string(coder%context,int(name%offset),str)
             str=trim(pm_opts%error)//' '//trim(message)//' '//trim(str)
          else
             str=trim(pm_opts%error)//' '//message
          endif
       else
          str=trim(pm_opts%error)//' '//message
       endif
       write(*,'(A)') trim(str)
    endif
    if(cnode_get_name(node,cnode_modl_name)==sym_pm_system.and.&
         pm_opts%hide_sysmod) then
       coder%supress_errors=.true.
    endif
    coder%num_errors=coder%num_errors+1
    if(coder%num_errors>max_code_errors) then
       call pm_stop('Too many type inference errors - compilation terminated')
    endif
  contains
    include 'fisnull.inc'
  end subroutine inf_error

  !===========================================================
  ! Output error message associated with given error type
  !==========================================================
  subroutine inf_type_error(coder,node,tno,var)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node,var
    integer,intent(in):: tno
    type(pm_ptr):: tv
    character(len=100):: str
    call pm_strval(pm_type_val(coder%context,tno),str)
    call inf_error(coder,node,trim(str)//': '//&
         trim(pm_name_as_string(coder%context,cnode_get_num(var,var_name))))
  end subroutine inf_type_error

  ! ============================================================
  ! Output trace of current call stack
  ! Calls stored in coder%trace(1:coder%trace_depth)
  !  and coder%trace_keys(1:coder%trace_depth)
  ! Ignores internal calls within PM__system
  !  unless pm_opts%hide_sysmod is false
  ! =============================================================
  subroutine inf_trace(coder)
    type(code_state):: coder
    type(pm_ptr):: node,modname,tv
    integer:: k,top
    if(.not.pm_main_process) return
    if(coder%supress_errors) return
    if(coder%trace_depth<1) return
    top=coder%trace_depth
    if(pm_opts%hide_sysmod.and.top<max_trace_depth) then
       node=coder%trace(top)
       if(hide(node)) then
          do while(top>1)
             top=top-1
             node=coder%trace(top)
             if(.not.hide(node)) then
                exit
             endif
          enddo
       endif
    endif

    if(top==1.and.pm_opts%hide_sysmod) then
       if(hide(coder%trace(top))) return
    endif
    
    write(*,*)
    write(*,*) '-------------CALL TRACE---------------------------'
    do k=top,1,-1
       if(k>max_trace_depth) then
          write(*,*) 'Procedure call: (call not recorded)'
          cycle
       endif
       node=coder%trace(k)
       if((.not.hide(node)).or.&
            (.not.pm_opts%hide_sysmod)) then
          call print_call_details(coder,node,&
               coder%trace_keys(k))
          if(k>1) write(*,*)
       endif
    enddo
    write(*,*) '--------------------------------------------------'
    write(*,*)
  contains

    function hide(node) result(hideit)
      type(pm_ptr),intent(in):: node
      logical:: hideit
      character(len=4):: prefix
      integer:: name
      if(cnode_get_name(node,cnode_modl_name)==sym_pm_system) then
         hideit=.true.
         return
      endif
      name=pm_name_stem(coder%context,sig_name(coder,cnode_get_num(node,call_sig)))
      if(name==sym_assignment.or.name==sym_assign_var.or.&
           name==sym_make_subref.or.name==sym_make_sublhs.or.&
           name==sym_make_sublhs_amp) then
         hideit=.false.
      else
         prefix=pm_name_as_string(coder%context,name)
         hideit=prefix=='PM__'
      endif
      !write(*,*) 'hide',hideit,name,sym_make_subref,sym_dump,pm_name_as_string(coder%context,name)
    end function hide

  end subroutine inf_trace
  
  ! ============================================
  ! Print details of individual call
  ! Argument information in wstack from base
  ! ==============================================
  subroutine print_call_details(coder,node,base,numargs)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: base
    integer,intent(in),optional:: numargs
    integer:: i
    character(len=100):: str
    character(len=2):: join,ampstr
    character(len=1):: procchr,dotchr
    integer:: n,k,nargs,nkeys
    integer::ampidx,signame,signamebase
    type(pm_ptr):: tv,key,val,amp,keyargs,keynames,name
    if(.not.pm_main_process) return
    if(coder%supress_errors) return
    nargs=cnode_numargs(cnode_get(node,call_args))-cnode_get_num(node,call_nret)
    if(present(numargs)) nargs=numargs

    nkeys=0
    ampidx=cnode_get_num(node,call_amp)
    if(ampidx==0) then
       amp=pm_null_obj
    else
       amp=pm_name_val(coder%context,ampidx)
    endif
    call pm_name_string(coder%context,&
         cnode_get_name(node,cnode_modl_name),str)
    if(pm_opts%colour) then
       write(*,'(A,A,A,A,I4,A)') 'Call at: ',pm_loc_start,trim(str),&
            ' line:',cnode_get_num(node,cnode_lineno),pm_loc_end
    else
       write(*,'(A,A,A,I4)') 'Call at: ',trim(str),&
            ' line:',cnode_get_num(node,cnode_lineno)
    endif
    call pm_error_header(coder%context,&
         cnode_get_name(node,cnode_modl_name),&
         cnode_get_name(node,cnode_lineno),&
         cnode_get_name(node,cnode_charno))
    signame=sig_name(coder,cnode_get_num(node,call_sig))
    signamebase=pm_name_stem(coder%context,signame)
    if(signame==sym_proc) then
       tv=pm_type_vect(coder%context,coder%wstack(base))
       signame=abs(pm_tv_name(tv))
    elseif(signamebase==sym_assignment.or.signamebase==sym_assign_var) then
       signame=sym_assign
    elseif(signamebase==sym_make_subref.or.signamebase==sym_make_sublhs.or.&
         signamebase==sym_make_sublhs_amp) then
       signame=sym_sub
    endif


    if(cnode_flags_set(node,call_flags,proccall_is_comm)) then
       n=6
       if(cnode_flags_set(node,call_flags,proccall_is_general)) then
          procchr=''''
       else
          procchr='%'
       endif
    else
       n=1
       procchr=' '
    endif

    if(cnode_flags_set(node,call_flags,proccall_is_ref)) then
       procchr=' '
       dotchr='.'
    else
       dotchr=' '
    endif

    if(cnode_flags_set(node,call_flags,proccall_is_block)) then
       n=n+3
    endif

    if(pm_opts%show_hidden) n=0
    
    call more_error(coder%context,dotchr//trim(pm_name_as_string(coder%context,&
            signame))//procchr//' (')
    k=0
    do i=n+1,nargs
       if(i<nargs.or.nkeys>0) then
          join=', '
       else
          join=' '
       endif
       call check_amp(i)
       call more_error(coder%context,&
            '   '//ampstr//&
            trim(pm_type_as_string(coder%context,coder%wstack(base+nkeys+2+i)))//join)
    enddo
    if(.not.present(numargs).and.cnode_flags_set(node,call_flags,call_is_vararg)) then
       call more_error(coder%context,'   ...')
    endif
    keynames=pm_name_val(coder%context,cnode_get_num(node,call_key_names))
    do i=1,nkeys
       if(i<nkeys) then
          join=', '
       else
          join=' '
       endif
       call more_error(coder%context,'     '//&
            trim(pm_name_as_string(coder%context,keynames%data%i(keynames%offset+i-1)))//&
            ' = '//&
            trim(pm_type_as_string(coder%context,coder%wstack(base+i)))//join)     
    enddo
 
    if(cnode_flags_set(node,call_flags,proccall_is_block)) then
       call more_error(coder%context,' ) yield (')
       call more_error(coder%context,'     '//&
            trim(pm_type_as_string(coder%context,coder%wstack(base+nkeys+2+n-2))))
       call more_error(coder%context,' )')
    else
       call more_error(coder%context,' )')
    endif
  contains
    include 'fesize.inc'
    include 'fisnull.inc'

    subroutine check_amp(i)
      integer:: i
      if(pm_fast_isnull(amp)) then
         ampstr='  '
         return
      endif
      if(k>pm_fast_esize(amp)) then
         ampstr='  '
         return
      endif
      do while(amp%data%i(amp%offset+k)<i)
         k=k+1
         if(k>pm_fast_esize(amp)) then
            ampstr='  '
            return
         endif
      enddo
      if(amp%data%i(amp%offset+k)==i) then
         ampstr=' &'
         k=k+1
      else
         ampstr='  '
      endif
    end subroutine check_amp
    
  end subroutine print_call_details

  !=====================================================
  ! Print details of procedure definition
  ! with given signature (sig) and parameter types (tno)
  ! ====================================================
  subroutine print_proc_details(coder,node)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    integer:: name
    integer:: istart,n,tno,nret,i
    character(len=512):: str,str2,buf1,buf2
    if(.not.pm_main_process) return
    if(coder%supress_errors) return
    name=cnode_get_num(node,pr_name)
    str=' '
    call pm_name_string(coder%context,&
         cnode_get_name(node,cnode_modl_name),str2)
    if(str=='PM__system'.and.pm_opts%hide_sysmod) then
       str='(System)'
    else
       write(buf1,'(I7)') cnode_get_num(node,cnode_lineno)
       write(buf2,'(I7)') cnode_get_num(node,cnode_charno)
       if(pm_opts%colour) then
          write(str,'(A,A,":",A,":",A,A)') &
               pm_loc_start,trim(str2),trim(adjustl(buf1)),&
               trim(adjustl(buf2)),pm_loc_end
       else
          write(str,'(A,":",A,":",A)') &
               trim(str2),trim(adjustl(buf1)),trim(adjustl(buf2))
       endif
    endif
    n=max(len_trim(str)+2,20)
    str(n:n)=':'
    n=n+2
    nret=cnode_get_num(node,pr_nret)
    do i=1,nret-1
       str(n:n+1)='_,'
       n=n+2
    enddo
    if(nret>0) then
       str(n:n+1)='_='
       n=n+2
    endif
 
    if(cnode_flags_set(node,pr_flags,proccall_is_ref)) then
       str(n:n)='.'
       n=n+1
    endif
    call pm_name_string(coder%context,name,str(n:))
    n=len_trim(str)+1
    if(cnode_flags_set(node,pr_flags,proccall_is_ref)) then
       istart=7
    elseif(cnode_flags_set(node,pr_flags,proccall_is_comm)) then
       if(cnode_flags_set(node,pr_flags,proccall_is_general)) then
          str(n:n)=''''
       else
          str(n:n)='%'
       endif
       n=n+1
       istart=7
    else
       istart=2
    endif
    if(cnode_flags_set(node,pr_flags,proccall_is_block)) istart=istart+3
    if(pm_opts%show_hidden) istart=1
    tno=cnode_get_num(node,pr_ptype)
    call pm_type_to_string(coder%context,tno,str,n,tuple_start=istart)
    n=n+1
    if(n>len(str)-20) then
       str(n:n+2)='...'
    else
       if(cnode_flags_set(node,pr_flags,proccall_is_block)) then
          str(n:)=')yield('
          n=n+7
          tno=pm_type_arg(coder%context,tno,istart)
          call pm_type_to_string(coder%context,tno,str,n)
          str(n:n)=')'
       endif
    endif
777 continue
    call more_error(coder%context,trim(str))
  contains

    include 'fesize.inc'
    include 'fisnull.inc'

  end subroutine print_proc_details


end module pm_infer
