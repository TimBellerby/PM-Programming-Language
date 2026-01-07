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
  logical,parameter:: debug_bprop=.false.
  logical,parameter:: debug_bprop_simple=.false.
  logical,parameter:: debug_bprop_tagging=.false.
 
  ! Maximum times a procedure template can call itself with
  ! *different* arguments types each time
  integer,parameter:: max_recur=32
 
  ! Special types
  integer,parameter:: undefined=-1
  integer,parameter:: error_type=-2

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
    type(pm_ptr):: cnode,cblock
    integer:: i

    if(debug_inference) write(*,*) 'INF PROG>'

    coder%flag_recursion=.false.
    coder%trace_depth=0
 
    coder%loop_depth=0

    coder%top=1
    coder%wtop=1
    coder%incomplete=.false.
    coder%taints=0

    coder%poly_cache=pm_dict_new(coder%context,32_pm_ln)
    coder%proc_cache=pm_dict_new(coder%context,32_pm_ln)

    ! Setup resolution stack block
    call create_stack_frame(coder,coder%index)

    ! Process program code
    cblock=top_code(coder)
    call inf_cblock(coder,cblock)

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

    call bprop(coder,cblock,&
         coder%stack(coder%base+1:coder%base+coder%index),.true.)
    
    ! Create resolved code object
    call code_int_vec(coder,coder%stack,coder%base,coder%top)
    call code_num(coder,coder%stack(2))
    call make_code(coder,pm_null_obj,cnode_is_resolved_proc,3)

    ! Finalise inference of procs with polymorphic arguments
    call inf_poly_procs(coder)

    if(debug_inference) write(*,*) 'END OF PROG> vtop=',coder%vtop

  contains
    include 'fnewnc.inc'
    include 'ftiny.inc'
  end subroutine  inf_prog

  ! ====================================================
  ! Type-infer procedure
  ! Returns signature index as tiny int on vstack
  ! ====================================================
  recursive function inf_proc(coder,procnode,callnode,atype,ptype,nret,nkeys,&
       keynames,keybase,proc_nkeys,nomatch,only_when,new_atype,new_envelope) result(rtype)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: procnode,callnode
    integer,intent(in):: atype,ptype
    integer,intent(in):: nret,nkeys,keybase,proc_nkeys
    logical,intent(in):: only_when
    logical,intent(out):: nomatch
    integer,intent(out):: new_atype
    type(pm_ptr),intent(in):: keynames
    type(pm_ptr),intent(out):: new_envelope
   
    integer:: rtype
    integer:: at
    integer,dimension(4+proc_nkeys):: key,base_key
    integer,dimension(proc_nkeys):: key_types,junk
    integer:: i,j,keysize,nk,tno
    integer(pm_ln):: k,kk
    logical:: save_incomplete,save_types_changed
    integer:: taints,save_taints,save_atype,save_new_atype,save_rtype,save_loop_depth
    integer:: keypartyp,keyargtyp,last_key_index,sp_code
    type(pm_ptr):: save_procnode,keys,keytypes
    type(pm_ptr):: cached,cac,base_cache,rt_cache,at_cache,rvec
    logical:: ok,added,change_added,pushed_stack_frame,incomplete
    integer,dimension(3):: rtn_cache
    type(pm_root),pointer:: save_envelope

    new_atype=-1
    new_envelope=pm_null_obj

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

    if(coder%top+1+cnode_get_num(procnode,pr_max_index)>max_code_stack) then
       call inf_error(coder,callnode,&
            'Very deep (probably recursive) set of nested calls that cannot be processed')
       call more_error(coder%context,'Check for recursive procedure calls generating a new type each time')
       call more_error(coder%context,&
            'and also for (mutually) recursive calls in the default value expressions for keyword arguments')
       call inf_trace(coder)
       rtype=error_type
       return
    endif
    
    call save_proc_state
    coder%loop_depth=0
    coder%atype=atype
    coder%proc=procnode
    
    keysize=2
    pushed_stack_frame=.false.

    ! Process keyword arguments - they form part of the hash key
    last_key_index=0
    if(proc_nkeys>0) then
       keys=cnode_get(procnode,pr_keys)
       last_key_index=keys%data%i(keys%offset+pm_fast_esize(keys))
       pushed_stack_frame=.true.
       call new_stack_frame(coder,cnode_get_num(procnode,pr_max_index))
       call init_stack_frame(coder,coder%base,1,coder%base+last_key_index)
       call inf_key_args(coder,callnode,procnode,atype,&
            nkeys,keynames,keybase,key_types,nk,.false.)
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
       if(.not.pushed_stack_frame) call new_stack_frame(coder,cnode_get_num(procnode,pr_max_index))
       call init_stack_frame(coder,coder%base,1,coder%base+last_key_index)
       call inf_arg_types(coder,procnode,atype)
       call inf_cblock(coder,cnode_get(procnode,pr_when))
       pushed_stack_frame=.true.
       tno=get_arg_type(coder,callnode,cnode_get(procnode,pr_whenvar))
       if(debug_inference) then
          write(*,*) 'WHEN>',pm_type_as_string(coder%context,tno)
       endif
       if(tno==coder%false_fix.or.tno==coder%false_literal) then
          call pop_stack_frame(coder)
          nomatch=.true.
          call restore_proc_state
          return
       elseif(tno/=coder%true_fix.and.tno/=coder%true_literal) then
          call inf_error(coder,procnode,&
               '"when" expression must have a fixed or literal bool value')
          call more_error(coder%context,'Type of expression is: '//&
               trim(pm_type_as_string(coder%context,tno)))
          return
       endif
    endif

    if(only_when) then
       call pop_stack_frame(coder)
       call restore_proc_state
       nomatch=.false.
       return
    endif

    ! Lookup combination of proc, arg types and all key types
    ! defined for the procedure (including defaults)
    key(1)=cnode_get_num(procnode,pr_id)
    key(2)=atype 
    do i=3,keysize
       key(i)=key_types(i-2) 
    enddo
    k=pm_ivect_lookup(coder%context,coder%proc_cache,key,keysize)

    if(debug_inference) then
       write(*,*) 'INF PROC>',key(1),key(2),k,&
            trim(pm_name_as_string(coder%context,&
            cnode_get_name(procnode,pr_name))),&
            trim(pm_type_as_string(coder%context,atype))
    endif


    ! This combination already cached
    if(k>0) then
       cached=pm_dict_val(coder%context,coder%proc_cache,k)

       if(debug_inference) then
          write(*,*) 'FOUND',k,'-->',key(1:keysize)
          write(*,*) 'CACHED>',k,cached%data%vkind,cached%offset,&
               trim(pm_name_as_string(coder%context,&
               cnode_get_name(procnode,pr_name))),sp_sig_recursive,sp_sig_in_process
       endif

       ! Dictionary entries in coder%proc_cache:
       ! Key is proc and argument types 
       ! Value is vector of ints with procedure return type, & arg types and taints
       ! or tiny int
       !  (-1) sp_sig_in_process in process of resolution
       !  (-2) sp_sig_recursive called recursively
       !  (-3) sp_sig_break  breaking (or previously broke) out of inference

       taints=0
       incomplete=.false.
       
       if(pm_fast_istiny(cached)) then
   
          sp_code=cached%offset
          if(sp_code==sp_sig_break) then
             at=atype
             goto 10
          elseif(sp_code==sp_sig_recursive) then
             if(coder%flag_recursion) then
                call inf_error(coder,procnode,'Recursive call to: '//&
                     trim(pm_name_as_string(coder%context,&
                     cnode_get_name(procnode,pr_name))))
                call inf_trace(coder)
                coder%flag_recursion=.false.
             endif
             incomplete=.true.
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
             incomplete=.true.
             rtype=error_type
          elseif(sp_code<0) then
             ! Another special sig
             rtype=atype
             call code_num(coder,int(sp_code))
           endif
       elseif(pm_fast_vkind(cached)==pm_int) then
          ! Cached return types
          rtype=cached%data%i(cached%offset)
          new_atype=cached%data%i(cached%offset+1)
          taints=cached%data%i(cached%offset+2)
          if(debug_inference) write(*,*) 'CACHED RETURN>',rtype,&
               trim(pm_type_as_string(coder%context,rtype))
          call code_num(coder,int(k))
       else

          ! Not a special code or set of return types - so have a fully inferred procedure

          ! Cached return types and taints
          taints=cnode_num_arg(cached,3)
          rtype=cnode_num_arg(cached,4)
          new_atype=cnode_num_arg(cached,5)
          new_envelope=cnode_arg(cached,8)
          
          ! Push signature
          call code_num(coder,int(k))

          if(debug_inference) write(*,*) 'CACHED RTYPE>',rtype
          
       endif
       if(pushed_stack_frame) call pop_stack_frame(coder)
       call restore_proc_state
       coder%incomplete=coder%incomplete.or.incomplete
       coder%taints=ior(coder%taints,iand(taints,proc_taints))
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
       call restore_proc_state
       return
    endif

20  continue

    ! Flag call to check for recursion
    call cnode_incr_num(procnode,pr_recurse,1)

    ! Get ready to type infer
    k=pm_idict_add(coder%context,coder%proc_cache,&
         key,keysize,pm_fast_tinyint(coder%context,sp_sig_in_process))
       
    if(.not.pushed_stack_frame) then
       call new_stack_frame(coder,cnode_get_num(procnode,pr_max_index))
    endif

    ! Repeatedly type infer until complete
    do
       if(debug_inference) write(*,*) 'TRY>',key(1),key(2),rtype,trim(pm_name_as_string(coder%context,&
            cnode_get_name(procnode,pr_name)))

       call init_stack_frame(coder,coder%base,last_key_index+1,coder%top)

       ! Process code
       coder%incomplete=.false.
       coder%taints=taints
       coder%new_atype=-1
       coder%rtype=-1
       coder%envelope=pm_null_obj
       
       call inf_cblock(coder,cnode_get(procnode,pr_cblock))

       ! Check  procedure record for recursion/completion
       cached=pm_dict_val(coder%context,coder%proc_cache,k)
   
       if(debug_inference) then
          write(*,*) 'TRY COMPLETE>',cached%offset,nret,trim(pm_name_as_string(coder%context,&
            cnode_get_name(procnode,pr_name)))
       endif

       if(pm_fast_istiny(cached)) then
          sp_code=cached%offset
          if(sp_code==sp_sig_in_process) then
             ! Not recursively called
             rtype=coder%rtype
             new_atype=coder%new_atype
             taints=coder%taints
             if(debug_inference) write(*,*) 'NOT RECURSIVE>',rtype,coder%incomplete
             exit
          else if(sp_code<=sp_sig_recursive) then
             ! Recursively called
             if(nret>0.and.coder%rtype<0) then
                ! No resolved type yet 
                ! flag cache entry
                ! and break out
                call pop_stack_frame(coder)
                sp_code=sp_sig_break
                call pm_dict_set_val(coder%context,&
                     coder%proc_cache,k,cached)
                call restore_proc_state
                coder%incomplete=.true.
                rtype=error_type
                new_atype=-1
                if(debug_inference) write(*,*) 'NOT RESOLVED>'
                return
             endif

             ! Cache resolved return type, new "&" types, taints
             rtn_cache(1)=coder%rtype
             rtn_cache(2)=coder%new_atype
             rtn_cache(3)=ior(coder%taints,proc_is_recursive)
             call code_int_vec(coder,rtn_cache,1,3)
             call pm_dict_set_val(coder%context,coder%proc_cache,k,top_code(coder))
             call drop_code(coder)
          endif
       else
          ! Recursive call for which we 
          ! already have a return type

          if(debug_inference) write(*,*) 'RT>',rtype,coder%stack(coder%base)

          if(pm_fast_vkind(cached)/=pm_int) call pm_panic('Bad cached proc kind')

          ! Get cached return types, changed "&" arg types and taints
          rtype=cached%data%i(cached%offset)
          new_atype=cached%data%i(cached%offset+1)
          taints=cached%data%i(cached%offset+2)

          if(debug_inference) then
             write(*,*) 'RECURSIVE WITH TYPE>',&
                  trim(pm_type_as_string(coder%context,rtype)),' FOR ',&
                  trim(pm_name_as_string(coder%context,cnode_get_num(procnode,pr_name)))
          endif

          ! If returning values or updating "&" arguments, need to check if types have changed
          added=.false.
          if(nret>0) then
             rtype=pm_type_combine(coder%context,rtype,coder%rtype,ok,added)
             if(.not.ok) then
                call inf_error_with_trace(coder,procnode,&
                     'Internal Compiler Error: Procedure return types changed: '//&
                     trim(pm_type_as_string(coder%context,rtype))//'<>'//&
                     trim(pm_type_as_string(coder%context,cached%data%i(cached%offset))))
             endif
          endif
          if(coder%new_atype/=-1) then
             new_atype=pm_type_combine(coder%context,coder%new_atype,new_atype,ok,change_added)
             if(.not.ok) then
                call inf_error_with_trace(coder,procnode,&
                     'Internal Compiler Error: Procedure returned "&" arg types changed')
             endif
             added=added.or.change_added
          endif
          if(ior(taints,coder%taints)/=taints) then
             added=.true.
             taints=ior(taints,coder%taints)
          endif
          if(added) then
             cached%data%i(cached%offset)=rtype
             cached%data%i(cached%offset+1)=new_atype
             cached%data%i(cached%offset+2)=taints
             cycle
          endif
          
          ! Flag procedure as recursive
          taints=ior(taints,proc_is_recursive)

          ! Inference is completed
          exit
       endif
    enddo

    if(debug_inference) then
       write(*,*) 'COMPLETED>',trim(pm_name_as_string(coder%context,&
            cnode_get_name(procnode,pr_name))),' ',k,coder%incomplete
    endif

    ! Pass a break out
    if(coder%incomplete) then
       if(debug_inference) then
          write(*,*) 'INCOMPLETE>',trim(pm_name_as_string(coder%context,&
            cnode_get_name(procnode,pr_name))),' ',k,coder%incomplete
       endif
       call pop_stack_frame(coder)
       ! clear cache entry
       cached%offset=sp_sig_break
       call pm_dict_set_val(coder%context,&
            coder%proc_cache,k,cached)
       if(rtype>=0) then
          call code_num(coder,int(k))
       endif
       call restore_proc_state
       coder%incomplete=.true.
       return
    endif

    ! Flag recursive calls with taints or keyword args as unfinished
    taints=iand(coder%taints,proc_taints)

    ! Determine a hash key with any polymorphic elements eliminated
    added=.false.
    base_key(1)=key(1)
    do i=2,keysize
       base_key(i)=pm_type_strip_poly(coder%context,key(i))
       added=added.or.(base_key(i)/=key(i))
    end do

    ! If the stripped-down hash key is different then need to create a record
    ! of this 
    if(added) then
       kk=pm_ivect_lookup(coder%context,coder%poly_cache,base_key,keysize)
       if(kk>0) then
          base_cache=pm_dict_val(coder%context,coder%poly_cache,kk)
          base_cache=cnode_arg(base_cache,2)
          do i=2,keysize
             base_cache%data%i(base_cache%offset+i-1)=&
                  pm_type_combine(coder%context,&
                  base_cache%data%i(base_cache%offset+i-1),key(i),ok,change_added)
          enddo
       else
          call code_val(coder,procnode)
          call code_int_vec(coder,key,1,keysize)
          rtn_cache(1)=rtype
          rtn_cache(2)=new_atype
          rtn_cache(3)=taints
          call code_int_vec(coder,rtn_cache,1,3)
          call make_code(coder,pm_null_obj,cnode_is_resolved_proc,3)
          kk=pm_idict_add(coder%context,coder%poly_cache,&
               base_key,keysize,top_code(coder))
          call drop_code(coder)
       endif
    endif
  
    ! Create record of type-annotated code
    call code_val(coder,procnode)
    if(added) then
       call code_num(coder,int(kk))
    else
       call code_int_vec(coder,coder%stack,coder%base,coder%top)
       rvec=top_code(coder)
    endif
    call code_num(coder,&
         ior(iand(cnode_get_num(procnode,pr_flags),&
         proccall_is_comm+proccall_is_inline+proccall_is_no_inline),&
         coder%taints))
    call code_num(coder,rtype)
    call code_num(coder,new_atype)

    ! Back-prop pass - push use info vectors for args and keys
    if(debug_bprop) write(*,*) 'BPROP>', trim(pm_name_as_string(coder%context,&
         cnode_get_name(procnode,pr_name)))
    
    if(added) then
       call bprop(coder,cnode_get(procnode,pr_cblock),&
            coder%stack(coder%base+1:coder%base+cnode_get_num(procnode,pr_max_index)),&
            .false.)
    else
       call bprop(coder,cnode_get(procnode,pr_cblock),&
            rvec%data%i(rvec%offset+1:rvec%offset+cnode_get_num(procnode,pr_max_index)),&
            .true.)

    endif
    if(proc_nkeys==0) then
       call code_null(coder)
    else
       call swap_code(coder)
    endif
    
    call code_val(coder,coder%envelope)

    new_envelope=coder%envelope
    
    ! Create record -- proc resvec flags rtype new_atype arg_uses key_uses
    call make_code(coder,pm_null_obj,cnode_is_resolved_proc,8)
    call pm_dict_set_val(coder%context,coder%proc_cache,k,top_code(coder))
    call drop_code(coder)
    call code_num(coder,int(k))

    ! Pop frame
    call pop_stack_frame(coder)
    call cnode_incr_num(procnode,pr_recurse,-1)
    call restore_proc_state
    
    if(debug_inference) then
       write(*,*) 'ENDPROCNODE>',trim(pm_name_as_string(coder%context,&
            cnode_get_name(procnode,pr_name))),k,coder%taints
    endif

    ! Pass out taint information
    coder%proc_taints=iand(coder%taints,proc_taints)
    coder%taints=ior(save_taints,coder%proc_taints)

  contains
    include 'fnewnc.inc'
    include 'fistiny.inc'
    include 'ftiny.inc'
    include 'fvkind.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
    
    subroutine save_proc_state
      save_loop_depth=coder%loop_depth
      save_types_changed=coder%types_changed
      save_incomplete=coder%incomplete
      save_taints=coder%taints
      save_procnode=coder%proc
      save_atype=coder%atype
      save_new_atype=coder%new_atype
      save_rtype=coder%rtype
      save_envelope=>pm_add_root(coder%context,coder%envelope)
    end subroutine save_proc_state
    
    subroutine restore_proc_state
      coder%incomplete=save_incomplete
      !coder%taints=save_taints
      coder%proc=save_procnode
      coder%atype=save_atype
      coder%new_atype=save_new_atype
      coder%rtype=save_rtype
      coder%loop_depth=save_loop_depth
      coder%types_changed=save_types_changed
      coder%envelope=save_envelope%ptr
      call pm_delete_root(coder%context,save_envelope)
    end subroutine restore_proc_state
    
  end function inf_proc

  !==================================================
  ! Resolve all procs with poly arguments listed in
  ! poly_cache
  !=================================================
  subroutine inf_poly_procs(coder)
    type(code_state),intent(inout):: coder
    integer(pm_ln):: i,k,kk
    integer:: j,n,atype,key_type,taints
    type(pm_ptr):: cached,procnode,keys,rtns,keyargs,junk
    logical:: ok
    do i=pm_dict_size(coder%context,coder%poly_cache),1,-1
       ! Details of this proc
       cached=pm_dict_val(coder%context,coder%poly_cache,i)
       procnode=cnode_arg(cached,1)
       keys=cnode_arg(cached,2)
       rtns=cnode_arg(cached,3)
       atype=keys%data%i(keys%offset+1)
       n=pm_fast_esize(keys)-1
       taints=rtns%data%i(rtns%offset+2)

       ! For a recursive proc, make sure that there is an resolved entry in proc_cache
       ! to handle any nested recursive calls
       if(iand(taints,proc_is_recursive)/=0) then
          junk=pm_dict_lookup(coder%context,coder%proc_cache,keys,kk)
          if(kk==0) then
             call code_val(coder,procnode)
             call code_num(coder,int(i))
             call code_num(coder,taints)
             call code_num(coder,rtns%data%i(rtns%offset))
             call code_num(coder,rtns%data%i(rtns%offset+1))
             call make_code(coder,pm_null_obj,cnode_is_resolved_proc,5)
             call pm_dict_set(coder%context,coder%proc_cache,keys,top_code(coder),.true.,.true.,ok)
             call drop_code(coder)
          endif
       endif

       call create_stack_frame(coder,cnode_get_num(procnode,pr_max_index))

       ! Handle keyword arguments
       if(n>0) then
          call inf_arg_types(coder,procnode,atype)
          keyargs=cnode_get(procnode,pr_keycall)
          do j=1,n
             call inf_cblock(coder,cnode_arg(keyargs,j*2-1+n+n))
             key_type=keys%data%i(keys%offset+j+1)
             call set_var_type(coder,cnode_arg(keyargs,j),key_type)
             call set_var_type(coder,cnode_arg(keyargs,j+n),key_type)
          enddo
       end if

       ! Infer main body
       coder%atype=atype
       coder%incomplete=.false.
       call inf_cblock(coder,cnode_get(procnode,pr_cblock))

       ! Set this entry in poly_cache to inference vector
       call code_int_vec(coder,coder%stack,coder%base,coder%top)
       call pm_dict_set_val(coder%context,coder%poly_cache,i,top_code(coder))
       call drop_code(coder)
    enddo
  contains
    include 'fesize.inc'
  end subroutine inf_poly_procs
  
  !=================================================
  ! Infer conventional (non-keyword) argument types
  ! (this routine mainly used as part of inferring
  ! keyword argument types)
  !=================================================
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
  recursive subroutine inf_key_args(coder,callnode,procnode,atype,nkeys,call_keys,key_base,&
       key_types,n,combine)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,procnode,call_keys
    integer,intent(in):: atype,nkeys,key_base
    integer,intent(out):: key_types(:),n
    logical,intent(in):: combine
    integer,dimension(size(key_types)):: keytypes
    integer i,j,cname,pname,ctype,ptype,dtype,pdtype,mtype
    logical:: nomatch,error,ok,added
    type(pm_ptr):: callkeys,proc_keys,arglist,tv
    integer:: nargs,totargs,tno,keytype
    
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
             keytype=mtype
          endif
       else
          keytype=dtype
       endif
       if(combine) then
          key_types(i)=pm_type_combine(coder%context,key_types(i),keytype,ok,added)
       else
          key_types(i)=keytype
       endif
       call set_var_type(coder,cnode_arg(arglist,i),key_types(i))
       call set_var_type(coder,cnode_arg(arglist,i+n),key_types(i))
    enddo
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine inf_key_args
    
  ! ==================================================
  ! Type infer builtin procedure
  ! ===================================================
  function inf_builtin(coder,procnode,callnode,atype,ptype,new_atype) result(rtype)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: procnode,callnode
    integer,intent(in):: atype,ptype
    integer,intent(out):: new_atype
    integer:: rtype,mode,atype1
    integer,dimension(1):: key
    integer:: k,t1,t2,n,opcode
    type(pm_ptr):: tv,v,args,arg
    logical:: isstatic,iscomm,ok,added

    new_atype=-1
    
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

    opcode=cnode_get_num(procnode,bi_opcode)

    if(opcode==op_error_type) then
       rtype=error_type
       return
    endif
    
    if(cnode_flags_set(procnode,pr_flags,proccall_is_comm)) then
       atype1=pm_type_arg(coder%context,atype,1+num_comm_args)
    else
       atype1=pm_type_strip_mode(coder%context,&
            pm_type_arg(coder%context,atype,2),mode)
    endif

    ! special handling of return types for some operations
    select case(opcode)
    case(first_fold:last_fold)
       rtype=fold(coder,procnode,atype,rtype)
       call code_num(coder,sp_sig_setval)
       goto 10
    case(op_clone_var)
       mode=cnode_get_num(procnode,bi_opcode2)
       rtype=pm_type_for_var(coder%context,atype1)
       if(mode/=0) rtype=pm_type_add_mode(coder%context,rtype,mode)
       call code_num(coder,sp_sig_dup)
       goto 10
    case(op_clone)
       mode=cnode_get_num(procnode,bi_opcode2)
       rtype=pm_type_for_var(coder%context,atype1)
       if(mode/=0) rtype=pm_type_add_mode(coder%context,rtype,mode)
    case(op_setref)
       rtype=atype1
       call code_num(coder,sp_sig_link)
       goto 10
    case(op_link_var)
       rtype=atype1
    case(op_noop)
       call code_num(coder,sp_sig_noop)
       goto 10
    case(op_init_var)
       new_atype=pm_type_arg(coder%context,atype,3)
       call code_num(coder,sp_sig_init)
       goto 10
    case(op_init_const)
       new_atype=pm_type_arg(coder%context,atype,3)
       call code_num(coder,sp_sig_init)
       goto 10
    case(op_array_get_elem,op_extractelm)
       rtype=pm_type_arg(coder%context,atype1,1)
    case(op_make_rf)
       rtype=pm_type_arg(coder%context,atype1,1)
       if(.not.pm_is_compiling) then
          call push_word(coder,pm_type_new_dref)
          call push_word(coder,0)
          call push_word(coder,rtype)
          call make_type(coder,3)
          rtype=pop_word(coder)
       endif
    case(op_get_dom)
       rtype=pm_type_arg(coder%context,atype1,2)
    case(op_as,op_get_poly_or)
       rtype=pm_type_arg(coder%context,atype,3)
    case(op_import_varg,op_broadcast_val,&
         op_get_rf)
       rtype=atype1
    case(op_elem)
       n=cnode_get_num(procnode,bi_opcode2)
       if(n/=0) then
          tv=pm_type_vect(coder%context,atype1)
          k=pm_tv_kind(tv)
          rtype=pm_tv_arg(tv,n-1)
       else
          tv=pm_type_vect(coder%context,atype)
          t1=pm_type_strip_mode(coder%context,pm_tv_arg(tv,3),mode)
          v=pm_type_val(coder%context,t1)
          n=v%data%ln(v%offset)
          t1=pm_type_strip_mode(coder%context,pm_tv_arg(tv,2),mode)
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
    case(op_array,op_make_array,op_var_array)
       t1=pm_type_arg(coder%context,atype,3)
       if(pm_is_compiling) then
          t2=t1
       else
          t2=pm_type_arg(coder%context,t1,1)
       endif
       if(opcode==op_var_array) then
          rtype=pm_new_arr_type(coder%context,sym_var,&
               pm_type_for_var(coder%context,atype1),&
               pm_type_for_var(coder%context,pm_type_arg(coder%context,atype,3)),t2)
       else
          rtype=pm_new_arr_type(coder%context,sym_const,&
               pm_type_for_var(coder%context,atype1),&
               t1,t2)
       endif
       if(opcode==op_make_array) rtype=pm_type_add_mode(coder%context,rtype,sym_shared)
!!$    case(op_redim)
!!$       tv=pm_type_vect(coder%context,atype1)
!!$       rtype=pm_new_arr_type(coder%context,pm_tv_name(tv),&
!!$            pm_tv_arg(tv,1),&
!!$            pm_type_arg(coder%context,atype,3),int(pm_long))
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
    case(op_export_param)
       t1=pm_type_strip_mode(coder%context,atype1,mode)
       if(mode<sym_invar) then
          rtype=pm_error_type_from_string(coder%context,'Can only access '//&
               '"shrd" or "invar" values in a "gbl" procedure, not: '//&
               trim(sym_names(mode)))
       else
          rtype=t1  ! private mode 
       endif
    case(op_list_concat)
       call infer_list_concat
    case(op_list_splice)
       call infer_list_splice
    case(op_assign)
       if(pm_type_kind(coder%context,atype1)==pm_type_is_uninitialised) then
          new_atype=pm_type_arg(coder%context,atype,3)
          call code_num(coder,sp_sig_init)
          goto 10
       else
          new_atype=pm_type_combine(coder%context,&
               pm_type_arg(coder%context,atype,2),pm_type_arg(coder%context,atype,3),&
               ok,added)
          call code_num(coder,sp_sig_assign)
          goto 10
       endif
    case(op_array_set_elem)
       new_atype=pm_new_arr_type(coder%context,pm_type_name(coder%context,atype1),&
            pm_type_combine(coder%context,&
            pm_type_arg(coder%context,atype1,1),&
            pm_type_arg(coder%context,atype,4),ok,added),&
            pm_type_arg(coder%context,atype1,2),&
            pm_type_arg(coder%context,atype1,3))
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
  recursive subroutine inf_cblock(coder,cblock)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    integer:: save_taints
    type(pm_ptr):: p
    if(pm_fast_isnull(cblock)) return
    save_taints=coder%taints
    coder%taints=0
    p=cnode_get(cblock,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       call inf_call(coder,cblock,p)      
       p=cnode_get(p,call_link)
    enddo
    coder%stack(coder%base+cnode_get_num(cblock,cblock_index))=coder%taints
    coder%taints=ior(save_taints,coder%taints)
  contains
    include 'fisnull.inc'
  end subroutine inf_cblock

  !==========================================
  ! Return tainta associated with a block
  ! - inference on block must be complete
  !==========================================
  function cblock_taints(coder,cblock) result(taints)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    integer:: taints
    taints=coder%stack(coder%base+cnode_get_num(cblock,cblock_index))
  end function cblock_taints

  !=======================================================
  ! Type infer general calls
  ! (which include control structures as a special case)
  !========================================================
  recursive subroutine inf_call(coder,cblock,callnode)
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
          coder%loop_depth=coder%loop_depth+1
          counter=0
          do
             if(coder%loop_depth==1) coder%types_changed=.false.
             call inf_cblock(coder,list)
             call check_logical(2,sig==sym_while_invar)
             if(arg_type(2)==coder%false_fix) return
             call inf_cblock(coder,list2)
             if(.not.coder%types_changed.or.coder%loop_depth>1) exit
             counter=counter+1
             if(counter>max_recur) then
                call inf_error_with_trace(coder,args,&
                     '"while" appears to lead to infinite types')
                exit
             endif
          enddo
          coder%loop_depth=coder%loop_depth-1
          if(sig/=sym_while) call mark_loop_cond(5)
       case(sym_until,sym_until_invar,sym_each)
          call check_loop_writes(3)
          list=cnode_arg(args,1)
          coder%loop_depth=coder%loop_depth+1
          counter=0
          do 
             if(coder%loop_depth==1) coder%types_changed=.false.
             call inf_cblock(coder,list)
             if(.not.coder%types_changed.or.coder%loop_depth>1) exit
             counter=counter+1
             if(counter>max_recur) then
                call inf_error_with_trace(coder,args,&
                     trim(sym_names(sig))//' appears to lead to infinite types')
                exit
             endif
          enddo
          call check_logical(2,sig==sym_until_invar)
          coder%loop_depth=coder%loop_depth-1
          if(sig/=sym_until) call mark_loop_cond(5)
       case(sym_if,sym_if_invar)
          call inf_if(count_updates(cnode_arg(args,4),2),sig==sym_if_invar)
       case(sym_pm_for)
          call inf_pm_for
       case(sym_pm_over)
          call inf_cblock(coder,cnode_arg(args,nargs+nret))
       case(sym_task)
          do i=1,nargs,3
             call inf_cblock(coder,cnode_arg(args,i))
             call inf_cblock(coder,cnode_arg(args,i+2))
          enddo
       case(sym_do)
          call inf_cblock(coder,cnode_arg(args,1))
       case(sym_pm_invar)
          call inf_cblock(coder,cnode_arg(args,nret+1))
          if(nret>1) call combine_types(cnode_arg(args,1),arg_type_with_mode(3))
       case(sym_sync,sym_pm_shared,sym_pm_shared_always,sym_pm_chan,sym_pm_chan_always)
          call inf_cblock(coder,cnode_arg(args,2))
       case(sym_pct)
          call inf_cblock(coder,cnode_arg(args,nargs))
       case(sym_null)
          tno=pm_type_add_mode(coder%context,int(pm_null),sym_invar)
          do i=1,nret
             coder%stack(get_slot(i))=tno
          enddo
       case(sym_assign_sync)
          call combine_types(cnode_arg(args,1),int(pm_logical))
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
       case(sym_pm_ref)
          call push_word(coder,pm_type_new_dref)
          call push_word(coder,cnode_num_arg(args,2))
          do i=3,nargs+1
             call push_word(coder,arg_type(i))
          enddo
          call make_type(coder,nargs+1)
          call combine_types(cnode_arg(args,1),pop_word(coder))
       case(sym_pm_each_index)
          call inf_each_index
       case(sym_pm_set_dotdotdot)
          tv=pm_type_vect(coder%context,arg_type(2))
          call push_word(coder,pm_type_new_tuple)
          call push_word(coder,0)
          n=pm_tv_numargs(tv)
          do i=1,n
             call push_word(coder,pm_tv_arg(tv,i))
          enddo
          call make_type(coder,n+2)
          coder%stack(get_slot(1))=pop_word(coder)
       case(sym_pm_envelope)
          wbase=coder%wtop
          do i=2,nargs
             call push_word(coder,arg_type(i))
          enddo
          call combine_nhd_envelope(coder,callnode,coder%wstack(wbase+1:coder%wtop),nargs/2)
          coder%wtop=wbase
       case(sym_rec)
          call inf_rec
       case(sym_pm_list,sym_update_list)
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
       case(sym_pm_import_list)
          tno=arg_type_with_mode(2)
          call combine_types(cnode_arg(args,1),pm_type_imported(coder%context,tno,ok))
       case(sym_dot,sym_dot_ref,sym_simple_list_elem)
          name=arg_type(3)
          tno=arg_type_with_mode(2)
          if(tno==error_type.or.name==error_type) then
             call set_arg_to_error_type(1)
          else
             if(tno>0) then
                call set_call_sig(resolve_elem(cnode_arg(args,2),tno,name,&
                     sig==sym_dot_ref,.false.,sig==sym_simple_list_elem,tno2))
                call combine_types(cnode_arg(args,1),tno2)
             else
                call set_arg_to_error_type(1)
             endif
          endif
       case(sym_get_dot,sym_get_dot_ref,sym_get_list_elem)
          ! Used in .{ }
          ! Check type is literal string or literal integer
          ! Cater for literal string of the form "_name"
          tno=arg_type(3)
          if(tno>0) then
             if(pm_type_kind(coder%context,tno)/=pm_type_is_single_name) then
                tv=pm_type_vect(coder%context,tno)
                if(pm_tv_kind(tv)==pm_type_is_literal_value.or.pm_tv_kind(tv)==pm_type_is_fix_value) then
                   tno2=pm_tv_arg(tv,1)
                   if(tno2==pm_string_type) then
                      tno=pm_name_type_from_literal_string(coder%context,tno,&
                           cnode_module_name(callnode))
                      if(tno<0) then
                         call inf_error(coder,callnode,&
                              'String value in ".{}" is not a valid name')
                         tno=error_type
                      endif
                   elseif(tno2/=pm_long) then
                      call inf_error(coder,callnode,&
                           'Expression in ".{}" must be a literal string or integer')
                      call more_error(coder%context,'Got: '//trim(pm_type_as_string(coder%context,tno2)))
                      call inf_trace(coder)
                      tno=error_type
                   endif
                else
                   call inf_error(coder,callnode,&
                        'Expression in ".{}" must be a literal string or integer')
                   call more_error(coder%context,'Got: '//trim(pm_type_as_string(coder%context,arg_type(2))))
                   call inf_trace(coder)
                   tno=error_type
                endif
             endif
          endif
          name=tno
          tno=arg_type_with_mode(2)
          if(tno==error_type.or.name==error_type) then
             call set_arg_to_error_type(1)
          else
             if(tno>0) then
                call set_call_sig(resolve_elem(cnode_arg(args,2),tno,name,&
                     sig==sym_get_dot_ref,.false.,sig==sym_get_list_elem,tno2))
                call combine_types(cnode_arg(args,1),tno2)
             else
                call set_arg_to_error_type(1)
             endif
          endif
       case(sym_cast)
          ! Arg 3 is type to cast to (-ve if in a conditional context)
!!$          write(*,*) 'CAST',pm_type_as_string(coder%context,arg_type(2)),&
!!$               pm_type_as_string(coder%context,arg_type(3))
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
          k=inf_cast(coder,callnode,tno,tno2)
          if(k<0) then
             call set_arg_to_error_type(1)
          else
             call set_call_sig(int(k))
             call combine_types(cnode_arg(args,1),&
                  pm_type_add_mode(coder%context,tno2,mode))
          endif
       case(sym_var_set_mode)
          mode2=cnode_num_arg(args,2)
          coder%stack(get_slot(1))=pm_type_add_mode(coder%context,&
               pm_type_strip_mode(coder%context,&
               arg_type_with_mode(1),mode),mode2)
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
       case(sym_pm_assign)
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
          tno=arg_type(2)
          if(tno==coder%false_fix.or.tno==coder%false_literal) then
             tno2=pm_type_strip_mode(coder%context,arg_type(1),mode)
             t=pm_type_vect(coder%context,tno2)
             if(pm_tv_kind(t)==pm_type_is_literal_value) then
                if(pm_tv_arg(t,1)==pm_string_type) then
                   call pm_strval(pm_type_val(coder%context,tno2),str)
                   call inf_error_with_trace(coder,callnode,str(1:len_trim(str)))
                else
                   call inf_error_with_trace(coder,callnode,&
                        'Check condition will always fail and check message is not a string') 
                endif
             else
                call inf_error_with_trace(coder,callnode,&
                     'Check condition will always fail') 
             endif
          elseif(pm_type_strip_to_basic(coder%context,arg_type(1))/=pm_string_type&
               .and.arg_type(1)/=error_type) then
             call inf_error_with_trace(coder,cnode_arg(args,1),&
                  'Check message is not a string, got:'//&
                  trim(pm_type_as_string(coder%context,arg_type(1))))
          elseif(tno/=coder%true_fix.and.tno/=coder%true_literal) then
             call check_logical(2,.false.)
             coder%taints=ior(coder%taints,proc_is_impure)
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
             if(sig==sym_fix) then
                tno=pm_new_fix_value_type(coder%context,pm_type_val(coder%context,tno),&
                     pm_tv_name(t))
             endif
          else
             if(sig==sym_literal) then
                tno=pm_new_literal_value_type(coder%context,pm_null_obj,0,tno)
             endif
          endif
          coder%stack(get_slot(1))=tno
       case(sym_check_par_state)
          if(arg_type(2)==pm_null) then
             call pm_strval(cnode_arg(cnode_arg(args,1),1),str)
             call inf_error_with_trace(coder,callnode,&
                  trim(str)//' cannot be executed in a block which has been invoked from a non-parallel context')
          endif
       case(sym_dcaret)
          coder%stack(get_slot(1))=pm_type_add_mode(coder%context,&
               pm_new_vect_type(coder%context,arg_type(2)),sym_shared)
       case(sym_open)
          if(nargs>0) then
             t=pm_type_vect(coder%context,coder%atype)
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
       case(sym_key)
          ! This is inferred in trav_proc
          continue
       case(sym_present)
          call combine_types(cnode_arg(args,1),int(pm_logical))
       case(sym_typeof)
          tno=get_arg_type(coder,callnode,cnode_arg(args,2),&
               init=call_takes_uninit)
          if(tno>0) then
             if(pm_type_kind(coder%context,tno)==pm_type_is_uninitialised) then
                tno=pm_type_arg(coder%context,tno,1)
             endif
             coder%stack(get_slot(1))=pm_new_type_type(coder%context,tno)
          else
             call set_arg_to_error_type(1)
          endif
       case(sym_result)
          call get_arg_types_and_modes
          call make_type_if_possible(coder,nargs+2)
          coder%rtype=pop_word(coder)
       case(sym_amp)
          call get_arg_types_and_modes
          call make_type_if_possible(coder,nargs+2)
          coder%new_atype=pop_word(coder)
       case(sym_update_from_list)
          tno=get_arg_type(coder,callnode,cnode_arg(args,nret+1))
          if(tno>0) then
             tv=pm_type_vect(coder%context,tno)
             do i=1,nret
                call combine_types(cnode_arg(args,i),pm_tv_arg(tv,i))
             enddo
          else
             do i=1,nret
                call set_arg_to_error_type(i)
             enddo
          endif
       case(sym_move)
          n=nargs/2
          do i=1,n
             coder%stack(get_slot(i))=coder%stack(get_slot(i+n))
          enddo
          do i=n+1,nargs
             coder%stack(get_slot(i))=pm_new_uninitialised_type(coder%context,arg_type_with_mode(i))
          enddo
       case(sym_underscore,sym_colon)
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
          if(arg_type_with_mode(1)>0) then
             call cnode_error(coder,callnode,'Type inference gives: '//&
                  trim(pm_type_as_string(coder%context,arg_type_with_mode(1))),warn=.true.)
          else
             call cnode_error(coder,callnode,'Type inference fails',warn=.true.)
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

    recursive subroutine inf_if(nupdates,isinvar)
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
               save_var_types(i)=get_var_type(coder,callnode,var,init=call_takes_uninit)
               p=p%data%ptr(p%offset+1)
               i=i+1
            end do
            call inf_cblock(coder,cnode_arg(args,2))
            i=1
            p=writelist
            do while(.not.pm_fast_isnull(p))
               var=p%data%ptr(p%offset)
               typ=save_var_types(i)
               save_var_types(i)=get_var_type(coder,callnode,var,init=call_takes_uninit)
               call set_var_type(coder,var,typ)
               p=p%data%ptr(p%offset+1)
               i=i+1
            end do
            call inf_cblock(coder,cnode_arg(args,3))
            i=1
            p=writelist
            do while(.not.pm_fast_isnull(p))
               var=p%data%ptr(p%offset)
               call combine_var_type(coder,callnode,var,save_var_types(i),no_init=.true.)
               p=p%data%ptr(p%offset+1)
               i=i+1
            end do
         endif
      else
         call inf_cblock(coder,cnode_arg(args,3))
      endif
    end subroutine inf_if

    recursive subroutine inf_any(nupdates)
      integer,intent(in):: nupdates
      integer,dimension(nupdates):: init_var_types,final_var_types
      integer:: i,j,slot,slot2
      type(pm_ptr):: changelist,writelist,list,list2,var,p,tv
      list2=cnode_arg(args,4)
      list2=cnode_arg(list2,1)
      changelist=cnode_arg(args,5)
      writelist=cnode_arg(changelist,2)
      slot=list2%data%i(list2%offset)
      slot2=list2%data%i(list2%offset+1)
      tno=pm_type_strip_mode(coder%context,arg_type(3),mode)
      if(tno/=error_type) then
         tv=pm_type_vect(coder%context,tno)
         n=pm_tv_numargs(tv)
         j=1
         p=writelist
         do while(.not.pm_fast_isnull(p))
            var=p%data%ptr(p%offset)
            init_var_types(j)=get_var_type(coder,callnode,var,init=call_takes_uninit)
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
            coder%stack(coder%base+slot:coder%base+slot2)=undefined
            call set_arg_to_type(1,pm_type_add_mode(coder%context,pm_tv_arg(tv,i),mode))
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

    recursive subroutine inf_rec
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
         namep=pm_name_val(coder%context,pm_tv_name(t2))
         call inf_error_with_trace(coder,callnode,&
              'Cannot use a shared value'//&
              ' in "rec" expression to initialise: '//&
              trim(pm_name_as_string(coder%context,&
              namep%data%i(namep%offset-mode))))
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
                 name)
            call more_error(coder%context,'Expected: '//trim(pm_type_as_string(coder%context,tno)))
            call more_error(coder%context,'Got:      '//trim(pm_type_as_string(coder%context,tno2)))
            call inf_trace(coder)
            tno2=error_type
         endif
      endif
      tno2=pm_type_add_mode(coder%context,tno2,mode)
      call combine_types(cnode_arg(args,1),tno2)
    end subroutine inf_rec

    recursive subroutine inf_each_index()
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
            call inf_error(coder,callnode,&
                 'Internal error: PM__each_index: not a literal or fix int parameter')
         endif
      else
         n=0
      endif
      if(nret>1) then
         call push_word(coder,pm_type_new_tuple+pm_type_is_list)
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
            call push_word(coder,arg_type_with_mode(nret+4))
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
    ! Infer PM__for
    !==================================================================
    recursive subroutine inf_pm_for
      integer:: i,n,tno,key(1)
      logical:: ok
      integer(pm_ln):: k
      type(pm_root),pointer:: root
      n=pm_type_int_value(coder%context,arg_type(3),ok)
      if(.not.ok) then
         call inf_error_with_trace(coder,callnode,'Bad rank in PM__for')
         return
      endif
      call push_word(coder,pm_type_new_tuple+pm_type_is_list)
      call push_word(coder,0)
      do i=1,n*4
         call push_word(coder,int(pm_long))
      enddo
      call make_type_if_possible(coder,n*4+2)
      call combine_types(cnode_arg(args,1),pop_word(coder))
      root=>pm_add_root(coder%context,coder%envelope)
      coder%envelope=pm_null_obj
      call inf_cblock(coder,cnode_arg(args,nargs+nret))
      if(.not.pm_fast_isnull(coder%envelope)) then
         call code_val(coder,coder%envelope)
         call make_code(coder,pm_null_obj,cnode_is_any_sig,1)
         key(1)=pm_dict_size(coder%context,coder%proc_cache)
         k=pm_idict_add(coder%context,coder%proc_cache,&
              key,1,top_code(coder))
         call drop_code(coder)
      else
         k=0
      endif
      call set_call_sig(int(k))
      coder%envelope=root%ptr
      call pm_delete_root(coder%context,root)
    end subroutine inf_pm_for

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
    recursive function resolve_elem(var,tno,nametyp,isref,isopt,islist,elem_type) result(sig)
      type(pm_ptr),intent(in):: var
      integer,intent(in):: tno,nametyp
      logical,intent(in):: isref,isopt,islist
      integer,intent(out):: elem_type
      integer:: sig,tk
      type(pm_ptr):: svec

      sig=pm_type_find_elem(coder%context,tno,nametyp,isref,islist,&
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
                    cnode_var_name(var))
               coder%stack(cnode_get_num(var,var_index)+coder%base)=error_type
            elseif(tk/=pm_type_is_rec.and.tk/=pm_type_is_tuple) then
               call inf_error(coder,callnode,&
                    'Cannot take an element of a value of type: "'//&
                    trim(pm_type_as_string(coder%context,tno))//'": ',&
                    cnode_var_name(var))
            else
               sig=pm_type_find_elem(coder%context,tno,nametyp,.false.,islist,&
                    elem_type)
               if(sig==0) then
                  call inf_error_with_trace(coder,callnode,&
                       'An object of type "'//trim(pm_type_as_string(coder%context,tno))//'"'//&
                       ' does not have element "'//&
                       trim(pm_type_as_string(coder%context,nametyp))//'"')
               else
                  call inf_error_with_trace(coder,callnode,&
                       'Cannot modify element "'//&
                       trim(pm_type_as_string(coder%context,nametyp))//&
                       '" of type "'//&
                       trim(pm_type_as_string(coder%context,tno))//'"')
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
  recursive subroutine inf_proc_call(coder,cblock,callnode,sig,args,num_args,nret)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,cblock,args
    integer,intent(in):: sig,num_args,nret
    logical:: is_comm,is_cond,is_unlabelled,is_invar,ignore_rules
    integer:: name,mode,mode2,i,j,tno,tno2,slot,flags
    integer:: nargs,nkey,keybase,ressig,amps
    logical:: undef_arg,bad_amp
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
    is_invar=iand(flags,call_is_invar)/=0
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
    call push_word(coder,pm_type_new_tuple)
    call push_word(coder,amps)
    call check_wstack(coder,nargs)

    do i=1,nargs
       tno=get_arg_type(coder,callnode,cnode_arg(args,i+nret),init=merge(flags,0,i==2))
       coder%wstack(coder%wtop+i)=tno
       undef_arg=undef_arg.or.tno<=0
    enddo

    if(is_comm) then 
       if(is_cond) then
          coder%wstack(coder%wtop+2)=coder%true_literal
       else
          is_cond=coder%wstack(coder%wtop+2)==coder%true_literal
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

    bad_amp=.false.
    if(amps/=0.and..not.ignore_rules) then
       
       amplocs=pm_name_val(coder%context,amps)
       do i=0,pm_fast_esize(amplocs)
!!$          write(*,*) 'update',&
!!$               trim(pm_type_as_string(coder%context,coder%wstack(coder%wtop+amplocs%data%i(amplocs%offset+i))))
          mode2=pm_type_get_mode(coder%context,&
               coder%wstack(coder%wtop+amplocs%data%i(amplocs%offset+i)))
          if(is_invar) then
             if(mode2/=sym_invar.and.(mode2/=sym_shared.or..not.is_comm)) then
                call call_error('Cannot modify a "'//trim(sym_names(mode2))//&
                     '" value in an "invar" statement')
             endif
          else
             if(mode2/=sym_private) then
                if(is_comm) then
                   if(is_unlabelled) then
                      call call_error('Cannot modify a "'//trim(sym_names(mode2))//&
                           '" value in an unlabelled conditional context')
                      bad_amp=.true.
                   endif
                else
                   call call_error('"'//trim(sym_names(mode2))//&
                        '" value can only be modified by an "invar" statement, "sync" assignment or "%" call')
                   bad_amp=.true.
                endif
             endif
          endif
       enddo
    endif
    
    ! Standard calls
    if(.not.is_comm) then
       
       if(debug_inference) then
          do i=1,nargs
             write(*,*) 'PRE-STRIPPED',&
                  trim(pm_type_as_string(coder%context,coder%wstack(coder%wtop+i)))
          enddo
       endif
           
       ! Implement mode combination rule for standard procedures (ignore topology arg)
       if(iand(flags,call_returns_private)/=0) then
          mode=sym_private
       else
          mode=pm_type_combine_modes(coder%context,&
               coder%wstack(coder%wtop+2:coder%wtop+nargs),is_cond,&
               ignore_rules)
          if(nkey>0) then
             mode=pm_type_combine_modes(coder%context,&
                  coder%wstack(keybase+1:keybase+nkey),is_cond,&
                  ignore_rules,mode)
          endif
          if(mode<0) then
             call call_error('Cannot pass a shared value to a standard procedure')
             call inf_error_with_trace(coder,cnode_arg(args,nret-mode),&
               'Cannot pass a shared value to a standard procedure')
             mode=sym_private
          endif
       endif
       
       ! Strip argument modes before passing
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
          if(pm_tv_kind(t)==pm_type_is_tuple.and.iand(pm_tv_flags(t),pm_type_is_list)==0) then
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
    recursive function simple_proc_call(sig,procs,err,sig_start) result(ressig)
      integer,intent(in):: sig
      type(pm_ptr),intent(in):: procs
      logical,intent(out),optional:: err
      integer,intent(in),optional:: sig_start
      integer:: ressig
      
      integer:: h,i,j,m,start,slot,pcheck,nkey_sig,jpass,nconsidered
      integer:: vbase,wbase
      type(pm_ptr):: tv,v,proc,match_proc,rtvect,new_env
      integer:: rt,rt2,pars,mpars,apars,new_apars,tno,match_pars,pflags
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
                 'cflags',iand(cnode_get_num(proc,pr_flags),proccall_is_comm+proccall_is_method+proccall_is_general),&
                 iand(flags,proccall_is_comm+proccall_is_method+proccall_is_general)
!!$            call pm_dump_tree(coder%context,6,pm_name_val(coder%context,cnode_get_num(proc,pr_amps)),2)
!!$            call pm_dump_tree(coder%context,6,pm_name_val(coder%context,amps),2)
            
            if(cnode_get_num(proc,pr_nret)/=nret) cycle
            if(cnode_get_num(proc,pr_amps)/=amps) cycle
            pflags=cnode_get_num(proc,pr_flags)
            if(iand(pflags,proccall_is_comm+proccall_is_method+proccall_is_general)/=&
                 iand(flags,proccall_is_comm+proccall_is_method+proccall_is_general)) cycle
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
                                int(pm_fast_esize(cnode_get(proc,pr_keys))+1)/2,when_no_match,.true.,new_apars,new_env)
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
                  rt=inf_builtin(coder,proc,callnode,apars,pars,new_apars)
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
                       int(pm_fast_esize(cnode_get(proc,pr_keys))+1)/2,when_no_match,.false.,new_apars,new_env)
                  coder%trace_depth=coder%trace_depth-1
                  if(when_no_match) then
                     cycle
                  endif
                  if(cnode_get_name(callnode,cnode_modl_name)/=sym_pm_system) then
                     coder%supress_errors=.false.
                  endif

                  if(nret>0.and.rt<0) then
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
                  
                  ! Be careful not to add GC code between inf_proc and here (new_env not protected)
                  if(.not.pm_fast_isnull(new_env)) then
                     call combine_nhd_call_envelope(coder,new_env)
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

               ! Types of & args may have changed
               if(new_apars>0.and.amps/=0.and..not.bad_amp.and..not.ignore_rules) then
                  if(debug_inference) then
                     write(*,*) 'Changing & to',trim(pm_type_as_string(coder%context,new_apars))
                  endif
                  amplocs=pm_name_val(coder%context,amps)
                  rtvect=pm_type_vect(coder%context,new_apars)
                  if(pm_tv_kind(rtvect)==pm_type_is_tuple.and.&
                       iand(pm_tv_flags(rtvect),pm_type_is_list)==0) then
                     do j=0,pm_fast_esize(amplocs)
                        m=amplocs%data%i(amplocs%offset+j)
                        tno=pm_tv_arg(rtvect,j+1)
                        if(is_invar.and..not.is_comm) tno=pm_type_add_mode(coder%context,tno,sym_invar)
                        call combine_types(cnode_arg(args,nret+m),tno)
                     enddo
                  else
                     m=amplocs%data%i(amplocs%offset)
                     tno=new_apars
                     if(is_invar.and..not.is_comm) tno=pm_type_add_mode(coder%context,tno,sym_invar)
                     call combine_types(cnode_arg(args,nret+m),tno)
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
                     if(iand(cnode_get_num(proc,pr_flags),proccall_is_comm+proccall_is_method+proccall_is_general)/=&
                          iand(flags,proccall_is_comm+proccall_is_method+proccall_is_general)) cycle
                  endif
                  pars=cnode_get_num(proc,pr_ptype)
                  call print_proc_details(coder,proc)
                  if(m>pm_opts%proc_list.and..not.pm_opts%show_all_procs) then
                     call more_error(coder%context,&
                          '... (to see all procedures use -fshow-all-procs)')
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
    recursive function var_call(callnode) result(ressig)
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
      !coder%wstack(coder%wtop-nargs)=proctyp

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
      if(iand(flags,proccall_is_method)/=0) then
         kind=sym_dot
      elseif(iand(flags,proccall_is_general)/=0) then
         kind=sym_yield
      elseif(iand(flags,proccall_is_comm)/=0) then
         kind=sym_pct
      else
         kind=sym_proc
      endif
      
      if(pm_tv_name(tv)/=kind) then
         call inf_error(coder,callnode,&
              'Call does not match procedure type ("'//&
              trim(pm_name_as_string(coder%context,pm_tv_name(tv)))//'" vs "'//&
              trim(pm_name_as_string(coder%context,kind))//'"): '//&
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
      call print_call_details(coder,callnode,keybase,nargs)
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
!!$       write(*,*) 'ARGTYP=',argtyp,trim(pm_type_as_string(coder%context,argtyp)),coder%wstack(wbase+1:&
!!$            wbase+nargs+2)

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
    logical:: converted_to_poly
    at=old_at
    nomatch=.false.
    error=.false.
    flags=cnode_get_num(callnode,call_flags)
    
    if(iand(flags,call_is_fixed)==0) then
       at2=pm_type_convert(coder%context,pt,at,iand(flags,call_keep_literals)==0,ipass>=2,.false.)
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
       at2=pm_type_convert(coder%context,pt,at,&
            iand(flags,call_keep_literals+call_is_fixed)==0,ipass>=2,ipass>=3,converted_to_poly)
       if(at2>0) then
          if(pm_type_includes(coder%context,pt,at2,pm_type_incl_val)) then
             if(debug_inference) then
                write(*,*) 'Converted',trim(pm_type_as_string(coder%context,pt)),'<>',&
                     trim(pm_type_as_string(coder%context,at2))
             endif
             if(converted_to_poly) then
                base=coder%wtop
                call push_word(coder,ielem)
                call push_word(coder,at2)
                call code_int_vec(coder,coder%wstack,base+1,coder%wtop)
                ! Correct parameter type to post-conversion value
                coder%wtop=base
             endif
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


  !===============================================================
  ! Create but do not intialise current stack frame
  !===============================================================
  subroutine new_stack_frame(coder,max_index)
    type(code_state),intent(inout):: coder
    integer,intent(in):: max_index
    coder%stack(coder%top+1)=coder%base
    coder%base=coder%top+1
    coder%top=coder%base+max_index
    if(coder%top>max_code_stack) &
         call pm_panic('Program too complex (nested calls)')
  end subroutine new_stack_frame

  !===============================================================
  ! Create and initialise a stack frame
  !===============================================================
  subroutine create_stack_frame(coder,max_index) 
    type(code_state),intent(inout):: coder
    integer,intent(in):: max_index
    call new_stack_frame(coder,max_index)
    call init_stack_frame(coder,coder%base,1,coder%top)
  end subroutine create_stack_frame

  !===============================================================
  ! (Re)initialise current stack frame
  ! Only slots first..last are initialised (as are control slots)
  !===============================================================
  subroutine init_stack_frame(coder,base,first,last) 
    type(code_state),intent(inout):: coder
    integer,intent(in):: base,first,last
    integer:: i
    do i=base+first,last
       coder%stack(i)=undefined
    enddo
  end subroutine init_stack_frame

  !===============================================================
  ! Pop off current stack frame
  !===============================================================
  subroutine pop_stack_frame(coder)
    type(code_state),intent(inout):: coder
    coder%top=coder%base-1
    coder%base=coder%stack(coder%base)
    if(coder%base==0) call pm_panic('xxx')
  end subroutine pop_stack_frame


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
          if(iand(init,call_converts_uninit)/=0) then
             tno=pm_type_arg(coder%context,tno,1)
             return
          elseif(iand(init,call_takes_uninit+call_needs_uninit)/=0) then
             return
          endif
       endif
       call cnode_error(coder,callnode,&
            'Attempt to use variable or constant before it is initialised: ',&
            cnode_var_name(var))
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
    logical:: ok,added
    typ0=get_var_type(coder,cnode,var,init=call_takes_uninit)
    typ2=typ0
!!$    write(*,*) 'Combining...',trim(pm_type_as_string(coder%context,typ0)),'<>',&
!!$         trim(pm_type_as_string(coder%context,typ))
    if(typ/=typ0) then
       if(typ0<=0) then
          typ2=typ
       elseif(typ>0) then
          if(pm_type_kind(coder%context,typ0)==pm_type_is_uninitialised.and..not.present(no_init)) then
             typ2=typ
          else
             typ2=pm_type_combine(coder%context,typ0,typ,ok,added)
             if(.not.ok) then
                call cnode_error(coder,var,'Value does not have consistent type:',&
                     cnode_var_name(var))
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
                typ2=error_type
                call inf_trace(coder)
             elseif(added) then
                coder%types_changed=.true.
             endif
          endif
       endif
    endif
!!$    write(*,*) '....to',trim(pm_type_as_string(coder%context,typ2))
    call set_var_type(coder,var,typ2)
    if(cnode_flags_set(var,var_flags,var_is_reference)) then
       call combine_subvar_type(coder,cnode_get(var,var_extra_info),typ0,typ2)
    endif
  end subroutine combine_var_type

  !===========================================================
  ! For a given variable, change any subelement of oldtype
  ! to newtype
  !===========================================================
  subroutine combine_subvar_type(coder,var,oldtype,newtype)
    type(code_state):: coder
    type(pm_ptr),intent(in):: var
    integer,intent(in):: oldtype,newtype
    integer:: vartype
    if(oldtype==newtype) return
    vartype=get_var_type(coder,var,var,init=call_takes_uninit)
    if(vartype<=0.or.oldtype<=0.or.newtype<=0) return
    vartype=pm_type_replace(coder%context,vartype,oldtype,newtype)
    call set_var_type(coder,var,vartype)
  end subroutine combine_subvar_type
  
  !===========================================================
  ! Type constraint / Cast
  ! Convert type tno2 to tno1
  ! Returns 0, type (for poly conversion) or -1 for error
  !===========================================================
  function inf_cast(coder,node,tno1,tno2) result(k)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: tno1
    integer,intent(inout):: tno2
    integer:: k
    logical:: ok,converted_to_poly
    integer:: tno3,base,key(1)
    k=0
    if(tno1<0.or.tno2<=0) then
       return
    endif
    if(debug_inference) then
       write(*,*) 'Cast:',trim(pm_type_as_string(coder%context,tno2)),&
            ' to: ',trim(pm_type_as_string(coder%context,tno1))
    endif
    tno3=pm_type_convert(coder%context,tno1,tno2,.true.,.false.,.false.)
    if(tno3>=0) tno2=tno3
    ok=pm_type_includes(coder%context,tno1,tno2,pm_type_incl_val)
    if(.not.ok) then
       tno3=pm_type_convert(coder%context,tno1,tno2,.true.,.true.,.true.,converted_to_poly)
       if(converted_to_poly) k=tno3
       if(tno3>=0) then
          tno2=tno3
          ok=.true.
       endif
    endif
    if(.not.ok) then
       call inf_error(coder,node,&
            'Value of type "'//trim(pm_type_as_string(coder%context,tno2))//&
            '" cannot be converted to type "'//trim(pm_type_as_string(coder%context,tno1))//'"')
       call inf_trace(coder)
       k=-1
    endif
    if(debug_inference) write(*,*) 'Cast Converts to:',trim(pm_type_as_string(coder%context,tno2))
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
    elseif(opcode==op_same_type_fold) then
       ok=pm_type_equal(coder%context,pm_type_arg(coder%context,atype,2),pm_type_arg(coder%context,atype,3))
       if(ok) then
          rtype=coder%true_literal
       else
          rtype=coder%false_literal
       endif
       return
    elseif(opcode==op_same_rec_fold) then
       ok=pm_type_same_rec(coder%context,pm_type_arg(coder%context,atype,2),pm_type_arg(coder%context,atype,3))
       if(ok) then
          rtype=coder%true_literal
       else
          rtype=coder%false_literal
       endif
       return
    elseif(opcode==op_directly_assignable_fold) then
       ok=pm_type_equal(coder%context,pm_type_arg(coder%context,atype,2),pm_type_arg(coder%context,atype,3)).and.&
            iand(pm_type_flags(coder%context,pm_type_arg(coder%context,atype,2)),pm_type_has_array+pm_type_has_poly)==0
       if(ok) then
          rtype=coder%true_literal
       else
          rtype=coder%false_literal
       endif
       return
    endif
    arg1=pm_type_val(coder%context,pm_tv_arg(tv,2))
    if(n>1) then
       arg2=pm_type_val(coder%context,pm_tv_arg(tv,3))
    endif
    rtyp=pm_type_strip_to_basic(coder%context,pm_type_arg(coder%context,rstype,1))
    
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
  contains
    include 'fname.inc'
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
    case(op_not_fold)
       ok=.not.a%data%l(a%offset)
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

  !================================================================
  ! Set the n-th flag bit associated with a call
  !================================================================
  subroutine set_call_bit(coder,callnode,n)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: callnode
    integer:: n
    integer:: m
    m=cnode_get_num(callnode,call_index)+(n+bit_size(1)-2)/bit_size(1)
    coder%stack(coder%base+m)=ibset(max(0,coder%stack(coder%base+m)),iand(n-1,bit_size(1)-1))
  end subroutine set_call_bit


  !================================================================
  ! Update the current envelope for @[] accesses to nhd vars
  !================================================================
  subroutine combine_nhd_envelope(coder,callnode,args,n)
    type(code_state):: coder
    type(pm_ptr):: callnode
    integer,dimension(:),intent(in):: args
    integer,intent(in):: n
    integer(pm_ln):: env(n*2)
    integer:: i,s,nz
    type(pm_ptr):: p
    logical:: ok

    ! Create envelope record if there is none
    ! Record goes: non_literal_flag min max ... (for ortho) min max ... (for non-ortho)
    if(pm_fast_isnull(coder%envelope)) then
       coder%envelope=pm_new(coder%context,pm_long,int(n*4,pm_ln))
       p=coder%envelope
       p%data%ln(p%offset)=0
       do i=0,n*4-2,2
          p%data%ln(p%offset+i)=huge(1_pm_ln)
          p%data%ln(p%offset+i+1)=-huge(1_pm_ln)
       enddo
    else
       p=coder%envelope
    endif

    ! This should only happen if there are errors elsewhere
    if(n*4/=pm_fast_esize(p)+1) then
       return
    endif

    ! Get envelope values from argument types (will set ok=false if non-literal type)
    ok=.true.
    do i=1,n*2-1,2
       env(i)=val_for(i)
       env(i+1)=val_for(i+1,env(i))
    enddo
    if(.not.ok) then
       call inf_error(coder,callnode,'Arguments to "@[]" must be "literal" or "fix" integers')
       return
    endif

    ! How many dimensions have non-zero indices?
    nz=0
    do i=1,n*2-1,2
       if(env(i)==0.and.env(i+1)==0) nz=nz+1
    enddo

    ! Update the otho envelope (which must also include non-otho so always update)
    do i=1,n*2-1,2
       p%data%ln(p%offset+i-1)=min(p%data%ln(p%offset+i-1),env(i))
       p%data%ln(p%offset+i)=max(p%data%ln(p%offset+i),env(i+1))
    enddo

    ! If this is a non-ortho access, then also update non-ortho envelope
    if(nz/=n-1) then
       s=n*2
       do i=1,n*2-1,2
          p%data%ln(p%offset+i+s-1)=min(p%data%ln(p%offset+i+s-1),env(i))
          p%data%ln(p%offset+i+s)=max(p%data%ln(p%offset+i+s),env(i+1))
       enddo
    endif

  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    
    function val_for(i,def) result(n)
      integer,intent(in):: i
      integer(pm_ln),intent(in),optional:: def
      integer(pm_ln):: n
      integer:: tno,tk
      type(pm_ptr):: p
      tno=args(i)
      tk=pm_type_kind(coder%context,tno)
      if(tk==pm_type_is_literal_value.or.tk==pm_type_is_fix_value) then
         p=pm_type_val(coder%context,tno)
         n=p%data%ln(p%offset)
      elseif(tno==pm_null) then
         if(present(def)) then
            n=def
         else
            ok=.false.
         endif
      else
         ok=.false.
      endif
    end function val_for
    
  end subroutine combine_nhd_envelope


  !==================================================
  ! Combine envelope returned by a % proc into
  ! the current nhd envelope
  !==================================================
  subroutine combine_nhd_call_envelope(coder,envelope)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: envelope
    integer:: i,esize
    if(pm_fast_isnull(envelope)) return
    if(pm_fast_isnull(coder%envelope)) then
       coder%envelope=envelope
       return
    endif
!!$    call pm_dump_tree(coder%context,6,coder%envelope,2)
!!$    write(*,*) '[--]'
!!$    call pm_dump_tree(coder%context,6,envelope,2)
    esize=pm_fast_esize(coder%envelope)
    if(esize/=pm_fast_esize(envelope)) return
    do i=0,pm_fast_esize(coder%envelope),2
       coder%envelope%data%ln(coder%envelope%offset+i)=min(&
            coder%envelope%data%ln(coder%envelope%offset+i),&
            envelope%data%ln(envelope%offset+i))
       coder%envelope%data%ln(coder%envelope%offset+i+1)=max(&
            coder%envelope%data%ln(coder%envelope%offset+i+1),&
            envelope%data%ln(envelope%offset+i+1))
    enddo
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine combine_nhd_call_envelope

  
  
  !================================================================
  ! Run backpropogation analysis on cblock in function
  ! that has been type-resolved yielding resolution vector rvec
  ! Update rvec if update is true
  !================================================================
  recursive subroutine bprop(coder,cblock,rvec,update)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    integer,dimension(:),intent(inout):: rvec
    logical,intent(in):: update
    integer(access_kind),allocatable,dimension(:):: access_info
    integer:: save_loop_depth,i
    if(debug_bprop) then
       write(*,*) 'BP', size(rvec)
    endif
    allocate(access_info(size(rvec)+pm_max_args+1))
    access_info(1:size(rvec))=0
    access_info(size(rvec)+1:)=access_is_var
    access_info(size(rvec)+pm_max_args+1)=-1
    save_loop_depth=coder%loop_depth
    call bprop_cblock(coder,cblock,access_info,rvec,update.and.pm_opts%run_bprop)
    coder%loop_depth=save_loop_depth
    if(update.and.pm_opts%run_bprop) then
       do i=1,size(rvec)
          if(access_info(i)==access_deactivated_call.or.&
               access_info(i)==access_is_var) then
             if(debug_bprop) then
                write(*,*) 'BP Deactivate',i
             endif
             rvec(i)=sp_sig_deactivated
          endif
       end do
       deallocate(access_info)
    endif
  end subroutine bprop

  
  !==========================================
  ! Back propogate information for code block
  !==========================================
  recursive subroutine bprop_cblock(coder,cblock,access_info,rvec,update)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    integer,dimension(:),intent(inout)::rvec
    integer(access_kind),dimension(:),allocatable,intent(inout):: access_info
    logical,intent(in):: update
    integer:: nvars,idx,newbase
    type(pm_ptr):: p
    if(pm_fast_isnull(cblock)) return
    p=cnode_get(cblock,cblock_first_var)
    do while(.not.pm_fast_isnull(p))
       idx=cnode_get_num(p,var_index)
       access_info(idx)=ior(access_info(idx),access_is_var)
       p=cnode_get(p,var_link)
    enddo
    p=cnode_get(cblock,cblock_last_call)
    do while(.not.pm_fast_isnull(p))
       call bprop_call(coder,cblock,p,access_info,rvec,update)
       p=cnode_get(p,call_back_link)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine bprop_cblock


  !==========================================
  ! Back propogate information for call cnode
  !==========================================
  recursive subroutine bprop_call(coder,cblock,callnode,access_info,rvec,update)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,cblock
    integer,dimension(:),intent(inout):: rvec
    integer(access_kind),dimension(:),allocatable,intent(inout):: access_info
    logical,intent(in):: update
    type(pm_ptr):: args,arg,tv
    integer:: nret,nargs,nvargs,opcode,i,j,n,sig,tno,slot,mode
    integer(access_kind):: acc
    logical:: any_accessed
    nret=cnode_get_num(callnode,call_nret)
    sig=-cnode_get_num(callnode,call_sig)
    args=cnode_get(callnode,call_args)
    nargs=cnode_numargs(args)-nret
    if(debug_bprop_simple) then
       if(sig>0) then
          write(*,*) 'BPCALL',sym_names(sig)
       else
          write(*,*) 'PROCCALL',sig,trim(sig_name_str(coder,-sig))
       endif
       do i=1,nret
          write(*,'(A,I4,X)',advance='NO') 'ARG=',i
          if(pm_fast_vkind(cnode_arg(args,i))==pm_pointer) then
             if(cnode_get_kind(cnode_arg(args,i))==cnode_is_var) then
                acc=access_info(cnode_get_num(cnode_arg(args,i),var_index))
                call print_bprop_item(6,acc)
                if(iand(acc,access_is_list)/=0) then
                   n=find_list_info(list_info_start(),access_info,&
                        cnode_get_num(cnode_arg(args,i),var_index))
                   if(n>0) then
                      write(*,*) '{'
                      do j=1,access_info(n+1)
                         call print_bprop_item(6,access_info(n+1+j))
                      enddo
                      write(*,*) '}'
                   endif
                endif
             else
                write(*,*) 'NONVAR'
             endif
          endif
       enddo
    endif
    call enable
    if(sig>0) then
       if(debug_bprop) then
          write(*,*) 'BPcall ',sym_names(sig)
       endif
       select case(sig)
       case(sym_while,sym_while_invar)
          if(cblock_must_run(cnode_arg(args,1)).or.cblock_must_run(cnode_arg(args,3)) &
               .or.block_writes_accessed(4)) then
             coder%loop_depth=coder%loop_depth+1
             call access(cnode_arg(args,2))
             call bprop_cblock(coder,cnode_arg(args,1),access_info,rvec,update)
             call bprop_cblock(coder,cnode_arg(args,3),access_info,rvec,update)
             call access(cnode_arg(args,2))
             call bprop_cblock(coder,cnode_arg(args,1),access_info,rvec,update)
             if(coder%loop_depth==1) then
                call access(cnode_arg(args,2))
                call bprop_cblock(coder,cnode_arg(args,3),access_info,rvec,update)
                call access(cnode_arg(args,2))
                call bprop_cblock(coder,cnode_arg(args,1),access_info,rvec,update)
             endif
             coder%loop_depth=coder%loop_depth-1
          else
             if(debug_bprop) then
                write(*,*) 'DISABLE while'
             endif
             call disable
          endif
       case(sym_until,sym_until_invar)
          if(cblock_must_run(cnode_arg(args,1)).or.&
               block_writes_accessed(3)) then
             call access(cnode_arg(args,2))
             coder%loop_depth=coder%loop_depth+1
             call access(cnode_arg(args,2))
             call bprop_cblock(coder,cnode_arg(args,1),access_info,rvec,update)
             call access(cnode_arg(args,2))
             if(coder%loop_depth==1) then
                call bprop_cblock(coder,cnode_arg(args,1),access_info,rvec,update)
                call access(cnode_arg(args,2))
             endif
             coder%loop_depth=coder%loop_depth-1
          else
             if(debug_bprop) then
                write(*,*) 'DISABLE until'
             endif
             call disable
          endif
       case(sym_if,sym_if_invar)
          call bprop_if(count_updates(cnode_arg(args,4),1))
       case(sym_do)
          call bprop_cblock(coder,cnode_arg(args,1),access_info,rvec,update)
       case(sym_pct)
          call bprop_cblock(coder,cnode_arg(args,nargs),access_info,rvec,update)
          if(nargs>1) call access(cnode_arg(args,1))
       case(sym_pm_for)
          i=cnode_get_num(callnode,call_index)
          call bprop_cblock(coder,cnode_arg(args,nret+nargs),access_info,rvec,update)
          if(.not.accessed(cnode_arg(args,1))) then
             rvec(i)=0
          elseif(rvec(i)==0) then
             rvec(i)=-2
          endif
          do i=1,nargs-1
             call access(cnode_arg(args,nret+i))
          enddo
          if(debug_bprop) then
             write(*,*) 'FOR>',accessed(cnode_arg(args,1)),rvec(i)
          endif
       case(sym_any,sym_any_invar)
          call bprop_multi_versions(cnode_arg(args,4),cnode_arg(args,2),cnode_arg(args,1))
          call access(cnode_arg(args,3))
       case(sym_pm_invar)
          if(nret==1) then
             if(.not.accessed(cnode_arg(args,1))) then
                call disable
                return
             endif
          endif
          call bprop_cblock(coder,cnode_arg(args,nret+1),access_info,rvec,update)
       case(sym_pm_set_dotdotdot)
          call access(cnode_arg(args,2))
       case(sym_pm_each_index)
          if(nret==1.or.accessed(cnode_arg(args,1))) then
             if(nret>1) then
                call bprop_multi_versions(cnode_arg(args,nret+3),cnode_arg(args,nret+2),&
                     cnode_arg(args,nret),cnode_arg(args,nret+4),cnode_arg(args,1))
             else
                call bprop_multi_versions(cnode_arg(args,nret+3),cnode_arg(args,nret+2),&
                     cnode_arg(args,nret))
             endif
             call access(cnode_arg(args,nret+1))
          else
             call disable
          endif
       case(sym_pm_envelope)
          call combine_access_info(cnode_arg(args,1),access_used_by_at,.false.)
       case(sym_check)
          call std_access(.true.,1)
       case(sym_test)
          if(cnode_num_arg(args,2)==sym_true.and.pm_opts%check_stmts) then
             call bprop_cblock(coder,cnode_arg(args,1),access_info,rvec,update)
          else
             call disable
          endif
       case(sym_dot,sym_dot_ref,sym_get_dot,sym_get_dot_ref)
          if(accessed(cnode_arg(args,1))) then
             arg=cnode_arg(args,2)
             call access(arg)
             call access(cnode_arg(args,3))
          else
             call disable
          endif
       case(sym_pm_list)
          if(cnode_flags_set(cnode_arg(args,1),var_flags,var_is_list)) then
             n=find_list_info(list_info_start(),access_info,cnode_get_num(cnode_arg(args,1),var_index))
             if(debug_bprop) then
                write(*,*)'LIST>',trim(pm_name_as_string(coder%context,cnode_get_num(cnode_arg(args,1),var_name))),'@',n
             endif
             if(n<0) then
                acc=get_access_info(cnode_arg(args,1))
                do i=2,nargs+nret
                   call combine_access_info(cnode_arg(args,i),acc,.false.)
                enddo
             else
                do i=2,nargs+nret
                   call combine_access_info(cnode_arg(args,i),access_info(n+i),.false.)
                enddo
             endif
             call set_final_tags_for_call(args,nargs,nargs,pm_null_obj,0)
          else
             if(accessed(cnode_arg(args,1))) then
                do i=2,nargs+nret
                   call access(cnode_arg(args,i))
                enddo
             else
                call disable
             endif
          endif
       case(sym_get_list_elem,sym_simple_list_elem)
          if(ever_accessed(cnode_arg(args,1))) then
             arg=cnode_arg(args,2)
             call access(arg)
             if(sig/=sym_simple_list_elem) call access(cnode_arg(args,3))
             if(.not.cnode_flags_clear(arg,var_flags,var_is_list+var_is_list_param)) then
                slot=cnode_get_num(arg,var_index)
                tno=rvec(slot)
                if(tno>0) then
                   tv=pm_type_vect(coder%context,pm_type_strip_mode(coder%context,tno,mode))
                   n=alloc_list_info(list_info_start(),pm_tv_numargs(tv),slot,access_is_var)
                   access_info(n+1+rvec(cnode_get_num(callnode,call_index)))=&
                        access_info(cnode_get_num(cnode_arg(args,1),var_index))
                   access_info(slot)=ior(access_info(slot),access_is_list)
                endif
             endif
          else
             call disable
          endif
       case(sym_update_from_list)
          any_accessed=.false.
          do i=1,nret
             arg=cnode_arg(args,i)
             any_accessed=any_accessed.or.accessed_after_modify(arg)
          enddo
          if(any_accessed) then
             call access(cnode_arg(args,nargs+nret))
          endif
       case(sym_pm_import_list)
          if(accessed(cnode_arg(args,1))) then
             arg=cnode_arg(args,2)
             call copy_list_info(cnode_arg(args,1),arg)
             call combine_access_info(arg,access_is_list,.false.)
             call access(arg)
          else
             call disable
          endif
       case(sym_pm_ref)
          call std_access(.false.,3)
       case(sym_open)
          nvargs=0
          n=0
          do i=1,nargs
             arg=cnode_arg(args,i)
             if(cnode_get_kind(arg)==cnode_is_var) then
                tno=rvec(cnode_get_num(arg,var_index))
                if(tno>0) then
                   tv=pm_type_vect(coder%context,tno)
                   if(pm_tv_kind(tv)==pm_type_is_tuple) then
                      if(iand(pm_tv_flags(tv),pm_type_is_list)==0) then
                         nvargs=pm_tv_numargs(tv)-1
                      else
                         n=n+pm_tv_numargs(tv)+2
                      endif
                      call combine_access_info(arg,access_holds_result,.false.)
                   endif
                endif
             endif
          enddo
          call code_val(coder,pm_new(coder%context,&
               access_pm_type,int(nargs+nvargs+n+2,pm_ln)))
          arg=top_code(coder)
          j=nargs+nvargs+1
          arg%data%i16(arg%offset)=j-1
          do i=1,nargs
             acc=get_arg_access_info(cnode_arg(args,i))
             arg%data%i16(arg%offset+i)=acc
             if(debug_bprop_tagging) then
                write(*,'("ARG>",i4,i4)',advance='NO') i,acc
                call print_bprop_item(6,acc)
             endif
             call copy_list_info_for_param(i,acc,arg%data%i16(arg%offset+1:),j,cnode_arg(args,i))
             if(debug_bprop) then
                write(*,*) 'BP store access',arg%data%i16(arg%offset+i),&
                     cnode_get_num(cnode_arg(args,i),var_index)
             endif
          enddo
          do i=1,nvargs
             acc=access_info(size(rvec)+i)
             if(acc==access_is_var) then
                acc=ior(acc,access_not_passed)
!!$             elseif(iand(pm_type_flags(coder%context,&
!!$                  pm_tv_arg(tv,i)),pm_type_has_storage)==0) then
!!$                acc=ior(acc,access_not_passed)
             endif
             arg%data%i16(arg%offset+i+nargs)=acc
             call copy_list_info_for_param_idx(i+nargs-1,acc,arg%data%i16(arg%offset+1:),j,size(rvec)+i)
          enddo
          arg%data%i16(arg%offset+j)=-1
          !write(*,*) '>>',arg%data%i16(arg%offset:arg%offset+pm_fast_esize(arg))
       case(sym_key)
          do i=1,nret/2
             call access(cnode_arg(args,i))
          enddo
          do i=2,nargs,2
             if(accessed(cnode_arg(args,i/2+nret/2))) then
                call access(cnode_arg(args,nret+i))
             endif
             call bprop_cblock(coder,cnode_arg(args,nret+i-1),access_info,rvec,update)
          enddo
          call code_val(coder,pm_new(coder%context,access_pm_type,int(max(nret/2,1)+1,pm_ln)))
          arg=top_code(coder)
          arg%data%i16(arg%offset)=nret/2
          do i=nret/2+1,nret
             arg%data%i16(arg%offset+i-nret/2)=get_arg_access_info(cnode_arg(args,i))
          enddo
       case(sym_amp)
          do i=nret+1,nargs+nret
             call set_access_info(cnode_arg(args,i),access_everything+access_used_by_at)
          enddo
       case(sym_result,sym_update_list)
          do i=nret+1,nargs+nret
             call set_access_info(cnode_arg(args,i),access_everything)
          enddo
 
       case(sym_rec)
          call std_access(.false.,3)
       case(sym_type_val)
          if(.not.accessed(cnode_arg(args,1))) call disable
       case(sym_move)
          n=nargs/2
          do i=1,n
             if(accessed(cnode_arg(args,i))) then
                call combine_access_info(cnode_arg(args,i+n),&
                     access_used_ever+access_used_now+access_is_var,.false.)
             endif
          enddo
       case(sym_var,sym_var_set_mode,sym_set_mode,sym_change_mode,sym_underscore,&
            sym_colon,sym_typeof,sym_pm_uninit)
          continue
       case default
          call std_access(.false.,1)
       end select
    else
       call bprop_proc_call
    endif
  contains

    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'

    recursive subroutine bprop_if(nupdates)
      integer:: nupdates
      type(pm_ptr):: readlist,p,var
      integer(access_kind),dimension(nupdates):: save_access
      integer:: i
      integer(access_kind):: acc

      if(.not.(block_writes_accessed(4).or.cblock_must_run(cnode_arg(args,2)).or.&
           cblock_must_run(cnode_arg(args,3)))) then
         call disable
         return
      endif

      readlist=cnode_arg(cnode_arg(args,4),1)
      i=1
      p=readlist
      do while(.not.pm_fast_isnull(p))
         var=p%data%ptr(p%offset)
         save_access(i)=get_access_info(var)
         p=p%data%ptr(p%offset+1)
         i=i+1
      enddo
      call bprop_cblock(coder,cnode_arg(args,2),access_info,rvec,update)
      i=1
      p=readlist
      do while(.not.pm_fast_isnull(p))
         var=p%data%ptr(p%offset)
         acc=access_info(cnode_get_num(var,var_index))
         call set_access_info(var,save_access(i))
         save_access(i)=acc
         p=p%data%ptr(p%offset+1)
         i=i+1
      enddo
      call bprop_cblock(coder,cnode_arg(args,3),access_info,rvec,update)
      i=1
      p=readlist
      do while(.not.pm_fast_isnull(p))
         var=p%data%ptr(p%offset)
         call combine_access_info(var,save_access(i),.false.)
         p=p%data%ptr(p%offset+1)
         i=i+1
      enddo
      call access(cnode_arg(args,1))
    end subroutine bprop_if

    recursive subroutine bprop_multi_versions(limitsc,cblock,rtn,res,fullres)
      type(pm_ptr),intent(in):: limitsc,cblock,rtn
      type(pm_ptr),intent(in),optional:: res,fullres
      integer:: i,j,slot1,slot2,sig,rtn_slot,rtn_access,list
      type(pm_ptr):: lrvecs,lrvec,limits
      if(present(fullres)) then
         list=find_list_info(list_info_start(),access_info,cnode_get_num(fullres,var_index))
      else
         list=-1
      endif
      sig=rvec(cnode_get_num(callnode,call_index))
      lrvecs=pm_dict_val(coder%context,coder%proc_cache,&
           int(sig,pm_ln))
      limits=cnode_arg(limitsc,1)
      slot1=limits%data%i(limits%offset)
      slot2=limits%data%i(limits%offset+1)
      rtn_slot=cnode_get_num(rtn,var_index)
      rtn_access=access_info(rtn_slot)
      if(debug_bprop) write(*,*) 'BPROP MULTI>',cnode_numargs(lrvecs)
      do j=cnode_numargs(lrvecs),1,-1
         lrvec=cnode_arg(lrvecs,j)
         rvec(slot1:slot2)=lrvec%data%i(lrvec%offset:lrvec%offset+slot2-slot1)
         access_info(slot1:slot2)=0
         access_info(rtn_slot)=rtn_access
         if(present(res)) then
            if(list>0) then
               access_info(cnode_get_num(res,var_index))=access_info(list+1+j)
            else
               call access(res)
            endif
         endif
         call bprop_cblock(coder,cblock,access_info,rvec,update)
         if(update) then
            do i=slot1,slot2
               if(access_info(i)==access_deactivated_call.or.&
                    access_info(i)==access_is_var) then
                  lrvec%data%i(lrvec%offset+i-slot1)=sp_sig_deactivated
                  if(debug_bprop) write(*,*) 'MU-DEACTIVATE>',i
               else
                  lrvec%data%i(lrvec%offset+i-slot1)=rvec(i)
               endif
            end do
         endif
      enddo
      if(debug_bprop) write(*,*) 'BPROP MULTI END>'
    end subroutine bprop_multi_versions

    recursive subroutine bprop_proc_call
      type(pm_ptr):: arg_access,key_access,key_names,proc_keys,arg,amps,key_args,procnode
      integer:: i,j,nkeys,nproc_keys,taints,sig,totargs,slot,bit,ifirst
      logical:: arg_accessed,is_accessed, all_accessed, needs_to_run, is_builtin
      integer(access_kind):: acc

      ifirst=merge(num_comm_args+1,2,cnode_flags_set(callnode,call_flags,proccall_is_comm))

      if(cnode_flags_set(callnode,call_flags,call_is_halo_exchange)) then
!!$         write(*,*) 'Test halo'
!!$         do i=1,nargs
!!$            call print_bprop_item(6,get_access_info(cnode_arg(args,i)))
!!$         enddo
!!$         write(*,*)'---'
         if(iand(get_access_info(cnode_arg(args,num_comm_args+1)),access_used_by_at)==0) then
            call disable
            return
         endif
      endif

      sig=rvec(cnode_get_num(callnode,call_index))
      if(sig<0) then
         select case(sig)
         case(sp_sig_init,sp_sig_assign)
            if(accessed_after_modify(cnode_arg(args,2))) then
               call access(cnode_arg(args,3))
               if(debug_bprop_tagging) then
                  call print_bprop_item(6,get_access_info(cnode_arg(args,2)))
               endif
               call modify(cnode_arg(args,2))
               call tag_associated_param(cnode_arg(args,3))
               call set_final_tags_for_call(args,nargs,3,pm_null_obj,0)
            else
               call disable
            endif
         case(sp_sig_dup)
            if(accessed(cnode_arg(args,1))) then
               call access(cnode_arg(args,3))
               call tag_associated_param(cnode_arg(args,3))
               call set_final_tags_for_call(args,nargs,2,pm_null_obj,0)
            else
               call disable
            endif
         case(sp_sig_link) 
            call combine_access_info(cnode_arg(args,3),&
                 get_access_info(cnode_arg(args,1)),.false.)
            call set_final_tags_for_call(args,nargs,2,pm_null_obj,0)
         case(sp_sig_noop)
            continue
         case default
            if(debug_bprop) write(*,*) 'BPspecial sig',sig
            call std_access(.false.,ifirst)
         end select
         return
      endif

      amps=cnode_get(callnode,call_amp)

      if(debug_bprop) then
         write(*,*) 'BPsig=',sig,trim(sig_name_str(coder,cnode_get_num(callnode,call_sig)))
      endif
      procnode=pm_dict_val(coder%context,coder%proc_cache,&
           int(sig,pm_ln))

      if(pm_fast_vkind(procnode)/=pm_pointer) then
         ! Recursive call
         call std_access(.false.,1)
         return
      endif

      if(cnode_get_kind(procnode)==cnode_is_autoconv_sig) then
         procnode=pm_dict_val(coder%context,coder%proc_cache,&
              int(cnode_num_arg(procnode,cnode_numargs(procnode)),pm_ln))
      endif

      is_builtin=cnode_get_kind(procnode)==cnode_is_builtin

      if(is_builtin) then
         taints=cnode_get_num(procnode,pr_flags)
      else
         taints=cnode_num_arg(procnode,3)
      endif

      if(debug_bprop) then
         write(*,*) 'BPcall  taints=',iand(taints,proc_must_run)
      endif

      is_accessed=.false.
      all_accessed=.true.
      do i=1,nret
         arg=cnode_arg(args,i)
         arg_accessed=accessed(arg)
         if(debug_bprop) then
            write(*,*) 'accessed #',i,cnode_get_num(arg,var_index),arg_accessed,&
                 cnode_flags_set(arg,var_flags,var_is_reference)
            call print_bprop_item(6,get_access_info(arg))
         endif
         is_accessed=is_accessed.or.arg_accessed
         all_accessed=all_accessed.and.arg_accessed
         !call modify(arg)
      enddo

      if(.not.pm_fast_isnull(amps)) then
         amps=pm_name_val(coder%context,int(amps%offset))
         do i=0,pm_fast_esize(amps)
            arg=cnode_arg(args,nret+amps%data%i(amps%offset+i))
!!$            if(cnode_flags_set(arg,var_flags,var_is_reference)) then
!!$               arg=cnode_get(arg,var_extra_info)
!!$            endif
            if(debug_bprop) then
               write(*,*) 'BPROP AMPS',i,nret+amps%data%i(amps%offset+i),accessed(arg)
               call print_bprop_item(6,get_access_info(arg))
            endif
            arg_accessed=accessed_after_modify(arg)
            is_accessed=is_accessed.or.arg_accessed
            if(debug_bprop) then
               write(*,*) 'AMP ACCESS',amps%data%i(amps%offset+i),is_accessed,all_accessed,&
                    trim(pm_name_as_string(coder%context,cnode_get_num(arg,var_name)))
            endif
            call modify(arg)
         enddo
      endif

      if(debug_bprop) then
         write(*,*) 'BPaccess',is_accessed,all_accessed
      endif

      if(.not.is_accessed.and.iand(taints,proc_must_run)==0) then
         if(debug_bprop) then
            write(*,*) 'BP disable',is_accessed,iand(taints,proc_must_run),&
                 trim(sig_name_str(coder,cnode_get_num(callnode,call_sig)))
         endif
         call disable
         return
      endif

      if(.not.all_accessed) then
         do i=1,nret
            call combine_access_info(cnode_arg(args,i),access_holds_result,.false.)
         enddo
         if(.not.pm_fast_isnull(amps)) then
            do i=0,pm_fast_esize(amps)
               arg=cnode_arg(args,nret+amps%data%i(amps%offset+i))
               call combine_access_info(arg,access_holds_result,.false.)
            enddo
         endif
      endif

      if(is_builtin) then
         totargs=cnode_get_num(procnode,pr_nargs)
         do i=ifirst,nargs-merge(1,0,totargs>nargs)
            !write(*,*) 'access',nret+i
            call access(cnode_arg(args,nret+i))
         enddo
         if(totargs>nargs) then
            do i=nargs,totargs
               j=size(rvec)+i-nargs+1
               call access_at_idx(j)
            enddo
         endif
      else  
         ! Main arguments
         arg_access=cnode_arg(procnode,6)
         totargs=arg_access%data%i16(arg_access%offset)
         do i=1,nargs
            acc=arg_access%data%i16(arg_access%offset+i)
            arg=cnode_arg(args,i+nret)
            call combine_arg_access_info(arg,acc)
            call copy_list_info_for_arg(acc,&
                 arg_access%data%i16(arg_access%offset+1:),totargs+1,i,arg)
         enddo

         if(totargs>nargs) then
            do i=nargs,totargs
               j=size(rvec)+i-nargs+1
               acc=arg_access%data%i16(arg_access%offset+i)
               call combine_arg_access_info_at_idx(j,acc)
               call copy_list_info_for_arg_idx(acc,&
                    arg_access%data%i16(arg_access%offset+1:),totargs+1,i,j)
            enddo
         endif

         ! Keyword arguments
         if(.not.pm_fast_isnull(cnode_get(callnode,call_keys))) then
            nkeys=cnode_numargs(cnode_get(callnode,call_keys))
            key_names=pm_name_val(coder%context,cnode_get_num(callnode,call_key_names))
            proc_keys=cnode_get(cnode_arg(procnode,1),pr_keys)
            nproc_keys=pm_fast_esize(proc_keys)/2
            key_args=cnode_get(callnode,call_keys)
            key_access=cnode_arg(procnode,7)
            outer: do j=1,nproc_keys
               do i=1,nkeys
                  if(proc_keys%data%i(proc_keys%offset+j-1)==&
                       key_names%data%i(key_names%offset+i-1)) then
                     call combine_arg_access_info(cnode_arg(key_args,i),&
                          key_access%data%i16(key_access%offset+j-1))
                     cycle outer
                  endif
               enddo
            enddo outer
         else
            nkeys=0
         endif

         call set_final_tags_for_call(args,nargs,totargs,key_args,nkeys)

      endif

    end subroutine bprop_proc_call

    subroutine set_final_tags_for_call(args,nargs,totargs,key_args,nkeys)
      type(pm_ptr),intent(in):: args,key_args
      integer,intent(in):: nargs,totargs,nkeys
      integer:: i,j,slot,bit
      slot=cnode_get_num(callnode,call_index)+1
      bit=0
      do i=1,nargs-merge(1,0,totargs>nargs)
         call set_final_flags(cnode_arg(args,i+nret),slot,bit)
      enddo
      if(totargs>nargs) then
         do i=nargs,totargs
            j=size(rvec)+i-nargs+1
            call set_final_flags_at_idx(j,slot,bit)
         enddo
      endif
      do i=1,nkeys
         call set_final_flags(cnode_arg(key_args,i),slot,bit)
      enddo
    end subroutine set_final_tags_for_call

    subroutine set_final_flags(arg,slot,bit)
      type(pm_ptr),intent(in):: arg
      integer,intent(inout):: slot,bit
      integer(access_kind):: acc
      type(pm_ptr):: var
      if(cnode_get_kind(arg)==cnode_is_var) then
         var=arg
         if(cnode_flags_set(var,var_flags,var_is_reference)) then
            var=cnode_get(var,var_extra_info)
         endif
         call set_final_flags_at_idx(cnode_get_num(var,var_index),slot,bit)
      endif
    end subroutine set_final_flags

    subroutine set_final_flags_at_idx(idx,slot,bit)
      integer,intent(in):: idx
      integer,intent(inout):: slot,bit
      integer(access_kind):: acc
      if(debug_bprop_tagging) then
         write(*,'(A,i4)',advance='NO') 'final',idx
         call print_bprop_item(6,access_info(idx))
      endif
      if(iand(access_info(idx),access_used_ever+access_not_last)==access_used_ever) then
         rvec(slot)=ibset(max(rvec(slot),0),bit)
      endif
      bit=bit+1
      if(bit>=bit_size(1)) then
         slot=slot+1
         bit=0
      endif
    end subroutine set_final_flags_at_idx

    recursive subroutine std_access(always,start)
      logical,intent(in):: always
      integer,intent(in):: start
      type(pm_ptr):: arg
      integer:: i
      logical:: arg_accessed,is_accessed,all_accessed,should_disable
      should_disable=.false.
      is_accessed=.false.
      all_accessed=.true.
      do i=1,nret
         arg=cnode_arg(args,i)
         arg_accessed=accessed(arg)
         is_accessed=is_accessed.or.arg_accessed
         all_accessed=all_accessed.and.arg_accessed
         !call modify(arg)
      enddo
      should_disable=.not.(is_accessed.or.always)
      if(should_disable) then
         do i=start,nargs
            arg=cnode_arg(args,i+nret)
            if(.not.pm_fast_isnull(arg)) then
               if(cnode_get_kind(arg)==cnode_is_cblock) then
                  should_disable=.false.
                  exit
               endif
            endif
         enddo
      endif
      if(.not.should_disable) then 
         do i=start,nargs
            call access(cnode_arg(args,i+nret))
         enddo
         if(nret>0.and..not.all_accessed) then
            do i=1,nret
               call combine_access_info(cnode_arg(args,i),access_holds_result,.false.)
            enddo
         endif
         do i=start,nargs
            arg=cnode_arg(args,i+nret)
            if(.not.pm_fast_isnull(arg)) then
               if(cnode_get_kind(arg)==cnode_is_cblock) then
                  call bprop_cblock(coder,arg,access_info,rvec,update)
               endif
            endif
         enddo
      else
         if(debug_bprop.and.sig>0) write(*,*) 'disable ',trim(sym_names(sig))
         call disable
      endif
    end subroutine std_access

    subroutine access(arg)
      type(pm_ptr):: arg
      integer:: idx
      type(pm_ptr):: var
      if(cnode_get_kind(arg)==cnode_is_var) then
         if(cnode_flags_set(arg,var_flags,var_is_reference)) then
            var=cnode_get(arg,var_extra_info)
            call access_at_idx(cnode_get_num(arg,var_index))
         endif
         call access_at_idx(cnode_get_num(arg,var_index))
      endif
    end subroutine access

    subroutine access_at_idx(idx)
      integer,intent(in):: idx
      call combine_access_info_at_idx(idx,access_used_ever+access_used_now+access_is_var,.true.)
    end subroutine access_at_idx

    function accessed(var) result(ok)
      type(pm_ptr),intent(in):: var
      logical:: ok
      type(pm_ptr):: rvar
      if(cnode_get_kind(var)==cnode_is_var) then
         ok=iand(access_info(cnode_get_num(var,var_index)),access_used_now)/=0
      else
         ok=.true.
      endif
    end function accessed

    function ever_accessed(var) result(ok)
      type(pm_ptr),intent(in):: var
      logical:: ok
      type(pm_ptr):: rvar
      if(cnode_get_kind(var)==cnode_is_var) then
         ok=iand(access_info(cnode_get_num(var,var_index)),access_used_ever)/=0
      else
         ok=.true.
      endif
    end function ever_accessed

    function accessed_after_modify(var) result(ok)
      type(pm_ptr),intent(in):: var
      logical:: ok
      type(pm_ptr):: rvar
      if(cnode_get_kind(var)==cnode_is_var) then
         ok=iand(access_info(cnode_get_num(var,var_index)),access_used_now)/=0
         if(.not.ok.and.cnode_flags_set(var,var_flags,var_is_reference)) then
            rvar=cnode_get(var,var_extra_info)
            ok=iand(access_info(cnode_get_num(rvar,var_index)),access_used_now)/=0
         endif
      else
         ok=.true.
      endif
    end function accessed_after_modify

    subroutine modify(var)
      type(pm_ptr),intent(in):: var
      integer:: idx
      ! Only a complete assignment resets _used_ flags
      if(cnode_flags_clear(var,var_flags,var_is_reference)) then
         idx=cnode_get_num(var,var_index)
         access_info(idx)=ior(access_is_var,&
              iand(not(access_used_now+access_used_by_at),access_info(idx)))
      endif
!!! modify any explicit list
    end subroutine modify

    subroutine enable
      access_info(cnode_get_num(callnode,call_index))=0
    end subroutine enable

    subroutine disable
      access_info(cnode_get_num(callnode,call_index))=access_deactivated_call
    end subroutine disable

    subroutine set_access_info(var,acc)
      type(pm_ptr),intent(in):: var
      integer(access_kind),intent(in):: acc
      if(cnode_get_kind(var)==cnode_is_var) then
         access_info(cnode_get_num(var,var_index))=acc
      endif
    end subroutine set_access_info

    subroutine combine_arg_access_info(arg,acc)
      type(pm_ptr),intent(in):: arg
      integer(access_kind),intent(in):: acc
      if(iand(acc,access_not_passed)==0) then
         call combine_access_info(arg,&
              iand(acc,not(access_not_passed+access_needs_movability)),&
              iand(acc,access_used_ever)/=0)
      endif
      if(iand(acc,access_needs_movability)/=0) call tag_associated_param(arg)
    end subroutine combine_arg_access_info

    subroutine combine_arg_access_info_at_idx(idx,acc)
      integer,intent(in):: idx
      integer(access_kind),intent(in):: acc
      if(iand(acc,access_not_passed)==0) then
         call combine_access_info_at_idx(idx,&
              iand(acc,not(access_not_passed)),&
              iand(acc,access_used_ever)/=0)
      endif
    end subroutine combine_arg_access_info_at_idx

    subroutine combine_access_info(arg,acc,count_as_access)
      type(pm_ptr),intent(in):: arg
      integer(access_kind),intent(in):: acc
      logical,intent(in):: count_as_access
      integer:: idx
      if(cnode_get_kind(arg)==cnode_is_var) then
         if(cnode_flags_set(arg,var_flags,var_is_reference)) then
            call combine_access_info_at_idx(&
                 cnode_get_num(cnode_get(arg,var_extra_info),var_index),&
                 acc,count_as_access)
         endif
         idx=cnode_get_num(arg,var_index)
         call combine_access_info_at_idx(idx,acc,count_as_access)
      endif
    end subroutine combine_access_info

    subroutine combine_access_info_at_idx(idx,acc,count_as_access)
      integer,intent(in):: idx
      integer(access_kind),intent(in):: acc
      logical,intent(in):: count_as_access
      ! if access is already used_now then set not_final
      if(count_as_access) then
         access_info(idx)=ior(access_info(idx),&
              ishft(iand(access_info(idx),access_used_ever),1))
      endif
      if(debug_bprop) then
         if(iand(acc,access_needs_movability)/=0) write(*,*) 'MOVING',idx
      endif
      access_info(idx)=ior(acc,access_info(idx))
    end subroutine combine_access_info_at_idx

    function get_access_info(var) result(acc)
      type(pm_ptr),intent(in):: var
      integer(access_kind):: acc
      if(cnode_get_kind(var)==cnode_is_var) then
         acc=access_info(cnode_get_num(var,var_index))
      else
         acc=0
      endif
    end function get_access_info

    function get_arg_access_info(var) result(acc)
      type(pm_ptr),intent(in):: var
      integer(access_kind):: acc
      if(cnode_get_kind(var)==cnode_is_var) then
         acc=access_info(cnode_get_num(var,var_index))
         if(acc==access_is_var) then
            acc=ior(acc,access_not_passed)
!!$         elseif(iand(pm_type_flags(coder%context,&
!!$              rvec(cnode_get_num(var,var_index))),pm_type_has_storage)==0) then
!!$            acc=access_not_passed
         endif
         if(debug_bprop) then
            write(*,*) 'GOT',cnode_get_num(var,var_index),acc
         endif
      else
         acc=0
      endif
    end function get_arg_access_info

    function cblock_must_run(cblock) result(ok)
      type(pm_ptr),intent(in):: cblock
      logical:: ok
      if(pm_fast_isnull(cblock)) then
         ok=.false.
      else
         ok=iand(cblock_taints(coder,cblock),proc_must_run)/=0
      endif
    end function cblock_must_run

    function block_writes_accessed(n) result(is_accessed)
      integer,intent(in):: n
      logical:: is_accessed
      type(pm_ptr):: writelist,p,var
      writelist=cnode_arg(cnode_arg(args,n),2)
      p=writelist
      is_accessed=.false.
      do while(.not.pm_fast_isnull(p))
         var=p%data%ptr(p%offset)
         is_accessed=is_accessed.or.accessed(var)
         p=p%data%ptr(p%offset+1)
      enddo
    end function block_writes_accessed

    function find_list_info(start,arr,var_index) result(pos)
      integer,intent(in):: start,var_index
      integer(access_kind),dimension(:),intent(in):: arr
      integer:: pos
      integer:: i
      !write(*,*) '@@',arr(start:min(size(arr),start+10))
      if(var_index>huge(arr(start))) call pm_panic('Program too large')
      i=start
      do while(arr(i)>=0)
         if(arr(i)==var_index) then
            pos=i
            return
         endif
         i=i+arr(i+1)+2
      enddo
      pos=-1
    end function find_list_info

    function alloc_list_info(start,n,var_index,acc) result(idx)
      integer,intent(in):: start,n,var_index
      integer(access_kind),intent(in):: acc
      integer:: idx
      integer:: i,old_size,new_size,iacc
      integer(access_kind),dimension(:),allocatable:: temp
      if(var_index>huge(acc)) call pm_panic('Program too large')
      iacc=acc
      i=start
      do while(access_info(i)>=0)
         if(access_info(i)==var_index) then
            idx=i
            return
         endif
         i=i+access_info(i+1)+2
      enddo
      old_size=size(access_info)
      if(i+n+2>old_size) then
         allocate(temp(old_size))
         temp=access_info
         deallocate(access_info)
         new_size=old_size+max(255,8*(i+n+2))
         allocate(access_info(new_size))
         access_info(1:old_size)=temp
      endif
      access_info(i)=var_index
      access_info(i+1)=n
      access_info(i+2:i+1+n)=iacc
      access_info(i+2+n)=-1
      idx=i
      if(debug_bprop) then
         write(*,*) 'Alloc list>',access_info(start:i+2+n)
      endif
    end function alloc_list_info

    subroutine copy_list_info_for_arg(acc,arr,start,arg_index,arg)
      integer(access_kind),intent(in):: acc
      integer(access_kind),dimension(:),intent(in)::arr
      integer,intent(in):: start,arg_index
      type(pm_ptr),intent(in):: arg
      integer:: idx
      if(cnode_get_kind(arg)==cnode_is_var) then
         idx=cnode_get_num(arg,var_index)
         call copy_list_info_for_arg_idx(acc,arr,start,arg_index,idx)
      endif
    end subroutine copy_list_info_for_arg

    subroutine copy_list_info_for_arg_idx(acc,arr,start,arg_index,var_index)
      integer(access_kind),intent(in):: acc
      integer(access_kind),dimension(:),intent(in)::arr
      integer,intent(in):: start,arg_index,var_index
      integer:: src,dst,i
      !write(*,*) '###',arr(1:start+5)
      if(iand(acc,access_is_list)/=0) then
         src=find_list_info(start,arr,arg_index)
         if(debug_bprop) then
            write(*,*) 'LIST ARG>',arg_index,var_index,src
         endif
         dst=find_list_info(list_info_start(),access_info,var_index)
         if(dst>0) then
            if(src>0) then
               do i=2,arr(dst+i)+1
                  access_info(dst+i)=ior(access_info(dst+i),arr(src+i))
               enddo
            else
               do i=2,arr(dst+i)+1
                  access_info(dst+i)=ior(access_info(dst+i),arr(iand(acc,not(access_is_list+access_not_passed))))
               enddo
            endif
            access_info(var_index)=ior(access_info(var_index),access_is_list)
            return
         endif
         if(src>0) then
            dst=alloc_list_info(list_info_start(),int(arr(src+1)),var_index,acc)
            do i=2,arr(src+1)+1
               access_info(dst+i)=arr(src+i)
            enddo
            access_info(var_index)=ior(access_info(var_index),access_is_list)
!!$         else
!!$            
!!$            do i=2,arr(src+1)+1
!!$               access_info(dst+i)=iand(acc,not(access_is_list+access_not_passed))
!!$            enddo
         endif
      endif
    end subroutine copy_list_info_for_arg_idx

    subroutine copy_list_info_for_param(par_index,acc,arr,start,arg)
      integer,intent(in):: par_index
      integer(access_kind),intent(in):: acc
      integer(access_kind),dimension(:),intent(inout)::arr
      type(pm_ptr),intent(in):: arg
      integer,intent(inout):: start
      integer:: idx
      if(cnode_get_kind(arg)==cnode_is_var) then
         idx=cnode_get_num(arg,var_index)
         call copy_list_info_for_param_idx(par_index,acc,arr,start,idx)
      endif
    end subroutine copy_list_info_for_param

    subroutine copy_list_info_for_param_idx(par_index,acc,arr,start,var_index)
      integer,intent(in):: par_index
      integer(access_kind),intent(in):: acc
      integer(access_kind),dimension(:),intent(inout)::arr
      integer,intent(in):: var_index
      integer,intent(inout):: start
      integer:: src,i
      if(iand(acc,access_is_list)/=0) then
         src=find_list_info(list_info_start(),access_info,var_index)
         if(debug_bprop) then
            write(*,*) 'COPY LIST INFO FOR PARAM>',par_index,var_index,src
         endif
         if(src<0) return
         arr(start)=par_index
         do i=1,access_info(src+1)+1
            arr(start+i)=access_info(src+i)
         enddo
         start=start+access_info(src+1)+2
         arr(start)=-1
      endif
    end subroutine copy_list_info_for_param_idx

    subroutine copy_list_info(arg1,arg2)
      type(pm_ptr),intent(in):: arg1,arg2
      integer:: list1,list2,n,i
      list1=find_list_info(list_info_start(),access_info,&
           cnode_get_num(arg1,var_index))
      if(debug_bprop) then
         write(*,*) 'COPY LIST INFO>',list1
      endif
      if(list1>0) then
         n=access_info(list1+1)
         list2=alloc_list_info(list_info_start(),n,&
              cnode_get_num(arg2,var_index),&
              access_is_var)
         do i=1,n
            access_info(list2+1+i)=ior(access_info(list2+1+i),access_info(list1+1+i))
         enddo
      endif
    end subroutine copy_list_info
    
    function list_info_start() result(n)
      integer:: n
      n=size(rvec)+pm_max_args+1
    end function list_info_start

    recursive subroutine tag_associated_param(arg)
      type(pm_ptr),intent(in):: arg
      integer:: flags
      type(pm_ptr):: var,new_call,new_var
      integer:: new_call_index,new_argn
      logical:: ok
      if(debug_bprop_tagging) write(*,*) 'TAG ASSOCIATED>'
      if(cnode_get_kind(arg)/=cnode_is_var) return
      var=arg
      flags=cnode_get_num(var,var_flags)
      if(iand(flags,var_is_reference+var_is_list_elem)==var_is_reference) then
         var=cnode_get(var,var_extra_info)
         flags=cnode_get_num(var,var_flags)
      endif
      if(debug_bprop_tagging) write(*,*) 'VAR_FLAGS_AND_INDEX>',flags,cnode_get_num(var,var_index)
      if(iand(flags,var_is_param+var_is_key+var_is_key_ptr+var_is_varg+var_is_list_elem)/=0) then
         if(debug_bprop_tagging) write(*,*) 'TAGGING>'
         call combine_access_info_at_idx(cnode_get_num(var,var_index),&
              access_needs_movability,.false.)
      endif
    end subroutine tag_associated_param

  end subroutine bprop_call
    
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
    integer,intent(in),optional:: name
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
    integer,intent(in),optional:: name
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
          if(name>0) then
             call pm_name_string(coder%context,name,str)
             str=trim(pm_opts%error)//' '//trim(message)//' '//trim(str)
          else
             str=trim(pm_opts%error)//' '//message
          endif
       else
          str=trim(pm_opts%error)//' '//message
       endif
       write(*,'(A)') trim(str)
       write(*,*)
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
         trim(pm_name_as_string(coder%context,cnode_var_name(var))))
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
    integer:: k,top,chunk
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
    write(*,'(a)')    '=====================CALL TRACE==========================='
    write(*,*)
    if(top>max_trace_depth) then
       write(*,*) '------------------------------------------------------'
       write(*,*) ' ... UNRECORDED PROCEDURES (TOO MANY NESTED CALLS) ...'
       write(*,*) '------------------------------------------------------'
       write(*,*)
       top=max_trace_depth
    endif
    if(top<=pm_opts%trace_list.or.pm_opts%show_full_trace) then
       do k=top,1,-1
          call trace_entry
       enddo
    else
       chunk=max(2,pm_opts%trace_list/2-1)
       do k=top,top-chunk+1,-1
          call trace_entry
       enddo
       write(*,*) '---------------------------'
       write(*,*) ' ...  (CALLS SKIPPED) ...'
       write(*,*) '---------------------------'
       write(*,*)
       do k=chunk,1,-1
          call trace_entry
       enddo
       write(*,*)
       write(*,*) ' (Use -fshow-full-trace to show the complete call trace)'
    endif
    write(*,'(a)')    '=========================================================='
    write(*,*)
  contains

    subroutine trace_entry
      node=coder%trace(k)
      if((.not.hide(node)).or.&
           (.not.pm_opts%hide_sysmod)) then
         call print_call_details(coder,node,&
              coder%trace_keys(k))
         if(k>1) write(*,*)
      endif
    end subroutine trace_entry

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
      if(name==sym_pm_assign.or.name==sym_assign_var.or.&
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
    character(len=1024):: str,string
    character(len=2):: join,ampstr
    character(len=1):: procchr,dotchr
    integer:: n,n0,k,nargs,nkeys
    integer::ampidx,signame,signamebase,tno,dtyp,ttyp,mode
    type(pm_ptr):: tv,key,val,amp,keyargs,keynames,name
    logical:: iscond
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
    elseif(signamebase==sym_pm_assign.or.signamebase==sym_assign_var) then
       signame=sym_assign
    elseif(signamebase==sym_make_subref.or.signamebase==sym_make_sublhs.or.&
         signamebase==sym_make_sublhs_amp) then
       signame=sym_sub
    endif


    if(cnode_flags_set(node,call_flags,proccall_is_comm)) then
       n=num_comm_args
       if(cnode_flags_set(node,call_flags,proccall_is_general)) then
          procchr=' '
       else
          procchr='%'
       endif
    else
       n=1
       procchr=' '
    endif

    if(cnode_flags_set(node,call_flags,proccall_is_method)) then
       procchr=' '
       dotchr= '.'
    else
       dotchr= ' '
    endif

    n0=n
    if(cnode_flags_set(node,call_flags,proccall_is_block)) then
       n=n+3
    endif

    string=dotchr//trim(pm_name_as_string(coder%context,&
         signame))//procchr

    k=len_trim(string)+1
    if(n0>1) then
       iscond=coder%wstack(base+nkeys+2+2)==coder%true_literal
       dtyp=pm_type_strip_mode(coder%context,coder%wstack(base+nkeys+3),mode)
       if(dtyp==pm_null) dtyp=0
    else
       iscond=.false.
       dtyp=0
    endif
    ttyp=pm_type_strip_mode(coder%context,coder%wstack(base+nkeys+3),mode)
    if(ttyp==pm_null) ttyp=0
    call par_context_to_string(coder%context,iscond,.false.,ttyp,dtyp,string,k)
    string(k:k)='('
    
    call more_error(coder%context,trim(string))
    n0=n
    if(pm_opts%show_hidden) n=0
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
       tno=coder%wstack(base+nkeys+2+n0-2)
       if(pm_type_kind(coder%context,tno)==pm_type_is_par_kind) tno=pm_type_arg(coder%context,tno,1)
       if(pm_type_kind(coder%context,tno)==pm_type_is_proc) tno=pm_type_arg(coder%context,tno,1)
       call more_error(coder%context,'     '//&
            trim(pm_type_as_string(coder%context,tno)))
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
    integer:: istart,istart0,n,tno,tno2,nret,i
    character(len=1024):: str,str2,buf1,buf2
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
 
    if(cnode_flags_set(node,pr_flags,proccall_is_method)) then
       str(n:n)='.'
       n=n+1
    endif
    call pm_name_string(coder%context,name,str(n:))
    n=len_trim(str)+1
    if(cnode_flags_set(node,pr_flags,proccall_is_comm)) then
       if(cnode_flags_set(node,pr_flags,proccall_is_general)) then
          str(n:n)=' '
       else
          str(n:n)='%'
       endif
       n=n+1
       istart=num_comm_args+1
    else
       istart=2
    endif
    istart0=istart
    if(cnode_flags_set(node,pr_flags,proccall_is_block)) istart=istart+3
    if(pm_opts%show_hidden) istart=1
    tno=cnode_get_num(node,pr_ptype)
    tno2=0
    if(istart0>2) then
       tno2=pm_type_arg(coder%context,tno,3)
    endif
    call par_context_to_string(coder%context,cnode_flags_set(node,pr_flags,proc_is_cond),&
         cnode_flags_set(node,pr_flags,proc_is_uncond),&
         pm_type_arg(coder%context,tno,1),tno2,str,n)
    call pm_type_to_string(coder%context,tno,str,n,tuple_start=istart)
    n=n+1
    if(n>len(str)-20) then
       str(n:n+2)='...'
    else
       if(cnode_flags_set(node,pr_flags,proccall_is_block)) then
          str(n:)='yield '
          n=n+6
          !write(*,*) istart, pm_type_as_string(coder%context,tno)
          tno=pm_type_arg(coder%context,pm_type_arg(coder%context,tno,istart0),1)
          call pm_type_to_string(coder%context,tno,str,n)
       endif
    endif
777 continue
    call more_error(coder%context,trim(str))
  contains

    include 'fesize.inc'
    include 'fisnull.inc'

  end subroutine print_proc_details
  
end module pm_infer
