!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2023
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
  use pm_parser
  use pm_sysdefs
  use pm_codegen
  implicit none

  ! Print compiler debugging messages
  logical,parameter:: debug_inference=.false.
  
  ! Maximum times a procedure template can call itself with
  ! *different* arguments types each time
  integer,parameter:: max_recur=32

  ! Special signatures
  integer,parameter:: sp_sig_in_process=-1_pm_p
  integer,parameter:: sp_sig_recursive=-2_pm_p
  integer,parameter:: sp_sig_break=-3_pm_p
  integer,parameter:: sp_sig_thru=-4_pm_p
  integer,parameter:: sp_sig_dup=-5_pm_p
  integer,parameter:: sp_sig_noop=-6_pm_p
  
  ! Special types
  integer,parameter:: undefined=-1
  integer,parameter:: error_type=-2

  ! Parallel modes
  integer,parameter:: par_mode_outer=1
  integer,parameter:: par_mode_multi_node=2
  integer,parameter:: par_mode_single_node=3
  integer,parameter:: par_mode_conc=4
  integer,parameter:: par_mode_inner=5

contains
  
  !============================================================
  ! The following routines process the intermediate code tree 
  ! applying type inference and resolving polymorphic procedure 
  ! calls at compile time
  !=============================================================

  !==============================
  ! Type-infer main program
  !============================== 
  subroutine prc_prog(coder)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: cnode
    integer:: i,base

    if(debug_inference) write(*,*) 'PRC PROG>'

    coder%flag_recursion=.false.
    coder%par_depth=0
    coder%poly_cache=pm_dict_new(coder%context,32_pm_ln)
    coder%first_pass=.true.
   
    do
       coder%top=1
       coder%wtop=1
       coder%par_kind2=par_mode_outer
       coder%par_kind=par_mode_outer
       coder%types_finished=.true.
       coder%redo_calls=.false.
       coder%incomplete=.false.
       coder%taints=0
       
       coder%proc_cache=pm_dict_new(coder%context,32_pm_ln)
       
       ! Setup resolution stack block
       base=create_stack_frame(coder,0,coder%index,0)
       
       ! Process program code
       call prc_cblock(coder,top_code(coder),3)

       ! Uncaught break implies infinite recursion
       if(coder%incomplete) then
          if(coder%num_errors==0) then
             call more_error(coder%context,&
                  'Error: A procedure in this program has infinite recursion')
             coder%flag_recursion=.true.
             call prc_cblock(coder,top_code(coder),3)
             call pm_stop('Program contains infinite recursion')
          endif
       endif
       if(debug_inference) write(*,*) 'FULL PASS FINISHED>',coder%types_finished
       if(coder%types_finished) exit
       coder%first_pass=.false.
    enddo
    
    ! Create resolved code object
    call code_int_vec(coder,coder%stack,3,coder%top)
    call code_num(coder,coder%stack(1))
    call make_code(coder,pm_null_obj,cnode_is_resolved_proc,3)

    if(debug_inference) write(*,*) 'END OF PROG> vtop=',coder%vtop

  contains
    include 'fnewnc.inc'
    include 'ftiny.inc'
  end subroutine  prc_prog

  ! ====================================================
  ! Type-infer procedure
  ! Returns signature index as tiny int in on vstack
  ! ====================================================
  function prc_proc(coder,prc,callnode,atype,ptype,nret,nkeys,&
       keynames,oldbase) result(rtype)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: prc,callnode
    integer,intent(in):: atype,ptype
    integer,intent(in):: nret,nkeys
    type(pm_ptr),intent(in):: keynames
    integer,intent(in):: oldbase
    type(pm_ptr):: cnode,cac
    integer:: rtype
    integer:: at
    integer,dimension(4+pm_max_args):: key
    integer:: i,j,base,keysize
    integer(pm_ln):: k
    logical:: save_redo_calls,save_incomplete
    integer:: taints,save_taints
    integer:: keypartyp,keyargtyp
    type(pm_ptr):: save_prc, keys
    logical:: iscomm
   
    taints=0

    if(pm_debug_checks) then
       if(cnode_get_kind(prc)/=cnode_is_proc) then
          call pm_panic('prc-proc prc not proc')
       endif
    endif

    if(cnode_flags_set(prc,pr_flags,proc_is_abstract)) then
       call infer_error(coder,callnode,&
            'Abstract procedure needs to be implemented for the given argument list')
       call infer_error(coder,prc,&
            'Abstract procedure definition referenced in the above error')
       call infer_trace(coder)
       rtype=error_type
    endif

    iscomm=cnode_flags_set(prc,pr_flags,proc_is_comm)

    ! Dictionary entries in coder%proc_cache:
    ! Key is proc and argument types and implicit par_kind
    ! Value is tiny int with procedure return type (if >0)
    ! or (-1) sp_in_process in process of resolution
    ! or (-2) sp_recursive called recursively
    ! or (-3) sp_break  breaking (or previously broke) out of inference
    
    ! Is this combination already cached?
    key(1)=cnode_get_num(prc,pr_id)
    key(2)=atype
    keysize=4
    keys=cnode_get(prc,pr_tkeys)
    if(.not.pm_fast_isnull(keys)) then
       ! If there are type-constrained key args, need to add their types to
       ! the cache key
       do i=0,pm_fast_esize(keys),2
          j=keys%data%i(keys%offset+i)
          key(5+i)=coder%wstack(coder%proc_key_base+j)
       enddo
       keysize=5+pm_fast_esize(keys)
    endif
    if(debug_inference) write(*,*) 'LOOKUP',coder%par_kind,coder%par_kind2
    if(iscomm) then
       key(3)=coder%par_kind
       key(4)=coder%par_kind2
       k=pm_ivect_lookup(coder%context,coder%proc_cache,key,keysize)
    else
       key(3)=-1
       key(4)=-1
       k=pm_ivect_lookup(coder%context,coder%proc_cache,key,keysize)
       if(k==0) then
          key(3)=coder%par_kind
          key(4)=-1
          k=pm_ivect_lookup(coder%context,coder%proc_cache,key,keysize)
       endif
    endif
    
    if(debug_inference) then
       write(*,*) 'PRC PROC>',key(1),key(2),key(3),key(4),k,&
            trim(pm_name_as_string(coder%context,&
            cnode_get_name(prc,pr_name))),&
            trim(pm_typ_as_string(coder%context,atype))
    endif

    ! This combination already cached
    if(k>0) then
       cnode=pm_dict_val(coder%context,coder%proc_cache,k)
       if(debug_inference) then
          write(*,*) 'FOUND',k,'-->',key(1:keysize)
          write(*,*) 'CACHED>',k,cnode%data%vkind,cnode%offset,&
               trim(pm_name_as_string(coder%context,&
               cnode_get_name(prc,pr_name))),sp_sig_recursive,sp_sig_in_process
       endif

       ! One of the special codes
       if(pm_fast_istiny(cnode)) then
          if(cnode%offset==sp_sig_break) then
             goto 10
          elseif(cnode%offset==sp_sig_recursive) then
             if(coder%flag_recursion) then
                call infer_error(coder,prc,'Recursive call to: '//&
                     trim(pm_name_as_string(coder%context,&
                     cnode_get_name(prc,pr_name))))
                call infer_trace(coder)
                coder%flag_recursion=.false.
             endif
             coder%incomplete=.true.
             rtype=error_type
          elseif(cnode%offset==sp_sig_in_process) then
             call pm_dict_set_val(coder%context,coder%proc_cache,&
                  k,pm_fast_tinyint(coder%context,sp_sig_recursive))
              if(coder%flag_recursion) then
                call infer_error(coder,prc,'Recursive call to: '//&
                     trim(pm_name_as_string(coder%context,&
                     cnode_get_name(prc,pr_name))))
                call infer_trace(coder)
                coder%flag_recursion=.false.
             endif
             coder%incomplete=.true.
             rtype=error_type
          elseif(cnode%offset<0) then
             ! Another special sig
             rtype=atype
             call code_num(coder,int(cnode%offset))
          else
             ! Return type
             rtype=cnode%offset
             if(debug_inference) write(*,*) 'CACHED RETURN>',rtype
             call code_num(coder,int(k))
          endif
          return
       endif

       ! Not a special code so have an inferred type
 
       ! Pass out taints
       taints=cnode_num_arg(cnode,3)
       coder%taints=ior(coder%taints,iand(taints,proc_taints))

       ! If not quite complete need to run through again
       if(iand(taints,proc_unfinished)/=0.and.&
            coder%redo_calls.and..not.coder%flag_recursion) goto 10

       ! Check keyword arguments
       keys=cnode_arg(cnode,4)
       do i=0,nkeys-1
          keyargtyp=coder%wstack(coder%proc_key_base+i+1)
          keypartyp=keys%data%i(keys%offset+i)
          if(keyargtyp/=keypartyp) then
             if(keypartyp==pm_tiny_int) then
                keys%data%i(keys%offset+i)=keyargtyp
             elseif(keyargtyp/=pm_tiny_int) then
                call infer_error(coder,prc,'Keyword/argument type mismatch:',&
                     pm_set_key(coder%context,keynames,int(i+1,pm_ln)))
                call more_error(coder%context,'Mismatched argument: '//&
                     trim(pm_typ_as_string(coder%context,keyargtyp)))
                call more_error(coder%context,'Mismatched parameter: '//&
                     trim(pm_typ_as_string(coder%context,keypartyp)))
                call infer_trace(coder)
             endif
          endif
       enddo

       ! Push signature
       call code_num(coder,int(k))
       
       ! Get return type
       cnode=cnode_arg(cnode,2)
       rtype=cnode%data%i(cnode%offset)
       if(nret==0) rtype=0
       if(debug_inference) write(*,*) 'CACHED RTYPE>',rtype
       return
    endif

10  continue

    ! Proc is not (or not yet fully) inferred

    at=atype
    
    ! Check for infinite recursion with changing arg types
    if(cnode_get_num(prc,pr_recurse)>max_recur) then
       call infer_error_with_trace(coder,prc,&
            'Recursion appears to require infinite types')
       call code_num(coder,0)
       return
    endif

20  continue

    ! Flag call to check for recursion
    call cnode_incr_num(prc,pr_recurse,1)
    k=pm_idict_add(coder%context,coder%proc_cache,&
         key,keysize,pm_fast_tinyint(coder%context,sp_sig_in_process))

    ! Repeatedly type infer until complete
    save_incomplete=coder%incomplete
    save_taints=coder%taints
    do
       if(debug_inference) write(*,*) 'TRY>',key(1),key(2),key(3),key(4),rtype

       ! Create stack frame
       base=create_stack_frame(coder,at,cnode_get_num(prc,pr_max_index),taints)
       
       ! Process code
       coder%incomplete=.false.
       coder%taints=taints
       save_prc=coder%proc
       coder%proc=prc
       call prc_cblock(coder,cnode_arg(prc,1),base)
       coder%proc=save_prc

       ! Check  procedure record for recursion/completion
       cnode=pm_dict_val(coder%context,coder%proc_cache,k)
       if(.not.pm_fast_istiny(cnode)) then
          write(*,*) cnode%data%vkind,k
          call pm_panic('prc-proc bad cache')
       endif
       
       if(debug_inference) then
          write(*,*) 'TRY COMPLETE>',cnode%offset,&
               coder%stack(base),coder%stack(base-1),nret
       endif
         
       if(cnode%offset==sp_sig_in_process) then
          ! Not recursively called
          rtype=coder%stack(base)
          if(nret==0) rtype=0
          if(debug_inference) write(*,*) 'NOT RECURSIVE>',rtype,coder%incomplete
          exit
       else if(cnode%offset<=sp_sig_recursive) then
          ! Recursively called
          if(nret==0) coder%stack(base)=0
          
          if(coder%stack(base)<0) then
             ! No resolved type yet 
             ! flag cache entry
             ! and break out
             call pop_stack_frame(coder,base)
             cnode%offset=sp_sig_break
             call pm_dict_set_val(coder%context,&
                  coder%proc_cache,k,cnode)
             coder%proc_par_depth=coder%par_depth
             coder%incomplete=.true.
             coder%taints=save_taints
             rtype=error_type
             if(debug_inference) write(*,*) 'NOT RESOLVED>'
             return
          endif
          
          ! Flag procedure as recursive
          coder%taints=ior(coder%taints,proc_is_recursive)
          
          ! Cache resolved return type
          cnode%offset=coder%stack(base)
          call pm_dict_set_val(coder%context,coder%proc_cache,k,cnode)
       else
          ! Recursive call for which we 
          ! already have a return type
          ! check against type just returned
          if(debug_inference) write(*,*) 'RT>',rtype,coder%stack(base)
          rtype=cnode%offset

          if(debug_inference) write(*,*) 'RECURSIVE WITH TYPE>',rtype
          
          ! This error should not happen
          !(implies compiler bug as proc output type determined by args)
          if(nret>0.and.rtype/=coder%stack(base)) then
             call infer_error_with_trace(coder,prc,&
                  'Internal Compiler Error: Procedure return type changed')
          endif
          
          ! Flag procedure as recursive
          coder%taints=ior(coder%taints,proc_is_recursive)
          exit
       endif
    enddo

    if(debug_inference) then
       write(*,*) 'COMPLETED>',coder%stack(base),&
            coder%stack(base-1),base,oldbase,coder%stack(oldbase-1)
    endif

    ! Pass a break out
    if(coder%incomplete) then
       if(debug_inference) then
          write(*,*) 'OUTBREAK>',oldbase
       endif
       call pop_stack_frame(coder,base)
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
    if(iand(coder%taints,proc_is_recursive)/=0) then
       if(taints>0.or.nkeys>0) then
          coder%taints=ior(coder%taints,proc_unfinished)
       endif
    endif
    
    ! Create record of type-annotated code
    call code_val(coder,prc)
    call code_int_vec(coder,coder%stack,base,coder%top)
    call code_num(coder,&
         ior(iand(cnode_get_num(prc,pr_flags),&
         proc_is_comm+proc_run_shared+proc_run_local+proc_inline+&
         proc_no_inline+proc_run_complete+proc_run_always),&
         coder%taints))
    if(nkeys>0) then
       call code_int_vec(coder,coder%wstack,&
            coder%proc_key_base+1,coder%proc_key_base+nkeys)
    else
       call code_null(coder)
    endif
    call make_code(coder,pm_null_obj,cnode_is_resolved_proc,4)
    cnode=top_code(coder)
    if(iscomm) then
       key(3)=coder%par_kind
       key(4)=coder%par_kind2
    else
       key(3)=coder%par_kind
       key(4)=-1
    endif
    if(debug_inference) write(*,*) coder%par_kind,'CACHE AS>',key(1:keysize)
    k=pm_idict_add(coder%context,coder%proc_cache,&
         key,keysize,cnode)
    call drop_code(coder)

    ! For tainted recursive calls
    ! or recursive calls with keywords need one final pass
    if(iand(coder%taints,proc_is_recursive)/=0) then
       if(taints>0.or.nkeys>0) then
          save_redo_calls=coder%redo_calls
          coder%redo_calls=.true.
          call init_stack_frame(coder,base,at,taints)
          call prc_cblock(coder,cnode_arg(prc,1),base)
          coder%redo_calls=save_redo_calls
          call cnode_set_num(cnode,cnode_args+2,&
               ieor(cnode_num_arg(cnode,3),proc_unfinished))
       endif
    endif

    ! Now we have complete taints can check if proc needs par_kind
    if(iand(coder%taints,proc_needs_par)==0) then
       ! If not include a new entry - this will shadow entries
       ! specialised on par_kind due to search order
       key(3)=-1
       key(4)=-1
       if(debug_inference) write(*,*) 'RECACHE AS>',key(1:keysize)
       k=pm_idict_add(coder%context,coder%proc_cache,&
            key,keysize,cnode)
    endif

    ! Process special cases of 'each' procedures
    if(iand(coder%taints,proc_is_not_pure_each)==0) then
       if(.not.cnode_flags_clear(prc,pr_flags,&
            proc_is_thru_each+proc_is_empty_each+proc_is_dup_each)) then
          cnode=pm_fast_tinyint(coder%context,sp_sig_dup)
          if(cnode_flags_set(prc,pr_flags,proc_is_thru_each)) then
             cnode%offset=sp_sig_thru
          elseif(cnode_flags_set(prc,pr_flags,proc_is_empty_each)) then
             cnode%offset=sp_sig_noop
          endif
          k=pm_idict_add(coder%context,coder%proc_cache,&
               key,keysize,cnode)
          k=cnode%offset
       endif
    endif

    call code_num(coder,int(k))
    call pop_stack_frame(coder,base)
    call cnode_incr_num(prc,pr_recurse,-1)

    ! If calling (unresolved) proc is not the same as this proc
    ! then this cannot be a pure each proc so taint it as such
    if(.not.(prc==coder%proc)) then
       coder%taints=ior(coder%taints,proc_is_not_pure_each)
    endif
    
    ! Pass out taint information
    coder%proc_taints=iand(coder%taints,proc_taints)
    coder%taints=ior(save_taints,coder%proc_taints)
    
    if(pm_debug_level>3) then
       write(*,*) 'ENDPRC>',key(1),key(2),key(3),key(4),k
    endif
    
  contains
    include 'fnewnc.inc'
    include 'fistiny.inc'
    include 'ftiny.inc'
    include 'fvkind.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end function prc_proc

  ! ==================================================
  ! Type infer builtin procedure
  ! ===================================================
  function prc_builtin(coder,prc,atype,ptype,oldbase) result(rtype)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: prc
    integer,intent(in):: atype,ptype
    integer,intent(in):: oldbase
    integer:: rtype,mode
    type(pm_ptr):: p
    integer:: base,i
    integer:: t1,t2
    integer,dimension(2):: key
    integer:: k
    integer:: sym
    type(pm_ptr):: tv,tv2
    logical:: isstatic
    type(pm_typ_einfo):: einfo


    p=cnode_get(prc,bi_rcode)
    if(pm_fast_istiny(p)) then
       tv=pm_typ_vect(coder%context,atype)
       tv=pm_typ_vect(coder%context,pm_tv_arg(tv,1))
       rtype=(pm_tv_arg(tv,int(p%offset)))
    elseif(pm_fast_isnull(p)) then
       rtype=cnode_get_num(prc,bi_rtype)
       if(rtype<0) then
          ! Cached concrete return type
          rtype=-rtype
       else
          ! Convert type to concrete only representation and cache it
          if(rtype/=0) then
             rtype=pm_typ_as_concrete(coder%context,rtype,coder%wstack,&
                  isstatic)
             if(isstatic) call cnode_set_num(prc,bi_rtype,int(-rtype))
          endif
       endif
       if(cnode_get_num(prc,bi_rsym)==sym_dash) then
          tv=pm_typ_vect(coder%context,atype)
          do i=1,pm_tv_numargs(tv)
             tv2=pm_typ_vect(coder%context,pm_tv_arg(tv,i))
             if(pm_tv_kind(tv2)/=pm_typ_is_value) goto 20
          enddo
          call fold
          if(pm_is_compiling) then
             call code_num(coder,sp_sig_noop)
             return
          endif
20        continue
       endif
    else
       ! Process code for return expression to get base return type
       base=create_stack_frame(coder,atype,cnode_num_arg(prc,2),0)
       call prc_cblock(coder,cnode_get(prc,bi_rcode),base)
       rtype=coder%stack(base)
       call pop_stack_frame(coder,base)
       
       sym=cnode_get_num(prc,bi_rsym)
       if(rtype/=error_type) then
          ! Special processing of return type
          ! Specified by special character in return spec
          select case(sym)
          case(sym_hash,sym_pct)
             tv=pm_typ_vect(coder%context,rtype)
             tv=pm_typ_vect(coder%context,pm_tv_arg(tv,1))
             if(sym==sym_hash) then
                rtype=(pm_tv_arg(tv,2))
             else
                rtype=(pm_tv_arg(tv,1))
             endif
          case(sym_dim1:sym_dim7)
             tv=pm_typ_vect(coder%context,rtype)
             tv=pm_typ_vect(coder%context,pm_typ_strip_mode(coder%context,&
                  pm_tv_arg(tv,1),mode))
             if(pm_tv_kind(tv)==pm_typ_is_vect) then
                tv=pm_typ_vect(coder%context,pm_tv_arg(tv,1))
             endif
             rtype=pm_tv_arg(tv,int(sym-sym_dim1+1))
          case(sym_d1:sym_d7)
             tv=pm_typ_vect(coder%context,rtype)
             tv=pm_typ_vect(coder%context,pm_typ_strip_mode(coder%context,&
                  pm_tv_arg(tv,1),mode))
             if(pm_tv_kind(tv)==pm_typ_is_vect) then
                rtype=pm_tv_arg(tv,1)
                tv=pm_typ_vect(coder%context,rtype)
                rtype=pm_typ_strip_mode(coder%context,pm_tv_arg(tv,int(sym-sym_d1+1)),mode)
                if(mode<sym_mirrored.and.pm_tv_name(tv)/=sym_pling) then
                   rtype=pm_new_vect_typ(coder%context,rtype)
                endif
             else
                rtype=pm_tv_arg(tv,int(sym-sym_d1+1))
             endif
          case(sym_dcaret)
             tv=pm_typ_vect(coder%context,rtype)
             rtype=pm_typ_strip_mode(coder%context,&
                  pm_tv_arg(tv,1),mode)
             tv=pm_typ_vect(coder%context,rtype)
             if(pm_tv_kind(tv)==pm_typ_is_vect) then
                rtype=pm_tv_arg(tv,1)
             endif
          case(sym_dot)
             tv=pm_typ_vect(coder%context,rtype)
             tv=pm_typ_vect(coder%context,pm_tv_arg(tv,1))
             if(pm_tv_kind(tv)/=pm_typ_is_user) then
                call pm_panic('ref-access')
             endif
             rtype=(pm_tv_arg(tv,1))
          case(sym_gt)
             tv=pm_typ_vect(coder%context,rtype)
             rtype=(max(pm_tv_arg(tv,1),pm_tv_arg(tv,2)))
          case(sym_dim)
             tv=pm_typ_vect(coder%context,rtype)
             rtype=pm_new_arr_typ(coder%context,sym_const,&
                  pm_tv_arg(tv,1),pm_tv_arg(tv,2),int(pm_long))
          case(sym_vdim)
             tv=pm_typ_vect(coder%context,rtype)
             rtype=pm_new_arr_typ(coder%context,sym_var,&
                  pm_tv_arg(tv,1),pm_tv_arg(tv,2),int(pm_long))
          case(sym_invar_dim)
             tv=pm_typ_vect(coder%context,rtype)
             rtype=pm_new_arr_typ(coder%context,sym_invar,&
                  pm_tv_arg(tv,1),pm_tv_arg(tv,2),int(pm_long))
          case(sym_fix_dim)
             tv=pm_typ_vect(coder%context,rtype)
             rtype=pm_new_arr_typ(coder%context,sym_fix,&
                  pm_tv_arg(tv,1),pm_tv_arg(tv,2),pm_tv_arg(tv,3))
          case(sym_over)
             tv=pm_typ_vect(coder%context,rtype)
             tv2=pm_typ_vect(coder%context,pm_tv_arg(tv,1))
             rtype=pm_new_arr_typ(coder%context,&
                  pm_tv_name(tv2),pm_tv_arg(tv2,1),pm_tv_arg(tv,2),int(pm_long))
          case(sym_eq)
             tv=pm_typ_vect(coder%context,rtype)
             t1=pm_tv_arg(tv,1)
             t2=pm_tv_arg(tv,2)
             if(pm_typ_equal(coder%context,t1,t2)) then
                rtype=(coder%true_name)
             else
                rtype=(coder%false_name)
             endif
          case(sym_caret)
             rtype=(int(pm_tiny_int))
          case(sym_includes)
             tv=pm_typ_vect(coder%context,rtype)
             t1=pm_tv_arg(tv,1)
             t2=pm_tv_arg(tv,2)
             tv=pm_typ_vect(coder%context,t1)
             t1=pm_tv_arg(tv,1)
             tv=pm_typ_vect(coder%context,t2)
             t2=pm_tv_arg(tv,1)
             if(pm_typ_includes(coder%context,t1,t2,pm_typ_incl_typ,einfo)) then
                rtype=(coder%true_name)
             else
                rtype=(coder%false_name)
             endif
          case(sym_type)
             tv=pm_typ_vect(coder%context,rtype)
             t1=pm_tv_arg(tv,1)
             rtype=pm_new_type_typ(coder%context,t1)
          case(sym_query)
             rtype=error_type
             coder%num_errors=coder%num_errors+1
          end select
       endif
    endif
    ! Create cache entry
    key(1)=-cnode_get_num(prc,bi_id)-1
    k=pm_idict_add(coder%context,&
         coder%proc_cache,key,1,prc)
    call code_num(coder,k)

    ! Pass out taint information
    coder%proc_taints=iand(proc_taints,cnode_get_num(prc,bi_flags))
    coder%taints=ior(coder%taints,coder%proc_taints)
  contains
    include 'fisnull.inc'
    include 'fistiny.inc'
    include 'fnew.inc'
    include 'fvkind.inc'

    ! Compile time computation of expressions
    subroutine fold
      integer:: i,n,errno
      type(pm_ptr):: arg1,arg2
      type(pm_ptr):: result
      logical:: ok
      character(len=100):: emess
      type(pm_ptr):: rtv
      integer:: rtyp
      n=pm_tv_numargs(tv)
      arg1=pm_dict_val(coder%context,coder%context%tcache,&
           int(pm_tv_arg(tv,1),pm_ln))
      if(n>1) then
         arg2=pm_dict_val(coder%context,coder%context%tcache,&
              int(pm_tv_arg(tv,2),pm_ln))
      endif
      rtv=pm_typ_vect(coder%context,rtype)
      rtyp=pm_tv_arg(rtv,1)
      if(rtyp/=pm_logical) then
         coder%temp=pm_fast_new(coder%context,int(rtyp,pm_p),1_pm_p)
         call fold_value(cnode_get_num(prc,bi_opcode),&
              coder%temp,arg1,arg2,ok,emess)
         if(.not.ok) then
            call infer_error_with_trace(coder,prc,&
                 'Cannot combine run time values: '//trim(emess))
         else
            rtype=(pm_new_value_typ(coder%context,coder%temp))
         endif
      else
         call fold_comparison(cnode_get_num(prc,bi_opcode),arg1,arg2,ok)
         if(ok) then
            rtype=(coder%true_name)
         else
            rtype=(coder%false_name)
         endif
      endif
      coder%temp=pm_null_obj
    end subroutine fold
    
  end function prc_builtin

  !==========================================
  ! Type infer code block
  !==========================================
  subroutine prc_cblock(coder,cblock,base)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    integer,intent(in):: base
    integer:: nvars,i,newbase
    type(pm_ptr):: p
    if(pm_fast_isnull(cblock)) return
    p=cnode_get(cblock,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       call prc_call(coder,cblock,p,base)      
       p=cnode_get(p,call_link)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine prc_cblock

  !=======================================================
  ! Type infer call
  ! (calls include control structures as a special case)
  !========================================================
  subroutine prc_call(coder,cblock,callnode,base)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,cblock
    integer,intent(in):: base
    integer:: sig
    integer:: tno,tno2,tno3,tno4,name,off,flags,mode,mode2
    type(pm_ptr):: args,t,t2,tv,list,list2,namep
    integer:: i,j,n,nret,nargs,nextra,slot,slot2,tbase,wbase
    integer:: vbase_check,tbase_check,counter,nerrors
    integer:: save_par_kind
    integer,dimension(2):: key
    logical:: ok,isstatic,mayfail,undef_arg,cond
    integer(pm_ln):: k
    character(len=100):: str
    type(pm_typ_einfo):: einfo

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
       case(sym_while)
          list=cnode_arg(args,2)
          list2=cnode_arg(args,4)
          counter=0
          do
             call clear_cblock_mark(list)
             call clear_cblock_mark(list2)
             call prc_cblock(coder,list,base)
             call check_logical(3)
             if(arg_type(3)==coder%false_name) return
             call prc_cblock(coder,list2,base)
             if(.not.(cblock_marked(list).or.&
                  cblock_marked(list2))) exit
             counter=counter+1
             if(counter>max_recur) then
                call infer_error_with_trace(coder,args,&
                     '"while" appears to lead to infinite types')
                exit
             endif
          enddo
          if(cblock_has_comm(cnode_arg(args,2))&
               .or.cblock_has_comm(cnode_arg(args,4))) then
             call set_call_sig(merge(1,0,coder%par_kind2<=par_mode_conc))
          else
             call set_call_sig(0)
          endif
       case(sym_until,sym_each)
          list=cnode_arg(args,2)
          counter=0
          do 
             call clear_cblock_mark(list)
             call prc_cblock(coder,list,base)
             if(.not.cblock_marked(list)) exit
             counter=counter+1
             if(counter>max_recur) then
                call infer_error_with_trace(coder,args,&
                     trim(sym_names(sig))//' appears to lead to infinite types')
                exit
             endif
          enddo
          call check_logical(3)
          if(cblock_has_comm(list)) then
             call set_call_sig(merge(1,0,coder%par_kind<=par_mode_conc))
          else
             call set_call_sig(0)
          endif
       case(sym_if,sym_if_invar)
          call check_logical(1)
          tno=arg_type(1)
          list=cnode_arg(args,2)
          if(tno/=coder%false_name) then
             call prc_cblock(coder,list,base)
          endif
          list=cnode_arg(args,3)
          if(tno/=coder%true_name) then
             call prc_cblock(coder,list,base)
          endif
       case(sym_do,sym_for,sym_also)
          call prc_cblock(coder,cnode_arg(args,1),base)
       case(sym_sync)
          call prc_cblock(coder,cnode_arg(args,2),base)
       case(sym_over)
          call prc_cblock(coder,cnode_arg(args,1),base)
          call prc_cblock(coder,cnode_arg(args,2),base)
       case(sym_import_val,sym_import_param)
          tno=pm_typ_strip_mode(coder%context,arg_type_with_mode(2),mode)
          coder%stack(get_slot(1))=pm_typ_add_mode(coder%context,tno,sym_shared,.false.)
          if(tno>0.and.(coder%par_kind==par_mode_conc)) then
             if(iand(pm_typ_flags(coder%context,tno),&
                  pm_typ_has_distributed)/=0) then
                tno=pm_typ_strip_mode(coder%context,arg_type_with_mode(3),mode)
                if(iand(pm_typ_flags(coder%context,tno),&
                     pm_typ_has_distributed)==0) then
                   if(cnode_get_name(callnode,cnode_modl_name)/=sym_pm_system) then
                      call infer_error_with_trace(coder,callnode,&
                           'Cannot import distributed value into mirrored "forall"')
                      coder%stack(get_slot(1))=error_type
                   endif
                endif
             endif
          endif
          call flag_import_export(tno)
       case(sym_import_varg)
          tno=arg_type(2)
          if(tno>0) then
             t=pm_typ_vect(coder%context,arg_type(2))
             n=pm_tv_numargs(t)
             call push_word(coder,pm_typ_new_tuple)
             call push_word(coder,0)
             do i=1,n
                tno=pm_typ_strip_mode(coder%context,pm_tv_arg(t,i),mode)
                if(iand(pm_typ_flags(coder%context,tno),pm_typ_has_distributed)/=0) then
                   call infer_error_with_trace(coder,callnode,&
                        'Cannot use a distibuted shared value as an argument'//&
                        ' to a non-communicating operation')
                endif
                call push_word(coder,&
                     pm_typ_add_mode(coder%context,tno,sym_shared,.false.))
             enddo
             call make_type(coder,n+2)
             tno=pop_word(coder)
             coder%stack(get_slot(1))=tno
             call flag_import_export(tno)
          endif
       case(sym_import_shared)
          tno=pm_typ_strip_mode(coder%context,arg_type_with_mode(2),mode)
          coder%stack(get_slot(1))=pm_typ_add_mode(coder%context,tno,sym_shared,.false.)
          call flag_import_export(tno)
       case(sym_export)
          tno=arg_type_with_mode(1)
          mode=pm_typ_get_mode(coder%context,tno)
          if(mode/=sym_coherent.and.mode/=sym_partial) then
             call infer_error(coder,callnode,&
                  'Cannot modify "'//trim(sym_names(mode))//'" variable as a "shared" value '//&
                  'in a nested parallel statement: '//&
                  trim(pm_name_as_string(coder%context,&
                  cnode_get_name(cnode_arg(args,1),var_name))))
             call infer_error_with_trace(coder,cnode_arg(args,1),&
                  'Definition of variable in above error')
          endif
          call flag_import_export(0)
       case(sym_export_as_new,sym_export_param)
          coder%stack(get_slot(1))=arg_type_with_mode(2)
          call flag_import_export(arg_type(1))
       case(sym_amp_error)
          if(nargs>0) then
             coder%stack(get_slot(1))=arg_type_with_mode(2)
          endif
          call infer_error_with_trace(coder,callnode,&
               'Cannot change variable in different parallel context')
       case(sym_pm_send:sym_pm_serve)
          call check_long(5)
          coder%taints=ior(coder%taints,proc_is_impure)
          coder%stack(get_slot(1))=pm_long
          tno=pm_typ_strip_mode_and_vect(coder%context,arg_type(4))
          t=pm_typ_vect(coder%context,tno)
          if(pm_tv_kind(t)/=pm_typ_is_dref) then
             call infer_error(coder,callnode,'Internal compiler error: Not a d-ref.')
          endif
          coder%stack(get_slot(2))=pm_typ_strip_mode_and_vect(coder%context,arg_type(4))
          if(sig==sym_pm_send.or.sig==sym_pm_collect) then
             coder%stack(get_slot(3))=pm_typ_strip_mode_and_vect(coder%context,arg_type(6))
          else
             coder%stack(get_slot(3))=pm_typ_strip_mode(coder%context,pm_tv_arg(t,1),mode)
          endif
          call prc_cblock(coder,cnode_arg(args,8),base)
       case(sym_pm_bcast)
          coder%taints=ior(coder%taints,proc_is_impure)
          coder%stack(get_slot(1))=arg_type(3)
          coder%stack(get_slot(2))=arg_type(4)
          call check_long(5)
          call prc_cblock(coder,cnode_arg(args,6),base)
       case(sym_pm_recv_req)
          coder%taints=ior(coder%taints,proc_is_impure)
          coder%stack(get_slot(1))=pm_long
          coder%stack(get_slot(2))=pm_typ_strip_mode_and_vect(coder%context,arg_type(3))
          call prc_cblock(coder,cnode_arg(args,5),base)
       case(sym_pm_recv_assn)
          coder%taints=ior(coder%taints,proc_is_impure)
          coder%stack(get_slot(1))=pm_long
          coder%stack(get_slot(2))=pm_typ_strip_mode_and_vect(coder%context,arg_type(4))
          coder%stack(get_slot(3))=pm_typ_strip_mode_and_vect(coder%context,arg_type(5))
          call prc_cblock(coder,cnode_arg(args,7),base)
       case(sym_pm_do,sym_pm_do_at)
          do i=merge(1,3,sig==sym_pm_do),nargs-1,2
             coder%stack(get_slot(i))=pm_typ_strip_mode_and_vect(coder%context,arg_type(i+1))
          enddo
          call prc_cblock(coder,cnode_arg(args,nargs),base)
       case(sym_pm_head_node)
          call prc_cblock(coder,cnode_arg(args,1),base)
       case(sym_pm_dref:sym_pm_ref)
          call push_word(coder,pm_typ_new_dref)
          slot=coder%wtop
          call push_word(coder,sym_pm_dref-sig-1)
          if(nargs==3) then
             t=pm_typ_vect(coder%context,arg_type(3))
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
                        trim(pm_typ_as_string(coder%context,coder%wstack(coder%wtop-i)))
                enddo
             endif
             call make_type_if_possible(coder,nargs+2)
          endif
          if(debug_inference) write(*,*) 'DREF=',&
               trim(pm_typ_as_string(coder%context,top_word(coder)))
          coder%stack(get_slot(1))=pop_word(coder)
       case(sym_hash)
          if(arg_type(2)/=pm_null) then
             call prc_cblock(coder,cnode_arg(args,1),base)
          endif
       case(sym_for_stmt)
          coder%taints=ior(coder%taints,proc_needs_par)
          tno=coder%par_kind
          coder%stack(get_slot(1))=pm_long
          coder%stack(get_slot(2))=tno
          if(tno==error_type) tno=pm_null
          if(tno>par_mode_multi_node.or.arg_type(7)==pm_null) then
             save_par_kind=coder%par_kind2
             coder%par_kind2=coder%par_kind
             if(tno>par_mode_multi_node) then
                coder%par_kind=par_mode_inner
             else
                coder%par_kind=par_mode_conc
             endif
             slot=get_slot(1)
             coder%stack(slot)=pm_long
             slot=get_slot(2)
             coder%stack(slot)=pm_null
             call prc_cblock(coder,cnode_arg(args,4),base)
             call prc_cblock(coder,cnode_arg(args,3),base)
             call make_code(coder,pm_null_obj,cnode_is_any_sig,0)
             coder%par_kind=coder%par_kind2
             coder%par_kind2=save_par_kind
          else
             save_par_kind=coder%par_kind2
             coder%par_kind2=coder%par_kind
             coder%par_kind=par_mode_multi_node
             nerrors=coder%num_errors
             call prc_cblock(coder,cnode_arg(args,4),base)
             list=cnode_arg(args,8)
             list=cnode_arg(list,1)
             slot=list%data%i(list%offset)
             slot2=list%data%i(list%offset+1)
             coder%stack(base+slot:base+slot2)=undefined
             call prc_cblock(coder,cnode_arg(args,3),base)
             coder%par_kind2=save_par_kind
             if(coder%num_errors==nerrors) then
                call code_int_vec(coder,coder%stack,base+slot,base+slot2)
                coder%stack(base+slot:base+slot2)=undefined
                coder%par_kind=par_mode_single_node
                call prc_cblock(coder,cnode_arg(args,3),base)
                if(coder%num_errors==nerrors) then
                   call code_int_vec(coder,coder%stack,base+slot,base+slot2)
                   call prc_cblock(coder,cnode_arg(args,5),base)
                   coder%par_kind=coder%par_kind2
                   coder%par_kind2=save_par_kind
                   if(coder%num_errors==nerrors) then
                      call make_code(coder,pm_null_obj,cnode_is_any_sig,2)
                   else
                      call drop_code(coder)
                      call drop_code(coder)
                      return
                   endif
                else
                   coder%par_kind=coder%par_kind2
                   coder%par_kind2=save_par_kind
                   call drop_code(coder)
                   return
                endif
             else
                coder%par_kind=coder%par_kind2
                coder%par_kind2=save_par_kind
                return
             endif
          endif
          list=pop_code(coder)
          key(1)=pm_dict_size(coder%context,coder%proc_cache)
          k=pm_idict_add(coder%context,coder%proc_cache,&
               key,1,list)
          call set_call_sig(int(k))
       case(sym_pct)
          if(nargs==1) then
             call prc_cblock(coder,cnode_arg(args,1),base)
          else
             tno=arg_type(3)
             slot=get_slot(1)
             coder%stack(slot)=tno
             call prc_cblock(coder,cnode_arg(args,2),base)
          endif
       case(sym_struct,sym_rec)
          t=cnode_arg(args,2)
          t=cnode_arg(t,1)
          if(cnode_num_arg(args,3)>=0) then
             tno=pm_user_typ_body(coder%context,cnode_num_arg(args,3))
             t2=pm_typ_vect(coder%context,tno)
          else
             tno=t%data%i(t%offset+1)
             t2=pm_typ_vect(coder%context,pm_tv_arg(pm_typ_vect(coder%context,tno),1))
          endif

          name=t%data%i(t%offset+2)
          if(sig==sym_struct) then
             call push_word(coder,pm_typ_new_struct+t%data%i(t%offset+4))
          else
             call push_word(coder,pm_typ_new_rec+t%data%i(t%offset+4))
          endif
          call push_word(coder,t%data%i(t%offset))
          do i=1,nargs-2
             call push_word(coder,arg_type_with_mode(i+3))
          enddo
          mode=pm_typ_combine_modes(coder%context,&
               coder%wstack(coder%wtop-nargs+3:coder%wtop),.false.,&
               cnode_flags_set(callnode,call_flags,proc_run_complete),&
               cnode_flags_set(callnode,call_flags,call_is_cond),&
               cnode_flags_set(callnode,call_flags,call_is_unlabelled))
          if(mode<0) then
             if(mode>-1000) then
                namep=pm_name_val(coder%context,pm_tv_name(t2))
                call infer_error_with_trace(coder,callnode,&
                     'Cannot use a shared distributed value'//&
                     ' in "new" to initialise: '//&
                     trim(pm_name_as_string(coder%context,&
                     namep%data%i(namep%offset-mode))))
             else
                call infer_error_with_trace(coder,callnode,&
                     'Cannot use a "prtl" value in "new" '//&
                     'in a "cplt" context to initialise: '//&
                     trim(pm_name_as_string(coder%context,&
                     namep%data%i(namep%offset-mode))))
             endif
             mode=sym_mirrored
          endif
          do i=1,nargs-2
             tno2=pm_typ_strip_mode(coder%context,coder%wstack(coder%wtop-nargs+2+i),mode2)
             tno3=pm_tv_arg(t2,i)
             if(tno2==pm_tiny_int) then
                tno2=tno3
                if(tno2==0.or.iand(pm_typ_flags(coder%context,tno2),&
                     pm_typ_has_storage)/=0) then
                   namep=pm_name_val(coder%context,pm_tv_name(t2))
                   call infer_error(coder,callnode,'Element "'//&
                        trim(pm_name_as_string(coder%context,&
                        namep%data%i(namep%offset+i)))//&
                        ':'//trim(pm_typ_as_string(coder%context,tno2))//'" of "'//&
                        trim(pm_name_as_string(coder%context,name))//&
                        '" needs to be initialised')
                endif
             endif
             
!!$             tno4=pm_typ_convert(coder%context,tno3,tno2,sig==sym_struct)
!!$             if(tno4>0) then
!!$                if(iand(pm_typ_flags(coder%context,tno4),pm_typ_has_params)/=0) then
!!$                   namep=pm_name_val(coder%context,pm_tv_name(t2))
!!$                   call infer_error(coder,callnode,'Element "'//&
!!$                        trim(pm_name_as_string(coder%context,&
!!$                        namep%data%i(namep%offset+i)))//&
!!$                        ':'//trim(pm_typ_as_string(coder%context,tno4))//'" of "'//&
!!$                        trim(pm_name_as_string(coder%context,name))//&
!!$                        '" cannot be initialised')
!!$                   call more_error(coder%context,&
!!$                        'unless the "new" statement supplies explict parameters for the type: new T(...) { }"')
!!$                endif
!!$                tno2=tno4
!!$                !write(*,*) 'Converted to:',trim(pm_typ_as_string(coder%context,tno4))
!!$             endif
   
             coder%wstack(coder%wtop-nargs+2+i)=tno2
          enddo
          call make_type_if_possible(coder,nargs)
          tno2=pop_word(coder)
          if(tno2>0) then
             if(.not.pm_typ_includes(coder%context,tno,tno2,&
                  pm_typ_incl_val,einfo)) then
                 call infer_error(coder,callnode,&
                      '"'//trim(sym_names(sig))//&
                      '" initial expression has wrong type for: ',&
                      pm_fast_name(coder%context,name))
                 call pm_typ_error(coder%context,einfo)
                 call infer_trace(coder)
                 tno2=error_type
              endif
           endif
           tno2=pm_typ_add_mode(coder%context,tno2,mode,&
               cnode_flags_set(callnode,call_flags,call_is_cond))
          call combine_types(cnode_arg(args,1),tno2)
       case(sym_dot,sym_dot_ref,sym_get_dot,sym_get_dot_ref)
          if(sig==sym_get_dot.or.sig==sym_get_dot_ref) then
             tno=arg_type(3)
             if(tno/=error_type) then
                namep=pm_typ_vect(coder%context,arg_type(3))
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
          tno=pm_typ_strip_mode_and_cond(coder%context,&
               arg_type_with_mode(2),mode,cond)
          if(tno>0) then
             call set_call_sig(resolve_elem(tno,name,&
                  sig==sym_dot_ref.or.sig==sym_get_dot_ref,.false.,tno2))
             call combine_types(cnode_arg(args,1),&
                  pm_typ_add_mode(coder%context,tno2,mode,cond))
          else
             call set_arg_to_error_type(1)
          endif
       case(sym_method_call)
          tno=arg_type(2)
          namep=cnode_arg(cnode_arg(args,3),1)
          name=namep%offset
          slot=resolve_elem(tno,name,.false.,.false.,tno2)
          if(slot/=0) then
             call combine_types(cnode_arg(args,1),tno2)
          endif
          call set_call_sig(slot)
       case(sym_default)
          tno=cnode_num_arg(cnode_arg(args,2),1)
          slot=get_slot(1)
          coder%stack(slot)=tno
       case(sym_cast)
          ! Arg 3 is type to cast to (-ve if in a conditional context)
          tno=arg_type(3)
          if(tno==error_type) then
             call set_arg_to_error_type(1)
             return
          endif
          if(pm_typ_kind(coder%context,tno)==pm_typ_is_type) then
             tno=pm_typ_arg(coder%context,tno,1)
          else
             call infer_error_with_trace(coder,callnode,&
                  '"as" second argument is not a type')
             call set_arg_to_error_type(1)
             return
          endif
          tno2=pm_typ_strip_mode_and_cond(coder%context,&
               arg_type_with_mode(2),mode,cond)
          k=prc_cast(coder,callnode,tno,tno2,.true.)
          call set_call_sig(int(k))
          call combine_types(cnode_arg(args,1),&
               pm_typ_add_mode(coder%context,tno2,mode,cond))
       case(sym_var_set_mode)
          mode2=cnode_num_arg(args,2)
          coder%stack(get_slot(1))=pm_typ_add_mode(coder%context,&
               pm_typ_strip_mode(coder%context,&
               arg_type_with_mode(1),mode),mode2,.false.)
          if(mode==sym_partial.or.&
               mode2>=sym_mirrored.and.mode<sym_mirrored) then
             call infer_error_with_trace(coder,callnode,&
                  'Cannot initialise "'//&
                  trim(sym_names(mode2))//'" variable with "'//&
                  trim(sym_names(mode))//'" value')
          endif
       case(sym_partial,sym_coherent)
          coder%stack(get_slot(1))=pm_typ_replace_mode(coder%context,&
               arg_type_with_mode(1),sig,.false.)
       case(sym_set_mode)
          mode=cnode_num_arg(args,2)
          coder%stack(get_slot(1))=pm_typ_replace_mode(coder%context,&
               arg_type_with_mode(1),mode,.false.)
       case(sym_change_mode)
          mode=cnode_num_arg(args,3)
          coder%stack(get_slot(1))=pm_typ_replace_mode(coder%context,&
               arg_type_with_mode(2),mode,.false.)
       case(sym_invar)
          tno=pm_typ_strip_mode(coder%context,arg_type_with_mode(1),mode)
          if(mode<sym_mirrored) then
             call infer_error_with_trace(coder,callnode,&
                  'Expression must be invariant instead of: '//trim(sym_names(mode)))
          endif
       case(sym_assignment)
          tno=pm_typ_get_mode(coder%context,arg_type_with_mode(1))
          if(tno>=sym_mirrored) then
             call infer_error(coder,callnode,&
                  'Assignments to "'//trim(sym_names(tno))//&
                  '" variables are not allowed outside of a "sync" statement') 
          elseif(tno>=sym_chan) then
             call infer_error(coder,callnode,&
                  'Assignments to "'//trim(sym_names(tno))//&
                  '" variables must be labelled in a conditional context') 
          endif
       case(sym_type_val)
          tno=cnode_num_arg(cnode_arg(args,2),1)
          call combine_types(cnode_arg(args,1),&
               pm_new_type_typ(coder%context,tno))
       case(sym_any)
          list2=cnode_arg(args,4)
          list2=cnode_arg(list2,1)
          slot=list2%data%i(list2%offset)
          slot2=list2%data%i(list2%offset+1)
          tno=pm_typ_strip_mode_and_cond(coder%context,arg_type(3),mode,cond)
          t=check_poly(coder,tno)
          if(tno/=error_type.and..not.pm_fast_isnull(t)) then
             n=pm_set_size(coder%context,t)
             do i=1,n
                list=pm_set_key(coder%context,t,int(i,pm_ln))
                tno=list%data%i(list%offset)
                coder%stack(base+slot:base+slot2)=undefined
                coder%stack(get_slot(1))=&
                     pm_typ_add_mode(coder%context,tno,mode,cond)
                call prc_cblock(coder,cnode_arg(args,2),base)
                call code_int_vec(coder,coder%stack,base+slot,base+slot2)
             enddo
             call make_code(coder,pm_null_obj,cnode_is_any_sig,n)
             list=pop_code(coder)
             if(.not.coder%incomplete) then
                key(1)=pm_dict_size(coder%context,coder%proc_cache)
                k=pm_idict_add(coder%context,coder%proc_cache,&
                     key,1,list)
                call set_call_sig(int(k))
             endif
          else
             coder%stack(base+slot:base+slot2)=undefined
             call set_arg_to_error_type(1)
             call prc_cblock(coder,cnode_arg(args,2),base)
          endif
       case(sym_each_proc)  ! this controls body for proc.. each()
          t=cnode_arg(args,nret+4)
          t=cnode_arg(t,1)
          slot=t%data%i(t%offset)
          slot2=t%data%i(t%offset+1)
          tno=arg_type(nret+5)
          if(tno<=0) then
             do i=1,nret
                call set_arg_to_error_type(i)
             enddo
             return
          endif
          t=pm_typ_vect(coder%context,tno)
          tno2=pm_tv_kind(t)
          flags=iand(pm_tv_flags(t),pm_typ_has_embedded)
          name=pm_tv_name(t)
          n=nargs-4
          if(tno2==pm_typ_is_struct.or.tno2==pm_typ_is_rec) then
             do i=nret+7,nargs-1,2
                tno=arg_type(i)
                t2=pm_typ_vect(coder%context,tno)
                if(pm_tv_kind(t2)/=tno2) then
                   if(coder%num_errors==0) &
                      call infer_error_with_trace(coder,callnode,&
                      '"proc <<each>>" arguments cannot mix "struct" and "rec"')
                endif
                if(pm_tv_name(t2)/=name) then
                   if(coder%num_errors==0) &
                        call infer_error_with_trace(coder,callnode,&
                        '"proc <<each>>" arguments must have the same "struct" or "rec" name')
                endif
             enddo
             n=pm_tv_numargs(t)
             if(nret>0) then
                call check_wstack(coder,nret*(n+2))
                tbase=coder%wtop
                coder%wtop=coder%wtop+nret*(n+2)
                do i=1,nret
                   coder%wstack(tbase+(i-1)*(n+2)+1)=ior(tno2,flags)
                   coder%wstack(tbase+(i-1)*(n+2)+2)=name
                enddo
             endif
             do i=1,n
                do j=nret+5,nargs-1,2
                   tno=arg_type(j)
                   t2=pm_typ_vect(coder%context,tno)
                   tno=pm_tv_arg(t2,i)
                   coder%stack(get_slot(j+1))=tno
                enddo
                call prc_cblock(coder,cnode_arg(args,nret+3),base)
                do j=1,nret
                   coder%wstack(tbase+(j-1)*(n+2)+i+2)=arg_type(nargs+j)
                enddo
                call code_int_vec(coder,coder%stack,base+slot,base+slot2)
             enddo
             call make_code(coder,pm_null_obj,cnode_is_any_sig,n)
             list=pop_code(coder)
             key(1)=pm_dict_size(coder%context,coder%proc_cache)
             k=pm_idict_add(coder%context,coder%proc_cache,&
                  key,1,list)
             slot=cnode_get_num(callnode,call_index)
             coder%stack(base+slot)=k
             tno3=pm_typ_from_recorded_name(coder%context,name)
             do i=nret,1,-1
                call make_type_if_possible(coder,n+2)
                if(.not.pm_typ_includes(coder%context,tno3,tno2,pm_typ_incl_val,einfo)) then
                   call infer_error(coder,args,&
                        '"'//trim(sym_names(tno2))//&
                        '" initial expression has wrong type for: ',&
                        pm_fast_name(coder%context,name))
                   call pm_typ_error(coder%context,einfo)
                   call infer_trace(coder)
                endif
                coder%stack(get_slot(i))=pop_word(coder)
             enddo
             call prc_cblock(coder,cnode_arg(args,nret+1),base)
          else
             call prc_cblock(coder,cnode_arg(args,nret+2),base)
             slot=cnode_get_num(callnode,call_index)
             coder%stack(base+slot)=0
          endif
       case(sym_test)
          call prc_cblock(coder,cnode_arg(args,1),base)
       case(sym_check)
          if(.not.pm_fast_isnull(cnode_arg(args,2))) then
             call prc_cblock(coder,cnode_arg(args,2),base)
          endif
          call prc_cblock(coder,cnode_arg(args,4),base)
          tno=arg_type(3)
          if(arg_type(1)/=pm_string_type.and.arg_type(1)/=error_type) then
             call infer_error_with_trace(coder,cnode_arg(args,1),&
                  'Check message is not a string, got:'//&
                  trim(pm_typ_as_string(coder%context,arg_type(1))))
          elseif(tno==coder%false_name) then
             if(cnode_get_kind(cnode_arg(args,1))==cnode_is_const) then
                call pm_strval(cnode_arg(cnode_arg(args,1),1),str)
                call infer_error_with_trace(coder,callnode,str(1:len_trim(str)-1))
             else
                call infer_error_with_trace(coder,callnode,&
                     'Check condition will always fail') 
             endif
          elseif(tno/=coder%true_name) then
             call check_logical(3)
             coder%stack(base-2)=ior(coder%stack(base-2),proc_is_impure)
          endif
       case(sym_dash)
          tno=arg_type(2)
          t=pm_typ_vect(coder%context,tno)
          if(iand(pm_tv_flags(t),pm_typ_has_storage)/=0) then
             call infer_error_with_trace(coder,callnode,&
                  'Value after '' cannot be determined at compile time')
          endif
          coder%stack(get_slot(1))=tno
       case(sym_dcaret)
          coder%stack(get_slot(1))=pm_new_vect_typ(coder%context,arg_type(2))
       case(sym_open)
          if(nargs>0) then
             t=pm_typ_vect(coder%context,coder%stack(base))
             n=pm_tv_numargs(t)
             do i=1,nargs
                slot=get_slot(i)
                coder%stack(slot)=pm_tv_arg(t,i)
                if(debug_inference) &
                     write(*,*) 'PARAM>',i,slot,&
                     pm_tv_arg(t,i+n-nargs),pm_tv_numargs(t)
             enddo
             if(n>nargs) then
                call push_word(coder,pm_typ_is_tuple)
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
          coder%stack(base)=undefined  
       case(sym_key)
          t=cnode_arg(args,2)
          t=cnode_arg(t,1)
          slot2=coder%wstack(coder%proc_key_base+t%data%i(t%offset))
          coder%stack(get_slot(1))=slot2
       case(sym_present)
          t=cnode_arg(args,4)
          t=cnode_arg(t,1)
          slot2=coder%wstack(coder%proc_key_base+t%data%i(t%offset))
          slot=arg_type(5)
          if(slot/=slot2)  then
             if(slot2==pm_tiny_int) then
                coder%wstack(coder%proc_key_base+t%data%i(t%offset))=slot
             elseif(nargs>3) then
                t=cnode_arg(args,6)
                t=cnode_arg(t,1)
                if(.not.pm_typ_includes(coder%context,t%data%i(t%offset),&
                     slot2,pm_typ_incl_val,&
                     einfo)) then
                   call infer_error(coder,callnode,'Keyword argument type mismatch:',&
                        cnode_get(cnode_arg(args,1),var_name))
                   call more_error(coder%context,'Mismatched argument: '//&
                        trim(pm_typ_as_string(coder%context,slot2)))
                   call more_error(coder%context,'Parameter type constraint: '//&
                        trim(pm_typ_as_string(coder%context,t%data%i(t%offset))))
                   call infer_trace(coder)
                endif
                slot=slot2
             else
                t=cnode_arg(args,1)
                t=cnode_get(t,var_name)
                call infer_error(coder,callnode,&
                     'Keyword argument type mismatch:',t)
                call more_error(coder%context,'Mismatched argument: '//&
                     trim(pm_typ_as_string(coder%context,slot2)))
                call more_error(coder%context,'Mismatched parameter: '//&
                     trim(pm_typ_as_string(coder%context,slot)))
                call infer_trace(coder)
             endif
          endif
          coder%stack(get_slot(1))=slot
          coder%stack(get_slot(2))=pm_logical
       case(sym_result)
          call get_arg_types_and_modes
          call make_type_if_possible(coder,nargs+2)
          coder%stack(base)=pop_word(coder)
       case(sym_start_loop)
           coder%stack(get_slot(2))=pm_logical
       case(sym_underscore,sym_colon,sym_end_loop,sym_init_var)
          continue
       case(sym_arg)
          call combine_types(cnode_arg(args,1),arg_type(2))
       case(first_pragma:last_pragma)
          if(sig==sym_infer_type.or.sig==sym_infer_type_and_stack) then
             call prc_cblock(coder,cnode_arg(args,1),base)
          endif
          if(sig==sym_infer_stack) then
             call cnode_error(coder,callnode,'Type inference stack trace:',warn=.true.)
          endif
          if(sig==sym_infer_type_and_stack.or.sig==sym_infer_stack) then
             call infer_trace(coder)
          endif
       case(sym_pm_dump)
          if(coder%first_pass) then
             if(arg_type_with_mode(1)>0) then
                call cnode_error(coder,callnode,'Type inference gives: '//&
                     trim(pm_typ_as_string(coder%context,arg_type_with_mode(1))),warn=.true.)
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
       call prc_proc_call(coder,cblock,callnode,-sig,args,nargs,nret,base)
    endif

    ! Check stacks are in proper state (no stack leaks)
    if(pm_debug_checks) then
       if(vbase_check/=coder%vtop) then
          if(sig>0) write(*,*) 'in',sym_names(sig)
          write(*,*) 'MISMATCH-vstack',coder%vtop,vbase_check
          call pm_panic('prc_call')
       endif
       if(tbase_check/=coder%wtop) then
          if(sig>0) write(*,*) 'in',sym_names(sig)
          write(*,*) 'MISMATCH-wstack',coder%wtop,tbase_check
          call pm_panic('prc_call')
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

    !===================================================================
    ! Push argument types with modes for all arguments
    !==================================================================
    subroutine get_arg_types_and_modes
      integer:: i,j
      type(pm_ptr):: v
      if(coder%wtop+nargs+2>max_code_stack) then
         call pm_panic('Program too complex')
      endif
      coder%wstack(coder%wtop+1)=pm_typ_is_tuple
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
      integer:: slot
      slot=get_slot_or_type(m)
      if(slot<0) then
         tno=-slot
      else
         tno=coder%stack(slot)
         if(pm_debug_checks) then
            if(tno==undefined) then
               call qdump_code_tree(coder,pm_null_obj,6,&
                    cnode_arg(args,m),2)
               call infer_error_with_trace(coder,args,'Internal Compiler Error: Broken type resulution::')
               !!call pm_panic('Broken type resolution chain')
            endif
         endif
      endif
    end function arg_type_with_mode


    !===================================================================
    ! Return the type for argument m (errors are checked)
    !==================================================================
    function arg_type(m) result(tno)
      integer,intent(in):: m
      integer:: tno
      integer:: mode
      tno=pm_typ_strip_mode(coder%context,arg_type_with_mode(m),mode)
    end function arg_type

    
    !===================================================================
    ! Return the type and mode for arguement m (no error check)
    !==================================================================
    function arg_type_noerr(m) result(tno)
      integer,intent(in):: m
      integer:: tno
      integer:: slot
      slot=get_slot_or_type(m)
      if(slot<0) then
         tno=-slot
      else
         tno=coder%stack(slot)
      endif
    end function arg_type_noerr

    !===================================================================
    ! Return the slot for arguement m (or -ve of typeno for a constant)
    !==================================================================
    function get_slot_or_type(m) result(slotno)
      integer,intent(in):: m
      integer:: slotno
      type(pm_ptr):: v
      v=cnode_arg(args,m)

      if(cnode_get_kind(v)==cnode_is_const) then
         slotno=-cnode_num_arg(v,2)
      else
         slotno=cnode_get_num(v,var_index)+base
      endif
    end function get_slot_or_type

    !===================================================================
    ! Return the slot and type for arguement m
    ! - slot will be -ve for a constant
    !==================================================================
    subroutine get_slot_and_type(m,slot,tno)
      integer,intent(in):: m
      integer,intent(out):: slot
      integer,intent(out):: tno
      slot=get_slot_or_type(m)
      if(slot<0) then
         tno=-slot
      else
         tno=coder%stack(slot)
         if(pm_debug_checks) then
            if(tno==undefined) then
               call infer_error_with_trace(coder,args,'Internal Compiler Error: Broken type resulution::')
            endif
         endif
      endif
    end subroutine get_slot_and_type

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
      slotno=cnode_get_num(v,var_index)+base
    end function get_slot

    !==================================================================
    ! Check if argument m has logical type (bool in PM)
    !==================================================================
    subroutine check_logical(m)
      integer,intent(in):: m
      integer:: slt
      integer:: ty
      integer:: i
      type(pm_ptr):: tv
      integer:: tno
      tno=arg_type(m)
      if(tno/=error_type) then
         if(tno/=pm_logical.and.&
              tno/=coder%true_name.and.tno/=coder%false_name) then
            call infer_error_with_trace(coder,callnode,&
                 'Expecting boolean expression, got: '//&
                 trim(pm_typ_as_string(coder%context,tno)))
         endif
      endif
    end subroutine check_logical

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
            call infer_error_with_trace(coder,callnode,&
                 'Expecting long expression, got: '//&
                 trim(pm_typ_as_string(coder%context,tno)))
         endif
      endif
    end subroutine check_long
    
    subroutine clear_cblock_mark(list)
      type(pm_ptr),intent(in):: list
      integer:: slot
      slot=base+cnode_get_num(list,cblock_index)
      coder%stack(slot)=0
    end subroutine clear_cblock_mark

    function cblock_marked(list) result(marked)
      type(pm_ptr),intent(in):: list
      logical:: marked
      integer:: slot
      slot=base+cnode_get_num(list,cblock_index)
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
      tkind=pm_typ_kind(coder%context,tno)
      call set_call_sig(&
           merge(1,0,tkind/=pm_typ_is_dref.and.tkind/=pm_typ_is_vect))
    end subroutine flag_import_export

    !==================================================================
    ! Set the signature of the current call to k
    !==================================================================
    subroutine set_call_sig(k)
      integer,intent(in):: k
      coder%stack(base+cnode_get_num(callnode,call_index))=k
    end subroutine set_call_sig

    !==================================================================
    ! Resolve signature for item.name
    ! This is either              2..  for regular structures/records
    ! or    pm_typ_dref_offset + 2 ..  for offset into a dref
    ! (starting at 2 is for historical reasons and is horrible)
    !==================================================================
    recursive function resolve_elem(tno,name,isref,isopt,elem_type) result(sig)
      integer,intent(in):: tno,name
      logical,intent(in):: isref,isopt
 
      integer,intent(out):: elem_type
      integer:: sig
      integer:: base,key(2)
      type(pm_ptr):: svec

      base=coder%wtop
      sig=pm_typ_find_elem(coder%context,tno,name,isref,&
           coder%wstack,coder%wtop,max_code_stack,elem_type,einfo)
      if(sig<0) then
         key(1)=-name
         key(2)=tno
         sig=-pm_ivect_lookup(coder%context,coder%proc_cache,key,2)
         if(sig==0) then
            svec=pm_fast_newnc(coder%context,pm_int,&
                 coder%wtop-base+1)
            svec%data%i(svec%offset)=elem_type
            svec%data%i(svec%offset+1:svec%offset+pm_fast_esize(svec))=&
                 coder%wstack(base+1:coder%wtop)
            sig=-pm_idict_add(coder%context,coder%proc_cache,&
              key,2,svec)
         endif
      elseif(sig==0) then
         if(.not.isopt) then
            call infer_error_with_trace(coder,callnode,&
                 'Error accessing element "'//&
                 trim(pm_name_as_string(coder%context,name))//&
                 '" of type "'//&
                 trim(pm_typ_as_string(coder%context,tno))//'"')
            call pm_typ_error(coder%context,einfo)
         endif
         elem_type=error_type
      endif
      coder%wtop=base
    end function resolve_elem

    !==================================================================
    ! Set argument m to have error type
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
    subroutine combine_types(vararg,typ)
      type(pm_ptr),intent(in)::vararg
      integer,intent(in):: typ
      integer:: slot
      integer:: typ0,n
      type(pm_ptr):: tv,p,q,var
      if(typ==undefined) call pm_panic('combine types')
      var=vararg
      if(pm_debug_level>3) then
         write(*,*) 'COMBINE TYPES> ',&
              trim(pm_name_as_string(coder%context,cnode_get_name(var,var_name)))
      endif
      slot=base+cnode_get_num(var,var_index)
      typ0=coder%stack(slot)
      if(typ0<0) then
         coder%stack(slot)=typ
      elseif(typ/=typ0.and.typ/=error_type.and.typ0/=error_type.and.&
           coder%num_errors==0) then
         call cnode_error(coder,var,'Variable changed type:',&
              cnode_get(var,var_name))
         call more_error(coder%context,&
            trim(pm_typ_as_string(coder%context,typ))//' <> '//&
            trim(pm_typ_as_string(coder%context,typ0)))
      endif
      
    end subroutine combine_types
    
  end subroutine prc_call

  !==================================================================
  ! Conventional procedure call
  !==================================================================
  subroutine prc_proc_call(coder,cblock,callnode,sig,args,num_args,nret,base)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,cblock,args
    integer,intent(in):: sig,num_args,nret,base
    logical:: is_comm,is_complete,is_cond,is_unlabelled,run_shared,ignore_rules
    integer:: name,mode,mode2,expected_mode,i,j,tno,tno2,slot,flags
    integer:: nargs,nkey,nextra,ressig,pdepth
    logical:: undef_arg
    type(pm_ptr):: arg,amps,proclist,t,tv
    nargs=num_args
    if(debug_inference) then
       write(*,*) 'PROCESS PROC CALL>',&
            trim(sig_name_str(coder,int(sig))),'@',&
            callnode%data%ptr(callnode%offset+cnode_lineno)%offset
       if(cnode_get_kind(args)/=cnode_is_arglist) call pm_panic('not arglist')
       call qdump_code_tree(coder,pm_null_obj,6,callnode,2)
    endif
    flags=cnode_get_num(callnode,call_flags)
    is_comm=iand(flags,call_is_comm)/=0
    is_complete=iand(flags,proc_run_complete)/=0
    is_cond=iand(flags,call_is_cond+proc_run_complete+proc_run_shared)==call_is_cond
    is_unlabelled=iand(flags,call_is_unlabelled)/=0
    ignore_rules=cnode_flags_set(callnode,call_flags,call_ignore_rules)
    run_shared=iand(flags,proc_run_shared)/=0
    proclist=pm_dict_val(coder%context,coder%sig_cache,int(sig,pm_ln))
    nkey=cnode_get_num(callnode,call_nkeys)
    nextra=0
    call push_word(coder,pm_typ_is_tuple)
    call push_word(coder,0)
    call check_wstack(coder,nargs)
    undef_arg=.false.
    do i=1,nargs
       tno=arg_type_with_mode(i+nret)
       coder%wstack(coder%wtop+i)=tno
       undef_arg=undef_arg.or.tno<=0
    enddo
    if(is_comm) then
       pdepth=cnode_get_num(cnode_arg(args,1+nret),var_par_depth)
       ! Modify modes of arguments if necessary
       do i=3,nargs
          arg=cnode_arg(args,i+nret)
          ! Set arguments from outside this context to shared
          if(cnode_get_kind(arg)==cnode_is_var) then
             if(cnode_get_num(arg,var_par_depth)==pdepth) then
                coder%wstack(coder%wtop+i)=&
                     pm_typ_replace_mode(coder%context,&
                     coder%wstack(coder%wtop+i),&
                     sym_shared,.false.)
             endif
          endif
          ! In an unlabelled conditional context, set coherent to partial
          if(is_unlabelled.and.&
               pm_typ_get_mode(coder%context,coder%wstack(coder%wtop+i))==sym_coherent) then
             coder%wstack(coder%wtop+i)=&
                  pm_typ_replace_mode(coder%context,&
                  coder%wstack(coder%wtop+i),&
                  sym_partial,.false.)
          endif
       enddo
    elseif(run_shared) then
       do i=1,nargs
          if(cnode_get_kind(cnode_arg(args,i+nret))==cnode_is_var) then
             if(.not.cnode_flags_set(cnode_arg(args,i+nret),var_flags,var_is_imported)) then
                coder%wstack(coder%wtop+i)=&
                     pm_typ_replace_mode(coder%context,&
                     coder%wstack(coder%wtop+i),&
                     sym_shared,.false.)
             endif
          endif
       enddo
    endif
    if(undef_arg) then
       do i=1,nret
          call set_arg_to_error_type(i)
       enddo
       coder%wtop=coder%wtop-2
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
                  trim(pm_typ_as_string(coder%context,coder%wstack(coder%wtop+i)))
          enddo
       endif
       
       ! Suspend 'no shared import' rule in system module code
       ignore_rules=ignore_rules.or.cnode_get_name(callnode,cnode_modl_name)==sym_pm_system
       
       ! Implement mode combination rule for standard procedures
       mode=pm_typ_combine_modes(coder%context,&
            coder%wstack(coder%wtop+1:coder%wtop+nargs),&
            ignore_rules.or.run_shared,is_complete,is_cond,is_unlabelled)
       if(mode<0) then
          if(mode>-1000) then
             call call_error('Cannot pass a shared distributed value to a standard procedure')
             call infer_error_with_trace(coder,cnode_arg(args,nret-mode),&
                  'Cannot pass a shared distributed value to a standard procedure')
          elseif(mode>-2000) then
             call infer_error_with_trace(coder,cnode_arg(args,nret-mode-1000),&
                  'Cannot pass a "partial" value to a "complete" procedure'//&
                  trim(sig_name_str(coder,int(sig))))
          elseif(.not.ignore_rules) then
             call infer_error_with_trace(coder,cnode_arg(args,nret-mode-2000),&
                  'Cannot pass a "coherent" value to a "complete" procedure in an unlabelled call in a conditional context: '//&
                  trim(sig_name_str(coder,int(sig))))
          endif
          mode=sym_coherent
       endif
       
       ! Argument mode rules for specialised run modes
       if(.not.ignore_rules) then
          if(is_complete.and.mode==sym_partial) then
             call call_error(&
                  'Cannot pass "partial" value to a procedure with "<<complete>>" attribute')
          elseif(run_shared.and.mode<sym_mirrored) then
             call call_error(&
                  'Cannot pass "'//trim(sym_names(mode))//&
                  '" value to a procedure with "<<shared>>" attribute')
          endif
       endif
       
       ! Rules for "&" arguments
       !!! -- Need better error positioning
       amps=cnode_get(callnode,call_amp)
       if(.not.pm_fast_isnull(amps).and..not.ignore_rules) then
          amps=pm_name_val(coder%context,int(amps%offset))
          do i=0,pm_fast_esize(amps)
             tno2=pm_typ_strip_mode(coder%context,&
                  coder%wstack(coder%wtop+amps%data%i(amps%offset+i)+nkey),mode2)
             if(tno2>0) then
                tv=pm_typ_vect(coder%context,tno2)
                if(pm_tv_kind(tv)==pm_typ_is_dref) then
                   do while(pm_tv_name(tv)>0)
                      tno2=pm_tv_arg(tv,2)
                      tv=pm_typ_vect(coder%context,tno2)
                   enddo
                   if(pm_tv_kind(tv)==pm_typ_is_dref.and.&
                        pm_tv_name(tv)/=pm_dref_is_ref) then
                      call call_error(&
                           'Cannot pass a mixed-mode reference as an "&" argument - must use "&&"')
                      coder%wstack(coder%wtop+amps%data%i(amps%offset+i)+nkey)=pm_tv_arg(tv,1)
                   endif
                endif
             endif
             if(is_complete) then
                if(mode2/=sym_coherent.and.mode2/=sym_chan) then
                   call call_error('Cannot have "'//trim(sym_names(mode2))//&
                        '" "&" parameter alongside "<<complete>>" call attribute')
                endif
             elseif(run_shared) then
                if(mode2/=sym_shared) then
                   call call_error('Cannot have "'//trim(sym_names(mode2))//&
                        '" "&" parameter alongside "<<shared>>" call attribute')
                endif
             elseif(mode2/=sym_partial.and.mode2/=sym_coherent.and.(mode2/=sym_chan.or.is_unlabelled)) then
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
               pm_typ_strip_mode(coder%context,coder%wstack(coder%wtop+i),mode2)
       enddo
    endif
    
    ! Move stack top to end of args (args were above stack top)
    coder%wtop=coder%wtop+nargs
    
    ! Deal with arg...
    if(cnode_flags_set(callnode,call_flags,call_is_vararg)) then
       if(top_word(coder)>0) then
          t=pm_typ_vect(coder%context,top_word(coder))
          if(pm_tv_kind(t)==pm_typ_is_tuple) then
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
    if(cnode_flags_set(proclist,cnode_args+1,proc_is_var)) then
       ressig=var_call(proclist,&
            pm_dict_key(coder%context,coder%sig_cache,int(sig,pm_ln)))
    else   
       ressig=simple_proc_call(sig,proclist)
       if(debug_inference) write(*,*) 'RESSIG>',ressig,coder%incomplete,&
            'for', trim(sig_name_str(coder,int(sig)))
    endif
    
    ! Standard procedure return modes
    if(.not.is_comm) then
!!$       ! If procedure just called is tainted 'variant' set return modes to private
!!$       if(iand(coder%proc_taints,proc_is_variant)/=0) then
!!$          mode=merge(sym_partial,sym_complete,is_cond)
!!$       endif
       
       ! Apply return mode to returned values
       if(mode/=sym_coherent) then
          do j=1,nret
             coder%stack(get_slot(j))=pm_typ_replace_mode(coder%context,&
                  coder%stack(get_slot(j)),mode,is_cond)
          enddo
       endif
    endif
    
    ! Tidy up
    if(debug_inference) then
       do j=1,nret
          write(*,*) 'RETURN',j,&
               trim(pm_typ_as_string(coder%context,coder%stack(get_slot(j))))
       enddo
    endif
    coder%wtop=coder%wtop-nargs-2
    slot=base+cnode_get_num(callnode,call_index)
    coder%stack(slot)=ressig
    if(debug_inference) then
       write(*,*) 'END PROC CALL>',&
            trim(sig_name_str(coder,int(sig))),coder%stack(4),coder%vtop
    endif
    
  contains

    include 'fesize.inc'
    include 'fisnull.inc'
    include 'ftiny.inc'

    !===================================================================
    ! Return type, without mode, for argument m
    !==================================================================
    function arg_type(m) result(tno)
      integer,intent(in):: m
      integer:: tno
      integer:: mode
      tno=pm_typ_strip_mode(coder%context,arg_type_with_mode(m),mode)
    end function arg_type

    !===================================================================
    ! Return type and mode for arguement m 
    !==================================================================
    function arg_type_with_mode(m) result(tno)
      integer,intent(in):: m
      integer:: tno
      integer:: slot
      slot=get_slot_or_type(m)
      if(slot<0) then
         tno=-slot
      else
         tno=coder%stack(slot)
         if(pm_debug_checks) then
            if(tno==undefined) then
               write(*,*) m,slot,pm_main_process
               call qdump_code_tree(coder,pm_null_obj,6,&
                    cnode_arg(args,m),2)
               call infer_error_with_trace(coder,args,'Broken::')
               !call pm_panic('Broken type resolution chain')
            endif
         endif
      endif
    end function arg_type_with_mode

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
      slotno=cnode_get_num(v,var_index)+base
    end function get_slot

    !===================================================================
    ! Return the slot for arguement n (or -ve of typeno for a constant)
    !==================================================================
    function get_slot_or_type(m) result(slotno)
      integer,intent(in):: m
      integer:: slotno
      type(pm_ptr):: v
      v=cnode_arg(args,m)

      if(cnode_get_kind(v)==cnode_is_const) then
         slotno=-cnode_num_arg(v,2)
      else
         slotno=cnode_get_num(v,var_index)+base
      endif
    end function get_slot_or_type

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
    subroutine combine_types(vararg,typ)
      type(pm_ptr),intent(in)::vararg
      integer,intent(in):: typ
      integer:: slot
      integer:: typ0,n
      type(pm_ptr):: tv,p,q,var
      if(typ==undefined) call pm_panic('combine types')
      var=vararg
      if(pm_debug_level>3) then
         write(*,*) 'COMBINE TYPES> ',&
              trim(pm_name_as_string(coder%context,cnode_get_name(var,var_name)))
      endif
      slot=base+cnode_get_num(var,var_index)
      typ0=coder%stack(slot)
      if(typ0<0) then
         coder%stack(slot)=typ
      elseif(typ/=typ0.and.typ/=error_type.and.typ0/=error_type.and.&
           coder%num_errors==0) then
         call cnode_error(coder,var,'Variable changed type:',&
              cnode_get(var,var_name))
         call more_error(coder%context,&
            trim(pm_typ_as_string(coder%context,typ))//' <> '//&
            trim(pm_typ_as_string(coder%context,typ0)))
      endif
      
    end subroutine combine_types

    !=========================================
    ! Print error message for a call
    !=========================================
    subroutine call_error(str)
      character(len=*):: str
      call infer_error(coder,callnode,str)
      call print_call_details(coder,callnode,coder%wtop,nargs)
      call infer_trace(coder)
    end subroutine call_error

    !================================================
    ! Call with variable procedure name: v.(args)
    !================================================
    function var_call(prlist,callsig) result(k)
      type(pm_ptr),intent(in):: prlist,callsig
      integer:: k
      integer:: i,sig,rsig
      type(pm_ptr):: pr,var,tv,tv2
      integer:: tno,tno2,name
      logical:: err
      ! Get value for procedure name (actually its type)
      var=cnode_get(callnode,call_var)
      if(cnode_get_kind(var)==cnode_is_var) then
         tno=coder%stack(cnode_get_num(var,var_index)+base)
      else
         tno=cnode_num_arg(var,2)
      endif
      if(tno==error_type) then
         do i=1,nret
            call set_arg_to_error_type(i)
         enddo
         return
      endif
      tv=pm_typ_vect(coder%context,tno)
      if(pm_tv_kind(tv)==pm_typ_is_par_kind) then
         tno=pm_tv_arg(tv,1)
         tv=pm_typ_vect(coder%context,tno)
      endif
      coder%wstack(coder%wtop-nargs)=tno
      if(pm_tv_kind(tv)/=pm_typ_is_proc) then
         call infer_error_with_trace(coder,callnode,&
              'Value does not hold proc name; got: '//&
              trim(pm_typ_as_string(coder%context,tno)))
         do i=1,nret
            call set_arg_to_error_type(i)
         enddo
         k=undefined
         return
      endif
      name=abs(pm_tv_name(tv))

      ! Now look for a signature with this name and process it
      rsig=undefined
      do i=5,cnode_numargs(prlist),2
         if(name==cnode_num_arg(prlist,i)) then
            sig=cnode_num_arg(prlist,i+1)
            pr=pm_dict_val(coder%context,coder%sig_cache,int(sig,pm_ln))
            if(pm_tv_name(tv)>=0) then
               rsig=simple_proc_call(sig,pr,issig=.true.)
            else
               tno2=pm_tv_arg(tv,1)
               tv2=pm_typ_vect(coder%context,tno2)
               rsig=simple_proc_call(sig,pr,sigpars=pm_tv_arg(tv2,1),&
                    sigtyp=tno,issig=.true.)
               if(rsig>0) call check_call_against_sig(tno,tv,callsig)
            endif
            k=rsig
            return
         endif
      enddo
      call infer_error_with_trace(coder,callnode,&
           'No match found for ".()" call using procedure name: '//&
           trim(pm_name_as_string(coder%context,name)))
      k=undefined
    end function  var_call

    !=======================================================
    ! If a call is v.(args) with v of a signature type
    ! then it is necessary to check the call against
    ! the signature
    !=======================================================
    subroutine check_call_against_sig(tno,tvp,callsig)
      integer,intent(in):: tno
      type(pm_ptr),intent(in):: tvp,callsig
      integer:: tno2,i,k,tno3
      type(pm_ptr):: tv,tv2,tv3,amplocs
      type(pm_typ_einfo):: einfo
      integer:: nret,flags,n,mode,argmode

      tv=pm_typ_vect(coder%context,pm_tv_arg(tvp,1))
      
      ! Get information on call
      i=callsig%offset+pm_fast_esize(callsig)
      nret=callsig%data%i(i-1)
      flags=callsig%data%i(i-3)
      
      ! Check type of call
      name=pm_tv_name(tv)
      if(iand(flags,call_is_comm)/=0) then
         if(name/=sym_pct) then
            call infer_error(coder,callnode,&
                 'Call does not match procedure type: '//&
                 pm_typ_as_string(coder%context,tno))
            call more_error(coder%context,&
                 'Expecting communicating "%" procedure')
            goto 10
         endif
      elseif(name/=sym_proc) then
         call infer_error(coder,callnode,&
              'Call does not match procedure type: '//&
              pm_typ_as_string(coder%context,tno))
         call more_error(coder%context,&
              'Not expecting communicating "%" procedure')
         goto 10
      endif

      ! Check returns
      tno2=pm_tv_arg(tv,2)
      tv2=pm_typ_vect(coder%context,tno2)
      n=pm_tv_numargs(tv2)
      if(nret/=n) then
         call infer_error(coder,callnode,&
              'Call does not match procedure type: '//&
              pm_typ_as_string(coder%context,tno))
         call more_error(coder%context,'Different number of return values')
         goto 10
      endif
      do i=1,n
         if(.not.pm_typ_includes(coder%context,pm_tv_arg(tv2,i),&
              arg_type(i),pm_typ_incl_val,einfo)) then

            !!!! Check conversion to interface/proc_sig
            
            call infer_error(coder,callnode,&
                 'Call does not match procedure type: '//&
                 pm_typ_as_string(coder%context,tno))
            call more_error(coder%context,&
                 'Return type mismatch: '//&
                 trim(pm_typ_as_string(coder%context,pm_tv_arg(tv2,i)))//&
                 ' vs: '//&
                 trim(pm_typ_as_string(coder%context,arg_type(i))))
         endif
      enddo

      return
10    continue
     
      call infer_error(coder,callnode,&
           'Call does not match procedure type: '//&
           pm_typ_as_string(coder%context,tno))
      call infer_trace(coder)
      
    end subroutine check_call_against_sig

    !========================================================================
    ! Procedure call for which signature has been resolved
    ! (either simple in the first place or an option for a vcall)
    ! - If err is present then no error messages - set err to true instead
    ! - If sigpars/sigtyp present then introduce a signature type into the
    !   procedure matching process
    ! - If issig is present then disable visibility rule (for "." call)
    !========================================================================
    function simple_proc_call(sig,procs,err,sigpars,sigtyp,issig) result(k)
      integer,intent(in):: sig
      type(pm_ptr),intent(in):: procs
      logical,intent(out),optional:: err
      integer,intent(in),optional:: sigpars,sigtyp
      logical,intent(in),optional:: issig
      integer:: k
      
      integer:: h,i,j,m,start,slot,pcheck,nkey_sig,jpass
      integer:: vbase,wbase
      type(pm_ptr):: v,proc,rtvect
      integer:: rt,rt2,pars,mpars,apars,tno,match_pars
      logical:: ok,found,visible,found_has_no_rtypes
      integer:: save_proc_key_base,save_par_kind,save_par_kind2
      type(pm_ptr):: keynames
      type(pm_typ_einfo):: einfo
      integer,dimension(1):: key
      integer:: memo

      ! Save some state information
      save_proc_key_base=coder%proc_key_base
      coder%proc_key_base=coder%wtop-nargs
      save_par_kind=coder%par_kind
      save_par_kind2=coder%par_kind2
      
      if(present(err)) err=.false.
      start=coder%vtop
      keynames=cnode_arg(procs,1)
      if(pm_fast_isnull(keynames)) then
         nkey_sig=0
      else
         nkey_sig=pm_set_size(coder%context,keynames)
      endif

      ! For shared calls, step back par kinds
      if(cnode_flags_set(callnode,call_flags,proc_run_shared)) then
         coder%par_kind=coder%par_kind2
      endif

      ! Find matching signature
      ! This is done in multiple passes with broader matching allowed in pass 2
      if(pm_debug_level>4) write(*,*) 'Checking',cnode_numargs(procs),' sigs'
      found=.false.
      ! For procedure signature "." call then don't check visibility
      visible=present(issig)
      do jpass=0,3
         if(debug_inference) write(*,*) 'MATCH PASS> ',jpass
         do i=3,cnode_numargs(procs),2
            pars=cnode_num_arg(procs,i)

            ! If this call is for a proc signature, then restrict matching to that signature
            if(present(sigpars)) then
               if(pm_typ_includes(coder%context,&
                    pars,sigpars,pm_typ_incl_typ,einfo)) then
                  mpars=sigpars
               elseif(pm_typ_includes(coder%context,&
                    sigpars,pars,pm_typ_incl_typ,einfo)) then
                  mpars=pars
               else
                  cycle
               endif
            else
               mpars=pars
            endif
            
            if(debug_inference) then
               write(*,*) 'CHECKING SIG',(i-1)/2,&
                    ' OF ',(cnode_numargs(procs)-2)/2,&
                    ' FOR>',trim(sig_name_str(coder,int(sig)))
               write(*,*) '>> ',trim(pm_typ_as_string(coder%context,pars))
            endif

            wbase=coder%wtop
            vbase=coder%vtop
            apars=check_call_sig(coder,callnode,cnode_arg(procs,i+1),&
                 mpars,nargs-nkey,nextra,jpass)
            if(apars>=0) then

               ! Check for a visible match
               if(is_visible(coder,callnode,cnode_arg(procs,i+1))) visible=.true.

               ! If this is a second (or later) match, then check for compatibility
               if(found) then
                  if(pm_typ_includes(coder%context,pars,&
                       match_pars,pm_typ_incl_typ,einfo)) then
                     coder%wtop=wbase
                     coder%vtop=vbase
                     ! Have to also check compatibility of return types
                     ! in the case where the enclosing procedure defines return types
                     ! and the first-match procedure does not define them
                     if(nret>0.and.rt>0.and.found_has_no_rtypes) then
                        rt2=abs(cnode_get_num(cnode_arg(procs,i+1),pr_rtype))
                        if(pm_typ_kind(coder%context,rt2)/=pm_typ_is_undef_result) then
                           if(.not.pm_typ_includes(coder%context,rt2,rt,pm_typ_incl_typ,einfo)) then
                              call infer_error(coder,proc,&
                                   'Procedure returns type(s) not compatible'//&
                                   ' with an enclosing procedure to which it conforms')
                              call pm_typ_error(coder%context,einfo)
                              call infer_error(coder,cnode_arg(procs,i+1),&
                                   'Enclosing procedure referenced in above error')
                              call more_error(coder%context,' ')
                              call print_call_details(coder,callnode,coder%proc_key_base,nargs)
                              call infer_trace(coder)
                           endif
                        endif
                     endif
                     cycle
                  else
                     if(.not.present(err)) then
                        call infer_error(coder,callnode,&
                             'Ambiguous call to: '//trim(sig_name_str(coder,int(sig))))
                        call print_call_details(coder,callnode,coder%proc_key_base,nargs)
                        call print_proc_details(coder,cnode_arg(procs,i+1),&
                             sig,&
                             cnode_flags_set(callnode,call_flags,call_is_comm),&
                             pars)
                        call print_proc_details(coder,proc,&
                             sig,&
                             cnode_flags_set(callnode,call_flags,call_is_comm),&
                             match_pars)
                     else
                        err=.true.
                     endif
                     coder%wtop=wbase
                     exit
                  endif
               endif


               ! A good match has been found
               ! infer the associated procedure
               found=.true.
               match_pars=pars
               proc=cnode_arg(procs,i+1)
               found_has_no_rtypes=&
                    pm_typ_kind(coder%context,cnode_get_num(proc,pr_rtype))==&
                    pm_typ_is_undef_result
               if(cnode_get_kind(proc)==cnode_is_builtin) then
                  rt=prc_builtin(coder,proc,apars,pars,base)
               else
                  
                  pcheck=coder%vtop
                  
                  ! Misuse loop stack as a traceback record 
                  ! of calls being processed
                  coder%par_depth=coder%par_depth+1
                  if(coder%par_depth<max_par_depth) then
                     coder%imports(coder%par_depth)=callnode
                     coder%import_cblock(coder%par_depth)=&
                          pm_fast_tinyint(coder%context,coder%proc_key_base)
                  endif
                  rt=prc_proc(coder,proc,callnode,apars,pars,nret,nkey,keynames,base)
                  if(cnode_get_name(callnode,cnode_modl_name)/=sym_pm_system) then
                     coder%supress_errors=.false.
                  endif
                  coder%par_depth=coder%par_depth-1
                  if(rt<0) then
                     if(debug_inference) then
                        write(*,*) 'INCOMPLETE PROC>',coder%vtop,start,coder%incomplete
                     endif
                     do j=1,nret
                        call set_arg_to_error_type(j)
                     enddo
                     coder%vtop=start
                     coder%proc_key_base=save_proc_key_base
                     coder%wtop=wbase
                     k=1234567
                     return
                  else
                     if(coder%vtop/=pcheck+1) call pm_panic('pcheck mismatch')
                  endif
               endif               
               
               if(nret>0) then
                  if(rt>0) then
                     rtvect=pm_typ_vect(coder%context,rt)
                     if(pm_tv_kind(rtvect)==pm_typ_is_tuple) then
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
            else
               ! Not this one - keep looking
               coder%vtop=vbase
               if(debug_inference) write(*,*) 'REJECTED>'
            endif
            coder%wtop=wbase
            if(debug_inference) write(*,*) 'CHECKED SIG'
         enddo
         if(found) exit
      enddo
      
      if(debug_inference) then
         write(*,*) 'ALL SIGS CHECKED>',trim(sig_name_str(coder,int(sig)))
      endif

      if(.not.found.or..not.visible) then
         ! If nothing found print error message
         ! or return error flag
         if(.not.present(err)) then
            if(.not.found) then
               call cnode_error(coder,callnode,&
                    'No matching procedure:')
            else
               call cnode_error(coder,callnode,&
                    'No matching procedure visible to module containing call')
            endif
            m=coder%wtop
            call make_type(coder,nargs+2)
            call print_call_details(coder,callnode,coder%proc_key_base,nargs)
            coder%wtop=m
            if(present(sigtyp)) then
               call more_error(coder%context,'Matching: '//&
                    trim(pm_typ_as_string(coder%context,sigtyp)))
            else
               call more_error(coder%context,'Procedures considered:')
               do m=3,cnode_numargs(procs),2
                  pars=cnode_num_arg(procs,m)
                  call print_proc_details(coder,cnode_arg(procs,m+1),&
                       sig,&
                       cnode_flags_set(callnode,call_flags,call_is_comm),&
                       pars)
                  if(m>pm_opts%proc_list.and..not.pm_opts%see_all_procs) then
                     call more_error(coder%context,&
                          '... (to see all procedures use -fsee-all-procs)')
                     exit
                  endif
               enddo
            endif
            call infer_trace(coder)
            do i=1,nret
               call set_arg_to_error_type(i)
            enddo
            k=undefined
         else
            err=.true.
         endif
      else
         ! Otherwise create resolved procedure cnode
         if(coder%vtop>start+1) then
            call make_code(coder,pm_null_obj,cnode_is_autoconv_sig,&
                 coder%vtop-start)
            key(1)=pm_dict_size(coder%context,coder%proc_cache)
            k=pm_idict_add(coder%context,coder%proc_cache,key,1,top_code(coder))
         else
            k=coder%vstack(coder%vtop)%offset
         endif
      endif

      ! Tidy up
      coder%vtop=start
      coder%proc_key_base=save_proc_key_base
      coder%par_kind=save_par_kind
      coder%par_kind2=save_par_kind2
      
    end function  simple_proc_call

 
  end subroutine prc_proc_call


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
  
  ! ================================================================================
  ! Set up type inference frame
  ! Three control slots:
  !  base-2   == taints for current procedure
  !  base-1   == break value -- flags changing types, resolution not complete if /= 0
  !  base     == argument (on entry) return (on exit) types
  ! Remaining slots:
  !  base+index == resolution information according to var or call index
  ! =================================================================================
  function create_stack_frame(coder,argtype,max_index,init_taints) result(base)
    type(code_state),intent(inout):: coder
    integer,intent(in):: argtype,max_index,init_taints
    integer:: base
    base=coder%top+3
    coder%top=base+max_index
    if(coder%top>max_code_stack) &
         call pm_panic('Program too complex (nested calls)')
    call init_stack_frame(coder,base,argtype,init_taints)
  end function  create_stack_frame

  !===============================================================
  ! (Re)initialise current stack frame
  !===============================================================
  subroutine init_stack_frame(coder,base,argtype,init_taints) 
    type(code_state),intent(inout):: coder
    integer,intent(in):: base,argtype,init_taints
    integer:: i
    coder%stack(base-2)=init_taints
    coder%stack(base-1)=0
    coder%stack(base)=argtype
    do i=base+1,coder%top
       coder%stack(i)=undefined
    enddo
  end subroutine init_stack_frame

  !===============================================================
  ! Pop off current stack frame
  !===============================================================
  subroutine pop_stack_frame(coder,base)
    type(code_state),intent(inout):: coder
    integer,intent(in):: base
    coder%top=base-3
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
    type(pm_typ_einfo):: einfo
    typ3=-1
    tv1=pm_typ_vect(coder%context,typ1)
    tv2=pm_typ_vect(coder%context,typ2)
    if(pm_tv_kind(tv1)==pm_typ_is_poly) then
       if(pm_tv_kind(tv2)==pm_typ_is_poly) then
          if(conv_poly.and.pm_typ_includes(coder%context,&
               pm_tv_arg(tv1,1),pm_tv_arg(tv2,1),&
               pm_typ_incl_typ,einfo)) then
             if(add_poly_to_poly(coder,typ1,typ2)) then
                coder%types_finished=.false.
             endif
             typ3=typ1
          endif
       else
         if(pm_typ_includes(coder%context,&
               pm_tv_arg(tv1,1),typ2,&
               pm_typ_incl_typ,einfo)) then
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

  !====================================================
  ! Find procedure matching a given call signature
  ! Call argument types must be on wstack
  !====================================================
  function check_call_sig(coder,callnode,matchnode,pars,nargs,ignore,ipass) result(tno)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,matchnode
    integer,intent(in):: pars
    integer,intent(in):: nargs,ignore,ipass
    integer:: tno
    integer:: at,at2,pt,pt2,slot
    type(pm_ptr):: pv,amb,av,vec
    integer:: i,rel,n,base,wbase,pk,pk2,dbase,status
    logical:: ok
    type(pm_typ_einfo):: einfo
    if(pars==error_type) then
       tno=undefined
       return
    endif
    pv=pm_typ_vect(coder%context,pars)
    pk=pm_tv_kind(pv)

    if(pm_debug_checks) then
       if(pk/=pm_typ_is_tuple.and.&
            pk/=pm_typ_is_vtuple) &
            call pm_panic('check-sig')
    endif
 
    if(debug_inference) then
       write(*,*) 'Check call sig: <ignore=',ignore,'> ('
       write(*,*) pars,' ',trim(pm_typ_as_string(coder%context,pars))
       write(*,*) '----'
       do i=1,nargs
          at=coder%wstack(coder%wtop-nargs+i)
          write(*,*) at,' ',trim(pm_typ_as_string(coder%context,at))
       enddo
       write(*,*) ')'
    endif
    
    wbase=coder%wtop
    if(coder%wtop+nargs+2>max_code_stack) then
       call pm_panic('Program too complex (check-sig)')
    endif

    ! Start building return type on wstack
    coder%wtop=coder%wtop+nargs+2
    coder%wstack(wbase+1)=pm_typ_is_tuple
    coder%wstack(wbase+2)=0
    n=pm_tv_numargs(pv)
    if(n+ignore>nargs) then
       tno=undefined
       return
    endif
    do i=1,nargs
       at=coder%wstack(wbase-nargs+i)
       if(i<=ignore) then
          coder%wstack(wbase+i+2)=at
          cycle
       endif
       if(at==undefined) call pm_panic('broken type resolution chain')
       if(at==error_type) then
          pt=0
          cycle
       endif
       if(i>n+ignore) then
          if(pk/=pm_typ_is_vtuple) then
             tno=undefined
             goto 10
          endif
       else
          pt=pm_tv_arg(pv,i-ignore)
       endif
       if(pm_typ_includes(coder%context,&
            pt,at,pm_typ_incl_val,einfo)) then
          coder%wstack(wbase+i+2)=at
          if(debug_inference) then
             write(*,*) 'Match',trim(pm_typ_as_string(coder%context,pt)),'<>',&
                  trim(pm_typ_as_string(coder%context,at))
          endif
       else
          if(einfo%kind==pm_typ_err_ambig) then
             call cnode_error(coder,matchnode,&
                  'Ambiguous match to proc definition ( match in multiple alternatives)')
             call cnode_error(coder,callnode,'... call being processed')
          elseif(ipass>=1) then
             pt2=pm_typ_strip_to_basic(coder%context,pt)
             at2=pm_typ_convert(coder%context,pt2,at,.false.)
             if(at2/=undefined) then
                coder%wstack(wbase+2+i)=at2
                goto 5
             endif
             if(ipass==1) then
                tno=undefined
                goto 10
             elseif(ipass>=2) then
                base=coder%wtop
                ! Push index value for autoconv signature
                call push_word(coder,nargs-i)
                ! Check indirect inclusion
                call pm_indirect_include(coder%context,pt,at,coder%wstack,max_code_stack,&
                     coder%wtop,einfo,at2,status)
                if(status/=pm_elem_not_found) then
                   if(status==pm_elem_clash) then
                      call ambiguous_match_error(coder,callnode,pt,at,at2)
                   endif
                   ! Match with indirect inclusion
                   ! Make autoconv object
                   call code_int_vec(coder,coder%wstack,base+1,coder%wtop)
                   coder%wtop=base
                   ! Correct parameter type to post-conversion value
                   coder%wstack(wbase+i+2)=at2
                else
                   if(ipass==3) then
                      ! On third pass check for poly conversions
                      at2=convert_poly(coder,pt2,at,.false.)
                      if(at2/=-1) then
                         call push_word(coder,at2)
                         call code_int_vec(coder,coder%wstack,base+1,coder%wtop)
                         ! Correct parameter type to post-conversion value
                         coder%wstack(wbase+i+2)=at2
                         goto 5
                      endif
                   endif
                   
                   ! No match found
                   if(debug_inference) then
                      write(*,*) 'Does not include',&
                           trim(pm_typ_as_string(coder%context,pt)),'<>',&
                           trim(pm_typ_as_string(coder%context,at))
                   endif
                   tno=undefined
                   goto 10
                endif
             endif
          else
             ! No match found (pass 1)
             if(debug_inference) then
                write(*,*) 'Does not include',&
                     trim(pm_typ_as_string(coder%context,pt)),'<>',&
                     trim(pm_typ_as_string(coder%context,at))
             endif
             tno=undefined
             goto 10
          endif
       endif
5      continue
    enddo
    
    ! Bundle arguments into a single type
    tno=pm_new_typ(coder%context,coder%wstack(wbase+1:&
         wbase+nargs+2))

    ! Error exit point
10  continue

    ! Tidy up
    coder%wtop=wbase
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'fesize.inc'
  end function check_call_sig

  ! ===============================================================
  ! Error message for ambiguous match
  ! (assumes wstack holds results from pm_indirect_include)
  ! ===============================================================
  subroutine ambiguous_match_error(coder,callnode,pt,at,at2)
    type(code_state):: coder
    type(pm_ptr):: callnode
    integer,intent(in):: pt,at,at2
    call infer_error(coder,callnode,'Ambiguous match to embedded value:')
    call typ_ambiguous_match_error(coder%context,pt,at,at2,coder%wstack,coder%wtop)
    call infer_trace(coder)
  end subroutine ambiguous_match_error

  !===========================================================
  ! Type constraint / Cast
  !===========================================================
  function prc_cast(coder,node,tno1,tno2,isvar) result(k)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: tno1
    integer,intent(inout):: tno2
    logical,intent(in):: isvar
    integer:: k
    logical:: ok
    integer:: tno1b,tno3,base,status,key(1)
    type(pm_typ_einfo):: einfo
    k=0
    if(tno1<0.or.tno2<=0) then
       return
    endif
    ok=pm_typ_includes(coder%context,tno1,tno2,pm_typ_incl_val,&
         einfo)
    if(.not.ok) then
       tno1b=pm_typ_strip_to_basic(coder%context,tno1)
       tno3=pm_typ_convert(coder%context,tno1b,tno2,.true.)
       if(tno3==undefined) then
          base=coder%wtop
          call pm_indirect_include(coder%context,tno1,tno2,&
               coder%wstack,max_code_stack,coder%wtop,&
               einfo,tno3,status)
          if(status/=pm_elem_not_found) then
             if(status==pm_elem_clash) then
                call ambiguous_match_error(coder,node,tno1,tno2,tno3)
                ok=.true. ! To supress error message
             else
                call code_int_vec(coder,coder%wstack,base+1,coder%wtop)
                call make_code(coder,pm_null_obj,cnode_is_any_sig,1)
                key(1)=pm_dict_size(coder%context,coder%proc_cache)
                k=pm_idict_add(coder%context,coder%proc_cache,&
                     key,1,pop_code(coder))
                tno2=tno3
                ok=.true.
             endif
          else
             tno3=convert_poly(coder,tno1b,tno2,.true.)
             if(tno3/=-1) then
                call push_word(coder,tno3)
                call code_int_vec(coder,coder%wstack,coder%wtop,coder%wtop)
                call make_code(coder,pm_null_obj,cnode_is_any_sig,1)
                key(1)=pm_dict_size(coder%context,coder%proc_cache)
                k=pm_idict_add(coder%context,coder%proc_cache,&
                     key,1,pop_code(coder))
                tno2=tno3
                ok=.true.
             endif
          endif
          coder%wtop=base
       else
          tno2=tno3
          ok=.true.
       endif
    endif
    if(.not.ok) then
       call infer_error(coder,node,&
            'Value cannot be cast to the given type')
       call pm_typ_error(coder%context,einfo)
       call infer_trace(coder)
    endif
  contains
    include 'fisnull.inc'
  end function prc_cast

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
    case(op_uminus_ln)
       a%data%ln(a%offset)=-b%data%ln(b%offset)
    case(op_add_ln)
       a%data%ln(a%offset)=b%data%ln(b%offset)+c%data%ln(c%offset)
    case(op_sub_ln)
       a%data%ln(a%offset)=b%data%ln(b%offset)-c%data%ln(c%offset)
    case(op_mult_ln)
       a%data%ln(a%offset)=b%data%ln(b%offset)*c%data%ln(c%offset)
    case(op_divide_ln)
       if(c%data%ln(c%offset)/=0) then
          a%data%ln(a%offset)=b%data%ln(b%offset)/c%data%ln(c%offset)
       else
          ok=.false.
          emess='division by zero'
       endif
    case(op_mod_ln)
       if(c%data%ln(c%offset)/=0) then
          a%data%ln(a%offset)=modulo(b%data%ln(b%offset),c%data%ln(c%offset))
       else
          ok=.false.
          emess='modulo zero'
       endif
    case(op_pow_ln)
       a%data%ln(a%offset)=b%data%ln(b%offset)**c%data%ln(c%offset)
    case(op_max_ln)
       a%data%ln(a%offset)=max(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_min_ln)
       a%data%ln(a%offset)=min(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_abs_ln)
       a%data%ln(a%offset)=abs(b%data%ln(b%offset))
    case(op_band_ln)
       a%data%ln(a%offset)=iand(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_bor_ln)
       a%data%ln(a%offset)=ior(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_bxor_ln)
       a%data%ln(a%offset)=ieor(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_bshift_ln)
       a%data%ln(a%offset)=ishft(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_bnot_ln)
       a%data%ln(a%offset)=not(b%data%ln(b%offset))
    case(op_pdiff_ln)
       a%data%ln(a%offset)=dim(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_sign_ln)
       a%data%ln(a%offset)=sign(b%data%ln(b%offset),c%data%ln(c%offset))
    case(op_modulo_ln)
        if(c%data%ln(c%offset)/=0) then
          a%data%ln(a%offset)=mod(b%data%ln(b%offset),c%data%ln(c%offset))
       else
          ok=.false.
          emess='modulo zero'
       endif
    end select
  end subroutine fold_value

  !===============================================
  ! Calculate logical operation on bool constants
  !===============================================
  subroutine fold_comparison(op,a,b,ok)
    integer,intent(in):: op
    type(pm_ptr),intent(in):: a,b
    logical,intent(out):: ok
    select case(op)
    case(op_gt_ln)
       ok=a%data%ln(a%offset)>b%data%ln(b%offset)
    case(op_ge_ln)
       ok=a%data%ln(a%offset)>=b%data%ln(b%offset)
    case(op_eq_ln)
       ok=a%data%ln(a%offset)>=b%data%ln(b%offset)
    case(op_ne_ln)
       ok=a%data%ln(a%offset)/=b%data%ln(b%offset)
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
  subroutine infer_error_with_trace(coder,node,message,name)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    character(len=*):: message
    type(pm_ptr),intent(in),optional:: name
    logical:: save_supress,current_supress
    current_supress=coder%supress_errors
    call infer_error(coder,node,message,name)
    save_supress=coder%supress_errors
    coder%supress_errors=current_supress
    call infer_trace(coder)
    coder%supress_errors=save_supress
  end subroutine infer_error_with_trace

  !=====================================
  ! Output error message
  !=====================================
  subroutine infer_error(coder,node,message,name)
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
          do i=coder%par_depth,1,-1
             modl_name=cnode_get_name(coder%imports(i),cnode_modl_name)
             if(modl_name/=sym_pm_system) then
                lineno=cnode_get_name(coder%imports(i),cnode_lineno)
                charno=cnode_get_name(coder%imports(i),cnode_charno)
                exit
             endif
          enddo
       endif
       call pm_error_header(coder%context,&
            modl_name,&
            lineno,&
            charno)
       if(present(name)) then
          call pm_name_string(coder%context,int(name%offset),str)
          str=trim(pm_opts%error)//trim(message)//' '//trim(str)
       else
          str=trim(pm_opts%error)//message
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
  end subroutine infer_error

  ! ============================================================
  ! Output trace of current call stack
  ! Calls stored in coder%imports(1:coder%par_depth)
  !  and coder%import_cblock(1:coder%par_depth)
  !  misused for this purpose
  ! Ignores internal calls within PM__system
  !  unless pm_opts%hide_sysmod is false
  ! =============================================================
  subroutine infer_trace(coder)
    type(code_state):: coder
    type(pm_ptr):: node,modname,tv
    integer:: k,top
    if(.not.pm_main_process) return
    if(coder%supress_errors) return
    if(coder%par_depth<1) return
    top=coder%par_depth
    if(pm_opts%hide_sysmod.and.top<max_par_depth) then
       node=coder%imports(top)
       if(hide(node)) then
          do while(top>1)
             top=top-1
             node=coder%imports(top)
             if(.not.hide(node)) then
!!$                call pm_error_header(coder%context,&
!!$                     cnode_get_name(node,cnode_modl_name),&
!!$                     cnode_get_name(node,cnode_lineno),&
!!$                     cnode_get_name(node,cnode_charno))
!!$                top=top-1
!!$                if(top<1) return
                exit
             endif
          enddo
       endif
    endif

    if(top==1.and.pm_opts%hide_sysmod) then
       if(hide(coder%imports(top))) return
    endif
    
    write(*,*)
    write(*,*) '-------------CALL TRACE---------------------------'
    do k=top,1,-1
       if(k>max_par_depth) then
          write(*,*) 'Procedure call: (call not recorded)'
          cycle
       endif
       node=coder%imports(k)
       if((.not.hide(node)).or.&
            (.not.pm_opts%hide_sysmod)) then
          call print_call_details(coder,node,&
               int(coder%import_cblock(k)%offset))
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
  end subroutine infer_trace
  
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
    integer:: n,k,nargs,nkeys
    integer::ampidx,signame,signamebase
    type(pm_ptr):: tv,key,val,amp,keyargs,name
    if(.not.pm_main_process) return
    if(coder%supress_errors) return
    nkeys=cnode_get_num(node,call_nkeys)
    nargs=cnode_numargs(cnode_get(node,call_args))-cnode_get_num(node,call_nret)
    if(present(numargs)) nargs=numargs
    k=0
    key=pm_dict_key(coder%context,&
         coder%sig_cache,int(cnode_get_num(node,call_sig),pm_ln))
    val=pm_dict_val(coder%context,&
         coder%sig_cache,int(cnode_get_num(node,call_sig),pm_ln))
    keyargs=cnode_arg(val,1)
    ampidx=key%data%i(key%offset+pm_fast_esize(key)-2)
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
       tv=pm_typ_vect(coder%context,coder%wstack(base))
       signame=abs(pm_tv_name(tv))
    elseif(signamebase==sym_assignment.or.signamebase==sym_assign_var) then
       signame=sym_assign
    elseif(signamebase==sym_make_subref.or.signamebase==sym_make_sublhs.or.&
         signamebase==sym_make_sublhs_amp) then
       signame=sym_sub
    endif
    if(cnode_flags_clear(node,call_flags,&
         call_is_comm)) then
       call more_error(coder%context,' '//trim(pm_name_as_string(coder%context,&
            signame))//' (')
       n=0
    else
       call more_error(coder%context,' '//trim(pm_name_as_string(coder%context,&
            signame))//'%(')
       call more_error(coder%context,'  region:   '//&
            trim(pm_typ_as_string(coder%context,&
            coder%wstack(base+nkeys+1),distr=.true.)))
       call more_error(coder%context,'  schedule: '//&
            trim(pm_typ_as_string(coder%context,&
            coder%wstack(base+nkeys+2),distr=.true.)))
       call more_error(coder%context,'  here:     '//&
            trim(pm_typ_as_string(coder%context,&
            coder%wstack(base+nkeys+3),distr=.true.)))
       n=3
    endif
    do i=nkeys+n+1,nargs
       if(i<nargs.or.nkeys>0) then
          join=', '
       else
          join=' '
       endif
       call check_amp(i-nkeys)
       call more_error(coder%context,&
            '   '//ampstr//&
            trim(pm_typ_as_string(coder%context,coder%wstack(base+i)))//join)
    enddo
    if(.not.present(numargs).and.cnode_flags_set(node,call_flags,call_is_vararg)) then
       call more_error(coder%context,'   ...')
    endif
    do i=1,nkeys
       if(i<nkeys) then
          if(all(coder%wstack(base+i+1:base+nkeys)==pm_tiny_int)) then
             join=' '
          else
             join=', '
          endif
       else
          join=' '
       endif
       name=pm_set_key(coder%context,keyargs,int(i,pm_ln))
       if(coder%wstack(base+i)/=pm_tiny_int) then
          call more_error(coder%context,'     '//&
               trim(pm_name_as_string(coder%context,int(name%offset)))//'='//&
               trim(pm_typ_as_string(coder%context,coder%wstack(base+i)))//join)
       endif
    enddo

    call more_error(coder%context,' )')
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
  subroutine print_proc_details(coder,node,sig,iscomm,tno)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: sig
    logical,intent(in):: iscomm
    integer,intent(in):: tno
    integer:: name,ampidx
    integer:: i,k,n,nx,nargs
    integer:: typ
    type(pm_ptr):: tv,tv2,key,amp,val
    character(len=256):: str,str2
    character(len=7):: buf1,buf2
    if(.not.pm_main_process) return
    if(coder%supress_errors) return
    k=0
    key=pm_dict_key(coder%context,coder%sig_cache,int(sig,pm_ln))
    val=pm_dict_val(coder%context,coder%sig_cache,int(sig,pm_ln))
    name=key%data%i(key%offset+pm_fast_esize(key))
    ampidx=key%data%i(key%offset+pm_fast_esize(key)-2)
    if(ampidx==0) then
       amp=pm_null_obj
    else
       amp=pm_name_val(coder%context,ampidx)
    endif
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
    if(add_char(':')) goto 777
    n=n+2
    call pm_name_string(coder%context,name,str(n:))
    n=len_trim(str)+1
    if(.not.iscomm) then
       if(pm_fast_isnull(amp)) then
          str(n:)=pm_typ_as_string(coder%context,tno)
       else
          tv=pm_typ_vect(coder%context,tno)
          nargs=pm_tv_numargs(tv)
          nx=0
          if(add_char('(')) goto 777
          do i=1,nargs
             call check_amp
             typ=pm_tv_arg(tv,i)
             call typ_to_str(coder%context,typ,str,n,.false.)
             if(n>len(str)-10) goto 777
             if(i<nargs) then
                if(add_char(',')) goto 777
             endif
          enddo
          if(add_char(')')) goto 777
       endif
    else
       if(add_char('%')) goto 777
       tv=pm_typ_vect(coder%context,tno)
       if(add_char('(')) goto 777
       typ=pm_tv_arg(tv,1)
       if(typ/=0) then
          if(add_char('region:')) goto 777
          call typ_to_str(coder%context,typ,str,n,.false.)
          if(add_char(',')) goto 777
       endif
       typ=pm_tv_arg(tv,2)
       if(typ/=0) then
          if(add_char('schedule:')) goto 777
          call typ_to_str(coder%context,typ,str,n,.false.)
          if(add_char(',')) goto 777
       endif
       typ=pm_tv_arg(tv,3)
       if(typ/=0) then
          if(add_char('here:')) goto 777
          call typ_to_str(coder%context,typ,str,n,.false.)
          if(add_char(',')) goto 777
       endif
       if(n>len(str)-10) goto 777
       nargs=pm_tv_numargs(tv)
       do i=4,nargs
          call check_amp
          typ=pm_tv_arg(tv,i)
          call typ_to_str(coder%context,typ,str,n,.false.)
          if(n>len(str)-10) goto 777
          if(i<nargs) then
             if(add_char(',')) goto 777
          endif
       enddo
       if(add_char(')')) goto 777
    endif
777 continue
    call more_error(coder%context,trim(str))
  contains

    include 'fesize.inc'
    include 'fisnull.inc'
    
    function add_char(c) result(term)
      character(len=*),intent(in):: c
      logical:: term
      if(n>len(str)-10-len(c)) then
         str(n:n+2)='...'
         term=.true.
      else
         str(n:n+len(c)-1)=c
         n=n+len(c)
         term=.false.
      endif
    end function add_char

    subroutine check_amp
      logical:: junk
      if(pm_fast_isnull(amp)) return
      if(k>pm_fast_esize(amp)) return
      if(amp%data%i(amp%offset+k)==i) then
         k=k+1
         junk=add_char('&')
      endif
    end subroutine check_amp
    
  end subroutine print_proc_details


end module pm_infer
