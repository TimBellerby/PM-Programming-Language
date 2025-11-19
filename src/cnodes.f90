!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2024
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

!================================================
! Middle-level Intermediate representation
!------------------------------------------------
! The structure is built of cnodes:
! proc    - user defined procedure
! builtin - intrinsic (built-in) procedure
! cblock  - list of calls
! call    - either linked to a signature
!           (list of argument types and procedures)
!           or flagged as a "special" call
!         - argument list refers to var, const, and
!           cblock cnodes
!         - control stuctures = calls with cblock args
! var     - vars and runtime consts
! const   - literal constants
!================================================

module pm_cnodes

  use pm_kinds
  use pm_sysdep
  use pm_compbase
  use pm_memory
  use pm_hash
  use pm_options
  use pm_lib
  use pm_symbol
  use pm_types
  use pm_ast

  implicit none

  ! Debug cnode operations
  logical,parameter:: debug_cnodes=.true.
  
  ! Magic number for code tree nodes
  integer,parameter:: cnode_magic_no=10456_pm_p
  
  ! Offsets common to all cnode structures
  integer,parameter:: cnode_magic=0
  integer,parameter:: cnode_kind=1
  integer,parameter:: cnode_modl_name=2
  integer,parameter:: cnode_lineno=3
  integer,parameter:: cnode_charno=4
  integer,parameter:: cnode_args=5

  ! Types of cnode structure
  integer,parameter:: cnode_is_cblock=1
  integer,parameter:: cnode_is_var=2
  integer,parameter:: cnode_is_const=3
  integer,parameter:: cnode_is_call=4
  integer,parameter:: cnode_is_arglist=5
  integer,parameter:: cnode_is_builtin=6
  integer,parameter:: cnode_is_proc=7
  integer,parameter:: cnode_is_resolved_proc=8
  integer,parameter:: cnode_is_arg_constraint=9
  integer,parameter:: cnode_is_par_constraint=10
  integer,parameter:: cnode_is_type_constraint=11
  integer,parameter:: cnode_is_any_sig=12
  integer,parameter:: cnode_is_autoconv_sig=13
  integer,parameter:: cnode_is_changelist=14
  integer,parameter:: cnode_is_callsig=15
  integer,parameter:: cnode_num_kinds=15

  ! Offsets into cblock cnodes
  integer,parameter:: cblock_parent=cnode_args+0
  integer,parameter:: cblock_first_var=cnode_args+1
  integer,parameter:: cblock_last_var=cnode_args+2
  integer,parameter:: cblock_first_call=cnode_args+3
  integer,parameter:: cblock_last_call=cnode_args+4
  integer,parameter:: cblock_sym=cnode_args+5
  integer,parameter:: cblock_start=cnode_args+6
  integer,parameter:: cblock_flags=cnode_args+7
  integer,parameter:: cblock_index=cnode_args+8
  integer,parameter:: cblock_last_loop_call=cnode_args+9
  integer,parameter:: cblock_var_inits=cnode_args+10
  integer,parameter:: cblock_node_size=11

  ! Flags for cblocks
  integer,parameter:: cblock_is_comm=1
  integer,parameter:: cblock_is_open=2

  ! Offsets into call cnodes
  integer,parameter:: call_args=cnode_args+0
  integer,parameter:: call_keys=cnode_args+1
  integer,parameter:: call_parent=cnode_args+2
  integer,parameter:: call_sig=cnode_args+3
  integer,parameter:: call_flags=cnode_args+4
  integer,parameter:: call_link=cnode_args+5
  integer,parameter:: call_back_link=cnode_args+6
  integer,parameter:: call_nret=cnode_args+7
  integer,parameter:: call_key_names=cnode_args+8
  integer,parameter:: call_index=cnode_args+9
  integer,parameter:: call_var=cnode_args+10
  integer,parameter:: call_amp=cnode_args+11
  integer,parameter:: call_node_size=12
  
  ! Offsets into var cnodes
  integer,parameter:: var_parent=cnode_args+0
  integer,parameter:: var_name=cnode_args+1
  integer,parameter:: var_flags=cnode_args+2
  integer,parameter:: var_link=cnode_args+3
  integer,parameter:: var_index=cnode_args+4
  integer,parameter:: var_lex_scope = cnode_args + 5
  integer,parameter:: var_node_size=6
  integer,parameter:: var_extra_info=cnode_args+6

  ! Flags for var cnodes
  integer,parameter:: var_is_var=1
  integer,parameter:: var_is_ref=2
  integer,parameter:: var_is_param=4
  integer,parameter:: var_is_shadowed=16
  integer,parameter:: var_is_imported=32
  integer,parameter:: var_is_accessed=64
  integer,parameter:: var_is_changed=128
  integer,parameter:: var_is_multi_access=256
  integer,parameter:: var_is_key=512
  integer,parameter:: var_is_varg=1024
  integer,parameter:: var_is_par_var=2048
  integer,parameter:: var_is_maybe_idx=4096
  integer,parameter:: var_is_where=8192
  integer,parameter:: var_is_reference=16384
  integer,parameter:: var_is_key_ptr=32768
  integer,parameter:: var_is_comm=65536
  integer,parameter:: var_is_temp=131072
  integer,parameter:: var_is_return=262144
  integer,parameter:: var_is_list_elem=524288
  integer,parameter:: var_is_param_move=1048576

  ! Offsets into proc & builtin nodes
  integer,parameter:: pr_ptype=cnode_args+0
  integer,parameter:: pr_rtype=cnode_args+1
  integer,parameter:: pr_nargs=cnode_args+2
  integer,parameter:: pr_nret=cnode_args+3
  integer,parameter:: pr_flags=cnode_args+4
  integer,parameter:: pr_amps=cnode_args+5
  integer,parameter:: pr_name=cnode_args+6

  ! Offets into proc nodes only
  integer,parameter:: pr_cblock=cnode_args+7
  integer,parameter:: pr_when= cnode_args+8
  integer,parameter:: pr_whenvar= cnode_args+9
  integer,parameter:: pr_max_index=cnode_args+10
  integer,parameter:: pr_recurse=cnode_args+11
  integer,parameter:: pr_id=cnode_args+12
  integer,parameter:: pr_ncalls=cnode_args+13
  integer,parameter:: pr_keys=cnode_args+14
  integer,parameter:: pr_keycall=cnode_args+15
  integer,parameter:: pr_argcall=cnode_args+16

  integer,parameter:: pr_node_size=17

  ! Offsets into builtin nodes only
  integer,parameter:: bi_opcode=cnode_args+7
  integer,parameter:: bi_opcode2=cnode_args+8
  integer,parameter:: bi_id=cnode_args+9
  integer,parameter:: bi_node_size=10

   ! Special signatures
  integer,parameter:: sp_sig_in_process=-1
  integer,parameter:: sp_sig_recursive=-2
  integer,parameter:: sp_sig_break=-3
  integer,parameter:: sp_sig_link=-4
  integer,parameter:: sp_sig_dup=-5
  integer,parameter:: sp_sig_noop=-6
  integer,parameter:: sp_sig_setval=-7
  integer,parameter:: sp_sig_init=-8
  integer,parameter:: sp_sig_assign=-9

  integer,parameter:: sp_sig_deactivated=-huge(1)

  ! Access codes
  ! Note - if change access_kind then need to
  ! check for and change any %data%i8 accesses
  ! in bprop routines
  integer,parameter:: access_kind=pm_i16
  integer(pm_p),parameter:: access_pm_type=pm_int16
  integer(access_kind),parameter:: access_deactivated_call=-1
  integer(access_kind),parameter:: access_is_var=1
  integer(access_kind),parameter:: access_used_ever=2
  integer(access_kind),parameter:: access_not_last=4
  integer(access_kind),parameter:: access_used_now=8
  integer(access_kind),parameter:: access_used_by_at=16
  integer(access_kind),parameter:: access_may_be_used=32
  integer(access_kind),parameter:: access_may_be_at=64
  integer(access_kind),parameter:: access_holds_result=128
  integer(access_kind),parameter:: access_not_passed=256
  integer(access_kind),parameter:: access_is_list=512
  integer(access_kind),parameter:: access_may_detag=1024
  integer(access_kind),parameter:: access_needs_movability=2048
  integer(access_kind),parameter:: access_everything=&
       access_is_var+access_used_ever+access_used_now+access_used_by_at
  
contains

  !=========================================================
  ! Given a result variable cnode - return the call, arg#
  ! and cnode of any argument liked to the result through
  ! returning a reference to that argument
  !
  ! If result will be always movable then returns true
  ! and may not set new_call etc.
  !=========================================================
  function follow_result_back(context,cache,rv,var,new_call,&
       new_call_index,new_var,new_argn) result(movable)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: cache
    integer,intent(in),dimension(:):: rv
    type(pm_ptr),intent(in):: var
    type(pm_ptr),intent(out):: new_call,new_var
    integer,intent(out):: new_call_index,new_argn
    logical:: movable
    integer:: n,new_proc_sig,nargs,i
    type(pm_ptr):: args,new_proc,key_names,keys

    movable=.false.
    
    ! - first find call that returned the value
    new_call=cnode_get(var,var_extra_info)
    new_call_index=cnode_get_num(new_call,call_index)
    n=-cnode_get_num(var,var_name)
    new_proc_sig=rv(new_call_index)
    if(new_proc_sig==sp_sig_link) then
       new_var=cnode_arg(cnode_get(new_call,call_args),2)
       new_argn=2
       return
    elseif(new_proc_sig<0) then
       movable=.true.
       return
    endif
    
    ! - now find the proc that was called from there
    new_proc=pm_dict_val(context,cache,int(new_proc_sig,pm_ln))
    if(cnode_get_kind(new_proc)==cnode_is_autoconv_sig) then
       new_proc=pm_dict_val(context,cache,&
            int(cnode_num_arg(new_proc,cnode_numargs(new_proc)),pm_ln))
    endif

    ! Builtin, so assume movable value is returned
    if(cnode_get_kind(new_proc)/=cnode_is_resolved_proc) then
       movable=.true.
       return
    endif
    new_proc=cnode_arg(new_proc,1)

    ! - now find the argument # (if any) which the returned value references
    new_argn=cnode_get_num(new_proc,cnode_args+pr_node_size+n-1)
    if(new_argn==0) then
       movable=.true.
       return
    endif
    
    ! Now detetermine the referenced argument
    args=cnode_get(new_call,call_args)
    nargs=cnode_numargs(args)
    if(new_argn<0) then
       key_names=pm_name_val(context,cnode_get_num(new_call,call_key_names))
       keys=cnode_get(new_call,call_keys)
       do i=1,cnode_numargs(keys)
          if(key_names%data%i(key_names%offset+i-1)==-new_argn) then
             new_var=cnode_arg(keys,i)
             new_argn=i+cnode_numargs(args)
          endif
       enddo
    else
       new_var=cnode_arg(args,new_argn+cnode_get_num(new_call,call_nret))
    endif
  end function follow_result_back

  !========================================================================
  ! Check arg #idx of call in callnode is a final use of a variable/param
  !========================================================================
  function final_flag_set(callnode,rvec,idx) result(ok)
    type(pm_ptr),intent(in):: callnode,rvec
    integer,intent(in):: idx
    logical:: ok
    integer:: slot
    ok=final_flag_set_at_call_index(cnode_get_num(callnode,call_index),rvec,idx)
  end function final_flag_set

  !========================================================================
  ! Check arg #idx of call @ call_index is a final use of a variable/param
  !========================================================================
  function final_flag_set_at_call_index(call_index,rvec,idx) result(ok)
    type(pm_ptr),intent(in):: rvec
    integer,intent(in):: call_index,idx
    logical:: ok
    integer:: slot
    slot=call_index+1
    slot=slot+(idx-1)/bit_size(1)
    ok=btest(max(rvec%data%i(rvec%offset+slot),0),iand(idx-1,bit_size(1)-1))
  end function final_flag_set_at_call_index

  !=================================
  ! Check cnode (debugging)
  !=================================
  subroutine check_cnode(ptr,n)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer(pm_p):: m
    if(.not.pm_fast_vkind(ptr)==pm_pointer) then
       write(*,*) 'vKind=',ptr%data%vkind
       call pm_panic('cnode not ptr')
    endif
    if(ptr%data%ptr(ptr%offset)%offset/=cnode_magic_no) then
       call pm_panic('bad cnode magic no')
    endif
    m=ptr%data%ptr(ptr%offset+1)%offset
    if(m<1.or.m>cnode_num_kinds) &
         call pm_panic('cnode bad kind')
    if(n<0.or.n>pm_fast_esize(ptr)) &
         call pm_panic('bad cnode offset')
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end subroutine check_cnode

  !==========================================
  ! Get name from variable cnode
  !==========================================
  function cnode_var_name(ptr) result(name)
    type(pm_ptr):: ptr
    integer:: name
    name=max(0,cnode_get_num(ptr,var_name))
  end function cnode_var_name

  !==========================================
  ! Get argument n from cnode
  !==========================================
  function cnode_arg(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr):: val
    if(debug_cnodes) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+cnode_args+n-1)
  end function cnode_arg

  !======================================
  ! Get element n from cnode
  !======================================
  function cnode_get(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr):: val
    if(debug_cnodes) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)
  end function cnode_get

  !====================================
  ! Set element n of cnode
  !====================================
  subroutine cnode_set(context,ptr,n,val)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr),intent(in):: val
    if(debug_cnodes) call check_cnode(ptr,n)
    call pm_ptr_assign(context,ptr,int(n,pm_ln),val)
  end subroutine  cnode_set

  ! ========================================
  ! Get element n from cnode as a number
  !=========================================
  function cnode_get_num(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer:: val
    if(debug_cnodes) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
  end function cnode_get_num

  !============================================
  ! Module name for a cnode
  !============================================
  function cnode_module_name(ptr) result(name)
    type(pm_ptr),intent(in):: ptr
    integer:: name
    name=cnode_get_num(ptr,cnode_modl_name)
  end function cnode_module_name

  !============================================
  ! Get argument n from cnode as a number
  !============================================
  function cnode_num_arg(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer:: val
    if(debug_cnodes) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n+cnode_args-1)%offset
  end function cnode_num_arg

  !=========================================
  ! Get element n from cnode as a name
  !=========================================
  function cnode_get_name(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer:: val
    if(debug_cnodes) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
  end function cnode_get_name

  !==========================================
  ! Set element n in cnode to a new number
  ! (must be number already)
  !==========================================
  subroutine cnode_set_num(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer,intent(in):: val
    if(debug_cnodes) call check_cnode(ptr,n)
    ptr%data%ptr(ptr%offset+n)%offset=val
  end subroutine  cnode_set_num

  !==========================================
  ! Increment argument n from cnode
  !==========================================
  subroutine cnode_incr_num(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer,intent(in):: val
    if(debug_cnodes) call check_cnode(ptr,n)
    ptr%data%ptr(ptr%offset+n)%offset=&
         ptr%data%ptr(ptr%offset+n)%offset+val
  end subroutine  cnode_incr_num

  !==============================================
  ! Set given flags in an element of a cnode
  ! (bitwise or of existing number)
  !==============================================
  subroutine cnode_set_flags(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer,intent(in):: val
    type(pm_ptr):: p
    if(debug_cnodes) then
       call check_cnode(ptr,n)
       p=ptr%data%ptr(ptr%offset+n)
       if(pm_fast_vkind(p)/=pm_tiny_int.and.pm_fast_vkind(p)/=pm_null) then
          write(*,*) 'vkind=',pm_fast_vkind(ptr)
          call pm_panic('Set flags')
       endif
       if(cnode_get_kind(ptr)==cnode_is_var.and.&
            n/=var_flags.or.&
            cnode_get_kind(ptr)==cnode_is_cblock.and.n/=cblock_flags.or.&
            cnode_get_kind(ptr)==cnode_is_call.and.n/=call_flags) then
          call pm_panic('set flags')
       endif
    endif
    ptr%data%ptr(ptr%offset+n)%offset=ior(&
         ptr%data%ptr(ptr%offset+n)%offset,int(val,pm_p))
  contains
    include 'fvkind.inc'
  end subroutine  cnode_set_flags

  !============================================
  ! Clear flags in element of a code code
  ! (Bitwise clear of exiting number)
  !============================================
  subroutine cnode_clear_flags(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer,intent(in):: val
    type(pm_ptr):: p
    if(debug_cnodes) then
       call check_cnode(ptr,n)
       p=ptr%data%ptr(ptr%offset+n)
       if(pm_fast_vkind(p)/=pm_tiny_int.and.pm_fast_vkind(p)/=pm_null) then
          write(*,*) 'vkind=',pm_fast_vkind(ptr)
          call pm_panic('Set flags')
       endif
       if(cnode_get_kind(ptr)==cnode_is_var.and.&
            n/=var_flags.or.&
            cnode_get_kind(ptr)==cnode_is_cblock.and.n/=cblock_flags.or.&
            cnode_get_kind(ptr)==cnode_is_call.and.n/=call_flags) then
          call pm_panic('set flags')
       endif
    endif
    ptr%data%ptr(ptr%offset+n)%offset=iand(&
         ptr%data%ptr(ptr%offset+n)%offset,not(int(val,pm_p)))
  contains
    include 'fvkind.inc'
  end subroutine  cnode_clear_flags

  !===================================================================
  ! Check all given flags in a given element of a cnode are clear
  ! (bitwise and of extisting number and check for zero)
  !===================================================================
  function cnode_flags_clear(ptr,n,flags) result(ok)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n,flags
    logical:: ok
    integer:: val
    if(debug_cnodes) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
    ok=iand(flags,val)==0
  end function cnode_flags_clear

  !============================================================
  ! Check all given flags in an element of a cnode are set
  ! (bitwise and)
  !============================================================
  function cnode_flags_set(ptr,n,flags) result(ok)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n,flags
    logical:: ok
    integer(pm_p):: val
    if(debug_cnodes) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
    ok=iand(flags,int(val))==flags
  end function cnode_flags_set

  !==========================================
  ! Return kind of a cnode
  !==========================================
  function cnode_get_kind(ptr) result(n)
    type(pm_ptr),intent(in):: ptr
    integer:: n
    if(pm_debug_checks) call check_cnode(ptr,0)
    n=ptr%data%ptr(ptr%offset+1)%offset
  end function cnode_get_kind

  !========================================
  ! Return number of arguments of a cnode
  !========================================
  function cnode_numargs(ptr) result(n)
    type(pm_ptr),intent(in):: ptr
    integer:: n
    if(debug_cnodes) call check_cnode(ptr,0)
    n=pm_fast_esize(ptr)-cnode_args+1
  contains
    include 'fesize.inc'
  end function cnode_numargs


  !========================================
  ! Does a cblock contain any communicating
  ! operations?
  !========================================
  function cblock_has_comm(cblock) result(ok)      
    type(pm_ptr):: cblock
    logical:: ok
    ok=(iand(cnode_get_num(cblock,cblock_flags),&
         cblock_is_comm)/=0)
  end function cblock_has_comm

  
  subroutine print_all_sigs(context,iunit,sig_cache,proc_cache,poly_cache)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: sig_cache,proc_cache,poly_cache
    integer:: i

    do i=1,pm_dict_size(context,proc_cache)
       call print_sig(context,iunit,sig_cache,proc_cache,poly_cache,i)
    enddo
    
  end subroutine print_all_sigs
    
  subroutine print_sig(context,iunit,sig_cache,proc_cache,poly_cache,n)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,n
    type(pm_ptr),intent(in):: sig_cache,proc_cache,poly_cache
    integer:: kind,i
    type(pm_ptr):: cnode,key,rvec
    key=pm_dict_key(context,proc_cache,int(n,pm_ln))
    cnode=pm_dict_val(context,proc_cache,int(n,pm_ln))
    if(pm_fast_vkind(cnode)==pm_pointer) then
       kind=cnode_get_kind(cnode)
       !write(*,*) 'KinD=',kind
       select case(kind)
       case(cnode_is_resolved_proc)
          write(iunit,'(a)') '['//trim(pm_int_as_string(n))//']'//&
               trim(pm_name_as_string(context,&
               cnode_get_name(cnode_arg(cnode,1),pr_name)))//' {'
          if(cnode_flags_set(cnode,cnode_args+2,proccall_is_comm)) &
               write(iunit,'(a)') '  [comm]'
          if(cnode_flags_set(cnode,cnode_args+2,proc_is_recursive)) &
               write(iunit,'(a)') '  [recursive]'
          if(cnode_flags_set(cnode,cnode_args+2,proc_unfinished)) &
               write(iunit,'(a)') '  [unfinished]'
          if(cnode_flags_set(cnode,cnode_args+2,proc_is_impure)) &
               write(iunit,'(a)') '  [impure]'
          if(cnode_flags_set(cnode,cnode_args+2,proc_is_not_inlinable)) &
               write(iunit,'(a)') '  [not inlinable]'
          if(cnode_flags_set(cnode,cnode_args+2,proc_has_for)) &
               write(iunit,'(a)') '  [has for]'
          if(cnode_flags_set(cnode,cnode_args+2,proc_is_not_pure_each)) &
               write(iunit,'(a)') '  [not pure each]'
          if(cnode_flags_set(cnode,cnode_args+2,proc_has_vkeys)) &
               write(iunit,'(a)') '  [has vkeys]'
          if(cnode_flags_set(cnode,cnode_args+2,proc_is_dcomm)) &
               write(iunit,'(a)') '  [dcomm]'
          if(cnode_flags_set(cnode,cnode_args+2,proc_is_file)) &
               write(iunit,'(a)') '  [file]'
          rvec=cnode_arg(cnode,2)
          
          if(pm_fast_istiny(rvec)) then
             write(iunit,*) '---->',cnode%offset
             rvec=pm_dict_val(context,poly_cache,int(rvec%offset,pm_ln))
          endif
          call print_proc_cnode(context,iunit,rvec,&
               sig_cache,proc_cache,cnode_arg(cnode,1))
          write(iunit,*) '   ----------------'
          call print_bprop_list(iunit,cnode_arg(cnode,6))
          write(iunit,*) '=='
          call print_bprop_list(iunit,cnode_arg(cnode,7))
          write(iunit,*) '   ----------------'
          call dump_parse_tree(context,iunit,cnode_arg(cnode,6),2)
          call dump_parse_tree(context,iunit,cnode_arg(cnode,7),2)
          write(iunit,'(a)') '}'
       case(cnode_is_callsig)
          write(iunit,'(a)') 'sig{'
          do i=1,cnode_numargs(cnode)
             call print_proc_cnode(context,iunit,pm_null_obj,&
                  sig_cache,proc_cache,cnode_arg(cnode,i))
          enddo
          write(iunit,'(a)') '}'
       case(cnode_is_arglist)
          write(iunit,'(a)') '['//trim(pm_int_as_string(n))//']'//'{'
          do i=3,cnode_numargs(cnode),2
             write(iunit,'(a)') '  '//&
                  trim(pm_name_as_string(context,&
                  key%data%i(key%offset+pm_fast_esize(key))))//&
                  trim(pm_type_as_string(context,&
                  cnode_num_arg(cnode,i)))//' {'
             call print_proc_cnode(context,iunit,pm_null_obj,&
                  sig_cache,proc_cache,cnode_arg(cnode,i+1))
             write(iunit,'(a)') '  }'
          enddo
          write(iunit,'(a)') '}'
       case(cnode_is_any_sig)
          write(iunit,'(a)') '['//trim(pm_int_as_string(n))//']'//'Any{'
          do i=1,cnode_numargs(cnode)
             call pm_dump_tree(context,iunit,cnode_arg(cnode,i),2)
          enddo
          write(iunit,'(a)') '}'
       case(cnode_is_autoconv_sig)
          write(iunit,'(a)') '['//trim(pm_int_as_string(n))//']'//'Auto {'
          do i=1,cnode_numargs(cnode)
             call pm_dump_tree(context,iunit,cnode_arg(cnode,i),2)
          enddo
          write(iunit,'(a)') '}'
       case(cnode_is_builtin)
          write(iunit,'(a)') '['//trim(pm_int_as_string(n))//'] {'
          call print_proc_cnode(context,iunit,pm_null_obj,&
               sig_cache,proc_cache,cnode)
          write(iunit,'(a)') '}'
       case default
          write(iunit,'("????",i5)') kind
       end select
    else
       call pm_dump_tree(context,iunit,cnode,1)
    endif
  contains
    include 'fesize.inc'
    include 'fvkind.inc'
    include 'fistiny.inc'
  end subroutine print_sig

  subroutine print_proc_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: rvec,sig_cache,proc_cache,cnode
    integer:: i,nret

    write(iunit,'(a)') '  '//&
         trim(pm_name_as_string(context,cnode_get_num(cnode,pr_name)))//&
         merge('.',merge(merge('''','%',cnode_flags_set(cnode,pr_flags,proccall_is_general)),' ',&
         cnode_flags_set(cnode,pr_flags,proccall_is_comm)),cnode_flags_set(cnode,pr_flags,proccall_is_method))//&
         trim(pm_type_as_string(context,cnode_get_num(cnode,pr_ptype)))//' {'
    
    if(cnode_get_kind(cnode)==cnode_is_builtin) then
       write(iunit,'(a)') '   Builtin '//&
            op_names(cnode_get_num(cnode,bi_opcode))//&
            pm_int_as_string(cnode_get_num(cnode,bi_opcode2))
    else
       nret=cnode_get_num(cnode,pr_nret)
       write(iunit,'(A,i2,A,i2,A,i2,A,i3,A)') &
            '   [nargs=',&
            cnode_get_num(cnode,pr_nargs),',nret=',nret,&
            ',ncalls=',cnode_get_num(cnode,pr_ncalls),']'
        call print_cblock_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode_get(cnode,pr_cblock),4)
        if(nret>0) then
           write(iunit,'(A,32i3)') '   RetRef:',(cnode_get_num(cnode,i),i=cnode_args+pr_node_size,cnode_args+pr_node_size+nret-1)
        endif
    endif
    write(iunit,'(a)') '  }'
  contains
    include 'fisnull.inc'
  end subroutine print_proc_cnode
  
  recursive subroutine print_cblock_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode,depth)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,depth
    type(pm_ptr),intent(in):: rvec,sig_cache,proc_cache,cnode
    type(pm_ptr)::p
    p=cnode_get(cnode,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       call print_call_cnode(context,iunit,rvec,sig_cache,proc_cache,p,depth)
       p=cnode_get(p,call_link)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine print_cblock_cnode
  
  recursive subroutine print_call_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode,depth)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,depth
    type(pm_ptr),intent(in):: rvec,sig_cache,proc_cache,cnode
    integer:: signo,name,i,j,k,nret,nargs,modl,line
    type(pm_ptr):: p,args,amps,keys,keynames
    character(len=120):: str,location

    args=cnode_get(cnode,call_args)
    nargs=cnode_numargs(args)
    nret=cnode_get_num(cnode,call_nret)
    amps=cnode_get(cnode,call_amp)
    amps=pm_name_val(context,int(amps%offset))
    
    signo=cnode_get_num(cnode,call_sig)
    str=' '
    if(.not.pm_fast_isnull(rvec)) then
       k=rvec%data%i(rvec%offset+cnode_get_num(cnode,call_index))
       if(k==sp_sig_deactivated) then
          str='[--]'
       endif
    endif
    if(signo<0) then
       str=repeat(' ',depth)//trim(str)//pm_name_as_string(context,-signo)
       i=len_trim(str)+1
       if(.not.pm_fast_isnull(rvec)) then
          k=rvec%data%i(rvec%offset+cnode_get_num(cnode,call_index))          
          if(k>=0) then
             if(signo==-sym_any.or.signo==-sym_any_invar) then
                call multi_version(k,2,4)
                goto 10
             elseif(signo==-sym_pm_each_index) then
                call multi_version(k,nret+2,nret+3)
                goto 10
             else
                call append_to_line(iunit,str,i,&
                     '['//trim(pm_int_as_string(k))//'] ',.false.,depth)
             endif
          endif
       endif
    elseif(signo==0) then
       str=repeat(' ',depth)//trim(str)//'var-call'
       i=len_trim(str)+1
       call print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,&
            cnode_get(cnode,call_var),depth,str,i)
       if(cnode_flags_set(cnode,call_flags,proccall_is_comm)) then
          call append_to_line(iunit,str,i,'%',.false.,depth)
       endif
       if(pm_fast_isnull(rvec)) then
          call append_to_line(iunit,str,i,': ',.false.,depth)
       else
          k=rvec%data%i(rvec%offset+cnode_get_num(cnode,call_index))
          if(k==sp_sig_deactivated) then
             call append_to_line(iunit,str,i,'[--]: ',.false.,depth)
          else
             call append_to_line(iunit,str,i,'['//trim(pm_int_as_string(k))//']: ',.false.,depth)
          endif
       endif
    else
       p=pm_dict_key(context,sig_cache,&
            int(signo,pm_ln))
       name=p%data%i(p%offset+pm_fast_esize(p))
       if(.not.pm_fast_isnull(cnode_get(cnode,call_var))) then
          str=repeat(' ',depth)//trim(str)//'call *('
          i=depth+7
          call print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,&
               cnode_get(cnode,call_var),depth,str,i)
          call append_to_line(iunit,str,i,') ',.false.,depth)
       elseif(pm_fast_isnull(rvec)) then
          str=repeat(' ',depth)//trim(str)//'call '//pm_name_as_string(context,name)
       else
          k=rvec%data%i(rvec%offset+cnode_get_num(cnode,call_index))
          if(k==sp_sig_link) then
             str=trim(str)//repeat(' ',depth)//trim(str)//'call [link]'//&
                  pm_name_as_string(context,name)
          elseif(k==sp_sig_dup) then
             str=repeat(' ',depth)//trim(str)//'call [dup]'//&
                  pm_name_as_string(context,name)
          elseif(k==sp_sig_noop) then
             str=repeat(' ',depth)//trim(str)//'call [noop]'//&
                  pm_name_as_string(context,name)
          elseif(k==sp_sig_setval) then
             str=repeat(' ',depth)//trim(str)//'call [setval]'//&
                  pm_name_as_string(context,name)
          elseif(k==sp_sig_init) then
             str=repeat(' ',depth)//trim(str)//'call [init]'//&
                  pm_name_as_string(context,name)
          elseif(k==sp_sig_assign) then
             str=repeat(' ',depth)//trim(str)//'call [assign]'//&
                  pm_name_as_string(context,name)     
          elseif(k==sp_sig_deactivated) then
             str=repeat(' ',depth)//trim(str)//'call '//&
                  pm_name_as_string(context,name)
          elseif(k<0) then
             str=repeat(' ',depth)//trim(str)//'call '//'!![-'//trim(pm_int_as_string(-k))//']'&
                  //pm_name_as_string(context,name)
          else
             str=repeat(' ',depth)//trim(str)//'call '//'['//trim(pm_int_as_string(k))//']'&
                  //pm_name_as_string(context,name)
          endif
       endif
       i=len_trim(str)
       call append_proc_call_flags(iunit,str,i,cnode_get_num(cnode,call_flags),.false.,depth)
       i=i+1
    end if

    if(nret>0) then
       do j=1,nret
          call print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode_arg(args,j),depth,str,i)
          i=i+1
       enddo
       call append_to_line(iunit,str,i,'<- ',.false.,depth)
    endif
    k=0
    do j=nret+1,nargs
       if(.not.pm_fast_isnull(amps)) then
          if(pm_fast_vkind(amps)/=pm_int) then
             call append_to_line(iunit,str,i,'?AMPS?',.false.,depth)
             exit
          else
             if(amps%data%i(amps%offset+k)==i-nret) then
                call append_to_line(iunit,str,i,'&',.false.,depth)
                k=min(k+1,pm_fast_esize(amps))
             endif
          endif
       endif
       if(.not.pm_fast_isnull(rvec)) then
          if(final_flag_set(cnode,rvec,j-nret)) then
             call append_to_line(iunit,str,i,'@',.false.,depth)
          endif
       endif
       call print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode_arg(args,j),depth,str,i)
       i=i+1
    enddo
    keys=cnode_get(cnode,call_keys)
    if(.not.pm_fast_isnull(keys)) then
       
       keynames=pm_name_val(context,cnode_get_num(cnode,call_key_names))
       do j=1,cnode_numargs(keys)
          call append_to_line(iunit,str,i,&
               trim(pm_name_as_string(context,keynames%data%i(keynames%offset+j-1))),.false.,depth)
          call append_to_line(iunit,str,i,':=:'//trim(pm_name_as_string(context,name))//':',.false.,depth)
          call print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode_arg(keys,j),depth,str,i)
          i=i+1
       enddo
    endif

10  continue
    modl=cnode_get_num(cnode,cnode_modl_name)
    line=cnode_get_num(cnode,cnode_lineno)
    location=trim(pm_name_as_string(context,modl))//':'//pm_int_as_string(line)
    if(i>len(str)-len_trim(location)) then
       write(iunit,'(a)') str
       str=' '
    endif
    str(len(str)-len_trim(location)+1:)=location
    write(iunit,'(a)') str

  contains
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'

    subroutine multi_version(csig,block_arg,limits_arg)
      integer,intent(in):: csig,block_arg,limits_arg
      integer:: j,k,slot1,slot2
      type(pm_ptr):: arg,rv,rvs,slots
      slots=cnode_arg(cnode_arg(args,limits_arg),1)
      slot1=slots%data%i(slots%offset)
      slot2=slots%data%i(slots%offset+1)
      rvs=pm_dict_val(context,proc_cache,&
            int(csig,pm_ln))
      do j=1,nret
         call print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode_arg(args,j),depth,str,i)
         i=i+1
      enddo
      call append_to_line(iunit,str,i,'<- ',.false.,depth)
      do j=nret+1,nargs
         if(j==block_arg) then
            call append_to_line(iunit,str,i,'#'//trim(pm_int_as_string(cnode_numargs(rvs)))//'{ ',.false.,depth)
            do k=1,cnode_numargs(rvs)
               rv=cnode_arg(rvs,k)
               rvec%data%i(rvec%offset+slot1:rvec%offset+slot2)=&
                    rv%data%i(rv%offset:rv%offset+slot2-slot1)
               call print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode_arg(args,j),depth,str,i)
            enddo
            call append_to_line(iunit,str,i,' }',.false.,depth)
         elseif(j==limits_arg) then
            call append_to_line(iunit,str,i,' '//trim(pm_int_as_string(slot1))//'..'//&
                 trim(pm_int_as_string(slot2)),.false.,depth)
         else
            call print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode_arg(args,j),depth,str,i)
         endif
         i=i+1
      enddo
    end subroutine multi_version
    
  end subroutine print_call_cnode

  recursive subroutine print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode,depth,str,i)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,depth
    type(pm_ptr),intent(in):: rvec,sig_cache,proc_cache,cnode
    character(len=*),intent(inout):: str
    integer,intent(inout):: i
    integer:: kind,name,tno
    type(pm_ptr):: p
    kind=pm_fast_vkind(cnode)
    if(kind==pm_tiny_int) then
       call append_to_line(iunit,str,i,&
            trim(pm_int_as_string(int(cnode%offset))),.false.,depth)
    elseif(kind==pm_null) then
       call append_to_line(iunit,str,i,&
            'NULL',.false.,depth)
    elseif(kind==pm_name) then
       call append_to_line(iunit,str,i,&
            "'"//trim(pm_name_as_string(context,int(cnode%offset))),.false.,depth)
    elseif(kind==pm_type) then
       call append_to_line(iunit,str,i,&
            '<'//trim(pm_type_as_string(context,int(cnode%offset)))//'>',.false.,depth)
    else
       kind=cnode_get_kind(cnode)
       select case(kind)
       case(cnode_is_var)
          name=cnode_var_name(cnode)
          if(name==0) then
             call append_to_line(iunit,str,i,'#'//&
                  trim(pm_int_as_string(cnode_get_num(cnode,var_index))),.false.,depth)
          else
             call append_quoted_to_line(iunit,str,i,&
                  trim(pm_name_as_string(context,name)),.false.,depth)
             if(.not.cnode_flags_clear(cnode,var_flags,var_is_imported+var_is_shadowed)) then
                call append_to_line(iunit,str,i,''''//&
                     trim(pm_int_as_string(cnode_get_num(cnode,var_index))),.false.,depth)
             endif
          endif
          if(.not.pm_fast_isnull(rvec)) then
             tno=rvec%data%i(rvec%offset+cnode_get_num(cnode,var_index))
             if(tno==sp_sig_deactivated) then
                call append_to_line(iunit,str,i,'[---]',.false.,depth)
             else
                call append_to_line(iunit,str,i,&
                     '['//trim(pm_type_as_string(context,tno))//']',.false.,depth)
             endif
          endif
          if(cnode_flags_set(cnode,var_flags,var_is_comm)) then
             call append_to_line(iunit,str,i,'^',.false.,depth)
          elseif(cnode_flags_set(cnode,var_flags,var_is_maybe_idx)) then
             call append_to_line(iunit,str,i,'#',.false.,depth)
          elseif(cnode_flags_set(cnode,var_flags,var_is_reference)) then
             call append_to_line(iunit,str,i,'{',.false.,depth)
             call print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,&
                  cnode_get(cnode,var_extra_info),depth,str,i)
             call append_to_line(iunit,str,i,'}',.false.,depth)
          endif
       case(cnode_is_const)
          p=cnode_arg(cnode,1)
          if(pm_fast_vkind(p)==pm_name) p=pm_name_val(context,int(p%offset))
          call append_to_line(iunit,str,i,&
               trim(pm_value_as_string(context,p)),.false.,depth)
       case(cnode_is_cblock)
          call append_to_line(iunit,str,i,'{',.true.,depth)
          call print_cblock_cnode(context,iunit,rvec,sig_cache,proc_cache,cnode,min(50,depth+2))
          str=' '
          str(depth+1:depth+1)='}'
          i=depth+1
       case(cnode_is_changelist)
          call append_to_line(iunit,str,i,'^(',.false.,depth)
          p=cnode_arg(cnode,1)
          do while(.not.pm_fast_isnull(p))
             call print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,p%data%ptr(p%offset),depth,str,i)
             p=p%data%ptr(p%offset+1)
             if(.not.pm_fast_isnull(p)) then
                call append_to_line(iunit,str,i,',',.false.,depth)
             endif
          enddo
          call append_to_line(iunit,str,i,') &(',.false.,depth)
          p=cnode_arg(cnode,2)
          do while(.not.pm_fast_isnull(p))
             call print_value_cnode(context,iunit,rvec,sig_cache,proc_cache,p%data%ptr(p%offset),depth,str,i)
             p=p%data%ptr(p%offset+1)
             if(.not.pm_fast_isnull(p)) then
                call append_to_line(iunit,str,i,',',.false.,depth)
             endif
          enddo
          call append_to_line(iunit,str,i,')',.false.,depth)
       end select
    endif
  contains
    include 'fvkind.inc'
    include 'fisnull.inc'
  end subroutine print_value_cnode

  subroutine append_proc_call_flags(iunit,str,i,flags,proc_flags,depth)
    integer,intent(in):: iunit
    character(len=*),intent(inout):: str
    integer,intent(inout):: i
    integer,intent(in):: flags
    logical,intent(in):: proc_flags
    integer,intent(in):: depth
    if(iand(flags,proccall_is_comm)/=0) then
       if(iand(flags,proccall_is_general)/=0) then
          call append_to_line(iunit,str,i,'''',.false.,depth)
       elseif(iand(flags,proccall_is_method)/=0) then
          call append_to_line(iunit,str,i,'.',.false.,depth)
       else
          call append_to_line(iunit,str,i,'%',.false.,depth)
       endif
    endif
    if(flags/=iand(flags,proccall_is_comm)) then
       call append_to_line(iunit,str,i,'<',.false.,depth)
       if(iand(flags,proccall_is_inline)/=0) then
          call append_to_line(iunit,str,i,'I',.false.,depth)
       endif
       if(iand(flags,proccall_is_no_inline)/=0) then
          call append_to_line(iunit,str,i,'N',.false.,depth)
       endif
       if(proc_flags) then
          if(iand(flags,proc_is_open)/=0) then
             call append_to_line(iunit,str,i,'o',.false.,depth)
          endif
          if(iand(flags,proc_is_cond)/=0) then
             call append_to_line(iunit,str,i,'c',.false.,depth)
          endif
          if(iand(flags,proc_is_uncond)/=0) then
             call append_to_line(iunit,str,i,'u',.false.,depth)
          endif
          if(iand(flags,proc_is_abstract)/=0) then
             call append_to_line(iunit,str,i,'a',.false.,depth)
          endif
       else
          if(iand(flags,call_is_fixed)/=0) then
             call append_to_line(iunit,str,i,'f',.false.,depth)
          endif
          if(iand(flags,call_is_assign_call)/=0) then
             call append_to_line(iunit,str,i,'a',.false.,depth)
          endif
          if(iand(flags,call_is_vararg)/=0) then
             call append_to_line(iunit,str,i,'v',.false.,depth)
          endif
          if(iand(flags,call_inline_when_compiling)/=0) then
             call append_to_line(iunit,str,i,'i',.false.,depth)
          endif
          if(iand(flags,call_is_cond)/=0) then
             call append_to_line(iunit,str,i,'c',.false.,depth)
          endif
          if(iand(flags,call_is_no_touch)/=0) then
             call append_to_line(iunit,str,i,'n',.false.,depth)
          endif
          if(iand(flags,call_is_unlabelled)/=0) then
             call append_to_line(iunit,str,i,'u',.false.,depth)
          endif
       endif
       call append_to_line(iunit,str,i,'>',.false.,depth)
    end if
  end subroutine append_proc_call_flags

  subroutine append_quoted_to_line(iunit,str,i,part,break,depth)
    integer,intent(in):: iunit
    character(len=*),intent(inout):: str
    integer,intent(inout):: i
    character(len=*),intent(in):: part
    logical,intent(in):: break
    integer,intent(in):: depth
    integer:: first
    first=iachar(part(1:1))
    if(first>=iachar('a').and.first<=iachar('z').or.&
         first>=iachar('A').and.first<=iachar('Z')) then
       call append_to_line(iunit,str,i,part,break,depth)
    else
       call append_to_line(iunit,str,i,"'"//trim(part)//"'",break,depth)
    endif
  end subroutine append_quoted_to_line
  
  subroutine append_to_line(iunit,str,i,part,break,depth)
    integer,intent(in):: iunit
    character(len=*),intent(inout):: str
    integer,intent(inout):: i
    character(len=*),intent(in):: part
    logical,intent(in):: break
    integer,intent(in):: depth
    integer:: n
    n=len(part)
    if(i+n>len(str)) then
       write(iunit,'(a)') str(1:min(len(str),i))
       str=repeat(' ',depth+1)//part(1:min(len(str)-depth-1,n))
       i=depth+1+n
    else
       str(i+1:i+n)=part(1:n)
       i=i+n
    endif
    if(break.or.i>len(str)) then
       write(iunit,'(a)') str(1:i)
       i=1
    endif
  end subroutine append_to_line

  subroutine print_bprop_list(iunit,list)
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: list
    integer:: i,j
    if(pm_fast_isnull(list)) return
    do i=1,list%data%i16(list%offset)
       call print_bprop_item(iunit,list%data%i16(list%offset+i))
    enddo
    i=list%data%i16(list%offset)+1
    do while(list%data%i16(list%offset+i)>0)
       write(iunit,'(a,i4,a)') 'List #',list%data%i16(list%offset+i),'{'
       do j=1,list%data%i16(list%offset+i+1)
          call print_bprop_item(iunit,list%data%i16(list%offset+i+j+1))
       enddo
       write(iunit,'(a)') '}'
       i=i+2+list%data%i16(list%offset+i+1)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine print_bprop_list

  subroutine print_bprop_item(iunit,item)
    integer,intent(in):: iunit
    integer(access_kind):: item
    character(len=120):: str
    integer:: n
    if(item==access_deactivated_call) then
       write(iunit,'(A)') '  -------'
       return
    endif
    str=' '
    n=3
    if(iand(item,access_is_var)/=0) then
       str(n:n+2)='var'
       n=n+4
    endif
    if(iand(item,access_used_ever)/=0) then
       str(n:n+3)='ever'
       n=n+5
    endif
    if(iand(item,access_not_last)/=0) then
       str(n:n+6)='notlast'
       n=n+8
    endif
    if(iand(item,access_used_now)/=0) then
       str(n:n+2)='now'
       n=n+4
    endif
    if(iand(item,access_used_by_at)/=0) then
       str(n:n+1)='at'
       n=n+3
    endif
    if(iand(item,access_may_be_used)/=0) then
       str(n:n+4)='maybe'
       n=n+6
    endif
    if(iand(item,access_may_be_at)/=0) then
       str(n:n+6)='maybeat'
       n=n+8
    endif
    if(iand(item,access_holds_result)/=0) then
       str(n:n+5)='result'
       n=n+7
    endif
    if(iand(item,access_not_passed)/=0) then
       str(n:n+5)='nopass'
       n=n+7
    endif
    if(iand(item,access_is_list)/=0) then
       str(n:n+3)='list'
       n=n+5
    endif
    if(iand(item,access_may_detag)/=0) then
       str(n:n+4)='detag'
       n=n+6
    endif
    if(iand(item,access_needs_movability)/=0) then
       str(n:n+3)='move'
       n=n+5
    endif
    write(iunit,'(I4,A)') item,trim(str)
  end subroutine print_bprop_item
  
end module pm_cnodes
