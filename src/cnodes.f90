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
  integer,parameter:: var_is_maybe_not_private=4096
  integer,parameter:: var_is_where=8192
  integer,parameter:: var_is_reference=16384
  integer,parameter:: var_is_key_ptr=32768

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
  integer,parameter:: pr_max_index=cnode_args+8
  integer,parameter:: pr_recurse=cnode_args+9
  integer,parameter:: pr_id=cnode_args+10
  integer,parameter:: pr_ncalls=cnode_args+11
  integer,parameter:: pr_keys=cnode_args+12
  integer,parameter:: pr_keycall=cnode_args+13
  integer,parameter:: pr_argcall=cnode_args+14
  integer,parameter:: pr_when= cnode_args+15
  integer,parameter:: pr_whenvar= cnode_args+16
  integer,parameter:: pr_node_size=17

  ! Offsets into builtin nodes only
  integer,parameter:: bi_opcode=cnode_args+7
  integer,parameter:: bi_opcode2=cnode_args+8
  integer,parameter:: bi_id=cnode_args+9
  integer,parameter:: bi_node_size=10

   ! Special signatures
  integer,parameter:: sp_sig_in_process=-1_pm_p
  integer,parameter:: sp_sig_recursive=-2_pm_p
  integer,parameter:: sp_sig_break=-3_pm_p
  integer,parameter:: sp_sig_link=-4_pm_p
  integer,parameter:: sp_sig_dup=-5_pm_p
  integer,parameter:: sp_sig_noop=-6_pm_p
  integer,parameter:: sp_sig_setval=-7_pm_p

  integer,parameter:: sp_sig_deactivated=-huge(1)

  ! Access codes
  ! Note - if change access_kind then need to
  ! check for and change any %data%i8 accesses
  ! in bprop routines
  integer,parameter:: access_kind=pm_i8
  integer(pm_p),parameter:: access_pm_type=pm_int8
  integer(access_kind),parameter:: access_deactivated_call=-1
  integer(access_kind),parameter:: access_is_var=1
  integer(access_kind),parameter:: access_used_ever=2
  integer(access_kind),parameter:: access_used_now=4
  integer(access_kind),parameter:: access_holds_result=8
  integer(access_kind),parameter:: access_not_passed=16
  integer(access_kind),parameter:: access_everything=&
       access_is_var+access_used_ever+access_used_now
  
contains

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

  
  subroutine print_all_sigs(context,iunit,sig_cache,proc_cache)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: sig_cache,proc_cache
    integer:: i

    do i=1,pm_dict_size(context,proc_cache)
       call print_sig(context,iunit,sig_cache,proc_cache,i)
    enddo
    
  end subroutine print_all_sigs
    
  subroutine print_sig(context,iunit,sig_cache,proc_cache,n)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,n
    type(pm_ptr),intent(in):: sig_cache,proc_cache
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
          else
             call print_proc_cnode(context,iunit,cnode_arg(cnode,2),&
                  sig_cache,cnode_arg(cnode,1))
          endif
          write(iunit,*) '   ----------------'
          call pm_dump_tree(context,iunit,cnode_arg(cnode,6),2)
          call pm_dump_tree(context,iunit,cnode_arg(cnode,7),2)
          write(iunit,'(a)') '}'
       case(cnode_is_callsig)
          write(iunit,'(a)') 'sig{'
          do i=1,cnode_numargs(cnode)
             call print_proc_cnode(context,iunit,pm_null_obj,&
                  sig_cache,cnode_arg(cnode,i))
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
                  sig_cache,cnode_arg(cnode,i+1))
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
               sig_cache,cnode)
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

  subroutine print_proc_cnode(context,iunit,rvec,sig_cache,cnode)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: rvec,sig_cache,cnode
    integer:: flags

    write(iunit,'(a)') '  '//&
         trim(pm_name_as_string(context,cnode_get_num(cnode,pr_name)))//&
         merge('.',merge(merge('''','%',cnode_flags_set(cnode,pr_flags,proccall_is_general)),' ',&
         cnode_flags_set(cnode,pr_flags,proccall_is_comm)),cnode_flags_set(cnode,pr_flags,proccall_is_ref))//&
         trim(pm_type_as_string(context,cnode_get_num(cnode,pr_ptype)))//' {'
    
    if(cnode_get_kind(cnode)==cnode_is_builtin) then
       write(iunit,'(a)') '   Builtin '//&
            op_names(cnode_get_num(cnode,bi_opcode))//&
            pm_int_as_string(cnode_get_num(cnode,bi_opcode2))
    else
        write(iunit,'(A,i2,A,i2,A,i2,A,i3,A)') &
            '   [nargs=',&
            cnode_get_num(cnode,pr_nargs),',nret=',cnode_get_num(cnode,pr_nret),&
            ',ncalls=',cnode_get_num(cnode,pr_ncalls),']'
       call print_cblock_cnode(context,iunit,rvec,sig_cache,cnode_get(cnode,pr_cblock),4)
    endif

    write(iunit,'(a)') '  }'
  contains
    include 'fisnull.inc'
  end subroutine print_proc_cnode
  
  recursive subroutine print_cblock_cnode(context,iunit,rvec,sig_cache,cnode,depth)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,depth
    type(pm_ptr),intent(in):: rvec,sig_cache,cnode
    type(pm_ptr)::p
    p=cnode_get(cnode,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       call print_call_cnode(context,iunit,rvec,sig_cache,p,depth)
       p=cnode_get(p,call_link)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine print_cblock_cnode
  
  recursive subroutine print_call_cnode(context,iunit,rvec,sig_cache,cnode,depth)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,depth
    type(pm_ptr),intent(in):: rvec,sig_cache,cnode
    integer:: signo,name,i,j,k,nret,nargs,modl,line
    type(pm_ptr):: p,args,amps,keys,keynames
    character(len=120):: str,location
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
             call append_to_line(iunit,str,i,&
                  '['//trim(pm_int_as_string(k))//'] ',.false.,depth)
          endif
       endif
    elseif(signo==0) then
       str=repeat(' ',depth)//trim(str)//'var-call'
       i=len_trim(str)+1
       call print_value_cnode(context,iunit,rvec,sig_cache,&
            cnode_get(cnode,call_var),depth,str,i)
       call append_to_line(iunit,str,i,': ',.false.,depth)
    else
       p=pm_dict_key(context,sig_cache,&
            int(signo,pm_ln))
       name=p%data%i(p%offset+pm_fast_esize(p))
       if(.not.pm_fast_isnull(cnode_get(cnode,call_var))) then
          str=repeat(' ',depth)//trim(str)//'call *('
          i=depth+7
          call print_value_cnode(context,iunit,rvec,sig_cache,&
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

    args=cnode_get(cnode,call_args)
    nargs=cnode_numargs(args)
    nret=cnode_get_num(cnode,call_nret)
    amps=cnode_get(cnode,call_amp)
    amps=pm_name_val(context,int(amps%offset))
    
    if(nret>0) then
       do j=1,nret
          call print_value_cnode(context,iunit,rvec,sig_cache,cnode_arg(args,j),depth,str,i)
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
       call print_value_cnode(context,iunit,rvec,sig_cache,cnode_arg(args,j),depth,str,i)
       i=i+1
    enddo
    keys=cnode_get(cnode,call_keys)
    if(.not.pm_fast_isnull(keys)) then
       
       keynames=pm_name_val(context,cnode_get_num(cnode,call_key_names))
       do j=1,cnode_numargs(keys)
          call append_to_line(iunit,str,i,&
               trim(pm_name_as_string(context,keynames%data%i(keynames%offset+j-1))),.false.,depth)
          call append_to_line(iunit,str,i,':=:'//trim(pm_name_as_string(context,name))//':',.false.,depth)
          call print_value_cnode(context,iunit,rvec,sig_cache,cnode_arg(keys,j),depth,str,i)
          i=i+1
       enddo
    endif
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
  end subroutine print_call_cnode

  recursive subroutine print_value_cnode(context,iunit,rvec,sig_cache,cnode,depth,str,i)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,depth
    type(pm_ptr),intent(in):: rvec,sig_cache,cnode
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
          name=cnode_get_num(cnode,var_name)
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
          if(cnode_flags_set(cnode,var_flags,var_is_maybe_not_private)) then
             call append_to_line(iunit,str,i,'^',.false.,depth)
          endif
       case(cnode_is_const)
          p=cnode_arg(cnode,1)
          if(pm_fast_vkind(p)==pm_name) p=pm_name_val(context,int(p%offset))
          call append_to_line(iunit,str,i,&
               trim(pm_value_as_string(context,p)),.false.,depth)
       case(cnode_is_cblock)
          call append_to_line(iunit,str,i,'{',.true.,depth)
          call print_cblock_cnode(context,iunit,rvec,sig_cache,cnode,min(50,depth+2))
          str=' '
          str(depth+1:depth+1)='}'
          i=depth+1
       case(cnode_is_changelist)
          call append_to_line(iunit,str,i,'^(',.false.,depth)
          p=cnode_arg(cnode,1)
          do while(.not.pm_fast_isnull(p))
             call print_value_cnode(context,iunit,rvec,sig_cache,p%data%ptr(p%offset),depth,str,i)
             p=p%data%ptr(p%offset+1)
             if(.not.pm_fast_isnull(p)) then
                call append_to_line(iunit,str,i,',',.false.,depth)
             endif
          enddo
          call append_to_line(iunit,str,i,') &(',.false.,depth)
          p=cnode_arg(cnode,2)
          do while(.not.pm_fast_isnull(p))
             call print_value_cnode(context,iunit,rvec,sig_cache,p%data%ptr(p%offset),depth,str,i)
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
       elseif(iand(flags,proccall_is_ref)/=0) then
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
          if(iand(flags,call_dup_result)/=0) then
             call append_to_line(iunit,str,i,'d',.false.,depth)
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

  
end module pm_cnodes
