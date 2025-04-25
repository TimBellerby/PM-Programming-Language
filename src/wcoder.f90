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

!====================================================================
! Generate word-codes for Virtual Machine (if interpreting)
! or Source Generator (if compiling)
!====================================================================
module pm_wcode
  use pm_sysdep
  use pm_compbase
  use pm_kinds
  use pm_memory
  use pm_hash
  use pm_options
  use pm_lib
  use pm_symbol
  use pm_types
  use pm_ast
  use pm_cnodes
  use pm_infer
  implicit none

  logical,parameter:: debug_wcode=.false.
  logical,parameter:: debug_wcode_wc=.false.
  
  integer,parameter:: max_code_size=2**15-1
  integer,parameter:: max_const=2**15-1-pm_max_args
  integer,parameter:: max_costack=2**15-1

  integer,parameter:: max_par_depth=256
  integer,parameter:: max_comm_par_depth=256
  integer,parameter:: max_labels=1024

  !! Keep at 1 until error recovery (esp. mismatch errors) is better
  integer,parameter:: max_wcode_errors=1

  ! Information on coroutines
  type costate
     type(pm_ptr):: p,cblock,rv
     integer:: first_pc,base,num_named,state
     integer:: ve,new_ve
     logical:: break
  end type costate
    
  ! State for wcode code generation stage
  type wcoder
     type(pm_context),pointer:: context
     type(pm_reg),pointer:: reg
     type(pm_ptr):: temp

     ! Constant values
     type(pm_ptr),dimension(max_const):: values
     integer:: nval

     ! Caches for code and signatures
     type(pm_ptr):: code_cache,sig_cache

     ! Program counter
     integer:: pc,last

     ! Variable allocation
     integer,dimension(pm_max_stack):: ref_count
     integer:: nvar,avar,npar,mvar

     ! Supplemental data field - one word per code node index
     integer,dimension(max_code_stack):: rdata
     integer:: base,top,oldbase,xbase,keybase

     ! Word code buffer
     integer:: wc_size
     integer(pm_wc),allocatable,dimension(:):: wc

     ! Coroutines
     integer,dimension(2):: cotop
     type(costate),dimension(2,max_costack):: costack
     integer:: cs
     
     ! Labels
     integer:: labels(1:max_labels)
     integer:: lbtop,lbbase

     ! Current proc
     integer:: loop_extra_arg
     logical:: proc_can_inline

     ! Inlining
     type(pm_ptr):: inline_args,inline_keys,inline_key_names,outer_rv
     logical:: inline_all,inline_none

     ! Compile time types
     integer:: true_name,false_name
     type(pm_ptr):: true_obj,false_obj

     ! Variable information (compiling only)
     integer(pm_wc),dimension(:),allocatable:: vinfo

     ! Copy of polymorphic type cache (compiling only)
     type(pm_ptr):: poly_cache

     ! Last coded instruction (compiling only)
     integer:: last_instr

     ! Vector engines for current parallel context
     integer:: shared_ve

     ! Stack of nested communicating sequential loops (compiling only)
     integer,dimension(max_comm_par_depth):: loop_stack
     integer:: loop_top

     ! Return and parameter values (compiling only)
     integer:: retvar,pvar
     type(pm_ptr):: keys

     ! Set of active types (compiling only)
     type(pm_ptr):: typeset

    ! Debugging info
     integer:: cur_modl,cur_line

     type(code_state),pointer:: coder
   
     integer:: num_errors
  end type wcoder

  
  integer,private,parameter:: pm_ve_type=-99

contains

  !====================================================
  ! Initialise wcode-stage control structure
  !====================================================
  subroutine init_wcoder(context,wcd,sig_cache,poly_cache)
    type(pm_context),pointer:: context
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: sig_cache,poly_cache
    type(pm_ptr):: true,false

    wcd%context=>context
    wcd%reg=>pm_register(context,'wcd',wcd%temp,&
         wcd%code_cache,wcd%sig_cache,wcd%poly_cache,&
         wcd%true_obj,wcd%false_obj,wcd%keys)
    wcd%code_cache=pm_dict_new(context,32_pm_ln)
    wcd%sig_cache=sig_cache
    wcd%poly_cache=poly_cache
    wcd%cs=1
    wcd%cotop=0
    wcd%true_obj=pm_new_small(context,pm_logical,1_pm_p)
    wcd%true_obj%data%l(wcd%true_obj%offset)=.true.
    wcd%false_obj=pm_new_small(context,pm_logical,1_pm_p)
    wcd%false_obj%data%l(wcd%false_obj%offset)=.false.
    wcd%true_name=pm_new_fix_value_type(wcd%context,wcd%true_obj)
    wcd%false_name=pm_new_fix_value_type(wcd%context,wcd%false_obj)
    if(pm_is_compiling) then
       wcd%typeset=pm_set_new(wcd%context,32_pm_ln)
    endif
    wcd%inline_args=pm_null_obj
    wcd%inline_keys=pm_null_obj
    wcd%inline_key_names=pm_null_obj
  end subroutine init_wcoder

  !====================================================
  ! Delete wcode stage control structure
  !====================================================
  subroutine term_wcoder(wcd)
    type(wcoder):: wcd
    call pm_delete_register(wcd%context,wcd%reg)
  end subroutine term_wcoder

  !====================================================
  ! Wcode main program
  !====================================================
  subroutine wcode_prog(wcd,p)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: p
    type(pm_ptr):: cblock,rv
    integer:: ve
    integer,dimension(1):: key
    integer:: junk,init_ve
    logical:: break
    key(1)=0
    junk=pm_idict_add(wcd%context,wcd%code_cache,&
         key,1,pm_null_obj)
    if(debug_wcode) then
       write (*,*) 'WCODE PROG>'
    endif
    call init_wcode_proc(wcd,p)
    cblock=cnode_arg(p,1)
    rv=cnode_arg(p,2)
    wcd%base=0
    wcd%top=pm_fast_esize(rv)+1
    init_ve=merge(0,pm_stack_nullve,pm_is_compiling)
    wcd%proc_can_inline=.true.
    ve=init_ve
    wcd%shared_ve=ve
    break=wcode_cblock(wcd,cblock,rv,ve)
    if(pm_is_compiling) then
       call make_proc_code_comp(wcd,1_pm_ln,sym_pm_system,&
            0,pm_fast_tinyint(wcd%context,proc_is_impure),&
            pm_null_obj,ve)
    else
       call make_proc_code(wcd,1_pm_ln,sym_pm_system,ve)
    endif
    if(debug_wcode) then
       write(*,*) 'WCODE PROG COMPLETE>'
    endif
  contains
    include 'fesize.inc'
    include 'ftiny.inc'
  end subroutine wcode_prog

  !====================================================
  ! Finise procedure definitions
  !====================================================
  subroutine wcode_procs(wcd)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr):: proc,pr,rv,cblock,p,p2,tv,taints,keys
    integer:: ve,k
    integer(pm_ln):: i,j,n
    integer:: nret,vev
    vev=0
    wcd%base=0
    i=2
    do while(i<=pm_dict_size(wcd%context,wcd%code_cache))
       p=pm_dict_key(wcd%context,wcd%code_cache,i)
       n=p%data%i(p%offset)
       if(pm_is_compiling) vev=pm_fast_esize(p)
       proc=pm_dict_val(wcd%context,wcd%sig_cache,n)
       call init_wcode_proc(wcd,proc)
       pr=cnode_arg(proc,1)
       rv=cnode_arg(proc,2)
       taints=cnode_arg(proc,3)
       !keys=cnode_arg(proc,4)
       if(pm_is_compiling) then
          ve=0
       else
          ve=alloc_var(wcd,pm_ve_type)
       endif
       wcd%loop_extra_arg=iand(cnode_get_num(pr,pr_flags),proccall_is_comm)
       wcd%proc_can_inline=cnode_flags_clear(proc,&
            cnode_args+2,proc_is_not_inlinable)
       wcd%npar=cnode_get_num(pr,pr_nret)+wcd%loop_extra_arg
       if(pm_is_compiling) then
          if(rv%data%i(rv%offset)==-1) then
             wcd%retvar=alloc_result_var(wcd,int(pm_null))
          else
             wcd%retvar=alloc_result_var(wcd,rv%data%i(rv%offset))
          endif
          nret=wcd%nvar
!!$          if(.not.pm_fast_isnull(keys)) then
!!$             p2=pm_fast_newnc(wcd%context,pm_int,&
!!$                  int(pm_fast_esize(keys)+1))
!!$             wcd%keys=p2
!!$             do j=0,pm_fast_esize(keys)
!!$                p2%data%i(p2%offset+j)=&
!!$                     alloc_key_var(wcd,keys%data%i(keys%offset+j))
!!$             enddo
!!$          endif
          if(wcd%loop_extra_arg/=0) then
             if(vev>0) then
                wcd%shared_ve=cvar_alloc_entry(wcd,v_is_parve,0,0,int(pm_logical))
             else
                wcd%shared_ve=ve
             endif
          endif
       else
          wcd%nvar=wcd%npar+1
          wcd%avar=wcd%npar+1
          wcd%ref_count(1:wcd%nvar)=1
          if(wcd%loop_extra_arg/=0) then
             wcd%shared_ve=pm_stack_locals+1
          else
             wcd%shared_ve=ve
          endif
       endif
       if(debug_wcode) then
          write(*,*) 'WCODE PROC> #',i,'SIGNO>',n,'VE>',ve,&
               'NRET>',wcd%npar,'NVAR>',wcd%nvar,&
               'CAN INLINE> ',wcd%proc_can_inline,&
               'EXTRA>',wcd%loop_extra_arg
       endif
       cblock=cnode_get(pr,pr_cblock)
       call wcode_proc_body(wcd,cblock,rv,ve)
       call release_var(wcd,ve)
       if(pm_is_compiling) then
          call make_proc_code_comp(wcd,i,&
               cnode_get_num(pr,pr_name),&
               nret,taints,wcd%keys,ve)
       else
          call make_proc_code(wcd,i,&
               cnode_get_num(pr,pr_name),ve)
       endif
       i=i+1
    end do
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fnewnc.inc'
  end subroutine  wcode_procs

  !====================================================
  ! Initialise wcoder state at start of proc
  !====================================================
  subroutine init_wcode_proc(wcd,proc)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: proc
    wcd%pc=1
    wcd%last=max_code_size
    wcd%nval=0
    wcd%nvar=0
    wcd%mvar=0
    wcd%avar=0
    wcd%npar=0
    wcd%base=0
    wcd%top=pm_fast_esize(cnode_arg(proc,2))+1
    wcd%xbase=wcd%top
    wcd%rdata(1:wcd%top)=-9999
    wcd%loop_extra_arg=0
    wcd%cur_modl=-1
    wcd%cur_line=-1
    if(pm_is_compiling) then
       wcd%mvar=pm_max_stack
       if(allocated(wcd%vinfo)) deallocate(wcd%vinfo)
       allocate(wcd%vinfo(wcd%mvar))
    endif
    wcd%wc_size=max_code_size
    if(allocated(wcd%wc)) deallocate(wcd%wc)
    allocate(wcd%wc(max_code_size))
    wcd%last_instr=0
    wcd%loop_top=0
    wcd%inline_all=.false.
    wcd%inline_none=.false.
    wcd%retvar=-1
    wcd%pvar=-1
    wcd%shared_ve=0
    wcd%keys=pm_null_obj
  contains
    include 'fesize.inc'
  end subroutine init_wcode_proc

  !====================================================
  ! Make proc object
  !====================================================
  subroutine make_proc_code(wcd,i,name,ve)
    type(wcoder),intent(inout):: wcd
    integer(pm_ln),intent(in):: i
    integer,intent(in):: name,ve
    integer:: n,m,vs,j,k
    type(pm_ptr):: p,p2
    if(debug_wcode) then
       write(*,*) 'MAKE PROC CODE>',i,&
            trim(pm_name_as_string(wcd%context,name))
    endif
    call wc(wcd,op_return)
    call wc(wcd,0)
    call wc(wcd,1)
    call wc(wcd,ve)
    call tidy_up(wcd)
    n=wcd%nval
    m=wcd%wc_size-wcd%last
    wcd%temp=pm_fast_new(wcd%context,pm_pointer,int(n+2,pm_p))
    p=wcd%temp
    call pm_ptr_assign(wcd%context,&
         pm_dict_vals(wcd%context,wcd%code_cache),i-1,p)
    
    p2=pm_assign_new(wcd%context,p,&
         0_pm_ln,pm_int16,int(wcd%pc+2,pm_ln),.false.)
    p2%data%i16(p2%offset)=wcd%mvar+pm_stack_locals ! Required stack size
    p2%data%i16(p2%offset+1)=wcd%npar+1
    p2%data%i16(p2%offset+2)=name
    p2%data%i16(p2%offset+3:p2%offset+wcd%pc+1)=wcd%wc(1:wcd%pc-1)
    p2=pm_assign_new(wcd%context,wcd%temp,&
         1_pm_ln,pm_int16,int(max(m,1),pm_ln),.false.)
    p2%data%i16(p2%offset:p2%offset+m-1)=wcd%wc(wcd%last+1:wcd%wc_size)
    
    if(n>0) then
       p=wcd%temp
       p%data%ptr(p%offset+2:p%offset+n+1)=&
            wcd%values(1:n)
    endif
  contains
    include 'fvkind.inc'
    include 'fnew.inc'
    include 'fesize.inc'
  end subroutine make_proc_code
  
  !====================================================
  ! Make proc object (compiler version)
  ! - wcode vars taints keys values...
  ! - wcode is retvar, pvar, name, shared_ve, wcodes...
  !====================================================
  subroutine make_proc_code_comp(wcd,i,name,nret,taints,keys,ve)
    type(wcoder),intent(inout):: wcd
    integer(pm_ln),intent(in):: i
    integer,intent(in):: name,ve
    integer,intent(in):: nret
    type(pm_ptr),intent(in):: taints,keys
    integer:: n,m,vs,j,k
    type(pm_ptr):: p,p2
    if(debug_wcode) then
       write(*,*) 'MAKE PROC CODE  COMP>',i,&
            trim(pm_name_as_string(wcd%context,name))
    endif
    !call comp_tidy_up(wcd)
    n=wcd%nval
    m=wcd%wc_size-wcd%last
    wcd%temp=pm_fast_new(wcd%context,pm_pointer,int(n+4,pm_p))
    p=wcd%temp
    p%data%ptr(p%offset+2)=taints
    p%data%ptr(p%offset+3)=keys
 
    call pm_ptr_assign(wcd%context,&
         pm_dict_vals(wcd%context,wcd%code_cache),i-1,p)
    
    p2=pm_assign_new(wcd%context,p,&
         0_pm_ln,pm_int,int(wcd%pc+3,pm_ln),.false.)
    p2%data%i(p2%offset)=wcd%retvar
    p2%data%i(p2%offset+1)=wcd%pvar
    p2%data%i(p2%offset+2)=name
    p2%data%i(p2%offset+3)=wcd%shared_ve
    p2%data%i(p2%offset+4:p2%offset+wcd%pc+2)=wcd%wc(1:wcd%pc-1)
    p2=pm_assign_new(wcd%context,wcd%temp,&
         1_pm_ln,pm_int,int(max(1,wcd%nvar),pm_ln),.false.)
    p2%data%i(p2%offset:p2%offset+wcd%nvar-1)=wcd%vinfo(1:wcd%nvar)
    if(n>0) then
       p=wcd%temp
       p%data%ptr(p%offset+4:p%offset+n+3)=&
            wcd%values(1:n)
    endif
  contains
    include 'fvkind.inc'
    include 'fnew.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine make_proc_code_comp

  !========================================
  ! Wcode a call block
  !========================================
  subroutine wcode_proc_body(wcd,cblock,rv,ve)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: cblock,rv
    integer,intent(in):: ve
    type(pm_ptr):: p,last
    integer:: par,num_named,first_pc
    logical:: break

    first_pc=wcd%pc

    ! Allocate parameter variables
    par=wcode_pars(wcd,cblock,rv,ve,p)
    
    ! Allocate multiple-use variables
    num_named=wcode_mvars(wcd,cblock,rv,ve,p)

    ! Process calls
    p=cnode_get(cblock,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       break=wcode_call(wcd,p,rv,ve,.false.)
       p=cnode_get(p,call_link)
    enddo

    ! Close variables
    if(.not.pm_is_compiling) then
       call close_vars(wcd,cblock,rv,ve,first_pc,num_named+par)
    endif

  contains
    include 'fisnull.inc'
  end subroutine wcode_proc_body

  !========================================
  ! Wcode parameter list
  !========================================
  function wcode_pars(wcd,cblock,rv,ve,pp) result(npar)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: cblock,rv
    integer,intent(in):: ve
    type(pm_ptr),intent(out):: pp
    integer:: npar
    type(pm_ptr):: p,tv
    integer:: slot,i
    integer:: v,xpar
    integer:: typ
    logical:: isref,isshared
    npar=wcd%npar
    p=cnode_get(cblock,cblock_first_var)
    if(.not.pm_fast_isnull(p)) then
!!$       write(*,*) 'ALLOCATING PARAM maybe>',&
!!$                  trim(pm_name_as_string(wcd%context,cnode_get_name(p,var_name))),&
!!$                  cnode_flags_set(p,var_flags,var_is_param)
       do while(cnode_flags_set(p,var_flags,var_is_param))
          slot=cnode_get_num(p,var_index)
          typ=get_var_type(wcd,p,rv)
          isref=cnode_flags_set(p,var_flags,var_is_ref)
          if(debug_wcode) then
             write(*,*) 'ALLOCATING PARAM>',&
                  trim(pm_name_as_string(wcd%context,cnode_get_name(p,var_name)))
          endif
          wcd%rdata(slot+wcd%base)=alloc_param_var(wcd,&
               typ,isref,.false.,cnode_get_num(p,var_name))
          if(debug_wcode) write(*,*) 'TO>',wcd%rdata(slot+wcd%base)
          
          npar=npar+1
          p=cnode_get(p,var_link)
          if(pm_fast_isnull(p)) exit
       enddo
       if(.not.pm_fast_isnull(p)) then
          if(cnode_flags_set(p,var_flags,var_is_varg)) then
             typ=get_var_type(wcd,p,rv)
             tv=pm_type_vect(wcd%context,typ)
             if(pm_tv_kind(tv)==pm_type_is_tuple) then
                xpar=pm_tv_numargs(tv)
                do i=1,xpar
                   typ=pm_tv_arg(tv,i)
                   slot=alloc_param_var(wcd,typ,.false.,.false.,0)
                   if(typ/=pm_tiny_int) then
                      wcd%top=wcd%top+1
                      wcd%rdata(wcd%top)=slot
                   endif
                enddo
                if(pm_is_compiling) then
                   v=cvar_alloc_slots(wcd,3+xpar)
                   do i=1,xpar
                      call cvar_set_ptr(wcd,v,i,wcd%rdata(wcd%top-xpar+i))
                   enddo
                   call cvar_set_info(wcd,v,v_is_group,xpar,v_is_tuple,0)
                   wcd%rdata(wcd%base+cnode_get_num(p,var_index))=v
                endif
                npar=npar+xpar
             else
                slot=alloc_param_var(wcd,typ,.false.,.false.,0)
                if(typ/=pm_tiny_int) then
                   wcd%top=wcd%top+1
                   wcd%rdata(wcd%top)=slot
                endif
                if(pm_is_compiling) then
                   wcd%rdata(wcd%base+cnode_get_num(p,var_index))=slot
                endif
                npar=npar+1
             endif
             p=cnode_get(p,var_link)
          endif
       endif
    endif
    if(pm_is_compiling) then
       wcd%npar=wcd%nvar
    else
       wcd%npar=npar
    endif
    pp=p
  contains
    include 'fisnull.inc'
  end function wcode_pars
  
  !========================================
  ! Wcode a call block
  !========================================
  function wcode_cblock(wcd,cblock,rv,ve) result(break)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: cblock,rv
    integer,intent(in):: ve
    logical:: break
    type(pm_ptr):: p
    integer:: num_named,first_pc

    ! Start block
    break=.false.
    first_pc=wcd%pc
    num_named=wcode_mvars(wcd,cblock,rv,ve)
    p=cnode_get(cblock,cblock_first_call)
    
    ! Process calls
    do while(.not.pm_fast_isnull(p))
       break=wcode_call(wcd,p,rv,ve,.false.)
       if(break) then
          if(debug_wcode) write(*,*) 'BREAKOUT>'
          call push_costate(wcd,cblock,p,first_pc,&
               num_named,rv,ve)
          return
       endif
       p=cnode_get(p,call_link)
    enddo

    ! Close variables
    if(.not.pm_is_compiling) then
       call close_vars(wcd,cblock,rv,ve,first_pc,num_named)
    endif
  contains
    include 'fisnull.inc'
  end function  wcode_cblock

  !========================================
  ! Push costate
  !========================================
  subroutine push_costate(wcd,cblock,p,first_pc,num_named,rv,ve)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: cblock,rv,p
    integer,intent(in):: ve
    integer,intent(in):: first_pc,num_named
    integer:: cs,top
    integer:: sig
    cs=wcd%cs
    top=wcd%cotop(cs)+1
    wcd%cotop(cs)=top
    if(top>max_costack) &
         call pm_panic('Program too complex')
    wcd%costack(cs,top)%cblock=cblock
    wcd%costack(cs,top)%p=p
    wcd%costack(cs,top)%first_pc=first_pc
    wcd%costack(cs,top)%num_named=num_named
    wcd%costack(cs,top)%base=wcd%base
    wcd%costack(cs,top)%rv=rv
    wcd%costack(cs,top)%ve=ve
    wcd%costack(cs,top)%state=0
  end subroutine push_costate
  

  !========================================
  ! Continue block where left off
  !========================================
  recursive function restart_cblock(wcd,ve) result(break)
    type(wcoder):: wcd
    integer,intent(out):: ve
    type(pm_ptr):: cblock,rv
    logical:: break
    type(pm_ptr):: p
    integer:: num_named,first_pc,cs,top,save_base
    logical:: restart

    ! Pop state
    cs=3-wcd%cs
    top=wcd%cotop(cs)
    if(top<1) then
       write(*,*) 'cs=',cs,'top=',top
       call pm_panic('restart cblock')
    endif
    cblock=wcd%costack(cs,top)%cblock
    p=wcd%costack(cs,top)%p
    if(debug_wcode) then
       write(*,*) 'RESTART:',top,sym_names(max(0,-cnode_get_num(p,call_sig)))
    endif
    first_pc=wcd%costack(cs,top)%first_pc
    num_named=wcd%costack(cs,top)%num_named
    save_base=wcd%base
    wcd%base=wcd%costack(cs,top)%base
    rv=wcd%costack(cs,top)%rv
    ve=wcd%costack(cs,top)%ve
    wcd%cotop(cs)=top-1
    
    ! Process calls
    restart=.true.
    do while(.not.pm_fast_isnull(p))
       break=wcode_call(wcd,p,rv,ve,restart)
       if(break) then
          call push_costate(wcd,cblock,p,first_pc,&
               num_named,rv,ve)
          wcd%base=save_base
          return
       endif
       p=cnode_get(p,call_link)
       restart=.false.
    enddo
    
    ! Close variables
    if(.not.pm_is_compiling) then
       call close_vars(wcd,cblock,rv,ve,first_pc,num_named)
    endif

    wcd%base=save_base
  contains
    include 'fisnull.inc'
  end function restart_cblock

  !========================================
  ! Wcode multiple-use variables
  !========================================
  function wcode_mvars(wcd,cblock,rv,ve,pp) result(num_named)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: cblock,rv
    integer,intent(in):: ve
    type(pm_ptr),intent(in),optional:: pp
    integer:: num_named,slot
    type(pm_ptr):: p,nam
    integer:: typ
    num_named=0
    if(present(pp)) then
       p=pp
    else
       p=cnode_get(cblock,cblock_first_var)
    endif
    do while(.not.pm_fast_isnull(p))
       if(arg_is_mvar(p).or.pm_is_compiling) then
          slot=cnode_get_num(p,var_index)
          nam=cnode_get(p,var_name)
          wcd%rdata(slot+wcd%base)=alloc_general_var(wcd,p,rv)
          if(cnode_get_num(p,var_name)/=0.or.pm_is_compiling) &
               num_named=num_named+1
       endif
       p=cnode_get(p,var_link)
    enddo
  contains
    include 'fisnull.inc'
  end function wcode_mvars

  !========================================
  ! Close variables defined in a call block
  !========================================
  subroutine close_vars(wcd,cblock,rv,ve,first_pc,nvars,pp)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: cblock,rv
    integer,intent(in):: ve
    integer,intent(in):: first_pc,nvars
    type(pm_ptr),optional,intent(in):: pp
    type(pm_ptr):: p
    integer:: slot,j
    integer:: name

    ! Info entry for parameters & named multi-use variables
    if(nvars>0) then
       if(wcd%last-4-nvars*2<=wcd%pc) call expand_wc(wcd)
       wcd%wc(wcd%last)=0
       wcd%wc(wcd%last-1)=nvars
       wcd%wc(wcd%last-2)=first_pc
       wcd%wc(wcd%last-3)=wcd%pc-1
       wcd%last=wcd%last-4-nvars*2
    endif

    ! Release multi-use variables
    j=1
    if(present(pp)) then
       p=pp
    else
       p=cnode_get(cblock,cblock_first_var)
    endif
    do while(.not.pm_fast_isnull(p))
       if(arg_is_mvar(p).or.cnode_flags_set(p,var_flags,var_is_param)) then
          slot=cnode_get_num(p,var_index)
          if(.not.pm_is_compiling) then
             call release_var(wcd,wcd%rdata(slot+wcd%base))
             name=cnode_get_num(p,var_name)
             if(name/=0) then
                wcd%wc(wcd%last+j*2)=name
                wcd%wc(wcd%last+j*2-1)=wcd%rdata(slot+wcd%base)
                j=j+1
             endif
          endif
       endif
       p=cnode_get(p,var_link)
    enddo
    
  contains
    include 'fisnull.inc'
  end subroutine close_vars

  !==========================================================
  ! Wcode a call node (which includes control structures)
  !==========================================================
  recursive function wcode_call(wcd,callnode,rv,ve,restart) result(break)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,rv
    integer,intent(in):: ve
    logical,intent(in):: restart
    logical:: break

    type(pm_ptr):: args
    integer:: nargs,totargs,nkeys,nret
    integer:: costart,cs,save_xbase,save_top,save_lbtop
    integer(pm_p):: m
    integer:: i,j,k,opr,tk,name,name2,new_ve,new_ve2,sig
    integer(pm_ln):: siz
    type(pm_ptr):: arg,u,v,tv
    logical:: varg,ok,break2,save_inline_none
    integer:: typ,pc,jmp,tno,idx,n,ii,kk,slot,slot1,slot2,slot3

    if(pm_debug_checks) then
       if(cnode_get_kind(callnode)/=cnode_is_call) &
            call pm_panic('Wcode call')
    endif

    break=.false.

    args=cnode_get(callnode,call_args)
    nargs=cnode_numargs(args)
    nret=cnode_get_num(callnode,call_nret)
    sig=-cnode_get_num(callnode,call_sig)
    new_ve=-1

    if(debug_wcode) then
       if(sig>0) then 
          write(*,*) 'WCODE CALL>',sym_names(sig)
       else
          write(*,*) 'WCODE CALL> sig=',-sig,'resolv=',&
               rvv(int(cnode_get_num(callnode,call_index)))
       endif
    endif
    select case(sig) 
    case(sym_if)
       tno=check_arg_type(wcd,args,rv,1)
       if(tno==wcd%true_name) then
          if(restart) then
             break=restart_cblock(wcd,new_ve)
          else
             break=wcode_cblock(wcd,cnode_arg(args,2),rv,ve)
          endif
          return
       elseif(tno==wcd%false_name) then
          if(.not.pm_fast_isnull(cnode_arg(args,3))) then
             if(restart) then
                break=restart_cblock(wcd,new_ve)
             else
                break=wcode_cblock(wcd,cnode_arg(args,3),rv,ve)
             endif
          endif
          return
       endif
       if(restart) then
          break=restart_cblock(wcd,new_ve)
          arg=cnode_arg(args,3)
          if(.not.pm_fast_isnull(arg)) then
             if(restart_cblock(wcd,new_ve2).neqv.break) then
                call wcode_error(wcd,callnode,&
                     'Communicating operations do not match'//&
                     ' in different branches of "if"/"switch"')
             endif
             call release_var(wcd,new_ve2)
          endif
          if(break) return
          call release_var(wcd,new_ve)
       else
          if(pm_is_compiling) then
             new_ve=cvar_alloc_ve(wcd,ve,0)
          else
             new_ve=alloc_var(wcd,pm_ve_type)
          endif
          call wc_call(wcd,callnode,op_and_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_sarg(wcd,cnode_arg(args,1),&
               pm_fast_isnull(cnode_arg(args,3)),rv,ve)
          k=wcd%pc
          break2=wcode_cblock(wcd,cnode_arg(args,2),rv,new_ve)
          arg=cnode_arg(args,3)
          if(.not.pm_fast_isnull(arg)) then
             if(pm_is_compiling) then
                new_ve2=cvar_alloc_ve(wcd,ve,new_ve)
             else
                if(break2) then
                   new_ve2=alloc_var(wcd,pm_ve_type)
                else
                   new_ve2=new_ve
                endif
                call wc_call(wcd,callnode,op_andnot_ve,0,3,1,ve)
                call wc(wcd,-new_ve2)
                call wc_arg(wcd,cnode_arg(args,1),.false.,rv,ve)
             endif
             k=wcd%pc
             if(wcode_cblock(wcd,arg,rv,new_ve2).neqv.break2) then
                call wcode_error(wcd,callnode,&
                     'Communicating operations do not match '//&
                     'in different branches of "if"/"switch"')
             endif
             if(.not.break2) call release_var(wcd,new_ve2)
             if(break2) then
                break=.true.
                return
             endif
          else
             if(break2) then
                break=.true.
                return
             endif
          endif
          call release_var(wcd,new_ve)
       endif
    case(sym_if_invar)
       tno=check_arg_type(wcd,args,rv,1)
       if(pm_is_compiling) then
          new_ve=0
          if(tno==wcd%true_name) then
             call wcode_comm_block(wcd,cnode_arg(args,2),&
                  wcd%shared_ve,rv,new_ve)
          elseif(tno==wcd%false_name) then
             if(.not.pm_fast_isnull(cnode_arg(args,3))) then
                call wcode_comm_block(wcd,cnode_arg(args,3),&
                     wcd%shared_ve,rv,new_ve)
             endif
          else
             call wc_call(wcd,callnode,op_if,0,4,0,ve)
             pc=comp_start_if_else_block(wcd)
             call wc_arg(wcd,cnode_arg(args,1),.false.,rv,ve)
             call wcode_comm_block(wcd,cnode_arg(args,2),&
                  wcd%shared_ve,rv,new_ve)
             if(.not.pm_fast_isnull(cnode_arg(args,3))) then
                call comp_start_else_block(wcd,pc)
                call wcode_comm_block(wcd,cnode_arg(args,3),&
                     wcd%shared_ve,rv,new_ve)
                call comp_finish_else_block(wcd,pc)
             else
                call comp_finish_block(wcd,pc)
             endif
          endif
          return
       endif
       if(tno/=wcd%false_name) then
          new_ve=alloc_var(wcd,pm_ve_type)
          call wc_call(wcd,callnode,op_and_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_sarg(wcd,cnode_arg(args,1),&
               pm_fast_isnull(cnode_arg(args,3)),rv,ve)
          jmp=wc_jump_call(wcd,callnode,op_skip_comms,0,1,new_ve)
          call wcode_comm_block(wcd,cnode_arg(args,2),&
               wcd%shared_ve,rv,new_ve)
          call set_jump_to_here(wcd,jmp)
       endif
       if(tno/=wcd%true_name) then
          arg=cnode_arg(args,3)
          if(.not.pm_fast_isnull(arg)) then
             call wc_call(wcd,callnode,op_andnot_ve,0,3,1,ve)
             call wc(wcd,-new_ve)
             call wc_arg(wcd,cnode_arg(args,1),.false.,rv,ve)
             jmp=wc_jump_call(wcd,callnode,op_skip_comms,0,1,new_ve)
             call wcode_comm_block(wcd,cnode_arg(args,3),&
                  wcd%shared_ve,rv,new_ve)
             call set_jump_to_here(wcd,jmp)
          endif
          call release_var(wcd,new_ve)
       endif
    case(sym_while,sym_while_invar)
       tno=check_arg_type(wcd,args,rv,2)
       if(tno==wcd%false_name) return
       if(restart) return
       if(cblock_has_comm(cnode_arg(args,1))&
            .or.cblock_has_comm(cnode_arg(args,3))) then
          break=.true.
          return
       endif
       if(pm_is_compiling) then
          new_ve=alloc_var(wcd,int(pm_logical))
          break2=wcode_cblock(wcd,cnode_arg(args,1),rv,ve)
          call wc_call(wcd,callnode,op_assign,111,3,0,ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,2),.false.,rv,ve)
          call wc_call(wcd,callnode,op_loop,0,3,0,ve)
          pc=comp_start_block(wcd)
          call wc(wcd,-new_ve)
          break2=wcode_cblock(wcd,cnode_arg(args,3),rv,0)
          break2=wcode_cblock(wcd,cnode_arg(args,1),rv,0)
          call wc_call(wcd,callnode,op_assign,111,3,0,0)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,2),.false.,rv,ve)
          call comp_finish_block(wcd,pc)
       else
          new_ve=alloc_var(wcd,pm_ve_type)
          call wc_call(wcd,callnode,op_clone_ve,int(new_ve),1,1,ve)
          jmp=wc_jump_call(wcd,callnode,op_jmp,0,1,ve)
          pc=wcd%pc
          break2=wcode_cblock(wcd,cnode_arg(args,3),rv,new_ve)
          call set_jump_to_here(wcd,jmp)
          break2=wcode_cblock(wcd,cnode_arg(args,1),rv,new_ve)
          call wc_call(wcd,callnode,op_and_jmp_any,&
               pc,3,1,new_ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,2),.false.,rv,ve)
          call release_var(wcd,new_ve)
       endif
    case(sym_until,sym_until_invar)
       if(restart) return
       if(cblock_has_comm(cnode_arg(args,1))) then
          break=.true.
          return
       endif
       if(pm_is_compiling) then
          new_ve=alloc_var(wcd,int(pm_logical))
          call wc_call(wcd,callnode,op_assign,111,3,0,ve)
          call wc(wcd,-new_ve)
          call wc(wcd,cvar_const_value(wcd,wcd%true_obj))
          call wc_call(wcd,callnode,op_loop,0,3,0,ve)
          pc=comp_start_block(wcd)
          call wc(wcd,-new_ve)
          break2=wcode_cblock(wcd,cnode_arg(args,1),rv,0)
          call wc_call(wcd,callnode,op_not,111,3,1,0)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,2),.false.,rv,ve)
          call comp_finish_block(wcd,pc)
       else
          new_ve=alloc_var(wcd,pm_ve_type)
          call wc_call(wcd,callnode,op_clone_ve,int(new_ve),1,1,ve)
          pc=wcd%pc
          break2=wcode_cblock(wcd,cnode_arg(args,1),rv,new_ve)
          call wc_call(wcd,callnode,op_andnot_jmp_any,&
               pc,3,1,new_ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,2),.false.,rv,new_ve)
          call release_var(wcd,new_ve)
       endif
    case(sym_over)
       call wc_call(wcd,callnode,op_over,0,2,0,ve)
       pc=comp_start_block(wcd)
       new_ve=ve
       break2=wcode_cblock(wcd,cnode_arg(args,1),rv,new_ve)
       if(restart) then
          break=restart_cblock(wcd,new_ve)
       else
          break=wcode_cblock(wcd,cnode_arg(args,2),rv,new_ve)
       endif
       call comp_finish_block(wcd,pc)
    case(sym_do)
       if(restart) then
          break=restart_cblock(wcd,new_ve)
       else
          break=wcode_cblock(wcd,cnode_arg(args,1),rv,ve)
       endif
    case(sym_sync,sym_colon)
       break=.not.restart
       return
    case(sym_for,sym_also)
       call wcode_comm_block(wcd,cnode_arg(args,1),&
            wcd%shared_ve,rv,ve)
    case(sym_pct)
       call wcode_comm_block(wcd,cnode_arg(args,2),&
            wcd%shared_ve,rv,ve)
    case(sym_hash)
       if(check_arg_type(wcd,args,rv,2)/=pm_null) then
          break=wcode_cblock(wcd,cnode_arg(args,1),rv,ve)
       endif
    case(sym_pm_send:sym_pm_serve)
       tno=check_arg_type(wcd,args,rv,7)
       if(pm_is_compiling) then
          save_inline_none=wcd%inline_none
          wcd%inline_none=.false. !.true.
          call comp_link_dref(wcd,cnode_arg(args,2),cnode_arg(args,4))
          call wc_call(wcd,callnode,&
               merge(merge(op_remote_call,op_remote_send_call,sig==sym_pm_recv),&
               merge(op_collect_call,op_server_call,sig==sym_pm_collect),&
               sig<=sym_pm_recv),&
               merge(1,0,tno==wcd%true_name),9,4,ve)
          new_ve=0
          pc=comp_start_if_else_block(wcd)
          call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)     ! p-from
          call wc_arg(wcd,cnode_arg(args,2),.true.,rv,ve)     ! x-recv
          call wc_arg(wcd,cnode_arg(args,3),.true.,rv,ve)     ! y-recv
          call wc_arg(wcd,cnode_arg(args,4),.true.,rv,ve)     ! x-send
          call wc_arg(wcd,cnode_arg(args,5),.false.,rv,ve)    ! p-send
          call wc_arg(wcd,cnode_arg(args,6),.false.,rv,ve)    ! y-(re)send
          break2=wcode_cblock(wcd,cnode_arg(args,8),rv,new_ve)
          call comp_start_else_block(wcd,pc)
          call arg_set_slot(wcd,cnode_arg(args,2),&
               arg_slot(wcd,cnode_arg(args,4)))
          if(sig==sym_pm_serve.or.sig==sym_pm_recv) then
             call arg_set_slot(wcd,cnode_arg(args,6),&
                  arg_slot(wcd,cnode_arg(args,3)))
          else
             call arg_set_slot(wcd,cnode_arg(args,3),&
                  arg_slot(wcd,cnode_arg(args,6)))
          endif
          break2=wcode_cblock(wcd,cnode_arg(args,8),rv,new_ve)
          call comp_finish_else_block(wcd,pc)
          wcd%inline_none=save_inline_none
       else
          call wc_call(wcd,callnode,&
               merge(merge(op_remote_call,op_remote_send_call,sig==sym_pm_recv),&
               merge(op_collect_call,op_server_call,sig==sym_pm_collect),&
               sig<=sym_pm_recv),&
               merge(1,0,tno==wcd%true_name),&
               8,3,ve)
          new_ve=alloc_var(wcd,pm_ve_type)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)     ! p-from
          call wc_arg(wcd,cnode_arg(args,2),.true.,rv,ve)     ! x-recv 
          call wc_arg(wcd,cnode_arg(args,3),.true.,rv,ve)     ! y-recv
          call wc_arg(wcd,cnode_arg(args,4),.false.,rv,ve)    ! x-send
          call wc_arg(wcd,cnode_arg(args,5),.false.,rv,ve)    ! p-send
          call wc_arg(wcd,cnode_arg(args,6),.false.,rv,ve)    ! y-(re)send
          if((sig==sym_pm_recv.or.sig==sym_pm_serve)) then
             wcd%wc(wcd%pc-1)=-wcd%wc(wcd%pc-1)
          endif
          slot=wc_jump_call(wcd,callnode,op_jmp,0,1,ve)
          break2=wcode_cblock(wcd,cnode_arg(args,8),rv,new_ve)
          call wc_call(wcd,callnode,op_par_loop_end,0,1,0,ve)
          call set_jump_to_here(wcd,slot)
          call release_var(wcd,new_ve)
       endif
    case(sym_pm_bcast)
       call wc_call(wcd,callnode,op_bcast_call,0,7,2,ve)
       if(pm_is_compiling) then
          call comp_link_dref(wcd,cnode_arg(args,1),cnode_arg(args,3))
          new_ve=0
          pc=comp_start_block(wcd)
       else
          new_ve=alloc_var(wcd,pm_ve_type)
          call wc(wcd,-new_ve)
       endif
       call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)     ! x-new
       call wc_arg(wcd,cnode_arg(args,2),.true.,rv,ve)     ! y-new
       call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)    ! x
       call wc_arg(wcd,cnode_arg(args,4),.false.,rv,ve)    ! y
       call wc_arg(wcd,cnode_arg(args,5),.false.,rv,ve)    ! p
       break2=wcode_cblock(wcd,cnode_arg(args,6),rv,new_ve)
       if(pm_is_compiling) call comp_finish_block(wcd,pc)
       call release_var(wcd,new_ve)
    case(sym_pm_recv_req)
       call wc_call(wcd,callnode,op_recv_req_call,0,5,2,ve)
       if(pm_is_compiling) then
          call comp_link_dref(wcd,cnode_arg(args,2),cnode_arg(args,3))
          new_ve=0
          pc=comp_start_block(wcd)
       else
          new_ve=alloc_var(wcd,pm_ve_type)
          call wc(wcd,-new_ve)
       endif
       call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)     ! p
       call wc_arg(wcd,cnode_arg(args,2),.true.,rv,ve)     ! x-new
       call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)    ! x
       break2=wcode_cblock(wcd,cnode_arg(args,5),rv,new_ve)
       call wc_call(wcd,callnode,op_isend_reply,0,3,0,new_ve)
       call wc_arg(wcd,cnode_arg(args,1),.false.,rv,new_ve)     ! p
       call wc_arg(wcd,cnode_arg(args,4),.false.,rv,new_ve)
       if(pm_is_compiling) then
          call cvar_set_shared(wcd,arg_slot(wcd,cnode_arg(args,1)))
          call comp_finish_block(wcd,pc)
       endif
       call release_var(wcd,new_ve)
    case(sym_pm_recv_assn)
       call wc_call(wcd,callnode,op_recv_assn_call,&
            merge(1,0,check_arg_type(wcd,args,rv,6)==wcd%true_name),7,2,ve)
       if(pm_is_compiling) then
          call comp_link_dref(wcd,cnode_arg(args,2),cnode_arg(args,4))
          new_ve=0
          pc=comp_start_block(wcd)
       else
          new_ve=alloc_var(wcd,pm_ve_type)
          call wc(wcd,-new_ve)
       endif
       call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)     ! p
       call wc_arg(wcd,cnode_arg(args,2),.true.,rv,ve)     ! x-new
       call wc_arg(wcd,cnode_arg(args,4),.false.,rv,ve)    ! x
       call wc_arg(wcd,cnode_arg(args,3),.true.,rv,ve)     ! y-new
       call wc_arg(wcd,cnode_arg(args,5),.false.,rv,ve)    ! x
       break2=wcode_cblock(wcd,cnode_arg(args,7),rv,new_ve)
       if(pm_is_compiling) call comp_finish_block(wcd,pc)
       call release_var(wcd,new_ve)
    case(sym_pm_do,sym_pm_do_at)
       do i=merge(1,3,sig==sym_pm_do),nargs-1,2
          if(pm_is_compiling) then
             slot=arg_slot(wcd,cnode_arg(args,i+1))
             if(cvar_kind(wcd,slot)==v_is_vect_wrapped) then
                slot=cvar_v1(wcd,slot)
             endif
             call comp_alias_slots(wcd,var_slot(wcd,cnode_arg(args,i)),slot)
          else
             call link_to_val(wcd,callnode,&
                  cnode_arg(args,i),wcd%base,&
                  cnode_arg(args,i+1),wcd%base,rv,ve)
          endif
       enddo
       if(sig==sym_pm_do.and..not.pm_is_compiling) then
          break2=wcode_cblock(wcd,cnode_arg(args,nargs),rv,ve)
       else
          call wc_call(wcd,callnode,op_do_at,merge(1,0,sig==sym_pm_do),&
               merge(4,2,sig==sym_pm_do_at),0,ve)
          if(pm_is_compiling) then
             new_ve=0
             pc=comp_start_block(wcd)
          else
             new_ve=alloc_var(wcd,pm_ve_type)
             call wc(wcd,-new_ve)
          endif
          if(sig==sym_pm_do_at) then
             if(pm_is_compiling) then
                call wc_arg(wcd,cnode_arg(args,2),.false.,rv,ve)
             else
                call wc_arg(wcd,cnode_arg(args,1),.false.,rv,ve)
                call wc_arg(wcd,cnode_arg(args,2),.false.,rv,ve)
             endif
          endif
          if(pm_is_compiling) then
             call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
          endif
          break2=wcode_cblock(wcd,cnode_arg(args,nargs),rv,new_ve)
          if(pm_is_compiling) call comp_finish_block(wcd,pc)
          call release_var(wcd,new_ve)
       endif
    case(sym_pm_head_node)
       if(pm_is_compiling) then
          new_ve=0
          call wc_call(wcd,callnode,op_head_node,0,2,0,new_ve)
          pc=comp_start_block(wcd)
          break2=wcode_cblock(wcd,cnode_arg(args,1),rv,new_ve)
          call comp_finish_block(wcd,pc)
       else
          new_ve=alloc_var(wcd,pm_ve_type)
          call wc_call(wcd,callnode,op_head_node,0,2,0,ve)
          call wc(wcd,-new_ve)
          break2=wcode_cblock(wcd,cnode_arg(args,1),rv,new_ve)
       endif
    case(sym_pm_dref:sym_pm_ref)
       if(pm_is_compiling) then
          i=var_slot(wcd,cnode_arg(args,1))
          do ii=2,nargs
             if(ii==4) then
                slot=arg_slot(wcd,cnode_arg(args,ii))
                call cvar_set_ptr(wcd,i,ii-1,slot)
             else
                call comp_alias_slots(wcd,cvar_ptr(wcd,i,ii-1),&
                     arg_slot(wcd,cnode_arg(args,ii)))
             endif
          enddo
          j=var_slot(wcd,cnode_arg(args,3))
          do ii=nargs,5
             call comp_alias_slots(wcd,cvar_ptr(wcd,i,ii),cvar_ptr(wcd,j,ii))
          enddo
          call wc_call(wcd,callnode,op_dref,0,nargs,0,ve)
          do ii=2,nargs
             call wc_arg(wcd,cnode_arg(args,ii),.false.,rv,ve)
          enddo
       else
          call wc_call_args(wcd,callnode,args,op_dref,&
               merge(0,1,sig==sym_pm_dref.or.sig==sym_pm_dref_slice),nargs,1,rv,ve)
       endif
    case(sym_pm_for)
       call pm_for(cnode_arg(args,1),cnode_arg(args,2),ve)
    case(sym_pm_shared_always)
       new_ve=wcd%shared_ve
       break=wcode_cblock(wcd,cnode_arg(args,2),rv,new_ve)
    case(sym_pm_shared)
       new_ve=alloc_var(wcd,pm_ve_type)
       call wc_call(wcd,callnode,op_skip_empty,0,3,0,ve)
       call wc(wcd,-new_ve)
       call wc(wcd,wcd%shared_ve)
       break=wcode_cblock(wcd,cnode_arg(args,2),rv,new_ve)
    case(sym_pm_chan,sym_pm_chan_always)
       new_ve=alloc_var(wcd,pm_ve_type)
       call wc_call(wcd,callnode,op_chan,0,2,0,ve)
       call wc(wcd,-new_ve)
    case(sym_task)
       do i=3,nargs,3
          break2=wcode_cblock(wcd,cnode_arg(args,i),rv,ve)
       enddo
    case(sym_any)
       call any_statement
    case(sym_pm_each_index)
       call each_index_statement
    case(sym_pval,sym_pval_as)
       if(pm_is_compiling) then
          tno=check_arg_type(wcd,args,rv,3)
          slot=cvar_alloc_entry(wcd,v_is_basic,0,0,tno)
          call add_to_typeset(wcd,tno)
          call comp_assign_to_slot(wcd,callnode,slot,cnode_arg(args,3),.true.,rv,ve)
       endif
       tno=get_arg_type(wcd,cnode_arg(args,1),rv)
       call wc_call(wcd,callnode,op_make_poly,tno,3,1,ve)
       call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)
       if(pm_is_compiling) then
          call wc(wcd,slot)
       else
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       endif
    case(sym_type_val)
       if(.not.pm_is_compiling) then
          tno=get_arg_type(wcd,cnode_arg(args,1),rv)
          call wc_call(wcd,callnode,op_make_type_val,tno,2,1,ve)
          call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)
       endif
    case(sym_rec)
       if(pm_is_compiling) then
          i=arg_slot(wcd,cnode_arg(args,1))
          if(cvar_kind(wcd,i)==v_is_group) then
             do kk=5,nargs
                call comp_alias(wcd,callnode,pm_null_obj,cnode_arg(args,kk),rv,ve,&
                     cvar_ptr(wcd,i,kk-4))
             enddo
          else
             typ=pm_type_strip_mode(wcd%context,get_arg_type(wcd,cnode_arg(args,1),rv),j)
             tv=pm_type_vect(wcd%context,typ)
             do kk=5,nargs
                if(pm_type_needs_storage(wcd%context,pm_tv_arg(tv,kk-4))) then
                   slot=arg_slot(wcd,cnode_arg(args,kk))
                   call comp_assign_slots(wcd,callnode,&
                        cvar_alloc_elem(wcd,i,kk-4),&
                        slot,.true.,rv,ve)
                endif
             enddo
          endif
       else
          typ=check_arg_type(wcd,args,rv,1)
          call wc_call(wcd,callnode,op_rec,&
               typ,nargs-2,1,ve)
          call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)
          do kk=5,nargs
             arg=cnode_arg(args,kk)
             call wc_arg(wcd,cnode_arg(args,kk),.false.,rv,ve)
          enddo
       endif
    case(sym_pm_list)
       typ=check_arg_type(wcd,args,rv,1)
       call wc_call_args(wcd,callnode,args,op_rec,typ,&
            nargs,1,rv,ve)
    case(sym_dot,sym_dot_ref,sym_get_dot,sym_get_dot_ref,sym_method_call)
       i=rvv(cnode_get_num(callnode,call_index))   
       if(i>pm_type_dref_offset/2) then
          j=op_elem_ref
          i=i-pm_type_dref_offset
       else
          j=op_elem
       endif
       call wc_args_get_elem(wcd,callnode,j,args,i,rv,ve)
    case(sym_test)
       if(pm_opts%check_stmts) then
          if(restart) then
             break=restart_cblock(wcd,new_ve)
          else
             break=wcode_cblock(wcd,cnode_arg(args,1),rv,ve)
          endif
       endif
    case(sym_check)
       if(check_arg_type(wcd,args,rv,3)==pm_logical&
            .and.pm_opts%check_stmts) then
          if(restart) then
             break=restart_cblock(wcd,new_ve)
          else
             break=wcode_cblock(wcd,cnode_arg(args,4),rv,ve)
          endif
          if(break) return
          if(.not.pm_fast_isnull(cnode_arg(args,2))) then
             new_ve=alloc_var(wcd,pm_ve_type)
             call wc_call(wcd,callnode,op_andnot_ve,0,3,1,ve)
             call wc(wcd,-new_ve)
             call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
             break=wcode_cblock(wcd,cnode_arg(args,2),rv,new_ve)
             call release_var(wcd,new_ve)
             if(break) call wcode_error(wcd,callnode,&
                  'Cannot have communicating operations in'//&
                  ' expression for "check" error message')
          endif
          call wc_call(wcd,callnode,op_check,0,3,0,ve)
          call wc_arg(wcd,cnode_arg(args,1),.false.,rv,ve)
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       endif
    case(sym_present)
       if(wcd%base==0) then
          call wc_call_args(wcd,callnode,args,op_present,0,2,1,rv,ve)
       else
          ! Inline version
          if(.not.pm_fast_isnull(wcd%inline_keys)) then
             ok=.false.
             do j=1,cnode_numargs(wcd%inline_keys)
                 if(wcd%inline_key_names%data%i(wcd%inline_key_names%offset+j-1)==&
                     cnode_get_num(cnode_arg(args,i),var_name)) then
                    ok=.true.
                 endif
             enddo
          else
             ok=.false.
          endif
          call wc_call(wcd,callnode,op_logical_return,merge(1,0,ok),2,1,ve)
          call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)
       endif
    case(sym_key)
       n=nargs/4
       if(wcd%base==0) then
          do i=1,nargs/4
              if(get_arg_type(wcd,cnode_arg(args,i+n),rv)==&
                  get_arg_type(wcd,cnode_arg(args,i*2+n+n),rv).or..true.) then
                if(pm_is_compiling) then
                   call wc_call(wcd,callnode,op_get_key,0,5,1,ve)
                   pc=comp_start_block(wcd)
                   break2=wcode_cblock(wcd,cnode_arg(args,i*2+n+n-1),rv,ve)
                   call comp_finish_block(wcd,pc)
                   call wc_arg(wcd,cnode_arg(args,i+n),.true.,rv,ve)
                   call wc_arg(wcd,cnode_arg(args,i),.false.,rv,ve)
                   call wc_arg(wcd,cnode_arg(args,i*2+n+n),.false.,rv,ve)
                else
                   new_ve=alloc_var(wcd,pm_ve_type)
                   call wc_call(wcd,callnode,op_get_key,0,4,0,ve)
                   call wc(wcd,-new_ve)
                   call wc_arg(wcd,cnode_arg(args,i+n),.true.,rv,ve)
                   call wc_arg(wcd,cnode_arg(args,i),.false.,rv,ve)
                   break2=wcode_cblock(wcd,cnode_arg(args,i*2+n+n-1),rv,new_ve)
                   call wc_call(wcd,callnode,op_default,0,3,1,new_ve)
                   call wc_arg(wcd,cnode_arg(args,i+n),.true.,rv,new_ve)
                   call wc_arg(wcd,cnode_arg(args,i*2+n+n),.false.,rv,new_ve)
                endif
             else
                write(*,*) 'Diffnt',&
                     trim(pm_type_as_string(wcd%context,get_arg_type(wcd,cnode_arg(args,i+n),rv))),&
                     trim(pm_type_as_string(wcd%context,get_arg_type(wcd,cnode_arg(args,i*2+n+n),rv)))
                ! key arg type differs from default value - so key arg must be present
                call link_to_val(wcd,callnode,cnode_arg(args,i+n),wcd%base,&
                     cnode_arg(args,i),wcd%base,rv,ve)
             endif
          enddo
       else
          ! Inline case
          outer:do i=1,n
             if(.not.pm_fast_isnull(wcd%inline_keys)) then
                do j=1,cnode_numargs(wcd%inline_keys)
                   v=cnode_arg(wcd%inline_keys,j)
                   if(wcd%inline_key_names%data%i(wcd%inline_key_names%offset+j-1)==&
                        cnode_get_num(cnode_arg(args,i),var_name)) then
                      call link_to_val(wcd,callnode,cnode_arg(args,i+n),wcd%base,&
                           cnode_arg(wcd%inline_keys,j),wcd%oldbase,rv,ve)
                      cycle outer
                   endif
                enddo
             endif
             break2=wcode_cblock(wcd,cnode_arg(args,i*2+n+n-1),rv,new_ve)
             call link_to_val(wcd,callnode,cnode_arg(args,i+n),wcd%base,&
                  cnode_arg(args,i*2+n+n),wcd%base,rv,ve)
          enddo outer
       endif
    case(sym_is)
       if(check_arg_type(wcd,args,rv,1)==wcd%true_name) then
          call wc_call(wcd,callnode,op_logical_return,1,2,1,ve)
       else
          call wc_call(wcd,callnode,op_logical_return,0,2,1,ve)
       endif
       call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)
    case(sym_open)
       if(pm_is_compiling.and.wcd%base==0) then
          wcd%pvar=cvar_alloc_slots(wcd,3+nargs)
          call cvar_set_info(wcd,wcd%pvar,v_is_group,&
               nargs,v_is_tuple,0)
          do kk=1,nargs
             call cvar_set_ptr(wcd,wcd%pvar,kk,&
                  var_slot(wcd,cnode_arg(args,kk)))
          enddo
       endif
    case(sym_private,sym_set_mode,sym_const,sym_var,&
         sym_invar,sym_shared,sym_var_set_mode,sym_assign,sym_sync_assign,sym_assignment)
       continue ! Nothing to do
    case(sym_null)
       call wc_call_args(wcd,callnode,args,op_nullify,0,nargs,nargs,rv,ve)
    case(sym_cast)
       i=rvv(cnode_get_num(callnode,call_index))
       if(i==0) then
          call link_to_val(wcd,callnode,cnode_arg(args,1),wcd%base,&
               cnode_arg(args,2),wcd%base,rv,ve)
       else
          call wc_call_args(wcd,callnode,args,op_make_poly,&
               i,3,1,rv,ve)
       endif
    case(sym_dcaret)
       if(pm_is_compiling) then
          slot1=var_slot(wcd,cnode_arg(args,1))
          slot2=arg_slot(wcd,cnode_arg(args,2))
          if(cvar_kind(wcd,slot2)==v_is_vect_wrapped) then
             slot2=cvar_v1(wcd,slot2)
          endif
          call cvar_set_info(wcd,slot1,v_is_vect_wrapped,slot2,0,cvar_type(wcd,slot2))
          call wc_call(wcd,callnode,op_wrap,0,2,0,ve)
          call wc(wcd,slot1)
       else
          call link_to_val(wcd,callnode,cnode_arg(args,1),wcd%base,&
               cnode_arg(args,2),wcd%base,rv,ve)
       endif
    case(sym_fix,sym_literal,sym_caret,sym_change_mode)
       call link_to_val(wcd,callnode,cnode_arg(args,1),wcd%base,&
            cnode_arg(args,2),wcd%base,rv,ve)
    case(sym_pm_set_dotdotdot)
       wcd%xbase=wcd%top
       tno=get_arg_type(wcd,cnode_arg(args,2),rv)
       tv=pm_type_vect(wcd%context,tno)
       do i=1,pm_tv_numargs(tv)
          call wc_call(wcd,callnode, op_elem,i+1,3,1,ve)
          wcd%top=wcd%top+1
          wcd%rdata(wcd%top)=alloc_var(wcd,pm_tv_arg(tv,i))
          call wc(wcd,-wcd%rdata(wcd%top))
          call wc(wcd,arg_slot(wcd,cnode_arg(args,2)))
       enddo
    case(sym_result)
       if(wcd%base==0) then
          if(pm_is_compiling) then
             if(debug_wcode) then
                write(*,*) 'RETURN-ACTUAL[',n,']:',&
                     trim(pm_type_as_string(wcd%context,get_arg_type(wcd,cnode_arg(args,n),rv)))
             endif
             i=wcd%retvar
             do kk=1,nargs
                arg=cnode_arg(args,kk)
                call comp_assign_to_slot(wcd,callnode,cvar_ptr(wcd,i,kk),arg,.true.,rv,ve)
             enddo
          else
             call wc_call_args(wcd,callnode,args,op_return,&
                  int(wcd%loop_extra_arg),nargs,0,rv,ve)
          endif
       else
          ! Inlining ... link returned values to return args
          do n=1,nargs
             if(debug_wcode) then
                write(*,*) 'RETURN[',n,']:',&
                     trim(pm_type_as_string(wcd%context,get_arg_type(wcd,cnode_arg(args,n),rv)))
             endif
             if(pm_is_compiling) then
                slot=cvar_strip_alias(wcd,arg_slot_in_frame(wcd,cnode_arg(args,n),wcd%base))
                slot2=cvar_strip_alias(wcd,arg_slot_in_frame(wcd,cnode_arg(wcd%inline_args,n),wcd%oldbase))

                if(debug_wcode) then
                   write(*,*) 'RETURN ASSN',&
                        trim(pm_type_as_string(wcd%context,&
                        check_arg_type_with_mode(wcd,wcd%inline_args,wcd%outer_rv,n)))
                endif

                ! If returning non-array mirrored value from non-shared, non-comm proc
                ! Or returning to a channel variable
                ! Then need to assign not link
                if((cvar_kind(wcd,slot)/=v_is_group.or.&
                     cvar_kind(wcd,slot)==v_is_group.and.&
                     cvar_v2(wcd,slot)/=v_is_array).and.&
                     wcd%loop_extra_arg==0.and.&
                     pm_type_get_mode(wcd%context,&
                     check_arg_type_with_mode(wcd,wcd%inline_args,wcd%outer_rv,n))>=sym_invar&
                     .or.cvar_kind(wcd,slot2)==v_is_chan_vect) then
                   if(debug_wcode) then
                      write(*,*) 'RETURN actual ASSN',cvar_kind(wcd,slot),&
                           trim(pm_type_as_string(wcd%context,&
                           check_arg_type_with_mode(wcd,wcd%inline_args,wcd%outer_rv,n)))
                   endif

                   call comp_assign_slots(wcd,callnode,&
                        arg_slot_in_frame(wcd,cnode_arg(wcd%inline_args,n),wcd%oldbase),&
                        slot,.true.,rv,ve)
                else
                   call link_to_val(wcd,callnode,&
                        cnode_arg(wcd%inline_args,n),wcd%oldbase,&
                        cnode_arg(args,n),wcd%base,rv,ve)
                endif
             else
                call link_to_val(wcd,callnode,&
                     cnode_arg(wcd%inline_args,n),wcd%oldbase,&
                     cnode_arg(args,n),wcd%base,rv,ve)
             endif
          enddo
       endif
    case(first_pragma:last_pragma)
       if(sig==sym_show) then
          call wc_call(wcd,callnode,op_show,0,1,0,ve)
       elseif(sig==sym_show_stack) then
          call wc_call(wcd,callnode,op_show_stack,0,1,0,ve)
       endif
    case default
       if(sig>0) then
          write(*,*) 'SIG=',sig
          write(*,*) sym_names(sig)
          call pm_panic('Unknown call type in wcode_call')
       endif
       ! For general procedure calls:
       !      nargs = number of args before arg...
       !      totargs = total number of args passed (including arg...)
       if(cnode_flags_set(callnode,call_flags,call_is_vararg)) then
          nargs=nargs-1
          totargs=nargs+wcd%top-wcd%xbase
       else
          totargs=nargs
       endif
       if(pm_fast_isnull(cnode_get(callnode,call_keys))) then
          nkeys=0
       else
          nkeys=cnode_numargs(cnode_get(callnode,call_keys))
       endif
       call wcode_proc_call(wcd,callnode,rv,ve,merge(wcd%shared_ve,-1,&
            cnode_flags_set(callnode,call_flags,proccall_is_comm)),&
            args,nargs,totargs,nkeys,nret,sig)
    end select
    if(debug_wcode) then
       if(sig>0) then 
          write(*,*) 'END WCODE CALL>',sym_names(sig)
       else
          write(*,*) 'END WCODE CALL> sig=',-sig,'resolv=',&
               rvv(int(cnode_get_num(callnode,call_index)))
       endif
    endif
  contains
    include 'fisnull.inc'
    include 'ftiny.inc'
    include 'fnewnc.inc'
    include 'fesize.inc'
    include 'fvkind.inc'

    subroutine pm_for(arg,stmts,ve)
      type(pm_ptr),intent(in):: arg,stmts
      integer,intent(in):: ve
      integer:: j,new_ve,save_xbase,save_lbtop,save_top,save_shared_ve
      save_xbase=wcd%xbase
      save_top=wcd%top
      if(.not.pm_is_compiling) then
         new_ve=alloc_var(wcd,pm_ve_type)
         call wc_call(wcd,callnode,op_par_loop,0,3,1,ve)
         call wc(wcd,-new_ve)
         call wc_arg(wcd,arg,.false.,rv,ve)
         j=wc_jump_call(wcd,callnode,op_jmp,0,1,ve)
      else
         call wc_simple_comp_call(wcd,op_comm_block,0,3,ve)
         j=comp_start_block(wcd)
         call wc_arg(wcd,cnode_arg(args,6),.false.,rv,ve)
         new_ve=0
      endif
      save_lbtop=wcd%lbtop
      save_shared_ve=wcd%shared_ve
      wcd%shared_ve=ve
      call wcode_comm_block(wcd,stmts,new_ve,rv,new_ve)
      if(.not.pm_is_compiling) then
         call wc_call(wcd,callnode,op_par_loop_end,0,1,0,ve)
         call set_jump_to_here(wcd,j)
      else
         call comp_finish_block(wcd,j)
      endif
      wcd%shared_ve=save_shared_ve
      wcd%lbtop=save_lbtop
      wcd%xbase=save_xbase
      wcd%top=save_top
    end subroutine pm_for

    subroutine any_statement
      logical:: any_break
      integer:: jmp
      v=cnode_arg(args,4)
      v=cnode_arg(v,1)
      slot=v%data%i(v%offset)
      slot2=v%data%i(v%offset+1_pm_p)
      u=pm_dict_val(wcd%context,wcd%sig_cache,int(&
           rvv(int(cnode_get_num(callnode,call_index))),pm_ln))
      if(.not.pm_is_compiling) new_ve=alloc_var(wcd,pm_ve_type)
      any_break=.false.
      do kk=1,cnode_numargs(u)
         if(pm_is_compiling) new_ve=alloc_var(wcd,pm_ve_type)
         arg=cnode_arg(u,kk)
         rv%data%i(rv%offset+slot:rv%offset+slot2)=&
              arg%data%i(arg%offset:arg%offset+slot2-slot)
         tno=check_arg_type(wcd,args,rv,1)
         call wc_call(wcd,callnode,op_any,tno,4,1,ve)
         call wc(wcd,-new_ve)
         if(pm_is_compiling) then
            call add_to_typeset(wcd,tno)
            slot3=cvar_alloc_entry(wcd,v_is_basic,0,0,tno)
            call wc(wcd,-slot3)
            wcd%rdata(cnode_get_num(cnode_arg(args,1),var_index)+wcd%base)=cvar_alloc(wcd,tno,0)
         else
            call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)
         endif
         call wc_sarg(wcd,cnode_arg(args,3),kk<cnode_numargs(u),rv,ve)
         if(pm_is_compiling) then
            call comp_assign_slots(wcd,callnode,var_slot(wcd,cnode_arg(args,1)),slot3,&
                 .true.,rv,new_ve)
         else
            jmp=wc_jump_call(wcd,callnode,op_skip_any,0,1,new_ve)
         endif
         any_break=any_break.or.wcode_cblock(wcd,cnode_arg(args,2),rv,new_ve)
         if(.not.pm_is_compiling) call set_jump_to_here(wcd,jmp)
      enddo
      if(.not.pm_is_compiling) call release_var(wcd,new_ve)
    end subroutine any_statement

    subroutine each_index_statement
      logical:: any_break
      integer:: k,kk,n,num_named,first_pc
      integer,dimension(:),allocatable::rets
      logical:: break
      type(pm_ptr):: cblock,p
      v=cnode_arg(args,nret+3)
      v=cnode_arg(v,1)
      slot=v%data%i(v%offset)
      slot2=v%data%i(v%offset+1_pm_p)
      u=pm_dict_val(wcd%context,wcd%sig_cache,int(&
           rvv(int(cnode_get_num(callnode,call_index))),pm_ln))
      cblock=cnode_arg(args,nret+2)
      n=cnode_numargs(u)
      if(nret>1) allocate(rets(n))
      do kk=1,n
         arg=cnode_arg(u,kk)
         rv%data%i(rv%offset+slot:rv%offset+slot2)=&
              arg%data%i(arg%offset:arg%offset+slot2-slot)

         first_pc=wcd%pc
         num_named=wcode_mvars(wcd,cblock,rv,ve)
        
         if(.not.pm_is_compiling) then
            call wc_call(wcd,callnode,op_number,kk,2,1,ve)
            call wc_arg(wcd,cnode_arg(args,nret),.true.,rv,ve)
         endif
         
         p=cnode_get(cblock,cblock_first_call)
         do while(.not.pm_fast_isnull(p))
            break=wcode_call(wcd,p,rv,ve,.false.)
            p=cnode_get(p,call_link)
         enddo

         ! Close variables
         if(.not.pm_is_compiling) then
            call close_vars(wcd,cblock,rv,ve,first_pc,num_named)
         endif

         if(nret>1) then
            call wc_call(wcd,callnode,op_setref,0,3,1,ve)
            rets(kk)=alloc_var(wcd,get_arg_type(wcd,cnode_arg(args,nret+4),rv))
            call wc(wcd,-rets(kk))
            call wc_arg(wcd,cnode_arg(args,nret+4),.false.,rv,ve)
         endif
         
      enddo
      if(nret>1) then
         call wc_call(wcd,callnode,op_struct,get_var_type(wcd,cnode_arg(args,1),rv),n+2,1,ve)
         call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)
         do kk=1,n
            call wc(wcd,rets(kk))
            call release_var(wcd,rets(kk))
         enddo
      end if
    end subroutine each_index_statement
    
    function rvv(n) result(m)
      integer,intent(in):: n
      integer:: m
      m=rv%data%i(rv%offset+n)
    end function rvv

    subroutine release_import_varg(xbase)
      integer,intent(in):: xbase
      integer:: i
      do i=xbase+1,wcd%top
         call release_var(wcd,wcd%rdata(i))
      enddo
    end subroutine release_import_varg

  end function  wcode_call

  !========================================
  ! Check if a call is flagged
  !========================================
  function call_flag_set(wcd,callnode,rv) result(ispar)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,rv
    logical:: ispar
    ispar=rv%data%i(rv%offset+cnode_get_num(callnode,call_index))/=0
  end function call_flag_set

  !====================================================================
  ! Wcode general procedure calls
  !      nargs   = number of args before arg...
  !      totargs = total number of args passed (including arg...)
  ! For comm calls only (for non-comm calls ve2==0)
  !      ve      = vector engine for inner scope
  !      ve2     = vector engine for outer scope
  !====================================================================
  recursive subroutine wcode_proc_call(wcd,callnode,rv,ve,ve2,&
       args,nargs,totargs,nkeys,nret,sig)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,rv
    integer,intent(in):: ve,ve2
    type(pm_ptr),intent(in):: args
    integer,intent(in):: nargs,totargs,nkeys,nret
    integer,intent(in):: sig
    integer:: ve1,idx,procnode_kind,op,slot,slot2,slot3,typ
    integer:: i,j,arg_base,op2,taints,par_kind,pc,tno,nproc_keys
    type(pm_ptr):: keys,key_names,proc_keys,procnode,arg,tv,amps,p
    logical:: varg,ok,autocv,save_inline_all,steps_back,enclosing_block
    integer:: extra_ve,ignore_args
    logical:: keep_ctime_const
    integer,dimension(-nkeys:totargs):: conv
    if(ve2<0) then
       extra_ve=0
    else
       extra_ve=1
    endif
    ignore_args=0
    idx=rvv(cnode_get_num(callnode,call_index))
    
    ! Check for special signatures
    if(idx<0) then
       select case(idx)
       case(sp_sig_dup)
          if(pm_is_compiling) then
             call comp_assign(wcd,callnode,&
                  cnode_arg(args,1),cnode_arg(args,3+extra_ve*(num_comm_args-1)),.true.,rv,ve)
          else
             call wc_call(wcd,callnode,op_clone,66,3,1,ve)
             call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)
             call wc_arg(wcd,cnode_arg(args,3+extra_ve*(num_comm_args-1)),.false.,rv,ve)
          endif
       case(sp_sig_link)
          call link_to_val(wcd,callnode,cnode_arg(args,3+extra_ve*(num_comm_args-1)),&
               wcd%base,cnode_arg(args,1),wcd%base,rv,ve)
       case(sp_sig_noop)
          continue
       case(sp_sig_setval)
          if(.not.pm_is_compiling) then
             call wc_call(wcd,callnode,op_setref,0,3,1,ve)
             call wc_arg(wcd,cnode_arg(args,1),.true.,rv,ve)
             call wc(wcd,&
                  -pm_max_stack-&
                  add_const(wcd,pm_type_val(wcd%context,check_arg_type(wcd,args,rv,1))))
          endif
       case default
          call wcode_error(wcd,callnode,'System Error!')
          write(*,*) 'IDX=',idx
          call pm_panic('bad sp_sig')
       end select
       return
    elseif(pm_debug_checks.and.idx==0) then
       call wcode_error(wcd,callnode,'Compiler internal error: idx=0')
    endif

    par_kind=0
    
    ! Get signature details

    !write(*,*) 'GETSIG>',idx
    
    procnode=pm_dict_key(wcd%context,wcd%sig_cache,int(idx,pm_ln))
    if(pm_fast_esize(procnode)>1) par_kind=procnode%data%i(procnode%offset+2)
    procnode=pm_dict_val(wcd%context,wcd%sig_cache,int(idx,pm_ln))
    varg=cnode_flags_set(callnode,call_flags,call_is_vararg)
    procnode_kind=cnode_get_kind(procnode)
    keys=cnode_get(callnode,call_keys)
    
    ! Process any autoconversions
    conv=-1
    if(procnode_kind==cnode_is_autoconv_sig) then
       call autoconv

       ! Get nested signature details
       procnode=cnode_arg(procnode,cnode_numargs(procnode))
       idx=procnode%offset
       procnode=pm_dict_key(wcd%context,wcd%sig_cache,int(idx,pm_ln))
       if(pm_fast_esize(procnode)>1) par_kind=procnode%data%i(procnode%offset+2)
       procnode=pm_dict_val(wcd%context,wcd%sig_cache,int(idx,pm_ln))
       procnode_kind=cnode_get_kind(procnode)
    endif

    taints=0
    ve1=ve
    
    ! Find the procedure being called
    if(procnode_kind==cnode_is_resolved_proc) then
       ! Non-intrinsic - inline if possible, otherwise code to op_call operation
       save_inline_all=wcd%inline_all
       wcd%inline_all=pm_is_compiling.and.(wcd%inline_all.or.&
            cnode_flags_set(callnode,call_flags,call_inline_when_compiling))
       taints=cnode_get_num(procnode,node_args+2)
       if(wcd%inline_all.or.(wcd%proc_can_inline&
            .and.inlinable(procnode,args,nargs,nret,extra_ve))) then
          call wcode_inlined_call(wcd,callnode,rv,ve1,ve2,args,nargs,&
               totargs,nret,taints,procnode,varg,conv)
          wcd%inline_all=save_inline_all
          return
       else
          op=merge(op_comm_call,op_call,ve2>=0)
          op2=add_proc(idx,ve2,extra_ve)
          ! add_proc may decide that conditional context should not be passed
          ! and so zero extra_ve
       endif
       wcd%inline_all=save_inline_all
       keep_ctime_const=.false.
       key_names=pm_name_val(wcd%context,cnode_get_num(callnode,call_key_names))
       proc_keys=cnode_get(cnode_arg(procnode,1),pr_keys)
       nproc_keys=pm_fast_esize(proc_keys)/2
    else
       ! Intrinsic procedure - operator info stored in proc object
       op=cnode_get_num(procnode,bi_opcode)
       op2=cnode_get_num(procnode,bi_opcode2)
       if(cnode_flags_set(procnode,pr_flags,proc_needs_type)) then
          if(op==op_logical_return) then
             if(check_arg_type(wcd,args,rv,2)==wcd%false_name) then
                op2=0
             else
                op2=1
             endif
          elseif(op==op_elem) then
             if(nargs==9) then
                tno=check_arg_type(wcd,args,rv,9)
                p=pm_type_val(wcd%context,tno)
                op2=p%data%ln(p%offset)+1
             endif
          else
              op2=check_arg_type(wcd,args,rv,1)
          endif
       endif
       if(pm_is_compiling) then
          ! Some built-in operations need recoding for the compiler backend
          !!! Does not handle autoconversions !!!!
          if(comp_transform_op(wcd,callnode,op,op2,args,nargs,totargs,nret,rv,ve,ve2,extra_ve,conv)) return
       endif
       if(extra_ve>0) then
          ignore_args=num_comm_args
          extra_ve=0
       else
          ignore_args=1
       endif
       keep_ctime_const=.true.
       nproc_keys=0
    endif

    !write(*,*) 'CALLVE> PRE',ve1

     
    if(pm_is_compiling.and.extra_ve>0) then
       extra_ve=0
    endif
    
    ! Start coding the call instruction
    !write(*,*) 'CALLVE>',ve1
    call wc_call(wcd,callnode,op,op2,&
         totargs+extra_ve+1-ignore_args+merge(nkeys,nproc_keys,pm_is_compiling),nret,ve1)
    if(extra_ve>0) then 
       call wc(wcd,ve2)
    endif
    
    ! Code returns
    do i=1,nret
       call wc_p_arg(wcd,cnode_arg(args,i),.true.,rv,ve1,.false.)
    enddo

    ! If compiling need to flag up any "&" args
    ! (mainly for optimiser)
    if(pm_is_compiling) then
       amps=cnode_get(callnode,call_amp)
       if(.not.pm_fast_isnull(amps)) then
          amps=pm_name_val(wcd%context,int(amps%offset))
       endif
    endif
    
    ! Code arguments before arg...
    arg_base=wcd%pc
    j=0
    do i=nret+1+ignore_args,nargs
       if(pm_is_compiling.and..not.pm_fast_isnull(amps)) then
          if(amps%data%i(amps%offset+j)+nret+nkeys==i) then
             call wc_p_arg(wcd,cnode_arg(args,i),.true.,rv,ve1,keep_ctime_const)
             j=j+1
             cycle
          endif
       endif
       if(conv(i)>0) then
          call wc_p(wcd,conv(i),keep_ctime_const)
          call release_var(wcd,conv(i))
       else
          call wc_p_arg(wcd,cnode_arg(args,i),.false.,rv,ve1,keep_ctime_const)
       endif
    enddo
    
    ! Code arg... if present (otherwise nargs==totargs)
    do i=nargs+1,totargs
       if(conv(i)>0) then
          ! Already converted
          call wc_p(wcd,conv(i),keep_ctime_const)
          call release_var(wcd,conv(i))
       else
          ! Get from frame above xbase
          call wc_p(wcd,wcd%rdata(i-nargs+wcd%xbase),keep_ctime_const)
       endif
    enddo

    ! Code keyword arguments
    if(pm_is_compiling) then
       ! ... TODO ...
    else
       if(pm_fast_isnull(keys)) then
          do j=1,nproc_keys
             call wc(wcd,&
                  -pm_max_stack-add_const(wcd,pm_fast_tinyint(wcd%context,-1)))
          enddo
       else
          outer: do j=1,nproc_keys
             do i=1,nkeys
                if(proc_keys%data%i(proc_keys%offset+j-1)==key_names%data%i(key_names%offset+i-1)) then
                   if(conv(-i)>0) then
                      call wc_p(wcd,conv(-i),keep_ctime_const)
                      call release_var(wcd,conv(-i))
                   else
                      call wc_p_arg(wcd,cnode_arg(keys,i),.false.,rv,ve1,keep_ctime_const)
                   endif
                   cycle outer
                endif
             enddo
             call wc(wcd,&
                  -pm_max_stack-add_const(wcd,pm_fast_tinyint(wcd%context,-1)))
          enddo outer
       endif
    endif

    ! When compiling some args may have been disagregated
    ! so neeed to correct number of arguments
    if(pm_is_compiling) then
       call wc_correct_call_args(wcd)
    endif

 
  contains

    include 'fisnull.inc'
    include 'ftiny.inc'
    include 'fnewnc.inc'
    include 'fesize.inc'
    
    function rvv(n) result(m)
      integer,intent(in):: n
      integer:: m
      m=rv%data%i(rv%offset+n)
    end function rvv

    ! Code auto conversions and store result value slot
    ! for each conversion in conv(argument_number)
    subroutine autoconv
      integer:: i,j,idx,tno
      integer:: slot
      type(pm_ptr):: arg,tv
      integer:: typ
      
      do i=1,cnode_numargs(procnode)-1
         arg=cnode_arg(procnode,i)
         ! Argument indices are coded as displacements back from end of args
         ! (gets around possible presence of keyword args)
         idx=arg%data%i(arg%offset)
         tno=arg%data%i(arg%offset+1)
         slot=alloc_var(wcd,tno)
         call wc_call(wcd,callnode,op_make_poly,tno,3,1,ve)
         call wc(wcd,-slot)
         if(idx<0) then
            call wc_arg(wcd,cnode_arg(args,idx),.false.,rv,ve)
         else
            call wc_arg(wcd,cnode_arg(args,idx),.false.,rv,ve)
         endif
         conv(idx)=slot
      enddo
    end subroutine autoconv

    ! Add a proc to the code cache
    ! Return slot number in cache (zero base)
    function add_proc(sig,ve2,extra_ve) result(n)
      integer,intent(in):: sig,ve2
      integer,intent(inout):: extra_ve
      integer:: n
      integer,dimension(2):: key
      type(pm_ptr):: proc
      integer::m
      key(1)=sig
      key(2)=0
      m=1
      if(pm_is_compiling.and.ve2>0) then
         ! If in a conditional context and called procedure
         ! does not have "cplt" execution mode then need to
         ! pass the conditional context
         proc=pm_dict_val(wcd%context,wcd%sig_cache,int(sig,pm_ln))
         proc=cnode_arg(proc,1)
         if(cnode_get_kind(proc)==cnode_is_proc) then
            key(2)=1
            m=2
         endif
         ! If not passing conditional context then zero extra_ve
         if(m==1) extra_ve=0
      endif

      ! Add the produre to the code cache if not there already
      n=pm_ivect_lookup(wcd%context,wcd%code_cache,key,m)-1
      if(n<0) then
         n=pm_idict_add(wcd%context,wcd%code_cache,key,m,pm_null_obj)-1
      endif
    end function add_proc

    ! Inlining criteria
    function inlinable(proc,args,nargs,nret,extra_ve) result(ok)
      type(pm_ptr),intent(in):: proc,args
      integer,intent(in):: nargs,nret,extra_ve
      type(pm_ptr):: p,newrv
      logical:: ok
      integer:: i,flags,tno

      ! Cannot inline recursive or non-inlinable procs
      p=cnode_arg(proc,1)
      if(.not.cnode_flags_clear(proc,cnode_args+2,&
           proc_is_recursive+proc_is_not_inlinable)) then
         ok=.false.
         return
      endif
      
  
      if(.not.pm_is_compiling) then
         ! This is limitation when producing VM code 
         do i=1,nret
            if(arg_is_mvar(cnode_arg(args,i))) then
               ok=.false.
               return
            endif
         enddo
      endif
      
!!$      ! Cannot currently inline procs with keyword args
      p=cnode_arg(proc,1)
      
      
      ! Forced inline/no-inline in some contexts
      if(wcd%inline_none) then
         ok=.false.
         return
      endif

      if(wcd%inline_all) then
         ok=.true.
         return
      endif
      
      ! Check if call and proc definition have no_inline or inline attributes
      flags=iand(ior(taints,cnode_get_num(callnode,call_flags)),proccall_is_inline+proccall_is_no_inline)
      if(iand(flags,proccall_is_no_inline)/=0) then
         ok=.false.
         return
      elseif(flags==proccall_is_inline) then
         ok=.true.
         return
      endif
    
      ! If we are beyond here then inlining is optional - so check room on stack
      newrv=cnode_arg(proc,2)
      if(wcd%top+pm_fast_esize(newrv)>=pm_max_stack) then
         ok=.false.
         return
      endif
      
      ! Inline as optimisation, rather than by request
      if(pm_opts%inline) then
         if(pm_is_compiling) then
            ok=cnode_get_num(cnode_arg(proc,1),pr_ncalls)<20
         else
            ok=cnode_get_num(cnode_arg(proc,1),pr_ncalls)<4
         endif
      endif
      
    end function inlinable
    
    ! Code keyword arguments (compling only)
    ! Each keyword comiled as pair of args: present/value
    subroutine comp_keys(nkeys)
      integer,intent(in):: nkeys
      type(pm_ptr):: key
      integer:: tvar,fvar,slot
      tvar=cvar_alloc_const(wcd,wcd%true_obj)
      fvar=cvar_alloc_const(wcd,wcd%false_obj)
      key=pm_dict_val(wcd%context,wcd%sig_cache,int(idx,pm_ln))
      key=cnode_arg(key,4)
      do i=1,nkeys
         if(check_arg_type(wcd,args,rv,nret+i)==pm_tiny_int) then
            call wc(wcd,fvar)
            call wc_p(wcd,cvar_alloc(wcd,key%data%i(key%offset+i-1),0),.false.)
         else
            slot=arg_slot(wcd,cnode_arg(args,i+nret))
            if(slot>0) call wc(wcd,tvar)
            call wc_p_arg(wcd,cnode_arg(args,i+nret),.false.,rv,ve,.false.)
         endif
      enddo
    end subroutine comp_keys
    
  end subroutine wcode_proc_call

  !====================================================================
  ! Inline procedure call
  !====================================================================
  subroutine wcode_inlined_call(wcd,callnode,old_rv,ve1,ve2,args,nargs,totargs,nret,taints,proc,varg,conv)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,args,proc
    type(pm_ptr),intent(in):: old_rv
    integer,intent(in):: ve1,ve2
    integer,intent(in):: nargs,totargs,nret,taints
    logical,intent(in):: varg
    integer,dimension(totargs):: conv
    
    integer:: save_base,save_oldbase,save_xbase,save_keybase,save_lbl
    integer:: save_loop_extra_arg
    type(pm_ptr):: save_args,save_rv,save_keys,save_key_names
    type(pm_ptr):: pr,p,c,cblock,rv,arg,tv
    integer:: pc,par,num_named,first_pc,npar,slot,i,n,xarg,tno,lastxarg,flags
    logical:: break
    integer:: ve
    integer:: nkeys
    integer:: save_shared_ve

    flags=cnode_get_num(callnode,call_flags)
    ve=ve1

    if(pm_is_compiling.and.ve1==shared_op_flag) then
       call wc_call(wcd,callnode,op_inline_shared,0,2,0,ve)
       pc=comp_start_block(wcd)
       if(debug_wcode) write(*,*) 'START SHARED INLINE',pc
       ve=0
    endif
    
    if(debug_wcode) write(*,*) 'START INLINING>'

    save_lbl=wcd%lbbase
    wcd%lbbase=wcd%lbtop
    save_oldbase=wcd%oldbase
    save_base=wcd%base
    save_xbase=wcd%xbase
    save_args=wcd%inline_args
    save_keys=wcd%inline_keys
    save_key_names=wcd%inline_key_names
    save_rv=wcd%outer_rv
    save_keybase=wcd%keybase
    save_loop_extra_arg=wcd%loop_extra_arg

    wcd%inline_args=args
    wcd%inline_keys=cnode_get(callnode,call_keys)
    wcd%inline_key_names=pm_name_val(wcd%context,cnode_get_num(callnode,call_key_names))
    
    if(debug_wcode) then
       write(*,*) 'INLINE PAR TYPES>>'
       do i=1,nargs
          p=cnode_arg(args,i)
          write(*,*) 'Par[',i,'] {',trim(pm_type_as_string(wcd%context,get_arg_type(wcd,p,old_rv))),'#',&
               arg_slot(wcd,cnode_arg(args,i)),'##',p%offset,wcd%base,old_rv%offset
          call pm_dump_tree(wcd%context,6,old_rv,2)
          call dump_cvar(wcd,6,arg_slot(wcd,cnode_arg(args,i)),nonest=.true.)
          write(*,*) '}'
       enddo
    endif
    
    wcd%outer_rv=old_rv
    
    first_pc=wcd%pc
    
    pr=cnode_arg(proc,1)
    if(debug_wcode) write(*,*) 'Inline>',pm_name_as_string(wcd%context,cnode_get_num(pr,pr_name))
    cblock=cnode_get(pr,pr_cblock)
    rv=cnode_arg(proc,2)
    !nkeys=cnode_get_num(pr,pr_nkeys)
    npar=nret+1
    wcd%keybase=nret
        
    save_shared_ve=wcd%shared_ve
    wcd%shared_ve=ve2
    wcd%loop_extra_arg=merge(1,0,ve2>0)
    wcd%rdata(wcd%top+1:wcd%top+pm_fast_esize(rv)+1)=-1
    
    ! Set parameters equal to arguments
    p=cnode_get(cblock,cblock_first_var)
    
    n=cnode_numargs(args)
    if(varg) n=n-1
    xarg=wcd%xbase+1
    lastxarg=wcd%top
    
    if(.not.pm_fast_isnull(p)) then
       do while(cnode_flags_set(p,var_flags,var_is_param))
          slot=cnode_get_num(p,var_index)
          if(npar>size(conv)) then
             write(*,*) npar,size(conv),totargs
             call wcode_error(wcd,callnode,'Internal Error: failed autoconversion while inlining')
          endif
          
          if(conv(npar)>0) then
             ! Result of auto-conversion
             wcd%rdata(slot+wcd%top)=conv(npar)
          elseif(npar>n) then
             if(debug_wcode) then
                write(*,*) 'COPY OVER> 1 arg',npar,wcd%rdata(wcd%xbase+npar-n)
             endif
             ! Take argument from args...
             wcd%rdata(slot+wcd%top)=wcd%rdata(xarg)
             xarg=xarg+1
          else
             wcd%rdata(slot+wcd%top)=arg_slot(wcd,cnode_arg(args,npar))
             if(debug_wcode) then
                write(*,*) 'MOVE_OVER> 1 arg',npar,wcd%rdata(slot+wcd%top)
             endif
          endif
          npar=npar+1
          p=cnode_get(p,var_link)
          if(pm_fast_isnull(p)) exit
       enddo
    endif
    
    wcd%oldbase=wcd%base
    wcd%base=wcd%top
    wcd%top=wcd%top+pm_fast_esize(rv)+1
    wcd%xbase=wcd%top
    if(wcd%top>max_code_stack) call pm_panic('out of code stack')
    
    ! Capture excess args into args... stored at top of frame
    if(debug_wcode) write(*,*) 'COPY EXCESSS> ',npar,n
    if(npar<=n) then
       do i=npar,n
          tno=check_arg_type(wcd,args,old_rv,i)
          if(debug_wcode) write(*,*) 'COPY VAL->',i,npar,n,tno
          if(tno==-1) then
             write(*,*) 'VAL=',npar,n,cnode_numargs(args),&
                  trim(pm_name_as_string(wcd%context,&
                  cnode_get_name(cnode_arg(args,i),var_name)))
             call wcode_error(wcd,callnode,'tno==-1')
          endif
          if(tno/=pm_tiny_int.and.tno/=-1) then
             tv=pm_type_vect(wcd%context,tno)
             if(pm_tv_kind(tv)/=pm_type_is_tuple) then
                wcd%top=wcd%top+1
                wcd%rdata(wcd%top)=&
                     arg_slot_in_frame(wcd,cnode_arg(args,i),wcd%oldbase)
                if(debug_wcode) then
                   write(*,*) 'VCOPY ARG>',i,wcd%rdata(wcd%top)
                endif
             endif
          endif
       enddo
    endif
    
    ! Copy over unused args... to top of frame
    if(debug_wcode) write(*,*) 'COPY UNUSED> -',xarg,lastxarg
    do i=xarg,lastxarg
       wcd%top=wcd%top+1
       wcd%rdata(wcd%top)=wcd%rdata(i)
       if(debug_wcode) then
          write(*,*) 'COPY ARG TO TOP>',i,wcd%rdata(i),wcd%rdata(wcd%top)
       endif
    enddo
    
    ! Allocate multiple-use variables
    num_named=wcode_mvars(wcd,cblock,rv,ve,p)
    
    if(debug_wcode) then
       write(*,*) 'INLINE...',wcd%base,wcd%oldbase
    endif
    
    ! Process calls
    c=cnode_get(cblock,cblock_first_call)
    do while(.not.pm_fast_isnull(c))
       if(debug_wcode) write(*,*) 'INLINE> ve=',wcd%shared_ve
       break=wcode_call(wcd,c,rv,ve,.false.)
       c=cnode_get(c,call_link)
    enddo
    
    if(debug_wcode) then
       write(*,*) '...INLINED',wcd%base,wcd%oldbase
    endif

    if(debug_wcode) write(*,*) 'Inlined>>',pm_name_as_string(wcd%context,cnode_get_num(pr,pr_name))

    ! Close multi-use variables
    if(.not.pm_is_compiling) then
       call close_vars(wcd,cblock,rv,ve,first_pc,num_named,p)
    endif
    
    wcd%top=wcd%base
    wcd%xbase=save_xbase
    wcd%base=save_base
    wcd%oldbase=save_oldbase
    wcd%outer_rv=save_rv
    wcd%inline_args=save_args
    wcd%inline_keys=save_keys
    wcd%inline_key_names=save_key_names
    wcd%keybase=save_keybase
    wcd%shared_ve=save_shared_ve
    wcd%lbtop=wcd%lbbase
    wcd%lbbase=save_lbl
    wcd%loop_extra_arg=save_loop_extra_arg
    
    ! Close down parameters releasing vars
    if(.not.pm_is_compiling) then
       do i=nret+1,nargs
          arg=cnode_arg(args,i)
          if(conv(i)>0) then
             call release_var(wcd,conv(i))
          elseif(arg_is_svar(arg)) then
             call release_var(wcd,var_slot(wcd,arg))
          endif
       enddo
    endif

    if(pm_is_compiling.and.ve1==shared_op_flag) then
       call comp_finish_block(wcd,pc)
       if(debug_wcode) write(*,*) 'FINISH SHARED',pc
    endif
    
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine wcode_inlined_call

  !====================================================================
  ! Reprocess some operators if compiling
  ! Returns true if processing complete and
  ! main subroutine should just return
  !====================================================================
  function comp_transform_op(wcd,callnode,op,op2,args,nargs,totargs,&
       nret,rv,ve,ve2,extra_ve,conv) result(finished)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,args,rv
    integer,intent(in):: op,op2,nargs,totargs,nret,ve,ve2,extra_ve
    integer,dimension(totargs):: conv
    logical:: finished
    integer:: slot,slot2,slot3,i
    integer,dimension(totargs):: argslot

    do i=1,totargs
       if(conv(i)>0) then
          argslot(i)=conv(i)
       else
          argslot(i)=cvar_strip_alias(wcd,arg_slot(wcd,cnode_arg(args,i)))
       endif
    enddo
    
    finished=.true.
    select case(op)
    case(op_clone)
       call comp_assign_slots(wcd,callnode,&
            argslot(1),argslot(2),.true.,rv,ve)
       return
    case(op_import_val,op_import_scalar,op_get_rf)
       call comp_alias_slots(wcd,argslot(1),&
            argslot(merge(2+num_comm_args,2,nargs>2)))
       return
    case(op_import_dref)
       slot=argslot(1)
       slot2=argslot(merge(2+num_comm_args,2,nargs>2))
       if(cvar_kind(wcd,slot2)==v_is_vect_wrapped) then
          slot2=cvar_v1(wcd,slot2)
       endif
       call comp_alias_slots(wcd,slot,slot2)
       return
    case(op_assign)
       call comp_assign_slots(wcd,callnode,&
            argslot(1),argslot(2),.false.,rv,ve)
       return
    case(op_redim)
       slot=argslot(1)
       slot2=argslot(2)
       if(cvar_kind(wcd,slot2)==v_is_group) then
          call cvar_set_alias(wcd,slot,&
               cvar_alloc_array_view(wcd,cvar_ptr(wcd,slot2,1),&
               argslot(3),cvar_type(wcd,slot)))
       else
          call cvar_set_alias(wcd,slot,&
               cvar_alloc_array_view(wcd,&
               cvar_alloc_elem(wcd,slot2,1),&
               argslot(3),cvar_type(wcd,slot)))
       endif
       return
    case(op_make_array)
       slot=argslot(1)
       slot2=argslot(2)
       if(pm_debug_checks) then
          if(cvar_kind(wcd,slot2)/=v_is_chan_vect) then
             call dump_cvar(wcd,6,slot2)
             write(*,*) '#',cvar_kind(wcd,slot2)
             call pm_panic('Transform op_make_array')
          endif
       endif
       call cvar_set_alias(wcd,slot,&
            cvar_alloc_array_view(wcd,&
            cvar_v1(wcd,slot2),&
            argslot(3),cvar_type(wcd,slot)))
       call wc_call(wcd,callnode,op_break_loop,0,2,0,ve)
       call wc(wcd,slot)
       return
    case(op_get_dom)
       slot2=argslot(1)
       slot=argslot(2)
       if(cvar_kind(wcd,slot)==v_is_group) then
          call comp_alias_slots(wcd,slot2,&
               cvar_ptr(wcd,slot,2))
       else
          call comp_get_elem(wcd,op_elem,slot2,slot,2)
       endif
       return
    case(op_make_rf,op_array_get_elem)
       call comp_get_subs(wcd,argslot(1),&
            argslot(2),argslot(3))
       return
    case(op_array_set_elem)
       slot2=comp_subs(wcd,argslot(1),&
            argslot(2))
       call comp_assign_slots(wcd,callnode,slot2,argslot(3),.false.,rv,ve)
       return
    case(op_dref)
       slot=argslot(1)
       call comp_alias_slots(wcd,cvar_ptr(wcd,slot,1),argslot(2))
       call comp_alias_slots(wcd,cvar_ptr(wcd,slot,2),argslot(3))
       call comp_alias_slots(wcd,cvar_ptr(wcd,slot,3),argslot(4))
       if(nargs<5) then
          slot2=argslot(3)
          call comp_alias_slots(wcd,cvar_ptr(wcd,slot,4),cvar_ptr(wcd,slot2,4))
          call comp_alias_slots(wcd,cvar_ptr(wcd,slot,5),cvar_ptr(wcd,slot2,5))
       else
          call comp_alias_slots(wcd,cvar_ptr(wcd,slot,4),argslot(5))
          call comp_alias_slots(wcd,cvar_ptr(wcd,slot,5),argslot(6))
       endif
       return
    case(op_elem_ref)
       slot=argslot(1)
       slot2=argslot(2)
       slot3=cvar_ptr(wcd,slot,1)
       call comp_get_elem(wcd,op_elem,slot3,cvar_ptr(wcd,slot2,1),op2-1)
       call comp_alias_slots(wcd,cvar_ptr(wcd,slot,2),cvar_ptr(wcd,slot2,2))
       call comp_alias_slots(wcd,cvar_ptr(wcd,slot,3),cvar_ptr(wcd,slot2,3))
       call comp_alias_slots(wcd,cvar_ptr(wcd,slot,4),cvar_ptr(wcd,slot2,4))
       call comp_alias_slots(wcd,cvar_ptr(wcd,slot,5),cvar_ptr(wcd,slot2,5))
       return
    case(op_dref_elem)
       call comp_alias_slots(wcd,argslot(1),&
            cvar_ptr(wcd,argslot(2),op2))
       return
    case(op_elem)
       ! Note -- this only works if op_elem is only applied to a dref.
       slot=argslot(1)
       slot2=argslot(2+merge(num_comm_args,0,nargs>3))
       if(cvar_kind(wcd,slot)==v_is_vect_wrapped) then
          slot=cvar_v1(wcd,slot)
       endif
       if(cvar_kind(wcd,slot2)==v_is_vect_wrapped) then
          slot2=cvar_v1(wcd,slot2)
       endif
       if(cvar_kind(wcd,slot2)==v_is_group.and.&
            (cvar_v2(wcd,slot2)==v_is_dref.or.cvar_v2(wcd,slot2)==v_is_shared_dref)) then
          slot2=cvar_ptr(wcd,slot2,op2)
          call comp_alias_slots(wcd,slot,slot2)
          return
       endif
       call pm_panic('transform op_elem')
!!$    case(op_intersect_aseq)
!!$       call wc_call(wcd,callnode,op,op2,nargs+1,ve)
!!$       call wc(wcd,-argslot(1))
!!$       call wc(wcd,-cvar_alloc_elem(wcd,argslot(2),1))
!!$       call wc(wcd,argslot(3))
!!$       call wc(wcd,cvar_alloc_elem(wcd,argslot(4),1))
!!$       call wc(wcd,argslot(5))
!!$       call wc(wcd,cvar_alloc_elem(wcd,argslot(6),1))
!!$       if(op2==2) call wc(wcd,cvar_ptr(wcd,argslot(7),1))
!!$       return
    case(op_read_file_array,op_write_file_array)
       call wc_call(wcd,callnode,op,op2,nargs+1,0,ve)
       call wc(wcd,-argslot(1))
       call wc(wcd,argslot(2))
       call wc(wcd,-cvar_ptr(wcd,argslot(3),1))
       call wc(wcd,argslot(4))
       return
    case(op_make_type_val)
       return
    end select
    finished=.false.
  end function comp_transform_op

  !====================================================================
  ! Wcode block that may contain communicating operations
  !====================================================================
  recursive subroutine wcode_comm_block(wcd,cblock,outve,rv,ve)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: cblock,rv
    integer,intent(in):: outve,ve
    integer:: cs,costart
    integer::new_ve
    logical:: break,reverse
    costart=wcd%cotop(wcd%cs)+1
    break=wcode_cblock(wcd,cblock,rv,ve)
    cs=wcd%cs
    reverse=.true.
    do while(break)
       reverse=.not.reverse
       if(debug_wcode) then
          write(*,*) 'OPS>',wcd%cs,wcd%cotop(wcd%cs),wcd%cotop(3-wcd%cs)
       endif
       call combine_ops(wcd,costart,outve,rv,ve,reverse)
       wcd%cs=3-wcd%cs
       costart=wcd%cotop(wcd%cs)+1
       break=restart_cblock(wcd,new_ve)
    enddo
    wcd%cs=cs
  end subroutine wcode_comm_block

  !====================================================================
  ! Combine communicating operations (labels) on different branches
  ! reverse gives order of pushed operations on costack
  !====================================================================
  recursive subroutine combine_ops(wcd,costart,out_ve,loop_rv,loop_ve,reverse)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: costart
    type(pm_ptr),intent(in):: loop_rv
    integer,intent(in):: loop_ve,out_ve
    logical,intent(in):: reverse
    integer:: i,sym,start,finish,step,cs

    cs=wcd%cs
    
    if(reverse) then
       start=wcd%cotop(cs)
       finish=costart
       step=-1
    else
       start=costart
       finish=wcd%cotop(cs)
       step=1
    endif

    if(debug_wcode) then
       write(*,*) 'COMBINE OPS>',start,finish,step
    endif
    
    do i=start,finish,step
       sym=-cnode_get_num(wcd%costack(cs,i)%p,call_sig)
       select case(sym)
       case(sym_if,sym_for,sym_do,sym_loop)
          continue
       case(sym_while,sym_until,sym_each)
          call combine_loops(wcd,i,finish,step,wcd%costack(cs,i)%p,out_ve,&
               loop_rv,loop_ve)
          return
       case(sym_colon,sym_sync)
          call combine_labels(wcd,sym,i,finish,step,wcd%costack(cs,i)%p,out_ve,&
               loop_rv,loop_ve)
          return
       case default
          if(sym<0) then
             write(*,*) 'SYM=',sym
          else
             write(*,*) 'SYM=',sym_names(sym)
          endif
          call pm_panic('combine ops')
       end select
    enddo
  end subroutine combine_ops

  !=======================================================================
  ! Combine labelled communicating statements on different branches
  ! Labelled statements must be pushed at start..finish by step on costack
  !=======================================================================
  subroutine combine_labels(wcd,sig,start,finish,step,first_p,out_ve,&
       loop_rv,loop_ve)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: sig,start,finish,step
    type(pm_ptr),intent(in):: first_p,loop_rv
    integer,intent(in):: loop_ve,out_ve
    type(pm_ptr):: p,n,args,rv
    type(pm_ptr):: name,name2
    integer:: j,sig2,base,ve,cs

    cs=wcd%cs
    
    name=cnode_arg(cnode_arg(cnode_get(first_p,call_args),1),1)
    call check_label(wcd,first_p,name)
    args=cnode_get(first_p,call_args)
    rv=wcd%costack(cs,start)%rv
    ve=wcd%costack(cs,start)%ve
    call wcode_comm_block(wcd,cnode_arg(args,2),out_ve,rv,ve)
    do j=start+step,finish,step
       p=wcd%costack(cs,j)%p
       sig2=-cnode_get_num(p,call_sig)       
       select case(sig2)
       case(sym_if,sym_for,sym_do,sym_loop)
          cycle
       case(sym_colon,sym_sync)
          args=cnode_get(p,call_args)
          base=wcd%costack(cs,j)%base
          rv=wcd%costack(cs,j)%rv
          ve=wcd%costack(cs,j)%ve
          name2=cnode_arg(cnode_arg(args,1),1)
          if(name%offset/=name2%offset) then
             call mismatch(wcd,first_p,p,&
                  '"sync" labels do not match: '//&
                  trim(pm_name_as_string(wcd%context,int(name%offset)))//' / '//&
                  trim(pm_name_as_string(wcd%context,int(name2%offset))))
          endif
          call wcode_comm_block(wcd,cnode_arg(args,2),out_ve,rv,ve)
       case(sym_sync_while)
          call mismatch(wcd,first_p,p,&
               '"sync" statement matched to "sync while"')
       end select
    enddo
    
  end subroutine combine_labels

  !====================================================================
  ! Combine communicating loops on different branches
  !====================================================================
  recursive subroutine combine_loops(wcd,costart,cofinish,costep,first_p,out_ve,&
       loop_rv,loop_ve)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: costart,cofinish,costep
    type(pm_ptr),intent(in):: first_p
    type(pm_ptr),intent(in):: loop_rv
    integer,intent(in):: loop_ve,out_ve
    integer:: i,start,sym,newcostart,base,n
    integer:: j,k
    logical:: break,anybreak,allbreak,ispar,reverse
    integer:: ve,new_ve,mask
    type(pm_ptr):: args,p,oldp,rv,name,name2
    integer:: cs,numve
    if(debug_wcode) then
       write(*,*) 'COMBINE LOOPS',costart,cofinish,costep
    endif
    numve=0
    cs=wcd%cs
    rv=wcd%costack(cs,costart)%rv
    ispar=loop_is_par(wcd,first_p,rv)
    name=cnode_arg(cnode_arg(cnode_get(first_p,call_args),1),1)
    call check_label(wcd,first_p,name)
    if(pm_is_compiling) then
       mask=alloc_var(wcd,int(pm_logical))
    endif
    do i=costart,cofinish,costep
       p=wcd%costack(cs,i)%p
       sym=-cnode_get_num(p,call_sig)
       if(sym==sym_if.or.sym==sym_do.or.sym==sym_for) cycle
       args=cnode_get(p,call_args)
       numve=numve+1
       ve=wcd%costack(cs,i)%ve
       base=wcd%costack(cs,i)%base
       rv=wcd%costack(cs,i)%rv
       if(pm_is_compiling) then
          wcd%costack(cs,i)%new_ve=ve
          new_ve=mask
       else
          new_ve=alloc_var(wcd,pm_ve_type)
          wcd%costack(cs,i)%new_ve=new_ve
       endif
       select case(sym)
       case(sym_while)
          if(wcode_cblock(wcd,cnode_arg(args,2),rv,ve)) then
             call wcode_error(wcd,args,&
                  'Communicating operation inside "while" test expression')
          endif
          call wc_call(wcd,p,op_and_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       case(sym_until)
          if(pm_is_compiling) then
             call wc_call(wcd,p,op_and_ve,0,3,1,ve)
             call wc(wcd,-new_ve)
             call wc(wcd,cvar_const_value(wcd,wcd%true_obj))
          else
             call wc_call(wcd,p,op_clone_ve,int(new_ve),1,1,ve)
          endif
       case(sym_each)
          call wc_call(wcd,p,op_and_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       case(sym_while_invar,sym_until_invar,sym_foreach_invar)
          call mismatch(wcd,args,first_p,&
               'labelled non-"invar" loop matched with "invar" loop')
       case default
          if(sym==-sym_colon) then
             call mismatch(wcd,args,first_p,&
                  'communicating loop matched with label')
          else
             call mismatch(wcd,args,first_p,&
                  'communicating loop matched with '//&
                  'single communicating operation')
          endif
       end select
       name2=cnode_arg(cnode_arg(args,1),1)
       if(pm_fast_isnull(name2).and..not.(p==first_p)) then
          call wcode_error(wcd,p,'Communicating loop needs to be labelled')
       elseif(name%offset/=name2%offset) then
          if(pm_fast_isnull(name)) then
             call wcode_error(wcd,first_p,'Communicating loop needs to be labelled')
          else
             call mismatch(wcd,p,first_p,&
                  'Communicating loops have different labels: '//&
                  trim(pm_name_as_string(wcd%context,int(name%offset)))//' <> '//&
                  trim(pm_name_as_string(wcd%context,int(name2%offset))))
          endif
       endif
    enddo
    if(pm_is_compiling) then
       call wc_call(wcd,first_p,&
            merge(op_comm_loop_par,op_comm_loop,ispar),0,3,1,ve)
       start=comp_start_block(wcd)
       call wc(wcd,-mask)
    else
       start=wcd%pc
    endif
    anybreak=.false.
    allbreak=.true.
    newcostart=wcd%cotop(wcd%cs)+1
    n=0
    do i=costart,cofinish,costep
       p=wcd%costack(cs,i)%p
       sym=-cnode_get_num(p,call_sig)
       if(sym==sym_if.or.sym==sym_do.or.sym==sym_for) cycle
       args=cnode_get(p,call_args)
       base=wcd%costack(cs,i)%base
       rv=wcd%costack(cs,i)%rv
       new_ve=wcd%costack(cs,i)%new_ve
       select case(sym)
       case(sym_while)
          break=wcode_cblock(wcd,cnode_arg(args,4),rv,new_ve)
          n=n+1
       case(sym_until) 
          break=wcode_cblock(wcd,cnode_arg(args,2),rv,new_ve)
          n=n+1
       case(sym_each) 
          break=wcode_cblock(wcd,cnode_arg(args,2),rv,new_ve)
          n=n+1
       end select
       anybreak=anybreak.or.break
       allbreak=allbreak.and.break
       if(anybreak.and..not.allbreak) then
          call mismatch(wcd,first_p,p,&
               'communicating operators do not match in'//&
               ' corresponding loops')
       endif
    enddo
    reverse=.true.
    do while(anybreak)
       anybreak=.false.
       allbreak=.true.
       reverse=.not.reverse
       call combine_ops(wcd,newcostart,out_ve,loop_rv,loop_ve,reverse)
       wcd%cs=3-wcd%cs
       newcostart=wcd%cotop(wcd%cs)+1
       do i=1,n
          p=wcd%costack(3-wcd%cs,wcd%cotop(3-wcd%cs))%p
          break=restart_cblock(wcd,ve)
          anybreak=anybreak.or.break
          allbreak=allbreak.and.break
          if(anybreak.and..not.allbreak) then
             call mismatch(wcd,oldp,p,&
                  'communicating operators do not match in corresponding loops')
          endif
          oldp=p
       enddo
    enddo
    do i=costart,cofinish,costep
       p=wcd%costack(cs,i)%p
       sym=-cnode_get_num(p,call_sig)
       args=cnode_get(p,call_args)
       ve=wcd%costack(cs,i)%new_ve
       if(pm_is_compiling) then
          new_ve=mask
       else
          new_ve=ve
       endif
       base=wcd%costack(cs,i)%base
       rv=wcd%costack(cs,i)%rv
       if(sym==sym_if.or.sym==sym_do.or.sym==sym_for) cycle
       select case(sym)
       case(sym_while)
          break=wcode_cblock(wcd,cnode_arg(args,2),rv,ve)
          call wc_call(wcd,p,op_and_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       case(sym_until) 
          call wc_call(wcd,p,op_andnot_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       case(sym_each) 
          call wc_call(wcd,p,op_and_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       end select
    enddo
 
    if(pm_is_compiling) then
       call comp_finish_block(wcd,start)
    else
       call wc_call(wcd,first_p,&
            merge(op_jmp_any_ve_par,op_jmp_any_ve,ispar),&
            start,numve+1,0,&
            loop_ve)
       do i=costart,cofinish,costep
          p=wcd%costack(cs,i)%p
          sym=-cnode_get_num(p,call_sig)
          if(sym==sym_while.or.sym==sym_until.or.sym==sym_each) then
             call wc(wcd,wcd%costack(cs,i)%new_ve)
          endif
       enddo
    endif
    wcd%cs=cs
  contains
    include 'fisnull.inc'
  end subroutine combine_loops


  !====================================================================
  ! Combine communicating loops on different branches
  !====================================================================
  recursive subroutine combine_invar_loops(wcd,costart,cofinish,costep,first_p,out_ve,&
       loop_rv,loop_ve)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: costart,cofinish,costep
    type(pm_ptr),intent(in):: first_p
    type(pm_ptr),intent(in):: loop_rv
    integer,intent(in):: loop_ve,out_ve
    integer:: i,start,sym,newcostart,base,n
    integer:: j,k
    logical:: break,anybreak,allbreak,ispar,reverse
    integer:: ve,new_ve,mask
    type(pm_ptr):: args,p,oldp,rv,name,name2
    integer:: cs,numve
    if(debug_wcode) then
       write(*,*) 'COMBINE LOOPS',costart,cofinish,costep
    endif
    numve=0
    cs=wcd%cs
    rv=wcd%costack(cs,costart)%rv
    ispar=loop_is_par(wcd,first_p,rv)
    name=cnode_arg(cnode_arg(cnode_get(first_p,call_args),1),1)
    call check_label(wcd,first_p,name)
    if(pm_is_compiling) then
       mask=alloc_var(wcd,int(pm_logical))
    endif
    do i=costart,cofinish,costep
       p=wcd%costack(cs,i)%p
       sym=-cnode_get_num(p,call_sig)
       if(sym==sym_if.or.sym==sym_do.or.sym==sym_for) cycle
       args=cnode_get(p,call_args)
       numve=numve+1
       ve=wcd%costack(cs,i)%ve
       base=wcd%costack(cs,i)%base
       rv=wcd%costack(cs,i)%rv
       if(pm_is_compiling) then
          wcd%costack(cs,i)%new_ve=ve
          new_ve=mask
       else
          new_ve=alloc_var(wcd,pm_ve_type)
          wcd%costack(cs,i)%new_ve=new_ve
       endif
       select case(sym)
       case(sym_while)
          if(wcode_cblock(wcd,cnode_arg(args,2),rv,ve)) then
             call wcode_error(wcd,args,&
                  'Communicating operation inside "while" test expression')
          endif
          call wc_call(wcd,p,op_and_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       case(sym_until)
          if(pm_is_compiling) then
             call wc_call(wcd,p,op_and_ve,0,3,1,ve)
             call wc(wcd,-new_ve)
             call wc(wcd,cvar_const_value(wcd,wcd%true_obj))
          else
             call wc_call(wcd,p,op_clone_ve,int(new_ve),1,1,ve)
          endif
       case(sym_each)
          call wc_call(wcd,p,op_and_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       case default
          if(sym==-sym_colon) then
             call mismatch(wcd,args,first_p,&
                  'communicating loop matched with label')
          else
             call mismatch(wcd,args,first_p,&
                  'communicating loop matched with '//&
                  'single communicating operation')
          endif
       end select
       name2=cnode_arg(cnode_arg(args,1),1)
       if(pm_fast_isnull(name2).and..not.(p==first_p)) then
          call wcode_error(wcd,p,'Communicating loop needs to be labelled')
       elseif(name%offset/=name2%offset) then
          if(pm_fast_isnull(name)) then
             call wcode_error(wcd,first_p,'Communicating loop needs to be labelled')
          else
             call mismatch(wcd,p,first_p,&
                  'Communicating loops have different labels: '//&
                  trim(pm_name_as_string(wcd%context,int(name%offset)))//' <> '//&
                  trim(pm_name_as_string(wcd%context,int(name2%offset))))
          endif
       endif
    enddo
    if(pm_is_compiling) then
       call wc_call(wcd,first_p,&
            merge(op_comm_loop_par,op_comm_loop,ispar),0,3,1,ve)
       start=comp_start_block(wcd)
       call wc(wcd,-mask)
    else
       start=wcd%pc
    endif
    anybreak=.false.
    allbreak=.true.
    newcostart=wcd%cotop(wcd%cs)+1
    n=0
    do i=costart,cofinish,costep
       p=wcd%costack(cs,i)%p
       sym=-cnode_get_num(p,call_sig)
       if(sym==sym_if.or.sym==sym_do.or.sym==sym_for) cycle
       args=cnode_get(p,call_args)
       base=wcd%costack(cs,i)%base
       rv=wcd%costack(cs,i)%rv
       new_ve=wcd%costack(cs,i)%new_ve
       select case(sym)
       case(sym_while)
          break=wcode_cblock(wcd,cnode_arg(args,4),rv,new_ve)
          n=n+1
       case(sym_until) 
          break=wcode_cblock(wcd,cnode_arg(args,2),rv,new_ve)
          n=n+1
       case(sym_each) 
          break=wcode_cblock(wcd,cnode_arg(args,2),rv,new_ve)
          n=n+1
       end select
       anybreak=anybreak.or.break
       allbreak=allbreak.and.break
       if(anybreak.and..not.allbreak) then
          call mismatch(wcd,first_p,p,&
               'communicating operators do not match in'//&
               ' corresponding loops')
       endif
    enddo
    reverse=.true.
    do while(anybreak)
       anybreak=.false.
       allbreak=.true.
       reverse=.not.reverse
       call combine_ops(wcd,newcostart,out_ve,loop_rv,loop_ve,reverse)
       wcd%cs=3-wcd%cs
       newcostart=wcd%cotop(wcd%cs)+1
       do i=1,n
          p=wcd%costack(3-wcd%cs,wcd%cotop(3-wcd%cs))%p
          break=restart_cblock(wcd,ve)
          anybreak=anybreak.or.break
          allbreak=allbreak.and.break
          if(anybreak.and..not.allbreak) then
             call mismatch(wcd,oldp,p,&
                  'communicating operators do not match in corresponding loops')
          endif
          oldp=p
       enddo
    enddo
    do i=costart,cofinish,costep
       p=wcd%costack(cs,i)%p
       sym=-cnode_get_num(p,call_sig)
       args=cnode_get(p,call_args)
       ve=wcd%costack(cs,i)%new_ve
       if(pm_is_compiling) then
          new_ve=mask
       else
          new_ve=ve
       endif
       base=wcd%costack(cs,i)%base
       rv=wcd%costack(cs,i)%rv
       if(sym==sym_if.or.sym==sym_do.or.sym==sym_for) cycle
       select case(sym)
       case(sym_while)
          break=wcode_cblock(wcd,cnode_arg(args,2),rv,ve)
          call wc_call(wcd,p,op_and_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       case(sym_until) 
          call wc_call(wcd,p,op_andnot_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       case(sym_each) 
          call wc_call(wcd,p,op_and_ve,0,3,1,ve)
          call wc(wcd,-new_ve)
          call wc_arg(wcd,cnode_arg(args,3),.false.,rv,ve)
       end select
    enddo
 
    if(pm_is_compiling) then
       call comp_finish_block(wcd,start)
    else
       call wc_call(wcd,first_p,&
            merge(op_jmp_any_ve_par,op_jmp_any_ve,ispar),&
            start,numve+1,&
            0,loop_ve)
       do i=costart,cofinish,costep
          p=wcd%costack(cs,i)%p
          sym=-cnode_get_num(p,call_sig)
          if(sym==sym_while.or.sym==sym_until.or.sym==sym_each) then
             call wc(wcd,wcd%costack(cs,i)%new_ve)
          endif
       enddo
    endif
    wcd%cs=cs
  contains
    include 'fisnull.inc'
  end subroutine combine_invar_loops

  
  !====================================================================
  ! Check if inference has flagged this loop as in a parallel context
  ! (and thus needing extra synchronisation)
  !====================================================================
  function loop_is_par(wcd,callnode,rv) result(ispar)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,rv
    logical:: ispar
    ispar=call_flag_set(wcd,callnode,rv)
  end function loop_is_par

  !====================================================================
  ! Check labels are only used once
  !====================================================================
  subroutine check_label(wcd,callnode,label)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,label
    integer:: i
    if(debug_wcode) then
       write(*,*) 'CHECK LABEL>',&
            trim(pm_name_as_string(wcd%context,int(label%offset))),&
            label%offset,wcd%lbbase,wcd%lbtop
    endif
    if(pm_fast_isnull(label)) return
    if(label%offset==0) return
    do i=wcd%lbbase+1,wcd%lbtop
       if(debug_wcode) then
          write(*,*) 'CHECK>>',&
               trim(pm_name_as_string(wcd%context,wcd%labels(i))),&
               wcd%labels(i),label%offset
       endif
       if(label%offset==wcd%labels(i)) then
          write(*,*) 'checked bad'
          call wcode_error(wcd,callnode,&
               'Label cannot be used twice within the same (or nested) parallel statement: '//&
               trim(pm_name_as_string(wcd%context,int(label%offset))))
       endif
    enddo
    wcd%lbtop=wcd%lbtop+1
    wcd%labels(wcd%lbtop)=label%offset
  contains
    include 'fisnull.inc'
  end subroutine check_label

  !====================================================================
  ! Start single block associated with an operation
  ! (compiler only)
  !====================================================================
  function comp_start_block(wcd) result(n)
    type(wcoder),intent(inout):: wcd
    integer:: n
    integer:: ve
    n=wcd%last_instr
    call wc(wcd,0)
  end function comp_start_block

  !====================================================================
  ! Complete single block associated with an operation
  ! (compiler only)
  !====================================================================
  subroutine comp_finish_block(wcd,n)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n
    if(n==wcd%last_instr.and..false.) then
       ! Empty block -- delete instruction
       wcd%pc=n
    else
       wcd%wc(n+6)=wcd%wc(n)
       wcd%wc(n)=0
       wcd%wc(wcd%last_instr)=0
       wcd%last_instr=n
    endif
  end subroutine comp_finish_block

  !====================================================================
  ! Start two blocks associated with an operation
  ! (compiler only)
  !====================================================================
  function comp_start_if_else_block(wcd) result(n)
    type(wcoder),intent(inout):: wcd
    integer:: n
    integer:: ve
    n=wcd%last_instr
    call wc(wcd,0)
    call wc(wcd,0)
  end function comp_start_if_else_block

  !====================================================================
  ! Start second (else) block associated with an operation
  ! (compiler only)
  !====================================================================
  subroutine comp_start_else_block(wcd,n)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n
    call comp_finish_block(wcd,n)
    wcd%wc(n+7)=wcd%pc
  end subroutine comp_start_else_block

  !====================================================================
  ! Complete second (else) block for
  ! an operation starting at n
  ! (compiler only)
  !====================================================================
  subroutine comp_finish_else_block(wcd,n)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n
    if(wcd%last_instr==n.and..false.) then
       ! Totally empty delete it
       wcd%pc=n
    else
       wcd%wc(wcd%last_instr)=0
       wcd%last_instr=n
       if(wcd%wc(n+7)==wcd%pc) then
          wcd%wc(n+7)=0
       endif
       wcd%wc(n)=0
    endif
  end subroutine comp_finish_else_block

  !====================================================================
  ! Swap the two branches of an if
  ! (compiler only)
  !====================================================================
  subroutine swap_if_branches(wcd,pc)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: pc
    integer:: temp
    temp=wcd%wc(pc+6)
    wcd%wc(pc+6)=wcd%wc(pc+7)
    wcd%wc(pc+7)=temp
  end subroutine swap_if_branches

  !====================================================================
  ! Code call to a jump operator
  !====================================================================
  function wc_jump_call(wcd,callnode,op_s,op_a,nargs,ve) result(pc)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode
    integer,intent(in):: op_s,ve
    integer,intent(in):: op_a
    integer,intent(in):: nargs
    integer:: pc
    pc=wcd%pc
    call wc_call(wcd,callnode,op_s,op_a,nargs,0,ve)   
  end function wc_jump_call

  !====================================================================
  ! Set the jump operation starting at position j
  ! to point to the current program counter
  ! (interpreter only)
  !====================================================================
  subroutine set_jump_to_here(wcd,j)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: j
    integer:: pc
    if(pm_is_compiling) return
    pc=wcd%pc
    wcd%wc(j+1)=pc/pm_ext_mult
    wcd%wc(j+2)=iand(int(wcd%wc(j+2)),pm_max_args)+&
         (pm_max_args+1)*iand(pc,pm_ext_mult-1)
  end subroutine set_jump_to_here

  !====================================================================
  ! Code call with arguments
  !====================================================================
  subroutine wc_call_args(wcd,callnode,args,op,op2,nargs,nret,rv,ve)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,args,rv
    integer,intent(in):: op,ve
    integer,intent(in):: op2
    integer,intent(in):: nargs,nret
    call wc_call(wcd,callnode,op,op2,nargs+1,nret,ve)
    call wc_arglist(wcd,callnode,args,nargs,nret,rv,ve)
  end subroutine wc_call_args

  !====================================================================
  ! Code argument list
  !====================================================================
  subroutine wc_arglist(wcd,callnode,args,nargs,nret,rv,ve)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,args,rv
    integer,intent(in):: ve
    integer,intent(in):: nargs,nret
    integer:: i
    integer:: slot
    type(pm_ptr):: arg

    do i=1+nret,nargs
       arg=cnode_arg(args,i)
       if(arg_is_svar(arg)) then
          slot=var_slot(wcd,arg)
          call release_var(wcd,slot)
       endif
    enddo
    do i=1,nargs
       arg=cnode_arg(args,i)
       if(i>nret.and.arg_is_svar(arg)) then
          call wc(wcd,var_slot(wcd,arg))
       else
          call wc_arg(wcd,arg,i<=nret,rv,ve)
       endif
    enddo
  end subroutine wc_arglist

  !====================================================================
  ! Just code call header -- arguments must be coded
  ! seperately
  !====================================================================
  subroutine wc_call(wcd,node,op,op2,nargs,nret,ve)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: node
    integer,intent(in):: op,ve
    integer,intent(in):: op2
    integer,intent(in):: nargs,nret
    integer:: modl,line,last
    integer:: flags
    !write(*,*) 'wc_call',trim(op_names(op)),' nret=',nret
    if(.not.(pm_fast_isnull(wcd%inline_args).or.pm_is_compiling)) then
       modl=cnode_get_num(wcd%inline_args,cnode_modl_name)
       line=cnode_get_num(wcd%inline_args,cnode_lineno)
    else
       modl=cnode_get_num(node,cnode_modl_name)
       !write(*,*) 'name=',modl,pm_name_as_string(wcd%context,modl)
       line=cnode_get_num(node,cnode_lineno)
    endif
    if(pm_is_compiling) then
       last=wcd%last_instr
       wcd%last_instr=wcd%pc
       if(last/=0) then
          wcd%wc(last)=wcd%pc
       endif
       if(op==op_elem) call pm_panic('op_elem should not be here')
       call wc(wcd,0)
       call wc(wcd,modl+line*modl_mult)
    else
       if(modl/=0.and.line/=0) then
          if(modl/=wcd%cur_modl) then
             wcd%last=wcd%last-2
             if(wcd%last<=wcd%pc) call expand_wc(wcd)
             wcd%wc(wcd%last+1)=wcd%pc
             wcd%wc(wcd%last+2)=modl
             wcd%cur_modl=modl
          endif
          if(line/=wcd%cur_line) then
             wcd%last=wcd%last-2
             if(wcd%last<=wcd%pc) call expand_wc(wcd)
             wcd%wc(wcd%last+1)=wcd%pc
             wcd%wc(wcd%last+2)=-line
             wcd%cur_line=line
          endif
       endif
    endif

    
    if(pm_debug_checks) then
       if(cnode_get_kind(node)/=cnode_is_call) then
          call pm_panic('wc call not callnode')
       endif
    endif
    
    flags=cnode_get_num(node,call_flags)
    if(.false..and.(.not.pm_is_compiling).and.&
         iand(flags,proccall_is_comm+proccall_is_general+proccall_is_ref)/=0) then
       if(pm_is_compiling) then
          call wc(wcd,op)
          call wc(wcd,op2)
          call wc(wcd,nargs+nret*comp_op_nret_div+comp_op_shared)
          call wc(wcd,shared_op_flag)
       else
          call wc_simple_call(wcd,op,op2,nargs,&
               wcd%shared_ve)
       endif
    else
       if(pm_is_compiling) then
          call wc(wcd,op)
          call wc(wcd,op2)
          call wc(wcd,nargs+nret*comp_op_nret_div)
          call wc(wcd,ve)
       else
          call wc_simple_call(wcd,op,op2,nargs,ve)
       endif
    endif
   
  contains
    include 'fisnull.inc'
  end subroutine wc_call

  !====================================================================
  ! Works in compiler only
  !====================================================================
  subroutine wc_correct_call_args(wcd)
    type(wcoder),intent(inout):: wcd
    wcd%wc(wcd%last_instr+4)=wcd%pc-wcd%last_instr-5+&
         iand(int(comp_op_nret_mask+comp_op_shared,pm_wc),wcd%wc(wcd%last_instr+4))
  end subroutine wc_correct_call_args

  !====================================================================
  ! Compiler-mode version of wc_simple_call
  !====================================================================
  subroutine wc_simple_comp_call(wcd,op,op2,nargs,ve)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: op,ve
    integer,intent(in):: op2
    integer,intent(in):: nargs
    integer:: last
    last=wcd%last_instr
    wcd%last_instr=wcd%pc
    if(last/=0) then
       wcd%wc(last)=wcd%pc
    endif
    call wc(wcd,0)
    call wc(wcd,0)
    call wc(wcd,op)
    call wc(wcd,op2)
    call wc(wcd,nargs)
    call wc(wcd,ve)
  end subroutine wc_simple_comp_call

  !=======================================================================
  ! Code very basic call (limited usefulness - don't use in compiler mode)
  !=======================================================================
  subroutine wc_simple_call(wcd,op,op2,nargs,ve)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: op,ve
    integer,intent(in):: op2
    integer,intent(in):: nargs
    call wc(wcd,op)
    if(pm_is_compiling) then
       call pm_panic('simple-call -- compiling')
    else
       call wc(wcd,op2/pm_ext_mult)
       call wc(wcd,nargs+(pm_max_args+1)*&
            iand(op2,pm_ext_mult-1))
    endif
    call wc(wcd,ve)
  end subroutine wc_simple_call

  !====================================================================
  ! Code one argument of operation
  !====================================================================
  subroutine wc_arg(wcd,argnode,isret,rv,ve)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: argnode
    logical,intent(in):: isret
    type(pm_ptr),intent(in):: rv
    integer,intent(in):: ve
    integer:: k,t,akind
    integer(pm_p):: sym
    integer:: slot,rslot
    type(pm_ptr):: arg,ass
    integer:: typ
    arg=argnode
    akind=cnode_get_kind(arg)
    if(pm_is_compiling) then
       if(akind==cnode_is_var) then
          slot=cnode_get_num(arg,var_index)+wcd%base
          k=wcd%rdata(slot)
          if(isret) then
             call wc(wcd,-k)
          else
             call wc(wcd,k)
          endif
       else
          call wc(wcd,cvar_const(wcd,arg))
       endif
       return
    endif
    if(arg_is_svar(arg)) then
       ! Single use variable
       if(isret) then 
          ! Make temp var
          k=alloc_general_var(wcd,arg,rv)
          call wc(wcd,-k)
          slot=cnode_get_num(arg,var_index)
          wcd%rdata(slot+wcd%base)=k
          if(debug_wcode) then
             write(*,*) 'MAKE_TEMP_VAR>',slot,slot+wcd%base,k
          endif
       else
          ! Get temp var
          slot=cnode_get_num(arg,var_index)
          k=wcd%rdata(slot+wcd%base)
          call wc(wcd,k)
          call release_var(wcd,k)
          if(debug_wcode)  then
             write(*,*) 'USE TEMP VAR>',slot,k
          endif
       endif
    else if(cnode_get_kind(arg)==cnode_is_var) then
       ! Multiple use variable
       slot=cnode_get_num(arg,var_index)+wcd%base
       if(pm_debug_checks) then
          rslot=wcd%rdata(slot)
          if(rslot>0) then
             if(wcd%ref_count(rslot-pm_stack_locals+1)==0) then
                call wcode_error(wcd,arg,'Var made=')
                write(*,*) rslot,trim(pm_name_as_string(wcd%context,&
                     cnode_get_name(arg,var_name))),wcd%pc
                call pm_panic('var-refcnt==0')
             endif
          endif
       endif
       if(isret) then
          call wc(wcd,-wcd%rdata(slot))
       else
          call wc(wcd,wcd%rdata(slot))
       endif
    else
       if(pm_debug_checks) then
          if(isret) &
               call pm_panic(&
               'wc_arg: Attempt to return to const')
       endif
       ! Constant
       call wc(wcd,&
            -pm_max_stack-add_const(wcd,cnode_arg(arg,1)))
    endif
  contains
    include 'fisnull.inc'

    function rvv(n) result(m)
      integer(pm_p),intent(in):: n
      integer:: m
      m=rv%data%i(rv%offset+n)
    end function rvv

  end subroutine wc_arg

  !=======================================================================
  ! Code argument without freeing slot if it is single use
  !=======================================================================
  subroutine wc_sarg(wcd,arg,lastuse,rv,ve)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: arg
    logical:: lastuse
    type(pm_ptr),intent(in):: rv
    integer,intent(in):: ve
    if(.not.lastuse) then
       if(.not.arg_is_svar(arg)) then
          call wc_arg(wcd,arg,.false.,rv,ve)
       else
          call wc(wcd,var_slot(wcd,arg))
       endif
    else
       call wc_arg(wcd,arg,.false.,rv,ve)
    endif
  end subroutine wc_sarg

  !====================================================================
  ! Get slot # associated with an argument
  !====================================================================
  function arg_slot(wcd,arg) result(n)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: arg
    integer:: n
    if(cnode_get_kind(arg)==cnode_is_var) then
       n=wcd%rdata(cnode_get_num(arg,var_index)+wcd%base)
    else
       if(pm_is_compiling) then
          n=cvar_const(wcd,arg)
       else
          n=-pm_max_stack-add_const(wcd,cnode_arg(arg,1))
       endif
    endif
  end function arg_slot

  !====================================================================
  ! Set the slot # associated with an argument
  !====================================================================
  subroutine arg_set_slot(wcd,arg,slot)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: arg
    integer,intent(in):: slot
    if(cnode_get_kind(arg)==cnode_is_var) then
       wcd%rdata(cnode_get_num(arg,var_index)+wcd%base)=slot
    else
       call pm_panic('arg_set_slot on const')
    endif
  end subroutine arg_set_slot

  !====================================================================
  ! Argument slot # with respect to a given frame (needed for inlining)
  !====================================================================
  function arg_slot_in_frame(wcd,arg,base) result(n)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: arg
    integer,intent(in):: base
    integer:: n
    if(cnode_get_kind(arg)==cnode_is_var) then
       n=wcd%rdata(cnode_get_num(arg,var_index)+base)
       if(debug_wcode) write(*,*) 'ISVAR>',n
    else
       if(pm_is_compiling) then
          n=cvar_const(wcd,arg)
          if(debug_wcode) write(*,*) 'ISCONST>',n
       else
          n=-pm_max_stack-add_const(wcd,cnode_arg(arg,1))
       endif
    endif
  end function arg_slot_in_frame

  !====================================================================
  ! Link arg1 <- arg2
  !====================================================================
  subroutine link_to_val(wcd,callnode,arg1,base1,arg2,base2,rv,ve)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: arg1,arg2
    integer,intent(in):: base1,base2
    type(pm_ptr),intent(in):: callnode,rv
    integer,intent(in):: ve
    integer:: slot1,slot2
    integer:: var,var2
    integer:: typ

    type(pm_ptr)::pp

    if(pm_is_compiling) then
       slot1=arg_slot_in_frame(wcd,arg1,base1)
       slot2=arg_slot_in_frame(wcd,arg2,base2)
       if(slot1<0) then
          slot1=cnode_get_num(arg1,var_index)+base1
          wcd%rdata(slot1)=slot2
       else
          call comp_alias_slots(wcd,slot1,slot2)
       endif
    else
       
       slot1=cnode_get_num(arg1,var_index)+base1
       var=arg_slot_in_frame(wcd,arg2,base2)
       if(debug_wcode) write(*,*) 'LINK>',wcd%rdata(slot1),var
       wcd%rdata(slot1)=var
       if(var<=0) return
       if(arg_is_mvar(arg2)) then
          wcd%ref_count(var-pm_stack_locals+1)=&
               wcd%ref_count(var-pm_stack_locals+1)+1
       endif
    endif
  end subroutine link_to_val
  
  !====================================================================
  ! Add a new constant to the current procedures constant pool
  !====================================================================
  function add_const(wcd,val) result(n)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: val
    integer:: n

    n=wcd%nval+1
    wcd%nval=n
    if(pm_fast_vkind(val)==pm_name) then
       wcd%values(n)=pm_name_val(wcd%context,int(val%offset))
    else
       wcd%values(n)=val
    endif
    n=n+1
  contains
    include 'fvkind.inc'
  end function add_const

  !====================================================================
  ! Allocate keyword parameter variable
  !====================================================================
  function alloc_key_var(wcd,typ) result(k)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: typ
    integer:: k
    k=cvar_alloc_slots(wcd,5)
    call cvar_set_info(wcd,k,v_is_group,2,v_is_tuple,typ)
    call cvar_set_ptr(wcd,k,1,cvar_alloc(wcd,int(pm_logical),&
         v_is_param))
    call cvar_set_ptr(wcd,k,2,cvar_alloc(wcd,typ,v_is_param))
  end function alloc_key_var

  !====================================================================
  ! Allocate parameter variable
  !====================================================================
  function alloc_param_var(wcd,typ,isref,iskey,name) result(k)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: typ,name
    logical,intent(in):: isref,iskey
    integer:: k
    integer:: flags
    !write(*,*) 'PARAM>>',trim(pm_type_as_string(wcd%context,typ))
    if(pm_is_compiling) then
       flags=v_is_param
       if(isref)   flags=ior(flags,v_is_ref)
       if(iskey)   flags=ior(flags,v_is_key)
       k=cvar_alloc(wcd,typ,flags,name)
    else
       k=alloc_var(wcd,typ)
    endif
  end function alloc_param_var

  !====================================================================
  ! Allocate result variable
  !====================================================================
  function alloc_result_var(wcd,typ) result(k)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: typ
    integer:: k
    integer:: flags
    if(pm_is_compiling) then
       flags=v_is_result
       k=cvar_alloc(wcd,typ,flags)
    else
       k=alloc_var(wcd,typ)
    endif
  end function alloc_result_var
  
  !====================================================================
  ! Allocate local variable
  !====================================================================
  function alloc_general_var(wcd,var,rv) result(k)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: var,rv
    integer:: k
    integer:: typ,flags
    if(pm_is_compiling) then
       typ=get_var_type(wcd,var,rv)
       flags=0
       if(arg_is_par_var(var)) flags=ior(flags,v_is_par)
       k=cvar_alloc(wcd,typ,flags,cnode_get_num(var,var_name))
       if(debug_wcode) then
          write(*,*) 'ALLOC GENERAL VAR',cnode_get_num(var,var_index),';',k,'::',cvar_kind(wcd,k),':',&
               trim(pm_name_as_string(wcd%context,cnode_get_name(var,var_name))),&
               ':',trim(pm_type_as_string(wcd%context,typ))
       endif
    else
       k=alloc_var(wcd,0)
    endif
  end function alloc_general_var

  !====================================================================
  ! Allocate variable
  !====================================================================
  function alloc_var(wcd,typ) result(k)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: typ
    integer:: i
    integer::k
    if(pm_is_compiling) then
       k=cvar_alloc(wcd,typ,0)
       return
    endif
    if(wcd%nvar==wcd%avar) then
       wcd%nvar=wcd%nvar+1
       if(wcd%nvar>wcd%mvar) wcd%mvar=wcd%nvar
       if(wcd%nvar>pm_max_stack) &
            call pm_panic('Program too complex:out of variables')
       wcd%avar=wcd%nvar
       wcd%ref_count(wcd%nvar)=1
       k=wcd%nvar
    else
       k=-21
       wcd%avar=wcd%avar+1
       do i=1,wcd%nvar
          if(wcd%ref_count(i)==0) then
             wcd%ref_count(i)=1
             k=i
             exit
          endif
       enddo
       if(pm_debug_checks) then
          if(k==-21) then
             write(*,*) wcd%nvar,wcd%avar
             call pm_panic('allocate var k')
          endif
       endif
    endif
    k=k+pm_stack_locals-1
    if(pm_debug_level>3) write(*,*) 'Alloc var:',k
  end function alloc_var

  !====================================================================
  ! Release variable
  !====================================================================
  subroutine release_var(wcd,slot)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: slot
    integer:: k
    if(pm_is_compiling) return
    if(slot<0) return
    k=slot-pm_stack_locals+1
    if(pm_debug_checks) then
       if(k<1.or.k>wcd%mvar) then
          write(*,*) 'k=',slot,k,wcd%mvar
          call pm_panic('release var')
       endif
    endif
    if(wcd%ref_count(k)==0) then
       return
    elseif(wcd%ref_count(k)>1) then
       wcd%ref_count(k)=wcd%ref_count(k)-1
       return
    endif
    if(k==wcd%nvar) then
       wcd%nvar=wcd%nvar-1
    endif
    wcd%ref_count(k)=0
    wcd%avar=wcd%avar-1
    if(debug_wcode) then
       write(*,*) 'RELEASE VAR>',slot,k,'Alloced=',wcd%avar,'Tot=',wcd%nvar
    endif
  end subroutine release_var

  !====================================================================
  ! Return slot associated with variable
  !====================================================================
  function var_slot(wcd,arg) result(slot)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: arg
    integer:: slot
    integer:: i
    i=cnode_get_num(arg,var_index)
    slot=wcd%rdata(wcd%base+i)
  end function var_slot

  !====================================================================
  ! Return type associated with variable (when compiling includes mode)
  !====================================================================
  function get_var_type(wcd,arg,rv) result(typ)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: arg,rv
    integer:: typ
    integer:: i
    i=cnode_get_num(arg,var_index)
    typ=strip_mode_for_interp(wcd,rv%data%i(rv%offset+i))
  end function get_var_type

  !====================================================================
  ! Get type of argument args[n] - strip mode
  !====================================================================
  function check_arg_type(wcd,args,rv,n) result(tno)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: args,rv
    integer,intent(in):: n
    integer:: tno
    type(pm_ptr):: var,arg
    integer:: i,k,mode
    var=cnode_arg(args,n)
    k=cnode_get_num(var,cnode_kind)
    if(k==cnode_is_const) then
       tno=pm_type_strip_mode(wcd%context,cnode_get_num(var,node_args+1),mode)
       return
    endif
    i=cnode_get_num(var,var_index)
    tno=pm_type_strip_mode(wcd%context,rv%data%i(rv%offset+i),mode)
  contains
    include 'ftypeof.inc'
  end function check_arg_type

  !====================================================================
  ! Get type of argument args[n] - keep mode
  !====================================================================
  function check_arg_type_with_mode(wcd,args,rv,n) result(tno)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: args,rv
    integer,intent(in):: n
    integer:: tno
    type(pm_ptr):: var,arg
    integer(pm_p):: i,k
    var=cnode_arg(args,n)
    k=cnode_get_num(var,cnode_kind)
    if(k==cnode_is_const) then
       tno=cnode_get_num(var,node_args+1)
       return
    endif
    i=cnode_get_num(var,var_index)
    tno=rv%data%i(rv%offset+i)
  contains
    include 'ftypeof.inc'
  end function check_arg_type_with_mode

  !====================================================================
  ! Return type of arg (with mode for compiler)
  !====================================================================
  function get_arg_type(wcd,arg,rv) result(tno)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: arg,rv
    integer:: tno
    integer(pm_p):: i,k
    integer:: mode
    k=cnode_get_num(arg,cnode_kind)
    if(k==cnode_is_const) then
       tno=strip_mode_for_interp(wcd,cnode_get_num(arg,node_args+1))
       return
    endif
    i=cnode_get_num(arg,var_index)
    tno=strip_mode_for_interp(wcd,rv%data%i(rv%offset+i))
  contains
    include 'ftypeof.inc'
  end function get_arg_type

  !====================================================================
  ! Strip mode from type, if not compiling
  !====================================================================
  function strip_mode_for_interp(wcd,tno) result(tno2)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: tno
    integer:: tno2
    integer:: mode
    if(pm_is_compiling) then
       tno2=tno
    else
       tno2=pm_type_strip_mode(wcd%context,tno,mode)
    endif
  end function  strip_mode_for_interp
  
  !====================================================================
  ! Is argument a variable?
  !====================================================================
  function arg_is_var(arg) result(ok)
    type(pm_ptr),intent(in):: arg
    logical:: ok
    ok=cnode_get_kind(arg)==cnode_is_var
  end function arg_is_var

  !====================================================================
  ! Is argument a single use variable?
  !====================================================================
  function arg_is_svar(arg) result(ok)
    type(pm_ptr),intent(in):: arg
    logical:: ok
    ok=.false.
    if(cnode_get_kind(arg)==cnode_is_var) then
       if(cnode_flags_clear(arg,var_flags,&
            ior(var_is_multi_access,var_is_changed))) then
          ok=.true.
       endif
    endif
  end function arg_is_svar

  !====================================================================
  ! Is argument a multiple use variable?
  !====================================================================
  function arg_is_mvar(arg) result(ok)
    type(pm_ptr),intent(in):: arg
    logical:: ok
    ok=.false.
    if(cnode_get_kind(arg)==cnode_is_var) then
      if(.not.cnode_flags_clear(arg,var_flags,&
            ior(ior(var_is_multi_access,var_is_changed),var_is_param))) then
         ok=.true.
      endif
    endif
  end function arg_is_mvar


  !====================================================================
  ! Argument is variable created in a par statement
  !====================================================================
  function arg_is_par_var(arg) result(ok)
    type(pm_ptr),intent(in):: arg
    logical:: ok
    ok=.false.
    if(cnode_get_kind(arg)==cnode_is_var) then
       if(.not.cnode_flags_clear(arg,var_flags,var_is_par_var)) then
         ok=.false.
      endif
    endif
  end function arg_is_par_var

  !====================================================================
  ! Code one word of code
  !====================================================================
  subroutine wc(wcd,val)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: val
    if(wcd%pc>=wcd%last) then
       call expand_wc(wcd)
    endif
    wcd%wc(wcd%pc)=val
    wcd%pc=wcd%pc+1
    if(debug_wcode_wc) then
       write(*,*) 'WC',wcd%pc-1,val,wcd%wc(wcd%pc-1)
    endif
  end subroutine wc


  !====================================================================
  ! Make more space for word-codes
  !====================================================================
  subroutine expand_wc(wcd)
    type(wcoder),intent(inout):: wcd
    integer,allocatable,dimension(:):: temp
    integer:: oldsize,newlast
    temp=wcd%wc
    oldsize=wcd%wc_size
    wcd%wc_size=oldsize*2
    deallocate(wcd%wc)
    allocate(wcd%wc(wcd%wc_size))
    wcd%wc(1:wcd%pc-1)=temp(1:wcd%pc-1)
    if(pm_is_compiling) then
       wcd%last=wcd%wc_size
    else
       newlast=wcd%last-oldsize+wcd%wc_size
       wcd%wc(newlast:wcd%wc_size)=temp(wcd%last:oldsize)
       wcd%last=newlast
    endif
    deallocate(temp)
  end subroutine expand_wc

  !====================================================================
  ! Get element of arg #2 returning it in arg #1
  !====================================================================
  subroutine wc_args_get_elem(wcd,callnode,op,args,elem,rv,ve)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: op,ve
    integer,intent(in):: elem
    type(pm_ptr):: callnode,rv,args
    call wc_arg_get_elem(wcd,callnode,op,cnode_arg(args,1),&
         cnode_arg(args,2),elem,rv,ve)
  end subroutine wc_args_get_elem

  !====================================================================
  ! Get elementof argin returning it in argout
  !====================================================================
  subroutine wc_arg_get_elem(wcd,callnode,op,argout,argin,elem,rv,ve)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: op,ve
    integer,intent(in):: elem
    type(pm_ptr),intent(in):: callnode,rv,argout,argin
    integer:: aslot
    if(pm_is_compiling) then
       aslot=arg_slot(wcd,argout)
       if(cvar_kind(wcd,aslot)/=v_is_ctime_const) then
          call comp_get_elem(wcd,op,&
               aslot,arg_slot(wcd,argin),elem)
       endif
    else
       call wc_call(wcd,callnode,op,merge(elem,elem+1,pm_is_compiling),3,1,ve)
       call wc_arg(wcd,argout,.true.,rv,ve)
       call wc_arg(wcd,argin,.false.,rv,ve)
    endif
  end subroutine wc_arg_get_elem


  !====================================================================
  ! Create a disagregated argument list
  ! for a given argument
  !====================================================================
  subroutine wc_p_arg(wcd,arg,isret,rv,ve,keep_ctime_const)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: arg
    logical,intent(in):: isret
    type(pm_ptr),intent(in):: rv
    integer,intent(in):: ve
    logical,intent(in):: keep_ctime_const
    integer:: k
    integer:: slot,tno,mode
    if(pm_is_compiling) then
       if(cnode_get_kind(arg)==cnode_is_var) then
          slot=cnode_get_num(arg,var_index)+wcd%base
          k=wcd%rdata(slot)
          if(isret) then
             call wc_p(wcd,-k,keep_ctime_const)
          else
             call wc_p(wcd,k,keep_ctime_const)
          endif
       else
          tno=cnode_get_num(arg,node_args+1)
          tno=pm_type_strip_mode(wcd%context,tno,mode)
          if(tno>pm_null.and.&
               tno<=pm_string.or.tno==pm_string_type) then
             call wc(wcd,cvar_const(wcd,arg))
          elseif(keep_ctime_const) then
             if(pm_type_kind(wcd%context,tno)==pm_type_is_fix_value) then
                call wc(wcd,cvar_const(wcd,arg))
             endif
          endif
       endif
    else
       call wc_arg(wcd,arg,isret,rv,ve)
    endif
  end subroutine wc_p_arg

  !====================================================================
  ! Create a disagregated argument list
  ! for a given variable
  ! Compile time constants are eliminated
  ! unless keep_ctime_const is true
  !====================================================================
  recursive subroutine wc_p(wcd,a,keep_ctime_const)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: a
    logical,intent(in):: keep_ctime_const
    integer:: slot,slota
    integer:: i,k
    if(pm_is_compiling.and..false.) then
       slot=cvar_strip_alias(wcd,abs(a))
!!$       write(*,*) 'WC_P'
!!$       call dump_cvar(wcd,6,slot,nonest=.true.)
       k=cvar_kind(wcd,slot)
       if(k==v_is_group) then
          select case(cvar_v2(wcd,slot))
          case(v_is_struct,v_is_tuple)
             do i=1,cvar_v1(wcd,slot)
                call wc_p(wcd,sign(cvar_ptr(wcd,slot,i),a),keep_ctime_const)
             enddo
          case(v_is_array,v_is_var_array)
             call wc_p(wcd,sign(cvar_ptr(wcd,slot,1),a),keep_ctime_const)
             call wc_p(wcd,cvar_ptr(wcd,slot,2),keep_ctime_const)
          case(v_is_dref,v_is_shared_dref)
             call wc_p(wcd,cvar_ptr(wcd,slot,1),keep_ctime_const)
             call wc_p(wcd,sign(cvar_ptr(wcd,slot,2),a),keep_ctime_const)
             call wc_p(wcd,cvar_ptr(wcd,slot,3),keep_ctime_const)
             call wc_p(wcd,cvar_ptr(wcd,slot,4),keep_ctime_const)
             call wc_p(wcd,cvar_ptr(wcd,slot,5),keep_ctime_const)
          end select
       elseif(k==v_is_ctime_const) then
          if(keep_ctime_const) then
             call wc(wcd,a)
          endif
       else 
          call wc(wcd,sign(slot,a))
       endif
    else
       call wc(wcd,a)
    endif
  end subroutine wc_p

  !************************************************************************
  ! COMPILER ONLY - HANDLE BASIC OPERATIONS BETWEEN VARIABLES WHILE
  ! ALLOWING FOR EITHER VARIABLE TO BE DISAGGREGATED (I.E. A GROUP OF
  ! OTHER VARIABLES)
  !*************************************************************************
  
  !=========================================================================
  ! Get element of struct or rec
  ! if dest==source, changes dest to equal the element (may be new variable)
  ! if dest/=source, set dest to the element
  ! (uses convention that first element is 2!)
  !=========================================================================
  recursive subroutine comp_get_elem(wcd,op,dest,asource,elem)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: op,asource
    integer,intent(in):: elem
    integer,intent(inout):: dest
    integer:: k,i
    integer:: svec,source,val
    type(pm_ptr):: tv
    if(.not.pm_is_compiling) call pm_panic('wc_get_elem')
!!$    write(*,*) '====>'
!!$    call dump_cvar(wcd,6,asource)
!!$    call dump_cvar(wcd,6,dest)
!!$    write(*,*) '<====='
    source=cvar_strip_alias(wcd,asource)
    dest=cvar_strip_alias(wcd,dest)
!!$    write(*,*) '+====>'
!!$    call dump_cvar(wcd,6,source)
!!$    call dump_cvar(wcd,6,dest)
    if(cvar_kind(wcd,source)==v_is_unit_elem) then
       call dump_cvar(wcd,6,cvar_v1(wcd,source))
       if(cvar_kind(wcd,cvar_v1(wcd,source))==v_is_vect_wrapped) then
          call dump_cvar(wcd,6,cvar_v1(wcd,cvar_v1(wcd,source)))
       endif
    endif
!    write(*,*) '<=====+'
    k=cvar_kind(wcd,source)
    select case(k)
    case(v_is_basic,v_is_elem,v_is_unit_elem,v_is_sub,v_is_vsub,&
         v_is_vect_wrapped,v_is_chan_vect)
       if(cvar_kind(wcd,dest)==v_is_group) then
          tv=pm_type_vect(wcd%context,cvar_type(wcd,dest))
          call comp_alias_slots(wcd,dest,&
               cvar_alloc_elem(wcd,source,elem))
       else
          if(source==dest) then
             dest=cvar_alloc_elem(wcd,source,elem)
          else
             call cvar_set_elem(wcd,dest,source,elem)
          endif
       endif
    case(v_is_group)
       if(op==op_elem_ref) then
          ! Element of a dref
          val=cvar_strip_alias(wcd,cvar_ptr(wcd,source,1))
          if(source==dest) then
             if(cvar_kind(wcd,val)==v_is_group) then
                call cvar_set_ptr(wcd,dest,1,cvar_ptr(wcd,val,1))
             else
                call cvar_set_ptr(wcd,dest,1,&
                     cvar_alloc_elem(wcd,val,elem))
             endif
          else
             if(cvar_kind(wcd,val)==v_is_group) then
                call comp_alias_slots(wcd,cvar_ptr(wcd,dest,1),&
                     cvar_ptr(wcd,val,elem))
             else
                call cvar_set_elem(wcd,cvar_ptr(wcd,dest,1),val,elem)
             endif
             call comp_alias_slots(wcd,cvar_ptr(wcd,dest,2),source)
             do i=3,5
                call comp_alias_slots(wcd,cvar_ptr(wcd,dest,i),&
                     cvar_ptr(wcd,source,i))
             enddo
          endif
       elseif(source==dest) then
          dest=cvar_ptr(wcd,source,elem)
       else
          call comp_alias_slots(wcd,dest,cvar_ptr(wcd,source,elem))
       endif
    case default
       write(*,*) 'as',source,asource
       write(*,*) 'Bad var kind=',k
       call pm_panic('wc_get_elem')
    end select
  end subroutine comp_get_elem


  !=======================================================================
  ! Set variable to be a subscript reference
  !=======================================================================
  subroutine comp_get_subs(wcd,n,aparent,asubs)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: aparent,asubs
    integer:: n,parent,subs
    type(pm_ptr):: tv
!!$    write(*,*) 'GETSUBS'
!!$    call dump_cvar(wcd,6,aparent)
    parent=cvar_strip_alias(wcd,aparent)
    subs=cvar_strip_alias(wcd,asubs)
    tv=pm_type_vect(wcd%context,cvar_type(wcd,parent))
    if(cvar_kind(wcd,parent)==v_is_group) then
       call cvar_set_info(wcd,n,v_is_vsub,&
            cvar_ptr(wcd,parent,1),subs,pm_tv_arg(tv,1))
    else
       call cvar_set_info(wcd,n,v_is_sub,&
            parent,subs,pm_tv_arg(tv,1))
    endif
!!$    write(*,*) '***'
!!$    call dump_cvar(wcd,6,n)
  end subroutine comp_get_subs

  !=======================================================================
  ! Return subscript reference as new variable
  !=======================================================================
  function comp_subs(wcd,parent,subs) result(n)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: parent,subs
    integer:: n
    type(pm_ptr):: tv
    tv=pm_type_vect(wcd%context,cvar_type(wcd,parent))
    n=cvar_alloc_slots(wcd,3)
    call comp_get_subs(wcd,n,parent,subs)
  end function comp_subs
  
  !=======================================================================
  ! Code assignment arg1:=arg2
  !=======================================================================
  subroutine comp_assign(wcd,callnode,arg1,arg2,dup,rv,ve)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: ve
    type(pm_ptr),intent(in):: arg1,arg2,callnode,rv
    logical,intent(in):: dup
    call comp_assign_to_slot(wcd,callnode,var_slot(wcd,arg1),arg2,dup,rv,ve)
  end subroutine comp_assign
  
  !=======================================================================
  ! Code assignment (*slot):=arg
  !=======================================================================
  subroutine comp_assign_to_slot(wcd,callnode,slot,arg,dup,rv,ve)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: ve,slot
    logical,intent(in):: dup
    type(pm_ptr),intent(in):: arg,callnode,rv
    call comp_assign_slots(wcd,callnode,slot,arg_slot(wcd,arg),dup,rv,ve)
  end subroutine comp_assign_to_slot

  !=======================================================================
  ! Code assignment (*slot1a):=(*slot2a)
  !=======================================================================
  recursive subroutine comp_assign_slots(wcd,callnode,aslot1,aslot2,dup,rv,ve)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: ve,aslot1,aslot2
    logical,intent(in):: dup
    type(pm_ptr),intent(in):: callnode,rv
    integer:: slot1,slot2,slota
    integer:: k1,k2,i,op

    slot1=cvar_strip_alias(wcd,aslot1)
    slot2=cvar_strip_alias(wcd,aslot2)
    
    k1=cvar_kind(wcd,slot1)
    k2=cvar_kind(wcd,slot2)
    if(k1==v_is_ctime_const.or.k1==v_is_const) then
       return
    elseif(k1==v_is_group) then
       if(k2==v_is_group) then
          if(pm_debug_checks) then
             if(cvar_v1(wcd,slot1)/=cvar_v1(wcd,slot2)) then
                call pm_panic('comp_assign_group_size_mismatch')
             endif
          endif
          do i=1,cvar_v1(wcd,slot1)
             call comp_assign_slots(wcd,callnode,cvar_ptr(wcd,slot1,i),&
                  cvar_ptr(wcd,slot2,i),dup,rv,ve)
          enddo
       elseif(k2/=v_is_const.and.k2/=v_is_ctime_const) then
          do i=1,cvar_v1(wcd,slot1)
             slota=cvar_alloc_elem(wcd,slot2,i)
             call comp_assign_slots(wcd,callnode,cvar_ptr(wcd,slot1,i),&
                  slota,dup,rv,ve)
          enddo
       endif
    else
       if(k2==v_is_group) then
          do i=1,cvar_v1(wcd,slot2)
             slota=cvar_alloc_elem(wcd,slot1,i)
             call comp_assign_slots(wcd,callnode,slota,cvar_ptr(wcd,slot2,i),&
                  dup,rv,ve)
          enddo
       else
          if(slot1==slot2) return
          call wc_call(wcd,callnode,op_assign,789,3,0,ve)
          call wc(wcd,-slot1)
          call wc(wcd,slot2)
       endif
    endif
  contains
    include 'fisnull.inc'
  end subroutine comp_assign_slots

  !=======================================================================
  ! Handle returning an invariant result from a proc this is not run
  ! as shared or communicating.
  !=======================================================================
  recursive subroutine comp_invar_result(wcd,callnode,slot1a,slot2,rv,ve)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,rv
    integer,intent(in):: slot1a,slot2,ve
    integer:: i,slot1
    slot1=cvar_strip_alias(wcd,slot1a)
    select case(cvar_kind(wcd,slot2))
    case(v_is_group)
       select case(cvar_v2(wcd,slot2))
       case(v_is_array,v_is_dref,v_is_shared_dref)
          call comp_alias_slots(wcd,slot1,slot2)
       case(v_is_storageless)
          continue
       case default
          if(cvar_kind(wcd,slot1)==v_is_group) then
             do i=1,cvar_v1(wcd,slot2)
                call comp_invar_result(wcd,callnode,cvar_ptr(wcd,slot1,i),&
                     cvar_ptr(wcd,slot2,i),rv,ve)
             enddo
          else
             call comp_assign_slots(wcd,callnode,slot1,slot2,.true.,rv,ve)
          endif
       end select   
    case(v_is_alias)
       call comp_invar_result(wcd,callnode,slot1,cvar_ptr(wcd,slot2,i),rv,ve)
    case default
       call comp_assign_slots(wcd,callnode,slot1,slot2,.true.,rv,ve)
    end select
  end subroutine comp_invar_result
  
  !=======================================================================
  ! Create alias arg1<-arg2
  !=======================================================================
  subroutine comp_alias(wcd,callnode,arg1,arg2,rv,ve,slot)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,arg1,arg2,rv
    integer,intent(in),optional:: slot
    integer:: ve
    integer:: slot1,slot2
    if(present(slot)) then
       slot1=slot
    else
       slot1=var_slot(wcd,arg1)
    endif
    slot2=arg_slot(wcd,arg2)
    call comp_alias_slots(wcd,slot1,slot2)
  end subroutine comp_alias

  !=======================================================================
  ! Create alias arg1<-arg2 removing any vector wrapper around arg2
  !=======================================================================
  subroutine comp_alias_devect(wcd,callnode,arg1,arg2,rv,ve,slot)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: callnode,arg1,arg2,rv
    integer,intent(in),optional:: slot
    integer:: ve
    integer:: slot1,slot2
    if(present(slot)) then
       slot1=slot
    else
       slot1=var_slot(wcd,arg1)
    endif
    slot2=arg_slot(wcd,arg2)
    if(cvar_kind(wcd,slot2)==v_is_vect_wrapped) then
       slot2=cvar_v1(wcd,slot2)
    endif
    call comp_alias_slots(wcd,slot1,slot2)
  end subroutine comp_alias_devect
  
  !=======================================================================
  ! Alias (*slot1)<-(*slot2)
  !=======================================================================
  recursive subroutine comp_alias_slots(wcd,slot1,aslot2)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: slot1,aslot2
    integer:: n,i,k,k2,slot,slot2
    slot2=cvar_strip_alias(wcd,aslot2)
    if(slot1<0.or.slot2<0) call pm_panic('comp_alias_slots')
    if(slot1==slot2) return
    k=cvar_kind(wcd,slot1)
    k2=cvar_kind(wcd,slot2)
    select case(k)
    case(v_is_group)
       n=cvar_v1(wcd,slot1)
       if(k2==v_is_group) then
          do i=1,n
             call comp_alias_slots(wcd,cvar_ptr(wcd,slot1,i),&
                  cvar_ptr(wcd,slot2,i))
          enddo
       else
          do i=1,n
             slot=cvar_ptr(wcd,slot1,i)
             if(cvar_kind(wcd,slot)/=v_is_ctime_const) then
                call comp_get_elem(wcd,op_elem,slot,&
                     slot2,i)
             endif
          enddo
       endif
    case(v_is_basic)
       if(k2==v_is_group.or.k2==v_is_basic) then
          call cvar_set_info(wcd,slot1,v_is_alias,slot2,0,&
               cvar_type(wcd,slot1))
       else
          call cvar_set_info(wcd,slot1,cvar_kind(wcd,slot2),&
               cvar_v1(wcd,slot2),cvar_v2(wcd,slot2),cvar_type(wcd,slot2))
       endif
    case(v_is_vect_wrapped)
       if(cvar_kind(wcd,slot2)/=v_is_vect_wrapped) then
          call pm_panic('Alias slots - vect_wrapped')
       endif
       call cvar_set_info(wcd,slot1,v_is_vect_wrapped,cvar_v1(wcd,slot2),0,&
            cvar_type(wcd,slot1))
    case(v_is_chan_vect)
!!$       call cvar_set_info(wcd,slot1,v_is_alias,slot2,0,&
!!$            cvar_type(wcd,slot1))
       write(*,*) '*******************'
       call dump_cvar(wcd,6,slot1)
       call pm_panic('Internal Error - Attempt to alias channel')
    case(v_is_ctime_const)
       continue
    case default
       write(*,*) k
       call pm_panic('Slot already aliased')
    end select
  end subroutine comp_alias_slots

  !=======================================================================
  ! Link distributed reference arg1 to distributed reference arg2
  !=======================================================================
  subroutine comp_link_dref(wcd,arg1,arg2)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: arg1,arg2
    integer:: slot1,slot2
    integer:: k2
    slot1=cvar_strip_alias(wcd,arg_slot(wcd,arg1))
    slot2=cvar_strip_alias(wcd,arg_slot(wcd,arg2))
    if(cvar_kind(wcd,slot2)==v_is_vect_wrapped) then
       slot2=cvar_v1(wcd,slot2)
    endif
    if(cvar_kind(wcd,slot1)==v_is_group) then
       k2=cvar_v2(wcd,slot1)
       if(k2==v_is_dref.or.k2==v_is_shared_dref) then
          if(k2==v_is_shared_dref) then
             call comp_alias_slots(wcd,cvar_ptr(wcd,slot1,3),cvar_ptr(wcd,slot2,3))
          endif
          call link_dref(slot1,slot2)
       endif
    endif
  contains
    recursive subroutine link_dref(slot1,slot2)
      integer,intent(in):: slot1,slot2
      integer:: slot3,slot4,k2
      slot3=cvar_ptr(wcd,slot1,2)
      slot4=cvar_ptr(wcd,slot2,2)
      if(cvar_kind(wcd,slot3)==v_is_group) then
         k2=cvar_v2(wcd,slot3)
         if(k2==v_is_dref.or.k2==v_is_shared_dref) then
            if(k2==v_is_shared_dref) then
               call comp_alias_slots(wcd,cvar_ptr(wcd,slot3,3),cvar_ptr(wcd,slot4,3))
            endif
            call link_dref(slot3,slot4)
            return
         endif
      endif
      call comp_alias_slots(wcd,slot3,slot4)
    end subroutine link_dref
  end subroutine comp_link_dref
  
  !***************************************************************
  ! COMPILER ONLY - MANAGE VARIABLES
  !***************************************************************
  
  !=======================================================================
  ! Alloc a variable (compiler only)
  !=======================================================================
  recursive function cvar_alloc(wcd,typ,flags,aname) result(n)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: typ
    integer,intent(in):: flags
    integer,intent(in),optional:: aname
    integer:: n
    integer:: i,k,m,tk,slot,vec,dom,tno
    type(pm_ptr):: tset,ts,tv,val
    integer:: v1,v2,nflags,name
    if(present(aname)) then
       name=aname
    else
       name=0
    endif
    v1=0
    v2=0
    if(typ==pm_ve_type) then
       n=cvar_alloc_entry(wcd,v_is_ve,0,0,int(pm_logical))
    elseif(typ<=0) then
       n=cvar_alloc_entry(wcd,v_is_group,0,v_is_storageless,int(pm_tiny_int))
    elseif(typ<=pm_null) then
       n=cvar_alloc_entry(wcd,v_is_group,0,v_is_storageless,typ)
    elseif(typ<=pm_string) then
       n=cvar_alloc_entry(wcd,v_is_basic,name,flags,typ)
    else
       tv=pm_type_vect(wcd%context,typ)
       tk=pm_tv_kind(tv)
       select case(tk)
       case(pm_type_is_basic)
          n=cvar_alloc_entry(wcd,v_is_basic,name,flags,typ)
          call add_to_typeset(wcd,typ)
       case(pm_type_is_rec)
          nflags=pm_tv_flags(tv)
          if(iand(nflags,pm_type_is_soa)/=0.or.&
               iand(nflags,pm_type_has_storage)==0)then
             m=pm_tv_numargs(tv)
             n=cvar_alloc_slots(wcd,3+m)
             v1=m
             v2=v_is_struct
             do i=1,m
                wcd%vinfo(n+i+2)=ptr(cvar_alloc(wcd,pm_tv_arg(tv,i),flags,name))
             enddo
             call cvar_set_info(wcd,n,v_is_group,v1,v2,typ)
          else
             !write(*,*) 'STRUCT>',trim(pm_type_as_string(wcd%context,typ))
             n=cvar_alloc_entry(wcd,v_is_basic,name,flags,typ)
             call add_to_typeset(wcd,typ)
          endif
       case(pm_type_is_array)
          if(iand(flags,v_is_param+v_is_result)/=0.and.&
               iand(flags,v_is_chan)==0) then
             nflags=ior(flags,v_is_array_par_dom)
             if(pm_tv_name(tv)/=sym_var) nflags=iand(nflags,not(v_is_ref))
             tno=pm_tv_arg(tv,1)
             if(iand(pm_type_flags(wcd%context,tno),pm_type_has_storage)/=0) then
                vec=cvar_alloc_entry(wcd,v_is_basic,pm_tv_arg(tv,3),&
                     ior(flags,v_is_array_par_vect),tno)
             else
                vec=cvar_alloc(wcd,tno,flags,aname)
             endif
             tno=pm_tv_arg(tv,2)
             if(iand(pm_type_flags(wcd%context,tno),pm_type_has_storage)/=0) then
                dom=cvar_alloc_entry(wcd,v_is_basic,name,&
                     nflags,tno)
             else
                dom=cvar_alloc(wcd,tno,flags,aname)
             endif
             n=cvar_alloc_array_view(wcd,vec,dom,typ)
          else
             n=cvar_alloc_entry(wcd,v_is_basic,name,ior(flags,v_is_farray),typ)
          endif
          call add_to_typeset(wcd,typ)
       case(pm_type_is_user)
          n=cvar_alloc(wcd,pm_user_type_body(wcd%context,typ),flags,aname)
       case(pm_type_is_tuple,pm_type_is_vtuple)
          m=pm_tv_numargs(tv)
          n=cvar_alloc_slots(wcd,3+m)
          v1=m
          v2=v_is_tuple
          do i=1,m
             wcd%vinfo(n+i+2)=ptr(cvar_alloc(wcd,pm_tv_arg(tv,i),flags))
          enddo
          call cvar_set_info(wcd,n,v_is_group,v1,v2,typ)
       case(pm_type_is_dref)
          nflags=ior(iand(flags,not(v_is_shared+v_is_ref+v_is_vect)),&
               merge(0,v_is_in_dref,pm_tv_name(tv)==sym_pling))
          i=pm_tv_flags(tv)
          n=cvar_alloc_slots(wcd,8)
          v1=5
          v2=merge(v_is_shared_dref,v_is_dref,&
               pm_type_get_mode(wcd%context,pm_tv_arg(tv,3))>=sym_invar)
          wcd%vinfo(n+3)=dptr(pm_tv_arg(tv,1),nflags) !ptr(cvar_alloc(wcd,pm_tv_arg(tv,1),nflags))
          wcd%vinfo(n+4)=dptr(pm_tv_arg(tv,2),ior(nflags,iand(flags,v_is_ref)))
          !ptr(cvar_alloc(wcd,pm_tv_arg(tv,2),ior(nflags,v_is_ref)))
          wcd%vinfo(n+5)=dptr(pm_tv_arg(tv,3),nflags) !ptr(cvar_alloc(wcd,pm_tv_arg(tv,3),nflags))
          wcd%vinfo(n+6)=dptr(pm_tv_arg(tv,4),nflags) !ptr(cvar_alloc(wcd,pm_tv_arg(tv,4),nflags))
          wcd%vinfo(n+7)=dptr(pm_tv_arg(tv,5),nflags) !ptr(cvar_alloc(wcd,pm_tv_arg(tv,5),nflags))
          call cvar_set_info(wcd,n,v_is_group,v1,v2,typ)
       case(pm_type_is_poly)
          n=cvar_alloc_entry(wcd,v_is_basic,name,ior(flags,v_is_poly),typ)
       case(pm_type_is_single_name,pm_type_is_proc,pm_type_is_type)
          n=cvar_alloc_entry(wcd,v_is_group,0,v_is_storageless,typ)
       case(pm_type_is_fix_value,pm_type_is_literal_value)
          n=cvar_alloc_entry(wcd,v_is_ctime_const,add_const(wcd,&
               pm_type_val(wcd%context,typ)),0,typ)
       case(pm_type_is_par_kind)
          k=pm_tv_name(tv)
          nflags=flags
          if(k>=sym_invar) nflags=ior(nflags,v_is_shared)
          if(k==sym_chan) then
             nflags=ior(nflags,v_is_chan+v_is_vect)
             n=cvar_alloc_entry(wcd,v_is_chan_vect,&
                  cvar_alloc(wcd,pm_tv_arg(tv,1),nflags,name),&
                  0,pm_tv_arg(tv,1))
             call add_to_typeset(wcd,pm_tv_arg(tv,1),int(pm_long))
          else
             n=cvar_alloc(wcd,pm_tv_arg(tv,1),nflags,name)
          endif
       case(pm_type_is_all)
          ! This just caters for _nhd variables
          n=cvar_alloc(wcd,pm_tv_arg(tv,1),flags,name)
       case(pm_type_is_vect)
          n=cvar_alloc(wcd,pm_tv_arg(tv,1),ior(flags,v_is_vect),name)
          n=cvar_alloc_entry(wcd,v_is_vect_wrapped,n,0,pm_tv_arg(tv,1))
       case default
          write(*,*) 'CVAR ALLOC:', tk,trim(pm_type_as_string(wcd%context,typ))
          call pm_panic('cvar_alloc')
       end select
    endif
  contains
    
    function ptr(n) result(nn)
      integer,intent(in):: n
      integer:: nn
      nn=n*cvar_flag_mult
    end function ptr

    function dptr(tno,flags) result(v)
      integer,intent(in):: tno,flags
      integer:: v
      integer:: typ,mode
      v=cvar_alloc(wcd,tno,flags)
      v=ptr(v)
    end function dptr

  end function cvar_alloc

  function cvar_alloc_array_view(wcd,v,d,typ) result(slot)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: v,d
    integer,intent(in),optional:: typ
    integer:: slot
    slot=cvar_alloc_slots(wcd,5)
    wcd%vinfo(slot+3)=v*cvar_flag_mult
    wcd%vinfo(slot+4)=d*cvar_flag_mult
    call cvar_set_info(wcd,slot,v_is_group,2,v_is_array,typ)
  end function cvar_alloc_array_view

  !=======================================================================
  ! Add a type to the list of active types used by the source generator
  ! Adding -typeno records allocatable vector of typeno
  !=======================================================================
  recursive subroutine add_to_typeset(wcd,typ,dim)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: typ
    integer,intent(in),optional:: dim
    type(pm_ptr):: tset
    integer:: tno,key(2),m
    integer:: i
    type(pm_ptr):: tv
    tno=pm_type_strip_to_basic(wcd%context,typ)
    tset=wcd%typeset
    if(tno>0.and.tno<=pm_string.and..not.present(dim)) return
    if(.not.pm_type_needs_storage(wcd%context,abs(tno))) return
    if(pm_type_kind(wcd%context,tno)==pm_type_is_poly) tno=pm_pointer
    key(1)=tno
    if(present(dim)) then
       key(2)=dim
    else
       key(2)=0
    endif
    if(pm_ivect_lookup(wcd%context,tset,key,2)<=0) then
       tv=pm_type_vect(wcd%context,abs(tno))
       if(pm_tv_kind(tv)==pm_type_is_array) then
          call add_to_typeset(wcd,pm_tv_arg(tv,1),pm_tv_arg(tv,3))
       endif
       do i=1,pm_tv_numargs(tv)
          call add_to_typeset(wcd,pm_tv_arg(tv,i))
       enddo
       m=pm_iset_add(wcd%context,tset,key,2)
    endif
  end subroutine add_to_typeset

  !=======================================================================
  ! Allocte n slots to hold a variable description record
  !=======================================================================
  function cvar_alloc_slots(wcd,n) result(slot)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n
    integer:: slot
    integer:: oldsize
    integer,dimension(:),allocatable:: temp
    if(wcd%nvar+n>wcd%mvar) then
       wcd%mvar=wcd%mvar*2
       temp=wcd%vinfo(1:wcd%nvar)
       deallocate(wcd%vinfo)
       allocate(wcd%vinfo(1:wcd%mvar))
       wcd%vinfo(1:wcd%nvar)=temp
       deallocate(temp)
    endif
    slot=wcd%nvar+1
    wcd%nvar=wcd%nvar+n
  end function cvar_alloc_slots

  !=======================================================================
  ! Create a 'compiler variable' slot for a constant
  !=======================================================================
  function cvar_const(wcd,arg) result(slot1)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: arg
    integer:: slot1,slot2
    integer:: tno,mode,tk
    type(pm_ptr):: tv
    tno=pm_type_strip_mode(wcd%context,cnode_get_num(arg,cnode_args+1),mode)
    if(tno<=pm_null) then
       slot1=cvar_alloc_entry(wcd,v_is_group,0,v_is_storageless,tno)
    else
       tv=pm_type_vect(wcd%context,tno)
       tk=pm_tv_kind(tv)
       if(tk==pm_type_is_single_name.or.tk==pm_type_is_proc) then
          slot1=cvar_alloc_entry(wcd,v_is_group,0,v_is_storageless,tno)
       else
          slot2=add_const(wcd,cnode_arg(arg,1))
          slot1=cvar_alloc_entry(wcd,&
               merge(v_is_ctime_const,v_is_const,pm_tv_kind(tv)==pm_type_is_fix_value),&
               slot2,0,tno)
       endif
    endif
  end function cvar_const

  !=======================================================================
  ! Create a 'compiler variable constant' slot for a given value
  !=======================================================================
  function cvar_const_value(wcd,val) result(slot)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr),intent(in):: val
    integer:: slot
    slot=cvar_alloc_entry(wcd,v_is_const,add_const(wcd,val),0,&
         pm_fast_typeof(val))
  contains
    include 'ftypeof.inc'
  end function cvar_const_value

  !=======================================================================
  ! Remove any initial aliases returning resulting slot #
  !=======================================================================
  function cvar_strip_alias(wcd,source) result(dest)
    type(wcoder),intent(inout):: wcd
    integer:: source
    integer:: dest
    dest=source
    do while(cvar_kind(wcd,dest)==v_is_alias)
       dest=cvar_v1(wcd,dest)
    enddo
  end function cvar_strip_alias

  !=======================================================================
  ! Return the i-th pointer of variable record at slot n
  !=======================================================================
  function cvar_ptr(wcd,n,i) result(m)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n
    integer,intent(in):: i
    integer:: m
    if(pm_debug_checks) then
       if(iand(int(wcd%vinfo(n)),cvar_flag_mult-1)/=v_is_group) then
          write(*,*) 'kind=',iand(int(wcd%vinfo(n)),cvar_flag_mult-1)
          call pm_panic('cvar_ptr - kind')
       endif
       if(i>wcd%vinfo(n)/cvar_flag_mult) then
          write(*,*) 'i=',i,'n=',wcd%vinfo(n)/cvar_flag_mult
          call pm_panic('cvar_ptr > n')
       endif
    endif
    m=wcd%vinfo(n+i+2)/cvar_flag_mult
  end function cvar_ptr

  !=======================================================================
  ! Set the i-th pointer of variable record at slot n
  !=======================================================================
  subroutine cvar_set_ptr(wcd,n,i,p)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n,p
    integer,intent(in):: i
    wcd%vinfo(n+i+2)=p*cvar_flag_mult
  end subroutine cvar_set_ptr

  !=======================================================================
  ! Flag as variable record as used
  !=======================================================================
  subroutine cvar_set_used(wcd,n)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n
    wcd%vinfo(n+1)=ior(int(wcd%vinfo(n+1)),v_is_used*cvar_flag_mult)
  end subroutine cvar_set_used

  !=======================================================================
  ! Flag as variable record as shared
  !=======================================================================
  subroutine cvar_set_shared(wcd,n)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n
    wcd%vinfo(n+1)=ior(int(wcd%vinfo(n+1)),v_is_shared*cvar_flag_mult)
  end subroutine cvar_set_shared

  !=======================================================================
  ! Allocate vector engine (mask) variable
  !=======================================================================
  function cvar_alloc_ve(wcd,parent,cove) result(n)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: parent,cove
    integer:: n
    n=cvar_alloc_entry(wcd,merge(v_is_cove,v_is_ve,cove/=0),parent,cove,int(pm_logical))
    if(cove/=0) then
       wcd%vinfo(cove+1)=n*cvar_flag_mult
    endif
  end function cvar_alloc_ve

  !=======================================================================
  ! Allocate element reference
  !=======================================================================
  function cvar_alloc_elem(wcd,parent,elem) result(n)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: parent,elem
    integer:: n
    type(pm_ptr):: tv
    tv=pm_type_vect(wcd%context,cvar_type(wcd,parent))
    n=cvar_alloc_entry(wcd,merge(v_is_unit_elem,v_is_elem,pm_tv_numargs(tv)==1.and..false.),&
         parent,elem,pm_tv_arg(tv,elem))
  end function cvar_alloc_elem

  !=======================================================================
  ! Allocate alias reference
  !=======================================================================
  function cvar_alloc_alias(wcd,parent) result(n)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: parent
    integer:: n
    n=cvar_alloc_entry(wcd,v_is_alias,&
         parent,0,cvar_type(wcd,parent))
  end function cvar_alloc_alias
  
  !=======================================================================
  ! Allocate constant reference
  !=======================================================================
  function cvar_alloc_const(wcd,val) result(n)
    type(wcoder),intent(inout):: wcd
    type(pm_ptr):: val
    integer:: n
    n=cvar_alloc_entry(wcd,v_is_const,add_const(wcd,val),&
         0,pm_fast_typeof(val))
  contains
    include 'ftypeof.inc'
  end function cvar_alloc_const

  !=======================================================================
  ! Set variable to be an element reference
  !=======================================================================
  subroutine cvar_set_elem(wcd,n,parent,elem)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n,parent,elem
    type(pm_ptr):: tv
    if(pm_debug_checks) then
       if(cvar_kind(wcd,n)/=v_is_basic) then
          call pm_panic('cvar_set_elem')
       endif
    endif
    tv=pm_type_vect(wcd%context,cvar_type(wcd,parent))
!!$    write(*,*) cvar_type(wcd,parent),'>>',elem,';',trim(pm_type_as_string(wcd%context,cvar_type(wcd,parent))),&
!!$         ';',trim(pm_type_as_string(wcd%context,cvar_type(wcd,n)))
    call cvar_set_info(wcd,n,merge(v_is_unit_elem,v_is_elem,pm_tv_numargs(tv)==1.and..false.),&
         parent,elem,pm_tv_arg(tv,elem))
  end subroutine cvar_set_elem

  !=======================================================================
  ! Set variable to be an alias reference
  !=======================================================================
  subroutine cvar_set_alias(wcd,n,parent)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n,parent
    call cvar_set_info(wcd,n,v_is_alias,&
         parent,0,cvar_type(wcd,parent))
  end subroutine cvar_set_alias

  !=======================================================================
  ! Allocate 3-slot variable description record
  !=======================================================================
  function cvar_alloc_entry(wcd,kind,v1,v2,tno) result(n)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: kind
    integer,intent(in):: v1,v2
    integer,intent(in):: tno
    integer:: n
    n=cvar_alloc_slots(wcd,3)
    call cvar_set_info(wcd,n,kind,v1,v2,tno)
  end function cvar_alloc_entry

  !=======================================================================
  ! Reset the information in a 3-slot variable description record
  !=======================================================================
  subroutine cvar_set_info(wcd,n,kind,v1,v2,tno)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: kind
    integer,intent(in):: n,v1,v2
    integer,intent(in):: tno
    integer:: junk
    if(pm_debug_checks) then
       if(kind==v_is_alias.and.v1==0) then
          call pm_panic('Alias to nothing...')
       endif
       if(tno/=0) junk=pm_type_name(wcd%context,tno)
    endif
    wcd%vinfo(n)=v1*cvar_flag_mult+kind
    wcd%vinfo(n+1)=v2*cvar_flag_mult
    wcd%vinfo(n+2)=tno*cvar_flag_mult
  end subroutine cvar_set_info

  !=======================================================================
  ! Return the kind of a variable description record
  !=======================================================================
  function cvar_kind(wcd,n) result(kind)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n
    integer:: kind
    integer:: info
    if(pm_debug_checks) then
       if(n<=0) then
          write(*,*) 'n=',n
          call pm_panic('cvar_kind')
       endif
    endif
    kind=iand(int(wcd%vinfo(n)),cvar_flag_mask)
  end function cvar_kind

  !=======================================================================
  ! Return first slot of a variable description record
  !=======================================================================
  function cvar_v1(wcd,n) result(v1)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n
    integer:: v1
    v1=int(wcd%vinfo(n))/cvar_flag_mult
  end function cvar_v1

  !=======================================================================
  ! Return second slot of a variable description record
  !=======================================================================
  function cvar_v2(wcd,n) result(v2)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n
    integer:: v2
    v2=int(wcd%vinfo(n+1))/cvar_flag_mult
  end function cvar_v2
  
  !=======================================================================
  ! Return type recorded in a variable description record
  !=======================================================================
  function cvar_type(wcd,n) result(v2)
    type(wcoder),intent(inout):: wcd
    integer,intent(in):: n
    integer:: v2
    v2=int(wcd%vinfo(n+2))/cvar_flag_mult
  end function cvar_type
  
  !=======================================================================
  ! Tidy up procedure code.
  ! - follow loop chains
  !=======================================================================
  subroutine tidy_up(wcd)
    type(wcoder),intent(inout):: wcd
    integer:: i,j,nargs
    integer::code,arg
    integer:: code2
    integer,dimension(pm_pointer):: vstart
 
    if(debug_wcode) call set_op_names
    i=1
    do
       code=wcd%wc(i)
       if(debug_wcode) then
          if(code>=0) then
             write(*,*) 'TIDY>',i,code,trim(op_names(code))
          else
             write(*,*) 'TIDY>',i,code,'????'
          endif
       endif
       code2=wcd%wc(i+1)*pm_ext_mult+wcd%wc(i+2)/(pm_max_args+1)
       nargs=iand(int(wcd%wc(i+2)),pm_max_args)
       ! Follow jump chain
       if(code>=first_jmp_op.and.code<=last_jmp_op.and.code/=op_skip_empty) then
          if(code2>0) then
             do while(wcd%wc(code2)==op_jmp)
                if(code2>i) then
                   code2=wcd%wc(code2+1)*pm_ext_mult+&
                        wcd%wc(code2+2)/(pm_max_args+1)
                else
                   !write(*,*) 'CODE>',wcd%wc(code2+1:code2+2),i,code2,op_names(wcd%wc(code2))
                   code2=code2+3+iand(int(wcd%wc(code2+2)),pm_max_args)+&
                        int(wcd%wc(code2+1))*pm_ext_mult+&
                        int(wcd%wc(code2+2))/(pm_max_args+1)-pm_jump_offset
                   !write(*,*) 'New code2=',code2
                endif
             enddo
             
             ! Change to relative displacement
             code2=code2-(i+3+nargs)+pm_jump_offset
          else
             code2=pm_jump_offset
          endif
          wcd%wc(i+1)=code2/pm_ext_mult
          wcd%wc(i+2)=nargs+(pm_max_args+1)*iand(code2,pm_ext_mult-1)
          !write(*,*) 'Convert ',i,code2,'==>',wcd%wc(i+1),wcd%wc(i+2)
       endif
       i=i+nargs+3
       if(i>=wcd%pc) exit
    enddo
  end subroutine tidy_up

  !=======================================================================
  ! Tidy up procedure (compiler version)
  !=======================================================================
  subroutine comp_tidy_up(wcd)
    type(wcoder),intent(inout):: wcd
    integer:: i,j,k,a
    integer:: arg,instr
    i=1
    do
       a=i+comp_op_arg0
       k=a+wcd%wc(i+comp_op_nargs)-1
       instr=wcd%wc(i+comp_op_opcode)
       if(debug_wcode) write(*,*) 'TIDY COMP>',op_names(instr),a,k,wcd%wc(a:k)
       select case(instr)
       case(op_do_loop)
          do j=a,k
             arg=wcd%wc(j)
             wcd%wc(j)=sign(tidy_arg(abs(arg)),arg)
          enddo
       case(op_if,op_if_shared)
          arg=wcd%wc(a+3)
          wcd%wc(a+3)=tidy_arg(arg)
       case(op_loop,op_comm_loop,op_comm_block)
          arg=wcd%wc(a+2)
          wcd%wc(a+2)=sign(tidy_arg(abs(arg)),arg)
       case(op_if_shared_node,op_if_restart,op_break_loop,&
            op_allocate,op_deallocate,op_mask,op_comm_proc,&
            op_comm_inline,op_over,op_skip_empty,op_head_node,op_inline_shared)
          continue
       case(op_remote_call,op_remote_send_call,&
            op_server_call,op_collect_call,op_bcast_call)
          do j=a+3,k
             arg=wcd%wc(j)
             wcd%wc(j)=sign(tidy_arg(abs(arg)),arg)
          enddo
       case default
          do j=a,k
             arg=wcd%wc(j)
             wcd%wc(j)=sign(tidy_arg(abs(arg)),arg)
          enddo
       end select
       i=k+1
       if(i>=wcd%pc) exit
    enddo
  contains
    recursive function tidy_arg(n) result(m)
      integer,intent(in)::n
      integer::m
      integer:: k,i
      if(n==0) then
         m=0
         return
      elseif(n==abs(shared_op_flag)) then
         m=n
         return
      endif
      k=cvar_kind(wcd,n)
      select case(k)
      case(v_is_const)
         m=n
      case(v_is_group)
         do i=1,cvar_v1(wcd,n)
            wcd%vinfo(n+1+i)=tidy_arg(cvar_ptr(wcd,n,i))*cvar_flag_mult
         enddo
         m=n
      case(v_is_basic)
         call cvar_set_used(wcd,n)
         m=n
      case(v_is_sub,v_is_vsub)
         call cvar_set_info(wcd,n,k,&
              tidy_arg(cvar_v1(wcd,n)),&
              tidy_arg(cvar_v2(wcd,n)),cvar_type(wcd,n))
         m=n
      case(v_is_elem)
         call cvar_set_info(wcd,n,v_is_elem,&
              tidy_arg(cvar_v1(wcd,n)),&
              cvar_v2(wcd,n),cvar_type(wcd,n))
         m=n
      case(v_is_unit_elem)
         call cvar_set_info(wcd,n,v_is_unit_elem,&
              tidy_arg(cvar_v1(wcd,n)),&
              cvar_v2(wcd,n),cvar_type(wcd,n))
         m=n
      case(v_is_alias)
         m=tidy_arg(cvar_v1(wcd,n))
      case default
          m=n
      end select
    end function tidy_arg
  end subroutine comp_tidy_up
  
  !=======================================================================
  ! Dump generated wordcodes (debugging)
  !=======================================================================
  subroutine dump_wc(context,iunit)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    integer(pm_ln):: idx
    integer:: i,ii,j,n,code_size
    integer:: v
    type(pm_ptr):: p,lines,q,qq
    integer(pm_wc),dimension(:),allocatable:: code
    integer:: k
    integer:: line,modl
    character(len=100):: str,str2
    character(len=20):: ostr,mstr
    character(len=10),dimension(0:16):: v_names= (/&
          '          ',&
          'v_is_basic',&
          'v_is_group',&
          'v_is_sub  ',&
          'v_is_elem ',&
          'v_is_alias',&
          'v_is_vsub ',&
          'v_is_const',&
          'v_is_ve   ',&
          'v_is_cove ',&
          'v_is_parve',&
          'v_is_pstve',&
          'v_is_ctcst',&
          'v_is_cvect',&
          'v_is_uelem',&
          'v_is_vcwrp',&
          'v_is_vshar'/)
    call set_op_names
    do idx=1_pm_ln,pm_dict_size(context,context%funcs)
       p=pm_dict_val(context,context%funcs,idx)
       if(pm_fast_isnull(p)) then
          write(iunit,*) '----------NULL FUNC!-----------'
          cycle
       endif
       !call pm_dump_tree(context,6,p,2)
       q=p%data%ptr(p%offset)
       code_size=1+pm_fast_esize(q)
       if(allocated(code)) deallocate(code)
       allocate(code(code_size))
       if(pm_is_compiling) then
          code(1:code_size)=q%data%i(q%offset:q%offset+pm_fast_esize(q))
       else
          code(1:code_size)=q%data%i16(q%offset:q%offset+pm_fast_esize(q))
       endif
!!$       if(pm_is_compiling.and..not.pm_opts%old_files) then
!!$          !write(*,*) 'CODE_SIZE',code_size
!!$          if(code_size<8) cycle
!!$          qq=p%data%ptr(p%offset+1)
!!$          call print_comp_proc(context,iunit,code(3),int(idx),code(1),code(4),code(2),&
!!$               code(5:),1,qq%data%i(qq%offset:),context%funcs,p%data%ptr(p%offset:),2,.true.)
!!$          write(iunit,*)
!!$          cycle
!!$       endif
       write(iunit,*) idx-1,'$',&
            trim(pm_name_as_string(context,int(code(3)))),&
            ' (=='
       if(pm_is_compiling) then
          write(iunit,*) 'RETVAR=',code(1),'PVAR=',code(2),'SHARED_VE=',code(4)
          qq=p%data%ptr(p%offset+1)
          call dump_full_cvar(context,iunit,int(code(1)),2,.false.,qq%data%i(qq%offset:))
          call dump_full_cvar(context,iunit,int(code(2)),2,.false.,qq%data%i(qq%offset:))
       else
          write(iunit,*) 'STACKSIZE=',code(1),'NARGS=',code(2)
       endif
       if(code_size==merge(8,7,pm_is_compiling)) cycle
       i=merge(5,4,pm_is_compiling)
       do while(i<code_size)
          if(pm_is_compiling) then
             modl=mod(code(i+1),modl_mult)
             line=code(i+1)/modl_mult
             write(iunit,*) '->',code(i)
             i=i+2
          else
             call proc_line_module(p,i-3,line,modl)
          endif
          k=code(i)
          if(pm_is_compiling) then
             j=code(i+1)
             n=iand(code(i+2),int(comp_op_nargs_mask,pm_wc))
             ii=i-5
          else
             j=code(i+1)
             j=j*pm_ext_mult+code(i+2)/(pm_max_args+1)
             n=iand(int(code(i+2)),pm_max_args)
             ii=i
          endif
          str='at:'
          call pm_name_string(context,modl,str(len_trim(str)+2:))
         if(k==op_call.or.k==op_comm_call) then
             q=pm_dict_val(context,context%funcs,j+1_pm_ln)
             if(q%data%vkind==5) then
                write(iunit,'(i4,1x,a20,i4,a20,i4,1x,a15,i4)') ii,&
                  op_names(k),j,'??',n,str,line
                goto 7
             endif
             q=q%data%ptr(q%offset)
             str2='('
             if(pm_is_compiling) then
                call pm_name_string(context,q%data%i(q%offset+2),&
                     str2(2:))
             else
                call pm_name_string(context,int(q%data%i16(q%offset+2)),&
                     str2(2:))
             endif
             str2(len_trim(str2)+1:)=')'
             write(iunit,'(i4,1x,a20,i4,a20,i4,1x,a15,i4)') ii,&
                  op_names(k),j,str2,n,str,line
7            continue
          elseif(k>=first_assign_op.and.k<=last_assign_op) then
             str2='('
             call pm_name_string(context,proc_slot_name(p,i,j),str2(2:))
             str2(len_trim(str2)+1:)=')'
             write(iunit,'(i4,1x,a20,i4,a20,i4,1x,a15,i4)') ii,&
                  op_names(k),j,str2,n,str,line
          else if(k>=first_jmp_op.and.k<=last_jmp_op.and.k/=op_skip_empty) then
             write(iunit,'(i4,1x,a20,i6,1a,i6,1a,14x,i4,1x,a15,i4)') &
                  ii,&
                  op_names(k),j-pm_jump_offset,'(',&
                  i+j-pm_jump_offset+3+n,')',n,str,line
             write(iunit,*) j,j-pm_jump_offset
          elseif(k>=op_if.and.k<=op_if_restart) then
             write(iunit,'(i4,1x,a20,i6,1xi6,1h-,i6,11x,i4,1x,a15,i4)') &
                  ii,op_names(k),j,code(i+4),code(i+5),n,str,line
             if(k==op_if_shared_node) goto 20
             n=n-3
             i=i+3
          elseif(k==op_comm_loop.or.k==op_loop.or.&
               k==op_comm_block.or.k==op_comm_proc.or.k==op_over) then
             write(iunit,'(i4,1x,a20,"->",i6)') &
                  ii,op_names(k),code(i+4)
             n=n-2
             i=i+2
          elseif(k==op_allocate) then
             write(iunit,'(i4,1x,a20,i6)') &
                  ii,op_names(k),code(i+3)
             goto 20
          else if(k>=0.and.k<=num_op) then
             write(iunit,'(i4,1x,a20,i4,20x,i4,1x,a15,i4)') ii,&
                  op_names(k),j,n,str,line
          else
             write(iunit,'(i4,1x,1a,i4,1a,14x,i4,20x,i4,1x,a15,i4)') &
                  ii,'?',k,'?',j,n,str,line
          endif
          do j=1,n
             if(i+j+2>size(code)) then
                write(iunit,*) '      OUT OF ARGS!'
                exit
             endif
             k=code(i+j+2)
             qq=p%data%ptr(p%offset+1)
             if(pm_is_compiling) then
                call dump_full_cvar(context,iunit,abs(k),2,.false.,qq%data%i(qq%offset:))
             else
                if(k>0) then
                   call pm_name_string(context,proc_slot_name(p,i,int(k)),str)
                   write(iunit,*) '      Stack:',k,trim(str)
                else if(k>=-pm_max_stack.or.pm_is_compiling) then
                   call pm_name_string(context,proc_slot_name(p,i,-int(k)),str)
                   write(iunit,*) '      Stackref:',k,trim(str)
                else if(-k-pm_max_stack>=2.and.&
                     -k-pm_max_stack<=pm_fast_esize(p)) then
                   write(iunit,*) '      Const:',-k-pm_max_stack
                   call pm_dump_tree(context,iunit,&
                        p%data%ptr(p%offset-k-pm_max_stack),4)
                else
                   write(iunit,*) '      ???:',k
                endif
             endif
          enddo
20        continue
          i=i+max(0,n)+3
       enddo
       if(pm_is_compiling) then
          write(iunit,*) '-------'
          q=p%data%ptr(p%offset+1)
          i=1
          do while(i<pm_fast_esize(q))
             call dump_single_cvar(context,iunit,i,&
                  q%data%i(q%offset:))
          enddo
!!$          do i=0,pm_fast_esize(q)
!!$             v=q%data%i(q%offset+i)
!!$             write(iunit,*) i+1,v_names(iand(v,cvar_flag_mask)),v/cvar_flag_mult
!!$             if(iand(v,cvar_flag_mask)==v_is_const) &
!!$                  call pm_dump_tree(context,iunit,&
!!$                  p%data%ptr(p%offset+v/cvar_flag_mult+2),6)
!!$          enddo
       endif
       write(iunit,*) '==)'
    enddo
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
  end subroutine dump_wc

  !=======================================================================
  ! Dump compiler variable record (debugging)
  !=======================================================================
  recursive subroutine dump_cvar(wcd,iunit,n,adepth,nonest)
    type(wcoder):: wcd
    integer,intent(in):: iunit,n
    integer,intent(in),optional:: adepth
    logical,intent(in),optional:: nonest
    integer:: v,nn,depth
    character(len=10),dimension(0:16):: v_names= (/&
          '          ',&
          'v_is_basic',&
          'v_is_group',&
          'v_is_sub  ',&
          'v_is_elem ',&
          'v_is_alias',&
          'v_is_vsub ',&
          'v_is_const',&
          'v_is_ve   ',&
          'v_is_cove ',&
          'v_is_parve',&
          'v_is_pstve',&
          'v_is_ctcst',&
          'v_is_cvect',&
          'v_is_uelem',&
          'v_is_vcwrp',&
          'v_is_vshar'/)
    character(len=20):: spaces='                    '
    type(pm_ptr)::val
    if(.not.pm_is_compiling) return
    depth=1
    if(present(adepth)) depth=adepth
    do nn=n,n+2
       v=wcd%vinfo(nn)
       write(iunit,*) spaces(1:depth),nn,v_names(iand(v,cvar_flag_mask)),v/cvar_flag_mult
       if(iand(v,cvar_flag_mask)==v_is_const)  then
          val= wcd%values(v/cvar_flag_mult-1)
          if(pm_fast_vkind(val)==pm_name) then
             write(*,*) spaces(1:depth+6),trim(pm_name_as_string(wcd%context,int(val%offset)))
          else
             call pm_dump_tree(wcd%context,iunit,&
                  val,depth+6)
          endif
       endif
    enddo
    if(present(nonest)) then
       do nn=n+3,n+2+merge(wcd%vinfo(n)/cvar_flag_mult,0,iand(int(wcd%vinfo(n)),cvar_flag_mask)==v_is_group)
          v=wcd%vinfo(nn)
          write(iunit,*) spaces(1:depth),nn,v_names(iand(v,cvar_flag_mask)),v/cvar_flag_mult
       enddo
    else
       do nn=n+3,n+2+merge(wcd%vinfo(n)/cvar_flag_mult,0,iand(int(wcd%vinfo(n)),cvar_flag_mask)==v_is_group)
          call dump_cvar(wcd,iunit,cvar_strip_alias(wcd,wcd%vinfo(nn)/cvar_flag_mult),depth+2)
       enddo
    endif
  contains
    include 'fvkind.inc'
  end subroutine dump_cvar


  !=======================================================================
  ! Dump compiler variable record (debugging)
  !=======================================================================
  recursive subroutine dump_full_cvar(context,iunit,n,adepth,nonest,vinfo)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,n
    integer,intent(in):: adepth
    logical,intent(in):: nonest
    integer,dimension(:),intent(in):: vinfo
    integer:: v,nn,depth
    character(len=10),dimension(0:16):: v_names= (/&
          '          ',&
          'v_is_basic',&
          'v_is_group',&
          'v_is_sub  ',&
          'v_is_elem ',&
          'v_is_alias',&
          'v_is_vsub ',&
          'v_is_const',&
          'v_is_ve   ',&
          'v_is_cove ',&
          'v_is_parve',&
          'v_is_pstve',&
          'v_is_ctcst',&
          'v_is_cvect',&
          'v_is_uelem',&
          'v_is_vcwrp',&
          'v_is_vshar'/)
    character(len=20):: spaces='                    '
    type(pm_ptr)::val
    if(.not.pm_is_compiling) return
    if(n<=0.or.n==32767) then
       write(iunit,*) spaces(1:depth),n,'****'
       return
    endif
    depth=1
    if(.true.) depth=adepth
!!$    do nn=n,n+2
!!$       v=vinfo(nn)
!!$       write(iunit,*) spaces(1:depth),nn,v_names(iand(v,cvar_flag_mask)),v/cvar_flag_mult
!!$    enddo
    nn=n
    write(iunit,'(a)',advance="no") spaces(1:depth)
    call dump_single_cvar(context,iunit,nn,vinfo)
    select case(iand(int(vinfo(n)),cvar_flag_mask))
    case(v_is_group)
       do nn=n+3,n+2+vinfo(n)/cvar_flag_mult
          call dump_full_cvar(context,iunit,vinfo(nn)/cvar_flag_mult,depth+2,nonest,vinfo)
       enddo
    case(v_is_alias,v_is_vect_wrapped,v_is_elem)
       call dump_full_cvar(context,iunit,vinfo(n)/cvar_flag_mult,depth+2,nonest,vinfo)
    case(v_is_sub,v_is_vsub)
       call dump_full_cvar(context,iunit,vinfo(n)/cvar_flag_mult,depth+2,nonest,vinfo)
       call dump_full_cvar(context,iunit,vinfo(n+1)/cvar_flag_mult,depth+2,nonest,vinfo)
    end select
  contains
    include 'fvkind.inc'
  end subroutine dump_full_cvar

  
!!! MOVE OVER TO VMDEFS..
  
  subroutine dump_op(iunit,opcode,opcode2,args)
    integer,intent(in):: iunit
    integer(pm_wc),intent(in):: opcode,opcode2
    integer(pm_wc),intent(in),dimension(:):: args
    integer:: i
    write(iunit,'(a20,i6,10(i6,a1))') op_names(opcode),opcode2,&
         (abs(args(i)),merge('&',' ',args(i)<0),i=1,min(10,size(args)))
  end subroutine dump_op

  subroutine dump_single_cvar(context,iunit,n,array)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    integer,intent(inout):: n
    integer,dimension(:):: array
    character(len=5),dimension(0:16):: v_names= (/&
         '     ',&
         'basic',&
         'group',&
         'sub  ',&
         'elem ',&
         'alias',&
         'vsub ',&
         'const',&
         've   ',&
         'cove ',&
         'parve',&
         'pstve',&
         'ctcst',&
         'cvect',&
         'unit ',&
         'vwrap',&
         'share'/)
    
    character(len=7),dimension(v_is_tuple):: v_groups= (/&
         'array  ',&
         'struct ',&
         'dref   ',&
         'dref_s ',&
         'nostore',&
         'tuple  ' /)
    integer,parameter:: nflags=14
    character(len=7),dimension(nflags):: v_flags= (/&
         'used   ',&
         'poly   ',&
         'param  ',&
         'shared ',&
         'ref    ',&
         'result ',&
         'key    ',&
         'par    ',&
         'vect   ',&
         'in_dref',&
         'chan   ',&
         'par_vec',&
         'par_dom',&
         'farray '/)
    integer,parameter:: nflagstr=80
    character(len=nflagstr):: flag_str
    integer:: i,j,kind,v1,v2,typ
    kind=iand(cvar_flag_mask,array(n))
    v1=array(n)/cvar_flag_mult
    v2=array(n+1)/cvar_flag_mult
    typ=array(n+2)/cvar_flag_mult
    if(kind==v_is_group) then
       write(iunit,'(i6,1x,a5,1x,a7,1x,a,1x,10i6)') n,v_names(kind),&
            v_groups(v2),trim(pm_type_as_string(context,typ)),&
            (array(n+2+i)/cvar_flag_mult,i=1,min(10,v1))
       n=n+v1+3
    elseif(kind==v_is_basic) then
       flag_str=""
       i=1
       j=1
       do
          if(iand(v2,i)/=0) then
             flag_str=trim(flag_str)//" "//v_flags(j)
          endif
          i=i*2
          j=j+1
          if(j>nflags.or.flag_str(nflagstr:nflagstr)/=' ') exit
       enddo
       if(iand(v2,v_is_array_par_vect)==0) then
          write(iunit,'(i6,1x,a5,1x,a,1x,i6,a,1x,a)') n,v_names(kind),&
               trim(pm_name_as_string(context,v1)),&
               typ,trim(pm_type_as_string(context,typ)),flag_str
       else
          write(iunit,'(i6,1x,a5,1x,a,1x,i6,a,1x,a)') n,v_names(kind),&
               trim(pm_type_as_string(context,v1)),&
               typ,trim(pm_type_as_string(context,typ)),flag_str
       endif
       n=n+3
    elseif(kind==0) then
       n=n+1
    else
       write(iunit,'(i6,1x,a5,i6,i6,1x,a)') n,v_names(kind),&
            v1,v2,trim(pm_type_as_string(context,typ))
       n=n+3
    endif
  end subroutine dump_single_cvar
  
  
  !=======================================================================
  ! Output error for mismatching communicating operations
  !=======================================================================
  subroutine mismatch(wcd,node,node2,mess)
    type(wcoder):: wcd
    type(pm_ptr),intent(in):: node,node2
    character(len=*),intent(in),optional:: mess
    call wcode_error(wcd,node,'Communication operation mismatch: '//mess)
    call wcode_error(wcd,node2,'Mismatched operation')
  end subroutine mismatch
  
  !=======================================================================
  ! Output error
  !=======================================================================
  subroutine wcode_error(wcd,node,mess)
    type(wcoder):: wcd
    type(pm_ptr),intent(in):: node
    character(len=*):: mess
    type(pm_ptr):: modname
    character(len=100):: str
    if(pm_main_process) then
       call pm_error_header(wcd%context,cnode_get_name(node,cnode_modl_name),&
            cnode_get_name(node,cnode_lineno),cnode_get_name(node,cnode_charno))
       write(*,'(A,X,A)') trim(pm_opts%error),trim(mess)
    endif
    wcd%num_errors=wcd%num_errors+1
    if(wcd%num_errors>max_wcode_errors) then
       call pm_stop('Too many errors in final coding stage - compilation terminated')
    endif
  end subroutine wcode_error

end module pm_wcode

