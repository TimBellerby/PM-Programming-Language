!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2017
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

! Generate word-code for Virtual Machine
module pm_backend
  use pm_sysdep
  use pm_compbase
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_sysdefs
  use pm_parser
  use pm_codegen
  use pm_array
  implicit none
  
  integer,parameter:: max_code_size=2**15-1
  integer,parameter:: max_const=2**15-1-pm_max_args
  integer,parameter:: max_costack=2**15-1

  integer,parameter:: max_final_errors=5

  ! Information on coroutines
  type costate
     type(pm_ptr):: p,cblock,rv
     integer:: first_pc,base,num_named
     integer(pm_i16):: ve,new_ve
     logical:: break
  end type costate
  
  ! State for final code generation stage
  type finaliser
     type(pm_context),pointer:: context
     type(pm_reg),pointer:: reg
     type(pm_ptr):: temp,temp2

     ! Constant values
     type(pm_ptr),dimension(max_const):: values
     integer:: nval

     ! Caches for code and signatures
     type(pm_ptr):: code_cache,sig_cache

     ! Program counter
     integer:: pc,last

     ! Variable allocation
     logical,dimension(pm_max_stack):: in_use
     integer:: nvar,avar,npar,mvar

     ! Supplemental data field - one 16-bit integer per code node index
     integer(pm_i16),dimension(max_code_stack):: rdata
     integer:: base,top

     ! Word code buffer
     integer(pm_i16),dimension(max_code_size):: wc

     ! Coroutines
     integer,dimension(2):: cotop
     type(costate),dimension(2,max_costack):: costack
     integer:: cs

     ! Loop stack
     integer(pm_i16),dimension(0:max_loop_depth):: lstack
     integer:: ltop

     ! Current proc
     integer(pm_i16):: loop_extra_arg

     ! Debugging info
     integer(pm_i16):: cur_modl,cur_line
     integer(pm_i16):: find_pc
     integer:: num_errors
  end type finaliser

contains

  ! Initialise final-stage control structure
  subroutine init_fs(context,fs,sig_cache)
    type(pm_context),pointer:: context
    type(finaliser),intent(out):: fs
    type(pm_ptr),intent(in):: sig_cache
    fs%context=>context
    fs%reg=>pm_register(context,'FS',fs%temp,&
         fs%temp2,fs%code_cache,fs%sig_cache)
    fs%code_cache=pm_dict_new(context,32_pm_ln)
    fs%sig_cache=sig_cache
    fs%cs=1
    fs%cotop=0
  end subroutine init_fs

  ! Delete finalise stage control structure
  subroutine delete_fs(fs)
    type(finaliser):: fs
    call pm_delete_register(fs%context,fs%reg)
  end subroutine delete_fs

  ! Finalise main program
  subroutine finalise_prog(fs,p)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: p
    type(pm_ptr):: cblock,rv
    integer(pm_i16):: ve
    integer(pm_i16),dimension(1):: key
    integer:: junk
    logical:: break
    key(1)=0
    junk=pm_idict_add(fs%context,fs%code_cache,&
         key,1,pm_null_obj)
    if(pm_debug_level>2) &
         write(*,*) 'FINALISE PROG>'
    call init_final_proc(fs,p)
    cblock=cnode_arg(p,1)
    rv=cnode_arg(p,2)
    fs%base=0
    fs%top=pm_fast_esize(rv)+1
    fs%ltop=0
    fs%lstack(fs%ltop)=pm_stack_nullve
    ve=pm_stack_nullve
    break=finalise_cblock(fs,cblock,rv,ve)
    call make_proc_code(fs,1_pm_ln,int(sym_pm_system,pm_i16),ve)
    if(pm_debug_level>2) &
           write(*,*) 'FINALISE PROG COMPLETE>'
  contains
    include 'fesize.inc'
  end subroutine finalise_prog

  ! Finise procedure definitions
  subroutine finalise_procs(fs)
    type(finaliser),intent(inout):: fs
    type(pm_ptr):: prc,pr,rv,cblock,p,tv
    integer(pm_i16):: ve,k
    integer(pm_ln):: i,n
    fs%base=0
    i=2
    do while(i<=pm_dict_size(fs%context,fs%code_cache))
       p=pm_dict_key(fs%context,fs%code_cache,i)
       n=p%data%i16(p%offset)
       prc=pm_dict_val(fs%context,fs%sig_cache,n)
       call init_final_proc(fs,prc)
       rv=cnode_arg(prc,2)
       fs%top=pm_fast_esize(rv)+1
       pr=cnode_arg(prc,1)
       fs%lstack(fs%ltop)=pm_stack_nullve
       fs%ltop=fs%ltop+1
       ve=alloc_var(fs)
       fs%lstack(fs%ltop)=ve
       fs%loop_extra_arg=cnode_get_num(pr,pr_flags)
       fs%npar=cnode_get_num(pr,pr_nret)+&
            cnode_get_num(pr,pr_nkeys)+fs%loop_extra_arg
       fs%nvar=fs%npar+1
       fs%avar=fs%npar+1
       fs%in_use(1:fs%nvar)=.true.
       cblock=cnode_get(pr,pr_cblock)
       if(pm_debug_level>2) then
          write(*,*) 'FINALISE PROC>',i,&
               'SIGNO>',n,'VE>',ve,'NRET>',fs%npar
       endif
       call finalise_proc_body(fs,cblock,rv,ve)
       call release_var(fs,ve)
       call make_proc_code(fs,i,&
            int(cnode_get_num(pr,pr_name),pm_i16),ve)
       i=i+1
    end do
  contains
    include 'fesize.inc'
  end subroutine  finalise_procs

  ! Initialise finaliser state at start of proc
  subroutine init_final_proc(fs,prc)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: prc
    fs%pc=1
    fs%last=max_code_size
    fs%nval=0
    fs%nvar=0
    fs%mvar=0
    fs%avar=0
    fs%npar=0
    fs%ltop=0
    fs%rdata(1:pm_fast_esize(cnode_arg(prc,2))+1)=-1
    fs%loop_extra_arg=0
  contains
    include 'fesize.inc'
  end subroutine init_final_proc
  
  ! Make proc object
  subroutine make_proc_code(fs,i,name,ve)
    type(finaliser),intent(inout):: fs
    integer(pm_ln),intent(in):: i
    integer(pm_i16),intent(in):: name,ve
    integer:: n,m,vs,j,k
    type(pm_ptr):: p,p2
    if(pm_debug_level>2) write(*,*) 'MAKE PROC CODE>',i,&
         trim(pm_name_as_string(fs%context,int(name,pm_p)))
    call wc(fs,op_return)
    call wc(fs,0_pm_i16)
    call wc(fs,1_pm_i16+int(pm_max_stack+1,pm_i16))
    call wc(fs,ve)
    call tidy_up(fs)
    n=fs%nval
    m=max_code_size-fs%last
    fs%temp=pm_fast_new(fs%context,pm_pointer,int(n+2,pm_p))
    p=fs%temp
    call pm_ptr_assign(fs%context,&
         pm_dict_vals(fs%context,fs%code_cache),i-1,p)
    p2=pm_assign_new(fs%context,p,&
         0_pm_ln,pm_int16,int(fs%pc+2,pm_ln),.false.)
    p2%data%i16(p2%offset)=fs%mvar+pm_stack_locals ! Required stack size
    p2%data%i16(p2%offset+1)=fs%npar+1
    p2%data%i16(p2%offset+2)=name
    p2%data%i16(p2%offset+3:p2%offset+fs%pc+1)=fs%wc(1:fs%pc-1)
    p2=pm_assign_new(fs%context,fs%temp,&
         1_pm_ln,pm_int16,int(m,pm_ln),.false.)
    p2%data%i16(p2%offset:p2%offset+m-1)=fs%wc(fs%last+1:max_code_size)
    if(n>0) then
       p=fs%temp
       p%data%ptr(p%offset+2:p%offset+n+1)=&
            fs%values(1:n)
    endif
  contains
    include 'fvkind.inc'
    include 'fnew.inc'
  end subroutine make_proc_code

  ! Finalise a call block
  subroutine finalise_proc_body(fs,cblock,rv,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: cblock,rv
    integer(pm_i16),intent(in):: ve
    type(pm_ptr):: p
    integer:: par,num_named,first_pc
    logical:: break

    first_pc=fs%pc

    ! Allocate parameter variables
    par=finalise_pars(fs,cblock,rv,ve,p)
    
    ! Allocate multiple-use variables
    num_named=finalise_mvars(fs,cblock,rv,ve,p)

    ! Process calls
    p=cnode_get(cblock,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       break=finalise_call(fs,p,rv,ve,.false.)
       p=cnode_get(p,call_link)
    enddo

    ! Close variables
    call close_vars(fs,cblock,rv,ve,first_pc,num_named+par)

  contains
    include 'fisnull.inc'
  end subroutine finalise_proc_body

  ! Finalise a call block
  function finalise_cblock(fs,cblock,rv,ve) result(break)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: cblock,rv
    integer(pm_i16),intent(in):: ve
    logical:: break
    type(pm_ptr):: p
    integer:: num_named,first_pc

    ! Start block
    first_pc=fs%pc
    num_named=finalise_mvars(fs,cblock,rv,ve)
    p=cnode_get(cblock,cblock_first_call)
    
    ! Process calls
    do while(.not.pm_fast_isnull(p))
       break=finalise_call(fs,p,rv,ve,.false.)
       if(break) then
          call push_costate(fs,cblock,p,first_pc,&
               num_named,rv,ve)
          return
       endif
       p=cnode_get(p,call_link)
    enddo

    ! Close variables
    call close_vars(fs,cblock,rv,ve,first_pc,num_named)
  contains
    include 'fisnull.inc'
  end function  finalise_cblock

  ! Push costate
  subroutine push_costate(fs,cblock,p,first_pc,num_named,rv,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: cblock,rv,p
    integer(pm_i16),intent(in):: ve
    integer,intent(in):: first_pc,num_named
    integer:: cs,top
    cs=fs%cs
    top=fs%cotop(cs)+1
    fs%cotop(cs)=top
    if(top>max_costack) &
         call pm_panic('Program too complex')
    fs%costack(cs,top)%cblock=cblock
    fs%costack(cs,top)%p=p
    fs%costack(cs,top)%first_pc=first_pc
    fs%costack(cs,top)%num_named=num_named
    fs%costack(cs,top)%base=fs%base
    fs%costack(cs,top)%rv=rv
    fs%costack(cs,top)%ve=ve
  end subroutine push_costate

  ! Continue block where left off
  function restart_cblock(fs,ve) result(break)
    type(finaliser):: fs
    integer(pm_i16),intent(out):: ve
    type(pm_ptr):: cblock,rv
    logical:: break
    type(pm_ptr):: p
    integer:: num_named,first_pc,cs,top
    logical:: restart

    ! Pop state
    cs=3-fs%cs
    top=fs%cotop(cs)
    if(top<1) call pm_panic('restart cblock')
    cblock=fs%costack(cs,top)%cblock
    p=fs%costack(cs,top)%p
    if(pm_debug_level>3) then
       write(*,*) 'RESTART:',top,sym_names(max(0,-cnode_get_num(p,call_sig)))
    endif
    first_pc=fs%costack(cs,top)%first_pc
    num_named=fs%costack(cs,top)%num_named
    fs%base=fs%costack(cs,top)%base
    rv=fs%costack(cs,top)%rv
    ve=fs%costack(cs,top)%ve
    fs%cotop(cs)=top-1

    ! Process calls
    restart=.true.
    do while(.not.pm_fast_isnull(p))
       break=finalise_call(fs,p,rv,ve,restart)
       if(break) then
          call push_costate(fs,cblock,p,first_pc,&
               num_named,rv,ve)
          return
       endif
       p=cnode_get(p,call_link)
       restart=.false.
    enddo
    
    ! Close variables
    call close_vars(fs,cblock,rv,ve,first_pc,num_named)
    
  contains
    include 'fisnull.inc'
  end function restart_cblock
  
  ! Finalise parameter list
  function finalise_pars(fs,cblock,rv,ve,pp) result(npar)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: cblock,rv
    integer(pm_i16),intent(in):: ve
    type(pm_ptr),intent(out):: pp
    integer:: npar
    type(pm_ptr):: p
    integer:: slot
    npar=fs%npar
    p=cnode_get(cblock,cblock_first_var)
    if(.not.pm_fast_isnull(p)) then
       do while(cnode_flags_set(p,var_flags,var_param))
          slot=cnode_get_num(p,var_index)
          fs%rdata(slot+fs%base)=alloc_var(fs)
          npar=npar+1
          p=cnode_get(p,var_link)
          if(pm_fast_isnull(p)) exit
       enddo
    endif
    fs%npar=npar
    pp=p
  contains
    include 'fisnull.inc'
  end function finalise_pars
  
  ! Finalise multiple-use variables 
  function finalise_mvars(fs,cblock,rv,ve,pp) result(num_named)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: cblock,rv
    integer(pm_i16),intent(in):: ve
    type(pm_ptr),intent(in),optional:: pp
    integer:: num_named,slot
    type(pm_ptr):: p
    num_named=0
    if(present(pp)) then
       p=pp
    else
       p=cnode_get(cblock,cblock_first_var)
    endif
    do while(.not.pm_fast_isnull(p))
       if(arg_is_mvar(p)) then
          slot=cnode_get_num(p,var_index)
          fs%rdata(slot+fs%base)=alloc_var(fs)
          if(cnode_get_num(p,var_name)/=0) &
               num_named=num_named+1
       endif
       p=cnode_get(p,var_link)
    enddo    
  contains
    include 'fisnull.inc'
  end function finalise_mvars

  ! Close variables defined in a call block
  subroutine close_vars(fs,cblock,rv,ve,first_pc,nvars)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: cblock,rv
    integer(pm_i16),intent(in):: ve
    integer,intent(in):: first_pc,nvars
    type(pm_ptr):: p
    integer:: slot,j
    integer(pm_i16):: name

    ! Info entry for parameters & named multi-use variables
    if(nvars>0) then
       fs%wc(fs%last)=0
       fs%wc(fs%last-1)=nvars
       fs%wc(fs%last-2)=first_pc
       fs%wc(fs%last-3)=fs%pc-1
       fs%last=fs%last-4-nvars*2
       if(fs%last<=fs%pc) &
            call pm_panic('Program too complex')
    endif

    ! Release multi-use variables
    j=1
    p=cnode_get(cblock,cblock_first_var)
    do while(.not.pm_fast_isnull(p))
       if(arg_is_mvar(p).or.cnode_flags_set(p,var_flags,var_param)) then
          slot=cnode_get_num(p,var_index)
          call release_var(fs,fs%rdata(slot+fs%base))
          name=cnode_get_num(p,var_name)
          if(name/=0) then
             fs%wc(fs%last+j*2)=name
             fs%wc(fs%last+j*2-1)=fs%rdata(slot+fs%base)
             j=j+1
          endif
       endif
       p=cnode_get(p,var_link)
    enddo
    
  contains
    include 'fisnull.inc'
  end subroutine close_vars
  
  ! Finalise control structures / special calls
  function finalise_call(fs,callnode,rv,ve,restart) result(break)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: callnode,rv
    integer(pm_i16),intent(in):: ve
    logical,intent(in):: restart
    logical:: break

    type(pm_ptr):: args,arg,v,w,tv
    integer:: nargs,nret,n,slot,slot2,kk,costart,cs
    integer(pm_p):: m
    integer(pm_i16):: i,j,k,opr,tno,tk,name,name2,new_ve,new_ve2,sig
    integer(pm_i16):: save_find_pc,save_find_var
    integer(pm_ln):: siz
    logical:: varg,ok,break2

    if(pm_debug_level>0) then
       if(cnode_get_kind(callnode)/=cnode_is_call) &
            call pm_panic('Finalise call')
    endif
    
    break=.false.

    args=cnode_get(callnode,call_args)
    nargs=cnode_numargs(args)
    nret=cnode_get_num(callnode,call_nret)
    sig=-cnode_get_num(callnode,call_sig)

    if(pm_debug_level>2) then
       if(sig>0) then 
          write(*,*) 'Final call-->',sym_names(sig)
       else
          write(*,*) 'Final call--> sig=',-sig
          write(*,*) rvv(int(cnode_get_num(callnode,call_index)))
       endif
    endif
    select case(sig) 
    case(sym_if)
       if(restart) then
          break=restart_cblock(fs,new_ve)
          if(restart_cblock(fs,new_ve2).neqv.break) then
             call final_error(fs,callnode,&
                  'Communicating operations do not match'//&
                  ' in different branches of "if"/"select"')
          endif
          if(break) return
          call release_var(fs,new_ve)
          call release_var(fs,new_ve2)
       else
          new_ve=alloc_var(fs)
          call wc_call(fs,callnode,op_and_jmp_none,&
               -1_pm_i16,3,ve)
          call wc(fs,-new_ve)
          call wc_sarg(fs,cnode_arg(args,1),&
               pm_fast_isnull(cnode_arg(args,3)),rv,ve)
          k=fs%pc
          break2=finalise_cblock(fs,cnode_arg(args,2),rv,new_ve)
          if(fs%pc==k.and..not.break2) then
             fs%pc=j
          endif
          arg=cnode_arg(args,3)
          if(.not.pm_fast_isnull(arg)) then
             if(break2) then
                new_ve2=alloc_var(fs)
             else
                new_ve2=new_ve
             endif
             call wc_call(fs,callnode,op_andnot_jmp_none,&
                  -1_pm_i16,3,ve)
             call wc(fs,-new_ve2)
             call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve)
             k=fs%pc
             if(finalise_cblock(fs,arg,rv,new_ve2).neqv.break2) then
               call final_error(fs,callnode,&
                    'Communicating operations do not match '//&
                    'in different branches of "if"/"select"')
             endif
             if(.not.break2) call release_var(fs,new_ve2)
             if(fs%pc==k.and..not.break2) then
                fs%pc=j
             endif
             if(break2) then
                break=.true.
                return
             endif
          else
             if(break2) then
                call final_error(fs,callnode,&
                     'Communicating operations do not match in'//&
                     ' different branches of "if"/"select"')
             endif
          endif
          call release_var(fs,new_ve)
       endif
    case(sym_invar)
       new_ve=alloc_var(fs)
       j=wc_jump_call(fs,callnode,op_and_jmp_none,&
            0_pm_i16,3,ve)
       call wc(fs,-new_ve)
       call wc_sarg(fs,cnode_arg(args,1),&
            pm_fast_isnull(cnode_arg(args,3)),rv,ve)
       k=fs%pc
       call finalise_comm_block(fs,cnode_arg(args,2),&
            fs%lstack(fs%ltop-1),rv,new_ve)
       if(fs%pc==k) then
          fs%pc=j
       else
          call set_jump_to_here(j)
       endif
       arg=cnode_arg(args,3)
       if(.not.pm_fast_isnull(arg)) then
          j=wc_jump_call(fs,callnode,op_andnot_jmp_none,&
               0_pm_i16,3,ve)
          call wc(fs,-new_ve)
          call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve)
          k=fs%pc
          call finalise_comm_block(fs,cnode_arg(args,3),&
               fs%lstack(fs%ltop-1),rv,new_ve)
          if(fs%pc==k) then
             fs%pc=j
          else
             call set_jump_to_here(j)
          endif
       endif
    case(sym_while)
       if(restart) return
       if(cblock_has_at(cnode_arg(args,1))&
            .or.cblock_has_at(cnode_arg(args,3))) then
          break=.true.
          return
       endif
       new_ve=alloc_var(fs)
       call wc_call(fs,callnode,op_clone_ve,new_ve,1,ve)
       i=wc_jump_call(fs,callnode,op_jmp,0_pm_i16,1,ve)
       j=fs%pc
       break2=finalise_cblock(fs,cnode_arg(args,3),rv,new_ve)
       call set_jump_to_here(i)
       break2=finalise_cblock(fs,cnode_arg(args,1),rv,new_ve)
       call wc_call(fs,callnode,op_and_jmp_any,&
            j,3,new_ve)
       call wc(fs,-new_ve)
       call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
       call release_var(fs,new_ve)
    case(sym_repeat)
       if(restart) return
       if(cblock_has_at(cnode_arg(args,2))) then
          break=.true.
          return
       endif
       new_ve=alloc_var(fs)
       call wc_call(fs,callnode,op_clone_ve,new_ve,1,ve)
       i=fs%pc
       break2=finalise_cblock(fs,cnode_arg(args,1),rv,new_ve)
       call wc_call(fs,callnode,op_andnot_jmp_any,&
            i,3,new_ve)
       call wc(fs,-new_ve)
       call wc_arg(fs,cnode_arg(args,2),.false.,rv,new_ve)
       call release_var(fs,new_ve)
    case(sym_each)
       if(restart) return
       if(cblock_has_at(cnode_arg(args,1))) then
          break=.true.
          return
       endif
       new_ve=alloc_var(fs)
       call wc_call(fs,callnode,op_clone_ve,new_ve,1,ve)
       i=wc_jump_call(fs,callnode,op_jmp,0_pm_i16,1,ve)
       j=fs%pc
       if(sig==-sym_find) then
          save_find_pc=fs%find_pc
          fs%find_pc=wc_jump_call(fs,callnode,op_jmp,0_pm_i16,1,ve)
       endif
       break2=finalise_cblock(fs,cnode_arg(args,1),rv,new_ve)
       call set_jump_to_here(i)
       call wc_call(fs,callnode,op_and_jmp_any,&
            j,3,new_ve)
       call wc(fs,-new_ve)
       call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
       call release_var(fs,new_ve)
    case(sym_do)
       if(restart) then
          break=restart_cblock(fs,new_ve)
       else
          break=finalise_cblock(fs,cnode_arg(args,1),rv,ve)
       endif
    case(sym_sync,sym_colon)
       break=.not.restart
       return
    case(sym_for,sym_also)
       call finalise_comm_block(fs,cnode_arg(args,1),&
            fs%lstack(fs%ltop-1),rv,ve)
    case(sym_pct)
       new_ve=pm_stack_locals+1
       fs%lstack(fs%ltop)=ve
       fs%ltop=fs%ltop+1
       if(fs%ltop>max_loop_depth) &
            call pm_panic('program too complex (nested loops)')
       break=finalise_cblock(fs,cnode_arg(args,1),rv,new_ve)
       fs%ltop=fs%ltop-1
    case(sym_hash)
       if(check_arg_type(args,rv,2)/=pm_null) then
          break=finalise_cblock(fs,cnode_arg(args,1),rv,ve)
       endif
    case(sym_endfor)
       if(check_arg_type(args,rv,7)==pm_null) then
          call wc_call(fs,callnode,op_setref,0_pm_i16,3,ve)
          call wc_arg(fs,cnode_arg(args,2),.true.,rv,ve)
          call wc_arg(fs,cnode_arg(args,9),.false.,rv,ve)
          call for_body
       else
          v=cnode_arg(args,8)
          v=cnode_arg(v,1)
          slot=v%data%i(v%offset)
          slot2=v%data%i(v%offset+1)
          w=pm_dict_val(fs%context,fs%sig_cache,int(&
               rvv(int(cnode_get_num(callnode,call_index))),pm_ln))
          v=cnode_arg(w,1)
          rv%data%i16(rv%offset+slot:rv%offset+slot2)=&
               v%data%i16(v%offset:v%offset+slot2-slot)
          j=wc_jump_call(fs,callnode,op_jmp_noshare,0_pm_i16,1,ve)
          call wc_call(fs,callnode,op_setref,0_pm_i16,3,ve)
          call wc_arg(fs,cnode_arg(args,2),.true.,rv,ve)
          call wc_arg(fs,cnode_arg(args,7),.false.,rv,ve)
          call for_body
          break2=finalise_cblock(fs,cnode_arg(args,5),rv,ve)
          if(break2) then
             call final_error(fs,callnode,'Cannot have communicating operators in loop sync')
          endif
          k=wc_jump_call(fs,callnode,op_jmp,0_pm_i16,1,ve)
          call set_jump_to_here(j)
          v=cnode_arg(w,2)
          rv%data%i16(rv%offset+slot:rv%offset+slot2)=&
               v%data%i16(v%offset:v%offset+slot2-slot)
          call wc_call(fs,callnode,op_setref,0_pm_i16,3,ve)
          call wc_arg(fs,cnode_arg(args,2),.true.,rv,ve)
          call wc_arg(fs,cnode_arg(args,9),.false.,rv,ve)
          call for_body
          call set_jump_to_here(k)
       endif
    case(sym_find)
       if(check_arg_type(args,rv,9)==pm_null) then
          call wc_call(fs,callnode,op_setref,0_pm_i16,3,ve)
          call wc_arg(fs,cnode_arg(args,2),.true.,rv,ve)
          call wc_arg(fs,cnode_arg(args,9),.false.,rv,ve)
          call find_body
       else
          v=cnode_arg(args,10)
          v=cnode_arg(v,1)
          slot=v%data%i(v%offset)
          slot2=v%data%i(v%offset+1)
          w=pm_dict_val(fs%context,fs%sig_cache,int(&
               rvv(int(cnode_get_num(callnode,call_index))),pm_ln))
          v=cnode_arg(w,1)
          rv%data%i16(rv%offset+slot:rv%offset+slot2)=&
               v%data%i16(v%offset:v%offset+slot2-slot)
          j=wc_jump_call(fs,callnode,op_jmp_noshare,0_pm_i16,1,ve)
          call wc_call(fs,callnode,op_setref,0_pm_i16,3,ve)
          call wc_arg(fs,cnode_arg(args,2),.true.,rv,ve)
          call wc_arg(fs,cnode_arg(args,9),.false.,rv,ve)
          call find_body
          break2=finalise_cblock(fs,cnode_arg(args,7),rv,ve)
          if(break2) then
             call final_error(fs,callnode,'Cannot have communicating operators in loop sync')
          endif
          k=wc_jump_call(fs,callnode,op_jmp,0_pm_i16,1,ve)
          call set_jump_to_here(j)
          v=cnode_arg(w,2)
          rv%data%i16(rv%offset+slot:rv%offset+slot2)=&
               v%data%i16(v%offset:v%offset+slot2-slot)
          call wc_call(fs,callnode,op_setref,0_pm_i16,3,ve)
          call wc_arg(fs,cnode_arg(args,2),.true.,rv,ve)
          call wc_arg(fs,cnode_arg(args,11),.false.,rv,ve)
          call find_body
          call set_jump_to_here(k)
       endif
    case(sym_reduce,sym_reduce_at)
       new_ve=alloc_var(fs)
       if(sig==sym_reduce_at) then
          call wc_call(fs,callnode,op_gather,new_ve,1+nret/2,ve)
          do n=1,nret/2
             call wc(fs,-get_var_slot(fs,cnode_arg(args,n+nret+1)))
          enddo
       else
          call wc_call(fs,callnode,op_clone_ve,new_ve,1,ve)
       endif
       j=wc_jump_call(fs,callnode,op_reduce,0_pm_i16,nargs+1,ve)
       call wc(fs,-new_ve)
       do n=1,nret
          call wc_arg(fs,cnode_arg(args,n),.true.,rv,ve)
       enddo
       do n=1,nret
          call wc_arg(fs,cnode_arg(args,n+nret+1),.false.,rv,ve)
       enddo
       break=finalise_cblock(fs,cnode_arg(args,nret+1),rv,new_ve)
       i=wc_jump_call(fs,callnode,op_jmp,j,1,ve)
       call set_jump_to_here(j)
       call release_var(fs,new_ve)
    case(sym_endtype) ! Each proc
       slot=cnode_get_num(callnode,call_index)
       slot2=rvv(slot)
       if(slot2==0) then
          break=finalise_cblock(fs,cnode_arg(args,nret+2),&
               rv,ve)
       else
          w=pm_dict_val(fs%context,fs%sig_cache,int(&
               rvv(int(cnode_get_num(callnode,call_index))),pm_ln))
          n=cnode_numargs(w)
          v=cnode_arg(args,nret+4)
          v=cnode_arg(v,1)
          slot=v%data%i(v%offset)
          slot2=v%data%i(v%offset+1)
          do i=0,n-1
             do j=nret+5,nargs-1,2
                call wc_call(fs,callnode,op_elem,i+2_pm_i16,3,ve)
                call wc_arg(fs,cnode_arg(args,j+1),.true.,rv,ve)
                call wc_arg(fs,cnode_arg(args,j+0),.false.,rv,ve)
             enddo
             v=cnode_arg(w,i+1)
             rv%data%i16(rv%offset+slot:rv%offset+slot2)=&
                  v%data%i16(v%offset:v%offset+slot2-slot)
             if(cblock_has_at(cnode_arg(args,nret+3))) then
                call finalise_comm_block(fs,cnode_arg(args,nret+3),&
                     fs%lstack(fs%ltop-1),rv,ve)
             else
                break=finalise_cblock(fs,cnode_arg(args,nret+3),rv,ve)
             endif
             do j=1,nret
                k=cnode_get_num(cnode_arg(args,nargs-nret+j),var_index)
                fs%rdata(fs%top+(j-1)*n+i)=fs%rdata(k)
             enddo
          enddo
          do j=1,nret
             i=check_arg_type(args,rv,int(j))
             v=pm_typ_vect(fs%context,i)
             if(pm_tv_kind(v)==pm_typ_is_struct) then
                call wc_call(fs,callnode,op_struct,i,n+2,ve)
             elseif(pm_tv_kind(v)==pm_typ_is_rec) then
                call wc_call(fs,callnode,op_rec,i,n+2,ve)
             else
                call pm_panic('Finalise each proc')
             endif
             call wc_arg(fs,cnode_arg(args,int(j)),.true.,rv,ve)
             do i=0,n-1
                call wc(fs,fs%rdata(fs%top+(j-1)*n+i))
             enddo
          enddo
          break=finalise_cblock(fs,cnode_arg(args,nret+1),rv,ve)
       endif
    case(sym_any)
       v=cnode_arg(args,4)
       v=cnode_arg(v,1)
       slot=v%data%i(v%offset)
       slot2=v%data%i(v%offset+1_pm_p)
       w=pm_dict_val(fs%context,fs%sig_cache,int(&
            rvv(int(cnode_get_num(callnode,call_index))),pm_ln))
       new_ve=alloc_var(fs)
       do kk=1,cnode_numargs(w)
          arg=cnode_arg(w,kk)
           rv%data%i16(rv%offset+slot:rv%offset+slot2)=&
               arg%data%i16(arg%offset:arg%offset+slot2-slot)
          tno=check_arg_type(args,rv,1)
          call wc_call(fs,callnode,op_any,tno,4,ve)
          call wc(fs,-new_ve)
          call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
          call wc_sarg(fs,cnode_arg(args,3),kk<cnode_numargs(w),rv,ve)
          break2=finalise_cblock(fs,cnode_arg(args,2),rv,new_ve)
       enddo
       call release_var(fs,new_ve)
    case(sym_lt)
       tno=cnode_get_num(args,cnode_args+1)
       call wc_call(fs,callnode,op_make_poly,tno,3,ve)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
       call wc_arg(fs,cnode_arg(args,3),.false.,rv,ve)
    case(sym_struct,sym_rec)
       i=fs%pc
       j=check_arg_type(args,rv,1)
       call wc_call(fs,callnode,op_struct+int(sig-sym_struct,pm_i16),&
            j,nargs,ve)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
       do kk=3,nargs
          arg=cnode_arg(args,kk)
             call wc_arg(fs,cnode_arg(args,kk),.false.,rv,ve)
       enddo
    case(sym_dot,sym_dotref)
       i=rvv(cnode_get_num(callnode,call_index))
       if(sig==sym_dotref) then
          j=op_elem_ref
       else
          j=op_elem
       endif
       if(i>0) then
          call wc_call(fs,callnode,j,i,3,ve)
          call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
       else
          v=pm_dict_val(fs%context,fs%sig_cache,int(-i,pm_ln))
          call wc_call(fs,callnode,j,v%data%i16(v%offset+1),3,ve)
          call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
          k=get_var_slot(fs,cnode_arg(args,1))
          do kk=3,pm_fast_esize(v),2
             call wc_call(fs,callnode,j,v%data%i16(v%offset+kk),3,ve)
             call wc(fs,-k)
             call wc(fs,k)
          enddo
       endif
    case(sym_dash)
       j=check_arg_type(args,rv,1)
       call wc_call_args(fs,callnode,args,op_dash,j,3,1,rv,ve)
    case(sym_check)
       if(check_arg_type(args,rv,2)==pm_logical) then
          call wc_call_args(fs,callnode,args,op_check,0_pm_i16,2,0,rv,ve)
       endif
    case(sym_present)
       v=cnode_arg(cnode_arg(args,3),1)
       k=v%data%i(v%offset)+pm_stack_locals+fs%loop_extra_arg+1
       call wc_call(fs,callnode,op_get_key,k,4,ve)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
       call wc_arg(fs,cnode_arg(args,2),.true.,rv,ve)
       call wc_arg(fs,cnode_arg(args,4),.false.,rv,ve)
    case(sym_key)
       v=cnode_arg(cnode_arg(args,2),1)
       k=v%data%i(v%offset)+pm_stack_locals+fs%loop_extra_arg+1
       call wc_call_args(fs,callnode,args,op_get_key2,k,1,1,rv,ve)
    case(sym_default)
       call wc_call_args(fs,callnode,args,op_default,&
            check_arg_type(args,rv,1),1,1,rv,ve)
    case(sym_open)
       continue ! Nothing to do
    case(sym_result)
       call wc_call_args(fs,callnode,args,op_return,&
            fs%loop_extra_arg,nargs,0,rv,ve)
    case default
       if(sig>0) then
          write(*,*) 'SIG=',sig
          write(*,*) sym_names(sig)
          call pm_panic('Unknown call type in final_call')
       endif
       if(.not.cnode_flags_clear(callnode,call_flags,&
            ior(call_is_reduce_call,call_is_loop_call))) then
          if(.not.restart) break=.true.
          return
       endif
       call finalise_proc_call(fs,callnode,rv,ve,0_pm_i16,&
            args,nargs,nret,sig)
    end select
  contains
    include 'fisnull.inc'
    include 'ftiny.inc'
    include 'fnewnc.inc'
    include 'fesize.inc'

    function rvv(n) result(m)
      integer,intent(in):: n
      integer(pm_i16):: m
      m=rv%data%i16(rv%offset+n)
    end function rvv

    subroutine set_jump_to_here(j)
      integer(pm_i16):: j
      fs%wc(j+1)=fs%pc
    end subroutine set_jump_to_here

    function cblock_has_at(cblock) result(ok)      
      type(pm_ptr):: cblock
      logical:: ok
      ok=(iand(cnode_get_num(cblock,cblock_flags),&
               cblock_contains_at)/=0)
    end function cblock_has_at

    subroutine for_body
      integer(pm_i16):: j
      break2=finalise_cblock(fs,cnode_arg(args,4),rv,ve)
      if(break2) then
         call final_error(fs,callnode,&
              'Cannot have communicating operations in partition/workshare')
      endif
      call wc_call(fs,callnode,op_par_loop,&
           0_pm_i16,3,ve)
      call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
      call wc_arg(fs,cnode_arg(args,6),.false.,rv,ve)
      j=wc_jump_call(fs,callnode,op_jmp,0_pm_i16,1,ve)
      new_ve=get_var_slot(fs,cnode_arg(args,1))
      fs%lstack(fs%ltop)=ve
      fs%ltop=fs%ltop+1
      if(fs%ltop>max_loop_depth) &
           call pm_panic('program too complex (nested loops)')
      break2=finalise_cblock(fs,cnode_arg(args,3),rv,new_ve)
      call wc_call(fs,callnode,op_par_loop_end,0_pm_i16,1,ve)
      call set_jump_to_here(j)
      fs%ltop=fs%ltop-1
      
    end subroutine for_body

    subroutine find_body
      integer(pm_i16):: j
      break2=finalise_cblock(fs,cnode_arg(args,6),rv,ve)
      if(break2) then
         call final_error(fs,callnode,&
              'Cannot have communicating operations in partition/workshare')
      endif
      call wc_call(fs,callnode,op_par_find,&
           0_pm_i16,5,ve)
      call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
      call wc_arg(fs,cnode_arg(args,3),.true.,rv,ve)
      call wc_arg(fs,cnode_arg(args,4),.true.,rv,ve)
      call wc_arg(fs,cnode_arg(args,8),.false.,rv,ve)
      j=wc_jump_call(fs,callnode,op_jmp,0_pm_i16,1,ve)
      new_ve=get_var_slot(fs,cnode_arg(args,1))
      fs%lstack(fs%ltop)=ve
      fs%ltop=fs%ltop+1
      break2=finalise_cblock(fs,cnode_arg(args,5),rv,new_ve)
      if(break2) call pm_panic('Break out of find')
      call wc_call(fs,callnode,op_par_find_end,0_pm_i16,1,ve)
      call set_jump_to_here(j)
      fs%ltop=fs%ltop-1
    end subroutine find_body
    
  end function  finalise_call

  ! Finalise actual procedure calls
  recursive subroutine finalise_proc_call(fs,callnode,rv,ve,ve2,&
       args,nargs,nret,sig)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: callnode,rv
    integer(pm_i16),intent(in):: ve,ve2
    type(pm_ptr),intent(in):: args
    integer,intent(in):: nargs,nret
    integer(pm_i16),intent(in):: sig
    integer(pm_i16):: i,j,tk,op2
    integer:: k
    type(pm_ptr):: v,w,arg
    logical:: varg,ok
    integer:: extra_ve
    if(ve2==0) then
       extra_ve=0
    else
       extra_ve=1
    endif
    i=rvv(cnode_get_num(callnode,call_index))
    v=pm_dict_val(fs%context,fs%sig_cache,int(i,pm_ln))
    varg=cnode_flags_set(callnode,call_flags,call_is_vararg)
    tk=cnode_get_kind(v)
    if(tk==cnode_is_single_proc) then
       if(varg) then
          call wc_call(fs,callnode,op_vcall,&
               int(pm_stack_locals+fs%npar-1,pm_i16),&
               nargs+1+extra_ve,ve)
          if(ve2/=0) call wc(fs,ve2)
          call wc_arglist(fs,callnode,args,&
               nargs,nret,rv,ve)
          call wc_call(fs,callnode,op_call,add_proc(i),1,ve)
       else
          call wc_call(fs,callnode,op_call,add_proc(i),&
               nargs+1+extra_ve,ve)
          if(ve2/=0) call wc(fs,ve2)
          call wc_arglist(fs,callnode,args,&
               nargs,nret,rv,ve)
       endif
    elseif(tk==cnode_is_var_proc) then
       w=v
       fs%nval=fs%nval+1
       i=build_var_call(w)
       if(varg) then
          call wc_call(fs,callnode,op_vcall,&
               int(pm_stack_locals+fs%npar-1,pm_i16),nargs+2+extra_ve,ve)
       else
          call wc_call(fs,callnode,op_var_call,&
               i,nargs+2+extra_ve,ve)
       endif
       call wc_arg(fs,cnode_arg(w,3),.false.,rv,ve)
       if(ve2/=0) call wc(fs,ve2)
       call wc_arglist(fs,callnode,args,nargs,nret,rv,ve)
       if(varg) then
          call wc_call(fs,callnode,op_var_call,i,1,ve)
       endif
    elseif(tk==cnode_is_multi_proc) then
       i=build_poly_call(v)
       if(varg) then
          call wc_call(fs,callnode,op_vcall,&
               int(pm_stack_locals+fs%npar-1,pm_i16),&
               nargs+1+extra_ve,ve)
          if(ve2/=0) call wc(fs,ve2)
          call wc_arglist(fs,callnode,args,&
               nargs,nret,rv,ve)
          call wc_call(fs,callnode,op_poly_call,i,1,ve)
       else
          call wc_call(fs,callnode,op_poly_call,i,nargs+1+extra_ve,ve)
          if(ve2/=0) call wc(fs,ve2)
          call wc_arglist(fs,callnode,args,&
               nargs,nret,rv,ve)
       endif
    else
       if(tk/=cnode_is_builtin) then
          write(*,*) 'tk=',tk
          call pm_panic('not builtin')
       endif
       op2=cnode_get_num(v,bi_opcode2)
       if(cnode_flags_set(v,bi_flags,proc_needs_type)) then
          op2=check_arg_type(args,rv,1)
       endif

       ! Builtin
       j=cnode_get_num(v,bi_opcode)
       if(varg) then
          call wc_call(fs,callnode,op_vcall,&
               int(pm_stack_locals+fs%npar-1,pm_i16),&
               nargs+1+extra_ve,ve)
          if(ve2/=0) call wc(fs,ve2)
          call wc_arglist(fs,callnode,args,&
               nargs,nret,rv,ve)
          call wc_call(fs,callnode,j,&
               op2,1,ve)
       else
          call wc_call(fs,callnode,j,op2,nargs+1+extra_ve,ve)
          if(ve2/=0) call wc(fs,ve2)
          call wc_arglist(fs,callnode,args,&
               nargs,nret,rv,ve)
       endif
    endif
    
  contains

    include 'fisnull.inc'
    include 'ftiny.inc'
    include 'fnewnc.inc'
    
    function rvv(n) result(m)
      integer,intent(in):: n
      integer(pm_i16):: m
      m=rv%data%i16(rv%offset+n)
    end function rvv

    ! Add a proc to the cache
    ! Return slot number in cache
    function add_proc(sig) result(n)
      integer(pm_i16),intent(in):: sig
      integer(pm_i16):: n
      integer(pm_i16),dimension(1):: key
      key(1)=sig
      n=pm_ivect_lookup(fs%context,fs%code_cache,key,1)-1
      if(n<0) then
         n=pm_idict_add(fs%context,fs%code_cache,key,1,pm_null_obj)-1
      endif
    end function add_proc

    ! Build the dispatch structure for call to proc variable
    ! Returns constant slot number
    function build_var_call(w) result(i)
      type(pm_ptr),intent(in):: w
      type(pm_ptr):: v
      integer:: i
      integer:: k,m
      i=add_const(fs,pm_dict_new(fs%context,8_pm_ln))
      do k=4,cnode_numargs(w),4
         v=pm_dict_val(fs%context,fs%sig_cache,&
              int(cnode_get_num(w,k+cnode_args+2),pm_ln))
         fs%temp2=pm_fast_newnc(fs%context,pm_pointer,4_pm_p)
         fs%temp2%data%ptr(fs%temp2%offset)=&
              pm_fast_tinyint(fs%context,&
              int(op_poly_call,pm_p))
         fs%temp2%data%ptr(fs%temp2%offset+1_pm_p)=&
              pm_fast_tinyint(fs%context,&
              build_poly_call(v))
         fs%temp2%data%ptr(fs%temp2%offset+2_pm_p)=cnode_arg(w,k+1)
         m=nret+1
         if(cnode_flags_set(w,cnode_args+k+1,proc_has_vkeys)) m=-m
         fs%temp2%data%ptr(fs%temp2%offset+3_pm_p)=&
              pm_fast_tinyint(fs%context,int(m,pm_p))
         call pm_dict_set(fs%context,fs%values(i-1),&
              cnode_arg(w,k),fs%temp2,.true.,.true.,ok)
         fs%temp2=pm_null_obj
      enddo

    end function  build_var_call

    ! Build the dispatch structure for polymorphic call
    ! Returns constant slot number
    function build_poly_call(v) result(k)
      type(pm_ptr):: v
      integer(pm_p):: k
      integer:: i
      integer(pm_ln):: siz
      type(pm_ptr):: arg
      call wc(fs,int(cnode_get_num(v,cnode_args),pm_i16))
      do i=2,cnode_numargs(v),3
         call wc(fs,int(cnode_get_num(v,cnode_args+i),pm_i16))
         j=cnode_get_num(v,cnode_args+i+1)
         arg=pm_dict_val(fs%context,fs%sig_cache,int(j,pm_ln))
         if(cnode_get_kind(arg)==cnode_is_single_proc) then
            call wc(fs,op_call)
            call wc(fs,add_proc(j))
         else
            call wc(fs,int(cnode_get_num(arg,bi_opcode),pm_i16))
            call wc(fs,int(cnode_get_num(arg,bi_opcode2),pm_i16))
         endif
      enddo
      siz=cnode_numargs(v)
      fs%temp=pm_new(fs%context,pm_int16,siz)
      fs%temp%data%i16(fs%temp%offset:fs%temp%offset+siz-1)=&
           fs%wc(fs%pc-siz:fs%pc-1)
      fs%pc=fs%pc-siz
      k=add_const(fs,fs%temp)
      fs%temp=pm_null_obj
    end function  build_poly_call

  end subroutine finalise_proc_call

  recursive subroutine finalise_comm_block(fs,cblock,outve,rv,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: cblock,rv
    integer(pm_i16),intent(in):: outve,ve
    integer:: cs,costart
    integer(pm_i16)::new_ve
    logical:: break
    costart=fs%cotop(fs%cs)+1
    break=finalise_cblock(fs,cblock,rv,ve)
    cs=fs%cs
    do while(break)
       if(pm_debug_level>3) &
            write(*,*) 'OPS>',fs%cs,fs%cotop(fs%cs),fs%cotop(3-fs%cs)
       call combine_ops(fs,costart,outve,rv,ve)
       fs%cs=3-fs%cs
       costart=fs%cotop(fs%cs)+1
       break=restart_cblock(fs,new_ve)
    enddo
    fs%cs=cs
  end subroutine finalise_comm_block

  recursive subroutine combine_ops(fs,costart,out_ve,loop_rv,loop_ve)
    type(finaliser),intent(inout):: fs
    integer,intent(in):: costart
    type(pm_ptr),intent(in):: loop_rv
    integer(pm_i16),intent(in):: loop_ve,out_ve
    integer:: i,sym
   
    do i=costart,fs%cotop(fs%cs)
       sym=-cnode_get_num(fs%costack(fs%cs,i)%p,call_sig)
       select case(sym)
       case(sym_if,sym_for,sym_do)
          continue
       case(sym_while,sym_repeat,sym_each)
          call combine_loops(fs,i,fs%costack(fs%cs,i)%p,out_ve,&
               loop_rv,loop_ve)
          return
       case(sym_colon)
          call combine_labels(fs,i,fs%costack(fs%cs,i)%p,out_ve,&
               loop_rv,loop_ve)
          return
       case default
          if(sym<0.or.sym==sym_sync) then
             call combine_calls(fs,i,fs%costack(fs%cs,i)%p,out_ve,&
                  loop_rv,loop_ve)
             return
          else
             write(*,*) sym_names(sym)
             call pm_panic('combine ops')
          endif
       end select
    enddo
  end subroutine combine_ops
  
  recursive subroutine combine_loops(fs,costart,first_p,out_ve,&
       loop_rv,loop_ve)
    type(finaliser),intent(inout):: fs
    integer,intent(in):: costart
    type(pm_ptr),intent(in):: first_p
    type(pm_ptr),intent(in):: loop_rv
    integer(pm_i16),intent(in):: loop_ve,out_ve
    integer:: i,sym,newcostart,base,n
    integer(pm_i16):: j,k
    logical:: break,anybreak,allbreak
    integer(pm_i16):: ve,new_ve,start
    type(pm_ptr):: args,p,oldp,rv
    integer:: cotop,cs,numve
    numve=0
    cotop=fs%cotop(fs%cs)
    cs=fs%cs
    do i=costart,cotop
       p=fs%costack(cs,i)%p
       sym=-cnode_get_num(p,call_sig)
       if(sym==sym_if.or.sym==sym_do.or.sym==sym_for) cycle
       args=cnode_get(p,call_args)
       new_ve=alloc_var(fs)
       fs%costack(cs,i)%new_ve=new_ve
       numve=numve+1
       ve=fs%costack(cs,i)%ve
       base=fs%costack(cs,i)%base
       rv=fs%costack(cs,i)%rv
       select case(sym)
       case(sym_while)
          j=wc_jump_call(fs,p,op_jmp_empty_ve,0_pm_i16,1,ve)
          if(finalise_cblock(fs,cnode_arg(args,1),rv,ve)) &
               call final_error(fs,args,&
               'Communicating operation inside "while" test expression')
          call set_jump_to_here(j)
          k=wc_jump_call(fs,p,op_and_jmp_any,0_pm_i16,3,ve)
          call wc(fs,-new_ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
          call set_jump_to_here(k)
       case(sym_repeat) 
          call wc_call(fs,p,op_clone_ve,new_ve,1,ve)
       case(sym_each)
          k=wc_jump_call(fs,p,op_and_jmp_any,0_pm_i16,3,ve)
          call wc(fs,-new_ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
          call set_jump_to_here(k)
       case default
          if(sym==-sym_colon) then
             call mismatch(fs,args,first_p,&
                  'communicating loop matched with label')
          else
             call mismatch(fs,args,first_p,&
                  'communicating loop matched with '//&
                  'single communicating operation')
          endif
       end select
    enddo
    start=fs%pc
    anybreak=.false.
    allbreak=.true.
    newcostart=fs%cotop(fs%cs)+1
    n=0
    do i=costart,cotop
       p=fs%costack(cs,i)%p
       sym=-cnode_get_num(p,call_sig)
       if(sym==sym_if.or.sym==sym_do.or.sym==sym_for) cycle
       args=cnode_get(p,call_args)
       base=fs%costack(cs,i)%base
       rv=fs%costack(cs,i)%rv
       new_ve=fs%costack(cs,i)%new_ve
       j=wc_jump_call(fs,p,op_jmp_empty_ve,0_pm_i16,1,new_ve)
       k=fs%pc
       select case(sym)
       case(sym_while)
          break=finalise_cblock(fs,cnode_arg(args,3),rv,new_ve)
          n=n+1
       case(sym_repeat) 
          break=finalise_cblock(fs,cnode_arg(args,1),rv,new_ve)
          n=n+1
       case(sym_each) 
          break=finalise_cblock(fs,cnode_arg(args,1),rv,new_ve)
          n=n+1
       end select
       if(k==fs%pc) then
          fs%pc=j
       else
          call set_jump_to_here(j)
       endif
       anybreak=anybreak.or.break
       allbreak=allbreak.and.break
       if(anybreak.and..not.allbreak) then
          call mismatch(fs,first_p,p,&
               'communicating operators do not match in'//&
               ' corresponding loops')
       endif
    enddo
    do while(anybreak)
       anybreak=.false.
       allbreak=.true.
       call combine_ops(fs,newcostart,out_ve,loop_rv,loop_ve)
       fs%cs=3-fs%cs
       newcostart=fs%cotop(fs%cs)+1
       do i=1,n
          p=fs%costack(3-fs%cs,fs%cotop(3-fs%cs))%p
          j=wc_jump_call(fs,p,op_jmp_empty_ve,0_pm_i16,1,&
               fs%costack(3-fs%cs,fs%cotop(3-fs%cs))%ve)
          k=fs%pc
          break=restart_cblock(fs,ve)
          if(k==fs%pc) then
             fs%pc=j
          else
             call set_jump_to_here(j)
          endif
          anybreak=anybreak.or.break
          allbreak=allbreak.and.break
          if(anybreak.and..not.allbreak) then
             call mismatch(fs,oldp,p,&
                  'communicating operators in corresponding loops')
          endif
          oldp=p
       enddo
    enddo
    do i=costart,cotop
       p=fs%costack(cs,i)%p
       sym=-cnode_get_num(p,call_sig)
       args=cnode_get(p,call_args)
       ve=fs%costack(cs,i)%new_ve
       base=fs%costack(cs,i)%base
       rv=fs%costack(cs,i)%rv
       if(sym==sym_if.or.sym==sym_do.or.sym==sym_for) cycle
       select case(sym)
       case(sym_while)
          j=wc_jump_call(fs,p,op_jmp_empty_ve,0_pm_i16,1,new_ve)
          break=finalise_cblock(fs,cnode_arg(args,1),rv,ve)
          call set_jump_to_here(j)
          k=wc_jump_call(fs,p,op_and_jmp_any,0_pm_i16,3,ve)
          call wc(fs,-ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
          call set_jump_to_here(k)
       case(sym_repeat) 
          k=wc_jump_call(fs,p,op_andnot_jmp_any,0_pm_i16,3,ve)
          call wc(fs,-ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
          call set_jump_to_here(k)
       case(sym_each) 
          k=wc_jump_call(fs,p,op_and_jmp_any,0_pm_i16,3,ve)
          call wc(fs,-ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
          call set_jump_to_here(k)
       end select
    enddo
    call wc_call(fs,p,op_jmp_any_ve,start,numve+1,&
         loop_ve)
    do i=costart,cotop
       p=fs%costack(cs,i)%p
       sym=-cnode_get_num(p,call_sig)
       if(sym==sym_if.or.sym==sym_do.or.sym==sym_for) cycle
       call wc(fs,fs%costack(cs,i)%new_ve)
    enddo
    fs%cs=cs
  contains
    subroutine set_jump_to_here(j)
      integer(pm_i16):: j
      fs%wc(j+1)=fs%pc
    end subroutine set_jump_to_here
  end subroutine combine_loops


  subroutine combine_labels(fs,costart,first_p,out_ve,&
       loop_rv,loop_ve)
    type(finaliser),intent(inout):: fs
    integer,intent(in):: costart
    type(pm_ptr),intent(in):: first_p,loop_rv
    integer(pm_i16),intent(in):: loop_ve,out_ve
    type(pm_ptr):: p,n
    integer(pm_p):: name,name2
    integer:: j,sig2
    n=cnode_arg(cnode_arg(cnode_get(p,call_args),1),1)
    name=n%offset
    do j=costart+1,fs%cotop(fs%cs)
       p=fs%costack(fs%cs,j)%p
       sig2=-cnode_get_num(p,call_sig)
       select case(sig2)
       case(sym_if,sym_for,sym_do)
          cycle
       case(sym_colon)
          n=cnode_arg(cnode_arg(cnode_get(p,call_args),1),1)
          name2=n%offset
          if(name/=name2) then
             call mismatch(fs,first_p,p,&
                  'labels do not match:'//&
                  trim(pm_name_as_string(fs%context,name))//' / '//&
                  trim(pm_name_as_string(fs%context,name)))
          endif
       case(sym_while,sym_each,sym_repeat)
          call mismatch(fs,first_p,p,&
               'label matched to communicating loop')
       case default
          call mismatch(fs,first_p,p,&
               'label matched to communicating operation')
       end select
    enddo
  end subroutine combine_labels

  recursive subroutine combine_calls(fs,costart,first_p,out_ve,&
       loop_rv,loop_ve)
    type(finaliser),intent(inout):: fs
    integer,intent(in):: costart
    type(pm_ptr),intent(in):: first_p,loop_rv
    integer(pm_i16),intent(in):: loop_ve,out_ve

    integer:: i,j,n,nret,nargs,nkeys,nloop
    integer:: nret2,nargs2,nkeys2,nloop2,start,start2
    integer(pm_i16):: sig,sig2,call_pos
    type(pm_ptr):: p,rv,args,args2,vv1,vv2
    type(pm_ptr):: name1,name2
    
    ! First stacked operation - passed into routine
    sig=cnode_get_num(first_p,call_sig)
    args=cnode_get(first_p,call_args)
    nargs=cnode_numargs(args)
    nret=cnode_get_num(first_p,call_nret)
    nkeys=cnode_get_num(first_p,call_nkeys)
    nloop=cnode_get_num(first_p,call_nloop)
    n=cnode_numargs(args)

    ! Is first stacked operation a call?
    if(sig>0) then
       call_pos=costart
       start=loop_call_extra_args+nret+nkeys
    else
       call_pos=-1
       start=0
       nloop=nargs
    endif

    ! Check all calls have same number of loop args
    do j=costart+1,fs%cotop(fs%cs)
       p=fs%costack(fs%cs,j)%p
       sig2=cnode_get_num(p,call_sig)
       if(sig2==-sym_if.or.sig2==-sym_for.or.sig2==-sym_do) cycle
       nloop2=cnode_get_num(p,call_nloop)
       args2=cnode_get(p,call_args)
       nargs2=cnode_numargs(args2)
       nret2=cnode_get_num(p,call_nret)
       nkeys2=cnode_get_num(p,call_nkeys)
       
       ! Cannot match two calls together
       if(sig2>0) then
          if(call_pos>0) then
             call mismatch(fs,fs%costack(fs%cs,call_pos)%p,p,&
                  'two loop calls/operators matched together')
             return
          else
             call_pos=j
             start2=loop_call_extra_args+nkeys2
          endif
       elseif(sig2==-sym_sync) then
          nloop2=nargs2
          start2=0
       else
          if(sig2==sym_colon) then
             call mismatch(fs,first_p,p,&
                  'communicating operation matched to label')
             return
          else
             call mismatch(fs,first_p,p,&
                  'communicating operation'//&
                  ' matched to communicating loop')
             return
          endif
       endif

       if(nloop/=nloop2) then
          call mismatch(fs,first_p,p,&
               'different number of loop arguments')
          return
       else
          do i=1,nloop
             vv1=cnode_arg(args,i+start)
             vv2=cnode_arg(args2,i+start2)
             if(.not.(vv1==vv2)) then
                write(*,*) cnode_get_kind(vv1),cnode_get_kind(vv2)
                name1=cnode_get(vv1,var_name)
                name2=cnode_get(vv2,var_name)
                call mismatch(fs,first_p,p,&
                      'loop arguments are not the same variable: '//&
                      trim(pm_name_as_string(fs%context,name1%offset))//&
                      ' <> '//&
                      trim(pm_name_as_string(fs%context,name2%offset)))
                write(*,*) '_===>',start,start2
                return
             endif
          enddo
       endif
    enddo
    
    if(call_pos>=0) then
       p=fs%costack(fs%cs,call_pos)%p
       sig=cnode_get_num(p,call_sig)
       args=cnode_get(p,call_args)
       nargs=cnode_numargs(args)
       nret=cnode_get_num(p,call_nret)
       nkeys=cnode_get_num(p,call_nkeys)
       nloop=cnode_get_num(p,call_nloop)
       call finalise_proc_call(fs,p,&
            fs%costack(fs%cs,call_pos)%rv,out_ve,&
            fs%costack(fs%cs,call_pos)%ve,&
            args,nargs,nret,sig)
    else
       call final_error(fs,first_p,&
            '"sync" operation does not match a loop call or operator')
    endif

  end subroutine combine_calls

  ! Code call to a jump operator
  function wc_jump_call(fs,callnode,op_s,op_a,nargs,ve) result(pc)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: callnode
    integer(pm_i16),intent(in):: op_s,op_a,ve
    integer,intent(in):: nargs
    integer:: pc
    pc=fs%pc
    call wc_call(fs,callnode,op_s,op_a,nargs,ve)   
  end function wc_jump_call

  ! Code call with arguments
  subroutine wc_call_args(fs,callnode,args,op,op2,nargs,nret,rv,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: callnode,args,rv
    integer(pm_i16),intent(in):: op,op2,ve
    integer,intent(in):: nargs,nret
    call wc_call(fs,callnode,op,op2,nargs+1,ve)
    call wc_arglist(fs,callnode,args,nargs,nret,rv,ve)
  end subroutine wc_call_args
  
  ! Code argument list
  subroutine wc_arglist(fs,callnode,args,nargs,nret,rv,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: callnode,args,rv
    integer(pm_i16),intent(in):: ve
    integer,intent(in):: nargs,nret
    integer:: i
    integer(pm_i16):: slot
    type(pm_ptr):: arg

    do i=1+nret,nargs
       arg=cnode_arg(args,i)
       if(arg_is_svar(arg)) then
          slot=get_var_slot(fs,arg)
          call release_var(fs,slot)
       endif
    enddo
    do i=1,nargs
       arg=cnode_arg(args,i)
       if(i>nret.and.arg_is_svar(arg)) then
          call wc(fs,get_var_slot(fs,arg))
       else
          call wc_arg(fs,arg,i<=nret,rv,ve)
       endif
    enddo
  end subroutine wc_arglist

  ! Just code call -- arguments must be coded
  ! seperately
  subroutine wc_call(fs,node,op,op2,nargs,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: node
    integer(pm_i16),intent(in):: op,op2,ve
    integer,intent(in):: nargs
    integer(pm_i16):: modl,line
    modl=cnode_get_num(node,cnode_modl_name)
    line=cnode_get_num(node,cnode_lineno)
    if(modl/=fs%cur_modl) then
       fs%last=fs%last-2
       fs%wc(fs%last+1)=fs%pc
       fs%wc(fs%last+2)=modl
    endif
    if(line/=fs%cur_line) then
       fs%last=fs%last-2
       fs%wc(fs%last+1)=fs%pc
       fs%wc(fs%last+2)=-line
    endif
    if(pm_debug_level>0) then
       if(cnode_get_kind(node)/=cnode_is_call) then
          call pm_panic('wc call not callnode')
       endif
    endif
    if(cnode_get_num(node,call_loop_depth)<fs%ltop) then
       call wc_simple_call(fs,op,op2,nargs,&
            fs%lstack(cnode_get_num(node,call_loop_depth)))
    else
       call wc_simple_call(fs,op,op2,nargs,ve)
    endif
  end subroutine wc_call

  subroutine wc_simple_call(fs,op,op2,nargs,ve)
    type(finaliser),intent(inout):: fs
    integer(pm_i16),intent(in):: op,op2,ve
    integer,intent(in):: nargs
    call wc(fs,op)
    call wc(fs,op2)
    call wc(fs,int(nargs+(pm_max_stack+1)*&
         (fs%nvar+pm_stack_locals-1),pm_i16))
    call wc(fs,ve)
  end subroutine wc_simple_call

  ! Code one argument of call/operation
  subroutine wc_arg(fs,argnode,isret,rv,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: argnode
    logical,intent(in):: isret
    type(pm_ptr),intent(in):: rv
    integer(pm_i16),intent(in):: ve
    integer(pm_i16):: k,t,akind
    integer(pm_p):: sym
    integer:: slot
    type(pm_ptr):: arg,ass
    arg=argnode
    akind=cnode_get_kind(arg)
    if(arg_is_svar(arg)) then
       ! Single use variable
       if(isret) then 
          ! Make temp var
          k=alloc_var(fs)
          call wc(fs,-k) 
          slot=cnode_get_num(arg,var_index)
          fs%rdata(slot+fs%base)=k
       else
          ! Get temp var
          slot=cnode_get_num(arg,var_index)
          k=fs%rdata(slot+fs%base)
          call wc(fs,k)
          call release_var(fs,k)
       endif
    else if(cnode_get_kind(arg)==cnode_is_var) then
       ! Multiple use variable
       slot=cnode_get_num(arg,var_index)
       if(isret) then
          call wc(fs,-fs%rdata(slot))
       else
          call wc(fs,fs%rdata(slot))
       endif
    else
       if(pm_debug_level>0) then
          if(isret) &
               call pm_panic(&
               'wc_arg: Attempt to return to const')
       endif
       ! Constant
       call wc(fs,&
            -32_pm_i16*(add_const(fs,cnode_arg(arg,1))+&
            int(pm_max_stack,pm_i16)))
    endif
  contains
    include 'fisnull.inc'

    function rvv(n) result(m)
      integer(pm_p),intent(in):: n
      integer(pm_i16):: m
      m=rv%data%i16(rv%offset+n)
    end function rvv

  end subroutine wc_arg

  subroutine wc_sarg(fs,arg,lastuse,rv,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: arg
    logical:: lastuse
    type(pm_ptr),intent(in):: rv
    integer(pm_i16),intent(in):: ve
    if(.not.lastuse) then
       if(.not.arg_is_svar(arg)) then
          call wc_arg(fs,arg,.false.,rv,ve)
       else
          call wc(fs,get_var_slot(fs,arg))
       endif
    else
       call wc_arg(fs,arg,.false.,rv,ve)
    endif
  end subroutine wc_sarg
  
  ! Type of variable to use for given PM kind
  function var_kind(vkind) result(varkind)
    integer:: vkind
    integer:: varkind
    if(vkind<=pm_null) then
       varkind=pm_pointer
    else  if(vkind>=pm_string) then
       varkind=pm_pointer
    else
       varkind=vkind
    endif
  end function var_kind

  ! Add a new constant to the current procedures constant pool
  function add_const(fs,val) result(n)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: val
    integer(pm_i16):: n

    n=fs%nval+1
    fs%nval=n
    fs%values(n)=val
    n=n+1
  contains
    include 'fvkind.inc'
  end function add_const

  ! Allocate variable
  function alloc_var(fs) result(k)
    type(finaliser),intent(inout):: fs
    integer:: i
    integer(pm_i16)::k
    if(fs%nvar==fs%avar) then
       fs%nvar=fs%nvar+1
       if(fs%nvar>fs%mvar) fs%mvar=fs%nvar
       fs%avar=fs%nvar
       fs%in_use(fs%nvar)=.true.
       k=fs%nvar
    else
       k=-21
       fs%avar=fs%avar+1
       do i=1,fs%nvar
          if(.not.fs%in_use(i)) then
             fs%in_use(i)=.true.
             k=i
             exit
          endif
       enddo
       if(pm_debug_level>0) then
          if(k==-21) call pm_panic('ak')
       endif
    endif
    k=k+pm_stack_locals-1
    if(pm_debug_level>3) write(*,*) 'Alloc var:',k
  end function alloc_var
  
  ! Release variable
  subroutine release_var(fs,slot)
    type(finaliser),intent(inout):: fs
    integer(pm_i16),intent(in):: slot
    integer:: k
    k=slot-pm_stack_locals+1
    if(pm_debug_level>0) then
       if(k<1.or.k>fs%mvar) then
          write(*,*) 'k=',k,fs%mvar
          call pm_panic('release var')
       endif
    endif
    if(.not.fs%in_use(k)) return
    if(k==fs%nvar) then
       fs%nvar=fs%nvar-1
    endif
    fs%in_use(k)=.false.
    fs%avar=fs%avar-1
    if(pm_debug_level>3) then
       write(*,*) 'Release var>',slot,k,'Alloced=',fs%avar,'Tot=',fs%nvar
    endif
  end subroutine release_var

  ! Get slot associated with variable
  function get_var_slot(fs,arg) result(slot)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: arg
    integer(pm_i16):: slot
    integer:: i
    i=cnode_get_num(arg,var_index)
    slot=fs%rdata(fs%base+i)
  end function get_var_slot
  
  ! Check type of args[n]
  function check_arg_type(args,rv,n) result(tno)
    type(pm_ptr),intent(in):: args,rv
    integer,intent(in):: n
    integer(pm_i16):: tno
    type(pm_ptr):: var,arg
    integer(pm_p):: i,k
    var=cnode_arg(args,n)
    k=cnode_get_num(var,cnode_kind)
    if(k==cnode_is_const) then
       tno=pm_fast_typeof(cnode_arg(var,1))
       return
    endif
    i=cnode_get_num(var,var_index)
    tno=rv%data%i16(rv%offset+i)
  contains
    include 'ftypeof.inc'
  end function check_arg_type

  ! Is argument a single use variable?
  function arg_is_svar(arg) result(ok)
    type(pm_ptr),intent(in):: arg
    logical:: ok
    ok=.false.
    if(cnode_get_kind(arg)==cnode_is_var) then
       if(cnode_flags_clear(arg,var_flags,&
            ior(var_multi_access,var_changed))) then
          ok=.true.
       endif
    endif
  end function arg_is_svar

  ! Is argument a multiple use variable
  function arg_is_mvar(arg) result(ok)
    type(pm_ptr),intent(in):: arg
    logical:: ok
    ok=.false.
    if(cnode_get_kind(arg)==cnode_is_var) then
      if(.not.cnode_flags_clear(arg,var_flags,&
            ior(ior(var_multi_access,var_changed),var_param))) then
         ok=.true.
      endif
    endif
  end function arg_is_mvar

  ! Code one word of code
  subroutine wc(fs,val)
    type(finaliser),intent(inout):: fs
    integer(pm_i16),intent(in):: val
    if(fs%pc>=fs%last) &
         call pm_panic('Program element too large')
    fs%wc(fs%pc)=val
    fs%pc=fs%pc+1
    if(pm_debug_level>2) then
       write(*,*) 'WC',fs%pc-1,val,fs%wc(fs%pc-1)
    endif
  end subroutine wc

  ! Tidy up procedure code.
  ! - follow loop chains
  subroutine tidy_up(fs)
    type(finaliser),intent(inout):: fs
    integer:: i,j,nargs
    integer(pm_i16)::code,code2,arg
    integer(pm_i16),dimension(pm_pointer):: vstart
    call set_op_names
    i=1
    do
       code=fs%wc(i)
       if(pm_debug_level>2) then
          write(*,*) 'TIDY>',i,code,op_names(code)
       endif
       code2=fs%wc(i+1)
       nargs=iand(fs%wc(i+2),127_pm_i16)
       ! Follow jump chain
       if(code>=first_jmp_op.and.code<=last_jmp_op) then
          do while(fs%wc(code2)==op_jmp)
             if(code2>i) then
                code2=fs%wc(code2+1)
             else
                code2=code2+3+fs%wc(code2+2)+fs%wc(code2+1)
             endif
          enddo
          if(code2>=0) then
             ! Change to relative displacement
             fs%wc(i+1)=code2-(i+3+nargs)
          else
             fs%wc(i+1)=0
          endif
       endif
       ! Change arguments
       do j=i+3,i+3+nargs-1
          arg=fs%wc(j)
          if(arg<-pm_max_stack) then
             fs%wc(j)=arg/32_pm_i16  !!! Correct
          endif
       enddo
       i=i+nargs+3
       if(i>=fs%pc) exit
    enddo
  end subroutine tidy_up

  subroutine dump_wc(context,iunit)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    integer(pm_ln):: idx
    integer:: i,j,n
    type(pm_ptr):: p,code,lines,q
    integer(pm_i16):: k
    integer(pm_p):: line,modl
    character(len=100):: str,str2
    character(len=20):: ostr,mstr
    call set_op_names
    do idx=1_pm_ln,pm_dict_size(context,context%funcs)
       p=pm_dict_val(context,context%funcs,idx)
       if(pm_fast_isnull(p)) then
          write(iunit,*) '----------NULL FUNC!-----------'
          cycle
       endif
!!$       code=p%data%ptr(p%offset+1)
!!$       do i=code%offset,code%offset+pm_fast_esize(code)
!!$          write(iunit,*) 'INFO>',i,code%data%i16(i)
!!$       enddo
       code=p%data%ptr(p%offset)
       do i=code%offset,code%offset+pm_fast_esize(code)
          write(iunit,*) 'CODE>',i-code%offset,&
               code%data%i16(i),iand(code%data%i16(i),pm_max_stack)
       enddo
       write(iunit,*) idx-1,&
            trim(pm_name_as_string(context,&
            int(code%data%i16(code%offset+2),pm_p))),&
            ' (=='
       write(iunit,*) 'STACKSIZE=',&
            code%data%i16(code%offset),'NARGS=',code%data%i16(code%offset+1)
       i=code%offset+3
       do
          k=code%data%i16(i)
          j=code%data%i16(i+1)
          n=iand(code%data%i16(i+2),127)
          call proc_line_module(p,i-int(code%offset)+1,line,modl)
          str='at:'
          call pm_name_string(context,modl,str(len_trim(str)+2:))
          if(k==op_call) then
             q=pm_dict_val(context,context%funcs,j+1_pm_ln)
             q=q%data%ptr(q%offset)
             str2='('
             call pm_name_string(context,int(q%data%i16(q%offset+2),pm_p),&
                  str2(2:))
             str2(len_trim(str2)+1:)=')'
             write(iunit,'(i4,1x,a20,i4,a20,i4,1x,a15,i4)') i-code%offset,&
                  op_names(k),j,str2,n,str,line
          elseif(k>=first_assign_op.and.k<=last_assign_op) then
             str2='('
             call pm_name_string(context,proc_slot_name(p,i,j),str2(2:))
             str2(len_trim(str2)+1:)=')'
             write(iunit,'(i4,1x,a20,i4,a20,i4,1x,a15,i4)') i-code%offset,&
                  op_names(k),j,str2,n,str,line
          else if(k>=first_jmp_op.and.k<=last_jmp_op) then
             write(iunit,'(i4,1x,a20,i4,1a,i4,1a,14x,i4,1x,a15,i4)') &
                  i-code%offset,&
                  op_names(k),j,'(',i+j+3+n-code%offset,')',n,str,line
          else if(k>=0.and.k<=num_op) then
             write(iunit,'(i4,1x,a20,i4,20x,i4,1x,a15,i4)') i-code%offset,&
                  op_names(k),j,n,str,line
          else
             write(iunit,'(i4,1x,1a,i4,1a,14x,i4,20x,i4,1x,a15,i4)') &
                  i-code%offset,'?',k,'?',j,n,str,line
          endif
          do j=1,n
             k=code%data%i16(i+j+2)
             if(k>0) then
                call pm_name_string(context,proc_slot_name(p,i,int(k)),str)
                write(iunit,*) '      Stack:',k,trim(str)
             else if(k>=-pm_max_stack) then
                call pm_name_string(context,proc_slot_name(p,i,-int(k)),str)
                write(iunit,*) '      Stackref:',k,trim(str)
             else if(-k-pm_max_stack<=pm_fast_esize(p)) then
                call pm_dump_tree(context,iunit,&
                     p%data%ptr(p%offset-k-pm_max_stack),3)
             else
                write(iunit,*) '      ???:',k
             endif
          enddo
          i=i+n+3
          if(i>code%offset+pm_fast_esize(code)) exit
       enddo
       write(iunit,*) '==)'
    enddo
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
  end subroutine dump_wc

  subroutine mismatch(fs,node,node2,mess)
    type(finaliser):: fs
    type(pm_ptr),intent(in):: node,node2
    character(len=*),intent(in),optional:: mess
    call final_error(fs,node,'Communication operation mismatch: '//mess)
    call final_error(fs,node2,'Mismatched operation')
  end subroutine mismatch

  subroutine final_error(fs,node,mess)
    type(finaliser):: fs
    type(pm_ptr),intent(in):: node
    character(len=*):: mess
    type(pm_ptr):: modname
    character(len=100):: str
    if(pm_main_process) then
       call pm_name_string(fs%context,cnode_get_name(node,cnode_modl_name),str)
       write(*,*) 'Error:',trim(str),' line:',cnode_get_num(node,cnode_lineno)
       write(*,*) mess
    endif
    fs%num_errors=fs%num_errors+1
    if(fs%num_errors>max_final_errors) &
         call pm_stop(&
         'Too many errors in final coding - compilation terminated')
  end subroutine final_error

end module pm_backend
