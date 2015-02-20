!
!PM (Parallel Models) Programming Language
!
!Released under the MIT License (MIT)
!
!Copyright (c) Tim Bellerby, 2015
!
!Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
!The above copyright notice and this permission notice shall be included in
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
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_sysdefs
  use pm_parser
  use pm_codegen
  implicit none
  
  integer,parameter:: max_code_size=9999
  integer,parameter:: max_const=9999-pm_max_args

  type finaliser
     type(pm_context),pointer:: context
     type(pm_reg),pointer:: reg
     type(pm_ptr),dimension(max_const):: values
     type(pm_ptr):: temp,code_cache,sig_cache
     integer:: nval,pc,last
     logical,dimension(pm_max_stack):: in_use
     integer:: nvar,avar,npar,mvar
     integer(pm_i16),dimension(max_code_stack):: rdata
     integer(pm_i16),dimension(max_code_size):: wc
     integer(pm_i16):: cur_modl,cur_line
  end type finaliser

contains

  ! Initialise final-stage control structure
  subroutine init_fs(context,fs,sig_cache)
    type(pm_context),pointer:: context
    type(finaliser),intent(out):: fs
    type(pm_ptr),intent(in):: sig_cache
    fs%context=>context
    fs%reg=>pm_register(context,'FS',fs%temp,&
         fs%code_cache,fs%sig_cache)
    fs%code_cache=pm_dict_new(context,32_pm_ln)
    fs%sig_cache=sig_cache
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
    integer(pm_i16),dimension(2):: key
    key(1)=0
    key(2)=0
    ve=pm_idict_add(fs%context,fs%code_cache,&
         key,2,pm_null_obj)
    if(pm_debug_level>2) &
         write(*,*) 'FINALISE PROG>'
    call init_final_proc(fs,p)
    cblock=cnode_arg(p,1)
    rv=cnode_arg(p,2)
    ve=0
    call finalise_cblock(fs,cblock,rv,ve,0)
    call make_proc_code(fs,1_pm_ln,int(sym_pm_system,pm_i16))
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
    i=2
    do while(i<=pm_dict_size(fs%context,fs%code_cache))
       p=pm_dict_key(fs%context,fs%code_cache,i)
       n=p%data%i16(p%offset)
       prc=pm_dict_val(fs%context,fs%sig_cache,n)
       ve=p%data%i16(p%offset+1_pm_p)
       call init_final_proc(fs,prc)
       rv=cnode_arg(prc,2)
       pr=cnode_arg(prc,1)
       fs%npar=cnode_get_num(pr,pr_nret)
       fs%nvar=fs%npar
       fs%avar=fs%npar
       fs%in_use(1:fs%nvar)=.true.
       cblock=cnode_get(pr,pr_cblock)
       if(pm_debug_level>2) &
            write(*,*) 'FINALISE PROC>',i,'SIGNO>',n,'VE>',ve,'NRET>',fs%npar
       call finalise_cblock(fs,cblock,rv,ve,0)
       call make_proc_code(fs,i,int(cnode_get_num(pr,pr_name),pm_i16))
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
    fs%rdata(1:pm_fast_esize(cnode_arg(prc,2))+1)=-1
  contains
    include 'fesize.inc'
  end subroutine init_final_proc
  
  ! Make proc object
  subroutine make_proc_code(fs,i,name)
    type(finaliser),intent(inout):: fs
    integer(pm_ln),intent(in):: i
    integer(pm_i16),intent(in):: name
    integer:: n,m,vs,j,k
    type(pm_ptr):: p,p2
    if(pm_debug_level>2) write(*,*) 'MAKE PROC CODE>',i,&
         trim(pm_name_as_string(fs%context,int(name,pm_p)))
    call wc(fs,op_return)
    call wc(fs,0_pm_i16)
    call wc(fs,0_pm_i16)
    call tidy_up(fs)
    n=fs%nval
    m=max_code_size-fs%last
    fs%temp=pm_fast_new(fs%context,pm_pointer,int(n+2,pm_p))
    call pm_ptr_assign(fs%context,&
         pm_dict_vals(fs%context,fs%code_cache),i-1,fs%temp)
    call pm_assign_new(fs%context,fs%temp,&
         0_pm_ln,pm_int16,int(fs%pc+2,pm_ln),.false.)
    call pm_assign_new(fs%context,fs%temp,&
         1_pm_ln,pm_int16,int(m,pm_ln),.false.)
    p=fs%temp
    p2=p%data%ptr(p%offset)
    p2%data%i16(p2%offset)=fs%mvar+pm_stack_locals ! Required stack size
    p2%data%i16(p2%offset+1)=fs%npar
    p2%data%i16(p2%offset+2)=name
    p2%data%i16(p2%offset+3:p2%offset+fs%pc+1)=fs%wc(1:fs%pc-1)
    p2=p%data%ptr(p%offset+1_pm_p)
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
  subroutine finalise_cblock(fs,cblock,rv,ve,base)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: cblock,rv
    integer(pm_i16),intent(in):: ve
    integer,intent(in):: base
    type(pm_ptr):: p
    integer:: slot,par,num_named,first_pc,j
    integer(pm_i16):: name
    integer(pm_p):: tk
    first_pc=fs%pc
    ! Allocate parameter variables
    par=fs%npar
    p=cnode_get(cblock,cblock_first_var)
    if(.not.pm_fast_isnull(p)) then
       do while(iand(cnode_get_num(p,var_flags),var_param)/=0)
          slot=cnode_get_num(p,var_index)
          fs%rdata(slot+base)=alloc_var(fs)
          par=par+1
          p=cnode_get(p,var_link)
          if(pm_fast_isnull(p)) exit
       enddo
    endif
    fs%npar=par
    ! Allocate multiple-use variables
    num_named=0
    do while(.not.pm_fast_isnull(p))
       if(arg_is_mvar(p)) then
          slot=cnode_get_num(p,var_index)
          fs%rdata(slot+base)=alloc_var(fs)
          if(cnode_get_num(p,var_name)/=0) &
               num_named=num_named+1
       endif
       p=cnode_get(p,var_link)
    enddo
    ! Process calls
    p=cnode_get(cblock,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       call finalise_call(fs,p,rv,ve,base)
       p=cnode_get(p,call_link)
    enddo
    if(num_named+par>0) then
       ! Info entry for named variables
       fs%wc(fs%last)=0
       fs%wc(fs%last-1)=num_named+par
       fs%wc(fs%last-2)=first_pc
       fs%wc(fs%last-3)=fs%pc-1
       fs%last=fs%last-4-(num_named+par)*2
       if(fs%last<=fs%pc) &
            call pm_panic('Program too complex')
       ! Release named variables
       j=1
       p=cnode_get(cblock,cblock_first_var)
       do while(.not.pm_fast_isnull(p))
          if(arg_is_mvar(p)) then
             slot=cnode_get_num(p,var_index)
             call release_var(fs,fs%rdata(slot+base))
             name=cnode_get_num(p,var_name)
             if(name/=0) then
                fs%wc(fs%last+j*2)=name
                fs%wc(fs%last+j*2-1)=fs%rdata(slot+base)
                j=j+1
             endif
          endif
          p=cnode_get(p,var_link)
       enddo
    endif
  contains
    include 'fisnull.inc'
  end subroutine finalise_cblock

  ! Finalise procedure calls and control structures
  subroutine finalise_call(fs,callnode,rv,ve,base)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: callnode,rv
    integer(pm_i16),intent(in):: ve
    integer,intent(in):: base
    type(pm_ptr):: args,arg,v,tv
    integer:: nargs,nret,n,slot,slot2
    integer:: k
    integer(pm_i16):: i,j,opr,tno,tk,name,name2,new_ve,sig
    integer(pm_ln):: siz
    logical:: varg

    if(pm_debug_level>0) then
       if(cnode_get_kind(callnode)/=cnode_is_call) &
            call pm_panic('Finalise call')
    endif
    args=cnode_get(callnode,call_args)
    nargs=cnode_numargs(args)
    nret=cnode_get_num(callnode,call_nret)
    sig=-cnode_get_num(callnode,call_sig)
    if(pm_debug_level>2) then
       if(sig>0) then 
          write(*,*) 'Final call-->',sym_names(sig)
       else
          write(*,*) 'Final call--> sig=',-sig
       endif
    endif
    select case(sig) 
    case(sym_if)
       if(ve==0) then
          i=wc_jump_call(fs,args,op_jmp_false,0_pm_i16,1)
          call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve,base)
          call finalise_cblock(fs,cnode_arg(args,2),rv,ve,base)
          arg=cnode_arg(args,3)
          if(.not.pm_fast_isnull(arg)) then
             j=wc_jump_call(fs,args,op_jmp,0_pm_i16,0)
             call set_jump_to_here(i)
             call finalise_cblock(fs,arg,rv,ve,base)
             call set_jump_to_here(j)
          else
             call set_jump_to_here(i)
          endif
       else
          new_ve=alloc_var(fs)
          j=wc_jump_call(fs,callnode,op_and_jmp_none,&
               0_pm_i16,3)
          call wc(fs,new_ve)
          call wc(fs,ve)
          call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve,base)
          call finalise_cblock(fs,cnode_arg(args,2),rv,new_ve,base)
          call set_jump_to_here(j)
          arg=cnode_arg(args,3)
          if(.not.pm_fast_isnull(arg)) then
             j=wc_jump_call(fs,callnode,op_andnot_jmp_none,&
                  0_pm_i16,3)
             call wc(fs,new_ve)
             call wc(fs,ve)
             call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve,base)
             call finalise_cblock(fs,arg,rv,new_ve,base)
             call set_jump_to_here(j)
          endif
          call release_var(fs,new_ve)
       endif
    case(sym_while)
       if(ve/=0) then
          new_ve=alloc_var(fs)
          call wc_call(fs,callnode,op_clone,new_ve,1)
          call wc(fs,ve)
       else
          new_ve=0
       endif
       i=wc_jump_call(fs,args,op_jmp,0_pm_i16,0)
       j=fs%pc
       call finalise_cblock(fs,cnode_arg(args,3),rv,new_ve,base)
       call set_jump_to_here(i)
       call finalise_cblock(fs,cnode_arg(args,1),rv,new_ve,base)
       if(ve/=0) then
          call wc_call(fs,args,op_and_jmp_any,&
               j,3)
          call wc(fs,new_ve)
          call wc(fs,ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve,base)
          call release_var(fs,new_ve)
       else
          call wc_call(fs,args,op_jmp_true,j,1)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve,base)
       endif
    case(sym_repeat)
       if(ve==0) then
          i=fs%pc
          call finalise_cblock(fs,cnode_arg(args,1),rv,ve,base)
          call wc_call(fs,callnode,op_jmp_false,&
               i,1)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve,base)
       else
          new_ve=alloc_var(fs)
          call wc_call(fs,callnode,op_clone,new_ve,1)
          call wc(fs,ve)
          i=fs%pc
          call finalise_cblock(fs,cnode_arg(args,1),rv,new_ve,base)
          call wc_call(fs,args,op_andnot_jmp_any,&
               i,2)
          call wc(fs,new_ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve,base)
          call release_var(fs,new_ve)
       endif
    case(sym_loop,sym_find)
       j=wc_jump_call(fs,callnode,&
            op_loop_start+ve_adds(1_pm_i16),0_pm_i16,2+ve_adds(1_pm_i16))
       if(ve/=0) call wc(fs,ve)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve,base)
       call wc(fs,get_var_slot(fs,cnode_arg(args,2),base))
       i=fs%pc
       call finalise_cblock(fs,cnode_arg(args,3),rv,ve,base)
       call wc_call(fs,callnode,&
            op_loop_end+ve_adds(1_pm_i16),i,2+ve_adds(1_pm_i16))
       if(ve/=0) call wc(fs,ve)
       call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve,base)
       call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve,base)
       call set_jump_to_here(j)
    case(sym_do)
       call finalise_cblock(fs,cnode_arg(args,1),rv,ve,base)
    case(sym_par_loop,sym_par_find)
       j=0
       if(sig==sym_par_find) j=j+1
       new_ve=alloc_var(fs)
       call wc_call(fs,args,op_par_loop+ve_adds(1_pm_i16),&
            new_ve,nargs-1-j+ve_adds(1_pm_i16))
       if(ve/=0) call wc(fs,ve)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve,base)
       do k=3+j,nargs
          call wc_arg(fs,cnode_arg(args,k),.false.,rv,ve,base)
       enddo
       j=wc_jump_call(fs,args,op_jmp,0_pm_i16,0)
       call finalise_cblock(fs,cnode_arg(args,2),rv,new_ve,base)
       call wc_call(fs,args,op_par_loop_end,0_pm_i16,0)
       call release_var(fs,new_ve)
       call set_jump_to_here(j)
    case(sym_arrow)
       i=get_var_slot(fs,cnode_arg(args,1),base)
       if(ve==0) then
          call wc_call(fs,callnode,op_setref,i,1)
       else
          call wc_call(fs,callnode,op_setref_vect,i,2)
       endif
       call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve,base)
    case(sym_array)
       j=check_arg_type(args,rv,1)
       if(.not.pm_typ_is_concrete(fs%context,j)) then
          j=-j
       endif
       call wc_call_args(fs,args,op_array+ve_adds(1_pm_i16),j,3,1,rv,ve,base)
    case(sym_any)
       tno=cnode_get_num(args,cnode_args+1)
       call wc_call(fs,args,op_any+ve_adds(1_pm_i16),tno,2)
       if(ve/=0) call wc(fs,ve)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve,base)
       call wc_arg(fs,cnode_arg(args,3),.false.,rv,ve,base)
    case(sym_struct,sym_rec)
       i=fs%pc
       do k=3,nargs
          arg=cnode_arg(args,k)
          if(cnode_get_kind(arg)==cnode_is_const.and.sig==sym_struct&
               .or.arg_is_mvar(arg)) then
             j=alloc_var(fs)
             call wc_call(fs,args,op_clone+ve_adds(1_pm_i16),0_pm_i16,2+ve_adds(1_pm_i16))
             if(ve/=0) call wc(fs,ve)
             call wc(fs,-j)
             call wc_arg(fs,arg,.false.,rv,ve,base)
          endif
       enddo
       j=check_arg_type(args,rv,1)
       if(.not.pm_typ_is_concrete(fs%context,j)) then
          j=-j
       endif
       call wc_call(fs,args,op_struct+ve_adds(1_pm_i16),&
            j,nargs-1+ve_adds(1_pm_i16))
       if(ve/=0) call wc(fs,ve)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve,base)
       do k=3,nargs
          arg=cnode_arg(args,k)
          if(cnode_get_kind(arg)==cnode_is_const.and.sig==sym_struct&
               .or.arg_is_mvar(arg)) then
             call wc(fs,-fs%wc(i+3))
             call release_var(fs,-fs%wc(i+3))
             i=i+5+ve_adds(1_pm_i16)
          else
             call wc_arg(fs,cnode_arg(args,k),.false.,rv,ve,base)
          endif
       enddo
    case(sym_dot,sym_dotref)
       i=rvv(cnode_get_num(callnode,call_index))
       j=ve_adds(1_pm_i16)
       if(i>0) then
          call wc_call(fs,callnode,op_elem+j,i,2+j)
       else 
          v=cnode_arg(cnode_arg(args,3),1)
          i=v%offset
          call wc_call(fs,callnode,op_poly_elem+j,i,2+j)
       endif
       if(ve/=0) call wc(fs,ve)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve,base)
       call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve,base)
    case(sym_set_dot,sym_set_dot_index,sym_set_dot_open_index)
       i=rvv(cnode_get_num(callnode,call_index))
       j=ve_adds(1_pm_i16)
       k=2+j
       if(sig/=sym_set_dot) then
          k=k+1
       else
          j=j+(sig-sym_set_dot)*2
       endif
       if(i>0) then
          call wc_call(fs,callnode,op_set_elem+j,i,k)
       else 
          v=cnode_arg(cnode_arg(args,3),1)
          i=v%offset
          call wc_call(fs,callnode,op_set_poly_elem+j,i,k)
       endif
       if(ve/=0) call wc(fs,ve)
       call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve,base)
       call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve,base)
       if(sig/=sym_set_dot) call wc_arg(fs,cnode_arg(args,3),.false.,rv,ve,base)
    case(sym_import)
       call wc_call(fs,args,op_import,0_pm_i16,nargs)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve,base)
       do k=2,nargs
          call wc_arg(fs,cnode_arg(args,k),.false.,rv,ve,base)
       enddo
    case(sym_export)
       call wc_call(fs,args,op_export,0_pm_i16,nargs)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve,base)
       do k=2,nargs
          call wc_arg(fs,cnode_arg(args,k),.false.,rv,ve,base)
       enddo
    case(sym_check)
       if(nargs==1) then
          call wc_call_args(fs,args,op_check,0_pm_i16,1,0,rv,ve,base)
       else
!!$          call wc_call_args(fs,args,op_check_assign,0_pm_i16,2,1,rv,ve,base)
       endif
    case(sym_open)
       continue ! Nothing to do
    case(sym_result)
       call wc_call_args(fs,args,op_return,0_pm_i16,nargs,0,rv,ve,base)
    case default
       if(sig>0) then
          write(*,*) 'SIG=',sig
          write(*,*) sym_names(sig)
          call pm_panic('Unknown call type in final_call')
       endif
       i=rvv(cnode_get_num(callnode,call_index))
       v=pm_dict_val(fs%context,fs%sig_cache,int(i,pm_ln))
       varg=cnode_flags_set(callnode,call_flags,call_is_vararg)
       tk=cnode_get_kind(v)
       if(tk==cnode_is_single_proc) then
          if(varg) then
             call wc_call_args(fs,args,op_vcall,int(pm_stack_locals+fs%npar-1,pm_i16),&
                  nargs,nret,rv,ve,base)
             call wc_call(fs,args,op_call,add_fn(i),0)
          else
             call wc_call_args(fs,args,op_call,add_fn(i),nargs,nret,rv,ve,base)
          endif
       elseif(tk==cnode_is_multi_proc) then
          do i=1,cnode_numargs(v),2
             call wc(fs,int(cnode_get_num(v,cnode_args+i-1),pm_i16))
             j=cnode_get_num(v,cnode_args+i)
             arg=pm_dict_val(fs%context,fs%sig_cache,int(j,pm_ln))
             if(cnode_get_kind(arg)==cnode_is_single_proc) then
                call wc(fs,op_call)
                call wc(fs,add_fn(j))
             else
                call wc(fs,int(cnode_get_num(arg,bi_opcode),pm_i16))
                call wc(fs,int(cnode_get_num(arg,bi_opcode2),pm_i16))
             endif
          enddo
          siz=(cnode_numargs(v)/2_pm_ln)*3_pm_ln
          fs%temp=pm_new(fs%context,pm_int16,siz)
          fs%temp%data%i16(fs%temp%offset:fs%temp%offset+siz-1)=&
               fs%wc(fs%pc-siz:fs%pc-1)
          fs%pc=fs%pc-siz
          if(varg) then
             call wc_call_args(fs,args,op_vcall,int(pm_stack_locals+fs%npar-1,pm_i16),&
                  nargs,nret,rv,ve,base)
             call wc_call(fs,args,op_poly_call,add_const(fs,fs%temp),0)
          else
             call wc_call_args(fs,args,op_poly_call,add_const(fs,fs%temp),&
                  nargs,nret,rv,ve,base)
          endif
       else
          if(tk/=cnode_is_builtin) then
             write(*,*) 'tk=',tk
             call pm_panic('not builtin')
          endif
          ! Builtin
          j=cnode_get_num(v,bi_opcode)
          if(varg) then
             call wc_call_args(fs,args,op_vcall,int(pm_stack_locals+fs%npar-1,pm_i16),&
                  nargs,nret,rv,ve,base)
             call wc_call(fs,callnode,j+ve_adds(1_pm_i16),&
                  int(cnode_get_num(v,bi_opcode2),pm_i16),0)
          else
             call wc_call_args(fs,args,j+ve_adds(1_pm_i16),&
                  int(cnode_get_num(v,bi_opcode2),pm_i16),&
                  nargs,nret,rv,ve,base)
          endif
       endif
    end select
  contains
    include 'fisnull.inc'

    function rvv(n) result(m)
      integer(pm_p),intent(in):: n
      integer(pm_i16):: m
      m=rv%data%i16(rv%offset+n)
    end function rvv

    function ve_adds(kk) result(jj)
      integer(pm_i16),intent(in):: kk
      integer(pm_i16):: jj
      if(ve==0) then
         jj=0
      else
         jj=kk
      endif
    end function ve_adds

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

    function add_fn(sig) result(n)
      integer(pm_i16),intent(in):: sig
      integer(pm_i16):: n
      integer(pm_i16),dimension(2):: key
      key(1)=sig
      ! This is ve passed to the proc
      if(ve==0) then
         key(2)=0 ! none
      else
         key(2)=pm_stack_locals ! first arg
      endif
      n=pm_ivect_lookup(fs%context,fs%code_cache,key,2)-1_pm_i16
      if(n<0) then
         n=pm_idict_add(fs%context,fs%code_cache,key,2,pm_null_obj)-1_pm_i16
      endif
    end function add_fn

  end subroutine finalise_call
  
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
  end subroutine release_var

  ! Code call to a jump operator
  function wc_jump_call(fs,args,op_s,op_a,nargs) result(pc)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: args
    integer(pm_i16),intent(in):: op_s,op_a
    integer,intent(in):: nargs
    integer:: pc
    pc=fs%pc
    call wc_call(fs,args,op_s,op_a,nargs)   
  end function wc_jump_call

  ! Code call with arguments
  subroutine wc_call_args(fs,args,op,op2,nargs,nret,rv,ve,base)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: args,rv
    integer(pm_i16),intent(in):: op,op2,ve
    integer,intent(in):: nargs,nret,base
    integer:: i,start
    integer(pm_i16):: slot
    type(pm_ptr):: arg
    if(ve==0) then
       start=0
    else
       start=1
    endif
    call wc_call(fs,args,op,op2,nargs+start)
    if(ve/=0) call wc(fs,ve)
    do i=1+nret,nargs
       arg=cnode_arg(args,i)
       if(arg_is_svar(arg)) then
          slot=get_var_slot(fs,arg,base)
          call release_var(fs,slot)
       endif
    enddo
    do i=1,nargs
       arg=cnode_arg(args,i)
       if(i>nret.and.arg_is_svar(arg)) then
          call wc(fs,get_var_slot(fs,arg,base))
       else
          call wc_arg(fs,arg,i<=nret,rv,ve,base)
       endif
    enddo
  end subroutine wc_call_args

  ! Just code call -- arguments must be coded
  ! seperately
  subroutine wc_call(fs,node,op,op2,nargs)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: node
    integer(pm_i16),intent(in):: op,op2
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
    call wc(fs,op)
    call wc(fs,op2)
    call wc(fs,int(nargs,pm_i16))
  end subroutine wc_call

  ! Code one argument of call/operation
  subroutine wc_arg(fs,argnode,isret,rv,ve,base)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: argnode
    logical,intent(in):: isret
    type(pm_ptr),intent(in):: rv
    integer(pm_i16),intent(in):: ve
    integer,intent(in):: base
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
          fs%rdata(slot+base)=k
       else
          ! Get temp var
          slot=cnode_get_num(arg,var_index)
          k=fs%rdata(slot)
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
            -32_pm_i16*(1_pm_i16+&
            add_const(fs,cnode_arg(arg,1))+int(pm_max_stack,pm_i16)))
    endif
  contains
    include 'fisnull.inc'

    function rvv(n) result(m)
      integer(pm_p),intent(in):: n
      integer(pm_i16):: m
      m=rv%data%i16(rv%offset+n)
    end function rvv


  end subroutine wc_arg

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
  end function add_const

  ! Get slot associated with variable
  function get_var_slot(fs,arg,base) result(slot)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: arg
    integer,intent(in):: base
    integer(pm_i16):: slot
    integer:: i
    i=cnode_get_num(arg,var_index)
    slot=fs%rdata(base+i)
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
            ior(var_multi_access,var_multi_change))) then
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
            ior(ior(var_multi_access,var_multi_change),var_param))) then
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
          ! Change to relative displacement
          fs%wc(i+1)=code2-(i+3+nargs)
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
!!$       do i=code%offset,code%offset+pm_fast_esize(code)
!!$          write(iunit,*) 'CODE>',i,code%data%i16(i)
!!$       enddo
       write(iunit,*) idx,&
            trim(pm_name_as_string(context,int(code%data%i16(code%offset+2),pm_p))),&
            ' (=='
       write(iunit,*) 'STACKSIZE=',&
            code%data%i16(code%offset),'NARGS=',code%data%i16(code%offset+1)
       i=code%offset+3
       do
          k=code%data%i16(i)
          j=code%data%i16(i+1)
          n=iand(code%data%i16(i+2),127)
          call proc_line_module(p,i-int(code%offset)+1,line,modl)
          str='at: '
          call pm_name_string(context,modl,str(5:))
          if(k==op_call) then
             q=pm_dict_val(context,context%funcs,j+1_pm_ln)
             q=q%data%ptr(q%offset)
             str2='('
             call pm_name_string(context,int(q%data%i16(q%offset+2),pm_p),str2(2:))
             str2(len_trim(str2)+1:)=')'
             write(iunit,'(i4,1x,a20,i4,a20,i4,1x,a15,i4)') i-code%offset+1,&
                  op_names(k),j,str2,n,str,line
          elseif(k>=first_assign_op.and.k<=last_assign_op) then
             str2='('
             call pm_name_string(context,proc_slot_name(p,i,j),str2(2:))
             str2(len_trim(str2)+1:)=')'
             write(iunit,'(i4,1x,a20,i4,a20,i4,1x,a15,i4)') i-code%offset+1,&
                  op_names(k),j,str2,n,str,line
          else if(k>=first_jmp_op.and.k<=last_jmp_op) then
             write(iunit,'(i4,1x,a20,i4,1a,i4,1a,14x,i4,1x,a15,i4)') i-code%offset+1,&
                  op_names(k),j,'(',i+j+3+n,')',n,str,line
          else if(k>=0.and.k<=num_op) then
             write(iunit,'(i4,1x,a20,i4,20x,i4,1x,a15,i4)') i-code%offset+1,&
                  op_names(k),j,n,str,line
          else
             write(iunit,'(i4,1x,1a,i4,1a,14x,i4,20x,i4,1x,a15,i4)') &
                  i-code%offset+1,'?',k,'?',j,n,str,line
          endif
          do j=1,n
             k=code%data%i16(i+j+2)
             if(k>0) then
                call pm_name_string(context,proc_slot_name(p,i,int(k)),str)
                write(iunit,*) '      Stack:',k,trim(str)
             else if(k>=-pm_max_stack) then
                write(iunit,*) '      Stack ref:',k
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



end module pm_backend
