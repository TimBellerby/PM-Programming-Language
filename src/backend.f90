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
     integer:: nval,pc,last,npar,stacktop
     logical,dimension(pm_max_stack,pm_pointer):: in_use
     integer,dimension(pm_pointer):: nvar,avar
     integer(pm_i16),dimension(max_code_stack):: rdata
     integer(pm_i16),dimension(max_code_size):: wc
     integer(pm_i16):: cur_modl,cur_line
  end type finaliser

contains

  ! Initialise finalise stage control structure
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
    call init_final_proc(fs,p)
    cblock=cnode_arg(p,1)
    rv=cnode_arg(p,2)
    ve=0
    call finalise_cblock(fs,cblock,rv,ve)
    call make_proc_code(fs,1_pm_ln)
  contains
    include 'fesize.inc'
  end subroutine finalise_prog

  ! Finise procedure definitions
  subroutine finalise_procs(fs)
    type(finaliser),intent(inout):: fs
    type(pm_ptr):: prc,rv,cblock,p,tv
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
       fs%npar=cnode_get_num(prc,pr_nret)
       cblock=cnode_arg(prc,1)
       cblock=cnode_get(cblock,pr_cblock)
       call finalise_cblock(fs,cblock,rv,ve)
       call make_proc_code(fs,i)
       i=i+1
    end do
  contains
    include 'fesize.inc'
  end subroutine  finalise_procs

  subroutine init_final_proc(fs,prc)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: prc
    fs%pc=1
    fs%last=max_code_size
    fs%nval=0
    fs%nvar=0
    fs%avar=0
    fs%npar=0
    fs%stacktop=0
    fs%rdata(1:pm_fast_esize(cnode_arg(prc,2))+1)=-1
  contains
    include 'fesize.inc'
  end subroutine init_final_proc
  
  subroutine make_proc_code(fs,i)
    type(finaliser),intent(inout):: fs
    integer(pm_ln),intent(in):: i
    integer:: n,m,nalloc,vs,j,k
    type(pm_ptr):: p,p2
    call wc(fs,op_return)
    call wc(fs,0_pm_i16)
    call wc(fs,0_pm_i16)
    call tidy_up(fs,nalloc)
    n=fs%nval
    m=max_code_size-fs%last
    fs%temp=pm_fast_newnc(fs%context,pm_pointer,int(n+2,pm_p))
    call pm_ptr_assign(fs%context,&
         pm_dict_vals(fs%context,fs%code_cache),i-1,fs%temp)
    call pm_assign_new(fs%context,fs%temp,&
         0_pm_ln,pm_int16,int(fs%pc-1+nalloc,pm_ln),.false.)
    call pm_assign_new(fs%context,fs%temp,&
         1_pm_ln,pm_int16,int(m,pm_ln),.false.)
    p=fs%temp
    p2=p%data%ptr(p%offset)
    j=p2%offset+1
    vs=pm_stack_locals+fs%npar
    do k=pm_int,pm_pointer
       if(fs%nvar(k)>0) then
          p2%data%i16(j)=op_alloc_int+k-pm_int
          p2%data%i16(j+1)=vs*(pm_max_stack+1)+fs%nvar(k)
          p2%data%i16(j+2)=128*(vs-1)
          vs=vs+fs%nvar(k)
          j=j+3
       endif
    enddo
    p2%data%i16(p2%offset)=vs ! Required stack size
    p2%data%i16(p2%offset+nalloc:p2%offset+fs%pc-2+nalloc)=fs%wc(1:fs%pc-1)
    p2=p%data%ptr(p%offset+1_pm_p)
    p2%data%i16(p2%offset:p2%offset+m-1)=fs%wc(fs%last+1:max_code_size)
    if(n>0) then
       p%data%ptr(p%offset+2:p%offset+n+1)=&
            fs%values(1:n)
    endif
  contains
    include 'fnewnc.inc'
  end subroutine make_proc_code

  subroutine finalise_cblock(fs,cblock,rv,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: cblock,rv
    integer(pm_i16),intent(in):: ve
    type(pm_ptr):: p
    integer:: slot,par,num_named,first_pc,j
    integer(pm_i16):: name
    integer(pm_p):: tk
  character(len=100):: str
    first_pc=fs%pc
    ! Allocate parameter variables
    par=fs%npar
    p=cnode_get(cblock,cblock_first_var)
    if(.not.pm_fast_isnull(p)) then
       do while(iand(cnode_get_num(p,var_flags),var_param)/=0)
          slot=cnode_get_num(p,var_index)
          fs%rdata(slot)=par+pm_stack_locals
          par=par+1
          p=cnode_get(p,var_link)
          if(pm_fast_isnull(p)) exit
       enddo
    endif
    fs%npar=par
    ! Allocate named variables
    num_named=0
    do while(.not.pm_fast_isnull(p))
       if(cnode_get_num(p,var_name)/=0) then
          if(iand(cnode_get_num(p,var_flags),var_iter)==0) then
             slot=cnode_get_num(p,var_index)
             if(ve==0) then
                tk=min(rv%data%i16(rv%offset+slot),pm_pointer)
             else
                tk=pm_pointer
             endif
             call pm_name_string(fs%context,cnode_get_num(p,var_name),str)
             fs%rdata(slot)=alloc_var(fs,&
                  tk)
          endif
          num_named=num_named+1
       endif
       p=cnode_get(p,var_link)
    enddo
    ! Process calls
    p=cnode_get(cblock,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       call finalise_call(fs,p,rv,ve)
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
          name=cnode_get_num(p,var_name)
          if(name/=0) then
             slot=cnode_get_num(p,var_index)
             fs%wc(fs%last+j*2)=name
             fs%wc(fs%last+j*2-1)=fs%rdata(slot)
             j=j+1
             if(iand(cnode_get_num(p,var_flags),var_iter)==0) &
                  call release_var(fs,fs%rdata(slot))
          endif
          p=cnode_get(p,var_link)
       enddo
    endif
  contains
    include 'fisnull.inc'
  end subroutine finalise_cblock

  ! Finalise procedure calls and control structures
  subroutine finalise_call(fs,callnode,rv,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: callnode,rv
    integer(pm_i16),intent(in):: ve
    type(pm_ptr):: args,arg,v,tv
    integer:: nargs,nret,n,slot,slot2
    integer:: k
    integer(pm_i16):: i,j,opr,tno,tk,name,name2,new_ve,sig
    integer(pm_ln):: siz
    if(pm_debug_level>0) then
       if(cnode_get_kind(callnode)/=cnode_is_call) &
            call pm_panic('Finalise call')
    endif
    args=cnode_get(callnode,call_args)
    nargs=cnode_numargs(args)
    nret=cnode_get_num(callnode,call_nret)
    sig=-cnode_get_num(callnode,call_sig)
    select case(sig) 
    case(sym_if)
       if(ve==0) then
          i=wc_jump_call(fs,args,op_jmp_false,0_pm_i16,1)
          call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve)
          call finalise_cblock(fs,cnode_arg(args,2),rv,ve)
          arg=cnode_arg(args,3)
          if(.not.pm_fast_isnull(arg)) then
             j=wc_jump_call(fs,args,op_jmp,0_pm_i16,0)
             call set_jump_to_here(i)
             call finalise_cblock(fs,arg,rv,ve)
             call set_jump_to_here(j)
          else
             call set_jump_to_here(i)
          endif
       else
          new_ve=alloc_var(fs,pm_pointer)
          j=wc_jump_call(fs,callnode,op_and_jmp_none,&
               0_pm_i16,3)
          call wc(fs,new_ve)
          call wc(fs,ve)
          call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve)
          call finalise_cblock(fs,cnode_arg(args,2),rv,new_ve)
          call set_jump_to_here(j)
          arg=cnode_arg(args,3)
          if(.not.pm_fast_isnull(arg)) then
             j=wc_jump_call(fs,callnode,op_andnot_jmp_none,&
                  0_pm_i16,3)
             call wc(fs,new_ve)
             call wc(fs,ve)
             call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve)
             call finalise_cblock(fs,arg,rv,new_ve)
             call set_jump_to_here(j)
          endif
          call release_var(fs,new_ve)
       endif
    case(sym_while)
       if(ve/=0) then
          new_ve=alloc_var(fs,pm_pointer)
          call wc_call(fs,callnode,op_clone,new_ve,1)
          call wc(fs,ve)
       else
          new_ve=0
       endif
       i=wc_jump_call(fs,args,op_jmp,0_pm_i16,0)
       j=fs%pc
       call finalise_cblock(fs,cnode_arg(args,3),rv,new_ve)
       call set_jump_to_here(i)
       call finalise_cblock(fs,cnode_arg(args,1),rv,new_ve)
       if(ve/=0) then
          call wc_call(fs,args,op_and_jmp_any,&
               j,3)
          call wc(fs,new_ve)
          call wc(fs,ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
          call release_var(fs,new_ve)
       else
          call wc_call(fs,args,op_jmp_true,j,1)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
       endif
    case(sym_repeat)
       if(ve==0) then
          i=fs%pc
          call finalise_cblock(fs,cnode_arg(args,1),rv,ve)
          call wc_call(fs,callnode,op_jmp_false,&
               i,1)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
       else
          new_ve=alloc_var(fs,pm_pointer)
          call wc_call(fs,callnode,op_clone,new_ve,1)
          call wc(fs,ve)
          i=fs%pc
          call finalise_cblock(fs,cnode_arg(args,1),rv,new_ve)
          call wc_call(fs,args,op_andnot_jmp_any,&
               i,2)
          call wc(fs,new_ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
          call release_var(fs,new_ve)
       endif
    case(sym_loop,sym_find)
       j=wc_jump_call(fs,callnode,op_loop_start+ve_adds(1_pm_i16),0_pm_i16,2+ve_adds(1_pm_i16))
       if(ve/=0) call wc(fs,ve)
       call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve)
       call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
       i=fs%pc
       call finalise_cblock(fs,cnode_arg(args,3),rv,ve)
       call wc_call(fs,callnode,op_loop_end+ve_adds(1_pm_i16),i,2+ve_adds(1_pm_i16))
       if(ve/=0) call wc(fs,ve)
       call wc_arg(fs,cnode_arg(args,1),.false.,rv,ve)
       call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
       call set_jump_to_here(j)
    case(sym_do)
       call finalise_cblock(fs,cnode_arg(args,1),rv,ve)
    case(sym_par_loop,sym_par_find)
       j=0
       if(sig==sym_par_find) j=j+1
       new_ve=alloc_contig_vars(fs,pm_pointer,nargs+1)
       call wc_call(fs,args,op_par_loop+ve_adds(1_pm_i16),new_ve,nargs-1-j+ve_adds(1_pm_i16))
       if(ve/=0) call wc(fs,ve)
       do k=2+j,nargs
          call wc_arg(fs,cnode_arg(args,k),k<=nret,rv,ve)
       enddo
       j=wc_jump_call(fs,args,op_jmp,0_pm_i16,0)
       call finalise_cblock(fs,cnode_arg(args,nret+1),rv,new_ve)
       call wc_call(fs,args,op_par_loop_end,0_pm_i16,0)
       call release_contig_vars(fs,pm_pointer,nargs+1,new_ve)
       call set_jump_to_here(j)
    case(sym_for)
       do k=1,nret
          arg=cnode_arg(args,k)
          slot=cnode_get_num(arg,var_index)
          fs%rdata(slot)=ve+int(k,pm_i16)*32
       enddo
    case(sym_define)
       arg=cnode_arg(args,2)
       if(.not.arg_is_tempvar(arg)) then
          tno=check_arg_type(args,rv,1)
          if(tno<pm_pointer) then
             i=get_var_slot(fs,cnode_arg(args,1))
             if(ve==0) then
                call wc_call(fs,callnode,op_assign_int+tno-int(pm_int,pm_i16),&
                     i,1)
             else
                call wc_call(fs,callnode,op_assign_int_vect+tno-int(pm_int,pm_i16),&
                     i,2)
                call wc(fs,ve)
             endif
             call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
          else
             i=get_var_slot(fs,cnode_get(args,1))
             if(ve==0) then
                call wc_call(fs,callnode,op_clone,i,1)
             else
                call wc_call(fs,callnode,op_clone_vect,i,1)
                call wc(fs,ve)
             endif
             call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
          endif
       endif
    case(sym_arrow)
       i=get_var_slot(fs,cnode_get(args,1))
       if(ve==0) then
          call wc_call(fs,callnode,op_setref,i,1)
       else
          call wc_call(fs,callnode,op_setref_vect,i,2)
       endif
       call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
    case(sym_struct)
       call wc_call_args(fs,args,op_struct+ve_adds(1_pm_i16),&
            check_arg_type(args,rv,1),nargs,nret,rv,ve)
    case(sym_dot,sym_dotref)
       i=rv%data%i16(rv%offset+cnode_get_num(callnode,call_index))
       j=ve_adds(1_pm_i16)
       if(i>0) then
          call wc_call(fs,callnode,op_struct_elem+j,i,2+j)
          if(ve/=0) call wc(fs,ve)
          call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
          call wc_arg(fs,cnode_arg(args,2),.false.,rv,ve)
       else
          !!!
       endif
    case(sym_import)
       call wc_call(fs,args,op_import,0_pm_i16,nargs)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
       do k=2,nargs
          call wc_arg(fs,cnode_arg(args,k),.false.,rv,ve)
       enddo
    case(sym_export)
       call wc_call(fs,args,op_export,0_pm_i16,nargs)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
       do k=2,nargs
          call wc_arg(fs,cnode_arg(args,k),.false.,rv,ve)
       enddo
    case(sym_array)
       call wc_call_args(fs,args,op_array+ve_adds(1_pm_i16),0_pm_i16,3,1,rv,ve)
    case(sym_any)
       tno=cnode_get_num(args,cnode_args+1)
       call wc_call(fs,args,op_any+ve_adds(1_pm_i16),tno,2)
       if(ve/=0) call wc(fs,ve)
       call wc_arg(fs,cnode_arg(args,1),.true.,rv,ve)
       call wc_arg(fs,cnode_arg(args,3),.false.,rv,ve)
    case(sym_check)
       if(nargs==1) then
          call wc_call_args(fs,args,op_check,0_pm_i16,1,0,rv,ve)
       else
!!$          call wc_call_args(fs,args,op_check_assign,0_pm_i16,2,1,rv,ve)
       endif
    case(sym_open)
       continue ! Nothing to do
    case(sym_result)
       do k=1,nargs
          i=k+pm_stack_locals-1
          arg=cnode_arg(args,k)
          if(arg_is_tempvar(arg)) then
             slot=fs%rdata(cnode_get_num(arg,var_index))
             fs%wc(slot)=i
          else
             if(tno<pm_pointer) then
                if(ve==0) then
                   call wc_call(fs,callnode,op_assign_int+tno-int(pm_int,pm_i16),&
                        i,1)
                else
                   call wc_call(fs,callnode,op_assign_int_vect+tno-int(pm_int,pm_i16),&
                        i,2)
                   call wc(fs,ve)
                endif
                call wc_arg(fs,arg,.false.,rv,ve)
             else
                if(ve==0) then
                   call wc_call(fs,callnode,op_clone,i,1)
                else
                   call wc_call(fs,callnode,op_clone_vect,i,1)
                   call wc(fs,ve)
                endif
                call wc_arg(fs,arg,.false.,rv,ve)
             endif
          endif
       enddo
    case default
       i=rv%data%i16(rv%offset+cnode_get_num(callnode,call_index))
       v=pm_dict_val(fs%context,fs%sig_cache,int(i,pm_ln))
       tk=cnode_get_kind(v)
       if(tk==cnode_is_single_proc) then
          call wc_call_args(fs,args,op_call,add_fn(i),nargs,nret,rv,ve)
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
          siz=cnode_numargs(v)*3_pm_ln
          fs%temp=pm_new(fs%context,pm_int16,siz)
          fs%temp%data%i16(fs%temp%offset:fs%temp%offset+siz-1)=&
               fs%wc(fs%pc-siz:fs%pc-1)
          fs%pc=fs%pc-siz
          call wc_call_args(fs,args,op_poly_call,add_const(fs,fs%temp),&
               nargs,nret,rv,ve)
       else
          if(tk/=cnode_is_builtin) then
             write(*,*) 'tk=',tk
             call pm_panic('not builtin')
          endif
          ! Builtin
          j=cnode_get_num(v,bi_opcode)
          select case(j)
          case(op_assign:op_assign_vect)
             arg=cnode_arg(args,2)
             slot=get_var_slot(fs,cnode_arg(args,1))
             if(arg_is_tempvar(arg)) then
                slot2=get_var_slot(fs,arg)
                if(slot==slot2) return ! Already optimised out
             endif
             if(ve==0) then
                call wc_call(fs,callnode,j,int(slot,pm_i16),1)
             else
                call wc_call(fs,callnode,j+(op_assign_vect-op_assign),int(slot,pm_i16),2)
                call wc(fs,ve)
             endif
             call wc_arg(fs,arg,.false.,rv,ve)
          case default
             call wc_call_args(fs,args,j+ve_adds(1_pm_i16),&
                  int(cnode_get_num(v,bi_opcode2),pm_i16),&
                  nargs,nret,rv,ve)
          end select
       endif
    end select
  contains
    include 'fisnull.inc'

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
      n=pm_idict_add(fs%context,fs%code_cache,key,2,pm_null_obj)-1_pm_i16
    end function add_fn
    
  end subroutine finalise_call
  
  ! Allocate variable yielding intermediate index
  ! that will need post-processing by tidy-up
  function alloc_var(fs,vkind) result(k)
    type(finaliser),intent(inout):: fs
    integer(pm_p),intent(in):: vkind
    integer:: i
    integer(pm_i16)::k
    if(pm_debug_level>0) then
       if(vkind<pm_int.or.vkind>pm_pointer) &
            call pm_panic('allocating unresolved var')
    endif
    if(fs%nvar(vkind)==fs%avar(vkind)) then
       fs%nvar(vkind)=fs%nvar(vkind)+1
       if(vkind==pm_pointer) fs%stacktop=fs%nvar(vkind)
       fs%avar(vkind)=fs%nvar(vkind)
       fs%in_use(fs%nvar(vkind),vkind)=.true.
       k=fs%nvar(vkind)*32+vkind
    else
       fs%avar(vkind)=fs%avar(vkind)+1
       do i=1,fs%nvar(vkind)
          if(.not.fs%in_use(i,vkind)) then
             fs%in_use(i,vkind)=.true.
             k=i*32+vkind
             if(vkind==pm_pointer.and.i>fs%stacktop)&
                  fs%stacktop=i
             return
          endif
       enddo
    endif
  end function alloc_var
  
  ! Release variable
  subroutine release_var(fs,k)
    type(finaliser),intent(inout):: fs
    integer(pm_i16):: k
    integer(pm_i16):: vk,n
    if(k<=fs%npar+pm_stack_locals) return
    vk=iand(k,31)
    n=k/32
    if(n==0) call pm_panic('jri')
    if(.not.fs%in_use(n,vk)) return
    fs%in_use(n,vk)=.false.
    fs%avar(vk)=fs%avar(vk)-1
    if(n==fs%stacktop.and.vk>=pm_pointer) &
         fs%stacktop=fs%stacktop-1
  end subroutine release_var

  ! Allocate contiguous block of variables of given type
  function alloc_contig_vars(fs,vk,n) result(slot)
    type(finaliser),intent(inout):: fs
    integer(pm_p),intent(in):: vk
    integer,intent(in):: n
    integer(pm_i16):: slot
    integer:: i
    slot=fs%nvar(vk)+1
    fs%nvar(vk)=fs%nvar(vk)+n
    fs%avar(vk)=fs%avar(vk)+n
    do i=slot,slot+n-1
       fs%in_use(i,vk)=.true.
    enddo
    slot=slot*32+vk
  end function alloc_contig_vars
  
  ! Release contiguous block of variables of given type
  subroutine release_contig_vars(fs,vk,n,slot)
    type(finaliser),intent(inout):: fs
    integer(pm_p),intent(in):: vk
    integer,intent(in):: n
    integer(pm_i16),intent(in):: slot
    integer(pm_i16):: i
    do i=slot+(n-1)*32,slot,-32
       call release_var(fs,i)
    enddo
  end subroutine release_contig_vars

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
  subroutine wc_call_args(fs,args,op,op2,nargs,nret,rv,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: args,rv
    integer(pm_i16),intent(in):: op,op2,ve
    integer,intent(in):: nargs,nret
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
       if(arg_is_tempvar(arg)) then
          slot=get_var_slot(fs,arg)
          call release_var(fs,slot)
       endif
    enddo
    do i=1,nargs
       call wc_arg(fs,cnode_arg(args,i),i<=nret,rv,ve)
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
    call wc(fs,int(nargs+128*fs%stacktop,pm_i16))
  end subroutine wc_call

  ! Code one argument of call/operation
  subroutine wc_arg(fs,argnode,isret,rv,ve)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: argnode
    logical,intent(in):: isret
    type(pm_ptr),intent(in):: rv
    integer(pm_i16),intent(in):: ve
    integer(pm_i16):: k,t,akind
    integer:: slot
    type(pm_ptr):: arg,ass
    arg=argnode
    akind=cnode_get_kind(arg)
    if(akind==cnode_is_var) then
       if(cnode_get_num(arg,var_name)==0) then
          ! Temporary variable
          if(isret) then
             ! Optimise out assignments/definitions/returns
             ass=cnode_get(arg,var_assign_call)
             if(.not.pm_fast_isnull(ass)) then
                k=cnode_get_num(ass,call_sig)
                if(k==-sym_result) then
                   call wc(fs,0_pm_i16)
                   fs%rdata(cnode_get_num(arg,var_index))=fs%pc-1
                   return
                endif
                ass=cnode_get(ass,call_args)
                ass=cnode_arg(ass,1)
                if(k==-sym_define) then
                   slot=cnode_get_num(ass,var_index)
                   call wc(fs,fs%rdata(slot))
                   return
                endif
                if(.not.arg_is_tempvar(ass)) then
                   k=rv%data%i16(rv%offset+cnode_get_num(arg,var_index))
                   t=rv%data%i16(rv%offset+cnode_get_num(ass,var_index))
                   if(k==t) then
                      if(pm_typ_is_concrete(fs%context,t)) then
                         slot=cnode_get_num(ass,var_index)
                         call wc(fs,fs%rdata(slot))
                         fs%rdata(cnode_get_num(arg,var_index))=fs%pc-1
                         return
                      endif
                   endif
                endif
             endif
             ! Make temp var
             if(ve/=0) then
                t=pm_pointer
                k=alloc_var(fs,pm_pointer)
             else
                t=rv%data%i16(rv%offset+cnode_get_num(arg,var_index))
                k=alloc_var(fs,min(t,pm_pointer))
             endif
             if(t>=pm_pointer) k=-k ! Allocate temp ptr
             call wc(fs,k) 
             slot=cnode_get_num(arg,var_index)
             fs%rdata(slot)=fs%pc-1
          else
             ! Get temp var
             slot=cnode_get_num(arg,var_index)
             k=abs(fs%wc(fs%rdata(slot)))
             call wc(fs,k)
             call release_var(fs,k)
          endif
       else
          ! Named variable
          slot=cnode_get_num(arg,var_index)
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
  end subroutine wc_arg

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
  function get_var_slot(fs,arg) result(slot)
    type(finaliser),intent(inout):: fs
    type(pm_ptr),intent(in):: arg
    integer(pm_i16):: slot
    type(pm_ptr):: var
    integer:: i,name
    var=arg
    name=cnode_get_num(var,var_name)
    i=cnode_get_num(var,var_index)
    if(name==0) then
       slot=abs(fs%wc(fs%rdata(i)))
    else
       slot=fs%rdata(i)
    endif
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

  ! Is argument a temporary variable?
  function arg_is_tempvar(arg) result(ok)
    type(pm_ptr),intent(in):: arg
    logical:: ok
    ok=.false.
    if(cnode_get_kind(arg)==cnode_is_var) then
       if(cnode_get_num(arg,var_name)==0) then
          ok=.true.
       endif
    endif
  end function arg_is_tempvar

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
  ! - post process variable indiced
  ! - follow loop chains
  ! - correct stack information in calls
  ! - correct stack information in info vect
  subroutine tidy_up(fs,nalloc)
    type(finaliser),intent(inout):: fs
    integer,intent(out):: nalloc
    integer:: i,j,k,n,nargs,stacktop,last_slot,start
    integer(pm_i16)::code,code2,arg
    integer(pm_i16),dimension(pm_pointer):: vstart
    call set_op_names
    k=fs%npar+pm_stack_locals-1
    do i=1,pm_pointer
       vstart(i)=k
       k=k+fs%nvar(i)
    enddo
    last_slot=k-1
    nalloc=1
    do k=pm_int,pm_pointer
       if(fs%nvar(k)>0) then
          nalloc=nalloc+3
       endif
    enddo
    start=nalloc
    i=1
    do
       code=fs%wc(i)
       if(pm_debug_level>2) then
          write(*,*) 'TIDY',op_names(code)
       endif
       code2=fs%wc(i+1)
       nargs=iand(fs%wc(i+2),127_pm_i16)
       stacktop=fs%wc(i+2)/128
       ! Correct second argument to assign
       if(code>=first_assign_op.and.code<=last_assign_op)&
            fs%wc(i+1)=fix_arg(code2)
       ! Correct stack top information
       if(stacktop==0) then
          fs%wc(i+2)=nargs+128*last_slot
       else
          fs%wc(i+2)=nargs+128*(stacktop+vstart(pm_pointer))
       endif
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
          if(arg>0) then
             fs%wc(j)=fix_arg(arg)
          else if(arg>=-pm_max_args*32) then
             fs%wc(j)=-fix_arg(-arg)
          else
             fs%wc(j)=arg/32_pm_i16
          endif
       enddo
       i=i+nargs+3
       if(i>fs%pc) exit
    enddo
    ! Fix slot information in info vector
    j=max_code_size
    do 
       k=fs%wc(j)
       if(k/=0) then 
          j=j-2
       else
          n=fs%wc(j-1)
          fs%wc(j-2)=fs%wc(j-2)+start
          fs%wc(j-3)=fs%wc(j-3)+start
          j=j-n*2-4
          do i=1,n
             fs%wc(j+i*2-1)=fix_arg(fs%wc(j+i*2-1))
          enddo
       endif
       if(j<fs%last) exit
    enddo
  contains
    function fix_arg(n) result(m)
      integer(pm_i16),intent(in):: n
      integer(pm_i16):: m
      integer(pm_i16):: k
      if(n<=fs%npar+pm_stack_locals) then
         m=n
         if(pm_debug_level>2) then
            write(*,*) 'FIX',n,'--->',m
         endif
      else
         k=iand(n,31_pm_i16)
         m=n/32+vstart(k)
         if(pm_debug_level>2) then
            write(*,*) 'FIX',n,k,n/32,fs%nvar(k),'--->',m
         endif
      endif
    end function fix_arg
  end subroutine tidy_up

  subroutine dump_wc(context,iunit)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    integer(pm_ln):: idx
    integer:: i,j,n
    type(pm_ptr):: p,code,lines
    integer(pm_i16):: k
    integer(pm_p):: line,modl
    character(len=100):: str,str2
    character(len=20):: ostr,mstr
    call set_op_names
    do idx=1_pm_ln,pm_dict_size(context,context%funcs)
       write(iunit,*) idx,'(=='
       p=pm_dict_val(context,context%funcs,idx)
       code=p%data%ptr(p%offset+1)
       do i=code%offset,code%offset+pm_fast_esize(code)
          write(iunit,*) 'INFO>',i,code%data%i16(i)
       enddo
       code=p%data%ptr(p%offset)
       do i=code%offset,code%offset+pm_fast_esize(code)
          write(iunit,*) 'CODE>',i,code%data%i16(i)
       enddo
       i=code%offset+1
       do
          k=code%data%i16(i)
          j=code%data%i16(i+1)
          n=iand(code%data%i16(i+2),127)
          call proc_line_module(p,i-int(code%offset)+1,line,modl)
          str='at: '
          call pm_name_string(context,modl,str(5:))
          if(k>=first_assign_op.and.k<=last_assign_op) then
             str2='('
             call pm_name_string(context,proc_slot_name(p,i,j),str2(2:))
             str2(len_trim(str2)+1:)=')'
             write(iunit,'(i4,1x,a20,i4,a20,i4,1x,a15,i4)') i-code%offset+1,&
                  op_names(k),j,str2,n,str,line
          else if(k>=first_alloc_op.and.k<=last_alloc_op) then
             write(iunit,'(i4,1x,a20,i4,1a,i4,i4,1a,10x,i4,1x,a15,i4)') i-code%offset+1,&
                  op_names(k),j,'(',iand(j,pm_max_stack),j/(pm_max_stack+1),')',n,str,line
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
    include 'fesize.inc'
  end subroutine dump_wc

  subroutine proc_line_module(prc,offset,line,modl)
    type(pm_ptr):: prc
    integer:: offset
    integer(pm_p),intent(out):: line,modl
    integer:: j
    integer(pm_i16):: k
    type(pm_ptr):: p
    p=prc%data%ptr(prc%offset+1)
    j=p%offset+pm_fast_esize(p)
    do
       k=p%data%i16(j)
       if(k==0) then
          j=j-p%data%i16(j-1)*2-4
       else
          if(p%data%i16(j-1)>offset) return
          if(k>0) then
             modl=k
          else 
             line=-k
          endif
          j=j-2
       endif
       if(j<p%offset) exit
    enddo
  contains
    include 'fesize.inc'
  end subroutine  proc_line_module

  function proc_slot_name(prc,offset,slot) result(name)
    type(pm_ptr):: prc
    integer:: offset,slot
    integer(pm_p):: name
    integer:: i,j,start,finish,n
    type(pm_ptr):: p
    integer(pm_i16):: k
    name=0
    p=prc%data%ptr(prc%offset+1)
    j=p%offset+pm_fast_esize(p)
    do
       k=p%data%i16(j)
       if(k/=0) then
          j=j-2
       else
          n=p%data%i16(j-1)
          start=p%data%i16(j-2)
          finish=p%data%i16(j-3)
          j=j-n*2-4
          if(offset>=start.and.offset<=finish) then
             do i=1,n
                if(p%data%i16(j+i*2-1)==slot) then
                   name=p%data%i16(j+i*2)
                   exit
                endif
             enddo
          endif
       endif
       if(j<p%offset) exit
    enddo
  contains
      include 'fesize.inc'
  end function proc_slot_name

end module pm_backend
