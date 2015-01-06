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
module pm_vm
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_types
  use pm_sysdefs
  implicit none

  integer,parameter:: shrink_thresh=4
contains

  recursive function pm_run(context,funcin,stackin,pcin,&
       op,op2,args,num_args,sptr,svec) result(errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: funcin,stackin,pcin
    integer(pm_i16),intent(in):: op,op2
    integer,intent(in):: num_args
    type(pm_ptr),dimension(num_args),intent(in):: args
    type(pm_ptr),intent(out):: sptr,svec
    integer:: errno
    type(pm_ptr),target,dimension(pm_max_args):: arg
    integer,target:: nargs
    type(pm_ptr),target:: stack
    type(pm_ptr):: func,pc,newstack,newfunc,newpc,ve,v
    integer(pm_i16):: opcode,opcode2,opcode3,oparg
    integer:: i,n
    integer(pm_ln):: esize,j,jj,k
    logical:: convert_out,flip,ok
    integer(pm_p):: stacksize
    type(pm_reg),pointer:: reg
    character(len=200):: emess
    nargs=1
    reg=>pm_register(context,'vm args',&
         stack,array=arg,array_size=nargs)
    opcode=op
    opcode2=op2
    if(num_args>0) arg(1:num_args)=args(1:num_args)
    nargs=num_args
    func=funcin
    stack=stackin
    pc=pcin
    goto 20

    10 continue
    
    nargs=0
    opcode=pc%data%i16(pc%offset)
    opcode2=pc%data%i16(pc%offset+1_pm_p)
    opcode3=pc%data%i16(pc%offset+2_pm_p)
    n=iand(opcode3,127) ! Number arguments
    do i=1,n
       oparg=pc%data%i16(pc%offset+i+2_pm_p)
       if(oparg>=0) then
          arg(i)=stack%data%ptr(stack%offset+oparg)
       else if(oparg>=-int(pm_max_stack,pm_i16)) then
          arg(i)%data=>stack%data
          arg(i)%offset=stack%offset-oparg
          stack%data%ptr(stack%offset-oparg)=pm_null_obj
       else
          arg(i)=func%data%ptr(func%offset-oparg-pm_max_stack)
       endif
    enddo
    nargs=n
    pc%offset=pc%offset+n+3_pm_p

    20 continue
    if(pm_debug_level>2) then
       write(*,*) 'RUNNING',opcode,&
            op_names(opcode),opcode2,opcode3
    endif
    select case(opcode)
    case(op_call)
       newfunc=context%funcs%data%ptr(&
            context%funcs%offset+opcode2)
       goto 30
    case(op_poly_call)
       call poly_call_lookup(context,&
            func%data%ptr(func%offset+opcode2),arg,nargs,&
            opcode,opcode2)
       if(opcode==op_call) then
          newfunc=context%funcs%data%ptr(&
            context%funcs%offset+opcode2)
          goto 30
       else
          goto 20
       endif
    case(op_poly_call_vect)
       stack%data%ptr(stack%offset)%offset=opcode3/128
       call vect_poly_call(context,&
            func%data%ptr(func%offset+opcode2),&
            arg,nargs,opcode,opcode2,sptr,svec)
       if(opcode==-1) then
          ! All done
          goto 10
       elseif(opcode==-2) then
          ! Stalled
          errno=-1
          return
       elseif(opcode==op_call) then
          newfunc=context%funcs%data%ptr(&
            context%funcs%offset+opcode2)
          goto 30
       else
          goto 20
       endif
    case(op_return)
       pc=stack%data%ptr(stack%offset+1)
       func=stack%data%ptr(stack%offset+3)
       stack=stack%data%ptr(stack%offset+2)
       if(pm_fast_isnull(pc)) then
          errno=0
          return
       endif
    case(op_jmp)
       pc%offset=pc%offset+opcode2
    case(op_jmp_true)
       if(arg(1)%data%l(arg(1)%offset)) &
            pc%offset=pc%offset+opcode2
    case(op_jmp_false)
       if(.not.arg(1)%data%l(arg(1)%offset)) &
            pc%offset=pc%offset+opcode2
    case(op_and_jmp_none:op_andnot_jmp_any)
       flip=opcode==op_andnot_jmp_none.or.opcode==op_andnot_jmp_any
       ve=arg(2)%data%ptr(arg(2)%offset)
       esize=pm_fast_esize(ve)
       if(pm_fast_vkind(ve)==pm_long) then
          ! Vector engine is using a list of indices 
          ! - subset active cells
          jj=0
          do j=1,esize
             k=ve%data%ln(ve%offset+j)
             if(arg(3)%data%l(arg(3)%offset+k).neqv.flip) jj=jj+1
          enddo
          stack%data%ptr(stack%offset)%offset=opcode3/128
          arg(1)%data%ptr(arg(1)%offset)=&
               pm_new(context,pm_long,jj+1_pm_ln)
          arg(1)=arg(1)%data%ptr(arg(1)%offset)
          arg(1)%data%ln(arg(1)%offset)=ve%data%ln(ve%offset)
          jj=1
          do j=1,esize
             k=ve%data%ln(ve%offset+j)
             if(arg(3)%data%l(arg(3)%offset+k).neqv.flip) then
                arg(1)%data%ln(arg(1)%offset+jj)=k
                jj=jj+1
             endif
          enddo
       else
          ! Calculate new vector of active cell flags
          stack%data%ptr(stack%offset)%offset=opcode3/128
          arg(1)%data%ptr(arg(1)%offset)=pm_new(context,&
               pm_logical,esize+1_pm_ln)
          arg(4)=arg(1)%data%ptr(arg(1)%offset)
          k=0
          do j=0,esize
             ok=ve%data%l(ve%offset+j).and.&
                  (arg(3)%data%l(arg(3)%offset+j).neqv.flip)
             arg(4)%data%l(arg(4)%offset+j)=ok
             if(ok) k=k+1
          enddo
          ! If only a small number of cells active in vector, 
          ! change to using an index list
          if(k>0.and.esize>k*shrink_thresh) &
               arg(1)%data%ptr(arg(1)%offset)=&
               pm_shrink(context,arg(4),k)
       endif
       if(opcode<op_and_jmp_any) then
             ! .. _jmp_none
          if(k==0) pc%offset=pc%offset+opcode2
       else
          ! .. _jmp_any
          if(k>0) pc%offset=pc%offset+opcode2
       endif
    case(op_par_loop)
       stack%data%ptr(stack%offset)%offset=opcode3/128
       call par_loop(context,func,stack,pc,args,nargs,opcode2)
    case(op_par_loop_end)
       errno=0
       return
    case(op_dump)
       write(*,*) '== dump =='
       call pm_dump_tree(context,6,arg(1),1)
       write(*,*) '=========='
    case(op_setref)
       arg(1)%data%ptr(arg(1)%offset)=arg(2)
    case(op_setref_vect)
       if(pm_fast_vkind(arg(1))==pm_null) then
          arg(2)%data%ptr(arg(2)%offset:arg(2)%offset+&
               pm_fast_esize(arg(2)))=&
               arg(3)%data%ptr(arg(3)%offset:arg(3)%offset+&
               pm_fast_esize(arg(3)))
       else if(pm_fast_vkind(arg(1))==pm_logical) then
          esize=pm_fast_esize(arg(1))
          where(arg(1)%data%l(arg(1)%offset:arg(1)%offset+esize))
             arg(2)%data%ptr(arg(2)%offset:arg(2)%offset+&
                  pm_fast_esize(arg(2)))=&
                  arg(3)%data%ptr(arg(3)%offset:arg(3)%offset+&
                  pm_fast_esize(arg(3)))
          end where
       else
          forall(j=1:pm_fast_esize(arg(1)))
             arg(2)%data%ptr(arg(1)%data%ln(arg(1)%offset+j)+&
                  arg(2)%offset)=&
                  arg(3)%data%ptr(arg(1)%data%ln(arg(1)%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_struct,op_struct_vect)
       
    case(op_clone)
       stack%data%ptr(stack%offset)%offset=opcode3/128
       arg(1)%data%ptr(arg(1)%offset)=pm_deep_copy(context,arg(2))
    case(op_array)
       
    case(op_check)

    case(op_struct_elem,op_struct_elem_vect)
       arg(1)%data%ptr(arg(1)%offset)=arg(2)%data%ptr(arg(2)%offset&
            +opcode2)
    case(op_alloc_int)
       stack%data%ptr(stack%offset)%offset=opcode3/128
       call pm_new_multi(context,pm_int,1_pm_ln,&
            int(opcode2/(pm_max_stack+1),pm_ln),&
            int(iand(opcode2,pm_max_stack),pm_ln),stack)
    case(op_alloc_logical)
       stack%data%ptr(stack%offset)%offset=opcode3/128
       call pm_new_multi(context,pm_logical,1_pm_ln,&
            int(opcode2/(pm_max_stack+1),pm_ln),&
            int(iand(opcode2,pm_max_stack),pm_ln),stack)
 
    case(op_assign_int)
       v=stack%data%ptr(stack%offset+opcode2)
       v%data%i(v%offset)=arg(1)%data%i(arg(1)%offset)
    case(op_assign_int_vect)
       if(pm_fast_vkind(arg(1))==pm_null) then
          arg(2)%data%i(arg(2)%offset:arg(2)%offset+&
               pm_fast_esize(arg(2)))=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+&
               pm_fast_esize(arg(3)))
       else if(pm_fast_vkind(arg(1))==pm_logical) then
          esize=pm_fast_esize(arg(1))
          where(arg(1)%data%l(arg(1)%offset:arg(1)%offset+esize))
             arg(2)%data%i(arg(2)%offset:arg(2)%offset+&
                  pm_fast_esize(arg(2)))=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+&
                  pm_fast_esize(arg(3)))
          end where
       else
          forall(j=1:pm_fast_esize(arg(1)))
             arg(2)%data%i(arg(1)%data%ln(arg(1)%offset+j)+&
                  arg(2)%offset)=&
                  arg(3)%data%i(arg(1)%data%ln(arg(1)%offset+j)&
                  +arg(3)%offset)
          end forall
       endif
    case(op_gt_i)
       arg(1)%data%l(arg(1)%offset)=&
            arg(2)%data%i(arg(2)%offset)>arg(3)%data%i(arg(3)%offset)
    case(op_add_i)
       arg(1)%data%i(arg(1)%offset)=arg(2)%data%i(arg(2)%offset)+ &
            arg(3)%data%i(arg(3)%offset)
    case(op_add_i_vect)
       esize=pm_fast_esize(arg(3))
       if(opcode2>0) then
          arg(2)%data%ptr(arg(2)%offset)=arg(opcode2)
       else
          stack%data%ptr(stack%offset)%offset=opcode3/128
          arg(2)%data%ptr(arg(2)%offset)=pm_new(context,pm_int,esize)
       endif
       arg(2)=arg(2)%data%ptr(arg(2)%offset)
       if(pm_fast_vkind(arg(1))==pm_null) then
          arg(2)%data%i(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(arg(1))==pm_logical) then
          where(arg(1)%data%l(arg(1)%offset:arg(1)%offset+esize))
             arg(2)%data%i(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=1:pm_fast_esize(arg(1)))
             arg(2)%data%i(arg(1)%data%ln(arg(1)%offset+j)+&
                  arg(2)%offset)=&
                  arg(3)%data%i(arg(1)%data%ln(arg(1)%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%i(arg(1)%data%ln(arg(1)%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case default
!!$       if(pm_lib_proc(context,opcode,&
!!$            int(func%data%ptr(func%offset+1)%offset,pm_i16),&
!!$            func%data%ptr(func%offset+2),&
!!$            func%data%ptr(func%offset+3),&
!!$            arg,nargs)) goto 888
       write(*,*) 'OPCODE=',opcode
       call pm_panic('unknown opcode')
    end select

    goto 10
    
30  continue

    ! ***********************************
    ! This section implements a call
    !************************************
    if(.not.pm_fast_isnull(stack)) &
         stack%data%ptr(stack%offset)%offset=oparg/128 
    ! save current stack top
    newpc=newfunc%data%ptr(newfunc%offset)
    stacksize=newpc%data%i16(newpc%offset)
    newstack=pm_fast_newnc(context,pm_stack,stacksize)
    newstack%data%ptr(newstack%offset)=&
         pm_fast_tinyint(context,int(pm_stack_locals,pm_p))
    newstack%data%ptr(newstack%offset+1)=pc
    newstack%data%ptr(newstack%offset+2)=stack
    newstack%data%ptr(newstack%offset+3)=func
    newstack%data%ptr(newstack%offset+4:newstack%offset+nargs+3)=&
         arg(1:nargs)
    pc=newpc
    pc%offset=pc%offset+1_pm_p
    stack=newstack
    func=newfunc
      
    goto 10

888 continue

    call pm_delete_register(context,reg)
    
    return
    
999 continue

    call pm_panic(emess)
    
  contains

    include 'fisnull.inc'
    include 'fvkind.inc'
    include 'fnew.inc'
    include 'fnewnc.inc'
    include 'fesize.inc'
    include 'ftiny.inc'

  end function pm_run
 
  subroutine par_loop(context,func,stack,pc,args,num_args,start) 
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: func,stack,pc
    type(pm_ptr),dimension(num_args):: args
    integer,intent(in):: num_args
    integer(pm_i16),intent(in):: start
    type(pm_ptr):: newpc,ve,sptr,svec
    integer:: errno,i
    newpc=pc
    newpc%offset=newpc%offset+3_pm_p
    stack%data%ptr(stack%offset+start)=pm_null_obj
    ve=pm_fast_newnc(context,pm_long,3_pm_p)
    ve%data%ln(ve%offset)=0
    ve%data%ln(ve%offset+1_pm_p)=&
         pm_fast_esize(args(1)%data%ptr(args(1)%offset))
    ve%data%ln(ve%offset+2_pm_p)=1
    stack%data%ptr(stack%offset+start+1)=ve
    do i=1,num_args
       stack%data%ptr(stack%offset+start+1+i)=&
            args(i)%data%ptr(args(i)%offset+1_pm_p)
    enddo
    errno=pm_run(context,func,stack,newpc,0_pm_i16,0_pm_i16,&
         args,num_args,sptr,svec)
  contains
    include 'fesize.inc'
    include 'fnewnc.inc'
  end subroutine par_loop

  function pm_fork(context,proc,args,numargs) result(thread)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: proc
    integer,intent(in):: numargs
    type(pm_ptr),intent(in),dimension(numargs):: args
    type(pm_ptr):: thread
    integer(pm_p):: stacksize
    type(pm_ptr):: func,pc,stack

    ! Set up the initial stack
    func=proc
    pc=func%data%ptr(func%offset)
    stacksize=pc%data%i16(pc%offset)
    stack=pm_fast_new(context,pm_stack,stacksize)
    stack%data%ptr(stack%offset+pm_stack_pc)=pm_null_obj
    stack%data%ptr(stack%offset+pm_stack_oldstack)=pm_null_obj
    stack%data%ptr(stack%offset+pm_stack_func)=pm_null_obj
    stack%data%ptr(stack%offset+pm_stack_locals: &
         stack%offset+pm_stack_locals+numargs-1)=args(1:numargs)
    pc%offset=pc%offset+1_pm_p

    ! Create thread and queue it
    thread=pm_fast_new(context,pm_usr,6_pm_p)
    thread%data%ptr(thread%offset+pm_thread_next)=&
         context%threads_start
    thread%data%ptr(thread%offset+pm_thread_last)=pm_null_obj
    if(context%threads_start%data%vkind==pm_null) then
       context%threads_start%data%ptr(context%threads_start%offset+2)&
            =thread
      endif
    context%threads_start=thread
    if(context%threads_end%data%vkind==pm_null) &
         context%threads_end=thread
    thread%data%ptr(thread%offset+pm_thread_func)=func
    thread%data%ptr(thread%offset+pm_thread_pc)=pc
    thread%data%ptr(thread%offset+pm_thread_stack)=stack
    thread%data%ptr(thread%offset+pm_thread_inputs)=pm_null_obj
  contains
    include 'fnew.inc'
  end function pm_fork

  function pm_switch(context,func,stack,pc) result(ok)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(inout):: func,stack,pc
    logical:: ok
    type(pm_ptr):: thread
    thread=context%running
    thread%data%ptr(thread%offset+pm_thread_func)=func
    thread%data%ptr(thread%offset+pm_thread_pc)=pc
    thread%data%ptr(thread%offset+pm_thread_stack)=stack
    thread%data%ptr(thread%offset+pm_thread_next)=context%wait_start
    if(pm_fast_isnull(context%wait_start)) then
       context%wait_end=thread
    else
       context%wait_start%data%ptr(context%wait_start%offset+&
            pm_thread_last)=thread
    endif
    context%wait_start=thread
    ok=pm_resume(context,func,stack,pc)
  contains
    include 'fisnull.inc'
  end function  pm_switch

  function pm_resume(context,func,stack,pc) result(ok)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(out):: func,stack,pc
    logical:: ok
    type(pm_ptr):: thread
    thread=context%threads_start
    if(thread%data%vkind==pm_null) then
       ok=.false.
    else
       ! Take thread from front of dequeue
       context%threads_start=thread%data%ptr(thread%offset+&
            pm_thread_next)
       if(context%threads_start%data%vkind/=pm_null) &
            context%threads_start%data%ptr(&
            context%threads_start%offset+pm_thread_last)=pm_null_obj
       thread%data%ptr(thread%offset+pm_thread_next)=pm_null_obj
       func=thread%data%ptr(thread%offset+pm_thread_func)
       pc=thread%data%ptr(thread%offset+pm_thread_pc)
       stack=thread%data%ptr(thread%offset+pm_thread_stack)
       context%running=thread
       ok=.true.
    endif
  end function pm_resume

  function make_indices(context,args,num_args) result(errno)
    type(pm_context),pointer:: context
    type(pm_ptr),dimension(num_args):: args
    integer,intent(in):: num_args
    integer:: errno
    type(pm_ptr):: p
    integer:: i
    integer(pm_ln):: j1,j2,j3,n,m
    if(num_args<4.or.num_args>22.or.&
         ((num_args-1)/3)*3/=num_args-1) then
       call runtime_error(context,&
            'Incorrect number of arguments')
    endif
    args(1)=pm_fast_newusr(context,pm_index_type,2_pm_p)
    call pm_assign_new(context,args(1),1_pm_ln,pm_long,&
         int(num_args+1,pm_ln),.false.)
    p=args(1)%data%ptr(args(1)%offset+1_pm_p)
    if(pm_fast_vkind(args(2))==pm_long) then
       do i=2,num_args
          p%data%ln(p%offset+i)=args(i)%data%ln(args(i)%offset)
       enddo
    else
       do i=2,num_args
          p%data%ln(p%offset+i)=args(i)%data%ln(args(i)%offset)
       enddo
    end if
    n=1
    m=1
    do i=p%offset+2,p%offset+pm_fast_esize(p),3
       j1=p%data%ln(i)
       j2=p%data%ln(i+1)
       j3=p%data%ln(i+2)
       if(j1>j2.and.j3>=0.or.j1<j2.and.j3<=0) then
          call runtime_error(context,&
               'Sequence is incorrect')
       endif
       n=n*(j2-j1)/j3
       m=m*(j2-j1)
    enddo
    p%data%ln(p%offset)=m
    p%data%ln(p%offset+1_pm_p)=n
  contains
    include 'fnewusr.inc'
    include 'fesize.inc'
    include 'fvkind.inc'
  end function make_indices


  ! Calculate arr[x]=b
  function set_elems_i(context,arr,x,b) result(errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: arr,x,b
    integer:: errno
    type(pm_ptr):: a,y
    a=arr%data%ptr(arr%offset+1_pm_p)
    y=arr%data%ptr(arr%offset+2_pm_p)
    if(pm_fast_esize(y)/=pm_fast_esize(x)) then
       call runtime_error(context,&
            'Subscript dimensions do not match')
       errno=-1
       return
    endif
    if(x%data%ln(x%offset+1)/=y%data%ln(y%offset).or.&
         x%data%ln(x%offset+1)/=pm_fast_esize(x)+1) then
       call runtime_error(context,&
            'Subscript number of elements does not match')
    endif
    select case(pm_fast_esize(x))
    case(5)
       call d1(a%data%i(a%offset:),&
            b%data%i(b%offset:),&
            y%data%ln(y%offset+3),&
            y%data%ln(y%offset+4),&
            x%data%ln(x%offset+3),&
            x%data%ln(x%offset+4),&
            x%data%ln(x%offset+5))
    case(8)
       call d2(a%data%i(a%offset:),&
            b%data%i(b%offset:),&
            y%data%ln(y%offset+3),&
            y%data%ln(y%offset+4),&
            y%data%ln(y%offset+6),&
            y%data%ln(y%offset+7),&
            x%data%ln(x%offset+3),&
            x%data%ln(x%offset+4),&
            x%data%ln(x%offset+5),&
            x%data%ln(x%offset+6),&
            x%data%ln(x%offset+7),&
            x%data%ln(x%offset+8))
    end select
  contains
    include 'fesize.inc'

    subroutine d1(v,w,&
      i1,i2,&
      j1,j2,j3)
      integer,dimension(i1:i2)::v
      integer,dimension((j2-j1)/j3+1)::w
      integer(pm_ln):: i1,i2
      integer(pm_ln):: j1,j2,j3
      v(j1:j2:j3)=w
    end subroutine d1

    subroutine d2(v,w,&
      i1,i2,i3,i4,&
      j1,j2,j3,j4,j5,j6)
      integer,dimension(i1:i2,i3:i4)::v
      integer,dimension((j2-j1)/j3+1,(j5-j4)/j6)::w
      integer(pm_ln):: i1,i2,i3,i4
      integer(pm_ln):: j1,j2,j3,j4,j5,j6
      v(j1:j2:j3,j4:j5:j6)=w
    end subroutine d2

  end function set_elems_i


  ! Look up polymorphic call
  subroutine poly_call_lookup(context,polyfunc,args,nargs,op,op2)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: polyfunc
    integer,intent(in):: nargs
    type(pm_ptr),dimension(nargs),intent(in):: args
    integer(pm_i16),intent(out):: op,op2
    integer(pm_i16):: tno
    integer:: i
    tno=pm_arglist_type(context,args,nargs)
    do i=0,pm_fast_esize(polyfunc),3
       if(pm_arglist_type_includes(context,&
            polyfunc%data%i16(polyfunc%offset+i),tno)) then
          op=polyfunc%data%i16(polyfunc%offset+i+1)
          op2=polyfunc%data%i16(polyfunc%offset+i+2)
          return
       endif
    enddo
  contains
    include 'fesize.inc'
  end subroutine poly_call_lookup
  
  ! Vectorised polymorphic call
  subroutine vect_poly_call(context,polyfunc,args,nargs,&
       op,op2,sptr,svec)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: polyfunc
    integer,intent(in):: nargs
    type(pm_ptr),dimension(nargs),intent(inout):: args
    integer(pm_i16),intent(out):: op,op2
    type(pm_ptr),intent(out):: sptr,svec
    integer,dimension(pm_max_args):: poly_arg
    integer(pm_i16),dimension(pm_max_args):: typ
    type(pm_ptr),dimension(pm_max_args):: xargs
    integer:: i,num_poly_args,na,istat
    integer(pm_ln):: esize,j,n,npending
    type(pm_ptr),target:: ivec,set,sptrs,svecs
    type(pm_ptr):: p,func,sp,sv
    type(pm_reg),pointer:: reg
    integer(pm_i16):: opcode,opcode2
    num_poly_args=0
    do i=1,nargs
       typ(i)=pm_fast_typeof(args(i))
       if(typ(i)==pm_poly_type) then
          num_poly_args=num_poly_args+1
          poly_arg(num_poly_args)=i
       endif
    enddo
    if(num_poly_args==0) then
       call poly_call_lookup(context,polyfunc,args,nargs,op,op2)
    else
       ! Get type vectors for each poly argument
       na=0
       reg=>pm_register(context,'poly call',&
            ivec,set,sptrs,svecs,array=xargs,array_size=na)
       esize=pm_fast_esize(args(1))
       ivec=pm_new(context,pm_long,esize)
       set=pm_set_new(context,16_pm_ln)
       do i=1,num_poly_args
          xargs(i)=args(poly_arg(i))%data%ptr(&
               args(poly_arg(i))%offset+1_pm_p)
       enddo
       do j=0,esize
          do i=1,num_poly_args
             typ(i)=pm_fast_typeof(xargs(i))
          enddo
          ivec%data%ln(ivec%offset+j)=&
               pm_iset_add(context,set,typ,nargs)-1
       enddo
       n=pm_set_size(context,set)
       xargs(1:nargs)=args(1:nargs)
       na=nargs
       call separate_vecs(context,args,nargs,ivec,n-1)
       do i=1,nargs
          if(pm_fast_vkind(args(i))==pm_stack) then
             args(i)=pm_new(context,pm_pointer,n)
          endif
       enddo
       npending=0
       do j=0,n-1
          do i=1,nargs
             if(pm_fast_vkind(xargs(i))/=pm_stack) &
                  xargs(i)=args(i)%data%ptr(args(i)%offset+j)
          enddo
          call poly_call_lookup(context,polyfunc,xargs,nargs,&
               opcode,opcode2)
          istat=pm_run(context,pm_null_obj,pm_null_obj,pm_null_obj,&
               opcode,opcode2,xargs,nargs,sp,sv)
          if(istat==0) then
             do i=1,nargs
                if(pm_fast_vkind(xargs(i))==pm_stack) then
                   call pm_ptr_assign(context,args(i),j,xargs(i))
                endif
             enddo
          else
             if(npending==0) then
                sptrs=pm_new(context,pm_usr,n+1)
                sptrs%data%ptr(sptrs%offset)%offset=pm_poly_group_type
                call pm_assign_new(context,sptrs,1_pm_ln,&
                     pm_pointer,int(nargs,pm_ln),.false.)
                p=sptrs%data%ptr(sptrs%offset+1_pm_p)
                p%data%ptr(p%offset:p%offset+nargs-1)=args(1:nargs)
                svecs=pm_new(context,pm_pointer,n)
             endif
             call pm_ptr_assign(context,sptrs,j+2,sp)
             call pm_ptr_assign(context,svecs,j,sv)
             npending=npending+1
          endif
       enddo
       if(npending==0) then
          ! All done - write back outputs
          do i=1,nargs
             if(pm_fast_vkind(xargs(i))==pm_stack) &
                  xargs(i)%data%ptr(xargs(i)%offset)=&
                  make_poly_vec(context,args(i),ivec)
          enddo
          op=-1
       else
          ! Make vector of message values from stalled processes
          svec=make_poly_vec(context,svecs,ivec)
          op=-2
       endif
       call pm_delete_register(context,reg)
    endif
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fnewusr.inc'
  end subroutine  vect_poly_call

  ! Create a polymorphic vector from polymorphic call result
  ! vals contains list of component vectors
  ! subs contains list of which vector to use for each location
  ! Vector elements are drawn in order from indicated vectors 
  function make_poly_vec(context,vals,subs) result(polyvec)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: vals,subs
    type(pm_ptr):: polyvec
    integer(pm_ln):: i,j,k,esize,vsize
    type(pm_ptr):: p,q,pv,dv,poly,n
    type(pm_root),pointer:: root,root2
    esize=pm_fast_esize(subs)
    vsize=pm_fast_esize(vals)
    poly=pm_fast_newusr(context,int(pm_poly_type,pm_p),3_pm_p)
    root=>pm_add_root(context,poly)
    pv=pm_new(context,pm_pointer,esize+1_pm_p)
    poly%data%ptr(poly%offset+1_pm_p)=pv
    dv=pm_new(context,pm_long,esize+1_pm_p)
    poly%data%ptr(poly%offset+2_pm_p)=dv
    root2=>pm_new_as_root(context,pm_long,pm_fast_esize(vals)+1_pm_ln)
    n=root2%ptr
    n%data%ln(n%offset:n%offset+vsize)=0_pm_ln
    do i=0,esize
       j=subs%data%ln(subs%offset+i)
       p=vals%data%ptr(vals%offset+j)
       k=n%data%ln(n%offset+j)
       n%data%ln(n%offset+j)=k+1_pm_ln
       if(pm_fast_typeof(p)==pm_poly_type) then
          q=p%data%ptr(p%offset+2_pm_p)
          k=q%data%ln(q%offset+k)
          q=p%data%ptr(p%offset+1_pm_p)
          p=q%data%ptr(q%offset+k)
       endif
       pv%data%ptr(pv%offset+i)=p
       dv%data%ln(dv%offset)=k
    enddo
    call pm_delete_root(context,root)
    call pm_delete_root(context,root2)
  contains
    include 'fesize.inc'
    include 'ftypeof.inc'
    include 'fnewusr.inc'
  end function make_poly_vec

  ! Separate nvec vectors by index numbers (0..vsize) in subs
  subroutine separate_vecs(context,vecs,nvecs,subs,vsize)
    type(pm_context),pointer:: context
    integer,intent(in):: nvecs
    type(pm_ptr),intent(inout),dimension(nvecs):: vecs
    type(pm_ptr),intent(in)::subs
    integer(pm_ln),intent(in):: vsize
    integer(pm_ln):: i,j,k,esize
    integer:: iv
    integer(pm_i16),dimension(1):: key
    type(pm_ptr),target:: pv,dv,vals,n
    type(pm_ptr):: avec,dvec
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'separate poly',&
         pv,dv,vals,n)
    ! Count number of elts in each category
    n=pm_new(context,pm_long,vsize+1_pm_ln)
    n%data%ln(n%offset:n%offset+vsize-1_pm_ln)=0
    do i=0,esize
       j=subs%data%ln(subs%offset+i)
       n%data%ln(n%offset+j)=n%data%ln(n%offset+j)+1
    enddo
    ! Work out starting point for each category
    ! in re-arranged vector
    j=0
    do i=0,vsize-1
       k=n%data%ln(n%offset+i)
       n%data%ln(n%offset+i)=j
       j=j+k
    enddo
    n%data%ln(n%offset+vsize)=esize+1
    ! Re-arrange the vectors
    pv=pm_new(context,pm_pointer,esize)
    dv=pm_new(context,pm_pointer,esize)
    do iv=1,nvecs
       if(pm_fast_vkind(vecs(i))==pm_stack) cycle
       if(pm_fast_typeof(vecs(i))==pm_poly_type) then
          avec=vecs(iv)%data%ptr(vecs(iv)%offset+1_pm_p)
          dvec=vecs(iv)%data%ptr(vecs(iv)%offset+2_pm_p)
          do i=0,esize
             j=subs%data%ln(subs%offset+i)
             k=n%data%ln(n%offset+j)
             pv%data%ptr(pv%offset+k)=&
                  avec%data%ptr(avec%offset+i)
             dv%data%ln(dv%offset+k)=&
                  dvec%data%ln(dvec%offset+i)
             n%data%ln(n%offset+j)=k+1
          enddo
       else
          pv%data%ptr(pv%offset:pv%offset+esize)=vecs(iv)
          do i=0,esize
             j=subs%data%ln(subs%offset+i)
             k=n%data%ln(n%offset+j)
             dv%data%ln(dv%offset+k)=i
             n%data%ln(n%offset+j)=k+1
          enddo
       endif
       vals=pm_new(context,pm_pointer,vsize+1_pm_p)
       do i=0,vsize-1
          call pm_ptr_assign(context,vals,i,&
               ptr_vec_get_type(context,pv,dv,&
               n%data%ln(n%offset+i),&
               n%data%ln(n%offset+i+1)-1_pm_ln))
       enddo
       vecs(iv)=vals
    enddo
    call pm_delete_register(context,reg)
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine separate_vecs

  ! Separate poly vector into list of non-poly vectors
  ! and a vector of subscripts into that list
  subroutine separate_poly_vec(context,poly,xsubs,vects)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: poly
    type(pm_ptr),intent(out):: xsubs,vects
    integer(pm_ln):: i,j,k,vsize,esize
    integer(pm_i16),dimension(1):: key
    type(pm_ptr),target:: subs,set,pv,dv,vals,n
    type(pm_ptr):: avec,dvec
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'separate poly',&
         set,pv,dv,subs,vals,n)
    avec=poly%data%ptr(poly%offset+1_pm_p)
    dvec=poly%data%ptr(poly%offset+2_pm_p)
    esize=pm_fast_esize(avec)
    set=pm_set_new(context,8_pm_ln)
    subs=pm_new(context,pm_long,esize+1_pm_ln)
    do i=0,esize
       key(1)=pm_fast_typeof(avec%data%ptr(avec%offset+i))
       subs%data%ln(subs%offset+i)=pm_iset_add(context,set,key,1)
    enddo
    vsize=pm_set_size(context,set)-1_pm_ln
    set=pm_null_obj
    xsubs=subs
    n=pm_new(context,pm_long,vsize+2_pm_ln)
    do i=0,esize
       j=subs%data%ln(subs%offset+i)
       n%data%ln(n%offset+j)=n%data%ln(n%offset+j)+1
    enddo
    n%data%ln(n%offset+vsize+1_pm_ln)=esize+1
    j=0
    do i=0,vsize
       k=n%data%ln(n%offset+i)
       n%data%ln(n%offset+i)=j
       j=j+k
    enddo
    pv=pm_new(context,pm_pointer,esize)
    dv=pm_new(context,pm_pointer,esize)
    do i=0,esize
       j=subs%data%ln(subs%offset+i)
       pv%data%ptr(pv%offset+k)=avec%data%ptr(avec%offset+i)
       dv%data%ln(dv%offset+k)=dvec%data%ln(dvec%offset+i)
       n%data%ln(n%offset+j)=n%data%ln(n%offset+j)+1_pm_ln
    enddo
    vals=pm_new(context,pm_pointer,vsize+1_pm_p)
    do i=0,vsize
       vals%data%ptr(vals%offset+i)=&
            ptr_vec_get_type(context,pv,dv,&
            n%data%ln(n%offset+i),n%data%ln(n%offset+i+1)-1_pm_ln)
    enddo
    vects=vals
    call pm_delete_register(context,reg)
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine separate_poly_vec

  recursive function ptr_vec_get_type(context,&
       vec,disps,start,finish) result(outvec)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: vec,disps
    integer(pm_ln):: start,finish
    type(pm_ptr):: outvec
    integer(pm_ln):: esize,j,k,n
    integer:: vkind
    type(pm_ptr):: p,pvec
    type(pm_root),pointer:: root1,root2
    p=vec%data%ptr(vec%offset)
    vkind=pm_fast_vkind(p)
    esize=finish-start
    select case(vkind)
    case(pm_int)
       outvec=pm_new(context,pm_int,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%i(outvec%offset+j)=&
               p%data%i(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_undef:pm_null,pm_pointer:pm_stack)
       outvec=pm_new(context,pm_pointer,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%ptr(outvec%offset+j)=&
               p%data%ptr(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_usr)
       n=pm_fast_esize(vec%data%ptr(vec%offset))
       root1=>pm_new_as_root(context,pm_pointer,n)
       root2=>pm_new_as_root(context,pm_pointer,esize)
       outvec=root1%ptr
       pvec=root2%ptr
       do j=1,n
          do k=0,esize
             p=vec%data%ptr(vec%offset+k+start)
             pvec%data%ptr(pvec%offset+k)=p%data%ptr(p%offset+j)
          enddo
          call pm_ptr_assign(context,outvec,j,&
               ptr_vec_get_type(context,pvec,disps,0_pm_ln,esize))
       enddo
       call pm_delete_root(context,root1)
       call pm_delete_root(context,root2)
    end select

  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end function  ptr_vec_get_type

  function pm_arglist_type(context,args,nargs) result(tno)
    type(pm_context),pointer:: context
    type(pm_ptr),dimension(nargs),intent(in):: args
    integer,intent(in):: nargs
    integer(pm_i16):: tno
    integer(pm_i16),dimension(pm_max_args+2):: t
    integer:: i
    t(1)=pm_typ_is_tuple
    t(2)=0
    do i=1,nargs
       t(i+2)=pm_fast_typeof(args(i))
    enddo
    !!!
  contains
    include 'ftypeof.inc'
  end function pm_arglist_type

  function pm_arglist_type_includes(context,supertype,subtype) &
       result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: supertype,subtype
    logical:: ok
    ok=pm_typ_includes(context,supertype,subtype)
  end function pm_arglist_type_includes

  subroutine runtime_error(context,errmesg)
    type(pm_context),pointer:: context
    character(len=*):: errmesg
    write(*,*) 'Runtime error: ',trim(errmesg)
  end subroutine runtime_error

end module pm_vm
