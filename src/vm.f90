!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2016
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

! Virtual Machine for PM interpreter

module pm_vm
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_types
  use pm_sysdefs
  use pm_array
  use pm_parlib
  implicit none

  integer,parameter:: shrink_thresh=4

  integer,parameter:: vm_ok=0
  integer,parameter:: vm_error=1
  integer,parameter:: vm_break=2
  integer,parameter:: vm_stepout=3
  integer,parameter:: vm_resume=4

  integer:: vm_depth=0
  character(len=20),parameter:: spaces='                    '

contains
  
  ! *************************************
  ! Run main interpreter loop
  ! *************************************
  recursive function pm_run(context,funcin,stackin,pcin,&
       op,op2,args,num_args) result(errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: funcin,stackin,pcin
    integer(pm_i16),intent(in):: op,op2
    integer,intent(in):: num_args
    type(pm_ptr),dimension(num_args),intent(in):: args
    integer:: errno
    type(pm_ptr),target,dimension(pm_max_args):: arg
    integer,target:: nargs
    type(pm_ptr),target:: stack,newve,new2
    type(pm_ptr):: func,pc,newstack,newfunc,newpc,ve,p,v,w
    integer(pm_i16):: opcode,opcode2,opcode3,oparg,t1,t2
    integer:: i,ii,n
    integer(pm_ln):: j,jj,k,kk,m,esize
    integer,dimension(7):: ibuffer
    logical,dimension(7):: lbuffer

    logical:: flip,ok,done
    integer(pm_p):: stacksize
    type(pm_reg),pointer:: reg
    character(len=200):: mess
    nargs=0
    reg=>pm_register(context,'vm args',&
         stack,newve,new2,array=arg,array_size=nargs)
    opcode=op
    opcode2=op2
    if(num_args>0) arg(1:num_args)=args(1:num_args)
    nargs=num_args
    func=funcin
    stack=stackin
    pc=pcin
    if(opcode>=0) goto 20

    10 continue
    
    nargs=0
    opcode=pc%data%i16(pc%offset)
    opcode2=pc%data%i16(pc%offset+1_pm_p)
    opcode3=pc%data%i16(pc%offset+2_pm_p)
    n=iand(opcode3,pm_max_stack)  ! Number arguments
    !stack%data%ptr(stack%offset)%offset=&
    !     opcode3/(pm_max_stack+1) ! Stack top
    if(pm_debug_level>3) then
       write(*,*) 'DECODING>',opcode,&
            op_names(opcode),opcode2,n,opcode3/(pm_max_stack+1),&
            'pc=',pc%offset
       write(*,*) 'stack=',stack%data%hash,stack%offset,stack%data%esize
       call pm_dump_tree(context,6,&
            stack%data%ptr(stack%offset+pc%data%i16(pc%offset+3_pm_p)),2)
    endif
    oparg=pc%data%i16(pc%offset+3_pm_p)
    arg(1)=stack%data%ptr(stack%offset+oparg)
    ve=arg(1)%data%ptr(arg(1)%offset+1)
    esize=ve%data%ln(ve%offset)
    ve=arg(1)%data%ptr(arg(1)%offset)
    if(pm_debug_level>3) then
       write(*,*) 've.kind=',arg(1)%data%vkind,&
            've.vec.kind=',ve%data%vkind,'esize=',esize
    endif
    do i=2,n
       oparg=pc%data%i16(pc%offset+i+2_pm_p)
       if(pm_debug_level>3) write(*,*) 'OPARG>', oparg
       if(oparg>=0) then
          arg(i)=stack%data%ptr(stack%offset+oparg)
          if(pm_debug_level>3) then
             write(*,*) i,'STACK>>',oparg
          endif
       else if(oparg>=-int(pm_max_stack,pm_i16)) then
          arg(i)%data=>stack%data
          arg(i)%offset=stack%offset-oparg
          if(pm_debug_level>3)  then
             write(*,*) i,'STACKREF>>',-oparg,&
                  arg(i)%data%esize,stack%data%vkind,&
                  arg(i)%data%hash,stack%data%esize,&
                  stack%offset+stack%data%esize,&
                  stack%offset,arg(i)%offset
          endif
       else
          if(pm_debug_level>3) then
             write(*,*) i,'CONST>>',-oparg-pm_max_stack
          endif
          w=func%data%ptr(func%offset-oparg-pm_max_stack)
          if(pm_debug_level>3) then
             call pm_dump_tree(context,6,w,2)
          endif
          ii=pm_fast_vkind(w)
          if(ii<=pm_null) then
             arg(i)=w
          elseif(ii==pm_string) then
             arg(i)=make_string_vector(context,w,esize)
          else
             arg(i)=pm_new(context,int(ii,pm_p),esize+1)
             call pm_fill_vect(context,arg(i),w)
          endif
       endif
    enddo

    nargs=n
    pc%offset=pc%offset+n+3_pm_p
    
    ! Empty ve
    if(pm_fast_vkind(ve)==pm_tiny_int) then
       if(pm_debug_level>3) then
          write(*,*) 'NULLIFIED OP>',op_names(opcode)
       endif
       if(opcode==op_jmp_empty_ve) then
          pc%offset=pc%offset+opcode2
       elseif(opcode>=op_and_jmp_none.and.opcode<=op_andnot_jmp_any) then
          call set_arg(2,arg(1))
       endif
       if(pm_debug_level>3) then
          write(*,*) 'SKIPPING>',opcode,&
               op_names(opcode),opcode2,opcode3,&
               'pc=',pc%offset,'esize=',esize
       endif
       goto 10
    endif

    20 continue
    if(pm_debug_level>3) then
       write(*,*) 'RUNNING>',opcode,&
            op_names(opcode),opcode2,opcode3,&
            'pc=',pc%offset,'esize=',esize,pm_fast_vkind(ve)
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
       if(opcode==-1) then
          ! All done
          goto 10
       elseif(opcode==op_call) then
          newfunc=context%funcs%data%ptr(&
            context%funcs%offset+opcode2)
          goto 30
       else
          goto 20
       endif
    case(op_var_call)
       call var_call
       opcode=v%data%ptr(v%offset)%offset
       opcode2=v%data%ptr(v%offset+1_pm_p)%offset
       goto 20
    case(op_lookup_error)
       call runtime_error(context,'Could not find matching procedure')
       goto 999
    case(op_vcall)
       v=arg(nargs)
       ii=0
       do i=0,pm_fast_esize(v)
          if(pm_fast_vkind(v%data%ptr(v%offset+i))/=pm_tiny_int) then
             arg(nargs+ii)=v%data%ptr(v%offset+i)
             ii=ii+1
          endif
       enddo
       nargs=nargs+ii-1
       opcode=pc%data%i16(pc%offset)
       opcode2=pc%data%i16(pc%offset+1_pm_p)
       opcode3=pc%data%i16(pc%offset+2_pm_p)
       pc%offset=pc%offset+4_pm_p
       goto 20
    case(op_return)
       if(nargs>0) then
          do i=2,nargs
             v=stack%data%ptr(stack%offset+pm_stack_locals+opcode2+i-1)
             v%data%ptr(v%offset)=arg(i)
         enddo
       endif
       pc=stack%data%ptr(stack%offset+pm_stack_pc)
       func=stack%data%ptr(stack%offset+pm_stack_func)
       stack=stack%data%ptr(stack%offset+pm_stack_oldstack)
       if(pm_debug_level>3) then
          write(*,*) '===RETURN==STACK==',stack%offset
          write(*,*) spaces(1:vm_depth*2),'Call- RETURN'
       endif
       vm_depth=vm_depth-1
       if(pm_fast_isnull(pc)) then
          errno=0
          goto 888
       endif
    case(op_jmp)
       pc%offset=pc%offset+opcode2
    case(op_and_jmp_none:op_andnot_jmp_any)
       flip=opcode==op_andnot_jmp_none.or.opcode==op_andnot_jmp_any
       if(pm_fast_isnull(ve)) then
          ! No active mask - logical vector becomes new mask
          newve=pm_new(context,pm_logical,esize+1_pm_ln)
          if(flip) then
             newve%data%l(newve%offset:newve%offset+esize)=.not. &
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
          else
             newve%data%l(newve%offset:newve%offset+esize)= &
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
          endif
          k=0
          do j=0,esize
             if(newve%data%l(newve%offset+j)) then
                k=k+1
             endif
          enddo
          if(k>0.and.esize+1>=k*shrink_thresh) then
             newve=shrink_ve(context,newve,esize,k)
          endif
       elseif(pm_fast_vkind(ve)==pm_long) then
          ! Vector engine is using a list of indices 
          ! - subset active cells
          jj=0
          do j=0,pm_fast_esize(ve)
             k=ve%data%ln(ve%offset+j)
             if(arg(3)%data%l(arg(3)%offset+k).neqv.flip) jj=jj+1
          enddo
          if(jj>0) then
             newve=pm_new(context,pm_int,int(jj,pm_ln))
             k=0
             do j=0,pm_fast_esize(ve)
                jj=ve%data%ln(ve%offset+j)
                if(arg(3)%data%l(arg(3)%offset+jj).neqv.flip) then
                   newve%data%ln(newve%offset+k)=jj
                   k=k+1
                endif
             enddo
          else
             k=0
          endif
       elseif(pm_fast_vkind(ve)==pm_tiny_int) then
          k=0
       else
          ! Calculate new vector of active cell flags
          newve=pm_new(context,pm_logical,esize+1)
          k=0
          do j=0,esize
             ok=ve%data%l(ve%offset+j).and.&
                  (arg(3)%data%l(arg(3)%offset+j).neqv.flip)
             newve%data%l(newve%offset+j)=ok
             if(ok) k=k+1
          enddo
          ! If only a small number of cells active in vector, 
          ! change to index list
          if(k>0.and.esize+1>=k*shrink_thresh) then
             newve=shrink_ve(context,newve,esize,k)
          endif
       endif
       
       ! Empty ve
       if(k==0) newve=pm_fast_tinyint(context,0_pm_p)
       
       ! New vector engine structure
       ve=pm_fast_newnc(context,pm_pointer,2_pm_p)
       ve%data%ptr(ve%offset)=newve
       ve%data%ptr(ve%offset+1)=arg(1)%data%ptr(arg(1)%offset+1)
       call set_arg(2,ve)
       if(opcode<op_and_jmp_any) then
          ! .. _jmp_none
          if(k==0) pc%offset=pc%offset+opcode2
       else
          ! .. _jmp_any
          if(k>0) pc%offset=pc%offset+opcode2
       endif
    case(op_jmp_empty_ve)
       if(pm_fast_vkind(ve)==pm_tiny_int) &
            pc%offset=pc%offset+opcode2
    case(op_jmp_any_ve)
       ok=.false.
       do i=2,nargs
          if(pm_fast_vkind(arg(i)%data%ptr(arg(i)%offset))&
               /=pm_tiny_int) then
             ok=.true.
             exit
          endif
       enddo
       if(prc_frame(prc_depth)%this_nprc>1) then
          ok=sync_loop_end(ok)
       endif
       if(ok) then
          pc%offset=pc%offset+opcode2
       endif
    case(op_reduce)
       call split_import_vector(context,&
            arg(2)%data%ptr(arg(2)%offset),&
            newve,new2,p,done)
       call set_arg(2,p)
       if(done) then
          pc%offset=pc%offset+opcode2
       else
          n=(nargs-2)/4
          do i=3,n+2
             call set_arg(i,vector_get_elems(context,&
                  arg(i+n+n),newve,errno))
             call set_arg(i+n,vector_get_elems(context,&
                  arg(i+n+n),new2,errno))
          enddo
       endif
    case(op_jmp_nopar)
       if(conc_depth>0) then
          pc%offset=pc%offset+opcode2
       endif
    case(op_par_loop)
       errno=par_loop(context,func,stack,pc,arg,nargs,esize)
       if(errno/=0) goto 888
    case(op_par_loop_end)
       errno=0
       goto 888
    case(op_par_find)
       errno=par_find(context,func,stack,pc,arg,nargs,esize)
       if(errno/=0) goto 888
    case(op_par_find_end)
       errno=0
       goto 888
    case(op_sys_prc)
       ibuffer(1)=sys_prc
       call set_args_from_ibuffer(2,2,ibuffer)
    case(op_sys_nprc)
       ibuffer(1)=sys_nprc
       call set_args_from_ibuffer(2,2,ibuffer)
    case(op_this_prc)
       ibuffer(1)=prc_frame(prc_depth)%this_prc
       call set_args_from_ibuffer(2,2,ibuffer)
    case(op_this_nprc)
       ibuffer(1)=prc_frame(prc_depth)%this_nprc
       call set_args_from_ibuffer(2,2,ibuffer)
    case(op_shared_prc)
       ibuffer(1)=prc_frame(prc_depth)%shared_prc
       call set_args_from_ibuffer(2,2,ibuffer)
    case(op_shared_nprc)
       ibuffer(1)=prc_frame(prc_depth)%shared_nprc
       call set_args_from_ibuffer(2,2,ibuffer)
    case(op_is_shared)
       lbuffer(1)=prc_frame(prc_depth)%shared_nprc>1
       call set_args_from_lbuffer(2,2,lbuffer)
    case(op_is_par)
       lbuffer(1)=conc_depth==0
       call set_args_from_lbuffer(2,2,lbuffer)
    case(op_push_prc_grid)
       n=(nargs-1)/2
       call get_args_to_lbuffer(n+2,n+n+1,lbuffer)
       call push_prc_grid(context,lbuffer,n,ibuffer)
       call set_args_from_ibuffer(2,n+1,ibuffer)
    case(op_push_prc_split)
       call push_prc_split(context,&
            int(arg(2)%data%ln(arg(2)%offset)))
    case(op_push_prc_conc)
       conc_depth=conc_depth+1
    case(op_pop_prc_conc)
       conc_depth=conc_depth-1
    case(op_prc_test_push)
       lbuffer(1)=conc_depth==0
       if(.not.lbuffer(1)) conc_depth=conc_depth+1
       call set_args_from_lbuffer(2,2,lbuffer)
    case(op_pop_prc)
       call pop_prc(context)
    case(op_broadcast)
       call broadcast(int(arg(3)%data%ln(arg(3)%offset)),arg(2))
    case(op_broadcast_val)
       call set_arg(2,broadcast_val(context,int(arg(4)%data%ln(arg(4)%offset)),arg(3)))
    case(op_get_remote)
       newve=arg(2)%data%ptr(arg(2)%offset+1)
       esize=newve%data%ln(newve%offset)
       v=empty_copy_vector(context,arg(4+loop_call_extra_args),esize+1)
       call set_arg(3,v)
       newve=arg(2)%data%ptr(arg(2)%offset)
       if(pm_fast_vkind(newve)==pm_logical) &
            newve=shrink_ve(context,newve,esize)
       call get_remote(context,arg(5+loop_call_extra_args),&
            arg(6+loop_call_extra_args),&
            arg(4+loop_call_extra_args),v,newve)
    case(op_gather)
       j=prc_frame(prc_depth)%this_nprc
       stack%data%ptr(stack%offset+opcode2)=&
            simple_ve(context,j)
       do i=2,nargs
          v=arg(i)%data%ptr(arg(i)%offset)
          new2=empty_copy_vector(context,v,j)
          call gather(v,new2)
          call set_arg(i,new2)
       enddo
    case(op_setref)
       call set_arg(2,arg(3))
    case(op_clone_ve)
       stack%data%ptr(stack%offset+opcode2)=arg(1) 
    case(op_makekeys)
       v=pm_dict_new(context,8_pm_ln)
       if(.not.pm_fast_isnull(arg(3))) then
          ok=pm_dict_merge(context,v,arg(3),.true.)
       endif
       call set_arg(2,v)
       do i=4,nargs,2
          call pm_dict_set(context,&
               v,arg(i),arg(i+1),.true.,.true.,ok,m)
       enddo
    case(op_delkeys)
       v=pm_dict_new(context,8_pm_ln)
       call set_arg(2,v)
       delkey: do j=1,pm_dict_size(context,arg(2))
          w=pm_dict_key(context,arg(3),int(j,pm_ln))
          do i=4,nargs
            if(w%offset==arg(i)%offset) cycle delkey 
          enddo
          call pm_dict_set(context,v,w,&
               pm_dict_val(context,arg(3),&
               int(j,pm_ln)),.true.,.true.,ok,m)
       enddo delkey
    case(op_checkkeys)
       do i=2,nargs,2
          if(.not.pm_fast_isnull(arg(i+1))) then
             call runtime_error(context,&
                  'Unexpected keyword argument: '//&
                  trim(pm_name_as_string(context,arg(i)%offset)))
             goto 999
          endif
       enddo
    case(op_getvkey)
       call set_arg(2,pm_dict_lookup(context,arg(3),arg(4)))
    case(op_get_key)
       v=stack%data%ptr(stack%offset+opcode2)
       w=stack%data%ptr(stack%offset+opcode2+1)
       call set_arg(3,v)
       if(v%data%l(v%offset)) then
          if(pm_arg_type(w)/=pm_arg_type(arg(4))) then
             call runtime_error(context,&
                  'Keyword argument has wrong type')
             goto 999
          endif
          call set_arg(2,w)
       else
          call set_arg(2,arg(4))
       endif
   case(op_get_key2)
       v=stack%data%ptr(stack%offset+opcode2)
       w=stack%data%ptr(stack%offset+opcode2+1)
       call set_arg(2,v)
       call set_arg(3,w)
    case(op_default)
       v=alloc_arg(int(opcode2,pm_p),2)
    case(op_miss_arg)
       arg(2)%data%ptr(arg(2)%offset)=pm_fast_tinyint(context,0_pm_p)
    case(op_print)
       if(prc_frame(prc_depth)%shared_prc==0) then
          ve=shrink_ve(context,ve,esize)
          call vector_print_string(context,arg(2),ve)
       endif
    case(op_dump)
       write(*,*) '====================== dump ======================'
       call vector_dump(context,arg(2),1)
       write(*,*) '=================================================='
    case(op_dump_id)
       write(*,*) '====================== dumpid ======================'
       write(*,*) arg(2)%data%hash,arg(2)%offset
       write(*,*) '=================================================='
    case(op_concat)
       ve=shrink_ve(context,ve,esize)
       call set_arg(2,vector_concat_string(context,ve,&
            arg(3),arg(4)))
    case(op_clone)
       call set_arg(2,copy_vector(context,arg(3),ve))
    case(op_assign)
       errno=0
       call vector_assign(context,arg(2),arg(3),ve,errno,esize)
       if(errno/=0) goto 997
    case(op_struct)
       v=pm_fast_newusr(context,pm_struct_type,int(nargs,pm_p))
       call set_arg(2,v)
       if(opcode2<0) opcode2=pm_arglist_type(context,&
            pm_typ_is_struct,-opcode2,arg(3:),nargs-1)
       v%data%ptr(v%offset+1_pm_p)=&
            pm_fast_tinyint(context,int(opcode2,pm_p))
       v%data%ptr(v%offset+2:v%offset+nargs-1)=arg(3:nargs)
    case(op_rec)
       v=pm_fast_newusr(context,pm_rec_type,int(nargs,pm_p))
       call set_arg(2,v)
       if(opcode2<0) opcode2=pm_arglist_type(context,&
            pm_typ_is_rec,-opcode2,arg(3:),nargs-1)
       v%data%ptr(v%offset+1_pm_p)=&
            pm_fast_tinyint(context,int(opcode2,pm_p))
       v%data%ptr(v%offset+2:v%offset+nargs-1)=arg(3:nargs)
    case(op_check_logical)
       if(pm_fast_vkind(arg(2))/=pm_logical) then
          !!! Check poly kind
          call runtime_error(context,&
               'Logical expression did not yield a bool value')
          goto 999
       endif
    case(op_check)
       if(pm_fast_isnull(ve)) then
          if(.not.all(arg(3)%data%l(arg(3)%offset:&
               arg(3)%offset+esize))) then
             ve=shrink_ve(context,ve,esize)
             call vector_get_string(context,arg(2),ve,0_pm_ln,mess)
             call runtime_error(context,mess)
             goto 999
          endif
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and..not.&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize))) then
             ve=shrink_ve(context,ve,esize)
             call vector_get_string(context,arg(2),ve,0_pm_ln,mess)
             call runtime_error(context,mess)
             goto 999 
          endif
       else
          if(.not.all(arg(3)%data%l(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))) then
             ve=shrink_ve(context,ve,esize)
             call vector_get_string(context,arg(2),ve,0_pm_ln,mess)
             call runtime_error(context,mess)
             goto 999 
          endif
       endif
    case(op_check_assign)
       if(pm_arg_type(arg(3))/=pm_arg_type(arg(4))) then
          ve=shrink_ve(context,ve,esize)
          call vector_get_string(context,arg(2),ve,0_pm_ln,mess)
          call runtime_error(context,mess)
       endif
    case(op_same_type)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(arg(3))==pm_pointer) then
          if(pm_fast_vkind(arg(4))==pm_pointer) then
             do j=0,esize
                v%data%l(v%offset+j)=&
                     pm_arg_type(arg(3)%data%ptr(arg(3)%offset+j))==&
                     pm_arg_type(arg(4)%data%ptr(arg(4)%offset+j))
             enddo
          else
             i=pm_arg_type(arg(4))
             do j=0,esize
                v%data%l(v%offset+j)=&
                     pm_arg_type(arg(3)%data%ptr(arg(3)%offset+j))==i
             enddo
          endif
       elseif(pm_fast_vkind(arg(4))==pm_pointer) then
          i=pm_arg_type(arg(3))
          do j=0,esize
             v%data%l(v%offset+j)=i==&
                  pm_arg_type(arg(4)%data%ptr(arg(4)%offset+j))
          enddo
       else
          v%data%l(v%offset:v%offset+esize)=&
               pm_arg_type(arg(3))==pm_arg_type(arg(4))
       endif
    case(op_has_same_type)
       if(pm_fast_vkind(arg(3))==pm_pointer) then
          if(pm_fast_vkind(arg(4))==pm_pointer) then
             v=alloc_arg(2,pm_pointer)
             do j=0,esize
                if(pm_arg_type(arg(3)%data%ptr(arg(3)%offset+j))/=&
                     pm_arg_type(arg(4)%data%ptr(arg(4)%offset+j))) then
                   v%data%ptr(v%offset+j)=pm_null_obj
                else
                   v%data%ptr(v%offset+j)=&
                        pm_fast_name(context,int(sym_true,pm_p))
                endif
             enddo
          else
             v=alloc_arg(2,pm_pointer)
             i=pm_arg_type(arg(4))
             do j=0,esize
                if(pm_arg_type(arg(3)%data%ptr(arg(3)%offset+j))/=i) then
                   v%data%ptr(v%offset+j)=pm_null_obj
                else
                   v%data%ptr(v%offset+j)=&
                        pm_fast_name(context,int(sym_true,pm_p))
                endif
             enddo
          endif
       elseif(pm_fast_vkind(arg(4))==pm_pointer) then
          v=alloc_arg(2,pm_pointer)
          i=pm_arg_type(arg(3))
          do j=0,esize
             if(pm_arg_type(arg(4)%data%ptr(arg(4)%offset+j))/=i) then
                v%data%ptr(v%offset+j)=pm_null_obj
             else
                v%data%ptr(v%offset+j)=pm_fast_name(context,&
                     int(sym_true,pm_p))
             endif
          enddo
       else
          if(pm_arg_type(arg(3))/=pm_arg_type(arg(4))) then
             call set_arg(2,pm_null_obj)
          else
             call set_arg(2,pm_fast_name(context,int(sym_true,pm_p)))
          endif
       endif
   case(op_elem)
      call set_arg(2,&
           arg(3)%data%ptr(arg(3)%offset+opcode2))
    case(op_elem_ref)
      if(pm_fast_typeof(arg(3))==pm_elemref_type) then
         v=arg(3)%data%ptr(arg(3)%offset+2)
         call set_arg(2,elemref(context,pm_arg_type(v),&
              v%data%ptr(v%offset+opcode2),&
              arg(3)%data%ptr(arg(3)%offset+3)))
      else
         call set_arg(2,&
              arg(3)%data%ptr(arg(3)%offset+opcode2))
      endif
    case(op_poly_elem)
      i=pm_arg_type(arg(2))
       if(i==pm_poly_type) then
          ve=shrink_ve(context,ve,esize)
          w=pm_fast_newusr(context,pm_poly_type,3_pm_p)
          call set_arg(2,w)
          v=arg(3)%data%ptr(arg(3)%offset+2_pm_p)
          w%data%ptr(w%offset+2_pm_p)=v
          w=pm_assign_new(context,w,1_pm_ln,pm_pointer,esize+1,.false.)
          v=arg(3)%data%ptr(arg(3)%offset+1_pm_p)
          i=0
          do jj=0,pm_fast_esize(ve)
             k=ve%data%ln(ve%offset+jj)
             p=v%data%ptr(v%offset+k)
             call pm_elem_offset(context,pm_arg_type(p),&
                  abs(opcode2),opcode2<0,t1,t2)
             if(i==0) then 
                i=t2
             elseif(i/=t2) then
                i=-1
             endif
             if(t1<0) goto 996
             w%data%ptr(w%offset+k)=p%data%ptr(p%offset+t1)
          enddo
          if(i>0) then
             call set_arg(2,ptr_vec_get_type(context,w,ve,0_pm_ln,esize))
          endif
       elseif(i==pm_elemref_type) then
          !!!
       elseif(i==pm_struct_type.or.i==pm_rec_type) then
          call pm_elem_offset(context,int(pm_arg_type(arg(2)),pm_i16),&
               opcode2,.false.,t1,t2)
          if(t1<0) goto 996
          call set_arg(2,arg(2)%data%ptr(arg(2)%offset+t1))
       else
          goto 996
       endif
    case(op_make_rf)
       errno=0
       call set_arg(2,make_elem_ref(context,arg(3),arg(4),ve,errno))
       if(errno/=0) goto 997
    case(op_get_rf)
       if(pm_fast_typeof(arg(3))==pm_elemref_type) then
          errno=0
          call set_arg(2,vector_get_elems(context,&
               arg(3)%data%ptr(arg(3)%offset+2_pm_p),&
               arg(3)%data%ptr(arg(3)%offset+3_pm_p),&
               errno))
          if(errno/=0) goto 997
       else
          call set_arg(2,arg(3))
       endif
    case(op_array)
       if(opcode2<0) opcode2=pm_arglist_type(context,&
            pm_typ_is_rec,0_pm_i16,arg(3:4),2)
       call set_arg(2,make_array_dim(context,opcode2,&
            arg(3),arg(4),arg(5),arg(6),ve))
    case(op_array_get_elem)
       errno=0
       call set_arg(2,array_index(context,arg(3),&
            arg(4),ve,errno))
       if(errno/=0) goto 997
    case(op_array_set_elem)
       errno=0
       call array_set_index(context,arg(2),arg(3),arg(4),ve,errno)
       if(errno/=0) goto 997
    case(op_get_dom)
       v=arg(3)
       if(pm_fast_typeof(v)==pm_elemref_type) then
          v=v%data%ptr(v%offset+2_pm_p)
       endif
       call set_arg(2,v%data%ptr(v%offset+pm_array_dom))
    case(op_export_array)
       errno=0
       call array_export(context,arg(2),arg(3),arg(4),&
            arg(1)%data%ptr(arg(1)%offset+1_pm_p),errno)
       if(errno/=0) goto 997
    case(op_iota)
       call set_arg(2,vector_iota(context,&
            arg(3),arg(4),arg(5),arg(6),&
            arg(7)%data%ptr(arg(7)%offset+1)))
    case(op_import_val)
       call set_arg(2,import_vector(context,&
            arg(3),arg(1)%data%ptr(arg(1)%offset+1)))
    case(op_import_varg)
       new2=pm_fast_new(context,pm_pointer,int(pm_fast_esize(arg(3)),pm_p)+1_pm_p)
       do j=0,pm_fast_esize(arg(3))
          call pm_ptr_assign(context,new2,j,import_vector(context,&
               arg(3)%data%ptr(arg(3)%offset+j),arg(1)%data%ptr(arg(1)%offset+1)))
       enddo
       call set_arg(2,new2)
    case(op_import_scalar)
       call set_arg(2,vector_from_scalar(context,arg(3),esize))
    case(op_extract)
       errno=0
       call vector_extract(context,arg(2),arg(3),arg(4),&
            arg(1)%data%ptr(arg(1)%offset+1),errno)
       if(errno/=0) goto 997
    case(op_extract_first)
       errno=0
       v=arg(1)%data%ptr(arg(1)%offset+1)
       new2=empty_copy_vector(context,arg(3),pm_fast_esize(v)-3)
       call vector_extract(context,new2,arg(3),arg(4),&
            v,errno)
       call set_arg(2,new2)
       if(errno/=0) goto 997
    case(op_extractelm)
       call set_arg(2,arg(3)%data%ptr(arg(3)%offset+pm_array_vect))
    case(op_make_array)
       if(opcode2<0) opcode2=pm_arglist_type(context,&
            pm_typ_is_array,0_pm_i16,arg(3:4),2)
       call set_arg(2,make_array_from_vect(context,opcode2,&
            arg(3),arg(4),arg(5),arg(6),ve))
    case(op_eq)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_long) then
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=.true.
          end forall
       else
          v%data%l(v%offset:v%offset+esize)=.true.
       endif
       call vector_eq(context,arg(3),arg(4),v,esize,ve)
    case(op_ne)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_long) then
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=.true.
          end forall
       else
          v%data%l(v%offset:v%offset+esize)=.true.
       endif
       call vector_eq(context,arg(3),arg(4),v,esize,ve)
       if(pm_fast_vkind(ve)==pm_long) then
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)= .not.v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)
          end forall
       else
          v%data%l(v%offset:v%offset+esize)=&
               .not.v%data%l(v%offset:v%offset+esize)
       endif
    case(op_string_i)
       ve=shrink_ve(context,ve,esize)
       call set_arg(2,vector_make_string(context,&
            ve,arg(3),fmt_i_width,fmt_i))
    case(op_assign_i)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%i(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%i(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             arg(2)%data%i(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%i(arg(3)%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
    case(op_add_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_sub_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_mult_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
   case(op_divide_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%i(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          if(any(arg(4)%data%i(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_mod_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%i(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          v%data%i(v%offset:v%offset+esize)=mod(&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=mod(&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          if(any(arg(4)%data%i(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=mod(&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_pow_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_max_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               max(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  max(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_min_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               min(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  min(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_uminus_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               -arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  -arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_eq_i)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_ne_i)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_gt_i)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_ge_i)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_long_i)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_real_i)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_double_i)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_string_ln)
       ve=shrink_ve(context,ve,esize)   
       call set_arg(2,&
            vector_make_string(context,&
            ve,arg(3),fmt_ln_width,fmt_ln))
    case(op_assign_ln)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%ln(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%ln(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             arg(2)%data%ln(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%ln(arg(3)%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
   case(op_add_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_sub_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_mult_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
   case(op_divide_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%ln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%ln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          if(any(arg(4)%data%ln(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_pow_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_mod_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%ln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          v%data%ln(v%offset:v%offset+esize)=&
               mod(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%ln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  mod(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          if(any(arg(4)%data%ln(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  mod(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_max_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               max(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  max(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
   case(op_min_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               min(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  min(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
   case(op_uminus_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               -arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  -arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
       
    case(op_eq_ln)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_ne_ln)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_gt_ln)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_ge_ln)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_int_ln)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_real_ln)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_double_ln)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
       

    case(op_string_r)
       ve=shrink_ve(context,ve,esize)
       call set_arg(2,vector_make_string(context,&
            ve,arg(3),fmt_r_width,fmt_r))
    case(op_assign_r)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%r(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%r(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             arg(2)%data%r(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%r(arg(3)%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
    case(op_add_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_sub_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_mult_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_divide_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%r(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
           v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%r(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          if(any(arg(4)%data%r(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_mod_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%r(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=mod(&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%r(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=mod(&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          if(any(arg(4)%data%r(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=mod(&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_pow_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_max_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               max(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  max(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_min_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               min(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  min(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_uminus_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               -arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  -arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_eq_r)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_ne_r)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_gt_r)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_ge_r)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
       
    case(op_int_r)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_long_r)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_double_r)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif

    case(op_string_d)
       ve=shrink_ve(context,ve,esize)
       call set_arg(2,vector_make_string(context,&
            ve,arg(3),fmt_d_width,fmt_d))
    case(op_assign_d)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%d(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%d(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             arg(2)%data%d(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%d(arg(3)%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
    case(op_add_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_sub_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_mult_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_divide_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%d(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%d(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          if(any(arg(4)%data%d(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_mod_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%d(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=mod(&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%d(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=mod(&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          if(any(arg(4)%data%d(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=mod(&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_pow_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_max_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               max(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  max(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_min_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               min(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  min(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_uminus_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               -arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  -arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_eq_d)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_ne_d)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_gt_d)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_ge_d)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
       
    case(op_int_d)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_long_d)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif
    case(op_real_d)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif

 
    case(op_string_l)
       ve=shrink_ve(context,ve,esize)
       call set_arg(2,&
            vector_make_string(context,&
            ve,arg(3),fmt_l_width,fmt_l))
    case(op_assign_l)
       if(pm_fast_vkind(arg(2))/=pm_logical) goto 999
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%l(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%l(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             arg(2)%data%l(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%l(arg(3)%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
    case(op_and)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).and.&
               arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).and.&
                  arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset).and.&
                  arg(4)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
   case(op_or)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).or.&
               arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).or.&
                  arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset).or.&
                  arg(4)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_not)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               .not.arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  .not.arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  .not.arg(3)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif

    case default
       write(*,*) 'Unknown opcode=',opcode
       write(*,*) op_names(opcode)
       call pm_panic('unknown opcode')
    end select

    goto 10
    
30  continue

    ! ***********************************
    ! This section implements a call
    !************************************
    ! save current stack top
    newpc=newfunc%data%ptr(newfunc%offset)
    stacksize=newpc%data%i16(newpc%offset)
    i=newpc%data%i16(newpc%offset+1)

    newstack=pm_fast_new(context,pm_stack,stacksize)
    if(pm_debug_level>3) then
       write(*,*) 'NEWSTACK>',newstack%offset,newstack%data%esize,&
            mod(newstack%offset-1,newstack%data%esize+1)
    endif
    if(pm_debug_level>0) then
       if(mod(newstack%offset-1,newstack%data%esize+1)/=0) &
            call pm_panic('stack alloc')
    endif
    newstack%data%ptr(newstack%offset)=&
         pm_fast_tinyint(context,int(stacksize-1,pm_p))
    newstack%data%ptr(newstack%offset+pm_stack_pc)=pc
    newstack%data%ptr(newstack%offset+pm_stack_oldstack)=stack
    newstack%data%ptr(newstack%offset+pm_stack_func)=func
    newstack%data%ptr(newstack%offset+pm_stack_nullve)=context%null_ve
    if(i<nargs) then
       if(pm_debug_level>3) write(*,*) 'TRIM ARG LIST>',i,nargs
       newstack%data%ptr(newstack%offset+pm_stack_locals:&
            newstack%offset+i+pm_stack_locals-1)=&
            arg(1:i)
       context%temp_obj1=newstack
       v=pm_fast_newnc(context,pm_pointer,int(nargs-i,pm_p))
       v%data%ptr(v%offset:v%offset+nargs-i-1)=arg(i+1:nargs)
       newstack%data%ptr(newstack%offset+i+pm_stack_locals)=v
       context%temp_obj1=pm_null_obj
       nargs=i+1
    else
       if(pm_debug_level>0) then
          if(i/=nargs) then
             write(*,*) &
                  trim(pm_name_as_string(context,proc_get_name(newfunc)))
             write(*,*) 'Num Args=',nargs,'Num pars expected=',i
             call pm_panic('Arg/param mismatch')
          endif
       endif
       if(nargs>0) then
          newstack%data%ptr(newstack%offset+pm_stack_locals:&
               newstack%offset+pm_stack_locals+nargs-1)=&
               arg(1:nargs)
       endif
    endif
    pc=newpc
    pc%offset=pc%offset+3_pm_p
    stack=newstack
    func=newfunc
    if(pm_debug_level>3) then
       vm_depth=vm_depth+1
       write(*,*) spaces(1:vm_depth*2),'Call-',esize,&
            trim(pm_name_as_string(context,proc_get_name(func)))
       write(*,*) '======CALL==NEW STACK======',&
            nargs,stack%data%esize,stack%offset
       do i=pm_stack_locals,nargs+pm_stack_locals-1
          write(*,*) i,nargs+4
          call vector_dump(context,stack%data%ptr(stack%offset+i),1)
       enddo
       write(*,*) '============================'
    endif
      
    goto 10

996 continue

    ! struct/rec operation error
    call runtime_error(context,&
         'Cannot access structure/record element: '//&
               trim(pm_name_as_string(context,int(opcode2,pm_p))))
    goto 999

997 continue

    ! Vector/array operation errors
    select case(errno)
    case(vector_type_error)
       call runtime_error(context,&
            'Type mismatch')
    case(vector_size_error)
       call runtime_error(context,&
            'Size mismatch')
    case(vector_slice_error)
       call runtime_error(context,&
            'Malformed slice')
    case(vector_index_error)
       call runtime_error(context,&
            'Index out of range')
    case(vector_shape_error)
       call runtime_error(context,&
            'Arrays do not have conforming domains')
    end select

999 continue

    call dump_stack(context,stack,func,pc)

888 continue

    call pm_delete_register(context,reg)

    return
    
  contains

    include 'fisnull.inc'
    include 'fvkind.inc'
    include 'fnew.inc'
    include 'fnewnc.inc'
    include 'fnewusr.inc'
    include 'fesize.inc'
    include 'ftiny.inc'
    include 'ftypeof.inc'
    include 'fname.inc'

    subroutine set_arg(iarg,val)
      integer,intent(in):: iarg
      type(pm_ptr),intent(in):: val
      arg(iarg)%data%ptr(arg(iarg)%offset)=val
    end subroutine set_arg

    function alloc_arg(vkind,iarg) result(ptr)
      integer(pm_p),intent(in):: vkind
      integer,intent(in)::iarg
      type(pm_ptr):: ptr
      ptr=pm_new(context,vkind,esize+1)
      arg(iarg)%data%ptr(arg(iarg)%offset)=ptr
    end function alloc_arg

    function shrink_ve(context,mask,esize,n) result(ind)
      type(pm_context),pointer:: context
      type(pm_ptr),intent(in):: mask
      integer(pm_ln),intent(in):: esize
      integer(pm_ln),intent(in),optional:: n
      type(pm_ptr):: ind
      integer(pm_ln):: j,k
      if(pm_fast_isnull(mask)) then
         ind=pm_new(context,pm_long,esize+1)
         do j=0,esize
            ind%data%ln(ind%offset+j)=j
         enddo
      elseif(pm_fast_vkind(mask)==pm_long) then
         ind=mask
      else
         if(present(n)) then
            k=n
         else
            k=0
            do j=0,esize
               if(mask%data%l(mask%offset+j)) then
                  k=k+1
               endif
            enddo
         endif
         ind=pm_new(context,pm_long,k)
         k=0
         do j=0,esize
            if(mask%data%l(mask%offset+j)) then
               ind%data%ln(ind%offset+k)=j
               k=k+1
            endif
         enddo
      endif
    end function shrink_ve

    ! Handle passing keys/vkeys to a var call
    subroutine var_call
      type(pm_ptr):: vk_set,vk,key,keys,vals,val 
      logical:: vkey
      integer:: i,j,k,m
      v=func%data%ptr(func%offset+opcode2)
!!$      call pm_dump_tree(context,6,pm_dict_keys(context,v),2)
!!$      write(*,*) arg(2)%offset
      v=pm_dict_lookup(context,v,arg(2))
      i=v%data%ptr(v%offset+3_pm_p)%offset
      if(i<0) then
         vkey=.true.
         i=-i
      else
         vkey=.false.
      endif
      write(*,*) 'i=',i
      ! Delete proc var param
      do j=2,nargs
         arg(j)=arg(j+1)
      enddo
      nargs=nargs-1
!!$      vk_set=v%data%ptr(v%offset+2_pm_p)
!!$      if(pm_fast_isnull(vk_set)) then
!!$         if(vkey) then
!!$            arg(i+1:nargs-1)=arg(i+2:nargs)
!!$            nargs=nargs-1
!!$         else
!!$            arg(i+1:nargs-2)=arg(i+3:nargs)
!!$            nargs=nargs-2
!!$         endif
!!$      else
!!$         if(vkey) then
!!$            vk=arg(i+2)
!!$            j=pm_set_size(context,vk_set)
!!$            arg(i+j+2:nargs+j+1)=arg(i+1:nargs)
!!$            nargs=nargs+j-1
!!$            if(pm_fast_isnull(vk)) then
!!$               do k=i+1,i+j+1
!!$                  arg(k)=pm_null_obj
!!$               enddo
!!$            else
!!$               arg(i+1:i+j)=pm_null_obj
!!$               arg(i+j+1)=pm_dict_new(context,8_pm_ln)
!!$               keys=pm_dict_keys(context,vk)
!!$               vals=pm_dict_vals(context,vk)
!!$               do k=0,pm_dict_size(context,vk)-1
!!$                  key=keys%data%ptr(keys%offset+k)
!!$                  m=pm_set_lookup(context,vk_set,key)
!!$                  if(m>0) then
!!$                     arg(i+m)=vals%data%ptr(vals%offset+k)
!!$                  else
!!$                     call pm_dict_set(context,arg(i+j+1),key,&
!!$                          vals%data%ptr(vals%offset+k),.true.,.true.,ok)
!!$                  endif
!!$               enddo
!!$            endif
!!$         else
!!$            vk=arg(i+2)
!!$            j=pm_set_size(context,vk_set)
!!$            arg(i+j+1:nargs+j-2)=arg(i+3:nargs)
!!$            nargs=nargs+j-2
!!$            if(pm_fast_isnull(vk)) then
!!$               do k=i+1,i+j
!!$                  arg(k)=pm_null_obj
!!$               enddo
!!$            else
!!$               keys=pm_set_keys(context,vk_set)
!!$               do k=i+1,i+j
!!$                  arg(k)=pm_dict_lookup(context,vk,&
!!$                       keys%data%ptr(keys%offset+k-i-1))
!!$               enddo
!!$            endif
!!$         endif
!!$      endif
    end subroutine var_call
    
    subroutine set_args_from_ibuffer(argstart,argend,ibuffer)
      integer,intent(in):: argstart,argend
      integer:: i,n,ierror
      integer,intent(in),dimension(*):: ibuffer
      type(pm_ptr):: v
      n=argend-argstart+1
      if(n<1.or.n>7) call pm_panic('args from ibuffer')
      if(esize<huge(v%offset)-1) then
         do i=argstart,argend
            v=pm_fast_newnc(context,pm_long,int(esize,pm_p)+1_pm_p)
            v%data%ln(v%offset:v%offset+esize)=ibuffer(i-argstart+1)
            call set_arg(i,v)
         enddo
      else
         do i=argstart,argend
            v=pm_new(context,pm_long,esize+1_pm_ln)
            v%data%ln(v%offset:v%offset+esize)=ibuffer(i-argstart+1)
            call set_arg(i,v)
         enddo
      endif
    end subroutine set_args_from_ibuffer

   subroutine set_args_from_lbuffer(argstart,argend,lbuffer)
      integer,intent(in):: argstart,argend
      integer:: i,n,ierror
      logical,intent(in),dimension(*):: lbuffer
      type(pm_ptr):: v
      n=argend-argstart+1
      if(n<1.or.n>7) call pm_panic('args from ibuffer')
      if(esize<huge(v%offset)-1) then
         do i=argstart,argend
            v=pm_fast_newnc(context,pm_logical,int(esize,pm_p)+1_pm_p)
            v%data%l(v%offset:v%offset+esize)=lbuffer(i-argstart+1)
            call set_arg(i,v)
         enddo
      else
         do i=argstart,argend
            v=pm_new(context,pm_int,esize+1_pm_ln)
            v%data%l(v%offset:v%offset+esize)=lbuffer(i-argstart+1)
            call set_arg(i,v)
         enddo
      endif
    end subroutine set_args_from_lbuffer

    subroutine get_args_to_lbuffer(argstart,argend,lbuffer)
      integer,intent(in):: argstart,argend
      logical,dimension(*),intent(out):: lbuffer
      integer:: i,n
      n=argend-argstart+1
      if(n<1.or.n>7) call pm_panic('args to lbuffer')
      do i=argstart,argend
         !write(*,*) '>>>',i,argstart
         !call pm_dump_tree(context,6,arg(i),2)
         lbuffer(i-argstart+1)=arg(i)%data%l(arg(i)%offset)
      enddo
    end subroutine get_args_to_lbuffer

  end function pm_run


  ! Parallel loop 
  function par_loop(context,func,stack,pc,arg,&
       num_args,old_esize) result(errno) 
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: func,stack,pc
    type(pm_ptr),dimension(num_args):: arg
    integer,intent(in):: num_args
    integer(pm_ln),intent(in):: old_esize
    integer:: errno
    type(pm_ptr):: newpc,newve
    integer(pm_ln):: n,m
    newpc=pc
    newpc%offset=newpc%offset+4_pm_p
    arg(2)%data%ptr(arg(2)%offset)=&
         pm_fast_newnc(context,pm_pointer,2_pm_p)
    newve=arg(2)%data%ptr(arg(2)%offset)
    newve%data%ptr(newve%offset)=pm_null_obj
    n=pm_fast_esize(arg(3))+1_pm_ln
    m=sum(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+n-1_pm_ln))-1_pm_ln
    newve=pm_assign_new(context,newve,1_pm_ln,pm_long,n+4_pm_ln,.false.)
    newve%data%ln(newve%offset)=m
    newve%data%ln(newve%offset+1)=0
    newve%data%ln(newve%offset+2)=0
    newve%data%ln(newve%offset+3)=m
    newve%data%ln(newve%offset+4:newve%offset+3+n)=&
         arg(3)%data%ln(arg(3)%offset:arg(3)%offset+n-1)
    errno=pm_run(context,func,stack,newpc,-1_pm_i16,0_pm_i16,&
         arg,num_args)
  contains
    include 'fesize.inc'
    include 'fnewnc.inc'
  end function  par_loop

  ! Parallel find
  function par_find(context,func,stack,pc,arg,&
       num_args,old_esize) result(errno) 
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: func,stack,pc
    type(pm_ptr),dimension(num_args):: arg
    integer,intent(in):: num_args
    integer(pm_ln),intent(in):: old_esize
    integer:: errno
    type(pm_ptr):: newpc,newve
    integer(pm_ln):: n,m,prc
    type(pm_ptr)::p
    newpc=pc
    newpc%offset=newpc%offset+4_pm_p
    arg(2)%data%ptr(arg(2)%offset)=&
         pm_fast_newnc(context,pm_pointer,2_pm_p)
    newve=arg(2)%data%ptr(arg(2)%offset)
    newve%data%ptr(newve%offset)=pm_null_obj
    n=pm_fast_esize(arg(5))+1_pm_ln
    m=sum(arg(5)%data%ln(arg(5)%offset:arg(5)%offset+n-1_pm_ln))-1_pm_ln
    newve=pm_assign_new(context,newve,1_pm_ln,pm_long,n+4_pm_ln,.false.)
    newve%data%ln(newve%offset)=m
    newve%data%ln(newve%offset+1)=0
    newve%data%ln(newve%offset+2)=0
    newve%data%ln(newve%offset+3)=m+1
    newve%data%ln(newve%offset+4:newve%offset+3+n)=&
         arg(5)%data%ln(arg(5)%offset:arg(5)%offset+n-1)
    p=pm_new(context,pm_logical,m+1)
    arg(4)%data%ptr(arg(4)%offset)=p
    p%data%l(p%offset:p%offset+m)=.false.
    errno=pm_run(context,func,stack,newpc,-1_pm_i16,0_pm_i16,&
         arg,num_args)
    if(prc_frame(prc_depth)%this_nprc>1) then
       prc=sync_find(p%data%l(p%offset))
       p=pm_fast_newnc(context,pm_long,1)
       arg(3)%data%ptr(arg(3)%offset)=p
       p%data%ln(p%offset)=prc
    endif
  contains
    include 'fesize.inc'
    include 'fnewnc.inc'
  end function par_find

  
  ! Look up polymorphic call
  subroutine poly_call_lookup(context,polyfunc,args,nargs,op,op2)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: polyfunc
    integer,intent(in):: nargs
    type(pm_ptr),dimension(nargs),intent(in):: args
    integer(pm_i16),intent(out):: op,op2
    integer(pm_i16):: tno
    integer:: i,st

    st=polyfunc%data%i16(polyfunc%offset)
    tno=pm_arglist_type(context,&
         pm_typ_is_tuple,0_pm_i16,args(st:),nargs-st+1)
    call dump_type(context,6,tno,2)
    do i=1,pm_fast_esize(polyfunc),3
       if(pm_arglist_type_includes(context,&
            polyfunc%data%i16(polyfunc%offset+i),tno)) then
          op=polyfunc%data%i16(polyfunc%offset+i+1)
          op2=polyfunc%data%i16(polyfunc%offset+i+2)
          return
       endif
    enddo
    op=op_lookup_error
  contains
    include 'fesize.inc'
  end subroutine poly_call_lookup
  
  ! Vectorised polymorphic call
  subroutine poly_call(context,polyfunc,args,nargs,esize,&
       op,op2)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: polyfunc
    integer,intent(in):: nargs
    type(pm_ptr),dimension(nargs),intent(inout):: args
    integer(pm_i16),intent(out):: op,op2
    integer,dimension(pm_max_args):: poly_arg
    integer(pm_i16),dimension(pm_max_args):: typ
    type(pm_ptr),dimension(pm_max_args):: xargs
    integer:: i,num_poly_args,na,istat
    integer(pm_ln):: esize,j,n
    type(pm_ptr),target:: ivec,set,sptrs,svecs
    type(pm_ptr):: p,func,sp,sv
    type(pm_reg),pointer:: reg
    integer(pm_i16):: opcode,opcode2
    num_poly_args=0
    do i=1,nargs
       typ(i)=pm_arg_type(args(i))
       if(typ(i)==pm_pointer) then
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
       ivec=pm_new(context,pm_long,esize+1)
       set=pm_set_new(context,16_pm_ln)
       do i=1,num_poly_args
          xargs(i)=args(poly_arg(i))
       enddo
       do j=0,esize
          do i=1,num_poly_args
             typ(i)=pm_fast_typeof(xargs(i))
          enddo
          ivec%data%ln(ivec%offset+j)=&
               pm_iset_add(context,set,typ,num_poly_args)-1
       enddo
       n=pm_set_size(context,set)
       xargs(1:nargs)=args(1:nargs)
       na=nargs
       do i=1,nargs
          if(pm_fast_vkind(args(i))==pm_stack) then
             args(i)=pm_new(context,pm_pointer,n)
          endif
       enddo
       do j=0,n-1
          do i=1,nargs
             if(pm_fast_vkind(xargs(i))/=pm_stack.and.&
                  pm_fast_vkind(args(i))==pm_pointer) &
                  xargs(i)=vec_separate(context,args(i),ivec,i)
          enddo
          call poly_call_lookup(context,polyfunc,xargs,nargs,&
               opcode,opcode2)
          istat=pm_run(context,pm_null_obj,pm_null_obj,pm_null_obj,&
               opcode,opcode2,xargs,nargs)
          if(istat==0) then
             do i=1,nargs
                if(pm_fast_vkind(xargs(i))==pm_stack) then
                   call pm_ptr_assign(context,args(i),j,&
                        xargs(i)%data%ptr(xargs(i)%offset))
                endif
             enddo
          endif
       enddo
       ! All done - write back outputs
       do i=1,nargs
          if(pm_fast_vkind(xargs(i))==pm_stack) then
             xargs(i)%data%ptr(xargs(i)%offset)=&
                  make_poly_vec(context,args(i),ivec)
          endif
       enddo
       op=-1
       call pm_delete_register(context,reg)
    endif
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fnewusr.inc'
  end subroutine  poly_call

  function simple_ve(context,n) result(ve)
    type(pm_context),pointer:: context
    integer(pm_ln),intent(in):: n
    type(pm_ptr):: ve
    type(pm_ptr):: ve1
    integer(pm_p):: extra
    if(n/=1) then
       extra=1
    else
       extra=0
    endif
    ve1=pm_fast_newnc(context,pm_long,4_pm_p+extra)
    ve1%data%ln(ve1%offset)=n-1
    ve1%data%ln(ve1%offset+1)=0_pm_ln
    ve1%data%ln(ve1%offset+2)=0_pm_ln
    ve1%data%ln(ve1%offset+3)=n
    if(extra/=0) ve1%data%ln(ve1%offset+4)=n
    context%temp_obj1=ve1
    ve=pm_fast_newnc(context,pm_pointer,2_pm_p)
    ve%data%ptr(ve%offset)=pm_null_obj
    ve%data%ptr(ve%offset+1)=ve1
    context%null_ve=ve
  contains
    include 'fnewnc.inc'
  end function simple_ve

  function pm_arglist_type(context,tkind,tname,args,nargs) result(tno)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: tkind,tname
    type(pm_ptr),dimension(nargs),intent(in):: args
    integer,intent(in):: nargs
    integer(pm_i16):: tno
    integer(pm_i16),dimension(pm_max_args+2):: t
    integer:: i
    t(1)=tkind
    t(2)=tname
    do i=1,nargs
       tno=pm_fast_typeof(args(i))
       if(tno>=pm_struct_type.and.tno<=pm_array_type) then
          tno=args(i)%data%ptr(args(i)%offset+1_pm_p)%offset
       endif
       t(i+2)=tno
    enddo
    tno=pm_ivect_lookup(context,context%tcache, &
         t,nargs+2)
    if(tno<=0) tno=pm_idict_add(context,context%tcache,&
         t,nargs+2,pm_null_obj)
  contains
    include 'ftypeof.inc'
  end function pm_arglist_type

  function pm_arg_type(arg) result(tno)
    type(pm_ptr),intent(in):: arg
    integer(pm_i16):: tno
    tno=pm_fast_typeof(arg)
    if(tno>=pm_struct_type.and.tno<=pm_elemref_type) then
       tno=arg%data%ptr(arg%offset+1_pm_p)%offset
    endif
  contains
    include 'ftypeof.inc'
  end function pm_arg_type

  function pm_arglist_type_includes(context,supertype,subtype) &
       result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: supertype,subtype
    logical:: ok
    ok=pm_typ_includes(context,supertype,subtype)
  end function pm_arglist_type_includes

  subroutine runtime_error(context,errmesg)
    type(pm_context),pointer:: context
    character(len=*),intent(in):: errmesg
    write(*,*)
    write(*,*) 'Runtime error: ',trim(errmesg)
  end subroutine runtime_error

  subroutine dump_stack(context,dstack,dfunc,dpc)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: dstack,dfunc,dpc
    type(pm_ptr):: stack,func,pc
    integer(pm_p):: name
    character(len=80):: mess
    integer(pm_p):: line,modl
    stack=dstack
    func=dfunc
    pc=dpc
    write(*,*) '=========================Call trace ========================='
    do while(.not.pm_fast_isnull(func))
       call proc_line_module(func,&
            int(pc%offset-func%data%ptr(func%offset)%offset)+1,line,modl)
       name=proc_get_name(func)
       if(name==sym_pm_system) then
          mess='Program: '
       else
          mess=trim(pm_name_as_string(context,name))//' in: '
       endif
       call pm_name_string(context,modl,mess(len_trim(mess)+2:))
       write(*,*) trim(mess),'  line',line
       func=stack%data%ptr(stack%offset+pm_stack_func)
       pc=stack%data%ptr(stack%offset+pm_stack_pc)
       stack=stack%data%ptr(stack%offset+pm_stack_oldstack)
    enddo
    write(*,*) '============================================================='
  contains
    include 'fisnull.inc'
  end subroutine dump_stack

end module pm_vm
