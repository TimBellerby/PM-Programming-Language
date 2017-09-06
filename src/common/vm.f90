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

! Virtual Machine for PM interpreter

module pm_vm
  use pm_kinds
  use pm_sysdep
  use pm_compbase
  use pm_memory
  use pm_lib
  use pm_types
  use pm_vmdefs
  use pm_sysdefs
  use pm_array
  use pm_parlib
  implicit none

  ! Return status codes
  integer,parameter:: vm_ok=0
  integer,parameter:: vm_error=1
  integer,parameter:: vm_break=2
  integer,parameter:: vm_stepout=3
  integer,parameter:: vm_resume=4

  ! Debugging (of VM)
  integer,private:: vm_depth=0
  character(len=20),private,parameter:: spaces='                    '


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
    character(len=pm_comm_mess_len):: mess
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
    errno=-1
    if(opcode>=0) then
       ve=pm_null_obj
       goto 20
    endif

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
            'pc=',pc%offset,'arg1=',pc%data%i16(pc%offset+3_pm_p)
       write(*,*) 'stack=',stack%data%hash,stack%offset,stack%data%esize
       call pm_dump_tree(context,6,&
            stack%data%ptr(stack%offset+pc%data%i16(pc%offset+3_pm_p)),2)
    endif
    oparg=pc%data%i16(pc%offset+3_pm_p)
    arg(1)=stack%data%ptr(stack%offset+oparg)
    if(pm_debug_level>0) then
       if(pm_fast_vkind(arg(1))/=pm_pointer) then
          write(*,*) 'BAD VE!!!!'
          goto 999
       endif
    endif
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
          !pc%offset=pc%offset+opcode2
       elseif(opcode>=op_and_jmp_none.and.opcode<=op_andnot_jmp_any) then
          call set_arg(2,arg(1))
       elseif(opcode==op_jmp_any_ve) then
          if(sync_loop_end(.false.)) pc%offset=pc%offset+opcode2
       elseif(opcode==op_clone_ve) then
          stack%data%ptr(stack%offset+opcode2)=arg(1)
       elseif(opcode==op_jmp) then
          pc%offset=pc%offset+opcode2
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
!!$      if(pm_debug_level>3.or.vm_depth==2) &
!!$           write(*,*) spaces(1:vm_depth*2),&
!!$      'RETURN>',trim(pm_name_as_string(context,proc_get_name(func)))
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
          if(k>0.and.esize+1>=k*pm_shrink_thresh) then
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
             newve=pm_new(context,pm_long,int(jj,pm_ln))
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
          if(k>0.and.esize+1>=k*pm_shrink_thresh) then
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
!!$       if(pm_fast_vkind(ve)==pm_tiny_int) &
!!$            pc%offset=pc%offset+opcode2
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
    case(op_jmp_noshare)
       if(prc_frame(prc_depth)%shared_nprc<=1) then
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
       lbuffer(1)=prc_frame(prc_depth)%is_shared
       call set_args_from_lbuffer(2,2,lbuffer)
    case(op_is_par)
       lbuffer(1)=conc_depth==0
       call set_args_from_lbuffer(2,2,lbuffer)
    case(op_get_dims)
       n=(nargs-2)/2
       call get_args_to_ibuffer(n+3,n+n+2,ibuffer)
       call get_dims(int(arg(n+2)%data%ln(arg(n+2)%offset)),n,ibuffer)
       call set_args_from_ibuffer(2,n+1,ibuffer)
    case(op_push_prc_grid)
       n=(nargs-1)/2
       call get_args_to_lbuffer(2,n+1,lbuffer)
       call get_args_to_ibuffer(n+2,n+n+1,ibuffer)
       call push_prc_grid(context,lbuffer,n,ibuffer)
    case(op_push_prc_split)
       call push_prc_split(context,&
            int(arg(2)%data%ln(arg(2)%offset)))
    case(op_push_prc_distr)
       call push_prc_distr(context)
    case(op_push_prc_conc)
       conc_depth=conc_depth+1
    case(op_pop_prc_conc)
       conc_depth=conc_depth-1
    case(op_prc_test_push)
       lbuffer(1)=conc_depth==0
       if(.not.lbuffer(1)) conc_depth=conc_depth+1
       call set_args_from_lbuffer(2,2,lbuffer)
    case(op_pop_prc)
       if(conc_depth==0) then
          if(sync_status(pm_prc_running)==pm_prc_error) goto 777
       endif
       call pop_prc(context)
    case(op_broadcast)
       if(sync_status(pm_prc_running)==pm_prc_error) goto 777
       call broadcast(context,int(arg(3)%data%ln(arg(3)%offset)),arg(2))
    case(op_broadcast_val)
       if(sync_status(pm_prc_running)==pm_prc_error) goto 777
       call set_arg(2,broadcast_val(context,&
            int(arg(4)%data%ln(arg(4)%offset)),arg(3)))
    case(op_get_remote)
       if(sync_status(pm_prc_running)==pm_prc_error) goto 777
       newve=arg(2)%data%ptr(arg(2)%offset+1)
       esize=newve%data%ln(newve%offset)
       v=empty_copy_vector(context,arg(4+loop_call_extra_args),esize+1)
       call set_arg(3,v)
       newve=arg(2)%data%ptr(arg(2)%offset)
       if(pm_fast_vkind(newve)==pm_logical) &
            newve=shrink_ve(context,newve,pm_fast_esize(newve))
       call get_remote(context,arg(5+loop_call_extra_args),&
            arg(6+loop_call_extra_args),&
            arg(4+loop_call_extra_args),v,newve)
    case(op_put_remote)
       if(sync_status(pm_prc_running)==pm_prc_error) goto 777
       newve=arg(2)%data%ptr(arg(2)%offset)
       if(pm_fast_vkind(newve)==pm_logical) &
            newve=shrink_ve(context,newve,pm_fast_esize(newve))
       call put_remote(context,arg(5+loop_call_extra_args),&
            arg(6+loop_call_extra_args),&
            arg(3+loop_call_extra_args),&
            arg(4+loop_call_extra_args),newve)

    case(op_get_remote_distr)
       if(sync_status(pm_prc_running)==pm_prc_error) goto 777
       newve=arg(2)%data%ptr(arg(2)%offset+1)
       esize=newve%data%ln(newve%offset)
       w=arg(4+loop_call_extra_args)
       w=w%data%ptr(w%offset+pm_array_vect)
       w=w%data%ptr(w%offset)
       v=empty_copy_vector(context,w,esize+1)
       call set_arg(3,v)
       newve=arg(2)%data%ptr(arg(2)%offset)
       if(pm_fast_vkind(newve)==pm_logical) &
            newve=shrink_ve(context,newve,pm_fast_esize(newve))
       call get_remote(context,arg(5+loop_call_extra_args),&
            arg(6+loop_call_extra_args),&
            w,v,newve)
    case(op_put_remote_distr)
       if(sync_status(pm_prc_running)==pm_prc_error) goto 777
       newve=arg(2)%data%ptr(arg(2)%offset)
       if(pm_fast_vkind(newve)==pm_logical) &
            newve=shrink_ve(context,newve,pm_fast_esize(newve))
       w=arg(3+loop_call_extra_args)
       w=w%data%ptr(w%offset+pm_array_vect)
       w=w%data%ptr(w%offset)
       call put_remote(context,arg(5+loop_call_extra_args),&
            arg(6+loop_call_extra_args),&
            w,arg(4+loop_call_extra_args),&
            newve)
    case(op_gather)
       if(sync_status(pm_prc_running)==pm_prc_error) goto 777
       j=prc_frame(prc_depth)%this_nprc
       stack%data%ptr(stack%offset+opcode2)=&
            simple_ve(context,j)
       do i=2,nargs
          v=arg(i)%data%ptr(arg(i)%offset)
          new2=empty_copy_vector(context,v,j)
          call gather(context,v,new2,j)
          call set_arg(i,new2)
       enddo
    case(op_isend)
       v=arg(5+loop_call_extra_args)%data%ptr(&
            arg(5+loop_call_extra_args)%offset&
            +pm_array_vect)
       v=v%data%ptr(v%offset)
       call isend(&
            int(arg(4+loop_call_extra_args)%data%&
            ln(arg(4+loop_call_extra_args)%offset)),&
            v,&
            arg(3+loop_call_extra_args),0_pm_ln,&
            1+pm_fast_esize(arg(3+loop_call_extra_args)),data_tag)
    case(op_irecv)
       v=arg(5+loop_call_extra_args)%data%ptr(&
            arg(5+loop_call_extra_args)%offset&
            +pm_array_vect)
       v=v%data%ptr(v%offset)
       call irecv(context,&
            int(arg(4+loop_call_extra_args)%data%&
            ln(arg(4+loop_call_extra_args)%offset)),&
            v,&
            arg(3+loop_call_extra_args),0_pm_ln,&
            1+pm_fast_esize(arg(3+loop_call_extra_args)),data_tag)
    case(op_sync_mess)
       if(sync_status(pm_prc_running)==pm_prc_error) goto 777
       call complete_messages(context)
    case(op_wshare)
       v=alloc_arg(pm_long,2)
       do j=0,esize
          w=arg(3)%data%ptr(arg(3)%offset+pm_array_vect)
          w=w%data%ptr(w%offset+j)
          v%data%ln(v%offset+j)=wshare(&
               w%data%ln(w%offset:w%offset+pm_fast_esize(w)),&
               arg(4)%data%ln(arg(4)%offset+j),arg(5)%data%ln(arg(5)%offset+j),&
               arg(6)%data%ln(arg(6)%offset+j))
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
       call set_arg(2,v)
       w=alloc_arg(pm_logical,3)
       if(pm_fast_vkind(v)==pm_tiny_int) then
          w%data%l(w%offset:w%offset+esize)=.false.
          call set_arg(2,arg(4))
       else
          w%data%l(w%offset:w%offset+esize)=.true. 
       endif
   case(op_get_key2)
       v=stack%data%ptr(stack%offset+opcode2)
       call set_arg(2,v)
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
       call set_arg(2,copy_vector(context,arg(3),ve,0_pm_ln,-1_pm_ln))
    case(op_assign)
       errno=0
       call vector_assign(context,arg(2),arg(3),ve,errno,esize)
       if(errno/=0) goto 997
    case(op_struct)
       v=pm_fast_newusr(context,pm_struct_type,int(nargs,pm_p))
       call set_arg(2,v)
       v%data%ptr(v%offset+1_pm_p)=&
            pm_fast_tinyint(context,int(opcode2,pm_p))
       v%data%ptr(v%offset+2:v%offset+nargs-1)=arg(3:nargs)
    case(op_rec)
       v=pm_fast_newusr(context,pm_rec_type,int(nargs,pm_p))
       call set_arg(2,v)
       v%data%ptr(v%offset+1_pm_p)=&
            pm_fast_tinyint(context,int(opcode2,pm_p))
       v%data%ptr(v%offset+2:v%offset+nargs-1)=arg(3:nargs)
     case(op_check)
       if(pm_fast_isnull(ve)) then
          if(.not.all(arg(3)%data%l(arg(3)%offset:&
               arg(3)%offset+esize))) then
             ve=shrink_ve(context,ve,esize)
             call vector_get_string(context,arg(2),ve,0_pm_ln,mess)
             call runtime_error(context,mess)
             errno=1
             goto 999
          endif
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and..not.&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize))) then
             ve=shrink_ve(context,ve,esize)
             call vector_get_string(context,arg(2),ve,0_pm_ln,mess)
             call runtime_error(context,mess)
             errno=1
             goto 999 
          endif
       else
          if(.not.all(arg(3)%data%l(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))) then
             ve=shrink_ve(context,ve,esize)
             call vector_get_string(context,arg(2),ve,0_pm_ln,mess)
             call runtime_error(context,mess)
             errno=1
             goto 999 
          endif
       endif
    case(op_check_assign)
       if(pm_arg_type(arg(3))/=pm_arg_type(arg(4))) then
          ve=shrink_ve(context,ve,esize)
          call vector_get_string(context,arg(2),ve,0_pm_ln,mess)
          call runtime_error(context,mess)
          errno=1
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
             v=alloc_arg(pm_pointer,2)
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
             v=alloc_arg(pm_pointer,2)
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
          v=alloc_arg(pm_pointer,2)
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
    case(op_elem,op_elem_ref)
       if(pm_fast_isnull(arg(3))) goto 999
       if(pm_fast_vkind(arg(3))==pm_int) goto 999
       call set_arg(2,&
            arg(3)%data%ptr(arg(3)%offset+opcode2))
    case(op_make_rf)
       errno=0
       call set_arg(2,make_elem_ref(context,arg(3),arg(4),ve,errno))
       if(errno/=0) goto 997
    case(op_get_rf)
       if(pm_fast_typeof(arg(3))==pm_elemref_type) then
          errno=0
          call set_arg(2,&
               get_elem_ref(context,arg(3),esize,errno))
          if(errno/=0) goto 997
       else
          call set_arg(2,arg(3))
       endif
    case(op_array)
       call set_arg(2,make_array_dim(context,opcode2,&
            arg(3),arg(4),arg(5),ve))
    case(op_array_get_elem)
       errno=0
       call set_arg(2,array_index(context,arg(3),&
            arg(4),ve,esize,errno))
       if(errno/=0) goto 997
    case(op_array_set_elem)
       errno=0
       call array_set_index(context,arg(2),arg(3),arg(4),ve,errno)
       if(errno/=0) goto 997
    case(op_get_dom)
       call set_arg(2,array_dom(context,arg(3),esize))
    case(op_iota)
       if(nargs==7) then
          call set_arg(2,vector_iota(context,&
               arg(3),arg(4),arg(5),arg(6),&
               arg(7)%data%ptr(arg(7)%offset+1)))
       else
          call set_arg(2,vector_iota_trunc(context,&
               arg(3),arg(4),arg(5),arg(6),arg(7),&
               arg(8)%data%ptr(arg(8)%offset+1)))
       endif
    case(op_indices)
       v=alloc_arg(pm_long,2)
       call vector_indices(arg(1)%data%ptr(arg(1)%offset+1),v)
    case(op_import_val)
       call set_arg(2,import_vector(context,&
            arg(3),arg(1)%data%ptr(arg(1)%offset+1)))
    case(op_import_varg)
       new2=pm_fast_new(context,pm_pointer,&
            int(pm_fast_esize(arg(3)),pm_p)+1_pm_p)
       do j=0,pm_fast_esize(arg(3))
          call pm_ptr_assign(context,new2,j,import_vector(context,&
               arg(3)%data%ptr(arg(3)%offset+j),&
               arg(1)%data%ptr(arg(1)%offset+1)))
       enddo
       call set_arg(2,new2)
    case(op_import_scalar)
       call set_arg(2,vector_from_scalar(context,arg(3),0_pm_ln,esize,.true.))
    case(op_extract)
       errno=0
       call vector_extract(context,arg(2),arg(3),arg(4),&
            arg(1)%data%ptr(arg(1)%offset+1),errno)
       if(errno/=0) goto 997
    case(op_extract_first)
       errno=0
       v=arg(1)%data%ptr(arg(1)%offset+1)
       if(pm_fast_isnull(ve)) then
          newve=pm_new(context,pm_long,esize+1)
          newve%data%l(newve%offset:newve%offset+esize)=.true.
       elseif(pm_fast_vkind(ve)==pm_logical) then
          newve=ve
       else
          newve=pm_new(context,pm_long,esize+1)
          newve%data%l(newve%offset:newve%offset+esize)=.false.
          forall(j=0:pm_fast_esize(ve))
             newve%data%l(newve%offset+ve%data%ln(ve%offset+j))=.true.
          end forall
       endif
       new2=empty_copy_vector(context,arg(3),pm_fast_esize(v)-3)
       call vector_extract(context,new2,arg(3),newve,&
            v,errno)
       call set_arg(2,new2)
       if(errno/=0) goto 997
    case(op_extractelm)
       call set_arg(2,get_elem_ref(context,arg(3),esize,errno))
       if(errno/=0) goto 997
    case(op_make_array)
       call set_arg(2,make_array_from_vect(context,opcode2,&
            arg(3),arg(4),arg(5),ve))
    case(op_make_mask)
       v=stack%data%ptr(stack%offset+pm_stack_locals+1)
       newve=v%data%ptr(v%offset+1)
       m=newve%data%ln(newve%offset)
       newve=v%data%ptr(v%offset)
       new2=pm_new(context,pm_logical,m+1)
       if(pm_fast_isnull(newve)) then
          new2%data%l(new2%offset:new2%offset+m)=.true.
       elseif(pm_fast_vkind(newve)==pm_logical)then
          new2%data%l(new2%offset:new2%offset+m)=&
               newve%data%l(newve%offset:newve%offset+m)
       elseif(pm_fast_vkind(newve)==pm_tiny_int) then
          new2%data%l(new2%offset:new2%offset+m)=.false.
       else
          new2%data%l(new2%offset:new2%offset+m)=.false.
          do j=0,pm_fast_esize(newve)
             new2%data%l(new2%offset+newve%data%ln(newve%offset+j))=.true.
          enddo
       endif
       call set_arg(2,new2)
    case(op_redim)
       arg(5)=arg(3)%data%ptr(arg(3)%offset+pm_array_vect)
       arg(6)=arg(4)
       call set_arg(2,array_redim(context,opcode2,arg(3),arg(4)))
    case(op_make_poly)
       ve=shrink_ve(context,ve,esize)
       call set_arg(2,poly_new(context,arg(3),ve,esize))
    case(op_any)
       newve=poly_check_type(context,arg(4),opcode2,ve,esize)
       k=-1
       do j=0,esize
          if(newve%data%l(newve%offset+j)) then
             k=j
             exit
          endif
       enddo
       if(k==-1) then
          newve=pm_fast_tinyint(context,0_pm_p)
       else
          k=count(newve%data%l(newve%offset+k:newve%offset+esize))
          if(esize+1>k*pm_shrink_thresh) then
             newve=shrink_ve(context,newve,esize,k)
          endif
          new2=empty_copy_vector(context,&
               arg(4)%data%ptr(arg(4)%offset+k),esize+1)
          errno=0
          call poly_get(context,new2,arg(4),newve,esize,errno)
          if(errno/=0) goto 997
          call set_arg(3,new2)
       endif
       ve=pm_fast_newnc(context,pm_pointer,2_pm_p)
       ve%data%ptr(ve%offset)=newve
       ve%data%ptr(ve%offset+1)=arg(1)%data%ptr(arg(1)%offset+1)
       call set_arg(2,ve)
    case(op_dash)
       new2=copy_vector(context,arg(4),ve,0_pm_ln,esize+1)
       call poly_get(context,new2,arg(3),ve,esize,errno)
       if(errno/=0) goto 997
       call set_arg(2,new2)
    case(op_get_poly)
       newve=poly_check_type(context,arg(3),opcode2,ve,esize)
       call poly_get(context,arg(2),arg(3),newve,esize,errno)
       if(errno/=0) goto 997
    case(op_get_poly2)
       t1=full_type(arg(3))
       newve=poly_check_type(context,arg(4),t1,ve,esize)
       call poly_get(context,arg(3),arg(4),newve,esize,errno)
       if(errno/=0) goto 997
       call set_arg(2,newve)
    case(op_get_poly_or)
       new2=copy_vector(context,arg(4),ve,0_pm_ln,esize+1)
       newve=poly_check_type(context,arg(3),opcode2,ve,esize)
       call poly_get(context,new2,arg(3),newve,esize,errno)
       if(errno/=0) goto 997
       call set_arg(2,new2)
    case(op_pack)
       call pm_dump_tree(context,6,arg(5),2)
       call set_arg(2,array_pack(context,arg(3),opcode2,arg(4),arg(5),arg(6)))
    case(op_eq)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_long) then
          v%data%l(v%offset:v%offset+esize)=.false.
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
          v%data%l(v%offset:v%offset+esize)=.false.
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
       if(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%i(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%i(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
       else
          forall(j=0:pm_fast_esize(ve))
             arg(2)%data%i(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%i(arg(3)%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
    case(op_add_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intmul) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
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
      if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          if(pm_mask_intdiv) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%i(v%offset:v%offset+esize)=&
                     arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)/&
                     arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
             end where
          else
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%i(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          if(pm_mask_intmod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%i(v%offset:v%offset+esize)=mod(&
                     arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%i(v%offset:v%offset+esize)=mod(&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%i(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          v%data%i(v%offset:v%offset+esize)=mod(&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intpow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  max(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               max(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  min(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               min(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  -arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               -arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
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
    case(op_abs_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intabs) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=abs(&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               abs(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=abs(&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_bnot_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=not(&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               not(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=not(&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_band_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  iand(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               iand(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  iand(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_bor_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  ior(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               ior(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ior(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_bxor_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  ieor(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               ieor(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ieor(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_bshift_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  ishft(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               ishft(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ishft(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
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
       esize=pm_fast_esize(arg(3))
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmul) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%ln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          if(pm_mask_longmul) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%ln(v%offset:v%offset+esize)=&
                     arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)/&
                     arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
             end where
          else
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%ln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
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
    case(op_mod_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%ln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          if(pm_mask_longmod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%ln(v%offset:v%offset+esize)=&
                     mod(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%ln(v%offset:v%offset+esize)=&
               mod(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%ln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          v%data%ln(v%offset:v%offset+esize)=&
               mod(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
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
    case(op_pow_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longpow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
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
    case(op_max_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  max(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               max(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  min(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               min(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  -arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               -arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_gt_ln)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
         v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
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
    case(op_abs_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longabs) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=abs(&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               abs(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=abs(&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_bnot_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=not(&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               not(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=not(&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_band_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  iand(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               iand(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  iand(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_bor_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  ior(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               ior(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ior(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_bxor_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  ieor(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               ieor(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ieor(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_bshift_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  ishft(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               ishft(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ishft(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmul) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%r(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
          if(pm_mask_realdiv) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%r(v%offset:v%offset+esize)=&
                     arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)/&
                     arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
             end where
          else
             v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%r(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'divide by zero')
             goto 999
          endif
           v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%r(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          if(pm_mask_realmod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%r(v%offset:v%offset+esize)=mod(&
                     arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%r(v%offset:v%offset+esize)=mod(&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%r(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,'mod zero')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=mod(&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realpow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  max(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               max(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  min(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               min(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  -arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               -arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
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
   case(op_abs_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realabs) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  abs(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
  case(op_acos_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>1)) then
             errno=1
             call runtime_error(context,'Argument for acos not in range -1..1')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               acos(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>1.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,'Argument for acos not in range -1..1')
             goto 999
          endif
          if(pm_mask_realmath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%r(v%offset:v%offset+esize)=&
                     acos(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%r(v%offset:v%offset+esize)=&
                  acos(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(abs(arg(3)%data%r(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))>1)) then
             errno=1
             call runtime_error(context,'Argument for acos not in range -1..1')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  acos(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_asin_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>1)) then
             errno=1
             call runtime_error(context,'Argument for asin not in range -1..1')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               asin(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>1.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,'Argument for asin not in range -1..1')
             goto 999
          endif
          if(pm_mask_realmath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%r(v%offset:v%offset+esize)=&
                     asin(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%r(v%offset:v%offset+esize)=&
                  asin(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(abs(arg(3)%data%r(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))>1)) then
             errno=1
             call runtime_error(context,'Argument for asin not in range -1..1')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  asin(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_atan_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  atan(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               atan(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  atan(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_atan2_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  atan2(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               atan2(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  atan2(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_cos_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  cos(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               cos(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cos(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
     case(op_cosh_r)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0)))) then
             errno=1
             call runtime_error(context,&
                  'Argument for cosh would result in overflow')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               cosh(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0)).and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,&
                  'Argument for cosh would result in overflow')
             goto 999
          endif
          if(pm_mask_realmath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%r(v%offset:v%offset+esize)=&
                     cosh(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%r(v%offset:v%offset+esize)=&
                  cosh(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(abs(arg(3)%data%r(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))>=log(huge(1.0)))) then
             errno=1
             call runtime_error(context,&
                  'Argument for cosh would result in overflow')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cosh(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif

    case(op_exp_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>=&
               log(huge(1.0)))) then
             errno=1
             call runtime_error(context,'Argument for exp would result in overflow')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               exp(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>=&
               log(huge(1.0)).and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,'Argument for exp would result in overflow')
             goto 999
          endif
          if(pm_mask_realmath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%r(v%offset:v%offset+esize)=&
                     exp(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%r(v%offset:v%offset+esize)=&
                  exp(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(arg(3)%data%r(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))>=log(huge(1.0)))) then
             errno=1
             call runtime_error(context,'Argument for exp would result in overflow')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  exp(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
   case(op_log_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<=0)) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               log(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          if(pm_mask_realmath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%r(v%offset:v%offset+esize)=&
                     log(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%r(v%offset:v%offset+esize)=&
                  log(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(arg(3)%data%r(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))<=0)) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  log(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_log10_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<=0)) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               log10(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          if(pm_mask_realmath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%r(v%offset:v%offset+esize)=&
                     log10(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%r(v%offset:v%offset+esize)=&
                  log10(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(arg(3)%data%r(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))<=0)) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  log10(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
   case(op_sin_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  sin(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               sin(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sin(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_sinh_r)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0)))) then
             errno=1
             call runtime_error(context,&
                  'Argument for sinh would result in overflow')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               sinh(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0)).and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,&
                  'Argument for sinh would result in overflow')
             goto 999
          endif
          if(pm_mask_realmath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%r(v%offset:v%offset+esize)=&
                     sinh(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%r(v%offset:v%offset+esize)=&
                  sinh(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(abs(arg(3)%data%r(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))>=log(huge(1.0)))) then
             errno=1
             call runtime_error(context,&
                  'Argument for sinh would result in overflow')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sinh(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_sqrt_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<0)) then
             errno=1
             call runtime_error(context,'Negative argument to sqrt')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               sqrt(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,'Negative argument to sqrt')
             goto 999
          endif
          if(pm_mask_realmath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%r(v%offset:v%offset+esize)=&
                     sqrt(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%r(v%offset:v%offset+esize)=&
                  sqrt(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(arg(3)%data%r(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))<=0)) then
             errno=1
             call runtime_error(context,'Negative argument to sqrt')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sqrt(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_tan_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  tan(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               tan(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tan(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_tanh_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  tanh(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               tanh(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tanh(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_floor_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  floor(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               floor(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  floor(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_ceil_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  ceiling(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               ceiling(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ceiling(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemul) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
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
          if(pm_mask_doublediv) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%d(v%offset:v%offset+esize)=&
                     arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)/&
                     arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
             end where
          else
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          endif
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
          if(pm_mask_doublemod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%d(v%offset:v%offset+esize)=mod(&
                     arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%d(v%offset:v%offset+esize)=mod(&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
          endif
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublepow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  max(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               max(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  min(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               min(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  -arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               -arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublecmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublecmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublecmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublecmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
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
  case(op_abs_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleabs) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  abs(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
  case(op_acos_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>1)) then
             errno=1
             call runtime_error(context,'Argument for acos not in range -1..1')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               acos(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>1.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,'Argument for acos not in range -1..1')
             goto 999
          endif
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%d(v%offset:v%offset+esize)=&
                     acos(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%d(v%offset:v%offset+esize)=&
                  acos(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(abs(arg(3)%data%d(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))>1)) then
             errno=1
             call runtime_error(context,'Argument for acos not in range -1..1')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  acos(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_asin_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>1)) then
             errno=1
             call runtime_error(context,'Argument for asin not in range -1..1')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               asin(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>1.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,'Argument for asin not in range -1..1')
             goto 999
          endif
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%d(v%offset:v%offset+esize)=&
                     asin(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%d(v%offset:v%offset+esize)=&
                  asin(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(abs(arg(3)%data%d(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))>1)) then
             errno=1
             call runtime_error(context,'Argument for asin not in range -1..1')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  asin(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_atan_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  atan(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               atan(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  atan(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_atan2_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  atan2(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               atan2(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  atan2(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          end forall
       endif
    case(op_cos_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  cos(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               cos(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cos(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
     case(op_cosh_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0d0)))) then
             errno=1
             call runtime_error(context,&
                  'Argument for cosh would result in overflow')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               cosh(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0d0)).and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,&
                  'Argument for cosh would result in overflow')
             goto 999
          endif
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%d(v%offset:v%offset+esize)=&
                     cosh(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%d(v%offset:v%offset+esize)=&
                  cosh(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(abs(arg(3)%data%d(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))>=log(huge(1.0d0)))) then
             errno=1
             call runtime_error(context,&
                  'Argument for cosh would result in overflow')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cosh(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif

    case(op_exp_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)>=&
               log(huge(1.0d0)))) then
             errno=1
             call runtime_error(context,&
                  'Argument for exp would result in overflow')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               exp(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)>=&
               log(huge(1.0d0)).and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,&
                  'Argument for exp would result in overflow')
             goto 999
          endif
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%d(v%offset:v%offset+esize)=&
                     exp(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%d(v%offset:v%offset+esize)=&
                  exp(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(arg(3)%data%d(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))>=log(huge(1.0d0)))) then
             errno=1
             call runtime_error(context,&
                  'Argument for exp would result in overflow')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  exp(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
   case(op_log_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<=0)) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               log(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%d(v%offset:v%offset+esize)=&
                     log(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%d(v%offset:v%offset+esize)=&
                  log(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(arg(3)%data%d(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))<=0)) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  log(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_log10_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<=0)) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               log10(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%d(v%offset:v%offset+esize)=&
                     log10(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%d(v%offset:v%offset+esize)=&
                  log10(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(arg(3)%data%d(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))<=0)) then
             errno=1
             call runtime_error(context,'Non-positive argument to log')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  log10(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
   case(op_sin_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  sin(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               sin(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sin(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
     case(op_sinh_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0d0)))) then
             errno=1
             call runtime_error(context,&
                  'Argument for sinh would result in overflow')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               sinh(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0d0)).and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,&
                  'Argument for sinh would result in overflow')
             goto 999
          endif
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%d(v%offset:v%offset+esize)=&
                     sinh(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%d(v%offset:v%offset+esize)=&
                  sinh(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(abs(arg(3)%data%d(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))>=log(huge(1.0d0)))) then
             errno=1
             call runtime_error(context,&
                  'Argument for sinh would result in overflow')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sinh(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_sqrt_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<0)) then
             errno=1
             call runtime_error(context,'Negative argument to sqrt')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               sqrt(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,'Negative argument to sqrt')
             goto 999
          endif
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%d(v%offset:v%offset+esize)=&
                     sqrt(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%d(v%offset:v%offset+esize)=&
                  sqrt(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          if(any(arg(3)%data%d(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))<=0)) then
             errno=1
             call runtime_error(context,'Negative argument to sqrt')
             goto 999
          endif
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sqrt(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_tan_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  tan(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               tan(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tan(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_tanh_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  tanh(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               tanh(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tanh(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_floor_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  floor(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               floor(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  floor(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          end forall
       endif
    case(op_ceil_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  ceiling(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               ceiling(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ceiling(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).and.&
                  arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).and.&
               arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).or.&
                  arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).or.&
               arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
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
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  .not.arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               .not.arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
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

    if(pm_debug_level>2) then
       vm_depth=vm_depth+1
       write(*,*) spaces(1:min(10,vm_depth)*2),'CALL>',esize,&
            trim(pm_name_as_string(context,proc_get_name(func)))
    endif
    if(pm_debug_level>3) then
       write(*,*) '======CALL==NEW STACK======',&
            nargs,stack%data%esize,stack%offset
       do i=pm_stack_locals-1,nargs+pm_stack_locals-1
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

    goto 888

777 continue
    errno=-1
    
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
         if(k==0) then
            ind=pm_fast_tinyint(context,0_pm_p)
            return
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
         lbuffer(i-argstart+1)=arg(i)%data%l(arg(i)%offset)
      enddo
    end subroutine get_args_to_lbuffer

    subroutine get_args_to_ibuffer(argstart,argend,ibuffer)
      integer,intent(in):: argstart,argend
      integer,dimension(*),intent(out):: ibuffer
      integer:: i,n
      n=argend-argstart+1
      if(n<1.or.n>7) call pm_panic('args to lbuffer')
      do i=argstart,argend
         ibuffer(i-argstart+1)=arg(i)%data%ln(arg(i)%offset)
      enddo
    end subroutine get_args_to_ibuffer
    
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
    if(m>=0) then
       newve=pm_assign_new(context,newve,1_pm_ln,pm_long,n+4_pm_ln,.false.)
       newve%data%ln(newve%offset)=m
       newve%data%ln(newve%offset+1)=0
       newve%data%ln(newve%offset+2)=0
       newve%data%ln(newve%offset+3)=m
       newve%data%ln(newve%offset+4:newve%offset+3+n)=&
            arg(3)%data%ln(arg(3)%offset:arg(3)%offset+n-1)
       errno=pm_run(context,func,stack,newpc,-1_pm_i16,0_pm_i16,&
            arg,num_args)
    else
       errno=0
    endif
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
       prc=sync_find(any(p%data%l(p%offset:p%offset+m)))
       p=pm_fast_newnc(context,pm_long,1_pm_p)
       arg(3)%data%ptr(arg(3)%offset)=p
       p%data%ln(p%offset)=prc
    endif
  contains
    include 'fesize.inc'
    include 'fnewnc.inc'
  end function par_find

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
    integer:: junk
    junk=sync_status(pm_prc_error)
    call mesg_q_flush()
    call mesg_q_mess('Runtime error: '//trim(errmesg))
  end subroutine runtime_error

  subroutine dump_stack(context,dstack,dfunc,dpc)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: dstack,dfunc,dpc
    type(pm_ptr):: stack,func,pc
    integer(pm_p):: name
    character(len=80):: mess
    integer(pm_p):: line,modl
    character(len=20):: buffer
    stack=dstack
    func=dfunc
    pc=dpc
    call mesg_q_mess(&
         '=========================Call trace =========================')
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
       write(buffer,'(i20)') int(line)
       call mesg_q_mess(trim(mess)//'  line: '//trim(adjustl(buffer)))
       func=stack%data%ptr(stack%offset+pm_stack_func)
       pc=stack%data%ptr(stack%offset+pm_stack_pc)
       stack=stack%data%ptr(stack%offset+pm_stack_oldstack)
    enddo
    call mesg_q_mess(&
         '=============================================================')
  contains
    include 'fisnull.inc'
  end subroutine dump_stack

  subroutine vector_print_string(context,v,ve)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ve
    integer(pm_ln):: i,j,start,size
    type(pm_ptr):: vec,len,off,p
    vec=v%data%ptr(v%offset+pm_array_vect)
    len=v%data%ptr(v%offset+pm_array_length)
    off=v%data%ptr(v%offset+pm_array_offset)
    do i=0,pm_fast_esize(ve)
       j=ve%data%ln(ve%offset+i)
       start=off%data%ln(off%offset+j)
       size=len%data%ln(len%offset+j)
       p=vec%data%ptr(vec%offset+j)
       call mesg_q_print(context,p%data%s(p%offset+start:p%offset+start+size-1))
    enddo
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine vector_print_string

  
end module pm_vm
