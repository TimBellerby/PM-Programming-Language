!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2022
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

module pm_backend
  use pm_kinds
  use pm_sysdep
  use pm_compbase
  use pm_memory
  use pm_hash
  use pm_options
  use pm_lib
  use pm_symbol
  use pm_types
  use pm_vmdefs
  use pm_sysdefs
  use pm_array
  use pm_parlib
  implicit none

  ! Debugging (of VM)
  logical,private:: trace_opcodes=.false.
  logical,private:: trace_calls=.false.
  integer,private:: vm_depth=0
  character(len=20),private,parameter:: spaces='                    '

  integer,parameter:: loop_call_extra_args=2
  
contains

  ! ***********************************************
  ! Run PM program stored in funcs
  ! - hand-coded call to proc #0 with no args
  !************************************************
  subroutine pm_run_prog(context,funcs)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: funcs
    integer:: err
    type(pm_ptr),dimension(1):: arg
    type(pm_ptr),target:: ve
    
    context%funcs=funcs
    
    ! Create intial vector engine structure
    ve=make_simple_ve(context,1_pm_ln)
    context%null_ve=ve
    context%call_depth=0
    arg(1)=ve
    
    ! Run the code
    err=pm_run(context,pm_null_obj,pm_null_obj,&
         pm_null_obj,op_call,0,arg,1,0,.false.)
    if(err<=0.or.pm_main_process) then
       call mesg_q_cleanup()
    else
       call mesg_q_mess_finish()
    endif
    
  end subroutine pm_run_prog

  
  ! *************************************
  ! Run main interpreter loop
  ! - take context [funcin,stackin,pcin]
  ! - execute first instruction op op2 args
  ! *************************************
  recursive function pm_run(context,funcin,stackin,pcin,&
       op,op2,args,num_args,nesting,noexit) result(errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: funcin,stackin,pcin
    integer,intent(in):: op
    integer,intent(in):: op2
    integer,intent(in):: num_args,nesting
    logical,intent(in):: noexit
    type(pm_ptr),dimension(num_args),intent(in):: args
    integer:: errno
    type(pm_ptr),target,dimension(pm_max_args):: arg
    integer,target:: nargs
    type(pm_ptr),target:: stack,newve,new2
    type(pm_ptr):: func,pc,newstack,newfunc,newpc,ve,empty_vector
    type(pm_ptr):: p,q,v,w
    integer(pm_i16):: opcode,opcode3,oparg
    integer:: opcode2
    integer:: i,ii,n,t1,t2,start_arg
    integer(pm_ln):: j,jj,k,kk,m,esize
    integer(pm_ln),dimension(8):: ibuffer,ibuffer2
    logical,dimension(8):: lbuffer
    integer:: modl,line
    logical:: flip,ok,done
    integer:: stacksize
    type(pm_reg),pointer:: reg
    character(len=pm_comm_mess_len):: mess

    empty_vector=pm_fast_tinyint(context,0)
    
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
    opcode2=opcode2*pm_ext_mult+opcode3/(pm_max_args+1)
    n=iand(int(opcode3),pm_max_args)  ! Number arguments
    if(pm_debug_level>3) then
       write(*,*) sys_node,'DECODING>',opcode,&
            trim(op_names(opcode)),opcode2,n,&
            'pc=',pc%offset,'arg1=',pc%data%i16(pc%offset+3_pm_p)
       write(*,*) 'stack=',stack%data%hash,stack%offset,stack%data%esize
       call pm_dump_tree(context,6,&
            stack%data%ptr(stack%offset+pc%data%i16(pc%offset+3_pm_p)),2)
    endif

    if(trace_opcodes) then
       call proc_line_module(func,&
            max(int(pc%offset-func%data%ptr(func%offset)%offset)-4,1),line,modl)
       write(*,*) sys_node,op_names(opcode),opcode2,'(',n,' args)',&
            '@',trim(pm_name_as_string(context,modl)),'#',line
    endif
   
    oparg=pc%data%i16(pc%offset+3_pm_p)
    arg(1)=stack%data%ptr(stack%offset+oparg)
    if(pm_debug_checks) then
       if(pm_fast_vkind(arg(1))/=pm_pointer) then
          write(*,*) 'BAD VE!!!!',oparg,op_names(opcode),opcode2
          call proc_line_module(func,&
               max(int(pc%offset-func%data%ptr(func%offset)%offset)-4,1),line,modl)
          write(*,*) sys_node,op_names(opcode),&
               '@',trim(pm_name_as_string(context,modl)),'#',line
          call pm_dump_tree(context,6,arg(1),2)
          call pm_panic('Bed vector engine')
          goto 999
       endif
    endif
    if(opcode==op_comm_call) then
       oparg=pc%data%i16(pc%offset+4_pm_p)
       arg(2)=stack%data%ptr(stack%offset+oparg)
       ve=arg(2)%data%ptr(arg(2)%offset+1)
       esize=ve%data%ln(ve%offset)
       ve=arg(2)%data%ptr(arg(2)%offset)
       start_arg=3
    else
       ve=arg(1)%data%ptr(arg(1)%offset+1)
       esize=ve%data%ln(ve%offset)
       ve=arg(1)%data%ptr(arg(1)%offset)
       start_arg=2
    endif
    if(pm_debug_level>3) then
       write(*,*) 've.kind=',arg(1)%data%vkind,&
            've.vec.kind=',ve%data%vkind,'esize=',esize
    endif
    do i=start_arg,n
       oparg=pc%data%i16(pc%offset+i+2_pm_p)
       if(pm_debug_level>3) write(*,*) 'OPARG>', oparg,pm_max_stack,int(pm_max_stack,pm_i16)
       if(oparg>=0) then
          arg(i)=stack%data%ptr(stack%offset+oparg)
          if(pm_debug_level>3) then
             write(*,*) i,'STACK>>',oparg
             call pm_dump_tree(context,6,arg(i),2)
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
       
       ! Opcode that should not be skipped
       select case(opcode)
       case(op_and_ve:op_andnot_jmp_any,op_do_at)
          call set_arg(2,arg(1))
       case(op_jmp_any_ve_par)
          if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
          ok=sync_loop_end(.false.)
          if(ok) then
             pc%offset=pc%offset+opcode2-pm_jump_offset
          endif
       case(op_clone_ve)
          stack%data%ptr(stack%offset+opcode2)=arg(1)
       case(op_jmp)
          pc%offset=pc%offset+opcode2-pm_jump_offset
       case(op_skip_comms)
          !write(*,*) 'SKIP COMMS (empty)>>',esize
          if(esize==1) pc%offset=pc%offset+opcode2-pm_jump_offset
       case(op_head_node)
          if(par_frame(par_depth)%shared_node/=0) pc%offset=pc%offset+opcode2-pm_jump_offset
       case(op_remote_call:op_bcast_call,&
            op_dref,op_par_loop_end,op_chan,op_export,&
            op_export_param,op_pop_node,op_sync_mess,op_import_val,op_return)
          !write(*,*) 'unskip',op_names(opcode)
          goto 20
       case(op_recv_req_call,op_recv_assn_call)
          call set_arg(2,arg(1))
       case(op_skip_empty)
          if(opcode2>0) then
             if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
             ok=sync_loop_end(.false.)
             if(ok) then
                call set_arg(2,make_new_ve(pm_null_obj,arg(3)))
             else
                call set_arg(2,make_new_ve(empty_vector,arg(3)))
             endif
          else
             call set_arg(2,make_new_ve(pm_null_obj,arg(3)))
          endif
       case(op_comm_call)
          if(pm_fast_vkind(arg(1)%data%ptr(arg(1)%offset))/=pm_tiny_int) goto 20
       end select
       if(pm_debug_level>3) then
          write(*,*) 'SKIPPING>',opcode,&
               op_names(opcode),opcode2,opcode3,&
               'pc=',pc%offset,'esize=',esize
       endif
       goto 10
    endif

    20 continue
    if(pm_debug_level>3) then
       write(*,*) par_frame(par_depth)%this_node,'RUNNING>',opcode,&
            op_names(opcode),opcode2,opcode3,&
            'pc=',pc%offset,'esize=',esize,pm_fast_vkind(ve)
    endif
    
    select case(opcode)
    case(op_call)
       ! op_call #proc ve args...
       ! op_comm_call #proc ve args...
       if(opcode==op_comm_call) write(*,*) sys_node,'Comm_call',opcode2,pm_fast_vkind(ve)
       newfunc=context%funcs%data%ptr(&
            context%funcs%offset+opcode2)
       goto 30
    case(op_comm_call)
       ve=arg(2)%data%ptr(arg(2)%offset+1)
       esize=ve%data%ln(ve%offset)
       ve=arg(2)%data%ptr(arg(2)%offset)
       newfunc=context%funcs%data%ptr(&
            context%funcs%offset+opcode2)
       goto 30
    case(op_skip_empty)
       ! op_skip_empty #0_or_2 ve &newve
       ! op_skip_empty #1 ve &newve oldve
       if(opcode2==1) then
          if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
          ok=sync_loop_end(.true.)
          ve=make_new_ve(pm_null_obj,arg(3))
       else
          newve=vector_export_if_needed(context,ve,arg(1)%data%ptr(arg(1)%offset+1))
          if(opcode2==2) newve=import_vector(context,newve,arg(1)%data%ptr(arg(1)%offset+1))
          ve=make_new_ve(newve,arg(3))
       endif
       call set_arg(2,ve)
    case(op_return)
       ! op_return args...
       !if(sys_node==0) write(*,*) 'Return>',trim(pm_name_as_string(context,proc_get_name(func))),nargs
       if(nargs>0) then
          do i=2,nargs
             v=stack%data%ptr(stack%offset+pm_stack_locals+opcode2+i-1)
             v%data%ptr(v%offset)=arg(i)
             !if(sys_node==0) call pm_dump_tree(context,6,arg(i),2)
          enddo
       endif
       context%call_depth=context%call_depth-1
!!$      if(pm_debug_level>3.or.vm_depth==2) &
!!$           write(*,*) spaces(1:vm_depth*2),&
       if(trace_calls) write(*,*) 'RETURN>',trim(pm_name_as_string(context,proc_get_name(func)))
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
       ! op_jmp #where ve 
       pc%offset=pc%offset+opcode2-pm_jump_offset
    case(op_skip_comms)
       ! op_skip_comms
       !write(*,*) 'SKIP COMMS (full)',esize
       continue
    case(op_head_node)
       ! op_head_node #where
       if(par_frame(par_depth)%shared_node/=0) pc%offset=pc%offset+opcode2-pm_jump_offset
    case(op_and_ve:op_andnot_jmp_any)
       ! op_and_ve #where ve &newve mask
       ! ...
       flip=opcode==op_andnot_ve.or.opcode==op_andnot_jmp_any
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
       if(k==0) newve=empty_vector
       
       ! New vector engine structure
       ve=make_new_ve(newve,arg(1))
       call set_arg(2,ve)
       
       if(opcode>=op_and_jmp_any) then
          ! .. _jmp_any
          if(k>0) pc%offset=pc%offset+opcode2-pm_jump_offset
       endif
    case(op_chan)
       ! op_chan &chan_out
       ve=make_new_ve(pm_null_obj,arg(1))
       call set_arg(2,ve)
    case(op_active)
       ! op_active &vec_out
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          v%data%l(v%offset:v%offset+esize)=ve%data%l(ve%offset:ve%offset+esize)
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=.true.
       else
          v%data%l(v%offset:v%offset+esize)=.false.
          v%data%l(ve%data%ln(ve%offset:ve%offset+esize))=.true.
       endif
    case(op_jmp_any_ve,op_jmp_any_ve_par)
       ! op_jmp_any_ve #where ve ve1 ve2...
       ! op_jmp_any_ve_par #where ve ve1 ve2...
       ok=.false.
       do i=2,nargs
          if(pm_fast_vkind(arg(i)%data%ptr(arg(i)%offset))&
               /=pm_tiny_int) then
             ok=.true.
             exit
          endif
       enddo
       if(opcode==op_jmp_any_ve_par) then
          if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
          ok=sync_loop_end(ok)
       endif
       if(ok) then
          pc%offset=pc%offset+opcode2-pm_jump_offset
       endif
    case(op_jmp_noshare)
       ! op_jmp_noshare #where
       if(par_frame(par_depth)%shared_nnode<=1) then
          pc%offset=pc%offset+opcode2-pm_jump_offset
       endif
    case(op_par_loop)
       ! op_par_loop ????
       errno=par_loop(context,func,stack,pc,arg,nargs,ve,esize,nesting,noexit)
       if(errno/=0) then
          call dump_stack(context,stack,func,pc,ve)
          goto 888
       endif
    case(op_par_loop_end)
       ! op_par_loop_end
       errno=0
       goto 888

    case(op_sys_node)
       ! op_sys_node ve &out
       ibuffer(1)=sys_node
       call fill_args_from_ibuffer(2,2,ibuffer)
    case(op_sys_nnode)
       ! op_sys_nnode ve &out
       ibuffer(1)=sys_nnode
       call fill_args_from_ibuffer(2,2,ibuffer)
    case(op_this_node)
       ! op_this_node ve &out
       ibuffer(1)=par_frame(par_depth)%this_node
       call fill_args_from_ibuffer(2,2,ibuffer)
    case(op_this_nnode)
       ! op_this_nnode ve &out
       ibuffer(1)=par_frame(par_depth)%this_nnode
       call fill_args_from_ibuffer(2,2,ibuffer)
    case(op_shared_node)
       ! op_shared_node ve &out
       ibuffer(1)=par_frame(par_depth)%shared_node
       call fill_args_from_ibuffer(2,2,ibuffer)
    case(op_shared_nnode)
       ! op_shared_nnode ve &out
       ibuffer(1)=par_frame(par_depth)%shared_nnode
       call fill_args_from_ibuffer(2,2,ibuffer)
    case(op_is_shared)
       ! op_is_shared ve &out
       lbuffer(1)=par_frame(par_depth)%is_shared
       call fill_args_from_lbuffer(2,2,lbuffer)
    case(op_root_node)
       ! op_root_node ve &out
       ibuffer(1)=par_frame(par_depth)%root_node
       call fill_args_from_ibuffer(2,2,ibuffer)
    case(op_is_par)
       ! op_is_par ve &out
       lbuffer(1)=conc_depth==0
       call fill_args_from_lbuffer(2,2,lbuffer)
    case(op_push_node_grid)
       ! op_push_node_grid ve mask1..maskN dim1..dimN
       n=(nargs-1)/2
       call get_args_to_lbuffer(0_pm_ln,2,n+1,lbuffer)
       call get_args_to_ibuffer(0_pm_ln,n+2,n+n+1,ibuffer)
       call push_node_grid(context,lbuffer,n,ibuffer)
    case(op_push_node_split)
       ! op_push_node_split ve colours
       call push_node_split(context,&
            int(arg(2)%data%ln(arg(2)%offset)))
    case(op_push_node_distr)
       ! op_push_node_distr ve
       call push_node_distr(context)
    case(op_push_node_conc)
       ! op_push_node_conc ve  -- OBSOLETE?
       conc_depth=conc_depth+1
    case(op_pop_node_conc)
       ! op_pop_node_conc ve   -- OBSOLETE?
       conc_depth=conc_depth-1
    case(op_push_node_back)
       ! op_push_node_back ve
       call push_node_back(context)
    case(op_pop_off_node)
       ! op_pop_off_node ve
       call pop_off_node(context)
    case(op_pop_node)
       ! op_pop_node ve
       if(conc_depth==0) then
          if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       endif
       call pop_node(context)
    case(op_broadcast)
       ! op_broadcast ve vec prc
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       call broadcast(context,int(arg(3)%data%ln(arg(3)%offset)),arg(2))
    case(op_broadcast_shared)
       ! op_broadcast_shared ve vec
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       call broadcast(context,0,arg(2),&
            par_frame(par_depth)%shared_comm,par_frame(par_depth)%shared_node)
    case(op_broadcast_val)
       ! op_broadcast_val ve &vec_out vec_in prc
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       call set_arg(2,broadcast_val(context,&
            int(arg(4)%data%ln(arg(4)%offset)),arg(3),j))
    case(op_get_remote)
       ! op_get_remote ve &outvec vec prc offset   - OBSOLETE?
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       newve=arg(1)%data%ptr(arg(1)%offset+1)
       esize=newve%data%ln(newve%offset)
       v=empty_copy_vector(context,arg(3),esize+1)
       call set_arg(2,v)
       newve=arg(1)%data%ptr(arg(1)%offset)
       if(pm_fast_vkind(newve)==pm_logical) &
            newve=shrink_ve(context,newve,pm_fast_esize(newve))
       call get_remote(context,arg(4),&
            arg(5),&
            arg(3),v,newve,errno)
       if(errno/=0) goto 997
    case(op_put_remote)
       ! op_put_remote ve &outvec vec prc offset  - OBSOLETE?
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       newve=arg(1)%data%ptr(arg(1)%offset)
       if(pm_fast_vkind(newve)==pm_logical) &
            newve=shrink_ve(context,newve,pm_fast_esize(newve))
       call put_remote(context,arg(4),&
            arg(5),&
            arg(2),&
            arg(3),newve,errno)
       if(errno/=0) goto 997
    case(op_get_remote_distr)
       ! op_get_remote_distr ve &outvec array prc offset 
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       newve=arg(1)%data%ptr(arg(1)%offset+1)
       esize=newve%data%ln(newve%offset)
       w=arg(3)
       w=w%data%ptr(w%offset+pm_array_vect)
       w=w%data%ptr(w%offset)
       v=empty_copy_vector(context,w,esize+1)
       call set_arg(2,v)
       newve=arg(1)%data%ptr(arg(1)%offset)
       if(pm_fast_vkind(newve)==pm_logical) &
            newve=shrink_ve(context,newve,pm_fast_esize(newve))
       call get_remote(context,arg(4),&
            arg(5),&
            w,v,newve,errno)
       if(errno/=0) goto 997
    case(op_put_remote_distr)
       ! op_get_remote_distr ve &outvec array prc offset 
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       newve=arg(2)%data%ptr(arg(2)%offset)
       if(pm_fast_vkind(newve)==pm_logical) &
            newve=shrink_ve(context,newve,pm_fast_esize(newve))
       w=arg(2)
       w=w%data%ptr(w%offset+pm_array_vect)
       w=w%data%ptr(w%offset)
       call put_remote(context,arg(4),&
            arg(5),&
            w,arg(3),&
            newve,errno)
       if(errno/=0) goto 997
    case(op_gather)
       ! op_gather ve &vec1 ... &vecN -- vecs both in and out
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       j=par_frame(par_depth)%this_nnode
       stack%data%ptr(stack%offset+opcode2)=&
            make_simple_ve(context,j)
       do i=2,nargs
          v=arg(i)%data%ptr(arg(i)%offset)
          new2=empty_copy_vector(context,v,j)
          call gather(context,v,new2,j)
          call set_arg(i,new2)
       enddo
    case(op_isend_offset,op_isend_grid)
       ! op_isend_offset ve offsets prc array
       !!! Do not sync status as number of ops can vary between procs
       v=arg(4)%data%ptr(&
            arg(4)%offset&
            +pm_array_vect)
       v=v%data%ptr(v%offset)
       call isend_disp(&
            int(arg(3)%data%&
            ln(arg(3)%offset)),&
            v,&
            arg(2),0_pm_ln,&
            1+pm_fast_esize(arg(2)),&
            data_tag)
    case(op_irecv_offset,op_irecv_grid)
       ! op_irecv_offset ve offsets prc array
       !!! Do not sync status as number of ops can vary between procs
       v=arg(4)%data%ptr(&
            arg(4)%offset&
            +pm_array_vect)
       v=v%data%ptr(v%offset)
       ok=.false.
       call irecv_disp(context,&
            int(arg(3)%data%&
            ln(arg(3)%offset)),&
            v,arg(2),0_pm_ln,&
            1+pm_fast_esize(arg(2)),&
            data_tag,ok)
       if(ok) then
          call runtime_error(context,func,pc,ve,noexit,'cannot run irecv_offset on composite')
          goto 999
       endif
    case(op_recv_offset,op_recv_grid,op_recv_offset_resend,op_recv_grid_resend)
       ! op_recv_offset ve offsets prc array
       !!! Do not sync status as number of ops can vary between procs
       v=arg(4)%data%ptr(&
            arg(4)%offset&
            +pm_array_vect)
       v=v%data%ptr(v%offset)
       m=1+pm_fast_esize(arg(2))
       call recv_disp(context,&
            int(arg(3)%data%&
            ln(arg(3)%offset)),&
            v,arg(2),0_pm_ln,&
            m,data_tag)
       if(opcode==op_recv_offset_resend.or.opcode==op_recv_grid_resend) then
          call pm_panic('should not be resending')
          do ii=1,par_frame(par_depth)%shared_nnode-1
             n=get_shared(i)
             call isend_disp(&
                  n,v,&
                  arg(2),0_pm_ln,&
                  1+pm_fast_esize(arg(2)),&
                  data_tag)
          enddo
       endif
    case(op_bcast_shared_offset,op_bcast_shared_grid)
       v=arg(3)%data%ptr(&
            arg(3)%offset&
            +pm_array_vect)
       v=v%data%ptr(v%offset)
       m=1+pm_fast_esize(arg(2))
       call broadcast_disp(context,&
            0,v,arg(2),0_pm_ln,&
            m,xcomm=par_frame(par_depth)%shared_comm)
    case(op_isend)
       ! op_isend ve prc array
       v=arg(3)%data%ptr(&
            arg(3)%offset&
            +pm_array_vect)
       v=v%data%ptr(v%offset)
       call isend(int(arg(2)%data%&
            ln(arg(2)%offset)),v,&
            data_tag)
    case(op_isend_reply)
       ! op_isend_reply ve prc vect
       call rsend(context,int(arg(2)%data%ln(arg(2)%offset)),&
            arg(3),data_tag)
    case(op_irecv)
       ! op_irecv ve prc array
       v=arg(3)%data%ptr(&
            arg(3)%offset&
            +pm_array_vect)
       v=v%data%ptr(v%offset)
       call irecv(int(arg(2)%data%&
            ln(arg(2)%offset)),v,&
            data_tag,ok)
       if(ok) then
          call runtime_error(context,func,pc,ve,noexit,'cannot run irecv on composite')
           goto 999
        endif
     case(op_recv)
        ! op_recv ve prc array
        v=arg(3)%data%ptr(&
            arg(3)%offset&
            +pm_array_vect)
        v=v%data%ptr(v%offset)
        call recv(context,int(arg(2)%data%&
             ln(arg(2)%offset)),v,&
             data_tag)
     case(op_isend_req,op_recv_reply,op_isend_assn)
        !!! Do not sync status as number of ops can vary between procs
        
        ! op_isend_req      ve offsets prc vec &vecout     [ mask ] 
        ! op_recv_reply     ve offsets prc &vec            [ mask ]
        ! op_isend_assn     ve offsets prc vec_lhs vec_rhs [ mask ]
        i=merge(4,5,opcode==op_recv_reply)
        if(nargs>i) then
           v=arg(2)
           w=arg(i+1)
           m=0
           do j=0,pm_fast_esize(arg(2))
              if(w%data%l(w%offset+v%data%ln(v%offset+j))) then
                 v%data%ln(v%offset+m)=v%data%ln(v%offset+j)
                 m=m+1
              endif
           enddo
        elseif(pm_fast_vkind(arg(2))==pm_long) then
           m=1+pm_fast_esize(arg(2))
        else
           w=arg(2)%data%ptr(arg(2)%offset+3)
           m=w%data%ln(w%offset)
        endif
        if(opcode==op_isend_req.or.opcode==op_isend_assn) then
           call isend_num(&
                int(arg(3)%data%&
                ln(arg(3)%offset)),&
                m,req_tag)
           call isend_disp(&
                int(arg(3)%data%&
                ln(arg(3)%offset)),&
                arg(4),&
                arg(2),0_pm_ln,&
                m,extra_req_tag)
           if(m>0) then
              if(opcode==op_isend_assn) then
                 call isend_val_disp(&
                      int(arg(3)%data%&
                      ln(arg(3)%offset)),&
                      arg(5),&
                      arg(2),0_pm_ln,&
                      m,extra_req_tag)
              else
                 call irecv_disp(context,&
                      int(arg(3)%data%&
                      ln(arg(3)%offset)),&
                      arg(5),&
                      arg(2),0_pm_ln,&
                      m,data_tag,ok)
              endif
           endif
        elseif(opcode==op_recv_reply) then
           call recv_rest_disp(context,&
                int(arg(3)%data%&
                ln(arg(3)%offset)),&
                arg(4),&
                arg(2),0_pm_ln,&
                m,data_tag)
        else !! op_broadcast_disp
           call broadcast_disp(context,&
                int(arg(3)%data%&
                ln(arg(3)%offset)),&
                arg(4),&
                arg(2),0_pm_ln,&
                m)
        endif
     case(op_recv_req_call,op_recv_assn_call)
        !!! Do not sync status as number of ops can vary between procs

        ! op_recv_req_call  ve new_ve &node_out &vec_lhs_out vec_lhs_in 
        ! op_recv_assn_call ve new_ve &node_out &vec_lhs_out vec_lhs_in &vec_rhs_out
        
        i=message_pending_node(req_tag)
        call recv_num(context,i,j,req_tag)
        esize=j-1
        v=alloc_arg(pm_long,3)
        v%data%ln(v%offset:v%offset+esize)=i
        if(j>0) then
           v=copy_dref(context,arg(5),j,.false.)
           call set_arg(4,v)
           call recv(context,i,v,extra_req_tag)
           if(opcode==op_recv_assn_call) then
              w=recv_val(context,i,extra_req_tag)
              call set_arg(6,w)
              if(opcode2==1) then
                 do ii=1,par_frame(par_depth)%shared_nnode-1
                    n=get_shared(ii)
                    call isend(n,v,extra_req_tag,&
                         par_frame(par_depth)%this_comm)
                    call isend_val(n,w,0_pm_ln,j,extra_req_tag,&
                         par_frame(par_depth)%this_comm)
                 enddo
              endif
           endif
           call set_arg(2,make_simple_ve(context,j))
        endif
        call set_arg(2,make_simple_ve(context,j))
     case(op_remote_call)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       v=arg(6)
       if(pm_fast_vkind(ve)/=pm_tiny_int) w=empty_copy_vector(context,v%data%ptr(v%offset+1),esize+1)
       arg(5)%data%ptr(arg(5)%offset)=w
       errno=0
       if(remote_call(context,arg(7),&
            w,arg(6),arg(8),func,stack,pc,arg,&
            pc%offset+4,remote_call_block,arg(1),&
            .false.,errno,opcode2/=0)) then
          i=sync_status(pc,pm_node_error)
          call mesg_q_flush()
          goto 999
       else
          if(errno/=0) goto 997
       endif
    case(op_remote_send_call)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       v=arg(6)
       errno=0
       if(remote_call(context,arg(7),&
            arg(8),arg(6),pm_null_obj,func,stack,pc,arg,&
            pc%offset+4,remote_call_block,arg(1),&
            .true.,errno,opcode2/=0)) then
          i=sync_status(pc,pm_node_error)
          call mesg_q_flush()
          goto 999
       elseif(errno/=0) then
          goto 997
       endif
    case(op_server_call)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       v=arg(6)
       w=empty_copy_vector(context,v%data%ptr(v%offset+1),esize+1)
       arg(5)%data%ptr(arg(5)%offset)=w
       ve=shrink_ve(context,ve,esize)
       errno=0
       if(collect_call(context,int(arg(7)%data%ln(arg(7)%offset)),&
            w,arg(6),arg(8),func,stack,pc,arg,&
            pc%offset+4,remote_call_block,arg(1),&
            ve,.false.,errno)) then
          i=sync_status(pc,pm_node_error)
          call mesg_q_flush()
          goto 999
       elseif(errno/=0) then
          goto 997
       endif
    case(op_collect_call)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       v=arg(6)
       ve=shrink_ve(context,ve,esize)
       errno=0
       if(collect_call(context,int(arg(7)%data%ln(arg(7)%offset)),&
            arg(8),arg(6),pm_null_obj,func,stack,pc,arg,&
            pc%offset+4,remote_call_block,arg(1),&
            ve,.true.,errno,opcode2==1)) then
          i=sync_status(pc,pm_node_error)
          call mesg_q_flush()
          goto 999
       elseif(errno/=0) then
          goto 997
       endif
    case(op_bcast_call)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       if(pm_fast_vkind(ve)==pm_tiny_int) then
          call set_arg(3,broadcast_val_disp(context,&
               newve,0_pm_ln,0_pm_ln,&
               int(arg(7)%data%ln(arg(7)%offset)),arg(5),j))
          call set_arg(4,broadcast_val_disp(context,&
               newve,0_pm_ln,0_pm_ln,&
               int(arg(7)%data%ln(arg(7)%offset)),arg(6),j))
       else
          newve=ve
          if(pm_fast_vkind(newve)/=pm_long) newve=shrink_ve(context,newve,esize)
          call set_arg(3,broadcast_val_disp(context,&
               newve,0_pm_ln,pm_fast_esize(newve)+1,&
               int(arg(7)%data%ln(arg(7)%offset)),arg(5),j))
          call set_arg(4,broadcast_val_disp(context,&
               newve,0_pm_ln,pm_fast_esize(newve)+1,&
               int(arg(7)%data%ln(arg(7)%offset)),arg(6),j))
       endif
       call set_arg(2,make_simple_ve(context,j))
    case(op_do_at)
       newve=make_simple_ve(context,arg(3)%data%ln(arg(3)%offset))
       newve%data%ptr(newve%offset)=arg(4)
       call set_arg(2,newve)
    case(op_sync_mess)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       call complete_messages(context)

       
       ! ******** Distibuted file operations *******
    case(op_open_file)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       v=alloc_arg(pm_int,2)
       w=alloc_arg(pm_int,3)
       p=arg(4)%data%ptr(arg(4)%offset+pm_array_vect)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          call get_args_to_lbuffer(j,5,11,lbuffer)
          v%data%i(v%offset+j)=pm_file_open(p%data%ptr(p%offset+j),&
               lbuffer(1),lbuffer(2),lbuffer(3),lbuffer(4),lbuffer(5),lbuffer(6),lbuffer(7),&
               w%data%i(w%offset+j))
       enddo
    case(op_close_file)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       w=alloc_arg(pm_int,2)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          call pm_file_close(arg(3)%data%i(arg(3)%offset+j),w%data%i(w%offset+j))
       enddo
    case(op_seek_file)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       w=alloc_arg(pm_int,2)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          call pm_file_seek(arg(3)%data%i(arg(3)%offset+j),&
               arg(4)%data%lln(arg(4)%offset+j),w%data%i(w%offset+j))
       enddo
    case(op_read_file)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       w=alloc_arg(pm_int,2)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          call pm_file_read(arg(3)%data%i(arg(3)%offset+j),arg(4),j,1_pm_ln,&
               w%data%i(w%offset+j))
       enddo
    case(op_write_file)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       w=alloc_arg(pm_int,2)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          call pm_file_write(arg(3)%data%i(arg(3)%offset+j),arg(4),j,1_pm_ln,&
               w%data%i(w%offset+j))
       enddo
    case(op_read_file_array)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       v=arg(4)%data%ptr(arg(4)%offset+pm_array_vect)
       w=alloc_arg(pm_int,2)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          call pm_file_read(arg(3)%data%i(arg(3)%offset+j),v%data%ptr(v%offset+j),&
               0_pm_ln,arg(5)%data%ln(arg(5)%offset+j),w%data%i(w%offset+j))
       enddo
    case(op_write_file_array)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       w=alloc_arg(pm_int,2)
       v=arg(4)%data%ptr(arg(4)%offset+pm_array_vect)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          call pm_file_write(arg(3)%data%i(arg(3)%offset+j),v%data%ptr(v%offset+j),&
               0_pm_ln,arg(5)%data%ln(arg(5)%offset+j),w%data%i(w%offset+j))
       enddo
    case(op_read_file_tile)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       w=alloc_arg(pm_int,2)
       call pm_file_read_disps(&
            arg(3)%data%i(arg(3)%offset),&
            arg(4),arg(5),&
            pm_fast_esize(arg(4))+1,&
            arg(6)%data%ln(arg(6)%offset),&
            ii)
       w%data%i(w%offset:w%offset+esize)=ii
    case(op_write_file_tile)
       if(sync_status(pc,pm_node_running)==pm_node_error) goto 777
       w=alloc_arg(pm_int,2)
       call pm_file_write_disps(&
            arg(3)%data%i(arg(3)%offset),&
            arg(4),arg(5),&
            pm_fast_esize(arg(4))+1,&
            arg(6)%data%ln(arg(6)%offset),&
            ii)
       w%data%i(w%offset:w%offset+esize)=ii
    case(op_io_error_string)
       newve=shrink_ve(context,ve,esize)
       call set_arg(2,vector_make_string(context,&
            newve,arg(3),fmt_i_width,pm_file_error_string))

       ! **** Support for basic operations ******
    case(op_setref)
       if(nargs==3) then
          call set_arg(2,arg(3))
       else
          ii=(nargs-1)/2
          do i=1,ii
             call set_arg(i+1,arg(i+1+ii))
          enddo
       endif
    case(op_nullify)
       call set_arg(2,pm_null_obj)
    case(op_clone_ve)
       stack%data%ptr(stack%offset+opcode2)=arg(1)
    case(op_logical_return)
       v=alloc_arg(pm_logical,2)
       v%data%l(v%offset:v%offset+esize)=opcode2/=0
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
       arg(2)%data%ptr(arg(2)%offset)=empty_vector
    case(op_print)
       if(par_frame(par_depth)%shared_node==0.or.opcode2==1) then
          newve=shrink_ve(context,ve,esize)
          call vector_print_string(context,arg(2),newve)
       endif
    case(op_dump)
       write(*,*) '====================== dump ======================', par_frame(par_depth)%this_node
       call vector_dump(context,arg(2),1)
       write(*,*) '====='
       call pm_dump_tree(context,6,arg(2),2)
       write(*,*) '----'
       call pm_dump_tree(context,6,arg(1),2)
       write(*,*) '=================================================='
    case(op_dump_id)
       write(*,*) '====================== dumpid ======================'
       write(*,*) arg(2)%data%hash,arg(2)%offset
       write(*,*) '=================================================='
    case(op_new_dump)
       newpc=stack%data%ptr(stack%offset+pm_stack_pc)
       newfunc=stack%data%ptr(stack%offset+pm_stack_func)
       ! Below needs to be properly integrated with messaging system
       !call print_module_and_line(context,newfunc,newpc,ve,printit=.true.)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          call vector_print_single_string(context,arg(2),j)
          call vector_dump_to(context,arg(3),j,mesg_q_print_str,2)
       enddo
    case(op_show)
       call print_module_and_line(context,func,pc,ve,printit=.true.)
    case(op_show_stack)
       call mesg_q_print_str(context,&
            '=================================================================')
       call print_module_and_line(context,func,pc,ve,printit=.true.)
            call mesg_q_print_str(context,&
            '-----------------------------------------------------------------')
       call dump_stack(context,stack,func,pc,ve,printit=.true.)
       call mesg_q_print_str(context,&
            '=================================================================')
    case(op_concat)
       newve=shrink_ve(context,ve,esize)
       call set_arg(2,vector_concat_string(context,newve,&
            arg(3),arg(4)))
    case(op_clone)
       call set_arg(2,copy_vector(context,arg(3),ve,0_pm_ln,esize+1))
    case(op_assign)
       errno=0
       call vector_assign(context,arg(2),arg(3),ve,errno,esize)
       if(errno/=0) goto 997
    case(op_struct)
       v=pm_fast_newusr(context,pm_struct_type,int(nargs,pm_p))
       call set_arg(2,v)
       v%data%ptr(v%offset+1_pm_p)=&
            pm_fast_tinyint(context,opcode2)
       v%data%ptr(v%offset+2:v%offset+nargs-1)=arg(3:nargs)
    case(op_rec)
       v=pm_fast_newusr(context,pm_rec_type,int(nargs,pm_p))
       call set_arg(2,v)
       v%data%ptr(v%offset+1_pm_p)=&
            pm_fast_tinyint(context,opcode2)
       v%data%ptr(v%offset+2:v%offset+nargs-1)=arg(3:nargs)
    case(op_check)
       if(pm_fast_isnull(ve)) then
          if(.not.all(arg(3)%data%l(arg(3)%offset:&
               arg(3)%offset+esize))) then
             newve=shrink_ve(context,ve,esize)
             call vector_get_string(context,arg(2),newve,first_false(newve,arg(3)),mess)
             call runtime_error(context,func,pc,ve,noexit,mess)
             errno=1
             goto 999
          endif
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and..not.&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize))) then
             newve=shrink_ve(context,ve,esize)
             call vector_get_string(context,arg(2),newve,first_false(newve,arg(3)),mess)
             call runtime_error(context,func,pc,ve,noexit,mess)
             errno=1
             goto 999 
          endif
       else
          if(.not.all(arg(3)%data%l(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))) then
             call vector_get_string(context,arg(2),ve,first_false(ve,arg(3)),mess)
             call runtime_error(context,func,pc,ve,noexit,mess)
             errno=1
             goto 999 
          endif
       endif
    case(op_elem)
       if(pm_fast_typeof(arg(3))==pm_elemref_type) then
          call set_arg(2,elem_ref_get_struct_elem(context,arg(3),opcode2,esize))
       else
          call set_arg(2,arg(3)%data%ptr(arg(3)%offset+opcode2))
       endif
    case(op_chan_array_elem)
       call set_arg(2,array_get_struct_elem(context,arg(3),opcode2,0))
    case(op_chan_array_vect)
       v=arg(3)%data%ptr(arg(3)%offset+pm_array_vect)
       call set_arg(2,v%data%ptr(v%offset))
    case(op_elem_ref)
       v=pm_fast_newnc(context,pm_usr,6)
       arg(2)%data%ptr(arg(2)%offset)=v
       v%data%ptr(v%offset)=arg(3)%data%ptr(arg(3)%offset)
       w=arg(3)%data%ptr(arg(3)%offset+1)
       v%data%ptr(v%offset+1)=w%data%ptr(w%offset+opcode2)
       v%data%ptr(v%offset+2)=arg(3)
       v%data%ptr(v%offset+3:v%offset+5)=arg(3)%data%ptr(arg(3)%offset+3:arg(3)%offset+5)
    case(op_make_rf)
       if(pm_main_process) then
          if(pm_fast_esize(arg(3)%data%ptr(arg(3)%offset+pm_array_vect))/=pm_fast_esize(arg(4))) then
             write(*,*) pm_fast_esize(arg(3)%data%ptr(arg(3)%offset+pm_array_vect)),pm_fast_esize(arg(4))
              call runtime_error(context,func,pc,ve,noexit,&
                   'Internal error: op_make_rf')
             goto 999
          endif
       endif
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

    case(op_dref)
       v=pm_fast_newusr(context,&
            merge(pm_dref_type,pm_dref_shared_type,opcode2==0),&
            int(6,pm_p))
       if(pm_fast_vkind(ve)==pm_tiny_int) then
          v%data%ptr(v%offset+2)=arg(4)
       else
          if(nargs==5) then
             v%data%ptr(v%offset+1:v%offset+3)=arg(3:5)
             v%data%ptr(v%offset+4:v%offset+5)=&
                  arg(4)%data%ptr(arg(4)%offset+4:arg(4)%offset+5)
          else
             v%data%ptr(v%offset+1:v%offset+nargs-2)=arg(3:nargs)
          endif
       endif
       call set_arg(2,v)
!!$       if(sys_node==0) then
!!$          write(*,*) 'dref',sys_node
!!$          call pm_dump_tree(context,6,v,2)
!!$       endif
    case(op_import_dref)
       v=arg(1)%data%ptr(arg(1)%offset)
       if(.not.pm_fast_istiny(v)) then
          call set_arg(2,&
               copy_dref(context,arg(3),esize+1,.true.,arg(1)))
       endif
       
    case(op_array)
       call set_arg(2,make_array_dim(context,opcode2,&
            arg(3),arg(4),arg(5),ve))
    case(op_var_array)
       call set_arg(2,make_array_vdim(context,opcode2,&
            arg(3),arg(4),arg(5),ve))
    case(op_array_get_elem)
       errno=0
       if(pm_fast_isnull(arg(4))) goto 999
       call set_arg(2,array_index(context,arg(3),&
            arg(4),ve,esize,errno))
       if(errno/=0) goto 997
    case(op_array_set_elem)
       errno=0
       call array_set_index(context,arg(2),arg(3),arg(4),ve,errno)
       if(errno/=0) goto 997
    case(op_get_dom)
       call set_arg(2,array_dom(context,arg(3),esize))
    case(op_get_size)
       if(pm_fast_typeof(arg(3))/=pm_array_type.and.&
            pm_fast_typeof(arg(3))/=pm_const_array_type) then
          call runtime_error(context,func,pc,ve,noexit,&
               trim(pm_typ_as_string(context,int(pm_fast_typeof(arg(3))))))
          goto 999
       endif
       call set_arg(2,array_size(context,arg(3),esize))
    case(op_make_array)
       call set_arg(2,make_array_from_vect(context,opcode2,&
            arg(3),arg(4),esize,arg(1)%data%ptr(arg(1)%offset+1)))
    case(op_redim)
       arg(5)=arg(3)%data%ptr(arg(3)%offset+pm_array_vect)
       arg(6)=arg(4)
       call set_arg(2,array_redim(context,opcode2,arg(3),arg(4)))

       
    case(op_iota)
       if(nargs==7) then
          call set_arg(2,vector_iota(context,&
               arg(3),arg(4),arg(5),arg(6),&
               arg(7)%data%ptr(arg(7)%offset+1)))
       else
          call set_arg(2,vector_iota_trunc(context,&
               arg(3),arg(4),arg(5),arg(6),arg(7),arg(8),&
               arg(9)%data%ptr(arg(9)%offset+1)))
       endif
    case(op_block_cyclic)
       call make_block_cyclic
    case(op_indices)
       v=alloc_arg(pm_long,2)
       call vector_indices(arg(1)%data%ptr(arg(1)%offset+1),v)
       
    case(op_import_val)
       call set_arg(2,import_vector(context,&
            arg(3),arg(1)%data%ptr(arg(1)%offset+1)))
    case(op_import_back)
       new2=import_vector(context,&
            arg(3),arg(1)%data%ptr(arg(1)%offset+1))
       call vector_assign(context,arg(2),new2,pm_null_obj,errno,esize)
    case(op_import_varg)
       new2=pm_fast_new(context,pm_pointer,&
            int(pm_fast_esize(arg(3)),pm_p)+1_pm_p)
       do j=0,pm_fast_esize(arg(3))
!!$          write(*,*) 'J=',j
!!$          call pm_dump_tree(context,6,arg(1),2)
!!$          call pm_dump_tree(context,6,arg(3),2)
          call pm_ptr_assign(context,new2,j,import_vector(context,&
               arg(3)%data%ptr(arg(3)%offset+j),&
               arg(1)%data%ptr(arg(1)%offset+1)))
       enddo
       call set_arg(2,new2)
    case(op_import_scalar)
       call set_arg(2,vector_from_scalar(context,arg(3),0_pm_ln,esize,.true.))
    case(op_export)
       call export_vector(context,arg(2),arg(3),arg(1)%data%ptr(arg(1)%offset+1))
    case(op_export_param)
!!$       write(*,*) 'Export'
!!$       call pm_dump_tree(context,6,arg(3),2)
       call  set_arg(2,export_vector_as_new(context,arg(3),arg(1)%data%ptr(arg(1)%offset+1),ve))
    case(op_extractelm)
       call set_arg(2,get_elem_ref(context,arg(3),esize,errno))
       if(errno/=0) goto 997
       
 
    case(op_make_poly)
       newve=shrink_ve(context,ve,esize)
       call set_arg(2,poly_new(context,arg(3),newve,esize))
    case(op_make_type_val)
       call set_arg(2,pm_fast_typeno(context,opcode2))
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
          newve=empty_vector
       else
          kk=count(newve%data%l(newve%offset+k:newve%offset+esize))
          if(esize+1>kk*pm_shrink_thresh) then
             newve=shrink_ve(context,newve,esize,kk)
          endif
          new2=empty_copy_vector(context,&
               arg(4)%data%ptr(arg(4)%offset+k),esize+1)
          errno=0
          call poly_get(context,new2,arg(4),newve,esize,errno)
          if(errno/=0) then
             goto 997
          endif
          call set_arg(3,new2)
       endif
       ve=make_new_ve(newve,arg(1))
       call set_arg(2,ve)
    case(op_as)
       errno=0
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
       call set_arg(2,array_pack(context,arg(3),opcode2,arg(4),arg(5),arg(6)))
    case(op_advance)
       call set_arg(2,advance(context,arg(3),arg(4)))
      
    case(op_get_dims)
       n=(nargs-2)/2
       call alloc_args_to_long(2,n+1)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          call get_args_to_ibuffer(j,n+3,n+n+2,ibuffer)
          call get_dims(int(arg(n+2)%data%ln(arg(n+2)%offset)),n,ibuffer)
          call set_args_from_ibuffer(j,2,n+1,ibuffer)
       end do
    case(op_wshare)
       v=alloc_arg(pm_long,2)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          w=arg(3)%data%ptr(arg(3)%offset+pm_array_vect)
          w=w%data%ptr(w%offset+j)
          v%data%ln(v%offset+j)=wshare(&
               w%data%ln(w%offset:w%offset+pm_fast_esize(w)),&
               arg(4)%data%ln(arg(4)%offset+j),arg(5)%data%ln(arg(5)%offset+j),&
               arg(6)%data%ln(arg(6)%offset+j))
       enddo
    case(op_intersect_seq)
       call alloc_args_to_long(2,5)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          call get_args_to_ibuffer(j,6,13,ibuffer)
          call intersect_seq(ibuffer(1),ibuffer(2),ibuffer(3),&
               ibuffer(4),ibuffer(5),ibuffer(6),ibuffer(7),ibuffer(8),&
               ibuffer2(1),ibuffer2(2),ibuffer2(3),ibuffer2(4))
          call set_args_from_ibuffer(j,2,5,ibuffer2)
       enddo
    case(op_intersect_aseq)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          v=arg(3)%data%ptr(arg(3)%offset+pm_array_vect)
          v=v%data%ptr(v%offset+j)
          k=arg(4)%data%ln(arg(4)%offset)
          w=arg(5)%data%ptr(arg(5)%offset+pm_array_vect)
          w=w%data%ptr(w%offset+j)
          kk=arg(6)%data%ln(arg(6)%offset)
          p=arg(7)
          p=p%data%ptr(p%offset+pm_array_vect)
          p=p%data%ptr(p%offset+j)
          if(opcode2==0) then
             call intersect_aseq(v%data%ln(v%offset:),k,w%data%ln(w%offset:),kk,&
                  p%data%ln(p%offset:),m)
          elseif(opcode2==1) then
             call overlap_aseq(v%data%ln(v%offset:),k,w%data%ln(w%offset:),kk,&
                  p%data%ln(p%offset:),m)
          else
             q=arg(8)
             q=q%data%ptr(q%offset+pm_array_vect)
             q=q%data%ptr(q%offset+j)
             call overlap_aseq2(v%data%ln(v%offset:),k,w%data%ln(w%offset:),kk,&
                  p%data%ln(p%offset:),q%data%ln(q%offset),m)
          endif
          p=arg(2)
          p%data%ln(p%offset+j)=m
       enddo
    case(op_includes_aseq)
       p=alloc_arg(pm_logical,2)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          v=arg(3)%data%ptr(arg(3)%offset+pm_array_vect)
          v=v%data%ptr(v%offset+j)
          k=arg(4)%data%ln(arg(4)%offset)
          w=arg(5)%data%ptr(arg(5)%offset+pm_array_vect)
          w=w%data%ptr(w%offset+j)
          kk=arg(6)%data%ln(arg(6)%offset)
          p%data%l(p%offset+j)=aseq_includes(v%data%ln(v%offset:),k,w%data%ln(w%offset:),kk)
       enddo
    case(op_index_aseq,op_in_aseq)
       p=alloc_arg(merge(pm_logical,pm_long,opcode==op_in_aseq),2)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          v=arg(3)%data%ptr(arg(3)%offset+pm_array_vect)
          v=v%data%ptr(v%offset+j)
          k=arg(4)%data%ln(arg(4)%offset)
          kk=arg(5)%data%ln(arg(5)%offset+j)
          m=aseq_index(v%data%ln(v%offset:),k,kk)
          if(opcode==op_index_aseq) then
             p%data%ln(p%offset+j)=m
          else
             if(m<0.or.m>=k) then
                p%data%l(p%offset+j)=.false.
             else
                p%data%l(p%offset+j)=v%data%ln(v%offset+m)==kk
             endif
          endif
       enddo
    case(op_expand_aseq)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          v=arg(3)%data%ptr(arg(3)%offset+pm_array_vect)
          v=v%data%ptr(v%offset+j)
          k=arg(4)%data%ln(arg(4)%offset)
          w=arg(5)%data%ptr(arg(5)%offset+pm_array_vect)
          w=w%data%ptr(w%offset+j)
          call expand_aseq(v%data%ln(v%offset:),k,arg(6)%data%ln(arg(6)%offset+j),&
               arg(7)%data%ln(arg(7)%offset+j),w%data%ln(w%offset:),m)
          p=arg(2)
          p%data%ln(p%offset+j)=m
       enddo
       
    case(op_intersect_bseq)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          v=arg(3)%data%ptr(arg(3)%offset+pm_array_vect)
          v=v%data%ptr(v%offset+j)
          if(opcode2==0) then
             call intersect_bseq(arg(4)%data%ln(arg(4)%offset+j),arg(5)%data%ln(arg(5)%offset+j),&
                  arg(6)%data%ln(arg(6)%offset+j),arg(7)%data%ln(arg(7)%offset+j),&
                  arg(8)%data%ln(arg(8)%offset+j),arg(9)%data%ln(arg(9)%offset+j),&
                  arg(10)%data%ln(arg(10)%offset+j),arg(11)%data%ln(arg(11)%offset+j),&
                  arg(12)%data%ln(arg(12)%offset+j),arg(13)%data%ln(arg(13)%offset+j),&
                  v%data%ln(v%offset:),m)
          elseif(opcode2==1) then
             call overlap_bseq(arg(4)%data%ln(arg(4)%offset+j),arg(5)%data%ln(arg(5)%offset+j),&
                  arg(6)%data%ln(arg(6)%offset+j),arg(7)%data%ln(arg(7)%offset+j),&
                  arg(8)%data%ln(arg(8)%offset+j),arg(9)%data%ln(arg(9)%offset+j),&
                  arg(10)%data%ln(arg(10)%offset+j),arg(11)%data%ln(arg(11)%offset+j),&
                  arg(12)%data%ln(arg(12)%offset+j),arg(13)%data%ln(arg(13)%offset+j),&
                  v%data%ln(v%offset:),m)
          else
             w=arg(4)%data%ptr(arg(4)%offset+pm_array_vect)
             w=w%data%ptr(w%offset+j)
             call overlap_bseq2(arg(5)%data%ln(arg(5)%offset+j),&
                  arg(6)%data%ln(arg(6)%offset+j),arg(7)%data%ln(arg(7)%offset+j),&
                  arg(8)%data%ln(arg(8)%offset+j),arg(9)%data%ln(arg(9)%offset+j),&
                  arg(10)%data%ln(arg(10)%offset+j),arg(11)%data%ln(arg(11)%offset+j),&
                  arg(12)%data%ln(arg(12)%offset+j),arg(13)%data%ln(arg(13)%offset+j),&
                  arg(14)%data%ln(arg(14)%offset+j),&
                  v%data%ln(v%offset:),w%data%ln(w%offset:),m)
          endif
          p=arg(2)
          p%data%ln(p%offset+j)=m
       enddo
    case(op_gcd)
       v=alloc_arg(pm_long,2)
       do jj=0,merge(esize,pm_fast_esize(ve),pm_fast_vkind(ve)/=pm_long)
          if(pm_fast_vkind(ve)==pm_null) then
             j=jj
          elseif(pm_fast_vkind(ve)==pm_logical) then
             j=jj
             if(.not.ve%data%l(ve%offset+jj)) cycle
          else
             j=ve%data%ln(ve%offset+jj)
          endif
          call extended_gcd(arg(3)%data%ln(arg(3)%offset+j),arg(4)%data%ln(arg(4)%offset+j),&
               k,kk,v%data%ln(v%offset+j))
       enddo

       
    case(op_eq)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_long) then
          v%data%l(v%offset:v%offset+esize)=.false.
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=.true.
          enddo
       else
          v%data%l(v%offset:v%offset+esize)=.true.
       endif
       call vector_eq(context,arg(3),arg(4),v,esize,ve)
    case(op_ne)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_long) then
          v%data%l(v%offset:v%offset+esize)=.false.
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=.true.
          enddo
       else
          v%data%l(v%offset:v%offset+esize)=.true.
       endif
       call vector_eq(context,arg(3),arg(4),v,esize,ve)
       if(pm_fast_vkind(ve)==pm_long) then
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)= .not.v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)
          enddo
       else
          v%data%l(v%offset:v%offset+esize)=&
               .not.v%data%l(v%offset:v%offset+esize)
       endif
       
    case(op_string_i)
       newve=shrink_ve(context,ve,esize)
       call set_arg(2,vector_make_string(context,&
            newve,arg(3),fmt_i_width,fmt_i))
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
          do j=0,pm_fast_esize(ve)
             arg(2)%data%i(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%i(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
   case(op_divide_i)
      v=alloc_arg(pm_int,2)
      if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
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
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
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
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mod_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          if(pm_mask_intmod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%i(v%offset:v%offset+esize)=modulo(&
                     arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%i(v%offset:v%offset+esize)=modulo(&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%i(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          v%data%i(v%offset:v%offset+esize)=modulo(&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
       else
          if(any(arg(4)%data%i(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=modulo(&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=abs(&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=not(&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  iand(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ior(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ieor(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ishft(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pdiff_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  dim(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               dim(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  dim(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_sign_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  sign(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               sign(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sign(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_modulo_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  mod(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i(v%offset:v%offset+esize)=&
               mod(arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  mod(arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_i8_i)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=(&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               (arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i16_i)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=(&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               (arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i32_i)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=(&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               (arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i64_i)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=(&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               (arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
   case(op_offset_i)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=(&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               (arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif

       
    case(op_string_ln)
       newve=shrink_ve(context,ve,esize)   
       call set_arg(2,&
            vector_make_string(context,&
            newve,arg(3),fmt_ln_width,fmt_ln))
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
          do j=0,pm_fast_esize(ve)
             arg(2)%data%ln(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%ln(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(op_add_ln)
       esize=pm_fast_esize(arg(3))
       v=alloc_arg(pm_long,2)
       !if(pm_fast_vkind(arg(3))/=pm_long.or.pm_fast_vkind(arg(4))/=pm_long) goto 999
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_sub_ln)
       if(pm_fast_vkind(arg(3))/=pm_long) goto 999
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
   case(op_divide_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%ln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          if(pm_mask_longmul.or..true.) then
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
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
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
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mod_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%ln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          if(pm_mask_longmod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%ln(v%offset:v%offset+esize)=&
                     modulo(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%ln(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%ln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          v%data%ln(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
       else
          if(any(arg(4)%data%ln(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  modulo(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=abs(&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=not(&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  iand(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ior(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ieor(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ishft(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pdiff_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  dim(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               dim(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  dim(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_sign_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  sign(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               sign(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sign(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_modulo_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  mod(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%ln(v%offset:v%offset+esize)=&
               mod(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  mod(arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_i8_ln)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=(&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               (arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i16_ln)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=(&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               (arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i32_ln)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=(&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               (arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i64_ln)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=(&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               (arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
   case(op_offset_ln)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=(&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               (arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif


    case(op_string_offset)
       newve=shrink_ve(context,ve,esize)   
       call set_arg(2,&
            vector_make_string(context,&
            newve,arg(3),fmt_lln_width,fmt_lln))
    case(op_assign_offset)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%lln(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%lln(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             arg(2)%data%lln(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%lln(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(op_add_offset)
       esize=pm_fast_esize(arg(3))
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_sub_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mult_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmul) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
   case(op_divide_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%lln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          if(pm_mask_longmul) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%lln(v%offset:v%offset+esize)=&
                     arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)/&
                     arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
             end where
          else
             v%data%lln(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%lln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          v%data%lln(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
       else
          if(any(arg(4)%data%lln(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mod_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%lln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          if(pm_mask_longmod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%lln(v%offset:v%offset+esize)=&
                     modulo(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%lln(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%lln(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          v%data%lln(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
       else
          if(any(arg(4)%data%lln(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  modulo(arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pow_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longpow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_max_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  max(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               max(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
   case(op_min_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  min(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               min(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
   case(op_uminus_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  -arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               -arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_eq_offset)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ne_offset)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_gt_offset)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ge_offset)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
         v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_int_offset)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_long_offset)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_real_offset)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_double_offset)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_abs_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longabs) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=abs(&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               abs(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=abs(&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_bnot_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=not(&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               not(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=not(&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_band_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  iand(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               iand(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  iand(arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bor_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  ior(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               ior(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ior(arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bxor_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  ieor(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               ieor(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ieor(arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bshift_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  ishft(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               ishft(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ishft(arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pdiff_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  dim(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               dim(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  dim(arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_sign_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  sign(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               sign(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sign(arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_modulo_offset)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  mod(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%lln(v%offset:v%offset+esize)=&
               mod(arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%lln(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  mod(arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_i8_offset)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=(&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               (arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i16_offset)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=(&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               (arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i32_offset)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=(&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               (arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i64_offset)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=(&
                  arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               (arg(3)%data%lln(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%lln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif       


    case(op_assign_i8)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%i8(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%i8(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             arg(2)%data%i8(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%i8(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(op_add_i8)
       esize=pm_fast_esize(arg(3))
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_sub_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mult_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmul) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
   case(op_divide_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i8(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          if(pm_mask_longmul) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%i8(v%offset:v%offset+esize)=&
                     arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)/&
                     arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
             end where
          else
             v%data%i8(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%i8(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          v%data%i8(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
       else
          if(any(arg(4)%data%i8(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mod_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i8(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          if(pm_mask_longmod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%i8(v%offset:v%offset+esize)=&
                     modulo(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%i8(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%i8(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          v%data%i8(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
       else
          if(any(arg(4)%data%i8(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  modulo(arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pow_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longpow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_max_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  max(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               max(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
   case(op_min_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  min(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               min(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
   case(op_uminus_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  -arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               -arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_eq_i8)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ne_i8)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_gt_i8)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ge_i8)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
         v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_int_i8)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_long_i8)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_offset_i8)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%lln(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_real_i8)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_double_i8)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_abs_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longabs) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=abs(&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               abs(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=abs(&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_bnot_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=not(&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               not(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=not(&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_band_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  iand(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               iand(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  iand(arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bor_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  ior(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               ior(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ior(arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bxor_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  ieor(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               ieor(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ieor(arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bshift_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  ishft(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               ishft(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ishft(arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pdiff_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  dim(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               dim(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  dim(arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_sign_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  sign(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               sign(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sign(arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_modulo_i8)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=&
                  mod(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               mod(arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i8(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  mod(arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_i16_i8)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=(&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               (arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i32_i8)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=(&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               (arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i64_i8)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=(&
                  arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               (arg(3)%data%i8(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i8(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif     


    case(op_assign_i16)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%i16(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%i16(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             arg(2)%data%i16(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%i16(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(op_add_i16)
       esize=pm_fast_esize(arg(3))
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_sub_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mult_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmul) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
   case(op_divide_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i16(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          if(pm_mask_longmul) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%i16(v%offset:v%offset+esize)=&
                     arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)/&
                     arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
             end where
          else
             v%data%i16(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%i16(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          v%data%i16(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
       else
          if(any(arg(4)%data%i16(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mod_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i16(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          if(pm_mask_longmod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%i16(v%offset:v%offset+esize)=&
                     modulo(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%i16(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%i16(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          v%data%i16(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
       else
          if(any(arg(4)%data%i16(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  modulo(arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pow_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longpow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_max_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  max(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               max(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
   case(op_min_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  min(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               min(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
   case(op_uminus_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  -arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               -arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_eq_i16)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ne_i16)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_gt_i16)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ge_i16)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
         v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_int_i16)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_long_i16)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_offset_i16)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%lln(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_real_i16)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_double_i16)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_abs_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longabs) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=abs(&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               abs(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=abs(&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_bnot_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=not(&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               not(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=not(&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_band_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  iand(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               iand(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  iand(arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bor_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  ior(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               ior(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ior(arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bxor_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  ieor(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               ieor(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ieor(arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bshift_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  ishft(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               ishft(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ishft(arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pdiff_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  dim(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               dim(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  dim(arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_sign_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  sign(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               sign(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sign(arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_modulo_i16)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=&
                  mod(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               mod(arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i16(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  mod(arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_i8_i16)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=(&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               (arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i32_i16)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=(&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               (arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i64_i16)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=(&
                  arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               (arg(3)%data%i16(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i16(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif     


    case(op_assign_i32)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%i32(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%i32(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             arg(2)%data%i32(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%i32(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(op_add_i32)
       esize=pm_fast_esize(arg(3))
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_sub_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mult_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmul) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
   case(op_divide_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i32(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          if(pm_mask_longmul) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%i32(v%offset:v%offset+esize)=&
                     arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)/&
                     arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
             end where
          else
             v%data%i32(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%i32(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          v%data%i32(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
       else
          if(any(arg(4)%data%i32(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mod_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i32(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          if(pm_mask_longmod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%i32(v%offset:v%offset+esize)=&
                     modulo(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%i32(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%i32(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          v%data%i32(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
       else
          if(any(arg(4)%data%i32(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  modulo(arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pow_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longpow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_max_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  max(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               max(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
   case(op_min_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  min(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               min(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
   case(op_uminus_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  -arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               -arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_eq_i32)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ne_i32)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_gt_i32)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ge_i32)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
         v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_int_i32)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_long_i32)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_offset_i32)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%lln(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_real_i32)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_double_i32)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_abs_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longabs) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=abs(&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               abs(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=abs(&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_bnot_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=not(&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               not(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=not(&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_band_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  iand(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               iand(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  iand(arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bor_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  ior(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               ior(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ior(arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bxor_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  ieor(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               ieor(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ieor(arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bshift_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  ishft(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               ishft(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ishft(arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pdiff_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  dim(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               dim(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  dim(arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_sign_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  sign(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               sign(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sign(arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_modulo_i32)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=&
                  mod(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               mod(arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i32(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  mod(arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_i8_i32)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=(&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               (arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i16_i32)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=(&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               (arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i64_i32)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=(&
                  arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               (arg(3)%data%i32(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i32(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif     


    case(op_string_i64)
       newve=shrink_ve(context,ve,esize)   
       call set_arg(2,&
            vector_make_string(context,&
            newve,arg(3),fmt_i64_width,fmt_i64))
    case(op_assign_i64)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%i64(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%i64(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             arg(2)%data%i64(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%i64(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(op_add_i64)
       esize=pm_fast_esize(arg(3))
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_sub_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mult_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmul) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
   case(op_divide_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i64(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          if(pm_mask_longmul) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%i64(v%offset:v%offset+esize)=&
                     arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)/&
                     arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
             end where
          else
             v%data%i64(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%i64(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          v%data%i64(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
       else
          if(any(arg(4)%data%i64(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mod_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%i64(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          if(pm_mask_longmod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%i64(v%offset:v%offset+esize)=&
                     modulo(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%i64(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%i64(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          v%data%i64(v%offset:v%offset+esize)=&
               modulo(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
       else
          if(any(arg(4)%data%i64(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  modulo(arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pow_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longpow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_max_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  max(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               max(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
   case(op_min_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  min(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               min(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
   case(op_uminus_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  -arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               -arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_eq_i64)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ne_i64)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_gt_i64)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ge_i64)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longcmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
         v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_int_i64)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_long_i64)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_offset_i64)
       v=alloc_arg(pm_longlong,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%lln(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_real_i64)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_double_i64)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_abs_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longabs) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=abs(&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               abs(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=abs(&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_bnot_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=not(&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               not(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=not(&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_band_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  iand(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               iand(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  iand(arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bor_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  ior(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               ior(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ior(arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bxor_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  ieor(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               ieor(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ieor(arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_bshift_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_longbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  ishft(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               ishft(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ishft(arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pdiff_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  dim(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               dim(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  dim(arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_sign_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  sign(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               sign(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sign(arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_modulo_i64)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i64(v%offset:v%offset+esize)=&
                  mod(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i64(v%offset:v%offset+esize)=&
               mod(arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%i64(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i64(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  mod(arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_i8_i64)
       v=alloc_arg(pm_int8,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i8(v%offset:v%offset+esize)=(&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i8(v%offset:v%offset+esize)=&
               (arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i8(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i16_i64)
       v=alloc_arg(pm_int16,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i16(v%offset:v%offset+esize)=(&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i16(v%offset:v%offset+esize)=&
               (arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i16(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_i32_i64)
       v=alloc_arg(pm_int32,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_intbits) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i32(v%offset:v%offset+esize)=(&
                  arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%i32(v%offset:v%offset+esize)=&
               (arg(3)%data%i64(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%i32(ve%data%ln(ve%offset+j)+&
                  v%offset)=(&
                  arg(3)%data%i64(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
       
       
       

    case(op_string_r)
       newve=shrink_ve(context,ve,esize)
       call set_arg(2,vector_make_string(context,&
            newve,arg(3),fmt_r_width,fmt_r))
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
          do j=0,pm_fast_esize(ve)
             arg(2)%data%r(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%r(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_divide_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%r(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
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
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
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
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mod_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%r(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          if(pm_mask_realmod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%r(v%offset:v%offset+esize)=modulo(&
                     arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%r(v%offset:v%offset+esize)=modulo(&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          endif
       elseif(pm_fast_vkind(ve)/=pm_long) then
          if(any(arg(4)%data%r(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=modulo(&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
       else
          if(any(arg(4)%data%r(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=modulo(&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_long_r)
       v=alloc_arg(pm_int64,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_offset_r)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%lln(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_complex_r)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%c(v%offset:v%offset+esize)=&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_complex2_r)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=cmplx(&
                  arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=cmplx(&
               arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=cmplx(&
                  arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  abs(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
  case(op_acos_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>1)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Argument for acos not in range -1..1')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               acos(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>1.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Argument for acos not in range -1..1')
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
             call runtime_error(context,func,pc,ve,noexit,'Argument for acos not in range -1..1')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  acos(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_asin_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>1)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Argument for asin not in range -1..1')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               asin(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>1.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Argument for asin not in range -1..1')
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
             call runtime_error(context,func,pc,ve,noexit,'Argument for asin not in range -1..1')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  asin(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  atan(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  atan2(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cos(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
     case(op_cosh_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0)))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,&
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
             call runtime_error(context,func,pc,ve,noexit,&
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
             call runtime_error(context,func,pc,ve,noexit,&
                  'Argument for cosh would result in overflow')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cosh(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif

    case(op_exp_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>=&
               log(huge(1.0)))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Argument for exp would result in overflow')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               exp(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)>=&
               log(huge(1.0)).and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Argument for exp would result in overflow')
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
             call runtime_error(context,func,pc,ve,noexit,'Argument for exp would result in overflow')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  exp(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
   case(op_log_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<=0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               log(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
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
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  log(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_log10_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<=0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               log10(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
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
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  log10(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sin(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_sinh_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0)))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,&
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
             call runtime_error(context,func,pc,ve,noexit,&
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
             call runtime_error(context,func,pc,ve,noexit,&
                  'Argument for sinh would result in overflow')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sinh(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_sqrt_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Negative argument to sqrt')
             goto 999
          endif
          v%data%r(v%offset:v%offset+esize)=&
               sqrt(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Negative argument to sqrt')
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
             call runtime_error(context,func,pc,ve,noexit,'Negative argument to sqrt')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sqrt(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tan(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tanh(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  floor(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ceiling(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_modulo_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  mod(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               mod(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  mod(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_sign_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  sign(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               sign(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sign(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pdiff_r)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  dim(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               dim(arg(3)%data%r(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  dim(arg(3)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
       
       
    case(op_string_d)
       newve=shrink_ve(context,ve,esize)
       call set_arg(2,vector_make_string(context,&
            newve,arg(3),fmt_d_width,fmt_d))
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
          do j=0,pm_fast_esize(ve)
             arg(2)%data%d(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%d(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_divide_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%d(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
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
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
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
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mod_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%d(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=modulo(&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%d(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          if(pm_mask_doublemod) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%d(v%offset:v%offset+esize)=modulo(&
                     arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                     arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
             end where
          else
             v%data%d(v%offset:v%offset+esize)=modulo(&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
          endif
       else
          if(any(arg(4)%data%d(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'mod zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=modulo(&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  max(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  min(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
   case(op_offset_d)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%lln(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%lln(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%lln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_complex_d)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%dc(v%offset:v%offset+esize)=&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_complex2_d)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=cmplx(&
                  arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize),kind=pm_d)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=cmplx(&
               arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize),kind=pm_d)
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=cmplx(&
                  arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset),kind=pm_d)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  abs(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
  case(op_acos_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>1)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Argument for acos not in range -1..1')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               acos(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>1.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Argument for acos not in range -1..1')
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
             call runtime_error(context,func,pc,ve,noexit,'Argument for acos not in range -1..1')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  acos(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_asin_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>1)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Argument for asin not in range -1..1')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               asin(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>1.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Argument for asin not in range -1..1')
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
             call runtime_error(context,func,pc,ve,noexit,'Argument for asin not in range -1..1')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  asin(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  atan(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  atan2(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cos(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
     case(op_cosh_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0d0)))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,&
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
             call runtime_error(context,func,pc,ve,noexit,&
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
             call runtime_error(context,func,pc,ve,noexit,&
                  'Argument for cosh would result in overflow')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cosh(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif

    case(op_exp_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)>=&
               log(huge(1.0d0)))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,&
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
             call runtime_error(context,func,pc,ve,noexit,&
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
             call runtime_error(context,func,pc,ve,noexit,&
                  'Argument for exp would result in overflow')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  exp(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
   case(op_log_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<=0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               log(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
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
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  log(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_log10_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<=0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               log10(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
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
             call runtime_error(context,func,pc,ve,noexit,'Non-positive argument to log')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  log10(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sin(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
     case(op_sinh_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(abs(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))>=&
               log(huge(1.0d0)))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,&
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
             call runtime_error(context,func,pc,ve,noexit,&
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
             call runtime_error(context,func,pc,ve,noexit,&
                  'Argument for sinh would result in overflow')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sinh(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_sqrt_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Negative argument to sqrt')
             goto 999
          endif
          v%data%d(v%offset:v%offset+esize)=&
               sqrt(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize)<=0.and.&
               ve%data%l(ve%offset:ve%offset+esize))) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'Negative argument to sqrt')
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
             call runtime_error(context,func,pc,ve,noexit,'Negative argument to sqrt')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sqrt(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tan(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tanh(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  floor(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  ceiling(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_modulo_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  mod(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               mod(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  mod(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_sign_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  sign(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               sign(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sign(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif
    case(op_pdiff_d)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_realmaxmin) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  dim(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%d(v%offset:v%offset+esize)=&
               dim(arg(3)%data%d(arg(3)%offset:arg(3)%offset+esize),&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  dim(arg(3)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset))
          enddo
       endif


    case(op_assign_c)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%c(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%c(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             arg(2)%data%c(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%c(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(op_add_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_sub_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mult_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemul) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_divide_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%c(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          v%data%c(v%offset:v%offset+esize)=&
               arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%c(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          if(pm_mask_doublediv) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%c(v%offset:v%offset+esize)=&
                     arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)/&
                     arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
             end where
          else
             v%data%c(v%offset:v%offset+esize)=&
                  arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
          endif
       else
          if(any(arg(4)%data%c(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_rpow_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublepow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%r(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%r(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_pow_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublepow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_uminus_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  -arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               -arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_eq_c)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublecmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ne_c)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublecmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%c(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_real_c)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%r(v%offset:v%offset+esize)=&
               real(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  real(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  real(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
  case(op_abs_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleabs) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  abs(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               abs(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  abs(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
  case(op_acos_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%c(v%offset:v%offset+esize)=&
               acos(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%c(v%offset:v%offset+esize)=&
                     acos(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%c(v%offset:v%offset+esize)=&
                  acos(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  acos(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_asin_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%c(v%offset:v%offset+esize)=&
               asin(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%c(v%offset:v%offset+esize)=&
                     asin(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%c(v%offset:v%offset+esize)=&
                  asin(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  asin(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_atan_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  atan(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               atan(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  atan(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_cos_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  cos(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               cos(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cos(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
     case(op_cosh_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%c(v%offset:v%offset+esize)=&
               cosh(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%c(v%offset:v%offset+esize)=&
                     cosh(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%c(v%offset:v%offset+esize)=&
                  cosh(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cosh(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif

    case(op_exp_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%c(v%offset:v%offset+esize)=&
               exp(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%c(v%offset:v%offset+esize)=&
                     exp(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%c(v%offset:v%offset+esize)=&
                  exp(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  exp(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
   case(op_log_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%c(v%offset:v%offset+esize)=&
               log(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%c(v%offset:v%offset+esize)=&
                     log(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%c(v%offset:v%offset+esize)=&
                  log(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  log(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
   case(op_sin_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  sin(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               sin(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sin(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
     case(op_sinh_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%c(v%offset:v%offset+esize)=&
               sinh(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%c(v%offset:v%offset+esize)=&
                     sinh(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%c(v%offset:v%offset+esize)=&
                  sinh(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sinh(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_sqrt_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%c(v%offset:v%offset+esize)=&
               sqrt(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%c(v%offset:v%offset+esize)=&
                     sqrt(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%c(v%offset:v%offset+esize)=&
                  sqrt(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sqrt(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_tan_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  tan(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               tan(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tan(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_tanh_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  tanh(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               tanh(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tanh(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
  
    case(op_imag_c)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  aimag(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               aimag(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  aimag(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_conj_c)
       v=alloc_arg(pm_single_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%c(v%offset:v%offset+esize)=&
                  conjg(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%c(v%offset:v%offset+esize)=&
               conjg(arg(3)%data%c(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%c(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  conjg(arg(3)%data%c(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif


       
    case(op_assign_dc)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%dc(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%dc(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             arg(2)%data%dc(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%dc(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(op_add_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_sub_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_mult_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemul) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_divide_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          if(any(arg(4)%data%dc(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          v%data%dc(v%offset:v%offset+esize)=&
               arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and.&
               arg(4)%data%dc(arg(4)%offset:&
               arg(4)%offset+esize)==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          if(pm_mask_doublediv) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%dc(v%offset:v%offset+esize)=&
                     arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)/&
                     arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
             end where
          else
             v%data%dc(v%offset:v%offset+esize)=&
                  arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
          endif
       else
          if(any(arg(4)%data%dc(arg(4)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve)))==0)) then
             errno=1
             call runtime_error(context,func,pc,ve,noexit,'divide by zero')
             goto 999
          endif
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_dpow_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublepow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%d(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%d(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
       
    case(op_pow_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublepow) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)**&
                  arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)**&
               arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)**&
                  arg(4)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_uminus_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleadd) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  -arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               -arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  -arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
       endif
    case(op_eq_dc)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublecmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)==&
                  arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)==&
               arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)==&
                  arg(4)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ne_dc)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublecmp) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)/=&
                  arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize)/=&
               arg(4)%data%dc(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/=&
                  arg(4)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_real_dc)
       v=alloc_arg(pm_double,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%d(v%offset:v%offset+esize)=&
               real(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize),kind=pm_d)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%d(v%offset:v%offset+esize)=&
                  real(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize),kind=pm_d)
          end where
       else
          do j=0,pm_fast_esize(ve)
             v%data%d(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  real(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset),kind=pm_d)
          enddo
       endif
  case(op_abs_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doubleabs) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  abs(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               abs(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  abs(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
  case(op_acos_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%dc(v%offset:v%offset+esize)=&
               acos(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%dc(v%offset:v%offset+esize)=&
                     acos(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%dc(v%offset:v%offset+esize)=&
                  acos(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  acos(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_asin_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%dc(v%offset:v%offset+esize)=&
               asin(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%dc(v%offset:v%offset+esize)=&
                     asin(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%dc(v%offset:v%offset+esize)=&
                  asin(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  asin(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_atan_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  atan(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               atan(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  atan(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_cos_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  cos(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               cos(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cos(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
     case(op_cosh_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%dc(v%offset:v%offset+esize)=&
               cosh(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%dc(v%offset:v%offset+esize)=&
                     cosh(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%dc(v%offset:v%offset+esize)=&
                  cosh(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  cosh(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif

    case(op_exp_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%dc(v%offset:v%offset+esize)=&
               exp(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%dc(v%offset:v%offset+esize)=&
                     exp(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%dc(v%offset:v%offset+esize)=&
                  exp(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  exp(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
   case(op_log_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%dc(v%offset:v%offset+esize)=&
               log(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%dc(v%offset:v%offset+esize)=&
                     log(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%dc(v%offset:v%offset+esize)=&
                  log(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  log(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
   case(op_sin_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  sin(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               sin(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sin(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
     case(op_sinh_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%dc(v%offset:v%offset+esize)=&
               sinh(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%dc(v%offset:v%offset+esize)=&
                     sinh(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%dc(v%offset:v%offset+esize)=&
                  sinh(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sinh(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_sqrt_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%dc(v%offset:v%offset+esize)=&
               sqrt(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(pm_mask_doublemath) then
             where(ve%data%l(ve%offset:ve%offset+esize))
                v%data%dc(v%offset:v%offset+esize)=&
                     sqrt(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
             end where
          else
             v%data%dc(v%offset:v%offset+esize)=&
                  sqrt(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          endif
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  sqrt(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_tan_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  tan(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               tan(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tan(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_tanh_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  tanh(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               tanh(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  tanh(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_imag_dc)
       v=alloc_arg(pm_single,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%r(v%offset:v%offset+esize)=&
                  aimag(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%r(v%offset:v%offset+esize)=&
               aimag(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%r(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  aimag(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif
    case(op_conj_dc)
       v=alloc_arg(pm_double_complex,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_doublemath) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%dc(v%offset:v%offset+esize)=&
                  conjg(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%dc(v%offset:v%offset+esize)=&
               conjg(arg(3)%data%dc(arg(3)%offset:arg(3)%offset+esize))
       else
          do j=0,pm_fast_esize(ve)
             v%data%dc(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  conjg(arg(3)%data%dc(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset))
          enddo
       endif

       
       
    case(op_string_l)
       newve=shrink_ve(context,ve,esize)
       call set_arg(2,&
            vector_make_string(context,&
            newve,arg(3),fmt_l_width,fmt_l))
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
          do j=0,pm_fast_esize(ve)
             arg(2)%data%l(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%l(arg(3)%offset+ve%data%ln(ve%offset+j))
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset).and.&
                  arg(4)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset).or.&
                  arg(4)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_eq_l)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).eqv.&
                  arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).eqv.&
               arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset).eqv.&
                  arg(4)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
       endif
    case(op_ne_l)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_logical.and.pm_mask_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).neqv.&
                  arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
          end where
       elseif(pm_fast_vkind(ve)/=pm_long) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).neqv.&
               arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
       else
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset).neqv.&
                  arg(4)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  .not.arg(3)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          enddo
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

    newstack=pm_fast_new(context,pm_stack,int(stacksize,pm_p))
    if(pm_debug_level>3) then
       write(*,*) 'NEWSTACK>',newstack%offset,newstack%data%esize,&
            modulo(newstack%offset-1,newstack%data%esize+1)
    endif
    if(pm_debug_checks) then
       if(modulo(newstack%offset-1,newstack%data%esize+1)/=0) &
            call pm_panic('stack alloc')
    endif
    newstack%data%ptr(newstack%offset)=&
         pm_fast_tinyint(context,stacksize-1)
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
       v=pm_fast_newnc(context,pm_pointer,nargs-i)
       v%data%ptr(v%offset:v%offset+nargs-i-1)=arg(i+1:nargs)
       newstack%data%ptr(newstack%offset+i+pm_stack_locals)=v
       context%temp_obj1=pm_null_obj
       nargs=i+1
    else
       if(pm_debug_checks) then
          if(i/=nargs) then
             write(*,*) opcode2,&
                  trim(pm_name_as_string(context,proc_get_name(newfunc)))
             write(*,*) 'Num Args=',nargs,'Num pars expected=',i
             call dump_stack(context,stack,func,pc,ve)
             call pm_panic('Arg/param mismatch')
          endif
       endif
       if(nargs>0) then
          newstack%data%ptr(newstack%offset+pm_stack_locals:&
               newstack%offset+pm_stack_locals+nargs-1)=&
               arg(1:nargs)
       endif
    endif
    !write(*,*) trim(pm_name_as_string(context,proc_get_name(newfunc)))
    pc=newpc
    pc%offset=pc%offset+3_pm_p
    stack=newstack
    func=newfunc

    context%call_depth=context%call_depth+1
    if(context%call_depth>512) then
       call runtime_error(context,func,pc,ve,noexit,&
            'Too many nested calls - possible infinite recursion')
       goto 999
    endif
    if(pm_debug_level>2.or.trace_calls) then
       call proc_line_module(func,&
            max(int(pc%offset-func%data%ptr(func%offset)%offset)-4,1),line,modl)
       !vm_depth=vm_depth+1
       write(*,*) spaces(1:min(10,vm_depth)*2),'CALL>',opcode2,&
            pm_name_as_string(context,proc_get_name(func)),&
            '@',trim(pm_name_as_string(context,modl)),'#',line
    endif
    if(pm_debug_level>3) then
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
    call runtime_error(context,func,pc,ve,noexit,&
         'Cannot access structure/record element: '//&
               trim(pm_name_as_string(context,opcode2)))
    goto 999

997 continue

    ! Vector/array operation errors
    select case(errno)
    case(vector_type_error)
       call runtime_error(context,func,pc,ve,noexit,&
            'Type mismatch')
    case(vector_size_error)
       call runtime_error(context,func,pc,ve,noexit,&
            'Size mismatch')
    case(vector_slice_error)
       call runtime_error(context,func,pc,ve,noexit,&
            'Malformed slice')
    case(vector_index_error)
       call runtime_error(context,func,pc,ve,noexit,&
            'Index out of range')
    case(vector_shape_error)
       call runtime_error(context,func,pc,ve,noexit,&
            'Arrays do not have conforming domains')
    end select

999 continue

    if(.not.noexit) call dump_stack(context,stack,func,pc,ve)

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
    include 'fistiny.inc'
    include 'ftypeof.inc'
    include 'fname.inc'
    include 'ftypeno.inc'

    subroutine set_arg(iarg,val)
      integer,intent(in):: iarg
      type(pm_ptr),intent(in):: val
      arg(iarg)%data%ptr(arg(iarg)%offset)=val
    end subroutine set_arg

    function alloc_arg(vkind,iarg) result(ptr)
      integer(pm_p),intent(in):: vkind
      integer,intent(in)::iarg
      type(pm_ptr):: ptr
      ptr=pm_new(context,vkind,max(1_pm_ln,esize+1))
      arg(iarg)%data%ptr(arg(iarg)%offset)=ptr
    end function alloc_arg

    subroutine alloc_args_to_long(argstart,argend)
      integer,intent(in):: argstart,argend
      integer:: i
      do i=argstart,argend
          call set_arg(i,&
               pm_new(context,pm_long,esize+1_pm_ln))
      enddo
    end subroutine alloc_args_to_long

    subroutine set_args_from_ibuffer(j,argstart,argend,ibuffer)
      integer(pm_ln),intent(in):: j
      integer,intent(in):: argstart,argend
      integer:: i,n,ierror
      integer(pm_ln),intent(in),dimension(*):: ibuffer
      type(pm_ptr):: v
      n=argend-argstart+1
      if(n<1.or.n>7) call pm_panic('args from ibuffer')
      do i=argstart,argend
         v=arg(i)%data%ptr(arg(i)%offset)
         v%data%ln(v%offset+j)=ibuffer(i-argstart+1)
      enddo
    end subroutine set_args_from_ibuffer

    subroutine fill_args_from_ibuffer(argstart,argend,ibuffer)
      integer,intent(in):: argstart,argend
      integer:: i,n,ierror
      integer(pm_ln),intent(in),dimension(*):: ibuffer
      type(pm_ptr):: v
      n=argend-argstart+1
      if(n<1.or.n>7) call pm_panic('args from ibuffer')
      
      do i=argstart,argend
         v=pm_new(context,pm_long,esize+1_pm_ln)
         call set_arg(i,v)
         v%data%ln(v%offset:v%offset+esize)=ibuffer(i-argstart+1)
      enddo
    end subroutine fill_args_from_ibuffer
    
    subroutine fill_args_from_lbuffer(argstart,argend,lbuffer)
      integer,intent(in):: argstart,argend
      integer:: i,n,ierror
      logical,intent(in),dimension(*):: lbuffer
      type(pm_ptr):: v
      n=argend-argstart+1
      if(n<1.or.n>7) call pm_panic('args from ibuffer')
      if(esize<huge(v%offset)-1) then
         do i=argstart,argend
            v=pm_fast_newnc(context,pm_logical,int(esize+1))
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
    end subroutine fill_args_from_lbuffer

    subroutine get_args_to_lbuffer(j,argstart,argend,lbuffer)
      integer(pm_ln),intent(in):: j
      integer,intent(in):: argstart,argend
      logical,dimension(*),intent(out):: lbuffer
      integer:: i,n
      n=argend-argstart+1
      if(n<1.or.n>8) call pm_panic('args to lbuffer')
      do i=argstart,argend
         lbuffer(i-argstart+1)=arg(i)%data%l(arg(i)%offset+j)
      enddo
    end subroutine get_args_to_lbuffer

    subroutine get_args_to_ibuffer(j,argstart,argend,ibuffer)
      integer(pm_ln),intent(in):: j
      integer,intent(in):: argstart,argend
      integer(pm_ln),dimension(*),intent(out):: ibuffer
      integer:: i,n
      n=argend-argstart+1
      if(n<1.or.n>8) call pm_panic('args to ibuffer')
      do i=argstart,argend
         ibuffer(i-argstart+1)=arg(i)%data%ln(arg(i)%offset+j)
      enddo
    end subroutine get_args_to_ibuffer

    function make_new_ve(ve_vec,oldve) result(ve)
      type(pm_ptr),intent(in):: ve_vec,oldve
      type(pm_ptr):: ve
      ve=pm_fast_newnc(context,pm_pointer,2)
      ve%data%ptr(ve%offset)=ve_vec
      ve%data%ptr(ve%offset+1)=oldve%data%ptr(oldve%offset+1)
    end function make_new_ve

    function pack_in_place(p,q,n) result(m)
      type(pm_ptr),intent(in):: p,q
      integer(pm_ln),intent(in):: n
      integer(pm_ln):: m
      integer(pm_ln):: i
      m=0
      do i=0,n
         if(q%data%l(q%offset+i)) then
            m=m+1
            p%data%ln(p%offset+m)=p%data%ln(p%offset+i)
         endif
      enddo
    end function pack_in_place

    subroutine make_block_cyclic
      integer(pm_ln),dimension(7):: start,end,step,ostart,oend,ostep,begin,finish
      integer(pm_ln):: tot,totsiz,k,off
      integer:: ndim,i
      type(pm_ptr),target,dimension(14):: arg
      type(pm_ptr):: import_vec,p,q,pp,qq
      type(pm_reg),pointer:: reg
      integer,target:: m

      import_vec=arg(11)
      totsiz=import_vec%data%ln(import_vec%offset)+1_pm_ln
      p=arg(2)%data%ptr(arg(2)%offset+2)
      q=arg(2)%data%ptr(arg(2)%offset+3)
      ndim=pm_fast_esize(p)-1
      reg=>pm_register(context,'bc',array=arg,array_size=m)
      m=0
      do i=1,ndim
         pp=pm_new(context,pm_long,totsiz)
         arg(i*2-1)=pp
         m=m+1
         qq=pm_new(context,pm_long,totsiz)
         arg(i*2)=qq
         m=m+1
         p%data%ptr(p%offset+i+1)=pp
         q%data%ptr(q%offset+i+1)=qq
      enddo

      off=0
      do k=0,esize
         call get_tuple_longs(arg(3),k,start,ndim)
         call get_tuple_longs(arg(4),k,end,ndim)
         call get_tuple_longs(arg(5),k,step,ndim)
         call get_tuple_longs(arg(6),k,ostart,ndim)
         call get_tuple_longs(arg(7),k,oend,ndim)
         call get_tuple_longs(arg(8),k,ostep,ndim)
         call get_tuple_longs(arg(9),k,begin,ndim)
         call get_tuple_longs(arg(10),k,finish,ndim)
         tot=import_vec%data%ln(import_vec%offset+k+4_pm_ln)
         call vector_bc(context,ndim,arg,start,end,step,&
              ostart,oend,ostep,begin,finish,tot,off)
         off=off+tot
      enddo

      call pm_delete_register(context,reg)
      
    end subroutine make_block_cyclic

    subroutine get_tuple_longs(v,k,j,n)
      type(pm_ptr):: v
      integer(pm_ln):: k
      integer(pm_ln),dimension(7):: j
      integer:: n
      do i=1,n
         p=v%data%ptr(v%offset+1+i)
         j(i)=p%data%ln(p%offset+k)
      enddo
    end subroutine get_tuple_longs

    function first_false(ve,mask) result(i)
      type(pm_ptr):: ve,mask
      integer(pm_ln):: i
      i=0
      do while(mask%data%l(mask%offset+ve%data%ln(ve%offset+i)))
         i=i+1
      enddo
    end function first_false
    
  end function pm_run


  ! Parallel loop 
  function par_loop(context,func,stack,pc,arg,&                 
       num_args,ve,old_esize,nesting,noexit) result(errno) 
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: func,stack,pc,ve
    type(pm_ptr),dimension(num_args):: arg
    integer,intent(in):: num_args,nesting
    integer(pm_ln),intent(in):: old_esize
    logical,intent(in):: noexit
    integer:: errno
    type(pm_ptr):: newpc,newve,hash
    integer(pm_ln):: n,m
    newpc=pc
    newpc%offset=newpc%offset+4_pm_p
    arg(2)%data%ptr(arg(2)%offset)=&
         pm_fast_newnc(context,pm_pointer,2)
    newve=arg(2)%data%ptr(arg(2)%offset)
    newve%data%ptr(newve%offset)=pm_null_obj
    hash=arg(3)
    n=pm_fast_esize(hash)+1_pm_ln
    if(.not.pm_fast_isnull(ve)) hash=vector_zero_unused(context,hash,ve)
    m=sum(hash%data%ln(hash%offset:hash%offset+n-1_pm_ln))-1_pm_ln
    if(m>=0) then
       newve=pm_assign_new(context,newve,1_pm_ln,pm_long,n+4_pm_ln,.false.)
       newve%data%ln(newve%offset)=m
       newve%data%ln(newve%offset+1)=0
       newve%data%ln(newve%offset+2)=0
       newve%data%ln(newve%offset+3)=m+1
       newve%data%ln(newve%offset+4:newve%offset+3+n)=&
            hash%data%ln(hash%offset:hash%offset+n-1)
       errno=pm_run(context,func,stack,newpc,-1,0,&
            arg,num_args,nesting,noexit)
    else
       newve%data%ptr(newve%offset)=pm_fast_tinyint(context,0)
       newve=pm_assign_new(context,newve,1_pm_ln,pm_long,n+4_pm_ln,.false.)
       newve%data%ln(newve%offset)=0
       newve%data%ln(newve%offset+1)=0
       newve%data%ln(newve%offset+2)=0
       newve%data%ln(newve%offset+3)=0
       newve%data%ln(newve%offset+4:newve%offset+3+n)=&
            hash%data%ln(hash%offset:hash%offset+n-1)
       errno=pm_run(context,func,stack,newpc,-1,0,&
            arg,num_args,nesting,noexit)
    endif
  contains
    include 'fesize.inc'
    include 'fnewnc.inc'
    include 'fisnull.inc'
    include 'ftiny.inc'
  end function  par_loop

  ! Call block of args
  ! set ref arg(2) to new ve (index values of ve if ve no null - otherwise simple ve length m)
  ! set ref arg(3) to node (scalar)
  ! set ref arg(4) to index (or index 2 if present)
  function remote_call_block(context,func,stack,pc,arg,pcoff,&
       node,xdata,v,m,issend,disps) result(errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: func,stack,pc
    type(pm_ptr),dimension(*),intent(in):: arg
    integer(pm_p),intent(in):: pcoff
    integer,intent(in):: node
    type(pm_ptr),intent(in):: xdata,v
    integer(pm_ln),intent(in):: m
    logical,intent(in):: issend
    integer(pm_ln),dimension(:),intent(in),optional:: disps
    integer:: errno
    integer:: num_args
    type(pm_ptr):: newve,p,newpc
    integer(pm_ln):: n,i

!!$    write(*,*) sys_node,'BLOCK m=',m
!!$    if(present(disps)) write(*,*) size(disps)
    
    if(m>0) then
       
       newve=pm_fast_newnc(context,pm_pointer,2)
       arg(2)%data%ptr(arg(2)%offset)=newve
       if(present(disps)) then
          if(size(disps)==0) then
             errno=0
             return
          else
             p=pm_assign_new(context,newve,0_pm_ln,pm_long,size(disps,kind=pm_ln),.false.)
             p%data%ln(p%offset:p%offset+size(disps)-1)=disps
          endif
       else
          newve%data%ptr(newve%offset)=pm_null_obj
       endif
       p=pm_fast_newnc(context,pm_long,1)
       arg(3)%data%ptr(arg(3)%offset)=p
       p%data%ln(p%offset:p%offset)=node

       arg(4)%data%ptr(arg(4)%offset)=xdata

       if(issend) arg(5)%data%ptr(arg(5)%offset)=v

       newve=pm_assign_new(context,newve,1_pm_ln,pm_long,5_pm_ln,.false.)
       newve%data%ln(newve%offset)=m-1
       newve%data%ln(newve%offset+1)=0
       newve%data%ln(newve%offset+2)=0
       newve%data%ln(newve%offset+3)=m
       newve%data%ln(newve%offset+4)=m
 
       newpc=pc
       newpc%offset=pcoff
       num_args=9
       errno=pm_run(context,func,stack,newpc,-1,0,&
            arg,num_args,0,.true.)
       !write(*,*) 'BLOCK_COMPLETED>'
    else
       errno=0
    endif
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'ftiny.inc'
  end function remote_call_block

  ! Make vector engine length n, no masking
  function make_simple_ve(context,n) result(ve)
    type(pm_context),pointer:: context
    integer(pm_ln),intent(in):: n
    type(pm_ptr):: ve
    type(pm_ptr):: ve1
    integer:: extra
    extra=merge(1,0,n>1)
    ve1=pm_fast_newnc(context,pm_long,4+extra)
    ve1%data%ln(ve1%offset)=n-1
    ve1%data%ln(ve1%offset+1)=0_pm_ln
    ve1%data%ln(ve1%offset+2)=0_pm_ln
    ve1%data%ln(ve1%offset+3)=n
    if(extra/=0) ve1%data%ln(ve1%offset+4)=n
    context%temp_obj1=ve1
    ve=pm_fast_newnc(context,pm_pointer,2)
    ve%data%ptr(ve%offset)=merge(pm_null_obj,pm_fast_tinyint(context,0),&
         n>0)
    ve%data%ptr(ve%offset+1)=ve1
    context%temp_obj1=pm_null_obj
  contains
    include 'fnewnc.inc'
    include 'ftiny.inc'
  end function make_simple_ve

  ! Create a new type of kind tkind and arguments from types of args
  function pm_arglist_type(context,tkind,tname,args,nargs) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: tkind,tname
    type(pm_ptr),dimension(nargs),intent(in):: args
    integer,intent(in):: nargs
    integer:: tno
    integer,dimension(pm_max_args+2):: t
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

  ! Find runtime type of current argument
  ! (some limitations)
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

  ! Output runtime error message using message queue
  subroutine runtime_error(context,func,pc,ve,noexit,errmesg)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: func,pc,ve
    logical,intent(in):: noexit
    character(len=*),intent(in):: errmesg
    integer:: junk
    if(.not.noexit) then
       junk=sync_status(pc,pm_node_error)
       call mesg_q_flush()
    endif
    if(pm_opts%colour) then
       call mesg_q_mess(pm_error_start//'Runtime error: '//pm_error_end//trim(errmesg))
    else
       call mesg_q_mess('Runtime error: '//trim(errmesg))
    endif
    call print_module_and_line(context,func,pc,ve)
  end subroutine runtime_error

  ! Dump current call stack to message queue (or print queue if printit defined)
  subroutine dump_stack(context,dstack,dfunc,dpc,ve,printit)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: dstack,dfunc,dpc,ve
    logical,intent(in),optional:: printit
    type(pm_ptr):: stack,func,pc
    integer(pm_p):: name
 
    stack=dstack
    func=stack%data%ptr(stack%offset+pm_stack_func)
    pc=stack%data%ptr(stack%offset+pm_stack_pc)
    stack=stack%data%ptr(stack%offset+pm_stack_oldstack)
    if(.not.pm_fast_isnull(func)) then
       if(.not.present(printit)) then
          call mesg_q_mess(&
               '=========================Call trace =========================')
       endif
       do while(.not.pm_fast_isnull(func))
          call print_module_and_line(context,func,pc,ve,printit=printit)
          func=stack%data%ptr(stack%offset+pm_stack_func)
          pc=stack%data%ptr(stack%offset+pm_stack_pc)
          stack=stack%data%ptr(stack%offset+pm_stack_oldstack)
          if(pm_fast_isnull(stack)) exit
       enddo
       if(.not.present(printit)) then
          call mesg_q_mess(&
               '=============================================================')
       endif
    endif
  contains
    include 'fisnull.inc'
  end subroutine dump_stack

  ! Print module and line to message queue (or print queue if printit present)
  subroutine print_module_and_line(context,func,pc,ve,printit)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: func,pc,ve
    logical,intent(in),optional:: printit
    integer:: name
    character(len=80):: mess
    integer:: line,modl
    character(len=67):: buffer
    call proc_line_module(func,&
         max(int(pc%offset-func%data%ptr(func%offset)%offset)-4,1),line,modl)
    if(pm_fast_isnull(func)) then
       mess='Startup: '
    else
       name=proc_get_name(func)
       if(name==sym_pm_system) then
          mess='Program: '
       else
          mess='Proc '//trim(pm_name_as_string(context,name))//': '
       endif
    endif
    call pm_name_string(context,modl,mess(len_trim(mess)+2:))
    write(buffer,'(i20)') int(line)
    if(present(printit)) then
       call mesg_q_print_str(context,trim(mess)//':'//trim(adjustl(buffer)))
       if(.not.pm_get_source_line(context,modl,line,buffer)) then
          call mesg_q_print_str(context,'   '//trim(buffer(1:67)))
       endif
    else
       call mesg_q_mess(trim(mess)//':'//trim(adjustl(buffer)))
       if(.not.pm_get_source_line(context,modl,line,buffer)) then
          call mesg_q_mess('   '//trim(buffer(1:67)))
       endif
    endif
  contains
    include 'fisnull.inc'
  end subroutine print_module_and_line

  ! Print strings in vector v at active locations defined in ve
  ! ve must be shrunk before calling this
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
       if(pm_opts%print_immediate) then
          write(*,*) p%data%s(p%offset+start:p%offset+start+size-1)
       else
          call mesg_q_print(context,p%data%s(p%offset+start:p%offset+start+size-1))
       endif
    enddo
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine vector_print_string

  ! Print string at location #j in vector v
  subroutine vector_print_single_string(context,v,j)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: j
    integer(pm_ln):: i,start,size
    type(pm_ptr):: vec,len,off,p
    vec=v%data%ptr(v%offset+pm_array_vect)
    len=v%data%ptr(v%offset+pm_array_length)
    off=v%data%ptr(v%offset+pm_array_offset)
    start=off%data%ln(off%offset+j)
    size=len%data%ln(len%offset+j)
    p=vec%data%ptr(vec%offset+j)
    call mesg_q_print(context,p%data%s(p%offset+start:p%offset+start+size-1))
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine vector_print_single_string

  
end module pm_backend


! This is just a placeholder as the VM version does not currently
! run an optimiser
module pm_optimise
use pm_memory
contains
  subroutine optimise_prog(context,p,poly_cache)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: p,poly_cache
  end subroutine optimise_prog

end module pm_optimise
