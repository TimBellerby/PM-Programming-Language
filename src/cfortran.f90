!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2023
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

! Generate FORTRAN code from wordcodes

module pm_backend

  use pm_kinds
  use pm_memory
  use pm_types
  use pm_vmdefs
  use pm_hash
  use pm_lib
  use pm_wcode

  implicit none

  logical,parameter:: debug_g=.false.

  ! Various limits
  integer,parameter:: ftn_max_line=130
  integer,parameter:: ftn_max_name=31
  integer,parameter:: max_vars=2**15-1
  integer,parameter:: max_levels=256
  integer,parameter:: max_loop_stack=10*1024

  ! Variable descriptor
  type gvar
     integer:: tno          ! Type
     integer:: flags        ! Flags from previous stage
     integer:: gflags       ! Flags applied in this stage
     integer:: state        ! State engine (determines if needs to be stored as vector)
     integer:: start        ! First instruction point var is used
     integer:: finish       ! Last instruction point var is used
     integer:: lthis        ! Parallel context
     integer:: link         ! Linked list of variables in creation order
     integer:: elink        ! Linked list of variables in destruction order
     integer:: index        ! Vector to store this variable (may be shared)
     integer:: free         ! List of variables free for reuse (*not unused records*)
     integer:: oindex       ! Index of variable in output from wordcode generator
     integer:: outer_lthis  ! Outermost parallel context in which variable is referenced
     logical:: finish_on_assign
                            ! Last statement using this variable is assignment to another variable
     integer:: name         ! PM name of variable
  end type gvar

  ! Variable states for identifying those
  ! that need storing in local arrays
  ! in concurrent code (employed by small
  ! state machine)
  integer,parameter:: var_state_unused=0
  integer,parameter:: var_state_open=1
  integer,parameter:: var_state_used=2
  integer,parameter:: var_state_used_before=3
  integer,parameter:: var_state_crossing=4
  integer,parameter:: var_state_closed=5

  ! Local flags for variables
  integer,parameter:: var_is_recycled=1
  integer,parameter:: var_is_async=2
  integer,parameter:: var_is_else_disabled=4
  integer,parameter:: var_is_comm_op_par=8
  integer,parameter:: var_is_used=16
  integer,parameter:: var_is_reused=32
  integer,parameter:: var_is_stacked_ve=64

  ! Loop modes
  integer,parameter:: loop_is_none=0
  integer,parameter:: loop_is_contig=1
  integer,parameter:: loop_is_nested=2

  ! Loop parameters
  integer,parameter:: loop_start=1
  integer,parameter:: loop_end=2
  integer,parameter:: loop_step=3
  integer,parameter:: loop_size=4
  integer,parameter:: loop_block=5
  
  ! Parallel context
  type gloop
     integer:: varlist
     integer:: evarlist
     integer:: defer_free
     integer:: idx
     integer:: free(2,pm_int:pm_string)
     integer:: nloops
     integer:: loop_mode
     integer:: loop_par
     logical:: loop_active
     integer:: depth
     integer:: parent
  end type gloop
  
  ! Current state of FORTRAN code generator
  type gen_state

     type(pm_context),pointer:: context

     ! Alloc procedures
     type(pm_ptr):: procs
     
     ! Current proc
     type(pm_ptr):: fn
     integer:: taints
     
     ! Wordcodes for current proc
     integer,dimension(:),pointer:: codes
     integer,dimension(:),pointer:: vars

     ! Variables
     integer:: nvars,index
     integer,dimension(:),allocatable:: varindex
     type(gvar),dimension(:),allocatable:: vardata
     type(pm_ptr):: freehash

     ! Parallel loop frames
     type(gloop),dimension(0:max_loop_stack):: lstack
     integer:: lthis,ltop

     ! VE used by last instruction previously coded
     integer:: last_ve

     ! Alternative loop frame for wrapped vectors in op_do_at
     integer:: lalt

     ! Typesets
     type(pm_ptr):: poly_cache,packables,mpi_types,mpi_root_types
     type(pm_reg),pointer:: reg

     ! Does current loop contain shared operations?
     logical:: loop_contains_shared
    
     ! Fortran code output
     character(len=ftn_max_line):: linebuffer
     integer:: n,outunit
     integer:: line_breaks
     
  end type gen_state

  ! Control how an argument is converted
  integer,parameter:: arg_no_index=1
  integer,parameter:: arg_ix_index=2
  integer,parameter:: arg_comm_arg=4
  integer,parameter:: arg_wrapped=8
  integer,parameter:: arg_chan=16
 
  ! Type of pack routine (for g_add_packable)
  integer,parameter:: pack_scalar=0
  integer,parameter:: pack_vect=1
  integer,parameter:: pack_array_vect=2
  integer,parameter:: pack_vect_disp=3
  integer,parameter:: pack_array_vect_disp=4
  integer,parameter:: unpack_vect=5
  integer,parameter:: unpack_vect_disp=6

  ! Modes of parameter passed to MPI comm operation
  integer,parameter:: mode_array=0
  integer,parameter:: mode_vect=1
  integer,parameter:: mode_array_vect=2
  
contains

  !===========================================
  ! Generate Fortran code for program
  !===========================================
  subroutine gen_prog(context,p,poly_cache,typeset,iunit)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: p,poly_cache,typeset
    integer,intent(in):: iunit
    type(gen_state),target:: g
    type(pm_ptr):: key
    integer:: i
    integer:: tno

    ! Set up code generator state
    g%context=>context
    g%reg=>pm_register(context,'gen_prog',&
         g%procs,g%poly_cache,g%packables,&
         g%mpi_types,g%mpi_root_types,g%freehash)
    g%procs=p
    g%poly_cache=poly_cache
    g%packables=pm_set_new(context,32_pm_ln)
    g%mpi_types=pm_set_new(context,32_pm_ln)
    g%mpi_root_types=pm_set_new(context,32_pm_ln)
    g%outunit=iunit
    g%line_breaks=0
    g%n=0

    ! Program code
    call out_line_noindent(g,'PROGRAM PM')
    i=iunit
    
    ! The rtime code calls gen_procs and out_types
    include 'rtime.inc'

    ! Tidy up
    call out_line_noindent(g,'END PROGRAM PM')
    call pm_delete_register(context,g%reg)
    
  contains

    include 'fesize.inc'

    ! Generate procedures
    subroutine gen_procs
      integer(pm_ln):: i
      
      ! Generate code for each procedure
      do i=1,pm_dict_size(context,p)
         if(debug_g) write(*,*) 'OUTING PROC>',i-1
         call gen_proc(g,pm_dict_val(context,p,i),int(i-1))
      enddo

      ! Generate procedures to pack and unpack types
      ! to a communications buffer
      call gen_packables(g)

      ! Generate procedure to create required MPI types
      call gen_mpi_types(g)
    end subroutine gen_procs

    ! Output type definitions (called from rtime.inc)
    subroutine out_types
      integer:: i
      type(pm_ptr):: keys,key
      call out_new_line(g)
      call out_type_defs(g,typeset)
      call out_new_line(g)
    end subroutine out_types

  end subroutine gen_prog


  !===========================================
  ! Generate code for a single procedure
  !===========================================
  subroutine gen_proc(g,p,no)
    type(gen_state):: g
    type(pm_ptr),intent(in)::p
    integer,intent(in):: no
    integer:: i,n,rvar,pvar,vevar,name
    type(pm_ptr)::q,taint,keys
    logical:: iscomm

    ! Get  wordcodes & meta-info for this function
    g%fn=p
    g%freehash=pm_dict_new(g%context,128_pm_ln)
    q=p%data%ptr(p%offset)
    rvar=q%data%i(q%offset)
    pvar=q%data%i(q%offset+1)
    name=q%data%i(q%offset+2)
    vevar=q%data%i(q%offset+3)
    g%codes=>q%data%i(q%offset+4:q%offset+pm_fast_esize(q))
    taint=p%data%ptr(p%offset+2)
    keys=p%data%ptr(p%offset+3)
    g%taints=taint%offset
    iscomm=iand(int(taint%offset),proc_is_comm)/=0
    
    ! Output spacing / comment
    call out_new_line(g)
    call out_line_noindent(g,' !'//&
         trim(pm_name_as_string(g%context,name)))
    if(debug_g) then
       write(*,*) 'OUT START> ',&
            trim(pm_name_as_string(g%context,q%data%i(q%offset+2)))
    endif

    ! Set up variable data tables
    q=p%data%ptr(p%offset+1)
    g%vars=>q%data%i(q%offset:q%offset+pm_fast_esize(q))
    n=1+pm_fast_esize(q)
    allocate(g%varindex(n))
    allocate(g%vardata(n/2))
    g%nvars=0
    g%varindex(1:n)=0

    ! Output procedure header
    if(iand(int(taint%offset),proc_is_recursive)/=0) &
         call out_str(g,'RECURSIVE ')
    if(iand(int(taint%offset),proc_is_impure)==0) &
         call out_str(g,'PURE ')
    call out_str(g,'SUBROUTINE PM__P')
    call out_idx(g,no)
    if(pm_opts%ftn_name_procs.and.no>0) then
       call out_ftn_name(g,name)
    endif
    
    ! Phase I - analyse variable use to determine variable lifetimes
    ! and which variables need to be
    ! stored as vectors
    ! Also output necessary variable definition lines here
    call init_g
    g%ltop=-1
    call g_new_frame(g)
    g%lstack(g%lthis)%loop_mode=loop_is_none
    if(vevar/=-1) then
       call use_var(g,vevar)
       call cross_var(g,vevar)
    endif
    if(.not.pm_fast_isnull(keys)) then
       do i=0,pm_fast_esize(keys)
          call use_var(g,keys%data%i(keys%offset+i))
       enddo
    endif
    if(pvar/=-1) call use_var(g,pvar)
    if(size(g%codes)>0) call gen_var_block(g,comp_op_start)
    if(rvar/=-1) call use_var(g,rvar)

    ! Phase II - analyse variable lifetimes to merge variables
    do i=0,g%ltop
       g%lthis=i
       call sort_var_list(g)
       call alloc_var_list(g)
    enddo
    
    ! Phase III - output necessary definition lines
    call out_char(g,'(')
    if(iscomm) then
       call out_str(g,'N1,')
       if(vevar>0) then
          call out_param(g,vevar)
          call out_char(g,',')
       endif
    endif
    if(rvar/=-1) then
       call out_param(g,rvar)
    endif
    if(.not.pm_fast_isnull(keys)) then
       do i=0,pm_fast_esize(keys)
          call out_param(g,keys%data%i(keys%offset+i))
       enddo
    endif
    if(pvar/=-1) then
       call out_param(g,pvar)
    endif
    call out_close(g)
    call out_new_line(g)
    if(iscomm) then
       call out_line(g,'INTEGER(PM__LN),INTENT(IN):: N1')
    endif
    do i=1,g%nvars
       call out_var_def(g,i,iscomm)
    enddo

    ! Phase IV - output code for the body of the procedure
    call init_g
    if(size(g%codes)>0) then
       call gen_block(g,comp_op_start)
       call gen_if_nest(g,g%last_ve,0)
       call gen_close_loops(g)
    endif
    call out_str_noindent(g,' END SUBROUTINE PM__P')
    call out_idx(g,no)
    if(pm_opts%ftn_name_procs.and.no>0) then
       call out_ftn_name(g,name)
    endif        
    call out_new_line(g)

    ! Tidy up
    deallocate(g%varindex)
    deallocate(g%vardata)
    if(debug_g) then
       write(*,*) 'OUT DONE> ',&
            trim(pm_name_as_string(g%context,&
            q%data%i(q%offset+2)))
    endif

  contains
    include 'fesize.inc'
    include 'fisnull.inc'
    subroutine init_g
      g%lthis=0
      g%ltop=0
      g%lalt=-1
      g%loop_contains_shared=.false.
    end subroutine init_g
  end subroutine gen_proc

  !===================================
  ! Return the name of procedure #n
  !===================================
  function g_procname(g,n) result(name)
    type(gen_state):: g
    integer,intent(in):: n
    integer:: name
    type(pm_ptr):: p,q
    p=pm_dict_val(g%context,g%procs,int(n+1,pm_ln))
    q=p%data%ptr(p%offset)
    name=q%data%i(q%offset+2)
  end function g_procname

  !************************************************
  ! PHASE I - VARIABLE ASSIGNMENT
  !************************************************

  !===========================================
  ! Variable assignment phase for a code block
  !===========================================
  recursive subroutine gen_var_block(g,loc)
    type(gen_state):: g
    integer,intent(in):: loc
    integer:: l
    if(debug_g) write(*,*) 'VAR BLOCK>'
    l=loc
    do while(l>0)
       if(debug_g) write(*,*) 'DO VAR>',l
       g%lstack(g%lthis)%idx=g%lstack(g%lthis)%idx+1
       call gen_var_op(g,l)
       l=g%codes(l)
       if(debug_g) write(*,*) 'NEXT VAR',l
    enddo
    if(debug_g) write(*,*) 'END VAR BLOCK>'
  end subroutine gen_var_block

  !==================================================
  ! Variable assignment phase for a single operation
  !==================================================
  recursive subroutine gen_var_op(g,l)
    type(gen_state):: g
    integer,intent(in):: l
    integer:: opcode,opcode2,n,arg
    integer:: i,j,a,save_lthis,ll,var1,idx1
    logical:: save_loop_contains_shared
    if(pm_debug_level>0) then
       if(l>size(g%codes)) then
          write(*,*) 'l=',l,size(g%codes)
          call pm_panic('gen_var_op bad l')
       endif
    endif
    opcode=g%codes(l+comp_op_opcode)
    opcode2=g%codes(l+comp_op_opcode2)
    n=iand(g%codes(l+comp_op_nargs),comp_op_nargs_mask)
    a=l+comp_op_arg0
    
    if(debug_g) then
       write(*,*) 'l=',l,n,op_names(opcode)
       write(*,*) 'VAR OP> ',g%lstack(g%lthis)%idx,l,op_names(opcode),n,&
            '>>',g%codes(l:l+comp_op_arg0+n-1)
    endif
    
    select case(opcode)
    case(op_if,op_if_shared,op_if_restart)
       call use_var(g,g%codes(a))
       call gen_var_block(g,g%codes(a+1))
       call gen_var_block(g,g%codes(a+2))
       call use_var(g,g%codes(a+3))
    case(op_if_shared_node)
       call use_var(g,g%codes(a))
       call gen_var_block(g,g%codes(a+1))
       call gen_var_block(g,g%codes(a+2))
    case(op_over,op_skip_empty)
       call cross_all_vars(g)
       call use_var(g,g%codes(a))
       call gen_var_block(g,g%codes(a+1))
       call cross_all_vars(g)
    case(op_head_node)
       call use_var(g,g%codes(a))
       call gen_var_block(g,g%codes(a+1))
    case(op_loop)
       save_loop_contains_shared=g%loop_contains_shared
       g%loop_contains_shared=.false.
       var1=g%lstack(g%lthis)%varlist
       idx1=g%lstack(g%lthis)%idx
       call use_var(g,g%codes(a))
       call gen_var_block(g,g%codes(a+1))
       call extend_finish_to_loop(g,idx1,g%lstack(g%lthis)%idx,var1)
       call use_var(g,g%codes(a+2))
       if(g%loop_contains_shared) then
          g%codes(l+comp_op_opcode2)=ior(g%codes(l+comp_op_opcode2),2)
       endif
       g%loop_contains_shared=save_loop_contains_shared
    case(op_comm_loop,op_comm_loop_par)
       save_loop_contains_shared=g%loop_contains_shared
       var1=g%lstack(g%lthis)%varlist
       idx1=g%lstack(g%lthis)%idx
       call cross_all_vars(g)
       call use_var(g,g%codes(a))
       call gen_var_block(g,g%codes(a+1))
       call extend_finish_to_loop(g,idx1,g%lstack(g%lthis)%idx,var1)
       call use_var(g,g%codes(a+2))
       call cross_all_vars(g)
       g%loop_contains_shared=save_loop_contains_shared
    case(op_comm_proc)
       save_loop_contains_shared=g%loop_contains_shared
       call use_var(g,g%codes(a))
       call gen_var_comm_block(g,g%codes(a+1))
       g%loop_contains_shared=save_loop_contains_shared
    case(op_inline_shared)
       save_loop_contains_shared=g%loop_contains_shared
       call use_var(g,g%codes(a))
       call gen_var_shared_block(g,g%codes(a+1))
       g%loop_contains_shared=save_loop_contains_shared
    case(op_comm_block)
       save_loop_contains_shared=g%loop_contains_shared
       call use_var(g,g%codes(a))
       call use_var(g,g%codes(a+2))
       call gen_var_comm_block(g,g%codes(a+1))
       g%loop_contains_shared=save_loop_contains_shared
    case(op_comm_call,op_dref,op_wrap)
       call cross_all_vars(g)
       do i=0,n-1
          call use_var(g,g%codes(a+i))
          call cross_var(g,g%codes(a+i))
          call use_var(g,g%codes(a+i))
       enddo
    case(op_init_var)
       call use_var(g,g%codes(a))
       call use_var(g,g%codes(a+1))
    case(op_comm_inline)   !!! Obsolete?
       call cross_all_vars(g)
    case(op_sync)
       call cross_all_vars(g)
    case(op_broadcast_val,&
         op_sync_mess,op_break_loop,&
         op_read_file_tile,op_write_file_tile,op_broadcast,&
         op_broadcast_shared,op_nested_loop,&
         op_blocked_loop,op_isend_offset,op_irecv_offset,&
         op_recv_offset,op_recv_offset_resend,op_isend_reply,&
         op_recv_reply,op_isend_req,op_isend_assn,op_active,op_get_size)
       call cross_all_vars(g)
       do i=0,n-1
          call use_var(g,g%codes(a+i))
          call cross_var(g,g%codes(a+i))
       enddo
    case(op_remote_send_call,op_collect_call)
       call cross_all_vars(g)
       call use_var(g,g%codes(a))
       do i=6,n-1  !!! Dont use or cross outputs
          call use_var(g,g%codes(a+i))
          call cross_var(g,g%codes(a+i))
       enddo
       save_lthis=g%lthis
       call g_new_frame(g)
       call use_var(g,g%codes(a+4))
       call cross_var(g,g%codes(a+4))
       call use_var(g,g%codes(a+5))
       call cross_var(g,g%codes(a+5))
       call gen_var_block(g,g%codes(a+1))
       g%lthis=save_lthis
       call gen_var_block(g,g%codes(a+2))
       call cross_all_vars(g)
       call use_var(g,g%codes(a))
       do i=6,n-1
          call use_var(g,g%codes(a+i))
          call cross_var(g,g%codes(a+i))
       enddo
    case(op_remote_call,op_server_call)
       call cross_all_vars(g)
       call use_var(g,g%codes(a))
       do i=5,7
          call use_var(g,g%codes(a+i))
          call cross_var(g,g%codes(a+i))
       enddo
       save_lthis=g%lthis
       call g_new_frame(g)
       call use_var(g,g%codes(a+4))
       call cross_var(g,g%codes(a+4))
       call use_var(g,g%codes(a+8))
       call cross_var(g,g%codes(a+8))
       call gen_var_block(g,g%codes(a+1))
       g%lthis=save_lthis
       call gen_var_block(g,g%codes(a+2))
       call cross_all_vars(g)
       call use_var(g,g%codes(a))
       do i=5,7
          call use_var(g,g%codes(a+i))
          call cross_var(g,g%codes(a+i))
       enddo
    case(op_bcast_call)
       call cross_all_vars(g)
       call use_var(g,g%codes(a))
       do i=5,n-1
          call use_var(g,g%codes(a+i))
          call cross_var(g,g%codes(a+i))
       enddo
       save_lthis=g%lthis
       call g_new_frame(g)
       call use_var(g,g%codes(a+2))
       call cross_var(g,g%codes(a+2))
       call use_var(g,g%codes(a+3))
       call cross_var(g,g%codes(a+3))
       call gen_var_block(g,g%codes(a+1))
       g%lthis=save_lthis
       call cross_all_vars(g)
       call use_var(g,g%codes(a))
       do i=5,n-1
          call use_var(g,g%codes(a+i))
          call cross_var(g,g%codes(a+i))
       enddo
    case(op_recv_req_call,op_recv_assn_call)
       call cross_all_vars(g)
       call use_var(g,g%codes(a))
       call use_var(g,g%codes(a+4))
       call cross_var(g,g%codes(a+4))
       call cross_all_vars(g)
       save_lthis=g%lthis
       call g_new_frame(g)
       call cross_all_vars(g)
       call use_var(g,g%codes(a+2))
       !call cross_var(g,g%codes(a+2))
       call use_var(g,g%codes(a+3))
       call cross_var(g,g%codes(a+3))
       if(opcode==op_recv_assn_call) then
          call use_var(g,g%codes(a+5))
          call cross_var(g,g%codes(a+5))
       endif
       call gen_var_block(g,g%codes(a+1))
       call cross_all_vars(g)
       g%lthis=save_lthis
    case(op_do_at)
       call use_var(g,g%codes(a))
       if(opcode2==0) then
          call use_var(g,g%codes(a+2))
          call use_var(g,g%codes(a+3))
       endif
       call gen_var_block(g,g%codes(a+1))
    case(op_assign)
       call use_var(g,g%codes(a))
       call use_var(g,g%codes(a+1))
       call use_var(g,g%codes(a+2),.true.)
    case default
       if(debug_g) write(*,*) 'VAR OP GENERIC>',op_names(opcode),g%codes(a:a+n-1)
       do i=0,n-1
          call use_var(g,g%codes(a+i))
       enddo
    end select

    if(debug_g) write(*,*) 'END VAR OP> ',l,op_names(opcode)

  end subroutine gen_var_op

  !=============================================================
  ! Variable assignment phase for a block with comm operations
  !=============================================================
  subroutine gen_var_comm_block(g,blk)
    type(gen_state):: g
    integer,intent(in):: blk
    integer:: save_lthis,ll
    save_lthis=g%lthis
    if(debug_g) write(*,*) 'VAR COMM BLK>',blk
    call g_new_frame(g)
    ll=blk
    do while(ll>0)
       g%lstack(g%lthis)%idx=g%lstack(g%lthis)%idx+1
       call gen_var_op(g,ll)
       ll=g%codes(ll)
    enddo
    g%lthis=save_lthis
    if(debug_g) write(*,*) 'VAR COMM BLK>',blk
  end subroutine gen_var_comm_block

  !=============================================================
  ! Variable assignment phase for inline shared proc body
  !=============================================================
  subroutine gen_var_shared_block(g,blk)
    type(gen_state):: g
    integer,intent(in):: blk
    integer:: save_lthis,ll
    if(debug_g) write(*,*) 'VAR SHARED BLK>',blk
    save_lthis=g%lthis
    call g_new_frame(g)
    g%lstack(g%lthis)%loop_mode=loop_is_none
    ll=blk
    do while(ll>0)
       g%lstack(g%lthis)%idx=g%lstack(g%lthis)%idx+1
       call gen_var_op(g,ll)
       ll=g%codes(ll)
    enddo
    call sort_var_list(g)
    g%lthis=save_lthis
    if(debug_g) write(*,*) 'END VAR SHARED BLK>',blk
  end subroutine gen_var_shared_block

  !=======================================================================
  ! Reverse list of vars (varlist/link) for current loop context
  ! to yield list in ascending order of start index (varlist/link)
  ! and also create sorted list by finish index yielding (evarlist/elink)
  !========================================================================
  subroutine sort_var_list(g)
    type(gen_state):: g
    integer:: v,v2
    integer:: finish,next,nextv
    v=g%lstack(g%lthis)%varlist
    g%lstack(g%lthis)%varlist=0
    g%lstack(g%lthis)%evarlist=0
    do while(v>0)
       nextv=g%vardata(v)%link
       g%vardata(v)%link=g%lstack(g%lthis)%varlist
       g%lstack(g%lthis)%varlist=v
       v2=g%lstack(g%lthis)%evarlist
       finish=g%vardata(v)%finish
       if(v2==0) then
          g%lstack(g%lthis)%evarlist=v
          g%vardata(v)%elink=0
       elseif(g%vardata(v2)%finish>=finish) then
          g%vardata(v)%elink=v2
          g%lstack(g%lthis)%evarlist=v
       else
          do while(g%vardata(v2)%elink>0)
             next=g%vardata(v2)%elink
             if(g%vardata(next)%finish>=finish) exit
             v2=next
          enddo
          g%vardata(v)%elink=g%vardata(v2)%elink
          g%vardata(v2)%elink=v
       endif
       v=nextv
    enddo

    if(debug_g) then
       write(*,*) 'REVERSE OUT VARLIST>'
       v=g%lstack(g%lthis)%varlist
       do while(v>0)
          write(*,*) v,g%vardata(v)%start
          v=g%vardata(v)%link
       enddo
       write(*,*) 'SORT OUT EVARLIST>'
       v=g%lstack(g%lthis)%evarlist
       do while(v>0)
          write(*,*) v,g%vardata(v)%finish
          v=g%vardata(v)%elink
       enddo
       write(*,*) 'END SORT>'
    endif
  end subroutine sort_var_list

  !==================================================
  ! Allocate variables on current list
  ! This merges variables that are of the same type
  ! but used at different times
  !==================================================
  subroutine alloc_var_list(g)
    type(gen_state):: g
    integer:: v,e,i
    if(debug_g) write(*,*) 'ALLOCATING>',g%lthis
    v=g%lstack(g%lthis)%varlist
    e=g%lstack(g%lthis)%evarlist
    do while(v/=0.and.e/=0)
       i=min(g%vardata(v)%start,g%vardata(e)%finish+1)
       do while(g%vardata(e)%finish+1==i)
          call deallocate_var(g,e)
          e=g%vardata(e)%elink
          if(e==0) exit
       enddo
       if(debug_g) then
          write(*,*) 'CONSIDER>',i,'#',v,g%vardata(v)%start,g%vardata(v)%finish,&
               e,g%vardata(e)%start,g%vardata(e)%finish,g%vardata(e)%finish_on_assign
       endif
       if(merge_vars(g,v,e,i)) then
          v=g%vardata(v)%link
          e=g%vardata(e)%elink
          cycle
       endif
       do while(g%vardata(v)%start==i)
          call allocate_var(g,v)
          v=g%vardata(v)%link
          if(v==0) exit
       enddo
    enddo
    do while(e/=0)
       call deallocate_var(g,e)
       e=g%vardata(e)%elink
    enddo
!!$    v=g%lstack(g%lthis)%varlist
!!$    do while(v/=0)
!!$       write(34,*) g%vardata(v)%index,g_var_at_index_is_a_vect(g,v)
!!$       v=g%vardata(v)%link
!!$    enddo
  end subroutine alloc_var_list

  !===========================================
  ! Allocate a single variable
  !===========================================
  subroutine allocate_var(g,v)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v
    integer:: tno,idx,isvect
    type(pm_ptr):: p
    integer(pm_ln):: j
    integer:: key(3)
    if(debug_g) write(*,*) 'Allocate ',v
    if(iand(g%vardata(v)%flags,v_is_param+v_is_chan+v_is_result+v_is_shared)==0) then
       isvect=merge(2,1,g_var_at_index_is_a_vect(g,v))
       tno=g%vardata(v)%tno
       if(tno<pm_int) then
          idx=0
          if(debug_g) write(*,*) 'no storage',tno
       elseif(tno>pm_string) then
          if(debug_g) write(*,*) 'Free hash',tno
          key(1)=g%lthis
          key(2)=isvect
          key(3)=tno
          j=pm_ivect_lookup(g%context,g%freehash,key,3)
          if(j>0) then
             p=pm_dict_val(g%context,g%freehash,j)
             idx=p%offset
             if(idx/=0) then
                p%offset=g%vardata(idx)%free
                call pm_dict_set_val(g%context,g%freehash,j,p)
                g%vardata(idx)%gflags=ior(g%vardata(idx)%gflags,var_is_recycled)
                idx=abs(g%vardata(idx)%index)
             else
                g%index=g%index+1
                idx=-v
             endif
          else
             idx=-v
          endif
       else
          if(debug_g) write(*,*) 'Freelist',tno,g%lstack(g%lthis)%free(isvect,tno)
          if(g%lstack(g%lthis)%free(isvect,tno)/=0) then
             idx=g%lstack(g%lthis)%free(isvect,tno)
             g%lstack(g%lthis)%free(isvect,tno)=g%vardata(idx)%free
             g%vardata(idx)%gflags=ior(g%vardata(idx)%gflags,var_is_recycled)
             idx=abs(g%vardata(idx)%index)
             if(debug_g) write(*,*) 'get free',idx
          else
             idx=-v
          endif
       endif
    else
       idx=0
    endif
    if(debug_g) write(*,*) 'ALLOCATED>',v,idx,g%lthis
    g%vardata(v)%index=idx
  end subroutine allocate_var

  !===========================================
  ! Deallocate a single variable
  !===========================================
  subroutine deallocate_var(g,v)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v
    integer:: tno,isvect
    type(pm_ptr):: p
    integer(pm_ln):: j
    integer:: key(3)
    if(g%vardata(v)%index/=0) then
       tno=g%vardata(v)%tno
       isvect=merge(2,1,g_var_at_index_is_a_vect(g,v))
       if(debug_g) write(*,*) 'Deallocate',v,tno,isvect
       if(tno<=pm_string) then
          g%vardata(v)%free=g%lstack(g%lthis)%free(isvect,tno)
          g%lstack(g%lthis)%free(isvect,tno)=v
       else
          key(1)=g%lthis
          key(2)=isvect
          key(3)=tno
          j=pm_ivect_lookup(g%context,g%freehash,key,3)
          if(j==0) then
             p=pm_fast_tinyint(g%context,v)
             j=pm_idict_add(g%context,g%freehash,key,3,p)
          else
             p=pm_dict_val(g%context,g%freehash,j)
             g%vardata(v)%free=p%offset
             p%offset=v
             call pm_dict_set_val(g%context,g%freehash,j,p)
          endif
       endif
    endif
  contains
    include 'ftiny.inc'
  end subroutine deallocate_var

  !===========================================
  ! Try to merge v and e into a single variable
  ! at index point i
  ! Returns .true. if successful
  !===========================================
  function merge_vars(g,v,e,i) result(merged)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v,e,i
    logical:: merged
    if(g%vardata(e)%finish==i.and.&
         g%vardata(v)%start==i.and.&
         g%vardata(e)%finish_on_assign.and.&
         iand(g%vardata(v)%flags,v_is_param+v_is_chan+v_is_result+v_is_shared)==0.and.&
         iand(g%vardata(e)%flags,v_is_param+v_is_chan+v_is_result+v_is_shared)==0.and.&
         g%vardata(v)%tno==g%vardata(e)%tno) then
       if(g%vardata(e)%index==0.or.&
            (g_var_at_index_is_a_vect(g,v).neqv.g_var_at_index_is_a_vect(g,e))) then
          merged=.false.
       else
          g%vardata(v)%index=abs(g%vardata(e)%index)
          g%vardata(e)%gflags=ior(g%vardata(e)%gflags,var_is_recycled)
          if(debug_g) then
             write(*,*) 'MERGED>',g%vardata(v)%index
          endif
          merged=.true.
       endif
    else
       merged=.false.
    endif
  end function merge_vars

  !=================================================================
  ! Use a variable - called in variable allocation phase
  ! Employs simple state engine to determine how a variable needs
  ! to be stored
  !=================================================================
  recursive subroutine use_var(g,avar,isassign)
    type(gen_state),intent(inout):: g
    integer,intent(in):: avar
    logical,intent(in),optional:: isassign
    integer:: kind,state,i,j,var,flags,tno
    integer,parameter,dimension(var_state_unused:var_state_closed):: new_state=(/&
         var_state_used,     &  ! var_state_unused
         var_state_used,     &  ! var_state_open
         var_state_used,     &  ! var_state_used
         var_state_crossing, &  ! var_state_used_before
         var_state_crossing, &  ! var_state_crossing
         var_state_used      &  ! var_state_closed
         /)
    if(avar==0.or.avar==shared_op_flag) return
    var=abs(avar)
    if(debug_g) write(*,*) 'USE VAR> ',var,g_index(g,var),g%lthis,g%lstack(g%lthis)%idx !,g_kind(g,var),g_v1(g,var)
    kind=g_kind(g,var)
    select case(kind)
    case(v_is_group)
       do i=1,g_v1(g,var)
          call use_var(g,g_ptr(g,var,i),isassign)
       enddo
    case(v_is_sub,v_is_vsub)
       call use_var(g,g_v1(g,var),isassign)
       call use_var(g,g_v2(g,var),isassign)
    case(v_is_elem,v_is_unit_elem,v_is_vect_wrapped)
       call use_var(g,g_v1(g,var),isassign)
       !g%varindex(var)=g%varindex(g_v1(g,var))
    case(v_is_const,v_is_ctime_const,v_is_parstmt_ve)
       continue
    case(v_is_cove)
       call use_var(g,g_v2(g,var),isassign)
       g%varindex(var)=g%varindex(g_v2(g,var))
    case(v_is_alias)
       call use_var(g,g_v1(g,var),isassign)
       g%varindex(var)=g%varindex(g_v1(g,var))
    case(v_is_chan_vect)
       call use_var(g,g_v1(g,var),isassign)
       g%varindex(var)=g%varindex(g_v1(g,var))
    case default
       i=g%varindex(var)
       if(i==0) then
          g%nvars=g%nvars+1
          if(debug_g) write(*,*) 'CREATE>',var,g%nvars
          i=g%nvars
          if(kind==v_is_parve) then
             g%vardata(i)%tno=pm_logical
             flags=v_is_param
             g%vardata(i)%name=0
          elseif(kind==v_is_ve) then
             call use_var(g,g_v1(g,var))
             g%vardata(i)%tno=pm_logical
             flags=0
             g%vardata(i)%name=0
          else
             flags=g_v2(g,var)
             tno=g_type(g,var)
             g%vardata(i)%tno=tno
             if(iand(flags,v_is_array_par_vect)==0) then
                g%vardata(i)%name=g_v1(g,var)
             else
                g%vardata(i)%name=0
             endif
          endif
          g%vardata(i)%flags=flags
          if(iand(g%taints,proc_is_comm)/=0.and.&
               iand(flags,v_is_param+v_is_result)/=0) then
             if(iand(flags,v_is_shared)/=0) then
                g%vardata(i)%state=var_state_used
                g%vardata(i)%lthis=g%lthis
             else
                g%vardata(i)%state=var_state_crossing
                if(iand(flags,v_is_result)/=0) then
                   g%vardata(i)%lthis=g%lthis
                else
                   g%vardata(i)%lthis=g%lthis+1
                endif
             endif
          else
             g%vardata(i)%lthis=g%lthis
             g%vardata(i)%state=var_state_used
          endif
          g%vardata(i)%outer_lthis=g%vardata(i)%lthis
          g%vardata(i)%start=g%lstack(g%lthis)%idx
          g%vardata(i)%finish=g%vardata(i)%start
          if(debug_g) write(*,*) 'START/FINISH=',g%vardata(i)%start
          g%vardata(i)%index=0
          g%vardata(i)%link=g%lstack(g%lthis)%varlist
          g%vardata(i)%gflags=0
          g%lstack(g%lthis)%varlist=i
          g%vardata(i)%oindex=var
          g%vardata(i)%finish_on_assign=.false.
          g%vardata(i)%free=0
          g%varindex(var)=i
          if(debug_g) write(*,*) 'NEW VAR>',var,i, g%vardata(i)%link
       else
          g%vardata(i)%state=new_state(g%vardata(i)%state)
          g%vardata(i)%finish=g%lstack(g%vardata(i)%lthis)%idx
          if(debug_g) then
             write(*,*) 'FINISH=',g%vardata(i)%start,g%vardata(i)%finish,g%vardata(i)%lthis
          endif
          g%vardata(i)%outer_lthis=g_common_frame(g,g%vardata(i)%outer_lthis,g%lthis)
          g%vardata(i)%gflags=ior(g%vardata(i)%gflags,&
               merge(var_is_reused,var_is_used,iand(g%vardata(i)%gflags,var_is_used)/=0))
          if(debug_g) then
             write(*,*) 'CONSIDER>',i,present(isassign)
          endif
          g%vardata(i)%finish_on_assign=present(isassign)
       endif
    end select
  end subroutine use_var

  !===========================================================
  ! In variable allocation phase - flag all active variables
  ! when a comm op is encountered
  !============================================================
  subroutine cross_all_vars(g)
    type(gen_state),intent(inout):: g
    integer:: var
    if(g%lstack(g%lthis)%loop_mode==loop_is_none) return
    g%loop_contains_shared=.true.
    if(debug_g) write(*,*) 'CROSS ALL'
    var=g%lstack(g%lthis)%varlist
    do while(var>0)
       if(debug_g) write(*,*) 'CROSS',var,'IN CROSS ALL'
       call cross_var_at_index(g,var)
       var=g%vardata(var)%link
    end do
    if(debug_g) write(*,*) 'CROSSED ALL'
  end subroutine cross_all_vars

  !============================================================
  ! Flag a single variable crossed by comm op
  !============================================================
  subroutine cross_var_at_index(g,i)
    type(gen_state),intent(inout):: g
    integer,intent(in):: i
    integer:: state
    integer,parameter,dimension(var_state_unused:var_state_closed):: new_state=(/&
         var_state_unused,         &  ! var_state_unused
         var_state_open,           &  ! var_state_open
         var_state_used_before,    &  ! var_state_used
         var_state_used_before,    &  ! var_state_used_before
         var_state_crossing,       &  ! var_state_crossing
         var_state_closed          &  ! var_state_closed
         /)
    if(g%lstack(g%lthis)%loop_mode==loop_is_none) return
    if(i==0) return
    if(g%vardata(i)%lthis/=g%lthis) return
    if(iand(g%vardata(i)%flags,v_is_shared)/=0) return
    state=g%vardata(i)%state
    if(debug_g) write(*,*) 'Crossing',i,state,new_state(state)
    g%vardata(i)%state=new_state(state)
  end subroutine cross_var_at_index

  !==================================================================
  ! Mark a variable as crossed (forced -not dependent on prior state)
  !==================================================================
  recursive subroutine cross_var(g,avar)
    type(gen_state),intent(inout):: g
    integer,intent(in):: avar
    integer:: i,var
    if(g%lstack(g%lthis)%loop_mode==loop_is_none) return
    if(avar==0.or.avar==shared_op_flag) return
    var=abs(avar)
    select case(g_kind(g,var))
    case(v_is_group)
       do i=1,g_v1(g,var)
          call cross_var(g,g_ptr(g,var,i))
       enddo
    case(v_is_sub,v_is_vsub)
       call cross_var(g,g_v1(g,var))
       call cross_var(g,g_v2(g,var))
    case(v_is_elem,v_is_unit_elem)
       call cross_var(g,g_v1(g,var))
    case(v_is_alias)
       call cross_var(g,g_v1(g,var))
    case(v_is_const,v_is_ctime_const,v_is_ve,v_is_cove)
       continue
    case default
       i=g%varindex(var)
       if(i/=0) then
          if(g%vardata(i)%lthis==g%lthis.and.&
               iand(g%vardata(i)%flags,v_is_shared)==0) then
             !g%vardata(i)%state=var_state_crossing#
             g%vardata(i)%gflags=ior(g%vardata(i)%gflags,var_is_comm_op_par)
          endif
       endif
    end select
  end subroutine cross_var

  !============================================================
  ! If a variable was created outside of an iterative loop
  ! then its lifetime must extend at least to the end of that
  ! loop
  !============================================================
  subroutine extend_finish_to_loop(g,loop_start_idx,loop_finish_idx,last_var_before_loop)
    type(gen_state):: g
    integer,intent(in):: loop_start_idx,loop_finish_idx,last_var_before_loop
    integer:: var
    ! Loop over vars created before loop
    var=last_var_before_loop
    do while(var>0)
       if(g%vardata(var)%lthis==g%lthis) then
          if(g%vardata(var)%finish>=loop_start_idx) then
             g%vardata(var)%finish=loop_finish_idx
          endif
       endif
       var=g%vardata(var)%link
    end do
  end subroutine extend_finish_to_loop


  !****************************************************
  ! PHASE II - Generating FORTRAN code
  !****************************************************
  
  !============================================================
  ! Generate code for a block of wordcode operations
  !============================================================
  recursive subroutine gen_block(g,loc) 
    type(gen_state):: g
    integer,intent(in):: loc
    integer:: l,save_last_ve
    save_last_ve=g%last_ve
    g%last_ve=0
    l=loc
    do while(l>0)
       call gen_op(g,l)
       l=g%codes(l)
    enddo
    call gen_close_ifs(g,save_last_ve)
    g%last_ve=save_last_ve
  end subroutine  gen_block

  !============================================================
  ! Generate code for single wordcode
  ! operation
  !============================================================
  recursive subroutine gen_op(g,loc)
    type(gen_state):: g
    integer,intent(in):: loc
    integer:: opcode,opcode2,n,a,arg,save_lthis,l,ll,i,j,k,m,tno
    logical:: ok,need_endif

    if(pm_debug_level>0) then
       if(loc+comp_op_arg0>size(g%codes)) then
          write(*,*) 'loc=',l,size(g%codes)
          call pm_panic('gen_op bad loc')
       endif
    endif
    
    l=loc
    
    opcode=g%codes(l+comp_op_opcode)
    opcode2=g%codes(l+comp_op_opcode2)
    n=iand(g%codes(l+comp_op_nargs),comp_op_nargs_mask)
    a=l+comp_op_arg0

    if(debug_g) write(*,*) 'GEN OP>',op_names(opcode),opcode2,n,'>',g%codes(a:a+n-1)

    if(pm_opts%ftn_comment_ops) then
       call out_str(g,'! '//trim(op_names(opcode)))
       do i=1,min(n,15)
          call out_char(g,' ')
          call out_idx(g,abs(g%codes(a+i-1)))
       enddo
       if(n>15) call out_str(g,' ...')
       call out_new_line(g)
    endif

    if(pm_opts%ftn_comment_lines) then
       call out_char_idx(g,'!',g%codes(l+comp_op_line)/modl_mult)
       call out_line(g,trim(pm_name_as_string(g%context,iand(g%codes(l+comp_op_line),modl_mult-1))))
    endif

    if(pm_opts%ftn_annotate) then
       call out_char_idx(g,'!',merge(1000,0,g%lstack(g%lthis)%loop_active)+g%lthis)
       call out_new_line(g)
    endif
    
    select case(opcode)
    case(op_if,op_if_shared)
       if(opcode==op_if_shared) call gen_loop(g,l,.true.)
       if(g%codes(a+1)/=0.or.g%codes(a+2)/=0) then
          call out_simple(g,'IF($3) THEN',l)
          call gen_block(g,g%codes(a+1))
          call gen_loop(g,l,.true.)
          call out_simple(g,'ELSE',l)
          call gen_block(g,g%codes(a+2))
          call gen_loop(g,l,.true.)
          call out_line(g,'ENDIF')
       endif
    case(op_if_shared_node)
       if(g%codes(a+1)/=0.or.g%codes(a+2)/=0) then
          call out_line(g,'IF(PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_NODE>1) THEN')
          call gen_block(g,g%codes(a+1))
          call out_line(g,'ELSE')
          call gen_block(g,g%codes(a+2))
          call out_line(g,'ENDIF')
       endif
    case(op_loop)
       if(opcode2/=0) then
          ! Loop contains shared operations so must
          ! be places outside parallel loops
          call gen_loop(g,l,.true.)
          if(g_is_vect(g,g%codes(a+2))) then
             call out_simple(g,'DO WHILE(ANY($#2))',l)
          else
             call out_simple(g,'DO WHILE($2)',l)
          endif
          call gen_block(g,g%codes(a+1))
          call gen_loop(g,l,.true.)
          call out_line(g,'ENDDO  ! LOOP')
       else
          call gen_loop(g,l,.false.)
          call out_simple(g,'DO WHILE($2)',l)
          call gen_block(g,g%codes(a+1))
          call out_line(g,'ENDDO  ! LOOP')
       endif
    case(op_comm_block)
       call gen_comm_block(g,l,g%codes(a+1),'$2')
    case(op_comm_proc)
       call gen_comm_block(g,l,g%codes(a+1),' ')
    case(op_inline_shared)
       if(pm_opts%ftn_annotate) call out_line(g,'! START SHARED BLOCK')
       call gen_shared_block(g,l,g%codes(a+1))
       if(pm_opts%ftn_annotate) call out_line(g,'! END SHARED BLOCK')
    case(op_comm_inline)
       call out_line(g,'op_comm_inline')
       call gen_loop(g,l,.true.)
    case(op_do_loop)
       call gen_loop(g,l,.true.)
       do i=n/2,1,-1
          call out_str(g,'DO I')
          call out_idx(g,i-1)
          call out_char_idx(g,'_',g%lthis)
          call out_str(g,'=0,-1+')
          call out_arg(g,g%codes(a+i+n/2),arg_no_index)
          call out_new_line(g)
       enddo
       do i=n/2,1,-1
          call out_arg(g,g%codes(a+i),0)
          call out_str(g,'=I')
          call out_idx(g,i-1)
          call out_char_idx(g,'_',g%lthis)
          call out_new_line(g)
       enddo
       call out_char_idx(g,'I',g%lthis)
       call out_str(g,'=1+')
       call out_char_idx(g,'I',0)
       call out_char_idx(g,'_',g%lthis)
       do i=2,n/2
          call out_char(g,'+')
          call out_arg(g,g%codes(a+i+n/2-1),0)
          call out_str(g,'*(I')
          call out_idx(g,i-1)
          call out_char_idx(g,'_',g%lthis)
       enddo
       do i=2,n/2
          call out_char(g,')')
       enddo
       call out_new_line(g)
       g%lstack(g%lthis)%nloops=n/2
       g%lstack(g%lthis)%loop_mode=loop_is_contig
       g%lstack(g%lthis)%loop_active=.true.
    case(op_nested_loop)
       call gen_loop(g,l,.true.)
       g%lstack(g%lthis)%loop_par=g%codes(a+1)
       g%lstack(g%lthis)%loop_mode=loop_is_nested
    case(op_blocked_loop)
       call gen_loop(g,l,.true.)
       g%lstack(g%lthis)%loop_par=g%codes(a+n-1)
       g%lstack(g%lthis)%loop_mode=loop_is_nested
       call gen_loop(g,l,.false.)
       do i=1,n-2
          call out_arg(g,g%codes(a+i),0)
          call out_str(g,'=I')
          call out_idx(g,i)
          call out_char_idx(g,'_',g%lthis)
          call out_new_line(g)
       enddo
    case(op_skip_empty)
       call gen_active_check_start(g,l)
       call gen_block(g,g%codes(a+1))
       call gen_active_check_end(g,l)
    case(op_head_node)
       call gen_loop(g,l,.true.)
       call out_line(g,'IF(PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_NODE==0) THEN')
       call gen_block(g,g%codes(a+1))
       call out_line(g,'ENDIF')
    case(op_over)
       call gen_loop(g,l,.true.)
       call gen_over_block(g,l,g%codes(a+1))
       call gen_loop(g,l,.true.)
    case(op_call)
       call gen_loop(g,l,.false.)
       if(pm_opts%ftn_annotate.and..not.pm_opts%ftn_name_procs) then
          call out_comment_line(g,trim(pm_name_as_string(g%context,g_procname(g,opcode2))))
       endif
       call out_str(g,'CALL PM__P')
       call out_idx(g,opcode2)
       if(pm_opts%ftn_name_procs) then
          call out_ftn_name(g,g_procname(g,opcode2))
       endif
       call out_char(g,'(')
       do i=1,n-1
          call out_call_arg(g,g%codes(a+i),0)
          call out_comma(g)
       enddo
       call out_close(g)
       call out_new_line(g)
    case(op_comm_call)
       if(g%codes(a)>0) then
          call gen_stacked_ve(g,l,g%codes(a))
       endif
       call gen_loop(g,l,.true.)
       if(pm_opts%ftn_annotate.and..not.pm_opts%ftn_name_procs) then
          call out_comment_line(g,trim(pm_name_as_string(g%context,g_procname(g,opcode2))))
       endif
       call out_str(g,'CALL PM__P')
       call out_idx(g,opcode2)
       if(pm_opts%ftn_name_procs) then
          call out_ftn_name(g,g_procname(g,opcode2))
       endif
       call out_str(g,'(N')
       call out_idx(g,g%lthis)
       call out_char(g,',')
       if(g%codes(a)>0) then
          call out_call_arg(g,g%codes(a),arg_no_index+arg_comm_arg)
          call out_char(g,',')
       endif
       do i=1,n-1
          call out_call_arg(g,g%codes(a+i),arg_no_index+arg_comm_arg)
          call out_comma(g)
       enddo
       call out_close(g)
       call out_new_line(g)
    case(op_comm_loop,op_comm_loop_par)
       call gen_loop(g,l,.true.)
       need_endif=.false.
       if(g_is_vect(g,g%codes(a+2))) then
          if(opcode==op_comm_loop_par) then
             call out_simple(g,'LOK=PM__TEST_LOOP(ANY($#2))',l)
          else
             call out_simple(g,'LOK=ANY($#2)',l)
          endif
          call out_line(g,'DO WHILE(LOK)')
          call out_simple(g,'IF($2) THEN',l)
          need_endif=.true.
       else
          call out_simple(g,'DO WHILE($2)',l)
       endif
       call gen_block(g,g%codes(a+1))
       call gen_loop(g,l,.true.)
       if(g_is_vect(g,g%codes(a+2))) then
          if(opcode==op_comm_loop_par) then
             call out_simple(g,'LOK=PM__TEST_LOOP(ANY($#2))',l)
          else
             call out_simple(g,'LOK=ANY($#2)')
          endif
       endif
       if(need_endif) call out_line(g,'ENDIF')
       call out_line(g,'ENDDO ! COMM LOOP')
    case(op_remote_call,op_remote_send_call)
       call gen_loop(g,l,.true.)
       if(pm_opts%ftn_annotate) call out_line(g,'!START REMOTE CALL')
       call gen_mpi_remote_call(g,l,opcode==op_remote_send_call)
       if(pm_opts%ftn_annotate) call out_line(g,'!END REMOTE CALL')
    case(op_server_call,op_collect_call)
       call gen_loop(g,l,.true.)
       call gen_mpi_collect_call(g,l,opcode==op_collect_call)
    case(op_bcast_call)
       call gen_loop(g,l,.true.)
       if(g%codes(a)==0) then
          call gen_mpi_bcast_call(g,l)
       else
          call gen_mpi_masked_bcast_call(g,l)
       endif
       
    case(op_do_at)
       call gen_loop(g,l,.false.)
       if(opcode2==1) then
          call out_simple(g,'IDO=I$N',n=g%lthis)
       else
          call out_simple(g,'IDO=$2+1',l)
       endif
       i=g_lthis(g,g%codes(a+merge(2,3,opcode2==1)))
       if(i/=0.and.g%lstack(i)%loop_active) then
          write(*,*) 'lthis=',g%lthis,'from',g%codes(a+3)
          call pm_panic('Loop active in op_do_at')
       endif
       g%lalt=i
       call gen_block(g,g%codes(a+1))
       g%lalt=-1
    case(op_get_size)
       call gen_loop(g,l,.true.)
       if(g_kind(g,g%codes(a+2))==v_is_group) then
          call out_simple_part(g,'$1=SIZE(',l)
          call out_arg(g,g_ptr(g,g%codes(a+2),1),0)
          call out_line(g,'%P)')
       else
          call out_simple(g,'$1=SIZE($2%E1%P)',l)
       endif
    case(op_dref,op_init_var)
       ! This does not generate code
       ! - just present for Phase I
       continue
    case(op_wrap)
       ! Save the current loop context for variable
       !write(*,*) 'WRAP',g%lthis,'to',g%codes(a+1)
       call g_set_v2(g,g%codes(a+1),g%lthis)

    case(op_sync)
       call gen_loop(g,l,.true.)
       
    case(op_isend)
       call gen_loop(g,l,.false.)
       call out_simple(g,'JNODE=$1',l)
       call gen_mpi_send(g,g%codes(a+2),'PM__DATA_TAG','ISEND',mode_array)
    case(op_irecv)
       call gen_loop(g,l,.false.)
       call out_simple(g,'JNODE=$1',l)
       call gen_mpi_recv(g,g%codes(a+2),'PM__DATA_TAG','IRECV',mode_array,.false.)
    case(op_recv)
       call gen_loop(g,l,.false.)
       call out_simple(g,'JNODE=$1',l)
       call gen_mpi_recv(g,g%codes(a+2),'PM__DATA_TAG','RECV',mode_array,.false.)
       
    case(op_isend_grid)
       call gen_loop(g,l,.false.)
       call out_simple(g,'JNODE=$2',l)
       call gen_mpi_send_disp_or_grid(g,g%codes(a+3),'PM__DATA_TAG','ISEND',g%codes(a+1),mode_array)
    case(op_irecv_grid)
       call gen_loop(g,l,.false.)
       call out_simple(g,'JNODE=$2',l)
       call gen_mpi_recv_disp_or_grid(g,g%codes(a+3),'PM__DATA_TAG','IRECV',g%codes(a+1),mode_array,.false.)       
    case(op_recv_grid)
       call gen_loop(g,l,.false.)
       call out_simple(g,'JNODE=$2',l)
       call gen_mpi_recv_disp_or_grid(g,g%codes(a+3),'PM__DATA_TAG','RECV',g%codes(a+1),mode_array,.false.)
    case(op_isend_offset)
       call gen_active_check_start(g,l)
       call out_simple(g,'JNODE=$2',l)
       call gen_mpi_send_disp_or_grid(g,g%codes(a+3),'PM__DATA_TAG','ISEND',g%codes(a+1),mode_array)
       call gen_active_check_end(g,l)
    case(op_irecv_offset)
       call gen_active_check_start(g,l)
       call out_simple(g,'JNODE=$2',l)
       call gen_mpi_recv_disp_or_grid(g,g%codes(a+3),'PM__DATA_TAG','IRECV',g%codes(a+1),mode_array,.false.)
       call gen_active_check_end(g,l)
    case(op_recv_offset)
       call gen_active_check_start(g,l)
       call out_simple(g,'JNODE=$2',l)
       call gen_mpi_recv_disp_or_grid(g,g%codes(a+3),'PM__DATA_TAG','RECV',g%codes(a+1),mode_array,.false.)
       call gen_active_check_end(g,l)
    case(op_isend_reply)
       call gen_active_check_start(g,l)
       call out_simple(g,'JNODE=$1',l)
       call gen_mpi_send(g,g%codes(a+2),'PM__DATA_TAG','RSEND',mode_vect)
       call gen_active_check_end(g,l)
       
    case(op_sync_mess)
       call gen_loop(g,l,.true.)
       call gen_sync_mess(g,l,a,n)
       
    case(op_broadcast)
       call gen_active_check_start(g,l)
       call out_simple(g,'JNODE=$2',l)
       call gen_mpi_bcast(g,g%codes(a+1))
       call gen_active_check_end(g,l)
    case(op_broadcast_shared)
       call gen_active_check_start(g,l)
       if(n==3) then
          call out_simple(g,'JNODE=$2',l)
       else
          call out_line(g,'JNODE=0')
       endif
       call gen_mpi_bcast(g,g%codes(a+1),isshared=.true.)
       call gen_active_check_end(g,l)
    case(op_broadcast_val)
       call gen_active_check_start(g,l)
       call out_simple(g,'JNODE=$3',l)
       call out_simple(g,&
            'IF(JNODE.EQ.PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NODE) $1=$2',l)
       call gen_mpi_bcast(g,g%codes(a+1))
       call gen_active_check_end(g,l)
       
    case(op_bcast_shared_offset)
       call gen_active_check_start(g,l)
       call gen_mpi_bcast_disp_or_grid(g,g%codes(a+2),.true.,g%codes(a+1),mode_array)
       call gen_active_check_end(g,l)
       
    case(op_bcast_shared_grid)
       call gen_loop(g,l,.false.)
       call gen_mpi_bcast_disp_or_grid(g,g%codes(a+2),.true.,g%codes(a+1),mode_array)

    case(op_isend_req,op_isend_assn)
       call gen_active_check_start(g,l)
       !call out_line(g,'write(*,*) "ISEND_ASSN"')
       call out_simple(g,'JNODE=$2',l)
       if(n>6) then
          call out_simple(g,'ALLOCATE RBUFFER%P(N$N)',n=g%lthis)
          call out_simple(g,'CALL PM__MASK_OFFSETS(N$N,$1,RBUFFER%P,$6,NREQ)',l,n=g%lthis)
          call out_line(g,'JCOMM=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM')
          call out_line(g,'CALL MPI_ISEND(NREQ,1,MPI_AINT,JNODE,PM__REQ_TAG,JCOMM,JMESS,JERRNO)')
          call out_line(g,'PM__PUSH_MESSAGE(JMESS)')
          call gen_mpi_send_part(g,g%codes(a+3),'PM__EXTRA_REQ_TAG','ISEND','RBUFFER%P','1','NREQ',mode_vect)
          if(opcode==op_isend_req) then
             call gen_mpi_recv_part(g,g%codes(a+3),'PM__DATA_TAG','IRECV',.false.,'RBUFFER%P','1','NREQ',mode_vect)
          else
             call gen_mpi_send_part(g,g%codes(a+3),'PM__EXTRA_REQ_TAG','ISEND','RBUFFER%P','1','NREQ',mode_vect)
          endif
       else
          if(g_kind(g,g%codes(a+1))==v_is_group) then
             call out_simple(g,'NREQ=$X',x=g_ptr(g,g%codes(a+1),2))
          else
             call out_simple(g,'NREQ=SIZE($#1)',l)
          endif
          call out_line(g,'JCOMM=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM')
          call out_line(g,'CALL MPI_ISEND(NREQ,1,MPI_AINT,JNODE,PM__REQ_TAG,JCOMM,JMESS,JERRNO)')
          call gen_mpi_send_disp_or_grid(g,g%codes(a+3),'PM__EXTRA_REQ_TAG','ISEND',g%codes(a+1),mode_vect)
          if(opcode==op_isend_req) then
             call gen_mpi_recv_disp_or_grid(g,g%codes(a+4),'PM__DATA_TAG','IRECV',g%codes(a+1),mode_vect,.false.)
          else
             call gen_mpi_send_disp_or_grid(g,g%codes(a+4),'PM__EXTRA_REQ_TAG','ISEND',g%codes(a+1),mode_vect)
          endif
       endif
       call gen_active_check_end(g,l)
       
    case(op_recv_reply)
       call gen_active_check_start(g,l)
       call out_simple(g,'JNODE=$2',l)
       if(n>4) then
          call out_simple(g,'ALLOCATE RBUFFER%P(N$N)',n=g%lthis)
          call out_simple(g,'CALL PM__MASK_OFFSETS(N$N,$1,RBUFFER%P,$5,NREQ)',l,n=g%lthis)
          call out_line(g,'JCOMM=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM')
          call gen_mpi_recv_part(g,g%codes(a+3),'PM__DATA_TAG','RECV',.true.,'RBUFFER%P','1','NREQ',mode_vect)
       else
          call gen_mpi_recv_disp_or_grid(g,g%codes(a+4),'PM__DATA_TAG','RECV',g%codes(a+1),mode_vect,.true.)
       endif
       call gen_active_check_end(g,l)
    case(op_recv_req_calL,op_recv_assn_call)
       call gen_active_check_start(g,l)
       call gen_mpi_recv_call(g,l,g%codes(a+1),opcode==op_recv_assn_call)
       call gen_active_check_end(g,l)

       
    case(op_active)
       call gen_stacked_ve(g,l,g%codes(a),g%codes(a+1))
       call gen_loop(g,l,.true.)
    case(op_wshare)
       call out_simple_scalar(g,'$1=PM__WSHARE($2%E1%P,$3,$4,$5)',l)
    case(op_sys_node)
       call out_simple_scalar(g,'$1=PM__SYS_NODE',l)
    case(op_sys_nnode)
       call out_simple_scalar(g,'$1=PM__SYS_NNODE',l)
    case(op_this_node)
       call out_simple_scalar(g,'$1=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NODE',l)
    case(op_this_nnode)
       call out_simple_scalar(g,'$1=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NNODE',l)
    case(op_shared_node)
       call out_simple_scalar(g,'$1=PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_NODE',l)
    case(op_shared_nnode)
       call out_simple_scalar(g,'$1=PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_NNODE',l)
    case(op_is_shared)
       call  out_simple_scalar(g,'$1=PM__NODE_FRAME(PM__NODE_DEPTH)%IS_SHARED',l)
    case(op_is_par)
       call  out_simple_scalar(g,'$1=PM__CONC_DEPTH.EQ.0',l)
    case(op_get_dims)
       call gen_loop(g,l,.false.)
       m=(n-2)/2
       if(m==1) then
          call out_simple(g,'$1=MERGE($3,$2,$3.GT.0.AND.$2.NE.1)',l)
       else
          call out_simple(g,'IF($X.GT.1)THEN',l,x=m+1)
          do i=m+2,m+m+1
             call out_simple(g,'Z%P($N)=MERGE(-1,INT($X),$X>HUGE(1))',l,n=i-m-1,x=i)
          enddo
          call out_simple(g,'CALL PM__GET_DIMS(INT($X),$N,Z%P)',l,n=m,x=m+1)
          do i=1,m
             call out_simple(g,'$X=Z%P($N)',l,n=i,x=i)
          enddo
          call out_line(g,'ELSE')
          do i=1,m
             call out_simple(g,'$X=1',l,x=i)
          enddo
          call out_line(g,'ENDIF')
       endif
    case(op_push_node_split)
       call out_simple_scalar(g,'CALL PM__PUSH_NODE_SPLIT($1)',l)
    case(op_push_node_distr)
       call gen_loop(g,l,.false.)
       call out_line(g,'CALL PM__PUSH_NODE_DISTR()')
    case(op_push_node_conc)
       call gen_loop(g,l,.false.)
       call out_line(g,'CONC_DEPTH=CONC_DEPTH+1')
    case(op_pop_node_conc)
       call gen_loop(g,l,.false.)
       call out_line(g,'CONC_DEPTH=CONC_DEPTH-1')
    case(op_pop_node)
       call gen_loop(g,l,.true.)
       call out_line(g,'CALL PM__POP_NODE()')
   case(op_make_poly)
       call out_simple_scalar(g,'IF(ALLOCATED($1%P)) DEALLOCATE($1%P)',l) 
       if(g_type(g,g%codes(a+2))<=pm_string) then
          call out_simple(g,'ALLOCATE($1%P,SOURCE=PM__BOX$S($2))',l,&
               x=g_type(g,g%codes(a+2)))
       else
          call out_simple(g,'ALLOCATE($1%P,SOURCE=$2)',l)
       endif 
    case(op_any)
       call out_simple_scalar(g,'$1=.FALSE.',l)
       call out_simple(g,'SELECT TYPE(POLYVAR=>$3%P)',l)
       if(opcode2<=pm_string) then
          call out_simple(g,'TYPE IS(PM__T$S)',x=opcode2)
          call out_simple(g,'$2=POLYVAR%P',l)
       else
          call out_simple(g,'TYPE IS(PM__T$N)',n=opcode2)
          call out_simple(g,'$2=POLYVAR',l)
       endif
       call out_simple(g,'$1=.TRUE.',l)
       call out_line(g,'END SELECT')

    case(op_intersect_seq)
       call out_simple_scalar(g,&
            'CALL PM__INTERSECT_SEQ($5,$6,$7,$8,$9,$(10),$(11),$(12),$1,$2,$3,$4)',l)
    case(op_intersect_aseq)
       if(opcode2==0) then
          call out_simple_scalar(g,&
               'CALL PM__INTERSECT_ASEQ($2%E1%P,$3,$4%E1%P,$5,$6%E1%P,$1)',l)
       elseif(opcode2==1) then
          call out_simple_scalar(g,&
               'CALL PM__OVERLAP_ASEQ($2%E1%P,$3,$4%E1%P,$5,$6%E1%P,$1)',l)
       else
          call out_simple_scalar(g,&
               'CALL PM__OVERLAP_ASEQ2($2%E1%P,$3,$4%E1%P,$5,$6%E1%P,$7%E1%P,$1)',l)
       endif
    case(op_intersect_bseq)
       if(opcode2==0) then
          call out_simple_scalar(g,&
               'CALL PM__INTERSECT_BSEQ($3,$4,$5,$6,$7,$8,$9,$(10),$(11),$(12),$2%E1%P,$1)',l)
       elseif(opcode2==1) then
          call out_simple_scalar(g,&
               'CALL PM__OVERLAP_BSEQ($3,$4,$5,$6,$7,$8,$9,$(10),$(11),$(12),$2%E1%P,$1)',l)
       else
          call out_simple_scalar(g,&
            'CALL PM__OVERLAP_BSEQ2($4,$5,$6,$7,$8,$9,$(10),$(11),$(12),$(13),$2%E1%P,$3%E1%P,$1)',l)
       endif
    case(op_expand_aseq)
       call out_simple_scalar(g,'PM__EXPAND_ASEQ($3%E1%P,$4,$5,$6,$2%E1%P,$1)',l)
    case(op_includes_aseq)
       call out_simple_scalar(g,'$1=PM__ASEQ_INCLUDES($2%E1%P,$3,$4%E1%P,$5)',l)
    case(op_index_aseq)
       call out_simple_scalar(g,'$1=PM__ASEQ_INDEX($2%E1%P,$3,$4)',l)
    case(op_in_aseq)
       call out_simple_scalar(g,'INDEX=PM__ASEQ_INDEX($2%E1%P,$3,$4)',l)
       call out_simple_scalar(g,'$1=INDEX>=0.and.INDEX<$3',l)
       call out_simple_scalar(g,'IF($1) $1=$2%E1%P(INDEX+1)==$4',l)
    case(op_assign)
       if(.not.(g_vars_are_merged(g,g%codes(a+1),g%codes(a+2)).or.&
            g_var_is_dead(g,g%codes(a+1)))) then
          if(pm_opts%ftn_annotate) then
             call out_simple(g,'!ASSIGN (opcode2=$N)',n=abs(opcode2))
          endif
          call out_simple_scalar(g,'$1=$2',l)
       else
          if(pm_opts%ftn_annotate) then
             call out_comment_line(g,'Eliminated merge assign:'//&
                  merge('Y','N',g_var_is_dead(g,g%codes(a+1)))//&
                  merge('Y','N',g_vars_are_merged(g,g%codes(a+1),g%codes(a+2))))
          endif
       endif
    case(op_init_farray)
       if(pm_opts%ftn_annotate) then
          call out_comment_line(g,'! INIT FARRAY')
       endif
       call out_simple_scalar(g,'$1%E1%P=$2',l)
    case(op_assign_farray)
       if(pm_opts%ftn_annotate) then
          call out_comment_line(g,'! ASSIGN FARRAY')
       endif
       call out_simple_scalar(g,'$1=$2',l)
    case(op_and_ve)
       if(pm_opts%ftn_annotate) then
          call out_comment_line(g,'! AND_VE')
       endif
       call out_simple_scalar(g,'$1=$2',l)
    case(op_andnot_ve)
       if(pm_opts%ftn_annotate) then
          call out_comment_line(g,'! AND_NOT_VE')
       endif
       call out_simple_scalar(g,'$1=.NOT.($2)',l)
    case(op_print)
       call out_simple_scalar(g,'CALL PM__PRINT($1)',l)
    case(op_concat)
       call out_simple_scalar(g,'$1=PM__CONCAT_STR($2,$3)',l)
    case(op_check)
       call out_simple_scalar(g,'IF(.NOT.$2) CALL PM__ABORT($1)',l)
    
    case(op_array,op_var_array)
       ! V n x
       call out_simple_scalar(g,'IF(ALLOCATED($1%E1%P)) DEALLOCATE($1%E1%P)',l)
       call out_simple(g,'ALLOCATE($1%E1%P(MAX($4,1)),SOURCE=$2)',l)
       call out_simple(g,'$1%E2=$3',l)
       if(g_type(g,g%codes(a+3))==pm_long) then
          call out_simple(g,'$1%E3=$4',l)
       endif
    case(op_fill)
       call gen_loop(g,l,.false.)
       call out_simple(g,'DO IJ=$N+1,$3',l,n=opcode2)
       call out_arg(g,g%codes(a+1),arg_no_index)
       call out_char(g,'(')
       if(g_is_vect(g,g%codes(a+1))) then
          call out_str(g,'IJ,I')
          call out_idx(g,g%lthis)
          call out_str(g,')=')
       else
          call out_str(g,'IJ)=')
       endif
       call out_arg(g,g%codes(a+2),0)
       call out_new_line(g)
       call out_line(g,'ENDDO')
    case(op_break_loop)
       call gen_loop(g,l,.true.)
    case(op_get_rf)
       call out_simple_scalar(g,'$1=$2',l)
    case(op_open_file)
       call out_simple_scalar(g,'$1=PM__FILE_OPEN($3%P,$4,$5,$6,$7,$8,$9,$(10),$2)',l)
    case(op_close_file)
       call out_simple_scalar(g,'CALL MPI_FILE_CLOSE($2,$1)',l)
    case(op_seek_file)
       call out_simple_scalar(g,'CALL MPI_FILE_SEEK($2,$3,MPI_SEEK_SET,$1)',l)
    case(op_read_file)
       call out_get_mpi_base_type(g,g_type(g,g%codes(a+3)))
       call out_simple_scalar(g,'CALL MPI_FILE_READ_ALL($2,$3,1,JBASE,MPI_STATUS_IGNORE,$1)',l)
    case(op_write_file)
       call gen_loop(g,l,.false.)
       call out_line(g,'IF(PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_NODE==0) THEN')
       call out_get_mpi_base_type(g,g_type(g,g%codes(a+3)))
       call out_simple(g,'CALL MPI_FILE_WRITE($2,$3,1,JBASE,MPI_STATUS_IGNORE,$1)',l,&
            x=g_type(g,g%codes(a+3)))
       call out_line(g,'ENDIF')
       call out_simple(g,'CALL PM__SYNC_FILE_WRITE($2,$1)',l)
    case(op_read_file_array)
       call gen_loop(g,l,.false.)
       call out_get_mpi_base_type(g,g_type(g,g%codes(a+3)))
       call out_simple(g,'CALL PM__GET_MPI_TYPE(JBASE,$4,JTYPE,JN,LNEW)',l)
       call out_simple(g,'CALL MPI_FILE_READ_ALL($2,$3,JN,JTYPE,MPI_STATUS_IGNORE,$1)',l)
       call out_line(g,'IF(LNEW) CALL MPI_TYPE_FREE(JTYPE,JERRNO)')
    case(op_write_file_array)
       call gen_loop(g,l,.false.)
       call out_line(g,'IF(PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_NODE==0) THEN')
       call out_get_mpi_base_type(g,g_type(g,g%codes(a+3)))
       call out_simple(g,'CALL PM__GET_MPI_TYPE(JBASE,$4,JTYPE,JN,LNEW)',l)
       call out_simple(g,'CALL MPI_FILE_WRITE($2,$3,JN,JTYPE,MPI_STATUS_IGNORE,$1)',l)
       call out_line(g,'IF(LNEW) CALL MPI_TYPE_FREE(JTYPE,JERRNO)')
       call out_line(g,'ENDIF')
       call out_simple(g,'CALL PM__SYNC_FILE_WRITE($2,$1)',l)
    case(op_read_file_tile,op_write_file_tile)
       call gen_loop(g,l,.true.)
       call out_get_mpi_base_type(g,g_type(g,g%codes(a+3)))
       call out_simple(g,'CALL PM__FILE_SET_VIEW($2,JBASE,$#4,N$N,$5,$1,OFFSET)',l,&
            n=g%lthis)
       call out_simple(g,'IF($1==0) THEN',l)
       call out_get_mpi_base_type(g,g_type(g,g%codes(a+5)))
       call out_simple(g,'CALL PM__GET_MPI_TYPE(JBASE,N$N,JTYPE,JN,LNEW)',l,n=g%lthis)
       if(opcode==op_read_file_tile) then
          call out_simple(g,'CALL MPI_FILE_READ_ALL($2,$#3,JN,JTYPE,MPI_STATUS_IGNORE,$1)',l)
       else 
          call out_simple(g,'CALL MPI_FILE_WRITE_ALL($2,$#3,JN,JTYPE,MPI_STATUS_IGNORE,$1)',l)
       endif
       call out_line(g,'IF(LNEW) CALL MPI_TYPE_FREE(JTYPE,JERRNO)')
       call out_line(g,'ENDIF')
       call out_simple(g,'CALL PM__FILE_RESET_VIEW($2,OFFSET,$1)',l)
    case(op_io_error_string)
       call out_simple_scalar(g,'$1=PM__IO_ERROR_STRING($2)',l)
       
    case(op_and)
       call out_simple_scalar(g,'$1=$2.AND.$3',l)
    case(op_or)
       call out_simple_scalar(g,'$1=$2.OR.$3',l)
    case(op_eq_l)
       call out_simple_scalar(g,'$1=$2.EQV.$3',l)
    case(op_ne_l)
       call out_simple_scalar(g,'$1=$2.NEQV.$3',l)
    case(op_not)
       call out_simple_scalar(g,'$1=.NOT.$2',l)
    case(op_assign_l)
       call out_simple_scalar(g,'$1=$2',l)

    case(op_string_i)
       call out_simple_scalar(g,'$1=PM__INT_TO_STR($2)',l)
    case(op_string_ln)
       call out_simple_scalar(g,'$1=PM__LONG_TO_STR($2)',l)
    case(op_string_offset)
       call out_simple_scalar(g,'$1=PM__OFFSET_TO_STR($2)',l)
    case(op_string_i64)
       call out_simple_scalar(g,'$1=PM__INT64_TO_STR($2)',l)
    case(op_string_r)
       call out_simple_scalar(g,'$1=PM__REAL_TO_STR($2)',l)
    case(op_string_l)
       call out_simple_scalar(g,'$1=PM__BOOL_TO_STR($2)',l)
    case(op_string_d)
       call out_simple_scalar(g,'$1=PM__DOUBLE_TO_STR($2)',l)
    case(op_add_i,op_add_ln,op_add_offset,op_add_i8,op_add_i16,op_add_i32,&
         op_add_i64,op_add_r,op_add_d,op_add_c,op_add_dc)
       call out_simple_scalar(g,'$1=$2+$3',l)
    case(op_sub_i,op_sub_ln,op_sub_offset,op_sub_i8,op_sub_i16,op_sub_i32,&
         op_sub_i64,op_sub_r,op_sub_d,op_sub_c,op_sub_dc)
       call out_simple_scalar(g,'$1=$2-$3',l)
    case(op_mult_i,op_mult_ln,op_mult_offset,op_mult_i8,op_mult_i16,op_mult_i32,&
         op_mult_i64,op_mult_r,op_mult_d,op_mult_c,op_mult_dc)
       call out_simple_scalar(g,'$1=$2*$3',l)
    case(op_divide_i,op_divide_ln,op_divide_offset,op_divide_i8,op_divide_i16,op_divide_i32,&
         op_divide_i64,op_divide_r,op_divide_d,op_divide_c,op_divide_dc)
       call out_simple_scalar(g,'$1=$2/$3',l)
    case(op_mod_i,op_mod_ln,op_mod_offset,op_mod_i8,op_mod_i16,op_mod_i32,&
         op_mod_i64,op_mod_r,op_mod_d)
       call out_simple_scalar(g,'$1=MODULO($2,$3)',l)
    case(op_pow_i,op_pow_ln,op_pow_offset,op_pow_i8,op_pow_i16,op_pow_i32,&
         op_pow_i64,op_pow_r,op_pow_d,op_pow_c,op_rpow_c,op_pow_dc,op_dpow_dc)
       call out_simple_scalar(g,'$1=$2**$3',l)
    case(op_uminus_i,op_uminus_ln,op_uminus_offset,op_uminus_i8,op_uminus_i16,op_uminus_i32,&
         op_uminus_i64,op_uminus_r,op_uminus_d,op_uminus_c,op_uminus_dc)
       call out_simple_scalar(g,'$1=-$2',l)
    case(op_eq_i,op_eq_ln,op_eq_offset,op_eq_i8,op_eq_i16,op_eq_i32,&
         op_eq_i64,op_eq_r,op_eq_d,op_eq_c,op_eq_dc)
       call out_simple_scalar(g,'$1=$2.EQ.$3',l)
    case(op_ne_i,op_ne_ln,op_ne_offset,op_ne_i8,op_ne_i16,op_ne_i32,&
         op_ne_i64,op_ne_r,op_ne_d,op_ne_c,op_ne_dc)
       call out_simple_scalar(g,'$1=$2.NE.$3',l)
    case(op_gt_i,op_gt_ln,op_gt_offset,op_gt_i8,op_gt_i16,op_gt_i32,&
         op_gt_i64,op_gt_r,op_gt_d)
       call out_simple_scalar(g,'$1=$2.GT.$3',l)
    case(op_ge_i,op_ge_ln,op_ge_offset,op_ge_i8,op_ge_i16,op_ge_i32,&
         op_ge_i64,op_ge_r,op_ge_d)
       call out_simple_scalar(g,'$1=$2.GE.$3',l)
    case(op_max_i,op_max_ln,op_max_offset,op_max_i8,op_max_i16,op_max_i32,&
         op_max_i64,op_max_r,op_max_d)
       call out_simple_scalar(g,'$1=MAX($2,$3)',l)
    case(op_min_i,op_min_ln,op_min_offset,op_min_i8,op_min_i16,op_min_i32,&
         op_min_i64,op_min_r,op_min_d)
       call out_simple_scalar(g,'$1=MIN($2,$3)',l)
    case(op_assign_i,op_assign_ln,op_assign_offset,op_assign_i8,op_assign_i16,op_assign_i32,&
         op_assign_i64,op_assign_r,op_assign_d,op_assign_c,op_assign_dc)
       call out_simple_scalar(g,'$1=$2',l)
    case(op_abs_i,op_abs_ln,op_abs_offset,op_abs_i8,op_abs_i16,op_abs_i32,&
         op_abs_i64,op_abs_r,op_abs_d,op_abs_c,op_abs_dc)
       call out_simple_scalar(g,'$1=ABS($2)',l)
    case(op_pdiff_i,op_pdiff_ln,op_pdiff_offset,op_pdiff_i8,op_pdiff_i16,op_pdiff_i32,&
         op_pdiff_i64,op_pdiff_r,op_pdiff_d)
       call out_simple_scalar(g,'$1=DIM($2,$3)',l)
    case(op_sign_i,op_sign_ln,op_sign_offset,op_sign_i8,op_sign_i16,op_sign_i32,&
         op_sign_i64,op_sign_r,op_sign_d)
       call out_simple_scalar(g,'$1=SIGN($2,$3) ! SIGN',l)
    case(op_modulo_i,op_modulo_ln,op_modulo_offset,op_modulo_i8,op_modulo_i16,op_modulo_i32,&
         op_modulo_i64,op_modulo_r,op_modulo_d)
       call out_simple_scalar(g,'$1=MOD($2,$3)',l)
 
    case(op_bnot_i,op_bnot_ln,op_bnot_offset,&
         op_bnot_i8,op_bnot_i16,op_bnot_i32,op_bnot_i64)
       call out_simple_scalar(g,'$1=NOT($2)',l)
    case(op_band_i,op_band_ln,op_band_offset,&
         op_band_i8,op_band_i16,op_band_i32,op_band_i64)
       call out_simple_scalar(g,'$1=IAND($2,$3)',l)
    case(op_bor_i,op_bor_ln,op_bor_offset,&
         op_bor_i8,op_bor_i16,op_bor_i32,op_bor_i64)
       call out_simple_scalar(g,'$1=IOR($2,$3)',l)
    case(op_bxor_i,op_bxor_ln,op_bxor_offset,&
         op_bxor_i8,op_bxor_i16,op_bxor_i32,op_bxor_i64)
       call out_simple_scalar(g,'$1=IEOR($2,$3)',l)
    case(op_bshift_i,op_bshift_ln,op_bshift_offset,&
         op_bshift_i8,op_bshift_i16,op_bshift_i32,op_bshift_i64)
       call out_simple_scalar(g,'$1=ISHFT($2,$3)',l)

    case(op_acos_r,op_acos_d,op_acos_c,op_acos_dc)
       call out_simple_scalar(g,'$1=ACOS($2)',l)
    case(op_asin_r,op_asin_d,op_asin_c,op_asin_dc)
       call out_simple_scalar(g,'$1=ASIN($2)',l)
    case(op_atan_r,op_atan_d,op_atan_c,op_atan_dc)
       call out_simple_scalar(g,'$1=ATAN($2)',l)
    case(op_atan2_r,op_atan2_d)
       call out_simple_scalar(g,'$1=ATAN2($2,$3)',l)
    case(op_cos_r,op_cos_d,op_cos_c,op_cos_dc)
       call out_simple_scalar(g,'$1=COS($2)',l)
    case(op_cosh_r,op_cosh_d,op_cosh_c,op_cosh_dc)
       call out_simple_scalar(g,'$1=COSH($2)',l)
    case(op_exp_r,op_exp_d,op_exp_c,op_exp_dc)
       call out_simple_scalar(g,'$1=EXP($2)',l)
    case(op_log_r,op_log_d,op_log_c,op_log_dc)
       call out_simple_scalar(g,'$1=LOG($2)',l)
    case(op_log10_r,op_log10_d)
       call out_simple_scalar(g,'$1=LOG10($2)',l)
    case(op_sin_r,op_sin_d,op_sin_c,op_sin_dc)
       call out_simple_scalar(g,'$1=SIN($2)',l)
    case(op_sinh_r,op_sinh_d,op_sinh_c,op_sinh_dc)
       call out_simple_scalar(g,'$1=SINH($2)',l)
    case(op_sqrt_r,op_sqrt_d,op_sqrt_c,op_sqrt_dc)
       call out_simple_scalar(g,'$1=SQRT($2)',l)
    case(op_tan_r,op_tan_d,op_tan_c,op_tan_dc)
       call out_simple_scalar(g,'$1=TAN($2)',l)
    case(op_tanh_r,op_tanh_d,op_tanh_c,op_tanh_dc)
       call out_simple_scalar(g,'$1=TANH($2)',l)
    case(op_floor_r,op_floor_d)
       call out_simple_scalar(g,'$1=FLOOR($2)',l)
    case(op_ceil_r,op_ceil_d)
       call out_simple_scalar(g,'$1=CEILING($2)',l)

    case(op_imag_c,op_imag_dc)
       call out_simple_scalar(g,'$1=IMAG($2)',l)
    case(op_conj_c,op_conj_dc)
       call out_simple_scalar(g,'$1=CONJG($2)',l)

    case(op_int_ln,op_int_offset,op_int_r,op_int_d,&
         op_int_i8,op_int_i16,op_int_i32,op_int_i64)
       call out_simple_scalar(g,'$1=INT($2)',l)
    case(op_long_i,op_long_offset,op_long_r,op_long_d,&
         op_long_i8,op_long_i16,op_long_i32,op_long_i64)
       call out_simple_scalar(g,'$1=INT($2,KIND=PM__LN)',l)
    case(op_offset_i,op_offset_ln,op_offset_r,op_offset_d,&
         op_offset_i8,op_offset_i16,op_offset_i32,op_offset_i64)
       call out_simple_scalar(g,'$1=INT($2,KIND=PM__LN)',l)
    case(op_real_i,op_real_ln,op_real_offset,op_real_d,&
         op_real_i8,op_real_i16,op_real_i32,op_real_i64,op_real_c)
       call out_simple_scalar(g,'$1=REAL($2)',l)
    case(op_double_i,op_double_ln,op_double_offset,op_double_r,&
         op_double_i8,op_double_i16,op_double_i32,op_double_i64,&
         op_real_dc)
       call out_simple_scalar(g,'$1=REAL($2,KIND=PM__D)',l)
    case(op_i8_i,op_i8_ln,op_i8_offset,&
         op_i8_i16,op_i8_i32,op_i8_i64)
       call out_simple_scalar(g,'$1=INT($2,KIND=PM__i8)',l)
    case(op_i16_i,op_i16_ln,op_i16_offset,&
         op_i16_i8,op_i16_i32,op_i16_i64)
       call out_simple_scalar(g,'$1=INT($2,KIND=PM__i16)',l)
    case(op_i32_i,op_i32_ln,op_i32_offset,&
         op_i32_i8,op_i32_i16,op_i32_i64)
       call out_simple_scalar(g,'$1=INT($2,KIND=PM__i32)',l)
    case(op_i64_i,op_i64_ln,op_i64_offset,&
         op_i64_i8,op_i64_i16,op_i64_i32)
       call out_simple_scalar(g,'$1=INT($2,KIND=PM__i64)',l)
    case(op_complex_r)
       call out_simple_scalar(g,'$1=CMPLX($2,0.0)',l)
    case(op_complex_d)
       call out_simple_scalar(g,'$1=CMPLX($2,0.0d0,kind=PM__D)',l)
    case(op_complex2_r)
       call out_simple_scalar(g,'$1=CMPLX($2,$3)',l)
    case(op_complex2_d)
       call out_simple_scalar(g,'$1=CMPLX($2,$3,kind=PM__D)',l)
    case(op_logical_return,op_miss_arg,op_default)
       continue
       
    case default
       write(*,*) opcode
       write(*,*) op_names(opcode),op_names(opcode-1),op_names(opcode+1)
       call pm_panic('cannot compile operation')
    end select

88  continue

    if(debug_g) write(*,*) 'GEN OP DONE> ',op_names(opcode),opcode2,n
    
  end subroutine gen_op

  !============================================================
  ! Implement an over command
  ! - basically save/resore current loop state
  !   to allow this to be overridden within the
  !   block
  !============================================================
  subroutine gen_over_block(g,l,blk)
    type(gen_state):: g
    integer,intent(in):: l,blk
    type(gloop):: save_loop
    save_loop=g%lstack(g%lthis)
    g%lstack(g%lthis)%nloops=0
    g%lstack(g%lthis)%loop_mode=loop_is_none
    call gen_block(g,blk)
    call gen_loop(g,l,.true.)
    g%lstack(g%lthis)=save_loop
  end subroutine gen_over_block

  !============================================================
  ! Generate code for block containing comm ops
  !============================================================
  subroutine gen_comm_block(g,l,lnew,nc)
    type(gen_state):: g
    integer,intent(in):: l,lnew
    character(len=*),intent(in):: nc
    integer:: save_lthis,ll,save_last_ve
    call gen_loop(g,l,.false.)
    save_last_ve=g%last_ve
    g%last_ve=0
    g%ltop=g%ltop+1
    save_lthis=g%lthis
    g%lthis=g%ltop
    g%lstack(g%lthis)%nloops=0
    g%lstack(g%lthis)%loop_mode=loop_is_contig
 
    if(pm_opts%ftn_annotate) call out_simple(g,'! BLOCK -> $N',n=g%lthis)
    if(nc/=' ') call out_simple(g,'N$N='//nc,l,n=g%lthis)
    call gen_vect_alloc(g)
    g%lstack(g%lthis)%loop_active=.false.
    ll=lnew
    do while(ll>0)
       call gen_op(g,ll)
       ll=g%codes(ll)
    enddo
    call gen_loop(g,l,.true.)
    call gen_vect_dealloc(g)
    if(pm_opts%ftn_annotate) call out_simple(g,'!ENDBLOCK -> $N',n=g%lthis)
    g%lthis=save_lthis
    g%last_ve=save_last_ve
  end subroutine gen_comm_block

  subroutine gen_mpi_recv_call(g,l,lnew,assn)
    type(gen_state):: g
    integer,intent(in):: l,lnew
    logical,intent(in):: assn
    integer:: a,save_lthis,ll,save_last_ve
    a=l+comp_op_arg0
    !call out_line(g,'write(*,*) "RECV CALL"')
    call out_line(g,'JCOMM=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM')
    call out_line(g,&
         'CALL MPI_PROBE(MPI_ANY_SOURCE,PM__REQ_TAG,JCOMM,PM__STAT,JERRNO)')
    !call out_line(g,'write(*,*) "PROBED"')
    call out_line(g,'JNODE=PM__STAT(MPI_SOURCE)')
    call out_simple(g,'$2=JNODE',l)
    save_last_ve=g%last_ve
    g%last_ve=0
    g%ltop=g%ltop+1
    save_lthis=g%lthis
    g%lthis=g%ltop
    g%lstack(g%lthis)%nloops=0
    g%lstack(g%lthis)%loop_mode=loop_is_contig
    call out_simple(g,'CALL MPI_RECV(N$N,1,MPI_AINT,JNODE,PM__REQ_TAG,JCOMM,MPI_STATUS_IGNORE,JERRNO)',n=g%lthis)
    call gen_vect_alloc(g)
    call gen_mpi_recv(g,g%codes(a+3),'PM__EXTRA_REQ_TAG','RECV',mode_vect,.false.)
    if(assn) then
       call gen_mpi_recv(g,g%codes(a+5),'PM__EXTRA_REQ_TAG','RECV',mode_vect,.false.)
       call out_line(g,'CALL PM__GET_SHARED_RANKS(WBUFFER%P,JNUMRANKS)')
       call out_line(g,'DO JI=1,JNUMRANKS')
       call out_line(g,'JNODE=WBUFFER%P(I)')
       call gen_mpi_send(g,g%codes(a+3),'PM__EXTRA_REQ_TAG','ISEND',mode_vect)
       call gen_mpi_send(g,g%codes(a+5),'PM__EXTRA_REQ_TAG','ISEND',mode_vect)
       call out_line(g,'ENDDO')
    endif
    !call out_line(g,'write(*,*) "RECVD CALL"')
    ll=lnew
    do while(ll>0)
       call gen_op(g,ll)
       ll=g%codes(ll)
    enddo
    call gen_loop(g,l,.true.)
    call gen_vect_dealloc(g)
    g%lthis=save_lthis
    g%last_ve=save_last_ve
  end subroutine gen_mpi_recv_call

  !============================================================
  ! Generate for block of code that is shared/invariant
  ! with respect to the current context
  !============================================================
  subroutine gen_shared_block(g,l,lnew)
    type(gen_state):: g
    integer,intent(in):: l,lnew
    integer:: save_lthis,ll,save_last_ve
    call gen_loop(g,l,.true.)
    save_last_ve=g%last_ve
    g%last_ve=0
    g%ltop=g%ltop+1
    save_lthis=g%lthis
    g%lthis=g%ltop
    g%lstack(g%lthis)%nloops=0
    g%lstack(g%lthis)%loop_mode=loop_is_none
    if(pm_opts%ftn_annotate) call out_simple(g,'! SHARED BLOCK -> $N',n=g%lthis)
    ll=lnew
    do while(ll>0)
       call gen_op(g,ll)
       ll=g%codes(ll)
    enddo
    if(pm_opts%ftn_annotate) call out_simple(g,'!END SHARED BLOCK -> $N',n=g%lthis)
    g%lthis=save_lthis
    g%last_ve=save_last_ve
  end subroutine gen_shared_block

  !================================================================
  ! Create a new frame in the parallel context / loop nesting stack
  ! Note all frames are retained - frames are pushed but not popped
  ! The stack is maintained as a linked list (%parent)
  !================================================================
  subroutine g_new_frame(g)
    type(gen_state),intent(inout):: g
    integer:: old_lthis
    old_lthis=g%lthis
    g%ltop=g%ltop+1
    g%lthis=g%ltop
    g%lstack(g%lthis)%varlist=0
    g%lstack(g%lthis)%evarlist=0
    g%lstack(g%lthis)%defer_free=0
    g%lstack(g%lthis)%idx=0
    g%lstack(g%lthis)%free=0
    g%lstack(g%lthis)%loop_mode=loop_is_contig
    g%lstack(g%lthis)%loop_active=.false.
    if(g%lthis==0) then
       g%lstack(g%lthis)%parent=0
       g%lstack(g%lthis)%depth=0
    else
       g%lstack(g%lthis)%parent=old_lthis
       g%lstack(g%lthis)%depth=g%lstack(old_lthis)%depth+1
    endif
  end subroutine g_new_frame

  !================================================================
  ! Given the indices of two loop stack frames, determine the
  ! index of a third frame that is common parent to both
  !================================================================
  function g_common_frame(g,lthis_1,lthis_2) result(lthis)
    type(gen_state):: g
    integer,intent(in):: lthis_1,lthis_2
    integer:: lthis1,lthis2,lthis
    lthis1=lthis_1
    lthis2=lthis_2
    if(lthis1==lthis2) then
       lthis=lthis1
       return
    endif
    do while(g%lstack(lthis1)%depth>g%lstack(lthis2)%depth)
       !write(73,*) '1>',lthis1,g%lstack(lthis1)%depth
       lthis1=g%lstack(lthis1)%parent
    enddo
    do while(g%lstack(lthis2)%depth>g%lstack(lthis1)%depth)
       !write(73,*) '2>',lthis2,g%lstack(lthis2)%depth
       lthis2=g%lstack(lthis2)%parent
    enddo
    do while(lthis1/=lthis2.and.lthis1/=0.and.lthis2/=0)
       !write(73,*) '12>',lthis1,lthis2,g%lstack(lthis1)%depth
       lthis1=g%lstack(lthis1)%parent
       lthis2=g%lstack(lthis2)%parent
    enddo
    !write(73,*) 'F>',lthis1,lthis2
    lthis=lthis1
    !write(73,*) lthis,min(lthis_1,lthis_2)
    !if(lthis/=min(lthis_1,lthis_2)) write(73,*)'********'
    return
  end function g_common_frame

  !============================================================
  ! If not in an active loop, generate required do loops
  ! and masking statements
  !============================================================
  subroutine gen_loop(g,l,isshared)
    type(gen_state):: g
    integer,intent(in):: l
    logical,intent(in):: isshared
    integer:: ve
    logical:: shared
   
    ve=g%codes(l+comp_op_arg0)
    shared=ve==shared_op_flag.or.isshared
    if(shared) ve=0
    
    ! Start up loops
    if((.not.shared).and.(.not.g_loop_active(g))) then
       call gen_loop_nest(g)
       g%lstack(g%lthis)%loop_active=.true.
       g%last_ve=0
    endif
    
    ! Close and re-open if statements
    if(g%last_ve/=ve) then
       call gen_if_nest(g,g%last_ve,ve)
       g%last_ve=ve
    endif
    
    ! Close down loops
    if(shared.and.g_loop_active(g)) then
       call gen_close_loops(g)
    endif
    
  end subroutine gen_loop

  !=========================================
  ! Are the loops for the current parallel
  ! context running
  !=========================================
  function g_loop_active(g) result(ok)
    type(gen_state):: g
    logical:: ok
    ok=g%lstack(g%lthis)%loop_active
  end function g_loop_active

  !============================================================
  ! Generate loops spanning the current domain
  !============================================================
  subroutine gen_loop_nest(g) 
    type(gen_state):: g
    integer:: nloops
    logical:: blocked
    integer:: i,n

    if(g_loop_active(g)) return
    
    select case(g%lstack(g%lthis)%loop_mode)
    case(loop_is_none)
       return
    case(loop_is_contig)
       call out_str(g,'DO I')
       call out_idx(g,g%lthis)
       call out_str(g,'=1,N')
       call out_idx(g,g%lthis)
       call out_new_line(g)
       nloops=1
    case(loop_is_nested)
       call gen_nested_loop(g,g%lstack(g%lthis)%loop_par,nloops)
    case default
       call pm_panic('gen_loop_nest')
    end select
    g%lstack(g%lthis)%loop_active=.true.
    g%lstack(g%lthis)%nloops=nloops
    g%last_ve=0
  end subroutine gen_loop_nest


  !============================================================
  ! Generate loops described by variable v
  ! -- v is a grouped tuple ( vdesc , dimension_sizes , blocking )
  ! -- vdesc is a grouped tuple of vdim
  ! -- each vdim is either a grouped tuple of sequence parameters
  !    or an array
  ! Returns number of loops opened in nloops
  !============================================================
  subroutine gen_nested_loop(g,v,nloops)
    type(gen_state):: g
    integer,intent(in):: v
    integer,intent(out):: nloops
    integer:: i,ndim,vdesc,vdim,vblock,vsize,array
    if(g_kind(g,v)/=v_is_group) call pm_panic('nested loop var not a group')
    vdesc=g_ptr(g,v,1)
    if(g_kind(g,v)/=v_is_group) call pm_panic('nested loop desc var not a group')
    ndim=g_v1(g,vdesc)
    nloops=0
    if(g_type(g,g_ptr(g,v,3))==pm_null) then
       ! Non-blocked
       do i=ndim,1,-1
          vdim=g_ptr(g,vdesc,i)
          if(g_kind(g,vdim)==v_is_group) then
             if(g_v2(g,vdim)==v_is_struct) then
                select case(g_v1(g,vdim))
                case(1)
                   ! single point
                   call out_simple(g,'I$N_$M=$I+1 !!! moo',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                   !nloops=nloops+1
                case(2)
                   ! range
                   call out_simple_part(g,'DO I$N_$M=$I,',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                   call out_arg(g,g_ptr(g,vdim,2),0)
                   call out_new_line(g)
                   nloops=nloops+1
                case(3)
                   ! strided range
                   call out_simple_part(g,'DO I$N_$M=$I,',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                   call out_arg(g,g_ptr(g,vdim,2),0)
                   call out_char(g,',')
                   call out_arg(g,g_ptr(g,vdim,3),0)
                   call out_new_line(g)
                   nloops=nloops+1
                case(4)
                   ! map seq
                   call out_simple(g,'DO I$N__$M=1,$I',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,2))
                   array=g_ptr(g,vdim,1)
                   if(g_kind(g,array)==v_is_group) then
                      call out_simple(g,'I$N_$M=$I(I$N__$M)',&
                           n=i,m=g%lthis,x=g_ptr(g,array,1))
                   else
                      call out_simple(g,'I$N_$M=$I%E1%P(I$N__$M)',&
                           n=i,m=g%lthis,x=array)
                   endif
                   nloops=nloops+1
                case(5)
                   ! blocked seq
                   call out_simple_part(g,'DO I$N__$M=($I)-',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                   call out_arg(g,g_ptr(g,vdim,5),0)
                   call out_str(g,',(')
                   call out_arg(g,g_ptr(g,vdim,2),0)
                   call out_str(g,'),')
                   call out_arg(g,g_ptr(g,vdim,3),0)
                   call out_new_line(g)
                   call out_simple_part(g,'DO I$N_$M=MAX($I,I$N__$M),',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                   call out_simple_part(g,'MIN(I$N__$M+($I)-1,',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,4))
                   call out_arg(g,g_ptr(g,vdim,2),0)
                   call out_char(g,')')
                   call out_new_line(g)
                   nloops=nloops+2
                end select
             else
                ! Array
                call out_simple(g,'DO I$N__$M=1,SIZE($I%P)',&
                     n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                call out_simple(g,'I$N_$M=$I(I$N__$M)',&
                     n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                nloops=nloops+1
             endif
          else
             call out_simple(g,'DO I$N__$M=1,SIZE($I%E1%P)',&
                  n=i,m=g%lthis,x=vdim)
             call out_simple(g,'I$N_$M=$I%E1%P(I$N__$M)',&
                  n=i,m=g%lthis,x=vdim)
             nloops=nloops+1
          endif
       enddo
    elseif(g_v1(g,v)==3) then
       ! Post blocking
       vblock=g_ptr(g,v,3)
       
       ! Outer loop
       do i=ndim,1,-1
          vdim=g_ptr(g,vdesc,i)
          if(g_kind(g,vdim)==v_is_group) then
             if(g_v2(g,vdim)==v_is_struct) then
                select case(g_v1(g,vdim))
                case(1)
                   ! single point
                   call out_simple(g,'I$N_$M=$I+1',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                case(2)
                   ! range
                   call out_simple_part(g,'DO I$N__$M=$I,',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                   call out_arg(g,g_ptr(g,vdim,2),0)
                   call out_simple_part(g,',$I%E$N',x=vblock,n=i)
                   call out_new_line(g)
                   nloops=nloops+1
                case(3)
                   ! strided range
                   call out_simple_part(g,'DO I$N__$M=$I,',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                   call out_arg(g,g_ptr(g,vdim,2),0)
                   call out_char(g,',')
                   call out_arg(g,g_ptr(g,vdim,3),0)
                   call out_simple_part(g,'*$I%E$N',x=vblock,n=i)
                   call out_new_line(g)
                   nloops=nloops+1
                case(5)
                   ! blocked seq
                   call out_simple_part(g,'DO I$N__$M=$I-',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                   call out_arg(g,g_ptr(g,vdim,5),0)
                   call out_char(g,',')
                   call out_arg(g,g_ptr(g,vdim,2),0)
                   call out_char(g,',')
                   call out_arg(g,g_ptr(g,vdim,3),0)
                   call out_new_line(g)
                   call out_simple_part(g,'IMAX$N__$M=MIN(I$N__$M+$I-1,',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,4))
                   call out_arg(g,g_ptr(g,vdim,2),0)
                   call out_line(g,')')
                   call out_simple_part(g,'DO I$N_$M=MAX($I,I$N__$M),IMAX$N__$M',&
                        n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                   call out_simple(g,',$I%E$N',x=vblock,n=i)
                   nloops=nloops+2
                end select
             else
                ! Arrays (split)
                call out_simple_part(g,'DO I$N___$M=1,SIZE($I%P),',&
                     n=i,m=g%lthis,x=g_ptr(g,vdim,1))
                call out_simple(g,',$I%E$N',x=vblock,n=i)
                nloops=nloops+1
             endif
          else
             ! Array
             call out_simple_part(g,'DO I$N___$M=1,SIZE($I%E1%P),',&
                  n=i,m=g%lthis,x=vdim)
             call out_simple(g,',$I%E$N',x=vblock,n=i)
             nloops=nloops+1
          endif
       enddo

       ! Loop over block
       do i=ndim,1,-1
          vdim=g_ptr(g,vdesc,i)
          if(g_kind(g,vdim)==v_is_group) then
             if(g_v2(g,vdim)==v_is_struct) then
                select case(g_v1(g,vdim))
                case(1)
                   ! single point
                   continue
                case(2)
                   ! range
                   call out_simple_part(g,'DO I$N_$M=I$N__$M,MIN(I$N__$M-1+$I%E$N,',&
                        n=i,m=g%lthis,x=vblock)
                   call out_arg(g,g_ptr(g,vdim,2),0)
                   call out_char(g,')')
                   call out_new_line(g)
                   nloops=nloops+1
                case(3)
                   ! strided range
                   call out_simple_part(g,'DO I$N_$M=I$N__$M,MIN(I$N__$M-1+$I%E$N*',&
                        n=i,m=g%lthis,x=vblock)
                   call out_arg(g,g_ptr(g,vdim,3),0)
                   call out_char(g,',')
                   call out_arg(g,g_ptr(g,vdim,2),0)
                   call out_str(g,'),')
                   call out_arg(g,g_ptr(g,vdim,3),0)
                   call out_new_line(g)
                   nloops=nloops+1
                case(5)
                   ! blocked seq
                   call out_simple(g,'DO I$N_$M=I$N__$M,MIN(IMAX$N__$M,I$N__$M+$I%E$N)',&
                        n=i,m=g%lthis,x=vblock)
                   nloops=nloops+1
                end select
             else
                ! Array (split)
                call out_simple_part(g,'DO I$N__$M=I$N__$M,MIN(I$N__$M-1+$I%E$N,SIZE(',&
                     n=i,m=g%lthis,x=vblock)
                call out_arg(g,g_ptr(g,vdim,1),0)
                call out_line(g,'))')
                call out_simple(g,'I$N_$M=$I(I$N__$M)',&
                     n=i,m=g%lthis,x=g_ptr(g,vdim,1))
             endif
          else
             ! Array
             call out_simple_part(g,'DO I$N__$M=I$N__$M,MIN(I$N__$M-1+$I%E$N,SIZE(',&
                  n=i,m=g%lthis,x=vblock)
             call out_arg(g,g_ptr(g,vdim,1),0)
             call out_line(g,'%E1))')
             call out_simple(g,'I$N_$M=$I%E1(I$N__$M)',&
                  n=i,m=g%lthis,x=g_ptr(g,vdim,1))
          endif
       enddo
    else
       if(g_kind(g,v)==v_is_storageless) then
          return
       else
          call pm_panic('Bad nested loop descriptor')
       endif
    endif

    ! Calculate combined index
    vsize=g_ptr(g,v,2)
    call out_char_idx(g,'I',g%lthis)
    call out_str(g,'=1+')
    call out_char_idx(g,'I',1)
    call out_char_idx(g,'_',g%lthis)
    do i=2,ndim
       call out_char(g,'+')
       call out_arg(g,g_ptr(g,vsize,i-1),0)
       call out_str(g,'*(I')
       call out_idx(g,i)
       call out_char_idx(g,'_',g%lthis)
    enddo
    do i=2,ndim
       call out_char(g,')')
    enddo
    call out_new_line(g)

  end subroutine gen_nested_loop

  
  !============================================================
  ! Close all active loops
  !============================================================
  subroutine gen_close_loops(g)
    type(gen_state):: g
    integer:: i
    if(g_loop_active(g)) then
       do i=1,g%lstack(g%lthis)%nloops
          call out_line(g,'ENDDO')
       enddo
       g%lstack(g%lthis)%loop_active=.false.
       g%lstack(g%lthis)%nloops=0
    endif
  end subroutine gen_close_loops

  !============================================================
  ! Generate nested if statements to
  ! apply masking
  !============================================================
  subroutine gen_if_nest(g,ve1,ve2)
    type(gen_state):: g
    integer,intent(in):: ve1,ve2
    integer,dimension(max_par_depth*2)::stack
    integer:: start1,end1
    integer:: start2,end2
    integer:: top,i,vevar
    logical:: have_else,cove
    top=0
    have_else=.false.
    if(ve1>0.and.ve2>0) then
       call stack_all(ve1,top,start1,end1)
       call stack_all(ve2,top,start2,end2)
       do while(stack(end1)==stack(end2))
          end1=end1-1
          end2=end2-1
          if(end1<1.or.end2<1) exit
       enddo
       if(end1>0.and.end2>0) then
          if(g_v2(g,stack(end1))==stack(end2)) then
             cove=g_kind(g,stack(end1))==v_is_cove
             vevar=stack(merge(end2,end1,cove))
             have_else=g_gflags_clear(g,vevar,var_is_else_disabled)
             if(have_else) then
                call g_set_gflags(g,vevar,var_is_else_disabled)
             else
                call g_clear_gflags(g,vevar,var_is_else_disabled)
             endif
          endif
          if(have_else) then
             end1=end1-1
             end2=end2-1
          endif
       endif
    else
       if(ve1>0) then
          call stack_all(ve1,top,start1,end1)
       else
          start1=2
          end1=1
       endif
       if(ve2>0) then
          call stack_all(ve2,top,start2,end2)
       else
          start2=2
          end2=1
       endif
    endif
    do i=start1,end1
       call out_line(g,'ENDIF')
    enddo
    if(have_else) then
       call out_line(g,'ELSE')
    endif
    do i=end2,start2,-1
       call out_str(g,'IF(')
       if(g_kind(g,stack(i))==v_is_cove) then
          call out_str(g,'.NOT.(')
          call out_arg(g,stack(i),0)
          call out_char(g,')')
          call g_clear_gflags(g,g_v2(g,stack(i)),var_is_else_disabled)
       else
          call out_arg(g,stack(i),0)
          call g_clear_gflags(g,stack(i),var_is_else_disabled)
       endif
       call out_line(g,') THEN')
    enddo
  contains
    
    subroutine stack_all(ve,top,istart,iend)
      integer,intent(in):: ve
      integer,intent(inout):: top
      integer,intent(out):: istart,iend
      integer:: parent
      call push(top,ve)
      istart=top
      parent=g_v1(g,ve)
      do while(parent/=0)
         call push(top,parent)
         parent=g_v1(g,parent)
      enddo
      iend=top
    end subroutine stack_all

    subroutine push(top,ve)
      integer,intent(inout):: top
      integer,intent(in):: ve
      top=top+1
      if(top>max_par_depth*2) &
           call pm_panic('Program too complex (nested if)')
      stack(top)=ve
    end subroutine push
    
  end subroutine gen_if_nest

  !============================================================
  ! Close currently open ifs in this par scope
  !============================================================
  subroutine gen_close_ifs(g,last)
    type(gen_state):: g
    integer,intent(in):: last
    integer:: i
 
    i=g%last_ve
    !write(*,*) 'LAST>>',i,last,g%last_ve
    do while(i/=last.and.i/=0)
       if(debug_g) write(*,*) 'FINISH>',i,g_kind(g,i),g_v1(g,i),g_v2(g,i)
       i=g_v1(g,i)
       call out_line(g,'ENDIF')
    enddo
    g%last_ve=0
  end subroutine gen_close_ifs

  !============================================================
  ! Generate code for IF( any active strand ) THEN
  !============================================================
  subroutine gen_active_check_start(g,l)
    type(gen_state):: g
    integer,intent(in):: l
    integer:: ve
    ve=g%codes(l+comp_op_arg0)
    call gen_stacked_ve(g,l,ve)
    call gen_loop(g,l,.true.)
    if(ve/=0.and.ve/=shared_op_flag) then
       if(g_is_a_vect(g,ve)) then
          call out_simple(g,'IF(ANY($A))THEN',x=ve)
       else
          call out_simple(g,'IF($A)THEN',x=ve)
       endif
    endif
  end subroutine gen_active_check_start

  !============================================================
  ! Generate code to close IF( any active strand ) THEN
  !============================================================
  subroutine gen_active_check_end(g,l)
    type(gen_state):: g
    integer,intent(in):: l
    integer:: ve
    ve=g%codes(l+comp_op_arg0)
    if(ve/=0.and.ve/=shared_op_flag) then
       call out_line(g,'ENDIF !active check')
    endif
  end subroutine gen_active_check_end

  !============================================================
  ! Generate code to combine stacked masks (ve)
  ! into single logical vector with all elements
  ! fully defined (nested ve only have valid
  ! values only for strands that were active
  ! when they were created)
  !
  ! - Assigned to vout if present
  ! - Otherwise change in place if necessary
  !============================================================
  subroutine gen_stacked_ve(g,l,v,vout)
    type(gen_state):: g
    integer,intent(in):: l,v
    integer,intent(in),optional:: vout
    integer:: parent,arg_flags
    logical:: stacked_already
    if(v==shared_op_flag) return
    if(v==0) then
       if(present(vout)) then
          if(g_loop_active(g)) then
             call out_arg(g,vout,0)
          else
             call out_arg(g,vout,arg_no_index)
          endif
          call out_line(g,'=.TRUE.')
       endif
       return
    else
       parent=g_v1(g,v)
       if(parent==0.and..not.present(vout)) return
    endif
    stacked_already=g_gflags_set(g,v,var_is_stacked_ve)
    if(stacked_already.and..not.present(vout)) return
    if(g_loop_active(g)) then
       call gen_close_ifs(g,0)
       arg_flags=0
    else
       arg_flags=arg_no_index
    endif
    if(present(vout)) then
       call out_arg(g,vout,arg_flags)
    else
       call out_arg(g,v,arg_flags)
    endif
    call out_char(g,'=')
    call out_arg(g,v,arg_flags)
    if(.not.stacked_already) then
       do while(parent/=0)
          call out_str(g,'.AND.')
          call out_arg(g,parent,arg_flags)
          parent=g_v1(g,parent)
       end do
    endif
    call out_str(g,' ! Stacked ve')
    call out_new_line(g)
    call g_set_gflags(g,v,var_is_stacked_ve)
  end subroutine gen_stacked_ve

  !============================================================
  ! Generate code to allocate a vector variable
  !============================================================
  subroutine gen_vect_alloc(g)
    type(gen_state):: g
    integer:: v
    v=g%lstack(g%lthis)%varlist
    do while(v>0)
       if(g_var_at_index_is_a_vect(g,v).and.&
            iand(g%vardata(v)%flags,v_is_result)==0.and.&
            iand(g%vardata(v)%gflags,var_is_recycled)==0) then
          call out_str(g,'IF(ALLOCATED(')
          call out_var_at_index(g,v)
          if(iand(g%vardata(v)%flags,v_is_chan)/=0) then
             call out_str(g,'%P')
          endif
          call out_str(g,')) DEALLOCATE(')
          call out_var_at_index(g,v)
          if(iand(g%vardata(v)%flags,v_is_chan)/=0) then
             call out_str(g,'%P')
          endif
          call out_line(g,')')
          call out_str(g,'ALLOCATE(')
          call out_var_at_index(g,v)
          if(iand(g%vardata(v)%flags,v_is_chan)/=0) then
             call out_str(g,'%P')
          endif
          call out_str(g,'(N')
          call out_idx(g,g%lthis)
          if(iand(g%vardata(v)%flags,v_is_ve+v_is_cove)/=0) then
             call out_str(g,'),SOURCE=.FALSE.)')
          else
             call out_line(g,'))')
          endif
       endif
       v=g%vardata(v)%link
    enddo
  end subroutine gen_vect_alloc

  !============================================================
  ! Generate code to deallocate a vector variable
  !============================================================
  subroutine gen_vect_dealloc(g)
    type(gen_state):: g
    integer:: v,outer
    v=g%lstack(g%lthis)%varlist
    do while(v>0)
       if(g_var_at_index_is_a_vect(g,v).and.&
            iand(g%vardata(v)%gflags,var_is_recycled)==0) then
          outer=g%vardata(v)%outer_lthis
          if(outer/=g%vardata(v)%lthis) then
             g%vardata(v)%elink=g%lstack(outer)%defer_free
             g%lstack(outer)%defer_free=v
          else
             call out_str(g,'DEALLOCATE(')
             call out_var_at_index(g,v)
             if(iand(g%vardata(v)%flags,v_is_chan)/=0) then
                call out_str(g,'%P')
             endif
             call out_line(g,')')
          endif
       endif
       v=g%vardata(v)%link
    enddo
    v=g%lstack(g%lthis)%defer_free
    do while(v>0)
       call out_str(g,'DEALLOCATE(')
       call out_var_at_index(g,v)
       if(iand(g%vardata(v)%flags,v_is_chan)/=0) then
          call out_str(g,'%P')
       endif
       call out_line(g,')')
       v=g%vardata(v)%elink
    enddo
  end subroutine gen_vect_dealloc


  !*************************************************************
  ! COMMUNICATION INSTRINSICS
  !*************************************************************
  
  !============================================================
  ! Send messages to multiple other nodes and service
  ! Coordinate using non-blocking barrier
  ! Arg list: ve block internal-block new-p new-v new-w v w p i
  !============================================================
  subroutine gen_mpi_remote_call(g,l,issend)
    type(gen_state):: g
    integer,intent(in):: l
    logical,intent(in):: issend
    integer:: a,ve,v,lthis
    a=l+comp_op_arg0
    ve=g%codes(a)
    call out_line(g,'PM__REQUEST=3-PM__REQUEST')
    call out_simple(g,'I$N=1',n=g%lthis)
    if(.not.issend) then
       v=g%codes(a+6)
    endif
    call out_line(g,'NNODE=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NNODE')
    call out_line(g,'JTHIS_NODE=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NODE')
    call out_line(g,'JCOMM=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM')
    if(ve==0) then
       call out_simple(g,'NTOT=SIZE($#7,KIND=PM__LN)',l)
    else
       call out_simple(g,'NTOT=COUNT($#0)',l)
    endif
    call out_line(g,'IF(NTOT.GT.0) THEN')
    call out_line(g,'ALLOCATE(RSTART%P(0:NNODE))')
    call out_line(g,'NXSIZE=MIN(PM__EXCHANGE_BLOCK,NTOT)')
    call out_line(g,'ALLOCATE(RFROM%P(NXSIZE))')
    if(ve/=0) call out_line(g,'IBG=1')
    call out_line(g,'DO ISTART=1,NTOT,PM__EXCHANGE_BLOCK')
    call out_line(g,'IFINISH=MIN(ISTART+PM__EXCHANGE_BLOCK-1,NTOT)')
    call out_line(g,'NTRANS=IFINISH-ISTART+1')
    call out_line(g,'RSTART%P(NNODE)=NTRANS+1')
    if(ve==0) then
       call out_simple(g,'CALL PM__COLLATE_MESSAGES($#7,ISTART,NTRANS,NNODE,RSTART%P,RFROM%P)',l)
    else
       call out_simple(g,'CALL PM__COLLATE_MESSAGES_MASKED($#7,COUNT($#0,KIND=PM__LN),NNODE,RSTART%P,RFROM%P,$#0,IBG)',l)
    endif
    call out_line(g,'CALL MPI_IRECV(QRBUFFER%P,1,MPI_AINT,MPI_ANY_SOURCE,PM__REQUEST,'//&
         'JCOMM,JMESS,JERRNO)')
    call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
    call out_line(g,'DO JNODEBLK=0,NNODE-1,PM__NODE_BLOCK')
    call out_line(g,'DO JNODE=JNODEBLK,MIN(JNODEBLK+PM__NODE_BLOCK,NNODE-1)')
    call out_line(g,'IF(JNODE.EQ.JTHIS_NODE.OR.RSTART%P(JNODE).EQ.RSTART%P(JNODE+1)) CYCLE')
    if(issend) then
       call gen_mpi_send_part(g,g%codes(a+8),'PM__EXTRA_REQ_TAG','ISEND','RFROM%P',&
            'RSTART%P(JNODE)','RSTART%P(JNODE+1)-1',mode_vect)
    else
       call gen_mpi_recv_part(g,g%codes(a+5),'PM__DATA_TAG','IRECV',.false.,'RFROM%P',&
            'RSTART%P(JNODE)','RSTART%P(JNODE+1)-1',mode_vect)
    endif
    call gen_mpi_send_part(g,g%codes(a+6),'PM__EXTRA_REQ_TAG','ISEND','RFROM%P','RSTART%P(JNODE)',&
         'RSTART%P(JNODE+1)-1',mode_vect)
    call out_line(g,'ENDDO')
    call out_line(g,'ALLOCATE(RSBUFFER%P(JNODEBLK:MIN(JNODEBLK+PM__NODE_BLOCK,NNODE-1)))')
    call out_line(g,'DO JNODE=JNODEBLK,MIN(JNODEBLK+PM__NODE_BLOCK,NNODE-1)')
    call out_line(g,'IF(JNODE.EQ.JTHIS_NODE.OR.RSTART%P(JNODE).EQ.RSTART%P(JNODE+1)) CYCLE')
    call out_line(g,'RSBUFFER%P(JNODE)=RSTART%P(JNODE+1)-RSTART%P(JNODE)')
    call out_line(g,'CALL MPI_ISSEND(RSBUFFER%P(JNODE),1,MPI_AINT,JNODE,PM__REQUEST,JCOMM,'//&
         'JMESS,JERRNO)')
    call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
    call out_line(g,'ENDDO')

    call gen_internal_server_block(g,l,'RFROM%P(IX)+1','RSTART%P(JTHIS_NODE)','RSTART%P(JTHIS_NODE+1)-1')
 
    call out_line(g,'JCOMPLETE=0')
    call out_line(g,'DO WHILE(JCOMPLETE.LT.PM__MESSAGE_TOP-1)')
    call out_line(g,'CALL MPI_WAITANY(PM__MESSAGE_TOP,PM__MESSAGE_STACK,JRQ,PM__STAT,JERRNO)')
    call out_line(g,'IF(JRQ.EQ.1)THEN')
    lthis=-1
    call gen_server_block(g,l,issend,1,lthis)
    call out_line(g,'ELSE')
    call out_line(g,'JCOMPLETE=JCOMPLETE+1')
    call out_line(g,'IF(JCOMPLETE.LT.PM__MESSAGE_TOP)THEN')
    call out_line(g,'PM__MESSAGE_STACK(JRQ)=MPI_MESSAGE_NULL')
    call out_line(g,'ELSE')
    call out_line(g,'EXIT')
    call out_line(g,'ENDIF')
    call out_line(g,'ENDIF')
    call out_line(g,'ENDDO')
    if(.not.issend) then
       call out_line(g,'DO JNODE=JNODEBLK,MIN(JNODEBLK+PM__NODE_BLOCK,NNODE-1)')
       call out_line(g,'IF(JNODE.EQ.JTHIS_NODE.OR.RSTART%P(JNODE).EQ.RSTART%P(JNODE+1)) CYCLE')
       call gen_mpi_recv_part(g,g%codes(a+5),&
            'PM__DATA_TAG','RECV',.true.,'RFROM%P','RSTART%P(JNODE)','RSTART%P(JNODE+1)-1',&
            mode_vect)
       call out_line(g,'ENDDO')
    endif
    call out_line(g,'JMESS=PM__MESSAGE_STACK(1)')
    call out_line(g,'DEALLOCATE(RSBUFFER%P)')
    call out_line(g,'CALL PM__TIDY_MESSAGES()')
    call out_line(g,'ENDDO')
    call out_line(g,'ENDDO')
    call out_line(g,'ELSE')
    call out_line(g,'CALL MPI_IRECV(QRBUFFER%P,1,MPI_AINT,MPI_ANY_SOURCE,PM__REQUEST,'//&
         'JCOMM,JMESS,JERRNO)')
    call out_line(g,'ENDIF')
    call out_line(g,'IF(NTOT.GT.0) DEALLOCATE(RSTART%P,RFROM%P)')
    
    call out_line(g,'PM__MESSAGE_STACK(1)=JMESS')
    call out_line(g,'CALL MPI_IBARRIER(JCOMM,PM__MESSAGE_STACK(2),JERRNO)')
    call out_line(g,'PM__MESSAGE_TOP=2')
    call out_line(g,'JCOMPLETE=0')
    call out_line(g,'DO')
    call out_line(g,'CALL MPI_WAITANY(PM__MESSAGE_TOP,PM__MESSAGE_STACK,JRQ,PM__STAT,JERRNO)')
    call out_line(g,'IF(JRQ==1)THEN')
    call gen_server_block(g,l,issend,1,lthis)
    call out_line(g,'ELSE')
    call out_line(g,'IF(JRQ.EQ.2)THEN')
    call out_line(g,'CALL MPI_CANCEL(PM__MESSAGE_STACK(1),JERRNO)')
    call out_line(g,'CALL MPI_WAIT(PM__MESSAGE_STACK(1),PM__STAT,JERRNO)')
    call out_line(g,'IF(PM__MESSAGE_TOP.EQ.2)EXIT')
    call out_line(g,'ENDIF')
    call out_line(g,'JCOMPLETE=JCOMPLETE+1')
    call out_line(g,'PM__MESSAGE_STACK(JRQ)=MPI_MESSAGE_NULL')
    call out_line(g,'IF(JCOMPLETE.GE.PM__MESSAGE_TOP-1)EXIT')
    call out_line(g,'ENDIF')
    call out_line(g,'ENDDO')
    call out_line(g,'CALL PM__TIDY_MESSAGES()')

  end subroutine gen_mpi_remote_call

  ! =============================================================
  ! Send messages to single (shared- known) node and service
  ! Arg list: ve block internal-block new-v new-w v w p i
  ! ==========================================================
  subroutine gen_mpi_collect_call(g,l,issend)
    type(gen_state):: g
    integer,intent(in):: l
    logical,intent(in):: issend
    integer:: a,ve,v,lthis
    a=l+comp_op_arg0
    ve=g%codes(a)
    if(.not.issend) then
       v=g%codes(a+6)
    endif
    if(ve==0) then
       call out_simple(g,'NTOT=N$N',l,n=g%lthis)
    else
       call out_simple(g,'NTOT=COUNT($#0)',l)
    endif
    call out_line(g,'NNODE=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NNODE')
    call out_line(g,'JTHIS_NODE=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NODE')
    call out_line(g,'JROOT_NODE=PM__NODE_FRAME(PM__NODE_DEPTH)%ROOT_NODE')
    call out_line(g,'JCOMM=PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM')
    call out_simple(g,'JNODE=$7',l)
    call out_line(g,'IF(JNODE.NE.JROOT_NODE)THEN')
    if(ve>0) call out_simple(g,'RFROM%P=PM__ACTIVE_CELLS($A)',x=ve)
    call out_line(g,'QXBUFFER%P(1)=PM__EXCHANGE_BLOCK')
    call out_line(g,'DO IXBLK=0,NTOT-1,PM__EXCHANGE_BLOCK')
    call out_line(g,'NXSIZE=MIN(PM__EXCHANGE_BLOCK,NTOT-IXBLK)')
    if(ve>0) then
       if(issend) then
          call gen_mpi_send_part(g,g%codes(a+8),&
               'PM__EXTRA_REQ_TAG','ISEND','RFROM%P','IXBLK','IXBLK+NXSIZE-1',mode_vect)
       else
          call gen_mpi_recv_part(g,g%codes(a+5),&
               'PM__EXTRA_REQ_TAG','IRECV',.false.,'RFROM%P','IXBLK','IXBLK+NXSIZE-1',mode_vect)
       endif
       call gen_mpi_send_part(g,g%codes(a+6),&
            'PM__EXTRA_REQ_TAG','ISEND','RFROM%P','IXBLK','IXBLK+NXSIZE-1',mode_vect)
    else
       if(issend) then
          call gen_mpi_send(g,g%codes(a+8),&
               'PM__EXTRA_REQ_TAG','ISEND',mode_vect)
       else
          call gen_mpi_recv(g,g%codes(a+5),&
               'PM__EXTRA_REQ_TAG','IRECV',mode_vect,.false.)
       endif
       call gen_mpi_send(g,g%codes(a+6),&
            'PM__EXTRA_REQ_TAG','ISEND',mode_vect)
    endif
    call out_line(g,'IF(NXSIZE.EQ.NTOT-IXBLK)THEN')
    call out_line(g,'QSBUFFER%P(1)=-NXSIZE')
    call out_simple(g,'CALL MPI_ISEND(QSBUFFER%P,1,MPI_AINT,JNODE,PM__REQ_TAG,JCOMM,JMESS,JERRNO)',l)
    call out_line(g,'ELSE')
    call out_simple(g,'CALL MPI_ISEND(QXBUFFER%P,1,MPI_AINT,JNODE,PM__REQ_TAG,JCOMM,JMESS,JERRNO)',l)
    call out_line(g,'ENDIF')
    call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
    if(.not.issend) then
       if(ve>0) then
          call gen_mpi_recv_part(g,g%codes(a+6),&
               'PM__DATA_TAG','RECV',.true.,'RFROM%P','IX','IX+NXSIZE-1',0)
       else
          call gen_mpi_recv(g,g%codes(a+5),&
               'PM__EXTRA_REQ_TAG','IRECV',mode_vect,.true.)
       endif
    endif
    call out_line(g,'CALL PM__COMPLETE_MESSAGES')
    call out_line(g,'ENDDO')
    call out_line(g,'ELSE')
    call out_line(g,'IF(NNODE.GT.1) THEN')
    call out_line(g,&
         'CALL MPI_IRECV(QRBUFFER%P,1,MPI_AINT,MPI_ANY_SOURCE,PM__REQ_TAG,JCOMM,JMESS,JERRNO)')
    call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
    call out_line(g,'ENDIF')
    if(ve>0) then
       call gen_internal_server_block(g,l,'RFROM%P(IX)','1','SIZE(RFROM%P)')
    else
       call gen_internal_server_block(g,l,'IX','1','NTOT')
    endif
    
    call out_line(g,'JCOMPLETE=0')
    call out_line(g,'DO WHILE(JCOMPLETE.LT.NNODE-1)')
    call out_line(g,'CALL MPI_WAITANY(PM__MESSAGE_TOP,PM__MESSAGE_STACK,JRQ,PM__STAT,JERRNO)')
    call out_line(g,'IF(JRQ.EQ.1)THEN')
    call out_line(g,'JNODE=PM__STAT(MPI_SOURCE)')
    call out_line(g,'NSIZE=QRBUFFER%P(1)')
    call out_line(g,'IF(NSIZE.LT.0)THEN')
    call out_line(g,'NSIZE=-NSIZE')
    call out_line(g,'JCOMPLETE=JCOMPLETE+1')
    call out_line(g,'ENDIF')
    lthis=-1
    call gen_server_block(g,l,issend,1,lthis)

    call out_line(g,'IF(JCOMPLETE.EQ.NNODE-1)EXIT')
    call out_line(g,&
         'CALL MPI_IRECV(QRBUFFER%P,1,MPI_AINT,MPI_ANY_SOURCE,PM__REQ_TAG,JCOMM,JMESS,JERRNO)')
    call out_line(g,'PM__MESSAGE_STACK(1)=JMESS')
    call out_line(g,'ENDIF')
    call out_line(g,'ENDDO')

    call out_line(g,'CALL PM__TIDY_MESSAGES()')
    
    call out_line(g,'ENDIF')
    
  end subroutine gen_mpi_collect_call

  !============================================================
  ! Code service of inter-node message
  ! Arg list: ve block [ internal-block ] new-v new-w v w p i
  ! [ internal-block ] present if extra_arg=1 (instead of 0)
  !============================================================
  subroutine gen_server_block(g,l,issend,extra_arg,lthis)
    type(gen_state):: g
    integer,intent(in):: l,extra_arg
    logical,intent(in):: issend
    integer,intent(inout):: lthis
    integer:: a,save_lthis,save_last_ve,ll
    
    a=l+comp_op_arg0+extra_arg
    save_lthis=g%lthis
    if(lthis<0) then
       g%ltop=g%ltop+1
       g%lthis=g%ltop
       lthis=g%lthis
    else
       g%lthis=lthis
    endif
    g%lstack(g%lthis)%nloops=1
    save_last_ve=g%last_ve
    g%last_ve=0

    if(pm_opts%ftn_annotate) then
       call out_simple(g,'! BLOCK $N (server)',n=g%lthis)
    endif
    
    call out_simple(g,'N$N=ABS(QRBUFFER%P(1))',n=g%lthis)
    call out_line(g,'NA0=1')

    call gen_vect_alloc(g)
    
    call out_line(g,'JNODE=PM__STAT(MPI_SOURCE)')
    if(issend) then
       call gen_mpi_recv(g,g%codes(a+4),'PM__EXTRA_REQ_TAG','RECV',mode_vect,.false.)
    endif
    call gen_mpi_recv(g,g%codes(a+3),'PM__EXTRA_REQ_TAG','RECV',mode_vect,.false.)
    
    if(issend) then
       call out_line(g,&
            'IF(PM__NODE_FRAME(PM__NODE_DEPTH)%IS_SHARED.AND.JTHIS_NODE.EQ.JROOT_NODE)THEN')
       call out_line(g,'DO JNODE2=1,PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_NNODE-1')
       call out_line(g,'JNODE=PM__GET_SHARED(JNODE2)')
       call gen_mpi_send(g,g%codes(a+3),'PM__EXTRA_REQ_TAG','ISEND',mode_vect)
       call gen_mpi_send(g,g%codes(a+4),'PM__EXTRA_REQ_TAG','ISEND',mode_vect)
       call out_line(g,'ENDDO')
       call out_line(g,'ENDIF')
    endif
    
    call out_simple(g,'DO I$N=1,N$N  ! Start of server block',n=g%lthis)
    g%lstack(g%lthis)%loop_active=.true.
    ll=g%codes(a+1-extra_arg)
    do while(ll>0)
       call gen_op(g,ll)
       ll=g%codes(ll)
    enddo
    call gen_if_nest(g,g%last_ve,0)

    call out_line(g,'ENDDO  !End of server block')
    g%lstack(g%lthis)%loop_active=.false.
    g%last_ve=0
    
    if(.not.issend) then
       call gen_mpi_send(g,g%codes(a+7),'PM__DATA_TAG','RSEND',mode_vect)
    endif
    
    call gen_vect_dealloc(g)
    
    g%last_ve=save_last_ve
    g%lthis=save_lthis
    
    call out_line(g,'CALL MPI_IRECV(QRBUFFER%P,1,MPI_AINT,MPI_ANY_SOURCE,PM__REQUEST,'//&
         'JCOMM,JMESS,JERRNO)')
    call out_line(g,'PM__MESSAGE_STACK(1)=JMESS')

    if(pm_opts%ftn_annotate) then
       call out_simple(g,'! END BLOCK $N (server)',n=g%lthis)
    endif
  end subroutine gen_server_block

  !============================================================
  ! Code to run block servicing intra-node messages
  ! Arg list: ve block internal-block new-v new-w v w p i
  ! Assumes aliasing of arguments:
  !   new-v <-> v
  !   new-w <-> w
  !============================================================
  subroutine gen_internal_server_block(g,l,vd,vs,ve)
    type(gen_state):: g
    integer,intent(in):: l
    character(len=*),intent(in):: vd,vs,ve
    integer:: ll,a
    a=l+comp_op_arg0
    call out_line(g,'DO IX='//vs//','//ve)
    call out_char_idx(g,'I',g%lthis)
    call out_char(g,'=')
    call out_line(g,vd)
    g%lstack(g%lthis)%nloops=1
    g%lstack(g%lthis)%loop_active=.true.
    ll=g%codes(a+2)
    do while(ll>0)
       call gen_op(g,ll)
       ll=g%codes(ll)
    enddo
    call out_line(g,'ENDDO')
    g%lstack(g%lthis)%nloops=0
    g%lstack(g%lthis)%loop_active=.false.
    g%last_ve=0
  end subroutine gen_internal_server_block

  !============================================================
  ! Broadcast message to all nodes in group and service
  ! Arg list: ve block new-v new-w v w p i
  !============================================================
  subroutine gen_mpi_bcast_call(g,l)
    type(gen_state),intent(inout):: g
    integer,intent(in):: l
    integer:: a,ll,save_lthis,save_last_ve
    a=l+comp_op_arg0
    call out_simple(g,'I$N=1',n=g%lthis)
    call out_simple(g,'JNODE=$6',l)
    call out_simple(g,'IF(JNODE.EQ.PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NODE) NA=N$N',n=g%lthis)
    call out_line(g,'CALL MPI_BCAST(NA,1,MPI_AINT,JNODE,PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,JERRNO)')
    
    g%ltop=g%ltop+1
    save_lthis=g%lthis
    g%lthis=g%ltop
    g%lstack(g%lthis)%nloops=0
    save_last_ve=g%last_ve
    g%last_ve=0
    
    call out_simple(g,'N$N=NA',n=g%lthis)
    call out_line(g,'NA0=1')

    call gen_vect_alloc(g)

    call gen_pack(g,g%codes(a+2),g%codes(a+4),0)
    call gen_pack(g,g%codes(a+3),g%codes(a+5),0)
    
    
    call gen_mpi_bcast(g,g%codes(a+2))
    call gen_mpi_bcast(g,g%codes(a+3))

    ll=g%codes(a+1)
    do while(ll>0)
       call gen_op(g,ll)
       ll=g%codes(ll)
    enddo

    call gen_loop(g,l,.true.)
    
    call gen_vect_dealloc(g)
    
    g%last_ve=save_last_ve
    g%lthis=save_lthis
    
  end subroutine gen_mpi_bcast_call

  !============================================================================
  ! Broadcast message to all nodes in group and service (conditional context)
  ! Arg list: ve block new-v new-w v w p i
  !============================================================================
  subroutine gen_mpi_masked_bcast_call(g,l)
    type(gen_state),intent(inout):: g
    integer,intent(in):: l
    integer:: a,ll,save_lthis,save_last_ve
    a=l+comp_op_arg0
    call out_simple(g,'I$N=1',n=g%lthis)
    call out_simple(g,'JNODE=$6',l)
    call out_simple(g,'IF(JNODE.EQ.PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NODE) NA=COUNT($#0)',l)
    call out_line(g,'CALL MPI_BCAST(NA,1,MPI_AINT,JNODE,PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,JERRNO)')
    
    g%ltop=g%ltop+1
    save_lthis=g%lthis
    g%lthis=g%ltop
    g%lstack(g%lthis)%nloops=0
    save_last_ve=g%last_ve
    g%last_ve=0
    
    call out_simple(g,'N$N=NA',n=g%lthis)

    call gen_vect_alloc(g)

    call out_line(g,'IF(NA.GT.0) THEN')

    call out_simple(g,'IF(JNODE.EQ.PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NODE)THEN')
    call gen_pack(g,g%codes(a+2),g%codes(a+4),g%codes(a))
    call gen_pack(g,g%codes(a+3),g%codes(a+5),g%codes(a))
    call out_line(g,'ENDIF')

    call gen_mpi_bcast(g,g%codes(a+2))
    call gen_mpi_bcast(g,g%codes(a+3))

    ll=g%codes(a+1)
    do while(ll>0)
       call gen_op(g,ll)
       ll=g%codes(ll)
    enddo

    call gen_loop(g,l,.true.)
    
    call out_line(g,'ENDIF')
    
    call gen_vect_dealloc(g)
    
    g%last_ve=save_last_ve
    g%lthis=save_lthis
    
  end subroutine gen_mpi_masked_bcast_call

  !============================================================
  ! Code mpi_send, mpi_isend, mpi_issend ...
  ! node must be in JNODE
  !============================================================
  recursive subroutine gen_mpi_send(g,v,tag,s,mode,comm)
    type(gen_state):: g
    integer,intent(in):: v
    integer,intent(in):: mode
    character(len=*),intent(in):: tag,s
    character(len=*),intent(in),optional:: comm
    integer:: k,k2,i,tno,a
    logical:: nonblocking
    character(len=5):: ibuffer
    type(pm_ptr):: tv
    !call out_simple(g,'WRITE(*,*) "SEND",$N',n=abs(v))
    nonblocking=s=='ISEND'.or.s=='ISSEND'
    k=g_kind(g,v)
    k2=g_v2(g,v)
    select case(k)
    case(v_is_group)
       if(k2==v_is_dref.or.k2==v_is_shared_dref) then
          i=g_ptr(g,v,2)
          if(g_kind(g,i)==v_is_group.and.g_v2(g,i)==v_is_dref) then
             call gen_mpi_send(g,i,tag,s,mode_vect)
          endif
          if(k2/=v_is_shared_dref) then
             call gen_mpi_send(g,g_ptr(g,v,3),tag,s,mode_vect)
          endif
       elseif(k2==v_is_storageless) then
          continue
       elseif(k2==v_is_array) then
          if(mode==mode_array) then
             call gen_mpi_send(g,g_ptr(g,v,1),tag,s,mode_array_vect)
          else
             ! Split array view
             tno=g_type(g,v)
             call g_add_packable(g,pack_array_vect,tno)
             call out_simple_part(g,'CALL PM__PACKAVEC$N(NA,$A,',&
                  n=tno,x=g_ptr(g,v,1))
             call out_arg(g,g_ptr(g,v,2),arg_no_index)
             call out_line(g,')')
             call send_buffer
          endif
       else
          ! Structures/records
          do i=1,g_v1(g,v)
             call gen_mpi_send(g,g_ptr(g,v,i),tag,s,mode)
          enddo
       endif
    case(v_is_basic,v_is_elem,v_is_unit_elem,v_is_sub,v_is_vsub,&
         v_is_const,v_is_ctime_const)
       call out_str(g,'NA=SIZE(')
       call out_comm_var(g,v,mode)
       call out_line(g,')')
       tno=g_type(g,v)
       if(mode==mode_array) tno=pm_typ_arg(g%context,tno,1)
       if(g_is_complex_type(g,tno)) then
          call g_add_packable(g,pack_vect,tno)
          call out_simple_part(g,'CALL PM__PACKVEC$N(NA,',n=tno)
          call out_comm_var(g,v,mode)
          call out_line(g,')')
          call send_buffer
       else
          call out_get_mpi_base_type(g,tno)
          call out_line(g,'CALL PM__GET_MPI_TYPE(JBASE,NA,JTYPE,JN,LNEW)')
          call out_str(g,'CALL MPI_'//s//'(')
          call out_comm_var(g,v,mode)
          if(nonblocking) then
             call out_str(g,',JN,JTYPE,JNODE,'//tag//',')
             call out_comm_str(g,comm)
             call out_line(g,',JMESS,JERRNO)')
             call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
          else
             call out_str(g,',JN,JTYPE,JNODE,'//tag//',')
             call out_comm_str(g,comm)
             call out_line(g,',JERRNO)')
          endif
          call out_line(g,'IF(LNEW) CALL MPI_TYPE_FREE(JTYPE,JERRNO)')
       endif
    case(v_is_alias,v_is_chan_vect,v_is_vect_wrapped)
       call gen_mpi_send(g,g_v1(g,v),tag,s,merge(mode_vect,mode,k==v_is_chan_vect),comm)
    case default
       write(*,*) 'v=',v,'k=',k
       call pm_panic('Problem var in gen_mpi_send')
    end select
  contains
    subroutine send_buffer
      if(s=='RSEND') then
         call out_str(g,'CALL PM__ISEND_BUFFER(JNODE,'//tag//',')
         call out_comm_str(g,comm)
         call out_line(g,')')
         call out_str(g,'CALL MPI_RSEND(J,0,MPI_INTEGER,JNODE,'//tag//',')
         call out_comm_str(g,comm)
         call out_line(g,',JERROR)')
      else
         call out_line(g,'CALL PM__'//s//'_BUFFER(JNODE,'//tag//',')
         call out_comm_str(g,comm)
         call out_line(g,')')
         if(nonblocking) then
            call out_str(g,'CALL MPI_'//s//'(J,0,MPI_INTEGER,JNODE,'//tag//',')
            call out_comm_str(g,comm)
            call out_line(g,',JMESS,JERRNO)')
            call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
         else
            call out_str(g,'CALL MPI_'//s//'(J,0,MPI_INTEGER,JNODE,'//tag//',')
            call out_comm_str(g,comm)
            call out_line(g,')')
         endif
      endif
    end subroutine send_buffer
  end subroutine gen_mpi_send

  !============================================================
  ! Output vector component of variable v - mode governs
  ! whether this is grouped array 
  !============================================================
  subroutine out_comm_var(g,v,mode)
    type(gen_state):: g
    integer,intent(in):: v
    integer,intent(in):: mode
    !write(*,*) 'v is ',v,g_kind(g,v)
    !write(*,*) 'SENDING OUT COMM',mode,g%lthis,g_lthis(g,v)
    call out_arg(g,v,merge(0,arg_no_index,mode==mode_array))
    if(mode==mode_array) then
       call out_str(g,'%E1%P')
    elseif(mode==mode_array_vect) then
       call out_str(g,'%P')
    endif
  end subroutine out_comm_var

  !============================================================
  ! If comm string is present then output it, otherwise
  ! output the default communicator for this context
  !============================================================
  subroutine out_comm_str(g,comm)
    type(gen_state):: g
    character(len=*),optional:: comm
    if(present(comm)) then
       call out_str(g,comm)
    else
       call out_str(g,'PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM')
    endif
  end subroutine out_comm_str
  
  !============================================================
  ! Code mpi_irecv mpi_isrecv ...
  ! Node must be in JNODE
  !============================================================
  recursive subroutine gen_mpi_recv(g,v,tag,s,mode,rest,comm)
    type(gen_state):: g
    integer,intent(in):: v
    integer,intent(in):: mode
    character(len=*),intent(in):: tag,s
    character,intent(in),optional:: comm
    logical,intent(in):: rest
    integer:: k,k2,i,tno,a
    logical:: nonblocking,nontrivial
    character(len=5):: ibuffer
    nonblocking=s=='IRECV'
    k=g_kind(g,v)
    k2=g_v2(g,v)
    select case(k)
    case(v_is_group)
       if(k2==v_is_dref.or.k2==v_is_shared_dref) then
          i=g_ptr(g,v,2)
          if(g_kind(g,i)==v_is_group.and.g_v2(g,i)==v_is_dref) then
             call gen_mpi_recv(g,i,tag,s,mode,rest,comm)
          endif
          if(k2/=v_is_shared_dref) then
             call gen_mpi_recv(g,g_ptr(g,v,3),tag,s,mode,rest,comm)
          endif
       elseif(k2==v_is_storageless) then
          continue
       elseif(k2==v_is_array) then
          if(mode/=mode_array) call pm_panic('recv to split array')
          call gen_mpi_recv(g,g_ptr(g,v,1),tag,s,mode_array_vect,rest,comm)
       else
          ! Structures/records
          do i=1,g_v1(g,v)
             call gen_mpi_recv(g,g_ptr(g,v,i),tag,s,mode,rest,comm)
          enddo
       endif
    case(v_is_basic)
       tno=g_type(g,v)
       if(mode==mode_array) tno=pm_typ_arg(g%context,tno,1)
       if(g_is_complex_type(g,tno)) then
          call out_str(g,'NA=SIZE(')
          call out_comm_var(g,v,mode)
          call out_line(g,')')
          if(nonblocking) then
             call out_line(g,'CALL MPI_IRECV(J,0,MPI_INTEGER,JNODE,'//tag//',')
             call out_comm_str(g,comm)
             call out_line(g,',JMESS,JERRNO)')
             call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
          else
             if(.not.rest) then
                call out_str(g,'CALL MPI_RECV(J,0,MPI_INTEGER,JNODE,'//tag//',')
                call out_comm_str(g,comm)
                call out_line(g,',MPI_STATUS_IGNORE,JERRNO)')
             endif
             call g_add_packable(g,unpack_vect,tno)
             call out_str(g,'CALL PM__RECV_BUFFER(JNODE,'//tag//',')
             call out_comm_str(g,comm)
             call out_line(g,')')
             call out_line(g,'PM__BUFFER%SIZEOF=0')
             call out_simple_part(g,'CALL PM__UNPACKVEC$N(NA,',n=tno)
             call out_comm_var(g,v,mode)
             call out_line(g,')')
          endif
       else
          if(.not.rest) then
             call out_str(g,'NA=SIZE(')
             call out_comm_var(g,v,mode)
             call out_line(g,')')
             call out_get_mpi_base_type(g,tno)
             call out_line(g,'CALL PM__GET_MPI_TYPE(JBASE,NA,JTYPE,JN,LNEW)')
             call out_str(g,'CALL MPI_'//s//'(')
             call out_comm_var(g,v,mode)
             if(nonblocking) then
                call out_str(g,',JN,JTYPE,JNODE,'//tag//',')
                call out_comm_str(g,comm)
                call out_line(g,',JMESS,JERRNO)')
                call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
             else
                call out_str(g,',JN,JTYPE,JNODE,'//tag//',')
                call out_comm_str(g,comm)
                call out_line(g,',MPI_STATUS_IGNORE,JERRNO)')
             endif
             call out_line(g,'IF(LNEW) CALL MPI_TYPE_FREE(JTYPE,JERRNO)')
          endif
       endif
    case(v_is_alias,v_is_chan_vect,v_is_vect_wrapped)
       call gen_mpi_recv(g,g_v1(g,v),tag,s,mode,rest,comm)
    case default
       write(*,*) 'v=',v,'k=',k
       call pm_panic('Problem var in gen_mpi_recv')
    end select
  end subroutine gen_mpi_recv

  !============================================================
  ! Generate code for mpi broadcast
  ! node must be in JNODE
  !============================================================
  recursive subroutine gen_mpi_bcast(g,v,isshared,array_vect)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v
    logical,intent(in),optional:: isshared,array_vect
    integer:: tno
    integer:: k,k2,i
    logical:: nontrivial,isvec
    k=g_kind(g,v)
    k2=g_v2(g,v)
    select case(k)
    case(v_is_group)
       if(k2==v_is_dref.or.k2==v_is_shared_dref) then
          if(debug_g) then
             write(*,*) 'DREF>',g_ptr(g,v,1),g_ptr(g,v,2),g_ptr(g,v,3),g_ptr(g,v,4),g_ptr(g,v,5)
          endif
          i=g_ptr(g,v,2)
          if(g_kind(g,i)==v_is_group.and.g_v2(g,i)==v_is_dref) then
             call gen_mpi_bcast(g,i,isshared,array_vect)
          endif
          if(k2/=v_is_shared_dref) then
             call gen_mpi_bcast(g,g_ptr(g,v,3),isshared,array_vect)
          endif
       elseif(k2<0) then
          continue
       elseif(k2<=v_is_array) then
          call gen_mpi_bcast(g,g_ptr(g,v,1),isshared,array_vect=.true.)
          call gen_mpi_bcast(g,g_ptr(g,v,2),isshared,array_vect)
       else
          ! Structures/records
          do i=1,g_v1(g,v)
             call gen_mpi_bcast(g,g_ptr(g,v,i),isshared,array_vect)
          enddo
       endif
    case(v_is_basic,v_is_chan_vect)
       tno=g_type(g,v)
       isvec=g_is_vect(g,v).or.present(array_vect)
       if(g_is_complex_type(g,tno)) then
          call g_add_packable(g,pack_scalar,tno)
          call out_line(g,'CALL PM__NEW_BUFFER')
          if(isvec) then
             call out_simple(g,'NA=SIZE($A)',x=v)
          endif
          if(present(isshared)) then
             call out_line(g,'IF(PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_NODE==0) THEN')
          else
             call out_line(g,'IF(JNODE.EQ.PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NODE) THEN')
          endif
          if(.not.isvec) then
             call g_add_packable(g,pack_scalar,tno)
             call out_simple(g,'CALL PM__COUNT($I)',x=v)
             call out_line(g,'CALL PM__ALLOCATE_BUFFER')
             call out_simple(g,'CALL PM__PACK($I)',x=v)
          else
             call g_add_packable(g,pack_vect,tno)
             call out_simple(g,'CALL PM__PACKVEC$N(NA,$A)',n=tno,x=v)
          endif
          if(present(isshared)) then
             call out_line(g,'CALL PM__BCAST_BUFFER(JNODE,PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_COMM)')
             call out_line(g,'ELSE')
             call out_line(g,'CALL PM__BCAST_BUFFER(JNODE,PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_COMM)')
          else
             call out_line(g,'CALL PM__BCAST_BUFFER(JNODE,PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM)')
             call out_line(g,'ELSE')
             call out_line(g,'CALL PM__BCAST_BUFFER(JNODE,PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM)')
          endif
          if(.not.isvec) then
             call out_simple(g,'CALL PM__UNPACK$N($A)',x=v,n=tno)
          else
             call out_simple(g,'CALL PM__UNPACKVEC$N(NA,$A)',x=v,n=tno)
          endif
          call out_line(g,'ENDIF')
       else
          tno=g_type(g,v)
          if(.not.isvec) then
             call out_line(g,'JN=1')
             call out_get_mpi_base_type(g,tno)
             call out_line(g,'JTYPE=JBASE')
          else
             if(present(array_vect)) then
                call out_simple(g,'NA=SIZE($A%P)',x=v)
             else
                call out_simple(g,'NA=SIZE($A)',x=v)
             endif
             call out_get_mpi_base_type(g,tno)
             call out_simple(g,&
                  'CALL PM__GET_MPI_TYPE(JBASE,NA,JTYPE,JN,LNEW)',n=g%lthis)
          endif
          call out_str(g,'CALL MPI_BCAST(')
          call out_arg(g,v,merge(arg_no_index,0,isvec))
          if(present(array_vect)) then
             call out_str(g,'%P')
          endif
          if(present(isshared)) then
             call out_line(g,&
                  ',JN,JTYPE,JNODE,PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_COMM,JERRNO)')
          else
             call out_line(g,&
                  ',JN,JTYPE,JNODE,PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,JERRNO)')
          endif
          if(isvec) then
             call out_line(g,'IF(LNEW) CALL MPI_TYPE_FREE(JTYPE,JERRNO)')
          endif
       endif
    case(v_is_alias)
       call gen_mpi_bcast(g,g_v1(g,v))
    case default
       call pm_panic('problem var in gen_mpi_bcast')
    end select
  end subroutine gen_mpi_bcast

  !========================================================================
  ! Code mpi_send mpi_isend ... for sub-array defined in disp_var
  ! Node must be in JNODE
  !========================================================================
  subroutine gen_mpi_send_disp_or_grid(g,v,tag,s,disp_var,mode)
    type(gen_state):: g
    integer,intent(in):: v,disp_var
    character(len=*),intent(in):: tag,s
    integer,intent(in),optional:: mode
    call gen_mpi_send_part(g,v,tag,s,'$A','1','SIZE($A)',mode,disp_var)
  end subroutine gen_mpi_send_disp_or_grid

  !========================================================================
  ! Code mpi_recv mpi_irecv ... for sub-array defined in disp_var
  ! Node must be in JNODE
  !========================================================================
  subroutine gen_mpi_recv_disp_or_grid(g,v,tag,s,disp_var,mode,rest)
    type(gen_state):: g
    integer,intent(in):: v,disp_var
    character(len=*),intent(in):: tag,s
    logical,intent(in):: rest
    integer,intent(in):: mode
    call gen_mpi_recv_part(g,v,tag,s,rest,'$A','1','SIZE($A)',mode,disp_var)
  end subroutine gen_mpi_recv_disp_or_grid

  !========================================================================
  ! Code mpi_recv mpi_irecv ... for sub-array defined in disp_var
  ! Node must be in JNODE unless isshared==.true.
  !========================================================================
  subroutine gen_mpi_bcast_disp_or_grid(g,v,is_shared,disp_var,mode)
    type(gen_state):: g
    integer,intent(in):: v,disp_var
    logical,intent(in):: is_shared
    integer,intent(in):: mode
    call gen_mpi_bcast_part(g,v,is_shared,'$A','1','SIZE($A)',mode,disp_var)
  end subroutine gen_mpi_bcast_disp_or_grid
  
  !========================================================================
  ! Code mpi_isend mpi_issend ... for sub-array
  ! - either dv(dv1:dv2) or by displacements defined in dvv
  ! Node must be in JNODE
  !========================================================================
  recursive subroutine gen_mpi_send_part(g,v,tag,s,dv,dv1,dv2,mode,dvv)
    type(gen_state):: g
    integer,intent(in):: v,mode
    character(len=*),intent(in):: dv,dv1,dv2,tag,s
    integer,intent(in),optional:: dvv
    integer:: k,k2,i,tno,a
    logical:: nonblocking,nontrivial,ok
    character(len=5):: ibuffer
    !call out_simple(g,'WRITE(*,*) "SEND DISP",$N,$M',n=abs(v),m=mode)
    if(debug_g) write(*,*) 'SEND_DISP> v=',v,'s=',trim(s),'k=',g_kind(g,v)
    nonblocking=s=='ISEND'.or.s=='ISSEND'
    k=g_kind(g,v)
    k2=g_v2(g,v)
    select case(k)
    case(v_is_group)
       if(k2==v_is_dref.or.k2==v_is_shared_dref) then
          i=g_ptr(g,v,2)
          if(g_kind(g,i)==v_is_group.and.g_v2(g,i)==v_is_dref) then
             call gen_mpi_send_part(g,i,tag,s,dv,dv1,dv2,mode,dvv)
          endif
          if(k2/=v_is_shared_dref) then
             call gen_mpi_send_part(g,g_ptr(g,v,3),tag,s,dv,dv1,dv2,mode,dvv)
          endif
       elseif(k2<0) then
          continue
       elseif(k2==v_is_array) then
          if(mode==mode_array) then
             call gen_mpi_send_part(g,g_ptr(g,v,1),tag,s,dv,dv1,dv2,mode_array_vect,dvv)
          else
             call out_simple(g,'NA='//dv2//'-'//dv1//'+1',x=dvv)
             tno=g_type(g,v)
             call g_add_packable(g,pack_array_vect_disp,tno)
             call out_simple_part(g,'CALL PM__PACKADVEC$N(NA,$A,',&
                  n=tno,x=g_ptr(g,v,1))
             call out_arg(g,g_ptr(g,v,2),arg_no_index)
             call out_simple(g,','//dv//'('//dv1//':'//dv2//'))',x=dvv)
             call out_line(g,'CALL PM__'//s//'_BUFFER(JNODE,'//tag//&
               ',PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM)')
          endif
       else
          ! Structures/records
          do i=1,g_v1(g,v)
             call gen_mpi_send_part(g,g_ptr(g,v,i),tag,s,dv,dv1,dv2,mode,dvv)
          enddo
       endif
    case(v_is_basic,v_is_sub,v_is_vsub,v_is_elem,&
         v_is_unit_elem,v_is_const,v_is_ctime_const)
       tno=g_type(g,v)
       if(mode==mode_array) tno=pm_typ_arg(g%context,tno,1)
       if(g_is_complex_type(g,tno)) then
          call out_simple(g,'NA='//dv2//'-'//dv1//'+1',x=dvv)
          if(nonblocking) then
             call out_line(g,'CALL MPI_'//s//'(J,0,MPI_INTEGER,JNODE,'//&
                  tag//',PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,JMESS,JERRNO)')
             call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
          else
             call out_line(g,'CALL MPI_'//s//'(J,0,MPI_INTEGER,JNODE,'//&
                  tag//',PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,JERRNO)')
          endif
          call g_add_packable(g,pack_vect_disp,tno)
          call out_simple_part(g,'CALL PM__PACKDVEC$N(NA,',n=tno)
          call out_comm_var(g,v,mode)
          call out_simple(g,','//dv//'('//dv1//':'//dv2//'))',x=dvv)
          call out_line(g,'CALL PM__'//s//'_BUFFER(JNODE,'//tag//&
               ',PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM)')
       elseif(tno>=pm_int) then
          call make_disp_mpi_type(g,tno,mode,dv,dv1,dv2,dvv)
          call out_str(g,'CALL MPI_'//s//'(')
          if(debug_g) write(*,*) 'SENDING> v=',v,nonblocking,mode
          call out_comm_var(g,v,mode)
          if(nonblocking) then
             call out_line(g,',1,JTYPE,JNODE,'//tag//&
                  ',PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,JMESS,JERRNO)')
             call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
          else
             call out_line(g,',1,JTYPE,JNODE,'//tag//&
                  ',PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,JERRNO)')
          endif
       endif
    case(v_is_alias,v_is_chan_vect,v_is_vect_wrapped)
       call gen_mpi_send_part(g,g_v1(g,v),tag,s,dv,dv1,dv2,mode,dvv)
    case default
       write(*,*) 'v=',v,'k=',k
       call pm_panic('Problem var in gen_mpi_send_part')
    end select
    if(debug_g) write(*,*) 'END SEND DISP>',v
  end subroutine gen_mpi_send_part

  !============================================================
  ! Code mpi_recv, mpi_irecv
  ! - either dv(dv1:dv2) or by displacements defined in dvv
  ! Node must be in JNODE
  !============================================================
  recursive subroutine gen_mpi_recv_part(g,v,tag,s,rest,dv,dv1,dv2,mode,dvv)
    type(gen_state):: g
    integer,intent(in):: v,mode
    character(len=*),intent(in):: tag,s,dv,dv1,dv2
    integer,intent(in),optional:: dvv
    logical,intent(in):: rest
    integer:: k,k2,i,tno,a
    logical:: nonblocking,ok
    character(len=5):: ibuffer
    if(debug_g) write(*,*) 'RECV>',v
    nonblocking=s=='IRECV'
    k=g_kind(g,v)
    k2=g_v2(g,v)
    select case(k)
    case(v_is_group)
       if(k2==v_is_dref.or.k2==v_is_shared_dref) then
          i=g_ptr(g,v,2)
          if(g_kind(g,i)==v_is_group.and.g_v2(g,i)==v_is_dref) then
             call gen_mpi_recv_part(g,i,tag,s,rest,dv,dv1,dv2,mode,dvv)
          endif
          if(k2/=v_is_shared_dref) then
             call gen_mpi_recv_part(g,g_ptr(g,v,3),tag,s,rest,dv,dv1,dv2,mode,dvv)
          endif
       elseif(k2<0) then
          continue
       elseif(k2<=v_is_array) then
          if(mode/=mode_array) call pm_panic('recv_disp to split array')
          call gen_mpi_recv_part(g,g_ptr(g,v,1),tag,s,rest,dv,dv1,dv2,mode_array_vect,dvv)        
       else
          ! Structures/records
          do i=1,g_v1(g,v)
             call gen_mpi_recv_part(g,g_ptr(g,v,i),tag,s,rest,dv,dv1,dv2,mode,dvv)
          enddo
       endif
    case(v_is_basic,v_is_sub,v_is_vsub,v_is_elem,v_is_unit_elem)
       tno=g_type(g,v)
       if(mode==mode_array) then
          !write(*,*) 'tno=',pm_typ_as_string(g%context,tno)
          tno=pm_typ_arg(g%context,tno,1)
       endif
       if(g_is_complex_type(g,tno)) then
          if(nonblocking) then
             call out_line(g,'CALL MPI_IRECV(J,0,MPI_INTEGER,JNODE,'//tag//&
                  ',PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,JMESS,JERRNO)')
             call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
          else
             if(.not.rest) then
                call out_line(g,'CALL MPI_RECV(J,0,MPI_INTEGER,JNODE,'//tag//&
                  ',PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,MPI_STATUS_IGNORE,JERRNO)')
             endif
             call out_line(g,&
                  'CALL PM__RECV_BUFFER(JNODE,'//tag//&
                  ',PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM)')
             call g_add_packable(g,unpack_vect_disp,tno)
             call out_simple_part(g,'CALL PM__UNPACKDVEC$N(',n=tno)
             call out_str(g,'('//dv2//')-('//dv1//')+1,')
             call out_comm_var(g,v,mode)
             call out_simple(g,','//dv//'('//dv1//':'//dv2//'))',x=dvv)
          endif
       elseif(tno>=pm_int) then
          if(.not.rest) then
             call make_disp_mpi_type(g,tno,mode,dv,dv1,dv2,dvv)
             call out_str(g,'CALL MPI_'//s//'(')
             call out_comm_var(g,v,mode)
             if(nonblocking) then
                call out_line(g,',1,JTYPE,JNODE,'//tag//&
                     ',PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,JMESS,JERRNO)')
                call out_line(g,'CALL PM__PUSH_MESSAGE(JMESS)')
             else
                call out_simple(g,',1,JTYPE,JNODE,'//tag//&
                     ',PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,MPI_STATUS_IGNORE,JERRNO)')
             endif
          endif
       endif
    case(v_is_alias,v_is_chan_vect,v_is_vect_wrapped)
       call gen_mpi_recv_part(g,g_v1(g,v),tag,s,rest,dv,dv1,dv2,mode,dvv)
    case default
       write(*,*) 'v=',v,'k=',k
       call pm_panic('Problem var in gen_mpi_recv_part')
    end select
  end subroutine gen_mpi_recv_part

  !============================================================
  ! Generate code for mpi broadcast
  ! node must be in JNODE
  !============================================================
  recursive subroutine gen_mpi_bcast_part(g,v,isshared,dv,dv1,dv2,mode,dvv)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v,mode
    logical,intent(in),optional:: isshared
    character(len=*),intent(in):: dv,dv1,dv2
    integer,intent(in),optional:: dvv
    integer:: tno
    integer:: k,k2,i
    k=g_kind(g,v)
    k2=g_v2(g,v)
    select case(k)
    case(v_is_group)
       if(k2==v_is_dref.or.k2==v_is_shared_dref) then
          if(debug_g) then
             write(*,*) 'DREF>',g_ptr(g,v,1),g_ptr(g,v,2),g_ptr(g,v,3),g_ptr(g,v,4),g_ptr(g,v,5)
          endif
          i=g_ptr(g,v,2)
          if(g_kind(g,i)==v_is_group.and.g_v2(g,i)==v_is_dref) then
             call gen_mpi_bcast_part(g,i,isshared,dv,dv1,dv2,mode,dvv)
          endif
          if(k2/=v_is_shared_dref) then
             call gen_mpi_bcast_part(g,g_ptr(g,v,3),isshared,dv,dv1,dv2,mode,dvv)
          endif
       elseif(k2<0) then
          continue
       elseif(k2<=v_is_array) then
          call gen_mpi_bcast_part(g,g_ptr(g,v,1),isshared,dv,dv1,dv2,mode_array_vect,dvv)
       else
          ! Structures/records
          do i=1,g_v1(g,v)
             call gen_mpi_bcast_part(g,g_ptr(g,v,i),isshared,dv,dv1,dv2,mode,dvv)
          enddo
       endif
    case(v_is_basic)
       tno=g_type(g,v)
       if(mode==mode_array) tno=pm_typ_arg(g%context,tno,1)
       if(g_is_complex_type(g,tno)) then
          call out_simple(g,'NA='//dv2//'-'//dv1//'+1',x=dvv)
          call g_add_packable(g,pack_vect_disp,tno)
          call g_add_packable(g,unpack_vect_disp,tno)
          if(isshared) then
             call out_line(g,'IF(PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_NODE==0) THEN')
          else
             call out_line(g,'IF(JNODE.EQ.PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_NODE) THEN')
          endif
          call out_simple_part(g,'CALL PM__PACKDVEC$N(NA,%A',n=tno,x=v)
          call out_simple(g,dv//'('//dv1//':'//dv2//'))',x=dvv)
          if(isshared) then
             call out_line(g,'CALL PM__BCAST_BUFFER(0,PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_COMM)')
             call out_line(g,'ELSE')
             call out_line(g,'CALL PM__BCAST_BUFFER(0,PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_COMM)')
          else
             call out_line(g,'CALL PM__BCAST_BUFFER(JNODE,PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM)')
             call out_line(g,'ELSE')
             call out_line(g,'CALL PM__BCAST_BUFFER(JNODE,PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM)')
          endif
          call out_simple_part(g,'CALL PM__UNPACKDVEC$N(NA,%A',n=tno,x=v)
          call out_line(g,'ENDIF')
       elseif(tno>=pm_int) then
          call make_disp_mpi_type(g,tno,mode,dv,dv1,dv2,dvv)
          call out_str(g,'CALL MPI_BCAST(')
          call out_arg(g,v,arg_no_index)
          
          if(isshared) then
             call out_line(g,',1,JTYPE,0,'//&
                  'PM__NODE_FRAME(PM__NODE_DEPTH)%SHARED_COMM,JERRNO)')
          else
             call out_simple(g,',1,JTYPE,JNODE,'//&
                  'PM__NODE_FRAME(PM__NODE_DEPTH)%THIS_COMM,JERRNO)')
          endif
       endif
    case(v_is_alias,v_is_chan_vect,v_is_vect_wrapped)
       call gen_mpi_bcast_part(g,g_v1(g,v),isshared,dv,dv1,dv2,mode,dvv)
    case default 
       call pm_panic('problem var in gen_mpi_bcast_disp')
    end select
  end subroutine gen_mpi_bcast_part

  !============================================================
  ! Output code to wait on all pending mpi messages
  ! implementing op_sync_mess
  !============================================================
  subroutine gen_sync_mess(g,l,a,n)
    type(gen_state):: g
    integer,intent(in):: l,a,n
    call gen_sync_reg
    call out_line(g,&
         'CALL MPI_WAITALL(PM__MESSAGE_TOP,PM__MESSAGE_STACK,MPI_STATUSES_IGNORE,JERRNO)')
    call gen_sync_reg
    call out_line(g,'CALL PM__TIDY_MESSAGES()')
  contains
    subroutine gen_sync_reg
      integer:: i,arg
      if(n>0) then
         call out_line(g,'IF(.NOT.MPI_ASYNC_PROTECTS_NONBLOCKING)THEN')
         do i=1,n-1
            arg=g%codes(a+i)
            if(g%varindex(arg)==0) cycle
            if(g_flags_set(g,arg,v_is_param)) cycle
            if(g_kind(g,arg)==v_is_ctime_const) cycle
            call out_simple(g,'CALL MPI_F_SYNC_REG($Y)',l,x=i)
         enddo
         call out_line(g,'ENDIF')
      endif
    end subroutine gen_sync_reg
  end subroutine gen_sync_mess


  !================================================================
  ! Create MPI datatype to either encode displacements dv(dv1:dv2)
  ! or PM grid defined by dvv (if dvv argument present)
  !================================================================

  !!! Change to disp_or_grid type
  !!! correct access to tuple

  subroutine make_disp_mpi_type(g,tno,mode,dv,dv1,dv2,dvv)
    type(gen_state):: g
    integer,intent(in):: tno,mode
    character(len=*),intent(in):: dv,dv1,dv2
    integer,intent(in),optional:: dvv
    integer:: grid_tuple,grid_dim,offsets,i,j,n_arg
    type(pm_ptr):: tv
    if(present(dvv)) then
       if(g_kind(g,dvv)==v_is_group) then
          grid_tuple=g_ptr(g,dvv,1)
          if(pm_debug_checks) then
             if(g_kind(g,grid_tuple)/=v_is_group) then
                call pm_panic('norm grid not a group')
             endif
          endif
          call out_get_mpi_base_type(g,tno)
          call out_line(g,'JTYPE=JBASE')
          do i=1,g_v1(g,grid_tuple)
             grid_dim=g_ptr(g,grid_tuple,i)
             if(g_kind(g,grid_dim)/=v_is_group) then
                call pm_panic('norm grid dim not a group')
             endif
             if(g_v1(g,grid_dim)==2) then
                offsets=g_ptr(g,grid_dim,1)
                if(g_kind(g,offsets)==v_is_group) then
                   if(g_v2(g,offsets)/=v_is_array.and.g_v2(g,offsets)/=v_is_var_array) then
                      call pm_panic('grid dim array not array')
                   endif
                   call out_simple(g,'CALL PM__GET_MPI_DISP_TYPE(JTYPE,$A,1_PM__LN,JTYPE_N)',&
                        x=g_ptr(g,offsets,1))
                elseif(pm_typ_kind(g%context,g_type(g,offsets))==pm_typ_is_array) then
                   call out_simple(g,'CALL PM__GET_MPI_DISP_TYPE(JTYPE,$A%E1%P,1_PM__LN,JTYPE_N)',&
                        x=offsets)
                else
                   call pm_panic('grid dim array is not an array')
                endif
                n_arg=2
             else
                if(g_v1(g,grid_dim)/=6) then
                   call pm_panic('grid_dim incorrect number of entries')
                endif
                call out_str(g,'CALL PM__GET_MPI_SUBRANGE_TYPE(JTYPE,')
                tv=pm_typ_vect(g%context,g_type(g,grid_dim))
                do j=2,6
                   if(iand(pm_typ_flags(g%context,pm_tv_arg(tv,j)),pm_typ_has_storage)/=0) then
                      call out_arg(g,g_ptr(g,grid_dim,j),0)
                      call out_char(g,',')
                   else
                      call out_const(g,pm_typ_val(g%context,pm_tv_arg(tv,j)))
                      call out_char(g,',')
                   endif
                enddo
                call out_line(g,'JTYPE_N)')
                n_arg=1
             endif
             if(i/=g_v1(g,grid_tuple)) then
                call out_line(g,'CALL MPI_TYPE_GET_EXTENT_X(JTYPE,ILB,ISIZ,JERRNO)')
                call out_simple(g,'CALL MPI_TYPE_CREATE_RESIZED(JTYPE_N,0_PM__LN,ISIZ*$I,JTYPE,JERRNO)',&
                     x=g_ptr(g,grid_dim,n_arg))
             else
                call out_line(g,'JTYPE=JTYPE_N')
             endif
          enddo
          call out_line(g,'CALL MPI_TYPE_COMMIT(JTYPE,JERRNO)')
          return
       endif
    endif
    call out_get_mpi_base_type(g,tno)
    !write(*,*) dvv,'::','CALL PM__GET_MPI_DISP_TYPE(JBASE,'//dv//'('//dv1//':'//dv2//')'
    call out_simple_part(g,'CALL PM__GET_MPI_DISP_TYPE(JBASE,'//dv//'('//dv1//':'//dv2//')',x=dvv)
    call out_line(g,',1_PM__LN,JTYPE)')
  end subroutine make_disp_mpi_type
     
  !============================================================
  ! Generate code for
  ! v1<-pack(v2,m) where m is logical vector
  !============================================================
  recursive subroutine gen_pack(g,v1,v2,m)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v1,v2,m
    integer:: k,k2,i
    if(debug_g) write(*,*) 'PACK> v1=',v1,g_kind(g,v1),'v2=',v2,g_kind(g,v2),'m=',m
    k=g_kind(g,v1)
    k2=g_v2(g,v1)
    if(k==v_is_group) then
       if(k2==v_is_dref.or.k2==v_is_shared_dref) then
          i=g_ptr(g,v1,2)
          if(g_kind(g,i)==v_is_group.and.g_v2(g,i)==v_is_dref) then
             call gen_pack(g,i,g_ptr(g,v2,2),m)
          endif
          if(k2/=v_is_shared_dref) then
             call gen_pack(g,g_ptr(g,v1,3),g_ptr(g,v2,3),m)
          endif
       elseif(k2<0) then
          continue
       elseif(k2<=v_is_array) then
          call gen_pack(g,g_ptr(g,v1,1),g_ptr(g,v2,1),m)
          call gen_pack(g,g_ptr(g,v1,2),g_ptr(g,v2,2),m)
       else
          ! Structures/records
          do i=1,g_v1(g,v1)
             call gen_pack(g,g_ptr(g,v1,i),g_ptr(g,v2,i),m)
          enddo
       endif
    elseif(iand(k2,v_is_poly)/=0) then
       call out_line(g,'IX=0')
       call out_simple(g,'DO I$N=1,N$N',n=g%lthis)
       if(m/=0) call out_simple(g,'IF($I) THEN',x=m)
       call out_line(g,'IX=IX+1')
       call out_arg(g,v1,arg_ix_index)
       call out_char(g,'=')
       call out_arg(g,v2,0)
       call out_new_line(g)
       if(m/=0) call out_line(g,'ENDIF')
       call out_line(g,'ENDDO')
    else
       call out_arg(g,v1,arg_no_index)
       if(m==0) then
          call out_char(g,'=')
          call out_arg(g,v2,arg_wrapped)
          call out_str(g,' ! pack - copy')
       else
          call out_str(g,'=PACK(')
          call out_arg(g,v2,arg_wrapped)
          call out_char(g,',')
          call out_arg(g,m,arg_wrapped)
          call out_char(g,')')
       endif
       call out_new_line(g)
    endif
    if(debug_g) write(*,*) 'PACKED> v1=',v1
  end subroutine gen_pack

  !***************************************************************
  ! ROUTINES TO (UN)PACK DATA TO A COMMUNICATIONS BUFFER
  !***************************************************************

  subroutine gen_packables(g)
    type(gen_state),intent(inout):: g
    integer:: i,kind,tno
    type(pm_ptr):: key
    ! Generate any required COUNT/PACK/UNPACK subroutines
    i=1
    do while(i<=pm_set_size(g%context,g%packables))
       key=pm_set_key(g%context,g%packables,int(i,pm_ln))
       kind=key%data%i(key%offset)
       tno=key%data%i(key%offset+1)
       select case(kind)
       case(pack_scalar)
          call gen_count_routine(g,tno)
          call gen_pack_routine(g,tno)
          call gen_unpack_routine(g,tno)
       case(pack_vect,pack_vect_disp)
          call gen_vect_pack_to_buffer(g,tno,kind/=pack_vect)
       case(pack_array_vect,pack_array_vect_disp)
          call gen_array_vect_pack_to_buffer(g,tno,kind/=pack_array_vect)
       case(unpack_vect,unpack_vect_disp)
          call gen_vect_unpack_from_buffer(g,tno,kind/=unpack_vect)
       case default
          call pm_panic('gen_packables')
       end select
       i=i+1
    enddo
  end subroutine gen_packables

  !====================================================
  ! Flag that we need pack/unpack routines for type tno
  ! kind= which routines needed (pack_scalar..)
  !====================================================
  recursive subroutine g_add_packable(g,kind,tno)
    type(gen_state),intent(inout):: g
    integer,intent(in):: kind,tno
    integer:: key(2),i
    integer(pm_ln):: j
    if(tno<=pm_null) return
    key(1)=kind
    key(2)=tno
    j=pm_iset_add(g%context,g%packables,key,2)
  end subroutine  g_add_packable

  !===============================================
  ! Pack variable v to newly allocated buffer
  ! v is a vector length NA
  !==============================================
  subroutine gen_vect_pack_to_buffer(g,tno,isdisp)
    type(gen_state):: g
    integer,intent(in):: tno
    logical,intent(in):: isdisp
    integer(pm_ln),dimension(pm_int:pm_string):: counts
    logical:: has_depth
    call out_new_line(g)
    if(isdisp) then
       call out_simple(g,'SUBROUTINE PM__PACKDVEC$N(NA,X,D)',n=tno)
    else
       call out_simple(g,'SUBROUTINE PM__PACKVEC$N(NA,X)',n=tno)
    endif
    call out_line(g,'INTEGER(PM__LN):: NA')
    call out_type(g,tno)
    call out_line(g,',DIMENSION(:)::X')
    if(isdisp) call out_line(g,'INTEGER(PM__LN),DIMENSION(NA)::D')
    call out_line(g,'CALL PM__NEW_BUFFER')
    has_depth=.false.
    counts=0
    call out_line(g,'NP1=NA')
    call precount(g,tno,counts,has_depth)
    if(has_depth) then
       if(isdisp) then
          call out_line(g,'DO IX=1,NA')
          call out_line(g,'IP1=D(IX)+1')
       else
          call out_line(g,'DO IP1=1,NA')
       endif
       call outcount(g,tno,'X'//'(IP1)',1)
       call out_line(g,'ENDDO')
    endif
    call outaddcount(g,counts,1)
    call out_line(g,'CALL PM__ALLOCATE_BUFFER')
    call g_add_packable(g,pack_scalar,tno)
    if(isdisp) then
       call out_line(g,'DO IIX=1,NA')
       call out_line(g,'IX=D(IIX)+1')
    else
       call out_line(g,'DO IX=1,NA')
    endif
    call out_simple(g,'CALL PM__PACK$N(X(IX))',n=tno)
    call out_line(g,'ENDDO')
    if(isdisp) then
       call out_simple(g,'END SUBROUTINE PM__PACKDVEC$N',n=tno)
    else
       call out_simple(g,'END SUBROUTINE PM__PACKVEC$N',n=tno)
    endif
  end subroutine gen_vect_pack_to_buffer

  !===============================================
  ! Pack variable v to newly allocated buffer
  ! v is a vector length NA
  !==============================================
  subroutine gen_vect_unpack_from_buffer(g,tno,isdisp)
    type(gen_state):: g
    integer,intent(in):: tno
    logical,intent(in):: isdisp
    call out_new_line(g)
    if(isdisp) then
       call out_simple(g,'SUBROUTINE PM__UNPACKDVEC$N(NA,X,D)',n=tno)
    else
       call out_simple(g,'SUBROUTINE PM__UNPACKVEC$N(NA,X)',n=tno)
    endif
    call out_line(g,'INTEGER(PM__LN):: NA')
    call out_type(g,tno)
    call out_line(g,',DIMENSION(:):: X')
    if(isdisp) then
       call out_line(g,'INTEGER(PM__LN),DIMENSION(NA):: D')
    endif
    call g_add_packable(g,pack_scalar,tno)
    if(isdisp) then
       call out_line(g,'DO IIX=1,NA')
       call out_line(g,'IX=D(IIX)+1')
    else
       call out_line(g,'DO IX=1,NA')
    endif
    call out_simple(g,'CALL PM__UNPACK$N(X(IX))',n=tno)
    call out_line(g,'ENDDO')
    if(isdisp) then
       call out_simple(g,'END SUBROUTINE PM__UNPACKDVEC$N',n=tno)
    else
       call out_simple(g,'END SUBROUTINE PM__UNPACKVEC$N',n=tno)
    endif
  end subroutine gen_vect_unpack_from_buffer

  
  !==================================================
  ! Pack split array v1/cv2 to newly allocated buffer
  ! v1 and v2 are vectors length NA
  !===================================================
  subroutine gen_array_vect_pack_to_buffer(g,tno,isdisp)
    type(gen_state):: g
    integer,intent(in):: tno
    logical:: isdisp
    integer(pm_ln),dimension(pm_int:pm_string):: counts
    logical:: has_depth
    type(pm_ptr):: tv
    integer:: tno1,tno2
    tv=pm_typ_vect(g%context,tno)
    tno1=pm_tv_arg(tv,1)
    tno2=pm_tv_arg(tv,2)
    call out_new_line(g)
    if(isdisp) then
       call out_simple(g,'SUBROUTINE PM__PACKADVEC$N(NA,X,Y,D)',n=tno)
    else
       call out_simple(g,'SUBROUTINE PM__PACKAVEC$N(NA,X,Y)',n=tno)
    endif
    call out_line(g,'INTEGER(PM__LN):: NA')
    call out_simple(g,'TYPE(PM__TV$N),DIMENSION(:):: X',n=tno)
    call out_type(g,tno2)
    call out_line(g,',DIMENSION(:)::Y')
    if(isdisp) then
       call out_line(g,'INTEGER(PM__LN),DIMENSION(NA):: D')
    endif
    call out_line(g,'CALL PM__NEW_BUFFER')
    has_depth=.false.
    counts=0
    call precount(g,pm_tv_arg(tv,1),counts,has_depth)
    if(has_depth) then
       call out_simple(g,'NP1=0')
       if(isdisp) then
          call out_line(g,'DO IX=1,NA')
          call out_line(g,'IP1=D(IX)+1')
       else
          call out_line(g,'DO IP1=1,NA')
       endif
       call out_line(g,'NP2=SIZE('//'X'//'(IP1)%P)')
       call out_line(g,'NP1=NP1+NP2')
       call out_line(g,'DO IP2=1,NP2')
       call outcount(g,pm_tv_arg(tv,1),'X'//'(IP1)%P(IP2)',2)
       call out_line(g,'ENDDO')
       call out_line(g,'ENDDO')
    else
       call out_line(g,'NP1=NA')
    endif
    call outaddcount(g,counts,1)
    call out_line(g,'CALL PM__ALLOCATE_BUFFER')
    call g_add_packable(g,pack_scalar,tno1)
    call g_add_packable(g,pack_scalar,tno2)
    call g_add_packable(g,pack_scalar,int(pm_long))
    if(isdisp) then
       call out_line(g,'DO IIX=1,NA')
       call out_line(g,'IX=D(IIX)+1')
    else
       call out_line(g,'DO IX=1,NA')
    endif
    call out_str(g,'NV=SIZE(X(IX)%P)')
    call out_simple(g,'CALL PM__PACK$N(NV)',n=int(pm_long))
    call out_line(g,'DO IY=1,NV')
    call out_simple_part(g,'CALL PM__PACK$N(X(IX)%P(IY))',n=tno1)
    call out_line(g,'ENDDO')
    call out_simple_part(g,'CALL PM__PACK$N(Y)',n=tno2)
    call out_line(g,'ENDDO')
    if(isdisp) then
       call out_simple(g,'END SUBROUTINE PM__PACKADVEC$N',n=tno)
    else
       call out_simple(g,'END SUBROUTINE PM__PACKAVEC$N',n=tno)
    endif
  end subroutine gen_array_vect_pack_to_buffer


  !===============================================================================
  ! Create subroutine PM__PACKtno(X,N) to pack the N elements of X
  ! into the current buffer
  ! Buffer must be allocated to correct size (determined by PM__COUNT) beforehand
  !===============================================================================
  subroutine gen_pack_routine(g,tno)
    type(gen_state):: g
    integer,intent(in):: tno
    integer,dimension(pm_int:pm_string):: counts
    logical:: hasdepth
    logical:: recur
    recur=iand(pm_typ_flags(g%context,tno),pm_typ_has_poly)/=0
    counts=0
    call out_new_line(g)
    if(recur) call out_str(g,'RECURSIVE ')
    call out_str(g,'SUBROUTINE PM__PACK')
    call out_idx(g,tno)
    call out_line(g,'(X)')
    call out_type(g,tno)
    call out_line(g,'::X')
    call outpack(tno,'X',0)
    call out_str(g,'END SUBROUTINE PM__PACK')
    call out_idx(g,tno)
    call out_new_line(g)
  contains

    include 'fisnull.inc'
    
    recursive subroutine outpack(tno,varname,depth)
      integer,intent(in):: tno
      character(len=*),intent(in):: varname
      integer,intent(in):: depth
      type(pm_ptr):: tv,tlist,telem
      character(len=5):: ibuffer
      integer:: i,n,tno2
      tv=pm_typ_vect(g%context,tno)
      select case(pm_tv_kind(tv))
      case(pm_typ_is_basic)
         if(tno>=pm_int.and.tno<=pm_string) then
            call out_str(g,'PM__BUFFER%SIZEOF(')
            call out_kind(g,tno)
            call out_str(g,')=PM__BUFFER%SIZEOF(')
            call out_kind(g,tno)
            call out_line(g,')+1')
            call out_str(g,'PM__BUFFER%')
            call out_suffix(g,tno)
            call out_str(g,'(PM__BUFFER%SIZEOF(')
            call out_kind(g,tno)
            call out_str(g,'))')
            call out_char(g,'=')
            call out_line(g,varname)
         endif
      case(pm_typ_is_array)
         write(ibuffer,'(i5)') depth+1
         ibuffer=adjustl(ibuffer)
         call out_line(g,'NP'//trim(ibuffer)//'=SIZE('//varname//'%E1%P)')
         call outpack(int(pm_long),'NP'//trim(ibuffer),depth)
         call out_line(g,'DO IP'//trim(ibuffer)//'=1,NP'//trim(ibuffer))
         call outpack(pm_tv_arg(tv,1),varname//'%E1%P(IP'//trim(ibuffer)//')',depth+1)
         call out_line(g,'ENDDO')
         call outpack(pm_tv_arg(tv,2),varname//'%E2',depth)
      case(pm_typ_is_struct,pm_typ_is_rec,pm_typ_is_dref)
         do i=1,pm_tv_numargs(tv)
            write(ibuffer,'(i5)') i
            call outpack(pm_tv_arg(tv,i),varname//'%E'//trim(adjustl(ibuffer)),depth)
         enddo
      case(pm_typ_is_poly)
         call out_line(g,'PM__BUFFER%SIZEOF(PM__INT)=PM__BUFFER%SIZEOF(PM__INT)+1')
         call out_line(g,'PM__BUFFER%I(PM__BUFFER%SIZEOF(PM__INT))=0')
         call out_str(g,'SELECT TYPE(POLYVAR=>')
         call out_str(g,varname)
         call out_line(g,'%P)')
         tlist=g_check_poly(g,tno)
         if(.not.pm_fast_isnull(tlist)) then
            n=pm_set_size(g%context,tlist)
            do i=1,n
               telem=pm_set_key(g%context,tlist,int(i,pm_ln))
               tno2=telem%data%i(telem%offset)
               if(tno2<=pm_string) then
                  call out_simple(g,'TYPE IS(PM__T$S)',x=tno2)
                  call outpack(tno2,'POLYVAR%P',depth)
               else
                  call out_str(g,'TYPE IS(PM__T')
                  call out_idx(g,tno2)
                  call out_line(g,')')
                  call out_str(g,'CALL PM__PACK')
                  call out_idx(g,tno2)
                  call out_line(g,'(POLYVAR)')
               endif
               call out_simple(g,'PM__BUFFER%I(PM__BUFFER%SIZEOF(PM__INT))=$N',n=tno2)
            enddo
         endif
         call out_line(g,'END SELECT')
      case(pm_typ_is_single_name,pm_typ_is_proc,pm_typ_is_value)
         continue
      case(pm_typ_is_all,pm_typ_is_par_kind,pm_typ_is_enveloped,&
         pm_typ_is_vect)
       call outpack(pm_tv_arg(tv,1),varname,depth)
      end select
    end subroutine outpack
  end subroutine gen_pack_routine

  !===============================================================================
  ! Create a subroutine PM__UNPACKtno(X,N) to unpack N elements from buffer into X
  !===============================================================================
  subroutine gen_unpack_routine(g,tno)
    type(gen_state):: g
    integer,intent(in):: tno
    logical:: recur
    recur=iand(pm_typ_flags(g%context,tno),pm_typ_has_poly)/=0
    if(recur) call out_str(g,'RECURSIVE ')
    call out_str(g,'SUBROUTINE PM__UNPACK')
    call out_idx(g,tno) 
    call out_line(g,'(X)')
    call out_type(g,tno)
    call out_line(g,'::X')
    if(recur) call declare_poly_vars(tno)
    call outunpack(tno,'X',0)
    call out_str(g,'END SUBROUTINE PM__UNPACK')
    call out_idx(g,tno)
    call out_new_line(g)
  contains
    include 'fisnull.inc'
    
    recursive subroutine declare_poly_vars(tno)
      integer,intent(in):: tno
      type(pm_ptr):: tv,tlist,telem
      integer:: tno2,i,n
      tv=pm_typ_vect(g%context,tno)
      select case(pm_tv_kind(tv))
      case(pm_typ_is_poly)
         tlist=g_check_poly(g,tno)
         if(.not.pm_fast_isnull(tlist)) then
            n=pm_set_size(g%context,tlist)
            do i=1,n
               telem=pm_set_key(g%context,tlist,int(i,pm_ln))
               tno2=telem%data%i(telem%offset)
               if(tno2<=pm_string) then
                  call out_simple_part(g,'TYPE(PM__T$S)',x=tno2)
               else
                  call out_type(g,tno2)
               endif
               call out_str(g,'::PVAL')
               call out_idx(g,tno2)
               call out_new_line(g)
            enddo
         endif
      case(pm_typ_is_array,&
           pm_typ_is_struct,pm_typ_is_rec,pm_typ_is_dref)
         do i=1,pm_tv_numargs(tv)
            call declare_poly_vars(pm_tv_arg(tv,i))
         enddo
      end select
    end subroutine declare_poly_vars

    recursive subroutine outunpack(tno,varname,depth)
      integer,intent(in):: tno
      character(len=*),intent(in):: varname
      integer,intent(in):: depth
      type(pm_ptr):: tv,tlist,telem
      character(len=5):: ibuffer
      integer:: i,n,tno2
      tv=pm_typ_vect(g%context,tno)
      select case(pm_tv_kind(tv))
      case(pm_typ_is_basic)
         if(tno>=pm_int.and.tno<=pm_string) then
            call out_str(g,'PM__BUFFER%SIZEOF(')
            call out_kind(g,tno)
            call out_str(g,')=PM__BUFFER%SIZEOF(')
            call out_kind(g,tno)
            call out_line(g,')+1')
            call out_str(g,varname)
            call out_str(g,'=PM__BUFFER%')
            call out_suffix(g,tno)
            call out_str(g,'(PM__BUFFER%SIZEOF(')
            call out_kind(g,tno)
            call out_line(g,'))')
         endif
      case(pm_typ_is_array)
         write(ibuffer,'(i5)') depth+1
         ibuffer=adjustl(ibuffer)
         call outunpack(int(pm_long),'NP'//trim(ibuffer),depth)
         call out_line(g,'IF(ALLOCATED('//varname//'%E1%P)) DEALLOCATE('//varname//'%E1%P)')
         call out_line(g,'ALLOCATE('//varname//'%E1%P(NP'//trim(ibuffer)//'))')
         call out_line(g,'DO IP'//trim(ibuffer)//'=1,NP'//trim(ibuffer))
         call outunpack(pm_tv_arg(tv,1),varname//'%E1%P(IP'//trim(ibuffer)//')',depth+1)
         call out_line(g,'ENDDO')
         call outunpack(pm_tv_arg(tv,2),varname//'%E2',depth)
      case(pm_typ_is_struct,pm_typ_is_rec,pm_typ_is_dref)
         do i=1,pm_tv_numargs(tv)
            write(ibuffer,'(i5)') i
            call outunpack(pm_tv_arg(tv,i),varname//'%E'//trim(adjustl(ibuffer)),depth)
         enddo
      case(pm_typ_is_poly)
         call out_line(g,'PM__BUFFER%SIZEOF(PM__INT)=PM__BUFFER%SIZEOF(PM__INT)+1')
         call out_line(g,'SELECT CASE(PM__BUFFER%I(PM__BUFFER%SIZEOF(PM__INT)))')
         tlist=g_check_poly(g,tno)
         if(.not.pm_fast_isnull(tlist)) then
            n=pm_set_size(g%context,tlist)
            do i=1,n
               telem=pm_set_key(g%context,tlist,int(i,pm_ln))
               tno2=telem%data%i(telem%offset)
               call out_str(g,'CASE(')
               call out_idx(g,tno2)
               call out_line(g,')')
               write(ibuffer,'(i5)') tno2
               if(tno2<=pm_string) then
                  call outunpack(tno2,'PVAL'//trim(adjustl(ibuffer))//'%P',depth)
               else
                  call out_str(g,'CALL PM__UNPACK')
                  call out_idx(g,tno2)
                  call out_line(g,'(PVAL'//trim(adjustl(ibuffer))//')')
               endif
               call out_simple(g,'ALLOCATE('//varname//'%P,SOURCE=PVAL$N)',n=tno2)
            enddo
         endif
         call out_line(g,'END SELECT')
      case(pm_typ_is_single_name,pm_typ_is_proc,pm_typ_is_value)
         continue
      case(pm_typ_is_all,pm_typ_is_par_kind,pm_typ_is_enveloped,&
           pm_typ_is_vect)
         call outunpack(pm_tv_arg(tv,1),varname,depth)
      end select
    end subroutine outunpack
  end subroutine gen_unpack_routine  

  
  !============================================================
  ! Generate a subroutine PM__COUNTtno(X,N) to count basic 
  ! elements in X (including dynamic sub-elements)
  ! placing result in current buffer
  !============================================================
  subroutine gen_count_routine(g,tno)
    type(gen_state):: g
    integer,intent(in):: tno
    integer(pm_ln),dimension(pm_int:pm_string):: counts
    logical:: has_depth
    logical:: recur
    recur=iand(pm_typ_flags(g%context,tno),pm_typ_has_poly)/=0
    if(recur) call out_str(g,'RECURSIVE ')
    call out_str(g,'SUBROUTINE PM__COUNT')
    call out_idx(g,tno)
    call out_line(g,'(X)')
    call out_type(g,tno)
    call out_line(g,'::X')
    counts=0
    has_depth=.false.
    call precount(g,tno,counts,has_depth)
    call outaddcount(g,counts,0)
    if(has_depth) then
       call outcount(g,tno,'X',0)
    endif
    call out_str(g,'END SUBROUTINE PM__COUNT')
    call out_idx(g,tno)
    call out_new_line(g)
  end subroutine gen_count_routine
    
  !=======================================================
  ! Determine the compile-time count of each basic type
  ! in type tno. Returns has_depth if there are elements 
  ! with dynamic run-time sizes (arrays/polymorphic types)
  !=======================================================
  recursive subroutine precount(g,tno,counts,has_depth)
    type(gen_state):: g
    integer,intent(in):: tno
    integer(pm_ln),intent(inout),dimension(pm_int:pm_string):: counts
    logical,intent(inout):: has_depth
    integer(pm_ln),dimension(pm_int:pm_string):: counts2
    logical:: has_depth2
    type(pm_ptr):: tv,val
    integer:: i
    integer(pm_ln):: n    
    tv=pm_typ_vect(g%context,tno)
    select case(pm_tv_kind(tv))
    case(pm_typ_is_basic)
       if(tno>=pm_int.and.tno<=pm_string) then
          counts(tno)=counts(tno)+1
       endif
    case(pm_typ_is_array)
       has_depth2=.false.
       call precount(g,pm_tv_arg(tv,2),counts,has_depth2)
       if(pm_tv_arg(tv,3)/=pm_long.and..not.has_depth2) then
          call precount(g,pm_tv_arg(tv,1),counts2,has_depth)
          if(.not.has_depth) then
             val=pm_typ_val(g%context,pm_tv_arg(tv,3))
             counts=counts+counts2*val%data%ln(val%offset)
          endif
       else
          has_depth=.true.
       endif
       counts(pm_long)=counts(pm_long)+1
    case(pm_typ_is_struct,pm_typ_is_rec,pm_typ_is_dref)
       do i=1,pm_tv_numargs(tv)
          call precount(g,pm_tv_arg(tv,i),counts,has_depth)
       enddo
    case(pm_typ_is_poly)
       counts(pm_int)=counts(pm_int)+1
       has_depth=.true.
    case(pm_typ_is_single_name,pm_typ_is_proc,pm_typ_is_value)
       continue
    case default
       call pm_panic("precount")
    end select
  end subroutine precount

  !===================================================================
  ! Code the addition of basic types counts determined by precount
  ! to the counts entries in the current buffer
  !===================================================================
  subroutine outaddcount(g,counts,depth)
    type(gen_state):: g
    integer(pm_ln),intent(inout),dimension(pm_int:pm_string):: counts
    integer,intent(in):: depth
    integer:: i
    do i=pm_int,pm_string
       if(counts(i)>0) then
          call out_str(g,'PM__BUFFER%SIZEOF(')
          call out_kind(g,i)
          call out_str(g,')=PM__BUFFER%SIZEOF(')
          call out_kind(g,i)
          call out_str(g,')+')
          call out_long(g,counts(i))
          if(depth>0) then
             call out_simple_part(g,'*NP$N',n=depth)
          endif
          call out_new_line(g)
       endif
    enddo
  end subroutine outaddcount

  !============================================================
  ! Output code to count dynamic elements of the type
  ! -- static elements already counted by precount
  !============================================================
  recursive subroutine outcount(g,tno,varname,depth)
    type(gen_state):: g
    integer,intent(in):: tno
    character(len=*):: varname
    integer,intent(in):: depth
    type(pm_ptr):: tv,tlist,telem
    integer:: i,n,tno2
    character(len=5):: ibuffer
    integer(pm_ln),dimension(pm_int:pm_string):: counts
    logical:: has_depth
    tv=pm_typ_vect(g%context,tno)
    select case(pm_tv_kind(tv))
    case(pm_typ_is_basic)
       continue
    case(pm_typ_is_array)
       call outcount(g,pm_tv_arg(tv,2),varname//'%E2',depth)
       has_depth=.false.
       counts=0
       call precount(g,pm_tv_arg(tv,1),counts,has_depth)
       if(has_depth) then
          write(ibuffer,'(i5)') depth+1
          ibuffer=adjustl(ibuffer)
          call get_vect_size(varname)
          call out_simple(g,'DO IP$N=1,NP$N',n=depth+1)
          call outcount(g,pm_tv_arg(tv,1),varname//'%E1%P(IP'//trim(ibuffer)//')',depth+1)
          call out_line(g,'ENDDO')
       else
          call get_vect_size(varname)
       endif
       call outaddcount(g,counts,depth+1)
    case(pm_typ_is_poly)
       call out_line(g,'SELECT TYPE(POLYVAR=>'//varname//'%P)')
       tlist=g_check_poly(g,tno)
       if(.not.pm_fast_isnull(tlist)) then
          n=pm_set_size(g%context,tlist)
          do i=1,n
             telem=pm_set_key(g%context,tlist,int(i,pm_ln))
             tno2=telem%data%i(telem%offset)
             if(tno2<=pm_string) then
                call out_simple(g,'TYPE IS(PM__T$S)',x=tno2)
                counts=0
                call precount(g,tno2,counts,has_depth)
                call outaddcount(g,counts,depth)
             else
                call out_simple(g,'TYPE IS(PM__T$N)',n=tno2)
                call g_add_packable(g,pack_scalar,tno2)
                call out_str(g,'CALL PM__COUNT')
                call out_idx(g,tno2)
                call out_line(g,'(POLYVAR)')
             endif
          enddo
       endif
       call out_line(g,'END SELECT')
    case(pm_typ_is_struct,pm_typ_is_rec,pm_typ_is_dref)
       do i=1,pm_tv_numargs(tv)
          write(ibuffer,'(i5)') i
          call outcount(g,pm_tv_arg(tv,i),varname//'%E'//trim(adjustl(ibuffer)),depth)
       enddo
    case(pm_typ_is_single_name,pm_typ_is_proc,pm_typ_is_value)
       continue
    case(pm_typ_is_all,pm_typ_is_par_kind,pm_typ_is_enveloped,&
         pm_typ_is_vect)
       call outcount(g,pm_tv_arg(tv,1),varname,depth)
    end select
  contains
    include 'fisnull.inc'
    
    subroutine get_vect_size(var)
      character(len=*):: var
      call out_simple(g,'NP$N=SIZE('//var//'%E1%P)',n=depth+1)
    end subroutine get_vect_size
  end subroutine outcount

  !========================================================
  ! Create subroutine PM__MAKE_MPI_TYPES that
  ! creates all required MPI types from PM types,
  ! fills global PM__MPI_TYPES
  !========================================================
  subroutine gen_mpi_types(g)
    type(gen_state):: g
    integer:: i,j,n,nn,size,typ,maxargs,set_key(1)
    type(pm_ptr):: keys,key,tv

    call out_new_line(g)
    call out_line_noindent(g,'SUBROUTINE PM__MAKE_MPI_TYPES')

    size=pm_set_size(g%context,g%mpi_types)
    if(size==0) then
       call out_line_noindent(g,'END SUBROUTINE PM__MAKE_MPI_TYPES')
       return
    endif
    
    keys=pm_set_keys(g%context,g%mpi_types)
    maxargs=0
    do i=0,size-1
       key=keys%data%ptr(keys%offset+i)
       typ=abs(key%data%i(key%offset))
       !write(*,*) 'TYPE #',i,' ',trim(pm_typ_as_string(g%context,typ))
       call out_type(g,typ)
       call out_str(g,'::T')
       call out_idx(g,typ)
       call out_line(g,'(2)')
       tv=pm_typ_vect(g%context,typ)
       maxargs=max(maxargs,pm_tv_numargs(tv))
    enddo
    call out_simple(g,'INTEGER,DIMENSION($N):: DATATYPES,BLOCKLENGTHS',n=maxargs)
    call out_simple(g,'INTEGER(MPI_ADDRESS_KIND),DIMENSION($N):: OFFSETS',n=maxargs)
    call out_line(g,'INTEGER(MPI_ADDRESS_KIND):: LOWER,EXTENT')
    call out_line(g,'BLOCKLENGTHS=1')
    call out_simple(g,'ALLOCATE(PM__MPI_TYPES($N))',n=size)
    
    do i=0,size-1
       key=keys%data%ptr(keys%offset+i)
       typ=abs(key%data%i(key%offset))
       call out_comment_line(g,pm_typ_as_string(g%context,typ))
       if(debug_g) then
          write(*,*) 'MPI TYP>',pm_typ_as_string(g%context,typ)
       endif
       tv=pm_typ_vect(g%context,typ)
       n=pm_tv_numargs(tv)
       nn=0
       do j=1,n
          if(iand(pm_typ_flags(g%context,pm_tv_arg(tv,j)),&
               pm_typ_has_storage)/=0) then
             call out_simple(g,'CALL MPI_GET_ADDRESS(T$N(1)%E$M,OFFSETS($M),JERROR)',&
                  n=typ,m=j)
             nn=nn+1
          endif
       enddo
       do j=2,nn
          call out_simple(g,'OFFSETS($N)=OFFSETS($N)-OFFSETS(1)',n=j)
       enddo
       call out_simple(g,'CALL MPI_GET_ADDRESS(T$N(2),EXTENT,JERROR)',n=typ)
       call out_line(g,'EXTENT=EXTENT-OFFSETS(1)')
       call out_line(g,'OFFSETS(1)=0')
       do j=1,nn
          call out_comment_line(g,pm_typ_as_string(g%context,pm_tv_arg(tv,j)))
          call out_get_mpi_base_type(g,pm_tv_arg(tv,j))
          call out_simple(g,'DATATYPES($N)=JBASE',n=j)
       enddo
       call out_simple(g,&
            'CALL MPI_TYPE_CREATE_STRUCT($N,BLOCKLENGTHS,OFFSETS,DATATYPES,JTYPE,JERROR)',&
            n=nn)
       call out_line(g,'CALL MPI_TYPE_CREATE_RESIZED(JTYPE,0_MPI_ADDRESS_KIND,EXTENT,JTYPE2,JERROR)')
       call out_simple(g,'PM__MPI_TYPES($N)=JTYPE2',n=i+1)
    enddo

    size=pm_set_size(g%context,g%mpi_root_types)
    keys=pm_set_keys(g%context,g%mpi_root_types)
    do i=0,size-1
       key=keys%data%ptr(keys%offset+i)
       set_key(1)=abs(key%data%i(key%offset))
       j=pm_ivect_lookup(g%context,g%mpi_types,set_key,1)
       if(j<0) call pm_panic('gen_mpi_types')
       call out_simple(g,'CALL MPI_TYPE_COMMIT(PM__MPI_TYPES($N),JERROR)',n=j)
    enddo
    
    call out_line_noindent(g,'END SUBROUTINE PM__MAKE_MPI_TYPES')
  end subroutine gen_mpi_types
  

  !********************************************************************
  ! VARIABLE AND TYPE DEFINITIONS
  !********************************************************************

  !============================================
  ! Output definition for variable at index i
  ! - in communicating proc, iscomm==.true.
  !============================================
  subroutine out_var_def(g,i,iscomm)
    type(gen_state):: g
    integer,intent(in):: i
    logical,intent(in):: iscomm
    integer:: ix,flags,j,oindex
    logical:: isvect

    isvect=g_var_at_index_is_a_vect(g,i)
    flags=g%vardata(i)%flags
    ix=abs(g%vardata(i)%index)
    
    if(iand(g%vardata(i)%gflags,var_is_recycled)/=0) then
       return
    endif

    oindex=g%vardata(i)%oindex
    if(debug_g) then
       write(*,*) 'VAR>>',i,oindex,' ',trim(pm_typ_as_string(g%context,g%vardata(i)%tno))
    endif

!!$    if(g_kind(g,oindex)==v_is_basic) call out_simple(g,'!'//&
!!$         trim(pm_name_as_string(g%context,g_v1(g,oindex))))
    
!!$    call out_simple_part(g,'! idx=$N / lthis=$M '//&
!!$         trim(pm_typ_as_string(g%context,g%vardata(i)%tno)),&
!!$         n=g%vardata(i)%oindex,m=g%vardata(i)%lthis)
!!$    call out_simple_part(g,'/ flags=$N state=$M',n=g%vardata(i)%flags,m=g%vardata(i)%state)
!!$    call out_simple_part(g,'/ start=$N finish=$M',n=g%vardata(i)%start,m=g%vardata(i)%finish)
!!$    call out_simple(g,'/ end_assign=$N',n=merge(1,0,g%vardata(i)%finish_on_assign))
    
    if(iand(flags,v_is_chan)/=0) then
       call out_str(g,'TYPE(PM__TV')
       call out_type_idx(g,g%vardata(i)%tno)
       call out_str(g,'_7)')
       isvect=.false.
    elseif(iand(flags,v_is_array_par_vect)/=0) then
       call out_str(g,'TYPE(PM__TV')
       call out_type_idx(g,g%vardata(i)%tno)
       call out_char(g,'_')
       call out_type_idx(g,g_v1(g,oindex))
       call out_char(g,')')
    else
       call out_type(g,g%vardata(i)%tno)
    endif

    if(iand(flags,v_is_ref+v_is_param)==v_is_ref+v_is_param) then
       call out_str(g,',INTENT(INOUT)')
    elseif(iand(flags,v_is_param)/=0) then
       call out_str(g,',INTENT(IN)')
    elseif(iand(flags,v_is_result)/=0) then
       if(iand(flags,v_is_chan)/=0) then
          call out_str(g,',INTENT(INOUT)')
       else
          call out_str(g,',INTENT(OUT)')
       endif
    endif

    if(iand(flags,v_is_key)/=0) call out_str(g,',OPTIONAL')
 
    if(isvect) then
       if(iand(flags,v_is_array_par_vect+v_is_array_par_dom)/=0) then
          call out_str(g,',DIMENSION(:)')
       elseif(iand(flags,v_is_param+v_is_result)/=0) then
          if(.not.iscomm) then
             call out_str(g,',DIMENSION(:)')
          else
             call out_str(g,',DIMENSION(N1)')
          endif
       else
          call out_str(g,',ALLOCATABLE,DIMENSION(:)')
       endif
    endif
    
    call out_str(g,'::')
    call out_var_name_at_index(g,i)

    call out_str(g,'    ! '//trim(pm_typ_as_string(g%context,g%vardata(i)%tno)))

    if(pm_opts%ftn_annotate) then
       call out_simple_part(g,'  idx=$N', n=oindex)
    endif
    
    call out_new_line(g)
  end subroutine out_var_def

  !===========================================
  ! Output definitions for all types in
  ! typeset
  !============================================
  subroutine out_type_defs(g,typeset)
    type(gen_state):: g
    type(pm_ptr),intent(in):: typeset
    integer:: i
    type(pm_ptr):: keys,key
    keys=pm_set_keys(g%context,typeset)
    do i=0,pm_set_size(g%context,typeset)-1
       key=keys%data%ptr(keys%offset+i)
       call out_type_def(g,key%data%i(key%offset),key%data%i(key%offset+1))
       call out_new_line(g)
    enddo
  contains
    include 'fesize.inc'
  end subroutine out_type_defs

  !========================================
  ! Output type definition for PM type tno
  ! - dim==0        : plain type
  ! - dim==pm_long  : allocatable vector
  ! - dim==...      : fixed length vector
  !========================================
  subroutine out_type_def(g,tno,dim)
    type(gen_state):: g
    integer,intent(in):: tno,dim
    type(pm_ptr):: tv,val
    integer:: i,n,k 
    if(tno==0) return
    if(iand(pm_typ_flags(g%context,tno),&
         pm_typ_has_storage)==0) return
    if(dim>0) then
       call out_comment_line(g,trim(pm_typ_as_string(g%context,tno)))
       call out_str(g,'TYPE PM__TV')
       call out_idx(g,tno)
       call out_char_idx(g,'_',dim)
       call out_new_line(g)
       call out_type(g,tno)
       if(dim==pm_long) then
          call out_line(g,',DIMENSION(:),ALLOCATABLE::P')
       else
          val=pm_typ_val(g%context,dim)
          call out_str(g,',DIMENSION(')
          call out_const(g,val)
          call out_line(g,')::P')
       endif
       call out_str(g,'END TYPE PM__TV')
       call out_idx(g,tno)
       call out_char_idx(g,'_',dim)
       call out_new_line(g)
       return
    endif
    call out_char(g,'!')
    call out_comment_line(g,trim(pm_typ_as_string(g%context,tno)))
    call out_str(g,'TYPE PM__T')
    call out_idx(g,tno)
    call out_new_line(g)
    !call out_line(g,',SEQUENTIAL')
    tv=pm_typ_vect(g%context,tno)
    n=pm_tv_numargs(tv)
    k=pm_tv_kind(tv)
    if(k==pm_typ_is_array) then
       call out_str(g,'TYPE(PM__TV')
       if(iand(pm_typ_flags(g%context,pm_tv_arg(tv,1)),&
            pm_typ_has_storage)/=0) then
          call out_type_idx(g,pm_tv_arg(tv,1))
          call out_char(g,'_')
          call out_type_idx(g,pm_tv_arg(tv,3))
          call out_line(g,')::E1')
       endif
       if(iand(pm_typ_flags(g%context,pm_tv_arg(tv,2)),&
            pm_typ_has_storage)/=0) then
          call out_type(g,pm_tv_arg(tv,2))
          call out_line(g,'::E2')
       endif
    elseif(k==pm_typ_is_vect) then
       continue
    else
       do i=1,n
          if(iand(pm_typ_flags(g%context,pm_tv_arg(tv,i)),&
               pm_typ_has_storage)/=0) then
             call out_type(g,pm_tv_arg(tv,i))
             call out_str(g,'::E')
             call out_idx(g,i)
             call out_new_line(g)
          endif
       enddo
    endif
    call out_str(g,'END TYPE PM__T')
    call out_idx(g,tno)
    call out_new_line(g)
  end subroutine out_type_def

  !=================================================
  ! Output the type index for a given type
  ! -- type number expect for polymorphic types
  !================================================
  subroutine out_type_idx(g,tno)
    type(gen_state):: g
    integer,intent(in):: tno
    if(pm_typ_kind(g%context,tno)==pm_typ_is_poly) then
       call out_idx(g,int(pm_pointer))
    else
       call out_idx(g,tno)
    endif
  end subroutine out_type_idx
  
  !============================================================
  ! Output code to allocate avar to size nc
  !============================================================
  recursive subroutine out_alloc_var(g,avar,nc)
    type(gen_state):: g
    integer,intent(in):: avar
    character(len=*),intent(in):: nc
    integer:: var,i,n
    var=abs(avar)
    select case(g_kind(g,var))
    case(v_is_group)
       n=g_v1(g,var)
       do i=1,n
          call out_alloc_var(g,g_ptr(g,var,i),nc)
       enddo
    case(v_is_sub,v_is_vsub,v_is_elem,v_is_unit_elem)
       write(*,*) 'v_',var
       call pm_panic('out_alloc_var')
    case(v_is_cove)
       call out_alloc_var(g,g_v2(g,var),nc)
    case(v_is_alias)
       call out_alloc_var(g,g_v1(g,var),nc)
    case(v_is_const,v_is_ctime_const)
       continue
    case default
       call out_str(g,'ALLOCATE(')
       call out_var_name_at_index(g,g_index(g,var))
       call out_line(g,'('//nc//'))')
    end select
  end subroutine out_alloc_var

  !============================================================
  ! Output code for simple operation with one output
  !============================================================
  subroutine out_simple_scalar(g,str,l,n,x)
    type(gen_state):: g
    character(len=*),intent(in):: str
    integer,intent(in):: l
    integer,intent(in),optional:: n,x
    integer:: arg1,ve
    ve=g%codes(l+comp_op_arg0)
    arg1=g%codes(l+comp_op_arg0+1)
    if(.not.g_is_shared(g,arg1)) then
       call gen_loop(g,l,.false.)
    else
       ! Close and re-open if statements
       if(g%last_ve/=ve) then
          call gen_if_nest(g,g%last_ve,ve)
          g%last_ve=ve
       endif
    endif
    if(g_kind(g,arg1)/=v_is_ctime_const) then
       call out_simple(g,str,l,n,x)
    endif
  end subroutine out_simple_scalar


  !=========================================================
  ! Return code string expanded with variable/value info
  ! followed by new line
  ! For expansions see out_simple_part
  !=========================================================
  recursive subroutine out_simple(g,str,l,n,m,x)
    type(gen_state):: g
    character(len=*),intent(in):: str
    integer,intent(in),optional:: l,n,m,x
    call out_simple_part(g,str,l,n,m,x)
    call out_new_line(g)
  end subroutine out_simple

  !=========================================================
  ! Return code string expanded with variable/value info
  ! Expansions:
  !    $0 .. $9   - given argument (l must be passed)
  !    $(10)...
  !    $#0 .. $#9 - given argument without index
  !    $#(10)...
  !    $A         - variable passed as x without index
  !    $I         - variable passed as x
  !    $N         - integer value passed as n
  !    $M         - integer value passed as m
  !    $X         - argument x (l must be passed)
  !    $Y         - argument x no index
  !    $S         - suffix string for pm basic type x
  !=========================================================
  recursive subroutine out_simple_part(g,str,l,n,m,x)
    type(gen_state):: g
    character(len=*),intent(in):: str
    integer,intent(in),optional:: l,n,m,x
    integer:: i,j,k,opt,tens
    character:: c
    opt=0
    k=len(str)
    j=1
    do while(j<=k)
       c=str(j:j)
       if(c/='$') then
          g%n=g%n+1
          g%linebuffer(g%n:g%n)=c
       else
          j=j+1
          c=str(j:j)
          select case(c)
          case('#')
             j=j+1
             c=str(j:j)
             call out_arg(g,g%codes(l+comp_op_arg0+iachar(c)-iachar('0')),arg_no_index)
          case('0','1','2','3','4','5','6','7','8','9')
             call out_arg(g,g%codes(l+comp_op_arg0+iachar(c)-iachar('0')),0)
          case('(')
             j=j+1
             c=str(j:j)
             tens=iachar(c)-iachar('0')
             j=j+1
             c=str(j:j)
             call out_arg(g,g%codes(l+comp_op_arg0+tens*10+iachar(c)-iachar('0')),0)
             j=j+1
             if(str(j:j)/=')') call pm_panic('out_simple: bad $( arg')
          case('A')
             call out_arg(g,x,arg_no_index)
          case('I')
             call out_arg(g,x,0)
             
          case('N')
             call out_idx(g,n)
          case('M')
             call out_idx(g,m)
          case('X')
             call out_arg(g,g%codes(l+comp_op_arg0+x),0)
          case('Y')
             call out_arg(g,g%codes(l+comp_op_arg0+x),arg_no_index)
          case('S')
             call out_suffix(g,x)
          case default
             write(*,*) 'Bad char (',c,')'
             call pm_panic('Bad char in out_simple')
          end select
       endif
       j=j+1
       if(g%n+k-j>ftn_max_line) call out_line_break(g)
    enddo
  end subroutine out_simple_part

  !=======================================================
  ! Output argument to a procedure call
  ! - expand all arrays to two components
  ! - ignore compile time constants
  ! - pass channels directly
  ! Options
  !     arg_no_index  Do not index vectors
  !     arg_ix_index  Index vectors with IX
  !     arg_comm_arg  Argument to a communicating procedure  
  !     arg_wrapped   Argument is wrapped vector
  !=======================================================
  recursive subroutine out_call_arg(g,avar,opts)
    type(gen_state):: g
    integer,intent(in):: avar,opts
    integer:: var,i,n,k,v,tno
    type(pm_ptr):: tv
    var=abs(avar)
    k=g_kind(g,var)
    select case(k)
    case(v_is_group)
       select case(g_v2(g,var))
       case(v_is_array)
          call out_arg(g,g_ptr(g,var,1),opts)
          call out_comma(g)
          call out_arg(g,g_ptr(g,var,2),opts)
       case(v_is_dref,v_is_shared_dref)
          n=g_v1(g,var)
          if(iand(opts,arg_wrapped)/=0) then
             tv=pm_typ_vect(g%context,g_type(g,var))
             call out_dref_vect_arg(g,g_ptr(g,var,1),pm_tv_arg(tv,1),opts)
             call out_comma(g)
             call out_call_arg(g,g_ptr(g,var,2),opts)
             call out_comma(g)
             do i=3,n
                call  out_dref_vect_arg(g,g_ptr(g,var,i),pm_tv_arg(tv,i),opts)
                if(i/=n) call out_comma(g)
             enddo
          else
             do i=1,n
                call out_call_arg(g,g_ptr(g,var,i),opts)
                if(i/=n) call out_comma(g)
             enddo
          end if
       case default
          n=g_v1(g,var)
          do i=1,n
             call out_call_arg(g,g_ptr(g,var,i),opts)
             if(i/=n) call out_comma(g)
          enddo
       end select
    case(v_is_ctime_const)
       continue
    case(v_is_chan_vect)
       if(iand(opts,arg_comm_arg)==0) then   
          call out_call_arg(g,g_v1(g,var),ior(opts,arg_chan))
       else
          call out_arg(g,g_v1(g,var),opts)
       endif
    case(v_is_vect_wrapped)
       call out_call_arg(g,g_v1(g,var),ior(opts,arg_wrapped))
    case(v_is_alias)
       call out_call_arg(g,g_v1(g,var),opts)
    case default
       tno=g_type(g,var)
       if(pm_typ_kind(g%context,tno)==pm_typ_is_array) then
          tv=pm_typ_vect(g%context,tno)
          if(iand(pm_typ_flags(g%context,pm_tv_arg(tv,1)),pm_typ_has_storage)/=0) then
             call out_arg(g,var,opts)
             call out_str(g,'%E1,')
          endif
          if(iand(pm_typ_flags(g%context,pm_tv_arg(tv,2)),pm_typ_has_storage)/=0) then
             call out_arg(g,var,opts)
             call out_str(g,'%E2')
          endif
          return
       else
          call out_arg(g,var,opts)
       endif
    end select
  end subroutine out_call_arg

  !===================================================
  ! Output one element of a dref
  ! - if the element is a vector it is wrapped
  !===================================================
  recursive subroutine out_dref_vect_arg(g,var,tno,opts)
    type(gen_state):: g
    integer,intent(in):: var,tno,opts
    if(pm_typ_get_mode(g%context,tno)>=sym_mirrored) then
       call out_call_arg(g,var,opts)
    else
       call out_call_arg(g,var,ior(opts,arg_wrapped))
    endif
  end subroutine out_dref_vect_arg

  !================================================
  ! Output parameter name
  ! - split groups
  ! - ignore compile time constants
  !================================================
  recursive subroutine out_param(g,avar)
    type(gen_state):: g
    integer,intent(in):: avar
    integer:: var,i,n,k
    var=abs(avar)
    k=g_kind(g,var)
    select case(k)
    case(v_is_group)
       n=g_v1(g,var)
       do i=1,n
          call out_param(g,g_ptr(g,var,i))
          call out_comma(g)
       enddo
    case(v_is_alias,v_is_chan_vect,v_is_vect_wrapped)
       call out_param(g,g_v1(g,var))
    case(v_is_ctime_const)
       continue
    case(v_is_cove)
       call out_param(g,g_v2(g,var))
    case default
       call out_arg_name(g,var,0)
    end select
  end subroutine out_param
  
  !================================================
  ! Output argument
  ! opts -
  !     arg_no_index  Do not index vectors
  !     arg_ix_index  Index vectors with IX
  !     arg_wrapped   Argument is wrapped vector
  !================================================
  recursive subroutine out_arg(g,avar,opts)
    type(gen_state):: g
    integer,intent(in):: avar,opts
    integer:: var,i,n,k
    var=abs(avar)
    k=g_kind(g,var)
    select case(k)
    case(v_is_group)
       n=g_v1(g,var)
       do i=1,n
          call out_arg(g,g_ptr(g,var,i),opts)
          if(i/=n) call out_comma(g)
       enddo
    case(v_is_sub)
       call out_arg(g,g_v1(g,var),opts)
       call out_str(g,'%E1%P((')
       call out_arg(g,g_v2(g,var),opts)
       call out_str(g,')+1)')
    case(v_is_vsub)
       call out_arg(g,g_v1(g,var),opts)
       call out_str(g,'%P((')
       call out_arg(g,g_v2(g,var),opts)
       call out_str(g,')+1)')
    case(v_is_vect_wrapped)
       call out_arg(g,g_v1(g,var),ior(opts,arg_wrapped))
    case(v_is_elem)
       call out_elem(g,g_v1(g,var),g_v2(g,var),opts)
    case(v_is_unit_elem)
       call out_arg(g,g_v1(g,var),opts)
    case(v_is_chan_vect)
       call out_arg(g,g_v1(g,var),ior(opts,arg_chan))
    case(v_is_const)
       call out_const(g,g%fn%data%ptr(g%fn%offset+2+g_v1(g,var)))
    case(v_is_ctime_const)
       call out_const(g,g%fn%data%ptr(g%fn%offset+2+g_v1(g,var)))
    case(v_is_cove)
       call out_arg(g,g_v2(g,var),opts)
    case(v_is_alias)
       call out_arg(g,g_v1(g,var),opts)
    case default
       call out_arg_name(g,var,opts)
       if(iand(opts,arg_chan)/=0) then
          call out_str(g,'%P')
          call out_loop_index(g,var,opts)
       elseif(.not.g_flags_set(g,var,v_is_chan)) then
          call out_loop_index(g,var,opts)
       endif
    end select
  end subroutine out_arg

  !=============================================
  ! Output variable name associated with var
  !=============================================
  subroutine out_arg_name(g,var,opt)
    type(gen_state):: g
    integer,intent(in):: var,opt
    call out_var_name_at_index(g,g_index(g,var))
  end subroutine out_arg_name

  !==============================================================
  ! If variable v is a vector than append the required index
  ! - usually the index variable for the associated parallel
  !   context
  ! - can also be IX for opt==arg_ix_index
  ! - can also be IDO for use inside op_do_at
  ! - can also be 1 outside of active loop for given context
  !=============================================================
  subroutine out_loop_index(g,v,opt)
    type(gen_state):: g
    integer,intent(in):: v,opt
    integer:: i,lthis
    
    if(g_is_a_vect(g,v)) then
       lthis=g_lthis(g,v)
       if(iand(opt,arg_ix_index)/=0.and.g_is_vect(g,v)) then
          call out_str(g,'(IX)')
       elseif(iand(opt,arg_no_index+arg_wrapped)==0.or.lthis/=g%lthis.and.iand(opt,arg_wrapped)==0) then
          if(lthis==g%lalt) then
             call out_str(g,'(IDO)')
          elseif(.not.g%lstack(lthis)%loop_active.or.lthis==0) then
             call out_str(g,'(1)')
          else
             call out_char(g,'(')
             call out_char_idx(g,'I',lthis)
             call out_char(g,')')
          endif
       endif
       if(g_flags_set(g,v,v_is_array_par_vect)) then
          call out_str(g,'%P')
       endif
    endif
  end subroutine out_loop_index

  !====================================================
  ! Output var.n as var%n
  !====================================================
  recursive subroutine out_elem(g,var,n,opts)
    type(gen_state):: g
    integer,intent(in):: var,n,opts
    call out_arg(g,var,opts)
    call out_char(g,'%')
    call out_char_idx(g,'E',n)
  end subroutine out_elem

  !====================================================
  ! Output variable at given index
  !====================================================
  subroutine out_var_at_index(g,i)
    type(gen_state):: g
    integer,intent(in):: i
    call out_var_name_at_index(g,i)
  end subroutine out_var_at_index

  !====================================================
  ! Output constant value associated with variable v
  !====================================================
  subroutine out_const(g,v)
    type(gen_state):: g
    type(pm_ptr),intent(in):: v
    character(len=max_line):: buffer
    integer:: vk,i,n
    buffer=' '
    vk=pm_fast_vkind(v)
    select case(vk)
    case(pm_int)
       write(buffer,*) v%data%i(v%offset)
    case(pm_long)
       write(buffer,*) v%data%ln(v%offset)
       call append('_PM__LN')
    case(pm_longlong)
       write(buffer,*) v%data%lln(v%offset)
       call append('_PM__LN')
    case(pm_int8)
       write(buffer,*) v%data%i8(v%offset)
       call append('_PM__I8')
    case(pm_int16)
       write(buffer,*) v%data%i16(v%offset)
       call append('_PM__I16')
    case(pm_int32)
       write(buffer,*) v%data%i32(v%offset)
       call append('_PM__I32')
    case(pm_int64)
       write(buffer,*) v%data%i64(v%offset)
       call append('_PM__I64')
    case(pm_single)
       write(buffer,*) v%data%r(v%offset)
    case(pm_double)
       write(buffer,*) v%data%d(v%offset)
       call append('_PM__D')
    case(pm_single_complex)
       write(buffer,*) v%data%c(v%offset)
    case(pm_double_complex)
       write(buffer,*) '(',real(v%data%dc(v%offset))
       call append('_PM__D,')
       write(buffer(len_trim(buffer)+1:),*) imag(v%data%dc(v%offset))
       call append('_PM__D)')
    case(pm_logical)
       if(v%data%l(v%offset)) then
          buffer='.TRUE.'
       else
          buffer='.FALSE.'
       endif
    case(pm_string)
       if(g%n+20>ftn_max_line) call out_line_break(g)
       n=pm_fast_esize(v)
       g%linebuffer(g%n+1:g%n+12)='PM__STRVAL("'
       g%n=g%n+12
       do i=0,n-1
          if(g%n>ftn_max_line-5) call out_line_break(g)
          g%n=g%n+1
          g%linebuffer(g%n:g%n)=v%data%s(v%offset+i)
       enddo
       g%linebuffer(g%n+1:g%n+2)='")'
       g%n=g%n+2
       return
    case(0:pm_null)
       return
    end select
    buffer=adjustl(buffer)
    n=len_trim(buffer)
    if(g%n+n>ftn_max_line) then
       call out_line_break(g)
    endif
    g%linebuffer(g%n+1:g%n+1)='('
    g%linebuffer(g%n+2:g%n+n+1)=buffer(1:n)
    g%linebuffer(g%n+n+2:g%n+n+2)=')'
    g%n=g%n+n+2
  contains
    include 'fvkind.inc'
    include 'fesize.inc'

    subroutine append(c)
      character(len=*),intent(in):: c
      integer:: m
      m=len_trim(buffer)
      buffer(m+1:m+len(c))=c
    end subroutine append
    
  end subroutine out_const

  !===========================================
  ! Output PM type typ as a Fortran type
  !===========================================
  subroutine out_type(g,typ)
    type(gen_state):: g
    integer,intent(in):: typ
    type(pm_ptr):: tv
    integer:: tno
    tno=pm_typ_strip_to_basic(g%context,typ)
    select case(tno)
    case(pm_int)
       call out_str(g,'INTEGER')
    case(pm_long)
       call out_str(g,'INTEGER(PM__LN)')
    case(pm_longlong)
       call out_str(g,'INTEGER(PM__LLN)')
    case(pm_int8)
       call out_str(g,'INTEGER(PM__I8)')
    case(pm_int16)
       call out_str(g,'INTEGER(PM__I16)')
    case(pm_int32)
       call out_str(g,'INTEGER(PM__I32)')
    case(pm_int64)
       call out_str(g,'INTEGER(PM__I64)')
    case(pm_single)
       call out_str(g,'REAL')
    case(pm_double)
       call out_str(g,'REAL(PM__D)')
    case(pm_single_complex)
       call out_str(g,'COMPLEX')
    case(pm_double_complex)
       call out_str(g,'COMPLEX(PM__D)')
    case(pm_logical)
       call out_str(g,'LOGICAL')
    case(pm_string)
       call out_str(g,'CHARACTER(LEN=1)')
    case(pm_string_type)
       call out_str(g,'TYPE(PM__STR)')
    case(pm_pointer,pm_poly_type)
       call out_str(g,'TYPE(PM__POLY)')
    case default
       tv=pm_typ_vect(g%context,tno)
       if(pm_tv_kind(tv)==pm_typ_is_poly) then
          call out_str(g,'TYPE(PM__POLY)')
       else
          call out_str(g,'TYPE(PM__T')
          call out_idx(g,tno)
          call out_char(g,')')
       endif
    end select
  end subroutine out_type

  !===========================================
  ! Output a kind (integer enum) associated with
  ! basic type tno
  !===========================================
  subroutine out_kind(g,tno)
    type(gen_state):: g
    integer,intent(in):: tno
    select case(tno)
    case(pm_int)
       call out_str(g,'PM__INT')
    case(pm_long)
       call out_str(g,'PM__LONG')
    case(pm_longlong)
       call out_str(g,'PM__LONGLONG')
    case(pm_int8)
       call out_str(g,'PM__INT8')
    case(pm_int16)
       call out_str(g,'PM__INT16')
    case(pm_int32)
       call out_str(g,'PM__INT32')
    case(pm_int64)
       call out_str(g,'PM__INT64')
    case(pm_single)
       call out_str(g,'PM__SINGLE')
    case(pm_double)
       call out_str(g,'PM__DOUBLE')
    case(pm_single_complex)
       call out_str(g,'PM__SINGLE_COMPLEX')
    case(pm_double_complex)
       call out_str(g,'PM__DOUBLE_COMPLEX')
    case(pm_logical)
       call out_str(g,'PM__LOGICAL')
    end select
  end subroutine out_kind

  !===========================================
  ! Output a standard suffix associated with
  ! basic type tno
  !===========================================
  subroutine out_suffix(g,tno)
    type(gen_state):: g
    integer,intent(in):: tno
    select case(tno)
    case(pm_int)
       call out_str(g,'I')
    case(pm_long)
       call out_str(g,'LN')
    case(pm_longlong)
       call out_str(g,'LLN')
    case(pm_int8)
       call out_str(g,'I8')
    case(pm_int16)
       call out_str(g,'I16')
    case(pm_int32)
       call out_str(g,'I32')
    case(pm_int64)
       call out_str(g,'I64')
    case(pm_single)
       call out_str(g,'R')
    case(pm_double)
       call out_str(g,'D')
    case(pm_single_complex)
       call out_str(g,'C')
    case(pm_double_complex)
       call out_str(g,'DC')
    case(pm_logical)
       call out_str(g,'L')
    end select
  end subroutine out_suffix

  !===========================================
  ! Output code to place the MPI type that
  ! corresponds to tno in JBASE
  !==========================================
  subroutine out_get_mpi_base_type(g,tno)
    type(gen_state):: g
    integer,intent(in):: tno
    integer:: j
    call out_str(g,'JBASE=')
    select case(tno)
    case(pm_int)
       call out_line(g,'MPI_INTEGER')
    case(pm_long)
       call out_line(g,'MPI_AINT')
    case(pm_longlong)
       call out_line(g,'MPI_OFFSET')
    case(pm_int8)
       call out_line(g,'MPI_INTEGER1')
    case(pm_int16)
       call out_line(g,'MPI_INTEGER2')
    case(pm_int32)
       call out_line(g,'MPI_INTEGER4')
    case(pm_int64)
       call out_line(g,'MPI_INTEGER8')
    case(pm_single)
       call out_line(g,'MPI_REAL')
    case(pm_double)
       call out_line(g,'MPI_DOUBLE_PRECISION')
    case(pm_single_complex)
       call out_line(g,'MPI_COMPLEX')
    case(pm_double_complex)
       call out_line(g,'MPI_DOUBLE_COMPLEX')
    case default
       j=add_mpi_type(g,tno)
       call out_simple(g,'PM__MPI_TYPES($N)',n=j)
    end select
  end subroutine out_get_mpi_base_type

  !=============================================
  ! Add to the set of mpi composite types
  ! that will need to be generated
  !==============================================
  recursive function add_mpi_type(g,typ) result(j)
    type(gen_state):: g
    integer,intent(in):: typ
    integer:: j
    type(pm_ptr):: tv
    integer:: i,tno
    if(debug_g) then
       write(*,*) 'ADD MPI TYPE',trim(pm_typ_as_string(g%context,typ))
    endif
    tno=pm_typ_strip_to_basic(g%context,typ)
    if(tno<=pm_string) return
    j=check_set(tno)
    if(j>0) return
    j=add_to_root_set(tno)
    tv=pm_typ_vect(g%context,tno)
    select case(pm_tv_kind(tv))
    case(pm_typ_is_struct,pm_typ_is_rec)
       do i=1,pm_tv_numargs(tv)
          j=add_mpi_type(g,pm_tv_arg(tv,i))
       enddo
       j=add_to_set(tno)
    case(pm_typ_is_poly)
       call pm_panic('add_mpi_type: poly type')
    case(pm_typ_is_array)
       if(pm_tv_arg(tv,3)==pm_long) then
          call pm_panic('add_mpi_type: var length array')
       else
          j=add_mpi_type(g,pm_tv_arg(tv,1))
          j=add_mpi_type(g,pm_tv_arg(tv,2))
          j=add_to_set(tno)
       endif
    case default
       write(*,*) 'Type',pm_typ_as_string(g%context,tno),' kind ',pm_tv_kind(tv)
       call pm_panic('add_mpi_type')
       j=0
    end select
  contains
    
    function check_set(tno) result(j)
      integer,intent(in):: tno
      integer:: j
      integer:: key(1)
      key(1)=tno
      j=pm_ivect_lookup(g%context,g%mpi_types,key,1)
    end function check_set
    
    function add_to_set(tno) result(j)
      integer,intent(in):: tno
      integer:: j,key(1)
      key(1)=tno
      j=pm_iset_add(g%context,g%mpi_types,key,1)
    end function add_to_set

    function check_root_set(tno) result(j)
      integer,intent(in):: tno
      integer:: j
      integer:: key(1)
      key(1)=tno
      j=pm_ivect_lookup(g%context,g%mpi_root_types,key,1)
    end function check_root_set
    
    function add_to_root_set(tno) result(j)
      integer,intent(in):: tno
      integer:: j,key(1)
      key(1)=tno
      j=pm_iset_add(g%context,g%mpi_root_types,key,1)
    end function add_to_root_set
    
  end function  add_mpi_type

  !=========================================
  ! Output given character followed by an
  ! integer index
  !=========================================
  subroutine out_char_idx(g,ltr,idx)
    type(gen_state):: g
    integer,intent(in):: idx
    character,intent(in):: ltr
    integer:: i,j
    if(idx<0) call pm_panic('out_char_idx')
    if(g%n>ftn_max_line-ftn_max_name) &
         call out_line_break(g)
    g%n=g%n+1
    g%linebuffer(g%n:g%n)=ltr
    i=idx
    j=10
    do while(j<=i)
       j=j*10
    end do
    j=j/10
    do while(j>0)
       g%n=g%n+1
       g%linebuffer(g%n:g%n)=&
            achar(iachar('0')+i/j)
       i=mod(i,j)
       j=j/10
    enddo
  end subroutine out_char_idx

  !=========================================
  ! Output integer index
  !=========================================
  subroutine out_idx(g,idx)
    type(gen_state):: g
    integer,intent(in):: idx
    integer:: i,j
    if(g%n>ftn_max_line-ftn_max_name) &
         call out_line_break(g)
    i=idx
    j=10
    do while(j<=i)
       j=j*10
    end do
    j=j/10
    do while(j>0)
       g%n=g%n+1
       g%linebuffer(g%n:g%n)=&
            achar(iachar('0')+i/j)
       i=mod(i,j)
       j=j/10
    enddo
  end subroutine out_idx

  !=========================================
  ! Output long integer number
  !=========================================
  subroutine out_long(g,idx)
    type(gen_state):: g
    integer(pm_ln),intent(in):: idx
    integer(pm_ln):: i,j
    if(g%n>ftn_max_line-ftn_max_name) &
         call out_line_break(g)
    i=idx
    j=10
    do while(j<=i)
       j=j*10
    end do
    j=j/10
    do while(j>0)
       g%n=g%n+1
       g%linebuffer(g%n:g%n)=&
            achar(iachar('0')+i/j)
       i=mod(i,j)
       j=j/10
    enddo
    g%linebuffer(g%n+1:g%n+7)='_PM__LN'
    g%n=g%n+7
  end subroutine out_long

  !=========================================
  ! Output given string as a complete line
  !=========================================
  subroutine out_line(g,str)
    type(gen_state):: g
    character(len=*):: str
    call out_str(g,str)
    call out_new_line(g)
  end subroutine out_line

  !=========================================
  ! Output comment line with text str
  !=========================================
  subroutine out_comment_line(g,str)
    type(gen_state):: g
    character(len=*):: str
    call out_new_line(g)
    call out_char(g,'!')
    call out_char(g,' ')
    if(len(str)>ftn_max_line-5) then
       call out_str(g,str(1:ftn_max_line-5))
    else
       call out_str(g,str)
    endif
    call out_new_line(g)
  end subroutine out_comment_line

  !==================================
  ! Output a given string
  !==================================
  subroutine out_str(g,str)
    type(gen_state):: g
    character(len=*):: str
    integer:: m
    m=len(str)
    if(g%n+m>ftn_max_line) call out_line_break(g)
    if(g%n+m>ftn_max_line) call pm_panic('line too long')
    g%linebuffer(g%n+1:g%n+m)=str
    g%n=g%n+m
  end subroutine out_str
  

  !=========================
  ! Output given character
  !=========================
  subroutine out_char(g,str)
    type(gen_state):: g
    character(len=1):: str
    if(g%n+1>ftn_max_line) call out_line_break(g)
    g%n=g%n+1
    g%linebuffer(g%n:g%n)=str
  end subroutine out_char

  !===============================================
  ! Output comma providing last character was
  ! not a comma or open parenthesis
  !================================================
  subroutine out_comma(g)
    type(gen_state):: g
    if(g%linebuffer(g%n:g%n)/=','&
         .and.g%linebuffer(g%n:g%n)/='(') call out_char(g,',')
  end subroutine out_comma

  !===============================================
  ! Output close brackets - removing any trailing
  ! comma
  !================================================
  subroutine out_close(g)
    type(gen_state):: g
    if(g%linebuffer(g%n:g%n)/=',') then
       g%n=g%n+1
       if(g%n>ftn_max_line) call out_line_break(g)
    endif
    g%linebuffer(g%n:g%n)=')'
  end subroutine out_close

  !==============================
  ! Start a new line
  !==============================
  subroutine out_new_line(g)
    type(gen_state):: g
    g%line_breaks=0
    write(g%outunit,'(A)') g%linebuffer(1:g%n)
    g%linebuffer(1:2)='  '
    g%n=2
  end subroutine out_new_line

  !==================================
  ! Start a new continuation line
  !==================================
  subroutine out_line_break(g)
    type(gen_state):: g
    g%line_breaks=g%line_breaks+1
    if(g%line_breaks>=pm_opts%ftn_lines) then
       call pm_panic('Program too complex - generated Fortran subexpression too long')
    endif
    write(g%outunit,'(A,"&")') g%linebuffer(1:g%n)
    g%n=1
    g%linebuffer(1:1)='&'
  end subroutine out_line_break

  !====================================================
  ! Output the start of a line with no starting indent
  !====================================================
  subroutine out_str_noindent(g,str)
    type(gen_state):: g
    character(len=*):: str
    g%linebuffer=str
    g%n=len_trim(g%linebuffer)
  end subroutine out_str_noindent

  !======================================
  ! Output a line with no starting indent
  !======================================
  subroutine out_line_noindent(g,str)
    type(gen_state):: g
    character(len=*):: str
    g%line_breaks=0
    write(g%outunit,'(A)') str
    g%linebuffer(1:1)=' '
    g%n=1
  end subroutine out_line_noindent


  !=========================================================
  ! Output variable name for the given index
  !=========================================================
  subroutine out_var_name_at_index(g,idx)
    type(gen_state):: g
    integer,intent(in):: idx
    integer:: i
    character(len=ftn_max_name-8):: name
    if(g%vardata(idx)%index/=0) then
       i=abs(g%vardata(idx)%index)
    else
       i=idx
    endif
    call out_check_name_has_space(g)
    call out_char_idx(g,'X',i)
    if(pm_opts%ftn_name_vars.or.&
         pm_opts%ftn_name_params.and.&
         iand(g%vardata(i)%flags,v_is_param)/=0) then
       call out_ftn_name(g,g%vardata(i)%name)
    endif
  end subroutine out_var_name_at_index

  !=========================================================
  ! Output Fortran name contribution for a PM name
  !=========================================================
  subroutine out_ftn_name(g,name)
    type(gen_state):: g
    integer,intent(in):: name
    integer:: n
    character(len=ftn_max_name-8):: name_str
    n=pm_name_stem(g%context,name)
    if(n>num_sym) then
       name_str=pm_name_as_string(g%context,n)
       call out_char(g,'_')
       call out_str(g,trim(name_str))
    endif
  end subroutine  out_ftn_name

  !=========================================================
  ! Check if there is space on the current line for a name
  ! - if not, start new continuation line
  !=========================================================
  subroutine out_check_name_has_space(g)
    type(gen_state):: g
    if(g%n+ftn_max_name>ftn_max_line) call out_line_break(g)
  end subroutine out_check_name_has_space

  !**********************************************************
  ! SERVICE ROUTINES (VARIABLES)
  !**********************************************************

  !========================================================
  ! Get kind of variable v
  !========================================================
  function g_kind(g,n) result(kind)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n
    integer:: kind
    integer:: info
    info=g%vars(abs(n))
    kind=iand(info,cvar_flag_mask)
  end function g_kind

  !========================================================
  ! Get first value associated with variable v
  !========================================================
  function g_v1(g,n) result(v1)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n
    integer:: v1
    integer:: info
    info=g%vars(abs(n))
    v1=info/cvar_flag_mult
  end function g_v1

  !========================================================
  ! Get second value associated with variable v
  !========================================================
  function g_v2(g,n) result(v2)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n
    integer:: v2
    integer:: info
    info=g%vars(abs(n)+1)
    v2=info/cvar_flag_mult
  end function g_v2


  !========================================================
  ! Set second value associated with variable v
  !========================================================
  subroutine g_set_v2(g,n,v)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n,v
    g%vars(abs(n)+1)=v*cvar_flag_mult
  end subroutine g_set_v2

  !========================================================
  ! Get type associated with variable v
  !========================================================
  function g_type(g,n) result(typ)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n
    integer:: typ
    integer:: info
    info=g%vars(abs(n)+2)
    typ=info/cvar_flag_mult
  end function g_type

  !========================================================
  ! Get pointer #i associated with variable v
  !========================================================
  function g_ptr(g,n,i) result(v)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n,i
    integer:: v
    if(pm_debug_checks) then
       if(g_kind(g,n)/=v_is_group) call pm_panic('g_ptr - kind')
       if(g_v1(g,n)<i) then
          write(*,*) '#',g_v1(g,n),i,g_v2(g,n)
          call pm_panic('g_ptr - n')
       endif
    endif
    v=g%vars(abs(n)+i+2)/cvar_flag_mult
  end function g_ptr


  !========================================================
  ! Is variable at given index a vector in any par context?
  !========================================================
  function g_var_at_index_is_a_vect(g,i) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in):: i
    logical:: ok
    integer:: flags,gflags
    flags=g%vardata(i)%flags
    gflags=g%vardata(i)%gflags
    ok=(g%vardata(i)%state==var_state_crossing.and.&
         iand(flags,v_is_par)==0.or.&
         iand(gflags,var_is_comm_op_par)/=0).and.&
         iand(flags,v_is_shared)==0.or.&
         iand(flags,v_is_vect)/=0.or.&
         iand(flags,v_is_in_dref+v_is_shared)==v_is_in_dref
  end function g_var_at_index_is_a_vect


  !===============================================
  ! Is variable a vector in any par. context?
  !===============================================
  function g_is_a_vect(g,n) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n
    logical:: ok
    integer:: i
    i=g%varindex(abs(n))
    if(i==0) then
       ok=g_kind(g,n)==v_is_chan_vect
    else
       ok=g_var_at_index_is_a_vect(g,i)
    endif
  end function g_is_a_vect

  !==============================================
  ! Is variable a vector in current par. context?
  !==============================================
  function g_is_vect(g,n) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n
    logical:: ok
    integer:: i
    i=g%varindex(abs(n))
    if(i==0) then
       ok=g_kind(g,n)==v_is_chan_vect
    else
       ok=g_is_a_vect(g,n).and.&
            (g%vardata(i)%lthis==g%lthis.or.&
            iand(g%vardata(i)%flags,v_is_vect)/=0.or.&
            iand(g%vardata(i)%flags,v_is_in_dref+v_is_shared)==v_is_in_dref)
    endif
  end function g_is_vect

  !=========================================================
  ! Get index of actual Fortran variable associated with
  ! a variable. Return 0 when variable is not directly
  ! associated with Fortran variable (group, element, etc.)
  !=========================================================
  function g_index(g,n) result(m)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n
    integer::m
    integer:: i
    m=g%varindex(abs(n))
  end function g_index

  !=============================================
  ! Parallel nesting depth of a variable
  !=============================================
  function g_lthis(g,n) result(lthis)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n
    integer:: lthis
    integer:: i
    i=g%varindex(abs(n))
    if(i==0) then
       lthis=get_lthis(abs(n))
       if(lthis<0) then
          call pm_panic('g_lthis')
       endif
    else
       lthis=g%vardata(i)%lthis
    endif
  contains
    recursive function get_lthis(n) result(lthis)
      integer,intent(in)::n
      integer:: lthis
      integer:: i
      select case(g_kind(g,n))
      case(v_is_alias,v_is_elem,v_is_chan_vect,&
           v_is_unit_elem,v_is_vect_wrapped)
         lthis=get_lthis(g_v1(g,n))
      case(v_is_sub,v_is_vsub)
         lthis=max(get_lthis(g_v1(g,n)),get_lthis(g_v2(g,n)))
      case(v_is_group)
         lthis=-1
         do i=1,g_v1(g,n)
            lthis=max(lthis,get_lthis(g_ptr(g,n,i)))
         enddo
      case(v_is_basic)
         i=g%varindex(abs(n))
         lthis=g%vardata(i)%lthis
      case default
         lthis=-1
      end select
    end function get_lthis
  end function g_lthis

  !=============================================
  ! Does variable v have all shared elements
  !=============================================
  recursive function g_is_shared(g,v) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in)::v
    logical:: ok
    integer:: i,n
    n=abs(v)
    select case(g_kind(g,n))
    case(v_is_alias,v_is_elem,v_is_chan_vect,&
         v_is_unit_elem,v_is_vect_wrapped)
       ok=g_is_shared(g,g_v1(g,n))
    case(v_is_sub,v_is_vsub)
       ok=g_is_shared(g,g_v1(g,n)).and.g_is_shared(g,g_v2(g,n))
    case(v_is_group)
       ok=.true.
       do i=1,g_v1(g,n)
          ok=ok.and.g_is_shared(g,g_ptr(g,n,i))
       enddo
    case(v_is_basic)
       ok=iand(g_v2(g,n),v_is_shared)/=0
    case default
       ok=.false.
    end select
  end function g_is_shared
  
  !========================================================
  ! Is variable set but never used?
  !========================================================
  function g_var_is_dead(g,n) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n
    logical::ok
    integer:: i
    ok=.false.
    return
    i=g%varindex(abs(n))
    if(i==0) then
       ok=.false.
    else
       ok=g%vardata(i)%start==g%vardata(i)%finish.and.&
            iand(g%vardata(i)%flags,v_is_result)==0.and.&
            g%vardata(i)%outer_lthis==g%vardata(i)%lthis
       if(ok) write(*,*) '>>>>',i,g%vardata(i)%start,g%vardata(i)%finish
    endif
  end function g_var_is_dead

  !========================================================
  ! Does variable share same Fortran variable with others?
  !========================================================
  function g_var_is_merged(g,n) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n
    logical::ok
    integer:: i
    i=g%varindex(abs(n))
    if(i==0) then
       ok=.false.
    else
       ok=g%vardata(i)%index/=0
    endif
  end function g_var_is_merged

  !========================================================
  ! Do two variables share same Fortran variable
  ! with each other?
  !========================================================
  function g_vars_are_merged(g,n,m) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in):: n,m
    logical::ok
    integer:: i,j
    i=g%varindex(abs(n))
    j=g%varindex(abs(m))
    if(i==0.or.j==0) then
       ok=.false.
    else
       ok=g%vardata(i)%index/=0.and.&
            abs(g%vardata(i)%index)==abs(g%vardata(j)%index)
    endif
  end function g_vars_are_merged

  !=====================================
  ! Return current parallel nesting level
  ! as a string
  !=======================================
  function g_this_n_str(g) result(str)
    type(gen_state):: g
    character(len=6):: str
    str(1:1)='N'
    write(str(2:),'(I5)') g%lthis
    str(2:)=adjustl(str(2:))
  end function g_this_n_str

  !==============================================
  ! Test if given flags are set for variable v
  !==============================================
  function g_flags_set(g,v,flags) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v,flags
    logical:: ok
    integer:: i
    if(v/=0) then
       i=g%varindex(abs(v))
       if(i==0) then
          ok=.false.
       else
          ok=iand(g%vardata(i)%flags,flags)==flags
       endif
    else
       ok=.false.
    endif
  end function g_flags_set

  !==============================================
  ! Test if given flags are clear for variable v
  !==============================================
  function g_flags_clear(g,v,flags) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v,flags
    logical:: ok
    integer:: i
    if(v/=0) then
       i=g%varindex(abs(v))
       ok=iand(g%vardata(i)%flags,flags)==0
    else
       ok=.false.
    endif
  end function g_flags_clear

  !==============================================
  ! Set given flags for variable v
  !==============================================
  function g_gflags_set(g,v,flags) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v,flags
    logical:: ok
    integer:: i
    if(v/=0) then
       i=g%varindex(abs(v))
       ok=iand(g%vardata(i)%gflags,flags)==flags
    else
       ok=.false.
    endif
  end function g_gflags_set

  !==============================================
  ! Clear given flags for variable v
  !==============================================
  function g_gflags_clear(g,v,flags) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v,flags
    logical:: ok
    integer:: i
    if(v/=0) then
       i=g%varindex(abs(v))
       ok=iand(g%vardata(i)%gflags,flags)==0
    else
       ok=.false.
    endif
  end function g_gflags_clear

  !======================================
  ! Set given flags for variable v
  !======================================
  subroutine g_set_gflags(g,v,flags)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v,flags
    integer:: i
    if(v/=0) then
       i=g%varindex(abs(v))
       g%vardata(i)%gflags=ior(g%vardata(i)%gflags,flags)
    endif
  end subroutine g_set_gflags

  !======================================
  ! Clear given flags for variable v
  !======================================
  subroutine g_clear_gflags(g,v,flags)
    type(gen_state),intent(inout):: g
    integer,intent(in):: v,flags
    integer:: i
    if(v/=0) then
       i=g%varindex(abs(v))
       g%vardata(i)%gflags=iand(g%vardata(i)%gflags,not(flags))
    endif
  end subroutine g_clear_gflags

  !=================================================
  ! Return set of types associated with polymorphic
  ! type typ
  !=================================================
  function g_check_poly(g,typ) result(ptr)
    type(gen_state),intent(inout):: g
    integer,intent(in):: typ
    type(pm_ptr):: ptr
    integer(pm_ln):: j
    integer,dimension(1):: key
    key(1)=typ
    j=pm_ivect_lookup(g%context,g%poly_cache,key,1)
    if(j==0) then
       ptr=pm_null_obj
    else
       ptr=pm_dict_val(g%context,g%poly_cache,j)
    endif
  end function g_check_poly

  !===============================================
  ! Is tno a complex type containing arrays or
  ! polymorphic elements?
  !===============================================
  function g_is_complex_type(g,tno) result(ok)
    type(gen_state),intent(inout):: g
    integer,intent(in):: tno
    logical:: ok
    ok=tno==pm_pointer.or.&
         iand(pm_typ_flags(g%context,tno),pm_typ_has_array+pm_typ_has_poly)/=0
  end function g_is_complex_type

  ! =========================================
  ! Placefiller - not needed for compiler
  ! =========================================
  subroutine init_par(context)
    type(pm_context),pointer:: context
  end subroutine init_par

  !===============================================
  ! Placefiller - not needed for compiler
  !================================================
  subroutine finalise_par(context)
    type(pm_context),pointer:: context
  end subroutine finalise_par
  
end module pm_backend
