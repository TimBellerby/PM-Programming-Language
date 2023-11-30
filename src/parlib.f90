!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2021
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

!==================================================================
! The following routines provide support for parallel operations
!==================================================================

module pm_parlib
  use pm_kinds
  use pm_memory
  use pm_hash
  use pm_lib
  use pm_array
  use mpi
  implicit none

  logical,private,parameter:: debug_par=.false.
  logical,private,parameter:: debug_mess=.false.

  ! Stack entry with details of (possibly nested) parallel statement
  type par_info_frame
     integer:: this_node          ! Rank in communicating group
     integer:: this_nnode         ! Num procs in communicating group 
     integer:: this_comm          ! MPI communicator for comm. group.
     integer:: root_node          ! Root node in shared group (as rank in this_comm)
     integer:: shared_node        ! Rank in shared group
                                  ! (running the same invocation)
     integer:: shared_nnode       ! Number of processes in shared group
     integer:: shared_comm        ! MPI communicator for shared group
     logical:: is_shared          ! Process is part of group, some of which share
  end type par_info_frame

  ! Stack of par_info_frame structures
  integer,parameter:: max_par_depth=128
  type(par_info_frame),dimension(max_par_depth):: par_frame
  integer:: par_depth

  ! Maximum message sizes
  integer,parameter:: max_message_size=(huge(1)/2)+1
  integer,parameter:: exchange_block= 16*1024 !*1024
  integer,parameter:: pm_node_block = 64

  ! Status of an MPI node
  integer,parameter:: pm_node_finished=0
  integer,parameter:: pm_node_running=1
  integer,parameter:: pm_node_error=2
  integer:: conc_depth
  integer:: sys_node,sys_nnode   ! Rank in MPI_COMM_WORLD

  ! MPI types for PM types
  integer,dimension(pm_int:pm_string):: typ_for_pm,large_typ_for_pm
  integer(pm_ln),dimension(pm_int:pm_string):: large_typ_size 

  ! Stacks of pending messages
  integer,parameter:: max_messages=1024
  integer,dimension(:),allocatable:: message_stack
  integer:: message_top,message_base
  integer:: message_stack_size

  
  ! Buffers for small headers
  type hdr_buf
     type(hdr_buf),pointer:: next
     integer(pm_ln),dimension(3):: hdr  
  end type hdr_buf
  type(hdr_buf),pointer:: hdr_bufs

  ! Buffer list to protect from gc
  type(pm_ptr),private,target:: buffer_list
  
  ! Data tags
  integer,parameter:: req_tag=1
  integer,parameter:: data_tag=2
  integer,parameter:: extra_tag=3
  integer,parameter:: hdr_tag=4
  integer,parameter:: extra_req_tag=5
  integer,parameter:: extra_req_extra_tag=6

  ! Message exchange
  integer:: pm_comm
  integer,parameter:: pm_comm_tag=0
  integer,parameter:: pm_comm_mess_len=200
  type(pm_ptr),private,target:: print_first,print_last
  type(pm_reg),private,pointer:: q_reg
  
contains

  ! Initialise PM MPI subsystem
  subroutine init_par(context)
    type(pm_context),pointer:: context
    integer:: error
    type(pm_ptr):: v
    call mpi_init(error)
    if(error/=MPI_SUCCESS) &
         call pm_panic('Cannot init mpi')
    par_depth=1
    call mpi_comm_size(MPI_COMM_WORLD,sys_nnode,error)
    call mpi_comm_rank(MPI_COMM_WORLD,sys_node,error)
    
    ! debug_mess=sys_node==0
    par_frame(par_depth)%this_nnode=sys_nnode
    par_frame(par_depth)%this_node=sys_node
    par_frame(par_depth)%this_node=sys_node
    par_frame(par_depth)%this_comm=MPI_COMM_WORLD
    par_frame(par_depth)%shared_comm=MPI_COMM_WORLD
    par_frame(par_depth)%shared_node=par_frame(par_depth)%this_node
    par_frame(par_depth)%shared_nnode=par_frame(par_depth)%this_nnode
    conc_depth=0

    ! Initialise MPI types array
    typ_for_pm(pm_int)=MPI_INTEGER
    typ_for_pm(pm_long)=MPI_AINT
    typ_for_pm(pm_longlong)=MPI_OFFSET
    typ_for_pm(pm_int8)=MPI_INTEGER1
    typ_for_pm(pm_int16)=MPI_INTEGER2
    typ_for_pm(pm_int32)=MPI_INTEGER4
    typ_for_pm(pm_int64)=MPI_INTEGER8
    typ_for_pm(pm_single)=MPI_REAL
    typ_for_pm(pm_double)=MPI_DOUBLE
    typ_for_pm(pm_single_complex)=MPI_COMPLEX
    typ_for_pm(pm_double_complex)=MPI_DOUBLE_COMPLEX
    typ_for_pm(pm_logical)=MPI_LOGICAL

    large_typ_for_pm = MPI_DATATYPE_NULL
    large_typ_size = -1

    ! Register lists/buffers with GC
    q_reg=>pm_register(context,'parlib',print_first,print_last,buffer_list)
    
    ! Initialise buffer lists
    message_top=1
    message_base=1
    message_stack_size=max_messages
    allocate(message_stack(max_messages))
    message_stack(message_base)=1
    nullify(hdr_bufs)
    buffer_list=pm_null_obj
    
    ! Initialise error communicator
    call mpi_comm_dup(MPI_COMM_WORLD,pm_comm,error)

    ! Initialise print queue
    print_first=pm_null_obj
    print_last=pm_null_obj
  end subroutine init_par

  ! Finalise MPI subsystem
  subroutine finalise_par(context)
    type(pm_context),pointer:: context
    integer:: error
    call mpi_finalize(error)
    if(error/=MPI_SUCCESS) &
         call pm_panic('Cannot finalise mpi')
    call pm_delete_register(context,q_reg)
  end subroutine finalise_par

  ! Get cartesian dimensions 
  subroutine get_dims(shared_nnode,ndims,dims_inout)
    integer,intent(in):: ndims
    integer(pm_ln),dimension(ndims),intent(inout):: dims_inout
    integer,intent(in):: shared_nnode
    integer:: nnodes,error,i,j,nrequired
    integer,dimension(ndims):: dims
    logical:: has_zero
    do i=1,ndims
       if(dims_inout(i)>huge(1)) then
          do j=1,ndims
             if(dims_inout(i)<=0) dims_inout(i)=1
          enddo
          return
       else
          dims(i)=max(0,dims_inout(i))
       endif
    enddo
    if(shared_nnode>1) then
       nrequired=1
       has_zero=.true.
       do i=1,ndims
          if(dims(i)/=0) then
             nrequired=nrequired*dims(i)
          else
             has_zero=.true.
          endif
       enddo
       if(nrequired>=shared_nnode) then
          if(has_zero) then
             do i=1,ndims
                if(dims(i)==0) dims(i)=1
             enddo
          endif
       elseif(has_zero) then
          nnodes=(shared_nnode/nrequired)*nrequired
          call mpi_dims_create(nnodes,ndims,dims,error)
          if(error/=MPI_SUCCESS) then
             call pm_panic('cannot create dims')
          endif
       endif
    else
       dims=1
    endif
    dims_inout=dims
  end subroutine get_dims

  ! Create a new communicating group using MPI split
  subroutine push_node_split(context,root) 
    type(pm_context),pointer:: context
    integer,intent(in):: root
    integer:: error,newcomm
    integer:: this_comm
    this_comm=par_frame(par_depth)%shared_comm
    call mpi_comm_split(this_comm,root,par_frame(par_depth)%this_node,&
         newcomm,error)
    par_depth=par_depth+1
    if(par_depth>max_par_depth) call pm_panic('too many nested for/do/find')

    par_frame(par_depth)%this_node=par_frame(par_depth-1)%shared_node
    par_frame(par_depth)%root_node=root
    par_frame(par_depth)%this_nnode=par_frame(par_depth-1)%shared_nnode
    par_frame(par_depth)%this_comm=par_frame(par_depth-1)%shared_comm
    par_frame(par_depth)%is_shared=.true.
    call mpi_comm_size(newcomm,par_frame(par_depth)%shared_nnode,error)
    call mpi_comm_rank(newcomm,par_frame(par_depth)%shared_node,error)
    par_frame(par_depth)%shared_comm=newcomm
    if(debug_par) then
       write(*,*) 'split:',sys_node,par_frame(par_depth)%this_comm,&
            par_frame(par_depth)%shared_comm,root
    endif
  contains
    include 'fnewnc.inc'
  end subroutine   push_node_split

  ! New frame used former shared communicator
  subroutine push_node_distr(context)
    type(pm_context),pointer:: context
    par_depth=par_depth+1
    par_frame(par_depth)%this_node=par_frame(par_depth-1)%shared_node
    par_frame(par_depth)%root_node=par_frame(par_depth-1)%shared_node   
    par_frame(par_depth)%this_nnode=par_frame(par_depth-1)%shared_nnode
    par_frame(par_depth)%this_comm=par_frame(par_depth-1)%shared_comm
    par_frame(par_depth)%shared_comm=MPI_COMM_SELF
    par_frame(par_depth)%shared_nnode=1
    par_frame(par_depth)%shared_node=0
    par_frame(par_depth)%is_shared=.false.
    if(debug_par) then
       write(*,*) 'push distr: ',sys_node,par_frame(par_depth)%this_comm,&
            par_frame(par_depth)%shared_comm
    endif
  end subroutine push_node_distr
    
  !  Create a new communicating group with grid topology
  subroutine push_node_grid(context,periods,ndims,dims_in)
    type(pm_context),pointer:: context
    integer,intent(in):: ndims
    logical,dimension(ndims),intent(in):: periods
    integer(pm_ln),dimension(ndims),intent(in):: dims_in
    integer,dimension(ndims):: dims
    type(pm_ptr):: ptr
    integer:: error,newcomm
    integer:: i,shared_nnode
    ! Convert from PM (=FORTRAN) order to MPI_CART (=C) order
    do i=1,ndims
       dims(i)=dims_in(ndims-i+1)
    enddo
    shared_nnode=par_frame(par_depth)%shared_nnode
    if(shared_nnode>1) then
       call mpi_cart_create(par_frame(par_depth)%shared_comm,ndims,dims,&
            periods,.true.,newcomm,error)
       if(error/=MPI_SUCCESS) &
            call pm_panic('cannot create cart comm')
       par_depth=par_depth+1
       if(par_depth>max_par_depth) call pm_panic('too many nested for/do/find')
       par_frame(par_depth)%this_comm=newcomm
       call mpi_comm_size(newcomm,par_frame(par_depth)%this_nnode,error)
       call mpi_comm_rank(newcomm,par_frame(par_depth)%this_node,error)
       par_frame(par_depth)%root_node=par_frame(par_depth)%this_node
       par_frame(par_depth)%shared_comm=MPI_COMM_SELF
       par_frame(par_depth)%shared_nnode=1
       par_frame(par_depth)%shared_node=0
       par_frame(par_depth)%is_shared=.false.
       if(debug_par) then
          write(*,*) 'push grid',par_depth,dims(1:ndims)
       endif
    else
       if(debug_par) then
          write(*,*) 'push_grid_conc',par_depth,dims(1:ndims)
       endif
       conc_depth=conc_depth+1
    endif
  contains
    include 'fnewnc.inc'
  end subroutine  push_node_grid
  
  ! Pop communicating group - revert to next outer layer
  subroutine pop_node(context)
    type(pm_context),pointer:: context
    integer:: error,old_shared,old_this
    if(conc_depth>0) then
       conc_depth=conc_depth-1
    else
       old_shared=par_frame(par_depth)%shared_comm
       old_this=par_frame(par_depth)%this_comm
       par_depth=par_depth-1
       if(old_this/=MPI_COMM_SELF.and.&
            par_frame(par_depth)%shared_comm/=old_this) then
          call mpi_comm_free(old_this,error)
       endif
       if(old_shared/=MPI_COMM_SELF) then
          call mpi_comm_free(old_shared,error)
       endif
    endif
  end subroutine pop_node

  ! Push frame above current
  subroutine push_node_back(context)
    type(pm_context),pointer:: context
    par_depth=par_depth+1
    par_frame(par_depth)=par_frame(par_depth-2)
  end subroutine push_node_back  

  ! Pop of push_node_back record (not freeing)
  subroutine pop_off_node(context)
    type(pm_context),pointer:: context
    if(conc_depth>0) then
       conc_depth=conc_depth-1
    else
       par_depth=par_depth-1
    endif
  end subroutine pop_off_node

  ! Get rank of proc #i in shared_comm to rank in this_comm
  function get_shared(i) result(node)
    integer,intent(in):: i
    integer:: node
    integer:: group_s,group_t,error
    integer,dimension(1):: in,out
    call mpi_comm_group(par_frame(par_depth)%shared_comm,group_s,error)
    call mpi_comm_group(par_frame(par_depth)%this_comm,group_t,error)
    in(1)=i
    call mpi_group_translate_ranks(group_s,1,in,group_t,out,error)
    node=out(1)
  end function get_shared

  ! Add a pointer to buffer GC protection list
  ! (list is nulled on message sync/tidy)
  subroutine push_buffer(context,p)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: p
    type(pm_ptr):: q
    q=pm_fast_newnc(context,pm_pointer,2)
    q%data%ptr(q%offset)=buffer_list
    buffer_list=q
    q%data%ptr(q%offset+1)=p
  contains
    include 'fnewnc.inc'
  end subroutine push_buffer
  
  ! Broadcast data (v) from node to all other nodes in current group (in place)
  recursive subroutine broadcast(context,node,v,xcomm,xthis)
    type(pm_context),pointer:: context
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v
    integer,intent(in),optional:: xcomm,xthis
    integer:: comm
    integer:: i,m,tno,errno
    integer(pm_ln):: j,nn
    type(pm_ptr):: avec,w
    if(debug_par) then
       write(*,*) 'BCAST',node,sys_node,par_frame(par_depth)%this_comm,pm_fast_esize(v)+1
    endif
    if(node==MPI_PROC_NULL) return
    if(present(xcomm)) then
       comm=xcomm
    else
       comm=par_frame(par_depth)%this_comm
    endif
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call broadcast(context,node,v%data%ptr(v%offset+pm_array_dom),xcomm,xthis)
       call broadcast(context,node,v%data%ptr(v%offset+pm_array_length),xcomm,xthis)
       w=v%data%ptr(v%offset+pm_array_vect)
       do j=0,pm_fast_esize(w)
          call pm_ptr_assign(context,w,j,&
               broadcast_val(context,node,w%data%ptr(w%offset+j),nn,xcomm,xthis))
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call broadcast(context,node,v%data%ptr(v%offset+i),xcomm,xthis)
       enddo
    case(pm_pointer)
       do j=0,pm_fast_esize(v)
         call pm_ptr_assign(context,v,j,&
               broadcast_val(context,node,v%data%ptr(v%offset+j),nn,xcomm,xthis))
       enddo
    case(pm_int)
       call get_mpi_type(pm_int,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%i(v%offset),m,&
            tno,node,comm,errno)
    case(pm_long)
       call get_mpi_type(pm_long,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%ln(v%offset),m,&
            tno,node,comm,errno)
    case(pm_longlong)
       call get_mpi_type(pm_longlong,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%ln(v%offset),m,&
            tno,node,comm,errno)
    case(pm_int8)
       call get_mpi_type(pm_int8,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%i8(v%offset),m,&
            tno,node,comm,errno)
    case(pm_int16)
       call get_mpi_type(pm_int16,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%i16(v%offset),m,&
            tno,node,comm,errno)
    case(pm_int32)
       call get_mpi_type(pm_int32,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%i32(v%offset),m,&
            tno,node,comm,errno)
    case(pm_int64)
       call get_mpi_type(pm_int64,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%i64(v%offset),m,&
            tno,node,comm,errno)
    case(pm_single)
       call get_mpi_type(pm_single,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%r(v%offset),m,&
            tno,node,comm,errno)
    case(pm_double)
       call get_mpi_type(pm_double,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%d(v%offset),m,&
            tno,node,comm,errno)
    case(pm_single_complex)
       call get_mpi_type(pm_single_complex,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%c(v%offset),m,&
            tno,node,comm,errno)
    case(pm_double_complex)
       call get_mpi_type(pm_double_complex,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%dc(v%offset),m,&
            tno,node,comm,errno)
    case(pm_logical)
       call get_mpi_type(pm_logical,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%l(v%offset),m,&
            tno,node,comm,errno)
    end select
    if(debug_par) then
       write(*,*) 'BCAST DONE'
    endif
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine broadcast

  recursive subroutine broadcast_disp(context,node,v,off,offstart,noff,xcomm)
    type(pm_context),pointer:: context
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln),intent(in):: offstart,noff
    integer,intent(in),optional:: xcomm
    integer:: comm
    integer:: i,tno,errno
    integer(pm_ln):: j,k,nout
    type(pm_ptr):: len,avec
    character,dimension(1):: buffer
    if(present(xcomm)) then
       comm=xcomm
    else
       comm=par_frame(par_depth)%this_comm
    endif
    tno=pm_fast_typeof(v)
    if(debug_mess) write(*,*) 'on',par_frame(par_depth)%this_node,'bcast disp',tno,'from',node
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call broadcast_disp(context,node,v%data%ptr(v%offset+pm_array_dom),&
            off,offstart,noff,xcomm)
       len=v%data%ptr(v%offset+pm_array_length)
       call broadcast_disp(context,node,len,off,offstart,noff,xcomm)
       avec=v%data%ptr(v%offset+pm_array_vect)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call pm_ptr_assign(context,avec,k,broadcast_val(context,&
               node,avec%data%ptr(avec%offset+k),nout,xcomm))
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call broadcast_disp(context,node,v%data%ptr(v%offset+i),off,offstart,noff,xcomm)
       enddo
    case(pm_pointer)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call pm_ptr_assign(context,v,k,broadcast_val(context,node,&
               v%data%ptr(v%offset+k),nout,xcomm))
       enddo
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,offstart,noff,tno)
       call mpi_bcast(v%data%i(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_long)
       call get_mpi_disp_type(pm_long,off,offstart,noff,tno)
       call mpi_bcast(v%data%ln(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_longlong)
       call get_mpi_disp_type(pm_longlong,off,offstart,noff,tno)
       call mpi_bcast(v%data%lln(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_int8)
       call get_mpi_disp_type(pm_int8,off,offstart,noff,tno)
       call mpi_bcast(v%data%i8(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_int16)
       call get_mpi_disp_type(pm_int16,off,offstart,noff,tno)
       call mpi_bcast(v%data%i16(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_int32)
       call get_mpi_disp_type(pm_int32,off,offstart,noff,tno)
       call mpi_bcast(v%data%i32(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_int64)
       call get_mpi_disp_type(pm_int64,off,offstart,noff,tno)
       call mpi_bcast(v%data%i64(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,offstart,noff,tno)
       call mpi_bcast(v%data%r(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,offstart,noff,tno)
       call mpi_bcast(v%data%d(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_single_complex)
       call get_mpi_disp_type(pm_single_complex,off,offstart,noff,tno)
       call mpi_bcast(v%data%c(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_double_complex)
       call get_mpi_disp_type(pm_double_complex,off,offstart,noff,tno)
       call mpi_bcast(v%data%dc(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,offstart,noff,tno)
       call mpi_bcast(v%data%l(v%offset),1,&
            tno,node,comm,errno)
       call mpi_type_free(tno,errno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine broadcast_disp
 
  ! Broadcast data (v) from node to all other nodes in current group
  recursive function broadcast_val(context,node,v,nout,xcomm,xthis) result(ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v
    integer,intent(in),optional:: xcomm,xthis
    integer(pm_ln),intent(inout):: nout
    type(pm_ptr):: ptr
    integer:: comm
    integer:: i,m,tno,errno
    integer(pm_ln),dimension(3):: hdr
    integer(pm_ln):: esize,j,nn
    type(pm_ptr):: avec,w
    type(pm_root),pointer:: root
    logical:: this_node
    
    if(debug_mess) then
       write(*,*) 'BCAST VAL',node,sys_node
    endif
    if(node==MPI_PROC_NULL) return
    if(present(xcomm)) then
       comm=xcomm
    else
       comm=par_frame(par_depth)%this_comm
    endif
    if(present(xthis)) then
       this_node=xthis==node
    else
       this_node=par_frame(par_depth)%this_node==node
    endif
    
    ! Send header: type/size/pm_type
    hdr(1)=pm_fast_typeof(v)
    hdr(2)=pm_fast_esize(v)
    if(pm_fast_vkind(v)==pm_usr) then
       hdr(3)=v%data%ptr(v%offset+1)%offset
    endif
    call mpi_bcast(hdr,3,MPI_AINT,node,comm,errno)
    tno=hdr(1)
    esize=hdr(2)
    nout=esize+1
    select case(tno)
    case(pm_null)
       ptr=pm_null_obj
    case(pm_array_type,pm_const_array_type)
       root=>pm_new_as_root(context,pm_usr,int(pm_array_size,pm_ln))
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)%offset=pm_array_type
       ptr%data%ptr(ptr%offset+1)%offset=hdr(3)
       call pm_ptr_assign(context,ptr,int(pm_array_dom,pm_ln),&
            broadcast_val(context,node,v%data%ptr(v%offset+pm_array_dom),nn,xcomm,xthis))
       call pm_ptr_assign(context,ptr,int(pm_array_length,pm_ln),&
            broadcast_val(context,node,v%data%ptr(v%offset+pm_array_length),nout,xcomm,xthis))
       esize=pm_fast_esize(ptr%data%ptr(ptr%offset+pm_array_length))
       w=pm_assign_new(context,ptr,int(pm_array_offset,pm_ln),&
            pm_long,esize+1,.true.)
       w=v%data%ptr(v%offset+pm_array_vect)
       avec=pm_assign_new(context,ptr,int(pm_array_vect,pm_ln),&
            pm_pointer,esize+1,.true.)
       do j=0,esize
          call pm_ptr_assign(context,avec,j,&
               broadcast_val(context,node,w%data%ptr(w%offset+j),nn,xcomm,xthis))
       enddo
       call pm_delete_root(context,root)
    case(pm_struct_type,pm_rec_type,pm_polyref_type,pm_dref_type,pm_dref_shared_type)
       root=>pm_new_as_root(context,pm_usr,esize+1)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)%offset=tno
       ptr%data%ptr(ptr%offset+1)%offset=hdr(3)
       do j=2,esize
          call pm_ptr_assign(context,ptr,j,&
               broadcast_val(context,node,v%data%ptr(v%offset+j),nn,xcomm,xthis))
       enddo
       call pm_delete_root(context,root)
       
    case(pm_pointer)
       root=>pm_new_as_root(context,pm_pointer,esize+1)
       ptr=root%ptr
       do j=0,esize
          call pm_ptr_assign(context,ptr,j,&
               broadcast_val(context,node,v%data%ptr(v%offset+i),nn,xcomm,xthis))
       enddo
       call pm_delete_root(context,root)
    case(pm_int)
       ptr=pm_new(context,pm_int,esize+1)
       if(this_node) ptr%data%i(ptr%offset:ptr%offset+esize)=&
            v%data%i(v%offset:v%offset+esize)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%i(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_long)
       ptr=pm_new(context,pm_long,esize+1)
       if(this_node) ptr%data%ln(ptr%offset:ptr%offset+esize)=&
            v%data%ln(v%offset:v%offset+esize)
       call get_mpi_type(pm_long,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%ln(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_longlong)
       ptr=pm_new(context,pm_longlong,esize+1)
       if(this_node) ptr%data%lln(ptr%offset:ptr%offset+esize)=&
            v%data%lln(v%offset:v%offset+esize)
       call get_mpi_type(pm_longlong,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%lln(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_int8)
       ptr=pm_new(context,pm_int8,esize+1)
       if(this_node) ptr%data%i8(ptr%offset:ptr%offset+esize)=&
            v%data%i8(v%offset:v%offset+esize)
       call get_mpi_type(pm_int8,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%i8(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_int16)
       ptr=pm_new(context,pm_int16,esize+1)
       if(this_node) ptr%data%i16(ptr%offset:ptr%offset+esize)=&
            v%data%i16(v%offset:v%offset+esize)
       call get_mpi_type(pm_int16,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%i16(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_int32)
       ptr=pm_new(context,pm_int32,esize+1)
       if(this_node) ptr%data%i32(ptr%offset:ptr%offset+esize)=&
            v%data%i32(v%offset:v%offset+esize)
       call get_mpi_type(pm_int32,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%i32(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_int64)
       ptr=pm_new(context,pm_int64,esize+1)
       if(this_node) ptr%data%i64(ptr%offset:ptr%offset+esize)=&
            v%data%i64(v%offset:v%offset+esize)
       call get_mpi_type(pm_int64,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%i64(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_single)
       ptr=pm_new(context,pm_single,esize+1)
       if(this_node) ptr%data%r(ptr%offset:ptr%offset+esize)=&
            v%data%r(v%offset:v%offset+esize)
       call get_mpi_type(pm_single,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%r(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_double)
       ptr=pm_new(context,pm_double,esize+1)
       if(this_node) ptr%data%d(ptr%offset:ptr%offset+esize)=&
            v%data%d(v%offset:v%offset+esize)
       call get_mpi_type(pm_double,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%d(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_single_complex)
       ptr=pm_new(context,pm_single_complex,esize+1)
       if(this_node) ptr%data%c(ptr%offset:ptr%offset+esize)=&
            v%data%r(v%offset:v%offset+esize)
       call get_mpi_type(pm_single_complex,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%c(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_double_complex)
       ptr=pm_new(context,pm_double_complex,esize+1)
       if(this_node) ptr%data%dc(ptr%offset:ptr%offset+esize)=&
            v%data%d(v%offset:v%offset+esize)
       call get_mpi_type(pm_double_complex,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%dc(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_logical)
       ptr=pm_new(context,pm_logical,esize+1)
       if(this_node) ptr%data%l(ptr%offset:ptr%offset+esize)=&
            v%data%l(v%offset:v%offset+esize)
       call get_mpi_type(pm_logical,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%l(ptr%offset),m,&
            tno,node,comm,errno)
    end select
    if(debug_mess) then
       write(*,*) 'BCAST VAL DONE'
    endif
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function  broadcast_val

  ! Broadcast dispersed data v[off] from node to all other nodes in current group
  recursive function broadcast_val_disp(context,off,offstart,noff,node,v,nout) result(ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: node
    integer(pm_ln):: offstart,noff
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln),intent(out):: nout
    type(pm_ptr):: ptr
    integer:: comm
    integer:: m,tno,errno
    integer(pm_ln),dimension(4):: hdr
    integer(pm_ln):: esize,j,k,nn
    type(pm_ptr):: avec,w
    type(pm_root),pointer:: root
    logical:: this_node
    
    if(debug_mess) then
       write(*,*) sys_node,'BCAST VAL DISP>',node,noff
    endif
    if(node==MPI_PROC_NULL) return
    comm=par_frame(par_depth)%this_comm
    this_node=par_frame(par_depth)%this_node==node
    ! Send header: type/size/pm_type
    hdr(1)=pm_fast_typeof(v)
    hdr(2)=pm_fast_esize(v)
    if(pm_fast_vkind(v)==pm_usr) then
       hdr(3)=v%data%ptr(v%offset+1)%offset
    endif
    if(pm_fast_vkind(off)==pm_long) then
       hdr(4)=noff
    else
       avec=off%data%ptr(off%offset+4)
       hdr(4)=avec%data%ln(avec%offset)
    endif
    call mpi_bcast(hdr,4,MPI_AINT,node,comm,errno)
    tno=hdr(1)
    esize=hdr(2)
    nout=hdr(4)
    select case(tno)
    case(pm_null)
       ptr=pm_null_obj
    case(pm_array_type,pm_const_array_type)
       root=>pm_new_as_root(context,pm_usr,int(pm_array_size,pm_ln))
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)%offset=pm_array_type
       ptr%data%ptr(ptr%offset+1)%offset=hdr(3)
       call pm_ptr_assign(context,ptr,int(pm_array_dom,pm_ln),&
            broadcast_val_disp(context,off,offstart,noff,node,&
            v%data%ptr(v%offset+pm_array_dom),nn))
       call pm_ptr_assign(context,ptr,int(pm_array_length,pm_ln),&
            broadcast_val_disp(context,off,offstart,noff,node,&
            v%data%ptr(v%offset+pm_array_length),nn))
       esize=pm_fast_esize(ptr%data%ptr(ptr%offset+pm_array_length))
       w=pm_assign_new(context,ptr,int(pm_array_offset,pm_ln),pm_long,noff,.true.)
       w=v%data%ptr(v%offset+pm_array_vect)
       avec=pm_assign_new(context,ptr,int(pm_array_vect,pm_ln),&
            pm_pointer,nout,.true.)
       do j=offstart,noff-1
          k=merge(off%data%ln(off%offset+k),0_pm_ln,this_node)
          call pm_ptr_assign(context,avec,j,&
               broadcast_val(context,node,w%data%ptr(w%offset+k),nn))
       enddo
       call pm_delete_root(context,root)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       root=>pm_new_as_root(context,pm_usr,esize+1)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)%offset=tno
       ptr%data%ptr(ptr%offset+1)%offset=hdr(3)
       do j=2,esize
          call pm_ptr_assign(context,ptr,j,&
               broadcast_val_disp(context,off,offstart,noff,&
               node,v%data%ptr(v%offset+j),nn))
       enddo
       call pm_delete_root(context,root)
    case(pm_dref_shared_type)
       root=>pm_new_as_root(context,pm_usr,4_pm_ln)
       w=root%ptr
       w%data%ptr(w%offset)%offset=pm_dref_shared_type
       tno=pm_fast_typeof(v%data%ptr(v%offset+2))
       if(tno==pm_dref_type.or.tno==pm_dref_shared_type) then
          w%data%ptr(w%offset+2)=broadcast_val_disp(context,off,offstart,noff,node,&
               v%data%ptr(v%offset+2),nn)
       else
          w%data%ptr(w%offset+2)=&
               vector_from_scalar(context,v%data%ptr(v%offset+2),0_pm_ln,nout-1,.true.)
       endif
       w%data%ptr(w%offset+3)=&
            vector_from_scalar(context,v%data%ptr(v%offset+3),0_pm_ln,nout-1,.true.)
       ptr=w
       call pm_delete_root(context,root)
    case(pm_dref_type)
       root=>pm_new_as_root(context,pm_usr,4_pm_ln)
       w=root%ptr
       w%data%ptr(w%offset)%offset=pm_dref_shared_type
       tno=pm_fast_typeof(v%data%ptr(v%offset+2))
       if(tno==pm_dref_type.or.tno==pm_dref_shared_type) then
          w%data%ptr(w%offset+2)=broadcast_val_disp(context,off,offstart,noff,node,&
               v%data%ptr(v%offset+2),nn)
       else
          w%data%ptr(w%offset+2)=&
               vector_from_scalar(context,v%data%ptr(v%offset+2),0_pm_ln,nout-1,.true.)
       endif
       w%data%ptr(w%offset+3)=broadcast_val_disp(context,off,offstart,noff,node,&
            v%data%ptr(v%offset+3),nn)
       ptr=w
       call pm_delete_root(context,root)
    case(pm_pointer)
       root=>pm_new_as_root(context,pm_pointer,noff)
       ptr=root%ptr
       do j=0,noff-1
          k=merge(off%data%ln(off%offset+offstart+k),0_pm_ln,this_node)
          call pm_ptr_assign(context,ptr,j,&
               broadcast_val(context,node,v%data%ptr(v%offset+k),nn))
       enddo
       call pm_delete_root(context,root)
    case(pm_int)
       ptr=pm_new(context,pm_int,nout)
       if(this_node) ptr%data%i(ptr%offset:ptr%offset+esize)=&
            v%data%i(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_int,nout,tno,m)
       call mpi_bcast(ptr%data%i(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_long)
       ptr=pm_new(context,pm_long,nout)
       if(this_node) ptr%data%ln(ptr%offset:ptr%offset+esize)=&
            v%data%ln(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_long,nout,tno,m)
       call mpi_bcast(ptr%data%ln(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_longlong)
       ptr=pm_new(context,pm_longlong,nout)
       if(this_node) ptr%data%lln(ptr%offset:ptr%offset+esize)=&
            v%data%lln(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_longlong,nout,tno,m)
       call mpi_bcast(ptr%data%lln(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_int8)
       ptr=pm_new(context,pm_int8,nout)
       if(this_node) ptr%data%i8(ptr%offset:ptr%offset+esize)=&
            v%data%i8(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_int8,nout,tno,m)
       call mpi_bcast(ptr%data%i8(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_int16)
       ptr=pm_new(context,pm_int16,nout)
       if(this_node) ptr%data%i16(ptr%offset:ptr%offset+esize)=&
            v%data%i16(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_int16,nout,tno,m)
       call mpi_bcast(ptr%data%i16(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_int32)
       ptr=pm_new(context,pm_int32,nout)
       if(this_node) ptr%data%i32(ptr%offset:ptr%offset+esize)=&
            v%data%i32(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_int32,nout,tno,m)
       call mpi_bcast(ptr%data%i32(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_int64)
       ptr=pm_new(context,pm_int64,nout)
       if(this_node) ptr%data%i64(ptr%offset:ptr%offset+esize)=&
            v%data%i64(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_int64,nout,tno,m)
       call mpi_bcast(ptr%data%i64(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_single)
       ptr=pm_new(context,pm_single,nout)
       if(this_node) ptr%data%r(ptr%offset:ptr%offset+esize)=&
            v%data%r(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_single,noff,tno,m)
       call mpi_bcast(ptr%data%r(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_double)
       ptr=pm_new(context,pm_double,nout)
       if(this_node) ptr%data%d(ptr%offset:ptr%offset+esize)=&
            v%data%d(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_double,nout,tno,m)
       call mpi_bcast(ptr%data%d(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_single_complex)
       ptr=pm_new(context,pm_single,nout)
       if(this_node) ptr%data%c(ptr%offset:ptr%offset+esize)=&
            v%data%c(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_single_complex,noff,tno,m)
       call mpi_bcast(ptr%data%c(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_double_complex)
       ptr=pm_new(context,pm_double_complex,nout)
       if(this_node) ptr%data%dc(ptr%offset:ptr%offset+esize)=&
            v%data%dc(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_double_complex,nout,tno,m)
       call mpi_bcast(ptr%data%d(ptr%offset),m,&
            tno,node,comm,errno)
    case(pm_logical)
       ptr=pm_new(context,pm_logical,nout)
       if(this_node) ptr%data%l(ptr%offset:ptr%offset+esize)=&
            v%data%l(v%offset+off%data%ln(off%offset+offstart:off%offset+offstart+noff-1))
       call get_mpi_type(pm_logical,nout,tno,m)
       call mpi_bcast(ptr%data%l(ptr%offset),m,&
            tno,node,comm,errno)
    end select
    if(debug_mess) then
       write(*,*) 'BCAST VAL DONE'
    endif
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function  broadcast_val_disp
  
  ! Gather data from all processes
  recursive subroutine gather(context,v,w,j)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,w
    integer(pm_ln),intent(in):: j
    integer:: comm
    integer:: i,m,tno,m2,tno2,errno
    integer(pm_ln):: nn
    type(pm_ptr):: avec
    integer:: k
    if(debug_mess) then
       write(*,*) 'GATHER',sys_node
    endif
    comm=par_frame(par_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call gather(context,v%data%ptr(v%offset+pm_array_dom),&
            w%data%ptr(w%offset+pm_array_dom),j)
       call gather(context,v%data%ptr(v%offset+pm_array_length),&
            w%data%ptr(w%offset+pm_array_length),j)
       do k=0,j
          call pm_ptr_assign(context,&
               w%data%ptr(w%offset+pm_array_vect),int(k,pm_ln),&
               broadcast_val(context,k,&
               v%data%ptr(v%offset+pm_array_vect),nn))
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call gather(context,v%data%ptr(v%offset+i),w%data%ptr(w%offset+i),j)
       enddo
    case(pm_pointer)
       do k=0,j
          call pm_ptr_assign(context,&
               v,int(k,pm_ln),&
               broadcast_val(context,k,v,nn))
       enddo
    case(pm_int)
       call mpi_allgather(v%data%i(v%offset),1,MPI_INTEGER,&
            w%data%i(w%offset),1,MPI_INTEGER,comm,errno)
    case(pm_long)
       call mpi_allgather(v%data%ln(v%offset),1,MPI_AINT,&
            w%data%ln(w%offset),1,MPI_AINT,comm,errno)
       
    case(pm_single)
       call mpi_allgather(v%data%r(v%offset),1,MPI_REAL,&
            w%data%r(w%offset),1,MPI_REAL,comm,errno)
    case(pm_double)
       call mpi_allgather(v%data%d(v%offset),1,MPI_DOUBLE,&
            w%data%d(w%offset),1,MPI_DOUBLE,comm,errno)
    case(pm_logical)
       call mpi_allgather(v%data%l(v%offset),1,MPI_LOGICAL,&
            w%data%l(w%offset),1,MPI_LOGICAL,comm,errno)
    end select
    if(debug_mess) then
       write(*,*) 'GATHERED',sys_node
        
    endif
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine gather
  
  recursive subroutine recv(context,node,v,mess_tag)
    type(pm_context),pointer:: context
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,tno,m,errno
    integer(pm_ln):: j,k,esize
    type(pm_ptr):: len,avec
    character,dimension(1):: buffer
    comm=par_frame(par_depth)%this_comm
    tno=pm_fast_typeof(v)
    esize=pm_fast_esize(v)
    if(debug_mess) write(*,*) 'on',par_frame(par_depth)%this_node,'recv',tno,'from',node
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call recv(context,node,v%data%ptr(v%offset+pm_array_dom),mess_tag)
       len=v%data%ptr(v%offset+pm_array_length)
       call recv(context,node,len,mess_tag)
       avec=v%data%ptr(v%offset+pm_array_vect)
       
       do j=0,pm_fast_esize(len)
          call pm_ptr_assign(context,avec,j,recv_val(context,node,mess_tag+1))
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,esize
          call recv(context,node,v%data%ptr(v%offset+i),mess_tag)
       enddo
    case(pm_dref_type,pm_dref_shared_type)
       tno=pm_fast_typeof(v%data%ptr(v%offset+2))
       if(tno==pm_dref_type.or.tno==pm_dref_shared_type) then
          call recv(context,node,v%data%ptr(v%offset+2),mess_tag)
       endif
       call pm_ptr_assign(context,v,3_pm_ln,recv_val(context,node,mess_tag+1))
    case(pm_pointer)
       call mpi_recv(buffer,0,MPI_CHARACTER,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       do j=0,esize
          call pm_ptr_assign(context,avec,j,recv_val(context,node,mess_tag+1))
       enddo
    case(pm_int)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%i(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_long)
       call get_mpi_type(pm_long,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%ln(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_longlong)
       call get_mpi_type(pm_longlong,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%lln(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_int8)
       call get_mpi_type(pm_int8,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%i8(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_int16)
       call get_mpi_type(pm_int16,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%i16(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_int32)
       call get_mpi_type(pm_int32,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%i32(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_int64)
       call get_mpi_type(pm_int64,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%i64(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_single)
       call get_mpi_type(pm_single,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%r(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_double)
       call get_mpi_type(pm_double,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%d(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_single_complex)
       call get_mpi_type(pm_single_complex,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%c(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_double_complex)
       call get_mpi_type(pm_double_complex,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%dc(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_logical)
       call get_mpi_type(pm_logical,esize+1_pm_ln,tno,m)
       call mpi_recv(v%data%l(v%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine recv

  recursive subroutine recv_rest(context,node,v,mess_tag)
    type(pm_context),pointer:: context
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,tno,m,errno
    integer(pm_ln):: j,esize
    type(pm_ptr):: len,avec
    character,dimension(1):: buffer
    comm=par_frame(par_depth)%this_comm
    tno=pm_fast_typeof(v)
    esize=pm_fast_esize(v)
    if(debug_mess) write(*,*) 'on',par_frame(par_depth)%this_node,'recv',tno,'from',node
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       avec=v%data%ptr(v%offset+pm_array_vect)
       do j=0,pm_fast_esize(avec)
          call pm_ptr_assign(context,avec,j,recv_val(context,node,mess_tag+1))
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,esize
          call recv_rest(context,node,v%data%ptr(v%offset+i),mess_tag)
       enddo
    case(pm_dref_type,pm_dref_shared_type)
       tno=pm_fast_typeof(v%data%ptr(v%offset+2))
       if(tno==pm_dref_type.or.tno==pm_dref_shared_type) then
          call recv_rest(context,node,v%data%ptr(v%offset+2),mess_tag)
       endif
       call pm_ptr_assign(context,v,3_pm_ln,recv_val(context,node,mess_tag+1))
    case(pm_pointer)
       call mpi_recv(buffer,0,MPI_CHARACTER,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       do j=0,esize
          call pm_ptr_assign(context,avec,j,recv_val(context,node,mess_tag+1))
       enddo
    case default
       return
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine recv_rest

  recursive subroutine recv_disp(context,node,v,off,offstart,noff,mess_tag)
    type(pm_context),pointer:: context
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln):: offstart,noff
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,tno,errno
    integer(pm_ln):: j,k
    type(pm_ptr):: len,avec
    character,dimension(1):: buffer
 
    comm=par_frame(par_depth)%this_comm
    tno=pm_fast_typeof(v)
    if(debug_mess) write(*,*) 'on',par_frame(par_depth)%this_node,'recv disp',tno,'from',node
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call recv_disp(context,node,v%data%ptr(v%offset+pm_array_dom),&
            off,offstart,noff,mess_tag)
       len=v%data%ptr(v%offset+pm_array_length)
       call recv_disp(context,node,len,off,offstart,noff,mess_tag)
       avec=v%data%ptr(v%offset+pm_array_vect)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call pm_ptr_assign(context,avec,k,recv_val(context,node,mess_tag+1))
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call recv_disp(context,node,v%data%ptr(v%offset+i),off,offstart,noff,mess_tag)
       enddo
    case(pm_pointer)
       call mpi_recv(buffer,0,MPI_CHARACTER,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call pm_ptr_assign(context,v,k,recv_val(context,node,mess_tag+1))
       enddo
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,offstart,noff,tno)
       call mpi_recv(v%data%i(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_long)
       call get_mpi_disp_type(pm_long,off,offstart,noff,tno)
       call mpi_recv(v%data%ln(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_longlong)
       call get_mpi_disp_type(pm_longlong,off,offstart,noff,tno)
       call mpi_recv(v%data%lln(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_int8)
       call get_mpi_disp_type(pm_int8,off,offstart,noff,tno)
       call mpi_recv(v%data%i8(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_int16)
       call get_mpi_disp_type(pm_int16,off,offstart,noff,tno)
       call mpi_recv(v%data%i16(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_int32)
       call get_mpi_disp_type(pm_int32,off,offstart,noff,tno)
       call mpi_recv(v%data%i32(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_int64)
       call get_mpi_disp_type(pm_int64,off,offstart,noff,tno)
       call mpi_recv(v%data%i64(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,offstart,noff,tno)
       call mpi_recv(v%data%r(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,offstart,noff,tno)
       call mpi_recv(v%data%d(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_single_complex)
       call get_mpi_disp_type(pm_single_complex,off,offstart,noff,tno)
       call mpi_recv(v%data%c(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_double_complex)
       call get_mpi_disp_type(pm_double_complex,off,offstart,noff,tno)
       call mpi_recv(v%data%dc(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,offstart,noff,tno)
       call mpi_recv(v%data%l(v%offset),1,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine recv_disp

  recursive subroutine recv_rest_disp(context,node,v,off,offstart,noff,mess_tag)
    type(pm_context),pointer:: context
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln):: offstart,noff
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,tno,errno
    integer(pm_ln):: j,k
    type(pm_ptr):: len,avec
    character,dimension(1):: buffer
    comm=par_frame(par_depth)%this_comm
    tno=pm_fast_typeof(v)
    if(debug_mess) write(*,*) 'ON',sys_node,'recv_rest_disp',tno
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       avec=v%data%ptr(v%offset+pm_array_vect)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call pm_ptr_assign(context,avec,k,recv_val(context,node,mess_tag+1))
       enddo
       if(debug_mess) call pm_dump_tree(context,6,v,2)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call recv_rest_disp(context,node,v%data%ptr(v%offset+i),off,offstart,noff,mess_tag)
       enddo
    case(pm_pointer)
       !write(*,*) 'RECV',noff,mess_tag,node
       !call mpi_recv(buffer,0,MPI_CHARACTER,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call pm_ptr_assign(context,v,k,recv_val(context,node,mess_tag+1))
       enddo
    case default
       return
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine recv_rest_disp

  ! Receive tagged data
  recursive function recv_val(context,node,mess_tag) result(ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: node,mess_tag
    type(pm_ptr):: ptr
    integer:: comm
    integer:: i,m,tno,errno
    integer(pm_ln),dimension(3):: hdr
    integer(pm_ln):: esize,j
    type(pm_ptr):: avec,w,len
    type(pm_root),pointer:: root

    if(debug_mess) then
       write(*,*) sys_node,'RECV VAL',node,mess_tag
    endif
    
    if(node==MPI_PROC_NULL) return
    comm=par_frame(par_depth)%this_comm

    ! recv header: type/size/pm_type
    call mpi_recv(hdr,3,MPI_AINT,node,mess_tag,comm,mpi_status_ignore,errno)

    tno=hdr(1)
    esize=hdr(2)
    if(debug_mess) then
       write(*,*) sys_node,'RECV_VAL',tno,esize
    endif
    select case(tno)
    case(pm_null)
       ptr=pm_null_obj
    case(pm_array_type,pm_const_array_type)
       root=>pm_new_as_root(context,pm_usr,int(pm_array_size,pm_ln))
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)%offset=pm_array_type
       ptr%data%ptr(ptr%offset+1)%offset=hdr(3)
       call pm_ptr_assign(context,ptr,int(pm_array_dom,pm_ln),&
            recv_val(context,node,mess_tag))
       call pm_ptr_assign(context,ptr,int(pm_array_length,pm_ln),&
            recv_val(context,node,mess_tag))
       len=ptr%data%ptr(ptr%offset+pm_array_length)
       esize=pm_fast_esize(len)
       w=pm_assign_new(context,ptr,int(pm_array_offset,pm_ln),&
            pm_long,esize+1,.true.)
       avec=pm_assign_new(context,ptr,int(pm_array_vect,pm_ln),&
            pm_pointer,esize+1,.true.)
       do j=0,esize
          call pm_ptr_assign(context,avec,j,&
               recv_val(context,node,mess_tag))
       enddo
       call pm_delete_root(context,root)
    case(pm_struct_type,pm_rec_type,pm_polyref_type,pm_dref_type,pm_dref_shared_type)
       root=>pm_new_as_root(context,pm_usr,esize+1)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)%offset=tno
       ptr%data%ptr(ptr%offset+1)%offset=hdr(3)
       do j=2,esize
          call pm_ptr_assign(context,ptr,j,&
               recv_val(context,node,mess_tag))
       enddo
       call pm_delete_root(context,root)
    case(pm_pointer)
       root=>pm_new_as_root(context,pm_pointer,esize+1)
       ptr=root%ptr
       do j=0,esize
          call pm_ptr_assign(context,ptr,j,&
               recv_val(context,node,mess_tag))
       enddo
       call pm_delete_root(context,root)
    case(pm_int)
       ptr=pm_new(context,pm_int,esize+1)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%i(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_long)
       ptr=pm_new(context,pm_long,esize+1)
       call get_mpi_type(pm_long,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%ln(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_longlong)
       ptr=pm_new(context,pm_longlong,esize+1)
       call get_mpi_type(pm_longlong,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%lln(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_int8)
       ptr=pm_new(context,pm_int8,esize+1)
       call get_mpi_type(pm_int8,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%i8(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_int16)
       ptr=pm_new(context,pm_int16,esize+1)
       call get_mpi_type(pm_int16,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%i16(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_int32)
       ptr=pm_new(context,pm_int32,esize+1)
       call get_mpi_type(pm_int32,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%i32(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_int64)
       ptr=pm_new(context,pm_int64,esize+1)
       call get_mpi_type(pm_int64,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%i64(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_single)
       ptr=pm_new(context,pm_single,esize+1)
       call get_mpi_type(pm_single,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%r(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_double)
       ptr=pm_new(context,pm_double,esize+1)
       call get_mpi_type(pm_double,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%d(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_single_complex)
       ptr=pm_new(context,pm_single_complex,esize+1)
       call get_mpi_type(pm_single_complex,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%c(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_double_complex)
       ptr=pm_new(context,pm_double_complex,esize+1)
       call get_mpi_type(pm_double_complex,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%dc(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_logical)
       ptr=pm_new(context,pm_logical,esize+1)
       call get_mpi_type(pm_logical,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%l(ptr%offset),m,&
            tno,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case default
!!$       write(*,*) 'HDR==',hdr
!!$       call pm_panic('recv_val')
       ptr=pm_null_obj
    end select
    if(debug_mess) then
       write(*,*) 'RECV VAL DONE'
    endif
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function recv_val

  ! Send from dispersed buffer v[off]
  recursive subroutine isend(node,v,mess_tag,xcomm)
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v
    integer,intent(in):: mess_tag
    integer,intent(in),optional:: xcomm
    integer:: comm
    integer:: i,mess,tno,errno,m
    integer(pm_ln):: j,k,esize
    type(pm_ptr):: avec,len
    character,dimension(1):: buffer
    if(present(xcomm)) then
       comm=xcomm
    else
       comm=par_frame(par_depth)%this_comm
    endif
    tno=pm_fast_typeof(v)
    esize=pm_fast_esize(v)
    if(debug_mess) then
       write(*,*) 'on',par_frame(par_depth)%this_node,'isend',tno,'to',node,'top',message_top
    endif
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call isend(node,v%data%ptr(v%offset+pm_array_dom),mess_tag,xcomm)
       len=v%data%ptr(v%offset+pm_array_length)
       call isend(node,len,mess_tag,xcomm)
       avec=v%data%ptr(v%offset+pm_array_vect)
       do j=0,pm_fast_esize(len)
          call isend_val(node,&
               avec%data%ptr(avec%offset+j),&
               0_pm_ln,-1_pm_ln,mess_tag+1,xcomm)
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call isend(node,v%data%ptr(v%offset+i),mess_tag,xcomm)
       enddo
    case(pm_dref_type,pm_dref_shared_type)
       tno=pm_fast_typeof(v%data%ptr(v%offset+2))
       if(tno==pm_dref_type.or.tno==pm_dref_shared_type) then
          call isend(node,v%data%ptr(v%offset+2),mess_tag,xcomm)
       endif
       call isend_val(node,v%data%ptr(v%offset+3),0_pm_ln,-1_pm_ln,mess_tag+1,xcomm)
    case(pm_pointer)
       !write(*,*) 'SEND'
       call mpi_isend(buffer,0,MPI_CHARACTER,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       do j=0,pm_fast_esize(v)
          call isend_val(node,v%data%ptr(v%offset+j),0_pm_ln,-1_pm_ln,mess_tag+1,xcomm)
       enddo
    case(pm_int)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%i(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
     case(pm_long)
       call get_mpi_type(pm_long,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%ln(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_longlong)
       call get_mpi_type(pm_longlong,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%lln(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int8)
       call get_mpi_type(pm_int8,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%i8(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int16)
       call get_mpi_type(pm_int16,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%i16(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int32)
       call get_mpi_type(pm_int32,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%i32(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int64)
       call get_mpi_type(pm_int64,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%i64(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_single)
       call get_mpi_type(pm_single,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%r(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_double)
       call get_mpi_type(pm_double,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%d(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_single_complex)
       call get_mpi_type(pm_single_complex,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%c(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_double_complex)
       call get_mpi_type(pm_double_complex,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%dc(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_logical)
       call get_mpi_type(pm_logical,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%l(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine isend


  ! Receive 
  recursive subroutine irecv(node,v,mess_tag,iserr,xcomm)
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v
    integer,intent(in):: mess_tag
    logical,intent(out):: iserr
    integer,intent(in),optional:: xcomm
    integer:: comm
    integer:: i,mess,tno,errno,m
    integer(pm_ln):: j,k,esize
    type(pm_ptr):: avec,len
    character,dimension(1):: buffer
    if(present(xcomm)) then
       comm=xcomm
    else
       comm=par_frame(par_depth)%this_comm
    endif
    tno=pm_fast_typeof(v)
    esize=pm_fast_esize(v)
    iserr=.false.
    if(debug_mess) then
       write(*,*) 'on',par_frame(par_depth)%this_node,'irecv',tno,'to',node,'top',message_top
    endif
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       iserr=.true.
       return
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call irecv(node,v%data%ptr(v%offset+i),mess_tag,iserr,xcomm)
       enddo
    case(pm_dref_type,pm_dref_shared_type)
       iserr=.true.
       return
    case(pm_pointer)
       iserr=.true.
       return
    case(pm_int)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%i(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
     case(pm_long)
       call get_mpi_type(pm_long,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%ln(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_longlong)
       call get_mpi_type(pm_longlong,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%lln(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int8)
       call get_mpi_type(pm_int8,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%i8(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int16)
       call get_mpi_type(pm_int16,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%i16(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int32)
       call get_mpi_type(pm_int32,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%i32(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int64)
       call get_mpi_type(pm_int64,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%i64(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_single)
       call get_mpi_type(pm_single,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%r(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_double)
       call get_mpi_type(pm_double,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%d(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_single_complex)
       call get_mpi_type(pm_single_complex,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%c(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_double_complex)
       call get_mpi_type(pm_double_complex,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%dc(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_logical)
       call get_mpi_type(pm_logical,esize+1_pm_ln,tno,m)
       call mpi_irecv(v%data%l(v%offset),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine irecv

  ! Asynchronous send of tagged data
  recursive subroutine isend_val(node,v,start,siz,mess_tag,xcomm)
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: start,siz
    integer,intent(in):: mess_tag
    integer,intent(in),optional:: xcomm
    integer:: comm
    integer:: i,m,tno,mess,errno
    integer(pm_ln):: j,esize
    type(pm_ptr):: len,off,avec
    character(len=1):: buffer
    type(hdr_buf),pointer:: hdr
    if(present(xcomm)) then
       comm=xcomm
    else
       comm=par_frame(par_depth)%this_comm
    endif
    tno=pm_fast_typeof(v)
    ! Send header: type/size/pm_type
    esize=pm_fast_esize(v)
    if(pm_fast_vkind(v)/=pm_usr.and.siz>0) esize=siz-1
    allocate(hdr)
    hdr%next=>hdr_bufs
    hdr_bufs=>hdr
    hdr%hdr(1)=tno
    hdr%hdr(2)=pm_fast_esize(v)
    if(pm_fast_vkind(v)==pm_usr) then
       hdr%hdr(3)=v%data%ptr(v%offset+1)%offset
    endif
    call mpi_isend(hdr%hdr,3,MPI_AINT,node,mess_tag,comm,mess,errno)
    call push_message(mess)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call isend_val(node,v%data%ptr(v%offset+pm_array_dom),start,siz,mess_tag,xcomm)
       len=v%data%ptr(v%offset+pm_array_length)
       call isend_val(node,len,start,siz,mess_tag,xcomm)
       off=v%data%ptr(v%offset+pm_array_offset)
       avec=v%data%ptr(v%offset+pm_array_vect)
       if(siz>0) then
          esize=start+siz-1
       else
          esize=pm_fast_esize(avec)
       endif
       do j=start,esize
          call isend_val(node,avec%data%ptr(avec%offset+j),&
               off%data%ln(off%offset+j),&
               len%data%ln(len%offset+j),mess_tag,xcomm)
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type,pm_dref_type,pm_dref_shared_type)
       do i=2,pm_fast_esize(v)
          call isend_val(node,v%data%ptr(v%offset+i),start,siz,mess_tag,xcomm)
       enddo
    case(pm_pointer)
       call mpi_isend(buffer,0,MPI_CHARACTER,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       if(siz>0) then
          esize=start+siz-1
       else
          esize=pm_fast_esize(v)
       endif
       do j=start,esize
          call isend_val(node,v%data%ptr(v%offset+j),&
               0_pm_ln,-1_pm_ln,mess_tag,xcomm)
       enddo
    case(pm_int)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%i(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_long)
       call get_mpi_type(pm_long,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%ln(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_longlong)
       call get_mpi_type(pm_longlong,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%lln(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int8)
       call get_mpi_type(pm_int8,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%i8(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int16)
       call get_mpi_type(pm_int16,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%i16(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int32)
       call get_mpi_type(pm_int32,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%i32(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int64)
       call get_mpi_type(pm_int64,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%i64(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_single)
       call get_mpi_type(pm_single,esize+1_pm_ln,tno,m)       
       call mpi_isend(v%data%r(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_double)
       call get_mpi_type(pm_double,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%d(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_single_complex)
       call get_mpi_type(pm_single_complex,esize+1_pm_ln,tno,m)       
       call mpi_isend(v%data%c(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_double_complex)
       call get_mpi_type(pm_double_complex,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%dc(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_logical)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%l(v%offset+start),m,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'
  end subroutine isend_val
  
  ! Send from dispersed buffer v[off]
  recursive subroutine isend_val_disp(node,v,off,offstart,noff,mess_tag)
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln),intent(in):: noff,offstart
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,mess,tno,errno
    integer(pm_ln):: j,k
    type(pm_ptr):: avec,len
    character,dimension(1):: buffer
    type(hdr_buf),pointer:: hdr
    comm=par_frame(par_depth)%this_comm
    tno=pm_fast_typeof(v)
    if(debug_mess) then
       write(*,*) 'on',par_frame(par_depth)%this_node,'isend_val_disp',tno,'to',node,'top',message_top
    endif
    allocate(hdr)
    hdr%next=>hdr_bufs
    hdr_bufs=>hdr
    hdr%hdr(1)=tno
    if(pm_fast_vkind(v)==pm_usr) then
       hdr%hdr(2)=pm_fast_esize(v)
    elseif(pm_fast_vkind(off)==pm_long) then
       hdr%hdr(2)=noff-1
    else
       avec=off%data%ptr(off%offset+4)
       hdr%hdr(2)=avec%data%ln(avec%offset)-1
    endif
    if(pm_fast_vkind(v)==pm_usr) then
       hdr%hdr(3)=v%data%ptr(v%offset+1)%offset
    else
       hdr%hdr(3)=0
    endif
    if(debug_mess) write(*,*) 'SEND HDR',hdr%hdr
    call mpi_isend(hdr%hdr,3,MPI_AINT,node,mess_tag,comm,mess,errno)
    call push_message(mess)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call isend_val_disp(node,v%data%ptr(v%offset+pm_array_dom),off,offstart,noff,mess_tag)
       len=v%data%ptr(v%offset+pm_array_length)
       call isend_val_disp(node,len,off,offstart,noff,mess_tag)
       avec=v%data%ptr(v%offset+pm_array_vect)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call isend_val(node,&
               avec%data%ptr(avec%offset+k),&
                  0_pm_ln,-1_pm_ln,mess_tag+1)
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call isend_val_disp(node,v%data%ptr(v%offset+i),off,offstart,noff,mess_tag)
       enddo
    case(pm_pointer)
       call mpi_isend(buffer,0,MPI_CHARACTER,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call isend_val(node,v%data%ptr(v%offset+k),0_pm_ln,-1_pm_ln,mess_tag+1)
       enddo
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,offstart,noff,tno)
       call mpi_isend(v%data%i(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
     case(pm_long)
       call get_mpi_disp_type(pm_long,off,offstart,noff,tno)
       call mpi_isend(v%data%ln(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_longlong)
       call get_mpi_disp_type(pm_longlong,off,offstart,noff,tno)
       call mpi_isend(v%data%lln(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int8)
       call get_mpi_disp_type(pm_int8,off,offstart,noff,tno)
       call mpi_isend(v%data%i8(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int16)
       call get_mpi_disp_type(pm_int16,off,offstart,noff,tno)
       call mpi_isend(v%data%i16(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int32)
       call get_mpi_disp_type(pm_int32,off,offstart,noff,tno)
       call mpi_isend(v%data%i32(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int64)
       call get_mpi_disp_type(pm_int64,off,offstart,noff,tno)
       call mpi_isend(v%data%i64(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,offstart,noff,tno)
       call mpi_isend(v%data%r(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,offstart,noff,tno)
       call mpi_isend(v%data%d(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_single_complex)
       call get_mpi_disp_type(pm_single_complex,off,offstart,noff,tno)
       call mpi_isend(v%data%c(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_double_complex)
       call get_mpi_disp_type(pm_double_complex,off,offstart,noff,tno)
       call mpi_isend(v%data%dc(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,offstart,noff,tno)
       call mpi_isend(v%data%l(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case default
!       if(hdr%hdr(1)==4) call pm_panic('isend_val_disp')
    end select
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine isend_val_disp

  ! Send from dispersed buffer v[off]
  recursive subroutine isend_disp(node,v,off,offstart,noff,mess_tag,xcomm)
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln),intent(in):: noff,offstart
    integer,intent(in):: mess_tag
    integer,intent(in),optional:: xcomm
    integer:: comm
    integer:: i,mess,tno,errno
    integer(pm_ln):: j,k
    type(pm_ptr):: avec,len
    character,dimension(1):: buffer
    if(present(xcomm)) then
       comm=xcomm
    else
       comm=par_frame(par_depth)%this_comm
    endif
    tno=pm_fast_typeof(v)
    if(debug_mess) then
       write(*,*) 'on',par_frame(par_depth)%this_node,'isend',tno,'to',node,'top',message_top
    endif
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call isend_disp(node,v%data%ptr(v%offset+pm_array_dom),off,offstart,noff,mess_tag)
       len=v%data%ptr(v%offset+pm_array_length)
       call isend_disp(node,len,off,offstart,noff,mess_tag)
       avec=v%data%ptr(v%offset+pm_array_vect)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call isend_val(node,&
               avec%data%ptr(avec%offset+k),&
               0_pm_ln,-1_pm_ln,mess_tag+1)
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call isend_disp(node,v%data%ptr(v%offset+i),off,offstart,noff,mess_tag)
       enddo
    case(pm_dref_type,pm_dref_shared_type)
       tno=pm_fast_typeof(v%data%ptr(v%offset+2))
       if(tno==pm_dref_type.or.tno==pm_dref_shared_type) then
          call isend_disp(node,v%data%ptr(v%offset+2),off,offstart,noff,mess_tag)
       endif
       call isend_val_disp(node,v%data%ptr(v%offset+3),off,offstart,noff,mess_tag+1)
    case(pm_pointer)
       call mpi_isend(buffer,0,MPI_CHARACTER,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call isend_val(node,v%data%ptr(v%offset+k),0_pm_ln,-1_pm_ln,mess_tag+1)
       enddo
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,offstart,noff,tno)
       call mpi_isend(v%data%i(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_long)
       call get_mpi_disp_type(pm_long,off,offstart,noff,tno)
       call mpi_isend(v%data%ln(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_longlong)
       call get_mpi_disp_type(pm_longlong,off,offstart,noff,tno)
       call mpi_isend(v%data%lln(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int8)
       call get_mpi_disp_type(pm_int8,off,offstart,noff,tno)
       call mpi_isend(v%data%i8(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int16)
       call get_mpi_disp_type(pm_int16,off,offstart,noff,tno)
       call mpi_isend(v%data%i16(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int32)
       call get_mpi_disp_type(pm_int32,off,offstart,noff,tno)
       call mpi_isend(v%data%i32(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int64)
       call get_mpi_disp_type(pm_int64,off,offstart,noff,tno)
       call mpi_isend(v%data%i64(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,offstart,noff,tno)
       call mpi_isend(v%data%r(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,offstart,noff,tno)
       call mpi_isend(v%data%d(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_single_complex)
       call get_mpi_disp_type(pm_single_complex,off,offstart,noff,tno)
       call mpi_isend(v%data%c(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_double_complex)
       call get_mpi_disp_type(pm_double_complex,off,offstart,noff,tno)
       call mpi_isend(v%data%dc(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,offstart,noff,tno)
       call mpi_isend(v%data%l(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine isend_disp

  ! Asynchronous send of single number
  subroutine isend_num(node,num,mess_tag)
    integer,intent(in):: node
    integer(pm_ln),intent(in):: num
    integer,intent(in):: mess_tag
    integer:: comm,errno,mess
    type(hdr_buf),pointer:: hdr
    comm=par_frame(par_depth)%this_comm
    allocate(hdr)
    hdr%next=>hdr_bufs
    hdr_bufs=>hdr
    hdr%hdr(1)=num
    call mpi_isend(hdr%hdr,1,MPI_AINT,node,mess_tag,comm,mess,errno)
    call push_message(mess)
  end subroutine isend_num

  ! Receive a single number
  subroutine recv_num(context,node,num,mess_tag)
    type(pm_context),pointer:: context
    integer,intent(in):: node,mess_tag
    integer(pm_ln),intent(out):: num
    integer:: comm,errno
    integer(pm_ln):: arr(1)
    comm=par_frame(par_depth)%this_comm
    call mpi_recv(arr,1,MPI_AINT,node,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    num=arr(1)
  end subroutine recv_num
  
  ! Receive into dispersed buffer v[off]
  recursive subroutine irecv_disp(context,node,v,off,offstart,noff,mess_tag,partial)
    type(pm_context),pointer:: context
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln):: offstart,noff
    integer,intent(in):: mess_tag
    logical,intent(out):: partial
    integer:: comm
    integer:: i,mess,tno,errno
    integer(pm_ln):: start1,finish1
    type(pm_ptr):: avec,len
    character,dimension(1):: buffer
    comm=par_frame(par_depth)%this_comm
    tno=pm_fast_typeof(v)
    if(debug_mess) write(*,*) 'on',par_frame(par_depth)%this_node,'irecv',tno,'from',node
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       partial=.true.
       call irecv_disp(context,node,&
            v%data%ptr(v%offset+pm_array_dom),&
            off,offstart,noff,mess_tag,partial)
       call irecv_disp(context,node,&
            v%data%ptr(v%offset+pm_array_length),&
            off,offstart,noff,mess_tag,partial)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call irecv_disp(context,node,v%data%ptr(v%offset+i),&
               off,offstart,noff,mess_tag,partial)
       enddo
    case(pm_pointer)
       partial=.true.
       call mpi_irecv(buffer,0,MPI_CHARACTER,node,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,offstart,noff,tno)
       call mpi_irecv(v%data%i(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_long)
       call get_mpi_disp_type(pm_long,off,offstart,noff,tno)
       call mpi_irecv(v%data%ln(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_longlong)
       call get_mpi_disp_type(pm_longlong,off,offstart,noff,tno)
       call mpi_irecv(v%data%lln(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int8)
       call get_mpi_disp_type(pm_int8,off,offstart,noff,tno)
       call mpi_irecv(v%data%i8(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int16)
       call get_mpi_disp_type(pm_int16,off,offstart,noff,tno)
       call mpi_irecv(v%data%i16(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int32)
       call get_mpi_disp_type(pm_int32,off,offstart,noff,tno)
       call mpi_irecv(v%data%i32(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_int64)
       call get_mpi_disp_type(pm_int64,off,offstart,noff,tno)
       call mpi_irecv(v%data%i64(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,offstart,noff,tno)
       call mpi_irecv(v%data%r(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,offstart,noff,tno)
       call mpi_irecv(v%data%d(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_single_complex)
       call get_mpi_disp_type(pm_single_complex,off,offstart,noff,tno)
       call mpi_irecv(v%data%c(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_double_complex)
       call get_mpi_disp_type(pm_double_complex,off,offstart,noff,tno)
       call mpi_irecv(v%data%dc(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,offstart,noff,tno)
       call mpi_irecv(v%data%l(v%offset),1,&
            tno,node,mess_tag,comm,mess,errno)
       call push_message(mess)
       call mpi_type_free(tno,errno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine irecv_disp

  ! Ready send from dispersed buffer v[off]
  recursive subroutine rsend(context,node,v,mess_tag)
    type(pm_context),pointer:: context
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,m,tno,errno
    integer(pm_ln):: j,k
    type(pm_ptr):: len,avec
    character,dimension(1):: buffer
    integer(pm_ln):: esize
    comm=par_frame(par_depth)%this_comm
    tno=pm_fast_typeof(v)
    esize=pm_fast_esize(v)
    if(debug_mess) write(*,*) 'on',par_frame(par_depth)%this_node,'rsend',tno,'to',node
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call rsend(context,node,v%data%ptr(v%offset+pm_array_dom),mess_tag)
       len=v%data%ptr(v%offset+pm_array_length)
       call rsend(context,node,len,mess_tag)
       avec=v%data%ptr(v%offset+pm_array_vect)
       call push_buffer(context,avec)
       do j=0,esize
          call isend_val(node,avec%data%ptr(avec%offset+j),&
               0_pm_ln,-1_pm_ln,mess_tag+1)
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,esize
          call rsend(context,node,v%data%ptr(v%offset+i),mess_tag)
       enddo
    case(pm_pointer)
       !write(*,*) 'RSEND',esize+1,mess_tag,node
       call mpi_rsend(buffer,0,MPI_CHARACTER,node,mess_tag,comm,errno)
       call push_buffer(context,v)
       do j=0,esize
          call isend_val(node,v%data%ptr(v%offset+j),&
               0_pm_ln,-1_pm_ln,mess_tag+1)
       enddo
    case(pm_int)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%i(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    case(pm_long)
       call get_mpi_type(pm_long,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%ln(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    case(pm_longlong)
       call get_mpi_type(pm_longlong,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%lln(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    case(pm_int8)
       call get_mpi_type(pm_int8,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%i8(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    case(pm_int16)
       call get_mpi_type(pm_int16,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%i16(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    case(pm_int32)
       call get_mpi_type(pm_int32,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%i32(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    case(pm_int64)
       call get_mpi_type(pm_int64,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%i64(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    case(pm_single)
       call get_mpi_type(pm_single,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%r(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    case(pm_double)
       call get_mpi_type(pm_double,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%d(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    case(pm_single_complex)
       call get_mpi_type(pm_single_complex,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%c(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    case(pm_double_complex)
       call get_mpi_type(pm_double_complex,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%dc(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    case(pm_logical)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_rsend(v%data%l(v%offset),m,&
            tno,node,mess_tag,comm,errno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine rsend
  
  ! Ready send from dispersed buffer v[off]
  recursive subroutine rsend_disp(context,node,v,off,offstart,noff,mess_tag)
    type(pm_context),pointer:: context
    integer,intent(in):: node
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln),intent(in):: offstart,noff
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,tno,errno
    integer(pm_ln):: j,k
    type(pm_ptr):: len,avec
    character,dimension(1):: buffer
    comm=par_frame(par_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call rsend_disp(context,node,v%data%ptr(v%offset+pm_array_dom),off,offstart,noff,mess_tag)
       len=v%data%ptr(v%offset+pm_array_length)
       call rsend_disp(context,node,len,off,offstart,noff,mess_tag)
       avec=v%data%ptr(v%offset+pm_array_vect)
       call push_buffer(context,avec)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call isend_val(node,avec%data%ptr(avec%offset+k),&
               0_pm_ln,-1_pm_ln,mess_tag+1)
        enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call rsend_disp(context,node,v%data%ptr(v%offset+i),off,offstart,noff,mess_tag)
       enddo
    case(pm_pointer)
       call mpi_rsend(buffer,0,MPI_CHARACTER,node,mess_tag,comm,errno)
       call push_buffer(context,v)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call isend_val(node,v%data%ptr(v%offset+k),&
               0_pm_ln,-1_pm_ln,mess_tag+1)
       enddo
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,offstart,noff,tno)
       call mpi_rsend(v%data%i(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_long)
       call get_mpi_disp_type(pm_long,off,offstart,noff,tno)
       call mpi_rsend(v%data%ln(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_longlong)
       call get_mpi_disp_type(pm_longlong,off,offstart,noff,tno)
       call mpi_rsend(v%data%lln(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_int8)
       call get_mpi_disp_type(pm_int8,off,offstart,noff,tno)
       call mpi_rsend(v%data%i8(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_int16)
       call get_mpi_disp_type(pm_int16,off,offstart,noff,tno)
       call mpi_rsend(v%data%i8(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_int32)
       call get_mpi_disp_type(pm_int8,off,offstart,noff,tno)
       call mpi_rsend(v%data%i32(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_int64)
       call get_mpi_disp_type(pm_int8,off,offstart,noff,tno)
       call mpi_rsend(v%data%i64(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,offstart,noff,tno)
       call mpi_rsend(v%data%r(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,offstart,noff,tno)
       call mpi_rsend(v%data%d(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_single_complex)
       call get_mpi_disp_type(pm_single,off,offstart,noff,tno)
       call mpi_rsend(v%data%c(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_double_complex)
       call get_mpi_disp_type(pm_double,off,offstart,noff,tno)
       call mpi_rsend(v%data%dc(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,offstart,noff,tno)
       call mpi_rsend(v%data%l(v%offset),1,&
            tno,node,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine rsend_disp


  ! Create a datatype for a potentially very long message
  subroutine get_mpi_type(tno,n,mpi_typ,m)
    integer(pm_p),intent(in):: tno
    integer(pm_ln),intent(in)::n
    integer,intent(out):: mpi_typ
    integer,intent(out):: m
    integer:: errno
    if(n>max_message_size) then
       if(n==large_typ_size(tno)) then
          mpi_typ=large_typ_for_pm(tno)
       else
          mpi_typ=mpi_contig_type(typ_for_pm(tno),n,m)
          if(large_typ_size(tno)>=0) then
             call mpi_type_free(large_typ_for_pm(tno),errno)
          endif
          call mpi_type_commit(mpi_typ,errno)
          large_typ_size(tno)=n
          large_typ_for_pm(tno)=mpi_typ
       endif
    else
       mpi_typ=mpi_contig_type(typ_for_pm(tno),n,m)
    endif
  end subroutine get_mpi_type
  
  ! Make a type from either a set of vector offsets(offstart:noff-1) or a normalised
  ! grid descriptor [ [gsize,start,end,step,width,align] or array, ... ]
  recursive subroutine get_mpi_disp_type(tno,offsets,offstart,noff,mpi_typ)
    integer(pm_p),intent(in):: tno
    type(pm_ptr),intent(in):: offsets
    integer(pm_ln),intent(in):: noff,offstart
    integer,intent(out):: mpi_typ
    integer:: errno,typ,i,j,new_mpi_typ
    type(pm_ptr):: seq,v,off
    integer(MPI_COUNT_KIND):: lb,siz
    integer(pm_ln):: gsize
    typ=pm_fast_typeof(offsets)
    if(typ==pm_long.or.noff==0) then
       mpi_typ=mpi_disp_type(typ_for_pm(tno),offsets,offstart,noff)
    else
       mpi_typ=typ_for_pm(tno)
       off=offsets%data%ptr(offsets%offset+2)
       do i=2,pm_fast_esize(off)
          seq=off%data%ptr(off%offset+i)
          if(pm_fast_esize(seq)==3) then
             gsize=arg(2)
             seq=seq%data%ptr(seq%offset+2)
             v=seq%data%ptr(seq%offset+pm_array_vect)
             v=v%data%ptr(v%offset)
             new_mpi_typ=mpi_disp_type(mpi_typ,v,0_pm_ln,&
                  pm_fast_esize(v)+1)
          else
             new_mpi_typ=mpi_subrange_type(mpi_typ,arg(2),&
                  arg(3),arg(4),arg(5),arg(6))
             gsize=arg(1)
          endif
          if(i/=pm_fast_esize(off)) then
             call mpi_type_get_extent_x(mpi_typ,lb,siz,errno)
             call mpi_type_create_resized(new_mpi_typ,&
                  int(0,MPI_ADDRESS_KIND),int(gsize*siz,MPI_ADDRESS_KIND),&
                  mpi_typ,errno)
          else
             mpi_typ=new_mpi_typ
          endif
       enddo
    endif
    call mpi_type_commit(mpi_typ,errno)
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    
    function arg(j) result(n)
      integer,intent(in):: j
      integer(pm_ln):: n
      type(pm_ptr):: a
      a=seq%data%ptr(seq%offset+1+j)
      n=a%data%ln(a%offset)
    end function arg
    
  end subroutine get_mpi_disp_type
    
  ! Make a type from a set of vector offsets
  recursive function mpi_disp_type(tno,off,offstart,noff) result(mpi_typ)
    integer,intent(in):: tno
    type(pm_ptr),intent(in):: off
    integer(pm_ln),intent(in):: noff,offstart
    integer:: mpi_typ
    integer:: errno,n
    integer(pm_ln):: start,end,i
    integer(MPI_ADDRESS_KIND),allocatable,dimension(:):: disps
    integer,allocatable,dimension(:):: lens,types
    if(noff>max_message_size) then
       n=(noff+max_message_size-1)/max_message_size
       allocate(disps(n),lens(n),types(n))
       disps=0
       lens=1
       do i=1,n
          start=(n-1)*max_message_size+1+offstart
          end=min(noff,n*max_message_size)+offstart
          call mpi_disp_short_type(tno,off,start,&
               int(end-start+1),types(i))
       enddo
       call mpi_type_create_struct(n,lens,disps,types,mpi_typ,errno)
       deallocate(disps,lens,types)
       return
    else
       call mpi_disp_short_type(tno,off,offstart,int(noff),mpi_typ)
    endif
  end function mpi_disp_type
  
  subroutine mpi_disp_short_type(basetyp,off,offstart,noff,mpi_typ)
    integer,intent(in):: basetyp
    type(pm_ptr),intent(in):: off
    integer(pm_ln),intent(in):: offstart
    integer,intent(in):: noff
    integer,intent(out):: mpi_typ
    integer:: errno
    integer(MPI_COUNT_KIND):: lb,siz
    integer(MPI_ADDRESS_KIND),allocatable,dimension(:):: disps
    allocate(disps(noff))
    call mpi_type_get_extent_x(basetyp,lb,siz,errno)
    disps=off%data%ln(off%offset+offstart:off%offset+offstart+noff-1)*siz
    if(debug_mess) then
       write(*,'(a8,8i8)') 'DISPS>',disps
    endif
    call mpi_type_create_hindexed_block(noff,1,disps,&
         basetyp,mpi_typ,errno)
    deallocate(disps)
  end subroutine mpi_disp_short_type

  ! Create MPI type for subrange (start..end by step [width,align]) of (0..gsize)
  function mpi_subrange_type(tno,start,end,step,width,align) result(tno2)
    integer,intent(in):: tno
    integer(pm_ln),intent(in):: start,end,step,width,align
    integer:: tno2
    integer(pm_ln):: n
    integer(MPI_COUNT_KIND):: lb,siz
    integer(MPI_ADDRESS_KIND):: displ(3)
    integer:: blk(3),types(3),errno,m,tno3
    if(debug_mess) write(*,*) sys_node,'SUBRANGE',start,end,step,width,align
    call mpi_type_get_extent_x(tno,lb,siz,errno)
    n=(end-start)/step+1
    if(width>1.and.(align>0.or.start+(n-1)*step+width>end)) then
       ! Partial block at start or end complicates things...
       if(n>2) then
          types(1)=mpi_contig_type(tno, max(0_pm_ln,width-align),blk(1))
          types(2)=mpi_strided_block_type(tno,max(0_pm_ln,n-2),step,width,blk(2))
          types(3)=mpi_contig_type(tno, min(width,end-(start-align+(n-1)*step)+1),blk(3))
          displ(1)=start*siz
          displ(2)=(start-align+step)*siz
          displ(3)=(start-align+(n-1)*step)*siz
          call mpi_type_create_struct(3,blk,displ,types,tno2,errno)
       elseif(n==2) then
          types(1)=mpi_contig_type(tno, max(0_pm_ln,width-align),blk(1))
          types(2)=mpi_contig_type(tno, min(width,end-(start-align+step)+1),blk(2))
          displ(1)=start*siz
          displ(2)=(start-align+step)*siz
          call mpi_type_create_struct(2,blk,displ,types,tno2,errno)
       else
          types(1)=mpi_contig_type(tno,min(end+1,start-align+width)-start,blk(1))
          displ(1)=start*siz
          call mpi_type_create_struct(1,blk,displ,types,tno2,errno)
       endif
    else
       tno3=mpi_strided_block_type(tno,n,step,width,m)
       displ(1)=start*siz
       call mpi_type_create_hindexed_block(1,m,displ,tno3,tno2,errno)
    endif
  end function mpi_subrange_type

  ! Create an mpi type for 0..step*n by step with given width
  function mpi_strided_block_type(tno,n,step,width,count) result(tno2)
    integer,intent(in):: tno
    integer(pm_ln),intent(in):: n,step,width
    integer,intent(out):: count
    integer:: tno2
    integer(MPI_COUNT_KIND):: lb,siz
    integer(MPI_ADDRESS_KIND):: displ(2)
    integer:: types(2),blk(2)
    integer:: tno3,errno,m
    
    if(n==0) then
       tno2=tno
    elseif(step==1.and.width==1) then
       tno2=mpi_contig_type(tno,n,count)
    elseif(n<max_message_size.and.abs(step)<max_message_size.and.width<max_message_size) then
       call mpi_type_vector(int(n),int(width),int(step),tno,tno2,errno)
    else
       ! At least something is larger than max_message_size
       ! contruct by hand

       ! First create a block of size width
       tno3=mpi_contig_type(tno,width)

       ! The extend extent of this type to step
       if(abs(step)>1) then
          call mpi_type_get_extent_x(tno,lb,siz,errno)
          call mpi_type_create_resized(tno3,&
               int(0,MPI_ADDRESS_KIND),int(abs(step)*siz,MPI_ADDRESS_KIND),&
               tno2,errno)
       else
          tno2=tno3
       endif

       ! Now repeat n times - backwards is necessary
       if(step>0) then
          ! Positive step - just repeat type
          tno2=mpi_contig_type(tno2,n,count)
       elseif(n>max_message_size) then
          ! Negative step and long size...
          call mpi_type_get_extent_x(tno2,lb,siz,errno)
          call mpi_type_vector(max_message_size,1,-1,tno2,tno3,errno)
          m=n/max_message_size
          call mpi_type_vector(m,1,-1,tno3,types(2),errno)
          m=n-max_message_size*m
          call mpi_type_vector(m,1,-1,tno2,types(1),errno)
          displ(1)=0
          displ(2)=-m*siz
          blk(1)=1
          blk(2)=1
          call mpi_type_create_struct(2,blk,displ,types,tno2,errno)
       else
          ! Negative step
          call mpi_type_vector(int(n),1,-1,tno,tno2,errno)
       endif
    endif
  end function mpi_strided_block_type

  ! MPI type - repeat n times
  ! (optionally create smaller type to repeat count times
  ! where count is standard int)
  function mpi_contig_type(tno,n,count) result(tno2)
    integer,intent(in):: tno
    integer(pm_ln),intent(in):: n
    integer,intent(out),optional:: count
    integer:: tno2
    integer(MPI_ADDRESS_KIND):: displ(2)
    integer(MPI_COUNT_KIND):: lb,siz
    integer:: types(2),blk(2),errno
    if(n<=1) then
       tno2=tno
       if(present(count)) count=n
    elseif(n>max_message_size) then
       call mpi_type_contiguous(max_message_size,tno,tno2,errno)
       call mpi_type_get_extent_x(tno2,lb,siz,errno)
       types(1)=tno
       types(2)=tno2
       blk(2)=n/max_message_size
       blk(1)=n-blk(2)*max_message_size
       displ(1)=0
       displ(2)=siz*blk(1)
       call mpi_type_create_struct(2,blk,displ,types,tno2,errno)
       if(present(count)) count=1
    elseif(.not.present(count)) then
       call mpi_type_contiguous(int(n),tno,tno2,errno)
    else
       tno2=tno
       count=n
    endif
  end function mpi_contig_type
 
  subroutine push_message(mess)
    integer,intent(in):: mess
    message_top=message_top+1
    if(message_top>=message_stack_size) then
       call grow_stack(message_stack,message_stack_size,message_stack_size*2)
    endif
    message_stack(message_top)=mess
  end subroutine push_message

  subroutine grow_stack(stack,stack_size,new_stack_size)
    integer,dimension(:),allocatable,intent(inout):: stack
    integer,intent(inout):: stack_size
    integer,intent(in):: new_stack_size
    integer,dimension(:),allocatable:: temp
    allocate(temp(stack_size))
    temp=stack
    deallocate(stack)
    allocate(stack(new_stack_size))
    stack(1:stack_size)=temp
    stack_size=new_stack_size
    deallocate(temp)
  end subroutine grow_stack
  
  subroutine complete_messages(context)
    type(pm_context),pointer:: context
    integer,dimension(max_messages):: status
    integer:: errno,i,rq,istat(MPI_STATUS_SIZE)
    if(debug_mess) then
       write(*,*) 'on',par_frame(par_depth)%this_node,'complete ',message_top,' messages'
    endif
    call mpi_waitall(message_top-message_base,message_stack(message_base+1:),&
         MPI_STATUSES_IGNORE,errno)
    call tidy_messages(context)
    if(debug_mess) write(*,*) 'on',par_frame(par_depth)%this_node,'completed all messages'
  end subroutine complete_messages

  subroutine tidy_messages(context)
    type(pm_context),pointer:: context
    integer:: i,errno
    type(pm_ptr):: v
    type(hdr_buf),pointer:: hdr
    if(message_base<max_messages.and.message_stack_size>max_messages) then
       deallocate(message_stack)
       allocate(message_stack(max_messages))
       message_stack_size=max_messages
    endif
    message_top=message_base
    do while(associated(hdr_bufs))
       hdr=>hdr_bufs
       hdr_bufs=>hdr%next
       deallocate(hdr)
    enddo
    buffer_list=pm_null_obj
  end subroutine tidy_messages

  subroutine collate_messages(node,n,nnode,start,from,ve)
    integer(pm_ln),dimension(*),intent(in):: node
    integer(pm_ln),intent(in):: n
    integer,intent(in):: nnode
    integer(pm_ln),dimension(0:nnode),intent(inout):: start
    integer(pm_ln),dimension(n),intent(inout):: from
    integer(pm_ln),dimension(n),optional:: ve
    integer(pm_ln):: i,m,tot
    integer:: p

    if(debug_mess) then
       !write(*,*) 'Collate> node-',n,'>',node(1:n)
       if(present(ve)) write(*,*) 've=',ve(1:n)
    endif
    start(0:nnode)=0
    if(present(ve)) then
       do i=1,n
          p=node(ve(i)+1)
          start(p)=start(p)+1
       enddo
    else
       do i=1,n
          p=node(i)
          start(p)=start(p)+1
       enddo
    endif

    if(debug_mess) then
       write(*,'(a8,8i4)') 'start+',start(0:nnode)
    endif
    
    tot=1
    do i=0,nnode
       m=start(i)
       tot=tot+m
       start(i)=tot
    enddo    
    
    if(debug_mess) then
       write(*,'(a8,8i4)') 'start-',start(0:nnode)
    endif

    if(present(ve)) then
       do i=n,1,-1
          p=node(ve(i)+1)
          m=start(p)-1
          from(m)=ve(i)
          start(p)=m
       enddo
    else
       do i=n,1,-1
          p=node(i)
          m=start(p)-1
          from(m)=i-1
          start(p)=m
       enddo
    endif

    if(debug_mess) then
       write(*,'(a8,8i4)') 'start=',start(0:nnode)
    endif
    
  end subroutine collate_messages
  
  subroutine collate_messages_and_indices(node,index,n,nnode,start,to,from,ve)
    integer(pm_ln),dimension(*),intent(in):: node
    integer(pm_ln),dimension(*),intent(in):: index
    integer(pm_ln),intent(in):: n
    integer,intent(in):: nnode
    integer(pm_ln),dimension(0:nnode),intent(inout):: start
    integer(pm_ln),dimension(n),intent(inout):: to,from
    integer(pm_ln),dimension(n),optional:: ve
    integer(pm_ln):: i,m,tot
    integer:: p

    if(debug_mess) then
       write(*,*) 'Collate> node-',node(1:n)
    endif
    start(0:nnode)=0
    if(present(ve)) then
       do i=1,n
          p=node(ve(i)+1)
          start(p)=start(p)+1
       enddo
    else
       do i=1,n
          p=node(i)
          start(p)=start(p)+1
       enddo
    endif

    if(debug_mess) then
       write(*,'(a8,8i4)') 'start+',start(0:nnode)
    endif
    
    tot=1
    do i=0,nnode
       m=start(i)
       tot=tot+m
       start(i)=tot
    enddo
    
    if(debug_mess) then
       write(*,'(a8,8i4)') 'start-',start(0:nnode)
    endif

    if(present(ve)) then
       do i=n,1,-1
          p=node(ve(i)+1)
          m=start(p)-1
          to(m)=index(ve(i)+1)
          from(m)=ve(i)
          start(p)=m
       enddo
    else
       do i=n,1,-1
          p=node(i)
          m=start(p)-1
          to(m)=index(i)
          from(m)=i-1
          start(p)=m
       enddo
    endif

    if(debug_mess) then
       write(*,'(a8,8i4)') 'start=',start(0:nnode)
    endif
  end subroutine collate_messages_and_indices

  ! w = v at (node,idx) for active
  ! ve (ve must be pre-shrunk if originally in logical vector form)
  subroutine get_remote(context,node,idx,v,w,ve,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: node,idx,v,ve
    integer,intent(out):: errno
    type(pm_ptr):: w
    integer(pm_ln),dimension(:),allocatable:: start,to
    integer:: nnode,rq,comm,i,ii,iii,ncomplete,nmess,inode
    integer:: itag,mess,rmess,rtop,xsize,node_block,this_node
    integer(pm_ln)::ntot,n,jstart,jfinish
    integer,dimension(MPI_STATUS_SIZE):: istat
    type(pm_ptr):: from,rbuffer
    type(pm_root),pointer:: root,root2
    logical:: partial
    
    if(debug_mess) then
       write(*,*) 'GET REMOTE:',sys_node
    endif

    errno=0
    
    nnode=par_frame(par_depth)%this_nnode
    comm=par_frame(par_depth)%this_comm
    this_node=par_frame(par_depth)%this_node
    if(pm_fast_isnull(ve)) then
       ntot=pm_fast_esize(node)+1
    else
       ntot=pm_fast_esize(ve)
    endif

    partial=.false.
    
    if(ntot>0) then
       
       allocate(start(0:nnode))
       
       xsize=exchange_block
       allocate(to(xsize))
       root2=>pm_new_as_root(context,pm_long,int(xsize,pm_ln))
       rbuffer=root2%ptr
      
       node_block=pm_node_block/vector_num_leaves(v)
       
       ! Phase I compute and send data requests and process any sent here
       do jstart=0,ntot,exchange_block

          root=>pm_new_as_root(context,pm_long,int(xsize,pm_ln))
          from=root%ptr
          
          jfinish=min(jstart+exchange_block-1,ntot-1)
          n=jfinish-jstart+1
          start(nnode)=n+1
          
          if(pm_fast_isnull(ve)) then
             call collate_messages_and_indices(node%data%ln(node%offset+jstart:),&
                  idx%data%ln(idx%offset+jstart:),&
                  n,nnode,start,to,from%data%ln(from%offset:))
          else
             call collate_messages_and_indices(node%data%ln(node%offset:),&
                  idx%data%ln(idx%offset:),&
                  n,nnode,start,to,from%data%ln(from%offset:),&
                  ve%data%ln(ve%offset+jstart:))
          endif

          ! Post speculative request receive
          call mpi_irecv(rbuffer%data%ln(rbuffer%offset),&
               exchange_block,MPI_AINT,&
               MPI_ANY_SOURCE,req_tag,&
               comm,rmess,errno)
          
          do ii=0,nnode-1,node_block

             ! Push speculative receive (top of stack)
             call push_message(rmess)
             
             ! Post data receives
             do i=ii,min(ii+node_block,nnode-1)
                if(i==this_node.or.start(i)==start(i+1)) cycle
                call irecv_disp(context,i,w,from,start(i)-1,&
                     start(i+1)-start(i),data_tag,partial)
             enddo
             
             ! Note number of receives
             rtop=message_top-message_base
             
             ! Post request sends
             do i=ii,min(ii+node_block,nnode-1)
                if(i==this_node.or.start(i)==start(i+1)) cycle
                if(debug_mess) then
                   write(*,'(a8,8i8)') 'send',this_node,i,&
                        to(start(i):start(i+1)-1)
                endif
                call mpi_issend(to(start(i)),&
                     int(start(i+1)-start(i)),MPI_AINT,i,req_tag,comm,&
                     mess,errno)
                call push_message(mess)
             enddo
            
             ! Process intra-node data
             if(start(this_node)/=start(this_node+1)) then
                if(debug_mess) then
                   write(*,*) 'cp',start(this_node),start(this_node+1)
                   write(*,*) 'copy over',&
                        to(start(this_node):start(this_node+1)-1),&
                        from%data%ln(from%offset-1+start(this_node):&
                        from%offset-1+start(this_node+1)-1)
                endif
                call vector_copy_elems(context,w,v,&
                     from%data%ln(from%offset-1+start(this_node):),&
                     to(start(this_node):),&
                     start(this_node+1)-start(this_node),errno)
             endif
             
             ! Process requests
             ncomplete=0
             do while(ncomplete<message_top-message_base-1)
                call mpi_waitany(message_top-message_base,message_stack(message_base+1:),rq,istat,errno)
                if(debug_mess) then
                   write(*,*) 'GOT',sys_node,rq,message_top
                endif
                if(rq==1) then
                   
                   ! Service data request
                   call mpi_get_count(istat,MPI_AINT,nmess,errno)
                   inode=istat(MPI_SOURCE)
                   itag=istat(MPI_TAG)
                   
                   if(debug_mess) then
                      write(*,'(a8,8i8)') 'recv',this_node,inode,&
                           rbuffer%data%ln(rbuffer%offset:&
                           rbuffer%offset+nmess-1)
                   endif
                   
                   call rsend_disp(context,inode,v,rbuffer,0_pm_ln,int(nmess,pm_ln),data_tag)
                   
                   ! Repost speculative receive
                   call mpi_irecv(rbuffer%data%ln(rbuffer%offset),&
                        exchange_block,&
                        MPI_AINT,MPI_ANY_SOURCE,req_tag,&
                        comm,mess,errno)
                   message_stack(1+message_base)=mess
                else
                   ! Count completed requests sent from here
                   ncomplete=ncomplete+1
                   if(ncomplete<message_top-message_base-1) then
                      if(debug_mess) then
                         write(*,*) sys_node,ncomplete,&
                              'Phase I completed out of',message_top-1
                      endif
                      message_stack(rq+message_base)=MPI_MESSAGE_NULL
                   else
                      if(debug_mess) then
                         write(*,*) 'Phase I all complete',sys_node
                      endif
                      exit
                   endif
                endif
             enddo

             if(partial) then
                do i=ii,min(ii+node_block,nnode-1)
                   if(i==this_node.or.start(i)==start(i+1)) cycle
                   call recv_rest_disp(context,i,w,from,start(i)-1,&
                     start(i+1)-start(i),data_tag)
                enddo
             endif
             
             rmess=message_stack(1+message_base)
             call tidy_messages(context)
 
          enddo
          
       enddo
       call pm_delete_root(context,root)
    else
       ! Post speculative receive
       xsize=exchange_block
       root2=>pm_new_as_root(context,pm_long,int(xsize,pm_ln))
       rbuffer=root2%ptr
       message_top=message_top+1
       call mpi_irecv(rbuffer%data%ln(rbuffer%offset),exchange_block,&
            MPI_AINT,MPI_ANY_SOURCE,req_tag,&
            comm,rmess,errno)
    endif
    
    if(debug_mess) then
       write(*,*) 'Phase II started',sys_node
    endif
    
    ! Phase II - post non blocking barrier and
    ! continue to service requests until it completes
    call tidy_messages(context)
    message_stack(1+message_base)=rmess
    call mpi_ibarrier(comm,message_stack(2+message_base),errno)
    message_top=2+message_base
    ncomplete=0
    do
       call mpi_waitany(2,message_stack,rq,istat,errno)
       if(debug_mess) then
          write(*,*) 'ANDGOT',this_node,rq
       endif
       if(rq==1) then
          call mpi_get_count(istat,MPI_AINT,nmess,errno)
          inode=istat(MPI_SOURCE)
          itag=istat(MPI_TAG)
          if(debug_mess) then
             write(*,*) 'nmess=',nmess
             write(*,'(a5,8i8)') 'recv-',this_node,inode,&
                  rbuffer%data%ln(rbuffer%offset:rbuffer%offset+nmess-1)
          endif
          call rsend_disp(context,inode,v,rbuffer,0_pm_ln,int(nmess,pm_ln),data_tag)
          ! Repost speculative receive
          call mpi_irecv(rbuffer%data%ln(rbuffer%offset),exchange_block,&
               MPI_AINT,MPI_ANY_SOURCE,req_tag,&
               comm,rmess,errno)
          message_stack(1+message_base)=rmess
       else
          if(rq==2) then
             ! Cancel speculative recv
             call mpi_cancel(message_stack(1+message_base),errno)
             message_stack(1+message_base)=MPI_MESSAGE_NULL
          endif
          ! rsend can spawn isends -- so message_top>2 is possible
          ncomplete=ncomplete+1
          message_stack(rq+message_base)=MPI_MESSAGE_NULL
          if(ncomplete>=message_top-message_base-1) exit
       endif
    enddo
    if(ntot>0) then
       deallocate(start,to)
    endif
    call pm_delete_root(context,root2)
    if(debug_mess) then
       write(*,*) 'get_remote_done',sys_node
    endif
    
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'fvkind.inc'
  end subroutine get_remote

  subroutine put_remote(context,node,idx,v,w,ve,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: node,idx,v,ve
    integer,intent(out):: errno
    type(pm_ptr):: w
    integer(pm_ln),dimension(:),allocatable:: start,to
    integer:: nnode,rq,comm,i,ii,k,m,ncomplete,nmess,inode
    integer:: itag,mess,rmess,rtop,xsize,node_block,this_node
    integer(pm_ln)::ntot,n,jstart,jfinish
    integer,dimension(MPI_STATUS_SIZE):: istat
    type(pm_ptr):: from,rbuffer
    type(pm_root),pointer:: root,root2
    logical:: shared
    integer,dimension(1):: ranks,co_procs
    
    if(debug_mess) then
       write(*,*) 'PUT REMOTE:',sys_node
       call pm_dump_tree(context,6,v,2)
       call pm_dump_tree(context,6,w,2)
    endif

    errno=0
    
    nnode=par_frame(par_depth)%this_nnode
    comm=par_frame(par_depth)%this_comm
    this_node=par_frame(par_depth)%this_node

    shared=par_frame(par_depth)%is_shared.and.par_frame(par_depth)%shared_node==0
    
    if(par_frame(par_depth)%shared_node>0) then
       ntot=0
    elseif(pm_fast_isnull(ve)) then
       ntot=pm_fast_esize(node)+1
    else
       ntot=pm_fast_esize(ve)
    endif

    if(ntot>0) then
       
       allocate(start(0:nnode))
       
       xsize=exchange_block
       allocate(to(xsize))
       root2=>pm_new_as_root(context,pm_long,int(xsize,pm_ln))
       rbuffer=root2%ptr
      
       node_block=pm_node_block/vector_num_leaves(v)
       
       ! Phase I compute and send data requests and process any sent here
       do jstart=0,ntot,exchange_block

          root=>pm_new_as_root(context,pm_long,int(xsize,pm_ln))
          from=root%ptr
          
          jfinish=min(jstart+exchange_block-1,ntot-1)
          n=jfinish-jstart+1
          start(nnode)=n+1
          
          if(pm_fast_isnull(ve)) then
             call collate_messages_and_indices(node%data%ln(node%offset+jstart:),&
                  idx%data%ln(idx%offset+jstart:),&
                  n,nnode,start,to,from%data%ln(from%offset:))
          else
             call collate_messages_and_indices(node%data%ln(node%offset:),&
                  idx%data%ln(idx%offset:),&
                  n,nnode,start,to,from%data%ln(from%offset:),&
                  ve%data%ln(ve%offset+jstart:))
          endif

          ! Post speculative request receive
          call mpi_irecv(rbuffer%data%ln(rbuffer%offset),&
               exchange_block,MPI_AINT,&
               MPI_ANY_SOURCE,req_tag,&
               comm,rmess,errno)
          
          do ii=0,nnode-1,node_block
             
             ! Note number of receives
             rtop=message_top-message_base

             ! Post data sends
             do i=ii,min(ii+node_block,nnode-1)
                if(i==this_node.or.start(i)==start(i+1)) cycle
                call isend_disp(i,w,from,start(i)-1,&
                     start(i+1)-start(i),data_tag)
             enddo
                
             ! Post request sends
             do i=ii,min(ii+node_block,nnode-1)
                if(i==this_node.or.start(i)==start(i+1)) cycle
                if(debug_mess) then
                   write(*,'(a8,8i8)') 'send',this_node,i,&
                        to(start(i):start(i+1)-1)
                endif
                call mpi_issend(to(start(i)),&
                     int(start(i+1)-start(i)),MPI_AINT,i,req_tag,comm,&
                     mess,errno)
                call push_message(mess)
 
             enddo
             
             ! Push speculative receive (top of stack)
             call push_message(rmess)
             
             ! Process intra-node data
             if(start(this_node)/=start(this_node+1)) then
                if(debug_mess) then
                   write(*,*) 'cp',start(this_node),start(this_node+1)
                   write(*,*) 'copy over',&
                        to(start(this_node):start(this_node+1)-1),'===>',&
                        from%data%ln(from%offset-1+start(this_node):&
                        from%offset-1+start(this_node+1)-1)
                endif
                call vector_copy_elems(context,v,w,&
                     to(start(this_node):),&
                     from%data%ln(from%offset-1+start(this_node):),&
                     start(this_node+1)-start(this_node),errno)
             endif
             
             ! Process requests
             ncomplete=0
             do while(ncomplete<message_top-message_base-1)
                call mpi_waitany(message_top-message_base,message_stack(message_base+1:),rq,istat,errno)
                if(debug_mess) then
                   write(*,*) 'GOT',sys_node,rq,message_top
                endif
                if(rq==message_top) then
                   
                   ! Service data request
                   call mpi_get_count(istat,MPI_AINT,nmess,errno)
                   inode=istat(MPI_SOURCE)
                   itag=istat(MPI_TAG)
                   
                   if(debug_mess) then
                      write(*,'(a8,8i8)') 'recv',this_node,inode,&
                           rbuffer%data%ln(rbuffer%offset:&
                           rbuffer%offset+nmess-1)
                   endif
                 
                   call recv_disp(context,inode,v,rbuffer,0_pm_ln,&
                        int(nmess,pm_ln),data_tag)

                   if(shared) then
                      do i=1,par_frame(par_depth)%shared_nnode-1
                         k=get_shared(i)
                         call mpi_isend(rbuffer,nmess,MPI_AINT,k,req_tag,&
                              comm,mess,errno) 
                         call isend_disp(k,v,rbuffer,0_pm_ln,int(nmess,pm_ln),data_tag,&
                              comm)
                      enddo
                   endif
                   
                   ! Repost speculative receive
                   call mpi_irecv(rbuffer%data%ln(rbuffer%offset),&
                        exchange_block,&
                        MPI_AINT,MPI_ANY_SOURCE,req_tag,&
                        comm,mess,errno)
                   message_stack(message_top)=mess
                else
                   ! Count completed requests sent from here
                   ncomplete=ncomplete+1
                   if(ncomplete<message_top-message_base-1) then
                      if(debug_mess) then
                         write(*,*) sys_node,ncomplete,&
                              'Phase I completed out of',message_top-1
                      endif
                      message_stack(rq+message_base)=MPI_MESSAGE_NULL
                   else
                      if(debug_mess) then
                         write(*,*) 'Phase I all complete',sys_node
                      endif
                      exit
                   endif
                endif
             enddo
             rmess=message_stack(message_top)
             call tidy_messages(context)
             call pm_delete_root(context,root)
          enddo

       enddo

    else
       ! Post speculative receive
       xsize=exchange_block
       root2=>pm_new_as_root(context,pm_long,int(xsize,pm_ln))
       rbuffer=root2%ptr
       call mpi_irecv(rbuffer%data%ln(rbuffer%offset),exchange_block,&
            MPI_AINT,MPI_ANY_SOURCE,req_tag,&
            comm,rmess,errno)
    endif
    
    if(debug_mess) then
       write(*,*) 'Phase II started',sys_node
    endif
    
    ! Phase II - post not blocking barrier and
    ! continue to service requests until it completes
    message_stack(1+message_base)=rmess
    call mpi_ibarrier(comm,message_stack(2+message_base),errno)
    do
       call mpi_waitany(2,message_stack(message_base+1:),rq,istat,errno)
       if(debug_mess) then
          write(*,*) 'ANDGOT',this_node,rq
       endif
       if(rq==1) then
          call mpi_get_count(istat,MPI_AINT,nmess,errno)
          inode=istat(MPI_SOURCE)
          itag=istat(MPI_TAG)
          if(debug_mess) then
             write(*,*) 'nmess=',nmess
             write(*,'(a5,8i8)') 'recv-',this_node,inode,&
                  rbuffer%data%ln(rbuffer%offset:rbuffer%offset+nmess-1)
          endif

          if(debug_mess) write(*,*) 'RECV=',sys_node

          call recv_disp(context,inode,v,rbuffer,0_pm_ln,int(nmess,pm_ln),data_tag)

          if(debug_mess) write(*,*) 'RECV DONE',sys_node

          if(shared) then
             do i=1,par_frame(par_depth)%shared_nnode-1
                k=get_shared(i)
                call mpi_isend(rbuffer,nmess,MPI_AINT,k,req_tag,&
                     comm,mess,errno) 
                call isend_disp(k,v,rbuffer,0_pm_ln,int(nmess,pm_ln),data_tag)
             enddo
          endif
          
          ! Repost speculative receive
          call mpi_irecv(rbuffer%data%ln(rbuffer%offset),exchange_block,&
               MPI_AINT,MPI_ANY_SOURCE,req_tag,&
               comm,mess,errno)
  
          message_stack(1+message_base)=mess
       else
          ! Cancel speculative recv
          call mpi_cancel(message_stack(1+message_base),errno)
          exit
       endif
    enddo
    if(ntot>0) then
       deallocate(start,to)
    endif
    call pm_delete_root(context,root2)
    if(debug_mess) then
       write(*,*) 'put_remote_done',sys_node
    endif
    
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'fvkind.inc'
  end subroutine put_remote


  ! Remote call to a block sending index and supplemental information
  ! in x to multiple node
  ! possibly receiving return data to w (which is returned by block in ref(y))
  function remote_call(context,node,w,x,y,func,stack,pc,args,&
       pc_recv,rblock,mainve,issend,errno,isat) result(iserr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: node,w,x,y,func,stack,pc,mainve
    type(pm_ptr),dimension(*),intent(in):: args
    integer(pm_p), intent(in):: pc_recv
    logical,intent(in):: issend
    integer,intent(out):: errno
    logical,intent(in):: isat
    interface
       function rblock(context,func,stack,pc,arg,pcoff,&
            node,xdata,v,n,issend,disps) result(errno)
         use pm_kinds
         use pm_memory
         type(pm_context),pointer:: context
         type(pm_ptr),intent(in):: func,stack,pc
         type(pm_ptr),dimension(*),intent(in):: arg
         integer(pm_p),intent(in):: pcoff
         integer,intent(in):: node
         integer(pm_ln),intent(in):: n
         type(pm_ptr),intent(in):: xdata,v
         logical,intent(in):: issend
         integer(pm_ln),intent(in),dimension(:),optional:: disps
         integer:: errno
       end function rblock
    end interface
    logical:: iserr
    integer(pm_ln),dimension(:),allocatable:: start
    integer:: nnode,rq,comm,i,ii,k,m,ncomplete,nmess,inode
    integer:: mess,rmess,xsize,node_block,this_node,ack_top
    integer(pm_ln)::ntot,n,jstart,jfinish,esize
    integer,dimension(MPI_STATUS_SIZE):: istat
    type(pm_ptr),target:: xx,vv,ve,from
    integer(pm_ln),dimension(1):: rbuffer
    integer(pm_ln),dimension(:),allocatable::sbuffer
    logical:: shared,haserr,partial
    type(pm_reg),pointer:: reg

    reg=>pm_register(context,'rcall',xx,vv,ve,from)
    if(debug_mess) then
       write(*,*) sys_node,'REMOTE CALL:',this_node,'message_top=',message_top
       call pm_dump_tree(context,6,mainve,2)
       call vector_dump(context,node,2)
       call vector_dump(context,x,2)
    endif

    errno=0
    iserr=.false.

    ve=mainve%data%ptr(mainve%offset+1)
    esize=ve%data%ln(ve%offset)
    ve=mainve%data%ptr(mainve%offset)
    if(pm_fast_vkind(ve)==pm_logical) ve=shrink_ve(context,ve,pm_fast_esize(ve))
    nnode=par_frame(par_depth)%this_nnode
    comm=par_frame(par_depth)%this_comm
    this_node=par_frame(par_depth)%this_node
    if(par_frame(par_depth)%is_shared.and.isat) this_node=par_frame(par_depth)%root_node
   
    shared=issend.and.par_frame(par_depth)%is_shared.and.par_frame(par_depth)%shared_node==0
    if(shared) shared=shared.and.isat

    !write(*,*) this_node,'Shared=',shared
    
    partial=.false.
    
    if(issend.and.par_frame(par_depth)%shared_node>0) then
       ntot=0
    elseif(pm_fast_isnull(ve)) then
       ntot=pm_fast_esize(node)+1
    else
       ntot=pm_fast_esize(ve)
    endif

    !write(*,*) 'ntot=',ntot
    !call pm_dump_tree(context,6,ve,2)
    

    if(debug_mess) then
       write(*,*) this_node,'REMOTE_CALL> ntot=',ntot,'top=',message_top,'ve_kind=',pm_fast_vkind(ve)
       !write(*,*) this_node,'node=(',node%data%ln(node%offset:node%offset+nnode-1),')'
       !call pm_dump_tree(context,6,ve,2)
    endif

    if(ntot>0) then
       
       allocate(start(0:nnode))
       
       xsize=min(exchange_block,ntot)
       from=pm_new(context,pm_long,int(xsize,pm_ln))
       node_block=pm_node_block
       
       ! Phase I compute and send data requests and process any sent here
       do jstart=0,ntot,exchange_block

          jfinish=min(jstart+exchange_block-1,ntot-1)
          n=jfinish-jstart+1
          start(nnode)=n+1
          
          if(pm_fast_isnull(ve)) then
             call collate_messages(node%data%ln(node%offset+jstart:),&
                  n,nnode,start,from%data%ln(from%offset:))
          else
             call collate_messages(node%data%ln(node%offset:),&
                  n,nnode,start,from%data%ln(from%offset:),&
                  ve%data%ln(ve%offset+jstart:))
          endif

          ! Post speculative request receive
          call mpi_irecv(rbuffer,&
               1,MPI_AINT,&
               MPI_ANY_SOURCE,req_tag,&
               comm,rmess,errno)

          ! Speculative receive - bottom of stack
          call push_message(rmess)
          
          do ii=0,nnode-1,node_block
                     
             ! Post data sends & receives
             do i=ii,min(ii+node_block,nnode-1)
                if(i==this_node.or.start(i)==start(i+1)) cycle
                if(start(i)==start(i+1)) cycle
                if(.not.issend) then
                   call irecv_disp(context,i,w,from,start(i)-1,&
                        start(i+1)-start(i),data_tag,partial)
                else
                   call isend_val_disp(i,w,from,start(i)-1,&
                        start(i+1)-start(i),extra_req_tag)
                endif
                call isend_disp(i,x,from,start(i)-1,&
                     start(i+1)-start(i),extra_req_tag)
             enddo

             if(debug_mess) write(*,*) sys_node,'after data sends have message_top=',message_top
             
             ! Post request sends
             allocate(sbuffer(ii:min(ii+node_block,nnode-1)))
             do i=ii,min(ii+node_block,nnode-1)
                if(i==this_node.or.start(i)==start(i+1)) cycle
                if(start(i)==start(i+1)) cycle
                if(debug_mess) then
                   write(*,'(a8,8i8)') 'send',this_node,i,sbuffer(i)
                endif
                sbuffer(i)=start(i+1)-start(i)
                call mpi_issend(sbuffer(i),&
                     1,MPI_AINT,i,req_tag,comm,&
                     mess,errno)
                call push_message(mess)
             enddo

             if(debug_mess) write(*,*) this_node,'after req sends have',message_top,'on',sys_node

             ! Process intra-node data
             if(debug_mess) write(*,*) this_node,'copying',start(this_node),start(this_node+1)
             if(start(this_node)/=start(this_node+1)) then
                xx=copy_dref(context,x,esize+1,.true.)
                if(rblock(context,func,stack,pc,args,&
                     pc_recv,inode,xx,w,esize+1,issend,&
                     from%data%ln(from%offset-1+start(this_node):&
                     from%offset-1+start(this_node+1)-1))/=0) then
                   iserr=.true.
                elseif(.not.issend) then
                   call vector_copy_elems(context,w,y%data%ptr(y%offset),&
                     from%data%ln(from%offset-1+start(this_node):),&
                     from%data%ln(from%offset-1+start(this_node):),&
                     start(this_node+1)-start(this_node),errno)
                endif
             endif

             if(debug_mess) write(*,*) this_node,'at start have',message_top
             
             ! Process requests
             ncomplete=0
             do while(ncomplete<message_top-message_base-1)
                if(debug_mess) write(*,*) this_node,'ncomplete=',ncomplete,'message_top=',message_top
                call mpi_waitany(message_top-message_base,message_stack(message_base+1:),rq,istat,errno)
                if(debug_mess) then
                   write(*,*) 'GOT',sys_node,rq,message_top,MPI_UNDEFINED
                endif
                if(rq==1) then
                   
                   !if(rbuffer(1)>32000) call pm_panic("Big!!")
                   
                   ! Service data request
                   inode=istat(MPI_SOURCE)

                   if(issend) then
                      ! Receive data to be assigned
                      vv=recv_val(context,inode,extra_req_tag)                      
                   endif
            
                   ! Receive additional request data
                   xx=copy_dref(context,x,rbuffer(1),.false.)
                   call recv(context,inode,xx,extra_req_tag)
                   xx=copy_dref(context,xx,rbuffer(1),.true.)

                   ! In a shared context pass on the request to other nodes
                   if(shared) then
                      do i=1,par_frame(par_depth)%shared_nnode-1
                         k=get_shared(i)
                         call isend_val(k,vv,0_pm_ln,&
                              rbuffer(1),extra_req_tag,&
                              par_frame(par_depth)%shared_comm)
                         call isend(k,xx,extra_req_tag)
                      enddo
                   endif
                   
                   ! Execute reception block
                   if(debug_mess) write(*,*) this_node,'EXECUTE BLOCK'
                   if(rblock(context,func,stack,pc,args,&
                        pc_recv,inode,xx,vv,rbuffer(1),issend)/=0) then
                      if(debug_mess) write(*,*) this_node,'BLOCK FAILED'
                      iserr=.true.
                      if(.not.issend) then
                         call rsend(context,inode,empty_copy_vector(context,w,1_pm_ln),data_tag)
                      endif
                   elseif(.not.issend) then
                      if(debug_mess) write(*,*) this_node,'BLOCK COMPLETED'
                      call rsend(context,inode,y%data%ptr(y%offset),data_tag)
                   endif
   
                   ! Repost speculative receive
                   call mpi_irecv(rbuffer,&
                        1,&
                        MPI_AINT,MPI_ANY_SOURCE,req_tag,&
                        comm,mess,errno)
                   message_stack(1+message_base)=mess
                else
                  ! Count completed requests sent from here
                   ncomplete=ncomplete+1
                   if(ncomplete<message_top-message_base-1) then
                      if(debug_mess) then
                         write(*,*) this_node,&
                              'Phase I completed',ncomplete,'out of',message_top-1,'on',sys_node
                      endif
                      message_stack(rq+message_base)=MPI_MESSAGE_NULL
                   else
                      if(debug_mess) then
                         write(*,*) this_node,'Phase I all complete',sys_node
                      endif
                      exit
                   endif
                endif
             enddo

             ! Synchronous data receive when needed
             if(partial.and..not.issend) then
                do i=ii,min(ii+node_block,nnode-1)
                   if(i==this_node.or.start(i)==start(i+1)) cycle
                   call recv_rest_disp(context,i,w,from,start(i)-1,&
                        start(i+1)-start(i),data_tag)
                enddo
             end if
             
             rmess=message_stack(1+message_base)
             deallocate(sbuffer)
             call tidy_messages(context)
          enddo
          if(debug_mess) write(*,*) this_node,'has got to end of a block'

       enddo

    else
       ! Post speculative receive
       message_top=message_top+1
       call mpi_irecv(rbuffer,1,&
            MPI_AINT,MPI_ANY_SOURCE,req_tag,&
            comm,rmess,errno)
       if(debug_mess) write(*,*) this_node,'no messages'
    endif
    
    if(debug_mess) then
       write(*,*) this_node,'Phase II started',sys_node
    endif
    
    ! Phase II - post not blocking barrier and
    ! continue to service requests until it completes
    message_stack(1+message_base)=rmess
    call mpi_ibarrier(comm,message_stack(2+message_base),errno)
    message_top=2+message_base
    ncomplete=0
    do
       call mpi_waitany(message_top-message_base,message_stack(message_base+1:),rq,istat,errno)
       if(debug_mess) then
          write(*,*) this_node,'ANDGOT',rq,MPI_UNDEFINED
       endif
       if(rq==1) then
          inode=istat(MPI_SOURCE)
 
          if(debug_mess) write(*,*) this_node,'RECV-',rbuffer(1),'top=',message_top
        
          ! Receive data to be assigned
          if(issend) then
             vv=recv_val(context,inode,extra_req_tag)
          endif

         if(debug_mess) write(*,*) 'NOW',message_top
          
          ! Receive additional request data
          xx=copy_dref(context,x,rbuffer(1),.false.)
          call recv(context,inode,xx,extra_req_tag)
          xx=copy_dref(context,xx,rbuffer(1),.true.)

          if(debug_mess) write(*,*) 'THEN',message_top

          ! In a shared context pass on the request to other nodes
          if(shared) then
             do i=1,par_frame(par_depth)%shared_nnode-1
                ii=get_shared(i)
                call isend_val(ii,vv,0_pm_ln,rbuffer(1),extra_req_tag,&
                     comm)
                call isend(ii,xx,extra_req_tag)
             enddo
          endif

          if(debug_mess) write(*,*) 'AFTER',message_top
          
          ! Execute reception block
          if(debug_mess) write(*,*) this_node,'EXECUTE BLOCK',rbuffer(1)
          if(rblock(context,func,stack,pc,args,&
               pc_recv,inode,xx,vv,rbuffer(1),issend)/=0) then
             if(debug_mess) write(*,*) this_node,'BLOCK FAILED'
             iserr=.true.
             if(.not.issend) then
                call rsend(context,inode,empty_copy_vector(context,w,1_pm_ln),data_tag)
             endif
          elseif(.not.issend) then
             if(debug_mess) write(*,*) this_node,'BLOCK COMPLETED'
             call rsend(context,inode,y%data%ptr(y%offset),data_tag)
          endif

          if(debug_mess) write(*,*) 'THEN....',message_top
          
          ! Repost speculative receive
          call mpi_irecv(rbuffer,1,&
               MPI_AINT,MPI_ANY_SOURCE,req_tag,&
               comm,mess,errno)
          message_stack(1+message_base)=mess
          if(debug_mess) write(*,*) this_node,'Completed ack - top now',message_top 
       else
          if(debug_mess) write(*,*) this_node,'Got',rq,'top now',ncomplete,message_top
          if(rq==2) then
             ! Cancel speculative recv
             call mpi_cancel(message_stack(1+message_base),errno)
             call mpi_wait(message_stack(1+message_base),istat,errno)
             message_stack(1+message_base)=MPI_MESSAGE_NULL
             if(message_top==2+message_base) then
                exit
             endif
          endif
          ncomplete=ncomplete+1
          message_stack(rq+message_base)=MPI_MESSAGE_NULL
          if(ncomplete>=message_top-message_base-1) then
             exit
          endif
       endif
    enddo
    if(ntot>0) then
       deallocate(start)
    endif

    call tidy_messages(context)

    call pm_delete_register(context,reg)
    
    if(debug_mess) then
       write(*,*) 'get_remote_done',sys_node
    endif
    
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'fvkind.inc'
  end function  remote_call


  ! Collect call to a block sending index and supplemental information
  ! in x to single node
  ! possibly receiving return data to w (which is returned by block in ref(y))
  function collect_call(context,node,w,x,y,func,stack,pc,args,&
       pc_recv,rblock,mainve,disps,issend,errno,isat) result(iserr)
    type(pm_context),pointer:: context
    integer:: node
    type(pm_ptr),intent(in):: w,x,y,func,stack,pc,mainve,disps
    type(pm_ptr),dimension(*),intent(in):: args
    integer(pm_p), intent(in):: pc_recv
    logical,intent(in):: issend
    integer,intent(out):: errno
    logical,intent(in),optional:: isat
    interface
       function rblock(context,func,stack,pc,arg,pcoff,&
            node,xdata,v,n,issend,disps) result(errno)
         use pm_kinds
         use pm_memory
         type(pm_context),pointer:: context
         type(pm_ptr),intent(in):: func,stack,pc
         type(pm_ptr),dimension(*),intent(in):: arg
         integer(pm_p),intent(in):: pcoff
         integer,intent(in):: node
         integer(pm_ln),intent(in):: n
         type(pm_ptr),intent(in):: xdata,v
         logical,intent(in):: issend
         integer(pm_ln),intent(in),dimension(:),optional:: disps
         integer:: errno
       end function rblock
    end interface
    logical:: iserr
    type(pm_ptr):: ve
    integer:: nnode,rq,comm,ii,j,k,ncomplete,nmess,inode
    integer:: mess,rmess,this_node,root_node
    integer(pm_ln)::i,ntot,n,esize,fsize,rsize,xsize
    integer,dimension(MPI_STATUS_SIZE):: istat
    type(pm_ptr),target:: xx,vv
    integer(pm_ln),dimension(1):: rbuffer,sbuffer,xbuffer
    logical:: shared,haserr,partial
    type(pm_reg),pointer:: reg

    reg=>pm_register(context,'ccall',xx,vv)
    if(debug_mess) then
       write(*,*) 'COLLECT CALL:',sys_node,'message_top=',message_top
    endif
    
    iserr=.false.
    errno=0

    ve=mainve%data%ptr(mainve%offset+1)
    fsize=ve%data%ln(ve%offset)
    if(pm_fast_vkind(disps)==pm_long) then
       esize=pm_fast_esize(disps)
    else
       esize=-1
    endif
    ve=mainve%data%ptr(mainve%offset)
    nnode=par_frame(par_depth)%this_nnode
    root_node=par_frame(par_depth)%root_node
    comm=par_frame(par_depth)%this_comm
    this_node=par_frame(par_depth)%this_node
    shared=issend.and.par_frame(par_depth)%is_shared.and.par_frame(par_depth)%shared_node==0
    if(shared) shared=shared.and.isat
    
    if(root_node/=node) then

       if(debug_mess) write(*,*) 'Sending requests'

       ! Posts data sends and receives
       xbuffer(1)=exchange_block
       do i=0,esize,exchange_block
          xsize=min(exchange_block,esize-i+1)
          if(issend) call isend_val_disp(node,w,disps,i,xsize,extra_req_tag)
          call isend_disp(node,x,disps,i,xsize,extra_req_tag)
 
          if(.not.issend) then
             call irecv_disp(context,node,w,disps,i,xsize,data_tag,partial)
          endif
          
          if(xsize==esize-i+1) then
             if(debug_mess) write(*,*) this_node,'send last req to',node
             sbuffer(1)=-xsize
             call mpi_isend(sbuffer,&
                  1,MPI_AINT,node,req_tag,comm,&
                  mess,errno)
             call push_message(mess)
          else
             if(debug_mess) write(*,*) this_node,'send req to',node
             call mpi_isend(xbuffer,&
               1,MPI_AINT,node,req_tag,comm,&
               mess,errno)
             call push_message(mess)
          endif

          if(partial) then
             call recv_rest_disp(context,node,w,disps,i,xsize,data_tag)
          endif
          
          call complete_messages(context)
       enddo
       
    else

       if(debug_mess) write(*,*) 'Collecting/serving requests',nnode

       ! Post speculative request receive
       if(nnode>1) then
          call mpi_irecv(rbuffer,&
               1,MPI_AINT,&
               MPI_ANY_SOURCE,req_tag,&
               comm,rmess,errno)
          call push_message(rmess)
       endif
       
       ! Process intra-node requests
       xx=copy_dref(context,x,fsize+1,.true.)
       if(esize>=0) then
          if(rblock(context,func,stack,pc,args,&
               pc_recv,inode,xx,w,fsize+1,issend,&
               disps%data%ln(disps%offset:&
               disps%offset+esize))/=0) then
             iserr=.true.
          elseif(.not.issend) then
             call vector_copy_elems(context,w,y%data%ptr(y%offset),&
                  disps%data%ln(disps%offset:),&
                  disps%data%ln(disps%offset:),&
                  esize+1,errno)
          endif
       endif
       
       ! Process incoming requests
       ! Process requests
       ncomplete=0
       do while(ncomplete<nnode-1)
          if(debug_mess) then
             write(*,*) 'COLLECTING at', this_node,'ncomplete=',ncomplete,&
                  'nnode=',nnode,'top=',message_top
             write(*,*) 'MESS>',message_stack(message_base+1:message_top)
          endif
          call mpi_waitany(message_top-message_base,message_stack(message_base+1:),rq,istat,errno)
          if(debug_mess) write(*,*) 'REVD',rq
          if(rq==1) then
             if(debug_mess) then
                write(*,*) 'GOT',sys_node,rq,message_top
             endif
             
             ! Service data request
             inode=istat(MPI_SOURCE)

             rsize=rbuffer(1)
             if(rsize<0) then
                rsize=-rsize
                ncomplete=ncomplete+1
             endif
             
             if(debug_mess) write(*,*) 'GOT RSIZE=',rsize
             
             ! Receive data to be assigned
             if(issend) then
                vv=recv_val(context,inode,extra_req_tag)
             endif
             
             ! Receive additional request data
             xx=copy_dref(context,x,rsize,.false.)
             call recv(context,inode,xx,extra_req_tag)
             xx=copy_dref(context,xx,rsize,.true.)

             ! Send on request
             if(shared) then
                do ii=1,par_frame(par_depth)%shared_nnode-1
                   k=get_shared(ii)
                   call isend_val(k,vv,0_pm_ln,rbuffer(1),extra_req_tag,&
                        comm)
                   call isend(k,xx,extra_req_tag)
                enddo
             endif
             
             ! Execute reception block
             if(debug_mess) write(*,*) 'BLOCK -->',inode
             if(rblock(context,func,stack,pc,args,&
                  pc_recv,inode,xx,vv,rsize,issend)/=0) then
                iserr=.true.
                if(.not.issend) then
                   call rsend(context,inode,empty_copy_vector(context,w,1_pm_ln),data_tag)
                endif
             elseif(.not.issend) then
                call rsend(context,inode,y%data%ptr(y%offset),data_tag)
             endif

             if(ncomplete==nnode-1) exit
             
             ! Repost speculative receive
             call mpi_irecv(rbuffer,&
                  1,&
                  MPI_AINT,MPI_ANY_SOURCE,req_tag,&
                  comm,mess,errno)
             message_stack(message_base+1)=mess
          endif
       enddo
       
       call tidy_messages(context)
       
    endif

    call pm_delete_register(context,reg)
    
    if(debug_mess) then
       write(*,*) 'collect_call_done',sys_node
    endif
    
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'fvkind.inc'
  end function  collect_call

  function message_pending_node(tag) result(node)
    integer,intent(in):: tag
    integer:: node
    integer:: errno
    integer,dimension(MPI_STATUS_SIZE):: istat
    call mpi_probe(MPI_ANY_SOURCE,tag,par_frame(par_depth)%this_comm,istat,errno)
    node=istat(MPI_SOURCE)
  end function message_pending_node
  
  function sync_loop_end(ok) result(allok)
    logical,intent(in):: ok
    logical:: allok
    integer:: errno
    if(debug_mess) &
         write(*,*) sys_node,'sync enter',ok
    call mpi_allreduce(ok,allok,1,MPI_LOGICAL,&
         MPI_LOR,par_frame(par_depth)%this_comm,errno)
    if(debug_mess) &
         write(*,*) sys_node,'sync leave',allok
  end function sync_loop_end

  function sync_find(ok) result(node)
    logical,intent(in):: ok
    integer(pm_ln):: node
    integer:: this,chosen,errno
    this=par_frame(par_depth)%this_node
    if(.not.ok) this=-1
     call  mpi_allreduce(this,chosen,1,MPI_INTEGER,MPI_MAX,&
         par_frame(par_depth)%this_comm,errno)
    if(chosen==-1) then
       node=MPI_PROC_NULL
    else
       node=chosen
    endif
  end function sync_find

  function sync_status(pc,status) result(allstatus)
    type(pm_ptr),intent(in):: pc
    integer,intent(in):: status
    integer:: allstatus
    integer:: i,junk,errno
    if(debug_mess) &
         write(*,*) sys_node,'sync_status',par_frame(par_depth)%this_comm,pc%offset
    call mpi_allreduce(status,allstatus,1,MPI_INTEGER,&
        MPI_BOR,par_frame(par_depth)%this_comm,errno)
   if(iand(allstatus,pm_node_error)/=0) then
      do i=par_depth-1,1,-1
         call mpi_allreduce(allstatus,junk,1,MPI_INTEGER,&
              MPI_BOR,par_frame(i)%this_comm,errno)
      enddo
      allstatus=pm_node_error
   endif
   if(debug_mess) &
        write(*,*) 'sync_status_done',sys_node
  end function sync_status

  subroutine mesg_q_mess_finish()
    integer:: errno
    if(.not.pm_main_process) then
       call mpi_send('X',1,MPI_CHARACTER,&
            pm_main_process_no,pm_comm_tag,&
            pm_comm,errno)
    endif
  end subroutine mesg_q_mess_finish

  subroutine mesg_q_mess(str)
    character*(*):: str
    integer:: errno
    if(pm_main_process) then
       write(*,*) trim(str)
    else
       call mpi_send(str,len(str),MPI_CHARACTER,&
            pm_main_process_no,pm_comm_tag,&
            pm_comm,errno)
    endif
  end subroutine mesg_q_mess

  subroutine mesg_q_cleanup
    integer:: i,error,status(mpi_status_size)
    logical:: ok
    character(len=pm_comm_mess_len):: buffer
    integer:: errno
    logical:: has
    if(pm_main_process) then
       do i=0,sys_nnode-1
          has=.false.
          if(i/=pm_main_process_no) then
             do
                buffer=' '
                call mpi_recv(buffer,pm_comm_mess_len,&
                     MPI_CHARACTER,i,pm_comm_tag,&
                     pm_comm,status,errno)
                if(buffer=='X') exit
                if(.not.has) then
                   has=.true.
                   write(*,*)
                   write(*,*) '_______________________'
                   write(*,*)
                   write(*,*) 'Process: ',i
                   write(*,*) '_______________________'
                   write(*,*)
                endif
                write(*,*) trim(buffer)
            enddo
          endif
       enddo
    else
       call mesg_q_flush()
       call mesg_q_mess_finish()
    endif
  end subroutine mesg_q_cleanup

  subroutine mesg_q_print(context,s)
    type(pm_context),pointer:: context
    character,dimension(:),intent(in):: s
    type(pm_ptr):: q,sp
    integer:: n
    if(pm_main_process) then
       write(*,*) s
    else
       q=pm_fast_newnc(context,pm_pointer,2)
       q%data%ptr(q%offset)=pm_null_obj
       q%data%ptr(q%offset+1)=pm_null_obj
       if(pm_fast_isnull(print_first)) then
          print_first=q
          print_last=q
       else
          print_last%data%ptr(print_last%offset)=q
          print_last=q
       endif
       n=size(s)
       sp=pm_fast_newnc(context,pm_string,max(n,1))
       if(n==0) sp%data%s(sp%offset)=' '
       sp%data%s(sp%offset:sp%offset+n-1)=s
       q%data%ptr(q%offset+1)=sp
    endif
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
  end subroutine mesg_q_print
  
  
  subroutine mesg_q_print_str(context,s)
    type(pm_context),pointer:: context
    character(len=*),intent(in):: s
    type(pm_ptr):: q,sp
    integer:: n,i
    if(pm_main_process) then
       write(*,*) s
    else
       q=pm_fast_newnc(context,pm_pointer,2)
       q%data%ptr(q%offset)=pm_null_obj
       q%data%ptr(q%offset+1)=pm_null_obj
       if(pm_fast_isnull(print_first)) then
          print_first=q
          print_last=q
       else
          print_last%data%ptr(print_last%offset)=q
          print_last=q
       endif
       n=len(s)
       sp=pm_fast_newnc(context,pm_string,max(n,1))
       if(n==0) sp%data%s(sp%offset)=' '
       do i=1,n
          sp%data%s(sp%offset+i-1)=s(i:i)
       enddo
       q%data%ptr(q%offset+1)=sp
    endif
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
  end subroutine mesg_q_print_str
  
  subroutine mesg_q_flush()
    type(pm_ptr):: p,s
    integer:: errno
    p=print_first
    if(.not.pm_main_process) then
       do while(.not.pm_fast_isnull(p))
          s=p%data%ptr(p%offset+1)
          call mpi_send(s%data%s(s%offset:s%offset+pm_fast_esize(s)),&
               int(1+pm_fast_esize(s)),MPI_CHARACTER,&
               pm_main_process_no,pm_comm_tag,&
               pm_comm,errno)
          p=p%data%ptr(p%offset)
       enddo
    endif
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
  end subroutine mesg_q_flush

  function wshare(work,nnode,snode,nsnode) result(node)
    integer(pm_ln),intent(in):: nnode,snode,nsnode
    integer(pm_ln),dimension(nnode):: work
    integer(pm_ln):: node
    integer(pm_ln):: i,s,z
    s=sum(work(1:nnode))
    z=s*(2_pm_ln*snode+1_pm_ln)/(2_pm_ln*nsnode)
    do i=1,nnode
       z=z-work(i)
       if(z<=0) then
          node=i-1
          return
       endif
    enddo
    call pm_panic('wshare')
  end function wshare

  function pm_file_open(name,append,create,temp,excl,readable,&
       writeable,sequential,ierr) result(handle)
    type(pm_ptr),intent(in)::name
    logical,intent(in):: append,create,temp,excl,readable,&
         writeable,sequential
    integer,intent(out):: ierr
    integer:: handle
    integer:: mode,i,ierror,file_handle
    character(len=pm_max_filename_size):: fname
    mode=0
    if(append) mode=ior(mode,MPI_MODE_APPEND)
    if(create) mode=ior(mode,MPI_MODE_CREATE)
    if(excl) mode=ior(mode,MPI_MODE_EXCL)
    if(temp)   mode=ior(mode,MPI_MODE_DELETE_ON_CLOSE)
    !if(sequential) mode=ior(mode,MPI_MODE_SEQUENTIAL)
    if(readable.and.writeable) then
       mode=ior(mode,MPI_MODE_RDWR)
    elseif(.not.(readable.or.writeable)) then
       if(sequential) then
          mode=ior(mode,MPI_MODE_RDONLY)
       else
          mode=ior(mode,MPI_MODE_RDWR)
       endif
    else
       if(readable) mode=ior(mode,MPI_MODE_RDONLY)
       if(writeable) mode=ior(mode,MPI_MODE_WRONLY)
    endif
    fname=' '
    do i=0,min(pm_fast_esize(name),pm_max_filename_size)-1
       fname(i+1:i+1)=name%data%s(name%offset+i)
    enddo
    call mpi_file_open(par_frame(par_depth)%shared_comm,trim(fname),&
         mode,MPI_INFO_NULL,file_handle,ierror)
    ierr=ierror
    handle=file_handle
  contains
    include 'fesize.inc'
  end function pm_file_open

  subroutine pm_file_close(handle,ierr)
    integer,intent(inout):: handle
    integer,intent(out):: ierr
    call mpi_file_close(handle,ierr)
  end subroutine pm_file_close

  subroutine pm_file_seek(handle,offset,ierr)
    integer,intent(inout):: handle
    integer(pm_lln),intent(in):: offset
    integer,intent(out):: ierr
    call mpi_file_seek(handle,offset,mpi_seek_set,ierr)
  end subroutine pm_file_seek

  subroutine pm_file_read(handle,buffer,j,n,ierr)
    integer,intent(in):: handle
    integer(pm_ln),intent(in):: j,n
    type(pm_ptr),intent(in):: buffer
    integer,intent(out):: ierr
    integer:: m,mtype

    select case(pm_fast_vkind(buffer))
    case(pm_int)
       call get_mpi_type(pm_int,n,mtype,m)
       call mpi_file_read_all(handle,buffer%data%i(buffer%offset+j),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_long)
       call get_mpi_type(pm_long,n,mtype,m)
       call mpi_file_read_all(handle,buffer%data%ln(buffer%offset+j),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_longlong)
       call get_mpi_type(pm_longlong,n,mtype,m)
       call mpi_file_read_all(handle,buffer%data%lln(buffer%offset+j),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_int8)
       call get_mpi_type(pm_int8,n,mtype,m)
       call mpi_file_read_all(handle,buffer%data%i8(buffer%offset+j),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_int16)
       call get_mpi_type(pm_int16,n,mtype,m)
       call mpi_file_read_all(handle,buffer%data%i16(buffer%offset+j),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_int32)
       call get_mpi_type(pm_int32,n,mtype,m)
       call mpi_file_read_all(handle,buffer%data%i32(buffer%offset+j),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_int64)
       call get_mpi_type(pm_int64,n,mtype,m)
       call mpi_file_read_all(handle,buffer%data%i64(buffer%offset+j),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_single)
       call get_mpi_type(pm_single,n,mtype,m)
       call mpi_file_read_all(handle,buffer%data%r(buffer%offset+j),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_double)
       call get_mpi_type(pm_double,n,mtype,m)
       call mpi_file_read_all(handle,buffer%data%d(buffer%offset+j),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_single_complex)
       call get_mpi_type(pm_single_complex,n,mtype,m)
       call mpi_file_read_all(handle,buffer%data%c(buffer%offset+j),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_double_complex)
       call get_mpi_type(pm_double_complex,n,mtype,m)
       call mpi_file_read_all(handle,buffer%data%dc(buffer%offset+j),&
            m,mtype,mpi_status_ignore,ierr)
    end select

  contains
    include 'fvkind.inc'
    
  end subroutine pm_file_read

  subroutine pm_file_write(handle,buffer,j,n,ierr)
    integer,intent(in):: handle
    integer(pm_ln):: j,n
    type(pm_ptr),intent(in):: buffer
    integer,intent(out):: ierr
    integer:: m,mtype
    integer(pm_ln):: off

    if(par_frame(par_depth)%shared_node==0) then
       select case(pm_fast_vkind(buffer))
       case(pm_int)
          call get_mpi_type(pm_int,n,mtype,m)
          call mpi_file_write(handle,buffer%data%i(buffer%offset+j),&
               m,mtype,mpi_status_ignore,ierr)
       case(pm_long)
          call get_mpi_type(pm_long,n,mtype,m)
          call mpi_file_write(handle,buffer%data%ln(buffer%offset+j),&
               m,mtype,mpi_status_ignore,ierr)
       case(pm_longlong)
          call get_mpi_type(pm_long,n,mtype,m)
          call mpi_file_write(handle,buffer%data%lln(buffer%offset+j),&
               m,mtype,mpi_status_ignore,ierr)
       case(pm_int8)
          call get_mpi_type(pm_int8,n,mtype,m)
          call mpi_file_write(handle,buffer%data%i8(buffer%offset+j),&
               m,mtype,mpi_status_ignore,ierr)
       case(pm_int16)
          call get_mpi_type(pm_int16,n,mtype,m)
          call mpi_file_write(handle,buffer%data%i16(buffer%offset+j),&
               m,mtype,mpi_status_ignore,ierr)
       case(pm_int32)
          call get_mpi_type(pm_int32,n,mtype,m)
          call mpi_file_write(handle,buffer%data%i32(buffer%offset+j),&
               m,mtype,mpi_status_ignore,ierr)
       case(pm_int64)
          call get_mpi_type(pm_int64,n,mtype,m)
          call mpi_file_write(handle,buffer%data%i64(buffer%offset+j),&
               m,mtype,mpi_status_ignore,ierr)
       case(pm_single)
          call get_mpi_type(pm_single,n,mtype,m)
          call mpi_file_write(handle,buffer%data%r(buffer%offset+j),&
               m,mtype,mpi_status_ignore,ierr)
       case(pm_double)
          call get_mpi_type(pm_double,n,mtype,m)
          call mpi_file_write(handle,buffer%data%d(buffer%offset+j),&
               m,mtype,mpi_status_ignore,ierr)
       case(pm_single_complex)
          call get_mpi_type(pm_single_complex,n,mtype,m)
          call mpi_file_write(handle,buffer%data%c(buffer%offset+j),&
               m,mtype,mpi_status_ignore,ierr)
       case(pm_double_complex)
          call get_mpi_type(pm_double_complex,n,mtype,m)
          call mpi_file_write(handle,buffer%data%dc(buffer%offset+j),&
               m,mtype,mpi_status_ignore,ierr)
       end select
       if(ierr==mpi_success) call mpi_file_get_position(handle,off,ierr)
    endif

    call mpi_bcast(ierr,1,MPI_INTEGER,0,&
         par_frame(par_depth)%shared_comm,ierr)
    
    if(ierr/=mpi_success) return

    call mpi_bcast(off,1,MPI_OFFSET,0,&
         par_frame(par_depth)%shared_comm,ierr)
        
    if(par_frame(par_depth)%shared_node/=0) then
       call mpi_file_seek(handle,off,mpi_seek_set,ierr)
    endif
    
  contains
    include 'fvkind.inc'
    
  end subroutine pm_file_write

  subroutine pm_file_read_disps(handle,buffer,offsets,noff,totsize,ierr)
    integer,intent(in):: handle
    type(pm_ptr),intent(in):: buffer
    type(pm_ptr),intent(in):: offsets
    integer(pm_ln),intent(in):: noff,totsize
    integer,intent(out):: ierr
    integer(pm_p):: tno
    integer:: etype,ctype,mtype,size
    integer(pm_lln):: disp
    tno=pm_fast_vkind(buffer)
    etype=typ_for_pm(tno)
    call mpi_type_size(etype,size,ierr)
    call mpi_file_get_position(handle,disp,ierr)
    if(ierr/=mpi_success) return
    call get_mpi_disp_type(tno,offsets,0_pm_ln,noff,ctype)
    call mpi_type_create_resized(ctype,0_pm_ln,totsize*size,mtype,ierr)
    call mpi_file_set_view(handle,disp,etype,mtype,"native",mpi_info_null,ierr)
    if(ierr/=mpi_success) return
    call pm_file_read(handle,buffer,0_pm_ln,noff,ierr)
    if(ierr/=mpi_success) return
    call mpi_file_set_view(handle,0_pm_ln,MPI_BYTE,MPI_BYTE,"native",mpi_info_null,ierr)
    if(ierr/=mpi_success) return
    disp=disp+totsize*size
    call mpi_file_seek(handle,disp,mpi_seek_set,ierr)
    if(ierr/=mpi_success) return
  contains
    include 'fvkind.inc'
  end subroutine pm_file_read_disps

  subroutine pm_file_write_disps(handle,buffer,offsets,noff,totsize,ierr)
    integer,intent(inout):: handle
    type(pm_ptr),intent(in):: buffer
    type(pm_ptr),intent(in):: offsets
    integer(pm_ln),intent(in):: noff,totsize
    integer,intent(out):: ierr
    integer(pm_p):: tno
    integer:: etype,ctype,mtype,size,m
    integer(pm_lln):: disp
    tno=pm_fast_vkind(buffer)
    etype=typ_for_pm(tno)
    call mpi_type_size(etype,size,ierr)
    call mpi_file_get_position(handle,disp,ierr)
    call get_mpi_disp_type(tno,offsets,0_pm_ln,noff,ctype)
    call mpi_type_create_resized(ctype,0_pm_ln,totsize*size,mtype,ierr)
    call mpi_file_set_view(handle,disp,etype,mtype,"native",mpi_info_null,ierr)
    select case(pm_fast_vkind(buffer))
    case(pm_int)
       call get_mpi_type(pm_int,noff,mtype,m)
       call mpi_file_write_all(handle,buffer%data%i(buffer%offset),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_long)
       call get_mpi_type(pm_long,noff,mtype,m)
       call mpi_file_write_all(handle,buffer%data%ln(buffer%offset),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_longlong)
       call get_mpi_type(pm_long,noff,mtype,m)
       call mpi_file_write_all(handle,buffer%data%lln(buffer%offset),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_int8)
       call get_mpi_type(pm_int8,noff,mtype,m)
       call mpi_file_write_all(handle,buffer%data%i8(buffer%offset),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_int16)
       call get_mpi_type(pm_int16,noff,mtype,m)
       call mpi_file_write_all(handle,buffer%data%i16(buffer%offset),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_int32)
       call get_mpi_type(pm_int32,noff,mtype,m)
       call mpi_file_write_all(handle,buffer%data%i32(buffer%offset),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_int64)
       call get_mpi_type(pm_int64,noff,mtype,m)
       call mpi_file_write_all(handle,buffer%data%i64(buffer%offset),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_single)
       call get_mpi_type(pm_single,noff,mtype,m)
       call mpi_file_write_all(handle,buffer%data%r(buffer%offset),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_double)
       call get_mpi_type(pm_double,noff,mtype,m)
       call mpi_file_write_all(handle,buffer%data%d(buffer%offset),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_single_complex)
       call get_mpi_type(pm_single_complex,noff,mtype,m)
       call mpi_file_write_all(handle,buffer%data%c(buffer%offset),&
            m,mtype,mpi_status_ignore,ierr)
    case(pm_double_complex)
       call get_mpi_type(pm_double_complex,noff,mtype,m)
       call mpi_file_write_all(handle,buffer%data%dc(buffer%offset),&
            m,mtype,mpi_status_ignore,ierr)
    end select
    if(ierr/=mpi_success) return
    call mpi_file_set_view(handle,0_pm_ln,MPI_BYTE,MPI_BYTE,"native",mpi_info_null,ierr)
    if(ierr/=mpi_success) return
    disp=disp+totsize*size
    call mpi_file_seek(handle,disp,mpi_seek_set,ierr)
    if(ierr/=mpi_success) return
  contains
    include 'fvkind.inc'
  end subroutine pm_file_write_disps

  subroutine pm_file_error_string(v,n,str)
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: n
    character(len=*),intent(out):: str
    character(len=mpi_max_error_string):: mess
    integer:: length,ierr
    call mpi_error_string(v%data%i(v%offset+n),mess,length,ierr)
    str=mess(index(mess,':')+1:)
  end subroutine  pm_file_error_string
  
end module pm_parlib
