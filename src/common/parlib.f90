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

!==================================================================
! The following routines provide support for parallel operations
!==================================================================

module pm_parlib
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_array
  use mpi
  implicit none

  ! Stack entry with details of (possibly nested) parallel statement
  type prc_info_frame
     integer:: this_prc          ! Rank in communcating group
     integer:: this_nprc         ! Num procs in communicating group 
     integer:: this_comm         ! MPI communicator for comm. group.
     integer:: shared_prc        ! Rank in shared group
                                 ! (running the same invocation)
     integer:: shared_nprc       ! Number of process in shared group
     integer:: shared_comm       ! MPI communicator for shared group
     logical:: is_shared         ! Process is part of group, some of which share
     integer,dimension(:),allocatable:: sharemap
  end type prc_info_frame

  ! Stack of prc_info_frame structures
  integer,parameter:: max_prc_depth=128
  type(prc_info_frame),dimension(max_prc_depth):: prc_frame
  integer:: prc_depth

  ! Maximum message sizes
  integer,parameter:: max_message_size=(huge(1)/2)+1
  integer,parameter:: exchange_block= 16*1024*1024
  integer,parameter:: pm_prc_block = 64

  ! Status of an MPI node
  integer,parameter:: pm_prc_finished=0
  integer,parameter:: pm_prc_running=1
  integer,parameter:: pm_prc_error=2
  integer:: conc_depth
  integer:: sys_prc,sys_nprc   ! Rank in MPI_COMM_WORLD

  ! MPI types for PM types
  integer,dimension(pm_int:pm_string):: typ_for_pm,block_typ_for_pm,&
       large_typ_for_pm
  integer(pm_ln),dimension(pm_int:pm_string):: large_typ_size 

  ! Stacks of pending messages
  integer,parameter:: max_messages=1024
  integer,dimension(:),allocatable:: message_stack,message_type,message_action
  integer:: message_top,message_type_top,message_action_top
  integer:: message_stack_size,message_type_stack_size,message_action_stack_size
  logical:: message_actions
  type(pm_root),pointer:: action_ptrs
  integer(pm_ln),allocatable,dimension(:):: action_start,action_size
  integer(pm_ln):: action_stack_size,action_top

  ! Data tags
  integer,parameter:: req_tag=1
  integer,parameter:: data_tag=2
  integer,parameter:: extra_tag=3

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
    prc_depth=1
    call mpi_comm_size(MPI_COMM_WORLD,sys_nprc,error)
    call mpi_comm_rank(MPI_COMM_WORLD,sys_prc,error)
    prc_frame(prc_depth)%this_nprc=sys_nprc
    prc_frame(prc_depth)%this_prc=sys_prc
    prc_frame(prc_depth)%this_comm=MPI_COMM_WORLD
    prc_frame(prc_depth)%shared_comm=MPI_COMM_WORLD
    prc_frame(prc_depth)%shared_prc=prc_frame(prc_depth)%this_prc
    prc_frame(prc_depth)%shared_nprc=prc_frame(prc_depth)%this_nprc
    conc_depth=0

    ! Initialise MPI types array
    typ_for_pm(pm_int)=MPI_INTEGER
    typ_for_pm(pm_long)=MPI_AINT
    typ_for_pm(pm_longlong)=MPI_OFFSET
    typ_for_pm(pm_single)=MPI_REAL
    typ_for_pm(pm_double)=MPI_DOUBLE
    typ_for_pm(pm_logical)=MPI_LOGICAL

    large_typ_for_pm = MPI_DATATYPE_NULL
    block_typ_for_pm = MPI_DATATYPE_NULL

    message_top=0
    message_type_top=0
    message_action_top=0
    action_top=0
    message_stack_size=max_messages
    message_type_stack_size=max_messages
    message_action_stack_size=max_messages
    action_stack_size=max_messages
    allocate(message_stack(max_messages),message_type(max_messages))
    allocate(message_action(max_messages))
    allocate(action_start(max_messages),action_size(max_messages))
    action_ptrs=>pm_new_as_root(context,pm_pointer,2_pm_ln)
    v=pm_assign_new(context,action_ptrs%ptr,0_pm_ln,&
         pm_pointer,int(max_messages,pm_ln),.true.)
    v=pm_assign_new(context,action_ptrs%ptr,1_pm_ln,&
         pm_pointer,int(max_messages,pm_ln),.true.)
    
    ! Initialise error communicator
    call mpi_comm_dup(MPI_COMM_WORLD,pm_comm,error)

    ! Initialise print queue
    q_reg=>pm_register(context,'parlib',print_first,print_last)
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
    call pm_delete_root(context,action_ptrs)
    call pm_delete_register(context,q_reg)
  end subroutine finalise_par

  ! Create a new communicating group using MPI split
  subroutine push_prc_split(context,split) 
    type(pm_context),pointer:: context
    integer,intent(in):: split
    integer:: error,newcomm
    integer:: this_comm
    this_comm=prc_frame(prc_depth)%this_comm
    call mpi_comm_split(this_comm,split,prc_frame(prc_depth)%this_prc,&
         newcomm,error)
    if(error/=MPI_SUCCESS)&
         call pm_panic('comm split failed')
    prc_depth=prc_depth+1
    if(prc_depth>max_prc_depth) call pm_panic('too many nested for/do/find')

    prc_frame(prc_depth)%this_prc=prc_frame(prc_depth-1)%shared_prc
    prc_frame(prc_depth)%this_nprc=prc_frame(prc_depth-1)%shared_nprc
    prc_frame(prc_depth)%this_comm=prc_frame(prc_depth-1)%shared_comm
    prc_frame(prc_depth)%is_shared=.true.
    call mpi_comm_size(newcomm,prc_frame(prc_depth)%shared_nprc,error)
    call mpi_comm_rank(newcomm,prc_frame(prc_depth)%shared_prc,error)
    prc_frame(prc_depth)%shared_comm=newcomm
    if(pm_debug_level>4) then
       write(*,*) 'split:',prc_frame(prc_depth)%this_comm,&
            prc_frame(prc_depth)%shared_comm
    endif
  contains
    include 'fnewnc.inc'
  end subroutine   push_prc_split

  ! New frame used former shared communicator
  subroutine push_prc_distr(context)
    type(pm_context),pointer:: context
    prc_depth=prc_depth+1
    prc_frame(prc_depth)%this_prc=prc_frame(prc_depth-1)%shared_prc
    prc_frame(prc_depth)%this_nprc=prc_frame(prc_depth-1)%shared_nprc
    prc_frame(prc_depth)%this_comm=prc_frame(prc_depth-1)%shared_comm
    prc_frame(prc_depth)%shared_comm=MPI_COMM_SELF
    prc_frame(prc_depth)%shared_nprc=1
    prc_frame(prc_depth)%shared_prc=0
    prc_frame(prc_depth)%is_shared=.false.
  end subroutine push_prc_distr

  ! Get cartesian dimensions 
  subroutine get_dims(shared_nprc,ndims,dims)
    integer,intent(in):: ndims
    integer,dimension(ndims),intent(inout):: dims
    integer,intent(in):: shared_nprc
    integer:: error
    if(shared_nprc>1) then
       call mpi_dims_create(shared_nprc,ndims,dims,error)
       if(error/=MPI_SUCCESS) &
            call pm_panic('cannot create dims')
    else
       dims=1
    endif
  end subroutine get_dims

  !  Create a new communicating group with grid topology
  subroutine push_prc_grid(context,periods,ndims,dims)
    type(pm_context),pointer:: context
    integer,intent(in):: ndims
    logical,dimension(ndims),intent(in):: periods
    integer,dimension(ndims),intent(in):: dims
    type(pm_ptr):: ptr
    integer:: error,newcomm
    integer:: shared_nprc
    shared_nprc=prc_frame(prc_depth)%shared_nprc
    if(shared_nprc>1) then
       call mpi_cart_create(prc_frame(prc_depth)%shared_comm,ndims,dims,&
            periods,.true.,newcomm,error)
       if(error/=MPI_SUCCESS) &
            call pm_panic('cannot create cart comm')
       prc_depth=prc_depth+1
       if(prc_depth>max_prc_depth) call pm_panic('too many nested for/do/find')
       prc_frame(prc_depth)%this_comm=newcomm
       call mpi_comm_size(newcomm,prc_frame(prc_depth)%this_nprc,error)
       call mpi_comm_rank(newcomm,prc_frame(prc_depth)%this_prc,error)
       prc_frame(prc_depth)%shared_comm=MPI_COMM_SELF
       prc_frame(prc_depth)%shared_nprc=1
       prc_frame(prc_depth)%shared_prc=0
       prc_frame(prc_depth)%is_shared=.false.
       if(pm_debug_level>4) then
          write(*,*) 'push grid',prc_depth,dims(1:ndims)
       endif
    else
       if(pm_debug_level>4) then
          write(*,*) 'push_grid_conc',prc_depth,dims(1:ndims)
       endif
       conc_depth=conc_depth+1
    endif
  contains
    include 'fnewnc.inc'
  end subroutine  push_prc_grid

  ! Pop communicating group - revert to next outer layer
  subroutine pop_prc(context)
    type(pm_context),pointer:: context
    integer:: error,old_shared,old_this
    if(conc_depth>0) then
       conc_depth=conc_depth-1
    else
       if(allocated(prc_frame(prc_depth)%sharemap)) then
          deallocate(prc_frame(prc_depth)%sharemap)
       endif
       old_shared=prc_frame(prc_depth)%shared_comm
       old_this=prc_frame(prc_depth)%this_comm
       prc_depth=prc_depth-1
       if(old_this/=MPI_COMM_SELF.and.&
            prc_frame(prc_depth)%shared_comm/=old_this) then
          call mpi_comm_free(old_this,error)
       endif
       if(old_shared/=MPI_COMM_SELF) then
          call mpi_comm_free(old_shared,error)
       endif
    endif
  end subroutine pop_prc

  ! Broadcast data (v) from prc to all other nodes in current group (in place)
  recursive subroutine broadcast(context,prc,v)
    type(pm_context),pointer:: context
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v
    integer:: comm
    integer:: i,m,tno,errno
    integer(pm_ln):: j
    type(pm_ptr):: avec,w
    if(pm_debug_level>4) then
       write(*,*) 'BCAST',prc,sys_prc
    endif
    if(prc==MPI_PROC_NULL) return
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call broadcast(context,prc,v%data%ptr(v%offset+pm_array_dom))
       call broadcast(context,prc,v%data%ptr(v%offset+pm_array_length))
       w=v%data%ptr(v%offset+pm_array_vect)
       do j=0,pm_fast_esize(w)
          call pm_ptr_assign(context,w,j,&
               broadcast_val(context,prc,w%data%ptr(w%offset+j)))
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call broadcast(context,prc,v%data%ptr(v%offset+i))
       enddo
    case(pm_pointer)
       do j=0,pm_fast_esize(v)
         call pm_ptr_assign(context,v,j,&
               broadcast_val(context,prc,v%data%ptr(v%offset+j)))
       enddo
    case(pm_int)
       call get_mpi_type(pm_int,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%i(v%offset),m,&
            tno,prc,comm,errno)
    case(pm_long)
       call get_mpi_type(pm_long,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%ln(v%offset),m,&
            tno,prc,comm,errno)
    case(pm_single)
       call get_mpi_type(pm_single,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%r(v%offset),m,&
            tno,prc,comm,errno)
    case(pm_double)
       call get_mpi_type(pm_double,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%d(v%offset),m,&
            tno,prc,comm,errno)
    case(pm_logical)
       call get_mpi_type(pm_logical,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_bcast(v%data%l(v%offset),m,&
            tno,prc,comm,errno)
    end select
    if(pm_debug_level>4) then
       write(*,*) 'BCAST DONE'
    endif
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine broadcast
 
  ! Broadcast data (v) from prc to all other nodes in current group
  recursive function broadcast_val(context,prc,v) result(ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v
    type(pm_ptr):: ptr
    integer:: comm
    integer:: i,m,tno,errno
    integer(pm_ln),dimension(3):: hdr
    integer(pm_ln):: esize,j
    type(pm_ptr):: avec,w
    type(pm_root),pointer:: root
    logical:: this_prc
    
    if(pm_debug_level>4) then
       write(*,*) 'BCAST VAL',prc,sys_prc
    endif
    if(prc==MPI_PROC_NULL) return
    comm=prc_frame(prc_depth)%this_comm
    this_prc=prc_frame(prc_depth)%this_prc==prc
    ! Send header: type/size/pm_type
    hdr(1)=pm_fast_typeof(v)
    hdr(2)=pm_fast_esize(v)
    if(pm_fast_vkind(v)==pm_usr) then
       hdr(3)=v%data%ptr(v%offset+1)%offset
    endif
    call mpi_bcast(hdr,3,MPI_AINT,prc,comm,errno)
    tno=hdr(1)
    esize=hdr(2)
    select case(tno)
    case(pm_null)
       ptr=pm_null_obj
    case(pm_array_type,pm_const_array_type)
       root=>pm_new_as_root(context,pm_usr,int(pm_array_size,pm_ln))
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)%offset=pm_array_type
       ptr%data%ptr(ptr%offset+1)%offset=hdr(3)
       call pm_ptr_assign(context,ptr,int(pm_array_dom,pm_ln),&
            broadcast_val(context,prc,v%data%ptr(v%offset+pm_array_dom)))
       call pm_ptr_assign(context,ptr,int(pm_array_length,pm_ln),&
            broadcast_val(context,prc,v%data%ptr(v%offset+pm_array_length)))
       esize=pm_fast_esize(ptr%data%ptr(ptr%offset+pm_array_length))
       w=pm_assign_new(context,ptr,int(pm_array_offset,pm_ln),&
            pm_long,esize+1,.true.)
       w=v%data%ptr(v%offset+pm_array_vect)
       avec=pm_assign_new(context,ptr,int(pm_array_vect,pm_ln),&
            pm_pointer,esize+1,.true.)
       do j=0,esize
          call pm_ptr_assign(context,avec,j,&
               broadcast_val(context,prc,w%data%ptr(w%offset+j)))
       enddo
       call pm_delete_root(context,root)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       root=>pm_new_as_root(context,pm_usr,esize+1)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)%offset=tno
       ptr%data%ptr(ptr%offset+1)%offset=hdr(3)
       do j=2,esize
          call pm_ptr_assign(context,ptr,j,&
               broadcast_val(context,prc,v%data%ptr(v%offset+j)))
       enddo
       call pm_delete_root(context,root)
    case(pm_pointer)
       root=>pm_new_as_root(context,pm_pointer,esize+1)
       ptr=root%ptr
       do j=0,esize
          call pm_ptr_assign(context,ptr,j,&
               broadcast_val(context,prc,v%data%ptr(v%offset+i)))
       enddo
       call pm_delete_root(context,root)
    case(pm_int)
       ptr=pm_new(context,pm_int,esize+1)
       if(this_prc) ptr%data%i(ptr%offset:ptr%offset+esize)=&
            v%data%i(v%offset:v%offset+esize)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%i(ptr%offset),m,&
            tno,prc,comm,errno)
    case(pm_long)
       ptr=pm_new(context,pm_long,esize+1)
       if(this_prc) ptr%data%ln(ptr%offset:ptr%offset+esize)=&
            v%data%ln(v%offset:v%offset+esize)
       call get_mpi_type(pm_long,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%ln(ptr%offset),m,&
            tno,prc,comm,errno)
    case(pm_single)
       ptr=pm_new(context,pm_single,esize+1)
       if(this_prc) ptr%data%r(ptr%offset:ptr%offset+esize)=&
            v%data%r(v%offset:v%offset+esize)
       call get_mpi_type(pm_single,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%r(ptr%offset),m,&
            tno,prc,comm,errno)
    case(pm_double)
       ptr=pm_new(context,pm_double,esize+1)
       if(this_prc) ptr%data%d(ptr%offset:ptr%offset+esize)=&
            v%data%d(v%offset:v%offset+esize)
       call get_mpi_type(pm_double,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%d(ptr%offset),m,&
            tno,prc,comm,errno)
    case(pm_logical)
       ptr=pm_new(context,pm_logical,esize+1)
       if(this_prc) ptr%data%l(ptr%offset:ptr%offset+esize)=&
            v%data%l(v%offset:v%offset+esize)
       call get_mpi_type(pm_logical,esize+1_pm_ln,tno,m)
       call mpi_bcast(ptr%data%l(ptr%offset),m,&
            tno,prc,comm,errno)
    end select
    if(pm_debug_level>4) then
       write(*,*) 'BCAST VAL DONE'
    endif
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function  broadcast_val
  
  ! Gather data from all processes
  recursive subroutine gather(context,v,w,j)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,w
    integer:: comm
    integer:: i,m,tno,m2,tno2,errno
    integer(pm_ln),intent(in):: j
    type(pm_ptr):: avec
    integer:: k
    if(pm_debug_level>4) then
       write(*,*) 'GATHER',sys_prc
    endif
    comm=prc_frame(prc_depth)%this_comm
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
               v%data%ptr(v%offset+pm_array_vect)))
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call gather(context,v%data%ptr(v%offset+i),w%data%ptr(w%offset+i),j)
       enddo
    case(pm_pointer)
       do k=0,j
          call pm_ptr_assign(context,&
               v,int(k,pm_ln),&
               broadcast_val(context,k,v))
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
    if(pm_debug_level>4) then
       write(*,*) 'GATHERED',sys_prc
        
    endif
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine gather

  ! Synchronous send of tagged data
  recursive subroutine send_val(prc,v,start,siz,mess_tag)
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: start,siz
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,m,tno,errno
    integer(pm_ln):: j,esize
    type(pm_ptr):: len,off,avec
    integer(pm_ln),dimension(3):: hdr
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    ! Send header: type/size/pm_type
    hdr(1)=tno
    esize=pm_fast_esize(v)
    if(pm_fast_vkind(v)/=pm_usr.and.siz>0) esize=siz
    hdr(2)=esize
    if(pm_fast_vkind(v)==pm_usr) then
       hdr(3)=v%data%ptr(v%offset+1)%offset
    endif
    if(pm_debug_level>4) then
       write(*,*)'SEND VAL',sys_prc,prc,tno,esize,mess_tag
    endif
    call mpi_send(hdr,3,MPI_AINT,prc,mess_tag,comm,errno)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call send_val(prc,v%data%ptr(v%offset+pm_array_dom),start,siz,mess_tag)
       len=v%data%ptr(v%offset+pm_array_length)
       call send_val(prc,len,start,siz,mess_tag)
       off=v%data%ptr(v%offset+pm_array_offset)
       avec=v%data%ptr(v%offset+pm_array_vect)
       if(siz>0) then
          esize=start+siz-1
       else
          esize=pm_fast_esize(avec)
       endif
       do j=start,esize
          if(len%data%ln(len%offset)>0) then
             call send_val(prc,avec%data%ptr(avec%offset+j),&
                  off%data%ln(off%offset+j),&
                  len%data%ln(len%offset+j),mess_tag)
          endif
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call send_val(prc,v%data%ptr(v%offset+i),start,siz,mess_tag)
       enddo
    case(pm_pointer)
       if(siz>0) then
          esize=start+siz-1
       else
          esize=pm_fast_esize(v)
       endif
       do j=start,esize
          call send_val(prc,v%data%ptr(v%offset+j),&
               0_pm_ln,-1_pm_ln,mess_tag)
       enddo
    case(pm_int)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_send(v%data%i(v%offset+start),m,&
            tno,prc,mess_tag,comm,errno)
    case(pm_long)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_send(v%data%ln(v%offset+start),m,&
            tno,prc,mess_tag,comm,errno)
    case(pm_single)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)       
       call mpi_send(v%data%r(v%offset+start),m,&
            tno,prc,mess_tag,comm,errno)
    case(pm_double)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_send(v%data%d(v%offset+start),m,&
            tno,prc,mess_tag,comm,errno)
    case(pm_logical)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_send(v%data%l(v%offset+start),m,&
            tno,prc,mess_tag,comm,errno)
    end select
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine send_val

  ! Receive all contents for vector of arrays or poly vector
  subroutine recv_all(context,v,off,offstart,noff,prc,mess_tag)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln),intent(in):: offstart,noff
    integer,intent(in):: prc,mess_tag
    integer(pm_ln):: i,j
    type(pm_ptr):: len,vec
    if(pm_fast_vkind(v)==pm_pointer) then
       do i=offstart,offstart+noff-1
          j=off%data%ln(off%offset+j)
          call pm_ptr_assign(context,v,&
               j,recv_val(context,prc,mess_tag))
       enddo
    else
       len=v%data%ptr(v%offset+pm_array_length)
       vec=v%data%ptr(v%offset+pm_array_vect)
       do i=offstart,offstart+noff-1
          j=off%data%ln(off%offset+i)
          if(len%data%ln(len%offset+j)>0) then
             call pm_ptr_assign(context,vec,&
                  j,recv_val(context,prc,mess_tag))
          endif
       enddo
    endif
  contains
    include 'fvkind.inc'
  end subroutine recv_all

  recursive subroutine recv(context,prc,v,off,offstart,noff,mess_tag)
    type(pm_context),pointer:: context
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln):: offstart,noff
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,tno,errno
    integer(pm_ln):: j,k
    type(pm_ptr):: len,avec
    character,dimension(1):: buffer
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call recv(context,prc,v%data%ptr(v%offset+pm_array_dom),&
            off,offstart,noff,mess_tag)
       len=v%data%ptr(v%offset+pm_array_length)
       call recv(context,prc,len,off,offstart,noff,mess_tag)
       avec=v%data%ptr(v%offset+pm_array_vect)
       do j=offstart,noff-1
          k=off%data%ln(off%offset+offstart+j)
          if(len%data%ln(len%offset+k)>0) then
             call pm_ptr_assign(context,avec,k,recv_val(context,prc,extra_tag))
          endif
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call recv(context,prc,v%data%ptr(v%offset+i),off,offstart,noff,mess_tag)
       enddo
    case(pm_pointer)
       call mpi_recv(buffer,1,MPI_CHARACTER,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       do j=offstart,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call pm_ptr_assign(context,avec,k,recv_val(context,prc,extra_tag))
       enddo
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,offstart,noff,tno)
       call mpi_recv(v%data%i(v%offset),1,&
            tno,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_long)
       call get_mpi_disp_type(pm_long,off,offstart,noff,tno)
       call mpi_recv(v%data%ln(v%offset),1,&
            tno,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,offstart,noff,tno)
       call mpi_recv(v%data%r(v%offset),1,&
            tno,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,offstart,noff,tno)
       call mpi_recv(v%data%d(v%offset),1,&
            tno,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,offstart,noff,tno)
       call mpi_recv(v%data%l(v%offset),1,&
            tno,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
       call mpi_type_free(tno,errno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine recv

  ! Receive tagged data
  recursive function recv_val(context,prc,mess_tag) result(ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: prc,mess_tag
    type(pm_ptr):: ptr
    integer:: comm
    integer:: i,m,tno,errno
    integer(pm_ln),dimension(3):: hdr
    integer(pm_ln):: esize,j
    type(pm_ptr):: avec,w,len
    type(pm_root),pointer:: root
    
    if(pm_debug_level>4) then
       write(*,*) sys_prc,'RECV VAL',prc,mess_tag
    endif
    
    if(prc==MPI_PROC_NULL) return
    comm=prc_frame(prc_depth)%this_comm
    
    ! recv header: type/size/pm_type
    call mpi_recv(hdr,3,MPI_AINT,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    tno=hdr(1)
    esize=hdr(2)
    if(pm_debug_level>4) then
       write(*,*) sys_prc,'RECV_VAL',tno,esize
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
            recv_val(context,prc,mess_tag))
       call pm_ptr_assign(context,ptr,int(pm_array_length,pm_ln),&
            recv_val(context,prc,mess_tag))
       len=ptr%data%ptr(ptr%offset+pm_array_length)
       w=pm_assign_new(context,ptr,int(pm_array_offset,pm_ln),&
            pm_long,esize+1,.true.)
       avec=pm_assign_new(context,ptr,int(pm_array_vect,pm_ln),&
            pm_pointer,esize+1,.true.)
       do j=0,esize
          if(len%data%ln(len%offset)>0) then
             call pm_ptr_assign(context,avec,j,&
                  recv_val(context,prc,mess_tag))
          endif
       enddo
       call pm_delete_root(context,root)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       root=>pm_new_as_root(context,pm_usr,esize+1)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)%offset=tno
       ptr%data%ptr(ptr%offset+1)%offset=hdr(3)
       do j=2,esize
          call pm_ptr_assign(context,ptr,j,&
               recv_val(context,prc,mess_tag))
       enddo
       call pm_delete_root(context,root)
    case(pm_pointer)
       root=>pm_new_as_root(context,pm_pointer,esize+1)
       ptr=root%ptr
       do j=0,esize
          call pm_ptr_assign(context,ptr,j,&
               recv_val(context,prc,mess_tag))
       enddo
       call pm_delete_root(context,root)
    case(pm_int)
       ptr=pm_new(context,pm_int,esize+1)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%i(ptr%offset),m,&
            tno,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_long)
       ptr=pm_new(context,pm_long,esize+1)
       call get_mpi_type(pm_long,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%ln(ptr%offset),m,&
            tno,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_single)
       ptr=pm_new(context,pm_single,esize+1)
       call get_mpi_type(pm_single,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%r(ptr%offset),m,&
            tno,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_double)
       ptr=pm_new(context,pm_double,esize+1)
       call get_mpi_type(pm_double,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%d(ptr%offset),m,&
            tno,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    case(pm_logical)
       ptr=pm_new(context,pm_logical,esize+1)
       call get_mpi_type(pm_logical,esize+1_pm_ln,tno,m)
       call mpi_recv(ptr%data%l(ptr%offset),m,&
            tno,prc,mess_tag,comm,MPI_STATUS_IGNORE,errno)
    end select
    if(pm_debug_level>4) then
       write(*,*) 'RECV VAL DONE'
    endif
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function recv_val

   ! Asynchronous send of tagged data
  recursive subroutine isend_val(prc,v,start,siz,mess_tag)
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: start,siz
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,m,tno,mess,errno
    integer(pm_ln):: j,esize
    type(pm_ptr):: len,off,avec
    integer(pm_ln),dimension(3):: hdr
    character(len=1):: buffer
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    ! Send header: type/size/pm_type
    esize=pm_fast_esize(v)
    if(pm_fast_vkind(v)/=pm_usr.and.siz>0) esize=siz-1
    hdr(1)=tno
    hdr(2)=pm_fast_esize(v)
    if(pm_fast_vkind(v)==pm_usr) then
       hdr(3)=v%data%ptr(v%offset+1)%offset
    endif
    call mpi_isend(hdr,3,MPI_AINT,prc,comm,mess_tag,mess,errno)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call isend_val(prc,v%data%ptr(v%offset+pm_array_dom),start,siz,mess_tag)
       len=v%data%ptr(v%offset+pm_array_length)
       call isend_val(prc,len,start,siz,mess_tag)
       off=v%data%ptr(v%offset+pm_array_offset)
       avec=v%data%ptr(v%offset+pm_array_vect)
       if(siz>0) then
          esize=start+siz-1
       else
          esize=pm_fast_esize(avec)
       endif
       do j=start,esize
          if(len%data%ln(len%offset)>0) then
             call isend_val(prc,avec%data%ptr(avec%offset+j),&
                  off%data%ln(off%offset+j),&
                  len%data%ln(len%offset+j),mess_tag)
          endif
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call isend_val(prc,v%data%ptr(v%offset+i),start,siz,mess_tag)
       enddo
    case(pm_pointer)
       call mpi_isend(buffer,1,MPI_CHARACTER,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
       if(siz>0) then
          esize=start+siz-1
       else
          esize=pm_fast_esize(v)
       endif
       do j=start,esize
          call isend_val(prc,v%data%ptr(v%offset+j),&
               0_pm_ln,-1_pm_ln,mess_tag)
       enddo
    case(pm_int)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%i(v%offset+start),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_long)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%ln(v%offset+start),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_single)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)       
       call mpi_isend(v%data%r(v%offset+start),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_double)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%d(v%offset+start),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_logical)
       call get_mpi_type(pm_int,esize+1_pm_ln,tno,m)
       call mpi_isend(v%data%l(v%offset+start),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'
  end subroutine isend_val
    
  ! Send from dispersed buffer v[off]
  recursive subroutine isend(prc,v,off,offstart,noff,mess_tag)
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln),intent(in):: noff,offstart
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,mess,tno,errno
    integer(pm_ln):: j,k
    type(pm_ptr):: avec,len
    character,dimension(1):: buffer
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call isend(prc,v%data%ptr(v%offset+pm_array_dom),off,offstart,noff,mess_tag)
       len=v%data%ptr(v%offset+pm_array_length)
       call isend(prc,len,off,offstart,noff,mess_tag)
       avec=v%data%ptr(v%offset+pm_array_vect)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          if(len%data%ln(len%offset+k)>0) then
             call isend_val(prc,&
                  avec%data%ptr(avec%offset+k),&
                  0_pm_ln,-1_pm_ln,extra_tag)
          endif
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call isend(prc,v%data%ptr(v%offset+i),off,offstart,noff,mess_tag)
       enddo
    case(pm_pointer)
       call mpi_isend(buffer,1,MPI_CHARACTER,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
       do j=0,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call isend_val(prc,v%data%ptr(v%offset+k),0_pm_ln,-1_pm_ln,extra_tag)
       enddo
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,offstart,noff,tno)
       call mpi_isend(v%data%i(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_long)
       call get_mpi_disp_type(pm_long,off,offstart,noff,tno)
       call mpi_isend(v%data%ln(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,offstart,noff,tno)
       call mpi_isend(v%data%r(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,offstart,noff,tno)
       call mpi_isend(v%data%d(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,offstart,noff,tno)
       call mpi_isend(v%data%l(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine isend
  
  ! Recieve into dispersed buffer v[off]
  recursive subroutine irecv(context,prc,v,off,offstart,noff,mess_tag)
    type(pm_context),pointer:: context
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln):: offstart,noff
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,mess,tno,errno
    integer(pm_ln):: start1,finish1
    type(pm_ptr):: avec,len
    character,dimension(1):: buffer
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call irecv(context,prc,&
            v%data%ptr(v%offset+pm_array_dom),&
            off,offstart,noff,mess_tag)
       call irecv(context,prc,&
            v%data%ptr(v%offset+pm_array_length),&
            off,offstart,noff,mess_tag)
       call defer_recv(context,prc,v,off,&
            offstart,noff,mess_tag)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call irecv(context,prc,v%data%ptr(v%offset+i),&
               off,offstart,noff,mess_tag)
       enddo
    case(pm_pointer)
       call mpi_irecv(buffer,1,MPI_CHARACTER,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
       call defer_recv(context,prc,v,off,offstart,noff,mess_tag)
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,offstart,noff,tno)
       call mpi_irecv(v%data%i(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_long)
       call get_mpi_disp_type(pm_long,off,offstart,noff,tno)
       call mpi_irecv(v%data%ln(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,offstart,noff,tno)
       call mpi_irecv(v%data%r(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,offstart,noff,tno)
       call mpi_irecv(v%data%d(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,offstart,noff,tno)
       call mpi_irecv(v%data%l(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine irecv

  ! Send from dispersed buffer v[off]
  recursive subroutine rsend(prc,v,off,offstart,noff,mess_tag)
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln):: offstart,noff
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,tno,errno
    integer(pm_ln):: j,k
    type(pm_ptr):: len,avec
    character,dimension(1):: buffer
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call rsend(prc,v%data%ptr(v%offset+pm_array_dom),off,offstart,noff,mess_tag)
       len=v%data%ptr(v%offset+pm_array_length)
       call rsend(prc,len,off,offstart,noff,mess_tag)
       avec=v%data%ptr(v%offset+pm_array_vect)
       do j=offstart,noff-1
          k=off%data%ln(off%offset+offstart+j)
          if(len%data%ln(len%offset+k)>0) then
             call send_val(prc,avec%data%ptr(avec%offset+k),&
                  0_pm_ln,-1_pm_ln,extra_tag)
          endif
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call rsend(prc,v%data%ptr(v%offset+i),off,offstart,noff,mess_tag)
       enddo
    case(pm_pointer)
       call mpi_rsend(buffer,1,MPI_CHARACTER,prc,mess_tag,comm,errno)
       do j=offstart,noff-1
          k=off%data%ln(off%offset+offstart+j)
          call send_val(prc,v%data%ptr(v%offset+k),&
               0_pm_ln,-1_pm_ln,extra_tag)
       enddo
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,offstart,noff,tno)
       call mpi_rsend(v%data%i(v%offset),1,&
            tno,prc,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_long)
       call get_mpi_disp_type(pm_long,off,offstart,noff,tno)
       call mpi_rsend(v%data%ln(v%offset),1,&
            tno,prc,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,offstart,noff,tno)
       call mpi_rsend(v%data%r(v%offset),1,&
            tno,prc,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,offstart,noff,tno)
       call mpi_rsend(v%data%d(v%offset),1,&
            tno,prc,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,offstart,noff,tno)
       call mpi_rsend(v%data%l(v%offset),1,&
            tno,prc,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine rsend

  ! Create a datatype for a potentially very long message
  subroutine get_mpi_type(tno,n,mpi_typ,m)
    integer(pm_p),intent(in):: tno
    integer(pm_ln),intent(in)::n
    integer,intent(out):: mpi_typ
    integer,intent(out):: m
    integer:: newtyp,k,errno
    integer(MPI_ADDRESS_KIND),dimension(2):: displ
    integer,dimension(2):: typ,blk
    if(n<=max_message_size) then
       mpi_typ=typ_for_pm(tno)
       m=n
       return
    elseif(large_typ_for_pm(tno)/=MPI_DATATYPE_NULL.and.&
         large_typ_size(tno)==n) then
       mpi_typ=large_typ_for_pm(tno)
       m=1
       return
    elseif(block_typ_for_pm(tno)==MPI_DATATYPE_NULL) then
       call mpi_type_contiguous(max_message_size,typ_for_pm(tno),newtyp,errno)
       block_typ_for_pm(tno)=newtyp
    else
       newtyp=block_typ_for_pm(tno)
    endif
    blk(2)=n/max_message_size
    blk(1)=n-blk(2)*max_message_size
    typ(2)=newtyp
    typ(1)=typ_for_pm(tno)
    call mpi_type_size(typ(1),k,errno)
    displ(2)=int(k,MPI_ADDRESS_KIND)*blk(1)
    displ(1)=0
    call mpi_type_create_struct(2,blk,displ,typ,mpi_typ,errno)
    call mpi_type_commit(mpi_typ,errno)
    if(large_typ_for_pm(tno)/=MPI_DATATYPE_NULL) then
       call mpi_type_free(large_typ_for_pm(tno),errno)
    endif
    large_typ_for_pm(tno)=mpi_typ
    large_typ_size(tno)=n
    m=1
  end subroutine get_mpi_type
  
  ! Make a type from a set of vector offsets
  recursive subroutine get_mpi_disp_type(tno,off,offstart,noff,mpi_typ)
    integer(pm_p),intent(in):: tno
    type(pm_ptr),intent(in):: off
    integer(pm_ln),intent(in):: offstart,noff
    integer,intent(out):: mpi_typ
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
          call get_mpi_disp_unit_type(tno,off,start,&
               int(end-start+1),types(i))
       enddo
       call mpi_type_create_struct(n,lens,disps,types,mpi_typ,errno)
       deallocate(disps,lens,types)
       return
    else
       call get_mpi_disp_unit_type(tno,off,offstart,int(noff),mpi_typ)
    endif
    call mpi_type_commit(mpi_typ,errno)
  end subroutine get_mpi_disp_type
  
  recursive subroutine get_mpi_disp_unit_type(tno,off,offstart,noff,mpi_typ)
    integer(pm_p),intent(in):: tno
    type(pm_ptr),intent(in):: off
    integer(pm_ln),intent(in):: offstart
    integer,intent(in):: noff
    integer,intent(out):: mpi_typ
    integer:: basetyp,siz,errno
    integer(MPI_ADDRESS_KIND),allocatable,dimension(:):: disps
    allocate(disps(noff))
    basetyp=typ_for_pm(tno)
    if(pm_debug_level>4) then
       write(*,*) 'DISP TYPE tno=',tno
    endif
    call mpi_type_size(basetyp,siz,errno)
    disps=off%data%ln(off%offset+offstart:off%offset+offstart+noff-1)*siz
    if(pm_debug_level>4) then
       write(*,'(a8,8i8)') 'disps=',disps
    endif
    call mpi_type_create_hindexed_block(noff,1,disps,&
         basetyp,mpi_typ,errno)
    deallocate(disps)
  end subroutine get_mpi_disp_unit_type
  
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

  subroutine push_message_and_type(mess,typ)
    integer:: mess,typ
    if(message_top==max_messages) then
       call pm_panic('too many messages at one time')
    endif
    call push_message(mess)
    message_type_top=message_type_top+1
    if(message_type_top>message_type_stack_size) then
       call grow_stack(message_type,message_type_stack_size,&
            message_type_stack_size*2)
    endif
    message_type(message_type_top)=typ
  end subroutine push_message_and_type
  
  subroutine complete_messages(context)
    type(pm_context),pointer:: context
    integer,dimension(max_messages):: status
    integer:: errno,i,rq,istat(MPI_STATUS_SIZE)
    if(message_actions) then
       do i=1,message_top
          call mpi_waitany(message_top,&
               message_stack,rq,istat,errno)
          if(rq<=message_action_stack_size) then
             if(message_action(rq)/=0) then
                call complete_action(context,rq,&
                     istat(MPI_SOURCE))
             endif
          endif
          message_stack(rq)=MPI_MESSAGE_NULL
       enddo
    else
       call mpi_waitall(message_top,message_stack,&
            MPI_STATUSES_IGNORE,errno)
    endif
    call tidy_messages(context)
  end subroutine complete_messages

  subroutine complete_action(context,rq,prc)
    type(pm_context),pointer:: context
    integer:: rq,prc
    type(pm_ptr):: w,v,off
    integer:: act
    w=action_ptrs%ptr
    v=w%data%ptr(w%offset)
    off=w%data%ptr(w%offset+1)
    act=message_action(rq)
    call recv_all(context,&
         v%data%ptr(v%offset+act),&
         off%data%ptr(off%offset+act),&
         action_start(act),&
         action_size(act),&
         prc,extra_tag)
  end subroutine complete_action

  subroutine tidy_messages(context)
    type(pm_context),pointer:: context
    integer:: i,errno
    type(pm_ptr):: v
    do i=1,message_type_top
           call mpi_type_free(message_type(i),errno)
    enddo
    if(message_stack_size>max_messages) then
       deallocate(message_stack)
       allocate(message_stack(max_messages))
       message_stack_size=max_messages
    endif
    if(message_type_stack_size>max_messages) then
       deallocate(message_type)
       allocate(message_type(max_messages))
       message_type_stack_size=max_messages
    endif
    if(message_action_stack_size>max_messages) then
       deallocate(message_action)
       allocate(message_action(max_messages))
       message_action_stack_size=max_messages
    endif
    if(action_stack_size>max_messages) then
       deallocate(action_start,action_size)
       allocate(action_start(max_messages),&
            action_size(max_messages))
       v=pm_assign_new(context,action_ptrs%ptr,&
            0_pm_ln,pm_pointer,&
            int(max_messages,pm_ln),.true.)
       v=pm_assign_new(context,action_ptrs%ptr,&
            1_pm_ln,pm_pointer,&
            int(max_messages,pm_ln),.true.)
       action_stack_size=max_messages
    endif
    message_top=0
    message_type_top=0
    message_action_top=0
    action_top=0
  end subroutine tidy_messages

  subroutine defer_recv(context,prc,v,off,offstart,noff,mess_tag)
    type(pm_context),pointer:: context
    integer,intent(in):: prc,mess_tag
    type(pm_ptr),intent(in):: v,off
    integer(pm_ln),intent(in):: offstart,noff
    integer:: mess,comm,errno
    integer(pm_ln),dimension(:),allocatable:: temp
    integer(pm_ln):: j
    comm=prc_frame(prc_depth)%this_comm
    action_top=action_top+1
    if(action_top>action_stack_size) then
       allocate(temp(action_stack_size))
       temp=action_start
       deallocate(action_start)
       allocate(action_start(action_stack_size*2))
       action_start(1:action_stack_size)=temp
       temp=action_size
       deallocate(action_size)
       allocate(action_size(action_stack_size*2))
       action_size(1:action_stack_size)=temp
       action_stack_size=action_stack_size*2
       call pm_expand(context,action_ptrs%ptr,0_pm_ln,&
            int(action_stack_size,pm_ln))
       call pm_expand(context,action_ptrs%ptr,1_pm_ln,&
            int(action_stack_size,pm_ln))
       deallocate(temp)
    endif
    action_start(action_top)=offstart
    action_size(action_top)=noff
    call pm_ptr_assign(context,&
         action_ptrs%ptr%data%ptr(action_ptrs%ptr%offset),&
         int(action_top,pm_ln),&
         v)
    call pm_ptr_assign(context,&
         action_ptrs%ptr%data%ptr(action_ptrs%ptr%offset+1),&
         int(action_top,pm_ln),&
         off)
    if(message_top>=message_action_stack_size) then
       call grow_stack(message_action,&
            message_action_stack_size,message_stack_size)
    endif
    do j=message_action_top+1,message_top-1
       message_action(j)=0
    enddo
    message_action(message_top)=action_top
    message_action_top=message_top
    message_actions=.true.
  end subroutine defer_recv

  subroutine collate_messages(prc,index,n,nprc,start,to,from,ve)
    integer(pm_ln),dimension(*),intent(in):: prc
    integer(pm_ln),dimension(*),intent(in):: index
    integer(pm_ln),intent(in):: n
    integer,intent(in):: nprc
    integer(pm_ln),dimension(0:nprc),intent(inout):: start
    integer(pm_ln),dimension(n),intent(inout):: to,from
    integer(pm_ln),dimension(n),optional:: ve
    integer(pm_ln):: i,m,tot
    integer:: p

    if(pm_debug_level>4) then
       write(*,*) 'Collate> prc-',prc(1:n)
    endif
    start(0:nprc-1)=0
    if(present(ve)) then
       do i=1,n
          p=prc(ve(i)+1)
          start(p)=start(p)+1
       enddo
    else
       do i=1,n
          p=prc(i)
          start(p)=start(p)+1
       enddo
    endif

    if(pm_debug_level>4) then
       write(*,'(a8,8i4)') 'start+',start(0:nprc)
    endif
    
    tot=1
    do i=0,nprc-1
       m=start(i)
       tot=tot+m
       start(i)=tot
    enddo
    
    if(pm_debug_level>4) then
       write(*,'(a8,8i4)') 'start-',start(0:nprc)
    endif

    if(present(ve)) then
       do i=1,n
          p=prc(ve(i)+1)
          m=start(p)-1
          to(m)=index(ve(i)+1)
          from(m)=ve(i)
          start(p)=m
       enddo
    else
       do i=1,n
          p=prc(i)
          m=start(p)-1
          to(m)=index(i)
          from(m)=i-1
          start(p)=m
       enddo
    endif

    if(pm_debug_level>4) then
       write(*,'(a8,8i4)') 'start=',start(0:nprc)
    endif
  end subroutine collate_messages

  ! w = v at (prc,idx) for active
  ! ve (ve must be pre-shrunk if originally in logical vector form)
  subroutine get_remote(context,prc,idx,v,w,ve)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: prc,idx,v,ve
    type(pm_ptr):: w
    integer(pm_ln),dimension(:),allocatable:: start,to
    integer:: nprc,rq,comm,i,ii,ncomplete,nmess,iprc,errno
    integer:: itag,mess,rmess,rtop,xsize,prc_block,this_prc
    integer(pm_ln)::ntot,n,jstart,jfinish
    integer,dimension(MPI_STATUS_SIZE):: istat
    type(pm_ptr):: from,rbuffer
    type(pm_root),pointer:: root,root2
    
    if(pm_debug_level>4) then
       write(*,*) 'GET REMOTE:',sys_prc
    endif
   
    nprc=prc_frame(prc_depth)%this_nprc
    comm=prc_frame(prc_depth)%this_comm
    this_prc=prc_frame(prc_depth)%this_prc
    if(pm_fast_vkind(ve)==pm_tiny_int) then
       ntot=0
    elseif(pm_fast_isnull(ve)) then
       ntot=pm_fast_esize(prc)+1
    else
       ntot=pm_fast_esize(ve)+1
    endif

    if(ntot>0) then
       
       allocate(start(0:nprc))
       
       xsize=exchange_block
       allocate(to(xsize))
       root2=>pm_new_as_root(context,pm_long,int(xsize,pm_ln))
       rbuffer=root2%ptr
      
       prc_block=pm_prc_block/vector_num_leaves(v)
       
       ! Phase I compute and send data requests and process any sent here
       do jstart=0,ntot,exchange_block

          root=>pm_new_as_root(context,pm_long,int(xsize,pm_ln))
          from=root%ptr
          
          jfinish=min(jstart+exchange_block-1,ntot-1)
          n=jfinish-jstart+1
          start(nprc)=n+1
          
          if(pm_fast_isnull(ve)) then
             call collate_messages(prc%data%ln(prc%offset+jstart:),&
                  idx%data%ln(idx%offset+jstart:),&
                  n,nprc,start,to,from%data%ln(from%offset:))
          else
             call collate_messages(prc%data%ln(prc%offset:),&
                  idx%data%ln(idx%offset:),&
                  n,nprc,start,to,from%data%ln(from%offset:),&
                  ve%data%ln(ve%offset+jstart:))
          endif

          ! Post speculative request receive
          call mpi_irecv(rbuffer%data%ln(rbuffer%offset),&
               exchange_block,MPI_AINT,&
               MPI_ANY_SOURCE,req_tag,&
               comm,rmess,errno)
          
          do ii=0,nprc-1,prc_block
             
             ! Post data receives
             do i=ii,min(ii+prc_block,nprc-1)
                if(i==this_prc.or.start(i)==start(i+1)) cycle
                call irecv(context,i,w,from,start(i)-1,&
                     start(i+1)-start(i),data_tag)
             enddo
             
             ! Note number of receives
             rtop=message_top
             
             ! Post request sends
             do i=ii,min(ii+prc_block,nprc-1)
                if(i==this_prc.or.start(i)==start(i+1)) cycle
                if(pm_debug_level>4) then
                   write(*,'(a8,8i8)') 'send',this_prc,i,&
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
             if(start(this_prc)/=start(this_prc+1)) then
                if(pm_debug_level>4) then
                   write(*,*) 'cp',start(this_prc),start(this_prc+1)
                   write(*,*) 'copy over',&
                        to(start(this_prc):start(this_prc+1)-1),&
                        from%data%ln(from%offset-1+start(this_prc):&
                        from%offset-1+start(this_prc+1)-1)
                endif
                call vector_copy_elems(context,w,v,&
                     from%data%ln(from%offset-1+start(this_prc):),&
                     to(start(this_prc):),&
                     start(this_prc+1)-start(this_prc),errno)
             endif
             
             ! Process requests
             ncomplete=0
             do while(ncomplete<message_top-1)
                call mpi_waitany(message_top,message_stack,rq,istat,errno)
                if(pm_debug_level>4) then
                   write(*,*) 'GOT',sys_prc,rq,message_top
                endif
                if(rq==message_top) then
                   
                   ! Service data request
                   call mpi_get_count(istat,MPI_AINT,nmess,errno)
                   iprc=istat(MPI_SOURCE)
                   itag=istat(MPI_TAG)
                   
                   if(pm_debug_level>4) then
                      write(*,'(a8,8i8)') 'recv',this_prc,iprc,&
                           rbuffer%data%ln(rbuffer%offset:&
                           rbuffer%offset+nmess-1)
                   endif
                   
                   call rsend(iprc,v,rbuffer,0_pm_ln,int(nmess,pm_ln),data_tag)
                   
                   ! Repost speculative receive
                   call mpi_irecv(rbuffer%data%ln(rbuffer%offset),&
                        exchange_block,&
                        MPI_AINT,MPI_ANY_SOURCE,req_tag,&
                        comm,mess,errno)
                   message_stack(message_top)=mess
                else
                   ! Execute any deferred sync receive
                   if(message_action_top>=rq) then
                      if(message_action(rq)/=0) then
                         call complete_action(context,rq,istat(MPI_SOURCE))
                       endif
                   endif
                   ! Count completed requests sent from here
                   ncomplete=ncomplete+1
                   if(ncomplete<message_top-1) then
                      if(pm_debug_level>4) then
                         write(*,*) sys_prc,ncomplete,&
                              'Phase I completed out of',message_top-1
                      endif
                      message_stack(rq)=MPI_MESSAGE_NULL
                   else
                      if(pm_debug_level>4) then
                         write(*,*) 'Phase I all complete',sys_prc
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
       message_top=message_top+1
       call mpi_irecv(rbuffer%data%ln(rbuffer%offset),exchange_block,&
            MPI_AINT,MPI_ANY_SOURCE,req_tag,&
            comm,rmess,errno)
    endif
    
    if(pm_debug_level>4) then
       write(*,*) 'Phase II started',sys_prc
    endif
    
    ! Phase II - post not blocking barrier and
    ! continue to service requests until it completes
    message_stack(1)=rmess
    call mpi_ibarrier(comm,message_stack(2),errno)
    do
       call mpi_waitany(2,message_stack,rq,istat,errno)
       if(pm_debug_level>4) then
          write(*,*) 'ANDGOT',this_prc,rq
       endif
       if(rq==1) then
          call mpi_get_count(istat,MPI_AINT,nmess,errno)
          iprc=istat(MPI_SOURCE)
          itag=istat(MPI_TAG)
          if(pm_debug_level>4) then
             write(*,*) 'nmess=',nmess
             write(*,'(a5,8i8)') 'recv-',this_prc,iprc,&
                  rbuffer%data%ln(rbuffer%offset:rbuffer%offset+nmess-1)
          endif
          call rsend(iprc,v,rbuffer,0_pm_ln,int(nmess,pm_ln),data_tag)
          ! Repost speculative receive
          call mpi_irecv(rbuffer%data%ln(rbuffer%offset),exchange_block,&
               MPI_AINT,MPI_ANY_SOURCE,req_tag,&
               comm,mess,errno)
          message_stack(1)=mess
       else
          ! Cancel speculative recv
          call mpi_cancel(message_stack(1),errno)
          exit
       endif
    enddo
    if(ntot>0) then
       deallocate(start,to)
    endif
    call pm_delete_root(context,root2)
    if(pm_debug_level>4) then
       write(*,*) 'get_remote_done',sys_prc
    endif
    
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'fvkind.inc'
  end subroutine get_remote

  subroutine put_remote(context,prc,idx,v,w,ve)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: prc,idx,v,ve
    type(pm_ptr):: w
    integer(pm_ln),dimension(:),allocatable:: start,to
    integer:: nprc,rq,comm,i,ii,m,ncomplete,nmess,iprc,errno
    integer:: itag,mess,rmess,rtop,xsize,prc_block,this_prc
    integer(pm_ln)::ntot,n,jstart,jfinish
    integer,dimension(MPI_STATUS_SIZE):: istat
    type(pm_ptr):: from,rbuffer
    type(pm_root),pointer:: root,root2
    logical:: shared
    
    if(pm_debug_level>4) then
       write(*,*) 'PUT REMOTE:',sys_prc
       call pm_dump_tree(context,6,v,2)
       call pm_dump_tree(context,6,w,2)
    endif
    nprc=prc_frame(prc_depth)%this_nprc
    comm=prc_frame(prc_depth)%this_comm
    this_prc=prc_frame(prc_depth)%this_prc

    shared=prc_frame(prc_depth)%is_shared
    if(shared) then
       if(.not.allocated(prc_frame(prc_depth)%sharemap)) then
          call get_sharemap
       endif
    endif
    
    if(prc_frame(prc_depth)%shared_prc>0) then
       ntot=0
    elseif(pm_fast_vkind(ve)==pm_tiny_int) then
       ntot=0
    elseif(pm_fast_isnull(ve)) then
       ntot=pm_fast_esize(prc)+1
    else
       ntot=pm_fast_esize(ve)+1
    endif

    if(ntot>0) then
       
       allocate(start(0:nprc))
       
       xsize=exchange_block
       allocate(to(xsize))
       root2=>pm_new_as_root(context,pm_long,int(xsize,pm_ln))
       rbuffer=root2%ptr
      
       prc_block=pm_prc_block/vector_num_leaves(v)
       
       ! Phase I compute and send data requests and process any sent here
       do jstart=0,ntot,exchange_block

          root=>pm_new_as_root(context,pm_long,int(xsize,pm_ln))
          from=root%ptr
          
          jfinish=min(jstart+exchange_block-1,ntot-1)
          n=jfinish-jstart+1
          start(nprc)=n+1
          
          if(pm_fast_isnull(ve)) then
             call collate_messages(prc%data%ln(prc%offset+jstart:),&
                  idx%data%ln(idx%offset+jstart:),&
                  n,nprc,start,to,from%data%ln(from%offset:))
          else
             call collate_messages(prc%data%ln(prc%offset:),&
                  idx%data%ln(idx%offset:),&
                  n,nprc,start,to,from%data%ln(from%offset:),&
                  ve%data%ln(ve%offset+jstart:))
          endif

          ! Post speculative request receive
          call mpi_irecv(rbuffer%data%ln(rbuffer%offset),&
               exchange_block,MPI_AINT,&
               MPI_ANY_SOURCE,req_tag,&
               comm,rmess,errno)
          
          do ii=0,nprc-1,prc_block
             
             ! Note number of receives
             rtop=message_top

             ! Post data sends
             do i=ii,min(ii+prc_block,nprc-1)
                if(i==this_prc.or.start(i)==start(i+1)) cycle
                call isend(i,w,from,start(i)-1,&
                     start(i+1)-start(i),data_tag)
                if(shared) then
                   m=prc_frame(prc_depth)%sharemap(i)
                   do while(m/=0)
                      call isend(m,w,from,start(i)-1,&
                           start(i+1)-start(i),data_tag)
                      m=prc_frame(prc_depth)%sharemap(m)
                   enddo
                endif
             enddo
                
             ! Post request sends
             do i=ii,min(ii+prc_block,nprc-1)
                if(i==this_prc.or.start(i)==start(i+1)) cycle
                if(pm_debug_level>4) then
                   write(*,'(a8,8i8)') 'send',this_prc,i,&
                        to(start(i):start(i+1)-1)
                endif
                call mpi_issend(to(start(i)),&
                     int(start(i+1)-start(i)),MPI_AINT,i,req_tag,comm,&
                     mess,errno)
                if(shared) then
                   m=prc_frame(prc_depth)%sharemap(i)
                   do while(m/=0)
                      call mpi_issend(to(start(i)),&
                           int(start(i+1)-start(i)),MPI_AINT,m,req_tag,comm,&
                           mess,errno)
                      m=prc_frame(prc_depth)%sharemap(m)
                   enddo
                endif
                call push_message(mess)
             enddo
             
             ! Push speculative receive (top of stack)
             call push_message(rmess)
             
             ! Process intra-node data
             if(start(this_prc)/=start(this_prc+1)) then
                if(pm_debug_level>4) then
                   write(*,*) 'cp',start(this_prc),start(this_prc+1)
                   write(*,*) 'copy over',&
                        to(start(this_prc):start(this_prc+1)-1),'===>',&
                        from%data%ln(from%offset-1+start(this_prc):&
                        from%offset-1+start(this_prc+1)-1)
                endif
                call vector_copy_elems(context,v,w,&
                     to(start(this_prc):),&
                     from%data%ln(from%offset-1+start(this_prc):),&
                     start(this_prc+1)-start(this_prc),errno)
             endif
             
             ! Process requests
             ncomplete=0
             do while(ncomplete<message_top-1)
                call mpi_waitany(message_top,message_stack,rq,istat,errno)
                if(pm_debug_level>4) then
                   write(*,*) 'GOT',sys_prc,rq,message_top
                endif
                if(rq==message_top) then
                   
                   ! Service data request
                   call mpi_get_count(istat,MPI_AINT,nmess,errno)
                   iprc=istat(MPI_SOURCE)
                   itag=istat(MPI_TAG)
                   
                   if(pm_debug_level>4) then
                      write(*,'(a8,8i8)') 'recv',this_prc,iprc,&
                           rbuffer%data%ln(rbuffer%offset:&
                           rbuffer%offset+nmess-1)
                   endif
                 
                   call recv(context,iprc,v,rbuffer,0_pm_ln,&
                        int(nmess,pm_ln),data_tag)
                   
                   ! Repost speculative receive
                   call mpi_irecv(rbuffer%data%ln(rbuffer%offset),&
                        exchange_block,&
                        MPI_AINT,MPI_ANY_SOURCE,req_tag,&
                        comm,mess,errno)
                   message_stack(message_top)=mess
                else
                   ! Count completed requests sent from here
                   ncomplete=ncomplete+1
                   if(ncomplete<message_top-1) then
                      if(pm_debug_level>4) then
                         write(*,*) sys_prc,ncomplete,&
                              'Phase I completed out of',message_top-1
                      endif
                      message_stack(rq)=MPI_MESSAGE_NULL
                   else
                      if(pm_debug_level>4) then
                         write(*,*) 'Phase I all complete',sys_prc
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
    
    if(pm_debug_level>4) then
       write(*,*) 'Phase II started',sys_prc
    endif
    
    ! Phase II - post not blocking barrier and
    ! continue to service requests until it completes
    message_stack(1)=rmess
    call mpi_ibarrier(comm,message_stack(2),errno)
    do
       call mpi_waitany(2,message_stack,rq,istat,errno)
       if(pm_debug_level>4) then
          write(*,*) 'ANDGOT',this_prc,rq
       endif
       if(rq==1) then
          call mpi_get_count(istat,MPI_AINT,nmess,errno)
          iprc=istat(MPI_SOURCE)
          itag=istat(MPI_TAG)
          if(pm_debug_level>4) then
             write(*,*) 'nmess=',nmess
             write(*,'(a5,8i8)') 'recv-',this_prc,iprc,&
                  rbuffer%data%ln(rbuffer%offset:rbuffer%offset+nmess-1)
          endif

          if(pm_debug_level>4) write(*,*) 'RECV=',sys_prc

          call recv(context,iprc,v,rbuffer,0_pm_ln,int(nmess,pm_ln),data_tag)

          if(pm_debug_level>4) write(*,*) 'RECV DONE',sys_prc

          ! Repost speculative receive
          call mpi_irecv(rbuffer%data%ln(rbuffer%offset),exchange_block,&
               MPI_AINT,MPI_ANY_SOURCE,req_tag,&
               comm,mess,errno)
          message_stack(1)=mess
       else
          ! Cancel speculative recv
          call mpi_cancel(message_stack(1),errno)
          exit
       endif
    enddo
    if(ntot>0) then
       deallocate(start,to)
    endif
    call pm_delete_root(context,root2)
    if(pm_debug_level>4) then
       write(*,*) 'put_remote_done',sys_prc
    endif
    
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'fvkind.inc'
  end subroutine put_remote

  subroutine check_rpt_recv(index,n,mask,err)
    type(pm_ptr):: index,mask
    integer(pm_ln):: n
    integer(pm_ln):: i,j
    logical,intent(inout):: err
    do i=0,n-1
       j=index%data%ln(index%offset+i)
       if(mask%data%l(mask%offset+j)) then
          err=.true.
       else
          mask%data%l(mask%offset+j)=.true.
       endif
    enddo
  end subroutine check_rpt_recv

  subroutine get_sharemap
    integer,dimension(:),allocatable:: heads
    integer,dimension(1):: zero,head
    integer:: comm,nprc,i,ii,errno,pgrp,sgrp
    nprc=prc_frame(prc_depth)%this_nprc
    comm=prc_frame(prc_depth)%this_comm
    allocate(heads(0:nprc-1))
    zero=0
    call mpi_comm_group(prc_frame(prc_depth)%shared_comm,sgrp,errno)
    call mpi_comm_group(comm,pgrp,errno)
    call mpi_group_translate_ranks(&
         sgrp,&
         1,zero,pgrp,head,errno)
    call mpi_allgather(head,1,MPI_INTEGER,heads,1,MPI_INTEGER,comm,errno)
    allocate(prc_frame(prc_depth)%sharemap(0:nprc-1))
    prc_frame(prc_depth)%sharemap(:)=0
    do i=1,nprc-1
       ii=heads(i)
       if(ii/=i) then
          prc_frame(prc_depth)%sharemap(i)=prc_frame(prc_depth)%sharemap(ii)
          prc_frame(prc_depth)%sharemap(ii)=i
       endif
    enddo
    deallocate(heads)
  end subroutine get_sharemap
  
  
  function sync_loop_end(ok) result(allok)
    logical,intent(in):: ok
    logical:: allok
    integer:: errno
    if(pm_debug_level>4) &
         write(*,*) 'sync enter',sys_prc,ok
    call mpi_allreduce(ok,allok,1,MPI_LOGICAL,&
         MPI_LOR,prc_frame(prc_depth)%this_comm,errno)
    if(pm_debug_level>4) &
         write(*,*) 'sync leave',sys_prc,allok
  end function sync_loop_end

  function sync_find(ok) result(prc)
    logical,intent(in):: ok
    integer(pm_ln):: prc
    integer:: this,chosen,errno
    this=prc_frame(prc_depth)%this_prc
    if(.not.ok) this=-1
     call  mpi_allreduce(this,chosen,1,MPI_INTEGER,MPI_MAX,&
         prc_frame(prc_depth)%this_comm,errno)
    if(chosen==-1) then
       prc=MPI_PROC_NULL
    else
       prc=chosen
    endif
  end function sync_find

  function sync_status(status) result(allstatus)
    integer,intent(in):: status
    integer:: allstatus
    integer:: i,junk,errno
    if(pm_debug_level>4) &
         write(*,*) 'sync_status',sys_prc,prc_frame(prc_depth)%this_comm
    call mpi_allreduce(status,allstatus,1,MPI_INTEGER,&
        MPI_BOR,prc_frame(prc_depth)%this_comm,errno)
   if(iand(allstatus,pm_prc_error)/=0) then
      do i=prc_depth-1,1,-1
         call mpi_allreduce(allstatus,junk,1,MPI_INTEGER,&
              MPI_BOR,prc_frame(i)%this_comm,errno)
      enddo
      allstatus=pm_prc_error
   endif
   if(pm_debug_level>4) &
        write(*,*) 'sync_status_done',sys_prc
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
       do i=0,sys_nprc-1
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
       q=pm_fast_newnc(context,pm_pointer,2_pm_p)
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
       sp=pm_fast_newnc(context,pm_string,int(n,pm_p))
       sp%data%s(sp%offset:sp%offset+n-1)=s
       q%data%ptr(q%offset+1)=sp
    endif
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
  end subroutine mesg_q_print
  
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

  function wshare(work,nprc,sprc,nsprc) result(prc)
    integer(pm_ln),intent(in):: nprc,sprc,nsprc
    integer(pm_ln),dimension(nprc):: work
    integer(pm_ln):: prc
    integer(pm_ln):: i,s,z
    s=sum(work(1:nprc))
    z=s*(2_pm_ln*sprc+1_pm_ln)/(2_pm_ln*nsprc)
    do i=1,nprc
       z=z-work(i)
       if(z<=0) then
          prc=i-1
          return
       endif
    enddo
    call pm_panic('wshare')
  end function wshare

end module pm_parlib
