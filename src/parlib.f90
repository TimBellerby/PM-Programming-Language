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

  type prc_info_frame
     integer:: this_prc          ! Rank in communcating group
     integer:: this_nprc         ! Num procs in communicating group 
     integer:: this_comm         ! MPI communicator for comm. group.
     integer:: shared_prc        ! Rank in shared group
                                 ! (running the same invocation)
     integer:: shared_nprc       ! Number of process in shared group
     integer:: shared_comm       ! MPI communicator for shared group
  end type prc_info_frame

  integer,parameter:: max_prc_depth=128

  type(prc_info_frame),dimension(max_prc_depth):: prc_frame
  integer:: prc_depth

  logical:: is_par            ! Comm group has other members?
  
  logical:: is_shared         ! Other processes running the same invocation?

  integer(pm_p),parameter:: prc_info_size=2
  
  integer,parameter:: max_message_size=(huge(1)/2)+1
  integer,parameter:: exchange_block= 16*1024*1024
  integer,parameter:: pm_prc_block = 64

  integer:: conc_depth

  integer:: sys_prc,sys_nprc

  integer,dimension(pm_int:pm_string):: typ_for_pm,block_typ_for_pm,&
       large_typ_for_pm
  integer(pm_ln),dimension(pm_int:pm_string):: large_typ_size 

  integer,parameter:: max_messages=1024
  integer,dimension(max_messages):: message_stack,message_type
  integer:: message_top
  
contains

  ! Initialise PM MPI subsystem
  subroutine init_par
    integer:: error
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
    typ_for_pm(pm_single)=MPI_REAL
    typ_for_pm(pm_double)=MPI_DOUBLE
    typ_for_pm(pm_logical)=MPI_LOGICAL

    large_typ_for_pm = MPI_DATATYPE_NULL
    block_typ_for_pm = MPI_DATATYPE_NULL

    message_top=0
    
  end subroutine init_par

  ! Finalise MPI subsystem
  subroutine finalise_par
    integer:: error
    call mpi_finalize(error)
    if(error/=MPI_SUCCESS) &
         call pm_panic('Cannot finalise mpi')
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

  !  Create a new communicating group with grid topology
  subroutine push_prc_grid(context,periods,ndims,dims)
    type(pm_context),pointer:: context
    integer,intent(in):: ndims
    logical,dimension(ndims),intent(in):: periods
    integer,dimension(ndims),intent(out):: dims
    type(pm_ptr):: ptr
    integer:: error,newcomm
    integer:: shared_nprc
    shared_nprc=prc_frame(prc_depth)%shared_nprc
    if(shared_nprc>1) then
       dims=0
       call mpi_dims_create(shared_nprc,ndims,dims,error)
       if(error/=MPI_SUCCESS) &
            call pm_panic('cannot create dims')
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
       if(pm_debug_level>4) then
          write(*,*) 'push grid',prc_depth,dims(1:ndims)
       endif
    else
       if(pm_debug_level>4) then
          write(*,*) 'push_grid_conc',prc_depth,dims(1:ndims)
       endif
       conc_depth=conc_depth+1
       dims(1:ndims)=1
    endif
  contains
    include 'fnewnc.inc'
  end subroutine  push_prc_grid

  ! Pop communicating group - revert to next outer layer
  subroutine pop_prc(context)
    type(pm_context),pointer:: context
    integer:: error,old_shared,old_this
    !write(*,*) 'pop',conc_depth,prc_depth
    if(conc_depth>0) then
       conc_depth=conc_depth-1
    else
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

  ! Create a datatype for a potentially very long message
  subroutine get_mpi_type(tno,n,mpi_typ,m)
    integer,intent(in):: tno
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

  ! Process data (v) from prc to all other nodes in current group
  recursive subroutine broadcast(prc,v)
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v
    integer:: comm
    integer:: i,m,tno,errno
    type(pm_ptr):: avec
    if(pm_debug_level>4) then
       write(*,*) 'BCAST',prc,sys_prc
    endif
    if(prc==MPI_PROC_NULL) return
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call broadcast(prc,v%data%ptr(v%offset+pm_array_dom))
       call broadcast(prc,v%data%ptr(v%offset+pm_array_offset))
       call broadcast(prc,v%data%ptr(v%offset+pm_array_length))
       call broadcast(prc,v%data%ptr(v%offset+pm_array_shape))
       call broadcast(prc,v%data%ptr(v%offset+pm_array_vect))
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call broadcast(prc,v%data%ptr(v%offset+i))
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


  ! Process data (v) from prc to all other nodes in current group
  recursive function broadcast_val(context,prc,v) result(ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v
    type(pm_ptr):: ptr
    integer:: comm
    integer:: i,m,tno,errno
    integer(pm_ln),dimension(3):: hdr
    integer(pm_ln):: esize,j
    type(pm_ptr):: avec
    type(pm_root),pointer:: root
    logical:: this_prc
    
    if(pm_debug_level>4) then
       write(*,*) 'BCAST',prc,sys_prc
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
    case(pm_array_type,pm_const_array_type)
       root=>pm_new_as_root(context,pm_usr,int(pm_array_size,pm_ln))
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)%offset=pm_array_type
       ptr%data%ptr(ptr%offset+1)%offset=hdr(3)
       call pm_ptr_assign(context,ptr,int(pm_array_dom,pm_ln),&
            broadcast_val(context,prc,v%data%ptr(v%offset+pm_array_dom)))
       call pm_ptr_assign(context,ptr,int(pm_array_offset,pm_ln),&
            broadcast_val(context,prc,v%data%ptr(v%offset+pm_array_offset)))
       call pm_ptr_assign(context,ptr,int(pm_array_length,pm_ln),&
            broadcast_val(context,prc,v%data%ptr(v%offset+pm_array_length)))
       call pm_ptr_assign(context,ptr,int(pm_array_shape,pm_ln),&
            broadcast_val(context,prc,v%data%ptr(v%offset+pm_array_shape)))
       call pm_ptr_assign(context,ptr,int(pm_array_vect,pm_ln),&
            broadcast_val(context,prc,v%data%ptr(v%offset+pm_array_vect)))
       call pm_delete_root(context,root)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       root=>pm_new_as_root(context,pm_usr,esize)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)%offset=tno
       ptr%data%ptr(ptr%offset+1)%offset=hdr(3)
       do j=2,esize
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
       write(*,*) 'BCAST DONE'
    endif
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function  broadcast_val
  

  ! Gather data from all processes
  recursive subroutine gather(v,w)
    type(pm_ptr),intent(in):: v,w
    integer:: comm
    integer:: i,m,tno,m2,tno2,errno
    type(pm_ptr):: avec
    if(pm_debug_level>4) then
       write(*,*) 'GATHER',sys_prc
    endif
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call gather(v%data%ptr(v%offset+pm_array_dom),&
            w%data%ptr(w%offset+pm_array_dom))
       call gather(v%data%ptr(v%offset+pm_array_offset),&
            w%data%ptr(w%offset+pm_array_offset))
       call gather(v%data%ptr(v%offset+pm_array_length),&
            w%data%ptr(w%offset+pm_array_length))
       call gather(v%data%ptr(v%offset+pm_array_shape),&
            w%data%ptr(w%offset+pm_array_shape))
       !!!!!
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call gather(v%data%ptr(v%offset+i),w%data%ptr(w%offset+i))
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
       write(*,*) 'GATHER'
    endif
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine gather

  
  ! Start a background send of v to prc
  recursive subroutine isend(prc,mess_tag,v)
    integer,intent(in):: prc,mess_tag
    type(pm_ptr),intent(in):: v
    integer:: comm
    integer:: i,m,tno,errno,mess
    type(pm_ptr):: avec
    if(pm_debug_level>4) then
       write(*,*) 'ISEND',prc,sys_prc
    endif
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call isend(prc,mess_tag,v%data%ptr(v%offset+pm_array_dom))
       call isend(prc,mess_tag,v%data%ptr(v%offset+pm_array_offset))
       call isend(prc,mess_tag,v%data%ptr(v%offset+pm_array_length))
       call isend(prc,mess_tag,v%data%ptr(v%offset+pm_array_shape))
       call isend(prc,mess_tag,v%data%ptr(v%offset+pm_array_vect))
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call isend(prc,mess_tag,v%data%ptr(v%offset+i))
       enddo
    case(pm_int)
       call get_mpi_type(pm_int,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_isend(v%data%i(v%offset),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_long)
       call get_mpi_type(pm_long,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_isend(v%data%ln(v%offset),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_single)
       call get_mpi_type(pm_single,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_isend(v%data%r(v%offset),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_double)
       call get_mpi_type(pm_double,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_isend(v%data%d(v%offset),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_logical)
       call get_mpi_type(pm_logical,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_isend(v%data%l(v%offset),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    end select
    if(pm_debug_level>4) then
       write(*,*) 'ISEND DONE'
    endif
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine isend

  ! Start a backround receive from prc to v
  recursive subroutine irecv(prc,mess_tag,v)
    integer,intent(in):: prc,mess_tag
    type(pm_ptr),intent(in):: v
    integer:: comm
    integer:: i,m,tno,errno,mess
    type(pm_ptr):: avec
    if(pm_debug_level>4) then
       write(*,*) 'IRECV',prc,sys_prc
    endif
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call irecv(prc,mess_tag,v%data%ptr(v%offset+pm_array_dom))
       call irecv(prc,mess_tag,v%data%ptr(v%offset+pm_array_offset))
       call irecv(prc,mess_tag,v%data%ptr(v%offset+pm_array_length))
       call irecv(prc,mess_tag,v%data%ptr(v%offset+pm_array_shape))
       call irecv(prc,mess_tag,v%data%ptr(v%offset+pm_array_vect))
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call irecv(prc,mess_tag,v%data%ptr(v%offset+i))
       enddo
    case(pm_int)
       call get_mpi_type(pm_int,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_irecv(v%data%i(v%offset),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_long)
       call get_mpi_type(pm_long,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_irecv(v%data%ln(v%offset),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_single)
       call get_mpi_type(pm_single,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_irecv(v%data%r(v%offset),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_double)
       call get_mpi_type(pm_double,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_irecv(v%data%d(v%offset),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    case(pm_logical)
       call get_mpi_type(pm_logical,pm_fast_esize(v)+1_pm_ln,tno,m)
       call mpi_irecv(v%data%l(v%offset),m,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message(mess)
    end select
    if(pm_debug_level>4) then
       write(*,*) 'IRECV DONE'
    endif
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine irecv

  ! Make a type from a set of vector offsets
  subroutine get_mpi_disp_type(tno,off,noff,mpi_typ)
    integer,intent(in):: tno
    integer(pm_ln),dimension(*),intent(in):: off
    integer:: noff
    integer,intent(out):: mpi_typ
    integer:: basetyp,siz,errno
    integer(MPI_ADDRESS_KIND),allocatable,dimension(:):: disps
    allocate(disps(noff))
    basetyp=typ_for_pm(tno)
    if(pm_debug_level>4) &
         write(*,*) 'tno=',tno
    call mpi_type_size(basetyp,siz,errno)
    disps=off(1:noff)*siz
    if(pm_debug_level>4)&
         write(*,'(a8,8i8)') 'disps=',disps
    call mpi_type_create_hindexed_block(noff,1,disps,&
         basetyp,mpi_typ,errno)
    call mpi_type_commit(mpi_typ,errno)
    deallocate(disps)
  end subroutine get_mpi_disp_type

  ! Recieve into dispersed buffer v[off]
  recursive subroutine recv_service(prc,v,off,noff,mess_tag)
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v
    integer(pm_ln),dimension(*):: off
    integer:: noff
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,mess,tno,errno
    integer(pm_ln):: start1,finish1
    type(pm_ptr):: offsets,lengths
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call recv_service(prc,v%data%ptr(v%offset+pm_array_dom),&
            off,noff,mess_tag)
       call recv_service(prc,v%data%ptr(v%offset+pm_array_offset),&
            off,noff,mess_tag)
       call recv_service(prc,v%data%ptr(v%offset+pm_array_length),&
            off,noff,mess_tag)
       call recv_service(prc,v%data%ptr(v%offset+pm_array_shape),&
            off,noff,mess_tag)
       offsets=v%data%ptr(v%offset+pm_array_offset)
       lengths=v%data%ptr(v%offset+pm_array_length)
       !!!!!
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call recv_service(prc,v%data%ptr(v%offset+i),off,noff,mess_tag)
      enddo
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,noff,tno)
       call mpi_irecv(v%data%i(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_long)
       call get_mpi_disp_type(pm_long,off,noff,tno)
       call mpi_irecv(v%data%ln(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,noff,tno)
       call mpi_irecv(v%data%r(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,noff,tno)
       call mpi_irecv(v%data%d(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,noff,tno)
       call mpi_irecv(v%data%l(v%offset),1,&
            tno,prc,mess_tag,comm,mess,errno)
       call push_message_and_type(mess,tno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine recv_service

  ! Send from dispersed buffer v[off]
  recursive subroutine service(prc,v,off,noff,mess_tag)
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v
    integer(pm_ln),dimension(*):: off
    integer:: noff
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,tno,errno
    integer(pm_ln):: start1,finish1
    type(pm_ptr):: offsets,lengths
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call service(prc,v%data%ptr(v%offset+pm_array_dom),off,noff,mess_tag)
       call service(prc,v%data%ptr(v%offset+pm_array_offset),off,noff,mess_tag)
       call service(prc,v%data%ptr(v%offset+pm_array_length),off,noff,mess_tag)
       call service(prc,v%data%ptr(v%offset+pm_array_shape),off,noff,mess_tag)
       offsets=v%data%ptr(v%offset+pm_array_offset)
       lengths=v%data%ptr(v%offset+pm_array_length)
       !!!!!
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call service(prc,v%data%ptr(v%offset+i),off,noff,mess_tag)
      enddo
    case(pm_int)
       call get_mpi_disp_type(pm_int,off,noff,tno)
       call mpi_rsend(v%data%i(v%offset),1,&
            tno,prc,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_long)
       call get_mpi_disp_type(pm_long,off,noff,tno)
       call mpi_rsend(v%data%ln(v%offset),1,&
            tno,prc,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_single)
       call get_mpi_disp_type(pm_single,off,noff,tno)
       call mpi_rsend(v%data%r(v%offset),1,&
            tno,prc,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_double)
       call get_mpi_disp_type(pm_double,off,noff,tno)
       call mpi_rsend(v%data%d(v%offset),1,&
            tno,prc,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    case(pm_logical)
       call get_mpi_disp_type(pm_logical,off,noff,tno)
       call mpi_rsend(v%data%l(v%offset),1,&
            tno,prc,mess_tag,comm,errno)
       call mpi_type_free(tno,errno)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine service
  
  recursive subroutine service_block(prc,v,start,finish,mess_tag)
    integer,intent(in):: prc
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: start,finish
    integer,intent(in):: mess_tag
    integer:: comm
    integer:: i,m,tno,errno
    type(pm_ptr):: avec
    ! write(*,*) 'BCAST',prc,this_prc
    comm=prc_frame(prc_depth)%this_comm
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call service_block(prc,v%data%ptr(v%offset+pm_array_dom),start,finish,mess_tag)
       call service_block(prc,v%data%ptr(v%offset+pm_array_offset),start,finish,mess_tag)
       call service_block(prc,v%data%ptr(v%offset+pm_array_length),start,finish,mess_tag)
       call service_block(prc,v%data%ptr(v%offset+pm_array_shape),start,finish,mess_tag)
       call service_block(prc,v%data%ptr(v%offset+pm_array_vect),start,finish,mess_tag)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(v)
          call service_block(prc,v%data%ptr(v%offset+i),start,finish,mess_tag)
       enddo
    case(pm_int)
       call get_mpi_type(pm_int,finish-start+1_pm_ln,tno,m)
       call mpi_rsend(v%data%i(v%offset+start),m,&
            tno,prc,mess_tag,comm,errno)
    case(pm_long)
       call get_mpi_type(pm_long,finish-start+1_pm_ln,tno,m)
       call mpi_rsend(v%data%ln(v%offset+start),m,&
            tno,prc,mess_tag,comm,errno)
    case(pm_single)
       call get_mpi_type(pm_single,finish-start+1_pm_ln,tno,m)
       call mpi_rsend(v%data%r(v%offset+start),m,&
            tno,prc,mess_tag,comm,errno)
    case(pm_double)
       call get_mpi_type(pm_double,finish-start+1_pm_ln,tno,m)
       call mpi_rsend(v%data%d(v%offset+start),m,&
            tno,prc,mess_tag,comm,errno)
    case(pm_logical)
       call get_mpi_type(pm_logical,finish-start+1_pm_ln,tno,m)
       call mpi_rsend(v%data%l(v%offset+start),m,&
            tno,prc,mess_tag,comm,errno)
    end select
    !write(*,*) 'BCAST DONE'
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine service_block
  
  subroutine push_message(mess)
    integer:: mess
    if(message_top==max_messages) then
       call pm_panic('too many messages at one time')
    endif
    message_top=message_top+1
    message_stack(message_top)=mess
  end subroutine push_message

  subroutine push_message_and_type(mess,typ)
    integer:: mess,typ
    if(message_top==max_messages) then
       call pm_panic('too many messages at one time')
    endif
    message_top=message_top+1
    message_stack(message_top)=mess
    message_type(message_top)=typ
  end subroutine push_message_and_type
  
  subroutine complete_messages
    integer,dimension(max_messages):: status
    integer:: errno
    call mpi_waitall(message_top,message_stack,MPI_STATUSES_IGNORE,errno)
  end subroutine complete_messages
  
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
    integer(pm_ln),dimension(:),allocatable:: start,to,from,rbuffer
    integer:: nprc,rq,comm,i,ii,ncomplete,nmess,iprc,errno
    integer:: itag,mess,rtop,xsize,prc_block,this_prc
    integer(pm_ln)::ntot,n,jstart,jfinish
    integer,dimension(MPI_STATUS_SIZE):: istat

    integer,parameter:: req_tag=1
    integer,parameter:: data_tag=2
    
    if(pm_debug_level>4) then
       write(*,*) 'get remote:',sys_prc
       !write(*,'(a8,8i8)') 'v=',v%data%ln(v%offset:v%offset+pm_fast_esize(v))
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
       
       xsize=min(ntot,exchange_block)
       allocate(rbuffer(xsize),from(xsize),to(xsize))
       
       prc_block=pm_prc_block/vector_num_leaves(v)
       
       ! Phase I compute and send data requests and process any sent here
       do jstart=0,ntot,exchange_block
          
          message_top=0
          
          jfinish=min(jstart+exchange_block-1,ntot-1)
          n=jfinish-jstart+1
          start(nprc)=n+1
          
          if(pm_fast_isnull(ve)) then
             call collate_messages(prc%data%ln(prc%offset+jstart:),&
                  idx%data%ln(idx%offset+jstart:),&
                  n,nprc,start,to,from)
          else
             call collate_messages(prc%data%ln(prc%offset:),&
                  idx%data%ln(idx%offset:),&
                  n,nprc,start,to,from,ve%data%ln(ve%offset+jstart:))
          endif
          
          do ii=0,nprc-1,prc_block
             
             ! Post data receives
             do i=ii,min(ii+prc_block,nprc-1)
                if(i==this_prc.or.start(i)==start(i+1)) cycle
                call recv_service(i,w,from(start(i)),&
                     int(start(i+1)-start(i)),data_tag)
             enddo
             
             ! Note number of receives
             rtop=message_top
             
             ! Post request sends
             do i=ii,min(ii+prc_block,nprc-1)
                if(i==this_prc.or.start(i)==start(i+1)) cycle
                if(pm_debug_level>4) then
                   write(*,'(a8,8i8)') 'send',this_prc,i,to(start(i):start(i+1)-1)
                endif
                call mpi_isend(to(start(i)),&
                     int(start(i+1)-start(i)),MPI_AINT,i,req_tag,comm,&
                     mess,errno)
                call push_message(mess)
             enddo
             
             ! Post speculative request receive
             call mpi_irecv(rbuffer,exchange_block,MPI_AINT,&
                  MPI_ANY_SOURCE,req_tag,&
                  comm,mess,errno)
             call push_message(mess)
             
             ! Process intra-node data
             if(start(this_prc)/=start(this_prc+1)) then
                if(pm_debug_level>4) then
                   write(*,*) 'cp',start(this_prc),start(this_prc+1)
                   write(*,*) 'copy over',to(start(this_prc):start(this_prc+1)-1),&
                        from(start(this_prc):start(this_prc+1)-1)
                endif
                call vector_copy_elems(context,w,v,from(start(this_prc):),&
                     to(start(this_prc):),&
                     start(this_prc+1)-start(this_prc),errno)
             endif
             
             ! Process requests
             ncomplete=0
             do while(ncomplete<message_top-1)
                call mpi_waitany(message_top,message_stack,rq,istat,errno)
                if(pm_debug_level>4) then
                   write(*,*) 'GOT',this_prc,rq,message_top
                   if(rq<=rtop) &
                        write(*,*) 'w=',this_prc,rq,w%data%ln(w%offset:w%offset+1)
                endif
                if(rq==message_top) then
                   
                   ! Service data request
                   call mpi_get_count(istat,MPI_AINT,nmess,errno)
                   iprc=istat(MPI_SOURCE)
                   itag=istat(MPI_TAG)
                   
                   if(pm_debug_level>4) then
                      write(*,'(a8,8i8)') 'recv',this_prc,iprc,rbuffer(1:nmess)
                   endif
                   
                   call service(iprc,v,rbuffer,nmess,data_tag)
                   
                   ! Repost speculative receive
                   call mpi_irecv(rbuffer,exchange_block,&
                        MPI_AINT,MPI_ANY_SOURCE,req_tag,&
                        comm,mess,errno)
                   message_stack(message_top)=mess
                   
                else
                   ! Count completed requests sent from here
                   ncomplete=ncomplete+1
                   if(ncomplete<message_top-1) then
                      if(pm_debug_level>4) &
                           write(*,*) ncomplete,'complete',message_top-1
                      message_stack(rq)=MPI_MESSAGE_NULL
                   else
                      if(pm_debug_level>4) &
                           write(*,*) 'all comlete',this_prc
                      exit
                   endif
                endif
             enddo
             
             ! Free types associated with data receives
             do i=1,rtop
                call mpi_type_free(message_type(i),errno)
             enddo
          enddo

       enddo

    else
       ! Post speculative receive
       xsize=min(ntot,exchange_block)
       allocate(rbuffer(xsize))
       message_top=message_top+1
       call mpi_irecv(rbuffer,exchange_block,&
            MPI_AINT,MPI_ANY_SOURCE,req_tag,&
            comm,mess,errno)
       message_stack(message_top)=mess
    endif
    
    if(pm_debug_level>4) then
       write(*,*) 'Phase II ',sys_prc
    endif
    
    ! Phase II - post not blocking barrier and
    ! continue to service requests until it completes
    message_stack(1)=message_stack(message_top)
    call mpi_ibarrier(comm,message_stack(2),errno)
    !call pm_post_ibarrier(comm,message_stack(2))
    do
       call mpi_waitany(2,message_stack,rq,istat,errno)
       if(pm_debug_level>4) & 
          write(*,*) 'ANDGOT',this_prc,rq
       if(rq==1) then
          call mpi_get_count(istat,MPI_AINT,nmess,errno)
          iprc=istat(MPI_SOURCE)
          itag=istat(MPI_TAG)
          if(pm_debug_level>4) then
             write(*,*) 'nmess=',nmess
             write(*,'(a5,8i8)') 'recv-',this_prc,iprc,rbuffer(1:nmess)
          endif
          call service(iprc,v,rbuffer,nmess,data_tag)
          ! Repost speculative receive
          call mpi_irecv(rbuffer,exchange_block,&
               MPI_AINT,MPI_ANY_SOURCE,req_tag,&
               comm,mess,errno)
          message_stack(1)=mess
       else
          ! Cancel speculative recv
          call mpi_cancel(message_stack(1),errno)
          exit
       endif
    enddo
    message_top=0
    deallocate(rbuffer)
    if(ntot>0) then
       deallocate(start,to,from)
    endif
    if(pm_debug_level>4) then
       write(*,*) 'get_remote_done',sys_prc
    endif
    
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'fvkind.inc'
  end subroutine get_remote

  function sync_loop_end(ok) result(allok)
    logical,intent(in):: ok
    logical:: allok
    integer:: errno
    if(pm_debug_level>4) &
         write(*,*) 'enter',sys_prc,ok
    call mpi_allreduce(ok,allok,1,MPI_LOGICAL,MPI_LOR,prc_frame(prc_depth)%this_comm,errno)
    if(pm_debug_level>4) &
         write(*,*) 'leave',sys_prc,allok
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
  
end module pm_parlib
