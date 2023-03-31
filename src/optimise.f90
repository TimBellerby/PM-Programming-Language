!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2020
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

! Scheduling and optimisation of worcodes


module pm_optimise

  use pm_kinds
  use pm_memory
  use pm_types
  use pm_vmdefs
  use pm_hash
  use pm_lib
  use pm_wcode

  implicit none

  logical,parameter:: debug_opt=.true.

  type instruction
     integer:: index
     integer:: ve,cove
     integer:: nbefore
     integer(pm_i16):: priority
     integer(pm_i16):: kind
  end type instruction

  type oz_state
     type(pm_context),pointer:: context
     
     ! Current proc
     type(pm_ptr):: fn,keys
     integer:: taints,pvar,rvar,vevar
     
     ! Wordcodes for current proc
     integer,dimension(:),pointer:: codes
     integer,dimension(:),pointer:: vars

     integer,allocatable,dimension(:):: varindex
     integer:: nvars
     integer:: vindex
     
  end type oz_state

  ! Variable access (stored per instruction)
  integer(pm_i8),parameter:: rw_none=0
  integer(pm_i8),parameter:: rw_read=1
  integer(pm_i8),parameter:: rw_write=2

  ! Instruction kind (== scheduling priority)
  ! -- also designed to group operations together
  integer,parameter:: instr_is_sync=0
  integer,parameter:: instr_is_sync_recv=1
  integer,parameter:: highest_sync_instr=instr_is_sync_recv
  integer,parameter:: instr_is_std=2
  integer,parameter:: lowest_shared_instr_kind=instr_is_std+1
  integer,parameter:: instr_is_shared=3
  integer,parameter:: instr_is_bcall=4
  integer,parameter:: instr_is_scall=5
  integer,parameter:: instr_is_rcall=6
  integer,parameter:: instr_is_comm=7

  ! Power of two > highest instr kind
  integer,parameter:: instr_kind_mult=8

contains

   subroutine optimise_prog(context,p,poly_cache)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: p,poly_cache
    type(oz_state),target:: oz
    integer(pm_ln):: i
    return
    oz%context=>context
    do i=1,pm_dict_size(context,p)
       if(debug_opt) write(*,*) 'OPT PROC>',i-1
       call optimise_proc(oz,pm_dict_val(context,p,i))
    enddo
  end subroutine optimise_prog

  subroutine optimise_proc(oz,p)
    type(oz_state):: oz
    type(pm_ptr),intent(in):: p
    type(pm_ptr):: q
    integer(pm_i8),dimension(:),allocatable:: writes
    integer:: l
    oz%fn=p
    q=p%data%ptr(p%offset)
    oz%rvar=q%data%i(q%offset)
    oz%pvar=q%data%i(q%offset+1)
    oz%vevar=q%data%i(q%offset+3)
    oz%codes=>q%data%i(q%offset+4:q%offset+pm_fast_esize(q))
    q=p%data%ptr(p%offset+1)
    oz%vars=>q%data%i(q%offset:q%offset+pm_fast_esize(q))
    oz%nvars=size(oz%vars)/2
    q=p%data%ptr(p%offset+2)
    oz%keys=p%data%ptr(p%offset+3)
    oz%taints=q%offset
    allocate(oz%varindex(size(oz%vars)))
    oz%varindex=0
    oz%vindex=0
    allocate(writes(oz%nvars))
    l=comp_op_start
    if(size(oz%codes)>0) then
       call schedule_block(oz,l,writes,.false.)
    endif
    deallocate(oz%varindex)
    deallocate(writes)
  contains
    include 'fesize.inc'
  end subroutine optimise_proc

  recursive subroutine schedule_block(oz,ptr_to_start,writes_above,must_schedule)
    type(oz_state):: oz
    integer,intent(in):: ptr_to_start
    integer(pm_i8),dimension(:),intent(out):: writes_above
    logical,intent(in):: must_schedule
    integer:: ninstr,ll,i,j,k,num_scheduled
    integer:: instr_ptr,instr_scheduled
    integer:: curr_priority,new_priority
    logical:: need_to_schedule,clash_before
    type(instruction),allocatable,dimension(:):: instr
    integer(pm_i8),allocatable,dimension(:,:):: writes
    logical(pm_pl),allocatable,dimension(:,:):: clash

    write(*,*) 'SCHEDULE BLOCK>',ptr_to_start
    
    ninstr=0
    ll=oz%codes(ptr_to_start)
    do while(ll/=0)
       ninstr=ninstr+1
       ll=oz%codes(ll)
    enddo
    allocate(instr(ninstr))
    allocate(writes(oz%nvars,ninstr),source=rw_none)
    allocate(clash(ninstr,ninstr),source=.false.)
    i=0
    ll=oz%codes(ptr_to_start)
    need_to_schedule=.false.
    writes_above=0
    do while(ll/=0)
       i=i+1
       call process_instr(oz,ll,instr,i,writes)
       ll=oz%codes(ll)
    enddo

    need_to_schedule=must_schedule
    do i=1,ninstr
       if(instr(i)%kind/=instr_is_std) need_to_schedule=.true.
       instr(i)%nbefore=0
       do j=1,oz%vindex
          writes_above(j)=max(writes_above(j),writes(j,i))
          if(writes(j,i)>0) then
             do k=1,i-1
                clash_before=clash(i,k)
                clash(i,k)=clash(i,k).or.(writes(j,k)+writes(j,i))>2
                if(clash(i,k).and..not.clash_before) &
                     instr(i)%nbefore=instr(i)%nbefore+1
             enddo
          endif
       enddo
    enddo
    
    if(.not.need_to_schedule) goto 10
    
    instr(ninstr)%priority=0
    do i=ninstr-1,1,-1
       instr(i)%priority=0
       do j=i+1,ninstr
          if(clash(j,i)) then
             instr(i)%priority=max(instr(i)%priority,instr(j)%priority+&
                  merge(1,0,instr(j)%kind>=lowest_shared_instr_kind))
          endif
       enddo
    enddo

    do i=1,ninstr
       write(*,*) op_names(oz%codes(instr(i)%index+2))
       do j=1,oz%nvars
          if(writes(j,i)>0) write(*,*) j,writes(j,i)
       enddo
    enddo

    write(*,'(" ",A,20i2)') op_names(1), (j,j=1,ninstr)
    do i=1,ninstr
       write(*,*) op_names(oz%codes(instr(i)%index+2)),clash(i,1:i-1)
    enddo

    instr_ptr=ptr_to_start
    num_scheduled=0

    if(oz%codes(instr(1)%index+2)==op_do_loop) then
       instr_scheduled=1
    else
       ! Find first instruction
       k=1
       do while(instr(k)%nbefore>0)
          k=k+1
       enddo
       j=k
       curr_priority=solo_priority(instr(j))
       do i=k+1,ninstr
          if(instr(i)%nbefore==0) then
             new_priority=solo_priority(instr(i))
             if(new_priority>curr_priority) then
                j=i
                curr_priority=new_priority
             endif
          endif
       enddo
       instr_scheduled=j
    endif
    call schedule(instr_scheduled)

    ! Schedule remaining instructions
    do while(num_scheduled<ninstr)
       curr_priority=-1
       j=-1
       write(*,'(20i2)') instr(1:ninstr)%nbefore
       do i=1,ninstr
          if(clash(i,instr_scheduled)) then
             instr(i)%nbefore=instr(i)%nbefore-1
          endif
          if(instr(i)%nbefore==0) then
             new_priority=co_priority(instr(i))
             if(new_priority>curr_priority) then
                j=i
                curr_priority=new_priority
             endif
          endif
       enddo
       if(pm_debug_level>0) then
          if(curr_priority<0) then
             write(*,*) 'curr_priority=',curr_priority
             call pm_panic('Scheduled priority <0')
          endif
          if(j<0) then
             write(*,*) ninstr
             write(*,'(20i2)') instr(1:ninstr)%nbefore
             do i=1,ninstr
                write(*,*) clash(:,i)
             enddo
             call pm_panic('cannot schedule')
          endif
       endif
       instr_scheduled=j
       call schedule(instr_scheduled)
       if(instr(instr_scheduled)%kind<=highest_sync_instr) then
          j=1
          do while(j<ninstr)
             if(instr(j)%nbefore==0.and.&
                  instr(j)%kind==instr(instr_scheduled)%kind) then
                call schedule(j)
             endif
             j=j+1
          enddo
       endif
    enddo

    oz%codes(instr_ptr)=0

10  continue
    
    deallocate(instr)
    deallocate(writes)
    deallocate(clash)

  contains

    subroutine schedule(i)
      integer,intent(in):: i
      write(*,*) 'scheduled:',i,op_names(oz%codes(instr(i)%index+2))
      oz%codes(instr_ptr)=instr(i)%index
      instr_ptr=instr(i)%index
      instr(i)%priority=-999
      num_scheduled=num_scheduled+1
    end subroutine schedule

    ! Priority of instruction considered alone
    function solo_priority(i) result(priority)
      type(instruction),intent(in):: i
      integer:: priority
      priority=(i%priority*instr_kind_mult+i%kind)*4
    end function solo_priority

    ! Priority of instruction in light of last
    ! scheduled instruction
    function co_priority(i) result(priority)
      type(instruction),intent(in):: i
      integer:: priority
      priority=solo_priority(i)
      if(i%ve==instr(instr_scheduled)%ve) priority=priority+2
      if(i%cove==instr(instr_scheduled)%ve) priority=priority+1
    end function co_priority

  end subroutine schedule_block

  recursive subroutine process_instr(oz,l,instr,i,writes)
    type(oz_state):: oz
    integer:: l,i
    type(instruction),dimension(:):: instr
    integer(pm_i8),dimension(:,:):: writes
    integer:: j,opcode,n,a,m,kind
    opcode=oz%codes(l+comp_op_opcode)
    write(*,*) 'Process instr>',i,l,op_names(opcode)
    n=oz%codes(l+comp_op_nargs)
    a=l+comp_op_arg0
    instr(i)%index=l
    instr(i)%ve=oz%codes(a)
    if(oz%codes(a)>0) then
       instr(i)%cove=oz_v2(oz,oz%codes(a))
       call do_var(a)
    else
       instr(i)%cove=0
    endif
    m=1
    select case(opcode)
    case(op_if_shared)
       call schedule_block(oz,a+1,writes(:,i),.false.)
       call schedule_block(oz,a+2,writes(:,i),.false.)
       kind=instr_is_shared
       m=3
    case(op_loop,op_comm_proc)
       call schedule_block(oz,a+1,writes(:,i),.false.)
       kind=instr_is_std
       m=2
    case(op_comm_block,op_comm_loop)
       call schedule_block(oz,a+1,writes(:,i),.true.)
       kind=instr_is_shared
       m=2
    case(op_comm_call)
       kind=instr_is_shared
    case(op_remote_call,op_remote_send_call)
       call schedule_block(oz,a+1,writes(:,i),.false.)
       kind=instr_is_rcall
       m=2
    case(op_server_call,op_collect_call)
       call schedule_block(oz,a+1,writes(:,i),.false.)
       kind=instr_is_scall
       m=2
    case(op_bcast_call)
       call schedule_block(oz,a+1,writes(:,i),.false.)
       kind=instr_is_bcall
       m=2
    case(op_isend_offset,op_irecv_offset,op_isend,op_irecv)
       kind=instr_is_comm
    case(op_recv_offset,op_recv)
       kind=instr_is_sync_recv
    case(op_sync_mess)
       kind=instr_is_sync
    case(op_broadcast,op_broadcast_val,op_break_loop,op_comm_inline)
       kind=instr_is_shared
    case default
       kind=instr_is_std
    end select
    instr(i)%kind=kind
    do j=m,n-1
       call do_var(a+j)
    enddo
  contains
    subroutine do_var(v)
      integer,intent(in):: v
      write(*,*) 'Process var>',oz%codes(v)
      call process_var(oz,abs(oz%codes(v)),&
           merge(rw_write,rw_read,oz%codes(v)<0),writes(:,i))
    end subroutine do_var
  end subroutine process_instr

  recursive subroutine process_var(oz,v,rw,writes)
    type(oz_state):: oz
    integer,intent(in):: v
    integer(pm_i8),intent(in):: rw
    integer(pm_i8),dimension(:),intent(inout):: writes
    integer:: j,k,index
    logical:: varclash

    if(v==0) return
    k=oz_kind(oz,v)
    select case(k)
    case(v_is_alias,v_is_elem)
       call process_var(oz,oz_v1(oz,v),rw,writes)
    case(v_is_group)
       do j=1,oz_v1(oz,v)
          call process_var(oz,oz_ptr(oz,v,j),rw,writes)
       enddo
    case(v_is_sub,v_is_vsub)
       call process_var(oz,oz_v1(oz,v),rw,writes)
       call process_var(oz,oz_v2(oz,v),rw,writes)
    case(v_is_cove)
       call process_var(oz,oz_v2(oz,v),rw,writes)
    case(v_is_basic,v_is_ctime_const,v_is_const,v_is_ve)
       index=oz%varindex(v)
       if(index==0) then
          index=oz%vindex+1
          oz%varindex(v)=index
          oz%vindex=index
       endif
       !write(*,*) '#',index,oz%vindex,oz%nvars,i
       writes(index)=max(writes(index),rw)
    case default
       write(*,*) 'k=',k
       call pm_panic('oz process-var')
    end select
  end subroutine process_var

  function oz_kind(oz,n) result(v1)
    type(oz_state),intent(inout):: oz
    integer,intent(in):: n
    integer:: v1
    integer:: info
    info=oz%vars(abs(n))
    v1=iand(info,cvar_flag_mask)
  end function oz_kind

  function oz_v1(oz,n) result(v1)
    type(oz_state),intent(inout):: oz
    integer,intent(in):: n
    integer:: v1
    integer:: info
    info=oz%vars(abs(n))
    v1=info/cvar_flag_mult
  end function oz_v1

  function oz_v2(oz,n) result(v2)
    type(oz_state),intent(inout):: oz
    integer,intent(in):: n
    integer:: v2
    integer:: info
    info=oz%vars(abs(n)+1)
    v2=info/cvar_flag_mult
  end function oz_v2

  function oz_ptr(oz,n,i) result(v)
    type(oz_state),intent(inout):: oz
    integer,intent(in):: n,i
    integer:: v
    integer:: info
    v=oz%vars(n+i+1)/cvar_flag_mult
  end function oz_ptr
  
end module pm_optimise
