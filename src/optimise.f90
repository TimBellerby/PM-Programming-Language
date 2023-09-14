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

! Scheduling and optimisation of wordcodes


module pm_optimise

  use pm_kinds
  use pm_memory
  use pm_types
  use pm_vmdefs
  use pm_hash
  use pm_options
  use pm_lib
  use pm_wcode

  implicit none

  logical,parameter:: debug_opt=.false.
  logical,parameter:: dump_block_schedules=.false.
  
  type instruction
     integer:: index
     integer:: ve,cove
     integer:: kind
     integer(pm_ln):: bloom
  end type instruction

  type oz_state
     type(pm_context),pointer:: context
     
     ! Current proc
     type(pm_ptr):: fn,keys
     integer:: taints,pvar,rvar,vevar
     
     ! Wordcodes for current proc
     integer,dimension(:),pointer:: codes
     integer,dimension(:),pointer:: vars

     ! Local variable descriptors
     integer,allocatable,dimension(:):: varindex
     integer:: nvars,nmask
     integer:: vindex
     
  end type oz_state

  integer,parameter:: instr_precedes_loop=0
  integer,parameter:: instr_is_comm=1
  integer,parameter:: instr_is_std=2
  integer,parameter:: instr_is_rcall=3
  integer,parameter:: instr_is_sync=4
  integer,parameter:: instr_follows_loop=5
  
  integer,parameter:: instr_is_shared=16
  integer,parameter:: instr_is_loop_break=32
  integer,parameter:: instr_priority_mask=15

  integer,parameter:: resource_is_print=1
  integer,parameter:: resource_is_comm=2
  integer,parameter:: last_resource=resource_is_comm
  
contains

   subroutine optimise_prog(context,p,poly_cache)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: p,poly_cache
    type(oz_state),target:: oz
    integer(pm_ln):: i
    if(.not.pm_opts%schedule) return
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
    integer(pm_ln):: bloom
    integer(pm_ln),dimension(:,:),allocatable:: mask
    integer(pm_ln):: eg_mask
    integer:: i,l,kind
    oz%fn=p
    q=p%data%ptr(p%offset)
    oz%rvar=q%data%i(q%offset)
    oz%pvar=q%data%i(q%offset+1)
    oz%vevar=q%data%i(q%offset+3)
    oz%codes=>q%data%i(q%offset+4:q%offset+pm_fast_esize(q))
    q=p%data%ptr(p%offset+1)
    oz%vars=>q%data%i(q%offset:q%offset+pm_fast_esize(q))
    oz%nvars=0
    i=1
    do while(i<=size(oz%vars))
       kind=iand(oz%vars(i),cvar_flag_mask)
       if(kind==v_is_basic.or.kind==v_is_ve) then
          oz%nvars=oz%nvars+1
          i=i+3
       elseif(kind==v_is_group) then
          i=i+3+oz%vars(i)/cvar_flag_mult
       elseif(kind==0) then
          i=i+1
       else
          i=i+3
       endif
    enddo
    oz%nmask=(oz%nvars+1)/bit_size(eg_mask)+1
    if(debug_opt) write(*,*) 'nvars>',oz%nvars,'nmask>',oz%nmask
    q=p%data%ptr(p%offset+2)
    oz%keys=p%data%ptr(p%offset+3)
    oz%taints=q%offset
    allocate(oz%varindex(size(oz%vars)))
    oz%varindex=0
    oz%vindex=last_resource
    allocate(mask(oz%nmask,2))
    l=comp_op_start
    if(size(oz%codes)>0) then
       call schedule_block(oz,l,bloom,mask)
    endif
    deallocate(oz%varindex)
    deallocate(mask)
  contains
    include 'fesize.inc'
  end subroutine optimise_proc

  ! ============================================================
  ! This performs a list scheduling of a block of instructions
  ! and may recursively schedule sub-blocks
  ! ============================================================
  recursive subroutine schedule_block(oz,ptr_to_start,outer_bloom,outer_mask)
    type(oz_state):: oz
    integer,intent(inout):: ptr_to_start
    integer(pm_ln),intent(inout):: outer_bloom
    integer(pm_ln),dimension(oz%nmask,2):: outer_mask
    integer:: ninstr,ll,i,j,k,num_scheduled
    integer:: instr_ptr,instr_scheduled
    integer:: curr_priority,new_priority
    logical:: have_shared,have_non_shared
    type(instruction),allocatable,dimension(:):: instr
    integer(pm_ln),dimension(:,:,:),allocatable:: mask
    integer,allocatable,dimension(:):: nafter
    logical(pm_pl),allocatable,dimension(:,:):: clash

    if(debug_opt) write(*,*) 'SCHEDULE BLOCK>',ptr_to_start
    
    ninstr=0
    ll=ptr_to_start
    do while(ll/=0)
       ninstr=ninstr+1
       ll=oz%codes(ll)
    enddo
    allocate(instr(ninstr))
    allocate(nafter(ninstr),source=0)
    allocate(clash(ninstr,ninstr),source=.false.)
    allocate(mask(oz%nmask,2,ninstr),source=0_pm_ln)

    ! Process each instruction to get information
    ! - recursively schedule any sub-blocks
    i=0
    ll=ptr_to_start
    do while(ll/=0)
       i=i+1
       call process_instr(oz,ll,instr(i),mask(:,:,i),outer_mask,outer_bloom)
       ll=oz%codes(ll)
    enddo

    ! Compute clash(preceding_instr,following_instr) - true if RAW, WAR or WAW conflict
    ! and nafter(instr) - number of instructions that must follow instr
    ! due to a direct clash
    have_shared=.false.
    have_non_shared=.false.
    do i=1,ninstr
       if(iand(instr(i)%kind,instr_is_shared)/=0) have_shared=.true.
       if(iand(instr(i)%kind,instr_is_shared)==0) have_non_shared=.true.
       do j=1,i-1
          clash(j,i)=conflict(oz,instr,mask,j,i)
          if(clash(j,i)) nafter(j)=nafter(j)+1
       enddo
    enddo

    if(dump_block_schedules) then
       ll=ptr_to_start
       write(22,*) '==========BEFORE======'
       i=0
       do while(ll/=0)
          i=i+1
          write(22,'(i4,z17,i7,a10,10i6)') nafter(i),instr(i)%bloom,ll,op_names(oz%codes(ll+2)),&
               oz%codes(ll+comp_op_arg0:ll+comp_op_arg0+&
               oz%codes(ll+comp_op_nargs)-1)
          ll=oz%codes(ll)
       enddo
       write(22,*) '======================='
       
       do i=1,ninstr
          write(33,*) op_names(oz%codes(instr(i)%index+2)),&
               (merge(merge('*','$',clash(j,i)),&
               '.',iand(instr(i)%bloom,instr(j)%bloom)/=0),j=1,i-1)
       enddo
    endif
    
    if(.not.(have_shared.and.have_non_shared)) then
       if(dump_block_schedules) write(22,*) '======DO NOT SCHEDULE===='
       goto 10
    endif

    instr_ptr=0
    num_scheduled=0

    ! Find last instruction
    k=1
    do while(nafter(k)>0)
       k=k+1
    enddo
    j=k
    curr_priority=solo_priority(instr(j))
    do i=k+1,ninstr
       if(nafter(i)==0) then
          new_priority=solo_priority(instr(i))
          if(new_priority>=curr_priority) then
             j=i
             curr_priority=new_priority
          endif
       endif
    enddo
    instr_scheduled=j
    call schedule(instr_scheduled)

    ! Schedule remaining instructions
    do while(num_scheduled<ninstr)
       curr_priority=-1
       j=-1
       do i=1,ninstr
          if(instr(i)%kind<0) cycle
          if(clash(i,instr_scheduled)) then
             nafter(i)=nafter(i)-1
          endif
          
          if(nafter(i)==0) then
             new_priority=co_priority(instr(i))
             if(new_priority>=curr_priority) then
                j=i
                curr_priority=new_priority
             endif
          endif
       enddo
       instr_scheduled=j
       call schedule(instr_scheduled)
    enddo
    ptr_to_start=instr_ptr

    if(dump_block_schedules) then
       ll=ptr_to_start
       i=ll-1
       write(22,*) '==========AFTER======'
       do while(ll/=0)
          write(22,'(a1,i7,a20,10i6)') merge(' ','*',ll>i),&
               ll,op_names(oz%codes(ll+2)),&
               oz%codes(ll+comp_op_arg0:ll+comp_op_arg0+&
               oz%codes(ll+comp_op_nargs)-1)
          i=ll
          ll=oz%codes(ll)
       enddo
       write(22,*) '======================='
    endif
    
10  continue
    
    deallocate(instr)
    deallocate(clash)

  contains

    subroutine schedule(i)
      integer,intent(in):: i
      integer:: index
      if(debug_opt) then
         write(*,*) 'Scheduled:',i
         write(*,*) op_names(oz%codes(instr(i)%index+2))
      endif
      index=instr(i)%index
      oz%codes(index)=instr_ptr
      instr_ptr=index
      instr(i)%kind=-999
      num_scheduled=num_scheduled+1
      if(debug_opt) write(*,*) 'num',num_scheduled,ninstr
    end subroutine schedule

    ! Priority of instruction considered alone
    function solo_priority(i) result(priority)
      type(instruction),intent(in):: i
      integer:: priority
      priority=iand(i%kind,instr_priority_mask+instr_is_shared)
    end function solo_priority

    ! Priority of instruction in light of last
    ! scheduled instruction
    function co_priority(i) result(priority)
      type(instruction),intent(in):: i
      integer:: priority
      priority=0
      if(iand(i%kind,instr_is_shared)==&
           iand(instr(instr_scheduled)%kind,instr_is_shared)) then
         priority=priority+4
      endif
      if(i%ve==instr(instr_scheduled)%ve) then
         priority=priority+2
      elseif(i%cove==instr(instr_scheduled)%ve) then
         priority=priority+1
      endif
      priority=iand(i%kind,instr_priority_mask)+priority*(instr_priority_mask+1)
    end function co_priority

  end subroutine schedule_block

  ! ==============================================
  ! Do instruction i and instruction j conflict?
  ! ==============================================
  function conflict(oz,instr,mask,i,j) result(yes)
    type(oz_state):: oz
    type(instruction),dimension(:),intent(in):: instr
    integer(pm_ln),dimension(oz%nmask,2,*):: mask
    integer,intent(in):: i,j
    logical:: yes
    integer:: ii,jj

    if(iand(instr(i)%kind,instr_is_loop_break)/=0.and.&
         iand(instr(j)%kind,instr_is_shared)==0) then
       yes=.true.
    elseif(iand(instr(i)%bloom,instr(j)%bloom)==0) then
       yes=.false.
    else
       yes=any(iand(mask(:,1,i),mask(:,2,j))/=0).or.&
            any(iand(mask(:,2,i),mask(:,1,j))/=0)
    endif
  end function conflict

  !==========================================================
  ! Process instruction at oz%codes(l)
  ! - fill in instr entry
  ! - set bits in mask for all used variables
  ! - update outer_bloom and outer_mask
  !==========================================================
  recursive subroutine process_instr(oz,l,instr,mask,outer_mask,outer_bloom)
    type(oz_state):: oz
    integer,intent(in):: l
    type(instruction),intent(inout):: instr
    integer(pm_ln),intent(inout),dimension(oz%nmask,2):: mask,outer_mask
    integer(pm_ln),intent(inout):: outer_bloom
    integer:: j,opcode,n,a,m,kind
    opcode=oz%codes(l+comp_op_opcode)
    if(debug_opt) write(*,*) 'Process instr>',l,op_names(opcode)
    n=oz%codes(l+comp_op_nargs)
    a=l+comp_op_arg0
    instr%index=l
    instr%bloom=0
    instr%ve=oz%codes(a)
    if(oz%codes(a)>0) then
       instr%cove=oz_v2(oz,oz%codes(a))
    else
       instr%cove=0
    endif

    call do_var(oz%codes(a))
    kind=-999
    select case(opcode)
    case(op_if,op_if_shared,op_if_restart,op_if_shared_node)
       kind=instr_is_std
       call do_block(oz%codes(a+1))
       call do_block(oz%codes(a+2))
       m=3
    case(op_over,op_skip_empty)
       kind=instr_is_std+instr_is_shared
       call do_block(oz%codes(a+1))
       m=2
    case(op_head_node)
       kind=instr_is_std
       call do_block(oz%codes(a+1))
       m=2
    case(op_do_loop)
       kind=instr_precedes_loop
       m=1
    case(op_loop)
       kind=instr_is_std
       call do_block(oz%codes(a+1))
       if(oz%codes(l+comp_op_opcode2)/=0) then
          kind=ior(kind,instr_is_shared)
       endif
       m=2
    case(op_comm_loop,op_comm_loop_par,op_comm_proc,op_comm_block,op_inline_shared)
       kind=instr_is_std+instr_is_shared
       call do_block(oz%codes(a+1))
       m=2
    case(op_read_file_tile,op_write_file_tile,&
         op_active,op_get_size)
       kind=instr_is_std+instr_is_shared
       m=1
    case(op_broadcast_val,op_broadcast,op_broadcast_shared,&
         op_isend_offset,op_irecv_offset,op_isend_reply,&
         op_isend_req,op_isend_assn,op_comm_call)
       kind=instr_is_comm+instr_is_shared
       call use_resource(resource_is_comm)
       m=1
    case(op_isend,op_irecv,op_isend_grid,op_irecv_grid,op_bcast_shared_grid)
       kind=instr_is_comm
       call use_resource(resource_is_comm)
       m=1
    case(op_recv,op_recv_grid,op_recv_grid_resend)
       kind=instr_is_sync
       call use_resource(resource_is_comm)
       m=1
    case(op_recv_offset,op_recv_offset_resend,op_bcast_shared_offset,op_recv_reply,op_pop_node)
       kind=instr_is_shared+instr_is_sync
       call use_resource(resource_is_comm)
       m=1
       
    case(op_sync_mess,op_dref,op_wrap)
       kind=instr_is_std+instr_is_shared
       if(opcode==op_sync_mess) kind=ior(kind,instr_is_sync)
       do j=1,n-1
          call do_var(-abs(oz%codes(a+j)))
       enddo
       goto 10
    case(op_remote_send_call,op_collect_call,op_remote_call,op_server_call)
       kind=instr_is_shared+instr_is_sync
       call do_block(oz%codes(a+1))
       call do_block(oz%codes(a+2))
       call use_resource(resource_is_comm)
       m=3
    case(op_bcast_call,op_recv_req_call,op_recv_assn_call)
       kind=instr_is_shared+instr_is_sync
       call do_block(oz%codes(a+1))
       call use_resource(resource_is_comm)
       m=2
    case(op_do_at)
       kind=instr_is_std
       call do_block(oz%codes(a+1))
       m=2
    case(op_nested_loop,op_sync,op_blocked_loop,&
         op_break_loop,op_comm_inline)
       kind=instr_precedes_loop+instr_is_loop_break+instr_is_shared
       m=1
    case(op_print)
       kind=instr_is_std
       call use_resource(resource_is_print)
       m=1
    case default
       kind=instr_is_std
       m=1
    end select
    if(kind<0) call pm_panic('instr scheduling - kind')
    do j=m,n-1
       call do_var(oz%codes(a+j))
    enddo
10  continue
    instr%kind=kind
  contains
    subroutine do_var(v)
      integer,intent(in):: v
      if(debug_opt) write(*,*) 'Process var>',v
      if(v/=0.and.v/=shared_op_flag) then
         call process_var(oz,abs(v),instr%bloom,v<0,mask)
         outer_bloom=ior(outer_bloom,instr%bloom)
         outer_mask=ior(outer_mask,mask)
      endif
    end subroutine do_var

    subroutine do_block(v)
      integer,intent(inout):: v
      if(v/=0) then
         call schedule_block(oz,v,instr%bloom,mask)
         outer_bloom=ior(outer_bloom,instr%bloom)
         outer_mask=ior(outer_mask,mask)
      endif
    end subroutine do_block

    subroutine use_resource(resource)
      integer,intent(in):: resource
      call record_index_use(oz,resource,instr%bloom,.true.,mask)
      outer_bloom=ior(outer_bloom,instr%bloom)
      outer_mask(1,:)=ior(outer_mask(1,:),mask(1,:))
    end subroutine use_resource
    
  end subroutine process_instr

  !==========================================================
  ! Record current instruction has accessed (+modified if set)
  ! variable v (updates bloom and mask)
  !==========================================================
  recursive subroutine process_var(oz,v,bloom,modify,mask)
    type(oz_state):: oz
    integer,intent(in):: v
    integer(pm_ln),intent(inout):: bloom
    integer(pm_ln),intent(inout),dimension(oz%nmask,2):: mask
    logical,intent(in):: modify
    integer:: j,k,index
    logical:: varclash
    if(v==0.or.v==shared_op_flag) return
    k=oz_kind(oz,v)
    select case(k)
    case(v_is_group)
       do j=1,oz_v1(oz,v)
          call process_var(oz,oz_ptr(oz,v,j),bloom,modify,mask)
       enddo
    case(v_is_sub,v_is_vsub)
       call process_var(oz,oz_v1(oz,v),bloom,modify,mask)
       call process_var(oz,oz_v2(oz,v),bloom,modify,mask)
    case(v_is_vect_wrapped,v_is_elem,v_is_unit_elem,v_is_chan_vect,v_is_alias)
       call process_var(oz,oz_v1(oz,v),bloom,modify,mask)
    case(v_is_cove)
       call process_var(oz,oz_v2(oz,v),bloom,modify,mask)
    case(v_is_basic,v_is_ve)
       index=oz%varindex(abs(v))
       if(index==0) then
          index=oz%vindex+1
          oz%varindex(abs(v))=index
          oz%vindex=index
       endif
       call record_index_use(oz,index,bloom,modify,mask)
       if(debug_opt) write(*,*) 'Push var>',index,v
    case(v_is_ctime_const,v_is_const)
       continue
    case default
       write(*,*) 'k=',k
       call pm_panic('oz process-var')
    end select
  end subroutine process_var

  !==========================================================
  ! Record current instruction has accessed (+modified if set)
  ! resource #index - updating bloom and mask
  !==========================================================
  subroutine record_index_use(oz,index,bloom,modify,mask)
    type(oz_state):: oz
    integer,intent(in):: index
    integer(pm_ln),intent(inout):: bloom
    integer(pm_ln),intent(inout),dimension(oz%nmask,2):: mask
    integer(pm_ln):: eg_mask
    logical,intent(in):: modify
    integer:: m,n
    !write(*,*) 'nvars> index',index
    m=mod(index,bit_size(bloom))
    bloom=ibset(bloom,m)
    m=mod(index,bit_size(eg_mask))
    n=index/bit_size(eg_mask)+1
    mask(n,1)=ibset(mask(n,1),m)
    if(modify) mask(n,2)=ibset(mask(n,2),m)
  end subroutine record_index_use

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
    v=oz%vars(abs(n)+i+2)/cvar_flag_mult
  end function oz_ptr
 
end module pm_optimise
