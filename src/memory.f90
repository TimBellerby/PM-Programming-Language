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
module pm_memory
  use pm_kinds
  implicit none

  private

  ! Public routines
  public :: operator(.eq.)
  interface operator(.eq.)
     module procedure pm_ptr_eq
  end interface operator(.eq.)
  public pm_init_gc, pm_spawn, pm_gc
  public pm_ptr_eq, pm_new, pm_new_multi,pm_new_small, pm_new_large
  public pm_assign_new, pm_expand, pm_ptr_assign
  public pm_new_as_root,pm_get_ptr_as_root, pm_copy, pm_assign_copy
  public pm_new_string, pm_concat_string, pm_strval
  public pm_add_root, pm_delete_root
  public pm_numroot, pm_delete_numroot
  public pm_register, pm_delete_register 
  public pm_verify_ptr, pm_dump_tree, pm_panic

  ! Data kinds supported (vkind parameter)
  integer(pm_p),public,parameter:: pm_undef=0
  integer(pm_p),public,parameter:: pm_tiny_int=1
  integer(pm_p),public,parameter:: pm_proc=2
  integer(pm_p),public,parameter:: pm_type=3
  integer(pm_p),public,parameter:: pm_name=4
  integer(pm_p),public,parameter:: pm_null=5
  integer(pm_p),public,parameter:: pm_int=6
  integer(pm_p),public,parameter:: pm_long=7
  integer(pm_p),public,parameter:: pm_int8=8
  integer(pm_p),public,parameter:: pm_int16=9  
  integer(pm_p),public,parameter:: pm_int32=10
  integer(pm_p),public,parameter:: pm_int64=11
  integer(pm_p),public,parameter:: pm_int128=12
  integer(pm_p),public,parameter:: pm_single=13
  integer(pm_p),public,parameter:: pm_double=14
  integer(pm_p),public,parameter:: pm_real32=15
  integer(pm_p),public,parameter:: pm_real64=16
  integer(pm_p),public,parameter:: pm_real128=17
  integer(pm_p),public,parameter:: pm_single_complex=18
  integer(pm_p),public,parameter:: pm_double_complex=19
  integer(pm_p),public,parameter:: pm_complex64=20
  integer(pm_p),public,parameter:: pm_complex128=21  
  integer(pm_p),public,parameter:: pm_complex256=22
  integer(pm_p),public,parameter:: pm_logical=23
  integer(pm_p),public,parameter:: pm_packed_logical=24
  integer(pm_p),public,parameter:: pm_string=25
  integer(pm_p),public,parameter:: pm_ext=26
  integer(pm_p),public,parameter:: pm_pointer=27
  integer(pm_p),public,parameter:: pm_stack=28
  integer(pm_p),public,parameter:: pm_usr=29

  integer(pm_p),public,parameter:: pm_num_vkind=29

  ! Language Thread - offsets
  integer,public,parameter:: pm_thread_next=0
  integer,public,parameter:: pm_thread_last=1
  integer,public,parameter:: pm_thread_func=2
  integer,public,parameter:: pm_thread_pc=3  
  integer,public,parameter:: pm_thread_stack=4
  integer,public,parameter:: pm_thread_inputs=5
  integer,public,parameter:: pm_thread_group=6
  integer,public,parameter:: pm_thread_size=7
  
  ! Stack Block - offsets
  integer,public,parameter:: pm_stack_offset=0
  integer,public,parameter:: pm_stack_pc=1
  integer,public,parameter:: pm_stack_oldstack=2
  integer,public,parameter:: pm_stack_func=3
  integer,public,parameter:: pm_stack_locals=4

  ! Various size parameters
  integer(pm_f):: eg_flag
  integer,parameter:: word_bits=bit_size(eg_flag)
  integer(pm_p),public,parameter:: pm_mark_size=128  
  ! pm_mark_size should be a power of 2
  integer,parameter:: block_size = pm_mark_size * word_bits
  integer,public,parameter:: pm_large_obj_size = block_size/2
  integer,public,parameter:: pm_default_free_cache_size=16*1024*block_size
  integer,parameter:: save_list_size = block_size/2
  integer,parameter:: stack_size = block_size*8
  integer,parameter:: max_blocks = 32
  integer,parameter:: max_ticks = 32
  integer,parameter:: max_push = 182
  integer,parameter:: init_free_roots=16
  integer,parameter:: max_free_roots=1024
  integer,parameter:: init_free_regs=16
  integer,parameter:: max_free_regs=256
  integer,parameter:: numroot_step=1024

  ! Represents pointer to element of the heap 
  type,public:: pm_ptr
     type(pm_block),pointer:: data
     integer(pm_p):: offset
  end type pm_ptr

  ! Pointer to root object
  type,public:: pm_root
     type(pm_ptr):: ptr
     type(pm_root),pointer:: last,next
  end type pm_root

  ! Pointer to pm_ptr
  type,public:: pm_ptr_ptr
     type(pm_ptr),pointer:: p
  end type pm_ptr_ptr

  ! Variable registration record
  type,public:: pm_reg
     type(pm_reg),pointer:: next,last
     integer:: num
     type(pm_ptr_ptr),dimension(8)::reg
     type(pm_ptr),dimension(:),pointer:: array
     integer,pointer:: asize
     integer:: asiz
     character(len=10):: tag
  end type pm_reg

  ! Numbered root
  type numroot
     type(pm_ptr):: ptr
     integer(pm_ln):: next
  end type numroot

  ! A block of heap memory
  type,public:: pm_block
     type(pm_block),pointer :: next,last
     type(pm_context),pointer:: context
     integer:: magic
     integer(pm_ln):: esize,size,hash
     integer(pm_p):: vkind, next_sweep, tick
     integer(pm_f),dimension(pm_mark_size):: marks

     ! Data blocks for various types
     integer,allocatable,dimension(:):: i
     integer(pm_ln),allocatable,dimension(:):: ln
     integer(pm_i8),allocatable,dimension(:):: i8
     integer(pm_i16),allocatable,dimension(:):: i16
     integer(pm_i32),allocatable,dimension(:):: i32
     integer(pm_i64),allocatable,dimension(:):: i64
     integer(pm_i128),allocatable,dimension(:):: i128
     real,allocatable,dimension(:):: r
     real(pm_d),allocatable,dimension(:):: d
     real(pm_r32),allocatable,dimension(:):: r32
     real(pm_r64),allocatable,dimension(:):: r64
     real(pm_r128),allocatable,dimension(:):: r128
     complex,allocatable,dimension(:):: c
     complex(pm_d),allocatable,dimension(:):: dc
     complex(pm_r32),allocatable,dimension(:):: c64
     complex(pm_r64),allocatable,dimension(:):: c128
     complex(pm_r128),allocatable,dimension(:):: c256
     character(len=1),dimension(:),allocatable:: s     
     logical,allocatable,dimension(:):: l
     logical(pm_pl),allocatable,dimension(:):: pl
     type(pm_ptr),allocatable,dimension(:):: ptr
  end type pm_block

  type pm_block_ptr
     type(pm_block),pointer:: p
  end type pm_block_ptr

  ! Per-system-process memory structure
  type,public::pm_context
     type(pm_context),pointer:: next,heap
     type(pm_block),pointer:: new_large, heap_large
     type(pm_ptr),dimension(stack_size):: stack
     type(pm_ptr),dimension(save_list_size):: save_list
     logical:: overflow
     integer:: top, next_save
     integer:: blocks_allocated, tick
     type(pm_root),pointer :: roots
     type(pm_root),pointer :: free_roots
     integer:: num_free_roots
     type(pm_reg),pointer:: regs
     type(pm_reg),pointer:: free_regs
     integer:: num_free_regs
     type(pm_ptr):: temp_obj1,temp_obj2,temp_obj3
     type(pm_ptr),&
          dimension(pm_large_obj_size,pm_int:pm_num_vkind):: obj_list
     type(pm_ptr):: threads_start,threads_end,running,wait_start,wait_end
     type(pm_ptr):: tcache,pcache,names,lnames,funcs
     type(pm_block_ptr),dimension(pm_int:pm_num_vkind):: &
          free_blocks, old_free_blocks,free_large_blocks,&
          old_free_large_blocks
     integer(pm_ln):: free_block_count,free_cache_size
     type(numroot),dimension(:),pointer:: numroots
     integer(pm_ln):: last_numroot, max_numroots, free_numroots
     integer(pm_ln):: hash
  end type pm_context

  integer,parameter,public:: pm_debug_level=0
  ! 0= basic checks
  ! 1= print short additional info
  ! 2= print substantial additional info
  ! 3= check everything

  type(pm_ptr),public:: pm_null_obj,pm_tinyint_obj,pm_name_obj

contains


  !======================================================
  !  Public routines
  !======================================================

  ! Initialise memory management
  function pm_init_gc() result(context)
    type(pm_context),pointer:: context
    allocate(context)
    call init_context(context,.true.)
    if(pm_debug_level>1) &
         write(*,*) 'GC initialisation completed'
  end function pm_init_gc

  ! Span a new process (structure for system thread)
  function pm_spawn(heap) result(context)
    type(pm_context),pointer:: heap,context
    allocate(context)
    call init_context(context,.false.)
    context%heap=>heap
    context%next=>heap%next
    heap%next=>context
    context%funcs=heap%funcs
  end function pm_spawn

  ! Add a numbered root
  function pm_numroot(context,ptr) result(n)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ptr
    integer(pm_ln):: n
    type(numroot),dimension(:),pointer:: s
    if(context%free_numroots>0) then
       n=context%free_numroots
       context%free_numroots=context%numroots(n)%next
    else
       context%last_numroot=context%last_numroot+1
       n=context%last_numroot
       if(n>context%max_numroots) then
          context%max_numroots=context%max_numroots+numroot_step
          s=>context%numroots
          allocate(context%numroots(context%max_numroots))
          context%numroots(1:size(s))=s(1:size(s))
          deallocate(s)
       endif
    endif
    context%numroots(n)%ptr=ptr
  end function pm_numroot
  
  !  Delete a numbered root
  subroutine pm_delete_numroot(context,n)
    type(pm_context),pointer:: context
    integer(pm_ln),intent(in):: n
    context%numroots(n)%next=context%free_numroots
    context%numroots(n)%ptr=pm_null_obj
    context%free_numroots=n
  end subroutine  pm_delete_numroot

  ! Add a root object
  function pm_add_root(context,ptr) result(root)
    type(pm_context):: context
    type(pm_ptr):: ptr
    type(pm_root),pointer:: root
    integer:: status
    if(context%num_free_roots>0) then
       root=>context%free_roots
       context%free_roots=>context%free_roots%next
       context%num_free_roots=context%num_free_roots-1
    else
       allocate(root,stat=status)
       if(status/=0) call pm_panic("Cannot allocate root")
    endif
    root%next=>context%roots%next
    context%roots%next=>root
    root%last=>context%roots
    if(associated(root%next)) root%next%last=>root
    root%ptr=ptr
  end function  pm_add_root

  ! Delete a root object
  subroutine pm_delete_root(context,root)
    type(pm_context):: context
    type(pm_root),pointer:: root
    if(associated(root%last)) &
         root%last%next=>root%next
    if(associated(root%next)) &
         root%next%last=>root%last
    if(context%num_free_roots<max_free_roots) then
       root%next=>context%free_roots
       context%free_roots=>root
       context%num_free_roots=context%num_free_roots+1
    else
       deallocate(root)
    endif
  end subroutine pm_delete_root

  ! Register pointer variables for garbage collection
  function pm_register(context,tag,ptr1,ptr2,ptr3,ptr4,ptr5,&
       ptr6,ptr7,ptr8,array,array_size) result(reg)
    type(pm_context):: context
    character(len=*):: tag
    type(pm_ptr),target,optional:: &
         ptr1,ptr2,ptr3,ptr4,ptr5,ptr6,ptr7,ptr8
    type(pm_ptr),target,dimension(:),optional:: array
    integer,target,optional:: array_size
    type(pm_reg),pointer:: reg
    integer:: num,status,i
    if(context%num_free_regs>0) then
       reg=>context%free_regs
       context%free_regs=>context%free_regs%next
       context%num_free_regs=context%num_free_regs-1
    else
       allocate(reg,stat=status)
       if(status/=0) call pm_panic("Cannot allocate reg")
    endif
    reg%tag=tag
    reg%next=>context%regs%next
    context%regs%next=>reg
    reg%last=>context%regs
    if(associated(reg%next)) reg%next%last=>reg
    num=0
    if(present(ptr1)) then
       num=num+1
       reg%reg(1)%p=>ptr1
       ptr1=pm_null_obj
       if(present(ptr2)) then
          num=num+1
          reg%reg(2)%p=>ptr2
          ptr2=pm_null_obj
          if(present(ptr3)) then
             num=num+1
             reg%reg(3)%p=>ptr3
             ptr3=pm_null_obj
             if(present(ptr4)) then
                num=num+1
                reg%reg(4)%p=>ptr4
                ptr4=pm_null_obj
                if(present(ptr5)) then
                   num=num+1
                   reg%reg(5)%p=>ptr5
                   ptr5=pm_null_obj
                   if(present(ptr6)) then
                      num=num+1
                      reg%reg(6)%p=>ptr6
                      ptr6=pm_null_obj
                      if(present(ptr7)) then
                         num=num+1
                         reg%reg(7)%p=>ptr7
                         ptr7=pm_null_obj
                         if(present(ptr8)) then
                            num=num+1
                            reg%reg(8)%p=>ptr8
                            ptr8=pm_null_obj
                         endif
                      endif
                   endif
                endif
             endif
          endif
       endif
    endif
    reg%num=num

    if(present(array)) then
       if(present(array_size)) then
          reg%asize=>array_size
       else
          reg%asize=>reg%asiz
          reg%asiz=size(array)
       endif
       reg%array=>array
       array(:)=pm_null_obj
    else
       nullify(reg%array)
    endif
  end function  pm_register

  ! Delete a register
  subroutine pm_delete_register(context,reg)
    type(pm_context):: context
    type(pm_reg),pointer:: reg
    reg%last%next=>reg%next
    if(associated(reg%next)) reg%next%last=>reg%last
    if(context%num_free_regs<max_free_regs) then
       reg%next=>context%free_regs
       context%free_regs=>reg
       context%num_free_regs=context%num_free_regs+1
    else
       deallocate(reg)
    endif
  end subroutine pm_delete_register

  ! Equality for pointer type
  elemental function pm_ptr_eq(ptr1,ptr2) result(eq)
    type(pm_ptr),intent(in):: ptr1,ptr2
    logical:: eq
    eq=associated(ptr1%data,ptr2%data).and.ptr1%offset==ptr2%offset
  end function pm_ptr_eq

  ! Return pointer within object as root
  function pm_get_ptr_as_root(context,obj,n) result(root)
    type(pm_context):: context
    type(pm_ptr),intent(in):: obj
    integer(pm_ln):: n
    type(pm_root),pointer:: root
    if(pm_debug_level>0) then
       call pm_verify_ptr(obj,'Get ptr')
       if(n<0.or.n>obj%data%esize) &
            call pm_panic('Get ptr: n out of range')
    endif
    root=>pm_add_root(context,obj%data%ptr(obj%offset+n))
  end function pm_get_ptr_as_root

  ! Assignment for pointer objects
  subroutine pm_ptr_assign(context,obj,n,ptr)
    type(pm_context),pointer:: context
    type(pm_ptr):: obj, ptr
    integer(pm_ln):: n
    if(pm_debug_level>0) then
       call pm_verify_ptr(obj,'Ptr assign dest')
       call pm_verify_ptr(ptr,'Ptr assign source')
       if(n<0.or.n>obj%data%esize) &
            call pm_panic('Ptr assign n out of range')
    endif
    obj%data%ptr(obj%offset+n)=ptr
    if(marked(obj)) then
       if(.not.marked(ptr)) then
          call mark(ptr)
          context%save_list(context%next_save)=ptr
          context%next_save=context%next_save+1
          if(context%next_save>save_list_size) &
               call pm_gc(context,.false.)
       endif
    endif
  end subroutine pm_ptr_assign

  ! Create new object and assign to slot in ptr object
  subroutine pm_assign_new(context,obj,n,vkind,esize,clear)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    integer(pm_ln),intent(in):: n
    integer(pm_p),intent(in):: vkind
    integer(pm_ln),intent(in):: esize
    logical,intent(in):: clear
    type(pm_ptr):: new_obj
    if(pm_debug_level>0) then
       call pm_verify_ptr(obj,'Assign new dest')
       if(obj%data%vkind<pm_pointer) &
            call pm_panic('Assign new on non ptr obj')
       if(n<0.or.n>obj%data%esize) &
            call pm_panic('Assign new: n out of scope')
    endif
    context%temp_obj1=obj
    new_obj=pm_new(context,vkind,esize)
    obj%data%ptr(obj%offset+n)=new_obj
    if(clear) call nullify_obj(new_obj,&
         0_pm_ln,esize-1)
    context%temp_obj1=pm_null_obj
  end subroutine pm_assign_new

  ! Expand object pointed to by given slot
  subroutine pm_expand(context,obj,n,newsize)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    integer(pm_ln),intent(in):: n,newsize
    integer i
    if(pm_debug_level>0) then
       call pm_verify_ptr(obj,'Expand: obj')
       if(n<0.or.n>obj%data%esize) &
            call pm_panic('Expand bad slot no')
       if(newsize<=0)&
            call pm_panic('Expand: Negative new size') 
    endif
    context%temp_obj2=obj%data%ptr(obj%offset+n)
    if(pm_debug_level>0) then 
       call pm_verify_ptr(context%temp_obj2,'Expanding: slot')
    endif
    call pm_assign_new(context,obj,n,&
         context%temp_obj2%data%vkind, &
         newsize,.true.)
    call copy_obj(context%temp_obj2,0_pm_ln,obj%data%ptr(obj%offset+n),0_pm_ln, &
         context%temp_obj2%data%esize)
    call nullify_obj(obj%data%ptr(obj%offset+n), &
         context%temp_obj2%data%esize+1,newsize-1)
    context%temp_obj2=pm_null_obj
  end subroutine pm_expand

  ! Create a new object
  function pm_new(context,vkind,esize) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_p),intent(in):: vkind
    integer(pm_ln),intent(in):: esize
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       if(vkind<pm_int.or.vkind>pm_usr) &
            call pm_panic('New - bad vkind')
       if(esize<=0) &
            call pm_panic('New - non +ve esize')
    endif
    if(esize<=pm_large_obj_size) then
       ptr=pm_new_small(context,vkind,int(esize,pm_p))
    else
       ptr=pm_new_large(context,vkind,esize)
    endif
  end function pm_new

  ! Create multiple new objects -- assign to locations loc,loc+1,...,loc+n-1 in vect
  subroutine pm_new_multi(context,vkind,esize,loc,n,vect)
    type(pm_context),pointer:: context
    integer(pm_p),intent(in):: vkind
    integer(pm_ln),intent(in):: loc,n,esize
    type(pm_ptr),intent(in):: vect
    integer(pm_ln):: i,j,k,m
    type(pm_ptr):: ptr
    type(pm_ptr),pointer:: ptr_p
    logical:: is_marked
    is_marked=marked(vect)
    if(esize<=pm_large_obj_size) then
       i=0
       do while(i<n)
          ptr_p=>context%obj_list(esize,vkind)
          ptr=ptr_p
          if(associated(ptr%data)) then
             if(ptr%offset<ptr%data%next_sweep) then
                m=min(ptr%data%next_sweep-ptr%offset-1_pm_ln,n-i-1_pm_ln)
                do k=0,m
                   j=loc+i+k
                   vect%data%ptr(vect%offset+j)=ptr
                   if(is_marked) call mark(ptr)
                   ptr%offset=ptr%offset+1_pm_p
                enddo
                i=i+m
                ptr_p=ptr
                cycle
             endif
          endif
          j=loc+i
          i=i+1_pm_ln
          vect%data%ptr(vect%offset+j)=pm_new_small(context,vkind,int(esize,pm_p))
          if(is_marked) call mark(ptr)
       enddo
    else
       do i=0,n-1
          j=loc+i
          ptr=pm_new_large(context,vkind,esize)
          vect%data%ptr(vect%offset+j)=ptr
          if(is_marked) call mark(ptr)
       enddo
    endif
  end subroutine pm_new_multi

  ! Return a new object as a root
  function pm_new_as_root(context,vkind,esize) result(root)
    type(pm_context),pointer:: context
    integer(pm_p),intent(in):: vkind
    integer(pm_ln),intent(in):: esize
    type(pm_root),pointer:: root
    type(pm_ptr):: ptr
    ptr=pm_new(context,vkind,esize)
    root=>pm_add_root(context,ptr)
  end function pm_new_as_root

  ! Create a new object of <= pm_large_obj_size elements
  function pm_new_small(context,vkind,esize) result(nptr)
    type(pm_context),pointer:: context
    integer(pm_p),intent(in):: vkind,esize
    type(pm_ptr):: nptr
    type(pm_ptr):: ptr
    type(pm_block),pointer:: oldblk
    type(pm_ptr),pointer:: ptr_p
    integer:: gcycle
    logical:: ok
    integer:: i
    
    if(pm_debug_level>0) then
       if(vkind<pm_null.or.vkind>pm_usr) &
            call pm_panic('New small - bad vkind')
       if(esize<=0.or.esize>pm_large_obj_size) &
            call pm_panic('New small - bad esize')
    endif

    ! Get allocation slot for kind and size
    ptr_p=>context%obj_list(esize,vkind)
    ptr=ptr_p

    if(associated(ptr%data)) then
       if(ptr%offset<ptr%data%next_sweep) then
          ! There is room in current run 
          nptr=ptr
          ptr_p%offset=ptr%offset+esize
          goto 10
       endif
    else
       ! No blocks at all for this slot - allocate one
       if(context%blocks_allocated>max_blocks) &
            call pm_gc(context,.false.)
       ptr%data=>new_block(context,vkind,int(esize,pm_ln))
       ptr%offset=1
       ptr%data%next=>ptr%data
       ptr%data%next_sweep=ptr%data%size-esize+1
       ptr_p%data=>ptr%data
       ptr_p%offset=1+esize
       nptr=ptr
       goto 10
    endif

    ! Check if GC has run since last use of this slot   
    if(ptr%data%tick<context%tick) then
        ! Restart scan from start of block
       ptr%data%tick=context%tick
       ptr%offset=1-esize
    endif

    ! Lazy sweep - find free slot
    call next_free()
    if(.not.ok) then
       ! Failed - call GC
       call pm_gc(context,.false.)
       ! Restart sweep from start of current block
       ptr%offset=1-esize
       ptr%data%tick=context%tick
       ! Try again
       call next_free()
       if(.not.ok) then
          ! All full - allocate new block
          ptr%data=>new_block(context,vkind,int(esize,pm_ln))
          ptr%offset=1
          ptr%data%next_sweep=ptr%data%size-esize+1
          ptr%data%next=>ptr_p%data%next
          ptr_p%data%next=>ptr%data
          ptr_p%data=>ptr%data
          ptr_p%offset=1+esize
          nptr=ptr
          goto 10
       endif
    endif

    ! Found free object
    nptr=ptr
    
    ! Find following free location
    call next_free()
    if(.not.ok) then
       ! No following loc - force gc next time
       ! allocation attempted for this slot
       ptr%data%next_sweep=ptr%data%size+1
       ptr%offset=ptr%data%size+1
       ptr_p=ptr
       goto 10
    endif

    ! Make alloc pointer point at this location
    ptr_p=ptr

    ! Find extent of run of free locations
    ptr%offset=ptr%offset+esize
    do 
       if(ptr%offset+esize>ptr%data%size+1) then
          ptr%offset=ptr%data%size+1
          exit
       endif
       if(marked(ptr)) exit
       ptr%offset=ptr%offset+esize
    enddo
    ptr%data%next_sweep=ptr%offset

 10 continue



    ! Always initialise pointers
    if(vkind>=pm_pointer) &
         nptr%data%ptr(nptr%offset:nptr%offset+esize-1)=pm_null_obj
 

  contains

    ! Find next free location in circular chain of blocks
    subroutine next_free()
      ptr%offset=ptr%offset+esize
      do
         if(ptr%offset+esize>ptr%data%size+1) then
            if(ptr%data%next%tick>=context%tick) then
               ! Scanned this block before - no hope
               ok=.false.
               return
            endif
            ! Advance to next block on chain
            ptr%data=>ptr%data%next
            ptr%offset=1
            ptr%data%tick=context%tick
         endif
         if(.not.marked(ptr)) exit
         ptr%offset=ptr%offset+esize
      enddo
    end subroutine next_free

  end function pm_new_small

  ! Create a new object of >= pm_large_obj_size elements
  function pm_new_large(context,vkind,esize) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_p),intent(in):: vkind
    integer(pm_ln):: esize
    type(pm_ptr):: ptr
    type(pm_block),pointer:: blk

    if(pm_debug_level>0) then
       if(vkind<pm_null.or.vkind>pm_usr) &
            call pm_panic('New large - bad vkind')
       if(esize<=0) &
            call pm_panic('New large -ve esize')
    endif

    if(context%blocks_allocated>max_blocks) &
         call pm_gc(context,.false.)
    blk=>new_block(context,vkind,esize)
    blk%next=>context%new_large
    context%new_large=>blk
    ptr%data=>blk
    ptr%offset=1
    if(vkind>=pm_pointer) &
         ptr%data%ptr(ptr%offset:ptr%offset+esize-1)=pm_null_obj
  end function pm_new_large

  ! Create new string object from FORTRAN string
  function pm_new_string(context,string) result(ptr)
    type(pm_context),pointer:: context
    character(len=*),intent(in):: string
    type(pm_ptr):: ptr
    integer::i,n
    n=len(string)
    ptr=pm_new(context,pm_string,int(n,pm_ln))
    do i=1,n
       ptr%data%s(ptr%offset+i-1)=string(i:i)
    enddo
  end function pm_new_string

  ! Get FORTRAN string from PM string
  subroutine pm_strval(ptr,str)
    type(pm_ptr),intent(in):: ptr
    character(len=*),intent(out):: str
    integer(pm_ln):: i
    if(pm_debug_level>0) then
       call pm_verify_ptr(ptr,'Strval')
    endif
    str=' '
    do i=1,min(len(str),ptr%data%esize+1)
       str(i:i)=ptr%data%s(ptr%offset+i-1)
    enddo
  end subroutine pm_strval

  ! Concatenate two string objects to create new object
  function pm_concat_string(context,obj1,obj2) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj1,obj2
    type(pm_ptr):: ptr
    integer(pm_ln)::i,j,n
    if(pm_debug_level>0) then
       call pm_verify_ptr(obj1,'Concat obj1')
       call pm_verify_ptr(obj2,'Concat obj2')
    endif
    i=obj1%data%esize
    j=obj2%data%esize
    n=i+1+j
    ptr=pm_new(context,pm_string,n+1)
    ptr%data%s(ptr%offset:ptr%offset+i)=&
         obj1%data%s(obj1%offset:obj1%offset+i)
    ptr%data%s(ptr%offset+i+1:ptr%offset+n)=&
         obj2%data%s(obj2%offset:obj2%offset+j)
  end function pm_concat_string

  ! Shallow copy an object
  function pm_copy(context,obj) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    type(pm_ptr):: ptr
    
    if(pm_debug_level>0) then
       call pm_verify_ptr(obj,'Copy')
    endif

    if(obj%data%vkind<=pm_null) then
       ptr=obj
    else
       context%temp_obj1=obj
       ptr=pm_new(context,obj%data%vkind,obj%data%esize+1)
       call copy_obj(ptr,0_pm_ln,context%temp_obj1,0_pm_ln,context%temp_obj1%data%esize)
       context%temp_obj1=pm_null_obj
    endif
  end function pm_copy

  ! Shallow copy an object
  subroutine pm_assign_copy(context,obj2,n,obj)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj,obj2
    integer(pm_ln):: n
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       call pm_verify_ptr(obj2,'Assign copy obj2')
       call pm_verify_ptr(obj,'Assign copy obj')
       if(n<0.or.n>obj2%data%esize) &
            call pm_panic('Assign copy n out of scope')
    endif
    if(obj%data%vkind<=pm_null) then
       obj2%data%ptr(obj2%offset+n)=obj
    else
       context%temp_obj1=obj
       context%temp_obj2=obj2
       context%temp_obj3=pm_new(context,obj%data%vkind,obj%data%esize+1)
       call copy_obj(ptr,0_pm_ln,context%temp_obj1,0_pm_ln,context%temp_obj1%data%esize)
       context%temp_obj2%data%ptr(context%temp_obj2%offset+n)=&
            context%temp_obj3
       context%temp_obj1=pm_null_obj
       context%temp_obj2=pm_null_obj
       context%temp_obj3=pm_null_obj
    endif
  end subroutine  pm_assign_copy

  ! Dump a tree - used mainly for debugging
  recursive subroutine pm_dump_tree(context,iunit,ptr,depth)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: depth
    character(len=100):: spaces
    integer:: i
    spaces=' '
    call pm_verify_ptr(ptr,'Dumping tree',.true.)
    select case(ptr%data%vkind)
    case(pm_name)
       write(iunit,*) spaces(1:depth*2),'Name: ',ptr%offset
    case(pm_tiny_int)
       write(iunit,*) spaces(1:depth*2),'Tiny int: ',ptr%offset
    case(pm_null)
       write(iunit,*) spaces(1:depth*2),'Null: ',ptr%offset
    case(pm_int)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Small Int:',&
               ptr%data%i(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Small Int',ptr%data%esize,'('
          do i=0,min(ptr%data%esize,19)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%i(ptr%offset+i)
          enddo
          if(ptr%data%esize>19) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_long)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Large int:',&
               ptr%data%ln(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Large Int('
          do i=0,min(ptr%data%esize,19)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%ln(ptr%offset+i)
          enddo
          if(ptr%data%esize>19) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_int8)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Int8:',ptr%data%i8(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Int8('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%i8(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_int16)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Int16:',ptr%data%i16(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Int16('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%i16(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_int32)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Int32:',&
               ptr%data%i32(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Int32('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%i32(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_int64)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Int64:',&
               ptr%data%i64(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Int64('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%i64(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_int128)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Int128:',&
               ptr%data%i128(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Int128('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%i128(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_single)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Single:',ptr%data%r(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Single('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%r(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_double)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'double:',ptr%data%d(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'double('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%d(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_real32)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Float32:',&
               ptr%data%r32(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Float32('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%r32(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_real64)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Float64:',&
               ptr%data%r64(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Float64('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%r64(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_real128)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Float128:',&
               ptr%data%r128(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Float128('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%r128(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_single_complex)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Single Complex:',&
               ptr%data%c(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Single Complex('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%c(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_double_complex)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Double Complex:',&
               ptr%data%dc(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Double Complex('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%dc(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_complex64)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Complex64:',&
               ptr%data%c64(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Complex64('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%c64(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_complex128)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Complex128:',&
               ptr%data%c128(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Complex128('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%c128(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
   case(pm_complex256)
       if(ptr%data%esize==0) then
          write(iunit,*) spaces(1:depth*2),'Complex256:',&
               ptr%data%c256(ptr%offset)
       else
          write(iunit,*) spaces(1:depth*2),'Complex256('
          do i=0,min(ptr%data%esize,9)
             write(iunit,*) spaces(1:depth*2+2),ptr%data%c256(ptr%offset+i)
          enddo
          if(ptr%data%esize>9) write(iunit,*) spaces(1:depth*2+2),'...'
          write(iunit,*) spaces(1:depth*2),')'
       endif
    case(pm_string)
       if(ptr%data%esize<75-depth*2) then
          write(iunit,*) spaces(1:depth*2),'String(',&
               ptr%data%s(ptr%offset:ptr%offset+ptr%data%esize),')'
       else
          write(iunit,*) spaces(1:depth*2),'String(',&
               ptr%data%s(ptr%offset:ptr%offset+75-depth*2),'...)'
       endif
    case(pm_logical)
       if(ptr%data%esize*5<75-depth*2) then
          write(iunit,*) spaces(1:depth*2),'Bool(',&
               ptr%data%l(ptr%offset:ptr%offset+ptr%data%esize),')'
       else
          write(iunit,*) spaces(1:depth*2),'Bool(',&
               ptr%data%l(ptr%offset:ptr%offset+75-depth*2),'...)'
       endif
    case(pm_packed_logical)
       if(ptr%data%esize*5<75-depth*2) then
          write(iunit,*) spaces(1:depth*2),'PackBool(',&
               ptr%data%pl(ptr%offset:ptr%offset+ptr%data%esize),')'
       else
          write(iunit,*) spaces(1:depth*2),'PackBool(',&
               ptr%data%pl(ptr%offset:ptr%offset+75-depth*2),'...)'
       endif
    case(pm_pointer:pm_usr)
       if(ptr%data%vkind==pm_pointer) then
          write(iunit,*) spaces(1:depth*2),'Pointer(',ptr%data%esize
       else if(ptr%data%vkind==pm_stack) then
          write(iunit,*) spaces(1:depth*2),'Stack(',ptr%data%esize
       else
          write(iunit,*) spaces(1:depth*2),'User(',ptr%data%esize
       endif
       if(depth>49) then
          write(iunit,*) spaces,'>>more'
       else
          do i=0,min(ptr%data%esize,19)
             call pm_dump_tree(context,iunit,ptr%data%ptr(ptr%offset+i),&
                  depth+1)
          enddo
          if(ptr%data%esize>19) write(iunit,*) spaces(1:depth*2+2),'...'
       endif
       write(iunit,*) spaces(1:depth*2),')'
    case default
       write(iunit,*) spaces(1:depth*2),'CORRUPT PTR!!'
    end select
  end subroutine pm_dump_tree

  ! Verify a pointer (used for debugging)
  subroutine pm_verify_ptr(ptr,emess,nocrash)
    type(pm_ptr):: ptr
    character(len=*)::emess
    logical,optional:: nocrash
    if(.not.associated(ptr%data)) then
       write(*,*) 'Non associated pointer'
       write(*,*) ptr%data%vkind
       if(.not.present(nocrash))&
            call pm_panic('corrupt memory')
    else if(ptr%data%magic/=1234567.or.ptr%data%vkind<0&
         .or.ptr%data%vkind>pm_num_vkind) then
       write(*,*) 'Bad ptr in ',emess,ptr%data%magic,ptr%data%vkind
       if(.not.present(nocrash))&
            call pm_panic('Corrupt memory')
    else if(ptr%data%vkind>pm_null) then
       if(ptr%offset<1.or.ptr%offset>ptr%data%size) then
          write(*,*) 'Bad ptr offset',&
               ptr%offset,ptr%data%size,ptr%data%vkind,' in: ',emess
          if(.not.present(nocrash)) &
               call pm_panic('Corrupt memory')
       endif
    endif
  end subroutine pm_verify_ptr

  ! Run the garbage collector
  subroutine pm_gc(context,force_major_cycle)
    type(pm_context),pointer:: context
    logical,intent(in):: force_major_cycle
    logical:: major_cycle
    integer:: vk,esize,i
    integer(pm_ln):: j
    type(pm_block),pointer:: newlist
    type(pm_root),pointer:: root_obj
    type(pm_reg),pointer:: reg_obj


    if(pm_debug_level>1) write(*,*) '==GC started=='

    major_cycle=force_major_cycle.or.context%tick>max_ticks
    10 continue

    if(pm_debug_level>1) write(*,*) 'Clearing bitmaps'

    ! Set up collection cycle
    call clear_large_obj_marks(context%new_large)
    
    if(major_cycle) then
       ! Clear bitmaps
       do vk=pm_int,pm_num_vkind
          do esize=1,pm_large_obj_size
             if(associated(context%obj_list(esize,vk)%data)) then
                call clear_bitmaps(&
                     context%obj_list(esize,vk)%data)
             endif
          enddo
       enddo
       call clear_large_obj_marks(context%heap_large)
    else
       ! Process save list
       do i=1,context%next_save-1
          call mark_from(context,context%save_list(i))
       enddo
    endif

    if(pm_debug_level>1) write(*,*) 'Marking from temporaries'

    ! Mark from temporary objects
    call mark_from(context,context%temp_obj1)
    call mark_from(context,context%temp_obj2)
    call mark_from(context,context%temp_obj3)
   
    if(pm_debug_level>1) write(*,*) 'Marking caches'

    ! Mark caches
    call mark_from(context,context%tcache)
    call mark_from(context,context%pcache)
    call mark_from(context,context%names)
    call mark_from(context,context%lnames)
    if(.not.associated(context%heap)) then
       call mark_from(context,context%funcs)
    endif

    if(pm_debug_level>1) write(*,*) 'Mark from registered:'

    ! Mark from registered variables
    reg_obj=>context%regs%next

    do while(associated(reg_obj))
       if(pm_debug_level>1) write(*,*) 'Next reg:',trim(reg_obj%tag)
       do i=1,reg_obj%num
          call mark_from(context,reg_obj%reg(i)%p)
       enddo
       if(associated(reg_obj%array)) then
          do i=1,reg_obj%asize
             call mark_from(context,reg_obj%array(i))
          enddo
       endif
       reg_obj=>reg_obj%next
    enddo

    if(pm_debug_level>1) write(*,*) 'Marking from roots'
    
    ! Mark from roots
    root_obj=>context%roots%next
    do while(associated(root_obj))
       call mark_from(context,root_obj%ptr)
       root_obj=>root_obj%next
    enddo

    if(pm_debug_level>1) write(*,*) 'Mark from numbered roots'

    ! Mark from numbered roots
    if(context%last_numroot>0) then
       do j=1,context%last_numroot
          call mark_from(context,context%numroots(j)%ptr)
       enddo
    endif

    if(pm_debug_level>1) write(*,*) 'Mark from threads'

    ! Mark from threads
    call mark_threads_from(context,context%threads_start)
    call mark_threads_from(context,context%wait_start)
    call mark_threads_from(context,context%running)

    if(pm_debug_level>1) write(*,*) 'Check stack overflow'

    ! Deal with mark stack overflows
    if(context%overflow) then
       if(pm_debug_level>1) write(*,*) 'Stack overflow confirmed'
       if(major_cycle) then
          if(pm_debug_level>1) write(*,*) 'Marking from marked'
          do while(context%overflow)
             context%overflow=.false.
             do vk=pm_pointer,pm_usr
                do esize=1,pm_large_obj_size
                   call mark_from_marked(context, &
                        context%obj_list(esize,vk)%data)
                enddo
             enddo
             call mark_from_marked(context,context%heap_large)
          enddo
       else
          if(pm_debug_level>1) write(*,*) 'Restarting as major cycle'
          major_cycle=.true.
          goto 10
       endif
    endif

    if(pm_debug_level>1) write(*,*) 'Migrating new large blocks'

    ! Migrate large blocks to heap
    call migrate_large_blocks(context,context%new_large,&
         context%heap_large)
    
    if(major_cycle) then
       if(pm_debug_level>1) write(*,*) 'Finalize'

       ! finalize any external objects
       call finalize(context)

       if(pm_debug_level>1) write(*,*) 'Clean up blocks'
       
       ! Clean up large object blocks
       newlist=>null()
       call migrate_large_blocks(context,context%heap_large,newlist)
       context%heap_large=>newlist
       ! Clean up any totally empty blocks
       do vk=pm_int,pm_num_vkind
          do esize=1,pm_large_obj_size
             call free_empty_blocks(context,&
                  context%obj_list(esize,vk)%data)
          enddo
       enddo

       ! Ticks start back at 1
       context%tick=1
    else
       ! Next tick
       context%tick=context%tick+1
    endif
    
    ! Clear save list
    context%next_save=1

    ! Reset block count
    context%blocks_allocated=0

    if(pm_debug_level>1) write(*,*) '==GC Finished=='

  end subroutine pm_gc

  !============================================================
  ! Support routines for the memory manager 
  !============================================================

  ! Initialise a context structure
  subroutine init_context(context,startup)
    type(pm_context),pointer:: context
    logical:: startup
    integer:: i,j
    type(pm_root),pointer::  root
    type(pm_reg),pointer:: reg
    if(startup) then
       if(pm_debug_level>1) write(*,*) 'Initialising GC'
       pm_null_obj%data=>new_block(context,pm_null,0_pm_ln)
       pm_null_obj%offset=0
       pm_tinyint_obj%data=>new_block(context,pm_tiny_int,0_pm_ln)
       pm_tinyint_obj%offset=0
       pm_name_obj%data=>new_block(context,pm_name,0_pm_ln)
       pm_name_obj%offset=0
    endif
    forall (i=1:pm_large_obj_size, j=pm_int:pm_num_vkind)
       context%obj_list(i,j)%data=>null()
    end forall
    forall (i=pm_int:pm_num_vkind)
       context%free_blocks(i)%p=>null()
       context%free_large_blocks(i)%p=>null()
       context%old_free_blocks(i)%p=>null()
       context%old_free_large_blocks(i)%p=>null()
    end forall
    context%free_block_count=0
    context%free_cache_size=pm_default_free_cache_size
    nullify(context%next)
    context%heap=>context
    nullify(context%new_large)
    nullify(context%heap_large)
    context%top=0
    context%next_save=1
    context%blocks_allocated=0
    context%tick=1
    allocate(context%roots)
    nullify(context%roots%next)
    nullify(context%roots%last)
    nullify(context%free_roots)
    do i=1,init_free_roots
       allocate(root)
       root%next=>context%free_roots
       context%free_roots=>root
    enddo
    context%num_free_roots=init_free_roots
    allocate(context%regs)
    nullify(context%regs%next)
    nullify(context%regs%last)
    nullify(context%free_regs)
    do i=1,init_free_regs
       allocate(reg)
       reg%next=>context%free_regs
       context%free_regs=>reg
    enddo
    context%num_free_regs=init_free_regs
    context%temp_obj1=pm_null_obj
    context%temp_obj2=pm_null_obj
    context%temp_obj3=pm_null_obj
    context%threads_start=pm_null_obj
    context%threads_end=pm_null_obj
    context%running=pm_null_obj
    context%wait_start=pm_null_obj
    context%wait_end=pm_null_obj
    context%tcache=pm_null_obj
    context%pcache=pm_null_obj
    context%names=pm_null_obj
    context%lnames=pm_null_obj
    context%funcs=pm_null_obj
    context%max_numroots=0
    context%last_numroot=0
    context%free_numroots=0
    context%hash=0
  end subroutine init_context

  ! allocate new data block for given type and size of object
  function new_block(context,vkind,esize) result(blk)
    type(pm_context),pointer:: context
    integer(pm_p),intent(in):: vkind
    integer(pm_ln),intent(in):: esize
    type(pm_block),pointer::blk
    type(pm_block),pointer::parent
    
    integer(pm_ln):: tsize,m
    integer:: status

    ! Calulate block size
    m=(esize+block_size-1)/block_size
    tsize=block_size*m

    ! See if a free block is available in the cache
    if(vkind>pm_pointer.and..false.) then
       if(m==1) then
          if(associated(context%free_blocks(vkind)%p)) then
             blk=>context%free_blocks(vkind)%p
             context%free_blocks(vkind)%p=>blk%next
             context%free_block_count=context%free_block_count-1
             goto 10
          endif
       else
          blk=>context%free_large_blocks(vkind)%p
          if(associated(blk)) then
             if(blk%size==tsize) then
                context%free_large_blocks(vkind)%p=>blk%next
                context%free_block_count=context%free_block_count-m
                goto 10
             endif
             parent=>blk
             blk=>blk%next
             do while(associated(blk))
                if(blk%size==tsize) then
                   parent%next=>blk%next
                   context%free_block_count=context%free_block_count-m
                   goto 10
                endif
                parent=>blk
                blk=>blk%next
             enddo
          endif
       endif
    endif

    ! Allocate main block
    allocate(blk,stat=status)
    if(status/=0) then
       call pm_panic("Out of memory")
    endif

    ! Allocate data area
    select case(vkind)
       case(pm_int)
          allocate(blk%i(tsize),stat=status)       
       case(pm_long)
          allocate(blk%ln(tsize),stat=status)
       case(pm_int8)
          allocate(blk%i8(tsize),stat=status)
       case(pm_int16)
          allocate(blk%i16(tsize),stat=status)
       case(pm_int32)
          allocate(blk%i32(tsize),stat=status)
       case(pm_int64)
          allocate(blk%i64(tsize),stat=status)
       case(pm_int128)
          allocate(blk%i128(tsize),stat=status)
       case(pm_single)
          allocate(blk%r(tsize),stat=status)
       case(pm_double)
          allocate(blk%d(tsize),stat=status)
       case(pm_real32)
         allocate(blk%r32(tsize),stat=status)
       case(pm_real64)
          allocate(blk%r64(tsize),stat=status)
       case(pm_real128)
          allocate(blk%r128(tsize),stat=status)
       case(pm_single_complex)
          allocate(blk%c(tsize),stat=status)
       case(pm_double_complex)
          allocate(blk%dc(tsize),stat=status)
       case(pm_complex64)
          allocate(blk%c64(tsize),stat=status)
       case(pm_complex128)
          allocate(blk%c128(tsize),stat=status)
       case(pm_complex256)
          allocate(blk%c256(tsize),stat=status)
       case(pm_logical)
          allocate(blk%l(tsize),stat=status)
       case(pm_packed_logical)
          allocate(blk%pl(tsize),stat=status)
       case(pm_string)
          allocate(blk%s(tsize),stat=status)
       case(pm_ext)
          allocate(blk%ln(tsize),stat=status)
          blk%ln=0 ! Must clear
       case(pm_pointer:pm_usr)
          allocate(blk%ptr(tsize),stat=status)
    end select

    if(status/=0) then
       call pm_panic("Out of memory")
    endif

    10 continue
    ! Finish initialising block
    blk%magic=1234567
    blk%context=>context
    blk%vkind=vkind
    blk%esize=esize-1
    blk%size=tsize
    blk%next_sweep=0
    blk%tick=context%tick
    blk%hash=context%hash
    context%hash=context%hash+1
    if(context%hash>huge(context%hash)-1) context%hash=0
    if(esize>pm_large_obj_size) then
       blk%marks(1)=0
    else
       blk%marks=0
    endif
    context%blocks_allocated=context%blocks_allocated+m
  end function new_block

  ! Free a block
  subroutine free_block(context,blk)
    type(pm_context),pointer:: context
    type(pm_block),pointer:: blk
    integer:: i
    if(blk%size/block_size>context%free_cache_size/4) then
       deallocate(blk)
       return
    endif
    if(context%free_block_count>context%free_cache_size) then
       do i=1,pm_num_vkind
          if(associated(context%old_free_blocks(i)%p)) &
               call purge(context%old_free_blocks(i)%p)
          context%old_free_blocks(i)%p=>context%free_blocks(i)%p
          if(associated(context%old_free_large_blocks(i)%p)) &
               call purge(context%old_free_large_blocks(i)%p)
          context%old_free_large_blocks(i)%p=>context%old_free_blocks(i)%p
       end do
    endif
    if(blk%size>block_size) then
       blk%next=>context%free_large_blocks(blk%vkind)%p
       context%free_large_blocks(blk%vkind)%p=>blk
    else
       blk%next=>context%free_blocks(blk%vkind)%p
       context%free_blocks(blk%vkind)%p=>blk
    endif
    context%free_block_count=context%free_block_count+blk%size/block_size
  contains
    subroutine purge(ptr)
      type(pm_block),pointer:: ptr
      type(pm_block),pointer:: f
      do while(associated(ptr%next))
         f=>ptr%next
         ptr%next=>f%next
         context%free_block_count=&
              context%free_block_count-f%size/block_size
         deallocate(f)
      enddo
    end subroutine purge
  end subroutine free_block

  ! Check if an object is marked
  function marked(ptr) result(ismarked)
    type(pm_ptr),intent(in)::ptr
    logical:: ismarked
    integer::m,n
    if(ptr%data%vkind<=pm_null) then
       ismarked=.true.
       return
    endif
    n=(ptr%offset-1)/pm_mark_size
    m=iand(ptr%offset-1,pm_mark_size-1)+1
    ismarked=btest(ptr%data%marks(m),n)
  end function marked

  ! Mark an object
  subroutine mark(ptr)
    type(pm_ptr),intent(in)::ptr
    integer::m,n,o,s
    
    if(ptr%data%vkind<=pm_null) then
       return
    elseif(ptr%data%vkind==pm_stack) then
       ! Check for interior pointer
       s=int(ptr%data%esize+1)
       o=((ptr%offset-1)/s)*s+1
    else
       o=ptr%offset
    endif
    n=(o-1)/pm_mark_size
    m=iand(ptr%offset-1,pm_mark_size-1)+1
    ptr%data%marks(m)=ibset(ptr%data%marks(m),n)
  end subroutine  mark
  
  ! Marked all objects pointed to by marked objects 
  ! in blocks on given (possibly circular) linked list
  subroutine mark_from_marked(context,list)
    type(pm_context),pointer:: context
    type(pm_block),pointer:: list
    type(pm_block),pointer:: blk
    integer::i,k,bit
    integer(pm_ln):: j
    blk=>list
    do while(associated(blk))
       do k=1,pm_mark_size
          if(blk%marks(k)/=0) then
             do bit=0,word_bits-1
                if(btest(blk%marks(k),bit)) then
                   i=k+bit*pm_mark_size
                   if(blk%vkind==pm_stack) then
                      i=i+3
                   else if(blk%vkind==pm_usr) then
                      i=i+1
                   endif
                   do j=i,i+blk%esize
                      call mark_from(context,blk%ptr(j))
                   enddo
                endif
             enddo
          endif
       enddo
       blk=>blk%next
       if(associated(blk,list)) exit
    enddo
  end subroutine mark_from_marked
  
  ! Mark all objects directly or indirectly pointed to by
  ! given object
  subroutine mark_from(context,ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ptr
    type(pm_ptr):: ptr2,ptr3,ptr4
    integer(pm_ln):: i,n
    if(pm_debug_level>0) &
         call pm_verify_ptr(ptr,'entering mark_from')
    if(ptr%data%vkind<=pm_null) return
    if(.not.associated(ptr%data%context,context)) return
    call mark(ptr)
    if(ptr%data%vkind<pm_pointer) return
    call push(ptr)
    do while(context%top>0)
       ptr2=pop()
       n=ptr2%data%esize
       if(ptr2%offset<0) then
          ptr2%offset=-ptr2%offset
          i=ptr%data%esize+1
          n=((ptr2%offset-1)/i)*i+i-ptr2%offset
       else
          n=ptr2%data%esize
          if(ptr2%data%vkind==pm_usr) then
             ! First slot is type info - do not scan
             ptr2%offset=ptr2%offset+1
             n=n-1
          elseif(ptr2%data%vkind==pm_stack) then
             ! Stack structure
             ! Slot 0 offset contains stack use info = end of used slots
             ! Slot 1 is PC not scanned
             ! Slot 2 is OLD STACK
             ! Slot 3 is FUNC not scanned
             ! Stack one below last new frame on the chain may be 
             ! dirty -- mark from it directly rather than pushing
             ptr3=ptr2%data%ptr(ptr2%offset+pm_stack_oldstack)
             if(associated(ptr3%data%context,context)) then 
                if(ptr3%data%vkind>pm_null) then 
                   if(marked(ptr3)) then
                      ! OLDSTACK is older frame - mark from it
                      n=ptr3%data%ptr(ptr3%offset)%offset
                      do i=ptr3%offset+3,ptr3%offset+n
                         ptr4=ptr3%data%ptr(ptr3%offset+i)
                         if(associated(ptr4%data%context,context)) then
                            if(.not.marked(ptr4)) then
                               if(ptr%data%vkind>=pm_pointer) then
                                  call mark(ptr4)
                                  call push(ptr4)
                               endif
                            endif
                         endif
                      enddo
                   else
                      ! OLDSTACK is a newly created stack frame - push it
                      call mark(ptr3)
                      call push(ptr3)
                   endif
                endif
             endif
             ! Ignore first four elements of stack
             n=ptr2%data%ptr(ptr2%offset)%offset-pm_stack_locals
             ptr2%offset=ptr2%offset+pm_stack_locals
          endif
       endif
       if(n>max_push) then
          ptr3%data=>ptr2%data
          ptr3%offset=-(ptr2%offset+max_push)
          call push(ptr3)
          n=max_push
       endif
       do i=ptr2%offset,ptr2%offset+n
          ptr3=ptr2%data%ptr(i)
          if(associated(ptr3%data%context,context)) then
             if(pm_debug_level>0) &
                  call pm_verify_ptr(ptr3,'in mark_from')
             if(ptr3%data%vkind>=pm_pointer) then
                if(.not.marked(ptr3)) then
                   call mark(ptr3)
                   call push(ptr3)
                endif
             else
                call mark(ptr3)
             endif
          endif
       enddo
    enddo

  contains

    ! Push object onto mark stack
    subroutine push(ptr)
      type(pm_ptr):: ptr
      context%top=context%top+1
      if(context%top>stack_size) then
         context%overflow=.true.
         context%top=context%top-1
      else
         context%stack(context%top)=ptr
      endif
    end subroutine push
    
    ! Pop object from mark stack
    function pop() result(ptr)
      type(pm_ptr):: ptr
      ptr=context%stack(context%top)
      context%top=context%top-1
    end function pop
    
  end subroutine mark_from

  ! Mark all threads on a list
  subroutine mark_threads_from(context,list)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: list
    type(pm_ptr):: thread
    thread=list
    do while(thread%data%vkind>pm_null)
       call mark(thread%data%ptr(thread%offset+pm_thread_stack))
       call mark(thread%data%ptr(thread%offset+pm_thread_inputs))
       call mark(thread%data%ptr(thread%offset+pm_thread_group))
       thread=thread%data%ptr(thread%offset+pm_thread_next)
    enddo
  end subroutine mark_threads_from


  ! Clear bit maps for blocks on given list
  subroutine clear_bitmaps(list)
    type(pm_block),pointer:: list,blk
    blk=>list
    do 
       blk%marks=0
       blk%tick=0
       blk=>blk%next
       if(associated(blk,list)) exit
    enddo
  end subroutine clear_bitmaps

  ! Clear marks on list of large object blocks
  subroutine clear_large_obj_marks(list)
    type(pm_block),pointer:: list
    type(pm_block),pointer:: blk
    blk=>list
    do while(associated(blk))
       blk%marks(1)=0
       blk=>blk%next
    enddo
  end subroutine clear_large_obj_marks

  ! Migrate live large objects to new list
  ! also free dead large objects
  subroutine migrate_large_blocks(context,list,list2)
    type(pm_context),pointer:: context
    type(pm_block),pointer:: list,list2,blk,next
    blk=>list
    do while(associated(blk))
       next=>blk%next
       if(blk%marks(1)==0) then
          call free_block(context,blk)
       else
          blk%next=>list2
          list2=>blk
       endif
       blk=>next
    enddo
  end subroutine migrate_large_blocks

  subroutine finalize(context)
    type(pm_context),pointer:: context
    integer:: i
    do i=1,pm_large_obj_size
       call finalize_list(context%obj_list(i,pm_ext)%data)
    enddo
    call finalize_list(context%heap_large)
  contains
    subroutine finalize_list(list)
      type(pm_block),pointer:: list
      type(pm_block),pointer:: blk
      integer:: k,bit,i
      if(.not.associated(list)) return
      blk=>list
      do
         if(blk%vkind==pm_ext) then
            do k=1,pm_mark_size
               if(blk%marks(k)/=not(0)) then
                  do bit=0,word_bits-1
                     if(.not.btest(blk%marks(k),bit)) then
                        i=k+bit*pm_mark_size
                        if(blk%ln(i)/=0) then
                           call pm_finalize(context,blk%ln(i:i+blk%esize))
                           blk%ln(i)=0
                        endif
                     endif
                  enddo
               endif
            enddo
         endif
         blk=>blk%next
         if(.not.associated(blk).or.associated(blk,list)) exit
      enddo
    end subroutine finalize_list
  end subroutine finalize
  

  ! Delete any blocks on a list which are empty
  subroutine free_empty_blocks(context,list)
    type(pm_context),pointer:: context
    type(pm_block),pointer:: list
    type(pm_block),pointer:: blk,next
    if(.not.associated(list)) then
       return
    elseif(associated(list%next,list)) then
       if(all(list%marks==0)) then
          call free_block(context,list)
          list=>null()
          return
       endif
    endif
    blk=>list
    do 
       next=>blk%next
       if(all(next%marks==0)) then
          blk%next=>next%next
          blk=>next%next
          if(associated(list,next)) list=>blk
          call free_block(context,next)
       else
          blk=>next
       endif
       if(associated(blk,list)) exit
    enddo
  end subroutine free_empty_blocks
 
  ! Copy object information
  subroutine copy_obj(ptr,start,ptr2,start2,esize)
    type(pm_ptr)::ptr,ptr2
    integer(pm_ln),intent(in):: start,start2,esize
    if(pm_debug_level>0) then
       call pm_verify_ptr(ptr,'copy1')
       call pm_verify_ptr(ptr2,'copy2')
    endif
    ! Copy over data between objects
    select case(ptr%data%vkind)  
    case(pm_int)
       ptr2%data%i(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%i(ptr%offset+start:ptr%offset+esize)
    case(pm_long)
       ptr2%data%ln(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%ln(ptr%offset+start:ptr%offset+esize)
    case(pm_int8)
       ptr2%data%i8(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%i8(ptr%offset+start:ptr%offset+esize)
    case(pm_int16)
       ptr2%data%i16(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%i16(ptr%offset+start:ptr%offset+esize)
    case(pm_int32)
       ptr2%data%i32(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%i32(ptr%offset+start:ptr%offset+esize)
    case(pm_int64)
       ptr2%data%i64(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%i64(ptr%offset+start:ptr%offset+esize)
    case(pm_int128)
       ptr2%data%i128(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%i128(ptr%offset+start:ptr%offset+esize)
    case(pm_single)
       ptr2%data%r(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%r(ptr%offset+start:ptr%offset+esize)
    case(pm_double)
       ptr2%data%d(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%d(ptr%offset+start:ptr%offset+esize)
    case(pm_real32)
       ptr2%data%r32(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%r32(ptr%offset+start:ptr%offset+esize)
    case(pm_real64)
       ptr2%data%r64(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%r64(ptr%offset+start:ptr%offset+esize)
    case(pm_real128)
       ptr2%data%r128(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%r128(ptr%offset+start:ptr%offset+esize)
    case(pm_single_complex)
       ptr2%data%c(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%c(ptr%offset+start:ptr%offset+esize)
    case(pm_double_complex)
       ptr2%data%dc(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%dc(ptr%offset+start:ptr%offset+esize)
    case(pm_complex64)
       ptr2%data%c64(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%c64(ptr%offset+start:ptr%offset+esize)
    case(pm_complex128)
       ptr2%data%c128(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%c128(ptr%offset+start:ptr%offset+esize)
    case(pm_complex256)
       ptr2%data%c256(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%c256(ptr%offset+start:ptr%offset+esize)
    case(pm_logical)
       ptr2%data%l(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%l(ptr%offset+start:ptr%offset+esize)
    case(pm_packed_logical)
       ptr2%data%pl(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%pl(ptr%offset+start:ptr%offset+esize)
    case(pm_string)
       ptr2%data%s(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%s(ptr%offset+start:ptr%offset+esize)
    case(pm_pointer:pm_usr)
       ptr2%data%ptr(ptr2%offset+start2:ptr2%offset+esize)= &
            ptr%data%ptr(ptr%offset+start:ptr%offset+esize)
    end select
  end subroutine copy_obj

  subroutine nullify_obj(ptr,loc1,loc2)
    type(pm_ptr):: ptr
    integer(pm_ln):: loc1,loc2
    call pm_verify_ptr(ptr,'nullify')
    select case(ptr%data%vkind)  
    case(pm_int)
       ptr%data%i(ptr%offset+loc1:ptr%offset+loc2)= 0
    case(pm_long)
       ptr%data%ln(ptr%offset+loc1:ptr%offset+loc2)= 0 
    case(pm_int8)
       ptr%data%i8(ptr%offset+loc1:ptr%offset+loc2)= 0
    case(pm_int16)
       ptr%data%i16(ptr%offset+loc1:ptr%offset+loc2)= 0
    case(pm_int32)
       ptr%data%i32(ptr%offset+loc1:ptr%offset+loc2)= 0
    case(pm_int64)
       ptr%data%i64(ptr%offset+loc1:ptr%offset+loc2)= 0
    case(pm_int128)
       ptr%data%i128(ptr%offset+loc1:ptr%offset+loc2)= 0
    case(pm_single)
       ptr%data%r(ptr%offset+loc1:ptr%offset+loc2)= 0.0
    case(pm_double)
       ptr%data%d(ptr%offset+loc1:ptr%offset+loc2)= 0.0
    case(pm_real32)
       ptr%data%r32(ptr%offset+loc1:ptr%offset+loc2)= 0.0
    case(pm_real64)
       ptr%data%r64(ptr%offset+loc1:ptr%offset+loc2)= 0.0
    case(pm_real128)
       ptr%data%r128(ptr%offset+loc1:ptr%offset+loc2)= 0.0
    case(pm_single_complex)
       ptr%data%c(ptr%offset+loc1:ptr%offset+loc2)= &
            cmplx(0.0,0.0)
    case(pm_double_complex)
       ptr%data%dc(ptr%offset+loc1:ptr%offset+loc2) = &
            cmplx(0.0,0.0,pm_d)
    case(pm_complex64)
       ptr%data%c64(ptr%offset+loc1:ptr%offset+loc2)= &
            cmplx(0.0,0.0,pm_r32)
    case(pm_complex128)
       ptr%data%c128(ptr%offset+loc1:ptr%offset+loc2)= &
            cmplx(0.0,0.0,pm_r64)
    case(pm_complex256)
       ptr%data%c256(ptr%offset+loc1:ptr%offset+loc2)= &
            cmplx(0.0,0.0,pm_r128)
    case(pm_logical)
       ptr%data%l(ptr%offset+loc1:ptr%offset+loc2)= .false.
    case(pm_packed_logical)
       ptr%data%pl(ptr%offset+loc1:ptr%offset+loc2)= .false.
    case(pm_string)
       ptr%data%s(ptr%offset+loc1:ptr%offset+loc2)= " "
    case(pm_pointer:pm_usr)
       ptr%data%ptr(ptr%offset+loc1:ptr%offset+loc2)= pm_null_obj
    end select
  end subroutine nullify_obj

  subroutine pm_finalize(context,ext)
    type(pm_context),pointer:: context
    integer(pm_ln),dimension(:)::ext
    select case(ext(1))
    case(1)
       call pm_delete_numroot(context%heap,ext(2))
    end select
  end subroutine pm_finalize

  subroutine pm_panic(emess)
    character(len=*):: emess
    write(*,*) 'Panic: '//trim(emess)
    write(*,*) pm_null_obj%data%ptr(1)%offset
  end subroutine pm_panic

end module pm_memory

