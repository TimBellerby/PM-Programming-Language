
module pm_array
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_types
  use pm_sysdefs
  implicit none

  ! Array type offsets
  integer(pm_p),parameter:: pm_array_typeof=1_pm_p
  integer(pm_p),parameter:: pm_array_vect=2_pm_p
  integer(pm_p),parameter:: pm_array_dom=3_pm_p
  integer(pm_p),parameter:: pm_array_offset=4_pm_p
  integer(pm_p),parameter:: pm_array_length=5_pm_p
  integer(pm_p),parameter:: pm_array_size=6_pm_p

  ! Error codes
  integer,parameter:: vector_type_error=-1
  integer,parameter:: vector_size_error=-2
  integer,parameter:: vector_slice_error=-3
  integer,parameter:: vector_index_error=-4

  integer,parameter:: fmt_i_width=10
  integer,parameter:: fmt_ln_width=10
  integer,parameter:: fmt_l_width=6

contains

  ! Zero any unused (according to ve) elements of vector of long ints
  function vector_zero_unused(context,v,ve) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ve
    type(pm_ptr):: ptr
    integer(pm_ln):: i,j,siz
    siz=pm_fast_esize(v)
    ptr=pm_new(context,pm_long,siz+1_pm_ln)
    if(pm_fast_isnull(ve)) then
       ptr%data%ln(ptr%offset:ptr%offset+siz)=v%data%ln(v%offset:v%offset+siz)
    elseif(pm_fast_vkind(ve)==pm_logical) then
       do i=0,siz
          if(ve%data%l(ve%offset+i)) then
             ptr%data%ln(ptr%offset+i)=v%data%ln(v%offset+i)
          else
             ptr%data%ln(ptr%offset+i)=0
          endif
       enddo
    else
       ptr%data%ln(ptr%offset:ptr%offset+siz)=0
       do j=0,pm_fast_esize(ve)
          i=ve%data%ln(ve%offset+j)
          ptr%data%ln(ptr%offset+i)=v%data%ln(v%offset+i)
       enddo
    end if
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'
  end function vector_zero_unused

  ! Make an array, or vector of arrays
  ! Defined by four vectors:
  !  vec - vector of elements (for all arrays in array vector)
  !  dom - vector of array domain values 
  !  len - vector of array lengths (num's of elements)
  !  off - vector giving offset of first element of each array in vec
  function make_array(context,typno,vec,dom,len,off) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: typno
    type(pm_ptr),intent(in):: vec,dom,len,off
    type(pm_ptr):: ptr
    ptr=pm_fast_newnc(context,pm_usr,pm_array_size)
    ptr%data%ptr(ptr%offset)=pm_fast_tinyint(context,pm_array_type)
    ptr%data%ptr(ptr%offset+pm_array_typeof)=&
         pm_fast_tinyint(context,int(typno,pm_p))
    ptr%data%ptr(ptr%offset+pm_array_vect)=vec
    ptr%data%ptr(ptr%offset+pm_array_dom)=dom
    ptr%data%ptr(ptr%offset+pm_array_length)=len
    ptr%data%ptr(ptr%offset+pm_array_offset)=off
  contains
    include 'fnewnc.inc'
    include 'ftiny.inc'
  end function make_array

  ! Make an array with j elements having intial value val
  ! domain dom
  ! (vector inputs yield vector of arrays)
  function make_array_dim(context,typno,val,dom,j,ve) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: typno
    type(pm_ptr),intent(in):: val,dom,j,ve
    type(pm_ptr):: ptr
    type(pm_ptr),target:: len,off,vec
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'arrdim',vec,len,off)
    if(pm_fast_isnull(ve)) then
       len=j
    else
       len=vector_zero_unused(context,j,ve)
    endif
    off=array_offsets(context,len)
    vec=make_vector(context,val,len,0_pm_ln,0_pm_p,total_size(len),.false.)
    ptr=make_array(context,typno,vec,dom,len,off)
    call pm_delete_register(context,reg)
  contains
    include 'fisnull.inc'
  end function make_array_dim

  ! Build array from vector of values
  function make_array_from_vect(context,typno,vec,dom,j,ve) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: typno
    type(pm_ptr),intent(in):: vec,dom,j,ve
    type(pm_ptr):: ptr
    type(pm_ptr),target:: len,off
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'arrdim',len,off)
    if(pm_fast_isnull(ve)) then
       len=j
    else
       len=vector_zero_unused(context,j,ve)
    endif
!!$    if(pm_fast_esize(vec)+1_pm_ln/=total_size(len)) then
!!$       write(*,*) pm_fast_esize(vec)+1_pm_ln,total_size(len)
!!$       call pm_panic('make_array_from_vect')
!!$    endif
    off=array_offsets(context,len)
    ptr=make_array(context,typno,vec,dom,len,off)
    call pm_delete_register(context,reg)
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
  end function make_array_from_vect

  ! Number of elements-1 in vector of arrays
  function array_vector_esize(array) result(size)
    type(pm_ptr),intent(in):: array
    integer(pm_ln):: size
    size=pm_fast_esize(array%data%ptr(array%offset+pm_array_length))
  contains
    include 'fesize.inc'
  end function array_vector_esize

  ! Return array element value for given index
  function array_index(context,array,index,ve,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: array,index,ve
    integer,intent(out):: errno
    type(pm_ptr):: ptr
    type(pm_root),pointer:: root
    root=>pm_add_root(context,&
         index_vector(context,&
         array%data%ptr(array%offset+pm_array_length),&
         array%data%ptr(array%offset+pm_array_offset),&
         index,ve,errno))
    if(errno==0) then
       ptr=vector_get_elems(context,&
            array%data%ptr(array%offset+pm_array_vect),&
            root%ptr,errno)
    else
       ptr=pm_null_obj
    endif
    call pm_delete_root(context,root)
  end function array_index

  ! Set array element at given index to given value (e)
  subroutine array_set_index(context,array,index,e,ve,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: array,index,e,ve
    integer,intent(out):: errno
    type(pm_root),pointer:: root
    root=>pm_add_root(context,index_vector(context,&
         array%data%ptr(array%offset+pm_array_length),&
         array%data%ptr(array%offset+pm_array_offset),&
         index,ve,errno))
    if(errno==0) then
       call vector_set_elems(context,&
            array%data%ptr(array%offset+pm_array_vect),root%ptr,e,errno)
    endif
    call pm_delete_root(context,root)
  end subroutine  array_set_index

  ! Set array of array elements to elements of vector e
  subroutine array_export(context,array,idx,e,import_vec,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: array,idx,e,import_vec
    integer,intent(out):: errno
    type(pm_reg),pointer:: reg
    type(pm_ptr):: idxv
    reg=>pm_register(context,'amidx',idxv)
    idxv=index_vector_nested(context,&
         array%data%ptr(array%offset+pm_array_length),&
         array%data%ptr(array%offset+pm_array_offset),&
         idx,&
         import_vec,&
         errno)
    if(errno==0) then
       call vector_set_elems(context,&
            array%data%ptr(array%offset+pm_array_vect),idxv,&
            e,errno)
    endif
    call pm_delete_register(context,reg)
  contains
    include 'fnewnc.inc'
    include 'fisnull.inc'
  end subroutine array_export

  ! Import vector using information in import_vec
  ! import_vec(0) : size of imported vector
  ! import_vec(1) : offset into vector being imported
  ! import_vec(2) : offset into element
  ! import_vec(3) : total size of imported vector
  function import_vector(context,v,import_vec) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,import_vec
    type(pm_ptr):: ptr
    integer(pm_ln):: siz,start
    siz=import_vec%data%ln(import_vec%offset)+1_pm_ln
    start=import_vec%data%ln(import_vec%offset+1)
    ptr=make_vector(context,v,import_vec,start,4_pm_p,siz,.true.)
  end function import_vector

  ! Build a vector by replicating a scalar value
  recursive function make_vector(context,v,j,dispv,dispj,siz,is_const) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,j
    integer(pm_p),intent(in):: dispj
    integer(pm_ln),intent(in):: dispv,siz
    logical,intent(in):: is_const
    type(pm_ptr):: ptr
    integer:: tno
    type(pm_root),pointer:: root
    integer(pm_p):: i
    integer(pm_ln):: s,k,m,n,disp
    type(pm_ptr):: p,q,len,off
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type)
       root=>pm_new_as_root(context,pm_usr,int(pm_array_size,pm_ln))
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+pm_array_typeof)=v%data%ptr(v%offset+pm_array_typeof)
       call pm_ptr_assign(context,ptr,int(pm_array_dom,pm_ln),&
           make_vector(context,v%data%ptr(v%offset+pm_array_dom),j,dispv,dispj,siz,is_const))
       len=make_vector(context,v%data%ptr(v%offset+pm_array_length),j,dispv,dispj,siz,is_const)
       call pm_ptr_assign(context,ptr,int(pm_array_length,pm_ln),len)
       off=make_vector(context,v%data%ptr(v%offset+pm_array_offset),j,dispv,dispj,siz,is_const)
       call pm_ptr_assign(context,ptr,int(pm_array_offset,pm_ln),off)
       if(is_const) then
          call pm_ptr_assign(context,ptr,int(pm_array_vect,pm_ln),&
               v%data%ptr(v%offset+pm_array_vect))
       else
          call pm_ptr_assign(context,ptr,int(pm_array_vect,pm_ln),&
               array_elem_vector(context,&
               v%data%ptr(v%offset+pm_array_vect),&
               len,off,total_size(len)))
          call set_offsets(off,len)
       endif
       call pm_delete_root(context,root)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       root=>pm_new_as_root(context,pm_usr,pm_fast_esize(v)+1_pm_ln)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+1_pm_p)=v%data%ptr(v%offset+1_pm_p)
       do i=2,pm_fast_esize(v)
          call pm_ptr_assign(context,ptr,int(i,pm_ln),&
               make_vector(context,v%data%ptr(v%offset+i),j,dispv,dispj,siz,is_const))
       enddo
       call pm_delete_root(context,root)
    case(pm_poly_type)
       root=>pm_new_as_root(context,pm_usr,3_pm_ln)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       call pm_ptr_assign(context,ptr,1_pm_ln,&
            make_vector(context,v%data%ptr(v%offset+1_pm_p),j,dispv,dispj,siz,is_const))
       call pm_ptr_assign(context,ptr,2_pm_ln,&
            make_vector(context,v%data%ptr(v%offset+2_pm_p),j,dispv,dispj,siz,is_const))
       call pm_delete_root(context,root)
    case(pm_int)
       ptr=pm_new(context,pm_int,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%i(ptr%offset+n)=v%data%i(v%offset+i+dispv)
             n=n+1
          enddo
       enddo
    case(pm_long)
       ptr=pm_new(context,pm_long,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%ln(ptr%offset+n)=v%data%ln(v%offset+i+dispv)
             n=n+1
          enddo
       enddo
    case(pm_logical)
       ptr=pm_new(context,pm_logical,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%l(ptr%offset+n)=v%data%l(v%offset+i+dispv)
             n=n+1
          enddo
       enddo
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function make_vector

  recursive function copy_vector(context,v,ve) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ve
    type(pm_ptr):: ptr
    integer:: vk,typ
    integer(pm_ln):: i
    type(pm_root),pointer:: root
    if(pm_fast_isnull(v)) then
       ptr=v
       return
    endif
    vk=pm_fast_vkind(v)
    if(vk<pm_string) then
       ptr=pm_new(context,vk,pm_fast_esize(v)+1)
       call pm_copy_obj(v,0_pm_ln,ptr,0_pm_ln,pm_fast_esize(v))
    else
       ptr=pm_new(context,pm_usr,pm_fast_esize(v)+1)
       root=>pm_add_root(context,ptr)
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+1_pm_p)=ptr%data%ptr(ptr%offset+1_pm_p)
       do i=2,pm_fast_esize(v)
          call pm_ptr_assign(context,ptr,i,&
               copy_vector(context,v%data%ptr(v%offset+i),ve))
       enddo
       call pm_delete_root(context,root)
    endif
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
  end function copy_vector

  recursive subroutine vector_assign(context,lhs,rhs,ve,esize)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: lhs,rhs,ve
    integer(pm_ln),intent(in):: esize
    integer(pm_p):: i
    integer(pm_i16):: tno
    integer(pm_ln):: j
    if(pm_fast_isnull(lhs)) return
    tno=pm_fast_typeof(lhs)
    select case(tno)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do i=2,pm_fast_esize(lhs)
          call vector_assign(context,&
               lhs%data%ptr(lhs%offset+i),&
               rhs%data%ptr(rhs%offset+i),ve,esize)
       enddo
    case(pm_poly_type)
       !!!
    case(pm_int)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%i(lhs%offset:lhs%offset+esize)=&
               rhs%data%i(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%i(lhs%offset:lhs%offset+esize)=&
                  rhs%data%i(rhs%offset:rhs%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             lhs%data%i(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%i(rhs%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
    case(pm_long)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%ln(lhs%offset:lhs%offset+esize)=&
               rhs%data%ln(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%ln(lhs%offset:lhs%offset+esize)=&
                  rhs%data%ln(rhs%offset:rhs%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             lhs%data%ln(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%ln(rhs%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
    case(pm_logical)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%l(lhs%offset:lhs%offset+esize)=&
               rhs%data%l(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%l(lhs%offset:lhs%offset+esize)=&
                  rhs%data%l(rhs%offset:rhs%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             lhs%data%l(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%l(rhs%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
       
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'
  end subroutine vector_assign
  
  ! Compute a vector of indices
  ! start, start+step, .., end
  ! Each index repeated elsize times
  function vector_iota(context,&
       elsize,start,end,step,import_vec) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: import_vec,elsize,start,end,step
    type(pm_ptr):: ptr
    type(pm_ptr),target:: vec,offset
    integer(pm_ln):: i,j,k,n,tstart,nstart,ielsize,istart,iend
    integer(pm_ln):: istep,idx,rpt
    type(pm_reg),pointer:: reg
    n=import_vec%data%ln(import_vec%offset)+1_pm_ln
    vec=pm_new(context,pm_long,n)
    tstart=import_vec%data%ln(import_vec%offset+1)
    nstart=import_vec%data%ln(import_vec%offset+2)
    k=0
    do i=nstart,pm_fast_esize(import_vec)-4_pm_ln+nstart
        ielsize=elsize%data%ln(elsize%offset+i)
        istart=start%data%ln(start%offset+i)
        iend=end%data%ln(end%offset+i)
        istep=step%data%ln(step%offset+i)
        if(tstart==0) then
           rpt=ielsize
           idx=istart
        else
           idx=tstart/ielsize
           rpt=ielsize-(tstart-idx*ielsize)
           idx=mod(idx,(iend-istart)/istep)
           idx=istart+idx*istep
        endif
        !write(*,*) 'IOTA',import_vec%data%ln(import_vec%offset+i-tstart+4_pm_ln),ielsize,istart,iend,istep
        do j=1,import_vec%data%ln(import_vec%offset+i-tstart+4_pm_ln)
           !write(*,*) 'iota',idx
           vec%data%ln(vec%offset+k)=idx
           k=k+1
           rpt=rpt-1
           if(rpt==0) then
              rpt=ielsize
              idx=idx+istep
              if(idx>iend) idx=istart
           endif
        enddo
    enddo
    ptr=vec
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end function vector_iota
  
  ! Get elements from a vector (indices start at 0)
  recursive function vector_get_elems(context,v,ix,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ix
    integer,intent(out):: errno
    type(pm_ptr):: ptr
    integer:: tno
    integer(pm_ln):: h,i,j,k,nsiz
    type(pm_ptr),target:: vec,dom,len,off
    type(pm_root),pointer:: root
    type(pm_reg),pointer:: reg
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type)
       reg=>pm_register(context,'vgetel',vec,dom,len,off)
       dom=vector_get_elems(context,v%data%ptr(v%offset+pm_array_dom),ix,errno)
       len=vector_get_elems(context,v%data%ptr(v%offset+pm_array_length),ix,errno)
       off=vector_get_elems(context,v%data%ptr(v%offset+pm_array_offset),ix,errno)
       nsiz=total_size(len)
       vec=array_elem_vector(context,v%data%ptr(v%offset+pm_array_vect),&
            len,off,nsiz)
       ptr=make_array(context,int(v%data%ptr(v%offset+pm_array_typeof)%offset,pm_i16),&
            vec,dom,len,off)
       call set_offsets(off,len)
       call pm_delete_register(context,reg)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       root=>pm_new_as_root(context,pm_usr,pm_fast_esize(v)+1)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+1_pm_p)=v%data%ptr(v%offset+1_pm_p)
       do i=2,pm_fast_esize(v)
          ptr%data%ptr(ptr%offset+i)=vector_get_elems(context,&
               v%data%ptr(v%offset+i),ix,errno)
       enddo
       call pm_delete_root(context,root)
    case(pm_poly_type)
       root=>pm_new_as_root(context,pm_usr,3_pm_ln)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       call pm_ptr_assign(context,ptr,1_pm_ln,&
            vector_get_elems(context,v%data%ptr(v%offset+1_pm_p),ix,errno))
       call pm_ptr_assign(context,ptr,2_pm_ln,&
            vector_get_elems(context,v%data%ptr(v%offset+2_pm_p),ix,errno))
       call pm_delete_root(context,root)
    case(pm_int)
       ptr=pm_new(context,pm_int,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%i(ptr%offset+i)=v%data%i(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_long)
       ptr=pm_new(context,pm_long,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%ln(ptr%offset+i)= v%data%ln(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_int8)
       ptr=pm_new(context,pm_int8,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%i8(ptr%offset+i)= v%data%i8(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_int16)
       ptr=pm_new(context,pm_int16,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%i16(ptr%offset+i)= v%data%i16(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_int32)
       ptr=pm_new(context,pm_int32,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%i32(ptr%offset+i)= v%data%i32(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_int64)
       ptr=pm_new(context,pm_int64,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%i64(ptr%offset+i)= v%data%i64(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_int128)
       ptr=pm_new(context,pm_int128,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%i128(ptr%offset+i)= v%data%i128(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_single)
       ptr=pm_new(context,pm_single,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%r(ptr%offset+i)= v%data%r(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_double)
       ptr=pm_new(context,pm_double,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%d(ptr%offset+i)= v%data%d(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_real32)
       ptr=pm_new(context,pm_real32,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%r32(ptr%offset+i)= v%data%r32(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_real64)
       ptr=pm_new(context,pm_real64,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%r64(ptr%offset+i)= v%data%r64(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_real128)
       ptr=pm_new(context,pm_real128,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%r128(ptr%offset+i)= v%data%r128(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_single_complex)
       ptr=pm_new(context,pm_single_complex,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%c(ptr%offset+i)= v%data%c(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_double_complex)
       ptr=pm_new(context,pm_double_complex,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%dc(ptr%offset+i) = v%data%dc(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_complex64)
       ptr=pm_new(context,pm_complex64,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%c64(ptr%offset+i)= v%data%c64(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_complex128)
       ptr=pm_new(context,pm_complex128,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%c128(ptr%offset+i)= v%data%c128(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_complex256)
       ptr=pm_new(context,pm_complex256,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%c256(ptr%offset+i)= v%data%c256(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_logical)
       ptr=pm_new(context,pm_logical,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%l(ptr%offset+i)= v%data%l(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_packed_logical)
       ptr=pm_new(context,pm_packed_logical,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%pl(ptr%offset+i)= v%data%pl(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_string)
       ptr=pm_new(context,pm_string,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%s(ptr%offset+i)= v%data%s(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case(pm_pointer:pm_usr)
       ptr=pm_new(context,pm_pointer,pm_fast_esize(ix)+1_pm_ln)
       do i=0,pm_fast_esize(ix)
          ptr%data%ptr(ptr%offset+i)= v%data%ptr(v%offset+ix%data%ln(ix%offset+i))
       enddo
    case default
       write(*,*) 'TYPENO=',tno
       call pm_panic('vector-get-elems')
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function vector_get_elems

  ! Set elements in a vector
  recursive subroutine vector_set_elems(context,v,ix,e,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ix,e
    integer,intent(out):: errno
    integer:: tno
    integer(pm_ln):: h,i,j,siz
    integer:: k
    type(pm_ptr):: len,off,avec,len2,off2,avec2
    tno=pm_fast_typeof(v)
    if(pm_fast_typeof(e)/=tno) then
       write(*,*) trim(pm_typ_as_string(context,int(tno,pm_i16))),'<-->',trim(pm_typ_as_string(context,pm_fast_typeof(e)))
       errno=vector_type_error
       return
    endif
    select case(tno)
    case(pm_array_type)
       len=v%data%ptr(v%offset+pm_array_length)
       off=v%data%ptr(v%offset+pm_array_offset)
       avec=v%data%ptr(v%offset+pm_array_vect)
       len2=e%data%ptr(e%offset+pm_array_length)
       off2=e%data%ptr(e%offset+pm_array_offset)
       avec2=e%data%ptr(e%offset+pm_array_vect)
       do i=0,pm_fast_esize(ix)
          j=ix%data%ln(ix%offset+i)
          siz=len%data%ln(len%offset+i)
          if(len2%data%ln(len2%offset+i)/=siz) then
             errno=vector_size_error
             return
          endif
          call vector_copy_range(context,avec,&
               off%data%ln(off%offset+j),&
               avec2,off2%data%ln(off2%offset+i),siz,errno)
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do k=2,pm_fast_esize(v)
          call vector_set_elems(context,v%data%ptr(v%offset+k),ix,e%data%ptr(e%offset+k),errno)
       enddo
    case(pm_poly_type)
       call vector_set_elems(context,&
            v%data%ptr(v%offset+1_pm_p),ix,e,errno)
       call vector_set_elems(context,&
            v%data%ptr(v%offset+2_pm_p),ix,e,errno)
    case(pm_int)
       do i=0,pm_fast_esize(ix)
          v%data%i(v%offset+ix%data%ln(ix%offset+i))=e%data%i(e%offset+i)
       enddo
    case(pm_long)
       do i=0,pm_fast_esize(ix)
          v%data%ln(v%offset+ix%data%ln(ix%offset+i))= e%data%ln(e%offset+i)
       enddo
    case(pm_int8)
       do i=0,pm_fast_esize(ix)
          v%data%i8(v%offset+ix%data%ln(ix%offset+i))= e%data%i8(e%offset+i)
       enddo
    case(pm_int16)
       do i=0,pm_fast_esize(ix)
          v%data%i16(v%offset+ix%data%ln(ix%offset+i))= e%data%i16(e%offset+i)
       enddo
    case(pm_int32)
       do i=0,pm_fast_esize(ix)
          v%data%i32(v%offset+ix%data%ln(ix%offset+i))= e%data%i32(e%offset+i)
       enddo
    case(pm_int64)
       do i=0,pm_fast_esize(ix)
          v%data%i64(v%offset+ix%data%ln(ix%offset+i))= e%data%i64(e%offset+i)
       enddo
    case(pm_int128)
       do i=0,pm_fast_esize(ix)
          v%data%i128(v%offset+ix%data%ln(ix%offset+i))= e%data%i128(e%offset+i)
       enddo
    case(pm_single)
       do i=0,pm_fast_esize(ix)
          v%data%r(v%offset+ix%data%ln(ix%offset+i))= e%data%r(e%offset+i)
       enddo
    case(pm_double)
       do i=0,pm_fast_esize(ix)
          v%data%d(v%offset+ix%data%ln(ix%offset+i))= e%data%d(e%offset+i)
       enddo
    case(pm_real32)
       do i=0,pm_fast_esize(ix)
          v%data%r32(v%offset+ix%data%ln(ix%offset+i))= e%data%r32(e%offset+i)
       enddo
    case(pm_real64)
       do i=0,pm_fast_esize(ix)
          v%data%r64(v%offset+ix%data%ln(ix%offset+i))= e%data%r64(e%offset+i)
       enddo
    case(pm_real128)
       do i=0,pm_fast_esize(ix)
          v%data%r128(v%offset+ix%data%ln(ix%offset+i))= e%data%r128(e%offset+i)
       enddo
    case(pm_single_complex)
       do i=0,pm_fast_esize(ix)
          v%data%c(v%offset+ix%data%ln(ix%offset+i))= e%data%c(e%offset+i)
       enddo
    case(pm_double_complex)
       do i=0,pm_fast_esize(ix)
          v%data%dc(v%offset+ix%data%ln(ix%offset+i)) = e%data%dc(e%offset+i)
       enddo
    case(pm_complex64)
       do i=0,pm_fast_esize(ix)
          v%data%c64(v%offset+ix%data%ln(ix%offset+i))= e%data%c64(e%offset+i)
       enddo
    case(pm_complex128)
       do i=0,pm_fast_esize(ix)
          v%data%c128(v%offset+ix%data%ln(ix%offset+i))= e%data%c128(e%offset+i)
       enddo
    case(pm_complex256)
       do i=0,pm_fast_esize(ix)
          v%data%c256(v%offset+ix%data%ln(ix%offset+i))= e%data%c256(e%offset+i)
       enddo
    case(pm_logical)
       do i=0,pm_fast_esize(ix)
          v%data%l(v%offset+ix%data%ln(ix%offset+i))= e%data%l(e%offset+i)
       enddo
    case(pm_packed_logical)
       do i=0,pm_fast_esize(ix)
          v%data%pl(v%offset+ix%data%ln(ix%offset+i))= e%data%pl(e%offset+i)
       enddo
    case(pm_string)
       do i=0,pm_fast_esize(ix)
          v%data%s(v%offset+ix%data%ln(ix%offset+i))= e%data%s(e%offset+i)
       enddo
    case(pm_pointer:pm_usr)
       do i=0,pm_fast_esize(ix)
          call pm_ptr_assign(context,v,i,e%data%ptr(e%offset+i))
       enddo    
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine vector_set_elems

  ! Copy elements of vector e to elements of vector v
  recursive subroutine vector_copy_elems(context,v,ix,e,iy,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ix,e,iy
    integer,intent(out):: errno
    integer:: tno
    integer(pm_ln):: h,i,j,siz
    integer:: k
    type(pm_ptr):: len,off,avec,len2,off2,avec2
    tno=pm_fast_typeof(v)
    if(pm_fast_typeof(e)/=tno) then
       errno=vector_type_error
       return
    endif
    select case(tno)
    case(pm_array_type)
       len=v%data%ptr(v%offset+pm_array_length)
       off=v%data%ptr(v%offset+pm_array_offset)
       avec=v%data%ptr(v%offset+pm_array_vect)
       len2=e%data%ptr(e%offset+pm_array_length)
       off2=e%data%ptr(e%offset+pm_array_offset)
       avec2=e%data%ptr(e%offset+pm_array_vect)
       do i=0,pm_fast_esize(ix)
          j=ix%data%ln(ix%offset+i)
          siz=len%data%ln(len%offset+i)
          if(len2%data%ln(len2%offset+iy%data%ln(iy%offset+i))/=siz) then
             errno=vector_size_error
             return
          endif
          call vector_copy_range(context,avec,&
               off%data%ln(off%offset+j),&
               avec2,off2%data%ln(off2%offset+iy%data%ln(iy%offset+i)),siz,errno)
       enddo
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       do k=2,pm_fast_esize(v)
          call vector_copy_elems(context,v%data%ptr(v%offset+k),ix,e,iy,errno)
       enddo
    case(pm_poly_type)
       call vector_copy_elems(context,&
            v%data%ptr(v%offset+1_pm_p),ix,e,iy,errno)
       call vector_copy_elems(context,&
            v%data%ptr(v%offset+2_pm_p),ix,e,iy,errno)
    case(pm_int)
       do i=0,pm_fast_esize(ix)
          v%data%i(v%offset+ix%data%ln(ix%offset+i))=&
               e%data%i(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_long)
       do i=0,pm_fast_esize(ix)
          v%data%ln(v%offset+ix%data%ln(ix%offset+i))=&
               e%data%ln(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_int8)
       do i=0,pm_fast_esize(ix)
          v%data%i8(v%offset+ix%data%ln(ix%offset+i))=&
               e%data%i8(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_int16)
       do i=0,pm_fast_esize(ix)
          v%data%i16(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%i16(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_int32)
       do i=0,pm_fast_esize(ix)
          v%data%i32(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%i32(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_int64)
       do i=0,pm_fast_esize(ix)
          v%data%i64(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%i64(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_int128)
       do i=0,pm_fast_esize(ix)
          v%data%i128(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%i128(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_single)
       do i=0,pm_fast_esize(ix)
          v%data%r(v%offset+ix%data%ln(ix%offset+i))=  & 
               e%data%r(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_double)
       do i=0,pm_fast_esize(ix)
          v%data%d(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%d(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_real32)
       do i=0,pm_fast_esize(ix)
          v%data%r32(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%r32(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_real64)
       do i=0,pm_fast_esize(ix)
          v%data%r64(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%r64(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_real128)
       do i=0,pm_fast_esize(ix)
          v%data%r128(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%r128(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_single_complex)
       do i=0,pm_fast_esize(ix)
          v%data%c(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%c(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_double_complex)
       do i=0,pm_fast_esize(ix)
          v%data%dc(v%offset+ix%data%ln(ix%offset+i)) = &
               e%data%dc(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_complex64)
       do i=0,pm_fast_esize(ix)
          v%data%c64(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%c64(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_complex128)
       do i=0,pm_fast_esize(ix)
          v%data%c128(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%c128(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_complex256)
       do i=0,pm_fast_esize(ix)
          v%data%c256(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%c256(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_logical)
       do i=0,pm_fast_esize(ix)
          v%data%l(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%l(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_packed_logical)
       do i=0,pm_fast_esize(ix)
          v%data%pl(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%pl(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_string)
       do i=0,pm_fast_esize(ix)
          v%data%s(v%offset+ix%data%ln(ix%offset+i))= &
               e%data%s(e%offset+iy%data%ln(iy%offset+i))
       enddo
    case(pm_pointer:pm_usr)
       do i=0,pm_fast_esize(ix)
          call pm_ptr_assign(context,v,i,e%data%ptr(e%offset+iy%data%ln(iy%offset+i)))
       enddo    
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine vector_copy_elems

  ! Copy a range of elements from one vector to another
  recursive subroutine vector_copy_range(context,v,start1,e,start2,siz,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,e
    integer(pm_ln),intent(in):: start1,start2,siz
    integer:: errno
    integer:: k,i
    integer(pm_ln):: j,jj,start_1,start_2,size1,size2
    type(pm_ptr):: p
    k=pm_fast_typeof(v)
    select case(k)
    case(pm_struct_type,pm_rec_type)
       if(k/=pm_fast_typeof(e)) then
          errno=vector_type_error
          return
       elseif(v%data%ptr(v%offset+1_pm_p)%offset/=&
            e%data%ptr(e%offset+1_pm_p)%offset) then
          errno=vector_type_error
          return
       endif
       do i=2,pm_fast_esize(v)
          call vector_copy_range(context,&
               v%data%ptr(v%offset+i),start1,&
               e%data%ptr(e%offset+i),start2,siz,errno)
          if(errno/=0) return
       enddo
    case(pm_array_type)
       if(pm_fast_typeof(e)/=pm_array_type) then
          errno=vector_type_error
          return
       endif
       p=v%data%ptr(v%offset+pm_array_offset)
       if(start1<0.or.start1+siz-1>pm_fast_esize(p)) then
          errno=vector_index_error
          return
       endif
       p=e%data%ptr(e%offset+pm_array_offset)
       if(start2<0.or.start2+siz-1>pm_fast_esize(p)) then
          errno=vector_index_error
          return
       endif
       do j=start1,start1+siz-1
          jj=j-start1+start2
          p=v%data%ptr(v%offset+pm_array_offset)
          start_1=p%data%ln(p%offset+j)
          p=v%data%ptr(v%offset+pm_array_length)
          size1=p%data%ln(p%offset+j)
          p=e%data%ptr(e%offset+pm_array_offset)
          start_2=p%data%ln(p%offset+jj)
          p=e%data%ptr(e%offset+pm_array_length)
          size2=p%data%ln(p%offset+jj)
          if(size2/=size1) then
             errno=vector_size_error
             return
          endif
          call vector_copy_range(context,v%data%ptr(v%offset+pm_array_vect),start_1,&
               e%data%ptr(e%offset+pm_array_vect),start_2,size1,errno)
          if(errno/=0) then
             return
          endif
       enddo
    case(pm_pointer)
       if(start1<0.or.start1+siz-1>pm_fast_esize(v).or.&
            start2<0.or.start2+siz-1>pm_fast_esize(e)) then
          errno=vector_index_error
       else
          do j=0,siz-1
             call pm_ptr_assign(context,v,start1+j,&
                  e%data%ptr(e%offset+start2+j))
          enddo
       endif
    case default
       if(k/=pm_fast_vkind(e)) then
          errno=vector_type_error
          return
       endif
       if(start1<0.or.start1+siz-1>pm_fast_esize(v).or.&
            start2<0.or.start2+siz-1>pm_fast_esize(e)) then
          errno=vector_index_error
       else
          call pm_copy_obj(v,start1,e,start2,siz-1)
       endif
    end select
    errno=0
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'   
  end subroutine  vector_copy_range

  ! Calculate total array vector size from array lengths vector
  function total_size(v) result(siz)
    type(pm_ptr),intent(in):: v
    integer(pm_ln):: siz
    siz=sum(v%data%ln(v%offset:v%offset+pm_fast_esize(v)))
  contains
    include 'fesize.inc'
  end function total_size

  ! Check that two vectors of longs are identical
  function all_equal(len,len2) result(ok)
    type(pm_ptr),intent(in):: len,len2
    logical:: ok
    integer(pm_ln):: siz
    siz=pm_fast_esize(len)
    ok=all(len%data%ln(len%offset:len%offset+siz)==&
         len2%data%ln(len2%offset:len2%offset+siz))
  contains
    include 'fesize.inc'
  end function all_equal


  ! Make contiguous array element vector from potentially
  ! non-contiguous vector
  recursive function array_elem_vector(context,v,len,off,siz) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,len,off
    integer(pm_ln),intent(in):: siz
    type(pm_ptr):: ptr
    integer:: tno
    type(pm_ptr):: lvec,ovec
    integer(pm_ln):: h,i,j,k,nsiz
    type(pm_root),pointer:: root
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type)
       root=>pm_new_as_root(context,pm_usr,int(pm_array_size,pm_ln))
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+pm_array_typeof)=&
            v%data%ptr(v%offset+pm_array_typeof)
       call pm_ptr_assign(context,ptr,int(pm_array_dom,pm_ln),&
            array_elem_vector(context,v%data%ptr(v%offset+pm_array_dom),len,off,siz))
       lvec=array_elem_vector(context,v%data%ptr(v%offset+pm_array_length),len,off,siz)
       call pm_ptr_assign(context,ptr,int(pm_array_length,pm_ln),lvec)
       ovec=array_elem_vector(context,v%data%ptr(v%offset+pm_array_offset),len,off,siz)
       call pm_ptr_assign(context,ptr,int(pm_array_offset,pm_ln),ovec)
       nsiz=total_size(lvec)
       call pm_ptr_assign(context,ptr,int(pm_array_offset,pm_ln),&
            array_elem_vector(context,v%data%ptr(v%offset+pm_array_offset),&
            lvec,ovec,nsiz))
       call set_offsets(ovec,lvec)
       call pm_delete_root(context,root)
    case(pm_struct_type,pm_rec_type,pm_polyref_type)
       root=>pm_new_as_root(context,pm_usr,pm_fast_esize(v)+1_pm_ln)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+1_pm_p)=v%data%ptr(v%offset+1_pm_p)
       do k=2,pm_fast_esize(v)
          call pm_ptr_assign(context,ptr,k,&
               array_elem_vector(context,v%data%ptr(v%offset+k),len,off,siz))
       enddo
       call pm_delete_root(context,root)
    case(pm_poly_type)
       root=>pm_new_as_root(context,pm_usr,3_pm_ln)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       call pm_ptr_assign(context,ptr,1_pm_ln,&
            array_elem_vector(context,v%data%ptr(v%offset+1_pm_p),len,off,siz))
       call pm_ptr_assign(context,ptr,2_pm_ln,&
            array_elem_vector(context,v%data%ptr(v%offset+2_pm_p),len,off,siz))
       call pm_delete_root(context,root)
    case(pm_int)
       ptr=pm_new(context,pm_int,siz)
       k=0
       do i=0,pm_fast_esize(len)
          h=off%data%ln(off%offset+i)
          do j=h,h+len%data%ln(len%offset+i)-1
             ptr%data%i(ptr%offset+k)=v%data%i(v%offset+j)
             k=k+1
          enddo
       enddo
    case(pm_long)
       ptr=pm_new(context,pm_long,siz)
       k=0
       do i=0,pm_fast_esize(len)
          h=off%data%ln(off%offset+i)
          do j=h,h+len%data%ln(len%offset+i)-1
             ptr%data%ln(ptr%offset+k)=v%data%ln(v%offset+j)
             k=k+1
          enddo
       enddo
 
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function array_elem_vector

  ! Calculate array offsets vector for contiguous elements
  ! from array length vector
  function array_offsets(context,len) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: len
    type(pm_ptr):: ptr
    ptr=pm_new(context,pm_long,pm_fast_esize(len)+1_pm_ln)
    call set_offsets(ptr,len)
  contains
    include 'fesize.inc'
  end function array_offsets

  ! Calculate array offsets vector for contiguous elements
  ! from array length vector (set pre-allocated vector)
  subroutine set_offsets(off,len)
    type(pm_ptr):: len,off
    integer(pm_ln):: i,s
    s=0
    do i=0,pm_fast_esize(off)
       off%data%ln(off%offset+i)=s
       s=s+len%data%ln(len%offset+i)
    enddo
  contains
    include 'fesize.inc'
  end subroutine set_offsets

  ! For vector of arrays (defined by len(gth) and off(set) vectors)
  ! and vector of indices, compute vector of locations in array
  ! element vector
  function index_vector(context,len,off,idx,ve,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: len,off,idx,ve
    integer,intent(out):: errno
    type(pm_ptr):: ptr
    integer(pm_ln):: siz,i,j
    logical:: iserr
    siz=pm_fast_esize(idx)
    ptr=vector_zero_unused(context,idx,ve)
    iserr=.false.
    do i=0,siz
       j=ptr%data%ln(ptr%offset+i)
       iserr=iserr.or.j<0.or.j>len%data%ln(len%offset+i)
       if(iserr) write(*,*) 'INDEX ERROR:',j,len%data%ln(len%offset+i)
       ptr%data%ln(ptr%offset+i)=j+off%data%ln(off%offset+i)
    enddo
    if(iserr) then
       errno=vector_index_error
       return
    endif
  contains
    include 'fesize.inc'
  end function index_vector

  ! Compute indices for all elements in an array
  function index_vector_all(context,len,off) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: len,off
    type(pm_ptr):: ptr
    integer(pm_ln):: siz,i,j,k
    siz=total_size(len)
    ptr=pm_new(context,pm_long,siz)
    j=0
    do i=0,pm_fast_esize(len)
       do k=0,len%data%ln(len%offset+i)-1
          ptr%data%ln(ptr%offset+j)=off%data%ln(off%offset+i)+k
          j=j+1
       enddo
    enddo
    if(pm_debug_level>0) then
       if(j/=siz) call pm_panic('index_vector_all')
    endif
  contains
    include 'fesize.inc'
  end function index_vector_all

  ! For vector of arrays defined by (len,off) and vector of arrays of indices
  ! defined by (len2[disp...],idx), calculate vector of array element locations 
  function index_vector_nested(context,len,off,idx,import_vec,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: len,off,idx,import_vec
    integer,intent(out):: errno
    type(pm_ptr):: ptr
    integer(pm_ln):: siz2,i,j,k,m
    integer(pm_ln):: length,offset,length2
    logical:: iserr
    integer:: disp2
    siz2=pm_fast_esize(import_vec)-4_pm_ln
    disp2=import_vec%data%ln(import_vec%offset+1)
    ptr=pm_new(context,pm_long,pm_fast_esize(idx)+1_pm_ln)
    m=0
    iserr=.false.
    do i=0,siz2
      length2=import_vec%data%ln(import_vec%offset+i+4_pm_ln)
      if(length2>0) then
         length=len%data%ln(len%offset+i+disp2)
         offset=off%data%ln(off%offset+i+disp2)
         do k=1,length2
             j=idx%data%ln(idx%offset+m)
             iserr=iserr.or.j<0.or.j>=length
             ptr%data%ln(ptr%offset+m)=j+offset
             m=m+1
          enddo
       endif
    enddo
    if(iserr) errno=vector_index_error
    if(pm_debug_level>0) then
       if(m/=pm_fast_esize(idx)+1_pm_ln) then
          write(*,*) m,pm_fast_esize(idx)
          call pm_panic('index_vector_nested')
       endif
    endif 
  contains
    include 'fesize.inc'
  end function index_vector_nested

  function vector_make_string(context,ve,v,buf_size,fmt) result(str)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ve,v
    integer,intent(in):: buf_size
    type(pm_ptr):: str
    integer(pm_ln):: vsize,esize,i,j,jj,k
    interface 
       subroutine fmt(xv,xk,xs)
         use pm_kinds
         use pm_memory
         type(pm_ptr),intent(in):: xv
         integer(pm_ln),intent(in):: xk
         character(len=*),intent(out):: xs
       end subroutine fmt
    end interface
    character(len=100):: mess
    type(pm_ptr),target:: vec,len,off
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'make_string',vec,len,off)
    vsize=pm_fast_esize(ve)
    esize=pm_fast_esize(v)
    len=pm_new(context,pm_long,esize+1)
    len%data%ln(len%offset:len%offset+esize)=0
    off=pm_new(context,pm_long,esize+1)
    off%data%ln(off%offset:off%offset+esize)=0
    vec=pm_new(context,pm_string,(vsize+1)*buf_size)
    j=0
    do i=0,vsize
       k=ve%data%ln(ve%offset+i)
       call fmt(v,k,mess)
       do jj=1,buf_size
          vec%data%s(vec%offset+j+jj-1)=mess(jj:jj)
       enddo
       len%data%ln(len%offset+k)=len_trim(mess)
       off%data%ln(off%offset+k)=j
       j=j+buf_size
    enddo
    str=make_array(context,int(pm_string_type,pm_i16),vec,len,len,off)
    call pm_delete_register(context,reg)
  contains
    include 'fesize.inc'
  end function vector_make_string

  function vector_concat_string(context,ve,v1,v2) result(str)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ve,v1,v2
    type(pm_ptr):: str
    type(pm_ptr),target:: vec,len,off
    type(pm_ptr):: vec1,vec2,len1,len2,off1,off2
    integer(pm_ln):: i,j,k,start,size,vsize,esize
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'concat',vec,len,off)
    vsize=pm_fast_esize(ve)
    len1=v1%data%ptr(v1%offset+pm_array_length)
    len2=v2%data%ptr(v2%offset+pm_array_length)
    esize=pm_fast_esize(len1)
    len=pm_new(context,pm_long,esize+1)
    len%data%ln(len%offset:len%offset+esize)=0
    k=0
    do i=0,vsize
       j=ve%data%ln(ve%offset+i)
       size=&
            len1%data%ln(len1%offset+j)+&
            len2%data%ln(len2%offset+j)
       len%data%ln(len%offset+j)=size
       k=k+size
    enddo
    vec=pm_new(context,pm_string,k)
    off=pm_new(context,pm_long,esize+1)
    off%data%ln(off%offset:off%offset+esize)=0
    off1=v1%data%ptr(v1%offset+pm_array_offset)
    off2=v2%data%ptr(v2%offset+pm_array_offset)
    vec1=v1%data%ptr(v1%offset+pm_array_vect)
    vec2=v2%data%ptr(v2%offset+pm_array_vect)
    k=0
    do i=0,vsize
       j=ve%data%ln(ve%offset+i)
       off%data%ln(off%offset+j)=k
       size=len1%data%ln(len1%offset+j)
       start=off1%data%ln(off1%offset+j)
       vec%data%s(vec%offset+k:vec%offset+k+size-1)=&
            vec1%data%s(vec1%offset+start:vec1%offset+start+size-1)
       k=k+size
       size=len2%data%ln(len2%offset+j)
       start=off2%data%ln(off2%offset+j)
       vec%data%s(vec%offset+k:vec%offset+k+size-1)=&
            vec2%data%s(vec2%offset+start:vec2%offset+start+size-1)
       k=k+size
    enddo
    str=make_array(context,int(pm_string_type,pm_i16),vec,len,len,off)
    call pm_delete_register(context,reg)
  contains
    include 'fesize.inc'
  end function vector_concat_string

  subroutine vector_print_string(context,v,ve)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ve
    integer(pm_ln):: i,j,start,size
    type(pm_ptr):: vec,len,off
    vec=v%data%ptr(v%offset+pm_array_vect)
    len=v%data%ptr(v%offset+pm_array_length)
    off=v%data%ptr(v%offset+pm_array_offset)
    do i=0,pm_fast_esize(ve)
       j=ve%data%ln(ve%offset+i)
       start=off%data%ln(off%offset+j)
       size=len%data%ln(len%offset+j)
       write(*,*) vec%data%s(vec%offset+start:vec%offset+start+size-1)
    enddo
  contains
      include 'fesize.inc'
  end subroutine vector_print_string

  subroutine vector_get_string(context,v,ve,i,str)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ve
    integer(pm_ln),intent(in):: i
    character(len=*)::str
    integer(pm_ln):: j,start,size
    integer:: k,m
    type(pm_ptr):: vec,leng,off
    vec=v%data%ptr(v%offset+pm_array_vect)
    leng=v%data%ptr(v%offset+pm_array_length)
    off=v%data%ptr(v%offset+pm_array_offset)
    j=ve%data%ln(ve%offset+i)
    start=off%data%ln(off%offset+j)
    size=leng%data%ln(leng%offset+j)
    str=' '
    do k=vec%offset+start,vec%offset+start+size-1
       m=k-vec%offset-start+1
       if(m>len(str)) exit
       str(m:m)=vec%data%s(k)
    enddo
  contains
    include 'fesize.inc'
  end subroutine vector_get_string
  
  function make_string_vector(context,val,esize) result(str)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: val
    integer(pm_ln),intent(in):: esize
    type(pm_ptr):: str
    type(pm_ptr):: len,off
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'make_str',len,off)
    len=pm_new(context,pm_long,esize+1)
    len%data%ln(len%offset:len%offset+esize)=pm_fast_esize(val)+1
    off=pm_new(context,pm_long,esize+1)
    off%data%ln(off%offset:off%offset+esize)=0
    str=make_array(context,int(pm_string_type,pm_i16),val,len,len,off)
    call pm_delete_register(context,reg)
  contains
    include 'fnewnc.inc'
    include 'fesize.inc'
  end function make_string_vector

  subroutine fmt_i(v,n,str)
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: n
    character(len=*),intent(out):: str
    character(len=10):: mess
    mess=' '
    write(mess,'(i10)') v%data%i(v%offset+n)
    str=adjustl(mess)
  end subroutine fmt_i

  subroutine fmt_ln(v,n,str)
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: n
    character(len=*),intent(out):: str
    character(len=10):: mess
    mess=' '
    write(mess,'(i10)') v%data%ln(v%offset+n)
    str=adjustl(mess)
  end subroutine fmt_ln

  subroutine fmt_l(v,n,str)
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: n
    character(len=*),intent(out):: str
    if(v%data%l(v%offset+n)) then
       str='true  '
    else
       str='false '
    endif
  end subroutine fmt_l

  ! Create a polymorphic vector from polymorphic call result
  ! vals contains list of component vectors
  ! subs contains list of which vector to use for each location
  ! Vector elements are drawn in order from indicated vectors 
  function make_poly_vec(context,vals,subs) result(polyvec)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: vals,subs
    type(pm_ptr):: polyvec
    integer(pm_ln):: i,j,k,esize,vsize
    type(pm_ptr):: p,q,pv,dv,poly,n
    type(pm_root),pointer:: root,root2
    esize=pm_fast_esize(subs)
    vsize=pm_fast_esize(vals)
    poly=pm_fast_newusr(context,int(pm_poly_type,pm_p),3_pm_p)
    root=>pm_add_root(context,poly)
    pv=pm_new(context,pm_pointer,esize+1_pm_p)
    poly%data%ptr(poly%offset+1_pm_p)=pv
    dv=pm_new(context,pm_long,esize+1_pm_p)
    poly%data%ptr(poly%offset+2_pm_p)=dv
    root2=>pm_new_as_root(context,pm_long,pm_fast_esize(vals)+1_pm_ln)
    n=root2%ptr
    n%data%ln(n%offset:n%offset+vsize)=0_pm_ln
    do i=0,esize
       j=subs%data%ln(subs%offset+i)
       p=vals%data%ptr(vals%offset+j)
       k=n%data%ln(n%offset+j)
       n%data%ln(n%offset+j)=k+1_pm_ln
       if(pm_fast_typeof(p)==pm_poly_type) then
          q=p%data%ptr(p%offset+2_pm_p)
          k=q%data%ln(q%offset+k)
          q=p%data%ptr(p%offset+1_pm_p)
          p=q%data%ptr(q%offset+k)
       endif
       pv%data%ptr(pv%offset+i)=p
       dv%data%ln(dv%offset)=k
    enddo
    call pm_delete_root(context,root)
    call pm_delete_root(context,root2)
  contains
    include 'fesize.inc'
    include 'ftypeof.inc'
    include 'fnewusr.inc'
  end function make_poly_vec

  ! Separate nvec vectors by index numbers (0..vsize) in subs
  subroutine separate_vecs(context,vecs,nvecs,subs,vsize)
    type(pm_context),pointer:: context
    integer,intent(in):: nvecs
    type(pm_ptr),intent(inout),dimension(nvecs):: vecs
    type(pm_ptr),intent(in)::subs
    integer(pm_ln),intent(in):: vsize
    integer(pm_ln):: i,j,k,esize
    integer:: iv
    integer(pm_i16),dimension(1):: key
    type(pm_ptr),target:: pv,dv,vals,n
    type(pm_ptr):: avec,dvec
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'separate poly',&
         pv,dv,vals,n)
    ! Count number of elts in each category
    n=pm_new(context,pm_long,vsize+1_pm_ln)
    n%data%ln(n%offset:n%offset+vsize-1_pm_ln)=0
    do i=0,esize
       j=subs%data%ln(subs%offset+i)
       n%data%ln(n%offset+j)=n%data%ln(n%offset+j)+1
    enddo
    ! Work out starting point for each category
    ! in re-arranged vector
    j=0
    do i=0,vsize-1
       k=n%data%ln(n%offset+i)
       n%data%ln(n%offset+i)=j
       j=j+k
    enddo
    n%data%ln(n%offset+vsize)=esize+1
    ! Re-arrange the vectors
    pv=pm_new(context,pm_pointer,esize)
    dv=pm_new(context,pm_pointer,esize)
    do iv=1,nvecs
       if(pm_fast_vkind(vecs(i))==pm_stack) cycle
       if(pm_fast_typeof(vecs(i))==pm_poly_type) then
          avec=vecs(iv)%data%ptr(vecs(iv)%offset+1_pm_p)
          dvec=vecs(iv)%data%ptr(vecs(iv)%offset+2_pm_p)
          do i=0,esize
             j=subs%data%ln(subs%offset+i)
             k=n%data%ln(n%offset+j)
             pv%data%ptr(pv%offset+k)=&
                  avec%data%ptr(avec%offset+i)
             dv%data%ln(dv%offset+k)=&
                  dvec%data%ln(dvec%offset+i)
             n%data%ln(n%offset+j)=k+1
          enddo
       else
          pv%data%ptr(pv%offset:pv%offset+esize)=vecs(iv)
          do i=0,esize
             j=subs%data%ln(subs%offset+i)
             k=n%data%ln(n%offset+j)
             dv%data%ln(dv%offset+k)=i
             n%data%ln(n%offset+j)=k+1
          enddo
       endif
       vals=pm_new(context,pm_pointer,vsize+1)
       vals%data%ptr(vals%offset)=n
       do i=1,vsize
          call pm_ptr_assign(context,vals,i,&
               ptr_vec_get_type(context,pv,dv,&
               n%data%ln(n%offset+i),&
               n%data%ln(n%offset+i+1)-1_pm_ln))
       enddo
       vecs(iv)=vals
    enddo
    call pm_delete_register(context,reg)
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine separate_vecs

  ! Separate poly vector into list of non-poly vectors
  ! and a vector of subscripts into that list
  subroutine separate_poly_vec(context,poly,xsubs,vects)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: poly
    type(pm_ptr),intent(out):: xsubs,vects
    integer(pm_ln):: i,j,k,vsize,esize
    integer(pm_i16),dimension(1):: key
    type(pm_ptr),target:: subs,set,pv,dv,vals,n
    type(pm_ptr):: avec,dvec
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'separate poly',&
         set,pv,dv,subs,vals,n)
    avec=poly%data%ptr(poly%offset+1_pm_p)
    dvec=poly%data%ptr(poly%offset+2_pm_p)
    esize=pm_fast_esize(avec)
    set=pm_set_new(context,8_pm_ln)
    subs=pm_new(context,pm_long,esize+1_pm_ln)
    do i=0,esize
       key(1)=pm_fast_typeof(avec%data%ptr(avec%offset+i))
       subs%data%ln(subs%offset+i)=pm_iset_add(context,set,key,1)
    enddo
    vsize=pm_set_size(context,set)-1_pm_ln
    set=pm_null_obj
    xsubs=subs
    n=pm_new(context,pm_long,vsize+2_pm_ln)
    do i=0,esize
       j=subs%data%ln(subs%offset+i)
       n%data%ln(n%offset+j)=n%data%ln(n%offset+j)+1
    enddo
    n%data%ln(n%offset+vsize+1_pm_ln)=esize+1
    j=0
    do i=0,vsize
       k=n%data%ln(n%offset+i)
       n%data%ln(n%offset+i)=j
       j=j+k
    enddo
    pv=pm_new(context,pm_pointer,esize)
    dv=pm_new(context,pm_pointer,esize)
    do i=0,esize
       j=subs%data%ln(subs%offset+i)
       pv%data%ptr(pv%offset+k)=avec%data%ptr(avec%offset+i)
       dv%data%ln(dv%offset+k)=dvec%data%ln(dvec%offset+i)
       n%data%ln(n%offset+j)=n%data%ln(n%offset+j)+1_pm_ln
    enddo
    vals=pm_new(context,pm_pointer,vsize+1_pm_p)
    do i=0,vsize
       call pm_ptr_assign(context,vals,i,&
            ptr_vec_get_type(context,pv,dv,&
            n%data%ln(n%offset+i),n%data%ln(n%offset+i+1)-1_pm_ln))
    enddo
    vects=vals
    call pm_delete_register(context,reg)
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine separate_poly_vec

  recursive function ptr_vec_get_type(context,&
       vec,disps,start,finish) result(outvec)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: vec,disps
    integer(pm_ln):: start,finish
    type(pm_ptr):: outvec
    integer(pm_ln):: esize,j,k,n
    integer:: vkind
    type(pm_ptr):: p,pvec
    type(pm_root),pointer:: root1,root2
    p=vec%data%ptr(vec%offset)
    vkind=pm_fast_vkind(p)
    esize=finish-start
    select case(vkind)
    case(pm_int)
       outvec=pm_new(context,pm_int,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%i(outvec%offset+j)=&
               p%data%i(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_long)
       outvec=pm_new(context,pm_long,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%ln(outvec%offset+j)=&
               p%data%ln(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_int8)
       outvec=pm_new(context,pm_int8,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%i8(outvec%offset+j)=&
               p%data%i8(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_int16)
       outvec=pm_new(context,pm_int16,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%i16(outvec%offset+j)=&
               p%data%i16(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_int32)
       outvec=pm_new(context,pm_int32,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%i32(outvec%offset+j)=&
               p%data%i32(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_int64)
       outvec=pm_new(context,pm_int64,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%i64(outvec%offset+j)=&
               p%data%i64(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_int128)
       outvec=pm_new(context,pm_int128,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%i128(outvec%offset+j)=&
               p%data%i128(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_single)
       outvec=pm_new(context,pm_single,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%r(outvec%offset+j)=&
               p%data%r(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_double)
       outvec=pm_new(context,pm_double,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%d(outvec%offset+j)=&
               p%data%d(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_real32)
       outvec=pm_new(context,pm_real32,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%r32(outvec%offset+j)=&
               p%data%r32(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_real64)
       outvec=pm_new(context,pm_real64,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%r64(outvec%offset+j)=&
               p%data%r64(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_real128)
       outvec=pm_new(context,pm_real128,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%r128(outvec%offset+j)=&
               p%data%r128(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_single_complex)
       outvec=pm_new(context,pm_single_complex,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%c(outvec%offset+j)=&
               p%data%c(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_double_complex)
       outvec=pm_new(context,pm_double_complex,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%dc(outvec%offset+j)=&
               p%data%dc(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_complex64)
       outvec=pm_new(context,pm_complex64,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%c64(outvec%offset+j)=&
               p%data%c64(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_complex128)
       outvec=pm_new(context,pm_complex128,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%c128(outvec%offset+j)=&
               p%data%c128(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_complex256)
       outvec=pm_new(context,pm_complex256,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%c256(outvec%offset+j)=&
               p%data%c256(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_logical)
       outvec=pm_new(context,pm_logical,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%l(outvec%offset+j)=&
               p%data%l(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_packed_logical)
       outvec=pm_new(context,pm_packed_logical,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%pl(outvec%offset+j)=&
               p%data%pl(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_undef:pm_null,pm_pointer:pm_stack)
       outvec=pm_new(context,pm_pointer,esize)
       k=0
       do j=0,esize 
          p=vec%data%ptr(vec%offset+j+start)
          outvec%data%ptr(outvec%offset+j)=&
               p%data%ptr(p%offset+disps%data%ln(disps%offset+j))
       enddo
    case(pm_usr)
       n=pm_fast_esize(vec%data%ptr(vec%offset))
       root1=>pm_new_as_root(context,pm_pointer,n)
       root2=>pm_new_as_root(context,pm_pointer,esize)
       outvec=root1%ptr
       pvec=root2%ptr
       do j=1,n
          do k=0,esize
             p=vec%data%ptr(vec%offset+k+start)
             pvec%data%ptr(pvec%offset+k)=p%data%ptr(p%offset+j)
          enddo
          call pm_ptr_assign(context,outvec,j,&
               ptr_vec_get_type(context,pvec,disps,0_pm_ln,esize))
       enddo
       call pm_delete_root(context,root1)
       call pm_delete_root(context,root2)
    end select
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end function  ptr_vec_get_type


end module pm_array
