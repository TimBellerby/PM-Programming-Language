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

module pm_array
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_types
  implicit none

  ! Array type offsets
  integer(pm_p),parameter:: pm_array_typeof=1_pm_p
  integer(pm_p),parameter:: pm_array_vect=2_pm_p
  integer(pm_p),parameter:: pm_array_dom=3_pm_p
  integer(pm_p),parameter:: pm_array_length=4_pm_p
  integer(pm_p),parameter:: pm_array_offset=5_pm_p
  integer(pm_p),parameter:: pm_array_size=6_pm_p

  ! Elemref type offsets
  integer(pm_p),parameter:: pm_elemref_typeof=1_pm_p
  integer(pm_p),parameter:: pm_elemref_vect=2_pm_p
  integer(pm_p),parameter:: pm_elemref_idx=3_pm_p
  integer(pm_p),parameter:: pm_elemref_offset=4_pm_p
  integer(pm_p),parameter:: pm_elemref_size=5_pm_p

  ! Error codes
  integer,parameter:: vector_type_error=-1
  integer,parameter:: vector_size_error=-2
  integer,parameter:: vector_slice_error=-3
  integer,parameter:: vector_index_error=-4
  integer,parameter:: vector_shape_error=5

  ! Widths for formatted outputs
  integer,parameter:: fmt_i_width=10
  integer,parameter:: fmt_ln_width=20
  integer,parameter:: fmt_r_width=15
  integer,parameter:: fmt_d_width=25
  integer,parameter:: fmt_l_width=6

contains

  ! Zero any unused (according to ve) elements of vector of long ints
  function vector_zero_unused(context,v,ve,zero) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ve
    integer(pm_ln),intent(in),optional:: zero
    type(pm_ptr):: ptr
    integer(pm_ln):: i,j,siz,z
    z=0_pm_ln
    if(present(zero)) z=zero
    siz=pm_fast_esize(v)
    ptr=pm_new(context,pm_long,siz+1_pm_ln)
    if(pm_fast_isnull(ve)) then
       ptr%data%ln(ptr%offset:ptr%offset+siz)=&
            v%data%ln(v%offset:v%offset+siz)
    elseif(pm_fast_vkind(ve)==pm_logical) then
       do i=0,siz
          if(ve%data%l(ve%offset+i)) then
             ptr%data%ln(ptr%offset+i)=v%data%ln(v%offset+i)
          else
             ptr%data%ln(ptr%offset+i)=z
          endif
       enddo
    else
       ptr%data%ln(ptr%offset:ptr%offset+siz)=z
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
  !  vec - vector of vectors of elements (for all arrays in array vector)
  !  dom - vector of array domain values 
  !  len - vector of array lengths (num's of elements)
  !  off - vector giving offset of first element of each array in vec
  function make_array(context,akind,typno,vec,dom,len,off) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_p),intent(in):: akind
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

  ! LHS value for single array element
  function make_elem_ref(context,array,index,ve,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: array,index,ve
    integer,intent(out):: errno
    type(pm_ptr):: ptr
    type(pm_ptr)::a
    type(pm_root),pointer:: root
    type(pm_ptr):: w,vec,newvec,oldoff,off,newoff,idx,newidx
    integer(pm_ln):: esize,i,j
    root=>pm_new_as_root(context,pm_usr,int(pm_elemref_size,pm_ln))
    ptr=root%ptr
    ptr%data%ptr(ptr%offset)=pm_fast_tinyint(context,pm_elemref_type)
 
    esize=pm_fast_esize(index)
    newidx=pm_assign_new(context,ptr,int(pm_elemref_idx,pm_ln),&
         pm_long,esize+1_pm_ln,.false.)
    newoff=pm_assign_new(context,ptr,int(pm_elemref_offset,pm_ln),&
         pm_long,esize+1_pm_ln,.false.)
    if(pm_fast_typeof(array)==pm_elemref_type) then
       newvec=pm_assign_new(context,ptr,int(pm_elemref_vect,pm_ln),&
            pm_pointer,esize+1_pm_ln,.true.)
       vec=array%data%ptr(array%offset+pm_elemref_vect)
       ptr%data%ptr(ptr%offset+pm_elemref_typeof)=&
            pm_fast_tinyint(context,int(full_type(vec%data%ptr(vec%offset)),pm_p))
       idx=array%data%ptr(array%offset+pm_elemref_idx)
       oldoff=array%data%ptr(array%offset+pm_elemref_offset)
       do i=0,esize
          w=vec%data%ptr(vec%offset+i)
          w=w%data%ptr(w%offset+pm_array_vect)
          off=w%data%ptr(w%offset+pm_array_offset)
          j=idx%data%ln(idx%offset+i)+oldoff%data%ln(oldoff%offset+i)
          call pm_ptr_assign(context,newvec,i,&
               w%data%ptr(w%offset+j))
          newidx%data%ln(newidx%offset+i)=&
               index%data%ln(index%offset+i)
          newoff%data%ln(newoff%offset+i)=&
               off%data%ln(off%offset+j)
       enddo
    else
       off=array%data%ptr(array%offset+pm_array_offset)
       vec=array%data%ptr(array%offset+pm_array_vect)
       ptr%data%ptr(ptr%offset+pm_elemref_typeof)=&
            pm_fast_tinyint(context,&
            int(full_type(vec%data%ptr(vec%offset)),pm_p))
       do i=0,esize
          newidx%data%ln(newidx%offset+i)=&
               index%data%ln(index%offset+i)
          newoff%data%ln(newoff%offset+i)=&
               off%data%ln(off%offset+i)
       enddo
       call pm_ptr_assign(context,ptr,int(pm_elemref_vect,pm_ln),&
            vec)
    endif
    call pm_delete_root(context,root)
  contains
    include 'ftiny.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function make_elem_ref

  function get_elem_ref(context,p,esize,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: p
    integer(pm_ln),intent(in):: esize
    integer,intent(out):: errno
    type(pm_ptr):: ptr
    type(pm_ptr):: v,idx,off
    type(pm_root),pointer:: root
    if(pm_fast_typeof(p)==pm_elemref_type) then
       v=p%data%ptr(p%offset+pm_elemref_vect)
       idx=p%data%ptr(p%offset+pm_elemref_idx)
       off=p%data%ptr(p%offset+pm_elemref_offset)
       root=>pm_add_root(context,empty_copy_vector(context,v%data%ptr(v%offset),&
            pm_fast_esize(idx)+1_pm_p))
       call array_vect_index(context,root%ptr,v,idx,off,esize)
       ptr=root%ptr
       call pm_delete_root(context,root)
    else
       ptr=p
    endif
  contains
    include 'fesize.inc'
    include 'ftypeof.inc'
  end function get_elem_ref

  ! Return domain of an array
  function array_dom(context,v,esize) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: esize
    type(pm_ptr):: ptr
    type(pm_ptr),target:: p,q
    type(pm_ptr):: w,x,idx,off
    integer(pm_ln):: i
    type(pm_reg),pointer:: reg
    if(pm_fast_typeof(v)==pm_array_type) then
       ptr=v%data%ptr(v%offset+pm_array_dom)
    else
       reg=>pm_register(context,'adom',p,q)
       w=v%data%ptr(v%offset+pm_elemref_vect)
       idx=v%data%ptr(v%offset+pm_elemref_idx)
       off=v%data%ptr(v%offset+pm_elemref_offset)
       p=empty_copy_vector(context,w%data%ptr(w%offset),esize+1)
       q=pm_new(context,pm_pointer,esize+1)
       do i=0,esize
          x=w%data%ptr(w%offset+i)
          q%data%ptr(q%offset+i)=x%data%ptr(x%offset+pm_array_dom)
       enddo
       call array_vect_index(context,p,q,idx,off,esize)
       call pm_delete_register(context,reg)
    endif
  contains
    include 'ftypeof.inc'
  end function array_dom

  ! Make an array with j elements having intial value val
  ! domain dom
  ! (vector inputs yield vector of arrays)
  function make_array_dim(context,typno,val,dom,j,ve) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: typno
    type(pm_ptr),intent(in):: val,dom,j,ve
    type(pm_ptr):: ptr
    type(pm_ptr),target:: len,off,vec,domv
    type(pm_reg),pointer:: reg
    integer(pm_ln):: i,esize
    reg=>pm_register(context,'arrdim',vec,len,off)
    esize=pm_fast_esize(j)
    len=vector_zero_unused(context,j,ve)
    vec=pm_new(context,pm_pointer,esize+1_pm_ln)
    call pm_ptr_assign(context,vec,0_pm_ln,vector_from_scalar(context,val,0_pm_ln,&
               max(0_pm_ln,len%data%ln(len%offset)-1_pm_ln),.false.))
    do i=1,esize
       if(len%data%ln(len%offset+i)>0) then
          call pm_ptr_assign(context,vec,i,vector_from_scalar(context,val,i,&
               len%data%ln(len%offset+i)-1_pm_ln,.false.))
       endif
    enddo
    off=pm_new(context,pm_long,esize+1_pm_ln)
    off%data%ln(off%offset:off%offset+esize)=0_pm_ln
    domv=copy_vector(context,dom,ve,0_pm_ln,-1_pm_ln)
    ptr=make_array(context,pm_array_type,typno,vec,domv,len,off)
    call pm_delete_register(context,reg)
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
  end function make_array_dim

  ! Create array with same elements (by ref) but new domain
  function array_redim(context,tno,array,dom) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: tno
    type(pm_ptr),intent(in):: array,dom
    type(pm_ptr):: ptr
    ptr=make_array(context,pm_array_type,&
         tno,&
         array%data%ptr(array%offset+pm_array_vect),&
         dom,&
         array%data%ptr(array%offset+pm_array_length),&
         array%data%ptr(array%offset+pm_array_offset))
  end function array_redim

  ! Build array from vector of values
  function make_array_from_vect(context,typno,vec,dom,j,ve) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: typno
    type(pm_ptr),intent(in):: vec,dom,j,ve
    type(pm_ptr):: ptr
    type(pm_ptr),target:: len,off,vect,domv
    type(pm_reg),pointer:: reg
    integer(pm_ln):: i,esize
    reg=>pm_register(context,'arrdim',vect,len,off,domv)
    esize=pm_fast_esize(j)
    len=vector_zero_unused(context,j,ve)
    vect=pm_new(context,pm_pointer,esize+1_pm_ln)
    vect%data%ptr(vect%offset:vect%offset+esize)=vec
    off=array_offsets(context,len)
    domv=copy_vector(context,dom,ve,0_pm_ln,-1_pm_ln)
    ptr=make_array(context,pm_array_type,typno,vect,domv,len,off)
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

  ! Size-1 of a vector
  recursive function vector_esize(v) result(esize)
    type(pm_ptr),intent(in):: v
    integer(pm_ln):: esize
    integer:: tno
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       esize=vector_esize(v%data%ptr(v%offset+pm_array_length))
    case(pm_struct_type,pm_rec_type)
       esize=vector_esize(v%data%ptr(v%offset+2_pm_p))
    case default
       esize=pm_fast_esize(v)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function vector_esize

  ! Number of leaves in a vector
  recursive function vector_num_leaves(v) result(n)
    type(pm_ptr),intent(in):: v
    integer:: n
    integer:: tno,i
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       n=vector_num_leaves(v%data%ptr(v%offset+pm_array_vect))+pm_array_size-3
    case(pm_struct_type,pm_rec_type)
       n=0
       do i=2,pm_fast_esize(v)
          n=n+vector_num_leaves(v%data%ptr(v%offset+i))
       enddo
    case default
       n=1
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function vector_num_leaves
  
  ! Return array element value for given index
  function array_index(context,array,index,ve,esize,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: array,index,ve
    integer(pm_ln),intent(in):: esize
    integer,intent(out):: errno
    type(pm_ptr):: ptr
    type(pm_ptr):: vec,off,len
    type(pm_ptr),target:: idx,p
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'aindx',idx,p)
    vec=array%data%ptr(array%offset+pm_array_vect)
    off=array%data%ptr(array%offset+pm_array_offset)
    len=array%data%ptr(array%offset+pm_array_length)
          p=empty_copy_vector(context,vec%data%ptr(vec%offset),&
               esize+1_pm_ln)
    ptr=p
    idx=vector_zero_unused(context,index,ve)
    if(any(idx%data%ln(idx%offset:idx%offset+esize)<0.or.&
         idx%data%ln(idx%offset:idx%offset+esize)>&
         len%data%ln(len%offset:len%offset+esize))) then
       errno=vector_index_error
       return
    endif
    call array_vect_index(context,p,vec,idx,off,esize)
    if(p%data%vkind/=vec%data%ptr(vec%offset+esize)%data%vkind) then
       errno=vector_type_error
    endif
    call pm_delete_register(context,reg)
  end function array_index

  recursive subroutine array_vect_index(context,p,v,idx,offset,esize)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: p,v,idx,offset
    integer(pm_ln),intent(in):: esize
    type(pm_ptr):: vec,dom,len,off,vec2,dom2,len2,off2,w,vv
    integer:: tno
    integer(pm_ln):: i,ix
    integer:: j
    type(pm_root),pointer:: root
    tno=pm_fast_typeof(p)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       vec=p%data%ptr(p%offset+pm_array_vect)
       dom=p%data%ptr(p%offset+pm_array_dom)
       len=p%data%ptr(p%offset+pm_array_length)
       off=p%data%ptr(p%offset+pm_array_offset)
       root=>pm_new_as_root(context,pm_pointer,esize+1_pm_ln)
       vv=root%ptr
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             vv%data%ptr(vv%offset+i)=w%data%ptr(w%offset+pm_array_dom)
          endif
       enddo
       call array_vect_index(context,dom,vv,idx,offset,esize)
       call pm_delete_root(context,root)      
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             vec2=w%data%ptr(w%offset+pm_array_vect)
             off2=w%data%ptr(w%offset+pm_array_offset)
             len2=w%data%ptr(w%offset+pm_array_length)
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)
             call pm_ptr_assign(context,vec,i,&
                  copy_vector(context,&
                  vec2%data%ptr(vec2%offset+ix),&
                  pm_null_obj,&
                  off2%data%ln(off2%offset+ix),&
                  len2%data%ln(len2%offset+ix)&
               ))
             len%data%ln(len%offset+i)=len2%data%ln(len2%offset+ix)
             off%data%ln(off%offset+i)=0_pm_ln
          endif
       enddo
    case(pm_struct_type,pm_rec_type)
       root=>pm_add_root(context,pm_null_obj)
       do j=2,pm_fast_esize(p)
          root%ptr=pm_new(context,pm_pointer,esize+1_pm_ln)
          vv=root%ptr
          do i=0,esize
             w=v%data%ptr(v%offset+i)
             if(.not.pm_fast_isnull(w)) then
                vv%data%ptr(vv%offset+i)=w%data%ptr(w%offset+j)
             endif
          enddo
          call array_vect_index(context,p%data%ptr(p%offset+j),&
               vv,idx,offset,esize)
       enddo
       call pm_delete_root(context,root)
    case(pm_int)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)
             p%data%i(p%offset+i)=w%data%i(w%offset+ix)
          endif
       enddo
    case(pm_long)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)    
             p%data%ln(p%offset+i)=w%data%ln(w%offset+ix)
          endif
       enddo
    case(pm_single)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)
             p%data%r(p%offset+i)=w%data%r(w%offset+ix)
          endif
       enddo
    case(pm_double)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)
             p%data%d(p%offset+i)=w%data%d(w%offset+ix)
          endif
       enddo
    case(pm_logical)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)
             p%data%l(p%offset+i)=w%data%l(w%offset+ix)
          endif
       enddo
    case(pm_pointer)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)
             call pm_ptr_assign(context,p,i,&
                  w%data%ptr(w%offset+ix))
          endif
       enddo
    end select
  contains
    include 'fisnull.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine array_vect_index

  ! Set array element at given index to given value (e)
  subroutine array_set_index(context,array,index,e,ve,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: array,index,e,ve
    integer,intent(out):: errno
    type(pm_ptr):: ptr
    type(pm_ptr):: vec,off,len,ix,v,w
    type(pm_ptr),target:: idx,newvec,newoff
    type(pm_reg),pointer:: reg
    integer(pm_ln):: i,j
    integer(pm_ln):: esize
    if(pm_fast_typeof(array)==pm_array_type) then
       reg=>pm_register(context,'aindx',idx)
       vec=array%data%ptr(array%offset+pm_array_vect)
       off=array%data%ptr(array%offset+pm_array_offset)
       len=array%data%ptr(array%offset+pm_array_length)
       idx=vector_zero_unused(context,index,ve,-987654321_pm_ln)
       esize=pm_fast_esize(idx)
       if(any((idx%data%ln(idx%offset:idx%offset+esize)<0.and.&
            idx%data%ln(idx%offset:idx%offset+esize)/=-987654321_pm_ln).or.&
            idx%data%ln(idx%offset:idx%offset+esize)>&
            len%data%ln(len%offset:len%offset+esize))) then
          errno=vector_index_error
          return
       endif
       call array_vect_set_index(context,vec,idx,off,e,esize,errno)
       call pm_delete_register(context,reg)
    else
       reg=>pm_register(context,'arindx',idx,newvec)
       vec=array%data%ptr(array%offset+pm_elemref_vect)
       off=array%data%ptr(array%offset+pm_elemref_offset)
       ix=array%data%ptr(array%offset+pm_elemref_idx)
       idx=vector_zero_unused(context,index,ve,-1_pm_ln)
       esize=pm_fast_esize(idx)
       newvec=pm_new(context,pm_pointer,esize+1)
       newoff=pm_new(context,pm_long,esize+1)
       do i=0,esize
          w=vec%data%ptr(vec%offset+i)
          j=ix%data%ln(ix%offset+i)+off%data%ln(off%offset+i)
          v=w%data%ptr(w%offset+pm_array_vect)
          newvec%data%ptr(newvec%offset+i)=v%data%ptr(v%offset+j)
          v=w%data%ptr(w%offset+pm_array_offset)
          newoff%data%ln(newoff%offset+i)=v%data%ln(v%offset+j)
          v=w%data%ptr(w%offset+pm_array_length)
          if(v%data%ln(v%offset+j)<=idx%data%ln(idx%offset+i).or.&
               idx%data%ln(idx%offset+i)<0) then
             errno=vector_index_error
             return
          endif
       enddo
       call array_vect_set_index(context,newvec,idx,newoff,e,esize,errno)
       call pm_delete_register(context,reg)
    endif
  contains
    include 'fesize.inc'
    include 'ftypeof.inc'
  end subroutine  array_set_index

  recursive subroutine array_vect_set_index(context,v,idx,offset,p,esize,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: p,v,idx,offset
    integer(pm_ln),intent(in):: esize
    integer,intent(out):: errno
    type(pm_ptr):: vec,dom,off,len,vec2,dom2,len2,off2,w,vv
    integer:: tno
    integer(pm_ln):: i,ix
    integer:: j
    type(pm_root),pointer:: root
    tno=pm_fast_typeof(p)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       vec=p%data%ptr(p%offset+pm_array_vect)
       dom=p%data%ptr(p%offset+pm_array_dom)
       len=p%data%ptr(p%offset+pm_array_length)
       off=p%data%ptr(p%offset+pm_array_offset)
       root=>pm_new_as_root(context,pm_pointer,esize+1_pm_ln)
       vv=root%ptr
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             vv%data%ptr(vv%offset+i)=w%data%ptr(w%offset+pm_array_dom)
          endif
       enddo
       call array_vect_set_index(context,vv,idx,offset,dom,esize,errno)
       call pm_delete_root(context,root)      
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             call array_assign(context,w,&
                  ix+offset%data%ln(offset%offset+i),&
                  p,i,errno)
          endif
       enddo
    case(pm_struct_type,pm_rec_type)
       root=>pm_add_root(context,pm_null_obj)
       do j=2,pm_fast_esize(p)
          root%ptr=pm_new(context,pm_pointer,esize+1_pm_ln)
          vv=root%ptr
          do i=0,esize
             w=v%data%ptr(v%offset+i)
             if(.not.pm_fast_isnull(w)) then
                vv%data%ptr(vv%offset+i)=w%data%ptr(w%offset+j)
             endif
          enddo
          call array_vect_set_index(context,vv,idx,offset,&
               p%data%ptr(p%offset+j),esize,errno)
       enddo
       call pm_delete_root(context,root)
    case(pm_int)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%i(w%offset+ix)=p%data%i(p%offset+i)
          endif
       enddo
    case(pm_long)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%ln(w%offset+ix)=p%data%ln(p%offset+i)
          endif
       enddo
    case(pm_single)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%r(w%offset+ix)=p%data%r(p%offset+i)
          endif
       enddo
    case(pm_double)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%d(w%offset+ix)=p%data%d(p%offset+i)
          endif
       enddo
    case(pm_logical)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%l(w%offset+ix)=p%data%l(p%offset+i)
          endif
       enddo
    case(pm_pointer)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             call pm_ptr_assign(context,w,ix,p%data%ptr(p%offset+i))
          endif
       enddo
    end select
  contains
    include 'fisnull.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine array_vect_set_index

  function array_pack(context,v,t,m,n,d) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_i16):: t
    type(pm_ptr),intent(in):: v,m,n,d
    type(pm_ptr):: ptr
    type(pm_reg),pointer:: reg
    type(pm_ptr),target:: nvec,noff
    type(pm_ptr):: vvec,mvec
    integer(pm_ln):: i,esize
    reg=>pm_register(context,'apack',nvec)
    vvec=v%data%ptr(v%offset+pm_array_vect)
    mvec=m%data%ptr(m%offset+pm_array_vect)
    esize=pm_fast_esize(n)
    nvec=pm_new(context,pm_pointer,esize+1)
    noff=pm_new(context,pm_long,esize+1)
    noff%data%ln(noff%offset:noff%offset+esize)=0
    do i=0,esize
       nvec%data%ptr(nvec%offset+i)=vector_pack(context,&
            vvec%data%ptr(vvec%offset+i),&
            mvec%data%ptr(mvec%offset+i),&
            n%data%ln(n%offset+i))
    enddo
    ptr=make_array(context,pm_array_type,t,nvec,d,n,noff)
    call pm_delete_register(context,reg)
  contains
    include 'fesize.inc'
  end function array_pack

  recursive function vector_pack(context,v,m,n) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,m
    integer(pm_ln),intent(in):: n
    type(pm_ptr):: ptr
    integer:: tno
    integer(pm_p):: k
    integer(pm_ln):: esize,esize2,i,j
    type(pm_ptr),target:: vv,dv,lv,ov
    type(pm_reg),pointer:: reg
    if(n<=0) then
       ptr=pm_null_obj
       return
    endif
    esize=n-1
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type)
       reg=>pm_register(context,'vpack',vv,dv,lv,ov)
       vv=vector_pack(context,v%data%ptr(v%offset+pm_array_vect),m,n)
       dv=vector_pack(context,v%data%ptr(v%offset+pm_array_dom),m,n)
       lv=vector_pack(context,v%data%ptr(v%offset+pm_array_length),m,n)
       ov=pm_new(context,pm_long,n)
       ov%data%ln(ov%offset:ov%offset+esize)=0
       ptr=make_array(context,pm_array_type,&
            int(v%data%ptr(v%offset+pm_array_typeof)%offset,pm_i16),&
            vv,dv,lv,ov)
       call pm_delete_register(context,reg)
    case(pm_struct_type,pm_rec_type)
       reg=>pm_register(context,'vpack2',vv)
       vv=pm_fast_new(context,pm_usr,int(pm_fast_esize(v),pm_p))
       vv%data%ptr(vv%offset)=v%data%ptr(v%offset)
       vv%data%ptr(vv%offset+1_pm_p)=v%data%ptr(v%offset+1_pm_p)
       do k=2,pm_fast_esize(v)
          vv%data%ptr(vv%offset+k)=vector_pack(context,&
               v%data%ptr(v%offset+k),m,n)
       enddo
       ptr=vv
       call pm_delete_register(context,reg)
    case(pm_pointer)
       ptr=pm_new(context,pm_pointer,n)
       j=0
       do i=0,pm_fast_esize(m)
          if(m%data%l(m%offset+i)) then
             ptr%data%ptr(ptr%offset+j)=v%data%ptr(v%offset+i)
             j=j+1
          endif
       enddo
    case(pm_int)
       ptr=pm_new(context,pm_int,n)
       esize2=pm_fast_esize(v)
       ptr%data%i(ptr%offset:ptr%offset+esize)=&
            pack(v%data%i(v%offset:v%offset+esize2),&
            m%data%l(m%offset:m%offset+esize2))
    end select
  contains
    include 'fesize.inc'
    include 'ftypeof.inc'
    include 'fnew.inc'
  end function vector_pack

  function poly_new(context,v,ve,esize) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ve
    integer(pm_ln),intent(in):: esize
    type(pm_ptr):: ptr
    integer(pm_ln):: i,j
    type(pm_ptr),target:: p,idx
    integer:: errno
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'polynw',p,idx)
    p=pm_new(context,pm_pointer,esize+1)
    idx=pm_fast_newnc(context,pm_long,1_pm_p)
    do i=0,pm_fast_esize(ve)
       j=ve%data%ln(ve%offset+i)
       idx%data%ln(idx%offset)=j
       call pm_ptr_assign(context,p,j,vector_get_elems(&
            context,v,idx,errno))
    enddo
    ptr=p
    call pm_delete_register(context,reg)
  contains
    include 'fesize.inc'
    include 'fvkind.inc'
    include 'fnewnc.inc'
  end function  poly_new

  subroutine poly_get(context,v,w,ve,esize,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,w,ve
    integer(pm_ln),intent(in):: esize
    integer,intent(inout):: errno
    integer(pm_ln):: i,j
    integer(pm_i16):: tno
    type(pm_ptr):: p
 
    tno=full_type(v)
    if(pm_fast_vkind(ve)==pm_null) then
       do j=0,esize
          p=w%data%ptr(w%offset+j)
          call assign_single(context,v,j,p,errno)
       enddo
    elseif(pm_fast_vkind(ve)==pm_logical) then
       do j=0,esize
          if(ve%data%l(ve%offset+j)) then
             p=w%data%ptr(w%offset+j)
             call assign_single(context,v,j,p,errno)
          endif
       enddo
    else
       do i=0,pm_fast_esize(ve)
          j=ve%data%ln(ve%offset+i)
          p=w%data%ptr(w%offset+j)
          call assign_single(context,v,j,p,errno)
       enddo
    endif
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end subroutine poly_get

  function poly_check_type(context,v,tno,ve,esize) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ve
    integer(pm_i16),intent(in):: tno
    integer(pm_ln),intent(in):: esize
    type(pm_ptr):: ptr
    integer(pm_ln):: i,j
    ptr=pm_new(context,pm_logical,esize+1)
    if(pm_fast_vkind(ve)==pm_null) then
       do i=0,esize
          ptr%data%l(ptr%offset+i)=&
               full_type(v%data%ptr(v%offset+i))==tno
       enddo
    elseif(pm_fast_vkind(ve)==pm_logical) then
       do i=0,esize
          if(ve%data%l(ve%offset+i)) then
             ptr%data%l(ptr%offset+i)=&
                  full_type(v%data%ptr(v%offset+i))==tno
          else
             ptr%data%l(ptr%offset+i)=.false.
          endif
       enddo
    else
       ptr%data%l(ptr%offset:ptr%offset+esize)=.false.
       do j=0,esize
          i=ve%data%ln(ve%offset+j)
          ptr%data%l(ptr%offset+i)=&
               full_type(v%data%ptr(v%offset+i))==tno
       enddo
    endif
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end function poly_check_type
     
  ! Import vector using information in import_vec
  ! import_vec(0)  : size of imported vector
  ! import_vec(1)  : offset into vector being imported
  ! import_vec(2)  : offset into element
  ! import_vec(3)  : total size of imported vector
  ! import_vec(4:) : size of each segment
  
  function import_vector(context,v,import_vec) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,import_vec
    type(pm_ptr):: ptr
    integer(pm_ln):: siz,start
    siz=import_vec%data%ln(import_vec%offset)+1_pm_ln
    start=import_vec%data%ln(import_vec%offset+1)
    ptr=make_vector(context,v,import_vec,start,4_pm_p,siz)
  end function import_vector

  subroutine split_import_vector(context,oldve,&
       lovec_out,hivec_out,ve_out,done_out)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: oldve
    type(pm_ptr),intent(out):: lovec_out,hivec_out,ve_out
    logical,intent(out):: done_out
    type(pm_ptr),target:: newvec,lovec,hivec,mask,newve
    type(pm_ptr):: oldvec
    integer(pm_ln):: i,j,n,nn,tot,odd,esize,jstart
    logical:: all,any,isodd
    type(pm_reg),pointer:: reg
    oldvec=oldve%data%ptr(oldve%offset+1_pm_p)
    reg=>pm_register(context,'splitv',newvec,lovec,hivec,mask,newve)
    esize=pm_fast_esize(oldvec)
    newvec=pm_new(context,pm_long,esize+1_pm_ln)
    newvec%data%ln(newvec%offset+1_pm_p)=oldvec%data%ln(oldvec%offset+1_pm_p)
    newvec%data%ln(newvec%offset+2_pm_p)=oldvec%data%ln(oldvec%offset+2_pm_p)
    newvec%data%ln(newvec%offset+3_pm_p)=oldvec%data%ln(oldvec%offset+3_pm_p)
    tot=0
    all=.true.
    do i=4_pm_ln,esize
       n=oldvec%data%ln(oldvec%offset+i)
       all=all.and.n<2
       n=(n+1)/2
       newvec%data%ln(newvec%offset+i)=n
       tot=tot+n
    enddo
    if(all) then
       done_out=.true.
       call pm_delete_register(context,reg)
       return
    endif
    newvec%data%ln(newvec%offset)=tot-1_pm_ln
    hivec=pm_new(context,pm_long,tot)
    lovec=pm_new(context,pm_long,tot)
    mask=pm_new(context,pm_logical,tot)
    done_out=.false.
    n=0
    jstart=0
    do i=4_pm_ln,esize
       nn=oldvec%data%ln(oldvec%offset+i)
       if(nn<2) then
          hivec%data%ln(hivec%offset+n)=0+jstart
          lovec%data%ln(lovec%offset+n)=0+jstart
          mask%data%l(mask%offset+n)=.false.
          n=n+1
       else
          do j=0,nn/2-1
             lovec%data%ln(lovec%offset+n)=j+jstart
             hivec%data%ln(hivec%offset+n)=j+nn/2+jstart
             mask%data%l(mask%offset+n)=.true.
             n=n+1
          enddo
          if(iand(nn,1)/=0) then
             lovec%data%ln(lovec%offset+n)=nn-1+jstart
             hivec%data%ln(hivec%offset+n)=nn-1+jstart
             mask%data%l(mask%offset+n)=.false.
             n=n+1
          endif
       endif
       jstart=jstart+nn
    enddo
    newve=pm_fast_newnc(context,pm_pointer,2_pm_p)
    newve%data%ptr(newve%offset)=mask
    newve%data%ptr(newve%offset+1_pm_p)=newvec
    ve_out=newve
    lovec_out=lovec
    hivec_out=hivec
    call pm_delete_register(context,reg)
  contains
    include 'fesize.inc'
    include 'fnewnc.inc'
    include 'fisnull.inc'
  end subroutine split_import_vector
  

  ! Assign first element in each iteration group for which mask==true
  ! to variable defined outside of for statement
  ! (used to implement find)
  subroutine vector_extract(context,vec,e,mask,import_vec,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: import_vec,mask,vec,e
    integer,intent(out):: errno
    type(pm_ptr):: index1,index2
    integer(pm_ln):: esize,i,j,k,k0,n,offset
    type(pm_root),pointer:: root1,root2
    esize=pm_fast_esize(import_vec)
    offset=import_vec%data%ln(import_vec%offset+1_pm_p)
    
    ! Calculate number of values to assign
    k=0
    n=0
    do i=4_pm_ln,esize
       k0=k
       do j=1,import_vec%data%ln(import_vec%offset+i)
          if(mask%data%l(mask%offset+k)) then
             n=n+1
             exit
          endif
          k=k+1
       enddo
       k=k0+import_vec%data%ln(import_vec%offset+i)
    enddo

    ! Build index vectors
    root1=>pm_new_as_root(context,pm_long,n)
    index1=root1%ptr
    root2=>pm_new_as_root(context,pm_long,n)
    index2=root2%ptr
    k=0
    n=0
    do i=4_pm_ln,esize
       k0=k
       do j=1,import_vec%data%ln(import_vec%offset+i)
          if(mask%data%l(mask%offset+k)) then
             index1%data%ln(index1%offset+n)=i-4_pm_ln+offset
             index2%data%ln(index2%offset+n)=k
             n=n+1
             exit
          endif
          k=k+1
       enddo
       k=k0+import_vec%data%ln(import_vec%offset+i)
    enddo
    ! Finally copy over values
    call vector_copy_elems(context,vec,e,index1%data%ln(index1%offset:),&
         index2%data%ln(index2%offset:),pm_fast_esize(index1)+1_pm_ln,errno)
    call pm_delete_root(context,root1)
    call pm_delete_root(context,root2)  
  contains
    include 'fesize.inc'
  end subroutine vector_extract
  
  ! Build a vector by replicating a scalar value
  recursive function make_vector(context,v,j,dispv,dispj,vsize) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,j
    integer(pm_p),intent(in):: dispj
    integer(pm_ln),intent(in):: dispv,vsize
    type(pm_ptr):: ptr
    integer:: tno
    type(pm_root),pointer:: root
    integer(pm_p):: i
    integer(pm_ln):: s,k,m,n,disp,siz
    type(pm_ptr):: p,q,len,off
    siz=max(vsize,1_pm_ln)
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       root=>pm_new_as_root(context,pm_usr,int(pm_array_size,pm_ln))
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+pm_array_typeof)=&
            v%data%ptr(v%offset+pm_array_typeof)
       call pm_ptr_assign(context,ptr,int(pm_array_dom,pm_ln),&
            make_vector(context,&
            v%data%ptr(v%offset+pm_array_dom),&
            j,dispv,dispj,siz))
       call pm_ptr_assign(context,ptr,int(pm_array_vect,pm_ln),&
            make_vector(context,&
            v%data%ptr(v%offset+pm_array_vect),&
            j,dispv,dispj,siz))
       call pm_ptr_assign(context,ptr,int(pm_array_length,pm_ln),&
            make_vector(context,&
            v%data%ptr(v%offset+pm_array_length),&
            j,dispv,dispj,siz))
       call pm_ptr_assign(context,ptr,int(pm_array_offset,pm_ln),&
            make_vector(context,&
            v%data%ptr(v%offset+pm_array_offset),&
            j,dispv,dispj,siz))
       call pm_delete_root(context,root)
    case(pm_struct_type,pm_rec_type)
       root=>pm_new_as_root(context,pm_usr,pm_fast_esize(v)+1_pm_ln)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+1_pm_p)=v%data%ptr(v%offset+1_pm_p)
       do i=2,pm_fast_esize(v)
          call pm_ptr_assign(context,ptr,int(i,pm_ln),&
               make_vector(context,v%data%ptr(v%offset+i),&
               j,dispv,dispj,siz))
       enddo
       call pm_delete_root(context,root)
    case(pm_pointer)
       ptr=pm_new(context,pm_pointer,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%ptr(ptr%offset+n)=v%data%ptr(v%offset+i+dispv)
             n=n+1
          enddo
       enddo
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
    case(pm_single)
       ptr=pm_new(context,pm_single,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%r(ptr%offset+n)=v%data%r(v%offset+i+dispv)
             n=n+1
          enddo
       enddo
    case(pm_double)
       ptr=pm_new(context,pm_double,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%d(ptr%offset+n)=v%data%d(v%offset+i+dispv)
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

  ! Make a copy of a vector
  ! size==-1 means use source size
  recursive function copy_vector(context,v,ve,start,size) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ve
    integer(pm_ln),intent(in):: start,size
    type(pm_ptr):: ptr
    integer:: typ
    integer(pm_p):: vk
    integer(pm_ln):: i,esize
    type(pm_ptr):: off,len,dom,vec,p,w,oldvec,oldoff,oldlen
    type(pm_root),pointer:: root
    type(pm_reg),pointer:: reg
    esize=size-1_pm_ln
    if(pm_fast_vkind(v)<=pm_null) then
       ptr=v
       return
    endif
    vk=pm_fast_vkind(v)
    if(vk<=pm_string) then
       if(esize<0) esize=pm_fast_esize(v)
       ptr=pm_new(context,vk,esize+1_pm_ln)
       call pm_copy_obj(v,start,ptr,0_pm_ln,esize)
    elseif(vk==pm_pointer) then
       if(esize<0) esize=pm_fast_esize(v)
       root=>pm_new_as_root(context,pm_pointer,esize+1_pm_ln)
       p=root%ptr
       do i=0,esize
          w=v%data%ptr(v%offset+i+start)
          if(.not.pm_fast_isnull(w)) then
             call pm_ptr_assign(context,p,i,&
                  copy_vector(context,w,ve,0_pm_ln,-1_pm_ln))
          endif
       enddo
       ptr=p
       call pm_delete_root(context,root)
    else
       typ=pm_fast_typeof(v)
       if(typ==pm_const_array_type.or.typ==pm_array_type) then
          reg=>pm_register(context,'cpvect',dom,len,off,vec)
          oldlen=v%data%ptr(v%offset+pm_array_length)
          if(esize<0) esize=pm_fast_esize(oldlen)
          dom=copy_vector(context,v%data%ptr(v%offset+pm_array_dom),&
               ve,start,esize+1_pm_ln)
          if(pm_fast_isnull(ve)) then
             len=pm_new(context,pm_long,esize+1_pm_ln)
             len%data%ln(len%offset:len%offset+esize)=&
                  oldlen%data%ln(oldlen%offset+start:oldlen%offset+start+esize)
          else
             len=vector_zero_unused(context,v%data%ptr(v%offset+pm_array_length),ve)
          endif
          off=pm_new(context,pm_long,esize+1_pm_ln)
          off%data%ln(off%offset:off%offset+esize)=0
          vec=pm_new(context,pm_pointer,esize+1_pm_ln)
          oldvec=v%data%ptr(v%offset+pm_array_vect)
          oldoff=v%data%ptr(v%offset+pm_array_offset)
          do i=0,esize
             if(len%data%ln(len%offset+i)>0) then
                call pm_ptr_assign(context,vec,i,copy_vector(context,&
                     oldvec%data%ptr(oldvec%offset+i+start),pm_null_obj,&
                     oldoff%data%ln(oldoff%offset+i+start),&
                     len%data%ln(len%offset+i)))
             endif
          enddo
          ptr=make_array(context,pm_array_type,&
               int(v%data%ptr(v%offset+pm_array_typeof)%offset,pm_i16),&
               vec,dom,len,off)
          call pm_delete_register(context,reg)
       else
          ptr=pm_new(context,pm_usr,pm_fast_esize(v)+1)
          root=>pm_add_root(context,ptr)
          ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
          ptr%data%ptr(ptr%offset+1_pm_p)=v%data%ptr(v%offset+1_pm_p)
          do i=2,pm_fast_esize(v)
             call pm_ptr_assign(context,ptr,i,&
                  copy_vector(context,v%data%ptr(v%offset+i),ve,start,esize+1_pm_ln))
          enddo
          call pm_delete_root(context,root)
       endif
    endif
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'ftypeof.inc'
    include 'ftiny.inc'
  end function copy_vector

  subroutine array_assign(context,lhs,ix,rhs,iy,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: lhs,rhs
    integer(pm_ln),intent(in):: ix,iy
    integer,intent(out):: errno
    type(pm_ptr):: vec1,len1,off1,vec2,len2,off2,v
    integer(pm_ln):: size1,size2
    vec1=lhs%data%ptr(lhs%offset+pm_array_vect)
    len1=lhs%data%ptr(lhs%offset+pm_array_length)
    off1=lhs%data%ptr(lhs%offset+pm_array_offset)
    vec2=rhs%data%ptr(rhs%offset+pm_array_vect)
    len2=rhs%data%ptr(rhs%offset+pm_array_length)
    off2=rhs%data%ptr(rhs%offset+pm_array_offset)
    size1=len1%data%ln(len1%offset+ix)
    size2=len2%data%ln(len2%offset+iy)
    if(size1==size2) then
       if(size1==0) return
       call vector_copy_range(context,&
            vec1%data%ptr(vec1%offset+ix),&
            off1%data%ln(off1%offset+ix),&
            vec2%data%ptr(vec2%offset+iy),&
            off2%data%ln(off2%offset+iy),&
            size1,errno)
    else
       if(size2==0) then
          if(ix==0) then
             v=vec1%data%ptr(vec1%offset)
             if(pm_fast_esize(v)>&
                  max(1_pm_ln,pm_fast_esize(vec1)/8_pm_ln)) then
                call pm_ptr_assign(context,vec1,ix,&
                     empty_copy_vector(context,&
                     v,1_pm_ln))
             endif
          else
             vec1%data%ptr(vec1%offset+ix)=pm_null_obj
          endif
          len1%data%ln(len1%offset+ix)=0
          off1%data%ln(off1%offset+ix)=0
       else
          call pm_ptr_assign(context,vec1,ix,&
               copy_vector(context,&
               vec2%data%ptr(vec2%offset+iy),&
               pm_null_obj,&
               off2%data%ln(off2%offset+iy),&
               len2%data%ln(len2%offset+iy)))
          len1%data%ln(len1%offset+ix)=size2
       endif
    endif
  contains
    include 'fesize.inc'
  end subroutine array_assign

  
  ! Make an empty copy of a vector with a new length
  recursive function empty_copy_vector(context,v,size) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: size
    type(pm_ptr):: ptr
    integer:: typ
    integer(pm_p):: vk
    integer(pm_ln):: i
    type(pm_ptr),target:: vec,dom,off,len
    type(pm_ptr):: w
    type(pm_root),pointer:: root
    type(pm_reg),pointer:: reg
    integer(pm_ln):: j,elsize
    if(pm_fast_isnull(v)) then
       ptr=v
       return
    endif
    vk=pm_fast_vkind(v)
    if(vk<=pm_null) then
       ptr=v
    elseif(vk<=pm_pointer) then
       ptr=pm_new(context,vk,size)
    else
       typ=pm_fast_typeof(v)
       if(typ==pm_array_type.or.typ==pm_const_array_type) then
          ! Empty copy of array - all with size 0
          reg=>pm_register(context,'vempty',vec,dom,len,off)
          dom=empty_copy_vector(&
               context,v%data%ptr(v%offset+pm_array_dom),size)
          vec=pm_new(context,pm_pointer,size)
          w=v%data%ptr(v%offset+pm_array_vect)
          call pm_ptr_assign(context,vec,0_pm_ln,&
               empty_copy_vector(context,w%data%ptr(w%offset),1_pm_ln))
          len=pm_new(context,pm_long,size)
          len%data%ln(len%offset:len%offset+size-1)=0
          off=pm_new(context,pm_long,size)
          off%data%ln(off%offset:off%offset+size-1)=0
          ptr=make_array(context,pm_array_type,&
               int(v%data%ptr(v%offset+pm_array_typeof)%offset,pm_i16),&
               vec,dom,len,off)
          call pm_delete_register(context,reg)
       else
          ptr=pm_new(context,pm_usr,pm_fast_esize(v)+1)
          root=>pm_add_root(context,ptr)
          ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
          ptr%data%ptr(ptr%offset+1_pm_p)=v%data%ptr(v%offset+1_pm_p)
          do i=2,pm_fast_esize(v)
             call pm_ptr_assign(context,ptr,i,&
                  empty_copy_vector(context,v%data%ptr(v%offset+i),size))
          enddo
          call pm_delete_root(context,root)
       endif
    endif
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'ftypeof.inc'
    include 'ftiny.inc'
  end function empty_copy_vector

  ! Make a vector by replicating v[offset]
  recursive function vector_from_scalar(context,v,offset,esize,is_const) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: offset,esize
    logical,intent(in):: is_const
    type(pm_ptr):: ptr
    integer:: typ
    integer(pm_p):: vk
    integer(pm_ln):: i
    type(pm_ptr),target:: vec,dom,off,len
    type(pm_ptr):: oldoff,oldlen,oldvec
    type(pm_reg),pointer:: reg
    type(pm_root),pointer:: root
    if(pm_fast_isnull(v)) then
       ptr=v
       return
    endif
    vk=pm_fast_vkind(v)
    if(vk<=pm_pointer) then
       ptr=pm_new(context,vk,esize+1_pm_ln)
       call pm_set_obj(ptr,0_pm_ln,esize,v,offset)
    else
       typ=pm_fast_typeof(v)
       if(typ==pm_const_array_type.or.typ==pm_array_type) then
          reg=>pm_register(context,'vscal',vec,dom,len,off)
          dom=vector_from_scalar(context,v%data%ptr(v%offset+pm_array_dom),&
               offset,esize,is_const)
          oldvec=v%data%ptr(v%offset+pm_array_vect)
          oldoff=v%data%ptr(v%offset+pm_array_offset)
          oldlen=v%data%ptr(v%offset+pm_array_length)
          len=pm_new(context,pm_long,esize+1_pm_ln)
          len%data%ln(len%offset:len%offset+esize)=&
               oldlen%data%ln(oldlen%offset+offset)
          if(is_const) then
             vec=pm_new(context,pm_pointer,esize+1_pm_ln)
             vec%data%ptr(vec%offset:vec%offset+esize)=&
                  oldvec%data%ptr(oldvec%offset+offset)
             off=pm_new(context,pm_long,esize+1_pm_ln)
             off%data%ln(off%offset:off%offset+esize)=&
                  oldoff%data%ln(oldoff%offset+offset)
          else
             vec=pm_new(context,pm_pointer,esize+1_pm_ln)
             do i=0,esize
                call pm_ptr_assign(context,vec,i,&
                     copy_vector(context,&
                     oldvec%data%ptr(oldvec%offset+offset),&
                     pm_null_obj,&
                     oldoff%data%ln(oldoff%offset+offset),&
                     oldlen%data%ln(oldlen%offset+offset)))
             enddo
             off=pm_new(context,pm_long,esize+1_pm_ln)
             off%data%ln(off%offset:off%offset+esize)=0
          endif
          ptr=make_array(context,pm_array_type,&
               int(v%data%ptr(v%offset+pm_array_typeof)%offset,pm_i16),&
               vec,dom,len,off)
          call pm_delete_register(context,reg)
       else
          ptr=pm_new(context,pm_usr,pm_fast_esize(v)+1)
          root=>pm_add_root(context,ptr)
          ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
          ptr%data%ptr(ptr%offset+1_pm_p)=v%data%ptr(v%offset+1_pm_p)
          do i=2,pm_fast_esize(v)
             call pm_ptr_assign(context,ptr,i,&
                  vector_from_scalar(context,v%data%ptr(v%offset+i),&
                  offset,esize,is_const))
          enddo
          call pm_delete_root(context,root)
       endif
    endif
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'ftypeof.inc'
    include 'ftiny.inc'
  end function vector_from_scalar

  recursive subroutine vector_assign(context,lhs,rhs,ve,errno,esize)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: lhs,rhs,ve
    integer(pm_ln),intent(in):: esize
    integer,intent(out):: errno
    integer(pm_p):: i
    integer(pm_i16):: tno,tno2
    integer(pm_ln):: j,k
    type(pm_ptr):: vec1,vec2,off1,off2,len1,len2,idx
    type(pm_root),pointer:: root
 
    if(pm_fast_isnull(lhs)) return
    tno=pm_fast_typeof(lhs)
    if(full_type(lhs)/=full_type(rhs)) then
       errno=vector_type_error
       return
    endif
    select case(tno)
    case(pm_array_type)
       call vector_assign(context,&
            lhs%data%ptr(lhs%offset+pm_array_dom),&
            rhs%data%ptr(rhs%offset+pm_array_dom),&
            ve,errno,esize)
       if(pm_fast_vkind(ve)==pm_null) then
          do j=0,esize
             call array_assign(context,lhs,j,rhs,j,errno)
             if(errno/=0) return
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do j=0,esize
             if(ve%data%l(ve%offset+j)) then
                call array_assign(context,lhs,j,rhs,j,errno)
                if(errno/=0) return
             endif
          enddo
       else
          do k=0,esize
             j=ve%data%ln(ve%offset+k)
             call array_assign(context,lhs,j,rhs,j,errno)
             if(errno/=0) return
          enddo
       endif
    case(pm_elemref_type)
       idx=vector_zero_unused(context,&
            lhs%data%ptr(lhs%offset+pm_elemref_idx),ve,-1_pm_ln)
       root=>pm_add_root(context,idx)
       call array_vect_set_index(context,&
            lhs%data%ptr(lhs%offset+pm_elemref_vect),&
            idx,&
            lhs%data%ptr(lhs%offset+pm_elemref_offset),&            
            rhs,esize,errno)
       call pm_delete_root(context,root)
    case(pm_struct_type,pm_rec_type)
       do i=2,pm_fast_esize(lhs)
          call vector_assign(context,&
               lhs%data%ptr(lhs%offset+i),&
               rhs%data%ptr(rhs%offset+i),ve,errno,esize)
       enddo
    case(pm_pointer)
       if(pm_fast_vkind(ve)==pm_null) then
          do j=0,esize
             call pm_ptr_assign(context,lhs,j,&
                  copy_vector(context,rhs%data%ptr(rhs%offset+j),&
                  pm_null_obj,0_pm_ln,-1_pm_ln))
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do j=0,esize
             if(ve%data%l(ve%offset+j)) then
                call pm_ptr_assign(context,lhs,j,&
                     copy_vector(context,rhs%data%ptr(rhs%offset+j),&
                     pm_null_obj,0_pm_ln,-1_pm_ln))
             endif
          enddo
       else
          do k=0,pm_fast_esize(ve)
             j=ve%data%ln(ve%offset+k)
             call pm_ptr_assign(context,lhs,j,&
                  copy_vector(context,rhs%data%ptr(rhs%offset+j),&
                  pm_null_obj,0_pm_ln,-1_pm_ln))
          enddo
       endif
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
    case(pm_single)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%r(lhs%offset:lhs%offset+esize)=&
               rhs%data%r(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%r(lhs%offset:lhs%offset+esize)=&
                  rhs%data%r(rhs%offset:rhs%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             lhs%data%r(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%r(rhs%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
    case(pm_double)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%d(lhs%offset:lhs%offset+esize)=&
               rhs%data%d(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%d(lhs%offset:lhs%offset+esize)=&
                  rhs%data%d(rhs%offset:rhs%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             lhs%data%d(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%d(rhs%offset+ve%data%ln(ve%offset+j))
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

  ! Test element-by_element equality of two vectors
  recursive subroutine vector_eq(context,v1,v2,eq,esize,ve)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v1,v2,ve
    integer(pm_ln),intent(in):: esize
    type(pm_ptr),intent(out):: eq
    integer:: tno1,tno2
    integer(pm_ln):: i,j
    integer:: k
    type(pm_ptr):: len1,off1,vec1,len2,off2,vec2

    tno1=pm_fast_typeof(v1)
    tno2=pm_fast_typeof(v2)
    if(full_type(v1)/=full_type(v2)) then
       eq%data%l(eq%offset:eq%offset+esize)=.false.
       return
    endif
    select case(tno1)
    case(pm_array_type)
       call vector_eq(context,v1%data%ptr(v1%offset+pm_array_dom),&
            v2%data%ptr(v2%offset+pm_array_dom),eq,esize,ve)
       len1=v1%data%ptr(v1%offset+pm_array_length)
       len2=v2%data%ptr(v2%offset+pm_array_length)
       off1=v1%data%ptr(v1%offset+pm_array_offset)
       off2=v2%data%ptr(v2%offset+pm_array_offset)
       vec1=v1%data%ptr(v1%offset+pm_array_vect)
       vec2=v2%data%ptr(v2%offset+pm_array_vect)
       if(pm_fast_vkind(ve)==pm_null) then
          do j=0,esize
             if(eq%data%l(eq%offset+j)) then
                if(len1%data%ln(len1%offset+j)/=len2%data%ln(len2%offset+j)) then
                   eq%data%l(eq%offset+j)=.false.
                else
                   eq%data%l(eq%offset+j)=vector_all_eq(context,&
                        vec1%data%ptr(vec1%offset+j),&
                        vec2%data%ptr(vec2%offset+j),&
                        off1%data%ln(off1%offset+j),&
                        off2%data%ln(off2%offset+j),&
                        len1%data%ln(len1%offset+j))
                endif
             endif
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do j=0,esize
             if(ve%data%l(ve%offset+j)) then
                if(eq%data%l(eq%offset+j)) then
                   if(len1%data%ln(len1%offset+j)/=&
                        len2%data%ln(len2%offset+j)) then
                      eq%data%l(eq%offset+j)=.false.
                   else
                      eq%data%l(eq%offset+j)=vector_all_eq(context,&
                           vec1%data%ptr(vec1%offset+j),&
                           vec2%data%ptr(vec2%offset+j),&
                           off1%data%ln(off1%offset+j),&
                           off2%data%ln(off2%offset+j),&
                           len1%data%ln(len1%offset+j))
                   endif
                endif
             endif
          enddo
       else
          do i=0,esize
             j=ve%data%ln(ve%offset+j)
             if(eq%data%l(eq%offset+j)) then
                if(len1%data%ln(len1%offset+j)/=&
                     len2%data%ln(len2%offset+j)) then
                   eq%data%l(eq%offset+j)=.false.
                else
                   eq%data%l(eq%offset+j)=vector_all_eq(context,&
                        vec1%data%ptr(vec1%offset+j),&
                        vec2%data%ptr(vec2%offset+j),&
                        off1%data%ln(off1%offset+j),&
                        off2%data%ln(off2%offset+j),&
                        len1%data%ln(len1%offset+j))
                endif
             endif
          enddo
       endif
    case(pm_struct_type,pm_rec_type)
       do k=2,pm_fast_esize(v1)
          call vector_eq(context,v1%data%ptr(v1%offset+k),&
               v2%data%ptr(v2%offset+k),eq,esize,ve)
       enddo
    case(pm_pointer)
       if(pm_fast_vkind(ve)==pm_null) then
          do j=0,esize
             call vector_eq(context,v1%data%ptr(v1%offset+j),&
                  v2%data%ptr(v2%offset+j),eq,esize,ve)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do j=0,esize
             if(ve%data%l(ve%offset+j)) then
                call vector_eq(context,v1%data%ptr(v1%offset+j),&
                     v2%data%ptr(v2%offset+j),eq,esize,ve)
             endif
          enddo
       else
          do i=0,pm_fast_esize(ve)
             j=ve%data%ln(ve%offset+i)
             call vector_eq(context,v1%data%ptr(v1%offset+j),&
                  v2%data%ptr(v2%offset+j),eq,esize,ve)
          enddo
       endif
    case(pm_int)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.&
               v1%data%i(v1%offset:v1%offset+esize)==&
               v2%data%i(v2%offset:v2%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.&
                  v1%data%i(v1%offset:v1%offset+esize)==&
                  v2%data%i(v2%offset:v2%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%i(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%i(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          end forall
       endif
    case(pm_long)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.&
               v1%data%ln(v1%offset:v1%offset+esize)==&
               v2%data%ln(v2%offset:v2%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.&
                  v1%data%ln(v1%offset:v1%offset+esize)==&
                  v2%data%ln(v2%offset:v2%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%ln(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%ln(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          end forall
       endif
    case(pm_single)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.&
               v1%data%r(v1%offset:v1%offset+esize)==&
               v2%data%r(v2%offset:v2%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.&
                  v1%data%r(v1%offset:v1%offset+esize)==&
                  v2%data%r(v2%offset:v2%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%r(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%r(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          end forall
       endif
    case(pm_double)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.&
               v1%data%d(v1%offset:v1%offset+esize)==&
               v2%data%d(v2%offset:v2%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.&
                  v1%data%d(v1%offset:v1%offset+esize)==&
                  v2%data%d(v2%offset:v2%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%d(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%d(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          end forall
       endif
    case(pm_logical)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.(&
               v1%data%l(v1%offset:v1%offset+esize).eqv.&
               v2%data%l(v2%offset:v2%offset+esize))
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.(&
                  v1%data%l(v1%offset:v1%offset+esize).eqv.&
                  v2%data%l(v2%offset:v2%offset+esize))
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.(&
                  v1%data%l(ve%data%ln(ve%offset+j)+&
                  v1%offset).eqv.&
                  v2%data%l(ve%data%ln(ve%offset+j)+&
                  v2%offset))
          end forall
       endif
    end select

  contains
    include 'fesize.inc'
    include 'ftypeof.inc'
    include 'fvkind.inc'
  end subroutine vector_eq

  recursive function vector_all_eq(context,v1,v2,istart1,istart2,isize) result(ok)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v1,v2
    integer(pm_ln),intent(in):: istart1,istart2,isize
    logical:: ok
    integer(pm_ln):: esize
    integer:: tno1,tno2
    integer(pm_ln):: size1,size2,start1,start2
    integer:: k
    type(pm_ptr):: len1,len2,off1,off2,vec1,vec2
    integer(pm_ln):: i,j
    esize=isize-1
    tno1=pm_fast_typeof(v1)
    tno2=pm_fast_typeof(v2)
    if(tno1/=tno2) then
       ok=.false.
       return
    endif
    select case(tno1)
    case(pm_array_type)
       if(.not.vector_all_eq(context,v1%data%ptr(v1%offset+pm_array_dom),&
            v2%data%ptr(v2%offset+pm_array_dom),istart1,istart2,isize)) then
          ok=.false.
          return
       endif
       len1=v1%data%ptr(v1%offset+pm_array_length)
       len2=v2%data%ptr(v2%offset+pm_array_length)
       off1=v1%data%ptr(v1%offset+pm_array_offset)
       off2=v2%data%ptr(v2%offset+pm_array_offset)
       vec1=v1%data%ptr(v1%offset+pm_array_vect)
       vec2=v2%data%ptr(v2%offset+pm_array_vect)
       do j=0,esize
          start1=off1%data%ln(off1%offset+j+istart1)
          start2=off2%data%ln(off2%offset+j+istart2)
          size1=len1%data%ln(len1%offset+j+istart1)
          size2=len2%data%ln(len2%offset+j+istart2)
          if(size1/=size2) then
             ok=.false.
             return
          endif
          ok=vector_all_eq(context,vec1%data%ptr(vec1%offset+j),&
               vec2%data%ptr(vec2%offset+j),start1,start2,size1)
          if(.not.ok) return
       enddo
    case(pm_struct_type,pm_rec_type)
       do k=2,pm_fast_esize(v1)
          if(.not.vector_all_eq(context,v1%data%ptr(v1%offset+k),&
               v2%data%ptr(v2%offset+k),istart1,istart2,isize)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
    case(pm_pointer)
       do j=0,esize
          if(.not.vector_all_eq(context,v1%data%ptr(v1%offset+istart1+j),&
                  v2%data%ptr(v2%offset+istart2+j),0_pm_ln,0_pm_ln,1_pm_ln)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
    case(pm_int)
       ok=all(v1%data%i(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%i(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_long)
       ok=all(v1%data%ln(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%ln(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_single)
       ok=all(v1%data%r(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%r(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_double)
       ok=all(v1%data%d(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%d(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_logical)
       ok=all(v1%data%l(v1%offset+istart1:v1%offset+istart1+esize).eqv.&
            v2%data%l(v2%offset+istart2:v2%offset+istart2+esize))
    end select

  contains
    include 'fesize.inc'
    include 'ftypeof.inc'
    include 'fvkind.inc'
  end function vector_all_eq

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
    tstart=import_vec%data%ln(import_vec%offset+2)
    nstart=import_vec%data%ln(import_vec%offset+1)
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
           idx=mod(idx,(iend-istart)/istep+1_pm_ln)
           idx=istart+idx*istep
        endif
        do j=1,import_vec%data%ln(import_vec%offset+i-nstart+4_pm_ln)
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

  ! Compute a vector of indices
  ! start, start+step, .., 
  ! Each index repeated elsize times
  ! The sequence truncated to siz elements
  function vector_iota_trunc(context,&
       elsize,start,end,step,siz,import_vec) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: import_vec,elsize,start,end,siz,step
    type(pm_ptr):: ptr
    type(pm_ptr),target:: vec,offset
    integer(pm_ln):: i,j,k,n,m,tstart,nstart,ielsize
    integer(pm_ln):: istart,iend,isiz,istep,idx,rpt
    type(pm_reg),pointer:: reg
    n=import_vec%data%ln(import_vec%offset)+1_pm_ln
    vec=pm_new(context,pm_long,n)
    tstart=import_vec%data%ln(import_vec%offset+2)
    nstart=import_vec%data%ln(import_vec%offset+1)
    k=0
    do i=nstart,pm_fast_esize(import_vec)-4_pm_ln+nstart
        ielsize=elsize%data%ln(elsize%offset+i)
        istart=start%data%ln(start%offset+i)
        iend=end%data%ln(end%offset+i)
        isiz=siz%data%ln(siz%offset+i)
        istep=step%data%ln(step%offset+i)
        if(tstart==0) then
           rpt=ielsize
           idx=istart
        else
           idx=mod(tstart,isiz)
           rpt=ielsize-(idx-(idx/ielsize)*ielsize)
           idx=istart+idx*istep
        endif
        m=0
        do j=1,import_vec%data%ln(import_vec%offset+i-nstart+4_pm_ln)
           vec%data%ln(vec%offset+k)=idx
           k=k+1
           rpt=rpt-1
           if(rpt==0) then
              rpt=ielsize
              idx=idx+istep
              m=m+1
              if(idx>iend) idx=istart
              if(m>=isiz) then
                 idx=istart
                 m=0
              endif
           endif
        enddo
    enddo
    ptr=vec
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end function vector_iota_trunc
  
  subroutine vector_indices(imp,idx)
    type(pm_ptr):: imp,idx
    integer(pm_ln):: i,j,k
    k=idx%offset
    do i=imp%offset+4,imp%offset+pm_fast_esize(imp)
       do j=0,imp%data%ln(i)-1
          idx%data%ln(k)=j
          k=k+1
       enddo
    enddo
  contains
    include 'fesize.inc'
  end subroutine vector_indices
  
  ! Get elements from a vector (indices start at 0)
  recursive function vector_get_elems(context,v,ix,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ix
    integer,intent(out):: errno
    type(pm_ptr):: ptr
    integer:: tno
    integer(pm_ln):: h,i,j,k,nsiz,esize
    type(pm_ptr),target:: vec,dom,shape,len,off,oldvec,oldlen,oldoff
    type(pm_root),pointer:: root
    type(pm_reg),pointer:: reg
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       reg=>pm_register(context,'vgetel',vec,dom,shape,len,off)
       dom=vector_get_elems(context,v%data%ptr(v%offset+pm_array_dom),ix,errno)
       oldlen=v%data%ptr(v%offset+pm_array_length)
       len=vector_get_elems(context,oldlen,ix,errno)
       oldoff=v%data%ptr(v%offset+pm_array_offset)
       off=pm_new(context,pm_long,esize+1_pm_ln)
       off%data%ln(off%offset:off%offset+esize)=0
       oldvec=v%data%ptr(v%offset+pm_array_vect)
       esize=pm_fast_esize(ix)
       vec=pm_new(context,pm_pointer,esize+1_pm_ln)
       do j=0,esize
          i=ix%data%ln(ix%offset+j)
          vec%data%ptr(vec%offset+j)=copy_vector(context,&
               oldvec%data%ptr(oldvec%offset+i),&
               pm_null_obj,&
               oldoff%data%ln(oldoff%offset+i),&
               oldlen%data%ln(oldlen%offset+i))
       enddo
       call pm_delete_register(context,reg)
    case(pm_struct_type,pm_rec_type)
       root=>pm_new_as_root(context,pm_usr,pm_fast_esize(v)+1)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+1_pm_p)=v%data%ptr(v%offset+1_pm_p)
       do i=2,pm_fast_esize(v)
          ptr%data%ptr(ptr%offset+i)=vector_get_elems(context,&
               v%data%ptr(v%offset+i),ix,errno)
       enddo
       call pm_delete_root(context,root)
    case(pm_pointer)
       root=>pm_new_as_root(context,pm_usr,3_pm_ln)
       ptr=root%ptr
       do i=0,pm_fast_esize(ix)
          call pm_ptr_assign(context,ptr,i,&
               v%data%ptr(v%offset+ix%data%ln(ix%offset+i)))
       enddo
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
    case default
       write(*,*) 'TYPENO=',tno
       call pm_panic('vector-get-elems')
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end function vector_get_elems

  ! Set elements in a vector
  recursive subroutine vector_set_elems(context,v,ix,e,ve,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ix,e,ve
    integer,intent(out):: errno
    integer:: tno
    integer(pm_ln):: h,i,j,m,siz
    integer:: k
    type(pm_ptr):: len,off,avec,len2,off2,avec2
    tno=pm_fast_typeof(v)
    if(full_type(e)/=full_type(v)) then
       write(*,*) trim(pm_typ_as_string(context,int(tno,pm_i16))),&
            '<-->',trim(pm_typ_as_string(context,pm_fast_typeof(e)))
       errno=vector_type_error
       return
    endif
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call vector_set_elems(context,&
            v%data%ptr(v%offset+pm_array_dom),&
            ix,e%data%ptr(e%offset+pm_array_dom),ve,errno)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             call array_assign(context,v,ix%data%ln(ix%offset+i),e,i,errno)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                call array_assign(context,v,ix%data%ln(ix%offset+i),e,i,errno)
             endif
          enddo
       else
          do m=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+m)
             call array_assign(context,v,ix%data%ln(ix%offset+i),e,i,errno)
          enddo
       endif
    case(pm_struct_type,pm_rec_type)
       do k=2,pm_fast_esize(v)
          call vector_set_elems(context,v%data%ptr(v%offset+k),&
               ix,e%data%ptr(e%offset+k),ve,errno)
       enddo
    case(pm_pointer)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             call pm_ptr_assign(context,v,ix%data%ln(ix%offset),&
                  copy_vector(context,e%data%ptr(e%offset+i),&
                  pm_null_obj,0_pm_ln,-1_pm_ln))
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                call pm_ptr_assign(context,v,ix%data%ln(ix%offset),&
                  copy_vector(context,e%data%ptr(e%offset+i),&
                  pm_null_obj,0_pm_ln,-1_pm_ln))
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             call pm_ptr_assign(context,v,ix%data%ln(ix%offset),&
                  copy_vector(context,e%data%ptr(e%offset+i),&
                  pm_null_obj,0_pm_ln,-1_pm_ln))
          enddo
       endif
    case(pm_int)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%i(v%offset+ix%data%ln(ix%offset+i))=e%data%i(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%i(v%offset+ix%data%ln(ix%offset+i))=e%data%i(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%i(v%offset+ix%data%ln(ix%offset+i))=e%data%i(e%offset+i)
          enddo
       endif
    case(pm_long)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%ln(v%offset+ix%data%ln(ix%offset+i))=e%data%ln(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%ln(v%offset+ix%data%ln(ix%offset+i))=e%data%ln(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%ln(v%offset+ix%data%ln(ix%offset+i))=e%data%ln(e%offset+i)
          enddo
       endif
    case(pm_single)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%r(v%offset+ix%data%ln(ix%offset+i))=e%data%r(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%r(v%offset+ix%data%ln(ix%offset+i))=e%data%r(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%r(v%offset+ix%data%ln(ix%offset+i))=e%data%r(e%offset+i)
          enddo
       endif
    case(pm_double)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%d(v%offset+ix%data%ln(ix%offset+i))=e%data%d(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%d(v%offset+ix%data%ln(ix%offset+i))=e%data%d(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%d(v%offset+ix%data%ln(ix%offset+i))=e%data%d(e%offset+i)
          enddo
       endif
    case(pm_logical)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%l(v%offset+ix%data%ln(ix%offset+i))=e%data%l(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%l(v%offset+ix%data%ln(ix%offset+i))=e%data%l(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%ln(v%offset+ix%data%ln(ix%offset+i))=e%data%ln(e%offset+i)
          enddo
       endif 
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'
  end subroutine vector_set_elems

  recursive subroutine assign_single(context,v,j,e,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,e
    integer(pm_ln),intent(in):: j
    integer,intent(out):: errno
    integer:: tno,k
    tno=pm_fast_typeof(v)
    if(full_type(v)/=full_type(e)) then
       errno=vector_type_error
       return
    endif
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call assign_single(context,v%data%ptr(v%offset+pm_array_dom),j,&
            e%data%ptr(e%offset+pm_array_dom),errno)
       call array_assign(context,v,j,e,0_pm_ln,errno)
    case(pm_struct_type,pm_rec_type)
       do k=2,pm_fast_esize(v)
          call assign_single(context,v%data%ptr(v%offset+k),j,e%data%ptr(e%offset+k),errno)
       enddo
    case(pm_pointer)
       call assign_single(context,v%data%ptr(v%offset+j),0_pm_ln,e%data%ptr(e%offset),errno)
    case(pm_int)
       v%data%i(v%offset+j)=e%data%i(e%offset)
    case(pm_long)
       v%data%ln(v%offset+j)=e%data%ln(e%offset)
    case(pm_single)
       v%data%r(v%offset+j)=e%data%r(e%offset)
    case(pm_double)
       v%data%d(v%offset+j)=e%data%d(e%offset)
    case(pm_logical)
       v%data%l(v%offset+j)=e%data%l(e%offset)
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine assign_single
  
  
  ! Copy elements of vector e to elements of vector v
  recursive subroutine vector_copy_elems(context,v,e,ix,iy,n,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,e
    integer(pm_ln),dimension(n),intent(in):: ix,iy
    integer(pm_ln),intent(in):: n
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
    case(pm_array_type,pm_const_array_type)
       call vector_copy_elems(context,v%data%ptr(v%offset+pm_array_dom),&
            e%data%ptr(e%offset+pm_array_dom),ix,iy,n,errno)
       do i=1,n
          call array_assign(context,v,ix(i),e,iy(i),errno)
          if(errno/=0) return
       enddo
    case(pm_struct_type,pm_rec_type)
       do k=2,pm_fast_esize(v)
          call vector_copy_elems(context,&
               v%data%ptr(v%offset+k),&
               e%data%ptr(e%offset+k),&
               ix,iy,n,errno)
       enddo
    case(pm_pointer)
       do i=1,n
          call assign_single(context,v%data%ptr(v%offset+ix(i)),0_pm_ln,&
               e%data%ptr(e%offset+iy(i)),errno)
       enddo
    case(pm_int)
       do i=1,n
          v%data%i(v%offset+ix(i))=&
               e%data%i(e%offset+iy(i))
       enddo
    case(pm_long)
       do i=1,n
          v%data%ln(v%offset+ix(i))=&
               e%data%ln(e%offset+iy(i))
       enddo
    case(pm_int8)
       do i=1,n
          v%data%i8(v%offset+ix(i))=&
               e%data%i8(e%offset+iy(i))
       enddo
    case(pm_int16)
       do i=1,n
          v%data%i16(v%offset+ix(i))= &
               e%data%i16(e%offset+iy(i))
       enddo
    case(pm_int32)
       do i=1,n
          v%data%i32(v%offset+ix(i))= &
               e%data%i32(e%offset+iy(i))
       enddo
    case(pm_int64)
       do i=1,n
          v%data%i64(v%offset+ix(i))= &
               e%data%i64(e%offset+iy(i))
       enddo
    case(pm_int128)
       do i=1,n
          v%data%i128(v%offset+ix(i))= &
               e%data%i128(e%offset+iy(i))
       enddo
    case(pm_single)
       do i=1,n
          v%data%r(v%offset+ix(i))=  & 
               e%data%r(e%offset+iy(i))
       enddo
    case(pm_double)
       do i=1,n
          v%data%d(v%offset+ix(i))= &
               e%data%d(e%offset+iy(i))
       enddo
    case(pm_real32)
       do i=1,n
          v%data%r32(v%offset+ix(i))= &
               e%data%r32(e%offset+iy(i))
       enddo
    case(pm_real64)
       do i=1,n
          v%data%r64(v%offset+ix(i))= &
               e%data%r64(e%offset+iy(i))
       enddo
    case(pm_real128)
       do i=1,n
          v%data%r128(v%offset+ix(i))= &
               e%data%r128(e%offset+iy(i))
       enddo
    case(pm_single_complex)
       do i=1,n
          v%data%c(v%offset+ix(i))= &
               e%data%c(e%offset+iy(i))
       enddo
    case(pm_double_complex)
       do i=1,n
          v%data%dc(v%offset+ix(i)) = &
               e%data%dc(e%offset+iy(i))
       enddo
    case(pm_complex64)
       do i=1,n
          v%data%c64(v%offset+ix(i))= &
               e%data%c64(e%offset+iy(i))
       enddo
    case(pm_complex128)
       do i=1,n
          v%data%c128(v%offset+ix(i))= &
               e%data%c128(e%offset+iy(i))
       enddo
    case(pm_complex256)
       do i=1,n
          v%data%c256(v%offset+ix(i))= &
               e%data%c256(e%offset+iy(i))
       enddo
    case(pm_logical)
       do i=1,n
          v%data%l(v%offset+ix(i))= &
               e%data%l(e%offset+iy(i))
       enddo
    case(pm_packed_logical)
       do i=1,n
          v%data%pl(v%offset+ix(i))= &
               e%data%pl(e%offset+iy(i))
       enddo
    case(pm_string)
       do i=1,n
          v%data%s(v%offset+ix(i))= &
               e%data%s(e%offset+iy(i))
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
    case(pm_array_type,pm_const_array_type)
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
       call vector_copy_range(context,&
            v%data%ptr(v%offset+pm_array_dom),start1,&
            e%data%ptr(e%offset+pm_array_dom),start2,&
            siz,errno)
       do j=0,siz-1
          call array_assign(context,v,j+start1,e,j+start2,errno)
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
          return
       else
          call pm_copy_obj(e,start2,v,start1,siz-1)
       endif
    end select
    errno=0
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'   
  end subroutine  vector_copy_range

  recursive subroutine vector_dump(context,v,depth)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v
    integer,intent(in):: depth
    character(len=80):: spaces=' '
    integer:: k
    type(pm_ptr):: name
    integer:: tno,i
    if(depth>=35) then
       write(*,*) spaces(1:depth*2),'...'
    endif
    k=pm_fast_typeof(v)
    select case(k)
    case(pm_array_type,pm_const_array_type)
       write(*,*) spaces(1:depth*2),'Array ('
       call vector_dump(context,v%data%ptr(v%offset+pm_array_vect),depth+1)
       write(*,*) spaces(1:depth*2),') over ('
       call vector_dump(context,v%data%ptr(v%offset+pm_array_dom),depth+1)
       write(*,*) spaces(1:depth*2),')'
    case(pm_struct_type,pm_rec_type)
       tno=full_type(v)
       name=pm_typ_vect(context,int(tno,pm_i16))
       name=pm_name_val(context,int(pm_tv_name(name),pm_p))
       tno=name%data%i16(name%offset)
       if(k==pm_struct_type) then
          write(*,*) spaces(1:depth*2),'struct ',&
               trim(pm_name_as_string(context,int(tno,pm_p))),'('
       else
          write(*,*) spaces(1:depth*2),'rec ',&
               trim(pm_name_as_string(context,int(tno,pm_p))),'('
       endif
       do i=1,pm_fast_esize(name)
          tno=name%data%i16(name%offset+i)
          write(*,*) spaces(1:depth*2+2),trim(pm_name_as_string(context,int(tno,pm_p))),'='
          call vector_dump(context,v%data%ptr(v%offset+i+1),depth+2)
       enddo
       write(*,*) spaces(1:depth*2),')'
    case default
       call pm_dump_tree(context,6,v,depth)
    end select

  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
       
  end subroutine vector_dump
  

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
       ptr%data%ln(ptr%offset+i)=j+off%data%ln(off%offset+i)
    enddo
    if(iserr) then
       errno=vector_index_error
       return
    endif
  contains
    include 'fesize.inc'
  end function index_vector

  ! For vector of arrays (defined by len(gth) and off(set) vectors)
  ! and vector of indices, combined with indirection vector,
  ! compute vector of locations in array element vector
  function index_vector_indirect(context,len,off,idx,ind,ve,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: len,off,idx,ind,ve
    integer,intent(out):: errno
    type(pm_ptr):: ptr
    integer(pm_ln):: siz,i,j,k
    logical:: iserr
    siz=pm_fast_esize(idx)
    ptr=vector_zero_unused(context,idx,ve)
    iserr=.false.
    do k=0,siz
       i=ind%data%ln(ind%offset+k)
       j=ptr%data%ln(ptr%offset+k)
       iserr=iserr.or.j<0.or.j>len%data%ln(len%offset+i)
       ptr%data%ln(ptr%offset+k)=j+off%data%ln(off%offset+i)
    enddo
    if(iserr) then
       errno=vector_index_error
       return
    endif
  contains
    include 'fesize.inc'
    include 'ftypeof.inc'
  end function index_vector_indirect

  ! For vector of arrays (defined by len(gth) and off(set) vectors)
  ! and vector of indices, compute vector of locations in array
  ! element vector, excluding inactive locations (as defined by ve)
  function index_vector_used(context,len,off,idx,ve,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: len,off,idx,ve
    integer,intent(out):: errno
    type(pm_ptr):: ptr
    integer(pm_ln):: siz,psiz,i,j,k
    logical:: iserr
    if(pm_fast_isnull(ve)) then
       siz=pm_fast_esize(idx)
       ptr=pm_new(context,pm_long,siz)
       iserr=.false.
       do i=0,siz
          j=idx%data%ln(idx%offset+i)
          iserr=iserr.or.j<0.or.j>len%data%ln(len%offset+i)
          ptr%data%ln(ptr%offset+i)=j+off%data%ln(off%offset+i)
       enddo
    elseif(pm_fast_vkind(ve)==pm_logical) then
       siz=pm_fast_esize(ve)
       psiz=0
       do i=0,siz
          if(ve%data%l(ve%offset+j)) then
             psiz=psiz+1
          endif
       enddo
       ptr=pm_new(context,pm_long,psiz)
       iserr=.false.
       k=0
       do i=0,siz
          if(ve%data%l(ve%offset+i)) then
             j=idx%data%ln(idx%offset+i)
             iserr=iserr.or.j<0.or.j>len%data%ln(len%offset+i)
             ptr%data%ln(ptr%offset+k)=j+off%data%ln(off%offset+i)
             k=k+1
          endif
       enddo
       if(pm_debug_level>0) then
          if(k/=psiz) then
             call pm_panic('index_vector_used')
          endif
       endif
    else
       psiz=pm_fast_esize(ve)
       ptr=pm_new(context,pm_long,siz)
       iserr=.false.
       do k=0,psiz
          i=ve%data%ln(ve%offset+k)
          j=idx%data%ln(idx%offset+i)
          iserr=iserr.or.j<0.or.j>len%data%ln(len%offset+i)
          ptr%data%ln(ptr%offset+k)=j+off%data%ln(off%offset+i)
       enddo
    endif
    
    if(iserr) then
       errno=vector_index_error
       return
    endif
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'
  end function index_vector_used

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
    vec=pm_new(context,pm_pointer,esize+1)
    j=0
    do i=0,vsize
       k=ve%data%ln(ve%offset+i)
       call fmt(v,k,mess)
       vec%data%ptr(vec%offset+k)=pm_new_string(context,trim(mess))
       len%data%ln(len%offset+k)=len_trim(mess)
    enddo
    str=make_array(context,pm_array_type,int(pm_string_type,pm_i16),vec,len,len,off)
    call pm_delete_register(context,reg)
  contains
    include 'fesize.inc'
  end function vector_make_string

  function vector_concat_string(context,ve,v1,v2) result(str)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ve,v1,v2
    type(pm_ptr):: str
    type(pm_ptr),target:: vec,len,off
    type(pm_ptr):: vec1,vec2,len1,len2,off1,off2,s,s1,s2
    integer(pm_ln):: i,j,k,start1,start2,size,size1,size2,vsize,esize
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'concat',vec,len,off)
    vsize=pm_fast_esize(ve)
    vec1=v1%data%ptr(v1%offset+pm_array_vect)
    vec2=v2%data%ptr(v2%offset+pm_array_vect)
    len1=v1%data%ptr(v1%offset+pm_array_length)
    len2=v2%data%ptr(v2%offset+pm_array_length)
    off1=v1%data%ptr(v1%offset+pm_array_offset)
    off2=v2%data%ptr(v2%offset+pm_array_offset)
    esize=pm_fast_esize(len1)
    len=pm_new(context,pm_long,esize+1)
    len%data%ln(len%offset:len%offset+esize)=0
    off=pm_new(context,pm_long,esize+1)
    off%data%ln(off%offset:off%offset+esize)=0
    vec=pm_new(context,pm_pointer,esize+1)
    do i=0,vsize
       j=ve%data%ln(ve%offset+i)
       size1=len1%data%ln(len1%offset+j)
       size2=len2%data%ln(len2%offset+j)
       start1=off1%data%ln(off1%offset+j)
       start2=off2%data%ln(off2%offset+j)
       size=size1+size2
       len%data%ln(len%offset+j)=size
       s=pm_new(context,pm_string,size)
       vec%data%ptr(vec%offset+j)=s
       s1=vec1%data%ptr(vec1%offset+j)
       s2=vec2%data%ptr(vec2%offset+j)
       s%data%s(s%offset:s%offset+size1-1)=&
            s1%data%s(s1%offset+start1:s1%offset+start1+size1-1)
       s%data%s(s%offset+size1:s%offset+size1+size2-1)=&
            s2%data%s(s2%offset+start2:s2%offset+start2+size2-1)
    enddo
    str=make_array(context,pm_array_type,int(pm_string_type,pm_i16),vec,len,len,off)
    call pm_delete_register(context,reg)
  contains
    include 'fesize.inc'
  end function vector_concat_string

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
    vec=vec%data%ptr(vec%offset+i)
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
    type(pm_ptr),target:: len,off,vec
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'make_str',len,off,vec)
    len=pm_new(context,pm_long,esize+1)
    len%data%ln(len%offset:len%offset+esize)=pm_fast_esize(val)
    off=pm_new(context,pm_long,esize+1)
    off%data%ln(off%offset:off%offset+esize)=0
    vec=pm_new(context,pm_pointer,esize+1)
    vec%data%ptr(vec%offset:vec%offset+esize)=val
    str=make_array(context,pm_array_type,int(pm_string_type,pm_i16),vec,len,len,off)
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
    character(len=20):: mess
    mess=' '
    write(mess,'(i20)') v%data%ln(v%offset+n)
    str=adjustl(mess)
  end subroutine fmt_ln

  subroutine fmt_r(v,n,str)
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: n
    character(len=*),intent(out):: str
    character(len=15):: mess
    mess=' '
    write(mess,'(g15.8)') v%data%r(v%offset+n)
    str=adjustl(mess)
  end subroutine fmt_r

  subroutine fmt_d(v,n,str)
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: n
    character(len=*),intent(out):: str
    character(len=25):: mess
    mess=' '
    write(mess,'(g25.15)') v%data%d(v%offset+n)
    str=adjustl(mess)
  end subroutine fmt_d

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

  function full_type(v) result(tno)
    type(pm_ptr):: v
    integer:: tno
    tno=pm_fast_typeof(v)
    if(tno>=pm_struct_type) then
       tno=v%data%ptr(v%offset+1_pm_p)%offset
    endif
  contains
    include 'ftypeof.inc'
  end function full_type

end module pm_array
