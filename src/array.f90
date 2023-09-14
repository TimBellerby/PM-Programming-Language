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

module pm_array
  use pm_kinds
  use pm_memory
  use pm_hash
  use pm_lib
  use pm_types
  implicit none

  ! Array type offsets
  integer(pm_p),parameter:: pm_array_typeof=1_pm_p
  integer(pm_p),parameter:: pm_array_vect=2_pm_p
  integer(pm_p),parameter:: pm_array_dom=3_pm_p
  integer(pm_p),parameter:: pm_array_length=4_pm_p
  integer(pm_p),parameter:: pm_array_offset=5_pm_p
  integer,parameter:: pm_array_size=6_pm_p

  ! Elemref type offsets
  integer(pm_p),parameter:: pm_elemref_typeof=1_pm_p
  integer(pm_p),parameter:: pm_elemref_vect=2_pm_p
  integer(pm_p),parameter:: pm_elemref_idx=3_pm_p
  integer(pm_p),parameter:: pm_elemref_offset=4_pm_p
  integer,parameter:: pm_elemref_size=5_pm_p

  ! Error codes
  integer,parameter:: vector_type_error=-1
  integer,parameter:: vector_size_error=-2
  integer,parameter:: vector_slice_error=-3
  integer,parameter:: vector_index_error=-4
  integer,parameter:: vector_shape_error=5

  ! Widths for formatted outputs
  integer,parameter:: fmt_i_width=10
  integer,parameter:: fmt_ln_width=20
  integer,parameter:: fmt_lln_width=25
  integer,parameter:: fmt_i32_width=10
  integer,parameter:: fmt_i64_width=20
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
    integer,intent(in):: typno
    type(pm_ptr),intent(in):: vec,dom,len,off
    type(pm_ptr):: ptr
    ptr=pm_fast_newnc(context,pm_usr,pm_array_size)
    ptr%data%ptr(ptr%offset)=pm_fast_typeno(context,int(pm_array_type))
    ptr%data%ptr(ptr%offset+pm_array_typeof)=&
         pm_fast_tinyint(context,typno)
    ptr%data%ptr(ptr%offset+pm_array_vect)=vec
    ptr%data%ptr(ptr%offset+pm_array_dom)=dom
    ptr%data%ptr(ptr%offset+pm_array_length)=len
    ptr%data%ptr(ptr%offset+pm_array_offset)=off
  contains
    include 'fnewnc.inc'
    include 'ftiny.inc'
    include 'ftypeno.inc'
  end function make_array

  ! LHS value for single array element <array[index]>
  function make_elem_ref(context,array,index,ve,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: array,index,ve
    integer,intent(inout):: errno
    type(pm_ptr):: ptr
    type(pm_ptr)::a
    type(pm_root),pointer:: root
    type(pm_ptr):: w,vec,newvec,oldoff,off,newoff,idx,newidx
    integer(pm_ln):: esize,i,j
    root=>pm_new_as_root(context,pm_usr,int(pm_elemref_size,pm_ln))
    ptr=root%ptr
    ptr%data%ptr(ptr%offset)=pm_fast_typeno(context,int(pm_elemref_type))
 
    esize=pm_fast_esize(index)
    newidx=pm_assign_new(context,ptr,int(pm_elemref_idx,pm_ln),&
         pm_long,esize+1_pm_ln,.false.)
    newoff=pm_assign_new(context,ptr,int(pm_elemref_offset,pm_ln),&
         pm_long,esize+1_pm_ln,.false.)
    if(pm_fast_typeof(array)==pm_elemref_type) then
       newvec=pm_assign_new(context,ptr,int(pm_elemref_vect,pm_ln),&
            pm_pointer,esize+1_pm_ln,.true.)
       vec=array%data%ptr(array%offset+pm_elemref_vect)
       w=vec%data%ptr(vec%offset)
       w=w%data%ptr(w%offset+pm_array_vect)
       ptr%data%ptr(ptr%offset+pm_elemref_typeof)=&
            pm_fast_tinyint(context,full_type(w%data%ptr(w%offset)))
       idx=array%data%ptr(array%offset+pm_elemref_idx)
       oldoff=array%data%ptr(array%offset+pm_elemref_offset)
       do i=0,esize
          w=vec%data%ptr(vec%offset+i)
          off=w%data%ptr(w%offset+pm_array_offset)
          w=w%data%ptr(w%offset+pm_array_vect)
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
            full_type(vec%data%ptr(vec%offset)))
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
    include 'ftypeno.inc'
    include 'fesize.inc'
  end function make_elem_ref

  ! Dereference an array element LHS reference <array[index]>
  function get_elem_ref(context,p,esize,errno) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: p
    integer(pm_ln),intent(in):: esize
    integer,intent(inout):: errno
    type(pm_ptr):: ptr
    type(pm_ptr):: v,idx,off
    type(pm_root),pointer:: root
    if(pm_fast_typeof(p)==pm_elemref_type) then
       v=p%data%ptr(p%offset+pm_elemref_vect)
       idx=p%data%ptr(p%offset+pm_elemref_idx)
       off=p%data%ptr(p%offset+pm_elemref_offset)
       root=>pm_add_root(context,empty_copy_vector(context,v%data%ptr(v%offset),&
            pm_fast_esize(idx)+1))
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

  ! Get element from reference to struct/rec <array[index]>.n
  function elem_ref_get_struct_elem(context,v,n,esize) result(w)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v
    integer,intent(in):: n
    integer(pm_ln),intent(in):: esize
    type(pm_ptr):: w
    type(pm_ptr),target:: newref,newvec
    type(pm_ptr):: vec,item
    type(pm_reg),pointer:: reg
    integer(pm_ln):: i
    reg=>pm_register(context,'refgetelem',newref,newvec)
    newvec=pm_new(context,pm_pointer,esize+1)
    vec=v%data%ptr(v%offset+pm_elemref_vect)
    do i=0,esize
       item=vec%data%ptr(vec%offset+i)
       newvec%data%ptr(newvec%offset+i)=item%data%ptr(item%offset+n)
    enddo
    newref=pm_fast_newnc(context,pm_usr,pm_elemref_size)
    newref%data%ptr(newref%offset)=pm_fast_typeno(context,int(pm_elemref_type))
    newref%data%ptr(newref%offset+pm_elemref_typeof)=pm_fast_tinyint(context,&
         full_type(item%data%ptr(item%offset+n)))
    newref%data%ptr(newref%offset+pm_elemref_vect)=newvec
    newref%data%ptr(newref%offset+pm_elemref_idx)=v%data%ptr(v%offset+pm_elemref_idx)
    newref%data%ptr(newref%offset+pm_elemref_offset)=v%data%ptr(v%offset+pm_elemref_offset)
    w=newref
    call pm_delete_register(context,reg)
  contains
    include 'ftiny.inc'
    include 'ftypeno.inc'
    include 'fnewnc.inc'
  end function elem_ref_get_struct_elem

  ! Create a new aray {a1.elem, a2.elem,...} from {a1, a2, ...}
  ! ONLY WORKS from array created with make_array_from_vect
  function array_get_struct_elem(context,ptr,n,tno) result(newarray)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n,tno
    type(pm_ptr):: newarray
    type(pm_ptr):: oldvec,newvec,vec
    type(pm_root),pointer:: root
    oldvec=ptr%data%ptr(ptr%offset+pm_array_vect)
    vec=oldvec%data%ptr(oldvec%offset)
    vec=vec%data%ptr(vec%offset+n)
    root=>pm_new_as_root(context,pm_pointer,pm_fast_esize(oldvec)+1)
    newvec=root%ptr
    newvec%data%ptr(newvec%offset:newvec%offset+pm_fast_esize(newvec))=vec
    newarray=make_array(context,pm_array_type,&
         tno,&  
         newvec,&
         ptr%data%ptr(ptr%offset+pm_array_dom),&
         ptr%data%ptr(ptr%offset+pm_array_length),&
         ptr%data%ptr(ptr%offset+pm_array_offset))
    call pm_delete_root(context,root)
  contains
    include "fesize.inc"
  end function array_get_struct_elem

  ! Return domain of an array / reference to array
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
       if(pm_fast_typeof(v)/=pm_elemref_type) then
          call pm_dump_tree(context,6,v,2)
          write(*,*) '#',pm_fast_typeof(v),pm_const_array_type
          call pm_panic('array_dom')
       endif
       reg=>pm_register(context,'adom',p,q)
       w=v%data%ptr(v%offset+pm_elemref_vect)
       idx=v%data%ptr(v%offset+pm_elemref_idx)
       off=v%data%ptr(v%offset+pm_elemref_offset)
       x=w%data%ptr(w%offset)
       p=empty_copy_vector(context,x%data%ptr(x%offset+pm_array_dom),esize+1)
       q=pm_new(context,pm_pointer,esize+1)
       do i=0,esize
          x=w%data%ptr(w%offset+i)
          q%data%ptr(q%offset+i)=x%data%ptr(x%offset+pm_array_dom)
       enddo
       call array_vect_index(context,p,q,idx,off,esize)
       ptr=p
       call pm_delete_register(context,reg)
    endif
  contains
    include 'ftypeof.inc'
  end function array_dom

  ! Return size of an array
  function array_size(context,v,esize) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: esize
    type(pm_ptr):: ptr
    type(pm_ptr),target:: p,q
    type(pm_ptr):: w,x,idx,off
    integer(pm_ln):: i
    type(pm_reg),pointer:: reg
    if(pm_fast_typeof(v)==pm_array_type) then
       ptr=v%data%ptr(v%offset+pm_array_length)
    else
       if(pm_fast_typeof(v)/=pm_elemref_type) then
          write(*,*) '#',pm_fast_typeof(v),pm_const_array_type
          call pm_panic('array_dom')
       endif

       reg=>pm_register(context,'asize',p,q)
       w=v%data%ptr(v%offset+pm_elemref_vect)
       idx=v%data%ptr(v%offset+pm_elemref_idx)
       off=v%data%ptr(v%offset+pm_elemref_offset)
       x=w%data%ptr(w%offset)
       p=empty_copy_vector(context,x%data%ptr(x%offset+pm_array_size),esize+1)
       q=pm_new(context,pm_pointer,esize+1)
       do i=0,esize
          x=w%data%ptr(w%offset+i)
          q%data%ptr(q%offset+i)=x%data%ptr(x%offset+pm_array_size)
       enddo
       call array_vect_index(context,p,q,idx,off,esize)
       call pm_delete_register(context,reg)
    endif
  contains
    include 'ftypeof.inc'
  end function array_size

  ! Make an array with j elements having intial value val
  ! domain dom
  ! (vector inputs yield vector of arrays)
  function make_array_dim(context,typno,val,dom,j,ve) result(ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: typno
    type(pm_ptr),intent(in):: val,dom,j,ve
    type(pm_ptr):: ptr
    type(pm_ptr),target:: len,off,vec,vecs,domv
    type(pm_reg),pointer:: reg
    integer(pm_ln):: i,k,n,esize
    reg=>pm_register(context,'arrdim',vec,vecs,len,off)
    esize=pm_fast_esize(j)
    len=vector_zero_unused(context,j,ve)
    n=sum(len%data%ln(len%offset:len%offset+esize))
    vecs=make_vector(context,val,len,0_pm_ln,0_pm_ln,n,.true.)
    vec=pm_new(context,pm_pointer,esize+1_pm_ln)
    vec%data%ptr(vec%offset:vec%offset+esize)=vecs
    off=pm_new(context,pm_long,esize+1_pm_ln)
    k=0
    do i=0,esize
       off%data%ln(off%offset+i)=k
       k=k+len%data%ln(len%offset+i)
    enddo
    domv=copy_vector(context,dom,ve,0_pm_ln,-1_pm_ln)
    ptr=make_array(context,pm_const_array_type,typno,vec,domv,len,off)
    call pm_delete_register(context,reg)
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
  end function make_array_dim
  
  ! Make an array with j elements having intial value val
  ! domain dom
  ! (vector inputs yield vector of arrays)
  function make_array_vdim(context,typno,val,dom,j,ve) result(ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: typno
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
       call pm_ptr_assign(context,vec,i,vector_from_scalar(context,val,i,&
            max(0_pm_ln,len%data%ln(len%offset+i)-1_pm_ln),.false.))
    enddo
    off=pm_new(context,pm_long,esize+1_pm_ln)
    off%data%ln(off%offset:off%offset+esize)=0_pm_ln
    domv=copy_vector(context,dom,ve,0_pm_ln,-1_pm_ln)
    ptr=make_array(context,pm_array_type,typno,vec,domv,len,off)
    call pm_delete_register(context,reg)
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
  end function make_array_vdim

  ! Create array with same elements (by ref) but new domain
  function array_redim(context,tno,array,dom) result(ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
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
  function make_array_from_vect(context,typno,vec,dom,esize,import_vec) result(ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: typno
    type(pm_ptr),intent(in):: vec,dom,import_vec
    integer(pm_ln),intent(in):: esize
    type(pm_ptr):: ptr
    type(pm_ptr),target:: len,off,vect,domv
    type(pm_reg),pointer:: reg
    integer(pm_ln):: i,j,k,s,o
    reg=>pm_register(context,'arrdim',vect,len,off,domv)
    vect=pm_new(context,pm_pointer,esize+1_pm_ln)
    vect%data%ptr(vect%offset:vect%offset+esize)=vec
    len=pm_new(context,pm_long,esize+1_pm_ln)
    off=pm_new(context,pm_long,esize+1_pm_ln)
    k=0
    o=0
    do i=4,pm_fast_esize(import_vec)
       s=import_vec%data%ln(import_vec%offset+i)
       do j=1,s
          len%data%ln(len%offset+k)=s
          off%data%ln(off%offset+k)=o
          k=k+1
       enddo
       o=o+s
    enddo
    if(k/=esize+1) call pm_panic('make_array_from_vect')
    domv=copy_vector(context,dom,pm_null_obj,0_pm_ln,-1_pm_ln)
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
    integer,intent(inout):: errno
    type(pm_ptr):: ptr,v,w,ix
    type(pm_ptr):: vec,off,len
    type(pm_ptr),target:: idx,p,newvec,newoff
    integer(pm_ln):: j
    type(pm_reg),pointer:: reg
    integer(pm_ln)::i
    if(pm_fast_typeof(array)==pm_array_type) then
       vec=array%data%ptr(array%offset+pm_array_vect)
       off=array%data%ptr(array%offset+pm_array_offset)
       len=array%data%ptr(array%offset+pm_array_length)
       reg=>pm_register(context,'aindx',idx,p)
       p=empty_copy_vector(context,vec%data%ptr(vec%offset),&
            esize+1_pm_ln)
       ptr=p
       idx=vector_zero_unused(context,index,ve)
       if(any(idx%data%ln(idx%offset:idx%offset+esize)<0.or.&
            idx%data%ln(idx%offset:idx%offset+esize)>&
            len%data%ln(len%offset:len%offset+esize))) then
          ptr=len
          errno=vector_index_error
          return
       endif
       call array_vect_index(context,p,vec,idx,off,esize)
       if(p%data%vkind/=vec%data%ptr(vec%offset+esize)%data%vkind) then
          errno=vector_type_error
       endif
       call pm_delete_register(context,reg)
    else
       reg=>pm_register(context,'arindx',idx,newvec,newoff)
       vec=array%data%ptr(array%offset+pm_elemref_vect)
       off=array%data%ptr(array%offset+pm_elemref_offset)
       ix=array%data%ptr(array%offset+pm_elemref_idx)
       idx=vector_zero_unused(context,index,ve,-1_pm_ln)
       newvec=pm_new(context,pm_pointer,esize+1)
       newoff=pm_new(context,pm_long,esize+1)
       w=vec%data%ptr(vec%offset)
       w=w%data%ptr(w%offset+pm_array_vect)
       p=empty_copy_vector(context,w%data%ptr(w%offset),&
            esize+1_pm_ln)
       do i=0,esize
          w=vec%data%ptr(vec%offset+i)
          j=ix%data%ln(ix%offset+i)+off%data%ln(off%offset+i)
          v=w%data%ptr(w%offset+pm_array_vect)
          newvec%data%ptr(newvec%offset+i)=v%data%ptr(v%offset+j)
          v=w%data%ptr(w%offset+pm_array_offset)
          newoff%data%ln(newoff%offset+i)=v%data%ln(v%offset+j)
          v=w%data%ptr(w%offset+pm_array_length)
          if(v%data%ln(v%offset+j)<idx%data%ln(idx%offset+i).or.&
               idx%data%ln(idx%offset+i)<0) then
             errno=vector_index_error
             return
          endif
       enddo
       call array_vect_index(context,p,newvec,idx,newoff,esize)
       ptr=p
       call pm_delete_register(context,reg)
    endif
  contains
    include 'ftypeof.inc'
    include 'fisnull.inc'
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
    case(pm_longlong)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)    
             p%data%lln(p%offset+i)=w%data%lln(w%offset+ix)
          endif
       enddo
    case(pm_int8)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)    
             p%data%i8(p%offset+i)=w%data%i8(w%offset+ix)
          endif
       enddo
    case(pm_int16)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)    
             p%data%i16(p%offset+i)=w%data%i16(w%offset+ix)
          endif
       enddo
    case(pm_int32)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)    
             p%data%i32(p%offset+i)=w%data%i32(w%offset+ix)
          endif
       enddo
    case(pm_int64)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)    
             p%data%i64(p%offset+i)=w%data%i64(w%offset+ix)
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
    case(pm_single_complex)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)
             p%data%c(p%offset+i)=w%data%c(w%offset+ix)
          endif
       enddo
    case(pm_double_complex)
       do i=0,esize
          w=v%data%ptr(v%offset+i)
          if(.not.pm_fast_isnull(w)) then
             ix=idx%data%ln(idx%offset+i)+offset%data%ln(offset%offset+i)
             p%data%dc(p%offset+i)=w%data%dc(w%offset+ix)
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
    integer,intent(inout):: errno
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
    integer,intent(inout):: errno
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
    case(pm_longlong)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%lln(w%offset+ix)=p%data%lln(p%offset+i)
          endif
       enddo
    case(pm_int8)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%i8(w%offset+ix)=p%data%i8(p%offset+i)
          endif
       enddo
    case(pm_int16)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%i16(w%offset+ix)=p%data%i16(p%offset+i)
          endif
       enddo
    case(pm_int32)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%i32(w%offset+ix)=p%data%i32(p%offset+i)
          endif
       enddo
    case(pm_int64)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%i64(w%offset+ix)=p%data%i64(p%offset+i)
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
    case(pm_single_complex)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%c(w%offset+ix)=p%data%c(p%offset+i)
          endif
       enddo
    case(pm_double_complex)
       do i=0,esize
          ix=idx%data%ln(idx%offset+i)
          if(ix>=0) then
             w=v%data%ptr(v%offset+i)
             ix=ix+offset%data%ln(offset%offset+i)
             w%data%dc(w%offset+ix)=p%data%dc(p%offset+i)
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
    integer:: t
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
            int(v%data%ptr(v%offset+pm_array_typeof)%offset),&
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
    idx=pm_fast_newnc(context,pm_long,1)
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
    integer:: tno
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
    integer,intent(in):: tno
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
  ! import_vec(0)  : esize of imported vector
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
    if(siz==1.and.pm_fast_esize(import_vec)==4) then
       ptr=v
    else
       start=import_vec%data%ln(import_vec%offset+1)
       ptr=make_vector(context,v,import_vec,start,4_pm_p,siz)
    endif
  contains
    include 'fesize.inc'
  end function import_vector
    
  ! Re-export elements of imported vector e back to v
  recursive subroutine export_vector(context,v,e,import_vec)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,e,import_vec
    integer:: tno
    integer(pm_ln):: i,j,n
    integer:: k,errno
    tno=pm_fast_typeof(v)
    if(pm_fast_typeof(e)/=tno) then
       write(*,*) trim(pm_typ_as_string(context,tno)),'<>',&
            trim(pm_typ_as_string(context,pm_fast_typeof(e)))
       call pm_panic('export_vector')
    endif
    n=pm_fast_esize(v)
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       continue
    case(pm_struct_type,pm_rec_type)
       do k=2,n
          call export_vector(context,&
               v%data%ptr(v%offset+k),&
               e%data%ptr(e%offset+k),&
               import_vec)
       enddo
    case(pm_pointer)
       j=0
       do i=0,n
          call assign_single(context,v%data%ptr(v%offset+i),0_pm_ln,&
               e%data%ptr(e%offset+j),errno)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_int)
       j=0
       do i=0,n
          v%data%i(v%offset+i)=&
               e%data%i(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_long)
       j=0
       do i=0,n
          v%data%ln(v%offset+i)=&
               e%data%ln(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_int8)
       j=0
       do i=0,n
          v%data%i8(v%offset+i)=&
               e%data%i8(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_int16)
       j=0
       do i=0,n
          v%data%i16(v%offset+i)= &
               e%data%i16(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_int32)
       j=0
       do i=0,n
          v%data%i32(v%offset+i)= &
               e%data%i32(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_int64)
       j=0
       do i=0,n
          v%data%i64(v%offset+i)= &
               e%data%i64(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_int128)
       j=0
       do i=0,n
          v%data%i128(v%offset+i)= &
               e%data%i128(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_single)
       j=0
       do i=0,n
          v%data%r(v%offset+i)=  & 
               e%data%r(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_double)
       j=0
       do i=0,n
          v%data%d(v%offset+i)= &
               e%data%d(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_real32)
       j=0
       do i=0,n
          v%data%r32(v%offset+i)= &
               e%data%r32(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_real64)
       j=0
       do i=0,n
          v%data%r64(v%offset+i)= &
               e%data%r64(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_real128)
       j=0
       do i=0,n
          v%data%r128(v%offset+i)= &
               e%data%r128(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_single_complex)
       j=0
       do i=0,n
          v%data%c(v%offset+i)= &
               e%data%c(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_double_complex)
       j=0
       do i=0,n
          v%data%dc(v%offset+i) = &
               e%data%dc(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_complex64)
       j=0
       do i=0,n
          v%data%c64(v%offset+i)= &
               e%data%c64(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_complex128)
       j=0
       do i=0,n
          v%data%c128(v%offset+i)= &
               e%data%c128(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_complex256)
       do i=0,n
          v%data%c256(v%offset+i)= &
               e%data%c256(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_logical)
       j=0
       do i=0,n
          v%data%l(v%offset+i)= &
               e%data%l(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_packed_logical)
       j=0
       do i=0,n
          v%data%pl(v%offset+i)= &
               e%data%pl(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo
    case(pm_string)
       j=0
       do i=0,n
          v%data%s(v%offset+i)= &
               e%data%s(e%offset+j)
          j=j+import_vec%data%ln(import_vec%offset+4+i)
       enddo  
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
  end subroutine export_vector
  
  function export_vector_as_new(context,v,import_vec,ve) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,import_vec,ve
    integer:: errno
    type(pm_ptr):: ptr
    type(pm_ptr):: index
    integer(pm_ln):: i,j,k,next
    integer:: vk
    type(pm_root),pointer:: root
    root=>pm_new_as_root(context,pm_long,pm_fast_esize(import_vec)-3)
    index=root%ptr
    k=0
    j=0
    if(pm_fast_vkind(ve)==pm_null.or.pm_fast_vkind(ve)==pm_tiny_int) then
       do i=4,pm_fast_esize(import_vec)
          index%data%ln(index%offset+i-4)=j
          j=j+import_vec%data%ln(import_vec%offset+i)
       enddo
    elseif(pm_fast_vkind(ve)==pm_logical) then
       do i=4,pm_fast_esize(import_vec)
          next=j+import_vec%data%ln(import_vec%offset+i)
          index%data%ln(index%offset+i-4)=0
          do k=j,next-1
             if(ve%data%l(ve%offset+k)) then
                index%data%ln(index%offset+i-4)=k
                exit
             endif
          enddo
          j=next
       enddo
    else
       do i=4,pm_fast_esize(import_vec)
          do while(ve%data%ln(ve%offset+k)<j)
             k=k+1
          enddo
          if(k<=pm_fast_esize(ve)) index%data%ln(index%offset+i-4)=ve%data%ln(ve%offset+k)
          j=j+import_vec%data%ln(import_vec%offset+i)
       enddo
    endif
    vk=pm_fast_typeof(v)
    if(vk==pm_dref_type.or.vk==pm_dref_shared_type) then
       ptr=v
    else
       ptr=vector_get_elems(context,v,index,errno,keeparrays=.true.)
    endif
    call pm_delete_root(context,root)
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
    include 'ftypeof.inc'
  end function export_vector_as_new

  ! For any group (defined by import_vec) if any index has a true
  ! in v then set all that group true in w (else false)
  function vector_if_needed(context,v,import_vec) result(w)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,import_vec
    type(pm_ptr):: w
    integer(pm_ln):: i,j,k,n
    logical:: ok
    integer:: vk
    vk=pm_fast_vkind(v)
    if(vk==pm_tiny_int.or.vk==pm_null) then
       w=v
    elseif(vk==pm_logical) then
       w=pm_new(context,pm_logical,pm_fast_esize(v)+1)
       j=0
       do i=4,pm_fast_esize(import_vec)
          n=import_vec%data%ln(import_vec%offset+i)
          ok=any(v%data%l(v%offset+j:v%offset+j+n-1))
          w%data%l(w%offset+j:w%offset+j+n-1)=ok
          j=j+n
       enddo
    else
       w=pm_new(context,pm_logical,pm_fast_esize(v)+1)
       j=0
       k=0
       do i=4,pm_fast_esize(import_vec)
          n=import_vec%data%ln(import_vec%offset+i)
          if(v%data%ln(v%offset+k)<j+n) then
             w%data%l(w%offset+j:w%offset+j+n-1)=.true.
             do while(v%data%ln(v%offset+k)<j+n)
                k=k+1
             enddo
          else
             w%data%l(w%offset+j:w%offset+j+n-1)=.false.
          endif
          j=j+n
       enddo
    endif
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end function vector_if_needed

  ! For any group (defined by import_vec) if any index has a true
  ! in v then next element true in w (else false)
  function vector_export_if_needed(context,v,import_vec) result(w)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,import_vec
    type(pm_ptr):: w
    integer(pm_ln):: i,j,k,m,n
    logical:: ok
    integer:: vk
    vk=pm_fast_vkind(v)
    if(vk==pm_tiny_int) then
       w=v
    else
       w=pm_new(context,pm_logical,pm_fast_esize(import_vec)-3)
       if(vk==pm_null) then
          k=0
          do i=4,pm_fast_esize(import_vec)
             n=import_vec%data%ln(import_vec%offset+i)
             w%data%l(w%offset+k)=n>0
             k=k+1
          enddo
       elseif(vk==pm_logical) then
          j=0
          k=0
          do i=4,pm_fast_esize(import_vec)
             n=import_vec%data%ln(import_vec%offset+i)
             if(n>0) then
                ok=any(v%data%l(v%offset+j:v%offset+j+n-1))
                w%data%l(w%offset+k)=ok
             else
                w%data%l(w%offset+k)=.false.
             endif
             k=k+1
             j=j+n
          enddo
       else
          j=0
          k=0
          m=0
          do i=4,pm_fast_esize(import_vec)
             n=import_vec%data%ln(import_vec%offset+i)
             if(n>0) then
                if(v%data%ln(v%offset+k)<j+n) then
                   w%data%l(w%offset+m)=.true.
                   do while(v%data%ln(v%offset+k)<j+n&
                        .and.k<=pm_fast_esize(v))
                      k=k+1
                   enddo
                else
                   w%data%l(w%offset+m)=.false.
                endif
                j=j+n
             else
                w%data%l(w%offset+m)=.false.
             endif
             m=m+1
          enddo
       endif
    endif
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end function vector_export_if_needed
  
  ! Build a vector by replicating a scalar value
  recursive function make_vector(context,v,j,dispv,dispj,vsize,full_copy) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,j
    integer(pm_p),intent(in):: dispj
    integer(pm_ln),intent(in):: dispv,vsize
    logical,intent(in),optional:: full_copy
    type(pm_ptr):: ptr
    integer:: tno
    type(pm_root),pointer:: root
    integer(pm_p):: i
    integer(pm_ln):: s,k,m,n,disp,siz
    type(pm_ptr):: p,q,len,off
    siz=max(vsize,1_pm_ln)
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_tiny_int:pm_null)
       ptr=v
    case(pm_array_type,pm_const_array_type)
       root=>pm_new_as_root(context,pm_usr,int(pm_array_size,pm_ln))
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+pm_array_typeof)=&
            v%data%ptr(v%offset+pm_array_typeof)
       call pm_ptr_assign(context,ptr,int(pm_array_dom,pm_ln),&
            make_vector(context,&
            v%data%ptr(v%offset+pm_array_dom),&
            j,dispv,dispj,siz,full_copy))
       call pm_ptr_assign(context,ptr,int(pm_array_vect,pm_ln),&
            make_vector(context,&
            v%data%ptr(v%offset+pm_array_vect),&
            j,dispv,dispj,siz,full_copy))
       call pm_ptr_assign(context,ptr,int(pm_array_length,pm_ln),&
            make_vector(context,&
            v%data%ptr(v%offset+pm_array_length),&
            j,dispv,dispj,siz,full_copy))
       call pm_ptr_assign(context,ptr,int(pm_array_offset,pm_ln),&
            make_vector(context,&
            v%data%ptr(v%offset+pm_array_offset),&
            j,dispv,dispj,siz,full_copy))
       call pm_delete_root(context,root)
    case(pm_elemref_type)
       root=>pm_new_as_root(context,pm_usr,int(pm_elemref_size,pm_ln))
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+pm_elemref_typeof)=&
            v%data%ptr(v%offset+pm_elemref_typeof)
       call pm_ptr_assign(context,ptr,int(pm_elemref_vect,pm_ln),&
            make_vector(context,&
            v%data%ptr(v%offset+pm_elemref_vect),&
            j,dispv,dispj,siz))
       call pm_ptr_assign(context,ptr,int(pm_elemref_idx,pm_ln),&
            make_vector(context,&
            v%data%ptr(v%offset+pm_elemref_idx),&
            j,dispv,dispj,siz,full_copy))
       call pm_ptr_assign(context,ptr,int(pm_elemref_offset,pm_ln),&
            make_vector(context,&
            v%data%ptr(v%offset+pm_elemref_offset),&
            j,dispv,dispj,siz,full_copy))
       call pm_delete_root(context,root)
    case(pm_struct_type,pm_rec_type)
       root=>pm_new_as_root(context,pm_usr,pm_fast_esize(v)+1_pm_ln)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+1_pm_p)=v%data%ptr(v%offset+1_pm_p)
       do i=2,pm_fast_esize(v)
          call pm_ptr_assign(context,ptr,int(i,pm_ln),&
               make_vector(context,v%data%ptr(v%offset+i),&
               j,dispv,dispj,siz,full_copy))
       enddo
       call pm_delete_root(context,root)
    case(pm_pointer)
       if(present(full_copy)) then
          root=>pm_new_as_root(context,pm_pointer,siz)
          ptr=root%ptr
          n=0
          do i=0,pm_fast_esize(j)-dispj
             do k=1,j%data%ln(j%offset+i+dispj)
                call pm_ptr_assign(context,ptr,n,&
                     copy_vector(context,v%data%ptr(v%offset+i+dispv),&
                     pm_null_obj,0_pm_ln,-1_pm_ln))
                n=n+1
             enddo
          enddo
          call pm_delete_root(context,root)
       else
          ptr=pm_new(context,pm_pointer,siz)
          n=0
          do i=0,pm_fast_esize(j)-dispj
             do k=1,j%data%ln(j%offset+i+dispj)
                ptr%data%ptr(ptr%offset+n)=v%data%ptr(v%offset+i+dispv)
                n=n+1
             enddo
          enddo
       endif
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
    case(pm_longlong)
       ptr=pm_new(context,pm_longlong,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%lln(ptr%offset+n)=v%data%lln(v%offset+i+dispv)
             n=n+1
          enddo
       enddo
    case(pm_int8)
       ptr=pm_new(context,pm_int8,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%i8(ptr%offset+n)=v%data%i8(v%offset+i+dispv)
             n=n+1
          enddo
       enddo
    case(pm_int16)
       ptr=pm_new(context,pm_int16,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%i16(ptr%offset+n)=v%data%i16(v%offset+i+dispv)
             n=n+1
          enddo
       enddo
    case(pm_int32)
       ptr=pm_new(context,pm_int32,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%i32(ptr%offset+n)=v%data%i32(v%offset+i+dispv)
             n=n+1
          enddo
       enddo
    case(pm_int64)
       ptr=pm_new(context,pm_int64,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%i64(ptr%offset+n)=v%data%i64(v%offset+i+dispv)
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
    case(pm_single_complex)
       ptr=pm_new(context,pm_single_complex,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%c(ptr%offset+n)=v%data%c(v%offset+i+dispv)
             n=n+1
          enddo
       enddo
    case(pm_double_complex)
       ptr=pm_new(context,pm_double_complex,siz)
       n=0
       do i=0,pm_fast_esize(j)-dispj
          do k=1,j%data%ln(j%offset+i+dispj)
             ptr%data%dc(ptr%offset+n)=v%data%dc(v%offset+i+dispv)
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
    case default
       write(*,*) '%%',trim(pm_typ_as_string(context,tno)),tno,pm_dref_type
       call pm_panic('Make vector')
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
               int(v%data%ptr(v%offset+pm_array_typeof)%offset),&
               vec,dom,len,off)
          call pm_delete_register(context,reg)
       elseif(typ==pm_dref_type.or.typ==pm_dref_shared_type) then
          ptr=v
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
  
  recursive function copy_dref(context,v,size,same_proc,ve) result(w)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: size
    logical,intent(in):: same_proc
    type(pm_ptr),intent(in),optional:: ve
    type(pm_ptr):: w
    integer:: tno
    type(pm_root),pointer:: root
    tno=pm_fast_typeof(v)
    select case(tno)
    case(pm_dref_type)
       root=>pm_new_as_root(context,pm_usr,4_pm_ln)
       w=root%ptr
       w%data%ptr(w%offset)%offset=pm_dref_type
       w%data%ptr(w%offset+2)=copy_dref(context,v%data%ptr(v%offset+2),size,same_proc,ve)
       if(same_proc) then
          w%data%ptr(w%offset+3)=v%data%ptr(v%offset+3)
       else
          w%data%ptr(w%offset+3)=pm_null_obj
       endif
       call pm_delete_root(context,root)
    case(pm_dref_shared_type)
       root=>pm_new_as_root(context,pm_usr,4_pm_ln)
       w=root%ptr
       w%data%ptr(w%offset)%offset=pm_dref_shared_type
       w%data%ptr(w%offset+2)=copy_dref(context,v%data%ptr(v%offset+2),size,same_proc,ve)
       if(same_proc) then
          if(present(ve)) then
             w%data%ptr(w%offset+3)=&
                  import_vector(context,v%data%ptr(v%offset+3),ve%data%ptr(ve%offset+1))
          else
             w%data%ptr(w%offset+3)=&
                  vector_from_scalar(context,v%data%ptr(v%offset+3),0_pm_ln,size-1,.true.)
          endif
       else
          w%data%ptr(w%offset+3)=pm_null_obj
       endif
       call pm_delete_root(context,root)
    case default
       if(present(ve)) then
          w=import_vector(context,v,ve%data%ptr(ve%offset+1))
       else
          w=vector_from_scalar(context,v,0_pm_ln,size-1,.true.)
       endif
    end select
  contains
    include 'ftypeof.inc'
  end function copy_dref
  
  subroutine array_assign(context,lhs,ix,rhs,iy,errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: lhs,rhs
    integer(pm_ln),intent(in):: ix,iy
    integer,intent(inout):: errno
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
       if(pm_fast_typeof(lhs)==pm_const_array_type) then
          errno=vector_size_error
       elseif(size2==0) then
          v=vec1%data%ptr(vec1%offset+ix)
          call pm_ptr_assign(context,vec1,ix,&
               empty_copy_vector(context,&
               v,1_pm_ln))
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
    include 'ftypeof.inc'
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
    type(pm_ptr),target:: vec,dom,off,len,elem
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
          reg=>pm_register(context,'vempty',vec,dom,len,off,elem)
          dom=empty_copy_vector(&
               context,v%data%ptr(v%offset+pm_array_dom),size)
          w=v%data%ptr(v%offset+pm_array_vect)
          elem=empty_copy_vector(context,w%data%ptr(w%offset),1_pm_ln)
          vec=pm_new(context,pm_pointer,size)
          vec%data%ptr(vec%offset:vec%offset+size-1)=elem
          len=pm_new(context,pm_long,size)
          len%data%ln(len%offset:len%offset+size-1)=0
          off=pm_new(context,pm_long,size)
          off%data%ln(off%offset:off%offset+size-1)=0
          ptr=make_array(context,pm_array_type,&
               int(v%data%ptr(v%offset+pm_array_typeof)%offset),&
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
    if(vk<=pm_null) then
       ptr=v
    elseif(vk<=pm_pointer) then
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
               int(v%data%ptr(v%offset+pm_array_typeof)%offset),&
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
    integer,intent(inout):: errno
    integer(pm_p):: i
    integer(pm_i16):: tno,tno2
    integer(pm_ln):: j,k
    type(pm_ptr):: vec1,vec2,off1,off2,len1,len2,idx
    type(pm_root),pointer:: root

    if(pm_fast_isnull(lhs)) return
    tno=pm_fast_typeof(lhs)
!!$    if(full_type(lhs)/=full_type(rhs)) then
!!$       errno=vector_type_error
!!$       write(*,*) 'Full types',pm_typ_as_string(context,full_type(lhs)),&
!!$            pm_typ_as_string(context,full_type(rhs))
!!$       !call pm_dump_tree(context,6,lhs,2)
!!$       !call pm_dump_tree(context,6,rhs,2)
!!$       return
!!$    endif
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       if(tno==pm_array_type) then
          call vector_assign(context,&
               lhs%data%ptr(lhs%offset+pm_array_dom),&
               rhs%data%ptr(rhs%offset+pm_array_dom),&
               ve,errno,esize)
       endif
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
          do j=0,pm_fast_esize(ve)
             lhs%data%i(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%i(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
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
          do j=0,pm_fast_esize(ve)
             lhs%data%ln(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%ln(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(pm_longlong)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%lln(lhs%offset:lhs%offset+esize)=&
               rhs%data%lln(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%lln(lhs%offset:lhs%offset+esize)=&
                  rhs%data%lln(rhs%offset:rhs%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             lhs%data%lln(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%lln(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(pm_int8)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%i8(lhs%offset:lhs%offset+esize)=&
               rhs%data%i8(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%i8(lhs%offset:lhs%offset+esize)=&
                  rhs%data%i8(rhs%offset:rhs%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             lhs%data%i8(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%i8(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(pm_int16)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%i16(lhs%offset:lhs%offset+esize)=&
               rhs%data%i16(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%i16(lhs%offset:lhs%offset+esize)=&
                  rhs%data%i16(rhs%offset:rhs%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             lhs%data%i16(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%i16(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(pm_int32)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%i32(lhs%offset:lhs%offset+esize)=&
               rhs%data%i32(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%i32(lhs%offset:lhs%offset+esize)=&
                  rhs%data%i32(rhs%offset:rhs%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             lhs%data%i32(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%i32(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(pm_int64)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%i64(lhs%offset:lhs%offset+esize)=&
               rhs%data%i64(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%i64(lhs%offset:lhs%offset+esize)=&
                  rhs%data%i64(rhs%offset:rhs%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             lhs%data%i64(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%i64(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
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
          do j=0,pm_fast_esize(ve)
             lhs%data%r(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%r(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
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
          do j=0,pm_fast_esize(ve)
             lhs%data%d(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%d(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(pm_single_complex)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%c(lhs%offset:lhs%offset+esize)=&
               rhs%data%c(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%c(lhs%offset:lhs%offset+esize)=&
                  rhs%data%c(rhs%offset:rhs%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             lhs%data%c(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%c(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
       endif
    case(pm_double_complex)
       if(pm_fast_vkind(ve)==pm_null) then
          lhs%data%dc(lhs%offset:lhs%offset+esize)=&
               rhs%data%dc(rhs%offset:rhs%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             lhs%data%dc(lhs%offset:lhs%offset+esize)=&
                  rhs%data%dc(rhs%offset:rhs%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             lhs%data%dc(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%dc(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
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
          do j=0,pm_fast_esize(ve)
             lhs%data%l(lhs%offset+ve%data%ln(ve%offset+j))=&
                  rhs%data%l(rhs%offset+ve%data%ln(ve%offset+j))
          enddo
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
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%i(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%i(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%ln(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%ln(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          enddo
       endif
    case(pm_longlong)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.&
               v1%data%lln(v1%offset:v1%offset+esize)==&
               v2%data%lln(v2%offset:v2%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.&
                  v1%data%lln(v1%offset:v1%offset+esize)==&
                  v2%data%lln(v2%offset:v2%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%lln(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%lln(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          enddo
       endif
    case(pm_int8)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.&
               v1%data%i8(v1%offset:v1%offset+esize)==&
               v2%data%i8(v2%offset:v2%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.&
                  v1%data%i8(v1%offset:v1%offset+esize)==&
                  v2%data%i8(v2%offset:v2%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%i8(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%i8(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          enddo
       endif
    case(pm_int16)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.&
               v1%data%i16(v1%offset:v1%offset+esize)==&
               v2%data%i16(v2%offset:v2%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.&
                  v1%data%i16(v1%offset:v1%offset+esize)==&
                  v2%data%i16(v2%offset:v2%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%i16(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%i16(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          enddo
       endif
    case(pm_int32)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.&
               v1%data%i32(v1%offset:v1%offset+esize)==&
               v2%data%i32(v2%offset:v2%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.&
                  v1%data%i32(v1%offset:v1%offset+esize)==&
                  v2%data%i32(v2%offset:v2%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%i32(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%i32(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          enddo
       endif
    case(pm_int64)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.&
               v1%data%i64(v1%offset:v1%offset+esize)==&
               v2%data%i64(v2%offset:v2%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.&
                  v1%data%i64(v1%offset:v1%offset+esize)==&
                  v2%data%i64(v2%offset:v2%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%i64(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%i64(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%r(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%r(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%d(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%d(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          enddo
       endif
    case(pm_single_complex)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.&
               v1%data%c(v1%offset:v1%offset+esize)==&
               v2%data%c(v2%offset:v2%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.&
                  v1%data%c(v1%offset:v1%offset+esize)==&
                  v2%data%c(v2%offset:v2%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%c(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%c(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          enddo
       endif
    case(pm_double_complex)
       if(pm_fast_vkind(ve)==pm_null) then
          eq%data%l(eq%offset:eq%offset+esize)=&
               eq%data%l(eq%offset:eq%offset+esize).and.&
               v1%data%dc(v1%offset:v1%offset+esize)==&
               v2%data%dc(v2%offset:v2%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             eq%data%l(eq%offset:eq%offset+esize)=&
                  eq%data%l(eq%offset:eq%offset+esize).and.&
                  v1%data%dc(v1%offset:v1%offset+esize)==&
                  v2%data%dc(v2%offset:v2%offset+esize)
          end where
       else
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.&
                  v1%data%dc(ve%data%ln(ve%offset+j)+&
                  v1%offset)==&
                  v2%data%dc(ve%data%ln(ve%offset+j)+&
                  v2%offset)
          enddo
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
          do j=0,pm_fast_esize(ve)
             eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset)=&
                  eq%data%l(ve%data%ln(ve%offset+j)+&
                  eq%offset).and.(&
                  v1%data%l(ve%data%ln(ve%offset+j)+&
                  v1%offset).eqv.&
                  v2%data%l(ve%data%ln(ve%offset+j)+&
                  v2%offset))
          enddo
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
    case(pm_longlong)
       ok=all(v1%data%lln(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%lln(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_int8)
       ok=all(v1%data%i8(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%i8(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_int16)
       ok=all(v1%data%i16(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%i16(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_int32)
       ok=all(v1%data%i32(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%i32(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_int64)
       ok=all(v1%data%i64(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%i64(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_single)
       ok=all(v1%data%r(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%r(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_double)
       ok=all(v1%data%d(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%d(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_single_complex)
       ok=all(v1%data%c(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%c(v2%offset+istart2:v2%offset+istart2+esize))
    case(pm_double_complex)
       ok=all(v1%data%dc(v1%offset+istart1:v1%offset+istart1+esize)==&
            v2%data%dc(v2%offset+istart2:v2%offset+istart2+esize))
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
  ! and truncated to start first elements in
  function vector_iota_trunc(context,&
       elsize,start,end,step,first,siz,import_vec) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: import_vec,elsize,start,end,first,siz,step
    type(pm_ptr):: ptr
    type(pm_ptr),target:: vec,offset
    integer(pm_ln):: i,j,k,n,m,tstart,itstart,nstart,ielsize
    integer(pm_ln):: istart,iend,isiz,ifirst,istep,idx,rpt
    type(pm_reg),pointer:: reg
    n=import_vec%data%ln(import_vec%offset)+1_pm_ln
    vec=pm_new(context,pm_long,n)
    itstart=import_vec%data%ln(import_vec%offset+2)
    nstart=import_vec%data%ln(import_vec%offset+1)
    k=0
    do i=nstart,pm_fast_esize(import_vec)-4_pm_ln+nstart
        ielsize=elsize%data%ln(elsize%offset+i)
        istart=start%data%ln(start%offset+i)
        iend=end%data%ln(end%offset+i)
        ifirst=first%data%ln(first%offset+i)
        isiz=siz%data%ln(siz%offset+i)
        istep=step%data%ln(step%offset+i)
        tstart=ifirst+itstart
        if(tstart==0) then
           rpt=ielsize
           idx=istart
        else
           idx=tstart/ielsize
           rpt=ielsize-(tstart-idx*ielsize)
           idx=istart+mod(idx,isiz)*istep
        endif
        m=0
        do j=1,import_vec%data%ln(import_vec%offset+i-nstart+4_pm_ln)
           vec%data%ln(vec%offset+k)=idx
           k=k+1
           rpt=rpt-1
           m=m+1
           if(m>=isiz) then
              idx=istart
              m=0
              rpt=ielsize
           elseif(rpt==0) then
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
  end function vector_iota_trunc

  ! Calculate indices within block cyclic tile
  subroutine vector_bc(context,ndim,arg,start,end,step,ostart,oend,ostep,&
       begin,finish,tot,off)
    type(pm_context),pointer:: context
    integer,intent(in):: ndim
    type(pm_ptr),dimension(14):: arg
    integer(pm_ln),dimension(7),intent(in):: start,end,step,ostart,oend,ostep,&
         begin,finish
    integer(pm_ln),intent(in):: tot,off
    integer(pm_ln):: i1,i2,i3,i4,i5,i6,i7
    integer(pm_ln):: j1,j2,j3,j4,j5,j6,j7
    integer(pm_ln):: s1,s2,s3,s4,s5,s6,s7
    integer(pm_ln):: k
    k=off
    select case(ndim)
    case(1)
       s1=begin(1)
       do i1=ostart(1),oend(1),ostep(1)
          do j1=start(1)+s1,min(finish(1)-i1,end(1)),step(1)
             arg(1)%data%ln(arg(1)%offset+k)=i1
             arg(2)%data%ln(arg(2)%offset+k)=j1
             k=k+1
          enddo
          s1=0
       enddo
    case(2)
       s2=begin(1)
       do i2=ostart(2),oend(2),ostep(2)
          s1=begin(1)
          do i1=ostart(1),oend(1),ostep(1)
             do j2=start(2)+s2,min(finish(2)-i2,end(2)),step(2)
                do j1=start(1)+s1,min(finish(1)-i1,end(1)),step(1)
                   arg(1)%data%ln(arg(1)%offset+k)=i1
                   arg(2)%data%ln(arg(2)%offset+k)=j1
                   arg(3)%data%ln(arg(3)%offset+k)=i2
                   arg(4)%data%ln(arg(4)%offset+k)=j2
                   k=k+1
                enddo
             enddo
             s1=0
          enddo
          s2=0
       enddo
    case(3)
       s3=begin(3)
       do i3=ostart(3),oend(3),ostep(3)
          s2=begin(2)
          do i2=ostart(2),oend(2),ostep(2)
             s1=begin(1)
             do i1=ostart(1),oend(1),ostep(1)
                do j3=start(3)+s3,min(finish(3)-i3,end(3)),step(3)
                   do j2=start(2)+s2,min(finish(2)-i2,end(2)),step(2)
                      do j1=start(1)+s1,min(finish(1)-i1,end(1)),step(1)
                         arg(1)%data%ln(arg(1)%offset+k)=i1
                         arg(2)%data%ln(arg(2)%offset+k)=j1
                         arg(3)%data%ln(arg(3)%offset+k)=i2
                         arg(4)%data%ln(arg(4)%offset+k)=j2
                         arg(5)%data%ln(arg(5)%offset+k)=i3
                         arg(6)%data%ln(arg(6)%offset+k)=j3
                         k=k+1
                      enddo
                   enddo
                enddo
                s1=0
             enddo
             s2=0
          enddo
          s3=0
       enddo
    case(4)
       s4=begin(4)
       do i4=ostart(4),oend(4),ostep(4)
          s3=begin(3)
          do i3=ostart(3),oend(3),ostep(3)
             s2=begin(2)
             do i2=ostart(2),oend(2),ostep(2)
                s1=begin(1)
                do i1=ostart(1),oend(1),ostep(1)
                   do j4=start(4)+s4,min(finish(4)-i4,end(4)),step(4)
                      do j3=start(3)+s3,min(finish(3)-i3,end(3)),step(3)
                         do j2=start(2)+s2,min(finish(2)-i2,end(2)),step(2)
                            do j1=start(1)+s1,min(finish(1)-i1,end(1)),step(1)
                               arg(1)%data%ln(arg(1)%offset+k)=i1
                               arg(2)%data%ln(arg(2)%offset+k)=j1
                               arg(3)%data%ln(arg(3)%offset+k)=i2
                               arg(4)%data%ln(arg(4)%offset+k)=j2
                               arg(5)%data%ln(arg(5)%offset+k)=i3
                               arg(6)%data%ln(arg(6)%offset+k)=j3
                               arg(7)%data%ln(arg(7)%offset+k)=i4
                               arg(8)%data%ln(arg(8)%offset+k)=j4
                               k=k+1
                            enddo
                         enddo
                      enddo
                      s1=0
                   enddo
                enddo
                s2=0
             enddo
             s3=0
          enddo
          s4=0
       enddo
    case(5)
       s5=begin(5)
       do i5=ostart(5),oend(5),ostep(5)
          s4=begin(4)
          do i4=ostart(4),oend(4),ostep(4)
             s3=begin(3)
             do i3=ostart(3),oend(3),ostep(3)
                s2=begin(2)
                do i2=ostart(2),oend(2),ostep(2)
                   s1=begin(1)
                   do i1=ostart(1),oend(1),ostep(1)
                      do j5=start(5)+s5,min(finish(5)-i5,end(5)),step(5)
                         do j4=start(4)+s4,min(finish(4)-i4,end(4)),step(4)
                            do j3=start(3)+s3,min(finish(3)-i3,end(3)),step(3)
                               do j2=start(2)+s2,min(finish(2)-i2,end(2)),step(2)
                                  do j1=start(1)+s1,min(finish(1)-i1,end(1)),step(1)
                                     arg(1)%data%ln(arg(1)%offset+k)=i1
                                     arg(2)%data%ln(arg(2)%offset+k)=j1
                                     arg(3)%data%ln(arg(3)%offset+k)=i2
                                     arg(4)%data%ln(arg(4)%offset+k)=j2
                                     arg(5)%data%ln(arg(5)%offset+k)=i3
                                     arg(6)%data%ln(arg(6)%offset+k)=j3
                                     arg(7)%data%ln(arg(7)%offset+k)=i4
                                     arg(8)%data%ln(arg(8)%offset+k)=j4
                                     arg(9)%data%ln(arg(9)%offset+k)=i5
                                     arg(10)%data%ln(arg(10)%offset+k)=j5                          
                                     k=k+1
                                  enddo
                               enddo
                            enddo
                         enddo
                         s1=0
                      enddo
                   enddo
                   s2=0
                enddo
                s3=0
             enddo
             s4=0
          enddo
          s5=0
       enddo
    case(6)
       s6=begin(6)
       do i6=ostart(6),oend(6),ostep(6)
          s5=begin(5)
          do i5=ostart(5),oend(5),ostep(5)
             s4=begin(4)
             do i4=ostart(4),oend(4),ostep(4)
                s3=begin(3)
                do i3=ostart(3),oend(3),ostep(3)
                   s2=begin(2)
                   do i2=ostart(2),oend(2),ostep(2)
                      s1=begin(1)
                      do i1=ostart(1),oend(1),ostep(1)
                         do j6=start(6)+s6,min(finish(6)-i6,end(6)),step(6)
                            do j5=start(5)+s5,min(finish(5)-i5,end(5)),step(5)
                               do j4=start(4)+s4,min(finish(4)-i4,end(4)),step(4)
                                  do j3=start(3)+s3,min(finish(3)-i3,end(3)),step(3)
                                     do j2=start(2)+s2,min(finish(2)-i2,end(2)),step(2)
                                        do j1=start(1)+s1,min(finish(1)-i1,end(1)),step(1)
                                           arg(1)%data%ln(arg(1)%offset+k)=i1
                                           arg(2)%data%ln(arg(2)%offset+k)=j1
                                           arg(3)%data%ln(arg(3)%offset+k)=i2
                                           arg(4)%data%ln(arg(4)%offset+k)=j2
                                           arg(5)%data%ln(arg(5)%offset+k)=i3
                                           arg(6)%data%ln(arg(6)%offset+k)=j3
                                           arg(7)%data%ln(arg(7)%offset+k)=i4
                                           arg(8)%data%ln(arg(8)%offset+k)=j4
                                           arg(9)%data%ln(arg(9)%offset+k)=i5
                                           arg(10)%data%ln(arg(10)%offset+k)=j5
                                           arg(11)%data%ln(arg(11)%offset+k)=i6
                                           arg(12)%data%ln(arg(12)%offset+k)=j6
                                           k=k+1
                                        enddo
                                     enddo
                                  enddo
                               enddo
                            enddo
                            s1=0
                         enddo
                      enddo
                      s2=0
                   enddo
                   s3=0
                enddo
                s4=0
             enddo
             s5=0
          enddo
          s6=0
       enddo
    case(7)
       do i7=ostart(7),oend(7),ostep(7)
          s6=begin(6)
          do i6=ostart(6),oend(6),ostep(6)
             s5=begin(5)
             do i5=ostart(5),oend(5),ostep(5)
                s4=begin(4)
                do i4=ostart(4),oend(4),ostep(4)
                   s3=begin(3)
                   do i3=ostart(3),oend(3),ostep(3)
                      s2=begin(2)
                      do i2=ostart(2),oend(2),ostep(2)
                         s1=begin(1)
                         do i1=ostart(1),oend(1),ostep(1)
                            do j7=start(7)+s7,min(finish(7)-i7,end(7)),step(7)
                               do j6=start(6)+s6,min(finish(6)-i6,end(6)),step(6)
                                  do j5=start(5)+s5,min(finish(5)-i5,end(5)),step(5)
                                     do j4=start(4)+s4,min(finish(4)-i4,end(4)),step(4)
                                        do j3=start(3)+s3,min(finish(3)-i3,end(3)),step(3)
                                           do j2=start(2)+s2,min(finish(2)-i2,end(2)),step(2)
                                              do j1=start(1)+s1,min(finish(1)-i1,end(1)),step(1)
                                                 arg(1)%data%ln(arg(1)%offset+k)=i1
                                                 arg(2)%data%ln(arg(2)%offset+k)=j1
                                                 arg(3)%data%ln(arg(3)%offset+k)=i2
                                                 arg(4)%data%ln(arg(4)%offset+k)=j2
                                                 arg(5)%data%ln(arg(5)%offset+k)=i3
                                                 arg(6)%data%ln(arg(6)%offset+k)=j3
                                                 arg(7)%data%ln(arg(7)%offset+k)=i4
                                                 arg(8)%data%ln(arg(8)%offset+k)=j4
                                                 arg(9)%data%ln(arg(9)%offset+k)=i5
                                                 arg(10)%data%ln(arg(10)%offset+k)=j5
                                                 arg(11)%data%ln(arg(11)%offset+k)=i6
                                                 arg(12)%data%ln(arg(12)%offset+k)=j6
                                                 arg(13)%data%ln(arg(11)%offset+k)=i7
                                                 arg(14)%data%ln(arg(12)%offset+k)=j7                      
                                                 k=k+1
                                              enddo
                                           enddo
                                        enddo
                                     enddo
                                  enddo
                               enddo
                            enddo
                            s1=0
                         enddo
                         s2=0
                      enddo
                      s3=0
                   enddo
                   s4=0
                enddo
                s5=0
             enddo
             s6=0
          enddo
          s7=0
       enddo
    end select
    if(k-off/=tot) call pm_panic('vector_bc')
  end subroutine vector_bc

  
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
  recursive function vector_get_elems(context,v,ix,errno,keeparrays) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v,ix
    integer,intent(inout):: errno
    logical,intent(in),optional:: keeparrays
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
       if(present(keeparrays)) then
          dom=vector_get_elems(context,v%data%ptr(v%offset+pm_array_dom),ix,errno)
          len=vector_get_elems(context,v%data%ptr(v%offset+pm_array_length),ix,errno)
          off=vector_get_elems(context,v%data%ptr(v%offset+pm_array_offset),ix,errno)
          vec=vector_get_elems(context,v%data%ptr(v%offset+pm_array_vect),ix,errno,keeparrays)
       else
          dom=vector_get_elems(context,v%data%ptr(v%offset+pm_array_dom),ix,errno)
          oldlen=v%data%ptr(v%offset+pm_array_length)
          len=vector_get_elems(context,oldlen,ix,errno)
          oldoff=v%data%ptr(v%offset+pm_array_offset)
          esize=pm_fast_esize(ix)
          off=pm_new(context,pm_long,esize+1_pm_ln)
          off%data%ln(off%offset:off%offset+esize)=0
          oldvec=v%data%ptr(v%offset+pm_array_vect)
          vec=pm_new(context,pm_pointer,esize+1_pm_ln)
          do j=0,esize
             i=ix%data%ln(ix%offset+j)
             vec%data%ptr(vec%offset+j)=copy_vector(context,&
                  oldvec%data%ptr(oldvec%offset+i),&
                  pm_null_obj,&
               oldoff%data%ln(oldoff%offset+i),&
               oldlen%data%ln(oldlen%offset+i))
          enddo
       endif
       ptr=make_array(context,pm_array_type,&
            int(v%data%ptr(v%offset+pm_array_typeof)%offset),&
            vec,dom,len,off)
       call pm_delete_register(context,reg)
    case(pm_struct_type,pm_rec_type)
       root=>pm_new_as_root(context,pm_usr,pm_fast_esize(v)+1)
       ptr=root%ptr
       ptr%data%ptr(ptr%offset)=v%data%ptr(v%offset)
       ptr%data%ptr(ptr%offset+1_pm_p)=v%data%ptr(v%offset+1_pm_p)
       do i=2,pm_fast_esize(v)
          ptr%data%ptr(ptr%offset+i)=vector_get_elems(context,&
               v%data%ptr(v%offset+i),ix,errno,keeparrays)
       enddo
       call pm_delete_root(context,root)
    case(pm_pointer)
       if(present(keeparrays)) then
          ptr=pm_new(context,pm_pointer,pm_fast_esize(ix)+1_pm_ln)
          do i=0,pm_fast_esize(ix)
             ptr%data%ptr(ptr%offset+i)=v%data%ptr(v%offset+ix%data%ln(ix%offset+i))
          enddo
       else
          root=>pm_new_as_root(context,pm_usr,3_pm_ln)
          ptr=root%ptr
          do i=0,pm_fast_esize(ix)
             call pm_ptr_assign(context,ptr,i,&
                  copy_vector(context,v%data%ptr(v%offset+ix%data%ln(ix%offset+i)),&
                  pm_null_obj,0_pm_ln,-1_pm_ln))
          enddo
          call pm_delete_root(context,root)
       endif
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
    case(pm_undef:pm_null)
       ptr=v
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
    integer,intent(inout):: errno
    integer:: tno
    integer(pm_ln):: h,i,j,m,siz
    integer:: k
    type(pm_ptr):: len,off,avec,len2,off2,avec2
    tno=pm_fast_typeof(v)
    if(full_type(e)/=full_type(v)) then
       write(*,*) trim(pm_typ_as_string(context,tno)),&
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
    case(pm_longlong)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%lln(v%offset+ix%data%ln(ix%offset+i))=e%data%lln(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%lln(v%offset+ix%data%ln(ix%offset+i))=e%data%lln(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%lln(v%offset+ix%data%ln(ix%offset+i))=e%data%lln(e%offset+i)
          enddo
       endif
    case(pm_int8)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%i8(v%offset+ix%data%ln(ix%offset+i))=e%data%i8(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%i8(v%offset+ix%data%ln(ix%offset+i))=e%data%i8(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%i8(v%offset+ix%data%ln(ix%offset+i))=e%data%i8(e%offset+i)
          enddo
       endif
    case(pm_int16)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%i16(v%offset+ix%data%ln(ix%offset+i))=e%data%i16(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%i16(v%offset+ix%data%ln(ix%offset+i))=e%data%i16(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%i16(v%offset+ix%data%ln(ix%offset+i))=e%data%i16(e%offset+i)
          enddo
       endif
    case(pm_int32)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%i32(v%offset+ix%data%ln(ix%offset+i))=e%data%i32(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%i32(v%offset+ix%data%ln(ix%offset+i))=e%data%i32(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%i32(v%offset+ix%data%ln(ix%offset+i))=e%data%i32(e%offset+i)
          enddo
       endif
    case(pm_int64)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%i64(v%offset+ix%data%ln(ix%offset+i))=e%data%i64(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%i64(v%offset+ix%data%ln(ix%offset+i))=e%data%i64(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%i64(v%offset+ix%data%ln(ix%offset+i))=e%data%i64(e%offset+i)
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
    case(pm_single_complex)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%c(v%offset+ix%data%ln(ix%offset+i))=e%data%c(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%c(v%offset+ix%data%ln(ix%offset+i))=e%data%c(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%c(v%offset+ix%data%ln(ix%offset+i))=e%data%c(e%offset+i)
          enddo
       endif
    case(pm_double_complex)
       if(pm_fast_isnull(ve)) then
          do i=0,pm_fast_esize(ix)
             v%data%dc(v%offset+ix%data%ln(ix%offset+i))=e%data%dc(e%offset+i)
          enddo
       elseif(pm_fast_vkind(ve)==pm_logical) then
          do i=0,pm_fast_esize(ix)
             if(ve%data%l(ve%offset+i)) then
                v%data%dc(v%offset+ix%data%ln(ix%offset+i))=e%data%dc(e%offset+i)
             endif
          enddo
       else
          do j=0,pm_fast_esize(ve)
             i=ve%data%ln(ve%offset+j)
             v%data%dc(v%offset+ix%data%ln(ix%offset+i))=e%data%dc(e%offset+i)
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
    integer,intent(inout):: errno
    integer:: tno,k
 
    tno=pm_fast_typeof(v)
    if(full_type(v)/=full_type(e)) then
       if(tno/=pm_pointer) then
          errno=vector_type_error
          return
       endif
    endif
    select case(tno)
    case(pm_array_type,pm_const_array_type)
       call assign_single(context,v%data%ptr(v%offset+pm_array_dom),j,&
            e%data%ptr(e%offset+pm_array_dom),errno)
       call array_assign(context,v,j,e,0_pm_ln,errno)
    case(pm_struct_type,pm_rec_type)
       do k=2,pm_fast_esize(v)
          call assign_single(context,v%data%ptr(v%offset+k),j,&
               e%data%ptr(e%offset+k),errno)
       enddo
    case(pm_pointer)
       if(v%data%ptr(v%offset+j)==pm_null_obj) then
          call pm_ptr_assign(context,v,j,copy_vector(context,&
               e%data%ptr(e%offset),pm_null_obj,0_pm_ln,-1_pm_ln))
       else
          call assign_single(context,v%data%ptr(v%offset+j),0_pm_ln,&
               e%data%ptr(e%offset),errno)
       endif
    case(pm_int)
       v%data%i(v%offset+j)=e%data%i(e%offset)
    case(pm_long)
       v%data%ln(v%offset+j)=e%data%ln(e%offset)
    case(pm_longlong)
       v%data%lln(v%offset+j)=e%data%lln(e%offset)
    case(pm_int8)
       v%data%i8(v%offset+j)=e%data%i8(e%offset)
    case(pm_int16)
       v%data%i16(v%offset+j)=e%data%i16(e%offset)
    case(pm_int32)
       v%data%i32(v%offset+j)=e%data%i32(e%offset)
    case(pm_int64)
       v%data%i64(v%offset+j)=e%data%i64(e%offset)
    case(pm_single)
       v%data%r(v%offset+j)=e%data%r(e%offset)
    case(pm_double)
       v%data%d(v%offset+j)=e%data%d(e%offset)
    case(pm_single_complex)
       v%data%c(v%offset+j)=e%data%c(e%offset)
    case(pm_double_complex)
       v%data%dc(v%offset+j)=e%data%dc(e%offset)
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
    integer,intent(inout):: errno
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
          call pm_ptr_assign(context,v,ix(i),&
               copy_vector(context,e%data%ptr(e%offset+iy(i)),pm_null_obj,0_pm_ln,-1_pm_ln))
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
    !write(*,*) 'k=',k
    select case(k)
    case(pm_array_type,pm_const_array_type)
       write(*,*) spaces(1:depth*2),'Array (',v%data%hash,v%offset
       call vector_dump(context,v%data%ptr(v%offset+pm_array_vect),depth+1)
       write(*,*) spaces(1:depth*2),') over ('
       call vector_dump(context,v%data%ptr(v%offset+pm_array_dom),depth+1)
       write(*,*) spaces(1:depth*2),')'
    case(pm_dref_type,pm_dref_shared_type)
       if(k==pm_dref_type) then
          write(*,*) spaces(1:depth*2),'D-ref ('
       else
          write(*,*) spaces(1:depth*2),'D-ref shared ('
       endif
       do i=1,pm_fast_esize(v)
          call vector_dump(context,v%data%ptr(v%offset+i),depth+1)
       enddo
       write(*,*) spaces(1:depth*2),')'
    case(pm_struct_type,pm_rec_type)
       tno=full_type(v)
       name=pm_typ_vect(context,tno)
       name=pm_name_val(context,pm_tv_name(name))
       tno=name%data%i(name%offset)
       if(k==pm_struct_type) then
          write(*,*) spaces(1:depth*2),'struct ',&
               trim(pm_name_as_string(context,tno)),'('
       else
          write(*,*) spaces(1:depth*2),'rec ',&
               trim(pm_name_as_string(context,tno)),'('
       endif
       do i=1,pm_fast_esize(name)
          tno=name%data%i(name%offset+i)
          write(*,*) spaces(1:depth*2+2),&
               trim(pm_name_as_string(context,tno)),'='
          call vector_dump(context,v%data%ptr(v%offset+i+1),depth+2)
       enddo
       write(*,*) spaces(1:depth*2),')'
    case(pm_name)
       write(*,*) spaces(1:depth*2),'''',trim(pm_name_as_string(context,int(v%offset)))
    case(pm_proc)
       write(*,*) spaces(1:depth*2),'proc{',&
            trim(pm_name_as_string(context,int(v%offset))),'}'
    case(pm_type)
       write(*,*) spaces(1:depth*2),'type{',&
            trim(pm_typ_as_string(context,int(v%offset))),'}'
    case default
       call pm_dump_tree(context,6,v,depth)
    end select

  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
       
  end subroutine vector_dump
  
  recursive subroutine vector_dump_to(context,v,j,output,depth)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: j
    integer,intent(in):: depth
    interface
       subroutine output(context,str)
         use pm_memory
         type(pm_context),pointer:: context
         character(len=*),intent(in):: str
       end subroutine output
    end interface
    character(len=80):: spaces=' '
    integer:: k
    type(pm_ptr):: name,w
    integer:: tno,i,name1
    integer(pm_ln):: jj,esize
    if(depth>=35) then
       call output(context,spaces(1:depth*2)//'...')
       return
    endif
    k=pm_fast_typeof(v)
    select case(k)
    case(pm_array_type,pm_const_array_type)
       call output(context,spaces(1:depth*2)//'Array (')
       w=v%data%ptr(v%offset+pm_array_vect)
       w=w%data%ptr(w%offset+j)
       esize=vector_esize(w)
       do jj=0,min(5,esize)
          call vector_dump_to(context,w,jj,output,depth+1)
       enddo
       if(esize>5) call output(context,spaces(1:depth*2+2)//'...')
       call output(context,spaces(1:depth*2)//') over (')
       call vector_dump_to(context,v%data%ptr(v%offset+pm_array_dom),j,output,depth+1)
       call output(context,spaces(1:depth*2)//')')
    case(pm_dref_type,pm_dref_shared_type)
       if(k==pm_dref_type) then
          call output(context,spaces(1:depth*2)//'D-ref (')
       else
          call output(context,spaces(1:depth*2)//'D-ref shared (')
       endif
       do i=1,pm_fast_esize(v)
          call vector_dump_to(context,v%data%ptr(v%offset+i),j,output,depth+1)
       enddo
       call output(context,spaces(1:depth*2)//')')
    case(pm_struct_type,pm_rec_type)
       tno=full_type(v)
       name=pm_typ_vect(context,tno)
       name=pm_name_val(context,pm_tv_name(name))
       tno=name%data%i(name%offset)
       if(k==pm_struct_type) then
          call output(context,spaces(1:depth*2)//'struct '//&
               trim(pm_name_as_string(context,tno))//'(')
       else
          call output(context,spaces(1:depth*2)//'rec '//&
               trim(pm_name_as_string(context,tno))//'(')
       endif
       do i=1,pm_fast_esize(name)
          name1=name%data%i(name%offset+i)
          if(abs(name1)>=sym_d1.and.abs(name1)<=sym_d7) then
             call output(context,spaces(1:depth*2+2)//&
                  achar(iachar('1')+name1-sym_d1)//'=')
          else
             call output(context,spaces(1:depth*2+2)//&
                  trim(pm_name_as_string(context,name1))//'=')
          endif
          call vector_dump_to(context,v%data%ptr(v%offset+i+1),j,output,depth+2)
       enddo
       call output(context,spaces(1:depth*2)//')')
    case(pm_name)
       call output(context,spaces(1:depth*2)//trim(pm_name_as_string(context,int(v%offset))))
    case(pm_null)
       call output(context,spaces(1:depth*2)//'null')
    case(pm_type)
       call output(context,spaces(1:depth*2)//trim(pm_typ_as_string(context,int(v%offset))))
    case(pm_proc)
       call output(context,spaces(1:depth*2)//'proc('//&
            trim(pm_name_as_string(context,int(v%offset)))//')')
    case(pm_pointer)
       call output(context,spaces(1:depth*2)//'pval(')
       call vector_dump_to(context,v%data%ptr(v%offset+j),0_pm_ln,output,depth+2)
       call output(context,spaces(1:depth*2)//')')
    case(pm_int:pm_complex256)
       call output(context,spaces(1:depth*2)//trim(pm_number_as_string(context,v,j)))
    case default
       call output(context,spaces(1:depth*2)//'?'//trim(pm_typ_as_string(context,k)))
    end select

  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
       
  end subroutine vector_dump_to

  
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
    integer,intent(inout):: errno
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
    integer,intent(inout):: errno
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
    integer,intent(inout):: errno
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
    integer,intent(inout):: errno
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
    str=make_array(context,pm_array_type,int(pm_string_type),vec,len,len,off)
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
    str=make_array(context,pm_array_type,int(pm_string_type),vec,len,len,off)
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
    vec=vec%data%ptr(vec%offset+j)
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
    str=make_array(context,pm_array_type,int(pm_string_type),vec,len,len,off)
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

  subroutine fmt_lln(v,n,str)
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: n
    character(len=*),intent(out):: str
    character(len=25):: mess
    mess=' '
    write(mess,'(i25)') v%data%lln(v%offset+n)
    str=adjustl(mess)
  end subroutine fmt_lln

  subroutine fmt_i32(v,n,str)
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: n
    character(len=*),intent(out):: str
    character(len=10):: mess
    mess=' '
    write(mess,'(i10)') v%data%i32(v%offset+n)
    str=adjustl(mess)
  end subroutine fmt_i32

  subroutine fmt_i64(v,n,str)
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: n
    character(len=*),intent(out):: str
    character(len=20):: mess
    mess=' '
    write(mess,'(i20)') v%data%i64(v%offset+n)
    str=adjustl(mess)
  end subroutine fmt_i64
  
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
       str='TRUE  '
    else
       str='FALSE '
    endif
  end subroutine fmt_l

  
  subroutine advance_dim(val,max,overflow,n)
    integer(pm_ln),dimension(*),intent(inout):: val,max
    integer(pm_ln),dimension(*),intent(inout):: overflow
    integer(pm_ln),intent(in):: n
    integer(pm_ln):: i
    do i=1,n
       val(i)=val(i)+overflow(i)
    enddo
    do i=1,n
       overflow(i)=merge(1_pm_ln,0_pm_ln,val(i)>=max(i))
       val(i)=merge(0_pm_ln,val(i),val(i)>=max(i))
    enddo
  end subroutine advance_dim

  function advance(context,val,max,ok) result(mask)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(inout):: val
    type(pm_ptr),intent(in)::max
    type(pm_ptr),intent(in),optional:: ok
    type(pm_ptr):: mask
    integer(pm_ln),allocatable,dimension(:):: overflow
    integer(pm_ln):: i,esize
    integer:: j
    type(pm_ptr):: v,m
    esize=pm_fast_esize(val%data%ptr(val%offset+2))
    allocate(overflow(0:esize))
    overflow=1
    do j=2,pm_fast_esize(val)
       v=val%data%ptr(val%offset+j)
       m=max%data%ptr(max%offset+j)
       call advance_dim(v%data%ln(v%offset:),m%data%ln(m%offset:),overflow,esize+1)
    enddo
    mask=pm_new(context,pm_logical,esize+1)
    if(present(ok)) then
       do i=0,esize
          mask%data%l(mask%offset+i)=overflow(i)==0.and.ok%data%l(ok%offset+i)
       enddo
    else
       do i=0,esize
          mask%data%l(mask%offset+i)=overflow(i)==0
       enddo
    endif
    deallocate(overflow)
  contains
    include 'fesize.inc'
  end function advance

  function init_loop(context,v) result(w)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v
    type(pm_ptr):: w
    integer(pm_ln):: esize,n,i
    type(pm_ptr):: p
    type(pm_root),pointer:: root
    esize=pm_fast_esize(v%data%ptr(v%offset+2))
    n=pm_fast_esize(v)
    root=>pm_new_as_root(context,pm_usr,n)
    w=root%ptr
    w%data%ptr(w%offset)=v%data%ptr(v%offset)
    w%data%ptr(w%offset+1)=v%data%ptr(v%offset+1)
    do i=2,n
       p=pm_assign_new(context,w,i,pm_long,esize+1,.false.)
       p%data%ln(p%offset:p%offset+esize)=0
    enddo
    call pm_delete_root(context,root)
  contains
    include 'fesize.inc'
  end function init_loop

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
  
  function shrink_ve(context,mask,esize,n) result(ind)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: mask
    integer(pm_ln),intent(in):: esize
    integer(pm_ln),intent(in),optional:: n
    type(pm_ptr):: ind
    integer(pm_ln):: j,k
    if(pm_fast_isnull(mask)) then
       ind=pm_new(context,pm_long,esize+1)
       do j=0,esize
          ind%data%ln(ind%offset+j)=j
       enddo
    elseif(pm_fast_vkind(mask)==pm_long.or.pm_fast_vkind(mask)==pm_tiny_int) then
       ind=mask
    else
       if(present(n)) then
          k=n
       else
          k=0
          do j=0,esize
             if(mask%data%l(mask%offset+j)) then
                k=k+1
             endif
          enddo
       endif
       if(k==0) then
          ind=pm_fast_tinyint(context,0)
          return
       endif
       ind=pm_new(context,pm_long,k)
       k=0
       do j=0,esize
          if(mask%data%l(mask%offset+j)) then
             ind%data%ln(ind%offset+k)=j
             k=k+1
          endif
       enddo
    endif
  contains
    include 'fvkind.inc'
    include 'fisnull.inc'
    include 'ftiny.inc'
  end function shrink_ve
  
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

  subroutine intersect_aseq(a1,n1,a2,n2,a3,n3)
    integer(pm_ln),dimension(*),intent(in):: a1,a2
    integer(pm_ln),intent(in):: n1,n2
    integer(pm_ln),dimension(*),intent(out):: a3
    integer(pm_ln),intent(out):: n3
    integer(pm_ln):: i,j,k
    logical:: a1asc,a2asc
    n3=0
    if(n1==0.or.n2==0) return
    if(max(a1(1),a1(n1)) < min(a2(1),a2(n2)).or.max(a2(1),a2(n2)) < min(a1(1),a1(n1))) return
    a1asc=.true.
    a2asc=.true.
    if(n1>1) a1asc=a1(2)>a1(1)
    if(n2>1) a2asc=a2(2)>a2(1)
    if(a1asc) then
       if(a2asc) then
          i=1
          j=1
          k=1
          do while(i<=n1.and.j<=n2)
             if(a1(i)<a2(j)) then
                i=i+1
             elseif(a1(i)==a2(j)) then
                a3(k)=a1(i)
                i=i+1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1
       else
          i=n1
          j=1
          k=1
          do while(i>0.and.j<=n2)
             if(a1(i)>a2(j)) then
                i=i-1
             elseif(a1(i)==a2(j)) then
                a3(k)=a1(i)
                i=i-1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1     
       endif
    else
       if(a2asc) then
          i=n1
          j=1
          k=1
          do while(i>0.and.j<=n2)
             if(a1(i)<a2(j)) then
                i=i-1
             elseif(a1(i)==a2(j)) then
                a3(k)=a1(i)
                i=i-1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1   
       else
          i=1
          j=1
          k=1
          do while(i<=n1.and.j<=n2)
             if(a1(i)>a2(j)) then
                i=i+1
             elseif(a1(i)==a2(j)) then
                a3(k)=a1(i)
                i=i+1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1         
       endif
    endif
  end subroutine intersect_aseq

  function aseq_includes(a1,n1,a2,n2) result(ok)
    integer(pm_ln),dimension(*),intent(in):: a1,a2
    integer(pm_ln),intent(in):: n1,n2
    logical:: ok
    integer(pm_ln):: i,j
    logical:: a1asc,a2asc
    ok=.false.
    if(n1==0.or.n2==0) return
    if(max(a1(1),a1(n1)) < min(a2(1),a2(n2)).or.max(a2(1),a2(n2)) < min(a1(1),a1(n1))) return
    a1asc=.true.
    a2asc=.true.
    if(n1>1) a1asc=a1(2)>a1(1)
    if(n2>1) a2asc=a2(2)>a2(1)
    if(a1asc) then
       if(a2asc) then
          i=1
          do j=1,n2
             do while(i<=n1.and.a1(i)<a2(j))
                i=i+1
             enddo
             if(a1(i)/=a2(j)) then
                ok=.false.
                return
             endif
          enddo
          ok=.true.
          return
       else
          i=1
          do j=n2,1,-1
             do while(i<=n1.and.a1(i)<a2(j))
                i=i+1
             enddo
             if(a1(i)/=a2(j)) then
                ok=.false.
                return
             endif
          enddo
          ok=.true.
          return
       endif
    else
       if(a2asc) then
          i=n1
          do j=1,n2
             do while(i>0.and.a1(i)<a2(j))
                i=i-1
             enddo
             if(a1(i)/=a2(j)) then
                ok=.false.
                return
             endif
          enddo
          ok=.true.
          return
       else
          i=n1
          do j=n2,1,-1
             do while(i>0.and.a1(i)<a2(j))
                i=i-1
             enddo
             if(a1(i)/=a2(j)) then
                ok=.false.
                return
             endif
          enddo
          ok=.true.
          return
       endif
    endif
  end function aseq_includes

  function aseq_index(a,n,v) result(index)
    integer(pm_ln),dimension(*),intent(in):: a
    integer(pm_ln),intent(in):: n
    integer(pm_ln),intent(in):: v
    integer(pm_ln):: index
    integer(pm_ln):: left,right,middle
    left=1
    right=n+1
    if(a(1)<a(2)) then
       if(v<a(1)) then
          index=-1
       endif
       do while(left<right-1)
          middle=(left+right)/2
          if(a(middle)<=v) then
             left=middle
          else
             right=middle
          endif
       enddo
       index=left-1
    else
       if(v<a(n)) then
          index=n
          return
       endif
       do while(left<right-1)
          middle=(left+right)/2
          if(a(middle)>v) then
             left=middle
          else
             right=middle
          endif
       enddo
       index=right-1
    endif
  end function aseq_index

  subroutine overlap_aseq(a1,n1,a2,n2,a3,n3)
    integer(pm_ln),dimension(*),intent(in):: a1,a2
    integer(pm_ln),intent(in):: n1,n2
    integer(pm_ln),dimension(*),intent(out):: a3
    integer(pm_ln),intent(out):: n3
    integer(pm_ln):: i,j,k
    logical:: a1asc,a2asc
    n3=0
    if(n1==0.or.n2==0) return
    if(max(a1(1),a1(n1)) < min(a2(1),a2(n2)).or.max(a2(1),a2(n2)) < min(a1(1),a1(n1))) return
    a1asc=.true.
    a2asc=.true.
    if(n1>1) a1asc=a1(2)>a1(1)
    if(n2>1) a2asc=a2(2)>a2(1)
    if(a1asc) then
       if(a2asc) then
          i=1
          j=1
          k=1
          do while(i<=n1.and.j<=n2)
             if(a1(i)<a2(j)) then
                i=i+1
             elseif(a1(i)==a2(j)) then
                a3(k)=i-1
                i=i+1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1
       else
          i=n1
          j=1
          k=1
          do while(i>0.and.j<=n2)
             if(a1(i)>a2(j)) then
                i=i-1
             elseif(a1(i)==a2(j)) then
                a3(k)=i-1
                i=i-1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1     
       endif
    else
       if(a2asc) then
          i=n1
          j=1
          k=1
          do while(i>0.and.j<=n2)
             if(a1(i)<a2(j)) then
                i=i-1
             elseif(a1(i)==a2(j)) then
                a3(k)=i-1
                i=i-1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1   
       else
          i=1
          j=1
          k=1
          do while(i<=n1.and.j<=n2)
             if(a1(i)>a2(j)) then
                i=i+1
             elseif(a1(i)==a2(j)) then
                a3(k)=i-1
                i=i+1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1         
       endif
    endif
  end subroutine overlap_aseq

  subroutine overlap_aseq2(a1,n1,a2,n2,a3,a4,n3)
    integer(pm_ln),dimension(*),intent(in):: a1,a2
    integer(pm_ln),intent(in):: n1,n2
    integer(pm_ln),dimension(*),intent(out):: a3,a4
    integer(pm_ln),intent(out):: n3
    integer(pm_ln):: i,j,k
    logical:: a1asc,a2asc
    n3=0
    if(n1==0.or.n2==0) return
    if(max(a1(1),a1(n1)) < min(a2(1),a2(n2)).or.max(a2(1),a2(n2)) < min(a1(1),a1(n1))) return
    a1asc=.true.
    a2asc=.true.
    if(n1>1) a1asc=a1(2)>a1(1)
    if(n2>1) a2asc=a2(2)>a2(1)
    if(a1asc) then
       if(a2asc) then
          i=1
          j=1
          k=1
          do while(i<=n1.and.j<=n2)
             if(a1(i)<a2(j)) then
                i=i+1
             elseif(a1(i)==a2(j)) then
                a3(k)=i-1
                a4(k)=j-1
                i=i+1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1
       else
          i=n1
          j=1
          k=1
          do while(i>0.and.j<=n2)
             if(a1(i)>a2(j)) then
                i=i-1
             elseif(a1(i)==a2(j)) then
                a3(k)=i-1
                a4(k)=j-1
                i=i-1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1     
       endif
    else
       if(a2asc) then
          i=n1
          j=1
          k=1
          do while(i>0.and.j<=n2)
             if(a1(i)<a2(j)) then
                i=i-1
             elseif(a1(i)==a2(j)) then
                a3(k)=i-1
                a4(k)=j-1
                i=i-1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1   
       else
          i=1
          j=1
          k=1
          do while(i<=n1.and.j<=n2)
             if(a1(i)>a2(j)) then
                i=i+1
             elseif(a1(i)==a2(j)) then
                a3(k)=i-1
                a4(k)=j-1
                i=i+1
                j=j+1
                k=k+1
             else
                j=j+1
             endif
          enddo
          n3=k-1         
       endif
    endif
  end subroutine overlap_aseq2
    
  subroutine intersect_bseq(l1,h1,wd1,st1,al1,l2,h2,wd2,st2,al2,a,n)
    integer(pm_ln),intent(in):: l1,h1,wd1,st1,al1,l2,h2,wd2,st2,al2
    integer(pm_ln),dimension(*),intent(out):: a
    integer(pm_ln),intent(out):: n
    integer(pm_ln):: start,finish,actual_finish,first1,last1,first2,last2
    integer(pm_ln):: w1,w2,i,j,k,kk,bstart1,bend1,bstart2,bend2,u,v,gcd,jcycle

    w1=wd1-1
    w2=wd2-1
    start=max(l1,l2)
    actual_finish=min(h1,h2)
    if(st1==st2) then
       finish=min(actual_finish,start+st1-1)
       jcycle=st1
    else
       call extended_gcd(st1,st2,u,v,gcd)
       jcycle=st1*st2/gcd
       finish=min(actual_finish,start+jcycle-1)
    endif
    first1=((start+al1-l1)/st1)*st1+l1-al1
    first2=((start+al2-l2)/st2)*st2+l2-al2
    bstart1=first1+al1
    bend1=first1+w1
    bstart2=first2+al2
    bend2=first2+w2
    k=1
    do i=max(bstart1,bstart2),min(bend1,bend2,finish)
       a(k)=i
       k=k+1
    end do
    if(st1<=st2) then
       last1=((finish+al1-l1)/st1)*st1+l1-al1
       if(last1>first1) then
          do j=first1+st1,last1-1,st1
             if(bend2<j) then
                bend2=bend2+st2
             endif
             do i=max(j,bend2-w2),min(j+w1,bend2)
                a(k)=i
                k=k+1
             end do
          enddo
          j=last1
          if(bend2<j) then
             bend2=bend2+st2
          endif
          do i=max(j,bend2-w2),min(j+w1,bend2,finish)
             a(k)=i
             k=k+1
          enddo
       endif
    else
       last2=((finish+al2-l2)/st2)*st2+l2-al2
       if(last2>first2) then
          do j=first2+st2,last2-1,st2
             if(bend1<j) then
                bend1=bend1+st1
             endif
             do i=max(j,bend1-w1),min(j+w2,bend1)
                a(k)=i
                k=k+1
             end do
          enddo
          j=last2
          if(bend1<j) then
             bend1=bend1+st1
          endif
          do i=max(j,bend1-w1),min(j+w2,bend1,finish)
             a(k)=i
             k=k+1
          enddo
       endif
    endif
    
    kk=k-1
    if(finish<actual_finish.and.kk>0) then
       j=jcycle
       do while(j+a(kk)<=actual_finish)
          do i=1,kk
             a(k)=a(i)+j
             k=k+1
          enddo
          j=j+jcycle
       enddo
       if(j+a(1)<=actual_finish) then
          i=1
          do while(a(i)+j<=actual_finish)
             a(k)=a(i)+j
             k=k+1
             i=i+1
          enddo
       endif
    endif
    n=k-1
  end subroutine intersect_bseq

  subroutine overlap_bseq(l1,h1,wd1,st1,al1,l2,h2,wd2,st2,al2,a,n)
    integer(pm_ln),intent(in):: l1,h1,wd1,st1,al1,l2,h2,wd2,st2,al2
    integer(pm_ln),dimension(*),intent(out):: a
    integer(pm_ln),intent(out):: n
    integer(pm_ln):: start,finish,actual_finish,first1,last1,first2,last2
    integer(pm_ln):: w1,w2,i,j,jj,k,kk,bstart1,bend1,bstart2,bend2,u,v,gcd,jcycle
    integer(pm_ln):: span,nb,jfinish

    w1=wd1-1
    w2=wd2-1
    start=max(l1,l2)
    actual_finish=min(h1,h2)
    if(st1==st2) then
       finish=min(actual_finish,start+st1-1)
       jcycle=1
    else
       call extended_gcd(st1,st2,u,v,gcd)
       jcycle=st2/gcd
       finish=min(actual_finish,start+jcycle*st1-1)
       write(*,*) 'HERE'
    endif
    write(*,*) 'jcycle=',jcycle
    first1=((start+al1-l1)/st1)*st1+l1-al1
    first2=((start+al2-l2)/st2)*st2+l2-al2
    bstart1=first1+al1
    bend1=first1+w1
    bstart2=first2+al2
    bend2=first2+w2
    k=1
    do i=max(bstart1,bstart2),min(bend1,bend2,finish)
       a(k)=i-l1
       k=k+1
    end do
    jj=wd1-al1
    if(st1<=st2) then
       last1=((finish+al1-l1)/st1)*st1+l1-al1
       if(last1>first1) then
          do j=first1+st1,last1-1,st1
             if(bend2<j) then
                bend2=bend2+st2
             endif
             do i=max(j,bend2-w2),min(j+w1,bend2)
                a(k)=i-j+jj
                k=k+1
             end do
             jj=jj+wd1
          enddo
          j=last1
          if(bend2<j) then
             bend2=bend2+st2
          endif
          do i=max(j,bend2-w2),min(j+w1,bend2,finish)
             a(k)=i-j+jj
             k=k+1
          enddo
       endif
    else
       last2=((finish+al2-l2)/st2)*st2+l2-al2
       if(last2>first2) then
          do j=first2+st2,last2-1,st2
             if(bend1<j) then
                bend1=bend1+st1
                jj=jj+wd1
             endif
             do i=max(j,bend1-w1),min(j+w2,bend1)
                a(k)=i-(bend1-w1)+jj
                k=k+1
             end do
          enddo
          j=last2
          if(bend1<j) then
             bend1=bend1+st1
             jj=jj+wd1
          endif
          do i=max(j,bend1-w1),min(j+w2,bend1,finish)
             a(k)=i-(bend1-w1)+jj
             k=k+1
          enddo
       endif
    endif
    
    kk=k-1
    if(finish<actual_finish.and.kk>0) then
       span=actual_finish-l1+al1
       nb=span/st1
       jfinish=min(wd1,span-nb*st1)+nb*wd1-1-al1
       jcycle=jcycle*wd1
       j=jcycle
       do while(j+a(kk)<=jfinish)
          do i=1,kk
             write(*,*) 'doin',j,k,kk,a(i),a(i)+j,'jcycle=',jcycle
             a(k)=a(i)+j
             k=k+1
          enddo
          j=j+jcycle
       enddo
       if(j+a(1)<=jfinish) then
          i=1
          do while(a(i)+j<=jfinish)
             a(k)=a(i)+j
             k=k+1
             i=i+1
          enddo
       endif
    endif
    n=k-1
  end subroutine overlap_bseq

  subroutine overlap_bseq2(l1,h1,wd1,st1,al1,l2,h2,wd2,st2,al2,a1,a2,n)
    integer(pm_ln),intent(in):: l1,h1,wd1,st1,al1,l2,h2,wd2,st2,al2
    integer(pm_ln),dimension(*),intent(out):: a1,a2
    integer(pm_ln),intent(out):: n
    integer(pm_ln):: start,finish,actual_finish,first1,last1,first2,last2
    integer(pm_ln):: w1,w2,i,j,jj1,jj2,k,kk,bstart1,bend1,bstart2,bend2,u,v,gcd,jcycle
    integer(pm_ln):: span,nb,jfinish

    w1=wd1-1
    w2=wd2-1
    start=max(l1,l2)
    actual_finish=min(h1,h2)
    if(st1==st2) then
       finish=min(actual_finish,start+st1-1)
       gcd=st1
    else
       call extended_gcd(st1,st2,u,v,gcd)
       finish=min(actual_finish,start+st1*st2/gcd-1)
    endif
    first1=((start+al1-l1)/st1)*st1+l1-al1
    first2=((start+al2-l2)/st2)*st2+l2-al2
    bstart1=first1+al1
    bend1=first1+w1
    bstart2=first2+al2
    bend2=first2+w2
    k=1
    do i=max(bstart1,bstart2),min(bend1,bend2,finish)
       a1(k)=i-l1
       a2(k)=i-l2
       k=k+1
    end do
    jj1=wd1-al1
    jj2=-al2
    if(st1<=st2) then
       last1=((finish+al1-l1)/st1)*st1+l1-al1
       if(last1>first1) then
          do j=first1+st1,last1-1,st1
             if(bend2<j) then
                bend2=bend2+st2
                jj2=jj2+wd2
             endif
             do i=max(j,bend2-w2),min(j+w1,bend2)
                a1(k)=i-j+jj1
                a2(k)=i-(bend2-w2)+jj2
                k=k+1
             end do
             jj1=jj1+wd1
          enddo
          j=last1
          if(bend2<j) then
             bend2=bend2+st2
             jj2=jj2+wd2
          endif
          do i=max(j,bend2-w2),min(j+w1,bend2,finish)
             a1(k)=i-j+jj1
             a2(k)=i-(bend2-w2)+jj2
             k=k+1
          enddo
       endif
    else
       last2=((finish+al2-l2)/st2)*st2+l2-al2
       if(last2>first2) then
          do j=first2+st2,last2-1,st2
             jj2=jj2+wd2
             if(bend1<j) then
                bend1=bend1+st1
                jj1=jj1+wd1
             endif
             do i=max(j,bend1-w1),min(j+w2,bend1)
                a1(k)=i-(bend1-w1)+jj1
                a2(k)=i-j+jj2
                k=k+1
                jj2=jj2+wd2
             end do
          enddo
          j=last2
          if(bend1<j) then
             bend1=bend1+st1
             jj1=jj1+wd1
          endif
          do i=max(j,bend1-w1),min(j+w2,bend1,finish)
             a1(k)=i-(bend1-w1)+jj1
             a2(k)=i-j+jj2
             k=k+1
          enddo
       endif
    endif
    
    kk=k-1
    if(finish<actual_finish.and.kk>0) then
       span=actual_finish-l1+al1
       nb=span/st1
       jfinish=min(wd1,span-nb*st1)+nb*wd1-1-al1
       jcycle=wd1*st2/gcd
       j=jcycle
       do while(j+a1(kk)<=jfinish)
          do i=1,kk
             a1(k)=a1(i)+j
             k=k+1
          enddo
          j=j+jcycle
       enddo
       if(j+a1(1)<=jfinish) then
          i=1
          do while(a1(i)+j<=jfinish)
             a1(k)=a1(i)+j
             k=k+1
             i=i+1
          enddo
       endif
       k=kk+1
       span=actual_finish-l2+al2
       nb=span/st2
       jfinish=min(wd2,span-nb*st2)+nb*wd2-1-al2
       jcycle=st1*wd2/gcd
       j=jcycle
       do while(j+a2(kk)<=jfinish)
          do i=1,kk
             a2(k)=a2(i)+j
             k=k+1
          enddo
          j=j+jcycle
       enddo
       if(j+a2(1)<=jfinish) then
          i=1
          do while(a2(i)+j<=jfinish)
             a2(k)=a2(i)+j
             k=k+1
             i=i+1
          enddo
       endif
    endif
    
    n=k-1
  end subroutine overlap_bseq2

  subroutine expand_aseq(a1,n1,lo,hi,a2,n2)
    integer(pm_ln),dimension(*),intent(in):: a1
    integer(pm_ln),intent(in):: n1,lo,hi
    integer(pm_ln),dimension(*),intent(out):: a2
    integer(pm_ln),intent(out):: n2
    integer(pm_ln):: i,j,k,m,s
    if(n1==0) then
       n2=0
       return
    elseif(n1==1) then
       k=1
       do i=a1(1)+lo,a1(1)+hi
          a2(k)=i
          k=k+1
       enddo
    elseif(hi-lo>0) then
       k=1
       if(a1(1)<a1(2)) then
          do i=1,n1-1
             do j=a1(i)+lo,min(a1(i)+hi,a1(i+1)+lo-1)
                a2(k)=j
                k=k+1
             enddo
          enddo
          do j=a1(n1)+lo,a1(n1)+hi
             a2(k)=j
             k=k+1
          enddo
       else
          do i=1,n1-1
             do j=a1(i)+hi,max(a1(i)+lo,a1(i+1)+hi+1),-1
                a2(k)=j
                k=k+1
             enddo
          enddo
          do j=a1(n1)+hi,a1(n1)+lo,-1
             a2(k)=j
             k=k+1
          enddo
       endif
    else
       j=1
       k=1
       do while(j<n1)
          m=0
          s=a1(j)
          do while(a1(j+1)==a1(j)+1)
             m=m+1
             j=j+1
             if(j==n1) exit
          enddo
          do i=s+lo,s+m+hi
             a2(k)=i
             k=k+1
          enddo
          j=j+1
       enddo
    endif
    n2=k-1
  end subroutine expand_aseq

  subroutine interior_index_aseq(a1,lo,hi,a2,n)
    integer(pm_ln),dimension(*),intent(in):: a1
    integer(pm_ln),intent(in):: n,lo,hi
    integer(pm_ln),dimension(*),intent(out):: a2
    integer(pm_ln):: i,k
    k=-lo
    a2(1)=a1(i)+k
    do i=2,n
       k=k+min(a1(i)-a1(i-1),hi-lo)
       a2(i)=k
    enddo
  end subroutine interior_index_aseq
  
  subroutine intersect_seq(l1,u1,s1,n1,l2,u2,s2,n2,l3,u3,s3,n3)
    integer(pm_ln),intent(in):: l1,u1,s1,n1,l2,u2,s2,n2
    integer(pm_ln),intent(out):: l3,u3,s3,n3
    integer(pm_ln):: c,u,v,g,s,nlo,nhi,up1,up2,as1,as2,as3
    !write(*,*) 'intersect_seq>',l1,u1,s1,n1,l2,u2,s2,n2
    if(n1==0.or.n2==0) then
       l3=0
       u3=-1
       s3=1
       n3=0
       return
    endif
    as1=abs(s1)
    as2=abs(s2)
    c=l1-l2
    call extended_gcd(-as1,as2,u,v,g)
    if(mod(c,g)/=0) then
       l3=0
       u3=-1
       s3=1
       n3=0
       return
    endif
    up1=l1+(u1-l1)/s1*s1
    up2=l2+(u2-l2)/s2*s2
    s=c/g*u*s1+l1
    s3=(as1/abs(g))*s2
    as3=abs(s3)
    nlo=max(min(l1,up1),min(l2,up2))-s
    if(nlo>0) then
       nlo=(nlo-1)/as3+1
    else
       nlo=nlo/as3
    endif
    nhi=min(max(l1,up1),max(l2,up2))-s
    if(nhi<0) then
       nhi=(nhi+1)/as3-1
    else
       nhi=nhi/as3
    endif
    l3=s+nlo*as3
    u3=s+nhi*as3
    n3=nhi-nlo+1
    if(s3<0) then
       c=u3
       u3=l3
       l3=c
    endif
  end subroutine intersect_seq

  subroutine extended_gcd(a,b,u,v,g)
    integer(pm_ln),intent(in):: a,b
    integer(pm_ln),intent(out):: u,v,g
    integer(pm_ln):: s,old_s,r,old_r,t,old_t,temp,q
    s=0
    old_s=1
    t=1
    old_t=0
    r=b
    old_r=a
    
    do while(r/=0)
       q=old_r/r
       temp=old_r-q*r
       old_r=r
       r=temp
       temp=old_s-q*s
       old_s=s
       s=temp
       temp=old_t-q*t
       old_t=t
       t=temp
    enddo
    
    u=old_s
    v=old_t
    g=old_r
  end subroutine extended_gcd



  
end module pm_array
