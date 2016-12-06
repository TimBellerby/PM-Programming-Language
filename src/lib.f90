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
module pm_lib
  use pm_kinds
  use pm_memory
  implicit none
  private
  
  public:: pm_dict_new, pm_dict_lookup,&
       pm_dict_set,pm_dict_merge
  public:: pm_dict_keys,pm_dict_vals,pm_dict_key,&
       pm_dict_val,pm_dict_set_val,pm_dict_size
  public:: pm_set_new, pm_set_lookup, pm_set_add, &
       pm_set_keys,pm_set_key,pm_set_size,pm_set_merge
  public:: pm_ivect_lookup,pm_iset_add,pm_idict_add
  public:: pm_obj_set_new, pm_obj_set_lookup, pm_obj_set_add, &
       pm_obj_set_keys,pm_obj_set_key,pm_obj_set_size,pm_obj_set_from
  public:: pm_deep_copy
  
  integer(pm_p),public,parameter:: pm_first_libtype=pm_usr+1
  integer(pm_p),public,parameter:: pm_matched_type=pm_first_libtype
  integer(pm_p),public,parameter:: pm_dict_type = pm_first_libtype+1
  integer(pm_p),public,parameter:: pm_set_type = pm_first_libtype+2
  integer(pm_p),public,parameter:: pm_obj_set_type = pm_first_libtype+3
  integer(pm_p),public,parameter:: pm_prc_type = pm_first_libtype+4
  integer(pm_p),public,parameter:: pm_string_type=pm_first_libtype+5
  integer(pm_p),public,parameter:: pm_poly_type=pm_first_libtype+6
  integer(pm_p),public,parameter:: pm_struct_type=pm_first_libtype+7
  integer(pm_p),public,parameter:: pm_rec_type=pm_first_libtype+8
  integer(pm_p),public,parameter:: pm_polyref_type=pm_first_libtype+9
  integer(pm_p),public,parameter:: pm_array_type=pm_first_libtype+10
  integer(pm_p),public,parameter:: pm_const_array_type=pm_first_libtype+11
  integer(pm_p),public,parameter:: pm_elemref_type=pm_first_libtype+12

  integer(pm_p),public,parameter:: pm_last_libtype=pm_elemref_type
  
  integer,parameter:: max_count=1000

contains

  ! Create a new dictionary object
  function pm_dict_new(context,initsize) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_ln),intent(in):: initsize
    type(pm_ptr):: ptr
    type(pm_ptr):: p
    if(pm_debug_level>0) then
       if(initsize<0) call pm_panic('New dictionary size')
    endif
    ptr=pm_fast_newusr(context,pm_dict_type,4_pm_p)
    call hash_new_table(context,ptr,initsize,0_pm_ln)
    p=pm_assign_new(context,ptr,2_pm_ln, &
         pm_pointer,initsize,.true.)
    p=pm_assign_new(context,ptr,3_pm_ln, &
         pm_pointer,initsize,.true.)
  contains
    include 'fnewusr.inc'
  end function pm_dict_new

  ! Lookup value in dictionary object
  function pm_dict_lookup(context,obj,key,n_out) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj,key
    integer(pm_ln),intent(out),optional:: n_out
    type(pm_ptr):: ptr
    integer(pm_ln):: h,n
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Dict lookup - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_dict_type) &
            call pm_panic('Dict lookup - not dict type')
    endif
    h=pm_val_hash(context,key)
    n=hash_lookup(context,obj,key,h,test_obj_eq)
    if(n>0) then
       ptr=obj%data%ptr(obj%offset+3)
       ptr=ptr%data%ptr(ptr%offset+n-1)
    else
       ptr=pm_null_obj
    endif
    if(present(n_out)) n_out=n
  end function pm_dict_lookup

  ! Set value in dictionary object (optionally add / overwrite)
  subroutine pm_dict_set(context,obj,key,val,addit,overwrt,ok,m)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    type(pm_ptr),intent(in)::key,val
    logical,intent(in):: addit,overwrt
    integer(pm_ln),intent(out),optional:: m
    logical,intent(out):: ok
    type(pm_ptr):: ptr
    integer(pm_ln):: n,h,newsize
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Dict set - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_dict_type) &
            call pm_panic('Dict set - not dict type')
    endif
    ok=.true.
    h=pm_val_hash(context,key)
    n=hash_lookup(context,obj,key,h,test_obj_eq)
    if(n==0) then
       if(.not.addit) then
          ok=.false.
          return
       endif
       if(hash_full(obj)) then
          newsize=resize(context,obj)
          call pm_expand(context,obj,2_pm_ln,newsize)
          call pm_expand(context,obj,3_pm_ln,newsize)
       endif
       n=hash_add(context,obj,h)
       ptr=obj%data%ptr(obj%offset+2)
       call pm_ptr_assign(context,ptr,n-1,key)
    else
       if(.not.overwrt) then
          ok=.false.
          return
       endif
    endif
    if(present(m)) m=n
    ptr=obj%data%ptr(obj%offset+3)
    call pm_ptr_assign(context,ptr,n-1,val)
  end subroutine  pm_dict_set

  ! Merge two dictionary objects
  function pm_dict_merge(context,obj,obj2,overwrt) result(ok)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj,obj2
    logical,intent(in):: overwrt
    logical:: ok
    logical:: changed
    type(pm_ptr):: keys,vals
    integer(pm_ln):: i,siz,n
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Dict merge obj1 - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_dict_type) &
            call pm_panic('Dict merge obj1 - not dict type')
       if(obj2%data%vkind/=pm_usr) &
            call pm_panic('Dict merge obj2 - not ptr')
       if(obj2%data%ptr(obj2%offset)%offset/=pm_dict_type) &
            call pm_panic('Dict merge obj2 - not dict type')
    endif
    keys=pm_dict_keys(context,obj2)
    vals=pm_dict_vals(context,obj2)
    siz=pm_fast_esize(keys)
    do i=0_pm_ln,hash_size(obj2)-1_pm_ln
       call pm_dict_set(context,obj,&
            keys%data%ptr(keys%offset+i),&
            vals%data%ptr(vals%offset+i),&
            .true.,overwrt,changed) 
       if(changed) then
          ok=.false.
          return
       endif
    enddo
    ok=.true.
  contains
    include 'fesize.inc'
  end function pm_dict_merge

  ! Dictionary object keys
  function pm_dict_keys(context,obj) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Dict keys - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_dict_type) &
            call pm_panic('Dict keys - not dict type')
    endif
    ptr=obj%data%ptr(obj%offset+2)
  end function pm_dict_keys

  ! Dictionary object values
  function pm_dict_vals(context,obj) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Dict vals - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_dict_type) &
            call pm_panic('Dict vals - not dict type')
    endif
    ptr=obj%data%ptr(obj%offset+3)
  end function pm_dict_vals

  ! Dictionary object key
  function pm_dict_key(context,obj,n) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    integer(pm_ln),intent(in)::n
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Dict key - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_dict_type) &
            call pm_panic('Dict key - not dict type')
    endif
    ptr=obj%data%ptr(obj%offset+2)
    ptr=ptr%data%ptr(ptr%offset+n-1)
  end function pm_dict_key

  ! Dictionary object value
  function pm_dict_val(context,obj,n) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    integer(pm_ln),intent(in):: n
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Dict val - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_dict_type) &
            call pm_panic('Dict val - not dict type')
    endif
    ptr=obj%data%ptr(obj%offset+3)
    ptr=ptr%data%ptr(ptr%offset+n-1)
  end function pm_dict_val

  ! Set dictionary object value
  subroutine pm_dict_set_val(context,obj,n,val)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    integer(pm_ln),intent(in):: n
    type(pm_ptr),intent(in):: val
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Dict set val - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_dict_type) &
            call pm_panic('Dict set val - not dict type')
    endif
    ptr=obj%data%ptr(obj%offset+3)
    call pm_ptr_assign(context,ptr,n-1_pm_ln,val)
  end subroutine pm_dict_set_val

  ! Dictionary object size
  function pm_dict_size(context,obj) result(n)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    integer(pm_ln)::n
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Dict size - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_dict_type) &
            call pm_panic('Dict size - not dict type')
    endif
    n=hash_size(obj)
  end function pm_dict_size

  ! Create a new set object
  function pm_set_new(context,initsize) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_ln),intent(in):: initsize
    type(pm_ptr):: ptr
    type(pm_ptr):: p
    ptr=pm_fast_newusr(context,pm_set_type,3_pm_p)
    call hash_new_table(context,ptr,initsize,0_pm_ln)
    p=pm_assign_new(context,ptr,2_pm_ln, &
         pm_pointer,initsize,.true.)
  contains
    include 'fnewusr.inc'
  end function pm_set_new

  ! Lookup index of object in set object (0 - not there)
  function pm_set_lookup(context,obj,key) result(n)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj,key
    integer(pm_ln):: h,n
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Set lookup - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_set_type) &
            call pm_panic('Set lookuo - not set type')
    endif
    h=pm_val_hash(context,key)
    n=hash_lookup(context,obj,key,h,test_obj_eq)
  end function pm_set_lookup
  
  ! Add value to set object
  function pm_set_add(context,obj,key) result(n) 
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    type(pm_ptr),intent(in)::key
    type(pm_ptr):: ptr
    integer(pm_ln):: n,h,newsize
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Set add - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_set_type) &
            call pm_panic('Set add - not set type')
    endif
    h=pm_val_hash(context,key)
    n=hash_lookup(context,obj,key,h,test_obj_eq)
    if(n==0) then
       if(hash_full(obj)) then
          newsize=resize(context,obj)
          call pm_expand(context,obj,2_pm_ln,newsize)
       endif
       n=hash_add(context,obj,h)
       ptr=obj%data%ptr(obj%offset+2)
       call pm_ptr_assign(context,ptr,n-1,key)
    endif
  end function pm_set_add

  ! Create copy of set object
  function pm_set_copy(context,obj) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    type(pm_ptr):: ptr
    type(pm_ptr):: keys
    integer(pm_ln):: i,siz,n
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Set copy - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_set_type) &
            call pm_panic('Set copy - not set type')
    endif
    keys=pm_set_keys(context,obj)
    siz=pm_fast_esize(keys)
    ptr=pm_set_new(context,siz)
    do i=0_pm_ln,hash_size(obj)-1_pm_ln
       n=pm_set_add(context,ptr,keys%data%ptr(keys%offset+i))
    enddo
  contains
    include 'fnew.inc'
    include 'fesize.inc'
  end function pm_set_copy

  ! Merge one set into another
  subroutine pm_set_merge(context,obj,obj2)
    type(pm_context),pointer:: context
    type(pm_ptr):: obj,obj2
    type(pm_ptr):: keys
    integer(pm_ln):: i,siz,n
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Set merge obj1 - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_set_type) &
            call pm_panic('Set merge obj1 - not set type')
       if(obj2%data%vkind/=pm_usr) &
            call pm_panic('Set merge obj2 - not ptr')
       if(obj2%data%ptr(obj2%offset)%offset/=pm_set_type) &
            call pm_panic('Set merge obj2 - not set type')
    endif
    keys=pm_set_keys(context,obj2)
    siz=pm_fast_esize(keys)
    do i=0_pm_ln,hash_size(obj2)-1_pm_ln
       n=pm_set_add(context,obj,keys%data%ptr(keys%offset+i))
    enddo
  contains
    include 'fesize.inc'
  end subroutine pm_set_merge

  ! Calculate union of two sets
  function pm_set_union(context,obj,obj2) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj,obj2
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Set union obj1 - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_set_type) &
            call pm_panic('Set union obj1 - not set type')
       if(obj2%data%vkind/=pm_usr) &
            call pm_panic('Set union obj2 - not ptr')
       if(obj2%data%ptr(obj2%offset)%offset/=pm_set_type) &
            call pm_panic('Set union obj2 - not set type')
    endif
    ptr=pm_set_new(context,2*(max(&
         pm_fast_esize(obj%data%ptr(obj%offset+2)),&
         pm_fast_esize(obj2%data%ptr(obj2%offset+2))&
         )+1))
    call pm_set_merge(context,ptr,obj)
    call pm_set_merge(context,ptr,obj2)
  contains
    include 'fnew.inc'
    include 'fesize.inc'
  end function pm_set_union

  ! Keys of a set object
  function pm_set_keys(context,obj) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Set keys - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_set_type) &
            call pm_panic('Set keys - not set type')
    endif
    ptr=obj%data%ptr(obj%offset+2)
  end function pm_set_keys

  ! Keys of a set object
  function pm_set_key(context,obj,n) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    integer(pm_ln),intent(in):: n
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Set key  - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_set_type) &
            call pm_panic('Set key - not set type')
    endif
    ptr=obj%data%ptr(obj%offset+2)
    ptr=ptr%data%ptr(ptr%offset+n-1)
  end function pm_set_key

  ! Size of a set object
  function pm_set_size(context,obj) result(n)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    integer(pm_ln)::n
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Set size - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_set_type) &
            call pm_panic('Set size - not set type')
    endif
    n=hash_size(obj)
  end function pm_set_size

  ! Lookup in set/dictionary key of i16 values (diverse system uses)
  function pm_ivect_lookup(context,hashtab,ikey,m) result(k)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: hashtab
    integer,intent(in):: m
    integer(pm_ln):: k
    type(pm_ptr):: ptr
    integer(pm_i16),dimension(m):: ikey
    integer(pm_ln):: hash,p,kp
    type(pm_ptr):: tab,keys,vals,lkey
    integer:: i
    if(pm_debug_level>0) then
       if(hashtab%data%vkind/=pm_usr) &
            call pm_panic('Ivect lookup - not use')
       if(hashtab%data%ptr(hashtab%offset)%offset/=pm_dict_type.and.&
            hashtab%data%ptr(hashtab%offset)%offset/=pm_set_type) &
            call pm_panic('Ivect lookup - not set/dict type')
    endif
    hash=0
    do i=1,m
       hash=hashfn(hash,int(ikey(i),pm_ln))
    enddo
    tab=hashtab%data%ptr(hashtab%offset+1)
    keys=hashtab%data%ptr(hashtab%offset+2)
    p=iand(hash,tab%data%ln(tab%offset+2))*3+3
    do
       if(tab%data%ln(tab%offset+p+2)==0) then
          k=0
          return
       else if(tab%data%ln(tab%offset+p+1)==hash) then
          kp=tab%data%ln(tab%offset+p+2)
          lkey=keys%data%ptr(keys%offset+kp-1)
          if(pm_fast_esize(lkey)==m-1) then
             if(all(lkey%data%i16(lkey%offset:lkey%offset+m-1)== &
                  ikey(1:m))) then
                vals=hashtab%data%ptr(hashtab%offset+3)
                k=kp
                return
             endif
          endif
       endif
       p=tab%data%ln(tab%offset+p)
       if(p==0) then
          k=0
          return
       endif
    enddo
  contains
    include 'fesize.inc'
  end function pm_ivect_lookup

  ! Add an entry with integer vector key
  function pm_iset_add(context,obj,key,m) result(k)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    integer(pm_i16),dimension(m),intent(in):: key
    integer,intent(in):: m
    integer(pm_ln):: k
    type(pm_ptr):: ptr
    type(pm_root),pointer:: root
    logical:: ok
    k=pm_ivect_lookup(context,obj,key,m)
    if(k>0) return
    ptr=pm_fast_newnc(context,pm_int16,int(m,pm_p))
    ptr%data%i16(ptr%offset:ptr%offset+m-1)=key(1:m)
    root=>pm_add_root(context,ptr)
    k=pm_set_add(context,obj,ptr)
    call pm_delete_root(context,root)
  contains
    include 'fnewnc.inc'
  end function pm_iset_add

  ! Add an entry with integer vector key
  function pm_idict_add(context,obj,key,m,val) result(k)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj,val
    integer(pm_i16),dimension(m),intent(in):: key
    integer,intent(in):: m
    integer(pm_ln):: k
    type(pm_ptr):: ptr
    type(pm_root),pointer:: root
    logical:: ok
    ptr=pm_fast_newnc(context,pm_int16,int(m,pm_p))
    ptr%data%i16(ptr%offset:ptr%offset+m-1)=key(1:m)
    root=>pm_add_root(context,ptr)
    call pm_dict_set(context,obj,ptr,val,.true.,.true.,ok,k)
    call pm_delete_root(context,root)
  contains
    include 'fnewnc.inc'
  end function pm_idict_add

 ! Create a new object-value dictionary
  function pm_obj_set_new(context,initsize) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_ln),intent(in):: initsize
    type(pm_ptr):: ptr
    type(pm_ptr):: p
    if(pm_debug_level>0) then
       if(initsize<0) call pm_panic('New obj dictionary size')
    endif
    ptr=pm_fast_newusr(context,pm_obj_set_type,3_pm_p)
    call hash_new_table(context,ptr,initsize,0_pm_ln)
    p=pm_assign_new(context,ptr,2_pm_ln, &
         pm_pointer,initsize,.true.)
  contains
    include 'fnewusr.inc'
  end function pm_obj_set_new

  ! Lookup value in object-value dictionary
  function pm_obj_set_lookup(context,obj,key) result(n)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj,key
    integer(pm_ln):: n
    type(pm_ptr):: ptr
    integer(pm_ln):: h
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Obj dict lookup - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_obj_set_type) &
            call pm_panic('Obj dict lookup - not obj set type')
    endif
    h=pm_obj_hash(context,key)
    n=hash_lookup(context,obj,key,h,test_same_obj)
  end function pm_obj_set_lookup

  ! Add value to object/value dictionary - return index
  function pm_obj_set_add(context,obj,key) result(n)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    type(pm_ptr),intent(in)::key
    integer(pm_ln):: n
    type(pm_ptr):: ptr
    integer(pm_ln):: h,newsize
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Obj set add - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_obj_set_type) &
            call pm_panic('Obj set add - not obj set type')
    endif
    if(hash_full(obj)) then
       newsize=resize(context,obj)
       call pm_expand(context,obj,2_pm_ln,newsize)
    endif
    h=pm_obj_hash(context,key)
    n=hash_add(context,obj,h)
    ptr=obj%data%ptr(obj%offset+2_pm_p)
    call pm_ptr_assign(context,ptr,n-1_pm_ln,key)
  end function pm_obj_set_add

  ! Size of a set object
  function pm_obj_set_size(context,obj) result(n)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    integer(pm_ln)::n
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Obj set size - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_obj_set_type) &
            call pm_panic('Obj set size - not set type')
    endif
    n=hash_size(obj)
  end function pm_obj_set_size

  ! Keys of an object set
  function pm_obj_set_keys(context,obj) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Obj set keys - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_obj_set_type) &
            call pm_panic('Obj set keys - not obj set type')
    endif
    ptr=obj%data%ptr(obj%offset+2)
  end function pm_obj_set_keys

 ! Keys of a set object
  function pm_obj_set_key(context,obj,n) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: obj
    integer(pm_ln),intent(in):: n
    type(pm_ptr):: ptr
    if(pm_debug_level>0) then
       if(obj%data%vkind/=pm_usr) &
            call pm_panic('Obj set key - not ptr')
       if(obj%data%ptr(obj%offset)%offset/=pm_obj_set_type) &
            call pm_panic('Obj Set key - not obj set type')
    endif
    ptr=obj%data%ptr(obj%offset+2)
    ptr=ptr%data%ptr(ptr%offset+n-1)
  end function pm_obj_set_key

  ! Create object set of all nodes following from ptr
  function pm_obj_set_from(context,ptr) result(objset)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ptr
    type(pm_ptr):: objset
    type(pm_ptr):: p,q
    integer(pm_ln):: i,n,m
    type(pm_root),pointer:: root
    p=pm_obj_set_new(context,32_pm_ln)
    root=>pm_add_root(context,p)
    m=pm_obj_set_add(context,root%ptr,ptr)
    n=0
    do while(n<m)
       n=n+1
       p=pm_obj_set_key(context,root%ptr,n)
       if(pm_fast_vkind(p)>=pm_pointer) then
          do i=0,pm_fast_esize(p)
             q=p%data%ptr(p%offset+i)
             if(pm_obj_set_lookup(context,root%ptr,q)<=0) then
                m=pm_obj_set_add(context,root%ptr,q)
             endif
          enddo
       endif
    enddo
    objset=root%ptr
    call pm_delete_root(context,root)
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end function pm_obj_set_from

  ! Deep copy based on object hash
  function pm_deep_copy(context,ptr) result(copy)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ptr
    type(pm_ptr):: copy
    integer(pm_ln):: i,j,n
    type(pm_ptr),target:: dict,q,r,pending
    type(pm_ptr):: p,keys
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'deep copy',dict,p,q,pending)
    dict=pm_obj_set_from(context,ptr)
    keys=pm_obj_set_keys(context,dict)
    n=pm_obj_set_size(context,dict)
    q=pm_new(context,pm_pointer,n)
    do i=0,n-1
       r=keys%data%ptr(keys%offset+i)
       q%data%ptr(q%offset+i)=pm_copy(context,r)
    enddo
    do i=0,n-1
       r=q%data%ptr(q%offset+i)
       if(pm_fast_vkind(r)>=pm_pointer) then
          do j=0,pm_fast_esize(r)
             r%data%ptr(r%offset+j)=&
                  q%data%ptr(q%offset+&
                  pm_obj_set_lookup(context,dict,r%data%ptr(r%offset+j))-1_pm_ln)
          enddo
       endif
    enddo
    copy=q%data%ptr(q%offset)
    call pm_delete_register(context,reg)
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'
  end function pm_deep_copy

  ! Hash table size
  function hash_size(obj) result(n)
    type(pm_ptr),intent(in):: obj
    integer(pm_ln):: n
    type(pm_ptr):: hashtab
    hashtab=obj%data%ptr(obj%offset+1)
    n=hashtab%data%ln(hashtab%offset)    
  end function hash_size

  ! New hash table
  subroutine hash_new_table(context,hashtab,initsize,initkp)
    type(pm_context),pointer:: context
    type(pm_ptr):: hashtab
    integer(pm_ln),intent(in):: initsize,initkp
    type(pm_ptr):: ptr
    ptr=pm_assign_new(context,hashtab,1_pm_ln, &
         pm_long,initsize*3+3,.true.)
    ptr%data%ln(ptr%offset)=initkp
    ptr%data%ln(ptr%offset+1)=3
    ptr%data%ln(ptr%offset+2)=initsize-1
  end subroutine hash_new_table

  ! Is hash table full
  function hash_full(ptr) result(full)
    type(pm_ptr),intent(in):: ptr
    logical:: full
    type(pm_ptr):: table
    table=ptr%data%ptr(ptr%offset+1)
    full=table%data%ln(table%offset)>&
         table%data%ln(table%offset+2)/2
  end function hash_full

  ! Add a new key to a hash table
  function hash_add(context,hashtable,hash) result(kp)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in)::hashtable
    integer(pm_ln),intent(in):: hash
    type(pm_ptr):: hashtab,tab
    integer(pm_ln):: p,p2,p3,mask,kp
    
    hashtab=hashtable
    tab=hashtab%data%ptr(hashtab%offset+1)
    mask=tab%data%ln(tab%offset+2)
    p=iand(hash,mask)*3+3
    if(tab%data%ln(tab%offset+p+2)/=0) then
       p2=iand(tab%data%ln(tab%offset+p+1),mask)*3+3
       if(p==p2) then
          p=free_pos(tab)
          tab%data%ln(tab%offset+p)=tab%data%ln(tab%offset+p2)
          tab%data%ln(tab%offset+p2)=p
       else
          do while(tab%data%ln(tab%offset+p2)/=p)
             p2=tab%data%ln(tab%offset+p2)
          enddo
          p3=free_pos(tab)
          tab%data%ln(tab%offset+p3)=tab%data%ln(tab%offset+p)
          tab%data%ln(tab%offset+p3+1)=tab%data%ln(tab%offset+p+1)
          tab%data%ln(tab%offset+p3+2)=tab%data%ln(tab%offset+p+2)
          tab%data%ln(tab%offset+p2)=p3
          tab%data%ln(tab%offset+p)=0
       endif
    else
       tab%data%ln(tab%offset+p)=0
    endif
    tab%data%ln(tab%offset+p+1)=hash
    kp=tab%data%ln(tab%offset)+1
    tab%data%ln(tab%offset)=kp
    tab%data%ln(tab%offset+p+2)=kp
  end function  hash_add

  ! Lookup position in hash table
  function hash_lookup(context,hashtab,obj,hash,eq_func) &
       result(kp)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: hashtab,obj
    integer(pm_ln),intent(in):: hash
    logical,external:: eq_func
    integer(pm_ln):: kp
    integer(pm_ln):: p
    type(pm_ptr):: tab
    tab=hashtab%data%ptr(hashtab%offset+1)
    p=iand(hash,tab%data%ln(tab%offset+2))*3+3
    do
       if(tab%data%ln(tab%offset+p+2)==0) then
          kp=0
          return
       else if(tab%data%ln(tab%offset+p+1)==hash) then
          kp=tab%data%ln(tab%offset+p+2)
          if(eq_func(hashtab,kp,obj)) then
             return
          endif
       endif
       p=tab%data%ln(tab%offset+p)
       if(p==0) then
          kp=0
          return
       endif
    enddo
  end function hash_lookup

  ! Resize table
  function resize(context,htab) result(newsize)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in)::htab
    integer(pm_ln):: newsize,oldkp
    type(pm_ptr):: newtab
    integer(pm_ln):: i,oldsize
    type(pm_root),pointer:: tab
    if(pm_debug_level>2) write(*,*) 'Resizing'
    tab=>pm_get_ptr_as_root(context,htab,1_pm_ln)
    oldkp=tab%ptr%data%ln(tab%ptr%offset)
    newsize=(tab%ptr%data%ln(tab%ptr%offset+2)+1)*2
    call hash_new_table(context,htab,newsize,oldkp)
    newtab=htab%data%ptr(htab%offset+1)
    do i=3,pm_fast_esize(tab%ptr),3
       if(tab%ptr%data%ln(tab%ptr%offset+i+2)>0) then
          call add_again(newtab,&
               tab%ptr%data%ln(tab%ptr%offset+i+1),&
               tab%ptr%data%ln(tab%ptr%offset+i+2),newsize-1)
       endif
    enddo
    call pm_delete_root(context,tab)
  contains
    include 'fesize.inc'
  end function resize
  
  ! Add an entry into resized table
  subroutine add_again(tab,hash,kp,mask)
    type(pm_ptr),intent(in)::tab
    integer(pm_ln),intent(in):: hash,kp
    integer(pm_ln):: p,p2,p3,mask

    p=iand(hash,mask)*3+3
    if(tab%data%ln(tab%offset+p+2)/=0) then
       p2=iand(tab%data%ln(tab%offset+p+1),mask)*3+3
       if(p==p2) then
          p=free_pos(tab)
          tab%data%ln(tab%offset+p)=tab%data%ln(tab%offset+p2)
          tab%data%ln(tab%offset+p2)=p
       else
          do while(tab%data%ln(tab%offset+p2)/=p)
             p2=tab%data%ln(tab%offset+p2)
          enddo
          p3=free_pos(tab)
          tab%data%ln(tab%offset+p3)=tab%data%ln(tab%offset+p)
          tab%data%ln(tab%offset+p3+1)=tab%data%ln(tab%offset+p+1)
          tab%data%ln(tab%offset+p3+2)=tab%data%ln(tab%offset+p+2)
          tab%data%ln(tab%offset+p2)=p3
          tab%data%ln(tab%offset+p)=0
       endif
    else
       tab%data%ln(tab%offset+p)=0
    endif
    tab%data%ln(tab%offset+p+1)=hash
    tab%data%ln(tab%offset+p+2)=kp
  end subroutine  add_again

  ! Find free position
  function free_pos(tab) result(pos)
    type(pm_ptr):: tab
    integer(pm_ln):: pos
    pos=tab%data%ln(tab%offset+1)
    do while(tab%data%ln(tab%offset+pos+2)>0)
       pos=pos+3
    enddo
    tab%data%ln(tab%offset+1)=pos+3
  end function free_pos

  ! Object equality
  function test_obj_eq(hashtab,n,obj) result(eq)
    type(pm_ptr):: hashtab,obj
    integer(pm_ln):: n
    logical:: eq
    type(pm_ptr):: keys
    integer:: count
    keys=hashtab%data%ptr(hashtab%offset+2)
    count=0
    eq=obj_eq(keys%data%ptr(keys%offset+n-1),obj,count)
  end function test_obj_eq

  ! Object value equality test
  function pm_obj_eq(ptr,ptr2) result(eq)
    type(pm_ptr)::ptr,ptr2
    logical:: eq
    integer:: count
    count=0
    eq=obj_eq(ptr,ptr2,count)
  end function pm_obj_eq

  ! Recursive implementation of equality test
  recursive function obj_eq(ptr,ptr2,count) result(eq)
    type(pm_ptr)::ptr,ptr2
    integer,intent(inout):: count
    logical:: eq
    integer(pm_ln):: esize,i
    
    if(count> max_count) then
       eq=.false.
       return
    endif
    count=count+1
    if(associated(ptr%data,ptr2%data).and.&
         ptr%offset==ptr2%offset) then
       eq=.true.
       return
    else if(pm_fast_vkind(ptr)/=pm_fast_vkind(ptr2)) then
       eq=.false.
       return
    else if(pm_fast_esize(ptr)/=pm_fast_esize(ptr2)) then
       eq=.false.
       return
    endif
    esize=pm_fast_esize(ptr)
    ! Copy over data between objects
    select case(pm_fast_vkind(ptr))
    case(pm_undef:pm_null)
       eq=ptr%offset==ptr2%offset
    case(pm_int)
       eq=all(ptr2%data%i(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%i(ptr%offset:ptr%offset+esize))
    case(pm_long)
       eq=all(ptr2%data%ln(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%ln(ptr%offset:ptr%offset+esize))
    case(pm_int8)
       eq=all(ptr2%data%i8(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%i8(ptr%offset:ptr%offset+esize))
    case(pm_int16)
       eq=all(ptr2%data%i16(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%i16(ptr%offset:ptr%offset+esize))
    case(pm_int32)
       eq=all(ptr2%data%i32(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%i32(ptr%offset:ptr%offset+esize))
    case(pm_int64)
       eq=all(ptr2%data%i64(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%i64(ptr%offset:ptr%offset+esize))
    case(pm_int128)
       eq=all(ptr2%data%i128(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%i128(ptr%offset:ptr%offset+esize))
    case(pm_single)
       eq=all(ptr2%data%r(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%r(ptr%offset:ptr%offset+esize))
    case(pm_double)
       eq=all(ptr2%data%d(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%d(ptr%offset:ptr%offset+esize))
    case(pm_real32)
       eq=all(ptr2%data%r32(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%r32(ptr%offset:ptr%offset+esize))
    case(pm_real64)
       eq=all(ptr2%data%r64(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%r64(ptr%offset:ptr%offset+esize))
    case(pm_real128)
       eq=all(ptr2%data%r128(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%r128(ptr%offset:ptr%offset+esize))
    case(pm_single_complex)
       eq=all(ptr2%data%c(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%c(ptr%offset:ptr%offset+esize))
    case(pm_double_complex)
       eq=all(ptr2%data%dc(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%dc(ptr%offset:ptr%offset+esize))
    case(pm_complex64)
       eq=all(ptr2%data%c64(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%c64(ptr%offset:ptr%offset+esize))
    case(pm_complex128)
       eq=all(ptr2%data%c128(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%c128(ptr%offset:ptr%offset+esize))
    case(pm_complex256)
       eq=all(ptr2%data%c256(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%c256(ptr%offset:ptr%offset+esize))
    case(pm_logical)
       eq=all(ptr2%data%l(ptr2%offset:ptr2%offset+esize).eqv. &
            ptr%data%l(ptr%offset:ptr%offset+esize))
    case(pm_packed_logical)
       eq=all(ptr2%data%pl(ptr2%offset:ptr2%offset+esize).eqv. &
            ptr%data%pl(ptr%offset:ptr%offset+esize))
    case(pm_string)
       eq=all(ptr2%data%s(ptr2%offset:ptr2%offset+esize)== &
            ptr%data%s(ptr%offset:ptr%offset+esize))
    case(pm_pointer:pm_usr)
       do i=0,esize-1
          if(.not.obj_eq(ptr%data%ptr(ptr%offset+i), &
               ptr2%data%ptr(ptr2%offset+i),count)) then
             eq=.false.
             return
          endif
       enddo
       eq=.true.
    case default
       stop 'Bad vkind on obj_eq'
    end select
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end function obj_eq

  ! Value hash function
  function pm_val_hash(context,ptr) result(h)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ptr
    integer(pm_ln)::h
    integer:: count
    count=0
    h=val_hash(context,ptr,count)
  end function pm_val_hash

  ! Value hash function - recursive implementation
  recursive function val_hash(context,ptr,count) result(h)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ptr
    integer,intent(inout):: count
    integer(pm_ln):: h
    integer(pm_ln):: v,esize,i
    integer(pm_ln),parameter:: bignum= huge(h)
    integer,parameter:: cbits=16
    integer:: vkind
    
    h=0
    if(count>max_count) return
    count=count+1
    esize=pm_fast_esize(ptr)
    vkind=pm_fast_vkind(ptr)
    select case(vkind)
    case(pm_undef:pm_null)
       v=ptr%offset
       h=hashfn(h,v)
    case(pm_int)
       do i=ptr%offset,ptr%offset+esize
          v=ptr%data%i(i)
          h=hashfn(h,v)
       enddo
    case(pm_int8)
       do i=ptr%offset,ptr%offset+esize
          v=ptr%data%i8(i)
          h=hashfn(h,v)
       enddo
    case(pm_int16)
       do i=ptr%offset,ptr%offset+esize
          v=ptr%data%i16(i)
          h=hashfn(h,v)
       enddo
    case(pm_int32)
       do i=ptr%offset,ptr%offset+esize
          v=ptr%data%i32(i)
          h=hashfn(h,v)
       enddo
    case(pm_int64)
       do i=ptr%offset,ptr%offset+esize
          v=ptr%data%i64(i)
          h=hashfn(h,v)
       enddo
    case(pm_int128)
       do i=ptr%offset,ptr%offset+esize
          v=ptr%data%i128(i)
          h=hashfn(h,v)
       enddo
    case(pm_single)
       do i=ptr%offset,ptr%offset+esize
          v=ishftc(int(fraction(ptr%data%r(i))*bignum,pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(ptr%data%r(i)),pm_ln)
          h=hashfn(h,v)
       enddo
    case(pm_double)
       do i=ptr%offset,ptr%offset+esize
          v=ishftc(int(fraction(ptr%data%d(i))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(ptr%data%d(i)),pm_ln)
          h=hashfn(h,v)
       enddo
    case(pm_real32)
       do i=ptr%offset,ptr%offset+esize
          v=ishftc(int(fraction(ptr%data%r32(i))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(ptr%data%r32(i)),pm_ln)
          h=hashfn(h,v)
       enddo
    case(pm_real64)
       do i=ptr%offset,ptr%offset+esize
          v=ishftc(int(fraction(ptr%data%r64(i))*bignum,pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(ptr%data%r64(i)),pm_ln)
          h=hashfn(h,v)
       enddo
    case(pm_real128)
       do i=ptr%offset,ptr%offset+esize
          v=ishftc(int(fraction(ptr%data%r128(i))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(ptr%data%r128(i)),pm_ln)
          h=hashfn(h,v)
       enddo
    case(pm_single_complex)
       do i=ptr%offset,ptr%offset+esize
          v=ishftc(int(fraction(real(ptr%data%c(i)))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(real(ptr%data%c(i))),pm_ln)
          h=hashfn(h,v)
          v=ishftc(int(fraction(aimag(ptr%data%c(i)))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(aimag(ptr%data%c(i))),pm_ln)
          h=hashfn(h,v)
       enddo
    case(pm_double_complex)
       do i=ptr%offset,ptr%offset+esize
          v=ishftc(int(fraction(real(ptr%data%dc(i)))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(real(ptr%data%dc(i))),pm_ln)
          h=hashfn(h,v)
          v=ishftc(int(fraction(aimag(ptr%data%dc(i)))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(aimag(ptr%data%dc(i))),pm_ln)
          h=hashfn(h,v)
       enddo
    case(pm_complex64)
       do i=ptr%offset,ptr%offset+esize
          v=ishftc(int(fraction(real(ptr%data%c64(i)))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(real(ptr%data%c64(i))),pm_ln)
          h=hashfn(h,v)
          v=ishftc(int(fraction(aimag(ptr%data%c64(i)))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(aimag(ptr%data%c64(i))),pm_ln)
          h=hashfn(h,v)
       enddo
    case(pm_complex128)
       do i=ptr%offset,ptr%offset+esize
          v=ishftc(int(fraction(real(ptr%data%c128(i)))&
               *bignum,pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(real(ptr%data%c128(i))),pm_ln)
          h=hashfn(h,v)
          v=ishftc(int(fraction(aimag(ptr%data%c128(i)))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(aimag(ptr%data%c128(i))),pm_ln)
          h=hashfn(h,v)
       enddo
    case(pm_complex256)
       do i=ptr%offset,ptr%offset+esize
          v=ishftc(int(fraction(real(ptr%data%c256(i)))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(real(ptr%data%c256(i))),pm_ln)
          h=hashfn(h,v)
          v=ishftc(int(fraction(aimag(ptr%data%c256(i)))*bignum,&
               pm_ln),cbits)
          h=hashfn(h,v)
          v=int(exponent(aimag(ptr%data%c256(i))),pm_ln)
          h=hashfn(h,v)
       enddo
    case(pm_logical)
      do i=ptr%offset,ptr%offset+esize
         if(ptr%data%l(i)) h=ieor(h,1_pm_ln)
         h=ishftc(h,1)
      enddo
    case(pm_packed_logical)
      do i=ptr%offset,ptr%offset+esize
         if(ptr%data%l(i)) h=ieor(h,1_pm_ln)
         h=ishftc(h,1)
      enddo
    case(pm_string)
       do i=ptr%offset,ptr%offset+esize
          v=ichar(ptr%data%s(i))
          h=hashfn(h,v)
       enddo
    case(pm_pointer:pm_usr)
       do i=ptr%offset,ptr%offset+esize
          v=val_hash(context,ptr%data%ptr(i),count)
          h=hashfn(h,v)
       enddo
    end select
  contains
    include 'fvkind.inc'
    include 'fesize.inc'

  end function val_hash

  ! Same object
  function test_same_obj(hashtab,n,obj) result(eq)
    type(pm_ptr):: hashtab,obj
    integer(pm_ln):: n
    logical:: eq
    type(pm_ptr):: keys
    integer:: count
    keys=hashtab%data%ptr(hashtab%offset+2)
    count=0
    eq=keys%data%ptr(keys%offset+n-1)==obj
  end function test_same_obj

  ! Hash value for object
  function pm_obj_hash(context,ptr) result(h)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: ptr
    integer(pm_ln)::h
    h=hashfn(ptr%data%hash,int(ptr%offset,pm_ln))
  end function pm_obj_hash

  ! Hash function
  function hashfn(oldhash,term) result(newhash)
    integer(pm_ln),intent(in):: oldhash,term
    integer(pm_ln):: newhash
    newhash=oldhash+term
    newhash=newhash+ishftc(newhash,11)
    newhash=ieor(newhash,ishftc(newhash,-6))
    newhash=newhash+ishftc(newhash,15)
  end function hashfn


end module pm_lib
