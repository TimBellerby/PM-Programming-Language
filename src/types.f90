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
module pm_types
  use pm_kinds
  use pm_memory
  use pm_lib
  implicit none

  integer(pm_i16),parameter:: pm_typ_is_user=1
  integer(pm_i16),parameter:: pm_typ_is_any=2
  integer(pm_i16),parameter:: pm_typ_is_struct=3
  integer(pm_i16),parameter:: pm_typ_is_rec=4
  integer(pm_i16),parameter:: pm_typ_is_array=5
  integer(pm_i16),parameter:: pm_typ_is_null=6
  integer(pm_i16),parameter:: pm_typ_is_tuple=7
  integer(pm_i16),parameter:: pm_typ_is_vtuple=8
  integer(pm_i16),parameter:: pm_typ_is_single_name=9
  integer(pm_i16),parameter:: pm_typ_is_single_proc=10
  integer(pm_i16),parameter:: pm_typ_is_ambig=11
  integer(pm_i16),parameter:: include_cache=13
  integer(pm_i16),parameter:: intersect_cache=14
  
  integer,parameter:: pm_typ_equal=0
  integer,parameter:: pm_typ_contains=1
  integer,parameter:: pm_typ_contained=2
  integer,parameter:: pm_typ_disjoint=3
  integer,parameter:: pm_typ_conjoint=4

  integer,parameter:: pm_tset_not_contained=0
  integer,parameter:: pm_tset_part_contained=1
  integer,parameter:: pm_tset_fully_contained=2
  
contains

  subroutine init_typ(context)
    type(pm_context),pointer:: context
    integer:: i,j
    integer(pm_i16),dimension(2):: key
    character(len=12),dimension(pm_last_libtype),parameter:: base_types= (/&
    '<tiny>     ','type       ','proc       ','name       ',&
    'null       ','int        ','long       ','int8       ',&
    'int16      ','int32      ','int64      ','int128     ',&
    'single     ','real       ','real32     ','real64     ',&
    'real128    ','single_cpx ','cpx        ','cpx64      ',&
    'cpx128     ','cpx256     ','bool       ','<packbool> ',&
    'string     ','<ext>      ','<pointer>  ','<stack>    ',&
    '<usr>      ','<matched>  ','<dict>     ','<set>      ',&
    '<obj_set>  ','<poly>     ','<polygroup>','index      '&
    /)
    context%tcache=pm_dict_new(context,128_pm_ln)
    key(1)=pm_typ_is_user
    do i=1,pm_last_libtype
       key(2)=pm_intern(context,trim(base_types(i)))
       if(pm_debug_level>2) write(*,*) 'Init types(',trim(base_types(i)),')',key(2)
       j=pm_idict_add(context,context%tcache,key,2,pm_null_obj)
       if(j/=i) call pm_panic('init_typ')
    enddo
    if(pm_debug_level>2) write(*,*) 'Types inited'
  end subroutine init_typ

  function pm_typ_includes(context,supertype,subtype)&
       result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: supertype,subtype
    logical:: ok
    integer(pm_i16),dimension(3):: key
    type(pm_ptr):: p
    integer(pm_ln):: j
    key(1)=include_cache
    key(2)=supertype
    key(3)=subtype
    j=pm_ivect_lookup(context,context%tcache,key,3)
    if(j>0) then
       p=pm_dict_key(context,context%tcache,j)
       ok=p%offset/=0
    else
       if(.not.associated(context%heap,context)) then
          j=pm_ivect_lookup(context,context%heap%tcache,key,3)
          if(j>0) then
             p=pm_dict_key(context,context%heap%tcache,j)
             ok=p%offset/=0
             j=pm_idict_add(context,context%tcache,key,3,p)
             return
          endif
       endif
       ok=pm_test_typ_includes(context,supertype,subtype)
       if(ok) then
          p=pm_fast_tinyint(context,1_pm_p)
       else
          p=pm_fast_tinyint(context,0_pm_p)
       endif
       j=pm_idict_add(context,context%tcache,key,3,p)
    endif
  contains
    include 'ftiny.inc'
  end function pm_typ_includes
  
  function pm_typ_intersects(context,supertype,subtype)&
       result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: supertype,subtype
    logical:: ok
    integer(pm_i16),dimension(3):: key
    type(pm_ptr):: p
    integer(pm_ln):: j
    key(1)=intersect_cache
    key(2)=supertype
    key(3)=subtype
    j=pm_ivect_lookup(context,context%tcache,key,3)
    if(j>0) then
       p=pm_dict_key(context,context%tcache,j)
       ok=p%offset/=0
    else
       if(.not.associated(context%heap,context)) then
          j=pm_ivect_lookup(context,context%heap%tcache,key,3)
          if(j>0) then
             p=pm_dict_key(context,context%heap%tcache,j)
             ok=p%offset/=0
             j=pm_idict_add(context,context%tcache,key,3,p)
             return
          endif
       endif
       ok=pm_test_typ_intersects(context,supertype,subtype)
       if(ok) then
          p=pm_fast_tinyint(context,1_pm_p)
       else
          p=pm_fast_tinyint(context,0_pm_p)
       endif
       j=pm_idict_add(context,context%tcache,key,3,p)
    endif
  contains
    include 'ftiny.inc'
  end function pm_typ_intersects

  recursive function pm_test_typ_includes(context,supertype,subtype)&
       result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: supertype,subtype
    logical:: ok
    integer(pm_i16):: p,q,s
    type(pm_ptr):: t,u,r
    integer:: i,j,tk,uk,nt,nu,usr_index
    p=supertype
    q=subtype
    if(p==q) then
       ok=.true.
       return
    elseif(p==0.or.q==0) then
       ok=(p==0)
       return
    endif
    t=pm_typ_vect(context,p)
    u=pm_typ_vect(context,q)
    uk=pm_tv_kind(u)
    if(uk==pm_typ_is_single_proc.and.p==pm_proc) then
       ok=.true.
       return
    else if(uk==pm_typ_is_single_name.and.p==pm_name) then
       ok=.true.
       return
    else if(uk==pm_typ_is_tuple.and.p==pm_matched_type) then
       if(pm_tv_numargs(u)==2) then
          ok=pm_tv_arg(u,1)==pm_tv_arg(u,2)
       else
          ok=.false.
       endif
       return
    else if(uk==pm_typ_is_ambig) then
       ok=pm_typ_includes(context,p,pm_tv_arg(u,1)).and.&
            pm_typ_includes(context,p,pm_tv_arg(u,2))
       return
    endif
    tk=pm_tv_kind(t)
    select case(tk)
    case(pm_typ_is_struct,pm_typ_is_rec)
       if(tk/=uk) then
          ok=.false.
          return
       endif
       if(.not.pm_tv_name(t)==pm_tv_name(u)) then
          ok=.false.
          return
       endif
       do i=1,pm_tv_numargs(t)
          if(.not.pm_typ_includes(context,pm_tv_arg(t,i),&
               pm_tv_arg(u,i))) then
             ok=.false.
             exit
          endif
          ok=.true.
       enddo
    case(pm_typ_is_null)
       if(uk==pm_typ_is_null) then
          ok=pm_typ_includes(context,&
               pm_tv_arg(t,1),pm_tv_arg(u,1))
       else
          ok=.false.
       endif
    case(pm_typ_is_any)
       if(uk==pm_typ_is_any) then
          ok=pm_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1))
       else
          ok=.false.
       endif
    case(pm_typ_is_array)
       if(uk/=pm_typ_is_array) then
          ok=.false.
       else
          ok=pm_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1)).and.&
               pm_typ_includes(context,pm_tv_arg(t,2),pm_tv_arg(u,2))
       endif
    case(pm_typ_is_tuple,pm_typ_is_vtuple)
       if(uk/=pm_typ_is_tuple.and.uk/=pm_typ_is_vtuple) then
          ok=.false.
       else
          nt=pm_tv_numargs(t)
          nu=pm_tv_numargs(u)
          if(nt>nu) then
             ok=.false.
             return
          endif
          if(nu>nt.and.pm_tv_kind(t)/=pm_typ_is_vtuple) then
             ok=.false.
             return
          endif
          do i=1,nt
             if(.not.pm_typ_includes(context,pm_tv_arg(t,i),&
                  pm_tv_arg(u,i))) then
                ok=.false.
                return
             endif
          enddo
          do i=nt+1,nu
             if(.not.pm_typ_includes(context,pm_tv_arg(t,nt),&
                  pm_tv_arg(u,i))) then
                ok=.false.
                return
             endif
          enddo
          ok=.true.
       endif
    case(pm_typ_is_user)
       r=pm_dict_val(context,context%tcache,int(p,pm_ln))
       if(pm_fast_isnull(r)) then
          ! Base types
          ok=.false.
          return
       endif
       ! Check included
       if(pm_tset_includes(context,r,q)) then
          ok=.true.
          return
       endif
       ok=.false.
    case(pm_typ_is_single_name,pm_typ_is_single_proc)
       ok=.false.
    case(pm_typ_is_ambig)
       ok=pm_typ_includes(context,pm_tv_arg(t,1),q)
    case default
       write(*,*) 'Type=',p
       write(*,*) 'Kind=',pm_tv_kind(t)
       write(*,*) 'Name=',pm_tv_name(t)
       do i=1,pm_tv_numargs(t)
          write(*,*) 'Arg=',pm_tv_arg(t,i)
       enddo
       call pm_panic('pm_typ_includes bad type kind')
    end select
    
  contains

    include 'fesize.inc'
    include 'fisnull.inc'
    
  end function pm_test_typ_includes

  recursive function pm_test_typ_intersects(context,p,q) result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: p,q
    logical:: ok
    integer(pm_i16):: s
    type(pm_ptr):: t,u,r
    integer:: i,j,nt,nu,tk,uk
    if(p==q) then
       ok=.true.
       return
    elseif(p==0.or.q==0) then
       ok=.true.
       return
    endif
    t=pm_typ_vect(context,p)
    u=pm_typ_vect(context,q)
    uk=pm_tv_kind(u)
    if(uk==pm_typ_is_single_proc.and.p==pm_proc) then
       ok=.true.
       return
    else if(uk==pm_typ_is_single_name.and.p==pm_name) then
       ok=.true.
       return
    elseif(uk==pm_typ_is_tuple.and.p==pm_matched_type) then
       if(pm_tv_numargs(u)==2) then
          ok=pm_typ_intersects(context,pm_tv_arg(u,1),pm_tv_arg(u,2))
       else
          ok=.false.
       endif
       return
    else if(uk==pm_typ_is_ambig) then
       ok=pm_typ_intersects(context,p,pm_tv_arg(u,1)).or.&
            pm_typ_intersects(context,p,pm_tv_arg(u,2))
       return
    endif
    tk=pm_tv_kind(t)
    select case(pm_tv_kind(t))
    case(pm_typ_is_struct,pm_typ_is_rec)
       if(uk/=tk) return
       if(.not.pm_tv_name(t)==pm_tv_name(u)) then
          ok=.false.
          return
       endif
       do i=1,pm_tv_numargs(t)
          if(.not.pm_typ_intersects(context,pm_tv_arg(t,i),&
               pm_tv_arg(u,i))) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
       return
    case(pm_typ_is_any,pm_typ_is_null)
       ok=pm_typ_intersects(context,pm_tv_arg(t,1),q)
       return
    case(pm_typ_is_array)
       if(pm_tv_kind(u)/=pm_typ_is_array) then
          return
       else
          ok=pm_typ_intersects(context,pm_tv_arg(t,1),pm_tv_arg(u,1)).and.&
               pm_typ_intersects(context,pm_tv_arg(t,2),pm_tv_arg(u,2))
          return
       endif
    case(pm_typ_is_tuple,pm_typ_is_vtuple)
       if(q==pm_matched_type) then
          nt=pm_tv_numargs(t)
          if(nt==2) then
             ok=pm_typ_intersects(context,pm_tv_arg(t,1),pm_tv_arg(t,2))
          else
             ok=.false.
          endif
          return
       endif
       if(pm_tv_kind(u)/=pm_typ_is_tuple.and.&
            pm_tv_kind(u)/=pm_typ_is_vtuple) then
          return
       else
          nt=pm_tv_numargs(t)
          nu=pm_tv_numargs(u)
          if(nt>nu) then
             r=t
             t=u
             u=r
             i=nt
             nt=nu
             nu=i
          endif
          if(nu>nt.and.pm_tv_kind(t)/=pm_typ_is_vtuple) then
             ok=.false.
             return
          endif
          do i=1,nt
             if(.not.pm_typ_intersects(context,&
                  pm_tv_arg(t,i),pm_tv_arg(u,i))) then
                ok=.false.
                return
             endif
          enddo
          do i=nt+1,nu
             if(.not.pm_typ_intersects(context,&
                  pm_tv_arg(t,nt),pm_tv_arg(u,i))) then
                ok=.false.
                return
             endif
          enddo
          ok=.true.
       endif
    case(pm_typ_is_user)
       r=pm_dict_val(context,context%tcache,int(p,pm_ln))
       if(pm_fast_isnull(r)) then
          ok=.false.
          return
       endif
       if(pm_typ_includes(context,p,q)) then
          ok=.true.
          return
       endif
       if(pm_typ_includes(context,q,p)) then
          ok=.true.
          return
       endif
       ok=pm_tset_intersect(context,r,q)
       return
    case(pm_typ_is_single_name)
       ok=q==pm_name
    case(pm_typ_is_single_proc)
       ok=q==pm_proc
    case default
       write(*,*) 'Type=',p
       write(*,*) 'Kind=',pm_tv_kind(t)
       write(*,*) 'Name=',pm_tv_name(t)
       do i=1,pm_tv_numargs(t)
          write(*,*) 'Arg=',pm_tv_arg(t,i)
       enddo
       call pm_panic('pm_typ_intersects bad type kind')
    end select

  contains

    include 'fisnull.inc'
    
  end function pm_test_typ_intersects

  function pm_typ_rel(context,p,q) result(rel)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: p,q
    integer:: rel
    type(pm_ptr):: t,u
    integer:: i,j,usr_index
    if(p==q) then
       rel=pm_typ_equal
    elseif(pm_typ_includes(context,p,q)) then
       rel=pm_typ_contains
    elseif(pm_typ_includes(context,q,p))then
       rel=pm_typ_contained
    else
       if(pm_typ_intersects(context,p,q)) then
          rel=pm_typ_conjoint
       else
          rel=pm_typ_disjoint
       endif
    end if
  end function pm_typ_rel

  recursive function pm_typ_is_concrete(context,tno) result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: tno
    logical:: ok
    type(pm_ptr):: tv,r
    integer(pm_i16):: tk
    integer:: i
    tv=pm_typ_vect(context,tno)
    tk=pm_tv_kind(tv)
    select case (tk)
    case(pm_typ_is_ambig)
       ok=.false.
    case(pm_typ_is_user)
      r=pm_dict_val(context,context%tcache,int(tno,pm_ln))
      if(pm_fast_isnull(r)) then
         ok=.true.
      else
         ok=.false.
      endif
    case default
       do i=1,pm_tv_numargs(tv)
          if(.not.pm_typ_is_concrete(context,pm_tv_arg(tv,i))) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
    end select
  contains
    include 'fisnull.inc'
  end function pm_typ_is_concrete

  function pm_typ_vect(context,tno) result(typ)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: tno
    type(pm_ptr):: typ,dict
    integer(pm_i16):: t
    if(tno<0) then
       dict=context%tcache
       t=-tno
    else
       dict=context%heap%tcache
       t=tno
    endif
    if(pm_debug_level>0) then
       if(tno<1.or.tno>pm_dict_size(context,dict)) &
            call pm_panic('pm_typ_vect')
    endif
    typ=pm_dict_key(context,context%tcache,int(tno,pm_ln))
  end function pm_typ_vect

  function pm_tv_arg(typ,m) result(arg)
    type(pm_ptr),intent(in):: typ
    integer,intent(in):: m
    integer(pm_i16):: arg
    arg=typ%data%i16(typ%offset+m+1)
  end function pm_tv_arg
    
  function pm_tv_name(typ) result(name)
    type(pm_ptr),intent(in):: typ
    integer(pm_i16):: name
    name=typ%data%i16(typ%offset+1)
  end function pm_tv_name
  
  function pm_tv_numargs(typ) result(num)
    type(pm_ptr),intent(in):: typ
    integer:: num
    num=pm_fast_esize(typ)-1
  contains
    include 'fesize.inc'
  end function pm_tv_numargs

  function pm_tv_kind(typ) result(k)
    type(pm_ptr),intent(in):: typ
    integer(pm_i16):: k
    k=typ%data%i16(typ%offset)
  end function pm_tv_kind

  function pm_tset_new(context) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr):: ptr
    ptr=pm_fast_newnc(context,pm_int16,4_pm_p)
  contains
    include 'fnewnc.inc'
  end function pm_tset_new

  function pm_tset_size(set) result(n)
    type(pm_ptr):: set
    integer(pm_i16):: n
    n=set%data%i16(set%offset)
  end function pm_tset_size

  recursive function pm_tset_includes(context,set,q) result(there)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: set
    integer(pm_i16):: q
    logical:: there
    integer:: j
    do j=1,set%data%i16(set%offset)
       there=pm_typ_includes(context,&
            set%data%i16(set%offset+j),q)
       if(there) return
    enddo
    there=.false.
  end function pm_tset_includes

  recursive function pm_tset_intersect(context,e,q) result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: q
    type(pm_ptr),intent(in):: e
    logical:: ok
    integer:: j
    do j=1,e%data%i16(e%offset)
       if(pm_typ_intersects(context,&
            q,e%data%i16(e%offset+j))) then
          ok=.true.
          return
       endif
    enddo
    ok=.false.
  contains
    include 'fisnull.inc'
  end function pm_tset_intersect

  function pm_tset_contained(context,t,set) result(rel)
    type(pm_context),pointer:: context
    integer(pm_i16):: t,a
    type(pm_ptr):: set
    integer:: rel,i
    rel=pm_tset_not_contained
    do i=1,set%data%i16(set%offset)
       if(pm_typ_includes(context,t,&
            set%data%i16(set%offset+i))) then
          if(rel==pm_tset_not_contained) &
               rel=pm_tset_fully_contained
       else if(rel==pm_tset_fully_contained) then
          rel=pm_tset_part_contained
       endif
    enddo
  end function pm_tset_contained

  subroutine pm_tset_add(context,set,typno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(inout):: set
    integer(pm_i16),intent(in):: typno
    type(pm_ptr):: newset
    integer(pm_ln):: siz
    integer(pm_p):: off
    integer:: i,j,n
    integer(pm_i16):: typno2
    type(pm_root),pointer:: root

    if(pm_debug_level>0) then
       if(typno<0.or.typno>&
            pm_dict_size(context,context%tcache)) then
          write(*,*) 'Bad typno=',typno
          call pm_panic('add-type-to-set bad type')
       endif
    endif
    siz=pm_fast_esize(set)
    off=set%data%i16(set%offset)
    if(off+1<=siz) then
       off=off+1
       set%data%i16(set%offset)=off
       set%data%i16(set%offset+off)=typno
    else
       root=>pm_add_root(context,set)
       newset=pm_new(context,pm_int16,siz*2)
       newset%data%i16(newset%offset:newset%offset+off)=&
               set%data%i16(set%offset:set%offset+off)
       off=off+1
       newset%data%i16(newset%offset)=off
       newset%data%i16(newset%offset+off)=typno
       call pm_delete_root(context,root)
       set=newset
    endif
  contains
    include 'fesize.inc'
  end subroutine pm_tset_add

  
end module pm_types
