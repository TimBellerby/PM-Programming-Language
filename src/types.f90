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
  use pm_parser
  implicit none

  logical,parameter:: pm_typ_xtra_debug=.false.

  integer(pm_i16),parameter:: pm_typ_is_user=1
  integer(pm_i16),parameter:: pm_typ_is_any=2
  integer(pm_i16),parameter:: pm_typ_is_struct=3
  integer(pm_i16),parameter:: pm_typ_is_rec=4
  integer(pm_i16),parameter:: pm_typ_is_array=5
  integer(pm_i16),parameter:: pm_typ_is_opt=6
  integer(pm_i16),parameter:: pm_typ_is_tuple=7
  integer(pm_i16),parameter:: pm_typ_is_vtuple=8
  integer(pm_i16),parameter:: pm_typ_is_single_name=9
  integer(pm_i16),parameter:: pm_typ_is_single_proc=10
  integer(pm_i16),parameter:: pm_typ_is_ambig=11
  integer(pm_i16),parameter:: pm_typ_is_intersect=12
  integer(pm_i16),parameter:: include_cache=13
  integer(pm_i16),parameter:: intersect_cache=14
  integer(pm_i16),parameter:: intersection_cache=15
  
  integer,private,parameter:: max_intersect_stack = 128

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
    'real       ','double     ','real32     ','real64     ',&
    'real128    ','cpx        ','double_cpx ','cpx64      ',&
    'cpx128     ','cpx256     ','bool       ','<packbool> ',&
    '<ext>      ','char       ','<pointer>  ','<stack>    ',&
    '<usr>      ','<matched>  ','<dict>     ',&
    '<set>      ','<obj_set>  ','string     ','<poly>     ',&
    '<struct>   ','<rec>      ','<polyref  >','<array>    '&
    /)
    context%tcache=pm_dict_new(context,128_pm_ln)
    context%pcache=pm_dict_new(context,1024_pm_ln)
    key(1)=pm_typ_is_user
    do i=1,pm_last_libtype
       key(2)=pm_intern(context,trim(base_types(i)))
       if(pm_debug_level>2) &
            write(*,*) 'Init types(',trim(base_types(i)),')',key(2)
       j=pm_idict_add(context,context%tcache,key,2,pm_null_obj)
       if(j/=i) call pm_panic('init_typ')
    enddo
    if(pm_debug_level>2) write(*,*) 'Types inited'
  end subroutine init_typ

  ! Does supertype include subtype?
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
    j=pm_ivect_lookup(context,context%pcache,key,3)
    if(j>0) then
       p=pm_dict_val(context,context%pcache,j)
       ok=p%offset/=0
       if(pm_typ_xtra_debug) write(*,*) 'Cached include',supertype,subtype,ok,'(',j,')'
    else
       if(.not.associated(context%heap,context)) then
          j=pm_ivect_lookup(context,context%heap%pcache,key,3)
          if(j>0) then
             p=pm_dict_key(context,context%heap%pcache,j)
             ok=p%offset/=0
             j=pm_idict_add(context,context%pcache,key,3,p)
             return
          endif
       endif
       ok=pm_test_typ_includes(context,supertype,subtype)
       if(ok) then
          p=pm_fast_tinyint(context,1_pm_p)
       else
          p=pm_fast_tinyint(context,0_pm_p)
       endif
       j=pm_idict_add(context,context%pcache,key,3,p)
       if(pm_typ_xtra_debug) &
            write(*,*) 'Calc include',supertype,subtype,ok,'cache as',j
    endif
  contains
    include 'ftiny.inc'
  end function pm_typ_includes
  
  ! Does supertype intersect with subtype?
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
    j=pm_ivect_lookup(context,context%pcache,key,3)
    if(j>0) then
       p=pm_dict_val(context,context%pcache,j)
       ok=p%offset/=0
       if(pm_typ_xtra_debug) write(*,*) 'caced intersect',supertype,subtype,ok
    else
       if(.not.associated(context%heap,context)) then
          j=pm_ivect_lookup(context,context%heap%pcache,key,3)
          if(j>0) then
             p=pm_dict_key(context,context%heap%pcache,j)
             ok=p%offset/=0
             j=pm_idict_add(context,context%pcache,key,3,p)
             return
          endif
       endif
       ok=pm_test_typ_intersects(context,supertype,subtype)
       if(pm_typ_xtra_debug) write(*,*) 'Calc intersect',supertype,subtype,ok
       if(ok) then
          p=pm_fast_tinyint(context,1_pm_p)
       else
          p=pm_fast_tinyint(context,0_pm_p)
       endif
       if(pm_typ_xtra_debug) &
            write(*,*) 'inter cac',trim(pm_name_as_string(context,216_pm_p))
       j=pm_idict_add(context,context%pcache,key,3,p)
       if(pm_typ_xtra_debug) &
            write(*,*) 'inter caced',trim(pm_name_as_string(context,216_pm_p))
    endif
  contains
    include 'ftiny.inc'
  end function pm_typ_intersects

  ! Test if one type includes another
  recursive function pm_test_typ_includes(context,supertype,subtype)&
       result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: supertype,subtype
    logical:: ok
    integer(pm_i16):: p,q,s
    type(pm_ptr):: t,u,r
    integer:: i,j,tk,uk,nt,nu,usr_index

    if(pm_typ_xtra_debug) write(*,*) 'Test incl',supertype,subtype

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
    tk=pm_tv_kind(t)

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
       do i=1,pm_tv_numargs(u)
          if(.not.pm_typ_includes(context,p,pm_tv_arg(u,i))) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
       return
    elseif(uk==pm_typ_is_intersect) then
       r=pm_dict_val(context,context%tcache,int(q,pm_ln))
       do i=1,r%data%i16(r%offset)
          if(.not.pm_typ_includes(context,p,r%data%i16(r%offset+i))) then
             ok=.false.
             return
          endif
          ok=.true.
          return
       enddo
    endif
 
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
    case(pm_typ_is_opt)
       if(uk==pm_typ_is_opt) then
          ok=pm_typ_includes(context,&
               pm_tv_arg(t,1),pm_tv_arg(u,1))
       else
          if(q==pm_null) then
             ok=.true.
          else
             ok=pm_typ_includes(context,pm_tv_arg(t,1),q)
          endif
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
          if(nt>nu.and.uk/=pm_typ_is_vtuple) then
             ok=.false.
             return
          endif
          if(nu>nt.and.tk/=pm_typ_is_vtuple) then
             ok=.false.
             return
          endif
          do i=1,min(nt,nu)
             if(.not.pm_typ_includes(context,pm_tv_arg(t,i),&
                  pm_tv_arg(u,i))) then
                ok=.false.
                return
             endif
          enddo
          if(nu>nt) then
             do i=nt+1,nu
                if(.not.pm_typ_includes(context,pm_tv_arg(t,nt),&
                     pm_tv_arg(u,i))) then
                   ok=.false.
                   return
                endif
             enddo
          else
             do i=nu+1,nt
                if(.not.pm_typ_includes(context,pm_tv_arg(t,nt),&
                     pm_tv_arg(u,i))) then
                   ok=.false.
                   return
                endif
             enddo
          endif
          ok=.true.
       endif
    case(pm_typ_is_user)
       r=pm_dict_val(context,context%tcache,int(p,pm_ln))
       if(pm_fast_isnull(r)) then
          ! Base types
          ok=.false.
          return
       endif
       ! Check P(p1,p2) < Q(q1,q2) <=> p1<q1, p2<q2 if P==Q
       if(uk==pm_typ_is_user) then
          if(pm_tv_name(t)==pm_tv_name(u)) then
             nt=pm_tv_numargs(t)
             nu=pm_tv_numargs(u)
             if(nt==nu) then
                do i=1,nt
                   ok=pm_typ_includes(context,pm_tv_arg(t,i),pm_tv_arg(u,i))
                   if(.not.ok) return
                enddo
                ok=.true.
                return
             endif
          endif
       endif
       ok=pm_tset_includes(context,r,q)
    case(pm_typ_is_intersect)
       r=pm_dict_val(context,context%tcache,int(p,pm_ln))
       ok=pm_tset_includes(context,r,q)
    case(pm_typ_is_single_name,pm_typ_is_single_proc)
       ok=.false.
    case(pm_typ_is_ambig)
       do i=1,pm_tv_numargs(t)
          if(pm_typ_includes(context,pm_tv_arg(t,i),q)) then
             ok=.true.
             return
          endif
       enddo
       ok=.false.
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

  ! Test if two types intersect
  recursive function pm_test_typ_intersects(context,p,q) result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: p,q
    logical:: ok
    integer(pm_i16):: s
    type(pm_ptr):: t,u,r
    integer:: i,j,nt,nu,tk,uk
    if(pm_typ_xtra_debug) write(*,*) 'Test intersect',p,q
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
    if(uk==pm_typ_is_opt.and.p==pm_null) then
       ok=.true.
       return
    elseif(uk==pm_typ_is_single_proc.and.p==pm_proc) then
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
       do i=1,pm_tv_numargs(u)
          if(pm_typ_intersects(context,p,pm_tv_arg(u,i))) then
             ok=.true.
             return
          endif
       enddo
       ok=.false.
       return
    else if(uk==pm_typ_is_user.or.uk==pm_typ_is_intersect) then
       r=pm_dict_val(context,context%tcache,int(q,pm_ln))
       if(.not.pm_fast_isnull(r)) then
          do i=1,r%data%i16(r%offset)
             if(pm_typ_intersects(context,p,r%data%i16(r%offset+i))) then
                ok=.true.
                return
             endif
          enddo
          ok=.false.
          return
       endif
    endif
    tk=pm_tv_kind(t)
    select case(pm_tv_kind(t))
    case(pm_typ_is_struct,pm_typ_is_rec)
       if(uk/=tk) then
          ok=.false.
          return
       endif
       if(pm_tv_name(t)/=pm_tv_name(u)) then
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
    case(pm_typ_is_any)
       if(uk/=pm_typ_is_any) then
          ok=.false.
          return
       endif
       ok=pm_typ_intersects(context,pm_tv_arg(t,1),pm_tv_arg(u,1))
       return
    case(pm_typ_is_opt)
       if(uk==pm_typ_is_opt) then
          ok=pm_typ_intersects(context,pm_tv_arg(t,1),pm_tv_arg(u,1))
       else
          if(q==pm_null) then
             ok=.true.
          else
             ok=pm_typ_intersects(context,pm_tv_arg(t,1),q)
          endif
       endif
    case(pm_typ_is_array)
       if(uk/=pm_typ_is_array) then
          ok=.false.
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
       if(uk/=pm_typ_is_tuple.and.&
            uk/=pm_typ_is_vtuple) then
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
    case(pm_typ_is_user,pm_typ_is_intersect)
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
    case(pm_typ_is_ambig)
       do i=1,pm_tv_numargs(t)
          if(pm_typ_intersects(context,pm_tv_arg(t,i),q)) then
             ok=.true.
             return
          endif
       enddo
       ok=.false.
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


  ! Intersection between two types as new type
  recursive function pm_typ_intersect(context,p,q,stack,nstack,ok) result(tno)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: p,q
    integer,intent(in):: nstack
    integer(pm_i16),dimension(nstack):: stack
    logical,intent(out):: ok
    integer(pm_i16):: tno
    integer(pm_i16),dimension(3):: key
    integer:: top
    type(pm_ptr):: v
    integer(pm_ln):: j
    if(pm_typ_xtra_debug) write(*,*) 'intersect::',p,q
    if(p==0) then
       tno=q
       ok=.true.
       return
    elseif(q==0) then
       tno=p
       ok=.true.
       return
    endif
    key(1)=intersection_cache
    key(2)=p
    key(3)=q
    j=pm_ivect_lookup(context,context%pcache,key,3)
    if(j>0) then
       v=pm_dict_val(context,context%pcache,j)
       tno=v%offset
       ok=tno/=-1
       if(pm_typ_xtra_debug) write(*,*) 'Cached intersection',p,q,tno
       return
    endif
    if(pm_typ_includes(context,p,q)) then
       if(pm_typ_xtra_debug) write(*,*) 'Intersection -- included',p,q
       ok=.true.
       tno=q
    elseif(pm_typ_includes(context,q,p)) then
       if(pm_typ_xtra_debug) write(*,*) 'Intersection - includes',p,q
       ok=.true.
       tno=p
    else
       stack(1)=pm_typ_is_ambig
       stack(2)=0
       top=2
       call pm_calc_typ_intersect(context,p,q,stack,nstack,top)
       if(pm_typ_xtra_debug) write(*,*) 'top=',top
       if(top==2) then
          ok=.false.
          tno=-1
       elseif(top==3) then
          ok=.true.
          tno=stack(3)
       else
          ok=.true.
          tno=pm_idict_add(context,context%tcache,stack,top,pm_null_obj)
          if(pm_typ_xtra_debug) write(*,*) 'MADE INTERSECT TYPE>',tno
       endif
    endif
    j=pm_idict_add(context,context%pcache,key,3,pm_fast_tinyint(context,int(tno,pm_p)))
  contains
    include 'ftiny.inc'
  end function pm_typ_intersect

  function pm_do_intersect(context,p,q,stack,nstack) result(tset)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: p,q
    integer,intent(in):: nstack
    integer(pm_i16),dimension(nstack):: stack
    type(pm_ptr):: tset
    integer:: top,i
    top=0
    call pm_calc_typ_intersect(context,p,q,stack,nstack,top)
    tset=pm_tset_new(context)
    do i=1,top
       call pm_tset_add(context,tset,stack(i))
    enddo
  end function pm_do_intersect

  ! Calculate type intersection
  recursive subroutine pm_calc_typ_intersect(context,p,q,stack,nstack,top)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: p,q
    integer,intent(in):: nstack
    integer(pm_i16),dimension(nstack):: stack
    integer,intent(inout):: top
    integer(pm_i16):: tno
    type(pm_ptr):: t,u,r,s
    integer:: base,i,j,nt,nu,tk,uk
    logical:: ok
    if(pm_typ_xtra_debug) write(*,*) 'Calc intersection::',p,q
    if(p==q) then
       call push(p)
       return
    elseif(p==0) then
       call push(q)
       return
    else if(q==0) then
       call push(p)
       return
    endif
    t=pm_typ_vect(context,p)
    u=pm_typ_vect(context,q)
    uk=pm_tv_kind(u)
    tk=pm_tv_kind(t)
    if(uk==pm_typ_is_opt.and.p==pm_null) then
       call push(int(pm_null,pm_i16))
       return
    elseif(uk==pm_typ_is_single_proc.and.p==pm_proc) then
       call push(q)
       return
    else if(uk==pm_typ_is_single_name.and.p==pm_name) then
       call push(q)
       return
    else if(uk==pm_typ_is_ambig) then
       do i=1,pm_tv_numargs(u)
          call pm_calc_typ_intersect(context,p,pm_tv_arg(u,i),stack,nstack,top)
       enddo
       return
    else if(uk==pm_typ_is_user.or.uk==pm_typ_is_intersect) then
       r=pm_dict_val(context,context%tcache,int(q,pm_ln))
       if(.not.pm_fast_isnull(r)) then
          do i=1,r%data%i16(r%offset)
             call pm_calc_typ_intersect(context,p,r%data%i16(r%offset+i),stack,nstack,top)
          enddo
          return
       endif
    endif
    select case(pm_tv_kind(t))
    case(pm_typ_is_struct,pm_typ_is_rec)
       if(uk/=tk) then
          return
       endif
       if(pm_tv_name(t)/=pm_tv_name(u)) then
          return
       endif
       base=top
       call push(int(tk,pm_i16))
       call push(pm_tv_name(t))
       do i=1,pm_tv_numargs(t)
          tno=pm_typ_intersect(context,pm_tv_arg(t,i),&
               pm_tv_arg(u,i),stack(top+1:),nstack-top,ok)
          if(.not.ok) then
             top=base
             return
          endif
          call push(tno)
       enddo
       call make_type(pm_tv_numargs(t)+2)
       return
    case(pm_typ_is_any)
       if(uk/=pm_typ_is_any) then
          return
       endif
       base=top
       call push(pm_typ_is_any)
       call push(0_pm_i16)
       tno=pm_typ_intersect(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
            stack(top+1:),nstack-top,ok)
       if(.not.ok) then
          top=base
          return
       endif
       call push(tno)
       call make_type(3)
       return
    case(pm_typ_is_opt)
       if(uk==pm_typ_is_opt) then
          base=top
          call push(pm_typ_is_opt)
          call push(0_pm_i16)
          tno=pm_typ_intersect(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               stack(top+1:),&
               nstack-top,ok)
          if(.not.ok) then
             top=base
             return
          endif
          call push(tno)
          call make_type(3)
       else
          if(q==pm_null) then
             call push(int(pm_null,pm_i16))
          else
             call pm_calc_typ_intersect(context,pm_tv_arg(t,1),q,stack,nstack,top)
          endif
       endif
    case(pm_typ_is_array)
       if(uk/=pm_typ_is_array) then
          return
       else
          base=top
          call push(pm_typ_is_array)
          call push(0_pm_i16)
          tno=pm_typ_intersect(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               stack(top+1:),nstack-top,ok)
          if(.not.ok) then
             top=base
             return
          endif
          call push(tno)
          tno=pm_typ_intersect(context,pm_tv_arg(t,2),pm_tv_arg(u,2),&
               stack(top+1:),nstack-top,ok)
          if(.not.ok) then
             top=base
             return
          endif
          call push(tno)
          call make_type(4)
          return
       endif
    case(pm_typ_is_tuple,pm_typ_is_vtuple)
       if(uk/=pm_typ_is_tuple.and.&
            uk/=pm_typ_is_vtuple) then
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
             i=tk
             tk=uk
             uk=i
          endif
          if(nu>nt.and.tk/=pm_typ_is_vtuple) then
             return
          endif
          base=top
          if(tk==pm_typ_is_vtuple.and.uk==pm_typ_is_vtuple) then
             call push(pm_typ_is_vtuple)
          else
             call push(pm_typ_is_tuple)
          endif
          call push(0_pm_i16)
          do i=1,nt
             if(pm_typ_xtra_debug) write(*,*) 'INTER',i,nt,pm_tv_arg(t,i),pm_tv_arg(u,i),top
             tno=pm_typ_intersect(context,&
                  pm_tv_arg(t,i),pm_tv_arg(u,i),stack(top+1:),nstack-top,ok)
             if(pm_typ_xtra_debug) write(*,*) 'IS',ok,tno,top
             if(.not.ok) then
                top=base
                return
             endif
             call push(tno)
          enddo
          do i=nt+1,nu
             if(.not.pm_typ_intersects(context,&
                  pm_tv_arg(t,nt),pm_tv_arg(u,i))) then
                top=base
                return
             endif
          enddo
          call make_type(nt+2)
       endif
    case(pm_typ_is_user,pm_typ_is_intersect)
       r=pm_dict_val(context,context%tcache,int(p,pm_ln))
       if(pm_fast_isnull(r)) return
       do i=1,r%data%i16(r%offset)
          call pm_calc_typ_intersect(context,&
               r%data%i16(r%offset+i),&
               q,stack,nstack,top)
       enddo
       return
    case(pm_typ_is_ambig)
       do i=1,pm_tv_numargs(t)
          call pm_calc_typ_intersect(context,pm_tv_arg(t,i),q,stack,nstack,top)
       enddo
    case(pm_typ_is_single_name)
       if(q==pm_name) call push(p)
    case(pm_typ_is_single_proc)
       if(q==pm_proc) call push(p)
    case default
       write(*,*) 'Type=',p
       write(*,*) 'Kind=',pm_tv_kind(t)
       write(*,*) 'Name=',pm_tv_name(t)
       do i=1,pm_tv_numargs(t)
          write(*,*) 'Arg=',pm_tv_arg(t,i)
       enddo
       call pm_panic('pm_typ_intersect bad type kind')
    end select

  contains

    include 'fisnull.inc'

    subroutine push(tval)
      integer(pm_i16),intent(in):: tval
      top=top+1
      if(top>nstack) &
           call pm_panic('Program too compex (intersect stack)')
      stack(top)=tval
      if(pm_typ_xtra_debug) write(*,*) 'push',tval,top
    end subroutine push

    subroutine make_type(siz)
      integer:: siz
      integer(pm_i16):: newtno
      if(pm_debug_level>0) then
         if(pm_typ_xtra_debug) write(*,*) 'make intersect type',siz,top
         if(siz>top) call pm_panic('siz')
      endif
      newtno=pm_ivect_lookup(context,context%tcache,stack(top-siz+1:top),siz)
      if(newtno<=0) then
        newtno=pm_idict_add(context,context%tcache,&
             stack(top-siz+1:top),siz,pm_null_obj)
      endif
      top=top-siz+1
      stack(top)=newtno
    end subroutine make_type
    
  end subroutine  pm_calc_typ_intersect

  ! Check is type corresponds to only one concrete type
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

  recursive function pm_typ_is_recur(context,rno,tno) result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: rno,tno
    logical:: ok
    type(pm_ptr):: tv,r
    integer(pm_i16):: tno2
    integer:: j

    ok=.false.
    if(tno==0) return
    tv=pm_typ_vect(context,tno)
    if(pm_tv_kind(tv)==pm_typ_is_user.or.pm_tv_kind(tv)==pm_typ_is_intersect) then
       r=pm_dict_val(context,context%tcache,int(tno,pm_ln))
       if(.not.pm_fast_isnull(r)) then
          do j=1,r%data%i16(r%offset)
             tno2=r%data%i16(r%offset+j)
             if(tno2==rno) then
                ok=.true.
                return
             elseif(pm_typ_is_recur(context,rno,tno2)) then
                ok=.true.
                return
             endif
          enddo
       endif
    endif
  contains
    include 'fisnull.inc'
  end function pm_typ_is_recur

  ! Get vector of int16 representation of type
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
       if(tno<1.or.tno>pm_dict_size(context,dict)) then
          write(*,*) 'tno=',tno
          call pm_panic('pm_typ_vect')
       endif
    endif
    typ=pm_dict_key(context,context%tcache,int(tno,pm_ln))
  end function pm_typ_vect

  ! Type kind from int16 type vector
  function pm_tv_kind(typ) result(k)
    type(pm_ptr),intent(in):: typ
    integer(pm_i16):: k
    k=typ%data%i16(typ%offset)
  end function pm_tv_kind

  ! Name from int16 type vector
  function pm_tv_name(typ) result(name)
    type(pm_ptr),intent(in):: typ
    integer(pm_i16):: name
    name=typ%data%i16(typ%offset+1_pm_p)
  end function pm_tv_name

  ! Argument m from int16 type vector
  function pm_tv_arg(typ,m) result(arg)
    type(pm_ptr),intent(in):: typ
    integer,intent(in):: m
    integer(pm_i16):: arg
    arg=typ%data%i16(typ%offset+m+1)
  end function pm_tv_arg
  
  ! Number of arguments in int16 type vector
  function pm_tv_numargs(typ) result(num)
    type(pm_ptr),intent(in):: typ
    integer:: num
    num=pm_fast_esize(typ)-1
  contains
    include 'fesize.inc'
  end function pm_tv_numargs

  ! Create a type set 
  ! (currently implemented as simple vector of types)
  function pm_tset_new(context) result(ptr)
    type(pm_context),pointer:: context
    type(pm_ptr):: ptr
    ptr=pm_fast_newnc(context,pm_int16,4_pm_p)
    ptr%data%i16(ptr%offset)=0
  contains
    include 'fnewnc.inc'
  end function pm_tset_new

  ! Type set size
  function pm_tset_size(set) result(n)
    type(pm_ptr):: set
    integer(pm_i16):: n
    n=set%data%i16(set%offset)
  end function pm_tset_size

  ! Type set inclusion test
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

  ! Intersection of two type sets
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

  ! Add type to type set
  recursive subroutine pm_tset_add(context,set,typno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(inout):: set
    integer(pm_i16),intent(in):: typno
    type(pm_ptr):: newset
    integer(pm_ln):: siz
    integer(pm_p):: off
    type(pm_root),pointer:: root
    
    if(pm_debug_level>0) then
       if(typno<0.or.typno>&
            pm_dict_size(context,context%tcache)) then
          write(*,*) 'Bad typno=',typno
          call pm_panic('add-type-to-set bad type')
       endif
       call pm_verify_ptr(set,'Set in tset-add')
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

  ! Find offset for element in struct/rec type
  subroutine pm_elem_offset(context,tno,name,change,offset,etyp)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: tno,name
    logical,intent(in):: change
    integer(pm_i16),intent(out):: offset,etyp
    integer:: j,hi,lo
    integer(pm_i16):: name2
    type(pm_ptr):: tv,nv

    if(tno==0) then
       offset=-1
       etyp=0
       return
    endif

    tv=pm_typ_vect(context,tno)
    offset=-1
    etyp=0
    if(pm_tv_kind(tv)==pm_typ_is_struct.or.&
         ((.not.change).and.pm_tv_kind(tv)==pm_typ_is_rec)) then
       nv=pm_name_val(context,int(pm_tv_name(tv),pm_p))
       if(nv%data%i16(nv%offset+1)>name) return
       if(nv%data%i16(nv%offset+pm_fast_esize(nv))<name) return
       lo=1
       hi=pm_fast_esize(nv)
       j=(lo+hi)/2
       do
          name2=nv%data%i16(nv%offset+j)
          if(name2==name) then
             etyp=pm_tv_arg(tv,j)
             offset=j+1
             return
          endif
          if(name2<name) then
             lo=j+1
          else
             hi=j-1
          endif
          if(lo>hi) exit
          j=(lo+hi)/2
       enddo
    endif
  contains
    include 'fesize.inc'
  end subroutine pm_elem_offset

  ! Find offset of element in "array of struct/rec" type
  subroutine pm_array_elem_offset(context,tno,name,change,offset,etyp)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: tno,name
    logical,intent(in):: change
    integer(pm_i16),intent(out):: offset,etyp
    type(pm_ptr):: tv
    if(tno==0) then
       offset=-1
       etyp=0
       return
    endif
    tv=pm_typ_vect(context,tno)
    if(pm_tv_kind(tv)==pm_typ_is_array) then
       call pm_elem_offset(context,pm_tv_arg(tv,1),name,change,offset,etyp)
    else
       offset=-1
       etyp=0
    endif
  end subroutine pm_array_elem_offset

  ! Display type as user readable string
  function pm_typ_as_string(context,tno) result(str)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: tno
    character(len=256):: str
    integer:: n
    str=''
    if(tno==0) then
       str='any'
    else
       n=1
       call typ_to_str(context,tno,str,n)
    endif
  end function  pm_typ_as_string

  recursive subroutine typ_to_str(context,tno,str,n)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: tno
    character(len=256),intent(inout):: str
    integer,intent(inout):: n
    type(pm_ptr):: tv,nv,nv2
    integer:: tk,narg
    integer(pm_p):: name,name2
    character(len=1),parameter:: open_brace = '{'
    character(len=1),parameter:: close_brace = '}'
    character(len=1),parameter:: open_square = '['
    character(len=1),parameter:: close_square = ']'
    integer:: i
    if(n>len(str)-10) return
    if(tno==0) return
    tv=pm_typ_vect(context,tno)
    tk=pm_tv_kind(tv)
    select case(tk)
       case(pm_typ_is_intersect)
          nv=pm_dict_val(context,context%tcache,int(tno,pm_ln))
          do i=1,nv%data%i16(nv%offset)
             call typ_to_str(context,nv%data%i16(nv%offset+i),str,n)
             if(i<nv%data%i16(nv%offset)) then
                if(add_char(' or ')) return
             endif
          enddo
       case(pm_typ_is_user)
          name=pm_tv_name(tv)
          nv=pm_name_val(context,name)
          if(pm_fast_vkind(nv)==pm_int16) then
             name=nv%data%i16(nv%offset)
             nv=pm_name_val(context,name)
          endif
          if(name==sym_array) then
             call typ_to_str(context,pm_tv_arg(tv,1),str,n)
             if(add_char(open_square)) return
             narg=pm_tv_numargs(tv)
             do i=2,narg-1
                call typ_to_str(context,pm_tv_arg(tv,1),str,n)
                if(add_char(',')) return
             enddo
             call typ_to_str(context,pm_tv_arg(tv,narg),str,n)
             if(add_char(close_square)) return
          else
             if(pm_fast_vkind(nv)==pm_int) then
                if(add_char('_')) return
                name=nv%data%i(nv%offset+1_pm_p)
             endif
             call pm_name_string(context,name,str(n:))
             n=len_trim(str)+1
             if(n>len(str)-10) return
             narg=pm_tv_numargs(tv)
             if(narg>0) then
                if(add_char(open_brace)) return
                do i=1,narg-1
                   call typ_to_str(context,pm_tv_arg(tv,i),str,n)
                   if(add_char(',')) return
                enddo
                call typ_to_str(context,pm_tv_arg(tv,narg),str,n)
                if(add_char(close_brace)) return
             endif
          endif
       case(pm_typ_is_tuple,pm_typ_is_vtuple)
          if(add_char('(')) return
          narg=pm_tv_numargs(tv)
          do i=1,narg-1
             call typ_to_str(context,pm_tv_arg(tv,i),str,n)
             if(add_char(',')) return
          enddo
          call typ_to_str(context,pm_tv_arg(tv,narg),str,n)
          if(tk==pm_typ_is_vtuple) then
             if(add_char(',...')) return
          endif
          if(add_char(')')) return
       case(pm_typ_is_struct,pm_typ_is_rec)
          if(tk==pm_typ_is_struct) then
             if(add_char('struct ')) return
          else
             if(add_char('rec ')) return
          endif
          nv=pm_name_val(context,int(pm_tv_name(tv),pm_p))
          name=nv%data%i16(nv%offset)
          if(name/=0) then
             nv2=pm_name_val(context,name)
             if(pm_fast_vkind(nv2)==pm_int) then
                if(add_char('_')) return
                call pm_name_string(context,int(nv2%data%i(nv2%offset+1_pm_p),pm_p),str(n:))
             else
                call pm_name_string(context,name,str(n:))
             endif
             n=len_trim(str)+1
             if(n>len(str)-10) return
          endif
          if(add_char(open_brace)) return
          narg=pm_tv_numargs(tv)
          do i=1,narg
             name=nv%data%i16(nv%offset+i)
             nv2=pm_name_val(context,name)
             if(pm_fast_vkind(nv2)==pm_int) then
                if(add_char('_')) return
                call pm_name_string(context,int(nv2%data%i(nv2%offset+1_pm_p),pm_p),str(n:))
             else   
                call pm_name_string(context,name,str(n:))
             endif
             n=len_trim(str)+1
             if(name>len(str)-10) return
             if(add_char(':')) return
             call typ_to_str(context,pm_tv_arg(tv,i),str,n)
             if(i<narg) then
                if(add_char(',')) return
             endif
          enddo
          if(add_char(close_brace)) return
       case(pm_typ_is_opt)
          if(add_char('opt ')) return
          call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       case(pm_typ_is_any)
          if(add_char('<')) return
          call typ_to_str(context,pm_tv_arg(tv,1),str,n)
          if(add_char('>')) return
       case(pm_typ_is_array)
          call typ_to_str(context,pm_tv_arg(tv,1),str,n)
          if(add_char(open_square)) return
          if(add_char('=')) return
          call typ_to_str(context,pm_tv_arg(tv,2),str,n)
          if(add_char(close_square)) return
       end select
     contains
       include 'fvkind.inc'
       
       function add_char(c) result(term)
         character(len=*),intent(in):: c
         logical:: term
         if(n>len(str)-10-len(c)) then
            str(n:n+2)='...'
            term=.true.
         else
            str(n:n+len(c)-1)=c
            n=n+len(c)
            term=.false.
         endif
       end function add_char
  end subroutine typ_to_str

  ! Dump type tree (debugging)
  recursive subroutine dump_type(context,iunit,tno,depth)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,depth
    integer(pm_i16),intent(in):: tno
    integer(pm_i16):: u
    character(len=100),parameter:: spaces=' '
    character(len=100):: str
    character(len=6),dimension(12),parameter:: typ_names= (/ &
         'user  ','any   ','struct','rec   ','array ',&
         'null  ','tuple ','vtuple','name  ',&
         'proc  ','ambig ','itsc  '/)
    type(pm_ptr):: t
    integer:: i
    if(depth>25) then
       write(iunit,*) spaces(1:depth*2),'...'
       return
    endif
    if(tno==0) then
       write(iunit,*) spaces(1:depth*2),'Any'
       return
    endif
    if(tno<1.or.tno>pm_dict_size(context,context%tcache)) then
       write(iunit,*) spaces(1:depth*2),'????',tno
       return
    endif
    t=pm_dict_key(context,context%tcache,int(tno,pm_ln))
    u=t%data%i16(t%offset)
    if(u<1.or.u>12) then
       write(str,'("Unknown type kind",i4)') u
    else
       str=typ_names(u)
    endif
    if(u==pm_typ_is_user.or.u==pm_typ_is_struct.or.u==pm_typ_is_rec) then
       call pm_name_string(context,int(t%data%i16(t%offset+1),pm_p),&
            str(len_trim(str)+2:))
    endif
    if(pm_fast_esize(t)<2) then
       write(iunit,*) spaces(1:depth*2),trim(str),tno,pm_fast_esize(t),t%data%i16(t%offset+1)
    else
       write(iunit,*) spaces(1:depth*2),trim(str),' (',tno,pm_fast_esize(t),t%data%i16(t%offset+1)
       do i=2,pm_fast_esize(t)
          call dump_type(context,iunit,t%data%i16(t%offset+i),depth+1)
       enddo
       write(iunit,*) spaces(1:depth*2),')'
    endif
    if(u==pm_typ_is_user) then
       t=pm_dict_val(context,context%tcache,int(tno,pm_ln))
       if(.not.pm_fast_isnull(t).and..false.) then
          write(iunit,*) spaces(1:depth*2),'Includes(',t%data%i16(t%offset),pm_fast_vkind(t)
          do i=1,t%data%i16(t%offset)
             call dump_type(context,iunit,t%data%i16(t%offset+i),depth+1)
          enddo
          write(iunit,*) spaces(1:depth*2),')'
       endif
    endif
  contains
    include 'fvkind.inc'
    include 'fesize.inc'  
    include 'fisnull.inc'
  end subroutine dump_type

end module pm_types
