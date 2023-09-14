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
module pm_linker
  use pm_kinds
  use pm_sysdep
  use pm_compbase
  use pm_memory
  use pm_hash
  use pm_lib
  use pm_parser
  implicit none

  integer,parameter:: max_link_errors=20

contains

  !=================================================
  ! The following routines process linkage between
  ! modules
  !=================================================

  ! Process all include statements
  subroutine link_includes(context,nerror,modl_dict)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    type(pm_ptr):: modl_dict
    type(pm_ptr),target:: modls,modl,dict
    type(pm_ptr),target:: incls,node,imodl
    type(pm_ptr)::p
    type(pm_reg),pointer:: reg
    integer:: i,j
    character(len=100):: str
    reg=>pm_register(context,'link includes',modls,modl,&
         dict,incls,node,imodl)
    modls=pm_dict_vals(context,modl_dict)
    ! Loop through all loaded modules
    do j=0,pm_dict_size(context,modl_dict)-1
       modl=modls%data%ptr(modls%offset+j)
       dict=modl%data%ptr(modl%offset+modl_include)
       incls=pm_dict_vals(context,dict)
       ! Loop through include definitions for given module
       do i=0,pm_dict_size(context,dict)-1
          node=incls%data%ptr(incls%offset+i)
          if(pm_debug_level>5) then
             call pm_name_string(context,&
                  int(node%data%ptr(node%offset+node_args)%offset),str)
             write(*,*) 'including',trim(str)
             p=pm_dict_key(context,dict,int(i+1,pm_ln))
             write(*,*) '..',trim(pm_name_as_string(context,int(p%offset)))
          endif
          imodl=node%data%ptr(node%offset+node_args+1)
          if(modl==imodl) then
             call link_error(context,nerror,node,'Module cannot include itself')
          endif
          if(node_sym(node)==sym_use) then
             call link_include(context,nerror,node,modl,imodl)
          else
             call link_include_mod(context,nerror,node,modl,imodl)
          endif
          if(pm_debug_level>5) then
             write(*,*) '... included ',trim(str)
          endif
       enddo
    enddo
    call pm_delete_register(context,reg)
  end subroutine link_includes

  ! Process a single unmodified include statement
  subroutine link_include(context,nerror,node,modl,imodl)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    type(pm_ptr),intent(in):: node,modl,imodl
    type(pm_ptr):: dict,elems,vals,elem,val
    integer:: i,j
    do i=modl_proc,modl_param
       dict=imodl%data%ptr(imodl%offset+i)
       elems=pm_dict_keys(context,dict)
       vals=pm_dict_vals(context,dict)
       do j=0,pm_dict_size(context,dict)-1
          elem=elems%data%ptr(elems%offset+j)
          val=vals%data%ptr(vals%offset+j)
          call link_include_elem(context,nerror,node,i,modl,elem,imodl,val)
       enddo
    enddo
  end subroutine link_include

  ! Process a modified include statement
  subroutine link_include_mod(context,nerror,node,modl,imodl)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    type(pm_ptr),intent(in):: node,modl,imodl
    integer i,slot
    type(pm_ptr):: list,name,dict,val,keys,vals,key,mnode
    list=node_arg(node,3)
    keys=pm_dict_keys(context,list)
    vals=pm_dict_vals(context,list)
    do i=0,pm_dict_size(context,list)-1
       key=keys%data%ptr(keys%offset+i)
       slot=key%data%i(key%offset)
       name=pm_fast_name(context,int(key%data%i(key%offset+1)))
       mnode=vals%data%ptr(vals%offset+i)
       dict=imodl%data%ptr(imodl%offset+slot)
       val=pm_dict_lookup(context,dict,name)
       if(pm_fast_isnull(val)) then
          call link_error(context,nerror,mnode,&
               'Unknown name in include modifier: ',name)
       endif
       call link_include_elem(context,nerror,mnode,slot,modl,name,imodl,val)
    enddo
  contains
    include 'fisnull.inc'
    include 'fname.inc'
  end subroutine link_include_mod
  
  ! Include a single named element
  subroutine link_include_elem(context,nerror,node,kind,modl,elem,imodl,val)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    integer,intent(in):: kind
    type(pm_ptr),intent(in):: node,modl,elem,imodl,val
    type(pm_ptr),target:: dict,lcl_dict,idict,old,v1,v2
    logical:: changed
    if(pm_debug_level>2) then
       write(*,*) 'Include elem: ',trim(pm_name_as_string(context,int(elem%offset))),&
            ' to ',trim(pm_name_as_string(context,get_modl_name(modl))),&
            ' from ',trim(pm_name_as_string(context,get_modl_name(modl)))
    endif
    dict=modl%data%ptr(modl%offset+kind)
    lcl_dict=modl%data%ptr(modl%offset+kind+modl_local)
    idict=imodl%data%ptr(imodl%offset+kind)
    ! Check existing entry
    old=pm_dict_lookup(context,dict,elem)
    if(pm_fast_isnull(old)) then
       old=pm_dict_lookup(context,lcl_dict,elem)
    endif
    if(pm_fast_isnull(old)) then
       call pm_dict_set(context,&
            lcl_dict,elem,val,.true.,.false.,changed)
    else
       v1=old%data%ptr(old%offset)
       v2=val%data%ptr(val%offset)
       call link_merge(context,nerror,node,kind,v1,v2)
       call link_join(context,nerror,old,val,v1)
    endif
  contains
    include 'fisnull.inc'
  end subroutine link_include_elem

  ! Merge two definitions if allowed
  ! updated definition stored in v1
  subroutine link_merge(context,nerror,node,kind,v1,v2) 
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    integer,intent(in):: kind
    type(pm_ptr),intent(in):: node,v1,v2
    type(pm_ptr):: first,name
    if(v1==v2) return
    select case(kind)
    case(modl_param)
       name=node_arg(v1,2)
       call link_error(context,nerror,v1,'Multiple definitions of parameter')
       call link_error(context,nerror,v2,'... conflicting definition')
       call link_error(context,nerror,node,'... definitions joined here')
    case(modl_proc)
       call pm_ptr_assign(context,&
            v1%data%ptr(v1%offset+node_args+2),&
            int(proc_link,pm_ln),&
            v2%data%ptr(v2%offset+node_args+1))
       call pm_ptr_assign(context,v1,int(node_args+2,pm_ln),&
            v2%data%ptr(v2%offset+node_args+2))
    case(modl_type)
       first=v1%data%ptr(v1%offset+node_args+1)
       if(node_sym(first)==sym_in) then
          call pm_ptr_assign(context,&
               first,&
               int(node_args,pm_ln),&
               v2%data%ptr(v2%offset+node_args+1))
       else
          call pm_ptr_assign(context,&
               first,&
               int(typ_link,pm_ln),&
               v2%data%ptr(v2%offset+node_args+1))
       endif
       call pm_ptr_assign(context,v1,int(node_args+2,pm_ln),&
            v2%data%ptr(v2%offset+node_args+2))
    case default
       call pm_panic('link_merge -- unknown kind')
    end select
  end subroutine  link_merge

  ! Join two groups of definitions and set value
  ! of all members in combined set
  subroutine link_join(context,nerror,node1,node2,val)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    type(pm_ptr),intent(in):: node1,node2,val
    type(pm_ptr):: p1,p2
    if(node1==node2) return
    p1=node1%data%ptr(node1%offset+1)
    p2=node2%data%ptr(node2%offset+1)
    call pm_ptr_assign(context,node1,1_pm_ln,p2)
    call pm_ptr_assign(context,node2,1_pm_ln,p1)
    p2=p1
    do
       p1%data%ptr(p1%offset)=val
       p1=p1%data%ptr(p1%offset+1)
       if(p1==p2) exit
    enddo
  end subroutine link_join

  ! Linker error
  subroutine link_error(context,nerror,node,mess,name)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    type(pm_ptr),intent(in):: node
    character(len=*),intent(in):: mess
    type(pm_ptr),intent(in),optional:: name
    character(len=100):: namestr,mnamestr,inamestr
    if(.not.pm_main_process) return
    write(*,*)
    call pm_error_header(context,node_get_modl_name(node),&
         node_get_lineno(node),node_get_charno(node))
    if(present(name)) then
       write(*,*) 'Error: '//mess//trim(pm_name_as_string(context,int(name%offset)))
    else
       write(*,*) 'Error: '//mess
    endif
    nerror=nerror+1
    if(nerror>max_link_errors) call pm_stop('Too many linking errors')
  end subroutine link_error

end module pm_linker

