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
module pm_linker
  use pm_kinds
  use pm_sysdep
  use pm_compbase
  use pm_memory
  use pm_lib
  use pm_parser
  implicit none

contains

  !=================================================
  ! The following routines process linkage between
  ! modules
  !=================================================

  ! Process all include statements
  subroutine link_includes(context,modl_dict)
    type(pm_context),pointer:: context
    type(pm_ptr):: modl_dict
    type(pm_ptr),target:: modls,modl,dict
    type(pm_ptr),target:: incls,node,imodl
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
       ! Loop through include definitions for this module
       do i=0,pm_dict_size(context,dict)-1
          node=incls%data%ptr(incls%offset+i)
          if(pm_debug_level>5) then
             call pm_name_string(context,&
                  node%data%ptr(node%offset+node_args)%offset,str)
             write(*,*) 'including',trim(str)
          endif
          imodl=node%data%ptr(node%offset+node_args+1)
          if(node_sym(node)==sym_include) then
             call link_include(context,node,modl,imodl)
          else
             call link_include_mod(context,node,modl,imodl)
          endif
       enddo
    enddo
    call pm_delete_register(context,reg)
  end subroutine link_includes

  ! Process a single include statement
  subroutine link_include(context,node,modl,imodl)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: node,modl,imodl
    type(pm_ptr),target:: dict,elems,vals,elem,val
    integer:: i,j
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'link include',dict,elems,vals,elem,val)
    do i=modl_proc,modl_param
       dict=imodl%data%ptr(imodl%offset+i)
       elems=pm_dict_keys(context,dict)
       vals=pm_dict_vals(context,dict)
       do j=0,pm_dict_size(context,dict)-1
          elem=elems%data%ptr(elems%offset+j)
          val=vals%data%ptr(vals%offset+j)
          call link_include_elem(context,node,i,modl,elem,imodl,val)
       enddo
    enddo
    call pm_delete_register(context,reg)
  end subroutine link_include

  ! Process a modified include statement
  subroutine link_include_mod(context,node,modl,imodl)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: node,modl,imodl
    integer i,slot
    type(pm_ptr):: list,name,rename,dict,val
    type(pm_reg),pointer:: reg
    reg=>pm_register(context,'include modified',list,name,rename,dict,val)
    list=node%data%ptr(node%offset+node_args+2)
    do i=node_args,node_args+node_numargs(list)-1,3
       slot=list%data%ptr(list%offset+i)%offset
       name=list%data%ptr(list%offset+i+1)
       rename=list%data%ptr(list%offset+i+2)
       if(pm_fast_isnull(rename)) rename=name
       dict=imodl%data%ptr(imodl%offset+slot)
       val=pm_dict_lookup(context,dict,name)
       if(pm_fast_isnull(name)) then
          call link_error(context,node,&
               'Unknown name in include modifier: ',name)
       endif
       call link_include_elem(context,node,slot,modl,rename,imodl,val)
    enddo
    call pm_delete_register(context,reg)
  contains
    include 'fisnull.inc'  
  end subroutine link_include_mod
  
  ! Include a single named element
  subroutine link_include_elem(context,node,kind,modl,elem,imodl,val)
    type(pm_context),pointer:: context
    integer,intent(in):: kind
    type(pm_ptr),intent(in):: node,modl,elem,imodl,val
    type(pm_ptr),target:: dict,lcl_dict,idict,old
    type(pm_reg),pointer:: reg
    character(len=100):: str,str2
    logical:: changed
    reg=>pm_register(context,'include elem',dict,lcl_dict,idict,old)
    if(pm_debug_level>2) then
       call pm_name_string(context,elem%offset,str2)
       str='include elem '//trim(str2)
       call pm_name_string(context,int(get_modl_name(modl),pm_p),str2)
       str=trim(str)//' to '//trim(str2)
       call pm_name_string(context,int(get_modl_name(imodl),pm_p),str2)
       str=trim(str)//' from '//trim(str2)
       write(*,*) str
    endif
    dict=modl%data%ptr(modl%offset+kind)
    lcl_dict=modl%data%ptr(modl%offset+kind+modl_local)
    idict=imodl%data%ptr(imodl%offset+kind)
    if(kind==modl_param) then
       if(.not.pm_fast_isnull(pm_dict_lookup(context,dict,elem))) &
            call link_error(context,node,'Repeated definition:',elem)
       call pm_dict_set(context,&
            lcl_dict,elem,val,.true.,.false.,changed)
    else
       ! Check existing entry
       old=pm_dict_lookup(context,dict,elem)
       if(pm_fast_isnull(old)) &
            old=pm_dict_lookup(context,lcl_dict,elem)
       if(pm_fast_isnull(old)) then
          ! No existing entry - just add
          call pm_dict_set(context,&
               lcl_dict,elem,val,.true.,.true.,changed)
       else
          ! Merge proc or type lists
          call pm_ptr_assign(context,&
               old%data%ptr(old%offset+node_args+2),&
               int(proc_link,pm_ln),&
               val%data%ptr(val%offset+node_args+1))
          call pm_ptr_assign(context,old,int(node_args+2,pm_ln),&
               val%data%ptr(val%offset+node_args+2))
          call pm_dict_set(context,idict,elem,old,.true.,.true.,changed)
       endif
    endif
    call pm_delete_register(context,reg)
  contains
    include 'fisnull.inc'
  end subroutine link_include_elem

  ! Linker error
  subroutine link_error(context,node,mess,name)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: node
    character(len=*),intent(in):: mess
    type(pm_ptr),intent(in):: name
    character(len=100):: namestr,mnamestr,inamestr
    if(.not.pm_main_process) return
    call pm_name_string(context,name%offset,namestr)
    call pm_name_string(context,node_get_modl_name(node),&
         mnamestr)
    call pm_name_string(context,node%data%ptr(&
         node%offset+node_args)%offset,inamestr)
    write(*,*) 'Error: '//trim(mnamestr)//&
         ' include ',trim(inamestr)
    write(*,*) mess//trim(namestr)
  end subroutine link_error

end module pm_linker

