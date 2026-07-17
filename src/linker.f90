!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2025
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
  logical,parameter:: debug_linker=.false.
  
contains

  !=================================================
  ! The following routines process linkage between
  ! modules
  !=================================================

  !===============================================================`
  ! Process all 'use' statements in all modules
  !===============================================================
  subroutine link_includes(context,nerror,modl_dict,sysmodl)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    type(pm_ptr),intent(in):: modl_dict,sysmodl
    type(pm_ptr):: modls,modl
    integer:: j
    modls=pm_dict_vals(context,modl_dict)
    ! Loop through all loaded modules
    do j=0,pm_dict_size(context,modl_dict)-1
       modl=modls%data%ptr(modls%offset+j)
       call link_includes_for_module(context,nerror,modl,sysmodl)
    enddo
  end subroutine link_includes

  !===============================================================
  ! Process all 'use' statements in a single module
  !===============================================================
  subroutine link_includes_for_module(context,nerror,modl,sysmodl)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    type(pm_ptr),intent(in):: modl,sysmodl
    type(pm_ptr):: modls,dict
    type(pm_ptr)::incls,node,imodl
    type(pm_ptr)::p
    type(pm_reg),pointer:: reg
    integer:: i
    character(len=100):: str
    dict=modl%data%ptr(modl%offset+modl_include)
    incls=pm_dict_vals(context,dict)
    ! Loop through include definitions for given module
    do i=0,pm_dict_size(context,dict)-1
       node=incls%data%ptr(incls%offset+i)
       if(debug_linker) then
          call pm_name_string(context,&
               int(node%data%ptr(node%offset+node_args)%offset),str)
          write(*,*) 'including',trim(str)
          p=pm_dict_key(context,dict,int(i+1,pm_ln))
          write(*,*) '..',trim(pm_name_as_string(context,int(p%offset)))
       endif
       imodl=node%data%ptr(node%offset+node_args+1)
       if(modl==imodl) then
          call link_error(context,nerror,node,'Module cannot "use" itself')
       endif
       if(imodl==sysmodl) then
          call link_include_sysmod(context,nerror,node,modl,imodl)
       elseif(node_sym(node)==sym_use) then
          call link_include(context,nerror,node,modl,imodl)
       else
          call link_include_mod(context,nerror,node,modl,imodl)
       endif
       if(debug_linker) then
          write(*,*) '... included ',trim(str)
       endif
    enddo
  end subroutine link_includes_for_module

  !==================================================================
  ! Process the implicit inclusion of the system module
  ! Here we import all names given
  ! - some calls to sysmod with new names are created in codegen
  ! - sysmod is at the root of everything, so this is not a problem
  !==================================================================
  subroutine link_include_sysmod(context,nerror,node,modl,imodl)
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
  end subroutine link_include_sysmod
  
  !===============================================================
  ! Process a single unmodified 'use' statement
  !===============================================================
  subroutine link_include(context,nerror,node,modl,imodl)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    type(pm_ptr),intent(in):: node,modl,imodl
    type(pm_ptr):: dict,names,elems,vals,elem,val
    integer:: i,j

    ! Import proc for every unqualified proc or type name (if in imported module)
    do j=modl_proc,modl_type
       dict=imodl%data%ptr(imodl%offset+modl_proc)
       names=modl%data%ptr(modl%offset+j-modl_proc+modl_proc_names)
       elems=pm_set_keys(context,names)
       do i=0,pm_set_size(context,names)-1
          elem=elems%data%ptr(elems%offset+i)
          elem=pm_fast_name(context,elem%data%i(elem%offset))
          val=pm_dict_lookup(context,dict,elem)
          if(.not.pm_fast_isnull(val)) then
             call link_include_elem(context,nerror,node,modl_proc,modl,elem,imodl,val)
          endif
       enddo
    enddo

    ! Import param for every entry in imported module
    ! (params have simple merge rules - they don't)
    dict=imodl%data%ptr(imodl%offset+modl_param)
    elems=pm_dict_keys(context,dict)
    vals=pm_dict_vals(context,dict)
    do j=0,pm_dict_size(context,dict)-1
       elem=elems%data%ptr(elems%offset+j)
       val=vals%data%ptr(vals%offset+j)
       call link_include_elem(context,nerror,node,modl_param,modl,elem,imodl,val)
    enddo

  contains
    include 'fname.inc'
    include 'fisnull.inc'
  end subroutine link_include

  !===============================================================
  ! Process a modified 'use' statement
  !===============================================================
  subroutine link_include_mod(context,nerror,node,modl,imodl)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    type(pm_ptr),intent(in):: node,modl,imodl
    integer:: i,j,kind
    type(pm_ptr):: list,name,dict,def,keys,defs,key,mnode,elems,elem,vals,val
    list=node_arg(node,3)
    keys=pm_dict_keys(context,list)
    defs=pm_dict_vals(context,list)
    do i=0,pm_dict_size(context,list)-1
       key=keys%data%ptr(keys%offset+i)
       kind=key%data%i(key%offset)
       name=pm_fast_name(context,int(key%data%i(key%offset+1)))
       mnode=defs%data%ptr(defs%offset+i)
       dict=imodl%data%ptr(imodl%offset+kind)
       def=pm_dict_lookup(context,dict,name)
       if(pm_fast_isnull(def)) then
          call link_error(context,nerror,mnode,&
               'Unknown name in include modifier: ',name)
       endif
       call link_include_elem(context,nerror,mnode,kind,modl,name,imodl,def)
    enddo
    
  contains
    include 'fisnull.inc'
    include 'fname.inc'
  end subroutine link_include_mod

  !===============================================================
  ! Include a single named element
  !===============================================================
  subroutine link_include_elem(context,nerror,node,kind,modl,name,imodl,def)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    integer,intent(in):: kind
    type(pm_ptr),intent(in):: node,modl,name,imodl,def
    type(pm_ptr),target:: dict,lcl_dict,current,v1,v2
    logical:: changed
    if(debug_linker) then
       write(*,*) 'Include elem: ',trim(pm_name_as_string(context,int(name%offset))),&
            ' to ',trim(pm_name_as_string(context,get_modl_name(modl))),&
            ' from ',trim(pm_name_as_string(context,get_modl_name(imodl)))
    endif
    dict=modl%data%ptr(modl%offset+kind)
    lcl_dict=modl%data%ptr(modl%offset+kind+modl_local)
    
    ! Check if this element is used by the importing module
    !  - if not then we do not merge definitions
    !if(.not.check_use(context,modl,kind,name)) return

    
    ! Check existing entry
    current=pm_dict_lookup(context,dict,name)
    if(pm_fast_isnull(current)) then
       current=pm_dict_lookup(context,lcl_dict,name)
    endif
       
    if(pm_fast_isnull(current)) then
 
       if(kind==modl_param) then
          ! For a parameter, need a new definition that can
          ! hold an error node if needed
          current=pm_new(context,pm_pointer,1_pm_ln)
          current%data%ptr(current%offset)=def%data%ptr(def%offset)
       else
          current=def
       endif
       call pm_dict_set(context,&
            lcl_dict,name,current,.true.,.false.,changed)
    else
       call link_merge(context,nerror,node,kind,current,def)
    endif
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
  end subroutine link_include_elem

  !===============================================================
  ! Merge two definitions if allowed
  ! updated definition stored in v1
  !===============================================================
  subroutine link_merge(context,nerror,node,kind,current,def) 
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    integer,intent(in):: kind
    type(pm_ptr),intent(in):: node,current,def
    type(pm_ptr):: last,name,v1,v2
    integer(pm_ln),parameter:: ptr_to_first=node_args+1
    integer(pm_ln),parameter:: ptr_to_last=node_args+2
    v1=current%data%ptr(current%offset)
    v2=def%data%ptr(def%offset)
    if(v1==v2) return
    select case(kind)
    case(modl_param)
       ! An imported def is size 1 (esize 0), def in the importing module is size 2 (esize 1)
       if(pm_fast_esize(current)==1) then
          !  This is always an error
          call link_error(context,nerror,v1,&
               'Parameter declaration clashes with definition from an imported module')
          call link_error(context,nerror,v2,&
               '... declaration accessed via "use" module')
          call link_error(context,nerror,node,&
               '... declarations joined here')
       else
          ! Create an error node - deferred error if this entry is accessed
          call pm_ptr_assign(context,current,0_pm_ln,make_error_node(context,node,v1,v2))
       endif
    case(modl_proc,modl_method,modl_cproc)
       ! Group the procs together
       call pm_ptr_assign(context,&
            v1%data%ptr(v1%offset+ptr_to_last),&
            int(proc_link,pm_ln),&
            v2%data%ptr(v2%offset+ptr_to_first))
       call pm_ptr_assign(context,v1,ptr_to_last,&
            v2%data%ptr(v2%offset+ptr_to_last))
       call link_join(context,nerror,current,def,v1)
    case(modl_type)
       ! Group the types together
       last=v1%data%ptr(v1%offset+ptr_to_last)
       if(node_sym(last)==sym_in) then
          call pm_ptr_assign(context,&
               last,&
               int(node_args,pm_ln),&
               v2%data%ptr(v2%offset+ptr_to_first))
       else
          call pm_ptr_assign(context,&
               last,&
               int(type_link,pm_ln),&
               v2%data%ptr(v2%offset+ptr_to_first))
       endif
       call pm_ptr_assign(context,v1,ptr_to_last,&
            v2%data%ptr(v2%offset+ptr_to_last))
       call link_join(context,nerror,current,def,v1)
    case default
       call pm_panic('link_merge -- unknown kind')
    end select
  contains
    include 'fesize.inc'
  end subroutine  link_merge

  !===============================================================
  ! Join two groups of definitions and set value
  ! of all members in combined set
  !===============================================================
  subroutine link_join(context,nerror,node1,node2,def)
    type(pm_context),pointer:: context
    integer,intent(inout):: nerror
    type(pm_ptr),intent(in):: node1,node2,def
    type(pm_ptr):: p1,p2
    if(node1==node2) return
    p1=node1%data%ptr(node1%offset+1)
    p2=node2%data%ptr(node2%offset+1)
    call pm_ptr_assign(context,node1,1_pm_ln,p2)
    call pm_ptr_assign(context,node2,1_pm_ln,p1)
    p2=p1
    do
       p1%data%ptr(p1%offset)=def
       p1=p1%data%ptr(p1%offset+1)
       if(p1==p2) exit
    enddo
  end subroutine link_join

  !===============================================================
  ! Create an error node when merging two param declarations
  ! This is a deferred error - codegen will error if if accesses
  ! this node
  !===============================================================
  function make_error_node(context,node,v1,v2) result(enode)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v1,v2,node
    type(pm_ptr):: enode
    enode=pm_new(context,pm_pointer,7_pm_ln)
    enode%data%ptr(enode%offset:enode%offset+4)=node%data%ptr(node%offset:node%offset+4)
    enode%data%ptr(enode%offset+node_symbol)%offset=sym_error
    enode%data%ptr(enode%offset+5)=v1
    enode%data%ptr(enode%offset+6)=v2
  end function make_error_node

  !===============================================================
  ! Linker error
  !===============================================================
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

