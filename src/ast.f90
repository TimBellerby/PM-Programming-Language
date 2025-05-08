!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2024
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

! Definitions for abstract syntax tree
! and some more general definition of flag values etc.

module pm_ast
  use pm_sysdep
  use pm_compbase
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_options
  use pm_hash
  use pm_symbol
  use pm_vmdefs
  use pm_types

  ! Langauge features
  integer,parameter:: num_comm_args=6
 
  ! Offsets into module objects
  integer,parameter:: modl_name=1
  integer,parameter:: modl_link=2
  integer,parameter:: modl_last=3
  integer,parameter:: modl_stmts=4
  integer,parameter:: modl_include=5
  integer,parameter:: modl_proc=6
  integer,parameter:: modl_type=7
  integer,parameter:: modl_param=8
  integer,parameter:: modl_local=3

  ! Offsets into parser node objects of various kinds
  integer,parameter:: node_magic=0
  integer,parameter:: node_symbol=1
  integer,parameter:: node_modl=2
  integer,parameter:: node_lineno=3
  integer,parameter:: node_charno=4
  integer,parameter:: node_args=5

  ! Type parse nodes
  integer,parameter:: type_name=node_args
  integer,parameter:: type_number=node_args+1
  integer,parameter:: type_module=node_args+2
  integer,parameter:: type_params=node_args+3
  integer,parameter:: type_constraints=node_args+4
  integer,parameter:: type_link=node_args+5
  integer,parameter:: type_parents=node_args+6
  integer,parameter:: type_includes=node_args+7
  integer,parameter:: type_num_args=8

  ! Proc parse nodes
  integer,parameter:: proc_name=node_args
  integer,parameter:: proc_link=node_args+1
  integer,parameter:: proc_module=node_args+2
  integer,parameter:: proc_flags=node_args+3
  integer,parameter:: proc_params=node_args+4
  integer,parameter:: proc_keys=node_args+5
  integer,parameter:: proc_amplocs=node_args+6
  integer,parameter:: proc_when=node_args+7
  integer,parameter:: proc_coded_results=node_args+8
  integer,parameter:: proc_coded_type=node_args+9
  integer,parameter:: proc_numret=node_args+10
  integer,parameter:: proc_result_types=node_args+11

  ! Alternative final sections for 'proc' parse nodes

  !   - user procs
  integer,parameter:: proc_reduce=node_args+12
  integer,parameter:: proc_check=node_args+13
  integer,parameter:: proc_result=node_args+14
  integer,parameter:: proc_stmts=node_args+15
  integer,parameter:: proc_code_tree=node_args+16
  integer,parameter:: proc_num_args=17

  !   - built in procs
  integer,parameter:: proc_retas=node_args+12
  integer,parameter:: proc_opcode=node_args+13
  integer,parameter:: proc_opcode2=node_args+14
  integer,parameter:: proc_data=node_args+15
  integer,parameter:: proc_coded_builtin=node_args+16
  integer,parameter:: sysproc_num_args=17

  ! Values for proc flags
  integer,parameter:: proccall_is_comm=       1
  integer,parameter:: proccall_is_ref =       2
  integer,parameter:: proccall_is_general =   4
  integer,parameter:: proccall_is_block =     8

  integer,parameter:: proccall_is_inline=     16
  integer,parameter:: proccall_is_no_inline=  32
  integer,parameter:: proccall_is_yield=      64

  integer,parameter:: proccall_is_lhs=        128
  integer,parameter:: proc_is_cond=           256
  integer,parameter:: proc_is_uncond=         512
  integer,parameter:: proc_run_complete=      2**10
  integer,parameter:: proc_run_local=         2**11
  integer,parameter:: proc_run_shared=        2**12
  integer,parameter:: proc_run_always=        2**13
  integer,parameter:: proc_is_open=           2**14
  integer,parameter:: proc_is_abstract=       2**15
  integer,parameter:: proc_is_generator =     2**16
  integer,parameter:: proc_needs_type =       2**17
  integer,parameter:: proc_is_recursive =     2**18
  integer,parameter:: proc_unfinished =       2**19
  
  integer,parameter:: proc_is_impure =        2**20
  integer,parameter:: proc_is_not_inlinable = 2**21
  integer,parameter:: proc_has_for =          2**22
  integer,parameter:: proc_is_not_pure_each = 2**23
  integer,parameter:: proc_has_vkeys =        2**24
  integer,parameter:: proc_is_dcomm =         2**25
  integer,parameter:: proc_is_file =          2**26
  integer,parameter:: proc_prints_out =       2**27

  ! Proc flags that can be taken as taints
  integer,parameter:: proc_taints = proc_is_impure &
       + proc_is_not_inlinable + proc_has_for      &
       + proc_is_dcomm + proc_is_file   &
       + proc_prints_out

  ! Flags for proc calls
  integer,parameter:: call_ignore_rules=      512
  integer,parameter:: call_is_fixed =         2**10
  integer,parameter:: call_is_assign_call =   2**11
  integer,parameter:: call_is_vararg =        2**12
  integer,parameter:: call_inline_when_compiling = 2**13
  integer,parameter:: call_dup_result =       2**14
  integer,parameter:: call_is_cond =          2**15
  integer,parameter:: call_is_no_touch =      2**16
  integer,parameter:: call_is_unlabelled =    2**17
  integer,parameter:: call_takes_uninit =     2**18
  integer,parameter:: call_converts_uninit =  2**19
  
contains

  !======================================================
  ! Check that a node is valid
  !======================================================
  subroutine check_node(node)
    type(pm_ptr),intent(in):: node
    if(pm_fast_vkind(node)==pm_pointer) then
       if(node%data%ptr(node%offset)%offset/=9876) then
          call pm_panic('Bad parse node')
       endif
    endif
  contains
    include 'fvkind.inc'
  end subroutine check_node

  !=======================================================
  ! Check that a node is valid and not a tiny int or value
  !=======================================================
  subroutine check_ptr_node(node)
    type(pm_ptr),intent(in):: node
    if(pm_fast_vkind(node)==pm_pointer) then
       if(node%data%ptr(node%offset)%offset/=9876) then
          call pm_panic('Bad parse node')
       endif
    else
       call pm_panic('not ptr parser node')
    endif
  contains
    include 'fvkind.inc'
  end subroutine check_ptr_node

  !======================================================
  ! Return symbol associated with a node
  !======================================================
  function node_sym(node) result(n)
    type(pm_ptr),intent(in):: node
    integer:: n
    if(pm_fast_vkind(node)/=pm_pointer) then
       n=0
    else
       if(pm_debug_checks) call check_node(node)
       n=node%data%ptr(node%offset+node_symbol)%offset
    endif
  contains
    include 'fvkind.inc'
  end function node_sym

  !======================================================
  ! Number of arguments in a node
  !======================================================
  function node_numargs(node) result(n)
    type(pm_ptr),intent(in):: node
    integer:: n
    if(pm_fast_vkind(node)/=pm_pointer) then
       n=0
    else
       if(pm_debug_checks) call check_node(node)
       n=pm_fast_esize(node)-node_args+1
    endif
  contains
    include 'fesize.inc'
    include 'fvkind.inc'
  end function node_numargs

  !======================================================
  ! Return n-th argument of a node
  !======================================================
  function node_arg(node,n) result(p)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    type(pm_ptr):: p
    if(pm_debug_checks) then
       call check_ptr_node(node)
       if(n<0.or.node_args+n-1>pm_fast_esize(node)) &
            call pm_panic('node_arg - n out of range')
    endif
    p=node%data%ptr(node%offset+node_args+n-1)
  contains
    include 'fesize.inc'
  end function node_arg

  !======================================================
  ! Return n-th argument of a node as a number
  ! (that argument should be tiny-int)
  !======================================================
  function node_num_arg(node,n) result(num)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    integer:: num
    type(pm_ptr):: p
    if(pm_debug_checks) then
       call check_ptr_node(node)
       if(n<0.or.node_args+n-1>pm_fast_esize(node)) &
            call pm_panic('node_arg - n out of range')
    endif
    p=node%data%ptr(node%offset+node_args+n-1)
    num=p%offset
  contains
    include 'fesize.inc'
  end function node_num_arg

  !======================================================
  ! Return n-th slot in a node (not the same as argument)
  !======================================================
  function node_get(node,n) result(p)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    type(pm_ptr):: p
    if(pm_debug_checks) then
       call check_ptr_node(node)
       if(n<0.or.n>pm_fast_esize(node)) &
            call pm_panic('node_get - n out of range')
    endif
    p=node%data%ptr(node%offset+n)
  contains
    include 'fesize.inc'
  end function node_get

  !======================================================
  ! Return n-th slot in a node (not the same as argument)
  ! as a number (must be tiny int)
  !======================================================
  function node_get_num(node,n) result(num)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    integer:: num
    type(pm_ptr):: p
    if(pm_debug_checks) then
       call check_ptr_node(node)
       if(n<0.or.n>pm_fast_esize(node)) &
            call pm_panic('node_get_num - n out of range')
    endif
    p=node%data%ptr(node%offset+n)
    num=p%offset
  contains
    include 'fesize.inc'
  end function node_get_num

  !======================================================
  ! Set n-th slot in a node (not the same as argument)
  ! to a number (tiny int)
  !======================================================
  subroutine node_set_num(node,n,num)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    integer,intent(in):: num
    if(pm_debug_checks) then
       call check_ptr_node(node)
       if(n<0.or.n>pm_fast_esize(node)) &
            call pm_panic('node_get_num - n out of range')
    endif
    node%data%ptr(node%offset+n)%offset=num
  contains
    include 'fesize.inc'
  end subroutine node_set_num

  !======================================================
  ! Get the line number associated with a node
  !======================================================
  function node_get_lineno(node) result(n)
    type(pm_ptr),intent(in):: node
    integer:: n
    if(pm_debug_checks) &
         call check_ptr_node(node)
    n=node%data%ptr(node%offset+node_lineno)%offset
  end function node_get_lineno

  !======================================================
  ! Get the character position (in source) associated
  ! with a node
  !======================================================
  function node_get_charno(node) result(n)
    type(pm_ptr),intent(in):: node
    integer:: n
    if(pm_debug_checks) &
         call check_ptr_node(node)
    n=node%data%ptr(node%offset+node_charno)%offset
  end function node_get_charno

  !======================================================
  ! Get the module object associated with a node
  !======================================================
  function node_get_modl(node) result(modl)
    type(pm_ptr),intent(in):: node
    type(pm_ptr):: modl
    if(pm_debug_checks) &
       call check_ptr_node(node)
    modl=node%data%ptr(node%offset+node_modl)
  contains
    include 'fvkind.inc'
  end function node_get_modl

  !======================================================
  ! Get the module name associated with a node
  !======================================================
  function node_get_modl_name(node) result(name)
    type(pm_ptr),intent(in):: node
    integer:: name
    type(pm_ptr):: modl
    if(pm_debug_checks) &
       call check_ptr_node(node)
    modl=node_get_modl(node)
    name=modl%data%ptr(modl%offset+modl_name)%offset
  end function node_get_modl_name


  !======================================================
  ! Dump a module (debugging)
  !======================================================
  subroutine dump_module(context,iunit,ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: ptr
    character(len=100):: str
    character(len=7),dimension(modl_include:modl_param):: dnames = &
         (/ &
         'include',&
         'proc   ',&
         'type   ',&
         'param  '/)
    integer:: i,j,k,m
    type(pm_ptr):: keys,vals,p
    call pm_name_string(context,int(ptr%data%ptr(ptr%offset+1)%offset),str)
    write(iunit,*) 'Module: ',trim(str)
    write(iunit,*) 'Stmts:'
    call dump_parse_tree(context,iunit,ptr%data%ptr(ptr%offset+modl_stmts),2)
    do k=0,modl_local,modl_local
       if(k==modl_local) then
          write(iunit,*) 'Local:'
          m=modl_proc
       else
          ! m=modl_include
          m=modl_proc
       endif
       do j=m,modl_param
          write(iunit,*) dnames(j),&
               marked(ptr%data%ptr(ptr%offset+j+k)),'::'
          keys=pm_dict_keys(context,ptr%data%ptr(ptr%offset+j+k))
          vals=pm_dict_vals(context,ptr%data%ptr(ptr%offset+j+k))
          write(iunit,*) marked(keys),marked(vals)
          do i=1,pm_dict_size(context,ptr%data%ptr(ptr%offset+j+k))
             call pm_name_string(context,&
                  int(keys%data%ptr(keys%offset+i-1)%offset),str)
             write(iunit,*) ' ',trim(str),'::'
             write(iunit,*) marked(vals%data%ptr(vals%offset+i-1))
             p=vals%data%ptr(vals%offset+i-1)
             call dump_parse_tree(context,iunit,&
                  p%data%ptr(p%offset),2)
          enddo
       enddo
    enddo
  end subroutine dump_module

  !======================================================
  ! Dump a parser tree (debugging)
  !======================================================
  recursive subroutine dump_parse_tree(context,iunit,ptr,depth)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: depth
    integer:: i, sym
    character(len=80),parameter:: spaces = ' '
    character(len=100):: str
    if(depth>30) then
       write(iunit,*) spaces(:depth*2),'>>>'
       return
    endif
    if(pm_fast_vkind(ptr)==pm_pointer) then
       if(ptr%offset<=0) then
          write(iunit,*) spaces(1:depth*2),'INVALID PTR'
          return
       elseif(ptr%data%ptr(ptr%offset)%offset/=9876) then
          if(ptr%data%ptr(ptr%offset)%offset==9875) then
             write(iunit,*) spaces(1:depth*2),'REUSED NODE',&
                  ptr%offset,ptr%data%hash,ptr%data%esize
          else
             write(iunit,*) spaces(1:depth*2),'INVALID NODE'
             return
          endif
       endif
       sym=ptr%data%ptr(ptr%offset+1)%offset
       if(sym>0.and.sym<=num_syshook) then
          write(iunit,*) spaces(1:depth*2),sym_names(sym),ptr%data%esize,&
               'line',node_get_lineno(ptr),&
               'Marked:',marked(ptr),&
               ptr%data%hash,ptr%offset,ptr%offset+ptr%data%esize
       else if(sym==0) then
          call pm_name_string(context,int(ptr%data%ptr(ptr%offset+1)%offset),str)
          write(iunit,*) spaces(1:depth*2),'Module: ',trim(str)
          return
       else
          write(iunit,*) spaces(1:depth*2),'???',trim(pm_name_as_string(context,sym))
          return
       endif
       do i=node_args,ptr%data%esize
          call dump_parse_tree(context,iunit,ptr%data%ptr(ptr%offset+i),&
               depth+1)
       enddo
    else if(pm_fast_isnull(ptr)) then
       write(iunit,*) spaces(1:depth*2),'NULL'
    else if(pm_fast_isname(ptr)) then
       call pm_name_string(context,int(ptr%offset),str)
       write(iunit,*) spaces(1:depth*2),'Name:',trim(str)
    else if(pm_fast_istiny(ptr)) then
       write(iunit,*) spaces(1:depth*2),'Tiny number:',ptr%offset
    else
       call pm_dump_tree(context,iunit,ptr,depth)
    endif
  contains
    include 'fvkind.inc'
    include 'fisnull.inc'
    include 'fisname.inc'
    include 'fistiny.inc'
  end subroutine dump_parse_tree
  
end module pm_ast
