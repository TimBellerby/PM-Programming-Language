!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2016
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

!================================================
! The following routines process the parse tree
! into a structure closer to the final code
!------------------------------------------------
! cblocks - lists of calls
! calls   - refer to var, const, and
!           cblocks (for control structures)
!================================================

module pm_codegen
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_types
  use pm_parser
  use pm_sysdefs
  implicit none

  ! Limits
  integer,parameter:: max_code_stack=4096
  integer,parameter:: code_local_hash=1024
  integer,parameter:: max_loop_depth=256

  ! Offsets common to all cnode structures
  integer,parameter:: cnode_kind=0
  integer,parameter:: cnode_modl_name=1
  integer,parameter:: cnode_lineno=2
  integer,parameter:: cnode_args=3

  ! Types of cnode structure
  integer,parameter:: cnode_is_cblock=1
  integer,parameter:: cnode_is_var=2
  integer,parameter:: cnode_is_const=3
  integer,parameter:: cnode_is_call=4
  integer,parameter:: cnode_is_arglist=5
  integer,parameter:: cnode_is_builtin=6
  integer,parameter:: cnode_is_proc=7
  integer,parameter:: cnode_is_single_proc=8
  integer,parameter:: cnode_is_multi_proc=9
  integer,parameter:: cnode_is_var_proc=10
  integer,parameter:: cnode_is_arg_constraint=11
  integer,parameter:: cnode_is_par_constraint=12
  integer,parameter:: cnode_num_kinds=12

  ! Offsets into cblock cnodes
  integer,parameter:: cblock_parent=cnode_args+0
  integer,parameter:: cblock_first_var=cnode_args+1
  integer,parameter:: cblock_last_var=cnode_args+2
  integer,parameter:: cblock_first_call=cnode_args+3
  integer,parameter:: cblock_last_call=cnode_args+4
  integer,parameter:: cblock_sym=cnode_args+5
  integer,parameter:: cblock_start=cnode_args+6
  integer,parameter:: cblock_flags=cnode_args+7
  integer,parameter:: cblock_index=cnode_args+8
  integer,parameter:: cblock_node_size=9

  ! Flags for cblocks
  integer,parameter:: cblock_contains_at=1

  ! Offsets into call cnodes
  integer,parameter:: call_args=cnode_args+0
  integer,parameter:: call_parent=cnode_args+1
  integer,parameter:: call_sig=cnode_args+2
  integer,parameter:: call_flags=cnode_args+3
  integer,parameter:: call_link=cnode_args+4
  integer,parameter:: call_nret=cnode_args+5
  integer,parameter:: call_nkeys=cnode_args+6
  integer,parameter:: call_nloop=cnode_args+7
  integer,parameter:: call_index=cnode_args+8
  integer,parameter:: call_loop_depth=cnode_args+9
  integer,parameter:: call_node_size=10

  ! Flags for call nodes
  integer,parameter:: call_is_reduce_call = 1
  integer,parameter:: call_is_loop_call = 2
  integer,parameter:: call_is_assign_call = 4
  integer,parameter:: call_is_vararg = 8

  ! Offsets into var cnodes
  integer,parameter:: var_parent=cnode_args+0
  integer,parameter:: var_name=cnode_args+1
  integer,parameter:: var_flags=cnode_args+2
  integer,parameter:: var_link=cnode_args+3
  integer,parameter:: var_index=cnode_args+4
  integer,parameter:: var_loop_depth=cnode_args+5
  integer,parameter:: var_create_depth = cnode_args + 6
  integer,parameter:: var_set_call=cnode_args+7
  integer,parameter:: var_get_call=cnode_args+8
  integer,parameter:: var_node_size=9

  ! Flags for var cnodes
  integer(pm_p),parameter:: var_const=1
  integer(pm_p),parameter:: var_ref=2
  integer(pm_p),parameter:: var_param=4
  integer(pm_p),parameter:: var_amp=8
  integer(pm_p),parameter:: var_iter=16
  integer(pm_p),parameter:: var_shadow=32
  integer(pm_p),parameter:: var_loop_param=64
  integer(pm_p),parameter:: var_accessed=128
  integer(pm_p),parameter:: var_changed=256
  integer(pm_p),parameter:: var_multi_access=512
  integer(pm_p),parameter:: var_used_key=1024

  ! Offsets into proc nodes
  integer,parameter:: pr_cblock=cnode_args+0
  integer,parameter:: pr_max_index=cnode_args+1
  integer,parameter:: pr_recurse=cnode_args+2
  integer,parameter:: pr_id=cnode_args+3
  integer,parameter:: pr_nargs=cnode_args+4
  integer,parameter:: pr_nkeys=cnode_args+5
  integer,parameter:: pr_nret=cnode_args+6
  integer,parameter:: pr_flags=cnode_args+7
  integer,parameter:: pr_name=cnode_args+8
  integer,parameter:: pr_node_size=9

  ! Offsets into builtin nodes
  integer,parameter:: bi_opcode=cnode_args+0
  integer,parameter:: bi_opcode2=cnode_args+1
  integer,parameter:: bi_data=cnode_args+2
  integer,parameter:: bi_flags=cnode_args+3
  integer,parameter:: bi_rtype=cnode_args+4
  integer,parameter:: bi_rcode=cnode_args+5
  integer,parameter:: bi_rsym=cnode_args+6
  integer,parameter:: bi_id=cnode_args+7
  integer,parameter:: bi_node_size=8
  
  ! Flags indicating start/end of a block of type variables
  ! as opposed to regular variables on variables stack
  integer,parameter:: typevar_start=-88
  integer,parameter:: typevar_end=-99

  ! Number of extra arguments to loop call
  integer,parameter:: loop_call_extra_args=4

  ! Maximum number of coding errors before exit
  integer,parameter:: max_code_errors = 20

  ! State of the code generator
  type code_state

     ! Link to memory management
     type(pm_context),pointer:: context
     type(pm_reg),pointer:: reg,reg2,reg3

     ! Stack for local variables (stack() for names, var() for info records)
     ! Fixed size hash table to search variable names (chained using link())
     integer(pm_i16),dimension(max_code_stack):: stack,link
     type(pm_ptr),dimension(max_code_stack):: var
     integer,dimension(code_local_hash):: hash
     integer:: top

     ! Stack of values for creating cnodes
     type(pm_ptr),dimension(max_code_stack):: vstack
     integer:: vtop

     ! Stack of words - primarily used for types
     integer(pm_i16),dimension(max_code_stack):: wstack
     integer:: wtop

     ! Code cblock for program
     type(pm_ptr):: prog_cblock

     ! Constants for current procedure
     type(pm_ptr):: params
     
     ! Flags for current procedure
     integer:: proc_flags

     ! For statements - import/export 
     type(pm_ptr):: loop_cblock
     type(pm_ptr),dimension(max_loop_depth):: &
          imports,import_cblock
     integer:: loop_depth,proc_loop_depth
     integer:: loop_base 

     ! Caches for call signatures and resolved procedures
     type(pm_ptr):: sig_cache,proc_cache

     ! List of deferred type checks
     type(pm_ptr):: typ_defer

     ! Procedures as values
     integer(pm_p):: call_sig
     type(pm_ptr):: proc_name_vals

     ! Misc values
     type(pm_ptr):: temp,temp2,true,false,one,check_mess

     ! Contextual information for this point in the traverse
     type(pm_ptr):: sub_array, proc_keys
     integer:: proc_base,sub_index, proc_nret

     ! Counter to give each proc a unique index
     integer(pm_p):: id
     
     ! Counter to provide unique index for all nodes created
     integer(pm_p):: index
 
     ! Error count
     integer:: num_errors

  end type code_state

contains

  ! Initialise code generator structure
  subroutine init_coder(context,coder)
    type(pm_context),pointer:: context
    type(code_state),intent(out):: coder
    type(pm_ptr):: sig
    integer:: sym
    coder%context=>context
    coder%top=0
    coder%vtop=0
    coder%wtop=0
    coder%hash=0
    coder%reg=>pm_register(context,'coder-var stack',coder%temp,coder%temp2,&
         coder%sig_cache,coder%proc_cache,coder%true,coder%false,coder%one,&
         array=coder%var,array_size=coder%top)
    coder%reg2=>pm_register(context,'coder-node stack',&
         coder%params,coder%proc_name_vals,array=coder%vstack,array_size=coder%vtop)
    coder%reg3=>pm_register(context,'coder-for stack',coder%typ_defer,&
         coder%check_mess,array=coder%imports,array_size=coder%loop_depth)
    coder%sig_cache=pm_dict_new(context,32_pm_ln)
    coder%proc_cache=pm_dict_new(context,32_pm_ln)
    coder%params=pm_dict_new(context,32_pm_ln)
    coder%prog_cblock=pm_null_obj
    coder%typ_defer=pm_null_obj
    coder%proc_base=0
    coder%sub_index=0
    coder%loop_base=0
    coder%loop_depth=0
    coder%sub_array=pm_null_obj
    coder%loop_cblock=pm_null_obj
    coder%proc_keys=pm_null_obj
    coder%index=0
    coder%true=pm_new_small(context,pm_logical,1_pm_p)
    coder%true%data%l(coder%true%offset)=.true.
    coder%false=pm_new_small(context,pm_logical,1_pm_p)
    coder%false%data%l(coder%false%offset)=.false.
    coder%one=pm_new_small(context,pm_int16,1_pm_p)
    coder%one%data%i16(coder%one%offset)=1_pm_i16
    coder%one=pm_fast_tinyint(coder%context,pm_intern_val(coder%context,coder%one))
    coder%check_mess=pm_new_string(coder%context,'Failed "check"')
    coder%proc_name_vals=pm_null_obj
    coder%id=0
  contains
    include 'fname.inc'
    include 'ftiny.inc'
  end subroutine init_coder

  ! Finalise and delete code generator
  subroutine delete_coder(coder)
    type(code_state),intent(inout):: coder
    call pm_delete_register(coder%context,coder%reg)
    call pm_delete_register(coder%context,coder%reg2)
    call pm_delete_register(coder%context,coder%reg3)
  end subroutine delete_coder

  ! Start traversing the program
  subroutine trav_prog(coder,statlist)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: statlist
    type(pm_ptr):: prog_cblock
    
    ! Traverse+code statement list, preceded by parameter expressions
    prog_cblock=make_cblock(coder,pm_null_obj,statlist,sym_do)
    coder%prog_cblock=prog_cblock
    call trav_statlist(coder,prog_cblock,statlist,statlist,sym_do)
    call code_params(coder,prog_cblock,statlist)
    call make_sp_call(coder,prog_cblock,statlist,sym_do,1,0)
    call close_cblock(coder,prog_cblock)

    ! Complete type definitions
    call complete_types(coder)

    ! Finalise any var calls
    call complete_vcall_sigs(coder)

    ! Sort signatures
    call sort_sigs(coder)

  contains
    include 'fnewnc.inc'
    include 'fname.inc'
  end subroutine trav_prog

  ! Traverse statement list - push cblock onto stack
  recursive subroutine trav_statlist(coder,parent,&
       listp,list,lsym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: parent,listp,list
    integer,intent(in):: lsym
    type(pm_ptr):: cblock
    cblock=make_cblock(coder,parent,listp,lsym)
    call trav_open_statlist(coder,cblock,&
       listp,list)
    call close_cblock(coder,cblock)
  end subroutine trav_statlist

  ! Traverse open list of statements - add to passed cblock
  recursive subroutine trav_open_statlist(coder,cblock,&
       listp,list)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,listp,list
    integer:: i,j,n,sym,base,vbase
    type(pm_ptr):: node,cblock2,var
    if(pm_fast_isnull(list)) goto 10
    do i=1,node_numargs(list)
       vbase=coder%vtop
       node=node_arg(list,i)
       sym=node_sym(node)
       if(pm_debug_level>2) then
          write(*,*) 'TRAVERSE>',sym_names(sym),coder%vtop,vbase
       endif
       select case(sym)
       case(sym_if)
          call trav_xexpr(coder,cblock,node,&
               node_arg(node,1))
          call trav_statlist(coder,cblock,node,&
               node_arg(node,2),sym_if)
          if(.not.pm_fast_isnull(node_arg(node,3))) then
             call trav_statlist(coder,cblock,&
                  node,node_arg(node,3),sym_if)
          else
             call code_null(coder)
          endif
          call make_sp_call(coder,cblock,node,&
               sym_if,3,0)
       case(sym_select)
          call trav_xexpr(coder,cblock,node,&
               node_arg(node,1))
          var=top_code(coder)
          call trav_select(coder,cblock,node,2,var)
          call drop_code(coder)
       case(sym_while)
          cblock2=make_cblock(coder,cblock,node,sym_while)
          call trav_xexpr(coder,cblock2,node,&
               node_arg(node,1))
          call close_cblock(coder,cblock2)
          call trav_statlist(coder,cblock,node,&
               node_arg(node,2),sym_while)
          call make_sp_call(coder,cblock,node,sym_while,3,0)
       case(sym_repeat)
          cblock2=make_cblock(coder,cblock,node,sym_repeat)
          call trav_open_statlist(coder,cblock2,node,&
               node_arg(node,1))
          call trav_xexpr(coder,cblock2,node,node_arg(node,2))
          call make_sp_call(coder,cblock,node,&
               sym_repeat,2,0)
          call close_cblock(coder,cblock2)
       case(sym_par_loop,sym_par_find,sym_conc_loop,sym_conc_find,sym_loop,sym_find)
          call trav_xexpr(coder,cblock,node,node_arg(node,2),node)
       case(sym_open)
          call trav_call(coder,cblock,list,node,0)
       case(sym_pct)
          call check_at_nesting(coder,cblock,node)
          call trav_call(coder,cblock,list,node,0)
       case(sym_assign,sym_define)
          call trav_assign(coder,cblock,list,node,sym)
       case(sym_const)
          call trav_assign_list(coder,cblock,list,node)
       case(sym_where,sym_check)
          call trav_xexpr(coder,cblock,listp,node)
       case(sym_sync)
          call check_at_nesting(coder,cblock,node)
          call trav_exprlist(coder,cblock,listp,node)
          n=node_numargs(node)
          call check_loop_args(coder,node,n,n)
          call make_sp_call(coder,cblock,node,sym_sync,&
               node_numargs(node),0)
       case default
          if(sym>0.and.sym<num_sym) then
             write(*,*) 'SYM=',sym_names(sym)
          else
             write(*,*) 'SYM=<non symbol> Number=',sym
          endif
          call pm_panic('Unknown node sym in trav_statlist')
       end select
       if(coder%vtop/=vbase) then
          write(*,*) 'Current code'
          write(*,*) '============'
          call dump_code_tree(coder,pm_null_obj,6,cblock,1)
          write(*,*) '============'
          write(*,*) 'Remaining node stack:',coder%vtop,vbase
          write(*,*) '======='
          do j=vbase+1,coder%vtop
             call dump_code_tree(coder,pm_null_obj,6,coder%vstack(j),1)
             write(*,*) '======='
          enddo
          call pm_panic('trav_open_statlist')
       endif
       if(pm_debug_level>3) write(*,*) 'TRAVERSED>',sym_names(sym)
    enddo
10  continue
  contains
    include 'fisnull.inc'
  end subroutine trav_open_statlist

  ! Select statement - cases and otherwise clause
  ! assumes expression is in var
  recursive subroutine trav_select(coder,cblock,stmt,idx,var)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    type(pm_ptr),intent(in):: stmt,var
    integer,intent(in):: idx
    type(pm_ptr):: cblock2
    integer:: base
    call make_temp_var(coder,cblock,stmt)
    call dup_code(coder)
    base=coder%vtop
    call code_val(coder,var)
    call trav_xexpr(coder,cblock,stmt,node_arg(stmt,idx))
    call make_sys_call(coder,cblock,stmt,&
         sym_checkcase,coder%vtop-base,1)
    call trav_statlist(coder,cblock,stmt,&
         node_arg(stmt,idx+1),sym_find)
    if(idx==node_numargs(stmt)-2) then
       call trav_statlist(coder,cblock,stmt,&
            node_arg(stmt,idx+2),sym_select)
    else
       cblock2=make_cblock(coder,cblock,stmt,sym_select)
       call trav_select(coder,cblock2,stmt,idx+2,var)
       call close_cblock(coder,cblock2)
    endif
    call make_sp_call(coder,cblock,stmt,sym_if,3,0)
  end subroutine trav_select

  ! Traverse assignment list 
  recursive subroutine trav_assign_list(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: i
    do i=1,node_numargs(node)
       call trav_assign(coder,cblock,node,node_arg(node,i),&
            node_sym(node))
    enddo
  end subroutine trav_assign_list

  ! Traverse single assignment 
  recursive subroutine trav_assign(coder,cblock,pnode,node,sym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer,intent(in):: sym
    integer:: i,n,base,nsym,rsym
    integer(pm_p):: flags
    type(pm_ptr):: rhs
    nsym=node_sym(node)
    n=node_numargs(node)-1 

    ! Right hand side(s)
    rhs=node_arg(node,node_numargs(node))
    rsym=node_sym(rhs)
    if(rsym==sym_par_loop.or.rsym==sym_conc_loop.or.rsym==sym_find.or.rsym==sym_par_find&
         .or.rsym==sym_conc_find.or.rsym==sym_where.or.rsym==sym_using) then
       base=coder%vtop
       do i=1,n
          call make_temp_var(coder,cblock,node)
       enddo
       do i=1,n
          call code_val(coder,coder%vstack(base+i))
       enddo
       call trav_xexpr(coder,cblock,rhs,node_arg(rhs,2),rhs)
    elseif(n>1) then
       base=coder%vtop
       do i=1,n
          call make_temp_var(coder,cblock,node)
       enddo
       do i=1,n
          call code_val(coder,coder%vstack(base+i))
       enddo
       call trav_call(coder,cblock,node,rhs,n)
    else
       call trav_expr(coder,cblock,node,rhs)
    endif

    ! Left hand sides
    if(nsym==sym_assign) then
       do i=n,1,-1
          call make_assignment(coder,cblock,node,node_arg(node,i))
       enddo
    else
       flags=0
       if(sym==sym_const.or.sym==sym_where) flags=var_const
       do i=n,1,-1
          call make_definition(coder,cblock,node,node_arg(node,i),flags)
       enddo
    endif
  contains
    include 'ftiny.inc'
    include 'fname.inc'
    include 'fisname.inc'
    include 'fisnull.inc'
  end subroutine trav_assign

  ! Variable in LHS expr
  subroutine trav_lhs_var(coder,cblock,pnode,name)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,name
    type(pm_ptr):: var
    logical:: local
    var=find_var(coder,name)
    if(.not.var_local(coder,var)) then
       call code_error(coder,pnode,&
            'Cannot assign to variable defined outside of loop: ',&
            name)
    endif
    if(.not.pm_fast_isnull(var)) then
       if(cnode_flags_set(var,var_flags,var_const)) then
          call code_error(coder,pnode,&
               'Cannot assign to: ',cnode_get(var,var_name))
       endif
       call cnode_set_flags(var,var_flags,var_changed)
       call code_val(coder,var)
    else
       call code_error(coder,pnode,&
            'Variable being assigned to has not been defined: ',name)
       call make_temp_var(coder,cblock,pnode)
       var=top_code(coder)
    endif
  contains
    include 'fisnull.inc'
  end subroutine trav_lhs_var

  ! Traverse named var or LHS yielding reference
  subroutine trav_lhs_or_name(coder,cblock,pnode,lhs)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,lhs
    if(pm_fast_isname(lhs)) then
       call trav_lhs_var(coder,cblock,pnode,lhs)
    else
       call trav_lhs(coder,cblock,pnode,lhs)
    endif
  contains
    include 'fisname.inc'
  end subroutine trav_lhs_or_name
  
  ! Traverse a left hand side yielding reference value
  recursive subroutine trav_lhs(coder,cblock,pnode,lhs)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,lhs
    integer:: sym,n,m
    logical:: save_loop
    if(pm_fast_isname(lhs)) then
       call make_temp_var(coder,cblock,pnode)
       call trav_lhs_var(coder,cblock,pnode,lhs)
       call make_sys_call(coder,cblock,pnode,sym_make_ref,1,1)
    else if(pm_fast_vkind(lhs)==pm_pointer) then
       sym=node_sym(lhs)
       select case(sym)
       case(sym_open_square) 
          call make_temp_var(coder,cblock,lhs)
          call dup_code(coder)
          call trav_lhs(coder,cblock,lhs,node_arg(lhs,1))
          call trav_index_list(coder,cblock,node_arg(lhs,2),m)
          call make_sys_call(coder,cblock,lhs,sym_make_subref,m+1,1)
       case(sym_open_brace)
          call make_temp_var(coder,cblock,lhs)
          call dup_code(coder)
          call trav_lhs(coder,cblock,lhs,node_arg(lhs,1))
          call trav_index_list(coder,cblock,node_arg(lhs,2),m)
          call make_sys_call(coder,cblock,lhs,sym_make_openref,m+1,1)
       case(sym_dot) 
          call make_temp_var(coder,cblock,lhs)
          call dup_code(coder)
          call trav_lhs(coder,cblock,lhs,node_arg(lhs,1))
          call make_const(coder,cblock,lhs,node_arg(lhs,2))
          call make_sp_call(coder,cblock,lhs,sym_dotref,2,1)
       case default
          call code_error(coder,pnode,&
               'Cannot assign to expression')
          call make_temp_var(coder,cblock,pnode)
       end select
    else
       call code_error(coder,pnode,&
            'Cannot assign to expression')
       call make_temp_var(coder,cblock,pnode)
    endif
  contains
    include 'fvkind.inc'
    include 'fisname.inc'
    include 'fisnull.inc'
    include 'ftiny.inc'
  end subroutine trav_lhs

  ! Assign expression on top of stack to lhs in node
  subroutine make_assignment(coder,cblock,node,arg)
    type(code_state):: coder
    type(pm_ptr):: cblock,node,arg
    if(pm_fast_isname(arg)) then
       call trav_lhs_var(coder,cblock,node,arg)
       call make_var_assignment(coder,cblock,node)
    elseif(node_sym(arg)==sym_underscore) then
       call drop_code(coder)
       return
    else
       call trav_lhs(coder,cblock,node,arg)
       call swap_code(coder)
       call make_assign_call(coder,cblock,node,sym_set_ref,2,0)
    endif
  contains
    include 'fisname.inc'
  end subroutine make_assignment

  ! Use expression on top of stack to create new variable
  subroutine make_definition(coder,cblock,node,arg,flags)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,arg
    integer,intent(in):: flags
    integer:: junk
    
    if(pm_fast_isname(arg)) then
       call make_var(coder,cblock,node,arg,flags)
       call swap_code(coder)
       ! Import init expression to current loop
       junk=import_arg_list(coder,cblock,node,1,0,0,0,0) 
       call make_basic_call(coder,cblock,node,sym_dup,1,1,coder%loop_depth,0)
    elseif(node_sym(arg)==sym_underscore) then
       call drop_code(coder)
    else
       call code_error(coder,node,&
            'Left hand side of definition must be variable name')
    endif
  contains
    include 'fisname.inc'
  end subroutine make_definition

  ! Create a new system variable from expr on top of stack
  subroutine define_sys_var(coder,cblock,node,name,depth)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: name,depth
    integer:: junk
    call make_sys_var(coder,cblock,node,name,0)
    call cnode_set_num(top_code(coder),var_loop_depth,depth)
    call swap_code(coder)
    junk=import_arg_list(coder,cblock,node,1,0,0,0,0) 
    call make_basic_call(coder,cblock,node,sym_dup,1,1,depth,0)
  end subroutine define_sys_var

    ! Create a new system variable from expr on top of stack
  subroutine init_sys_var(coder,cblock,node,var,depth)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,var
    integer,intent(in):: depth
    integer:: junk
    call code_val(coder,var)
    call cnode_set_num(var,var_loop_depth,depth)
    call swap_code(coder)
    junk=import_arg_list(coder,cblock,node,1,0,0,0,0) 
    call make_basic_call(coder,cblock,node,sym_dup,1,1,depth,0)
  end subroutine init_sys_var

  ! Assignment call to a variable
  subroutine make_var_assignment(coder,cblock,node,var)
    type(code_state):: coder
    type(pm_ptr):: cblock,node
    type(pm_ptr),optional::var
    type(pm_ptr):: v
    if(present(var)) then
       call code_val(coder,var)
       v=var
    else
       v=top_code(coder)
    endif
    call swap_code(coder)
    if(cnode_flags_set(v,var_flags,var_ref)) then
       call make_assign_call(coder,cblock,node,sym_set_ref,2,0)
    else
       call make_assign_call(coder,cblock,node,sym_assignment,2,0)
    endif
    call cnode_set_flags(v,var_flags,var_changed)
  end subroutine make_var_assignment

  ! Index list
  subroutine trav_index_list(coder,cblock,node,n)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(out):: n
    integer:: i,save_sub_index
    type(pm_ptr):: save_sub_array
    save_sub_index=coder%sub_index
    save_sub_array=coder%sub_array
    coder%sub_array=top_code(coder)
    n=node_numargs(node)
    do i=1,n
       coder%sub_index=i
       call trav_expr(coder,cblock,&
            node,node_arg(node,i))
    enddo
    coder%sub_index=save_sub_index
    coder%sub_array=save_sub_array
  end subroutine trav_index_list

  ! Traverse extended expression 
  recursive subroutine trav_xexpr(coder,cblock,exprp,exprn,stmt)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,exprp,exprn
    type(pm_ptr),intent(in),optional:: stmt
    type(pm_ptr)::p,pp,mess
    integer:: i,j,base,top
    p=exprn
    if(node_sym(p)==sym_where) then
       base=coder%top
       do 
          call trav_assign_list(coder,cblock,p,node_arg(p,2))
          p=node_arg(p,1)
          if(node_sym(p)/=sym_where) exit
       enddo
       top=coder%top
    else
       base=-1
    endif
    if(node_sym(p)==sym_check) then
       call apply_x(p,node_arg(p,1))
       mess=node_arg(p,2)
       if(pm_fast_isnull(mess)) then
          mess=coder%check_mess
       endif
       call make_const(coder,cblock,p,mess)
       p=node_arg(p,3)
       do i=1,node_numargs(p)
          pp=node_arg(p,i)
          if(node_sym(pp)==sym_query_assign) then
             call trav_expr(coder,cblock,pp,node_arg(pp,1))
             call trav_expr(coder,cblock,pp,node_arg(pp,2))
             call make_sp_call(coder,cblock,pp,sym_check,3,0)
          else
             call trav_expr(coder,cblock,p,pp)
             call make_sp_call(coder,cblock,pp,sym_check,2,0)
          endif
       enddo
    else
       call apply_x(exprp,p)
    endif
    if(base>=0) call hide_vars(coder,base+1,top)
  contains
    include 'fisnull.inc'
    include 'fisname.inc'
    include 'fname.inc'
    include 'ftiny.inc'

    subroutine apply_x(qp,q)
      type(pm_ptr),intent(in):: qp,q
      type(pm_ptr):: var,fol,cblock2,qq,arg,old_proc_grid
      integer:: i,j,k,lbase,nq,iter,sym,nx
      integer(pm_p)::flag
      type(pm_ptr):: vlhs

      if(pm_fast_isnull(q)) return
      select case(node_sym(q))
      case(sym_const,sym_find)
         call trav_assign_list(coder,cblock,qp,q)
      case(sym_assign,sym_define)
         call trav_assign(coder,cblock,qp,q,node_sym(q))
      case(sym_iter)
         call trav_for(coder,cblock,qp,q,base,stmt)
      case(sym_list)
         call trav_exprlist(coder,cblock,qp,q)
      case default
         call trav_expr(coder,cblock,qp,q)
      end select
    end subroutine apply_x

  end subroutine trav_xexpr

  ! Traverse a for statement
  recursive subroutine trav_for(coder,cblock,listp,list,base,stmt)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,listp,list,stmt
    integer,intent(in):: base
    type(pm_ptr):: var,fol
    type(pm_ptr):: cblock2,cblock3,cblock4
    integer:: i,j,k,n,lbase,vbase,xbase,xbasev,nlist,iter,iter2,sym,rbase,nret
    integer:: find_var_pos, found_var_pos
    integer(pm_p)::flag
    type(pm_ptr):: vlhs,procs,sig
    integer:: save_loop_base
    type(pm_ptr):: save_loop_cblock
    sym=node_sym(stmt)
    rbase=coder%vtop
 
    ! Variable(s) holding result from find
    if(sym==sym_par_find.or.sym==sym_conc_find) then
       ! Variable holding success 
       call make_int_const(coder,cblock,list,-1)
       call define_sys_var(coder,cblock,list,sym_find,coder%loop_depth)
       
       ! Variable holding "found" message channel
       call make_temp_var(coder,cblock,list)
       call dup_code(coder)
       call make_sys_call(coder,cblock,list,sym_start_find,0,1)
       call define_sys_var(coder,cblock,list,sym_query,coder%loop_depth)
       find_var_pos=coder%top
    endif

    if(sym==sym_par_find.or.sym==sym_conc_find.or.sym==sym_find) then
       ! "otherwise" expressions
       nret=coder%vtop
       call trav_xexpr(coder,cblock3,stmt,node_arg(stmt,6))
       nret=coder%vtop-nret
       do i=nret,1,-1
          call code_val(coder,coder%vstack(rbase-nret+i))
          call swap_code(coder)
          call make_sys_call(coder,cblock,stmt,sym_dup,1,1)
       enddo
    endif

    ! Expressions to iterate over
    nlist=node_numargs(list)
    lbase=coder%vtop

    do i=nlist/2+1,nlist
       fol=node_arg(list,i)
       if(node_sym(fol)==sym_from) then
          call trav_from_follow(coder,cblock,list,fol)
       else
          call trav_expr(coder,cblock,list,fol)
       endif
    enddo
    
    ! Hide any where clauses (may need them later)
    if(base>=0) then
       call hide_vars(coder,base+1,coder%top)
    endif

    vbase=coder%top

    ! Calculate common domain/shape
    call make_temp_var(coder,cblock,list)
    call dup_code(coder)
    call repl_exprs(coder,coder%vtop-nlist/2-1,coder%vtop-2)
    call make_sys_call(coder,cblock,list,&
         sym_check_conform,nlist/2,1)
    call define_sys_var(coder,cblock,list,sym_for,coder%loop_depth)
    if(sym==sym_par_loop.or.sym==sym_par_find.or.sym==sym_conc_loop.or.sym==sym_conc_find) then
       
       save_loop_base=coder%loop_base
       save_loop_cblock=coder%loop_cblock

       ! Variable sym_proc - processor info for this loop
       call make_sys_var(coder,cblock,list,sym_proc,0)
       
       ! Variable sym_using - disribution for this loop
       call make_sys_var(coder,cblock,list,sym_using,0)
       
       if(sym==sym_conc_find.or.sym==sym_conc_loop) then
          
          iter=coder%top+1
          call make_var_tab_entry(coder,&
               pm_fast_name(coder%context,int(sym_in,pm_p)),&
               coder%var(iter-3))
          ! sym_proc,sym_using := conc()
          call make_temp_var(coder,cblock,list)
          call make_temp_var(coder,cblock,list)
          call code_val(coder,coder%vstack(coder%vtop-1))
          call code_val(coder,coder%vstack(coder%vtop-1))
          call make_sys_call(coder,cblock,list,sym_concurrent,0,2)
          call init_sys_var(coder,cblock,list,coder%var(iter-1),coder%loop_depth)
          call init_sys_var(coder,cblock,list,coder%var(iter-2),coder%loop_depth)
       else
          
          ! Variable sym_in computed using 'single' function
          call make_temp_var(coder,cblock,list)
          call dup_code(coder)
          call code_val(coder,coder%var(coder%top-2))
          call make_sys_call(coder,cblock,list,sym_single,1,1)
          call define_sys_var(coder,cblock,list,sym_in,coder%loop_depth)
          iter=coder%top
          
          ! If more than one current processor compute new value for sym_in
          cblock2=make_cblock(coder,cblock,stmt,sym_hash)
          ! sym_proc,sym_using := partition(sym_for, using_key_args...)
          call make_temp_var(coder,cblock2,list)
          call make_temp_var(coder,cblock2,list)
          call code_val(coder,coder%vstack(coder%vtop-1))
          call code_val(coder,coder%vstack(coder%vtop-1))
          k=coder%vtop
          procs=find_sig(coder,list,pm_fast_name(coder%context,sym_partition),&
               pm_null_obj,0,1,2,0,sig)
          n=trav_keys(coder,cblock2,node_arg(stmt,1),sig,.false.)
          call code_val(coder,coder%var(iter-3))
          call make_full_call(coder,cblock2,stmt,procs,0,1,2,n,0,coder%loop_depth)
          call init_sys_var(coder,cblock2,list,coder%var(iter-1),coder%loop_depth)
          call init_sys_var(coder,cblock2,list,coder%var(iter-2),coder%loop_depth)
          
          ! sym_in =sym_using{this_prc()}
          call make_temp_var(coder,cblock2,stmt)
          call dup_code(coder)
          call code_val(coder,coder%var(iter-1))
          call make_temp_var(coder,cblock2,stmt)
          call dup_code(coder)
          call make_sys_call(coder,cblock2,stmt,sym_this_prc,0,1)
          call make_sys_call(coder,cblock2,stmt,sym_open_brace,2,1)
          call make_var_assignment(coder,cblock2,stmt,coder%var(iter))
          call close_cblock(coder,cblock2)
          call make_sp_call(coder,cblock,stmt,sym_hash,1,0)
       endif

       ! Variable sym_dollar set to number of elements in domain
       call make_temp_var(coder,cblock,list)
       call dup_code(coder)
       call code_val(coder,coder%var(iter))
       call make_sys_call(coder,cblock,list,sym_num_elements,1,1)
       call define_sys_var(coder,cblock,list,sym_dollar,coder%loop_depth)
       
       ! Variable sym_hash set to number of elements in micro-thread
       call make_sys_var(coder,cblock,list,sym_hash,0)
       
       ! Outer code block (contains imports/exports)
       cblock2=make_cblock(coder,cblock,list,sym_par_loop)
       call push_for_level(coder,cblock2,coder%var(iter+2))
       coder%loop_cblock=cblock2
       coder%loop_base=iter
       
       ! Variable sym_iter set to iteration indices in this thread
       call make_sys_var(coder,cblock2,list,sym_iter,0)
       call code_val(coder,coder%var(iter))
       call code_val(coder,coder%var(iter+2))
       call make_basic_call(coder,cblock2,list,&
            sym_generate,2,1,coder%loop_depth-1,0)

       ! Inner block - contains statements
       cblock3=make_cblock(coder,cblock2,list,sym_do)

       ! Get array/domain elements for this iteration
       do i=1,nlist/2
          call make_var(coder,cblock3,list,&
               node_arg(list,i),0_pm_p)
          call code_val(coder,coder%vstack(lbase+i))
          call code_val(coder,coder%var(iter+3))
          call make_sys_call(coder,cblock3,list,sym_open_brace,2,1)
       enddo

       ! Loop body
       call trav_open_statlist(coder,cblock3,stmt,node_arg(stmt,3))

       if(sym==sym_par_loop.or.sym==sym_conc_loop) then
          
          ! Process build clause
          if(node_numargs(stmt)==4) then
             nret=coder%vtop
             call trav_xexpr(coder,cblock3,stmt,node_arg(stmt,4))
             nret=coder%vtop-nret
             do i=nret,1,-1
                call code_val(coder,coder%vstack(rbase-nret+i))
                call swap_code(coder)
                var=top_code(coder)
                if(cnode_get_kind(var)==cnode_is_var) then
                   if(cnode_get_num(var,var_loop_depth)==coder%loop_depth) then
                      call code_error(coder,node_arg(stmt,4),&
                           '"build" expression must be loop invariant')
                   endif
                endif
                call make_sys_call(coder,cblock3,stmt,sym_dup,1,1)
             enddo
          endif

          ! Inner call to for
          call close_cblock(coder,cblock3)
          call make_sp_call(coder,cblock2,list,sym_for,1,0)

          if(base>=0) call reveal_vars(coder,base,vbase)
          
          ! Export variables that have changed
          xbase=coder%wtop
          xbasev=coder%vtop
          do i=1,nlist/2
             var=coder%var(iter+3+i)
             if(cnode_flags_set(var,var_flags,var_changed)) then
                call push_word(coder,int(i,pm_i16))
                call trav_lhs(coder,cblock2,list,&
                     node_arg(list,i+nlist/2))
                call code_val(coder,coder%var(iter+3))
                call code_val(coder,var)
                call make_basic_call(coder,cblock2,stmt,&
                     sym_export_array,1,1,coder%loop_depth-1,0)
             endif
          enddo

          ! Finish building par-loop call
          ! - body of loop
          call close_cblock(coder,cblock2)
          call code_val(coder,coder%var(iter+2))
          call code_val(coder,coder%loop_cblock)
          call code_val(coder,coder%var(iter+1))
          call make_basic_sp_call(coder,cblock,list,sym_par_loop,2,1,coder%loop_depth-1)
          call cnode_set_num(coder%var(iter+2),var_loop_depth,coder%loop_depth)

          call pop_for_level(coder,cblock,stmt)


          ! Sync changes across processors
          if(sym==sym_par_loop) then
             cblock2=make_cblock(coder,cblock,stmt,sym_hash)
             if(coder%wtop>xbase) then
                call make_temp_var(coder,cblock2,stmt)
                call dup_code(coder)
                call make_sys_call(coder,cblock2,stmt,sym_prc_grid,0,1)
                call define_sys_var(coder,cblock2,stmt,sym_pct,coder%loop_depth)
                iter2=call_start(cblock2)
                cblock3=make_cblock(coder,cblock2,stmt,sym_loop)
                do i=coder%wtop,xbase+1,-1
                   j=pop_word(coder)
                   call trav_lhs_or_name(coder,cblock3,list,node_arg(list,j+nlist/2))
                   call make_temp_var(coder,cblock3,list)
                   call dup_code(coder)
                   call code_val(coder,coder%var(iter-1))
                   call code_val(coder,coder%var(iter2))
                   call make_sys_call(coder,cblock3,stmt,sym_open_brace,2,1)
                   call code_val(coder,coder%var(iter2))
                   call make_assign_call(coder,cblock3,stmt,sym_sync_it,3,0)
                enddo
                call call_next(cblock3,iter2)
                call close_cblock(coder,cblock3)
                call code_val(coder,coder%var(iter2+1))
                call make_sp_call(coder,cblock2,stmt,sym_loop,2,0)
             endif
             call code_val(coder,coder%var(iter-2))
             call make_sys_call(coder,cblock2,stmt,sym_pop_prc,1,0)
             call close_cblock(coder,cblock2)
             call make_sp_call(coder,cblock,stmt,sym_hash,1,0)
          else
             call code_val(coder,coder%var(iter-2))
             call make_sys_call(coder,cblock,stmt,sym_pop_conc,1,0)
          endif
          
       else 

          ! Code 
          !    if e then ... else test_find(find_var_pos) endif 
          call trav_xexpr(coder,cblock3,stmt,node_arg(stmt,5))
          cblock4=make_cblock(coder,cblock3,list,sym_par_find)
          call trav_xexpr(coder,cblock4,stmt,node_arg(stmt,4))
          do i=1,nret
             var=top_code(coder)
             if(cnode_get_kind(var)==cnode_is_var) then
                if(cnode_get_num(var,var_loop_depth)==coder%loop_depth) then
                   call make_temp_var(coder,cblock,stmt)
                   call dup_code(coder)
                   call code_val(coder,var)
                   call make_basic_call(coder,cblock4,stmt,&
                        sym_extract,1,1,coder%loop_depth-1,0)
                   call cnode_set_num(top_code(coder),&
                        var_loop_depth,coder%loop_depth-1)
                   coder%vstack(coder%vtop-1)=coder%vstack(coder%vtop)
                   coder%vtop=coder%vtop-1
                endif
                call make_var_assignment(coder,cblock4,&
                     stmt,coder%vstack(rbase-nret+1))
             endif
          enddo
          call close_cblock(coder,cblock4)
          cblock4=make_cblock(coder,cblock3,list,sym_par_find)
          call code_val(coder,coder%var(find_var_pos+1))
          call make_sys_call(coder,cblock4,list,sym_test_find,1,0)
          call close_cblock(coder,cblock4)
          call make_sp_call(coder,cblock3,list,sym_if,3,0)
          
          ! par_find
          call close_cblock(coder,cblock3)
          call code_val(coder,cblock3)
          call make_sp_call(coder,cblock2,list,sym_for,1,0)
          call code_val(coder,coder%var(iter+2))
          call close_cblock(coder,cblock2)
          call code_val(coder,cblock2)          
          call make_basic_sp_call(coder,cblock,list,sym_par_loop,2,0,coder%loop_depth-1)
          call cnode_set_num(coder%var(iter+2),var_loop_depth,coder%loop_depth)
          call pop_for_level(coder,cblock,stmt)

          cblock2=make_cblock(coder,cblock,stmt,sym_hash)
          ! Synchronise:  sym_when = sync_find(sym_find)
          call make_var(coder,cblock2,list,&
               pm_fast_name(coder%context,int(sym_when,pm_p)),var_shadow)
          j=coder%top
          call code_val(coder,coder%var(find_var_pos))
          call make_sys_call(coder,cblock2,list,sym_sync_find,1,1)
          do i=1,nret
             call code_val(coder,coder%vstack(rbase-nret+i))
             call code_val(coder,coder%var(j))
             call make_assign_call(coder,cblock2,list,sym_broadcast,2,0)
          end do
          call code_val(coder,coder%var(iter-2))
          call make_sys_call(coder,cblock2,list,sym_pop_prc,1,0)
          call close_cblock(coder,cblock)
          call make_sp_call(coder,cblock,stmt,sym_hash,1,0)
       endif

       coder%loop_base=save_loop_base
       coder%loop_cblock=save_loop_cblock

    else
       ! For.. seq loop
       iter=call_start(cblock)

       ! Loop body
       cblock2=make_cblock(coder,cblock,list,sym_seq)

       ! Get array/domain elements for this iteration
       do i=1,nlist/2
          call make_var(coder,cblock2,list,&
               node_arg(list,i),0_pm_p)
          call code_val(coder,coder%vstack(lbase+i))
          call code_val(coder,coder%var(iter+3))
          call make_sys_call(coder,cblock2,list,sym_open_brace,2,1)
       enddo

       ! Loop body
       call trav_open_statlist(coder,cblock2,stmt,node_arg(stmt,3))
       
       if(sym==sym_loop) then

          ! modify changed array elements
          do i=1,nlist/2
             var=coder%var(iter+3+i)
             if(cnode_flags_set(var,var_flags,var_changed)) then
                call make_temp_var(coder,cblock2,list)
                call dup_code(coder)
                call trav_lhs(coder,cblock2,list,&
                     node_arg(list,i+nlist/2))
                call code_val(coder,coder%var(iter+3))
                call make_sys_call(coder,cblock2,list,&
                     sym_make_openref,2,1)
                call code_val(coder,var)
                call code_val(coder,coder%vstack(coder%vtop-1))
                call make_assign_call(coder,cblock2,list,sym_assignment,2,0)
             endif
          enddo
          
          ! Next iteration
          call call_next(cblock2,iter)

       else

          ! Find statement
          call trav_xexpr(coder,cblock2,stmt,node_arg(stmt,5))
          cblock3=make_cblock(coder,cblock2,list,sym_seq)
          call trav_xexpr(coder,cblock3,stmt,node_arg(stmt,4))
          do i=nret,1,-1
             call make_var_assignment(coder,cblock3,stmt,&
                  coder%vstack(rbase-nret+i))
          enddo
          call make_const(coder,cblock3,stmt,pm_false_obj)
          call make_var_assignment(coder,cblock3,stmt,coder%var(iter+1))
          call close_cblock(coder,cblock3)
          cblock3=make_cblock(coder,cblock2,list,sym_seq)
          call call_next(cblock3,iter)
          call close_cblock(coder,cblock3)
          call make_sp_call(coder,cblock2,list,sym_if,3,0)
       endif

       ! Build call
       call close_cblock(coder,cblock2)
       call code_val(coder,coder%var(iter+1))
       if(sym==sym_find) then
          call make_sp_call(coder,cblock,list,sym_find,2,0)
       else
          call make_sp_call(coder,cblock,list,sym_loop,2,0)
       endif
    endif

    ! Clean up
    coder%vtop=rbase
    call pop_vars_to(coder,vbase)
  contains
    include 'fisnull.inc'
    include 'fisname.inc'
    include 'fname.inc'
    include 'ftiny.inc'

    function call_start(cblock) result(iter)
      type(pm_ptr),intent(in):: cblock
      integer:: iter

      iter=coder%top
      ! Code iter,state,not_end=first(domain)
      call make_temp_var(coder,cblock,list)
      call make_temp_var(coder,cblock,list)
      call make_temp_var(coder,cblock,list)
      call code_val(coder,coder%vstack(coder%vtop-2))
      call code_val(coder,coder%vstack(coder%vtop-2))
      call code_val(coder,coder%vstack(coder%vtop-2))      
      call code_val(coder,coder%var(iter))
      call make_sys_call(coder,cblock,list,sym_first,1,3)
      ! Loop end
      call define_sys_var(coder,cblock,list,sym_endfor,coder%loop_depth)
      ! State
      call define_sys_var(coder,cblock,list,sym_dollar,coder%loop_depth)
      ! Iterator
      call define_sys_var(coder,cblock,list,sym_iter,coder%loop_depth)

    end function  call_start


    ! Code either iter,state,end=next(domain,state,iter) or
    ! iter,state,end=first(domain)
    subroutine call_next(cblock,iter)
      type(pm_ptr),intent(in):: cblock
      integer,intent(in):: iter
      type(pm_ptr):: dvar,ivar,svar,evar
      dvar=coder%var(iter)
      ivar=coder%var(iter+3)
      svar=coder%var(iter+2)
      evar=coder%var(iter+1)
      call make_temp_var(coder,cblock,list)
      call make_temp_var(coder,cblock,list)
      call make_temp_var(coder,cblock,list)
      call code_val(coder,coder%vstack(coder%vtop-2))
      call code_val(coder,coder%vstack(coder%vtop-2))
      call code_val(coder,coder%vstack(coder%vtop-2))
      call code_val(coder,dvar)
      call code_val(coder,svar)
      call code_val(coder,ivar)
      call make_sys_call(coder,cblock,list,sym_next,3,3)
      call make_var_assignment(coder,cblock,list,evar)
      call make_var_assignment(coder,cblock,list,svar)
      call make_var_assignment(coder,cblock,list,ivar)
    end subroutine call_next    

  end subroutine trav_for

  ! Push a new loop level (enter for loop)
  subroutine push_for_level(coder,cblock,var)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,var
    integer:: depth
    depth=coder%loop_depth
    if(depth==max_loop_depth) &
         call pm_panic('Program too complex (nested loops)')
    coder%imports(depth+1)=pm_dict_new(coder%context,32)
    depth=depth+1
    coder%import_cblock(depth)=cblock
    coder%loop_depth=depth
  end subroutine push_for_level

  ! Pop down one for loop level (exit for loop)
  subroutine pop_for_level(coder,cblock,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    coder%imports(coder%loop_depth)=pm_null_obj
    coder%loop_depth=coder%loop_depth-1
  end subroutine pop_for_level

  ! Import a variable into loop at given depth
  function import_var_to_loop(coder,cblock,node,var,depth) result(ivar)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,var
    integer,intent(in):: depth
    type(pm_ptr):: ivar
    type(pm_ptr):: jvar,kvar,iblock
    integer:: i,j,vdepth,vcdepth
    vdepth=cnode_get_num(var,var_loop_depth)
    vcdepth=cnode_get_num(var,var_create_depth)
    if(pm_debug_level>3) then
       write(*,*) 'IMPORT TO:',&
            trim(pm_name_as_string(coder%context,int(cnode_get_num(var,var_name),pm_p))),&
            depth,vdepth,vcdepth,coder%loop_depth
    endif
    if(depth==0.or.depth==vdepth) then
       ivar=var
    elseif(vdepth>coder%loop_depth) then
       call pm_panic('import var to loop')
    elseif(vdepth>depth) then
       call make_temp_var(coder,cblock,node)
       ivar=top_code(coder)
       call code_val(coder,var)
       call make_sys_call(coder,cblock,node,sym_export,1,1)
    else
       jvar=import_cached(var,depth)
       if(pm_fast_isnull(jvar)) then
          do i=depth-1,vcdepth+1,-1
             jvar=import_cached(var,i)
             if(.not.pm_fast_isnull(jvar)) exit
          enddo
          if(pm_fast_isnull(jvar)) jvar=var
          do i=cnode_get_num(jvar,var_loop_depth)+1,depth
             j=i
             if(j<vcdepth+1) j=vcdepth+1
             if(j>coder%loop_depth) then
                iblock=cblock
             else
                iblock=coder%import_cblock(j)
             endif
             call make_sys_var(coder,iblock,node,&
                  make_name2(coder,sym_gt,cnode_get_num(jvar,var_name)),0)
             kvar=top_code(coder)
             call cnode_set_num(kvar,var_loop_depth,i)
             call code_val(coder,jvar)
             call make_basic_call(coder,iblock,node,&
                  sym_import_val,1,1,i,0)
             call cache_import(var,i,kvar)
             jvar=kvar
          enddo
          ivar=jvar
       else
          ivar=jvar
       endif
    endif
  contains
    include 'ftiny.inc'
    include 'fisnull.inc'

    function import_cached(varx,depthx) result(ivarx)
      type(pm_ptr):: varx
      integer::depthx
      type(pm_ptr):: ivarx
      integer(pm_p):: vindex
      vindex=cnode_get_num(varx,var_index)
      ivarx=pm_dict_lookup(coder%context,coder%imports(depthx),&
           pm_fast_tinyint(coder%context,vindex))
    end function import_cached

    subroutine cache_import(varx,depthx,ivarx)
      type(pm_ptr):: varx,ivarx
      integer:: depthx
      integer(pm_p):: vindex
      logical:: ok
      vindex=cnode_get_num(varx,var_index)
      call pm_dict_set(coder%context,coder%imports(depthx),&
           pm_fast_tinyint(coder%context,vindex),ivarx,.true.,.false.,ok)
      if(.not.ok) call pm_panic('Cache import')
    end subroutine cache_import

  end function import_var_to_loop

  ! Check that loop arguments (on stack) are local to loop
  subroutine check_loop_args(coder,node,nargs,nloop)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: node
    integer:: nargs,nloop
    integer:: j
    type(pm_ptr):: var
    do j=nargs-nloop,nargs-1
       var=coder%vstack(coder%vtop-j)
       if(.not.var_local(coder,var)) then
          call code_error(coder,node,'Not a loop-invariant variable: ',&
                     cnode_get(var,var_name))
       elseif(cnode_flags_set(var,var_flags,var_param)) then
          if(cnode_flags_clear(var,var_flags,var_loop_param)) then
             call code_error(coder,node,&
                  'Only a loop parameter may be passed as a loop argument: ',&
                  cnode_get(var,var_name))
          endif
       endif
    enddo
  end subroutine check_loop_args

  ! Extra arguments pass to all loop calls (details of for loop)
  subroutine loop_extra_args(coder,cblock,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer:: i
    if(coder%loop_base>0) then
       call code_val(coder,coder%var(coder%loop_base-3))
       do i=-1,1
          call code_val(coder,coder%var(coder%loop_base+i))
       enddo
    else
       do i=1,loop_call_extra_args
          call make_temp_var(coder,cblock,node)
       enddo
    endif
  end subroutine loop_extra_args

  ! Import an argument list in top nret+nkeys*2+nargs slots of vstack
  function import_arg_list(coder,cblock,node,nargs,nkeys,nret,flags,pflags) result(idepth)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: cblock,node
    integer,intent(in):: nargs,nkeys,nret,flags,pflags
    integer:: idepth
    integer:: i,depth,rdepth
    type(pm_ptr):: p
    if(nret==0.and.iand(flags,call_is_assign_call)==0) then
       depth=coder%loop_depth
       rdepth=depth
    elseif(iand(flags,call_is_loop_call)/=0) then
       depth=coder%loop_depth-1
       rdepth=coder%loop_depth
    elseif(iand(flags,call_is_reduce_call)/=0) then
       depth=coder%loop_depth-1
       rdepth=coder%loop_depth-1
    else
       depth=0
       do i=coder%vtop-nargs-nkeys+1,coder%vtop
          p=coder%vstack(i)
          if(pm_fast_vkind(p)==pm_pointer) then
             if(cnode_get_kind(p)==cnode_is_var) then
                depth=max(depth,cnode_get_num(p,var_loop_depth))
             endif
          endif
       enddo
       rdepth=depth
    endif
    if(depth>0) then
       do i=coder%vtop-nargs+1,coder%vtop
          p=coder%vstack(i)
          if(pm_fast_vkind(p)==pm_pointer) then
             if(cnode_get_kind(p)==cnode_is_var) then
                coder%vstack(i)=import_var_to_loop(coder,&
                     cblock,node,p,depth)
             endif
          endif
       enddo
       do i=coder%vtop-nargs-nkeys+1,coder%vtop-nargs+1,2
          p=coder%vstack(i)
          if(pm_fast_vkind(p)==pm_pointer) then
             if(cnode_get_kind(p)==cnode_is_var) then
                coder%vstack(i)=import_var_to_loop(coder,&
                     cblock,node,p,depth)
             endif
          endif
       enddo
    endif
    do i=coder%vtop-nargs-nkeys-nret+1,coder%vtop-nargs-nkeys
       call cnode_set_num(coder%vstack(i),var_loop_depth,rdepth)
    enddo
    idepth=depth
  contains
    include 'fisnull.inc'
    include 'fvkind.inc'
  end function  import_arg_list

  ! Traverse expression list
  recursive subroutine trav_exprlist(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: i
    do i=1,node_numargs(node)
       call trav_expr(coder,cblock,node,node_arg(node,i))
    enddo
  end subroutine trav_exprlist

  ! Traverse expression 
  recursive subroutine trav_expr(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: sym,i,n,save_sub_index,nsym,base
    logical:: local
    type(pm_ptr):: list,p,q,save_sub_array

    if(pm_fast_isname(node)) then
       if(node%offset<num_sym.and.node%offset/=sym_arg) then
          sym=node%offset
          select case(sym)
          case(sym_true)
             call make_const(coder,cblock,pnode,coder%true)
          case(sym_false)
             call make_const(coder,cblock,pnode,coder%false)
          case(sym_null) 
             call make_const(coder,cblock,pnode,pm_null_obj)
          case(sym_argc)
             call make_sp_call(coder,cblock,pnode,sym_argc,0,1)
          case(sym_hi,sym_lo,sym_step)
             if(coder%sub_index==0) then
                call code_error(coder,pnode,&
                  'Use of "high","low" or "step" outside of subscript expression:'&
                     ,node)
             elseif(coder%sub_index>7) then
                call code_error(coder,pnode,&
                     'Use of "high","low" or "step" after 7th subscript expression:'&
                     ,node)
             else
                call make_temp_var(coder,cblock,pnode)
                call dup_code(coder)

                call make_temp_var(coder,cblock,pnode)
                call dup_code(coder)
                call make_temp_var(coder,cblock,pnode)
                call dup_code(coder)
                call dup_expr(coder,coder%sub_array)
                call make_sys_call(coder,cblock,pnode,sym_dom,1,1)
                call make_sys_call(coder,cblock,pnode,sym_dim1+coder%sub_index-1,1,1)
                call make_sys_call(coder,cblock,pnode,sym,1,1)
             endif
          case default
             write(*,*) sym_names(sym)
             call pm_panic('trav-expr strange sym')
          end select
          return
       end if
       p=find_var(coder,node)
       if(pm_fast_isnull(p)) then
          p=find_param(coder,cblock,pnode,node)
          if(pm_fast_isnull(p)) then
             p=find_decl(coder,pnode,node,modl_proc)
             if(pm_fast_isnull(p)) then
                call code_error(coder,pnode,&
                     'Name not defined:',node)
                call make_temp_var(coder,cblock,pnode)
             else
                call proc_const(pnode,node)
             endif
          endif
       else
          call code_val(coder,p)
       endif
    else if(pm_fast_vkind(node)==pm_pointer) then
       sym=node_sym(node)
       if(sym==sym_proc) then
          call proc_const(node,node_arg(node,1))
          return
       else if(sym==sym_dollar) then
          call make_const(coder,cblock,node,node_arg(node,1))
          return
       endif
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)  
       select case(sym)
       case(first_unary:last_opr,sym_arg)
          do i=1,node_numargs(node)
             call trav_expr(coder,cblock,&
                  node,node_arg(node,i))
          enddo
          if(sym==sym_uminus) then
             sym=sym_minus
          else if(sym==sym_lt) then
             call swap_code(coder)
             sym=sym_gt
          else if(sym==sym_le) then
             call swap_code(coder)
             sym=sym_ge
          endif
          call make_sys_call(coder,cblock,node,&
               sym,node_numargs(node),1)
       case(sym_at,sym_square_at,sym_brace_at,sym_reduce_at)
          if(sym==sym_at) then
             if(node_numargs(node)>1) then
                nsym=sym_at2
                local=.true.
             else
                nsym=sym_at1
             endif
          elseif(sym==sym_square_at) then
             nsym=sym_at1_sub
          elseif(sym==sym_brace_at) then
             nsym=sym_at1_open
          else
             nsym=sym_at2_reduce
          endif
          call check_at_nesting(coder,cblock,node)
          call loop_extra_args(coder,cblock,node)
          call trav_expr(coder,cblock,node,node_arg(node,1))
          n=0
          if(node_numargs(node)>1) &
               call trav_index_list(coder,cblock,node_arg(node,2),n)
          if(nsym==sym_at1) then
             call make_loop_sys_call(coder,cblock,node,&
                  make_name2(coder,sym_dcolon,nsym),1,1+n,1,call_is_reduce_call)
          else
             call make_loop_sys_call(coder,cblock,node,&
                  make_name2(coder,sym_pct,nsym),1,1+n,1,call_is_loop_call)
          endif
       case(sym_open_square,sym_open_brace)
          call trav_expr(coder,cblock,node,node_arg(node,1))
          list=node_arg(node,2)
          call trav_index_list(coder,cblock,list,n)
          if(n==0) then
             call dump_parse_tree(coder%context,6,node,2)
             call pm_panic('No args')
          endif
          call make_sys_call(coder,cblock,list,sym,&
               1+n,1)
       case(sym_open)
          call trav_call(coder,cblock,pnode,node,1)
       case(sym_dcolon,sym_pct)
          call check_at_nesting(coder,cblock,node)
          call trav_call(coder,cblock,pnode,node,1)
       case(sym_dot)
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call make_const(coder,cblock,node,&
               node_arg(node,2))
          call make_sp_call(coder,cblock,node,sym_dot,2,1)
       case(sym_any)
          call trav_type(coder,node,node_arg(node,1))
          if(node_sym(node_arg(node,1))==sym_opt) then
             p=pm_typ_vect(coder%context,top_word(coder))
             i=pm_tv_arg(p,1)
             if(i>=pm_int.and.i<=pm_packed_logical) then
                call trav_expr(coder,cblock,node,node_arg(node,2))
                call make_temp_var(coder,cblock,node)
                call dup_code(coder)
                call make_const(coder,cblock,node,&
                     pm_fast_tinyint(coder%context,int(i,pm_p)))
                call make_sp_call(coder,cblock,node,sym_default,1,1)
                call make_sys_call(coder,cblock,node,sym_opt_num,2,1)
                return
             endif
          endif
          call make_const(coder,cblock,node,&
               pm_fast_tinyint(coder%context,int(pop_word(coder),pm_p)))
          call trav_expr(coder,cblock,node,node_arg(node,2))
          call make_sp_call(coder,cblock,node,sym_any,2,1)
       case(sym_close_square,sym_close_brace)
          !!!!!!!!!!! TODO
       case(sym_struct,sym_rec)
          call make_const(coder,cblock,node,node_arg(node,2))
          p=node_arg(node,1)
          do i=1,node_numargs(p)
             call trav_expr(coder,cblock,p,node_arg(p,i))
          enddo
          call make_sp_call(coder,cblock,p,sym,node_numargs(p)+1,1)
       case default
          write(*,*) sym_names(sym)
          call pm_panic('Code generator - unexpected node')
       end select
    else if(pm_fast_vkind(node)<pm_pointer) then
       call make_const(coder,cblock,pnode,node)
    else
       call pm_panic('Unknown node type building call tree for expression')
    end if
  contains
    include 'fisnull.inc'
    include 'fisname.inc'
    include 'fvkind.inc'
    include 'ftiny.inc'
    include 'fname.inc'

    subroutine proc_const(pnode,pr)
      type(pm_ptr),intent(in):: pnode,pr
      integer:: junk
      call push_word(coder,pm_typ_is_single_proc)
      call push_word(coder,int(pr%offset,pm_i16))
      call make_type(coder,2)
      call make_const(coder,cblock,pnode,pr,pop_word(coder))
      if(pm_fast_isnull(coder%proc_name_vals)) then
         coder%proc_name_vals=pm_set_new(coder%context,8_pm_ln)
      endif
      junk=pm_set_add(coder%context,coder%proc_name_vals,pr)
    end subroutine proc_const

  end subroutine trav_expr

  subroutine check_at_nesting(coder,list_head,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: list_head,node
    type(pm_ptr):: list
    integer:: i
    list=list_head
    i=cnode_get_num(list,cblock_sym)
    do 
       if(i==sym_par_loop) then
          exit
       elseif(i==sym_par_find) then
          call code_error(coder,node,&
               'communicating operation inside parallel "find"')
       endif
       if(cnode_flags_set(list,cblock_flags,cblock_contains_at)) exit
       call cnode_set_flags(list,cblock_flags,cblock_contains_at)
       list=cnode_get(list,cblock_parent)
       if(pm_fast_isnull(list)) then
          call code_error(coder,node,&
                 'communicating operator outside of "for" statement')
          return
       endif
       i=cnode_get_num(list,cblock_sym)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine check_at_nesting

  ! Traverse from.. follow
  subroutine trav_from_follow(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    !!!! TODO
  end subroutine trav_from_follow

  ! Traverse a procedure call
  subroutine trav_call(coder,cblock,pnode,node,nret)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer,intent(in):: nret
    type(pm_ptr):: list,procs,keys,sig,name,amp,prvar,var,v,keynames
    integer:: nloop,flags,i,j,start,cn_sym,nargs,nkeys,base,lcl_base
    integer:: depth, moo,obase
    
    obase=coder%vtop

    ! Determine properties of call
    cn_sym=node_sym(node)
    if(cn_sym==sym_pct) then
       flags=call_is_loop_call
    elseif(cn_sym==sym_dcolon) then
       flags=call_is_reduce_call
    else
       flags=0
    endif
    list=node_arg(node,2)
    if(node_sym(list)==sym_dotdotdot) &
         flags=ior(flags,call_is_vararg)
    nargs=node_numargs(list)
    nloop=node_get_num(node,node_args+4)
    name=node_arg(node,1)

    if(pm_debug_level>3) then
       write(*,*) 'TRAV CALL>',&
            trim(pm_name_as_string(coder%context,name%offset)),&
            nargs,nret,coder%vtop
    endif

    ! Loop-call extra arguments
    if(cn_sym/=sym_open) &
         call loop_extra_args(coder,cblock,node)

    ! Process standard arguments
    base=coder%vtop
    call check_wstack(coder,nargs)
    lcl_base=coder%wtop
    coder%wstack(coder%wtop+1:coder%wtop+nargs)=0
    coder%wtop=coder%wtop+nargs
    amp=node_arg(node,4)
    if(pm_fast_isnull(amp)) then
       do i=1,nargs
          moo=coder%vtop
          call trav_expr(coder,cblock,list,&
               node_arg(list,i))
          if(pm_debug_level>0) then
             if(coder%vtop/=moo+1) then
                write(*,*) 'Argument positions:',moo,coder%vtop
                write(*,*) '------------------'
                call dump_parse_tree(coder%context,6,node_arg(list,i),2)
                call pm_panic('arg mismatch')
             endif
          endif
          if(var_local(coder,top_code(coder))) then
             coder%wstack(lcl_base+i)=1
          endif
       enddo
    else
       amp=pm_name_val(coder%context,amp%offset)
       j=0
       do i=1,nargs
          if(amp%data%i16(amp%offset+j)==i-1) then
             call trav_lhs(coder,cblock,list,&
                  node_arg(list,i))
             if(j<pm_fast_esize(amp)) j=j+1
          else
             call trav_expr(coder,cblock,list,&
                  node_arg(list,i))
          endif
          if(var_local(coder,top_code(coder))) &
               coder%wstack(lcl_base+i)=1
       enddo
    endif

    ! Check any loop args are loop-local
    if(nloop>0) call check_loop_args(coder,node,nargs,nloop)

    ! Find procs with this signature
    prvar=find_var(coder,name)
    amp=node_arg(node,4)
    if(pm_fast_isnull(prvar)) then
       if(cn_sym/=sym_open) then
          procs=find_sig(coder,node,name,&
               amp,nloop,nargs,nret,flags,sig,lcl_base=lcl_base)
       else
          procs=find_sig(coder,node,name,&
               amp,nloop,nargs,nret,flags,sig)
       endif
    else
       procs=find_vcall_sig(coder,node,prvar,amp,&
            nargs,nret,flags,sig)
    endif
    coder%wtop=lcl_base
    
    ! Error return if no such proc
    if(pm_fast_isnull(procs)) then
       coder%vtop=obase-nret
       return
    endif

    ! Allow for extra loop arguments
    if(cn_sym/=sym_open) nargs=nargs+loop_call_extra_args 

    ! Keyword arguments
    list=node_arg(node,3)
    nkeys=trav_keys(coder,cblock,list,sig,cn_sym/=sym_proc)

    ! Swap keyword and standard args
    if(nkeys>0) then
       call check_vstack(coder,nargs)
       do i=base+1,base+nargs
          coder%vtop=coder%vtop+1
          coder%vstack(coder%vtop)=coder%vstack(i)
       enddo
       do i=base+nargs+1,coder%vtop
          coder%vstack(i-nargs)=coder%vstack(i)
       enddo
       coder%vtop=coder%vtop-nargs
    endif

    ! Make the call
    if(pm_debug_level>3) then
       write(*,*) 'TRAV CALL MAKE FULL CALL>',&
            trim(pm_name_as_string(coder%context,name%offset)),nargs,nret,nkeys,coder%vtop
       do i=coder%vtop-nargs-nkeys-nret+1,coder%vtop
          call dump_code_tree(coder,pm_null_obj,6,coder%vstack(i),2)
       enddo
       write(*,*) 'TRAV CALL END MAKE FULL CALL>'
    endif
    depth=import_arg_list(coder,cblock,node,nargs,nkeys,nret,flags,&
         cnode_get_num(sig,2))    
    call make_full_call(coder,cblock,node,procs,nloop,&
         nargs,nret,nkeys,flags,depth)

    ! If this is a variable call, flag the variable
    if(.not.pm_fast_isnull(prvar)) then
       call cnode_set(coder,prvar,var_get_call,cnode_get(cblock,cblock_last_call))
       if(cnode_flags_set(prvar,var_flags,var_accessed)) then
          call cnode_set_flags(prvar,var_flags,var_multi_access)
       else
          call cnode_set_flags(prvar,var_flags,var_accessed)
       endif
    endif

    if(pm_debug_level>0) then
       if(coder%vtop/=obase-nret) then
          write(*,*) base,coder%vtop
          call pm_panic('trav call')
       endif
    endif

  contains
    include 'fisnull.inc'
    include 'fname.inc'
    include 'fesize.inc'

  end subroutine trav_call

  ! Process keyword arguments - returns num of arguments used
  ! -- represented as block of arguments between returns and standard args
  ! -- each *expected* keyword is two arguments - true/false for present and
  ! -- value. Order is determined by signature of *called* procedure(s)
  recursive function trav_keys(coder,cblock,list,sig,invar) result(nkeys)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,list,sig
    logical,intent(in):: invar
    integer:: nkeys
    integer:: start,i,j
    type(pm_ptr):: keys,keynames
    keys=cnode_arg(sig,1)
    if(.not.pm_fast_isnull(keys)) then
       ! Create empty block of arguments
       start=coder%vtop
       nkeys=pm_set_size(coder%context,keys)
       do i=1,nkeys
          call code_null(coder)
          call code_null(coder)
       enddo
    else
       nkeys=0
    endif
    if(.not.pm_fast_isnull(list)) then
       if(nkeys==0) then
          call code_error(coder,list,&
               'Unexpected keyword arguments')
       else
          ! Fill in correct entry for each supplied keyword argument
          call make_const(coder,cblock,list,coder%true)
          do i=1,node_numargs(list),2
             call trav_expr(coder,cblock,list,node_arg(list,i+1))
             if(invar) then
                if(var_local(coder,top_code(coder))) then
                   call code_error(coder,list,&
                        'Keyword argument not loop invariant: ',&
                        node_arg(list,i))
                endif
             endif
             j=pm_set_lookup(coder%context,keys,node_arg(list,i))
             if(j>0) then
                coder%vstack(start+j*2)=pop_code(coder)
                coder%vstack(start+j*2-1)=top_code(coder)
             else
                call code_error(coder,list,&
                     'Unexpected keyword argument: ',&
                     node_arg(list,i))
                call drop_code(coder)
             endif
          enddo
          call drop_code(coder)
          ! Process keys... in call
          if(node_sym(list)==sym_dotdotdot) then
             if(.not.pm_fast_isnull(coder%proc_keys)) then
                call pm_set_merge(coder%context,coder%proc_keys,keys)
                keynames=pm_set_keys(coder%context,keys)
                do i=2,nkeys*2,2
                   if(pm_fast_isnull(coder%vstack(start+i))) then
                      j=pm_set_lookup(coder%context,coder%proc_keys,&
                           keynames%data%ptr(keynames%offset+(i-2)/2))
                      if(j>0) then
                         call make_temp_var(coder,cblock,list)
                         coder%vstack(start+i-1)=top_code(coder)
                         call make_temp_var(coder,cblock,list)
                         coder%vstack(start+i)=top_code(coder)
                         call make_int_const(coder,cblock,list,j*2+coder%proc_nret-1)
                         call make_basic_sp_call(coder,cblock,list,sym_key,1,2,coder%loop_depth)
                      endif
                   endif
                enddo
             endif
          endif
       endif
    endif
    if(nkeys>0) then
       ! Fill in unsuplied keyword arguments
       call make_const(coder,cblock,list,coder%false)
       do i=2,nkeys*2,2
          if(pm_fast_isnull(coder%vstack(start+i))) then
             call make_temp_var(coder,cblock,list)
             coder%vstack(start+i)=top_code(coder)
             call make_sp_call(coder,cblock,list,sym_key,0,1)
             coder%vstack(start+i-1)=top_code(coder)
          endif
       enddo
       call drop_code(coder)
    endif
    nkeys=nkeys*2
  contains
    include 'fisnull.inc'
  end function  trav_keys
  
  ! Traverse type expression in parse tree
  recursive subroutine trav_type(coder,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: pnode,node
    integer:: sym,i,n
    integer(pm_i16)::typno
    integer(pm_p):: gen
    type(pm_ptr):: name,val,p
    character(len=100):: str
    if(pm_fast_isnull(node)) then
       call push_word(coder,0_pm_i16)
       return
    endif
    sym=node_sym(node)
    if(pm_debug_level>3) then
       write(*,*) 'Trav type:',sym_names(sym)
       write(*,*) '------------------'
       call dump_parse_tree(coder%context,6,node,2)
       write(*,*) '------------------'
    endif
    select case(sym)
    case(sym_any)
       if(node_numargs(node)>0) then
          call push_word(coder,pm_typ_is_any)
          call push_word(coder,0_pm_i16)
          call trav_type(coder,node,node_arg(node,1))
       else
          call push_word(coder,0_pm_i16)
       endif
    case(sym_opt)
       call push_word(coder,pm_typ_is_opt)
       call push_word(coder,0_pm_i16)
       call trav_type(coder,node,node_arg(node,1))
       call make_type(coder,3)
    case(sym_type)
       n=node_numargs(node)
       name=node_arg(node,n)
       if(pm_debug_level>2) then
          call pm_name_string(coder%context,name%offset,str)
          write(*,*) 'Traverse named type: ',&
               trim(str),n,name%offset,sym_array
       endif
       call push_word(coder,pm_typ_is_user)
       call push_word(coder,int(name%offset,pm_i16))
       if(n>1) then
          ! Type arguments
          do i=1,n-1
             call trav_type(coder,node,node_arg(node,i))
          enddo
       endif
       typno=get_typeno(n+1)
       if(typno==0) then
          call trav_type_def(coder,node,name,n-1)
       else
          coder%wtop=coder%wtop-n-1
          call push_word(coder,typno)
       endif
    case(sym_open_brace)
       name=node_arg(node,1)
       call push_word(coder,pm_typ_is_user)
       call push_word(coder,int(name%offset,pm_i16))
       typno=get_typeno(2)
       if(typno==0) call pm_panic('Intrinsic type not found')
       call push_word(coder,typno)
    case(sym_struct,sym_rec)
       name=node_arg(node,2)
       if(sym==sym_struct) then
          call push_word(coder,pm_typ_is_struct)
       else
          call push_word(coder,pm_typ_is_rec)
       endif
       call push_word(coder,int(name%offset,pm_i16))
       val=node_arg(node,1)
       n=node_numargs(val)
       do i=1,n
          call trav_type(coder,val,node_arg(val,i))
       enddo
       call make_type(coder,n+2)
    case(sym_hash,sym_open_square)
       call push_word(coder,pm_typ_is_array)
       call push_word(coder,0_pm_i16)
       call trav_type(coder,node,node_arg(node,1))
       call trav_type(coder,node,node_arg(node,2))
       call make_type(coder,4)
    case(sym_list,sym_dotdotdot)
       if(sym==sym_list.and.node_numargs(node)==0) then
          call push_word(coder,0_pm_i16)
          return
       endif
       if(sym==sym_dotdotdot) then
          call push_word(coder,pm_typ_is_vtuple)
       else
          call push_word(coder,pm_typ_is_tuple)
       endif
       call push_word(coder,0_pm_i16)
       n=node_numargs(node)
       do i=2,n,2
          val=node_arg(node,i)
          if(node_sym(node)==sym_invar) then
             call trav_type(coder,node,node_arg(val,1))
          else
             call trav_type(coder,node,val)
          endif
       enddo
       call make_type(coder,n/2+2)
    case(sym_eq)
       call push_word(coder,int(pm_matched_type,pm_i16))
    case(sym_result)
       call push_word(coder,pm_typ_is_tuple)
       call push_word(coder,0_pm_i16)
       n=node_numargs(node)
       do i=1,n
          val=node_arg(node,i)
          if(node_sym(node)==sym_at.or.node_sym(node)==sym_dcolon) then
             call trav_type(coder,node,node_arg(val,1))
          else
             call trav_type(coder,node,val)
          endif
       enddo
       call make_type(coder,n+2)
    case default
       if(sym>=0.and.sym<=num_sym) then
          write(*,*) 'SYM=',sym_names(sym)
       else
          write(*,*) 'SYMno=',sym
       endif
       call dump_parse_tree(coder%context,6,node,2)
       call pm_panic('Type parse node not ok')
    end select
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'fistiny.inc'
    include 'fname.inc'
    
    ! Look up type name and return number
    function get_typeno(size) result(tno)
      integer,intent(in):: size
      integer(pm_i16):: tno
      tno=pm_ivect_lookup(coder%context,coder%context%tcache, &
           coder%wstack(coder%wtop-size+1:),&
           size)
    end function get_typeno

  end subroutine trav_type

  ! Traverse a type definition (specific to given type args)
  recursive subroutine trav_type_def(coder,node,name,nargs)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,name
    integer,intent(in):: nargs
    type(pm_ptr):: decl,dec,inc,pargs,also_node,incl_node,tname,tval
    type(pm_ptr),target:: incset
    logical:: is_present,also_present,type_present
    integer:: sym,i,j,base,ibase
    integer(pm_i16):: newtyp,argtyp,basetyp,partyp,tno
    type(pm_reg),pointer:: reg
    logical:: ok
 
    if(pm_debug_level>2) then
       write(*,*) 'Traverse type def: ',&
            trim(pm_name_as_string(coder%context,name%offset))
    endif

    base=coder%wtop
    if(nargs==0) then
       decl=find_type_var(name)
       if(pm_fast_isnull(decl)) then
          decl=find_decl(coder,node,name,modl_type)
          if(pm_fast_isnull(decl)) then
             call code_error(coder,node,&
                  'Cannot find type: '//trim(type_name(name)))
             coder%wtop=coder%wtop-nargs-1
             coder%wstack(coder%wtop)=0
             return
          endif
          argtyp=0
       else
          if(pm_debug_level>3) write(*,*) 'def is local:',decl%offset
          coder%wtop=coder%wtop-nargs-1
          coder%wstack(coder%wtop)=decl%offset
          return
       endif
    else
       decl=find_decl(coder,node,name,modl_type)
       if(pm_fast_isnull(decl)) then
          call code_error(coder,node,&
               'Cannot find type: '//trim(type_name(name)))
          coder%wtop=coder%wtop-nargs-1
          coder%wstack(coder%wtop)=0
          return
       endif
       call push_word(coder,pm_typ_is_tuple)
       call push_word(coder,0_pm_i16)
       do i=1,nargs
          call push_word(coder,coder%wstack(base-nargs+i))
       enddo
       call make_type(coder,nargs+2)
       argtyp=pop_word(coder)
    endif

    newtyp=pm_idict_add(coder%context,&
         coder%context%tcache,&
         coder%wstack(coder%wtop-nargs-1:),nargs+2,&
         pm_tset_new(coder%context))

    is_present=.false.
    also_present=.false.
    type_present=.false.
    basetyp=0
    dec=node_arg(decl,2)
    do
       if(node_sym(dec)==sym_includes.or.node_sym(dec)==sym_is) then
          call trav_type(coder,dec,node_get(dec,typ_params))
          basetyp=pop_word(coder)
          incl_node=dec
          i=node_get_num(incl_node,typ_number)
          if(i>1000) then
             call code_error(coder,incl_node,&
                  'Very complex recursive type - probably infinite: '&
                  //trim(type_name(name)))
             coder%wtop=coder%wtop-nargs-1
             coder%wstack(coder%wtop)=0
             return
          endif
          call node_set_num(incl_node,typ_number,i+1)
          exit
       endif
       dec=node_get(dec,typ_link)
       if(pm_fast_isnull(dec)) then
          call code_error(coder,decl,&
               'Type not defined with "is" or "includes"'//trim(type_name(name)))
          call push_word(coder,0_pm_i16)
          return
       endif
    enddo
    if(nargs>0) argtyp=typ_with_default(basetyp,argtyp)

    incset=pm_tset_new(coder%context)
    call code_val(coder,incset)

    ! Defer test that type arguments match constraints
    call code_val(coder,coder%typ_defer)
    call code_num(coder,int(basetyp,pm_p))
    call code_num(coder,int(argtyp,pm_p))
    call make_code(coder,node,cnode_is_arg_constraint,3)
    coder%typ_defer=pop_code(coder)

    dec=node_arg(decl,2)
    do
       if(pm_debug_level>0) then
          if(dec%data%vkind/=pm_pointer) &
               call pm_panic('Type node not ptr in trav def')
       endif
       sym=node_sym(dec)
       if(pm_debug_level>2) then
          write(*,*) 'CHECK TYPE DEF>',sym_names(sym)
          if(pm_debug_level>3) then
             write(*,*) '**************'
             call dump_parse_tree(coder%context,6,dec,2)
             write(*,*) '*******************'
          endif
       endif
       if(sym==sym_in) then
          call make_type_vars(pm_null_obj,0_pm_i16)
          call trav_type(coder,dec,node_arg(dec,2))
          call pm_tset_add(coder%context,incset,top_word(coder))
          call pop_type_vars(nargs)
          if(.not.also_present) then
             also_present=.true.
             also_node=dec
          endif
          dec=node_arg(dec,1)
       else
          if(sym==sym_is) then
             if(is_present.or.type_present) then
                call code_error(coder,dec,&
                     'Type is defined twice:'//trim(type_name(name)))
             endif
             is_present=.true.
          else if(sym==sym_also) then
             also_present=.true.
             also_node=dec
          else
             if(pm_debug_level>0) then
                if(sym/=sym_includes) then
                   if(sym>=0.and.sym<=num_sym) then
                      write(*,*) 'SYM=',trim(sym_names(sym))
                   else
                      write(*,*) 'SYM=',sym
                   endif
                   call pm_panic('Not a type in trav_type_def')
                endif
             endif
             if(is_present.or.type_present) then
                call code_error(coder,dec,&
                     'Type is defined twice:'//trim(type_name(name)))
             endif
             type_present=.true.
          endif
          
          if(nargs>0) then
             pargs=node_get(dec,typ_params)
             if(sym==sym_also) then
                ! Parameter must conform to base definition
                call trav_type(coder,dec,pargs)
                partyp=typ_with_default(basetyp,pop_word(coder))
                call code_val(coder,coder%typ_defer)
                call code_num(coder,int(basetyp,pm_p))
                call code_num(coder,int(partyp,pm_p))
                call make_code(coder,dec,cnode_is_par_constraint,3)
                coder%typ_defer=pop_code(coder)
                call make_type_vars(pargs,argtyp,partyp)
             else 
                call make_type_vars(pargs,argtyp)
             endif
          endif

          inc=node_get(dec,typ_includes)
          if(.not.pm_fast_isnull(inc)) then
             if(pm_debug_level>3) then
                write(*,*) 'includes>----------'
                call dump_parse_tree(coder%context,6,inc,2)
                write(*,*) 'end includes-------'
             endif
             do i=1,node_numargs(inc)
                call trav_type(coder,dec,node_arg(inc,i))
                call pm_tset_add(coder%context,&
                     incset,top_word(coder))
                do j=1,i-1
                   if(coder%wstack(base+j)==&
                        coder%wstack(coder%wtop)) then
                      call code_error(coder,dec,&
                           'Repeated type inclusion'//trim(type_name(name)))
                   endif
                enddo
             enddo
          endif
          if(nargs>0) call pop_type_vars(nargs)
 20       continue
          dec=node_get(dec,typ_link)
       endif
       coder%wtop=base
       if(pm_fast_isnull(dec)) exit
    enddo

    base=base-nargs-1
    coder%wstack(base)=pm_idict_add(coder%context,&
         coder%context%tcache,&
         coder%wstack(base:),nargs+2,incset)
    if(pm_debug_level>0) then
       if(coder%wstack(base)/=newtyp) then
          write(*,*) coder%wstack(base),newtyp,base,&
               '(',coder%wstack(base:base+nargs+1),')'
          call pm_panic('trav typ def mismatch')
       endif
    endif
    coder%wtop=base
    
    if(is_present.and.also_present) &
         call code_error(coder,also_node,&
         'Cannot add to type defined with "is":'//&
         trim(type_name(name)))
    if(also_present.and..not.type_present) &
         call code_error(coder,also_node,&
         'Type "also includes" without original "includes":'//&
         trim(type_name(name)))
    call drop_code(coder)
    if(pm_debug_level>2) then
       write(*,*) 'definition traversed for ',&
            trim(pm_name_as_string(coder%context,name%offset))
    endif
  contains
    
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'fesize.inc'
    include 'ftiny.inc'
    include 'fvkind.inc'

    ! Push information on arguments to parameterised type
    subroutine make_type_vars(pnames,argtyp,partyp)
      type(pm_ptr),intent(in):: pnames
      integer(pm_i16),intent(in):: argtyp
      integer(pm_i16),intent(in),optional:: partyp
      integer:: k,j,m,base
      integer(pm_p):: vtyp
      type(pm_ptr):: pname,tv,tv2
      logical:: local
      coder%top=coder%top+1
      coder%stack(coder%top)=typevar_start
      coder%link(coder%top)=0
      coder%var(coder%top)=pm_null_obj
      base=coder%top
      if(argtyp/=0) then
         tv=pm_typ_vect(coder%context,argtyp)
         m=pm_tv_numargs(tv)
         if(present(partyp).and.partyp/=0) then
            tv2=pm_typ_vect(coder%context,partyp)
         endif
         do k=1,m
            pname=node_arg(pnames,k*2-1)
            coder%stack(k+coder%top+1)=pname%offset
            vtyp=pm_tv_arg(tv,k)

            ! If this is a further constrained part of the
            ! type then need to compute intersection of
            ! constraint and argument type (computation is deferred)
            if(present(partyp).and.partyp/=0) then
               call push_word(coder,pm_typ_is_intersect)
               call push_word(coder,0_pm_i16)
               call push_word(coder,int(vtyp,pm_i16))
               call push_word(coder,pm_tv_arg(tv2,k))
               call make_type(coder,4)
               vtyp=pop_word(coder)
            endif
            if(find_var_entry(coder,int(pname%offset,pm_i16),base)>0) then
               call code_error(coder,node,'Repitition of type parameter name:',&
                    pname)
            else
               call make_var_tab_entry(coder,pname,&
                    pm_fast_tinyint(coder%context,vtyp))
            endif
         enddo
      else
         m=0
      endif
      coder%top=coder%top+1
      coder%stack(coder%top)=typevar_end
      coder%link(coder%top)=base
      coder%var(coder%top)=pm_null_obj
    end subroutine make_type_vars

    subroutine pop_type_vars(m)
      integer:: m
      integer:: base
      base=coder%link(coder%top)
      if(pm_debug_level>0) then
         if(coder%stack(coder%top)/=typevar_end) &
              call pm_panic('Pop type vars  - no end recors')
         if(base/=coder%top-m-1) &
              call pm_panic('Pop type vars - size mismatch')
      endif
      coder%top=coder%top-1
      call pop_vars_to(coder,base)
      coder%top=coder%top-1
    end subroutine pop_type_vars

    function find_type_var(vname) result(vr)
      type(pm_ptr),intent(in):: vname
      type(pm_ptr):: vr
      integer:: k
      integer(pm_i16):: n
      if(coder%top==0) then
         vr=pm_null_obj
      elseif(coder%stack(coder%top)/=typevar_end) then
         vr=pm_null_obj
      else
         n=vname%offset
         k=find_var_entry(coder,n,int(coder%link(coder%top)))
         if(k/=0) then
            vr=coder%var(k)
         else
            vr=pm_null_obj
         endif
      end if
    end function find_type_var
 
    function typ_with_default(pt,at) result(newat)
      integer(pm_i16),intent(in):: pt,at
      integer(pm_i16):: newat
      integer:: i
      type(pm_ptr):: pv,av
      newat=at
      if(pt==0.or.at==0) return
      pv=pm_typ_vect(coder%context,pt)
      av=pm_typ_vect(coder%context,at)
      if(pm_tv_kind(pv)/=pm_typ_is_tuple.or.&
           pm_tv_kind(av)/=pm_typ_is_tuple) return
      if(pm_tv_numargs(pv)/=pm_tv_numargs(av)) return
      call push_word(coder,pm_typ_is_tuple)
      call push_word(coder,0_pm_i16)
      do i=1,pm_tv_numargs(av)
         if(pm_tv_arg(av,i)==0) then
            call push_word(coder,pm_tv_arg(pv,i))
         else
            call push_word(coder,pm_tv_arg(av,i))
         endif
      enddo
      call make_type(coder,pm_tv_numargs(pv)+2)
      newat=pop_word(coder)
    end function typ_with_default

    function type_name(name) result(str)
      type(pm_ptr),intent(in):: name
      character(len=100):: str
      type(pm_ptr):: val
      integer(pm_p):: n
      val=pm_name_val(coder%context,name%offset)
      if(pm_fast_vkind(val)==pm_int16) then
         n=val%data%i16(val%offset)
      endif
      call pm_name_string(coder%context,n,str)
    end function type_name
    
  end subroutine trav_type_def

  ! Traverse procedure definition
  recursive subroutine trav_proc(coder,callnode,node,keyargs,&
       amplocs,nloop,nargs,nret,lcl_base)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,node,amplocs,keyargs
    integer,intent(in):: nloop,nargs,nret
    integer,intent(in):: lcl_base

    type(pm_ptr):: cblock,cblock2,p,par,amp,rtypes,arg
    integer:: i,j,base,obase,npars,cbase,nkeyargs,&
         flags,sym,loop_pars,reduce_base
    integer(pm_i16):: partyp
    integer(pm_p):: save_index,t
    integer:: save_proc_base,save_sub_index,&
         save_loop_base, save_proc_loop_depth, save_proc_nret
    type(pm_ptr):: save_sub_array,save_loop_cblock, save_proc_keys

    if(pm_debug_level>4) then
       write(*,*) 'TRAV PROC>',&
            trim(pm_name_as_string(coder%context,&
            node_get_num(node,proc_name))),coder%wtop
    endif

    if(pm_fast_isnull(keyargs)) then
       nkeyargs=0 
    else
       nkeyargs=pm_set_size(coder%context,keyargs)
    endif
    
    ! Parameter types
    obase=coder%vtop
    partyp=node_get_num(node,proc_coded_params)
    if(partyp<0) then
       p=node_get(node,proc_params)
       call trav_type(coder,node,p)
       partyp=pop_word(coder)
       call node_set_num(node,proc_coded_params,int(partyp,pm_p))
    endif
    call code_num(coder,int(partyp,pm_p))

    sym=node_sym(node)
    if(sym==sym_endproc) then
       ! Builtin procedure
       p=node_get(node,proc_opcode)
       p=node_get(node,proc_coded_builtin)
       if(.not.pm_fast_isnull(p)) then
          call code_val(coder,p)          
       else
          ! Create proc code object
          cbase=coder%vtop
          call code_val(coder,node_get(node,proc_opcode))
          call code_val(coder,node_get(node,proc_opcode2))
          call code_val(coder,node_get(node,proc_data))
          call code_val(coder,node_get(node,proc_flags))
          if(nret>0) then
             if(pm_fast_isnull(node_get(node,proc_retas))) then
                p=node_get(node,proc_rettypes)
                call trav_type(coder,node,p)
                call code_num(coder,int(pop_word(coder),pm_p))
                call code_null(coder)
                call code_num(coder,0)
             else
                call code_num(coder,0_pm_p)
                call save_proc_state
                call init_proc_state
                cblock=make_cblock(coder,pm_null_obj,node,sym_do)
                npars=0
                call code_params(cblock,.false.)
                p=node_get(node,proc_retas)
                sym=node_sym(p)
                p=node_arg(p,1)
                base=coder%vtop
                call trav_xexpr(coder,cblock,node,p)
                call make_sp_call(coder,cblock,node,&
                     sym_result,coder%vtop-base,0)
                coder%vtop=base
                call close_cblock(coder,cblock)
                call code_num(coder,sym)
                call restore_proc_state
             endif
          else
             call code_num(coder,0_pm_p)
             call code_null(coder)
             call code_num(coder,0_pm_p)
          endif
       endif
       coder%id=coder%id+1
       call code_num(coder,coder%id)
       if(pm_debug_level>0) then
          if(coder%vtop-cbase/=bi_node_size) then
             write(*,*) '===========',coder%vtop,cbase
             do i=cbase+1,coder%vtop
                call dump_code_tree(coder,pm_null_obj,6,coder%vstack(i),2)
                write(*,*) '===='
             enddo
             call pm_panic('making bi')
          endif
       endif
       call make_code(coder,node,cnode_is_builtin,bi_node_size)
    else
       ! User-defined procedure

       ! Check if cached
       p=node_get(node,proc_code_tree)
       if(.not.pm_fast_isnull(p)) then
          call code_val(coder,p)
          return
       endif

       call save_proc_state
       call init_proc_state
       
       ! Set up code block and imports
       cblock=make_cblock(coder,pm_null_obj,node,sym_proc)
       coder%loop_depth=coder%loop_depth+1
       coder%imports(coder%loop_depth)=pm_dict_new(coder%context,32)
       coder%import_cblock(coder%loop_depth)=cblock

       ! Special types of procedure
       npars=0
       select case(sym)
       case(sym_reduce)
          call code_extra_params(cblock)
          call code_params(cblock,.false.)
          call code_keys(cblock)
          cblock2=code_reduce_args(cblock)
          call code_check(cblock2)
          call code_body(cblock2)
          call code_reduce_result1(cblock2)
          call close_cblock(coder,cblock2)
          call make_sp_call(coder,cblock,node,sym_reduce,&
               nret*2,1+nret)
          call code_reduce_result2(cblock)
       case(sym_local,sym_reduce_at)
          call code_extra_params(cblock)
          call code_params(cblock,.true.)
          call code_grid_params(cblock)
          call code_keys(cblock)
          call code_check(cblock)
          call code_body(cblock)
          if(sym==sym_local) then
             call code_extract_results(cblock)
          else
             call code_result(cblock)
          endif
       case(sym_pct,sym_dcolon)
          call code_extra_params(cblock)
          call code_params(cblock,.true.)
          call code_keys(cblock)
          cblock2=code_loop_startup(cblock)
          call code_check(cblock2)
          call code_body(cblock2)
          call code_result(cblock2)
          call close_cblock(coder,cblock2)
          call code_loop_finish(cblock,cblock2)
       case(sym_proc)
          call code_params(cblock,.false.)
          call code_keys(cblock)
          call code_check(cblock)
          call code_body(cblock)
          call code_result(cblock) 
       case default
          write(*,*) 'SYM=',sym
          write(*,*) sym_names(sym)
          call pm_panic('Strange proc sym')
       end select
       call close_cblock(coder,cblock)

       if(pm_debug_level>0) then
          if(coder%vtop/=obase+2) then
             write(*,*) coder%vtop,obase+2
             do i=obase+3,coder%vtop
                write(*,*) '==================='
                call dump_code_tree(coder,pm_null_obj,6,coder%vstack(i),2)
                write(*,*) '==================='
             enddo
             write(*,*) sym_names(sym)
             call pm_panic( 'Proc cnode mismatch' )
          endif
       endif

       ! Create proc code object 
       call code_num(coder,coder%index)      ! Maximum index
       call code_num(coder,0_pm_p)           ! Recursion flag
       coder%id=coder%id+1
       call code_num(coder,coder%id)         ! Procedure identifier
       call code_num(coder,int(npars,pm_p))  ! Number of parameters
       call code_num(coder,0_pm_p)           ! Number of keywords (fixed later)
       call code_num(coder,int(nret,pm_p))   ! Number of returns
       call code_num(coder,0_pm_p)           !!! Flags
       call code_num(coder,node_get_num(node,proc_name)) ! Name
       call make_code(coder,node,cnode_is_proc,pr_node_size)

       call restore_proc_state

       coder%loop_depth=coder%loop_depth-1

    endif
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'fname.inc'
    include 'ftiny.inc'

    subroutine save_proc_state
       save_index=coder%index
       save_proc_base=coder%proc_base
       save_sub_index=coder%sub_index
       save_loop_base=coder%loop_base
       save_sub_array=coder%sub_array
       save_loop_cblock=coder%loop_cblock
       save_proc_keys=coder%proc_keys
       save_proc_loop_depth=coder%proc_loop_depth
       save_proc_nret=coder%proc_nret
    end subroutine save_proc_state

    subroutine init_proc_state
       coder%index=0
       coder%proc_base=coder%top
       coder%sub_index=0
       coder%loop_base=coder%top
       coder%proc_keys=keyargs
       coder%proc_loop_depth=coder%loop_depth
       coder%proc_nret=nret
    end subroutine init_proc_state

   subroutine restore_proc_state
       coder%index=save_index
       coder%proc_base=save_proc_base
       coder%sub_index=save_sub_index
       coder%loop_base=save_loop_base
       coder%sub_array=save_sub_array
       coder%loop_cblock=save_loop_cblock
       coder%proc_keys=save_proc_keys
       coder%proc_loop_depth=save_proc_loop_depth
       coder%proc_nret=save_proc_nret
    end subroutine restore_proc_state

    subroutine code_params(cblock,bump_varying)
      type(pm_ptr),intent(in):: cblock
      logical,intent(in):: bump_varying
      type(pm_ptr):: name
      p=node_get(node,proc_params)
      if(.not.pm_fast_isnull(p)) then
         amp=node_get(node,proc_amplocs)
         if(pm_fast_isnull(amp)) then
            do i=1,node_numargs(p),2
               flags=ior(var_const,var_param)
               if(i<=nloop) flags=ior(flags,var_loop_param)
               name=node_arg(p,i)
               if(name%offset==sym_arg) flags=0
               call make_var(coder,cblock,p,name,flags)
               if(bump_varying) then
                  if(coder%wstack(lcl_base+i/2+1)==1) then
                     call cnode_set_num(top_code(coder),&
                          var_loop_depth,coder%loop_depth+1)
                  endif
               endif
            enddo
         else
            j=0
            amp=pm_name_val(coder%context,amp%offset)
            do i=1,node_numargs(p),2
               if(amp%data%i16(amp%offset+j)==(i-1)/2) then
                  flags=ior(ior(var_amp,var_param),var_ref)
                  if(i<=nloop) flags=ior(flags,var_loop_param)
                  if(j<pm_fast_esize(amp)) j=j+1
               else
                  flags=ior(var_const,var_param)
                  if(i<=nloop) flags=ior(flags,var_loop_param)
               endif
               name=node_arg(p,i)
               if(name%offset==sym_arg) flags=0
               call make_var(coder,cblock,p,node_arg(p,i),flags)
               if(bump_varying) then
                  if(coder%wstack(lcl_base+i/2+1)==1) then
                     call cnode_set_num(top_code(coder),&
                          var_loop_depth,coder%loop_depth+1)
                  endif
               endif
            enddo
         endif
         npars=npars+node_numargs(p)/2
         call make_basic_sp_call(coder,cblock,p,&
              sym_open,npars,0,coder%loop_depth)
      endif
    end subroutine code_params

    subroutine code_keys(cblock)
      type(pm_ptr),intent(in):: cblock
      ! Keyword arguments
      p=node_get(node,proc_keys)
      if(.not.pm_fast_isnull(p)) then
         base=coder%vtop
         do i=1,node_numargs(p),2
            call make_var(coder,cblock,p,node_arg(p,i),&
                 var_const)
            j=pm_set_lookup(coder%context,keyargs,node_arg(p,i))
            if(pm_debug_level>0) then
               if(j<0) call pm_panic('lookup key arg')
            endif
            call make_int_const(coder,cblock,p,j*2+nret-1)
            call trav_expr(coder,cblock,p,node_arg(p,i+1))
            call make_basic_sp_call(coder,cblock,p,sym_key,2,1,coder%loop_depth)
         enddo
         if(pm_debug_level>0) then
            if(base/=coder%vtop) call pm_panic('trav_proc key mismatch')
         endif
      endif
    end subroutine code_keys

    subroutine code_check(cblock)
      type(pm_ptr),intent(in):: cblock
      ! Check expression
      p=node_get(node,proc_check)
      if(.not.pm_fast_isnull(p)) then
         base=coder%vtop
         call trav_xexpr(coder,cblock,node,p)
         call make_sp_call(coder,cblock,node,sym_check,coder%vtop-base,0)
      endif
    end subroutine code_check

    subroutine code_body(cblock)
      type(pm_ptr),intent(in):: cblock
      ! Body of statements
       p=node_get(node,proc_stmts)
       if(.not.pm_fast_isnull(p)) then
          call trav_open_statlist(coder,cblock,node,p)
       endif
    end subroutine code_body

    subroutine code_result(cblock)
      type(pm_ptr),intent(in):: cblock
      ! Result expression
       p=node_get(node,proc_result)
       if(.not.pm_fast_isnull(p)) then
          base=coder%vtop
          call trav_xexpr(coder,cblock,node,p)
          if(sym==sym_dcolon) then
             do i=0,nret-1
                if(var_local(coder,coder%vstack(coder%vtop-i))) then
                   call code_error(coder,node,&
                        'Returns from "::" proc must be loop-invariant')
                endif
             enddo
          endif
          call make_sp_call(coder,cblock,node,sym_result,nret,0)
          if(pm_debug_level>0) then
             if(coder%vtop/=base) then
                write(*,*) '***************',nret
                do i=base+1,coder%vtop
                   call dump_code_tree(coder,pm_null_obj,6,coder%vstack(i),2)
                enddo
                write(*,*) coder%vtop,base
                write(*,*) '%%%%%%%%%%%%'
                call dump_parse_tree(coder%context,6,p,2)
                call pm_panic('rtn mismatch')
             endif
          endif
       endif
    end subroutine code_result

    subroutine code_extra_params(cblock)
      type(pm_ptr),intent(in):: cblock
      loop_pars=coder%top
      call make_sys_var(coder,cblock,node,sym_for,var_param)
      call make_sys_var(coder,cblock,node,sym_using,var_param)
      call make_sys_var(coder,cblock,node,sym_in,var_param)
      call make_sys_var(coder,cblock,node,sym_dollar,var_param)
      npars=npars+4
    end subroutine code_extra_params

    function code_loop_startup(cblock) result(cblock2)
      type(pm_ptr),intent(in):: cblock
      type(pm_ptr):: cblock2
      integer:: iter
      call make_var_tab_entry(coder,&
           pm_fast_name(coder%context,int(sym_this_shape,pm_p)),&
           coder%var(loop_pars+1))
      coder%top=coder%top+1
      coder%stack(coder%top)=0
      call make_var_tab_entry(coder,&
           pm_fast_name(coder%context,int(sym_this_distr,pm_p)),&
           coder%var(loop_pars+2))
      call make_var_tab_entry(coder,&
           pm_fast_name(coder%context,int(sym_this_tile,pm_p)),&
           coder%var(loop_pars+3))
      call make_var_tab_entry(coder,&
           pm_fast_name(coder%context,int(sym_this_tile_size,pm_p)),&
           coder%var(loop_pars+4))
      iter=coder%top-1
      coder%loop_base=iter
      ! Variable sym_hash set to number of elements in micro-thread
      call make_sys_var(coder,cblock,node,sym_hash,0)
      call drop_code(coder)
      coder%loop_cblock=make_cblock(coder,cblock,node,sym_par_loop)
      call drop_code(coder)
      call push_for_level(coder,coder%loop_cblock,coder%var(iter+2))
      ! Variable sym_iter set to iteration indices in this thread
      call make_sys_var(coder,coder%loop_cblock,node,sym_iter,0)
      call code_val(coder,coder%var(iter))
      call code_val(coder,coder%var(iter+2))
      call make_basic_call(coder,coder%loop_cblock,node,sym_generate,2,1,coder%loop_depth-1,0)
      cblock2=make_cblock(coder,cblock,node,sym_do)
    end function  code_loop_startup

    subroutine code_loop_finish(cblock,cblock2)
      type(pm_ptr),intent(in):: cblock,cblock2
      call close_cblock(coder,cblock2)
      call make_sp_call(coder,coder%loop_cblock,node,sym_for,1,0)
      call code_val(coder,coder%var(coder%loop_base+2))
      call code_val(coder,coder%loop_cblock)
      call code_val(coder,coder%var(coder%loop_base+1))
      call make_basic_sp_call(coder,cblock,node,sym_par_loop,2,1,coder%loop_depth-1)
      call cnode_set_num(coder%var(coder%loop_base+2),var_loop_depth,coder%loop_depth)
      call pop_for_level(coder,cblock,node)
    end subroutine code_loop_finish

    subroutine code_grid_params(cblock)
      type(pm_ptr),intent(in):: cblock
      integer:: i,sym,flags
      type(pm_ptr):: var,params
      params=node_get(node,proc_params)
      do i=loop_call_extra_args+1,npars
         var=coder%var(loop_pars+i)
         flags=iand(cnode_get_num(var,var_flags),var_ref)
         flags=ior(flags,var_const)
         flags=ior(flags,var_shadow)
         if(cnode_get_num(var,var_loop_depth)==coder%proc_loop_depth+1) then
            sym=node_sym(node_arg(params,(i-loop_call_extra_args)*2-1))
            if(sym/=sym_invar) then
               call make_var(coder,cblock,node,cnode_get(var,var_name),&
                    flags)
               call code_val(coder,var)
               call code_val(coder,coder%var(loop_pars+3))
               call make_sys_call(coder,cblock,node,sym_dim,2,1)
            endif
         else
            call cnode_set_num(var,var_loop_depth,coder%proc_loop_depth+1)
            call make_var(coder,cblock,node,cnode_get(var,var_name),&
                 flags)
            call code_val(coder,var)
            call code_val(coder,coder%var(loop_pars+3))
            call make_sys_call(coder,cblock,node,sym_make_array,2,1)
         endif
      enddo
    end subroutine code_grid_params

    subroutine code_extract_results(cblock)
      type(pm_ptr),intent(in):: cblock
      integer::i,base
      p=node_get(node,proc_result)
      if(.not.pm_fast_isnull(p)) then
         base=coder%vtop
         call trav_xexpr(coder,cblock,node,p)
         do i=1,nret
            call make_temp_var(coder,cblock,node)
            call dup_code(coder)
            call code_val(coder,coder%vstack(i+base))
            call make_sys_call(coder,cblock,node,sym_extract_elems,1,1)
         enddo
         call make_sp_call(coder,cblock,node,sym_result,nret,0)
         coder%vtop=base
      endif
    end subroutine code_extract_results

    function code_reduce_args(cblock) result(cblock2)
      type(pm_ptr),intent(in):: cblock
      type(pm_ptr):: cblock2
      type(pm_ptr):: reduce,var
      integer:: nreduce
      reduce=node_get(node,proc_reduce)
      nreduce=node_numargs(reduce)
      do i=1,nloop
         var=coder%var(loop_pars+i+loop_call_extra_args)
         call make_var(coder,cblock,node,cnode_get(var,var_name),&
              ior(cnode_get_num(var,var_flags),var_shadow))
      enddo
      do i=1,nreduce
         call make_var(coder,cblock,node,node_arg(reduce,i),0)
      enddo
      cblock2=make_cblock(coder,cblock,node,sym_reduce)
      reduce_base=coder%top
      do i=1,nret
         call code_val(coder,coder%var(loop_pars+i+loop_call_extra_args))
         call define_sys_var(coder,cblock,node,&
              make_name2(coder,sym_reduce,i),coder%loop_depth)
         call code_val(coder,coder%var(coder%top))
      enddo
    end function code_reduce_args

    subroutine code_reduce_result1(cblock)
      type(pm_ptr),intent(in):: cblock
      integer::i,base
      type(pm_ptr):: p
      base=coder%vtop
      p=node_get(node,proc_result)
      call trav_xexpr(coder,cblock,node,p)
      do i=1,nret
         call code_val(coder,coder%vstack(i+base))
         call make_var_assignment(coder,cblock,node,coder%var(reduce_base+i))
      enddo
      call make_sp_call(coder,cblock,node,sym_result,nret,0)
      coder%vtop=base
    end subroutine code_reduce_result1

    subroutine code_reduce_result2(cblock)
      type(pm_ptr),intent(in):: cblock
      integer::i,base
      do i=1,nret
         call code_val(coder,coder%var(reduce_base+i))
      enddo
      call make_sp_call(coder,cblock,node,sym_result,nret,0)
    end subroutine code_reduce_result2

  end subroutine trav_proc
  
  ! Find a parameter
  function find_param(coder,cblock,node,name) result(v)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,name
    type(pm_ptr):: v
    type(pm_ptr):: p
    logical:: ok
    integer(pm_ln):: n
    p=pm_dict_lookup(coder%context,coder%params,name,n)
    if(pm_fast_isnull(p)) then
       p=find_decl(coder,node,name,modl_param)
       if(pm_fast_isnull(p)) then
          v=pm_null_obj
          return
       endif
       call pm_dict_set(coder%context,&
            coder%params,name,p,.true.,.false.,ok,n)
       call make_temp_var(coder,cblock,node)
       v=top_code(coder)
       call make_int_const(coder,cblock,node,int(n))
       call make_sys_call(coder,cblock,node,sym_get_param,1,1)
    endif
  contains
    include 'fisnull.inc'
  end function find_param
  
  ! Code all used parameter declarations
  subroutine code_params(coder,cblock,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer:: i
    type(pm_ptr):: keys,vals
    keys=pm_dict_keys(coder%context,coder%params)
    vals=pm_dict_vals(coder%context,coder%params)
    do i=0,pm_dict_size(coder%context,coder%params)-1
       call trav_expr(coder,cblock,node,vals%data%ptr(vals%offset+i))
       call make_int_const(coder,cblock,node,i)
       call make_sys_call(coder,cblock,node,sym_set_param,2,0)
    enddo
  end subroutine code_params

  ! Find declaration
  function find_decl(coder,node,name,where) result(v)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,name
    integer,intent(in):: where
    type(pm_ptr):: v
    type(pm_ptr):: modl
    modl=node_get_modl(node)
    v=pm_dict_lookup(coder%context,modl%data%ptr(modl%offset+where),name)
    if(pm_fast_isnull(v)) &
         v=pm_dict_lookup(coder%context,&
         modl%data%ptr(modl%offset+where+modl_local),name)
  contains
    include 'fisnull.inc'
  end function find_decl

  ! Find of construct procedure signature
  recursive function find_sig(coder,node,name,amplocs,&
       nloop,nargs,nret,flags,sigvect,noerr,lcl_base) result(sig)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,name,amplocs
    integer,intent(in):: nloop,nargs,nret,flags
    type(pm_ptr),intent(out):: sigvect
    logical,optional,intent(in):: noerr
    integer,optional,intent(in):: lcl_base
    type(pm_ptr):: sig
    type(pm_ptr):: cblock,keys, procdef,proc,amplocs2,rtn,keyargs, cproc
    integer:: nret2,base,i,j,n
    integer(pm_p):: k
    integer:: flags2,nloop2,sig_len,locl_base
    logical:: vkeys
    character(len=100):: str
    if(present(lcl_base)) then
       ! For loop procedure calls code
       ! separate signature for each combination
       ! of argument locality
       locl_base=lcl_base
       sig_len=4+coder%wtop-(lcl_base+nloop)
    else
       locl_base=-1
       sig_len=4
    endif
    call check_wstack(coder,4)
    coder%wstack(coder%wtop+1)=nloop
    coder%wstack(coder%wtop+2)=amplocs%offset
    coder%wstack(coder%wtop+3)=nret
    coder%wstack(coder%wtop+4)=name%offset
    coder%wtop=coder%wtop+4
    if(pm_debug_level>2) then
       call pm_name_string(coder%context,name%offset,str)
       call pm_name_string(coder%context,amplocs%offset,str(len_trim(str)+2:))
       write(*,*) 'SIG:',trim(str),'NLOOP=',nloop,' NARGS=',nargs,'nret=',nret,&
            'AMP',amplocs%offset
    endif
    k=pm_ivect_lookup(coder%context,coder%sig_cache,&
         coder%wstack(coder%wtop-sig_len+1:),sig_len)
    if(k==0) then
       procdef=find_decl(coder,node,name,modl_proc)
       if(pm_fast_isnull(procdef)) then
          call code_error(coder,node,&
               'Cannot find procedure:',name)
          sig=pm_null_obj
          sigvect=pm_null_obj
          coder%wtop=coder%wtop-4
          return
       else
          ! Find keywords for this signature
          keys=pm_null_obj
          vkeys=.false.
          proc=node_arg(procdef,2)
          do
             keyargs=node_get(proc,proc_keys)
             if(.not.pm_fast_isnull(keyargs)) then
                if(node_sym(keyargs)==sym_dotdotdot) vkeys=.true. 
                ! Add keywords to set
                if(pm_fast_isnull(keys)) then
                   keys=pm_set_new(coder%context,&
                        8_pm_ln)
                   call code_val(coder,keys)
                endif
                do i=1,node_numargs(keyargs),2
                   j=pm_set_add(coder%context,keys,&
                        node_arg(keyargs,i))
                enddo
             endif
             proc=node_get(proc,proc_link)
             if(pm_fast_isnull(proc)) exit
          enddo
          if(pm_fast_isnull(keys)) call code_val(coder,pm_null_obj)
          if(vkeys) then
             call code_num(coder,int(proc_has_vkeys,pm_p))
          else
             call code_num(coder,0_pm_p)
          endif
          call make_code(coder,node,cnode_is_arglist,2)
          k=pm_idict_add(coder%context,coder%sig_cache,&
               coder%wstack(coder%wtop-sig_len+1:),sig_len,&
               top_code(coder))
          call drop_code(coder)
          base=coder%vtop
          ! Signature node starts with set of keys and combined flags
          call code_val(coder,keys)
          if(vkeys) then
             call code_num(coder,int(proc_has_vkeys,pm_p))
          else
             call code_num(coder,0_pm_p)
          endif
          proc=node_arg(procdef,2)
          do
             if(pm_debug_level>2) &
                  write(*,*) 'CHECK PROC SIG>',pm_fast_isnull(node_get(proc,proc_link))
             ! Find conforming procs
             if(proc_conforms()) then
                if(pm_debug_level>2) &
                     write(*,*) 'CONFORMING_PROC>',coder%vtop,base,trim(str)
                call trav_proc(coder,node,proc,keys,amplocs,nloop,nargs,nret,locl_base)
                if(pm_debug_level>2) &
                     write(*,*) 'FINISHED TRAVERSING CONFORMING PROC>',coder%vtop,base,trim(str)
                cproc=top_code(coder)
                if(cnode_get_kind(cproc)==cnode_is_builtin) then
                   flags2=cnode_get_num(cproc,bi_flags)
                else
                   flags2=cnode_get_num(cproc,pr_flags)
                endif
                ! Combine flags for this signature
                coder%vstack(base+2)%offset=&
                     ior(coder%vstack(base+2)%offset,&
                     int(flags2,pm_p))
             endif
             proc=node_get(proc,proc_link)
             if(pm_fast_isnull(proc)) exit
          enddo
          if(.not.pm_fast_isnull(keys)) then
             n=pm_set_size(coder%context,keys)
             do i=base+4,coder%vtop,2
                if(cnode_get_kind(coder%vstack(i))==cnode_is_proc) then
                   call cnode_set_num(coder%vstack(i),pr_nkeys,n)
                endif
             enddo
          endif
          if(pm_debug_level>2) then
             write(*,*) 'SIGEND>',trim(str),&
                  'NARGS=',nargs,'nret=',nret,'N=',coder%vtop-base,'WT=',coder%wtop
          endif
          if(coder%vtop>base+2) then
             if(pm_debug_level>2) &
                  write(*,*) 'MAKE-SIG-VECT>',base,coder%vtop,coder%vtop-base
             call make_code(coder,node,cnode_is_arglist,coder%vtop-base)
             sig=top_code(coder)
             k=pm_idict_add(coder%context,coder%sig_cache,&
                  coder%wstack(coder%wtop-sig_len+1:),sig_len,sig)
             call drop_code(coder)
             sigvect=sig
             sig=pm_fast_tinyint(coder%context,k)
          else
             call drop_code(coder)
             call drop_code(coder)
             if(.not.present(noerr)) then
                call code_error(coder,node,&
                     'Cannot find procedure with correct signature:',name)
             endif
             sig=pm_null_obj
          endif
       endif
    else
       sig=pm_fast_tinyint(coder%context,k)
       sigvect=pm_dict_val(coder%context,coder%sig_cache,int(k,pm_ln))
    endif
    coder%wtop=coder%wtop-4
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'ftiny.inc'

    function proc_conforms() result(ok)
      logical:: ok
      type(pm_ptr):: params
      integer:: sym
      amplocs2=node_get(proc,proc_amplocs)
      rtn=node_get(proc,proc_numret)
      nret2=rtn%offset
      nloop2=node_get_num(proc,proc_numloop)
      if(pm_debug_level>3) then
         write(*,*) 'CHECK CONFORM>',&
              amplocs%offset,amplocs2%offset,&
              nret,nret2,nloop,nloop2
      endif
      ok=(amplocs2%offset==amplocs%offset&
           .and.nret2==nret.and.nloop2==nloop)
      if(ok.and.sig_len>4) then
         params=node_get(proc,proc_params)
         do i=nloop+1,nargs
            sym=node_sym(node_arg(params,i*2-1))
            if(sym==sym_invar) then
               if(coder%stack(lcl_base+i)==1) then
                  ok=.false.
                  exit
               endif
            endif
         enddo
      endif
    end function proc_conforms

  end function find_sig

  ! Find or construct signature for variable argument call
  recursive function find_vcall_sig(coder,node,var,amplocs,&
       nargs,nret,flags,sigvect) result(sig)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,var,amplocs
    integer,intent(in):: nargs,nret,flags
    type(pm_ptr),intent(out):: sigvect
    type(pm_ptr):: sig
    integer(pm_i16),dimension(4):: sig_key
    integer(pm_p):: k
    integer:: cflags,flags2
    character(len=100):: str
    sig_key(1)=sym_proc
    sig_key(2)=amplocs%offset
    sig_key(3)=nret
    sig_key(4)=flags
    k=pm_ivect_lookup(coder%context,coder%sig_cache,sig_key,4)
    if(k>0) then
       sig=pm_fast_tinyint(coder%context,k)
       sigvect=pm_dict_val(coder%context,coder%sig_cache,int(k,pm_ln))
    else
       call code_null(coder)
       call code_num(coder,int(ior(proc_has_vkeys,proc_is_var),pm_p))
       call code_val(coder,var)
       call code_num(coder,coder%call_sig)
       call code_val(coder,node)
       call make_code(coder,node,cnode_is_arglist,5)
       sigvect=top_code(coder)
       k=pm_idict_add(coder%context,&
            coder%sig_cache,sig_key,4,sigvect)
       coder%call_sig=k
       sig=pm_fast_tinyint(coder%context,k)
       call drop_code(coder)
    endif
  contains
    include 'ftiny.inc'
  end function find_vcall_sig

  subroutine complete_vcall_sigs(coder)
    type(code_state):: coder
    integer(pm_ln):: k,newk
    type(pm_ptr):: name,names,amplocs,key,keys,sig,vals,val,node,sigvect
    integer:: i,nret,flags,nargs,base
    k=coder%call_sig
    if(k==0) return
    keys=pm_dict_keys(coder%context,coder%sig_cache)
    vals=pm_dict_vals(coder%context,coder%sig_cache)
    if(pm_fast_isnull(coder%proc_name_vals)) then
       val=vals%data%ptr(vals%offset+k-1)
       call cnode_error(coder,val,&
            'Variable procedure call but program does not create any procedure values')
       return
    endif
    do while(k/=0)
       key=keys%data%ptr(keys%offset+k-1)
       val=vals%data%ptr(vals%offset+k-1)
       node=cnode_arg(val,5)
       amplocs=pm_fast_name(coder%context,int(key%data%i16(key%offset+1_pm_p),pm_p))
       nret=key%data%i16(key%offset+2_pm_p)
       flags=key%data%i16(key%offset+3_pm_p)
       names=pm_set_keys(coder%context,coder%proc_name_vals)
       base=coder%vtop
       call code_val(coder,cnode_arg(val,1))
       call code_val(coder,cnode_arg(val,2))
       call code_val(coder,cnode_arg(val,3))
       do i=1,pm_set_size(coder%context,coder%proc_name_vals)
          name=names%data%ptr(names%offset+i-1)
          sig=find_sig(coder,node,name,amplocs,&
               0,nargs,nret,flags,sigvect,noerr=.true.)
          if(.not.pm_fast_isnull(sig)) then
             call code_val(coder,name)
             call code_val(coder,sig)
          endif
       enddo
       newk=cnode_get_num(val,cnode_args+3)
       if(coder%vtop>base+3) then
          call make_code(coder,node,cnode_is_arglist,coder%vtop-base)
          call pm_dict_set_val(coder%context,coder%sig_cache,k,top_code(coder))
          call drop_code(coder)
       else
          call cnode_error(coder,node,&
               'No possible match for proc variable call')
       endif
       coder%vtop=base
       k=newk
    enddo
  contains
    include 'fname.inc'
    include 'fisnull.inc'
  end subroutine complete_vcall_sigs

  ! Complete type definitions - calculate intersections
  ! and check parameter and argument constraints
  subroutine complete_types(coder)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: p,keys,vals,tv
    integer(pm_ln):: i
    integer(pm_p):: k
    integer(pm_i16):: tno,tno1,tno2
    keys=pm_dict_keys(coder%context,coder%context%tcache)
    vals=pm_dict_vals(coder%context,coder%context%tcache)
    do i=0,pm_dict_size(coder%context,coder%context%tcache)-1
       tv=keys%data%ptr(keys%offset+i)
       if(pm_tv_kind(tv)==pm_typ_is_intersect) then
          call pm_ptr_assign(coder%context,vals,i,&
               pm_do_intersect(coder%context,&
               pm_tv_arg(tv,1),pm_tv_arg(tv,2),&
               coder%wstack(coder%wtop+1:),&
               max_code_stack-coder%wtop-1))
       elseif(pm_tv_kind(tv)==pm_typ_is_user) then
          tno=i+1
          if(pm_typ_is_recur(coder%context,tno,tno)) then
             call cnode_error(coder,p,'Type directly includes itself: '//&
                  trim(pm_typ_as_string(coder%context,tno)))
             call pm_ptr_assign(coder%context,vals,i,pm_null_obj)
          endif
       endif
    enddo
    p=coder%typ_defer
    do while(.not.pm_fast_isnull(p))
       k=cnode_get_kind(p)
       if(k==cnode_is_arg_constraint) then
          tno1=cnode_get_num(p,cnode_args+1)
          tno2=cnode_get_num(p,cnode_args+2)
          if(.not.pm_typ_includes(coder%context,tno1,tno2)) then
             call cnode_error(coder,p,'Type argument does not meet constraint: '//&
                  trim(pm_typ_as_string(coder%context,tno1))//' <-> '//&
                  trim(pm_typ_as_string(coder%context,tno2)))
          endif
       else if(k==cnode_is_par_constraint) then
          tno1=cnode_get_num(p,cnode_args+1)
          tno2=cnode_get_num(p,cnode_args+2)
          if(.not.pm_typ_includes(coder%context,tno1,tno2)) then
             call cnode_error(coder,p,&
                  '"also" parameter contraints do not conform to base type constraints: '//&
                  trim(pm_typ_as_string(coder%context,tno1))//' <-> '//&
                  trim(pm_typ_as_string(coder%context,tno2)))
          endif
       endif
       p=cnode_arg(p,1)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine complete_types

  ! Sort all defined signatures
  subroutine sort_sigs(coder)
    type(code_state),intent(inout):: coder
    integer(pm_ln):: i
    type(pm_ptr):: vals,v,n
    if(pm_debug_level>3) write(*,*) 'SORT SIGS',pm_dict_size(coder%context,coder%sig_cache)
    vals=pm_dict_vals(coder%context,coder%sig_cache)
    do i=0,pm_dict_size(coder%context,coder%sig_cache)-1
       v=vals%data%ptr(vals%offset+i)
       if(cnode_flags_clear(v,cnode_args+1,proc_is_var)) then
          if(pm_debug_level>3) then
             n=pm_dict_key(coder%context,coder%sig_cache,i+1)
             write(*,*) 'SORT SIG',i,pm_dict_size(coder%context,coder%sig_cache)-1,&
                  trim(pm_name_as_string(coder%context,int(n%data%i16(n%offset))))
          endif
          call sort_sig(coder,v)
       else if(pm_debug_level>3) then
          n=pm_dict_key(coder%context,coder%sig_cache,i+1)
          write(*,*) 'SORT SIG SKIP',i,pm_dict_size(coder%context,coder%sig_cache)-1,&
                  trim(pm_name_as_string(coder%context,int(n%data%i16(n%offset))))
       endif
    enddo
  end subroutine sort_sigs

  ! Partial order sort for signature
  subroutine sort_sig(coder,sig)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: sig
    integer:: start,end
    integer:: i,j,k,rel
    integer(pm_i16):: typ1,typ2,typ3,inter,union
    type(pm_ptr):: code,pars
    logical:: ok
    start=sig%offset+cnode_args+2
    end=sig%offset+pm_fast_esize(sig)
    if(pm_debug_level>2) write(*,*) 'SORT SIGNATURE>',start,end
    do i=end-3,start,-2
       if(pm_debug_level>3) write(*,*) 'I=',i,i+2,end-1
       typ1=sig%data%ptr(i)%offset
       code=sig%data%ptr(i+1)
       j=i+2
       do while(j<=end-1)
          typ2=sig%data%ptr(j)%offset
          pars=pm_dict_key(coder%context,&
               coder%context%tcache,int(typ2,pm_ln))
          if(pm_debug_level>3) then
             write(*,*) 'COMPARE SIGS>',typ1,typ2
             write(*,*) trim(pm_typ_as_string(coder%context,typ1))
             write(*,*) trim(pm_typ_as_string(coder%context,typ2))
          endif
          if(typ1==typ2) then
             call cnode_error(coder,code,&
                  'Procedures defined with identical signatures')
             call cnode_error(coder,sig%data%ptr(j+1),&
                  'Conflicting definition')
          else if(pm_typ_includes(coder%context,typ2,typ1)) then
             exit
          else
             sig%data%ptr(j-2)=sig%data%ptr(j)
             sig%data%ptr(j-1)=sig%data%ptr(j+1)
             j=j+2
          endif
       enddo
       sig%data%ptr(j-2)%offset=typ1
       sig%data%ptr(j-1)=code
    enddo
    if(pm_debug_level>0) then
       do i=start,end-1,2
          if(sig%data%ptr(i+1)%data%vkind/=pm_pointer)&
               call pm_dump_tree(coder%context,6,sig%data%ptr(i+1),2)
          if(cnode_get_kind(sig%data%ptr(i+1))/=cnode_is_proc&
               .and.cnode_get_kind(sig%data%ptr(i+1))/=cnode_is_builtin) then
             write(*,*) cnode_get_kind(sig%data%ptr(i+1))
             call pm_panic('sort_sig not proc')
          endif
       end do
    end if
  contains
    include 'fesize.inc'
  end subroutine sort_sig

  ! Make type description node
  function new_type(coder,arr) result(k)
    type(code_state):: coder
    integer(pm_i16),dimension(:),intent(in):: arr
    type(pm_ptr):: ptr
    integer:: k
    k=pm_ivect_lookup(coder%context,coder%context%tcache, &
         arr,&
         size(arr))
    if(k==0) k=pm_idict_add(coder%context,coder%context%tcache,&
         arr,&
         size(arr),pm_null_obj)
    ptr=pm_dict_key(coder%context,coder%context%tcache,int(k,pm_ln))
  end function new_type
  
  ! Make type using size elements from type stack - leave on type stack
  subroutine make_type(coder,size)
    type(code_state),intent(inout):: coder
    integer,intent(in):: size
    coder%wtop=coder%wtop-size+1
    coder%wstack(coder%wtop)=&
         new_type(coder,coder%wstack(coder%wtop:coder%wtop+size-1))
  end subroutine make_type

  ! Make an array type, given element and domain types
  function make_array_type(coder,etyp,dtyp) result(atyp)
    type(code_state),intent(inout):: coder
    integer(pm_i16),intent(in):: etyp,dtyp
    integer(pm_i16):: atyp
    if(coder%wtop+4>max_code_stack) &
         call pm_panic('Program too compex')
    coder%wstack(coder%wtop+1)=pm_typ_is_array
    coder%wstack(coder%wtop+2)=0_pm_i16
    coder%wstack(coder%wtop+3)=etyp
    coder%wstack(coder%wtop+4)=dtyp
    atyp=new_type(coder,coder%wstack(coder%wtop+1:coder%wtop+4))
  end function make_array_type
  
  ! Make an optional type, given base type
  function make_any_type(coder,btyp) result(atyp)
    type(code_state),intent(inout):: coder
    integer(pm_i16),intent(in):: btyp
    integer(pm_i16):: atyp
    if(coder%wtop+4>max_code_stack) &
         call pm_panic('Program too compex')
    coder%wstack(coder%wtop+1)=pm_typ_is_any
    coder%wstack(coder%wtop+2)=0_pm_i16
    coder%wstack(coder%wtop+3)=btyp
    atyp=new_type(coder,coder%wstack(coder%wtop+1:coder%wtop+3))
  end function make_any_type

  ! Check available space on the word stack
  subroutine check_wstack(coder,amount)
    type(code_state),intent(in):: coder
    integer:: amount
    if(coder%wtop+amount>max_code_stack) then
       call pm_panic('Program too complex')
    endif
  end subroutine check_wstack

  ! Push type onto type stack
  subroutine push_word(coder,k)
    type(code_state),intent(inout):: coder
    integer(pm_i16),intent(in):: k
    if(coder%wtop>=max_code_stack) &
         call pm_panic('Program too complex')
    coder%wtop=coder%wtop+1
    coder%wstack(coder%wtop)=k
  end subroutine push_word

  ! Pop type from type stack
  function pop_word(coder) result(k)
    type(code_state),intent(inout):: coder
    integer(pm_i16):: k
    k=coder%wstack(coder%wtop)
    coder%wtop=coder%wtop-1
    if(pm_debug_level>0) then
       if(coder%wtop<0) call pm_panic('pop type')
    endif
  end function pop_word

  ! Return top of type stack
  function top_word(coder) result(k)
    type(code_state),intent(inout):: coder
    integer(pm_i16):: k
    k=coder%wstack(coder%wtop)
  end function top_word

  function var_local(coder,var) result(islocal)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: var
    logical:: islocal
    islocal=.false.
    if(pm_fast_vkind(var)==pm_pointer) then
       if(cnode_get_kind(var)==cnode_is_var) then
          islocal=cnode_get_num(var,var_loop_depth)==coder%loop_depth
       endif
    endif
  contains
    include 'fvkind.inc'
  end function var_local

  ! Find a local variable
  function find_var(coder,name) result(v)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: name
    type(pm_ptr):: v
    integer:: i
    integer(pm_i16)::n
    n=name%offset
    i=find_var_entry(coder,n,coder%proc_base)
    if(i/=0) then
       v=coder%var(i)
    else
       v=pm_null_obj
    endif
    return
  end function find_var

  function find_var_and_entry(coder,name,local,i) result(v)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: name
    logical,intent(out):: local
    integer,intent(out):: i
    type(pm_ptr):: v
    integer(pm_i16)::n
    n=name%offset
    i=find_var_entry(coder,n,coder%proc_base)
    if(i/=0) then
       v=coder%var(i)
       local=i>coder%loop_base
    else
       v=pm_null_obj
       local=.true.
    endif
    return
  end function find_var_and_entry


  ! Find variable entry in hash table
  function find_var_entry(coder,n,base) result(index)
    type(code_state),intent(inout):: coder
    integer(pm_i16),intent(in):: n
    integer,intent(in):: base
    integer:: index
    integer:: i
    index=0
    i=coder%hash(var_hash(n))
    do while(i>base) 
       if(coder%stack(i)==n) then
          index=i
          return
       endif
       i=coder%link(i)
    enddo    
  end function find_var_entry

  ! Delete variables from stack frame
  subroutine delete_vars(coder,start,end)
    type(code_state),intent(inout):: coder
    integer,intent(in):: start,end
    integer:: i
    do i=end+1,coder%top
       coder%stack(i-end-1+start)=coder%stack(i)
       coder%var(i-end-1+start)=coder%var(i)
    enddo
    coder%top=coder%top-end-1+start
  end subroutine delete_vars

  ! Hide a block of variables from name searches
  subroutine hide_vars(coder,start,end)
    type(code_state),intent(inout):: coder
    integer,intent(in):: start,end
    integer:: i
    do i=start,end
       coder%stack(i)=-coder%stack(i)
    enddo
  end subroutine hide_vars
  
  ! Undo hide_vars for block of variables
  subroutine reveal_vars(coder,start,end)
    type(code_state),intent(inout):: coder
    integer,intent(in):: start,end
    integer:: i
    do i=start,end
       coder%stack(i)=-coder%stack(i)
    enddo
  end subroutine reveal_vars

  ! Make a temporary variable
  subroutine make_temp_var(coder,cblock,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    type(pm_ptr):: link
    call code_val(coder,cblock)
    call code_null(coder)
    call code_num(coder,var_const)
    call code_null(coder)
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    call code_val(coder,pm_fast_tinyint(coder%context,coder%loop_depth))
    call code_val(coder,pm_fast_tinyint(coder%context,coder%loop_depth))
    call code_val(coder,pm_null_obj)
    call code_val(coder,pm_null_obj)
    call make_code(coder,node,cnode_is_var,var_node_size)
    link=cnode_get(cblock,cblock_last_var)
    if(pm_fast_isnull(link)) then
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_first_var,pm_ln),&
         top_code(coder))
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_last_var,pm_ln),&
         top_code(coder))
    else
       call pm_ptr_assign(coder%context,link,&
            int(var_link,pm_ln),&
            top_code(coder))
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_last_var,pm_ln),&
            top_code(coder))
    endif
  contains
    include 'fisnull.inc'
    include 'ftiny.inc'
  end subroutine make_temp_var

  ! Make a system variable 
  ! (typically using reserved word/symbol as name)
  subroutine make_sys_var(coder,cblock,node,name,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: name,flags
    call make_var(coder,cblock,node,&
         pm_fast_name(coder%context,int(name,pm_p)),ior(flags,var_shadow))
  contains
    include 'fname.inc'  
  end subroutine make_sys_var

  ! Make a local variable code node
  subroutine make_var(coder,cblock,node,name,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,name
    integer(pm_p),intent(in):: flags
    type(pm_ptr):: var,link
    logical:: local

    ! Check for prior definition
    if(iand(flags,var_shadow)==0) then
       var=find_var(coder,name)
       if(.not.pm_fast_isnull(var)) then
          call code_error(coder,node,&
               'Cannot redefine local variable or constant:',name)
          call code_val(coder,var)
          return
       endif
    endif
    
    ! Create variable node
    call code_val(coder,cblock)
    call code_val(coder,name)
    ! Kludge - all named variables multi access for now
    call code_num(coder,int(ior(flags,var_multi_access),pm_p))
    call code_null(coder)
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    call code_val(coder,pm_fast_tinyint(coder%context,coder%loop_depth))
    call code_val(coder,pm_fast_tinyint(coder%context,coder%loop_depth))
    call code_val(coder,pm_null_obj)
    call code_val(coder,pm_null_obj)
    call make_code(coder,node,cnode_is_var,var_node_size)

    ! Add variable to hash table
    call make_var_tab_entry(coder,name,top_code(coder))

    ! Link variable to enclosing code block
    link=cnode_get(cblock,cblock_last_var)
    if(pm_fast_isnull(link)) then
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_first_var,pm_ln),&
         top_code(coder))
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_last_var,pm_ln),&
         top_code(coder))
    else
       call pm_ptr_assign(coder%context,link,int(var_link,pm_ln),&
            top_code(coder))
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_last_var,pm_ln),&
            top_code(coder))
    endif

  contains
    include 'fisnull.inc'
    include 'ftiny.inc'
  end subroutine make_var

  ! Make an entry for a variable in the hash table
  subroutine make_var_tab_entry(coder,name,var)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: name,var
    integer:: i,j

    if(coder%top>=max_code_stack) then
       call pm_panic('Program too complex')
    endif
    coder%top=coder%top+1
    j=coder%top
    coder%stack(j)=name%offset
    coder%var(j)=var
    i=var_hash(coder%stack(j))
    coder%link(j)=coder%hash(i)
    coder%hash(i)=j    
  end subroutine make_var_tab_entry

  ! Pop variables down to newbase
  subroutine pop_vars_to(coder,newbase)
    type(code_state),intent(inout):: coder
    integer,intent(in):: newbase
    integer:: i,k
    integer(pm_i16):: j
    do i=coder%top,newbase+1,-1
       j=coder%stack(i)
       if(j/=0) then
          k=var_hash(abs(j))
          coder%hash(k)=coder%link(i)
       endif
    enddo
    coder%top=newbase
  end subroutine pop_vars_to
    
  function var_hash(n) result(h)
    integer(pm_i16),intent(in):: n
    integer:: h
    h=iand(int(n),code_local_hash-1)+1
  end function var_hash

  ! Make integer constant node
  subroutine make_int_const(coder,cblock,node,val)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer:: val
    type(pm_ptr):: ptr
    ptr=pm_fast_newnc(coder%context,pm_int,1)
    ptr%data%i(ptr%offset)=val
    coder%temp=ptr
    call make_const(coder,cblock,node,ptr)
    coder%temp=pm_null_obj
  contains
    include 'fnewnc.inc'
  end subroutine make_int_const

  ! Make a constant access code node
  subroutine make_const(coder,cblock,node,val,typ)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,val
    integer(pm_i16),intent(in),optional:: typ
    integer(pm_p):: tno
    tno=pm_fast_typeof(val)
    if(tno==pm_string) tno=pm_string_type
    if(present(typ)) tno=typ
    call code_val(coder,val)
    call code_num(coder,tno)
    call make_code(coder,node,cnode_is_const,2)
  contains
    include 'ftypeof.inc'
  end subroutine make_const

  ! Dupicate an expression
  subroutine dup_expr(coder,expr)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: expr
    type(pm_ptr):: e
    e=expr
    if(cnode_get_kind(e)==cnode_is_var) then
         call cnode_set_flags(e,var_flags,ior(var_multi_access,var_accessed))
      endif
    call code_val(coder,e)
  end subroutine dup_expr
  
  ! Replicate top n expressions on stack
  subroutine repl_exprs(coder,start,finish)
    type(code_state):: coder
    integer,intent(in):: start,finish
    integer:: i,n
    type(pm_ptr):: expr
    n=finish-start+1
    if(coder%vtop+n>max_code_stack) &
         call pm_panic('Program too complex')
    do i=1,n
       expr=coder%vstack(start+i-1)
       if(cnode_get_kind(expr)==cnode_is_var) &
            call cnode_set_flags(expr,var_flags,&
            ior(var_multi_access,var_accessed))
       coder%vstack(coder%vtop+i)=expr
    enddo
    coder%vtop=coder%vtop+n
  end subroutine repl_exprs

  ! Make a procedure call code node for some builtin operations
  subroutine make_sp_call(coder,cblock,node,sym,narg,nret)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret
    integer:: depth
    depth=import_arg_list(coder,cblock,node,narg,0,nret,0,0)
    call make_full_call(coder,cblock,node,&
         pm_fast_tinyint(coder%context,-int(sym,pm_p)),0,narg,nret,0,0,&
         depth)
  contains
    include 'ftiny.inc'
  end subroutine make_sp_call

    ! Make a procedure call code node for some builtin operations
  subroutine make_basic_sp_call(coder,cblock,node,sym,narg,nret,depth)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret,depth
    call make_full_call(coder,cblock,node,&
         pm_fast_tinyint(coder%context,-int(sym,pm_p)),0,narg,nret,0,0,&
         depth)
  contains
    include 'ftiny.inc'
  end subroutine make_basic_sp_call

   ! Make a call to an intrinsic procedure
  subroutine make_sys_call(coder,cblock,node,sym,narg,nret,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret
    integer,optional,intent(in):: flags
    integer:: depth,flag
    type(pm_ptr):: procs,svect
    flag=0
    if(present(flags)) flag=flags
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,int(sym,pm_p))&
         ,pm_null_obj,0,narg,nret,0,svect)
    depth=import_arg_list(coder,cblock,node,narg,0,nret,&
         flag,cnode_get_num(svect,2))
    call make_full_call(coder,cblock,node,&
         procs,0,narg,nret,0,flag,depth)
  contains
    include 'fname.inc'
  end subroutine make_sys_call

  ! Make a call to an intrinsic procedure
  subroutine make_basic_call(coder,cblock,node,sym,narg,nret,depth,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret,depth,flags
    type(pm_ptr):: procs,svect
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,int(sym,pm_p))&
         ,pm_null_obj,0,narg,nret,0,svect)
    call make_full_call(coder,cblock,node,&
         procs,0,narg,nret,0,flags,depth)
  contains
    include 'fname.inc'
  end subroutine make_basic_call

  ! Make a call to an intrinsic procedure with & on first argument
  subroutine make_assign_call(coder,cblock,node,sym,narg,nret)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret
    type(pm_ptr):: procs,svect
    integer:: depth
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,int(sym,pm_p))&
         ,coder%one,0,narg,nret,0,svect)
    depth=import_arg_list(coder,cblock,node,narg,0,nret,call_is_assign_call,&
         cnode_get_num(svect,2))
    call make_full_call(coder,cblock,node,&
         procs,0,narg,nret,0,call_is_assign_call,depth)
    if(pm_debug_level>3) then
       write(*,*) 'SYSCALL &>>',&
            trim(pm_name_as_string(coder%context,int(sym,pm_p))),'===',&
            trim(pm_name_as_string(coder%context,sig_name(coder,procs%offset)))
    endif
  contains
    include 'fname.inc'
  end subroutine make_assign_call

  ! Make a loop call to an intrinsic procedure
  subroutine make_loop_sys_call(coder,cblock,node,sym,nloop,narg,nret,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: nloop,sym,narg,nret,flags
    type(pm_ptr):: procs,svect
    integer:: depth,i,lcl_base
    call check_loop_args(coder,node,narg,nloop)
    depth=import_arg_list(coder,cblock,node,narg+loop_call_extra_args,0,nret,flags,0)
    lcl_base=coder%wtop
    do i=coder%vtop-narg+1,coder%vtop
       if(var_local(coder,coder%vstack(i))) then
          call push_word(coder,1_pm_i16)
       else
          call push_word(coder,0_pm_i16)
       endif
    enddo
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,int(sym,pm_p))&
         ,pm_null_obj,nloop,narg,nret,0,svect,lcl_base=lcl_base)
    call make_full_call(coder,cblock,node,&
         procs,1,narg+loop_call_extra_args,nret,0,flags,depth)
  contains
    include 'fname.inc'
  end subroutine make_loop_sys_call

  ! Make a procedure call code node
  subroutine make_full_call(coder,cblock,node,procs,nloop,narg,nret,nkeys,iflag,depth)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,procs
    integer,intent(in):: nloop,narg,nret,nkeys
    integer,intent(in):: iflag,depth
    type(pm_ptr):: p,q,n,args
    integer:: i
    if(pm_debug_level>0) then
       if(cnode_get_kind(cblock)/=cnode_is_cblock) &
            call pm_panic('full call cblock')
    endif
    call make_code(coder,node,cnode_is_arglist,nret+nkeys+narg)
    args=top_code(coder)
    call code_val(coder,cblock)
    call code_val(coder,procs)
    call code_num(coder,int(iflag,pm_p))
    call code_null(coder)
    call code_num(coder,int(nret,pm_p))
    call code_num(coder,int(nkeys,pm_p))
    call code_num(coder,int(nloop,pm_p))
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    call code_num(coder,int(depth-coder%proc_loop_depth,pm_p))
    call make_code(coder,node,cnode_is_call,call_node_size)
    n=top_code(coder)
    do i=nret+1,nret+nkeys+narg
       p=cnode_arg(args,i)
       if(pm_fast_vkind(p)==pm_pointer) then
          if(cnode_get_kind(p)==cnode_is_var) then
             call cnode_set(coder,p,var_get_call,n)
             if(cnode_flags_set(p,var_flags,var_accessed)) then
                call cnode_set_flags(p,var_flags,var_multi_access)
             else
                call cnode_set_flags(p,var_flags,var_accessed)
             endif
          endif
       endif
    enddo
    do i=1,nret
       p=cnode_arg(args,i)
       call cnode_set(coder,p,var_set_call,n)
    enddo
    p=cnode_get(cblock,cblock_last_call)
    if(pm_fast_isnull(p)) then
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_first_call,pm_ln),n)
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_last_call,pm_ln),n)
    else
       if(pm_debug_level>0) then
          call pm_verify_ptr(p,'make-full-call-p')
          call pm_verify_ptr(n,'make-full-call-n')
       endif
       call pm_ptr_assign(coder%context,p,int(call_link,pm_ln),n)
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_last_call,pm_ln),n)
    endif
    n=pop_code(coder)
  contains
    include 'fisnull.inc'
    include 'fvkind.inc'
  end subroutine make_full_call
  
  ! Make a cblock code node
  function make_cblock(coder,parent,node,sym) result(cblock)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: parent,node
    integer,intent(in):: sym
    type(pm_ptr):: cblock
    integer:: i
    ! Create a new cblock object
    call code_val(coder,parent)
    do i=cblock_first_var,cblock_last_call
       call code_null(coder)
    enddo
    call code_num(coder,int(sym,pm_p))
    call code_num(coder,int(coder%top,pm_p))
    call code_num(coder,0_pm_p)
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    if(pm_fast_isnull(node)) call pm_panic('NULL in makecblock')
    call make_code(coder,node,cnode_is_cblock,cblock_node_size)
    cblock=top_code(coder)
    if(pm_fast_isnull(cblock)) call pm_panic('make cblock null')
  contains
    include 'fisnull.inc'
  end function make_cblock

  ! Close name space associated with cblock
  subroutine close_cblock(coder,cblock)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    call pop_vars_to(coder,int(cblock%data%ptr(cblock%offset+cblock_start)%offset))
  end subroutine close_cblock
  
  ! Make a code tree node
  subroutine make_code(coder,node,ckind,nargs)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: ckind,nargs
    type(pm_ptr):: modl
    integer:: i
    if(pm_debug_level>0) then
       if(coder%vtop-nargs<0) call pm_panic('make code')
    endif
    coder%temp=pm_fast_newnc(coder%context,pm_pointer,&
         int(nargs+cnode_args,pm_p))
    coder%temp%data%ptr(coder%temp%offset)=&
           pm_fast_tinyint(coder%context,int(ckind,pm_p))
    if(.not.pm_fast_isnull(node)) then
       modl=node_get_modl(node)
       coder%temp%data%ptr(coder%temp%offset+1)=&
         modl%data%ptr(modl%offset+modl_name)
       coder%temp%data%ptr(coder%temp%offset+2)=&
         node%data%ptr(node%offset+node_lineno)
    else
       coder%temp%data%ptr(coder%temp%offset+1)=pm_null_obj
       coder%temp%data%ptr(coder%temp%offset+2)=pm_null_obj
    endif
    coder%temp%data%ptr(coder%temp%offset+3:coder%temp%offset+2+nargs)=&
         coder%vstack(coder%vtop-nargs+1:coder%vtop)
    if(pm_debug_level>0) then
       do i=coder%temp%offset+3,coder%temp%offset+2+nargs
          !write(*,*) i-coder%temp%offset-3
          call pm_verify_ptr(coder%temp%data%ptr(i),'Arg to new cnode')
       enddo
    endif
    coder%vtop=coder%vtop-nargs+1
    coder%vstack(coder%vtop)=coder%temp
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'ftiny.inc'
  end subroutine make_code

  ! Check room on code stack (stack v)
  subroutine check_vstack(coder,amount)
    type(code_state),intent(in):: coder
    integer:: amount
    if(coder%vtop+amount>max_code_stack) then
       call pm_panic('Program too complex')
    endif
  end subroutine check_vstack

  ! Push a value onto the code stack
  subroutine code_val(coder,val)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: val
    if(coder%vtop>=max_code_stack) &
         call pm_panic("Procedure too complex")
    coder%vtop=coder%vtop+1
    coder%vstack(coder%vtop)=val
  end subroutine code_val

  ! Push a name onto the code stack
  subroutine code_name(coder,name)
    type(code_state),intent(inout):: coder
    integer,intent(in):: name
    type(pm_ptr):: val
    val=pm_fast_name(coder%context,int(name,pm_p))
    call code_val(coder,val)
  contains
    include 'fname.inc'
  end subroutine code_name
  
  ! Create a two component composite name
  function make_name2(coder,name1,name2) result(sym)
    type(code_state),intent(inout):: coder
    integer,intent(in):: name1,name2
    integer:: sym
    coder%temp=pm_fast_newnc(coder%context,pm_int16,&
         2_pm_p)
    coder%temp%data%i16(coder%temp%offset)=name1
    coder%temp%data%i16(coder%temp%offset+1)=name2
    sym=pm_set_lookup(coder%context,coder%context%names,coder%temp)
    if(sym==0) then
       sym=pm_set_add(coder%context,coder%context%names,coder%temp)
    endif
  contains
    include 'fnewnc.inc'
  end function  make_name2

  ! Push tiny number onto code stack
  subroutine code_num(coder,n)
    type(code_state),intent(inout):: coder
    integer(pm_p),intent(in):: n
    type(pm_ptr):: val
    val=pm_fast_tinyint(coder%context,n)
    call code_val(coder,val)
  contains
    include 'ftiny.inc'  
  end subroutine code_num

  ! Push null value onto code stack
  subroutine code_null(coder)
    type(code_state),intent(inout):: coder
    call code_val(coder,pm_null_obj)
  end subroutine code_null
  
  ! Duplicate code on top of code stack
  subroutine dup_code(coder)
    type(code_state),intent(inout):: coder
    call code_val(coder,top_code(coder))
  end subroutine dup_code

  ! Swap top 2 items on code stack
  subroutine swap_code(coder)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: temp
    temp=coder%vstack(coder%vtop)
    coder%vstack(coder%vtop)=coder%vstack(coder%vtop-1)
    coder%vstack(coder%vtop-1)=temp
  end subroutine swap_code

  ! Pop value from the code stack
  function pop_code(coder) result(val)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: val
    if(pm_debug_level>0) then
       if(coder%vtop<1) &
            call pm_panic('pop code stack')
    endif
    val=coder%vstack(coder%vtop)
    coder%vtop=coder%vtop-1
  end function pop_code

  ! Drop value from the code stack
  subroutine drop_code(coder) 
    type(code_state),intent(inout):: coder
    if(pm_debug_level>0) then
       if(coder%vtop<1) &
            call pm_panic('drop code stack')
    endif
    coder%vtop=coder%vtop-1
  end subroutine drop_code
  
  ! Pop value from the code stack
  function top_code(coder) result(val)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: val
    val=coder%vstack(coder%vtop)
  end function top_code
  
  ! Check code node (debugging)
  subroutine check_cnode(ptr,n)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer(pm_p):: m
    if(.not.pm_fast_vkind(ptr)==pm_pointer) &
         call pm_panic('cnode not ptr')
    m=ptr%data%ptr(ptr%offset)%offset
    if(m<1.or.m>cnode_num_kinds) &
         call pm_panic('cnode bad kind')
    if(n<0.or.n>pm_fast_esize(ptr)) &
         call pm_panic('bad cnode offset')
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end subroutine check_cnode

  ! Get argument n from code node
  function cnode_arg(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr):: val
    if(pm_debug_level>2) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+cnode_args+n-1)
  end function cnode_arg

  ! Get argument n from code node
  function cnode_get(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr):: val
    if(pm_debug_level>2) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)
  end function cnode_get

  ! Set argument n from code node
  subroutine cnode_set(coder,ptr,n,val)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr),intent(in):: val
    if(pm_debug_level>2) call check_cnode(ptr,n)
    call pm_ptr_assign(coder%context,ptr,int(n,pm_ln),val)
  end subroutine  cnode_set

  ! Get argument n from code node
  function cnode_get_num(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer(pm_p):: val
    if(pm_debug_level>2) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
  end function cnode_get_num

  ! Set argument n in code node
  subroutine cnode_set_num(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer(pm_p),intent(in):: val
    if(pm_debug_level>2) call check_cnode(ptr,n)
    ptr%data%ptr(ptr%offset+n)%offset=val
  end subroutine  cnode_set_num

  ! Increment argument n from code node
  subroutine cnode_incr_num(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer(pm_p),intent(in):: val
    if(pm_debug_level>2) call check_cnode(ptr,n)
    ptr%data%ptr(ptr%offset+n)%offset=&
         ptr%data%ptr(ptr%offset+n)%offset+val
  end subroutine  cnode_incr_num

  ! Increment argument n from code node
  subroutine cnode_set_flags(ptr,n,val)
    type(pm_ptr),intent(inout):: ptr
    integer,intent(in):: n
    integer(pm_p),intent(in):: val
    type(pm_ptr):: p
    if(pm_debug_level>2) then
       call check_cnode(ptr,n)
       p=ptr%data%ptr(ptr%offset+n)
       if(pm_fast_vkind(p)/=pm_tiny_int.and.pm_fast_vkind(p)/=pm_null) then
          write(*,*) 'vkind=',pm_fast_vkind(ptr)
          call pm_panic('Set flags')
       endif
       if(cnode_get_kind(ptr)==cnode_is_var.and.&
            n/=var_flags.or.&
            cnode_get_kind(ptr)==cnode_is_cblock.and.n/=cblock_flags.or.&
            cnode_get_kind(ptr)==cnode_is_call.and.n/=call_flags) call pm_panic('set flags')
    endif
    ptr%data%ptr(ptr%offset+n)%offset=ior(&
         ptr%data%ptr(ptr%offset+n)%offset,val)
  contains
    include 'fvkind.inc'
  end subroutine  cnode_set_flags

  ! Check all given flags clear
  function cnode_flags_clear(ptr,n,flags) result(ok)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n,flags
    logical:: ok
    integer(pm_p):: val
    if(pm_debug_level>2) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
    ok=iand(flags,val)==0
  end function cnode_flags_clear

  ! Check all given flags set
  function cnode_flags_set(ptr,n,flags) result(ok)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n,flags
    logical:: ok
    integer(pm_p):: val
    if(pm_debug_level>2) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
    ok=iand(flags,val)==flags
  end function cnode_flags_set

  ! Get kind (cnode_is...) of a code node
  function cnode_get_kind(ptr) result(n)
    type(pm_ptr),intent(in):: ptr
    integer:: n
    if(pm_debug_level>2) call check_cnode(ptr,0)
    n=ptr%data%ptr(ptr%offset)%offset
  end function cnode_get_kind

  ! Get number of arguments to a code node
  function cnode_numargs(ptr) result(n)
    type(pm_ptr),intent(in):: ptr
    integer:: n
    if(pm_debug_level>2) call check_cnode(ptr,0)
    n=pm_fast_esize(ptr)-cnode_args+1
  contains
    include 'fesize.inc'
  end function cnode_numargs

  ! Dump a code tree (debugging)
  recursive subroutine dump_code_tree(coder,rvec,iunit,node,depth)
    type(code_state):: coder
    type(pm_ptr),intent(in):: rvec
    integer,intent(in):: iunit,depth
    type(pm_ptr),intent(in):: node
    character(len=100),parameter:: spaces=' '
    character(len=100):: str
    character(len=100):: varkind
    type(pm_ptr):: p
    integer:: i,n
    if(pm_fast_isnull(node)) then
       write(iunit,*) spaces(1:depth*2),'NULL'
       return
    else if(node%data%vkind/=pm_pointer) then
       write(iunit,*) spaces(1:depth*2),'Non-ptr',node%data%vkind
       call pm_dump_tree(coder%context,iunit,node,depth+1)
       return
    elseif(cnode_get_kind(node)<1.or.cnode_get_kind(node)>cnode_num_kinds) then 
       write(iunit,*) spaces(1:depth*2),'Bad kind'
       return
    endif
    if(depth>30) then
       write(iunit,*) spaces(1:depth*2),'>>>'
       return
    endif
    select case(cnode_get_kind(node))
    case(cnode_is_cblock)
       write(iunit,*) spaces(1:depth*2),'Call List: ',&
            sym_names(cnode_get_num(node,cblock_sym)),'('
       p=cnode_get(node,cblock_first_call)
       do while(.not.pm_fast_isnull(p))
          call dump_code_tree(coder,rvec,iunit,p,depth+1)
          p=cnode_get(p,call_link)
       enddo
       p=cnode_get(node,cblock_first_var)
       if(.not.pm_fast_isnull(p)) then
          write(iunit,*) spaces(1:depth*2),') vars ('
          do while(.not.pm_fast_isnull(p))
             if(pm_fast_vkind(p)/=pm_pointer) then
                write(iunit,*) '???'
                exit
             endif
             call dump_code_tree(coder,rvec,iunit,p,depth+1)
             p=cnode_get(p,var_link)
          enddo
       endif
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_var)
       p=cnode_get(node,var_name)
       if(pm_fast_isnull(p)) then
          str='/Temp/'
       else
          call pm_name_string(coder%context,p%offset,str)
       endif
       p=cnode_get(node,var_flags)
       varkind=''
       if(iand(p%offset,int(var_const,pm_p))/=0)&
            varkind=trim(varkind)//' const'
       if(iand(p%offset,int(var_ref,pm_p))/=0)&
            varkind=trim(varkind)//' ref'
       if(iand(p%offset,int(var_param,pm_p))/=0)&
            varkind=trim(varkind)//' par'
       if(iand(p%offset,int(var_amp,pm_p))/=0)&
            varkind=trim(varkind)//' amp'
       write(iunit,*) spaces(1:depth*2),'Variable '&
            //trim(adjustl(varkind))//': '//trim(str),&
            cnode_get_num(node,var_index),pm_fast_isnull(cnode_get(node,var_get_call)),&
            pm_fast_isnull(cnode_get(node,var_get_call))
       if(.not.pm_fast_isnull(rvec)) then
          i= rvec%data%i16(rvec%offset+&
               cnode_get_num(node,var_index))
          if(i<0) then
             write(iunit,*) spaces(1:depth*2),' Unresolved!!'
          else
             write(iunit,*) spaces(1:depth*2),' Resolved:',i
             call dump_type(coder%context,iunit,&
                  int(i,pm_i16),depth+1)
          endif
       endif
    case(cnode_is_const)
       write(iunit,*) spaces(1:depth*2),'Constant ('
       call pm_dump_tree(coder%context,iunit,cnode_arg(node,1),depth+1)
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_call)
       p=cnode_get(node,call_sig)
       if(pm_fast_istiny(p)) then
          if(p%offset<0) then
             write(iunit,*) spaces(1:depth*2),&
                  'Call "',trim(sym_names(-p%offset)),'" ('
          else
             p=pm_dict_key(coder%context,coder%sig_cache,&
                  int(p%offset,pm_ln))
             call pm_name_string(coder%context,&
                  int(p%data%i16(p%offset+pm_fast_esize(p)),pm_p),str)
             write(iunit,*) spaces(1:depth*2),'Call (',trim(str),') ('
             if(.not.pm_fast_isnull(rvec)) then
                i= rvec%data%i16(rvec%offset+&
                     cnode_get_num(node,call_index))
                if(i<0) then
                   write(iunit,*) spaces(1:depth*2),' Unresolved Sig!!'
                else
                   write(iunit,*) spaces(1:depth*2),' Resolved Sig:',i
                endif
             endif
          endif
       else
          write(iunit,*) spaces(1:depth*2),'Call (-- corrupt sig----) ('
       endif
       p=cnode_get(node,call_args)
       n=cnode_get_num(node,call_nret)
       if(n>0) then
          write(iunit,*) spaces(1:depth*2),'Returns:'
          do i=1,n
             call dump_code_tree(coder,rvec,iunit,&
                  cnode_arg(p,i),depth+1)
          enddo
       endif
       write(iunit,*) spaces(1:depth*2),'Args:'
       do i=n+1,cnode_numargs(p)
          call dump_code_tree(coder,rvec,iunit,&
               cnode_arg(p,i),depth+1)
       enddo
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_builtin)
       write(iunit,*) spaces(1:depth*2),'Builtin',&
            cnode_get_num(node,cnode_args),&
            cnode_get_num(node,cnode_args+1),'('
       if(.not.pm_fast_isnull(cnode_get(node,bi_rcode))) then
          call dump_code_tree(coder,rvec,iunit,&
               cnode_get(node,bi_rcode),depth+1)
       endif
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_proc)
       write(iunit,*) spaces(1:depth*2),'Proc',&
            cnode_get_num(node,cnode_args+1),&
            cnode_get_num(node,cnode_args+2),'('
       call dump_code_tree(coder,rvec,iunit,&
            cnode_arg(node,1),depth+1)
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_single_proc)
       write(iunit,*) spaces(1:depth*2),'Resolved Proc('
       call dump_code_tree(coder,cnode_arg(node,2),&
            iunit,cnode_arg(node,1),depth+1)
        write(iunit,*) spaces(1:depth*2),')'
     case(cnode_is_multi_proc)
        write(iunit,*) spaces(1:depth*2),'Multi Proc(',cnode_numargs(node)
        do i=2,cnode_numargs(node),3
           write(iunit,*)  spaces(1:depth*2+1),&
                trim(pm_typ_as_string(coder%context,int(cnode_get_num(node,cnode_args+i),pm_i16))),&
                '---> sig:',cnode_get_num(node,cnode_args+i+1)
        enddo
        write(iunit,*) spaces(1:depth*2),')'
     case(cnode_is_var_proc)
        write(iunit,*) spaces(1:depth*2),'Var Proc(',cnode_numargs(node)
        do i=4,cnode_numargs(node),2
           write(iunit,*)  spaces(1:depth*2+1),&
                trim(pm_name_as_string(coder%context,cnode_get_num(node,cnode_args+i-1))),&
                '---> sig:',cnode_get_num(node,cnode_args+i)
        enddo
        write(iunit,*) spaces(1:depth*2),')'
     case(cnode_is_arglist)
        write(iunit,*) spaces(1:depth*2),'Var Sig List(',cnode_numargs(node)
        if(cnode_flags_set(node,cnode_args+1,proc_is_var)) then
           do i=4,cnode_numargs(node),2
              write(iunit,*) spaces(1:depth*2),&
                   trim(pm_name_as_string(coder%context,cnode_get_num(node,cnode_args+i-1))),'-->',&
                   cnode_get_num(node,cnode_args+i)
           enddo
        else
           write(iunit,*) spaces(1:depth*2),'Sig List(',cnode_numargs(node)
           do i=2,cnode_numargs(node),2
              call dump_type(coder%context,iunit,&
                   int(cnode_get_num(node,cnode_args+i-1),pm_i16),depth+1)
              call dump_code_tree(coder,rvec,iunit,cnode_arg(node,i+1),depth+1)
           enddo
        endif
        write(iunit,*) spaces(1:depth*2),')'
    case default 
       write(iunit,*) spaces(1:depth*2),'<<Unknown Cnode!!!>>'
    end select
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fistiny.inc'
    include 'fvkind.inc'
  end subroutine  dump_code_tree

  ! Dump a code tree (debugging)
  recursive subroutine qdump_code_tree(coder,rvec,iunit,node,depth)
    type(code_state):: coder
    type(pm_ptr),intent(in):: rvec
    integer,intent(in):: iunit,depth
    type(pm_ptr),intent(in):: node
    character(len=100),parameter:: spaces=' '
    character(len=100):: str
    type(pm_ptr):: p
    integer:: i,n
    if(pm_fast_isnull(node)) then
       write(iunit,*) spaces(1:depth*2),'NULL'
       return
    else if(node%data%vkind/=pm_pointer) then
       write(iunit,*) spaces(1:depth*2),'Non-ptr',node%data%vkind
       return
    elseif(cnode_get_kind(node)<1.or.cnode_get_kind(node)>cnode_num_kinds) then 
       write(iunit,*) spaces(1:depth*2),'Bad kind'
       return
    endif
    if(depth>30) then
       write(iunit,*) spaces(1:depth*2),'>>>'
       return
    endif
    select case(cnode_get_kind(node))
    case(cnode_is_cblock)
       write(iunit,*) spaces(1:depth*2),'Begin: ',&
            sym_names(cnode_get_num(node,cblock_sym))
       p=cnode_get(node,cblock_first_call)
       do while(.not.pm_fast_isnull(p))
          call qdump_code_tree(coder,rvec,iunit,p,depth+1)
          p=cnode_get(p,call_link)
       enddo
       write(iunit,*) spaces(1:depth*2),'End: ',&
            sym_names(cnode_get_num(node,cblock_sym))
    case(cnode_is_var)
       p=cnode_get(node,var_name)
       if(pm_fast_isnull(p)) then
          str='/Temp/'
       else
          call pm_name_string(coder%context,p%offset,str)
       endif
       write(iunit,*) spaces(1:depth*2),trim(str),cnode_get_num(node,var_index),&
            cnode_get_num(node,var_loop_depth)
    case(cnode_is_const)
       call pm_dump_tree(coder%context,iunit,cnode_arg(node,1),depth)
    case(cnode_is_call)
       p=cnode_get(node,call_sig)
       if(pm_fast_istiny(p)) then
          if(p%offset<0) then
             write(iunit,*) spaces(1:depth*2),&
                  'Call "',trim(sym_names(-p%offset)),':'
          else
             p=pm_dict_key(coder%context,coder%sig_cache,&
                  int(p%offset,pm_ln))
             call pm_name_string(coder%context,&
                  int(p%data%i16(p%offset+pm_fast_esize(p)),pm_p),str)
             write(iunit,*) spaces(1:depth*2),'Call (',trim(str),') (',&
                  cnode_get_num(node,call_loop_depth)
          endif
       else
          write(iunit,*) spaces(1:depth*2),'Call (-- corrupt sig----) ('
       endif
       p=cnode_get(node,call_args)
       n=cnode_get_num(node,call_nret)
       if(n>0) then
          write(iunit,*) spaces(1:depth*2),'Returns:'
          do i=1,n
             call qdump_code_tree(coder,rvec,iunit,&
                  cnode_arg(p,i),depth+1)
          enddo
       endif
       write(iunit,*) spaces(1:depth*2),'Args:'
       do i=n+1,cnode_numargs(p)
          call qdump_code_tree(coder,rvec,iunit,&
               cnode_arg(p,i),depth+1)
       enddo
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_builtin)
       write(iunit,*) spaces(1:depth*2),'Builtin',&
            cnode_get_num(node,cnode_args),&
            cnode_get_num(node,cnode_args+1),'('
       if(.not.pm_fast_isnull(cnode_get(node,bi_rcode))) then
          call qdump_code_tree(coder,rvec,iunit,&
               cnode_get(node,bi_rcode),depth+1)
       endif
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_proc)
       write(iunit,*) spaces(1:depth*2),'Proc [',&
            cnode_get_num(node,pr_nargs),&
            cnode_get_num(node,pr_nkeys),cnode_get_num(node,pr_nret),'] ('
       call qdump_code_tree(coder,rvec,iunit,&
            cnode_arg(node,1),depth+1)
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_single_proc)
       write(iunit,*) spaces(1:depth*2),'Resolved Proc('
       call dump_code_tree(coder,cnode_arg(node,2),&
            iunit,cnode_arg(node,1),depth+1)
        write(iunit,*) spaces(1:depth*2),')'
     case(cnode_is_multi_proc)
        write(iunit,*) spaces(1:depth*2),'Multi Proc(',cnode_numargs(node)
        do i=2,cnode_numargs(node),3
           write(iunit,*)  spaces(1:depth*2+1),&
                trim(pm_typ_as_string(coder%context,int(cnode_get_num(node,cnode_args+i),pm_i16))),&
                '---> sig:',cnode_get_num(node,cnode_args+i+1)
        enddo
        write(iunit,*) spaces(1:depth*2),')'
     case(cnode_is_var_proc)
        write(iunit,*) spaces(1:depth*2),'Var Proc(',cnode_numargs(node)
        do i=4,cnode_numargs(node),2
           write(iunit,*)  spaces(1:depth*2+1),&
                trim(pm_name_as_string(coder%context,cnode_get_num(node,cnode_args+i-1))),&
                '---> sig:',cnode_get_num(node,cnode_args+i)
        enddo
        write(iunit,*) spaces(1:depth*2),')'
     case(cnode_is_arglist)
        write(iunit,*) spaces(1:depth*2),'Var Sig List(',cnode_numargs(node)
        if(cnode_flags_set(node,cnode_args+1,proc_is_var)) then
           do i=4,cnode_numargs(node),2
              write(iunit,*) spaces(1:depth*2),&
                   trim(pm_name_as_string(coder%context,cnode_get_num(node,cnode_args+i-1))),'-->',&
                   cnode_get_num(node,cnode_args+i)
           enddo
        else
           write(iunit,*) spaces(1:depth*2),'Sig List(',cnode_numargs(node)
           do i=2,cnode_numargs(node),2
              call dump_type(coder%context,iunit,&
                   int(cnode_get_num(node,cnode_args+i-1),pm_i16),depth+1)
              call dump_code_tree(coder,rvec,iunit,cnode_arg(node,i+1),depth+1)
           enddo
        endif
        write(iunit,*) spaces(1:depth*2),')'
    case default 
       write(iunit,*) spaces(1:depth*2),'<<Unknown Cnode!!!>>'
    end select
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fistiny.inc'
    include 'fvkind.inc'
  end subroutine  qdump_code_tree

  function sig_name(coder,m) result(name)
    type(code_state),intent(in):: coder
    integer,intent(in):: m
    integer(pm_p):: name
    type(pm_ptr):: key
    key=pm_dict_key(coder%context,coder%sig_cache,int(m,pm_ln))
    name=key%data%i16(key%offset+pm_fast_esize(key))
  contains
    include 'fesize.inc'
  end function sig_name

  function sig_name_str(coder,m) result(str)
    type(code_state),intent(in):: coder
    integer,intent(in):: m
    character(len=100):: str
    call pm_name_string(coder%context,sig_name(coder,m),str)
  end function sig_name_str

  subroutine dump_sigs(coder,iunit)
    type(code_state),intent(in):: coder
    integer,intent(in):: iunit
    type(pm_ptr):: keys,vals,sig,code,typ
    integer:: i,j
    character(len=100):: str,str2
    keys=pm_dict_keys(coder%context,coder%sig_cache)
    vals=pm_dict_vals(coder%context,coder%sig_cache)
    do i=0,pm_dict_size(coder%context,coder%sig_cache)-1
       sig=keys%data%ptr(keys%offset+i)
       code=vals%data%ptr(vals%offset+i)
       !write(iunit,*) 'Node:',code%data%hash,code%offset
       call pm_name_string(coder%context,&
            int(sig%data%i16(sig%offset+pm_fast_esize(sig)),pm_p),str)
       call pm_name_string(coder%context,&
            int(sig%data%i16(sig%offset+pm_fast_esize(sig)-2),pm_p),str2)
       write(iunit,*) 'Sig(',trim(str),&
            'Amplocs=',trim(str2),&
            'nret=',sig%data%i16(sig%offset+pm_fast_esize(sig)-1),&
            'nloop=',sig%data%i16(sig%offset+pm_fast_esize(sig)-3),') ('
       if(cnode_flags_set(code,cnode_args+1,proc_is_var)) then
          do j=4,cnode_numargs(code),2
             write(iunit,*) trim(pm_name_as_string(coder%context,cnode_get_num(code,j+cnode_args-1)))
             write(iunit,*) cnode_get_num(code,j+cnode_args)
          enddo
       else
          do j=3,cnode_numargs(code),2
             typ=cnode_arg(code,j)
             write(iunit,*) 'Type:'
             call dump_type(coder%context,iunit,int(typ%offset,pm_i16),2)
             write(iunit,*) 'Code:',j,cnode_numargs(code)
             call qdump_code_tree(coder,pm_null_obj,iunit,cnode_arg(code,j+1),2)
          enddo
       endif
       write(iunit,*) ')'
    enddo
  contains
    include 'fesize.inc'
  end subroutine dump_sigs

  subroutine code_error(coder,node,message,name)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    character(len=*):: message
    type(pm_ptr),intent(in),optional:: name
    character(len=100):: str
    type(pm_ptr):: modname
    call pm_name_string(coder%context,node_get_modl_name(node),str)
    write(*,*) 'Error:',trim(str),' line:',node_get_lineno(node)
    if(present(name)) then
       call pm_name_string(coder%context,name%offset,str)
       str=trim(message)//' '//trim(str)
    else
       str=message
    endif
    write(*,*) str
    coder%num_errors=coder%num_errors+1
    if(coder%num_errors>max_code_errors) &
         stop 'Too many errors - compilation terminated'
  end subroutine code_error

  ! Error related to a given cnode
  subroutine cnode_error(coder,node,message,name)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    character(len=*):: message
    type(pm_ptr),intent(in),optional:: name
    character(len=100):: str
    type(pm_ptr):: modname
    call pm_name_string(coder%context,cnode_get_num(node,cnode_modl_name),str)
    write(*,*) 'Error:',trim(str),' line:',cnode_get_num(node,cnode_lineno)
    if(present(name)) then
       call pm_name_string(coder%context,name%offset,str)
       str=trim(message)//' '//trim(str)
    else
       str=message
    endif
    write(*,*) str
    coder%num_errors=coder%num_errors+1
    if(coder%num_errors>max_code_errors) &
         stop 'Too many errors - compilation terminated'
  end subroutine cnode_error

  subroutine more_error(context,message)
    type(pm_context),pointer:: context
    character(len=*):: message
    write(*,*) trim(message)
  end subroutine more_error

end module pm_codegen

