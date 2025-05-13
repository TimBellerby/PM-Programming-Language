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
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.

!================================================
! The following routines process the parse tree
! into an intermediate form representation
! (defined in pm_cnodes)
!------------------------------------------------
! The new structure is built of cnodes:
! proc    - user defined procedure
! builtin - intrinsic (built-in) procedure
! cblock  - list of calls
! call    - either linked to a signature
!           (list of argument types and procedures)
!           or flagged as a "special" call
!         - argument list refers to var, const, and
!           cblock cnodes
!         - control stuctures = calls with cblock args
! var     - vars and runtime consts
! const   - literal constants
!================================================

module pm_codegen
  use pm_kinds
  use pm_sysdep
  use pm_compbase
  use pm_memory
  use pm_hash
  use pm_options
  use pm_lib
  use pm_symbol
  use pm_types
  use pm_ast
  use pm_cnodes
  implicit none

  logical,parameter:: debug_codegen=.false.
  logical,parameter:: debug_more_codegen=.false.
  
  ! Limits
  integer,parameter:: max_code_stack=4096
  integer,parameter:: code_local_hash=1024
  integer,parameter:: max_trace_depth=256
  integer,parameter:: max_type_nesting=64
  integer,parameter:: max_error_nodes=1024

  ! Parallel context
  integer,parameter:: par_state_none=0
  integer,parameter:: par_state_for=1
  integer,parameter:: par_state_comm_proc=2
  integer,parameter:: par_state_masked=3
  integer,parameter:: par_state_cond=4
  integer,parameter:: par_state_par=5
  integer,parameter:: par_state_sync=6
  
  ! Flags indicating start/end of a block of type variables
  ! as opposed to regular variables on variables stack
  integer,parameter:: typevar_start=-4
  integer,parameter:: typevar_end=-5

  ! Maximum number of coding errors before exit
  integer,parameter:: max_code_errors = 20

  ! State of the code generator
  ! (same structure is used for type inference)
  type code_state

     ! Link to memory management
     type(pm_context),pointer:: context
     type(pm_reg),pointer:: reg,reg2,reg3

     ! Visibility matrix
     type(pm_ptr):: visibility
     
     ! Stack for local variables (stack() for names, var() for info records)
     integer,dimension(max_code_stack):: stack
     type(pm_ptr),dimension(max_code_stack):: var
     integer:: top

     ! Stack of values for creating cnodes
     type(pm_ptr),dimension(max_code_stack):: vstack
     integer:: vtop

     ! Stack of words
     integer,dimension(max_code_stack):: wstack
     integer:: wtop

     ! Code cblock for program
     type(pm_ptr):: prog_cblock
     
     ! Flags for current procedure
     integer:: proc_flags

     ! State variables (as position in coder%var)
     integer:: state_base,mask
     
     ! Caches for call signatures and resolved procedures
     type(pm_ptr):: sig_cache,proc_cache,poly_cache

     ! Lists of deferred type checks
     type(pm_ptr):: defer_check

     ! Procedures as values
     integer:: call_sig
     type(pm_ptr):: proc_name_vals

     ! Misc values
     type(pm_ptr):: temp,temp2,true,false,one,comm_amp
     type(pm_ptr):: std_amp,block_amp,iter_amps,iter_block_amps
     type(pm_ptr):: undef_val

     ! 'true and 'false types
     integer:: true_fix,false_fix,true_literal,false_literal

     ! '1 type
     integer:: unit_type

     ! Check default error message
     integer:: check_mess

     ! Contextual information for this point in the traverse
     type(pm_ptr):: proc
     integer:: proc_base,proc_ncalls
     logical:: fixed
     integer:: par_state

     ! Counter to give each proc a unique index for all procs
     integer:: id
     
     ! Counter to provide unique index for all nodes created
     integer:: index

     ! Counter to provide unique index for all blocks
     integer:: block_id

     ! Lexical scope (offset into vstack)
     integer:: lex_scope

     ! Blocks
     integer:: block_entry,block_base

     ! Flags indicating type inference not complete
     logical:: types_finished,redo_calls,incomplete,first_pass

     ! Taints
     integer:: taints,proc_taints

     ! Type inference base of current proc record
     integer:: base

     ! Type inference flag recursion -- use to locate infinite recursion
     logical:: flag_recursion

     ! Type inference procedure trace
     type(pm_ptr),dimension(max_trace_depth):: trace
     integer,dimension(max_trace_depth)::trace_keys
     integer:: trace_depth
  
     ! Error count
     type(pm_ptr):: error_nodes(max_error_nodes)
     integer:: num_errors
     logical:: supress_errors

  end type code_state

contains

  !********************************************************
  ! SETUP
  !********************************************************
  
  !========================================================
  ! Initialise code generator structure
  !========================================================
  subroutine init_coder(context,coder,visibility)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: visibility
    type(code_state),intent(out):: coder
    coder%context=>context
    coder%visibility=visibility
    coder%top=1
    coder%vtop=0
    coder%wtop=0
    coder%reg=>pm_register(context,'coder-var stack',coder%temp,coder%temp2,&
         coder%sig_cache,coder%proc_cache,coder%true,coder%false,coder%one,&
         coder%undef_val,&
         array=coder%var,array_size=coder%top)
    coder%reg2=>pm_register(context,'coder-node stack',&
         coder%proc_name_vals,coder%poly_cache,coder%comm_amp,&
         coder%std_amp,coder%block_amp,coder%iter_amps,&
         coder%iter_block_amps,array=coder%vstack,&
         array_size=coder%vtop)
    coder%reg3=>pm_register(context,'coder-for stack',coder%defer_check)
    coder%sig_cache=pm_dict_new(context,32_pm_ln)
    coder%prog_cblock=pm_null_obj
    coder%defer_check=pm_null_obj
    coder%proc_base=1
    coder%proc_ncalls=0
    coder%index=0
    coder%lex_scope=0
    coder%true=pm_new_small(context,pm_logical,1_pm_p)
    coder%true%data%l(coder%true%offset)=.true.
    coder%false=pm_new_small(context,pm_logical,1_pm_p)
    coder%false%data%l(coder%false%offset)=.false.

    coder%one=pm_new_small(context,pm_long,1_pm_p)
    coder%one%data%ln(coder%one%offset)=1
    coder%unit_type=pm_new_fix_value_type(coder%context,coder%one)

    coder%one=pm_new_small(context,pm_int,1_pm_p)
    coder%one%data%i(coder%one%offset)=1
    coder%comm_amp=pm_new_small(context,pm_int,1_pm_p)
    coder%comm_amp%data%i(coder%comm_amp%offset)=num_comm_args+1
    coder%std_amp=pm_new_small(context,pm_int,1_pm_p)
    coder%std_amp%data%i(coder%std_amp%offset)=2
    coder%block_amp=pm_new_small(context,pm_int,1_pm_p)
    coder%block_amp%data%i(coder%block_amp%offset)=num_comm_args+2
    coder%iter_amps=pm_new_small(context,pm_int,2_pm_p)
    coder%iter_amps%data%i(coder%iter_amps%offset)=num_comm_args+2
    coder%iter_amps%data%i(coder%iter_amps%offset+1)=num_comm_args+4
    coder%iter_block_amps=pm_new_small(context,pm_int,2_pm_p)
    coder%iter_block_amps%data%i(coder%iter_block_amps%offset)=num_comm_args+1
    coder%iter_block_amps%data%i(coder%iter_block_amps%offset+1)=num_comm_args+3
    coder%one=pm_fast_tinyint(coder%context,&
         pm_intern_val(coder%context,coder%one))
    coder%comm_amp=pm_fast_tinyint(coder%context,&
         pm_intern_val(coder%context,coder%comm_amp))
    coder%std_amp=pm_fast_tinyint(coder%context,&
         pm_intern_val(coder%context,coder%std_amp))
    coder%block_amp=pm_fast_tinyint(coder%context,&
         pm_intern_val(coder%context,coder%block_amp))   
    coder%iter_amps=pm_fast_tinyint(coder%context,&
         pm_intern_val(coder%context,coder%iter_amps))
    coder%iter_block_amps=pm_fast_tinyint(coder%context,&
         pm_intern_val(coder%context,coder%iter_block_amps))
    coder%check_mess=pm_new_literal_value_type(coder%context,&
         pm_new_string(coder%context,'Failed "check" or "test""'))
    coder%proc_name_vals=pm_dict_new(coder%context,8_pm_ln)
    coder%id=0
    coder%block_id=0
    coder%true_fix=pm_new_fix_value_type(coder%context,coder%true)
    coder%false_fix=pm_new_fix_value_type(coder%context,coder%false)
    coder%true_literal=pm_new_literal_value_type(coder%context,coder%true)
    coder%false_literal=pm_new_literal_value_type(coder%context,coder%false)

    coder%num_errors=0
    coder%supress_errors=.false.
    coder%fixed=.false.
    coder%par_state=par_state_none

  contains
    include 'fname.inc'
    include 'ftiny.inc'
  end subroutine init_coder

  !========================================================
  ! Finalise and delete code generator
  ! (this is actually done after inference is completed)
  !========================================================
  subroutine term_coder(coder)
    type(code_state),intent(inout):: coder
    call pm_delete_register(coder%context,coder%reg)
    call pm_delete_register(coder%context,coder%reg2)
    call pm_delete_register(coder%context,coder%reg3)
  end subroutine term_coder

  !========================================================
  ! Start traversing the program
  !========================================================
  subroutine trav_prog(coder,stmt_list)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: stmt_list
    type(pm_ptr):: prog_cblock
    
    prog_cblock=make_cblock(coder,pm_null_obj,stmt_list,sym_do)
    coder%prog_cblock=prog_cblock

    ! Some general constants
    call make_const(coder,prog_cblock,stmt_list,&
         pm_fast_tinyint(coder%context,-9999),int(pm_tiny_int))
    coder%undef_val=pop_code(coder)

    ! State variables (set to null)
    call make_state_vars(coder,prog_cblock,stmt_list)
    
    ! filesystem variable
    call make_sys_var(coder,prog_cblock,stmt_list,sym_filesystem,var_is_var)
    call make_sys_call(coder,prog_cblock,stmt_list,sym_get_filesystem,0,1)
    
    call trav_stmt_list(coder,prog_cblock,stmt_list,stmt_list,sym_do)
    call make_sp_call(coder,prog_cblock,stmt_list,sym_do,1,0)
    call close_cblock(coder,prog_cblock)

    if(coder%num_errors/=0) return
    
    ! Complete type definitions
    call complete_type_checks(coder)

    if(coder%num_errors/=0) return
 
    ! Sort signatures
    call sort_sigs(coder)

  contains
    include 'fnewnc.inc'
    include 'fname.inc'
    include 'ftiny.inc'
  end subroutine trav_prog

  !===============================================
  ! Create state variables and set to null
  !===============================================
  subroutine make_state_vars(coder,cblock,node,topo)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    type(pm_ptr),intent(in),optional:: topo
    coder%state_base=coder%top
    if(present(topo)) then
       call push_var(coder,sym_topology,topo)
    else
       call make_sys_var(coder,cblock,node,sym_topology,0)
    endif
    call make_sys_var(coder,cblock,node,sym_outer,0)
    call make_sys_var(coder,cblock,node,sym_region,0)
    call make_sys_var(coder,cblock,node,sym_subregion,0)
    call make_sys_var(coder,cblock,node,sym_here_in_tile,0)
    call make_sys_var(coder,cblock,node,sym_mask,0)
    coder%mask=coder%top
    call make_sp_call(coder,cblock,node,sym_null,0,&
         num_comm_args-merge(1,0,present(topo)))
  end subroutine make_state_vars

  !*******************************************************
  ! STATEMENTS
  !*******************************************************

  !========================================================
  ! Traverse statement list - push cblock onto stack
  !========================================================
  recursive subroutine trav_stmt_list(coder,parent,&
       listp,list,lsym,open_scope)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: parent,listp,list
    integer,intent(in):: lsym
    logical,intent(in),optional:: open_scope
    type(pm_ptr):: cblock
    cblock=make_cblock(coder,parent,listp,lsym)
    call trav_open_stmt_list(coder,cblock,&
       listp,list)
    call close_cblock(coder,cblock)
    if(present(open_scope)) then
       call cnode_set_flags(top_code(coder),cblock_flags,cblock_is_open)
    endif
  end subroutine trav_stmt_list

  !========================================================
  ! Traverse open list of statements - add to passed cblock
  !========================================================
  recursive subroutine trav_open_stmt_list(coder,cblock,&
       listp,list)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,listp,list
    integer:: i,j,n,sym,base,vbase,wbase,lex_scope,save_par_state
    integer:: xbase,xtop,dtop
    type(pm_ptr):: node,cblock2,var,p
    logical:: iscomm
    if(pm_fast_isnull(list)) goto 10
    do i=1,node_numargs(list)
       vbase=coder%vtop
       wbase=coder%wtop
       node=node_arg(list,i)
       sym=node_sym(node)
       if(debug_codegen) then
          write(*,*) 'TRAVERSE>',sym_names(sym),coder%vtop,vbase
          !call dump_parse_tree(coder%context,6,node,2)
       endif
       select case(sym)
       case(sym_if,sym_if_invar)
          lex_scope=push_lex_scope(coder)
          save_par_state=coder%par_state
          call trav_xexpr(coder,cblock,node,&
               node_arg(node,1))
          if(sym==sym_if_invar) then
              call code_check_invar(coder,cblock,node,top_code(coder),sym_if_invar)
          else
             coder%par_state=merge(merge(par_state_masked,par_state_cond,&
                  pm_fast_isnull(node_arg(node,3))),par_state_none,&
                  coder%par_state/=par_state_none)
          endif
          coder%lex_scope=lex_scope
          call trav_stmt_list(coder,cblock,node,&
               node_arg(node,2),sym_if)
          if(.not.pm_fast_isnull(node_arg(node,3))) then
             call trav_stmt_list(coder,cblock,&
                  node,node_arg(node,3),sym_if)
          else
             call code_null(coder)
          endif
          coder%par_state=save_par_state
          call get_lex_scope(coder,node)
          call make_sp_call(coder,cblock,node,&
               sym_if,4,0)
          call pop_lex_scope(coder)
       case(sym_switch,sym_switch_invar)
          call trav_xexpr(coder,cblock,node,&
               node_arg(node,1))
          save_par_state=coder%par_state
          if(sym==sym_switch_invar) then
             call code_check_invar(coder,cblock,node,top_code(coder),sym_switch_invar)
          else
             coder%par_state=merge(merge(par_state_masked,par_state_cond,&
                  pm_fast_isnull(node_arg(node,node_numargs(node)))),par_state_none,&
                  coder%par_state/=par_state_none)
          endif
          var=top_code(coder)
          call trav_switch_stmt(coder,cblock,node,2,var,&
               merge(sym_if_invar,sym_if,sym==sym_switch_invar))
          call drop_code(coder)
          coder%par_state=save_par_state
       case(sym_while,sym_while_invar)
          lex_scope=push_lex_scope(coder)
          save_par_state=coder%par_state
          coder%par_state=loop_par_state(coder,node,&
               sym,sym==sym_while_invar)
          cblock2=make_cblock(coder,cblock,node,sym_while)
          call trav_xexpr(coder,cblock2,node,node_arg(node,1))
          call close_cblock(coder,cblock2)
          coder%lex_scope=lex_scope
          call trav_stmt_list(coder,cblock,node,&
               node_arg(node,2),sym_while)
          call get_lex_scope(coder,node)
          if(sym/=sym_while) call code_val(coder,coder%var(coder%mask))
          call make_sp_call(coder,cblock,node,sym,merge(4,5,sym==sym_while),0)
          call pop_lex_scope(coder)
          coder%par_state=save_par_state
       case(sym_until,sym_until_invar)
          save_par_state=coder%par_state
          coder%par_state=loop_par_state(coder,node,sym,&
               sym==sym_until_invar)
          lex_scope=push_lex_scope(coder)
          cblock2=make_cblock(coder,cblock,node,sym_until)
          coder%lex_scope=lex_scope
          call trav_open_stmt_list(coder,cblock2,node,&
               node_arg(node,2))
          iscomm=cnode_flags_set(top_code(coder),cblock_flags,cblock_is_comm)
          call trav_xexpr(coder,cblock2,node,node_arg(node,1))
          call close_cblock(coder,cblock2)
          call get_lex_scope(coder,node)
          if(sym/=sym_until) call code_val(coder,coder%var(coder%mask))
          call make_sp_call(coder,cblock,node,&
               sym,merge(4,5,sym==sym_until),0)
          call pop_lex_scope(coder)
          coder%par_state=save_par_state
       case(sym_do_stmt)
          if(node_numargs(node)==1) then
             call trav_stmt_list(coder,cblock,node,node_arg(node,1),sym_do)
             call make_sp_call(coder,cblock,node,sym_do,1,0)
          else
             call trav_subexpr(coder,cblock,node,node_arg(node,6),xbase,xtop,dtop)
             if(xbase>=0) call hide_where_vars(coder,xbase+1,dtop)
             call make_block_proc(coder,cblock,node_arg(node,3),&
                  node_arg(node,1),node_num_arg(node,2),&
                  node_arg(node,5),node_numargs(node_arg(node,5)),&
                  node_arg(node,4))
             if(xbase>=0) call reveal_vars(coder,xbase+1,dtop)
             call trav_call(coder,cblock,node,node_arg(node,3),0,.true.)
             if(xbase>=0) call hide_where_vars(coder,xbase+1,xtop)
          endif
       case(sym_for,sym_forall)
          call trav_for_stmt(coder,cblock,list,node)
       case(sym_each,sym_foreach_invar)
          call trav_foreach_stmt(coder,cblock,list,node)
       case(sym_test)
          if(pm_fast_isnull(node_arg(node,1))) then
             call trav_stmt_list(coder,cblock,node,node_arg(node,2),sym_check)
             call make_sp_call(coder,cblock,node,sym_test,1,0)
          elseif(.not.pm_fast_isnull(node_arg(node,2))) then
             cblock2=make_cblock(coder,cblock,node,sym_check)
             call trav_open_stmt_list(coder,cblock,node,node_arg(node,2))
             call trav_xexpr(coder,cblock2,node,node_arg(node,1))
             call close_cblock(coder,cblock2)
             call make_sp_call(coder,cblock,node,sym_test,1,0)
          else
             call trav_xexpr(coder,cblock,node,node_arg(node,1))
          endif
       case(sym_do)
          p=node_arg(node,1)
          call trav_call(coder,cblock,node,p,0,.true.)
       case(sym_var,sym_const)
          do j=1,node_numargs(node)-1
             call make_var(coder,cblock,node,node_num_arg(node,j),&
                  merge(var_is_var,0,sym==sym_var))
          enddo
          call push_word(coder,pm_type_new_uninitialised)
          call push_word(coder,0)
          call push_word(coder,pm_type_new_type)
          call push_word(coder,0)
          call trav_type(coder,node,node_arg(node,node_numargs(node)))
          call make_type(coder,3)
          call make_type(coder,3)
          call code_num(coder,pop_word(coder))
          call make_sp_call(coder,cblock,node,sym_var,1,node_numargs(node)-1)
       case(sym_over)
          call trav_over_stmt(coder,cblock,list,node)
       case(sym_assign)
          call trav_assign_define(coder,cblock,list,node)
       case(sym_assign_list)
          call trav_assign_define_list(coder,cblock,list,node)
       case(sym_all)
          call trav_all_stmt(coder,cblock,list,node)
       case(sym_where,sym_split,sym_check,sym_amp)
          call trav_xexpr(coder,cblock,listp,node)
       case(sym_sync)
          call trav_sync_stmt(coder,cblock,list,node)
       case(sym_par)
          call trav_par_stmt(coder,cblock,list,node)
       case(sym_task)
          call trav_task(coder,cblock,list,node)
       case(sym_any,sym_any_invar)
          call trav_any_stmt(coder,cblock,list,node,sym)
       case(sym_yield)
          p=node_arg(node,1)
          call trav_call(coder,cblock,node,p,0,.true.)
       case(sym_ddollar)
          n=node_num_arg(node,1)
          select case(n)
          case(sym_infer_stack,sym_show,sym_show_stack)
             if(node_numargs(node)/=1) then
                call code_error(coder,node,'"$$'//sym_names(n)//&
                     '" does not take arguments',warn=.true.)
             else
                call make_sp_call(coder,cblock,node,n,0,0)
             endif
          case(sym_infer_type,sym_infer_type_and_stack)
             if(node_numargs(node)/=2) then
                call code_error(coder,node,'"$$'//sym_names(n)//&
                     '" takes exactly one argument',warn=.true.)
             else
                cblock2=make_cblock(coder,cblock,node,sym_using)
                call trav_expr(coder,cblock2,node,node_arg(node,2))
                call make_sp_call(coder,cblock2,node,sym_pm_dump,1,0)
                call close_cblock(coder,cblock2)
                call make_sp_call(coder,cblock,node,n,1,0)
             endif
          case(sym_dump)
             j=node_numargs(node)
             if(j/=2.and.j/=3) then
                call code_error(coder,node,&
                     '"$$dump" takes one or two arguments',warn=.true.)
             else
                call trav_expr(coder,cblock,node,node_arg(node,2))
                if(j==3) call trav_expr(coder,cblock,node,node_arg(node,3))
                call make_comm_sys_call(coder,cblock,node,sym_pm_dump,j-1,0)
             endif
          case default
             call code_error(coder,node,'Pragma not recognised: $$'//&
                  trim(pm_name_as_string(coder%context,n)),warn=.true.)
          end select
       case(sym_pm_send:sym_pm_serve)
          base=coder%vtop
          call make_sys_var(coder,cblock,node,node_get_num(node,node_args),&
               var_is_shadowed)
          call dup_code(coder)
          call make_sys_var(coder,cblock,node,node_get_num(node,node_args+1),&
               var_is_shadowed)
          call make_sys_var(coder,cblock,node,node_get_num(node,node_args+2),&
               var_is_shadowed)
          call trav_expr(coder,cblock,node,node_arg(node,4))
          call trav_expr(coder,cblock,node,node_arg(node,5))
          if(sym==sym_pm_send.or.sym==sym_pm_collect) then
             call trav_expr(coder,cblock,node,node_arg(node,6))
             call trav_expr(coder,cblock,node,node_arg(node,7))
             call trav_stmt_list(coder,cblock,node,node_arg(node,8),sym_caret)
             call make_sp_call(coder,cblock,node,sym,5,3)
          else
             call make_sys_var(coder,cblock,node,sym_pm_recv,var_is_shadowed)
             call trav_expr(coder,cblock,node,node_arg(node,6))
             cblock2=make_cblock(coder,cblock,node,sym_pm_send)
             call trav_expr(coder,cblock2,node,node_arg(node,7))
             call init_var(coder,cblock2,node,&
                  coder%vstack(coder%vtop-3))
             call make_sp_call(coder,cblock,node,sym,5,3)
             call close_cblock(coder,cblock2)
          endif
          if(coder%vtop/=base) call pm_panic('pm_send/recv')
       case(sym_pm_bcast)
          call make_sys_var(coder,cblock,node,node_get_num(node,node_args),&
               var_is_shadowed)
          call make_sys_var(coder,cblock,node,node_get_num(node,node_args+1),&
               var_is_shadowed)
          call trav_expr(coder,cblock,node,node_arg(node,3))
          call trav_expr(coder,cblock,node,node_arg(node,4))
          call trav_expr(coder,cblock,node,node_arg(node,5))
          call trav_stmt_list(coder,cblock,node,node_arg(node,6),sym_caret)
          call make_sp_call(coder,cblock,node,sym,4,2)
       case(sym_pm_recv_req)
          call make_sys_var(coder,cblock,node,node_get_num(node,node_args),&
               var_is_shadowed)
          call make_sys_var(coder,cblock,node,node_get_num(node,node_args+1),&
               var_is_shadowed)
          call trav_expr(coder,cblock,node,node_arg(node,3))
          call make_sys_var(coder,cblock,node,sym_pm_recv,var_is_shadowed)
          cblock2=make_cblock(coder,cblock,node,sym_pm_send)
          call trav_expr(coder,cblock2,node,node_arg(node,4))
          call init_var(coder,cblock2,node,coder%vstack(coder%vtop-2))
          call make_sp_call(coder,cblock,node,sym,3,2)
          call close_cblock(coder,cblock2)
       case(sym_pm_recv_assn)
          call make_sys_var(coder,cblock,node,node_get_num(node,node_args),&
               var_is_shadowed)
          call make_sys_var(coder,cblock,node,node_get_num(node,node_args+1),&
               var_is_shadowed)
          call make_sys_var(coder,cblock,node,node_get_num(node,node_args+2),&
               var_is_shadowed)
          call trav_expr(coder,cblock,node,node_arg(node,4))
          call trav_expr(coder,cblock,node,node_arg(node,5))
          call trav_expr(coder,cblock,node,node_arg(node,6))
          call trav_stmt_list(coder,cblock,node,node_arg(node,7),sym_caret)
          call make_sp_call(coder,cblock,node,sym,4,3)
       case(sym_pm_do,sym_pm_do_at)
          if(sym==sym_pm_do_at) then
             if(pm_is_compiling) then
                call code_null(coder)
             else
                call trav_expr(coder,cblock,node,node_arg(node,1))
             endif
             call trav_expr(coder,cblock,node,node_arg(node,2))
          endif
          do j=merge(1,3,sym==sym_pm_do),node_numargs(node)-1,2
             p=node_arg(node,j)
             call make_sys_var(coder,cblock,node,node_num_arg(node,j),&
                  var_is_shadowed+var_is_var)
             call trav_expr(coder,cblock,node,node_arg(node,j+1))
          enddo
          call trav_stmt_list(coder,cblock,node,node_arg(node,node_numargs(node)),sym_caret)
          call make_sp_call(coder,cblock,node,sym,node_numargs(node),0)
       case(sym_pm_each_index)
          call trav_pm_each_index(coder,cblock,list,node,.false.)
       case(sym_pm_context)
          call trav_pm_context(coder,cblock,list,node)
       case(sym_pm_set_dotdotdot)
          call make_sys_var(coder,cblock,list,sym_dotdotdot,var_is_param+var_is_varg)
          call trav_expr(coder,cblock,list,node_arg(node,1))
          call make_sp_call(coder,cblock,list,sym_pm_set_dotdotdot,1,1)
       case(sym_pm_head_node)
          call trav_stmt_list(coder,cblock,node,node_arg(node,1),sym_caret)
          call make_sp_call(coder,cblock,node,sym,1,0)
       case(sym_pm_over)
          call trav_xexpr(coder,cblock,node,node_arg(node,1))
          call trav_stmt_list(coder,cblock,node,node_arg(node,2),sym_pm_for)
          call make_sp_call(coder,cblock,node,sym,2,0)
       case(sym_pm_for)
          call trav_xexpr(coder,cblock,node,node_arg(node,1))
          save_par_state=coder%par_state
          coder%par_state=par_state_for
          call trav_stmt_list(coder,cblock,node,node_arg(node,2),sym_pm_for)
          coder%par_state=save_par_state
          call make_sp_call(coder,cblock,node,sym,2,0)
       case(sym_pm_shared,sym_pm_shared_always,sym_pm_chan,sym_pm_chan_always)
          save_par_state=coder%par_state
          ! Assumes test correctly separates shared/chan
          coder%par_state=merge(par_state_for,par_state_none,sym>=sym_pm_chan)
          call trav_stmt_list(coder,cblock,node,node_arg(node,1),sym)
          coder%par_state=save_par_state
          call make_sp_call(coder,cblock,node,sym,1,0)
       case(sym_pm_foreach)
          lex_scope=push_lex_scope(coder)
          call trav_xexpr(coder,cblock,node,node_arg(node,1))
          coder%lex_scope=lex_scope
          save_par_state=coder%par_state
          coder%par_state=par_state_masked
          call trav_stmt_list(coder,cblock,node,&
               node_arg(node,2),sym_pm_foreach)
          coder%par_state=save_par_state
          call get_lex_scope(coder,node)
          call make_sp_call(coder,cblock,node,sym_pm_foreach,3,0)
          call pop_lex_scope(coder)
       case default
          if(sym>0.and.sym<num_sym) then
             write(*,*) 'SYM=',sym_names(sym)
          else
             write(*,*) 'SYM=<non symbol> Number=',sym
          endif
          !call dump_parse_tree(coder%context,6,listp,2)
          call code_error(coder,list,'Err::')
          !call pm_dump_tree(coder%context,6,node,2)
          call pm_panic('Unknown node sym in trav_stmt_list')
       end select
       if(coder%vtop/=vbase) then
          write(*,*) 'Current code'
          write(*,*) '============'
          call qdump_code_tree(coder,pm_null_obj,6,cblock,1)
          write(*,*) '============'
          write(*,*) 'Remaining node stack:',coder%vtop,vbase
          write(*,*) '======='
          do j=vbase+1,coder%vtop
             call qdump_code_tree(coder,pm_null_obj,6,coder%vstack(j),1)
             write(*,*) '======='
          enddo
          write(*,*) 'MISMATCH-->',coder%vtop,vbase
          call pm_panic('trav_open_stmt_list')
       endif
       if(coder%wtop/=wbase) then
          write(*,*) 'Statement wstack mismatch',sym_names(sym),wbase,coder%wtop
          call pm_panic('trav_open_stmt_list wstack')
       endif
       if(debug_codegen) write(*,*) 'TRAVERSED>',sym_names(sym)
    enddo
10  continue
  contains
    include 'fisnull.inc'
    include 'fname.inc'   
  end subroutine trav_open_stmt_list


  function loop_par_state(coder,node,sym,isinvar) result(new_par_state)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: sym
    logical,intent(in):: isinvar
    integer:: new_par_state
    integer:: par_state
    par_state=coder%par_state
    if(par_state==par_state_none) then
       if(isinvar) then
          call code_error(coder,node,&
               '"'//trim(sym_names(sym))//'" cannot be used outside of a parallel context')
       endif
    else
       if(isinvar) then
          if(par_state>=par_state_cond) then
             call code_error(coder,node,&
                  '"'//trim(sym_names(sym))//'" cannot be used in this conditional context')
          endif
       endif
    endif
    coder%par_state=merge(par_state,par_state_none,isinvar)
  end function loop_par_state
  
  !========================================================
  ! switch statement - cases and otherwise clause
  ! assumes expression is in var
  !========================================================
  recursive subroutine trav_switch_stmt(coder,cblock,stmt,idx,var,sym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    type(pm_ptr),intent(in):: stmt,var
    integer,intent(in):: idx,sym
    type(pm_ptr):: cblock2
    integer:: base,n,i,lex_scope
    lex_scope=push_lex_scope(coder)
    base=coder%vtop

    call trav_xexpr(coder,cblock,stmt,node_arg(stmt,idx))
    n=coder%vtop-base
    do i=1,n
       call code_val(coder,var)
       call code_val(coder,coder%vstack(base+i))
       call make_sys_call_rtn(coder,cblock,stmt,&
            sym_checkcase,2,1)
       if(i>1) then
          call make_sys_call_rtn(coder,cblock,stmt,&
               sym_or,2,1)
       endif
    enddo
    coder%vstack(base+1)=top_code(coder)
    coder%vtop=base+1
    if(sym==sym_if_invar) then
       call code_check_invar(coder,cblock,stmt,top_code(coder),sym_switch_invar)
    endif
    coder%lex_scope=lex_scope
    call trav_stmt_list(coder,cblock,stmt,&
         node_arg(stmt,idx+1),sym_switch)
    if(idx==node_numargs(stmt)-2) then
       if(pm_fast_isnull(node_arg(stmt,idx+2))) then
          call code_null(coder)
       else
          call trav_stmt_list(coder,cblock,stmt,&
               node_arg(stmt,idx+2),sym_switch)
       endif
    else
       cblock2=make_cblock(coder,cblock,stmt,sym_switch)
       call trav_switch_stmt(coder,cblock2,stmt,idx+2,var,sym)
       call close_cblock(coder,cblock2)
    endif
    call get_lex_scope(coder,stmt)
    call make_sp_call(coder,cblock,stmt,sym,4,0)
    call pop_lex_scope(coder)
  contains
    include 'fisnull.inc'
  end subroutine trav_switch_stmt

  !========================================================
  ! Traverse "any" statement (sym is "any" or "any invar")
  !========================================================
  recursive subroutine trav_any_stmt(coder,cblock,pnode,node,sym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer,intent(in):: sym
    integer:: flags,start,finish,vb,lex_scope
    type(pm_ptr):: cblock2,v,var

    lex_scope=push_lex_scope(coder)
    
    if(pm_fast_isnull(node_arg(node,2))) then
       flags=var_is_shadowed+var_is_var
       call trav_expr(coder,cblock,node,node_arg(node,1))
    else
       flags=var_is_var
       call trav_xexpr(coder,cblock,node,node_arg(node,2))
    endif
    v=top_code(coder)
    if(sym==sym_any_invar) then
       call code_check_invar(coder,cblock,node,v,sym_any_invar)
    endif
    cblock2=make_cblock(coder,cblock,node,sym_any)
    call make_var(coder,cblock,node,node_num_arg(node_arg(node,1),1),flags)
    vb=coder%top
    var=top_code(coder)
    start=coder%index
    call swap_code(coder)
    coder%lex_scope=lex_scope
    call trav_open_stmt_list(coder,cblock2,node,node_arg(node,3))
    if(cnode_flags_set(var,var_flags,var_is_changed)) then
       call code_val(coder,var)
       call dup_expr(coder,v)
       call make_sys_call_rtn(coder,cblock2,node,sym_as,2,1)
       call hide_vars(coder,vb,vb)
       if(pm_fast_isnull(node_arg(node,2))) then
          call make_assignment_noalias(coder,cblock2,node,node_arg(node,1))
       else
          call make_assignment_noalias(coder,cblock2,node,node_arg(node,2))
       endif
       call reveal_vars(coder,vb,vb)
    endif
    finish=coder%index
    call close_cblock(coder,cblock2)
    call code_val(coder,v)
    v=pm_fast_newnc(coder%context,pm_int,2)
    coder%temp2=v
    v%data%i(v%offset)=start
    v%data%i(v%offset+1)=finish
    call make_const(coder,cblock,node,coder%temp2)
    call get_lex_scope(coder,node)
    call make_sp_call(coder,cblock,node,sym_any,4,1)
    call drop_code(coder)
    call hide_vars(coder,vb,vb)
    call pop_lex_scope(coder)
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
  end subroutine trav_any_stmt

  subroutine trav_pm_each_index(coder,cblock,nodep,node,isexpr)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,nodep,node
    logical,intent(in):: isexpr
    integer:: base,start,finish
    type(pm_ptr):: cblock2,v
    base=coder%top
    if(isexpr) then
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
    endif
    call trav_expr(coder,cblock,node,node_arg(node,2))
    start=coder%index+1
    cblock2=make_cblock(coder,cblock,node,sym_pm_each_index)
    call make_var(coder,cblock2,node,node_num_arg(node,1),0)
    call swap_code_1_2(coder)
    if(isexpr) then
       call trav_expr(coder,cblock2,node,node_arg(node,3))
    else
       call trav_open_stmt_list(coder,cblock2,node,node_arg(node,3))
    endif
    call close_cblock(coder,cblock2)
    finish=coder%index
    v=pm_fast_newnc(coder%context,pm_int,2)
    coder%temp2=v
    v%data%i(v%offset)=start
    v%data%i(v%offset+1)=finish
    call make_const(coder,cblock,node,coder%temp2)
    if(isexpr) call swap_code(coder)
    coder%temp2=pm_null_obj
    call make_sp_call(coder,cblock,node,sym_pm_each_index,&
         merge(4,3,isexpr),merge(2,1,isexpr))
    call hide_vars(coder,base+1,base+1)
  contains
    include 'fnewnc.inc'
  end subroutine trav_pm_each_index

  !====================================================================
  ! Traverse a PM__context statement node
  ! Creates new variable entries and runs block with modified
  ! values of coder%state_base (start-1 of block of context vars) and
  ! coder%mask (location on stack of PM__mask variable)
  !====================================================================
  subroutine trav_pm_context(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: save_state_base,save_mask
    save_state_base=coder%state_base
    save_mask=coder%mask
    coder%state_base=coder%top
    call trav_name(coder,cblock,node,sym_name,node_num_arg(node,1))
    call push_var(coder,sym_topology,pop_code(coder))
    call trav_name(coder,cblock,node,sym_name,node_num_arg(node,2))
    call push_var(coder,sym_outer,pop_code(coder))
    call trav_name(coder,cblock,node,sym_name,node_num_arg(node,3))
    call push_var(coder,sym_region,pop_code(coder))
    call trav_name(coder,cblock,node,sym_name,node_num_arg(node,4))
    call push_var(coder,sym_subregion,pop_code(coder))
    call trav_name(coder,cblock,node,sym_name,node_num_arg(node,5))
    call push_var(coder,sym_here_in_tile,pop_code(coder))
    call trav_name(coder,cblock,node,sym_name,node_num_arg(node,6))
    call push_var(coder,sym_mask,pop_code(coder))
    coder%mask=coder%top
    call trav_open_stmt_list(coder,cblock,node,node_arg(node,7))
    coder%mask=save_mask
    coder%state_base=save_state_base
  end subroutine trav_pm_context
  
  !====================================================================
  ! Traverse foreach statement converting to a call to PM__foreach_stmt
  !====================================================================
  subroutine trav_foreach_stmt(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    type(pm_ptr):: stmts,condition,iter,cblock2,amps
    integer:: lex_scope,base,xtop,dtop,sym,call_sym

    amps=coder%iter_amps
    iter=node_arg(node,1)
    condition=node_arg(node,2)
    stmts=node_arg(node,4)

    call trav_subexpr(coder,cblock,node,node_arg(node,3),base,xtop,dtop)

    if(node_sym(condition)==sym_while) then
       lex_scope=push_lex_scope(coder)
       call trav_xexpr(coder,cblock,node,&
            node_arg(condition,1))
       coder%lex_scope=lex_scope
       cblock2=make_cblock(coder,cblock,node,sym_each)
    else
       cblock2=cblock
    endif

    !!! coder%par_state
    
    if(base>=0) call hide_where_vars(coder,base+1,dtop)
    
    if(pm_fast_isnull(condition)) then
       call make_block_proc(coder,cblock2,node_arg(node,1),pm_null_obj,&
            int(coder%iter_block_amps%offset),pm_null_obj,0,&
            stmts,iter,.true.,.false.)
    else
       call make_block_proc(coder,cblock2,node_arg(node,1),pm_null_obj,&
            int(coder%iter_block_amps%offset),&
            node_arg(condition,1),1,stmts,iter,.true.,.false.)
    endif

    call code_val(coder,find_sys_var(coder,node,sym_block_proc_a))
    call code_val(coder,find_sys_var(coder,node,sym_block_inouts_a))
    call code_val(coder,find_sys_var(coder,node,sym_block_ins_a))

    if(base>=0) call reveal_vars(coder,base+1,dtop)
    
    call make_iter_lists(coder,cblock2,iter,node_numargs(iter),.true.,.false.)

    call trav_expr(coder,cblock2,node,node_arg(iter,2))
    call make_sys_call_rtn(coder,cblock2,node,sym_hash,1,1)
    
    if(base>=0) call hide_where_vars(coder,base+1,xtop)

    sym=node_sym(node)
    if(sym==sym_foreach_invar) then
       call_sym=sym_pm_foreach_invar_stmt
    else
       call_sym=sym_pm_foreach_stmt
    endif
    
    call make_full_sys_call(coder,cblock2,node,call_sym,6,0,amps,pm_null_obj,&
         pm_null_obj,proccall_is_comm+proccall_is_general)
    
    if(node_sym(condition)==sym_while) then
       call close_cblock(coder,cblock2)
       call code_null(coder)
       call get_lex_scope(coder,node)
       call make_sp_call(coder,cblock,node,&
            sym_if,4,0)
       call pop_lex_scope(coder)
    endif

  contains
    include 'fisnull.inc'
  end subroutine trav_foreach_stmt

  !====================================================================
  ! Traverse a for or forall statement node
  ! converting to a call to PM__for_stmt / PM__forall_stmt
  !====================================================================
  subroutine trav_for_stmt(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    type(pm_ptr):: stmts,iter,amps,keys,keynames
    integer:: i,base,xtop,dtop
    
    amps=coder%iter_amps
    iter=node_arg(node,1)
    keys=node_arg(node,2)
    if(pm_fast_isnull(keys)) then
       keynames=pm_null_obj
    else
       keynames=node_arg(keys,2)
       keys=node_arg(keys,1)
    endif
    stmts=node_arg(node,4)

    call trav_subexpr(coder,cblock,node,node_arg(node,3),base,xtop,dtop)
    if(base>=0) call hide_where_vars(coder,base+1,dtop)
    
    call make_block_proc(coder,cblock,node_arg(node,1),pm_null_obj,&
         int(coder%iter_block_amps%offset),pm_null_obj,0,stmts,iter,.true.,.true.)
    
    call code_val(coder,find_sys_var(coder,node,sym_block_proc_a))
    call code_val(coder,find_sys_var(coder,node,sym_block_inouts_a))
    call code_val(coder,find_sys_var(coder,node,sym_block_ins_a))

    if(base>=0) call reveal_vars(coder,base+1,dtop)
    
    call make_iter_lists(coder,cblock,iter,node_numargs(iter),.true.,.true.)

    call trav_expr(coder,cblock,node,node_arg(iter,2))
    call make_sys_call_rtn(coder,cblock,node,sym_hash,1,1)
    
    if(.not.pm_fast_isnull(keys)) then
       call trav_exprlist(coder,cblock,node,keys)
       call make_code(coder,keys,cnode_is_arglist,node_numargs(keys))
       keys=pop_code(coder)
       coder%temp2=keys ! protect from GC
    endif
    
    if(base>=0) call hide_where_vars(coder,base+1,xtop)
    
    call make_full_sys_call(coder,cblock,node,&
         merge(sym_pm_for_stmt,sym_pm_forall_stmt,node_sym(node)==sym_for),&
         7,0,amps,keys,keynames,proccall_is_comm+proccall_is_general)

    coder%temp2=pm_null_obj
    
  contains
    include 'fisnull.inc'
  end subroutine trav_for_stmt

  
  !====================================================================
  !  
  !====================================================================
  subroutine trav_par_stmt(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    type(pm_ptr):: stmts,iter,amps,keys,keynames
    integer:: i,base,xtop,dtop
    
    amps=coder%block_amp
    keys=node_arg(node,1)
    if(pm_fast_isnull(keys)) then
       keynames=pm_null_obj
    else
       keynames=node_arg(keys,2)
       keys=node_arg(keys,1)
    endif
    stmts=node_arg(node,3)

    call trav_subexpr(coder,cblock,node,node_arg(node,2),base,xtop,dtop)
    if(base>=0) call hide_where_vars(coder,base+1,dtop)
    
    call make_block_proc(coder,cblock,node,pm_null_obj,&
         int(coder%comm_amp%offset),pm_null_obj,0,stmts)
  
    call code_val(coder,find_sys_var(coder,node,sym_block_proc_a))
    call code_val(coder,find_sys_var(coder,node,sym_block_inouts_a))
    call code_val(coder,find_sys_var(coder,node,sym_block_ins_a))

    if(base>=0) call reveal_vars(coder,base+1,dtop)
    
    call make_long_const(coder,cblock,node,int((node_numargs(node_arg(stmts,1))-1)/2,pm_ln))
    
    if(.not.pm_fast_isnull(keys)) then
       call trav_exprlist(coder,cblock,node,keys)
       call make_code(coder,keys,cnode_is_arglist,node_numargs(keys))
       keys=pop_code(coder)
       coder%temp2=keys ! protect from GC
    endif
    
    if(base>=0) call hide_where_vars(coder,base+1,xtop)
    
    call make_full_sys_call(coder,cblock,node,&
         sym_pm_par_stmt,4,0,amps,keys,keynames,&
         proccall_is_comm+proccall_is_general)

    coder%temp2=pm_null_obj
    
  contains
    include 'fisnull.inc'
  end subroutine trav_par_stmt

  recursive subroutine trav_task(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: i,n,save_par_state
    type(pm_ptr):: arg,cblock2
    n=node_numargs(node)
    do i=2,n,2
       arg=node_arg(node,i)
       call make_var(coder,cblock,arg,node_num_arg(arg,1),0)
       call make_long_const(coder,cblock,arg,int(i/2,pm_ln))
       call make_sys_call(coder,cblock,arg,sym_clone,1,1)
    enddo
    call trav_open_stmt_list(coder,cblock,node,node_arg(node,1))
    save_par_state=coder%par_state
    coder%par_state=par_state_par
    do i=3,n,2
       arg=node_arg(node,i)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       cblock2=make_cblock(coder,cblock,node,sym_task)
       call swap_code(coder)
       call make_long_const(coder,cblock2,arg,int((i-1)/2,pm_ln))
       call make_comm_sys_call(coder,cblock2,arg,sym_check_task,1,1)
       call close_cblock(coder,cblock2)
       call trav_stmt_list(coder,cblock,node,arg,sym_task)
    enddo
    coder%par_state=save_par_state
    call make_sp_call(coder,cblock,node,sym_task,3*(n/2),0)
  end subroutine trav_task

  
  !========================================================
  ! Traverse "all" assignment
  !========================================================
  recursive subroutine trav_all_stmt(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: base,xtop
    type(pm_ptr):: var,arg1,arg3
    integer:: sym1,sym3
    if(coder%par_state==par_state_none) then
       call code_error(coder,node,&
            'Cannot have an "all" statement outside of a parallel context')
    elseif(coder%par_state==par_state_cond.or.coder%par_state==par_state_par) then
       call code_error(coder,node,&
            'In a conditional statement, an "all" statement must be enclosed by a "sync" statement')
    endif
    
    arg1=node_arg(node,1)
    sym1=node_sym(arg1)
    arg3=node_arg(node,3)
    sym3=node_sym(arg3)
    
    call trav_subexpr(coder,cblock,node,node_arg(node,4),base,xtop)
    call trav_ref_to_var(coder,cblock,node,node_num_arg(node,1),.true.)
    var=top_code(coder)

    call trav_expr(coder,cblock,node,node_arg(node,2))
    call trav_expr(coder,cblock,node,arg3)

    if(node_sym(arg1)==sym_reference) then
       !call trav_comm_ref(coder,cblock,node,arg1,.true.,.true.,var)
       call make_comm_sys_call(coder,cblock,node,sym_all_stmt,3+node_numargs(arg1),0)
    else
       call make_comm_sys_call(coder,cblock,node,sym_all_stmt,3,0)
    endif
    if(base>=0) call hide_where_vars(coder,base+1,xtop)
    
  end subroutine trav_all_stmt

  !================================================================
  ! Traverse a reference leaving an object or object reference
  ! on vstack
  ! If isalias is true then a second element on the stack is a
  ! list of reference elements to be used in alias checking
  !================================================================
  subroutine trav_reference(coder,cblock,pnode,node,islhs,skipdot,isalias,call_n)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    logical,intent(in):: islhs,skipdot,isalias
    integer,intent(out),optional:: call_n
    type(pm_ptr):: arg
    integer:: i,j,n,sym,start,base,vbase,abase,atop
    logical:: iscomm,isvar
    
    sym=node_sym(node) 
    if(sym==sym_name) then
       call trav_ref_to_var(coder,cblock,node,node_num_arg(node,1),islhs)
       return
    endif

    arg=node_arg(node,1)
    if(node_sym(arg)==sym_name) then
       call trav_ref_to_var(coder,cblock,arg,node_num_arg(arg,1),islhs)
       isvar=.true.
    else
       call trav_expr(coder,cblock,node,arg)
       isvar=.false.
    endif
   
    start=2
    arg=node_arg(node,start)
    sym=node_sym(arg)
    abase=coder%vtop
    if(sym==sym_pling) then
       call make_comm_sys_call_rtn(coder,cblock,node,sym_pm_pling,1,1)
       iscomm=.true.
       start=3
    elseif(sym==sym_at) then
       call make_comm_sys_call_rtn(coder,cblock,node,sym_pm_at,1,merge(2,1,isalias))
       iscomm=.true.
       start=3
    else
       arg=top_code(coder)
       if(cnode_get_kind(arg)==cnode_is_var) then
          if(isvar) then
             iscomm=cnode_flags_set(arg,var_flags,var_is_maybe_not_private)
          else
             iscomm=coder%par_state/=par_state_none
          endif
       else
          iscomm=.false.
       endif
    endif

    vbase=coder%vtop
    base=coder%vtop-start+2
    
    n=node_numargs(node)
    do i=start,n
       arg=node_arg(node,i)
       sym=node_sym(arg)
       select case(sym)
       case(sym_dot)
          call code_name_as_string(coder,cblock,arg,node_num_arg(arg,1))
       case(sym_open_brace)
          call trav_expr(coder,cblock,arg,node_arg(arg,1))
          call make_sp_call(coder,cblock,arg,sym_open_brace,1,0,flags=call_is_no_touch)
       case(sym_sub)
          call trav_expr(coder,cblock,arg,node_arg(arg,1))
       case(sym_open)
          call trav_expr(coder,cblock,arg,node_arg(arg,1))
          call trav_expr(coder,cblock,arg,node_arg(arg,2))
          call make_sp_call_rtn(coder,cblock,node,sym_list,2,1)
       end select
    enddo

    atop=coder%vtop

    call code_val(coder,coder%vstack(vbase))
    
    i=start
    
    if(skipdot) then
       arg=node_arg(node,i)
       sym=node_sym(arg)
       do while(sym==sym_dot.or.sym==sym_open_brace)
          call code_val(coder,coder%vstack(base+i))
          call make_sp_call_rtn(coder,cblock,arg,sym_dot,2,1)
          i=i+1
          if(i>n) exit
          arg=node_arg(node,i)
          sym=node_sym(arg)
       enddo
    endif
    
    if(i<n) then
       do j=i+1,n
          call code_val(coder,coder%vstack(base+j))
       enddo
       if(present(call_n)) then
          call_n=n-i
       else
          if(.not.iscomm) then
             call make_sys_call_rtn(coder,cblock,node,&
                  merge(sym_rhs,sym_lhs,islhs),n-i+1,1)
          else
             call make_comm_sys_call_rtn(coder,cblock,node,&
                  merge(sym_rhs,sym_lhs,islhs),n-i+1,1)
          endif
       endif
    end if
    
    if(isalias) then
       call dup_expr(coder,coder%vstack(vbase))
       do j=abase+1,atop
          call dup_expr(coder,coder%vstack(base+j)) 
       enddo
       call make_sp_call_rtn(coder,cblock,node,sym_list,atop-abase+1,1)
       coder%vstack(vbase)=coder%vstack(coder%vtop-1)
       coder%vstack(vbase+1)=coder%vstack(coder%vtop)
       coder%vtop=vbase+1
    else
       coder%vstack(vbase)=coder%vstack(coder%vtop)
       coder%vtop=vbase
    endif
    
  end subroutine trav_reference

  function check_aliased(coder,node1,node2,str) result(aliased)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node1,node2
    character(len=*):: str
    logical:: aliased
    logical:: hard_aliased
    aliased=is_aliased(node1,node2,hard_aliased)
    if(hard_aliased) then
       call code_error(coder,node1,str)
       call code_error(coder,node2,'Corresponding variable access for the above error')
    endif
  end function check_aliased

  function is_aliased(node1,node2,hard_aliased) result(aliased)
    type(pm_ptr),intent(in):: node1,node2
    logical,intent(out),optional:: hard_aliased
    logical:: aliased
    integer:: i,start,ds1,ds2,sym1,sym2
    type(pm_ptr):: arg1,arg2

    arg1=node_arg(node1,1)
    sym1=node_sym(arg1)
    arg2=node_arg(node2,1)
    sym2=node_sym(arg2)
    if(sym1/=sym_name.or.sym2/=sym_name) then
       aliased=.false.
       if(present(hard_aliased)) hard_aliased=.false.
       return
    else
       if(node_num_arg(arg1,1)/=node_num_arg(arg2,1)) then
          aliased=.false.
          if(present(hard_aliased)) hard_aliased=.false.
          return
       endif
    endif
    
    start=2
    sym1=node_sym(node_arg(node1,2))
    sym2=node_sym(node_arg(node2,2))
    if(sym1==sym_pling.or.sym2==sym_pling) then
       if(sym1/=sym2) then
          aliased=.true.
          if(present(hard_aliased)) hard_aliased=.true.
          return
       endif
       start=3
    endif

    ds1=merge(1,0,sym1==sym_at)
    ds2=merge(1,0,sym2==sym_at)
       
    do i=start,min(node_numargs(node1),node_numargs(node2))
       arg1=node_arg(node1,i+ds1)
       arg2=node_arg(node2,i+ds2)
       if(node_sym(arg1)==sym_dot.and.node_sym(arg2)==sym_dot) then
          if(node_num_arg(arg1,1)/=node_num_arg(arg2,1)) then
             aliased=.false.
             if(present(hard_aliased)) hard_aliased=.false.
             return
          endif
       else
          aliased=.true.
          if(present(hard_aliased)) hard_aliased=.false.
          return
       endif
    enddo
    aliased=.true.
    if(present(hard_aliased)) hard_aliased=.true.
  end function is_aliased
  
  subroutine code_name_as_string(coder,cblock,node,name)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: name
    call make_literal_const(coder,cblock,node,&
         pm_new_literal_value_type(coder%context,&
         pm_name_val(coder%context,name),name))
  end subroutine code_name_as_string


  !========================================================
  ! Traverse "sync" statement
  !========================================================
  recursive subroutine trav_sync_stmt(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: save_par_state,base
    type(pm_ptr):: label,body
    label=node_arg(node,1)
    body=node_arg(node,2)
    select case(coder%par_state)
    case(par_state_none)
       call code_error(coder,node,&
            'Cannot have a "sync" statement outside of a parallel context')
    case(par_state_for,par_state_comm_proc,par_state_sync)
       if(.not.pm_fast_isnull(label)) then
          call code_error(coder,node,&
               'Can only have "sync(...)" statements inside a branch of a conditional statement')
       endif
    end select
    save_par_state=coder%par_state
    coder%par_state=par_state_sync

    sym=node_sym(body)
    if(sym==sym_open) then
       call trav_call(coder,cblock,node,body,0,.true.)
    elseif(sym==sym_assign) then
       call trav_sync_assign(coder,cblock,node,body)
    else
       call trav_stmt_list(coder,cblock,node,node_arg(node,2),sym_sync,open_scope=.true.)
    endif
    if(.not.pm_fast_isnull(parser)) then
       call code_val(coder,label)
       call make_sp_call(coder,cblock,node,sym_sync,2,0)
    endif
    coder%par_state=save_par_state
  contains
    include 'fisnull.inc'
  end subroutine trav_sync_stmt

  subroutine trav_sync_assign(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    
  end subroutine trav_sync_assign

  
  !========================================================
  ! Traverse "sync while" statement
  !========================================================
  recursive subroutine trav_sync_while_stmt(coder,cblock,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer:: save_par_state
    select case(coder%par_state)
    case(par_state_none)
       call code_error(coder,node,&
            'Cannot have a "sync while" statement outside of a parallel context')
    case(par_state_for,par_state_comm_proc)
       call code_error(coder,node,&
            'Can only have "sync while" statements inside a branch of a conditional statement')
    case(par_state_sync)
       call code_error(coder,node,&
            'Cannot nest "sync" or "sync while" statements inside each other')
    end select
    save_par_state=coder%par_state
    coder%par_state=par_state_sync
    !call trav_name(coder,cblock,node,node_num_arg(node,1))  !!! ref to var?
    call trav_stmt_list(coder,cblock,node,node_arg(node,2),sym_sync_while)
    call make_sp_call(coder,cblock,node,sym_sync_while,2,0)
    coder%par_state=save_par_state
  end subroutine trav_sync_while_stmt

  !========================================================
  ! Traverse statement qualified by a mode
  !========================================================
  recursive subroutine trav_mode_stmt(coder,cblock,node,sym,call_sym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,call_sym
    integer:: base,xtop
    type(pm_ptr):: keynames,keys

    if(coder%par_state==par_state_none) then
       call code_error(coder,node,&
            'Cannot have "'//sym_names(sym)//' statement outside of a parallel context')
    endif
    
    keys=node_arg(node,1)
    if(pm_fast_isnull(keys)) then
       keynames=pm_null_obj
    else
       keynames=node_arg(keys,2)
       keys=node_arg(keys,1)
    endif

    call trav_subexpr(coder,cblock,node,node_arg(node,2),base,xtop)
    
    if(.not.pm_fast_isnull(keys)) then
       call trav_exprlist(coder,cblock,node,keys)
       call make_code(coder,keys,cnode_is_arglist,node_numargs(keys))
       keys=pop_code(coder)
       coder%temp2=keys ! protect from GC
    endif

    if(base>=0) call hide_where_vars(coder,base+1,xtop)

    call make_block_proc(coder,cblock,node,&
         pm_null_obj,&
         int(coder%comm_amp%offset),pm_null_obj,0,&
         node_arg(node,3))
    call code_val(coder,find_sys_var(coder,node,sym_block_proc_a))
    call code_val(coder,find_sys_var(coder,node,sym_block_inouts_a))
    call code_val(coder,find_sys_var(coder,node,sym_block_ins_a))
    call make_full_sys_call(coder,cblock,node,call_sym,3,0,&
         coder%block_amp,keys,keynames,&
         proccall_is_comm+proccall_is_general)

    coder%temp2=pm_null_obj
  contains
    include 'fisnull.inc'
  end subroutine trav_mode_stmt


  !========================================================
  ! Traverse over statement
  !========================================================
  recursive subroutine trav_over_stmt(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: base,xtop,vbase
    type(pm_ptr):: keynames,keys

    if(coder%par_state==par_state_none) then
       call code_error(coder,node,&
            'Cannot have an "over" statement outside of a parallel context')
    elseif(coder%par_state>=par_state_masked) then
       call code_error(coder,node,&
            'Cannot have an "over" statement inside a conditional statement')
    endif
    
    keys=node_arg(node,2)
    if(pm_fast_isnull(keys)) then
       keynames=pm_null_obj
    else
       keynames=node_arg(keys,2)
       keys=node_arg(keys,1)
    endif

    call trav_subexpr(coder,cblock,node,node_arg(node,3),base,xtop)
    
    if(.not.pm_fast_isnull(keys)) then
       call trav_exprlist(coder,cblock,node,keys)
       call make_code(coder,keys,cnode_is_arglist,node_numargs(keys))
       keys=pop_code(coder)
       coder%temp2=keys ! protect from GC
    endif

    call trav_expr(coder,cblock,node,node_arg(node,1))
    vbase=coder%vtop

    if(base>=0) call hide_where_vars(coder,base+1,xtop)

    call make_block_proc(coder,cblock,node,&
         pm_null_obj,&
         int(coder%comm_amp%offset),pm_null_obj,0,&
         node_arg(node,4))
    call code_val(coder,find_sys_var(coder,node,sym_block_proc_a))
    call code_val(coder,find_sys_var(coder,node,sym_block_inouts_a))
    call code_val(coder,find_sys_var(coder,node,sym_block_ins_a))
    call code_val(coder,coder%vstack(vbase))
    call make_full_sys_call(coder,cblock,node,sym_pm_over_stmt,4,0,&
         coder%block_amp,keys,keynames,&
         proccall_is_comm+proccall_is_general)
    call drop_code(coder)
    coder%temp2=pm_null_obj
  contains
    include 'fisnull.inc'
  end subroutine trav_over_stmt
  
  !=================================================================================
  ! Create code to create up to three lists of values
  ! derived from expressions in an iterator node:
  !  [ amp_values ] values [ star_values ]
  ! relating to &x in Y, x in Y and *x in Y iterator entries respectively
  !
  ! Also codes any necessary alias checks between "&" items and other items
  !==================================================================================
  subroutine make_iter_lists(coder,cblock,node,n,may_have_amp,may_have_star)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: n
    logical,intent(in):: may_have_amp,may_have_star
    type(pm_ptr):: arg,arg2
    integer:: i,j,k,kind,m,sym,name,base,base2,nalias
    
    base=coder%vtop

    ! Check for any potential aliases
    if(may_have_amp) then
       do i=1,n,2
          if(node_sym(node_arg(node,i))==sym_amp) then
             arg=node_arg(node,i+1)
             name=node_num_arg(arg,1)
             kind=node_sym(arg)
             do j=1,n,2
                if(j/=i) then
                   arg2=node_arg(node,j+1)
                   k=node_sym(arg2)
                   if(k==sym_name.or.k==sym_reference) then
!!$                      call check_aliased(coder,cblock,arg,arg2,&
!!$                           '"&" item aliases with another item')
                   endif
                endif
             enddo
          endif
       enddo
    endif
    
    ! Now evaluate each value
    if(may_have_amp) then
       do i=1,n,2
          arg=node_arg(node,i+1)
          k=node_sym(arg)
          if(k==sym_name.or.k==sym_reference) then
             call trav_reference(coder,cblock,node,arg,&
                  node_sym(node_arg(node,i))/=sym_amp,.true.,.false.)
          else
             call trav_expr(coder,cblock,node,node_arg(node,i+1))
          endif
       enddo
    else
       do i=1,n,2
          call trav_expr(coder,cblock,node,node_arg(node,i+1))
       enddo
    endif

    ! Create conformity checks
    call dup_expr(coder,coder%vstack(base+1))
    call make_sys_call(coder,cblock,node,check_fn(node_arg(node,1)),1,0)
    do i=2,n/2
       call dup_expr(coder,coder%vstack(base+i))
       call dup_expr(coder,coder%vstack(base+1))
       call make_sys_call(coder,cblock,node,check_fn(node_arg(node,i*2-1)),2,0)
    enddo

    ! Finally create the lists
    base2=coder%vtop
 
    if(may_have_amp) then
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       m=0
       do i=1,n,2
          if(node_sym(node_arg(node,i))==sym_amp) then
             call code_val(coder,coder%vstack(base+(i+1)/2))
             m=m+1
          endif
       enddo
       call make_basic_sp_call(coder,cblock,node,sym_pm_list,m,1)
    endif
    call make_temp_var(coder,cblock,node)
    call dup_code(coder)
    m=0
    do i=1,n,2
       sym=node_sym(node_arg(node,i))
       if(sym/=sym_amp.and.sym/=sym_mult) then
          call code_val(coder,coder%vstack(base+(i+1)/2))
          m=m+1
       endif
    enddo
    call make_basic_sp_call(coder,cblock,node,sym_pm_list,m,1)
    if(may_have_star) then
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       m=0
       do i=1,n,2
          if(node_sym(node_arg(node,i))==sym_mult) then
             call code_val(coder,coder%vstack(base+(i+1)/2))
             m=m+1
          endif
       enddo
       call make_basic_sp_call(coder,cblock,node,sym_pm_list,m,1)
    endif

    ! Copy lists back down
    j=coder%vtop
    coder%vtop=base
    do i=base2+1,j
       coder%vtop=coder%vtop+1
       coder%vstack(coder%vtop)=coder%vstack(i)
    enddo

  contains

    function check_fn(node) result(fn)
      type(pm_ptr),intent(in):: node
      integer:: fn
      integer:: k
      k=node_sym(node)
      if(k==sym_amp) then
         fn=sym_check_iter_amp
      elseif(k==sym_mult) then
         fn=sym_check_iter_star
      else
         fn=sym_check_iter
      endif
    end function check_fn
    
  end subroutine make_iter_lists

  !============================================================================
  ! Create code to extract variables defined by iter node (node) from named
  ! list variable PM__amp_iter_args, PM__iter_args and PM__star iter args as
  ! appropriate depending on whether iter entry is &x in Y, x in Y or *x in Y
  !===========================================================================
  subroutine extract_iter_lists(coder,cblock,node,may_have_amp,may_have_star)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    logical,intent(in):: may_have_amp,may_have_star
    integer:: i,m,n,sym
    type(pm_ptr)::p,avar
    n=node_numargs(node)
    if(may_have_amp) then
       avar=find_sys_var(coder,node,sym_amp_iter_args)
       m=1
       do i=1,n,2
          p=node_arg(node,i)
          if(node_sym(p)==sym_amp) then
             call make_var(coder,cblock,node,node_num_arg(p,1),var_is_var+var_is_ref)
             call extract_var(coder,cblock,node,pop_code(coder),avar,m)
             m=m+1
          endif
       enddo
    endif
    avar=find_sys_var(coder,node,sym_iter_args)
    m=1
    do i=1,n,2
       p=node_arg(node,i)
       sym=node_sym(p)
       if(sym/=sym_amp.and.sym/=sym_mult) then
          call make_var(coder,cblock,node,int(p%offset),0)
          call extract_var(coder,cblock,node,pop_code(coder),avar,m)
          m=m+1
       endif
    enddo
    if(may_have_star) then
       avar=find_sys_var(coder,node,sym_star_iter_args)
       m=1
       do i=1,n,2
          if(node_sym(node_arg(node,i))==sym_mult) then
             call make_var(coder,cblock,node,node_num_arg(p,1),0)
             call extract_var(coder,cblock,node,pop_code(coder),avar,m)
             m=m+1
          endif
       enddo
    endif
  end subroutine extract_iter_lists

  !======================================================================================
  ! Turns a block (stmtlist) into a procedure
  ! Pushes 3 elements onto vstack
  !    procedure value defining block
  !    list of changed variables
  !    list of accessed variables
  ! Parameter list for block procedure starts with
  !    PM__inouts_a PM__ins_a
  !    - code is added to the start of the procedure body to disaggregate these
  !      parameters from lists back into changed and accessed variables respectively
  ! If iters is  not present
  !    - The remaining block-procedure parameters are defined by (namelist,amps)
  ! If iters is present
  !    -the remaining block-procedure parameters are
  !      [ & PM__amp_iter_args ] PM__iter_args [ PM__star_iter_args ]
  !     and namelist (but not amps) is disregarded
  !     Extra disaggregation code for iter parameters is added inside the block
  !======================================================================================
  subroutine make_block_proc(coder,cblock,node,namelist,amps,rtns,nret,stmtlist,iters,iter_amps,iter_stars)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,namelist,rtns,stmtlist
    integer,intent(in):: amps,nret
    type(pm_ptr),intent(in),optional:: iters
    logical,intent(in),optional:: iter_amps,iter_stars

    type(pm_ptr):: cblock2,cblock3,proc
    integer:: nargs,base,i,partype,restype,flags,vbase
    logical:: varargs
    integer:: save_index,save_ncalls,save_state_base,save_mask
    integer:: name,signo,flags0,args(1)
    character(len=15):: namestr
    
    if(present(iters)) then
       nargs=1+merge(1,0,iter_amps)+merge(1,0,iter_stars)
    else
       nargs=node_numargs(namelist)
    endif
    
    varargs=node_sym(namelist)==sym_dotdotdot
    flags=proccall_is_comm+proccall_is_general

    ! Parameter type
    call push_word(coder,merge(pm_type_new_vtuple,pm_type_new_tuple,varargs))
    call push_word(coder,amps)
    do i=1,nargs+8
       call push_word(coder,0)
    enddo
    call make_type(coder,nargs+10)
    partype=pop_word(coder)

    ! Result type
    call push_word(coder,pm_type_is_undef_result)
    call push_word(coder,nret)
    call make_type(coder,2)
    restype=pop_word(coder)

    ! Create block proc name
    coder%block_id=coder%block_id+1
    namestr='PM__block'//trim(pm_int_as_string(coder%block_id))
    name=pm_name2(coder%context,sym_block,pm_name_entry(coder%context,namestr))

    call make_sys_var(coder,cblock,node,sym_block_proc_a,var_is_shadowed)
    
    ! Create proc object
    call code_num(coder,partype)
    call code_num(coder,restype)
    call code_num(coder,nargs)
    call code_num(coder,nret)
    call code_num(coder,flags)
    call code_num(coder,amps)
    call code_num(coder,name)
    cblock2=make_cblock(coder,cblock,stmtlist,sym_do_stmt)
    call code_num(coder,0)
    call code_num(coder,0)
    coder%id=coder%id+1
    call code_num(coder,coder%id) 
    call code_num(coder,0)
    call code_null(coder)
    call code_null(coder)
    call code_null(coder)
    call code_null(coder)
    call code_null(coder)
    call make_code(coder,node,cnode_is_proc,pr_node_size)
    proc=top_code(coder)
        
    ! Create one-element signature
    call make_code(coder,node,cnode_is_callsig,1)

    args(1)=name
    signo=pm_idict_add(coder%context,coder%sig_cache,&
         args,1,pop_code(coder))
    
    ! Create procedure value type
    call push_word(coder,pm_type_new_proc)
    call push_word(coder,name)
    call push_word(coder,pm_type_new_proc_sig)
    call push_word(coder,sym_dash)
    call push_word(coder,partype)
    call push_word(coder,restype)
    call make_type(coder,4)
    call make_type(coder,3)
   
    call make_const(coder,cblock,node,&
         pm_fast_name(coder%context,name),pop_word(coder))
    call make_sys_call(coder,cblock,node,sym_clone,1,1)
    save_index=coder%index
    save_ncalls=coder%proc_ncalls
    coder%index=0
    coder%proc_ncalls=0
    
    call push_block_scope(coder,cblock2)

    base=coder%top
    vbase=coder%vtop

    save_state_base=coder%state_base
    save_mask=coder%mask
    coder%state_base=coder%top
    
    ! Create state variable parameters
    call make_sys_var(coder,cblock2,node,sym_topology,var_is_param+var_is_shadowed)
    call make_sys_var(coder,cblock2,node,sym_outer,var_is_param+var_is_shadowed)
    call make_sys_var(coder,cblock2,node,sym_region,var_is_param+var_is_shadowed)
    call make_sys_var(coder,cblock2,node,sym_subregion,var_is_param+var_is_shadowed)
    call make_sys_var(coder,cblock2,node,sym_here_in_tile,&
         var_is_param+var_is_shadowed)
    call make_sys_var(coder,cblock2,node,sym_mask,var_is_param+var_is_shadowed)

    ! Create variables for block imports and exports
    call make_sys_var(coder,cblock2,node,&
         sym_block_inouts,var_is_param+var_is_ref+var_is_var+var_is_shadowed)
    call make_sys_var(coder,cblock2,node,&
         sym_block_ins,var_is_param+var_is_shadowed)
 
    ! Remaining parameter variables
    if(present(iters)) then
       flags0=var_is_maybe_not_private
       if(iter_amps) then
          call make_sys_var(coder,cblock2,node,&
               sym_amp_iter_args,flags0+var_is_param+var_is_ref+var_is_var+var_is_shadowed)
       endif
       call make_sys_var(coder,cblock2,node,&
            sym_iter_args,flags0+var_is_param+var_is_shadowed)
       if(iter_stars) then
          call make_sys_var(coder,cblock2,node,&
               sym_star_iter_args,flags0+var_is_param+var_is_shadowed)
       endif
       call make_basic_sp_call(coder,cblock2,node,&
            sym_open,coder%vtop-vbase,0)
    else
       call trav_params(coder,cblock2,namelist,amps,1,8)
    endif
    call code_val(coder,coder%var(base+4))
    cblock3=make_cblock(coder,cblock2,stmtlist,sym_do_stmt)
    coder%lex_scope=coder%lex_scope+1

    if(present(iters)) then
       call extract_iter_lists(coder,cblock3,iters,iter_amps,iter_stars)
    endif
 
    call trav_open_stmt_list(coder,cblock3,node,stmtlist)
   
    call trav_xexpr(coder,cblock3,node,rtns)
    call make_sp_call(coder,cblock3,node,sym_result,nret,0)
    coder%lex_scope=coder%lex_scope-1
    call close_cblock(coder,cblock3)
    
    call extract_block_vars(coder,cblock2,node,coder%var(base+7),.true.)
    call extract_block_vars(coder,cblock2,node,coder%var(base+8),.false.)

    call make_sp_call(coder,cblock2,node,sym_pct,2,0)

    
    call cnode_set_num(proc,pr_max_index,coder%index)
    call cnode_set_num(proc,pr_ncalls,coder%proc_ncalls)
    coder%index=save_index
    coder%proc_ncalls=save_ncalls
    coder%state_base=save_state_base
    coder%mask=save_mask
    
    call close_cblock(coder,cblock2)
   
    ! This also pushes lists of changed and accessed variables
    call pop_block_scope(coder,cblock,node,present(iters))

  contains
    include 'fisnull.inc'
    include 'fname.inc'
  end subroutine make_block_proc

  !===============================================================
  ! Create code to disaggregate variables from list variable avar
  ! Variables are obtained from the access/change list
  ! for current block scope and only included if their change
  ! status (modified=> true) is equal to access
  !===============================================================
  subroutine extract_block_vars(coder,cblock,node,avar,access)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,avar
    logical,intent(in):: access

    type(pm_ptr):: p,var
    integer:: index,i

    index=coder%wstack(coder%block_entry+3)
    p=coder%vstack(index)
    i=1
    do while(.not.pm_fast_isnull(p))
       index=p%data%ptr(p%offset)%offset
       var=coder%var(index)
       if(iand(cnode_get_num(var,var_flags),var_is_changed)/=0.eqv.access) then
          call extract_var(coder,cblock,node,coder%var(index),avar,i)
          i=i+1
       endif
       p=p%data%ptr(p%offset+1)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine extract_block_vars

  !===========================================================
  ! Make code to extract index'th element of list avar to var
  !===========================================================
  subroutine extract_var(coder,cblock,node,var,avar,index)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,var,avar
    integer,intent(in):: index

    call code_val(coder,var)
    call code_val(coder,avar)
    call make_long_const(coder,cblock,node,int(index,pm_ln))
    call make_comm_sys_call(coder,cblock,node,sym_elem_at_index,2,1,&
         aflags=proccall_is_ref+proccall_is_general,assign=.true.)
  end subroutine extract_var

  !========================================================
  ! Push a new block scope record onto wstack and vstack
  !========================================================
  subroutine push_block_scope(coder,cblock)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    integer:: base
    base=coder%wtop+1
    call push_word(coder,coder%block_entry)
    call push_word(coder,coder%top)
    call push_word(coder,coder%lex_scope+1)
    call code_null(coder)
    call push_word(coder,coder%vtop)
    call code_val(coder,cblock)
    coder%block_base=coder%top
    coder%block_entry=base
  end subroutine push_block_scope

  ! ================================================================
  ! Import a variable into a block scope
  ! - creates new variable linked to the old one
  ! - change local symbol table to point to new variable
  ! ================================================================
  recursive subroutine import_to_block_scope(coder,index,var,block_entry)
    type(code_state),intent(inout):: coder
    integer,intent(in):: index,block_entry
    type(pm_ptr),intent(inout):: var
    integer:: var_scope,block_scope,block_links
    if(debug_more_codegen) then
       write(*,*) 'import_to_block_scope',block_entry,&
            trim(pm_name_as_string(coder%context,cnode_get_num(var,var_name)))
    endif
    if(block_entry==0) return
    var_scope=cnode_get_num(var,var_lex_scope)
    block_scope=coder%wstack(block_entry+2)
    block_links=coder%wstack(block_entry+3)
    if(var_scope>=block_scope) return
    call make_var(coder,&
         coder%vstack(block_links+1),&
         pm_null_obj,&
         cnode_get_num(var,var_name),&
         ior(cnode_get_num(var,var_flags),var_is_imported),&
         extra_info=var)
    var=pop_code(coder)
    call cnode_set_num(var,var_lex_scope,coder%wstack(block_entry+2))
    if(debug_more_codegen) then
       write(*,*) 'lex scope now',coder%wstack(block_entry+2)
       write(*,*) 'index now',cnode_get_num(var,var_index)
    endif
    call add_to_change_list(coder,coder%vstack(block_links),&
         pm_fast_tinyint(coder%context,index))
  contains
    include 'fisnull.inc'
    include 'ftiny.inc'
  end subroutine import_to_block_scope

  ! ======================================================
  ! Pop block scope from top of wstack & vstack
  ! - pop any imported variables back to their originals
  !   via their link
  !
  ! Push two list values onto vstack for variables
  ! respectively changed and accessed in the block
  !
  ! Also check for aliasing with call arguments/iterator
  ! (call or iterator must be passed as node)
  ! ======================================================
  subroutine pop_block_scope(coder,cblock,node,iter)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    logical,intent(in):: iter
    type(pm_ptr):: list
    type(pm_ptr)::p,var
    integer:: index,nwrites,nreads,base
    logical:: changed

    ! Check we are aligned with block scope records on vstack and wstack
    if(pm_debug_checks) then
       if(coder%wtop/=coder%block_entry+3) then
          call pm_panic("pop_block_scope: wstack")
       endif
       if(coder%vtop-1/=coder%wstack(coder%block_entry+3)) then
          call pm_panic("pop_block_scope: vstack")
       endif
    endif

    ! Pop the block scope record from wstack and vstack
    list=coder%vstack(coder%vtop-1)
    coder%temp2=list  ! Protect list from the gc
    coder%block_entry=coder%wstack(coder%block_entry)
    coder%block_base=coder%wstack(coder%block_entry+1)
    coder%vtop=coder%vtop-2
    coder%wtop=coder%wtop-4

    base=coder%wtop
    
    ! Now pop each variable
    p=list
    do while(.not.pm_fast_isnull(p))
       index=p%data%ptr(p%offset)%offset
       var=coder%var(index)
       changed=cnode_flags_set(var,var_flags,var_is_changed)
       var=cnode_get(var,var_extra_info)
       ! May need to re-import into current scope
       call import_to_block_scope(coder,index,var,coder%block_entry)
       call access_var(coder,var,changed)
       coder%var(index)=var
       ! Flag changed variables with a -ve index
       if(changed) p%data%ptr(p%offset)%offset=-index
       p=p%data%ptr(p%offset+1)
    enddo

    ! Create list of all changed variables
    p=list
    nwrites=0
    call make_sys_var(coder,cblock,node,sym_block_inouts_a,&
         var_is_shadowed+var_is_var+var_is_ref)
    do while(.not.pm_fast_isnull(p))
       index=p%data%ptr(p%offset)%offset
       ! Changed variables have -ve index
       if(index<0) then
          call code_val(coder,coder%var(-index))
          call push_word(coder,cnode_get_num(coder%var(-index),var_name))
          nwrites=nwrites+1
       endif
       p=p%data%ptr(p%offset+1)
    enddo
    call make_basic_sp_call(coder,cblock,node,sym_pm_list,nwrites,1)

    ! Create a list of all accessed variables
    p=list
    nreads=0
    call make_sys_var(coder,cblock,node,sym_block_ins_a,&
         var_is_shadowed+var_is_var)
    do while(.not.pm_fast_isnull(p))
       index=p%data%ptr(p%offset)%offset
       ! Unchanged (but accessed) variables have a +ve index
       if(index>0) then
          call code_val(coder,coder%var(index))
          call push_word(coder,cnode_get_num(coder%var(index),var_name))
          nreads=nreads+1
       endif
       p=p%data%ptr(p%offset+1)
    enddo
    call make_basic_sp_call(coder,cblock,node,sym_pm_list,nreads,1)

    ! Check for alias with iter or argument list
    if(.not.iter) then
       call check_call_block_alias(coder,cblock,node,base,nreads,nwrites)
    else
       call check_iter_block_alias(coder,cblock,node,base,nreads,nwrites)
    endif
    
    ! Clean up
    coder%wtop=base
    coder%temp2=pm_null_obj
  contains
    include 'fisnull.inc'
  end subroutine  pop_block_scope

  !===================================================================
  ! Check variables used as arguments to call to those 
  ! accessed/modified by block (whose names must be pushed on wstack)
  ! above base (nwrites writes followed by nreads reads)
  !===================================================================
  subroutine check_call_block_alias(coder,cblock,node,base,nreads,nwrites)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: base,nreads,nwrites
    type(pm_ptr):: args,amp,arg
    integer:: i,j,k,name,sym
    args=node_arg(node,2)
    amp=pm_name_val(coder%context,node_num_arg(node,3))
    if(pm_fast_isnull(amp)) then
       do i=1,node_numargs(args)
          arg=node_arg(args,i)
          sym=node_sym(arg)
          if(sym==sym_reference.or.sym==sym_name) then
             name=root_name(arg)
             do j=1,nwrites
                if(coder%wstack(base+i)==name) then
                   call code_error(coder,arg,&
                        'Variable is modified by the block that is also used by an argument: ',&
                        node_num_arg(arg,1))
                endif
             enddo
          endif
       enddo
    else
       k=0
       do i=1,node_numargs(args)
          arg=node_arg(args,i)
          if(amp%data%i(amp%offset+k)==i) then
             name=node_num_arg(arg,1)
             do j=1,nwrites+nreads
                if(coder%wstack(base+i)==name) then
                   if(j<=nwrites) then
                      call code_error(coder,arg,&
                           'Variable is modified by the block that is also modified as an argument: ',&
                           node_num_arg(arg,1))
                   else
                      call code_error(coder,arg,&
                           'Variable is accessed by the block that is also modified as an argument: ',&
                           node_num_arg(arg,1))
                   endif
                endif
             enddo
             k=min(k+1,pm_fast_esize(amp))
          else
             if(node_sym(arg)==sym_amp) then
                name=node_num_arg(arg,1)
                do j=1,nwrites
                   if(coder%wstack(base+i)==name) then
                      call code_error(coder,arg,&
                           'Variable is modified by the block that is also used by an argument: ',&
                           node_num_arg(arg,1))
                   endif
                enddo
             endif
          endif
       enddo
    endif
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
  end subroutine check_call_block_alias


  !===================================================================
  ! Check variables defined in iterator against variables
  ! accessed/modified by block (whose names must be pushed on wstack)
  ! above base (nwrites writes followed by nreads reads)
  !===================================================================
 subroutine check_iter_block_alias(coder,cblock,node,base,nreads,nwrites)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: base,nreads,nwrites
    integer:: i,j,name,sym
    type(pm_ptr):: arg
    do i=1,node_numargs(node),2
       arg=node_arg(node,i+1)
       sym=node_sym(arg)
       if(sym==sym_name.or.sym==sym_reference) then
          name=root_name(arg)
          do j=1,merge(nwrites+nreads,nreads,node_sym(node_arg(node,i))==sym_amp)
             if(coder%wstack(base+j)==name) then
                if(j<=nwrites) then
                   call code_error(coder,arg,'Block modifies variable that is used by iterator: ',&
                        node_num_arg(arg,1))
                else
                   call code_error(coder,arg,'Block uses variable which is modified by iterator: ',&
                        node_num_arg(arg,1))
                endif
             endif
          enddo
       endif
    enddo
  end subroutine check_iter_block_alias
  
  function root_name(arg) result(name)
    type(pm_ptr),intent(in):: arg
    integer:: name
    integer:: sym
    sym=node_sym(arg)
    if(sym==sym_name) then
       name=node_num_arg(arg,1)
    else !! reference
       name=node_num_arg(node_arg(arg,1),1)
    endif
  end function root_name

  
  !================================================
  ! Create a new lexical scope (used to identify
  ! variables defined outside the  statement that
  ! are accessed or modified within it)
  ! Pushes small record on vstack
  !================================================
  function push_lex_scope(coder) result(new_lex_scope)
    type(code_state),intent(inout):: coder
    integer:: new_lex_scope
    call code_num(coder,coder%lex_scope)
    call code_null(coder)
    call code_null(coder)
    new_lex_scope=coder%vtop
  end function  push_lex_scope

  !===========================================
  ! Push change lists of current lexical scope
  ! as a changelist cnode
  !===========================================
  subroutine get_lex_scope(coder,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    call code_val(coder,coder%vstack(coder%lex_scope))
    call code_val(coder,coder%vstack(coder%lex_scope-1))
    call make_code(coder,node,cnode_is_changelist,2)
  end subroutine  get_lex_scope

  !===================================
  ! Exit lexical scope
  ! Pops record off the top of vstack
  !===================================
  subroutine pop_lex_scope(coder)
    type(code_state),intent(inout):: coder
    coder%lex_scope=coder%vstack(coder%vtop-2)%offset
    call drop_code(coder)
    call drop_code(coder)
    call drop_code(coder)
  end subroutine  pop_lex_scope

  !==========================================
  ! Record read or write (if modify is true)
  ! access to a variable
  !===========================================
  subroutine access_var(coder,var,modify)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(inout):: var
    logical,intent(in):: modify
    if(debug_more_codegen) then
       if(coder%block_base>0) write(*,*) 'access var',modify,&
            trim(pm_name_as_string(coder%context,cnode_get_num(var,var_name))),&
            cnode_get_num(var,var_index)
    endif
    if(modify) then
       call cnode_set_flags(var,var_flags,var_is_changed)
    else
       if(cnode_flags_set(var,var_flags,var_is_accessed)) then
          call cnode_set_flags(var,var_flags,var_is_multi_access)
       else
          call cnode_set_flags(var,var_flags,var_is_accessed)
       endif
    endif
    call update_change_lists(coder,var,modify)
  end subroutine access_var
  
  !=============================================
  ! Add var to the change list for all if scopes
  ! that are nested inside the scope in which
  ! the variable was defined
  !=============================================
  subroutine update_change_lists(coder,var,modify)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: var
    logical,intent(in):: modify
    integer:: lex_scope,lex_scope_of_var
    lex_scope=coder%lex_scope
    lex_scope_of_var=cnode_get_num(var,var_lex_scope)
    do while(lex_scope_of_var<lex_scope)
        call add_to_change_list(coder,coder%vstack(lex_scope-merge(1,0,modify)),var)
        lex_scope=coder%vstack(lex_scope-2)%offset
    end do
  end subroutine update_change_lists

  !==============================================
  ! Add var to change list headed by list
  !==============================================
  subroutine add_to_change_list(coder,list,var)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(inout):: list
    type(pm_ptr),intent(in):: var
    type(pm_ptr):: p
    if(.not.pm_fast_isnull(list)) then
       p=list
       do while(.not.pm_fast_isnull(p))
          if(p%data%ptr(p%offset)==var) return
          p=p%data%ptr(p%offset+1)
       enddo
    endif
    p=pm_fast_newnc(coder%context,pm_pointer,2)
    p%data%ptr(p%offset)=var
    p%data%ptr(p%offset+1)=list
    list=p
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
  end subroutine add_to_change_list


  !==============================================
  ! Dump contents of change list to vstack
  ! returning count of #elements pushed
  !==============================================
  subroutine retrieve_change_list(coder,list,count)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: list
    integer,intent(out):: count
    type(pm_ptr):: p
    count=0
    p=list
    do while(.not.pm_fast_isnull(p))
       call code_val(coder,p%data%ptr(p%offset))
       p=p%data%ptr(p%offset+1)
       count=count+1
    enddo
  contains
    include 'fisnull.inc'
  end subroutine retrieve_change_list
  
  !==============================================================
  ! Traverse extended expression: expr [check expr] { where ...}
  !==============================================================
  recursive subroutine trav_xexpr(coder,cblock,exprp,exprn) 
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,exprp,exprn
    integer:: base,xtop
    call trav_subexpr(coder,cblock,exprp,exprn,base,xtop)
    if(base>=0) call hide_vars(coder,base+1,xtop)
  end subroutine trav_xexpr
  
  !==============================================================
  ! Traverse extended expression: expr [check expr] { where ...}
  !==============================================================
  recursive subroutine trav_subexpr(coder,cblock,exprp,exprn,base,top,dtop)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,exprp,exprn
    integer,intent(out):: base,top
    integer,intent(out),optional:: dtop
    type(pm_ptr)::p,ass,arg
    integer:: i,j,wbase,name
    logical:: ok
    p=exprn
    base=-1
    top=-2
    if(pm_fast_isnull(p)) return
    if(node_sym(p)==sym_where) then
       base=coder%top
       do
          ass=node_arg(p,2)
          do i=1,node_numargs(ass)
             call trav_assign_define(coder,cblock,ass,node_arg(ass,i))
          enddo
          p=node_arg(p,1)
          if(node_sym(p)/=sym_where) exit
       enddo
       top=coder%top
       if(present(dtop)) dtop=coder%top
    endif
    if(node_sym(p)==sym_check) then
       call apply_x(p,node_arg(p,1))
       call make_check(coder,cblock,p,0)
    else
       call apply_x(exprp,p)
    endif
  contains
    include 'fisnull.inc'
    include 'fisname.inc'
    include 'fname.inc'
    include 'ftiny.inc'

    recursive subroutine apply_x(nodep,node)
      type(pm_ptr),intent(in):: nodep,node
      type(pm_ptr):: nodei
      integer:: wbase,i,nsym
      wbase=coder%wtop
      if(pm_fast_isnull(node)) return
      select case(node_sym(node))
      case(sym_assign)
         call trav_assign_define(coder,cblock,nodep,node)
      case(sym_assign_list)
         call trav_assign_define_list(coder,cblock,nodep,node)
      case(sym_list)
         call trav_exprlist(coder,cblock,nodep,node)
      case(sym_result)
         call push_word(coder,pm_type_is_tuple)
         call push_word(coder,0)
         do i=1,node_numargs(node),2
            nodei=node_arg(node,i)
            call trav_expr(coder,cblock,node,nodei)
            nsym=node_sym(nodei)
            if(nsym==sym_name.or.nsym==sym_reference) then
               nodei=find_var(coder,root_name(nodei))
               if(.not.pm_fast_isnull(nodei)) then
                  if(cnode_flags_set(nodei,var_flags,var_is_param)) then
                     call make_sys_call_rtn(coder,cblock,node,sym_clone,1,1)
                  endif
               endif
            endif
            nodei=node_arg(node,i+1)
            if(.not.pm_fast_isnull(nodei)) then
               call trav_cast(coder,cblock,node,nodei,sym_const)
               call trav_type(coder,node,nodei)
            else
               call push_word(coder,0)
            endif
         enddo
         call make_type(coder,node_numargs(node)/2+2)
         return
      case(sym_do)
         call trav_call(coder,cblock,node,node_arg(node,1),0,.true.)
      case(sym_test)
         call make_check(coder,cblock,node,base)
      case default
         call trav_top_expr(coder,cblock,nodep,node)
      end select
      if(pm_debug_checks) then
         if(coder%wtop/=wbase) then
            write(*,*) coder%wtop,wbase
            call dump_parse_tree(coder%context,6,node,2)
            call pm_panic('xexpr wstack mismatch')
         endif
      endif
    end subroutine apply_x

  end subroutine trav_subexpr

  !========================================================
  ! Compile check
  !========================================================
  recursive subroutine make_check(coder,cblock,p,base)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,p
    integer,intent(in):: base
    type(pm_ptr):: mess,cblock2,cblock3
    integer:: i
    do i=2,node_numargs(p),2
       mess=node_arg(p,i)
       if(pm_fast_isnull(mess)) then
          call make_literal_const(coder,cblock,p,coder%check_mess)
          call code_null(coder)
       elseif(node_sym(mess)==sym_string) then
          call make_literal_const(coder,cblock,p,node_num_arg(mess,1))
          call code_null(coder)
       else
          call make_sys_var(coder,cblock,p,sym_check,var_is_shadowed)
          cblock2=make_cblock(coder,cblock,p,sym_check)
          call trav_expr(coder,cblock2,p,mess)
          call init_var(coder,cblock2,p,&
               coder%vstack(coder%vtop-2))
          call close_cblock(coder,cblock2)
       endif
       call make_sys_var(coder,cblock,p,sym_query,var_is_shadowed)
       cblock3=make_cblock(coder,cblock,p,sym_check)
       call trav_expr(coder,cblock3,p,node_arg(p,i+1))
       call init_var(coder,cblock3,p,coder%vstack(coder%vtop-2))
       call close_cblock(coder,cblock3)
       call make_sp_call(coder,cblock,p,sym_check,4,0)
    end do
  contains
    include 'fisnull.inc'
  end subroutine make_check

  !========================================================
  ! Code a check if value is invariant
  ! If sym is present then also check for correct par_state
  !========================================================
  subroutine code_check_invar(coder,cblock,node,val,sym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,val
    integer,intent(in),optional:: sym
    if(present(sym)) then
       if(coder%par_state==par_state_none) then
          call code_error(coder,node,'Cannot have "'//trim(sym_names(sym))//'"'//&
               ' outside of a parallel context')
       elseif(coder%par_state>par_state_masked) then
          call code_error(coder,node,'Cannot have "'//trim(sym_names(sym))//'"'//&
               ' inside this conditional context')
       endif
    endif
    call code_val(coder,val)
    call make_sp_call(coder,cblock,node,sym_invar,1,0,flags=call_is_no_touch)
  end subroutine code_check_invar

  !**************************************************
  ! PARALLEL STATEMENTS
  !**************************************************
  

!!$  !========================================================
!!$  ! Traverse par { }
!!$  !========================================================
!!$  recursive subroutine trav_par_stmt(coder,cblock,pnode,node)
!!$    type(code_state),intent(inout):: coder
!!$    type(pm_ptr),intent(in):: cblock,pnode,node
!!$    type(pm_ptr):: let,clause,cblock_main,cblock_pre,cblock_post
!!$    type(pm_ptr):: vlist,save_loop_cblock
!!$    integer:: i,j,k,iter,istart,vstart,base,rbase,m
!!$    integer:: save_par_base,save_over_base,save_par_state
!!$    integer:: slot1,slot2
!!$    integer:: name,flags
!!$    
!!$    vstart=coder%vtop
!!$    save_loop_cblock=coder%loop_cblock
!!$    save_par_base=coder%par_base
!!$    save_over_base=coder%over_base
!!$    save_par_state=coder%par_state
!!$    
!!$    base=coder%top
!!$
!!$    if(node_numargs(node)==4) then
!$       call code_error(coder,node,'"par" statement has only one branch')
!!$       coder%vtop=vstart
!!$       return
!!$    endif
!!$    
!!$    ! Variable sym_for
!!$    call make_long_const(coder,cblock,node,&
!!$         int((node_numargs(node)-2)/2,pm_ln))
!!$    call make_sys_call_rtn(coder,cblock,node,sym_array,1,1)
!!$    call define_sys_var(coder,cblock,node,sym_for,var_is_shadowed)
!!$
!!$    ! Partition the domain across processors
!!$    iter=code_par_scope_start(coder,cblock,node,coder%var(coder%top),&
!!$         node_arg(node,1),cblock_main,cblock_pre,sym_also,.false.)
!!$
!!$    slot1=coder%index
!!$    
!!$    do i=3,node_numargs(node),2
!!$       call make_long_const(coder,cblock_main,node,int((i-2)/2,pm_ln))
!!$       call make_definition(coder,cblock_main,node,node_arg(node,i),0)
!!$    enddo
!!$
!!$    coder%par_state=par_state_for
!!$    
!!$    ! statements before any branch
!!$     call trav_open_stmt_list(coder,cblock_main,node,node_arg(node,2))
!!$
!!$    coder%par_state=par_state_par
!!$    
!!$    ! branches
!!$    call branch(cblock_main,3)
!!$
!!$    slot2=coder%index
!!$    
!!$    ! Build parallel statement call
!!$    call code_par_scope_end(coder,iter,node,cblock,cblock_main,&
!!$         cblock_pre,&
!!$         save_par_base,slot1,slot2,.false.,.false.)
!!$    
!!$    coder%vtop=vstart
!!$    coder%loop_cblock=save_loop_cblock
!!$    coder%par_base=save_par_base
!!$    coder%over_base=save_over_base
!!$    call pop_vars_to(coder,base)
!!$  contains
!!$    include 'fisnull.inc'
!!$    include 'ftiny.inc'
!!$
!!$    recursive subroutine branch(cblock,i)
!!$      type(pm_ptr),intent(in):: cblock
!!$      integer,intent(in):: i
!!$      type(pm_ptr):: cblock4,cblock5
!!$      type(pm_ptr):: prc_test_var
!!$      integer:: n
!!$      
!!$      ! Is this branch running on this processor?
!!$      call make_temp_var(coder,cblock,node)
!!$      prc_test_var=top_code(coder)
!!$      call make_long_const(coder,cblock,node,int((i-2)/2,pm_ln))
!!$      call make_sys_call_rtn(coder,cblock,node,sym_tuple,1,1)
!!$      call code_val(coder,coder%var(iter+lv_here))
!!$      call make_sys_call(coder,cblock,node,sym_eq,2,1)
!!$      
!!$      if(i/=cnode_numargs(node)-1) then   
!!$         ! If statment (if running_here then ... endif)
!!$         coder%lex_scope=push_lex_scope(coder)
!!$         call code_val(coder,prc_test_var)
!!$         cblock4=make_cblock(coder,cblock,node,sym_also)
!!$         ! task clause
!!$         call trav_open_stmt_list(coder,cblock4,node,node_arg(node,i+1))
!!$         call close_cblock(coder,cblock4)
!!$         cblock5=make_cblock(coder,cblock,node,sym_also)
!!$         ! remaining task clauses
!!$         call branch(cblock5,i+2)
!!$         call close_cblock(coder,cblock5)
!!$         call get_lex_scope(coder,node)
!!$         call make_sp_call(coder,cblock,node,sym_if,4,0)
!!$         call pop_lex_scope(coder)
!!$      else
!!$         ! task clause
!!$         call trav_open_stmt_list(coder,cblock,node,node_arg(node,i+1))
!!$      endif
!!$     
!!$    end subroutine branch
!!$    
!!$  end subroutine trav_par_stmt


  !*****************************************************
  ! ASSIGNMENTS AND VARIABLE DEFINITIONS
  !*****************************************************

  !========================================================
  ! Traverse single assignment or var/const definition
  !========================================================
  recursive subroutine trav_assign_define(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    type(pm_ptr):: lhs,rhs
    integer:: n,sym,base
    base=coder%vtop
    lhs=node_arg(node,1)
    rhs=node_arg(node,2)
    sym=node_sym(lhs)
    n=lhs_size(lhs)
    call trav_rhs(coder,cblock,node,rhs,n)
    call trav_lhs(coder,cblock,node,lhs,rhs)
    coder%vtop=base
  end subroutine trav_assign_define

  !========================================================
  ! Number of elements in LHS node
  !========================================================
  recursive function lhs_size(lhs) result(n)
    type(pm_ptr),intent(in):: lhs
    integer:: n
    integer:: sym,i
    n=node_numargs(lhs)
    sym=node_sym(lhs)
    if(sym==sym_assign_list) then
       n=0
       do i=1,node_numargs(lhs)
          n=n+lhs_size(node_arg(lhs,i))
       enddo
    elseif(sym/=sym_assign.and.sym/=sym_where) then
       n=n-2
    endif
  end function lhs_size

  !========================================================
  ! Traverse multiple assignments, var/const definitions
  !========================================================
  recursive subroutine trav_assign_define_list(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    type(pm_ptr):: assn,lhs,rhs
    integer:: i,n,sym,base
    base=coder%vtop
    do i=1,node_numargs(node)
       assn=node_arg(node,i)
       lhs=node_arg(assn,1)
       sym=node_sym(lhs)
       n=lhs_size(lhs)
       rhs=node_arg(assn,2)
       call trav_rhs(coder,cblock,node,rhs,n)
    enddo
    do i=node_numargs(node),1,-1
       assn=node_arg(node,i)
       lhs=node_arg(assn,1)
       rhs=node_arg(assn,2)
       call trav_lhs(coder,cblock,node,lhs,rhs)
    enddo
    coder%vtop=base
  end subroutine trav_assign_define_list
  
  !========================================================
  ! Traverse left hand side of assignment or definition
  ! Computes these in *reverse* order assuming RHS has
  ! stacked them one after the other.
  !========================================================
  recursive subroutine trav_lhs(coder,cblock,node,lhs,rhs)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,lhs,rhs
    integer:: i,n,sym
    type(pm_ptr):: lhs_val,rhs_val
    n=node_numargs(lhs)
    sym=node_sym(lhs)
    select case(sym)
    case(sym_var,sym_const)
       do i=n-2,1,-1
          lhs_val=node_arg(lhs,i)
          if(node_sym(lhs_val)==sym_dotdotdot) then
             call make_definition(coder,cblock,lhs,node_arg(lhs_val,1),&
                  merge(0,var_is_var,sym==sym_const),node_arg(lhs,n),&
                  mode=node_num_arg(lhs,n-1),dotdotdot=.true.)
          else
             call make_definition(coder,cblock,lhs,lhs_val,&
                  merge(0,var_is_var,sym==sym_const),node_arg(lhs,n),&
                  mode=node_num_arg(lhs,n-1))
          endif
       enddo
    case(sym_where)
       do i=n,1,-1
          call make_definition(coder,cblock,lhs,node_arg(lhs,i),var_is_where)
       enddo
    case(sym_assign)
       if(node_sym(rhs)==sym_assign) then
          rhs_val=node_arg(rhs,1)
       else
          rhs_val=rhs
       endif
       do i=n,1,-1
          call trav_single_lhs(coder,cblock,lhs,node_arg(lhs,i),rhs_val)
       enddo
    case(sym_assign_list)
       do i=n,1,-1
          call trav_lhs(coder,cblock,lhs,node_arg(lhs,i),rhs)
       enddo
    end select
  end subroutine trav_lhs

  !=============================================================
  ! Traverse single element of the left hand side of assignment
  ! or definition (simple "=", not var or const)
  !=============================================================
  subroutine trav_single_lhs(coder,cblock,node,lhs,rhs)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,lhs,rhs
    type(pm_ptr):: var
    integer:: name
    if(pm_fast_isname(lhs)) then
       name=lhs%offset
    elseif(node_sym(lhs)==sym_name) then
       name=node_num_arg(lhs,1)
    elseif(node_sym(lhs)==sym_lt) then
       call make_op_assignment_noalias(coder,cblock,lhs,node_arg(lhs,1),node_arg(lhs,2))
       return
    else
       call make_assignment(coder,cblock,node,lhs,rhs)
       return
    endif
    var=find_var(coder,name)
    if(pm_fast_isnull(var)) then
       call make_definition(coder,cblock,node,lhs,0)
    else
       call make_assignment(coder,cblock,node,lhs,rhs,var)
    endif
  contains
    include 'fisname.inc'
    include 'fisnull.inc'
  end subroutine trav_single_lhs

  !========================================================
  ! Traverse right hand side of assignment or definition
  ! which is required to produce n items
  !========================================================
  subroutine trav_rhs(coder,cblock,node,rhs,n)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,rhs
    integer,intent(in):: n
    integer:: i,rsym,base
    rsym=node_sym(rhs)
    base=coder%vtop
    if(rsym==sym_assign) then
       call trav_top_expr(coder,cblock,node,node_arg(rhs,1))
       do i=2,n
          call dup_expr(coder,top_code(coder))
       enddo
    elseif(rsym==sym_do_stmt) then
       do i=1,n
          call make_temp_var(coder,cblock,node)
       enddo
       call make_block_proc(coder,cblock,node_arg(rhs,3),&
            node_arg(rhs,1),node_num_arg(rhs,2),&
            node_arg(rhs,5),node_numargs(node_arg(rhs,5)),&
            node_arg(rhs,4))
       do i=1,n
          call code_val(coder,coder%vstack(base+i))
       enddo
       call trav_call(coder,cblock,node,node_arg(rhs,3),n,.true.)
    elseif(n>1) then
       do i=1,n
          call make_temp_var(coder,cblock,node)
       enddo
       do i=1,n
          call code_val(coder,coder%vstack(base+i))
       enddo
       call trav_call(coder,cblock,node,rhs,n,.true.)
    else
       call trav_top_expr(coder,cblock,node,rhs)
    endif
  end subroutine trav_rhs

  !========================================================
  ! Assign expression on top of stack to lhs in node
  ! Need to also provide rhs node (which has already been
  ! traversed) to enable alias checking
  !========================================================
  recursive subroutine make_assignment(coder,cblock,pnode,lhs,rhs,avar)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,pnode,lhs,rhs
    type(pm_ptr),intent(in),optional:: avar
    integer:: rsym,lsym,rbase,lbase,i,lex_scope
    logical:: ok
    type(pm_ptr):: rname,lname
    rsym=node_sym(rhs)
    lsym=node_sym(lhs)
!!$    if(rsym==sym_sub.and.lsym==sym_sub) then
!!$       rbase=coder%vtop
!!$       ok=get_ref_pattern(coder,rhs,rname)
!!$       lbase=coder%vtop
!!$       ok=get_ref_pattern(coder,lhs,lname)
!!$       if(lname%offset/=rname%offset) then
!!$          coder%vtop=rbase
!!$          call make_assignment_noalias(coder,cblock,pnode,lhs,avar)
!!$       elseif(match_ref_names(coder,cblock,pnode,rbase,lbase)) then
!!$          coder%vtop=rbase
!!$          call make_assignment_noalias(coder,cblock,pnode,lhs,avar)
!!$       else
!!$          ! Code if <aliased> : <aliased_assign> else: <no_alias_assign>
!!$          lex_scope=push_lex_scope(coder)
!!$          call match_ref_pattern(coder,cblock,pnode,rbase,lbase,test=.true.)
!!$          coder%vstack(rbase+1)=coder%vstack(coder%vtop)
!!$          coder%vtop=rbase+1
!!$          coder%lex_scope=lex_scope
!!$          cblock1=make_cblock(coder,cblock,pnode,sym_if)
!!$          call code_val(coder,coder%vstack(rbase))
!!$          call make_assignment_noalias(coder,cblock1,pnode,lhs,avar,alias=.true.)
!!$          call close_cblock(coder,cblock1)
!!$          cblock2=make_cblock(coder,cblock,pnode,sym_if)
!!$          call code_val(coder,coder%vstack(rbase))
!!$          call make_assignment_noalias(coder,cblock2,pnode,lhs,avar)
!!$          call close_cblock(coder,cblock2)
!!$          call get_lex_scope(coder,pnode)
!!$          call make_sp_call(coder,cblock,pnode,sym_if,4,0)
!!$          call pop_lex_scope(coder)
!!$       endif
!!$    else
       call make_assignment_noalias(coder,cblock,pnode,lhs,avar)
!!$    endif
  end subroutine make_assignment
  
  !============================================================
  ! Assign expression on top of stack to lhs in node
  ! LHS must not alias RHS
  ! (unless alias is present in which case LHS must alias RHS)
  !============================================================
  recursive subroutine make_assignment_noalias(coder,cblock,pnode,node,avar,alias)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    type(pm_ptr),intent(in),optional:: avar
    logical,intent(in),optional:: alias
    integer:: sym,outmode
    if(present(avar)) then
       call trav_ref_to_var(coder,cblock,pnode,0,.true.,avar)
       call assign_call(pnode,&
            cnode_flags_clear(top_code(coder),var_flags,var_is_ref))
    elseif(node_sym(node)==sym_underscore) then
       call drop_code(coder)
       return
    elseif(pm_fast_isname(node)) then
       call trav_ref_to_var(coder,cblock,pnode,int(node%offset),.true.)
       call assign_call(pnode,&
            cnode_flags_clear(top_code(coder),var_flags,var_is_ref))
    else
       sym=node_sym(node)
       select case(sym)
       case(sym_reference)
          call trav_reference(coder,cblock,pnode,node,.true.,.true.,.false.)
          call assign_call(node,.false.)
       case(sym_name)
          call trav_ref_to_var(coder,cblock,node,node_num_arg(node,1),.true.)
          call assign_call(node,&
               cnode_flags_clear(top_code(coder),var_flags,var_is_ref))
       case default
          !write(*,*) sym_names(sym)
          call code_error(coder,pnode,&
               'Cannot assign to expression')
          call drop_code(coder)
       end select
    endif
  contains
    include 'fisname.inc'
    include 'fisnull.inc'
    include 'ftiny.inc'

    subroutine assign_call(pnode,simple)
      type(pm_ptr),intent(in):: pnode
      logical,intent(in):: simple
      integer:: call_sym
      if(simple) then
         call_sym=merge(sym_assign_or_init,sym_init_const,&
              cnode_flags_set(top_code(coder),var_flags,var_is_var))
         call dup_code(coder)
         call swap_code_2_1(coder)
         call make_assign_call(coder,cblock,pnode,call_sym,2,1,&
              aflags=call_takes_uninit+call_is_assign_call)
      else
         call swap_code(coder)
         call make_assign_call(coder,cblock,pnode,&
              merge(sym_aliased_assign,sym_pm_assign,present(alias)),&
              2,0,aflags=call_is_assign_call)
      endif
    end subroutine assign_call
    
  end subroutine make_assignment_noalias

  !========================================================
  ! Assign expression on top of stack to lhs in node
  !========================================================
  recursive subroutine make_op_assignment_noalias(coder,cblock,pnode,node,op)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node,op
    integer:: n
    if(node_sym(node)==sym_underscore) then
       call drop_code(coder)
       return
    endif
    call trav_reference(coder,cblock,pnode,node,.true.,.true.,.false.)
    call swap_code(coder)
    call trav_expr(coder,cblock,pnode,op)
    call make_assign_call(coder,cblock,pnode,sym_pm_assign,3,0)
  end subroutine make_op_assignment_noalias

  !===================================================================
  ! Use expression on top of stack to create new variable or constant
  !===================================================================
  recursive subroutine make_definition(coder,cblock,node,vname,flags,vtype,mode,dotdotdot)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,vname
    integer,intent(in):: flags
    type(pm_ptr),intent(in),optional:: vtype
    integer,intent(in),optional:: mode
    logical,intent(in),optional:: dotdotdot
    type(pm_ptr):: pnode,var
    integer:: name,has_type
    integer:: vcall,vflags
    logical:: has_mode

    if(node_sym(vname)==sym_name.or.pm_fast_isname(vname)) then
       if(pm_fast_isname(vname)) then
          name=vname%offset
          pnode=node
       else
          name=node_num_arg(vname,1)
          pnode=vname
       endif
       has_mode=present(mode)
       if(has_mode) has_mode=mode>0
       vflags=flags
       if(has_mode) vflags=ior(vflags,var_is_maybe_not_private)
       if(iand(flags,var_is_var)==0) then
          var=top_code(coder)
          if(cnode_get_kind(var)==cnode_is_var) then
             if(cnode_flags_set(var,var_flags,var_is_maybe_not_private)) then
                vflags=ior(vflags,var_is_maybe_not_private)
             endif
          endif
       endif
       if(present(dotdotdot)) then
          var=find_var(coder,name)
          if(pm_fast_isnull(var)) then
             call code_error(coder,vname,&
                  'Variable being intialised with "..." has not been previously defined')
             call make_temp_var(coder,cblock,node)
             var=top_code(coder)
          else
             if(cnode_flags_set(var,var_flags,var_is_var).neqv.iand(flags,var_is_var)/=0) then
                if(iand(flags,var_is_var)/=0) then
                   call code_error(coder,node,&
                        '"let..." cannot be intialised using "var ...="')
                else
                   call code_error(coder,node,&
                        '"var..." cannot be intialised using "let ...="')
                endif
             endif
             call code_val(coder,var)
          endif
       else
          call make_var(coder,cblock,pnode,name,vflags)
          var=top_code(coder)
       endif
       call swap_code(coder)
       has_type=0
       if(present(vtype)) then
          if(.not.pm_fast_isnull(vtype)) then
             has_type=1
             call trav_type_constraint(coder,node,vtype)
             call make_const(coder,cblock,node,&
                  pm_fast_tinyint(coder%context,pop_word(coder)))
             call make_sp_call_rtn(coder,cblock,node,sym_type_val,1,1)
          endif
       endif
       vcall=merge(sym_make_var,sym_make_const,iand(flags,var_is_var)/=0)
       if(present(dotdotdot)) then
          vcall=merge(sym_init_var,sym_init_const,iand(flags,var_is_var)/=0)
          call code_val(coder,var)
          call make_sp_call_rtn(coder,cblock,pnode,sym_dotdotdot,1,1)
          call update_change_lists(coder,var,.true.)
          has_type=1
       endif
       if(coder%par_state>par_state_none) then
          ! Assumes cannot have "const" in a mode statement
          if(has_mode) vcall=vcall+mode-sym_chan+1
          call make_comm_sys_call(coder,cblock,pnode,vcall,1+has_type,1)
       else
          if(has_mode) call code_error(coder,node,'Cannot have a "'//&
               trim(sym_names(mode))//' var" definition outside of a parallel context')
          call make_sys_call(coder,cblock,pnode,vcall,1+has_type,1)
       endif
    elseif(node_sym(vname)==sym_underscore) then
       call drop_code(coder)
    else
       call code_error(coder,node,&
            'Left hand side of definition must be variable name')
    endif

  contains
    include 'fisname.inc'
    include 'fvkind.inc'
    include 'fisnull.inc'
    include 'ftiny.inc'
  end subroutine make_definition
  
  !========================================================
  ! Reference to a variable
  !========================================================
  subroutine trav_ref_to_var(coder,cblock,pnode,name,islhs,avar)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode
    integer,intent(in):: name
    logical,intent(in):: islhs
    type(pm_ptr),intent(in),optional:: avar
    type(pm_ptr):: var
    integer:: flags,var_index
    if(present(avar)) then
       var=avar
    else
       var=find_var_and_entry(coder,name,var_index)
       if(pm_fast_isnull(var)) then
          call code_error(coder,pnode,&
               'Variable or constant has not been defined: ',name)
          call make_temp_var(coder,cblock,pnode)
          return
       endif
    endif

    if(islhs) then
       if(cnode_get_kind(var)==cnode_is_var) then
          flags=cnode_get_num(var,var_flags)
          if(iand(flags,var_is_var)==0.and..false.) then
             call code_error(coder,pnode,&
                  'Cannot assign to constant: ',name)
          else
             call access_var(coder,var,.true.)
          endif
       else
          call code_error(coder,pnode,&
               'Cannot assign to constant: ',name)
       endif
    else
       if(cnode_get_kind(var)==cnode_is_var) then
          call access_var(coder,var,.false.)
       endif
    endif
    call code_val(coder,var)
  contains
    include 'fisnull.inc'
  end subroutine trav_ref_to_var

  !========================================================
  ! Create a new system variable from expr on top of stack
  !========================================================
  subroutine define_sys_var(coder,cblock,node,name,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: name
    integer,intent(in):: flags
    type(pm_ptr):: var
    call make_sys_var(coder,cblock,node,name,flags)
    var=top_code(coder)
    call swap_code(coder)
    call make_comm_sys_call(coder,cblock,node,&
         merge(sym_make_var,sym_make_const,iand(flags,var_is_var)/=0),1,1)
  end subroutine define_sys_var

  !========================================================
  ! Initialise system variable from expr on top of stack
  !========================================================
  subroutine init_var(coder,cblock,node,var)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,var
    call code_val(coder,var)
    call swap_code(coder)
    call make_comm_sys_call(coder,cblock,node,&
         merge(sym_make_var,sym_make_const,cnode_flags_set(var,var_flags,var_is_var)),&
         1,1)
  end subroutine init_var

  !========================================================
  ! Assign top of stack to a given variable
  !========================================================
  subroutine make_var_assignment(coder,cblock,node,var,aflags)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node
    type(pm_ptr),intent(in)::var
    integer,intent(in),optional:: aflags
    type(pm_ptr):: v
    integer:: flags
    flags=call_ignore_rules
    if(present(aflags)) then
       flags=ior(flags,aflags)
    endif
    call code_val(coder,var)
    v=var
    call swap_code(coder)
    if(cnode_flags_set(v,var_flags,var_is_ref)) then
       call make_assign_call(coder,cblock,node,sym_set_ref,2,0,aflags=flags)
    else
       call make_assign_call(coder,cblock,node,sym_pm_assign,2,0,aflags=flags)
    endif
    call access_var(coder,v,.true.)
  end subroutine make_var_assignment


  !***************************************************
  ! EXPRESSIONS
  !***************************************************

  
  !========================================================
  ! Traverse expression list
  !========================================================
  recursive subroutine trav_exprlist(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: i
    do i=1,node_numargs(node)
       call trav_top_expr(coder,cblock,node,node_arg(node,i))
    enddo
  end subroutine trav_exprlist

  !=============================================================
  ! Traverse top-level expression
  ! - may consist of a call with "&" args
  !=============================================================
  recursive subroutine trav_top_expr(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    if(node_sym(node)==sym_open) then
       call make_temp_var(coder,cblock,pnode)
       call dup_code(coder)
       call trav_call(coder,cblock,pnode,node,1,.true.)
    else
       call trav_expr(coder,cblock,pnode,node)
    endif
  contains
    include 'fname.inc'
  end subroutine trav_top_expr

  !================================================================
  ! Traverse closed expression
  ! - no access to any variable/constant outside of the expression
  !================================================================
  recursive subroutine trav_closed_expr(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: proc_base
    proc_base=coder%proc_base
    coder%proc_base=coder%top
    call trav_expr(coder,cblock,pnode,node)
    coder%proc_base=proc_base
  end subroutine trav_closed_expr

  !========================================================
  ! Traverse expression
  !========================================================
  recursive subroutine trav_expr(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: sym,i,n,base,outmode
    logical:: save_fixed
    type(pm_ptr):: list,p,q
    integer:: loop_flags
    
    sym=node_sym(node)
    loop_flags=0
    select case(sym)
    case(sym_true)
       if(coder%fixed) then
          call make_const(coder,cblock,pnode,coder%true,coder%true_fix)
       else
          call make_const(coder,cblock,pnode,coder%true)
       endif
    case(sym_false)
       if(coder%fixed) then
          call make_const(coder,cblock,pnode,coder%false,coder%false_fix)
       else
          call make_const(coder,cblock,pnode,coder%false)
       endif
    case(sym_null) 
       call make_const(coder,cblock,pnode,pm_null_obj,int(pm_null))
    case(sym_dotdotdot,sym_name,sym_use)
       call trav_name(coder,cblock,node,sym,node_num_arg(node,1))
    case(sym_proc)
       call proc_const(coder,cblock,pnode,node)
    case(sym_param)
       if(node_numargs(node)==2) then
          p=find_param(coder,cblock,node,node_num_arg(node,1),&
               node_num_arg(node,2))
          if(pm_fast_isnull(p)) then
             call code_error(coder,node,'Cannot find parameter: ',&
                  node_num_arg(node,1))
             call make_temp_var(coder,cblock,node)
          else
             call code_val(coder,p)
          endif
       else
          p=find_param(coder,cblock,node,node_num_arg(node,1))
          if(pm_fast_isnull(p)) then
             call code_error(coder,node,&
                  'Cannot find parameter: ',node_num_arg(node,1))
             call make_temp_var(coder,cblock,node)
          else
             call code_val(coder,p)
          endif
       endif
    case(sym_underscore)
       call name_const(node,sym_stretch_dim)
       return
    case(sym_unique)
       call name_const(node,node_num_arg(node,1))
       return
    case(sym_fix,sym_literal)
       save_fixed=coder%fixed
       coder%fixed=.true.
       call trav_expr(coder,cblock,node,node_arg(node,1))
       coder%fixed=save_fixed
       call make_sp_call_rtn(coder,cblock,node,sym,1,1)
    case(sym_present)
       i=node_num_arg(node,1)
       i=find_var_entry(coder,i,coder%proc_base)
       if(i==0) then
          call code_error(coder,node,'Object undefined in "present": ',i)
          call make_temp_var(coder,cblock,node)
       else
          q=coder%var(i)
          if(cnode_flags_set(q,var_flags,var_is_key)) then
             call code_val(coder,cnode_get(q,var_extra_info))
             call make_sp_call_rtn(coder,cblock,node,sym_present,1,1)
          else
             call code_error(coder,node,&
                     '"present" applied to an object that is not a keyword argument: ',i)
             call make_temp_var(coder,cblock,node)
          endif
       endif
    case(first_operator:first_non_idx_operator-1)
       n=node_numargs(node)
       do i=1,n
          call trav_expr(coder,cblock,&
               node,node_arg(node,i))
       enddo
       if(check_args_for_chan(n)) then
          call make_comm_sys_call_rtn(coder,cblock,node,&
               sym,n,1)
       else
          call make_sys_call_rtn(coder,cblock,node,&
               sym,n,1)
       endif
    case(first_non_idx_operator:last_operator)
       n=node_numargs(node)
       do i=1,n
          call trav_expr(coder,cblock,&
               node,node_arg(node,i))
       enddo
       call make_sys_call_rtn(coder,cblock,node,&
            sym,n,1)
    case(sym_as)
       call trav_expr(coder,cblock,&
            node,node_arg(node,1))
       call trav_expr(coder,cblock,&
            node,node_arg(node,2))
       call make_sys_call_rtn(coder,cblock,node,&
            sym,2,1,aflags=call_takes_uninit+call_converts_uninit)
    case(sym_pm_list)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       do i=1,node_numargs(node)
          call trav_expr(coder,cblock,&
               node,node_arg(node,i))
       enddo
       call make_sp_call(coder,cblock,node,&
            sym_pm_list,node_numargs(node),1)
    case(sym_if_expr)
       do i=1,node_numargs(node)
          call trav_expr(coder,cblock,&
               node,node_arg(node,i))
       enddo
       call make_sys_call_rtn(coder,cblock,node,&
            sym,node_numargs(node),1)
    case(sym_switch_expr)
       n=node_numargs(node)
       do i=2,n-2
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          if(i==2) then
             call trav_expr(coder,cblock,node,node_arg(node,1))
             base=coder%vtop
          else
             call dup_expr(coder,coder%vstack(base))
          endif
          p=node_arg(node,i)
          if(node_sym(p)==sym_dotdot) then
             call make_temp_var(coder,cblock,node)
             call dup_code(coder)
             call trav_expr(coder,cblock,node,node_arg(p,1))
             call trav_expr(coder,cblock,node,node_arg(p,2))
             call make_sys_call(coder,cblock,node,sym_case_range,2,1)
          else
             call trav_expr(coder,cblock,node,p)
          endif
          call make_sys_call(coder,cblock,node,sym_checkcase,2,1)
          call trav_expr(coder,cblock,node,node_arg(node,i+1))
       enddo
       call trav_expr(coder,cblock,node,node_arg(node,n))
       do i=2,n-2
          call make_sys_call(coder,cblock,node,sym_if_expr,3,1)
       enddo
    case(sym_uhash,sym_ustar)
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call make_comm_sys_call_rtn(coder,cblock,node,&
            merge(sym_hash,sym_mult,sym==sym_uhash),1,1)
    case(sym_lt)
       call trav_expr(coder,cblock,node,node_arg(node,2))
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call make_sys_call_rtn(coder,cblock,node,sym_gt,2,1)
    case(sym_le)
       call trav_expr(coder,cblock,node,node_arg(node,2))
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call make_sys_call_rtn(coder,cblock,node,sym_ge,2,1)
    case(sym_pm_dref:sym_pm_ref)
       do i=1,node_numargs(node)
          call trav_expr(coder,cblock,node,node_arg(node,i))
       enddo
       call make_sp_call_rtn(coder,cblock,node,sym,node_numargs(node),1)
    case(sym_pm_each_index)
       call trav_pm_each_index(coder,cblock,pnode,node,.true.)
    case(sym_reference)
       call trav_reference(coder,cblock,pnode,node,.false.,.true.,.false.)
    case(sym_open)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call trav_call(coder,cblock,pnode,node,1,.false.)
    case(sym_pval,sym_pval_as)
       call trav_expr(coder,cblock,node,node_arg(node,2))
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call make_sp_call_rtn(coder,cblock,node,sym,2,1)
    case(sym_type_val)
       call trav_type_constraint(coder,node,node_arg(node,1))
       call make_const(coder,cblock,node,&
            pm_fast_tinyint(coder%context,pop_word(coder)))
       call make_sp_call_rtn(coder,cblock,node,sym_type_val,1,1)
    case(sym_array_former,sym_matrix_former)
       i=node_get_num(node,node_args+2)
       call array_span(node_get_num(node,node_args+3),node_get_num(node,node_args+1))
       if(i/=1) then
          call array_span(node_get_num(node,node_args+4),i)
          call make_sys_call_rtn(coder,cblock,node,sym_array,2,1)
       else
          call make_sys_call_rtn(coder,cblock,node,sym_array,1,1)
       endif
       list=node_arg(node,1)
       call trav_expr(coder,cblock,list,node_arg(list,1))
       call swap_code(coder)
       call make_sys_call_rtn(coder,cblock,list,sym_do_dim,2,1)
       if(sym==sym_matrix_former) then
          call make_sys_call_rtn(coder,cblock,node,sym_matrix_former,1,1)
       endif
       do i=2,node_numargs(list)
          call dup_code(coder)
          call make_long_const(coder,cblock,node,int(i-1,pm_ln))
          call trav_expr(coder,cblock,list,node_arg(list,i))
          call make_assign_call(coder,cblock,list,sym_set_elem,3,0,aflags=call_ignore_rules)
       enddo
    case(sym_rec)
       call trav_rec(coder,cblock,node)
    case(sym_query)
       if(pm_fast_isnull(node_arg(node,1))) then
          call make_comm_sys_call_rtn(coder,cblock,node,sym_active,0,1)
       else
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call make_comm_sys_call_rtn(coder,cblock,node,sym_active,1,1)
       endif
    case(sym_for)
!!$       call trav_par_expr(coder,cblock,node)
    case(sym_cast)
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call trav_expr(coder,cblock,node,node_arg(node,2))
       call make_sp_call_rtn(coder,cblock,node,sym_cast,2,1)
    case(sym_number,sym_string)
       if(coder%fixed) then
          call make_literal_const(coder,cblock,node,node_num_arg(node,1),fixit=.true.)
       else
          call make_literal_const(coder,cblock,node,node_num_arg(node,1))
       endif
    case default
       call dump_parse_tree(coder%context,6,pnode,2)
       write(*,*) sym_names(sym)
       call dump_parse_tree(coder%context,6,node,2)
       call pm_panic('Code generator - unexpected node in expr')
    end select

  contains
    include 'fisnull.inc'
    include 'fisname.inc'
    include 'fvkind.inc'
    include 'ftiny.inc'
    include 'fname.inc'
    include 'fnewnc.inc'
    include 'fesize.inc'

    subroutine name_const(pnode,nm)
      type(pm_ptr),intent(in):: pnode
      integer:: nm
      call make_const(coder,cblock,pnode,&
           pm_fast_name(coder%context,nm),&
           pm_new_name_type(coder%context,nm))
    end subroutine name_const
    
    subroutine range_const(p,n)
      type(pm_ptr):: p
      integer:: n
      call make_long_const(coder,cblock,p,0_pm_ln)
      call make_long_const(coder,cblock,p,int(n-1,pm_ln))
      call make_sys_call_rtn(coder,cblock,p,sym_dotdot,2,1)
    end subroutine range_const

    subroutine array_span(low,n)
      integer,intent(in):: low,n
      if(low==0) then
         call make_long_const(coder,cblock,node,&
              int(n,pm_ln))
      else
         call make_long_const(coder,cblock,node,&
              int(low,pm_ln))
         call make_long_const(coder,cblock,node,&
              int(low+n-1,pm_ln))
         call make_sys_call_rtn(coder,cblock,node,sym_dotdot,2,1)
      endif
    end subroutine array_span

    ! Check if any of the n arguments on the top of the vstack is
    ! a var with the var_is_maybe_chan flag set
    function check_args_for_chan(n) result(ok)
      integer,intent(in):: n
      logical:: ok
      integer:: i
      do i=coder%vtop-n+1,coder%vtop
         if(cnode_get_kind(coder%vstack(i))==cnode_is_var) then
            if(cnode_flags_set(coder%vstack(i),var_flags,var_is_maybe_not_private)) then
               ok=.true.
               return
            endif
         endif
      enddo
      ok=.false.
    end function check_args_for_chan
    
  end subroutine trav_expr

  !==================================================================
  ! Name in usual expression context (may be variable or parameter)
  !==================================================================
  subroutine trav_name(coder,cblock,node,sym,name)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in)::name
    integer:: sym
    type(pm_ptr):: p

    if(sym==sym_use) then
       p=find_param(coder,cblock,node,name,node_num_arg(node,2))
       if(pm_fast_isnull(p)) then
          p=find_imported_decl(coder,node,&
               name,node_num_arg(node,2),&
               modl_proc)
          if(pm_fast_isnull(p)) then
             ! Note find_imported decl gives own error messages
             call make_var(coder,cblock,node,name,0)
          else
             call proc_const_from_decl(coder,cblock,node,p)
          endif
       else
          call code_val(coder,p)
       endif
    else
       call trav_ref_to_var(coder,cblock,p,name,.false.)
    endif

  contains

    include 'fisnull.inc'
    
  end subroutine trav_name

  !========================================================
  ! Traverse "rec" expression
  ! Parse node contains full_type/ list_of_expr / name / tag
  !========================================================
  recursive subroutine trav_rec(coder,cblock,node)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node
    type(pm_ptr):: exprs,p,decl,tag,name1,name2,elems,info
    integer:: i,j,k,name,vbase,n,m,count,nam1,nam2,sym,basex,tno

    ! Find  associated type declaraton (decl)
    name=node_num_arg(node,4)
    tag=node_arg(node,3)
    decl=find_decl(coder,node,name,modl_type)
    if(pm_fast_isnull(decl)) then
       call code_error(coder,node,'No such type: ',name)
       call make_temp_var(coder,cblock,node)
       return
    else
       decl=node_arg(decl,2)
       if(node_sym(decl)/=sym_is) then
          call code_error(coder,node,'Not a "rec" type name:',name)
          call make_temp_var(coder,cblock,node)
          return
       else
          decl=node_arg(node_get(decl,type_includes),1)
          sym=node_sym(decl)
          if(sym/=sym_rec) then
             call code_error(coder,node,'Does not reference "rec" type')
             call code_error(coder,decl,'Declaration referenced in above error')
             call make_temp_var(coder,cblock,node)
             return
          endif
       endif
    endif
   
    elems=node_arg(decl,1)
    name2=node_arg(decl,2)
    vbase=coder%vtop
    
    ! Traverse element-initialisation expressions
    exprs=node_arg(node,2)
    do i=1,node_numargs(exprs)
       call trav_expr(coder,cblock,exprs,node_arg(node_arg(exprs,i),1))
    enddo
    
    ! Set up rec creation call
    basex=coder%vtop
    info=trav_rec_decl(coder,decl,decl)
    call make_const(coder,cblock,node,info)
    if(pm_fast_isnull(node_arg(node,1))) then
       call code_num(coder,-1)
       tno=info%data%i(info%offset+1)
       tno=pm_type_arg(coder%context,tno,1)
    else
       call trav_type(coder,node,node_arg(node,1))
       tno=pm_user_type_body(coder%context,top_word(coder))
       call code_num(coder,pop_word(coder))
    endif
    call code_val(coder,coder%var(coder%mask))
    
    ! At this point tno contains the body of the rec type
    
    ! Match element names and push values in correct order
    name1=pm_name_val(coder%context,int(tag%offset))
    name2=pm_name_val(coder%context,abs(int(name2%offset)))
    m=pm_fast_esize(name1)
    n=pm_fast_esize(name2)
    count=0
    outer:do j=1,n
       nam2=abs(name2%data%i(name2%offset+j))
       do i=1,m
          nam1=name1%data%i(name1%offset+i)
          if(nam1==nam2) then
             count=count+1
             call code_val(coder,coder%vstack(vbase+i))
             call cast_element(node_arg(exprs,i),pm_type_arg(coder%context,tno,j))
             cycle outer
          endif
       enddo
       p=node_arg(elems,j)
       if(node_sym(p)==sym_assign) then
          call trav_closed_expr(coder,cblock,p,node_arg(p,2))
       else
          call code_val(coder,coder%undef_val)
       endif
    enddo outer
    
    ! Some element names do no match - issue errors
    if(count/=m) then
       outer2:do i=1,m
          do j=1,n
             nam1=name1%data%i(name1%offset+i)
             nam2=abs(name2%data%i(name2%offset+j))
             if(nam1==nam2) cycle outer2
          enddo
          call element_error(exprs,sym,name,name1,i)
       enddo outer2
    endif

    ! Tidy up and create call
    if(pm_debug_checks) then
       if(coder%vtop/=basex+n+3) then
          write(*,*) '>>',coder%vtop,n,coder%vtop-n-2
          call pm_panic('trav_rec')
       endif
    endif
    call make_sp_call_rtn(coder,cblock,node,sym,n+3,1)
    coder%vstack(vbase+1)=coder%vstack(coder%vtop)
    coder%vtop=vbase+1
    
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'ftiny.inc'
    
    subroutine element_error(node,sym,name,name1,i)
      type(pm_ptr),intent(in):: node
      integer,intent(in):: name
      type(pm_ptr),intent(in):: name1
      integer,intent(in):: sym,i
      call code_error(coder,node_arg(node,i),'"'//trim(sym_names(sym))//' '//&
           trim(pm_name_as_string(coder%context,name))//&
           '" does not have element "'//&
           trim(pm_name_as_string(coder%context,&
           name1%data%i(name1%offset+i)))//'"')
    end subroutine element_error

    subroutine cast_element(node,tno)
      type(pm_ptr),intent(in):: node
      integer,intent(in):: tno
      !if(tno/=0) then
         call make_cast(coder,cblock,node,tno)
      !endif
    end subroutine cast_element
    
  end subroutine trav_rec

  !========================================================
  ! Traverse a cast to a type defined by node
  ! - sym gives some context, now ignored
  !========================================================
  recursive subroutine trav_cast(coder,cblock,pnode,node,sym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer,intent(in):: sym
    call trav_type_constraint(coder,pnode,node)
    if(top_word(coder)/=0) then
       call make_cast(coder,cblock,node,pop_word(coder))
    else
       call drop_word(coder)
    endif
  contains
    include 'fisnull.inc'
  end subroutine trav_cast

  !========================================================
  ! Make code for a cast to type tno
  !========================================================
  recursive subroutine make_cast(coder,cblock,node,tno)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: tno
    call make_const(coder,cblock,node,&
         pm_fast_tinyint(coder%context,tno),int(pm_tiny_int))
    call make_sp_call_rtn(coder,cblock,node,sym_type_val,1,1)
    call make_sys_call_rtn(coder,cblock,node,sym_as,2,1)
  contains
    include 'ftiny.inc'
  end subroutine make_cast


  !*************************************************
  ! TYPES
  !*************************************************
  
  !========================================================  
  ! Traverse type expression in parse tree
  !========================================================
  recursive subroutine trav_type_constraint(coder,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: pnode,node
    if(coder%top/=coder%proc_base) then
       call copy_type_vars(coder)
    endif
    call trav_type(coder,node,node)
    if(coder%top/=coder%proc_base) then
       call pop_type_vars(coder)
    endif
  end subroutine trav_type_constraint

  !========================================================
  ! Traverse type expression in parse tree
  !========================================================
  recursive subroutine trav_type(coder,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: pnode,node
    integer:: sym,i,n,flags
    integer::typno
    type(pm_ptr):: name,val,p
    if(pm_fast_isnull(node)) then
       call push_word(coder,0)
       return
    endif
    sym=node_sym(node)
    if(debug_codegen) then
       write(*,*) 'Trav type:',sym_names(sym)
       write(*,*) '------------------'
       call dump_parse_tree(coder%context,6,node,2)
       write(*,*) '------------------'
    endif

    select case(sym)
    case(sym_any)
       call push_word(coder,0)
    case(sym_or)
       n=node_numargs(node)
       call push_word(coder,pm_type_new_any)
       call push_word(coder,0)
       do i=1,n
          call trav_type(coder,pnode,node_arg(node,i))
       enddo
       call make_type(coder,2+n)
    case(sym_and)
       n=node_numargs(node)
       call push_word(coder,pm_type_new_all)
       call push_word(coder,0)
       do i=1,n
          call trav_type(coder,pnode,node_arg(node,i))
       enddo
       call make_type(coder,2+n)
    case(sym_pval)
       call push_word(coder,pm_type_new_poly)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_type_val)
       call push_word(coder,pm_type_new_type)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_includes)
       call push_word(coder,pm_type_new_includes)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       typno=top_word(coder)
       call trav_type(coder,pnode,node_arg(node,2))
       ! Defer test that include constraint meets main constraint
       call defer_type_check(coder,node,pnode,&
            typno,top_word(coder),sym_includes,&
            cnode_is_arg_constraint)
       call make_type(coder,4)
    case(sym_proc)
       if(node_numargs(node)==1) then
          p=find_decl(coder,node,node_num_arg(node,1),modl_proc)
          if(pm_fast_isnull(p)) then
             call code_error(coder,node,&
                  'proc value not associated with any defined procedure: ',&
                  node_num_arg(node,1))
             call push_word(coder,0)
             return
          endif
       elseif(node_numargs(node)==2) then
          p=find_imported_decl(coder,node,node_num_arg(node,1),&
               node_num_arg(node,2),modl_proc)
          if(pm_fast_isnull(p)) then
             call push_word(coder,0)
             return
          endif
       else
          call proc_type
          return
       endif
       call push_word(coder,proc_type_from_decl(coder,p,node))
    case(sym_unique)
       call push_word(coder,pm_new_name_type(coder%context,node_num_arg(node,1)))
    case(sym_fix)
       name=node_arg(node,1)
       select case(node_sym(name))
       case(sym_true)
          call push_word(coder,coder%true_fix)
       case(sym_false)
          call push_word(coder,coder%false_fix)
       case(sym_number,sym_string)
          call push_word(coder,&
               pm_fix_value_type_from_literal(coder%context,node_num_arg(name,1)))
       case default
          call push_word(coder,pm_type_new_fix)
          call push_word(coder,0)
          call trav_type(coder,pnode,name)
          call make_type(coder,3)
       end select
    case(sym_literal)
       name=node_arg(node,1)
       select case(node_sym(name))
       case(sym_true)
          call push_word(coder,coder%true_literal)
       case(sym_false)
          call push_word(coder,coder%false_literal)
       case(sym_number,sym_string)
          call push_word(coder,node_num_arg(name,1))
       case default
          call push_word(coder,pm_type_new_unfixed)
          call push_word(coder,0)
          call trav_type(coder,pnode,name)
          typno=pm_type_strip_to_basic(coder%context,pop_word(coder))
          if(typno/=0.and.typno/=pm_long.and.typno/=pm_double.and.&
               typno/=pm_logical.and.typno/=pm_string_type) then
             call code_error(coder,node,'Cannot have a literal type for: '//&
                  trim(pm_type_as_string(coder%context,typno)))
          endif
          call push_word(coder,typno)
          call make_type(coder,3)
       end select
    case(sym_contains)
       call push_word(coder,pm_type_new_contains)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_casts_to)
       call push_word(coder,pm_type_new_has)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_except)
       call push_word(coder,pm_type_new_except)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call trav_type(coder,pnode,node_arg(node,2))
       call make_type(coder,4)
    case(sym_type)
       call trav_type_decl(coder,pnode,node)
    case(sym_assign_or_init)
       call push_word(coder,pm_type_new_uninitialised)
       call push_word(coder,0)
       call push_word(coder,0)
       call make_type(coder,3)
    case(sym_open_brace)
       call push_word(coder,pm_type_new_user)
       call push_word(coder,node_num_arg(node,1))
       typno=get_typeno(2)
       if(typno==0) call pm_panic('Intrinsic type not found')
       call push_word(coder,typno)
    case(sym_rec)
       flags=node_num_arg(node,7)
       name=node_arg(node,2)
       call push_word(coder,pm_type_new_rec+flags)
       call push_word(coder,abs(int(name%offset)))
       val=node_arg(node,1)
       n=node_numargs(val)
       do i=1,n
          call trav_type(coder,pnode,node_arg(val,i))
       enddo
       call make_type(coder,n+2)
    case(sym_caret)
       call push_word(coder,pm_type_new_array)
       call push_word(coder,node_get_num(node,node_args+1))
       call trav_type(coder,pnode,node_arg(node,1))
       call trav_type(coder,pnode,node_arg(node,3))
       call push_word(coder,int(pm_long))
       call make_type(coder,5)
    case(sym_dcaret)
       call push_word(coder,pm_type_new_vect)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_underscore)
       call push_word(coder,pm_type_new_bottom)
       call push_word(coder,0)
       call make_type(coder,2)
    case(sym_const)
       call push_word(coder,pm_type_new_unfixed)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_list,sym_dotdotdot)
       if(sym==sym_dotdotdot) then
          call push_word(coder,pm_type_new_vtuple)
       else
          call push_word(coder,pm_type_new_tuple)
       endif
       call push_word(coder,0)
       n=node_numargs(node)
       do i=1,n,2
          val=node_arg(node,i)
          call trav_type(coder,val,val)
       enddo
       call make_type(coder,n/2+2)
    case(sym_pm_list)
       call push_word(coder,pm_type_new_vtuple+pm_type_is_list)
       call push_word(coder,0)
       call push_word(coder,0)
       call make_type(coder,3)
    case(sym_assign,sym_var)
       call trav_type(coder,pnode,node_arg(node,1))
    case(sym_pm_dref)
       call push_word(coder,pm_type_is_dref)
       n=node_get_num(node,node_args)
       call push_word(coder,n)
       n=node_numargs(node)
       do i=2,n
          call trav_type(coder,pnode,node_arg(node,i))
       enddo
       call make_type(coder,n+1)
    case(sym_mode)
       call trav_type(coder,pnode,node_arg(node,1))
       typno=pop_word(coder)
       call push_word(coder,&
            pm_type_add_mode(coder%context,typno,&
            node_num_arg(node,2),istype=.true.)) 
    case(sym_result)
       call push_word(coder,pm_type_new_tuple)
       call push_word(coder,0)
       n=node_numargs(node)
       do i=1,n
          val=node_arg(node,i)
          call trav_type(coder,pnode,val)
       enddo
       call make_type(coder,n+2)
    case default
       write(*,*) '======BAD PARSE NODE IN TYPE===='
       if(sym>=0.and.sym<=num_sym) then
          write(*,*) 'SYM=',sym_names(sym)
       else
          write(*,*) 'SYMno=',sym
       endif
       write(*,*) '=============PNODE=============='
       call dump_parse_tree(coder%context,6,pnode,2)
       write(*,*) '============= NODE=============='
       call dump_parse_tree(coder%context,6,node,2)
       write(*,*)  '================================'
       call pm_panic('Type parse node not ok')
    end select
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'fistiny.inc'
    include 'fname.inc'
    include 'fisname.inc'

    ! Look up type name and return number
    function get_typeno(size) result(tno)
      integer,intent(in):: size
      integer:: tno
      tno=pm_type_lookup(coder%context,&
           coder%wstack(coder%wtop-size+1:coder%wtop))
    end function get_typeno

    ! Look up type name and return number
    function get_user_typeno(size) result(tno)
      integer,intent(in):: size
      integer:: tno
      tno=pm_user_type_lookup(coder%context,&
           coder%wstack(coder%wtop-size+1:coder%wtop))
    end function get_user_typeno

    recursive subroutine proc_type
      type(pm_ptr):: dp,list,arg
      integer:: i,j,n,base

      call push_word(coder,pm_type_new_proc)
      call push_word(coder,0)

      base=coder%wtop
      dp=node_arg(node,1)
      do i=1,node_numargs(dp),2
         if(.not.pm_fast_isnull(find_type_var(coder,node_num_arg(dp,i)))) then
            call code_error(coder,node,&
                 'Cannot shadow type-match parameter:',node_num_arg(dp,i))
         endif
      enddo
      call push_word(coder,pm_type_new_proc_sig)
      call push_word(coder,node_num_arg(node,2))
 
      do i=3,4
         list=node_arg(node,i)
         call push_word(coder,&
              merge(pm_type_is_vtuple,pm_type_is_tuple,node_sym(list)==sym_dotdotdot))
         if(i==4.or.pm_fast_isnull(node_arg(node,5))) then
            call push_word(coder,0)
         else
            call push_word(coder,node_num_arg(node,5))
         endif
         n=node_numargs(list)
         do j=1,n
            arg=node_arg(list,j)
            call trav_type(coder,arg,arg)
         enddo
         call make_type(coder,n+2)
      enddo
      call make_type(coder,4)
      coder%wstack(base+1)=coder%wstack(coder%wtop)
      coder%wtop=base+1
      call make_type(coder,3)
      
    end subroutine proc_type

  end subroutine trav_type

  !=================================================================
  ! Traverse a type reference T or T(args)
  ! - type node is of the form args type_name
  ! - process any associated type definition, if not already cached
  !=================================================================
  recursive subroutine trav_type_decl(coder,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: pnode,node
    type(pm_ptr):: namenode,decl,dec,inc,pargs,also_dec
    type(pm_ptr):: twice_dec,main_dec,pars,newdec
    logical:: is_present,also_present,type_present
    logical:: dotdotdot_present,multiple_modules,twice,has_constraints
    integer:: name,nargs,sym,i,base,parbase,ibase,npars,idepth
    integer:: new_type

    ! Type name and arguments
    nargs=node_numargs(node)-1
    namenode=node_arg(node,nargs+1)
    if(pm_fast_isname(namenode)) then
       name=namenode%offset
    else
       name=node_num_arg(namenode,2)
    endif
    call push_word(coder,pm_type_new_user)
    call push_word(coder,-1)
    if(nargs>0) then
       ! Type arguments
       do i=1,nargs
          call trav_type(coder,pnode,node_arg(node,i))
       enddo
    endif

    ! Debugging stuff
    if(debug_codegen) then
       write(*,*) 'Traverse type def: ',&
            trim(pm_name_as_string(coder%context,name)),' nargs=',nargs,'{'
       do i=coder%wtop-nargs+1,coder%wtop
          write(*,*) coder%wstack(i)
       enddo
       write(*,*) '}'
    endif

    base=coder%wtop

    ! Check if this is a type variable
    if(nargs==0.and.pm_fast_isname(namenode)) then
       decl=find_type_var(coder,name)
       if(.not.pm_fast_isnull(decl)) then
          coder%wtop=coder%wtop-nargs-1
          coder%wstack(coder%wtop)=decl%offset
          return
       endif
    endif

    ! Find declaration
    if(.not.pm_fast_isname(namenode)) then
       decl=find_imported_decl(coder,namenode,&
            node_num_arg(namenode,1),node_num_arg(namenode,2),modl_type)
       if(pm_fast_isnull(decl)) then
          call code_error(coder,node,&
               'Cannot find type: '//&
               trim(pm_name_as_string(coder%context,node_num_arg(namenode,1)))//'::'//&
               trim(pm_name_as_string(coder%context,name)))
          goto 888
       endif
    else 
       decl=find_decl(coder,node,name,modl_type)
       if(pm_fast_isnull(decl)) then
          ! Not found but may be intrinsic declaration
          coder%wstack(coder%wtop-nargs)=name
          new_type=get_typeno(nargs+2)
          if(new_type>0) then
             ! .. is intrinsic declaraton, return it
             coder%wtop=base-nargs-1
             coder%wstack(coder%wtop)=new_type
             return
          endif
          call code_error(coder,node,&
               'Cannot find type: '//&
               trim(pm_name_as_string(coder%context,name)))
          goto 888
       endif
    endif

    ! Get any cached type
    coder%wstack(coder%wtop-nargs)=node_num_arg(decl,1)
    new_type=get_typeno(nargs+2)
    if(new_type>0) then
       ! Already processed and cached - just return
       coder%wtop=base-nargs-1
       coder%wstack(coder%wtop)=new_type
       return
    endif
 
    ! Create user type entry - pointing to nothing to start with
    new_type=pm_new_user_type(coder%context,coder%wstack(base-nargs-1:base),0)
 
    ! Check for excessive recursion
    idepth=node_get_num(decl,node_args+4)
    if(idepth>max_type_nesting) then
       if(idepth==max_type_nesting+1) then
          call code_error(coder,decl,&
               'Very complex recursive type - most probably infinite: '&
               //trim(pm_name_as_string(coder%context,&
               name)))
          call node_set_num(decl,node_args+4,idepth+1)
       endif
       coder%wtop=coder%wtop-nargs-1
       coder%wstack(coder%wtop)=0
       return
    endif
    call node_set_num(decl,node_args+4,idepth+1)

    ! Find main definition of type
    dotdotdot_present=.false.
    multiple_modules=.false.
    dec=node_arg(decl,2)
    ibase=-1
    do
       sym=node_sym(dec)
       select case(sym)
       case(sym_includes,sym_is,sym_dotdotdot)
          main_dec=dec
          parbase=coder%wtop
          pars=node_get(dec,type_params)
          npars=node_numargs(pars)/2
          call make_type_vars(coder,name,dec,node,pars,&
               base-nargs,nargs)
          inc=node_get(dec,type_parents)
          if(.not.pm_fast_isnull(inc)) then
             do i=1,node_numargs(inc)
                call trav_type(coder,pnode,node_arg(inc,i))
                call drop_word(coder)
             enddo
          endif
          has_constraints=.not.pm_fast_isnull(node_get(main_dec,type_constraints))
          if(sym/=sym_is) then
             inc=node_get(dec,type_includes)
             if(.not.pm_fast_isnull(inc)) then
                ibase=coder%wtop
                call push_word(coder,pm_type_new_any)
                call push_word(coder,0)
                do i=1,node_numargs(inc)
                   call trav_type(coder,pnode,node_arg(inc,i))
                   if(has_constraints) then
                      call check_constraints(top_word(coder),node_arg(inc,i))
                   endif
                enddo
             else
                ibase=coder%wtop
                dotdotdot_present=.true.
                call push_word(coder,pm_type_new_any)
                call push_word(coder,0)
             endif
          else
             inc=node_get(dec,type_includes)
             call trav_type(coder,pnode,node_arg(inc,1))
             if(has_constraints) then
                call check_constraints(top_word(coder),inc)
             endif
          endif
          call pop_type_vars(coder)
          exit
       case(sym_in)
          dec=node_arg(dec,1)
       case default
          dec=node_get(dec,type_link)
       end select
       if(pm_fast_isnull(dec)) then
          call code_error(coder,decl,&
               'Type is extended using ":" or "..." but not defined: '//&
               trim(pm_name_as_string(coder%context,name)))
          goto 999
       endif
    enddo

    ! Process any "in" or also ("...,") declarations for this type
    is_present=.false.
    also_present=.false.
    type_present=.false.
    twice=.false.
    dec=node_arg(decl,2)
    do
       if(pm_debug_checks) then
          if(dec%data%vkind/=pm_pointer) &
               call pm_panic('Type node not ptr in trav def')
       endif
       sym=node_sym(dec)
       if(debug_codegen) then
          write(*,*) 'CHECK TYPE DEF>',sym_names(sym)
       endif
       if(sym==sym_in) then
          call make_type_vars(coder,name,pnode,node,&
               pm_null_obj,0,0)
          call trav_type(coder,pnode,node_arg(dec,2))
          call pop_type_vars(coder)
          if(has_constraints) then
             call check_constraints(top_word(coder),dec)
          endif
          if(.not.also_present) then
             also_present=.true.
             also_dec=dec
          endif
          newdec=node_arg(dec,1)
       else
          if(sym==sym_is) then
             if(is_present.or.type_present) then
                twice=.true.
                twice_dec=dec
             endif
             is_present=.true.
          else if(sym==sym_also) then
             also_present=.true.
             also_dec=dec
             pargs=node_get(dec,type_params)
             call make_type_vars(coder,name,&
                  pnode,node,pargs,base-nargs,nargs,&
                  parbase,npars)
             inc=node_get(dec,type_includes)
             if(.not.pm_fast_isnull(inc)) then
                do i=1,node_numargs(inc)
                   call trav_type(coder,pnode,node_arg(inc,i))
                   if(has_constraints) then
                      call check_constraints(top_word(coder),dec)
                   endif
                enddo
             endif
             call pop_type_vars(coder)
          else
             ! sym_dotdotdot, sym_includes
             if(pm_debug_checks) then
                if(sym/=sym_dotdotdot.and.sym/=sym_includes) then
                   if(sym>=0.and.sym<=num_sym) then
                      write(*,*) 'SYM=',trim(sym_names(sym))
                   else
                      write(*,*) 'SYM=',sym
                   endif
                   call pm_panic('Not a type in trav_type_decl')
                endif
             endif
             if(sym==sym_dotdotdot) then
                dotdotdot_present=.true.
             endif
             if(is_present.or.type_present) then
                twice=.true.
                twice_dec=dec
                also_dec=dec
             endif
             type_present=.true.
          endif
          
          newdec=node_get(dec,type_link)
       endif
       if(pm_fast_isnull(newdec)) exit
       if(node_get_modl_name(dec)/=&
            node_get_modl_name(newdec)) multiple_modules=.true.
       dec=newdec
    enddo

    ! Create a union type from the parts brought together
    if(.not.is_present.and.coder%wtop>ibase) then
       if(coder%wtop-ibase>3) then
          call make_type(coder,coder%wtop-ibase)
       endif
    endif

    ! Set the body of the user type to be the new type
    call pm_user_type_set_body(coder%context,new_type,top_word(coder))

    ! Tidy up and place new type on wstack
    base=base-nargs-1
    coder%wstack(base)=new_type
    coder%wtop=base

    ! Pop current type nesting level (used to check recursion)
    idepth=node_get_num(decl,node_args+4)
    if(idepth<max_type_nesting) then
       call node_set_num(decl,node_args+4,idepth-1)
    else
       return
    endif

    ! Check for a range of errors
    if(is_present.and.also_present) then
       call code_error(coder,also_dec,&
            'Cannot add to this type using ":" or "...": '//&
            trim(pm_name_as_string(coder%context,name)))
       call code_error(coder,main_dec,&
            'Type declaration being extended in the above error')
    endif
    if(also_present.and..not.type_present) then
       call code_error(coder,also_dec,&
            '"Type extended using "..." or ":" without original "type is " definition present: '//&
            trim(pm_name_as_string(coder%context,name)))
    endif
    if(multiple_modules.and.also_present.and..not.dotdotdot_present) then
       call code_error(coder,also_dec,&
            'Type is extended using "..." or ":" across multiple modules"//&
            " without "..." present in original "type is": '//&
            trim(pm_name_as_string(coder%context,name)))
    endif
    if(twice) then
       call code_error(coder,main_dec,&
            'Type is defined twice: ',name)
       call code_error(coder,twice_dec,'... second definition')
       call node_set_num(decl,node_args+4,max_type_nesting+2)
    endif
    
    if(debug_codegen) then
       write(*,*) 'definition traversed for ',&
            trim(pm_name_as_string(coder%context,name)),'#',top_word(coder)
       write(*,*) '#', trim(pm_type_as_string(coder%context,top_word(coder)))
    endif
 
    return

    ! Error returns with tidy up
999 continue

    call node_set_num(decl,node_args+4,max_type_nesting+2)

888 continue
    coder%wtop=coder%wtop-nargs-1
    coder%wstack(coder%wtop)=0
    
  contains
    
    include 'fisnull.inc'
    include 'ftypeno.inc'
    include 'fnewnc.inc'
    include 'fesize.inc'
    include 'ftiny.inc'
    include 'fvkind.inc'
    include 'fisname.inc'

    ! Look up type name and return number
    function get_typeno(size) result(tno)
      integer,intent(in):: size
      integer:: tno
      tno=pm_type_lookup(coder%context,&
           coder%wstack(coder%wtop-size+1:coder%wtop))
    end function get_typeno

    subroutine check_constraints(tno,node)
      integer,intent(in):: tno
      type(pm_ptr):: node
      type(pm_ptr):: constraints
      integer:: i
      
      ! Make an entry for each ": type" entry to be checked later
      constraints=node_get(main_dec,type_constraints)
      do i=1,node_numargs(constraints)
         call trav_type(coder,main_dec,node_arg(constraints,i))
         call defer_type_check(coder,node,node_arg(constraints,i),pop_word(coder),tno,&
              0,cnode_is_type_constraint)
      enddo
    end subroutine check_constraints
    
  end subroutine trav_type_decl


  !===============================================================
  ! Create a template type from rec declaration
  !===============================================================
  recursive function trav_rec_decl(coder,pnode,decl) result(vect)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: decl,pnode
    type(pm_ptr):: vect
    
    integer:: tno
    integer:: i,n,base
    type(pm_ptr):: params,arg

    vect=node_arg(decl,6)
    if(.not.pm_fast_isnull(vect)) return
    
    base=coder%wtop
    params=node_arg(decl,5)
    if(.not.pm_fast_isnull(params)) then 
       n=node_numargs(params)
       do i=1,n,2
          arg=node_arg(params,i+1)
          call push_word(coder,pm_type_new_param)
          call push_word(coder,(i+1)/2)
          call trav_type(coder,arg,arg)
          call make_type(coder,3)
       enddo
       n=n/2
       call make_type_vars(coder,0,pnode,decl,params,coder%wtop-n,n)
       call trav_type(coder,pnode,decl)
       tno=pop_word(coder)
       call pop_type_vars(coder)
       tno=pm_new_params_type(coder%context,n,tno)
       vect=pm_fast_newnc(coder%context,pm_int,5)
       call code_val(coder,vect) ! protect from GC
       vect%data%i(vect%offset)=node_num_arg(decl,2)
       vect%data%i(vect%offset+1)=tno
       vect%data%i(vect%offset+2)=node_num_arg(decl,3)
       vect%data%i(vect%offset+3)=n
       vect%data%i(vect%offset+4)=node_num_arg(decl,7)
       call pm_ptr_assign(coder%context,decl,node_args+5_pm_ln,vect)
       call drop_code(coder)
    else
       call trav_type(coder,pnode,decl)
       tno=pop_word(coder)
       tno=pm_new_params_type(coder%context,0,tno)
       vect=pm_fast_newnc(coder%context,pm_int,5)
       call code_val(coder,vect) ! protect from GC
       vect%data%i(vect%offset)=node_num_arg(decl,2)
       vect%data%i(vect%offset+1)=tno
       vect%data%i(vect%offset+2)=node_num_arg(decl,3)
       vect%data%i(vect%offset+3)=0
       vect%data%i(vect%offset+4)=node_num_arg(decl,7)
       call pm_ptr_assign(coder%context,decl,node_args+5_pm_ln,vect)
       call drop_code(coder)
    endif
    call pm_type_record_by_name(coder%context,&
            node_get_num(decl,node_args+2),tno)
    coder%wtop=base
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
  end function trav_rec_decl

  !===========================================================
  ! Push information on arguments to parameterised type
  ! If parbase is not present, leave parameters on wstack
  !===========================================================
  subroutine make_type_vars(coder,parent,pnode,callnode,pnames,argbase,nargs,&
       parbase,nbasepars)
    type(code_state),intent(inout):: coder
    integer,intent(in):: parent
    type(pm_ptr),intent(in):: pnode,callnode,pnames
    integer,intent(in):: argbase,nargs
    integer,intent(in),optional:: parbase
    integer,intent(in),optional:: nbasepars
    integer:: k,base,wbase,npars
    integer:: vtyp,partyp,name,pname
    logical:: check_against_base
    check_against_base=.false.
    name=node_num_arg(callnode,node_numargs(callnode))
    if(pm_fast_isnull(pnames)) then
       if(nargs/=0) then
          call code_error(coder,callnode,&
               'Type arguments provided to a type with no parameters: ',name)
       endif
       npars=0
    else
       npars=node_numargs(pnames)/2
       if(present(nbasepars)) then
          if(nbasepars/=npars) then
             call code_error(coder,pnames,&
                  'Number of parameters in "type is ...,"'//&
                  ' does not match original definition:',name)
             goto 10
          else
             check_against_base=.true.
          endif
       endif
       if(nargs>npars) then
          call code_error(coder,callnode,&
               'Number of type arguments supplied is greater '//&
               ' than the number of parameters:',name)
       endif
10     continue
    endif
    coder%top=coder%top+1
    coder%stack(coder%top)=typevar_start
    coder%var(coder%top)=pm_null_obj
 
    base=coder%top
    wbase=coder%wtop
    if(.not.present(parbase)) then
       do k=1,npars
          call trav_type(coder,pnode,node_arg(pnames,k*2))
       enddo
    endif
    do k=npars,1,-1
       pname=node_num_arg(pnames,k*2-1)
       coder%stack(k+coder%top+1)=pname
       if(k>nargs) then
          vtyp=0
       else
          vtyp=coder%wstack(argbase+k)
       endif
      
       if(present(parbase)) then
          call trav_type(coder,pnode,node_arg(pnames,k*2))
          partyp=pop_word(coder)
          ! If this is a further constrained part of the
          ! type then need to compute intersection of
          ! constraint and argument type
          
          if(partyp>0.and.check_against_base) then

             ! Defer test that parameter conforms to
             ! parameter in base definition
             call defer_type_check(coder,pnames,pnode,coder%wstack(parbase+k),&
                  partyp,pname,cnode_is_par_constraint)
             
             ! Intersect argument and parameter
             call push_word(coder,pm_type_new_all)
             call push_word(coder,0)
             call push_word(coder,min(vtyp,partyp))
             call push_word(coder,max(vtyp,partyp))
             call make_type(coder,4)
             vtyp=pop_word(coder)
          endif
          
       else
          partyp=coder%wstack(wbase+k)
          
          if(vtyp==0) then
             vtyp=partyp
          else
             ! Defer test that argument meets parameter constraint
             call defer_type_check(coder,callnode,pnode,&
                  partyp,vtyp,pname,cnode_is_arg_constraint)
          endif
       endif

       if(find_var_entry(coder,pname,base)>0) then
          call code_error(coder,pnames,&
               'Repitition of type parameter name:',&
               pname)
       else
          call push_var(coder,pname,&
               pm_fast_tinyint(coder%context,vtyp))
       endif

    enddo
    coder%top=coder%top+1
    coder%stack(coder%top)=typevar_end
    coder%var(coder%top)%offset=base
  contains
    include 'ftiny.inc'
    include 'fisnull.inc'
  end subroutine make_type_vars

  !=========================================================
  ! Pop current frame of type variables from variable stack
  !========================================================
  subroutine pop_type_vars(coder)
    type(code_state),intent(inout):: coder
    integer:: base
    base=coder%var(coder%top)%offset
    if(pm_debug_checks) then
       if(coder%stack(coder%top)/=typevar_end) &
            call pm_panic('Pop type vars  - no end record')
    endif 
    coder%top=coder%top-1
    coder%top=base
    if(pm_debug_checks) then
       if(coder%stack(coder%top)/=typevar_start) &
            call pm_panic('Pop type vars  - not at start record')
    endif
    coder%top=coder%top-1
  end subroutine pop_type_vars

  !========================================================
  ! Copy type variables (obsolete?)
  !========================================================
  subroutine copy_type_vars(coder)
    type(code_state),intent(inout):: coder
    integer:: top,base,i,nbase
    top=coder%proc_base
    base=coder%var(top)%offset
    coder%top=coder%top+1
    nbase=coder%top
    coder%stack(coder%top)=typevar_start
    coder%var(coder%top)=pm_null_obj
    do i=base+1,top-1
       if(coder%stack(i)/=0) then
          call push_var(coder,coder%stack(i),coder%var(i))
       endif
    enddo
    coder%top=coder%top+1
    coder%stack(coder%top)=typevar_end
    coder%var(coder%top)%offset=nbase
  end subroutine copy_type_vars

  !========================================
  ! Find type variable (parameter)
  !========================================
  function find_type_var(coder,vname) result(vr)
    type(code_state),intent(inout):: coder
    integer,intent(in):: vname
    type(pm_ptr):: vr
    integer:: k
    integer:: n
    if(coder%top==0) then
       vr=pm_null_obj
    elseif(coder%stack(coder%top)/=typevar_end) then
       vr=pm_null_obj
    else
       n=vname
       k=find_var_entry(coder,n,int(coder%var(coder%top)%offset))
       if(k/=0) then
          vr=coder%var(k)
       else
          vr=pm_null_obj
       endif
    end if
  end function find_type_var

  !=========================================================
  ! Defer a type check until all types have been constructed
  !=========================================================
  subroutine defer_type_check(coder,pnode,node,typ1,typ2,sym,kind)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: pnode,node
    integer,intent(in):: typ1,typ2,sym,kind
    call code_val(coder,coder%defer_check)
    call code_num(coder,typ1)
    call code_num(coder,typ2)
    call code_num(coder,sym)
    call code_val(coder,node)
    call make_code(coder,pnode,kind,5)
    coder%defer_check=pop_code(coder)
  end subroutine defer_type_check

  !==========================================================
  ! Complete type definitions - check type validity
  ! and process deferred checks of various type constraints
  !===========================================================
  subroutine complete_type_checks(coder)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: p,keys,vals,tv
    integer(pm_ln):: i
    integer:: k
    integer:: tno,tno1,tno2
    type(pm_ptr):: name
    p=coder%prog_cblock
    keys=pm_dict_keys(coder%context,coder%context%tcache)
    vals=pm_dict_vals(coder%context,coder%context%tcache)

    ! Check no named type is problematically recursive
    do i=0,pm_dict_size(coder%context,coder%context%tcache)-1
       tv=keys%data%ptr(keys%offset+i)
       if(pm_tv_kind(tv)==pm_type_is_user) then
          tno=i+1
          if(pm_type_is_recur(coder%context,tno,tno)) then
             call code_error(coder,pm_null_obj,&
                  'Type directly includes itself: '//&
                  trim(pm_type_as_string(coder%context,tno)))
             call pm_ptr_assign(coder%context,vals,i,pm_null_obj)
          endif
       endif
    enddo
    
    ! Check all named types include themselves (weeds out some errors)
    do i=0,pm_dict_size(coder%context,coder%context%tcache)-1
       tv=keys%data%ptr(keys%offset+i)
       if(pm_tv_kind(tv)==pm_type_is_user) then
          tno=i+1
          ! Check type includes its body to avoid automatic true return
          if(.not.pm_type_includes(coder%context,tno,&
               pm_user_type_body(coder%context,tno),pm_type_incl_type)) then
             call code_error(coder,pm_null_obj,&
                  'Type is incorrectly defined: '//&
                  trim(pm_type_as_string(coder%context,tno)))
          endif
       endif
    enddo

    ! Now complete all deferred checks
    p=coder%defer_check
    do while(.not.pm_fast_isnull(p))
       k=cnode_get_kind(p)
       name=cnode_arg(p,4)
       select case(k)
       case(cnode_is_arg_constraint)
          tno1=cnode_num_arg(p,2)
          tno2=cnode_num_arg(p,3)
          if(.not.pm_type_includes(coder%context,tno1,tno2,pm_type_incl_type)) then
             call cnode_error(coder,p,&
                  'Type argument "'//&
                  trim(pm_name_as_string(coder%context,&
                  int(name%offset)))//&
                  '" does not meet constraint: '//&
                  trim(pm_type_as_string(coder%context,tno1))//&
                  ' inc '//&
                  trim(pm_type_as_string(coder%context,tno2)))
             call code_error(coder,cnode_arg(p,5),&
                  'Constraint that gave rise to above error')
          endif
       case(cnode_is_par_constraint)
          tno1=cnode_get_num(p,cnode_args+1)
          tno2=cnode_get_num(p,cnode_args+2)
          if(.not.pm_type_includes(coder%context,tno1,tno2,pm_type_incl_type)) then
             call cnode_error(coder,p,&
                  'Parameter "'//&
                  trim(pm_name_as_string(coder%context,&
                  int(name%offset)))//&
                  '" does not match base type; parameter contraint: '//&
                  trim(pm_type_as_string(coder%context,tno1))//&
                  ' ,argument: '//&
                  trim(pm_type_as_string(coder%context,tno2)))
             call code_error(coder,cnode_arg(p,5),&
                  'Constraint that gave rise to the above error')
          endif
       case(cnode_is_type_constraint)
          tno1=cnode_get_num(p,cnode_args+1)
          tno2=cnode_get_num(p,cnode_args+2)
          if(.not.pm_type_includes(coder%context,tno1,tno2,pm_type_incl_equiv)) then
             call cnode_error(coder,p,'Type does not meet constraint:')
             call code_error(coder,cnode_arg(p,5),&
                  'Type constraint referenced in above error')
          endif
       end select
       p=cnode_arg(p,1)
    enddo
    coder%defer_check=pm_null_obj
  contains
    include 'fisnull.inc'
  end subroutine complete_type_checks

  
  !********************************************************
  ! CALLS AND PROCEDURES
  !********************************************************

  !========================================================
  ! Traverse a procedure call
  !========================================================
  recursive subroutine trav_call(coder,cblock,pnode,node,nret,amps_ok)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer,intent(in):: nret
    logical,intent(in):: amps_ok
    type(pm_ptr):: list,procs,keys,keynames,name,amp,amps,prvar,proc
    integer:: flags,i,nargs,nkeys,vsym,outmode
    integer:: otop,obase,owbase,base
    logical:: iscomm,isdot
    
    ! Save stack tops to check clean up
    otop=coder%top
    obase=coder%vtop
    owbase=coder%wtop

    ! Determine properties of call
    name=node_arg(node,1)
    list=node_arg(node,2)
    amp=node_arg(node,3)
    keys=node_arg(node,4)
    keynames=node_arg(node,5)
    flags=node_num_arg(node,6)
    if(node_sym(list)==sym_dotdotdot) then
       flags=ior(flags,call_is_vararg)
    endif
    nargs=node_numargs(list)
    iscomm=iand(flags,proccall_is_comm)/=0
    isdot=iand(flags,proccall_is_ref)/=0
    
    if(debug_codegen) then
       write(*,*) 'TRAV CALL>',&
            trim(pm_name_as_string(coder%context,int(name%offset))),&
            nargs,nret,coder%vtop,flags
    endif

    if(coder%par_state==par_state_none.and.&
         iscomm.and.iand(flags,proccall_is_general)==0) then
       call code_error(coder,node,&
            'Cannot call a "%" procedure outside of a parallel context: ',int(name%offset))
    endif

    base=coder%vtop

    ! write(*,*) 'AMP',pm_fast_isnull(amp),trim(pm_name_as_string(coder%context,int(name%offset)))
    
    ! Standard arguments
    if(pm_fast_isnull(amp)) then
       do i=1,nargs
          call trav_expr(coder,cblock,list,&
               node_arg(list,i))
       enddo
    else
       amps=pm_name_val(coder%context,int(amp%offset))
       flags=ior(flags,call_is_assign_call)
       if(.not.amps_ok) then
          if(iand(flags,proccall_is_yield)/=0) then
             call code_error(coder,list,'"yield" cannot be a component of an expression')
          elseif(iand(flags,proccall_is_ref)==0) then
             call code_error(coder,list,&
                 'Call using "&" arguments cannot be a component of an expression')
          endif
       endif
       call process_amp_args(list,amps)
    endif

    ! Create argument list node from nargs + nret values on vstack
    call make_arglist(coder,cblock,node,nargs,nret,.false.,iscomm,comm_args_present=.true.)

    ! Keyword arguments
    if(.not.pm_fast_isnull(keys)) then
       nkeys=node_numargs(keys)
       do i=1,nkeys
          call trav_expr(coder,cblock,node,node_arg(keys,i))
       enddo
       call make_arglist(coder,cblock,node,nkeys,0,.false.,iscomm)
    else
       nkeys=0
       call code_null(coder)
    endif

    ! Find procs with this name
    proc=pm_null_obj
    if(pm_fast_isname(name)) then
       proc=find_decl(coder,node,int(name%offset),modl_proc)
       if(pm_fast_isnull(proc)) then
          call code_error(coder,node,'Cannot find proc: ',int(name%offset))
          call make_temp_var(coder,cblock,node)
       endif
    else
       vsym=node_sym(name)
       select case(vsym)
       case(sym_name)
          proc=find_decl(coder,name,node_num_arg(name,1),modl_proc)
          if(pm_fast_isnull(proc)) then
             call code_error(coder,name,'Cannot find proc: ',node_num_arg(name,1))
          endif
       case(sym_use)
          proc=find_imported_decl(coder,name,node_num_arg(name,1),&
               node_num_arg(name,2),modl_proc)
       case(sym_dot)
          call trav_expr(coder,cblock,node,node_arg(name,1))
       case(sym_proc)
          if(node_numargs(name)==1) then
             proc=find_decl(coder,name,node_num_arg(name,1),modl_proc)
             if(pm_fast_isnull(proc)) then
                call code_error(coder,name,'Cannot find proc: ',node_num_arg(name,1))
             endif
          else
             proc=find_imported_decl(coder,name,node_num_arg(name,1),&
                  node_num_arg(name,2),modl_proc)
          endif
       case default
          write(*,*) sym_names(vsym)
          call pm_panic('Bad VSYM in trav_call')
       end select
       if(vsym/=sym_dot.and.vsym/=sym_method_call.and.pm_fast_isnull(proc)) then
          call make_temp_var(coder,cblock,name)
       endif
    endif

    ! Now find procs with this signature
    if(.not.pm_fast_isnull(proc)) then
       prvar=pm_null_obj
       procs=find_sig(coder,node,name,proc)
    else
       ! f.(...) call
       prvar=pop_code(coder)
       procs=pm_fast_tinyint(coder%context,0)
    endif
    
    ! Error return if no such proc
    if(pm_fast_isnull(procs)) then
       coder%vtop=obase-nret
       return
    endif
   
    ! Make the actual call node
    call make_full_call(coder,cblock,node,procs,amp,&
         nargs,nret,nkeys,keynames,flags,prvar)
    
    ! If this is a variable call, flag the variable
    if(.not.pm_fast_isnull(prvar)) then
       if(cnode_get_kind(prvar)==cnode_is_var) then
          if(cnode_flags_set(prvar,var_flags,var_is_accessed)) then
             call cnode_set_flags(prvar,var_flags,var_is_multi_access)
          else
             call cnode_set_flags(prvar,var_flags,var_is_accessed)
          endif
       endif
    endif
    
    ! If debugging compiler, check tidy up
    if(pm_debug_checks) then
       if(coder%vtop/=obase-nret) then
          write(*,*) coder%vtop,'/=',obase,'-',nret,&
               trim(pm_name_as_string(coder%context,int(name%offset)))
          call pm_panic('trav_call vstack mismatch')
       endif
       if(coder%wtop/=owbase) then
          write(*,*) coder%wtop,'/=',owbase,&
               trim(pm_name_as_string(coder%context,int(name%offset)))
          call pm_panic('trav_call wstack mismatch')
       endif
!!$       if(coder%top/=otop) then
!!$          write(*,*) coder%top,'/=',otop
!!$          call pm_panic('trav_call local stack mismatch')
!!$       endif
    endif

    if(debug_codegen) then
       write(*,*) 'END TRAV CALL>',&
            trim(pm_name_as_string(coder%context,int(name%offset)))
    endif

  contains
    include 'fisnull.inc'
    include 'fname.inc'
    include 'fisname.inc'
    include 'fesize.inc'
    include 'fvkind.inc'
    include 'ftiny.inc'

    subroutine process_amp_args(node,amp)
      type(pm_ptr),intent(in):: node
      type(pm_ptr),intent(in):: amp
      integer:: i,j,jj,k,sym,first_amp
      type(pm_ptr):: arg,arg2
      logical:: aliased(nargs),xaliased(nargs,nargs),is_amp(nargs),alias,any_aliased
      character(len=*),parameter:: emess='An "&" argument aliases another argument'

      j=0
      any_aliased=.false.
      aliased=.false.
      first_amp=amp%data%i(amp%offset)
      do i=1,nargs
         arg=node_arg(node,i)
         is_amp(i)=amp%data%i(amp%offset+j)==i
         if(is_amp(i)) then
            xaliased(:,i)=.false.
            do k=1,i-1
               arg2=node_arg(list,k)
               sym=node_sym(arg2)
               if(sym==sym_name.or.sym==sym_reference) then
                  if(check_aliased(coder,arg,arg2,emess)) then
                     aliased(i)=.true.
                     aliased(k)=.true.
                     xaliased(k,i)=.true.
                  endif
               endif
            enddo
            j=min(j+1,pm_fast_esize(amp))
         else
            sym=node_sym(arg)
            if(sym==sym_reference.or.sym==sym_name) then
               do k=first_amp,i-1
                  if(is_amp(k)) then
                     if(check_aliased(coder,arg,arg2,emess)) then 
                        aliased(i)=.true.
                        aliased(k)=.true.
                        xaliased(i,k)=.true.
                     endif
                  endif
               enddo
            endif
         endif
      enddo

      if(any_aliased) then
         do i=1,nargs
            arg=node_arg(node,i)
            if(aliased(i)) then
               call trav_reference(coder,cblock,node,arg,is_amp(i),.true.,.true.)
            elseif(is_amp(i)) then
               call trav_reference(coder,cblock,node,arg,.true.,.true.,.false.)
               call code_null(coder)
            else
               call trav_expr(coder,cblock,node,arg)
               call code_null(coder)
            endif
         enddo
         do i=1,nargs
            if(is_amp(i).and.aliased(i)) then
               do j=1,nargs
                  if(i/=j.and..not.(is_amp(j).and.j>i)) then
                     if(xaliased(k,j)) then
                        call code_val(coder,coder%vstack(base+i*2-1))
                        call code_val(coder,coder%vstack(base+j*2-1))
                        call make_sys_call(coder,cblock,node_arg(node,i),&
                             sym_check_alias,2,0)
                     endif
                  endif
               enddo
            endif
         enddo
         do i=1,nargs
            coder%vstack(base+i)=coder%vstack(base+2*i)
         enddo
         coder%vtop=coder%vtop-nargs
      else
         do i=1,nargs
            arg=node_arg(node,i)
            if(is_amp(i)) then
               call trav_reference(coder,cblock,node,arg,.true.,.true.,.false.)
            else
               call trav_expr(coder,cblock,node,arg)
            endif
         enddo
      endif
    end subroutine process_amp_args
    
  end subroutine trav_call

  !=================================================
  ! Make an alias check - PM__alias_check(p1,p2)
  !=================================================
  subroutine make_alias_check(coder,cblock,node,p1,p2)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    type(pm_ptr),intent(in):: p1,p2
    call dup_expr(coder,p1)
    call dup_expr(coder,p2)
    call make_sys_call(coder,cblock,node,sym_check_alias,2,0)
  end subroutine make_alias_check
  

  !===============================================================
  ! Traverse procedure definition
  !===============================================================
  recursive subroutine trav_proc(coder,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node

    integer:: nargs,nret
    type(pm_ptr):: cblock,cblock2
    type(pm_ptr):: p,amp,keycall,argcall
    type(pm_ptr),target:: tkeys
    integer:: i,j,base,obase,wbase,npars,cbase
    integer:: flags,sym,rsig
    integer:: save_index,save_proc_base,save_proc_ncalls,&
         save_lex_scope,save_par_state,&
         save_state_base,save_mask

    integer:: pr_flags
    type(pm_reg),pointer:: reg
    integer,save:: pdepth=0

    nargs=node_numargs(node_get(node,proc_params))/2
    nret=node_get_num(node,proc_numret)
    flags=node_get_num(node,proc_flags)

    if(debug_codegen) then
       write(*,*) repeat(' ',pdepth),'TRAV PROC>',&
            trim(pm_name_as_string(coder%context,&
            node_get_num(node,proc_name))),&
            node_get_lineno(node),coder%wtop,coder%top
       pdepth=pdepth+1
    endif

    !nkeyargs=0

    ! Parameter types
    wbase=coder%wtop
    obase=coder%vtop
    
    call code_num(coder,proc_param_type(coder,node))
    call code_num(coder,proc_result_type(coder,node))
    call code_num(coder,nargs)
    call code_num(coder,nret)
    call code_num(coder,flags)
    call code_val(coder,node_get(node,proc_amplocs))
    call code_val(coder,node_get(node,proc_name))

    sym=node_sym(node)
    if(sym==sym_builtin) then

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
          coder%id=coder%id+1
          call code_num(coder,coder%id)
          call make_code(coder,node,cnode_is_builtin,bi_node_size)
       end if
    else
       ! User-defined procedure

       ! Check if cached
       p=node_get(node,proc_code_tree)
       if(.not.pm_fast_isnull(p)) then
          call code_val(coder,p)
          call pop_type_vars(coder)
          return
       endif

       call save_proc_state
       call init_proc_state

       ! Set up code block and imports
       cblock=make_cblock(coder,pm_null_obj,node,sym_proc)


       reg=>pm_register(coder%context,'tproc',tkeys)

       ! Different types of procedure
       npars=0
       flags=node_get_num(node,proc_flags)
       pr_flags=flags
       if(iand(flags,proc_run_shared+proc_run_local+proc_run_complete)/=0) then
          call code_params(cblock,.true.,argcall)
          call export_params(cblock)
          call code_keys(cblock,tkeys,keycall,.true.,.true.)
          call code_special_check_body_and_result(cblock)
       elseif(iand(flags,proccall_is_comm)/=0) then
          coder%par_state=merge(par_state_comm_proc,par_state_none,&
               iand(flags,proc_is_uncond)==0)
          call code_params(cblock,.true.,argcall)
          call code_keys(cblock,tkeys,keycall,.true.,.false.)
          call code_loop_check_body_and_result(cblock)
       else
          coder%par_state=par_state_none
          call code_params(cblock,.false.,argcall)
          call make_state_vars(coder,cblock,node,&
               topo=coder%var(coder%proc_base+1))
          call code_keys(cblock,tkeys,keycall,.false.,.false.)
          call code_check(cblock)
          call code_body(cblock)
          call code_result(cblock,flags)
       endif

       call close_cblock(coder,cblock)
       ! Create proc code object 
       call code_num(coder,coder%index)                   ! Maximum index
       call code_num(coder,0)                             ! Recursion flag
       coder%id=coder%id+1
       call code_num(coder,coder%id)                      ! Procedure id. index
       call code_num(coder,coder%proc_ncalls)             ! Number of calls
       call code_val(coder,tkeys)                         ! Keyword arg info
       call code_val(coder,keycall)                       ! Keyword call
       call code_val(coder,argcall)                       ! Arguments call
       if(.not.pm_fast_isnull(node_get(node,proc_when))) then
          cblock2=make_cblock(coder,cblock,node,sym_when)
          call trav_xexpr(coder,cblock2,node,node_get(node,proc_when))
          call close_cblock(coder,cblock2)
       else
          call code_null(coder)
          call code_null(coder)
       endif
       call make_code(coder,node,cnode_is_proc,pr_node_size)

       call pm_delete_register(coder%context,reg)
       
       call restore_proc_state

    endif

    if(pm_debug_checks) then
       if(coder%vtop/=obase+1) then
          call pm_panic('trav_proc: vstack')
       endif
       if(coder%wtop/=wbase) then
          call pm_panic('trav_proc: vstack')
       endif
    endif

    if(debug_codegen) then
       pdepth=pdepth-1
       write(*,*) repeat(' ',pdepth),'END TRAV PROC>',&
            trim(pm_name_as_string(coder%context,&
            node_get_num(node,proc_name))),coder%wtop,coder%top
       !call dump_parse_tree(coder%context,6,node,2) 
    endif

  contains
    include 'fisnull.inc'
    include 'fistiny.inc'
    include 'fesize.inc'
    include 'fname.inc'
    include 'ftiny.inc'
    include 'fnewnc.inc'
    include 'fisname.inc'
    include 'fvkind.inc'

    subroutine save_proc_state
      save_index=coder%index
      save_lex_scope=coder%lex_scope
      save_proc_base=coder%proc_base
      save_proc_ncalls=coder%proc_ncalls
      save_par_state=coder%par_state
      save_state_base=coder%state_base
      save_mask=coder%mask
    end subroutine save_proc_state

    subroutine init_proc_state
      coder%index=0
      coder%lex_scope=0
      coder%proc_base=coder%top
      coder%proc_ncalls=0
    end subroutine init_proc_state

    subroutine restore_proc_state
      coder%index=save_index
      coder%lex_scope=save_lex_scope
      coder%proc_base=save_proc_base
      coder%proc_ncalls=save_proc_ncalls
      coder%par_state=save_par_state
      coder%state_base=save_state_base
      coder%mask=save_mask
    end subroutine restore_proc_state

    subroutine code_params(cblock,iscomm,argcall)
      type(pm_ptr),intent(in):: cblock
      logical,intent(in):: iscomm
      type(pm_ptr),intent(out):: argcall
      type(pm_ptr):: p
      integer:: name,flags,flags0
      if(iscomm) then
         coder%state_base=coder%top
         coder%mask=coder%state_base+num_comm_args
      endif
      p=node_get(node,proc_params)
      flags0=merge(var_is_maybe_not_private,0,iscomm)
      if(.not.pm_fast_isnull(p)) then
         amp=node_get(node,proc_amplocs)
         if(pm_fast_isnull(amp)) then
            do i=1,node_numargs(p),2
               flags=flags0+var_is_param
               name=node_num_arg(p,i)
               if(name==sym_dotdotdot) flags=var_is_varg
               call make_var(coder,cblock,p,name,flags)
            enddo
         else
            j=0
            amp=pm_name_val(coder%context,int(amp%offset))
            do i=1,node_numargs(p),2
               if(amp%data%i(amp%offset+j)==(i+1)/2) then
                  flags=flags0+var_is_ref+var_is_param+var_is_var
                  if(node_sym(node_arg(p,i+1))/=sym_pm_dref) then
                     flags=ior(flags,var_is_ref)
                  endif
                  if(j<pm_fast_esize(amp)) j=j+1
               else
                  flags=flags0+var_is_param
               endif
               name=node_num_arg(p,i)
               if(name==sym_dotdotdot) flags=var_is_varg
               call make_var(coder,cblock,p,name,flags)
            enddo
         endif
         npars=npars+node_numargs(p)/2
         call make_basic_sp_call(coder,cblock,p,&
              sym_open,npars,0)
         argcall=cnode_get(cnode_get(cblock,cblock_last_call),call_args)
      else
         argcall=pm_null_obj
      endif
    end subroutine code_params
  
    recursive subroutine code_keys(cblock,tkeys,key_call,iscomm,isshrd)
      type(pm_ptr),intent(in):: cblock
      type(pm_ptr),intent(inout):: key_call
      type(pm_ptr),intent(inout),target:: tkeys
      logical,intent(in):: iscomm,isshrd
      type(pm_ptr):: p,typ,cblock2
      integer:: i,n,base,newbase,vname,vbase,wbase,tno,flags0

      flags0=merge(var_is_maybe_not_private,0,iscomm)

      p=node_get(node,proc_keys)
      if(pm_fast_isnull(p)) then
         tkeys=pm_null_obj
         key_call=pm_null_obj
         return
      endif
      n=node_numargs(p)/3
      vbase=coder%vtop
      base=coder%top

      ! Create actual keyword parameter variables
      wbase=coder%wtop
      do i=1,node_numargs(p),3
         vname=node_num_arg(p,i)
         call push_word(coder,vname)
         call make_var(coder,cblock,p,vname,&
              flags0+var_is_param+var_is_key+var_is_multi_access)
      enddo

      ! Export parameters for a gbl proc
      if(isshrd) then
         newbase=coder%top
         do i=1,node_numargs(p)/3
            call make_var(coder,cblock,p,vname,&
                 flags0+var_is_param+var_is_key+var_is_multi_access+var_is_shadowed)
            call code_val(coder,coder%var(base+i))
            call make_sys_call(coder,cblock,node,sym_export_param,1,1)
         enddo
         base=newbase
      endif

      ! Create a vector of all key names followed by all key types
      ! and finally by largest index associated with keys
      do i=1,node_numargs(p),3
         typ=node_arg(p,i+1)
         if(pm_fast_isnull(typ)) then
            call push_word(coder,-1)
         else
            call trav_type(coder,p,typ)
         endif
      enddo
      tkeys=pm_fast_newnc(coder%context,pm_int,coder%wtop-wbase+1)
      tkeys%data%i(tkeys%offset:tkeys%offset+coder%wtop-wbase-1)=&
           coder%wstack(wbase+1:coder%wtop)
      coder%wtop=wbase

      ! Create visible keyword parameters
      do i=1,node_numargs(p),3
         vname=node_num_arg(p,i)
         call make_var(coder,cblock,p,vname,&
              flags0+var_is_key+var_is_multi_access+var_is_shadowed,&
              extra_info=coder%var(base+(i+2)/3))
      enddo
      
      call hide_vars(coder,base+1,coder%top)

      ! Create blocks to compute default values
      do i=1,node_numargs(p),3
         cblock2=make_cblock(coder,cblock,node,sym_key)
         call trav_expr(coder,cblock2,p,node_arg(p,i+2))
         tno=tkeys%data%i(tkeys%offset+n+i-1)
         ! For stated type constraints, convert default value to
         ! that type
         if(tno>=0) then
            call make_const(coder,cblock2,node,&
                 pm_fast_tinyint(coder%context,tno))
            call make_sp_call_rtn(coder,cblock2,node,sym_type_val,1,1)
            call make_sp_call_rtn(coder,cblock2,node,sym_cast,2,1)
         endif
         call close_cblock(coder,cblock2)
         call reveal_vars(coder,base+n+(i+2)/3,base+n+(i+2)/3)
      enddo

      ! Create call: key keyarg... keyvar... (block defvar)...
      call make_sp_call(coder,cblock,node,sym_key,n*2,n*2)
      key_call=cnode_get(cnode_get(cblock,cblock_last_call),call_args)

      ! Last index used by default expressions
      tkeys%data%i(tkeys%offset+pm_fast_esize(tkeys))=coder%index
      
    end subroutine code_keys

    recursive subroutine code_check(cblock)
      type(pm_ptr),intent(in):: cblock
      ! Check expression
      p=node_get(node,proc_check)
      if(.not.pm_fast_isnull(p)) then
         base=coder%vtop
         call trav_xexpr(coder,cblock,node,p)
      endif
    end subroutine code_check

    recursive subroutine code_body(cblock)
      type(pm_ptr),intent(in):: cblock
      ! Body of statements
      p=node_get(node,proc_stmts)
      if(.not.pm_fast_isnull(p)) then
         call trav_open_stmt_list(coder,cblock,node,p)
      endif
    end subroutine code_body

    recursive subroutine code_result(cblock,flags)
      type(pm_ptr),intent(in):: cblock
      integer,intent(in):: flags
      type(pm_ptr):: p
      integer:: i

      ! Result expression
      p=node_get(node,proc_result)
      if(.not.pm_fast_isnull(p)) then
         base=coder%vtop
         call trav_xexpr(coder,cblock,node,p)
         if(iand(flags,proc_run_shared+proc_run_local)/=0) then
            do i=coder%vtop+1-nret,coder%vtop
               call make_temp_var(coder,cblock,node)
               call dup_code(coder)
               call code_val(coder,coder%vstack(i))
               call make_comm_sys_call(coder,cblock,node,sym_import_param,1,1)
               coder%vstack(i)=pop_code(coder)
            enddo
         end if
         call make_sp_call(coder,cblock,node,&
              sym_result,nret,0)
         rsig=pop_word(coder)
         if(pm_debug_checks) then
            if(coder%vtop/=base) then
               write(*,*) '***************',nret
               do i=base+1,coder%vtop
                  call qdump_code_tree(coder,pm_null_obj,6,&
                       coder%vstack(i),2)
               enddo
               write(*,*) coder%vtop,base
               write(*,*) '%%%%%%%%%%%%'
               call dump_parse_tree(coder%context,6,p,2)
               call pm_panic('rtn mismatch')
            endif
         endif
      else
         rsig=0
      endif
    end subroutine code_result

    subroutine code_special_check_body_and_result(cblock)
      type(pm_ptr),intent(in):: cblock
      type(pm_ptr):: cblock2
      integer:: sym
      if(iand(flags,proc_run_shared+proc_run_local)/=0) then
         sym=merge(sym_pm_shared_always,sym_pm_shared,iand(flags,proc_run_always)/=0)
      else
         sym=merge(sym_pm_chan_always,sym_pm_chan,iand(flags,proc_run_always)/=0)
      endif
      call code_val(coder,coder%var(coder%mask))
      cblock2=make_cblock(coder,cblock,node,sym)
      call code_check(cblock2)
      coder%par_state=par_state_none
      call code_body(cblock2)
      call import_params(cblock2)
      call code_result(cblock2,flags)
      call close_cblock(coder,cblock2)
      call make_sp_call(coder,cblock,node,sym,2,0)
    end subroutine code_special_check_body_and_result

    subroutine export_params(cblock)
      type(pm_ptr),intent(in):: cblock
      integer:: i
      type(pm_ptr):: var,p
      p=node_get(node,proc_params)
      call hide_vars(coder,coder%state_base+1,coder%state_base+num_comm_args)
      do i=num_comm_args+1,npars
         var=coder%var(coder%state_base+i)
         call make_var(coder,cblock,p,cnode_get_num(var,var_name),&
              ior(iand(cnode_get_num(var,var_flags),&
              var_is_var+var_is_ref),var_is_shadowed))
         call code_val(coder,var)
         call make_comm_sys_call(coder,cblock,p,sym_export_param,&
              1,1)
      enddo
    end subroutine export_params

    subroutine import_params(cblock)
      type(pm_ptr),intent(in):: cblock
      integer:: i,j
      type(pm_ptr):: amp,p
      p=node_get(node,proc_params)
      amp=node_get(node,proc_amplocs)
      if(.not.pm_fast_isnull(amp)) then
         amp=pm_name_val(coder%context,int(amp%offset))
         do j=0,pm_fast_esize(amp)
            i=amp%data%i(amp%offset+j)
            call code_val(coder,coder%var(coder%state_base+i))
            call code_val(coder,coder%var(coder%state_base+npars-num_comm_args+i))
            call make_comm_sys_call(coder,cblock,p,sym_import_param,2,0,assign=.true.)
         enddo
      endif
    end subroutine import_params

    subroutine code_loop_check_body_and_result(cblock)
      type(pm_ptr),intent(in):: cblock
      type(pm_ptr):: cblock2
      call code_val(coder,coder%var(coder%state_base+4))
      cblock2=make_cblock(coder,cblock,node,sym_pct)
      call code_check(cblock2)
      call code_body(cblock2)
      call code_result(cblock2,flags)
      call close_cblock(coder,cblock2)
      call make_sp_call(coder,cblock,node,sym_pct,2,0)
    end subroutine code_loop_check_body_and_result

  end subroutine trav_proc


  !========================================================
  ! Traverse a procedure parameter list
  ! !!! stuff to say
  !========================================================
   subroutine trav_params(coder,cblock,paramlist,amps,step,pre_args)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,paramlist
    integer,intent(in):: amps,step,pre_args

    integer:: i,j,k,flags,flags0,nargs,name
    type(pm_ptr):: amp
    flags0=var_is_param+var_is_maybe_not_private
    nargs=node_numargs(paramlist)
    if(amps==0) then
       do i=1,nargs,step
          flags=flags0
          name=node_num_arg(paramlist,i)
          if(name==sym_dotdotdot) flags=flags0+var_is_varg
          call make_sys_var(coder,cblock,paramlist,name,flags)
       enddo
    else
       j=0
       k=0
       amp=pm_name_val(coder%context,amps)
       do i=1,nargs,step
          k=k+1
          if(amp%data%i(amp%offset+j)==k) then
             flags=var_is_ref+var_is_param+var_is_var
             if(j<pm_fast_esize(amp)) j=j+1
          else
             flags=flags0
          endif
          name=node_num_arg(paramlist,i)
          if(name==sym_dotdotdot) flags=flags0+var_is_varg
          call make_sys_var(coder,cblock,paramlist,name,flags)
       enddo
    endif
    call make_basic_sp_call(coder,cblock,paramlist,&
         sym_open,nargs/step+pre_args,0)
  contains
    include 'fesize.inc'
  end subroutine trav_params

  !========================================================
  ! Create a procedure constant
  !========================================================
  subroutine proc_const(coder,cblock,pnode,pr)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,pr
    type(pm_ptr):: p
    integer:: name

    if(node_numargs(pr)==1) then
       p=find_decl(coder,pnode,node_num_arg(pr,1),modl_proc)
       name=node_num_arg(pr,1)
    else
       p=find_imported_decl(coder,pnode,&
            node_num_arg(pr,1),node_num_arg(pr,2),modl_proc)
       name=node_num_arg(pr,2)
    endif
    if(pm_fast_isnull(p)) then
       call code_error(coder,pnode,&
            'proc value not associated with any defined procedure: ',name)
       call make_temp_var(coder,cblock,pnode)
       return
    endif
    call proc_const_from_decl(coder,cblock,pnode,p)
  contains
    include 'fisnull.inc'
  end subroutine proc_const

  !===========================================================
  ! Create a procedure constant from a given proc declaration
  !===========================================================
  subroutine proc_const_from_decl(coder,cblock,node,p)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,p
    type(pm_ptr):: namep,sig
    namep=node_get(p,proc_name)
    call make_const(coder,cblock,node,namep,&
         proc_type_from_decl(coder,p,node))
    sig=find_sig(coder,node,namep,p)
  end subroutine proc_const_from_decl

  !========================================================
  ! Returns proc type for a given procedure declaration
  !========================================================
  function proc_type_from_decl(coder,node,cnode) result(proctyp)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,cnode
    integer:: proctyp
    integer:: name,n
    type(pm_ptr):: p
    p=node_arg(node,1)
    name=p%offset
    call push_word(coder,pm_type_new_proc)
    call push_word(coder,name)
    n=2
    p=node_arg(node,2)
    do while(.not.pm_fast_isnull(p))
       call push_word(coder,&
            proc_type_from_single_decl(coder,p,cnode))
       p=node_get(p,proc_link)
       n=n+1
    enddo
    call make_type(coder,n)
    proctyp=pop_word(coder)
  contains
    include 'fisnull.inc'
  end function proc_type_from_decl

  !============================================================
  ! Returns proc type for a single declaration
  ! This is type of the form proc.(  )->( )
  ! (caches result type in proc_coded_type in proc parse node)
  !============================================================
  function proc_type_from_single_decl(coder,node,cnode) result(partyp)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,cnode
    integer:: partyp
    integer:: flags,sym,wbase

    partyp=node_get_num(node,proc_coded_type)
    if(partyp<0) then
       wbase=coder%wtop
       flags=node_get_num(node,proc_flags)
       if(iand(flags,proccall_is_comm)/=0) then
          sym=sym_pct
       else
          sym=sym_proc
       endif
       call push_word(coder,pm_type_new_proc_sig)
       call push_word(coder,sym)
       call push_word(coder,proc_param_type(coder,node))
       call push_word(coder,proc_result_type(coder,node))
       call make_type(coder,4)
       partyp=pop_word(coder)
       coder%wtop=wbase
       call node_set_num(node,proc_coded_type,int(partyp))
       if(.not.pm_fast_isnull(node_get(node,proc_keys))) then
          call code_error(coder,cnode,&
               'Cannot have proc() value of procedure with keyword arguments for any variant')
          call code_error(coder,node,'... problematic procedure definition for above')
       endif
    endif

  contains
    include 'fisnull.inc'
    include 'fesize.inc'
  end function proc_type_from_single_decl

  !========================================================
  ! Get parameter types for a procedure (as tuple type)
  !========================================================
  function proc_param_type(coder,node) result(tno)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer:: tno
    type(pm_ptr):: p,amp,arg
    integer:: i,n,when

    if(node_sym(node)==sym_proc) then
       when=merge(0,pm_type_is_when,pm_fast_isnull(node_get(node,proc_when)))
    else
       when=0
    endif
    p=node_get(node,proc_params)
    call push_word(coder,merge(pm_type_is_vtuple,pm_type_is_tuple,&
         node_sym(p)==sym_dotdotdot)+when)
    amp=node_get(node,proc_amplocs)
    if(pm_fast_isnull(amp)) then
       call push_word(coder,0)
    else
       call push_word(coder,int(amp%offset))
    endif
    n=node_numargs(p)
    do i=2,n,2
       arg=node_arg(p,i)
       call trav_type(coder,arg,arg)
    enddo
    call make_type(coder,n/2+2)
    tno=pop_word(coder)

  contains
    include 'fisnull.inc'
  end function proc_param_type

  !========================================================
  ! Get return types for a procedure (as tuple type)
  !========================================================
  function proc_result_type(coder,node) result(tno)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer:: tno
    integer:: nret,i
    type(pm_ptr):: p,arg
 
    p=node_get(node,proc_result_types)
    if(node_sym(node)==sym_builtin.and.node_sym(p)==sym_dash) then
       p=node_arg(p,1)
    endif
    if(pm_fast_isnull(p)) then
       nret=node_get_num(node,proc_numret)
       if(nret==0) then
          call push_word(coder,pm_type_is_tuple)
          call push_word(coder,0)
       else
          call push_word(coder,pm_type_is_undef_result)
          call push_word(coder,nret)
       endif
       call make_type(coder,2)
    else
       if(node_sym(p)==sym_dash) then
          p=node_arg(p,1)
       endif
       call push_word(coder,pm_type_is_tuple)
       call push_word(coder,0)
       nret=node_numargs(p)
       do i=1,nret
          arg=node_arg(p,i)
          call trav_type(coder,arg,arg)
       enddo
       call make_type(coder,nret+2)   
    endif
    tno=pop_word(coder)
  contains
    include 'fisnull.inc'
  end function proc_result_type

  recursive function find_sig(coder,node,pname,pdef) result(sig)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,pname
    type(pm_ptr),intent(in),optional:: pdef
    type(pm_ptr)::sig
    type(pm_ptr):: procdef,proc
    integer:: base,args(1),signo

    if(present(pdef)) then
       procdef=pdef
    else
       procdef=find_decl(coder,node,int(pname%offset),modl_proc)
       if(pm_fast_isnull(procdef)) then
          call code_error(coder,node,&
               'Cannot find procedure: ',int(pname%offset))
          sig=pm_null_obj
          return
       endif
    endif
    
    args(1)=node_get_num(procdef,proc_name)
    signo=pm_ivect_lookup(coder%context,coder%sig_cache,&
         args,1)
    if(signo>0) then
       sig=pm_fast_tinyint(coder%context,signo)
       return
    endif
    
    call make_code(coder,node,cnode_is_callsig,0)
    signo=pm_idict_add(coder%context,coder%sig_cache,&
         args,1,pop_code(coder))
    
    base=coder%vtop
    proc=node_arg(procdef,2)
    do
       call trav_proc(coder,proc)
       proc=node_get(proc,proc_link)
       if(pm_fast_isnull(proc)) exit
    enddo
    call make_code(coder,node,cnode_is_callsig,coder%vtop-base)
    signo=pm_idict_add(coder%context,coder%sig_cache,&
         args,1,top_code(coder))
    call drop_code(coder)
    sig=pm_fast_tinyint(coder%context,signo)
  contains
    include 'fisnull.inc'
    include 'ftiny.inc'
  end function find_sig
  
  !===========================================
  ! Sort all defined signatures
  !===========================================
  subroutine sort_sigs(coder)
    type(code_state),intent(inout):: coder
    integer(pm_ln):: i
    type(pm_ptr):: vals,v
    if(debug_codegen) then
       write(*,*) 'SORT SIGS',&
            pm_dict_size(coder%context,coder%sig_cache)
    endif
    vals=pm_dict_vals(coder%context,coder%sig_cache)
    do i=0,pm_dict_size(coder%context,coder%sig_cache)-1
       v=vals%data%ptr(vals%offset+i)
       call sort_sig(coder,v,int(i+1))
    enddo
  end subroutine sort_sigs

  !=================================================
  ! Partial order sort for signature
  !=================================================
  subroutine sort_sig(coder,sig,signo)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: sig
    integer,intent(in):: signo
    integer:: i,j,typ1,typ2
    type(pm_ptr):: proc1,proc2

    if(debug_codegen) write(*,*) 'SORT SIGNATURE>'
       
    do i=cnode_numargs(sig),1,-1
       proc1=cnode_arg(sig,i)
       typ1=cnode_get_num(proc1,pr_ptype)
       j=i+1
       do while(j<=cnode_numargs(sig))
          proc2=cnode_arg(sig,j)
          typ2=cnode_get_num(proc2,pr_ptype)

          if(debug_codegen) then
             write(*,*) 'COMPARE SIGS>',typ1,typ2
             write(*,*) '--------------------------------------'
             write(*,*) trim(pm_type_as_string(coder%context,typ2))
             write(*,*) trim(pm_type_as_string(coder%context,typ1))
             write(*,*) '--------------------------------------'
          endif
          if(cnode_get_num(proc1,pr_nret)/=cnode_get_num(proc2,pr_nret).or.&
               iand(cnode_get_num(proc1,pr_flags),proccall_is_comm+proc_is_cond)/=&
               iand(cnode_get_num(proc2,pr_flags),proccall_is_comm+proc_is_cond).or.&
               iand(cnode_get_num(proc1,pr_flags),proccall_is_comm+proc_is_uncond)/=&
               iand(cnode_get_num(proc2,pr_flags),proccall_is_comm+proc_is_cond)) then
             if(debug_more_codegen) write(*,*) 'SIG DIFFERENT'
             sig%data%ptr(sig%offset+cnode_args+j-2)=proc2
             j=j+1
          else if(pm_type_includes(coder%context,typ2,typ1,pm_type_incl_type)) then
             if(pm_type_includes(coder%context,typ1,typ2,pm_type_incl_type)) then
                if(.not.pm_type_has_when(coder%context,typ2)) then
                   call cnode_error(coder,proc1,&
                        'Procedure "'//trim(sig_name_str(coder,signo))//&
                        '" defined with identical signatures:'//&
                        trim(pm_type_as_string(coder%context,typ2)))
                   call cnode_error(coder,proc2,'Conflicting definition')
                   return
                else
                   sig%data%ptr(sig%offset+cnode_args+j-2)=proc2
                   j=j+1
                endif
             else
                if(debug_more_codegen) write(*,*) 'SIG INCL'
                call check_nesting(proc1,proc2)
                exit
             endif
          else
             if(debug_more_codegen) write(*,*) 'SIG NOT INCL'
             if(pm_type_includes(coder%context,typ1,typ2,pm_type_incl_type)) then
                call check_nesting(proc2,proc1)
             endif
             sig%data%ptr(sig%offset+cnode_args+j-2)=proc2
             j=j+1
          endif
       enddo
       sig%data%ptr(sig%offset+cnode_args+j-2)=proc1
    enddo

  contains
    include 'fesize.inc'

    subroutine check_nesting(first,second)
      type(pm_ptr),intent(in):: first,second
      logical:: isbad
      integer:: ret1,ret2,rtype1,rtype2,ii
      type(pm_ptr):: tv1,tv2

      if(cnode_flags_clear(second,&
           pr_flags,proc_is_open)) then
         if(.not.(cnode_get(first,cnode_modl_name)==&
              cnode_get(second,cnode_modl_name))) then
            call cnode_error(coder,first,&
                 'Attempt to specialise procedure defined without "..." across modules')
            call cnode_error(coder,second,&
                 'Conflicting definition')
         endif
      endif

      ret1=cnode_get_num(second,pr_rtype)
      ret2=cnode_get_num(first,pr_rtype)
      tv1=pm_type_vect(coder%context,ret1)
      tv2=pm_type_vect(coder%context,ret2)
      if(pm_tv_kind(tv1)/=pm_type_is_undef_result.and.&
           pm_tv_kind(tv2)/=pm_type_is_undef_result) then
         isbad=.false.
         do ii=1,pm_tv_numargs(tv1)
            rtype1=pm_tv_arg(tv1,ii)
            rtype2=pm_tv_arg(tv2,ii)
            if(.not.pm_type_includes(coder%context,&
                 rtype1,rtype2,pm_type_incl_type)) then
               if(.not.isbad) then
                  call cnode_error(coder,first,&
                       'Procedure "'//trim(sig_name_str(coder,signo))//&
                       '" specialises a procedure with incompatible return types')
               endif
               call more_error(coder%context,'Return value #'//&
                    trim(pm_int_as_string(ii))//&
                    ' in original procedure has type: '//&
                    trim(pm_type_as_string(coder%context,rtype1)))
               call more_error(coder%context,&
                    'but in this procedure has type: '//&
                    trim(pm_type_as_string(coder%context,rtype2)))
               isbad=.true.

            endif
         enddo
         if(isbad) then
            call cnode_error(coder,second,&
                 'Original procedure in above error')
         endif
      endif

    end subroutine check_nesting

  end subroutine sort_sig
  
  
  !***********************************************************
  ! SERVICE ROUTINES
  !***********************************************************
  

  !========================================================
  ! Find a parameter
  !========================================================
  recursive function find_param(coder,cblock,node,name,name2) result(v)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: name
    integer,intent(in),optional:: name2
    type(pm_ptr):: v
    type(pm_ptr):: p
    if(present(name2)) then
       p=find_imported_decl(coder,node,name,name2,modl_param,.true.)
    else
       p=find_decl(coder,node,name,modl_param)
    endif
    if(pm_fast_isnull(p)) then
       v=pm_null_obj
       return
    endif
    if(p%data%ptr(p%offset+node_args)%offset/=0) then
       call code_error(coder,node,'Parameter definition cannot be recursive:',name)
       v=pm_null_obj
       return
    endif
    p%data%ptr(p%offset+node_args)%offset=1
    call trav_closed_expr(coder,cblock,node,node_arg(p,2))
    p%data%ptr(p%offset+node_args)%offset=0
    v=pop_code(coder)
  contains
    include 'fisnull.inc'
  end function find_param


  !========================================================
  ! Find declaration
  !========================================================
  function find_decl(coder,node,name,where) result(ptr)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: name,where
    type(pm_ptr):: ptr
    type(pm_ptr):: v,modl
    modl=node_get_modl(node)
    v=pm_dict_lookup(coder%context,modl%data%ptr(modl%offset+where),&
         pm_fast_name(coder%context,name))
    if(pm_fast_isnull(v)) then
       v=pm_dict_lookup(coder%context,&
            modl%data%ptr(modl%offset+where+modl_local),&
            pm_fast_name(coder%context,name))
       if(pm_fast_isnull(v)) then
          ptr=v
          return
       endif
    endif
    ptr=v%data%ptr(v%offset)
  contains
    include 'fisnull.inc'
    include 'fname.inc'
  end function find_decl

  !========================================================
  ! Find declaration correspoding to name::name
  !========================================================
  function find_imported_decl(coder,node,name1,name2,where,noerr) result(p)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: name1,name2
    integer,intent(in):: where
    logical,intent(in),optional:: noerr
    type(pm_ptr):: p
 
    type(pm_ptr):: thismodl,modl
    character(len=5):: str
    thismodl=node_get_modl(node)
    modl=pm_dict_lookup(coder%context,&
         thismodl%data%ptr(thismodl%offset+modl_include),&
         pm_fast_name(coder%context,name1))
    if(pm_fast_isnull(modl)) then
       call code_error(coder,node,'No such module: ',name1)
       p=pm_null_obj
    else
       modl=node_arg(modl,2)
       p=pm_dict_lookup(coder%context,&
            modl%data%ptr(modl%offset+where),pm_fast_name(coder%context,name2))
       if(pm_fast_isnull(p)) then
          if(.not.present(noerr)) then
             str='proc'
             if(where==modl_type) then
                str='type'
             elseif(where==modl_param) then
                str='param'
             endif
             call code_error(coder,node,'Cannot find '//str//' '//&
                  trim(pm_name_as_string(coder%context,name2))//&
                  ' in: ',name1)
          endif
       else
          p=p%data%ptr(p%offset)
       endif
    endif
  contains
    include 'fisnull.inc'
    include 'fname.inc'
  end function find_imported_decl


  !=======================================================================
  ! Make type using size elements from wstack and push it on wstack
  !=======================================================================
  subroutine make_type(coder,size,val)
    type(code_state),intent(inout):: coder
    integer,intent(in):: size
    type(pm_ptr),intent(in),optional:: val
    coder%wtop=coder%wtop-size+1
    if(pm_debug_checks) then
       if(coder%wtop<1) call pm_panic('make type')
    endif
    coder%wstack(coder%wtop)=&
         pm_new_type(coder%context,coder%wstack(coder%wtop:coder%wtop+size-1),&
         val)
  end subroutine make_type

  !========================================================================
  ! Make type using size elements from wstack and push it on wstack
  ! (unlike make_type does not accumulate flags)
  !========================================================================
  subroutine make_basic_type(coder,size,val)
    type(code_state),intent(inout):: coder
    integer,intent(in):: size
    type(pm_ptr),intent(in),optional:: val
    coder%wtop=coder%wtop-size+1
    if(pm_debug_checks) then
       if(coder%wtop<1) call pm_panic('make type')
    endif
    coder%wstack(coder%wtop)=&
         pm_new_basic_type(coder%context,coder%wstack(coder%wtop:coder%wtop+size-1),&
         val)
  end subroutine make_basic_type

  !===========================================================
  ! Make reference to user defined type : name(params)
  !===========================================================
  function make_user_type(coder,n,tno) result(new_type)
    type(code_state),intent(inout):: coder
    integer,intent(in):: n
    integer,intent(in):: tno
    integer:: new_type
    integer:: deftyp
    deftyp=pm_type_lookup(coder%context,coder%wstack(coder%wtop-n+1:coder%wtop))
    if(deftyp>=0) then
       new_type=-1
    else
       new_type=pm_new_type(coder%context,coder%wstack(coder%wtop-n+1:coder%wtop),&
            val=pm_fast_typeno(coder%context,tno))
    endif
  contains
    include 'ftypeno.inc'
  end function make_user_type


  !===================================
  ! Find a variable
  !===================================
  function find_var(coder,name) result(v)
    type(code_state),intent(inout):: coder
    integer,intent(in):: name
    type(pm_ptr):: v
    integer:: i
    integer::n
    i=find_var_entry(coder,name,coder%proc_base)
    if(i/=0) then
       v=coder%var(i)
       if(i<=coder%block_base) then
          if(debug_more_codegen) then
             write(*,*) 'importing>',trim(pm_name_as_string(coder%context,n))
          endif
          call import_to_block_scope(coder,i,v,coder%block_entry)
          coder%var(i)=v
       endif
    else
       v=pm_null_obj
    endif
    return
  end function find_var

  !=========================================
  ! Find a system variable which must exist
  !==========================================
  function find_sys_var(coder,node,name) result(v)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: name
    type(pm_ptr):: v
    integer::i
    v=find_var(coder,name)
    if(pm_debug_checks) then
       if(pm_fast_isnull(v)) then
          call code_error(coder,node,'Internal_error - cannot find sys var: '//&
               trim(pm_name_as_string(coder%context,name)))
          do i=1,coder%top
             write(*,*) '>>',trim(pm_name_as_string(coder%context,coder%stack(i)))
          enddo
          call pm_panic('ooo')
       endif
    endif
  contains
    include 'fname.inc'
    include 'fisnull.inc'
  end function find_sys_var

  
  !==========================================
  ! Find a variable and its table entry
  !==========================================
  function find_var_and_entry(coder,name,i) result(v)
    type(code_state),intent(inout):: coder
    integer,intent(in):: name
    integer,intent(out):: i
    type(pm_ptr):: v
    integer::n
    n=name
    i=find_var_entry(coder,n,coder%proc_base)
    if(i/=0) then
       v=coder%var(i)
       if(i<=coder%block_base) then
          call import_to_block_scope(coder,i,v,coder%block_entry)
          coder%var(i)=v
       endif
    else
       v=pm_null_obj
    endif
    return
  end function find_var_and_entry

  !========================================
  ! Find variable entry in hash table
  !========================================
  function find_var_entry(coder,n,base) result(index)
    type(code_state),intent(inout):: coder
    integer,intent(in):: n
    integer,intent(in):: base
    integer:: index
    integer:: i
    index=0
    do i=coder%top,base+1,-1
       if(coder%stack(i)==n) then
          index=i
          return
       endif
    enddo    
  end function find_var_entry

  !===================================================
  ! Hide a block of variables from name searches
  !===================================================
  subroutine hide_vars(coder,start,end)
    type(code_state),intent(inout):: coder
    integer,intent(in):: start,end
    integer:: i
    do i=start,end
       coder%stack(i)=-coder%stack(i)
    enddo
  end subroutine hide_vars

  subroutine hide_where_vars(coder,start,end)
    type(code_state),intent(inout):: coder
    integer,intent(in):: start,end
    integer:: i
    do i=start,end
       if(cnode_get_kind(coder%var(i))==cnode_is_var) then
          if(cnode_flags_set(coder%var(i),var_flags,var_is_where)) then
             coder%stack(i)=-coder%stack(i)
          endif
       endif
    enddo
  end subroutine hide_where_vars
  
  
  !====================================================
  ! Undo hide_vars for block of variables
  !====================================================
  subroutine reveal_vars(coder,start,end)
    type(code_state),intent(inout):: coder
    integer,intent(in):: start,end
    integer:: i
    do i=start,end
       if(coder%stack(i)<0) coder%stack(i)=-coder%stack(i)
    enddo
  end subroutine reveal_vars

  !==============================
  ! Make a temporary variable
  !==============================
  subroutine make_temp_var(coder,cblock,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    type(pm_ptr):: link
    integer:: flags
    if(pm_fast_isnull(node)) call pm_panic('null node in make_temp_var')
    call code_val(coder,cblock)
    call code_null(coder)
    flags=0
    call code_num(coder,flags)
    call code_null(coder)
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    call code_num(coder,coder%lex_scope)
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

  !================================================
  ! Make a system variable 
  ! (typically using reserved word/symbol as name)
  !================================================
  subroutine make_sys_var(coder,cblock,node,name,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: name,flags
    call make_var(coder,cblock,node,name,flags)
  contains
    include 'fname.inc'  
  end subroutine make_sys_var

  !====================================
  ! Make a local variable
  !====================================
  subroutine make_var(coder,cblocka,node,name,flags,extra_info)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblocka,node
    integer,intent(in):: name,flags
    type(pm_ptr),optional:: extra_info
    type(pm_ptr):: var,link,cblock
    integer:: vflags

    
    ! Check for prior definition
    if(iand(flags,var_is_shadowed+var_is_imported)==0) then
       var=find_var(coder,name)
       if(.not.pm_fast_isnull(var)) then
          if(pm_debug_checks) then
             if(name==0) call pm_panic('null name in make_var')
          endif
          call code_error(coder,node,&
               'Cannot redefine local variable or constant:',name)
          call code_val(coder,var)
          return
       endif
    endif

    if(cnode_flags_set(cblocka,cblock_flags,cblock_is_open)) then
       cblock=cnode_get(cblocka,cblock_parent)
    else
       cblock=cblocka
    endif
    
    ! Create variable node
    call code_val(coder,cblock)
    call code_num(coder,name)

    ! Flag variables according to current par state
    vflags=merge(ior(var_is_par_var,flags),flags,coder%par_state==par_state_par)
    
    ! All named variables multi access (this may change)
    call code_num(coder,ior(vflags,var_is_multi_access))
    call code_null(coder)
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    call code_num(coder,coder%lex_scope)
    if(present(extra_info)) then
       call code_val(coder,extra_info)
       call make_code(coder,node,cnode_is_var,var_node_size+1)
    else
       call make_code(coder,node,cnode_is_var,var_node_size)
    endif

    !    write(*,*) 'make-var>',trim(pm_name_as_string(coder%context,int(name%offset))),coder%index


    ! Add variable to stack
    call push_var(coder,name,top_code(coder))

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

  !====================================================
  ! Make an entry for a variable in the hash table
  !=====================================================
  subroutine push_var(coder,name,var)
    type(code_state),intent(inout):: coder
    integer:: name
    type(pm_ptr),intent(in):: var
    integer:: j


!!$    if(coder%top==9) then
!!$       write(*,*) 'Push',var%data%vkind,cnode_get_kind(var)
!!$       call qdump_code_tree(coder,pm_null_obj,6,var,2)
!!$       if(cnode_get_kind(var)/=cnode_is_var) call pm_panic('poo')
!!$    endif
    
    if(name==0) return
    if(coder%top>=max_code_stack) then
       call pm_panic('Program too complex')
    endif
    coder%top=coder%top+1
    j=coder%top
    coder%stack(j)=name
    coder%var(j)=var
  end subroutine push_var

  !=====================================
  ! Pop variables down to newbase
  !=====================================
  subroutine pop_vars_to(coder,newbase)
    type(code_state),intent(inout):: coder
    integer,intent(in):: newbase
    integer:: old_top
    old_top=coder%top
    coder%top=newbase
  end subroutine pop_vars_to

  !=========================================
  ! Make integer constant node (PM sint)
  !=========================================
  subroutine make_int_const(coder,cblock,node,val)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer:: val
    type(pm_ptr):: ptr
    ptr=pm_fast_newnc(coder%context,pm_int,1)
    ptr%data%i(ptr%offset)=val
    coder%temp2=ptr
    call make_const(coder,cblock,node,ptr)
    coder%temp2=pm_null_obj
  contains
    include 'fnewnc.inc'
  end subroutine make_int_const

  !==============================================
  ! Make long integer constant node (PM int)
  !===============================================
  subroutine make_long_const(coder,cblock,node,val)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer(pm_ln):: val
    type(pm_ptr):: ptr
    ptr=pm_fast_newnc(coder%context,pm_long,1)
    ptr%data%ln(ptr%offset)=val
    coder%temp2=ptr
    call make_const(coder,cblock,node,ptr)
    coder%temp2=pm_null_obj
  contains
    include 'fnewnc.inc'
  end subroutine make_long_const

  !================================================
  ! Make integer constant node, e.g: '123
  !================================================
  subroutine make_static_long_const(coder,cblock,node,val)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer(pm_ln):: val
    type(pm_ptr):: ptr
    ptr=pm_fast_newnc(coder%context,pm_long,1)
    ptr%data%ln(ptr%offset)=val
    coder%temp2=ptr
    call make_const(coder,cblock,node,ptr,pm_new_fix_value_type(coder%context,ptr))
    coder%temp2=pm_null_obj
  contains
    include 'fnewnc.inc'
  end subroutine make_static_long_const

  !===================================================
  ! Make static constant node, 'true or 'false
  !===================================================
  subroutine make_static_bool_const(coder,cblock,node,ok)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    logical,intent(in):: ok
    if(ok) then
       call make_const(coder,cblock,node,coder%true,&
            coder%true_fix)
    else
       call make_const(coder,cblock,node,coder%false,&
            coder%false_fix)
    endif
  end subroutine make_static_bool_const

  !===========================================
  ! Make a constant
  !===========================================
  subroutine make_const(coder,cblock,node,val,typ)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,val
    integer,intent(in),optional:: typ
    integer:: tno
    if(present(typ)) then
       tno=typ
    elseif(pm_fast_typeof(val)>=pm_int) then
       tno=pm_new_literal_value_type(coder%context,val)
    else
       tno=pm_fast_typeof(val)
    endif
    tno=pm_type_add_mode(coder%context,tno,sym_invar)
    call code_val(coder,val)
    call code_num(coder,tno)
    call make_code(coder,node,cnode_is_const,2)
  contains
    include 'ftypeof.inc'
  end subroutine make_const

  !===========================================
  ! Make a constant from a literal type
  !===========================================
  subroutine make_literal_const(coder,cblock,node,typ,fixit)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: typ
    logical,intent(in),optional:: fixit
    integer:: tno
    tno=typ
    if(present(fixit)) tno=pm_fix_value_type_from_literal(coder%context,tno)
    call code_val(coder,pm_type_val(coder%context,tno))
    call code_num(coder,tno)
    call make_code(coder,node,cnode_is_const,2)
  end subroutine make_literal_const

  
  !===========================
  ! Dupicate an expression
  !===========================
  subroutine dup_expr(coder,expr)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: expr
    type(pm_ptr):: e
    e=expr
    if(cnode_get_kind(e)==cnode_is_var) then
         call cnode_set_flags(e,var_flags,ior(var_is_multi_access,var_is_accessed))
      endif
    call code_val(coder,e)
  end subroutine dup_expr

  !==========================================
  ! Replicate top n expressions on stack
  !==========================================
  subroutine repl_expr(coder,index)
    type(code_state):: coder
    integer,intent(in):: index
    type(pm_ptr):: expr
    expr=coder%vstack(index)
    if(cnode_get_kind(expr)==cnode_is_var) then
         call cnode_set_flags(expr,var_flags,&
              ior(var_is_multi_access,var_is_accessed))
      endif
    call code_val(coder,expr)
  end subroutine repl_expr

  !=================================================================
  ! Make a procedure call cnode for some builtin operations
  !==================================================================
  subroutine make_sp_call(coder,cblock,node,sym,nargs,nret,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,nargs,nret
    integer,intent(in),optional:: flags
    integer:: aflags
    aflags=0
    if(present(flags)) aflags=flags
    call make_arglist(coder,cblock,node,nargs,nret,.false.,.false.)
    call code_null(coder)
    call make_full_call(coder,cblock,node,&
         pm_fast_tinyint(coder%context,-sym),pm_null_obj,nargs,abs(nret),0,&
         pm_null_obj,aflags,&
         pm_null_obj)
  contains
    include 'ftiny.inc'
  end subroutine make_sp_call

  !=================================================================
  ! Make a procedure call cnode for some builtin operations
  ! creating temporary variables for returns on stack
  !==================================================================
  subroutine make_sp_call_rtn(coder,cblock,node,sym,narg,nret,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret
    integer,intent(in),optional:: flags
    call make_sp_call(coder,cblock,node,sym,narg,-nret,flags)
  end subroutine make_sp_call_rtn
  
  !==================================================================
  ! Make a procedure call cnode for some builtin operations
  ! (does not create imports/exports)
  !==================================================================
  subroutine make_basic_sp_call(coder,cblock,node,sym,nargs,nret)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,nargs,nret
    call make_arglist(coder,cblock,node,nargs,nret,.false.,.false.,notouch=.true.)
    call code_null(coder)
    call make_full_call(coder,cblock,node,&
          pm_fast_tinyint(coder%context,-sym),pm_null_obj,&
          nargs,abs(nret),0,pm_null_obj,0,&
          pm_null_obj)
  contains
    include 'ftiny.inc'
  end subroutine make_basic_sp_call

  !=============================================
  ! Make a call to an intrinsic procedure
  ! Returns precede arguments on the stack
  !=============================================
  subroutine make_sys_call(coder,cblock,node,sym,&
       nargs,nret,aflags,assign)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,nargs,nret
    integer,intent(in),optional:: aflags
    logical,intent(in),optional:: assign   
    type(pm_ptr):: procs,avec
    integer:: flags
    if(present(aflags)) then
       flags=aflags
    else
       flags=0
    endif
    if(present(assign)) then
       avec=coder%std_amp
    else
       avec=pm_null_obj
    endif
 
    call make_arglist(coder,cblock,node,nargs,nret,.true.,.false.)
    call code_null(coder)
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,sym))
    call make_full_call(coder,cblock,node,&
         procs,avec,nargs+1,abs(nret),0,&
         pm_null_obj,flags,pm_null_obj)
  contains
    include 'fname.inc'
  end subroutine make_sys_call

  !=============================================
  ! Make a call to an intrinsic procedure
  ! Arguments must be on vstack
  ! Temporary return variables created and left
  ! on the vstack
  !=============================================
  subroutine make_sys_call_rtn(coder,cblock,node,sym,&
       nargs,nret,aflags,assign)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,nargs,nret
    integer,intent(in),optional:: aflags
    logical,intent(in),optional:: assign
    call make_sys_call(coder,cblock,node,sym,&
       nargs,-nret,aflags,assign)
  end subroutine make_sys_call_rtn
  
  !======================================================
  ! Make a call to an intrinsic communicating procedure
  ! Returns precede arguments on the stack
  !======================================================
  subroutine make_comm_sys_call(coder,cblock,node,sym,&
       nargs,nret,aflags,assign)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,nargs,nret
    integer,intent(in),optional:: aflags
    logical,intent(in),optional:: assign
    type(pm_ptr):: procs,avec
    integer:: flags,narg
    narg=nargs+num_comm_args
    if(present(aflags)) then
       flags=ior(aflags,proccall_is_comm+proccall_is_general)
    else
       flags=proccall_is_comm+proccall_is_general
    endif
    if(present(assign)) then
       avec=coder%comm_amp
    else
       avec=pm_null_obj
    endif
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,sym))
    call make_arglist(coder,cblock,node,nargs,nret,.false.,.true.)
    call code_null(coder)
    call make_full_call(coder,cblock,node,&
         procs,avec,narg,abs(nret),0,pm_null_obj,flags,&
         pm_null_obj)
  contains
    include 'fname.inc'
  end subroutine make_comm_sys_call

  !=============================================
  ! Make a call to a communicating procedure
  ! Arguments must be on vstack
  ! Temporary return variables created and left
  ! on the vstack
  !=============================================
  subroutine make_comm_sys_call_rtn(coder,cblock,node,sym,&
       nargs,nret,aflags,assign)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,nargs,nret
    integer,intent(in),optional:: aflags
    logical,intent(in),optional:: assign
    call make_comm_sys_call(coder,cblock,node,sym,&
         nargs,-nret,aflags,assign)
  end subroutine make_comm_sys_call_rtn
  
  !====================================================================
  ! Make a call to an intrinsic procedure with & on first argument
  ! Returns must precede arguments on the stack
  !====================================================================
  subroutine make_assign_call(coder,cblock,node,sym,nargs,nret,aflags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,nargs,nret
    integer,intent(in),optional:: aflags
    call make_sys_call(coder,cblock,node,sym,nargs,nret,&
         aflags=aflags,assign=.true.)
  end subroutine make_assign_call

  !=================================================
  ! Make a call to an intrinsic procedure
  ! with no processing of imports/exports
  ! Returns must precede arguments on the stack
  !=================================================
  subroutine make_basic_sys_call(coder,cblock,node,sym,narg,nret,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret,flags
    type(pm_ptr):: procs
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,sym))
    call make_arglist(coder,cblock,node,narg,nret,.true.,.false.,.true.)
    call code_null(coder)
    call make_full_call(coder,cblock,node,&
         procs,pm_null_obj,narg+1,abs(nret),0,pm_null_obj,&
         flags,pm_null_obj)
  contains
    include 'fname.inc'
  end subroutine make_basic_sys_call

  subroutine make_full_sys_call(coder,cblock,node,sym,&
       nargs,nret,amps,keys,keynames,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,amps,keys,keynames
    integer,intent(in):: sym,nargs,nret,flags
    type(pm_ptr):: procs
    integer:: narg,nkeys
    narg=nargs+num_comm_args
    nkeys=0
    if(.not.pm_fast_isnull(keys)) nkeys=cnode_numargs(keys)
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,sym))
    call make_arglist(coder,cblock,node,nargs,nret,.false.,.true.)
    call code_val(coder,keys)
    call make_full_call(coder,cblock,node,&
         procs,amps,narg,abs(nret),nkeys,keynames,flags,&
         pm_null_obj)
  contains
    include 'fname.inc'
    include 'fisnull.inc'
  end subroutine make_full_sys_call

  
  !==========================================
  ! Make a procedure call
  ! Argument list and key argument list (or null)
  ! must be on top of vstack.
  !==========================================
  subroutine make_full_call(coder,cblock,node,procs,&
       amps,nargs,nret,nkeys,keynames,flags,var)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,procs,amps,var,keynames
    integer,intent(in):: nargs,nret,nkeys,flags
    type(pm_ptr):: p,n
    integer:: iflag
    
    if(pm_debug_checks) then
       if(cnode_get_kind(cblock)/=cnode_is_cblock) then
          call pm_panic('full call cblock')
       endif
    endif

    iflag=flags
    if(coder%par_state>=par_state_masked) then
       iflag=ior(flags,call_is_cond)
    endif
    if(coder%par_state==par_state_cond.or.&
         coder%par_state==par_state_par) then
       iflag=ior(flags,call_is_unlabelled)
    endif
    call code_val(coder,cblock)
    call code_val(coder,procs)
    call code_num(coder,iflag)
    call code_null(coder)
    call code_val(coder,cnode_get(cblock,cblock_last_call))
    call code_num(coder,nret)
    call code_val(coder,keynames)
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    call code_val(coder,var)
    call code_val(coder,amps)
    call make_code(coder,node,cnode_is_call,call_node_size)
    
    n=top_code(coder)
    p=cnode_get(cblock,cblock_last_call)
    if(pm_fast_isnull(p)) then
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_first_call,pm_ln),n)
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_last_call,pm_ln),n)
    else
       if(pm_debug_checks) then
          call pm_verify_ptr(p,'make-full-call-p')
          call pm_verify_ptr(n,'make-full-call-n')
       endif
       call pm_ptr_assign(coder%context,p,int(call_link,pm_ln),n)
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_last_call,pm_ln),n)
    endif
 
    n=pop_code(coder)
    
    coder%proc_ncalls=coder%proc_ncalls+1
  contains
    include 'fisnull.inc'
    include 'fvkind.inc'
  end subroutine make_full_call

  !========================================================
  ! Make an argument list cnode on vstack
  ! - list will contain returns, implicit args, arguments
  !   in that order
  ! - nargs arguments must be present at top of vstack
  ! - if nret>0 then nret returns must precede arguments
  ! - if nret<0 then nret temp variables created and left
  !   on vstack before the argument list cnode
  !========================================================
  recursive subroutine make_arglist(coder,cblock,node,nargs,nret,isstd,&
       iscomm,notouch,comm_args_present)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: nargs,nret
    logical,intent(in):: isstd,iscomm
    logical,intent(in),optional:: notouch,comm_args_present
    integer:: i,ret0,arg0,extra0,nextra,base
    type(pm_ptr):: arglist
    
    if(.not.present(notouch)) then
       do i=coder%vtop-nargs+1,coder%vtop
          call update_arg(coder%vstack(i))
       enddo
    endif
    arg0=coder%vtop-nargs
    if(nret<0) then
       base=arg0
       ret0=coder%vtop
       do i=1,-nret
          call make_temp_var(coder,cblock,node)
       enddo
    else
       ret0=arg0-nret
       base=ret0
    endif

    if(iscomm) then
       do i=1,abs(nret)
          call cnode_set_flags(coder%vstack(ret0+i),var_flags,var_is_maybe_not_private)
       enddo
    endif
    
    if(iscomm.and..not.present(comm_args_present)) then
       extra0=coder%state_base
       nextra=num_comm_args-1
    elseif(isstd) then
       extra0=coder%state_base
       nextra=1
    else
       extra0=coder%state_base
       nextra=0
    endif
   
    arglist=make_arglist_cnode(coder,node,abs(nret),ret0,nextra,extra0,&
         iscomm.and..not.present(comm_args_present),nargs,arg0)
    if(nret<0) then
       if(ret0>base) then
          do i=1,-nret
             coder%vstack(base+i)=coder%vstack(ret0+i)
          enddo
          coder%vtop=base-nret+1
       else
          coder%vtop=base-nret+1
       endif
    else
       coder%vtop=base+1
    endif

    coder%vstack(coder%vtop)=arglist
  contains
    include 'fvkind.inc'
    
    subroutine update_arg(p)
      type(pm_ptr),intent(inout)::p
      
      if(pm_fast_vkind(p)==pm_pointer) then
         if(cnode_get_kind(p)==cnode_is_var) then
            if(cnode_flags_set(p,var_flags,var_is_split)) then
               call code_error(coder,p,&
                    'Variable is used by an enclosing "distinct" clause and may not be used directly: ',&
                    cnode_get_num(p,var_name))
               call code_error(coder,p,'This is the "distinct" clause that splits this variable')
            endif
            call update_change_lists(coder,p,.false.)
!!$            if(.not.iscomm) then
!!$               if(cnode_flags_set(p,var_flags,var_is_maybe_not_private)) then
!!$                  call code_val(coder,p)
!!$                  call make_comm_sys_call_rtn(coder,cblock,node,sym_dechan,1,1)
!!$                  p=pop_code(coder)
!!$               endif
!!$            endif
         endif
      endif
    end subroutine update_arg
    
  end subroutine make_arglist

  !=================================
  ! Make a cblock
  !=================================
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
    call code_num(coder,sym)
    call code_num(coder,coder%top)
    call code_num(coder,0)
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    if(pm_fast_isnull(node)) call pm_panic('NULL in makecblock')
    call code_null(coder)
    call code_null(coder)
    call make_code(coder,node,cnode_is_cblock,cblock_node_size)
    cblock=top_code(coder)
    if(pm_fast_isnull(cblock)) call pm_panic('make cblock null')
  contains
    include 'fisnull.inc'
  end function make_cblock

  !=============================================
  ! Close a cblock
  !==============================================
  subroutine close_cblock(coder,cblock)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    type(pm_ptr):: p
    if(cnode_get_name(cblock,cblock_sym)/=sym_sync.and.&
         cnode_get_name(cblock,cblock_sym)/=sym_any) then
       call pop_vars_to(coder,&
            int(cblock%data%ptr(cblock%offset+cblock_start)%offset))
    endif
    p=cnode_get(cblock,cblock_last_loop_call)
    if(pm_fast_isnull(p)) then
       call cnode_set(coder%context,cblock,cblock_last_loop_call,&
            cnode_get(cblock,cblock_first_call))
    else
       call cnode_set(coder%context,cblock,cblock_last_loop_call,&
            cnode_get(p,call_link))
    endif
  contains
    include 'fisnull.inc'
  end subroutine close_cblock
  
  !===========================================================
  ! Make a code tree node (cnode) from nargs values on vstack
  !===========================================================
  subroutine make_code(coder,node,ckind,nargs)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: ckind,nargs
    integer:: i
    if(pm_debug_checks) then
       if(coder%vtop-nargs<0) then
          write(*,*) '#',coder%vtop,'<',nargs
          call pm_panic('make code - not enough values on stack')
       endif
    endif
    call make_code_stem(coder,node,ckind,nargs)
    coder%temp%data%ptr(coder%temp%offset+5:coder%temp%offset+4+nargs)=&
         coder%vstack(coder%vtop-nargs+1:coder%vtop)
    if(pm_debug_checks) then
       do i=coder%temp%offset+5,coder%temp%offset+4+nargs
          !write(*,*) ckind,'##', i-coder%temp%offset-cnode_args
          call pm_verify_ptr(coder%temp%data%ptr(i),'Arg to new cnode')
       enddo
    endif
    coder%vtop=coder%vtop-nargs+1
    coder%vstack(coder%vtop)=coder%temp
  end subroutine make_code

  !===========================================================
  ! Make a combined argument list cnode, built as follows
  ! vstack(ret0+1)..vstack(ret0+nret) var(extra0+1..extra0+nextra)
  !   [ var(coder%mask) if mask ] vstack(args0+1..args0+nargs)
  !===========================================================
  function make_arglist_cnode(coder,node,nret,ret0,nextra,extra0,mask,nargs,args0) result(arglist)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: nret,ret0,nextra,extra0,nargs,args0
    logical,intent(in):: mask
    type(pm_ptr):: arglist
    integer:: i,j,totargs
    totargs=nret+nextra+nargs+merge(1,0,mask)
    !write(*,*) '####',nret,ret0,nextra,extra0,nargs,args0,mask
    call make_code_stem(coder,node,cnode_is_arglist,totargs)
    j=coder%temp%offset+cnode_args
    coder%temp%data%ptr(j:j+nret-1)=coder%vstack(ret0+1:ret0+nret)
    j=j+nret
    coder%temp%data%ptr(j:j+nextra-1)=coder%var(extra0+1:extra0+nextra)
    j=j+nextra
    if(mask) then
       coder%temp%data%ptr(j)=coder%var(coder%mask)
       j=j+1
    endif
    coder%temp%data%ptr(j:j+nargs-1)=coder%vstack(args0+1:args0+nargs)
    if(pm_debug_checks) then
       if(j+nargs/=coder%temp%offset+5+totargs) call pm_panic('make_arglist')
       do i=coder%temp%offset+5,coder%temp%offset+4+nargs
           call pm_verify_ptr(coder%temp%data%ptr(i),'Arg to new cnode')
       enddo
    endif
    arglist=coder%temp
  end function make_arglist_cnode

  !======================================================================
  ! Make a code tree node (cnode) with unfilled space for nargs arguments
  !======================================================================
  subroutine make_code_stem(coder,node,ckind,nargs)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: ckind,nargs
    type(pm_ptr):: modl
    integer:: ii
    coder%temp=pm_fast_newnc(coder%context,pm_pointer,&
         nargs+cnode_args)
    if(pm_debug_checks.and..false.) then
       if(coder%temp%data%ptr(coder%temp%offset)%offset&
            ==cnode_magic_no) then
          write(*,*) '------------'
          call qdump_code_tree(coder,pm_null_obj,62,coder%temp,2)
          do ii=1,coder%vtop
             call qdump_code_tree(coder,pm_null_obj,63,coder%vstack(ii),2)
          end do
          call pm_panic('Reuse cnode')
       endif
    endif
    coder%temp%data%ptr(coder%temp%offset)=&
         pm_fast_tinyint(coder%context,cnode_magic_no)
    coder%temp%data%ptr(coder%temp%offset+1)=&
         pm_fast_tinyint(coder%context,ckind)
    if(.not.pm_fast_isnull(node)) then
       modl=node_get_modl(node)
       coder%temp%data%ptr(coder%temp%offset+2)=&
         modl%data%ptr(modl%offset+modl_name)
       coder%temp%data%ptr(coder%temp%offset+3)=&
            node%data%ptr(node%offset+node_lineno)
       coder%temp%data%ptr(coder%temp%offset+4)=&
            node%data%ptr(node%offset+node_charno)
    else
       coder%temp%data%ptr(coder%temp%offset+2)=pm_null_obj
       coder%temp%data%ptr(coder%temp%offset+3)=pm_null_obj
       coder%temp%data%ptr(coder%temp%offset+4)=pm_null_obj
    endif
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'ftiny.inc'
  end subroutine make_code_stem


  
  !=======================================
  ! Check room on vstack
  !=======================================
  subroutine check_vstack(coder,amount)
    type(code_state),intent(in):: coder
    integer:: amount
    if(coder%vtop+amount>max_code_stack) then
       call pm_panic('Program too complex')
    endif
  end subroutine check_vstack

  !=======================================
  ! Push a value onto vstack
  !======================================
  subroutine code_val(coder,val)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: val
    if(coder%vtop>=max_code_stack) &
         call pm_panic("Procedure too complex")
    coder%vtop=coder%vtop+1
    coder%vstack(coder%vtop)=val
  end subroutine code_val


  !========================================
  ! Push tiny number onto vstack
  !========================================
  subroutine code_num(coder,n)
    type(code_state),intent(inout):: coder
    integer,intent(in):: n
    type(pm_ptr):: val
    val=pm_fast_tinyint(coder%context,n)
    call code_val(coder,val)
  contains
    include 'ftiny.inc'  
  end subroutine code_num

  !========================================
  ! Push null value onto vstack
  !========================================
  subroutine code_null(coder)
    type(code_state),intent(inout):: coder
    call code_val(coder,pm_null_obj)
  end subroutine code_null

  !===================================================
  ! Push a name value onto vstack
  !===================================================
  subroutine code_name(coder,val)
    type(code_state),intent(inout):: coder
    integer,intent(in):: val
    call code_val(coder,pm_fast_name(coder%context,val))
  contains
    include 'fname.inc'
  end subroutine code_name

  !======================================================================
  ! Make an integer vector from array(start:end) and push onto vstack
  !======================================================================
  subroutine code_int_vec(coder,array,start,end)
    type(code_state),intent(inout):: coder
    integer,dimension(:),intent(in):: array
    integer,intent(in):: start,end
    type(pm_ptr):: p
    call code_val(coder,pm_fast_newnc(coder%context,pm_int,end-start+1))
    p=top_code(coder)
    p%data%i(p%offset:p%offset+end-start)=array(start:end)
  contains
    include 'fnewnc.inc'
  end subroutine code_int_vec

  !=======================================
  ! Duplicate code on top of vstack
  !=======================================
  subroutine dup_code(coder)
    type(code_state),intent(inout):: coder
    call code_val(coder,top_code(coder))
  end subroutine dup_code

  !======================================
  ! Swap top 2 items on vstack
  !=====================================
  subroutine swap_code(coder)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: temp
    temp=coder%vstack(coder%vtop)
    coder%vstack(coder%vtop)=coder%vstack(coder%vtop-1)
    coder%vstack(coder%vtop-1)=temp
  end subroutine swap_code

  !==========================================================
  ! Swap top 2 items on vstack and duplicate lower item
  ! ... a b -> ... b b a
  !===========================================================
  subroutine swap_and_dup_code(coder)
    type(code_state),intent(inout):: coder
    coder%vstack(coder%vtop+1)=coder%vstack(coder%vtop-1)
    coder%vstack(coder%vtop-1)=coder%vstack(coder%vtop)
    coder%vtop=coder%vtop+1
  end subroutine swap_and_dup_code

  !=================================================
  ! Remove 2nd item on vstack, replacing with top
  ! ... a b  ->  ... b
  !=================================================
  subroutine pushdown_code(coder)
    type(code_state),intent(inout):: coder
    coder%vstack(coder%vtop-1)=coder%vstack(coder%vtop)
    coder%vtop=coder%vtop-1
  end subroutine pushdown_code

  !======================================================
  ! Swap top 2 items on the stack with single item below
  ! ... a b c  ->  ... b c a
  !======================================================
  subroutine swap_code_2_1(coder)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: temp
    temp=coder%vstack(coder%vtop)
    coder%vstack(coder%vtop)=coder%vstack(coder%vtop-2)
    coder%vstack(coder%vtop-2)=coder%vstack(coder%vtop-1)
    coder%vstack(coder%vtop-1)=temp
  end subroutine swap_code_2_1

  !======================================================
  ! Swap top item on the stack with 2 items below
  ! ... a b c  ->  ... c a b
  !======================================================
  subroutine swap_code_1_2(coder)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: temp
    temp=coder%vstack(coder%vtop)
    coder%vstack(coder%vtop)=coder%vstack(coder%vtop-1)
    coder%vstack(coder%vtop-1)=coder%vstack(coder%vtop-2)
    coder%vstack(coder%vtop-2)=temp
  end subroutine swap_code_1_2

  !=================================
  ! Pop value from vstack
  !=================================
  function pop_code(coder) result(val)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: val
    if(pm_debug_checks) then
       if(coder%vtop<1) &
            call pm_panic('pop code stack')
    endif
    val=coder%vstack(coder%vtop)
    coder%vtop=coder%vtop-1
  end function pop_code

  !===================================
  ! Drop value from vstack
  !===================================
  subroutine drop_code(coder) 
    type(code_state),intent(inout):: coder
    if(pm_debug_checks) then
       if(coder%vtop<1) &
            call pm_panic('drop code stack')
    endif
    coder%vtop=coder%vtop-1
  end subroutine drop_code

  !=================================
  ! Pop value from vstack
  !=================================
  function top_code(coder) result(val)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: val
    val=coder%vstack(coder%vtop)
  end function top_code

  !=========================================================
  ! Check available space on the wstack is >=amount
  !=========================================================
  subroutine check_wstack(coder,amount)
    type(code_state),intent(in):: coder
    integer:: amount
    if(coder%wtop+amount>max_code_stack) then
       call pm_panic('Program too complex')
    endif
  end subroutine check_wstack

  !================================
  ! Push value onto wstack
  !================================
  subroutine push_word(coder,k)
    type(code_state),intent(inout):: coder
    integer,intent(in):: k
    if(coder%wtop>=max_code_stack) &
         call pm_panic('Program too complex')
    coder%wtop=coder%wtop+1
    coder%wstack(coder%wtop)=k
  end subroutine push_word

  !=============================
  ! Pop value from wstack
  !=============================
  function pop_word(coder) result(k)
    type(code_state),intent(inout):: coder
    integer:: k
    k=coder%wstack(coder%wtop)
    coder%wtop=coder%wtop-1
    if(pm_debug_checks) then
       if(coder%wtop<0) call pm_panic('pop word')
    endif
  end function pop_word

  !================================
  ! Drop top entry of wstack
  !================================
  subroutine drop_word(coder)
    type(code_state),intent(inout):: coder
    coder%wtop=coder%wtop-1
    if(pm_debug_checks) then
       if(coder%wtop<0) call pm_panic('drop word')
    endif
  end subroutine drop_word

  !===================================
  ! Return top of wstack
  !===================================
  function top_word(coder) result(k)
    type(code_state),intent(inout):: coder
    integer:: k
    k=coder%wstack(coder%wtop)
  end function top_word

     
  !=====================================
  ! Dump a cnode tree (debugging)
  !=====================================
  recursive subroutine qdump_code_tree(coder,rvec,iunit,node,depth)
    type(code_state):: coder
    type(pm_ptr),intent(in):: rvec
    integer,intent(in):: iunit,depth
    type(pm_ptr),intent(in):: node
    character(len=100),parameter:: spaces=' '
    character(len=100):: str
    type(pm_ptr):: p
    integer:: i,n,po
    if(pm_fast_isnull(node)) then
       write(iunit,*) spaces(1:depth*2),'NULL'
       return
    else if(node%data%vkind/=pm_pointer) then
       if(pm_fast_isname(node)) then
          write(iunit,*) spaces(1:depth*2),trim(pm_name_as_string(coder%context,int(node%offset)))
          return
       else
          write(iunit,*) spaces(1:depth*2),'Non-ptr',node%data%vkind
          call pm_dump_tree(coder%context,iunit,node,2)
          return
       endif
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
            trim(pm_name_as_string(coder%context,&
            cnode_get_name(node,cblock_sym)))
       p=cnode_get(node,cblock_first_call)
       do while(.not.pm_fast_isnull(p))
          if(p==cnode_get(node,cblock_last_loop_call)) then
             write(iunit,*) spaces(1:depth*2), 'LAST_CALL:'
          endif
          call qdump_code_tree(coder,rvec,iunit,p,depth+1)
          p=cnode_get(p,call_link)
       enddo
       write(iunit,*) spaces(1:depth*2),'End: ',&
            trim(pm_name_as_string(coder%context,&
            cnode_get_name(node,cblock_sym)))
    case(cnode_is_var)
       p=cnode_get(node,var_name)
       if(pm_fast_isnull(p)) then
          str='/Temp/'
       else
          call pm_name_string(coder%context,int(p%offset),str)
       endif
       write(iunit,'(A,A,"      Idx=",I4," Chng=",L1," Flags=",I4,"offset=",I6,I6,L)') &
            spaces(1:depth*2),trim(str),&
            cnode_get_num(node,var_index),&
            cnode_flags_set(node,var_flags,var_is_changed),&
            cnode_get_num(node,var_flags),node%offset,node%data%hash,marked(node)
       if(.not.pm_fast_isnull(rvec)) then
          i= rvec%data%i(rvec%offset+&
               cnode_get_num(node,var_index))
          if(i<0) then
             write(iunit,*) spaces(1:depth*2),' Unresolved!!'
          else
             write(iunit,*) spaces(1:depth*2),' Resolved:',i,&
                  trim(pm_type_as_string(coder%context,i))
          endif
       endif
    case(cnode_is_const)
       call pm_dump_tree(coder%context,iunit,cnode_arg(node,1),depth)
!!$       write(iunit,*)  spaces(1:depth*2),&
!!$            trim(pm_type_as_string(coder%context,&
!!$            cnode_get_num(node,node_args+1)))
    case(cnode_is_call)
       p=cnode_get(node,call_sig)
       if(pm_fast_istiny(p)) then
          po=p%offset
          if(p%offset<0) then
             write(iunit,*) spaces(1:depth*2),&
                  'Call "',trim(sym_names(-p%offset)),'":',&
                  cnode_get_num(node,call_index)
          else
             write(iunit,*) 'ISS>',p%offset
             if(p%offset==0) then
                str='varcall'
             else
                p=pm_dict_key(coder%context,coder%sig_cache,&
                     int(p%offset,pm_ln))
                call pm_name_string(coder%context,&
                     p%data%i(p%offset+pm_fast_esize(p)),str)
             endif
             write(iunit,'(A,A,A,A,"      Idx=",I4," Flags=",I4,"<",I4,">")') &
                  spaces(1:depth*2),'Call (',trim(str),') (',&
                  cnode_get_num(node,call_index),&
                  cnode_get_num(node,call_flags),po
          endif
          if(.not.pm_fast_isnull(rvec)) then
             i= rvec%data%i(rvec%offset+&
                  cnode_get_num(node,call_index))
             if(i<0) then
                if(i==sp_sig_link) then
                   write(iunit,*) spaces(1:depth*2),' sp_sig_thru'
                elseif(i==sp_sig_dup) then
                   write(iunit,*) spaces(1:depth*2),' sp_sig_dup'
                elseif(i==sp_sig_noop) then
                   write(iunit,*) spaces(1:depth*2),' sp_sig_noop'
                elseif(p%offset>=0) then
                   write(iunit,*) spaces(1:depth*2),' Unresolved Sig!!'
                endif
             else
                write(iunit,*) spaces(1:depth*2),' Resolved Sig:',i
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
             call qdump_code_tree(coder,rvec,iunit,&
                  cnode_arg(p,i),depth+1)
          enddo
       endif
       write(iunit,*) spaces(1:depth*2),'Args:'
       do i=n+1,cnode_numargs(p)
          call qdump_code_tree(coder,rvec,iunit,&
               cnode_arg(p,i),depth+1)
       enddo
       if(.not.pm_fast_isnull(cnode_get(node,call_var))) then
          write(iunit,*) spaces(1:depth*2),'Callvar:'
          call qdump_code_tree(coder,rvec,iunit,&
               cnode_get(node,call_var),depth+1)
       endif

       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_builtin)
       if(cnode_get_num(node,cnode_args)>=0) then
          write(iunit,*) spaces(1:depth*2),'Builtin ',&
               op_names(cnode_get_num(node,cnode_args)),&
               cnode_get_num(node,cnode_args+1),'('
       else
          write(iunit,*) spaces(1:depth*2),'Fold ',&
               (cnode_get_num(node,cnode_args)),&
               cnode_get_num(node,cnode_args+1),'('
       endif
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_proc)
       write(iunit,'(A,A,i2,A,i2,A,i2,A,i3,A,i3,A)') spaces(1:depth*2),&
            'Proc [nargs=',&
            cnode_get_num(node,pr_nargs),',nret=',cnode_get_num(node,pr_nret),&
            ',ncalls=',cnode_get_num(node,pr_ncalls),',flags=',cnode_get_num(node,pr_flags),'] ('
       call qdump_code_tree(coder,rvec,iunit,&
            cnode_arg(node,1),depth+1)
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_resolved_proc)
       write(iunit,*) spaces(1:depth*2),'Resolved Proc(',&
            cnode_get_num(node,cnode_args+2),&
            trim(pm_name_as_string(coder%context,&
            cnode_get_name(cnode_arg(node,1),pr_name)))
       if(cnode_flags_set(node,cnode_args+2,proc_is_recursive)) &
            write(iunit,*) spaces(1:depth*2+1),'[recursive]'
       if(cnode_flags_set(node,cnode_args+2,proc_is_impure)) &
            write(iunit,*) spaces(1:depth*2+1),'[impure]'
       if(cnode_flags_set(node,cnode_args+2,proc_is_not_inlinable)) &
            write(iunit,*) spaces(1:depth*2+1),'[not inlinable]'
       call qdump_code_tree(coder,cnode_arg(node,2),&
            iunit,cnode_arg(node,1),depth+1)
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_arglist)
       write(iunit,*) spaces(1:depth*2),'Var Sig List(',cnode_numargs(node)
       write(iunit,*) spaces(1:depth*2),'Sig List(',cnode_numargs(node)
       do i=2,cnode_numargs(node),2
          write(iunit,*) spaces(1:depth*2),trim(pm_type_as_string(coder%context,&
               cnode_get_num(node,cnode_args+i-1)))
          call qdump_code_tree(coder,rvec,iunit,cnode_arg(node,i+1),depth+1)
       enddo
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_any_sig)
       write(iunit,*) spaces(1:depth*2),'Any signature ('
       do i=1,cnode_numargs(node)
          call pm_dump_tree(coder%context,iunit,cnode_arg(node,i),depth+1)
       enddo
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_autoconv_sig)
       write(iunit,*) spaces(1:depth*2),'Auto convert signature ('
       do i=1,cnode_numargs(node)
          call pm_dump_tree(coder%context,iunit,cnode_arg(node,i),depth+1)
       enddo
       write(iunit,*) spaces(1:depth*2),')'
    case default 
       write(iunit,*) spaces(1:depth*2),'<<Unknown Cnode!!!>>'
    end select
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fistiny.inc'
    include 'fvkind.inc'
    include 'fisname.inc'
  end subroutine  qdump_code_tree

  !========================================
  ! Return the name of a given signature
  !========================================
  function sig_name(coder,m) result(name)
    type(code_state),intent(in):: coder
    integer,intent(in):: m
    integer:: name
    type(pm_ptr):: key
    if(m==0) then
       name=sym_var
    else
       key=pm_dict_key(coder%context,coder%sig_cache,int(m,pm_ln))
       name=key%data%i(key%offset+pm_fast_esize(key))
    endif
  contains
    include 'fesize.inc'
  end function sig_name

  !==================================================
  ! Return the name of a given signature as a string
  !==================================================
  function sig_name_str(coder,m) result(str)
    type(code_state),intent(in):: coder
    integer,intent(in):: m
    character(len=100):: str
    if(m==0) then
       str='var'
    else
       call pm_name_string(coder%context,sig_name(coder,m),str)
    endif
  end function sig_name_str

  !============================================
  ! Dump information on all defined signatures
  !============================================
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
            sig%data%i(sig%offset+pm_fast_esize(sig)),str)
       call pm_name_string(coder%context,&
            sig%data%i(sig%offset+pm_fast_esize(sig)-2),str2)
       write(iunit,*) 'Sig(',trim(str),&
            ',Amplocs=',trim(str2),&
            ',nret=',sig%data%i(sig%offset+pm_fast_esize(sig)-1),') ('
       if(pm_fast_vkind(code)==pm_int) then
          call pm_dump_tree(coder%context,iunit,code,2)
       else
          do j=3,cnode_numargs(code),2
             typ=cnode_arg(code,j)
             write(iunit,*) 'Type:',trim(pm_type_as_string(coder%context,&
                  int(typ%offset)))
             write(iunit,*) 'Code:',j,cnode_numargs(code)
             call qdump_code_tree(coder,pm_null_obj,iunit,cnode_arg(code,j+1),2)
          enddo
       endif
       write(iunit,*) ')'
    enddo
  contains
    include 'fesize.inc'
    include 'fvkind.inc'
  end subroutine dump_sigs


  !===================================================================
  ! Procedure signature as a string (including module/line/char info)
  !===================================================================
  function proc_sig_as_str(coder,proc) result(str)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: proc
    character(len=256):: str
    type(pm_ptr):: args
    character(len=7):: buf1,buf2
    args=node_get(proc,proc_params)
    call pm_name_string(coder%context,&
         node_get_modl_name(proc),str)
    if(str=='PM__system'.and.pm_opts%hide_sysmod) then
       str='(System):'
    else
       write(buf1,'(I7)') node_get_lineno(proc)
       write(buf2,'(I7)') node_get_charno(proc)
       write(str,'(A,":",A,":",A,":")') trim(str),trim(adjustl(buf1)),trim(adjustl(buf2))
    endif
    str(len_trim(str)+2:)=sig_as_str(coder,node_get(proc,proc_name),node_get(proc,proc_amplocs),&
         node_numargs(args)/2,node_get_num(proc,proc_numret),&
         node_get_num(proc,proc_flags),args=args)
  end function proc_sig_as_str

  !===================================================================
  ! Procedure signature as a string 
  !===================================================================
  function sig_as_str(coder,name,ampl,numargs,nret,flags,args) result(str)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: name,ampl
    integer,intent(in):: numargs,nret,flags
    type(pm_ptr),intent(in),optional:: args
    character(len=256):: str
    integer::i,n,m,k,nargs,sym
    type(pm_ptr):: amp
    character(len=26*2),parameter:: vn='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
    nargs=numargs
    if(pm_fast_isnull(ampl)) then
       m=-1
    else
       amp=pm_name_val(coder%context,int(ampl%offset))
       m=0
    endif
    n=1
    k=1
    do i=1,nret-1
       str(n:n+1)=vn(k:k)//','
       n=n+2
       k=k+1
    enddo
    if(nret>0) then
       str(n:n+1)=vn(k:k)//'='
       n=n+2
       k=k+1
    endif
    call pm_name_string(coder%context,int(name%offset),str(n:))
    n=len_trim(str)+1
    if(iand(flags,proccall_is_comm)/=0) then
       str(n:n)='%'
       n=n+1
    endif
    if(present(args).and.&
         iand(flags,proccall_is_comm)/=0) then
              str(n:n)='('
       n=n+1
       do i=num_comm_args+1,nargs
          call check_amp(i)
          sym=node_sym(node_arg(args,i*2))
          if(sym==sym_mode) then
             sym=node_get_num(node_arg(args,i*2),node_args+1)
             str(n:)=sym_names(sym)
             n=len_trim(str)+2
          endif
          str(n:n)=vn(k:k)
          n=n+1
          k=k+1
          if(n>230.or.k>26*2) then
             str(n:n+4)='<...>'
             n=n+5
             exit
          endif
          if(i<nargs) then
             str(n:n)=','
             n=n+1
          endif
       enddo
    else
       str(n:n)='('
       n=n+1
       do i=1,nargs
          call check_amp(i)
          str(n:n)=vn(k:k)
          n=n+1
          k=k+1
          if(n>230.or.k>26*2) then
             str(n:n+4)='<...>'
             n=n+5
             exit
          endif
          if(i<nargs) then
             str(n:n)=','
             n=n+1
          endif
       enddo
    endif
    if(present(args)) then
       if(node_sym(args)==sym_dotdotdot) then
          str(n:n+2)='...'
          n=n+3
       endif
       if(iand(flags,proc_is_cond)/=0) then
          str(n:n+7)='<<cond>>'
          n=n+8
       endif
       if(iand(flags,proc_is_uncond)/=0) then
          str(n:n+9)='<<uncond>>'
          n=n+10
       endif
    endif
    str(n:n)=')'
  contains
    include 'fisnull.inc'

    subroutine check_amp(k)
      integer:: k
      if(m>=0) then
         if(amp%data%i(amp%offset+m)==k) then
            str(n:n)='&'
            n=n+1
            m=m+1
         endif
      endif
    end subroutine check_amp
    
  end function sig_as_str
  
  !=======================================================
  ! Error message - location information from given node
  !=======================================================
  subroutine code_error(coder,node,message,name,warn)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    character(len=*):: message
    integer,intent(in),optional:: name
    logical,intent(in),optional:: warn
    character(len=256):: str
    if(pm_main_process) then
       write(*,*)
       if(.not.pm_fast_isnull(node)) then
          call pm_error_header(coder%context,node_get_modl_name(node),&
                  node_get_lineno(node),node_get_charno(node))
       endif
       if(.not.present(warn)) then
          if(present(name)) then
             call pm_name_string(coder%context,name,str)
             str=trim(pm_opts%error)//' '//trim(message)//' '//trim(str)
          else
             str=trim(pm_opts%error)//' '//message
          endif
          write(*,'(A)') trim(str)
       else
          write(*,'(A)') trim(message)
       endif
    endif
    if(.not.present(warn)) then
       coder%num_errors=coder%num_errors+1
       if(coder%num_errors>max_code_errors) then
          call pm_stop('Too many semantic errors - compilation terminated')
       endif
    endif
  contains
    include 'fisnull.inc'
  end subroutine code_error

  !=======================================================
  ! Error message - location information from given cnode
  !=======================================================
  subroutine cnode_error(coder,node,message,name,warn)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    character(len=*):: message
    type(pm_ptr),intent(in),optional:: name
    logical,intent(in),optional:: warn
    character(len=256):: str
    if(pm_main_process) then
       call pm_error_header(coder%context,&
            cnode_get_name(node,cnode_modl_name),&
            cnode_get_name(node,cnode_lineno),&
            cnode_get_name(node,cnode_charno))
       if(present(warn)) then
          str=message
       elseif(present(name)) then
          call pm_name_string(coder%context,int(name%offset),str)
          str=trim(pm_opts%error)//' '//trim(message)//' '//trim(str)
       else
          str=trim(pm_opts%error)//' '//trim(message)
       endif
       write(*,'(A)') trim(str)
    endif
    if(.not.present(warn)) then
       coder%num_errors=coder%num_errors+1
       if(coder%num_errors>max_code_errors) then
          call pm_stop('Too many semantic errors - compilation terminated')
       endif
    endif
  end subroutine cnode_error


end module pm_codegen

