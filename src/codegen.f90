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
  integer,parameter:: max_par_depth=256
  integer,parameter:: max_type_nesting=64
  integer,parameter:: max_error_nodes=1024

  ! Flags indicating state within parallel statement nesting
  integer,parameter:: par_state_nhd=0
  integer,parameter:: par_state_outer=1
  integer,parameter:: par_state_for=2
  integer,parameter:: par_state_loop=3
  integer,parameter:: par_state_cond_loop=4
  ! -- The following are conditional states (can check >=par_state_cond)
  integer,parameter:: par_state_cond=5
  integer,parameter:: par_state_par=6
  integer,parameter:: par_state_masked=7
  integer,parameter:: par_state_over=8
  integer,parameter:: par_state_any=9
  integer,parameter:: par_state_labelled=10

  ! Reference flags
  integer,parameter:: ref_is_val=1
  integer,parameter:: ref_ignores_rules=2
  integer,parameter:: ref_is_dollar=4
  integer,parameter:: ref_is_priv=8
  integer,parameter:: ref_is_shared=16
  integer,parameter:: ref_is_subscripted=32
  integer,parameter:: ref_has_at=64
  integer,parameter:: ref_is_amp=256
  
  ! Offsets for loop variables
  integer,parameter:: lv_dom=-3
  integer,parameter:: lv_prc=-2
  integer,parameter:: lv_distr=-1
  integer,parameter:: lv_tile=0
  integer,parameter:: lv_num=1
  integer,parameter:: lv_numz=2
  integer,parameter:: lv_index=5
  integer,parameter:: lv_here=6
  integer,parameter:: lv_end=1
  integer,parameter:: lv_state=2
  integer,parameter:: lv_last=3
  integer,parameter:: lv_idx=3
  
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
     integer,dimension(max_code_stack):: stack,imps
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

     ! for & par statements - import/export 
     type(pm_ptr):: loop_cblock
     type(pm_ptr),dimension(max_par_depth):: import_cblock
     integer:: par_depth,proc_par_depth
     integer:: par_base,over_base

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
     type(pm_ptr):: temp,temp2,true,false,one,comm_amp,std_amp,check_mess,undef_val

     ! 'true and 'false types
     integer:: true_fix,false_fix,true_literal,false_literal

     ! '1 type
     integer:: unit_type

     ! Contextual information for this point in the traverse
     type(pm_ptr):: proc, proc_keys
     integer:: proc_base,proc_nret,proc_key_base,proc_ncalls
     integer:: run_mode,run_flags,par_state
     type(pm_ptr):: label,default_label
     logical:: fixed,aliased,in_sync

     ! This point in a subscript tuple
     integer:: subs_index

     ! Counter to give each proc a unique index for all procs
     integer:: id
     
     ! Counter to provide unique index for all nodes created
     integer:: index

     ! Counter to provide unique index for all blocks
     integer:: block_id

     ! Nesting depth of if statements (offset into vstack)
     integer:: lex_scope

     ! Blocks
     integer:: block_entry,block_base

     ! Flags indicating type inference not complete
     logical:: types_finished,redo_calls,incomplete,first_pass

     ! Taints
     integer:: taints,proc_taints

     ! Type inference base of current proc record
     integer:: base
     
     ! This is the parallel kind storeageless implicit argument
     integer:: par_kind,par_kind2

     ! Type inference flag recursion -- use to locate infinite recursion
     logical:: flag_recursion

     ! Type inference procedure trace
     type(pm_ptr),dimension(max_par_depth):: trace
     integer,dimension(max_par_depth)::trace_keys
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
    type(pm_ptr):: sig
    integer:: sym
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
         coder%proc_name_vals,coder%poly_cache,coder%comm_amp,coder%std_amp,array=&
         coder%vstack,array_size=coder%vtop)
    coder%reg3=>pm_register(context,'coder-for stack',coder%defer_check,&
         coder%check_mess)
    coder%sig_cache=pm_dict_new(context,32_pm_ln)
    coder%prog_cblock=pm_null_obj
    coder%defer_check=pm_null_obj
    coder%proc_base=1
    coder%proc_ncalls=0
    coder%par_base=0
    coder%over_base=0
    coder%par_depth=0
    coder%proc_par_depth=0
    coder%par_state=par_state_outer
    coder%run_mode=sym_private
    coder%run_flags=0
    coder%loop_cblock=pm_null_obj
    coder%proc_keys=pm_null_obj
    coder%index=0
    coder%lex_scope=0
    coder%true=pm_new_small(context,pm_logical,1_pm_p)
    coder%true%data%l(coder%true%offset)=.true.
    coder%false=pm_new_small(context,pm_logical,1_pm_p)
    coder%false%data%l(coder%false%offset)=.false.

    coder%one=pm_new_small(context,pm_long,1_pm_p)
    coder%one%data%ln(coder%one%offset)=1
    coder%unit_type=pm_new_fix_type(coder%context,coder%one)

    coder%one=pm_new_small(context,pm_int,1_pm_p)
    coder%one%data%i(coder%one%offset)=1
    coder%comm_amp=pm_new_small(context,pm_int,1_pm_p)
    coder%comm_amp%data%i(coder%comm_amp%offset)=num_comm_args+1
    coder%std_amp=pm_new_small(context,pm_int,1_pm_p)
    coder%std_amp%data%i(coder%std_amp%offset)=2
    
    coder%one=pm_fast_tinyint(coder%context,&
         pm_intern_val(coder%context,coder%one))
    coder%comm_amp=pm_fast_tinyint(coder%context,&
         pm_intern_val(coder%context,coder%comm_amp))
    coder%std_amp=pm_fast_tinyint(coder%context,&
         pm_intern_val(coder%context,coder%std_amp))
    coder%check_mess=pm_new_string(coder%context,'Failed "check" or "test""')
    coder%proc_name_vals=pm_dict_new(coder%context,8_pm_ln)
    coder%id=0
    coder%block_id=0
    coder%true_fix=pm_new_fix_type(coder%context,coder%true)
    coder%false_fix=pm_new_fix_type(coder%context,coder%false)
    coder%true_literal=pm_new_literal_type(coder%context,coder%true)
    coder%false_literal=pm_new_literal_type(coder%context,coder%false)

    coder%default_label=pm_fast_name(coder%context,sym_pct)
    coder%label=coder%default_label
    coder%num_errors=0
    coder%supress_errors=.false.
    coder%fixed=.false.
    coder%aliased=.false.
    coder%in_sync=.false.
    coder%subs_index=-1

  contains
    include 'fname.inc'
    include 'ftiny.inc'
    
    function name_type(n) result(u)
      integer,intent(in):: n
      integer:: u
      u=pm_new_name_type(coder%context,n)
    end function name_type

  end subroutine init_coder

  !========================================================
  ! Finalise and delete code generator
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
    integer:: i
    
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
    call make_var_mode(coder,prog_cblock,stmt_list,coder%var(coder%top))
    
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
  ! SEQUENTIAL STATEMENTS
  !*******************************************************

  
  !========================================================
  ! Traverse statement list - push cblock onto stack
  !========================================================
  recursive subroutine trav_stmt_list(coder,parent,&
       listp,list,lsym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: parent,listp,list
    integer,intent(in):: lsym
    type(pm_ptr):: cblock
    cblock=make_cblock(coder,parent,listp,lsym)
    call trav_open_stmt_list(coder,cblock,&
       listp,list)
    call close_cblock(coder,cblock)
  end subroutine trav_stmt_list

  !========================================================
  ! Traverse open list of statements - add to passed cblock
  !========================================================
  recursive subroutine trav_open_stmt_list(coder,cblock,&
       listp,list)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,listp,list
    integer:: i,j,n,sym,base,vbase,wbase,lex_scope
    integer:: save_par_state,save_over_base,save_run_flags
    type(pm_ptr):: node,cblock2,var,p
    logical:: iscomm,isshared,ok,oldfix
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
       case(sym_if)
          save_par_state=coder%par_state
          lex_scope=push_lex_scope(coder)
          call trav_xexpr(coder,cblock,node,&
               node_arg(node,1))
          if(coder%par_state>par_state_outer) then
             if(pm_fast_isnull(node_arg(node,3))) then
                coder%par_state=par_state_masked
             else
                coder%par_state=par_state_cond
             endif
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
          call get_lex_scope(coder,node)
          call make_sp_call(coder,cblock,node,&
               sym_if,4,0)
          call pop_lex_scope(coder)
          coder%par_state=save_par_state
       case(sym_if_invar)
          if(coder%par_state>=par_state_cond) then
             call code_error(coder,node,&
                  'Cannot have "if invar" in a conditional context')
          endif
          lex_scope=push_lex_scope(coder)
          call trav_xexpr(coder,cblock,node,&
               node_arg(node,1))
          call code_check_invar(coder,cblock,node,top_code(coder))
          coder%lex_scope=lex_scope
          call trav_stmt_list(coder,cblock,node,&
               node_arg(node,2),sym_if_invar)
          if(.not.pm_fast_isnull(node_arg(node,3))) then
             call trav_stmt_list(coder,cblock,&
                  node,node_arg(node,3),sym_if_invar)
          else
             call code_null(coder)
          endif
          call get_lex_scope(coder,node)
          call make_sp_call(coder,cblock,node,sym,4,0)
          call pop_lex_scope(coder)
       case(sym_switch)
          save_par_state=coder%par_state
          call trav_xexpr(coder,cblock,node,&
               node_arg(node,1))
          var=top_code(coder)
          if(save_par_state>par_state_outer) then
             if(node_numargs(node)==4) then
                  if(pm_fast_isnull(node_arg(node,4))) then
                     coder%par_state=par_state_masked
                  else
                     coder%par_state=par_state_cond
                  endif
             else
                coder%par_state=par_state_cond
             endif
          endif
          call trav_switch_stmt(coder,cblock,node,2,var,sym_if)
          call drop_code(coder)
          coder%par_state=save_par_state
       case(sym_switch_invar)
          if(coder%par_state>=par_state_cond) then
             call code_error(coder,node,&
                  'Cannot have "switch invar" in a conditional context')
          endif
          call trav_xexpr(coder,cblock,node,&
               node_arg(node,1))
          call code_check_invar(coder,cblock,node,top_code(coder))
          call trav_switch_stmt(coder,cblock,node,2,var,sym_if_invar)
          call drop_code(coder)     
       case(sym_while,sym_while_invar)
          save_par_state=coder%par_state
          lex_scope=push_lex_scope(coder)
          cblock2=make_cblock(coder,cblock,node,sym_while)
          call trav_xexpr(coder,cblock2,node,node_arg(node,1))
          if(sym==sym_while_invar) then
             call code_check_invar(coder,cblock2,node,top_code(coder))
          endif
          call close_cblock(coder,cblock2)
          coder%par_state=save_par_state
          coder%lex_scope=lex_scope
          call trav_stmt_list(coder,cblock,node,&
               node_arg(node,2),sym_while)
          call get_lex_scope(coder,node)
          call make_sp_call(coder,cblock,node,sym_while,4,0)
          call pop_lex_scope(coder)
          coder%par_state=save_par_state
       case(sym_until,sym_until_invar)
          lex_scope=push_lex_scope(coder)
          save_par_state=coder%par_state
          cblock2=make_cblock(coder,cblock,node,sym_until)
          coder%lex_scope=lex_scope
          call trav_open_stmt_list(coder,cblock2,node,&
               node_arg(node,2))
          iscomm=cnode_flags_set(top_code(coder),cblock_flags,cblock_is_comm)
          call trav_xexpr(coder,cblock2,node,node_arg(node,1))
          if(sym==sym_until_invar) then
             call code_check_invar(coder,cblock2,node,top_code(coder))
          endif
          call close_cblock(coder,cblock2)
          do j=coder%vtop-1,coder%vtop
             write(*,*) '++++++++++++++++',j
             call qdump_code_tree(coder,pm_null_obj,6,coder%vstack(j),2)
          enddo
          write(*,*) '+++++++++++++++++++++'
          call get_lex_scope(coder,node)
          call make_sp_call(coder,cblock,node,&
               sym_until,3,0)
          call pop_lex_scope(coder)
          coder%par_state=save_par_state
       case(sym_do_stmt)
          if(node_numargs(node)==1) then
             call trav_stmt_list(coder,cblock,node,node_arg(node,1),sym_do)
             call make_sp_call(coder,cblock,node,sym_do,1,0)
          else
             call make_block_proc(coder,cblock,node,&
                  node_arg(node,1),node_num_arg(node,2),&
                  pm_null_obj,0,&
                  node_arg(node,4))
             call trav_call(coder,cblock,node,node_arg(node,3),0,.true.)
          endif
       case(sym_proceed)
          continue
       case(sym_mode)
          call trav_mode_stmt(coder,cblock,node,sym,.false.)
       case(sym_for)
          call trav_xexpr(coder,cblock,node,node_arg(node,2),node)
       case(sym_each,sym_foreach_invar)
          call trav_xexpr(coder,cblock,node,node_arg(node,1),node)
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
             call make_var(coder,cblock,node,node_arg(node,j),&
                  ior(var_is_not_inited,&
                  merge(var_is_var,0,sym==sym_var)))
          enddo
          call push_word(coder,pm_type_new_uninitialised)
          call push_word(coder,0)
          call push_word(coder,pm_type_new_type)
          call push_word(coder,0)
          call trav_type(coder,node,node_arg(node,node_numargs(node)))
          call make_type(coder,3)
          call make_type(coder,3)
          call code_num(coder,pop_word(coder))
          call make_sp_call(coder,cblock,node,sym_var,node_numargs(node)-1,1)
       case(sym_with)
          base=coder%top
          call trav_open_stmt_list(coder,cblock,node,node_arg(node,1))
          j=coder%top
          call trav_open_stmt_list(coder,cblock,node,node_arg(node,2))
          call hide_vars(coder,base+1,j)
       case(sym_over)
!          call trav_over_stmt(coder,cblock,list,node)
       case(sym_assign)
          call trav_assign_define(coder,cblock,list,node)
       case(sym_assign_list)
          call trav_assign_define_list(coder,cblock,list,node)
       case(sym_sync_assign)
          call trav_sync_assign(coder,cblock,list,node)
       case(sym_where,sym_check,sym_amp)
          call trav_xexpr(coder,cblock,listp,node)
       case(sym_sync)
          select case(coder%par_state)
          case(par_state_cond,par_state_par)
             save_par_state=coder%par_state
             coder%par_state=par_state_labelled
             call check_par_context(coder,cblock,node,.false.)
             coder%label=node_arg(node,2)
             call make_const(coder,cblock,node,&
                  node_arg(node,2))
             call trav_stmt_list(coder,cblock,node,node_arg(node,1),sym_sync)
             call make_sp_call(coder,cblock,node,sym_sync,2,0)
             coder%par_state=save_par_state
             coder%label=coder%default_label
          case(par_state_outer)
             call code_error(coder,node,&
                  'Labelled statement not allowed outside of any parallel statement')
          case(par_state_nhd)
             call code_error(coder,node,&
                  'Labelled statement not allowed in a "nhd" statement')
          case(par_state_any)
             call code_error(coder,node,&
                  'Labelled statement not allowed in an "any" statement')
          case(par_state_cond_loop)
             call code_error(coder,node,&
                  'Labelled statement in unlabelled loop')
          case(par_state_for,par_state_loop,par_state_masked,par_state_over,par_state_labelled)
             call trav_open_stmt_list(coder,cblock,node,node_arg(node,1))
          case default
             write(*,*) 'Par state=',coder%par_state
             call pm_panic('Unknown par state (sym_sync)')
          end select
       case(sym_par)
 !         call trav_par_stmt(coder,cblock,list,node)
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
                if(coder%par_state==par_state_outer) then
                   call make_sys_call(coder,cblock,node,sym_pm_dump,j-1,0)
                else
                   call make_comm_sys_call(coder,cblock,node,sym_pm_dump,j-1,0)
                endif
             endif
          case default
             call code_error(coder,node,'Pragma not recognised: $$'//&
                  trim(pm_name_as_string(coder%context,n)),warn=.true.)
          end select
       case(sym_pm_send:sym_pm_serve)
          base=coder%vtop
          call make_sys_var(coder,cblock,node,node_get_num(node,node_args),&
               var_is_shadowed)
          call set_var_as_shared(coder,top_code(coder))
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
          call set_var_as_shared(coder,pop_code(coder))
          call check_par_context(coder,cblock,node,.false.)
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
          call check_par_context(coder,cblock,node,.false.)
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
       case(sym_pm_head_node)
          call trav_stmt_list(coder,cblock,node,node_arg(node,1),sym_caret)
          call make_sp_call(coder,cblock,node,sym,1,0)
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
    if(coder%block_base>0) write(*,*) 'access var',modify,&
         trim(pm_name_as_string(coder%context,cnode_get_num(var,var_name))),&
         cnode_get_num(var,var_index)
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
    do while(var_lex_scope<lex_scope)
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

  subroutine make_block_proc(coder,cblock,stmt,namelist,amps,rtns,nret,stmtlist)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,stmt,namelist,rtns,stmtlist
    integer,intent(in):: amps,nret

    type(pm_ptr):: cblock2,cblock3,proc
    integer:: nargs,base,i,partype,restype,flags
    logical:: varargs
    integer:: name,save_index,save_ncalls
    integer:: signo,args(1)
    character(len=12):: namestr

    write(*,*) 'START--->',coder%wtop,coder%vtop
    
    nargs=node_numargs(namelist)
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

    write(*,*) 'REGAIN--->',coder%wtop,coder%vtop

    call make_sys_var(coder,cblock,stmt,sym_block_proc_a,var_is_shadowed)
    
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
    call make_code(coder,stmt,cnode_is_proc,pr_node_size)
    proc=top_code(coder)
    
    write(*,*) 'AGAIN--->',coder%wtop,coder%vtop
    
    ! Create one-element signature
    call make_code(coder,stmt,cnode_is_callsig,1)

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

    write(*,*) 'proctyp=',trim(pm_type_as_string(coder%context,top_word(coder)))
    
    call make_const(coder,cblock,stmt,&
         pm_fast_name(coder%context,name),pop_word(coder))
    call make_sys_call(coder,cblock,stmt,sym_dup,1,1)

 
    save_index=coder%index
    save_ncalls=coder%proc_ncalls
    coder%index=0
    coder%proc_ncalls=0
    
    call push_block_scope(coder,cblock2)

                       write(*,*) 'MARZ'
             do i=1,coder%top
                write(*,*) pm_name_as_string(coder%context,coder%stack(i))
             enddo
    write(*,*) '---end---'

    base=coder%top
    
    ! Create state variable parameters
    call make_sys_var(coder,cblock2,stmt,sym_topology,var_is_param+var_is_shadowed)
    call make_sys_var(coder,cblock2,stmt,sym_outer,var_is_param+var_is_shadowed)
    call make_sys_var(coder,cblock2,stmt,sym_region,var_is_param+var_is_shadowed)
    call make_sys_var(coder,cblock2,stmt,sym_subregion,var_is_param+var_is_shadowed)
    call make_sys_var(coder,cblock2,stmt,sym_here_in_tile,&
         var_is_param+var_is_shadowed)
    call make_sys_var(coder,cblock2,stmt,sym_mask,var_is_param+var_is_shadowed)

    ! Create variables for block imports and exports
    call make_sys_var(coder,cblock2,stmt,&
         sym_block_inouts,var_is_param+var_is_ref+var_is_var)
    call make_sys_var(coder,cblock2,stmt,&
         sym_block_ins,var_is_param)
 
    ! Remaining parameter variables
    call trav_params(coder,cblock2,namelist,amps,1,8)
 
    write(*,*) 'THEN--->',coder%wtop,coder%vtop
 
    cblock3=make_cblock(coder,cblock2,stmtlist,sym_do_stmt)
    coder%lex_scope=coder%lex_scope+1

 
    call trav_open_stmt_list(coder,cblock3,stmt,stmtlist)
   
    call trav_xexpr(coder,cblock3,stmt,rtns)
    coder%lex_scope=coder%lex_scope-1
    call close_cblock(coder,cblock3)

                             write(*,*) 'CARZ'
             do i=1,coder%top
                write(*,*) pm_name_as_string(coder%context,coder%stack(i))
             enddo
    write(*,*) '---end---'
    
    call extract_block_vars(coder,cblock2,stmt,coder%var(base+7),.true.)
    call extract_block_vars(coder,cblock2,stmt,coder%var(base+8),.false.)
    call make_sp_call(coder,cblock2,stmt,sym_do,1,0)

    call cnode_set_num(proc,pr_max_index,coder%index)
    call cnode_set_num(proc,pr_ncalls,coder%proc_ncalls)
    coder%index=save_index
    coder%proc_ncalls=save_ncalls
    
    write(*,*) 'AFTA--->',coder%wtop,coder%vtop
    call close_cblock(coder,cblock2)

                       write(*,*) 'VARZ'
             do i=1,coder%top
                write(*,*) pm_name_as_string(coder%context,coder%stack(i))
             enddo
    write(*,*) '---end---'
    
    call pop_block_scope(coder,cblock,stmt)


    
    write(*,*) 'FINALLY--->',coder%wtop,coder%vtop

  contains
    include 'fisnull.inc'
    include 'fname.inc'
  end subroutine make_block_proc

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

  subroutine push_block_vars(coder,list,access,n)
    type(code_state),intent(inout):: coder
    logical,intent(in):: access
    type(pm_ptr),intent(in):: list
    integer,intent(out):: n

    type(pm_ptr):: p,var
    integer:: index,i

    p=list
    i=0
    do while(.not.pm_fast_isnull(p))
       index=p%data%ptr(p%offset)%offset
       var=coder%var(index)
       if(iand(cnode_get_num(var,var_flags),var_is_changed)/=0.eqv.access) then
          call code_val(coder,var)
          i=i+1
       endif
       p=p%data%ptr(p%offset+1)
    enddo
    n=i
  contains
    include 'fisnull.inc'
  end subroutine  push_block_vars
  
  subroutine extract_var(coder,cblock,node,var,avar,index)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,var,avar
    integer,intent(in):: index

    call code_val(coder,var)
    call code_val(coder,avar)
    call make_long_const(coder,cblock,node,int(index,pm_ln))
    call make_comm_sys_call(coder,cblock,node,sym_elem_at_index,2,1,&
         aflags=proccall_is_ref,assign=.true.)
  end subroutine extract_var

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

  subroutine pop_block_scope(coder,cblock,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    type(pm_ptr):: list
    type(pm_ptr)::p,var
    integer:: index,nwrites,nreads
    if(pm_debug_checks) then
       if(coder%wtop/=coder%block_entry+3) then
          call pm_panic("pop_block_scope: wstack")
       endif
       if(coder%vtop-1/=coder%wstack(coder%block_entry+3)) then
          call pm_panic("pop_block_scope: vstack")
       endif
    endif
    list=coder%vstack(coder%vtop-1)
    coder%temp2=list
    coder%block_entry=coder%wstack(coder%block_entry)
    coder%block_base=coder%wstack(coder%block_entry+1)
    coder%vtop=coder%vtop-2
    coder%wtop=coder%wtop-4
    p=list
    nwrites=0
    call make_sys_var(coder,cblock,node,sym_block_inouts_a,&
         var_is_shadowed+var_is_var+var_is_ref)
    do while(.not.pm_fast_isnull(p))
       index=p%data%ptr(p%offset)%offset
       var=coder%var(index)
       if(cnode_flags_set(var,var_flags,var_is_changed)) then
          call code_val(coder,cnode_get(var,var_extra_info))
          nwrites=nwrites+1
       endif
       p=p%data%ptr(p%offset+1)
    enddo
    if(nwrites>0) then
       call make_sp_call(coder,cblock,node,sym_open_smiley,nwrites,1)
    else
       call make_sp_call(coder,cblock,node,sym_null,0,1)
    endif
    p=list
    nreads=0
    call make_sys_var(coder,cblock,node,sym_block_ins_a,var_is_shadowed)
    do while(.not.pm_fast_isnull(p))
       index=p%data%ptr(p%offset)%offset
       var=coder%var(index)
       write(*,*) 'REAd',&
            trim(pm_name_as_string(coder%context,cnode_get_num(var,var_name)))
       if(cnode_flags_clear(var,var_flags,var_is_changed)) then
          var=cnode_get(coder%var(index),var_extra_info)
          call code_val(coder,var)
          nreads=nreads+1
       else
          var=cnode_get(coder%var(index),var_extra_info)
       endif
       coder%var(index)=var
       p=p%data%ptr(p%offset+1)
    enddo
    if(nreads>0) then
       call make_sp_call(coder,cblock,node,sym_open_smiley,nreads,1)
    else
       call make_sp_call(coder,cblock,node,sym_null,0,1)
    endif
    coder%temp2=pm_null_obj
  contains
    include 'fisnull.inc'
  end subroutine  pop_block_scope

  recursive subroutine import_to_block_scope(coder,index,var,block_entry)
    type(code_state),intent(inout):: coder
    integer,intent(in):: index,block_entry
    type(pm_ptr),intent(inout):: var
    integer:: var_scope,block_scope,block_links
    write(*,*) 'import_to_block_scope',block_entry,&
         trim(pm_name_as_string(coder%context,cnode_get_num(var,var_name)))
    if(block_entry==0) return
    var_scope=cnode_get_num(var,var_lex_scope)
    block_scope=coder%wstack(block_entry+2)
    block_links=coder%wstack(block_entry+3)
    write(*,*) 'with',var_scope,block_scope,block_entry
    if(var_scope>=block_scope) return
    write(*,*) 'recursing with',coder%wstack(block_entry)
    call import_to_block_scope(coder,index,var,coder%wstack(block_entry))
    call make_var(coder,&
         coder%vstack(block_links+1),&
         pm_null_obj,&
         cnode_get(var,var_name),&
         ior(cnode_get_num(var,var_flags),var_is_imported),&
         extra_info=var)
    var=pop_code(coder)
    call cnode_set_num(var,var_lex_scope,coder%wstack(block_entry+2))
    write(*,*) 'lex scope now',coder%wstack(block_entry+2)
    write(*,*) 'index now',cnode_get_num(var,var_index)
    call add_to_change_list(coder,coder%vstack(block_links),&
         pm_fast_tinyint(coder%context,index))
    write(*,*) 'pushing block var',index
    call qdump_code_tree(coder,pm_null_obj,6,var,2)
    coder%var(index)=var
  contains
    include 'fisnull.inc'
    include 'ftiny.inc'
  end subroutine import_to_block_scope


  !==============================================================
  ! Traverse extended expression: expr [check expr] { where ...}
  !==============================================================
  recursive subroutine trav_xexpr(coder,cblock,exprp,exprn,stmt)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,exprp,exprn
    type(pm_ptr),intent(in),optional:: stmt
    type(pm_ptr)::p,ass
    integer:: i,j,base,top
    p=exprn
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
    else
       base=-1
    endif
    if(node_sym(p)==sym_check) then
       call apply_x(p,node_arg(p,1))
       call make_check(coder,cblock,p,0)
    else
       call apply_x(exprp,p)
    endif
    if(base>=0) call hide_vars(coder,base+1,top)
  contains
    include 'fisnull.inc'
    include 'fisname.inc'
    include 'fname.inc'
    include 'ftiny.inc'

    subroutine apply_x(nodep,node)
      type(pm_ptr),intent(in):: nodep,node
      type(pm_ptr):: nodei
      integer:: wbase,i,flags,numret
      logical:: outer

      wbase=coder%wtop
      if(pm_fast_isnull(node)) return
      select case(node_sym(node))
      case(sym_assign)
         call trav_assign_define(coder,cblock,nodep,node)
      case(sym_case)
         do i=1,node_numargs(node)
            nodei=node_arg(node,i)
            if(node_sym(nodei)==sym_dotdot) then
               call trav_expr(coder,cblock,node,node_arg(nodei,1))
               call trav_expr(coder,cblock,node,node_arg(nodei,2))
               call make_sys_call_rtn(coder,cblock,node,sym_case_range,2,1)
            else
               call trav_expr(coder,cblock,node,nodei)
            endif
         enddo
      case(sym_assign_list)
         call trav_assign_define_list(coder,cblock,nodep,node)
      case(sym_sync_assign)
         call trav_sync_assign(coder,cblock,nodep,node)
      case(sym_iter)
         if(node_sym(stmt)==sym_for) then
!            call trav_for_stmt(coder,cblock,nodep,node,base,stmt)
         else
            call trav_foreach_stmt(coder,cblock,nodep,node,base,stmt)
         endif
      case(sym_list)
         call trav_exprlist(coder,cblock,nodep,node)
      case(sym_result)
         call push_word(coder,pm_type_is_tuple)
         call push_word(coder,0)
         do i=1,node_numargs(node),2
            call trav_expr(coder,cblock,node,node_arg(node,i))
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

  end subroutine trav_xexpr


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
          call make_const(coder,cblock,p,coder%check_mess)
          call code_null(coder)
       elseif(node_sym(mess)==sym_string) then
          call make_const(coder,cblock,p,node_arg(mess,1))
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
  ! switch statement - cases and otherwise clause
  ! assumes expression is in var
  !========================================================
  recursive subroutine trav_switch_stmt(coder,cblock,stmt,idx,var,sym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    type(pm_ptr),intent(in):: stmt,var
    integer,intent(in):: idx,sym
    type(pm_ptr):: cblock2
    integer:: base,save_par_state,n,i,j,lex_scope
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
       call code_check_invar(coder,cblock,stmt,top_code(coder))
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
  ! Traverse statement qualified by a mode
  !========================================================
  recursive subroutine trav_mode_stmt(coder,cblock,node,nsym,isexpr)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: nsym
    logical,intent(in):: isexpr
    integer:: sym,save_run_mode,save_run_flags,save_par_state
!!$    sym=node_num_arg(node,2)
!!$    if(coder%par_state==par_state_outer) then
!!$       call code_error(coder,node,'Cannot have "'//&
!!$            trim(sym_names(sym))//&
!!$            '" statement outside of a parallel context')
!!$    elseif(coder%par_state==par_state_nhd) then
!!$       call code_error(coder,node,'Cannot have "'//&
!!$            trim(sym_names(sym))//&
!!$            '" statement in main body of "nhd" statement')
!!$    endif
!!$    
!!$    save_run_mode=coder%run_mode
!!$    save_run_flags=coder%run_flags
!!$    coder%run_mode=sym
!!$    select case(sym)
!!$    case(sym_coherent:sym_invar)
!!$       coder%run_flags=proc_run_complete+proc_run_always
!!$    case(sym_shared)
!!$       coder%run_flags=proc_run_shared+proc_run_always
!!$    end select
!!$    if(isexpr) then
!!$       call trav_expr(coder,cblock,node,node_arg(node,1))
!!$    else
!!$       call trav_open_stmt_list(coder,cblock,node,node_arg(node,1))
!!$    endif
!!$    coder%run_mode=save_run_mode
!!$    coder%run_flags=save_run_flags
  end subroutine trav_mode_stmt

  !========================================================
  ! Traverse "any" statement
  !========================================================
  recursive subroutine trav_any_stmt(coder,cblock,pnode,node,sym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer,intent(in):: sym
    integer:: k,i,j,n,flags,start,finish,vb,lex_scope
    type(pm_ptr):: cblock2,vlist,v,var
    integer:: save_par_state

    lex_scope=push_lex_scope(coder)
    
    if(pm_fast_isnull(node_arg(node,2))) then
       flags=var_is_shadowed+var_is_var
       call trav_expr(coder,cblock,node,node_arg(node,1))
    else
       flags=var_is_var
       call trav_xexpr(coder,cblock,node,node_arg(node,2))
    endif
    v=top_code(coder)
    save_par_state=coder%par_state
    if(sym==sym_any) then
       coder%par_state=par_state_any
    else
       call code_check_invar(coder,cblock,node,v)
    endif
    cblock2=make_cblock(coder,cblock,node,sym_any)
    call make_var(coder,cblock,node,node_arg(node_arg(node,1),1),flags)
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
    coder%par_state=save_par_state
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
  end subroutine trav_any_stmt

  !========================================================
  ! Traverse a for each statement
  ! -- also used for foreach clause of nhd statement
  !========================================================
  recursive subroutine trav_foreach_stmt(coder,cblock,listp,list,base,stmt,nhd,nbase,nvars)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,listp,list,stmt
    integer,intent(in):: base
    type(pm_ptr),intent(in),optional:: nhd
    integer,intent(in),optional:: nbase,nvars
    type(pm_ptr):: var,vlist
    type(pm_ptr):: cblock2,cblock3,cblock4
    type(pm_ptr):: cblock_main
    integer:: i,j,k,n,lbase,vbase,xbase,xbasev,lex_scope
    integer:: nlist,iter,iter2,sym,rbase,wbase,name,flags,sindex,sbase
    integer:: slot1,slot2,while_var,outmode,rflags
    integer(pm_p)::flag
    type(pm_ptr):: vlhs,procs,sig,xvar,p
    integer:: save_par_state,save_run_flags,save_run_mode
    type(pm_ptr):: save_loop_cblock
    logical:: iscomm,outer,invar,c_invar
    sym=node_sym(stmt)

    wbase=coder%wtop
    invar=sym==sym_foreach_invar
    c_invar=pm_is_compiling.and.invar
    rflags=merge(proc_run_shared+proc_run_always,0,c_invar)

    if(debug_codegen) write(*,*) 'TRAVEACH>'

    lex_scope=push_lex_scope(coder)
    rbase=coder%vtop
    
    ! Process iterator expression
    call trav_iter(coder,cblock,list,sym_dims,lbase,vbase,nlist)

    ! Check invariance of for-each-invar expressions
    if(invar) then
       do i=1,nlist/2
          call code_check_invar(coder,cblock,node_arg(list,i*2),&
               coder%vstack(lbase+i))
       enddo
    endif
    
    ! Hide any where clauses (may need them later)
    if(base>=0) then
       call hide_vars(coder,base+1,coder%top)
    endif

    if(.not.pm_is_compiling) then
       call make_const(coder,cblock,stmt,node_arg(stmt,4))
    endif
    
    save_par_state=coder%par_state
!!$    coder%par_state=par_state_for_loop(coder,stmt,coder%par_state,&
!!$         node_get_num(stmt,node_args+3)/=0,sym==sym_foreach_invar)
!!$   
    ! Start for-each loop
    iter=call_start(coder,cblock,list,invar)
    
    ! Get array/domain elements for first iteration
    xbase=coder%top
    do i=1,nlist/2
       call make_var(coder,cblock,list,&
            node_arg(list,i*2-1),var_is_var)
       call code_val(coder,coder%vstack(lbase+i))
       call code_val(coder,coder%var(iter+lv_idx))
       call make_sys_call(coder,cblock,list,sym_get_element,2,1,&
            aflags=rflags)
    enddo
    
    ! While clause
    if(.not.pm_fast_isnull(node_arg(stmt,2))) then
       p=node_arg(stmt,2)
       if(node_sym(p)==sym_while) then
          call code_val(coder,coder%var(iter+lv_end))
          call trav_xexpr(coder,cblock,p,node_arg(p,1))
          if(invar) then
             call code_check_invar(coder,cblock,p,top_code(coder))
          endif
          call make_sys_call_rtn(coder,cblock,stmt,sym_and,2,1,aflags=rflags)
          call make_var_assignment(coder,cblock,stmt,coder%var(iter+lv_end),aflags=rflags)
       endif
    endif

    coder%lex_scope=lex_scope
    
    ! Loop body
    cblock2=make_cblock(coder,cblock,list,sym_each)
    call trav_open_stmt_list(coder,cblock2,stmt,node_arg(stmt,3))
    
    ! Modify changed array elements
    do i=1,nlist/2
       var=coder%var(xbase+i)
       if(cnode_flags_set(var,var_flags,var_is_changed)) then
          outmode=trav_ref(coder,cblock2,list,&
               node_arg(list,i*2),0)
          call code_val(coder,var)
          call code_val(coder,coder%var(iter+lv_idx))
          call make_assign_call(coder,cblock2,list,sym_set_element,3,0,aflags=rflags)
       endif
    enddo
    
    ! Next iteration
    if(.not.pm_fast_isnull(node_arg(stmt,2))) then
       ! While/until
       p=node_arg(stmt,2)
       if(node_sym(p)/=sym_while) then
          lex_scope=push_lex_scope(coder)
          call trav_xexpr(coder,cblock2,stmt,node_arg(p,1))
          if(sym==sym_foreach_invar) then
             call code_check_invar(coder,cblock2,p,top_code(coder))
          endif
          coder%lex_scope=lex_scope
          cblock_main=make_cblock(coder,cblock2,stmt,sym_each)
          call make_const(coder,cblock_main,stmt,coder%false)
          call make_var_assignment(coder,cblock_main,stmt,&
               coder%var(iter+lv_end),aflags=rflags)
          call close_cblock(coder,cblock_main)
          cblock_main=make_cblock(coder,cblock2,stmt,sym_each)
          call call_next(coder,cblock_main,list,iter,invar)
          call close_cblock(coder,cblock_main)
          call get_lex_scope(coder,stmt)
          call make_sp_call(coder,cblock2,stmt,merge(sym_if_invar,sym_if,c_invar),4,0)
          call pop_lex_scope(coder)
       else
          call call_next(coder,cblock2,list,iter,invar)
       endif
    else
       call call_next(coder,cblock2,list,iter,invar)
    endif

    ! Get elements for next iteration
    lex_scope=push_lex_scope(coder)
    call code_val(coder,coder%var(iter+lv_end))
    coder%lex_scope=lex_scope
    cblock_main=make_cblock(coder,cblock2,stmt,sym_each)
    do i=1,nlist/2
       call code_val(coder,coder%vstack(lbase+i))
       call code_val(coder,coder%var(iter+lv_idx))
       call make_sys_call_rtn(coder,cblock_main,list,sym_get_element,2,1,aflags=rflags)
       call make_var_assignment(coder,cblock_main,stmt,coder%var(xbase+i),aflags=rflags)
    enddo
    if(.not.pm_fast_isnull(node_arg(stmt,2))) then
       p=node_arg(stmt,2)
       if(node_sym(p)==sym_while) then
          call trav_xexpr(coder,cblock_main,p,node_arg(p,1))
          if(invar) then
             call code_check_invar(coder,cblock,p,top_code(coder))
          endif
          call make_var_assignment(coder,cblock_main,stmt,&
               coder%var(iter+lv_end),aflags=rflags)
       endif
    endif
    call close_cblock(coder,cblock_main)
    call code_null(coder)
    call get_lex_scope(coder,stmt)
    call make_sp_call(coder,cblock2,stmt,merge(sym_if_invar,sym_if,c_invar),4,0)
    call pop_lex_scope(coder)
    call close_cblock(coder,cblock2)
    
    ! Build call
    call code_val(coder,coder%var(iter+lv_end))
    call get_lex_scope(coder,stmt)
    call make_sp_call(coder,cblock,list,sym_each,4,0)
    
    ! Clean up
    coder%par_state=save_par_state
    coder%vtop=rbase
    
    call pop_vars_to(coder,vbase)

    call pop_lex_scope(coder)

  contains
    include 'fisnull.inc'
    include 'fisname.inc'
    include 'fname.inc'
    include 'ftiny.inc'

  end subroutine trav_foreach_stmt

  !=========================================================
  ! Call iter,state,end=first(domain)
  ! -- Domain must be variable at top of variable (not value)
  !    stack and also must not be shared (cannot import)
  !=========================================================
  function call_start(coder,cblock,list,invar) result(iter)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,list
    logical,intent(in):: invar
    integer:: iter,i
    integer:: save_run_flags

    if(invar.and.pm_is_compiling) then
       save_run_flags=coder%run_flags
       coder%run_flags=proc_run_shared+proc_run_always
    endif
    
    iter=coder%top
    
    ! Code iter,state,not_end=first(domain)   
    call code_val(coder,coder%var(iter))
    call make_sys_call_rtn(coder,cblock,list,sym_first,1,3)
    
    ! Loop end
    call define_sys_var(coder,cblock,list,sym_for_stmt,&
         var_is_shadowed+var_is_var)
    ! State
    call define_sys_var(coder,cblock,list,sym_pling,&
         var_is_shadowed+var_is_var)
    ! Iterator
    call define_sys_var(coder,cblock,list,sym_iter,&
         var_is_shadowed+var_is_var)

    if(invar) then
       do i=coder%top-2,coder%top
          call code_val(coder,coder%var(i))
          call code_num(coder,sym_invar)
          call make_basic_sp_call(coder,cblock,list,sym_set_mode,2,0,coder%par_depth)
       enddo
       if(pm_is_compiling) then
          coder%run_flags=save_run_flags
       endif
    endif

  end function  call_start

  !========================================================
  ! Code either iter,state,end=next(domain,state,iter) 
  !========================================================
  subroutine call_next(coder,cblock,list,iter,invar)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,list
    integer,intent(in):: iter
    logical,intent(in):: invar
    type(pm_ptr):: dvar,ivar,svar,evar
    integer:: save_run_flags
    
    if(invar.and.pm_is_compiling) then
       save_run_flags=coder%run_flags
       coder%run_flags=proc_run_shared+proc_run_always
    endif
    
    dvar=coder%var(iter)
    ivar=coder%var(iter+lv_idx)
    svar=coder%var(iter+lv_state)
    evar=coder%var(iter+lv_end)
    call code_val(coder,dvar)
    call code_val(coder,svar)
    call code_val(coder,ivar)
    call make_sys_call_rtn(coder,cblock,list,sym_next,3,3)
    call make_var_assignment(coder,cblock,list,evar)
    call make_var_assignment(coder,cblock,list,svar)
    call make_var_assignment(coder,cblock,list,ivar)

    if(invar.and.pm_is_compiling) then
       coder%run_flags=save_run_flags
    endif
    
  end subroutine call_next

  
  subroutine check_par_context(coder,list_head,node,cond_is_ok)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: list_head,node
    logical,intent(in):: cond_is_ok
    type(pm_ptr):: list
    integer:: i
    select case(coder%par_state)
    case(par_state_for,par_state_loop,par_state_masked,par_state_over,&
         par_state_labelled)
       continue
    case(par_state_outer)
       call code_error(coder,node,&
            'Cannot have communicating operation outside of any parallel statement')
       return
    case(par_state_nhd)
       call code_error(coder,node,&
            'Cannot have communicating operation in the main body of a "nhd" statement')
       return
    case(par_state_any)
       if(.not.cond_is_ok) then
          call code_error(coder,node,&
               'Cannot have active communicating operation in an "any" statement')
       endif
    case(par_state_cond)
       if(.not.cond_is_ok) then
          call code_error(coder,node,&
               'Unlabelled communicating operation in conditional statement')
       endif
    case(par_state_cond_loop)
       if(.not.cond_is_ok) then
          call code_error(coder,node,&
               'Communicating operation in unlabelled loop')
       endif
    case(par_state_par)
       if(.not.cond_is_ok) then
          call code_error(coder,node,&
               'Unlabelled communicating operation in par statement')
     endif
    case default
       write(*,*) 'Par state=',coder%par_state
       call pm_panic('Unknown par state')
    end select
  end subroutine check_par_context


  !========================================================
  ! Code a check if value is invariant
  !========================================================
  subroutine code_check_invar(coder,cblock,node,val)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,val
    if(var_private(coder,val)) then
       call code_val(coder,val)
       call make_sp_call(coder,cblock,node,sym_invar,1,0,flags=call_is_no_touch)
    endif
  end subroutine code_check_invar

  !========================================================
  ! Iteration clause ' x in A, y in B '
  !========================================================
  recursive subroutine trav_iter(coder,cblock,list,shape_sym,lbase,vbase,nlist)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,list
    integer,intent(out):: nlist,lbase,vbase
    integer,intent(in):: shape_sym
    integer:: i
    
    ! Expressions to iterate over
    nlist=node_numargs(list)
    lbase=coder%vtop
    do i=2,nlist,2
       call trav_expr(coder,cblock,list,node_arg(list,i))
    enddo
    
    vbase=coder%top

    ! Check that all elements conform
    do i=2,nlist/2
       if(node_get_num(list,(i-1)*2+node_args)>0) then
          call repl_expr(coder,lbase+1)
          call repl_expr(coder,lbase+i)
          call make_sys_call(coder,cblock,list,&
               sym_check_conform,2,0)
       endif
    enddo

    ! Calculate common iteration domain from 1st element in list
    call repl_expr(coder,lbase+1)
    call make_sys_call_rtn(coder,cblock,list,shape_sym,1,1)
    call define_sys_var(coder,cblock,list,sym_for,var_is_shadowed)
  end subroutine trav_iter  

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
!!$       call code_error(coder,node,'"par" statement has only one branch')
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
    n=node_numargs(lhs)
    if(sym/=sym_assign) n=n-1
    call trav_rhs(coder,cblock,node,rhs,n)
    call trav_lhs(coder,cblock,node,lhs,rhs)
    coder%vtop=base
  end subroutine trav_assign_define

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
       n=node_numargs(lhs)
       if(sym/=sym_assign) n=n-1
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
  subroutine trav_lhs(coder,cblock,node,lhs,rhs)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,lhs,rhs
    integer:: i,n,sym
    type(pm_ptr):: rhs_val
    n=node_numargs(lhs)
    sym=node_sym(lhs)
    select case(sym)
    case(sym_var,sym_const)
       do i=n-1,1,-1
          call trav_cast(coder,cblock,lhs,node_arg(lhs,n),sym)
          call make_definition(coder,cblock,lhs,node_arg(lhs,i),&
               merge(0,var_is_var,sym==sym_const))
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
    type(pm_ptr):: name
    integer:: rsym
    if(pm_fast_isname(lhs)) then
       name=lhs
    elseif(node_sym(lhs)==sym_name) then
       name=node_arg(lhs,1)
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
       if(iand(cnode_get_num(var,var_flags),&
            var_is_var+var_is_not_inited)==var_is_not_inited) then
          call make_split_definition(coder,cblock,node,var)
       else
          call make_assignment(coder,cblock,node,lhs,rhs,var)
       endif
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
  ! Traverse sync lhs = rhs
  !========================================================
  subroutine trav_sync_assign(coder,cblock,pnode,node)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: save_par_state,n,base
    n=node_numargs(node)
    base=coder%top
    ! Make a shadow variable to prevent access to LHS var on RHS
    ! or subscripts
    ! This keeps a link to the node to ensure that we can still
    ! access variable in the one place we need to
    call make_var(coder,cblock,node,node_arg(node_arg(node,1),1),&
         var_is_shadowed+var_is_sync+var_is_var,extra_info=node_arg(node,1))
    call drop_code(coder)
    call trav_expr(coder,cblock,node,node_arg(node,n))
    call check_par_context(coder,cblock,pnode,.false.)
    save_par_state=coder%par_state
    coder%in_sync=.true.
    if(n==3) then
       call make_assignment_noalias(coder,cblock,node,node_arg(node,2))
    else
       call make_op_assignment_noalias(coder,cblock,node,&
            node_arg(node,2),node_arg(node,3))
    endif
    ! Null out link to node to prevent retaining link to module
    call cnode_set(coder%context,coder%var(base+1),var_extra_info,pm_null_obj)
    call pop_vars_to(coder,base)
    coder%par_state=save_par_state
  end subroutine trav_sync_assign

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
    type(pm_ptr):: rname,lname,cblock1,cblock2
    rsym=node_sym(rhs)
    lsym=node_sym(lhs)
    if(rsym==sym_sub.and.lsym==sym_sub) then
       rbase=coder%vtop
       ok=get_ref_pattern(coder,rhs,rname)
       lbase=coder%vtop
       ok=get_ref_pattern(coder,lhs,lname)
       if(lname%offset/=rname%offset) then
          coder%vtop=rbase
          call make_assignment_noalias(coder,cblock,pnode,lhs,avar)
       elseif(match_ref_names(coder,cblock,pnode,rbase,lbase)) then
          coder%vtop=rbase
          call make_assignment_noalias(coder,cblock,pnode,lhs,avar)
       else
          ! Code if <aliased> : <aliased_assign> else: <no_alias_assign>
          lex_scope=push_lex_scope(coder)
          call match_ref_pattern(coder,cblock,pnode,rbase,lbase,test=.true.)
          coder%vstack(rbase+1)=coder%vstack(coder%vtop)
          coder%vtop=rbase+1
          coder%lex_scope=lex_scope
          cblock1=make_cblock(coder,cblock,pnode,sym_if)
          call code_val(coder,coder%vstack(rbase))
          call make_assignment_noalias(coder,cblock1,pnode,lhs,avar,alias=.true.)
          call close_cblock(coder,cblock1)
          cblock2=make_cblock(coder,cblock,pnode,sym_if)
          call code_val(coder,coder%vstack(rbase))
          call make_assignment_noalias(coder,cblock2,pnode,lhs,avar)
          call close_cblock(coder,cblock2)
          call get_lex_scope(coder,pnode)
          call make_sp_call(coder,cblock,pnode,sym_if,4,0)
          call pop_lex_scope(coder)
       endif
    else
       call make_assignment_noalias(coder,cblock,pnode,lhs,avar)
    endif
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
    integer:: n,i,sym,flags,mode,depth,outmode
    type(pm_ptr):: v,w
    logical:: outer,shared
    if(present(avar)) then
       call trav_ref_to_var(coder,cblock,pnode,node,0,avar)
       call assign_call(pnode,outer,&
            cnode_flags_clear(top_code(coder),var_flags,var_is_ref),&
            cnode_flags_set(top_code(coder),var_flags,var_is_not_inited),&
            .false.)
    elseif(node_sym(node)==sym_underscore) then
       call drop_code(coder)
       return
    elseif(pm_fast_isname(node)) then
       call trav_ref_to_var(coder,cblock,pnode,node,0)
       call assign_call(pnode,outer,&
            cnode_flags_clear(top_code(coder),var_flags,var_is_ref),&
            cnode_flags_set(top_code(coder),var_flags,var_is_not_inited),&
            .false.)
    else
       sym=node_sym(node)
       select case(sym)
       case(sym_sub,sym_dot_sub,sym_dot,sym_get_dot,sym_at,sym_pling,sym_open_smiley)
          outmode=trav_ref(coder,cblock,pnode,node,0)
          call assign_call(node,outer,.false.,.false.,iand(outmode,ref_has_at)/=0)
       case(sym_name)
          call trav_ref_to_var(coder,cblock,node,node_arg(node,1),0)
          call assign_call(node,outer,&
               cnode_flags_clear(top_code(coder),var_flags,var_is_ref),&
               cnode_flags_set(top_code(coder),var_flags,var_is_not_inited),&
               .false.)
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

    subroutine assign_call(pnode,outer,simple,undef,has_pling)
      type(pm_ptr),intent(in):: pnode
      logical,intent(in):: outer,simple,undef,has_pling
      type(pm_ptr):: v,w
      if(.not.coder%in_sync) then
         if(simple.and.undef) then
            call dup_code(coder)
            call swap_code_2_1(coder)
            call make_sys_call(coder,cblock,pnode,&
                 sym_assign_or_init,2,1,aflags=call_is_uninitialised)
         else
            call swap_code(coder)
            call make_assign_call(coder,cblock,pnode,&
                 merge(sym_aliased_assign,&
                 merge(sym_assign_var,sym_assignment,simple),&
                 present(alias)),&
                 2,0,aflags=call_is_assign_call)
         endif
      else
         v=pop_code(coder)
         w=pop_code(coder)
         call code_val(coder,v)
         call code_val(coder,w)
         call make_static_bool_const(coder,cblock,pnode,has_pling)
         call check_par_context(coder,cblock,pnode,.true.)
         call make_comm_sys_call(coder,cblock,pnode,&
              merge(sym_aliased_assign,sym_assignment,present(alias)),&
              3,0,assign=.true.)
      endif
    end subroutine assign_call
    
  end subroutine make_assignment_noalias

  !========================================================
  ! Assign expression on top of stack to lhs in node
  !========================================================
  recursive subroutine make_op_assignment_noalias(coder,cblock,pnode,node,op)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node,op
    integer:: n,i,sym,flags,mode,depth,outmode
    type(pm_ptr):: v,w
    logical:: outer,shared
    if(node_sym(node)==sym_underscore) then
       call drop_code(coder)
       return
    endif
    outmode=trav_ref(coder,cblock,pnode,node,0)
    if(coder%in_sync) then
       v=pop_code(coder)
       w=pop_code(coder)
       call code_val(coder,v)
       call code_val(coder,w)
       if(node_sym(op)==sym_proc.and.node_sym(node_arg(op,1))==sym_minus) then
          call make_sys_call_rtn(coder,cblock,pnode,sym_minus,1,1)
       endif
       call trav_expr(coder,cblock,pnode,op)
       call make_static_bool_const(coder,cblock,pnode,&
            iand(outmode,ref_has_at)/=0)
       call check_par_context(coder,cblock,pnode,.true.)
       call make_comm_sys_call(coder,cblock,pnode,&
            sym_assignment,4,0,assign=.true.)
       call check_par_context(coder,cblock,pnode,.false.)
    else
       call swap_code(coder)
       if(node_sym(op)==sym_proc.and.node_sym(node_arg(op,1))==sym_minus) then
          call make_sys_call_rtn(coder,cblock,pnode,sym_minus,1,1)
       endif
       call trav_expr(coder,cblock,pnode,op)
       call make_assign_call(coder,cblock,pnode,sym_assignment,3,0)
    endif
  end subroutine make_op_assignment_noalias

  !===================================================================
  ! Use expression on top of stack to create new variable or constant
  !===================================================================
  recursive subroutine make_definition(coder,cblock,node,vname,flags)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,vname
    integer,intent(in):: flags
    integer:: junk,depth
    type(pm_ptr):: name,pnode,expr,var

    if(node_sym(vname)==sym_name) then
       name=node_arg(vname,1)
       pnode=vname
    else
       name=vname
       pnode=node
    endif
    if(pm_fast_isname(name)) then
       call make_var(coder,cblock,pnode,name,flags)
       var=top_code(coder)
       call swap_code(coder)
       call make_sys_call(coder,cblock,pnode,&
            merge(sym_dup,sym_clone,iand(flags,var_is_var)/=0),&
            1,1,aflags=coder%run_flags)
       call make_var_mode(coder,cblock,node,var)
    elseif(node_sym(name)==sym_underscore) then
       call drop_code(coder)
    else
       call code_error(coder,node,&
            'Left hand side of definition must be variable name')
    endif
  contains
    include 'fisname.inc'
    include 'fvkind.inc'
    include 'fisnull.inc'
  end subroutine make_definition

  !===================================================================
  ! Use expression on top of stack to initialise a constant
  !===================================================================
  recursive subroutine make_split_definition(coder,cblock,node,var)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,var
    call code_val(coder,var)
    call make_sp_call(coder,cblock,node,sym_const,1,0)
    call code_val(coder,var)
    call swap_code(coder)
    call make_sys_call(coder,cblock,node,sym_clone,&
         1,1)
    call update_change_lists(coder,var,.true.)
  end subroutine make_split_definition
  
  !========================================================
  ! Reference to a variable
  !========================================================
  subroutine trav_ref_to_var(coder,cblock,pnode,name,mode,avar)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,name
    integer,intent(in):: mode
    type(pm_ptr),intent(in),optional:: avar
    type(pm_ptr):: var
    integer:: depth,flags,var_index
    if(present(avar)) then
       var=avar
    else
       var=find_var_and_entry(coder,name,var_index)
       if(pm_fast_isnull(var)) then
          call code_error(coder,pnode,&
               'Variable has not been defined: ',name)
          call make_temp_var(coder,cblock,pnode)
          return
       endif
    endif
    flags=cnode_get_num(var,var_flags)

    if(iand(flags,var_is_sync)/=0) then
       if(.not.(cnode_get(var,var_extra_info)==pnode)) then
          call code_error(coder,pnode,&
               'Cannot access "sync" left-hand-side variable in right-hand-side expression or subscript')
       endif
       call hide_vars(coder,var_index,var_index)
       var=find_var(coder,name)
       if(pm_fast_isnull(var)) then
          call code_error(coder,pnode,&
               'Variable has not been defined: ',name)
          call make_temp_var(coder,cblock,pnode)
          return
       endif
       flags=cnode_get_num(var,var_flags)
       call reveal_vars(coder,var_index,var_index)
    endif
    
    if(iand(flags,var_is_aliased)/=0) then
       coder%aliased=.true.
       var=cnode_get(var,var_extra_info)
    endif
    if(.not.iand(mode,ref_is_val)/=0) then
       if(iand(flags,var_is_var)==0) then
          call code_error(coder,pnode,&
               'Cannot assign to constant: ',name)
       else
          call access_var(coder,var,.true.)
       endif
    endif
    if(iand(mode,ref_is_val)/=0) then
!       var=import_to_par_scope(coder,cblock,pnode,var,coder%par_depth)
    endif
    call code_val(coder,var)
    if(iand(mode,ref_is_val)==0) then
       if(coder%par_state>par_state_outer.and..not.coder%in_sync&
            .and.iand(mode,ref_ignores_rules)==0) then
          if(par_depth(coder,var)<coder%par_depth) then
             call make_basic_sp_call(coder,cblock,pnode,sym_amp_error,&
                  0,0,coder%par_depth)
          endif
       endif
       if(coder%par_state>=par_state_cond.and.&
            coder%par_state<=par_state_par) then
          call code_val(coder,var)
          call make_basic_sp_call(coder,cblock,pnode,&
               sym_assignment,1,0,coder%par_depth)
       endif
    endif
  contains
    include 'fisnull.inc'
  end subroutine trav_ref_to_var

  !========================================================
  ! Traverse a  reference value
  !========================================================
  recursive function trav_ref(coder,cblock,pnode,node,mode) result(outmode)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer,intent(in):: mode
    integer:: outmode
    logical:: outer
    integer:: newmode,sym,n,m,flags,depth,save_run_flags
    logical:: d_index
    integer:: aflags,acall,i
    type(pm_ptr):: p,q
    outer=.false.
    outmode=0
    if(pm_fast_isname(node)) then
       call trav_ref_to_var(coder,cblock,pnode,node,mode)
    else if(pm_fast_vkind(node)==pm_pointer) then
       sym=node_sym(node) 
       select case(sym)
       case(sym_sub,sym_dot_sub)
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          p=node_arg(node,2)
          d_index=trav_index_list(coder,cblock,p,iand(mode,ref_is_val)/=0)
          newmode=trav_ref(coder,cblock,node,node_arg(node,1),&
               merge(ior(mode,ref_is_dollar),mode,d_index))
          p=pop_code(coder)
          q=pop_code(coder)
          aflags=call_inline_when_compiling
          if(iand(mode,ref_is_val)/=0) then
             acall=merge(sym_make_noderef,sym_make_subref,sym==sym_dot_sub)
          else
             if(iand(mode,ref_is_amp)/=0) then
                acall=merge(sym_make_nodelhs_amp,sym_make_sublhs_amp,sym==sym_dot_sub)
                !call code_error(coder,node,'Cannot have "[]" in "&" argument')
             else
                acall=merge(sym_make_nodelhs,sym_make_sublhs,sym==sym_dot_sub)
             endif
          endif
          if(coder%in_sync.or.&
               coder%par_state>par_state_outer.and.iand(mode,ref_is_val)/=0) then
             call code_val(coder,p)
             call code_val(coder,q)
             call make_comm_sys_call(coder,cblock,node,acall,2,1,&
                  aflags=aflags)
             call check_par_context(coder,cblock,node,.true.)
          else
             if(sym==sym_dot_sub) then
                if(iand(mode,ref_is_val)/=0) then
                   call code_error(coder,node,&
                        'Cannot have a ".[]" subscript outside of a parallel context')
                else
                   call code_error(coder,node,&
                        'Cannot assign to a ".[]" subscript outside of a "sync" statement')
                endif
                call drop_code(coder)
             else
                call code_val(coder,p)
                call code_val(coder,q)
                call make_sys_call(coder,cblock,node,acall,2,1,aflags=ior(aflags,call_ignore_rules))
             endif
          endif
          outmode=ior(outmode,merge(ref_is_dollar+ref_is_subscripted,&
               ref_is_subscripted,d_index))
       case(sym_dot)
          call make_temp_var(coder,cblock,pnode)
          call dup_code(coder)
          outmode=trav_ref(coder,cblock,node,node_arg(node,1),mode)
          depth=par_depth(coder,top_code(coder))
          call make_const(coder,cblock,node,node_arg(node,2))
          call make_basic_sp_call(coder,cblock,node,&
               merge(sym_dot,sym_dot_ref,&
               iand(mode,ref_is_val)/=0),2,1,depth)
          call var_set_par_depth(coder,top_code(coder),depth)
       case(sym_get_dot)
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          outmode=trav_ref(coder,cblock,node,node_arg(node,1),mode)
          call trav_expr(coder,cblock,node,node_arg(node,2))
          call make_basic_sp_call(coder,cblock,node,&
               merge(sym_get_dot,sym_get_dot_ref,&
               iand(mode,ref_is_val)/=0),2,1,coder%par_depth)
       case(sym_get_dot_ref)
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          outmode=trav_ref(coder,cblock,node,node_arg(node,1),mode)
          call trav_expr(coder,cblock,node,node_arg(node,2))
          call make_basic_sp_call(coder,cblock,node,&
               sym_get_dot_ref,2,1,coder%par_depth)
       case(sym_open_smiley)
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          do i=1,node_numargs(node)
             outmode=trav_ref(coder,cblock,node,node_arg(node,i),mode)
          enddo
          call make_sp_call(coder,cblock,node,sym_open_smiley,node_numargs(node),1)
       case(sym_caret)
          save_run_flags=coder%run_flags
          coder%run_flags=ior(coder%run_flags,call_inline_when_compiling)
          call trav_expr(coder,cblock,pnode,node_arg(node,1))
          coder%run_flags=save_run_flags
          outmode=0
       case(sym_pling)
          if(iand(mode,ref_is_val+ref_ignores_rules)==0.and..not.coder%in_sync) then
             call code_error(coder,node,&
                  'Cannot change value of "!" expression outside of a "sync" statement') 
          endif
          call check_par_context(coder,cblock,node,.false.)
          outmode=ior(trav_ref(coder,cblock,node,node_arg(node,1),mode),&
               ref_has_at)
          call make_comm_sys_call_rtn(coder,cblock,node,sym_make_array,1,1,aflags=proc_run_complete)
          !call var_set_par_depth(coder,top_code(coder),coder%par_depth-1)
          call dup_code(coder)
          call code_num(coder,sym_shared)
          call make_basic_sp_call(coder,cblock,node,sym_set_mode,2,0,coder%par_depth)
          call check_par_context(coder,cblock,node,.true.)
       case(sym_name)
          call trav_ref_to_var(coder,cblock,node,node_arg(node,1),mode)
          outmode=0
       case(sym_dot_call)
          outmode=trav_ref(coder,cblock,node,node_arg(node,1),mode)
          call trav_exprlist(coder,cblock,node,node_arg(node,3))
          call make_comm_sys_call_rtn(coder,cblock,node,node_num_arg(node,2),&
               node_numargs(node_arg(node,3))+1,1,aflags=proccall_is_ref,assign=.true.)
       case default
          if(iand(mode,ref_is_val)==0) then
             call code_error(coder,pnode,&
                  'Cannot indirectly assign to expression - value is updated')
             call dump_parse_tree(coder%context,6,node,2)
             call make_temp_var(coder,cblock,pnode)
          else
             call trav_expr(coder,cblock,pnode,node)
          endif
       end select
    else
       call code_error(coder,pnode,&
            'Cannot make reference')
       call make_temp_var(coder,cblock,pnode)
    endif
  contains
    include 'fvkind.inc'
    include 'fisname.inc'
    include 'fisnull.inc'
    include 'ftiny.inc'
  end function  trav_ref

  !==========================================================
  ! Create alias checks for argument #j in argument list/amp
  ! Will raise an error if alias is detectable at compile time
  ! Will code run-time checks if needed
  ! Will set vstack[argbase+i] to a tiny int value
  ! (or increment existing tiny int value)
  ! if argument #i definitely does not alias argument #j
  !==========================================================

  ! BROKEN _ Does bad things to vstack (probably called with wrong argbase)
  subroutine trav_alias_checks(coder,cblock,list,amp,j,argbase)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,list,amp
    integer,intent(in):: j,argbase
    integer:: i,k,base1,base2
    logical:: finished
    type(pm_ptr):: p,name,name2,var
    return
    p=node_arg(list,j)
    p=node_arg(p,1)
    base1=coder%vtop
    name=pm_null_obj
    finished=get_ref_pattern(coder,p,name)
    if(pm_fast_isnull(name)) return
    k=0
    do i=1,node_numargs(list)
       p=node_arg(list,i)
       if(amp%data%i(amp%offset+k)==i) then
          k=min(pm_fast_esize(amp),k+1)
          if(node_sym(p)==sym_amp) then
             if(i>=j) cycle
             p=node_arg(p,1)
             base2=coder%vtop
             finished=get_ref_pattern(coder,p,name2)
             if(.not.pm_fast_isnull(name2)) then
                call match_ref_pattern(coder,cblock,p,base1,base2,&
                     j,i,list)
             endif
          else
             cycle
          endif
       elseif(node_sym(p)==sym_dot.or.node_sym(p)==sym_sub) then
          base2=coder%vtop
          finished=get_ref_pattern(coder,p,name2)
          if(.not.pm_fast_isnull(name2)) then
             if(match_ref_names(coder,cblock,p,base1,base2)) then
                if(pm_fast_vkind(coder%vstack(argbase+i))==pm_tiny_int) then
                   coder%vstack(argbase+i)%offset=&
                        coder%vstack(argbase+i)%offset+1
                else
                   coder%vstack(argbase+i)=pm_fast_tinyint(coder%context,1)
                endif
             endif
          endif
       endif
    enddo
    var=find_var(coder,name)
    if(pm_fast_isnull(var)) then
       call make_temp_var(coder,cblock,list)
       var=pop_code(coder)
    endif
    if(cnode_flags_clear(var,var_flags,var_is_aliased)) then
       call make_var(coder,cblock,p,name,&
            var_is_var+var_is_aliased+var_is_shadowed,var)
    endif
    coder%vtop=base1
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'ftiny.inc'
    include 'fvkind.inc'
  end subroutine trav_alias_checks

  !============================================================
  ! Get the pattern of .name and [] subscripts in a reference
  ! -- truncated after @
  !============================================================
  recursive function get_ref_pattern(coder,node,name) result(finished)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    type(pm_ptr),intent(inout):: name
    logical:: finished
    integer:: sym
    sym=node_sym(node)
    select case(sym)
    case(sym_sub)
       finished=get_ref_pattern(coder,node_arg(node,1),name)
       if(.not.finished) call code_val(coder,node)
    case(sym_dot)
       finished=get_ref_pattern(coder,node_arg(node,1),name)
       if(.not.finished) call code_val(coder,node_arg(node,2))
    case(sym_at,sym_pling)
       finished=get_ref_pattern(coder,node_arg(node,1),name)
       finished=.true.
    case(sym_name)
       name=node_arg(node,1)
       call code_val(coder,name)
       finished=.false.
    case default
       finished=.true.
    end select
  contains
    include 'fname.inc'
  end function  get_ref_pattern

  !=======================================================================
  ! Match reference patterns in coder%vstack(base1+1..base2) and
  ! coder%vstack(base2+1..coder%vtop) coding runtime checks when needed
  ! Either
  !   Compile alias checking of arguments (provide idx1,idx2,list)
  ! Or
  !   Compile alias check between two references (provide test)
  !=======================================================================
  subroutine match_ref_pattern(coder,cblock,node,base1,base2,idx1,idx2,list,test)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: base1,base2
    integer,intent(in),optional:: idx1,idx2
    type(pm_ptr),intent(in),optional:: list
    logical,intent(in),optional:: test
    type(pm_ptr):: p1,p2
    integer:: i,j,n1,n2,m,vbase
    logical:: has_dollar

    n1=base2-base1
    n2=coder%vtop-base2
    if(n1==0.or.n2==0) return

    if(.not.present(test)) then
       ! Check is cross matching of .name proves no alias is possible
       if(match_ref_names(coder,cblock,node,base1,base2)) return
    endif
    
    ! May alias - code any required run-time subscript checks
    if(present(test)) then
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call dup_expr(coder,coder%vstack(base1))
       m=1
    else
       call make_int_const(coder,cblock,node,idx1)
       call make_int_const(coder,cblock,node,idx2)
       m=2
    endif
    i=1
    j=1
    do while(i<=n1.and.j<=n2)
       p1=coder%vstack(base1+i)
       p2=coder%vstack(base2+j)
       
       ! Check for matching "[]" in both arguments
       if(.not.pm_fast_isname(p1).and..not.pm_fast_isname(p2)) then
 
          ! Code subscript in 1st argument
          has_dollar=trav_index_list(coder,cblock,node_arg(p1,2),.true.)
          
          ! Check for and consolidate subsequent subscripts
          i=i+1
          if(i<=n1) then
             p1=coder%vstack(base1+i)
             if(.not.pm_fast_isname(p1)) then
                vbase=coder%vtop-1
                do while(.not.pm_fast_isname(p1))
                   has_dollar=trav_index_list(coder,cblock,node_arg(p1,2),.true.)
                   i=i+1
                   if(i>n1) exit
                   p1=coder%vstack(base1+i)
                enddo
                call make_sys_call_rtn(coder,cblock,node,sym_combine_indices,&
                     coder%vtop-vbase,1)
             endif
          endif
          
          ! Code subscript in 2nd argument
          has_dollar=trav_index_list(coder,cblock,node_arg(p2,2),.true.)
          
          ! Check for and consolidate subsequent subscripts
          j=j+1
          if(j<=n2) then
             p2=coder%vstack(base2+j)
             if(.not.pm_fast_isname(p2)) then
                vbase=coder%vtop-1
                do while(.not.pm_fast_isname(p2))
                   has_dollar=trav_index_list(coder,cblock,node_arg(p2,2),.true.)
                   j=j+1
                   if(j>n2) exit
                   p2=coder%vstack(base2+j)
                enddo
                call make_sys_call_rtn(coder,cblock,node,sym_combine_indices,&
                     coder%vtop-vbase,1)
             endif
          endif
          
          ! Now have 2 more arguments for alias checker
          m=m+2
       elseif(pm_fast_isname(p1).and.pm_fast_isname(p2)) then
          ! Matching .name in both arguments - just skip
          i=i+1
          j=j+1
       else
          ! Matching .name with []
          ! This situation cannot occur with type-correct code
          ! So quit here - a type error will be raised later,
          ! while an alias error would be confusing
          coder%vtop=coder%vtop-m
          return
       endif
    enddo
    if(m<=2) then
       if(.not.present(test)) then
          ! There were no subscripts, so clash can be confirmed at compile time
          call code_error(coder,node_arg(list,idx2),&
               'Argument aliased by "&" argument')
          call code_error(coder,node_arg(list,idx1),&
               'Argument causing the alias')
       endif
       coder%vtop=coder%vtop-2
    else
       ! Code call to check for subscript aliasing
       call make_sys_call(coder,cblock,node,sym_check_alias,m,&
            merge(1,0,present(test)))
    endif
  contains
    include 'fisname.inc'
    include 'fvkind.inc'
  end subroutine match_ref_pattern


  !=======================================================================
  ! Match reference patterns in coder%vstack(base1+1..base2) and
  ! coder%vstack(base2+1..coder%vtop) to compare presence of .name
  ! qualifiers to see if  mismatch proves no aliasing is possible
  !=======================================================================
  function match_ref_names(coder,cblock,node,base1,base2) &
       result(differ)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: base1,base2
    logical:: differ
    type(pm_ptr):: p1,p2
    integer:: i,j,n1,n2,m,vbase
    logical:: has_dollar
    differ=.false.
    n1=base2-base1
    n2=coder%vtop-base2
    if(n1==0.or.n2==0) return
    i=1
    j=1
    do while(i<=n1.and.j<=n2)
       p1=coder%vstack(base1+i)
       p2=coder%vstack(base2+j)
       if(pm_fast_isname(p1).and.pm_fast_isname(p2)) then
          ! Compare "." names
          if(p1%offset/=p2%offset) then
             differ=.true.
             return
          endif
          i=i+1
          j=j+1
       else
          ! Skip past "[]" subscripts
          if(.not.pm_fast_isname(p1)) then
             i=i+1
          endif
          if(.not.pm_fast_isname(p2)) then
             j=j+1
          endif
       endif
    enddo
  contains
    include 'fisname.inc'
  end function match_ref_names
  
  !========================================================
  ! Subscripts (including dollar subscripts)
  !========================================================
  recursive function trav_index_list(coder,cblock,node,is_val) result(has_dollar)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    logical,optional,intent(in):: is_val
    logical:: has_dollar
    integer:: n,flags
    integer:: i,j,save_subs_index,max_idx
    logical:: replicated
    integer:: which(0:8)
    type(pm_ptr):: p
    logical:: has_underscore_error
    has_underscore_error=.false.
    flags=0
    n=node_numargs(node)
    save_subs_index=coder%subs_index
    which=0
    replicated=.false.
    max_idx=0
    do i=1,n
       coder%subs_index=0
       p=node_arg(node,i)
       if(node_sym(p)==sym_underscore) then
          p=find_param(coder,cblock,node,&
               pm_fast_name(coder%context,sym_stretch_dim))
          if(pm_fast_isnull(p)) call pm_panic('Cant find stretch dim')
          call code_val(coder,p)
          if(.not.present(is_val).and..not.has_underscore_error) then
             call code_error(coder,node_arg(node,i),&
                  'Cannot have "_" in a left-hand-side subscript')
             has_underscore_error=.true.
          endif
       else
          call trav_expr(coder,cblock,&
               node,node_arg(node,i))
       endif
       j=coder%subs_index
       replicated=replicated.or.(which(j)>0.and.j>0)
       which(j)=i
       max_idx=max(max_idx,j)
    enddo
    if(node_sym(node)==sym_dotdotdot) then
       flags=call_is_vararg
    endif
    if(which(1)>0.and.n/=1) then
       call code_error(coder,node,&
            'Cannot use "$" without ".dimension" in subscript with more than one argument')
    endif
    if(n>1.or.node_sym(node)==sym_dotdotdot) then
       call make_sys_call_rtn(coder,cblock,node,sym_tuple,n,1,&
            aflags=flags)
    endif
    if(max_idx>0) then
       if(coder%par_state==par_state_outer) then
          call code_error(coder,node,'Cannot have "$" index outside of any parallel context')
       endif
       coder%temp2=pop_code(coder)
       call code_val(coder,coder%temp2)
       coder%temp2=pm_null_obj
       if(max_idx>1.and.all(which(2:max_idx)>0)) then
          do i=2,max_idx
             if(which(i)>0) then
                call make_static_long_const(coder,cblock,node,int(which(i),pm_ln))
             else
                call make_const(coder,cblock,node,pm_null_obj,int(pm_null))
             endif
          end do
          call make_sys_call_rtn(coder,cblock,node,sym_tuple,max_idx-1,1)
          call make_sys_call_rtn(coder,cblock,node,sym_make_dtuple,2,1)
       else
          call make_sys_call_rtn(coder,cblock,node,sym_make_dtuple,1,1)
       endif
    endif
    coder%subs_index=save_subs_index
    has_dollar=max_idx>0
  contains
    include 'fname.inc'
    include 'fisnull.inc'
  end function trav_index_list

  !========================================================
  ! Create a new system variable from expr on top of stack
  !========================================================
  subroutine define_sys_var(coder,cblock,node,name,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: name
    integer,intent(in):: flags
    integer:: junk
    type(pm_ptr):: var
    write(*,*) '>>>',pm_name_as_string(coder%context,name)
    call make_sys_var(coder,cblock,node,name,flags)
    var=top_code(coder)
    call swap_code(coder)
    call make_sys_call(coder,cblock,node,&
         merge(sym_dup,sym_clone,iand(flags,var_is_var)/=0),1,1,aflags=call_ignore_rules)
    call make_var_mode(coder,cblock,node,var)
  end subroutine define_sys_var

  !========================================================
  ! Initialise system variable from expr on top of stack
  !========================================================
  subroutine init_var(coder,cblock,node,var)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,var
    call code_val(coder,var)
    call swap_code(coder)
    call make_sys_call(coder,cblock,node,&
         merge(sym_dup,sym_clone,cnode_flags_set(var,var_flags,var_is_var)),&
         1,1,aflags=call_ignore_rules)
    call make_var_mode(coder,cblock,node,var)
  end subroutine init_var

  !========================================================
  !  Set the mode (and maybe depth) of a variable
  !========================================================
  subroutine make_var_mode(coder,cblock,node,var)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node
    type(pm_ptr),intent(in)::var
    if(coder%run_flags==0) then
       if(iand(cnode_get_num(var,var_flags),var_is_var)/=0) then
          call code_val(coder,var)
          call make_basic_sp_call(coder,cblock,node,sym_private,&
               1,0,coder%par_depth)
       endif
    else
       call code_val(coder,var)
       call code_num(coder,coder%run_mode)
       if(iand(coder%run_flags,proc_run_shared+proc_run_local)/=0) then
          call var_set_par_depth(coder,var,coder%par_depth-1)
          call make_basic_sp_call(coder,cblock,node,&
               sym_var_set_mode,2,0,coder%par_depth-1)
       else
          call make_basic_sp_call(coder,cblock,node,&
               sym_var_set_mode,2,0,coder%par_depth)
       endif
    endif
  end subroutine make_var_mode

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
       call make_assign_call(coder,cblock,node,sym_assignment,2,0,aflags=flags)
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
    integer:: save_par_state
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
    integer:: sym,i,n,m,nsym,base,flags,outmode
    logical:: outer,shared,isproc,ischan,save_fixed
    type(pm_ptr):: list,name,p,q,save_sub_array
    integer:: save_par_state,loop_flags
      
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
       call trav_name(coder,cblock,node,sym,node_arg(node,1))
    case(sym_proc)
       call proc_const(coder,cblock,pnode,node)
    case(sym_param)
       if(node_numargs(node)==2) then
          p=find_param(coder,cblock,node,node_arg(node,1),&
               node_arg(node,2))
          if(pm_fast_isnull(p)) then
             call code_error(coder,node,'Cannot find parameter: ',&
                  node_arg(node,1))
             call make_temp_var(coder,cblock,node)
          else
             call code_val(coder,p)
          endif
       else
          p=find_param(coder,cblock,node,node_arg(node,1))
          if(pm_fast_isnull(p)) then
             call code_error(coder,node,&
                  'Cannot find parameter: ',node_arg(node,1))
             call make_temp_var(coder,cblock,node)
          else
             call code_val(coder,p)
          endif
       endif
    case(sym_underscore)
       call name_const(node,sym_stretch_dim)
       return
    case(sym_unique)
       p=node_arg(node,1)
       call name_const(node,int(p%offset))
       return
    case(sym_fix)
       save_fixed=coder%fixed
       coder%fixed=.true.
       call trav_expr(coder,cblock,node,node_arg(node,1))
       coder%fixed=save_fixed
       call make_sp_call_rtn(coder,cblock,node,sym_dash,1,1)
    case(sym_present)
       p=node_arg(node,1)
       i=p%offset
       i=find_var_entry(coder,i,coder%proc_base)
       if(i==0) then
          call code_error(coder,node,'Variable undefined in "present": ',p)
          call make_temp_var(coder,cblock,node)
       else
          q=coder%var(i)
          if(cnode_flags_set(q,var_flags,var_is_key)) then
             call code_val(coder,cnode_get(q,var_extra_info))
             call make_sp_call_rtn(coder,cblock,node,sym_present,1,1)
          else
             call code_error(coder,node,&
                     'Not a keyword argument in "present": ',p)
             call make_temp_var(coder,cblock,node)
          endif
       endif
    case(first_operator:last_operator)
       do i=1,node_numargs(node)
          call trav_expr(coder,cblock,&
               node,node_arg(node,i))
       enddo
       call make_sys_call_rtn(coder,cblock,node,&
            sym,node_numargs(node),1)
    case(sym_open_smiley)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       do i=1,node_numargs(node)
          call trav_expr(coder,cblock,&
               node,node_arg(node,i))
       enddo
       call make_sp_call(coder,cblock,node,&
            sym_open_smiley,node_numargs(node),1)
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
       if(coder%par_state>par_state_outer) then
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call make_comm_sys_call_rtn(coder,cblock,node,&
               merge(sym_hash,sym_mult,sym==sym_uhash),1,1)
       else
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call make_sys_call_rtn(coder,cblock,node,&
               merge(sym_hash,sym_mult,sym==sym_uhash),1,1)
       endif
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
    case(sym_caret)
       if(node_numargs(node)==1) then
          outmode=trav_ref(coder,cblock,pnode,node,0)
       elseif(.not.pm_fast_isnull(node_arg(node,2))) then
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call code_val(coder,node_arg(node,2))
          call make_basic_sp_call(coder,cblock,node,sym_change_mode,2,1,coder%par_depth)
       else
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call make_var(coder,cblock,node,pm_fast_name(coder%context,sym_caret),&
               var_is_shadowed+var_is_no_import_export,pop_code(coder))
       endif
    case(sym_dcaret)
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call make_sp_call_rtn(coder,cblock,node,sym_dcaret,1,1)
    case(sym_dot)
       if(node_sym(node_arg(node,1))==sym_name) then
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call make_const(coder,cblock,node,node_arg(node,2))
          call make_sp_call_rtn(coder,cblock,node,sym_dot,2,1)
       else
          outmode=trav_ref(coder,cblock,pnode,node,ref_is_val)
          if(coder%par_state>par_state_outer) then
             p=pop_code(coder)
             call code_val(coder,p)
             call make_static_bool_const(coder,cblock,node,iand(outmode,ref_has_at)/=0)
             call make_comm_sys_call_rtn(coder,cblock,node,sym_get_ref,2,1)
             call check_par_context(coder,cblock,node,.true.)
          else
             call make_sys_call_rtn(coder,cblock,node,sym_get_val_ref,1,1)
          endif
       endif
    case(sym_get_dot_ref)
       outmode=trav_ref(coder,cblock,pnode,node,ref_is_val)
    case(sym_get_dot,sym_sub,sym_dot_sub,sym_dot_call)
       outmode=trav_ref(coder,cblock,pnode,node,ref_is_val)
       if(coder%par_state>par_state_outer) then
          p=pop_code(coder)
          call code_val(coder,p)
          call make_static_bool_const(coder,cblock,node,iand(outmode,ref_has_at)/=0)
          call make_comm_sys_call_rtn(coder,cblock,node,sym_get_ref,2,1)
          call check_par_context(coder,cblock,node,.true.)
       else
          call make_sys_call_rtn(coder,cblock,node,sym_get_val_ref,1,1)
       endif
    case(sym_pling)
       outmode=trav_ref(coder,cblock,pnode,node,ref_is_val)
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
    case(sym_struct,sym_rec)
       call trav_structrec(coder,cblock,node)
    case(sym_query)
       if(coder%par_state==par_state_outer) then
          call code_error(coder,node,&
               'Cannot use "?" or "??" outside of any parallel statement')
       elseif(coder%par_state<par_state_cond) then
          if(pm_fast_isnull(node_arg(node,1))) then
             call make_const(coder,cblock,node,coder%true,&
                  coder%true_fix)
             return
          endif
       endif
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       if(pm_fast_isnull(node_arg(node,1))) then
          call make_sys_call(coder,cblock,node,sym_active,0,1)
          call dup_code(coder)
          call make_basic_sp_call(coder,cblock,node,sym_private,1,0,coder%par_depth)
       else
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call make_sys_call(coder,cblock,node,sym_active,1,1)
       endif
    case(sym_dollar)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call code_val(coder,coder%var(coder%par_base+lv_here))
       call make_sys_call(coder,cblock,node,&
            sym_make_dollar,2,1)
       
    case(sym_for)
!!$       call trav_par_expr(coder,cblock,node)
    case(sym_mode,sym_always)
       call trav_mode_stmt(coder,cblock,node,sym,.true.)
    case(sym_cast)
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call trav_expr(coder,cblock,node,node_arg(node,2))
       call make_sp_call_rtn(coder,cblock,node,sym_cast,2,1)
    case(sym_number,sym_string)
       if(coder%fixed) then
          p=node_arg(node,1)
          call make_const(coder,cblock,node,p,&
               pm_new_fix_type(coder%context,p))
       else
          call make_const(coder,cblock,node,node_arg(node,1))
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
      integer:: junk
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
    
  end subroutine trav_expr

  !==================================================================
  ! Name in usual expression context (may be variable or parameter)
  !==================================================================
  subroutine trav_name(coder,cblock,node,sym,name)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,name
    integer:: sym
    type(pm_ptr):: p

    if(sym==sym_use) then
       p=find_param(coder,cblock,node,name,node_arg(node,2))
       if(pm_fast_isnull(p)) then
          p=find_imported_decl(coder,node,&
               name,node_arg(node,2),&
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
       p=find_var(coder,name)
       if(pm_fast_isnull(p)) then
          p=find_param(coder,cblock,node,name)
          if(pm_fast_isnull(p)) then
             call code_error(coder,node,&
                  'Name not defined:',name)
             call make_var(coder,cblock,node,name,0)
          else
             call code_val(coder,p)
          endif
       else
          if(cnode_flags_set(p,var_flags,&
               var_is_aliased)) then
             p=cnode_get(p,var_extra_info)
             coder%aliased=.true.
          endif
          if(cnode_flags_set(p,var_flags,var_is_sync)) then
             call code_error(coder,node,&
                  'Cannot access "sync" left-hand-side variable in right-hand-side expression')
          endif
          if(cnode_flags_set(p,var_flags,var_is_ref)) then
             call code_val(coder,p)
             call make_sys_call_rtn(coder,cblock,node,sym_get_ref,1,1,&
                  aflags=merge(proc_run_complete+proc_run_always,0,&
                  coder%par_state>par_state_outer))
          else
             call code_val(coder,p)
          endif
       endif
    endif

  contains

    include 'fisnull.inc'
    
  end subroutine trav_name

  !========================================================
  ! Traverse struct or rec creation "new" expression
  ! Parse node contains full_type/ list_of_expr / name / tag
  !========================================================
  recursive subroutine trav_structrec(coder,cblock,node)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node
    type(pm_ptr):: exprs,p,name,decl,tag,name1,name2,elems,info
    integer:: i,j,k,vbase,n,m,count,nam1,nam2,sym,basex,tno

    ! Find  associated type declaraton (decl)
    name=node_arg(node,4)
    tag=node_arg(node,3)
    decl=find_decl(coder,node,name,modl_type)
    if(pm_fast_isnull(decl)) then
       call code_error(coder,node,'No such type: ',name)
       call make_temp_var(coder,cblock,node)
       return
    else
       decl=node_arg(decl,2)
       if(node_sym(decl)/=sym_is) then
          call code_error(coder,node,'Not a "struct" or "rec" type name:',name)
          call make_temp_var(coder,cblock,node)
          return
       else
          decl=node_arg(node_get(decl,type_includes),1)
          sym=node_sym(decl)
          if(sym/=sym_struct.and.sym/=sym_rec) then
             call code_error(coder,node,'Does not reference "struct" or "rec" type')
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
    
    ! Set up struct/rec creation call
    basex=coder%vtop
    info=trav_structrec_decl(coder,decl,decl)
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
    ! At this point tno contains the body of the struct/rec type
    
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
             nam2=name2%data%i(name2%offset+j)
             if(nam1==nam2) cycle outer2
          enddo
          call element_error(exprs,sym,name,name1,i)
       enddo outer2
    endif

    ! Tidy up and create call
    if(pm_debug_checks) then
       if(coder%vtop/=basex+n+2) then
          write(*,*) '>>',coder%vtop,n,coder%vtop-n-2
          call pm_panic('trav_structrec')
       endif
    endif
    call make_sp_call_rtn(coder,cblock,node,sym,n+2,1,flags=coder%run_flags)
    coder%vstack(vbase+1)=coder%vstack(coder%vtop)
    coder%vtop=vbase+1
    
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
    include 'ftiny.inc'
    
    subroutine element_error(node,sym,name,name1,i)
      type(pm_ptr),intent(in):: node,name,name1
      integer,intent(in):: sym,i
      call code_error(coder,node_arg(node,i),'"'//trim(sym_names(sym))//' '//&
           trim(pm_name_as_string(coder%context,int(name%offset)))//&
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
    
  end subroutine trav_structrec

  !========================================================
  ! Traverse a cast to a type defined by node
  ! - sym gives some context, now ignored
  !========================================================
  recursive subroutine trav_cast(coder,cblock,pnode,node,sym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer,intent(in):: sym
    integer:: mode,depth,tno
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

  !========================================================
  ! Parallel scope depth of a value
  !========================================================
  function par_depth(coder,val) result(depth)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: val
    integer:: depth
    depth=max(1,coder%par_depth-1)
    if(pm_fast_vkind(val)==pm_pointer) then
       if(cnode_get_kind(val)==cnode_is_var) then
          depth=cnode_get_num(val,var_par_depth)+coder%proc_par_depth
       endif
    endif
  contains
    include 'fvkind.inc'
  end function par_depth

  !========================================================
  ! Set a variable to be shared
  !========================================================
  subroutine set_var_as_shared(coder,var)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: var
    call cnode_set_num(var,var_par_depth,&
         coder%par_depth-1-coder%proc_par_depth)
  end subroutine set_var_as_shared


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
    integer:: sym,i,n,m,nshared,base,flags
    integer::typno
    type(pm_ptr):: name,val,p
    character(len=100):: str
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
          p=find_decl(coder,node,node_arg(node,1),modl_proc)
          if(pm_fast_isnull(p)) then
             call code_error(coder,node,&
                  'proc value not associated with any defined procedure: ',&
                  node_arg(node,1))
             call push_word(coder,0)
             return
          endif
       elseif(node_numargs(node)==2) then
          p=find_imported_decl(coder,node,node_arg(node,1),&
               node_arg(node,2),modl_proc)
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
       name=node_arg(node,1)
       call push_word(coder,pm_new_name_type(coder%context,int(name%offset)))
    case(sym_fix)
       name=node_arg(node,1)
       select case(node_sym(name))
       case(sym_true)
          call push_word(coder,coder%true_fix)
       case(sym_false)
          call push_word(coder,coder%false_fix)
       case(sym_number,sym_string)
          call push_word(coder,pm_new_fix_type(coder%context,node_arg(name,1)))
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
          call push_word(coder,pm_new_literal_type(coder%context,name))
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
       name=node_arg(node,1)
       call push_word(coder,pm_type_new_user)
       call push_word(coder,int(name%offset))
       typno=get_typeno(2)
       if(typno==0) call pm_panic('Intrinsic type not found')
       call push_word(coder,typno)
    case(sym_struct,sym_rec)
       flags=node_num_arg(node,7)
       name=node_arg(node,2)
       if(sym==sym_struct) then
          call push_word(coder,pm_type_new_struct+flags)
       else
          call push_word(coder,pm_type_new_rec+flags)
       endif
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
    case(sym_list,sym_dotdotdot,sym_open_smiley)
       if(sym==sym_open_smiley) then
          p=node_arg(node,1)
          sym=node_sym(p)
          i=pm_type_is_list
          m=1
       else
          p=node
          i=0
          m=2
       endif
       if(sym==sym_dotdotdot) then
          call push_word(coder,pm_type_new_vtuple+i)
       else
          call push_word(coder,pm_type_new_tuple+i)
       endif
       call push_word(coder,0)
       nshared=0
       n=node_numargs(p)
       do i=m,n,m
          val=node_arg(p,i)
          call trav_type(coder,val,val)
       enddo
       call make_type(coder,n/m+2)
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
         if(.not.pm_fast_isnull(find_type_var(coder,node_arg(dp,i)))) then
            call code_error(coder,node,&
                 'Cannot shadow type-match parameter:',node_arg(dp,i))
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
    type(pm_ptr):: namenode,name,decl,dec,inc,pargs,also_dec
    type(pm_ptr):: twice_dec,main_dec,tname,tval,pars,newdec
    type(pm_ptr),target:: incset
    logical:: is_present,also_present,type_present
    logical:: dotdotdot_present,multiple_modules,twice,has_constraints
    integer:: nargs,sym,i,j,n,base,parbase,ibase,npars,idepth
    integer:: new_type,tno
    type(pm_reg),pointer:: reg
    logical:: ok

    ! Type name and arguments
    nargs=node_numargs(node)-1
    namenode=node_arg(node,nargs+1)
    if(pm_fast_isname(namenode)) then
       name=namenode
    else
       name=node_arg(namenode,2)
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
            trim(pm_name_as_string(coder%context,int(name%offset))),' nargs=',nargs,'{'
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
            node_arg(namenode,1),node_arg(namenode,2),modl_type)
       if(pm_fast_isnull(decl)) then
          call code_error(coder,node,&
               'Cannot find type: '//&
               trim(pm_name_as_string(coder%context,node_num_arg(namenode,1)))//'::'//&
               trim(pm_name_as_string(coder%context,int(name%offset))))
          goto 888
       endif
    else 
       decl=find_decl(coder,node,name,modl_type)
       if(pm_fast_isnull(decl)) then
          ! Not found but may be intrinsic declaration
          coder%wstack(coder%wtop-nargs)=name%offset
          new_type=get_typeno(nargs+2)
          if(new_type>0) then
             ! .. is intrinsic declaraton, return it
             coder%wtop=base-nargs-1
             coder%wstack(coder%wtop)=new_type
             return
          endif
          call code_error(coder,node,&
               'Cannot find type: '//&
               trim(pm_name_as_string(coder%context,int(name%offset))))
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
               int(name%offset))))
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
          call make_type_vars(coder,int(name%offset),dec,node,pars,&
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
               trim(pm_name_as_string(coder%context,int(name%offset))))
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
          call make_type_vars(coder,int(name%offset),pnode,node,&
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
             call make_type_vars(coder,int(name%offset),&
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
          
 20       continue
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
            trim(pm_name_as_string(coder%context,int(name%offset))))
       call code_error(coder,main_dec,&
            'Type declaration being extended in the above error')
    endif
    if(also_present.and..not.type_present) then
       call code_error(coder,also_dec,&
            '"Type extended using "..." or ":" without original "type is " definition present: '//&
            trim(pm_name_as_string(coder%context,int(name%offset))))
    endif
    if(multiple_modules.and.also_present.and..not.dotdotdot_present) then
       call code_error(coder,also_dec,&
            'Type is extended using "..." or ":" across multiple modules"//&
            " without "..." present in original "type is": '//&
            trim(pm_name_as_string(coder%context,int(name%offset))))
    endif
    if(twice) then
       call code_error(coder,main_dec,&
            'Type is defined twice: ',name)
       call code_error(coder,twice_dec,'... second definition')
       call node_set_num(decl,node_args+4,max_type_nesting+2)
    endif
    
    if(debug_codegen) then
       write(*,*) 'definition traversed for ',&
            trim(pm_name_as_string(coder%context,int(name%offset))),'#',top_word(coder)
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
  ! Create a template type from struct/rec declaration
  !===============================================================
  recursive function trav_structrec_decl(coder,pnode,decl) result(vect)
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
  end function trav_structrec_decl

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
    logical:: ok
    integer:: k,base,wbase,npars
    integer:: vtyp,partyp
    type(pm_ptr):: pname,tv,name
    logical:: local,check_against_base
    check_against_base=.false.
    name=node_arg(callnode,node_numargs(callnode))
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
    coder%imps(coder%top)=0
 
    base=coder%top
    wbase=coder%wtop
    if(.not.present(parbase)) then
       do k=1,npars
          call trav_type(coder,pnode,node_arg(pnames,k*2))
       enddo
    endif
    do k=npars,1,-1
       pname=node_arg(pnames,k*2-1)
       coder%stack(k+coder%top+1)=pname%offset
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
                  partyp,int(pname%offset),cnode_is_par_constraint)
             
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
                  partyp,vtyp,int(pname%offset),cnode_is_arg_constraint)
          endif
       endif

       if(find_var_entry(coder,&
            int(pname%offset),base)>0) then
          call code_error(coder,pnames,&
               'Repitition of type parameter name:',&
               pname)
       else
          call push_var(coder,int(pname%offset),&
               pm_fast_tinyint(coder%context,vtyp))
       endif

    enddo
    coder%top=coder%top+1
    coder%stack(coder%top)=typevar_end
    coder%var(coder%top)%offset=base
    coder%imps(coder%top)=0
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
    coder%imps(coder%top)=0
    do i=base+1,top-1
       if(coder%stack(i)/=0) then
          call push_var(coder,coder%stack(i),coder%var(i))
       endif
    enddo
    coder%top=coder%top+1
    coder%stack(coder%top)=typevar_end
    coder%var(coder%top)%offset=nbase
    coder%imps(coder%top)=0
  end subroutine copy_type_vars

  !========================================
  ! Find type variable (parameter)
  !========================================
  function find_type_var(coder,vname) result(vr)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: vname
    type(pm_ptr):: vr
    integer:: k
    integer:: n
    if(coder%top==0) then
       vr=pm_null_obj
    elseif(coder%stack(coder%top)/=typevar_end) then
       vr=pm_null_obj
    else
       n=vname%offset
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
    integer(pm_ln):: i,j
    integer:: k
    integer:: tno,tno1,tno2
    type(pm_ptr):: tset,name
    type(pm_type_einfo):: einfo
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
               pm_user_type_body(coder%context,tno),pm_type_incl_type,&
               einfo)) then
             call code_error(coder,pm_null_obj,&
                  'Type is incorrectly defined: '//&
                  trim(pm_type_as_string(coder%context,tno)))
             call pm_type_error(coder%context,einfo)
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
          if(.not.pm_type_includes(coder%context,tno1,tno2,pm_type_incl_type,&
               einfo)) then
             call cnode_error(coder,p,&
                  'Type argument "'//&
                  trim(pm_name_as_string(coder%context,&
                  int(name%offset)))//&
                  '" does not meet constraint: '//&
                  trim(pm_type_as_string(coder%context,tno1))//&
                  ' inc '//&
                  trim(pm_type_as_string(coder%context,tno2)))
             call pm_type_error(coder%context,einfo)
             call code_error(coder,cnode_arg(p,5),&
                  'Constraint that gave rise to above error')
          endif
       case(cnode_is_par_constraint)
          tno1=cnode_get_num(p,cnode_args+1)
          tno2=cnode_get_num(p,cnode_args+2)
          if(.not.pm_type_includes(coder%context,tno1,tno2,pm_type_incl_type,&
               einfo)) then
             call cnode_error(coder,p,&
                  'Parameter "'//&
                  trim(pm_name_as_string(coder%context,&
                  int(name%offset)))//&
                  '" does not match base type; parameter contraint: '//&
                  trim(pm_type_as_string(coder%context,tno1))//&
                  ' ,argument: '//&
                  trim(pm_type_as_string(coder%context,tno2)))
             call pm_type_error(coder%context,einfo)
             call code_error(coder,cnode_arg(p,5),&
                  'Constraint that gave rise to the above error')
          endif
       case(cnode_is_type_constraint)
          tno1=cnode_get_num(p,cnode_args+1)
          tno2=cnode_get_num(p,cnode_args+2)
          if(.not.pm_type_includes(coder%context,tno1,tno2,pm_type_incl_equiv,&
               einfo)) then
             call cnode_error(coder,p,'Type does not meet constraint:')
             call pm_type_error(coder%context,einfo)
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
    type(pm_ptr):: list,procs,keys,keynames,sig,name,amp,prvar,proc,p,arg
    integer:: flags,i,j,nargs,nkeys,ampbase,vsym,outmode,nref
    integer:: depth,otop,obase,owbase,base,abase,atop,babase,astart
    logical:: iscomm,outer,has_shared_amp_arg,need_alias_checks,shared_ref_ok
    integer:: save_run_mode,save_run_flags
    
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
    
    if(debug_codegen) then
       write(*,*) 'TRAV CALL>',&
            trim(pm_name_as_string(coder%context,int(name%offset))),&
            nargs,nret,coder%vtop,flags
    endif

    base=coder%vtop
    has_shared_amp_arg=.false.

    ! write(*,*) 'AMP',pm_fast_isnull(amp),trim(pm_name_as_string(coder%context,int(name%offset)))
    
    ! Standard arguments
    if(pm_fast_isnull(amp)) then
       do i=1,nargs
          call trav_expr(coder,cblock,list,&
               node_arg(list,i))
       enddo
    else
       if(.not.amps_ok) then
          call code_error(coder,list,&
               'Call using "&" arguments cannot be a component of an expression')
       endif

       amp=pm_name_val(coder%context,int(amp%offset))
       flags=ior(flags,call_is_assign_call)

      ! Alias checks if needed
       nref=0
       abase=coder%top
       if(pm_opts%check_alias) then
          do j=0,pm_fast_esize(amp)
             i=amp%data%i(amp%offset+j)
             arg=node_arg(list,i)

             !!! ampbase not set here -- and should be
             call trav_alias_checks(coder,cblock,list,amp,i,ampbase)
             nref=nref+1
          enddo
       endif
       atop=coder%top

       do i=1,nret
          call code_val(coder,coder%vstack(base-nret+i))
       enddo
       base=coder%vtop
       j=0
       do i=1,nargs
          if(amp%data%i(amp%offset+j)==i) then
             arg=node_arg(list,i)
             outmode=trav_ref(coder,cblock,list,&
                  arg,merge(ref_ignores_rules+ref_is_amp,ref_is_amp,iscomm))
             j=min(pm_fast_esize(amp),j+1)
          else
             arg=node_arg(list,i)
             call trav_expr(coder,cblock,list,arg)
          endif
       enddo
       call hide_vars(coder,abase+1,atop)
    endif
    babase=merge(base+3,base+1,iscomm)

    call make_arglist(coder,cblock,node,nargs,nret,.false.,.false.)

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

    ! Find procs with this signature
    amp=node_arg(node,3)
    proc=pm_null_obj
    if(pm_fast_isname(name)) then
       proc=find_decl(coder,node,name,modl_proc)
       if(pm_fast_isnull(proc)) then
          call code_error(coder,node,'Cannot find proc: ',name)
          call make_temp_var(coder,cblock,node)
       endif
    else
       vsym=node_sym(name)
       select case(vsym)
       case(sym_name)
          proc=find_decl(coder,name,name,modl_proc)
          if(pm_fast_isnull(proc)) then
             call code_error(coder,name,'Cannot find proc: ',node_arg(name,1))
          endif
       case(sym_use)
          proc=find_imported_decl(coder,name,node_arg(name,1),&
               node_arg(name,2),modl_proc)
       case(sym_dot)
          call trav_expr(coder,cblock,node,node_arg(name,1))
       case(sym_proc)
          if(node_numargs(name)==1) then
             proc=find_decl(coder,name,node_arg(name,1),modl_proc)
             if(pm_fast_isnull(proc)) then
                call code_error(coder,name,'Cannot find proc: ',node_arg(name,1))
             endif
          else
             proc=find_imported_decl(coder,name,node_arg(name,1),&
                  node_arg(name,2),modl_proc)
          endif
       case(sym_method_call) 
          call code_val(coder,coder%vstack(babase))
          call make_const(coder,cblock,node,node_arg(name,1))
          call make_sp_call_rtn(coder,cblock,node,sym_method_call,2,1)
       case default
          write(*,*) sym_names(vsym)
          call pm_panic('Bad VSYM in trav_call')
       end select
       if(vsym/=sym_dot.and.vsym/=sym_method_call.and.pm_fast_isnull(proc)) then
          call make_temp_var(coder,cblock,name)
       endif
    endif

    !write(*,*) '++++>',coder%vtop,trim(pm_name_as_string(coder%context,int(name%offset)))
    
    if(.not.pm_fast_isnull(proc)) then
       prvar=pm_null_obj
       procs=find_sig(coder,node,name)
    else
       prvar=pop_code(coder)
       procs=pm_fast_tinyint(coder%context,0)
    endif

    !write(*,*) '++++=>',coder%vtop,trim(pm_name_as_string(coder%context,int(name%offset)))
    
    ! Error return if no such proc
    if(pm_fast_isnull(procs)) then
       coder%vtop=obase-nret
       return
    endif
   
!!$    ! Keyword arguments (need sig first)
!!$    if(.not.pm_fast_isnull(prvar).and..not.pm_fast_isnull(keys)) then
!!$       call code_error(coder,keys,'Cannot have keyword arguments in ".()" call')
!!$       nkeys=0
!!$    else
!!$       nkeys=trav_keys(coder,cblock,keys,sig,iscomm)
!!$    endif

    if(coder%par_state>=par_state_cond) then
       flags=ior(flags,call_is_cond)
    endif
    
    if(coder%par_state==par_state_cond.or.&
         coder%par_state==par_state_par) then
       flags=ior(flags,call_is_unlabelled)
    endif
    
    ! Make the call
    !call import_args(coder,cblock,node,nargs,nret,nkeys,amp,flags,abase)

    !write(*,*) '==>',obase,coder%vtop,nargs,nret
    
    call make_full_call(coder,cblock,node,procs,amp,&
         nargs,nret,nkeys,keynames,flags,prvar,coder%par_depth)

    !write(*,*) '===>',obase,coder%vtop,nargs,nret
    
    
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

    if(.not.pm_fast_isnull(amp)) coder%vtop=obase-nret
    
    ! If debugging, check tidy up
    if(pm_debug_checks) then
       if(coder%vtop/=obase-nret.or.coder%wtop/=owbase) then
          write(*,*) obase,nret,obase-nret,coder%vtop,owbase,coder%wtop,nargs,otop,&
               coder%top,pm_fast_isnull(amp),&
               trim(pm_name_as_string(coder%context,int(name%offset)))
          call pm_panic('trav call')
       endif
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
  end subroutine trav_call

  !===============================================================
  ! Traverse procedure definition
  !===============================================================
  recursive subroutine trav_proc(coder,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node

    integer:: nargs,nret
    type(pm_ptr):: cblock,cblock2,cblock3,cblock4
    type(pm_ptr):: p,par,amp,rtypes,arg,rv,keycall,argcall
    type(pm_ptr),target:: tkeys
    integer:: i,j,n,base,obase,wbase,npars,cbase
    integer:: flags,sym,loop_pars,reduce_base,reduce_start,rsig
    integer:: partyp
    integer:: save_index,t
    integer:: save_proc_base,&
         save_par_base, save_over_base,save_proc_par_depth,&
         save_proc_nret,save_par_state,save_proc_ncalls,&
         save_subs_index,save_lex_scope,save_run_mode,save_run_flags,&
         save_state_base,save_mask
    type(pm_ptr):: save_sub_array,save_loop_cblock, &
         save_proc_keys,save_label
    logical:: save_aliased,save_in_sync

    integer:: pr_flags
    type(pm_reg),pointer:: reg
    logical:: complete,old_complete
    integer,save:: pdepth=0

    nargs=node_numargs(node_get(node,proc_params))/2
    nret=node_get_num(node,proc_numret)
    flags=node_get_num(node,proc_flags)
    !amps=node_get(node,proc_amplocs)
    !keyargs=pm_null_obj

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

       old_complete=coder%par_state<par_state_cond

       call save_proc_state
       call init_proc_state

       ! Set up code block and imports
       cblock=make_cblock(coder,pm_null_obj,node,sym_proc)

!!$       call push_par_scope(coder,cblock)
       coder%par_depth=coder%par_depth+1

       reg=>pm_register(coder%context,'tproc',tkeys)

       ! Different types of procedure
       npars=0
       flags=node_get_num(node,proc_flags)
       pr_flags=flags
       if(iand(flags,proccall_is_comm)/=0) then
          loop_pars=coder%top
!!$          if(iand(flags,proc_run_complete)/=0) then
!!$             complete=.true.
!!$             call check_param_modes(sym_complete,sym_complete)
!!$          elseif(iand(flags,proc_is_uncond)/=0) then
!!$             complete=.true.
!!$          elseif(iand(flags,proc_is_cond)/=0) then
!!$             complete=.false.
!!$          else
             complete=old_complete
!!$          endif
          call code_params(cblock,.true.,argcall)
          call code_keys(cblock,tkeys,keycall)
          call code_loop_startup(cblock,cblock2,cblock3)
          call code_check(cblock3)
          call code_body(cblock3)
          call code_result(cblock3,flags)
          call code_loop_finish(cblock,cblock2,cblock3)
       else
          call code_params(cblock,.false.,argcall)
          call make_state_vars(coder,cblock,node,&
               topo=coder%var(coder%proc_base+1))
          call code_keys(cblock,tkeys,keycall)
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

!!$       call pop_par_scope(coder,cblock,node)

       coder%par_depth=coder%par_depth-1
       
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
      save_par_base=coder%par_base
      save_over_base=coder%over_base
      save_loop_cblock=coder%loop_cblock
      save_proc_par_depth=coder%proc_par_depth
      save_proc_nret=coder%proc_nret
      save_par_state=coder%par_state
      save_label=coder%label
      save_subs_index=coder%subs_index
      save_run_mode=coder%run_mode
      save_run_flags=coder%run_flags
      save_aliased=coder%aliased
      save_in_sync=coder%in_sync
      save_state_base=coder%state_base
      save_mask=coder%mask
    end subroutine save_proc_state

    subroutine init_proc_state
      coder%index=0
      coder%lex_scope=0
      coder%proc_base=coder%top
      coder%proc_ncalls=0
      coder%par_base=coder%top
      coder%over_base=coder%top+2
      coder%proc_par_depth=coder%par_depth
      coder%proc_nret=nret
      coder%par_state=par_state_outer
      coder%run_mode=sym_private
      coder%subs_index=-1
      coder%run_flags=0
      coder%aliased=.false.
      coder%in_sync=.false.
    end subroutine init_proc_state

    subroutine restore_proc_state
      coder%index=save_index
      coder%lex_scope=save_lex_scope
      coder%proc_base=save_proc_base
      coder%proc_ncalls=save_proc_ncalls
      coder%par_base=save_par_base
      coder%over_base=save_over_base
      coder%loop_cblock=save_loop_cblock
      coder%par_depth=coder%proc_par_depth
      coder%proc_par_depth=save_proc_par_depth
      coder%proc_nret=save_proc_nret
      coder%par_state=save_par_state
      coder%run_mode=save_run_mode
      coder%run_flags=save_run_flags
      coder%label=save_label
      coder%subs_index=save_subs_index
      coder%aliased=save_aliased
      coder%in_sync=save_in_sync
      coder%state_base=save_state_base
      coder%mask=save_mask
    end subroutine restore_proc_state

    subroutine code_params(cblock,iscomm,argcall)
      type(pm_ptr),intent(in):: cblock
      logical,intent(in):: iscomm
      type(pm_ptr),intent(out):: argcall
      type(pm_ptr):: name,var,p
      integer:: state,flags,cflags
      p=node_get(node,proc_params)
      if(.not.pm_fast_isnull(p)) then
         amp=node_get(node,proc_amplocs)
         if(pm_fast_isnull(amp)) then
            do i=1,node_numargs(p),2
               flags=var_is_param
               name=node_arg(p,i)
               if(name%offset==sym_dotdotdot) flags=var_is_varg
               call make_var(coder,cblock,p,name,flags)
            enddo
         else
            j=0
            amp=pm_name_val(coder%context,int(amp%offset))
            do i=1,node_numargs(p),2
               if(amp%data%i(amp%offset+j)==(i+1)/2) then
                  flags=var_is_ref+var_is_param+var_is_var
                  if(node_sym(node_arg(p,i+1))/=sym_pm_dref) then
                     flags=ior(flags,var_is_ref)
                  endif
                  if(j<pm_fast_esize(amp)) j=j+1
               else
                  flags=var_is_param
               endif
               name=node_arg(p,i)
               if(name%offset==sym_dotdotdot) flags=var_is_varg
               call make_var(coder,cblock,p,name,flags)
            enddo
         endif
         npars=npars+node_numargs(p)/2
         call make_basic_sp_call(coder,cblock,p,&
              sym_open,npars,0,coder%par_depth)
         argcall=cnode_get(cnode_get(cblock,cblock_last_call),call_args)
      else
         argcall=pm_null_obj
      endif
    end subroutine code_params

    subroutine export_params
      integer:: i,flags
      type(pm_ptr):: var
      p=node_get(node,proc_params)
      do i=2,npars
         var=coder%var(loop_pars+i)
         call make_var(coder,cblock,p,cnode_get(var,var_name),&
              ior(iand(cnode_get_num(var,var_flags),&
              var_is_var+var_is_ref),var_is_shadowed))
         call code_val(coder,var)
         call make_basic_sp_call(coder,cblock,p,sym_export_param,&
              1,1,coder%par_depth)
      enddo
    end subroutine export_params

    subroutine check_param_modes(mode,flag_sym)
      integer,intent(in):: mode,flag_sym
      type(pm_ptr):: p,arg
      integer:: i
      p=node_get(node,proc_params)
      do i=num_comm_args*2+2,node_numargs(p),2
         arg=node_arg(p,i)
         if(node_sym(arg)/=sym_mode) then
            call code_error(coder,node,&
                 'All parameters for "<<'//trim(sym_names(flag_sym))//&
                 '>>" procedure must have an explicit mode')
         else
            if(.not.pm_mode_compatable(mode,node_num_arg(arg,2))) then
               call code_error(coder,arg,&
                    'Parameter for "<<'//trim(sym_names(flag_sym))//&
                    '>>" procedure cannot have this mode: ',node_arg(arg,2))
            endif
         endif
      enddo
      p=node_get(node,proc_result_types)
      do i=1,node_numargs(p)
         arg=node_arg(p,i)
         if(node_sym(arg)==sym_mode) then
            call code_error(coder,node,&
                 'Return modes for a "<<'//&
                 trim(sym_names(flag_sym))//&
                 '>>" procedure must be undefined or "'//&
                 trim(sym_names(mode)))
         endif
      enddo
    end subroutine check_param_modes
 
    recursive subroutine code_keys(cblock,tkeys,key_call)
      type(pm_ptr),intent(in):: cblock
      type(pm_ptr),intent(inout):: key_call
      type(pm_ptr),intent(inout),target:: tkeys
      type(pm_ptr):: p,vname,typ,cblock2
      integer:: i,n,base,vbase,wbase,tno

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
         vname=node_arg(p,i)
         call push_word(coder,int(vname%offset))
         call make_var(coder,cblock,p,vname,&
              var_is_param+var_is_key+var_is_multi_access)
      enddo

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
         vname=node_arg(p,i)
         call make_var(coder,cblock,p,vname,&
              var_is_key+var_is_multi_access+var_is_shadowed,&
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
      type(pm_ptr):: p,q
      integer:: status,i,j

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
               call make_basic_sp_call(coder,cblock,node,sym_import_param,1,1,&
                    coder%par_depth)
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

    ! This sets up a par-loop structure for comm proc
    subroutine code_loop_startup(cblock,cblock2,cblock3)
      type(pm_ptr),intent(in):: cblock
      type(pm_ptr),intent(out):: cblock2,cblock3
      integer:: iter

      cblock3=cblock
      cblock2=cblock

!!$      !coder%over_base=coder%top
!!$      call push_var(coder,sym_for,&
!!$           coder%var(loop_pars+1))
!!$
!!$
!!$      call make_sys_var(coder,cblock,node,sym_in,0)
!!$
!!$      iter=coder%top
!!$      coder%par_base=iter
!!$      call make_sys_var(coder,cblock,node,sym_pling,var_is_shadowed)
!!$      call code_val(coder,coder%var(iter+lv_distr))
!!$      call make_sys_call(coder,cblock,node,sym_get_tile_sz,1,2)
!!$      call make_sys_var(coder,cblock,node,sym_hash,var_is_shadowed)
!!$      call var_set_par_depth(coder,coder%var(iter+lv_numz),coder%par_depth+1)
!!$      call drop_code(coder)
!!$      cblock2=make_cblock(coder,cblock,node,sym_proc)
!!$      coder%loop_cblock=cblock2
!!$      call drop_code(coder)
!!$
!!$      ! Alias the region and subregion variables
!!$      call push_var(coder,sym_region,coder%var(iter+lv_distr))
!!$      call push_var(coder,sym_subregion,coder%var(loop_pars+2))
!!$      coder%over_base =coder%top
!!$
!!$      call push_par_scope(coder,cblock2)
!!$      call push_var(coder,sym_here_in_tile,coder%var(loop_pars+3))
!!$      call make_sys_var(coder,cblock2,node,sym_here,var_is_shadowed)
!!$      call code_val(coder,coder%var(iter+lv_tile))
!!$      call code_val(coder,coder%var(iter+lv_index))
!!$      call make_sys_call(coder,cblock2,node,sym_get_element,2,1)
!!$
!!$      if(iter+lv_here/=coder%top) then
!!$         write(*,*) '#',iter+lv_here,coder%top
!!$         call pm_panic('iter mismatch in code_loop_startup')
!!$      endif
!!$
!!$      coder%par_state=par_state_for
!!$      coder%run_mode=sym_complete
!!$
!!$      coder%par_state=merge(par_state_for,par_state_masked,complete)
!!$
!!$      if(complete) pr_flags=ior(pr_flags,proc_run_complete)
!!$      cblock3=make_cblock(coder,cblock2,node,sym_for_stmt)
!!$
!!$      if(pm_is_compiling) then
!!$         ! Call PM__do_over to set-up subset loops
!!$         call make_sys_var(coder,cblock3,node,sym_nested_loop,var_is_shadowed)
!!$         call dup_code(coder)
!!$         call code_val(coder,coder%var(coder%over_base))
!!$         call code_val(coder,coder%var(iter+lv_distr))
!!$         call make_basic_sys_call(coder,cblock3,node,sym_do_over,2,1,&
!!$              coder%par_depth-1,call_inline_when_compiling)
!!$         call make_basic_sys_call(coder,cblock3,node,sym_nested_loop,1,0,&
!!$              coder%par_depth-1,call_inline_when_compiling)
!!$      endif

    end subroutine  code_loop_startup

    subroutine code_loop_finish(cblock,cblock2,cblock3)
      type(pm_ptr),intent(in):: cblock,cblock2,cblock3
!!$      call close_cblock(coder,cblock3)
!!$      call make_sp_call(coder,cblock2,node,sym_for,1,0)
!!$      call close_cblock(coder,cblock2)
!!$      call pop_par_scope(coder,cblock,node)
!!$      call code_val(coder,coder%var(coder%par_base+lv_numz))
!!$      call code_val(coder,cblock2)
!!$      call code_val(coder,coder%var(coder%par_base+lv_num))
!!$      call make_sp_call(coder,cblock,node,sym_pct,2,1)
    end subroutine code_loop_finish

  end subroutine trav_proc


  !========================================================
  ! Traverse a procedure parameter list
  ! !!! stuff to say
  !========================================================
   subroutine trav_params(coder,cblock,paramlist,amps,step,pre_args)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,paramlist
    integer,intent(in):: amps,step,pre_args

    integer:: i,j,k,flags,nargs,name
    type(pm_ptr):: amp
    nargs=node_numargs(paramlist)
    if(amps==0) then
       do i=1,nargs,step
          flags=var_is_param
          name=node_num_arg(paramlist,i)
          if(name==sym_dotdotdot) flags=var_is_varg
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
             flags=var_is_param
          endif
          name=node_num_arg(paramlist,i)
          if(name==sym_dotdotdot) flags=var_is_varg
          call make_sys_var(coder,cblock,paramlist,name,flags)
       enddo
    endif
    call make_basic_sp_call(coder,cblock,paramlist,&
         sym_open,nargs/step+pre_args,0,coder%par_depth)
  contains
    include 'fesize.inc'
  end subroutine trav_params

  !========================================================
  ! Create a procedure constant
  !========================================================
  subroutine proc_const(coder,cblock,pnode,pr)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,pr
    integer:: name
    type(pm_ptr):: p
    integer(pm_ln):: m
    logical:: ok

    if(node_numargs(pr)==1) then
       p=find_decl(coder,pnode,node_arg(pr,1),modl_proc)
    else
       p=find_imported_decl(coder,pnode,&
            node_arg(pr,1),node_arg(pr,2),modl_proc)
    endif
    if(pm_fast_isnull(p)) then
       call code_error(coder,pnode,&
            'proc value not associated with any defined procedure: ',pr)
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
    logical:: ok
    integer(pm_ln):: m
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
    integer:: flags,i,j,n,sym,tno,wbase,nret
    type(pm_ptr):: amp,p,arg

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
       procdef=find_decl(coder,node,pname,modl_proc)
       if(pm_fast_isnull(procdef)) then
          call code_error(coder,node,&
               'Cannot find procedure: ',pname)
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
    logical:: ok
    type(pm_type_einfo):: einfo

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
               iand(cnode_get_num(proc2,pr_flags),proccall_is_comm+proc_is_cond)) then
             if(debug_more_codegen) write(*,*) 'SIG DIFFERENT'
             sig%data%ptr(sig%offset+cnode_args+j-2)=proc2
             j=j+1
          else if(pm_type_includes(coder%context,typ2,typ1,pm_type_incl_type,&
               einfo)) then
             if(pm_type_includes(coder%context,typ1,typ2,pm_type_incl_type,&
                  einfo)) then
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
             if(pm_type_includes(coder%context,typ1,typ2,pm_type_incl_type,&
                  einfo)) then
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
                 rtype1,rtype2,pm_type_incl_type,einfo)) then
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
    type(pm_ptr),intent(in):: cblock,node,name
    type(pm_ptr),intent(in),optional:: name2
    type(pm_ptr):: v
    type(pm_ptr):: p
    logical:: ok
    integer:: i,n
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
    type(pm_ptr),intent(in):: node,name
    integer,intent(in):: where
    type(pm_ptr):: ptr
    type(pm_ptr):: v,modl
    modl=node_get_modl(node)
    v=pm_dict_lookup(coder%context,modl%data%ptr(modl%offset+where),name)
    if(pm_fast_isnull(v)) then
       v=pm_dict_lookup(coder%context,&
            modl%data%ptr(modl%offset+where+modl_local),name)
       if(pm_fast_isnull(v)) then
          ptr=v
          return
       endif
    endif
    ptr=v%data%ptr(v%offset)
  contains
    include 'fisnull.inc'
  end function find_decl

  !========================================================
  ! Find declaration correspoding to name::name
  !========================================================
  function find_imported_decl(coder,node,name1,name2,where,noerr) result(p)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node,name1,name2
    integer,intent(in):: where
    logical,intent(in),optional:: noerr
    type(pm_ptr):: p
 
    type(pm_ptr):: thismodl,modl,dict
    character(len=5):: str
    thismodl=node_get_modl(node)
    modl=pm_dict_lookup(coder%context,&
         thismodl%data%ptr(thismodl%offset+modl_include),&
         name1)
    if(pm_fast_isnull(modl)) then
       call code_error(coder,node,'No such module: ',name1)
       p=pm_null_obj
    else
       modl=node_arg(modl,2)
       p=pm_dict_lookup(coder%context,&
            modl%data%ptr(modl%offset+where),name2)
       if(pm_fast_isnull(p)) then
          if(.not.present(noerr)) then
             str='proc'
             if(where==modl_type) then
                str='type'
             elseif(where==modl_param) then
                str='param'
             endif
             call code_error(coder,node,'Cannot find '//str//' '//&
                  trim(pm_name_as_string(coder%context,int(name2%offset)))//&
                  ' in: ',name1)
          endif
       else
          p=p%data%ptr(p%offset)
       endif
    endif
  contains
    include 'fisnull.inc'
  end function find_imported_decl

  
  !===============================================
  ! Push two implicit communicating proc arguments
  !===============================================
  subroutine make_comm_call_args(coder,cblock,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: cblock,node
    if(coder%par_base==0) then
       call make_temp_var(coder,cblock,node)
       call make_temp_var(coder,cblock,node)
       call make_temp_var(coder,cblock,node)      
    else
       call code_val(coder,coder%var(coder%par_base+lv_distr))
       call code_val(coder,coder%var(coder%over_base))
       call code_val(coder,coder%var(coder%par_base+lv_index))
    endif
  end subroutine make_comm_call_args

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



  !==========================================
  ! Set the parallel depth of a variable
  !==========================================
  subroutine var_set_par_depth(coder,var,depth)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: var
    integer:: depth
    call cnode_set_num(var,var_par_depth,depth-coder%proc_par_depth)
  end subroutine var_set_par_depth

  !=====================================================
  ! Check if a variable is local to the current loop
  !=====================================================
  function var_private(coder,var) result(islocal)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: var
    logical:: islocal
    islocal=.false.
    if(pm_fast_vkind(var)==pm_pointer) then
       if(cnode_get_kind(var)==cnode_is_var) then
          islocal=cnode_get_num(var,var_par_depth)==&
               coder%par_depth-coder%proc_par_depth
       endif
    endif
  contains
    include 'fvkind.inc'
  end function var_private

  !================================
  ! Is value shared?
  !================================
  function var_shared(coder,var) result(isshared)
   type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: var
    logical:: isshared
    integer:: depth
    isshared=.true.
    if(pm_fast_vkind(var)==pm_pointer) then
       if(cnode_get_kind(var)==cnode_is_var) then
          depth=cnode_get_num(var,var_par_depth)
          isshared=depth<=coder%par_depth-1-coder%proc_par_depth
       endif
    endif
  contains
    include 'fvkind.inc'
  end function var_shared

  !=============================================================================
  ! Check if a variable was created in parallel scope containing current loop
  !=============================================================================
  function var_outer(coder,var) result(isouter)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: var
    logical:: isouter
    isouter=.false.
    if(pm_fast_vkind(var)==pm_pointer) then
       if(cnode_get_kind(var)==cnode_is_var) then
          isouter=cnode_get_num(var,var_par_depth)==&
               coder%par_depth-1-coder%proc_par_depth
       endif
    endif
  contains
    include 'fvkind.inc'
  end function var_outer

  !===================================
  ! Find a variable
  !===================================
  function find_var(coder,name) result(v)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: name
    type(pm_ptr):: v
    integer:: i
    integer::n
    n=name%offset
    i=find_var_entry(coder,n,coder%proc_base)
    if(i/=0) then
       v=coder%var(i)
       if(i<=coder%block_base) then
          call import_to_block_scope(coder,i,v,coder%block_entry)
       endif
    else
       v=pm_null_obj
    endif
    return
  end function find_var

  !==========================================
  ! Find a variable and its table entry
  !==========================================
  function find_var_and_entry(coder,name,i) result(v)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: name
    integer,intent(out):: i
    type(pm_ptr):: v
    integer::n
    n=name%offset
    i=find_var_entry(coder,n,coder%proc_base)
    if(i/=0) then
       v=coder%var(i)
       if(i<=coder%block_base) then
          call import_to_block_scope(coder,i,v,coder%block_entry)
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
    type(pm_ptr):: node

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

  !====================================================
  ! Undo hide_vars for block of variables
  !====================================================
  subroutine reveal_vars(coder,start,end)
    type(code_state),intent(inout):: coder
    integer,intent(in):: start,end
    integer:: i
    do i=start,end
       coder%stack(i)=-coder%stack(i)
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
    if(coder%par_state>=par_state_cond) then
       flags=ior(flags,var_is_incomplete)
    endif
    if(coder%par_state==par_state_par) then
       flags=ior(flags,var_is_par_var)
    endif
    call code_num(coder,flags)
    call code_null(coder)
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    call code_val(coder,&
         pm_fast_tinyint(coder%context,coder%par_depth-coder%proc_par_depth))
    call code_val(coder,&
         pm_fast_tinyint(coder%context,coder%par_depth-coder%proc_par_depth))
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
    call make_var(coder,cblock,node,&
         pm_fast_name(coder%context,name),flags)
  contains
    include 'fname.inc'  
  end subroutine make_sys_var

  !====================================
  ! Make a local variable
  !====================================
  subroutine make_var(coder,cblocka,node,name,flags,extra_info)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblocka,node,name
    integer,intent(in):: flags
    type(pm_ptr),optional:: extra_info
    type(pm_ptr):: var,link,cblock
    logical:: local
    integer:: vflags
    
    ! Check for prior definition
    if(iand(flags,var_is_shadowed+var_is_imported)==0) then
       var=find_var(coder,name)
       if(.not.pm_fast_isnull(var)) then
          if(pm_debug_checks) then
             if(name%offset==0) call pm_panic('null name in make_var')
          endif
          call code_error(coder,node,&
               'Cannot redefine local variable or constant:',name)
          call code_val(coder,var)
          return
       endif
    endif

    if(cnode_get_name(cblocka,cblock_sym)==sym_sync.or.&
         cnode_get_name(cblocka,cblock_sym)==sym_any) then
       cblock=cnode_get(cblocka,cblock_parent)
    else
       cblock=cblocka
    endif
    
    ! Create variable node
    call code_val(coder,cblock)
    call code_val(coder,name)

    ! Flag variables according to current par state
    vflags=flags
    if(coder%par_state>=par_state_cond) then
       vflags=ior(vflags,var_is_incomplete)
    endif
    if(coder%par_state==par_state_par) then
       vflags=ior(vflags,var_is_par_var)
    endif
    
    ! All named variables multi access (this may change)
    call code_num(coder,ior(vflags,var_is_multi_access))
    call code_null(coder)
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    call code_val(coder,pm_fast_tinyint(coder%context,&
         coder%par_depth-coder%proc_par_depth))
    call code_val(coder,pm_fast_tinyint(coder%context,&
         coder%par_depth-coder%proc_par_depth))
    call code_num(coder,coder%lex_scope)
    if(present(extra_info)) then
       call code_val(coder,extra_info)
       call make_code(coder,node,cnode_is_var,var_node_size+1)
    else
       call make_code(coder,node,cnode_is_var,var_node_size)
    endif

    ! Add variable to stack
    call push_var(coder,int(name%offset),top_code(coder))

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
    integer:: i,j

    if(name==0) return
    if(coder%top>=max_code_stack) then
       call pm_panic('Program too complex')
    endif
    coder%top=coder%top+1
    j=coder%top
    coder%stack(j)=name
    coder%var(j)=var
    coder%imps(j)=0
  end subroutine push_var

  !=====================================
  ! Pop variables down to newbase
  !=====================================
  subroutine pop_vars_to(coder,newbase)
    type(code_state),intent(inout):: coder
    integer,intent(in):: newbase
    integer:: i,old_top
    old_top=coder%top
    coder%top=newbase
    do i=newbase+1,old_top
!!$       if(cnode_flags_clear(coder%var(i),var_flags,var_is_accessed+var_is_changed)) then
!!$          call cnode_error(coder,coder%var(i),'Variable is defined but never used: ',&
!!$               cnode_get(coder%var(i),var_name))
!!$       endif
       if(coder%imps(i)/=0) then
          if(coder%imps(i)<=coder%par_depth) then
             ! Keep imports to current parallel scope
             coder%top=coder%top+1
             coder%imps(coder%top)=coder%imps(i)
             coder%stack(coder%top)=coder%stack(i)
             coder%var(coder%top)=coder%var(i)
          elseif(par_depth(coder,coder%var(i))<coder%par_depth) then
             ! Keep imports through multiple parallel
             ! scopes to an inner scope that has been popped
             coder%top=coder%top+1
             coder%imps(coder%top)=coder%par_depth
             coder%stack(coder%top)=coder%stack(i)
             coder%var(coder%top)=cnode_get(coder%var(i),var_extra_info)
          end if
       endif
    enddo
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
    call make_const(coder,cblock,node,ptr,pm_new_fix_type(coder%context,ptr))
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
       tno=pm_new_literal_type(coder%context,val)
    else
       tno=pm_fast_typeof(val)
    endif
    if(coder%par_state/=par_state_outer) then
       tno=pm_type_add_mode(coder%context,tno,sym_invar)
    endif
    call code_val(coder,val)
    call code_num(coder,tno)
    call make_code(coder,node,cnode_is_const,2)
  contains
    include 'ftypeof.inc'
  end subroutine make_const

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
    integer:: depth,base,aflags
    aflags=0
    if(present(flags)) aflags=flags
    call make_arglist(coder,cblock,node,nargs,nret,.false.,.false.)
    call code_null(coder)
    call make_full_call(coder,cblock,node,&
         pm_fast_tinyint(coder%context,-sym),pm_null_obj,nargs,abs(nret),0,&
         pm_null_obj,ior(aflags,coder%run_flags),&
         pm_null_obj,coder%par_depth)
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
  subroutine make_basic_sp_call(coder,cblock,node,sym,nargs,nret,depth)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,nargs,nret,depth
    call make_arglist(coder,cblock,node,nargs,nret,.false.,.false.,notouch=.true.)
    call code_null(coder)
    call make_full_call(coder,cblock,node,&
          pm_fast_tinyint(coder%context,-sym),pm_null_obj,&
          nargs,abs(nret),0,pm_null_obj,coder%run_flags,&
          pm_null_obj,depth)
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
    type(pm_ptr):: procs,svect,avec
    integer:: flags,depth,base
    if(present(aflags)) then
       flags=aflags
    else
       flags=0
    endif
    flags=ior(flags,coder%run_flags)
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
         pm_null_obj,flags,pm_null_obj,coder%par_depth)
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
    type(pm_ptr):: procs,svect,avec
    integer:: depth,flags,base,narg
    narg=nargs+num_comm_args
    if(present(aflags)) then
       flags=ior(aflags,proccall_is_comm)
    else
       flags=proccall_is_comm
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
         pm_null_obj,coder%par_depth)
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
  subroutine make_basic_sys_call(coder,cblock,node,sym,narg,nret,depth,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret,depth,flags
    type(pm_ptr):: procs,svect
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,sym))
    call make_arglist(coder,cblock,node,narg,nret,.true.,.false.,.true.)
    call code_null(coder)
    call make_full_call(coder,cblock,node,&
         procs,pm_null_obj,narg+1,abs(nret),0,pm_null_obj,&
         ior(flags,coder%run_flags),pm_null_obj,depth)
  contains
    include 'fname.inc'
  end subroutine make_basic_sys_call

  !==========================================
  ! Make a procedure call
  ! Argument list and key argument list (or null)
  ! must be on top of vstack.
  !==========================================
  subroutine make_full_call(coder,cblock,node,procs,&
       amps,nargs,nret,nkeys,keynames,iflag,var,depth)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,procs,amps,var,keynames
    integer,intent(in):: nargs,nret,nkeys,iflag,depth
    type(pm_ptr):: p,q,n,args,keys
    integer:: i
    if(pm_debug_checks) then
       if(cnode_get_kind(cblock)/=cnode_is_cblock) then
          call pm_panic('full call cblock')
       endif
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
    call code_num(coder,depth-coder%proc_par_depth)
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

    !write(*,*) '#nargs=',cnode_numargs(cnode_get(n,call_args))
    
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
  subroutine make_arglist(coder,cblock,node,nargs,nret,isstd,iscomm,notouch)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: nargs,nret
    logical,intent(in):: isstd,iscomm
    logical,intent(in),optional:: notouch
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
       extra0=coder%state_base
       nextra=num_comm_args-1
    elseif(isstd) then
       extra0=coder%state_base
       nextra=1
    else
       extra0=coder%state_base
       nextra=0
    endif
   
    arglist=make_arglist_cnode(coder,node,abs(nret),ret0,nextra,extra0,iscomm,nargs,arg0)
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
      type(pm_ptr)::p
!!! check for chan and deref if required
!!! Check for block import
      if(pm_fast_vkind(p)==pm_pointer) then
         if(cnode_get_kind(p)==cnode_is_var) then
            call update_change_lists(coder,p,.false.)
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
    integer:: i,ii
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
    type(pm_ptr):: temp
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
       write(iunit,'(A,A,"      Idx=",I4," Dep=",I4," Chng=",L1," Flags=",I4,"offset=",I6,I6,L)') &
            spaces(1:depth*2),trim(str),&
            cnode_get_num(node,var_index),&
            cnode_get_num(node,var_par_depth),&
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
             p=pm_dict_key(coder%context,coder%sig_cache,&
                  int(p%offset,pm_ln))
             call pm_name_string(coder%context,&
                  p%data%i(p%offset+pm_fast_esize(p)),str)
             write(iunit,'(A,A,A,A,"      Idx=",I4," Depth=",I4," Flags=",I4,"<",I4,">")') &
                  spaces(1:depth*2),'Call (',trim(str),') (',&
                  cnode_get_num(node,call_index),&
                  cnode_get_num(node,call_par_depth),cnode_get_num(node,call_flags),po
          endif
          if(.not.pm_fast_isnull(rvec)) then
             i= rvec%data%i(rvec%offset+&
                  cnode_get_num(node,call_index))
             if(i<0) then
                if(i==spsig_thru) then
                   write(iunit,*) spaces(1:depth*2),' sp_sig_thru'
                elseif(i==spsig_dup) then
                   write(iunit,*) spaces(1:depth*2),' sp_sig_dup'
                elseif(i==spsig_noop) then
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
    if(.not.present(args).and.&
         iand(flags,proccall_is_comm)/=0&
         .and.coder%par_state>=par_state_cond) then
       str(n+2:)='Conditional context'
    endif
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
    type(pm_ptr),intent(in),optional:: name
    logical,intent(in),optional:: warn
    character(len=256):: str
    type(pm_ptr):: modname
    if(pm_main_process) then
       write(*,*)
       if(.not.pm_fast_isnull(node)) then
          call pm_error_header(coder%context,node_get_modl_name(node),&
                  node_get_lineno(node),node_get_charno(node))
       endif
       if(.not.present(warn)) then
          if(present(name)) then
             call pm_name_string(coder%context,int(name%offset),str)
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
    type(pm_ptr):: modname
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

