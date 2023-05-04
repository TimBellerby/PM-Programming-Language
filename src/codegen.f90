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

!================================================
! The following routines process the parse tree
! into an intermediate form representation
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
  use pm_parser
  use pm_sysdefs
  implicit none

  logical,parameter:: debug_codegen=.false.
  logical,parameter:: debug_more_codegen=.false.

  ! Langauge features
  integer,parameter:: num_comm_args=3
  
  ! Limits
  integer,parameter:: max_code_stack=4096
  integer,parameter:: code_local_hash=1024
  integer,parameter:: max_par_depth=256
  integer,parameter:: max_type_nesting=64
  integer,parameter:: max_error_nodes=1024

  ! Magic number for code tree nodes
  integer,parameter:: cnode_magic_no=10456_pm_p
  
  ! Offsets common to all cnode structures
  integer,parameter:: cnode_magic=0
  integer,parameter:: cnode_kind=1
  integer,parameter:: cnode_modl_name=2
  integer,parameter:: cnode_lineno=3
  integer,parameter:: cnode_charno=4
  integer,parameter:: cnode_args=5

  ! Types of cnode structure
  integer,parameter:: cnode_is_cblock=1
  integer,parameter:: cnode_is_var=2
  integer,parameter:: cnode_is_const=3
  integer,parameter:: cnode_is_call=4
  integer,parameter:: cnode_is_arglist=5
  integer,parameter:: cnode_is_builtin=6
  integer,parameter:: cnode_is_proc=7
  integer,parameter:: cnode_is_resolved_proc=8
  integer,parameter:: cnode_is_arg_constraint=9
  integer,parameter:: cnode_is_par_constraint=10
  integer,parameter:: cnode_is_typ_constraint=11
  integer,parameter:: cnode_is_interface_constraint=12
  integer,parameter:: cnode_is_any_sig=13
  integer,parameter:: cnode_is_autoconv_sig=14
  integer,parameter:: cnode_num_kinds=14

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
  integer,parameter:: cblock_last_loop_call=cnode_args+9
  integer,parameter:: cblock_var_inits=cnode_args+10
  integer,parameter:: cblock_node_size=11

  ! Flags for cblocks
  integer,parameter:: cblock_is_comm=1

  ! Offsets into call cnodes
  integer,parameter:: call_args=cnode_args+0
  integer,parameter:: call_parent=cnode_args+1
  integer,parameter:: call_sig=cnode_args+2
  integer,parameter:: call_flags=cnode_args+3
  integer,parameter:: call_link=cnode_args+4
  integer,parameter:: call_nret=cnode_args+5
  integer,parameter:: call_nkeys=cnode_args+6
  integer,parameter:: call_index=cnode_args+7
  integer,parameter:: call_par_depth=cnode_args+8
  integer,parameter:: call_var=cnode_args+9
  integer,parameter:: call_amp=cnode_args+10
  integer,parameter:: call_node_size=11

  ! Flags for call nodes
  ! (call_is_comm=1.. defined in parser)
  integer,parameter:: call_is_fixed = 2**10
  integer,parameter:: call_is_assign_call = 2**11
  integer,parameter:: call_is_vararg = 2**12
  integer,parameter:: call_inline_when_compiling = 2**13
  integer,parameter:: call_dup_result = 2**14
  integer,parameter:: call_is_cond = 2**15
  integer,parameter:: call_is_no_touch = 2**16
  integer,parameter:: call_is_unlabelled = 2**17
  
  ! Offsets into var cnodes
  integer,parameter:: var_parent=cnode_args+0
  integer,parameter:: var_name=cnode_args+1
  integer,parameter:: var_flags=cnode_args+2
  integer,parameter:: var_link=cnode_args+3
  integer,parameter:: var_index=cnode_args+4
  integer,parameter:: var_par_depth=cnode_args+5
  integer,parameter:: var_create_depth = cnode_args + 6
  integer,parameter:: var_node_size=7
  integer,parameter:: var_extra_info=cnode_args+7

  ! Flags for var cnodes
  integer,parameter:: var_is_var=1
  integer,parameter:: var_is_ref=2
  integer,parameter:: var_is_param=4
  integer,parameter:: var_is_shadowed=16
  integer,parameter:: var_is_imported=32
  integer,parameter:: var_is_accessed=64
  integer,parameter:: var_is_changed=128
  integer,parameter:: var_is_multi_access=256
  integer,parameter:: var_is_key=512
  integer,parameter:: var_is_varg=1024
  integer,parameter:: var_is_par_var=2048
  integer,parameter:: var_is_incomplete=4096
  integer,parameter:: var_is_aliased=8192
  integer,parameter:: var_is_not_inited=16384
  integer,parameter:: var_is_no_import_export=32768

  ! Offsets into proc nodes
  integer,parameter:: pr_cblock=cnode_args+0
  integer,parameter:: pr_max_index=cnode_args+1
  integer,parameter:: pr_recurse=cnode_args+2
  integer,parameter:: pr_id=cnode_args+3
  integer,parameter:: pr_rtype=cnode_args+4 ! Must be same as bi_rtype
  integer,parameter:: pr_nargs=cnode_args+5
  integer,parameter:: pr_nkeys=cnode_args+6
  integer,parameter:: pr_nret=cnode_args+7
  integer,parameter:: pr_flags=cnode_args+8
  integer,parameter:: pr_name=cnode_args+9
  integer,parameter:: pr_ncalls=cnode_args+10
  integer,parameter:: pr_tkeys=cnode_args+11
  integer,parameter:: pr_node_size=12

  ! Offsets into builtin nodes
  integer,parameter:: bi_opcode=cnode_args+0
  integer,parameter:: bi_opcode2=cnode_args+1
  integer,parameter:: bi_data=cnode_args+2
  integer,parameter:: bi_flags=cnode_args+3
  integer,parameter:: bi_rtype=cnode_args+4  ! Must be same as pr_rtype
  integer,parameter:: bi_rcode=cnode_args+5
  integer,parameter:: bi_rsym=cnode_args+6
  integer,parameter:: bi_id=cnode_args+7
  integer,parameter:: bi_node_size=8

  ! Parallel status of a value
  integer,parameter:: value_is_shared=0
  integer,parameter:: value_is_private=1

  ! Flags indicating state within parallel statement nesting
  integer,parameter:: par_state_nhd=0
  integer,parameter:: par_state_outer=1
  integer,parameter:: par_state_for=2
  integer,parameter:: par_state_sync=3
  integer,parameter:: par_state_loop=4
  integer,parameter:: par_state_cond_loop=5
  ! -- The following are conditional states (can check >=par_state_cond)
  integer,parameter:: par_state_cond=6
  integer,parameter:: par_state_par=7
  integer,parameter:: par_state_masked=8
  integer,parameter:: par_state_over=9
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
  integer,parameter:: typevar_start=-88
  integer,parameter:: typevar_end=-99

  ! Must match definitions in infer
  integer(pm_p),parameter:: spsig_thru=-4_pm_p
  integer(pm_p),parameter:: spsig_dup=-5_pm_p
  integer(pm_p),parameter:: spsig_noop=-6_pm_p

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
     ! Fixed size hash table to search variable names (chained using link())
     integer,dimension(max_code_stack):: stack,link
     type(pm_ptr),dimension(max_code_stack):: var
     integer,dimension(code_local_hash):: hash
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
     type(pm_ptr),dimension(max_par_depth):: &
          imports,import_cblock,region
     integer:: par_depth,proc_par_depth
     integer:: par_base,over_base

     ! Caches for call signatures and resolved procedures
     type(pm_ptr):: sig_cache,proc_cache,poly_cache

     ! Lists of deferred type checks
     type(pm_ptr):: defer_check

     ! Procedures as values
     integer:: call_sig
     type(pm_ptr):: proc_name_vals

     ! Misc values
     type(pm_ptr):: temp,temp2,true,false,one,comm_amp,check_mess,undef_val

     ! 'true and 'false types
     integer:: true_name,false_name

     ! '1 type
     integer:: unit_type

     ! Contextual information for this point in the traverse
     type(pm_ptr):: proc, proc_keys
     integer:: proc_base,proc_nret,proc_key_base,proc_ncalls
     integer:: run_mode,run_flags,par_state
     type(pm_ptr):: label,default_label
     logical:: fixed,aliased

     ! This point in a subscript tuple
     integer:: subs_index

     ! Counter to give each proc a unique index
     integer:: id
     
     ! Counter to provide unique index for all nodes created
     integer:: index

     ! Flags indicating type inference not complete
     logical:: types_finished,redo_calls,incomplete,first_pass

     ! Taints
     integer:: taints,proc_taints
     
     ! This is the parallel kind storeageless implicit argument
     integer:: par_kind,par_kind2

     ! Type inference flag recursion -- use to locate infinite recursion
     logical:: flag_recursion
 
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
    coder%hash=0
    coder%reg=>pm_register(context,'coder-var stack',coder%temp,coder%temp2,&
         coder%sig_cache,coder%proc_cache,coder%true,coder%false,coder%one,&
         coder%undef_val,&
         array=coder%var,array_size=coder%top)
    coder%reg2=>pm_register(context,'coder-node stack',&
         coder%proc_name_vals,coder%poly_cache,coder%comm_amp,array=&
         coder%vstack,array_size=coder%vtop)
    coder%reg3=>pm_register(context,'coder-for stack',coder%defer_check,&
         coder%check_mess,&
         array=coder%imports,&
         array_size=coder%par_depth)
    coder%sig_cache=pm_dict_new(context,32_pm_ln)
    coder%prog_cblock=pm_null_obj
    coder%defer_check=pm_null_obj
    coder%proc_base=1
    coder%link(coder%proc_base)=0
    coder%proc_ncalls=0
    coder%par_base=0
    coder%over_base=0
    coder%par_depth=0
    coder%proc_par_depth=0
    coder%par_state=par_state_outer
    coder%run_mode=sym_complete
    coder%run_flags=0
    coder%loop_cblock=pm_null_obj
    coder%proc_keys=pm_null_obj
    coder%index=0
    coder%true=pm_new_small(context,pm_logical,1_pm_p)
    coder%true%data%l(coder%true%offset)=.true.
    coder%false=pm_new_small(context,pm_logical,1_pm_p)
    coder%false%data%l(coder%false%offset)=.false.

    coder%one=pm_new_small(context,pm_long,1_pm_p)
    coder%one%data%ln(coder%one%offset)=1
    coder%unit_type=pm_new_value_typ(coder%context,coder%one)

    coder%one=pm_new_small(context,pm_int,1_pm_p)
    coder%one%data%i(coder%one%offset)=1
    coder%comm_amp=pm_new_small(context,pm_int,1_pm_p)
    coder%comm_amp%data%i(coder%comm_amp%offset)=num_comm_args+1
    
    coder%one=pm_fast_tinyint(coder%context,&
         pm_intern_val(coder%context,coder%one))
    coder%comm_amp=pm_fast_tinyint(coder%context,&
         pm_intern_val(coder%context,coder%comm_amp))
    coder%check_mess=pm_new_string(coder%context,'Failed "check" or "test"')
    coder%proc_name_vals=pm_dict_new(coder%context,8_pm_ln)
    coder%id=0
    coder%true_name=pm_new_value_typ(coder%context,coder%true)
    coder%false_name=pm_new_value_typ(coder%context,coder%false)

    coder%default_label=pm_fast_name(coder%context,sym_pct)
    coder%label=coder%default_label
    coder%num_errors=0
    coder%supress_errors=.false.
    coder%fixed=.false.
    coder%aliased=.false.
    coder%subs_index=-1
  contains
    include 'fname.inc'
    include 'ftiny.inc'
    
    function name_type(n) result(u)
      integer,intent(in):: n
      integer:: u
      u=pm_new_name_typ(coder%context,n)
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
    
    prog_cblock=make_cblock(coder,pm_null_obj,stmt_list,sym_do)
    coder%prog_cblock=prog_cblock

    ! Some general constants
    call make_const(coder,prog_cblock,stmt_list,&
         pm_fast_tinyint(coder%context,-9999))
    coder%undef_val=pop_code(coder)

    ! filesystem variable
    call make_sys_var(coder,prog_cblock,stmt_list,sym_filesystem,var_is_var)
    call make_sys_call(coder,prog_cblock,stmt_list,sym_get_filesystem,0,1)
    call make_var_mode(coder,prog_cblock,stmt_list,coder%var(coder%top))
    
    call trav_stmt_list(coder,prog_cblock,stmt_list,stmt_list,sym_do)
    call make_sp_call(coder,prog_cblock,stmt_list,sym_do,1,0)
    call close_cblock(coder,prog_cblock)

    if(coder%num_errors/=0) return

    ! Finalise any var calls
    call complete_vcall_sigs(coder)
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
    integer:: i,j,n,sym,base,vbase,wbase
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
          call dump_parse_tree(coder%context,6,node,2)
       endif
       select case(sym)
       case(sym_if)
          save_par_state=coder%par_state
          call trav_xexpr(coder,cblock,node,&
               node_arg(node,1))
          if(coder%par_state>par_state_outer) then
             if(pm_fast_isnull(node_arg(node,3))) then
                coder%par_state=par_state_masked
             else
                coder%par_state=par_state_cond
             endif
          endif
          call trav_stmt_list(coder,cblock,node,&
               node_arg(node,2),sym_if)
          if(.not.pm_fast_isnull(node_arg(node,3))) then
             call trav_stmt_list(coder,cblock,&
                  node,node_arg(node,3),sym_if)
          else
             call code_null(coder)
          endif
          call resolve_if_inits(coder,node)
          call make_sp_call(coder,cblock,node,&
               sym_if,3,0)
          coder%par_state=save_par_state
       case(sym_if_invar)
          if(coder%par_state>=par_state_cond) then
             call code_error(coder,node,&
                  'Cannot have "if invar" in a conditional context')
          endif
          call trav_xexpr(coder,cblock,node,&
               node_arg(node,1))
          call code_check_invar(coder,cblock,node,top_code(coder))
          call trav_stmt_list(coder,cblock,node,&
               node_arg(node,2),sym_if_invar)
          if(.not.pm_fast_isnull(node_arg(node,3))) then
             call trav_stmt_list(coder,cblock,&
                  node,node_arg(node,3),sym_if_invar)
          else
             call code_null(coder)
          endif
          call resolve_if_inits(coder,node)
          call make_sp_call(coder,cblock,node,sym,3,0)
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
          if(pm_is_compiling) then
             call trav_xexpr(coder,cblock,node,node_arg(node,2))
             call make_sys_var(coder,cblock,node,sym_while,&
                 var_is_shadowed)
             cblock2=make_cblock(coder,cblock,node,sym_each)
             coder%par_state=save_par_state
             coder%par_state=par_state_for_loop(coder,node,coder%par_state,&
                  node_get_num(node,node_args)/=0,sym==sym_while_invar)
             call trav_open_stmt_list(coder,cblock2,node,&
                  node_arg(node,3))
             iscomm=cnode_flags_set(top_code(coder),cblock_flags,&
                  cblock_is_comm)
             call trav_xexpr(coder,cblock2,node,node_arg(node,2))
             if(sym==sym_while_invar) then
                call code_check_invar(coder,cblock2,node,top_code(coder))
             endif
             coder%par_state=save_par_state
             call close_cblock(coder,cblock2)
             if(iscomm.and.(sym/=sym_while_invar.or.&
                  coder%par_state>=par_state_cond)) then
                call make_const(coder,cblock,node,node_arg(node,1))
                call code_val(coder,coder%vstack(coder%vtop-3))
                call code_val(coder,coder%vstack(coder%vtop-5))
                call make_sp_call(coder,cblock,node,sym_start_loop,&
                     3,0)
                call make_sp_call(coder,cblock,node,&
                     sym_loop,3,0)
                call make_sp_call(coder,cblock,node,sym_end_loop,0,0)
             else
                call code_val(coder,coder%vstack(coder%vtop-3))
                call init_var(coder,cblock,node,&
                     coder%vstack(coder%vtop-3))
                call make_sp_call(coder,cblock,node,&
                     sym_loop,3,0)
             endif
             call drop_code(coder)
          else
             call make_const(coder,cblock,node,node_arg(node,1))
             cblock2=make_cblock(coder,cblock,node,sym_while)
             call trav_xexpr(coder,cblock2,node,node_arg(node,2))
             if(sym==sym_while_invar) then
                call code_check_invar(coder,cblock2,node,top_code(coder))
             endif
             call close_cblock(coder,cblock2)
             coder%par_state=save_par_state
             coder%par_state=par_state_for_loop(coder,node,coder%par_state,&
                  node_get_num(node,node_args)/=0,sym==sym_while_invar)
             call trav_stmt_list(coder,cblock,node,&
                  node_arg(node,3),sym_while)
             call make_sp_call(coder,cblock,node,sym_while,4,0)
             coder%par_state=save_par_state
          endif
       case(sym_until,sym_until_invar)
          if(pm_is_compiling) then
             call make_sys_var(coder,cblock,node,sym_while,&
                  var_is_shadowed)
          else
             call make_const(coder,cblock,node,node_arg(node,1))
          endif
          save_par_state=coder%par_state
          coder%par_state=par_state_for_loop(coder,node,coder%par_state,&
               node_get_num(node,node_args)/=0,sym==sym_until_invar)
          cblock2=make_cblock(coder,cblock,node,sym_until)
          call trav_open_stmt_list(coder,cblock2,node,&
               node_arg(node,3))
          iscomm=cnode_flags_set(top_code(coder),cblock_flags,cblock_is_comm)
          if(pm_is_compiling) then
             call make_temp_var(coder,cblock,node)
             call dup_code(coder)
             call trav_xexpr(coder,cblock2,node,node_arg(node,2))
             if(sym==sym_until_invar) then
                call code_check_invar(coder,cblock2,node,top_code(coder))
             endif
             call make_sys_call(coder,cblock2,node,sym_not,1,1)
             call close_cblock(coder,cblock2)
             if(sym==sym_until_invar.and.save_par_state<par_state_cond.or..not.iscomm) then
                call make_const(coder,cblock,node,coder%true)
                call init_var(coder,cblock,node,&
                     coder%vstack(coder%vtop-3))
                call make_sp_call(coder,cblock,node,&
                     sym_loop,3,0)
             else
                call make_const(coder,cblock,node,node_arg(node,1))
                call code_val(coder,coder%vstack(coder%vtop-3))
                call make_const(coder,cblock,node,coder%true)
                call make_sp_call(coder,cblock,node,&
                     sym_start_loop,3,0)
                call make_sp_call(coder,cblock,node,&
                     sym_loop,3,0)
                call make_sp_call(coder,cblock,node,sym_end_loop,0,0)
             endif
          else
             call trav_xexpr(coder,cblock2,node,node_arg(node,2))
             if(sym==sym_until_invar) then
                call code_check_invar(coder,cblock2,node,top_code(coder))
             endif
             call close_cblock(coder,cblock2)
             call make_sp_call(coder,cblock,node,&
                  sym_until,3,0)
          endif
          coder%par_state=save_par_state
       case(sym_do_stmt)
          call trav_stmt_list(coder,cblock,node,node_arg(node,2),sym_do)
          call make_sp_call(coder,cblock,node,sym_do,1,0)
       case(sym_mode)
          call trav_mode_stmt(coder,cblock,node,sym,.false.)
       case(sym_for)
          call trav_xexpr(coder,cblock,node,node_arg(node,2),node)
       case(sym_each,sym_foreach_invar)
          call trav_xexpr(coder,cblock,node,node_arg(node,1),node)
       case(sym_nhd)
          call trav_xexpr(coder,cblock,node,node_arg(node,3),node)
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
          call trav_var_no_init(coder,cblock,list,node)
       case(sym_with)
          base=coder%top
          call trav_open_stmt_list(coder,cblock,node,node_arg(node,1))
          j=coder%top
          call trav_open_stmt_list(coder,cblock,node,node_arg(node,2))
          call hide_vars(coder,base+1,j)
       case(sym_over)
          call trav_over_stmt(coder,cblock,list,node)
       case(sym_define)
          call trav_assign_define(coder,cblock,list,node)
       case(sym_sync_assign)
          call trav_assign_define(coder,cblock,node,node_arg(node,1),issync=.true.)
       case(sym_where,sym_check,sym_amp)
          call trav_xexpr(coder,cblock,listp,node)
       case(sym_sync)
          select case(coder%par_state)
          case(par_state_cond,par_state_par)
             save_par_state=coder%par_state
             coder%par_state=par_state_labelled
             coder%label=node_arg(node,2)
             call check_par_nesting(coder,cblock,node,.false.)
             call make_const(coder,cblock,node,&
                  node_arg(node,2))
             call make_sp_call(coder,cblock,node,sym_sync,1,0)
             if(.not.pm_fast_isnull(node_arg(node,1))) then
                call trav_open_stmt_list(coder,cblock,node,node_arg(node,1))
             endif
             call make_const(coder,cblock,node,&
                  pm_null_obj)
             call make_sp_call(coder,cblock,node,sym_sync,1,0)
             coder%par_state=save_par_state
             coder%label=coder%default_label
          case(par_state_outer)
             call code_error(coder,node,&
                  'Labelled statement not allowed outside of any parallel statement')
          case(par_state_nhd)
             call code_error(coder,node,&
                  'Labelled statement not allowed in a "nhd" statement')
          case(par_state_cond_loop)
             call code_error(coder,node,&
                  'Labelled statement in unlabelled loop')
          case(par_state_for,par_state_loop,par_state_masked,par_state_over)
             if(.not.pm_fast_isnull(node_arg(node,1))) then
                call trav_open_stmt_list(coder,cblock,node,node_arg(node,1))
             endif
          case default
             write(*,*) 'Par state=',coder%par_state
             call pm_panic('Unknown par state (sym_sync)')
          end select
       case(sym_par)
          call trav_par_stmt(coder,cblock,list,node)
       case(sym_any)
          call trav_any_stmt(coder,cblock,list,node)
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
                if(coder%par_state/=par_state_outer) then
                   call make_comm_call_args(coder,cblock,node)
                endif
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
          call check_par_nesting(coder,cblock,node,.false.)
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
          call check_par_nesting(coder,cblock,node,.false.)
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
          call pm_dump_tree(coder%context,6,node,2)
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
      case(sym_define)
         call trav_assign_define(coder,cblock,nodep,node)
      case(sym_sync_assign)
         call trav_assign_define(coder,cblock,node,node_arg(node,1),issync=.true.)
      case(sym_iter)
         if(node_sym(stmt)==sym_for) then
            call trav_for_stmt(coder,cblock,nodep,node,base,stmt)
         else
            call trav_foreach_stmt(coder,cblock,nodep,node,base,stmt)
         endif
      case(sym_nhd)
         call trav_nhd_stmt(coder,cblock,nodep,stmt,base)
      case(sym_list)
         call trav_exprlist(coder,cblock,nodep,node)
      case(sym_result)
         call push_word(coder,pm_typ_is_tuple)
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
    integer:: base,save_par_state
    call make_temp_var(coder,cblock,stmt)
    call dup_code(coder)
    base=coder%vtop
    call code_val(coder,var)
    call trav_xexpr(coder,cblock,stmt,node_arg(stmt,idx))
    if(sym==sym_if_invar) then
       call code_check_invar(coder,cblock,stmt,top_code(coder))
    endif
    call make_sys_call(coder,cblock,stmt,&
         sym_checkcase,coder%vtop-base,1)
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
    call resolve_if_inits(coder,stmt)
    call make_sp_call(coder,cblock,stmt,sym,3,0)
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
    sym=node_num_arg(node,2)
    if(coder%par_state==par_state_outer) then
       call code_error(coder,node,'Cannot have "'//&
            trim(sym_names(sym))//&
            '" statement outside of a parallel context')
    elseif(coder%par_state==par_state_nhd) then
       call code_error(coder,node,'Cannot have "'//&
            trim(sym_names(sym))//&
            '" statement in main body of "nhd" statement')
    endif
    
    save_run_mode=coder%run_mode
    save_run_flags=coder%run_flags
    coder%run_mode=sym
    select case(sym)
    case(sym_coherent:sym_mirrored)
       coder%run_flags=proc_run_complete+proc_run_always
    case(sym_shared)
       coder%run_flags=proc_run_shared+proc_run_always
    end select
    if(isexpr) then
       call trav_expr(coder,cblock,node,node_arg(node,1))
    else
       call trav_open_stmt_list(coder,cblock,node,node_arg(node,1))
    endif
    coder%run_mode=save_run_mode
    coder%run_flags=save_run_flags
  end subroutine trav_mode_stmt

  !========================================================
  ! Traverse "any" statement
  !========================================================
  recursive subroutine trav_any_stmt(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: k,i,j,n,flags,start,finish,vb
    type(pm_ptr):: cblock2,vlist,v,var

    if(pm_fast_isnull(node_arg(node,2))) then
       flags=var_is_shadowed+var_is_var
       call trav_expr(coder,cblock,node,node_arg(node,1))
    else
       flags=var_is_var
       call trav_xexpr(coder,cblock,node,node_arg(node,2))
    endif
    v=top_code(coder)
    cblock2=make_cblock(coder,cblock,node,sym_any)
    call make_var(coder,cblock,node,node_arg(node_arg(node,1),1),flags)
    vb=coder%top
    var=top_code(coder)
    start=coder%index
    call swap_code(coder)
    call trav_open_stmt_list(coder,cblock2,node,node_arg(node,3))
!!$    if(cnode_flags_set(cblock2,cblock_flags,cblock_is_comm)) then
!!$       call code_error(coder,node,&
!!$            '"any" statement cannot contain communicating operation')
!!$    endif
    if(cnode_flags_set(var,var_flags,var_is_changed)) then
       call make_temp_var(coder,cblock2,node)
       call dup_code(coder)
       call code_val(coder,var)
       call dup_expr(coder,v)
       call make_sys_call(coder,cblock2,node,sym_as,2,1)
       call hide_vars(coder,vb,vb)
       if(pm_fast_isnull(node_arg(node,2))) then
          call make_assignment(coder,cblock2,node,node_arg(node,1))
       else
          call make_assignment(coder,cblock2,node,node_arg(node,2))
       endif
       call reveal_vars(coder,vb,vb)
    endif
    finish=coder%index
    call close_cblock(coder,cblock2)
    call code_val(coder,v)
    v=pm_fast_newnc(coder%context,pm_int,2)
    coder%temp=v
    v%data%i(v%offset)=start
    v%data%i(v%offset+1)=finish
    call make_const(coder,cblock,node,coder%temp)
    call make_sp_call(coder,cblock,node,sym_any,3,1)
    call drop_code(coder)
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
    integer:: i,j,k,n,lbase,vbase,xbase,xbasev
    integer:: nlist,iter,iter2,sym,rbase,wbase,name,flags,sindex,sbase
    integer:: slot1,slot2,while_var,outmode,rflags
    integer(pm_p)::flag
    type(pm_ptr):: vlhs,procs,sig,xvar,p
    integer:: save_par_state,save_run_flags,save_run_mode
    type(pm_ptr):: save_loop_cblock
    logical:: iscomm,outer,invar,c_invar
    sym=node_sym(stmt)
    rbase=coder%vtop
    wbase=coder%wtop
    invar=sym==sym_foreach_invar
    c_invar=pm_is_compiling.and.invar
    rflags=merge(proc_run_shared,0,c_invar)

    if(debug_codegen) write(*,*) 'TRAVEACH>'

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
    coder%par_state=par_state_for_loop(coder,stmt,coder%par_state,&
         node_get_num(stmt,node_args+3)/=0,sym==sym_foreach_invar)
   
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
          call make_temp_var(coder,cblock,stmt)
          call dup_code(coder)
          call code_val(coder,coder%var(iter+lv_end))
          call trav_xexpr(coder,cblock,p,node_arg(p,1))
          if(invar) then
             call code_check_invar(coder,cblock,p,top_code(coder))
          endif
          call make_sys_call(coder,cblock,stmt,sym_and,2,1,aflags=rflags)
          call make_var_assignment(coder,cblock,stmt,coder%var(iter+lv_end),aflags=rflags)
       endif
    endif
    
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
          call trav_xexpr(coder,cblock2,stmt,node_arg(p,1))
          if(sym==sym_foreach_invar) then
             call code_check_invar(coder,cblock2,p,top_code(coder))
          endif
          cblock_main=make_cblock(coder,cblock2,stmt,sym_each)
          call make_const(coder,cblock_main,stmt,coder%false)
          call make_var_assignment(coder,cblock_main,stmt,&
               coder%var(iter+lv_end),aflags=rflags)
          call close_cblock(coder,cblock_main)
          cblock_main=make_cblock(coder,cblock2,stmt,sym_each)
          call call_next(coder,cblock_main,list,iter,invar)
          call close_cblock(coder,cblock_main)
          call make_sp_call(coder,cblock2,stmt,merge(sym_if_invar,sym_if,c_invar),3,0)
       else
          call call_next(coder,cblock2,list,iter,invar)
       endif
    else
       call call_next(coder,cblock2,list,iter,invar)
    endif

    ! Get elements for next iteration
    call code_val(coder,coder%var(iter+lv_end))
    cblock_main=make_cblock(coder,cblock2,stmt,sym_each)
    do i=1,nlist/2
       call make_temp_var(coder,cblock_main,stmt)
       call dup_code(coder)
       call code_val(coder,coder%vstack(lbase+i))
       call code_val(coder,coder%var(iter+lv_idx))
       call make_sys_call(coder,cblock_main,list,sym_get_element,2,1,aflags=rflags)
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
    call make_sp_call(coder,cblock2,stmt,merge(sym_if_invar,sym_if,c_invar),3,0)
    call close_cblock(coder,cblock2)
    
    ! Build call
    if(pm_is_compiling) then
       iscomm=cnode_flags_set(coder%vstack(coder%vtop),&
            cblock_flags,cblock_is_comm)
       if(iscomm.and.(sym/=sym_foreach_invar.or.save_par_state>=par_state_cond)) then
          call make_const(coder,cblock,stmt,node_arg(stmt,4))
          call code_val(coder,coder%var(iter+lv_end))
          call code_val(coder,coder%var(iter+lv_end))
          call define_sys_var(coder,cblock,stmt,sym_while,0)
          call code_val(coder,coder%var(coder%top))
          call make_sp_call(coder,cblock,stmt,sym_start_loop,3,0)
          call code_val(coder,coder%var(iter+lv_end))
          call swap_code(coder)
          call code_val(coder,coder%var(iter+lv_end))
          call make_sp_call(coder,cblock,stmt,sym_loop_body,3,0)
          call make_sp_call(coder,cblock,stmt,sym_end_loop,0,0)
       else
          call code_val(coder,coder%var(iter+lv_end))
          call swap_code(coder)
          call code_val(coder,coder%var(iter+lv_end))
          call make_sp_call(coder,cblock,stmt,sym_loop,3,0)
       endif
    else
       call code_val(coder,coder%var(iter+lv_end))
       call make_sp_call(coder,cblock,list,sym_each,3,0)
    endif
    
    coder%par_state=save_par_state

    ! Clean up
    coder%vtop=rbase
    
    call pop_vars_to(coder,vbase)

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
       coder%run_flags=proc_run_shared
    endif
    
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
          call code_num(coder,sym_mirrored)
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
       coder%run_flags=proc_run_shared
    endif
    
    dvar=coder%var(iter)
    ivar=coder%var(iter+lv_idx)
    svar=coder%var(iter+lv_state)
    evar=coder%var(iter+lv_end)
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

    if(invar.and.pm_is_compiling) then
       coder%run_flags=save_run_flags
    endif
    
  end subroutine call_next

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
    call make_temp_var(coder,cblock,list)
    call dup_code(coder)
    call repl_expr(coder,lbase+1)
    call make_sys_call(coder,cblock,list,shape_sym,1,1)
    call define_sys_var(coder,cblock,list,sym_for,var_is_shadowed)
  end subroutine trav_iter  

  !===================================================================
  ! Work out the parallel state within a sequential loop
  !===================================================================
  function par_state_for_loop(coder,node,oldstate,labelled,invar) result(newstate)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: oldstate
    logical,intent(in):: labelled,invar
    integer:: newstate
    if(invar.and.oldstate==par_state_outer) then
       call code_error(coder,node,&
            'Cannot have "invar" loop outside of a parallel context')
    elseif(invar.and.oldstate==par_state_nhd) then
       call code_error(coder,node,&
            'Cannot have "invar" loop in then main body of a "nhd" statement')
    endif
    
    if(invar.and.labelled) then
       call code_error(coder,node,&
            'An "invar" loop cannot be labelled')
    endif
    newstate=oldstate
    if(oldstate>=par_state_cond.and.&
         oldstate<=par_state_par) then
       if(labelled) then
          newstate=par_state_cond
       else
          newstate=par_state_cond_loop
       endif
    elseif(oldstate==par_state_for) then
       newstate=par_state_loop
    elseif(labelled) then
       if(oldstate==par_state_outer) then
          call code_error(coder,node,&
               'Cannot have labelled loop outside of any parallel context')
       elseif(oldstate==par_state_nhd) then
          call code_error(coder,node,&
               'Cannot have labelled loop in the main body of a "nhd" statement')
       elseif(oldstate==par_state_cond_loop) then
          call code_error(coder,node,&
               'Cannot have labelled loop inside unlabelled loop within conditional context')
       endif
    endif
  end function par_state_for_loop
  
  !==============================
  ! Traverse a nhd statement
  !==============================
  recursive subroutine trav_nhd_stmt(coder,cblock,pnode,node,base)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer,intent(in):: base
    type(pm_ptr):: list,namelist,loop,cblock2,cblock3,edges
    integer:: i,j,k,iter,var,nhd_base,join_base,expr_base,nvar_base,block_base,over_base,nlist,nvars
    integer:: save_par_state

    ! node for nhd statement is
    !  (nhd (name expr)* bounds)*,attr,subexp,block
   
    ! Traverse nhd extent and boundary expressions
    ! Leave nhd descriptors on vstack above nhd_base
    nhd_base=coder%vtop
    list=node_arg(node,1)
    nlist=node_numargs(list)
    do i=1,nlist,3
       call make_temp_var(coder,cblock,list)
       call dup_code(coder)
       call make_comm_call_args(coder,cblock,node)
       call trav_expr(coder,cblock,list,node_arg(list,i))
       call trav_expr(coder,cblock,list,node_arg(node_arg(list,i+2),1))
       var=coder%vtop
       call make_comm_call_args(coder,cblock,node)
       call dup_expr(coder,coder%vstack(var-1))
       call dup_expr(coder,coder%vstack(var))
       call make_comm_sys_call(coder,cblock,node_arg(list,i+2),sym_check_bounds,2,0)
       call make_comm_sys_call(coder,cblock,list,sym_pm_nhd,2,1)
    enddo

    ! Compute the envelope of the NHDs
    call dup_expr(coder,coder%vstack(nhd_base+1))
    if(nlist<=3) then
       call make_temp_var(coder,cblock,node)
       call swap_and_dup_code(coder)
       call make_sys_call(coder,cblock,node,sym_envelope,1,1)
    else 
       do i=1,nlist/3
          call make_temp_var(coder,cblock,node)
          call swap_and_dup_code(coder)
          call dup_expr(coder,coder%vstack(nhd_base+i))
          call make_sys_call(coder,cblock,node,sym_envelope,2,1)
       enddo
    endif
    call define_sys_var(coder,cblock,node,sym_envelope,var_is_shadowed)

    ! Traverse block attribute
    block_base=-1
    if(.not.pm_fast_isnull(node_arg(node,2))) then
       call trav_expr(coder,cblock,node,node_arg(node_arg(node,2),1))
       block_base=coder%vtop
    endif

    ! Hide any where clauses 
    if(base>=0) then
       call hide_vars(coder,base+1,coder%top)
    endif

    ! Traverse expressions
    expr_base=coder%vtop
    do i=1,nlist,3
       namelist=node_arg(list,i+1)
       do j=1,node_numargs(namelist),2
          call trav_expr(coder,cblock,node,node_arg(namelist,j+1))
       enddo
    enddo

    ! Create extended tile variables
    nvar_base=coder%top
    nvars=0
    do i=1,nlist,3
       namelist=node_arg(list,i+1)
       do j=1,node_numargs(namelist),2
          nvars=nvars+1
          call make_var(coder,cblock,namelist,node_arg(node_arg(namelist,j),1),var_is_shadowed)
          call make_comm_call_args(coder,cblock,node)
          call code_val(coder,coder%vstack(expr_base+nvars))
          call code_val(coder,coder%vstack(nhd_base+(i+2)/3))
          call code_val(coder,coder%var(coder%par_base+lv_index))
          call code_val(coder,coder%var(coder%par_base+lv_here))
          call make_comm_sys_call(coder,cblock,node_arg(namelist,j),sym_nhd_var,4,1)
       enddo
    enddo
    
    ! Initialise extended tile variables
    k=1
    do i=1,nlist,3
       namelist=node_arg(list,i+1)
       do j=1,node_numargs(namelist),2
          call make_comm_call_args(coder,cblock,node)
          call code_val(coder,coder%var(nvar_base+k))
          call code_val(coder,coder%vstack(expr_base+k))
          call make_comm_sys_call(coder,cblock,node_arg(namelist,j),sym_set_nhd,2,0,assign=.true.)
          k=k+1
       enddo
    enddo
    
    join_base=coder%vtop

    ! join extended tile variables in each nhd into a list for each nhd
    k=1
    do i=1,nlist,3
       namelist=node_arg(list,i+1)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call code_val(coder,coder%var(k+nvar_base))
       call make_sys_call(coder,cblock,node,sym_nhd_join,1,1,&
            aflags=proc_run_shared+call_ignore_rules+call_inline_when_compiling)
       k=k+1
       do j=3,node_numargs(namelist),2
          call make_temp_var(coder,cblock,node)
          call swap_and_dup_code(coder)
          call code_val(coder,coder%var(k+nvar_base))
          call make_sys_call(coder,cblock,node,sym_nhd_join,2,1,&
               aflags=proc_run_shared+call_ignore_rules+call_inline_when_compiling)
          k=k+1
       enddo
    enddo
   
    ! Send intersecting boundaries
    do i=1,nlist/3
       call make_comm_call_args(coder,cblock,node)
       call code_val(coder,coder%vstack(join_base+i))
       call code_val(coder,coder%vstack(nhd_base+i))
       call make_comm_sys_call(coder,cblock,node,sym_send_nhd,2,0,assign=.true.)
    enddo

    call make_const(coder,cblock,node,coder%true)
    call define_sys_var(coder,cblock,node,sym_in_interior,var_is_shadowed)
    var=coder%top

    ! Check we are in an acceptable parallel context
    if(coder%par_state==par_state_outer) then
       call code_error(coder,node,&
            'Cannot have "nhd" statement outside of any parallel context')
    elseif(coder%par_state==par_state_nhd) then
       call code_error(coder,node,&
            'Cannot have "nhd" statement in the main body of another "nhd" statement')
    elseif(coder%par_state>=par_state_cond) then
       call code_error(coder,node,&
            'Cannot have "nhd" statement in a conditional context')
    endif
    save_par_state=coder%par_state
    coder%par_state=par_state_nhd

    ! Loop over interior and then boundaries
    call make_temp_var(coder,cblock,node)
    call dup_code(coder)
    call code_val(coder,coder%var(coder%par_base+lv_tile))
    call code_val(coder,coder%var(nvar_base))
    call make_sys_call(coder,cblock,node,sym_chunks,2,1,aflags=proc_run_shared)
    call define_sys_var(coder,cblock,node,sym_for,var_is_shadowed)
    iter=call_start(coder,cblock,list,.true.)
    
    if(pm_is_compiling) then
       call make_sys_var(coder,cblock,node,sym_while,&
            var_is_shadowed)
    else
       call code_null(coder)
    endif
    
    cblock2=make_cblock(coder,cblock,node,sym_nhd)
    if(block_base>0) then
       call make_comm_call_args(coder,cblock,node)
       call code_val(coder,coder%vstack(block_base))
       call make_comm_sys_call(coder,cblock,node_arg(node,2),sym_blocking,1,0)
    endif
    
    call make_sys_var(coder,cblock2,node,sym_subregion,var_is_shadowed)
    call var_set_par_depth(coder,top_code(coder),coder%par_depth-1)
    over_base=coder%top
    call make_comm_call_args(coder,cblock,node)
    call make_temp_var(coder,cblock,node)
    call var_set_par_depth(coder,top_code(coder),coder%par_depth-1)
    call dup_code(coder)
    call code_val(coder,coder%var(coder%par_base+lv_distr))
    call code_val(coder,coder%var(coder%par_base+lv_tile))
    call code_val(coder,coder%var(nvar_base))
    call code_val(coder,coder%var(iter+lv_idx))
    call make_sys_call(coder,cblock2,node,sym_get_chunk,4,1,aflags=proc_run_shared)
    call make_comm_sys_call(coder,cblock2,node_arg(node,4),sym_make_over,1,1)
    call make_do_over(coder,cblock2,node,node,node_arg(node,4),over_base)
    
    call code_val(coder,coder%var(var))

    ! If this is first (interior) chunk then recv and sync
    cblock3=make_cblock(coder,cblock2,node,sym_if)
    
    ! RECV intersecting boundaries
    do i=1,nlist/3
       call make_comm_call_args(coder,cblock,node)
       call code_val(coder,coder%vstack(join_base+i))
       call code_val(coder,coder%vstack(nhd_base+i))
       call make_comm_sys_call(coder,cblock3,node,sym_recv_nhd,2,0,assign=.true.)
    enddo

    ! Sync messages
    do i=1,nlist/3
       call code_val(coder,coder%vstack(join_base+i))
       call make_basic_sys_call(coder,cblock3,node,sym_sync_messages,1,0,coder%par_depth,0)
    enddo
 
    call make_const(coder,cblock3,node,coder%false)
    call make_var_assignment(coder,cblock3,node,coder%var(var))
    call close_cblock(coder,cblock3)
    call code_null(coder)
    call make_sp_call(coder,cblock2,node,sym_if_invar,3,0)

    call call_next(coder,cblock2,list,iter,.true.)
    
    if(pm_is_compiling) then
       call code_val(coder,coder%var(iter+lv_end))
       call close_cblock(coder,cblock2)
       call make_const(coder,cblock,node,coder%true)
       call init_var(coder,cblock,node,&
            coder%vstack(coder%vtop-3))
       call make_sp_call(coder,cblock,node,&
            sym_loop,3,0)
    else
       call code_val(coder,coder%var(iter+lv_end))
       call close_cblock(coder,cblock2)
       call make_sp_call(coder,cblock,node,&
            sym_each,3,0)
    endif

    coder%vtop=nhd_base
    coder%par_state=save_par_state
  contains
    include 'fisnull.inc'    
  end subroutine trav_nhd_stmt


  !**************************************************
  ! PARALLEL STATEMENTS
  !**************************************************
  
  !========================================================
  ! Traverse a for statement
  !========================================================
  recursive subroutine trav_for_stmt(coder,cblock,listp,list,base,stmt)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,listp,list,stmt
    integer,intent(in):: base
    type(pm_ptr):: var,vlist
    type(pm_ptr):: cblock2,cblock3,cblock4
    type(pm_ptr):: cblock_main,cblock_pre,cblock_post
    integer:: i,j,k,n,lbase,vbase
    integer:: nlist,iter,iter2,sym,rbase,wbase,name,flags,sindex,sbase
    integer:: slot1,slot2,outmode
    integer(pm_p)::flag
    type(pm_ptr):: vlhs,procs,sig,xvar,p
    integer:: save_par_base,save_over_base,save_par_state,save_run_mode
    type(pm_ptr):: save_loop_cblock
    logical:: iscomm,outer,is_conc,is_tile
    
    sym=node_sym(stmt)
    is_conc=node_get_num(stmt,node_args+2)==sym_conc
    is_tile=node_get_num(stmt,node_args+2)==sym_tile
    rbase=coder%vtop
    wbase=coder%wtop

    call trav_iter(coder,cblock,list,sym_hash,lbase,vbase,nlist)
   
    save_par_base=coder%par_base
    save_over_base=coder%over_base
    save_loop_cblock=coder%loop_cblock
    save_par_state=coder%par_state
    save_run_mode=coder%run_mode
    
    ! Start parallel loop call
    iter=code_par_scope_start(coder,cblock,stmt,coder%var(coder%top),&
         node_arg(stmt,1),&
         cblock_main,cblock_pre,sym_for_stmt,is_tile)
    
    slot1=coder%index
    
    ! Hide any where clauses (may need them later)
    if(base>=0) then
       call hide_vars(coder,base+1,coder%top)
    endif
   
    sindex=iter+lv_here
    
    ! Create array/domain element variables for this iteration
    sbase=coder%top
    do i=1,nlist,2
       var=node_arg(list,i)
       if(node_sym(var)==sym_define) then
          call make_var(coder,cblock_main,list,&
               node_arg(var,1),var_is_var)
       else
          call make_var(coder,cblock_main,list,&
               var,var_is_var)
       endif
    enddo

    ! Get array/domain elements for this iteration
    if(is_tile) then
       do i=1,nlist/2
          call code_val(coder,coder%var(sbase+i))
          call code_val(coder,coder%vstack(lbase+i))
          call make_basic_sys_call(coder,cblock_main,list,sym_pm_local,1,1,&
               coder%par_depth,call_ignore_rules)
       enddo
    else
       do i=1,nlist/2
          if(node_sym(node_arg(list,i*2-1))==sym_define) then
             call code_val(coder,coder%var(sbase+i))
             call var_set_par_depth(coder,top_code(coder),coder%par_depth-1)
             call code_val(coder,coder%vstack(lbase+i))
             call make_basic_sys_call(coder,cblock_main,list,sym_make_dollar,1,1,&
               coder%par_depth-1,call_ignore_rules)
          else
             call code_val(coder,coder%var(sbase+i))
             call code_val(coder,coder%var(iter+lv_distr))
             call code_val(coder,coder%var(coder%over_base))
             call code_val(coder,coder%var(iter+lv_here))
             call code_val(coder,coder%vstack(lbase+i))
             call code_val(coder,coder%var(iter+lv_index))
             call code_val(coder,coder%var(sindex))
             call make_comm_sys_call(coder,cblock_main,list,sym_for_get_element,3,1)
             call code_val(coder,coder%var(sbase+i))
             call make_sp_call(coder,cblock_main,list,sym_coherent,1,0)
          endif
       enddo
    endif
    
    ! Main body
    coder%par_state=par_state_for
    coder%run_mode=sym_complete
    call trav_open_stmt_list(coder,cblock_main,stmt,node_arg(stmt,4))
    
    if(base>=0) call reveal_vars(coder,base,vbase)
    
    ! Export variables that have changed
    if(.not.is_tile) then
       do i=1,nlist/2
          var=coder%var(sbase+i)
          if(cnode_flags_set(var,var_flags,var_is_changed)) then
             call code_val(coder,coder%var(iter+lv_distr))
             call code_val(coder,coder%var(coder%over_base))
             call code_val(coder,coder%var(iter+lv_here))
             outmode=trav_ref(coder,cblock_main,node_arg(list,i*2),&
                  node_arg(list,i*2),ref_ignores_rules)
             call code_val(coder,var)
             call code_val(coder,coder%var(iter+lv_index))
             call code_val(coder,coder%var(sindex))
             call make_comm_sys_call(coder,cblock_main,stmt,&
                  sym_for_set_element,4,0,assign=.true.,&
                  aflags=call_is_comm+call_inline_when_compiling)
          endif
       enddo
    endif
    slot2=coder%index
    
    ! Complete parallel for call
    call code_par_scope_end(coder,iter,list,cblock,&
         cblock_main,cblock_pre,save_par_base,slot1,slot2,is_conc,is_tile)
    
    coder%par_base=save_par_base
    coder%over_base=save_over_base
    coder%loop_cblock=save_loop_cblock
    coder%par_state=save_par_state
    coder%run_mode=save_run_mode
    
    if(base>=0) call hide_vars(coder,base,vbase)
    
    ! Clean up
    coder%vtop=rbase
    
    call pop_vars_to(coder,vbase)
    
  contains
    include 'fisnull.inc'
    include 'fisname.inc'
    include 'fname.inc'
    include 'ftiny.inc'
  end subroutine trav_for_stmt
  

  !========================================================
  ! Traverse par { }
  !========================================================
  recursive subroutine trav_par_stmt(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    type(pm_ptr):: let,clause,cblock_main,cblock_pre,cblock_post
    type(pm_ptr):: vlist,save_loop_cblock
    integer:: i,j,k,iter,istart,vstart,base,rbase,m
    integer:: save_par_base,save_over_base,save_par_state
    integer:: slot1,slot2
    integer:: name,flags
    
    vstart=coder%vtop
    save_loop_cblock=coder%loop_cblock
    save_par_base=coder%par_base
    save_over_base=coder%over_base
    save_par_state=coder%par_state
    
    base=coder%top

    if(node_numargs(node)==4) then
       call code_error(coder,node,'"par" statement has only one branch')
       coder%vtop=vstart
       return
    endif
    
    ! Variable sym_for
    call make_temp_var(coder,cblock,node)
    call dup_code(coder)
    call make_long_const(coder,cblock,node,&
         int((node_numargs(node)-2)/2,pm_ln))
    call make_sys_call(coder,cblock,node,sym_array,1,1)
    call define_sys_var(coder,cblock,node,sym_for,var_is_shadowed)

    ! Partition the domain across processors
    iter=code_par_scope_start(coder,cblock,node,coder%var(coder%top),&
         node_arg(node,1),cblock_main,cblock_pre,sym_also,.false.)

    slot1=coder%index
    
    do i=3,node_numargs(node),2
       call make_long_const(coder,cblock_main,node,int((i-2)/2,pm_ln))
       call make_definition(coder,cblock_main,node,node_arg(node,i),0)
    enddo

    coder%par_state=par_state_for
    
    ! statements before any branch
     call trav_open_stmt_list(coder,cblock_main,node,node_arg(node,2))

    coder%par_state=par_state_par
    
    ! branches
    call branch(cblock_main,3)

    slot2=coder%index
    
    ! Build parallel statement call
    call code_par_scope_end(coder,iter,node,cblock,cblock_main,&
         cblock_pre,&
         save_par_base,slot1,slot2,.false.,.false.)
    
    coder%vtop=vstart
    coder%loop_cblock=save_loop_cblock
    coder%par_base=save_par_base
    coder%over_base=save_over_base
    call pop_vars_to(coder,base)
  contains
    include 'fisnull.inc'
    include 'ftiny.inc'

    recursive subroutine branch(cblock,i)
      type(pm_ptr),intent(in):: cblock
      integer,intent(in):: i
      type(pm_ptr):: cblock4,cblock5
      type(pm_ptr):: prc_test_var
      
      ! Is this branch running on this processor?
      call make_temp_var(coder,cblock,node)
      prc_test_var=top_code(coder)
      call make_temp_var(coder,cblock,node)
      call dup_code(coder)
      call make_long_const(coder,cblock,node,int((i-2)/2,pm_ln))
      call make_sys_call(coder,cblock,node,sym_tuple,1,1)
      call code_val(coder,coder%var(iter+lv_here))
      call make_sys_call(coder,cblock,node,sym_eq,2,1)
      
      if(i/=cnode_numargs(node)-1) then   
         cblock4=make_cblock(coder,cblock,node,sym_also)
         call drop_code(coder)
      else
         cblock4=cblock
      endif
      
      ! do clause
      call trav_open_stmt_list(coder,cblock4,node,node_arg(node,i+1))

      if(i/=node_numargs(node)-1) then
         ! If statment (if running_here then ... endif)
         call close_cblock(coder,cblock4)
         call code_val(coder,prc_test_var)
         call code_val(coder,cblock4)
         cblock5=make_cblock(coder,cblock,node,sym_also)
         call branch(cblock5,i+2)
         call close_cblock(coder,cblock5)
         call make_sp_call(coder,cblock,node,sym_if,3,0)
      endif
    end subroutine branch
    
  end subroutine trav_par_stmt

  !========================================================
  ! traverse {  :  } expression
  !========================================================
  recursive subroutine trav_par_expr(coder,cblock,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer:: iter,slot1,slot2,lbase,vbase,nlist,base
    integer:: sbase,i,save_par_base,save_over_base,save_par_state
    type(pm_ptr):: rvar,var,cblock_main,cblock_pre,list,save_loop_cblock

    list=node_arg(node,2)
    call make_temp_var(coder,cblock,node)
    base=coder%vtop
    rvar=top_code(coder)
    
    call trav_iter(coder,cblock,list,sym_hash,lbase,vbase,nlist)

    save_par_base=coder%par_base
    save_loop_cblock=coder%loop_cblock
    save_par_state=coder%par_state
    save_over_base=coder%over_base
    
    iter=code_par_scope_start(coder,cblock,node,coder%var(coder%top),pm_null_obj,&
         cblock_main,cblock_pre,sym_for_stmt,.false.)
    slot1=coder%index

    ! Get array/domain elements
    sbase=coder%top
    do i=1,nlist,2
       var=node_arg(list,i)
       call make_var(coder,cblock_main,node,&
            var,0)
    enddo  
    do i=1,nlist/2
       call code_val(coder,coder%var(sbase+i))
       call code_val(coder,coder%var(iter+lv_distr))
       call code_val(coder,coder%var(coder%over_base))
       call code_val(coder,coder%var(iter+lv_here))
       call code_val(coder,coder%vstack(lbase+i))
       call code_val(coder,coder%var(iter+lv_index))
       call code_val(coder,coder%var(iter+lv_index))
       call make_comm_sys_call(coder,cblock_main,node,sym_for_get_element,3,1)
    enddo
    coder%par_state=par_state_for
    call code_val(coder,rvar)
    call make_comm_call_args(coder,cblock,node)
    if(pm_is_compiling) then
       call make_temp_var(coder,cblock_main,node)
       call dup_code(coder)
    endif

    
    call trav_expr(coder,cblock_main,node,node_arg(node,1))
 
    if(pm_is_compiling) then
       call  make_sys_call(coder,cblock_main,node,sym_dup,1,1,aflags=call_ignore_rules)
    endif
    call dup_code(coder)
    call code_num(coder,sym_chan)
    call make_basic_sp_call(coder,cblock_main,node,sym_set_mode,2,0,coder%par_depth)
    call make_comm_sys_call(coder,cblock_main,node,sym_make_array,1,1)
 
    slot2=coder%index
    call code_par_scope_end(coder,iter,node,cblock,cblock_main,&
         cblock_pre,save_par_base,slot1,slot2,.true.,.false.)
    coder%par_base=save_par_base
    coder%over_base=save_over_base
    coder%par_state=save_par_state
    coder%loop_cblock=save_loop_cblock
    call pop_vars_to(coder,vbase)
    coder%vtop=base
  end subroutine trav_par_expr


  !========================================================
  ! Over statement
  !========================================================
  recursive subroutine trav_over_stmt(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: base,save_par_state,save_over_base
    integer:: save_run_mode,save_run_flags
    type(pm_ptr):: p,stmt_list,cblock2
    logical:: ok
    
    if(coder%par_state==par_state_outer)then
       call code_error(coder,node,&
            'Cannot have "over" statement outside of a parallel context')
    elseif(coder%par_state/=par_state_for.and.coder%par_state/=par_state_over.and.&
         coder%par_state/=par_state_nhd) then
       call code_error(coder,node,&
            'Cannot have "over" statement in a conditional context')
    endif
    save_par_state=coder%par_state
    coder%par_state=par_state_for
    call make_sys_var(coder,cblock,node,sym_subregion,var_is_shadowed)
    call var_set_par_depth(coder,top_code(coder),coder%par_depth-1)
    base=coder%top
    call hide_vars(coder,base,base)
    call make_comm_call_args(coder,cblock,node)
    save_run_mode=coder%run_mode
    save_run_flags=coder%run_flags
    coder%run_mode=sym_shared
    coder%run_flags=proc_run_shared+proc_run_always
    call trav_xexpr(coder,cblock,node,node_arg(node,1))
    coder%run_mode=save_run_mode
    coder%run_flags=save_run_flags
    call make_comm_sys_call(coder,cblock,node,sym_make_over,1,1)
    call reveal_vars(coder,base,base)
    stmt_list=node_arg(node,2)    
    call make_do_over(coder,cblock,pnode,node,stmt_list,base)
    coder%par_state=save_par_state
  contains
    include 'fisnull.inc'
  end subroutine trav_over_stmt

  !=================================
  ! Body of an over statement
  ! (also used by nhd statements)
  !=================================
  recursive subroutine make_do_over(coder,cblock,pnode,node,statlist,base)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node,statlist
    integer,intent(in):: base
    integer:: save_par_state,save_over_base
    type(pm_ptr):: cblock2
    
    save_par_state=coder%par_state
    save_over_base=coder%over_base
    
    coder%over_base=base
    if(coder%par_state/=par_state_nhd) coder%par_state=par_state_over
    if(pm_is_compiling) then
       cblock2=make_cblock(coder,cblock,node,sym_using)
       call make_sys_var(coder,cblock2,node,sym_nested_loop,var_is_shadowed)
       call var_set_par_depth(coder,top_code(coder),coder%par_depth-1)
       call dup_code(coder)
       call code_val(coder,coder%var(base))
       call code_val(coder,coder%var(coder%par_base+lv_distr))
       call make_basic_sys_call(coder,cblock2,node,sym_do_over,2,1,&
            coder%par_depth-1,call_inline_when_compiling+proc_run_shared)
       call make_basic_sys_call(coder,cblock2,node,sym_nested_loop,1,0,&
            coder%par_depth-1,call_inline_when_compiling)
       call close_cblock(coder,cblock2)
       call trav_stmt_list(coder,cblock,node,statlist,sym_over)
       call make_sp_call(coder,cblock,node,sym_over,2,0)
    else
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call make_comm_call_args(coder,cblock,node)
       call code_val(coder,coder%var(base))
       call make_comm_sys_call(coder,cblock,node,sym_do_over,1,1)
       call trav_stmt_list(coder,cblock,node,statlist,sym_over)
       call code_null(coder)
       call make_sp_call(coder,cblock,node,sym_if,3,0)
    endif
    coder%par_state=save_par_state
    coder%over_base=save_over_base
  end subroutine make_do_over
  
  !========================================================
  ! Start a new parallel scope
  !========================================================
  function code_par_scope_start(coder,cblock,stmt,var,using,&
       cblock_main,cblock_pre,sym,is_tile) result(iter)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,stmt,var,using
    type(pm_ptr),intent(out):: cblock_main,cblock_pre
    integer,intent(in):: sym
    logical,intent(in):: is_tile
    integer:: iter
    integer:: n,i,vbase
    type(pm_ptr):: loop_cblock
    
    call make_sys_var(coder,cblock,stmt,sym_proc,var_is_shadowed)
    call make_sys_var(coder,cblock,stmt,sym_region,var_is_shadowed)
    call make_sys_var(coder,cblock,stmt,sym_in,var_is_shadowed)
    iter=coder%top
    call make_sys_var(coder,cblock,stmt,sym_pling,var_is_shadowed)
    call make_sys_var(coder,cblock,stmt,sym_hash,var_is_shadowed)
    call var_set_par_depth(coder,coder%var(iter+lv_numz),coder%par_depth+1)
    !@call cnode_set_num(coder%var(iter+lv_numz),var_par_depth,coder%par_depth+1)
    
    ! sym_in,region := partition(sym_proc,sym_for,using...)
    vbase=coder%vtop
    cblock_pre=make_cblock(coder,cblock,stmt,sym_using)
    call make_temp_var(coder,cblock_pre,stmt)
    call make_temp_var(coder,cblock_pre,stmt)
    call code_val(coder,coder%vstack(coder%vtop-1))
    call code_val(coder,coder%vstack(coder%vtop-1))
    call code_val(coder,coder%var(iter+lv_prc))
    call code_val(coder,var)
    
    if(.not.pm_fast_isnull(using)) then
       do i=1,num_using_clauses
          call trav_expr(coder,cblock_pre,using,node_arg(using,i))
       enddo
       call make_sys_call(coder,cblock_pre,using,&
            sym_partition,&
            2+num_using_clauses,2)
    else
       call make_sys_call(coder,cblock_pre,stmt,sym_partition,2,2)
    endif
    
    ! Init variables: tile, region
    call code_val(coder,coder%vstack(vbase+2))
    call init_var(coder,cblock_pre,stmt,coder%var(iter+lv_tile))
    call code_val(coder,coder%vstack(vbase+3))
    call init_var(coder,cblock_pre,stmt,coder%var(iter+lv_distr))
    call close_cblock(coder,cblock_pre)
    
    ! Variable sym_pling set to number of elements in domain
    call make_temp_var(coder,cblock_pre,stmt)
    call dup_code(coder)
    call code_val(coder,coder%var(iter+lv_distr))
    call make_sys_call(coder,cblock_pre,stmt,sym_num_elements,1,1)
    call init_var(coder,cblock_pre,stmt,coder%var(iter+lv_num))
    
    ! Alias the region variable
    call make_var_tab_entry(coder,sym_region,coder%var(iter+lv_distr))

    call make_const(coder,cblock,stmt,pm_null_obj)
    call define_sys_var(coder,cblock,stmt,sym_subregion,var_is_shadowed)
    coder%over_base =coder%top
    
    ! Outer code block (contains imports/exports)
    loop_cblock=make_cblock(coder,cblock,stmt,sym)
    call push_par_scope(coder,loop_cblock)
    coder%loop_cblock=loop_cblock
    coder%par_base=iter

    ! Inner block - contains statements
    cblock_main=make_cblock(coder,loop_cblock,stmt,sym_for)

    if(.not.is_tile) then
    
    ! Variable here_in_tile set to local iteration indices in this thread
    call make_sys_var(coder,cblock_main,stmt,sym_here_in_tile,var_is_shadowed)
    call code_val(coder,coder%var(iter+lv_distr))
    call code_val(coder,coder%var(iter+lv_numz))
    call make_basic_sys_call(coder,cblock_main,stmt,&
         sym_generate,2,1,coder%par_depth-1,&
         call_inline_when_compiling)
    call var_set_par_depth(coder,coder%var(iter+lv_index),coder%par_depth)

    ! Get element [here_in_tile] from tile to yield here
    call make_sys_var(coder,cblock_main,stmt,sym_here,&
         var_is_shadowed)
    call code_val(coder,coder%var(iter+lv_tile))
    call code_val(coder,coder%var(iter+lv_index))
    call make_sys_call(coder,cblock_main,stmt,sym_get_element,2,1)
    
    if(coder%top/=iter+7) then
       do i=iter,coder%top
          write(*,*) pm_name_as_string(coder%context,coder%stack(i))
       enddo
       write(*,*) coder%top,iter,coder%top-iter
       call pm_panic('push_par_scope')
    endif

    endif
    
    ! Clean up
    coder%vtop=vbase

  contains

    include 'fname.inc'
    include 'fisnull.inc'
    
  end function code_par_scope_start

  !============================================================================
  ! Complete current parallel scope
  ! - iter, cblock_main -- as returned by code_par_scope_start
  ! - cblock_post -- must be allocated as a code block if not "conc"
  ! - old_par_base -- no longer used
  ! - slot1,slot2 -- range of indices (coder%index) covered by this statement
  !============================================================================
  subroutine code_par_scope_end(coder,iter,node,&
       cblock,cblock_main,cblock_pre,old_par_base,slot1,slot2,is_conc,is_tile)
    type(code_state),intent(inout):: coder
    integer,intent(in):: iter
    type(pm_ptr),intent(in):: node,cblock,cblock_main,cblock_pre
    integer,intent(in):: old_par_base,slot1,slot2
    logical,intent(in):: is_conc,is_tile
    type(pm_ptr):: p

    ! Inner call to for
    call close_cblock(coder,cblock_main)
    call code_val(coder,cblock_main)
    call make_sp_call(coder,coder%loop_cblock,node,sym_for,1,0)

    call code_val(coder,coder%var(coder%par_base+lv_distr))
    call make_sys_call(coder,coder%loop_cblock,node,sym_pop_node,1,0,aflags=call_ignore_rules)
    
    ! Close loop cblock
    call close_cblock(coder,coder%loop_cblock)
    call pop_par_scope(coder,coder%loop_cblock,node)

    ! Build par-loop call
    ! #, sym_prc  = main_block,pre_block,post_block, $, is_conc, slots
    call code_val(coder,coder%var(iter+lv_numz))
    call code_val(coder,coder%var(iter+lv_prc))
   
    call code_val(coder,coder%loop_cblock)
    call code_val(coder,cblock_pre)
    call code_val(coder,pm_null_obj) !!!@ Legacy - need to remove
    call code_val(coder,coder%var(iter+lv_num))
    if(is_conc) then
       call make_const(coder,cblock,node,pm_null_obj)
    else
       call make_const(coder,cblock,node,coder%true)
    endif
    
    ! Two-integer entry giving range of slots used by statement
    p=pm_fast_newnc(coder%context,pm_int,2)
    call make_const(coder,cblock,node,p)
    p%data%i(p%offset)=slot1
    p%data%i(p%offset+1)=slot2

    ! Code parallel statement call
    call make_sp_call(coder,cblock,node,&
         merge(sym_tile,sym_for_stmt,is_tile),6,2)
     
    ! Adjust par_depth of result
    call var_set_par_depth(coder,coder%var(iter+lv_numz),coder%par_depth+1)

  contains
    include 'fnewnc.inc'
    include 'fisnull.inc'
  end subroutine code_par_scope_end
  
  !========================================================
  ! Push a parallel scope level
  !========================================================
  subroutine push_par_scope(coder,cblock)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    integer:: depth, kk
    depth=coder%par_depth
    if(depth==max_par_depth) &
         call pm_panic('Program too complex (nested loops)')
    depth=depth+1
    coder%imports(depth)=&
         pm_dict_new(coder%context,32_pm_ln)
    coder%import_cblock(depth)=cblock
    coder%par_depth=depth
  end subroutine push_par_scope

  !========================================================
  ! Pop down parallel scope level 
  !========================================================
  subroutine pop_par_scope(coder,cblock,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer:: depth
    depth=coder%par_depth
    coder%imports(depth)=pm_null_obj
    coder%par_depth=coder%par_depth-1
  contains
    include 'fisnull.inc'
  end subroutine pop_par_scope

  !========================================================
  ! Import argument list for a call
  ! - returns parallel depth of call
  ! Also returns export information for return values
  ! at vstack locations base..vtop
  !========================================================
  subroutine import_args(coder,cblock,node,narg,nret,nkey,amps,flags,base) 
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,amps
    integer,intent(in)::narg,nret,nkey,flags
    integer,intent(out):: base
    integer:: depth
    integer:: i,j,top,totsize,nextra
    logical:: isamp,export,iscomm
    type(pm_ptr):: amp

    top=coder%vtop

    iscomm=iand(flags,call_is_comm)/=0
    if(iscomm) then
       depth=coder%par_depth-1
       export=.false.
    elseif(iand(flags,proc_run_shared+proc_run_local)/=0) then
       depth=coder%par_depth-1
       export=.true.
    else
       depth=coder%par_depth
       export=.false.
    endif

    if(pm_fast_isnull(amps)) then
       do i=top+1-narg-nkey,top
          call import_arg(i,.false.)
       enddo
    else
       amp=pm_name_val(coder%context,int(amps%offset))
       do i=top+1-narg-nkey,top-narg
          call import_arg(i,.false.)
       enddo
       j=0
       do i=top+1-narg,top
          isamp=amp%data%i(amp%offset+j)==i-(top-narg)
          if(isamp.and.j<pm_fast_esize(amp)) j=j+1
          call import_arg(i,isamp)
       enddo
    endif

    if(export) then
       do i=top+1-narg-nkey-nret,top-narg-nkey
          call var_set_par_depth(coder,coder%vstack(i),depth)
       enddo
    endif
    
  contains
    include 'fvkind.inc'
    include 'fisnull.inc'
    include 'fesize.inc'
    
    subroutine import_arg(index,modify)
      integer,intent(in):: index
      logical,intent(in):: modify
      type(pm_ptr):: var,nvar
      integer:: vdepth
      var=coder%vstack(index)
      if(pm_fast_vkind(var)/=pm_pointer) return
      if(cnode_get_kind(var)/=cnode_is_var) return
      !write(*,*) 'IMPORT>',trim(pm_name_as_string(coder%context,cnode_get_name(var,var_name)))
      if(iscomm.and.index<=top+2-narg) return
      if(cnode_flags_set(var,var_flags,var_is_no_import_export)) then
         coder%vstack(index)=cnode_get(var,var_extra_info)
         return
      endif
      vdepth=par_depth(coder,var)
      if(modify.and.vdepth<coder%par_depth-1) then
         call code_error(coder,node,&
              'Cannot modify variable from outside of parallel scope enclosing current parallel scope: ',&
              cnode_get(var,var_name))
         nvar=var
      elseif(modify.and.depth/=vdepth.and.&
           (iand(flags,call_ignore_rules)==0.and.vdepth<depth).and..not.iscomm) then
         call make_temp_var(coder,cblock,node)
         call dup_code(coder)
         call code_val(coder,var)
         call make_basic_sp_call(coder,cblock,node,sym_amp_error,&
              1,1,coder%par_depth)
         call var_set_par_depth(coder,top_code(coder),depth)
         nvar=pop_code(coder)
      elseif(vdepth<depth) then
         nvar=import_to_par_scope(coder,cblock,node,var,&
              depth,modify)
      elseif(vdepth>depth.and.export) then
         call make_temp_var(coder,cblock,node)
         call dup_code(coder)
         call cnode_set_flags(top_code(coder),var_flags,var_is_imported)
         call code_val(coder,var)
         call make_basic_sp_call(coder,cblock,node,sym_export_as_new,&
              1,1,coder%par_depth)
         call var_set_par_depth(coder,top_code(coder),depth)
         nvar=pop_code(coder)
      else
         nvar=var
      endif
      coder%vstack(index)=nvar
    end subroutine import_arg
    
  end subroutine import_args


  !========================================================
  ! Import a variable into a parallel scope at given depth
  !========================================================
  function import_to_par_scope(coder,cblock,node,var,depth,modify) result(ivar)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,var
    integer,intent(in):: depth
    logical,intent(in):: modify
    type(pm_ptr):: ivar
    type(pm_ptr):: jvar,kvar,iblock
    integer:: i,j,vdepth,vcdepth,name
    ivar=var
    if(pm_fast_vkind(var)/=pm_pointer) return
    if(cnode_get_kind(var)/=cnode_is_var) return
    if(cnode_flags_set(var,var_flags,var_is_no_import_export)) then
       ivar=cnode_get(var,var_extra_info)
       return
    endif
    vdepth=cnode_get_num(var,var_par_depth)+coder%proc_par_depth
    vcdepth=cnode_get_num(var,var_create_depth)+coder%proc_par_depth
    if(debug_codegen) then
       write(*,*) 'IMPORT TO:',&
            trim(pm_name_as_string(coder%context,&
            cnode_get_num(var,var_name))),&
            depth,vdepth,coder%par_depth,modify
    endif
    if(depth==0) then
       ivar=var
    elseif(vdepth>depth) then
       ivar=var
    elseif(vdepth==depth) then
       ivar=var
    elseif(modify.and.vdepth<coder%par_depth-1) then
       call code_error(coder,node,&
            'Cannot modify a variable outside of the enclosing scope')
       ivar=var
    else
       if(depth==coder%par_depth.and.cnode_flags_set(var,var_flags,var_is_var)) then
          jvar=pm_null_obj
       else
          jvar=import_cached(coder,node,var,depth)
       endif
       if(pm_fast_isnull(jvar)) then
          do i=depth-1,vdepth+1,-1
             jvar=import_cached(coder,node,var,i)
             if(.not.pm_fast_isnull(jvar)) exit
          enddo
          if(pm_fast_isnull(jvar)) jvar=var
          do i=cnode_get_num(jvar,var_par_depth)+1+coder%proc_par_depth,depth
             j=i
             if(j<vcdepth+1) j=vcdepth+1
             if(j>=coder%par_depth) then
                iblock=cblock
             else
                iblock=coder%import_cblock(j)
             endif
             name=cnode_get_num(jvar,var_name)
             if(name/=0) name=pm_name2(coder%context,sym_gt,name)
             call make_sys_var(coder,iblock,node,&
                  name,var_is_shadowed+var_is_imported)
             kvar=top_code(coder)
             call cnode_set_num(kvar,var_par_depth,i-coder%proc_par_depth)
             call code_val(coder,jvar)
             
             if(cnode_flags_set(jvar,var_flags,var_is_varg)) then
                call cnode_set_flags(kvar,var_flags,var_is_varg)
                call make_basic_sp_call(coder,iblock,node,&
                     sym_import_varg,1,1,i+1)
             else
                call make_basic_sp_call(coder,iblock,node,&
                     sym_import_val,1,1,i+1)
             endif
             if(debug_codegen) then
                write(*,*) 'IMPORT VAL> ',&
                     trim(pm_name_as_string(coder%context,&
                     cnode_get_num(var,var_name))),i
             endif
             ! Note cannot cache import to current level
             ! - shared variable may change
             ! - import may be in code that does not run
             !    if 'false or similar
             if(i<coder%par_depth) then
                call cache_import(coder,node,var,i,kvar)
             endif
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
    include 'fvkind.inc'
    include 'fnewnc.inc'
    
  end function import_to_par_scope

  !============================================================
  ! Retrieve a cached import (if one exists)
  !============================================================
  function import_cached(coder,node,var,depth) result(ivar)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,var
    integer,intent(in)::depth
    type(pm_ptr):: ivar
    integer:: vindex
    type(pm_ptr):: entry
    vindex=cnode_get_num(var,var_index)
    ivar=pm_dict_lookup(coder%context,&
         coder%imports(depth),&
         pm_fast_tinyint(coder%context,vindex))
  contains
    include 'ftiny.inc'
    include 'fisnull.inc'
  end function import_cached

  !==========================================================
  ! Cache an import
  !==========================================================
  subroutine cache_import(coder,node,var,depth,ivar)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,var,ivar
    integer,intent(in):: depth
    integer:: vindex
    logical:: ok
    vindex=cnode_get_num(var,var_index)
    call pm_dict_set(coder%context,&
           coder%imports(depth),&
           pm_fast_tinyint(coder%context,vindex),&
           ivar,.true.,.false.,ok)
    if(.not.ok) call pm_panic('Cache import')
  contains
    include 'ftiny.inc'
    include 'fnewnc.inc'
  end subroutine cache_import

  subroutine check_par_nesting(coder,list_head,node,cond_is_ok)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: list_head,node
    logical,intent(in):: cond_is_ok
    type(pm_ptr):: list
    integer:: i
    select case(coder%par_state)
    case(par_state_for,par_state_loop,par_state_masked,par_state_over,&
         par_state_labelled,par_state_sync)
       continue
    case(par_state_outer)
       call code_error(coder,node,&
            'Cannot have communicating operation outside of any parallel statement')
       return
    case(par_state_nhd)
       call code_error(coder,node,&
            'Cannot have communicating operation in the main body of a "nhd" statement')
       return
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
    
    list=list_head
    i=cnode_get_num(list,cblock_sym)
    do
       call cnode_set(coder,list,cblock_last_loop_call,&
            cnode_get(list,cblock_last_call))
       if(i==sym_for.or.i==sym_for_stmt.or.i==sym_nhd) then
          exit
       endif
       if(cnode_flags_set(list,cblock_flags,cblock_is_comm)) exit
       call cnode_set_flags(list,cblock_flags,cblock_is_comm)

 
       list=cnode_get(list,cblock_parent)
       if(pm_fast_isnull(list)) then
          call code_error(coder,node,&
               'communicating operation outside of "for"/"par" statement')
          return
       endif
       i=cnode_get_num(list,cblock_sym)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine check_par_nesting


  !*****************************************************
  ! ASSIGNMENTS AND VARIABLE DEFINITIONS
  !*****************************************************

  
  !========================================================
  ! Traverse assignment or var/const definition
  !========================================================
  recursive subroutine trav_assign_define(coder,cblock,pnode,node,issync)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    logical,intent(in),optional:: issync
    type(pm_ptr):: lhs,lhsi,op
    integer:: i,j,n,sym,symi,base,base2,save_par_state
    base=coder%vtop
    lhs=node_arg(node,1)
    sym=node_sym(lhs)
    n=node_numargs(lhs)
    if(sym/=sym_assign) n=n-1
    call trav_rhs(coder,cblock,node,node_arg(node,2),n)
    if(present(issync)) then
       save_par_state=coder%par_state
       coder%par_state=par_state_sync
    endif
    lhs=node_arg(node,1)
    select case(node_sym(lhs))
    case(sym_var,sym_const)
       do i=n,1,-1
          lhsi=node_arg(lhs,i)
          symi=node_sym(lhsi)
          select case(symi)
          case(sym_var,sym_const)
             call trav_cast(coder,cblock,node,node_arg(lhsi,2),node_sym(lhsi))
             call make_definition(coder,cblock,lhsi,node_arg(lhsi,1),&
                  merge(0,var_is_var,symi==sym_const))
          case(sym_assign)
             call make_assignment(coder,cblock,lhsi,node_arg(lhsi,1))
          case(sym_lt)
             op=node_arg(lhsi,2)
             call make_op_assignment(coder,cblock,lhsi,node_arg(lhsi,1),op)
          case default
             call trav_cast(coder,cblock,node,node_arg(lhs,n+1),node_sym(lhs))
             call make_definition(coder,cblock,lhs,lhsi,&
                  merge(0,var_is_var,sym==sym_const))
          end select
       enddo
    case(sym_assign)
       do i=n,1,-1
          call make_assignment(coder,cblock,lhs,node_arg(lhs,i))
       enddo
    case(sym_lt)
       op=node_arg(lhs,n+1)
       do i=n,1,-1
          call make_op_assignment(coder,cblock,lhs,node_arg(lhs,i),op)
       enddo
    end select
    coder%vtop=base
    if(present(issync)) coder%par_state=save_par_state
  contains
    include 'fisnull.inc'
  end subroutine trav_assign_define

  !========================================================
  ! Traverse right hand side of assignment or definition
  !========================================================
  subroutine trav_rhs(coder,cblock,node,rhs,n)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,rhs
    integer,intent(in):: n
    integer:: i,rsym,base
    rsym=node_sym(rhs)
    base=coder%vtop
    if(rsym==sym_define) then
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
  ! Assign expression on top of stack to lhs in node
  !========================================================
  recursive subroutine make_assignment(coder,cblock,pnode,node)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: n,i,sym,flags,mode,depth,outmode
    type(pm_ptr):: v,w
    logical:: outer,shared
    if(node_sym(node)==sym_underscore) then
       call drop_code(coder)
       return
    elseif(pm_fast_isname(node)) then
       call trav_ref_to_var(coder,cblock,pnode,node,0)
       call assign_call(outer,&
            cnode_flags_clear(top_code(coder),var_flags,var_is_ref),.false.)
    else
       sym=node_sym(node)
       select case(sym)
       case(sym_sub,sym_dot,sym_get_dot,sym_at)
          outmode=trav_ref(coder,cblock,pnode,node,0)
          call assign_call(outer,.false.,iand(outmode,ref_has_at)/=0)
       case(sym_name)
          call trav_ref_to_var(coder,cblock,node,node_arg(node,1),0)
          call assign_call(outer,&
               cnode_flags_clear(top_code(coder),var_flags,var_is_ref),&
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

    subroutine assign_call(outer,simple,has_pling)
      logical,intent(in):: outer,simple
      logical,intent(in):: has_pling
      type(pm_ptr):: v,w
      if(coder%par_state/=par_state_sync) then
         call swap_code(coder)
         call make_assign_call(coder,cblock,pnode,&
              merge(sym_assign_var,sym_assignment,simple),&
              2,0,aflags=call_is_assign_call)
      else
         v=pop_code(coder)
         w=pop_code(coder)
         call make_comm_call_args(coder,cblock,pnode)
         call code_val(coder,v)
         call code_val(coder,w)
         call make_static_bool_const(coder,cblock,pnode,has_pling)
         call check_par_nesting(coder,cblock,pnode,.true.)
         call make_comm_sys_call(coder,cblock,pnode,sym_assignment,&
              3,0,assign=.true.)
      endif
    end subroutine assign_call
    
  end subroutine make_assignment

  !========================================================
  ! Assign expression on top of stack to lhs in node
  !========================================================
  recursive subroutine make_op_assignment(coder,cblock,pnode,node,op)
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
    if(coder%par_state==par_state_sync) then
       v=pop_code(coder)
       w=pop_code(coder)
       call make_comm_call_args(coder,cblock,pnode)
       call code_val(coder,v)
       call code_val(coder,w)
       if(node_sym(op)==sym_proc.and.node_sym(node_arg(op,1))==sym_minus) then
          call make_temp_var(coder,cblock,pnode)
          call swap_and_dup_code(coder)
          call make_sys_call(coder,cblock,pnode,sym_minus,1,1)
       endif
       call trav_expr(coder,cblock,pnode,op)
       call make_static_bool_const(coder,cblock,pnode,&
            iand(outmode,ref_has_at)/=0)
       call check_par_nesting(coder,cblock,pnode,.true.)
       call make_comm_sys_call(coder,cblock,pnode,&
            sym_assignment,4,0,assign=.true.)
       call check_par_nesting(coder,cblock,pnode,.false.)
    else
       call swap_code(coder)
       if(node_sym(op)==sym_proc.and.node_sym(node_arg(op,1))==sym_minus) then
          call make_temp_var(coder,cblock,pnode)
          call swap_and_dup_code(coder)
          call make_sys_call(coder,cblock,pnode,sym_minus,1,1)
       endif
       call trav_expr(coder,cblock,pnode,op)
       call make_assign_call(coder,cblock,pnode,sym_assignment,3,0)
    endif
  end subroutine make_op_assignment

  !===============================================================
  ! Traverse var or const definition with no immediate definition
  !===============================================================
  subroutine trav_var_no_init(coder,cblock,pnode,node)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: n,flags,i
    type(pm_ptr):: tnode
    n=node_numargs(node)-1
    tnode=node_arg(node,n+1)
    flags=var_is_not_inited
    if(node_sym(node)==sym_var) flags=ior(flags,var_is_var)
    do i=1,n
       call make_var(coder,cblock,node,node_arg(node,i),flags,tnode)
       call cache_var_init(coder,cblock,node,top_code(coder))
       call drop_code(coder)
    enddo
  end subroutine trav_var_no_init

  !==================================================================
  ! Initialise a variable in a nested block (relative to definition)
  !==================================================================
  subroutine var_init_in_cblock(coder,cblock,node,var,flags)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,var
    integer,intent(in):: flags
    type(pm_ptr):: p,def_block,tnode,ivar
    logical:: has_if,in_par

    def_block=cnode_get(var,var_parent)
    has_if=.false.
    in_par=.false.
    p=cblock
    do while(.not.p==def_block)
       select case(cnode_get_num(p,cblock_sym))
       case(sym_for)
          if(in_par) then
             call code_error(coder,node,&
                  'Cannot intialise a "var" or "const" inside multiply nested'//&
                  ' parallel statements')
             call cnode_error(coder,var,&
                  'Variable definition incorrectly intialised by the above')
             call cnode_error(coder,p,&
                  'Parallel statement preventing the above initialisation')
          else
             in_par=.true.
          endif
       case(sym_if,sym_if_invar,sym_switch)
          if(var_init_cached(coder,p,var)) then
             if(.not.has_if) then
                call code_error(coder,node,&
                     'Cannot initialise variable twice: ',&
                     cnode_get(var,var_name))
             endif
             exit
          else
             call cache_var_init(coder,p,node,var)
             has_if=.true.
          endif
       case(sym_while,sym_until,sym_each)
          call code_error(coder,node,&
               'Cannot initialise a variable defined outside '//&
               'of a sequential loop within that loop: ',&
               cnode_get(var,var_name))
       case(sym_any)
          call code_error(coder,node,&
               'Cannot initialise a variable defined outside '//&
               'of an "any" statement within that statement: ',&
               cnode_get(var,var_name))
       end select
       p=cnode_get(p,cblock_parent)
    enddo
    if(.not.has_if) then
       call cnode_clear_flags(var,var_flags,var_is_not_inited)
    endif
    if(cnode_flags_set(var,var_flags,var_is_var).neqv.&
         iand(flags,var_is_var)/=0) then
       if(iand(flags,var_is_var)==0) then
          call code_error(coder,node,'Cannot initialise "var" using "=" or "const =": ',&
               cnode_get(var,var_name))
       else
          call code_error(coder,node,'Cannot initialise constant using "var =": ',&
               cnode_get(var,var_name))
       endif
       call cnode_error(coder,var,'Definition conflicting with above')
    endif
    if(in_par) then
       if(coder%par_state>=par_state_cond.and.coder%par_state<=par_state_par) then
          call code_error(coder,node,&
               'Initialisation of shared "var" or "const" must be labelled in'//&
               ' a conditional context: ',cnode_get(var,var_name))
       endif
       tnode=cnode_get(var,var_extra_info)
!!$       if(node_sym(tnode)==sym_mode) then
!!$          if(node_num_arg(tnode,2)/=sym_complete.and.&
!!$               node_num_arg(tnode,2)/=sym_partial) then
!!$             call code_error(coder,node,&
!!$                  'Cannot initialise non-"private" "var" or "const" inside a nested '//&
!!$                  'parallel statement',&
!!$                  cnode_get(var,var_name))
!!$          endif
!!$          tnode=node_arg(tnode,1)
!!$       endif
       call code_check_invar(coder,cblock,node,top_code(coder))
       call trav_cast(coder,cblock,node,tnode,&
            merge(sym_var,sym_const,cnode_flags_set(var,var_flags,var_is_var)))
       call init_var(coder,cblock,node,ivar)
    else
       call trav_cast(coder,cblock,node,&
            cnode_get(var,var_extra_info),&
             merge(sym_var,sym_const,cnode_flags_set(var,var_flags,var_is_var)))
       call init_var(coder,cblock,node,var)
    endif
  contains
    include 'fisnull.inc'
    include 'fvkind.inc'
  end subroutine var_init_in_cblock

  !========================================================
  ! Check 2 cblocks (or null) on the top of the
  ! vstack to see if initialisations match
  !========================================================
  subroutine resolve_if_inits(coder,node)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    type(pm_ptr):: cblock1,cblock2,cache1,cache2
    logical:: empty1,empty2
    integer:: i
    cblock1=coder%vstack(coder%vtop-1)
    cblock2=coder%vstack(coder%vtop)
    cache1=cnode_get(cblock1,cblock_var_inits)
    if(pm_fast_isnull(cblock2)) then
       cache2=pm_null_obj
    else
       cache2=cnode_get(cblock2,cblock_var_inits)
    endif
    empty1=pm_fast_isnull(cache1)
    empty2=pm_fast_isnull(cache2)
    if(empty1.and.empty2) then
       return
    elseif(.not.(empty1.or.empty2)) then
       if(pm_dict_size(coder%context,cache1)>&
            pm_dict_size(coder%context,cache2)) then
          call compare(cache1,cache2)
       else
          call compare(cache2,cache1)
       endif
    else
       if(empty1) cache1=cache2
       do i=1,pm_dict_size(coder%context,cache1)
          call error(cache1,i)
       enddo
    endif
  contains
    include 'fisnull.inc'

    ! Compare larger cache1 to cache2
    subroutine compare(cache1,cache2)
      type(pm_ptr),intent(in):: cache1,cache2
      type(pm_ptr):: var,dblock,p
      integer:: i
      
      outer: do i=1,pm_dict_size(coder%context,cache1)
         if(pm_fast_isnull(pm_dict_lookup(coder%context,cache2,&
              pm_dict_key(coder%context,cache1,int(i,pm_ln))))) then
            call error(cache1,i)
         endif
         ! Check if variable is now completely initialised
         ! (true if not still nested in any conditional statement)
         var=pm_dict_val(coder%context,cache1,int(i,pm_ln))
         dblock=cnode_get(var,var_parent)
         p=cnode_get(cblock1,cblock_parent)
         do while(.not.p==dblock)
            select case(cnode_get_num(p,cblock_sym))
            case(sym_if,sym_if_invar)
               cycle outer
            end select
            p=cnode_get(p,cblock_parent)
         enddo
         call cnode_clear_flags(var,var_flags,var_is_not_inited)
      enddo outer
    end subroutine compare
    
    subroutine error(cache,j)
      type(pm_ptr):: cache
      integer:: j
      call code_error(coder,node,&
           'Variable is only intialised in one branch of this if statement: ',&
           cnode_get(pm_dict_val(coder%context,cache,int(j,pm_ln)),var_name))
    end subroutine error
    
  end subroutine resolve_if_inits

  !====================================================================
  ! Check if variable initialisation is in the cache for a given block
  !====================================================================
  function var_init_cached(coder,cblock,var) result(iscached)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,var
    logical:: iscached
    type(pm_ptr):: index,cache
    index=cnode_get(var,var_index)
    cache=cnode_get(cblock,cblock_var_inits)
    if(pm_fast_isnull(cache)) then
       iscached=.false.
    else
       iscached=.not.pm_fast_isnull(pm_dict_lookup(coder%context,&
            cache,index))
    endif
  contains
    include 'fisnull.inc'
  end function var_init_cached

  !========================================================
  ! Add a variable initialisation to the cache for a block
  !========================================================
  subroutine cache_var_init(coder,cblock,node,var)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,var
    type(pm_ptr):: index,cache,tnode2
    logical:: ok
    integer(pm_ln):: i
    index=cnode_get(var,var_index)
    cache=cnode_get(cblock,cblock_var_inits)
    if(pm_fast_isnull(cache)) then
       cache=pm_dict_new(coder%context,8_pm_ln)
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_var_inits,pm_ln),&
            cache)
    endif
    call pm_dict_set(coder%context,cache,index,var,.true.,.false.,ok)
    if(.not.ok) then
       call code_error(coder,node,&
            'Cannot re-initialise variable: ',&
            cnode_get(var,var_name))
    endif
  contains
    include 'fisnull.inc'
    include 'ftiny.inc'
  end subroutine cache_var_init

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
       var=find_var(coder,name)
       if(.not.pm_fast_isnull(var)) then
          if(cnode_flags_set(var,var_flags,var_is_not_inited)) then
             call var_init_in_cblock(coder,cblock,node,var,flags)
             return
          endif
       endif
       call make_var(coder,cblock,pnode,name,flags)
       var=top_code(coder)
       call swap_code(coder)
       call make_sys_call(coder,cblock,pnode,&
            sym_dup,1,1,aflags=coder%run_flags)
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

  !========================================================
  ! Reference to a variable
  !========================================================
  subroutine trav_ref_to_var(coder,cblock,pnode,name,mode)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,name
    integer,intent(in):: mode
    type(pm_ptr):: var
    integer:: depth,flags
    var=find_var(coder,name)
    if(pm_fast_isnull(var)) then
       call code_error(coder,pnode,&
            'Variable has not been defined: ',name)
       call make_temp_var(coder,cblock,pnode)
       return
    endif
    flags=cnode_get_num(var,var_flags)
    if(iand(flags,var_is_aliased)/=0) then
       coder%aliased=.true.
       var=cnode_get(var,var_extra_info)
    endif
    if(.not.iand(mode,ref_is_val)/=0) then
       if(iand(flags,var_is_var)==0) then
          call code_error(coder,pnode,&
               'Cannot assign to constant: ',name)
       else
          call cnode_set_flags(var,var_flags,var_is_changed)
       endif
    endif
    if(iand(flags,var_is_not_inited)/=0) then
       call code_error(coder,pnode,&
            'Cannot assign to a non-initialised variable: ',name)
    endif
    if(iand(mode,ref_is_val)/=0) then
       var=import_to_par_scope(coder,cblock,pnode,var,coder%par_depth,&
            .false.)
    endif
    call code_val(coder,var)
    if(iand(mode,ref_is_val)==0) then
       if(coder%par_state>par_state_outer.and.coder%par_state/=par_state_sync&
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
               sym_assign,1,0,coder%par_depth)
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
    integer:: aflags,acall
    type(pm_ptr):: p,q
    outer=.false.
    if(pm_fast_isname(node)) then
       call trav_ref_to_var(coder,cblock,pnode,node,mode)
       outmode=0
    else if(pm_fast_vkind(node)==pm_pointer) then
       sym=node_sym(node) 
       select case(sym)
       case(sym_sub)
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
             acall=sym_make_subref
          else
             if(iand(mode,ref_is_amp)/=0) then
                acall=sym_make_sublhs_amp
                !call code_error(coder,node,'Cannot have "[]" in "&" argument')
             else
                acall=sym_make_sublhs
             endif
          endif
          if(coder%par_state==par_state_sync.or.&
               coder%par_state>par_state_outer.and.iand(mode,ref_is_val)/=0) then
             call make_comm_call_args(coder,cblock,pnode)
             call code_val(coder,p)
             call code_val(coder,q)
             call make_comm_sys_call(coder,cblock,node,acall,2,1,&
                  aflags=aflags)
             call check_par_nesting(coder,cblock,node,.true.)
          else
             call code_val(coder,p)
             call code_val(coder,q)
             call make_sys_call(coder,cblock,node,acall,2,1,aflags=ior(aflags,call_ignore_rules))
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
       case(sym_caret)
          save_run_flags=coder%run_flags
          coder%run_flags=ior(coder%run_flags,call_inline_when_compiling)
          call trav_expr(coder,cblock,pnode,node_arg(node,1))
          coder%run_flags=save_run_flags
          outmode=0
       case(sym_at)
          if(iand(mode,ref_is_val+ref_ignores_rules)==0.and.coder%par_state/=par_state_sync) then
             call code_error(coder,node,&
                  'Cannot change value of "@" expression outside of a "sync" statement') 
          endif
          call check_par_nesting(coder,cblock,node,.false.)
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          call make_comm_call_args(coder,cblock,node)
          outmode=ior(trav_ref(coder,cblock,node,node_arg(node,1),mode),&
               ref_has_at)
          call make_comm_sys_call(coder,cblock,node,sym_make_array,1,1,aflags=proc_run_complete)
          call var_set_par_depth(coder,top_code(coder),coder%par_depth-1)
          call dup_code(coder)
          call code_num(coder,sym_shared)
          call make_basic_sp_call(coder,cblock,node,sym_set_mode,2,0,coder%par_depth)
          call check_par_nesting(coder,cblock,node,.true.)
       case(sym_name)
          call trav_ref_to_var(coder,cblock,pnode,node_arg(node,1),mode)
          outmode=0
       case default
          if(iand(mode,ref_is_val)==0) then
             call code_error(coder,pnode,&
                  'Cannot indirectly assign to expression - value is updated')
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
  subroutine trav_alias_checks(coder,cblock,list,amp,j,argbase)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,list,amp
    integer,intent(in):: j,argbase
    integer:: i,k,base1,base2
    logical:: finished
    type(pm_ptr):: p,name,name2,var
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
             if(match_ref_names(coder,cblock,p,base1,base2,j,i,list)) then
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
    case(sym_at)
       finished=get_ref_pattern(coder,node_arg(node,1),name)
       finished=.true.
    case(sym_name)
       name=node_arg(node,1)
       call code_val(coder,name)
    case default
       finished=.true.
    end select
  contains
    include 'fname.inc'
  end function  get_ref_pattern

  !=======================================================================
  ! Match reference patterns in coder%vstack(base1+1..base2) and
  ! coder%vstack(base2+1..coder%vtop) coding runtime checks when needed
  !=======================================================================
  subroutine match_ref_pattern(coder,cblock,node,base1,base2,idx1,idx2,list)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,list
    integer,intent(in):: base1,base2,idx1,idx2
    type(pm_ptr):: p1,p2
    integer:: i,j,n1,n2,m,vbase
    logical:: has_dollar

    n1=base2-base1
    n2=coder%vtop-base2
    if(n1==0.or.n2==0) return

    ! Check is cross matching of .name proves no alias is possible
    if(match_ref_names(coder,cblock,node,base1,base2,idx1,idx2,list)) return
   
    ! May alias - code any required run-time subscript checks
    m=2
    call make_int_const(coder,cblock,node,idx1)
    call make_int_const(coder,cblock,node,idx2)
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
                call make_temp_var(coder,cblock,node)
                call swap_and_dup_code(coder)
                vbase=coder%vtop-1
                do while(.not.pm_fast_isname(p1))
                   has_dollar=trav_index_list(coder,cblock,node_arg(p1,2),.true.)
                   i=i+1
                   if(i>n1) exit
                   p1=coder%vstack(base1+i)
                enddo
                call make_sys_call(coder,cblock,node,sym_combine_indices,&
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
                call make_temp_var(coder,cblock,node)
                call swap_and_dup_code(coder)
                vbase=coder%vtop-1
                do while(.not.pm_fast_isname(p2))
                   has_dollar=trav_index_list(coder,cblock,node_arg(p2,2),.true.)
                   j=j+1
                   if(j>n2) exit
                   p2=coder%vstack(base2+j)
                enddo
                call make_sys_call(coder,cblock,node,sym_combine_indices,&
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
    if(m==2) then
       ! There were no subscripts, so clash can be confirmed at compile time
       call code_error(coder,node_arg(list,idx2),&
            'Argument aliased by "&" argument')
       call code_error(coder,node_arg(list,idx1),&
            'Argument causing the alias')
       coder%vtop=coder%vtop-2
    else
       ! Code call to check for subscript aliasing
       call make_sys_call(coder,cblock,node,sym_check_alias,m,0)
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
  function match_ref_names(coder,cblock,node,base1,base2,idx1,idx2,list) &
       result(differ)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,list
    integer,intent(in):: base1,base2,idx1,idx2
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
    if(n>1.or.node_sym(node)==sym_dotdotdot) then
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
    endif
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
       call make_sys_call(coder,cblock,node,sym_tuple,n,1,&
            aflags=flags)
    endif
    if(max_idx>0) then
       if(coder%par_state==par_state_outer) then
          call code_error(coder,node,'Cannot have "$" index outside of any parallel context')
       endif
       coder%temp2=pop_code(coder)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call code_val(coder,coder%temp2)
       coder%temp2=pm_null_obj
       if(max_idx>1.and.all(which(2:max_idx)>0)) then
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          do i=2,max_idx
             if(which(i)>0) then
                call make_static_long_const(coder,cblock,node,int(which(i),pm_ln))
             else
                call make_const(coder,cblock,node,pm_null_obj)
             endif
          end do
          call make_sys_call(coder,cblock,node,sym_tuple,max_idx-1,1)
          call make_sys_call(coder,cblock,node,sym_make_dtuple,2,1)
       else
          call make_sys_call(coder,cblock,node,sym_make_dtuple,1,1)
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
    call make_sys_var(coder,cblock,node,name,flags)
    var=top_code(coder)
    call swap_code(coder)
    call make_sys_call(coder,cblock,node,sym_dup,1,1,aflags=call_ignore_rules)
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
    call make_sys_call(coder,cblock,node,sym_dup,1,1,aflags=call_ignore_rules)
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
          call make_basic_sp_call(coder,cblock,node,&
               merge(sym_partial,sym_coherent,coder%par_state>=par_state_cond),&
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
    call cnode_set_flags(v,var_flags,var_is_changed)
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
       call make_const(coder,cblock,pnode,coder%true)
    case(sym_false)
       call make_const(coder,cblock,pnode,coder%false)
    case(sym_null) 
       call make_const(coder,cblock,pnode,pm_null_obj)
    case(sym_arg,sym_name,sym_use)
       call trav_name(coder,cblock,node,sym,node_arg(node,1))
    case(sym_proc)
       if(node_numargs(node)==1) then
          call proc_const(coder,cblock,node,node_arg(node,1))
       else
          p=find_imported_decl(coder,node,&
               node_arg(node,1),node_arg(node,2),modl_proc)
          if(pm_fast_isnull(p)) then
             call make_temp_var(coder,cblock,node)
          else
             call proc_const_from_decl(coder,cblock,node,p)
          endif
       endif
       return
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
    case(sym_unique)
       p=node_arg(node,1)
       call name_const(node,int(p%offset))
       return
    case(sym_dash)
       p=node_arg(node,1)
       if(pm_fast_isname(p)) then
          call make_static_bool_const(coder,cblock,node,&
               p%offset==sym_true)
       else
          call make_const(coder,cblock,node,p,&
               pm_new_value_typ(coder%context,p))
       endif
       return
    case(sym_fix)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       save_fixed=coder%fixed
       coder%fixed=.true.
       call trav_closed_expr(coder,cblock,node,node_arg(node,1))
       coder%fixed=save_fixed
       call make_sp_call(coder,cblock,node,sym_dash,1,1)
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
             call code_val(coder,coder%var(i+1))
          else
             call code_error(coder,node,&
                     'Not a keyword argument in "present": ',p)
             call make_temp_var(coder,cblock,node)
          endif
       endif
    case(first_operator:last_operator,&
         sym_if_expr,sym_switch_expr)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       do i=1,node_numargs(node)
          call trav_expr(coder,cblock,&
               node,node_arg(node,i))
       enddo
       call make_sys_call(coder,cblock,node,&
            sym,node_numargs(node),1)
    case(sym_uhash,sym_ustar)
       if(coder%par_state>par_state_outer) then
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          call make_comm_call_args(coder,cblock,pnode)
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call make_comm_sys_call(coder,cblock,node,&
               merge(sym_hash,sym_mult,sym==sym_uhash),1,1)
       else
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call make_sys_call(coder,cblock,node,&
               merge(sym_hash,sym_mult,sym==sym_uhash),1,1)
       endif
    case(sym_lt)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call trav_expr(coder,cblock,node,node_arg(node,2))
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call make_sys_call(coder,cblock,node,sym_gt,2,1)
    case(sym_le)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call trav_expr(coder,cblock,node,node_arg(node,2))
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call make_sys_call(coder,cblock,node,sym_ge,2,1)
    case(sym_pm_dref:sym_pm_ref)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       do i=1,node_numargs(node)
          call trav_expr(coder,cblock,node,node_arg(node,i))
       enddo
       call make_sp_call(coder,cblock,node,sym,node_numargs(node),1)
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
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call make_sp_call(coder,cblock,node,sym_dcaret,1,1)
    case(sym_dot)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       if(node_sym(node_arg(node,1))==sym_name) then
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call make_const(coder,cblock,node,node_arg(node,2))
          call make_sp_call(coder,cblock,node,sym_dot,2,1)
       else
          outmode=trav_ref(coder,cblock,pnode,node,ref_is_val)
          if(coder%par_state>par_state_outer) then
             p=pop_code(coder)
             call make_comm_call_args(coder,cblock,node)
             call code_val(coder,p)
             call make_static_bool_const(coder,cblock,node,iand(outmode,ref_has_at)/=0)
             call make_comm_sys_call(coder,cblock,node,sym_get_ref,2,1)
             call check_par_nesting(coder,cblock,node,.true.)
          else
             call make_sys_call(coder,cblock,node,sym_get_val_ref,1,1)
          endif
       endif
    case(sym_get_dot_ref)
       outmode=trav_ref(coder,cblock,pnode,node,ref_is_val)
    case(sym_get_dot,sym_sub)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       outmode=trav_ref(coder,cblock,pnode,node,ref_is_val)
       if(coder%par_state>par_state_outer) then
          p=pop_code(coder)
          call make_comm_call_args(coder,cblock,node)
          call code_val(coder,p)
          call make_static_bool_const(coder,cblock,node,iand(outmode,ref_has_at)/=0)
          call make_comm_sys_call(coder,cblock,node,sym_get_ref,2,1)
          call check_par_nesting(coder,cblock,node,.true.)
       else
          call make_sys_call(coder,cblock,node,sym_get_val_ref,1,1)
       endif
    case(sym_at)
       outmode=trav_ref(coder,cblock,pnode,node,ref_is_val)
    case(sym_open)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call trav_call(coder,cblock,pnode,node,1,.false.)
    case(sym_pval,sym_pval_as)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call trav_expr(coder,cblock,node,node_arg(node,2))
       call swap_code(coder)
       call make_sp_call(coder,cblock,node,sym,2,1)
    case(sym_type_val)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call trav_type_constraint(coder,node,node_arg(node,1))
       call make_const(coder,cblock,node,&
            pm_fast_tinyint(coder%context,pop_word(coder)))
       call make_sp_call(coder,cblock,node,sym_type_val,1,1)
    case(sym_array_former,sym_matrix_former)
       i=node_get_num(node,node_args+2)
       if(sym==sym_matrix_former) then
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
       endif
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call array_span(node_get_num(node,node_args+3),node_get_num(node,node_args+1))
       if(i/=1) then
          call array_span(node_get_num(node,node_args+4),i)
          call make_sys_call(coder,cblock,node,sym_array,2,1)
       else
          call make_sys_call(coder,cblock,node,sym_array,1,1)
       endif
       list=node_arg(node,1)
       call trav_expr(coder,cblock,list,node_arg(list,1))
       call swap_code(coder)
       call make_sys_call(coder,cblock,list,sym_do_dim,2,1)
       if(sym==sym_matrix_former) then
          call make_sys_call(coder,cblock,node,sym_matrix_former,1,1)
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
                  coder%true_name)
             return
          endif
       endif
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       if(pm_fast_isnull(node_arg(node,1))) then
          call make_sys_call(coder,cblock,node,sym_active,0,1)
          call dup_code(coder)
          call make_basic_sp_call(coder,cblock,node,sym_coherent,1,0,coder%par_depth)
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
       call trav_par_expr(coder,cblock,node)
    case(sym_mode,sym_always)
       call trav_mode_stmt(coder,cblock,node,sym,.true.)
    case(sym_cast)
       call make_temp_var(coder,cblock,node)
       call dup_code(coder)
       call trav_expr(coder,cblock,node,node_arg(node,1))
       call trav_expr(coder,cblock,node,node_arg(node,2))
       call make_sp_call(coder,cblock,node,sym_cast,2,1)
    case(sym_number,sym_string)
       if(coder%fixed) then
          p=node_arg(node,1)
          call make_const(coder,cblock,node,p,&
               pm_new_value_typ(coder%context,p))
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
           pm_new_name_typ(coder%context,nm))
    end subroutine name_const
    
    subroutine range_const(p,n)
      type(pm_ptr):: p
      integer:: n
      call make_temp_var(coder,cblock,p)
      call dup_code(coder)
      call make_long_const(coder,cblock,p,0_pm_ln)
      call make_long_const(coder,cblock,p,int(n-1,pm_ln))
      call make_sys_call(coder,cblock,p,sym_dotdot,2,1)
    end subroutine range_const

    subroutine array_span(low,n)
      integer,intent(in):: low,n
      if(low==0) then
         call make_long_const(coder,cblock,node,&
              int(n,pm_ln))
      else
         call make_temp_var(coder,cblock,node)
         call dup_code(coder)
         call make_long_const(coder,cblock,node,&
              int(low,pm_ln))
         call make_long_const(coder,cblock,node,&
              int(low+n-1,pm_ln))
         call make_sys_call(coder,cblock,node,sym_dotdot,2,1)
      endif
    end subroutine array_span
    
  end subroutine trav_expr

  !========================================================
  ! Traverse single variable in an expression
  !========================================================
  subroutine trav_var(coder,cblock,node,name)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,name
    type(pm_ptr):: var
    var=find_var(coder,name)
    if(pm_fast_isnull(var)) then
       call code_error(coder,node,'Variable has not been defined: ',name)
       call make_temp_var(coder,cblock,node)
       return
    endif
    if(cnode_flags_set(var,var_flags,var_is_not_inited)) then
       call code_error(coder,node,&
            'Variable is not yet initialised: ',name)
    endif
    if(cnode_flags_set(var,var_flags,var_is_aliased)) then
       coder%aliased=.true.
       var=cnode_get(var,var_extra_info)
    endif
    call code_val(coder,var)
  contains
    include 'fisnull.inc'
  end subroutine trav_var

  !==================================================================
  ! Name in usual expression context (may be variable or parameter)
  !==================================================================
  subroutine trav_name(coder,cblock,node,sym,name,proc)
    type(code_state):: coder
    type(pm_ptr),intent(in):: cblock,node,name
    type(pm_ptr),intent(out),optional:: proc
    integer:: sym
    type(pm_ptr):: p

    if(sym==sym_use) then
       p=find_param(coder,cblock,node,name,node_arg(node,2))
       if(pm_fast_isnull(p)) then
          p=find_imported_decl(coder,node,&
               name,node_arg(node,2),&
               modl_proc)
          if(present(proc)) then
             proc=p
          else
             if(pm_fast_isnull(p)) then
                ! Note find_imported decl gives own error messages
                call make_var(coder,cblock,node,name,0)
             else
                call proc_const_from_decl(coder,cblock,node,p)
             endif
          endif
       else
          call code_val(coder,p)
       endif
    else
       p=find_var(coder,name)
       if(pm_fast_isnull(p)) then
          p=find_param(coder,cblock,node,name)
          if(pm_fast_isnull(p)) then
             p=find_decl(coder,node,name,modl_proc)
             if(pm_fast_isnull(p)) then
                call code_error(coder,node,&
                     'Name not defined:',name)
                call make_var(coder,cblock,node,name,0)
             else
                if(present(proc)) then
                   proc=find_decl(coder,node,name,modl_proc)
                else
                   call proc_const(coder,cblock,node,name)
                endif
             endif
          else
             call code_val(coder,p)
          endif
       else
          if(.not.cnode_flags_clear(p,var_flags,&
               var_is_not_inited+var_is_aliased)) then
             if(cnode_flags_set(p,var_flags,&
                  var_is_not_inited)) then
                call code_error(coder,node,&
                     'Variable is not yet initialised: ',name)
             else
                p=cnode_get(p,var_extra_info)
                coder%aliased=.true.
             endif
          endif
          if(cnode_flags_set(p,var_flags,var_is_ref)) then
             call make_temp_var(coder,cblock,node)
             call dup_code(coder)
             call code_val(coder,p)
             call make_sys_call(coder,cblock,node,sym_get_ref,1,1,&
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
          decl=node_arg(node_get(decl,typ_includes),1)
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
    call make_temp_var(coder,cblock,node)
    call dup_code(coder)
    basex=coder%vtop
    info=trav_structrec_decl(coder,decl,decl)
    call make_const(coder,cblock,node,info)
    if(pm_fast_isnull(node_arg(node,1))) then
       call code_num(coder,-1)
       tno=info%data%i(info%offset+1)
       tno=pm_typ_arg(coder%context,tno,1)
    else
       call trav_type(coder,node,node_arg(node,1))
       tno=pm_user_typ_body(coder%context,top_word(coder))
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
             call cast_element(node_arg(exprs,i),pm_typ_arg(coder%context,tno,j))
             cycle outer
          endif
       enddo
       p=node_arg(elems,j)
       if(node_sym(p)==sym_define) then
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
          call element_error(node,sym,name,name1,i)
       enddo outer2
    endif

    ! Tidy up and create call
    if(pm_debug_checks) then
       if(coder%vtop/=basex+n+2) then
          write(*,*) '>>',coder%vtop,n,coder%vtop-n-2
          call pm_panic('trav_structrec')
       endif
    endif
    call make_sp_call(coder,cblock,node,sym,n+2,1,flags=coder%run_flags)
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
      if(tno/=0) then
         call make_cast(coder,cblock,node,tno)
      endif
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
    call make_temp_var(coder,cblock,node)
    call swap_and_dup_code(coder)
    call make_temp_var(coder,cblock,node)
    call dup_code(coder)
    call make_const(coder,cblock,node,&
         pm_fast_tinyint(coder%context,tno))
    call make_sp_call(coder,cblock,node,sym_type_val,1,1)
    call make_sys_call(coder,cblock,node,sym_as,2,1)
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
       call push_word(coder,pm_typ_new_any)
       call push_word(coder,0)
       do i=1,n
          call trav_type(coder,pnode,node_arg(node,i))
       enddo
       call make_type(coder,2+n)
    case(sym_and)
       n=node_numargs(node)
       call push_word(coder,pm_typ_new_all)
       call push_word(coder,0)
       do i=1,n
          call trav_type(coder,pnode,node_arg(node,i))
       enddo
       call make_type(coder,2+n)
    case(sym_pval)
       call push_word(coder,pm_typ_new_poly)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_type_val)
       call push_word(coder,pm_typ_new_type)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_includes)
       call push_word(coder,pm_typ_new_includes)
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
       call push_word(coder,pm_new_name_typ(coder%context,int(name%offset)))
    case(sym_dash)
       name=node_arg(node,1)
       if(pm_fast_isname(name)) then
          if(name%offset==sym_true) then
             call push_word(coder,coder%true_name)
          else
             call push_word(coder,coder%false_name)
          endif
       else
          call push_word(coder,pm_new_value_typ(coder%context,name))
       endif
    case(sym_contains)
       call push_word(coder,pm_typ_new_contains)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_casts_to)
       call push_word(coder,pm_typ_new_has)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_except)
       call push_word(coder,pm_typ_new_except)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call trav_type(coder,pnode,node_arg(node,2))
       call make_type(coder,4)
    case(sym_type)
       call trav_type_decl(coder,pnode,node)
    case(sym_open_brace)
       name=node_arg(node,1)
       call push_word(coder,pm_typ_new_user)
       call push_word(coder,int(name%offset))
       typno=get_typeno(2)
       if(typno==0) call pm_panic('Intrinsic type not found')
       call push_word(coder,typno)
    case(sym_struct,sym_rec)
       flags=node_num_arg(node,7)
       name=node_arg(node,2)
       if(sym==sym_struct) then
          call push_word(coder,pm_typ_new_struct+flags)
       else
          call push_word(coder,pm_typ_new_rec+flags)
       endif
       call push_word(coder,abs(int(name%offset)))
       val=node_arg(node,1)
       n=node_numargs(val)
       do i=1,n
          call trav_type(coder,pnode,node_arg(val,i))
       enddo
       call make_type(coder,n+2)
    case(sym_caret)
       call push_word(coder,pm_typ_new_array)
       call push_word(coder,node_get_num(node,node_args+1))
       call trav_type(coder,pnode,node_arg(node,1))
       call trav_type(coder,pnode,node_arg(node,3))
       call push_word(coder,int(pm_long))
       call make_type(coder,5)
    case(sym_dcaret)
       call push_word(coder,pm_typ_new_vect)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_underscore)
       call push_word(coder,pm_typ_new_bottom)
       call push_word(coder,0)
       call make_type(coder,2)
    case(sym_const)
       call push_word(coder,pm_typ_new_const)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_list,sym_dotdotdot)
       if(sym==sym_dotdotdot) then
          call push_word(coder,pm_typ_new_vtuple)
       else
          call push_word(coder,pm_typ_new_tuple)
       endif
       call push_word(coder,0)
       base=coder%wtop
       nshared=0
       n=node_numargs(node)
       do i=2,n,2
          val=node_arg(node,i)
          if(node_sym(val)==sym_mode)  nshared=nshared+1
          call trav_type(coder,val,val)
       enddo
       coder%wstack(base)=nshared
       call make_type(coder,n/2+2)
    case(sym_define,sym_var)
       call trav_type(coder,pnode,node_arg(node,1))
    case(sym_pm_dref)
       call push_word(coder,pm_typ_is_dref)
       n=node_get_num(node,node_args)
       call push_word(coder,n)
       n=node_numargs(node)
       do i=2,n
          call trav_type(coder,pnode,node_arg(node,i))
       enddo
       call make_type(coder,n+1)
    case(sym_amp)
       call push_word(coder,pm_typ_new_amp)
       call push_word(coder,0)
       call trav_type(coder,pnode,node_arg(node,1))
       call make_type(coder,3)
    case(sym_mode)
       call trav_type(coder,pnode,node_arg(node,1))
       typno=pop_word(coder)
       call push_word(coder,&
            pm_typ_add_mode(coder%context,typno,&
            node_num_arg(node,2),.false.,.true.)) 
    case(sym_result)
       call push_word(coder,pm_typ_new_tuple)
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
      tno=pm_typ_lookup(coder%context,&
           coder%wstack(coder%wtop-size+1:coder%wtop))
    end function get_typeno

    ! Look up type name and return number
    function get_user_typeno(size) result(tno)
      integer,intent(in):: size
      integer:: tno
      tno=pm_user_typ_lookup(coder%context,&
           coder%wstack(coder%wtop-size+1:coder%wtop))
    end function get_user_typeno

    recursive subroutine proc_type
      type(pm_ptr):: dp,list,arg
      integer:: i,j,n,base

      call push_word(coder,pm_typ_new_proc)
      call push_word(coder,0)

      base=coder%wtop
      dp=node_arg(node,1)
      do i=1,node_numargs(dp),2
         if(.not.pm_fast_isnull(find_type_var(coder,node_arg(dp,i)))) then
            call code_error(coder,node,&
                 'Cannot shadow type-match parameter:',node_arg(dp,i))
         endif
      enddo
      call push_word(coder,pm_typ_new_proc_sig)
      call push_word(coder,node_get_num(node,node_args+1))
 
      do i=3,4
         list=node_arg(node,i)
         call push_word(coder,&
              merge(pm_typ_is_vtuple,pm_typ_is_tuple,node_sym(list)==sym_dotdotdot))
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
  ! - type node should be of the form args type_name
  ! - process any associated type definition, if not already cached
  !=================================================================
  recursive subroutine trav_type_decl(coder,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: pnode,node
    type(pm_ptr):: namenode,name,decl,dec,inc,pargs,also_dec
    type(pm_ptr):: twice_dec,main_dec,tname,tval,pars,newdec
    type(pm_ptr),target:: incset
    logical:: is_present,also_present,type_present,is_interface
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
    call push_word(coder,pm_typ_new_user)
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

    ! Check if this is a type variable
    base=coder%wtop
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
    new_type=pm_new_user_typ(coder%context,coder%wstack(base-nargs-1:base),0)
 
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
    is_interface=.false.
    dec=node_arg(decl,2)
    ibase=-1
    do
       sym=node_sym(dec)
       select case(sym)
       case(sym_includes,sym_is,sym_dotdotdot,sym_interface)
          main_dec=dec
          parbase=coder%wtop
          pars=node_get(dec,typ_params)
          npars=node_numargs(pars)/2
          call make_type_vars(coder,int(name%offset),dec,node,pars,&
               base-nargs,nargs)
          inc=node_get(dec,typ_ins)
          if(.not.pm_fast_isnull(inc)) then
             do i=1,node_numargs(inc)
                call trav_type(coder,pnode,node_arg(inc,i))
                call drop_word(coder)
             enddo
          endif
          has_constraints=.not.pm_fast_isnull(node_get(main_dec,typ_constraints))
          if(sym/=sym_is) then
             is_interface=sym==sym_interface
             inc=node_get(dec,typ_includes)
             if(.not.pm_fast_isnull(inc)) then
                ibase=coder%wtop
                call push_word(coder,pm_typ_new_any)
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
                call push_word(coder,pm_typ_new_any)
                call push_word(coder,0)
             endif
          else
             inc=node_get(dec,typ_includes)
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
          dec=node_get(dec,typ_link)
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
          if(is_interface) then
             call defer_type_check(coder,dec,main_dec,&
                  new_type,top_word(coder),sym_includes,&
                  cnode_is_interface_constraint)
          endif
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
             pargs=node_get(dec,typ_params)
             call make_type_vars(coder,int(name%offset),&
                  pnode,node,pargs,base-nargs,nargs,&
                  parbase,npars)
             inc=node_get(dec,typ_includes)
             if(.not.pm_fast_isnull(inc)) then
                do i=1,node_numargs(inc)
                   call trav_type(coder,pnode,node_arg(inc,i))
                   if(is_interface) then
                      call defer_type_check(coder,node_arg(inc,i),main_dec,&
                           new_type,top_word(coder),sym_includes,&
                           cnode_is_interface_constraint)
                   endif
                   if(has_constraints) then
                      call check_constraints(top_word(coder),dec)
                   endif
                enddo
             endif
             call pop_type_vars(coder)
          else
             ! sym_dotdotdot, sym_includes or sym_interface
             if(pm_debug_checks) then
                if(sym/=sym_dotdotdot.and.sym/=sym_includes.and.sym/=sym_interface) then
                   if(sym>=0.and.sym<=num_sym) then
                      write(*,*) 'SYM=',trim(sym_names(sym))
                   else
                      write(*,*) 'SYM=',sym
                   endif
                   call pm_panic('Not a type in trav_type_decl')
                endif
             endif
             if(sym==sym_dotdotdot.or.sym==sym_interface) then
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
          newdec=node_get(dec,typ_link)
       endif
       if(pm_fast_isnull(newdec)) exit
       if(node_get_modl_name(dec)/=&
            node_get_modl_name(newdec)) multiple_modules=.true.
       dec=newdec
    enddo

    ! Create a union type from the parts brought together
    if(.not.is_present.and.coder%wtop>ibase) then
       if(node_sym(main_dec)==sym_interface) then
          ! This is an interface type -- create it
          call make_type(coder,coder%wtop-ibase)
          call trav_interface(coder,node_get(main_dec,typ_interface),&
               pop_word(coder))
       elseif(coder%wtop-ibase>3) then
          call make_type(coder,coder%wtop-ibase)
       endif
    endif

    ! Set the body of the user type to be the new type
    call pm_user_typ_set_body(coder%context,new_type,top_word(coder))

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
       write(*,*) '#', trim(pm_typ_as_string(coder%context,top_word(coder)))
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
      tno=pm_typ_lookup(coder%context,&
           coder%wstack(coder%wtop-size+1:coder%wtop))
    end function get_typeno

    subroutine check_constraints(tno,node)
      integer,intent(in):: tno
      type(pm_ptr):: node
      type(pm_ptr):: constraints
      integer:: i
      
      ! Make an entry for each "<: type" entry to be checked later
      constraints=node_get(main_dec,typ_constraints)
      do i=1,node_numargs(constraints)
         call trav_type(coder,main_dec,node_arg(constraints,i))
         call defer_type_check(coder,node,node_arg(constraints,i),pop_word(coder),tno,&
              0,cnode_is_typ_constraint)
      enddo
    end subroutine check_constraints
    
  end subroutine trav_type_decl

  !===============================================================
  ! Traverse interface declaration and make type
  ! base_type must be union of types conforming to the interface
  !==============================================================
  recursive subroutine trav_interface(coder,node,base_type)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: base_type
    type(pm_ptr):: p,val
    integer:: n,i
    p=node_arg(node,2)
    call push_word(coder,pm_typ_new_interface)
    call push_word(coder,int(p%offset))
    call push_word(coder,base_type)
    p=node_arg(node,1)
    n=node_numargs(p)
    do i=1,n
       val=node_arg(p,i)
       call trav_type(coder,val,val)
    enddo
    call make_type(coder,n+3)
  end subroutine trav_interface


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
          call push_word(coder,pm_typ_new_param)
          call push_word(coder,(i+1)/2)
          call trav_type(coder,arg,arg)
          call make_type(coder,3)
       enddo
       n=n/2
       call make_type_vars(coder,0,pnode,decl,params,coder%wtop-n,n)
       call trav_type(coder,pnode,decl)
       tno=pop_word(coder)
       call pop_type_vars(coder)
       tno=pm_new_params_typ(coder%context,n,tno)
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
       tno=pm_new_params_typ(coder%context,0,tno)
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
    call pm_typ_record_by_name(coder%context,&
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
    coder%link(coder%top)=0
    coder%var(coder%top)=pm_null_obj
 
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
             call push_word(coder,pm_typ_new_all)
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
          call make_var_tab_entry(coder,int(pname%offset),&
               pm_fast_tinyint(coder%context,vtyp))
       endif

    enddo
    coder%top=coder%top+1
    coder%stack(coder%top)=typevar_end
    coder%link(coder%top)=base
    coder%var(coder%top)=pm_null_obj
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
    base=coder%link(coder%top)
    if(pm_debug_checks) then
       if(coder%stack(coder%top)/=typevar_end) &
            call pm_panic('Pop type vars  - no end record')
    endif 
    coder%top=coder%top-1
    call pop_vars_to(coder,base)
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
    base=coder%link(top)
    coder%top=coder%top+1
    nbase=coder%top
    coder%stack(coder%top)=typevar_start
    coder%link(coder%top)=0
    coder%var(coder%top)=pm_null_obj
    do i=base+1,top-1
       if(coder%stack(i)/=0) then
          call make_var_tab_entry(coder,coder%stack(i),coder%var(i))
       endif
    enddo
    coder%top=coder%top+1
    coder%stack(coder%top)=typevar_end
    coder%link(coder%top)=nbase
    coder%var(coder%top)=pm_null_obj
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
       k=find_var_entry(coder,n,int(coder%link(coder%top)))
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
    type(pm_typ_einfo):: einfo
    p=coder%prog_cblock
    keys=pm_dict_keys(coder%context,coder%context%tcache)
    vals=pm_dict_vals(coder%context,coder%context%tcache)

    ! Check no named type is problematically recursive
    do i=0,pm_dict_size(coder%context,coder%context%tcache)-1
       tv=keys%data%ptr(keys%offset+i)
       if(pm_tv_kind(tv)==pm_typ_is_user) then
          tno=i+1
          if(pm_typ_is_recur(coder%context,tno,tno)) then
             call code_error(coder,pm_null_obj,&
                  'Type directly includes itself: '//&
                  trim(pm_typ_as_string(coder%context,tno)))
             call pm_ptr_assign(coder%context,vals,i,pm_null_obj)
          endif
       endif
    enddo
    
    ! Check all named types include themselves (weeds out some errors)
    do i=0,pm_dict_size(coder%context,coder%context%tcache)-1
       tv=keys%data%ptr(keys%offset+i)
       if(pm_tv_kind(tv)==pm_typ_is_user) then
          tno=i+1
          ! Check type includes its body to avoid automatic true return
          if(.not.pm_typ_includes(coder%context,tno,&
               pm_user_typ_body(coder%context,tno),pm_typ_incl_typ,&
               einfo)) then
             call code_error(coder,pm_null_obj,&
                  'Type is incorrectly defined: '//&
                  trim(pm_typ_as_string(coder%context,tno)))
             call pm_typ_error(coder%context,einfo)
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
          if(.not.pm_typ_includes(coder%context,tno1,tno2,pm_typ_incl_typ,&
               einfo)) then
             call cnode_error(coder,p,&
                  'Type argument "'//&
                  trim(pm_name_as_string(coder%context,&
                  int(name%offset)))//&
                  '" does not meet constraint: '//&
                  trim(pm_typ_as_string(coder%context,tno1))//&
                  ' inc '//&
                  trim(pm_typ_as_string(coder%context,tno2)))
             call pm_typ_error(coder%context,einfo)
             call code_error(coder,cnode_arg(p,5),&
                  'Constraint that gave rise to above error')
          endif
       case(cnode_is_par_constraint)
          tno1=cnode_get_num(p,cnode_args+1)
          tno2=cnode_get_num(p,cnode_args+2)
          if(.not.pm_typ_includes(coder%context,tno1,tno2,pm_typ_incl_typ,&
               einfo)) then
             call cnode_error(coder,p,&
                  'Parameter "'//&
                  trim(pm_name_as_string(coder%context,&
                  int(name%offset)))//&
                  '" does not match base type; parameter contraint: '//&
                  trim(pm_typ_as_string(coder%context,tno1))//&
                  ' ,argument: '//&
                  trim(pm_typ_as_string(coder%context,tno2)))
             call pm_typ_error(coder%context,einfo)
             call code_error(coder,cnode_arg(p,5),&
                  'Constraint that gave rise to the above error')
          endif
       case(cnode_is_typ_constraint)
          tno1=cnode_get_num(p,cnode_args+1)
          tno2=cnode_get_num(p,cnode_args+2)
          if(.not.pm_typ_includes(coder%context,tno1,tno2,pm_typ_incl_equiv,&
               einfo)) then
             call cnode_error(coder,p,'Type does not meet constraint:')
             call pm_typ_error(coder%context,einfo)
             call code_error(coder,cnode_arg(p,5),&
                  'Type constraint referenced in above error')
          endif
       case(cnode_is_interface_constraint)
          tno1=pm_user_typ_body(coder%context,cnode_get_num(p,cnode_args+1))
          tno2=cnode_get_num(p,cnode_args+2)
          if(.not.pm_interface_typ_conforms(coder%context,tno1,tno2,einfo)) then
             call cnode_error(coder,p,'Type does not conform to interface')
             call pm_typ_error(coder%context,einfo)
             call code_error(coder,cnode_arg(p,5),&
                  'Definition of interface referenced in above error')
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
    type(pm_ptr):: list,procs,keys,sig,name,amp,prvar,proc,p,arg
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
    keys=node_arg(node,3)
    amp=node_arg(node,4)
    flags=node_get_num(node,node_args+4)
    shared_ref_ok=iand(flags,call_ignore_rules+proc_run_shared+proc_run_local+call_is_comm)/=0
    if(node_sym(list)==sym_dotdotdot) then
       flags=ior(flags,call_is_vararg)
    endif
    if(iand(flags,proc_run_local+proc_run_shared)/=0) then
       if(coder%par_state==par_state_outer) then
          call code_error(coder,node,&
               'Cannot have <<shared>> call attribute outside of a parallel context',&
               name)
       endif
    endif
    nargs=node_numargs(list)
    if(debug_codegen) then
       write(*,*) 'TRAV CALL>',&
            trim(pm_name_as_string(coder%context,int(name%offset))),&
            nargs,nret,coder%vtop,flags
       write(*,*) 'TOP==',coder%top
    endif

    ! Extra argument for comm calls
    if(iand(flags,call_is_comm)/=0) then
       iscomm=.true.
       call check_par_nesting(coder,cblock,node,pm_fast_isnull(amp))

       ! Comm call (& its args) operates in standard run mode
       ! - not the current one
       save_run_mode=coder%run_mode
       save_run_flags=coder%run_flags
       coder%run_mode=sym_complete
       coder%run_flags=0
    elseif(iand(flags,proc_run_shared+proc_run_local)/=0) then
       call check_par_nesting(coder,cblock,node,pm_fast_isnull(amp))
       iscomm=.false.
    elseif(iand(flags,proc_run_complete)/=0) then
       call check_par_nesting(coder,cblock,node,.false.)
       iscomm=.false.
    else
       iscomm=.false.
    endif

    base=coder%vtop
    has_shared_amp_arg=.false.
    
    ! Standard arguments
    if(pm_fast_isnull(amp)) then
       do i=1,nargs
          call trav_expr(coder,cblock,list,&
               node_arg(list,i))
       enddo
    else
       if(.not.amps_ok) then
          call code_error(coder,list,&
               'Call using "&" or "&&" arguments cannot be a component of an expression')
       endif

       amp=pm_name_val(coder%context,int(amp%offset))
       flags=ior(flags,call_is_assign_call)
      
       ! Make space on vstack to store in/out temp vars for amp args
       ampbase=coder%vtop
       coder%vstack(coder%vtop+1:coder%vtop+nargs)=pm_null_obj
       coder%vtop=coder%vtop+nargs

      ! Alias checks if needed
       nref=0
       abase=coder%top
       if(pm_opts%check_alias) then
          do j=0,pm_fast_esize(amp)
             i=amp%data%i(amp%offset+j)
             arg=node_arg(list,i)
             if(node_sym(arg)==sym_amp) then
                call trav_alias_checks(coder,cblock,list,amp,i,ampbase)
                nref=nref+1
             endif
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
             if(node_sym(arg)==sym_amp) then
                outmode=trav_ref(coder,cblock,list,&
                     node_arg(arg,1),&
                     merge(ref_ignores_rules+ref_is_amp,ref_is_amp,shared_ref_ok))
             else
                call make_temp_var(coder,cblock,list)
                call dup_code(coder)
                call trav_expr(coder,cblock,list,arg)
                call make_basic_sys_call(coder,cblock,node,sym_dup,1,1,&
                     coder%par_depth,call_ignore_rules)
                coder%vstack(ampbase+i)=top_code(coder)
             endif
             j=min(pm_fast_esize(amp),j+1)
          else
             arg=node_arg(list,i)
             coder%aliased=.false.
             call trav_expr(coder,cblock,list,arg)
             if(coder%aliased) then
                if(node_sym(arg)==sym_dot.or.&
                     node_sym(arg)==sym_sub) then
                   arg=coder%vstack(ampbase+i)
                   if(pm_fast_vkind(arg)==pm_tiny_int) then
                      !write(*,*) 'nref=',nref,arg%offset,'#',i
                      if(arg%offset==nref) cycle
                   endif
                endif
                call make_temp_var(coder,cblock,list)
                call swap_and_dup_code(coder)
                call make_sys_call(coder,cblock,node,sym_clone,1,1)
                coder%aliased=.false.
             endif
          endif
       enddo
       call hide_vars(coder,abase+1,atop)
    endif
    babase=merge(base+3,base+1,iscomm)

    ! Find procs with this signature
    amp=node_arg(node,4)
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
          call make_temp_var(coder,cblock,node)
          call dup_code(coder)
          call code_val(coder,coder%vstack(babase))
          call make_const(coder,cblock,node,node_arg(name,1))
          call make_sp_call(coder,cblock,node,sym_method_call,2,1)
       case default
          write(*,*) sym_names(vsym)
          call pm_panic('Bad VSYM in trav_call')
       end select
       if(vsym/=sym_dot.and.vsym/=sym_method_call.and.pm_fast_isnull(proc)) then
          call make_temp_var(coder,cblock,name)
       endif
    endif

    if(.not.pm_fast_isnull(proc)) then
       prvar=pm_null_obj
       procs=find_sig(coder,node,name,&
            amp,nargs,nret,flags,sig,iscomm,procroot=proc)
    else
       prvar=pop_code(coder)
       procs=find_vcall_sig(coder,node,amp,&
            nargs,nret,flags,sig,iscomm)
    endif
    
    ! Error return if no such proc
    if(pm_fast_isnull(procs)) then
       coder%vtop=obase-nret
       return
    endif
   
    ! Keyword arguments (need sig first)
    if(.not.pm_fast_isnull(prvar).and..not.pm_fast_isnull(keys)) then
       call code_error(coder,keys,'Cannot have keyword arguments in ".()" call')
       nkeys=0
    else
       nkeys=trav_keys(coder,cblock,keys,sig,iscomm)
    endif
    
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

    ! Debugging trace
    if(debug_codegen) then
       if(pm_fast_isname(name)) then
          write(*,*) 'TRAV CALL MAKE FULL CALL>',&
               trim(pm_name_as_string(coder%context,int(name%offset))),&
               nargs,nret,nkeys,coder%vtop
       else
         write(*,*) 'TRAV CALL MAKE FULL CALL> .proc ',&
               nargs,nret,nkeys,coder%vtop
       endif
       
       do i=coder%vtop-nargs-nkeys-nret+1,coder%vtop
          call qdump_code_tree(coder,pm_null_obj,6,coder%vstack(i),2)
       enddo
       write(*,*) 'TRAV CALL END MAKE FULL CALL>'
    endif

    flags=ior(flags,coder%run_flags)
    if(coder%par_state>=par_state_cond.and.&
         iand(flags,proc_run_complete+proc_run_shared+proc_run_local)==0) then
       flags=ior(flags,call_is_cond)
    endif
    if(coder%par_state==par_state_cond.or.&
         coder%par_state==par_state_par) then
       flags=ior(flags,call_is_unlabelled)
    endif
    
    ! Make the call
    call import_args(coder,cblock,node,nargs,nret,nkeys,amp,flags,abase)
    call make_full_call(coder,cblock,node,procs,amp,&
         nargs,nret,nkeys,flags,prvar,coder%par_depth)
 
    ! Write out && args
    if(.not.pm_fast_isnull(amp)) then
       amp=pm_name_val(coder%context,int(amp%offset))
       do i=0,pm_fast_esize(amp)
          j=amp%data%i(amp%offset+i)
          if(.not.pm_fast_isnull(coder%vstack(ampbase+j))) then
             call code_val(coder,coder%vstack(ampbase+j))
             call make_assignment(coder,cblock,list,node_arg(list,j))
          endif
       enddo
       coder%vtop=obase-nret
    endif
    
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

    ! Restore run mode/run flags for comm calls
    if(iscomm) then
       coder%run_mode=save_run_mode
       coder%run_flags=save_run_flags
    endif

    ! If debugging, check tidy up
    if(pm_debug_checks) then
       if(coder%vtop/=obase-nret.or.coder%wtop/=owbase) then
          write(*,*) obase,nret,obase-nret,coder%vtop,owbase,coder%wtop,nargs,otop,coder%top
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
  end subroutine trav_call

  !===========================================================================
  ! Process keyword arguments - returns num of arguments used
  ! -- represented as block of arguments between returns and standard args
  ! -- Order is determined by signature of *called* procedure(s)
  !==========================================================================
  recursive function trav_keys(coder,cblock,list,sig,iscomm) result(nkeys)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,list,sig
    logical,intent(in):: iscomm
    integer:: nkeys
    integer:: start,i,j
    type(pm_ptr):: keys,keynames,empty
    keys=cnode_arg(sig,1)
    if(.not.pm_fast_isnull(keys)) then
       ! Create empty block of arguments
       start=coder%vtop
       nkeys=pm_set_size(coder%context,keys)
       do i=1,nkeys
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
          do i=1,node_numargs(list),2
             call trav_expr(coder,cblock,list,node_arg(list,i+1))
             if(iscomm) then
                if(var_private(coder,top_code(coder))) then
                   call code_error(coder,list,&
                        'Keyword arguments to communicating call must be shared')
                endif
             endif
             j=pm_set_lookup(coder%context,keys,node_arg(list,i))
             if(j>0) then
                coder%vstack(start+j)=pop_code(coder)
             else
                call code_error(coder,list,&
                     'Unexpected keyword argument: ',&
                     node_arg(list,i))
                call drop_code(coder)
             endif
          enddo
          
          ! Process keys... in call
          if(node_sym(list)==sym_dotdotdot) then
             if(.not.pm_fast_isnull(coder%proc_keys)) then
                call pm_set_merge(coder%context,coder%proc_keys,keys)
                keynames=pm_set_keys(coder%context,keys)
                do i=1,nkeys
                   if(pm_fast_isnull(coder%vstack(start+i))) then
                      j=pm_set_lookup(coder%context,coder%proc_keys,&
                           keynames%data%ptr(keynames%offset+i-1))
                      if(j>0) then
                         call make_temp_var(coder,cblock,list)
                         coder%vstack(start+i)=top_code(coder)
                         call make_int_const(coder,cblock,&
                              list,j)
                         call make_int_const(coder,cblock,&
                              list,coder%proc_nret)
                         call make_sp_call(coder,cblock,&
                              list,sym_key,2,1)
                      endif
                   endif
                enddo
             endif
          endif
       endif
    endif
    if(nkeys>0) then
       empty=coder%undef_val
       do i=1,nkeys
          if(pm_fast_isnull(coder%vstack(start+i))) then
             coder%vstack(start+i)=empty
          endif
       enddo
    endif
  contains
    include 'fisnull.inc'
    include 'ftiny.inc'
  end function  trav_keys

  
  !===============================================================
  ! Traverse procedure definition
  !===============================================================
  recursive subroutine trav_proc(coder,callnode,node,nargs,keyargs,&
       amplocs,nret,procs,numprocs,lclbase)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,node,amplocs,keyargs,procs
    integer,intent(in):: nargs,nret,numprocs,lclbase

    type(pm_ptr):: cblock,cblock2,cblock3,cblock4
    type(pm_ptr):: p,par,amp,rtypes,arg,rv,tkeys
    integer:: i,j,n,base,obase,wbase,npars,cbase,nkeyargs
    integer:: flags,sym,loop_pars,reduce_base,reduce_start,rsig
    integer:: partyp
    integer:: save_index,t
    integer:: save_proc_base,&
         save_par_base, save_over_base,save_proc_par_depth,&
         save_proc_nret,save_par_state,save_proc_ncalls,&
         save_subs_index,save_run_mode,save_run_flags
    type(pm_ptr):: save_sub_array,save_loop_cblock, &
         save_proc_keys,save_label
    logical:: save_aliased

    integer:: pr_flags
    type(pm_reg),pointer:: reg
    logical:: complete,old_complete
    integer,save:: pdepth=0
    
    if(debug_codegen) then
       write(*,*) repeat(' ',pdepth),'TRAV PROC>',&
            trim(pm_name_as_string(coder%context,&
            node_get_num(node,proc_name))),&
            node_get_lineno(node),coder%wtop,coder%top
       pdepth=pdepth+1
    endif

    if(pm_fast_isnull(keyargs)) then
       nkeyargs=0 
    else
       nkeyargs=pm_set_size(coder%context,keyargs)
    endif
    
    ! Parameter types
    wbase=coder%wtop
    obase=coder%vtop
    partyp=proc_param_type(coder,node)
    call code_num(coder,int(partyp))
    if(coder%wtop/=wbase) call pm_panic('par type wstack mismatch')
    
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
          call code_val(coder,node_get(node,proc_data))
          call code_val(coder,node_get(node,proc_flags))
          call code_num(coder,proc_result_type(coder,node))
          if(nret>0) then
             p=node_get(node,proc_retas)
             if(pm_fast_istiny(p)) then
                call code_val(coder,p)
                call code_num(coder,0)
             elseif(pm_fast_isnull(p)) then
                p=node_get(node,proc_result_types)
                if(node_sym(p)==sym_dash) then
                   call code_null(coder)
                   call code_num(coder,sym_dash)
                else
                   call code_null(coder)
                   call code_num(coder,0)
                endif
             else
                call save_proc_state
                call init_proc_state
                cblock=make_cblock(coder,pm_null_obj,node,sym_present)
                npars=0
                call code_params(cblock,.false.)
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
             call code_null(coder)
             call code_num(coder,0)
          endif
       endif
       coder%id=coder%id+1
       call code_num(coder,coder%id)
       if(pm_debug_checks) then
          if(coder%vtop-cbase/=bi_node_size) then
             write(*,*) '===========',coder%vtop,cbase,cbase+bi_node_size
             do i=cbase+1,coder%vtop
                call qdump_code_tree(coder,pm_null_obj,6,&
                     coder%vstack(i),2)
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
          call pop_type_vars(coder)
          return
       endif

       old_complete=coder%par_state<par_state_cond
       
       call save_proc_state
       call init_proc_state
       
       ! Set up code block and imports
       cblock=make_cblock(coder,pm_null_obj,node,sym_proc)

       call push_par_scope(coder,cblock)

       reg=>pm_register(coder%context,'tproc',tkeys)
 
       ! Different types of procedure
       npars=0
       flags=node_get_num(node,proc_flags)
       pr_flags=flags
       if(iand(flags,proc_is_each_proc)==0) then
          if(iand(flags,proc_is_comm+proc_run_shared+proc_run_local)==&
               proc_is_comm) then
             loop_pars=coder%top
             if(iand(flags,proc_run_complete)/=0) then
                complete=.true.
                call check_param_modes(sym_complete,sym_complete)
             elseif(iand(flags,proc_is_uncond)/=0) then
                complete=.true.
             elseif(iand(flags,proc_is_cond)/=0) then
                complete=.false.
             else
                complete=old_complete
             endif
             call code_params(cblock,.true.)
             call code_keys(cblock,tkeys)
             call code_loop_startup(cblock,cblock2,cblock3)
             call code_check(cblock3)
             call code_body(cblock3)
             call code_result(cblock3,flags)
             call code_loop_finish(cblock,cblock2,cblock3)
          else
             if(iand(flags,proc_run_shared+proc_run_local)/=0) then
                loop_pars=coder%top
                call check_param_modes(sym_mirrored,sym_shared)
                call code_params(cblock,.true.)
                call export_params
             else
                call code_params(cblock,.false.)
             endif
             call code_keys(cblock,tkeys)
             call code_check(cblock)
             call code_body(cblock)
             call code_result(cblock,flags)
          endif
       else
          pr_flags=ior(pr_flags,proc_is_each_proc)
          call code_proc_each
       endif
       call close_cblock(coder,cblock)

       if(pm_debug_checks) then
          if(coder%vtop/=obase+2) then
             write(*,*) coder%vtop,obase+2
             do i=obase+3,coder%vtop
                write(*,*) '==================='
                call qdump_code_tree(coder,pm_null_obj,6,&
                     coder%vstack(i),2)
                write(*,*) '==================='
             enddo
             write(*,*) sym_names(sym)
             call pm_panic( 'Proc cnode mismatch' )
          endif
          if(coder%wtop/=wbase) then
             write(*,*) coder%wtop,wbase
             call pm_panic('proc wstack mismatch')
          endif
       endif

       
       ! Create proc code object 
       call code_num(coder,coder%index)                   ! Maximum index
       call code_num(coder,0)                             ! Recursion flag
       coder%id=coder%id+1
       call code_num(coder,coder%id)                      ! Procedure id. index
       call code_num(coder,proc_result_type(coder,node))  ! Return type
       call code_num(coder,npars)                         ! Number of parameters
       call code_num(coder,0)                             ! Number of keywords
       !                                                    (fixed later)
       call code_num(coder,nret)                          ! Number of returns
       call code_num(coder,pr_flags)                      ! Flags
       call code_num(coder,node_get_num(node,proc_name))  ! Name
       call code_num(coder,coder%proc_ncalls)             ! Number of calls
       call code_val(coder,tkeys)                         ! Keyword arg info
       call make_code(coder,node,cnode_is_proc,pr_node_size)
       
       call pm_delete_register(coder%context,reg)

       call pop_par_scope(coder,cblock,node)

       call restore_proc_state

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
       save_proc_base=coder%proc_base
       save_proc_ncalls=coder%proc_ncalls
       save_par_base=coder%par_base
       save_over_base=coder%over_base
       save_loop_cblock=coder%loop_cblock
       save_proc_keys=coder%proc_keys
       save_proc_par_depth=coder%proc_par_depth
       save_proc_nret=coder%proc_nret
       save_par_state=coder%par_state
       save_label=coder%label
       save_subs_index=coder%subs_index
       save_run_mode=coder%run_mode
       save_run_flags=coder%run_flags
       save_aliased=coder%aliased
    end subroutine save_proc_state

    subroutine init_proc_state
       coder%index=0
       coder%proc_base=coder%top
       coder%proc_ncalls=0
       coder%par_base=coder%top
       coder%over_base=coder%top+2
       coder%proc_keys=keyargs
       coder%proc_par_depth=coder%par_depth
       coder%proc_nret=nret
       coder%par_state=par_state_outer
       coder%run_mode=sym_complete
       coder%subs_index=-1
       coder%run_flags=0
       coder%aliased=.false.
    end subroutine init_proc_state

   subroutine restore_proc_state
       coder%index=save_index
       coder%proc_base=save_proc_base
       coder%proc_ncalls=save_proc_ncalls
       coder%par_base=save_par_base
       coder%over_base=save_over_base
       coder%loop_cblock=save_loop_cblock
       coder%proc_keys=save_proc_keys
       coder%par_depth=coder%proc_par_depth
       coder%proc_par_depth=save_proc_par_depth
       coder%proc_nret=save_proc_nret
       coder%par_state=save_par_state
       coder%run_mode=save_run_mode
       coder%run_flags=save_run_flags
       coder%label=save_label
       coder%subs_index=save_subs_index
       coder%aliased=save_aliased
    end subroutine restore_proc_state

    subroutine code_params(cblock,iscomm)
      type(pm_ptr),intent(in):: cblock
      logical,intent(in):: iscomm
      type(pm_ptr):: name,var,p
      integer:: state,flags,cflags
      p=node_get(node,proc_params)
      if(.not.pm_fast_isnull(p)) then
         amp=node_get(node,proc_amplocs)
         if(pm_fast_isnull(amp)) then
            do i=1,node_numargs(p),2
               flags=var_is_param
               name=node_arg(p,i)
               if(name%offset==sym_arg) flags=var_is_varg
               if(iscomm.and.i>3) then
                  cflags=coder%wstack(lclbase+(i+1)/2)
                  call make_var(coder,cblock,p,name,flags) 
                  if(iand(cflags,var_is_imported)==0) then
                     call var_set_par_depth(coder,top_code(coder),coder%par_depth+1)
                  endif
               else
                  call make_var(coder,cblock,p,name,flags)
               endif
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
               if(name%offset==sym_arg) flags=var_is_varg
               if(iscomm.and.i>3) then
                  cflags=coder%wstack(lclbase+(i+1)/2)
                  call make_var(coder,cblock,p,name,flags)
                  if(iand(cflags,var_is_imported)==0) then
                     call var_set_par_depth(coder,top_code(coder),coder%par_depth+1)
                  endif
               else
                  call make_var(coder,cblock,p,name,flags)
               endif
            enddo
         endif
         npars=npars+node_numargs(p)/2
         call make_basic_sp_call(coder,cblock,p,&
              sym_open,npars,0,coder%par_depth)
      endif
    end subroutine code_params

    subroutine export_params
      integer:: i,flags
      type(pm_ptr):: var
      p=node_get(node,proc_params)
      do i=2,npars
         var=coder%var(loop_pars+i)
         flags=coder%wstack(lclbase+i)
         if(iand(flags,var_is_imported)==0) then
            call make_var(coder,cblock,p,cnode_get(var,var_name),&
                 ior(iand(cnode_get_num(var,var_flags),&
                 var_is_var+var_is_ref),var_is_shadowed))
            call code_val(coder,var)
            call make_basic_sp_call(coder,cblock,p,sym_export_param,&
                 1,1,coder%par_depth)
         endif
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
                 'Return modes for a "<<'//trim(sym_names(flag_sym))//&
                 '>>" procedure must be undefined or "'//&
                 trim(sym_names(mode)))
         endif
      enddo
    end subroutine check_param_modes
    
    recursive subroutine code_keys(cblock,tkeys)
      type(pm_ptr),intent(in):: cblock
      type(pm_ptr),intent(inout):: tkeys
      type(pm_ptr):: vname,arg
      integer:: tbase,vb,base
      ! Keyword arguments
      p=node_get(node,proc_keys)
      if(.not.pm_fast_isnull(p)) then
         base=coder%vtop
         tbase=coder%wtop
         do i=1,node_numargs(p),3
            vname=node_arg(p,i)
            call make_var(coder,cblock,p,vname,&
                 var_is_key+var_is_multi_access)
            vb=coder%top
            call hide_vars(coder,coder%top,vb) ! Stop recursive use of tag name
            call make_sys_var(coder,cblock,p,&
                 pm_name2(coder%context,sym_present,&
                 int(vname%offset)),var_is_multi_access)
            j=pm_set_lookup(coder%context,keyargs,node_arg(p,i))
            if(pm_debug_checks) then
               if(j<0) call pm_panic('lookup key arg')
            endif
            ! key= sym_present(nret,j,default, [type])
            call make_int_const(coder,cblock,p,nret)
            call make_int_const(coder,cblock,p,j)
            call trav_expr(coder,cblock,p,node_arg(p,i+2))
            call reveal_vars(coder,vb,vb)
            if(.not.pm_fast_isnull(node_arg(p,i+1))) then
               call push_word(coder,(i+2)/3)
               arg=node_arg(p,i+1)
               call trav_type(coder,arg,arg)
               call make_int_const(coder,cblock,p,top_word(coder))
               call make_sp_call(coder,cblock,p,&
                    sym_present,4,2)
            else
               call make_sp_call(coder,cblock,p,&
                    sym_present,3,2)
            endif
         enddo
         ! Create tkeys (defined in parent subroutine) with vector of type constraints
         if(coder%wtop>tbase) then
            tkeys=pm_fast_newnc(coder%context,pm_int,coder%wtop-tbase)
            tkeys%data%i(tkeys%offset:tkeys%offset+coder%wtop-tbase-1)=&
                 coder%wstack(tbase+1:coder%wtop)
            coder%wtop=tbase
         endif
         if(pm_debug_checks) then
            if(base/=coder%vtop) call pm_panic('trav_proc key mismatch')
         endif
      endif
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
      

      !coder%over_base=coder%top
      call make_var_tab_entry(coder,sym_for,&
           coder%var(loop_pars+1))
 
      
      call make_sys_var(coder,cblock,node,sym_in,0)
      
      iter=coder%top
      coder%par_base=iter
      call make_sys_var(coder,cblock,node,sym_pling,var_is_shadowed)
      call code_val(coder,coder%var(iter+lv_distr))
      call make_sys_call(coder,cblock,node,sym_get_tile_sz,1,2)
      call make_sys_var(coder,cblock,node,sym_hash,var_is_shadowed)
      call var_set_par_depth(coder,coder%var(iter+lv_numz),coder%par_depth+1)
      call drop_code(coder)
      cblock2=make_cblock(coder,cblock,node,sym_proc)
      coder%loop_cblock=cblock2
      call drop_code(coder)
      
      ! Alias the region and subregion variables
      call make_var_tab_entry(coder,sym_region,coder%var(iter+lv_distr))
      call make_var_tab_entry(coder,sym_subregion,coder%var(loop_pars+2))
      coder%over_base =coder%top
      
      call push_par_scope(coder,cblock2)
      call make_var_tab_entry(coder,sym_here_in_tile,coder%var(loop_pars+3))
      call make_sys_var(coder,cblock2,node,sym_here,var_is_shadowed)
      call code_val(coder,coder%var(iter+lv_tile))
      call code_val(coder,coder%var(iter+lv_index))
      call make_sys_call(coder,cblock2,node,sym_get_element,2,1)
  
!!$      if(iter+lv_here/=coder%top) then
!!$         write(*,*) '#',iter+lv_here,coder%top
!!$         call pm_panic('iter mismatch in code_loop_startup')
!!$      endif
      
      coder%par_state=par_state_for
      coder%run_mode=sym_complete
      
      coder%par_state=merge(par_state_for,par_state_masked,complete)
           
      if(complete) pr_flags=ior(pr_flags,proc_run_complete)
      cblock3=make_cblock(coder,cblock2,node,sym_for_stmt)

      if(pm_is_compiling) then
         ! Call PM__do_over to set-up subset loops
         call make_sys_var(coder,cblock3,node,sym_nested_loop,var_is_shadowed)
         call dup_code(coder)
         call code_val(coder,coder%var(coder%over_base))
         call code_val(coder,coder%var(iter+lv_distr))
         call make_basic_sys_call(coder,cblock3,node,sym_do_over,2,1,&
              coder%par_depth-1,call_inline_when_compiling)
         call make_basic_sys_call(coder,cblock3,node,sym_nested_loop,1,0,&
              coder%par_depth-1,call_inline_when_compiling)
      endif
      
    end subroutine  code_loop_startup

    subroutine code_loop_finish(cblock,cblock2,cblock3)
      type(pm_ptr),intent(in):: cblock,cblock2,cblock3
      call close_cblock(coder,cblock3)
      call make_sp_call(coder,cblock2,node,sym_for,1,0)
      call close_cblock(coder,cblock2)
      call pop_par_scope(coder,cblock,node)
      call code_val(coder,coder%var(coder%par_base+lv_numz))
      call code_val(coder,cblock2)
      call code_val(coder,coder%var(coder%par_base+lv_num))
      call make_sp_call(coder,cblock,node,sym_pct,2,1)
    end subroutine code_loop_finish

    recursive subroutine code_proc_each
      integer:: cflags
      type(pm_ptr):: cblock5,cblock6,arg1
      integer:: slot1
      loop_pars=coder%top
      call code_params(cblock,iand(flags,proc_is_comm)/=0)
      call code_keys(cblock,tkeys)
      base=coder%vtop
      do i=1,nret
         call make_temp_var(coder,cblock,node)
      enddo
      cblock3=make_cblock(coder,cblock,node,sym_each)
      if(nret>0) then
         do i=1,nret
            call code_val(coder,coder%vstack(base+i))
         enddo
         call make_sp_call(coder,cblock3,node,sym_result,nret,0)
      endif
      call close_cblock(coder,cblock3)
      cblock2=make_cblock(coder,cblock,node,sym_each)
      slot1=coder%index+1
      if(iand(flags,proc_is_comm)==0) then
         if(debug_codegen) then
            write(*,*) 'TRAV EACH PROC>',&
                 trim(pm_name_as_string(coder%context,&
                 node_get_num(node,proc_name))),coder%wtop,coder%proc_base
         endif
         call code_body(cblock2)
         call code_result(cblock2,flags)
         ! Flag special cases: f(x) each(x)=x; f(x) each(x) {}; PM__dup()
         pr_flags=proc_is_each_proc
         if(npars==1.and.nret==1) then
            if(pm_fast_isnull(node_get(node,proc_stmts))) then
               arg1=node_arg(node_get(node,proc_params),1)
               if(arg1%offset==sym_dup) then
                  pr_flags=pr_flags+proc_is_dup_each
               elseif(arg1==node_arg(node_get(node,proc_result),1)) then
                  pr_flags=pr_flags+proc_is_thru_each
               endif
            endif
         elseif(nret==0) then
            if(node_numargs(node_get(node,proc_stmts))==0) then
               pr_flags=pr_flags+proc_is_empty_each
            endif
         endif
      else
         call code_loop_startup(cblock2,cblock4,cblock5)
         call code_check(cblock5)
         call code_body(cblock5)
         call code_result(cblock5,flags)
         call code_loop_finish(cblock2,cblock4,cblock5)
         pr_flags=proc_is_each_proc+proc_is_comm
      endif
      call close_cblock(coder,cblock2)
      cblock4=make_cblock(coder,cblock,node,sym_each)
      coder%temp=pm_fast_newnc(coder%context,&
           pm_int,2)
      rv=coder%temp
      call make_const(coder,cblock,node,coder%temp)
      rv%data%i(rv%offset)=slot1
      p=node_get(node,proc_reduce)
      n=node_numargs(p)
      do i=1,n
         par=node_arg(p,i)
         arg=find_var(coder,node_arg(p,i))
         if(pm_fast_isnull(arg)) then
            call code_error(coder,p,&
                 '"each" variable not in parameter list',par)
         elseif(cnode_flags_clear(arg,var_flags,var_is_param)) then
            call code_error(coder,p,&
                 'repeated "each" variable')
         endif
         call code_val(coder,arg)
         call make_var(coder,cblock,node,node_arg(p,i),var_is_shadowed)
      enddo
      base=coder%vtop
      do i=1,nret
         call make_temp_var(coder,cblock,node)
      enddo
      if(sym/=sym_proc) then
         call code_loop_startup(cblock4,cblock5,cblock6)
      else
         cblock6=cblock4
      endif
      do i=1,nret
         call code_val(coder,coder%vstack(base+i))
      enddo
      if(nkeyargs>0) then
         do i=1,nkeyargs
            call make_temp_var(coder,cblock6,node)
            call dup_code(coder)            
            call make_int_const(coder,cblock6,&
                 node,i+nret)
            call make_sp_call(coder,cblock6,&
                 node,sym_key,1,1)
         enddo
      endif
      p=node_get(node,proc_params)
      do i=1,node_numargs(p),2
         call code_val(coder,find_var(coder,node_arg(p,i)))
      enddo
      if(sym==sym_pct) then
         cflags=call_is_comm
      else
         cflags=0
      endif
      call make_full_call(coder,cblock6,node,procs,amplocs,npars,&
           nret,nkeyargs,cflags,pm_null_obj,coder%par_depth)
      if(sym/=sym_proc) then
         call code_loop_finish(cblock4,cblock5,cblock6)
      endif
      call close_cblock(coder,cblock4)
      rv%data%i(rv%offset+1)=coder%index
      call make_sp_call(coder,cblock,node,sym_each_proc,4+nret+n*2,nret)
    end subroutine code_proc_each

  end subroutine trav_proc


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
    p=find_decl(coder,pnode,pr,modl_proc)
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
    type(pm_ptr):: namep
    logical:: ok
    integer(pm_ln):: m
    namep=node_get(p,proc_name)
    call make_const(coder,cblock,node,namep,&
         proc_type_from_decl(coder,p,node))
    call pm_dict_set(coder%context,coder%proc_name_vals,&
         namep,p,.true.,.true.,ok,m)
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
    call push_word(coder,pm_typ_new_proc)
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
       if(iand(flags,proc_is_comm)/=0) then
          sym=sym_pct
       else
          sym=sym_proc
       endif
       call push_word(coder,pm_typ_new_proc_sig)
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
    integer:: i,n

    tno=node_get_num(node,proc_coded_params)
    if(tno>0) return
    
    p=node_get(node,proc_params)
    call push_word(coder,merge(pm_typ_is_vtuple,pm_typ_is_tuple,&
         node_sym(p)==sym_dotdotdot))
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

    call node_set_num(node,proc_coded_params,tno)
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
    tno=node_get_num(node,proc_coded_results)
    if(tno>0) return
    p=node_get(node,proc_result_types)
    if(node_sym(node)==sym_builtin.and.node_sym(p)==sym_dash) then
       p=node_arg(p,1)
    endif
    if(pm_fast_isnull(p)) then
       nret=node_get_num(node,proc_numret)
       if(nret==0) then
          call push_word(coder,pm_typ_is_tuple)
          call push_word(coder,0)
       else
          call push_word(coder,pm_typ_is_undef_result)
          call push_word(coder,nret)
       endif
       call make_type(coder,2)
    else
       if(node_sym(p)==sym_dash) then
          p=node_arg(p,1)
       endif
       call push_word(coder,pm_typ_is_tuple)
       call push_word(coder,0)
       nret=node_numargs(p)
       do i=1,nret
          arg=node_arg(p,i)
          call trav_type(coder,arg,arg)
       enddo
       call make_type(coder,nret+2)   
    endif
    tno=pop_word(coder)
    call node_set_num(node,proc_coded_results,tno)
  contains
    include 'fisnull.inc'
  end function proc_result_type

  
  !========================================================
  ! Find of construct procedure signature
  !========================================================
  recursive function find_sig(coder,node,pname,amplocs,&
       nargs,nret,flags,sigvect,iscomm,noerr,procroot) result(sig)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,pname,amplocs
    integer,intent(in):: nargs,nret,flags
    type(pm_ptr),intent(out):: sigvect
    logical,intent(in):: iscomm
    logical,optional,intent(in):: noerr
    type(pm_ptr),optional,intent(in):: procroot
    type(pm_ptr):: sig,name,modl,xproc,arg
    type(pm_ptr):: cblock,keys, procdef,proc,amplocs2,keyargs, cproc
    integer:: nret2,nargs2,base,i,j,n,oldwtop,numprocs,lclbase
    integer:: k,oldk
    integer:: flags2,owtop,sigflags,siglen
    logical:: vkeys
    character(len=100):: str
    owtop=coder%wtop
    lclbase=-1
    if(iscomm) then
       lclbase=coder%wtop
       do i=1,nargs
          arg=coder%vstack(coder%vtop-nargs+i)
          if(cnode_get_kind(arg)==cnode_is_var) then
             coder%wstack(coder%wtop+i)=&
                  merge(var_is_imported,0,par_depth(coder,arg)<coder%par_depth)
          else
             coder%wstack(coder%wtop+i)=0
          endif
       enddo
       coder%wtop=coder%wtop+nargs
    end if
    if(present(procroot)) then
       procdef=procroot
    else
       procdef=find_decl(coder,node,pname,modl_proc)
       if(pm_fast_isnull(procdef)) then
          call code_error(coder,node,&
               'Cannot find procedure:',pname)
          sig=pm_null_obj
          sigvect=pm_null_obj
          return
       endif
    endif
    name=node_arg(procdef,1)
    call check_wstack(coder,4)
    sigflags=iand(flags,call_is_comm)
    siglen=4
    if(coder%fixed) sigflags=ior(sigflags,call_is_fixed)
    if(int(sigflags,call_is_comm)/=0) then
       if(coder%par_state<par_state_cond) then
          sigflags=ior(sigflags,proc_run_complete)
       endif
       siglen=siglen+nargs
    endif
    coder%wstack(coder%wtop+1)=sigflags
    if(coder%fixed) coder%wstack(coder%wtop+1)=ior(coder%wstack(coder%wtop+1),call_is_fixed)
    coder%wstack(coder%wtop+2)=amplocs%offset
    coder%wstack(coder%wtop+3)=nret
    coder%wstack(coder%wtop+4)=name%offset       
    coder%wtop=coder%wtop+4
    if(debug_codegen) then
       call pm_name_string(coder%context,int(name%offset),str)
       call pm_name_string(coder%context,int(amplocs%offset),str(len_trim(str)+2:))
       write(*,*) 'SIG:',trim(str),' NARGS=',nargs,'nret=',nret,&
            'AMP',amplocs%offset,'FLAGS=',flags
    endif
    k=pm_ivect_lookup(coder%context,coder%sig_cache,&
         coder%wstack(coder%wtop-siglen+1:),siglen)
    if(k==0) then
       ! Find keywords for this signature
       keys=pm_null_obj
       vkeys=.false.
       proc=node_arg(procdef,2)
       modl=node_get(proc,proc_module)
       numprocs=0
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
             do i=1,node_numargs(keyargs),3
                j=pm_set_add(coder%context,keys,&
                     node_arg(keyargs,i))
             enddo
          endif
          numprocs=numprocs+1
          proc=node_get(proc,proc_link)
          if(pm_fast_isnull(proc)) exit
       enddo
       if(pm_fast_isnull(keys)) call code_val(coder,pm_null_obj)
       if(vkeys) then
          call code_num(coder,proc_has_vkeys)
       else
          call code_num(coder,0)
       endif
       call make_code(coder,node,cnode_is_arglist,2)
       oldwtop=coder%wtop
       oldk=pm_idict_add(coder%context,coder%sig_cache,&
            coder%wstack(coder%wtop-siglen+1:),siglen,&
            top_code(coder))
       sig=pm_fast_tinyint(coder%context,oldk)
       call drop_code(coder)
       base=coder%vtop
       
       ! Signature node starts with set of keys and combined flags
       call code_val(coder,keys)
       if(vkeys) then
          call code_num(coder,proc_has_vkeys)
       else
          call code_num(coder,0)
       endif
       
       ! Find and process conforming procs
       proc=node_arg(procdef,2)
       do
          if(debug_codegen) then
             write(*,*) 'CHECK PROC SIG>',&
                  pm_fast_isnull(node_get(proc,proc_link))
          endif
          if(proc_conforms()) then
             if(debug_codegen) then
                write(*,*) 'CONFORMING_PROC>',coder%vtop,base,trim(str)
             endif
             call trav_proc(coder,node,proc,nargs,keys,&
                  amplocs,nret,sig,numprocs,lclbase)
             if(coder%wtop/=oldwtop) call pm_panic('stray w')
             if(debug_codegen) then
                write(*,*) 'FINISHED TRAVERSING CONFORMING PROC>',&
                     coder%vtop,base,trim(str)
             endif
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
       if(debug_codegen) then
          write(*,*) 'SIGEND> ',&
               'NARGS=',nargs,'nret=',nret,'N=',&
               coder%vtop-base,'WT=',coder%wtop
       endif
       if(coder%vtop>base+2) then
          if(debug_codegen) &
               write(*,*) 'MAKE-SIG-VECT>',base,coder%vtop,coder%vtop-base
          call make_code(coder,node,cnode_is_arglist,coder%vtop-base)
          sig=top_code(coder)
          k=pm_idict_add(coder%context,coder%sig_cache,&
               coder%wstack(coder%wtop-siglen+1:),siglen,sig)
          if(k/=oldk) then
             write(*,*) k,oldk,oldwtop
             call pm_panic('k moved')
          endif
          call drop_code(coder)
          sigvect=sig
          sig=pm_fast_tinyint(coder%context,k)
       else
          call drop_code(coder)
          call drop_code(coder)
          if(.not.present(noerr)) then
             call code_error(coder,node,&
                  'Cannot find procedure with compatable signature: ')
             call more_error(coder%context,&
                  '        '//trim(sig_as_str(coder,name,amplocs,&
                  nargs,nret,flags)))
             proc=node_arg(procdef,2)
             call more_error(coder%context,&
                  '      Procedures considered:')
             i=0
             do
                call more_error(coder%context,&
                     '        '//trim(proc_sig_as_str(coder,proc)))
                proc=node_get(proc,proc_link)
                if(pm_fast_isnull(proc)) exit
                i=i+1
                if(i>11.and..not.pm_opts%see_all_procs) then
                   call more_error(coder%context,'... (to see all procedures use -fsee-all-procs)')
                   exit
                endif
             enddo
          endif
          sig=pm_null_obj
       endif
    else
       sig=pm_fast_tinyint(coder%context,k)
       sigvect=pm_dict_val(coder%context,coder%sig_cache,int(k,pm_ln))
    endif
    coder%wtop=owtop
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'ftiny.inc'

    function proc_conforms() result(ok)
      logical:: ok
      type(pm_ptr):: params,arg
      integer:: sym,flags2
      amplocs2=node_get(proc,proc_amplocs)
      nret2=node_get_num(proc,proc_numret)
      flags2=node_get_num(proc,proc_flags)
      if(debug_codegen) then
         write(*,*) 'CHECK CONFORM>',&
              amplocs%offset,amplocs2%offset,&
              nret,nret2,flags,flags2
      endif
      ok=(iand(flags,call_is_comm)==&
           iand(flags2,proc_is_comm)&
           .and.amplocs2%offset==amplocs%offset&
           .and.nret2==nret)
      if(iand(flags2,proc_is_cond+proc_is_uncond)/=0) then
         if(iand(flags2,proc_is_cond)/=0) then
            if(coder%par_state<par_state_cond) ok=.false.
         else
            if(coder%par_state>=par_state_cond) ok=.false.
         endif
      endif
    end function proc_conforms
       
  end function find_sig
  
  !======================================================================
  ! Find or construct signature for variable call using variable selector
  !======================================================================
  recursive function find_vcall_sig(coder,node,amplocs,&
       nargs,nret,flags,sigvect,iscomm) result(sig)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,amplocs
    integer,intent(in):: nargs,nret,flags
    type(pm_ptr),intent(out):: sigvect
    logical,intent(in):: iscomm
    type(pm_ptr):: sig,arg
    integer:: k
    integer:: i,cflags,flags2,locl_base,siglen
    character(len=100):: str
    siglen=4
    if(iscomm) then
       do i=1,nargs
          arg=coder%vstack(coder%vtop-nargs+i)
          if(cnode_get_kind(arg)==cnode_is_var) then
             coder%wstack(coder%wtop+i)=cnode_get_num(arg,var_flags)
          else
             coder%wstack(coder%wtop+i)=0
          endif
       enddo
       coder%wtop=coder%wtop+nargs
       siglen=siglen+nargs
    end if
    call check_wstack(coder,4)
    coder%wstack(coder%wtop+1)=iand(flags,call_is_comm)
    coder%wstack(coder%wtop+2)=amplocs%offset
    coder%wstack(coder%wtop+3)=nret
    coder%wstack(coder%wtop+4)=sym_proc
    coder%wtop=coder%wtop+4
    k=pm_ivect_lookup(coder%context,coder%sig_cache,&
         coder%wstack(coder%wtop-siglen+1:),siglen)
    if(k>0) then
       sig=pm_fast_tinyint(coder%context,k)
       sigvect=pm_dict_val(coder%context,&
            coder%sig_cache,int(k,pm_ln))
    else
       call code_null(coder)
       call code_num(coder,proc_is_var)
       call code_num(coder,coder%call_sig)
       call code_val(coder,node)
       call make_code(coder,node,cnode_is_arglist,4)
       sigvect=top_code(coder)
       k=pm_idict_add(coder%context,&
            coder%sig_cache,&
            coder%wstack(coder%wtop-siglen+1:),&
            siglen,sigvect)
       coder%call_sig=k
       
       sig=pm_fast_tinyint(coder%context,k)
       call drop_code(coder)
       
    endif
    coder%wtop=coder%wtop-siglen
  contains
    include 'ftiny.inc'
  end function find_vcall_sig

  !=======================================================================
  ! Complete variable-call signatures now all proc{..} values located
  !=======================================================================
  subroutine complete_vcall_sigs(coder)
    type(code_state):: coder
    integer(pm_ln):: kbase,k,newk,nbase
    type(pm_ptr):: name,names,nameset,amplocs,key,keys
    type(pm_ptr):: sig,vals,val,node,sigvect,procroot,procroots
    integer:: i,nret,nargs,base,n,lbase,flags
    k=coder%call_sig
    if(k==0) return

    if(pm_fast_isnull(coder%proc_name_vals)) then
       val=vals%data%ptr(vals%offset+k-1)
       call cnode_error(coder,val,&
            'Call to procedure value '//&
            ' but program does not create any procedure values')
       return
    endif
    do
       k=coder%call_sig
       kbase=k
       nameset=coder%proc_name_vals
       nbase=pm_dict_size(coder%context,nameset)
       names=pm_dict_keys(coder%context,nameset)
       procroots=pm_dict_vals(coder%context,nameset)
       keys=pm_dict_keys(coder%context,coder%sig_cache)
       vals=pm_dict_vals(coder%context,coder%sig_cache)
       do while(k/=0)
          key=keys%data%ptr(keys%offset+k-1)
          val=vals%data%ptr(vals%offset+k-1)
          node=cnode_arg(val,4)
          n=pm_fast_esize(key)
          flags=key%data%i(key%offset+n-3)
          nargs=n-4
          amplocs=pm_fast_name(coder%context,&
               key%data%i(key%offset+n-2))
          nret=key%data%i(key%offset+n-1)
          base=coder%vtop
          lbase=coder%wtop
          call check_wstack(coder,int(pm_fast_esize(key)-2))
          coder%wstack(coder%wtop+1:coder%wtop+pm_fast_esize(key)-3)=&
               key%data%i(key%offset:key%offset+pm_fast_esize(key)-4)
          coder%wtop=coder%wtop+pm_fast_esize(key)-3
          call code_null(coder)
          call code_val(coder,cnode_arg(val,2))
          call code_val(coder,cnode_arg(val,3))
          call code_val(coder,cnode_arg(val,4))
          do i=1,pm_dict_size(coder%context,nameset)
             procroot=procroots%data%ptr(procroots%offset+i-1)
             name=names%data%ptr(names%offset+i-1)
             sig=find_sig(coder,node,name,amplocs,&
                  nargs,nret,flags,sigvect,.false.,&
                  noerr=.true.,procroot=procroot)
             if(.not.pm_fast_isnull(sig)) then
                call code_val(coder,name)
                call code_val(coder,sig)
             endif
          enddo
          coder%wtop=lbase
          newk=cnode_num_arg(val,3)
          call make_code(coder,node,cnode_is_arglist,&
               coder%vtop-base)
          call pm_dict_set_val(coder%context,&
               coder%sig_cache,k,top_code(coder))
          call drop_code(coder)
          coder%vtop=base
          k=newk
       enddo
       if(nbase==pm_dict_size(coder%context,coder%proc_name_vals).and.&
            kbase==coder%call_sig) exit
    enddo
  contains
    include 'fname.inc'
    include 'fisnull.inc'
    include 'fesize.inc'
  end subroutine complete_vcall_sigs

  !===========================================
  ! Sort all defined signatures
  !===========================================
  subroutine sort_sigs(coder)
    type(code_state),intent(inout):: coder
    integer(pm_ln):: i
    type(pm_ptr):: vals,v,n
    if(debug_codegen) then
       write(*,*) 'SORT SIGS',pm_dict_size(coder%context,coder%sig_cache)
    endif
    vals=pm_dict_vals(coder%context,coder%sig_cache)
    do i=0,pm_dict_size(coder%context,coder%sig_cache)-1
       v=vals%data%ptr(vals%offset+i)
       if(cnode_flags_clear(v,cnode_args+1,proc_is_var)) then
          if(debug_codegen) then
             write(*,*) 'SORT SIG',i,&
                  pm_dict_size(coder%context,coder%sig_cache)-1,&
                  trim(sig_name_str(coder,int(i+1))),&
                  v%data%ptr(v%offset+cnode_args+1)%offset
          endif
          call sort_sig(coder,v,int(i+1))
       else if(debug_codegen) then
          write(*,*) 'SORT SIG SKIP',i,&
               pm_dict_size(coder%context,coder%sig_cache)-1,&
               trim(sig_name_str(coder,int(i+1))) 
       endif
    enddo
  end subroutine sort_sigs

  !=================================================
  ! Partial order sort for signature
  !=================================================
  subroutine sort_sig(coder,sig,signo)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: sig
    integer,intent(in):: signo
    integer:: start,end
    integer:: h,i,j,k,ii,rel,ret1,ret2,rtype1,rtype2
    integer:: typ1,typ2,typ3,inter,union
    type(pm_ptr):: code,pars,tv1,tv2
    logical:: ok,isbad
    type(pm_typ_einfo):: einfo
    start=sig%offset+cnode_args+2
    end=sig%offset+pm_fast_esize(sig)
    if(debug_codegen) write(*,*) 'SORT SIGNATURE>',start,end
    do i=end-3,start,-2
       if(debug_more_codegen) write(*,*) 'I=',i,i+2,end-1
       typ1=sig%data%ptr(i)%offset
       code=sig%data%ptr(i+1)
       j=i+2
       do while(j<=end-1)
          typ2=sig%data%ptr(j)%offset
          if(debug_codegen) then
             write(*,*) 'COMPARE SIGS>',typ1,typ2
             write(*,*) '--------------------------------------'
             write(*,*) trim(pm_typ_as_string(coder%context,typ2))
             write(*,*) trim(pm_typ_as_string(coder%context,typ1))
             write(*,*) '--------------------------------------'
          endif
          if(typ1==typ2) then
             call cnode_error(coder,code,&
                  'Procedure "'//trim(sig_name_str(coder,signo))//&
                  '" defined with identical signatures:'//&
                  trim(pm_typ_as_string(coder%context,typ2)))
             call cnode_error(coder,sig%data%ptr(j+1),&
                  'Conflicting definition')
             return
          else if(pm_typ_includes(coder%context,typ2,typ1,pm_typ_incl_typ,&
               einfo)) then
             if(debug_more_codegen) write(*,*) 'INCL'
             if(cnode_flags_clear(sig%data%ptr(j+1),&
                  pr_flags,proc_is_open)) then
                if(.not.(cnode_get(code,cnode_modl_name)==&
                     cnode_get(sig%data%ptr(j+1),cnode_modl_name))) then
                   call cnode_error(coder,code,&
                        'Procedure "'//trim(sig_name_str(coder,signo))//&
                        '" attempts to specialise procedure defined without "..." across modules')
                   call cnode_error(coder,sig%data%ptr(j+1),&
                        'Conflicting definition')
                endif
             endif

             ! This would implement the ".." internal-to-module extension 
             ! rule - not yet decided if this is a good idea
             
!!$             if(cnode_flags_clear(sig%data%ptr(j+1),&
!!$                  pr_flags,proc_is_extensible)) then
!!$                call cnode_error(coder,code,&
!!$                     'Procedure "'//trim(sig_name_str(coder,signo))//&
!!$                     '" attempts to specialise procedure defined without ".." or "..."')
!!$                call cnode_error(coder,sig%data%ptr(j+1),&
!!$                     'Conflicting definition')
!!$             endif
             
             ! This assumes pr_rtype == bi_rtype
             ret1=cnode_get_num(code,pr_rtype)
             ret2=cnode_get_num(sig%data%ptr(j+1),pr_rtype)
             tv1=pm_typ_vect(coder%context,ret1)
             tv2=pm_typ_vect(coder%context,ret2)
             if(pm_tv_kind(tv1)/=pm_typ_is_undef_result.and.&
                  pm_tv_kind(tv2)/=pm_typ_is_undef_result) then
                isbad=.false.
                do ii=1,pm_tv_numargs(tv1)
                   rtype1=pm_tv_arg(tv1,ii)
                   rtype2=pm_tv_arg(tv2,ii)
                   if(.not.pm_typ_includes(coder%context,&
                        rtype1,rtype2,pm_typ_incl_typ,einfo)) then
                      if(.not.isbad) then
                         call cnode_error(coder,code,&
                              'Procedure "'//trim(sig_name_str(coder,signo))//&
                              '" specialises a procedure with incompatible return types')
                      endif
                      call more_error(coder%context,'Return value #'//&
                           trim(pm_int_as_string(ii))//&
                           ' in original procedure has type: '//&
                           trim(pm_typ_as_string(coder%context,rtype1)))
                      call more_error(coder%context,&
                           'but in this procedure has type: '//&
                           trim(pm_typ_as_string(coder%context,rtype2)))
                      isbad=.true.

                      
                   endif
                enddo
                if(isbad) then
                   call cnode_error(coder,sig%data%ptr(j+1),&
                        'Original procedure in above error')
                endif
             endif
             
             exit
          else
             if(debug_more_codegen) write(*,*) 'NOT INCL'
             sig%data%ptr(j-2)=sig%data%ptr(j)
             sig%data%ptr(j-1)=sig%data%ptr(j+1)
             j=j+2
          endif
       enddo
       sig%data%ptr(j-2)%offset=typ1
       sig%data%ptr(j-1)=code
    enddo
   

    if(pm_debug_checks) then
       do i=start,end-1,2
          !write(*,*) '::',i,sig%data%ptr(i+1)%data%vkind
          if(sig%data%ptr(i+1)%data%vkind/=pm_pointer) then
             call pm_dump_tree(coder%context,6,sig%data%ptr(i+1),2)
             !write(*,*) sym_names(sig%data%ptr(i+1)%offset)
          else
             !call qqdump_code_tree(coder,pm_null_obj,6,sig%data%ptr(i+1),2)
          endif
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
         pm_new_typ(coder%context,coder%wstack(coder%wtop:coder%wtop+size-1),&
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
         pm_new_basic_typ(coder%context,coder%wstack(coder%wtop:coder%wtop+size-1),&
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
    deftyp=pm_typ_lookup(coder%context,coder%wstack(coder%wtop-n+1:coder%wtop))
    if(deftyp>=0) then
       new_type=-1
    else
       new_type=pm_new_typ(coder%context,coder%wstack(coder%wtop-n+1:coder%wtop),&
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
    else
       v=pm_null_obj
    endif
    return
  end function find_var

  !==========================================
  ! Find a variable and its table entry
  !==========================================
  function find_var_and_entry(coder,name,local,i) result(v)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: name
    logical,intent(out):: local
    integer,intent(out):: i
    type(pm_ptr):: v
    integer::n
    n=name%offset
    i=find_var_entry(coder,n,coder%proc_base)
    if(i/=0) then
       v=coder%var(i)
       local=i>coder%par_base
    else
       v=pm_null_obj
       local=.true.
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
    i=coder%hash(var_hash(n))
    if(pm_debug_checks) then
       if(i>coder%top) then
          write(*,*) '#',i,coder%top,var_hash(n)
          call pm_panic('bad hash')
       endif
    endif
    do while(i>base)
       if(coder%stack(i)==n) then
          index=i
          return
       endif
       i=coder%link(i)
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
  subroutine make_var(coder,cblock,node,name,flags,extra_info)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,name
    integer,intent(in):: flags
    type(pm_ptr),optional:: extra_info
    type(pm_ptr):: var,link
    logical:: local
    integer:: vflags
    
    ! Check for prior definition
    if(iand(flags,var_is_shadowed)==0) then
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
    if(present(extra_info)) then
       call code_val(coder,extra_info)
       call make_code(coder,node,cnode_is_var,var_node_size+1)
    else
       call make_code(coder,node,cnode_is_var,var_node_size)
    endif

    ! Add variable to hash table
    call make_var_tab_entry(coder,int(name%offset),top_code(coder))

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
  subroutine make_var_tab_entry(coder,name,var)
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
    i=var_hash(coder%stack(j))
    coder%link(j)=coder%hash(i)
    coder%hash(i)=j
  end subroutine make_var_tab_entry

  !=====================================
  ! Pop variables down to newbase
  !=====================================
  subroutine pop_vars_to(coder,newbase)
    type(code_state),intent(inout):: coder
    integer,intent(in):: newbase
    integer:: i,k
    integer:: j
    if(pm_debug_checks) then
       if(newbase>coder%top) call pm_panic('pop_vars_to')
    endif
    do i=coder%top,newbase+1,-1
       j=coder%stack(i)
       if(j/=0) then 
          if(pm_debug_checks) then
             if(j==typevar_start.or.j==typevar_end) then
                write(*,*) 'i=',i,j
                do j=coder%top,newbase+1,-1
                   write(*,*) j,coder%stack(j),&
                        trim(pm_name_as_string(coder%context,max(0,coder%stack(j))))
                enddo
                call pm_panic('Cannot pop typevar start/end')
             endif
          endif
          k=var_hash(abs(j))
          coder%hash(k)=coder%link(i)
       endif
    enddo
    coder%top=newbase
  end subroutine pop_vars_to

  !=======================================
  ! Variable hash from variable name
  !=======================================
  function var_hash(n) result(h)
    integer,intent(in):: n
    integer:: h
    h=iand(abs(int(n)),code_local_hash-1)+1
    h=1
  end function var_hash

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
    coder%temp=ptr
    call make_const(coder,cblock,node,ptr)
    coder%temp=pm_null_obj
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
    coder%temp=ptr
    call make_const(coder,cblock,node,ptr)
    coder%temp=pm_null_obj
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
    coder%temp=ptr
    call make_const(coder,cblock,node,ptr,pm_new_value_typ(coder%context,ptr))
    coder%temp=pm_null_obj
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
            coder%true_name)
    else
       call make_const(coder,cblock,node,coder%false,&
            coder%false_name)
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
    tno=pm_fast_typeof(val)
    if(tno==pm_string) tno=pm_string_type
    if(present(typ)) tno=typ
    if(coder%par_state/=par_state_outer) then
       tno=pm_typ_add_mode(coder%context,tno,sym_mirrored,.false.)
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
  subroutine make_sp_call(coder,cblock,node,sym,narg,nret,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret
    integer,intent(in),optional:: flags
    integer:: depth,base,aflags
    aflags=0
    if(present(flags)) aflags=flags
    call import_args(coder,cblock,node,narg,nret,0,pm_null_obj,0,base)
    call make_full_call(coder,cblock,node,&
         pm_fast_tinyint(coder%context,-sym),pm_null_obj,narg,nret,0,&
         ior(aflags,coder%run_flags),&
         pm_null_obj,coder%par_depth)
  contains
    include 'ftiny.inc'
  end subroutine make_sp_call

  !==================================================================
  ! Make a procedure call cnode for some builtin operations
  ! (does not create imports/exports)
  !==================================================================
  subroutine make_basic_sp_call(coder,cblock,node,sym,narg,nret,depth)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret,depth
     call make_full_call(coder,cblock,node,&
          pm_fast_tinyint(coder%context,-sym),pm_null_obj,&
          narg,nret,0,coder%run_flags,&
          pm_null_obj,depth)
  contains
    include 'ftiny.inc'
  end subroutine make_basic_sp_call

  !=============================================
  ! Make a call to an intrinsic procedure
  !=============================================
  subroutine make_sys_call(coder,cblock,node,sym,&
       narg,nret,aflags,assign)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret
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
       avec=coder%one
    else
       avec=pm_null_obj
    endif
    if(coder%par_state>=par_state_cond.and.&
         iand(flags,proc_run_complete+proc_run_shared+proc_run_local)==0) then
       flags=ior(flags,call_is_cond)
    endif
    call import_args(coder,cblock,node,narg,nret,0,avec,flags,base)
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,sym),&
         avec,narg,nret,flags,svect,.false.)
    call make_full_call(coder,cblock,node,&
         procs,avec,narg,nret,0,flags,pm_null_obj,coder%par_depth)
  contains
    include 'fname.inc'
  end subroutine make_sys_call

  !======================================================
  ! Make a call to an intrinsic communicating procedure
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
       flags=ior(aflags,call_is_comm)
    else
       flags=call_is_comm
    endif
    if(present(assign)) then
       avec=coder%comm_amp
    else
       avec=pm_null_obj
    endif
    call import_args(coder,cblock,node,narg,nret,0,avec,flags,base)
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,sym)&
         ,avec,narg,nret,flags,svect,.true.)
    call make_full_call(coder,cblock,node,&
         procs,avec,narg,nret,0,flags,pm_null_obj,coder%par_depth)
  contains
    include 'fname.inc'
  end subroutine make_comm_sys_call

  !====================================================================
  ! Make a call to an intrinsic procedure with & on first argument
  !====================================================================
  subroutine make_assign_call(coder,cblock,node,sym,narg,nret,aflags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret
    integer,intent(in),optional:: aflags
    call make_sys_call(coder,cblock,node,sym,narg,nret,&
         aflags=aflags,assign=.true.)
  end subroutine make_assign_call

  !=================================================
  ! Make a call to an intrinsic procedure
  ! with no processing of imports/exports
  !=================================================
  subroutine make_basic_sys_call(coder,cblock,node,sym,narg,nret,depth,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret,depth,flags
    type(pm_ptr):: procs,svect
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,sym),&
         pm_null_obj,narg,nret,0,svect,.false.)
    call make_full_call(coder,cblock,node,&
         procs,pm_null_obj,narg,nret,0,ior(flags,coder%run_flags),pm_null_obj,depth)
  contains
    include 'fname.inc'
  end subroutine make_basic_sys_call

  !==========================================
  ! Make a procedure call
  !==========================================
  subroutine make_full_call(coder,cblock,node,procs,&
       amp,narg,nret,nkeys,iflag,var,depth)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,procs,amp,var
    integer,intent(in):: narg,nret,nkeys
    integer,intent(in):: iflag,depth
    type(pm_ptr):: p,q,n,args
    integer:: i
    if(pm_debug_checks) then
       if(cnode_get_kind(cblock)/=cnode_is_cblock) &
            call pm_panic('full call cblock')
    endif
    if(iand(iflag,proc_run_shared+proc_run_local+proc_run_complete+proc_run_always)/=0) then
       if(coder%par_state==par_state_outer) then
          call code_error(coder,node,&
               'Cannot have "<<shared>>", "<<invar>>", "<<complete>>" or "<<always>>" attributes'//&
               ' outside of a parallel context')
       endif
    endif
    call make_code(coder,node,cnode_is_arglist,nret+nkeys+narg)
    args=top_code(coder)
    call code_val(coder,cblock)
    call code_val(coder,procs)
    call code_num(coder,iflag)
    call code_null(coder)
    call code_num(coder,nret)
    call code_num(coder,nkeys)
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    call code_num(coder,depth-coder%proc_par_depth)
    call code_val(coder,var)
    call code_val(coder,amp)
    call make_code(coder,node,cnode_is_call,call_node_size)
    n=top_code(coder)
    if(iand(iflag,call_is_no_touch)==0) then
       do i=nret+1,nret+nkeys+narg
          p=cnode_arg(args,i)
          if(pm_fast_vkind(p)==pm_pointer) then
             if(cnode_get_kind(p)==cnode_is_var) then
                if(cnode_flags_set(p,var_flags,var_is_accessed)) then
                   call cnode_set_flags(p,var_flags,var_is_multi_access)
                else
                   call cnode_set_flags(p,var_flags,var_is_accessed)
                endif
             endif
          endif
       enddo
    end if
    do i=1,nret
       p=cnode_arg(args,i)
    enddo
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
    call pop_vars_to(coder,&
         int(cblock%data%ptr(cblock%offset+cblock_start)%offset))
    p=cnode_get(cblock,cblock_last_loop_call)
    if(pm_fast_isnull(p)) then
       call cnode_set(coder,cblock,cblock_last_loop_call,&
            cnode_get(cblock,cblock_first_call))
    else
       call cnode_set(coder,cblock,cblock_last_loop_call,&
            cnode_get(p,call_link))
    endif
  contains
    include 'fisnull.inc'
  end subroutine close_cblock
  
  !===============================
  ! Make a code tree node (cnode)
  !===============================
  subroutine make_code(coder,node,ckind,nargs)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node
    integer,intent(in):: ckind,nargs
    type(pm_ptr):: modl
    integer:: i
    if(pm_debug_checks) then
       if(coder%vtop-nargs<0) call pm_panic('make code')
    endif
    coder%temp=pm_fast_newnc(coder%context,pm_pointer,&
         nargs+cnode_args)
    if(pm_debug_checks.and..false.) then
       if(coder%temp%data%ptr(coder%temp%offset)%offset&
            ==cnode_magic_no) then
          call qdump_code_tree(coder,pm_null_obj,6,coder%temp,2)
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
    coder%temp%data%ptr(coder%temp%offset+5:coder%temp%offset+4+nargs)=&
         coder%vstack(coder%vtop-nargs+1:coder%vtop)
    if(pm_debug_checks) then
       do i=coder%temp%offset+5,coder%temp%offset+4+nargs
   !       write(*,*) i-coder%temp%offset-3
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
  ! ... a b c  ->  ... c a b
  !======================================================
  subroutine swap_code_2_1(coder)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: temp
    temp=coder%vstack(coder%vtop)
    coder%vstack(coder%vtop)=coder%vstack(coder%vtop-1)
    coder%vstack(coder%vtop-1)=coder%vstack(coder%vtop-2)
    coder%vstack(coder%vtop-2)=temp
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

  !=================================
  ! Check cnode (debugging)
  !=================================
  subroutine check_cnode(ptr,n)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer(pm_p):: m
    if(.not.pm_fast_vkind(ptr)==pm_pointer) then
       write(*,*) 'Kind=',ptr%data%vkind
       call pm_panic('cnode not ptr')
    endif
    if(ptr%data%ptr(ptr%offset)%offset/=cnode_magic_no) then
       call pm_panic('bad cnode magic no')
    endif
    m=ptr%data%ptr(ptr%offset+1)%offset
    if(m<1.or.m>cnode_num_kinds) &
         call pm_panic('cnode bad kind')
    if(n<0.or.n>pm_fast_esize(ptr)) &
         call pm_panic('bad cnode offset')
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end subroutine check_cnode

  !==========================================
  ! Get argument n from cnode
  !==========================================
  function cnode_arg(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr):: val
    if(debug_codegen) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+cnode_args+n-1)
  end function cnode_arg

  !======================================
  ! Get element n from cnode
  !======================================
  function cnode_get(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr):: val
    if(debug_codegen) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)
  end function cnode_get

  !====================================
  ! Set element n of cnode
  !====================================
  subroutine cnode_set(coder,ptr,n,val)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr),intent(in):: val
    if(debug_codegen) call check_cnode(ptr,n)
    call pm_ptr_assign(coder%context,ptr,int(n,pm_ln),val)
  end subroutine  cnode_set

  ! ========================================
  ! Get element n from cnode as a number
  !=========================================
  function cnode_get_num(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer:: val
    if(debug_codegen) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
  end function cnode_get_num

  !============================================
  ! Get argument n from cnode as a number
  !============================================
  function cnode_num_arg(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer:: val
    if(debug_codegen) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n+cnode_args-1)%offset
  end function cnode_num_arg

  !=========================================
  ! Get element n from cnode as a name
  !=========================================
  function cnode_get_name(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer:: val
    if(debug_codegen) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
  end function cnode_get_name

  !==========================================
  ! Set element n in cnode to a new number
  ! (must be number already)
  !==========================================
  subroutine cnode_set_num(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer,intent(in):: val
    if(debug_codegen) call check_cnode(ptr,n)
    ptr%data%ptr(ptr%offset+n)%offset=val
  end subroutine  cnode_set_num

  !==========================================
  ! Increment argument n from cnode
  !==========================================
  subroutine cnode_incr_num(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer,intent(in):: val
    if(debug_codegen) call check_cnode(ptr,n)
    ptr%data%ptr(ptr%offset+n)%offset=&
         ptr%data%ptr(ptr%offset+n)%offset+val
  end subroutine  cnode_incr_num

  !==============================================
  ! Set given flags in an element of a cnode
  ! (bitwise or of existing number)
  !==============================================
  subroutine cnode_set_flags(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer,intent(in):: val
    type(pm_ptr):: p
    if(debug_codegen) then
       call check_cnode(ptr,n)
       p=ptr%data%ptr(ptr%offset+n)
       if(pm_fast_vkind(p)/=pm_tiny_int.and.pm_fast_vkind(p)/=pm_null) then
          write(*,*) 'vkind=',pm_fast_vkind(ptr)
          call pm_panic('Set flags')
       endif
       if(cnode_get_kind(ptr)==cnode_is_var.and.&
            n/=var_flags.or.&
            cnode_get_kind(ptr)==cnode_is_cblock.and.n/=cblock_flags.or.&
            cnode_get_kind(ptr)==cnode_is_call.and.n/=call_flags) then
          call pm_panic('set flags')
       endif
    endif
    ptr%data%ptr(ptr%offset+n)%offset=ior(&
         ptr%data%ptr(ptr%offset+n)%offset,int(val,pm_p))
  contains
    include 'fvkind.inc'
  end subroutine  cnode_set_flags

  !============================================
  ! Clear flags in element of a code code
  ! (Bitwise clear of exiting number)
  !============================================
  subroutine cnode_clear_flags(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer,intent(in):: val
    type(pm_ptr):: p
    if(debug_codegen) then
       call check_cnode(ptr,n)
       p=ptr%data%ptr(ptr%offset+n)
       if(pm_fast_vkind(p)/=pm_tiny_int.and.pm_fast_vkind(p)/=pm_null) then
          write(*,*) 'vkind=',pm_fast_vkind(ptr)
          call pm_panic('Set flags')
       endif
       if(cnode_get_kind(ptr)==cnode_is_var.and.&
            n/=var_flags.or.&
            cnode_get_kind(ptr)==cnode_is_cblock.and.n/=cblock_flags.or.&
            cnode_get_kind(ptr)==cnode_is_call.and.n/=call_flags) then
          call pm_panic('set flags')
       endif
    endif
    ptr%data%ptr(ptr%offset+n)%offset=iand(&
         ptr%data%ptr(ptr%offset+n)%offset,not(int(val,pm_p)))
  contains
    include 'fvkind.inc'
  end subroutine  cnode_clear_flags

  !===================================================================
  ! Check all given flags in a given element of a cnode are clear
  ! (bitwise and of extisting number and check for zero)
  !===================================================================
  function cnode_flags_clear(ptr,n,flags) result(ok)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n,flags
    logical:: ok
    integer:: val
    if(debug_codegen) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
    ok=iand(flags,val)==0
  end function cnode_flags_clear

  !============================================================
  ! Check all given flags in an element of a cnode are set
  ! (bitwise and)
  !============================================================
  function cnode_flags_set(ptr,n,flags) result(ok)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n,flags
    logical:: ok
    integer(pm_p):: val
    if(debug_codegen) call check_cnode(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
    ok=iand(flags,int(val))==flags
  end function cnode_flags_set

  !==========================================
  ! Return kind of a cnode
  !==========================================
  function cnode_get_kind(ptr) result(n)
    type(pm_ptr),intent(in):: ptr
    integer:: n
    if(pm_debug_checks) call check_cnode(ptr,0)
    n=ptr%data%ptr(ptr%offset+1)%offset
  end function cnode_get_kind

  !========================================
  ! Return number of arguments of a cnode
  !========================================
  function cnode_numargs(ptr) result(n)
    type(pm_ptr),intent(in):: ptr
    integer:: n
    if(debug_codegen) call check_cnode(ptr,0)
    n=pm_fast_esize(ptr)-cnode_args+1
  contains
    include 'fesize.inc'
  end function cnode_numargs


  !========================================
  ! Does a cblock contain any communicating
  ! operations?
  !========================================
  function cblock_has_comm(cblock) result(ok)      
    type(pm_ptr):: cblock
    logical:: ok
    ok=(iand(cnode_get_num(cblock,cblock_flags),&
         cblock_is_comm)/=0)
  end function cblock_has_comm
    
  
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
       write(iunit,'(A,A,"      Idx=",I4," Dep=",I4," Chng=",L1," Flags=",I4)') &
            spaces(1:depth*2),trim(str),&
            cnode_get_num(node,var_index),&
            cnode_get_num(node,var_par_depth),&
            cnode_flags_set(node,var_flags,var_is_changed),&
            cnode_get_num(node,var_flags)
       if(.not.pm_fast_isnull(rvec)) then
          i= rvec%data%i(rvec%offset+&
               cnode_get_num(node,var_index))
          if(i<0) then
             write(iunit,*) spaces(1:depth*2),' Unresolved!!'
          else
             write(iunit,*) spaces(1:depth*2),' Resolved:',i,&
                  trim(pm_typ_as_string(coder%context,i))
          endif
       endif
    case(cnode_is_const)
       call pm_dump_tree(coder%context,iunit,cnode_arg(node,1),depth)
!!$       write(iunit,*)  spaces(1:depth*2),&
!!$            trim(pm_typ_as_string(coder%context,&
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
       write(iunit,*) spaces(1:depth*2),'Builtin ',&
            op_names(cnode_get_num(node,cnode_args)),&
            cnode_get_num(node,cnode_args+1),'('
       if(.not.pm_fast_isnull(cnode_get(node,bi_rcode))) then
          call qdump_code_tree(coder,rvec,iunit,&
               cnode_get(node,bi_rcode),depth+1)
       endif
       write(iunit,*) spaces(1:depth*2),')'
    case(cnode_is_proc)
       write(iunit,'(A,A,i2,A,i2,A,i2,A,i3,A)') spaces(1:depth*2),&
            'Proc [nargs=',&
            cnode_get_num(node,pr_nargs),',nkeys=',&
            cnode_get_num(node,pr_nkeys),',nret=',cnode_get_num(node,pr_nret),&
            ',ncalls=',cnode_get_num(node,pr_ncalls),'] ('
       if(cnode_flags_set(node,pr_flags,proc_is_comm)) &
            write(iunit,*) spaces(1:depth*2+1),'[loop]'
       if(cnode_flags_set(node,pr_flags,proc_is_each_proc)) &
            write(iunit,*) spaces(1:depth*2+1),'[each]'
       if(cnode_flags_set(node,pr_flags,proc_is_dup_each)) &
            write(iunit,*) spaces(1:depth*2+1),'[dup-each]'
       if(cnode_flags_set(node,pr_flags,proc_is_thru_each)) &
            write(iunit,*) spaces(1:depth*2+1),'[thru-each]'
       if(cnode_flags_set(node,pr_flags,proc_is_empty_each)) &
            write(iunit,*) spaces(1:depth*2+1),'[empty-each]'
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
       if(cnode_flags_set(node,cnode_args+2,proc_is_not_pure_each)) &
            write(iunit,*) spaces(1:depth*2+1),'[not pure each]'
       call qdump_code_tree(coder,cnode_arg(node,2),&
            iunit,cnode_arg(node,1),depth+1)
        write(iunit,*) spaces(1:depth*2),')'
     case(cnode_is_arglist)
        write(iunit,*) spaces(1:depth*2),'Var Sig List(',cnode_numargs(node)
        if(cnode_flags_set(node,cnode_args+1,proc_is_var)) then
           do i=5,cnode_numargs(node),2
              write(iunit,*) spaces(1:depth*2),&
                   trim(pm_name_as_string(coder%context,&
                   cnode_get_name(node,cnode_args+i-1))),'-->',&
                   cnode_get_num(node,cnode_args+i)
           enddo
        else
           write(iunit,*) spaces(1:depth*2),'Sig List(',cnode_numargs(node)
           do i=2,cnode_numargs(node),2
              write(iunit,*) spaces(1:depth*2),trim(pm_typ_as_string(coder%context,&
                   cnode_get_num(node,cnode_args+i-1)))
              call qdump_code_tree(coder,rvec,iunit,cnode_arg(node,i+1),depth+1)
           enddo
        endif
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
    key=pm_dict_key(coder%context,coder%sig_cache,int(m,pm_ln))
    name=key%data%i(key%offset+pm_fast_esize(key))
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
    call pm_name_string(coder%context,sig_name(coder,m),str)
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
       elseif(cnode_flags_set(code,cnode_args+1,proc_is_var)) then
          do j=3,cnode_numargs(code),2
             write(iunit,*) trim(pm_name_as_string(coder%context,&
                  cnode_get_name(code,j+cnode_args-1)))
             write(iunit,*) cnode_get_num(code,j+cnode_args)
          enddo
       else
          do j=3,cnode_numargs(code),2
             typ=cnode_arg(code,j)
             write(iunit,*) 'Type:',trim(pm_typ_as_string(coder%context,&
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
    if(iand(flags,call_is_comm)/=0) then
       str(n:n)='%'
       n=n+1
    endif
    if(present(args).and.&
         iand(flags,proc_is_comm)/=0) then
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
         iand(flags,proc_is_comm)/=0&
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
             str=trim(pm_opts%error)//trim(message)//' '//trim(str)
          else
             str=trim(pm_opts%error)//message
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
          str=trim(pm_opts%error)//trim(message)//' '//trim(str)
       else
          str=trim(pm_opts%error)//trim(message)
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

