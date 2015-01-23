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


!================================================
! The following routines process the parse tree
! into a structure much closer to the final code
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
  integer,parameter:: max_ambig_type=128
  
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
  integer,parameter:: cnode_num_kinds=9

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
  integer,parameter:: cblock_loop_invar=2

  ! Offsets into call cnodes
  integer,parameter:: call_args=cnode_args+0
  integer,parameter:: call_parent=cnode_args+1
  integer,parameter:: call_sig=cnode_args+2
  integer,parameter:: call_flags=cnode_args+3
  integer,parameter:: call_link=cnode_args+4
  integer,parameter:: call_nret=cnode_args+5
  integer,parameter:: call_nkeys=cnode_args+6
  integer,parameter:: call_index=cnode_args+7
  integer,parameter:: call_node_size=8

  ! Flags for call nodes
  integer,parameter:: call_is_reduce = 1
  integer,parameter:: call_is_loop_call = 2
  integer,parameter:: call_is_vararg = 4

  ! Offsets into var cnodes
  integer,parameter:: var_parent=cnode_args+0
  integer,parameter:: var_name=cnode_args+1
  integer,parameter:: var_flags=cnode_args+2
  integer,parameter:: var_link=cnode_args+3
  integer,parameter:: var_index=cnode_args+4
  integer,parameter:: var_loop=cnode_args+5
  integer,parameter:: var_assign_call=cnode_args+6
  integer,parameter:: var_node_size=7

  ! Flags for var cnodes
  integer(pm_p),parameter:: var_const=1
  integer(pm_p),parameter:: var_ref=2
  integer(pm_p),parameter:: var_param=4
  integer(pm_p),parameter:: var_amp=8
  integer(pm_p),parameter:: var_iter=16
  integer(pm_p),parameter:: var_changed=32
  integer(pm_p),parameter:: var_shadow=64
  integer(pm_p),parameter:: var_imported=128
  integer(pm_p),parameter:: var_undefined=256

  ! Offsets into proc nodes
  integer,parameter:: pr_cblock=cnode_args+0
  integer,parameter:: pr_max_index=cnode_args+1
  integer,parameter:: pr_recurse=cnode_args+2
  integer,parameter:: pr_id=cnode_args+3
  integer,parameter:: pr_nargs=cnode_args+4
  integer,parameter:: pr_nret=cnode_args+5
  integer,parameter:: pr_flags=cnode_args+6
  integer,parameter:: pr_node_size=7

  ! Offsets into builtin nodes
  integer,parameter:: bi_opcode=cnode_args+0
  integer,parameter:: bi_opcode2=cnode_args+1
  integer,parameter:: bi_data=cnode_args+2
  integer,parameter:: bi_rtype=cnode_args+3
  integer,parameter:: bi_rcode=cnode_args+4
  integer,parameter:: bi_rsym=cnode_args+5
  integer,parameter:: bi_id=cnode_args+6
  integer,parameter:: bi_node_size=7
  
  ! Flags indicating start/end of a block of type variables
  ! as opposed to regular variables on variables stack
  integer,parameter:: typevar_start=-88
  integer,parameter:: typevar_end=-99

  ! Maximum number of coding errors before exit
  integer,parameter:: max_code_errors = 20

  ! State of the code generator
  type code_state
     ! Link to memory management
     type(pm_context),pointer:: context
     type(pm_reg),pointer:: reg,reg2
     ! Stack for variables
     integer(pm_i16),dimension(max_code_stack):: stack
     type(pm_ptr),dimension(max_code_stack):: var
     integer:: top
     ! Stack of values for creating cnodes
     type(pm_ptr),dimension(max_code_stack):: vstack
     integer:: vtop
     ! Stack of types
     integer(pm_i16),dimension(max_code_stack):: tstack
     integer:: ttop
     ! Caches for call signatures and resolved procedures
     type(pm_ptr):: sig_cache,proc_cache
     ! Misc info on procedures (particularly proc=)
     type(pm_ptr)::assign_proc
     integer(pm_p):: assign_sig,subseq_sig,id
     ! Misc values
     type(pm_ptr):: temp,true,false
     ! Contextual information for this point in the traverse
     logical:: loop_is_par
     type(pm_ptr):: sub_array,loop_cblock
     integer:: proc_base,loop_base,sub_index
     integer(pm_p):: index
     logical:: loop_local
     ! Error count
     integer:: num_errors
  end type code_state

contains


  ! Initialise code generator structure
  subroutine init_coder(context,coder)
    type(pm_context),pointer:: context
    type(code_state),intent(out):: coder
    type(pm_ptr):: sig
    coder%context=>context
    coder%reg=>pm_register(context,'coder-var stack',coder%temp,&
         coder%sig_cache,coder%proc_cache,coder%true,coder%false,&
         array=coder%var,array_size=coder%top)
    coder%reg2=>pm_register(context,'coder-node stack',&
         array=coder%vstack,array_size=coder%vtop)
    coder%sig_cache=pm_dict_new(context,32_pm_ln)
    coder%proc_cache=pm_dict_new(context,32_pm_ln)
    coder%proc_base=0
    coder%sub_index=0
    coder%loop_base=0
    coder%loop_local=.false.
    coder%sub_array=pm_null_obj
    coder%loop_cblock=pm_null_obj
    coder%index=0
    coder%true=pm_new_small(context,pm_logical,1_pm_p)
    coder%true%data%l(coder%true%offset)=.true.
    coder%false=pm_new_small(context,pm_logical,1_pm_p)
    coder%false%data%l(coder%false%offset)=.false.
    coder%assign_sig=-1
    coder%subseq_sig=-1
    coder%assign_proc=pm_null_obj
    coder%id=0
  contains
    include 'fname.inc'
  end subroutine init_coder

  ! Finalise and delete code generator
  subroutine delete_coder(coder)
    type(code_state),intent(inout):: coder
    call pm_delete_register(coder%context,coder%reg)
    call pm_delete_register(coder%context,coder%reg2)
  end subroutine delete_coder

  ! Start traversing the program
  subroutine trav_prog(coder,statlist)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: statlist
    type(pm_ptr):: sv,amp,sig
    
    ! Initialise assignment, definition 
    ! and element access signatures
    amp=pm_fast_newnc(coder%context,pm_int16,1_pm_p)
    amp%data%i16(amp%offset)=1
    call code_val(coder,amp)
    amp%offset=pm_set_add(coder%context,coder%context%names,amp)
    sig=find_sig(coder,statlist,pm_fast_name(coder%context,&
         int(sym_assign,pm_p)),amp,2,0,0,sv)
    coder%assign_sig=sig%offset
    sig=find_sig(coder,statlist,pm_fast_name(coder%context,&
         int(sym_subseq,pm_p)),amp,3,0,0,sv)
    coder%subseq_sig=sig%offset
    call drop_code(coder)
    
    ! Traverse statement list
    call trav_statlist(coder,pm_null_obj,statlist,statlist,sym_do)
  contains
    include 'fnewnc.inc'
    include 'fname.inc'
  end subroutine trav_prog

  ! Traverse statement list (closed variable scope)
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

  ! Traverse open list of statements (not necessarily variable scope)
  recursive subroutine trav_open_statlist(coder,cblock,&
       listp,list)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,listp,list
    integer:: i,j,sym,base,save_loop_base,vbase
    type(pm_ptr):: node,cblock2,save_loop_cblock
    if(pm_fast_isnull(list)) goto 10
    do i=1,node_numargs(list)
       vbase=coder%vtop
       node=node_arg(list,i)
       sym=node_sym(node)
       if(pm_debug_level>2) then
          write(*,*) 'Traverse-->',sym_names(sym),coder%vtop,vbase
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
          call make_call(coder,cblock,node,&
               sym_if,3,0)
       case(sym_while)
          cblock2=make_cblock(coder,cblock,node,sym_while)
          call trav_xexpr(coder,cblock2,node,&
               node_arg(node,1))
          call close_cblock(coder,cblock2)
          call trav_statlist(coder,cblock,node,&
               node_arg(node,2),sym_while)
          call make_call(coder,cblock,node,sym_while,3,0)
       case(sym_repeat)
          cblock2=make_cblock(coder,cblock,node,sym_repeat)
          call trav_open_statlist(coder,cblock2,node,&
               node_arg(node,1))
          call trav_xexpr(coder,cblock2,node,node_arg(node,2))
          call make_call(coder,cblock,node,&
               sym_repeat,2,0)
          call close_cblock(coder,cblock2)
       case(sym_par_loop,sym_par_find)
          save_loop_base=coder%loop_base
          save_loop_cblock=coder%loop_cblock
          ! Use this to pass in node for processing in trav_xexpr
          coder%loop_cblock=node 
          call trav_xexpr(coder,cblock,node,node_arg(node,1),node)
          coder%loop_base=save_loop_base
          coder%loop_cblock=save_loop_cblock
       case(sym_loop,sym_find)
          call trav_xexpr(coder,cblock,node,node_arg(node,1),node)
       case(sym_call)
          call trav_call(coder,cblock,list,node,0)
       case(sym_assign,sym_define,sym_arrow)
          call trav_assign(coder,cblock,list,node,sym)
       case(sym_const,sym_let)
          call trav_assign_list(coder,cblock,list,node)
       case(sym_found)
          cblock2=make_cblock(coder,cblock,node,sym_found)
          call trav_assign_list(coder,cblock2,list,node)
          call close_cblock(coder,cblock2)
          call make_call(coder,cblock,node,sym_found,1,0)
       case(sym_where,sym_check)
          call trav_xexpr(coder,cblock,listp,node)
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
    enddo
10  continue
  contains
    include 'fisnull.inc'
  end subroutine trav_open_statlist

  ! Traverse assignment list in parse tree
  recursive subroutine trav_assign_list(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer:: i
    do i=1,node_numargs(node)
       call trav_assign(coder,cblock,node,node_arg(node,i),&
            node_sym(node))
    enddo
  end subroutine trav_assign_list

  ! Traverse single assignment in parser tree
  recursive subroutine trav_assign(coder,cblock,pnode,node,sym)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    integer,intent(in):: sym
    integer:: i,n,base,nsym
    integer(pm_p):: flag
    type(pm_ptr):: lhs,var,p,arg
    logical:: save_loop_local,confine_assign
   
    nsym=node_sym(node)
    n=node_numargs(node)-1
    save_loop_local=coder%loop_local
    coder%loop_local=.false.
    confine_assign=.false.
    if(nsym==sym_let.or.nsym==sym_assign) then
       if(n>1) then
          base=coder%vtop
          do i=1,n
             call make_temp_var(coder,cblock,node)
          enddo
          do i=1,n
             call code_val(coder,coder%vstack(base+i))
          enddo
       endif
    else
       flag=0
       if(nsym==sym_arrow) flag=ior(flag,var_ref)
       if(sym==sym_const) flag=ior(flag,var_const)
       if(n>1) then
          base=coder%vtop
          do i=1,n
             call make_var(coder,cblock,node,&
                  node_arg(node,i),flag)
             call make_temp_var(coder,cblock,node)
          enddo
          do i=1,n
             call code_val(coder,coder%vstack(base+i*2))
          enddo
       else
          call make_var(coder,cblock,node,&
                  node_arg(node,1),flag)
       endif
    endif
 
    coder%loop_local=.true.
    if(n>1) then
       call trav_call(coder,cblock,node,&
            node_arg(node,node_numargs(node)),&
            node_numargs(node)-2)
    else
       call trav_expr(coder,cblock,node,&
            node_arg(node,node_numargs(node)))
    endif

    if(nsym==sym_arrow) then
       call make_call(coder,cblock,node,sym_arrow,2,0)
    else 
       if(nsym==sym_let.or.nsym==sym_assign) then
          do i=1,n
             arg=node_arg(node,i)
             if(node_sym(arg)==sym_dot) then
                call trav_lhs(coder,cblock,node,node_arg(arg,1))
                call swap_code(coder)
                call make_const(coder,cblock,node,node_arg(arg,2))
                call make_call(coder,cblock,node,sym_dotref,2,1)
             else if(node_sym(arg)==sym_open_square) then
                call trav_lhs(coder,cblock,node,node_arg(arg,1))
                call swap_code(coder)
                call trav_index_list(coder,cblock,arg)
                call make_full_call(coder,cblock,node,&
                     pm_fast_tinyint(coder%context,coder%subseq_sig),&
                     node_numargs(arg)+1,0,0,0)
             else
                call trav_lhs(coder,cblock,node,arg)
                call swap_code(coder)
                call make_full_call(coder,cblock,node,&
                     pm_fast_tinyint(coder%context,coder%assign_sig),2,0,0,0)
                if(cnode_is_tempvar(arg)) &
                     call cnode_set(arg,var_assign_call,&
                     cnode_get(cblock,cblock_last_call))
             endif
          enddo
          if(confine_assign.and.coder%loop_local) then
             call code_error(coder,node,&
                  'Cannot assign loop-local value to non loop-local variable')
          endif
       else
          do i=1,n
             arg=top_code(coder)
             call make_call(coder,cblock,node,sym_define,2,0)
             if(cnode_is_tempvar(arg)) &
                  call cnode_set(arg,var_assign_call,&
                  cnode_get(cblock,cblock_last_call))
          enddo
       endif
    endif
  contains
    include 'ftiny.inc'
    include 'fisname.inc'
    include 'fisnull.inc'
  end subroutine trav_assign

  ! Traverse a left hand side
  recursive subroutine trav_lhs(coder,cblock,pnode,lhs)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,lhs
    type(pm_ptr):: var,p
    integer:: sym
    if(pm_fast_isname(lhs)) then
       var=find_var(coder,lhs,base=coder%loop_base)
       if(pm_fast_isnull(var)) then
          var=find_var(coder,lhs,top=coder%loop_base)
       else
          coder%loop_local=.true.
       endif
       if(.not.pm_fast_isnull(var)) then
          if(iand(cnode_get_num(var,var_flags),var_const)/=0) then
             call code_error(coder,pnode,&
                  'Cannot assign to:',cnode_get(var,var_name))
          endif
          call cnode_set_flags(var,var_flags,var_changed)
          call code_val(coder,var)
       else
          call code_error(coder,pnode,&
               'Variable being assigned to has not been defined:',lhs)
          call make_temp_var(coder,cblock,pnode)
       endif
    else 
       sym=node_sym(lhs)
       select case(sym)
       case(sym_dot)
          call make_temp_var(coder,cblock,lhs)
          call trav_lhs(coder,cblock,lhs,node_arg(lhs,1))
          call make_const(coder,cblock,lhs,node_arg(lhs,2))
          call make_call(coder,cblock,lhs,sym_dot,2,1)
       case(sym_open_square)
          call make_temp_var(coder,cblock,lhs)
          call trav_lhs(coder,cblock,lhs,node_arg(lhs,1))
          call trav_index_list(coder,cblock,lhs)
          call make_sys_call(coder,cblock,lhs,sym_open_square,node_numargs(lhs),1)
       case default
          call code_error(coder,pnode,&
               'Cannot assign to expression')
       end select
    endif
  contains
    include 'fisname.inc'
    include 'fisnull.inc'
    include 'ftiny.inc'
  end subroutine trav_lhs

  ! Index list
  subroutine trav_index_list(coder,cblock,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer:: i,save_sub_index
    logical:: save_loop_local
    type(pm_ptr):: save_sub_array,lhs
    lhs=node_arg(node,2)
    save_loop_local=coder%loop_local
    save_sub_index=coder%sub_index
    save_sub_array=coder%sub_array
    coder%sub_array=top_code(coder)
    do i=1,node_numargs(lhs)
       coder%sub_index=i
       call trav_expr(coder,cblock,&
            lhs,node_arg(lhs,i))
    enddo
    coder%sub_index=save_sub_index
    coder%sub_array=save_sub_array
    coder%loop_local=save_loop_local
  end subroutine trav_index_list

  ! Traverse extended expression in a statement
  recursive subroutine trav_xexpr(coder,cblock,exprp,exprn,stmt)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,exprp,exprn
    type(pm_ptr),intent(in),optional:: stmt
    type(pm_ptr)::p,pp
    integer:: i,base,top
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
       p=node_arg(p,2)
       do i=1,node_numargs(p)
          pp=node_arg(p,i)
          if(node_sym(pp)==sym_query_assign) then
             call trav_expr(coder,cblock,pp,node_arg(pp,1))
             call trav_expr(coder,cblock,pp,node_arg(pp,2))
             call make_call(coder,cblock,pp,sym_check,2,0)
          else
             call trav_expr(coder,cblock,p,pp)
             call make_call(coder,cblock,pp,sym_check,1,0)
          endif
       enddo
    else
       call apply_x(exprp,p)
    endif
    if(base>=0) call delete_vars(coder,base+1,top)
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
      if(pm_fast_isnull(q)) return
      select case(node_sym(q))
      case(sym_const,sym_found,sym_let)
         call trav_assign_list(coder,cblock,qp,q)
      case(sym_assign,sym_define,sym_arrow)
         call trav_assign(coder,cblock,qp,q,node_sym(q))
      case(sym_iter)
         sym=node_sym(stmt)
         nq=node_numargs(q)
         lbase=coder%vtop
         do i=nq/2+1,nq
            fol=node_arg(q,i)
            if(node_sym(fol)==sym_from) then
               call trav_from_follow(coder,cblock,q,fol)
            else
               call trav_expr(coder,cblock,q,fol)
            endif
         enddo
         if(base>=0) then
            do i=base+1,coder%top
               coder%stack(i)=-coder%stack(i)
            enddo
            nx=coder%top
         endif
         if(sym==sym_par_loop.or.sym==sym_par_find) then
            old_proc_grid=find_var(coder,pm_fast_name(coder%context,sym_proc_grid))
            if(pm_fast_isnull(old_proc_grid)) then
               call make_var(coder,cblock,q,&
                    pm_fast_name(coder%context,int(sym_proc_grid,pm_p)),&
                    var_shadow)
               old_proc_grid=top_code(coder)
               call make_sys_call(coder,cblock,q,sym_num_procs,0,1)
            endif
            cblock2=make_cblock(coder,cblock,q,sym_for)
            coder%loop_base=coder%top
            coder%loop_cblock=cblock2
            call make_var(coder,cblock,q,&
                 pm_fast_name(coder%context,int(sym_in,pm_p)),&
                 ior(var_shadow,var_iter))
            call make_var(coder,cblock,q,&
                 pm_fast_name(coder%context,int(sym_hash,pm_p)),&
                 ior(var_shadow,var_iter))
            call make_var(coder,cblock,q,&
                 pm_fast_name(coder%context,int(sym_proc_grid,pm_p)),&
                 ior(var_shadow,var_iter))
            call make_var(coder,cblock,q,&
                 pm_fast_name(coder%context,int(sym_includes,pm_p)),&
                 ior(var_shadow,var_iter))
            call code_val(coder,old_proc_grid)
            do i=1,nq/2
               call dup_expr(coder,coder%vstack(lbase+i))
            enddo
            call make_sys_call(coder,cblock,q,sym_partition,1+nq/2,4)
            call make_var(coder,cblock2,q,&
                 pm_fast_name(coder%context,int(sym_iter,pm_p)),&
                 ior(var_shadow,var_iter))
            do i=1,nq/2
               call make_var(coder,cblock2,q,&
                    node_arg(q,i),var_iter)
               call code_val(coder,coder%vstack(lbase+i))
               call code_val(coder,coder%var(coder%loop_base+1))
               call code_val(coder,coder%var(coder%loop_base+5))
               call make_sys_call(coder,cblock2,q,sym_import,3,1)
            enddo
            coder%loop_cblock=cblock2
            call trav_statlist(coder,cblock2,stmt,node_arg(stmt,3),sym_do)
            call make_call(coder,cblock2,q,sym_do,1,0)
            j=0
            do i=1,nq/2
               var=coder%var(coder%loop_base+i+5)
               if(iand(cnode_get_num(var,var_flags),var_changed)&
                    /=0) then
                  call push_type(coder,int(i,pm_i16))
                  call code_val(coder,coder%vstack(lbase+i))
                  call code_val(coder,var)
                  call code_val(coder,coder%var(coder%loop_base+1))
                  call code_val(coder,coder%var(coder%loop_base+5))
                  call make_sys_call(coder,cblock2,q,sym_export,4,0)
                  j=j+1
               endif
            enddo
            call close_cblock(coder,cblock2)
            call code_val(coder,cblock2)
            if(sym==sym_par_find) then
               call trav_statlist(coder,cblock,stmt,node_arg(stmt,4),sym_otherwise)
               k=4
            else
               k=3
            endif
            call code_val(coder,coder%var(coder%loop_base+2))
            call code_val(coder,coder%var(coder%loop_base+4))
            call make_call(coder,cblock,q,sym,k,0)
            if(j>0) then
               if(base>=0) then
                  do i=base+1,nx
                     coder%stack(i)=-coder%stack(i)
                  enddo
               endif
               do i=1,j
                  k=pop_type(coder)
                  call code_val(coder,coder%vstack(lbase+k))
                  call code_val(coder,coder%var(coder%loop_base+1))
                  call code_val(coder,coder%var(coder%loop_base+3))
                  call code_val(coder,old_proc_grid)
                  call make_sys_call(coder,cblock,q,sym_sync,4,0)
               enddo
            endif
            coder%vtop=lbase
         else
            call repl_exprs(coder,nq/2)
            call make_sys_call(coder,cblock,q,&
                 sym_check_conform,nq/2,0)
            call make_temp_var(coder,cblock,q)
            call dup_code(coder)
            call dup_expr(coder,coder%vstack(coder%vtop-2))
            call make_sys_call(coder,cblock,q,sym_num_elements,1,1)
            call make_var(coder,cblock,q,&
                 pm_fast_name(coder%context,int(sym_dollar,pm_p)),var_shadow)
            call swap_code(coder)
            call make_call(coder,cblock,q,sym_define,1,1)
            call make_var(coder,cblock,q,&
                 pm_fast_name(coder%context,int(sym_iter,pm_p)),var_shadow)
            iter=coder%top
            cblock2=make_cblock(coder,cblock,q,sym_seq)
            do i=1,nq/2
               call make_var(coder,cblock2,q,&
                    node_arg(q,i),0_pm_p)
               call code_val(coder,coder%vstack(lbase+i))
               call code_val(coder,coder%var(iter))
               call make_sys_call(coder,cblock2,q,sym_get_element,2,1)
            enddo
            call code_val(coder,coder%var(iter))
            call code_val(coder,coder%var(iter-1))
            call code_val(coder,cblock2)
            if(sym==sym_find) then
               call trav_statlist(coder,cblock,stmt,node_arg(stmt,4),sym_otherwise)
               call make_call(coder,cblock,q,sym_find,4,0)
            else
               call make_call(coder,cblock,q,sym_loop,3,0)
            endif
            call trav_open_statlist(coder,cblock2,stmt,node_arg(stmt,3))
            do i=1,nq/2
               var=find_var(coder,node_arg(q,i))
               if(iand(cnode_get_num(var,var_flags),var_changed)&
                    /=0) then
                  call trav_lhs(coder,cblock2,q,node_arg(q,i+nq/2))
                  call code_val(coder,var)
                  call code_val(coder,coder%var(iter))
                  call make_sys_call(coder,cblock2,q,sym_set_element,3,0)
               endif
            enddo
            call close_cblock(coder,cblock2)
            coder%vtop=lbase
         endif
      case(sym_list)
         call trav_exprlist(coder,cblock,qp,q)
      case default
         call trav_expr(coder,cblock,qp,q)
      end select
    end subroutine apply_x
  end subroutine trav_xexpr

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
    integer:: sym,i,n,save_sub_index
    logical:: save_loop_local,save_have_import
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
             call make_call(coder,cblock,pnode,sym,0,1)
          case(sym_hi,sym_lo,sym_step)
             if(coder%sub_index==0) then
                call code_error(coder,pnode,&
                  'Use of "high","low" or "step" outside of subscript expression:'&
                     ,node)
             else
                call make_temp_var(coder,cblock,pnode)
                call dup_expr(coder,coder%sub_array)
                call make_const(coder,cblock,pnode,&
                     pm_fast_tinyint(coder%context,&
                     int(coder%sub_index,pm_p)))
                call make_sys_call(coder,cblock,pnode,sym,2,1)
             endif
          case default
             write(*,*) sym_names(sym)
             call pm_panic('trav-expr strange sym')
          end select
          return
       end if
       p=find_var(coder,node,base=coder%loop_base)
       if(pm_fast_isnull(p)) then
          p=find_var(coder,node,top=coder%loop_base)
          if(.not.pm_fast_isnull(p)) then
             call make_var(coder,cblock,pnode,node,&
                  ior(var_const,var_shadow))
             call dup_code(coder)
             call code_val(coder,p)
             q=cnode_get(p,var_loop)
             if(pm_fast_isnull(q)) then
                call code_val(coder,coder%var(coder%loop_base+1))
                call make_call(coder,coder%loop_cblock,pnode,sym_import,2,1)
             else
                call code_val(coder,cnode_get(q,cblock_first_var))
                call code_val(coder,coder%var(coder%loop_base+1))
                call make_call(coder,coder%loop_cblock,pnode,sym_import,3,1)
             endif
             return
          endif
       else
          if(cnode_flags_clear(p,var_flags,var_imported)) &
               coder%loop_local=.true.
       endif
       if(.not.pm_fast_isnull(p)) then
          call code_val(coder,p)
       else 
          p=find_decl(coder,pnode,node,modl_param)
          if(pm_fast_isnull(p)) then
             p=find_decl(coder,pnode,node,modl_proc)
             if(pm_fast_isnull(p)) then
                call code_error(coder,pnode,'Name not defined:',node)
                call make_temp_var(coder,cblock,pnode)
             else
                call make_const(coder,cblock,pnode,p)
             endif
          else
             call trav_expr(coder,cblock,pnode,p)
          endif
       endif
    else if(pm_fast_vkind(node)==pm_pointer) then
       sym=node_sym(node)
       if(sym==sym_null) then
          call trav_type(coder,node,node_arg(node,2))
          call make_const(coder,cblock,node,node_arg(node,1),pop_type(coder))
          return
       else if(sym==sym_proc) then
          call push_type(coder,pm_typ_is_single_proc)
          call push_type(coder,int(node_get_num(node,node_args),pm_i16))
          call make_type(coder,2)
          call make_const(coder,cblock,node,node_arg(node,1),pop_type(coder))
          return
       else if(sym==sym_dollar) then
          call push_type(coder,pm_typ_is_single_name)
          call push_type(coder,int(node_get_num(node,node_args),pm_i16))
          call make_type(coder,2)
          call make_const(coder,cblock,node,node_arg(node,1),pop_type(coder))
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
       case(sym_at,sym_square_at,sym_brace_at)
          list=cblock
          i=cnode_get_num(list,cblock_sym)
          do 
             select case(i)
             case(sym_if,sym_select,sym_while,sym_repeat)
                call code_error(coder,node,&
                     '"at" operator cannot be in conditional statement')
                call drop_code(coder)
                return
             case(sym_find,sym_loop) 
                if(cnode_flags_clear(list,cblock_flags,cblock_loop_invar)) &
                     call code_error(coder,node,&
                     '"at" operator indside loop of varying iterations')
             case(sym_par_loop,sym_par_find)
                list%data%ptr(list%offset+cblock_flags)%offset=ior(&
                     list%data%ptr(list%offset+cblock_flags)%offset,&
                     cblock_contains_at)
                exit
             end select
             list=cnode_get(list,cblock_parent)
             if(pm_fast_isnull(list)) then
                call code_error(coder,node,&
                     '"at" operator outside of loop')
                call drop_code(coder)
                return
             endif
             i=cnode_get_num(list,cblock_sym)
          enddo
          call code_val(coder,coder%var(coder%loop_base+1))
          save_loop_local=coder%loop_local
          coder%loop_local=.false.
          call trav_expr(coder,cblock,node,node_arg(node,1))
          if(.not.coder%loop_local) then
             call code_error(coder,node,&
                  'First argument of "at" must depend on a loop-local value')
          endif
          n=node_numargs(node)
          if(node_numargs(node)>1) then
             if(sym==sym_at) then
                coder%loop_local=.false.
                call trav_expr(coder,cblock,&
                     node,node_arg(node,2))
                if(.not.coder%loop_local) then
                   sym=sym_global_at
                endif
             else
                list=node_arg(node,2)
                save_sub_index=coder%sub_index
                save_sub_array=coder%sub_array
                coder%sub_array=top_code(coder)
                do i=1,node_numargs(list)
                   coder%sub_index=i
                   call trav_expr(coder,cblock,&
                        list,node_arg(list,i))
                enddo
                n=1+node_numargs(list)
                coder%sub_index=save_sub_index
                coder%sub_array=save_sub_array
             endif
          endif
          coder%loop_local=save_loop_local
          call make_sys_call(coder,cblock,node,&
               sym,n+1,1)
       case(sym_open_square,sym_open_brace)
          call trav_expr(coder,cblock,node,node_arg(node,1))
          list=node_arg(node,2)
          save_sub_index=coder%sub_index
          save_sub_array=coder%sub_array
          coder%sub_array=top_code(coder)
          do i=1,node_numargs(list)
             coder%sub_index=i
             call trav_expr(coder,cblock,&
                  list,node_arg(list,i))
          enddo
          call make_sys_call(coder,cblock,node,sym,&
               1+node_numargs(list),1)
          coder%sub_index=save_sub_index
          coder%sub_array=save_sub_array
       case(sym_call,sym_loop_call)
          call trav_call(coder,cblock,pnode,node,1)
       case(sym_reduce)
          p=find_sig(coder,node,node_arg(node,1),pm_null_obj,&
               1,1,call_is_reduce,q)
          call trav_expr(coder,cblock,node,node_arg(node,2))
          call make_full_call(coder,cblock,node,p,1,1,0,call_is_reduce)
       case(sym_dot)
          call trav_expr(coder,cblock,node,node_arg(node,1))
          call make_const(coder,cblock,node,&
               node_arg(node,2))
          call make_call(coder,cblock,node,sym_dot,2,1)
       case(sym_any)
          call trav_type(coder,node,node_arg(node,1))
          call make_const(coder,cblock,node,&
               pm_fast_tinyint(coder%context,int(pop_type(coder),pm_p)))
          call trav_expr(coder,cblock,node,node_arg(node,2))
          call make_call(coder,cblock,node,sym_any,2,1)
       case(sym_close,sym_close_square,sym_close_brace)
          !!!!!!!!!!! TODO
       case(sym_struct,sym_rec)
          call make_const(coder,cblock,node,node_arg(node,2))
          p=node_arg(node,1)
          do i=1,node_numargs(p)
             call trav_expr(coder,cblock,p,node_arg(p,i))
          enddo
          call make_call(coder,cblock,p,sym,node_numargs(p)+1,1)
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
  end subroutine trav_expr

  ! Traverse from.. follow
  subroutine trav_from_follow(coder,cblock,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,node
    !!!! TODO
  end subroutine trav_from_follow

  ! Traverse a procedure call in parse tree
  subroutine trav_call(coder,cblock,pnode,cnode,nret)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,pnode,cnode
    integer,intent(in):: nret
    type(pm_ptr):: list,procs,keys,sig,node,name,var
    integer:: flags,i,j,start,cn_sym,nargs,nkeys
    cn_sym=node_sym(cnode)
    if(cn_sym==sym_loop_call) then
       node=node_arg(cnode,1)
       flags=call_is_loop_call
    else
       node=cnode
       flags=0
    endif
    list=node_arg(node,2)
    if(node_sym(list)==sym_dotdotdot) &
         flags=ior(flags,call_is_vararg)
    nargs=node_numargs(list)
    name=node_arg(node,1)
    var=find_var(coder,name)
    if(pm_fast_isnull(var)) then
       procs=find_sig(coder,node,name,&
            node_arg(node,4),nargs,nret,flags,sig)
    else
!!       procs=find_vcall_sig(coder,node,name,node_arg(node,4),&
!!            nargs,nret,flags,sig)
    endif
    if(pm_fast_isnull(procs)) then
       coder%vtop=coder%vtop-nret
       return
    endif
    keys=cnode_arg(sig,1)
    list=node_arg(node,3)
    if(.not.pm_fast_isnull(list)) then
       if(pm_fast_isnull(keys)) then
          call code_error(coder,node,&
               'Unexpected keyword arguments in call to:',&
               node_arg(node,1))
       else
         start=coder%vtop
         nkeys=pm_set_size(coder%context,keys)
         do i=1,nkeys
            call make_const(coder,cblock,node,pm_null_obj)
         enddo
         do i=1,node_numargs(list),2
            call trav_expr(coder,cblock,list,node_arg(list,i+1))
            j=pm_set_lookup(coder%context,keys,node_arg(list,i))
            if(j>0) then
               coder%vstack(start+j)=pop_code(coder)
            else
               call code_error(coder,list,&
                    'Unexpected keyword argument',&
                    node_arg(list,i))
               call drop_code(coder)
            endif
         enddo
       endif
    else if(.not.pm_fast_isnull(keys)) then
       nkeys=pm_set_size(coder%context,keys)
       do i=1,nkeys
          call make_const(coder,cblock,node,pm_null_obj)
       enddo
    else
       nkeys=0
    endif
    list=node_arg(node,2)
    if(.not.pm_fast_isnull(list)) then
       j=node_numargs(list)
       do i=1,j
          call trav_expr(coder,cblock,list,&
               node_arg(list,i))
       enddo
    endif
    call make_full_call(coder,cblock,node,procs,nargs,nret,nkeys,flags)
  contains
    include 'fisnull.inc'
  end subroutine trav_call
  
  recursive subroutine trav_type(coder,pnode,node)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: pnode,node
    integer:: asbase
    asbase=coder%vtop
    call trav_type_from(coder,.false.,pnode,node)
    coder%vtop=asbase
  end subroutine trav_type

  ! Traverse type expression in parse tree
  recursive subroutine trav_type_from(coder,inopt,pnode,node)
    type(code_state),intent(inout):: coder
    logical,intent(in):: inopt
    type(pm_ptr),intent(in):: pnode,node
    integer:: sym,i,n
    integer(pm_i16)::typno
    integer(pm_p):: gen
    type(pm_ptr):: name,val,p
    character(len=100):: str
    if(pm_fast_isnull(node)) then
       call push_type(coder,0_pm_i16)
       return
    endif
    sym=node_sym(node)
    if(pm_debug_level>2) write(*,*) 'Trav type:',sym_names(sym)
    select case(sym)
    case(sym_any)
       if(node_numargs(node)>0) then
          call push_type(coder,pm_typ_is_any)
          call push_type(coder,0_pm_i16)
          call trav_type_from(coder,.false.,node,node_arg(node,1))
       else
          call push_type(coder,0_pm_i16)
       endif
    case(sym_null)
       call push_type(coder,pm_typ_is_null)
       call push_type(coder,0_pm_i16)
       call trav_type_from(coder,.true.,node,node_arg(node,1))
       call make_type(coder,3)
    case(sym_type)
       n=node_numargs(node)
       name=node_arg(node,n)
       call pm_name_string(coder%context,name%offset,str)
       if(pm_debug_level>2) &
            write(*,*) 'Traverse named type: ',trim(str),n
       call push_type(coder,pm_typ_is_user)
       call push_type(coder,int(name%offset,pm_i16))
       if(n>1) then
          ! Type arguments
          do i=1,n-1
             call trav_type_from(coder,.false.,node,node_arg(node,i))
          enddo
       endif
       typno=get_typeno(n+1)
       if(typno==0) then
          call trav_type_def(coder,node,name,n-1)
       else
          coder%ttop=coder%ttop-n-1
          if(inopt) then
             call push_type(coder,typno)
          else
             p=pm_dict_val(coder%context,coder%context%tcache,int(typno,pm_ln))
             if(pm_fast_istiny(p)) then
                call code_error(coder,node,&
                     'Type cannot occur in its own definition except for opt')
                call push_type(coder,0_pm_i16)
             else
                call push_type(coder,typno)
             endif
          endif
       endif
    case(sym_open_brace)
       name=node_arg(node,1)
       call push_type(coder,pm_typ_is_user)
       call push_type(coder,int(name%offset,pm_i16))
       typno=get_typeno(2)
       if(typno==0) call pm_panic('Intrinsic type not found')
       call push_type(coder,typno)
    case(sym_struct,sym_rec)
       name=node_arg(node,2)
       if(sym==sym_struct) then
          call push_type(coder,pm_typ_is_struct)
       else
          call push_type(coder,pm_typ_is_rec)
       endif
       call push_type(coder,int(name%offset,pm_i16))
       val=node_arg(node,1)
       n=node_numargs(val)
       do i=1,n
          call trav_type_from(coder,.false.,val,node_arg(val,i))
       enddo
       call make_type(coder,n+2)
    case(sym_open_square)
       call push_type(coder,pm_typ_is_array)
       call push_type(coder,0_pm_i16)
       call trav_type_from(coder,.false.,node,node_arg(node,1))
       p=node_arg(node,2)
       if(node_numargs(p)==0) then
          call push_type(coder,pm_typ_is_user)
          call push_type(coder,int(sym_dom,pm_i16))
          typno=get_typeno(2)
          if(typno==0) then
             call trav_type_def(coder,node,pm_fast_name(coder%context,sym_dom),0)
          else
             coder%ttop=coder%ttop-2
             call push_type(coder,typno)
          endif
       elseif(node_numargs(p)<=1) then 
          call trav_type_from(coder,.false.,node,p)
       else
          call push_type(coder,pm_typ_is_user)
          call push_type(coder,int(sym_grid,pm_i16))
          do i=1,node_numargs(p)
             call trav_type_from(coder,.false.,p,node_arg(p,i))
          enddo
          call make_type(coder,node_numargs(p)+2)
       endif
       call make_type(coder,4)
    case(sym_list,sym_dotdotdot)
       if(sym==sym_dotdotdot) then
          call push_type(coder,pm_typ_is_vtuple)
       else
          call push_type(coder,pm_typ_is_tuple)
       endif
       call push_type(coder,0_pm_i16)
       n=node_numargs(node)
       do i=2,n,2
          call trav_type_from(coder,.false.,node,node_arg(node,i))
       enddo
       call make_type(coder,n/2+2)
    case(sym_eq)
       call push_type(coder,int(pm_matched_type,pm_i16))
    case(sym_result)
       call push_type(coder,pm_typ_is_tuple)
       call push_type(coder,0_pm_i16)
       n=node_numargs(node)
       do i=1,n
          call trav_type_from(coder,.false.,node,node_arg(node,i))
       enddo
       call make_type(coder,n+2)
    case default
       if(sym>=0.and.sym<=num_sym) then
          write(*,*) 'SYM=',sym_names(sym)
       else
          write(*,*) 'SYMno=',sym
       endif
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
           coder%tstack(coder%ttop-size+1:),&
           size)
    end function get_typeno

  end subroutine trav_type_from

  ! Traverse a type definition (specific to given type args)
  recursive subroutine trav_type_def(coder,node,name,nargs)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,name
    integer,intent(in):: nargs
    type(pm_ptr):: decl,dec,inc,pargs,also_node,incl_node
    type(pm_ptr),target:: incset
    logical:: is_present,also_present,type_present
    integer:: sym,i,j,base
    integer(pm_i16):: newtyp,argtyp,argtype,basetyp,partyp
    type(pm_reg),pointer:: reg
    if(pm_debug_level>2) &
         write(*,*) 'Traverse type def: ',trim(pm_name_as_string(coder%context,name%offset))
    base=coder%ttop
    if(nargs==0) then
       decl=find_type_var(name)
       if(pm_fast_isnull(decl)) then
          decl=find_decl(coder,node,name,modl_type)
          if(pm_fast_isnull(decl)) then
             call code_error(coder,node,'Cannot find type:',name)
             coder%ttop=coder%ttop-nargs-1
             coder%tstack(coder%ttop)=0
             return
          endif
          argtype=0
       else
          coder%ttop=coder%ttop-nargs-1
          coder%tstack(coder%ttop)=decl%offset
          return
       endif
    else
       decl=find_decl(coder,node,name,modl_type)
       if(pm_fast_isnull(decl)) then
          call code_error(coder,node,'Cannot find type:',name)
          coder%ttop=coder%ttop-nargs-1
          coder%tstack(coder%ttop)=0
          return
       endif
       call push_type(coder,pm_typ_is_tuple)
       call push_type(coder,0_pm_i16)
       do i=1,nargs
          call push_type(coder,coder%tstack(base-nargs+i))
       enddo
       call make_type(coder,nargs+2)
       argtype=pop_type(coder)
    endif
    newtyp=pm_idict_add(coder%context,&
         coder%context%tcache,&
         coder%tstack(coder%ttop-nargs-1:),nargs+2,&
         pm_fast_tinyint(coder%context,1_pm_p))
    incset=pm_tset_new(coder%context)
    call code_val(coder,incset)
    is_present=.false.
    also_present=.false.
    type_present=.false.
    basetyp=0
    if(nargs>0) then
       dec=node_arg(decl,2)
       do
          if(node_sym(dec)==sym_includes.or.node_sym(dec)==sym_is) then
             call trav_type(coder,dec,node_get(dec,typ_params))
             basetyp=pop_type(coder)
             incl_node=dec
          endif
          dec=node_get(dec,typ_link)
          if(pm_fast_isnull(dec)) exit
       enddo
    endif
    dec=node_arg(decl,2)
    do
       argtyp=argtype
       if(pm_debug_level>0) then
          if(dec%data%vkind/=pm_pointer) &
               call pm_panic('Type node not ptr in trav def')
       endif
       sym=node_sym(dec)
       if(pm_debug_level>2) &
            write(*,*) 'CHECK TYPE DEF>',sym_names(sym)
       if(sym==sym_in) then
          call make_type_vars(pm_null_obj,0)
          call trav_type(coder,dec,node_arg(dec,2))
          call pm_tset_add(coder%context,incset,top_type(coder))
          call pop_type_vars(nargs)
          if(.not.also_present) then
             also_present=.true.
             also_node=dec
          endif
          dec=node_arg(dec,1)
       else
          if(sym==sym_is) then
             is_present=.true.
          else if(sym==sym_also) then
             also_present=.true.
             also_node=dec
          else
             if(pm_debug_level>0) then
                if(sym/=sym_type) then
                   if(sym>=0.and.sym<=num_sym) then
                      write(*,*) 'SYM=',trim(sym_names(sym))
                   else
                      write(*,*) 'SYM=',sym
                   endif
                   call pm_panic('Not a type in trav_type_def')
                endif
             endif
             type_present=.true.
          endif
          pargs=node_get(dec,typ_params)
          if(nargs>0) then
             call trav_type(coder,dec,pargs)
             partyp=pop_type(coder)
             if(sym==sym_also) then
                if(.not.pm_typ_includes(coder%context,basetyp,partyp)) then
                   partyp=typ_with_default(basetyp,argtyp)
                   if(.not.pm_typ_includes(coder%context,basetyp,partyp)) then
                      call code_error(coder,dec,&
                           'Parameter constraints for "also includes" do not conform to "includes"',&
                           name)
                      call code_error(coder,incl_node,&
                           'Conflicting definition')
                   endif
                endif
             endif
             if(.not.pm_typ_includes(coder%context,partyp,argtyp)) then
                argtyp=typ_with_default(basetyp,argtyp)
                if(.not.pm_typ_includes(coder%context,partyp,argtyp)) then
                   if(sym==sym_also) goto 20
                   call code_error(coder,node,&
                        'Type arguments do not match type parameter constraints:',&
                        name)
                   write(*,*) '------------'
                   call dump_type(coder%context,6,partyp,2)
                   write(*,*) '------------'
                   call dump_type(coder%context,6,argtyp,2)
                   call code_error(coder,node,&
                        'Conflicting definition')
                endif
             endif
             call node_set_num(dec,typ_number,int(partyp,pm_p))
          endif
          call make_type_vars(pargs,nargs)
          inc=node_get(dec,typ_includes)
          if(.not.pm_fast_isnull(inc)) then
             do i=1,node_numargs(inc)
                call trav_type(coder,dec,node_arg(inc,i))
                call pm_tset_add(coder%context,&
                     incset,top_type(coder))
                do j=1,i-1
                   if(coder%tstack(base+j)==&
                        coder%tstack(coder%ttop)) then
                      call code_error(coder,dec,&
                           'Repeated type inclusion')
                   endif
                enddo
             enddo
             call pop_type_vars(nargs)
          endif
 20       continue
          dec=node_get(dec,typ_link)
       endif
       if(pm_fast_isnull(dec)) exit
    enddo
    coder%ttop=base
    base=base-nargs-1
    coder%tstack(base)=pm_idict_add(coder%context,&
         coder%context%tcache,&
         coder%tstack(base:),nargs+2,incset)
    if(coder%tstack(base)/=newtyp) then
       write(*,*) coder%tstack(base),newtyp,base,'(',coder%tstack(base:base+nargs+1),')'
       call pm_panic('trav typ def mismatch')
    endif
    coder%ttop=base
    if(is_present.and.also_present) &
         call code_error(coder,also_node,&
         'Cannot add to type defined with "is":',&
         name)
    if(also_present.and..not.type_present) &
         call code_error(coder,also_node,&
         'Type "also includes" without original "includes":',&
         name)
    call drop_code(coder)
    if(pm_debug_level>2) write(*,*) 'DEFINITION TRAVERSED'
  contains
    
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'fesize.inc'
    include 'ftiny.inc'

    subroutine make_type_vars(pnames,m)
      type(pm_ptr),intent(in):: pnames
      integer,intent(in):: m
      integer:: k,j
      type(pm_ptr):: pname
      coder%stack(coder%top+1)=typevar_start
      do k=1,m
         pname=node_arg(pnames,k*2-1)
         coder%stack(k+coder%top+1)=pname%offset
         coder%var(k+coder%top+1)=pm_fast_tinyint(&
              coder%context,int(coder%tstack(coder%ttop-m+k),pm_p))
         do j=1,k-1
            if(coder%stack(j+coder%top+1)==coder%stack(k+coder%top+1)) then
               call code_error(coder,node,'Repition of type parameter:',&
                    pname)
            endif
         enddo
      enddo
      coder%stack(coder%top+m+2)=typevar_end
      coder%top=coder%top+m+2
    end subroutine make_type_vars

    subroutine pop_type_vars(m)
      integer:: m
      coder%top=coder%top-m-2
    end subroutine pop_type_vars

    function find_type_var(vname) result(vr)
      type(pm_ptr),intent(in):: vname
      type(pm_ptr):: vr
      integer:: k
      if(coder%stack(coder%top)/=typevar_end) then
         vr=pm_null_obj
      else
         k=coder%top-1
         do while(coder%stack(k)/=typevar_start)
            if(coder%stack(k)==vname%offset) then
               vr=coder%var(k)
               return
            endif
            k=k-1
            if(pm_debug_level>0) then
               if(k<=0) call pm_panic('find type var')
            endif
         enddo
         vr=pm_null_obj
      endif
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
      call push_type(coder,pm_typ_is_tuple)
      call push_type(coder,0_pm_i16)
      do i=1,pm_tv_numargs(av)
         if(pm_tv_arg(av,i)==0) then
            call push_type(coder,pm_tv_arg(pv,i))
         else
            call push_type(coder,pm_tv_arg(av,i))
         endif
      enddo
      call make_type(coder,pm_tv_numargs(pv)+2)
      newat=pop_type(coder)
    end function typ_with_default
    
  end subroutine trav_type_def

  ! Traverse procedure definition
  recursive subroutine trav_proc(coder,node,keyargs,amplocs,nargs,nret)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,amplocs,keyargs
    integer,intent(in):: nargs,nret
    type(pm_ptr):: cblock,p,par,amp,rtypes,arg
    integer:: i,j,base,obase,npars,cbase
    integer(pm_i16):: partyp
    integer(pm_p):: flags,save_index,t,sym
    integer:: save_proc_base,save_sub_index,save_loop_base
    type(pm_ptr):: save_sub_array,save_loop_cblock
    logical:: save_loop_local
       
    obase=coder%vtop
    partyp=node_get_num(node,proc_coded_params)
    if(partyp<0) then
       p=node_get(node,proc_params)
       call trav_type(coder,node,p)
       partyp=pop_type(coder)
       call node_set_num(node,proc_coded_params,int(partyp,pm_p))
    endif
    call code_num(coder,int(partyp,pm_p))
    if(node_sym(node)==sym_endproc) then
       p=node_get(node,proc_opcode)
       p=node_get(node,proc_coded_builtin)
       if(.not.pm_fast_isnull(p)) then
          call code_val(coder,p)          
          return
       endif
       cbase=coder%vtop
       call code_val(coder,node_get(node,proc_opcode))
       call code_val(coder,node_get(node,proc_opcode2))
       call code_val(coder,node_get(node,proc_data))
       if(nret>0) then
          if(pm_fast_isnull(node_get(node,proc_retas))) then
             call trav_type(coder,node,node_get(node,proc_rettypes))
             call code_num(coder,int(pop_type(coder),pm_p))
             call code_null(coder)
             call code_num(coder,0)
          else
             call code_num(coder,0_pm_p)
             call save_proc_state
             call init_proc_state
             call code_params
             p=node_get(node,proc_retas)
             sym=node_sym(p)
             p=node_arg(p,1)
             base=coder%vtop
             call trav_exprlist(coder,cblock,node,p)
             call make_call(coder,cblock,node,&
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
       coder%id=coder%id+1
       call code_num(coder,coder%id)
       if(coder%vtop-cbase/=bi_node_size) call pm_panic('making bi')
       call make_code(coder,node,cnode_is_builtin,bi_node_size)
    else
       p=node_get(node,proc_code_tree)
       if(.not.pm_fast_isnull(p)) then
          call code_val(coder,p)
          return
       endif
       call save_proc_state
       call init_proc_state
       call code_params
       p=node_get(node,proc_keys)
       if(.not.pm_fast_isnull(p)) then
          do i=1,node_numargs(p),2
             call make_var(coder,cblock,p,node_arg(p,i),&
                  ior(var_const,var_param))
             j=pm_set_lookup(coder%context,keyargs,node_arg(p,i))
             if(pm_debug_level>0) then
                if(j<0) call pm_panic('lookup key arg')
             endif
             call code_val(coder,coder%var(coder%proc_base+j))
             call trav_expr(coder,cblock,p,node_arg(p,i+1))
             call make_call(coder,cblock,p,sym_close,3,0)
          enddo
       endif
       p=node_get(node,proc_check)
       if(.not.pm_fast_isnull(p)) then
          base=coder%vtop
          call trav_xexpr(coder,cblock,node,p)
          call make_call(coder,cblock,node,sym_check,coder%vtop-base,0)
       endif
       p=node_get(node,proc_stmts)
       if(.not.pm_fast_isnull(p)) then
          call trav_open_statlist(coder,cblock,node,p)
       endif
       p=node_get(node,proc_result)
       if(.not.pm_fast_isnull(p)) then
          base=coder%vtop
          call trav_xexpr(coder,cblock,node,p)
          do i=1,nret
             arg=coder%vstack(coder%vtop+1-i)
             if(cnode_is_tempvar(arg)) &
                  call cnode_set(arg,var_assign_call,&
                  cnode_get(cblock,cblock_last_call))
          enddo
          call make_call(coder,cblock,node,sym_result,nret,0)
          if(pm_debug_level>0) then
             if(coder%vtop/=base) call pm_panic('rtn mismatch')
          endif
       endif
       call close_cblock(coder,cblock)
       call code_num(coder,coder%index) ! Maximum index
       call code_num(coder,0_pm_p)      ! Recursion flag
       coder%id=coder%id+1
       call code_num(coder,coder%id)    ! Procedure identifier
       call code_num(coder,int(npars,pm_p))  ! Number of parameters
       call code_num(coder,int(nret,pm_p))   ! Number of returns
       call code_num(coder,0_pm_p)           !!! Flags
       call make_code(coder,node,cnode_is_proc,pr_node_size)
       call restore_proc_state
       if(pm_debug_level>0) then
          if(coder%vtop/=obase+2) then
             write(*,*) 'Vtop=',coder%vtop,'Obase=',obase
             call pm_panic('Code node misbalance trav_proc')
          endif
       endif
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
       save_loop_local=coder%loop_local
       save_loop_cblock=coder%loop_cblock      
    end subroutine save_proc_state

    subroutine init_proc_state
       coder%index=0
       coder%proc_base=coder%top
       coder%sub_index=0
       coder%loop_base=coder%top
       coder%loop_local=.false.
    end subroutine init_proc_state

    subroutine code_params
      npars=0
      cblock=make_cblock(coder,pm_null_obj,node,sym_proc)
      if(.not.pm_fast_isnull(keyargs)) then
         do i=1,pm_set_size(coder%context,keyargs)
            call make_var(coder,cblock,node,&
                 pm_fast_name(coder%context,int(sym_assign,pm_p)),&
                 ior(var_const,ior(var_param,var_shadow)))
         enddo
         npars=npars+pm_set_size(coder%context,keyargs)
      endif
      p=node_get(node,proc_params)
      if(.not.pm_fast_isnull(p)) then
         amp=node_get(node,proc_amplocs)
         if(pm_fast_isnull(amp)) then
            do i=1,node_numargs(p),2
               call make_var(coder,cblock,p,node_arg(p,i),&
                    ior(var_const,var_param))
            enddo
         else
            j=0
            amp=pm_name_val(coder%context,amp%offset)
            do i=1,node_numargs(p),2
               if(amp%data%i16(amp%offset+j)==(i-1)/2) then
                  flags=ior(var_amp,var_param)
                  if(j<pm_fast_esize(amp)) j=j+1
               else
                  flags=ior(var_const,var_param)
               endif
               call make_var(coder,cblock,p,node_arg(p,i),flags)
            enddo
         endif
         call make_call(coder,cblock,p,sym_open,node_numargs(p)/2,0)
         npars=npars+node_numargs(p)/2
      endif
    end subroutine code_params

    subroutine restore_proc_state
       coder%index=save_index
       coder%proc_base=save_proc_base
       coder%sub_index=save_sub_index
       coder%loop_base=save_loop_base
       coder%sub_array=save_sub_array
       coder%loop_local=save_loop_local
       coder%loop_cblock=save_loop_cblock
    end subroutine restore_proc_state
  end subroutine trav_proc
  
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

  ! Find procedure signature
  recursive function find_sig(coder,node,name,amplocs,&
       nargs,nret,flags,sigvect) result(sig)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: node,name,amplocs
    integer,intent(in):: nargs,nret,flags
    type(pm_ptr),intent(out):: sigvect
    type(pm_ptr):: sig,cblock,keys
    type(pm_ptr):: procdef,proc,amplocs2,rtn,keyargs
    integer:: nret2,base,i,j
    integer(pm_i16),dimension(4):: sig_key
    integer(pm_p):: k
    integer:: cflags,flags2
    character(len=100):: str
    sig_key(1)=name%offset
    sig_key(2)=amplocs%offset
    sig_key(3)=nret
    sig_key(4)=flags
    if(pm_debug_level>2) then
       call pm_name_string(coder%context,name%offset,str)
       call pm_name_string(coder%context,amplocs%offset,str(len_trim(str)+2:))
       write(*,*) 'SIG:',trim(str),'NARGS=',nargs,'nret=',nret
    endif
    k=pm_ivect_lookup(coder%context,coder%sig_cache,sig_key,4)
    if(k==0) then
       procdef=find_decl(coder,node,name,modl_proc)
       if(pm_fast_isnull(procdef)) then
          call code_error(coder,node,&
               'Cannot find procedure:',name)
          sig=pm_null_obj
          return
       else
          if(iand(flags,call_is_reduce)/=0) then
             cflags=proc_is_reduce
          else if(iand(flags,call_is_loop_call)/=0) then
             cflags=proc_is_loop_proc
          else
             cflags=0
          endif
          ! Find keywords for this signature
          keys=pm_null_obj
          proc=node_arg(procdef,2)
          do
                keyargs=node_get(proc,proc_keys)
                if(.not.pm_fast_isnull(keys)) then
                   ! Add keywords to set
                   if(pm_fast_isnull(keys)) then
                      keys=pm_set_new(coder%context,&
                           8_pm_ln)
                   endif
                   do i=1,node_numargs(keyargs),2
                      j=pm_set_add(coder%context,keys,&
                           node_arg(keyargs,i))
                   enddo
                endif
                proc=node_get(proc,proc_link)
                if(pm_fast_isnull(proc)) exit
          enddo
          call code_val(coder,keys)
          call code_num(coder,0_pm_p)
          call make_code(coder,node,cnode_is_arglist,2)
          k=pm_idict_add(coder%context,coder%sig_cache,sig_key,4,top_code(coder))
          call drop_code(coder)
          base=coder%vtop
          ! Signature node starts with set of keys and combined flags
          call code_val(coder,keys)
          call code_num(coder,0_pm_p)
          proc=node_arg(procdef,2)
          do
             if(pm_debug_level>2) &
                  write(*,*) 'CHECK PROC SIG>',pm_fast_isnull(node_get(proc,proc_link))
             ! Find conforming procs
             if(proc_conforms()) then
                if(pm_debug_level>2) &
                     write(*,*) 'CONFORMING_PROC>',coder%vtop,base
                call trav_proc(coder,proc,keys,amplocs,nargs,nret)
                if(pm_debug_level>2) &
                     write(*,*) 'FINISHED TRAVERSING CONFORMING PROC>',coder%vtop,base
                ! Combine flags for this signature
                coder%vstack(base+1)%offset=&
                     ior(coder%vstack(base+1)%offset,&
                     int(flags2,pm_p))
             endif
             proc=node_get(proc,proc_link)
             if(pm_fast_isnull(proc)) exit
          enddo
          if(pm_debug_level>2) &
               write(*,*) 'SIGEND>',trim(str),'NARGS=',nargs,'nret=',nret,'N=',coder%vtop-base
          if(coder%vtop>base+2) then
             if(coder%vtop>base+3) call sort_sig(coder,base+2,coder%vtop)
             if(pm_debug_level>2) &
                  write(*,*) 'MAKE-ARG-CODE>',base,coder%vtop,coder%vtop-base
             call make_code(coder,node,cnode_is_arglist,coder%vtop-base)
             sig=pop_code(coder)
             k=pm_idict_add(coder%context,coder%sig_cache,sig_key,4,sig)
             sigvect=sig
             sig=pm_fast_tinyint(coder%context,k)
             return
          else
             call drop_code(coder)
             call drop_code(coder)
             call code_error(coder,node,&
               'Cannot find procedure with correct signature:',name)
             sig=pm_null_obj
          endif
       endif
    else
       sig=pm_fast_tinyint(coder%context,k)
       sigvect=pm_dict_val(coder%context,coder%sig_cache,int(k,pm_ln))
    endif
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'ftiny.inc'

    function proc_conforms() result(ok)
      logical:: ok
      flags2=node_get_num(proc,proc_flags)
      amplocs2=node_get(proc,proc_amplocs)
      rtn=node_get(proc,proc_numret)
      nret2=rtn%offset
      ok=(amplocs2%offset==amplocs%offset&
           .and.nret2==nret.and.&
           iand(flags2,cflags)==cflags)
    end function proc_conforms

  end function find_sig

  ! Partial order sort for signature
  subroutine sort_sig(coder,start,end)
    type(code_state),intent(inout):: coder
    integer,intent(in):: start,end
    integer:: i,j,rel
    integer(pm_i16):: typ1,typ2
    type(pm_ptr):: code,pars
    if(pm_debug_level>2) write(*,*) 'SORT',start,end
    do i=end-3,start,-2
       if(pm_debug_level>2) write(*,*) 'I=',i,i+2,end-1
       typ1=coder%vstack(i)%offset
       code=coder%vstack(i+1)
       j=i+2
       do while(j<=end-1)
          typ2=coder%vstack(j)%offset
          pars=pm_dict_key(coder%context,&
               coder%context%tcache,int(typ2,pm_ln))
          if(pm_debug_level>2) write(*,*) 'COMPARE SIGS>',typ1,typ2
          rel=pm_typ_rel(coder%context,typ1,typ2)
          if(pm_debug_level>2) then
             write(*,*) '----------'
             call dump_type(coder%context,6,typ1,2)
             write(*,*) '==='
             call dump_type(coder%context,6,typ2,2)
             write(*,*) '----------',rel
          endif
          select case(rel)
          case(pm_typ_equal,pm_typ_conjoint)
             if(rel==pm_typ_equal) then
                call cnode_error(coder,code,&
                     'Procedures defined with identical signatures')
                call cnode_error(coder,coder%vstack(j+1),&
                     'Conflicting definition')
             else
                call cnode_error(coder,code,&
                     'Procedures have ambiguous signatures')
                call cnode_error(coder,coder%vstack(j+1),&
                     'Conflicting definition')
             endif
             exit
          case(pm_typ_contained)
             exit
          case default
             coder%vstack(j-2)=coder%vstack(j)
             coder%vstack(j-1)=coder%vstack(j+1)
             j=j+2
          end select
       enddo
       coder%vstack(j-2)%offset=typ1
       coder%vstack(j-1)=code
    enddo
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

  ! Push type onto type stack
  subroutine push_type(coder,k)
    type(code_state),intent(inout):: coder
    integer(pm_i16),intent(in):: k
    if(coder%ttop>=max_code_stack) &
         call pm_panic('Program too complex')
    coder%ttop=coder%ttop+1
    coder%tstack(coder%ttop)=k
  end subroutine push_type
  
  ! Make type using size elements from type stack - leave on type stack
  subroutine make_type(coder,size)
    type(code_state),intent(inout):: coder
    integer,intent(in):: size
    coder%ttop=coder%ttop-size+1
    coder%tstack(coder%ttop)=&
         new_type(coder,coder%tstack(coder%ttop:coder%ttop+size-1))
  end subroutine make_type

  ! Make an array type, given element and domain types
  function make_array_type(coder,etyp,dtyp) result(atyp)
    type(code_state),intent(inout):: coder
    integer(pm_i16),intent(in):: etyp,dtyp
    integer(pm_i16):: atyp
    if(coder%ttop+4>max_code_stack) &
         call pm_panic('Program too compex')
    coder%tstack(coder%ttop+1)=pm_typ_is_array
    coder%tstack(coder%ttop+2)=0_pm_i16
    coder%tstack(coder%ttop+3)=etyp
    coder%tstack(coder%ttop+4)=dtyp
    atyp=new_type(coder,coder%tstack(coder%ttop+1:coder%ttop+4))
  end function make_array_type
  

  ! Make an optional type, given base type
  function make_any_type(coder,btyp) result(atyp)
    type(code_state),intent(inout):: coder
    integer(pm_i16),intent(in):: btyp
    integer(pm_i16):: atyp
    if(coder%ttop+4>max_code_stack) &
         call pm_panic('Program too compex')
    coder%tstack(coder%ttop+1)=pm_typ_is_any
    coder%tstack(coder%ttop+2)=0_pm_i16
    coder%tstack(coder%ttop+3)=btyp
    atyp=new_type(coder,coder%tstack(coder%ttop+1:coder%ttop+3))
  end function make_any_type

  ! Pop type from type stack
  function pop_type(coder) result(k)
    type(code_state),intent(inout):: coder
    integer(pm_i16):: k
    k=coder%tstack(coder%ttop)
    coder%ttop=coder%ttop-1
    if(pm_debug_level>0) then
       if(coder%ttop<0) call pm_panic('pop type')
    endif
  end function pop_type

  ! Return top of type stack
  function top_type(coder) result(k)
    type(code_state),intent(inout):: coder
    integer(pm_i16):: k
    k=coder%tstack(coder%ttop)
  end function top_type

  ! Find a local variable
  function find_var(coder,name,base,top) result(v)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: name
    integer,intent(in),optional:: top,base
    type(pm_ptr):: v
    integer:: i,start,end
    if(present(base)) then
       start=base
    else
       start=coder%proc_base
    endif
    if(present(top)) then
       end=top
    else
       end=coder%top
    endif
    do i=coder%top,start+1,-1
       if(coder%stack(i)==name%offset) then
          v=coder%var(i)
          return
       endif
    enddo
    v=pm_null_obj
    return
  end function find_var

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
    call code_val(coder,coder%loop_cblock)
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
  end subroutine make_temp_var

  ! Make a local variable code node
  subroutine make_var(coder,cblock,node,name,flags)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,name
    integer(pm_p),intent(in):: flags
    type(pm_ptr):: var,link
    if(iand(flags,var_shadow)==0) then
       var=find_var(coder,name)
       if(.not.pm_fast_isnull(var)) then
          if(iand(flags,var_ref)/=0) then
             if(iand(cnode_get_num(var,var_flags),&
                  var_ref)==0) then
                call code_error(coder,node,&
                     'Cannot use "->" on non-reference',name)
             endif
             call code_val(coder,var)
             return
          endif
          call code_error(coder,node,&
               'Cannot redefine local variable or constant:',name)
          call code_val(coder,var)
          return
       endif
    endif
    if(coder%top>=max_code_stack) then
       call pm_panic('Function too complex')
    endif
    call code_val(coder,cblock)
    call code_val(coder,name)
    call code_num(coder,int(flags,pm_p))
    call code_null(coder)
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    call code_val(coder,coder%loop_cblock)
    call code_val(coder,pm_null_obj)
    call make_code(coder,node,cnode_is_var,var_node_size)
    coder%top=coder%top+1
    coder%stack(coder%top)=name%offset
    coder%var(coder%top)=top_code(coder)
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
  end subroutine make_var

  ! Is code node a temp variable?
  function cnode_is_tempvar(arg) result(ok)
    type(pm_ptr),intent(in):: arg
    logical:: ok
    if(pm_fast_vkind(arg)==pm_pointer) then
       if(cnode_get_kind(arg)==cnode_is_var) then
          if(cnode_get_num(arg,var_name)==0) then
             ok=.true.
             return
          endif
       endif
    endif
    ok=.false.
  contains
    include 'fvkind.inc'
  end function cnode_is_tempvar

  ! Make a constant access code node
  subroutine make_const(coder,cblock,node,val,typ)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,val
    integer(pm_i16),intent(in),optional:: typ
    integer(pm_p):: tno
    if(coder%loop_base>coder%proc_base) then
       call make_temp_var(coder,coder%loop_cblock,node)
       call dup_code(coder)
       call mk_const
       call code_val(coder,coder%var(coder%loop_base+1))
       call make_sys_call(coder,coder%loop_cblock,node,sym_import_val,2,1)
    else
       call mk_const
    endif
  contains
    subroutine mk_const
      tno=-1
      if(present(typ)) tno=typ
      call code_val(coder,val)
      call code_num(coder,tno)
      call make_code(coder,node,cnode_is_const,2)
    end subroutine mk_const
  end subroutine make_const

  ! Dupicate an expression
  subroutine dup_expr(coder,expr)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: expr
    type(pm_ptr):: e
    ! Give temp var a name - nameless vars may only
    ! be used once
    e=expr
    if(cnode_is_tempvar(e)) &
         call cnode_set_num(e,var_name,sym_var)
    call code_val(coder,e)
  end subroutine dup_expr
  
  ! Replicate top n expressions on stack
  subroutine repl_exprs(coder,n)
    type(code_state):: coder
    integer,intent(in):: n
    integer:: i
    type(pm_ptr):: expr
    if(coder%vtop+n>max_code_stack) &
         call pm_panic('Program too complex')
    do i=1,n
       expr=coder%vstack(coder%vtop-n+1)
       ! Give temp var a name - nameless vars may only
       ! be used once
       if(cnode_is_tempvar(expr)) &
            call cnode_set_num(expr,var_name,sym_var)
       coder%vstack(coder%vtop+i)=expr
    enddo
    coder%vtop=coder%vtop+n
  end subroutine repl_exprs

  ! Make a procedure call code node for some builtin operations
  subroutine make_call(coder,cblock,node,sym,narg,nret)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret
    call make_full_call(coder,cblock,node,&
         pm_fast_tinyint(coder%context,-int(sym,pm_p)),narg,nret,0,0)
  contains
    include 'ftiny.inc'
  end subroutine make_call

  ! Make a call to an intrinsic procedure
  subroutine make_sys_call(coder,cblock,node,sym,narg,nret)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node
    integer,intent(in):: sym,narg,nret
    type(pm_ptr):: procs,svect
    procs=find_sig(coder,node,&
         pm_fast_name(coder%context,int(sym,pm_p))&
         ,pm_null_obj,narg,nret,0,svect)
    call make_full_call(coder,cblock,node,&
         procs,narg,nret,0,0)
  contains
    include 'fname.inc'
  end subroutine make_sys_call

  ! Make a procedure call code node
  subroutine make_full_call(coder,cblock,node,procs,narg,nret,nkeys,iflag)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,node,procs
    integer,intent(in):: narg,nret,nkeys
    integer,intent(in):: iflag
    type(pm_ptr):: p,q,n
    integer i
    call make_code(coder,node,cnode_is_arglist,narg+nret+nkeys)
    call code_val(coder,cblock)
    call code_val(coder,procs)
    call code_num(coder,int(iflag,pm_p))
    call code_null(coder)
    call code_num(coder,int(nret,pm_p))
    call code_num(coder,int(nkeys,pm_p))
    coder%index=coder%index+1
    call code_num(coder,coder%index)
    call make_code(coder,node,cnode_is_call,call_node_size)
    n=top_code(coder)
    p=cnode_get(cblock,cblock_last_call)
    if(pm_fast_isnull(p)) then
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_first_call,pm_ln),n)
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_last_call,pm_ln),n)
    else
       call pm_verify_ptr(p,'make-full-call-p')
       call pm_verify_ptr(n,'make-full-call-n')
       call pm_ptr_assign(coder%context,p,int(call_link,pm_ln),n)
       call pm_ptr_assign(coder%context,cblock,&
            int(cblock_last_call,pm_ln),n)
    endif
    n=pop_code(coder)
  contains
    include 'fisnull.inc'
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
  contains
    include 'fisnull.inc'
  end function make_cblock

  ! Close name space associated with cblock
  subroutine close_cblock(coder,cblock)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    coder%top=cblock%data%ptr(cblock%offset+cblock_start)%offset
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
  subroutine check_node(ptr,n)
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
  end subroutine check_node

  ! Get argument n from code node
  function cnode_arg(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr):: val
    if(pm_debug_level>2) call check_node(ptr,n)
    val=ptr%data%ptr(ptr%offset+cnode_args+n-1)
  end function cnode_arg

  ! Get argument n from code node
  function cnode_get(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr):: val
    if(pm_debug_level>2) call check_node(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)
  end function cnode_get

  ! Get argument n from code node
  subroutine cnode_set(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    type(pm_ptr),intent(in):: val
    if(pm_debug_level>2) call check_node(ptr,n)
    ptr%data%ptr(ptr%offset+n)=val
  end subroutine  cnode_set

  ! Get argument n from code node
  function cnode_get_num(ptr,n) result(val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer(pm_p):: val
    if(pm_debug_level>2) call check_node(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
  end function cnode_get_num

  ! Set argument n in code node
  subroutine cnode_set_num(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer(pm_p),intent(in):: val
    if(pm_debug_level>2) call check_node(ptr,n)
    ptr%data%ptr(ptr%offset+n)%offset=val
  end subroutine  cnode_set_num

  ! Increment argument n from code node
  subroutine cnode_incr_num(ptr,n,val)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n
    integer(pm_p),intent(in):: val
    if(pm_debug_level>2) call check_node(ptr,n)
    ptr%data%ptr(ptr%offset+n)%offset=&
         ptr%data%ptr(ptr%offset+n)%offset+val
  end subroutine  cnode_incr_num

  ! Increment argument n from code node
  subroutine cnode_set_flags(ptr,n,val)
    type(pm_ptr),intent(inout):: ptr
    integer,intent(in):: n
    integer(pm_p),intent(in):: val
    if(pm_debug_level>2) call check_node(ptr,n)
    ptr%data%ptr(ptr%offset+n)%offset=ior(&
         ptr%data%ptr(ptr%offset+n)%offset,val)
  end subroutine  cnode_set_flags

  ! Check all given flags clear
  function cnode_flags_clear(ptr,n,flags) result(ok)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n,flags
    logical:: ok
    integer(pm_p):: val
    if(pm_debug_level>2) call check_node(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
    ok=iand(flags,val)==0
  end function cnode_flags_clear

  ! Check all given flags set
  function cnode_flags_set(ptr,n,flags) result(ok)
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: n,flags
    logical:: ok
    integer(pm_p):: val
    if(pm_debug_level>2) call check_node(ptr,n)
    val=ptr%data%ptr(ptr%offset+n)%offset
    ok=iand(flags,val)==flags
  end function cnode_flags_set

  ! Get kind (cnode_is...) of a code node
  function cnode_get_kind(ptr) result(n)
    type(pm_ptr),intent(in):: ptr
    integer:: n
    if(pm_debug_level>2) call check_node(ptr,0)
    n=ptr%data%ptr(ptr%offset)%offset
  end function cnode_get_kind

  ! Get number of arguments to a code node
  function cnode_numargs(ptr) result(n)
    type(pm_ptr),intent(in):: ptr
    integer:: n
    if(pm_debug_level>2) call check_node(ptr,0)
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
            cnode_get_num(node,var_index)
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
                  int(p%data%i16(p%offset),pm_p),str)
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
        do i=1,cnode_numargs(node),2
          call dump_type(coder%context,iunit,&
               int(cnode_get_num(node,cnode_args+i-1),pm_i16),depth+1)
          write(iunit,*) spaces(1:depth*2+2),cnode_get_num(node,cnode_args+i)
        enddo
        write(iunit,*) spaces(1:depth*2),')'
     case(cnode_is_arglist)
        write(iunit,*) spaces(1:depth*2),'Sig List(',cnode_numargs(node)
        do i=2,cnode_numargs(node),2
          call dump_type(coder%context,iunit,&
               int(cnode_get_num(node,cnode_args+i-1),pm_i16),depth+1)
          call dump_code_tree(coder,rvec,iunit,cnode_arg(node,i+1),depth+1)
        enddo
        write(iunit,*) spaces(1:depth*2),')'
    case default 
       write(iunit,*) spaces(1:depth*2),'<<Unknown Cnode!!!>>'
    end select
  contains
    include 'fisnull.inc'
    include 'fistiny.inc'
  end subroutine  dump_code_tree

  ! Dump type tree (debugging)
  recursive subroutine dump_type(context,iunit,tno,depth)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,depth
    integer(pm_i16),intent(in):: tno
    integer(pm_i16):: u
    character(len=100),parameter:: spaces=' '
    character(len=100):: str
    character(len=6),dimension(11),parameter:: typ_names= (/ &
         'user  ','any   ','struct','rec   ','array ',&
         'null  ','tuple ','vtuple','name  ',&
         'proc  ','ambig '/)
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
    if(u<1.or.u>9) then
       write(str,'("Unknown type kind",i4)') u
    else
       str=typ_names(u)
    endif
    if(u==pm_typ_is_user.or.u==pm_typ_is_struct.or.u==pm_typ_is_rec) then
       call pm_name_string(context,int(t%data%i16(t%offset+1),pm_p),&
            str(len_trim(str)+2:))
    endif
    if(pm_fast_esize(t)<2) then
       write(iunit,*) spaces(1:depth*2),trim(str),tno,pm_fast_esize(t)
    else
       write(iunit,*) spaces(1:depth*2),trim(str),' (',tno,pm_fast_esize(t)
       do i=2,pm_fast_esize(t)
          call dump_type(context,iunit,t%data%i16(t%offset+i),depth+1)
       enddo
       write(iunit,*) spaces(1:depth*2),')'
    endif
    if(u==pm_typ_is_user) then
       t=pm_dict_val(context,context%tcache,int(tno,pm_ln))
       if(.not.pm_fast_isnull(t)) then
          write(iunit,*) spaces(1:depth*2),'Includes('
          do i=1,t%data%i16(t%offset)
             call dump_type(context,iunit,t%data%i16(t%offset+i),depth+1)
          enddo
          write(iunit,*) spaces(1:depth*2),')'
       endif
    endif
  contains
    include 'fesize.inc'  
    include 'fisnull.inc'
  end subroutine dump_type

  function sig_name(coder,m) result(name)
    type(code_state),intent(in):: coder
    integer,intent(in):: m
    integer(pm_p):: name
    type(pm_ptr):: key
    key=pm_dict_key(coder%context,coder%sig_cache,int(m,pm_ln))
    name=key%data%i16(key%offset)
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
       write(iunit,*) 'Node:',code%data%hash,code%offset
       call pm_name_string(coder%context,&
            int(sig%data%i16(sig%offset),pm_p),str)
       call pm_name_string(coder%context,&
            int(sig%data%i16(sig%offset+1),pm_p),str2)
       write(iunit,*) 'Sig(',trim(str),&
            ' Amplocs=',trim(str2),&
            ' nret=',sig%data%i16(sig%offset+2),&
            'flags=',sig%data%i16(sig%offset+3),') ('
       do j=3,cnode_numargs(code),2
          typ=cnode_arg(code,j)
          write(iunit,*) 'Type:'
          call dump_type(coder%context,iunit,int(typ%offset,pm_i16),2)
          write(iunit,*) 'Code:',j,cnode_numargs(code)
          call dump_code_tree(coder,pm_null_obj,iunit,cnode_arg(code,j+1),2)
       enddo
       write(iunit,*) ')'
    enddo
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
    stop
    coder%num_errors=coder%num_errors+1
    if(coder%num_errors>max_code_errors) &
         stop 'Too many errors - compilation terminated'
  end subroutine cnode_error

end module pm_codegen

