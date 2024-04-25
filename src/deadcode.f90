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

! Inter and intra-procedural dead code elimination

module pm_deadcode  
  use pm_sysdep
  use pm_compbase
  use pm_kinds
  use pm_memory
  use pm_hash
  use pm_lib
  use pm_symbol
  use pm_types
  use pm_parser
  use pm_sysdefs
  use pm_cnodes
  use pm_codegen
  implicit none

  logical,parameter:: debug_deadcode=.false.
  
contains

  recursive subroutine deadcode_proc(coder,proc)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: proc
    
    
    ! Make sure everything called from here is analysed first
    ! as we will need the parameter liveliness information
    
    
  end subroutine deadcode_proc

  recursive subroutine find_procs_for_cblock(coder,cblock,rvec)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,rvec,procs
    type(pm_ptr):: p
    p=cnode_get(cblock,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       call find_procs_for_call(coder,p,rvec)
       p=cblock_get(p,call_link)
    end do
  contains
    include 'fisnull.inc'
  end subroutine find_procs_for_cblock

  recursive subroutine find_procs_for_call(coder,callnode,rvec)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,rvec
    integer:: sig,idx,i
    type(pm_ptr):: procnode,args,arg
    sig=cnode_get_num(callnode,call_sig)
    if(sig>0) then
       idx=cnode_get_num(callnode,call_index)
       idx=rvec%data%i(rvec%offset+idx)
       procnode=pm_dict_val(coder%context,coder%sig_cache,int(idx,pm_ln))
       if(pm_fast_isnull(cnode_arg(procnode,5))) then
          call deadcode_proc(coder,procnode)
       endif
    else
       args=cnode_get(callnode,call_args)
       do i=1,cnode_numargs(args)
          arg=cnode_arg(args,i)
          if(pm_fast_vkind(arg)==pm_pointer) then
             if(cnode_kind(arg)==cnode_is_cblock) then
                call find_procs_for_cblock(coder,arg,rvec)
             endif
          endif
       enddo
    endif
  end subroutine find_procs_for_call
  
  subroutine dce_cblock(coder,cblock,rvec,alive,nested,eliminate)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock,rvec
    logical,dimension(*):: alive
    logical,intent(in):: nested,eliminate
    type(pm_ptr):: p
    p=cnode_get(cblock,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       call dce_call(coder,p,rvec,alive,eliminate)
       p=cblock_get(p,call_link)
    end do
  contains
    include 'fisnull.inc'
  end subroutine dce_cblock

  subroutine dce_call(coder,callnode,rvec,alive,nested,eliminate)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,rvec
    logical,dimension(*):: alive
    logical,intent(in):: nested,eliminate
    
    args=cnode_get(callnode,call_args)
    if(sig>0) then
       
    else
       select case(-sig)
       case(sym_if,sym_if_invar)
          do while(.not.pm_fast_isnull(p))
             call vpush(coder,p)
          enddo
       case(sym_while)

       case(sym_until,sym_each)
          if(.not.nested) then

          endif
          
       case default:
 
          outs_alive=.false.
          do i=1,nret
             arg=cnode_arg(args,i)
             idx=cnode_get_num(arg,var_index)
             outs_alive=outs_alive.or.alive(idx)
             alive(idx)=.false.
          enddo
          has_blocks=.false.
          do i=nret+1,cnode_numargs(args)
             arg=cnode_arg(args,i)
             if(pm_fast_vkind(arg)==pm_pointer) then
                kind=cnode_kind(arg)
                if(kind==cnode_is_var) then
                   idx=cnode_get_num(arg,var_index)
                   alive(idx)=.true.
                elseif(kind==cnode_is_cblock) then
                   has_blocks=.true.
                endif
             endif
          enddo
          if(has_blocks) then
             do i=1,cnode_numargs(args)
                arg=cnode_arg(args,i)
                if(pm_fast_vkind(arg)==pm_pointer) then
                   if(cnode_kind(arg)==cnode_is_cblock) then
                      call find_procs_for_cblock(coder,arg,rvec)
                   endif
                endif
             enddo
          endif
       end select
    end if
    if(eliminate.and.no_bad_taints.and..not.outs_alive) then
       rev%data%i(rvec%offset+cnode_get_num(callnode,call_index))=&
            call_noop_flag
    endif
  end subroutine dce_call
  
end module pm_deadcode
