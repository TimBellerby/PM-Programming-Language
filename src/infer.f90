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

! Simple feed-forward type inference
module pm_infer
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_types
  use pm_parser
  use pm_sysdefs
  use pm_codegen
  implicit none
contains
  
  !============================================================
  ! The following routines process the intermediate code tree 
  ! applying type inference and resolving polymorphic procedure 
  ! calls at compile time if possible
  !=============================================================

  ! Type-infer main program
  subroutine prc_prog(coder)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: cnode
    integer:: i
    coder%stack(1)=0
    do i=2,coder%index+2
       coder%stack(i)=-1
    enddo
    coder%top=coder%index+2
    call prc_cblock(coder,top_code(coder),2)
    if(coder%stack(1)/=0) then
       stop 'Program contains infinite recursion'
    endif
    call code_val(coder,pm_fast_newnc(coder%context,pm_int16,&
         int(coder%top-1,pm_p)))
    cnode=top_code(coder)
    cnode%data%i16(cnode%offset:cnode%offset+coder%top-2)=&
         coder%stack(2:coder%top)
    call make_code(coder,pm_null_obj,cnode_is_single_proc,2)
    if(pm_debug_level>2) write(*,*) 'END OF PROG> vtop=',coder%vtop
  contains
    include 'fnewnc.inc'  
  end subroutine  prc_prog

  ! Type-infer proc
  function prc_proc(coder,prc,atype,ptype,oldbase,break) result(rtype)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: prc
    integer(pm_i16),intent(in):: atype,ptype
    integer,intent(in):: oldbase
    logical,intent(out):: break
    type(pm_ptr):: cnode,cac
    integer(pm_i16):: rtype
    integer(pm_i16):: at
    integer(pm_i16),dimension(3):: key
    integer:: i,base
    integer(pm_ln):: k

    break=.false.

    ! Is this combination already cached? 
    key(1)=cnode_get_num(prc,pr_id)
    key(2)=atype
    k=pm_ivect_lookup(coder%context,coder%proc_cache,key,2)
    if(pm_debug_level>2) &
         write(*,*) 'PRC PROC>',key(1),key(2),k
    if(k>0) then
       cnode=pm_dict_val(coder%context,coder%proc_cache,k)
       if(pm_fast_istiny(cnode)) then
          if(cnode%offset==-3) then
             goto 10
          elseif(cnode%offset<0) then
             call pm_dict_set_val(coder%context,coder%proc_cache,&
                  k,pm_fast_tinyint(coder%context,-2))
             break=.true.
          else
             rtype=cnode%offset
             if(pm_debug_level>2) write(*,*) 'CAC RTN>',rtype
             call code_num(coder,int(k,pm_p))
          endif
          return
       endif
       call code_num(coder,int(k,pm_p))
       cnode=cnode_arg(cnode,2)
       rtype=cnode%data%i16(cnode%offset)
       return
    endif
 10 continue
    ! Check for recursion
    if(cnode_get_num(prc,pr_recurse)/=0.and.ptype/=atype) then
       ! Do not specialise arguments for direct recursion
       at=ptype
       ! Is this combination already cached? 
       key(2)=at
       k=pm_ivect_lookup(coder%context,coder%proc_cache,key,2)
       if(k>0) then
          cnode=pm_dict_val(coder%context,coder%proc_cache,k)
          if(pm_fast_istiny(cnode)) then
             if(cnode%offset==-3) then
                goto 20
             elseif(cnode%offset<0) then
                call code_num(coder,int(k,pm_p))
                call pm_dict_set_val(coder%context,coder%proc_cache,&
                     k,pm_fast_tinyint(coder%context,-2))
                break=.true.
                return
             else
                call code_num(coder,int(k,pm_p))
                rtype=cnode%offset
                return
             endif
          endif
          call code_num(coder,int(k,pm_p))
          cnode=cnode_arg(cnode,2)
          rtype=cnode%data%i16(cnode%offset)
          return
       endif
    else
       at=atype       
    endif
20  continue
    ! Flag call to check for recursion
    call cnode_incr_num(prc,pr_recurse,1_pm_p)
    k=pm_idict_add(coder%context,coder%proc_cache,&
         key,2,pm_fast_tinyint(coder%context,-1))
    base=coder%top+2
    do
       if(pm_debug_level>2) write(*,*) 'TRY',key(1),key(2),rtype
       ! Set up type inference frame
       coder%top=base+cnode_get_num(prc,pr_max_index)
       if(coder%top>max_code_stack) &
            call pm_panic('Program too complex (pc-proc)')
       coder%stack(base-1)=0
       coder%stack(base)=at
       do i=base+1,coder%top
          coder%stack(i)=-1
       enddo
       ! Process code
       call prc_cblock(coder,cnode_arg(prc,1),base)
       cnode=pm_dict_val(coder%context,coder%proc_cache,k)
       if(.not.pm_fast_istiny(cnode)) call pm_panic('prc-proc bad cac')
       if(pm_debug_level>2) &
            write(*,*) 'TRY COMPLETE>',cnode%offset,coder%stack(base),coder%stack(base-1)
       if(cnode%offset==-1) then
          rtype=coder%stack(base)
          exit
       else if(cnode%offset<=-2) then
          if(coder%stack(base)<0) then
             coder%stack(oldbase-1)=1
             coder%top=base-2
             cnode%offset=-3
             call pm_dict_set_val(coder%context,&
                  coder%proc_cache,k,cnode)
             break=.true.
             return
          endif
          cnode%offset=coder%stack(base)
          call pm_dict_set_val(coder%context,coder%proc_cache,k,cnode)
       else 
          if(pm_debug_level>2) write(*,*) 'RT>',rtype,coder%stack(base)
          rtype=cnode%offset
          if(pm_typ_includes(coder%context,rtype,&
               coder%stack(base))) then
             exit
          else
             if(pm_typ_includes(coder%context,&
                  coder%stack(base),rtype)) then
                cnode%offset=coder%stack(base)
             else
                cnode%offset=make_ambig_type(coder,rtype,&
                     coder%stack(base))
                if(cnode%offset==0) then
                   rtype=0
                   exit
                endif
             endif
             call pm_dict_set_val(coder%context,&
                  coder%proc_cache,k,cnode)
          endif
       endif
    enddo
    if(pm_debug_level>2) &
         write(*,*) 'COMPLETED>',coder%stack(base)
    ! Pass a break out
    if(coder%stack(base-1)==1) then
       coder%stack(oldbase-1)=1
       coder%top=base-2
       cnode%offset=-3
       call pm_dict_set_val(coder%context,&
            coder%proc_cache,k,cnode)
       if(rtype<0) then
          break=.true.
       else
          call code_num(coder,int(k,pm_p))
       endif
       return
    endif

    ! Create record of type-annotated code
    call code_val(coder,prc)
    call code_val(coder,pm_fast_newnc(coder%context,pm_int16,&
         int(coder%top-base+1,pm_p)))
    cnode=top_code(coder)
    cnode%data%i16(cnode%offset:cnode%offset+coder%top-base)=&
         coder%stack(base:coder%top)
    call make_code(coder,pm_null_obj,cnode_is_single_proc,2)
    k=pm_idict_add(coder%context,coder%proc_cache,&
         key,2,top_code(coder))
    call drop_code(coder)
    call code_num(coder,int(k,pm_p))
    coder%top=base-2
    call cnode_incr_num(prc,pr_recurse,-1_pm_p)
  contains
    include 'fnewnc.inc'
    include 'fistiny.inc'
    include 'ftiny.inc'
  end function prc_proc

  ! Type infer builtin function 
  function prc_builtin(coder,prc,atype,ptype) result(rtype)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: prc
    integer(pm_i16),intent(in):: atype,ptype
    integer(pm_i16):: rtype
    type(pm_ptr):: p
    integer:: base,i
    integer(pm_i16),dimension(2):: key
    integer(pm_p):: k,sym
    type(pm_ptr):: tv
    p=cnode_get(prc,bi_rcode)
    if(pm_fast_isnull(p)) then
       rtype=cnode_get_num(prc,bi_rtype)
    else
       ! Set up type inference frame
       base=coder%top+1
       coder%top=base+cnode_get_num(prc,cnode_args+1)
       coder%stack(base)=atype
       do i=base+1,coder%top
          coder%stack(i)=-1
       enddo
       ! Process code
       call prc_cblock(coder,cnode_get(prc,bi_rcode),base)
       rtype=coder%stack(base)
       coder%top=base-1
       sym=cnode_get_num(prc,bi_rsym)
       select case(sym)
       case(sym_hash,sym_mult)
          tv=pm_typ_vect(coder%context,rtype)
          tv=pm_typ_vect(coder%context,pm_tv_arg(tv,1))
          if(pm_tv_kind(tv)/=pm_typ_is_array) &
               call pm_panic('Not array in prc-builtin')
          if(sym==sym_hash) then
             rtype=unit_vector(pm_tv_arg(tv,2))
          else
             rtype=unit_vector(pm_tv_arg(tv,1))
          endif
       case(sym_gt)
          tv=pm_typ_vect(coder%context,rtype)
          rtype=unit_vector(max(pm_tv_arg(tv,1),pm_tv_arg(tv,2)))
       case(sym_dim)
          tv=pm_typ_vect(coder%context,rtype)
          rtype=unit_vector(make_array_type(coder,pm_tv_arg(tv,1),pm_tv_arg(tv,2)))
       end select
    endif
    key(1)=cnode_get_num(prc,bi_id)
    k=pm_idict_add(coder%context,&
         coder%proc_cache,key,1,prc)
    call code_num(coder,k)
  contains
    include 'fisnull.inc'
    function unit_vector(t) result(u)
      integer(pm_i16),intent(in):: t
      integer(pm_i16):: u
      call push_type(coder,pm_typ_is_tuple)
      call push_type(coder,0_pm_i16)
      call push_type(coder,t)
      call make_type(coder,3)
      u=pop_type(coder)
    end function unit_vector
  end function prc_builtin

  ! Type infer code block
  subroutine prc_cblock(coder,cblock,base)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: cblock
    integer,intent(in):: base
    integer:: nvars,i,newbase
    logical:: break
    type(pm_ptr):: p
    p=cnode_get(cblock,cblock_first_call)
    do while(.not.pm_fast_isnull(p))
       call prc_call(coder,cblock,p,base,break)
       if(break) exit
       p=cnode_get(p,call_link)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine prc_cblock

  ! Type infer call
  subroutine prc_call(coder,cblock,callnode,base,break)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: callnode,cblock
    integer,intent(in):: base
    logical,intent(out):: break
    integer(pm_i16):: sig,tno,tno2,name,off
    type(pm_ptr):: p,t,list,list2
    integer:: i,j,nret,narg,slot,slot2,tbase
    integer:: vbase_check,tbase_check
    logical:: ok,mayfail
    character*100 str
    if(pm_debug_level>0) then
       vbase_check=coder%vtop
       tbase_check=coder%ttop
    endif
    break=.false.
    nret=cnode_get_num(callnode,call_nret)
    sig=cnode_get_num(callnode,call_sig)
    p=cnode_get(callnode,call_args)
    narg=cnode_numargs(p)-nret
    if(sig<0) then
       if(pm_debug_level>2) then
          write(*,*) 'PROCESS CALL>',sym_names(-sig),&
               'ttop=',coder%ttop,'vtop=',coder%vtop
       endif
       select case(-sig)
       case(sym_while)
          list=cnode_arg(p,1)
          list2=cnode_arg(p,3)
          do
             call clear_cblock_mark(list)
             call clear_cblock_mark(list2)
             call prc_cblock(coder,list,base)
             call check_logical(2)
             call prc_cblock(coder,list2,base)
             if(.not.(cblock_marked(list).or.&
                  cblock_marked(list2))) exit
          enddo
       case(sym_repeat)
          list=cnode_arg(p,1)
          do 
             call clear_cblock_mark(list)
             call prc_cblock(coder,list,base)
             if(.not.cblock_marked(list)) exit
          enddo
          call check_logical(2)
       case(sym_if)
          call check_logical(1)
          list=cnode_arg(p,2)
          call prc_cblock(coder,list,base)
          list=cnode_arg(p,3)
          if(.not.pm_fast_isnull(list)) &
               call prc_cblock(coder,list,base)
       case(sym_do,sym_found)
          call prc_cblock(coder,cnode_arg(p,1),base)
       case(sym_loop)
          coder%stack(get_slot(1))=pm_long
          call prc_cblock(coder,cnode_arg(p,3),base)
       case(sym_find)
          coder%stack(get_slot(1))=pm_long
          call prc_cblock(coder,cnode_arg(p,3),base)
          call prc_cblock(coder,cnode_arg(p,4),base)
       case(sym_par_loop,sym_par_find)
          slot=get_slot(1)
          coder%stack(slot)=pm_long
          call prc_cblock(coder,cnode_arg(p,nret+1),base)
          if(sig==-sym_par_find) &
               call prc_cblock(coder,cnode_arg(p,nret+2),base)
       case(sym_define)
          slot=get_slot(1)
          slot2=get_slot_or_type(2)
          if(slot2>0) then
             coder%stack(slot)=var_type(coder%stack(slot2))
          else
             coder%stack(slot)=-slot2
          endif
       case(sym_assign)
          call full_call(coder%assign_sig)   
       case(sym_arrow)
          call combine_types(coder,base,callnode,&
               cnode_arg(p,1),arg_type(2))
       case(sym_import)
          slot=get_slot(1)
          coder%stack(slot)=arg_type(2)
       case(sym_export)
          slot=get_slot(1)
          tno=arg_type(3)
          coder%stack(slot)=make_array_type(coder,tno,int(pm_long,pm_i16))
       case(sym_for)
          call unrestrict(1)
          do i=2,nret
             call get_slot_and_type(i+nret-1,slot,tno)
             if(tno==0) then
                call unrestrict(i)
             else
                call find_array_elems(cnode_arg(p,i),tno,ok)
                if(.not.ok) call cnode_error(coder,callnode,&
                  'Expression not an array expression')
             endif
          enddo
       case(sym_struct,sym_rec)
          if(-sig==sym_struct) then
             call push_type(coder,pm_typ_is_struct)
          else
             call push_type(coder,pm_typ_is_rec)
          endif
          t=cnode_arg(p,2)
          call push_type(coder,int(cnode_get_num(t,cnode_args),pm_i16))
          slot=get_slot(1)
          tno=coder%stack(slot)
          if(tno>=0) then
             t=pm_typ_vect(coder%context,tno)
             if(pm_tv_kind(t)==pm_typ_is_ambig) then
                if(pm_tv_name(t)>max_ambig_type-2) then
                   do i=2,narg
                      call push_type(coder,0_pm_i16)
                   enddo
                else
                   do i=2,narg
                      call push_type(coder,arg_type(1+i))
                   enddo
                endif
             endif
          else
             do i=2,narg
                call push_type(coder,arg_type(1+i))
             enddo
          endif
          call make_type(coder,narg+1)
          call combine_types(coder,base,callnode,&
               cnode_arg(p,1),pop_type(coder))
       case(sym_dot,sym_dotref)
          call resolve_elem
       case(sym_set_dot,sym_set_dot_index,sym_set_dot_open_index)
          tno=arg_type(1)
          name=cnode_get_num(cnode_arg(p,3),cnode_args)
          if(sig/=-sym_set_dot) then
             call pm_array_elem_offset(coder%context,tno,name,.true.,off,tno2)
          else
             call pm_elem_offset(coder%context,tno,name,.true.,off,tno2)
          endif
          i=base+cnode_get_num(callnode,call_index)
          coder%stack(i)=off
          if(off<0) then
             ok=.false.
             call resolve_struct_elems(pm_null_obj,tno,sym_dotref,int(name,pm_p),sig/=sym_set_dot)
             if(.not.ok) then
                call cnode_error(coder,callnode,'Value being set cannot have element: ',&
                     cnode_arg(cnode_arg(p,3),1))
             endif
          endif
       case(sym_any)
          tno=cnode_get_num(p,cnode_args+1)
          call get_slot_and_type(3,slot2,tno2)
          if(.not.pm_typ_intersects(coder%context,tno,tno2)) then
             call cnode_error(coder,callnode,&
                  '"any" has incompatible argument type')
          endif
          slot=get_slot(1)
          coder%stack(slot)=make_any_type(coder,tno)
       case(sym_check)
          if(narg==1) then
             call check_logical(1)
          else
             call full_call(coder%assign_sig)
          endif
       case(sym_open)
          t=pm_typ_vect(coder%context,coder%stack(base))
          do i=1,narg
             slot=get_slot(i)
             coder%stack(slot)=pm_tv_arg(t,i)
             if(pm_debug_level>2) &
                  write(*,*) 'PARAM',i,slot,pm_tv_arg(t,i),pm_tv_numargs(t)
          enddo
          coder%stack(base)=-1       
       case(sym_result)
          call get_arg_types
          call make_type(coder,narg+2)
          coder%stack(base)=pop_type(coder)
       case default
          if(-sig>=0.and.-sig<=num_sym) then
             write(*,*) sym_names(-sig)
          else
             write(*,*) 'Sym no:',-sig
          endif
          call pm_panic('Unexpected call symbol')
       end select
    else
       if(pm_debug_level>2) then
          write(*,*) 'FULL CALL>',sig_name_str(coder,int(sig))
       endif
       call full_call(int(sig,pm_p))
    endif
    if(pm_debug_level>0) then
       if(vbase_check/=coder%vtop) then
          write(*,*) 'MISMATCH-vstack',coder%vtop,vbase_check
          call pm_panic('prc_call')
       endif
       if(tbase_check/=coder%ttop) then
          write(*,*) 'MISMATCH-tstack',coder%ttop,tbase_check
          call pm_panic('prc_call')
       endif
    endif
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'fname.inc'

    subroutine full_call(sig)
      integer(pm_p),intent(in):: sig
      integer:: i,j,start,slot,pcheck
      integer(pm_i16):: rt
      type(pm_ptr):: v,procs,proc,rtvect
      integer(pm_i16):: pars,apars,k,key(1)
      logical:: allin,ok,varg
      call get_slots_and_types
      varg=cnode_flags_set(callnode,call_flags,call_is_vararg)
      procs=pm_dict_val(coder%context,&
           coder%sig_cache,int(sig,pm_ln))
      start=coder%vtop
      do j=1,nret
         coder%stack(get_slot(j))=-1
      enddo
      do i=3,cnode_numargs(procs),2
         pars=cnode_get_num(procs,cnode_args+i-1)
         apars=check_call_sig(coder,&
              pars,narg,varg,allin)
         if(apars>=0) then
            call code_num(coder,int(pars,pm_p))
            proc=cnode_arg(procs,i+1)
            if(cnode_get_kind(proc)==cnode_is_builtin) then
               rt=prc_builtin(coder,proc,apars,pars)
            else
               pcheck=coder%vtop
               rt=prc_proc(coder,proc,apars,pars,base,break)
               if(break) then
                  if(pm_debug_level>2) &
                       write(*,*) 'BREAK>',coder%vtop,start
                  coder%stack(base-1)=1
                  coder%ttop=coder%ttop-narg
                  coder%vtop=start
                  return
               else
                  if(coder%vtop/=pcheck+1) call pm_panic('pcheck mismatch')
               endif
            endif
            if(nret>0) then
               rtvect=pm_typ_vect(coder%context,rt)
               do j=1,nret
                  v=cnode_arg(p,j)
                  call combine_types(coder,base,callnode,v,&
                       pm_tv_arg(rtvect,j))
               enddo
            endif
            if(allin) exit
         else
            if(pm_debug_level>2) write(*,*) 'REJECTED>'
         endif
      enddo
      if(coder%vtop>start+2) then
         call make_code(coder,pm_null_obj,cnode_is_multi_proc,&
              coder%vtop-start)
         key(1)=pm_dict_size(coder%context,coder%proc_cache)
         k=pm_idict_add(coder%context,coder%proc_cache,key,&
              1,top_code(coder))
         call drop_code(coder)
      else if(coder%vtop==start) then
         if(sig==coder%assign_sig) then
            call cnode_error(coder,callnode,&
                 'No matching assignment procedure')
         else
            call cnode_error(coder,callnode,&
                 'No matching procedure',&
                 pm_fast_name(coder%context,sig_name(coder,sig)))
         endif
         do i=1,nret
            call unrestrict(i)
         enddo
      else
         k=coder%vstack(coder%vtop)%offset
         call drop_code(coder)
         call drop_code(coder)
      endif
      slot=base+cnode_get_num(callnode,call_index)
      coder%stack(slot)=k
      coder%ttop=coder%ttop-narg
    end subroutine full_call

    subroutine get_arg_types
      integer:: i,j
      type(pm_ptr):: v
      if(coder%ttop+narg+2>max_code_stack) &
           call pm_panic('Program too complex')
      coder%tstack(coder%ttop+1)=pm_typ_is_tuple
      coder%tstack(coder%ttop+2)=0
      do i=1,narg
         coder%tstack(coder%ttop+i+2)=arg_type(nret+i)
      enddo
      coder%ttop=coder%ttop+narg+2
    end subroutine get_arg_types

    function arg_type(m) result(tno)
      integer,intent(in):: m
      integer(pm_i16):: tno
      integer:: slot
      type(pm_ptr):: v
      slot=get_slot_or_type(m)
      if(slot<0) then
         tno=-slot
      else
         tno=coder%stack(slot)
         if(pm_debug_level>0) then
            if(tno==-1) &
                 call pm_panic('Broken type resolution chain')
         endif
      endif
    end function arg_type

    subroutine get_slots_and_types
      integer:: i
      if(coder%ttop+narg>max_code_stack) &
           call pm_panic('Program too complex')
      do i=1,narg
         coder%tstack(coder%ttop+i)=get_slot_or_type(i+nret)
      enddo
      coder%ttop=coder%ttop+narg
    end subroutine get_slots_and_types

    function get_slot_or_type(m) result(slotno)
      integer,intent(in):: m
      integer:: slotno
      type(pm_ptr):: v
      v=cnode_arg(p,m)
      if(cnode_get_kind(v)==cnode_is_const) then
         slotno=-cnode_get_num(v,cnode_args+1)
         if(slotno>0) &
              slotno=-pm_fast_typeof(cnode_arg(v,1))
      else
         slotno=cnode_get_num(v,var_index)+base
      endif
    end function get_slot_or_type

    subroutine get_slot_and_type(m,slot,tno)
      integer,intent(in):: m
      integer,intent(out):: slot
      integer(pm_i16),intent(out):: tno
      slot=get_slot_or_type(m)
      if(slot<0) then
         tno=-slot
      else
         tno=coder%stack(slot)
         if(pm_debug_level>0) then
            if(tno==-1) &
                 call pm_panic('Broken type resolution chain')
         endif
      endif
    end subroutine get_slot_and_type

    function get_slot(m) result(slotno)
      integer,intent(in):: m
      integer:: slotno
      type(pm_ptr):: v
      v=cnode_arg(p,m)
      if(pm_debug_level>0) then
         if(cnode_get_kind(v)/=cnode_is_var) &
              call pm_panic('get_slot')
      endif
      slotno=cnode_get_num(v,var_index)+base
    end function get_slot

    subroutine check_logical(m)
      integer,intent(in):: m
      integer:: slt
      integer(pm_i16):: ty
      integer:: i
      type(pm_ptr):: tv
      if(.not.pm_typ_includes(coder%context,arg_type(m),int(pm_logical,pm_i16))) then
         call cnode_error(coder,callnode,&
              'Expecting boolean expression')
      endif
    end subroutine check_logical

    subroutine clear_cblock_mark(list)
      type(pm_ptr),intent(in):: list
      integer:: slot
      slot=base+cnode_get_num(list,cblock_index)
      coder%stack(slot)=0
    end subroutine clear_cblock_mark

    function cblock_marked(list) result(marked)
      type(pm_ptr),intent(in):: list
      logical:: marked
      integer:: slot
      slot=base+cnode_get_num(list,cblock_index)
      marked=coder%stack(slot)/=0
    end function cblock_marked
  
    recursive function var_type(tno) result(newtno)
      integer(pm_i16),intent(in):: tno
      integer(pm_i16):: newtno
      integer(pm_i16):: t1,t2,t3,t4
      integer:: i,n
      logical:: changed
      type(pm_ptr):: tv
      if(tno==0) return
      tv=pm_typ_vect(coder%context,tno)
      select case(pm_tv_kind(tv))
      case(pm_typ_is_ambig)
         t1=pm_tv_arg(tv,1)
         t2=var_type(t1)
         if(t1/=t2) then
            t4=var_type(pm_tv_arg(tv,2))
         else
            t3=pm_tv_arg(tv,2)
            t4=var_type(t3)
            if(t3==t4) then
               newtno=tno
               return
            endif
         endif
         call push_type(coder,pm_typ_is_ambig)
         call push_type(coder,pm_tv_name(tv))
         call push_type(coder,t2)
         call push_type(coder,t4)
         call make_type(coder,4)
         newtno=pop_type(coder)
      case(pm_typ_is_struct,pm_typ_is_rec)
         n=pm_tv_numargs(tv)
         if(coder%ttop+n+2>max_code_stack) &
              call pm_panic('Program too complex')
         coder%tstack(coder%ttop+1)=pm_typ_is_struct
         coder%tstack(coder%ttop+2)=pm_tv_name(tv)
         coder%ttop=coder%ttop+2
         changed=.false.
         do i=1,n
            t1=pm_tv_arg(tv,i)
            t2=var_type(t1)
            coder%ttop=coder%ttop+1
            coder%tstack(coder%ttop)=t2
            if(t1/=t2) changed=.true.
         enddo
         if(changed) then
            call make_type(coder,n+2)
            newtno=pop_type(coder)
         else
            coder%ttop=coder%ttop-2-n
            newtno=tno
         endif
      case(pm_typ_is_single_proc)
         newtno=pm_proc
      case(pm_typ_is_single_name)
         newtno=pm_name
      case  default
         newtno=tno
      end select
    end function var_type

    function check_slot_intersect(m,n) result(ok)
      integer,intent(in):: m,n
      logical:: ok
      integer:: slt1,slt2,i,j
      integer(pm_i16):: tp1,tp2
      type(pm_ptr):: ty1,ty2
      ok=pm_typ_intersects(coder%context,arg_type(m),arg_type(n))
      if(pm_debug_level>2) write(*,*) 'Check intersect',arg_type(m),arg_type(n),ok
    end function  check_slot_intersect

    subroutine resolve_elem
      integer(pm_i16):: eltyp,offset
      type(pm_ptr):: namep
      j=cnode_get_num(callnode,call_index)
      namep=cnode_arg(cnode_arg(p,3),1)
      name=namep%offset
      tno=arg_type(2)
      if(tno==0) then
         coder%stack(base+j)=0
         call unrestrict(1)
      else
         call pm_elem_offset(coder%context,tno,name,sig==-sym_dotref,offset,eltyp)
         coder%stack(base+j)=offset
         if(offset<0_pm_i16) then
            call resolve_struct_elems(cnode_arg(p,1),tno,int(-sig,pm_p),int(name,pm_p),.false.)
         else
            call combine_types(coder,base,callnode,cnode_arg(p,1),eltyp)
         endif
      endif
    end subroutine resolve_elem

    recursive subroutine resolve_struct_elems(&
         var,typ,which,name,array)
      type(pm_ptr),intent(in):: var
      integer(pm_i16),intent(in):: typ
      integer(pm_p),intent(in):: name,which
      logical,intent(in):: array
      type(pm_ptr):: tv,nv,set
      integer(pm_i16):: tk,tno
      integer:: tbase,i
      if(typ==0) then
         if(.not.pm_fast_isnull(var)) &
              call combine_types(coder,base,callnode,var,0_pm_i16)
         return
      endif
      tv=pm_typ_vect(coder%context,typ)
      tk=pm_tv_kind(tv)
      select case(tk)
      case(pm_typ_is_struct,pm_typ_is_rec)
         if(which==sym_dotref.and.tk==pm_typ_is_rec) return
         if(array) return
         nv=pm_name_val(coder%context,int(pm_tv_name(tv),pm_p))
         if(nv%data%i16(nv%offset+1)>name) return
         if(nv%data%i16(nv%offset+pm_fast_esize(nv))<name) return
         do i=1,pm_fast_esize(nv)
            if(nv%data%i16(nv%offset+i)==name) then
               tno=pm_tv_arg(tv,int(i))
               if(.not.pm_fast_isnull(var))&
                    call combine_types(coder,base,callnode,var,tno)
               ok=.true.
               return
            endif
         enddo
      case(pm_typ_is_array)
         if(.not.array) return
         call resolve_struct_elems(var,pm_tv_arg(tv,1),sym_dot,name,.false.)
      case(pm_typ_is_user)
         set=pm_dict_val(coder%context,&
              coder%context%tcache,int(typ,pm_ln))
         if(pm_fast_isnull(set)) then
            return
         else
            do i=1,set%data%i16(set%offset)
               call resolve_struct_elems(var,&
                    set%data%i16(set%offset+i),&
                    which,name,array)
            enddo
         endif
      case(pm_typ_is_ambig)
         call resolve_struct_elems(var,pm_tv_arg(tv,1),which,name,array)
         call resolve_struct_elems(var,pm_tv_arg(tv,2),which,name,array)
      end select
    end subroutine resolve_struct_elems

    recursive subroutine find_array_elems(var,typ,ok)
      type(pm_ptr),intent(in):: var
      integer(pm_i16),intent(in):: typ
      logical,intent(inout):: ok
      type(pm_ptr):: tv,set
      integer(pm_i16):: tk
      integer:: i
      tv=pm_typ_vect(coder%context,typ)
      tk=pm_tv_kind(tv)
      select case (tk)
      case(pm_typ_is_array)
         call combine_types(coder,base,callnode,var,&
              pm_tv_arg(tv,1))
         ok=.true.
      case(pm_typ_is_user)
         set=pm_dict_val(coder%context,&
              coder%context%tcache,int(typ,pm_ln))
         if(pm_fast_isnull(set)) then
            return
         else
            do i=1,set%data%i16(set%offset)
               call find_array_elems(var,&
                    set%data%i16(set%offset+i),ok)
            enddo
         endif
      case(pm_typ_is_ambig)
         call find_array_elems(var,pm_tv_arg(tv,1),ok)
         call find_array_elems(var,pm_tv_arg(tv,2),ok)
      end select
    end subroutine find_array_elems

    subroutine unrestrict(m)
      integer:: m
      integer:: slot
      type(pm_ptr):: ptr
      slot=get_slot(m)
      coder%stack(slot)=0
    end subroutine unrestrict

  end subroutine prc_call

  ! Find procedure matching a given call signature
  function check_call_sig(coder,pars,num_args,varg,allin) result(tno)
    type(code_state),intent(inout):: coder
    integer(pm_i16),intent(in):: pars
    integer,intent(in):: num_args
    logical,intent(in):: varg
    logical,intent(out):: allin
    integer(pm_i16):: tno
    integer(pm_i16):: at,pt,slot
    type(pm_ptr):: pv,amb
    integer:: i,rel,n,narg
    narg=num_args
    if(num_args==0) then
       tno=pars
       allin=.true.
       return
    elseif(pars==pm_matched_type) then
       n=2
    else
       pv=pm_typ_vect(coder%context,pars)
       if(pm_debug_level>0) then
          if(pm_tv_kind(pv)/=pm_typ_is_tuple.and.&
               pm_tv_kind(pv)/=pm_typ_is_vtuple) &
               call pm_panic('check-sig')
          if(coder%ttop+narg+2>max_code_stack) &
               call pm_panic('Program too complex (check-sig)')
       endif
       n=pm_tv_numargs(pv)
    endif
    if(n>narg) then
       if(varg) then
          at=top_type(coder)
          do i=narg+1,n
             call push_type(coder,at)
          enddo
          narg=n
       else
          tno=-1
          return
       endif
    endif
    if(pm_debug_level>2) then
       write(*,*) 'Check call sig: ('
       call dump_type(coder%context,6,pars,2)
       write(*,*) '----'
       do i=1,narg
          slot=coder%tstack(coder%ttop-narg+i)
          if(slot>0) then
             at=coder%stack(slot)
          else
             at=-slot
          endif
          call dump_type(coder%context,6,at,2)
       enddo
       write(*,*) ')'
    endif
    if(pars==pm_matched_type) then
       slot=coder%stack(coder%ttop-1)
       if(slot>0) then
          at=coder%stack(slot)
       else
          at=-slot
       endif
       slot=coder%stack(coder%ttop)
       if(slot>0) then
          pt=coder%stack(slot)
       else
          pt=-slot
       endif
       if(at==pt) then
          tno=pars
          allin=.true.
          goto 10
       else if(pm_typ_intersects(coder%context,pt,at)) then
          tno=pars
          allin=.false.
          goto 10
       else
          tno=-1
          allin=.false.
          goto 10
       endif
    endif
    coder%tstack(coder%ttop+1)=pm_typ_is_tuple
    coder%tstack(coder%ttop+2)=0_pm_p
    allin=.true.
    do i=1,narg
       slot=coder%tstack(coder%ttop-narg+i)
       if(slot>0) then
          at=coder%stack(slot)
       else
          at=-slot
       endif
       if(i>n) then
          if(pm_tv_kind(pv)/=pm_typ_is_vtuple) then
             tno=-1
             goto 10
          endif
       else
          pt=pm_tv_arg(pv,i)
       endif
       if(pm_typ_includes(coder%context,&
            pt,at)) then
          coder%tstack(coder%ttop+i+2)=at
       else
          if(pm_typ_intersects(coder%context,&
               pt,at)) then
             coder%tstack(coder%ttop+i+2)=pt
             allin=.false.
          else
             tno=-1
             goto 10
          endif
       endif
    enddo
    tno=new_type(coder,coder%tstack(coder%ttop+1:&
         coder%ttop+narg+2))
10  continue
    if(narg>num_args) coder%ttop=coder%ttop+num_args-narg
  contains
    include 'fisnull.inc'
  end function check_call_sig

  ! Augment the list of types stored in a given variable
  subroutine combine_types(coder,base,callnode,vararg,typ)
    type(code_state):: coder
    integer,intent(in):: base
    type(pm_ptr),intent(in):: callnode,vararg
    integer(pm_i16),intent(in):: typ
    integer:: slot
    integer(pm_i16):: typ0,n
    type(pm_ptr):: tv,p,q,var
    if(typ<0) call pm_panic('combine types')
    var=vararg
    slot=base+cnode_get_num(var,var_index)
    typ0=coder%stack(slot)
    if(typ0<0) then
       coder%stack(slot)=typ
       return
    endif
    if(pm_typ_includes(coder%context,typ0,typ)) return
    if(pm_typ_includes(coder%context,typ,typ0)) then
       coder%stack(slot)=typ
    else
       coder%stack(slot)=make_ambig_type(coder,typ0,typ)
    endif
    p=cnode_get(callnode,call_parent)
    q=cnode_get(var,var_parent)
    do while(.not.p==q)
       slot=base+cnode_get_num(p,cblock_index)
       coder%stack(slot)=1
       p=cnode_get(p,cblock_parent)
       if(pm_fast_isnull(p)) call pm_panic('combine types')
    enddo
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine combine_types

  function make_ambig_type(coder,typ0,typ) result(atyp)
    type(code_state):: coder
    integer(pm_i16),intent(in):: typ0,typ
    integer(pm_i16):: atyp
    integer(pm_i16):: n
    type(pm_ptr):: tv
    n=0
    tv=pm_typ_vect(coder%context,typ0)
    if(pm_tv_kind(tv)==pm_typ_is_ambig) &
         n=pm_tv_name(tv)
    tv=pm_typ_vect(coder%context,typ)
    if(pm_tv_kind(tv)==pm_typ_is_ambig) &
         n=n+pm_tv_name(tv)
    if(n>max_ambig_type) then
       atyp=0
    else
       call push_type(coder,pm_typ_is_ambig)
       call push_type(coder,n)
       call push_type(coder,typ)
       call push_type(coder,typ0)
       call make_type(coder,4)
       atyp=pop_type(coder)
    endif
  end function make_ambig_type

  ! Dump resolved proc signatures (debugging)
  subroutine dump_res_sigs(coder,iunit)
    type(code_state),intent(in):: coder
    integer,intent(in):: iunit
    integer(pm_ln):: i
    do i=1,pm_dict_size(coder%context,coder%proc_cache)
       write(iunit,*) 'Resolved Signature',i,'('
       call dump_code_tree(coder,pm_null_obj,iunit,&
            pm_dict_val(coder%context,coder%proc_cache,i),&
            2)
       write(iunit,*) ')'
    enddo
  end subroutine dump_res_sigs



end module pm_infer
