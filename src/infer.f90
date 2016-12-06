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

! Simple feed-forward type inference
module pm_infer
  use pm_sysdep
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_types
  use pm_parser
  use pm_sysdefs
  use pm_codegen
  implicit none

  integer,parameter:: max_ambig_type=128
  integer,parameter:: max_recur=8

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

    if(pm_debug_level>2) write(*,*) 'PROCESS PROG>'

    coder%loop_depth=0

    ! Setup resolution stack block
    coder%stack(1)=0
    do i=2,coder%index+2
       coder%stack(i)=-1
    enddo
    coder%top=coder%index+2

    ! Process program code
    call prc_cblock(coder,top_code(coder),2)
    if(coder%stack(1)/=0) then
       if(coder%num_errors==0) then
          call more_error(coder%context,'Error: Procedure has infinite recursion')
          coder%loop_depth=coder%proc_loop_depth
          call infer_trace(coder)
          stop 'Program contains infinite recursion'
       endif
    endif

    ! Create resolved code object
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

    if(pm_debug_level>0) then
       if(cnode_get_kind(prc)/=cnode_is_proc) then
          call pm_panic('prc-proc prc not proc')
       endif
    endif

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
                  k,pm_fast_tinyint(coder%context,int(-2,pm_p)))
             break=.true.
          else
             rtype=cnode%offset
             if(pm_debug_level>2) write(*,*) 'CAC RTN>',rtype
             call code_num(coder,int(k))
          endif
          return
       endif
       call code_num(coder,int(k))
       cnode=cnode_arg(cnode,2)
       rtype=cnode%data%i16(cnode%offset)
       return
    endif

 10 continue

    ! Check for recursion
    if(cnode_get_num(prc,pr_recurse)>max_recur.and.ptype/=atype) then
       ! Do not specialise arguments after max_recur recursions
       at=ptype
       ! Is this combination already cached? 
       key(2)=at
       k=pm_ivect_lookup(coder%context,coder%proc_cache,key,2)
       if(k>0) then
          cnode=pm_dict_val(coder%context,coder%proc_cache,k)
          if(pm_fast_istiny(cnode)) then
             if(cnode%offset==-3) then
                ! Cache entry cleared - carry on
                goto 20
             elseif(cnode%offset<0) then
                ! Recursive call - break out
                call code_num(coder,int(k))
                call pm_dict_set_val(coder%context,coder%proc_cache,&
                     k,pm_fast_tinyint(coder%context,int(-2,pm_p)))
                break=.true.
                return
             else
                ! This proc/arg combination already infered - return
                call code_num(coder,int(k))
                rtype=cnode%offset
                return
             endif
          endif
          call code_num(coder,int(k))
          cnode=cnode_arg(cnode,2)
          rtype=cnode%data%i16(cnode%offset)
          return
       endif
    else
       at=atype       
    endif
20  continue

    ! Flag call to check for recursion
    call cnode_incr_num(prc,pr_recurse,1)
    k=pm_idict_add(coder%context,coder%proc_cache,&
         key,2,pm_fast_tinyint(coder%context,int(-1,pm_p)))
    base=coder%top+2

    ! Repeatedly type infer until complete
    do
       if(pm_debug_level>2) write(*,*) 'TRY>',key(1),key(2),rtype

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

       ! Check  procedure record for recursion/completion
       cnode=pm_dict_val(coder%context,coder%proc_cache,k)
       if(.not.pm_fast_istiny(cnode)) call pm_panic('prc-proc bad cac')
       if(pm_debug_level>2) &
            write(*,*) 'TRY COMPLETE>',cnode%offset,coder%stack(base),coder%stack(base-1)

       if(cnode%offset==-1) then
          ! Not recursively called
          rtype=coder%stack(base)
          exit
       else if(cnode%offset<=-2) then
          ! Recursively called
          if(coder%stack(base)<0) then
             ! No resolved type yet 
             ! clear cache entry
             ! and break out
             coder%stack(oldbase-1)=1
             coder%top=base-2
             cnode%offset=-3
             call pm_dict_set_val(coder%context,&
                  coder%proc_cache,k,cnode)
             coder%proc_loop_depth=coder%loop_depth
             break=.true.
             return
          endif
          ! Have a resolved type - cache it
          cnode%offset=coder%stack(base)
          call pm_dict_set_val(coder%context,coder%proc_cache,k,cnode)
       else 
          ! Already have a return type - merge with type just returned
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

    if(pm_debug_level>2) then
       write(*,*) 'COMPLETED>',coder%stack(base),&
            coder%stack(base-1),base,oldbase,coder%stack(oldbase-1)
    endif

    ! Pass a break out
    if(coder%stack(base-1)==1) then
       if(pm_debug_level>2) &
            write(*,*) 'OUTBREAK>',oldbase
       coder%stack(oldbase-1)=1
       coder%top=base-2
       ! clear cache entry
       cnode%offset=-3
       call pm_dict_set_val(coder%context,&
            coder%proc_cache,k,cnode)
       if(rtype<0) then
          break=.true.
       else
          call code_num(coder,int(k))
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
    call code_num(coder,int(k))
    coder%top=base-2
    call cnode_incr_num(prc,pr_recurse,-1)

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
    integer(pm_i16):: t1,t2
    integer(pm_i16),dimension(2):: key
    integer:: k
    integer(pm_p):: sym
    type(pm_ptr):: tv
    p=cnode_get(prc,bi_rcode)
    if(pm_fast_isnull(p)) then
       rtype=cnode_get_num(prc,bi_rtype)
    else
       ! Set up type inference frame
       base=coder%top+1
       coder%top=base+cnode_get_num(prc,cnode_args+1)
       if(coder%top>max_code_stack) &
            call pm_panic('Program too complex (prc-bi)')
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
             !call dump_type(coder%context,6,rtype,2)
          else
             rtype=unit_vector(pm_tv_arg(tv,1))
          endif
       case(sym_dot)
          tv=pm_typ_vect(coder%context,rtype)
          tv=pm_typ_vect(coder%context,pm_tv_arg(tv,1))
          if(pm_tv_kind(tv)/=pm_typ_is_user) &
               call pm_panic('ref-access')
          rtype=unit_vector(pm_tv_arg(tv,1))
       case(sym_gt)
          tv=pm_typ_vect(coder%context,rtype)
          rtype=unit_vector(max(pm_tv_arg(tv,1),pm_tv_arg(tv,2)))
       case(sym_dim)
          tv=pm_typ_vect(coder%context,rtype)
          rtype=unit_vector(make_array_type(coder,pm_tv_arg(tv,1),pm_tv_arg(tv,2)))
       case(sym_query)
          tv=pm_typ_vect(coder%context,rtype)
          rtype=unit_vector(make_any_type(coder,pm_tv_arg(tv,1)))
       case(sym_amp)
          tv=pm_typ_vect(coder%context,rtype)
          rtype=unit_vector(make_opt_type(coder,pm_tv_arg(tv,1)))
       case(sym_eq)
          tv=pm_typ_vect(coder%context,rtype)
          t1=pm_tv_arg(tv,1)
          t2=pm_tv_arg(tv,2)
          if(pm_typ_is_concrete(coder%context,t1).and.&
               pm_typ_is_concrete(coder%context,t2)) then
             if(t1==t2) then
                rtype=unit_vector(int(pm_name,pm_i16))
             else
                ! Note pm_name reused as affirm type
                rtype=unit_vector(int(pm_null,pm_i16))
             endif
          else
             rtype=unit_vector(make_ambig_type(coder,int(pm_name,pm_i16),int(pm_null,pm_i16)))
          endif
       end select
    endif
    key(1)=-cnode_get_num(prc,bi_id)-1
    k=pm_idict_add(coder%context,&
         coder%proc_cache,key,1,prc)
    call code_num(coder,k)
  contains
    include 'fisnull.inc'
    function unit_vector(t) result(u)
      integer(pm_i16),intent(in):: t
      integer(pm_i16):: u
      call push_word(coder,pm_typ_is_tuple)
      call push_word(coder,0_pm_i16)
      call push_word(coder,t)
      call make_type(coder,3)
      u=pop_word(coder)
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
    if(pm_fast_isnull(cblock)) return
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
    integer:: sig
    integer(pm_i16):: tno,tno2,name,off,ressig
    type(pm_ptr):: p,t,list,list2,proclist
    integer:: i,j,n,nret,narg,nkey,nextra,slot,slot2,tbase
    integer:: vbase_check,tbase_check
    logical:: ok,mayfail
    character*100 str

    if(pm_debug_level>0) then
       vbase_check=coder%vtop
       tbase_check=coder%wtop
    endif

    break=.false.
    nret=cnode_get_num(callnode,call_nret)
    sig=cnode_get_num(callnode,call_sig)
    p=cnode_get(callnode,call_args)
    narg=cnode_numargs(p)-nret

    if(sig<0) then
       if(pm_debug_level>2) then
          write(*,*) 'PROCESS CALL>',sym_names(-sig),&
               'ttop=',coder%wtop,'vtop=',coder%vtop
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
          call prc_cblock(coder,list,base)
       case(sym_do,sym_for,sym_pct,sym_hash,sym_colon,sym_also)
          call prc_cblock(coder,cnode_arg(p,1),base)
       case(sym_each)
          call prc_cblock(coder,cnode_arg(p,1),base)
       case(sym_endfor)
          slot=get_slot(1)
          coder%stack(slot)=pm_long
          call prc_cblock(coder,cnode_arg(p,2),base)
       case(sym_reduce,sym_reduce_at)
          do i=1,nret/2
             coder%stack(get_slot(i))=arg_type(i+nret+1)
             coder%stack(get_slot(i+nret/2))=arg_type(i+nret+1)
          enddo
          call prc_cblock(coder,cnode_arg(p,nret+1),base)
          do i=1,nret/2
             if(arg_type(i+nret+1)/=arg_type(i+nret+1+nret/2)) then
                call cnode_error(coder,p,&
                     'Result of reduction must have same'//&
                     ' type as original argument',&
                     cnode_get(cnode_arg(p,i+nret+1),var_name))
             endif
          enddo
       case(sym_find)
          slot=get_slot(1)
          coder%stack(slot)=pm_long
          slot=get_slot(2)
          coder%stack(slot)=arg_type(6)
          slot=get_slot(3)
          coder%stack(slot)=pm_logical
          call prc_cblock(coder,cnode_arg(p,4),base)
       case(sym_define)
          slot=get_slot(1)
          slot2=get_slot_or_type(2)
          if(slot2>0) then
             coder%stack(slot)=var_type(coder%stack(slot2))
          else
             coder%stack(slot)=-slot2
          endif 
       case(sym_arrow)
          call combine_types(coder,base,callnode,&
               cnode_arg(p,1),arg_type(2))
       case(sym_struct,sym_rec)
          if(-sig==sym_struct) then
             call push_word(coder,pm_typ_is_struct)
          else
             call push_word(coder,pm_typ_is_rec)
          endif
          t=cnode_arg(p,2)
          call push_word(coder,int(cnode_get_num(t,cnode_args),pm_i16))
          slot=get_slot(1)
          tno=coder%stack(slot)
          if(tno>=0) then
             t=pm_typ_vect(coder%context,tno)
             if(pm_tv_kind(t)==pm_typ_is_ambig) then
                if(pm_tv_name(t)>max_ambig_type-2) then
                   do i=2,narg
                      call push_word(coder,0_pm_i16)
                   enddo
                else
                   do i=2,narg
                      call push_word(coder,arg_type(1+i))
                   enddo
                endif
             endif
          else
             do i=2,narg
                call push_word(coder,arg_type(1+i))
             enddo
          endif
          call make_type(coder,narg+1)
          !write(*,*) '::::::'
          !call dump_type(coder%context,6,top_word(coder),2)
          call combine_types(coder,base,callnode,&
               cnode_arg(p,1),pop_word(coder))
       case(sym_dot)
          call resolve_elem
       case(sym_dotref)
          tno=arg_type(2)
          name=cnode_get_num(cnode_arg(p,3),cnode_args)
          call pm_elem_offset(coder%context,tno,name,.true.,off,tno2)
          i=base+cnode_get_num(callnode,call_index)
          coder%stack(i)=off
          call combine_types(coder,base,callnode,cnode_arg(p,1),tno2)
       case(sym_default)
          tno=cnode_get_num(cnode_arg(p,2),cnode_args)
          slot=get_slot(1)
          coder%stack(slot)=tno
       case(sym_any)
          tno=cnode_get_num(cnode_arg(p,2),cnode_args)
          t=pm_typ_vect(coder%context,tno)
          call get_slot_and_type(3,slot2,tno2)
          if(.not.pm_typ_intersects(coder%context,pm_tv_arg(t,1),tno2)) then
             call infer_error(coder,callnode,&
                  '"any" has incompatible argument type')
          endif
          slot=get_slot(1)
          coder%stack(slot)=tno
       case(sym_check)
          if(narg==2) then
             call check_logical(2)
          else
             if(.not.pm_typ_intersects(coder%context,arg_type(2),&
                  arg_type(3))) then
                call infer_error(coder,callnode,&
                     'Types never assignable')
             endif
          endif
       case(sym_open)
          if(narg>0) then
             t=pm_typ_vect(coder%context,coder%stack(base))
             n=pm_tv_numargs(t)
             do i=1,narg
                slot=get_slot(i)
                coder%stack(slot)=pm_tv_arg(t,i)
                if(pm_debug_level>2) &
                     write(*,*) 'PARAM>',i,slot,pm_tv_arg(t,i+n-narg),pm_tv_numargs(t)
             enddo
             if(n>narg) then
                call push_word(coder,pm_typ_is_tuple)
                call push_word(coder,0_pm_i16)
                j=0
                do i=narg,n
                   if(pm_tv_arg(t,i)/=pm_tiny_int) then
                      call push_word(coder,pm_tv_arg(t,i))
                      j=j+1
                   endif
                enddo
                call make_type(coder,j+2)
                slot=get_slot(narg)
                coder%stack(slot)=pop_word(coder)
             endif
          endif
          coder%stack(base)=-1  
       case(sym_key)
          if(nret==2) then
             coder%stack(get_slot(1))=0
             coder%stack(get_slot(2))=pm_logical
          elseif(narg==0) then
             slot=get_slot(1)
             coder%stack(slot)=0
          else
             call pm_panic('infer key')
          endif
       case(sym_present)
          slot=get_slot(1)
          slot2=get_slot_or_type(3)
          if(slot2>0) then
             coder%stack(slot)=var_type(coder%stack(slot2))
          else
             coder%stack(slot)=-slot2
          endif
          coder%stack(get_slot(2))=pm_logical
       case(sym_result)
          call get_arg_types
          call make_type(coder,narg+2)
          coder%stack(base)=pop_word(coder)
       case(sym_underscore,sym_sync)
          continue
       case default
          if(-sig>=0.and.-sig<=num_sym) then
             write(*,*) sym_names(-sig)
          else
             write(*,*) 'Sym no:',-sig
             write(*,*) trim(pm_name_as_string(coder%context,int(-sig,pm_p)))
          endif
          call pm_panic('Unexpected call symbol')
       end select
    else
       if(pm_debug_level>2) then
          write(*,*) 'PROCESS FULL CALL>',&
               trim(sig_name_str(coder,int(sig)))
          if(cnode_get_kind(p)/=cnode_is_arglist) call pm_panic('not arglist')
          call qdump_code_tree(coder,pm_null_obj,6,callnode,2)
       endif
       proclist=pm_dict_val(coder%context,coder%sig_cache,int(sig,pm_ln))
       nkey=cnode_get_num(callnode,call_nkeys)
       nextra=0
       call check_wstack(coder,narg-nkey)
       do i=1,narg-nkey
          coder%wstack(coder%wtop+i)=arg_type(i+nret+nkey)
       enddo
       coder%wtop=coder%wtop+narg-nkey
       if(cnode_flags_set(callnode,call_flags,call_is_vararg)) then
          if(top_word(coder)/=0) then
             t=pm_typ_vect(coder%context,top_word(coder))
             if(pm_tv_kind(t)==pm_typ_is_tuple) then
                call drop_word(coder)
                do i=1,pm_tv_numargs(t)
                   call push_word(coder,pm_tv_arg(t,i))
                   !write(*,*) 'PUSH TYP',trim(pm_typ_as_string(coder%context,pm_tv_arg(t,i)))
                enddo
                narg=narg+pm_tv_numargs(t)-1
             endif
          endif
       endif
       if(cnode_flags_set(proclist,cnode_args+1,proc_is_var)) then
          ressig=var_call(proclist)
       else
          do j=1,nret
             coder%stack(get_slot(j))=-1
          enddo
          ressig=full_call(sig,proclist)
       endif
       coder%wtop=coder%wtop-narg+nkey
       slot=base+cnode_get_num(callnode,call_index)
       coder%stack(slot)=ressig
       if(pm_debug_level>2) then
          write(*,*) 'END FULL CALL>',trim(sig_name_str(coder,int(sig))),coder%stack(4)
       endif
    endif
    if(pm_debug_level>0) then
       if(vbase_check/=coder%vtop) then
          write(*,*) 'MISMATCH-vstack',coder%vtop,vbase_check
          call pm_panic('prc_call')
       endif
       if(tbase_check/=coder%wtop) then
          write(*,*) 'MISMATCH-tstack',coder%wtop,tbase_check
          call pm_panic('prc_call')
       endif
    endif
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'fname.inc'
    include 'ftiny.inc'

    function var_call(prlist) result(k)
      type(pm_ptr),intent(in):: prlist
      integer(pm_i16):: k
      integer:: i,vbase
      integer:: sig,rsig
      type(pm_ptr):: pr,var,tv
      integer(pm_i16):: tno,tno2,name
      logical:: err
      integer(pm_i16),dimension(1)::key
      var=cnode_arg(prlist,3)
      tno=coder%stack(cnode_get_num(var,var_index)+base)
      tv=pm_typ_vect(coder%context,tno)
      vbase=coder%vtop
      do i=1,nret
         coder%stack(get_slot(i))=-1
      enddo
      call code_val(coder,cnode_arg(prlist,1))
      call code_val(coder,cnode_arg(prlist,2))
      call code_val(coder,cnode_arg(prlist,3))
      do i=4,cnode_numargs(prlist),2
         name=cnode_get_num(prlist,cnode_args+i-1)
         call push_word(coder,pm_typ_is_single_proc)
         call push_word(coder,name)
         call make_type(coder,2)
         tno2=pop_word(coder)
         if(.not.pm_typ_intersects(coder%context,tno,tno2)) cycle
         sig=cnode_get_num(prlist,cnode_args+i)
         pr=pm_dict_val(coder%context,coder%sig_cache,int(sig,pm_ln))
         rsig=full_call(sig,pr,err)
         if(.not.err) then
            call code_val(coder,cnode_arg(prlist,i))
            call code_val(coder,cnode_arg(pr,1))
            call code_val(coder,cnode_arg(pr,2))
            call code_num(coder,rsig)
         endif
      enddo
      if(coder%vtop==vbase+3) then
         call infer_error(coder,callnode,&
              'No possible match for call to proc variable')
         k=-1
      elseif(coder%vtop==vbase+7) then
         k=coder%vstack(vbase+7)%offset
      else
         call make_code(coder,pm_null_obj,cnode_is_var_proc,coder%vtop-vbase)
         key(1)=pm_dict_size(coder%context,coder%proc_cache)
         k=pm_idict_add(coder%context,coder%proc_cache,key,&
              1,top_code(coder))
      endif
      coder%vtop=vbase
    end function  var_call

    function full_call(sig,procs,err) result(k)
      integer,intent(in):: sig
      type(pm_ptr),intent(in):: procs
      logical,intent(out),optional:: err
      integer(pm_i16):: k
      integer:: h,i,j,m,start,slot,pcheck,nkey_sig
      integer(pm_i16):: rt
      type(pm_ptr):: v,proc,rtvect
      integer(pm_i16):: pars,apars,key(1),tno
      logical:: allin,ok
      character(len=2):: join
      
      if(present(err)) err=.false.
      start=coder%vtop
      v=cnode_arg(procs,1)
      if(pm_fast_isnull(v)) then
         nkey_sig=0
      else
         nkey_sig=pm_set_size(coder%context,v)
      endif
      
      ! First significant argument
      call code_num(coder,nret+nkey_sig*2+nextra+1)
      if(pm_debug_level>4) write(*,*) 'Checking',cnode_numargs(procs),' sigs'
      do i=3,cnode_numargs(procs),2
         if(pm_debug_level>2) write(*,*) 'CHECKING SIG',i,'OF',cnode_numargs(procs),&
              ' FOR>',trim(sig_name_str(coder,int(sig)))
         pars=cnode_get_num(procs,cnode_args+i-1)
         apars=check_call_sig(coder,&
              pars,narg-nkey,nextra,allin)
         if(apars>=0) then
            call code_num(coder,int(apars))
            call code_num(coder,int(pars))
            proc=cnode_arg(procs,i+1)
            if(cnode_get_kind(proc)==cnode_is_builtin) then
               rt=prc_builtin(coder,proc,apars,pars)
            else
               pcheck=coder%vtop

               ! Misuse loop stack as a traceback record !
               ! of calls being processed
               coder%loop_depth=coder%loop_depth+1
               if(coder%loop_depth<max_loop_depth) then
                  coder%imports(coder%loop_depth)=callnode
                  coder%import_cblock(coder%loop_depth)=&
                       pm_fast_tinyint(coder%context,int(apars,pm_p))
               endif
               
               rt=prc_proc(coder,proc,apars,pars,base,break)
               coder%loop_depth=coder%loop_depth-1
               if(break) then
                  if(pm_debug_level>2) &
                       write(*,*) 'BREAK>',coder%vtop,start
                  coder%stack(base-1)=1
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
      if(pm_debug_level>2) &
           write(*,*) 'ALL SIGS CHECKED>',trim(sig_name_str(coder,int(sig)))
      if(coder%vtop>start+4) then
         ! This is a polymorphic call
         call infer_error(coder,callnode,&
              'Polymorphic call to:'//trim(sig_name_str(coder,int(sig))))
         

         ! Create multi-proc node
         call make_code(coder,pm_null_obj,cnode_is_multi_proc,&
              coder%vtop-start)
         key(1)=pm_dict_size(coder%context,coder%proc_cache)
         k=pm_idict_add(coder%context,coder%proc_cache,key,&
              1,top_code(coder))
         if(pm_debug_level>2) write(*,*) 'MAKE MULTI>',k
      else if(coder%vtop==start+1) then
         if(.not.present(err)) then
            call cnode_error(coder,callnode,&
                 'No matching procedure:')
            call more_error(coder%context,'Failed call:')
            call more_error(coder%context,' ')
            call more_error(coder%context,&
                 ' '//trim(pm_name_as_string(coder%context,sig_name(coder,sig)))//' (')
            do i=nkey+nextra+1,narg
               k=coder%wstack(coder%wtop-narg+i)
               if(i==narg) then
                  join='  '
               else
                  join=', '
               endif
               call more_error(coder%context,&
                    '     '//trim(pm_typ_as_string(coder%context,k))//join)
            enddo
            call more_error(coder%context,' )')
            call more_error(coder%context,'Procedures considered:')
            do m=3,cnode_numargs(procs),2
               pars=cnode_get_num(procs,cnode_args+m-1)
               call more_error(coder%context,&
                    trim(pm_name_as_string(coder%context,sig_name(coder,sig)))//&
                    trim(pm_typ_as_string(coder%context,pars)))
            enddo
            call more_error(coder%context,'-------------------')
            call infer_trace(coder)
            do i=1,nret
               call unrestrict(i)
            enddo
         else
            err=.true.
         endif
         k=-1
      else
         if(allin) then
            k=coder%vstack(coder%vtop)%offset
         else
            call make_code(coder,pm_null_obj,cnode_is_multi_proc,&
                 coder%vtop-start)
            key(1)=pm_dict_size(coder%context,coder%proc_cache)
            k=pm_idict_add(coder%context,coder%proc_cache,key,&
                 1,top_code(coder))
         endif
      endif
      coder%vtop=start
    end function  full_call

    subroutine get_arg_types
      integer:: i,j
      type(pm_ptr):: v
      if(coder%wtop+narg+2>max_code_stack) &
           call pm_panic('Program too complex')
      coder%wstack(coder%wtop+1)=pm_typ_is_tuple
      coder%wstack(coder%wtop+2)=0
      do i=1,narg
         coder%wstack(coder%wtop+i+2)=arg_type(nret+i)
      enddo
      coder%wtop=coder%wtop+narg+2
    end subroutine get_arg_types

    function arg_type(m) result(tno)
      integer,intent(in):: m
      integer(pm_i16):: tno
      integer:: slot
      slot=get_slot_or_type(m)
      if(slot<0) then
         tno=-slot
      else
         tno=coder%stack(slot)
         if(pm_debug_level>0) then
            if(tno==-1) then
               write(*,*) m,slot
               call pm_panic('Broken type resolution chain')
            endif
         endif
      endif
    end function arg_type

    subroutine get_slots_and_types
      integer:: i
      if(coder%wtop+narg>max_code_stack) &
           call pm_panic('Program too complex')
      do i=1,narg
         coder%wstack(coder%wtop+i)=get_slot_or_type(i+nret)
      enddo
      coder%wtop=coder%wtop+narg
    end subroutine get_slots_and_types

    function get_slot_or_type(m) result(slotno)
      integer,intent(in):: m
      integer:: slotno
      type(pm_ptr):: v
      v=cnode_arg(p,m)

      if(cnode_get_kind(v)==cnode_is_const) then
         slotno=-cnode_get_num(v,cnode_args+1)
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
      if(.not.pm_typ_includes(coder%context,arg_type(m),&
           int(pm_logical,pm_i16))) then
         call infer_error(coder,callnode,&
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
         call push_word(coder,pm_typ_is_ambig)
         call push_word(coder,pm_tv_name(tv))
         call push_word(coder,t2)
         call push_word(coder,t4)
         call make_type(coder,4)
         newtno=pop_word(coder)
      case(pm_typ_is_struct,pm_typ_is_rec)
         n=pm_tv_numargs(tv)
         if(coder%wtop+n+2>max_code_stack) &
              call pm_panic('Program too complex')
         coder%wstack(coder%wtop+1)=pm_typ_is_struct
         coder%wstack(coder%wtop+2)=pm_tv_name(tv)
         coder%wtop=coder%wtop+2
         changed=.false.
         do i=1,n
            t1=pm_tv_arg(tv,i)
            t2=var_type(t1)
            coder%wtop=coder%wtop+1
            coder%wstack(coder%wtop)=t2
            if(t1/=t2) changed=.true.
         enddo
         if(changed) then
            call make_type(coder,n+2)
            newtno=pop_word(coder)
         else
            coder%wtop=coder%wtop-2-n
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
            ok=.false.
            call resolve_struct_elems(cnode_arg(p,1),tno,&
                 -sig,int(name,pm_p),.false.)
            if(.not.ok) then
               call infer_error(coder,callnode,&
                    'Value cannot have element:',namep)
               call more_error(coder%context,&
                    trim(pm_typ_as_string(coder%context,tno)))
               call unrestrict(1)
            endif
         else
            call combine_types(coder,base,callnode,cnode_arg(p,1),eltyp)
         endif
      endif
    end subroutine resolve_elem

    recursive subroutine resolve_struct_elems(&
         var,typ,which,name,array)
      type(pm_ptr),intent(in):: var
      integer(pm_i16),intent(in):: typ
      integer(pm_p),intent(in):: name
      integer,intent(in):: which
      logical,intent(in):: array
      type(pm_ptr):: tv,nv,set
      integer(pm_i16):: tk,tno,offset
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
         call pm_elem_offset(coder%context,typ,int(name,pm_i16),which==sym_dotref,offset,tno)
         if(offset>=0) then
            if(.not.pm_fast_isnull(var))&
                 call combine_types(coder,base,callnode,var,tno)
            ok=.true.
            return
         endif
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
  function check_call_sig(coder,pars,num_args,ignore,allin) result(tno)
    type(code_state),intent(inout):: coder
    integer(pm_i16),intent(in):: pars
    integer,intent(in):: num_args,ignore
    logical,intent(out):: allin
    integer(pm_i16):: tno
    integer(pm_i16):: at,pt,slot
    type(pm_ptr):: pv,amb
    integer:: i,rel,n,narg
    logical:: ok
    narg=num_args
    if(num_args==0) then
       if(pars==0) then
          tno=pars
          allin=.true.
       else
          tno=-1
          allin=.false.
       endif
       return
    elseif(pars==0) then
       tno=-1
       allin=.false.
       return
    elseif(pars==pm_matched_type) then
       n=2
       pv=pm_typ_vect(coder%context,pars)
    else
       pv=pm_typ_vect(coder%context,pars)
       if(pm_debug_level>0) then
          if(pm_tv_kind(pv)/=pm_typ_is_tuple.and.&
               pm_tv_kind(pv)/=pm_typ_is_vtuple) &
               call pm_panic('check-sig')
       endif
       if(coder%wtop+narg+2>max_code_stack) &
            call pm_panic('Program too complex (check-sig)')
       n=pm_tv_numargs(pv)
    endif
    if(n+ignore>narg) then
       tno=-1
       return
    endif
    if(pm_debug_level>2) then
       write(*,*) 'Check call sig: <ignore=',ignore,'> ('
       write(*,*) trim(pm_typ_as_string(coder%context,pars))
       write(*,*) '----'
       do i=1,narg
          at=coder%wstack(coder%wtop-narg+i)
          write(*,*) trim(pm_typ_as_string(coder%context,at))
       enddo
       write(*,*) ')'
    endif
    coder%wstack(coder%wtop+1)=pm_typ_is_tuple
    coder%wstack(coder%wtop+2)=0_pm_p
    allin=.true.
    do i=1,narg
       at=coder%wstack(coder%wtop-narg+i)
       if(i<=ignore) then
          coder%wstack(coder%wtop+i+2)=at
          cycle
       endif
       if(at<0) call pm_panic('broken type resolution chain')
       if(i>n+ignore) then
          if(pm_tv_kind(pv)/=pm_typ_is_vtuple) then
             tno=-1
             goto 10
          endif
       else
          pt=pm_tv_arg(pv,i-ignore)
       endif
       if(pm_typ_includes(coder%context,&
            pt,at)) then
          coder%wstack(coder%wtop+i+2)=at
          if(pm_debug_level>2) &
               write(*,*) 'Match',trim(pm_typ_as_string(coder%context,pt))
       else
          if(pm_debug_level>2) then
             write(*,*) 'Does not inlude',&
                  trim(pm_typ_as_string(coder%context,pt)),'<>',&
                  trim(pm_typ_as_string(coder%context,at))
             call dump_type(coder%context,6,pt,2)
             call dump_type(coder%context,6,at,2)
          endif
          coder%wstack(coder%wtop+i+2)=pm_typ_intersect(coder%context,pt,at,&
               coder%wstack(coder%wtop+narg+3:),&
               max_code_stack-coder%wtop-narg-3,ok)
          if(ok) then
             if(pm_debug_level>3) then
                write(*,*) 'Create intersect type:',coder%wstack(coder%wtop+i+2),ok
                call dump_type(coder%context,6,pt,2)
                write(*,*) '---'
                call dump_type(coder%context,6,at,2)
                write(*,*) '---'
                call dump_type(coder%context,6,coder%wstack(coder%wtop+i+2),2)
                write(*,*) '========='
             endif
             allin=.false.
          else
             tno=-1
             goto 10
          endif
       endif
    enddo
    tno=new_type(coder,coder%wstack(coder%wtop+1:&
         coder%wtop+narg+2))
10  continue
    if(narg>num_args) coder%wtop=coder%wtop+num_args-narg
  contains
    include 'fisnull.inc'
  end function check_call_sig

  ! Augment the type stored in a given variable vararg by adding typ
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
    if(pm_debug_level>3) then
       write(*,*) 'COMBINE TYPES> ',&
            trim(pm_name_as_string(coder%context,cnode_get_name(var,var_name)))
    endif
    slot=base+cnode_get_num(var,var_index)
    typ0=coder%stack(slot)
    if(pm_debug_level>3) then
       write(*,*) 'Combine types',slot,typ0,typ,&
         trim(pm_name_as_string(coder%context,cnode_get_name(var,var_name)))
    endif
    if(typ0<0) then
       coder%stack(slot)=typ
       if(pm_debug_level>3) write(*,*) 'set',slot,'to',typ
       return
    endif
    if(pm_typ_includes(coder%context,typ0,typ)) then
       if(pm_debug_level>3) write(*,*) slot,'absorb',typ,'into',typ0
       return
    endif
    if(pm_typ_includes(coder%context,typ,typ0)) then
       coder%stack(slot)=typ
    else
       coder%stack(slot)=make_ambig_type(coder,typ0,typ)
    endif
    if(pm_debug_level>3) write(*,*) 'New type:',slot,coder%stack(slot)
    
    ! Flag any change in type up through the call stack
    p=cnode_get(callnode,call_parent)
    q=cnode_get(var,var_parent)
    do while(.not.p==q)
       slot=base+cnode_get_num(p,cblock_index)
       coder%stack(slot)=1
       p=cnode_get(p,cblock_parent)
       if(pm_debug_level>0) then
          if(pm_fast_isnull(p)) call pm_panic('combine types')
       endif
    enddo
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine combine_types

  ! Make an ambiguous type: union of typ0 and typ
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
       call push_word(coder,pm_typ_is_ambig)
       call push_word(coder,n)
       call push_word(coder,typ)
       call push_word(coder,typ0)
       call make_type(coder,4)
       atyp=pop_word(coder)
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

  subroutine infer_error(coder,node,message,name)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    character(len=*):: message
    type(pm_ptr),intent(in),optional:: name
    character(len=100):: str
    type(pm_ptr):: modname
    if(pm_main_process) then
       call pm_name_string(coder%context,cnode_get_name(node,cnode_modl_name),str)
       write(*,*) 'Error:',trim(str),' line:',cnode_get_num(node,cnode_lineno)
       if(present(name)) then
          call pm_name_string(coder%context,name%offset,str)
          str=trim(message)//' '//trim(str)
       else
          str=message
       endif
       write(*,*) str
       call infer_trace(coder)
    endif
    coder%num_errors=coder%num_errors+1
    if(coder%num_errors>max_code_errors) &
         call pm_stop('Too many type/call errors - compilation terminated')
  end subroutine infer_error

  subroutine infer_trace(coder)
    type(code_state):: coder
    type(pm_ptr):: node,modname,tv
    character(len=100):: str
    integer:: i,k
    character(len=2):: join
    if(.not.pm_main_process) return
    do k=coder%loop_depth,1,-1
       if(k>max_loop_depth) then
          write(*,*)
          write(*,*) 'Procedure call: (call not recorded)'
          cycle
       endif
       node=coder%imports(k)
       call pm_name_string(coder%context,cnode_get_name(node,cnode_modl_name),str)
       write(*,*)
       write(*,*) 'Procedure call: ',trim(str),' line:',cnode_get_num(node,cnode_lineno)
       write(*,*)
       call more_error(coder%context,' '//trim(sig_name_str(coder,&
            cnode_get_num(coder%imports(k),call_sig)))//' (')
       tv=pm_typ_vect(coder%context,int(coder%import_cblock(k)%offset,pm_i16))
       do i=1,pm_tv_numargs(tv)
          if(i<pm_tv_numargs(tv)) then
             join=', '
          else
             join=' '
          endif
          call more_error(coder%context,&
               '     '//trim(pm_typ_as_string(coder%context,pm_tv_arg(tv,i)))//join)
       enddo
       call more_error(coder%context,' )')
       write(*,*)
    enddo
  end subroutine infer_trace


end module pm_infer
