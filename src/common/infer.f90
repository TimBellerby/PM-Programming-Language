!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2017
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
  use pm_compbase
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_types
  use pm_parser
  use pm_sysdefs
  use pm_codegen
  implicit none

  integer,parameter:: max_recur=16

contains
  
  !============================================================
  ! The following routines process the intermediate code tree 
  ! applying type inference and resolving polymorphic procedure 
  ! calls at compile time
  !=============================================================

  ! Type-infer main program
  subroutine prc_prog(coder)
    type(code_state),intent(inout):: coder
    type(pm_ptr):: cnode
    integer:: i

    if(pm_debug_level>2) write(*,*) 'PROCESS PROG>'

    coder%loop_depth=0
    coder%poly_cache=pm_dict_new(coder%context,32_pm_ln)

    do
       coder%types_finished=.true.
       coder%proc_cache=pm_dict_new(coder%context,32_pm_ln)
       
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
             call more_error(coder%context,&
                  'Error: Procedure has infinite recursion')
             coder%loop_depth=coder%proc_loop_depth
             call infer_trace(coder)
             call pm_stop('Program contains infinite recursion')
          endif
       endif
       
       if(coder%types_finished) exit
    enddo
    
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
  function prc_proc(coder,prc,atype,ptype,haskeys,oldbase,break) result(rtype)
    type(code_state),intent(inout):: coder
    type(pm_ptr),intent(in):: prc
    integer(pm_i16),intent(in):: atype,ptype
    logical,intent(in):: haskeys
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
    if(k>0.and..not.haskeys) then
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

    at=atype
    
    ! Check for recursion
    if(cnode_get_num(prc,pr_recurse)>max_recur) then
       call infer_error(coder,prc,'Recursion appears to require infinite types')
       call code_num(coder,0)
       return
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
            write(*,*) 'TRY COMPLETE>',cnode%offset,&
            coder%stack(base),coder%stack(base-1)
       
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
          if(rtype/=coder%stack(base)) then
             call infer_error(coder,prc,'Procedure return type changed')
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
    type(pm_ptr):: tv,tv2
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
          rtype=unit_vector(make_array_type(coder,&
               pm_tv_arg(tv,1),pm_tv_arg(tv,2)))
       case(sym_over)
          tv=pm_typ_vect(coder%context,rtype)
          tv2=pm_typ_vect(coder%context,pm_tv_arg(tv,1))
          rtype=unit_vector(make_array_type(coder,&
               pm_tv_arg(tv2,1),pm_tv_arg(tv,2)))
       case(sym_eq)
          tv=pm_typ_vect(coder%context,rtype)
          t1=pm_tv_arg(tv,1)
          t2=pm_tv_arg(tv,2)
          if(t1==t2) then
             rtype=unit_vector(coder%true_name)
          else
             ! Note pm_name reused as affirm type
             rtype=unit_vector(coder%false_name)
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
    type(pm_ptr):: p,t,t2,list,list2,proclist
    integer:: i,j,n,nret,narg,nkey,nextra,slot,slot2,tbase
    integer:: vbase_check,tbase_check,counter
    integer(pm_i16),dimension(2):: key
    logical:: ok,mayfail
    integer(pm_ln):: k
    character(len=100):: str

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
          counter=0
          do
             call clear_cblock_mark(list)
             call clear_cblock_mark(list2)
             call prc_cblock(coder,list,base)
             call check_logical(2)
             call prc_cblock(coder,list2,base)
             if(.not.(cblock_marked(list).or.&
                  cblock_marked(list2))) exit
             counter=counter+1
             if(counter>max_recur) then
                call infer_error(coder,p,&
                     '"while" appears to lead to infinite types')
                exit
             endif
          enddo
       case(sym_repeat)
          list=cnode_arg(p,1)
          counter=0
          do 
             call clear_cblock_mark(list)
             call prc_cblock(coder,list,base)
             if(.not.cblock_marked(list)) exit
             counter=counter+1
             if(counter>max_recur) then
                call infer_error(coder,p,&
                     '"repeat" appears to lead to infinite types')
                exit
             endif
          enddo
          call check_logical(2)
       case(sym_if,sym_invar)
          call check_logical(1)
          list=cnode_arg(p,2)
          call prc_cblock(coder,list,base)
          list=cnode_arg(p,3)
          call prc_cblock(coder,list,base)
       case(sym_do,sym_for,sym_pct,sym_colon,sym_also)
          call prc_cblock(coder,cnode_arg(p,1),base)
       case(sym_each)
          call prc_cblock(coder,cnode_arg(p,1),base)
       case(sym_hash)
          if(arg_type(2)/=pm_null) then
             call prc_cblock(coder,cnode_arg(p,1),base)
          endif
       case(sym_endfor)
          tno=arg_type(7)
          if(tno==pm_null) then
             slot=get_slot(1)
             coder%stack(slot)=pm_long
             slot=get_slot(2)
             coder%stack(slot)=tno
             call prc_cblock(coder,cnode_arg(p,4),base)
             call prc_cblock(coder,cnode_arg(p,3),base)
             call make_code(coder,pm_null_obj,cnode_is_any_sig,0)
          else
             list=cnode_arg(p,8)
             list=cnode_arg(list,1)
             slot=list%data%i(list%offset)
             slot2=list%data%i(list%offset+1)
             coder%stack(base+slot:base+slot2)=-1
             coder%stack(get_slot(1))=pm_long
             coder%stack(get_slot(2))=tno
             call prc_cblock(coder,cnode_arg(p,4),base)
             call prc_cblock(coder,cnode_arg(p,3),base)
             call prc_cblock(coder,cnode_arg(p,5),base)
             call code_val(coder,&
                  pm_fast_newnc(coder%context,pm_int16,int(slot2-slot+1,pm_p)))
             list=top_code(coder)
             list%data%i16(list%offset:list%offset+slot2-slot)=&
                  coder%stack(base+slot:base+slot2)
             coder%stack(base+slot:base+slot2)=-1
             coder%stack(get_slot(1))=pm_long
             coder%stack(get_slot(2))=pm_null
             call prc_cblock(coder,cnode_arg(p,4),base)
             call prc_cblock(coder,cnode_arg(p,3),base)
             call code_val(coder,&
                  pm_fast_newnc(coder%context,pm_int16,int(slot2-slot+1,pm_p)))
             list=top_code(coder)
             list%data%i16(list%offset:list%offset+slot2-slot)=&
                  coder%stack(base+slot:base+slot2)
             call make_code(coder,pm_null_obj,cnode_is_any_sig,2)
          endif
          list=pop_code(coder)
          key(1)=pm_dict_size(coder%context,coder%proc_cache)
          k=pm_idict_add(coder%context,coder%proc_cache,&
               key,1,list)
          coder%stack(base+cnode_get_num(callnode,call_index))=k
       case(sym_find)
          tno=arg_type(9)
          if(tno==pm_null) then
             coder%stack(get_slot(1))=pm_long
             coder%stack(get_slot(2))=tno
             coder%stack(get_slot(3))=arg_type(12)
             coder%stack(get_slot(4))=pm_logical
             call prc_cblock(coder,cnode_arg(p,6),base)
             call prc_cblock(coder,cnode_arg(p,5),base)
             call make_code(coder,pm_null_obj,cnode_is_any_sig,0)
          else
             list=cnode_arg(p,10)
             list=cnode_arg(list,1)
             slot=list%data%i(list%offset)
             slot2=list%data%i(list%offset+1)
             coder%stack(base+slot:base+slot2)=-1
             coder%stack(get_slot(1))=pm_long
             coder%stack(get_slot(2))=tno
             coder%stack(get_slot(3))=arg_type(12)
             coder%stack(get_slot(4))=pm_logical
             call prc_cblock(coder,cnode_arg(p,6),base)
             call prc_cblock(coder,cnode_arg(p,5),base)
             call prc_cblock(coder,cnode_arg(p,7),base)
             call code_val(coder,&
                  pm_fast_newnc(coder%context,pm_int16,int(slot2-slot+1,pm_p)))
             list=top_code(coder)
             list%data%i16(list%offset:list%offset+slot2-slot)=&
                  coder%stack(base+slot:base+slot2)
             coder%stack(base+slot:base+slot2)=-1
             coder%stack(get_slot(1))=pm_long
             coder%stack(get_slot(2))=pm_null
             coder%stack(get_slot(3))=arg_type(12)
             coder%stack(get_slot(4))=pm_logical
             call prc_cblock(coder,cnode_arg(p,6),base)
             call prc_cblock(coder,cnode_arg(p,5),base)
             call code_val(coder,&
                  pm_fast_newnc(coder%context,pm_int16,int(slot2-slot+1,pm_p)))
             list=top_code(coder)
             list%data%i16(list%offset:list%offset+slot2-slot)=&
                  coder%stack(base+slot:base+slot2)
             call make_code(coder,pm_null_obj,cnode_is_any_sig,2)
          endif
          list=pop_code(coder)
          key(1)=pm_dict_size(coder%context,coder%proc_cache)
          k=pm_idict_add(coder%context,coder%proc_cache,&
               key,1,list)
          coder%stack(base+cnode_get_num(callnode,call_index))=k
       case(sym_reduce,sym_reduce_at)
          do i=1,nret/2
             coder%stack(get_slot(i))=arg_type(i+nret+1)
             coder%stack(get_slot(i+nret/2))=arg_type(i+nret+1)
          enddo
          call prc_cblock(coder,cnode_arg(p,nret+1),base)
          do i=1,nret/2
             if(arg_type(i+nret+1)/=arg_type(i+nret+1+nret/2)) then
                if(coder%num_errors==0) &
                     call cnode_error(coder,p,&
                     'Result of reduction must have same'//&
                     ' type as original argument',&
                     cnode_get(cnode_arg(p,i+nret+1),var_name))
             endif
          enddo
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
          do i=2,narg
             call push_word(coder,arg_type(1+i))
          enddo
          call make_type(coder,narg+1)
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
       case(sym_dash)
          tno=arg_type(3)
          call combine_types(coder,base,callnode,cnode_arg(p,1),tno)
       case(sym_default)
          tno=cnode_get_num(cnode_arg(p,2),cnode_args)
          slot=get_slot(1)
          coder%stack(slot)=tno
       case(sym_lt)
          tno=cnode_get_num(cnode_arg(p,2),cnode_args)
          call get_slot_and_type(3,slot2,tno2)
          t=pm_typ_vect(coder%context,tno2)
          if(pm_tv_kind(t)==pm_typ_is_poly) then
             list=check_poly(coder,tno2)
             tno2=pm_tv_arg(t,1)
          endif
          if(.not.pm_typ_includes(coder%context,tno,tno2)) then
             if(tno2/=0) &
                  call infer_error(coder,callnode,&
                  '"<type>value" has incompatible argument type')
          endif
          slot=get_slot(1)
          call push_word(coder,pm_typ_is_poly)
          call push_word(coder,0_pm_i16)
          call push_word(coder,tno)
          call make_type(coder,3)
          coder%stack(slot)=pop_word(coder)
          if(pm_tv_kind(t)==pm_typ_is_poly) then
             if(.not.pm_fast_isnull(list)) then
                n=pm_set_size(coder%context,list)
                do i=1,n
                   list2=pm_set_key(coder%context,list,int(i,pm_ln))
                   if(add_poly(coder,coder%stack(slot),&
                        list2%data%i16(list2%offset))) then
                      coder%types_finished=.false.
                   endif
                enddo
             endif
          else
             if(add_poly(coder,coder%stack(slot),tno2)) then
                coder%types_finished=.false.
             endif
          endif
       case(sym_any)
          list2=cnode_arg(p,4)
          list2=cnode_arg(list2,1)
          slot=list2%data%i(list2%offset)
          slot2=list2%data%i(list2%offset+1)
          t=check_poly(coder,arg_type(3))
          if(.not.pm_fast_isnull(t)) then
             n=pm_set_size(coder%context,t)
             do i=1,n
                list=pm_set_key(coder%context,t,int(i,pm_ln))
                tno=list%data%i16(list%offset)
                coder%stack(base+slot:base+slot2)=-1
                coder%stack(get_slot(1))=tno
                call prc_cblock(coder,cnode_arg(p,2),base)
                call code_val(coder,pm_fast_newnc(coder%context,pm_int16,&
                     int(slot2-slot+1,pm_p)))
                list=top_code(coder)
                list%data%i16(list%offset:list%offset+slot2-slot)=&
                     coder%stack(base+slot:base+slot2)
             enddo
             call make_code(coder,pm_null_obj,cnode_is_any_sig,n)
             list=pop_code(coder)
             key(1)=pm_dict_size(coder%context,coder%proc_cache)
             k=pm_idict_add(coder%context,coder%proc_cache,&
                  key,1,list)
             coder%stack(base+cnode_get_num(callnode,call_index))=k
          endif
       case(sym_endtype)  ! this controls body for proc.. each()
          t=cnode_arg(p,nret+4)
          t=cnode_arg(t,1)
          slot=t%data%i(t%offset)
          slot2=t%data%i(t%offset+1)
          tno=arg_type(nret+5)
          t=pm_typ_vect(coder%context,tno)
          tno2=pm_tv_kind(t)
          name=pm_tv_name(t)
          n=narg-nret-3
          if(tno2==pm_typ_is_struct.or.tno2==pm_typ_is_rec) then
             do i=nret+7,narg-nret-1,2
                tno=arg_type(i)
                t2=pm_typ_vect(coder%context,tno)
                if(pm_tv_kind(t2)/=tno2) then
                   if(coder%num_errors==0) &
                      call infer_error(coder,callnode,&
                      '"proc each" arguments cannot mix "struct" and "rec"')
                endif
                if(pm_tv_name(t2)/=name) then
                   if(coder%num_errors==0) &
                        call infer_error(coder,callnode,&
                        '"proc each" arguments do not have same elements')
                endif
             enddo
             n=pm_tv_numargs(t)
             if(nret>0) then
                call check_wstack(coder,nret*(n+2))
                tbase=coder%wtop
                coder%wtop=coder%wtop+nret*(n+2)
                do i=1,nret
                   coder%wstack(tbase+(i-1)*(n+2)+1)=tno2
                   coder%wstack(tbase+(i-1)*(n+2)+2)=name
                enddo
             endif
             do i=1,n
                do j=nret+5,narg-1,2
                   tno=arg_type(j)
                   t2=pm_typ_vect(coder%context,tno)
                   tno=pm_tv_arg(t2,i)
                   coder%stack(get_slot(j+1))=tno
                enddo
                call prc_cblock(coder,cnode_arg(p,nret+3),base)
                call code_val(coder,&
                  pm_fast_newnc(coder%context,pm_int16,int(slot2-slot+1,pm_p)))
                list=top_code(coder)
                list%data%i16(list%offset:list%offset+slot2-slot)=&
                     coder%stack(base+slot:base+slot2)
                do j=1,nret
                   coder%wstack(tbase+(j-1)*(n+2)+i+2)=arg_type(narg+j)
                enddo
             enddo
             call make_code(coder,pm_null_obj,cnode_is_any_sig,n)
             list=pop_code(coder)
             key(1)=pm_dict_size(coder%context,coder%proc_cache)
             k=pm_idict_add(coder%context,coder%proc_cache,&
                  key,1,list)
             slot=cnode_get_num(callnode,call_index)
             coder%stack(base+slot)=k
             do i=nret,1,-1
                call make_type(coder,n+2)
                coder%stack(get_slot(i))=pop_word(coder)
             enddo
            call prc_cblock(coder,cnode_arg(p,nret+1),base)
          else
             call prc_cblock(coder,cnode_arg(p,nret+2),base)
             slot=cnode_get_num(callnode,call_index)
             coder%stack(base+slot)=0
          endif
       case(sym_check)
          tno=arg_type(2)
          if(tno==coder%false_name) then
             call pm_strval(cnode_arg(cnode_arg(p,1),1),str)
             call infer_error(coder,callnode,str(1:len_trim(str)-1))
          elseif(tno/=coder%true_name) then
             call check_logical(2)
          endif
       case(sym_open)
          if(narg>0) then
             t=pm_typ_vect(coder%context,coder%stack(base))
             n=pm_tv_numargs(t)
             do i=1,narg
                slot=get_slot(i)
                coder%stack(slot)=pm_tv_arg(t,i)
                if(pm_debug_level>2) &
                     write(*,*) 'PARAM>',i,slot,&
                     pm_tv_arg(t,i+n-narg),pm_tv_numargs(t)
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
          t=cnode_arg(p,2)
          t=cnode_arg(t,1)
          slot2=coder%wstack(coder%proc_key_base+t%data%i(t%offset))
          coder%stack(get_slot(1))=slot2
       case(sym_present)
          t=cnode_arg(p,3)
          t=cnode_arg(t,1)
          slot2=coder%wstack(coder%proc_key_base+t%data%i(t%offset))
          slot=arg_type(4)
          if(slot/=slot2.and.slot2/=pm_tiny_int) then
             t=cnode_arg(p,1)
             t=cnode_get(t,var_name)
             call infer_error(coder,callnode,'Keyword argument type mismatch:',t)
             call more_error(coder%context,'Mismatched argument: '//&
                  trim(pm_typ_as_string(coder%context,int(slot2,pm_i16))))
             call more_error(coder%context,'Mismatched parameter: '//&
                  trim(pm_typ_as_string(coder%context,int(slot,pm_i16))))
          endif
          coder%stack(get_slot(1))=slot
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
       call check_wstack(coder,narg)
       do i=1,narg
          coder%wstack(coder%wtop+i)=arg_type(i+nret)
       enddo
       coder%wtop=coder%wtop+narg
       if(cnode_flags_set(callnode,call_flags,call_is_vararg)) then
          if(top_word(coder)/=0) then
             t=pm_typ_vect(coder%context,top_word(coder))
             if(pm_tv_kind(t)==pm_typ_is_tuple) then
                call drop_word(coder)
                do i=1,pm_tv_numargs(t)
                   call push_word(coder,pm_tv_arg(t,i))
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
       coder%wtop=coder%wtop-narg
       slot=base+cnode_get_num(callnode,call_index)
       coder%stack(slot)=ressig
       if(pm_debug_level>2) then
          write(*,*) 'END FULL CALL>',&
               trim(sig_name_str(coder,int(sig))),coder%stack(4)
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
      integer:: save_proc_key_base

      save_proc_key_base=coder%proc_key_base
      coder%proc_key_base=coder%wtop-narg
      
      if(present(err)) err=.false.
      start=coder%vtop
      v=cnode_arg(procs,1)
      if(pm_fast_isnull(v)) then
         nkey_sig=0
      else
         nkey_sig=pm_set_size(coder%context,v)
      endif
      
      if(pm_debug_level>4) write(*,*) 'Checking',cnode_numargs(procs),' sigs'
      do i=3,cnode_numargs(procs),2
         if(pm_debug_level>2) &
              write(*,*) 'CHECKING SIG',i,'OF',cnode_numargs(procs),&
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

               ! Misuse loop stack as a traceback record 
               ! of calls being processed
               coder%loop_depth=coder%loop_depth+1
               if(coder%loop_depth<max_loop_depth) then
                  coder%imports(coder%loop_depth)=callnode
                  coder%import_cblock(coder%loop_depth)=&
                       pm_fast_tinyint(coder%context,int(apars,pm_p))
               endif
               
               rt=prc_proc(coder,proc,apars,pars,nkey>0,base,break)
               coder%loop_depth=coder%loop_depth-1
               if(break) then
                  if(pm_debug_level>2) &
                       write(*,*) 'BREAK>',coder%vtop,start
                  coder%stack(base-1)=1
                  coder%vtop=start
                  coder%proc_key_base=save_proc_key_base
                  return
               else
                  if(coder%vtop/=pcheck+1) call pm_panic('pcheck mismatch')
               endif
            endif
            if(nret>0) then
               if(rt/=0) then
                  rtvect=pm_typ_vect(coder%context,rt)
                  do j=1,nret
                     v=cnode_arg(p,j)
                     call combine_types(coder,base,callnode,v,&
                          pm_tv_arg(rtvect,j))
                  enddo
               else
                  do j=1,nret
                     v=cnode_arg(p,j)
                     call combine_types(coder,base,callnode,v,&
                          0_pm_i16)
                  enddo
               endif
            endif
            if(allin) exit
         else
            if(pm_debug_level>2) write(*,*) 'REJECTED>'
         endif
      enddo
      if(pm_debug_level>2) &
           write(*,*) 'ALL SIGS CHECKED>',trim(sig_name_str(coder,int(sig)))
      if(coder%vtop>start+3) then
         if(coder%num_errors==0) then
            ! This is a an ambiguos call
            call infer_error(coder,callnode,&
                 'Ambiguous call to:'//trim(sig_name_str(coder,int(sig))))
         endif
      else if(coder%vtop==start) then
         if(.not.present(err)) then
            call cnode_error(coder,callnode,&
                 'No matching procedure:')
            m=coder%wtop
            call make_type(coder,narg+2)
            call print_call_details(coder,callnode,top_word(coder))
            coder%wtop=m
            call more_error(coder%context,'Procedures considered:')
            do m=3,cnode_numargs(procs),2
               pars=cnode_get_num(procs,cnode_args+m-1)
               call print_proc_details(coder,&
                    sig_name(coder,sig),&
                    .not.cnode_flags_clear(callnode,call_flags,&
                    ior(call_is_loop_call,call_is_reduce_call)),&
                    cnode_get_num(callnode,call_nloop),&
                    pars)
            enddo
            call infer_trace(coder)
            do i=1,nret
               call unrestrict(i)
            enddo
         else
            err=.true.
         endif
         k=-1
      else
         k=coder%vstack(coder%vtop)%offset
      endif
      coder%vtop=start

      coder%proc_key_base=save_proc_key_base
      
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
               call dump_code_tree(coder,pm_null_obj,6,cnode_arg(p,m),2)
               call infer_error(coder,p,'Broken::')
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
  
    function check_slot_intersect(m,n) result(ok)
      integer,intent(in):: m,n
      logical:: ok
      integer:: slt1,slt2,i,j
      integer(pm_i16):: tp1,tp2
      type(pm_ptr):: ty1,ty2
      ok=pm_typ_intersects(coder%context,arg_type(m),arg_type(n))
      if(pm_debug_level>2) &
           write(*,*) 'Check intersect',arg_type(m),arg_type(n),ok
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
         call pm_elem_offset(coder%context,tno,name,&
              sig==-sym_dotref,offset,eltyp)
         coder%stack(base+j)=offset
         if(offset==0) then
            ok=.false.
            call resolve_struct_elems(cnode_arg(p,1),tno,&
                 -sig,int(name,pm_p))
            if(.not.ok) then
               if(coder%num_errors==0) then
                  call infer_error(coder,callnode,&
                       'Value cannot have element:',namep)
                  call more_error(coder%context,&
                       trim(pm_typ_as_string(coder%context,tno)))
               endif
               call unrestrict(1)
            endif
         elseif(offset<0) then
            call resolve_embed
         else
            call combine_types(coder,base,callnode,cnode_arg(p,1),eltyp)
         endif
      endif
    end subroutine resolve_elem

    subroutine resolve_embed
      integer(pm_i16),dimension(2):: key
      integer:: cac,status
      integer(pm_i16):: etype
      type(pm_ptr):: svec,namep
      integer:: wbase
      key(1)=-name
      key(2)=tno
      cac=pm_ivect_lookup(coder%context,coder%proc_cache,key,2)
      if(cac>0) then
         coder%stack(base+j)=-cac
         svec=pm_dict_val(coder%context,coder%proc_cache,int(cac,pm_ln))
         call combine_types(coder,base,callnode,cnode_arg(p,1),&
              svec%data%i16(svec%offset))
         return
      endif
      wbase=coder%wtop
      call pm_indirect_offset(coder%context,tno,name,coder%wstack,&
           max_code_stack,coder%wtop,sig==-sym_dotref,etype,status)
      if(status==pm_elem_found) then
         svec=pm_fast_newnc(coder%context,pm_int16,&
              int(coder%wtop-wbase+1,pm_p))
         svec%data%i16(svec%offset)=etype
         svec%data%i16(svec%offset+1:svec%offset+pm_fast_esize(svec))=&
              coder%wstack(wbase+1:coder%wtop)
         coder%stack(base+j)=-pm_idict_add(coder%context,coder%proc_cache,&
              key,2,svec)
        call combine_types(coder,base,callnode,cnode_arg(p,1),etype)
      elseif(status==pm_elem_clash) then
         namep%offset=name
         call infer_error(coder,callnode,&
              'Value has element through multiple routes:',namep)
         call more_error(coder%context,&
              trim(pm_typ_as_string(coder%context,tno)))
         call unrestrict(1)
      else ! Not found
         namep%offset=name
         call infer_error(coder,callnode,&
              'Value cannot have element:',namep)
         call more_error(coder%context,&
              trim(pm_typ_as_string(coder%context,tno)))
         call unrestrict(1)
      endif
      coder%wtop=wbase
    end subroutine resolve_embed

    recursive subroutine resolve_struct_elems(&
         var,typ,which,name)
      type(pm_ptr),intent(in):: var
      integer(pm_i16),intent(in):: typ
      integer(pm_p),intent(in):: name
      integer,intent(in):: which
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
         call pm_elem_offset(coder%context,&
              typ,int(name,pm_i16),which==sym_dotref,offset,tno)
         if(offset>0) then
            if(.not.pm_fast_isnull(var))&
                 call combine_types(coder,base,callnode,var,tno)
            ok=.true.
            return
         endif
         nv=pm_name_val(coder%context,int(pm_tv_name(tv),pm_p))
         do i=1,pm_tv_numargs(tv)
            if(nv%data%i16(nv%offset+i)<0) then
               call resolve_struct_elems(var,pm_tv_arg(tv,i),which,name)
            endif
         enddo
      case(pm_typ_is_user)
         set=pm_dict_val(coder%context,&
              coder%context%tcache,int(typ,pm_ln))
         if(pm_fast_isnull(set)) then
            return
         else
            do i=1,set%data%i16(set%offset)
               call resolve_struct_elems(var,&
                    set%data%i16(set%offset+i),&
                    which,name)
            enddo
         endif
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
      end select
    end subroutine find_array_elems

    subroutine unrestrict(m)
      integer:: m
      integer:: slot
      type(pm_ptr):: ptr
      slot=get_slot(m)
      coder%stack(slot)=0
    end subroutine unrestrict

    ! Flag any change in poly type membership through the call stack
    subroutine flag_change(callnode,var)
      type(pm_ptr),intent(in):: callnode,var
      type(pm_ptr):: p,q
      integer:: slot
  
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
    end subroutine flag_change

  end subroutine prc_call

  function check_poly(coder,typ) result(ptr)
    type(code_state),intent(inout):: coder
    integer(pm_i16),intent(in):: typ
    type(pm_ptr):: ptr
    integer(pm_ln):: j
    integer(pm_i16),dimension(1):: key
    key(1)=typ
    j=pm_ivect_lookup(coder%context,coder%poly_cache,key,1)
    if(j==0) then
       ptr=pm_null_obj
    else
       ptr=pm_dict_val(coder%context,coder%poly_cache,j)
    endif
  end function check_poly

  function add_poly(coder,typ,mtyp) result(changed)
    type(code_state),intent(inout):: coder
    integer(pm_i16),intent(in):: typ,mtyp
    logical:: changed
    integer(pm_i16),dimension(1):: key
    integer(pm_ln):: j
    type(pm_ptr):: v
     key(1)=typ
    j=pm_ivect_lookup(coder%context,coder%poly_cache,key,1)
    if(j==0) then
       coder%temp=pm_set_new(coder%context,32_pm_ln)
       j=pm_idict_add(coder%context,&
            coder%poly_cache,&
            key,1,coder%temp)
       key(1)=mtyp
       j=pm_iset_add(coder%context,&
            coder%temp,key,1)
       changed=.true.
    else
       key(1)=mtyp
       v=pm_dict_val(coder%context,coder%poly_cache,j)
       j=pm_ivect_lookup(coder%context,v,key,1)
       if(j==0) then
          j=pm_iset_add(coder%context,v,key,1)
          changed=.true.
       else
          changed=.false.
       endif
    endif
  end function add_poly

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
    if(typ0<0) then
       coder%stack(slot)=typ
    elseif(typ/=typ0.and.coder%num_errors==0) then
       call cnode_error(coder,var,'Variable changed type:',cnode_get(var,var_name))
       call more_error(coder%context,trim(pm_typ_as_string(coder%context,typ))//' <> '//&
            trim(pm_typ_as_string(coder%context,typ0)))
    endif
    
  contains
    include 'fesize.inc'
    include 'fisnull.inc'
  end subroutine combine_types

  ! Dump resolved proc signatures (debugging)
  subroutine dump_res_sigs(coder,iunit)
    type(code_state),intent(in):: coder
    integer,intent(in):: iunit
    integer(pm_ln):: i,n
    n=pm_dict_size(coder%context,coder%proc_cache)
    do i=1,n
       write(iunit,*) 'Resolved Signature',i,'of',n,'('
       call qdump_code_tree(coder,pm_null_obj,iunit,&
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
       write(*,*)
       call pm_name_string(coder%context,&
            cnode_get_name(node,cnode_modl_name),str)
       write(*,*) 'Error:',trim(str),' line:',cnode_get_num(node,cnode_lineno)
       if(present(name)) then
          call pm_name_string(coder%context,name%offset,str)
          str=trim(message)//' '//trim(str)
       else
          str=message
       endif
       write(*,*) trim(str)
       call infer_trace(coder)
    endif
    coder%num_errors=coder%num_errors+1
    if(coder%num_errors>max_code_errors) &
         call pm_stop('Too many type/call errors - compilation terminated')
  end subroutine infer_error

  subroutine infer_trace(coder)
    type(code_state):: coder
    type(pm_ptr):: node,modname,tv
    integer:: k
    if(.not.pm_main_process) return
    write(*,*)
    write(*,*) '-------------CALL TRACE---------------------------'
    do k=coder%loop_depth,1,-1
       if(k>max_loop_depth) then
          write(*,*) 'Procedure call: (call not recorded)'
          cycle
       endif
       node=coder%imports(k)
       call print_call_details(coder,node,&
            int(coder%import_cblock(k)%offset,pm_i16))
       if(k>1) write(*,*)
    enddo
    write(*,*) '--------------------------------------------------'
    write(*,*)
  end subroutine infer_trace

  subroutine print_call_details(coder,node,tno)
    type(code_state):: coder
    type(pm_ptr),intent(in):: node
    integer(pm_i16),intent(in):: tno
    integer:: i
    character(len=100):: str
    character(len=2):: join
    integer:: nloop,n
    type(pm_ptr):: tv
    call pm_name_string(coder%context,&
         cnode_get_name(node,cnode_modl_name),str)
    tv=pm_typ_vect(coder%context,tno)
    write(*,*) 'Call at: ',trim(str),&
         ' line:',cnode_get_num(node,cnode_lineno)
    if(cnode_flags_clear(node,call_flags,&
         ior(call_is_loop_call,call_is_reduce_call))) then
       call more_error(coder%context,' '//trim(sig_name_str(coder,&
            cnode_get_num(node,call_sig)))//' (')
       nloop=0
    else
       call more_error(coder%context,' '//trim(sig_name_str(coder,&
            cnode_get_num(node,call_sig)))//' <')
       do i=sym_this_dom,sym_this_tile
          if(i<sym_this_tile) then
             join=', '
          else
             join=' '
          endif
          call more_error(coder%context,&
               '     '//trim(syshook(i-num_sym))//'= '//&
               trim(pm_typ_as_string(coder%context,&
               pm_tv_arg(tv,i-sym_this_dom+1)))//join)
       enddo
       n=sym_this_index-sym_this_dom+1
       nloop=cnode_get_num(node,call_nloop)+n
       if(nloop>n) then
          call more_error(coder%context,&
               ' > [')
          do i=n+1,nloop
             if(i<nloop) then
                join=', '
             else
                join=' '
             endif
             call more_error(coder%context,&
                  '     '//&
                  trim(pm_typ_as_string(coder%context,&
                  pm_tv_arg(tv,i)))//join)
          enddo
          if(nloop+1>pm_tv_numargs(tv)) then
            call more_error(coder%context,&
                  ' ]')
          else
             call more_error(coder%context,&
                  ' ] (')
          endif
       else
          if(nloop+1>pm_tv_numargs(tv)) then
             call more_error(coder%context,&
                  ' >')
          else
             call more_error(coder%context,&
                  ' > (')
          endif
       endif
    endif
    if(nloop+1<=pm_tv_numargs(tv)) then
       do i=nloop+1,pm_tv_numargs(tv)
          if(i<pm_tv_numargs(tv)) then
             join=', '
          else
             join=' '
          endif
          call more_error(coder%context,&
               '     '//&
               trim(pm_typ_as_string(coder%context,pm_tv_arg(tv,i)))//join)
       enddo
       call more_error(coder%context,' )')
    endif
  end subroutine print_call_details

  subroutine print_proc_details(coder,name,isloop,nloop,tno)
    type(code_state):: coder
    integer(pm_p),intent(in):: name
    logical,intent(in):: isloop
    integer,intent(in):: nloop
    integer(pm_i16),intent(in):: tno
    integer:: i,n,nx,narg
    integer(pm_i16):: typ
    type(pm_ptr):: tv
    character(len=256):: str
    logical:: angles
    str=' '
    call pm_name_string(coder%context,name,str(2:))
    n=len_trim(str)+1
    if(.not.isloop) then
       str(n:)=pm_typ_as_string(coder%context,tno)
    else
       tv=pm_typ_vect(coder%context,tno)
       angles=.false.
       nx=sym_this_index-sym_this_dom+1
       do i=1,nx
          typ=pm_tv_arg(tv,i)
          if(typ/=0) then
             if(.not.angles) then
                if(add_char('<')) goto 777
                angles=.true.
             else
                if(add_char(',')) goto 777
             endif
             call typ_to_str(coder%context,typ,str,n,.false.)
             if(n>len(str)-10) goto 777
          endif
       enddo
       if(angles) then
          if(add_char('>')) goto 777
       endif
       if(nloop>0) then
          if(add_char('[')) goto 777
          do i=nx+1,nx+nloop
             typ=pm_tv_arg(tv,i)
             call typ_to_str(coder%context,typ,str,n,.false.)
             if(n>len(str)-10) goto 777
             if(i<nx+nloop) then
                if(add_char(',')) goto 777
             endif
          enddo
          if(add_char(']')) goto 777
       endif
       narg=pm_tv_numargs(tv)
       if(narg>nx+nloop) then
          if(add_char('(')) goto 777
          do i=nx+nloop+1,narg
             typ=pm_tv_arg(tv,i)
             call typ_to_str(coder%context,typ,str,n,.false.)
             if(n>len(str)-10) goto 777
             if(i<narg) then
                if(add_char(',')) goto 777
             endif
          enddo
          if(add_char(')')) goto 777
       endif
    endif
777 continue
    call more_error(coder%context,trim(str))
  contains
    function add_char(c) result(term)
      character(len=*),intent(in):: c
      logical:: term
      if(n>len(str)-10-len(c)) then
         str(n:n+2)='...'
         term=.true.
      else
         str(n:n+len(c)-1)=c
         n=n+len(c)
         term=.false.
      endif
    end function add_char
  end subroutine print_proc_details

end module pm_infer
