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

! Virtual Machine for PM interpreter

module pm_vm
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_types
  use pm_sysdefs
  use pm_array
  implicit none

  integer,parameter:: shrink_thresh=4

  integer,parameter:: vm_ok=0
  integer,parameter:: vm_error=1
  integer,parameter:: vm_break=2
  integer,parameter:: vm_stepout=3
  integer,parameter:: vm_resume=4

contains
  
  ! *************************************
  ! Run main interpreter loop
  ! *************************************
  recursive function pm_run(context,funcin,stackin,pcin,&
       op,op2,args,num_args) result(errno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: funcin,stackin,pcin
    integer(pm_i16),intent(in):: op,op2
    integer,intent(in):: num_args
    type(pm_ptr),dimension(num_args),intent(in):: args
    integer:: errno
    type(pm_ptr),target,dimension(pm_max_args):: arg
    integer,target:: nargs
    type(pm_ptr),target:: stack,newve
    type(pm_ptr):: func,pc,newstack,newfunc,newpc,ve,p,v,w
    integer(pm_i16):: opcode,opcode2,opcode3,oparg,t1,t2
    integer:: i,ii,n
    integer(pm_ln):: j,jj,k,kk,m,esize

    logical:: flip,ok
    integer(pm_p):: stacksize
    type(pm_reg),pointer:: reg
    character(len=200):: mess
    nargs=0
    reg=>pm_register(context,'vm args',&
         stack,newve,array=arg,array_size=nargs)
    opcode=op
    opcode2=op2
    if(num_args>0) arg(1:num_args)=args(1:num_args)
    nargs=num_args
    func=funcin
    stack=stackin
    pc=pcin
    if(opcode>=0) goto 20

    10 continue
    
    nargs=0
    opcode=pc%data%i16(pc%offset)
    opcode2=pc%data%i16(pc%offset+1_pm_p)
    opcode3=pc%data%i16(pc%offset+2_pm_p)
    n=iand(opcode3,pm_max_stack)  ! Number arguments
    !stack%data%ptr(stack%offset)%offset=&
    !     opcode3/(pm_max_stack+1) ! Stack top
    if(pm_debug_level>3) then
       write(*,*) 'DECODING',opcode,&
            op_names(opcode),opcode2,n,opcode3/(pm_max_stack+1),'pc=',pc%offset
    endif
    oparg=pc%data%i16(pc%offset+3_pm_p)
    arg(1)=stack%data%ptr(stack%offset+oparg)
    ve=arg(1)%data%ptr(arg(1)%offset+1)
    esize=ve%data%ln(ve%offset)
    ve=arg(1)%data%ptr(arg(1)%offset)
    if(pm_debug_level>3) then
       write(*,*)arg(1)%data%vkind,ve%data%vkind,esize
    endif
    do i=2,n
       oparg=pc%data%i16(pc%offset+i+2_pm_p)
       if(pm_debug_level>3) write(*,*) 'OPARG=', oparg
       if(oparg>=0) then
          arg(i)=stack%data%ptr(stack%offset+oparg)
          if(pm_debug_level>3) then
             write(*,*) i,'STACK',oparg
          endif
       else if(oparg>=-int(pm_max_stack,pm_i16)) then
          arg(i)%data=>stack%data
          arg(i)%offset=stack%offset-oparg
          if(pm_debug_level>3) &
               write(*,*) i,'STACKREF',-oparg,arg(i)%data%esize,&
               arg(i)%data%hash,stack%offset,arg(i)%offset
       else
          w=func%data%ptr(func%offset-oparg-pm_max_stack)
          ii=pm_fast_vkind(w)
          if(ii==pm_null) then
             arg(i)=w
          elseif(ii==pm_string) then
             arg(i)=make_string_vector(context,w,esize)
          else
             arg(i)=pm_new(context,ii,esize+1)
             call pm_fill_vect(context,arg(i),w)
          endif
       endif
    enddo

    nargs=n
    pc%offset=pc%offset+n+3_pm_p
    ! Empty ve
    if(pm_fast_vkind(ve)==pm_tiny_int) then
       if(opcode==op_jmp_empty_ve) then
          pc%offset=pc%offset+opcode2
       elseif(opcode>=op_and_jmp_none.and.opcode<=op_andnot_jmp_any) then
          call set_arg(2,arg(1))
       endif
       goto 10
    endif

    20 continue
    if(pm_debug_level>3) then
       write(*,*) 'RUNNING',opcode,&
            op_names(opcode),opcode2,opcode3,'pc=',pc%offset
    endif
    select case(opcode)
    case(op_call)
       newfunc=context%funcs%data%ptr(&
            context%funcs%offset+opcode2)
       goto 30
    case(op_poly_call)
       call poly_call_lookup(context,&
            func%data%ptr(func%offset+opcode2),arg,nargs,&
            opcode,opcode2)
       if(opcode==-1) then
          ! All done
          goto 10
       elseif(opcode==op_call) then
          newfunc=context%funcs%data%ptr(&
            context%funcs%offset+opcode2)
          goto 30
       else
          goto 20
       endif
    case(op_var_call)
       call var_call
       opcode=v%data%ptr(v%offset)%offset
       opcode2=v%data%ptr(v%offset+1_pm_p)%offset
       goto 20
    case(op_lookup_error)
       call runtime_error(context,'Could not find matching procedure')
       goto 999
    case(op_vcall)
       v=arg(nargs)
       do i=0,pm_fast_esize(v)
          arg(nargs+i)=v%data%ptr(v%offset+i)
       enddo
       nargs=nargs+pm_fast_esize(v)
       opcode=pc%data%i16(pc%offset)
       opcode2=pc%data%i16(pc%offset+1_pm_p)
       opcode3=pc%data%i16(pc%offset+2_pm_p)
       pc%offset=pc%offset+4_pm_p
       goto 20
    case(op_return)
       if(nargs>0) then
          do i=2,nargs
             v=stack%data%ptr(stack%offset+pm_stack_locals+i-1)
             v%data%ptr(v%offset)=arg(i)
          enddo
       endif
       pc=stack%data%ptr(stack%offset+pm_stack_pc)
       func=stack%data%ptr(stack%offset+pm_stack_func)
       stack=stack%data%ptr(stack%offset+pm_stack_oldstack)
       if(pm_fast_isnull(pc)) then
          errno=0
          goto 888
       endif
    case(op_jmp)
       pc%offset=pc%offset+opcode2
    case(op_and_jmp_none:op_andnot_jmp_any)
       flip=opcode==op_andnot_jmp_none.or.opcode==op_andnot_jmp_any
       if(pm_fast_isnull(ve)) then
          ! No active mask - logical vector becomes new mask
          newve=pm_new(context,pm_logical,esize)
          if(flip) then
             newve%data%l(newve%offset:newve%offset+esize)=.not. &
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
          else
             newve%data%l(newve%offset:newve%offset+esize)= &
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
          endif
          k=count(newve%data%l(newve%offset:newve%offset+esize))
          if(k>0.and.esize+1>=k*shrink_thresh) then
             newve=shrink_ve(context,newve,esize,k)
          endif
       elseif(pm_fast_vkind(ve)==pm_int) then
          ! Vector engine is using a list of indices 
          ! - subset active cells
          jj=0
          do j=0,pm_fast_esize(ve)
             k=ve%data%ln(ve%offset+j)
             if(arg(3)%data%l(arg(3)%offset+k).neqv.flip) jj=jj+1
          enddo
          if(jj>0) then
             newve=pm_new(context,pm_int,int(jj,pm_ln))
             k=0
             do j=0,pm_fast_esize(ve)
                jj=ve%data%ln(ve%offset+j)
                if(arg(3)%data%l(arg(3)%offset+jj).neqv.flip) then
                   newve%data%ln(newve%offset+k)=jj
                   k=k+1
                endif
             enddo
          else
             k=0
          endif
       else
          ! Calculate new vector of active cell flags
          newve=pm_new(context,pm_logical,esize+1)
          k=0
          do j=0,esize
             ok=ve%data%l(ve%offset+j).and.&
                  (arg(3)%data%l(arg(3)%offset+j).neqv.flip)
             newve%data%l(newve%offset+j)=ok
             if(ok) k=k+1
          enddo
          ! If only a small number of cells active in vector, 
          ! change to index list
          if(k>0.and.esize+1>=k*shrink_thresh) then
             newve=shrink_ve(context,newve,esize,k)
          endif
       endif
       ! Empty ve
       if(k==0) newve=pm_fast_tinyint(context,0_pm_p)
       ! New vector engine structure
       ve=pm_fast_newnc(context,pm_pointer,2)
       ve%data%ptr(ve%offset)=newve
       ve%data%ptr(ve%offset+1)=arg(1)%data%ptr(arg(1)%offset+1)
       call set_arg(2,ve)
       if(opcode<op_and_jmp_any) then
          ! .. _jmp_none
          if(k==0) pc%offset=pc%offset+opcode2
       else
          ! .. _jmp_any
          if(k>0) pc%offset=pc%offset+opcode2
       endif
    case(op_jmp_any_ve)
       do i=1,nargs
          if(pm_fast_vkind(arg(i)%data%ptr(arg(i)%offset))/=pm_tiny_int) then
             pc%offset=pc%offset+opcode2
             exit
          endif
       enddo
    case(op_jmp_empty_ve)
       continue
    case(op_par_loop)
       errno=par_loop(context,func,stack,pc,arg,nargs,esize)
       if(errno/=0) goto 888
    case(op_par_loop_end)
       errno=0
       goto 888
    case(op_clone_ve)
       stack%data%ptr(stack%offset+opcode2)=arg(1)
    case(op_makekeys)
       v=pm_dict_new(context,8_pm_ln)
       if(.not.pm_fast_isnull(arg(3))) then
          ok=pm_dict_merge(context,v,arg(3),.true.)
       endif
       call set_arg(2,v)
       do i=4,nargs,2
          call pm_dict_set(context,v,arg(i),arg(i+1),.true.,.true.,ok,m)
       enddo
    case(op_delkeys)
       v=pm_dict_new(context,8_pm_ln)
       call set_arg(2,v)
       delkey: do j=1,pm_dict_size(context,arg(2))
          w=pm_dict_key(context,arg(3),int(j,pm_ln))
          do i=4,nargs
            if(w%offset==arg(i)%offset) cycle delkey 
          enddo
          call pm_dict_set(context,v,w,&
               pm_dict_val(context,arg(3),int(j,pm_ln)),.true.,.true.,ok,m)
       enddo delkey
    case(op_checkkeys)
       do i=2,nargs,2
          if(.not.pm_fast_isnull(arg(i+1))) then
             call runtime_error(context,&
                  'Unexpected keyword argument: '//&
                  trim(pm_name_as_string(context,arg(i)%offset)))
             goto 999
          endif
       enddo
    case(op_getvkey)
       call set_arg(2,pm_dict_lookup(context,arg(3),arg(4)))
    case(op_get_key)
       v=stack%data%ptr(stack%offset+opcode2)
       w=stack%data%ptr(stack%offset+opcode2+1)
       if(v%data%l(v%offset)) then
          if(pm_arg_type(w)/=pm_arg_type(arg(3))) then
             call runtime_error(context,'Keyword argument has wrong type')
             goto 999
          endif
          call set_arg(2,w)
       else
          call set_arg(2,arg(3))
       endif   
   case(op_get_key2)
       v=stack%data%ptr(stack%offset+opcode2)
       w=stack%data%ptr(stack%offset+opcode2+1)
       call set_arg(2,v)
       call set_arg(3,w)
    case(op_default)
       v=alloc_arg(int(opcode2),2)
    case(op_print)
       ve=shrink_ve(context,ve,esize)
       call vector_print_string(context,arg(2),ve)
    case(op_dump)
       write(*,*) '====================== dump ======================'
       call pm_dump_tree(context,6,arg(2),1)
       write(*,*) '=================================================='
    case(op_concat)
       ve=shrink_ve(context,ve,esize)
       call set_arg(2,vector_concat_string(context,ve,&
            arg(3),arg(4)))
    case(op_clone)
       call set_arg(2,copy_vector(context,arg(3),ve))
    case(op_assign)
       call vector_assign(context,arg(2),arg(3),ve,esize)
    case(op_struct)
       v=pm_fast_newusr(context,pm_struct_type,int(nargs,pm_p))
       call set_arg(2,v)
       if(opcode2<0) opcode2=pm_arglist_type(context,&
            pm_typ_is_struct,-opcode2,arg(3:),nargs-1)
       v%data%ptr(v%offset+1_pm_p)=pm_fast_tinyint(context,int(opcode2,pm_p))
       v%data%ptr(v%offset+2:v%offset+nargs-1)=arg(3:nargs)
    case(op_rec)
       v=pm_fast_newusr(context,pm_rec_type,int(nargs,pm_p))
       call set_arg(2,v)
       if(opcode2<0) opcode2=pm_arglist_type(context,&
            pm_typ_is_rec,-opcode2,arg(3:),nargs-1)
       v%data%ptr(v%offset+1_pm_p)=pm_fast_tinyint(context,int(opcode2,pm_p))
       v%data%ptr(v%offset+2:v%offset+nargs-1)=arg(3:nargs)
    case(op_check_logical)
       if(pm_fast_vkind(arg(2))/=pm_logical) then
          !!! Check poly kind
          call runtime_error(context,&
               'Logical expression did not yield a bool value')
          goto 999
       endif
    case(op_check)
       if(pm_fast_isnull(ve)) then
          if(.not.all(arg(3)%data%l(arg(3)%offset:&
               arg(3)%offset+esize))) then
             ve=shrink_ve(context,ve,esize)
             call vector_get_string(context,arg(2),ve,0_pm_ln,mess)
             call runtime_error(context,mess)
             goto 999
          endif
       elseif(pm_fast_vkind(ve)==pm_logical) then
          if(any(ve%data%l(ve%offset:ve%offset+esize).and..not.&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize))) then
             ve=shrink_ve(context,ve,esize)
             call vector_get_string(context,arg(2),ve,0_pm_ln,mess)
             call runtime_error(context,mess)
             goto 999 
          endif
       else
          if(.not.all(arg(3)%data%l(arg(3)%offset+&
               ve%data%ln(ve%offset:&
               ve%offset+pm_fast_esize(ve))))) then
             ve=shrink_ve(context,ve,esize)
             call vector_get_string(context,arg(2),ve,0_pm_ln,mess)
             call runtime_error(context,mess)
             goto 999 
          endif
       endif
    case(op_elem)
       call set_arg(2,&
            arg(3)%data%ptr(arg(3)%offset+opcode2))
    case(op_poly_elem)
       i=pm_arg_type(arg(2))
       if(i==pm_poly_type) then
          ve=shrink_ve(context,ve,esize)
          w=pm_fast_newusr(context,pm_poly_type,3_pm_p)
          call set_arg(2,w)
          v=arg(3)%data%ptr(arg(3)%offset+2_pm_p)
          w%data%ptr(w%offset+2_pm_p)=v
          w=pm_assign_new(context,w,1_pm_ln,pm_pointer,esize+1,.false.)
          v=arg(3)%data%ptr(arg(3)%offset+1_pm_p)
          i=0
          do jj=0,pm_fast_esize(ve)
             k=ve%data%ln(ve%offset+jj)
             p=v%data%ptr(v%offset+k)
             call pm_elem_offset(context,pm_arg_type(p),&
                  opcode2,.false.,t1,t2)
             if(i==0) then 
                i=t2
             elseif(i/=t2) then
                i=-1
             endif
             if(t1<0) goto 996
             w%data%ptr(w%offset+k)=p%data%ptr(p%offset+t1)
          enddo
          if(i>0) then
             call set_arg(2,ptr_vec_get_type(context,w,ve,0_pm_ln,esize))
          endif
       elseif(i==pm_struct_type.or.i==pm_rec_type) then
          call pm_elem_offset(context,int(pm_arg_type(arg(2)),pm_i16),&
               opcode2,.false.,t1,t2)
          if(t1<0) goto 996
          call set_arg(2,arg(2)%data%ptr(arg(2)%offset+t1))
       else
          goto 996
       endif
    case(op_array)
       call set_arg(2,make_array_dim(context,t1,&
            arg(3),arg(4),arg(5),ve))
    case(op_array_get_elem)
       errno=0
       call set_arg(2,array_index(context,arg(3),&
            arg(4),ve,errno))
       if(errno/=0) goto 997
    case(op_array_set_elem)
       errno=0
       call array_set_index(context,arg(2),arg(3),arg(4),ve,errno)
       if(errno/=0) goto 997
    case(op_export_array)
       errno=0
       call array_export(context,arg(2),arg(3),arg(4),&
            arg(1)%data%ptr(arg(1)%offset),errno)
       if(errno/=0) goto 997
    case(op_iota)
       call set_arg(2,vector_iota(context,&
            arg(3),arg(4),arg(5),arg(6),arg(7)%data%ptr(arg(7)%offset+1)))
    case(op_import_val)
       call set_arg(2,import_vector(context,&
            arg(3),arg(1)%data%ptr(arg(1)%offset+1)))
    case(op_export)
       call set_arg(2,arg(3))  !!! Modify for multiple threads
    case(op_extract)
       
    case(op_extractelm)
       call set_arg(2,arg(3)%data%ptr(arg(3)%offset+pm_array_vect))
    case(op_make_array)
       call set_arg(2,make_array_from_vect(context,t1,&
            arg(3),arg(4),arg(5),ve))
    case(op_string_i)
       ve=shrink_ve(context,ve,esize)
       call set_arg(2,vector_make_string(context,&
            ve,arg(3),fmt_i_width,fmt_i))
    case(op_assign_i)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%i(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%i(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             arg(2)%data%i(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%i(arg(3)%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
    case(op_add_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_sub_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_mult_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
   case(op_divide_i)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_gt_i)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_ge_i)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%i(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_long_i)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%i(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%i(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif

    case(op_string_ln)
       ve=shrink_ve(context,ve,esize)
       call set_arg(2,&
            vector_make_string(context,&
            ve,arg(3),fmt_ln_width,fmt_ln))
    case(op_assign_ln)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%ln(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%ln(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             arg(2)%data%ln(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%ln(arg(3)%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
   case(op_add_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)+&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)+&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)+&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_sub_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)-&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)-&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)-&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_mult_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)*&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)*&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)*&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
   case(op_divide_ln)
       v=alloc_arg(pm_long,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%ln(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)/&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%ln(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)/&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%ln(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)/&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_gt_ln)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_ge_ln)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>=&
               arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)>=&
                  arg(4)%data%ln(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)>=&
                  arg(4)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_int_ln)
       v=alloc_arg(pm_int,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%i(v%offset:v%offset+esize)=&
               arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%i(v%offset:v%offset+esize)=&
                  arg(3)%data%ln(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%i(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%ln(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif

   case(op_string_l)
       ve=shrink_ve(context,ve,esize)
       call set_arg(2,&
            vector_make_string(context,&
            ve,arg(3),fmt_l_width,fmt_l))
    case(op_assign_l)
       if(pm_fast_vkind(ve)==pm_null) then
          arg(2)%data%l(arg(2)%offset:arg(2)%offset+esize)=&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             arg(2)%data%l(arg(2)%offset:arg(2)%offset+esize)=&
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             arg(2)%data%l(arg(2)%offset+ve%data%ln(ve%offset+j))=&
                  arg(3)%data%l(arg(3)%offset+ve%data%ln(ve%offset+j))
          end forall
       endif
    case(op_and)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).and.&
               arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).and.&
                  arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset).and.&
                  arg(4)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
   case(op_or)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).or.&
               arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize).or.&
                  arg(4)%data%l(arg(4)%offset:arg(4)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  arg(3)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset).or.&
                  arg(4)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(4)%offset)
          end forall
       endif
    case(op_not)
       v=alloc_arg(pm_logical,2)
       if(pm_fast_vkind(ve)==pm_null) then
          v%data%l(v%offset:v%offset+esize)=&
               .not.arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
       elseif(pm_fast_vkind(ve)==pm_logical) then
          where(ve%data%l(ve%offset:ve%offset+esize))
             v%data%l(v%offset:v%offset+esize)=&
                  .not.arg(3)%data%l(arg(3)%offset:arg(3)%offset+esize)
          end where
       else
          forall(j=0:pm_fast_esize(ve))
             v%data%l(ve%data%ln(ve%offset+j)+&
                  v%offset)=&
                  .not.arg(3)%data%l(ve%data%ln(ve%offset+j)+&
                  arg(3)%offset)
          end forall
       endif

    case default
       write(*,*) 'Unknown opcode=',opcode
       write(*,*) op_names(opcode)
       call pm_panic('unknown opcode')
    end select

    goto 10
    
30  continue

    ! ***********************************
    ! This section implements a call
    !************************************
    ! save current stack top
    newpc=newfunc%data%ptr(newfunc%offset)
    stacksize=newpc%data%i16(newpc%offset)
    i=newpc%data%i16(newpc%offset+1)
    newstack=pm_fast_new(context,pm_stack,stacksize)
    newstack%data%ptr(newstack%offset)=&
         pm_fast_tinyint(context,int(stacksize-1,pm_p))
    newstack%data%ptr(newstack%offset+1)=pc
    newstack%data%ptr(newstack%offset+2)=stack
    newstack%data%ptr(newstack%offset+3)=func
    if(i<nargs) then
       if(pm_debug_level>3) write(*,*) 'TRIM ARG LIST>',i,nargs
       newstack%data%ptr(newstack%offset+pm_stack_locals:&
            newstack%offset+i+pm_stack_locals-1)=&
            arg(1:i)
       context%temp_obj1=newstack
       v=pm_fast_newnc(context,pm_pointer,int(nargs-i,pm_p))
       v%data%ptr(v%offset:v%offset+nargs-i-1)=arg(i+1:nargs)
       newstack%data%ptr(newstack%offset+i+pm_stack_locals)=v
       context%temp_obj1=pm_null_obj
       nargs=i+1
    else
       if(pm_debug_level>0) then
          if(i/=nargs) then
             write(*,*) 'Num Args=',nargs,'Num pars expected=',i
             call pm_panic('Arg/param mismatch')
          endif
       endif
       if(nargs>0) then
          newstack%data%ptr(newstack%offset+pm_stack_locals:&
               newstack%offset+pm_stack_locals+nargs-1)=&
               arg(1:nargs)
       endif
    endif
    pc=newpc
    pc%offset=pc%offset+3_pm_p
    stack=newstack
    func=newfunc
    if(pm_debug_level>3) then
       write(*,*) '======CALL==NEW STACK======',nargs
       do i=5,nargs+3
          write(*,*) i,nargs+3
          call pm_dump_tree(context,6,stack%data%ptr(stack%offset+i),1)
       enddo
       write(*,*) '============================'
    endif
      
    goto 10

996 continue

    ! struct/rec operation error
    call runtime_error(context,'Cannot access structure/record element: '//&
               trim(pm_name_as_string(context,int(opcode2,pm_p))))
    goto 999

997 continue

    ! Vector/array operation errors
    select case(errno)
    case(vector_type_error)
       call runtime_error(context,'Type mismatch')
    case(vector_size_error)
       call runtime_error(context,'Size mismatch')
    case(vector_slice_error)
       call runtime_error(context,'Malformed slice')
    case(vector_index_error)
       call runtime_error(context,'Index out of range')
    end select

999 continue

    call dump_stack(context,stack,func,pc)

888 continue

    call pm_delete_register(context,reg)
    
    return
    
  contains

    include 'fisnull.inc'
    include 'fvkind.inc'
    include 'fnew.inc'
    include 'fnewnc.inc'
    include 'fnewusr.inc'
    include 'fesize.inc'
    include 'ftiny.inc'
    include 'ftypeof.inc'



    subroutine set_arg(iarg,val)
      integer,intent(in):: iarg
      type(pm_ptr),intent(in):: val
      arg(iarg)%data%ptr(arg(iarg)%offset)=val
    end subroutine set_arg

    function alloc_arg(vkind,iarg) result(ptr)
      integer,intent(in):: vkind,iarg
      type(pm_ptr):: ptr
      ptr=pm_new(context,vkind,esize+1)
      arg(iarg)%data%ptr(arg(iarg)%offset)=ptr
    end function alloc_arg

    function shrink_ve(context,mask,esize,n) result(ind)
      type(pm_context),pointer:: context
      type(pm_ptr),intent(in):: mask
      integer(pm_ln),intent(in):: esize
      integer,intent(in),optional:: n
      type(pm_ptr):: ind
      integer(pm_ln):: j,k
      if(pm_fast_isnull(mask)) then
         ind=pm_new(context,pm_long,esize+1)
         do j=0,esize
            ind%data%ln(ind%offset+j)=j
         enddo
      elseif(pm_fast_vkind(mask)==pm_long) then
         ind=mask
      else
         if(present(n)) then
            k=n
         else
            k=count(mask%data%l(mask%offset:mask%offset+esize))
         endif
         ind=pm_new(context,pm_long,k)
         k=0
         do j=0,esize
            if(mask%data%l(mask%offset+j)) then
               ind%data%ln(ind%offset+k)=j
               k=k+1
            endif
         enddo
      endif
    end function shrink_ve

    ! Handle passing keys/vkeys to a var call
    subroutine var_call
      type(pm_ptr):: vk_set,vk,key,keys,vals,val 
      logical:: vkey
      integer:: i,j,k,m
      v=func%data%ptr(func%offset+opcode2)
      v=pm_dict_lookup(context,v,arg(1))
      i=v%data%ptr(v%offset+3_pm_p)%offset
      if(i<0) then
         vkey=.true.
         i=-i
      else
         vkey=.false.
      endif
      ! Delete proc var param
      do j=1,i
         arg(j)=arg(j+1)
      enddo
      vk_set=v%data%ptr(v%offset+2_pm_p)
      if(pm_fast_isnull(vk_set)) then
         if(vkey) then
            arg(i+1:nargs-1)=arg(i+2:nargs)
            nargs=nargs-1
         else
            arg(i+1:nargs-2)=arg(i+3:nargs)
            nargs=nargs-2
         endif
      else
         if(vkey) then
            vk=arg(i+2)
            j=pm_set_size(context,vk_set)
            arg(i+j+2:nargs+j+1)=arg(i+1:nargs)
            nargs=nargs+j-1
            if(pm_fast_isnull(vk)) then
               do k=i+1,i+j+1
                  arg(k)=pm_null_obj
               enddo
            else
               arg(i+1:i+j)=pm_null_obj
               arg(i+j+1)=pm_dict_new(context,8_pm_ln)
               keys=pm_dict_keys(context,vk)
               vals=pm_dict_vals(context,vk)
               do k=0,pm_dict_size(context,vk)-1
                  key=keys%data%ptr(keys%offset+k)
                  m=pm_set_lookup(context,vk_set,key)
                  if(m>0) then
                     arg(i+m)=vals%data%ptr(vals%offset+k)
                  else
                     call pm_dict_set(context,arg(i+j+1),key,&
                          vals%data%ptr(vals%offset+k),.true.,.true.,ok)
                  endif
               enddo
            endif
         else
            vk=arg(i+2)
            j=pm_set_size(context,vk_set)
            arg(i+j+1:nargs+j-2)=arg(i+3:nargs)
            nargs=nargs+j-2
            if(pm_fast_isnull(vk)) then
               do k=i+1,i+j
                  arg(k)=pm_null_obj
               enddo
            else
               keys=pm_set_keys(context,vk_set)
               do k=i+1,i+j
                  arg(k)=pm_dict_lookup(context,vk,&
                       keys%data%ptr(keys%offset+k-i-1))
               enddo
            endif
         endif
      endif
    end subroutine var_call

  end function pm_run
 
  ! Parallel loop 
  function par_loop(context,func,stack,pc,arg,num_args,old_esize) result(errno) 
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: func,stack,pc
    type(pm_ptr),dimension(num_args):: arg
    integer,intent(in):: num_args
    integer(pm_ln),intent(in):: old_esize
    integer:: errno
    type(pm_ptr):: newpc,newve
    integer(pm_ln):: n,m
    newpc=pc
    newpc%offset=newpc%offset+4_pm_p
    arg(2)%data%ptr(arg(2)%offset)=pm_fast_newnc(context,pm_pointer,2)
    newve=arg(2)%data%ptr(arg(2)%offset)
    newve%data%ptr(newve%offset)=pm_null_obj
    n=pm_fast_esize(arg(3))+1_pm_ln
    m=sum(arg(3)%data%ln(arg(3)%offset:arg(3)%offset+n-1_pm_ln))-1_pm_ln
    newve=pm_assign_new(context,newve,1_pm_ln,pm_long,n+4_pm_ln,.false.)
    newve%data%ln(newve%offset)=m
    newve%data%ln(newve%offset+1)=0
    newve%data%ln(newve%offset+2)=0
    newve%data%ln(newve%offset+3)=m
    newve%data%ln(newve%offset+4:newve%offset+3+n)=&
         arg(3)%data%ln(arg(3)%offset:arg(3)%offset+n-1)
    errno=pm_run(context,func,stack,newpc,-1_pm_i16,0_pm_i16,&
         arg,num_args)
  contains
    include 'fesize.inc'
    include 'fnewnc.inc'
  end function  par_loop

  ! Look up polymorphic call
  subroutine poly_call_lookup(context,polyfunc,args,nargs,op,op2)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: polyfunc
    integer,intent(in):: nargs
    type(pm_ptr),dimension(nargs),intent(in):: args
    integer(pm_i16),intent(out):: op,op2
    integer(pm_i16):: tno
    integer:: i,st

    st=polyfunc%data%i16(polyfunc%offset)
    tno=pm_arglist_type(context,pm_typ_is_tuple,0_pm_i16,args(st:),nargs-st+1)
    call dump_type(context,6,tno,2)
    do i=1,pm_fast_esize(polyfunc),3
       if(pm_arglist_type_includes(context,&
            polyfunc%data%i16(polyfunc%offset+i),tno)) then
          op=polyfunc%data%i16(polyfunc%offset+i+1)
          op2=polyfunc%data%i16(polyfunc%offset+i+2)
          return
       endif
    enddo
    op=op_lookup_error
  contains
    include 'fesize.inc'
  end subroutine poly_call_lookup
  
  ! Vectorised polymorphic call
  subroutine poly_call(context,polyfunc,args,nargs,&
       op,op2,start,finish,sptr,svec)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: polyfunc
    integer,intent(in):: nargs
    type(pm_ptr),dimension(nargs),intent(inout):: args
    integer(pm_i16),intent(out):: op,op2
    integer(pm_ln):: start,finish
    type(pm_ptr),intent(out):: sptr,svec
    integer,dimension(pm_max_args):: poly_arg
    integer(pm_i16),dimension(pm_max_args):: typ
    type(pm_ptr),dimension(pm_max_args):: xargs
    integer:: i,num_poly_args,na,istat
    integer(pm_ln):: esize,j,n
    type(pm_ptr),target:: ivec,set,sptrs,svecs
    type(pm_ptr):: p,func,sp,sv
    type(pm_reg),pointer:: reg
    integer(pm_i16):: opcode,opcode2
    num_poly_args=0
    do i=1,nargs
       typ(i)=pm_fast_typeof(args(i))
       if(typ(i)==pm_poly_type) then
          num_poly_args=num_poly_args+1
          poly_arg(num_poly_args)=i
       endif
    enddo
    if(num_poly_args==0) then
       call poly_call_lookup(context,polyfunc,args,nargs,op,op2)
    else
       ! Get type vectors for each poly argument
       na=0
       reg=>pm_register(context,'poly call',&
            ivec,set,sptrs,svecs,array=xargs,array_size=na)
       esize=finish-start+1
       ivec=pm_new(context,pm_long,esize+1)
       set=pm_set_new(context,16_pm_ln)
       do i=1,num_poly_args
          xargs(i)=args(poly_arg(i))%data%ptr(&
               args(poly_arg(i))%offset+1_pm_p)
       enddo
       do j=0,esize
          do i=1,num_poly_args
             typ(i)=pm_fast_typeof(xargs(i))
          enddo
          ivec%data%ln(ivec%offset+j)=&
               pm_iset_add(context,set,typ,num_poly_args)-1
       enddo
       n=pm_set_size(context,set)
       xargs(1:nargs)=args(1:nargs)
       na=nargs
       call separate_vecs(context,args,nargs,ivec,n-1)
       do i=1,nargs
          if(pm_fast_vkind(args(i))==pm_stack) then
             args(i)=pm_new(context,pm_pointer,n)
          endif
       enddo
       do j=0,n-1
          do i=1,nargs
             if(pm_fast_vkind(xargs(i))/=pm_stack) &
                  xargs(i)=args(i)%data%ptr(args(i)%offset+j)
          enddo
          call poly_call_lookup(context,polyfunc,xargs,nargs,&
               opcode,opcode2)
          istat=pm_run(context,pm_null_obj,pm_null_obj,pm_null_obj,&
               opcode,opcode2,xargs,nargs)
          if(istat==0) then
             do i=1,nargs
                if(pm_fast_vkind(xargs(i))==pm_stack) then
                   call pm_ptr_assign(context,args(i),j,&
                        xargs(i)%data%ptr(xargs(i)%offset))
                endif
             enddo
          endif
       enddo
       ! All done - write back outputs
       do i=1,nargs
          if(pm_fast_vkind(xargs(i))==pm_stack) then
             xargs(i)%data%ptr(xargs(i)%offset)=&
                  make_poly_vec(context,args(i),ivec)
          endif
       enddo
       op=-1
       call pm_delete_register(context,reg)
    endif
  contains
    include 'fvkind.inc'
    include 'ftypeof.inc'
    include 'fesize.inc'
    include 'fnewusr.inc'
  end subroutine  poly_call

  function pm_arglist_type(context,tkind,tname,args,nargs) result(tno)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: tkind,tname
    type(pm_ptr),dimension(nargs),intent(in):: args
    integer,intent(in):: nargs
    integer(pm_i16):: tno
    integer(pm_i16),dimension(pm_max_args+2):: t
    integer:: i
    t(1)=tkind
    t(2)=tname
    do i=1,nargs
       tno=pm_fast_typeof(args(i))
       if(tno>=pm_struct_type.and.tno<=pm_array_type) then
          tno=args(i)%data%ptr(args(i)%offset+1_pm_p)%offset
       endif
       t(i+2)=tno
    enddo
    tno=pm_ivect_lookup(context,context%tcache, &
         t,nargs+2)
    if(tno<=0) tno=pm_idict_add(context,context%tcache,&
         t,nargs+2,pm_null_obj)
  contains
    include 'ftypeof.inc'
  end function pm_arglist_type

  function pm_arg_type(arg) result(tno)
    type(pm_ptr),intent(in):: arg
    integer(pm_i16):: tno
    tno=pm_fast_typeof(arg)
    if(tno>=pm_struct_type.and.tno<=pm_array_type) then
       tno=arg%data%ptr(arg%offset+1_pm_p)%offset
    endif
  contains
    include 'ftypeof.inc'
  end function pm_arg_type

  function pm_arglist_type_includes(context,supertype,subtype) &
       result(ok)
    type(pm_context),pointer:: context
    integer(pm_i16),intent(in):: supertype,subtype
    logical:: ok
    ok=pm_typ_includes(context,supertype,subtype)
  end function pm_arglist_type_includes

  subroutine runtime_error(context,errmesg)
    type(pm_context),pointer:: context
    character(len=*),intent(in):: errmesg
    write(*,*)
    write(*,*) 'Runtime error: ',trim(errmesg)
  end subroutine runtime_error

  subroutine dump_stack(context,dstack,dfunc,dpc)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: dstack,dfunc,dpc
    type(pm_ptr):: stack,func,pc
    integer(pm_p):: name
    character(len=80):: mess
    integer(pm_p):: line,modl
    stack=dstack
    func=dfunc
    pc=dpc
    write(*,*) '=========================Call trace ========================='
    do while(.not.pm_fast_isnull(func))
       call proc_line_module(func,&
            int(pc%offset-func%data%ptr(func%offset)%offset)+1,line,modl)
       name=proc_get_name(func)
       if(name==sym_pm_system) then
          mess='Program: '
       else
          mess=trim(pm_name_as_string(context,name))//' in: '
       endif
       call pm_name_string(context,modl,mess(len_trim(mess)+2:))
       write(*,*) trim(mess),'  line',line
       func=stack%data%ptr(stack%offset+pm_stack_func)
       pc=stack%data%ptr(stack%offset+pm_stack_pc)
       stack=stack%data%ptr(stack%offset+pm_stack_oldstack)
    enddo
    write(*,*) '============================================================='
  contains
    include 'fisnull.inc'
  end subroutine dump_stack

end module pm_vm
