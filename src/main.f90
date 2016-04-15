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

program pm
  use pm_memory
  use pm_parser
  use pm_linker
  use pm_codegen
  use pm_infer
  use pm_sysdefs
  use pm_backend
  use pm_vm
  implicit none
  type(pm_context),pointer:: context
  type(parse_state),target:: parser
  type(code_state),target:: coder
  type(finaliser),target:: fs
  type(pm_ptr):: root,prog,sptr,svec,p
  type(pm_ptr),dimension(1):: arg
  type(pm_ptr),target:: ve,ve1
  integer:: name,err
  character*100 str
  logical:: out_debug_files,ok
  integer(pm_p):: i
  integer(pm_ln):: jj
  type(pm_reg),pointer:: reg

  call pm_check_kinds

  context=>pm_init_gc()
  reg=>pm_register(context,'main',ve,ve1)

  if(iargc()==1) then
     call getarg(1,str)
     out_debug_files=.false.
  else if(iargc()==2) then
     call getarg(1,str)
     if(str/='-d') call usage() 
     out_debug_files=.true.
     call getarg(2,str)
  else
     call usage()
  endif
  inquire(file=trim(str)//'.pmm',exist=ok)
  if(.not.ok) call usage()

  ! Parse sytem module
  call init_parser(parser,context)
  call sysdefs(parser)
  call pm_gc(context,.false.)
  if(out_debug_files) then
     open(unit=9,file='sysmod.dmp')
     call dump_module(context,9,parser%sysmodl)
     close(9)
  endif

  ! ************* Parser ********************
  if(pm_debug_level>1) write(*,*) 'PARSING'
  name=name_entry(parser,trim(str))
  call new_modl(parser,name)
  root=parser%modls
  do
     parser%modl=parser%modls
     if(pm_fast_isnull(parser%modl)) exit
     parser%modls=parser%modl%data%ptr(&
          parser%modl%offset+modl_link)
     if(pm_fast_isnull(parser%modl)) exit
     call pm_name_string(context,&
          int(get_modl_name(parser%modl),pm_p),str)
     open(unit=8,file=trim(str)//'.pmm')
     parser%iunit=8
     parser%lineno=0
     call next_line(parser)
     call scan(parser)
     call decl(parser)
     close(8)
     call pm_gc(context,.false.)
     do jj=0,pm_dict_size(context,parser%modl_dict)
        p=pm_dict_val(context,parser%modl_dict,jj)
     enddo
     if(out_debug_files) then
        open(unit=9,file=trim(str)//'.dmp')
        call dump_module(context,9,parser%modl)
        close(9)
     endif
  enddo
  if(parser%error_count>0) stop 'Parse errors'
  call pm_gc(context,.false.)

  ! ***************Linker*******************
  if(pm_debug_level>1) write(*,*) 'LINKING'
  call link_includes(context,parser%modl_dict)
  call pm_gc(context,.false.)
  if(out_debug_files) then
     open(unit=8,file='linker.out')
     call dump_module(context,8,root)
     close(8)
  endif
  
  !************ Code generation ***************
  if(pm_debug_level>1) write(*,*) 'CODE GENERATION'
  prog=root%data%ptr(root%offset+modl_stmts)
  if(pm_fast_isnull(prog)) stop 'No program defined to run'
  call init_coder(context,coder)
  call trav_prog(coder,prog)
  if(out_debug_files) then
!!$     open(unit=8,file='coder.out')
!!$     call dump_code_tree(coder,pm_null_obj,8,coder%vstack(1),1)
!!$     write(8,*) '------------------------'
!!$     call dump_sigs(coder,8)
!!$     close(8)
     open(unit=8,file='codegen.out')
     call qdump_code_tree(coder,pm_null_obj,8,coder%vstack(1),1)
     call dump_sigs(coder,8)
     close(8)
  endif
  if(coder%num_errors>0) stop 'Code generation errors'
  
  ! *********** Type Inference *********************
  if(pm_debug_level>1) write(*,*) 'TYPE INFERENCE'
  call prc_prog(coder)
  if(out_debug_files) then
     open(unit=8,file='infer.out')
     call dump_code_tree(coder,pm_null_obj,8,coder%vstack(1),1)
     call dump_res_sigs(coder,8)
     close(8)
  endif
  if(coder%num_errors>0) stop 'Type inference errors'


  !**************** Backend **********************
  if(pm_debug_level>1) write(*,*) 'FINAL STAGE'
  call init_fs(context,fs,coder%proc_cache)
  call finalise_prog(fs,coder%vstack(1))
  call finalise_procs(fs)
  if(out_debug_files) then
     open(unit=8,file='final.out')
     context%funcs=fs%code_cache
     call dump_wc(context,8)
     close(8)
  endif

  !********** Run interpreter ***********************
  if(pm_debug_level>1) write(*,*) 'RUNNING...'
  ! Pass over code from backend
  context%funcs=pm_dict_vals(context,fs%code_cache)
  ! Create intial vector engine structure
  ve1=pm_fast_newnc(context,pm_long,4_pm_p)
  ve1%data%ln(ve1%offset)=0_pm_ln
  ve1%data%ln(ve1%offset+1)=0_pm_ln
  ve1%data%ln(ve1%offset+2)=0_pm_ln
  ve1%data%ln(ve1%offset+3)=1_pm_ln
  ve=pm_fast_newnc(context,pm_pointer,2_pm_p)
  ve%data%ptr(ve%offset)=pm_null_obj
  ve%data%ptr(ve%offset+1)=ve1
  arg(1)=ve

  ! Run the code
  err=pm_run(context,pm_null_obj,pm_null_obj,&
       pm_null_obj,op_call,0_pm_i16,arg,1)

  call pm_delete_register(context,reg)
contains
  include 'fisnull.inc'
  include 'fnewnc.inc'
  subroutine usage()
    write(*,*) 'Usage: pm [-d] root_module_name'
    write(*,*) 'Module name should not have .pmm suffix'
    write(*,*) 'Option -d outputs intermediate compilation for debug'
    stop
  end subroutine usage
end program pm
