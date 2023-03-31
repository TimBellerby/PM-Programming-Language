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
  use pm_kinds
  use pm_sysdep
  use pm_compbase
  use pm_memory
  use pm_hash
  use pm_lib
  use pm_parser
  use pm_linker
  use pm_codegen
  use pm_infer
  use pm_sysdefs
  use pm_cbackend
  implicit none
  type(pm_context),pointer:: context
  type(parse_state),target:: parser
  type(code_state),target:: coder
  type(finaliser),target:: fs
  type(pm_ptr):: root,prog,sptr,svec,p
  type(pm_ptr),dimension(1):: arg
  type(pm_ptr),target:: ve
  integer:: name,err
  character(len=pm_max_filename_size):: str
  logical:: out_debug_files,ok
  integer(pm_p):: i
  integer(pm_ln):: jj
  type(pm_reg),pointer:: reg

  ! ****** Initialise ******

  call pm_check_kinds
  context=>pm_init_gc()
  call pm_init_compilation
  call pm_init_names(context)
  call init_typ(context)

  ! ****** Command line *****
  
  reg=>pm_register(context,'main',ve)

  if(pm_get_cl_count()==1) then
     call pm_get_cl_arg(1,str)
     out_debug_files=.false.
  else if(pm_get_cl_count()==2) then
     call pm_get_cl_arg(1,str)
     if(str/='-d') call usage() 
     out_debug_files=pm_main_process
     call pm_get_cl_arg(2,str)
  else
     call usage()
  endif

  call pm_module_filename(str)
  !write(*,*) 'Input file:',trim(str)
  if(.not.pm_file_exists(str)) call usage()

  ! ************* Parser ********************
  if(pm_debug_level>1) write(*,*) 'PARSING>>'
  
  ! Parse sytem module
  call init_parser(parser,context)
  call sysdefs(parser)
  call pm_gc(context,.false.)
  if(out_debug_files) then
     open(unit=9,file='sysmod.dmp')
     call dump_module(context,9,parser%sysmodl)
     close(9)
  endif

  ! Parse other modules
  name=pm_name_entry(context,trim(str))
  call new_modl(parser,name)
  root=parser%modls
  do
     parser%modl=parser%modls
     if(pm_fast_isnull(parser%modl)) exit
     parser%modls=parser%modl%data%ptr(&
          parser%modl%offset+modl_link)
     if(pm_fast_isnull(parser%modl)) exit
     str=' '
     call pm_name_string(context,&
          int(get_modl_name(parser%modl),pm_p),str)
     call pm_module_filename(str)
     call pm_open_file(pm_comp_file_unit,str,ok)
     if(.not.ok) then
        if(pm_main_process) then
           write(*,*) 'Cannot open: '//trim(str)
           call pm_stop('Compilation terminated')
        endif
     endif
     parser%iunit=pm_comp_file_unit
     parser%lineno=0
     call next_line(parser)
     call scan(parser)
     call decl(parser)
     close(pm_comp_file_unit)
     call pm_gc(context,.false.)
     if(out_debug_files) then
        open(unit=9,file=trim(str)//'.dmp')
        call dump_module(context,9,parser%modl)
        close(9)
     endif
  enddo
  if(parser%error_count>0) call pm_stop('Parse errors')
  call pm_gc(context,.false.)

  ! ***************Linker*******************
  if(pm_debug_level>1) write(*,*) 'LINKING>>'
  call link_includes(context,parser%modl_dict)
  call pm_gc(context,.false.)
  if(out_debug_files) then
     open(unit=pm_comp_file_unit,file='linker.out')
     call dump_module(context,pm_comp_file_unit,root)
     close(pm_comp_file_unit)
  endif
  
  !************ Code generation ***************
  if(pm_debug_level>1) write(*,*) 'CODE GENERATION>>'
  prog=root%data%ptr(root%offset+modl_stmts)
  if(pm_fast_isnull(prog)) call pm_stop('No program defined to run')
  call init_coder(context,coder)
  call trav_prog(coder,prog)
  if(out_debug_files) then
     open(unit=pm_comp_file_unit,file='codegen.out')
     call qdump_code_tree(coder,pm_null_obj,pm_comp_file_unit,coder%vstack(1),1)
     call dump_sigs(coder,pm_comp_file_unit)
     close(pm_comp_file_unit)
     
  endif
  if(coder%num_errors>0) call pm_stop('Code generation errors')
  
  ! *********** Type Inference *********************
  if(pm_debug_level>1) write(*,*) 'TYPE INFERENCE>>'
  call prc_prog(coder)
  if(out_debug_files) then
     open(unit=pm_comp_file_unit,file='infer.out')
     call dump_code_tree(coder,pm_null_obj,pm_comp_file_unit,coder%vstack(1),1)
     call dump_res_sigs(coder,pm_comp_file_unit)
     close(pm_comp_file_unit)
  endif
  if(coder%num_errors>0) call pm_stop('Type inference errors')
  
  !**************** Backend **********************
  if(pm_debug_level>1) write(*,*) 'FINAL STAGE>>'
  call init_fs(context,fs,coder%proc_cache)
  open(unit=pm_comp_file_unit,file='pmout.f90')
  fs%outunit=pm_comp_file_unit
  call finalise_prog(fs,coder%vstack(1))
  close(unit=pm_comp_file_unit)
  if(coder%num_errors>0) call pm_stop('Errors in final coding stage')
contains
  
  include 'fisnull.inc'
  include 'fnewnc.inc'
  
  subroutine usage()
    if(pm_main_process) then
       write(*,*) 'Usage: pm [-d] root_module_name'
       write(*,*) 'Module name should not have .pmm suffix'
       write(*,*) 'Option -d outputs intermediate compilation for debug'
    endif
    call pm_stop(' ')
  end subroutine usage

end program pm
