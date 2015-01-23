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

program pm
  use pm_memory
  use pm_parser
  use pm_linker
  use pm_codegen
  use pm_infer
  use pm_sysdefs
  use pm_backend
  use pm_vm
  type(pm_context),pointer:: context
  type(parse_state),target:: parser
  type(code_state),target:: coder
  type(finaliser),target:: fs
  type(pm_ptr):: root,prog,sptr,svec,args(1)
  integer:: name,err
  character*100 str
  logical:: out_debug_files,ok
  context=>pm_init_gc()

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

  call init_parser(parser,context)
  call sysdefs(parser)

  if(pm_debug_level>0) write(*,*) 'PARSING'
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
     if(out_debug_files) then
        open(unit=9,file=trim(str)//'.dmp')
        call dump_module(context,9,parser%modl)
        close(9)
     endif
  enddo
  if(parser%error_count>0) stop 'Parse errors'
  if(pm_debug_level>0) write(*,*) 'LINKING'
  call link_includes(context,parser%modl_dict)
  if(out_debug_files) then
     open(unit=8,file='linker.out')
     call dump_module(context,8,root)
     close(8)
  endif
  if(pm_debug_level>0) write(*,*) 'CODE GENERATION'
  prog=root%data%ptr(root%offset+modl_stmts)
  if(pm_fast_isnull(prog)) stop 'No program defined to run'
  call init_coder(context,coder)
  call trav_prog(coder,prog)
  if(out_debug_files) then
     open(unit=8,file='coder.out')
     call dump_sigs(coder,8)
     call dump_code_tree(coder,pm_null_obj,8,coder%vstack(1),1)
     close(8)
  endif
  if(coder%num_errors>0) stop 'Code generation errors'
  if(pm_debug_level>0) write(*,*) 'INFER'
  call prc_prog(coder)
  if(out_debug_files) then
     open(unit=8,file='infer.out')
     call dump_code_tree(coder,pm_null_obj,8,coder%vstack(1),1)
     call dump_res_sigs(coder,8)
     close(8)
  endif
  if(coder%num_errors>0) stop 'Type inference errors'
  if(pm_debug_level>0) write(*,*) 'FINAL'
  call init_fs(context,fs,coder%proc_cache)
  call finalise_prog(fs,coder%vstack(1))
  call finalise_procs(fs)
  if(out_debug_files) then
     open(unit=8,file='final.out')
     context%funcs=fs%code_cache
     call dump_wc(context,8)
     close(8)
  endif
  context%funcs=pm_dict_vals(context,fs%code_cache)
  err=pm_run(context,pm_null_obj,pm_null_obj,&
       pm_null_obj,op_call,0_pm_i16,args,0,sptr,svec)
contains
  include 'fisnull.inc'
  subroutine usage()
    write(*,*) 'Usage: pm [-d] root_module_name'
    write(*,*) 'Module name should not have .pmm suffix'
    write(*,*) 'Option -d outputs intermediate compilation for debug'
    stop
  end subroutine usage
end program pm
