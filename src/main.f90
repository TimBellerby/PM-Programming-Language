!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2020
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
  use pm_options
  use pm_lib
  use pm_symbol
  use pm_parser
  use pm_linker
  use pm_codegen
  use pm_infer
  use pm_sysdefs
  use pm_wcode
  use pm_optimise
  use pm_backend
  implicit none

  ! Memory manager state
  type(pm_context),pointer:: context
  
  character(len=pm_max_filename_size):: module_name
  type(pm_ptr),target:: root_module,module_dict,visibility
  type(pm_ptr),target:: prog_code,proc_cache,code_cache,poly_cache,typeset
  logical:: out_debug_files,ok
  type(pm_reg),pointer:: reg
  real:: time,newtime,time0

  ! Initialise
  call pm_check_kinds
  context=>pm_init_gc()
  call init_par(context)
  call pm_init_compilation
  call pm_init_names(context)
  call set_op_names
  call init_typ(context)
  call cpu_time(time)
  time0=time
  reg=>pm_register(context,'main',root_module,module_dict,visibility,&
       prog_code,proc_cache,code_cache,typeset)

  ! Command line 
  call pm_get_command_line(context,module_name,out_debug_files)
  
  ! Compilation
  call run_parser(module_name,root_module,module_dict,visibility)
  call run_linker(root_module,module_dict)
  call run_coder_and_inference(root_module,visibility,prog_code,proc_cache,poly_cache)
  call run_wcode_stage(prog_code,proc_cache,code_cache,poly_cache,typeset)

  if(pm_opts%print_timings) write(*,'(A20,F7.4,1Hs)') 'TOTAL',time-time0
  
  ! Run wordcodes or use them to generate source
  if(pm_is_compiling) then
     if(pm_debug_level>1) write(*,*) 'OPTIMISING...'
     call optimise_prog(context,code_cache,poly_cache)
     if(out_debug_files) then
       open(unit=pm_comp_file_unit,file='optimiser.out')
       context%funcs=code_cache
       call dump_wc(context,pm_comp_file_unit)
       close(pm_comp_file_unit)
    endif
 
     if(pm_debug_level>1) write(*,*) 'CREATING SOURCE...'
     open(unit=9,file='PMOUT.F90')
     call gen_prog(context,code_cache,poly_cache,typeset,9)
     close(9)
  else
     if(pm_debug_level>1) write(*,*) 'RUNNING...'
     call pm_run_prog(context,pm_dict_vals(context,code_cache))
  endif

  ! Tidy up
  call pm_delete_register(context,reg)
  call finalise_par(context)
  
contains
  
  include 'fisnull.inc'
  include 'fnewnc.inc'

  ! ************* Parser ********************
  subroutine run_parser(mname,root,dict,visibility)
    character(len=*):: mname
    type(pm_ptr),intent(out):: root,dict,visibility
    ! Parser state
    type(parse_state),target:: parser
    integer:: name
    character(len=pm_max_filename_size):: str,str2

    if(pm_debug_level>1) write(*,*) 'PARSING>>'

    if(pm_opts%out_sysmod) then
       open(unit=45,file='sysmod.out')
    endif
    
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
    call pm_module_filename(mname,str2)
    if(.not.pm_file_exists(str2)) then
       if(pm_main_process) then
          write(*,*) 'Cannot open source file '//trim(str2)//&
               ' for module: '//trim(mname)
       endif
       call pm_stop('Compilation terminated')
    endif
    name=pm_name_entry(context,trim(mname))
    pm_main_module=name
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
            get_modl_name(parser%modl),str)
       call pm_module_filename(str,str2)
       call pm_open_file(pm_comp_file_unit,str2,ok)
       if(.not.ok) then
          if(pm_main_process) then
             write(*,*) 'Cannot open source file '//trim(str2)//&
                  'for module: '//trim(str)
          endif
          call pm_stop('Compilation terminated')
       endif
       !write(*,*) 'Parsing',trim(str)
       call parse_file_on_unit(parser,pm_comp_file_unit,root==parser%modl)
       close(pm_comp_file_unit)
       if(out_debug_files) then
          open(unit=9,file=trim(str)//'.dmp')
          call dump_module(context,9,parser%modl)
          close(9)
       endif
       if(parser%error_count==0) call pm_gc(context,.false.)
    enddo

    if(pm_opts%out_sysmod) close(45)
    
    if(parser%error_count>0) &
         call pm_stop('Compilation terminated due to syntax errors')

    dict=parser%modl_dict
    visibility=parser%visibility
 
    call term_parser(parser)
    call pm_gc(context,.false.)

    if(pm_opts%print_timings) then
       call cpu_time(newtime)
       write(*,'(A20,F7.4,1Hs)') 'PARSING TOOK',newtime-time
       time=newtime
    endif
    
  end subroutine run_parser

  ! ***************Linker*******************
  subroutine run_linker(root,modl_dict)
    type(pm_ptr),intent(in):: root,modl_dict
    integer:: err
    if(pm_debug_level>1) write(*,*) 'LINKING>>'
    err=0
    call link_includes(context,err,modl_dict)
    if(err>0) &
         call pm_stop('Compilation terminated due to errors linking modules')
    call pm_gc(context,.false.)
    if(out_debug_files) then
       open(unit=pm_comp_file_unit,file='linker.out')
       call dump_module(context,pm_comp_file_unit,root)
       close(pm_comp_file_unit)
    endif

    if(pm_opts%print_timings) then
       call cpu_time(newtime)
       write(*,'(A20,F7.4,1Hs)') 'LINKING TOOK',newtime-time
       time=newtime
    endif
    
  end subroutine run_linker

  subroutine run_coder_and_inference(root,visibility,proc_code,proc_cache,poly_cache)
    type(pm_ptr),intent(in):: root,visibility
    type(pm_ptr),intent(out):: proc_code,proc_cache,poly_cache
    type(code_state),target:: coder
    call init_coder(context,coder,visibility)
    call run_coder(coder,root)
    call run_type_inference(coder)
    proc_code=coder%vstack(1)
    proc_cache=coder%proc_cache
    if(pm_is_compiling) then
       poly_cache=coder%poly_cache
    else
       poly_cache=pm_null_obj
    endif
    call term_coder(coder)
    call pm_gc(context,.false.)
  end subroutine run_coder_and_inference

  !************ Intermediate code generation ***************
  subroutine run_coder(coder,root)
    type(code_state):: coder
    type(pm_ptr),intent(in):: root
    type(pm_ptr):: prog
    
    if(pm_debug_level>1) write(*,*) 'CODE GENERATION>>'
    prog=root%data%ptr(root%offset+modl_stmts)
    if(pm_fast_isnull(prog)) call pm_stop('No program defined to run')
    call trav_prog(coder,prog)
    if(coder%num_errors>0) &
         call pm_stop('Compilation terminated due to semantic errors')
    if(out_debug_files) then
       open(unit=pm_comp_file_unit,file='codegen.out')
       call qdump_code_tree(coder,pm_null_obj,pm_comp_file_unit,coder%vstack(1),1)
       call dump_sigs(coder,pm_comp_file_unit)
       close(pm_comp_file_unit)
    endif
    if(pm_opts%print_timings) then
       call cpu_time(newtime)
       write(*,'(A20,F7.4,1Hs)') 'CODING TOOK',newtime-time
       time=newtime
    endif
  end subroutine run_coder
  
  ! *********** Type Inference *********************
  subroutine run_type_inference(coder)
    type(code_state):: coder
    integer:: i
    logical:: save_variants,save_elems,save_members
    if(pm_debug_level>1) write(*,*) 'TYPE INFERENCE>>'
    call prc_prog(coder)

    if(pm_opts%out_typelist) then
       write(*,*) 'TOTAL TYPES::',pm_dict_size(context,context%tcache)
    endif

    if(pm_opts%out_typelist) then
       open(unit=4,file='types.out')
       save_members=pm_opts%show_members
       save_elems=pm_opts%show_elems
       save_variants=pm_opts%show_variants
       !pm_opts%show_members=.true.
       !pm_opts%show_elems=.true.
       !pm_opts%show_variants=.true.
       do i=1,pm_dict_size(context,context%tcache)
          write(4,*) 'TYPE',i,pm_typ_kind(context,i)
          write(4,*) i,trim(pm_typ_as_string(context,i))
          call pm_dump_tree(context,4,pm_typ_val(context,i),2)
          call dump_type(context,4,i)
          write(4,*) 'DONE',i
       enddo
       pm_opts%show_members=save_members
       pm_opts%show_elems=save_elems
       pm_opts%show_variants=save_variants
       close(4)
    endif
    
    
    if(coder%num_errors>0) then
       call pm_stop('Compilation terminated due to type-inference errors')
    endif
 
    if(out_debug_files) then
       open(unit=pm_comp_file_unit,file='infer.out')
       call qdump_code_tree(coder,pm_null_obj,pm_comp_file_unit,coder%vstack(1),1)
       call dump_res_sigs(coder,pm_comp_file_unit)
       close(pm_comp_file_unit)
    endif
    
    if(pm_opts%print_timings) then
       call cpu_time(newtime)
       write(*,'(A20,F7.4,1Hs)') 'INFERENCE TOOK',newtime-time
       time=newtime
    endif

  end subroutine run_type_inference

  ! ******** Wcode stage - create wordcodes ***********
  subroutine run_wcode_stage(prog_code,proc_cache,code_cache,poly_cache,typeset)
    type(pm_ptr),intent(in):: prog_code,proc_cache,poly_cache
    type(pm_ptr),intent(out):: code_cache,typeset
    
    ! Wcode stage state
    type(wcoder),target:: wcd
    
    if(pm_debug_level>1) write(*,*) 'WCODE STAGE>>'
    call init_wcoder(context,wcd,proc_cache,poly_cache)
    call wcode_prog(wcd,prog_code)
    call wcode_procs(wcd)
    if(out_debug_files) then
       open(unit=pm_comp_file_unit,file='wcode.out')
       context%funcs=wcd%code_cache
       call dump_wc(context,pm_comp_file_unit)
       close(pm_comp_file_unit)
    endif
    code_cache=wcd%code_cache
    if(pm_is_compiling) then
       typeset=wcd%typeset
    endif
    if(wcd%num_errors>0) call pm_stop(&
         'Compilation terminated due to errors in parallel/concurrent matching')
    call term_wcoder(wcd)
    call pm_gc(context,.false.)
    if(pm_opts%print_timings) then
       call cpu_time(newtime)
       write(*,'(A20,F7.4,1Hs)') 'WCODE STAGE TOOK',newtime-time
       time=newtime
    endif
  end subroutine run_wcode_stage
  
end program pm
