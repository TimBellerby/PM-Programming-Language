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

program pm
  use pm_kinds
  use pm_sysdep
  use pm_compbase
  use pm_memory
  use pm_hash
  use pm_options
  use pm_lib
  use pm_symbol
  use pm_types
  use pm_parser
  use pm_linker
  use pm_codegen
  use pm_infer
  use pm_wcode
  use pm_optimise
  use pm_backend
  implicit none

  ! Memory manager state
  type(pm_context),pointer:: context
  
  character(len=pm_max_filename_size):: module_name
  type(pm_ptr),target:: root_module,module_dict,visibility,sysmodl
  type(pm_ptr),target:: prog_code,proc_cache,code_cache,poly_cache,typeset
  logical:: ok
  type(pm_reg),pointer:: reg
  real:: time,newtime,time0
  integer:: num_modl
  integer,dimension(:),pointer:: modl_names
  integer:: i
  ! Initialise
  call pm_check_kinds
  context=>pm_init_gc()
  call init_par(context)
  call pm_init_compilation
  call pm_init_names(context)
  call set_op_names
  call pm_init_types(context)
  call cpu_time(time)
  time0=time
  reg=>pm_register(context,'main',root_module,module_dict,visibility,sysmodl,&
       prog_code,proc_cache,code_cache,typeset)

  ! Command line 
  call pm_get_command_line(context,module_name)

  ! Run REPL if -i option is present
  if(pm_opts%is_repl) then
     call repl(context)
     stop
  endif

  ! Compilation
  call run_parser(module_name,root_module,module_dict,visibility,sysmodl,num_modl,modl_names)
  
  call run_linker(root_module,module_dict,sysmodl)
  call run_coder_and_inference(root_module,visibility,prog_code,proc_cache,poly_cache,num_modl,modl_names)
  call run_wcode_stage(prog_code,proc_cache,code_cache,poly_cache,typeset,modl_names)

  if(pm_opts%print_timings) write(*,'(A20,F7.4,"s")') 'TOTAL',time-time0
  
  ! Run wordcodes or use them to generate source
  if(pm_is_compiling) then
     !if(pm_debug_level>1) write(*,*) 'OPTIMISING...'
     !call optimise_prog(context,code_cache,poly_cache)
     !if(pm_opts%out_debug_files) then
     !  open(unit=pm_comp_file_unit,file='optimiser.out')
     !  context%funcs=code_cache
     !  call dump_wc(context,6) !pm_comp_file_unit)
     !  close(pm_comp_file_unit)
     !endif
 
     if(pm_debug_level>1.or..true.) write(*,*) 'CREATING SOURCE...'
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
  subroutine run_parser(mname,root,dict,visibility,sysmodl,num_modl,modl_names)
    character(len=*):: mname
    type(pm_ptr),intent(out):: root,dict,visibility,sysmodl
    integer,intent(out):: num_modl
    integer,dimension(:),pointer,intent(out):: modl_names
    
    ! Parser state
    type(parse_state),target:: parser
    integer:: name,module_name,package_name
    character(len=pm_max_filename_size):: str,str2

    if(pm_debug_level>1) write(*,*) 'PARSING>>'

    if(pm_opts%out_sysmod) then
       open(unit=45,file='sysmod.out')
    endif
    
    ! Parse sytem module
    call init_parser(parser,context)

    if(pm_opts%checks_to_run_set) then
       if(pm_is_compiling) then
          call parse_from_string(parser,trim(pm_opts%checks_to_run))
       else
          call parse_from_string(parser,'sys,'//trim(pm_opts%checks_to_run))
       endif
       call scan(parser)
       if(checks(parser,pm_null_obj)) then
          call pm_stop('Bad -fcheck= option')
       endif
    endif
    
    call dcl_module(parser,'PM__system')
    parser%sysmodl=parser%modl
    sysmodl=parser%modl

    call pm_module_filename('lib.sys.pm',str2,pm_opts%lib_path_set,pm_opts%lib_path)
    call pm_open_file(pm_comp_file_unit,str2,ok)
    if(.not.ok) then
       if(pm_main_process) then
          write(*,*) 'Cannot open system module: '//trim(str2)
       endif
       call pm_stop('Compilation terminated')
    endif
    call parse_file_on_unit(parser,pm_comp_file_unit,.false.)
    close(pm_comp_file_unit)
    if(parser%error_count>0) then
       if(pm_main_process) then
          write(*,*) 'Cannot parse system module: '//trim(str2)
       endif
       call pm_stop('Compilation terminated')
    endif
    
    call pm_gc(context,.false.)
    if(pm_opts%out_debug_files) then
       open(unit=9,file='sysmod.dmp')
       call dump_module(context,9,parser%sysmodl)
       close(9)
    endif
    
    ! Parse other modules
    call pm_module_filename(mname,str2,pm_opts%lib_path_set,pm_opts%lib_path)
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
       module_name=get_modl_name(parser%modl)
       call pm_name_string(context,module_name,str)
       call pm_module_filename(str,str2,pm_opts%lib_path_set,pm_opts%lib_path)
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
       if(pm_opts%out_debug_files) then
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
    num_modl=parser%modl_index
    modl_names=>parser%modl_names
    
    call term_parser(parser)
    call pm_gc(context,.false.)

    if(pm_opts%print_timings) then
       call cpu_time(newtime)
       write(*,'(A20,F7.4,"s")') 'PARSING TOOK',newtime-time
       time=newtime
    endif
    
  end subroutine run_parser

  ! ***************Linker*******************
  subroutine run_linker(root,modl_dict,sysmodl)
    type(pm_ptr),intent(in):: root,modl_dict,sysmodl
    integer:: err
    if(pm_debug_level>1) write(*,*) 'LINKING>>'
    err=0
    call link_includes(context,err,modl_dict,sysmodl)
    if(err>0) &
         call pm_stop('Compilation terminated due to errors linking modules')
    call pm_gc(context,.false.)
    if(pm_opts%out_debug_files) then
       open(unit=pm_comp_file_unit,file='linker.out')
       call dump_module(context,pm_comp_file_unit,root)
       close(pm_comp_file_unit)
    endif

    if(pm_opts%print_timings) then
       call cpu_time(newtime)
       write(*,'(A20,F7.4,"s")') 'LINKING TOOK',newtime-time
       time=newtime
    endif
    
  end subroutine run_linker

  subroutine run_coder_and_inference(root,visibility,proc_code,proc_cache,poly_cache,num_modl,modl_names)
    type(pm_ptr),intent(in):: root,visibility
    integer,intent(in):: num_modl
    integer,dimension(:),pointer,intent(in):: modl_names
    type(pm_ptr),intent(out):: proc_code,proc_cache,poly_cache
    type(code_state),target:: coder
    call init_coder(context,coder,visibility,num_modl,modl_names)
    call run_coder(coder,root)
    call run_type_inference(coder)
    proc_code=coder%vstack(1)
    proc_cache=coder%proc_cache
    poly_cache=coder%poly_cache
    call term_coder(coder)
    call pm_gc(context,.false.)
  end subroutine run_coder_and_inference

  !************ Intermediate code generation ***************
  subroutine run_coder(coder,root)
    type(code_state),target:: coder
    type(pm_ptr),intent(in):: root
    type(pm_ptr):: prog
    
    if(pm_debug_level>1) write(*,*) 'CODE GENERATION>>'
    prog=root%data%ptr(root%offset+modl_stmts)
    if(pm_fast_isnull(prog)) call pm_stop('No program defined to run')
    call trav_prog(coder,prog)
    if(coder%num_errors>0) &
         call pm_stop('Compilation terminated due to semantic errors')
    if(pm_opts%out_debug_files) then
       open(unit=pm_comp_file_unit,file='codegen.out')
       if(pm_opts%old_files) then
          call qdump_code_tree(coder,pm_null_obj,pm_comp_file_unit,coder%vstack(1),1)
          call dump_sigs(coder,pm_comp_file_unit)
       else
          call print_cblock_cnode(coder%context,pm_comp_file_unit,pm_null_obj,coder%modl_names,coder%sig_cache,&
               pm_null_obj,coder%vstack(1),2)
          call print_all_sigs(coder%context,pm_comp_file_unit,coder%modl_names,coder%sig_cache,coder%sig_cache,coder%poly_cache)
       endif
       close(pm_comp_file_unit)
    endif
    if(pm_opts%print_timings) then
       call cpu_time(newtime)
       write(*,'(A20,F7.4,"s")') 'CODING TOOK',newtime-time
       time=newtime
    endif
  end subroutine run_coder
  
  ! *********** Type Inference *********************
  subroutine run_type_inference(coder)
    type(code_state),target:: coder
    integer:: i
    logical:: save_variants,save_elems,save_members
    if(pm_debug_level>1) write(*,*) 'TYPE INFERENCE>>'
    call inf_prog(coder)

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
           write(4,*) iand(pm_type_flags(context,i),pm_type_has_storage)/=0,iand(pm_type_flags(context,i),pm_type_has_fix)/=0,&
               iand(pm_type_flags(context,i),pm_type_is_soa),trim(pm_type_as_string(context,i))
       enddo
       pm_opts%show_members=save_members
       pm_opts%show_elems=save_elems
       pm_opts%show_variants=save_variants
       close(4)
    endif
    
    if(pm_opts%out_debug_files) then
       open(unit=pm_comp_file_unit,file='infer.out')
       if(pm_opts%old_files) then
          call qdump_code_tree(coder,pm_null_obj,pm_comp_file_unit,coder%vstack(1),1)
          call dump_res_sigs(coder,pm_comp_file_unit)
       else
          call print_cblock_cnode(coder%context,pm_comp_file_unit,cnode_arg(coder%vstack(1),2),coder%modl_names,coder%sig_cache,&
               coder%proc_cache,cnode_arg(coder%vstack(1),1),2)
          call print_all_sigs(coder%context,pm_comp_file_unit,coder%modl_names,coder%sig_cache,coder%proc_cache,coder%poly_cache)
       endif
       close(pm_comp_file_unit)
    endif

   if(coder%num_errors>0) then
       call pm_stop('Compilation terminated due to type-inference errors')
    endif

    
    if(pm_opts%print_timings) then
       call cpu_time(newtime)
       write(*,'(A20,F7.4,"s")') 'INFERENCE TOOK',newtime-time
       time=newtime
    endif

  end subroutine run_type_inference

  ! ******** Wcode stage - create wordcodes ***********
  subroutine run_wcode_stage(prog_code,proc_cache,code_cache,poly_cache,typeset,modl_names)
    type(pm_ptr),intent(in):: prog_code,proc_cache,poly_cache
    integer,dimension(:),pointer,intent(in):: modl_names
    type(pm_ptr),intent(out):: code_cache,typeset
    
    ! Wcode stage state
    type(wcoder),target:: wcd
    
    if(pm_debug_level>1) write(*,*) 'WCODE STAGE>>'
    call init_wcoder(context,wcd,proc_cache,poly_cache,modl_names)
    call wcode_prog(wcd,prog_code)
    call wcode_procs(wcd)
    if(pm_opts%out_debug_files) then
       open(unit=pm_comp_file_unit,file='wcode.out')
       context%funcs=wcd%code_cache
       if(pm_is_compiling.and..not.pm_opts%old_files) then
          call print_comp_procs(context,pm_comp_file_unit,context%funcs)
       else
          call dump_wc(context,pm_comp_file_unit)
       endif
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
       write(*,'(A20,F7.4,"s")') 'WCODE STAGE TOOK',newtime-time
       time=newtime
    endif
  end subroutine run_wcode_stage

  subroutine repl(context)
    type(pm_context),pointer:: context
    type(parse_state),target:: parser
    type(code_state):: coder
    type(wcoder),target:: wcd
    type(pm_ptr):: root,prog,dict
    integer:: name,err
    character(len=2000):: line,str2
    logical:: first
    call init_parser(parser,context)
    call dcl_module(parser,'PM__system')
    parser%sysmodl=parser%modl
    call pm_module_filename('lib.sys.pm',str2,pm_opts%lib_path_set,pm_opts%lib_path)
    call pm_open_file(pm_comp_file_unit,str2,ok)
    if(.not.ok) then
       if(pm_main_process) then
          write(*,*) 'Cannot open system module: '//trim(str2)
       endif
       call pm_stop('Compilation terminated')
    endif
    !write(*,*) 'Parsing',trim(str)
    call parse_file_on_unit(parser,pm_comp_file_unit,.false.)
    close(pm_comp_file_unit)
    if(parser%error_count>0) then
       if(pm_main_process) then
          write(*,*) 'Cannot parse system module: '//trim(str2)
       endif
       call pm_stop('Compilation terminated')
    endif
    name=pm_name_entry(context,'PM__REPL')
    call new_modl(parser,name)
    root=parser%modls
    parser%modl=parser%modls
    first=.true.
    write(*,'(a)') 'PM interactive mode - "exit" to exit'
    do
       write(*,'(a)',advance='NO') 'PM> '
       read(*,'(a)') line
       if(line=='exit') return
       call parse_expr_from_string(parser,line,first)
       first=.false.
       dict=parser%modl_dict
       visibility=parser%visibility
       num_modl=parser%modl_index
       if(parser%error_count==0) then
          err=0
          call link_includes(context,err,dict,parser%sysmodl)
          if(err==0) then
             prog=root%data%ptr(root%offset+modl_stmts)
             if(pm_fast_isnull(prog)) call pm_stop('No program defined to run')
             !call dump_parse_tree(context,6,prog,2)
             call init_coder(context,coder,visibility,num_modl,modl_names)
             call trav_prog(coder,prog)
             call inf_prog(coder)
             if(coder%num_errors==0) then
                prog_code=coder%vstack(1)
                proc_cache=coder%proc_cache
                call init_wcoder(context,wcd,proc_cache,pm_null_obj,modl_names)
                call wcode_prog(wcd,prog_code)
                code_cache=wcd%code_cache
                call wcode_procs(wcd)
                if(wcd%num_errors==0) then
                   call pm_run_prog(context,pm_dict_vals(context,code_cache))
                endif
             endif
          endif
       endif
    enddo
  end subroutine repl
  
end program pm
