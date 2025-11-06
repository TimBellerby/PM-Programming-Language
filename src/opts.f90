!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2025
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
module pm_options
  use pm_sysdep
  use pm_kinds
  use pm_memory
  use pm_compbase
  use pm_hash
  implicit none

  type pm_opts_type
     
     logical:: inline
     logical:: check_stmts
     logical:: show_elems
     logical:: show_members
     logical:: show_all_procs
     logical:: show_full_trace
     logical:: out_debug_files
     logical:: old_files
     integer:: proc_list
     integer:: trace_list
     logical:: show_variants
     logical:: show_details
     logical:: check_alias
     logical:: show_all_ref
     logical:: print_immediate
     logical:: show_hidden
     logical:: run_bprop
     logical:: lib_path_set
     character(len=pm_max_filename_size):: lib_path
     logical:: checks_to_run_set
     character(len=1024):: checks_to_run
     
     logical:: out_sysmod
     logical:: out_typelist
     logical:: print_timings
     logical:: hide_sysmod

     logical:: schedule

     integer:: ftn_dims
     integer:: ftn_lines
     logical:: ftn_contig
     logical:: ftn_nonptr_arg
     integer(pm_ln):: ftn_max_stack_array
     logical:: ftn_comment_lines
     logical:: ftn_comment_ops
     logical:: ftn_annotate
     logical:: ftn_name_procs
     logical:: ftn_name_vars
     logical:: ftn_name_params
     logical:: ftn_name_types
     logical:: ftn_name_elems

     logical:: is_repl

     character(len=25):: error
     character(len=7):: error_start
     logical:: colour
     
  end type pm_opts_type
  
  type(pm_opts_type):: pm_opts

contains

  subroutine init_opts(context)
    type(pm_context),pointer:: context
    logical:: colour,bright
    pm_opts%inline=.true.
    pm_opts%check_stmts=.not.pm_is_compiling
    pm_opts%show_elems=.false.
    pm_opts%show_members=.false.
    pm_opts%show_all_procs=.false.
    pm_opts%show_full_trace=.false.
    pm_opts%proc_list=11
    pm_opts%trace_list=11
    pm_opts%show_variants=.false.
    pm_opts%show_details=.false.
    pm_opts%check_alias=.not.pm_is_compiling
    pm_opts%show_all_ref=.false.
    pm_opts%print_immediate=.false.
    pm_opts%show_hidden=.false.
    pm_opts%lib_path_set=.false.
    pm_opts%checks_to_run_set=.false.
    
    pm_opts%out_sysmod=.false.
    pm_opts%out_typelist=.false.
    pm_opts%print_timings=.false.
    pm_opts%hide_sysmod=.true.
    pm_opts%out_debug_files=.false.
    pm_opts%old_files=.false.
    pm_opts%run_bprop=.true.

    pm_opts%schedule=.false.
    
    pm_opts%ftn_dims=pm_default_ftn_dims
    pm_opts%ftn_lines=pm_default_ftn_lines
    pm_opts%ftn_max_stack_array=pm_default_ftn_max_stack_array
    pm_opts%ftn_contig=pm_default_ftn_has_contiguous
    pm_opts%ftn_nonptr_arg=pm_default_ftn_nonptr_arg
    pm_opts%ftn_comment_lines=.false.
    pm_opts%ftn_comment_ops=.false.
    pm_opts%ftn_annotate=.false.
    pm_opts%ftn_name_procs=.false.
    pm_opts%ftn_name_vars=.false.
    pm_opts%ftn_name_params=.false.
    pm_opts%ftn_name_types=.false.
    pm_opts%ftn_name_elems=.false.
    colour=pm_colour_messages.and.pm_isatty(6)
    bright=pm_bright_messages
    if(bright) then
       pm_opts%error_start=pm_error_start_bright
    else
       pm_opts%error_start=pm_error_start
    endif
    if(colour) then
       pm_opts%error=pm_opts%error_start//'Error:'//pm_error_end
    else
       pm_opts%error='Error: '
    endif
    pm_opts%colour=colour

    call pm_get_env_var(pm_env_var,pm_opts%lib_path,pm_opts%lib_path_set)
    if(.not.pm_opts%lib_path_set) then
       pm_opts%lib_path_set=pm_default_lib_path_set
       pm_opts%lib_path=pm_default_lib_path
    endif

    pm_opts%is_repl=.false.
  end subroutine init_opts

  subroutine print_usage
    if(pm_main_process) then
       write(*,*)
       if(pm_is_compiling) then
          write(*,*) 'Usage: pmc [ options ] root_module_name_or_filename | --help'
       else
          write(*,*) 'Usage: pm [ options ] [ -i | root_module_name_or_filename ] | --help'
       endif
       write(*,*)
       write(*,*) '  OPTIONS'
       write(*,*) '  --help        Output longer help message'
       if(.not.pm_is_compiling)  write(*,*) '  -i            Interactive mode'
       write(*,*) '  -L<path>      Set path to locate PM libraries'
       write(*,*) '  -f<opt>       Language options'
       write(*,*) '  -H<opt>       Terminal output options'
       if(pm_is_compiling) then
          write(*,*) '  -ftn<opt>     Fortran language output options'
          write(*,*) '  -opt<opt>     Optimisation options'
       endif
       write(*,*) '  -D<opt>       Options for debugging the compiler itself (use --helpD to list)'
    endif
  end subroutine print_usage

  
  subroutine usage
    call print_usage
    call pm_stop(' ')
  end subroutine usage

  subroutine help
    call print_usage
    if(pm_main_process) then
       write(*,*) 
       write(*,*) '  Here root_module_name_or_filename is either a module name such as'
       write(*,*) '  module1 or mymodules.module2 or a filename such as mymodules/module2.pmm'
       write(*,*) '  Only the main (program) module must be named - other modules are'
       write(*,*) '  included automatically, from either the current directory or the'
       write(*,*) '  designated lib directory (set by the -L command line option, environment'
       write(*,*) '  variable PM_LANG_LIBS or equal to ./lib by default)'
       write(*,*)
       write(*,*) '  CONFIGURATION OPTIONS'
       write(*,*) '  -L<library path>  Look for library files in <library path> rather than lib'
       write(*,*) 
       write(*,*) '  LANGUAGE OPTIONS'
       write(*,*) '  -fno-inline       Do not inline any procedures.'
       write(*,*) '  -fno-check        Do not run "check" or "test" statements.'
       write(*,*) '  -fcheck           Run "check" and "test" statements.'
       write(*,*) '  -fcheck=<check_list>'
       write(*,*) '                    Give list of checks and tests to run or exclude'
       if(pm_is_compiling) then
          write(*,*) '                    Equivalent to adding the pragma $$check(check_list)'
       else
          write(*,*) '                    Equivalent to adding the pragma $$check(sys,check_list)'
       endif
       write(*,*) '                    to the start of each module'
       write(*,*) '  -fno-alias-check  Do not include runtime checks for argument aliasing'
       write(*,*) '  -falias_check     Include runtime checks for argument aliasing' 
       if(.not.pm_is_compiling) then
          write(*,*) '  -fprint-immediate'
          write(*,*) '                    Do not buffer print output by node'
       endif
       write(*,*)
       write(*,*) '  ERROR AND WARNING MESSAGE OUTPUT OPTIONS'
       write(*,*) '  -fshow-elems      Show structure/record elements in error messages.'
       write(*,*) '  -fshow-members    Show members of user defined types in error messages'
       write(*,*) '  -fshow-variants   Show all variants for proc types'
       write(*,*) '  -fshow-all-procs  List all alternative procedures in error messages'
       write(*,*) '  -fproc-list=n     Maximum number of procs to list if show-all-procs not invoked'
       write(*,*) '  -fshow-full-trace List all alternative procedures in error messages'
       write(*,*) '  -ftrace-list=n    Maximum number of calls to list if show-full-trace not invoked'

       write(*,*)
       write(*,*) '  TERMINAL DISPLAY OPTIONS'
       write(*,*) '  -HN               Do not colour-highlight error messages'
       write(*,*) '  -H                Colour-highlight error messages'
       write(*,*) '  -HB               Colour-highlight error messages using bright colours'
       write(*,*) '  -HS               Colour-highlight error messages using standard colours'
       if(pm_is_compiling) then
          write(*,*)
          write(*,*) '  OPTIMISER OPTIONS'
          write(*,*) '  -opt-sched    Run list scheduler (default)'
          write(*,*) '  -opt-no-sched Do not run list scheduler'
          write(*,*)
          write(*,*) '  FORTRAN SOURCE OUTPUT OPTIONS'
          write(*,*) '  -ftn-max-dims=n'
          write(*,*) '                 Maximum number (n) of Fortran array dimensions'
          write(*,*) '  -ftn-contig    Fortran supports CONTIGUOUS attribute'
          write(*,*) '  -ftn-no-contig Fortran does not support CONTIGUOUS attribute'
          write(*,*) '  -ftn-max-stack_array=n'
          write(*,*) '                 Maximum size (n) of array that can be stored on the stack'
          write(*,*) '  -ftn-max-lines=n'
          write(*,*) '                 Maximum number of continuation lines'
          write(*,*) '  -ftn-nonptr-args'
          write(*,*) '                 Can associate TARGET variables with POINTER parameters'
          write(*,*) '  -ftn-comment-lines'
          write(*,*) '                 Comment Fortran code with source lines'
          write(*,*) '  -ftn-comment-ops'
          write(*,*) '                 Comment Fortran code with word-code operations'
          write(*,*) '  -ftn-name-procs'
          write(*,*) '  -ftn-no-name-procs'
          write(*,*) '                 Include (or not) PM name in Fortran procedure names'
          write(*,*) '  -ftn-name-vars'
          write(*,*) '  -ftn-no-name-vars'
          write(*,*) '                 Include (or not) PM name in Fortan variable names'
          write(*,*) '  -ftn-name-params'
          write(*,*) '  -ftn-no-name-params'
          write(*,*) '                 Include (or not) PM name in Fortran parameter names'
          write(*,*) '  -ftn-name-types'
          write(*,*) '  -ftn-no-name-types'
          write(*,*) '                 Include (or not) PM name in Fortan derived types'
          write(*,*) '  -ftn-name-elems'
          write(*,*) '  -ftn-no-name-elems'
          write(*,*) '                 Include (or not) PM name in Fortan derived types elements'
          write(*,*) '  -ftn-name-all'
          write(*,*) '                 Include PM name in Fortran names'
          write(*,*) '  -ftn-annotate'
          write(*,*) '                 Include various annotation comments (mainly for debugging the compiler)'
       endif
    endif
    call pm_stop('  ')
  end subroutine help

  subroutine help_d
    write(*,*) '  OPTIONS FOR DEBUGGING THE COMPILER ITSELF'
    write(*,*) '  -Dshow-details    Show extra details of types'
    write(*,*) '  -Dshow-hidden     Show hidden procedure parameters'
    write(*,*) '  -Dno-bprop        Do not run BPROP pass'
    write(*,*) '  -D                Activate all debugging options listed below.'
    write(*,*) '  -Dfiles           Output files from each compiler stage.'
    write(*,*) '  -Dtimings         Output time taken by each compilation stage.'
    write(*,*) '  -Dtype-list       Output a list of all types used by the system.'
    call pm_stop(' ')
  end subroutine help_d
  
  subroutine pm_get_command_line(context,mname)
    type(pm_context),pointer:: context
    character(len=*),intent(out):: mname
    character(len=pm_max_filename_size+5):: arg
    integer:: i,n
    call init_opts(context)
    n=pm_get_cl_count()
    if(n==0) then
       if(pm_is_compiling) then
          call usage
       else
          pm_opts%is_repl=.true.
          return
       endif
    endif
    i=1
    do while(i<n)
       call pm_get_cl_arg(i,arg)
       if(arg(1:1)/='-') then
          if(pm_main_process) then
             write(*,*) 'Not a command line option: ',trim(arg)
             call usage()
          endif
       elseif(arg(1:2)=='-L') then
          pm_opts%lib_path_set=.true.
          pm_opts%lib_path=arg(3:)
       elseif(arg=='-HN') then
          pm_opts%error='Error:'
          pm_opts%colour=.false.
       elseif(arg=='-HS') then
          pm_opts%error=pm_error_start//'Error: '//pm_error_end
          pm_opts%error_start=pm_error_start
          pm_opts%colour=.true.
       elseif(arg=='-HB') then
          pm_opts%error=pm_error_start_bright//'Error: '//pm_error_end
          pm_opts%error_start=pm_error_start_bright
          pm_opts%colour=.true.
       elseif(arg=='-H') then
          pm_opts%error=pm_opts%error_start//'Error: '//pm_error_end
          pm_opts%colour=.true.
       elseif(arg(1:2)=='-D') then
          if(arg=='-Dshow-details') then
             pm_opts%show_details=.true.
          elseif(arg=='-Dshow-hidden') then
             pm_opts%show_hidden=.true.
          elseif(arg=='-Dno-bprop') then
             pm_opts%run_bprop=.false.
          elseif(arg=='-D') then
             pm_opts%out_debug_files=.true.
             pm_opts%out_sysmod=.true.
             pm_opts%out_typelist=.true.
             pm_opts%print_timings=.true.
             pm_opts%hide_sysmod=.false.
             pm_opts%show_all_ref=.true.
          elseif(arg=='-Dfiles') then
             pm_opts%out_debug_files=.true.
          elseif(arg=='-Dsys-mod') then
             pm_opts%out_sysmod=.true.
          elseif(arg=='-Dtype-list') then
             pm_opts%out_typelist=.true.
          elseif(arg=='-Dtimings') then
             pm_opts%print_timings=.true.
          elseif(arg=='-Dshow-sys-mod') then
             pm_opts%hide_sysmod=.false.
          elseif(arg=='-Dold-files') then
             pm_opts%old_files=.true.
          elseif(pm_main_process) then
             write(*,*) 'Not a valid compiler debugging (-D) option:',trim(arg)
             call usage()
          endif
       elseif(arg(1:2)=='-f') then
          if(arg=='-fno-inline') then
             pm_opts%inline=.false.
          elseif(arg=='-fno-check') then
             pm_opts%check_stmts=.false.
          elseif(arg=='-check') then
             pm_opts%check_stmts=.true.
          elseif(arg(1:8)=='-fcheck=') then
             pm_opts%checks_to_run=arg(9:)
             pm_opts%checks_to_run_set=.true.
          elseif(arg=='-fshow-elems') then
             pm_opts%show_elems=.true.
          elseif(arg=='-fshow-members') then
             pm_opts%show_members=.true.
	  elseif(arg=='-fshow-variants') then
             pm_opts%show_variants=.true.
          elseif(arg=='-fshow-all-procs') then
             pm_opts%show_all_procs=.true.
          elseif(arg=='-fshow-full-trace') then
             pm_opts%show_full_trace=.true.
          elseif(arg=='-falias-check') then
             pm_opts%check_alias=.true.
          elseif(arg=='-fno-alias-check') then
             pm_opts%check_alias=.false.
          elseif(arg=='-fprint-immediate'.and..not.pm_is_compiling) then
             pm_opts%print_immediate=.true.
          elseif(arg(1:12)=='-fproc-list=') then
             pm_opts%proc_list=get_num_opt(arg,arg(13:))
          elseif(arg(1:12)=='-ftrace-list=') then
             pm_opts%trace_list=get_num_opt(arg,arg(14:))
          elseif(arg(3:4)=='tn'.and.pm_is_compiling) then
             if(arg=='-ftn-contig') then
                pm_opts%ftn_contig=.true.
             elseif(arg=='-ftn-no-contig') then
                pm_opts%ftn_contig=.false.
             elseif(arg=='-ftn-ptr-arg') then
                pm_opts%ftn_nonptr_arg=.false.
             elseif(arg=='-ftn-nonptr-arg') then
                pm_opts%ftn_nonptr_arg=.true.
             elseif(arg=='-ftn-comment-lines') then
                pm_opts%ftn_comment_lines=.true.
             elseif(arg=='-ftn-comment-ops') then
                pm_opts%ftn_comment_ops=.true.
             elseif(arg(1:14)=='-ftn-max-dims=') then
	        pm_opts%ftn_dims=get_num_opt(arg,arg(15:))
             elseif(arg(1:15)=='-ftn-max-lines=') then
                pm_opts%ftn_lines=get_num_opt(arg,arg(15:))
             elseif(arg(1:21)=='-ftn-max-stack-array=') then
                pm_opts%ftn_max_stack_array=get_num_opt(arg,arg(22:))
             elseif(arg=='-ftn-annotate') then
                pm_opts%ftn_annotate=.true.
             elseif(arg(1:9)=='-ftn-name') then
                if(arg=='-ftn-name-procs') then
                   pm_opts%ftn_name_procs=.true.
                elseif(arg=='-ftn-name-vars') then
                   pm_opts%ftn_name_vars=.true.
                elseif(arg=='-ftn-name-params') then
                   pm_opts%ftn_name_params=.true.
                elseif(arg=='-ftn-name-types') then
                   pm_opts%ftn_name_types=.true.
                elseif(arg=='-ftn-name-elems') then
                   pm_opts%ftn_name_elems=.true.
                elseif(arg=='-ftn-name-all') then
                   pm_opts%ftn_name_procs=.true.
                   pm_opts%ftn_name_vars=.true.
                   pm_opts%ftn_name_params=.true.
                   pm_opts%ftn_name_types=.true.
                   pm_opts%ftn_name_elems=.true.
                elseif(pm_main_process) then
                   write(*,*) 'Not a valid fortran name (-ftn-name) option:',trim(arg)
                   call usage()
                endif
             elseif(arg(1:12)=='-ftn-no-name') then
                if(arg=='-ftn-no-name-procs') then
                   pm_opts%ftn_name_procs=.false.
                elseif(arg=='-ftn-no-name-vars') then
                   pm_opts%ftn_name_vars=.false.
                elseif(arg=='-ftn-no-name-params') then
                   pm_opts%ftn_name_params=.false.
                elseif(arg=='-ftn-no-name-types') then
                   pm_opts%ftn_name_types=.false.
                elseif(arg=='-ftn-no-name-elems') then
                   pm_opts%ftn_name_elems=.false.
                elseif(arg=='-ftn-no-name-all') then
                   pm_opts%ftn_name_procs=.false.
                   pm_opts%ftn_name_vars=.false.
                   pm_opts%ftn_name_params=.false.
                   pm_opts%ftn_name_types=.false.
                   pm_opts%ftn_name_elems=.false.
                elseif(pm_main_process) then
                   write(*,*) 'Not a valid fortran name (-ftn-no-name) option: ',trim(arg)
                   call usage()
                endif
             elseif(pm_main_process) then
                write(*,*) 'Not a valid fortran output (-ftn) option: ',trim(arg)
                call usage()
             endif
          elseif(pm_main_process) then
             write(*,*) 'Not a valid language (-f) option: ',trim(arg)
             call usage()
          endif
       elseif(arg(1:4)=='-opt') then
          if(arg=='-opt-sched') then
             pm_opts%schedule=.true.
          elseif(arg=='-opt-no-sched') then
             pm_opts%schedule=.false.
          elseif(pm_main_process) then
             write(*,*) 'Not a valid optimiser (-opt) option: ',trim(arg)
             call usage()
          endif
       elseif(arg=='--help') then
          call help
       elseif(arg=='--helpD') then
          call help_d
       else
          if(pm_main_process) then
             write(*,*) 'Do not recognise option: ',trim(arg)
             call usage()
          endif
       endif
       i=i+1
    enddo
    call pm_get_cl_arg(i,mname)
    if(mname=='--help') then
       call help
    elseif(mname=='--helpD') then
       call help_d
    elseif(mname=='-i'.and..not.pm_is_compiling) then
       pm_opts%is_repl=.true.
    elseif(mname(1:1)=='-') then
       call usage()
    endif
  contains
    function get_num_opt(opt,str) result(n)
      character(len=*):: opt,str
      integer(pm_ln):: n
      integer:: i
      n=0
      i=1
      do while(i<=len(str))
         select case(str(i:i))
         case('0','1','2','3','4','5','6','7','8','9')
            n=n*10+iachar(str(i:i))-iachar('0')
         case default
            if(pm_main_process) then
               write(*,*) 'Bad numeric option:',trim(opt)
            endif
         end select
      enddo
    end function get_num_opt
         
  end subroutine pm_get_command_line
  
  
end module pm_options
