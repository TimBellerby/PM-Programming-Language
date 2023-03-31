!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2021
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
     logical:: see_all_procs
     integer:: proc_list
     logical:: show_variants
     logical:: check_alias
     
     logical:: out_sysmod
     logical:: out_typelist
     logical:: print_timings
     logical:: hide_sysmod

     integer:: ftn_dims
     logical:: ftn_contig
     integer(pm_ln):: ftn_max_stack_array
     logical:: ftn_comment_lines
     logical:: ftn_comment_ops

     character(len=25):: error
     logical:: colour
     
  end type pm_opts_type
  
  type(pm_opts_type):: pm_opts

contains

  subroutine init_opts(context)
    type(pm_context),pointer:: context
    logical:: colour
    pm_opts%inline=.true.
    pm_opts%check_stmts=.not.pm_is_compiling
    pm_opts%show_elems=.false.
    pm_opts%show_members=.false.
    pm_opts%see_all_procs=.false.
    pm_opts%proc_list=11
    pm_opts%show_variants=.false.
    pm_opts%check_alias=.not.pm_is_compiling
    
    pm_opts%out_sysmod=.false.
    pm_opts%out_typelist=.false.
    pm_opts%print_timings=.false.
    pm_opts%hide_sysmod=.true.

    pm_opts%ftn_dims=pm_default_ftn_dims
    pm_opts%ftn_contig=pm_default_ftn_has_contiguous
    pm_opts%ftn_comment_lines=.false.
    pm_opts%ftn_comment_ops=.false.
    colour=pm_colour_messages.and.pm_isatty(6)
    if(colour) then
       pm_opts%error=pm_error_start//'Error: '//pm_error_end
    else
       pm_opts%error='Error: '
    endif
    pm_opts%colour=colour
  end subroutine init_opts

  subroutine print_usage
    if(pm_main_process) then
       write(*,*) 'Usage: pm [-f<opt>] [-D<opt>] [-help] root_module_name_or_filename'
       write(*,*)
       write(*,*) '  -f<opt>        Language options'
       write(*,*) '  --help         Longer help message'
       write(*,*) '  -D<opt>        Output compiler debugging information'
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
       write(*,*) '  included automatically.'
       write(*,*)
       write(*,*) '  LANGUAGE OPTIONS'
       write(*,*) '  -fno-inline      Do not inline any procedures.'
       write(*,*) '  -fno-check       Do not run "check" or "test" statements.'
       write(*,*) '  -fcheck          Run "check" and "test" statements.'
       write(*,*) '  -fshow-elems     Show structure/record elements in error messages.'
       write(*,*) '  -fshow-members   Show members of user defined types in error messages'
       write(*,*) '  -fshow-variants  Show all variants for proc types'
       write(*,*) '  -fsee-all-procs  List all alternative procedures in error messages'
       write(*,*) '  -fproc-list=n    Maximum number of procs to list if see-all-procs not invoked'
       write(*,*) '  -fno-alias-check Do not check for argument aliasing'
       write(*,*) '  -falias_check    Check for argument aliasing'
       write(*,*)
       write(*,*) '  GENERAL OPTIONS'
       write(*,*) '  -N              Do not colour-highlight error messages'
       write(*,*) '  -H              Colour-highlight error messages'
       if(pm_is_compiling) then
          write(*,*)
          write(*,*) '  FORTRAN SOURCE OUTPUT OPTIONS'
          write(*,*) '  -ftn-dims=n    Maximum number (n) of Fortran array dimensions'
          write(*,*) '  -ftn-contig    Fortran supports CONTIGUOUS attribute'
          write(*,*) '  -ftn-no-contig Fortran does not support CONTIGUOUS attribute'
          write(*,*) '  -ftn-max-stack_array=n'
          write(*,*) '                 Maximum size (n) of array that can be stored on the stack'
          write(*,*) '  -ftn-comment-lines'
          write(*,*) '  -ftn-comment-ops'
       endif
       write(*,*)
       write(*,*) '  COMPILER DEBUGGING OPTIONS'
       write(*,*) '  -D              Activate all debugging options listed below.'
       write(*,*) '  -Dfiles         Output files from each compiler stage.'
       write(*,*) '  -Dtimings       Output time taken by each compilation stage.'
       write(*,*) '  -Dsys-mod       Output a listing of the system module.'
       write(*,*) '  -Dtype-list     Output a list of all types used by the system.'
    endif
    call pm_stop('  ')
  end subroutine help
  
  subroutine pm_get_command_line(context,mname,out_debug_files)
    type(pm_context),pointer:: context
    character(len=*),intent(out):: mname
    logical,intent(out):: out_debug_files
    character(len=pm_max_filename_size):: arg
    integer:: i,n
    out_debug_files=.false.
    call init_opts(context)
    n=pm_get_cl_count()
    i=1
    do while(i<n)
       call pm_get_cl_arg(i,arg)
       if(arg(1:1)/='-') then
          if(pm_main_process) then
             write(*,*) 'Not a command line option: ',trim(arg)
             call usage()
          endif
       elseif(arg(1:2)=='-N') then
          pm_opts%error='Error: '
          pm_opts%colour=.false.
       elseif(arg(1:2)=='-H') then
          pm_opts%error=pm_error_start//'Error: '//pm_error_end
          pm_opts%colour=.true.
       elseif(arg(1:2)=='-D') then
          if(arg=='-D') then
             out_debug_files=.true.
             pm_opts%out_sysmod=.true.
             pm_opts%out_typelist=.true.
             pm_opts%print_timings=.true.
             pm_opts%hide_sysmod=.false.
          elseif(arg=='-Dfiles') then
             out_debug_files=.true.
          elseif(arg=='-Dsys-mod') then
             pm_opts%out_sysmod=.true.
          elseif(arg=='-Dtype-list') then
             pm_opts%out_typelist=.true.
          elseif(arg=='-Dtimings') then
             pm_opts%print_timings=.true.
          elseif(arg=='-Dshow-sys-mod') then
             pm_opts%hide_sysmod=.false.
          endif
       elseif(arg(1:2)=='-f') then
          if(arg=='-fno-inline') then
             pm_opts%inline=.false.
          elseif(arg=='-fno-check') then
             pm_opts%check_stmts=.false.
          elseif(arg=='-check') then
             pm_opts%check_stmts=.true.
          elseif(arg=='-fshow-elems') then
             pm_opts%show_elems=.true.
          elseif(arg=='-fshow-members') then
             pm_opts%show_members=.true.
	  elseif(arg=='-fshow-variants') then
             pm_opts%show_variants=.true.
          elseif(arg=='-fsee-all-procs') then
             pm_opts%see_all_procs=.true.
          elseif(arg=='-falias-check') then
             pm_opts%check_alias=.true.
          elseif(arg=='-fno-alias-check') then
             pm_opts%check_alias=.false.
          elseif(arg(1:12)=='-fproc-list=') then
             pm_opts%proc_list=get_num_opt(arg,arg(13:))
          elseif(arg(3:4)=='tn') then
             if(arg=='-ftn-contig') then
                pm_opts%ftn_contig=.true.
             elseif(arg=='-ftn-no-contig') then
                pm_opts%ftn_contig=.false.
             elseif(arg=='-ftn-comment-lines') then
                pm_opts%ftn_comment_lines=.true.
             elseif(arg=='-ftn-comment-ops') then
                pm_opts%ftn_comment_ops=.true.
             elseif(arg(1:14)=='-ftn-max-dims=') then
	        pm_opts%ftn_dims=get_num_opt(arg,arg(15:))
             elseif(arg(1:21)=='-ftn-max-stack-array=') then
                pm_opts%ftn_max_stack_array=get_num_opt(arg,arg(22:))
             endif
          elseif(pm_main_process) then
             write(*,*) 'Not a valid language (-f) option:',trim(arg)
             call usage()
          endif
       elseif(arg=='--help') then
          call help
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
