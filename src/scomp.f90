!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2019
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

! Run compiler on each node (for vm target)

module pm_compbase
  use pm_sysdep
  implicit none

  integer,private,pointer:: crash_ptr
  logical,parameter:: pm_main_process=.true.
  logical,parameter:: pm_is_compiling=.true.
  integer,parameter:: pm_wc=kind(1)
  integer,parameter:: pm_main_process_no=0
  integer:: pm_main_module

contains

  subroutine pm_stop(mess)
    character(len=*),intent(in)::mess
    integer:: ierror
    write(*,*)
    write(*,*) mess
    stop
  end subroutine pm_stop

  subroutine pm_abort(mess)
    character(len=*),intent(in)::mess
    integer:: ierror
    write(*,*)
    write(*,*) mess
    stop
  end subroutine pm_abort
  
  subroutine pm_panic(emess)
    character(len=*):: emess
    integer:: ierror
    write(*,*) 'Panic: '//trim(emess)
    if(pm_crash_on_panic) then
       crash_ptr=1
    endif
    stop 'System crash!'
  end subroutine pm_panic

  subroutine pm_init_compilation
    continue
  end subroutine pm_init_compilation

  subroutine pm_read_line(iunit,buffer,ios)
    character(len=*) buffer
    integer,intent(in):: iunit
    integer,intent(out):: ios
    integer:: irank,ierror
    read(unit=iunit,fmt='(a1000)',&
         iostat=ios,err=10,end=10) buffer
    if(ios/=0) buffer=pm_eof_char
    return
10  buffer=pm_eof_char
    ios=-1
 end subroutine pm_read_line

 function pm_get_cl_count() result(n)
   integer:: n
   integer:: ierror
   n=pm_argc()
 end function pm_get_cl_count
   
 subroutine pm_get_cl_arg(i,str)
   character(len=*):: str
   integer,intent(in):: i
   integer:: ierror
   call pm_getarg(i,str)
 end subroutine pm_get_cl_arg

 subroutine pm_open_file(iunit,filename,ok)
   integer,intent(in):: iunit
   character(len=*),intent(in):: filename
   logical,intent(out):: ok
   open(unit=iunit,file=filename,err=10)
   ok=.true.
   return
10 continue
   ok=.false.
 end subroutine pm_open_file

 subroutine pm_close_file(iunit)
   integer:: iunit
   close(iunit)
 end subroutine pm_close_file

 function pm_file_exists(filename) result(ok)
   character(len=*):: filename
   logical:: ok
   integer:: ierror
   inquire(file=filename,exist=ok)
 end function pm_file_exists

end module pm_compbase
