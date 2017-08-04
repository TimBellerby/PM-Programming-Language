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

! Run compiler on each node (for vm target)

module pm_compbase
  use mpi
  use pm_sysdep
  implicit none

  integer,private,pointer:: crash_ptr
  logical:: pm_main_process
  integer:: pm_main_process_no
  logical,parameter:: pm_is_compiling=.false.
  integer:: pm_main_module
  
contains

  subroutine pm_stop(mess)
    character(len=*),intent(in)::mess
    integer:: ierror
    if(pm_main_process) then
       write(*,*) mess
    endif
    call mpi_finalize(ierror)
    stop
  end subroutine pm_stop

  subroutine pm_abort(mess)
    character(len=*),intent(in)::mess
    integer:: ierror
    if(pm_main_process) then
       write(*,*) mess
    endif
    call mpi_abort(MPI_COMM_WORLD,99,ierror)
    stop
  end subroutine pm_abort
  
  subroutine pm_panic(emess)
    character(len=*):: emess
    integer:: ierror
    write(*,*) 'Panic: '//trim(emess)
    if(pm_crash_on_panic) then
       crash_ptr=1
    else
       call mpi_abort(MPI_COMM_WORLD,99,ierror)
    endif
    stop 'System crash!'
  end subroutine pm_panic

  subroutine pm_init_compilation
    integer:: irank,ierror
    call mpi_comm_rank(MPI_COMM_WORLD,irank,ierror)
    pm_main_process=irank==0
    pm_main_process_no=0
  end subroutine pm_init_compilation

  subroutine pm_read_line(iunit,buffer,ios)
    character(len=*) buffer
    integer,intent(in):: iunit
    integer,intent(out):: ios
    integer:: irank,ierror
    if(pm_main_process) then
       read(unit=iunit,fmt='(a1000)',&
            iostat=ios,err=10,end=10) buffer
       if(ios/=0) buffer=pm_eof_char
    endif
20  continue
    call mpi_bcast(buffer,len(buffer),MPI_CHARACTER,0,MPI_COMM_WORLD,ierror)
    ios=0
    if(ierror/=MPI_SUCCESS) ios=-1
    if(buffer(1:1)==pm_eof_char) ios=-1
   return
10 buffer=pm_eof_char
   ios=-1
   goto 20
 end subroutine pm_read_line

 function pm_get_cl_count() result(n)
   integer:: n
   integer:: ierror
   if(pm_main_process) then
      n=pm_argc()
   endif
   call mpi_bcast(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierror)
 end function pm_get_cl_count
   
 subroutine pm_get_cl_arg(i,str)
   character(len=*):: str
   integer,intent(in):: i
   integer:: ierror
   if(pm_main_process) then
      call pm_getarg(i,str)
   endif
   call mpi_bcast(str,len(str),MPI_CHARACTER,0,MPI_COMM_WORLD,ierror)
 end subroutine pm_get_cl_arg

 subroutine pm_open_file(iunit,filename,ok)
   integer,intent(in):: iunit
   character(len=*),intent(in):: filename
   logical,intent(out):: ok
   if(pm_main_process) then
      open(unit=iunit,file=filename,err=10)
   endif
   ok=.true.
   return
10 continue
   ok=.false.
 end subroutine pm_open_file

 subroutine pm_close_file(iunit)
   integer:: iunit
   if(pm_main_process) then
      close(iunit)
   endif
 end subroutine pm_close_file

 function pm_file_exists(filename) result(ok)
   character(len=*):: filename
   logical:: ok
   integer:: ierror
   if(pm_main_process) then
      inquire(file=filename,exist=ok)
   endif
   call mpi_bcast(ok,1,MPI_LOGICAL,0,MPI_COMM_WORLD,ierror)
 end function pm_file_exists
  
end module pm_compbase
