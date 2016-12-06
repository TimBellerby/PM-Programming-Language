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


module pm_sysdep
  use mpi
  implicit none

  ! ***************************************************************
  ! ********** These you may want/need to change ******************
  ! ***************************************************************

  ! ********* File settings *****************
  ! Standard output
  integer,parameter:: pm_stdout_unit=6

  ! Unit number used by compiler for file IO 
  integer,parameter:: pm_comp_file_unit=8
 
  ! Suffix for input files
  character(len=4),parameter:: pm_file_suffix='.pmm'

  ! Prefix for library files
  character(len=15),parameter:: pm_file_prefix='/usr/lib/pm/lib'

  ! Directory separator (one character only)
  character(len=1),parameter:: pm_file_dirsep='/' 
  
  ! Maximum size of file name
  integer,parameter:: pm_max_filename_size=4096

  ! Character value used to signal end of file
  character(len=1),parameter:: pm_eof_char=achar(0)


  ! ************ Memory model *********************
  
  ! If PM fails on startup and ask you to 
  ! change 'fudge' then edit below to fudge=1
  integer,parameter:: pm_fudge_ints=0

  ! Packed logical kind used in some larger arrays - redefine if available
  ! to smallest possible logical (typically 1 byte)
  integer,parameter:: pm_pl=kind(.false.)
  
  ! Types used by memory model (block offsets,object sizes,bitmap flags)
  
  integer,parameter:: pm_p=kind(1)    ! Pointer offsets (0..4095)
                                      ! No point in making this int16
                                      ! unless it somehow reduces storage
                                      ! without performance penalty
  
  integer,parameter:: pm_f=kind(1)    ! Bitmap storage  (integer word)
                                      ! On some systems int64 may
                                      ! improve things slightly

  
  ! ********* Debugging settings ***********
  ! (for debugging compiler/VM not PM code)
  ! ****************************************
  
  integer,parameter,public:: pm_debug_level=0
  ! 1= basic checks - slows things down
  ! 2= print short additional info
  ! 3= print substantial additional info
  ! 4= print & check everything
  
  ! Crash system if panic is called
  logical,parameter:: pm_crash_on_panic=.true.

  ! ******************************************
  ! ********* Stuff not to change  ***********
  ! ******************************************
  
  ! Long integers - big enough to address any array
  integer,parameter:: pm_ln=MPI_ADDRESS_KIND

  integer,private,pointer:: crash_ptr 
  logical:: pm_main_process
  
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
    !write(*,*) 'Rank:',irank,'gets (',trim(buffer),')'
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
      n=iargc()
   endif
   call mpi_bcast(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierror)
 end function pm_get_cl_count
   
 subroutine pm_get_cl_arg(i,str)
   character(len=*):: str
   integer,intent(in):: i
   integer:: ierror
   if(pm_main_process) then
      call getarg(i,str)
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
 
 subroutine pm_change_filename(buffer)
   character(len=*):: buffer
   integer:: n,m
   n=len_trim(buffer)
   if(buffer(n-len(pm_file_suffix)+1:n)==pm_file_suffix) return
   if(buffer(1:4)=='lib.') then
      m=len(pm_file_prefix)
      buffer(m+1:m+n)=buffer(1:n)
      buffer(1:m)=pm_file_prefix
   endif
   do m=1,n
      if(buffer(m:m)=='.') then
         buffer(m:m)=pm_file_dirsep
      endif
   enddo
   buffer(n+1:n+len(pm_file_suffix))=pm_file_suffix
 end subroutine pm_change_filename
  
end module pm_sysdep
