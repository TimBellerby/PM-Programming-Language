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

  ! ********* Vector Virtual Machine ********
  
  ! Proportion of live elements below which masking
  ! is replaced by indexing
  integer,parameter:: pm_shrink_thresh=4

  ! For differnt operations on given types is a masked
  ! operation (with >1/pm_shrink_thresh live elements)
  ! faster than unmasked one
  
  logical,parameter:: pm_mask_intadd=.false.
  logical,parameter:: pm_mask_intmul=.false.
  logical,parameter:: pm_mask_intdiv=.true.
  logical,parameter:: pm_mask_intpow=.true.
  logical,parameter:: pm_mask_intmod=.true.
  logical,parameter:: pm_mask_intmaxmin=.true.
  logical,parameter:: pm_mask_intcmp=.true.
  logical,parameter:: pm_mask_intabs=.false.
  logical,parameter:: pm_mask_intbits=.false.
  logical,parameter:: pm_mask_intmath=.true.

  logical,parameter:: pm_mask_longadd=.false.
  logical,parameter:: pm_mask_longmul=.false.
  logical,parameter:: pm_mask_longdiv=.true.
  logical,parameter:: pm_mask_longpow=.true.
  logical,parameter:: pm_mask_longmod=.true.
  logical,parameter:: pm_mask_longmaxmin=.true.
  logical,parameter:: pm_mask_longcmp=.true.
  logical,parameter:: pm_mask_longabs=.true.
  logical,parameter:: pm_mask_longbits=.true.
  logical,parameter:: pm_mask_longmath=.true.

  logical,parameter:: pm_mask_realadd=.false.
  logical,parameter:: pm_mask_realmul=.false.
  logical,parameter:: pm_mask_realdiv=.true.
  logical,parameter:: pm_mask_realpow=.true.
  logical,parameter:: pm_mask_realmod=.true.
  logical,parameter:: pm_mask_realmaxmin=.true.
  logical,parameter:: pm_mask_realcmp=.true.
  logical,parameter:: pm_mask_realabs=.false.
  logical,parameter:: pm_mask_realmath=.true.

  logical,parameter:: pm_mask_doubleadd=.false.
  logical,parameter:: pm_mask_doublemul=.false.
  logical,parameter:: pm_mask_doublediv=.true.
  logical,parameter:: pm_mask_doublepow=.true.
  logical,parameter:: pm_mask_doublemod=.true.
  logical,parameter:: pm_mask_doublemaxmin=.true.
  logical,parameter:: pm_mask_doublecmp=.true.
  logical,parameter:: pm_mask_doubleabs=.false.
  logical,parameter:: pm_mask_doublemath=.true.
  logical,parameter:: pm_mask_logical=.true.
  
  
  ! ********* Debugging settings ***********
  ! (for debugging compiler/VM not PM code)
  ! ****************************************
  
  integer,parameter,public:: pm_debug_level=1
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

  ! Long long integers - big enough to address any file
  integer,parameter:: pm_lln=MPI_OFFSET_KIND
   

contains

  function pm_argc() result(n)
    integer:: n
    n=iargc()
  end function pm_argc

  subroutine pm_getarg(n,str)
    integer,intent(in)::n
    character(len=*):: str
    call getarg(n,str)
  end subroutine pm_getarg
  
  subroutine pm_module_filename(buffer)
   character(len=*):: buffer
   integer:: n,m
   n=len_trim(buffer)
   if(n>len(pm_file_suffix)) then
     if(buffer(n-len(pm_file_suffix)+1:n)==pm_file_suffix) return
   endif
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
 end subroutine pm_module_filename
  
end module pm_sysdep
