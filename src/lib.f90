!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2022
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

!=============================================
! General routines needed by system
!=============================================
module pm_lib
  use pm_sysdep
  use pm_kinds
  use pm_memory
  use pm_compbase
  use pm_hash
  use pm_options
  use pm_symbol
  
contains

  !===============================
  ! Error reporting
  !===============================
  
  ! Header for error message including location and source line
  subroutine pm_error_header(context,modl_name,lineno,charno)
   type(pm_context),pointer:: context
   integer,intent(in):: modl_name,lineno,charno
   character(len=2048):: modl,buffer,caret
   character(len=7):: lbuffer,lbuffer2
   integer:: i,n
   type(pm_ptr):: p
   if(pm_main_process) then
      write(*,*)
      if(modl_name==sym_pm_system.and.pm_opts%hide_sysmod) then
         return
      else
         write(lbuffer,'(I7)') lineno
         write(lbuffer2,'(I7)') charno
         if(pm_opts%colour) then
            write(*,'(A,A,A,A,A,A,A)') pm_loc_start,trim(pm_name_as_string(context,&
                 modl_name)),':',&
                 trim(adjustl(lbuffer)),':',adjustl(lbuffer2),pm_loc_end
         else
            write(*,'(A,A,A,A,A)') trim(pm_name_as_string(context,&
                 modl_name)),':',&
                 trim(adjustl(lbuffer)),':',adjustl(lbuffer2)
         endif
      endif
      write(*,*)
      if(pm_get_source_line(context,modl_name,lineno,buffer)) goto 20
      i=1
      n=charno
      if(n==0) n=1
      do while(n>67) 
         i=i+60
         n=n-60
      enddo
      if(i>1) write(*,'(3X,A,"...")') buffer(1:60)
      write(*,'(3x,A67)') buffer(i:)
      caret=' '
      caret(n:n)='!'
      if(pm_opts%colour) then
         write(*,'(3X,A,A67,A)') pm_error_start,caret,pm_error_end
      else
         write(*,'(3X,A67)') caret
      endif
      write(*,*)
      close(3)
 
      20 continue
   endif
 end subroutine pm_error_header

 
 ! Get line from source file to include in error header
 function pm_get_source_line(context,modl_name,lineno,buffer) result(iserr)
   type(pm_context),pointer:: context
   integer,intent(in):: modl_name,lineno
   logical:: iserr
   character(len=*)::  buffer
   integer:: i
   character(len=7):: lbuffer
   !if(.not.pm_main_process) call pm_panic('pm_get_source_line - not main process')
   iserr=.true.
   if(modl_name==sym_pm_system) then
      if(.not.pm_opts%out_sysmod) goto 20
      open(unit=3,file='sysmod.out',status='OLD',err=20)
      do
         read(3,'(I4,A7,A)',err=20,end=20) i,lbuffer,buffer
         if(i==lineno) exit
      enddo
      close(3)
   else
      call pm_module_filename(trim(pm_name_as_string(context,&
           modl_name)),buffer)
      open(unit=3,file=buffer,status='OLD',err=20)
      do i=1,lineno
         read(3,'(A1024)',err=20,end=20) buffer
      enddo
      close(3)
   endif
   iserr=.false.
20 continue
 end function  pm_get_source_line

 ! Print out portion of an error message
 subroutine more_error(context,message)
   type(pm_context),pointer:: context
   character(len=*):: message
   if(.not.pm_main_process) return
    write(*,*) trim(message)
  end subroutine more_error

  ! =================================
  !  Other misc functions...
  ! =================================
  
  ! Returns number as a string
  function pm_number_as_string(context,v,j) result(str)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: v
    integer(pm_ln),intent(in):: j
    character(len=82):: str
    str=''
    select case(pm_fast_vkind(v))
    case(pm_tiny_int)
       write(str,'(i40)') v%offset
    case(pm_int)
       write(str,'(i40)') v%data%i(v%offset+j)
       str=trim(adjustl(str))//'''s'
    case(pm_long)
       write(str,'(i40)') v%data%ln(v%offset+j)
    case(pm_longlong)
       write(str,'(i40)') v%data%lln(v%offset+j)
       str=trim(adjustl(str))//'''l'
    case(pm_int8)
       write(str,'(i40)') v%data%i8(v%offset+j)
       str=trim(adjustl(str))//'''8'
    case(pm_int16)
       write(str,'(i40)') v%data%i16(v%offset+j)
       str=trim(adjustl(str))//'''16'
    case(pm_int32)
       write(str,'(i40)') v%data%i32(v%offset+j)
       str=trim(adjustl(str))//'''32'
    case(pm_int64)
       write(str,'(i40)') v%data%i64(v%offset+j)
       str=trim(adjustl(str))//'''64'
    case(pm_int128)
       write(str,'(i40)') v%data%i128(v%offset+j)
       str=trim(adjustl(str))//'_int128'
    case(pm_single)
       write(str,'(g15.8)') v%data%r(v%offset+j)
       call fix(str)
       str=trim(str)//'''s'
       return
    case(pm_double)
       write(str,'(g25.15)') v%data%d(v%offset+j)
       call fix(str)
       return
    case(pm_real32)
       write(str,'(g15.8)') v%data%r32(v%offset+j)
       call fix(str)
       str=trim(str)//'_real32'
       return
    case(pm_real64)
       write(str,'(g25.15)') v%data%r64(v%offset+j)
       call fix(str)
       str=trim(str)//'_real64'
       return
    case(pm_real128)
       write(str,'(g35.25)') v%data%r128(v%offset+j)
       call fix(str)
       str=trim(str)//'_real128'
       return
    case(pm_single_complex)
       if(real(v%data%c(v%offset+j))/=0.0) then
          write(str,'(g15.8,"+")') real(v%data%c(v%offset+j))
          call fix(str(1:40))
       endif
       write(str(41:),'(g15.8)') aimag(v%data%c(v%offset+j))
       call fix(str(41:))
       str=trim(adjustl(str(1:40)))//trim(adjustl(str(41:80)))//'i''s'
       return
    case(pm_double_complex)
       if(real(v%data%dc(v%offset+j))/=0.0) then
          write(str,'(g25.15,"+")') real(v%data%dc(v%offset+j))
          call fix(str(1:40))
       endif
       write(str(41:),'(g25.15)') aimag(v%data%dc(v%offset+j))
       call fix(str(41:))
       str=trim(adjustl(str(1:40)))//trim(adjustl(str(41:80)))//'i'
       return
    case(pm_complex64)
       if(real(v%data%c64(v%offset+j))/=0.0) then
          write(str,'(g15.8,"+")') real(v%data%c64(v%offset+j))
          call fix(str(1:40))
       endif
       write(str(41:),'(g15.8)') aimag(v%data%c64(v%offset+j))
       call fix(str(41:))
       str=trim(adjustl(str(1:40)))//trim(adjustl(str(41:80)))//'i_cpx64'
       return
    case(pm_complex128)
       if(real(v%data%c128(v%offset+j))/=0.0) then
          write(str,'(g25.15,"+")') real(v%data%c128(v%offset+j))
          call fix(str(1:40))
       endif
       write(str(41:),'(g25.15)') aimag(v%data%c128(v%offset+j))
       call fix(str(41:))
       str=trim(adjustl(str(1:40)))//trim(adjustl(str(41:80)))//'i_cpx128'
       return
    case(pm_complex256)
       if(real(v%data%c256(v%offset+j))/=0.0) then
          write(str,'(g35.25,"+")') real(v%data%c256(v%offset+j))
          call fix(str(1:40))
       endif
       write(str(41:),'(g35.25)') aimag(v%data%c256(v%offset+j))
       call fix(str(41:))
       str=trim(adjustl(str(1:40)))//trim(adjustl(str(41:80)))//'i_cpx256'
       return
    end select
    str=adjustl(str)
  contains
    include 'fvkind.inc'
    subroutine fix(str)
      character(len=*):: str
      integer::n,m
      m=1
      n=len_trim(str)
      do while(str(n:n)=='0'.and.str(n-1:n-1)/='.')
         n=n-1
      enddo
      do while(str(m:m)==' ')
         m=m+1
      enddo
      str=str(m:n)
    end subroutine fix
  end function pm_number_as_string

  ! Left-justified print of integer to a string
  function pm_int_as_string(num) result(str)
    integer,intent(in):: num
    integer,parameter:: field_width=20
    character(len=field_width):: str
    integer:: n,j,k,m,d
    n=num
    m=1
    do while(m<=n)
       m=m*10
    enddo
    j=1
    do while(m>1.and.j<=field_width)
       m=m/10
       d=n/m
       str(j:j)=achar(iachar('0')+d)
       j=j+1
       n=n-d*m
    enddo
    str(j:field_width)=' '
  end function pm_int_as_string

  
end module pm_lib
