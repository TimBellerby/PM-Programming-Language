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

module pm_kinds
  
  use pm_sysdep

  !*****************************************************************
  
  ! Double precision 
  integer,parameter:: pm_d=kind(1.0d0)

  ! Types with specific sizes (defaults to best possible)
  integer,parameter:: pm_i8=selected_int_kind(1+pm_fudge_ints)
  integer,parameter:: pm_i16=selected_int_kind(4+pm_fudge_ints)
  integer,parameter:: pm_has_i32=selected_int_kind(9+pm_fudge_ints)
  integer,parameter:: pm_i32=pm_has_i32*(1+sign(1,pm_has_i32))/2 &
       +kind(1)*(1-sign(1,pm_has_i32))/2
  integer,parameter:: pm_has_i64=selected_int_kind(18+pm_fudge_ints)
  integer,parameter:: pm_i64=pm_has_i64*(1+isign(1,pm_has_i64))/2 &
       +pm_i32*(1-isign(1,pm_has_i64))/2
  integer,parameter:: pm_has_i128=selected_int_kind(36+pm_fudge_ints)
  integer,parameter:: pm_i128=pm_has_i128*(1+isign(1,pm_has_i128))/2 & 
       +pm_i64*(1-isign(1,pm_has_i128))/2
  integer,parameter:: pm_r32=selected_real_kind(1)
  integer,parameter:: pm_has_r64=selected_real_kind(15)
  integer,parameter:: pm_r64=pm_has_r64*(1+sign(1,pm_has_r64))/2 &
       +pm_r32*(1-sign(1,pm_has_r64))/2 
  integer,parameter:: pm_has_r128=selected_real_kind(24)
  integer,parameter:: pm_r128=pm_has_r128*(1+sign(1,pm_has_r128))/2 &
       +pm_r64*(1-sign(1,pm_has_r128))/2
  
contains

  subroutine pm_check_kinds
    integer(pm_i8):: eg_i8
    integer(pm_i16):: eg_i16
    integer(pm_i32):: eg_i32
    integer(pm_i64):: eg_i64
    integer(pm_i128):: eg_i128
    if(bit_size(eg_i8)<8) &
         stop 'int8 has wrong size - set pm_fudge_ints=1 in sysdep.f95'
    if(bit_size(eg_i16)<16) &
         stop 'int16 has wrong size - set pm_fudge_ints=1 in sysdep.f95'
    if(bit_size(eg_i32)<32.and.pm_i32/=kind(1)) &
         stop 'int32 has wrong size - set pm_fudge_ints=1 in sysdep.f95'
    if(bit_size(eg_i64)<64.and.pm_i64/=pm_i32) &
         stop 'int64 has wrong size - set pm_fudge_ints=1 in sysdep.f95'
    if(bit_size(eg_i128)<128.and.pm_i128/=pm_i64) &
         stop 'int128 has wrong size - set pm_fudge_ints=1 in sysdep.f95'
  end subroutine pm_check_kinds


end module pm_kinds
