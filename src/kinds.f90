!
!PM (Parallel Models) Programming Language
!
!Released under the MIT License (MIT)
!
!Copyright (c) Tim Bellerby, 2015
!
!Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
!The above copyright notice and this permission notice shall be included in
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

  ! Basic types integer/single real/double/logical/string
  integer,parameter:: pm_d=kind(1.0d0)

  ! Packed logical used in larger arrays - redefine if available
  integer,parameter:: pm_pl=kind(.false.)
  
  ! Types with specific sizes (defaults to best possible)
  integer,parameter:: pm_i8=selected_int_kind(1)
  integer,parameter:: pm_i16=selected_int_kind(4)
  integer,parameter:: pm_has_i32=selected_int_kind(9)
  integer,parameter:: pm_i32=pm_has_i32*(1+sign(1,pm_has_i32))/2 &
       +pm_i16*(1-sign(1,pm_has_i32))/2
  integer,parameter:: pm_has_i64=selected_int_kind(18)
  integer,parameter:: pm_i64=pm_has_i64*(1+isign(1,pm_has_i64))/2 &
       +pm_i32*(1-isign(1,pm_has_i64))/2
  integer,parameter:: pm_has_i128=selected_int_kind(36)
  integer,parameter:: pm_i128=pm_has_i128*(1+isign(1,pm_has_i128))/2 & 
       +pm_i64*(1-isign(1,pm_has_i128))/2
  integer,parameter:: pm_r32=selected_real_kind(1)
  integer,parameter:: pm_has_r64=selected_real_kind(15)
  integer,parameter:: pm_r64=pm_has_r64*(1+sign(1,pm_has_r64))/2 &
       +pm_r32*(1-sign(1,pm_has_r64))/2 
  integer,parameter:: pm_has_r128=selected_real_kind(24)
  integer,parameter:: pm_r128=pm_has_r128*(1+sign(1,pm_has_r128))/2 &
       +pm_r64*(1-sign(1,pm_has_r128))/2
  
  ! Other parts of memory model (block offsets,object sizes,bitmap flags)
  integer,parameter:: pm_p=pm_i32     ! Pointer offsets
  integer,parameter:: pm_ln=pm_i64    ! Long integers
  integer,parameter:: pm_f=pm_i32     ! Flag storage

end module pm_kinds
