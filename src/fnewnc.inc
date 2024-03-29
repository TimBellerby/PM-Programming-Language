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
  function pm_fast_newnc(context,vkind,esize) result(ptr)
    type(pm_context),pointer:: context
    integer(pm_p),intent(in):: vkind
    integer,intent(in):: esize
    type(pm_ptr):: ptr
    type(pm_ptr),pointer:: ptr_p
    integer(pm_p):: next_sweep
    if(pm_debug_level>0) then
       if(esize<=0) call pm_panic('Bad element size')
    endif
    ptr_p=>context%obj_list(esize,vkind)
    ptr=ptr_p
    if(associated(ptr%data)) then
       if(ptr%offset<ptr%data%next_sweep) then 
         ptr_p%offset=ptr%offset+esize
         return
       endif
    endif
    ptr=pm_new_small(context,vkind,int(esize,pm_p))
  end function pm_fast_newnc
