!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2023
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

! System module code (intrinsic types and procedures)
! Note: this is *not* written in canonical PM
! some rules are suspended and use is made of internal-only constructs

module pm_sysdefs
  use pm_kinds
  use pm_memory
  use pm_parser
  use pm_hash
  use pm_lib
  use pm_symbol
  use pm_types
  use pm_vmdefs
  implicit none

  ! Flag values for procs
  ! (note values 1=proc_is_comm,...  defined in parser)
  integer,parameter:: proc_is_thru_each =     2**12
  integer,parameter:: proc_is_empty_each =    2**13
  integer,parameter:: proc_is_dup_each =      2**14
  integer,parameter:: proc_is_var =           2**15
  integer,parameter:: proc_is_generator =     2**16
  integer,parameter:: proc_needs_type =       2**17
  integer,parameter:: proc_is_recursive =     2**18
  integer,parameter:: proc_unfinished =       2**19
  integer,parameter:: proc_is_impure =        2**20
  integer,parameter:: proc_is_not_inlinable = 2**21
  integer,parameter:: proc_has_for =          2**22
  integer,parameter:: proc_is_not_pure_each = 2**23
  integer,parameter:: proc_has_vkeys =        2**24
  integer,parameter:: proc_is_dcomm =         2**25
  integer,parameter:: proc_is_variant =       2**26
  integer,parameter:: proc_needs_par =        2**27

  integer,parameter:: proc_taints = proc_is_impure &
       + proc_is_not_inlinable + proc_has_for      &
       + proc_is_not_pure_each + proc_is_variant   &
       + proc_needs_par

contains

  subroutine sysdefs(parser)
    type(parse_state):: parser
    integer:: line
    line=1

    call dcl_module(parser,'PM__system')
    parser%sysmodl=parser%modl

    ! **************************************
    !  BASIC TYPES
    ! **************************************
    
    ! String type
    call dcl_proc(parser,'print(string)',op_print,0,line,proc_is_impure)
    call dcl_uproc(parser,'print(x) { print(string(x)) }',line)
    call dcl_proc(parser,'print_all(string)',op_print,1,line,proc_is_impure)
    call dcl_uproc(parser,'print_all(x) { print_all(string(x)) }',line)
    call dcl_proc(parser,'++(string,string)->string',op_concat,0,line,0)
    call dcl_uproc(parser,'++(x:string,y)=$++.(x,string(y))',line)
    call dcl_uproc(parser,'++(x,y)=$++.(string(x),string(y))',line)
    call dcl_uproc(parser,'string(x:string)=x',line)
    call dcl_uproc(parser,'string(x:null)="null"',line)
    
    ! sint type
    call dcl_proc(parser,'PM__assign_var(&sint,sint)',&
         op_assign_i,0,line,0)
    call dcl_proc(parser,'mod(sint,sint)->sint',op_mod_i,0,line,0)
    call dcl_proc(parser,'==(sint,sint)->bool',op_eq_i,0,line,0)
    call dcl_proc(parser,'/=(sint,sint)->bool',op_ne_i,0,line,0)
    call dcl_proc(parser,'>=(sint,sint)->bool',op_ge_i,0,line,0)
    call dcl_proc(parser,'>(sint,sint)->bool',op_gt_i,0,line,0)
    call dcl_proc(parser,'+(sint,sint)->sint',op_add_i,0,line,0)
    call dcl_proc(parser,'-(sint,sint)->sint',op_sub_i,0,line,0)
    call dcl_proc(parser,'*(sint,sint)->sint',op_mult_i,0,line,0)
    call dcl_proc(parser,'/(sint,sint)->sint',op_divide_i,0,line,0)
    call dcl_proc(parser,'**(sint,sint)->sint',op_pow_i,0,line,0)
    call dcl_proc(parser,'max(sint,sint)->sint',op_max_i,0,line,0)
    call dcl_proc(parser,'min(sint,sint)->sint',op_min_i,0,line,0)
    call dcl_proc(parser,'-(sint)->sint',op_uminus_i,0,line,0)
    call dcl_proc(parser,'string(sint)->string',op_string_i,0,line,0)
    call dcl_proc(parser,'int(sint)->int',op_long_i,0,line,0)
    call dcl_proc(parser,'sreal(sint)->sreal',op_real_i,0,line,0)
    call dcl_proc(parser,'real(sint)->real',op_double_i,0,line,0)
    call dcl_uproc(parser,'sint(x:sint)=x',line)
    call dcl_proc(parser,'abs(sint)->sint',op_abs_i,0,line,0)
    call dcl_proc(parser,'bit_not(sint)->sint',op_bnot_i,0,line,0)
    call dcl_proc(parser,'&(sint,sint)->sint',op_band_i,0,line,0)
    call dcl_proc(parser,'|(sint,sint)->sint',op_bor_i,0,line,0)
    call dcl_proc(parser,'xor(sint,sint)->sint',op_bxor_i,0,line,0)
    call dcl_proc(parser,'shift(sint,sint)->sint',op_bshift_i,0,line,0)
    call dcl_proc(parser,'pdiff(sint,sint)->sint',op_pdiff_i,0,line,0)
    call dcl_proc(parser,'sign(sint,sint)->sint',op_sign_i,0,line,0)
    call dcl_proc(parser,'rem(sint,sint)->sint',op_modulo_i,0,line,0)
    call dcl_proc(parser,'int8(sint)->int8',op_i8_i,0,line,0)
    call dcl_proc(parser,'int16(sint)->int16',op_i16_i,0,line,0)
    call dcl_proc(parser,'int32(sint)->int32',op_i32_i,0,line,0)
    call dcl_proc(parser,'int64(sint)->int64',op_i64_i,0,line,0)
    call dcl_proc(parser,'lint(sint)->lint',op_offset_i,0,line,0)
    
    ! int type
    call dcl_proc(parser,'PM__assign_var(&int,int)',&
         op_assign_ln,0,line,0)
    call dcl_proc(parser,'mod(int,int)->''int',op_mod_ln,0,line,0)
    call dcl_proc(parser,'==(int,int)->''bool',op_eq_ln,0,line,0)
    call dcl_proc(parser,'/=(int,int)->''bool',op_ne_ln,0,line,0)
    call dcl_proc(parser,'>=(int,int)->''bool',op_ge_ln,0,line,0)
    call dcl_proc(parser,'>(int,int)->''bool',op_gt_ln,0,line,0)
    call dcl_proc(parser,'+(int,int)->''int',op_add_ln,0,line,0)
    call dcl_uproc(parser,'+(x:int,y:''0)=x',line)
    call dcl_uproc(parser,'+(x:''0,y:int)=y',line)
    call dcl_proc(parser,'-(int,int)->''int',op_sub_ln,0,line,0)
    call dcl_uproc(parser,'-(x:int,y:''0)=x',line)
    call dcl_proc(parser,'*(int,int)->''int',op_mult_ln,0,line,0)
    call dcl_uproc(parser,'*(x:int,y:''1)=x',line)
    call dcl_uproc(parser,'*(x:''1,y:int)=y',line)
    call dcl_proc(parser,'/(int,int)->''int',op_divide_ln,0,line,0)
    call dcl_uproc(parser,'/(x:int,y:''1)=x',line)
    call dcl_proc(parser,'**(int,int)->''int',op_pow_ln,0,line,0)
    call dcl_uproc(parser,'**(x:int,y:''0)=1',line)
    call dcl_uproc(parser,'**(x:int,y:''1)=x',line)
    call dcl_uproc(parser,'**(x:int,y:''2)=x*x',line)
    call dcl_proc(parser,'max(int,int)->''int',op_max_ln,0,line,0)
    call dcl_proc(parser,'min(int,int)->''int',op_min_ln,0,line,0)
    call dcl_proc(parser,'-(int)->''int',op_uminus_ln,0,line,0)
    call dcl_proc(parser,'string(int)->string',op_string_ln,0,line,0)
    call dcl_proc(parser,'sint(int)->sint',op_int_ln,0,line,0)
    call dcl_proc(parser,'sreal(int)->sreal',op_real_ln,0,line,0)
    call dcl_proc(parser,'real(int)->real',op_double_ln,0,line,0)
    call dcl_uproc(parser,'int(x:int)=x',line)
    call dcl_proc(parser,'abs(int)->int',op_abs_ln,0,line,0)
    call dcl_proc(parser,'!(int)->int',op_bnot_ln,0,line,0)
    call dcl_proc(parser,'&(int,int)->int',op_band_ln,0,line,0)
    call dcl_proc(parser,'|(int,int)->int',op_bor_ln,0,line,0)
    call dcl_proc(parser,'xor(int,int)->int',op_bxor_ln,0,line,0)
    call dcl_proc(parser,'shift(int,int)->int',&
         op_bshift_ln,0,line,0)
    call dcl_proc(parser,'pdiff(int,int)->''int',op_pdiff_ln,0,line,0)
    call dcl_proc(parser,'sign(int,int)->''int',op_sign_ln,0,line,0)
    call dcl_proc(parser,'rem(int,int)->''int',op_modulo_ln,0,line,0)
    call dcl_proc(parser,'int8(int)->int8',op_i8_ln,0,line,0)
    call dcl_proc(parser,'int16(int)->int16',op_i16_ln,0,line,0)
    call dcl_proc(parser,'int32(int)->int32',op_i32_ln,0,line,0)
    call dcl_proc(parser,'int64(int)->int64',op_i64_ln,0,line,0)
    call dcl_proc(parser,'lint(int)->lint',op_offset_ln,0,line,0)

    ! lint type
    call dcl_proc(parser,'PM__assign_var(&lint,lint)',&
         op_assign_offset,0,line,0)
    call dcl_proc(parser,'mod(lint,lint)->lint',op_mod_offset,0,line,0)
    call dcl_proc(parser,'==(lint,lint)->bool',op_eq_offset,0,line,0)
    call dcl_proc(parser,'/=(lint,lint)->bool',op_ne_offset,0,line,0)
    call dcl_proc(parser,'>=(lint,lint)->bool',op_ge_offset,0,line,0)
    call dcl_proc(parser,'>(lint,lint)->bool',op_gt_offset,0,line,0)
    call dcl_proc(parser,'+(lint,lint)->lint',op_add_offset,0,line,0)
    call dcl_uproc(parser,'+(x:lint,y:''0)=x',line)
    call dcl_uproc(parser,'+(x:''0,y:lint)=y',line)
    call dcl_proc(parser,'-(lint,lint)->lint',op_sub_offset,0,line,0)
    call dcl_uproc(parser,'-(x:lint,y:''0)=x',line)
    call dcl_proc(parser,'*(lint,lint)->lint',op_mult_offset,0,line,0)
    call dcl_uproc(parser,'*(x:lint,y:''1)=x',line)
    call dcl_uproc(parser,'*(x:''1,y:lint)=y',line)
    call dcl_proc(parser,'/(lint,lint)->lint',op_divide_offset,0,line,0)
    call dcl_uproc(parser,'/(x:lint,y:''1)=x',line)
    call dcl_proc(parser,'**(lint,lint)->lint',op_pow_offset,0,line,0)
    call dcl_uproc(parser,'**(x:lint,y:''0)=1',line)
    call dcl_uproc(parser,'**(x:lint,y:''1)=x',line)
    call dcl_uproc(parser,'**(x:lint,y:''2)=x*x',line)
    call dcl_proc(parser,'max(lint,lint)->lint',op_max_offset,0,line,0)
    call dcl_proc(parser,'min(lint,lint)->lint',op_min_offset,0,line,0)
    call dcl_proc(parser,'-(lint)->lint',op_uminus_offset,0,line,0)
    call dcl_proc(parser,'string(lint)->string',op_string_offset,0,line,0)
    call dcl_proc(parser,'sint(lint)->sint',op_int_offset,0,line,0)
    call dcl_proc(parser,'sreal(lint)->sreal',op_real_offset,0,line,0)
    call dcl_proc(parser,'real(lint)->real',op_double_offset,0,line,0)
    call dcl_uproc(parser,'lint(x:lint)=x',line)
    call dcl_proc(parser,'abs(lint)->lint',op_abs_offset,0,line,0)
    call dcl_proc(parser,'!(lint)->lint',op_bnot_offset,0,line,0)
    call dcl_proc(parser,'&(lint,lint)->lint',op_band_offset,0,line,0)
    call dcl_proc(parser,'|(lint,lint)->lint',op_bor_offset,0,line,0)
    call dcl_proc(parser,'xor(lint,lint)->lint',op_bxor_offset,0,line,0)
    call dcl_proc(parser,'shift(lint,lint)->lint',&
         op_bshift_offset,0,line,0)
    call dcl_proc(parser,'pdiff(lint,lint)->lint',op_pdiff_offset,0,line,0)
    call dcl_proc(parser,'sign(lint,lint)->lint',op_sign_offset,0,line,0)
    call dcl_proc(parser,'rem(lint,lint)->lint',op_modulo_offset,0,line,0)
    call dcl_proc(parser,'int8(lint)->int8',op_i8_offset,0,line,0)
    call dcl_proc(parser,'int16(lint)->int16',op_i16_offset,0,line,0)
    call dcl_proc(parser,'int32(lint)->int32',op_i32_offset,0,line,0)
    call dcl_proc(parser,'int64(lint)->int64',op_i64_offset,0,line,0)
    call dcl_proc(parser,'int(lint)->int',op_long_offset,0,line,0)

    ! int8 type
    call dcl_proc(parser,'PM__assign_var(&int8,int8)',&
         op_assign_i8,0,line,0)
    call dcl_proc(parser,'mod(int8,int8)->int8',op_mod_i8,0,line,0)
    call dcl_proc(parser,'==(int8,int8)->bool',op_eq_i8,0,line,0)
    call dcl_proc(parser,'/=(int8,int8)->bool',op_ne_i8,0,line,0)
    call dcl_proc(parser,'>=(int8,int8)->bool',op_ge_i8,0,line,0)
    call dcl_proc(parser,'>(int8,int8)->bool',op_gt_i8,0,line,0)
    call dcl_proc(parser,'+(int8,int8)->int8',op_add_i8,0,line,0)
    call dcl_uproc(parser,'+(x:int8,y:''0)=x',line)
    call dcl_uproc(parser,'+(x:''0,y:int8)=y',line)
    call dcl_proc(parser,'-(int8,int8)->int8',op_sub_i8,0,line,0)
    call dcl_uproc(parser,'-(x:int8,y:''0)=x',line)
    call dcl_proc(parser,'*(int8,int8)->int8',op_mult_i8,0,line,0)
    call dcl_uproc(parser,'*(x:int8,y:''1)=x',line)
    call dcl_uproc(parser,'*(x:''1,y:int8)=y',line)
    call dcl_proc(parser,'/(int8,int8)->int8',op_divide_i8,0,line,0)
    call dcl_uproc(parser,'/(x:int8,y:''1)=x',line)
    call dcl_proc(parser,'**(int8,int8)->int8',op_pow_i8,0,line,0)
    call dcl_uproc(parser,'**(x:int8,y:''0)=1',line)
    call dcl_uproc(parser,'**(x:int8,y:''1)=x',line)
    call dcl_uproc(parser,'**(x:int8,y:''2)=x*x',line)
    call dcl_proc(parser,'max(int8,int8)->int8',op_max_i8,0,line,0)
    call dcl_proc(parser,'min(int8,int8)->int8',op_min_i8,0,line,0)
    call dcl_proc(parser,'-(int8)->int8',op_uminus_i8,0,line,0)
    call dcl_proc(parser,'sint(int8)->sint',op_int_i8,0,line,0)
    call dcl_proc(parser,'sreal(int8)->sreal',op_real_i8,0,line,0)
    call dcl_proc(parser,'real(int8)->real',op_double_i8,0,line,0)
    call dcl_uproc(parser,'int8(x:int8)=x',line)
    call dcl_proc(parser,'abs(int8)->int8',op_abs_i8,0,line,0)
    call dcl_proc(parser,'!(int8)->int8',op_bnot_i8,0,line,0)
    call dcl_proc(parser,'&(int8,int8)->int8',op_band_i8,0,line,0)
    call dcl_proc(parser,'|(int8,int8)->int8',op_bor_i8,0,line,0)
    call dcl_proc(parser,'xor(int8,int8)->int8',op_bxor_i8,0,line,0)
    call dcl_proc(parser,'shift(int8,int8)->int8',&
         op_bshift_i8,0,line,0)
    call dcl_proc(parser,'pdiff(int8,int8)->int8',op_pdiff_i8,0,line,0)
    call dcl_proc(parser,'sign(int8,int8)->int8',op_sign_i8,0,line,0)
    call dcl_proc(parser,'rem(int8,int8)->int8',op_modulo_i8,0,line,0)
    call dcl_proc(parser,'int16(int8)->int16',op_i16_i8,0,line,0)
    call dcl_proc(parser,'int32(int8)->int32',op_i32_i8,0,line,0)
    call dcl_proc(parser,'int64(int8)->int64',op_i64_i8,0,line,0)
    call dcl_proc(parser,'int(int8)->int',op_long_i8,0,line,0)
    call dcl_proc(parser,'lint(int8)->lint',op_offset_i8,0,line,0)

    ! int16 type
    call dcl_proc(parser,'PM__assign_var(&int16,int16)',&
         op_assign_i16,0,line,0)
    call dcl_proc(parser,'mod(int16,int16)->int16',op_mod_i16,0,line,0)
    call dcl_proc(parser,'==(int16,int16)->bool',op_eq_i16,0,line,0)
    call dcl_proc(parser,'/=(int16,int16)->bool',op_ne_i16,0,line,0)
    call dcl_proc(parser,'>=(int16,int16)->bool',op_ge_i16,0,line,0)
    call dcl_proc(parser,'>(int16,int16)->bool',op_gt_i16,0,line,0)
    call dcl_proc(parser,'+(int16,int16)->int16',op_add_i16,0,line,0)
    call dcl_uproc(parser,'+(x:int16,y:''0)=x',line)
    call dcl_uproc(parser,'+(x:''0,y:int16)=y',line)
    call dcl_proc(parser,'-(int16,int16)->int16',op_sub_i16,0,line,0)
    call dcl_uproc(parser,'-(x:int16,y:''0)=x',line)
    call dcl_proc(parser,'*(int16,int16)->int16',op_mult_i16,0,line,0)
    call dcl_uproc(parser,'*(x:int16,y:''1)=x',line)
    call dcl_uproc(parser,'*(x:''1,y:int16)=y',line)
    call dcl_proc(parser,'/(int16,int16)->int16',op_divide_i16,0,line,0)
    call dcl_uproc(parser,'/(x:int16,y:''1)=x',line)
    call dcl_proc(parser,'**(int16,int16)->int16',op_pow_i16,0,line,0)
    call dcl_uproc(parser,'**(x:int16,y:''0)=1',line)
    call dcl_uproc(parser,'**(x:int16,y:''1)=x',line)
    call dcl_uproc(parser,'**(x:int16,y:''2)=x*x',line)
    call dcl_proc(parser,'max(int16,int16)->int16',op_max_i16,0,line,0)
    call dcl_proc(parser,'min(int16,int16)->int16',op_min_i16,0,line,0)
    call dcl_proc(parser,'-(int16)->int16',op_uminus_i16,0,line,0)
    call dcl_proc(parser,'sint(int16)->sint',op_int_i16,0,line,0)
    call dcl_proc(parser,'sreal(int16)->sreal',op_real_i16,0,line,0)
    call dcl_proc(parser,'real(int16)->real',op_double_i16,0,line,0)
    call dcl_uproc(parser,'int16(x:int16)=x',line)
    call dcl_proc(parser,'abs(int16)->int16',op_abs_i16,0,line,0)
    call dcl_proc(parser,'!(int16)->int16',op_bnot_i16,0,line,0)
    call dcl_proc(parser,'&(int16,int16)->int16',op_band_i16,0,line,0)
    call dcl_proc(parser,'|(int16,int16)->int16',op_bor_i16,0,line,0)
    call dcl_proc(parser,'xor(int16,int16)->int16',op_bxor_i16,0,line,0)
    call dcl_proc(parser,'shift(int16,int16)->int16',&
         op_bshift_i16,0,line,0)
    call dcl_proc(parser,'pdiff(int16,int16)->int16',op_pdiff_i16,0,line,0)
    call dcl_proc(parser,'sign(int16,int16)->int16',op_sign_i16,0,line,0)
    call dcl_proc(parser,'rem(int16,int16)->int16',op_modulo_i16,0,line,0)
    call dcl_proc(parser,'int8(int16)->int16',op_i8_i16,0,line,0)
    call dcl_proc(parser,'int32(int16)->int32',op_i32_i16,0,line,0)
    call dcl_proc(parser,'int64(int16)->int64',op_i64_i16,0,line,0)
    call dcl_proc(parser,'int(int16)->int',op_long_i16,0,line,0)
    call dcl_proc(parser,'lint(int16)->lint',op_offset_i16,0,line,0)

    ! int32 type
    call dcl_proc(parser,'PM__assign_var(&int32,int32)',&
         op_assign_i32,0,line,0)
    call dcl_proc(parser,'mod(int32,int32)->int32',op_mod_i32,0,line,0)
    call dcl_proc(parser,'==(int32,int32)->bool',op_eq_i32,0,line,0)
    call dcl_proc(parser,'/=(int32,int32)->bool',op_ne_i32,0,line,0)
    call dcl_proc(parser,'>=(int32,int32)->bool',op_ge_i32,0,line,0)
    call dcl_proc(parser,'>(int32,int32)->bool',op_gt_i32,0,line,0)
    call dcl_proc(parser,'+(int32,int32)->int32',op_add_i32,0,line,0)
    call dcl_uproc(parser,'+(x:int32,y:''0)=x',line)
    call dcl_uproc(parser,'+(x:''0,y:int32)=y',line)
    call dcl_proc(parser,'-(int32,int32)->int32',op_sub_i32,0,line,0)
    call dcl_uproc(parser,'-(x:int32,y:''0)=x',line)
    call dcl_proc(parser,'*(int32,int32)->int32',op_mult_i32,0,line,0)
    call dcl_uproc(parser,'*(x:int32,y:''1)=x',line)
    call dcl_uproc(parser,'*(x:''1,y:int32)=y',line)
    call dcl_proc(parser,'/(int32,int32)->int32',op_divide_i32,0,line,0)
    call dcl_uproc(parser,'/(x:int32,y:''1)=x',line)
    call dcl_proc(parser,'**(int32,int32)->int32',op_pow_i32,0,line,0)
    call dcl_uproc(parser,'**(x:int32,y:''0)=1',line)
    call dcl_uproc(parser,'**(x:int32,y:''1)=x',line)
    call dcl_uproc(parser,'**(x:int32,y:''2)=x*x',line)
    call dcl_proc(parser,'max(int32,int32)->int32',op_max_i32,0,line,0)
    call dcl_proc(parser,'min(int32,int32)->int32',op_min_i32,0,line,0)
    call dcl_proc(parser,'-(int32)->int32',op_uminus_i32,0,line,0)
    call dcl_proc(parser,'sint(int32)->sint',op_int_i32,0,line,0)
    call dcl_proc(parser,'sreal(int32)->sreal',op_real_i32,0,line,0)
    call dcl_proc(parser,'real(int32)->real',op_double_i32,0,line,0)
    call dcl_uproc(parser,'int32(x:int32)=x',line)
    call dcl_proc(parser,'abs(int32)->int32',op_abs_i32,0,line,0)
    call dcl_proc(parser,'!(int32)->int32',op_bnot_i32,0,line,0)
    call dcl_proc(parser,'&(int32,int32)->int32',op_band_i32,0,line,0)
    call dcl_proc(parser,'|(int32,int32)->int32',op_bor_i32,0,line,0)
    call dcl_proc(parser,'xor(int32,int32)->int32',op_bxor_i32,0,line,0)
    call dcl_proc(parser,'shift(int32,int32)->int32',&
         op_bshift_i32,0,line,0)
    call dcl_proc(parser,'pdiff(int32,int32)->int32',op_pdiff_i32,0,line,0)
    call dcl_proc(parser,'sign(int32,int32)->int32',op_sign_i32,0,line,0)
    call dcl_proc(parser,'rem(int32,int32)->int32',op_modulo_i32,0,line,0)
    call dcl_proc(parser,'int8(int32)->int32',op_i8_i32,0,line,0)
    call dcl_proc(parser,'int16(int32)->int32',op_i16_i32,0,line,0)
    call dcl_proc(parser,'int64(int32)->int64',op_i64_i32,0,line,0)
    call dcl_proc(parser,'int(int32)->int',op_long_i32,0,line,0)
    call dcl_proc(parser,'lint(int32)->lint',op_offset_i32,0,line,0)

    ! int64 type
    call dcl_proc(parser,'PM__assign_var(&int64,int64)',&
         op_assign_i64,0,line,0)
    call dcl_proc(parser,'mod(int64,int64)->int64',op_mod_i64,0,line,0)
    call dcl_proc(parser,'==(int64,int64)->bool',op_eq_i64,0,line,0)
    call dcl_proc(parser,'/=(int64,int64)->bool',op_ne_i64,0,line,0)
    call dcl_proc(parser,'>=(int64,int64)->bool',op_ge_i64,0,line,0)
    call dcl_proc(parser,'>(int64,int64)->bool',op_gt_i64,0,line,0)
    call dcl_proc(parser,'+(int64,int64)->int64',op_add_i64,0,line,0)
    call dcl_uproc(parser,'+(x:int64,y:''0)=x',line)
    call dcl_uproc(parser,'+(x:''0,y:int64)=y',line)
    call dcl_proc(parser,'-(int64,int64)->int64',op_sub_i64,0,line,0)
    call dcl_uproc(parser,'-(x:int64,y:''0)=x',line)
    call dcl_proc(parser,'*(int64,int64)->int64',op_mult_i64,0,line,0)
    call dcl_uproc(parser,'*(x:int64,y:''1)=x',line)
    call dcl_uproc(parser,'*(x:''1,y:int64)=y',line)
    call dcl_proc(parser,'/(int64,int64)->int64',op_divide_i64,0,line,0)
    call dcl_uproc(parser,'/(x:int64,y:''1)=x',line)
    call dcl_proc(parser,'**(int64,int64)->int64',op_pow_i64,0,line,0)
    call dcl_uproc(parser,'**(x:int64,y:''0)=1',line)
    call dcl_uproc(parser,'**(x:int64,y:''1)=x',line)
    call dcl_uproc(parser,'**(x:int64,y:''2)=x*x',line)
    call dcl_proc(parser,'max(int64,int64)->int64',op_max_i64,0,line,0)
    call dcl_proc(parser,'min(int64,int64)->int64',op_min_i64,0,line,0)
    call dcl_proc(parser,'-(int64)->int64',op_uminus_i64,0,line,0)
    call dcl_proc(parser,'string(int64)->string',op_string_i64,0,line,0)
    call dcl_proc(parser,'sint(int64)->sint',op_int_i64,0,line,0)
    call dcl_proc(parser,'sreal(int64)->sreal',op_real_i64,0,line,0)
    call dcl_proc(parser,'real(int64)->real',op_double_i64,0,line,0)
    call dcl_uproc(parser,'int64(x:int64)=x',line)
    call dcl_proc(parser,'abs(int64)->int64',op_abs_i64,0,line,0)
    call dcl_proc(parser,'!(int64)->int64',op_bnot_i64,0,line,0)
    call dcl_proc(parser,'&(int64,int64)->int64',op_band_i64,0,line,0)
    call dcl_proc(parser,'|(int64,int64)->int64',op_bor_i64,0,line,0)
    call dcl_proc(parser,'xor(int64,int64)->int64',op_bxor_i64,0,line,0)
    call dcl_proc(parser,'shift(int64,int64)->int64',&
         op_bshift_i64,0,line,0)
    call dcl_proc(parser,'pdiff(int64,int64)->int64',op_pdiff_i64,0,line,0)
    call dcl_proc(parser,'sign(int64,int64)->int64',op_sign_i64,0,line,0)
    call dcl_proc(parser,'rem(int64,int64)->int64',op_modulo_i64,0,line,0)
    call dcl_proc(parser,'int8(int64)->int64',op_i8_i64,0,line,0)
    call dcl_proc(parser,'int16(int64)->int64',op_i16_i64,0,line,0)
    call dcl_proc(parser,'int32(int64)->int64',op_i32_i64,0,line,0)
    call dcl_proc(parser,'int(int64)->int',op_long_i64,0,line,0)
    call dcl_proc(parser,'lint(int64)->lint',op_offset_i64,0,line,0)
    
    ! sreal type
    call dcl_proc(parser,'PM__assign_var(&sreal,sreal)',&
         op_assign_r,0,line,0)
    call dcl_proc(parser,'mod(sreal,sreal)->sreal',op_mod_r,0,line,0)
    call dcl_proc(parser,'==(sreal,sreal)->bool',op_eq_r,0,line,0)
    call dcl_proc(parser,'/=(sreal,sreal)->bool',op_ne_r,0,line,0)
    call dcl_proc(parser,'>=(sreal,sreal)->bool',op_ge_r,0,line,0)
    call dcl_proc(parser,'>(sreal,sreal)->bool',op_gt_r,0,line,0)
    call dcl_proc(parser,'+(sreal,sreal)->sreal',op_add_r,0,line,0)
    call dcl_proc(parser,'-(sreal,sreal)->sreal',op_sub_r,0,line,0)
    call dcl_proc(parser,'*(sreal,sreal)->sreal',op_mult_r,0,line,0)
    call dcl_proc(parser,'/(sreal,sreal)->sreal',op_divide_r,0,line,0)
    call dcl_proc(parser,'**(sreal,sreal)->sreal',op_pow_r,0,line,0)
    call dcl_proc(parser,'max(sreal,sreal)->sreal',op_max_r,0,line,0)
    call dcl_proc(parser,'min(sreal,sreal)->sreal',op_min_r,0,line,0)
    call dcl_proc(parser,'-(sreal)->sreal',op_uminus_r,0,line,0)
    call dcl_proc(parser,'string(sreal)->string',op_string_r,0,line,0)
    call dcl_proc(parser,'strunc(sreal)->sint',op_int_r,0,line,0)
    call dcl_proc(parser,'trunc(sreal)->int',op_long_r,0,line,0)
    call dcl_proc(parser,'ltrunc(sreal)->lint',op_offset_r,0,line,0)
    call dcl_proc(parser,'real(sreal)->real',op_double_r,0,line,0)
    call dcl_uproc(parser,'sreal(x:sreal)=x',line)
    call dcl_proc(parser,'abs(sreal)->sreal',op_abs_r,0,line,0)
    call dcl_proc(parser,'acos(sreal)->sreal',op_acos_r,0,line,0)
    call dcl_proc(parser,'asin(sreal)->sreal',op_asin_r,0,line,0)
    call dcl_proc(parser,'atan(sreal)->sreal',op_atan_r,0,line,0)
    call dcl_proc(parser,'atan2(sreal,sreal)->sreal',op_atan2_r,0,line,0)
    call dcl_proc(parser,'cos(sreal)->sreal',op_cos_r,0,line,0)
    call dcl_proc(parser,'cosh(sreal)->sreal',op_cosh_r,0,line,0)
    call dcl_proc(parser,'exp(sreal)->sreal',op_exp_r,0,line,0)
    call dcl_proc(parser,'log(sreal)->sreal',op_log_r,0,line,0)
    call dcl_proc(parser,'log10(sreal)->sreal',op_log10_r,0,line,0)
    call dcl_proc(parser,'sin(sreal)->sreal',op_sin_r,0,line,0)
    call dcl_proc(parser,'sinh(sreal)->sreal',op_sinh_r,0,line,0)
    call dcl_proc(parser,'sqrt(sreal)->sreal',op_sqrt_r,0,line,0)
    call dcl_proc(parser,'tan(sreal)->sreal',op_tan_r,0,line,0)
    call dcl_proc(parser,'tanh(sreal)->sreal',op_tanh_r,0,line,0)
    call dcl_proc(parser,'floor(sreal)->sreal',op_floor_r,0,line,0)
    call dcl_proc(parser,'ceil(sreal)->sreal',op_ceil_r,0,line,0)
    call dcl_proc(parser,'rem(sreal,sreal)->sreal',op_modulo_r,0,line,0)
    call dcl_proc(parser,'sign(sreal,sreal)->sreal',op_sign_r,0,line,0)
    call dcl_proc(parser,'pdiff(sreal,sreal)->sreal',op_pdiff_r,0,line,0)
    call dcl_proc(parser,'lint(sreal)->lint',op_offset_r,0,line,0)
    call dcl_proc(parser,'scpx(sreal)->scpx',op_complex_r,0,line,0)
    call dcl_proc(parser,'_scpx2(sreal,sreal)->scpx',op_complex2_r,0,line,0)
    call dcl_uproc(parser,'scpx(x:any_real,y:any_real)=_scpx2(sreal(x),sreal(y))',line)
    
    ! real type
    call dcl_proc(parser,'PM__assign_var(&real,real)',&
         op_assign_d,0,line,0)
    call dcl_proc(parser,'mod(real,real)->real',op_mod_d,0,line,0)
    call dcl_proc(parser,'==(real,real)->bool',op_eq_d,0,line,0)
    call dcl_proc(parser,'/=(real,real)->bool',op_ne_d,0,line,0)
    call dcl_proc(parser,'>=(real,real)->bool',op_ge_d,0,line,0)
    call dcl_proc(parser,'>(real,real)->bool',op_gt_d,0,line,0)
    call dcl_proc(parser,'+(real,real)->real',op_add_d,0,line,0)
    call dcl_proc(parser,'-(real,real)->real',op_sub_d,0,line,0)
    call dcl_proc(parser,'*(real,real)->real',op_mult_d,0,line,0)
    call dcl_proc(parser,'/(real,real)->real',op_divide_d,0,line,0)
    call dcl_proc(parser,'**(real,real)->real',op_pow_d,0,line,0)
    call dcl_proc(parser,'max(real,real)->real',op_max_d,0,line,0)
    call dcl_proc(parser,'min(real,real)->real',op_min_d,0,line,0)
    call dcl_proc(parser,'-(real)->real',op_uminus_d,0,line,0)
    call dcl_proc(parser,'string(real)->string',op_string_d,0,line,0)
    call dcl_proc(parser,'strunc(real)->sint',op_int_d,0,line,0)
    call dcl_proc(parser,'trunc(real)->int',op_long_d,0,line,0)
    call dcl_proc(parser,'ltrunc(real)->lint',op_offset_d,0,line,0)
    call dcl_proc(parser,'sreal(real)->sreal',op_real_d,0,line,0)
    call dcl_uproc(parser,'real(x:real)=x',line)
    call dcl_proc(parser,'abs(real)->real',op_abs_d,0,line,0)
    call dcl_proc(parser,'acos(real)->real',op_acos_d,0,line,0)
    call dcl_proc(parser,'asin(real)->real',op_asin_d,0,line,0)
    call dcl_proc(parser,'atan(real)->real',op_atan_d,0,line,0)
    call dcl_proc(parser,'atan2(real,real)->real',&
         op_atan2_d,0,line,0)
    call dcl_proc(parser,'cos(real)->real',op_cos_d,0,line,0)
    call dcl_proc(parser,'cosh(real)->real',op_cosh_d,0,line,0)
    call dcl_proc(parser,'exp(real)->real',op_exp_d,0,line,0)
    call dcl_proc(parser,'log(real)->real',op_log_d,0,line,0)
    call dcl_proc(parser,'log10(real)->real',op_log10_d,0,line,0)
    call dcl_proc(parser,'sin(real)->real',op_sin_d,0,line,0)
    call dcl_proc(parser,'sinh(real)->real',op_sinh_d,0,line,0)
    call dcl_proc(parser,'sqrt(real)->real',op_sqrt_d,0,line,0)
    call dcl_proc(parser,'tan(real)->real',op_tan_d,0,line,0)
    call dcl_proc(parser,'tanh(real)->real',op_tanh_d,0,line,0)
    call dcl_proc(parser,'floor(real)->real',op_floor_d,0,line,0)
    call dcl_proc(parser,'ceil(real)->real',op_ceil_d,0,line,0)
    call dcl_proc(parser,'rem(real,real)->real',op_modulo_d,0,line,0)
    call dcl_proc(parser,'sign(real,real)->real',op_sign_d,0,line,0)
    call dcl_proc(parser,'pdiff(real,real)->real',op_pdiff_d,0,line,0)
    call dcl_proc(parser,'lint(real)->lint',op_offset_d,0,line,0)
    call dcl_proc(parser,'cpx(real)->cpx',op_complex_d,0,line,0)
    call dcl_proc(parser,'_cpx2(real,real)->cpx',op_complex2_d,0,line,0)
    call dcl_uproc(parser,'cpx(x:real_num,y:real_num)=_cpx2(real(x),real(y))',line)
    
    ! scpx type
    call dcl_proc(parser,'PM__assign_var(&scpx,scpx)',&
         op_assign_c,0,line,0)
    call dcl_proc(parser,'+(scpx,scpx)->scpx',op_add_c,0,line,0)
    call dcl_proc(parser,'-(scpx,scpx)->scpx',op_sub_c,0,line,0)
    call dcl_proc(parser,'*(scpx,scpx)->scpx',op_mult_c,0,line,0)
    call dcl_proc(parser,'/(scpx,scpx)->scpx',op_divide_c,0,line,0)
    call dcl_proc(parser,'**(scpx,sreal)->scpx',op_rpow_c,0,line,0)
    call dcl_proc(parser,'**(scpx,scpx)->scpx',op_pow_c,0,line,0)
    call dcl_proc(parser,'-(scpx)->scpx',op_uminus_c,0,line,0)
    call dcl_proc(parser,'==(scpx,scpx)->bool',op_eq_c,0,line,0)
    call dcl_proc(parser,'/=(scpx,scpx)->bool',op_ne_c,0,line,0)
    call dcl_proc(parser,'re(scpx)->sreal',op_real_c,0,line,0)
    call dcl_proc(parser,'abs(scpx)->scpx',op_abs_c,0,line,0)
    call dcl_proc(parser,'acos(scpx)->scpx',op_acos_c,0,line,0)
    call dcl_proc(parser,'asin(scpx)->scpx',op_asin_c,0,line,0)
    call dcl_proc(parser,'atan(scpx)->scpx',op_atan_c,0,line,0)
    call dcl_proc(parser,'atan2(scpx,scpx)->scpx',&
         op_atan2_c,0,line,0)
    call dcl_proc(parser,'cos(scpx)->scpx',op_cos_c,0,line,0)
    call dcl_proc(parser,'cosh(scpx)->scpx',op_cosh_c,0,line,0)
    call dcl_proc(parser,'exp(scpx)->scpx',op_exp_c,0,line,0)
    call dcl_proc(parser,'log(scpx)->scpx',op_log_c,0,line,0)
    call dcl_proc(parser,'sin(scpx)->scpx',op_sin_c,0,line,0)
    call dcl_proc(parser,'sinh(scpx)->scpx',op_sinh_c,0,line,0)
    call dcl_proc(parser,'sqrt(scpx)->scpx',op_sqrt_c,0,line,0)
    call dcl_proc(parser,'tan(scpx)->scpx',op_tan_c,0,line,0)
    call dcl_proc(parser,'tanh(scpx)->scpx',op_tanh_c,0,line,0)
    call dcl_proc(parser,'im(scpx)->sreal',op_imag_c,0,line,0)
    call dcl_proc(parser,'conj(scpx)->scpx',op_conj_c,0,line,0)
    
    ! cpx type
    call dcl_proc(parser,'PM__assign_var(&cpx,cpx)',&
         op_assign_dc,0,line,0)
    call dcl_proc(parser,'+(cpx,cpx)->cpx',op_add_dc,0,line,0)
    call dcl_proc(parser,'-(cpx,cpx)->cpx',op_sub_dc,0,line,0)
    call dcl_proc(parser,'*(cpx,cpx)->cpx',op_mult_dc,0,line,0)
    call dcl_proc(parser,'/(cpx,cpx)->cpx',op_divide_dc,0,line,0)
    call dcl_proc(parser,'**(cpx,real)->cpx',op_dpow_dc,0,line,0)
    call dcl_uproc(parser,'**(x:cpx,y:sreal)=x**real(y)',line)
    call dcl_proc(parser,'**(cpx,cpx)->cpx',op_pow_dc,0,line,0)
    call dcl_proc(parser,'-(cpx)->cpx',op_uminus_dc,0,line,0)
    call dcl_proc(parser,'==(cpx,cpx)->bool',op_eq_dc,0,line,0)
    call dcl_proc(parser,'/=(cpx,cpx)->bool',op_ne_dc,0,line,0)
    call dcl_proc(parser,'re(cpx)->real',op_real_dc,0,line,0)
    call dcl_proc(parser,'abs(cpx)->cpx',op_abs_dc,0,line,0)
    call dcl_proc(parser,'acos(cpx)->cpx',op_acos_dc,0,line,0)
    call dcl_proc(parser,'asin(cpx)->cpx',op_asin_dc,0,line,0)
    call dcl_proc(parser,'atan(cpx)->cpx',op_atan_dc,0,line,0)
    call dcl_proc(parser,'atan2(cpx,cpx)->cpx',&
         op_atan2_dc,0,line,0)
    call dcl_proc(parser,'cos(cpx)->cpx',op_cos_dc,0,line,0)
    call dcl_proc(parser,'cosh(cpx)->cpx',op_cosh_dc,0,line,0)
    call dcl_proc(parser,'exp(cpx)->cpx',op_exp_dc,0,line,0)
    call dcl_proc(parser,'log(cpx)->cpx',op_log_dc,0,line,0)
    call dcl_proc(parser,'sin(cpx)->cpx',op_sin_dc,0,line,0)
    call dcl_proc(parser,'sinh(cpx)->cpx',op_sinh_dc,0,line,0)
    call dcl_proc(parser,'sqrt(cpx)->cpx',op_sqrt_dc,0,line,0)
    call dcl_proc(parser,'tan(cpx)->cpx',op_tan_dc,0,line,0)
    call dcl_proc(parser,'tanh(cpx)->cpx',op_tanh_dc,0,line,0)
    call dcl_proc(parser,'im(cpx)->real',op_imag_dc,0,line,0)
    call dcl_proc(parser,'conj(cpx)->cpx',op_conj_dc,0,line,0)
    
    ! Cannot convert real to int (must use nint or trunc)
    call dcl_uproc(parser,&
         'sint(x:any_real)=sint(0) :test "Cannot convert real to integer" => ''false',line)
    call dcl_uproc(parser,&
         'int(x:any_real)=0 :test "Cannot convert real to integer" => ''false',line)
    call dcl_uproc(parser,&
         'lint(x:any_real)=lint(0) :test "Cannot convert real to integer" => ''false',line)

    ! Some numeric conversions not hard-coded
    call dcl_uproc(parser,'cpx(x:real_num)=cpx(real(x))',line)
    call dcl_uproc(parser,'scpx(x:real_num)=cpx(sreal(x))',line)
    call dcl_uproc(parser,'string(x:any_int)=string(int64(x))',line)
    call dcl_uproc(parser,&
         'string(x:any_cpx)=string(re(x))++if(im>=0=>"+"++string(im),"-"++string(-im))++"i" where im=im(x)',line)
    call dcl_uproc(parser,'int(x:fix int)=x',line)
    
    ! Abstract numeric types
    call dcl_type(parser,'any_int is sint,int,lint,int8,int16,int32,int64',&
         line)
    call dcl_type(parser,'any_real is sreal,real',line)
    call dcl_type(parser,'any_cpx is scpx,cpx',line)
    call dcl_type(parser,'int_num is any_int',line)
    call dcl_type(parser,'real_num is int_num, any_real',line)
    call dcl_type(parser,'cpx_num is real_num,any_cpx',line)
    call dcl_type(parser,'num is cpx_num',line)

    ! Numeric type conversion
    call dcl_uproc(parser,'convert(x,y)=x',line)
    call dcl_uproc(parser,'convert(x:int_num,y:sint)=sint(x)',line)
    call dcl_uproc(parser,'convert(x:int_num,y:int)=int(x)',line)
    call dcl_uproc(parser,'convert(x:int_num,y:lint)=lint(x)',line)
    call dcl_uproc(parser,'convert(x:int_num,y:int8)=int8(x)',line)
    call dcl_uproc(parser,'convert(x:int_num,y:int16)=int16(x)',line)
    call dcl_uproc(parser,'convert(x:int_num,y:int32)=int32(x)',line)
    call dcl_uproc(parser,'convert(x:int_num,y:int64)=int64(x)',line)
    call dcl_uproc(parser,'convert(x:int_num,y:sreal)=sreal(x)',line)
    call dcl_uproc(parser,'convert(x:int_num,y:real)=real(x)',line)
    call dcl_uproc(parser,'convert(x:real_num,y:cpx)=cpx(x)',line)
    call dcl_uproc(parser,'convert(x:real_num,y:scpx)=scpx(x)',line)

    call dcl_uproc(parser,'as(x:int_num,y:<sint>)=sint(x)',line)
    call dcl_uproc(parser,'as(x:int_num,y:<int>)=int(x)',line)
    call dcl_uproc(parser,'as(x:int_num,y:<lint>)=lint(x)',line)
    call dcl_uproc(parser,'as(x:int_num,y:<int8>)=int8(x)',line)
    call dcl_uproc(parser,'as(x:int_num,y:<int16>)=int16(x)',line)
    call dcl_uproc(parser,'as(x:int_num,y:<int32>)=int32(x)',line)
    call dcl_uproc(parser,'as(x:int_num,y:<int64>)=int64(x)',line)
    call dcl_uproc(parser,'as(x:real_num,y:<sreal>)=sreal(x)',line)
    call dcl_uproc(parser,'as(x:real_num,y:<real>)=real(x)',line)
    call dcl_uproc(parser,'as(x:real_num,y:<scpx>)=scpx(x)',line)
    call dcl_uproc(parser,'as(x:real_num,y:<cpx>)=cpx(x)',line)
 
    ! Auto-conversion on assignment
    call dcl_uproc(parser,'PM__assign(&x:num,y:num) {_assign_element(&x,convert(y,x))}',line)
    call dcl_uproc(parser,'PM__assign_var(&x:num,y:num) {PM__assign(&x,convert(y,x))}',line)
    
    ! Mixed arithmatic
    call dcl_type(parser,'_to_sint is int',line)
    call dcl_type(parser,'_to_lint is sint,int',line)
    call dcl_type(parser,'_to_int8 is sint,int,lint',line)
    call dcl_type(parser,'_to_int16 is sint,int,lint,int8',line)
    call dcl_type(parser,'_to_int32 is sint,int,lint,int8,int16',line)
    call dcl_type(parser,'_to_int64 is sint,int,lint,int8,int16,int32',line)    
    call dcl_type(parser,'_to_real is any_int',line)
    call dcl_type(parser,'_to_sreal is any_int,real',line)
    call dcl_type(parser,'_to_cpx is real_num',line)
    call dcl_type(parser,'_to_scpx is real_num,cpx',line)
    
    call dcl_uproc(parser,'balance(x:sint,y:sint)=x,y',line)
    call dcl_uproc(parser,'balance(x:int,y:int)=x,y',line)
    call dcl_uproc(parser,'balance(x:lint,y:lint)=x,y',line)
    call dcl_uproc(parser,'balance(x:int8,y:int8)=x,y',line)
    call dcl_uproc(parser,'balance(x:int16,y:int16)=x,y',line)
    call dcl_uproc(parser,'balance(x:int32,y:int32)=x,y',line)
    call dcl_uproc(parser,'balance(x:int64,y:int64)=x,y',line)
    call dcl_uproc(parser,'balance(x:sreal,y:sreal)=x,y',line)
    call dcl_uproc(parser,'balance(x:real,y:real)=x,y',line)
    call dcl_uproc(parser,'balance(x:scpx,y:scpx)=x,y',line)
    call dcl_uproc(parser,'balance(x:cpx,y:cpx)=x,y',line)
    
    call dcl_uproc(parser,'balance(x:sint,y:_to_sint)=x,sint(y)',line)
    call dcl_uproc(parser,'balance(x:lint,y:_to_lint)=x,lint(y)',line)
    call dcl_uproc(parser,'balance(x:int8,y:_to_int8)=x,int8(y)',line)
    call dcl_uproc(parser,'balance(x:int16,y:_to_int16)=x,int16(y)',line)
    call dcl_uproc(parser,'balance(x:int32,y:_to_int32)=x,int32(y)',line)
    call dcl_uproc(parser,'balance(x:int64,y:_to_int64)=x,int64(y)',line)
    call dcl_uproc(parser,'balance(x:sreal,y:_to_sreal)=x,sreal(y)',line)
    call dcl_uproc(parser,'balance(x:real,y:_to_real)=x,real(y)',line)
    call dcl_uproc(parser,'balance(x:scpx,y:_to_scpx)=x,scpx(y)',line)
    call dcl_uproc(parser,'balance(x:cpx,y:_to_cpx)=x,cpx(y)',line)

    call dcl_uproc(parser,'balance(x:_to_sint,y:sint)=sint(x),y',line)
    call dcl_uproc(parser,'balance(x:_to_lint,y:lint)=lint(x),y',line)
    call dcl_uproc(parser,'balance(x:_to_int8,y:int8)=int8(x),y',line)
    call dcl_uproc(parser,'balance(x:_to_int16,y:int16)=int16(x),y',line)
    call dcl_uproc(parser,'balance(x:_to_int32,y:int32)=int32(x),y',line)
    call dcl_uproc(parser,'balance(x:_to_int64,y:int64)=int64(x),y',line)
    call dcl_uproc(parser,'balance(x:_to_sreal,y:sreal)=sreal(x),y',line)
    call dcl_uproc(parser,'balance(x:_to_real,y:real)=real(x),y',line)
    call dcl_uproc(parser,'balance(x:_to_scpx,y:scpx)=scpx(x),y',line)
    call dcl_uproc(parser,'balance(x:_to_cpx,y:cpx)=cpx(x),y',line)

    call dcl_uproc(parser,&
         'mod(x:real_num,y:real_num)=xx mod yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '==(x:num,y:num)=xx==yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '/=(x:num,y:num)=xx/=yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '>=(x:real_num,y:real_num)=xx>=yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '>(x:real_num,y:real_num)=xx>yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '+(x:num,y:num)=xx+yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '-(x:num,y:num)=xx-yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '*(x:num,y:num)=xx*yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '/(x:num,y:num)=xx/yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '**(x:num,y:num)=xx**yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         'max(x:num,y:num)=max(xx,yy) where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         'min(x:num,y:num)=min(xx,yy) where xx,yy=balance(x,y)',line)

    ! bool type
    call dcl_proc(parser,'PM__assign_var(&bool,bool)',&
         op_assign_l,0,line,0)
    call dcl_proc(parser,'string(bool)->string',op_string_l,0,line,0)
    call dcl_proc(parser,'and(bool,bool)->bool',op_and,0,line,0)
    call dcl_proc(parser,'or(bool,bool)->bool',op_or,0,line,0)
    call dcl_proc(parser,'not(bool)->bool',op_not,0,line,0)
    call dcl_proc(parser,'==(bool,bool)->bool',op_eq_l,0,line,0)
    call dcl_proc(parser,'/=(bool,bool)->bool',op_ne_l,0,line,0)

    ! Compile time bool values
    call dcl_uproc(parser,'and(x:''false,y:''false)=''false',line)
    call dcl_uproc(parser,'and(x:''true,y:''false)=''false',line)
    call dcl_uproc(parser,'and(x:''false,y:''true)=''false',line)
    call dcl_uproc(parser,'and(x:''true,y:''true)=''true',line)
    call dcl_uproc(parser,'and(x:''true,y:bool)=y',line)
    call dcl_uproc(parser,'and(x:''false,y:bool)=''false',line)
    call dcl_uproc(parser,'and(x:bool,y:''true)=x',line)
    call dcl_uproc(parser,'and(x:bool,y:''false)=''false',line)
    call dcl_uproc(parser,'or(x:''false,y:''false)=''false',line)
    call dcl_uproc(parser,'or(x:''true,y:''false)=''true',line)
    call dcl_uproc(parser,'or(x:''false,y:''true)=''true',line)
    call dcl_uproc(parser,'or(x:''true,y:''true)=''true',line)
    call dcl_uproc(parser,'or(x:''true,y:bool)=''true',line)
    call dcl_uproc(parser,'or(x:''false,y:bool)=y',line)
    call dcl_uproc(parser,'or(x:bool,y:''true)=''true',line)
    call dcl_uproc(parser,'or(x:bool,y:''false)=x',line)
    call dcl_uproc(parser,'not(x:''true)=''false',line)
    call dcl_uproc(parser,'not(x:''false)=''true',line)
    call dcl_uproc(parser,'==(x:''false,y:''false)=''true',line)
    call dcl_uproc(parser,'==(x:''true,y:''false)=''false',line)
    call dcl_uproc(parser,'==(x:''false,y:''true)=''false',line)
    call dcl_uproc(parser,'==(x:''true,y:''true)=''true',line)
    call dcl_uproc(parser,'==(x:bool,y:''true)=x',line)
    call dcl_uproc(parser,'==(x:''true,y:bool)=y',line)
    call dcl_uproc(parser,'/=(x:''false,y:''false)=''false',line)
    call dcl_uproc(parser,'/=(x:''true,y:''false)=''true',line)
    call dcl_uproc(parser,'/=(x:''false,y:''true)=''true',line)
    call dcl_uproc(parser,'/=(x:''true,y:''true)=''false',line)
    call dcl_uproc(parser,'/=(x:bool,y:''false)=x',line)
    call dcl_uproc(parser,'/=(x:''false,y:bool)=y',line)
    
    ! Masked types
    call dcl_type(parser,'masked(x) is '//&
         'rec {_val:x,_there:bool}',line)
    call dcl_uproc(parser,'|(x:masked,y)=if(x._there=>x._val,y)'//&
         'check "Right operand of ""|"" does not match masked type on the left"=>same_type(x._val,y)',line)
    call dcl_uproc(parser,&
         'masked(val,there:bool)=new masked {_val=val,_there=there}',line)
    call dcl_uproc(parser,'defined(x:masked)=x._there',line)
    call dcl_uproc(parser,'val(x:masked)=x._val '//&
         'check "masked value is undefined"=>x._there',line)
    call dcl_uproc(parser,'get(&x,y:masked) {if y._there{x=y._val}}',line)
    call dcl_uproc(parser,&
         'get(&x,y:masked(x)) {if y._there{x=y._val};return y._there}',line)

    ! Polymorphic types
    call dcl_proc(parser,'get(x:*any,y:any)->=y',op_as,0,line,0)
    call dcl_proc(parser,'get(&x:any,y:*any)',op_get_poly,0,line,&
         proc_needs_type)
    call dcl_proc(parser,'get(&x:any,y:*any)->bool',op_get_poly2,0,line,&
         proc_needs_type)
    call dcl_proc(parser,'|(x:*any,y:any)->=y',op_get_poly_or,0,line,&
         proc_needs_type)
    
    ! val function having null effect
    call dcl_uproc(parser,'val(x)=x',line)
    

    ! ********************************************
    ! TUPLES
    ! ********************************************

    ! Tuple types
    call dcl_type(parser,&
         'tuple1d(t1) is rec {PM__d1:t1}',line)
    call dcl_type(parser,&
         'tuple2d(t1,t2) is rec {PM__d1:t1,PM__d2:t2}',line)
    call dcl_type(parser,&
         'tuple3d(t1,t2,t3) is'//&
         ' rec {PM__d1:t1,PM__d2:t2,PM__d3:t3}',line)
    call dcl_type(parser,&
         'tuple4d(t1,t2,t3,t4) is'//&
         ' rec {PM__d1:t1,PM__d2:t2,PM__d3:t3,PM__d4:t4}',line)
    call dcl_type(parser,&
         'tuple5d(t1,t2,t3,t4,t5) is'//&
         ' rec {PM__d1:t1,PM__d2:t2,PM__d3:t3,PM__d4:t4,PM__d5:t5}',line)
    call dcl_type(parser,&
         'tuple6d(t1,t2,t3,t4,t5,t6) is'//&
         ' rec {PM__d1:t1,PM__d2:t2,PM__d3:t3,PM__d4:t4,PM__d5:t5,PM__d6:t6}',line)
    call dcl_type(parser,&
         'tuple7d(t1,t2,t3,t4,t5,t6,t7) is'//&
         ' rec {PM__d1:t1,PM__d2:t2,PM__d3:t3,PM__d4:t4,PM__d5:t5,PM__d6:t6,PM__d7:t7}',line)
    
    call dcl_type(parser,'tuple1d_of(t) is tuple1d(t)',line)
    call dcl_type(parser,'tuple2d_of(t) is tuple2d(t,t)',line)
    call dcl_type(parser,'tuple3d_of(t) is tuple3d(t,t,t)',line)
    call dcl_type(parser,'tuple4d_of(t) is tuple4d(t,t,t,t)',line)
    call dcl_type(parser,'tuple5d_of(t) is tuple5d(t,t,t,t,t)',line)
    call dcl_type(parser,'tuple6d_of(t) is tuple6d(t,t,t,t,t,t)',line)
    call dcl_type(parser,'tuple7d_of(t) is tuple7d(t,t,t,t,t,t,t)',line)
    
    call dcl_type(parser,'tuple(t) is '//&
         'tuple1d(t),tuple2d(t,t),tuple3d(t,t,t),tuple4d(t,t,t,t),'//&
         'tuple5d(t,t,t,t,t),tuple6d(t,t,t,t,t,t),'//&
         'tuple7d(t,t,t,t,t,t,t)',line)

    call dcl_uproc(parser,'tuple(x)=new tuple1d {PM__d1=x}',line)
    call dcl_uproc(parser,'tuple(x,y)='//&
         'new tuple2d {PM__d1=x,PM__d2=y}',line)
    call dcl_uproc(parser,'tuple(x,y,z)='//&
         'new tuple3d {PM__d1=x,PM__d2=y,PM__d3=z}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t)='//&
         'new tuple4d {PM__d1=x,PM__d2=y,PM__d3=z,PM__d4=t}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u)='//&
         'new tuple5d {PM__d1=x,PM__d2=y,PM__d3=z,PM__d4=t,PM__d5=u}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u,v)='//&
         'new tuple6d {PM__d1=x,PM__d2=y,PM__d3=z,PM__d4=t,PM__d5=u,PM__d6=v}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u,v,w)='//&
         'new tuple7d {PM__d1=x,PM__d2=y,PM__d3=z,PM__d4=t,PM__d5=u,PM__d6=v,PM__d7=w}',line)

    call dcl_uproc(parser,'get_dim(t:tuple1d,n:''1 or [''1])=t.1',line)
    call dcl_uproc(parser,'get_dim(t:tuple2d,n:''1 or [''1])=t.1',line)
    call dcl_uproc(parser,'get_dim(t:tuple3d,n:''1 or [''1])=t.1',line)
    call dcl_uproc(parser,'get_dim(t:tuple4d,n:''1 or [''1])=t.1',line)
    call dcl_uproc(parser,'get_dim(t:tuple5d,n:''1 or [''1])=t.1',line)
    call dcl_uproc(parser,'get_dim(t:tuple6d,n:''1 or [''1])=t.1',line)
    call dcl_uproc(parser,'get_dim(t:tuple7d,n:''1 or [''1])=t.1',line)
    call dcl_uproc(parser,'get_dim(t:tuple2d,n:''2 or [''2])=t.2',line)
    call dcl_uproc(parser,'get_dim(t:tuple3d,n:''2 or [''2])=t.2',line)
    call dcl_uproc(parser,'get_dim(t:tuple4d,n:''2 or [''2])=t.2',line)
    call dcl_uproc(parser,'get_dim(t:tuple5d,n:''2 or [''2])=t.2',line)
    call dcl_uproc(parser,'get_dim(t:tuple6d,n:''2 or [''2])=t.2',line)
    call dcl_uproc(parser,'get_dim(t:tuple7d,n:''2 or [''2])=t.2',line)
    call dcl_uproc(parser,'get_dim(t:tuple3d,n:''3 or [''3])=t.3',line)
    call dcl_uproc(parser,'get_dim(t:tuple4d,n:''3 or [''3])=t.3',line)
    call dcl_uproc(parser,'get_dim(t:tuple5d,n:''3 or [''3])=t.3',line)
    call dcl_uproc(parser,'get_dim(t:tuple6d,n:''3 or [''3])=t.3',line)
    call dcl_uproc(parser,'get_dim(t:tuple7d,n:''3 or [''3])=t.3',line)
    call dcl_uproc(parser,'get_dim(t:tuple4d,n:''4 or [''4])=t.4',line)
    call dcl_uproc(parser,'get_dim(t:tuple5d,n:''4 or [''4])=t.4',line)
    call dcl_uproc(parser,'get_dim(t:tuple6d,n:''4 or [''4])=t.4',line)
    call dcl_uproc(parser,'get_dim(t:tuple7d,n:''4 or [''4])=t.4',line)
    call dcl_uproc(parser,'get_dim(t:tuple5d,n:''5 or [''5])=t.5',line)
    call dcl_uproc(parser,'get_dim(t:tuple6d,n:''5 or [''5])=t.5',line)
    call dcl_uproc(parser,'get_dim(t:tuple7d,n:''5 or [''5])=t.5',line)
    call dcl_uproc(parser,'get_dim(t:tuple6d,n:''6 or [''6])=t.6',line)
    call dcl_uproc(parser,'get_dim(t:tuple7d,n:''6 or [''6])=t.6',line)
    call dcl_uproc(parser,'get_dim(t:tuple7d,n:''7 or [''7])=t.7',line)
!!$    call dcl_uproc(parser,'get_dim(t:tuple,n:fix int)=t.1'//&
!!$         ' :test "tuple subscript out of range" => ''false',line)

    call dcl_uproc(parser,'indices(x:tuple1d)=[''1]',line)
    call dcl_uproc(parser,'indices(x:tuple2d)=[''1,''2]',line)
    call dcl_uproc(parser,'indices(x:tuple3d)=[''1,''2,''3]',line)
    call dcl_uproc(parser,'indices(x:tuple4d)=[''1,''2,''3,''4]',line)
    call dcl_uproc(parser,'indices(x:tuple5d)=[''1,''2,''3,''4,''5]',line)
    call dcl_uproc(parser,'indices(x:tuple6d)=[''1,''2,''3,''4,''5,''6]',line)
    call dcl_uproc(parser,'indices(x:tuple7d)=[''1,''2,''3,''4,''5,''6,''7]',line)
       
    call dcl_uproc(parser,'full_rank(x:tuple1d)=''1',line)
    call dcl_uproc(parser,'full_rank(x:tuple2d)=''2',line)
    call dcl_uproc(parser,'full_rank(x:tuple3d)=''3',line)
    call dcl_uproc(parser,'full_rank(x:tuple4d)=''4',line)
    call dcl_uproc(parser,'full_rank(x:tuple5d)=''5',line)
    call dcl_uproc(parser,'full_rank(x:tuple6d)=''6',line)
    call dcl_uproc(parser,'full_rank(x:tuple7d)=''7',line)

    call dcl_uproc(parser,'rank(x:tuple)=full_rank(x)',line)
    
    call dcl_uproc(parser,'reduce(p:proc,x:tuple1d)=x.1',line)
    call dcl_uproc(parser,'reduce(p:proc,x:tuple2d)='//&
         'p.(x.2,x.1)',line)
    call dcl_uproc(parser,'reduce(p:proc,x:tuple3d)='//&
         'p.(p.(x.3,x.2),x.1)',line)
    call dcl_uproc(parser,'reduce(p:proc,x:tuple4d)='//&
         'p.(p.(p.(x.4,x.3),x.2),x.1)',line)
    call dcl_uproc(parser,'reduce(p:proc,x:tuple5d)='//&
         'p.(p.(p.(p.(x.5,x.4),x.3),x.2),x.1)',line)
    call dcl_uproc(parser,'reduce(p:proc,x:tuple6d)='//&
         'p.(p.(p.(p.(p.(x.6,x.5),x.4),x.3),x.2),x.1)',line)
    call dcl_uproc(parser,'reduce(p:proc,x:tuple7d)='//&
         'p.(p.(p.(p.(p.(p.(x.7,x.6),x.5),x.4),x.3),x.2),x.1)',line)
    
    call dcl_uproc(parser,'map(p:proc,x:tuple1d)='//&
         '[p.(x.1)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple2d)='//&
         '[p.(x.1),p.(x.2)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple3d)='//&
         '[p.(x.1),p.(x.2),p.(x.3)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple4d)='//&
         '[p.(x.1),p.(x.2),p.(x.3),p.(x.4)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple5d)='//&
         '[p.(x.1),p.(x.2),p.(x.3),p.(x.4),p.(x.5)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple6d)='//&
         '[p.(x.1),p.(x.2),p.(x.3),p.(x.4),p.(x.5),p.(x.6)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple7d)='//&
         '[p.(x.1),p.(x.2),p.(x.3),p.(x.4),p.(x.5),p.(x.6),p.(x.7)]',line)

    call dcl_uproc(parser,'map(p:proc,x:tuple,y:tuple)=error_type()'//&
         ' :test "Number of dimensions does not match" => ''false',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple1d,y:tuple1d)='//&
         '[p.(x.1,y.1)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple2d,y:tuple2d)='//&
         '[p.(x.1,y.1),p.(x.2,y.2)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple3d,y:tuple3d)='//&
         '[p.(x.1,y.1),p.(x.2,y.2),p.(x.3,y.3)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple4d,y:tuple4d)='//&
         '[p.(x.1,y.1),p.(x.2,y.2),p.(x.3,y.3),'//&
         'p.(x.4,y.4)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple5d,y:tuple5d)='//&
         '[p.(x.1,y.1),p.(x.2,y.2),p.(x.3,y.3),'//&
         'p.(x.4,y.4),p.(x.5,y.5)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple6d,y:tuple6d)='//&
         '[p.(x.1,y.1),p.(x.2,y.2),p.(x.3,y.3),'//&
         'p.(x.4,y.4),p.(x.5,y.5),p.(x.6,y.6)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple7d,y:tuple7d)='//&
         '[p.(x.1,y.1),p.(x.2,y.2),p.(x.3,y.3),'//&
         'p.(x.4,y.4),p.(x.5,y.5),p.(x.6,y.6),p.(x.7,y.7)]',line)

    call dcl_uproc(parser,'map(p:proc,x:tuple,y:tuple,z:tuple)=error_type()'//&
         ' :test "Number of dimensions does not match" => ''false',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple1d,y:tuple1d,z:tuple1d)='//&
         '[p.(x.1,y.1,z.1)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple2d,y:tuple2d,z:tuple2d)='//&
         '[p.(x.1,y.1,z.1),p.(x.2,y.2,z.2)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple3d,y:tuple3d,z:tuple3d)='//&
         '[p.(x.1,y.1,z.1),p.(x.2,y.2,z.2),p.(x.3,y.3,z.3)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple4d,y:tuple4d,z:tuple4d)='//&
         '[p.(x.1,y.1,z.1),p.(x.2,y.2,z.2),p.(x.3,y.3,z.3),'//&
         'p.(x.4,y.4,z.4)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple5d,y:tuple5d,z:tuple5d)='//&
         '[p.(x.1,y.1,z.1),p.(x.2,y.2,z.2),p.(x.3,y.3,z.3),'//&
         'p.(x.4,y.4,z.4),p.(x.5,y.5,z.5)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple6d,y:tuple6d,z:tuple6d)='//&
         '[p.(x.1,y.1,z.1),p.(x.2,y.2,z.2),p.(x.3,y.3,z.3),'//&
         'p.(x.4,y.4,z.4),p.(x.5,y.5,z.5),p.(x.6,y.6,z.6)]',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple7d,y:tuple7d,z:tuple7d)='//&
         '[p.(x.1,y.1,z.1),p.(x.2,y.2,z.2),p.(x.3,y.3,z.3),'//&
         'p.(x.4,y.4,z.4),p.(x.5,y.5,z.5),p.(x.6,y.6,z.6),'//&
         'p.(x.7,y.7,z.7)]',line)

    call dcl_uproc(parser,'map(p:proc,x:tuple1d,y:tuple1d)=[u1],[v1]'//&
         'where u1,v1=p.(x.1,y.1)',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple2d,y:tuple2d)=[u1,u2],[v1,v2]'//&
         'where u1,v1=p.(x.1,y.1),u2,v2=p.(x.2,y.2)',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple3d,y:tuple3d)=[u1,u2,u3],[v1,v2,v3]'//&
         'where u1,v1=p.(x.1,y.1),u2,v2=p.(x.2,y.2),u3,v3=p.(x.3,y.3)',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple4d,y:tuple4d)=[u1,u2,u3,u4],[v1,v2,v3,v4]'//&
         'where u1,v1=p.(x.1,y.1),u2,v2=p.(x.2,y.2),u3,v3=p.(x.3,y.3),u4,v4=p.(x.4,y.4)',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple5d,y:tuple5d)=[u1,u2,u3,u4,u5],[v1,v2,v3,v4,v5]'//&
         'where u1,v1=p.(x.1,y.1),u2,v2=p.(x.2,y.2),u3,v3=p.(x.3,y.3),u4,v4=p.(x.4,y.4),u5,v5=p.(x.5,y.5)',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple6d,y:tuple6d)=[u1,u2,u3,u4,u5,u6],[v1,v2,v3,v4,v5,v6]'//&
         'where u1,v1=p.(x.1,y.1),u2,v2=p.(x.2,y.2),u3,v3=p.(x.3,y.3),u4,v4=p.(x.4,y.4),u5,v5=p.(x.5,y.5),'//&
         'u6,v6=p.(x.6,y.6)',line)
    call dcl_uproc(parser,'map(p:proc,x:tuple7d,y:tuple7d)=[u1,u2,u3,u4,u5,u6,u7],[v1,v2,v3,v4,v5,v6,v7]'//&
         'where u1,v1=p.(x.1,y.1),u2,v2=p.(x.2,y.2),u3,v3=p.(x.3,y.3),u4,v4=p.(x.4,y.4),u5,v5=p.(x.5,y.5),'//&
         'u6,v6=p.(x.6,y.6),u7,v7=p.(x.7,y.7)',line)    

    call dcl_uproc(parser,'map_const(p:proc,x:tuple1d,y)='//&
         '[p.(x.1,y)]',line)
    call dcl_uproc(parser,'map_const(p:proc,x:tuple2d,y)='//&
         '[p.(x.1,y),p.(x.2,y)]',line)
    call dcl_uproc(parser,'map_const(p:proc,x:tuple3d,y)='//&
         '[p.(x.1,y),p.(x.2,y),p.(x.3,y)]',line)
    call dcl_uproc(parser,'map_const(p:proc,x:tuple4d,y)='//&
         '[p.(x.1,y),p.(x.2,y),p.(x.3,y),'//&
         'p.(x.4,y)]',line)
    call dcl_uproc(parser,'map_const(p:proc,x:tuple5d,y)='//&
         '[p.(x.1,y),p.(x.2,y),p.(x.3,y),'//&
         'p.(x.4,y),p.(x.5,y)]',line)
    call dcl_uproc(parser,'map_const(p:proc,x:tuple6d,y)='//&
         '[p.(x.1,y),p.(x.2,y),p.(x.3,y),'//&
         'p.(x.4,y),p.(x.5,y),p.(x.6,y)]',line)
    call dcl_uproc(parser,'map_const(p:proc,x:tuple7d,y)='//&
         '[p.(x.1,y),p.(x.2,y),p.(x.3,y),'//&
         'p.(x.4,y),p.(x.5,y),p.(x.6,y),p.(x.7,y)]',line)

    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple1d)='//&
         'q.(x.1)',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple2d)='//&
         'p.(q.(x.2),q.(x.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple3d)='//&
         'p.(p.(q.(x.3),q.(x.2)),q.(x.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple4d)='//&
         'p.(p.(p.(q.(x.4),q.(x.3)),q.(x.2)),q.(x.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple5d)='//&
         'p.(p.(p.(p.(q.(x.5),q.(x.4)),q.(x.3)),'//&
         'q.(x.2)),q.(x.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple6d)='//&
         'p.(p.(p.(p.(p.(q.(x.6),q.(x.5)),q.(x.4)),'//&
         'q.(x.3)),q.(x.2)),q.(x.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple7d)='//&
         'p.(p.(p.(p.(p.(p.(q.(x.7),q.(x.6)),q.(x.5)),'//&
         'q.(x.4)),q.(x.3)),q.(x.2)),q.(x.1))',line)
    
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple,y:tuple)=error_type()'//&
         ' :test "Number of dimensions does not match" => ''false',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple1d,y:tuple1d)='//&
         'q.(x.1,y.1)',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple2d,y:tuple2d)='//&
         'p.(q.(x.2,y.2),q.(x.1,y.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple3d,y:tuple3d)='//&
         'p.(p.(q.(x.3,y.3),q.(x.2,y.2)),'//&
         'q.(x.1,y.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple4d,y:tuple4d)='//&
         'p.(p.(p.(q.(x.4,y.4),q.(x.3,y.3)),q.(x.2,y.2)),'//&
         'q.(x.1,y.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple5d,y:tuple5d)='//&
         'p.(p.(p.(p.(q.(x.5,y.5),q.(x.4,y.4)),'//&
         'q.(x.3,y.3)),q.(x.2,y.2)),q.(x.1,y.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple6d,y:tuple6d)='//&
         'p.(p.(p.(p.(p.(q.(x.6,y.6),q.(x.5,y.5)),q.(x.4,y.4)),'//&
         'q.(x.3,y.3)),q.(x.2,y.2)),q.(x.1,y.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple7d,y:tuple7d)='//&
         'p.(p.(p.(p.(p.(p.(q.(x.7,y.7),q.(x.6,y.6)),q.(x.5,y.5)),'//&
         'q.(x.4,y.4)),q.(x.3,y.3)),q.(x.2,y.2)),q.(x.1,y.1))',line)

    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple,y:tuple,z:tuple)=error_type()'//&
         ' :test "Number of dimensions does not match" => ''false',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple1d,y:tuple1d,z:tuple1d)='//&
         'q.(x.1,y.1,z.1)',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple2d,y:tuple2d,z:tuple2d)='//&
         'p.(q.(x.2,y.2,z.2),q.(x.1,y.1,z.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple3d,y:tuple3d,z:tuple3d)='//&
         'p.(p.(q.(x.3,y.3,z.3),q.(x.2,y.2,z.2)),'//&
         'q.(x.1,y.1,z.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple4d,y:tuple4d,z:tuple4d)='//&
         'p.(p.(p.(q.(x.4,y.4,z.4),q.(x.3,y.3,z.3)),q.(x.2,y.2,z.2)),'//&
         'q.(x.1,y.1,z.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple5d,y:tuple5d,z:tuple5d)='//&
         'p.(p.(p.(p.(q.(x.5,y.5,z.5),q.(x.4,y.4,z.4)),'//&
         'q.(x.3,y.3,z.3)),q.(x.2,y.2,z.2)),q.(x.1,y.1,z.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple6d,y:tuple6d,z:tuple6d)='//&
         'p.(p.(p.(p.(p.(q.(x.6,y.6,z.6),q.(x.5,y.5,z.5)),q.(x.4,y.4,z.4)),'//&
         'q.(x.3,y.3,z.3)),q.(x.2,y.2,z.2)),q.(x.1,y.1,z.1))',line)
    call dcl_uproc(parser,'map_reduce(q:proc,p:proc,x:tuple7d,y:tuple7d,z:tuple7d)='//&
         'p.(p.(p.(p.(p.(p.(q.(x.7,y.7,z.7),q.(x.6,y.6,z.6)),q.(x.5,y.5,z.5)),'//&
         'q.(x.4,y.4,z.4)),q.(x.3,y.3,z.3)),q.(x.2,y.2,z.2)),q.(x.1,y.1,z.1))',line)
    
    call dcl_uproc(parser,'apply(p:proc,x:tuple1d)='//&
         'p.(x.1)',line)
    call dcl_uproc(parser,'apply(p:proc,x:tuple2d)='//&
         'p.(x.1,x.2)',line)
    call dcl_uproc(parser,'apply(p:proc,x:tuple3d)='//&
         'p.(x.1,x.2,x.3)',line)
    call dcl_uproc(parser,'apply(p:proc,x:tuple4d)='//&
         'p.(x.1,x.2,x.3,x.4)',line)
    call dcl_uproc(parser,'apply(p:proc,x:tuple5d)='//&
         'p.(x.1,x.2,x.3,x.4,x.5)',line)
    call dcl_uproc(parser,'apply(p:proc,x:tuple6d)='//&
         'p.(x.1,x.2,x.3,x.4,x.5,x.6)',line)
    call dcl_uproc(parser,'apply(p:proc,x:tuple7d)='//&
         'p.(x.1,x.2,x.3,x.4,x.5,x.6,x.7)',line)

    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple1d)='//&
         'p.(q.(x.1))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple2d)='//&
         'p.(q.(x.1),q.(x.2))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple3d)='//&
         'p.(q.(x.1),q.(x.2),q.(x.3))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple4d)='//&
         'p.(q.(x.1),q.(x.2),q.(x.3),q.(x.4))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple5d)='//&
         'p.(q.(x.1),q.(x.2),q.(x.3),q.(x.4),'//&
         'q.(x.5))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple6d)='//&
         'p.(q.(x.1),q.(x.2),q.(x.3),q.(x.4),'//&
         'q.(x.5),q.(x.6))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple7d)='//&
         'p.(q.(x.1),q.(x.2),q.(x.3),q.(x.4),'//&
         'q.(x.5),q.(x.6),q.(x.7))',line)

    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple,y:tuple)=error_type()'//&
         ':test "Number of dimensions does not match" => ''false',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple1d,y:tuple1d)='//&
         'p.(q.(x.1,y.1))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple2d,y:tuple2d)='//&
         'p.(q.(x.1,y.1),q.(x.2,y.2))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple3d,y:tuple3d)='//&
         'p.(q.(x.1,y.1),q.(x.2,y.2),q.(x.3,y.3))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple4d,y:tuple4d)='//&
         'p.(q.(x.1,y.1),q.(x.2,y.2),q.(x.3,y.3),q.(x.4,y.4))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple5d,y:tuple5d)='//&
         'p.(q.(x.1,y.1),q.(x.2,y.2),q.(x.3,y.3),q.(x.4,y.4),'//&
         'q.(x.5,y.5))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple6d,y:tuple6d)='//&
         'p.(q.(x.1,y.1),q.(x.2,y.2),q.(x.3,y.3),q.(x.4,y.4),'//&
         'q.(x.5,y.5),q.(x.6,y.6))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple7d,y:tuple7d)='//&
         'p.(q.(x.1,y.1),q.(x.2,y.2),q.(x.3,y.3),q.(x.4,y.4),'//&
         'q.(x.5,y.5),q.(x.6,y.6),q.(x.7,y.7))',line)

    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple,y:tuple,z:tuple)='//&
         'error_type() :test "Number of dimensions does not match" => ''false',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple1d,y:tuple1d,z:tuple1d)='//&
         'p.(q.(x.1,y.1,z.1))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple2d,y:tuple2d,z:tuple2d)='//&
         'p.(q.(x.1,y.1,z.1),q.(x.2,y.2,z.2))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple3d,y:tuple3d,z:tuple3d)='//&
         'p.(q.(x.1,y.1,z.1),q.(x.2,y.2,z.2),q.(x.3,y.3,z.3))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple4d,y:tuple4d,z:tuple4d)='//&
         'p.(q.(x.1,y.1,z.1),q.(x.2,y.2,z.2),q.(x.3,y.3,z.3),q.(x.4,y.4,z.4))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple5d,y:tuple5d,z:tuple5d)='//&
         'p.(q.(x.1,y.1,z.1),q.(x.2,y.2,z.2),q.(x.3,y.3,z.3),q.(x.4,y.4,z.4),'//&
         'q.(x.5,y.5,z.5))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple6d,y:tuple6d,z:tuple6d)='//&
         'p.(q.(x.1,y.1,z.1),q.(x.2,y.2,z.2),q.(x.3,y.3,z.3),q.(x.4,y.4,z.4),'//&
         'q.(x.5,y.5,z.5),q.(x.6,y.6,z.6))',line)
    call dcl_uproc(parser,'map_apply(q:proc,p:proc,x:tuple7d,y:tuple7d,z:tuple7d)='//&
         'p.(q.(x.1,y.1,z.1),q.(x.2,y.2,z.2),q.(x.3,y.3,z.3),q.(x.4,y.4,z.4),'//&
         'q.(x.5,y.5,z.5),q.(x.6,y.6,z.6),q.(x.7,y.7,z.7))',line)
    

    call dcl_uproc(parser,'scan(p:proc,x:tuple1d)=x.1',line)
    call dcl_uproc(parser,'scan(p:proc,x:tuple2d)=[x.1,p.(x.1,x.2)]',line)
    call dcl_uproc(parser,'scan(p:proc,x:tuple3d)=[x.1,x2,p.(x2,x.3)]'//&
         ' where x2=p.(x.1,x.2)',line)
    call dcl_uproc(parser,'scan(p:proc,x:tuple4d)=[x.1,x2,x3,p.(x3,x.4)]'//&
         ' where x3=p.(x2,x.3) where x2=p.(x.1,x.2)',line)
    call dcl_uproc(parser,'scan(p:proc,x:tuple5d)=[x.1,x2,x3,x4,p.(x4,x.5)]'//&
         ' where x4=p.(x3,x.4) where x3=p.(x2,x.3) where x2=p.(x.1,x.2)',line)
    call dcl_uproc(parser,'scan(p:proc,x:tuple6d)=[x.1,x2,x3,x4,x5,p.(x5,x.6)]'//&
         ' where x5=p.(x4,x.5) where x4=p.(x3,x.4) where x3=p.(x2,x.3) '//&
         ' where x2=p.(x.1,x.2)',line)
    call dcl_uproc(parser,'scan(p:proc,x:tuple7d)=[x.1,x2,x3,x4,x5,x6,p.(x6,x.7)]'//&
         ' where x6=p.(x5,x.6) where x5=p.(x4,x.5) where x4=p.(x3,x.4) '//&
         ' where x3=p.(x2,x.3) where x2=p.(x.1,x.2)',line)

    call dcl_uproc(parser,'pre_scan(p:proc,x:tuple1d,x0)=x0',line)
    call dcl_uproc(parser,'pre_scan(p:proc,x:tuple2d,x0)=[x0,x.1]',line)
    call dcl_uproc(parser,'pre_scan(p:proc,x:tuple3d,x0)=[x0,x.1,p.(x.1,x.2)]',line)
    call dcl_uproc(parser,'pre_scan(p:proc,x:tuple4d,x0)=[x0,x.1,x2,p.(x2,x.3)]'//&
         ' where x2=p.(x.1,x.2)',line)
    call dcl_uproc(parser,'pre_scan(p:proc,x:tuple5d,x0)=[x0,x.1,x2,x3,p.(x3,x.4)]'//&
         ' where x3=p.(x2,x.3) where x2=p.(x.1,x.2)',line)
    call dcl_uproc(parser,'pre_scan(p:proc,x:tuple6d,x0)=[x0,x.1,x2,x3,x4,p.(x4,x.5)]'//&
         ' where x4=p.(x3,x.4) where x3=p.(x2,x.3) where x2=p.(x.1,x.2)',line)
    call dcl_uproc(parser,'pre_scan(p:proc,x:tuple7d,x0)=[x0,x.1,x2,x3,x4,x5,p.(x5,x.6)]'//&
         ' where x5=p.(x4,x.5) where x4=p.(x3,x.4) where x3=p.(x2,x.3) '//&
         ' where x2=p.(x.1,x.2)',line)

    call dcl_type(parser,'empty_head is unique',line)
    call dcl_uproc(parser,'head(x:null)=empty_head',line)
    call dcl_uproc(parser,'head(x:tuple)=x.1',line)

    call dcl_uproc(parser,'tail(x:null)=null',line)
    call dcl_uproc(parser,'tail(x:tuple1d)=null',line)
    call dcl_uproc(parser,'tail(x:tuple2d)=[x.2]',line)
    call dcl_uproc(parser,'tail(x:tuple3d)=[x.2,x.3]',line)
    call dcl_uproc(parser,'tail(x:tuple4d)=[x.2,x.3,x.4]',line)
    call dcl_uproc(parser,'tail(x:tuple5d)=[x.2,x.3,x.4,x.5]',line)
    call dcl_uproc(parser,'tail(x:tuple6d)=[x.2,x.3,x.4,x.5,x.6]',line)
    call dcl_uproc(parser,'tail(x:tuple7d)=[x.2,x.3,x.4,x.5,x.6,x.7]',line)

    call dcl_uproc(parser,'prepend(y,x:null)=[y]',line)
    call dcl_uproc(parser,'prepend(y,x:tuple1d)=[y,x.1]',line)
    call dcl_uproc(parser,'prepend(y,x:tuple2d)=[y,x.1,x.2]',line)
    call dcl_uproc(parser,'prepend(y,x:tuple3d)=[y,x.1,x.2,x.3]',line)
    call dcl_uproc(parser,'prepend(y,x:tuple4d)=[y,x.1,x.2,x.3,x.4]',line)
    call dcl_uproc(parser,'prepend(y,x:tuple5d)=[y,x.1,x.2,x.3,x.4,x.5]',line)
    call dcl_uproc(parser,'prepend(y,x:tuple6d)=[y,x.1,x.2,x.4,x.4,x.5,x.6]',line)
    call dcl_uproc(parser,&
         'prepend(y,x:tuple7d)=error_type() :test "Cannot add dimension to 7d tuple" => ''false',line)

    call dcl_uproc(parser,'append(x:null,y)=[y]',line)
    call dcl_uproc(parser,'append(x:tuple1d,y)=[x.1,y]',line)
    call dcl_uproc(parser,'append(x:tuple2d,y)=[x.1,x.2,y]',line)
    call dcl_uproc(parser,'append(x:tuple3d,y)=[x.1,x.2,x.3,y]',line)
    call dcl_uproc(parser,'append(x:tuple4d,y)=[x.1,x.2,x.3,x.4,y]',line)
    call dcl_uproc(parser,'append(x:tuple5d,y)=[x.1,x.2,x.3,x.4,x.5,y]',line)
    call dcl_uproc(parser,'append(x:tuple6d,y)=[x.1,x.2,x.4,x.4,x.5,x.6,y]',line)
    call dcl_uproc(parser,&
         'append(y,x:tuple7d)=error_type() :test "Cannot add dimension to 7d tuple" => ''false',line)

    call dcl_uproc(parser,'elems(x:tuple1d)=x.1',line)
    call dcl_uproc(parser,'elems(x:tuple2d)=x.1,x.2',line)
    call dcl_uproc(parser,'elems(x:tuple3d)=x.1,x.2,x,3',line)
    call dcl_uproc(parser,'elems(x:tuple4d)=x.1,x.2,x.3,x.4',line)
    call dcl_uproc(parser,'elems(x:tuple5d)=x.1,x.2,x.3,x.4,x.5',line)
    call dcl_uproc(parser,'elems(x:tuple6d)=x.1,x.2,x.3,x.4,x.5,x.6',line)
    call dcl_uproc(parser,'elems(x:tuple7d)=x.1,x.2,x.3,x.4,x.5,x.6,x.7',line)

    call dcl_uproc(parser,'replace(x:tuple1d,y:''1,z)=[z]',line)
    call dcl_uproc(parser,'replace(x:tuple2d,y:''1,z)=[z,x.2]',line)
    call dcl_uproc(parser,'replace(x:tuple3d,y:''1,z)=[z,x.2,x.3]',line)
    call dcl_uproc(parser,'replace(x:tuple4d,y:''1,z)=[z,x.2,x.3,x.4]',line)
    call dcl_uproc(parser,'replace(x:tuple5d,y:''1,z)=[z,x.2,x.3,x.4,x.5]',line)
    call dcl_uproc(parser,'replace(x:tuple6d,y:''1,z)=[z,x.2,x.3,x.4,x.5,x.6]',line)
    call dcl_uproc(parser,'replace(x:tuple7d,y:''1,z)=[z,x.2,x.3,x.4,x.5,x.6,x.7]',line)
    call dcl_uproc(parser,'replace(x:tuple2d,y:''2,z)=[x.1,z]',line)
    call dcl_uproc(parser,'replace(x:tuple3d,y:''2,z)=[x.1,z,x.3]',line)
    call dcl_uproc(parser,'replace(x:tuple4d,y:''2,z)=[x.1,z,x.3,x.4]',line)
    call dcl_uproc(parser,'replace(x:tuple5d,y:''2,z)=[x.1,z,x.3,x.4,x.5]',line)
    call dcl_uproc(parser,'replace(x:tuple6d,y:''2,z)=[x.1,z,x.3,x.4,x.5,x.6]',line)
    call dcl_uproc(parser,'replace(x:tuple7d,y:''2,z)=[x.1,z,x.3,x.4,x.5,x.6,x.7]',line)
    call dcl_uproc(parser,'replace(x:tuple3d,y:''3,z)=[x.1,x.2,z]',line)
    call dcl_uproc(parser,'replace(x:tuple4d,y:''3,z)=[x.1,x.2,z,x.4]',line)
    call dcl_uproc(parser,'replace(x:tuple5d,y:''3,z)=[x.1,x.2,z,x.4,x.5]',line)
    call dcl_uproc(parser,'replace(x:tuple6d,y:''3,z)=[x.1,x.2,z,x.4,x.5,x.6]',line)
    call dcl_uproc(parser,'replace(x:tuple7d,y:''3,z)=[x.1,x.2,z,x.4,x.5,x.6,x.7]',line)
    call dcl_uproc(parser,'replace(x:tuple4d,y:''4,z)=[x.1,x.2,x.3,z]',line)
    call dcl_uproc(parser,'replace(x:tuple5d,y:''4,z)=[x.1,x.2,x.3,z,x.5]',line)
    call dcl_uproc(parser,'replace(x:tuple6d,y:''4,z)=[x.1,x.2,x.3,z,x.5,x.6]',line)
    call dcl_uproc(parser,'replace(x:tuple7d,y:''4,z)=[x.1,x.2,x.3,z,x.5,x.6,x.7]',line)
    call dcl_uproc(parser,'replace(x:tuple5d,y:''5,z)=[x.1,x.2,x.3,x.4,z]',line)
    call dcl_uproc(parser,'replace(x:tuple6d,y:''5,z)=[x.1,x.2,x.3,x.4,z,x.6]',line)
    call dcl_uproc(parser,'replace(x:tuple7d,y:''5,z)=[x.1,x.2,x.3,x.4,z,x.6,x.7]',line)
    call dcl_uproc(parser,'replace(x:tuple6d,y:''6,z)=[x.1,x.2,x.3,x.4,x.5,z]',line)
    call dcl_uproc(parser,'replace(x:tuple7d,y:''6,z)=[x.1,x.2,x.3,x.4,x.5,z,x.7]',line)
    call dcl_uproc(parser,'replace(x:tuple7d,y:''7,z)=[x.1,x.2,x.3,x.4,x.5,x.6,z]',line)

    call dcl_uproc(parser,'spread(x,y:tuple1d or ''1)=[x]',line)
    call dcl_uproc(parser,'spread(x,y:tuple2d or ''2)=[x,x]',line)
    call dcl_uproc(parser,'spread(x,y:tuple3d or ''3)=[x,x,x]',line)
    call dcl_uproc(parser,'spread(x,y:tuple4d or ''4)=[x,x,x,x]',line)
    call dcl_uproc(parser,'spread(x,y:tuple5d or ''5)=[x,x,x,x,x]',line)
    call dcl_uproc(parser,'spread(x,y:tuple6d or ''6)=[x,x,x,x,x,x]',line)
    call dcl_uproc(parser,'spread(x,y:tuple7d or ''7)=[x,x,x,x,x,x,x]',line)
    
    call dcl_uproc(parser,'+(x:tuple(num),y:tuple(num))=map($+,x,y)',line)
    call dcl_uproc(parser,'-(x:tuple(num),y:tuple(num))=map($-,x,y)',line)
    call dcl_uproc(parser,'*(x:tuple(num),y:tuple(num))=map($*,x,y)',line)
    call dcl_uproc(parser,'/(x:tuple(num),y:tuple(num))=map($/,x,y)',line)
    call dcl_uproc(parser,'**(x:tuple(num),y:tuple(num))=map($**,x,y)',line)
    call dcl_uproc(parser,'mod(x:tuple(num),y:tuple(num))=map($mod,x,y)',line)
    
    call dcl_uproc(parser,'+(x:tuple(num),y:num)=map_const($+,x,y)',line)
    call dcl_uproc(parser,'-(x:tuple(num),y:num)=map_const($-,x,y)',line)
    call dcl_uproc(parser,'*(x:tuple(num),y:num)=map_const($*,x,y)',line)
    call dcl_uproc(parser,'/(x:tuple(num),y:num)=map_const($/,x,y)',line)
    call dcl_uproc(parser,'**(x:tuple(num),y:num)=map_const($**,x,y)',line)
    call dcl_uproc(parser,'mod(x:tuple(num),y:num)=map_const($mod,x,y)',line)

    call dcl_uproc(parser,'max(x:tuple(real_num),y:tuple(real_num))=map($max,x,y)',line)
    call dcl_uproc(parser,'min(x:tuple(real_num),y:tuple(real_num))=map($min,x,y)',line)
    call dcl_uproc(parser,'max(x:tuple(real_num))=reduce($max,x)',line)
    call dcl_uproc(parser,'min(x:tuple(real_num))=reduce($min,x)',line)
    call dcl_uproc(parser,'sum(x:tuple(num))=reduce($+,x)',line)
    call dcl_uproc(parser,'prod(x:tuple(num))=reduce($*,x)',line)

    call dcl_uproc(parser,'sint(x:tuple(num))=map($sint,x)',line)
    call dcl_uproc(parser,'int(x:tuple(num))=map($int,x)',line)
    call dcl_uproc(parser,'sreal(x:tuple(num))=map($sreal,x)',line)
    call dcl_uproc(parser,'real(x:tuple(num))=map($real,x)',line)

    call dcl_uproc(parser,'string(x:tuple1d)="[ "++x.1++" ]"',line)
    call dcl_uproc(parser,'string(x:tuple2d)="[ "++x.1++", "++x.2++" ]"',line)
    call dcl_uproc(parser,'string(x:tuple3d)="[ "++x.1++", "++x.2++", "++x.3++" ]"',line)
    call dcl_uproc(parser,'string(x:tuple4d)="[ "++x.1++", "++x.2++", "++x.3++", "++x.4++" ]"',line)
    call dcl_uproc(parser,&
         'string(x:tuple5d)="[ "++x.1++", "++x.2++", "++x.3++", "++x.4++", "++x.5++" ]"',line)
    call dcl_uproc(parser,'string(x:tuple6d)="[ "++x.1++", "++x.2++", "++x.3++", "++x.4++", "++x.5++'//&
         '", "++x.6++" ]"',line)
    call dcl_uproc(parser,'string(x:tuple7d)="[ "++x.1++", "++x.2++", "++x.3++", "++x.4++", "++x.5++'//&
         '", "++x.6++", "++x.7++" ]"',line)
    
    ! *****************************************************
    ! RANGES AND SEQUENCES
    ! *****************************************************

    ! Treat null as empty sequence in some cases
    call dcl_uproc(parser,'in(x,y:null)=''false',line)
    call dcl_uproc(parser,'in(x:null,y:null)=''true',line)
    
    ! Range base type (might later expand to interface)
    call dcl_type(parser,'range_base is real_num',line)

    ! Single point sequence
    call dcl_type(parser,'single_point(t:range_base) is rec {_t:t}',line)
    call dcl_uproc(parser,'single_point(x)=new single_point {_t=x}',line)
    call dcl_uproc(parser,'low(x:single_point)=x._t',line)
    call dcl_uproc(parser,'high(x:single_point)=x._t',line)
    call dcl_uproc(parser,'step(x:single_point)=x._t',line)
    call dcl_uproc(parser,'width(x:single_point)=''1',line)
    call dcl_uproc(parser,'norm(x:single_point)=x',line)
    call dcl_uproc(parser,'#(x:single_point)=shape([''0..''0])',line)
    call dcl_uproc(parser,'_shp(x:single_point)=''0..''0',line)
    call dcl_uproc(parser,'dims(x:single_point)=[''1]',line)
    call dcl_uproc(parser,'size(x:single_point)=''1',line)
    call dcl_uproc(parser,'+(x:single_point,y:range_base)=new single_point {_t=x._t+y}',line)
    call dcl_uproc(parser,'-(x:single_point,y:range_base)=new single_point {_t=x._t-y}',line)
    call dcl_uproc(parser,'*(x:single_point,y:range_base)=new single_point {_t=x._t*y}',line)
    call dcl_uproc(parser,'_arb(x:single_point)=x._t',line)
    call dcl_uproc(parser,'in(x:range_base,y:single_point)=x==y._t',line)
    call dcl_uproc(parser,' inc(x:single_point,y:seq)=low(y)==x._t and high(y)==x._t',line)
    !call dcl_uproc(parser,' inc(x:seq,y:single_point)=y._t in x',line)
    call dcl_uproc(parser,'convert(x:single_point,y:range_base)=single_point(convert(x._t,y))',line)
    call dcl_uproc(parser,'sint(x:single_point)=single_point(sint(x._t))',line)
    call dcl_uproc(parser,'int(x:single_point)=single_point(int(x._t))',line)
    call dcl_uproc(parser,'sreal(x:single_point)=single_point(sreal(x._t))',line)
    call dcl_uproc(parser,'real(x:single_point)=single_point(real(x._t))',line)
    call dcl_uproc(parser,'#(x:single_point,y:index)=''0',line)
    call dcl_uproc(parser,'#(x:single_point,y:grid_slice_dim)=''0..''0',line)
    call dcl_uproc(parser,'#(x:single_point,y:single_point)=''0..''0',line)
    call dcl_uproc(parser,'#(x:grid_slice_dim,y:single_point)=xx..xx where xx=x#y._t',line)
    call dcl_uproc(parser,'overlap(x:single_point(any_int),y:single_point(any_int))=0..if(x._t==y._t=>0,-1)',line)
    call dcl_uproc(parser,'overlap(x:grid_slice_dim,y:single_point(any_int))=if(y._t in x=>x#y._t..x#y._t,0..-1)',line)
    call dcl_uproc(parser,'overlap(x:single_point(any_int),y:grid_slice_dim)=0..if(x._t in y=>0,-1)',line)
    call dcl_uproc(parser,'intersect(x:single_point(any_int),y:grid_slice_dim)=x._t..if(x._t in y=>x._t,x._t-1)',line)
    call dcl_uproc(parser,'intersect(y:grid_slice_dim,x:single_point(any_int))=x._t..if(x._t in y=>x._t,x._t-1)',line)
    call dcl_uproc(parser,'intersect(x:single_point(any_int),y:single_point(any_int))='//&
         'x._t..if(x._t==y._t=>x._t,x._t-1)',line)
 
    call dcl_uproc(parser,'_get_elem(x:single_point,y:index)=x._t',line)
    call dcl_uproc(parser,'_get_elem(x:single_point,y:subs)=x._t..x._t',line)
    call dcl_uproc(parser,'string(x:single_point)=string(x._t)',line)
    
    ! Range types
    call dcl_type(parser,'range(t:range_base) is rec {_lo:t,_hi:t,_n:t}',line)
    call dcl_uproc(parser,'..(x:range_base,y:range_base)='//&
         'new range {_lo=xx,_hi=yy,_n=max(0,int(yy-xx)+1)} where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,'..(x:fix int,y:fix int)='//&
         'new range {_lo=x,_hi=y,_n=max(''0,y-x+''1)}',line)
    call dcl_uproc(parser,'low(x:range)=x._lo',line)
    call dcl_uproc(parser,'high(x:range)=x._hi',line)
    call dcl_uproc(parser,'step(x:range)=convert(1,x._lo)',line)
    call dcl_uproc(parser,'width(x:range)=''1',line)
    call dcl_uproc(parser,'norm(x:range)=x',line)
    call dcl_uproc(parser,'#(x:range(int))=shape([0..x._n-1])',line)
    call dcl_uproc(parser,'_shp(x:range(int))=0..x._n-1',line)
    call dcl_uproc(parser,'dims(x:range(int))=[x._n]',line)
    call dcl_uproc(parser,&
         'size(x:range(int))=x._n',line)
    call dcl_uproc(parser,'+(x:range,y:range_base)=new range {_lo=x._lo+y,_hi=x._hi+y,_n=x._n}',line)
    call dcl_uproc(parser,'-(x:range,y:range_base)=new range {_lo=x._lo-y,_hi=x._hi-y,_n=x._n}',line)
!!$    call dcl_uproc(parser,'*(x:range,y:range_base)=new range {_lo=x._lo*y,_hi=x._hi*y,_n=x._n}',line)
    call dcl_uproc(parser,&
         '_arb(x:range)=low(x)',line)
    call dcl_uproc(parser,&
         'in(x:range_base,y:range())=x>=y._lo and x<=y._hi',line)
    call dcl_uproc(parser,'convert(x:range,y:range_base)='//&
         'new range {_lo=convert(x._lo,y),_hi=convert(x._hi,y),_n=x._n}',line)
    call dcl_uproc(parser,&
         'sint(x:range)=new range {_lo=sint(x._lo),_hi=sint(x._hi),_n=x._n}',line)
    call dcl_uproc(parser,&
         'int(x:range)=new range {_lo=int(x._lo),_hi=int(x._hi),_n=x._n}',line)
    call dcl_uproc(parser,&
         'sreal(x:range)=new range {_lo=sreal(x._lo),_hi=sreal(x._hi)}',line)
    call dcl_uproc(parser,'real(x:range)='//&
         'new range {_lo=real(x._lo),_hi=real(x._hi),_n=x._n}',line)
    call dcl_uproc(parser,&
         ' inc(x:range,y:seq())='//&
         ' low(y)>=x._lo and high(y)<=x._hi',line)
    call dcl_uproc(parser,&
         '_get_elem(x:range,y:int)=x._lo+convert(y,x._lo)',line)
    call dcl_uproc(parser,&
         '_get_elem(x:range(int),y:range(int))='//&
         '_get_elem(x,y._lo).._get_elem(x,y._hi)',line)
    call dcl_uproc(parser,&
         '_get_elem(x:range(int),y:seq(int))='//&
         '_get_elem(x,y._lo).._get_elem(x,y._hi) by y._st',line)
    call dcl_uproc(parser,&
         '_get_elem(x:range(int),y:range_above(int))='//&
         '_get_elem(x,y._t)..x._hi',line)
    call dcl_uproc(parser,&
         '_get_elem(x:range(int),y:range_below(int))='//&
         'x._lo.._get_elem(x,y._t)',line)
    call dcl_uproc(parser,&
         '_get_elem(x:range(int),y:strided_range_above(int))='//&
         '_get_elem(x,y._t)..x._hi by y._st',line)
    call dcl_uproc(parser,&
         '_get_elem(x:range(int),y:strided_range_below(int))='//&
         'x._lo .. _get_elem(x,y._t) by y._st',line)
    call dcl_uproc(parser,&
         '_get_elem(x:range(int),y:stride(int))=x by y._st',line)
    call dcl_uproc(parser,&
         '_get_elem(x:range,y:null)=x',line)
    call dcl_uproc(parser,&
         '#(y:range(int),x:int)=int(x-y._lo)',line)
    call dcl_uproc(parser,'#(y:range(int),x:range(int))='//&
         'int(x._lo-y._lo)..int(x._hi-y._lo)',line)
    call dcl_uproc(parser,'#(y:range(int),x:seq(int))='//&
         '_intseq(int(x._lo-y._lo),int(x._hi-y._lo), x._st)',line)
    call dcl_uproc(parser,&
         '#(y:range(int),x:range_below(int))='//&
         '0..int(x._t-y._lo)',line)
    call dcl_uproc(parser,&
         '#(y:range(int),x:range_above(int))='//&
         'int(x._t-y._lo)..size(y)-1',line)
    call dcl_uproc(parser,'#(y:range(int),x:strided_range_below(int))='//&
         '_intseq(0,int(x._t-y._lo),x._st)',line)
    call dcl_uproc(parser,'#(y:range(int),x:strided_range_above(int))='//&
         '_intseq(int(x._t-y._lo),size(y)-1,int(x._st))',line)
    call dcl_uproc(parser,'#(y:range(int),x:stride(int))='//&
         '_intseq(0,size(y),int(x._st))',line)
    call dcl_uproc(parser,'#(y:range(int),x:null)=0..size(y)',line)
    call dcl_uproc(parser,'intersect(x:range(int),y:range(int))='//&
         'max(y._lo,x._lo)..min(y._hi,x._hi)',line)
    call dcl_uproc(parser,'overlap(x:range(int),y:range(int))='//&
         'max(y._lo,x._lo)-x._lo..min(y._hi,x._hi)-x._lo',line)
    
    call dcl_uproc(parser,'expand(x:range(),y:range())='//&
         'x._lo+y._lo..x._hi+y._hi',line)
    call dcl_uproc(parser,'contract(x:range(),y:range())='//&
         'x._lo-y._lo..x._hi-y._hi',line)
    call dcl_uproc(parser,'empty(x:range)=new range {_lo=x._hi,_hi=x._lo,_n=0}',line)
    call dcl_uproc(parser,'string(x:range)=string(x._lo)++".."++(x._hi)',line)
    
    ! Strided range types
    call dcl_type(parser,&
         'strided_range(t) is rec {_lo:t,_hi:t,_st:t,_n:int}',line)
    call dcl_type(parser,&
         '_any_seq(t:range_base):indexable is strided_range(t), ... ',line)
    call dcl_type(parser,&
         '_any_seq(t:any_int) is ..., range(t),single_point(t)',line)
    call dcl_type(parser,'seq(t:range_base) is  _any_seq(t)',line)
    call dcl_uproc(parser,'_seq(lo,hi,st)=new strided_range {_lo=lo,_hi=lo+n*st,_st=st,_n=n}'//&
         'where n=max(0,int((hi-lo)/st))',line)
    call dcl_uproc(parser,'by(x:range(int),y:range_base)=_seq(lo,hi,st)'//&
         ' where hi=convert(x._hi,lo) where lo,st=balance(x._lo,y)',line)
    call dcl_uproc(parser,'by(x:seq,y:range_base)=_seq(lo,hi,st) where'//&
         ' lo=convert(x._lo,st),hi=convert(x._hi,st)'//&
         ' where st=x._st*y',line)
    call dcl_uproc(parser,'_intseq(x:int,y:int,st:int)='//&
         ' new strided_range {_lo=x,_hi=x+n*s,_st=s,_n=n}'//&
         ' where s=if(x>y=>-abs(st),abs(st)) where n=abs((y-x)/st)',line)
    call dcl_uproc(parser,'low(x:strided_range)=x._lo',line)
    call dcl_uproc(parser,'high(x:strided_range)=x._hi',line)
    call dcl_uproc(parser,'step(x:strided_range)=x._st',line)
    call dcl_uproc(parser,'size(x:strided_range)=x._n',line)
    call dcl_uproc(parser,'width(x:strided_range)=''1',line)
    call dcl_uproc(parser,'norm(x:strided_range)=min(lo,hi)..max(lo,hi) by abs(x._st)'//&
         'where hi=lo+(x._n-1)*x._st where lo=x._lo',line)
    call dcl_uproc(parser,'align(x:seq)=''0',line)
    call dcl_uproc(parser,'#(x:strided_range)=shape([0..x._n-1])',line)
    call dcl_uproc(parser,'_shp(x:strided_range)=0..x._n-1',line)
    call dcl_uproc(parser,'dims(x:strided_range)=[x._n]',line)
    call dcl_uproc(parser,'+(x:strided_range,y:range_base)='//&
         'new strided_range {_lo=x._lo+y,_hi=x._hi+y,_st=x._st,_n=x._n}',line)
    call dcl_uproc(parser,'-(x:strided_range,y:range_base)='//&
         'new strided_range {_lo=x._lo-y,_hi=x._hi-y,_st=x._st,_n=x._n}',line)
!!$    call dcl_uproc(parser,'*(x:strided_range,y:range_base)='//&
!!$         'new strided_range {_lo=x._lo*y,_hi=x._hi*y,_st=x._st*y,_n=x._n}',line)
    call dcl_uproc(parser,'_arb(x:seq)=x._lo',line)
    call dcl_uproc(parser,'convert(x:strided_range,y:range_base)='//&
         'new strided_range {_lo=convert(x._lo,y),_hi=convert(x._hi,y),'//&
         '_st=convert(x._st,y),_n=x._n}',line)
    call dcl_uproc(parser,'sint(x:strided_range)='//&
         'new strided_range {_lo=sint(x._lo),_hi=sint(x._hi),'//&
         '_st=sint(x._st),_n=x._n}',line)
    call dcl_uproc(parser,'int(x:strided_range)='//&
         'new strided_range {_lo=int(x._lo),_hi=int(x._hi),'//&
         '_st=int(x._st),_n=x._n}',line)
    call dcl_uproc(parser,'sreal(x:strided_range)='//&
         'new strided_range {_lo=sreal(x._lo),_hi=sreal(x._hi),'//&
         '_st=sreal(x._st),_n=x._n}',line)
    call dcl_uproc(parser,'real(x:strided_range)='//&
         'new strided_range {_lo=real(x._lo),_hi=real(x._hi),'//&
         '_st=real(x._st),_n=x._n}',line)
    call dcl_uproc(parser,&
         'in(x:int,y:strided_range(int))='//&
         'y._lo<=x and x<=y._hi and (x-y._lo) mod y._st==0',line)
    call dcl_uproc(parser,&
         'inc(x:strided_range(int),y:strided_range(int))='//&
         'y._lo in x and y._hi in x and (y._n==1 or y._lo+y._st in x)',line)
    call dcl_uproc(parser,&
         'inc(x:strided_range(int),y:range(int) or single_point(int))='//&
         'x inc low(y)..high(y) by 1',line)
    call dcl_uproc(parser,&
         '#(y:seq,x:range_base)=int((x-y._lo+y._a)/y._st)',line)
    call dcl_uproc(parser,&
         '#(y:seq,x:range)=y#x._lo..y#x._hi',line)
    call dcl_uproc(parser,&
         '#(y:seq,x:seq)=_intseq(lo,hi,int(x._st)) '//&
         '  where lo=y#x._lo,hi=y#x._hi',line)
    call dcl_uproc(parser,&
         '#(y:seq,x:range_below)=0..y#x._t',line)
    call dcl_uproc(parser,&
         '#(y:seq,x:range_above)=y#x._t..size(y)-1',line)
    call dcl_uproc(parser,&
         '#(y:seq,x:strided_range_below)='//&
         '_intseq(0,y#x._t,int((x._st+y._st/2)/y._st))',line)
    call dcl_uproc(parser,&
         '#(y:seq,x:strided_range_above)='//&
         '_intseq(y#x._t,size(y)-1,int((x._st+y._st/2)/y._st))',line)
    call dcl_uproc(parser,&
         '#(y:seq,x:stride)=_intseq(0,size(y),int((x._st+y._st/2)/y._st))',line)
    call dcl_uproc(parser,&
         '#(y:seq,x:null)=0..size(y)-1',line)
    call dcl_uproc(parser,'string(x:strided_range)=x._lo++".."++x._hi++" by "++x._st',line)
    
    call dcl_uproc(parser,&
         '_get_elem(x:strided_range,y:int)=x._lo+convert(y,x._lo)*x._st',line)
    call dcl_uproc(parser,&
         '_get_elem(x:strided_range,y:range(int))='//&
         '_seq(_get_elem(x,y._lo),_get_elem(x,y._hi),x._st)',line)
    call dcl_uproc(parser,&
         '_get_elem(x:strided_range,y:seq(int))='//&
         '_seq(_get_elem(x,y._lo),_get_elem(x,y._hi),'//&
         'convert(st*y._st,st)) where st=x._st',line)
    call dcl_uproc(parser,&
         '_get_elem(x:strided_range,y:range_above(int))='//&
         '_seq(_get_elem(x,y._t),x._hi,x._st)',line)
    call dcl_uproc(parser,&
         '_get_elem(x:strided_range,y:range_below(int))='//&
         '_seq(x._lo,_get_elem(x,y._t),x._st)',line)
    call dcl_uproc(parser,&
         '_get_elem(x:strided_range,y:strided_range_above(int))='//&
         '_seq(_get_elem(x,y._t),x._hi,convert(st*y._st,st)) where st=x._st',line)
    call dcl_uproc(parser,&
         '_get_elem(x:strided_range,y:strided_range_below(int))='//&
         '_seq(x._lo,_get_elem(x,y._t),convert(st*y._st,st)) where st=x._st',line)
    call dcl_uproc(parser,&
         '_get_elem(x:strided_range,y:stride(int))=x by y',line)
    call dcl_uproc(parser,&
         '_get_elem(x:strided_range,y:null)=x',line)

    call dcl_uproc(parser,&
         'overlap(x:strided_range(any_int),y:range(any_int))='//&
         ' max(0,(y._lo-x._lo+x._st-1)/x._st)..min(x._n,(y._hi-x._lo)/x._st)',line)
    call dcl_uproc(parser,&
         'overlap(x:range(any_int),y:strided_range(any_int))='//&
         'max((-d+y._st-1)/y._st*y._st+d,d)..min(x._n,y._hi-x._lo) by y._st where d=y._lo-x._lo',line) !!! WRONG
    call dcl_uproc(parser,&
         'intersect(x:strided_range(any_int),y:range(any_int))='//&
         'x._lo+max(convert(0,n1),n1)*x._st..x._lo+min(convert(x._n,n2),n2)*x._st by x._st'//&
         ' where n1=(y._lo-x._lo+x._st-1)/x._st,n2=(y._hi-x._lo)/x._st',line)
    call dcl_uproc(parser,&
         'intersect(x:range(any_int),y:strided_range(any_int))=intersect(y,x)',line)
    
    call dcl_proc(parser,&
         '_intersect_seq(int,int,int,int,int,int,int,int)->int,int,int,int',&
         op_intersect_seq,0,line,0)
    call dcl_uproc(parser,'intersect(x:strided_range(any_int),y:strided_range(any_int))='//&
         'new strided_range {_lo=lo,_hi=hi,_st=st,_n=n}'//&
         'where lo,hi,st,n='//&
         '_intersect_seq(int(x._lo),int(x._hi),int(x._st),int(x._n),'//&
         'int(y._lo),int(y._hi),int(y._st),int(y._n))',line)
    call dcl_uproc(parser,'overlap(x:strided_range(any_int),y:strided_range(any_int))='//&
         'new strided_range {_lo=(lo-x._lo)/x._st,_hi=(hi-x._lo)/x._st,_st=st/x._st,_n=n}'//&
         ' where lo,hi,st,n=_intersect_seq(int(x._lo),int(x._hi),int(x._st),int(x._n),'//&
         ' int(y._lo),int(y._hi),int(y._st),int(y._n))',line)
    call dcl_uproc(parser,'empty(x:strided_range(any_int))='//&
         'new strided_range {_lo=x._hi,_hi=x._lo,_st=x._st,_n=0}',line)
 
    ! Block sequence
    call dcl_type(parser,&
         'block_seq is rec { _lo:int,_hi:int,_st:int,_b:int,_n:int,_align:int}',line)
    call dcl_uproc(parser,'block_seq(lo:int,hi:int,st:int,b:int,align:int){'//&
         'test "Block sequence width must be non-negative: "++b=>b>=0;'//&
         'test "Block sequence width must be less than step: "++b++">="++st=>b<=st;'//&
         'test "Block sequence alignment must be less that width: "++align++">="++b=>align<st;'//&
         'return new block_seq {_lo=lo,_hi=min(hi,lo-align+nc*st+b-1),_st=st,_b=b,_n=n,_align=align}'//&
         ' where n=nc*b+max(0,min(b,df+1-nc*st))-align where nc=df/st where df=hi-lo+align}',line)
    call dcl_uproc(parser,'first_block(x:block_seq)=if(x._align==0=>1..0,x._lo..x._lo-x._align+x._b-1)',line)
    call dcl_uproc(parser,'last_block(x:block_seq)=low..min(low+x._b,x._hi) '//&
         'where low=x._lo-x._align+nb*x._st where nb=(x._hi-x._lo+x._align+x._st-x._b+1)/x._st',line)
    call dcl_uproc(parser,'middle_blocks(x:block_seq)=new block_seq {'//&
         '_lo=low,_hi=low+nb*x._st-1,_st=x._st,_b=x._b,_n=nb*x._b,_align=0}'//&
         'where nb=(x._hi-low+x._st-x._b+1)/x._st '//&
         'where low=if(x._align==0=>x._lo,x._lo-x._align+x._st)',line)
    call dcl_uproc(parser,&
         'string(x:block_seq)=x._lo++".."++x._hi++" by "++x._st++" width "++x._b++" align "++x._align',&
         line)
    call dcl_uproc(parser,'low(x:block_seq)=x._lo',line)
    call dcl_uproc(parser,'high(x:block_seq)=x._hi',line)
    call dcl_uproc(parser,'step(x:block_seq)=x._st',line)
    call dcl_uproc(parser,'width(x:block_seq)=x._b',line)
    call dcl_uproc(parser,'norm(x:block_seq)=x',line)
    call dcl_uproc(parser,'align(x:block_seq)=x._align',line)
    call dcl_uproc(parser,'#(x:block_seq)=shape([0..x._n-1])',line)
    call dcl_uproc(parser,'_shp(x:block_seq)=0..x._n-1',line)
    call dcl_uproc(parser,'dims(x:block_seq)=x._n',line)
    call dcl_uproc(parser,'size(x:block_seq)=x._n',line)
    call dcl_uproc(parser,'+(x:block_seq,y:int)='//&
         'new block_seq {_lo=x._lo+y,_hi=x._hi+y,_st=x._st,_b=x._b,_n=x._n,_align=x._align}',line)
    call dcl_uproc(parser,'-(x:block_seq,y:int)='//&
         'new block_seq {_lo=x._lo-y,_hi=x._hi-y,_st=x._st,_b=x._b,_n=x._n,_align=x._align}',line)
!!$    call dcl_uproc(parser,'*(x:block_seq,y:int)='//&   !!! Not sure this is right definition !!
!!$         'new block_seq {_lo=x._lo*y,_hi=x._hi*y,_st=x._st*y,_b=x._b*y,_n=x._n,_align=x._align*y}',line)
    call dcl_uproc(parser,'_arb(x:block_seq)=x._lo',line)
    call dcl_uproc(parser,'in(x:int,y:block_seq)='//&
         'x>=y._lo and x<=y._hi and (x-y._lo+y._align) mod y._st<y._b',line)
    call dcl_uproc(parser,&
         '#(y:block_seq,x:int)=nb*y._b+xx-nb*y._st '//&
         ' where nb=xx/y._st where xx=x-y._lo+y._align',line)
    call dcl_uproc(parser,&
         '_get_elem(x:block_seq,y:int)='//&
         'x._lo+y+((y+x._align)/x._b)*(x._st-x._b)',line)
    
    call dcl_uproc(parser,&
         'intersect(x:block_seq,y:range(any_int)){'//&
         ' var lo=max(x._lo,y._lo);var hi=min(x._hi,y._hi);'//&
         ' oldbase=x._lo-x._align;'//&
         ' nblo=((lo-oldbase)/x._st)*x._st+oldbase;'//&
         ' nbhi=((hi-oldbase)/x._st)*x._st+oldbase;'//&
         ' if lo-nblo>=x._b:lo:=nblo+x._st;'//&
         ' if hi-nbhi>=x._b:hi:=nbhi+x._b-1;'//&
         ' align=base-(base/x._st)*x._st  '//&
         '   where base=lo-oldbase;'//&
         ' return block_seq(lo,hi,x._st,x._b,align)'//&
         '}',line)
    call dcl_uproc(parser,'intersect(x:range(any_int),y:block_seq)=intersect(y,x)',line)
    call dcl_uproc(parser,'overlap(x:range(any_int),y:block_seq) {'//&
         'z=intersect(y,x);'//&
         'return block_seq(z._lo-x._lo,z._hi-x._lo,z._st,z._b,z._align)}',line)
    call dcl_uproc(parser,'overlap(x:block_seq,y:range(any_int)) {'//&
         'z=intersect(x,y);'//&
         'return start..start+size(z)-1 where start=z#z._lo}',line)
    call dcl_uproc(parser,'overlap(x:block_seq,y:range(any_int)) {'//&
         'z=intersect(x,y);'//&
         'return start..start+size(z)-1,'//&
         ' block_seq(z._lo-x._lo,z._hi-x._lo,z._st,z._b,z._align)'//&
         ' where start=z#z._lo}',line)
    call dcl_uproc(parser,'overlap(x:range(any_int),y:block_seq)=yy,xx'//&
         ' where xx,yy=overlap(x,y)',line)
    call dcl_uproc(parser,'empty(x:block_seq)=block_seq(1,0,1,1,0)',line)
    
    ! Mapped sequence
    call dcl_type(parser,'map_seq(t:array(int)) is rec {array:t}',line)
    call dcl_uproc(parser,'map_seq(x:grid_dim){var a=array(0,#x);for i in a,j in x <<conc>>:i:=j;'//&
         'return new map_seq{array=a}}',line)
    call dcl_uproc(parser,'map_seq(x:array(int,mshape1d))=new map_seq {array=x} '//&
         'check "Array for ""map_seq"" must be strictly increasing or stricly decreasing"=>_mono(x)',line)
    call dcl_uproc(parser,'_mono(x) {xs=#x;var ok=true;'//&
         'if x[low(xs.1)]<x[high(xs.1)] {for i in low(xs.1)+1..high(xs.1):if x[i]<x[i-1]:sync ok:=false} '//&
         'else{ for i in low(xs.1)+1..high(xs.1):if x[i]>x[i-1]:sync ok:=false};return ok}' ,line)
    
    call dcl_uproc(parser,'map_seq(x:map_seq)=x',line)

    call dcl_uproc(parser,'#(x:map_seq)=#(x.array)',line)
    call dcl_uproc(parser,'_shp(x:map_seq)=0..size(x.array)-1',line)
    call dcl_uproc(parser,'dims(x:map_seq)=size(x.array)',line)
    call dcl_uproc(parser,'size(x:map_seq)=size(x.array)',line)
    call dcl_uproc(parser,'+(x:map_seq,y:range_base)=new map_seq{array=x.array+y}',line)
    call dcl_uproc(parser,'-(x:map_seq,y:range_base)=new map_seq{array=x.array-y}',line)
    call dcl_uproc(parser,'*(x:map_seq,y:range_base)=new map_seq{array=x.array*y}',line)
    
    call dcl_uproc(parser,'_arb(x:map_seq)=_arb(x.array)',line)
    call dcl_uproc(parser,'_get_elem(x:map_seq,y:int)=_get_elem(x.array,y)',line)
    
    call dcl_proc(parser,'_intersect_aseq(&any,any,any,any,any,&any)',&
         op_intersect_aseq,0,line,0)
    call dcl_proc(parser,'_overlap_aseq(&any,any,any,any,any,&any)',&
         op_intersect_aseq,1,line,0)
    call dcl_proc(parser,'_overlap_aseq2(&any,any,any,any,any,&any,&any)',&
         op_intersect_aseq,2,line,0)
    call dcl_proc(parser,'_expand_aseq(&any,any,any,&any,any,any)',&
         op_expand_aseq,0,line,0)
    call dcl_proc(parser,'_intersect_bseq(&any,any,any,any,any,any,any,any,any,any,any,any)',&
         op_intersect_bseq,0,line,0)
    call dcl_proc(parser,'_overlap_bseq(&any,any,any,any,any,any,any,any,any,any,any,any)',&
         op_intersect_bseq,1,line,0)
    call dcl_proc(parser,'_overlap_bseq2(&any,any,any,any,any,any,any,any,any,any,any,any,any)',&
         op_intersect_bseq,2,line,0)
    call dcl_proc(parser,'_includes_aseq(any,any,any,any)->bool',&
         op_includes_aseq,0,line,0)
    call dcl_proc(parser,'_index_aseq(any,any,any)->int',&
         op_index_aseq,0,line,0)
    call dcl_proc(parser,'_in_aseq(any,any,any)->bool',&
         op_in_aseq,0,line,0)
    
!!$    call dcl_uproc(parser,'intersect(x:block_seq,y:block_seq) {'//&
!!$         'var a=array(0,[0..min(x._n,y._n)-1]);'//&
!!$         'var n=0;_intersect_bseq(&n,a,x._lo,x._hi,x._b,x._st,x._align,'//&
!!$         'y._lo,y._hi,y._b,y._st,y._align);'//&
!!$         'v=new map_seq {array=a[0..n-1]};return v}',line)
!!$
!!$    call dcl_uproc(parser,'overlap(x:block_seq,y:block_seq) {'//&
!!$         'var a=array(0,[0..min(x._n,y._n)-1]);'//&
!!$         'var n=0;_overlap_bseq(&n,a,x._lo,x._hi,x._b,x._st,x._align,'//&
!!$         'y._lo,y._hi,y._b,y._st,y._align);'//&
!!$         'v=new map_seq {array=a[0..n-1]};return v}',line)
!!$    
!!$    call dcl_uproc(parser,'overlap(x:block_seq,y:block_seq) {'//&
!!$         'm=[0..min(x._n,y._n)-1];var a=array(0,m);var b=array(0,m);'//&
!!$         'var n=0;_overlap_bseq2(&n,a,b,x._lo,x._hi,x._b,x._st,x._align,'//&
!!$         'y._lo,y._hi,y._b,y._st,y._align);ns=shape([0..n-1]);'//&
!!$         'v=new map_seq {array=a[ns]};w=new map_seq {array=b[ns]};return v,w}',line)

    call dcl_uproc(parser,'intersect(x:block_seq,y:block_seq)=intersect(map_seq(x),map_seq(y))',line)
    call dcl_uproc(parser,'overlap(x:block_seq,y:block_seq)=overlap(map_seq(x),map_seq(y))',line)

    call dcl_uproc(parser,'overlap(x:block_seq,y:block_seq)=v,w'//&
         ' where v,w=overlap(map_seq(x),map_seq(y))',line)

    
    call dcl_uproc(parser,'intersect(x:map_seq,y:map_seq) {'//&
         'var a=array(0,[0..max(0,min(size(x.array),size(y.array))-1)]);'//&
         'var n=0;_intersect_aseq(&n,x.array,size(x.array),y.array,size(y.array),&a);'//&
         'v=new map_seq {array=a[0..n-1]};return v}',line)
    
    call dcl_uproc(parser,'overlap(x:map_seq,y:map_seq) {'//&
         'var a=array(0,[0..max(0,min(size(x.array),size(y.array))-1)]);'//&
         'var n=0;_overlap_aseq(&n,x.array,size(x.array),y.array,size(y.array),&a);'//&
         'v=new map_seq {array=a[0..n-1]};return v}',line)
    
    call dcl_uproc(parser,'overlap(x:map_seq,y:map_seq) {'//&
         'var a=array(0,[0..max(0,min(size(x.array),size(y.array))-1)]);'//&
         'var b=array(0,[0..max(0,min(size(x.array),size(y.array))-1)]);'//&
         'var n=0;_overlap_aseq2(&n,x.array,size(x.array),y.array,size(y.array),&a,&b);'//&
         'ns=shape([0..n-1]);'//&
         'v=new map_seq {array=a[ns]};w=new map_seq {array=b[ns]};return v,w}',line)
   
    call dcl_uproc(parser,'overlap(x:seq,y:seq)=overlap(x,y),overlap(y,x)',line)

    call dcl_uproc(parser,&
         'expand(t:map_seq,i:range(any_int)) {'//&
         'var a=array(0,[0..max(1,size(t)*max(1,size(i))-1)]);var m=0;'//&
         '_expand_aseq(&m,t.array,size(t.array),&a,low(i),high(i));'//&
         'v=new map_seq {array=a[0..m-1]};return v}',line)

    call dcl_uproc(parser,'inc(x:map_seq,y:map_seq)=_includes_aseq(x.array,size(x.array),y.array,size(y.array))',line)
    call dcl_uproc(parser,'inc(x:map_seq,y:seq or block_seq)=x inc map_seq(y)',line)
    call dcl_uproc(parser,'inc(x:block_seq,y:block_seq)=map_seq(x) inc map_seq(y)',line)
    call dcl_uproc(parser,'inc(x:seq or block_seq,y:map_seq)=map_seq(x) inc y',line)
    call dcl_uproc(parser,'in(y:any_int,x:map_seq)=_in_aseq(x.array,size(x.array),int(y))',line)
    call dcl_uproc(parser,'#(x:map_seq,y:any_int)=_index_aseq(x.array,size(x.array),int(y))',line)

    call dcl_uproc(parser,'empty(x:map_seq) {a=array(0,[1..0]);return new map_seq {array=a}}',line)

    ! Grids (tuples of sequences)
    call dcl_type(parser,'grid_dim is seq,block_seq,map_seq',line)
    call dcl_type(parser,'grid1d(t1:grid_dim) is tuple(t1)',line)
    call dcl_type(parser,'grid2d(t1:grid_dim,t2:grid_dim) is tuple(t1,t2)',line)
    call dcl_type(parser,'grid3d(t1:grid_dim,t2:grid_dim,t3:grid_dim) is'//&
         ' tuple(t1,t2,t3)',line)
    call dcl_type(parser,'grid4d(t1:grid_dim,t2:grid_dim,t3:grid_dim,t4:grid_dim) is'//&
         ' tuple(t1,t2,t3,t4)',line)
    call dcl_type(parser,'grid5d(t1:grid_dim,t2:grid_dim,t3:grid_dim,t4:grid_dim,'//&
         't5:grid_dim) is'//&
         ' tuple(t1,t2,t3,t4,t5)',line)
    call dcl_type(parser,'grid6d(t1:grid_dim,t2:grid_dim,t3:grid_dim,t4:grid_dim,'//&
         't5:grid_dim,t6:grid_dim) is'//&
         ' tuple(t1,t2,t3,t4,t5,t6)',line)
    call dcl_type(parser,'grid7d(t1:grid_dim,t2:grid_dim,t3:grid_dim,t4:grid_dim,'//&
         't5:grid_dim,t6:grid_dim,t7:grid_dim) is'//&
         ' tuple(t1,t2,t3,t4,t5,t6,t7)',line)    
    call dcl_type(parser,'grid is tuple(grid_dim)',line)
    call dcl_uproc(parser,'_get_elem(x:grid_dim,y:tuple1d)=_get_elem(x,y.1)',line)
    call dcl_uproc(parser,'#(x:grid)=shape(map($_shp,x))',line)
    call dcl_uproc(parser,'+(x:grid,y:tuple(range_base))=map($+,x,y)',line)
    call dcl_uproc(parser,'empty(x:grid)=map($empty,x)',line)
    
    ! Slices of grids (may have dims that are just an integer and also _ or _() or null)
    call dcl_uproc(parser,'_shp(x:stretch_dim)=null',line)
    call dcl_uproc(parser,'_shp(x:null)=x',line)
    call dcl_uproc(parser,'_size(x:stretch_dim or null)=''1',line)
    call dcl_uproc(parser,'_size(x)=size(x)',line)
    call dcl_type(parser,'grid_slice_dim:indexable is grid_dim,single_point,null',line)
    call dcl_type(parser,'grid_slice:indexable is extent,tuple(grid_slice_dim)',line)
    call dcl_proc(parser,'_act(x:single_point)->PM__tinyint',op_miss_arg,0,line,0)
    call dcl_uproc(parser,'_act(x)=x',line)
    call dcl_uproc(parser,'_sliceit(arg...)=tuple(arg...)',line)
    call dcl_uproc(parser,'active_dims(x:grid_slice)=map_apply($_act,$_sliceit,x)',line)
    call dcl_uproc(parser,'active_dims(x:single_point)=null',line)
    call dcl_uproc(parser,'_ar(x:single_point)=''0',line)
    call dcl_uproc(parser,'_ar(x)=''1',line)
    call dcl_uproc(parser,'rank(x:grid_slice)=map_reduce($_ar,$+,x)',line)

    call dcl_uproc(parser,'_get_elem(x:grid_slice,arg...:index)'//&
         '{t=_tup(arg...);'//&
         'return _ges(head(x),tail(x),head(t),tail(t),''false)  }',line)
    call dcl_uproc(parser,'_get_elem(x:grid_slice,arg...:subs)'//&
         '{t=_tup(arg...);'//&
         'return _ges(head(x),tail(x),head(t),tail(t),''true)  }',line)

    call dcl_uproc(parser,'_get_elem(x:null,y)=null',line)
    call dcl_uproc(parser,'_spnt(i,y:''true)=i',line)
    call dcl_uproc(parser,'_spnt(i,y:''false)=i._t',line)
    call dcl_uproc(parser,'_ges(i:single_point,x,j,y,t)=prepend(_spnt(i,t),_ges(head(x),tail(x),j,y,t))',line)
    call dcl_uproc(parser,'_ges(i:empty_head,x,j,y,t)=error_type() :test "Rank mismatch in subscript" => ''false',line)
    call dcl_uproc(parser,'_ges(i,x,j,y,t)=prepend(_get_elem(i,j),_ges(head(x),tail(x),head(y),tail(y),t))',line)
    call dcl_uproc(parser,'_ges(i:null,x,j,y,t:''true)='//&
         'prepend(_get_elem(i,j),_ges(head(x),tail(x),head(y),tail(y),t))',line)
    call dcl_uproc(parser,'_ges(i:null,x,j,y,t:''false)=_ges(head(x),tail(x),head(y),tail(y),t)',line)
    call dcl_uproc(parser,'_ges(i:single_point,x,j:stretch_dim,y,t)=prepend(null,_ges(i,x,head(y),tail(y),t))',line)
    call dcl_uproc(parser,'_ges(i:empty_head,x,j:stretch_dim,y,t)=prepend(null,_ges(i,x,head(y),tail(y),t))',line)
    call dcl_uproc(parser,'_ges(i,x,j:stretch_dim,y,t)=prepend(null,_ges(i,x,head(y),tail(y),t))',line)
    call dcl_uproc(parser,'_ges(i:null,x,j:stretch_dim,y,t:''true)=prepend(null,_ges(i,x,head(y),tail(y),t))',line)
    call dcl_uproc(parser,'_ges(i:null,x,j:stretch_dim,y,t:''false)=prepend(null,_ges(i,x,head(y),tail(y),t))',line)
    call dcl_uproc(parser,'_ges(i:single_point,x,j:empty_head,y,t)=prepend(_spnt(i,t),_ges(head(x),tail(x),j,y,t))',line)
    call dcl_uproc(parser,'_ges(i:empty_head,x,j:empty_head,y,t)=null',line)
    call dcl_uproc(parser,'_ges(i,x,j:empty_head,y,t)=error_type() :test "Rank mismatch" => ''false',line)
    call dcl_uproc(parser,'_ges(i:null,x,j:empty_head,y,t:''true)=error_type() :test "Rank mismatch" => ''false',line)
    call dcl_uproc(parser,'_ges(i:null,x,j:empty_head,y,t:''false)=null',line)

    call dcl_uproc(parser,'size(x:grid_slice)=map_reduce($_size,$*,x)',line)
    call dcl_uproc(parser,'#(x:grid_slice)=shape(map($_shp,active_dims(x)))',line) !!!
    call dcl_uproc(parser,'dims(x:grid_slice)=map($_size,active_dims(x))',line)
    call dcl_uproc(parser,'_arb(x:grid_slice)=map($_arb,x)',line)
    call dcl_uproc(parser,'in(x:tuple(range_base),y:grid_slice)='//&
         'map_reduce($in,$and,x,y)',line)
    call dcl_uproc(parser,' inc(x:grid_slice,y:grid_slice)='//&
         'map_reduce($inc,$and,x,y)',line)
    call dcl_uproc(parser,'#(x:grid,y:tuple(subs_dim))='//&
         'map($#,x,y)',line)
    call dcl_proc(parser,'_acthash(x:single_point,y:any)->PM__tinyint',op_miss_arg,0,line,0)
    call dcl_uproc(parser,'_acthash(x,y)=x#y',line)
    call dcl_uproc(parser,'#(x:grid_slice,y:tuple(subs_dim) or grid_slice)='//&
         'map_apply($_acthash,$_sliceit,x,y)',line)
    call dcl_uproc(parser,'convert(x:grid_slice,y:real_num)=map_const($convert,x,y)',line)
    call dcl_uproc(parser,'sint(x:grid_slice)=map($sint,x)',line)
    call dcl_uproc(parser,'int(x:grid_slice)=map($int,x)',line)
    call dcl_uproc(parser,'sreal(x:grid_slice)=map($sreal,x)',line)
    call dcl_uproc(parser,'real(x:grid_slice)=map($real,x)',line)
    call dcl_uproc(parser,'low(x:grid_slice)=map($low,x)',line)
    call dcl_uproc(parser,'high(x:grid_slice)=map($high,x)',line)
    call dcl_uproc(parser,'overlap(x:grid_slice,y:grid_slice)=map($overlap,x,y)',line)
    call dcl_uproc(parser,'overlap(x:grid_slice,y:grid_slice)=u,v'//&
         ' where u,v=map($overlap,x,y)',line)
    call dcl_uproc(parser,'intersect(x:grid_slice,y:grid_slice)=map($intersect,x,y)',line)
    call dcl_uproc(parser,'intersect(x:grid_slice_dim,y:grid_slice_dim)=intersect(map_seq(x),map_seq(y))',line)
    call dcl_uproc(parser,'overlap(x:grid_slice_dim,y:grid_slice_dim)=overlap(map_seq(x),map_seq(y))',line)
    call dcl_uproc(parser,'overlap(x:grid_slice_dim,y:grid_slice_dim)=u,v where u,v=overlap(map_seq(x),map_seq(y))',line)
    call dcl_uproc(parser,'intersect_aligned(x:grid,y:grid)=map($intersect_aligned,x,y)',line)
    call dcl_uproc(parser,'expand(x:grid_slice,y:grid)='//&
         'map($expand,x,y)',line)
    call dcl_uproc(parser,'contact(x:grid_slice,y:grid)='//&
         'map($contract,x,y)',line)
        
    call dcl_proc(parser,'gcd(x:int,y:int)->int',op_gcd,0,line,0)

   ! *****************************************************
    ! SHAPES
    ! *****************************************************

    call dcl_type(parser,'extent is tuple(range(int) ),'//&
         'extent1d,extent2d,extent3d,extent4d,extent5d,extent6d,extent7d',line)

    call dcl_type(parser,'extent1d is tuple1d_of(range(int))',line)
    call dcl_type(parser,'extent2d is tuple2d_of(range(int))',line)
    call dcl_type(parser,'extent3d is tuple3d_of(range(int))',line)
    call dcl_type(parser,'extent4d is tuple4d_of(range(int))',line)
    call dcl_type(parser,'extent5d is tuple5d_of(range(int))',line)
    call dcl_type(parser,'extent6d is tuple6d_of(range(int))',line)
    call dcl_type(parser,'extent7d is tuple7d_of(range(int))',line)
    call dcl_type(parser,'mshape(extent_t:extent) is '//&
         'rec {use _extent:extent_t,_n:int,_o:int}',line)
    
    call dcl_type(parser,'mshape1d is mshape(extent1d)',line)
    call dcl_type(parser,'mshape2d is mshape(extent2d)',line)
    call dcl_type(parser,'mshape3d is mshape(extent3d)',line)
    call dcl_type(parser,'mshape4d is mshape(extent4d)',line)
    call dcl_type(parser,'mshape5d is mshape(extent5d)',line)
    call dcl_type(parser,'mshape6d is mshape(extent6d)',line)
    call dcl_type(parser,'mshape7d is mshape(extent7d)',line)

    ! Create array, vector and matrix shapes
    call dcl_uproc(parser,'_low(x)=low(x)',line)
    call dcl_uproc(parser,'_low(x:null)=''0',line)
    call dcl_uproc(parser,'_off(x)=-index(dims(x),map($_low,x))',line)
    call dcl_uproc(parser,'PM__array(arg...)=shape(map($_extnt,[arg...]))',line)
    call dcl_uproc(parser,'_extnt(n:any_int)=0..int(n)-1',line)
    call dcl_uproc(parser,'_extnt(n:null)=null',line)
    call dcl_uproc(parser,'_extnt(n:range(any_int))=int(n)',line)
    
    call dcl_uproc(parser,'shape(extent:extent)='//&
         'new mshape {_extent=extent,_n=size(extent),_o=_off(extent)}',line)
    
    ! Conforming mshapes
    call dcl_uproc(parser,'check_conform(x,y) {check_conform(#x,#y)}',line)
    call dcl_uproc(parser,'check_conform(x:mshape,y:mshape) {'//&
         ' test "Mshapes "++x++" and "++y++" do not conform"=>'//&
         'conform(x,y)}',line)
    call dcl_uproc(parser,'_conform(x,y)=size(x)==size(y)',line)
    call dcl_uproc(parser,'_conform(x:null,y)=size(y)==''1',line)
    call dcl_uproc(parser,'_conform(x,y:null)=''true',line)
    call dcl_uproc(parser,'_conform(x:null,y:null)=''true',line)
    call dcl_uproc(parser,'conform(x:mshape,y:mshape)='//&
         'map_reduce($_conform,$and,x,y) or size(x)==0 or size(y)==0'//&
         'check "Values of different ranks cannot conform"=>rank(x)==rank(y)',line)

    ! Local size of a mshape
    call dcl_uproc(parser,'_local_size(x:mshape)=size(x._extent)',line)

    ! Extent of a mshape
    call dcl_uproc(parser,'extent(x:shape)=x._extent',line)
    
    ! Dimensions of a mshape
    call dcl_uproc(parser,'dims(x:mshape)=map($size,x._extent)',line)
   
    ! Size from dimensions
    call dcl_uproc(parser,'size(x:tuple(int))=reduce($*,x)',line)

    ! Empty mshape
    call dcl_uproc(parser,'_empty(x)=1..0',line)
    call dcl_uproc(parser,'empty(x:extent)=map($_empty,x)',line)
  
    ! *****************************************************
    ! INDEXING AND SLICING
    ! *****************************************************

    ! Generic types supporting indexing and mapping
    call dcl_type(parser,'indexable is grid_slice,grid,seq,array,...',line)
    call dcl_uproc(parser,'[](x:indexable,arg...)'//&
         '{y=_tup(arg...);check_contains(#x,y);return _get_elem(x,y)}',line)
    
    ! Index type
    call dcl_type(parser,'index is any_int,tuple(any_int)',line)

    ! Slice and subscript types
    call dcl_type(parser,'range_below(x) is rec {_t:x}',line)
    call dcl_type(parser,'range_above(x) is rec {_t:x}',line)
    call dcl_type(parser,&
         'strided_range_below(x) is rec {_t:x,_st:x}',line)
    call dcl_type(parser,&
         'strided_range_above(x) is rec {_t:x,_st:x}',line)
    call dcl_type(parser,'stride(x) is rec {_st:x}',line)
    call dcl_type(parser,'slice_dim is'//&
         ' range(any_int),strided_range(any_int),range_above(any_int),'//&
         ' range_below(any_int),strided_range_above(any_int),strided_range_below(any_int),'//&
         ' stride(any_int),null,stretch_dim',&
         line)
    call dcl_type(parser,'slice is slice_dim,tuple(slice_dim)'&
         ,line)
    call dcl_type(parser,'subs_dim is slice_dim,int',line)
    call dcl_type(parser,'subs is index,slice,subs_dim,tuple(subs_dim)',line)
    

    ! Partial ranges/sequences mainly used in subscripts
    call dcl_uproc(parser,'..._(x)=new range_below {_t=x}',line)
    call dcl_uproc(parser,'_...(x)=new range_above {_t=x}',line)
    call dcl_uproc(parser,'by(x:range_base)=new stride {_st=x}',line)
    call dcl_uproc(parser,'by(x:range_above(),y)='//&
         'new strided_range_above {_t=x._t,_st=convert(y,x._t)}',line)
    call dcl_uproc(parser,'by(x:range_below(),y)='//&
         'new strided_range_below {_t=x._t,_st=convert(y,x._t)}',line)
    call dcl_uproc(parser,'string(x:range_above)=x._t++"..."',line)
    call dcl_uproc(parser,'string(x:range_below)="..."++x._t',line)
    call dcl_uproc(parser,'string(x:strided_range_above)=x._t++"... by"++x._st',line)
    call dcl_uproc(parser,'string(x:strided_range_below)="..."++x._t++"by "++x._st',line)
    call dcl_uproc(parser,'string(x:stride)="by "++x._st',line)
    call dcl_uproc(parser,'low(x:range_above)=x._t',line)
    call dcl_uproc(parser,'low(x:strided_range_above)=x._t',line)
    call dcl_uproc(parser,'high(x:range_below)=x._t',line)
    call dcl_uproc(parser,'high(x:strided_range_below)=x._t',line)
    call dcl_uproc(parser,'step(x:range_above or range_below)=''1',line)
    call dcl_uproc(parser,'step(x:strided_range_above or strided_range_below)=x._st',line)
    call dcl_uproc(parser,'width(x:strided_range_above or strided_range_below or '//&
         'range_above or range_below)=''1',line)

    ! Stretch dimension in subscript
    call dcl_type(parser,'stretch_dim is unique{PM__strdim}',line)
    call dcl_uproc(parser,'string(x:stretch_dim)="_"',line)
    call dcl_uproc(parser,'size(x:stretch_dim)=''1',line)
    call dcl_uproc(parser,'expand(x:stretch_dim,y:grid)=x',line)
    call dcl_uproc(parser,'contract(x:stretch_dim,y:grid)=x',line)
    call dcl_uproc(parser,'in(x:stretch_dim,y)=''true',line)
    call dcl_uproc(parser,' inc(x:stretch_dim,y)=''true',line)
    call dcl_uproc(parser,'convert(x:stretch_dim,y:range_base)=x',line)
    call dcl_uproc(parser,'#(x:stretch_dim,y:index)=''0',line)
    call dcl_uproc(parser,'#(x:stretch_dim,y:grid_slice_dim)=''0..''0',line)
    call dcl_uproc(parser,'intersect(x:stretch_dim,y:grid_slice_dim)=y',line)
    call dcl_uproc(parser,'intersect(x:grid_slice_dim,y:stretch_dim)=x',line)
    call dcl_uproc(parser,'intersect(x:stretch_dim,y:stretch_dim)=x',line)
    call dcl_uproc(parser,'overlap(x:grid_slice_dim,y:stretch_dim)=#x',line)
    call dcl_uproc(parser,'overlap(x:stretch_dim,y:grid_slice_dim)=''0..''0',line)
    call dcl_uproc(parser,'overlap(x:stretch_dim,y:stretch_dim)=''0..''0',line)
    
    ! Check subscript is in range
    call dcl_uproc(parser,'check_contains(a:extent,arg...) '//&
         '{test "Index "++t++" out of bounds "++a=>contains(a,t) where t=_tup(arg...)}',line)
    call dcl_uproc(parser,'check_contains(a:mshape,arg...) {check_contains(a._extent,arg...)}',line)
    call dcl_uproc(parser,'check_contains(a,arg...) {check_contains(#a,arg...)}',line)
    call dcl_uproc(parser,'check_contains(a:dshape,arg...) {check_contains(a._mshape._extent,arg...)}',line)
    call dcl_uproc(parser,'_contains(x:null,y)=''true',line)
    call dcl_uproc(parser,'_contains(x:range(int),y:any_int)=yy>=x._lo and yy<=x._hi where yy=int(y)',line)    
    call dcl_uproc(parser,'_contains(x:range(int),y:range(any_int) or seq(any_int))='//&
         '_contains(x,y._lo) and _contains(x,y._hi) or y._lo>y._hi',line)
    call dcl_uproc(parser,'_contains(x:range(int),y:range_below(any_int) or '//&
         'strided_range_below(any_int) or range_above(any_int) or '//&
         'strided_range_above(any_int))=_contains(x,y._t)',line)
    call dcl_uproc(parser,'_contains(x:range(int),y:stride(any_int))=''true',line)
    call dcl_uproc(parser,'_contains(x:range(int),y:null)=''true',line)
    call dcl_uproc(parser,'contains(x:mshape1d,y:subs_dim)='//&
         '_contains(x.1,y)',line)
    call dcl_uproc(parser,'contains(x:extent,y:tuple(subs_dim))='//&
         'map_reduce($_contains,$and,x,y)',line)
    call dcl_proc(parser,'_rgd(x:stretch_dim)->PM__tinyint',op_miss_arg,0,line,0)
    call dcl_uproc(parser,'_rgd(x)=x',line)
    call dcl_uproc(parser,'_rigid_dims(x:grid_slice or tuple(subs_dim))='//&
         'map_apply($_rgd,$_sliceit,x)',line)
    call dcl_uproc(parser,'contains(x:extent,y:tuple(subs_dim)'//&
         ' and contains(stretch_dim))=contains(x,_rigid_dims(y))',line)
    call dcl_uproc(parser,'contains(x:extent,y,arg...)=contains(x,[y,arg...])',line)

    ! Complete a subscript using a base mshape
    call dcl_uproc(parser,&
         'fill_in(x:null,y)=y :test "Cannot use incomplete subscript on null dimension" => ''false',line)
    call dcl_uproc(parser,'fill_in(x:seq(int) or null,y:any_int)=single_point(int(y))',line)
    call dcl_uproc(parser,'fill_in(x:seq(int) or null,y:range(any_int))=int(y)',line)
    call dcl_uproc(parser,'fill_in(x:seq(int) or null,y:strided_range(any_int))=int(y)',line)
    call dcl_uproc(parser,'fill_in(x:seq(int),y:range_below(any_int))=x._lo..int(y._t)',line)
    call dcl_uproc(parser,'fill_in(x:seq(int),y:range_above(any_int))=int(y._t)..x._hi',line)
    call dcl_uproc(parser,'fill_in(x:seq(int),y:strided_range_below(any_int))=lo..int(y._t) by y._st'//&
         ' where lo=y._t-(y._t-x._lo)/y._st*y._st',line)
    call dcl_uproc(parser,'fill_in(x:seq(int),y:strided_range_above(any_int))=int(y._t)..x._hi by y._st',line)
    call dcl_uproc(parser,'fill_in(x:seq(int),y:stride(any_int))=x by int(y._st)',line)
    call dcl_uproc(parser,'fill_in(x:seq(int) or null,y:null)=x',line)
    call dcl_uproc(parser,'fill_in(x:grid,y:tuple(subs_dim))=map($fill_in,x,y)',line)
    call dcl_uproc(parser,'fill_in(x:grid,y:tuple(subs_dim) and contains(stretch_dim))='//&
         '_fill_in(x,head(y),tail(y))',line)
    call dcl_uproc(parser,'_fill_in(x,y,z)=prepend(fill_in(x.1,y),'//&
         '_fill_in(tail(x),head(z),tail(z)))',line)
    call dcl_uproc(parser,'_fill_in(x,y:stretch_dim,z)='//&
         'prepend(null,_fill_in(x,head(z),tail(z)))',line)
    call dcl_uproc(parser,'_fill_in(x:null,y:empty_head,z)=null',line)
    call dcl_uproc(parser,&
         '_fill_in(x:empty_head,y:stretch_dim,z)=prepend(null,_fill_in(x,head(z),tail(z)))',line)
    call dcl_uproc(parser,'_fill_in(x:empty_head,y,z)=error_type() :test "Rank mismatch in slice" => ''false',line)

    ! *******************************************************
    ! SUBSCRIPT INTERSECTION AND ALIASING
    ! *******************************************************
    
    ! Test for intersection between two subscripts
    call dcl_uproc(parser,'intersects(x:null,y:subs_dim)=''true',line)
    call dcl_uproc(parser,'intersects(x:subs_dim,y:null)=''true',line)
    call dcl_uproc(parser,'intersects(x:null,y:null)=''true',line)
    call dcl_uproc(parser,'intersects(x:range(any_int),y:range(any_int))='//&
         'not(x._hi<y._lo or x._lo>y._hi)',line)
    call dcl_uproc(parser,'intersects(x:seq(any_int),y:seq(any_int))=size(intersect(x,y))>0',line)
    call dcl_uproc(parser,'intersects(x:range(any_int),'//&
         'y:range_above(any_int) or strided_range_above(any_int))=x._hi>=y._t',line)
    call dcl_uproc(parser,'intersects(x:range_above(any_int) or strided_range_above(any_int),'//&
         'y:range(any_int))=y._hi>=x._t',line)
    call dcl_uproc(parser,'intersects(x:range(any_int),'//&
         'y:range_below(any_int) or strided_range_below(any_int))=x._lo<=y._t',line)
    call dcl_uproc(parser,'intersects(x:range_below(any_int) or strided_range_below(any_int),'//&
         'y:range(any_int))=y._lo<=x._t',line)
    call dcl_uproc(parser,'intersects(x:strided_range(any_int),'//&
         'y:range_above(any_int) or strided_range_above(any_int))='//&
         'intersects(y._t..x._hi by step(y),x)',line)
    call dcl_uproc(parser,'intersects(y:range_above(any_int) or strided_range_above(any_int),'//&
         'x:strided_range(any_int))=intersects(y._t..x._hi by step(y),x)',line)
    call dcl_uproc(parser,'intersects(x:strided_range(any_int),'//&
         'y:range_below(any_int) or strided_range_below(any_int))='//&
         'intersects(x,y._t..x._lo by -step(y))',line)
    call dcl_uproc(parser,'intersects(y:range_below(any_int) or strided_range_below(any_int),'//&
         'x:strided_range(any_int))='//&
         'intersects(x,y._t..x._lo by -step(y))',line)
    call dcl_uproc(parser,'intersects(x:range_below(any_int) or strided_range_below(any_int),'//&
         'y:range_above(any_int) or strided_range_above(any_int))=x._t>=y._t',line)
    call dcl_uproc(parser,'intersects(x:range_above(any_int) or strided_range_above(any_int),'//&
         'y:range_below(any_int) or strided_range_below(any_int))=y._t>=x._t',line)
    call dcl_uproc(parser,'intersects(x:range_above(any_int) or strided_range_above(any_int),'//&
         'y:range_above(any_int) or strided_range_above(any_int))=''true',line)
    call dcl_uproc(parser,'intersects(x:range_below(any_int) or strided_range_below(any_int),'//&
         'y:range_below(any_int) or strided_range_below(any_int))=''true',line)
    call dcl_uproc(parser,'intersects(x:strided_range_below(any_int),'//&
         'y:strided_range_above(any_int))='//&
         'size(intersect(y._t..x._t by y._st,x._t..y._t by -x._st))>0',line)
    call dcl_uproc(parser,'intersects(y:strided_range_above(any_int),'//&
         'x:strided_range_below(any_int))='//&
         'size(intersect(y._t..x._t by y._st,x._t..y._t by -x._st))>0',line)
    call dcl_uproc(parser,'intersects(x:strided_range_above(any_int),'//&
         'y:strided_range_above(any_int))=abs(x._t-y._t) mod gcd(int(x._st),int(y._st))==0',&
         line)
    call dcl_uproc(parser,'intersects(x:strided_range_below(any_int),'//&
         'y:strided_range_below(any_int))='//&
         'abs(x._t-y._t) mod gcd(abs(int(x._st)),abs(int(y._st)))==0',&
         line)
    call dcl_uproc(parser,'intersects(x:stride(any_int),y:subs_dim)=''true',line)
    call dcl_uproc(parser,'intersects(x:subs_dim,y:stride(any_int))=''true',line)
    call dcl_uproc(parser,'intersects(x:stride(any_int),y:stride(any_int))=''true',line)
    
    call dcl_uproc(parser,'intersects(x:seq,y:int)=y in x',line)
    call dcl_uproc(parser,'intersects(x:int,y:seq)=x in y',line)
    call dcl_uproc(parser,'intersects(x:int,y:int)=x==y',line)
    
    call dcl_uproc(parser,'intersects(x:int,y:range_above(any_int))=x>=y._t',line)
    call dcl_uproc(parser,'intersects(x:int,y:range_below(any_int))=x<=y._t',line)
    call dcl_uproc(parser,'intersects(x:int,y:strided_range_above(any_int))='//&
         'x>=y._t and (x-y._t) mod y._st==0',line)
    call dcl_uproc(parser,'intersects(x:int,y:strided_range_below(any_int))='//&
         'x<=y._t and (y._t-x) mod y._st==0',line)
    call dcl_uproc(parser,'intersects(y:range_above(any_int),x:int)=x>=y._t',line)
    call dcl_uproc(parser,'intersects(y:range_below(any_int),x:int)=x<=y._t',line)
    call dcl_uproc(parser,'intersects(y:strided_range_above(any_int),x:int)='//&
         'x>=y._t and (x-y._t) mod y._st==0',line)
    call dcl_uproc(parser,'intersects(y:strided_range_below(any_int),x:int)='//&
         'x<=y._t and (y._t-x) mod y._st==0',line)
    call dcl_uproc(parser,'_intersects(x,y,z:''true)=map_reduce($intersects,$and,x,y)',line)
    call dcl_uproc(parser,'_intersects(x,y,z:''false)=''false',line)
    call dcl_uproc(parser,'intersects(x:tuple(subs_dim except stretch_dim),'//&
         'y:tuple(subs_dim except stretch_dim))=_intersects(x,y,rank(x)==rank(y))',line)
    call dcl_uproc(parser,'intersects(x:tuple(subs_dim),y:tuple(subs_dim))='//&
         '_intersects(rx,ry,rank(rx)==rank(ry))'//&
         'where rx=_rigid_dims(x),ry=_rigid_dims(y)',line)
    call dcl_uproc(parser,'intersects(x:tuple(subs_dim),y:null)=''true',line)
    call dcl_uproc(parser,'intersects(x:null,y:tuple(subs_dim))=''true',line)

    call dcl_uproc(parser,'_intersects(x:subs,y:subs)=intersects(x,y)',line)
    call dcl_uproc(parser,'_intersects(x,y)=''false',line)
    
    ! Alias checking
    call dcl_uproc(parser,'PM__check_alias(i,j,x,y) {'//&
         'test "Aliasing error between arguments #"++i++" and #"++j=>not _intersects(x,y) }',line)
    call dcl_uproc(parser,'PM__check_alias(i,j,x,y,arg...) {'//&
         'if _intersects(x,y):PM__check_alias(i,j,arg...)}',line)

    ! Combining subscripts
    call dcl_uproc(parser,'PM__cmbidx(x,y)=_cmb(x,y)',line)
    call dcl_uproc(parser,'PM__cmbidx(x,y,arg...)=PM__cmbidx(_cmb(x,y),arg...)',line)
    call dcl_type(parser,'_cmb_error is unique',line)
    call dcl_uproc(parser,'_cmb(x,y)=_cmb_error',line)
    call dcl_uproc(parser,'_cmb(x:subs except index,y:subs)=_cmb1(x,y)',line)
    call dcl_uproc(parser,'_cmb1(x,y)=_cmb_error',line)
    call dcl_uproc(parser,'_cmb1(x:subs_dim,y:subs_dim)=x[y]',line)
    call dcl_uproc(parser,'_cmb1(x:tuple,y:tuple)=_cmb2(x,y,rank(x)==rank(y))',line)
    call dcl_uproc(parser,'_cmb2(x,y,z:''true)=x[y]',line)
    call dcl_uproc(parser,'_cmb2(x,y,z:''false)=_cmb_error',line)
    
    ! *******************************************************
    ! ITERATION - SEQUENTIAL AND CONCURRENT
    ! *******************************************************
    
    ! Iteration over mshape

    ! - first element
    call dcl_uproc(parser,'PM__first(d:int)=0,null,d>0',line)
    call dcl_uproc(parser,'PM__first(d:tuple1d)='//&
         'i,s,e where i,s,e=PM__first(d.1)',line)
    call dcl_uproc(parser,'PM__first(d:tuple2d)='//&
         '[j1,j2],[s1,s2],e1 and e2'//&
         ' where j1,s1,e1=PM__first(d.1),j2,s2,e2=PM__first(d.2)',line)
    call dcl_uproc(parser,'PM__first(d:tuple3d)='//&
         '[j1,j2,j3],[s1,s2,s3],e1 and e2 and e3'//&
         ' where j1,s1,e1=PM__first(d.1),'//&
         'j2,s2,e2=PM__first(d.2),j3,s3,e3=PM__first(d.3)',&
         line)
    call dcl_uproc(parser,'PM__first(d:tuple4d)='//&
         '[j1,j2,j3,j4],[s1,s2,s3,s4],'//&
         'e1 and e2 and e3 and e4'//&
         ' where j1,s1,e1=PM__first(d.1),j2,s2,e2=PM__first(d.2),'//&
         'j3,s3,e3=PM__first(d.3),j4,s4,e4=PM__first(d.4)',line)
    call dcl_uproc(parser,'PM__first(d:tuple5d)=[j1,j2,j3,j4,j5],'//&
         ' [s1,s2,s3,s4,s5],'//&
         'e1 and e2 and e3 and e4 and e5'//&
         ' where j1,s1,e1=PM__first(d.1),j2,s2,e2=PM__first(d.2),j3,s3,'//&
         'e3=PM__first(d.3),j4,s4,e4=PM__first(d.4),'//&
         ' j5,s5,e5=PM__first(d.5)',line)
    call dcl_uproc(parser,'PM__first(d:tuple6d)=[j1,j2,j3,j4,j5,j6],'//&
         '[s1,s2,s3,s4,s5,s6],'//&
         'e1 and e2 and e3 and e4 and e5 and e6'//&
         ' where j1,s1,e1=PM__first(d.1),j2,s2,e2=PM__first(d.2),'//&
         'j3,s3,e3=PM__first(d.3),j4,s4,e4=PM__first(d.4),'//&
         ' j5,s5,e5=PM__first(d.5),j6,s6,e6=PM__first(d.6)',line)
    call dcl_uproc(parser,'PM__first(d:tuple7d)=[j1,j2,j3,j4,j5,j6,j7],'//&
         '[s1,s2,s3,s4,s5,s6,s7],'//&
         'e1 and e2 and e3 and e4 and e5 and e6 and e7'//&
         ' where j1,s1,e1=PM__first(d.1),j2,s2,'//&
         'e2=PM__first(d.2),j3,s3,e3=PM__first(d.3),'//&
         'j4,s4,e4=PM__first(d.4),'//&
         ' j5,s5,e5=PM__first(d.5),'//&
         'j6,s6,e6=PM__first(d.6),j7,s7,e7=PM__first(d.7)',line)

    ! - subsequent elements
    call dcl_uproc(parser,'PM__next(d:int,g,i)=ii,null,ii<d where ii=i+1',line)
    
    call dcl_uproc(parser,&
         'PM__next(d:tuple1d,g,i)=j,s,e where j,s,e=PM__next(d.1,g,i)',line)
    call dcl_uproc(parser,'PM__next(d:tuple2d,g:tuple2d,i:tuple2d){ '//&
         'var j1, s1, e=PM__next(d.1,g.1,i.1);var j2=i.2;var s2=g.2;'//&
         'if not e { '//&
         '  j1,s1,_:=PM__first(d.1);j2,s2,e:=PM__next(d.2,g.2,i.2) };'//&
         'return [j1,j2],[s1,s2],e}',line)
    call dcl_uproc(parser,&
         'PM__next(d:tuple3d,g:tuple3d,i:tuple3d){ '//&
         'var j1, s1, e=PM__next(d.1,g.1,i.1);'//&
         'var j2=i.2;var s2=g.2;var j3=i.3;var s3=g.3;'//&
         'if not e { j1,s1,_:=PM__first(d.1);'//&
         'j2,s2,e:=PM__next(d.2,g.2,i.2) };'//&
         'if not e { j2,s2,_:=PM__first(d.2);'//&
         'j3,s3,e:=PM__next(d.3,g.3,i.3) };'//&
         'return [j1,j2,j3],[s1,s2,s3],e}',line)
    call dcl_uproc(parser,&
         'PM__next(d:tuple4d,g:tuple4d,i:tuple4d){ '//&
         'var j1, s1, e=PM__next(d.1,g.1,i.1);var j2=i.2;var s2=g.2;'//&
         'var j3=i.3;var s3=g.3;var j4=i.4;var s4=g.4;'//&
         'if not e { j1,s1,_:=PM__first(d.1);'//&
         'j2,s2,e:=PM__next(d.2,g.2,i.2) };'//&
         'if not e { j2,s2,_:=PM__first(d.2);'//&
         'j3,s3,e:=PM__next(d.3,g.3,i.3) };'//&
         'if not e { j3,s3,_:=PM__first(d.3);'//&
         'j4,s4,e:=PM__next(d.4,g.4,i.4) };'//&
         'return [j1,j2,j3,j4],[s1,s2,s3,s4],e}',line)
    call dcl_uproc(parser,&
         'PM__next(d:tuple5d,g:tuple5d,i:tuple5d) { '//&
         'var j1, s1, e=PM__next(d.1,g.1,i.1);'//&
         'var j2=i.2;var s2=g.2;var j3=i.3;var s3=g.3;var j4=i.4;'//&
         'var s4=g.4;var j5=i.5;var s5=g.5;'//&
         'if not e { j1,s1,_:=PM__first(d.1);'//&
         'j2,s2,e:=PM__next(d.2,g.2,i.2) };'//&
         'if not e { j2,s2,_:=PM__first(d.2);'//&
         'j3,s3,e:=PM__next(d.3,g.3,i.3) };'//&
         'if not e { j3,s3,_:=PM__first(d.3);'//&
         'j4,s4,e:=PM__next(d.4,g.4,i.4) };'//&
         'if not e { j4,s4,_:=PM__first(d.4);'//&
         'j5,s5,e:=PM__next(d.5,g.5,i.5) };'//&
         'return [j1,j2,j3,j4,j5],'//&
         '[s1,s2,s3,s4,s5],e}',line)
    call dcl_uproc(parser,&
         'PM__next(d:tuple6d,g:tuple6d,i:tuple6d) { '//&
         'var j1, s1, e=PM__next(d.1,g.1,i.1);'//&
         'var j2=i.2;var s2=g.2;var j3=i.3;var s3=g.3;'//&
         'var j4=i.4;var s4=g.4;var j5=i.5;var s5=g.5;var j6=i.6;var s6=g.6;'//&
         'if not e { j1,s1,_:=PM__first(d.1);'//&
         'j2,s2,e:=PM__next(d.2,g.2,i.2) };'//&
         'if not e { j2,s2,_:=PM__first(d.2);'//&
         'j3,s3,e:=PM__next(d.3,g.3,i.3) };'//&
         'if not e { j3,s3,_:=PM__first(d.3);'//&
         'j4,s4,e:=PM__next(d.4,g.4,i.4) };'//&
         'if not e { j4,s4,_:=PM__first(d.4);'//&
         'j5,s5,e:=PM__next(d.5,g.5,i.5) };'//&
         'if not e { j5,s5,_:=PM__first(d.5);'//&
         'j6,s6,e:=PM__next(d.5,g.6,i.6) };'//&
         'return [j1,j2,j3,j4,j5,j6],'//&
         '[s1,s2,s3,s4,s5,s6],e}',line)
    call dcl_uproc(parser,&
         'PM__next(d:tuple7d,g:tuple7d,i:tuple7d) { '//&
         'var j1, s1, e=PM__next(d.1,g.1,i.1);'//&
         'var j2=i.2;var s2=g.2;var j3=i.3;var s3=g.3;var j4=i.4;var s4=g.4;'//&
         'var j5=i.5;var s5=g.5;var j6=i.6;var s6=g.6;var j7=i.7;var s7=g.7;'//&
         'if not e { j1,s1,_:=PM__first(d.1);'//&
         'j2,s2,e:=PM__next(d.2,g.2,i.2) };'//&
         'if not e { j2,s2,_:=PM__first(d.2);'//&
         'j3,s3,e:=PM__next(d.3,g.3,i.3) };'//&
         'if not e { j3,s3,_:=PM__first(d.3);'//&
         'j4,s4,e:=PM__next(d.4,g.4,i.4) };'//&
         'if not e { j4,s4,_:=PM__first(d.4);'//&
         'j5,s5,e:=PM__next(d.5,g.5,i.5) };'//&
         'if not e { j5,s5,_:=PM__first(d.5);'//&
         'j6,s6,e:=PM__next(d.5,g.6,i.6) };'//&
         'if not e { j6,s6,_:=PM__first(d.6);'//&
         'j7,s7,e:=PM__next(d.7,g.7,i.7) };'//&
         'return [j1,j2,j3,j4,j5,j6,j7],'//&
         '[s1,s2,s3,s4,s5,s6,s7],e}',line)
     
    ! Concurrent execution for each member of mshape
 
    if(pm_is_compiling) then
       call dcl_uproc(parser,'PM__generate(x:dshape,n,s)=dims(x._tilesz)',line)
       call dcl_uproc(parser,'PM__generate(x:mshape,n,s)=dims(x)',line)
       call dcl_uproc(parser,'PM__generate(x)=_elts(x)',line)
       
!!$       call dcl_uproc(parser,'PM__generate(x:dshape,n,s:schedule)=_belts(s,x)',line)
!!$       call dcl_uproc(parser,'PM__generate(x:mshape,n,s:schedule)=_belts(s,x)',line)
       
       call dcl_proc(parser,&
            '_doloop(int)->int',&
            op_do_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_doloop(int,int)->int,int',&
            op_do_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_doloop(int,int,int)->int,int,int',&
            op_do_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_doloop(int,int,int,int)->int,int,int,int',&
            op_do_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_doloop(int,int,int,int,int)->int,int,int,int,int',&
            op_do_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_doloop(int,int,int,int,int,int)->int,int,int,int,int,int',&
            op_do_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_doloop(int,int,int,int,int,int,int)->'//&
            'int,int,int,int,int,int,int',&
            op_do_loop,0,line,&
            proc_is_generator)
       call dcl_uproc(parser,&
            '_elts(x:int)=i '//&
            'where i=_doloop(x)',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple1d)=[i] where i=_elts(x.1)',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple2d)=[i,j] where '//&
            'i,j=_doloop(x.1,x.2)',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple3d)=[i,j,k] where '//&
            'i,j,k=_doloop(x.2,x.2,x.3)',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple4d)=[i,j,k,l] where '//&
            'i,j,k,l=_doloop(x.1,x.2,x.3,x.4)',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple5d)=[i,j,k,l,m] where '//&
            'i,j,k,l,m=_doloop(x.1,x.2,x.3,x.4,x.5)',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple6d)=[i,j,k,l,m,n] where '//&
            'i,j,k,l,m,n=_doloop(x.1,x.2,x.3,x.4,x.5,x.6)',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple7d)=[i,j,k,l,m,n,o] where '//&
            'i,j,k,l,m,n,o=_doloop(x.1,x.2,x.3,x.4,x.5,x.6,x.7)',line)

       call dcl_proc(parser,&
            '_blockedloop(any)->int',&
            op_blocked_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_blockedloop(any)->int,int',&
            op_blocked_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_blockedloop(any)->int,int,int',&
            op_blocked_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_blockedloop(any)->int,int,int,int',&
            op_blocked_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_blockedloop(any)->int,int,int,int,int',&
            op_blocked_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_blockedloop(any)->int,int,int,int,int,int',&
            op_blocked_loop,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_blockedloop(any)->'//&
            'int,int,int,int,int,int,int',&
            op_blocked_loop,0,line,&
            proc_is_generator) 
       call dcl_uproc(parser,&
            '_belts(x,y:shape1d)=[i] where '//&
            'i=_blockedloop(PM__do_over(x,y))',line)
       call dcl_uproc(parser,&
            '_belts(x,y:shape2d)=[i,j] where '//&
            'i,j=_blockedloop(PM__do_over(x,y))',line)
       call dcl_uproc(parser,&
            '_belts(x,y:shape3d)=[i,j,k] where '//&
            'i,j,k=_blockedloop(PM__do_over(x,y))',line)
       call dcl_uproc(parser,&
            '_belts(x,y:shape4d)=[i,j,k,l] where '//&
            'i,j,k,l=_blockedloop(PM__do_over(x,y))',line)
       call dcl_uproc(parser,&
            '_belts(x,y:shape5d)=[i,j,k,l,m] where '//&
            'i,j,k,l,m=_blockedloop(PM__do_over(x,y))',line)
       call dcl_uproc(parser,&
            '_belts(x,y:shape6d)=[i,j,k,l,m,n] where '//&
            'i,j,k,l,m,n=_blockedloop(PM__do_over(x,y))',line)
       call dcl_uproc(parser,&
            '_belts(x,y:shape7d)=[i,j,k,l,m,n,o] where '//&
            'i,j,k,l,m,n,o=_blockedloop(PM__do_over(x,y))',line)
    else

       call dcl_uproc(parser,'PM__generate(x:dshape,n,s)=_elts(dims(x._tilesz),1,n)',line)
       call dcl_uproc(parser,'PM__generate(x:mshape,n,s)=_elts(dims(x),1,n)',line)
    
       call dcl_proc(parser,&
            '_iota(siz:int,start:int,finish:int,incr:int,totsiz:int)->int',&
            op_iota,0,line,&
            proc_is_generator)
       call dcl_proc(parser,&
            '_iota(siz:int,start:int,finish:int,incr:int,'//&
            'first:int,trunc:int,totsiz:int)->int',&
            op_iota,0,line,&
            proc_is_generator)
       call dcl_uproc(parser,'_n(x:int)=x',line)
       call dcl_uproc(parser,&
            '_elts(x:int,siz,tot)=_iota(siz,0,x-1,1,tot)',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple1d,siz,tot)=tuple(_elts(x.1,siz,tot))',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple2d,siz,tot)='//&
            'tuple(_elts(x.1,siz,tot),_elts(x.2,siz*_n(x.1),tot) )',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple3d,siz,tot)=tuple(_elts( x.1,siz,tot),'//&
            '_elts(x.2,s1,tot),'//&
            '_elts( x.3, s1*_n(x.2), tot)) where s1=siz*_n(x.1)',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple4d,siz,tot)='//&
            'tuple(_elts( x.1,siz,tot),_elts(x.2,s1,tot),'//&
            '_elts(x.3,s2,tot),_elts(x.4,s2*_n(x.3),tot)) '//&
            'where s2=s1*_n(x.2) where s1=siz*_n(x.1)',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple5d,siz,tot)='//&
            'tuple(_elts( x.1,siz,tot),_elts(x.2,s1,tot),'//&
            '_elts(x.3,s2,tot),_elts(x.4,s3,tot),'//&
            '_elts(x.5,s3*_n(x.4), tot)) '//&
            'where s3=s2*_n(x.3) where s2=s1*_n(x.2) '//&
            'where s1=siz*_n(x.1)',&
            line)
       call dcl_uproc(parser,&
            '_elts(x:tuple6d,siz,tot)=tuple(_elts( x.1,siz,tot),'//&
            '_elts(x.2,s1,tot),'//&
            '_elts(x.3,s2,tot),_elts(x.4,s3,tot),_elts(x.5,s4,tot),'//&
            '_elts(x.6,s4*_n(x.5), tot)) '//&
            'where s4=s3*_n(x.4) where s3=s2*_n(x.3) '//&
            'where s2=s1*_n(x.2) where s1=siz*_n(x.1)',line)
       call dcl_uproc(parser,&
            '_elts(x:tuple7d,siz,tot)=tuple(_elts( x.1,siz,tot),'//&
            '_elts(x.2,s1,tot),'//&
            '_elts(x.3,s2,tot),_elts(x.4,s3,tot),'//&
            '_elts(x.5,s4,tot),_elts(x.6,s5,tot),'//&
            '_elts(x.7,s5*_n(x.6), tot)) '//&
            'where s5=s4*_n(x.5) where s4=s3*_n(x.4) '//&
            'where s3=s2*_n(x.3) '//&
            'where s2=s1*_n(x.2) where s1=siz*_n(x.1)',line)
    endif

    call dcl_proc(parser,&
            '_indices(any)->int',op_indices,0,line,0)
    
    ! **************************************
    ! ARRAYS
    ! **************************************

    ! Array types
    call dcl_type(parser,&
         'array(e,d:shape) is varray(e,d),farray(e,d)',line)
    call dcl_type(parser,'varray(e,d:shape) is'//&
         ' e^var d,array_template(e,d,''true)',line)
    call dcl_type(parser,'farray(e,d:shape) is'//&
         ' e^const d,e^invar d,e^fix d,array_template(e,d,''false)',line)
    call dcl_type(parser,'farray(e,d:mshape) is ...,'//&
         'array_slice(e^const any),array_slice(e^var any),'//&
         'array_slice(e^invar any),array_slice(e^fix any)',line)
    
    ! Array operations
    call dcl_uproc(parser,'_arb(x:any^mshape)='//&
         '_get_aelem(x,0)',line)
    call dcl_proc(parser,'size(x:any^mshape)->int',op_get_size,0,line,0)
    call dcl_proc(parser,'_array(x:any,y:any,z:any,v:''false)->PM__dim x,y',&
         op_array,0,line,proc_needs_type)
    call dcl_proc(parser,'_array(x:any,y:any,z:any,v:''true)->PM__vdim x,y',&
         op_var_array,0,line,proc_needs_type)
    call dcl_proc(parser,'_array(x:any,y:any,z:any,v:''false,i:''true)->PM__invar_dim x,y',&
         op_array,0,line,proc_needs_type)
    call dcl_proc(parser,'_array(x:any,y:any,z:any,v:''false,i:''false)->PM__fix_dim x,y,z',&
         merge(op_init_farray,op_array,pm_is_compiling),0,line,proc_needs_type)
    call dcl_proc(parser,&
         '_redim(x:any^any,y:any)->over x,y',&
         op_redim,0,line,proc_needs_type)
    call dcl_proc(parser,'PM__dim_noinit(x:any,y:any,z:any)->PM__dim x,y',&
         op_array_noinit,0,line,proc_needs_type)

    call dcl_uproc(parser,'#%(x:invar any^any)=_array_shape(x <<PM__ignore>>)',line)
    call dcl_uproc(parser,'#%(x)=_get_shape(x)',line)
    call dcl_uproc(parser,'_get_shape(x)=#x',line)
    call dcl_uproc(parser,'#(x:any^any)=_array_shape(x)',line)
    call dcl_proc(parser,'_array_shape(x:any^any)->#x',op_get_dom,0,line,0)
    call dcl_uproc(parser,'dims(x:any^mshape)=dims(#x)',line)
    call dcl_proc(parser,'PM__extractelm(x:any^any)->%x',&
         op_extractelm,0,line,0)
   
    call dcl_uproc(parser,'_get_elem(a:any^mshape,t:index)='//&
         '_get_aelem(a,index(#(a),t))',line)
    call dcl_uproc(parser,'_set_elem(&a:any^mshape,v,t:index)'//&
         '{ PM__setaelem(&a,index(#(a),t),v) }',line)
    call dcl_uproc(parser,'_make_subref(a:any^mshape,t:index)='//&
         '_make_subref(a,index(#(a),t))',line)
    call dcl_proc(parser,'_make_subref(a:any^mshape,i:int)->%a',&
         op_make_rf,0,line,0)

    call dcl_proc(parser,'_get_aelem(x:any^any,y:int)->%x',&
         op_array_get_elem,0,line,0)
    call dcl_proc(parser,'PM__setaelem(&x:any^any,y:int,z:any)',&
         op_array_set_elem,0,line,0)

    ! Linear index of tuple mshape (zero base,unit stride)
    call dcl_uproc(parser,'_indx(g:null,s)=''0',line)
    call dcl_uproc(parser,'_indx(g:range(int),s)=int(s)',line)
    call dcl_uproc(parser,'_indx(g:any_int,s)=int(s)',line)
    call dcl_uproc(parser,'_sz(x:null)=''1',line)
    call dcl_uproc(parser,'_sz(x:int)=x',line)
    call dcl_uproc(parser,'_sz(x:range(int))=x._n',line)
    call dcl_uproc(parser,'_offset(x:mshape)=x._o',line)
    call dcl_uproc(parser,'_offset(x)=''0',line)
    call dcl_uproc(parser,'index(g:mshape1d or tuple1d_of(int),s:any_int)=int(_indx(g.1,s))+_offset(g)',line)
    call dcl_uproc(parser,'index(g:mshape1d or tuple1d_of(int),s:tuple1d_of(any_int))=int(_indx(g.1,s.1))+_offset(g)',line)
    call dcl_uproc(parser,'index(g:mshape2d or tuple2d_of(int),s:tuple2d_of(any_int))='//&
         'int(_indx(g.1,s.1)+_sz(g.1)*'//&
         '_indx(g.2,s.2))+_offset(g)',line)
    call dcl_uproc(parser,'index(g:mshape3d or tuple3d_of(int),s:tuple3d_of(any_int))='//&
         'int(_indx(g.1,s.1)+_sz(g.1)*'//&
         '(_indx(g.2,s.2)+_sz(g.2)*'//&
         '_indx(g.3,s.3)))+_offset(g)',line)
    call dcl_uproc(parser,&
         'index(g:mshape4d or tuple4d_of(int),s:tuple4d_of(any_int))='//&
         ' int(_indx(g.1,s.1)+_sz(g.1)*'//&
         ' (_indx(g.2,s.2)+_sz(g.2)*'//&
         ' (_indx(g.3,s.3)+_sz(g.3)*'//&
         ' _indx(g.4,s.4))))+_offset(g)',line)
    call dcl_uproc(parser,&
         'index(g:mshape5d or tuple5d_of(int),s:tuple5d_of(any_int))='//&
         ' int(_indx(g.1,s.1)+_sz(g.1)*'//&
         ' (_indx(g.2,s.2)+_sz(g.2)*'//&
         ' (_indx(g.3,s.3)+_sz(g.3)*'//&
         ' (_indx(g.4,s.4)+_sz(g.4)*'//&
         ' _indx(g.5,s.5)))))+_offset(g)',line)
    call dcl_uproc(parser,&
         'index(g:mshape6d or tuple6d_of(int),s:tuple6d_of(any_int))='//&
         ' int(_indx(g.1,s.1)+_sz(g.1)*'//&
         ' (_indx(g.2,s.2)+_sz(g.2)*'//&
         ' (_indx(g.3,s.3)+_sz(g.3)*'//&
         ' (_indx(g.4,s.4)+_sz(g.4)*'//&
         ' (_indx(g.5,s.5)+_sz(g.5)*'//&
         ' _indx(g.6,s.6))))))+_offset(g)',line)
    call dcl_uproc(parser,&
         'index(g:mshape7d or tuple7d_of(int),s:tuple7d_of(any_int))='//&
         ' int(_indx(g.1,s.1)+_sz(g.1)*'//&
         ' (_indx(g.2,s.2)+_sz(g.2)*'//&
         ' (_indx(g.3,s.3)+_sz(g.3)*'//&
         ' (_indx(g.4,s.4)+_sz(g.4)*'//&
         ' (_indx(g.5,s.5)+_sz(g.5)*'//&
         ' (_indx(g.6,s.6)+_sz(g.6)*'//&
         ' (_indx(g.7,s.7))))))))+_offset(g)',line)

    call dcl_uproc(parser,'index2point(i:int,s:range(int))=[i+s._lo]',line)
    call dcl_uproc(parser,'index2point(i:int,s:int)=[i]',line)
    call dcl_uproc(parser,'index2point(i:int,s:tuple1d_of(int))=[i]',line)
    call dcl_uproc(parser,&
         'index2point(i:int,s:tuple2d_of(int))='//&
         '[i1,i2] where i1=i-i2*_sz(s.1) where i2=i/_sz(s.1)',line)
    call dcl_uproc(parser,'index2point(i:int,s:tuple3d_of(int))='//&
         '[i1,i2,i3] where i1=i-j2*_sz(s.1) '//&
         'where i2=j2-i3*_sz(s.2) where i3=j2/_sz(s.2) where j2=i/_sz(s.1)',line)
    call dcl_uproc(parser,&
         'index2point(i:int,s:tuple4d_of(int))='//&
         '[i1,i2,i3,i4]'//&
         ' where i1=i-j2*_sz(s.1) where i2=j2-j3*_sz(s.2) where i3=j3-i4*_sz(s.3) where i4=j3/_sz(s.3)'//&
         ' where j3=j2/_sz(s.2) where j2=i/_sz(s.1)',line)
    call dcl_uproc(parser,&
         'index2point(i:int,s:tuple5d_of(int))='//&
         '[i1,i2,i3,i4,i5]'//&
         ' where i1=i-j2*_sz(s.1) where i2=j2-j3*_sz(s.2) '//&
         ' where i3=j3-j4*_sz(s.3) where i4=j4-i5*_sz(s.4) '//&
         ' where i5=j4/_sz(s.4)'//&
         ' where j4=j3/_sz(s.3) where j3=j2/_sz(s.2) where j2=i/_sz(s.1)',line)
    call dcl_uproc(parser,'index2point(i:int,s:tuple6d_of(int))='//&
         '[i1,i2,i3,i4,i5,i6]'//&
         ' where i1=i-j2*_sz(s.1) where i2=j2-j3*_sz(s.2) '//&
         ' where i3=j3-j4*_sz(s.3) where i4=j4-j5*_sz(s.4) '//&
         ' where i5=j5-i6*_sz(s.5) where i6=j5/_sz(s.5)'//&
         ' where j5=j4/_sz(s.4) where j4=j3/_sz(s.3) where j3=j2/_sz(s.2) where j2=i/_sz(s.1)',line)
    call dcl_uproc(parser,'index2point(i:int,s:tuple7d_of(int))='//&
         '[i1,i2,i3,i4,i5,i6,i7]'//&
         ' where i1=i-j2*_sz(s.1) where i2=j2-j3*_sz(s.2) '//&
         ' where i3=j3-j4*_sz(s.3) where i4=j4-j5*_sz(s.4) '//&
         ' where i5=j5-j6*_sz(s.5) where i6=j6-i7*_sz(s.6) where i7=j6/_sz(s.6)'//&
         ' where j6=j5/_sz(s.5) where j5=j4/_sz(s.4) where j4=j3/_sz(s.3)'//&
         ' where j3=j2/_sz(s.2) where j2=i/_sz(s.1)',line)

    ! *****************************************
    ! ARRAY TEMPLATES
    ! *****************************************

    ! Array templates
    call dcl_type(parser,'array_template(a,d:mshape or dshape,v:fix bool,i:fix bool,f:fix bool)'//&
         ' is rec {_a:a,_d:d,_s:int,_v:v,_i:i=''false,_f:f=''false}',line)
    call dcl_uproc(parser,&
         'array(a:any,s:dshape)='//&
         'new array_template {_a=a,_d=s,_s=size(s),_v=''false}',line)
    call dcl_uproc(parser,&
         'array(a:any,s:mshape(tuple(range(int))))='//&
         'new array_template {_a=a,_d=s,_s=size(s),_v=''false}',line)
    call dcl_uproc(parser,&
         'array(a:any,s:fix mshape(tuple(range(int))))='//&
         'new array_template {_a=a,_d=s,_s=size(s),_v=''false,_f=''true}',line)
    call dcl_uproc(parser,&
         'array(a:any,s:tuple(range(any_int)))=array(a,shape(s))',line)
    call dcl_uproc(parser,'dim%(a,d)=array(a,d)',line)
    call dcl_uproc(parser,'dim%(a,s:invar mshape(tuple(range(int))))='//&
         'new array_template {_a=a,_d=s,_s=size(s),_v=''false,_i=''true}',line)
    call dcl_uproc(parser,'dim%(a,sh:invar tuple(range(any_int)))='//&
         'new array_template {_a=a,_d=s,_s=size(s),_v=''false,_i=''true}'//&
         'where s=shape(sh)',line)
    
    call dcl_uproc(parser,&
         'varray(a:any,s:mshape or dshape)='//&
         'new array_template {_a=a,_d=s,_s=size(s),_v=''true}',line)
    call dcl_uproc(parser,&
         'varray(a:any,s:tuple(range(any_int)))=varray(a,shape(s))',line)
    call dcl_uproc(parser,'_zero(x)=0',line)
    call dcl_uproc(parser,&
         'varray(a:any,s:tuple(null))=varray(a,shape(map($_zero,s)))',line)

    ! Treat a template as if it were an array
    call dcl_uproc(parser,'_arb(a:array_template)=a._a',line)
    call dcl_uproc(parser,'#(a:array_template(,mshape,))=a._d',line)
    call dcl_uproc(parser,'dims(a:array_template(,mshape,))=dims(a._d)',line)
    call dcl_uproc(parser,'size(a:array_template)=a._s',line)
    call dcl_uproc(parser,'redim(a:array_template,d:mshape)='//&
         ' new array_template { _a=a,_d=d,_s=size(d),_v=a._v}'//&
         ' check "New dshape does not have same size in redim"=>'//&
         ' size(d)==a._s',line)
    call dcl_uproc(parser,'_get_elem(a:array_template,arg...:subs)=a._a',line)

    ! Array creation from template
    call dcl_uproc(parser,'PM__dup(a:array_template(,mshape,))='//&
         '_array(PM__dup(a._a),a._d,int(a._s),a._v)',line)
    call dcl_uproc(parser,'PM__dup(a:array_template(,mshape,,,''true))='//&
         '_array(PM__dup(a._a),a._d,int(a._s),a._v,''false)',line)
    call dcl_uproc(parser,'PM__dup(a:array_template(,mshape,,''true,''false))='//&
         '_array(PM__dup(a._a),a._d,int(a._s),a._v,''true)',line)
    
    call dcl_uproc(parser,'PM__do_dim(a:any,d:mshape)='//&
         '_array(a,d,size(d),''false)',&
         line)

    
    !*****************************************
    ! MATRIX AND VECTOR
    ! *****************************************

    
    
    call dcl_type(parser,'matrix_element is num,bool,...',line)
    call dcl_type(parser,'_matrix(t) is struct{use array:t}',line)
    call dcl_type(parser,'matrix(t:matrix_element) is _matrix(array(t,shape2d))',line)
    call dcl_type(parser,'vector(t:matrix_element) is _matrix(array(t,shape1d))',line)
    call dcl_type(parser,'matrix_template(t:matrix_element) is _matrix(array_template(t,shape2d))',line)
    call dcl_type(parser,'vector_template(t:matrix_element) is _matrix(array_template(t,shape1d))',line)
    call dcl_uproc(parser,'PM__matrix(x)=new _matrix{array=x}',line)
    call dcl_uproc(parser,'vector(x:matrix_element,n:shape1d or extent1d)='//&
         'PM__matrix(array(x,n))',line)
    call dcl_uproc(parser,'vvector(x:matrix_element,n:shape1d or extent1d)='//&
         'PM__matrix(varray(x,n))',line)
    call dcl_uproc(parser,'dvector(x:matrix_element,n:shape1d or extent1d)='//&
         'PM__matrix(array(x,n,BLOCK_CYCLIC(32)))',line)
    call dcl_uproc(parser,'dvvector(x:matrix_element,n:shape1d or extent1d)='//&
         'PM__matrix(varray(x,n,BLOCK_CYCLIC(32)))',line)
    call dcl_uproc(parser,'vector(x:matrix_element,n:shape1d or extent1d,'//&
         'distr:distr_template,key...)='//&
         'PM__matrix(array(x,n,distr,key...))',line)
    call dcl_uproc(parser,'vvector(x:matrix_element,n:shape1d or extent1d,'//&
         'distr:distr_template,key...)='//&
         'PM__matrix(varray(x,n,key...))',line)
    call dcl_uproc(parser,'matrix(x:matrix_element,n:shape2d or extent2d)='//&
             'PM__matrix(array(x,n))',line)
    call dcl_uproc(parser,'vmatrix(x:matrix_element,n:shape2d or extent2d)='//&
         'PM__matrix(varray(x,n))',line)
    call dcl_uproc(parser,'dmatrix(x:matrix_element,n:shape2d or extent2d)='//&
         'PM__matrix(array(x,n,BLOCK_CYCLIC(32)))',line)
    call dcl_uproc(parser,'dvmatrix(x:matrix_element,n:shape2d or extent2d)='//&
         'PM__matrix(varray(x,n,BLOCK_CYCLIC(32)))',line)
    call dcl_uproc(parser,'matrix(x:matrix_element,n:shape2d or extent2d,'//&
         'distr:distr_template,key...)='//&
         'PM__matrix(array(x,n,distr,key...))',line)
    call dcl_uproc(parser,'vmatrix(x:matrix_element,n:shape2d or extent2d,'//&
         'distr:distr_template,key...)='//&
         'PM__matrix(varray(x,n,key...))',line)

    call dcl_uproc(parser,'matrix_element_zero(x:num)=convert(0,x)',line)
    call dcl_uproc(parser,'matrix_element_balance(x:num,y:num)=xx,yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,'matrix_element_add(x:num,y:num)=x+y',line)
    call dcl_uproc(parser,'matrix_element_subtract(x:num,y:num)=x-y',line)
    call dcl_uproc(parser,'matrix_element_multiply(x:num,y:num)=x*y',line)
    call dcl_uproc(parser,'matrix_element_zero(x:bool)=false',line)
    call dcl_uproc(parser,'matrix_element_balance(x:bool,y:bool)=x,y',line)
    call dcl_uproc(parser,'matrix_element_add(x:bool,y:bool)=x or y',line)
    call dcl_uproc(parser,'matrix_element_multiply(x:bool,y:bool)=x and y',line)
    
    call dcl_uproc(parser,'+(x:matrix,y:matrix) {'//&
         'check_conform(x.array,y.array);'//&
         'test "Cannot add zero size matrices"=>size(x)>0;'//&
         'var z=matrix(b,#x.array) where b,_=matrix_element_balance(_arb(x.array),_arb(y.array));'//&
         'for xx in x.array, yy in y.array, zz in z.array:zz:=matrix_element_add(xx,yy) '//&
         'return z }',line)

    call dcl_uproc(parser,'-(x:matrix,y:matrix) {'//&
         'check_conform(x.array,y.array);'//&
         'test "Cannot add zero size matrices"=>size(x)>0;'//&
         'var z=matrix(b,#x.array) where b,_=matrix_element_balance(_arb(x.array),_arb(y.array));'//&
         'for xx in x.array, yy in y.array, zz in z.array:zz:=matrix_element_subtract(xx,yy) '//&
         'return z }',line)
    
    call dcl_uproc(parser,'*(x:matrix,y:matrix) {'//&
         'sx=#x;sy=#y;sz=[sx.1,sy.2];test "Matrices do not conform for multiplication"=>size(sx.2)==size(sy.1);'//&
         'test "Cannot multiply zero size matrices"=>size(x)>0 and size(y)>0;'//&
         'var z=matrix(b,sz) where b,_=matrix_element_balance(_arb(x.array),_arb(y.array));'//&
         'for *i in sz,zz in z.array {'//&
         'var s=matrix_element_zero(_arb(z.array));'//&
         'foreach invar k in #(sx.1) {'//&
         ' s:=matrix_element_add(s,matrix_element_multiply(x.array[i.1,sx.2[k]],y.array[sy.1[k],i.2]))};'//&
         'zz:=s};'//&
         'return z }',line)
    
    ! *****************************************
    ! DISTRIBUTED SHAPE (DSHAPE)
    ! *****************************************

    call dcl_type(parser,'_distrb(extent:extent,dist:distr or null)',line)
    call dcl_type(parser,'_distrb(extent:extent,dist:distr) is ...,dshape(extent,dist)',line)
    call dcl_type(parser,'_distrb(extent:extent,dist:null) is ...,mshape(extent)',line)
    call dcl_type(parser,'shape(extent:extent,dist:distr or null) is _distrb(extent,dist)',line)
    call dcl_type(parser,'shape1d(extent:extent1d,dist:distr or null) is shape(extent,dist)',line)
    call dcl_type(parser,'shape2d(extent:extent2d,dist:distr or null) is shape(extent,dist)',line)
    call dcl_type(parser,'shape3d(extent:extent3d,dist:distr or null) is shape(extent,dist)',line)
    call dcl_type(parser,'shape4d(extent:extent4d,dist:distr or null) is shape(extent,dist)',line)
    call dcl_type(parser,'shape5d(extent:extent5d,dist:distr or null) is shape(extent,dist)',line)
    call dcl_type(parser,'shape6d(extent:extent6d,dist:distr or null) is shape(extent,dist)',line)
    call dcl_type(parser,'shape7d(extent:extent7d,dist:distr or null) is shape(extent,dist)',line)
    call dcl_type(parser,'PM__distr_tag is unique',line)
    
    call dcl_type(parser,'dshape(extent:extent,dist:distr) '//&
         'is rec {use _mshape:mshape(extent),dist:dist,_tile,'//&
         '_tilesz,_size:int,_level:int,_dtag:PM__distr_tag}',line)
    call dcl_uproc(parser,&
         'check_conform(x:dshape,y:mshape) { check_conform(x._mshape,y) }',line)
    call dcl_uproc(parser,&
         'check_conform(x:mshape,y:dshape) { '//&
         ' test "A distributed object connot conform to a non-distributed value" => ''false'//&
         '}',line)
    call dcl_uproc(parser,&
         'check_conform(x:dshape,y:dshape) { '//&
         ' check_conform(x._mshape,y._mshape);'//&
         ' test "Objects have different distributions"=>'//&
         ' x.dist==y.dist }',line)
    call dcl_uproc(parser,'conform(x:dshape,y:mshape)=conform(x._mshape,y)',line)
    call dcl_uproc(parser,'conform(x:mshape,y:dshape)=''false',line)
    call dcl_uproc(parser,'conform(x:dshape,y:dshape)=conform(x,y) and x.dist==y.dist',line)
    call dcl_uproc(parser,'_local_size(x:dshape)=size(x._tile)',line)
 
    ! Get an element from a null tile - just pass index through
    call dcl_uproc(parser,'_get_elem(x:null,y:tuple(index))=y',line)
    call dcl_uproc(parser,'_get_elem(x:null,y:int)=y',line)
    
    call dcl_uproc(parser,'size(d:dshape)=d._size',line)
    call dcl_uproc(parser,'#(d:dshape)=d',line)

    ! *****************************************
    ! DISTRIBUTED ARRAY AND SHAPE TEMPLATES
    ! *****************************************

    call dcl_type(parser,'darray_template(e,d,p,t) is '//&
         'rec {_e:e,_d:d,_p:p,_t:t,_v}',line)

    call dcl_type(parser,'dshape_template(d,p,t) is '//&
         'rec {_d:d,_p:p,_t:t}',line)

    call dcl_uproc(parser,'darray(e,d:extent)=array(e,d,VBLOCK)',line)
    
    call dcl_uproc(parser,'array(e,d:extent,'//&
         'distr:distr_template,topo:any=null)='//&
         'new darray_template {_e=e,_d=d,_p=distr,_t=topo,_v=''true}',line)

    call dcl_uproc(parser,'array(e,d:extent,distr:null or tuple(null))=array(e,d)',line)

    call dcl_uproc(parser,'dvarray(e,d:extent)=varray(e,d,VBLOCK)',line)
    
    call dcl_uproc(parser,'varray(e,d:extent,'//&
         'distr:distr_template,topo:any=null)='//&
         'new darray_template {_e=e,_d=d,_p=distr,_t=topo,_v=''true}',line)

    call dcl_uproc(parser,'varray(e,d:extent,distr:null or tuple(null))=varray(e,d)',line)

    call dcl_uproc(parser,'shape(d:extent,'//&
         'distr,topo:any=null)='//&
         'new dshape_template {_d=d,_p=distr,_t=topo}',line)

    
    ! *****************************************
    ! DISTRIBUTED ARRAYS
    ! *****************************************
    
    call dcl_uproc(parser,&
         'PM__dup(d:darray_template) { '//&
         ' dd=dims(d._d);'//&
         ' topo=topology(d._t,d._p,dd,'//&
         '       min(size(d._d),shrd_nnode()));'//&
         ' dist=distribute(d._p,dd,topo);'//&
         ' test "Not enough processors to implement distribution"=>'//&
         '    size(#dist)<=shrd_nnode();'//&
         ' p=_shrd_node();'//&
         ' var elem=empty(dist);'//&
         ' if p<size(topo) { elem:=_get_elem(dist,p) };'//&
         ' tshape=#elem;'//&
         ' dom=new dshape {_mshape=shape(d._d),dist=dist,_tile=elem,'//&
         '_tilesz=tshape,_size=size(tshape),_level=_lvl()'//&
         '};'//&
         'return _array(_ddup(d._e),dom,int(size(elem)),''false) }',line)

    call dcl_uproc(parser,&
         'PM__dup(d:dshape_template) { '//&
         ' dd=dims(d._d);'//&
         ' topo=topology(d._t,d._p,dd,'//&
         '       min(size(d._d),shrd_nnode()));'//&
         ' dist=distribute(d._p,dd,topo);'//&
         ' test "Not enough processors to implement distribution"=>'//&
         '    size(#dist)<=shrd_nnode();'//&
         ' p=_shrd_node();'//&
         ' var elem=empty(dist);'//&
         ' if p<size(topo) { elem:=_get_elem(dist,p) };'//&
         ' tshape=#elem;'//&
         ' dom=new dshape {_mshape=shape(d._d),dist=dist,_tile=elem,'//&
         '_tilesz=tshape,_size=size(tshape),_level=_lvl()'//&
         '};'//&
         'return dom }',line)
    
    call dcl_type(parser,'_distr is darray_template,dshape',line)

    call dcl_uproc(parser,'_ddup(e)=PM__dup(e)',line)
    call dcl_uproc(parser,'_ddup(e:contains(_distr))=PM__dup(e)'//&
         ' :test "Distributed array cannot have distributed elements" => ''false',line)
    
    call dcl_uproc(parser,'_arb(dd:any^dshape)=_get_aelem(dd,0)',line)
    call dcl_uproc(parser,&
         'dims(dd:any^dshape)=dims((#dd)._mshape)',line)

    call dcl_uproc(parser,'PM__redim(a,d)=_redim(a,d)',line)
    call dcl_uproc(parser,'PM__local(a:any^dshape)=_redim(a,(#a)._tilesz)',line)
    call dcl_uproc(parser,'PM__local(a:any^mshape)=a',line)
    call dcl_uproc(parser,'_get_elem(a:any^dshape,t) { '//&
         'p,i=node_and_index((#a).dist,(#a)._mshape#_tup(t));'//&
         'var r=_arb(a);if p==_this_node():r:=_get_aelem(a,i);PM__broadcast(&r,p);return r} ',line)
    call dcl_uproc(parser,'_set_elem(&a:any^dshape,v,t) {'//&
         ' p,i=node_and_index((#a).dist,(#a)._mshape#_tup(t));'//&
         ' if p==_this_node():PM__setaelem(&a,i,v)}',line)



    !*************************************************
    ! SLICES
    !*************************************************

    ! Slices
    call dcl_type(parser,'array_slice(a,s) is struct^{_a:a,_s:s}',line)
    call dcl_uproc(parser,'_arb(x:array_slice)=_arb(x._a)',line)
    call dcl_uproc(parser,'#(x:array_slice)=#(x._s)',line)
    call dcl_uproc(parser,'conform(x:mshape,y:array_slice)='//&
         'map_reduce($_conform,$and,x,y._s)',line)
    call dcl_uproc(parser,'conform(x:dshape,y:array_slice)='//&
         'map_reduce($_conform,$and,x._mshape,y._s)',line)
    call dcl_uproc(parser,'conform(x,y:array_slice)='//&
         'map_reduce($_conform,$and,#x,y._s)',line)
    call dcl_uproc(parser,'dims(x:array_slice)=dims(x._s)',line)
    call dcl_uproc(parser,'size(x:array_slice)=size(x._s)',line)
    call dcl_uproc(parser,'_get_elem(x:array_slice(any^mshape,),y:index)=_get_elem(x._a,x._s[y])',line)
    call dcl_uproc(parser,'_get_elem(x:array_slice,y:subs)=new array_slice {_a=x._a,_s=x._s[y]}',line)
    call dcl_uproc(parser,'_set_elem(&x:array_slice(any^mshape,),v,y:index)'//&
         ' {PM__setaelem(&^(x._a),index(##(x._a),x._s[y]),v)}',line)
    call dcl_uproc(parser,'PM__dup(x:array_slice(any^mshape,))'//&
         '{var a=array(_arb(x),#x);a:=x;return a}',line) 
    
    !*************************************************
    ! ARRAY & SLICE ASSIGNMENT
    !*************************************************

    call dcl_uproc(parser,'PM__assign(&xx:farray,x:any)'//&
         ' {check_assign_types(_arb(xx),x);_set_array(&xx,x)}',line)
    call dcl_uproc(parser,'PM__assign(&xx:farray,x:array) '//&
         '{_array_assign(&xx,x,same_type(_arb(x),_arb(xx)))}',&
         line)
    call dcl_uproc(parser,'PM__assign(&xx:varray,x:farray) '//&
         '{_array_assign(&xx,x,same_type(_arb(x),_arb(xx)))}',&
         line)
    call dcl_uproc(parser,'PM__assign_var(&xx:farray,x:any)'//&
         ' {check_assign_types(_arb(xx),x);_set_array(&xx,x)}',line)
    call dcl_uproc(parser,'PM__assign_var(&xx:farray,x:array) '//&
         '{_array_assign(&xx,x,same_type(_arb(x),_arb(xx)))}',&
         line)
    call dcl_uproc(parser,'PM__assign_var(&xx:varray,x:farray) '//&
         '{_array_assign(&xx,x,same_type(_arb(x),_arb(xx)))}',&
         line)
    call dcl_uproc(parser,'_array_assign(&xx,x,v:''false)'//&
         '{check_assign_types(_arb(xx),x);_set_array(&xx,x)}',line)
    call dcl_uproc(parser,'_array_assign(&xx,x,v:''true)'//&
         '{check_conform(extent(#xx),extent(#x));'//&
         'if _copy_array(&xx,x):_sync_messages(xx,x)}',line)
    call dcl_uproc(parser,'_array_assign(&xx:varray,x,v:''true) '//&
         '{_assign_element(&xx,x)}',line)
    call dcl_uproc(parser,'_array_assign(&xx:varray,x:array_template,v:''true) '//&
         '{_assign_element(&xx,PM__dup(x))}',line)
    
    call dcl_uproc(parser,'_set_slice(&x,a,y,b) {x[a]:=y[b]}',line)

    call dcl_type(parser,'_non_d is any^mshape,array_slice(any^mshape,)',line)

    call dcl_uproc(parser,'_set_array(&x:any^mshape,y)'//&
         '{for i in x  <<conc>>{ _assign(&i,y) } } ',line)
    call dcl_uproc(parser,'_set_array(&x:any^dshape,y)'//&
         '{_set_array(&^(PM__local(^(&x))),y)}',line)
    call dcl_uproc(parser,'_set_array(&x:array_slice(any^mshape,),y)'//&
         '{for i in x._s  <<conc>>{_set_elem(&x._a,y,i <<PM__ignore>>) }}',line)
    call dcl_uproc(parser,'_set_array(&x:array_slice(any^dshape,),y){'//&
         'tile=(#x._a)._tile;'//&
         't=overlap(tile,((#x._a)#x._s));'//&
         'for i in t  <<conc>>{ '//&
         '  _set_elem(&x._a,y,i <<PM__ignore>>) '//&
         '}}',line)
       
    call dcl_uproc(parser,'_copy_array(&a:_non_d,b:_non_d)'//&
         '{for i in a, j in b  <<conc>>{i:=j};return ''false}',line)

    call dcl_uproc(parser,'_copy_array(&xx:any^dshape,x:_non_d) {'//&
         'tile=(#xx)._tile;'//&
         'for i in tile  <<conc>>{PM__setaelem(&xx,'//&
         'index(dims(tile),here),x[i] <<PM__ignore>>)};return ''false }',&
         line)

    call dcl_uproc(parser,'_copy_array(&xx:array_slice(any^dshape,),x:_non_d) {'//&
         'tile=(#xx._a)._tile;subtile,subarray=overlap(tile,xx._s);'//&
         'for i in subtile,j in subarray  <<conc>>{'//&
         ' PM__setaelem(&xx._a,index(dims(tile),i),x[j] <<PM__ignore>>) };return ''false }',&
         line)
    
    call dcl_uproc(parser,'_copy_array(&a:_non_d,x:any^dshape) {'//&
         ' dist=(#x).dist; '//&
         ' foreach p in #(dist) {'//&
         '   tile=_get_elem(dist,p);'//&
         '   i=index(dims(dist),p);'//&
         '   if i==_this_node() { '//&
         '     for kk in PM__local(x),j in tile  <<conc>>{ '//&
         '       var k=kk;PM__broadcast(&k,i);'//&
         '       _set_elem(&a,k,j <<PM__ignore>>)'//&
         '     }'//&
         '   } else { '//&
         '     for j in tile  <<conc>>{ '//&
         '       var k=_arb(a);'//&
         '       PM__broadcast(&k,i);'//&
         '       _set_elem(&a,k,j <<PM__ignore>>);'//&
         '     }'//&
         '   }'//&
         ' };return ''false}',&
         line)

    call dcl_uproc(parser,'_copy_array(&v:_non_d,x:array_slice(any^dshape,)) {'//&
         ' dist=(#(x._a)).dist;xs=(#(x._a))._mshape#x._s;'//&
         ' nodes=#dist;'//&
         ' foreach pp in overlap(nodes,nodes_for_grid(dist,xs)) {'//&
         '   p=nodes[pp];utile,elem=overlap(dist[nodes[p]],xs);'//&
         '   i=index(dims(dist),p);'//&
         '   if i==_this_node() {'//&
         '     for j in utile, jj in elem  <<conc>>{ '//&
         '       var k=_get_elem(PM__local(x._a),j); '//&
         '       PM__broadcast(&k,i);'//&
         '       _set_elem(&v,k,jj <<PM__ignore>>)'//&
         '     }'//&
         '   } else { '//&
         '     for j in elem  <<conc>>{'//&
         '       var k=_arb(x._a);'//&
         '       PM__broadcast(&k,i);'//&
         '       _set_elem(&v,k,j <<PM__ignore>>)'//&
         '     }'//&
         '   }'//&
         ' };return ''false}',&
         line)

    call dcl_uproc(parser,'_copy_array(&x:any^mshape,y:array_template) '//&
         '{_set_array(&x,y._a);return ''false }',line)
    call dcl_uproc(parser,'_copy_array(&x:any^dshape,y:array_template) {'//&
         '_set_array(&^(PM__local(^(&x))),y._a);return ''false}',line)
    
    call dcl_type(parser,'_comp is contains(array or *any or ^*(,,,,))',line)
    
    call dcl_uproc(parser,'_copy_array(&xx:any^dshape,x:any^dshape) {'//&
         'newd=#xx;oldd=#x;'//&
         'foreach pp in nodes_for_grid(oldd.dist,newd._tile) {'//&
         '  p=index(dims(oldd.dist),pp);'//&
         '  if p/=_this_node() {tile=_get_elem(oldd.dist,p);'//&
         '  _recv_slice(p,&xx,overlap(newd._tile,tile))}'//&
         '};'//&
         'foreach pp in nodes_for_grid(newd.dist,oldd._tile) {'//&
         '  p=index(dims(newd.dist),pp);'//&
         '  if p/=_this_node() {tile=_get_elem(newd.dist,p);'//&
         '  _send_slice(p,x,overlap(oldd._tile,tile))}'//&
         '};'//&
         'o,oo=overlap(newd._tile,oldd._tile);'//&
         '_set_slice(&^(PM__local(^(&xx))),o,PM__local(x),oo);'//&
         'return ''true}',line)
    
    call dcl_uproc(parser,'_copy_array(&xx:array_slice(any^dshape,),'//&
         'x:array_slice(any^dshape,)) {'//&
         'newd=#(xx._a);oldd=#(x._a);'//&
         'xs=(#(x._a))._mshape#x._s;xxs=(#(xx._a))._mshape#xx._s;'//&
         'oldpart,oldtile=overlap(xs,oldd._tile);'//&
         'newpart,newtile=overlap(xxs,newd._tile);'//&
         'foreach pp in nodes_for_grid(oldd.dist,_get_elem(xs,newpart)) {'//&
         '  p=index(dims(oldd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '   tile=_get_elem(xxs,overlap(xs,_get_elem(oldd.dist,p)));'//&
         '   _recv_slice(p,&^(PM__local(^(&xx._a))),overlap(newd._tile,tile))}'//&
         '};'//&
         'foreach pp in nodes_for_grid(newd.dist,_get_elem(xxs,oldpart)) {'//&
         '  p=index(dims(newd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '  tile=_get_elem(xs,overlap(xxs,_get_elem(newd.dist,p)));'//&
         '  _send_slice(p,PM__local(x._a),overlap(oldd._tile,tile))}'//&
         '};'//&
         'o,oo=overlap(newpart,oldpart);'//&
         '_set_slice(&^(PM__local(^(&xx._a))),_get_elem(newtile,o),PM__local(x._a),'//&
         '  _get_elem(oldtile,oo));'//&
         'return ''true}',line)

    call dcl_uproc(parser,'_copy_array(&xx:any^dshape,x:array_slice(any^dshape,)) {'//&
         'newd=#(xx);oldd=#(x._a);'//&
         'xs=(#(x._a))._mshape#x._s;'//&
         'oldpart,oldtile=overlap(xs,oldd._tile);'//&
         'foreach pp in nodes_for_grid(oldd.dist,_get_elem(xs,newd._tile)) {'//&
         '  p=index(dims(oldd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '   tile=overlap(xs,_get_elem(oldd.dist,p));'//&
         '   _recv_slice(p,&^(PM__local(^(&xx))),overlap(newd._tile,tile))}'//&
         '};'//&
         'foreach pp in nodes_for_grid(newd.dist,oldpart) {'//&
         '  p=index(dims(newd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '  tile=_get_elem(xs,_get_elem(newd.dist,p));'//&
         '  _send_slice(p,PM__local(x._a),overlap(oldd._tile,tile))}'//&
         '};'//&
         'o,oo=overlap(newd._tile,oldpart);'//&
         '_set_slice(&^(PM__local(^(&xx))),o,PM__local(x._a),'//&
         '  _get_elem(oldtile,oo));'//&
         'return ''true}',line)

    call dcl_uproc(parser,'_copy_array(&xx:array_slice(any^dshape,),x:any^dshape) {'//&
         'newd=#(xx._a);oldd=#x;'//&
         'xxs=(#(xx._a))._mshape#xx._s;'//&
         'newpart,newtile=overlap(xxs,newd._tile);'//&
         'foreach pp in nodes_for_grid(oldd.dist,newpart) {'//&
         '  p=index(dims(oldd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '   tile=_get_elem(xxs,_get_elem(oldd.dist,p));'//&
         '   _recv_slice(p,&^(PM__local(^(&xx._a))),overlap(newd._tile,tile))}'//&
         '};'//&
         'foreach pp in nodes_for_grid(newd.dist,_get_elem(xxs,oldd._tile)) {'//&
         '  p=index(dims(newd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '  tile=overlap(xxs,_get_elem(newd.dist,p));'//&
         '  _send_slice(p,PM__local(x),overlap(oldd._tile,tile))}'//&
         '};'//&
         'o,oo=overlap(newpart,oldd._tile);'//&
         '_set_slice(&^(PM__local(^(&xx._a))),_get_elem(newtile,o),PM__local(x),'//&
         '  oo);'//&
         'return ''true}',line)

    
    call dcl_uproc(parser,'_copy_array(&xx:_comp^dshape,x:any^dshape) {'//&
         'newd=#xx;oldd=#x;'//&
         'foreach pp in nodes_for_grid(newd.dist,oldd._tile) {'//&
         '  p=index(dims(newd.dist),pp);'//&
         '  if p/=_this_node() {tile=_get_elem(newd.dist,p);'//&
         '  _send_slice(p,PM__local(x),overlap(oldd._tile,tile))}'//&
         '};'//&
         'foreach pp in nodes_for_grid(oldd.dist,newd._tile) {'//&
         '  p=index(dims(oldd.dist),pp);'//&
         '  if p/=_this_node() {tile=_get_elem(oldd.dist,p);'//&
         '  _recv_slice_sync(p,&^(PM__local(^(&xx))),overlap(newd._tile,tile))}'//&
         '};'//&
         'o,oo=overlap(newd._tile,oldd._tile);'//&
         '_set_slice(&^(PM__local(^(&xx))),o,PM__local(x),oo);'//&
         'return ''true}',line)

    call dcl_uproc(parser,'_copy_array(&xx:array_slice(_comp^dshape,),'//&
         'x:array_slice(any^dshape,)) {'//&
         'newd=#(xx._a);oldd=#(x._a);'//&
         'xs=(#(x._a))._mshape#x._s;xxs=(#(xx._a))._mshape#xx._s;'//&
         'oldpart,oldtile=overlap(xs,oldd._tile);'//&
         'newpart,newtile=overlap(xxs,newd._tile);'//&
         'foreach pp in nodes_for_grid(newd.dist,_get_elem(xxs,oldpart)) {'//&
         '  p=index(dims(newd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '  tile=_get_elem(xs,overlap(xxs,_get_elem(newd.dist,p)));'//&
         '  _send_slice(p,PM__local(x._a),overlap(oldd._tile,tile))}'//&
         '};'//&
         'o,oo=overlap(newpart,oldpart);'//&
         '_set_slice(&^(PM__local(^(&xx._a))),_get_elem(newtile,o),PM__local(x._a),'//&
         '  _get_elem(oldtile,oo));'//&
         'foreach pp in nodes_for_grid(oldd.dist,_get_elem(xs,newpart)) {'//&
         '  p=index(dims(oldd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '   tile=_get_elem(xxs,overlap(xs,_get_elem(oldd.dist,p)));'//&
         '   _recv_slice_sync(p,&xx._a,overlap(newd._tile,tile))}'//&
         '};'//&
         'return ''true}',line)

    call dcl_uproc(parser,'_copy_array(&xx:_comp^dshape,x:array_slice(any^dshape,)) {'//&
         'newd=#xx;oldd=#(x._a);'//&
         'xs=(#(x._a))._mshape#x._s;'//&
         'oldpart,oldtile=overlap(xs,oldd._tile);'//&
         'foreach pp in nodes_for_grid(newd.dist,oldpart) {'//&
         '  p=index(dims(newd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '  tile=_get_elem(xs,_get_elem(newd.dist,p));'//&
         '  _send_slice(p,PM__local(x._a),overlap(oldd._tile,tile))}'//&
         '};'//&
         'o,oo=overlap(newd._tile,oldpart);'//&
         '_set_slice(&^(PM__local(^(&xx))),o,PM__local(x._a),'//&
         '  _get_elem(oldtile,oo));'//&
         'foreach pp in nodes_for_grid(oldd.dist,_get_elem(xs,newd._tile)) {'//&
         '  p=index(dims(oldd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '   tile=overlap(xs,_get_elem(oldd.dist,p));'//&
         '   _recv_slice_sync(p,&^(PM__local(^(&xx))),overlap(newd._tile,tile))}'//&
         '};'//&
         'return ''true}',line)

    call dcl_uproc(parser,'_copy_array(&xx:array_slice(_comp^dshape,),x:any^dshape) {'//&
         'newd=#(xx._a);oldd=#x;'//&
         'xxs=(#(xx._a))._mshape#xx._s;'//&
         'newpart,newtile=overlap(xxs,newd._tile);'//&
         'foreach pp in nodes_for_grid(newd.dist,_get_elem(xxs,oldd._tile)) {'//&
         '  p=index(dims(newd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '  tile=overlap(xxs,_get_elem(newd.dist,p));'//&
         '  _send_slice(p,PM__local(x),overlap(oldd._tile,tile))}'//&
         '};'//&
         'o,oo=overlap(newpart,oldd._tile);'//&
         '_set_slice(&^(PM__local(^(&xx._a))),_get_elem(newtile,o),PM__local(x),'//&
         '  oo);'//&
         'foreach pp in nodes_for_grid(oldd.dist,newpart) {'//&
         '  p=index(dims(oldd.dist),pp);'//&
         '  if p/=_this_node() {'//&
         '   tile=_get_elem(xxs,_get_elem(oldd.dist,p));'//&
         '   _recv_slice_sync(p,&^(PM__local(^(&xx._a))),overlap(newd._tile,tile))}'//&
         '};'//&
         'return ''true}',line)

    !*************************************************
    ! REFERENCES (SUBSCRIPTS AND SLICES)
    !*************************************************

    ! Reference type for & args
    call dcl_type(parser,'PM__reftype(x) is x,^shared(x,,,,)',line) 

    ! Support for internal ^(...) reference type
    call dcl_proc(parser,'_v1(x:any)->PM__d1 x',op_elem,1,line,0)
    call dcl_proc(parser,'_v2(x:any)->PM__d2 x',op_elem,2,line,0)
    call dcl_proc(parser,'_v3(x:any)->PM__d3 x',op_elem,3,line,0)
    call dcl_proc(parser,'_v4(x:any)->PM__d4 x',op_elem,4,line,0)
    call dcl_proc(parser,'_v5(x:any)->PM__d5 x',op_elem,5,line,0)

    call dcl_proc(parser,'_v1%(r:any,s:any,h:any,x:any)->PM__d1% x',op_elem,1,line,0)
    call dcl_proc(parser,'_v2%(r:any,s:any,h:any,x:any)->PM__d2% x',op_elem,2,line,0)
    call dcl_proc(parser,'_v3%(r:any,s:any,h:any,x:any)->PM__d3% x',op_elem,3,line,0)
    call dcl_proc(parser,'_v4%(r:any,s:any,h:any,x:any)->PM__d4% x',op_elem,4,line,0)
    call dcl_proc(parser,'_v5%(r:any,s:any,h:any,x:any)->PM__d5% x',op_elem,5,line,0)
     
    ! Right hand side references
    call dcl_uproc(parser,'_make_null(x)=null',line)
    call dcl_uproc(parser,&
         'PM__subref(x,t)=error_type() check "Incorrect type in subscript"=>''false',line) 
    call dcl_uproc(parser,'PM__subref(x,t:subs)'//&
         '{tt=_tup(t);check_contains(#x,tt);return _subref(x,tt) }',line)
    call dcl_uproc(parser,'PM__subref(x,t:null)=PM__subref(x,map($_make_null,#x))',line)
    call dcl_uproc(parser,'PM__subref(x:^*(,,,,),t:null)='//&
         'PM__subref(x,map($_make_null,#_v1(x)))',line)
    call dcl_uproc(parser,'_subref(x:any^mshape,t:index)=_get_elem(x,t)',line)
    call dcl_uproc(parser,'_subref(x:any^any,t:subs)=new array_slice {_a=x,_s=fill_in(#x,t)}',line)
    call dcl_uproc(parser,'_subref(x:array_slice(any^mshape,),t:index)=_get_elem(x._a,x._s[t])',line)
    call dcl_uproc(parser,'_subref(x:array_slice,t:subs)=new array_slice {_a=x._a,_s=x._s[t]}',line)
    call dcl_uproc(parser,'_subref(x:any^dshape,t:index)='//&
         ' PM__ref(_arb(x),x,i,p,_s_ref)'//&
         ' where p,i=node_and_index((#x).dist,(#x)._mshape#_tup(t))',line)
    call dcl_uproc(parser,'_subref(x:array_slice(any^dshape,),t:index)=_subref(x._a,x._s[t])',line)
    call dcl_uproc(parser,'_subref(a:^*(,,,,),t)='//&
         'PM__ref(_arb(_v1(a)),a,_tup(t),_v4(a),_v5(a))',line)
    call dcl_uproc(parser,'_subref(a,t)=$[](a,t)',line)
    call dcl_uproc(parser,'[](a:array,arg...)=PM__getref(PM__subref(a,_tup(arg...)))',line)
  
    ! Left hand side references
    call dcl_uproc(parser,&
         'PM__sublhsamp(x,t)=error_type() check "Incorrect type in subscript"=>''false',line)
    call dcl_uproc(parser,'PM__sublhsamp(x,t:subs)'//&
         ' {tt=_tup(t);check_contains(#x,tt);return _sublhs(x,tt)}',line)
    call dcl_uproc(parser,'PM__sublhsamp(x:any^dshape,t:subs)'//&
         ' {test "Cannot have subscript of a distributed array in ""&"" argument"=>''false;'//&
         ' return _arb(x)}',line)
    call dcl_uproc(parser,&
         'PM__sublhs(x,t)=error_type() check "Incorrect type in subscript"=>''false',line)
    call dcl_uproc(parser,'PM__sublhs(x,t:subs)'//&
         ' {tt=_tup(t);check_contains(#x,tt);return _sublhs(x,tt)}',line)
    call dcl_uproc(parser,'PM__sublhs(x:^!(,,,,),t:subs)'//&
         ' {tt=_tup(t);return _sublhs(x,tt)}',line)    
    call dcl_uproc(parser,&
         'PM__sublhs(x,t:null)=PM__sublhs(x,map($_make_null,#x))',line)
    call dcl_uproc(parser,'PM__sublhs(x:^!(,,,,),t:null)='//&
         'PM__sublhs(x,map($_make_null,#_v1(x)))',line)
    call dcl_uproc(parser,'_sublhs(x:any^mshape,t:index)=_make_subref(x,t)',line)
    call dcl_uproc(parser,&
         '_sublhs(x:any^any,t:subs)=new array_slice {_a=x,_s=fill_in(#x,t)}',line)
    call dcl_uproc(parser,'_sublhs(x:array_slice(any^mshape,),t:index)='//&
         '_make_subref(x._a,x._s[t])',line)
    call dcl_uproc(parser,'_sublhs(x:array_slice,t:subs)='//&
         'new array_slice {_a=x._a,_s=x._s[t]}',line)
    call dcl_uproc(parser,'_sublhs(x:any^dshape,t:index)='//&
         ' PM__ref(_arb(x),x,i,p,_s_ref)'//&
         ' where p,i=node_and_index((#x).dist,(#x)._mshape#_tup(t))',line)
    call dcl_uproc(parser,'_sublhs(a:^!(,,,,),t)='//&
         'PM__ref(_arb(_v1(a)),a,_tup(t),_v4(a),_v5(a))',line)
    call dcl_uproc(parser,'[](&a:array,v,arg...)'//&
         '{ PM__assign(&^(PM__sublhs(^(&a),_tup(arg...))),v)}',line)

    ! Realise a reference
    call dcl_uproc(parser,'PM__valref(x)=x',line)
    call dcl_uproc(parser,'PM__valref(x:^*(,,,,)) {'//&
         ' var v=_v1(x);if _v4(x)==_this_node() { v:=_getref(x,null)};'//&
         ' PM__broadcast(&v,_v4(x));return v }',line)
    !call dcl_uproc(parser,'PM__getref(x:^!(,,,,))=PM__valref(x)',line)
    
    ! Assign to a reference
    call dcl_uproc(parser,'PM__assign(&x:^*(,,,,),y) {'//&
         'check_assign_types(_v1(^(&x)),y);'//&
         'if _v4(^(&x))==_this_node() { PM__assign(&^(_getlhs(^(&x),null)),y) }}',line)
   
    ! *************************************************************
    ! DISTRIBUTED REFERENCES
    ! *************************************************************

    !  Distributed reference is an internal compiler type
    !  ^ ( value or value_example, parent, subscript, node or [indexed_dim,dshape] , mode)
    !  mode is:
    !       null         -- local reference
    !       _s_ref       -- shrd index on darray, only shrd/indexed otherwise
    !       _sp_ref      -- shrd index on darray, some priv after (or before)
    !       _d_ref       -- indexed index on darray, shrd/indexed otherwise
    !       _dp_ref      -- indexed index on darray, some priv after (or before)
    !       _p_ref       -- priv index on darray and possibly elsewhere

    call dcl_type(parser,'_s_ref is unique',line)
    call dcl_type(parser,'_sp_ref is unique',line)
    call dcl_type(parser,'_d_ref is unique',line)
    call dcl_type(parser,'_dp_ref is unique',line)
    call dcl_type(parser,'_p_ref is unique',line)

    call dcl_proc(parser,&
         '_import_dref%(r:any,s:any,h:any,x:any)->^^x',op_import_dref,0,line,0)

    ! Some trivial referencing cases
    call dcl_uproc(parser,'PM__sublhsamp%(x,t:subs)=PM__sublhs%(x,t)',line)
    call dcl_uproc(parser,'PM__sublhsamp%(x:any^dshape,t:subs)'//&
         ' {test "Cannot have subscript of a distributed array in ""&"" argument"=>''false;'//&
         ' return _arb(x)}',line)
    call dcl_uproc(parser,'PM__sublhs%(x,y)=PM__subref%(x,y)',line)
    call dcl_uproc(parser,'PM__sublhs%(x:priv ^*(,,,,),y)=PM__subref%(x,y)',line)
    call dcl_uproc(parser,'PM__sublhs%(x:priv,y)=PM__sublhs(x,y):'//&
         'test """sync"" assignment updating a private variable"=>''false',line)
    call dcl_uproc(parser,'PM__subref%(x:priv,y)=PM__subref(x,y)',line)
    call dcl_uproc(parser,'PM__sublhs%(x:priv,y:invar indexed)=PM__sublhs(x,*y)',line)
    call dcl_uproc(parser,'PM__subref%(x:priv,y:invar indexed)=PM__subref(x,*y)',line)
    call dcl_uproc(parser,'PM__subref%(region:mshape,x:invar any^mshape,y:index)=PM__subref(x,y)',line)
    call dcl_uproc(parser,'PM__subref%(region:mshape,x:invar any^mshape,y:subs)=PM__subref(x,y)',line)
    call dcl_uproc(parser,'PM__subref%(region:mshape,x:invar any^mshape,y:invar indexed)='//&
         'PM__subref(x,_dmap(y,here))',line)
    call dcl_uproc(parser,'PM__sublhs%(region:mshape,x:priv,y)=PM__sublhs(x,y):'//&
         'test """sync"" assignment updating a private variable"=>''false',line)
    call dcl_uproc(parser,'PM__sublhs%(region:mshape,x,y)=PM__sublhs(x,y)',line)
    call dcl_uproc(parser,'PM__subref%(region:mshape,x:any^dshape,y)=_arb(x)'//&
         ':test "Cannot subscript distributed array in ""for <<conc>>""" => ''false',line)
    call dcl_uproc(parser,'PM__subref%(region:mshape,x:invar any^dshape,y:invar indexed)='//&
         '_arb(x)'//&
         ':test "Cannot subscript distributed array in ""for <<conc>>""" => ''false',line)
    call dcl_uproc(parser,'PM__sublhs%(region:mshape,x:any^dshape,y)=PM__subref%(x,y)',line)
    call dcl_uproc(parser,'PM__subref%(x,y:indexed_dim)=PM__subref%(x,tuple(y <<shared>>))',line)

    
    ! Reference of non-distributed array with priv or indexed subscript
    call dcl_uproc(parser,'PM__subref%(x:invar any^mshape,t:index)'//&
         '{tt=_tup(t);check_contains(#x,tt);i=index(#x,tt);'//&
         'return PM__dref(_get_aelem(x,i),x,i,null,null)}',line)
    call dcl_uproc(parser,'PM__subref%(x:invar any^mshape,t:subs)'//&
         '{tt=_tup(t);check_contains(#x,tt);return PM__drefs(x,x,tt,null,null)}',line)
    call dcl_uproc(parser,'PM__subref%(x:invar any^mshape,t:invar indexed)='//&
         'PM__subref%(x,_dmap(t,here))',line)
    call dcl_uproc(parser,&
         'PM__subref%(x:invar array_slice,t,m)=PM__subref%(x._a,x._s[t])',line)
    call dcl_uproc(parser,'PM__subref%(x,t)='//&
         'PM__dref($[](x,t),x,t,null,null)',line)
    
    ! Subscript or slice of distributed array
    call dcl_uproc(parser,'PM__subref%(x:shared any^dshape,t:invar index) <<complete,always>>'//&
         '{tt=_tup(t <<shared>>);check_contains(#(x),tt);'//&
         'return PM__dref(_arb(x),x,i,p,_s_ref) '//&
         'where p,i=node_and_index((#x).dist,(#x)._mshape#tt)}',line)
    call dcl_uproc(parser,'PM__subref%(x:shared any^dshape,t:index)'//&
         '{tt=_tup(t);check_contains(#(x),tt);'//&
         'return PM__dref(_arb(x),x,i,p,_p_ref) '//&
         'where p,i=node_and_index((#x).dist,(#x)._mshape#tt)}',line)
    call dcl_uproc(parser,'PM__subref%(x:shared any^dshape,t:subs)'//&
         '{tt=_tup(t);check_contains(#(x),tt);var xx=varray(_arb(x),empty(#x));'//&
         'return PM__drefs(xx,x,tt,p,_p_ref) '//&
         'where p=nodes_for_grid((#x).dist,tt)}',line)
    call dcl_uproc(parser,'PM__subref%(x:shared any^dshape,t:invar indexed) <<complete,always>> {'//&
         'check_contains(#x,_dmap(t,here));'//&
         'return PM__drefi(_arb(x),x,tt,[tt,#x],_d_ref) where tt=_tup(t)}',line)

    ! Subscript or slice of non-distristuted array which is itself result of variant slice
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^mshape,,,null,null),t:index)'//&
         '{tt=_tup(t);check_contains(#_v1%(x),tt);i=index(#(_v1%(x)),tt)'//&
         'return PM__dref(_get_aelem(_v1%(x),i),x,i,null,null)}',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^mshape,,,null,null),t:invar index){'//&
         'tt=_tup(t <<shared>>);check_contains(#_v1%(x),tt);i=index(#(_v1%(x)),tt);'//&
         'return PM__drefi(_get_aelem(_v1%(x),i),x,t,null,null)}',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^mshape,,,null,null),t:subs) {'//&
         'tt=_tup(t);check_contains(#(_v1%(x)),tt);'//&
         'return PM__drefs(PM__import_val(_v1%(x)),x,tt,null,null)}',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^mshape,,,null,null),t:invar indexed)='//&
         'PM__subref%(x,_dmap(t,here))',line)

    ! Subscript or slice of darray which is itself the result of a priv subscript
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^dshape,,,null,null),t:index)'//&
         '{tt=_tup(t);check_contains(#_v1%(x),tt);'//&
         'return PM__dref(_arb(_v1%(x)),_v2%(x),i,p,_p_ref) '//&
         'where p,i=node_and_index((#_v1%(x)).dist,(#_v1%(x))._mshape#tt)}',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^dshape,,,null,null),t:subs)'//&
         '{tt=_tup(t);check_contains(#_v1%(x),tt);'//&
         'return PM__drefs(PM__import_val(_v1%(x)),x,tt,p,_p_ref) '//&
         'where p=nodes_for_grid((#_v1%(x)).dist,tt)}',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^dshape,,,null,null),t:invar indexed)='//&
         'PM__subref%(x,_dmap(t,here))',line)

    ! Subscript of a priv slice
    call dcl_uproc(parser,'PM__subref%(x:priv ^#(,,,null,null),t:subs)='//&
         'PM__subref%(_v2%(x),_v3%(x)[_tup(t)])',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^#(,,,null,null),t:invar indexed)='//&
         'PM__subref%(_v2%(x),_v3%(x)[tt]) where tt=_dmap(t,here)',line)
    
    ! Subscript of distributed reference
    call dcl_uproc(parser,'_arb%(x:partial)=_arb(x)',line)
    call dcl_uproc(parser,'_arb%(x:complete)=_arb(x <<complete,always>>)',line)
    call dcl_uproc(parser,'_arb%(x:chan)=_arb(x <<complete,always>>)',line)
    call dcl_uproc(parser,'_arb%(x:invar)=_arb(x <<complete,always>>)',line)
    
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^any,,,,),t:invar subs)='//&
         'PM__drefi(_arb%(_v1%(x)),x,_tup(t <<shared>>))',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^any,,,,),t:priv subs)='//&
         'PM__dref(_arb%(_v1%(x)),x,_tup(t))',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^any,,,,_s_ref),t:priv subs)='//&
         'PM__dref(_arb%(_v1%(x)),x,_tup(t),_v4%(x),_sp_ref)',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^any,,,,),t:invar indexed)='//&
         'PM__subref%(x,_dmap(_tup(t),here))',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^any,,,,_s_ref),t:invar indexed)='//&
         'PM__drefi(_arb%(_v1%(x)),x,_tup(t))',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^any,,,,_d_ref),t:invar indexed)='//&
         'PM__drefi(_arb%(_v1%(x)),x,_tup(t))',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^any,,,,_d_ref),t:invar subs)='//&
         'PM__drefi(_arb%(_v1%(x)),x,_tup(t <<shared>>))',line)
    call dcl_uproc(parser,'PM__subref%(x:priv ^*(any^any,,,,_d_ref),t:priv subs)='//&
         'PM__dref(_arb%(_v1%(x)),x,_tup(t),_v4%(x),_dp_ref)',line)

    ! Node .[] subscript of distributed array
    call dcl_type(parser,'_lcl is unique{_LCL}',line)
    call dcl_uproc(parser,'PM__nodelhs%(x,y)=PM__noderef%(x,y)',line)
    call dcl_uproc(parser,'PM__noderef%(region:dshape,x:shared any^dshape,y:invar null)='//&
         '^(PM__import_val(PM__local(x)),coherent)',line)
    call dcl_uproc(parser,'PM__noderef%(region:dshape,x:shared any^dshape,y:invar any_int){'//&
         'xd=#((#x).dist);check_contains(xd,y);'//&
         'return PM__drefi(PM__local(x),x,_LCL,p,_s_ref)'//&
         'where p=index(xd,int(y))}',line)
    call dcl_uproc(parser,'PM__noderef%(region:dshape,x:shared any^dshape,y:priv any_int){'//&
         'xd=#((#x).dist);check_contains(xd,y);'//&
         'return PM__drefi(PM__local(x),x,_LCL,p,_p_ref)'//&
         'where p=index(xd,y)}',line)
    call dcl_uproc(parser,'PM__noderef%(region:dshape,x:shared any^dshape,y:shared indexed)='//&
         'PM__noderef%(x,*y)',line)
    call dcl_uproc(parser,'PM__noderef%(x:priv ^*(any^dshape,,,null,null),y:invar any_int){'//&
         'xd=#((#x).dist);check_contains(xd,y);'//&
         'return PM__drefi(PM__local(_v1%(x)),x,_LCL,p,_sp_ref)'//&
         'where p=index(xd,int(y))}',line)
    call dcl_uproc(parser,'PM__noderef%(x:priv ^*(any^dshape,,,null,null),y:priv any_int){'//&
         'xd=#((#x).dist);check_contains(xd,y);'//&
         'return PM__drefi(PM__local(_v1%(x)),x,_LCL,p,_p_ref)'//&
         'where p=index(xd,int(y))}',line)
    call dcl_uproc(parser,'PM__noderef%(x:priv ^*(any^dshape,,,null,null),y:shared indexed)='//&
         'PM__noderef%(x,*y)',line)
    call dcl_uproc(parser,'PM__noderef%(x,y)=error_type() {'//&
         'if not region is <dshape> {'//&
         '  test """.[]"" subscript in non-distributed region"=>''false'//&
         '} elseif x is <any^mshape> {'//&
         '  test """.[]"" subscript cannot be applied to a mirrored array"=>''false'//&
         '} elseif not x is <any^any> {'//&
         '  test """.[]"" subscript applied to a non-array"=>''false'//&
         '} elseif not y is <any_int> {'//&
         '  test """.[]"" subscript must have an integer value"=>''false'//&
         '} else {'//&
         '  test "Incorrect "".[]"" subscript"=>''false'//&
         '}}',line)
    
    ! May need to cap off reference with here
    call dcl_type(parser,'_here(t) is rec {here:t}',line)
    call dcl_uproc(parser,'_cap%(x,h)<<inline>>=x',line)
    call dcl_uproc(parser,'_cap%(x:contains(indexed),h)<<inline>>=PM__dref(_v1%(x),x,new _here {here=h})',line)
    call dcl_uproc(parser,'_capn%(x,h)<<inline>>=PM__dref(_v1%(x),x,new _here {here=h})',line)

    ! Treat @ variables differently only for limited circumstances in drefs
    call dcl_uproc(parser,'_drat(at,tile,t)=''false',line)
    call dcl_uproc(parser,'_drat(at:''true,tile:tuple(range or block_seq),t:indexed and _dr)=''true',line)
    call dcl_type(parser,'_di(n) is indexed_dim(''1,''1,,n) or int',line)
    call dcl_type(parser,'_dr is [_di(''1)],[_di(''1),_di(''2)],'//&
         '[_di(''1),_di(''2),_di(''3)],[_di(''1),_di(''2),_di(''3),_di(''4)],'//&
         '[_di(''1),_di(''2),_di(''3),_di(''4),_di(''5)],'//&
         '[_di(''1),_di(''2),_di(''3),_di(''4),_di(''5),_di(''6)],'//&
         '[_di(''1),_di(''2),_di(''3),_di(''4),_di(''5),_di(''6),_di(''7)]',line)
    
    ! Resolve a distributed reference
    call dcl_uproc(parser,'PM__getref%(x,at)=x',line)
    call dcl_uproc(parser,'PM__getref%(x:priv ^*(,,,null,null),at)=_v1%(x)',line)
    call dcl_uproc(parser,'PM__getref%(x:priv ^*(,,,int,_p_ref),at) {'//&
         'PM__recv pp,xx,v,_cap%(x,here),_v4%(x),at,_getref(xx,null);return v}',line)
    call dcl_uproc(parser,'PM__getref%(x:priv ^*(,,,int,_sp_ref),at) {'//&
         'PM__serve pp,xx,v,_cap%(x,here),_v4%(x),at,_getref(xx,null);return v}',line)
    call dcl_uproc(parser,'_scatter(x,region) {'//&
         'if _this_node()==_v4(x):foreach node in #region.dist {'//&
         '  d=#region._mshape;a={_getref(_import_dref%(x),j): j in d};'//&
         '  p=index(dims(region.dist),node);'//&
         '  _send_slice(p,a,region.dist[node])}}',line)
    call dcl_uproc(parser,'PM__getref%(x:complete ^*(,,,int,_s_ref),at:invar) <<complete,always>> {'//&
         'chan var xx=_v1%(x);_getref_s(&xx@,region,^^(x),at <<PM__node>>);_bcast_shared(&xx);return xx}',line)
    call dcl_uproc(parser,'_getref_s(&xx,region,x,at) {'//&
         'PM__head_node{_irecv(_v4(x),&xx)};'//&
         '_scatter(x,region);'//&
         '_sync_messages(xx,x)}',line)
    call dcl_uproc(parser,'PM__getref%(x:complete _comp and ^*(,,,int,_s_ref),at:invar) <<complete,always>>{'//&
         'chan var xx=_v1%(x);_getref_sc(&xx@,region,^^(x),at <<PM__node>>);_bcast_shared(&xx);return xx}',line)
    call dcl_uproc(parser,'_getref_sc(&xx,region,x,at) {'//&
         '_scatter(x,region);PM__head_node{_recv(_v4(x),&xx)};'//&
         '_sync_messages(xx,x)}',line)
    call dcl_uproc(parser,'PM__getref%(x:complete ^*(,^*(,,,,),,,_d_ref),at:invar) <<complete,always>> {'//&
         'chan var a=_v1%(x);_getref_d(&^(PM__local(^(&a@) <<shared>>)),region,subregion(schedule),'//&
         '^^(x),at  <<PM__node,PM__ignore>>);'//&
         '_bcast_shared(&a);return a}',line)
    call dcl_uproc(parser,'_getref_d(&a,region,subregion,x,at) {'//&
         '_get_dindex_from_dref(&a,x,t.2,'//&
         '_local_region(region._tile,subregion),region,t.1,'//&
         '_drat(at,region._tile,t.1)) where t=_v4(x)'//&
         '}',line)
    call dcl_uproc(parser,'PM__getref%(x:complete ^*(,,,,_d_ref),at:invar) <<complete,always>> {'//&
         'chan var a=_arb(_v2%(x));'//&
         '_getref_dc(&a@,region,subregion(schedule),^^(x),at <<PM__node,PM__ignore>>);_bcast_shared(&a);return a}',line)
    call dcl_uproc(parser,'_getref_dc(&a,region,subregion,x,at) {'//&
         'PM__head_node{_get_dindex(&^(PM__local(^(&a))),PM__local(_v2(x)),t.2,'//&
         '_local_region(region._tile,subregion),region,t.1,_drat(at,region._tile,t.1)) '//&
         ' where t=_v4(x)}}',line)
    call dcl_uproc(parser,'PM__getref%(x:complete ^*(,,,,_dp_ref),at:invar) <<complete,always>> {'//&
         'chan var a=_v1%(x);'//&
         '_getref_dp(&^(^^(^(&a))),region,subregion(schedule),^^(_cap%(x,here)),at,^^(^??),_v4(x) <<PM__node,PM__ignore>>);'//&
         '_bcast_shared(&a);return a}',line)
    call dcl_uproc(parser,'_getref_dp(&a,region,subregion,x,at,atq,t) {'//&
         'PM__head_node{_get_dindex_from_ref(&a,x,t.2,'//&
         ' _local_region(region._tile,subregion),region,'//&
         ' t.1,atq,_drat(at,region._tile,t.1))}'//&
         '}',line)
    
    call dcl_uproc(parser,'PM__getref%(x:priv ^#(,,,,),at) {'//&
         'var v=varray(_arb(_v1%(x)),#_v3%(x));'//&
         'var vv=varray(_arb(_v1%(x)),empty(#_v1%(x)));'//&
         'foreach p in _v4%(x) {'//&
         '  dist=(#(_getref(_v2%(x),null))).dist;'//&
         '  u=overlap(_v3%(x),dist[p]);'//&
         '  ppp=index(dims(dist),p);'//&
         '  PM__recv pp,xx,vvv,_cap%(x,here),ppp,at,_getref(xx,null);'//&
         '  v[u]:=vvv};return v}',line)


    ! Resolve reference locally (once communicated)
    call dcl_uproc(parser,'_getref_elem(x:any^mshape,i)=_get_aelem(x,i)',line)
    call dcl_uproc(parser,&
         '_getref_elem(x:any^dshape,i)=_get_aelem(PM__local(x),i)',line)
    call dcl_uproc(parser,&
         '_getref(x:^*(,,int,,),y)=_getref_elem(_getref(_v2(x),y),_v3(x))',line)
    call dcl_uproc(parser,'_getref(x:^*(,,_here,,),y:null)<<inline>>=_getref(_v2(x),_v3(x).here)',line)
    call dcl_uproc(parser,'_getref(x:^*(,,subs,,),y)<<inline>>=_getref(_v2(x),y)[_v3(x)]',line)
    call dcl_uproc(parser,'_getref(x:^*(,,null,,),y)<<inline>>=_getref(_v2(x),y)',line)
    call dcl_uproc(parser,'_getref(x:^*(,,_lcl,,),y)<<inline>>=PM__local(_getref(_v2(x),y))',line)
    call dcl_uproc(parser,'_getref(x:^.(,,,,),y)<<inline>>=_getref(_v2(x),y).^(x)',line)
    call dcl_uproc(parser,'_getref(x:^shared(,null,null,null,null),y)<<inline>>=_v1(x)',line)
    call dcl_uproc(parser,'_getref(x:^#(,,,,),y)<<inline>>=_getslice(_getref(_v2(x),y),_v3(x))',line)
    call dcl_uproc(parser,'_getslice(x:any^dshape,tt) {t=overlap((#x)._tile,tt);'//&
         'var v=varray(_arb(x),#t);v:=PM__local(x)[t];return v}',line)
    call dcl_uproc(parser,'_getslice(x:any^mshape,t) {'//&
         'var v=varray(_arb(x),#t);v:=x[t];return v}',line)
    call dcl_uproc(parser,'_getref(x:any^any,y)=x',line)
    call dcl_uproc(parser,'_getref(x:^shared(,,indexed,,),y)<<inline>>='//&
         '_getref(_v2(x),y)[_dmap(_v3(x),y)]',line)
    call dcl_uproc(parser,'_getref(x:^shared(,any^dshape,indexed,,),y)<<inline>>='//&
         '_get_elem(PM__local(_v2(x)),ms._tile#_dmap(_correct(_v3(x),ms._mshape._extent),y)) '//&
         'where ms=#_v2(x)',line)
    call dcl_uproc(parser,'_getref(x:^shared(,,indexed,,),y:null)<<inline>>=_v1(x) '//&
         ':test "Internal error - uncapped dref" => ''false',line)


    if(pm_is_compiling) then
       call dcl_proc(parser,'_sync%(any,any,any,&x:any)',op_sync,0,line,0)
    else
       call dcl_uproc(parser,'_sync%(&x:any){}',line)
    endif
    
    ! Assignment of distributed and/or shared or uniform references
    call dcl_uproc(parser,'PM__assign%(&x:priv,y,at) {'//&
         '_sync%(&x);PM__assign(&x,y <<PM__ignore>>)}',line)
    call dcl_uproc(parser,'PM__assign%(&x:invar,y,at) {_assign_to_invar%(&x,y) }',line)
    call dcl_uproc(parser,'_assign_to_invar%(&x:uniform,y:invar) '//&
         '{ _sync%(&x);PM__assign(&x,y <<PM__ignore,complete>>) }',line)
    call dcl_uproc(parser,'_assign_to_invar%(&x:shared,y:invar) '//&
         '{ _sync%(&x);PM__assign(&x,y <<shared>>) }',line)
    call dcl_uproc(parser,&
         '_assign_to_invar%(&x:invar,y:priv) '//&
         '{ test "Can only assign an ""invar"" value to an ""invar"" variable" => ''false }',line)
 
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,int,_p_ref),y,at) {'//&
         'PM__send p,xx,yy,_cap%(x,here),_v4%(x),y,at { PM__assign(&^(_getlhs(^(&xx),null)),yy)};'//&
         'PM__send p,xx,yy,_cap%(x,here),_v4%(x),y,''false {'//&
         '  test "Cannot assign element twice in same assignment"=>'//&
         '    _getref(xx,null)==yy } }',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,int,_p_ref),y:invar,at) {'//&
         'PM__send p,xx,yy,_cap%(x,here),_v4%(x),null,at { PM__assign(&^(_getlhs(^(&xx),null)),y) }}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,null,null),y,at) {'//&
         'foreach invar p in 0..size(#region.dist)-1 {'//&
         '  PM__bcast xx,yy,_cap%(x,here),y,p {'//&
         '     PM__assign(&^(_getlhs(^(&xx),null)),yy) }};'//&
         'foreach invar p in 0..size(#region.dist)-1 {'//&
         '  PM__bcast xx,yy,_cap%(x,here),y,p {'//&
         '     test "Cannot assign an element two different values in a single assignment"=>'//&
         '        _getref(xx,null)==yy}}}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,null,null),y:invar,at) {'//&
         '_sync%(&x);var xx=_import_dref%(x);PM__assign(&^(_getlhs(^(&xx),here)),y)}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,null,null,null,null),y:invar,at) {'//&
         '_sync%(&x);PM__assign(&^(_v1%(^(&x))),y)}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,int,_sp_ref or _s_ref),y,at) {'//&
         'PM__collect p,xx,yy,_cap%(x,here),_v4%(x),y,at { PM__assign(&^(_getlhs(^(&xx),null)),yy)};'//&
         'PM__collect p,xx,yy,_cap%(x,here),_v4%(x),y,''false '//&
         '   { test "Cannot assign element to two different values in same assignment"=>'//&
         '  _getref(xx,null)==yy }}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,int,_sp_ref or _s_ref),y:invar,at) {'//&
         'PM__collect p,xx,yy,_cap%(x,here),_v4%(x),null,at '//&
         '{ PM__assign(&^(_getlhs(^(&xx),null)),PM__import_val(y))}}',line)
    
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,,_d_ref or _dp_ref),y,at) {'//&
         '_set_ref_dp(&^(^^(_cap%(^(&x),here))),^(^^(y)),'//&
         ' region,subregion(schedule),$_just_assign,^^(^??),at,_v4(x) <<PM__node,PM__ignore>>)}',line)
    call dcl_uproc(parser,'_just_assign(x,y)=y',line)
    call dcl_uproc(parser,'_set_ref_dp(&x,y,region,subregion,prc,atq,at,t) {'//&
         '_set_dindex_of_ref(&x,y,t.2,'//&
         '_local_region(region._tile,subregion),'//&
         'region,t.1,prc,atq,at)'//&
         '}',line)
    
    ! Operater assignment: x <proc>=y
    call dcl_uproc(parser,'PM__assign%(&x:priv,y:priv,pr,at) {PM__assign(&x,y,pr)}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv,y:invar,pr,at) {PM__assign(&x,y,pr)}',line)
    call dcl_uproc(parser,'PM__assign%(&x:invar,y,pr,at) { _assign_to_invar%(&x,y,pr,at) }',line)
    call dcl_uproc(parser,'_assign_to_invar%(&x:uniform,y:invar,pr,at) '//&
         '{ PM__assign(&x,y,pr <<complete,PM__ignore>>) }',line)
    call dcl_uproc(parser,'_assign_to_invar%(&x:shared,y:invar,pr,at) '//&
         '{ PM__assign(&x,y,pr <<shared>>) }',line)
    call dcl_uproc(parser,'_assign_to_invar%(&x:invar,y:priv,pr,at){'//&
         '_assign_to_invar%(&x,_reduce_for_assign%(pr,y,x),pr,at)}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,int,_p_ref),y,pr,at) {'//&
         'PM__send p,xx,yy,_cap%(x,here),_v4%(x),y,at { PM__assign(&^(_getlhs(^(&xx),null)),yy,pr)}}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,int,_p_ref),y:invar,pr,at) {'//&
         'PM__send p,xx,yy,_cap%(x,here),_v4%(x),null,at { PM__assign(&^(_getlhs(^(&xx),null)),y,pr) }}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,null,null),y:priv,pr,at) {'//&
         'foreach invar p in 0..size(region.dist)-1 {'//&
         '  PM__bcast xx,yy,_cap%(x,here),y,p {'//&
         '     PM__assign(&^(_getlhs(^(&xx),null)),yy,pr) }}}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,null,null),y:invar,pr,at) {'//&
         'var xx=_import_dref%(x);PM__assign(&^(_getlhs(^(&xx),here)),y,pr)}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,null,null,null,null),y:invar,pr,at) {'//&
         'PM__assign(&^(_v1%(^(&x))),y,pr)}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,int,_sp_ref or _s_ref),y:priv,pr,at) {'//&
         'PM__collect p,xx,yy,_cap%(x,here),_v4%(x),y,at { PM__assign(&^(_getlhs(^(&xx),null)),yy,pr)}}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,int,_sp_ref),y:invar,pr,at) {'//&
         'PM__collect p,xx,yy,_cap%(x,here),_v4%(x),null,at { PM__assign(&^(_getlhs(^(&xx),null)),y,pr)}}',line)
    call dcl_uproc(parser,'PM__assign%(&x:priv ^*(,,,,_d_ref or _dp_ref),y:priv,pr,at) {'//&
         '_set_dindex_of_ref(&^(^^(_cap%(^(&x),here))),^^(y),t.2,'//&
         '_local_region(region._tile,subregion(schedule)),'//&
         'region,t.1,pr,^^(^??),at <<PM__node,always,PM__ignore>>)'//&
         'where t=_v4%(x)}',line)
    
    ! Resolve LHS reference (locally after communication)
    call dcl_uproc(parser,'_getlhs(x:^*(,,_here,,),y)=_getlhs(_v2(x),_v3(x).here)',line)
    call dcl_uproc(parser,'_getlhs(x:^*(,,_lcl,,),y)=_getlhs(_v2(x),y)',line)
    call dcl_uproc(parser,'_getlhs(x:^(,,int,,),y)='//&
         '_make_subref(_getlhs(_v2(x),y),int(_v3(x)))',line)
    call dcl_uproc(parser,'_getlhs(x:^shared(,,null,,),y)=_getlhs(_v2(x),y)',line)
    call dcl_uproc(parser,'_getlhs(x:^shared(,,int,,),y)='//&
         '_make_subref(_getlhs(_v2(x),y),int(_v3(x)))',line)
    call dcl_uproc(parser,'_getlhs(x:^(,,subs,,),y)='//&
         'PM__sublhs(_getlhs(_v2(x),y),_v3(x))',line)
    call dcl_uproc(parser,'_getlhs(x:^shared(,,subs,,),y)='//&
         'PM__sublhs(_getlhs(_v2(x),y),_v3(x)) ',line)
    call dcl_uproc(parser,&
         '_local_ref(x,t)=PM__subref(x,overlap((#x)._tile,t))',line)
    call dcl_uproc(parser,'_getlhs(x:^#(,,subs,,),y)<<inline>>=_local_ref(x,_v3(x))',line)
    call dcl_uproc(parser,'_getlhs(x:^#shared(,,subs,,),y)<<inline>>=_local_ref(x,_v3(x))',line)
    call dcl_uproc(parser,'_getlhs(x:^.(,,,,),y)<<inline>>=_getlhs(_v2(x),y).^&(x)',line)
    call dcl_uproc(parser,'_getlhs(x:^shared(,null,null,null,null),y)<<inline>>=_v1(x)',line)
    call dcl_uproc(parser,'_getlhs(x:^(,null,null,null,null),y)<<inline>>=_v1(x)',line)
    call dcl_uproc(parser,'_getlhs(x:any^any,y)=x',line)
    call dcl_uproc(parser,'_getlhs(x:any^dshape,y)=PM__local(x)',line)
    call dcl_uproc(parser,'_getlhs(x:^shared(,,indexed,,),y)<<inline>>='//&
         '_make_subref(_getlhs(_v2(x),y),_dmap(_v3(x),y))',line)
    call dcl_uproc(parser,'_getlhs(x:^shared(,any^dshape,indexed,,),y)<<inline>>='//&
         '_make_subref(PM__local(_v2(x)),ms._tile#_dmap(_correct(_v3(x),ms._mshape._extent),y)) '//&
         'where ms=#_v2(x)',line)
    
    call dcl_uproc(parser,'_getlhs(x:^shared(,,indexed,,),y:null)='//&
         '_v1(x) :test "Internal error -- uncapped indexed ref" => ''false',line)
    
    !**************************************************************
    ! INDEXED VARIABLES
    !**************************************************************

    call dcl_type(parser,'indexed_dim(d:int,m:int,c:int,n:int) is rec {_m:m=''1,_c:c=''0,_d:d=''1,_n:n}',line)
    call dcl_type(parser,'indexed(t:int) is tuple(indexed_dim or int) except tuple(int)',line)
    call dcl_uproc(parser,'PM__makeidxdim(x:null,y)=new indexed_dim {_n=y}',line)
    call dcl_uproc(parser,'PM__makeidxdim(x:null)=new indexed_dim {_n=null}',line)
    call dcl_uproc(parser,'PM__makeidxdim(x:range,y)=new indexed_dim {_c=x._lo,_n=y}',line)
    call dcl_uproc(parser,'PM__makeidxdim(x:strided_range,y)=new indexed_dim {_c=x._lo,_m=x._st,_n=y}',line)
    call dcl_uproc(parser,'PM__makeidxdim(x,y)=PM__makeidxdim(get_dim(x,y),y)',line)
    call dcl_uproc(parser,'PM__makeidxdim(x:tuple)'//&
         '=map($PM__makeidxdim,x,indices(x))',line)
    call dcl_uproc(parser,'PM__makeidxdim(x:seq)=[PM__makeidxdim(x,''1)]',line)


    !!! Obsolete?
    call dcl_uproc(parser,'PM__makeidx(x:indexed_dim or tuple(indexed_dim or int))='//&
         'new _indexed {_t=_tup(x),_r=null}',line)
    call dcl_uproc(parser,'PM__makeidx(x:indexed_dim or tuple(indexed_dim or int),y)='//&
         'new _indexed {_t=_tup(x),_r=y}',line)
    call dcl_uproc(parser,'PM__makeidx(x,y)=x :test "Malformed indexed expression" => ''false',line)
    call dcl_uproc(parser,'PM__makeidx(x)=x :test "Malformed indexed expression" => ''false',line)

    call dcl_uproc(parser,'*%(x:indexed)=_dmap(x,here)',line)
    call dcl_uproc(parser,'*%(x)=here check'//&
         '"""*"" operator can only be applied to an ""indexed"" value"=>''false',line)
    call dcl_uproc(parser,'*(x)=x check'//&
         '"""*"" operator cannot be applied outside of a parallel context"=>''false',line)
    
    call dcl_uproc(parser,'+(x:indexed_dim,yy:any_int)='//&
         'new indexed_dim {_m=x._m,_c=x._c+y*x._d,_d=x._d,_n=x._n} where y=int(yy)',line)
    call dcl_uproc(parser,'+(yy:any_int,x:indexed_dim)='//&
         'new indexed_dim {_m=x._m,_c=x._c+y*x._d,_d=x._d,_n=x._n} where y=int(yy)',line)
    call dcl_uproc(parser,'-(x:indexed_dim,yy:any_int)='//&
         'new indexed_dim {_m=x._m,_c=x._c-y*x._d,_d=x._d,_n=x._n} where y=int(yy)',line)
    call dcl_uproc(parser,'-(yy:any_int,x:indexed_dim)='//&
         'new indexed_dim {_m=-x._m,_c=-x._c+y*x._d,_d=x._d,_n=x._n} where y=int(yy)',line)
    call dcl_uproc(parser,'*(x:indexed_dim,yy:any_int)='//&
         'new indexed_dim {_m=x._m*y,_c=y*x._c,_d=x._d,_n=x._n} where y=int(yy)',line)
    call dcl_uproc(parser,'*(yy:any_int,x:indexed_dim)='//&
         'new indexed_dim {_m=x._m*y,_c=y*x._c,_d=x._d,_n=x._n} where y=int(yy)',line)
    call dcl_uproc(parser,'/(x:indexed_dim,yy:any_int)='//&
         'new indexed_dim {_m=x._m,_c=x._c,_d=x._d*y,_n=x._n} where y=int(yy)',line)
    call dcl_uproc(parser,'+(x:indexed_dim,y:indexed_dim)='//&
         'new indexed_dim {_m=x._m*y._d+y._m*x._d,_c=x._c*y._d+y._c*x._d,_d=x._d*y._d,_n=x._n}',line)
    call dcl_uproc(parser,'-(x:indexed_dim,y:indexed_dim)='//&
         'new indexed_dim {_m=x._m*y._d-y._m*x._d,_c=x._c*y._d-y._c*x._d,_d=x._d*y._d,_n=x._n}',line)

    call dcl_uproc(parser,'string(x:indexed_dim)="($here."++x._n++"*"++x._m++"+"++x._c++")/"++x._d',line)
    call dcl_uproc(parser,'string(x:indexed_dim(''1))="$here."++x._n++"*"++x._m++"+"++x._c',line)
    call dcl_uproc(parser,'string(x:indexed_dim(''1,''1))="$here."++x._n++"+"++x._c',line)
    call dcl_uproc(parser,'string(x:indexed_dim(''1,''1,''0))="$here."++x._n',line)

    call dcl_uproc(parser,'_correct(x:tuple(indexed_dim),y:extent)=map($-,x,low(y))',line)

    call dcl_uproc(parser,'_dmap(x:any_int,n:int)=x',line)
    call dcl_uproc(parser,'_dmap(x:any_int,n:grid_slice_dim)=single_point(x)',line)
    call dcl_uproc(parser,'_dmap(x:any_int,n:tuple(int))=x',line)
    call dcl_uproc(parser,'_dmap(x:any_int,n:tuple(grid_slice_dim))=single_point(x)',line)
    call dcl_uproc(parser,'_dmap(x:indexed_dim,n:int)=(n*x._m+x._c)/x._d',line)
    call dcl_uproc(parser,'_dmap(x:indexed_dim,n:tuple)=_dmap(x,get_dim(n,x._n))',line)
    call dcl_uproc(parser,'_dmap(x:indexed_dim,n:grid_slice_dim)=min(lo,hi)..max(lo,hi)'//&
         'where lo=_dmap(x,low(n)),hi=_dmap(x,high(n))',line)
    call dcl_uproc(parser,'_dmap(x:indexed_dim(''1,''1),n:strided_range)=n._lo+x._c..n._hi+x._c  by n._st',line)
    call dcl_uproc(parser,'_dmap(x:indexed_dim(''1,''1),n:block_seq)='//&
         'block_seq(n._lo+x._c,n._hi+x._c,n._st.n._b,n._align)',line)
    call dcl_uproc(parser,'_dmap(x:tuple(indexed_dim or any_int),n:tuple(int) or grid_slice)='//&
         'map_const($_dmap,x,n)',line)
    call dcl_uproc(parser,'_dmap(x:tuple(indexed_dim or any_int),n:tuple(int) or grid_slice,s:extent)='//&
         's#map_const($_dmap,x,n)',line)

    call dcl_uproc(parser,'_drev(x:indexed_dim,n)=new indexed_dim {_m=x._d,_c=-x._c,_d=x._m,_n=n}',line)
    call dcl_uproc(parser,'_drev(n,x:tuple(indexed_dim))=_drev(get_dim(x,n),n)',line)
    call dcl_uproc(parser,'_drev(x:tuple(indexed_dim),y:tuple(fix int))=map_const($_drev,y,x)',line)

    call dcl_type(parser,'_round_up is unique',line)
    call dcl_type(parser,'_round_down is unique',line)
    call dcl_uproc(parser,'_dunmap(x:indexed_dim,n:null)=null',line)
    call dcl_uproc(parser,'_dunmap(x:indexed_dim,n:int,r:_round_down)=(n*x._d-x._c)/x._m',line)
    call dcl_uproc(parser,'_dunmap(x:indexed_dim,n:int,r:_round_up)=(n*x._d+x._m-sign(1,x._m)-x._c)/x._m',line)
    call dcl_uproc(parser,'_dunmap(x:indexed_dim,n:tuple)=_dunmap(get_dim(n,x._n),x)',line)
    call dcl_uproc(parser,'_dunmap(x:indexed_dim,n:grid_slice_dim)='//&
         'min(lo,hi)..max(lo,hi) where lo=_dunmap(x,low(n),_round_down),hi=_dunmap(x,high(n),_round_up)',line)
    call dcl_uproc(parser,&
         '_dun(x:indexed_dim,m:range(int),n:extent)=replace(n,x._n,intersect(get_dim(n,x._n),_dunmap(x,m)))',line)
    call dcl_uproc(parser,'_dun(x:int,m:range(int),n:extent)=n',line)
    call dcl_uproc(parser,'_dunmap(x:tuple(indexed_dim or int),m:grid_slice or tuple(int),n:extent)='//&
         '_dun(x.1,m.1,nn) where nn=_dunmap(tail(x),tail(m),n)',line)
    call dcl_uproc(parser,'_dunmap(x:[indexed_dim or int],m:grid_slice or tuple(int),n:extent)='//&
         '_dun(x.1,m.1,n)',line)

    ! Given tile and global region (which may be null) compute local region
    call dcl_uproc(parser,'_local_region(t,r:null)=t',line)
    call dcl_uproc(parser,'_local_region(t,r)=intersect(t,r)',line)

    call dcl_uproc(parser,'_root_node(at:''true)=_root_node()',line)
    call dcl_uproc(parser,'_root_node(at:''false)=_this_node()',line)

    ! Resolve x[indexed]
    call dcl_uproc(parser,&
         '_get_dindex(&a,x,shapex,local_tile,local_region,t:tuple(indexed_dim),at) {'//&
         'tt=_correct(t,shapex._mshape);'//&
         'if size(_dmap(tt,local_region._mshape))*4>size(local_region._mshape) {'//&
         '  if at or a is <_comp^any> {'//&
         '   _get_dindex_ss(&a,x,shapex,local_tile,local_region,tt,at);'//&
         '  } else {'//&
         '   _get_dindex_s(&a,x,shapex,local_tile,local_region,tt,at);'//&
         '  }'//&
         '} else {'//&
         '  if at or a is <_comp^any> {'//&
         '   _get_dindex_rs(&a,x,shapex,local_tile,local_region,tt,at);'//&
         '  } else {'//&
         '   _get_dindex_r(&a,x,shapex,local_tile,local_region,tt,at);'//&
         '  }'//&
         '}}',line)

    ! Resolve x[indexed] for cases where size(x[indexed])>=size(region)
    ! -- in this case send one value for every point in current (sub)region
    call dcl_uproc(parser,&
         '_get_dindex_s(&a:any^any,x,shapex,local_tile,local_region,t:tuple(indexed_dim),at) {'//&
         'PM__head_node{'//&
         ' shapexx=#shapex._mshape;'//&
         ' this_tile=shapex._tile;'//&
         ' foreach p in nodes_for_grid(shapex.dist,_dmap(t,local_tile)) {'//&
         '  if contains(#(shapex.dist),p) {'//&
         '   i=index(dims(local_region.dist),p);'//&
         '   if i/=_this_node(){'//&
         '    other_tile=_get_elem(shapex.dist,p);'//&
         '    dest_range2=_dunmap(t,other_tile,local_region._mshape);'//&
         '    portion_to_recv=overlap(local_tile,dest_range2);'//&
         '    if size(portion_to_recv)>0:_recv_slice(i,&a,portion_to_recv)'//&
         ' }}};'//&
         ' dest_range=_dunmap(t,shapex._tile,local_region._mshape);'//&
         ' foreach p in nodes_for_grid(local_region.dist,dest_range){'//&
         '  if contains(#(local_region.dist),p) {'//&
         '   i=index(dims(local_region.dist),p);'//&
         '   if i/=_this_node() {'//&
         '    other_tile=_get_elem(local_region.dist,p);'//&
         '    portion_to_recv=intersect(other_tile,dest_range);'//&
         '    if size(portion_to_recv)>0: '//&
         '       _send_slice_mapped(i,x,portion_to_recv,t,shapex._tile)'//&
         ' }}};'//&
         ' _copy_dmapped(&a,local_tile,local_region._mshape,x,shapex._tile,t);'//&
         ' _sync_messages(a,x)}}',line)

    ! Resolve x[indexed] for cases where size(x[indexed])<=size(region)
    ! -- in this case send those values in x which are needed to calculate x[indexed]
    call dcl_uproc(parser,&
         '_get_dindex_r(&a:any^any,x,shapex,local_tile,local_region,t:tuple(indexed_dim),at) {'//&
         'shapexx=#shapex._mshape;'//&
         'src_range=_dmap(t,local_tile);var b=array(_arb(a),#src_range);'//&
         'foreach p in nodes_for_grid(shapex.dist,src_range) {'//&
         ' if contains(#(local_region.dist),p) {'//&
         '  i=index(dims(local_region.dist),p);'//&
         '  if i/=_this_node() {'//&
         '   other_tile=_get_elem(shapex.dist,p);'//&
         '   portion_to_send=overlap(src_range,other_tile);'//&
         '   if size(portion_to_send)>0:_recv_slice(i,&b,portion_to_send);'//&
         ' }}};'//&
         ' dest_range=_dunmap(t,shapex._tile,local_region._mshape);'//&
         ' foreach p in nodes_for_grid(local_region.dist,dest_range){'//&
         '  if contains(#(local_region.dist),p) {'//&
         '   i=index(dims(local_region.dist),p);'//&
         '   if i/=_this_node() {'//&
         '    other_tile=_get_elem(local_region.dist,p);'//&
         '    src_range2=_dmap(t,other_tile);'//&
         '    portion_to_send=overlap(shapex._tile,src_range2);'//&
         '    if size(portion_to_send)>0:_send_slice(i,x,portion_to_send);'//&
         ' }}};'//&
         ' u,v=overlap(src_range,shapex._tile);'//&
         ' for i in u,j in v <<conc>>{_set_elem(&b,PM__getelem(x,j),i <<PM__ignore>>)};'//&
         ' _sync_messages(x,b);'//&
         '_copy_dmapped(&a,local_tile,local_region._mshape,b,src_range,t)}',line)
    
    ! Resolve x[indexed] for cases where size(x[indexed])>=size(region)
    ! -- in this case send one value for every point in current (sub)region
    ! -- Version for more complex types that need sync receive
    call dcl_uproc(parser,&
         '_get_dindex_ss(&a:any^any,x,shapex,local_tile,local_region,t:tuple(indexed_dim),at) {'//&
         'shapexx=#shapex._mshape;'//&
         'this_tile=shapex._tile;'//&
         'dest_range=_dunmap(t,shapex._tile,local_region._mshape);'//&
         'foreach p in nodes_for_grid(local_region.dist,dest_range){'//&
         ' if contains(#(local_region.dist),p) {'//&
         '  i=index(dims(local_region.dist),p);'//&
         '  if i/=_this_node() {'//&
         '   other_tile=_get_elem(local_region.dist,p);'//&
         '   portion_to_recv=intersect(other_tile,dest_range);'//&
         '   if size(portion_to_recv)>0: '//&
         '      _send_slice_mapped(i,x,portion_to_recv,t,shapex._tile)'//&
         '}}};'//&
         '_copy_dmapped(&a,local_tile,local_region._mshape,x,shapex._tile,t);'//&
         'foreach p in nodes_for_grid(shapex.dist,_dmap(t,local_tile)) {'//&
         ' if contains(#(shapex.dist),p) {'//&
         '  i=index(dims(local_region.dist),p);'//&
         '  if i/=_this_node() {'//&
         '   other_tile=_get_elem(shapex.dist,p);'//&
         '   dest_range2=_dunmap(t,other_tile,local_region._mshape);'//&
         '   portion_to_recv=overlap(local_tile,dest_range2);'//&
         '   if size(portion_to_recv)>0 { '//&
         '     _recv_slice_sync(i,&a,portion_to_recv);'//&
         '}}}};_sync_messages(a,x)}',line)

    ! Resolve x[indexed] for cases where size(x[indexed])<=size(region)
    ! -- in this case send those values in x which are needed to calculate x[indexed]
    ! -- Version for more complex types that need sync receive
    call dcl_uproc(parser,&
         '_get_dindex_rs(&a:_comp^any,x,shapex,local_tile,local_region,t:tuple(indexed_dim),at) {'//&
         'PM__head_node {'//&
         'shapexx=#shapex._mshape;'//&
         'dest_range=_dunmap(t,shapex._tile,local_region._mshape);'//&
         'foreach p in nodes_for_grid(local_region.dist,dest_range){'//&
         '  if contains(#(local_region.dist),p) {'//&
         '   i=index(dims(local_region.dist),p);'//&
         '   if i/=_this_node() {'//&
         '    other_tile=_get_elem(local_region.dist,p);'//&
         '    src_range=_dmap(t,other_tile);'//&
         '    portion_to_send=overlap(shapex._tile,src_range);'//&
         '    if size(portion_to_send)>0:_send_slice(i,x,portion_to_send);'//&
         '}}}};'//&
         'src_range=_dmap(t,local_tile);var b=array(_arb(a),#src_range);'//&
         'if _head_node() or at { u,v=overlap(src_range,shapex._tile);'//&
         ' for i in u,j in v  <<conc>>{_set_elem(&b,PM__getelem(x,j),i <<PM__ignore>>)};'//&
         ' foreach p in nodes_for_grid(shapex.dist,src_range) {'//&
         '  if contains(#(local_region.dist),p) {'//&
         '   i=index(dims(local_region.dist),p);'//&
         '   if i/=_this_node() {'//&
         '    other_tile=_get_elem(shapex.dist,p);'//&
         '    portion_to_send=overlap(src_range,other_tile);'//&
         '    if size(portion_to_send)>0{ PM__head_node{_recv_slice_sync(i,&b,portion_to_send)};'//&
         '       if at:_bcast_slice_shared(&b,#b,portion_to_send,''true)}'//&
         ' }}}};'//&
         ' _copy_dmapped(&a,local_tile,local_region._mshape,b,src_range,t);'//&
         ' _sync_messages(x,b)}',line)

    call dcl_uproc(parser,'_copy_dmapped(&a,a_tile,a_extent,b,b_tile,t) {'//&
         'u=overlap(a_tile,_dunmap(t,b_tile,a_extent));'//&
         'for i in u  <<conc>>{_set_elem(&a,PM__getelem(b,j),i <<PM__ignore>>) '//&
         ' where j=b_tile#_dmap(t,a_tile[i])}}',line)
    
   call dcl_uproc(parser,'_copy_dmapped_ref(&a,a_tile,a_extent,b,b_tile,t) {'//&
         'u=intersect(a_tile,_dunmap(t,b_tile,a_extent));'//&
         'for i in u  <<conc>>{ bb=_import_dref%(b);_set_elem(&a,_getref(bb,i),a_tile#i <<PM__ignore>>);'//&
         '}}',line)

   ! Resolve x[ indexed ][ indexed or shared ] ... 
    call dcl_uproc(parser,&
         '_get_dindex_from_dref(&a:any^any,x,shapex,local_tile,local_region,tt:tuple(indexed_dim),at) {'//&
         't=_correct(tt,shapex._mshape);shapexx=#shapex._mshape;'//&
         'this_tile=shapex._tile;'//&
         'dest_range=_dunmap(t,shapex._tile,local_region._mshape);'//&
         'foreach p in nodes_for_grid(shapex.dist,_dmap(t,local_tile)) {'//&
         ' if contains(#(shapex.dist),p) {'//&
         '  i=index(dims(local_region.dist),p);'//&
         '  if i/=_this_node(){'//&
         '   other_tile=_get_elem(shapex.dist,p);'//&
         '   dest_range2=_dunmap(t,other_tile,local_region._mshape);'//&
         '   portion_to_recv=overlap(local_tile,dest_range2);'//&
         '   if size(portion_to_recv)>0:_recv_slice(i,&a,portion_to_recv);'//&
         '}}};'//&
         'nodes=nodes_for_grid(local_region.dist,dest_range);'//&
         'var b=array(varray(_arb(a),spread(1..0,local_region._mshape)),#nodes);'//&
         'foreach pp in #nodes {'//&
         ' p=nodes[pp];'//&
         ' if contains(#(local_region.dist),p) {'//&
         '  i=index(dims(local_region.dist),p);'//&
         '  if i/=_this_node() {'//&
         '   other_tile=_get_elem(local_region.dist,p);'//&
         '   portion_to_recv=intersect(other_tile,dest_range);'//&
         '   if size(portion_to_recv)>0 {'//&
         '      b[pp]:={_getref(_import_dref%(x),h):h in portion_to_recv};'//&
         '      _isend(i,b[pp]) }'//&
         '}}};'//&
         '_copy_dmapped_ref(&a,local_tile,local_region._mshape,x,shapex._tile,t);'//&
         '_sync_messages(a,x)'//&
         '}',line)

   
    ! Resolve x[ indexed ][ indexed or shared ] ... 
    call dcl_uproc(parser,&
         '_get_dindex_from_dref_s(&a:_comp^any,x,shapex,local_tile,local_region,tt:tuple(indexed_dim),at) {'//&
         't=_correct(tt,shapex._mshape);shapexx=#shapex._mshape;'//&
         'this_tile=shapex._tile;'//&
         'dest_range=_dunmap(t,shapex._tile,local_region._mshape);'//&
         'nodes=nodes_for_grid(local_region.dist,dest_range);'//&
         'var b=array(varray(_arb(a),spread(1..0,local_region._mshape)),#nodes);'//&
         ' foreach pp in #nodes {'//&
         '  p=nodes[pp];'//&
         '  if contains(#(local_region.dist),p) {'//&
         '   i=index(dims(local_region.dist),p);'//&
         '   if i/=_this_node() {'//&
         '    other_tile=_get_elem(local_region.dist,p);'//&
         '    portion_to_recv=intersect(other_tile,dest_range);'//&
         '    if size(portion_to_recv)>0 {'//&
         '      b[pp]:={_getref(_import_dref%(x),h):h in portion_to_recv};'//&
         '      _isend(i,b[pp]) }'//&
         '}}};'//&
         '_copy_dmapped_ref(&a,local_tile,local_region._mshape,x,shapex._tile,t);'//&
         'foreach p in nodes_for_grid(shapex.dist,_dmap(t,local_tile)) {'//&
         '  if contains(#(shapex.dist),p) {'//&
         '   i=index(dims(local_region.dist),p);'//&
         '   if i/=_this_node(){'//&
         '    other_tile=_get_elem(shapex.dist,p);'//&
         '    dest_range2=_dunmap(t,other_tile,local_region._mshape);'//&
         '    portion_to_recv=overlap(local_tile,dest_range2);'//&
         '    if size(portion_to_recv)>0{ PM__head_node{ _recv_slice_sync(i,&a,portion_to_recv)};'//&
         '        if at:_bcast_shared_slice(&a,#a,portion_to_recv,''true);}'//&
         '  }};'//&
         '};_sync_messages(a,x)}',line)

    ! Resolve x[ indexed ][ priv ]
    call dcl_uproc(parser,&
         '_get_dindex_from_ref(&a,x,shapex,this_tile,local_region,t:tuple(indexed_dim),complt,at) {'//&
         'dest_range=_dmap(t,this_tile,#shapex._mshape);'//&
         ' foreach p in nodes_for_grid(shapex.dist,dest_range){'//&
         '  i=index(dims(local_region.dist),p);'//&
         '  if contains(#(local_region.dist),p) and i/=_this_node() {'//&
         '   other_tile=_get_elem(shapex.dist,p);'//&
         '   portion_to_send=overlap(this_tile,_dunmap(t,other_tile,local_region._mshape));'//&
         '   _send_recv_slice_req(i,x,&a,local_region._mshape,portion_to_send,complt)'//&
         '}};'//&
         'src_range=_dunmap(t,shapex._tile,local_region._mshape);'//&
         'for j in overlap(this_tile,src_range)  <<conc>>{'//&
         '  jj=index(#this_tile,j);PM__do_at size(this_tile),jj,aa,a,xx,x { '//&
         '  PM__assign(&aa,_getref(xx,null))}}; '//&
         'foreach p in nodes_for_grid(local_region.dist,src_range) {'//&
         ' if contains(#(local_region.dist),p) and index(dims(local_region.dist),p)/=_this_node() {'//&
         '    PM__recv_req pp,xx,x,_getref(xx,null)'//&
         ' }};'//&
         ' if a is <_comp>: foreach p in nodes_for_grid(shapex.dist,dest_range){'//&
         '  i=index(dims(local_region.dist),p);'//&
         '  if contains(#(local_region.dist),p) and i/=_this_node() {'//&
         '   other_tile=_get_elem(shapex.dist,p);'//&
         '   portion_to_send=overlap(this_tile,_dunmap(t,other_tile,local_region._mshape));'//&
         '   _recv_slice_reply(i,&a,local_region._mshape,portion_to_send,complt)'//&
         ' }};'//&
         ' _sync_messages(a,x);'//&
         '}',line)

    ! Resolve x[ indexed ][ whatever ] <pr> = priv
    call dcl_uproc(parser,&
         '_set_dindex_of_ref(&x,y,shapex,this_tile,local_region,tt:tuple(indexed_dim),'//&
         '     pr:proc,complt,at) {'//&
         't=_correct(tt,shapex._mshape);dest_range=_dmap(t,this_tile,#shapex._mshape);'//&
         'PM__head_node{foreach p in nodes_for_grid(shapex.dist,dest_range){'//&
         '  i=index(dims(shapex.dist),p);'//&
         '  if contains(#(shapex.dist),p) and i/=_this_node() {'//&
         '   other_tile=_get_elem(shapex.dist,p);'//&
         '   portion_to_send=overlap(this_tile,_dunmap(t,other_tile,local_region._mshape));'//&
         '   _send_slice_assn(i,x,y,#this_tile,portion_to_send,complt)'//&
         '}}};'//&
         'src_range=_dunmap(t,shapex._tile,local_region._mshape);'//&
         'for j in overlap(this_tile,src_range) {'//&
         '  jj=index(#this_tile,j);PM__do_at size(this_tile),jj,yy,y,xx,x { '//&
         '  PM__assign(&^(_getlhs(xx,null)),yy,pr)}}; '//&
         'foreach p in nodes_for_grid(local_region.dist,src_range) {'//&
         ' i=index(dims(shapex.dist),p);'//&
         ' if contains(#(local_region.dist),p) and i/=_this_node()  {'//&
         '    PM__recv_assn pp,xx,yy,x,y,at{PM__assign(&^(_getlhs(^(&xx),null)),yy,pr)}'//&
         '}};'//&
         '_sync_messages(x,y)}',line)

    !**************************************************************
    ! Envelope and stencil definitions
    !**************************************************************

    call dcl_type(parser,'envelope is rec{cross:extent or null,corner:extent or null,envelope:extent}',line)
    call dcl_uproc(parser,'ortho(x:extent)=new envelope {cross=x,corner=spread(0..0,x),envelope=x}',line)
    call dcl_uproc(parser,'ortho(x:extent,y:extent)='//&
         'new envelope {cross=x,corner=y,envelope=envelope(x,y)}',line)
    call dcl_uproc(parser,'envelope(x:envelope)=x.envelope',line)
    call dcl_uproc(parser,'envelope(x:extent)=x',line)
    call dcl_uproc(parser,'envelope(x:any_int,y:any_int)='//&
         'min(xx,yy)..max(xx,yy) where xx=int(x),yy=int(y)',line)
    call dcl_uproc(parser,'envelope(x:any_int,y:seq(any_int))=envelope(x..x,y)',line)
    call dcl_uproc(parser,'envelope(x:seq(any_int),y:any_int)=envelope(x,y..y)',line)
    call dcl_uproc(parser,'envelope(x:seq(any_int),y:seq(any_int))='//&
         'if(size(x)>0=>if(size(y)>0=>min(lx,ly)..max(hx,hy),lx..hx),if(size(y)>0=>0..0,ly..hy)) '//&
         'where lx=min(llx,hhx),ly=min(lly,hhy),hx=max(llx,hhx),hy=max(lly,hhy)'//&
         'where llx=int(low(x)),lly=int(low(y)),hhx=int(high(x)),hhy=int(high(y))',line)
    call dcl_uproc(parser,'envelope(x:tuple,y:tuple)=map($envelope,x,y)',line)
    call dcl_uproc(parser,'envelope(x:null,y:extent)=y',line)
    call dcl_uproc(parser,'envelope(x:extent,y:null)=x',line)
    call dcl_uproc(parser,'envelope(x:extent or null,y:envelope)=envelope(x,y.envelope)',line)
    call dcl_uproc(parser,'envelope(x:envelope,y:extent or null)=envelope(x.envelope,y)',line)
    call dcl_uproc(parser,'envelope(x:envelope,y:envelope)=envelope(x.envelope,y.envelope)',line)
    call dcl_uproc(parser,'string(x:envelope)=string(x.cross++" ortho "++x.corner)',line)
    
    !**************************************************************
    ! Support for nhd statement
    !**************************************************************

    call dcl_type(parser,'_nhd is rec{_nbhd,_tile,_tilesz,_bounds,_interior,_limits}',line)
    call dcl_type(parser,'nbhd(t) is struct^{_array:farray(t),_nbhd,_index,_here}',line)
    call dcl_uproc(parser,'PM__nhd%(x:invar envelope or extent,y:invar boundary)<<shared>>='//&
         'new _nhd {_nbhd=x,_tile=t,_tilesz=#t,_bounds=y,_interior=overlap(t,region._tile),'//&
         '_limits=region._extent} '//&
         'where t=_get_halo(region,region._tile,envelope(x))',line)
    call dcl_uproc(parser,'PM__nhd%(x,y)='//&
         '^(new _nhd {_nbhd=n,_tile=t,_tilesz=#t,_bounds=null,_interior=t,'//&
         '_limits=region._extent},shared) '//&
         'where t=region._tile where n=xx '//&
         'where xx=spread(0..0,here){_check_nhd%(x)}',line)
    
    call dcl_uproc(parser,'_check_nhd%(n:invar envelope or extent) {}',line)
    call dcl_uproc(parser,'_check_nhd%(n:extent):test "Neighbourhood must be invar"=>''false',line)
    call dcl_uproc(parser,'_check_nhd%(n):test "Neighbourhood must be an extent or envelope"=>''false',line)
    call dcl_uproc(parser,'PM__check_bounds%(n,b:invar boundary){_check_ranks(n,b)}',line)
    call dcl_uproc(parser,'PM__check_bounds%(n,b:boundary):test "Bounds must be ""invar"""=>''false',line)
    call dcl_uproc(parser,'PM__check_bounds%(n,b):test "Bounds must have a boundary type"=>''false',line)
    call dcl_uproc(parser,'_check_ranks(n:tuple,b:tuple):'//&
         'test "Rank of boundary does not match that of neighbourhood"=>same_type(rank(n),rank(b))',line)
    call dcl_uproc(parser,'_check_ranks(n:envelope,b:tuple):'//&
         'test "Rank of boundary does not match that of neighbourhood"=>same_type(rank(n.cross),rank(b))',line)
    call dcl_uproc(parser,'_check_ranks(n,b:tuple):'//&
         'test "Rank of boundary does not match that of neighbourhood"=>same_type(''1,rank(b))',line)
    call dcl_uproc(parser,'_check_ranks(n,b){}',line)

    call dcl_uproc(parser,'PM__set_nhd%(&n,x:complete){'//&
         'n._array[n._nbhd._interior[n._index]]:=x}',line)
    call dcl_uproc(parser,'PM__set_nhd%(&n,x):n._array[n._nbhd._interior[n._index]]:=x'//&
         ' check "Expression in ""nhd"" must be ""complete"""=>''false',line)

    call dcl_uproc(parser,'PM__nhd_join(x)=x._array',line)
    call dcl_uproc(parser,'PM__nhd_join(x,y)=new _join{head=x,tail=y._array}',line)
    call dcl_uproc(parser,'PM__nhd_var%(x,n:_nhd,i,h)='//&
         'new nbhd{_array=_make_nhd(^(x,shared),n._tilesz <<shared>>),_nbhd=n,_index=i,_here=h}',line)
    call dcl_uproc(parser,'_make_nhd(x,d){var v=array(x,d);return v}',line)

    call dcl_uproc(parser,'PM__set_edge%(&x,y,z){}',line)

    call dcl_uproc(parser,'PM__subref(x:nbhd,t:subs)=_nhd_sub(x,t)',line)
    call dcl_uproc(parser,'PM__subref(x:nbhd,t:null)=_nhd_sub(x,t)',line)
    
    call dcl_uproc(parser,'_nhd_sub(x:nbhd,t:any except contains(null or stretch_dim))='//&
         'PM__subref(x._array,tt+x._nbhd._interior[x._index])'//&
         '{tt=_tup(t);'//&
         'test t++" not in neighbourhood"++x._nbhd._nbhd=>contains(_foot(tt,x._nbhd._nbhd),tt),'//&
         '"At "++x._here++" nhd "++t++" goes outside of boundary "++limits=>'//&
         '_check_limits(dtt,limits,x._nbhd._bounds)'//&
         'where limits=x._nbhd._limits '//&
         'where dtt=tt+x._here}',line)
    call dcl_uproc(parser,'_nhd_sub(x:nbhd,t)=_arb(x._array)'//&
         'check "Subscripts with null or ""_"" dimensions not accepted for ""nbhd"""=>''false',line)

    call dcl_uproc(parser,'_check_limits(t:int,extent:range,bound:null)=t in extent',line)
    call dcl_uproc(parser,'_check_limits(t:seq,extent:range,bound:null)=extent inc t',line)
    call dcl_uproc(parser,'_check_limits(t,extent,bound)=''true',line)
    call dcl_uproc(parser,'_check_limits(t:tuple,extent,bound:tuple)='//&
         'map_reduce($_check_limits,$and,t,extent,bound)',line)
    call dcl_uproc(parser,'_check_limits(t:tuple(int),extent,bound:null)=t in extent',line)
    call dcl_uproc(parser,'_check_limits(t:tuple,extent,bound:null)=extent inc t',line)
    call dcl_uproc(parser,'_check_limits(t:tuple,extent,bound)=''true',line)
    
    call dcl_uproc(parser,'PM__dup(x:nbhd)=x:test "Cannot make a variable or constant of type ""nbhd"""=>''false',line)
    
    call dcl_uproc(parser,'envelope(x:_nhd)=envelope(x._nbhd)',line)
    call dcl_uproc(parser,'envelope(x,y:_nhd)=envelope(x,y._nbhd)',line)
    call dcl_uproc(parser,'envelope(x:_nhd,y:_nhd)=envelope(x._nbhd,y._nbhd)',line)
    
    call dcl_type(parser,'_join is struct^{head,tail}',line)

    call dcl_uproc(parser,'PM__blocking%(x:null){}',line)
    call dcl_uproc(parser,'PM__blocking%(x):'//&
         'test "Block expression must be tuple of integers"=>''false',line)
    call dcl_uproc(parser,'PM__blocking%(x:tuple(any_int)){'//&
         'test "Rank of ""block="" does not match region"=>same_type(rank(x),rank(region));'//&
         'test "Block sizes must be positive"=>map_reduce($_positive,$and,x)}',line)
    call dcl_uproc(parser,'_positive(x)=x>0',line)
    
    call dcl_uproc(parser,&
         'PM__send_nhd%(&a:shared,nbhd:shared) <<shared,always>> { '//&
         ' this_tile=region._tile;this_tile_x=nbhd._tile;'//&
         ' pp=index2point(_this_node(),dims(region.dist));'//&
         ' foreach i in node_nhd(region.dist,pp,envelope(nbhd._nbhd)) {'//&
         '   ok,p=_to_exchange(region.dist,i,nbhd._bounds);if ok {'//&
         '     other_tile=_wrapped_tile(region.dist,region._extent,i);'//&
         '     other_tile_x=_get_halo(region._mshape,other_tile,'//&
         '       _foot(i-pp,nbhd._nbhd));'//&
         '     ov=overlap(this_tile_x,other_tile);if size(ov)>0 {'//&
         '     _recv_slice(p,&a,ov);'//&
         '   }}'//&
         ' };'//&
         ' foreach ii in node_nhd(region.dist,pp,_rev(envelope(nbhd._nbhd))) { '//&
         '   i=pp*2-ii;ok,p=_to_exchange(region.dist,i,nbhd._bounds);if ok {'//&
         '     other_tile=_wrapped_tile(region.dist,region._extent,i);'//&
         '     other_tile_x=_get_halo(region._mshape,other_tile,_foot(i-pp,nbhd._nbhd));'//&
         '     ov=overlap(this_tile_x,intersect(this_tile,other_tile_x));if size(ov)>0 {'//& 
         '            _send_slice(p,a,ov);'//&
         '   }} '//&
         ' }'//&
         '}',line)

    call dcl_uproc(parser,'_exchange_cyclic_bounds(&a,nbhd,region,bound) {'//&
         ' this_tile=region._tile;this_tile_x=nbhd._tile;'//&
         ' pp=spread(0,this_tile);'//&
         ' print("exchange");foreach i in node_nhd(region.dist,pp,envelope(nbhd._nbhd)) {'//&
         '   print("ww"++i);ok,p=_to_exchange(region.dist,i,nbhd._bounds);if ok {'//&
         '     other_tile=_wrapped_tile(region.dist,region._extent,i);'//&
         '     other_tile_x=_get_halo(region._mshape,other_tile,'//&
         '       _foot(i-pp,nbhd._nbhd));print("CP"++i);'//&
         '     ov=overlap(this_tile_x,other_tile);if size(ov)>0 {'//&
         '     ov2=overlap(other_tile_x,intersect(other_tile,this_tile_x));'//&
         '     print("copy"++ov2++"to"++ov);for k1 in ov,k2 in ov2:sync a[k1]:=v where v=a[k2]'//&
         '   }}'//&
         '}}',line)
    call dcl_uproc(parser,&
         '_exchange_cyclic_bounds(&a,nbhd,this_tile,bound:boundary except contains(CYCLE)) {}',line)
    
    call dcl_uproc(parser,'_foot(d,n:envelope)=if(_crss(d)=>n.cross,n.corner)',line)
    call dcl_uproc(parser,'_foot(d,n:extent)=n',line)

    call dcl_uproc(parser,'PM__recv_nhd%(&a:shared,nbhd:shared) {}',line)
    
    call dcl_uproc(parser,&
         'PM__recv_nhd%(&a:shared _comp^any,nbhd:shared) <<shared,always>> { '//&
         ' this_tile=region._tile;this_tile_x=nbhd._tile;'//&
         ' pp=index2point(_this_node(),dims(region.dist));'//&
         ' foreach i in node_nhd(region.dist,pp,envelope(nbhd._nbhd)) {'//&
         '   ok,p=_to_exchange(region.dist,i,nbhd._bounds);if ok {'//&
         '     other_tile=_wrapped_tile(region.dist,i);'//&
         '     other_tile_x=_get_halo(region._mshape,other_tile,_foot(_crss(i-pp),nbhd._nbhd));'//&
         '     ov=overlap(this_tile_x,other_tile);if size(ov)>0 {'//&
         '      _recv_slice_sync(p,&a,ov);'//&
         '   }}'//&
         ' }}',line)

    call dcl_uproc(parser,'_rev(a:tuple)=map($_rev,a)',line)
    call dcl_uproc(parser,'_rev(a:range)=-high(a)..-low(a)',line)

    call dcl_uproc(parser,'_to_exchange(d,p,b:boundary_dim)=ok,pp'//&
         ' where ok,pp=_to_exchange(d,p,spread(b,p))',line)
    call dcl_uproc(parser,'_to_exchange(d,p,b)=ok,pp {'//&
         'var pp=0;dd=dims(d);var ok=_exchange_needed(dd,p,b);'//&
         'if ok {pp2=index(dd,p);pp:=index(dd,map($_mod_if_needed,p,dd,b));ok:=pp2/=_this_node()}}',line)
    call dcl_uproc(parser,'_exchange_needed(d:tuple,p:tuple,b:tuple)='//&
         'map_reduce($_exchange_needed,$and,d,p,b)',line)
    call dcl_uproc(parser,'_exchange_needed(d,p,b)='//&
         'p>=0 and p<d',line)
    call dcl_uproc(parser,'_exchange_needed(d,p,b:CYCLE)='//&
         '''true',line)
    call dcl_uproc(parser,'_mod_if_needed(p,d,b)=p',line)
    call dcl_uproc(parser,'_mod_if_needed(p,d,b:CYCLE)=p mod d',line)

    call dcl_uproc(parser,'_wrapped_tile(d:tuple,s:tuple,p:tuple)=map($_wrapped_tile,d,s,p)',line)
    call dcl_uproc(parser,'_wrapped_tile(d,s,p)=_get_elem(d,p mod ss)+_rdiv(p,ss)*size(s) where ss=size(d)',line)
!!$         '{qq=_get_elem(d,p mod ss)+_rdiv(p,ss)*size(s) where ss=size(d);for q in qq:print(_sys_node()++">>"++q)}',line)
         
!!$         '{print(_sys_node()++">>>"++p++";"++p mod size(d)++";"++size(s)++";"++'//&
!!$         '_get_elem(d,p mod ss)+_rdiv(p,ss)*size(s)) where ss=size(d)}',line)

    call dcl_uproc(parser,'_send_slice(p,a:_join,o) {_send_slice(p,a.head,o);_send_slice(p,a.tail,o)}',line)
    call dcl_uproc(parser,'_recv_slice(p,&a:_join,o) {_recv_slice(p,&a.head,o);_recv_slice(p,&a.tail,o)}',line)
    call dcl_uproc(parser,'_recv_slice_sync(p,&a:_join,o)'//&
         '{_recv_slice_sync(p,&a.head,o);_recv_slice_sync(p,&a.tail,o)}',line)
    call dcl_uproc(parser,'_sync_messages(x:_join):_sync_messages(x.head,x.tail)',line)
       
    call dcl_uproc(parser,'_not_zero(x)=if(x/=0=>1,0)',line)
    call dcl_uproc(parser,'_crss(x)=1==map_reduce($_not_zero,$+,x)',line)
    
    call dcl_type(parser,'boundary is boundary_dim,tuple(boundary_dim)',line)
    call dcl_type(parser,'boundary_dim is CYCLE,NO_EDGE,null',line)
    call dcl_type(parser,'CYCLE is unique',line)
    call dcl_type(parser,'NO_EDGE is unique',line)
 
    ! Add (anti) halo around tile
    ! Args: mshape / tile / displacement
    ! (mshape not used at the moment - there to cope with other topologies)
    ! Result: expanded tile
    call dcl_uproc(parser,&
         '_get_halo(d:range(int),t:range(int),i:any_int)='//&
         'low(t)+ii..high(t)+ii where ii=int(i)',line)
    call dcl_uproc(parser,&
         '_get_halo(d:range(int),t:strided_range(int),i:any_int)='//&
         ' low(t)+ii..high(t)+ii by step(t) where ii=int(i)',line)
    call dcl_uproc(parser,&
         '_get_halo(d:range(int),t:block_seq,i:any_int){'//&
         'return block_seq(low(t)+ii,high(t)+ii,step(t),width(t),0) where ii=int(i)}',line)
    call dcl_uproc(parser,&
         '_get_halo(d:range(int),t:map_seq,i:any_int) {'//&
         'var a=array(0,size(t));for j in a,k in t.array:j:=k+i;return new map_seq {array=a}}',line)
    
    call dcl_uproc(parser,&
         '_get_halo(d:range(int),t:range(int),i:range(any_int))='//&
         'low(t)+int(low(i))..high(t)+int(high(i))',line)
    call dcl_uproc(parser,&
         '_get_halo(d:range(int),t:strided_range(int),i:range(any_int)){'//&
         'var step=step(t);var width=size(i);'//&
         'if width>step {step:=1;width:=1};'//&
         'return block_seq(low(t)+int(low(i)),high(t)+int(high(i)),step,width,0)}',line)
    call dcl_uproc(parser,&
         '_get_halo(d:range(int),t:block_seq,i:range(any_int)){'//&
         'var step=step(t);var width=size(i)+width(t)-1;'//&
         'if width>step {step:=1;width:=1}'//&
         'return block_seq(low(t)+int(low(i)),high(t)+int(high(i)),step,width,0)}',line)
    call dcl_uproc(parser,&
         '_get_halo(d:range(int),t:map_seq,i:range(any_int)) {'//&
         'var a=array(0,[0..size(t)*size(i)-1]);var m=0;'//&
         '_expand_aseq(&m,t.array,size(t.array),&a,low(i),high(i));'//&
         'return _redim(a,shape(m))}',line)    
    call dcl_uproc(parser,&
         '_get_halo(d:extent,j:grid,i:tuple(any_int or range(any_int)))='//&
         ' map($_get_halo,d,j,i)',line)
    call dcl_uproc(parser,&
         '_get_halo(d:tuple1d(range(int)),t:grid1d,i:any_int or range(any_int))='//&
         '_get_halo(d.1,t.1,i)',line)

    call dcl_uproc(parser,'_get_halo(d,t,i:null)=t',line)

    call dcl_uproc(parser,&
         '_get_anti_halo(d:range(int),t:range(int),i:any_int)='//&
         'low(t)-ii..high(t)-ii where ii=int(i)',line)
    call dcl_uproc(parser,&
         '_get_anti_halo(d:range(int),t:strided_range(int),i:any_int)='//&
         ' low(t)-ii..high(t)-ii by step(t) where ii=int(i)',line)
    call dcl_uproc(parser,&
         '_get_anti_halo(d:range(int),t:block_seq,i:any_int){'//&
         'return block_seq(low(t)-ii,high(t)-ii,step(t),width(t)) where ii=int(i)}',line)
    call dcl_uproc(parser,&
         '_get_anti_halo(d:range(int),t:map_seq,i:any_int) {'//&
         'var a=array(0,size(t));for j in a,k in t.array:j:=k-i;return new map_seq {array=a}}',line)
    call dcl_uproc(parser,&
         '_get_anti_halo(d:range(int),t:range(int),i:range(any_int))='//&
         'low(t)-int(high(i))..high(t)-int(low(i))',line)
    
    call dcl_uproc(parser,&
         '_get_anti_halo(d:range(int),t:strided_range(int),i:range(any_int)){'//&
         'var step=step(t);var width=size(i);'//&
         'if width>step {step:=1;width:=1}'//&
         'return block_seq(low(t)-int(high(i)),high(t)-int(low(i)),step,width)}',line)
    call dcl_uproc(parser,&
         '_get_anti_halo(d:range(int),t:block_seq,i:range(any_int)){'//&
         'var step=step(t);var width=size(i)+width(t)-1;'//&
         'if width>step {step:=1;width:=1}'//&
         'return block_seq(low(t)+int(low(i)),high(t)+int(high(i)),step,width)}',line)
    call dcl_uproc(parser,&
         '_get_anti_halo(d:range(int),t:map_seq,i:range(any_int)) {'//&
         'var a=array(0,[0..size(t)*size(i)-1]);var m=0;'//&
         '_expand_aseq(&m,t.array,size(t.array),&a,-high(i),-low(i));'//&
         'return _redim(a,shape(m))}',line)   
    call dcl_uproc(parser,&
         '_get_anti_halo(d:extent,j:grid,'//&
         ' i:tuple(any_int or range(any_int)))='//&
         ' map($_get_anti_halo,d,j,i)',line)
    call dcl_uproc(parser,&
         '_get_anti_halo(d:tuple1d(range(int)),t:grid1d,i:any_int or range(any_int))='//&
         '_get_anti_halo(d.1,t.1,i)',line)
    call dcl_uproc(parser,'_get_anti_halo(d,t,i:null)=t',line)

 
    call dcl_uproc(parser,'_interior(d:range,t:range,n:range)=low(t)+max(0,-low(n))..high(t)-max(0,high(n))',line)
    call dcl_uproc(parser,'_interior(d:range,t:strided_range,n:range)=1..0 by 1',line)
    call dcl_uproc(parser,'_interior(d:range,tt:block_seq,n:range)='//&
         'block_seq(low(t)+max(-low(n),0),high(t),width(t),width(t)-max(-low(n),0)-max(high(n),0),0)'//&
         'where t=middle_blocks(tt)',line)
    call dcl_uproc(parser,'_get_chunk(d:range,t:range,n:range,l:''true)='//&
         'low(t)..low(t)+min(-min(0,low(n))-1,size(t)-1)',line)
    call dcl_uproc(parser,'_get_chunk(d:range,t:range,n:range,l:''false)='//&
         'low(t)+max(size(t)-max(high(n),0),-min(low(n),-1))..high(t)',line)
    call dcl_uproc(parser,'_get_chunk(d:range,tt:block_seq,n:range,l:''true)='//&
         'if(low(n)<0=>block_seq(low(t),high(t),step(t),min(-low(n),width(t)),0),empty(t))'//&
         ' where t=middle_blocks(tt)',line)
    call dcl_uproc(parser,'_get_chunk(d:range,tt:block_seq,n:range,l:''false)='//&
         'if(high(n)>0=>block_seq(low(t)+max(0,width(t)-high(n)),high(t),step(t),min(width(t),high(n)),0),empty(t))'//&
         ' where t=middle_blocks(tt)',line)
    call dcl_uproc(parser,'_get_chunk(d:range,t:grid_dim,n:range,l:_left or _right)=empty(t)',line)
    call dcl_uproc(parser,'_get_chunk(d:range,t:block_seq,n:range,l:_left)='//&
         'block_seq(low(b),high(b),1,1,0) where b=first_block(t)',line)
    call dcl_uproc(parser,'_get_chunk(d:range,t:block_seq,n:range,l:_right)='//&
         'block_seq(low(b),high(b),1,1,0) where b=last_block(t)',line)
    
    call dcl_uproc(parser,'_chunk(d,t,n,r,e,l)='//&
         'if(r<e=>_interior(d,t,n),r>e=>t,_get_chunk(d,t,n,l))',line)
    call dcl_uproc(parser,'_get_chunk(d:tuple1d,t,n,e,l)=[_chunk(d.1,t.1,n.1,''1,e,l)]',line)
    call dcl_uproc(parser,'_get_chunk(d:tuple2d,t,n,e,l)=[_chunk(d.1,t.1,n.1,''1,e,l),'//&
         '_chunk(d.2,t.2,n.2,''2,e,l)]',line)
    call dcl_uproc(parser,'_get_chunk(d:tuple3d,t,n,e,l)=[_chunk(d.1,t.1,n.1,''1,e,l),'//&
         '_chunk(d.2,t.2,n.2,''2,e,l),_chunk(d.3,t.3,n.3,''3,e,l)]',line)
    call dcl_uproc(parser,'_get_chunk(d:tuple4d,t,n,e,l)=[_chunk(d.1,t.1,n.1,''1,e,l),'//&
         '_chunk(d.2,t.2,n.2,''2,e,l),_chunk(d.3,t.3,n.3,''3,e,l),'//&
         '_chunk(d.4,t.4,n.4,''4,e,l)]',line)
    call dcl_uproc(parser,'_get_chunk(d:tuple5d,t,n,e,l)=[_chunk(d.1,t.1,n.1,''1,e,l),'//&
         '_chunk(d.2,t.2,n.2,''2,e,l),_chunk(d.3,t.3,n.3,''3,e,l),'//&
         '_chunk(d.4,t.4,n.4,''4,e,l),_chunk(d.5,t.5,n.5,''5,e,l)]',line)
    call dcl_uproc(parser,'_get_chunk(d:tuple6d,t,n,e,l)=[_chunk(d.1,t.1,n.1,''1,e,l),'//&
         '_chunk(d.2,t.2,n.2,''2,e,l),_chunk(d.3,t.3,n.3,''3,e,l),'//&
         '_chunk(d.4,t.4,n.4,''4,e,l),_chunk(d.5,t.5,n.5,''5,e,l),'//&
         '_chunk(d.6,t.6,n.6,''6,e,l)]',line)
    call dcl_uproc(parser,'_get_chunk(d:tuple7d,t,n,e,l)=[_chunk(d.1,t.1,n.1,''1,e,l),'//&
         '_chunk(d.2,t.2,n.2,''2,e,l),_chunk(d.3,t.3,n.3,''3,e,l),'//&
         '_chunk(d.4,t.4,n.4,''4,e,l),_chunk(d.5,t.5,n.5,''5,e,l),'//&
         '_chunk(d.6,t.6,n.6,''6,e,l),_chunk(d.7,t.7,n.7,''7,e,l)]',line)

    call dcl_uproc(parser,'chunks(t:tuple(range(any_int)),n:extent)=rank(t)*2+1',line)
    call dcl_uproc(parser,'chunks(t:tuple(range(any_int) or block_seq),n:extent)=rank(t)*4+1',line)
    call dcl_uproc(parser,'chunks(t:grid,n:extent)=2',line)

    call dcl_uproc(parser,'_cr(i,n):test "Index out of range in ""get_chunk"""=>i>=0 and i<=n',line)
    
    call dcl_uproc(parser,'get_chunk(d:shape,t:tuple(range(any_int)),n:extent,i:int) {'//&
         '_cr(i,rank(t)*2);const r;switch i {'//&
         'case 0: r=map($_interior,d,t,n);'//&
         'case 1: r=_get_chunk(d,t,n,''1,''true);'//&
         'case 2: r=_get_chunk(d,t,n,''1,''false);'//&
         'case 3: r=_get_chunk(d,t,n,''2,''true);'//&
         'case 4: r=_get_chunk(d,t,n,''2,''false);'//&
         'case 5: r=_get_chunk(d,t,n,''3,''true);'//&
         'case 6: r=_get_chunk(d,t,n,''3,''false);'//&
         'case 7: r=_get_chunk(d,t,n,''4,''true);'//&
         'case 8: r=_get_chunk(d,t,n,''4,''false);'//&
         'case 9: r=_get_chunk(d,t,n,''5,''true);'//&
         'case 10:r=_get_chunk(d,t,n,''5,''false);'//&
         'case 11:r=_get_chunk(d,t,n,''6,''true);'//&
         'case 12:r=_get_chunk(d,t,n,''6,''false);'//&
         'case 13:r=_get_chunk(d,t,n,''7,''true);'//&
         'case 14:r=_get_chunk(d,t,n,''7,''false);'//&
         'default:r=_get_chunk(d,t,n,''1,''true);'//&
         '};return r}',line)

    call dcl_type(parser,'_left is unique',line)
    call dcl_type(parser,'_right is unique',line)
    
    call dcl_uproc(parser,'get_chunk(d:shape,t:tuple(range(any_int) or block_seq),n:extent,i:int) {'//&
         '_cr(i,rank(t)*4);const r;switch i {'//&
         'case 0:r=map($_interior,d,t,n);'//&
         'case 1:r=_get_chunk(d,t,n,''1,''true);'//&
         'case 2:r=_get_chunk(d,t,n,''1,''false);'//&
         'case 3:r=_get_chunk(d,t,n,''1,_left);'//&
         'case 4:r=_get_chunk(d,t,n,''1,_right);'//&
         'case 5:r=_get_chunk(d,t,n,''2,''true);'//&
         'case 6:r=_get_chunk(d,t,n,''2,''false);'//&
         'case 7:r=_get_chunk(d,t,n,''2,_left);'//&
         'case 8:r=_get_chunk(d,t,n,''2,_right);'//&
         'case 9:r=_get_chunk(d,t,n,''3,''true);'//&
         'case 10:r=_get_chunk(d,t,n,''3,''false);'//&
         'case 11:r=_get_chunk(d,t,n,''3,_left);'//&
         'case 12:r=_get_chunk(d,t,n,''3,_right);'//&
         'case 13:r=_get_chunk(d,t,n,''4,''true);'//&
         'case 14:r=_get_chunk(d,t,n,''4,''false);'//&
         'case 15:r=_get_chunk(d,t,n,''4,_left);'//&
         'case 16:r=_get_chunk(d,t,n,''4,_right);'//&
         'case 17:r=_get_chunk(d,t,n,''5,''true);'//&
         'case 18:r=_get_chunk(d,t,n,''5,''false);'//&
         'case 19:r=_get_chunk(d,t,n,''5,_left);'//&
         'case 20:r=_get_chunk(d,t,n,''5,_right);'//&
         'case 21:r=_get_chunk(d,t,n,''6,''true);'//&
         'case 22:r=_get_chunk(d,t,n,''6,''false);'//&
         'case 23:r=_get_chunk(d,t,n,''6,_left);'//&
         'case 24:r=_get_chunk(d,t,n,''6,_right);'//&
         'case 25:r=_get_chunk(d,t,n,''7,''true);'//&
         'case 26:r=_get_chunk(d,t,n,''7,''false);'//&
         'case 27:r=_get_chunk(d,t,n,''7,_left);'//&
         'case 28:r=_get_chunk(d,t,n,''7,_right);'//&
         'default:r=_get_chunk(d,t,n,''1,''true);'//&
         '};return r}',line)

    call dcl_uproc(parser,'get_chunk(d:shape,t:grid,n:extent,i:int) {'//&
         '_cr(i,1);const r;switch i {case 0:r=empty(#t);default:r=#t};return r}',line)
         
    ! *************************************************************
    ! nbr% and nbhd% intrinsics
    ! *************************************************************

    call dcl_type(parser,'disp_index is any_int,tuple(any_int)',line)
    call dcl_type(parser,&
         'disp_sub is disp_index,tuple(any_int or range(any_int))',line)
    
    ! *** Default ***
    call dcl_uproc(parser,'nbr%(x:chan,t:shared disp_index,v:shared){'//&
         ' test "Default and chan values must have same type in ""nbr"""=>same_type(x,v);'//&
         ' j=displace(region._mshape,here,t);'//&
         ' var y=v;if contains(region._mshape,j) {y:=x@[j]};'//&
         ' return y} ',line)
    call dcl_uproc(parser,&
         'nbhd%(x:chan,t:shared disp_sub,v:shared) { '//&
         ' test "Default and chan values must have same type in ""nbr"""=>same_type(x,v);'//&
         ' var a=array(v,#t);'//&
         ' foreach invar i in t {'//&
         '   j=displace(region._mshape,here,i);'//&
         '   if j in region._mshape {a[here]:=x@[j]}'//&
         ' };return a}',line)
    
    ! *** Blocked distributions ***
    call dcl_uproc(parser,&
         'nbr%(region:dshape(,blocked_distr),x:chan,t:shared disp_index,v:shared) {'//&
         ' test "Default and chan values must have same type in ""nbr"""=>same_type(x,v);'//&
         ' var j=displace(region._mshape,here,t);'//&
         ' a,ad=tile_with_halo%(x,t,v);'//&
         ' return a[ad#j]}',&
         line)
    call dcl_uproc(parser,&
         'nbhd%(region:dshape(,blocked_distr),x:chan,t:shared disp_sub,v:shared){ '//&
         ' test "Default and chan values must have same type in ""nbr"""=>same_type(x,v);'//&
         ' a=displace(region._mshape,here,t);'//&
         ' y,yd=tile_with_halo%(x,t,v);'//&
         ' return y[yd#a]'//&
         '} ',&
         line)
    
    call dcl_uproc(parser,'tile_with_halo%(x,t,v) {return a,d '//&
         'where a,d=_local_tile_with_halo(region,PM__local(x),t,v)}',line)
    
    ! Return local tile with halo cells 
    call dcl_uproc(parser,&
         '_local_tile_with_halo(region,x,t,v) { '//&
         ' this_tile_x=_get_halo(region._mshape,region._tile,t);'//&
         ' var a=array(v,#this_tile_x);'//&
         ' foreach i in nodes_for_grid(region.dist,this_tile_x) {'//&
         '   other_tile=_get_elem(region.dist,i);'//&
         '   other_tile_x=_get_halo(region._mshape,other_tile,t);'//&
         '   var p=index(dims(region.dist),i);'//&
         '   if contains(#region.dist,i) and p/=_this_node() {'//&
         '    _recv_slice(p,&a,overlap(this_tile_x,other_tile));'//&
         '   } '//&
         ' };'//&
         ' foreach i in '//&
         '  nodes_for_grid(region.dist,_get_anti_halo(region._mshape,region._tile,t)) {'//&
         '   other_tile=_get_elem(region.dist,i);'//&
         '   other_tile_x=_get_halo(region._mshape,other_tile,t);'//&
         '   p=index(dims(region.dist),i);'//&
         '   if contains(#region.dist,i) and p/=_this_node() {'//&
         '    _send_slice(p,x,overlap(region._tile,other_tile_x));'//&
         '   } '//&
         ' };'//&
         ' o,oo=overlap(this_tile_x,region._tile);a[o]:=x[oo];'//&
         ' _sync_messages(a,x);return a,this_tile_x }',line)

    call dcl_uproc(parser,&
         '_local_tile_with_halo(region,x:_comp,t,v) { '//&
         ' this_tile_x=_get_halo(region._mshape,region._tile,t);'//&
         ' var a=array(v,#this_tile_x);'//&
         ' foreach i in '//&
         '  nodes_for_grid(region.dist,_get_anti_halo(region._mshape,region._tile,t)) {'//&
         '   other_tile=_get_elem(region.dist,i);'//&
         '   other_tile_x=_get_halo(region._mshape,other_tile,t);'//&
         '   p=index(dims(region.dist),i);'//&
         '   if contains(#region.dist,i) and p/=_this_node() {'//&
         '    _send_slice(p,x,overlap(region._tile,other_tile_x));'//&
         '   } '//&
         ' };'//&
         ' o,oo=overlap(this_tile_x,region._tile);a[o]:=x[oo];'//&
         ' foreach i in nodes_for_grid(region.dist,this_tile_x) {'//&
         '   other_tile=_get_elem(region.dist,i);'//&
         '   other_tile_x=_get_halo(region._mshape,other_tile,t);'//&
         '   p=index(dims(region.dist),i);'//&
         '   if contains(dims(region.dist),i) and p/=_this_node() {'//&
         '    _recv_slice_sync(p,&a,overlap(this_tile_x,other_tile));'//&
         '   } '//&
         ' };'//&
         ' _sync_messages(a,x);return a,this_tile_x }',line)
   
    ! Displace x by y within mshape d
    call dcl_uproc(parser,'displace(d:range(int),x:int,y:any_int)='//&
         'x+int(y)',line)
    call dcl_uproc(parser,'displace(d:range(int),x:int,y:range(any_int))='//&
         'x+int(y._lo)..x+int(y._hi)',line)
    call dcl_uproc(parser,'displace(d:extent1d,x:tuple1d(int),y:range(any_int) or any_int)='//&
         'displace(d.1,x.1,y)',line)
    call dcl_uproc(parser,'displace(d:extent,'//&
         'x:tuple(int),y:tuple(range(any_int) or any_int))='//&
         'map($displace,d,x,y)',line)

 
    !************************************************
    ! TOPOLOGIES
    !************************************************
    
    call dcl_proc(parser,'_get_dims(int,int)->int',op_get_dims,0,&
         line,proc_is_impure)
    call dcl_proc(parser,'_get_dims(int,int,int)->int,int',&
         op_get_dims,0,&
         line,proc_is_impure)
    call dcl_proc(parser,'_get_dims(int,int,int,int)->int,int,int',&
         op_get_dims,0,&
         line,proc_is_impure)
    call dcl_proc(parser,&
         '_get_dims(int,int,int,int,int)->int,int,int,int',&
         op_get_dims,0,&
         line,proc_is_impure)
    call dcl_proc(parser,'_get_dims(int,int,int,int,int,int)->'//&
         'int,int,int,int,int',&
         op_get_dims,0,&
         line,proc_is_impure)
    call dcl_proc(parser,'_get_dims(int,int,int,int,int,int,int)->'//&
         'int,int,int,int,int,int',&
         op_get_dims,0,&
         line,proc_is_impure)
    call dcl_proc(parser,&
         '_get_dims(int,int,int,int,int,int,int,int)->'//&
         'int,int,int,int,int,int,int',&
         op_get_dims,0,&
         line,proc_is_impure)

    call dcl_uproc(parser,'_zd(x,y:null)=1',line)
    call dcl_uproc(parser,'_zd(x,y)=if(x==1=>1,0)',line)
    call dcl_uproc(parser,&
         'cart_topo(dd:tuple,t:null,n:int)=cart_topo(dd,dd,n)',line)
    call dcl_uproc(parser,&
         'cart_topo(dd:tuple,t,n:int)=cart_topo(dd,spread(t,dd),n)',line)
    call dcl_uproc(parser,&
         'cart_topo(d:tuple1d,t:tuple1d,n:int)='//&
         'tuple(_get_dims(n,_zd(d.1,t.1)))',line)
    call dcl_uproc(parser,'cart_topo(d:tuple2d,t:tuple2d,n:int)=tuple(b,a) '//&
         'where a,b=_get_dims(n,_zd(d.2,t.2),_zd(d.1,t.1))',line)
    call dcl_uproc(parser,'cart_topo(d:tuple3d,t:tuple3d,n:int)=tuple(c,b,a) '//&
         'where a,b,c=_get_dims(n,_zd(d.3,t.3),_zd(d.2,t.2),_zd(d.1,t.1))',line)
    call dcl_uproc(parser,'cart_topo(d:tuple4d,t:tuple4d,n:int)=tuple(dd,c,b,a) '//&
         'where a,b,c,dd=_get_dims(n,_zd(d.4,t.4),_zd(d.3,t.3),'//&
         '_zd(d.2,t.2),_zd(d.1,t.1))',line)
    call dcl_uproc(parser,'cart_topo(d:tuple5d,t:tuple5d,n:int)=tuple(e,dd,c,b,a) '//&
         'where a,b,c,dd,e=_get_dims(n,_zd(d.5,t.5),_zd(d.4,t.4),'//&
         '_zd(d.3,t.3),_zd(d.2,t.2),_zd(d.1,t.1))',line)
    call dcl_uproc(parser,'cart_topo(d:tuple6d,t:tuple6d,n:int)=tuple(f,e,dd,c,b,a) '//&
         'where a,b,c,dd,e,f=_get_dims(n,_zd(d.6,t.6),_zd(d.5,t.5),'//&
         '_zd(d.4,t.4),_zd(d.3,t.3),_zd(d.2,t.2),_zd(d.1,t.1))',line)
    call dcl_uproc(parser,'cart_topo(d:tuple7d,t:tuple7d,n:int)=tuple(g,f,e,dd,c,b,a) '//&
         'where a,b,c,dd,e,f,g=_get_dims(n,_zd(d.7,t.7),_zd(d.6,t.6),'//&
         '_zd(d.5,t.5),_zd(d.4,t.4),_zd(d.3,t.3),_zd(d.2,t.2),_zd(d.1,t.1))',line)    
     
    ! ************************************************
    ! DISTRIBUTIONS
    ! ************************************************
    call dcl_type(parser,&
         'distr_dim is no_distr,direct_distr,block_distr,vblock_distr,cyclic_distr,block_cyclic_distr',line)
    call dcl_type(parser,'distr:indexable is null,blocked_distr,distr_dim,tuple(distr_dim)',line)
    call dcl_type(parser,'blocked_distr is block_distr,vblock_distr,tuple(block_distr or vblock_distr)',line) 
    
    ! Null distribution (mirroring)
    call dcl_type(parser,'no_distr is rec {_hi:int,_p:int}',line)
    
    call dcl_uproc(parser,&
         'no_distr(g:int,d:int)='//&
         'new no_distr {_hi=int(g),_p=int(d)}',line)
    call dcl_uproc(parser,'#(b:no_distr)=shape([0..b._p-1])',line) 
    call dcl_uproc(parser,'_shp(b:no_distr)=0..b._p-1',line)
    call dcl_uproc(parser,'dims(b:no_distr)=[b._p]',line)
    call dcl_uproc(parser,'size(b:no_distr)=b._p',line)
    call dcl_uproc(parser,&
         '_get_elem(b:no_distr,i:int)=0..b._hi-1',line)
    call dcl_uproc(parser,&
         'tile_size(b:no_distr,i:int)=b._hi',line)
    call dcl_uproc(parser,'empty(b:no_distr)=1..0',line)
    call dcl_uproc(parser,'nodes_for_grid(b:no_distr,g:seq(int))='//&
         '0..0',line)
    call dcl_uproc(parser,'node_for(b:no_distr,j:int)=0',line)
    call dcl_uproc(parser,'index(b:no_distr,j:int,p:int)=j',line)
    call dcl_uproc(parser,'node_nhd(b:no_distr,p:int,d:range(int))=0..0',line)

    ! Direct distribution (1-1 map to processor topology)
    call dcl_type(parser,'direct_distr is rec {_p:int}',line)
    call dcl_uproc(parser,&
         'direct_distr(d:int)=new direct_distr {_p=int(d)}',line)
    call dcl_uproc(parser,'#(b:direct_distr)=shape([0..b._p-1])',line)
    call dcl_uproc(parser,'_shp(b:direct_distr)=0..b._p-1',line)
    call dcl_uproc(parser,'dims(b:direct_distr)=[b._p]',line)
    call dcl_uproc(parser,'size(b:direct_distr)=b._p',line)
    call dcl_uproc(parser,&
         '_get_elem(b:direct_distr,i:int)=''0..''0',line)
    call dcl_uproc(parser,&
         'tile_size(b:direct_distr,i:int)=''1',line)
    call dcl_uproc(parser,'empty(b:direct_distr)=''1..''0',line)
    call dcl_uproc(parser,'nodes_for_grid(b:direct_distr,g:seq(int))='//&
         'int(g)',line)
    call dcl_uproc(parser,'node_for(b:direct_distr,j:int)=j',line)
    call dcl_uproc(parser,'index(b:direct_distr,j:int,p:int)=''0',line)
    call dcl_uproc(parser,'node_nhd(b:direct_distr,p:int,d:int or range(int))=d+p',line)
    call dcl_uproc(parser,'node_co_nhd(b:direct_distr,p:int,d:int or range(int))='//&
         '-high(d)+p..-low(d)+p by -1',line)
    
    ! Variable block distribution
    call dcl_type(parser,'vblock_distr is rec {_hi:int,_p:int}',line)
    call dcl_uproc(parser,&
         'vblock_distr(g:int,d:int)='//&
         'new vblock_distr {_hi=int(g),_p=int(d)}',line)
    call dcl_uproc(parser,'#(b:vblock_distr)=shape([0..b._p-1])',line)
    call dcl_uproc(parser,'_shp(b:vblock_distr)=0..b._p-1',line)
    call dcl_uproc(parser,'dims(b:vblock_distr)=[b._p]',line)
    call dcl_uproc(parser,'size(b:vblock_distr)=b._p',line)
    call dcl_uproc(parser,&
         '_get_elem(b:vblock_distr,i:int)=start..finish'//&
         ' where finish=(ii+1)*b._hi/b._p-1'//&
         ' where start=ii*b._hi/b._p where ii=int(i)',line)
    call dcl_uproc(parser,&
         'tile_size(b:vblock_distr,i:int)=finish-start+1'//&
         ' where finish=(ii+1)*b._hi/b._p-1'//&
         ' where start=ii*b._hi/b._p where ii=int(i)',line)
    call dcl_uproc(parser,'empty(b:vblock_distr)=1..0',line)
    call dcl_uproc(parser,'nodes_for_grid(b:vblock_distr,g:seq(int))='//&
         'lo1..hi1 where'//&
         ' lo1=_rdiv(b._p*(int(low(g))+1)-1,b._hi),'//&
         ' hi1=_rdiv(b._p*(int(high(g))+1)-1,b._hi)',line)
    call dcl_uproc(parser,'node_for(b:vblock_distr,j:int)=p'//&
         ' where p=_rdiv(b._p*(jj+1)-1,b._hi) where jj=int(j)',line)
    call dcl_uproc(parser,'index(b:vblock_distr,j:int,p:int)=i'//&
         ' where i=jj-s1'//&
         ' where s1=p*b._hi/b._p'//&
         ' where jj=int(j)',line)
    call dcl_uproc(parser,'_rdiv(x,y)=if(x>0=>x/y,(x-y+1)/y)',line)
    call dcl_uproc(parser,'node_nhd(b:vblock_distr,p:int,d:range(int))='//&
         'node_for(b,low(r)+low(d))..node_for(b,high(r)+high(d))'//&
         ' where r=_get_elem(b,p)',line)
    call dcl_uproc(parser,'node_co_nhd(b:vblock_distr,p:int,d:range(int))='//&
         'node_for(b,high(r)-low(d))..node_for(b,low(r)-high(d)) by -1'//&
         ' where r=_get_elem(b,p)',line)
 
    ! fixed block distribution
    call dcl_type(parser,'block_distr is rec {_b:int,_s:int,_p:int}',line)
    call dcl_uproc(parser,&
         'block_distr(s:int,t:int,b:int)='//&
         'new block_distr {_b=b,_s=s,_p=p} where p=(s+b-1)/b',line)
    call dcl_uproc(parser,'#(b:block_distr)=shape([0..b._p-1])',line)
    call dcl_uproc(parser,'_shp(b:block_distr)=0..b._p-1',line)
    call dcl_uproc(parser,'dims(b:block_distr)=[b._p]',line)
    call dcl_uproc(parser,'size(b:block_distr)=b._p',line)
    call dcl_uproc(parser,&
         '_get_elem(b:block_distr,i:int)=start..finish'//&
         ' where finish=min(b._s-1,(ii+1)*b._b-1)'//&
         ' where start=ii*b._b where ii=int(i)',line)
    call dcl_uproc(parser,&
         'tile_size(b:block_distr,i:int)=finish-start+1'//&
         ' where finish=min(b._s-1,(ii+1)*b._b-1)'//&
         ' where start=ii*b._b where ii=int(i)',line)
    call dcl_uproc(parser,&
         'empty(b:block_distr)=1..0',line)
    call dcl_uproc(parser,'nodes_for_grid(b:block_distr,g:seq(int))='//&
         'lo1..hi1 where'//&
         ' lo1=_rdiv(int(low(g)),b._b),'//&
         ' hi1=_rdiv(int(high(g)),b._b)',line)
    call dcl_uproc(parser,'node_for(b:block_distr,j:int)=p'//&
         ' where p=_rdiv(jj,b._b) where jj=int(j)',line)
    call dcl_uproc(parser,'index(b:block_distr,j:int,p:int)=i'//&
         ' where i=jj-s1'//&
         ' where s1=p*b._b'//&
         ' where jj=int(j)',line)
    call dcl_uproc(parser,'node_nhd(b:block_distr,p:int,d:range(int))='//&
         'p+(low(d)-b._b+1)/b._b..p+(high(d)+b._b-1)/b._b',line)
    call dcl_uproc(parser,'node_co_nhd(b:block_distr,p:int,d:range(int))='//&
         'p+(-low(d)+b._b-1)/b._b..p+(-high(d)-b._b+1)/b._b by -1',line)

    ! Cyclic distribution
    call dcl_type(parser,'cyclic_distr is rec {_hi:int,_p:int}',line)
    call dcl_uproc(parser,&
         'cyclic_distr(g:int,d:int)='//&
         'new cyclic_distr {_hi=int(g),_p=int(d)}',line)
    call dcl_uproc(parser,'#(b:cyclic_distr)=shape([0..b._p-1])',line)
    call dcl_uproc(parser,'_shp(b:cyclic_distr)=0..b._p-1',line)
    call dcl_uproc(parser,'dims(b:cyclic_distr)=[b._p]',line)
    call dcl_uproc(parser,'size(b:cyclic_distr)=b._p',line)
    call dcl_uproc(parser,&
         '_get_elem(b:cyclic_distr,i:int)='//&
         ' int(i)..b._hi-1 by b._p',line)
    call dcl_uproc(parser,&
         'tile_size(b:cyclic_distr,i:int)='//&
         '(b._hi-1-int(i))/b._p+1',line)
    call dcl_uproc(parser,&
         'empty(b:cyclic_distr)=1..0 by 1',line)
    call dcl_uproc(parser,'_cyx(p,low,high){'//&
         ' var lo=0;var hi=p-1;'//&
         ' if high-low<p {'//&
         '  lo2=low mod p;'//&
         '  hi2=high mod p;'//&
         '  if hi2>lo2 {lo:=lo2;hi:=hi2}'//&
         ' };'//&
         'return lo,hi}',line)    
    call dcl_uproc(parser,'nodes_for_grid(b:cyclic_distr,g:seq(int))='//&
         'lo1..hi1 '//&
         ' where lo1,hi1=_cyx(b._p,int(low(g)),int(high(g)))',&
         line)
    call dcl_uproc(parser,'node_for(b:cyclic_distr,j:any_int)=p'//&
         ' where p=int(j) mod b._p',line)
    call dcl_uproc(parser,'index(b:cyclic_distr,j:int,p:int)=int(j)/b._p',line)
    call dcl_uproc(parser,'node_nhd(b:cyclic_distr,p:int,d:range(int))='//&
         'p+low(d)..p+high(d)',line)
    call dcl_uproc(parser,'node_co_nhd(b:cyclic_distr,p:int,d:range(int))='//&
         'p-low(d)..p-high(d) by -1',line)

    ! Block cyclic distribution
    call dcl_type(parser,&
         'block_cyclic_distr is rec {_hi:int,_p:int,_b:int,_s:int}',line)
    call dcl_uproc(parser,&
         'block_cyclic_distr(g:int,p:int,b:int)='//&
         'new block_cyclic_distr {_hi=int(g),_p=int(p),_b=int(b),'//&
         ' _s=s}'//&
         ' where s=p*b',line)    
    call dcl_uproc(parser,'#(b:block_cyclic_distr)=shape([0..b._p-1])',line)
    call dcl_uproc(parser,'_shp(b:block_cyclic_distr)=0..b._p-1',line)
    call dcl_uproc(parser,'dims(b:block_cyclic_distr)=[b._p]',line)
    call dcl_uproc(parser,'size(b:block_cyclic_distr)=b._p',line)
    call dcl_uproc(parser,&
         '_get_elem(b:block_cyclic_distr,i:int)='//&
         ' block_seq(s,b._hi-1,b._s,b._b,0)'//&
         ' where s=ii*b._b where ii=int(i)',&
         line)
    call dcl_uproc(parser,&
         'tile_size(b:block_cyclic_distr,i:int)='//&
         'nc*b._b+max(0,min(b._b,df-nc*b._s)) where nc=df/b._s where df=b._hi-s'//&
         ' where s=ii*b._b where ii=int(i)',&
         line)
    call dcl_uproc(parser,&
         'empty(b:block_cyclic_distr)=block_seq(1,0,1,1)',line)
    call dcl_uproc(parser,'_cybx(b,p,low,high) {'//&
         ' var lo=0;var hi=p;'//&
         ' if high-low<p*b {'//&
         '  lo2=_rdiv(low,b) mod p;'//&
         '  hi2=_rdiv(high,b) mod p;'//&
         '  if hi2>lo2 {lo:=lo2;hi:=hi2};'//&
         ' }; '//&
         ' return lo,hi}',line)
    call dcl_uproc(parser,'nodes_for_grid(b:block_cyclic_distr,g:seq(int))='//&
         'lo1..hi1 '//&
         ' where lo1,hi1=_cybx(b._b,b._p,int(low(g)),int(high(g)))',&
         line)
    call dcl_uproc(parser,'nodes_for_grid(b:block_cyclic_distr,g:block_seq){'//&
         ' var r=0..b._p;'//&
         ' if b._s==step(g) {r:=nodes_for_grid(b,low(g)..low(g)+width(g)-1)};return r}',line)
    call dcl_uproc(parser,'node_for(b:block_cyclic_distr,j:int)=p'//&
         ' where p=_rdiv(jj,b._b) mod b._p where jj=int(j)',line)
    call dcl_uproc(parser,'index(b:block_cyclic_distr,j:int,p:int)=i'//&
         ' where i=r+b._b*(s-p)'//&
         ' where r=j-s*b._s'//&
         ' where s=_rdiv(j,b._s)',line)
    call dcl_uproc(parser,'node_nhd(b:block_cyclic_distr,p:int,d:range(int))='//&
         'p+(low(d)-b._b+1)/b._b..p+(high(d)+b._b-1)/b._b',line)
    call dcl_uproc(parser,'node_co_nhd(b:block_cyclic_distr,p:int,d:range(int))='//&
         'p+(-low(d)+b._b-1)/b._b..p+(-high(d)-b._b+1)/b._b by -1',line)

    
    ! Tuple of distributions
    call dcl_uproc(parser,'node_for(b:tuple(distr_dim),j:tuple(int))='//&
         'map($node_for,b,j)',line)
    call dcl_uproc(parser,'#(b:tuple(distr_dim))=shape(map($_shp,b))',line)
    call dcl_uproc(parser,'dims(b:tuple(distr_dim))=map($size,b)',line)
    call dcl_uproc(parser,'_get_elem(b:tuple(distr_dim),i:tuple(int))'//&
         '=map($_get_elem,b,i)',line)
    call dcl_uproc(parser,'_get_elem(b:tuple(distr_dim),i:int)='//&
         '_get_elem(b,index2point(i,dims(b)))',line)
    call dcl_uproc(parser,'tile_size(b:tuple(distr_dim),i:tuple(int))'//&
         '=map($tile_size,b,i)',line)
    call dcl_uproc(parser,'empty(b:tuple(distr_dim))=map($empty,b)',line)
    call dcl_uproc(parser,'nodes_for_grid(b:tuple(distr_dim),g:grid)'//&
         '=map($nodes_for_grid,b,g)',line)
    call dcl_uproc(parser,'node_num_for(b:tuple(distr_dim),j:tuple(int))'//&
         '=index(dims(b),map($node_for,b,j))',line)
    call dcl_uproc(parser,'index(b:tuple(distr_dim),j:tuple(int),p:tuple(int))'//&
         '=index(tile_size(b,p),map($index,b,j,p))',line)
    call dcl_uproc(parser,'node_and_index(b:tuple(distr_dim),j:tuple(int))'//&
         '=index(dims(b),p),i where i=index(tile_size(b,p),map($index,b,j,p)) '//&
         ' where p=map($node_for,b,j)',line)
    call dcl_uproc(parser,'node_and_index(b:distr_dim,j:int)=p,i '//&
         'where i=index(b,j,p) where p=node_for(b,j)',line)
    call dcl_uproc(parser,'node_nhd(b:tuple,p:tuple,d:tuple)=map($node_nhd,b,p,d)',line)
    call dcl_uproc(parser,'node_co_nhd(b:tuple,p:tuple,d:tuple)=map($node_co_nhd,b,p,d)',line)
    
    ! *****************************************
    ! SUPPORT FOR PARALLEL STATEMENTS
    ! *****************************************

    ! Get and set elements in "for"
    call dcl_uproc(parser,'PM__getelem(x:grid_slice_dim,y)=_get_elem(x,y)',line)
    call dcl_uproc(parser,'PM__getelem(x:grid_slice,y)=_get_elem(x,y)',line)
    call dcl_uproc(parser,'PM__getelem(a:any^mshape,t)=_get_aelem(a,index(dims(a),t))',line)
    call dcl_uproc(parser,'PM__getelem(a:array_slice(any^any,),y)=_get_elem(a,y)',line)
    call dcl_uproc(parser,'PM__getelem(x:array_template,y)=x._a',line)
!!$    call dcl_uproc(parser,'PM__getelem(a:any^dshape,t)=_get_aelem(a,i) '//&
!!$         'check p==_this_node() where p,i=node_and_index(_shape(a).dist,_tup(t))',line)
    call dcl_uproc(parser,'PM__getelem(a:any^dshape,t)=_get_elem(a,(#a)[t])',line)
    
    call dcl_uproc(parser,'PM__setelem(&x,v,y) {_set_elem(&x,v,(#x)[y])}',line)
    call dcl_uproc(parser,'PM__setelem(&a:any^mshape,v,t:index)'//&
         '{ PM__setaelem(&a,index(dims(a),t),v) }',line)
    call dcl_uproc(parser,'PM__setelem(&a:any^dshape,v,t:index) {'//&
         ' PM__setaelem(&a,i,v) check p==_this_node() '//&
         '  where p,i=node_and_index((#a).dist,_tup(t)) }',line)
    call dcl_uproc(parser,'PM__setelem(&a:array_slice(any^any,),v,t:index) {'//&
         ' _set_elem(&a._a,v,a._s[t]) }',line)

    call dcl_uproc(parser,'PM__get_elem%(x:shared,i,h)=PM__getelem(x,h)',line)
    call dcl_uproc(parser,'PM__set_elem%(&x:shared,v:complete,i,h)'//&
         '{PM__setelem(&x,v,h <<PM__ignore>>);_assemble(&x,region <<shared,always>>)}',line)
    
    call dcl_uproc(parser,'PM__get_elem%(x:shared any^dshape,i,h)='//&
         '_get_elem(PM__local(x),i)',line)
    call dcl_uproc(parser,'PM__set_elem%(&x:shared any^dshape,v:complete,i,h) '//&
         '{_set_elem(&x,v,i <<PM__ignore>>)}',line)

    call dcl_uproc(parser,'_assemble(&a:any^mshape,region:mshape) {}',line)
    
    call dcl_uproc(parser,'_assemble(&a:array_slice(any^shape,),region:mshape) {}',line)
    
    call dcl_uproc(parser,'_assemble(&a:any^mshape,region) {'//&
         ' dist=region.dist; '//&
         ' foreach p in #(dist) {'//&
         '   tile=dist[p];'//&
         '   i=index(dims(dist),p);'//&
         '   if i==_this_node() {'//&
         '     for j in tile  <<conc>>{ '//&
         '       var k=PM__getelem(a,j); '//&
         '       PM__broadcast(&k,i)'//&
         '     };'//&
         '   } else { '//&
         '     for j in tile  <<conc>>{ '//&
         '       var k=_arb(a);'//&
         '       PM__broadcast(&k,i);'//&
         '       PM__setelem(&a,k,j <<PM__ignore>>)'//&
         '     }'//&
         '   }'//&
         ' } }',&
         line)
  
   call dcl_uproc(parser,'_assemble(&a:array_slice(any^shape,),region) {'//&
        ' dist=region.dist; '//&
         ' foreach p in #(dist) {'//&
         '   tile=intersect((#(a._a))#a._s,dist[p]);'//&
         '   i=index(dims(dist),p);'//&
         '   if i==_this_node() {'//&
         '     for j in tile  <<conc>>{ '//&
         '       var k=PM__getelem(a._a,j); '//&
         '       PM__broadcast(&k,i)'//&
         '     };'//&
         '   } else { '//&
         '     for j in tile  <<conc>>{ '//&
         '       var k=_arb(a._a);'//&
         '       PM__broadcast(&k,i);'//&
         '       PM__setelem(&a._a,k,j <<PM__ignore>>)'//&
         '     }'//&
         '   }'//&
         ' } }',&
         line)
        
    ! Support for % procs
    call dcl_uproc(parser,'PM__get_tilesz(d)=d._tile,d._size',line)
    call dcl_uproc(parser,'PM__get_tilesz(d:mshape)=d,size(d)',line)
    
    ! Support for @ operator
    if(pm_is_compiling) then
       call dcl_uproc(parser,&
            'PM__makearray%(x:chan)<<complete,always>>=_makearray(x,region,size(region))',line)
       call dcl_uproc(parser,&
            'PM__makearray%(x:priv)=_makearray(x,region,size(region))'//&
            ':test "Can only apply ""@"" to a ""chan"" " => ''false',line)
       call dcl_uproc(parser,&
            'PM__makearray%(x:invar)=_makearray(x,region,size(region))'//&
            ':test "Cannot apply ""@"" to a ""shared"" or ""uniform"" value" => ''false',line)
       call dcl_proc(parser,&
            '_makearray(x:any,y:any,z:any)->PM__invar_dim x,y',&
            op_make_array,0,line,proc_needs_type)
    else
       call dcl_uproc(parser,&
            'PM__makearray%(x:chan)<<complete,always>>=_makearray(x,region)',line)
       call dcl_uproc(parser,&
            'PM__makearray%(x:priv)=_makearray(x,region)'//&
            ':test "Can only apply ""@"" to a ""chan"" " => ''false',line)
       call dcl_uproc(parser,&
            'PM__makearray%(x:invar)=_makearray(x,region)'//&
            ':test "Cannot apply ""@"" to a ""shared"" or ""uniform"" value" => ''false',line)
       call dcl_proc(parser,&
            '_makearray(x:any,y:any)->PM__dim x,y',&
            op_make_array,0,line,proc_needs_type)
!!$       call dcl_uproc(parser,'PM__correctarray(x)=_redim(PM__export
    endif
    
    ! active%() intrinsic
    call dcl_uproc(parser,'active%(x)=masked(^(x,coherent),^(^??,coherent) <<complete,always,PM__ignore>>)',line)
    call dcl_uproc(parser,'active%()=^(^??,coherent)',line)
    call dcl_proc(parser,'PM__active()->bool',op_active,0,line,0)
    
    ! Imports and exports
    call dcl_proc(parser,'_import_val(x:any)->=x',&
         op_import_val,0,line,0)
    call dcl_proc(parser,'PM__importshrd(x:any)->=x',&
         op_import_val,0,line,0)
    call dcl_proc(parser,'PM__importvarg(x:any)->=x',&
         op_import_varg,0,line,proc_is_not_inlinable)
    call dcl_proc(parser,'_import_scalar(x:any)->invar x',&
         op_import_scalar,0,line,0)
    call dcl_uproc(parser,'PM__import_val(x) {PM__checkimp(x);return _import_val(x)}',line)
    call dcl_uproc(parser,'PM__impscalar(x) {PM__checkimp(x);return _import_scalar(x)}',line)
    call dcl_uproc(parser,'PM__import_val(x:^*(,,,,)) {'//&
         'test "Compiler internal error:importing reference" => ''false;return x}',line)
    call dcl_uproc(parser,'PM__impscalar(x:^*(,,,,)) {'//&
         'test "Compiler internal error:importing reference" => ''false;return x}',line)

    call dcl_uproc(parser,'PM__checkimp(x,arg...) {PM__checkimp(x);PM__checkimp(arg...)}',line)
    call dcl_uproc(parser,'PM__checkimp(x) {}',line)
    call dcl_uproc(parser,'PM__checkimp(x:contains(PM__distr_tag)) {'//&
         'test "Cannot import a distributed value into a nested parallel scope" => ''false}',&
         line)

    call dcl_type(parser,'schedule(subregion,blocking) is '//&
         'rec{_subregion:subregion,_subtile,_blocking:blocking}',line)
    call dcl_uproc(parser,'subregion(schedule:schedule)=schedule._subregion',line)
    call dcl_uproc(parser,'subregion(schedule:null)=null',line)
    call dcl_uproc(parser,'subtile(schedule:schedule)=schedule._subtile',line)
 
    ! Over statements
     call dcl_uproc(parser,'PM__over%(schedule:null,x:invar,block:invar)<<shared>>='//&
         'new schedule{_subregion=x,_subtile=overlap(region._tile,x),_blocking=_blocking(block,region)}',line)
    call dcl_uproc(parser,'PM__over%(x:invar,block:invar)<<shared>>='//&
         'new schedule{_subregion=s,_subtile=overlap(region._tile,s),_blocking=_blocking(block,region)}'//&
         'where s=intersect(x,schedule._subregion)',line)
    call dcl_uproc(parser,'PM__make_over%(schedule:null,'//&
         'x:invar tuple(subs_dim except stretch_dim),block:invar)<<shared>>='//&
         'new schedule{_subregion=s,_subtile=overlap(region._tile,s),_blocking=_blocking(block,region)}'//&
         'check "Value"++s++" in ""over"" out of bounds: "++region._extent=>region._extent inc s '//&
         'where s=fill_in(region._extent,x)',line)
    call dcl_uproc(parser,'PM__make_over%(x:invar tuple(subs_dim except stretch_dim)'//&
         ',block:invar)<<shared>>='//&
         'new schedule{_subregion=s,_subtile=overlap(region._tile,s),_blocking=_blocking(block,region)}'//&
         'where s=intersect(map($norm,fill_in(region._extent,x)),schedule._subregion)',line)
    call dcl_uproc(parser,'PM__make_over%(x:invar,block)=x'//&
         ' check "Expression in an ""over"" statement must be a subscript tuple"=>''false',line)
    call dcl_uproc(parser,'PM__make_over%(x,block)=x'//&
         ' check "Expression in an ""over"" statement must be ""invar"""=>''false',line)

    call dcl_uproc(parser,'_blocking(b:tuple(any_int),region)=int(b) {'//&
         'test "Blocking factor must have same rank as current region"=>'//&
         '  rank(b)==rank(extent(region))}',&
         line)
    call dcl_uproc(parser,'_blocking(b,region)=null {'//&
         'test "Blocking factor must be a tuple of integers"=>''false}',line)
    call dcl_uproc(parser,'_blocking(b:null,region)=null',line)
    
    
    if(.not.pm_is_compiling) then
       call dcl_uproc(parser,'PM__do_over%(x:null)=true',line)
       call dcl_uproc(parser,'PM__do_over%(x:invar schedule(tuple(seq or block_seq)))=here in x._subregion',line)
       call dcl_uproc(parser,'PM__do_over%(x:invar schedule(grid))=PM__do_over%(schedule._subtile)',line)
       call dcl_uproc(parser,'PM__do_over%(x:invar grid) <<complete,always>>'//&
            '{chan var t=false;'//&
            ' _in(x,&^(PM__local(^(&t@))) <<shared,always,PM__ignore>>);'//&
            ' return t}',line)
       call dcl_uproc(parser,'PM__do_over%(x:invar tuple(seq or block_seq),h:complete)=h in x',line)
       call dcl_uproc(parser,'_in(x,&t){for i in x  <<conc>>{sync t[i]:=true}}',line)
    else
       call dcl_uproc(parser,'PM__do_over(x:null,region)=x',line)
       call dcl_uproc(parser,'PM__do_over(x:schedule,region)='//&
            '_st(map_apply($_do_elem,$_st,x._subtile),_ldims(region),x._blocking)',line)
       call dcl_uproc(parser,'_do_elem(x:range(int))=_st(low(x),high(x))',line)
       call dcl_uproc(parser,'_do_elem(x:strided_range(int))=_st(low(x),high(x),step(x))',line)
       call dcl_uproc(parser,'_do_elem(x:block_seq)=_st(low(x),high(x),step(x),width(x),align(x))',line)
       call dcl_uproc(parser,'_do_elem(x:map_seq)=x.array',line)
       call dcl_uproc(parser,'_do_elem(x:single_point)=_st(x._t)',line)
       call dcl_uproc(parser,'PM__nested_loop(x:null){}',line)
       call dcl_proc(parser,'PM__nested_loop(any)',op_nested_loop,0,line,0)
       call dcl_uproc(parser,'_ldims(x:mshape)=map_apply($size,$_st,x._extent)',line)
       call dcl_uproc(parser,'_ldims(x:dshape)=map_apply($size,$_st,x._tile)',line)
    endif
        
    ! Parallel processing inquiry
    call dcl_proc(parser,'_sys_node()->int',op_sys_node,0,line,0)
    call dcl_proc(parser,'sys_nnode()->int',op_sys_nnode,0,line,0)
    call dcl_proc(parser,'_this_node()->int',op_this_node,1,line,0)
    call dcl_proc(parser,'this_node%(r:any,s:any,h:any)->int',op_this_node,2,line,0)
    call dcl_proc(parser,'this_nnode()->int',op_this_nnode,0,line,0)
    call dcl_proc(parser,'_shrd_node()->int',op_shared_node,0,line,0)
    call dcl_proc(parser,'shrd_nnode()->int',op_shared_nnode,0,line,0)
    call dcl_proc(parser,'_root_node()->int',op_root_node,0,line,0)
    call dcl_proc(parser,'is_shrd()->bool',op_is_shared,0,line,0)
    call dcl_proc(parser,'is_shrd(any)->bool',op_is_shared,0,line,0)
    call dcl_proc(parser,'is_par()->bool',op_is_par,0,line,0)
    call dcl_uproc(parser,'_head_node()=_shrd_node()==0',line)

    ! Parallel system nested contexts
    call dcl_proc(parser,'_push_node_grid(arg...:any)',&
         op_push_node_grid,0,line,proc_is_impure+proc_has_for)
    call dcl_uproc(parser,'_push_node(d:int,t:int){'//&
         '_push_node_grid(false,t)}',line)
    call dcl_uproc(parser,'_push_node(d:tuple1d,t:tuple1d,e:tuple1d) { '//&
         '_push_node_grid(is_cyclic(e.1),t.1) }',line)
    call dcl_uproc(parser,'_push_node(d:tuple2d,t:tuple2d,e:tuple2d) { '//&
         '_push_node_grid(is_cyclic(e.1),is_cyclic(e.2),t.1,t.2) }',&
         line)
    call dcl_uproc(parser,'_push_node(d:tuple3d,t:tuple3d,e:tuple3d) { '//&
         '_push_node_grid(is_cyclic(e.1),is_cyclic(e.2),is_cyclic(e.3),'//&
         't.1,t.2,t.3) }',line)
    call dcl_uproc(parser,'_push_node(d:tuple4d,t:tuple4d,e:tuple4d) { '//&
         '_push_node_grid(is_cyclic(e.1),is_cyclic(e.2),is_cyclic(e.3),'//&
         'is_cyclic(e.4),'//&
         't.1,t.2,t.3,t.4) }',line)
    call dcl_uproc(parser,'_push_node(d:tuple5d,t:tuple5d,e:tuple5d) { '//&
         '_push_node_grid(is_cyclic(e.1),is_cyclic(e.2),is_cyclic(e.3),'//&
         'is_cyclic(e.4),is_cyclic(e.5),'//&
         't.1,t.2,t.3,t.4,t.5) }',line)
    call dcl_uproc(parser,'_push_node(d:tuple6d,t:tuple6d,e:tuple6d) { '//&
         '_push_node_grid(is_cyclic(e.1),is_cyclic(e.2),is_cyclic(e.3),'//&
         'is_cyclic(e.4),is_cyclic(e.5),is_cyclic(e.6),'//&
         't.1,t.2,t.3,t.4,t.5,t.6) }',line)
    call dcl_uproc(parser,'_push_node(d:tuple7d,t:tuple7d,e:tuple7) { '//&
         '_push_node_grid(is_cyclic(e.1),is_cyclic(e.2),is_cyclic(e.3),'//&
         'is_cyclic(e.4),is_cyclic(e.5),is_cyclic(e.6),is_cyclic(e.7),'//&
         't.1,t.2,t.3,t.4,t.5,t.6,t.7) }',line)

    call dcl_proc(parser,'_push_node_split(int)',op_push_node_split,&
         0,&
         line,proc_is_impure+proc_has_for)
    call dcl_proc(parser,'_push_node_conc()',op_push_node_conc,0,&
         line,0)
    call dcl_uproc(parser,'PM__pop_node(x:mshape) {}',line)
    call dcl_proc(parser,'PM__pop_node(x:shape)',op_pop_node,0,line,0)
    call dcl_proc(parser,'PM__pop_conc(bool)',op_pop_node_conc,&
         0,line,0)
    call dcl_proc(parser,'_push_node_dist()',op_push_node_distr,0,line,proc_is_impure+proc_has_for)
    call dcl_uproc(parser,'_lvl()=1',line)

    
    ! ************************************************
    ! PROCESSOR ALLOCATION
    ! ************************************************
    
    call dcl_uproc(parser,&
         'PM__partition(pp:null,dd:dshape)='//&
         ' dd._tile,#dd._mshape,null',line)
    call dcl_uproc(parser,&
         'PM__partition(pp,d:dshape) {'//&
         '_push_node_dist();return dd._tile,dd,null where dd=new dshape {_mshape=#d._mshape,dist=d.dist,'//&
         '_tile=d._tile,_tilesz=d._tilesz,_size=d._size,_level=d._level'//&
         '}}',line)
    call dcl_uproc(parser,&
         'PM__partition(pp,d:dshape,distr:null,topo:null,simplify:null,work:null,sched,block) {'//&
         '_push_node_dist();return dd._tile,dd,_block_schedule(block,dd)'//&
         ' where dd=new dshape {_mshape=#d._mshape,dist=d.dist,'//&
         '   _tile=d._tile,_tilesz=d._tilesz,_size=d._size,_level=d._level'//&
         '}}',line)
    call dcl_uproc(parser,&
         'PM__partition(pp,d:dshape,distr,topo,simplify,work,sched,block) {'//&
         'test "Cannot have attributes in ""for""statement over distributed value" => ''false;'//&
         'return dd._tile,dd,null where dd=new dshape {_mshape=##d,dist=d.dist,'//&
         '_tile=d._tile,_tilesz=d._tilesz,_size=d._size,_level=d._level'//&
         '}}',&
         line)
    call dcl_uproc(parser,'PM__partition(pp,d:mshape)=tile,shape,sched'//&
         ' where tile,shape,sched='//&
         '   PM__partition(pp,d,VBLOCK,null,null,null,null,null)',line) 
    call dcl_uproc(parser,&
         'PM__partition(pp,mshape:mshape,distr,topo,simplify,work,sched,block) {'//&
         ' d=dims(mshape);topol=topology(topo,distr,d,min(max(1,size(d)),shrd_nnode()));'//&
         ' var p=_shrd_node();'//&
         ' dist=distribute(distr,d,topol);'//&
         ' s=size(#(dist));np=shrd_nnode(); '//&
         ' test "requested topology "++#dist++" larger than available processors: "++s++">"++np=>s<=np;'//&
         ' if s<np { '//&
         '     if p>=s { '//&
         '      p:=workshare(work,mshape,dist,s,p-s,shrd_nnode()-s) '//&
         '     };'//&
         '     _push_node_split(p) '//&
         ' } else { '//&
         '     _push_node_dist() '//&
         ' }; '//&
         ' elem=_get_elem(dist,p); elemsz=#elem; '//&
         ' dd=new dshape {_mshape=#mshape,dist=dist,_tile=elem,_tilesz=elemsz,'//&
         '    _size=size(elemsz),_level=_lvl()};'//&
         'return dd._tile,dd,_block_schedule(block,dd) }',line)
    call dcl_uproc(parser,&
        'PM__partition(pp:null,d:mshape,distr,topo,simplify,work,sched,block)='//&
        '#d._extent,#d,_block_schedule(block,#d)',line)
    call dcl_uproc(parser,&
        'PM__partition(pp:null,d:dshape,distr,topo,simplify,work,sched,block)='//&
        '#d._extent,#d._mshape,_block_schedule(block,#d._mshape)',line)

    call dcl_uproc(parser,'_block_schedule(block:null,region)=null',line)
    call dcl_uproc(parser,'_block_schedule(block,region)='//&
         'new schedule{_subregion=region,_subtile=region._tile,_blocking=_blocking(block,region)}',line)

    call dcl_uproc(parser,&
         'topology(tp:null,dis,d:tuple,l:int)=cart_topo(int(d),dis,l)',line)
    call dcl_uproc(parser,&
         'topology(tp,dis,d,l:int)=tp',line)

    call dcl_type(parser,&
         'distr_template is null,distr_template_dim,tuple(distr_template_dim),...',line)
    call dcl_type(parser,&
         'distr_template_dim is null,vblock_template,'//&
         'block_template,cyclic_template,block_cyclic_template,...',line)
    call dcl_type(parser,'vblock_template is unique{VBLOCK}',line)
    call dcl_type(parser,'_block_template is rec{block:int}',line)
    call dcl_type(parser,'_block_template_default is unique{BLOCK}',line)
    call dcl_type(parser,&
         'block_template is _block_template_default,_block_template',line)
    call dcl_uproc(parser,&
         'BLOCK(block:int)=new _block_template {block=block}',line)
    call dcl_type(parser,'cyclic_template is unique{CYCLIC}',line)
    call dcl_type(parser,'block_cyclic_template is rec {block:int}',line)
    call dcl_uproc(parser,'BLOCK_CYCLIC(block:int)=new block_cyclic_template{block=block}',line)

    call dcl_uproc(parser,&
         'distribute(dis:tuple(distr_template_dim),d:tuple(int),t:tuple(int))='//&
         'map($distribute,dis,d,t):print("Topo:"++t)',line)
    call dcl_uproc(parser,&
         'distribute(dis:distr_template_dim,d:tuple(int),t:tuple(int))='//&
         'map($distribute,spread(dis,d),d,t)',line)
    call dcl_uproc(parser,&
         'distribute(dis:null,d:tuple(int),t:tuple(int))=distribute(VBLOCK,d,t)',line)
    
    call dcl_uproc(parser,&
         'distribute(dis:null,d:int,t:int)='//&
         'no_distr(d,t)',line)
    call dcl_uproc(parser,&
         'distribute(dis:vblock_template,d:int,t:int)='//&
         'vblock_distr(d,t)',line)
    call dcl_uproc(parser,&
         'distribute(dis:_block_template,d:int,t:int)='//&
         'block_distr(d,t,dis.block)',line)
    call dcl_uproc(parser,&
         'distribute(dis:block_template,d:int,t:int)='//&
         'block_distr(d,t)',line)
    call dcl_uproc(parser,&
         'distribute(dis:cyclic_template,d:int,t:int)='//&
         'cyclic_distr(d,t)',line)
    call dcl_uproc(parser,&
         'distribute(dis:block_cyclic_template,d:int,t:int)='//&
         'block_cyclic_distr(d,t,dis.block)',line)
    
    call dcl_uproc(parser,&
         'workshare(work:null,d,dist,nnode:int,'//&
         'snode:int,nsnode:int)='//&
         'nnode*(2*snode+1)/(2*nsnode)',line)
    call dcl_uproc(parser,&
         'workshare(work:array(int),d,dist,nnode:int,snode:int,nsnode:int) { '//&
         'test "work array does not conform to mshape"=>'//&
         'conform(#work,#d); '//&
         'var wk=work;return _wshare(wk,nnode,snode,nsnode) }',line)
    call dcl_proc(parser,&
         '_wshare(int^any,int,int,int)->int',op_wshare,0,line,0)

    ! *************************************************************
    ! I/O OPERATIONS
    ! *************************************************************

    ! Built-in operators
    call dcl_proc(parser,'_open_file(string,bool,bool,bool,bool,bool,bool,bool)->sint,sint',&
         op_open_file,0,line,proc_is_impure+proc_is_variant)
    call dcl_proc(parser,'_close_file(sint)->sint',&
         op_close_file,0,line,proc_is_impure+proc_is_variant)
    call dcl_proc(parser,'_seek_file(sint,lint)->sint',&
         op_seek_file,0,line,proc_is_impure+proc_is_variant)
    call dcl_proc(parser,'_read_file(sint,&any)->sint',&
         op_read_file,0,line,proc_is_impure+proc_is_variant)
    call dcl_proc(parser,'_write_file(sint,any)->sint',&
         op_write_file,0,line,proc_is_impure+proc_is_variant)
    call dcl_proc(parser,'_read_file_array(sint,&any,int)->sint',&
         op_read_file_array,0,line,proc_is_impure+proc_is_variant)
    call dcl_proc(parser,'_write_file_array(sint,any,int)->sint',&
         op_write_file_array,0,line,proc_is_impure+proc_is_variant) 
    call dcl_proc(parser,'_read_file_tile%(any,any,any,sint,&any,int,int)->sint',&
         op_read_file_tile,0,line,proc_is_impure+proc_is_dcomm+proc_is_variant)
    call dcl_proc(parser,'_write_file_tile%(any,any,any,sint,any,int,int)->sint',&
         op_write_file_tile,0,line,proc_is_impure+proc_is_dcomm+proc_is_variant)
    call dcl_proc(parser,'_io_error_string(sint)->string',&
         op_io_error_string,0,line,proc_is_impure)

    ! IO/related types
    call dcl_type(parser,'io_type is num,bool',line)
    call dcl_type(parser,'filesystem is rec{_tag:PM__distr_tag}',line)
    call dcl_type(parser,'file is struct {_f:sint,_tag:PM__distr_tag}',line)
    call dcl_type(parser,'io_error is rec {_errno:sint,use _iserr:bool}',line)
    call dcl_uproc(parser,'PM__filesys()=new filesystem{}',line)

    ! Basic operations
    call dcl_uproc(parser,'open(&filesystem:filesystem,name,'//&
         'append=false,create=false,temp=false,'//&
         'excl=false,read=false,write=false,seq=false)=new file {_f=f},_make_file_error(err) '//&
         'where f,err=_open_file(name,append,create,'//&
         'temp,excl,read,write,seq)',line)
    call dcl_uproc(parser,'_make_file_error(x:sint)=new io_error {_errno=x,_iserr=x/=0}',line)
    call dcl_uproc(parser,'close(&f:file)'//&
         '{err=_close_file(f._f);return _make_file_error(err)}',line)
    call dcl_uproc(parser,'seek(&f:file,j:lint)'//&
         '{err=_seek_file(f._f,j);return _make_file_error(err)}',line)
    call dcl_uproc(parser,'read(&f:file,&x:io_type)'//&
         '{err=_read_file(f._f,&x);return _make_file_error(err)}',line)
    call dcl_uproc(parser,'write(&f:file,x:io_type)'//&
         '{err=_write_file(f._f,x);return _make_file_error(err)}',line)

    ! Array I/O
    call dcl_uproc(parser,&
         'read(&f:file,&x:io_type^mshape)'//&
         '{err=_read_file_array(f._f,&x,size(x));return _make_file_error(err)}',line)
    call dcl_uproc(parser,&
         'write(&f:file,x:io_type^mshape)'//&
         '{err=_write_file_array(f._f,x,size(x));return _make_file_error(err)}',line)
    call dcl_uproc(parser,&
         'read(&f:file,&x:io_type^dshape) '//&
         '{var err=_make_file_error(0''s);for i in x:err:=read%(&f,&i);return err}',&
         line)
    call dcl_uproc(parser,&
         'write(&f:file,x:io_type^dshape) '//&
         '{var err=_make_file_error(0''s);for i in x:err:=write%(&f,i)}',line)

    ! Distributed I/O
    call dcl_uproc(parser,'partition%(f:filesystem)=f:test "Partition not yet implemented"=>''false',line)
    call dcl_uproc(parser,&
         'read%(&f:shared file,&x:complete io_type)'//&
         '{err=_read_file_tile%(f._f,&x,index(dims(region._mshape),here),size(region._mshape));'//&
         'return _make_file_error(err)}',line)
    call dcl_uproc(parser,&
         'write%(&f:shared file,x:complete io_type)'//&
         '{err=_write_file_tile%(f._f,x,index(dims(region._mshape),here),size(region._mshape));'//&
         'return _make_file_error(err)}',line)

    ! Error trapping versions of I/O routines
    call dcl_uproc(parser,'string(error:io_error)=_io_error_string(error._errno)',line)
    call dcl_uproc(parser,'open(&f:filesystem,name:string,key...) {file,error=open(&f,name,key...);'//&
         ' test "Error opening file """++name++""": "++error=>not(error);return file}',line)
    call dcl_uproc(parser,'close(&f:file) '//&
         '{error=close(&f);test "Error closing file:"++error=>not(error)}',line)
    call dcl_uproc(parser,'read(&f:file,&x) '//&
         '{error=read(&f,&x);test "Error reading from file:"++error=>not(error)}',line)
    call dcl_uproc(parser,'write(&f:file,x) '//&
         '{error=write(&f,x);test "Error writing to file:"++error=>not(error)}',line)
    call dcl_uproc(parser,'seek(&f:file,x:lint) '//&
         '{error=seek(&f,x);test "Error on seek:"++error=>not(error)}',line)
    call dcl_uproc(parser,'read%(&f:shared file,&x) '//&
         '{error=read%(&f,&x);test "Error reading from file:"++error=>not(error)}',line)
    call dcl_uproc(parser,'write%(&f:shared file,x) '//&
         '{error=write%(&f,x);test "Error writing to file:"=>not(error)}',line)
    
    ! *************************************************************
    ! SUPPORT PROCEDURES FOR COMMUNICATING OPERATIONS
    ! *************************************************************

    ! SOA tuples
    call dcl_type(parser,'_stuple1d is rec^{t1}',line)
    call dcl_type(parser,'_stuple2d is rec^{t1,t2}',line)
    call dcl_type(parser,'_stuple3d is rec^{t1,t2,t3}',line)
    call dcl_type(parser,'_stuple4d is rec^{t1,t2,t3,t4}',line)
    call dcl_type(parser,'_stuple5d is rec^{t1,t2,t3,t4,t5}',line)
    call dcl_type(parser,'_stuple6d is rec^{t1,t2,t3,t4,t5,t6}',line)
    call dcl_type(parser,'_stuple7d is rec^{t1,t2,t3,t4,t5,t6,t7}',line)

    call dcl_uproc(parser,&
         '_st(t1)=new _stuple1d{t1=t1}',line)
    call dcl_uproc(parser,&
         '_st(t1,t2)=new _stuple2d{t1=t1,t2=t2}',line)
    call dcl_uproc(parser,&
         '_st(t1,t2,t3)=new _stuple3d{t1=t1,t2=t2,t3=t3}',line)
    call dcl_uproc(parser,&
         '_st(t1,t2,t3,t4)=new _stuple4d{t1=t1,t2=t2,t3=t3,t4=t4}',line)
    call dcl_uproc(parser,&
         '_st(t1,t2,t3,t4,t5)=new _stuple5d{t1=t1,t2=t2,t3=t3,t4=t4,t5=t5}',line)
    call dcl_uproc(parser,&
         '_st(t1,t2,t3,t4,t5,t6)=new _stuple6d{t1=t1,t2=t2,t3=t3,t4=t4,t5=t5,t6=t6}',line)
    call dcl_uproc(parser,&
         '_st(t1,t2,t3,t4,t5,t6,t7)=new _stuple7d{t1=t1,t2=t2,t3=t3,t4=t4,t5=t5,t6=t6,t7=t7}',line)    

    ! Create normalised form of a grid used for _xxx_slice operations
    call dcl_uproc(parser,'_norm(n,x:seq or block_seq)=_st(n,low(x),high(x),step(x),width(x),align(x))',line)
    call dcl_uproc(parser,'_norm(n,x:map_seq)=_st(x.array)',line)
    call dcl_uproc(parser,'_norm(n,x:grid)=_st(map_apply($_norm,$_st,n,x),size(x))',line)

    ! Apply idxdim index and convert to normal for for _send_slice_mapped
    call dcl_uproc(parser,'_dnorm(x:indexed_dim(''1),m,n:single_point)='//&
         '_st(m,t,t,1,1,0) where t=_dmap(x,n._t)',line)
    call dcl_uproc(parser,'_dnorm(x:indexed_dim(''1),m,n:range)='//&
         '_st(m,_dmap(x,n._lo),_dmap(x,n._hi),1,1,0)',line)
    call dcl_uproc(parser,'_dnorm(x:indexed_dim(''1),m,n:strided_range)='//&
         '_st(m,_dmap(x,n._lo),_dmap(x,n._hi),x._st*n._m,1,0)',line)
    call dcl_uproc(parser,'_dnorm(x:indexed_dim(''1,''1),m,n:block_seq)='//&
         '_st(m,n._lo+x._c,n._hi+x._c,n._st,n._b,n._align)',line)
    call dcl_uproc(parser,'_dnorm(x:indexed_dim(''1),m,n:block_seq)='//&
         '_dnorm(x,m,map_seq(n))',line)
    call dcl_uproc(parser,'_dnorm(x:indexed_dim(''1),m,n:map_seq){'//&
         'var a=array(0,#n._array);for i in a,j in n._array <<conc>>:i:=_dmap(x,j);return _st(a)}',line)
    call dcl_uproc(parser,'_dnorm(x:indexed_dim(''1),m,n:grid)='//&
         '_st(map_apply($_dnorm,$_st,x,m,n),size(n))',line)

!!$    call dcl_type(parser,'_griddef is rec^{grid,elems,size}',line)
!!$    call dcl_uproc(parser,'_gd(grid,elems,size)=new _griddef{grid=grid,elems=elems,size=size}',line)

    call dcl_type(parser,'_griddef is rec^{grid,elems}',line)
    call dcl_uproc(parser,'_gd(grid,elems,size)=new _griddef{grid=grid,elems=elems}',line)
  
    call dcl_uproc(parser,&
         '_send_slice(p,x:_comp^any,d) { '//&
         'for i in d  <<conc>>{ _isend_offset%(j,p,x) '//&
         'where j=index(#x,i) } }',line)
    call dcl_uproc(parser,&
         '_send_slice(p,x,d) { '//&
         '_isend_offset(_norm(dims(x),d),p,x) }',line)
    call dcl_uproc(parser,&
         '_send_slice_mapped(p,x,d,t,s) { '//&
         'for k in d  <<conc>>{ _isend_offset%(j,p,x) '//&
         'where j=index(dims(s),s#i) where i=_dmap(t,k)}}',line)
    call dcl_uproc(parser,&
         '_send_slice_mapped(p,x:_comp^any,d,t:indexed_dim(''1),s) { '//&
         'for k in d  <<conc>>{ _isend_offset%(j,p,x) '//&
         'where j=index(dims(s),s#i) where i=_dmap(t,k)}}',line)
    call dcl_uproc(parser,&
         '_send_slice_mapped(p,x,d,t:indexed_dim(''1),s) { '//&
         '_isend_offset(_dnorm(t,dims(x),d),p,x)}',line)
    
    call dcl_uproc(parser,&
         '_recv_slice(p,&x:_comp^any,d) { '//&
         'for i in d  <<conc>>{ _irecv_offset%(j,p,&x) '//&
         'where j=index(#x,i) } }',line)
    call dcl_uproc(parser,&
         '_recv_slice(p,&x,d) { '//&
         '_irecv_offset(_norm(dims(x),d),p,&x) }',line)
    call dcl_uproc(parser,&
         '_recv_slice_sync(p,&x:_comp^any,d) { '//&
         'for i in d  <<conc>>{ _recv_offset%(j,p,&x) '//&
         'where j=index(#x,i) } }',line)
    call dcl_uproc(parser,&
         '_recv_slice_sync(p,&x,d) { '//&
         '_recv_offset(_norm(dims(x),d),p,&x) }',line)
    call dcl_uproc(parser,&
         '_recv_slice_resend(p,&x:_comp^any,d) { '//&
         'for i in d  <<conc>>{ _recv_resend%(j,p,&x) '//&
         'where j=index(#x,i) } }',line)
    call dcl_uproc(parser,&
         '_recv_slice_resend(p,&x,d) { '//&
         '_recv_resend(_norm(dims(x),d),p,&x) }',line)
    call dcl_uproc(parser,&
         '_send_recv_slice_req(p,&x:_comp,a,sx,d,c:''true) {'//&
         'for i in d  <<conc>>{ j=index(sx,i);'//&
         '_isend_recv_req%(j,p,^(x),&^(^(a))); '//&
         '} }',line)
    call dcl_uproc(parser,&
         '_send_recv_slice_req(p,x,&a,sx,d,c:''true) {'//&
         '_isend_recv_req(_norm(dims(sx),d),p,^(x),&^(^(a)))}',line)
    call dcl_uproc(parser,&
         '_send_recv_slice_req(p,x,&a,sx,d,c) {'//&
         'for i in d  <<conc>>{ j=index(sx,i);_isend_recv_req%(j,p,^(x),&^(^(a)),c) '//&
         '} }',line)
    call dcl_uproc(parser,&
         '_send_slice_assn(p,x:_comp,y,sx,d,c:''true) {'//&
         'for i in d  <<conc>>{ _isend_assn%(j,^(p),^(x),^(y)) '//&
         'where j=index(sx,i) } }',line)
    call dcl_uproc(parser,&
         '_send_slice_assn(p,x,y,sx,d,c:''true) {'//&
         '_isend_assn(_norm(dims(sx),d),p,x,y)}',line)
    call dcl_uproc(parser,&
         '_send_slice_assn(p,x,y,sx,d,c) {'//&
         'for i in d  <<conc>>{ _isend_assn%(j,p,^(x),^(y),^(c)) '//&
         'where j=index(sx,i) } }',line)
    call dcl_uproc(parser,&
         '_recv_slice_reply(p,&x:_comp,sx,d,c:''true) {'//&
         'for i in d  <<conc>>{ _recv_reply%(j,^(p),&^(^(x))) '//&
         'where j=index(sx,i) } }',line)
    call dcl_uproc(parser,&
         '_recv_slice_reply(p,&x,sx,d,c:''true) {'//&
         '_recv_reply(_norm(dims(sx),d),p,&x) }',line)
    call dcl_uproc(parser,&
         '_recv_slice_reply(p,&x,sx,d,c) {'//&
         'for i in d  <<conc>>{ _recv_reply%(j,p,&^(^(x)),c) '//&
         'where j=index(sx,i) } }',line)
    call dcl_uproc(parser,&
         '_bcast_slice(&x:_comp,sx,d,c:''true) {'//&
         'for i in d  <<conc>>{ _bcast_slice_shared%(_gd(_norm(dims(sx),d),j,size(d)),_head_node(),&^(^(x))) '//&
         'where j=index(sx,i) } }',line)
    call dcl_uproc(parser,&
         '_bcast_slice(&x,sx,d,c:''true) {'//&
         '_bcast_slice_shared(_gd(_norm(dims(sx),d),null,size(d)),_head_node(),&^(^(x)))}',line)
    call dcl_uproc(parser,&
         '_bcast_slice_shared(&x,sx,d,c) {'//&
         'for i in d  <<conc>>{ _bcast_slice_shared%(j,_head_node(),&^(^(x)),c) '//&
         'where j=index(sx,i) } }',line)
        
    call dcl_proc(parser,'_isend_offset%(r:any,s:any,h:any,j:any,p:any,x:any)',&
         op_isend_offset,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_isend_offset(j:any,p:any,x:any)',&
         op_isend_grid,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_irecv_offset%(r:any,s:any,h:any,j:any,p:any,&x:any)',&
         op_irecv_offset,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_irecv_offset(j:any,p:any,&x:any)',&
         op_irecv_grid,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_recv_offset%(r:any,s:any,h:any,j:any,p:any,&x:any)',&
         op_recv_offset,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_recv_offset(j:any,p:any,&x:any)',&
         op_recv_grid,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_recv_resend%(r:any,s:any,h:any,j:any,p:any,&x:any)',&
         op_recv_offset_resend,1,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_recv_resend(j:any,p:any,&x:any)',&
         op_recv_grid_resend,1,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_isend(p:any,x:any)',&
         op_isend,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_irecv(p:any,&x:any)',&
         op_irecv,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_recv(p:any,&x:any)',&
         op_recv,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_isend_recv_req%(r:any,s:any,h:any,j:any,p:any,x:any,&a:any)',&
         op_isend_req,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_isend_recv_req(j:any,p:any,x:any,&a:any)',&
         op_isend_req,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_isend_recv_req%(r:any,s:any,h:any,j:any,p:any,x:any,&a:any,c:any)',&
         op_isend_req,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_isend_assn%(r:any,s:any,h:any,j:any,p:any,x:any,y:any)',&
         op_isend_assn,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_isend_assn(j:any,p:any,x:any,y:any)',&
         op_isend_assn,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_isend_assn%(r:any,s:any,h:any,j:any,p:any,x:any,y:any,c:any)',&
         op_isend_assn,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_recv_reply%(r:any,s:any,h:any,j:any,p:any,&x:any,c:any)',&
         op_recv_reply,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_recv_reply%(r:any,s:any,h:any,j:any,p:any,&x:any)',&
         op_recv_reply,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_recv_reply(j:any,p:any,&x:any)',&
         op_recv_reply,0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_bcast_slice_shared%(r:any,s:any,h:any,j:any,p:any,&x:any,c:any)',&
         op_broadcast_disp,1,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_bcast_slice_shared%(r:any,s:any,h:any,j:any,p:any,&x:any)',&
         op_broadcast_disp,1,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_bcast_slice_shared(j:any,p:any,&x:any)',&
         op_broadcast_disp,1,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'_bcast_shared(&x:any)',op_broadcast_shared,0,line,proc_is_impure)
    call dcl_type(parser,'_ct is array_slice,^*(,,,,),any^any,^^(any)',line)

    call dcl_uproc(parser,'PM__sync_messages(x)<<inline>>:_sync_messages(x)',line)
    if(pm_is_compiling) then
       call dcl_uproc(parser,'_sync_messages(x:_ct):_do_sync_messages(_core(x))',line)
       call dcl_uproc(parser,'_sync_messages(x:_ct,y:_ct):_do_sync_messages(_core(x),_core(y))',line)
       call dcl_uproc(parser,'_core(x:any^any)=x',line)
       call dcl_uproc(parser,'_core(x:^^(any))=x',line)
       call dcl_uproc(parser,'_core(x:array_slice)=_core(x._a)',line)
       call dcl_uproc(parser,'_core(x:^*(,,,,))=_core(_v2(x))',line)
       call dcl_proc(parser,&
            '_do_sync_messages(arg...:^^(any) or any^any)',&
            op_sync_mess,0,line,proc_is_impure+proc_is_dcomm)
    else
       call dcl_proc(parser,&
            '_sync_messages(arg...:_ct)',&
            op_sync_mess,0,line,proc_is_impure+proc_is_dcomm)
    endif
    call dcl_uproc(parser,'_tup(x:tuple)=x',line)
    call dcl_uproc(parser,'_tup(arg...)=tuple(arg...)',line)
    call dcl_uproc(parser,'_tup(x:null)=x',line)
    
    call dcl_proc(parser,'PM__broadcast(&b:any,a:int)',op_broadcast,&
         0,line,proc_is_impure+proc_is_dcomm)
    call dcl_proc(parser,'PM__broadcast(b:any,a:int)->=b',op_broadcast_val,&
         0,line,proc_is_impure+proc_is_dcomm)

    call dcl_proc(parser,&
         'get_remote%(r:any,s:any,h:any,a:shared any^dshape,'//&
         'b:int,c:int)->%a',&
         op_get_remote_distr,&
         0,line,proc_is_impure+proc_is_dcomm)
    
    call dcl_proc(parser,&
         'put_remote%(r:any,s:any,h:any,a:shared any^dshape,'//&
         'b:any,c:int,d:int)',&
         op_put_remote_distr,&
         0,line,proc_is_impure+proc_is_dcomm)
    
    ! ********************************************************
    ! OTHER COMMUNICATING & ARRAY OPERATIONS
    ! ********************************************************

    call dcl_uproc(parser,'map(p:proc,x:any^any) {var z=array(_arb(x),#x);for i in z, j in x:i:=p.(j)}',line)
    call dcl_uproc(parser,'map(p:proc,x:any^any,y:any^any) '//&
         '{var z=array(p.(_arb(x),_arb(y)),#x);for i in z,j in x,k in y:i=p.(j,k)}',line)
    call dcl_uproc(parser,'map(p:proc,x:any^mshape,y:any^dshape) '//&
         '{var z=array(p.(_arb(x),_arb(y)),#(y));for i in z,j in x,k in y:i:=p.(j,k)}',line)
    call dcl_uproc(parser,'map_const(p:proc,x:any^mshape,y:any)'//&
         '{var z=array(p.(_arb(x),y),#x);for i in z,j in x:i:=p.(j,y)}',line)

    call dcl_uproc(parser,'+(x:num^any,y:num^any)=map($+,x,y)',line)
    call dcl_uproc(parser,'-(x:num^any,y:num^any)=map($-,x,y)',line)
    call dcl_uproc(parser,'*(x:num^any,y:num^any)=map($*,x,y)',line)
    call dcl_uproc(parser,'/(x:num^any,y:num^any)=map($/,x,y)',line)
    call dcl_uproc(parser,'**(x:num^any,y:num^any)=map($**,x,y)',line)
    call dcl_uproc(parser,'mod(x:real_num^any,y:real_num^any)=map($mod,x,y)',line)
    call dcl_uproc(parser,'max(x:real_num^any,y:real_num^any)=map($max,x,y)',line)
    call dcl_uproc(parser,'min(x:real_num^any,y:real_num^any)=map($min,x,y)',line)
    call dcl_uproc(parser,'+(x:num^any,y:any)=map_const($+,x,y)',line)
    call dcl_uproc(parser,'-(x:num^any,y:any)=map_const($-,x,y)',line)
    call dcl_uproc(parser,'*(x:num^any,y:any)=map_const($*,x,y)',line)
    call dcl_uproc(parser,'/(x:num^any,y:any)=map_const($/,x,y)',line)
    call dcl_uproc(parser,'**(x:num^any,y:any)=map_const($**,x,y)',line)
    call dcl_uproc(parser,'mod(x:real_num^any,y:real_num)=map_const($mod,x,y)',line)
    call dcl_uproc(parser,'max(x:real_num^any,y:real_num)=map_const($max,x,y)',line)
    call dcl_uproc(parser,'min(x:real_num^any,y:real_num)=map_const($min,x,y)',line)
    
    call dcl_proc(parser,'_pack(v:any,any,any,d:any)->PM__dim v,d',&
         op_pack,0,line,0)
    call dcl_uproc(parser,'pack(v:any^mshape,m:bool^mshape) { '//&
         ' test "arrays do not conform"=>conform(#v,#m); '//&
         ' result =_pack(v,m,n,tuple(0..n-1)) where n=count(m) }',line)
    call dcl_uproc(parser,'pack(vv:array,mm:array) {'//&
         ' var v=vv;var m=mm; '//&
         ' return _pack(v,m,n,tuple(0..n-1))'//&
         ' where n=count(m) }',line)


    
    ! Reduction
    call dcl_type(parser,'associative_proc is $+,$*,$max,$min,'//&
         '$&,$|,$xor,$++,$==,...',line)

    if(pm_is_compiling) then
       call dcl_uproc(parser,'reduce(p:proc,x:array(,mshape)) {'//&
            'var s=_get_aelem(x,0);'//&
            'foreach i in 1..size(#x)-1 {'//&
            's:=p.(s,_get_aelem(x,i))'//&
            '};return s}',line)
    else
       call dcl_uproc(parser,'reduce(p:proc,x:array(,mshape)) {'//&
            'var y=x;var n=size(x);'//&
            'while n>1 {'//&
            ' var m=(n+1)/2;'//&
            ' for k in m..n-1  <<conc>>{'//&
            '   PM__setaelem(&y,k-m,p.(_get_aelem(y,k-m),_get_aelem(y,k)) <<PM__ignore>>)};'//&
            ' n:=m};return _get_aelem(y,0)}',line)
    endif
    
    call dcl_uproc(parser,'reduce(p:proc,y:array)='//&
         '_reduce(p,reduce(p,PM__local(y)))',line)

    call dcl_uproc(parser,'_reduce_for_assign%(p:invar associative_proc,y,init:invar){'//&
         'chan yy=y;return reduce%(p,yy,init)}',line)
    call dcl_uproc(parser,'_reduce_for_assign%(p:invar $-,y,init:invar){'//&
         'chan yy=y;return init - _reduce%($+,yy,init)}',line)
    call dcl_uproc(parser,'_reduce_for_assign%(p:invar $/,y,init:invar){'//&
         'chan yy=y;return init / _reduce%($*,yy,init)}',line)
    
    call dcl_uproc(parser,'reduce%(p:invar proc,y:chan,init)='//&
         '^(p.(init,_reduce(p,reduce(p,PM__local(y@) <<PM__node,PM__ignore>>)<<PM__node,PM__ignore>>)),uniform)',line)
    call dcl_uproc(parser,'_reduce%(p:invar proc,y:chan)='//&
         '^(_reduce(p,reduce(p,PM__local(y@) <<PM__node,PM__ignore>>)<<PM__node,PM__ignore>>),uniform)',line)

    call dcl_uproc(parser,'_reduce(p:proc,y) {'//&
         'var x=array(y,[0..0]);var z=array(y,[0..0]);'//&
         'var n=this_nnode();var i=1;'//&
         'do {'//&
         ' other=_this_node() xor i;'//&
         ' if other<n {_isend(other,x);_recv(other,&z);'//&
         '  _sync_messages(x);x[0]:=p.(x[0],z[0])};'//&
         ' i:=i*2'//&
         '} until i>n-1;return x[0]}',line)


    ! **************************************************
    ! SUPPORT FOR OTHER LANGUAGE FEATURES
    ! **************************************************

    ! Keyword arguments
    call dcl_uproc(parser,'PM__getkey(x:any,y:any)=convert(x,y)',line)
    call dcl_uproc(parser,'PM__getkey(x:null,y:any)=y',line)

    ! Select statement
    call dcl_uproc(parser,&
         'PM__checkcase(x,y,arg...) { var e=match_switch_case(x,y); '//&
         'if not e { e:=PM__checkcase(x,arg...) };return e }',line)
    call dcl_uproc(parser,'PM__checkcase(x,y)=match_switch_case(x,y)',line)
    call dcl_uproc(parser,'match_switch_case(x,y)=x==y',line)
    call dcl_uproc(parser,&
         'match_switch_case(x:real_num,y:range(real_num))=x>=y._lo and x<=y._hi',&
         line)
    call dcl_uproc(parser,'match_switch_case(x:<any>,y:<any>)=y inc x',line)

    ! Conditional operators
    call dcl_uproc(parser,&
         'PM__if(x,y,z) check "Incompatible types in different ""if""branches"=> '//&
         'same_type(y,z) { var r=z; if x { r:=y };return r }',&
         line)
    call dcl_uproc(parser,'PM__if(x:''true,y,z)=y',line)
    call dcl_uproc(parser,'PM__if(x:''false,y,z)=z',line)
    call dcl_uproc(parser,'PM__if(x,y,arg...)=PM__if(x,y,PM__if(arg...))',line)
    call dcl_uproc(parser,&
         'PM__switch(w,x,y,z) check "Incompatible types in different ""switch"" branches"=> '//&
         'same_type(y,z) { var r=z; if match(w,x) { r:=y };return r }',&
         line)
    call dcl_uproc(parser,'PM__switch(w,x,y,arg...)=PM__switch(w,x,y,PM__switch(w,arg...))',line)
 
    ! Assignment
    call dcl_uproc(parser,'PM__assign_var(&a,b) {PM__assign(&a,b)}',line)
    call dcl_uproc(parser,&
         'PM__assign(&a:any,b:any) {check_assign_types(a,b);_assign(&a,b)}',line)
    call dcl_type(parser,'assignment_operator is $_just_assign,$+,$*,$&,$|,$xor,$and,$or,$++,...',line)
    call dcl_uproc(parser,&
         'PM__assign(&a:any,b:any,c:assignment_operator) { PM__assign(&a,c.(a,b)) }',line)
    call dcl_uproc(parser,&
         'PM__assign(&a:any,b:any,c:proc) { test "Not a recognised assignment operator"=>''false }',line)
    call dcl_uproc(parser,'check_assign_types(x,y)'//&
         '{test "Type mismatch in assignment"=>same_type(x,y)}',line)
    call dcl_uproc(parser,'_assign(&a,b) {_assign_element(&a,b)}',line)
    call dcl_uproc(parser,'_assign(&a:contains(farray),b) {_assign_structure(&a,b)}',line)
    call dcl_uproc(parser,'_assign_structure(&a,b)<<foreach^(a,b)>>{_assign_element(&a,b)}',line)
    call dcl_uproc(parser,'_assign_structure(&a:farray,b){_array_assign(&a,b,''true)}',line)
    call dcl_proc(parser,'_assign_element(&any,any)',op_assign,0,line,0)

    ! Other variable operations
    call dcl_proc(parser,'PM__clone(x:any)->=x',op_clone,0,line,0)
    call dcl_uproc(parser,'PM__dup(PM__dup) <<foreach^(PM__dup)>>=PM__clone(PM__dup)',line)
    call dcl_proc(parser,'PM__getref(x:any)->=x',op_get_rf,0,line,0)
    call dcl_proc(parser,'same_type(x:any,y:any)->==x,y',&
         op_logical_return,0,line,proc_needs_type)
    call dcl_uproc(parser,'==(x:any,y:any) {'//&
         'test "Cannot apply ""=="" to different types"=> same_type(x,y);'//&
         'var ok=true;_eq(x,y,&ok);return ok}',line)
    call dcl_uproc(parser,&
         '_eq(x:any,y:any,&ok) <<foreach^(x,y)>> { ok:=ok and x==y }',line)
    call dcl_proc(parser,'PM__copy_out(x:any)->=x',op_clone,0,line,0)
    call dcl_proc(parser,'PM__copy_back(x:any)->=x',op_assign,0,line,0)
    
    call dcl_uproc(parser,'next_enum(x:int)=x+convert(1,x)',line)
    call dcl_uproc(parser,'next_enum(x:int,y:int)=x+convert(y,x)',line)

    ! Type values
    call dcl_proc(parser,'typeof(x:any)->type x',op_make_type_val,0,line,proc_needs_type)
    call dcl_uproc(parser,'is(x,t)=t inc typeof(x)',line)
    call dcl_uproc(parser,'as(x,t:<any>)...=PM__cast(x,t)',line)
    call dcl_uproc(parser,'as(x,t)=PM__cast(x,typeof(t))',line)
    call dcl_proc(parser,'inc(x:<any>,y:<any>)-> inc x,y',op_logical_return,0,line,proc_needs_type)
    call dcl_uproc(parser,'==(x:<any>,y:<any>)=x inc y and y inc x',line)
    call dcl_proc(parser,'error_type()->?int',0,0,line,proc_needs_type)
    
    ! Debugging
    call dcl_proc(parser,'_dump(any,any)',op_new_dump,0,line,proc_is_impure)
    call dcl_uproc(parser,'PM__dump(x)<<no_inline>>:_dump("Value:",x)',line)
    call dcl_uproc(parser,'PM__dump(y,x)<<no_inline>>:if y:_dump("Value:",x)',line)
    call dcl_uproc(parser,'PM__dump%(x)<<no_inline>>{print("$"++here);_dump(string(here),x)}',line)
    call dcl_uproc(parser,'PM__dump%(y:bool,x)<<no_inline>>{if y:_dump(string(here),x)}',line)
    call dcl_uproc(parser,'PM__dump%(y,x){'//&
         'test "Selection expression in ''$$dump'' not ''bool''" => ''false;$$infer_type(y)}',line)

    call dcl_proc(parser,'old_dump(any)',op_dump,0,line,proc_is_impure)
    call dcl_uproc(parser,'old_dumpit(a) { old_dump(a);return a }',line)
    call dcl_proc(parser,'old_dump_id(any)',op_dump_id,0,line,proc_is_impure)

  end subroutine sysdefs

end module pm_sysdefs
