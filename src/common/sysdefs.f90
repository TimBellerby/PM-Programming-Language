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

module pm_sysdefs
  use pm_kinds
  use pm_memory
  use pm_parser
  use pm_types
  use pm_vmdefs
  implicit none

  integer,parameter:: pm_max_stack=127
  integer,parameter:: proc_is_pass = 1
  integer,parameter:: proc_global_return = 2
  integer,parameter:: proc_is_generator = 3
  integer,parameter:: proc_needs_type = 4

  integer,parameter:: ftn_simple=1
  integer,parameter:: ftn_dim_array=2
  integer,parameter:: ftn_extract_array_elem=3
  integer,parameter:: ftn_make_array=4
  integer,parameter:: ftn_subref=5
  integer,parameter:: ftn_get_elem=6
  integer,parameter:: ftn_set_elem=7
  integer,parameter:: ftn_miss_arg=8
  integer,parameter:: ftn_import_val=9
  integer,parameter:: ftn_import_varg=10
  integer,parameter:: ftn_import_scalar=11
  integer,parameter:: ftn_export=12
  integer,parameter:: ftn_extract=13
  integer,parameter:: ftn_extract_first=14
  integer,parameter:: ftn_broadcast=15
  integer,parameter:: ftn_broadcast_val=16
  integer,parameter:: ftn_get_remote=17
  integer,parameter:: ftn_put_remote=18
  integer,parameter:: ftn_makekeys=19
  integer,parameter:: ftn_delkeys=20
  integer,parameter:: ftn_checkkeys=21
  integer,parameter:: ftn_getvkey=22
  integer,parameter:: ftn_assign=23
  integer,parameter:: ftn_dup=24
  integer,parameter:: ftn_getref=25
  integer,parameter:: ftn_same_type=26
  integer,parameter:: ftn_has_same_type=27
  integer,parameter:: ftn_eq=28
  integer,parameter:: ftn_ne=29
  integer,parameter:: ftn_array_dom=30
  integer,parameter:: ftn_export_array=31
  integer,parameter:: ftn_iota=32
  integer,parameter:: ftn_indices=33
  integer,parameter:: ftn_redim=34
  integer,parameter:: ftn_make_mask=35
  integer,parameter:: ftn_get_remote_distr=36
  integer,parameter:: ftn_put_remote_distr=37
  
contains

  subroutine sysdefs(parser)
    type(parse_state):: parser
    integer:: line
    line=1

    call dcl_module(parser,'PM__system')
    parser%sysmodl=parser%modl
 
    ! String type
    call dcl_proc(parser,'print(string)',op_print,0_pm_i16,line,0,&
         'IF PRC_FRAME(PRC_DEPTH)%SHARED_PRC==0 THEN WRITE(*,*) $s1',ftn_simple)
    call dcl_uproc(parser,'print(x) do print(string(x)) endproc',line)
    call dcl_proc(parser,'//(string,string)->string',op_concat,0_pm_i16,line,0,&
         '$1=PM_CONCAT_STRING(PM_CONTEXT,$2,$3)',ftn_simple)
    call dcl_uproc(parser,'//(x:string,y)=proc{//}(x,string(y))',line)
    call dcl_uproc(parser,'//(x,y)=proc{//}(string(x),string(y))',line)
    call dcl_uproc(parser,'string(x:string)=x',line)
    call dcl_uproc(parser,'string(x:null)="null"',line)

    ! Int type
    call dcl_proc(parser,'PM__assign_var(&int,int)',&
         op_assign_i,0_pm_i16,line,0,&
         '$1=$2',ftn_simple)
    call dcl_proc(parser,'mod(int,int)->int',op_mod_i,0_pm_i16,line,0,&
         '$1=MOD($2,$3)',ftn_simple)
    call dcl_proc(parser,'==(int,int)->bool',op_eq_i,0_pm_i16,line,0,&
         '$1=$2==$3',ftn_simple)
    call dcl_proc(parser,'/=(int,int)->bool',op_ne_i,0_pm_i16,line,0,&
         '$1=$2/=$3',ftn_simple)
    call dcl_proc(parser,'>=(int,int)->bool',op_ge_i,0_pm_i16,line,0,&
         '$1=$2>=$3',ftn_simple)
    call dcl_proc(parser,'>(int,int)->bool',op_gt_i,0_pm_i16,line,0,&
         '$1=$2>$3',ftn_simple)
    call dcl_proc(parser,'+(int,int)->int',op_add_i,0_pm_i16,line,0,&
         '$1=$2+$3',ftn_simple)
    call dcl_proc(parser,'-(int,int)->int',op_sub_i,0_pm_i16,line,0,&
         '$1=$2-$3',ftn_simple)
    call dcl_proc(parser,'*(int,int)->int',op_mult_i,0_pm_i16,line,0,&
         '$1=$2*$3',ftn_simple)
    call dcl_proc(parser,'/(int,int)->int',op_divide_i,0_pm_i16,line,0,&
         '$1=$2/$3',ftn_simple)
    call dcl_proc(parser,'**(int,int)->int',op_pow_i,0_pm_i16,line,0,&
         '$1=$2**$3',ftn_simple)
    call dcl_proc(parser,'max(int,int)->int',op_max_i,0_pm_i16,line,0,&
         '$1=MAX($2,$3)',ftn_simple)
    call dcl_proc(parser,'min(int,int)->int',op_min_i,0_pm_i16,line,0,&
         '$1=MIN($2,$3)',ftn_simple)
    call dcl_proc(parser,'-(int)->int',op_uminus_i,0_pm_i16,line,0,&
         '$1=-$2',ftn_simple)
    call dcl_proc(parser,'string(int)->string',op_string_i,0_pm_i16,line,0,&
         '$1=VECTOR_MAKE_STRING(PM_CONTEXT,PM_NULL_OBJ,$p2,FMT_I_WIDTH,FMT_I)',&
         ftn_simple)
    call dcl_proc(parser,'long(int)->long',op_long_i,0_pm_i16,line,0,&
         '$1=INT($2,KIND=PM_LN)',ftn_simple)
    call dcl_proc(parser,'real(int)->real',op_real_i,0_pm_i16,line,0,&
         '$1=REAL($2)',ftn_simple)
    call dcl_proc(parser,'double(int)->double',op_double_i,0_pm_i16,line,0,&
         '$1=DOUBLE($2)',ftn_simple)
    call dcl_uproc(parser,'int(x:int)=x',line)
    call dcl_proc(parser,'abs(int)->int',op_abs_i,0_pm_i16,line,0,&
         '$1=ABS($2)',ftn_simple)
    call dcl_proc(parser,'bit_not(int)->int',op_bnot_i,0_pm_i16,line,0,&
         '$1=NOT($2)',ftn_simple)
    call dcl_proc(parser,'bit_and(int,int)->int',op_band_i,0_pm_i16,line,0,&
         '$1=IAND($2,$3)',ftn_simple)
    call dcl_proc(parser,'bit_or(int,int)->int',op_bor_i,0_pm_i16,line,0,&
         '$1=IOR($2,$3)',ftn_simple)
    call dcl_proc(parser,'bit_xor(int,int)->int',op_bxor_i,0_pm_i16,line,0,&
         '$1=IEOR($2,$3)',ftn_simple)
    call dcl_proc(parser,'bit_shift(int,int)->int',op_bshift_i,0_pm_i16,line,0,&
         '$1=ISHFT($2,$3)',ftn_simple)
    
    ! Long type
    call dcl_proc(parser,'PM__assign_var(&long,long)',&
         op_assign_ln,0_pm_i16,line,0,'$1=$2',ftn_simple)
    call dcl_proc(parser,'mod(long,long)->long',op_mod_ln,0_pm_i16,line,0,&
         '$1=MOD($2,$3)',ftn_simple)
    call dcl_proc(parser,'==(long,long)->bool',op_eq_ln,0_pm_i16,line,0,&
         '$1=$2==$2',ftn_simple)
    call dcl_proc(parser,'/=(long,long)->bool',op_ne_ln,0_pm_i16,line,0,&
         '$1=$2/=$3',ftn_simple)
    call dcl_proc(parser,'>=(long,long)->bool',op_ge_ln,0_pm_i16,line,0,&
         '$1=$2>=$3',ftn_simple)
    call dcl_proc(parser,'>(long,long)->bool',op_gt_ln,0_pm_i16,line,0,&
         '$1=$2>$3',ftn_simple)
    call dcl_proc(parser,'+(long,long)->long',op_add_ln,0_pm_i16,line,0,&
         '$1=$2+$3',ftn_simple)
    call dcl_proc(parser,'-(long,long)->long',op_sub_ln,0_pm_i16,line,0,&
         '$1=$2-$3',ftn_simple)
    call dcl_proc(parser,'*(long,long)->long',op_mult_ln,0_pm_i16,line,0,&
         '$1=$2*$3',ftn_simple)
    call dcl_proc(parser,'/(long,long)->long',op_divide_ln,0_pm_i16,line,0,&
         '$1=$2/$3',ftn_simple)
    call dcl_proc(parser,'**(long,long)->long',op_pow_ln,0_pm_i16,line,0,&
         '$1=$2**$3',ftn_simple)
    call dcl_proc(parser,'max(long,long)->long',op_max_ln,0_pm_i16,line,0,&
         '$1=MAX($2,$3)',ftn_simple)
    call dcl_proc(parser,'min(long,long)->long',op_min_ln,0_pm_i16,line,0,&
         '$1=MIN($2,$3)',ftn_simple)
    call dcl_proc(parser,'-(long)->long',op_uminus_ln,0_pm_i16,line,0,&
         '$1=-$2',ftn_simple)
    call dcl_proc(parser,'string(long)->string',op_string_ln,0_pm_i16,line,0,&
         '$1=VECTOR_MAKE_STRING(PM_CONTEXT,PM_NULL_OBJ,$p1,'//&
         'FMT_LN_SIZE,FMT_LN)',&
         ftn_simple)
    call dcl_proc(parser,'int(long)->int',op_int_ln,0_pm_i16,line,0,&
         '$1=INT($2)',ftn_simple)
    call dcl_proc(parser,'real(long)->real',op_real_ln,0_pm_i16,line,0,&
         '$1=REAL($2)',ftn_simple)
    call dcl_proc(parser,'double(long)->double',op_double_ln,0_pm_i16,line,0,&
         '$1=DOUBLE($2)',ftn_simple)
    call dcl_uproc(parser,'long(x:long)=x',line)
    call dcl_proc(parser,'abs(long)->long',op_abs_ln,0_pm_i16,line,0,&
         '$1=ABS($2)',ftn_simple)
    call dcl_proc(parser,'bit_not(long)->long',op_bnot_ln,0_pm_i16,line,0,&
         '$1=NOT($2)',ftn_simple)
    call dcl_proc(parser,'bit_and(long,long)->long',op_band_ln,0_pm_i16,line,0,&
         '$1=IAND($2,$3)',ftn_simple)
    call dcl_proc(parser,'bit_or(long,long)->long',op_bor_ln,0_pm_i16,line,0,&
         '$1=IOR($2,$3)',ftn_simple)
    call dcl_proc(parser,'bit_xor(long,long)->long',op_bxor_ln,0_pm_i16,line,0,&
         '$1=IEOR($2,$3)',ftn_simple)
    call dcl_proc(parser,'bit_shift(long,long)->long',&
         op_bshift_ln,0_pm_i16,line,0,&
         '$1=ISHFT($2,$3)',ftn_simple)

    ! Real type
    call dcl_proc(parser,'PM__assign_var(&real,real)',&
         op_assign_r,0_pm_i16,line,0,'$1=$2',ftn_simple)
    call dcl_proc(parser,'mod(real,real)->real',op_mod_r,0_pm_i16,line,0,&
         '$1=MOD($2,$3)',ftn_simple)
    call dcl_proc(parser,'==(real,real)->bool',op_eq_r,0_pm_i16,line,0,&
         '$1=$2==$3',ftn_simple)
    call dcl_proc(parser,'/=(real,real)->bool',op_ne_r,0_pm_i16,line,0,&
         '$1=$2/=$3',ftn_simple)
    call dcl_proc(parser,'>=(real,real)->bool',op_ge_r,0_pm_i16,line,0,&
         '$1=$2>=$3',ftn_simple)
    call dcl_proc(parser,'>(real,real)->bool',op_gt_r,0_pm_i16,line,0,&
         '$1=$2>$3',ftn_simple)
    call dcl_proc(parser,'+(real,real)->real',op_add_r,0_pm_i16,line,0,&
         '$1=$2+$3',ftn_simple)
    call dcl_proc(parser,'-(real,real)->real',op_sub_r,0_pm_i16,line,0,&
         '$1=$2-$3',ftn_simple)
    call dcl_proc(parser,'*(real,real)->real',op_mult_r,0_pm_i16,line,0,&
         '$1=$2*$3',ftn_simple)
    call dcl_proc(parser,'/(real,real)->real',op_divide_r,0_pm_i16,line,0,&
         '$1=$2/$3',ftn_simple)
    call dcl_proc(parser,'**(real,real)->real',op_pow_r,0_pm_i16,line,0,&
         '$1=$2**$3',ftn_simple)
    call dcl_proc(parser,'max(real,real)->real',op_max_r,0_pm_i16,line,0,&
         '$1=MAX($2,$3)',ftn_simple)
    call dcl_proc(parser,'min(real,real)->real',op_min_r,0_pm_i16,line,0,&
         '$1=MIN($2,$3)',ftn_simple)
    call dcl_proc(parser,'-(real)->real',op_uminus_r,0_pm_i16,line,0,&
         '$1=-$2',ftn_simple)
    call dcl_proc(parser,'string(real)->string',op_string_r,0_pm_i16,line,0,&
         '$1=VECTOR_MAKE_STRING(PM_CONTEXT,'//&
         'PM_NULL_OBJ,$p2,FMT_R_LENGTH,FMT_R)',&
         ftn_simple)
    call dcl_proc(parser,'int(real)->int',op_int_r,0_pm_i16,line,0,&
         '$1=INT($2)',ftn_simple)
    call dcl_proc(parser,'long(real)->long',op_long_r,0_pm_i16,line,0,&
         '$1=REAL($2)',ftn_simple)
    call dcl_proc(parser,'double(real)->double',op_double_r,0_pm_i16,line,0,&
         '$1=DOUBLE($2)',ftn_simple)
    call dcl_uproc(parser,'real(x:real)=x',line)
    call dcl_proc(parser,'abs(real)->real',op_abs_r,0_pm_i16,line,0,&
         '$1=ABS($2)',ftn_simple)
    call dcl_proc(parser,'acos(real)->real',op_acos_r,0_pm_i16,line,0,&
         '$1=ACOS($2)',ftn_simple)
    call dcl_proc(parser,'asin(real)->real',op_asin_r,0_pm_i16,line,0,&
         '$1=ASIN($2)',ftn_simple)
    call dcl_proc(parser,'atan(real)->real',op_atan_r,0_pm_i16,line,0,&
         '$1=ATAN($2)',ftn_simple)
    call dcl_proc(parser,'atan2(real,real)->real',op_atan2_r,0_pm_i16,line,0,&
         '$1=ATAN2($2,$3)',ftn_simple)
    call dcl_proc(parser,'cos(real)->real',op_cos_r,0_pm_i16,line,0,&
         '$1=COS($2)',ftn_simple)
    call dcl_proc(parser,'cosh(real)->real',op_cosh_r,0_pm_i16,line,0,&
         '$1=ABS($2)',ftn_simple)
    call dcl_proc(parser,'exp(real)->real',op_exp_r,0_pm_i16,line,0,&
         '$1=EXP($2)',ftn_simple)
    call dcl_proc(parser,'ln(real)->real',op_log_r,0_pm_i16,line,0,&
         '$1=LOG($2)',ftn_simple)
    call dcl_proc(parser,'log10(real)->real',op_log10_r,0_pm_i16,line,0,&
         '$1=LOG10($2)',ftn_simple)
    call dcl_proc(parser,'sin(real)->real',op_sin_r,0_pm_i16,line,0,&
         '$1=SIN($2)',ftn_simple)
    call dcl_proc(parser,'sinh(real)->real',op_sinh_r,0_pm_i16,line,0,&
         '$1=SINH($2)',ftn_simple)
    call dcl_proc(parser,'sqrt(real)->real',op_sqrt_r,0_pm_i16,line,0,&
         '$1=SQRT($2)',ftn_simple)
    call dcl_proc(parser,'tan(real)->real',op_tan_r,0_pm_i16,line,0,&
         '$1=TAN($2)',ftn_simple)
    call dcl_proc(parser,'tanh(real)->real',op_tanh_r,0_pm_i16,line,0,&
         '$1=TANH($2)',ftn_simple)
    call dcl_proc(parser,'floor(real)->real',op_floor_r,0_pm_i16,line,0,&
         '$1=FLOOR($2)',ftn_simple)
    call dcl_proc(parser,'ceil(real)->real',op_ceil_r,0_pm_i16,line,0,&
         '$1=CEILING($2)',ftn_simple)

    ! Double type
    call dcl_proc(parser,'PM__assign_var(&double,double)',&
         op_assign_d,0_pm_i16,line,0,'$1=$2',ftn_simple)
    call dcl_proc(parser,'mod(double,double)->double',op_mod_d,0_pm_i16,line,0,&
         '$1=MOD($2,$3)',ftn_simple)
    call dcl_proc(parser,'==(double,double)->bool',op_eq_d,0_pm_i16,line,0,&
         '$1=$2==$3',ftn_simple)
    call dcl_proc(parser,'/=(double,double)->bool',op_ne_d,0_pm_i16,line,0,&
         '$1=$2/=$3',ftn_simple)
    call dcl_proc(parser,'>=(double,double)->bool',op_ge_d,0_pm_i16,line,0,&
         '$1=$2>=$3',ftn_simple)
    call dcl_proc(parser,'>(double,double)->bool',op_gt_d,0_pm_i16,line,0,&
         '$1=$2>$3',ftn_simple)
    call dcl_proc(parser,'+(double,double)->double',op_add_d,0_pm_i16,line,0,&
         '$1=$2+$3',ftn_simple)
    call dcl_proc(parser,'-(double,double)->double',op_sub_d,0_pm_i16,line,0,&
         '$1=$2-$3',ftn_simple)
    call dcl_proc(parser,'*(double,double)->double',op_mult_d,0_pm_i16,line,0,&
         '$1=$2*$3',ftn_simple)
    call dcl_proc(parser,'/(double,double)->double',op_divide_d,0_pm_i16,line,0,&
         '$1=$2/$3',ftn_simple)
    call dcl_proc(parser,'**(double,double)->double',op_pow_d,0_pm_i16,line,0,&
         '$1=$2**$3',ftn_simple)
    call dcl_proc(parser,'max(double,double)->double',op_max_d,0_pm_i16,line,0,&
         '$1=MAX($2,$3)',ftn_simple)
    call dcl_proc(parser,'min(double,double)->double',op_min_d,0_pm_i16,line,0,&
         '$1=MIN($2,$3)',ftn_simple)
    call dcl_proc(parser,'-(double)->double',op_uminus_d,0_pm_i16,line,0,&
         '$1=-$2',ftn_simple)
    call dcl_proc(parser,'string(double)->string',op_string_d,0_pm_i16,line,0,&
         '$1=VECTOR_MAKE_STRING(PM_CONTEXT,PM_NULL_OBJ,$p1,'//&
         'FMT_D_LENGTH,FMT_D)',&
         ftn_simple)
    call dcl_proc(parser,'int(double)->int',op_int_d,0_pm_i16,line,0,&
         '$1=INT($2)',ftn_simple)
    call dcl_proc(parser,'long(double)->long',op_long_d,0_pm_i16,line,0,&
         '$1=LONG($2)',ftn_simple)
    call dcl_proc(parser,'real(double)->real',op_real_d,0_pm_i16,line,0,&
         '$1=REAL($2)',ftn_simple)
    call dcl_uproc(parser,'double(x:double)=x',line)
    call dcl_proc(parser,'abs(double)->double',op_abs_d,0_pm_i16,line,0,&
         '$1=ABS($2)',ftn_simple)
    call dcl_proc(parser,'acos(double)->double',op_acos_d,0_pm_i16,line,0,&
         '$1=ACOS($2)',ftn_simple)
    call dcl_proc(parser,'asin(double)->double',op_asin_d,0_pm_i16,line,0,&
         '$1=ASIN($2)',ftn_simple)
    call dcl_proc(parser,'atan(double)->double',op_atan_d,0_pm_i16,line,0,&
         '$1=ATAN($2)',ftn_simple)
    call dcl_proc(parser,'atan2(double,double)->double',&
         op_atan2_d,0_pm_i16,line,0,&
         '$1=ATAN2($2,$3)',ftn_simple)
    call dcl_proc(parser,'cos(double)->double',op_cos_d,0_pm_i16,line,0,&
         '$1=COS($2)',ftn_simple)
    call dcl_proc(parser,'cosh(double)->double',op_cosh_d,0_pm_i16,line,0,&
         '$1=ABS($2)',ftn_simple)
    call dcl_proc(parser,'exp(double)->double',op_exp_d,0_pm_i16,line,0,&
         '$1=EXP($2)',ftn_simple)
    call dcl_proc(parser,'ln(double)->double',op_log_d,0_pm_i16,line,0,&
         '$1=LOG($2)',ftn_simple)
    call dcl_proc(parser,'log10(double)->double',op_log10_d,0_pm_i16,line,0,&
         '$1=LOG10($2)',ftn_simple)
    call dcl_proc(parser,'sin(double)->double',op_sin_d,0_pm_i16,line,0,&
         '$1=SIN($2)',ftn_simple)
    call dcl_proc(parser,'sinh(double)->double',op_sinh_d,0_pm_i16,line,0,&
         '$1=SINH($2)',ftn_simple)
    call dcl_proc(parser,'sqrt(double)->double',op_sqrt_d,0_pm_i16,line,0,&
         '$1=SQRT($2)',ftn_simple)
    call dcl_proc(parser,'tan(double)->double',op_tan_d,0_pm_i16,line,0,&
         '$1=TAN($2)',ftn_simple)
    call dcl_proc(parser,'tanh(double)->double',op_tanh_d,0_pm_i16,line,0,&
         '$1=TANH($2)',ftn_simple)
    call dcl_proc(parser,'floor(double)->double',op_floor_d,0_pm_i16,line,0,&
         '$1=FLOOR($2)',ftn_simple)
    call dcl_proc(parser,'ceil(double)->double',op_ceil_d,0_pm_i16,line,0,&
         '$1=CEILING($2)',ftn_simple)

    ! Bool type
    call dcl_proc(parser,'PM__assign_var(&bool,bool)',&
         op_assign_l,0_pm_i16,line,0,'$1=$2',ftn_simple)
    call dcl_proc(parser,'string(bool)->string',op_string_l,0_pm_i16,line,0,&
         '$1=VECTOR_MAKE_STRING(PM_CONTEXT,PM_NULL_OBJ,$p1,FMT_L_LENGTH,FMT_L)',&
         ftn_simple)
    call dcl_proc(parser,'and(bool,bool)->bool',op_and,0_pm_i16,line,0,&
         '$1=$2 .AND. $3',ftn_simple)
    call dcl_proc(parser,'or(bool,bool)->bool',op_or,0_pm_i16,line,0,&
         '$1=$2 .OR. $3',ftn_simple)
    call dcl_proc(parser,'not(bool)->bool',op_not,0_pm_i16,line,0,&
         '$1=.NOT. $2',ftn_simple)

    ! Debugging
    call dcl_proc(parser,'dump(any)',op_dump,0_pm_i16,line,0,&
         'PM_DUMP_TREE(PM_CONTEXT,PM_STDOUT,$p1,2)',ftn_simple)
    call dcl_uproc(parser,'dumpit(a)=a do dump(a) endproc',line)
    call dcl_proc(parser,'dump_id(any)',op_dump_id,0_pm_i16,line,0,&
         'WRITE(*,*) $p1%offset',ftn_simple)
    
    ! Abstract numeric types
    call dcl_type(parser,'std_int is int,long',line)
    call dcl_type(parser,'any_int is std_int,int8,int16,int32,int64,int128',&
         line)
    call dcl_type(parser,'std_real is real,double',line)
    call dcl_type(parser,'any_real is std_real,real32,real64,real128',line)
    call dcl_type(parser,'std_cpx is cpx,double_cpx',line)
    call dcl_type(parser,'any_cpx is std_cpx, cpx64, cpx128, cpx256',line)
    call dcl_type(parser,'int_num includes any_int',line)
    call dcl_type(parser,'real_num includes int_num, any_real',line)
    call dcl_type(parser,'cpx_num includes real_num,any_cpx',line)
    call dcl_type(parser,'num includes cpx_num',line)

    ! Numeric type conversion
    call dcl_uproc(parser,'convert(x,y)=x',line)
    call dcl_uproc(parser,'convert(x:real_num,y:int)=int(x)',line)
    call dcl_uproc(parser,'convert(x:real_num,y:long)=long(x)',line)
    call dcl_uproc(parser,'convert(x:real_num,y:real)=real(x)',line)
    call dcl_uproc(parser,'convert(x:real_num,y:double)=double(x)',line)

    ! Mixed arithmatic
    call dcl_type(parser,'_to_long is int',line)
    call dcl_type(parser,'_to_real is int, long',line)
    call dcl_type(parser,'_to_double is int,long,real',line)
    call dcl_uproc(parser,'balance(x:int,y:int)=x,y',line)
    call dcl_uproc(parser,'balance(x:long,y:long)=x,y',line)
    call dcl_uproc(parser,'balance(x:real,y:real)=x,y',line)
    call dcl_uproc(parser,'balance(x:double,y:double)=x,y',line)
    call dcl_uproc(parser,'balance(x:long,y:_to_long)=x,long(y)',line)
    call dcl_uproc(parser,'balance(x:real,y:_to_real)=x,real(y)',line)
    call dcl_uproc(parser,'balance(x:double,y:_to_double)=x,double(y)',line)
    call dcl_uproc(parser,'balance(x:_to_long,y:long)=long(x),y',line)
    call dcl_uproc(parser,'balance(x:_to_real,y:real)=real(x),y',line)
    call dcl_uproc(parser,'balance(x:_to_double,y:double)=double(x),y',line)

    call dcl_uproc(parser,&
         'mod(x:num,y:num)=xx mod yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '==(x:num,y:num)=xx==yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '/=(x:num,y:num)=xx/=yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '>=(x:num,y:num)=xx>=yy where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,&
         '>(x:num,y:num)=xx>yy where xx,yy=balance(x,y)',line)
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
    
    ! Optional types
    call dcl_type(parser,'opt{x} is '//&
         'struct _opt{_val:x,_there:bool}',line)
    call dcl_uproc(parser,':/(x:struct _opt{_val,_there},y) do z:=y;'//&
         'if x._there then z=x._val endif; result=z endproc ',line)
    call dcl_uproc(parser,&
         ':/(x:struct _opt{_val,_there}#any,y)=z '//&
         'do for i in x conc '//&
         'return z::=gather::w where w=i:/y endfor endproc',line)
    call dcl_uproc(parser,&
         'opt(val,there:bool)=struct _opt{_val=val,_there=there}',&
         line)
    call dcl_uproc(parser,'opt(x)=struct _opt{_val=x,_there=true}',line)
    call dcl_uproc(parser,&
         'null(x)=struct _opt{_val=x,_there=false}',line)
    call dcl_uproc(parser,'val(x:struct _opt{_val,_there})=x._val '//&
         'check "optional value is undefined":x._there',line)
    
    ! Polymorphic types
    call dcl_proc(parser,'get(&x:any,y:<>)',op_get_poly,0_pm_i16,line,&
         proc_needs_type,&
         ' ',ftn_simple)
    call dcl_proc(parser,'get(&x:any,y:<>)->bool',op_get_poly2,0_pm_i16,line,&
         proc_needs_type,&
         ' ',ftn_simple)
    call dcl_proc(parser,'|(x:<>,y:any)->=y',op_get_poly_or,0_pm_i16,line,&
         proc_needs_type,&
         ' ',ftn_simple)
    
    ! Tuple types
    call dcl_type(parser,&
         'tuple{t1} is rec _tuple1d{d1:t1}',line)
    call dcl_type(parser,&
         'tuple{t1,t2} is rec _tuple2d{d1:t1,d2:t2}',line)
    call dcl_type(parser,&
         'tuple{t1,t2,t3} is'//&
         ' rec _tuple3d{d1:t1,d2:t2,d3:t3}',line)
    call dcl_type(parser,&
         'tuple{t1,t2,t3,t4} is'//&
         ' rec _tuple4d{d1:t1,d2:t2,d3:t3,d4:t4}',line)
    call dcl_type(parser,&
         'tuple{t1,t2,t3,t4,t5} is'//&
         ' rec _tuple5d{d1:t1,d2:t2,d3:t3,d4:t4,d5:t5}',line)
    call dcl_type(parser,&
         'tuple{t1,t2,t3,t4,t5,t6} is'//&
         ' rec _tuple6d{d1:t1,d2:t2,d3:t3,d4:t4,d5:t5,d6:t6}',line)
    call dcl_type(parser,&
         'tuple{t1,t2,t3,t4,t5,t6,t7} is'//&
         ' rec _tuple7d{d1:t1,d2:t2,d3:t3,d4:t4,d5:t5,d6:t6,d7:t7}',line)

    call dcl_type(parser,'tuple1d is tuple{}',line)
    call dcl_type(parser,'tuple2d is tuple{,}',line)
    call dcl_type(parser,'tuple3d is tuple{,,}',line)
    call dcl_type(parser,'tuple4d is tuple{,,,}',line)
    call dcl_type(parser,'tuple5d is tuple{,,,,}',line)
    call dcl_type(parser,'tuple6d is tuple{,,,,,}',line)
    call dcl_type(parser,'tuple7d is tuple{,,,,,,}',line)
    call dcl_type(parser,'tuple is '//&
         'tuple1d,tuple2d,tuple3d,tuple4d,tuple5d,tuple6d,tuple7d',line)
    
    call dcl_uproc(parser,'tuple(x)=rec _tuple1d{d1=x}',line)
    call dcl_uproc(parser,'tuple(x,y)='//&
         'rec _tuple2d{d1=x,d2=y}',line)
    call dcl_uproc(parser,'tuple(x,y,z)='//&
         'rec _tuple3d{d1=x,d2=y,d3=z}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t)='//&
         'rec _tuple4d{d1=x,d2=y,d3=z,d4=t}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u)='//&
         'rec _tuple5d{d1=x,d2=y,d3=z,d4=t,d5=u}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u,v)='//&
         'rec _tuple6d{d1=x,d2=y,d3=z,d4=t,d5=u,d6=v}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u,v,w)='//&
         'rec _tuple7d{d1=x,d2=y,d3=z,d4=t,d5=u,d6=v,d7=w}',line)


    ! Size of shape defined by tuple
    call dcl_uproc(parser,'size(t:tuple{long})=t.d1',line)
    call dcl_uproc(parser,'size(t:tuple{long,long})=t.d1*t.d2',line)
    call dcl_uproc(parser,'size(t:tuple{long,long,long})=t.d1*t.d2*t.d3',line)
    call dcl_uproc(parser,&
         'size(t:tuple{long,long,long,long})=t.d1*t.d2*t.d3*t.d4',line)
    call dcl_uproc(parser,&
         'size(t:tuple{long,long,long,long,long})='//&
         't.d1*t.d2*t.d3*t.d4*t.d5',line)
    call dcl_uproc(parser,&
         'size(t:tuple{long,long,long,long,long,long})='//&
         't.d1*t.d2*t.d3*t.d4*t.d5*t.d6',line)
    call dcl_uproc(parser,&
         'size(t:tuple{long,long,long,long,long,long,long})='//&
         't.d1*t.d2*t.d3*t.d4*t.d5*t.d6*t.d7',line)   

    
    ! Generic sequence types
    call dcl_type(parser,'bd_seq is bd_seq{}',line)
    call dcl_type(parser,'any_seq is any_seq{}',line)
    call dcl_type(parser,'bd_seq{t} is range{t},seq{t}',line)
    call dcl_type(parser,'any_seq{t} is cycle{t},bd_seq{t}',line)
    
    call dcl_type(parser,'range_base includes real_num',line)
    
    ! Range types
    call dcl_type(parser,'range is range{}',line)
    call dcl_type(parser,'range{t:range_base} is rec _range{_lo:t,_hi:t}',line)
    call dcl_uproc(parser,'..(x:range_base,y:range_base)='//&
         ' rec _range{_lo=xx,_hi=yy} where xx,yy=balance(x,y)',line)
    call dcl_uproc(parser,'is_cyclic(x:range{})=false',line)
    call dcl_uproc(parser,'low(x:range{})=x._lo',line)
    call dcl_uproc(parser,'high(x:range{})=x._hi',line)
    call dcl_uproc(parser,'step(x:range{any_int})=convert(1,x._lo)',line)
    call dcl_uproc(parser,'dom(x:range{any_int})=grid(0l..size(x)-1l)',line)
    call dcl_uproc(parser,'shape(x:range{any_int})=tuple(size(x))',line)
    call dcl_uproc(parser,&
         'size(x:range{any_int})=max(long(x._hi-x._lo),-1l)+1l',line)
    call dcl_uproc(parser,&
         'in(x:range_base,y:range{})= x>=y._lo and x<=y._hi',line)
    call dcl_uproc(parser,'convert(x:range{},y:range_base)='//&
         'rec _range{_lo=convert(x._lo,y),_hi=convert(x._hi,y)}',line)
    call dcl_uproc(parser,&
         'long(x:range{})=rec _range{_lo=long(x._lo),_hi=long(x._hi)}',line)
    call dcl_uproc(parser,&
         'includes(x:range{},y:bd_seq{}) do '//&
         ' result= y._lo>=x._lo and y._hi<=x._hi endproc',line)
    call dcl_uproc(parser,&
         'index(y:range{any_int},x:any_int)=long(x-y._lo)',line)
    call dcl_uproc(parser,&
         'loc_in_shape(y:range{any_int},x:any_int)=long(x-y._lo)',line)
    call dcl_uproc(parser,&
         'PM__first(x:range{any_int})=x._lo,null,x._lo<=x._hi',line)
    call dcl_uproc(parser,&
         'PM__next(x:range{any_int},y,z)=zz,null,zz<=x._hi'//&
         ' where zz=z+convert(1,x._lo)',line)
    call dcl_uproc(parser,'PM__get_elem(x:range{any_int},y:any_int)='//&
         'x._lo+convert(y,x._lo) ',line)
    call dcl_uproc(parser,'PM__get_elem(x:range{any_int},y:range{any_int})= '//&
         ' convert(y._lo,x._lo)+x._lo..convert(y._hi,x._lo)+x._lo',line)
    call dcl_uproc(parser,'PM__get_elem(x:range{any_int},y:seq{any_int})= '//&
         ' convert(PM__first(y),x._lo)+x._lo..'//&
         'convert(last(y),x._lo)+x._lo by y._st',line)
    call dcl_uproc(parser,&
         'PM__get_elem(x:range{any_int},y:range_below{any_int})= '//&
         ' x._lo..convert(y._t,x._lo)+x._lo',line)
    call dcl_uproc(parser,&
         'PM__get_elem(x:range{any_int},y:range_above{any_int})= '//&
         ' convert(y._lo,x._lo)+x._lo..x._hi',line)
    call dcl_uproc(parser,'PM__get_elem(x:range{any_int},'//&
         ' y:strided_range_below{any_int})= '//&
         ' x._lo..convert(y._t,x._lo)+x._lo by y._s',line)
    call dcl_uproc(parser,'PM__get_elem(x:range{any_int},'//&
         ' y:strided_range_above{any_int})= '//&
         ' convert(y._lo,x._lo)+x._lo..x._hi by y._s',line)
    call dcl_uproc(parser,'PM__get_elem(x:range{any_int},y:stride{any_int})= '//&
         ' x by y._s ',line)
    call dcl_uproc(parser,'PM__get_elem(x:range{any_int},y:null)=x ',line)
    call dcl_uproc(parser,'overlap(x:range{},y:range{})='//&
         'rec _range{_lo=max(x._lo,y._lo),_hi=min(x._hi,y._hi)}',line)
    call dcl_uproc(parser,'expand(x:range{},y:range{})='//&
         'rec _range{_lo=x._lo+y._lo,_hi=x._hi+y._hi}',line)
    call dcl_uproc(parser,'expand(x:range{},y:seq{})='//&
         'rec _range{_lo=x._lo+y._lo,_hi=x._hi+y._hi}',line)
    call dcl_uproc(parser,'contract(x:range{},y:range{})='//&
         'rec _range{_lo=x._lo-y._lo,_hi=x._hi-y._hi}',line)
    call dcl_uproc(parser,'contract(x:range{},y:seq{})='//&
         'rec _range{_lo=x._lo-y._lo,_hi=x._hi-y._hi}',line)
    
    ! Sequence types
    call dcl_type(parser,'seq is seq{}',line)
    call dcl_type(parser,&
         'seq{t:range_base} is rec _seq{_lo:t,_hi:t,_st:t,_f:t,_n}',line)
    call dcl_uproc(parser,'by(x:range{},y:range_base) do'//&
         ' m:=(x._hi-x._lo)/y;'//&
         ' lo:=convert(x._lo,m);hi:=convert(x._hi,m);st:=convert(y,m);'//&
         ' result=rec _seq{ _lo=min(lo,hi),_hi=max(lo,hi),_st=st,'//&
         ' _f=lo,_n=max(0l,long(m)+1l)}'//&
         'endproc',line)
    call dcl_uproc(parser,'by(x:seq{},y:range_base)='//&
         ' rec _seq{_lo=x._lo,_hi=x._hi,_f=x._f,_st=x._st*y,_n=x._n/y}',line)
    call dcl_uproc(parser,'is_cyclic(x:seq{})=false',line)
    call dcl_uproc(parser,'low(x:seq{})=x._lo',line)
    call dcl_uproc(parser,'high(x:seq{})=x._hi',line)
    call dcl_uproc(parser,'step(x:seq{})=x._st',line)
    call dcl_uproc(parser,'size(x:seq{})=x._n',line)
    call dcl_uproc(parser,'PM__first(x:seq{})=x._f',line)
    call dcl_uproc(parser,'last(x:seq{})=x._f+x._n*x._st-x._st',line)
    call dcl_uproc(parser,'initial(x:seq{})=x.st>0=>x._lo||x._hi',line)
    call dcl_uproc(parser,'final(x:seq{})=x.st<0=>x._lo||x._hi',line)
    call dcl_uproc(parser,'dom(x:seq{})=grid(0l..x._n-1l)',line)
    call dcl_uproc(parser,'shape(x:seq{})=tuple(x._n)',line)
    call dcl_uproc(parser,'convert(x:seq{},y:range_base)='//&
         'rec _seq{_lo=convert(x._lo,y),_hi=convert(x._hi,y),'//&
         '_f=convert(x._f,y),_st=convert(x._st,y),_n=x._n}',line)
    call dcl_uproc(parser,'long(x:seq{})='//&
         'rec _seq{_lo=long(x._lo),_hi=long(x._hi),_f='//&
         'long(x._f),_st=long(x._st),_n=x._n}',line)
    call dcl_uproc(parser,&
         'in(x:any_int,y:seq{any_int})='//&
         'y._lo<=x and x<=y._hi and (x-y._f) mod y._st==0',line)
    call dcl_uproc(parser,&
         'includes(x:seq{any_int},y:seq{any_int})='//&
         'y._lo in x and y._hi in x and (y._n==1 or y._f+y._st in x)',line)
    call dcl_uproc(parser,&
         'index(y:seq{long},x:any_int)=(long(x)-y._f)/y._st',line)
    call dcl_uproc(parser,&
         'loc_in_shape(y:seq{long},x:any_int)=(long(x)-y._f)/y._st',line)
    call dcl_uproc(parser,'PM__first(x:seq{})=x._f,0l,x._n>0l',line)
    call dcl_uproc(parser,&
         'PM__next(x:seq{},y,z)=z+x._st,yy,yy<x._n where yy=y+1l',line)
    call dcl_uproc(parser,'PM__get_elem(x:seq{},y:any_int)= '//&
         'x._f+x._st*convert(y,x._lo)',line)
    call dcl_uproc(parser,'PM__get_elem(x:seq{},y:range{any_int}) do'//&
         ' first:=PM__get_elem(x,y._lo);n:=size(y);'//&
         ' last:=first+convert(n,x._lo)*x._st;'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         '_f=first,_st=x._st,_n=n} '//&
         'endproc',line)
    call dcl_uproc(parser,'PM__get_elem(x:seq{},y:seq{any_int}) do'//&
         ' first:=PM__get_elem(x,y._lo);st:=x._st*convert(y._st,x._st);'//&
         ' n:=size(y);last:=first+convert(n,x._lo)*st;'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         ' _f=first,_st=st,_n=n} '//&
         'endproc',line)
    call dcl_uproc(parser,'PM__get_elem(x:seq{},y:range_below{any_int}) do'//&
         ' first:=x._f;last:=PM__get_elem(x,y._t);n:=long(y._t)+1l'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         ' _f=first,_st=x._st,_n=n} '//&
         'endproc',line)
    call dcl_uproc(parser,'PM__get_elem(x:seq{},y:range_above{any_int}) do'//&
         ' first:=PM__get_elem(x,y._t);last:=x._f+x._n*x._st;n:=x._n-long(y._t)'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         ' _f=first,_st=x._st,_n=n} '//&
         'endproc',line)
    call dcl_uproc(parser,&
         'PM__get_elem(x:seq{},y:strided_range_below{any_int}) do'//&
         ' first:=x._f;last:=PM__get_elem(x,y._t);n:=long(y._t)+1l'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         ' _f=first,_st=x._st*y._s,_n=n/y._s} '//&
         'endproc',line)
    call dcl_uproc(parser,&
         'PM__get_elem(x:seq{},y:strided_range_above{any_int}) do'//&
         ' first:=PM__get_elem(x,y._t);last:=x._f+x._n*x._st;n:=x._n-long(y._t)'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         ' _f=first,_st=x._st*y._s,_n=n/y._s} '//&
         'endproc',line)
    call dcl_uproc(parser,'PM__get_elem(x:seq{},y:stride{any_int}) do'//&
         ' result=rec _seq{_lo=x._lo,_hi=x._hi,'//&
         ' _f=x._f,_st=x._st*y._t,_n=x._n/y._t} '//&
         'endproc',line)
    call dcl_uproc(parser,'PM__get_elem(x:seq,y:null)=x',line)
    call dcl_uproc(parser,'expand(x:seq{},y:range{})='//&
         'rec _range{_lo=x._lo+y._lo,_hi=x._hi+y._hi}',line)
    call dcl_uproc(parser,'expand(x:seq{},y:seq{})='//&
         'rec _range{_lo=x._lo+y._lo,_hi=x._hi+y._hi}',line)
    call dcl_uproc(parser,'contract(x:seq{},y:range{})='//&
         'rec _range{_lo=x._lo-y._lo,_hi=x._hi-y._hi}',line)
    call dcl_uproc(parser,'contract(x:seq{},y:seq{})='//&
         'rec _range{_lo=x._lo-y._lo,_hi=x._hi-y._hi}',line)
    call dcl_uproc(parser,'overlap(x:seq{},y:range{})='//&
         'get_slice(x,lo..hi) where lo=max(x._lo,y._lo),hi=min(x._hi,y._hi)',&
         line)
    call dcl_uproc(parser,'overlap(x:range{},y:seq{})='//&
         'get_slice(y,lo..hi) where lo=max(x._lo,y._lo),hi=min(x._hi,y._hi)',&
         line)
    
    ! Cyclic types
    call dcl_type(parser,'cycle is _cycle{}',line)
    call dcl_type(parser,'cycle{t} is _cycle{t}',line)
    call dcl_type(parser,'_cycle{t} includes rec _cycle{_x:t}',line)
    call dcl_type(parser,'_cycle{t:range_base} also '//&
         'includes rec _cycle{_x:bd_seq{t}}',line) 
    call dcl_uproc(parser,'cycle(x:range{})=rec _cycle{_x=x}',line)
    call dcl_uproc(parser,'cycle(x:seq{})=rec _cycle{_x=x}',line)
    call dcl_uproc(parser,'is_cyclic(x:cycle{})=true',line)
    call dcl_uproc(parser,'size(x:cycle)=size(x._x)',line)
    call dcl_uproc(parser,'dom(x:cycle{})=grid(cycle(0..size(x)-1l))',line)
    call dcl_uproc(parser,'shape(x:cycle{})=tuple(size(x))',line)
    call dcl_uproc(parser,'convert(x:cycle,y:range_base)='//&
         'rec _cycle{_x=convert(x._x,y)}',line)
    call dcl_uproc(parser,'long(x:cycle)=rec _cycle{_x=long(x._x)}',line)
    call dcl_uproc(parser,'in(x:range_base,y:cycle)=true',line)
    call dcl_uproc(parser,'includes(x:cycle,y:cycle)=x._x includes y._x',line)
    call dcl_uproc(parser,'includes(x:cycle,y)=x._x includes y',line)
    call dcl_uproc(parser,'low(x:cycle)=low(x._x)',line)
    call dcl_uproc(parser,'high(x:cycle)=high(x._x)',line)
    call dcl_uproc(parser,'PM__first(x:cycle)=PM__first(x._x)',line)
    call dcl_uproc(parser,'last(x:cycle)=last(x._x)',line)
    call dcl_uproc(parser,'initial(x:cycle)=initial(x._x)',line)
    call dcl_uproc(parser,'final(x:cycle)=final(x._x)',line)
    call dcl_uproc(parser,'step(x:cycle)=step(x._x)',line)
    call dcl_uproc(parser,'PM__get_elem(x:cycle,y:index)=PM__get_elem(x._x,y)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(x:cycle,y:subs)=rec _cycle{_x=PM__get_elem(x._x,y)}',line)
    call dcl_uproc(parser,'overlap(x:cycle,y:bd_seq)=y',line)
    call dcl_uproc(parser,'overlap(x:bd_seq,y:cycle)=x',line)
    call dcl_uproc(parser,'overlap(x:cycle,y:cycle)=null',line)
    call dcl_uproc(parser,&
         'index(x:cycle{long},y:any_int)=index(x._x,y) mod size(x._x)',&
         line)
    call dcl_uproc(parser,'loc_in_shape(x:cycle{long},y:any_int)='//&
         'index(x._x,y) mod size(x._x)',line)
    call dcl_uproc(parser,&
         'PM__first(x:cycle)=u,v,w where u,v,w=PM__first(x._x)',line)
    call dcl_uproc(parser,&
         'PM__next(x:cycle,y,z)=u,v,w where u,v,w=PM__next(x._x,y,z)',line) 
    
    ! Vector type
    call dcl_type(parser,'vector is rec _vect{_d}',line)
    call dcl_uproc(parser,&
         'vector(rows:any_seq{std_int})=rec _vect{_d=rows} '//&
         'where lrows=long(rows)',line)
    call dcl_uproc(parser,'size(x:vector)=size(x._d)',line)
    call dcl_uproc(parser,'dom(x:vector)=x',line)
    call dcl_uproc(parser,'in(x:any_int,y:vector)=x in y._d',line)
    
    call dcl_uproc(parser,'includes(x:vector,y:vector)=x._d includes y._d',line)
    call dcl_uproc(parser,&
         'PM__first(x:vector)=u,v,w where u,v,w=PM__first(x._d)',line)
    call dcl_uproc(parser,&
         'PM__next(x:vector,y,z)=u,v,w where u,v,w=PM__next(x._d,y,z)',line)
    call dcl_uproc(parser,'PM__get_elem(x:vector,y)=PM__get_elem(x._d,y)',line)
    call dcl_uproc(parser,'overlap(x:vector,y:vector)='//&
         'rec _vect{_d=d} where d=overlap(x._d,y._d)',line)
    call dcl_uproc(parser,'expand(x:vector,y:vector)='//&
         'rec _vect{_d=d} where d=expand(x._d,y._d)',line)
    call dcl_uproc(parser,'expand(x:vector,y:grid1d)='//&
         'rec _vect{_d=d} where d=expand(x._d,y.d1)',line)
    call dcl_uproc(parser,'contract(x:vector,y:vector)='//&
         'rec _vect{_d=d} where d=contract(x._d,y._d)',line)
    call dcl_uproc(parser,'contract(x:vector,y:grid1d)='//&
         'rec _vect{_d=d} where d=contract(x._d,y.d1)',line)
    
    ! Matrix type
    call dcl_type(parser,'matrix is rec _mat{_d}',line)
    call dcl_uproc(parser,&
        'matrix(rows,cols)=rec _mat{_d=grid(rows,cols)}',line)
    call dcl_uproc(parser,'size(x:matrix)=x._d._n',line)
    call dcl_uproc(parser,'dom(x:matrix)=x',line)
    call dcl_uproc(parser,'in(x:matrix,y:matrix)=x._d in y._d',line)
    call dcl_uproc(parser,'includes(x:matrix,y:matrix)=x._d includes y._d',line)
    call dcl_uproc(parser,&
         'PM__first(x:matrix)=u,v,w where u,v,w=PM__first(x._d)',line)
    call dcl_uproc(parser,&
         'PM__next(x:matrix,y,z)=u,v,w where u,v,w=PM__next(x._d,y,z)',line)
    call dcl_uproc(parser,'PM__get_elem(x:matrix,y)=PM__get_elem(x._d,y)',line)
    call dcl_uproc(parser,'overlap(x:matrix,y:matrix)='//&
        'rec _mat{_d=d} where d=overlap(x._d,y._d)',line)
    call dcl_uproc(parser,'expand(x:matrix,y:matrix)='//&
        'rec _mat{_d=d} where d=expand(x._d,y._d)',line)
    call dcl_uproc(parser,'expand(x:matrix,y:grid2d)='//&
        'rec _mat{_d=d} where d=expand(x._d,y)',line)
    call dcl_uproc(parser,'contract(x:matrix,y:matrix)='//&
        'rec _mat{_d=d} where d=expand(x._d,y._d)',line)
    call dcl_uproc(parser,'contract(x:matrix,y:grid2d)='//&
        'rec _mat{_d=d} where d=expand(x._d,y)',line)

    ! Grid type
    call dcl_type(parser,'grid_base is any_seq{long}',line)
    call dcl_type(parser,'grid{t1:grid_base} is rec _grid1d{_n,d1:t1}',line)
    call dcl_type(parser,'grid{t1:grid_base,t2:grid_base} is'//&
         ' rec _grid2d{_n,d1:t1,d2:t2}',line)
    call dcl_type(parser,&
         'grid{t1:grid_base,t2:grid_base,t3:grid_base} is'//&
         ' rec _grid3d{_n,d1:t1,d2:t2,d3:t3}',line)
    call dcl_type(parser,&
         'grid{t1:grid_base,t2:grid_base,t3:grid_base,t4:grid_base} is'//&
         ' rec _grid4d{_n,d1:t1,d2:t2,d3:t3,d4:t4}',line)
    call dcl_type(parser,&
         'grid{t1:grid_base,t2:grid_base,t3:grid_base,t4:grid_base,'//&
         't5:grid_base}'//&
         'is rec _grid5d{_n,d1:t1,d2:t2,d3:t3,d4:t4,d5:t5}',line)
    call dcl_type(parser,&
         'grid{t1:grid_base,t2:grid_base,t3:grid_base,t4:grid_base,'//&
         't5:grid_base,t6:grid_base} is'//&
         ' rec _grid6d{_n,d1:t1,d2:t2,d3:t3,d4:t4,d5:t5,d6:t6}',line)
    call dcl_type(parser,&
        'grid{t1:grid_base,t2:grid_base,t3:grid_base,t4:grid_base,'//&
        't5:grid_base,'//&
        't6:grid_base,t7:grid_base} is'//&
        ' rec _grid7d{_n,d1:t1,d2:t2,d3:t3,'//&
        'd4:t4,d5:t5,d6:t6,d7:t7}',line)
   
    call dcl_type(parser,'grid1d is grid{}',line)
    call dcl_type(parser,'grid2d is grid{,}',line)
    call dcl_type(parser,'grid3d is grid{,,}',line)
    call dcl_type(parser,'grid4d is grid{,,,}',line)
    call dcl_type(parser,'grid5d is grid{,,,,}',line)
    call dcl_type(parser,'grid6d is grid{,,,,,}',line)
    call dcl_type(parser,'grid7d is grid{,,,,,,}',line)
    call dcl_type(parser,&
         'grid is grid1d,grid2d,grid3d,grid4d,grid5d,grid6d,grid7d',line)
    
   call dcl_type(parser,'_garg is any_seq{any_int}',line)
   call dcl_uproc(parser,'grid(d1:_garg)=rec _grid1d{d1=t1,_n=nn}'//&
        ' where nn=size(d1) '//&
        ' where t1=long(d1)',line)
   call dcl_uproc(parser,'grid(d1:_garg,d2:_garg)='//&
        'rec _grid2d{d1=t1,d2=t2,_n=nn}'//&
        ' where nn=size(d1)*size(d2) '//&
        ' where t1=long(d1),t2=long(d2)',line)
   call dcl_uproc(parser,&
        'grid(d1:_garg,d2:_garg,d3:_garg)='//&
        ' rec _grid3d{d1=t1,d2=t2,d3=t3,_n=nn}'//&
        ' where nn=size(d1)*size(d2)*size(d3) where'//&
        ' t1=long(d1),t2=long(d2),t3=long(d3)',line)
   call dcl_uproc(parser,&
        'grid(d1:_garg,d2:_garg,d3:_garg,d4:_garg)='//&
        ' rec _grid4d{d1=t1,d2=t2,d3=t3,d4=t4,_n=nn}'//&
        ' where nn=size(d1)*size(d2)*size(d3)*size(d4)'//&
        ' where t1=long(d1),t2=long(d2),t3=long(d3),t4=long(d4)',line)
   call dcl_uproc(parser,&
        'grid(d1:_garg,d2:_garg,d3:_garg,d4:_garg,d5:_garg)='//&
        ' rec _grid5d{d1=t1,d2=t2,d3=t3,d4=t4,d5=t5,_n=nn}'//&
        ' where nn=size(d1)*size(d2)*size(d3)*size(d4)*size(d5)'//&
        ' where t1=long(d1),t2=long(d2),t3=long(d3),t4=long(d4),t5=long(d5)',&
        line)
   call dcl_uproc(parser,&
        'grid(d1:_garg,d2:_garg,d3:_garg,d4:_garg,d5:_garg,d6:_garg)='//&
        ' rec _grid6d{d1=t1,d2=t2,d3=t3,d4=t4,d5=t5,d6=t6,_n=nn}'//&
        ' where nn=size(d1)*size(d2)*size(d3)*size(d4)*size(d5)*size(d6)'//&
        ' where t1=long(d1),t2=long(d2),t3=long(d3),'//&
        ' t4=long(d4),t5=long(d5),t6=long(d6)',line)
   call dcl_uproc(parser,&
        'grid(d1:_garg,d2:_garg,d3:_garg,d4:_garg,d5:_garg,'//&
        ' d6:_garg,d7:_garg)='//&
        ' rec _grid7d{d1=t1,d2=t2,d3=t3,d4=t4,d5=t5,d6=t6,d7=t7,_n=nn}'//&
        ' where nn=size(d1)*size(d2)*size(d3)*size(d4)'//&
        '  *size(d5)*size(d6)*size(d7)'//&
        ' where t1=long(d1),t2=long(d2),t3=long(d3),t4=long(d4),'//&
        ' t5=long(d5),t6=long(d6),t7=long(d7)',line)
   
   call dcl_uproc(parser,'size(x:grid)=x._n',line)

   ! Subscript a grid
   call dcl_uproc(parser,'PM__get_elem(x:grid1d,y:any_int)='//&
        'PM__get_elem(x.d1,long(y))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid1d,y:tuple{any_int})=tuple('//&
        'PM__get_elem(x.d1,y.d1))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid2d,y:tuple{any_int,any_int})=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2))',line)
   call dcl_uproc(parser,&
        'PM__get_elem(x:grid3d,y:tuple{any_int,any_int,any_int})=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid4d,'//&
        'y:tuple{any_int,any_int,any_int,any_int})=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid5d,'//&
        'y:tuple{any_int,any_int,any_int,any_int,any_int})=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4),'//&
        'PM__get_elem(x.d5,y.d5))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid6d,'//&
        'y:tuple{any_int,any_int,any_int,any_int,any_int,any_int})=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4),'//&
        'PM__get_elem(x.d5,y.d5),PM__get_elem(x.d6,y.d6))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid7d,'//&
        'y:tuple{any_int,any_int,any_int,any_int,'//&
        'any_int,any_int,any_int})=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4),'//&
        'PM__get_elem(x.d5,y.d5),PM__get_elem(x.d6,y.d6),'//&
        'PM__get_elem(x.d7,y.d7))',line)

   call dcl_uproc(parser,'_gelem(x,y:any_int)=z..z where z=PM__get_elem(x,y)',line)
   call dcl_uproc(parser,'_gelem(x,y)=PM__get_elem(x,y)',line)
   
   call dcl_uproc(parser,'PM__get_elem(x:grid1d,y:tuple1d)=grid('//&
        '_gelem(x.d1,y.d1))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid2d,y:tuple2d)=grid('//&
        '_gelem(x.d1,y.d1),_gelem(x.d2,y.d2))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid3d,y:tuple3d)=grid('//&
        '_gelem(x.d1,y.d1),_gelem(x.d2,y.d2),'//&
        '_gelem(x.d3,y.d3))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid4d,y:tuple4d)=grid('//&
        '_gelem(x.d1,y.d1),_gelem(x.d2,y.d2),'//&
        '_gelem(x.d3,y.d3),_gelem(x.d4,y.d4))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid5d,y:tuple5d)=grid('//&
        '_gelem(x.d1,y.d1),_gelem(x.d2,y.d2),'//&
        '_gelem(x.d3,y.d3),_gelem(x.d4,y.d4),'//&
        '_gelem(x.d5,y.d5))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid6d,y:tuple6d)=grid('//&
        '_gelem(x.d1,y.d1),_gelem(x.d2,y.d2),'//&
        '_gelem(x.d3,y.d3),_gelem(x.d4,y.d4),'//&
        '_gelem(x.d5,y.d5),_gelem(x.d6,y.d6))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid7d,y:tuple7d)=grid('//&
        '_gelem(x.d1,y.d1),_gelem(x.d2,y.d2),'//&
        '_gelem(x.d3,y.d3),_gelem(x.d4,y.d4),'//&
        '_gelem(x.d5,y.d5),_gelem(x.d6,y.d6),'//&
        '_gelem(x.d7,y.d7))',line)
   
   call dcl_uproc(parser,'PM__get_elem(x:grid1d,y:grid1d)=grid('//&
        'PM__get_elem(x.d1,y.d1))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid2d,y:grid2d)=grid('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid3d,y:grid3d)=grid('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid4d,y:grid4d)=grid('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid5d,y:grid5d)=grid('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4),'//&
        'PM__get_elem(x.d5,y.d5))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid6d,y:grid6d)=grid('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4),'//&
        'PM__get_elem(x.d5,y.d5),PM__get_elem(x.d6,y.d6))',line)
   call dcl_uproc(parser,'PM__get_elem(x:grid7d,y:grid7d)=grid('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4),'//&
        'PM__get_elem(x.d5,y.d5),PM__get_elem(x.d6,y.d6),'//&
        'PM__get_elem(x.d7,y.d7))',line)
   
   ! Check grid inclusion (subgrid)
   call dcl_uproc(parser,'includes(x:grid1d,y:grid1d)='//&
        'x.d1 includes y.d1',line)
   call dcl_uproc(parser,'includes(x:grid2d,y:grid2d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2',line)
   call dcl_uproc(parser,'includes(x:grid3d,y:grid3d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2 and '//&
        'x.d3 includes y.d3',line)
   call dcl_uproc(parser,'includes(x:grid4d,y:grid4d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2 and '//&
        'x.d3 includes y.d3 and x.d4 includes y.d4',line)
   call dcl_uproc(parser,'includes(x:grid5d,y:grid5d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2 and '//&
        'x.d3 includes y.d3 and x.d4 includes y.d4 and x.d5 includes y.d5',&
        line)
   call dcl_uproc(parser,'includes(x:grid6d,y:grid6d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2 and '//&
        'x.d3 includes y.d3 and x.d4 includes y.d4 and x.d5 includes y.d5'//&
        ' and x.d6 includes y.d6',line)
   call dcl_uproc(parser,'includes(x:grid7d,y:grid7d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2 and '//&
        'x.d3 includes y.d3 and x.d4 includes y.d4 and x.d5 includes y.d5 '//&
        'and x.d6 includes y.d6 and '//&
        'x.d7 includes y.d7',line)

   ! Domain of a grid
   call dcl_uproc(parser,'dom(x:grid)=x',line)
   
   ! Shape of a grid
   call dcl_uproc(parser,'shape(x:grid1d)=tuple(size(x.d1))',line)
   call dcl_uproc(parser,'shape(x:grid2d)=tuple(size(x.d1),size(x.d2))',line)
   call dcl_uproc(parser,'shape(x:grid3d)=tuple(size(x.d1),size(x.d2),'//&
        'size(x.d3))',line)
   call dcl_uproc(parser,'shape(x:grid4d)=tuple(size(x.d1),size(x.d2),'//&
        'size(x.d3),size(x.d4))',line)
   call dcl_uproc(parser,'shape(x:grid5d)=tuple(size(x.d1),size(x.d2),'//&
        'size(x.d3),size(x.d4),size(x.d5))',line)
   call dcl_uproc(parser,'shape(x:grid6d)=tuple(size(x.d1),size(x.d2),'//&
        'size(x.d3),size(x.d4),size(x.d5),size(x.d6))',line)
   call dcl_uproc(parser,'shape(x:grid7d)=tuple(size(x.d1),size(x.d2),'//&
        'size(x.d3),size(x.d4),size(x.d5),size(x.d6),size(x.d7))',line)
   
   ! Check if grids conform
   call dcl_uproc(parser,'_conform(a,b)=size(a)==size(b)',line)
   call dcl_uproc(parser,'conform(a,b)=false',line)
   call dcl_uproc(parser,'conform(a:grid1d,b:grid1d)='//&
        '_conform(a.d1,b.d1)',line)
   call dcl_uproc(parser,'conform(a:grid2d,b:grid2d)='//&
        '_conform(a.d1,b.d1) and _conform(a.d2,b.d2)',line)
   call dcl_uproc(parser,'conform(a:grid3d,b:grid3d)='//&
        '_conform(a.d1,b.d1) and _conform(a.d2,b.d2)'//&
        ' and _conform(a.d3,b.d3)',line)
   call dcl_uproc(parser,'conform(a:grid4d,b:grid4d)='//&
        '_conform(a.d1,b.d1) and _conform(a.d2,b.d2)'//&
        ' and _conform(a.d3,b.d3) and _conform(a.d4,b.d4)',line)
   call dcl_uproc(parser,'conform(a:grid5d,b:grid5d)='//&
        '_conform(a.d1,b.d1) and _conform(a.d2,b.d2)'//&
        ' and _conform(a.d3,b.d3) and _conform(a.d4,b.d4)'//&
        ' and _conform(a.d5,b.d5) ',line)
   call dcl_uproc(parser,'conform(a:grid6d,b:grid6d)='//&
        '_conform(a.d1,b.d1) and _conform(a.d2,b.d2)'//&
        ' and _conform(a.d3,b.d3) and _conform(a.d4,b.d4)'//&
        ' and _conform(a.d5,b.d5) '//&
        ' and _conform(a.d6,b.d6) ',line)
   call dcl_uproc(parser,'conform(a:grid7d,b:grid7d)='//&
        '_conform(a.d1,b.d1) and _conform(a.d2,b.d2)'//&
        ' and _conform(a.d3,b.d3) and _conform(a.d4,b.d4)'//&
        ' and _conform(a.d5,b.d5) '//&
        ' and _conform(a.d6,b.d6) and _conform(a.d7,b.d7) ',line)
   
   ! Element membership of a grid
   call dcl_uproc(parser,'in(x:any_int,y:grid1d)=long(x) in y.d1',line)
   call dcl_uproc(parser,'in(x:tuple1d,y:grid1d)=x.d1 in y.d1',line)
   call dcl_uproc(parser,&
        'in(x:tuple2d,y:grid2d)=x.d1 in y.d1 and x.d2 in y.d2',line)
   call dcl_uproc(parser,&
        'in(x:tuple3d,y:grid3d)=x.d1 in y.d1 and x.d2 in y.d2'//&
        ' and x.d3 in y.d3',line)
   call dcl_uproc(parser,&
        'in(x:tuple4d,y:grid4d)=x.d1 in y.d1 and x.d2 in y.d2'//&
        ' and x.d3 in y.d3 and x.d4 in y.d4',line)
   call dcl_uproc(parser,&
        'in(x:tuple5d,y:grid5d)=x.d1 in y.d1 and x.d2 in y.d2'//&
        ' and x.d3 in y.d3 and x.d4 in y.d4 and x.d5 in y.d5',line)
   call dcl_uproc(parser,&
        'in(x:tuple6d,y:grid6d)=x.d1 in y.d1 and x.d2 in y.d2'//&
        ' and x.d3 in y.d3 and x.d4 in y.d4 and x.d5 in y.d5'//&
        ' and x.d6 in y.d6',line)
   call dcl_uproc(parser,&
        'in(x:tuple7d,y:grid7d)=x.d1 in y.d1 and x.d2 in y.d2'//&
        ' and x.d3 in y.d3 and x.d4 in y.d4 and x.d5 in y.d5'//&
        ' and x.d6 in y.d6 and x.d7 in y.d7',line)
   
   ! Intersection of grids
   call dcl_uproc(parser,'overlap(x:grid1d,y:grid1d)='//&
        'grid(overlap(x.d1,y.d1))',line)
   call dcl_uproc(parser,'overlap(x:grid2d,y:grid2d)='//&
        'grid(overlap(x.d1,y.d1),overlap(x.d2,y.d2)) ',line)
   call dcl_uproc(parser,'overlap(x:grid3d,y:grid3d)='//&
        'grid(overlap(x.d1,y.d1),overlap(x.d2,y.d2), '//&
        'overlap(x.d3,y.d3) ) ',line)
   call dcl_uproc(parser,'overlap(x:grid4d,y:grid4d)='//&
        'grid(overlap(x.d1,y.d1),overlap(x.d2,y.d2), '//&
        'overlap(x.d3,y.d3),overlap(x.d4,y.d4) ) ',line)
   call dcl_uproc(parser,'overlap(x:grid5d,y:grid5d)='//&
        'grid(overlap(x.d1,y.d1),overlap(x.d2,y.d2), '//&
        'overlap(x.d3,y.d3),overlap(x.d4,y.d4),overlap(x.d5,y.d5) ) ',line)
   call dcl_uproc(parser,'overlap(x:grid6d,y:grid6d)='//&
        'grid(overlap(x.d1,y.d1),overlap(x.d2,y.d2), '//&
        'overlap(x.d3,y.d3),overlap(x.d4,y.d4),overlap(x.d5,y.d5),'//&
        'overlap(x.d6,y.d6) ) ',line)
   call dcl_uproc(parser,'overlap(x:grid7d,y:grid7d)='//&
        'grid(overlap(x.d1,y.d1),overlap(x.d2,y.d2), '//&
        'overlap(x.d3,y.d3),overlap(x.d4,y.d4),'//&
        'overlap(x.d5,y.d5),overlap(x.d6,y.d6),'//&
        'overlap(x.d7,y.d7) ) ',line)

   ! Expand grid
   call dcl_uproc(parser,'expand(x:grid1d,y:grid1d)='//&
        'grid(expand(x.d1,y.d1)) ',line)
   call dcl_uproc(parser,'expand(x:grid2d,y:grid2d)='//&
        'grid(expand(x.d1,y.d1),expand(x.d2,y.d2)) ',line)
   call dcl_uproc(parser,'expand(x:grid3d,y:grid3d)='//&
        'grid(expand(x.d1,y.d1),expand(x.d2,y.d2), '//&
        'expand(x.d3,y.d3) ) ',line)
   call dcl_uproc(parser,'expand(x:grid4d,y:grid4d)='//&
        'grid(expand(x.d1,y.d1),expand(x.d2,y.d2), '//&
        'expand(x.d3,y.d3),expand(x.d4,y.d4) ) ',line)
   call dcl_uproc(parser,'expand(x:grid5d,y:grid5d)='//&
        'grid(expand(x.d1,y.d1),expand(x.d2,y.d2), '//&
        'expand(x.d3,y.d3),expand(x.d4,y.d4),expand(x.d5,y.d5) ) ',line)
   call dcl_uproc(parser,'expand(x:grid6d,y:grid6d)='//&
        'grid(expand(x.d1,y.d1),expand(x.d2,y.d2), '//&
        'expand(x.d3,y.d3),expand(x.d4,y.d4),expand(x.d5,y.d5),'//&
        'expand(x.d6,y.d6) ) ',line)
   call dcl_uproc(parser,'expand(x:grid7d,y:grid7d)='//&
        'grid(expand(x.d1,y.d1),expand(x.d2,y.d2), '//&
        'expand(x.d3,y.d3),expand(x.d4,y.d4),'//&
        'expand(x.d5,y.d5),expand(x.d6,y.d6),'//&
        'expand(x.d7,y.d7) ) ',line)
   
   ! Contract grid
   call dcl_uproc(parser,'contract(x:grid1d,y:grid1d)='//&
        'grid(contract(x.d1,y.d1)) ',line)
   call dcl_uproc(parser,'contract(x:grid2d,y:grid2d)='//&
        'grid(contract(x.d1,y.d1),contract(x.d2,y.d2)) ',line)
   call dcl_uproc(parser,'contract(x:grid3d,y:grid3d)='//&
        'grid(contract(x.d1,y.d1),contract(x.d2,y.d2), '//&
        'contract(x.d3,y.d3) ) ',line)
   call dcl_uproc(parser,'contract(x:grid4d,y:grid4d)='//&
        'grid(contract(x.d1,y.d1),contract(x.d2,y.d2), '//&
        'contract(x.d3,y.d3),contract(x.d4,y.d4) ) ',line)
   call dcl_uproc(parser,'contract(x:grid5d,y:grid5d)='//&
        'grid(contract(x.d1,y.d1),contract(x.d2,y.d2), '//&
        'contract(x.d3,y.d3),contract(x.d4,y.d4),contract(x.d5,y.d5) ) ',line)
   call dcl_uproc(parser,'contract(x:grid6d,y:grid6d)='//&
        'grid(contract(x.d1,y.d1),contract(x.d2,y.d2), '//&
        'contract(x.d3,y.d3),contract(x.d4,y.d4),contract(x.d5,y.d5),'//&
        'contract(x.d6,y.d6) ) ',line)
   call dcl_uproc(parser,'contract(x:grid7d,y:grid7d)='//&
        'grid(contract(x.d1,y.d1),contract(x.d2,y.d2), '//&
        'contract(x.d3,y.d3),contract(x.d4,y.d4),'//&
        'contract(x.d5,y.d5),contract(x.d6,y.d6),'//&
        'contract(x.d7,y.d7) ) ',line)

   ! Shape index of point within a grid
   call dcl_uproc(parser,'loc_in_shape(x:grid1d,y)=loc_in_shape(x.d1,y)',line)
   call dcl_uproc(parser,'loc_in_shape(x:grid1d,y:tuple1d)='//&
        'tuple(loc_in_shape(x.d1,y.d1))',line)
   call dcl_uproc(parser,'loc_in_shape(x:grid2d,y:tuple2d)='//&
        'tuple(loc_in_shape(x.d1,y.d1),loc_in_shape(x.d2,y.d2))',line)
   call dcl_uproc(parser,'loc_in_shape(x:grid3d,y:tuple3d)='//&
        'tuple(loc_in_shape(x.d1,y.d1),loc_in_shape(x.d2,y.d2), '//&
        'loc_in_shape(x.d3,y.d3)) ',line)
   call dcl_uproc(parser,'loc_in_shape(x:grid4d,y:tuple4d)='//&
        'tuple(loc_in_shape(x.d1,y.d1),loc_in_shape(x.d2,y.d2), '//&
        'loc_in_shape(x.d3,y.d3),loc_in_shape(x.d4,y.d4) ) ',line)
   call dcl_uproc(parser,'loc_in_shape(x:grid5d,y:tuple5d)='//&
        'tuple(loc_in_shape(x.d1,y.d1),loc_in_shape(x.d2,y.d2), '//&
        'loc_in_shape(x.d3,y.d3),loc_in_shape(x.d4,y.d4),'//&
        'loc_in_shape(x.d5,y.d5) )',&
        line)
   call dcl_uproc(parser,'loc_in_shape(x:grid6d,y:tuple6d)='//&
        'tuple(loc_in_shape(x.d1,y.d1),loc_in_shape(x.d2,y.d2), '//&
        'loc_in_shape(x.d3,y.d3),loc_in_shape(x.d4,y.d4),'//&
        'loc_in_shape(x.d5,y.d5),'//&
        'loc_in_shape(x.d6,y.d6) ) ',line)
   call dcl_uproc(parser,'loc_in_shape(x:grid7d,y:tuple7d)='//&
        'tuple(loc_in_shape(x.d1,y.d1),loc_in_shape(x.d2,y.d2), '//&
        'loc_in_shape(x.d3,y.d3),loc_in_shape(x.d4,y.d4),'//&
        'loc_in_shape(x.d5,y.d5),'//&
        'loc_in_shape(x.d6,y.d6),loc_in_shape(x.d7,y.d7) ) ',&
        line)

   ! Shapes and tiles

   ! Tile size
   call dcl_uproc(parser,'tile_size(x:long)=x',line)
   call dcl_uproc(parser,'tile_size(x)=size(x)',line)
   call dcl_uproc(parser,'tile_size(x:tuple1d)=tile_size(x.d1)',line)
   call dcl_uproc(parser,&
        'tile_size(x:tuple2d)=tile_size(x.d1)*tile_size(x.d2)',line)
   call dcl_uproc(parser,'tile_size(x:tuple3d)='//&
        'tile_size(x.d1)*tile_size(x.d2)*tile_size(x.d3)',line)
   call dcl_uproc(parser,'tile_size(x:tuple4d)='//&
        'tile_size(x.d1)*tile_size(x.d2)*tile_size(x.d3)*tile_size(x.d4)',line)
   call dcl_uproc(parser,'tile_size(x:tuple5d)='//&
        'tile_size(x.d1)*tile_size(x.d2)*tile_size(x.d3)*tile_size(x.d4)*'//&
        'tile_size(x.d5)',line)
   call dcl_uproc(parser,'tile_size(x:tuple6d)='//&
        'tile_size(x.d1)*tile_size(x.d2)*tile_size(x.d3)*tile_size(x.d4)*'//&
        'tile_size(x.d5)*tile_size(x.d6)',line)
   call dcl_uproc(parser,'tile_size(x:tuple7d)='//&
        'tile_size(x.d1)*tile_size(x.d2)*tile_size(x.d3)*tile_size(x.d4)*'//&
        'tile_size(x.d5)*tile_size(x.d6)*tile_size(x.d7)',line)

   ! Tile domain
   call dcl_uproc(parser,'dom(x:tuple1d)=grid(0l..tile_size(x.d1)-1l)',line)
   call dcl_uproc(parser,'dom(x:tuple2d)='//&
        'grid(0l..tile_size(x.d1)-1l,0l..tile_size(x.d2)-1l)',line)
   call dcl_uproc(parser,'dom(x:tuple3d)='//&
        'grid(0l..tile_size(x.d1)-1l,'//&
        '0l..tile_size(x.d2)-1l,0l..tile_size(x.d3)-1l)',line)
   call dcl_uproc(parser,'dom(x:tuple4d)='//&
        'grid(0l..tile_size(x.d1)-1l,0l..tile_size(x.d2)-1l,'//&
        '0l..tile_size(x.d3)-1l,0l..tile_size(x.d4)-1l)',line)
   call dcl_uproc(parser,'dom(x:tuple5d)='//&
        'grid(0l..tile_size(x.d1)-1l,'//&
        '0l..tile_size(x.d2)-1l,0l..tile_size(x.d3)-1l,'//&
        '0l..tile_size(x.d4)-1l,'//&
        '0l..tile_size(x.d5)-1l)',line)
   call dcl_uproc(parser,'dom(x:tuple6d)='//&
        'grid(0l..tile_size(x.d1),'//&
        '0l..tile_size(x.d2)-1l,0l..tile_size(x.d3)-1l,'//&
        '0l..tile_size(x.d4)-1l,'//&
        '0l..tile_size(x.d5)-1l,0l..tile_size(x.d6)-1l)',line)
   call dcl_uproc(parser,'dom(x:tuple7d)='//&
        'grid(0l..tile_size(x.d1)-1l,'//&
        '0l..tile_size(x.d2)-1l,0l..tile_size(x.d3)-1l,'//&
        '0l..tile_size(x.d4)-1l,'//&
        '0l..tile_size(x.d5)-1l,0l..'//&
        'tile_size(x.d6)-1l,0l..tile_size(x.d7)-1l)',line)

   ! Subscript a tuple sequence
   call dcl_uproc(parser,'PM__get_elem(x:tuple1d,y:any_int)='//&
        'PM__get_elem(x.d1,long(y))',line)
   call dcl_uproc(parser,'PM__get_elem(x:tuple1d,y:tuple1d)=tuple('//&
        'PM__get_elem(x.d1,y.d1))',line)
   call dcl_uproc(parser,'PM__get_elem(x:tuple2d,y:tuple2d)=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2))',line)
   call dcl_uproc(parser,'PM__get_elem(x:tuple3d,y:tuple3d)=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3))',line)
   call dcl_uproc(parser,'PM__get_elem(x:tuple4d,y:tuple4d)=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4))',line)
   call dcl_uproc(parser,'PM__get_elem(x:tuple5d,y:tuple5d)=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4),'//&
        'PM__get_elem(x.d5,y.d5))',line)
   call dcl_uproc(parser,'PM__get_elem(x:tuple6d,y:tuple6d)=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4),'//&
        'PM__get_elem(x.d5,y.d5),PM__get_elem(x.d6,y.d6))',line)
   call dcl_uproc(parser,'PM__get_elem(x:tuple7d,y:tuple7d)=tuple('//&
        'PM__get_elem(x.d1,y.d1),PM__get_elem(x.d2,y.d2),'//&
        'PM__get_elem(x.d3,y.d3),PM__get_elem(x.d4,y.d4),'//&
        'PM__get_elem(x.d5,y.d5),PM__get_elem(x.d6,y.d6),'//&
        'PM__get_elem(x.d7,y.d7))',line)
   
   ! Iteration
   
   ! - first element
   call dcl_uproc(parser,'PM__first(d:long)=0l,null,d>0l',line)
   call dcl_uproc(parser,'PM__first(d:tuple1d)='//&
        'i,s,e where i,s,e=PM__first(d.d1)',line)
   call dcl_uproc(parser,'PM__first(d:tuple2d)='//&
        'tuple(j1,j2),rec _gs2d{d1=s1,d2=s2},e1 and e2'//&
        ' where j1,s1,e1=PM__first(d.d1),j2,s2,e2=PM__first(d.d2)',line)
   call dcl_uproc(parser,'PM__first(d:tuple3d)='//&
        'tuple(j1,j2,j3),rec _gs3d{d1=s1,d2=s2,d3=s3},e1 and e2 and e3'//&
        ' where j1,s1,e1=PM__first(d.d1),'//&
        'j2,s2,e2=PM__first(d.d2),j3,s3,e3=PM__first(d.d3)',&
        line)
   call dcl_uproc(parser,'PM__first(d:tuple4d)='//&
        'tuple(j1,j2,j3,j4),'//&
        'rec _gs4d{d1=s1,d2=s2,d3=s3,d4=s4},e1 and e2 and e3 and e4'//&
        ' where j1,s1,e1=PM__first(d.d1),j2,s2,e2=PM__first(d.d2),'//&
        'j3,s3,e3=PM__first(d.d3),j4,s4,e4=PM__first(d.d4)',line)
   call dcl_uproc(parser,'PM__first(d:tuple5d)=tuple(j1,j2,j3,j4,j5),'//&
        ' rec _gs5d{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5},'//&
        'e1 and e2 and e3 and e4 and e5'//&
        ' where j1,s1,e1=PM__first(d.d1),j2,s2,e2=PM__first(d.d2),j3,s3,'//&
        'e3=PM__first(d.d3),j4,s4,e4=PM__first(d.d4),'//&
        ' j5,s5,e5=PM__first(d.d5)',line)
   call dcl_uproc(parser,'PM__first(d:tuple6d)=tuple(j1,j2,j3,j4,j5,j6),'//&
        'rec _gs6d{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5,d6=s6},'//&
        'e1 and e2 and e3 and e4 and e5 and e6'//&
        ' where j1,s1,e1=PM__first(d.d1),j2,s2,e2=PM__first(d.d2),'//&
        'j3,s3,e3=PM__first(d.d3),j4,s4,e4=PM__first(d.d4),'//&
        ' j5,s5,e5=PM__first(d.d5),j6,s6,e6=PM__first(d.d6)',line)
   call dcl_uproc(parser,'PM__first(d:tuple7d)=tuple(j1,j2,j3,j4,j5,j6,j7),'//&
        'rec _gs7d{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5,d6=s6,d7=s7},'//&
        'e1 and e2 and e3 and e4 and e5 and e6 and e7'//&
        ' where j1,s1,e1=PM__first(d.d1),j2,s2,'//&
        'e2=PM__first(d.d2),j3,s3,e3=PM__first(d.d3),'//&
        'j4,s4,e4=PM__first(d.d4),'//&
        ' j5,s5,e5=PM__first(d.d5),'//&
        'j6,s6,e6=PM__first(d.d6),j7,s7,e7=PM__first(d.d7)',line)

   ! - subsequent elements
   call dcl_uproc(parser,'PM__next(d:long,g,i)=ii,null,ii<d where ii=i+1',line)
   call dcl_uproc(parser,&
        'PM__next(d:tuple1d,g,i)=j,s,e where j,s,e=PM__next(d.d1,g,i)',line)
   call dcl_uproc(parser,'PM__next(d:tuple2d,g:rec _gs2d{d1,d2},i:tuple2d) do '//&
        'j1,s1,e:=PM__next(d.d1,g.d1,i.d1);j2:=i.d2;s2:=g.d2;'//&
        'if not e then '//&
        '  j1,s1,_=PM__first(d.d1);j2,s2,e=PM__next(d.d2,g.d2,i.d2) endif;'//&
        'result=tuple(j1,j2),rec _gs2d{d1=s1,d2=s2},e endproc',line)
   call dcl_uproc(parser,&
        'PM__next(d:tuple3d,g:rec _gs3d{d1,d2,d3},i:tuple3d) do '//&
        'j1,s1,e:=PM__next(d.d1,g.d1,i.d1);'//&
        'j2:=i.d2;s2:=g.d2;j3:=i.d3;s3:=g.d3;'//&
        'if not e then j1,s1,_=PM__first(d.d1);'//&
        'j2,s2,e=PM__next(d.d2,g.d2,i.d2) endif;'//&
        'if not e then j2,s2,_=PM__first(d.d2);'//&
        'j3,s3,e=PM__next(d.d3,g.d3,i.d3) endif;'//&
        'result=tuple(j1,j2,j3),rec _gs3d{d1=s1,d2=s2,d3=s3},e endproc',line)
   call dcl_uproc(parser,&
        'PM__next(d:tuple4d,g:rec _gs4d{d1,d2,d3,d4},i:tuple4d) do '//&
        'j1,s1,e:=PM__next(d.d1,g.d1,i.d1);j2:=i.d2;s2:=g.d2;'//&
        'j3:=i.d3;s3:=g.d3;j4:=i.d4;s4:=g.d4;'//&
        'if not e then j1,s1,_=PM__first(d.d1);'//&
        'j2,s2,e=PM__next(d.d2,g.d2,i.d2) endif;'//&
        'if not e then j2,s2,_=PM__first(d.d2);'//&
        'j3,s3,e=PM__next(d.d3,g.d3,i.d3) endif;'//&
        'if not e then j3,s3,_=PM__first(d.d3);'//&
        'j4,s4,e=PM__next(d.d4,g.d4,i.d4) endif;'//&
        'result=tuple(j1,j2,j3,j4),rec _gs4d{d1=s1,d2=s2,d3=s3,d4=s4},e'//&
        ' endproc',line)
   call dcl_uproc(parser,&
        'PM__next(d:tuple5d,g:rec _gs5d{d1,d2,d3,d4,d5},i:tuple5d) do '//&
        'j1,s1,e:=PM__next(d.d1,g.d1,i.d1);'//&
        'j2:=i.d2;s2:=g.d2;j3:=i.d3;s3:=g.d3;j4:=i.d4;'//&
        's4:=g.d4;j5:=i.d5;s5:=g.d5;'//&
        'if not e then j1,s1,_=PM__first(d.d1);'//&
        'j2,s2,e=PM__next(d.d2,g.d2,i.d2) endif;'//&
        'if not e then j2,s2,_=PM__first(d.d2);'//&
        'j3,s3,e=PM__next(d.d3,g.d3,i.d3) endif;'//&
        'if not e then j3,s3,_=PM__first(d.d3);'//&
        'j4,s4,e=PM__next(d.d4,g.d4,i.d4) endif;'//&
        'if not e then j4,s4,_=PM__first(d.d4);'//&
        'j5,s5,e=PM__next(d.d5,g.d5,i.d5) endif;'//&
        'result=tuple(j1,j2,j3,j4,j5),'//&
        'rec _gs5d{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5},e'//&
        ' endproc',line)
   call dcl_uproc(parser,&
        'PM__next(d:tuple6d,g:rec _gs6d{d1,d2,d3,d4,d5,d6},i:tuple6d) do '//&
        'j1,s1,e:=PM__next(d.d1,g.d1,i.d1);'//&
        'j2:=i.d2;s2:=g.d2;j3:=i.d3;s3:=g.d3;'//&
        'j4:=i.d4;s4:=g.d4;j5:=i.d5;s5:=g.d5;j6:=i.d6;s6:=g.d6;'//&
        'if not e then j1,s1,_=PM__first(d.d1);'//&
        'j2,s2,e=PM__next(d.d2,g.d2,i.d2) endif;'//&
        'if not e then j2,s2,_=PM__first(d.d2);'//&
        'j3,s3,e=PM__next(d.d3,g.d3,i.d3) endif;'//&
        'if not e then j3,s3,_=PM__first(d.d3);'//&
        'j4,s4,e=PM__next(d.d4,g.d4,i.d4) endif;'//&
        'if not e then j4,s4,_=PM__first(d.d4);'//&
        'j5,s5,e=PM__next(d.d5,g.d5,i.d5) endif;'//&
        'if not e then j5,s5,_=PM__first(d.d5);'//&
        'j6,s6,e=PM__next(d.d5,g.d6,i.d6) endif;'//&
        'result=tuple(j1,j2,j3,j4,j5,j6),'//&
        'rec _gs6d{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5,d6=s6},e endproc',line)
   call dcl_uproc(parser,&
        'PM__next(d:tuple7d,g:rec _gs7d{d1,d2,d3,d4,d5,d6,d7},i:tuple7d) do '//&
        'j1,s1,e:=PM__next(d.d1,g.d1,i.d1);'//&
        'j2:=i.d2;s2:=g.d2;j3:=i.d3;s3:=g.d3;j4:=i.d4;s4:=g.d4;'//&
        'j5:=i.d5;s5:=g.d5;j6:=i.d6;s6:=g.d6;j7:=i.d7;s7:=g.d7;'//&
        'if not e then j1,s1,_=PM__first(d.d1);'//&
        'j2,s2,e=PM__next(d.d2,g.d2,i.d2) endif;'//&
        'if not e then j2,s2,_=PM__first(d.d2);'//&
        'j3,s3,e=PM__next(d.d3,g.d3,i.d3) endif;'//&
        'if not e then j3,s3,_=PM__first(d.d3);'//&
        'j4,s4,e=PM__next(d.d4,g.d4,i.d4) endif;'//&
        'if not e then j4,s4,_=PM__first(d.d4);'//&
        'j5,s5,e=PM__next(d.d5,g.d5,i.d5) endif;'//&
        'if not e then j5,s5,_=PM__first(d.d5);'//&
        'j6,s6,e=PM__next(d.d5,g.d6,i.d6) endif;'//&
        'if not e then j6,s6,_=PM__first(d.d6);'//&
        'j7,s7,e=PM__next(d.d7,g.d7,i.d7) endif;'//&
        'result=tuple(j1,j2,j3,j4,j5,j6,j7),'//&
        'rec _gs7d{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5,d6=s6,d7=s7},e endproc',line)

   ! Vector element generation
   call dcl_uproc(parser,&
        '_elts(x:range{long},siz,tot)=_iota(siz,x._lo,x._hi,1l,tot)',line)
   call dcl_uproc(parser,&
        '_elts(x:seq{long},siz,tot)=_iota(siz,x._lo,x._hi,x._st,tot)',line)
   call dcl_uproc(parser,&
        '_elts(x:grid1d,siz,tot)=_elts(x.d1,siz,tot)',line)
   call dcl_uproc(parser,&
        '_elts(x:grid2d,siz,tot)='//&
        'tuple(_elts(x.d1,siz,tot),_elts(x.d2,siz*size(x.d1),tot) )',line)
   call dcl_uproc(parser,&
        '_elts(x:grid3d,siz,tot)=tuple(_elts( x.d1,siz,tot),'//&
        '_elts(x.d2,s1,tot),'//&
        '_elts( x.d3, s1*size(x.d2), tot)) where s1=siz*size(x.d1)',line)
   call dcl_uproc(parser,&
        '_elts(x:grid4d,siz,tot)='//&
        'tuple(_elts( x.d1,siz,tot),_elts(x.d2,s1,tot),'//&
        '_elts(x.d3,s2,tot),_elts(x.d4,s2*size(x.d3),tot)) '//&
        'where s2=s1*size(x.d2) where s1=siz*size(x.d1)',line)
   call dcl_uproc(parser,&
        '_elts(x:grid5d,siz,tot)='//&
        'tuple(_elts( x.d1,siz,tot),_elts(x.d2,s1,tot),'//&
        '_elts(x.d3,s2,tot),_elts(x.d4,s3,tot),'//&
        '_elts(x.d5,s3*size(x.d4), tot)) '//&
        'where s3=s2*size(x.d3) where s2=s1*size(x.d2) '//&
        'where s1=siz*size(x.d1)',&
        line)
   call dcl_uproc(parser,&
        '_elts(x:grid6d,siz,tot)=tuple(_elts( x.d1,siz,tot),'//&
        '_elts(x.d2,s1,tot),'//&
        '_elts(x.d3,s2,tot),_elts(x.d4,s3,tot),_elts(x.d5,s4,tot),'//&
        '_elts(x.d6,s4*size(x.d5), tot)) '//&
        'where s4=s3*size(x.d4) where s3=s2*size(x.d3) '//&
        'where s2=s1*size(x.d2) where s1=siz*size(x.d1)',line)
   call dcl_uproc(parser,&
        '_elts(x:grid7d,siz,tot)=tuple(_elts( x.d1,siz,tot),'//&
        '_elts(x.d2,s1,tot),'//&
        '_elts(x.d3,s2,tot),_elts(x.d4,s3,tot),'//&
        '_elts(x.d5,s4,tot),_elts(x.d6,s5,tot),'//&
        '_elts(x.d7,s5*size(x.d6), tot)) '//&
        'where s5=s4*size(x.d5) where s4=s3*size(x.d4) '//&
        'where s3=s2*size(x.d3) '//&
        'where s2=s1*size(x.d2) where s1=siz*size(x.d1)',line)
  

   ! Index type
   call dcl_type(parser,'index includes any_int,tuple{any_int},'//&
         'tuple{any_int,any_int},tuple{any_int,any_int,any_int},'//&
         'tuple{any_int,any_int,any_int,any_int},'//&
         'tuple{any_int,any_int,any_int,any_int,any_int},'//&
         'tuple{any_int,any_int,any_int,any_int,any_int,any_int},'//&
         'tuple{any_int,any_int,any_int,any_int,any_int,any_int,any_int}',line)

   ! Slice and subscript types
   call dcl_type(parser,'range_below{x} is rec _range_below{_t:x}',line)
   call dcl_type(parser,'range_above{x} is rec _range_above{_t:x}',line)
   call dcl_type(parser,&
        'strided_range_below{x} is rec _strided_range_below{_t:x,_s:x}',line)
   call dcl_type(parser,&
        'strided_range_above{x} is rec _strided_range_above{_t:x,_s:x}',line)
   call dcl_type(parser,'stride{x} is rec _stride{_t:x}',line)
   call dcl_type(parser,'simple_slice is'//&
        ' range{any_int},seq{any_int},range_above{any_int},'//&
        ' range_below{any_int},stride{any_int},null',&
        line)
   call dcl_type(parser,'slice is simple_slice,dom,tuple{simple_slice},'//&
        'tuple{simple_slice,simple_slice},'//&
        'tuple{simple_slice,simple_slice,simple_slice},'//&
        'tuple{simple_slice,simple_slice,simple_slice,simple_slice},'//&
        'tuple{simple_slice,simple_slice,simple_slice,simple_slice,'//&
        'simple_slice},'//&
        'tuple{simple_slice,simple_slice,simple_slice,simple_slice,'//&
        'simple_slice,'//&
        'simple_slice},'//&
        'tuple{simple_slice,simple_slice,simple_slice,simple_slice,'//&
        'simple_slice,'//&
        'simple_slice,simple_slice}'&
        ,line)
   call dcl_type(parser,'subs is slice,index',line)

   call dcl_uproc(parser,'..._(x)=rec _range_below{_t=x}',line)
   call dcl_uproc(parser,'_...(x)=rec _range_above{_t=x}',line)
   call dcl_uproc(parser,'by(x)=rec _stride{_t=x}',line)
   call dcl_uproc(parser,'by(x:range_above{},y)='//&
        'rec _strided_range_above{_t=x._t,_s=convert(y,x._t)}',line)
   call dcl_uproc(parser,'by(x:range_below{},y)='//&
        'rec _strided_range_below{_t=x._t,_s=convert(y,x._t)}',line)
   
   ! Slicing
   call dcl_uproc(parser,'get_slice(d:range{long},x:any_int)'//&
        '=xx..xx where xx=long(x)',line)
   call dcl_uproc(parser,'get_slice(d:range{long},x:range{any_int})='//&
        ' long(x) ',line)
   call dcl_uproc(parser,'get_slice(d:range{long},x:seq{any_int})='//&
        ' long(x)',line)
   call dcl_uproc(parser,'get_slice(d:range{long},x:range_below{any_int})='//&
        ' d._lo .. long(x._t)',line)
   call dcl_uproc(parser,'get_slice(d:range{long},x:range_above{any_int})='//&
        ' long(x._t) .. d._hi',line)
   call dcl_uproc(parser,'get_slice(d:range{long},x:stride{any_int})='//&
        ' d by x._t',line)
   call dcl_uproc(parser,&
        'get_slice(d:range{long},x:strided_range_below{any_int})='//&
        ' d._lo .. long(x._t) by x._s',line)
   call dcl_uproc(parser,&
        'get_slice(d:range{long},x:strided_range_above{any_int})='//&
        ' long(x._t) .. d._hi by x._s',line)
   call dcl_uproc(parser,&
        'get_slice(d:seq{long},x:range_base)=long(x)',line)
   call dcl_uproc(parser,'get_slice(d:seq{any_int},x:range{any_int}) do'//&
        ' lo:=0l;hi:=0l;'//&
        ' if d._st>0 then '//&
        '  lo=(long(x._lo)-d._f+d._st-1)/d._st*d._st+d._f;'//&
        '  hi=(long(x._hi)-d._f)/d._st*d._st+d._f '//&
        ' else '//&
        '  lo=d._f-(d._f-long(x._hi)-d._st-1)/d._st*d._st;'//&
        '  hi=d._f-(d._f-long(x._lo))/d._st*d._st '//&
        ' endif; result=lo..hi by d._st endproc',&
        line)
   call dcl_uproc(parser,'get_slice(d:seq{long},x:seq{any_int}) do'//&
        ' lo:=0l;hi:=0l;'//&
        ' if d._st>0 then '//&
        '  lo=(long(x._lo)-d._f+d._st-1)/d._st*d._st+d._f;'//&
        '  hi=(long(x._hi)-d._f)/d._st*d._st+d._f '//&
        ' else '//&
        '  lo=d._f-(d._f-long(x._hi)-d._st-1)/d._st*d._st;'//&
        '  hi=d._f-(d._f-long(x._lo))/d._st*d._st '//&
        ' endif; '//&
        'result=x._st>0=>lo..hi by d._st*x._st||hi..lo by d._st*x._st endproc',&
        line)
   call dcl_uproc(parser,'get_slice(d:seq{long},x:range_above{any_int}) do'//&
        ' lo:=0l;hi:=0l;'//&
        ' if d._st>0 then '//&
        '  lo=(long(x._t)-d._f+d._st-1)/d._st*d._st+d._f;'//&
        '  hi=d._hi '//&
        ' else '//&
        '  lo=d._hi;'//&
        '  hi=d._f-(d._f-long(x._t))/d._st*d._st '//&
        ' endif; result=lo..hi by d._st endproc',&
        line)
   call dcl_uproc(parser,&
        'get_slice(d:seq{long},x:strided_range_above{any_int}) do'//&
        ' lo:=0l;hi:=0l;'//&
        ' if d._st>0 then '//&
        '  lo=(long(x._t)-d._f+d._st-1)/d._st*d._st+d._f;'//&
        '  hi=d._hi '//&
        ' else '//&
        '  lo=d._hi;'//&
        '  hi=d._f-(d._f-long(x._t))/d._st*d._st '//&
        ' endif; '//&
        'result=x._s>0=>lo..hi by d._st*x._s||hi..lo by d._st*x._s endproc',&
        line)
   call dcl_uproc(parser,'get_slice(d:seq{long},x:range_below{any_int}) do'//&
        ' lo:=0l;hi:=0l;'//&
        ' if d._st>0 then '//&
        '  lo=d._lo;'//&
        '  hi=(long(x._t)-d._f)/d._st*d._st+d._f '//&
        ' else '//&
        '  lo=d._f-(d._f-long(x._t)-d._st-1)/d._st*d._st;'//&
        '  hi=d._lo '//&
        ' endif; result=lo..hi by d._st endproc',&
        line)
   call dcl_uproc(parser,&
        'get_slice(d:seq{long},x:strided_range_below{any_int}) do'//&
        ' lo:=0l;hi:=0l;'//&
        ' if d._st>0 then '//&
        '  lo=d._lo;'//&
        '  hi=(long(x._t)-d._f)/d._st*d._st+d._f '//&
        ' else '//&
        '  lo=d._f-(d._f-long(x._t)-d._st-1)/d._st*d._st;'//&
        '  hi=d._lo '//&
        ' endif; '//&
        'result=x._s>0=>lo..hi by d._st*x._st||hi..lo by d._st*x._s endproc',&
        line)
   call dcl_uproc(parser,'get_slice(d:seq{long},x:stride{any_int})=d by x',line)
   call dcl_uproc(parser,'get_slice(d:range{long},x:null)=d',line)
   call dcl_uproc(parser,'get_slice(d:seq{long},x:null)=d',line)
   call dcl_uproc(parser,'get_slice(d:cycle,x:index)=get_slice(d._x,x)',line)
   call dcl_uproc(parser,&
        'get_slice(d:cycle,x:subs)=rec _cycle{_x=get_slice(d._x,x)}',line)
   call dcl_uproc(parser,'get_slice(x:vector,y)=get_slice(x._d,y)',line)
   call dcl_uproc(parser,'get_slice(x:matrix,y,z)=get_slice(x._d,y,z)',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid1d,i:tuple1d)=get_slice(d,i.d1)',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid2d,i:tuple2d)=get_slice(d,i.d1,i.d2)',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid3d,i:tuple3d)=get_slice(d,i.d1,i.d2,i.d3)',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid4d,i:tuple4d)=get_slice(d,i.d1,i.d2,i.d3,i.d4)',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid5d,i:tuple5d)='//&
        'get_slice(d,i.d1,i.d2,i.d3,i.d4,i.d5)',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid6d,i:tuple6d)='//&
        'get_slice(d,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6)',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid7d,i:tuple7d)='//&
        'get_slice(d,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6.i.d7)',line)
   call dcl_uproc(parser,&
        'get_slice(d:vector,i:tuple1d)=get_slice(d,i.d1)',line)
   call dcl_uproc(parser,&
        'get_slice(d:matrix,i:tuple2d)=get_slice(d,i.d1,i.d2)',line)
   
   call dcl_uproc(parser,&
        'get_slice(d:grid1d,i:grid1d)=i',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid2d,i:grid2d)=i',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid3d,i:grid3d)=i',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid4d,i:grid4d)=i',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid5d,i:grid5d)=i',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid6d,i:grid6d)=i',line)
   call dcl_uproc(parser,&
        'get_slice(d:grid7d,i:grid7d)=i',line)
   
   call dcl_uproc(parser,'get_slice(d,x)=grid(get_slice(d.d1,x))',line)
   call dcl_uproc(parser,'get_slice(d,x,y)='//&
        'grid(get_slice(d.d1,x),get_slice(d.d2,y))',line)
   call dcl_uproc(parser,&
        'get_slice(d,x,y,z)='//&
        'grid(get_slice(d.d1,x),get_slice(d.d2,y),get_slice(d.d3,z))',line)
   call dcl_uproc(parser,&
        'get_slice(d,x,y,z,a)=grid(get_slice(d.d1,x),get_slice(d.d2,y),'//&
        'get_slice(d.d3,z),get_slice(d.d4,a))',line)
   call dcl_uproc(parser,&
        'get_slice(d,x,y,z,a,b)=grid(get_slice(d.d1,x),'//&
        'get_slice(d.d2,y),get_slice(d.d3,z),'//&
        'get_slice(d.d4,a),get_slice(d.d5,b))',line)
   call dcl_uproc(parser,&
        'get_slice(d,x,y,z,a,b,c)=grid(get_slice(d.d1,x),'//&
        'get_slice(d.d2,y),get_slice(d.d3,z),'//&
        'get_slice(d.d4,a),get_slice(d.d5,b),get_slice(d.d6,c))',line)
   call dcl_uproc(parser,&
        'get_slice(d,x,y,z,a,b,c,e)=grid(get_slice(d.d1,x),'//&
        'get_slice(d.d2,y),get_slice(d.d3,z),'//&
        'get_slice(d.d4,a),get_slice(d.d5,b),'//&
        'get_slice(d.d6,c),get_slice(d.d7,e))',line)
   
   ! Displacement slicing
   ! Arguments - domain / location  / displacement slice
   ! Result: displacement 
   call dcl_uproc(parser,'get_disp(d:cycle{long},j:long,x:any_int)='//&
        ' long(x) ',line)
   call dcl_uproc(parser,'get_disp(d:cycle{long},j:long,x:range{any_int})='//&
        ' long(x) ',line)
   call dcl_uproc(parser,'get_disp(d:cycle{long},j:long,x:seq{any_int})='//&
        ' long(x) ',line)
   call dcl_uproc(parser,&
        'get_disp(d:cycle{range{long}},j:long,x:range_above{any_int})='//&
        ' -j..long(x._t) ',line)
   call dcl_uproc(parser,&
        'get_disp(d:cycle{seq{long}},j:long,x:range_above{any_int})='//&
        ' -j..long(x._t) ',line)
   call dcl_uproc(parser,&
        'get_disp(d:cycle{range{long}},j:long,x:range_below{any_int})='//&
        ' long(x._t)..d._x._hi-d._x._lo-j ',line)
   call dcl_uproc(parser,&
        'get_disp(d:cycle{seq{long}},j:long,x:range_below{any_int})='//&
        ' long(x._t)..d._x._hi-d._x._lo-j ',line)
   call dcl_uproc(parser,&
        'get_disp(d:cycle{range{long}},j:long,x:stride{any_int})='//&
        ' -j..d._x._hi-d._x._lo-j by x._t',line)
   call dcl_uproc(parser,&
        'get_disp(d:cycle{seq{long}},j:long,x:stride{any_int})='//&
        ' -j..d._x._hi-d._x._lo-j by d._x._st*x._t',line)
   call dcl_uproc(parser,'get_disp(d:cycle{range{long}},j:long,x:null)='//&
        ' -j..d._x._hi-d._x._lo-j',line)
   call dcl_uproc(parser,'get_disp(d:cycle{seq{long}},j:long,x:null)='//&
        ' -j..d._x._hi-d._x._lo-j by d._x._st',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:any_int)='//&
        ' max(-j,xx)..min(d._hi-d._lo-j,xx) where xx=long(x)',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:range{any_int})='//&
        ' max(-j,long(x._lo))..min(d._hi-d._lo-j,long(x._hi))',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:seq{any_int})='//&
        ' max(-j,long(x._lo))..min(d._hi-d._lo-j,long(x._hi)) by x._st',line)
   call dcl_uproc(parser,&
        'get_disp(d:range{long},j:long,x:range_below{any_int})='//&
        ' -j..min(d._hi-d._lo-j,long(x._t))',line)
   call dcl_uproc(parser,&
        'get_disp(d:range{long},j:long,x:range_above{any_int})='//&
        ' max(-j,long(x._t))..d._hi-d._lo-j',line)
   call dcl_uproc(parser,&
        'get_disp(d:range{long},j:long,x:strided_range_below{any_int})='//&
        ' -j..min(d._hi-d._lo-j,long(x._t)) by x._s',line)
   call dcl_uproc(parser,&
        'get_disp(d:range{long},j:long,x:strided_range_above{any_int})='//&
        ' max(-j,long(x._t))..d._hi-d._lo-j by x._s',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:stride{any_int})='//&
        ' -j..d._hi-d._lo-j by long(x._t) ',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:null)='//&
        ' -j..d._hi-d._lo-j',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:any_int)'//&
        '=max(-j,xx)..min(d._hi-d._lo-j,xx) where xx=long(x)',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:range{any_int})'//&
        '=max(-j,long(x._lo))..min(d._hi-d._lo-j,long(x._hi))',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:seq{any_int})'//&
        '=max(-j,long(x._lo))..min(d._hi-d._lo-j,long(x._hi)) by x._st',line)
   call dcl_uproc(parser,&
        'get_disp(d:seq{long},j:long,x:range_below{any_int})='//&
        ' -j..min(d._hi-d._lo-j,long(x._t))',line)
   call dcl_uproc(parser,&
        'get_disp(d:seq{long},j:long,x:range_above{any_int})='//&
        ' max(-j,long(x._t))..d._hi-d._lo-j',line)
   call dcl_uproc(parser,&
        'get_disp(d:seq{long},j:long,x:strided_range_below{any_int})='//&
        ' -j..min(d._hi-d._lo-j,long(x._t)) by x._s ',line)
   call dcl_uproc(parser,&
        'get_disp(d:seq{long},j:long,x:strided_range_above{any_int})='//&
        ' max(-j,long(x._t))..d._hi-d._lo-j by x._s',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:stride{any_int})='//&
        ' -j..d._hi-d._lo-j by long(x._t)',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:null)='//&
        ' -j..d._hi-d._lo-j ',line)
   
   call dcl_uproc(parser,'get_disp(x:vector,j,y)=get_disp(x._d,j,y)',line)
   call dcl_uproc(parser,'get_disp(x:matrix,j,y,z)=get_disp(x._d,j,y,z)',line)
   call dcl_uproc(parser,&
        'get_disp(d:grid1d,j:tuple1d,i:tuple1d)=get_disp(d,j.d1,i.d1)',line)
   call dcl_uproc(parser,&
        'get_disp(d:grid1d,j:long,i:tuple1d)=get_disp(d,j,i.d1)',line)
   call dcl_uproc(parser,&
        'get_disp(d:grid2d,j:tuple2d,i:tuple2d)=get_disp(d,j,i.d1,i.d2)',line)
   call dcl_uproc(parser,&
        'get_disp(d:grid3d,j:tuple3d,i:tuple3d)='//&
        'get_disp(d,j,i.d1,i.d2,i.d3)',line)
   call dcl_uproc(parser,&
        'get_disp(d:grid4d,j:tuple4d,i:tuple4d)='//&
        'get_disp(d,j,i.d1,i.d2,i.d3,i.d4)',line)
   call dcl_uproc(parser,&
        'get_disp(d:grid5d,j:tuple5d,i:tuple5d)='//&
        'get_disp(d,j,i.d1,i.d2,i.d3,i.d4,i.d5)',line)
   call dcl_uproc(parser,&
        'get_disp(d:grid6d,j:tuple6d,i:tuple6d)='//&
        'get_disp(d,j,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6)',line)
   call dcl_uproc(parser,&
        'get_disp(d:grid7d,j:tuple7d,i:tuple7d)='//&
        'get_disp(d,j,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6.i.d7)',line)
   call dcl_uproc(parser,&
        'get_disp(d:vector,j:long,i:tuple1d)=get_disp(d,j,i.d1)',line)
   call dcl_uproc(parser,&
        'get_disp(d:vector,j:tuple1d,i:tuple1d)=get_disp(d,j.d1,i.d1)',line)
   call dcl_uproc(parser,&
        'get_disp(d:matrix,j:tuple2d,i:tuple2d)=get_disp(d,j,i.d1,i.d2)',line)
   call dcl_uproc(parser,'get_disp(d,j,x)=grid(get_disp(d.d1,j,x))',line)
   call dcl_uproc(parser,'get_disp(d,j,x,y)='//&
        'grid(get_disp(d.d1,j,x),get_disp(d.d2,j,y))',line)
   call dcl_uproc(parser,&
        'get_disp(d,j,x,y,z)='//&
        'grid(get_disp(d.d1,j,x),get_disp(d.d2,j,y),get_disp(d.d3,j,z))',line)
   call dcl_uproc(parser,&
        'get_disp(d,j,x,y,z,a)=grid(get_disp(d.d1,j,x),get_disp(d.d2,j,y),'//&
        'get_disp(d.d3,j,z),get_disp(d.d4,j,a))',line)
   call dcl_uproc(parser,&
        'get_disp(d,j,x,y,z,a,b)=grid(get_disp(d.d1,j,x),'//&
        'get_disp(d.d2,j,y),get_disp(d.d3,j,z),'//&
         'get_disp(d.d4,j,a),get_disp(d.d5,j,b))',line)
   call dcl_uproc(parser,&
        'get_disp(d,j,x,y,z,a,b,c)=grid(get_disp(d.d1,j,x),'//&
        'get_disp(d.d2,j,y),get_disp(d.d3,j,z),'//&
        'get_disp(d.d4,j,a),get_disp(d.d5,j,b),get_disp(d.d6,j,c))',line)
   call dcl_uproc(parser,&
        'get_disp(d,j,x,y,z,a,b,c,e)=grid(get_disp(d.d1,j,x),'//&
        'get_disp(d.d2,j,y),get_disp(d.d3,j,z),'//&
        'get_disp(d.d4,j,a),get_disp(d.d5,j,b),'//&
        'get_disp(d.d6,j,c),get_disp(d.d7,j,e))',line)
   
   ! Displacement slicing - add halo around tile
   ! Args: domain / tile / displacement slice
   ! Result: new tile
   call dcl_uproc(parser,'_clo(d,x)=max(0l,min(x,size(d)))',line)
   call dcl_uproc(parser,'_chi(d,x)=max(0l,min(x,size(d)-1l))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:range{long},t:range{long},i:any_int)='//&
        '_clo(d,low(t)+long(i)).._chi(d,high(t)+long(i))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{range{long}},t:range{long},i:any_int)='//&
        '_clo(d,low(t)+long(i)).._chi(d,high(t)+long(i))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:seq{long},t:range{long},i:any_int)='//&
        '_clo(d,low(t)+long(i)/step(d)).._chi(d,high(t)+long(i)/step(d))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{seq{long}},t:range{long},i:any_int)='//&
        '_clo(d,low(t)+long(i)/step(d)).._chi(d,high(t)+long(i)/step(d))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:range{long},t:range{long},i:any_seq{any_int})='//&
        '_clo(d,low(t)+low(long(i))).._chi(d,high(t)+high(long(i)))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{range{long}},'//&
        't:range{long},i:any_seq{any_int})='//&
        '_clo(d,low(t)+low(long(i))).._chi(d,high(t)+high(long(i)))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:seq{long},t:range{long},i:any_seq{any_int})='//&
        '_clo(d,low(t)+low(long(i))/step(d))..'//&
        '_chi(d,high(t)+high(long(i))/step(d))',&
        line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{seq{long}},t:range{long},i:any_seq{any_int})='//&
        '_clo(d,low(t)+low(long(i))/step(d))..'//&
        '_chi(d,high(t)+high(long(i))/step(d))',&
        line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:range{long},t:range{long},i:range_below{any_int})='//&
        '0l.._chi(d,high(t)+long(i._t))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{range{long}},t:range{long},'//&
        'i:range_below{any_int})='//&
        '0l.._chi(d,high(t)+long(i._t))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:seq{long},t:range{long},i:range_below{any_int})='//&
        '0l.._chi(d,high(t)+long(i._t)/step(d))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{seq{long}},'//&
        't:range{long},i:range_below{any_int})='//&
        '0l.._chi(d,high(t)+long(i._t)/step(d))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:range{long},t:range{long},i:range_above{any_int})='//&
        '_clo(d,low(t)+long(i._t))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{range{long}},'//&
        't:range{long},i:range_above{any_int})='//&
        '_clo(d,low(t)+long(i._t))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:seq{long},t:range{long},i:range_above{any_int})='//&
        '_clo(d,low(t)+long(i._t)/step(d))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{seq{long}},'//&
        't:range{long},i:range_above{any_int})='//&
        '_clo(d,low(t)+long(i._t)/step(d))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:range{long},'//&
        't:range{long},i:strided_range_below{any_int})='//&
        '0l.._chi(d,high(t)+long(i._t))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{range{long}},t:range{long},'//&
        'i:strided_range_below{any_int})='//&
        '0l.._chi(d,high(t)+long(i._t))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:seq{long},'//&
        't:range{long},i:strided_range_below{any_int})='//&
        '0l.._chi(d,high(t)+long(i._t)/step(d))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{seq{long}},t:range{long},'//&
        'i:strided_range_below{any_int})='//&
        '0l.._chi(d,high(t)+long(i._t)/step(d))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:range{long},'//&
        't:range{long},i:strided_range_above{any_int})='//&
        '_clo(d,low(t)+long(i._t))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{range{long}},t:range{long},'//&
        'i:strided_range_above{any_int})='//&
        '_clo(d,low(t)+long(i._t))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:seq{long},'//&
        't:range{long},i:strided_range_above{any_int})='//&
        '_clo(d,low(t)+long(i._t)/step(d))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:cycle{seq{long}},'//&
        't:range{long},i:strided_range_above{any_int})='//&
        '_clo(d,low(t)+long(i._t)/step(d))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d,t:range{long},i:stride{long})=0..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d,t:range{long},i:null)=0..size(d)-1l',line)
      call dcl_uproc(parser,&
        'get_disp_halo(d:grid1d,j:grid1d,i:tuple1d)='//&
        'grid(get_disp_halo(d.d1,j.d1,i.d1))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:grid1d,j:grid1d,i)='//&
        'grid(get_disp_halo(d.d1,j.d1,i))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:grid2d,j:grid2d,i:tuple2d)='//&
        'get_disp_halo(d,j,i.d1,i.d2)',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:grid3d,j:grid3d,i:tuple3d)='//&
        'get_disp_halo(d,j,i.d1,i.d2,i.d3)',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:grid4d,j:grid4d,i:tuple4d)='//&
        'get_disp_halo(d,j,i.d1,i.d2,i.d3,i.d4)',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:grid5d,j:grid5d,i:tuple5d)='//&
         'get_disp_halo(d,j,i.d1,i.d2,i.d3,i.d4,i.d5)',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:grid6d,j:grid6d,i:tuple6d)='//&
        'get_disp_halo(d,j,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6)',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d:grid7d,j:grid7d,i:tuple7d)='//&
        'get_disp_halo(d,j,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6.i.d7)',line)
   call dcl_uproc(parser,'get_disp_halo(d,j,x,y)='//&
        'grid(get_disp_halo(d.d1,j.d1,x),get_disp_halo(d.d2,j.d2,y))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d,j,x,y,z)='//&
        'grid(get_disp_halo(d.d1,j.d1,x),get_disp_halo(d.d2,j.d2,y),'//&
        'get_disp_halo(d.d3,j.d3,z))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d,j,x,y,z,a)='//&
        'grid(get_disp_halo(d.d1,j.d1,x),get_disp_halo(d.d2,j.d2,y),'//&
        'get_disp_halo(d.d3,j.d3,z),get_disp_halo(d.d4,j.d4,a))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d,j,x,y,z,a,b)=grid(get_disp_halo(d.d1,j.d1,x),'//&
        'get_disp_halo(d.d2,j.d2,y),get_disp_halo(d.d3,j.d3,z),'//&
        'get_disp_halo(d.d4,j.d4,a),get_disp_halo(d.d5,j.d5,b))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d,j,x,y,z,a,b,c)=grid(get_disp_halo(d.d1,j.d1,x),'//&
        'get_disp_halo(d.d2,j.d2,y),get_disp_halo(d.d3,j.d3,z),'//&
        'get_disp_halo(d.d4,j.d4,a),get_disp_halo(d.d5,j.d5,b),'//&
        'get_disp_halo(d.d6,j.d6,c))',line)
   call dcl_uproc(parser,&
        'get_disp_halo(d,j,x,y,z,a,b,c,e)=grid(get_disp_halo(d.d1,j.d1,x),'//&
        'get_disp_halo(d.d2,j.d2,y),get_disp_halo(d.d3,j.d3,z),'//&
        'get_disp_halo(d.d4,j.d4,a),get_disp_halo(d.d5,j.d5,b),'//&
        'get_disp_halo(d.d6,j.d6,c),get_disp_halo(d.d7,j.d7,e))',line)


   ! Is a halo contiguous?
   call dcl_uproc(parser,&
        'halo_intact(d:bd_seq,j:range{long},i:any_int)=true',line)
   call dcl_uproc(parser,&
        'halo_intact(d:bd_seq,j:range{long},i:range{any_int})=true',line)
   call dcl_uproc(parser,&
        'halo_intact(d:range{long},j:range{long},i:seq{any_int})='//&
        'size(j)>=i._st',line)
   call dcl_uproc(parser,&
        'halo_intact(d:seq{long},j:range{long},i:seq{any_int})='//&
        'size(j)*d._st>=i._st',line)
   call dcl_uproc(parser,&
        'halo_intact(d:cycle{},j:any_seq,i:subs)=halo_intact(d._x,j,i)',line)
   call dcl_uproc(parser,'halo_intact(d:grid,j:grid,i,arg...)='//&
        'halo_intact(d,j,tuple(i,arg...))',line)
   call dcl_uproc(parser,'halo_intact(d:grid1d,j:grid1d,i)='//&
        'halo_intact(d.d1,j.d1,i)',line)
   call dcl_uproc(parser,'halo_intact(d:grid1d,j:grid1d,i:tuple1d)='//&
        'halo_intact(d.d1,j.d1,i.d1)',line)
   call dcl_uproc(parser,'halo_intact(d:grid2d,j:grid2d,i:tuple2d)='//&
        'halo_intact(d.d1,j.d1,i.d1) and halo_intact(d.d2,j.d2,i.d2)',line)
   call dcl_uproc(parser,'halo_intact(d:grid3d,j:grid3d,i:tuple3d)='//&
        'halo_intact(d.d1,j.d1,i.d1) and halo_intact(d.d2,j.d2,i.d2) and'//&
        ' halo_intact(d.d3.j.d3,i.d3)',line)
   call dcl_uproc(parser,'halo_intact(d:grid4d,j:grid4d,i:tuple4d)='//&
        'halo_intact(d.d1,j.d1,i.d1) and halo_intact(d.d2,j.d2,i.d2) and'//&
        ' halo_intact(d.d3.j.d3,i.d3) and halo_intact(d.d4,j.d4,i.d4)',line)
   call dcl_uproc(parser,'halo_intact(d:grid5d,j:grid5d,i:tuple5d)='//&
        'halo_intact(d.d1,j.d1,i.d1) and halo_intact(d.d2,j.d2,i.d2) and'//&
        ' halo_intact(d.d3.j.d3,i.d3) and halo_intact(d.d4,j.d4,i.d4) and'//&
        ' halo_intact(d.d5,j.d5,i.d5)',line)
   call dcl_uproc(parser,'halo_intact(d:grid6d,j:grid6d,i:tuple6d)='//&
        'halo_intact(d.d1,j.d1,i.d1) and halo_intact(d.d2,j.d2,i.d2) and'//&
        ' halo_intact(d.d3.j.d3,i.d3) and halo_intact(d.d4,j.d4,i.d4) and'//&
        ' halo_intact(d.d5,j.d5,i.d5) and halo_intact(d.d6,j.d6,i.d6)',line)
   call dcl_uproc(parser,'halo_intact(d:grid7d,j:grid7d,i:tuple7d)='//&
        'halo_intact(d.d1,j.d1,i.d1) and halo_intact(d.d2,j.d2,i.d2) and'//&
        ' halo_intact(d.d3.j.d3,i.d3) and halo_intact(d.d4,j.d4,i.d4) and'//&
        ' halo_intact(d.d5,j.d5,i.d5) and halo_intact(d.d6,j.d6,i.d6) and'//&
        ' halo_intact(d.d7,j.d7,i.d7)',line)
   
   ! Displacement slicing - add anit-halo around tile
   ! Args: domain / tile / displacement slice 
   ! Result: new tile
   call dcl_uproc(parser,&
        'get_anti_halo(d:range{long},t:range{long},i:any_int)='//&
        '_clo(d,low(t)-long(i)).._chi(d,high(t)-long(i))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{range{long}},t:range{long},i:any_int)='//&
        '_clo(d,low(t)-long(i)).._chi(d,high(t)-long(i))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:seq{long},t:range{long},i:any_int)='//&
        '_clo(d,low(t)-long(i)/step(d)).._chi(d,high(d)-long(i)/step(d))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{seq{long}},t:range{long},i:any_int)='//&
        '_clo(d,low(t)-long(i)/step(d)).._chi(d,high(t)-long(i)/step(d))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:range{long},t:range{long},i:any_seq{any_int})='//&
        '_clo(d,low(t)-high(long(i))).._chi(d,high(t)-low(long(i)))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{range{long}},'//&
        't:range{long},i:any_seq{any_int})='//&
        '_clo(d,low(t)-high(long(i))).._chi(d,high(t)-low(long(i)))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:seq{long},'//&
        't:range{long},i:any_seq{any_int})='//&
        '_clo(d,low(t)-high(long(i))/step(d))'//&
        '.._chi(d,high(t)-low(long(i))/step(d))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{seq{long}},'//&
        't:range{long},i:any_seq{any_int})='//&
        '_clo(d,low(t)-high(long(i))/step(d))'//&
        '.._chi(d,high(t)-low(long(i))/step(d))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:range{long},t:range{long},i:range_above{any_int})='//&
        '0l.._chi(d,high(t)-long(i._t))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{range{long}},'//&
        't:range{long},i:range_above{any_int})='//&
        '0l.._chi(d,high(t)-long(i._t))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:seq{long},'//&
        't:range{long},i:range_above{any_int})='//&
        '0l.._chi(d,high(t)-long(i._t)/step(d))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{seq{long}},'//&
        't:range{long},i:range_above{any_int})='//&
        '0l.._chi(d,high(t)-long(i._t)/step(d))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:range{long},t:range{long},i:range_below{any_int})='//&
        '_clo(d,low(t)-long(i._t))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{range{long}},'//&
        't:range{long},i:range_below{any_int})='//&
        '_clo(d,low(t)-long(i._t))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:seq{long},t:range{long},i:range_below{any_int})='//&
        '_clo(d,low(t)-long(i._t)/step(d))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{seq{long}},'//&
        't:range{long},i:range_below{any_int})='//&
        '_clo(d,low(t)-long(i._t)/step(d))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:range{long},'//&
        't:range{long},i:strided_range_above{any_int})='//&
        '0l.._chi(d,high(t)-long(i._t))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{range{long}},t:range{long},'//&
        'i:strided_range_above{any_int})='//&
        '0l.._chi(d,high(t)-long(i._t))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:seq{long},t:range{long},'//&
        'i:strided_range_above{any_int})='//&
        '0l.._chi(d,high(t)-long(i._t)/step(d))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{seq{long}},t:range{long},'//&
        'i:strided_range_above{any_int})='//&
        '0l.._chi(d,high(t)-long(i._t)/step(d))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:range{long},t:range{long},'//&
        'i:strided_range_below{any_int})='//&
        '_clo(d,low(t)-long(i._t))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{range{long}},t:range{long},'//&
        'i:strided_range_below{any_int})='//&
        '_clo(d,low(t)-long(i._t))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:seq{long},t:range{long},'//&
        'i:strided_range_below{any_int})='//&
        '_clo(d,low(t)-long(i._t)/step(d))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:cycle{seq{long}},t:range{long},'//&
        'i:strided_range_below{any_int})='//&
        '_clo(d,low(t)-long(i._t)/step(d))..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d,t:range{long},i:stride{long})=0..size(d)-1l',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d,t:range{long},i:null)=0..size(d)-1l',line)
      call dcl_uproc(parser,&
        'get_anti_halo(d:grid1d,j:grid1d,i:tuple1d)='//&
        'grid(get_anti_halo(d.d1,j.d1,i.d1))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:grid1d,j:grid1d,i)='//&
        'grid(get_anti_halo(d.d1,j.d1,i))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:grid2d,j:grid2d,i:tuple2d)='//&
        'get_anti_halo(d,j,i.d1,i.d2)',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:grid3d,j:grid3d,i:tuple3d)='//&
        'get_anti_halo(d,j,i.d1,i.d2,i.d3)',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:grid4d,j:grid4d,i:tuple4d)='//&
        'get_anti_halo(d,j,i.d1,i.d2,i.d3,i.d4)',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:grid5d,j:grid5d,i:tuple5d)='//&
         'get_anti_halo(d,j,i.d1,i.d2,i.d3,i.d4,i.d5)',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:grid6d,j:grid6d,i:tuple6d)='//&
        'get_anti_halo(d,j,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6)',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d:grid7d,j:grid7d,i:tuple7d)='//&
        'get_anti_halo(d,j,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6.i.d7)',line)
   call dcl_uproc(parser,'get_anti_halo(d,j,x,y)='//&
        'grid(get_anti_halo(d.d1,j.d1,x),get_anti_halo(d.d2,j.d2,y))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d,j,x,y,z)='//&
        'grid(get_anti_halo(d.d1,j.d1,x),get_anti_halo(d.d2,j.d2,y),'//&
        'get_anti_halo(d.d3,j.d3,z))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d,j,x,y,z,a)='//&
        'grid(get_anti_halo(d.d1,j.d1,x),get_anti_halo(d.d2,j.d2,y),'//&
        'get_anti_halo(d.d3,j.d3,z),get_anti_halo(d.d4,j.d4,a))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d,j,x,y,z,a,b)=grid(get_anti_halo(d.d1,j.d1,x),'//&
        'get_anti_halo(d.d2,j.d2,y),get_anti_halo(d.d3,j.d3,z),'//&
        'get_anti_halo(d.d4,j.d4,a),get_anti_halo(d.d5,j.d5,b))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d,j,x,y,z,a,b,c)=grid(get_anti_halo(d.d1,j.d1,x),'//&
        'get_anti_halo(d.d2,j.d2,y),get_anti_halo(d.d3,j.d3,z),'//&
        'get_anti_halo(d.d4,j.d4,a),get_anti_halo(d.d5,j.d5,b),'//&
        'get_anti_halo(d.d6,j.d6,c))',line)
   call dcl_uproc(parser,&
        'get_anti_halo(d,j,x,y,z,a,b,c,e)=grid(get_anti_halo(d.d1,j.d1,x),'//&
        'get_anti_halo(d.d2,j.d2,y),get_anti_halo(d.d3,j.d3,z),'//&
        'get_anti_halo(d.d4,j.d4,a),get_anti_halo(d.d5,j.d5,b),'//&
        'get_anti_halo(d.d6,j.d6,c),get_anti_halo(d.d7,j.d7,e))',line)

   ! Displacement of a point in a domain
   call dcl_uproc(parser,&
        'displace(d:cycle{long},x:long,y:any_int)= '//&
        '(x+long(y)-low(d)) mod (high(d)-low(d)) + low(d)',line)
   call dcl_uproc(parser,'displace(d:range{long},x:long,y:any_int)='//&
        'x+long(y)',line)
   call dcl_uproc(parser,'displace(d:seq{long},x:long,y:any_int)='//&
        'x+long(y)',line)
   call dcl_uproc(parser,'displace(x:vector,y,z)=displace(x._d,y,z)',line)
   call dcl_uproc(parser,'displace(x:matrix,y,z)=displace(x._d,y,z)',line)
   call dcl_uproc(parser,'displace(d:grid1d,x:any_int,y:any_int)='//&
        'displace(d.d1,x,y)',line)
   call dcl_uproc(parser,'displace(d:grid1d,x:tuple1d,y:tuple1d)='//&
        'tuple(displace(d.d1,x.d1,y.d1))',line)
   call dcl_uproc(parser,'displace(d:grid1d,x:any_int,y:tuple1d)='//&
        'tuple(displace(d.d1,x,y.d1))',line)
   call dcl_uproc(parser,'displace(d:grid1d,x:tuple1d,y:any_int)='//&
        'tuple(displace(d.d1,x.d1,y))',line)
    call dcl_uproc(parser,'displace(d:grid2d,x:tuple2d,y:tuple2d)='//&
         'tuple(displace(d.d1,x.d1,y.d1),displace(d.d2,x.d2,y.d2)) ',line)
    call dcl_uproc(parser,'displace(d:grid3d,x:tuple3d,y:tuple3d)='//&
         'tuple(displace(d.d1,x.d1,y.d1),displace(d.d2,x.d2,y.d2), '//&
         'displace(d.d3,x.d3,y.d3) ) ',line)
    call dcl_uproc(parser,'displace(d:grid4d,x:tuple4d,y:tuple4d)='//&
         'tuple(displace(d.d1,x.d1,y.d1),displace(d.d2,x.d2,y.d2), '//&
         'displace(d.d3,x.d3,y.d3),displace(d.d4,x.d4,y.d4) ) ',line)
    call dcl_uproc(parser,'displace(d:grid5d,x:tuple5d,y:tuple5d)='//&
         'tuple(displace(d.d1,x.d1,y.d1),displace(d.d2,x.d2,y.d2), '//&
         'displace(d.d3,x.d3,y.d3),displace(d.d4,x.d4,y.d4),'//&
         'displace(d.d5,x.d5,y.d5) ) ',line)
    call dcl_uproc(parser,'displace(d:grid6d,x:tuple6d,y:tuple6d)='//&
         'tuple(displace(d.d1,x.d1,y.d1),displace(d.d2,x.d2,y.d2), '//&
         'displace(d.d3,x.d3,y.d3),displace(d.d4,x.d4,y.d4),'//&
        'displace(d.d5,x.d5,y.d5),displace(d.d6,x.d6,y.d6) ) ',line)
    call dcl_uproc(parser,'displace(d:grid7d,x:tuple7d,y:tuple7d)='//&
         'tuple( displace(d.d1,x.d1,y.d1),displace(d.d2,x.d2,y.d2), '//&
         'displace(d.d3,x.d3,y.d3),displace(d.d4,x.d4,y.d4),'//&
         'displace(d.d5,x.d5,y.d5),'//&
         'displace(d.d6,x.d6,y.d6),displace(d.d7,x.d7,y.d7) ) ',line)
    call dcl_uproc(parser,'displace(d:grid2d,x:tuple2d,d1:any_int,d2:any_int)='//&
         'tuple(displace(d.d1,x.d1,d1),displace(d.d2,x.d2,d2)) ',line)
    call dcl_uproc(parser,'displace(d:grid3d,x:tuple3d,'//&
         'd1:any_int,d2:any_int,d3:any_int)='//&
         'tuple(displace(d.d1,x.d1,d1),displace(d.d2,x.d2,d2), '//&
         'displace(d.d3,x.d3,d3) ) ',line)
    call dcl_uproc(parser,'displace(d:grid4d,x:tuple4d,d1:any_int,'//&
         'd2:any_int,d3:any_int,d4:any_int)='//&
         'tuple(displace(d.d1,x.d1,d1),displace(d.d2,x.d2,d2), '//&
         'displace(d.d3,x.d3,d3),displace(d.d4,x.d4,d4) ) ',line)
    call dcl_uproc(parser,'displace(d:grid5d,x:tuple5d,d1:any_int,d2:any_int,'//&
         'd3:any_int,d4:any_int,d5:any_int)='//&
         'tuple(displace(d.d1,x.d1,d1),displace(d.d2,x.d2,d2), '//&
         'displace(d.d3,x.d3,d3),displace(d.d4,x.d4,d4),'//&
         'displace(d.d5,x.d5,d5) ) ',line)
    call dcl_uproc(parser,'displace(d:grid6d,x:tuple6d,d1:any_int,'//&
         'd2:any_int,d3:any_int,d4:any_int,d5:any_int,d6:any_int)='//&
         'tuple(displace(d.d1,x.d1,d1),displace(d.d2,x.d2,d2), '//&
         'displace(d.d3,x.d3,d3),displace(d.d4,x.d4,d4),'//&
        'displace(d.d5,x.d5,d5),displace(d.d6,x.d6,d6) ) ',line)
    call dcl_uproc(parser,'displace(d:grid7d,x:tuple7d,d1:any_int,'//&
         'd2:any_int,d3:any_int,d4:any_int,'//&
         'd5:any_int,d6:any_int,d7:any_int)='//&
         'tuple( displace(d.d1,x.d1,d1),displace(d.d2,x.d2,d2), '//&
         'displace(d.d3,x.d3,d3),displace(d.d4,x.d4,d4),'//&
         'displace(d.d5,x.d5,d5),'//&
         'displace(d.d6,x.d6,d6),displace(d.d7,x.d7,d7) ) ',line)
    
    ! Index checking
    call dcl_uproc(parser,'_in(x,y)=low(y)<=x and x<=high(y)',line) 
    call dcl_uproc(parser,'_incl(x,y:any_int)= y in x',line)
    call dcl_uproc(parser,&
         '_incl(x,y:range{any_int})= _in(y._lo,x) and _in(y._hi,x)',line)
    call dcl_uproc(parser,&
         '_incl(x,y:seq{any_int})=  _in(y._lo,x) and _in(y._hi,x)',line)
    call dcl_uproc(parser,&
         '_incl(x,y:range_below{any_int})= _in(y._t,x)',line)
    call dcl_uproc(parser,&
         '_incl(x,y:range_above{any_int})= _in(y._t,x)',line)
    call dcl_uproc(parser,&
         '_incl(x,y:strided_range_below{any_int})='//&
         ' _in(y._t,x) and y._s/=0',line)
    call dcl_uproc(parser,&
         '_incl(x,y:strided_range_above{any_int})='//&
         ' _in(y._t,x) and y._s/=0',line)
    call dcl_uproc(parser,'_incl(x,y:stride{any_int})=y/=0',line)
    call dcl_uproc(parser,'_incl(x,y:null)=true',line)
    
    call dcl_uproc(parser,&
         'contains(g:grid1d,t:grid1d)=g includes t',line)
    call dcl_uproc(parser,&
         'contains(g:grid2d,t:grid2d)=g includes t',line)
    call dcl_uproc(parser,&
         'contains(g:grid3d,t:grid3d)=g includes t',line)
    call dcl_uproc(parser,&
         'contains(g:grid4d,t:grid4d)=g includes t',line)
    call dcl_uproc(parser,&
         'contains(g:grid5d,t:grid5d)=g includes t',line)
    call dcl_uproc(parser,&
         'contains(g:grid6d,t:grid6d)=g includes t',line)
    call dcl_uproc(parser,&
         'contains(g:grid7d,t:grid7d)=g includes t',line)
    
    call dcl_uproc(parser,&
         'contains(g:grid1d,t:tuple1d)=contains(g,t.d1)',line)
    call dcl_uproc(parser,&
         'contains(g:vector,t:tuple1d)=contains(g,t.d1)',line)
    call dcl_uproc(parser,&
         'contains(g:matrix,t:tuple2d)=contains(g,t.d1,t.d2)',line)
    call dcl_uproc(parser,&
         'contains(g:grid2d,t:tuple2d)=contains(g,t.d1,t.d2)',line)
    call dcl_uproc(parser,&
         'contains(g:grid3d,t:tuple3d)=contains(g,t.d1,t.d2,t.d3)',line)
    call dcl_uproc(parser,&
         'contains(g:grid4d,t:tuple4d)=contains(g,t.d1,t.d2,t.d3,t.d4)',line)
    call dcl_uproc(parser,&
         'contains(g:grid5d,t:tuple5d)='//&
         'contains(g,t.d1,t.d2,t.d3,t.d4,t.d5)',line)
    call dcl_uproc(parser,&
         'contains(g:grid6d,t:tuple6d)='//&
         'contains(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6)',line)
    call dcl_uproc(parser,&
         'contains(g:grid7d,t:tuple7d)='//&
         'contains(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6,t.d7)',&
         line)
    call dcl_uproc(parser,'contains(a:grid1d,d1)='//&
         '_incl(a.d1,d1)',line)
    call dcl_uproc(parser,'contains(a:grid2d,d1,d2)='//&
         '_incl(a.d1,d1) and _incl(a.d2,d2)',line)
    call dcl_uproc(parser,'contains(a:grid3d,d1,d2,d3)='//&
         '_incl(a.d1,d1) and _incl(a.d2,d2)'//&
         ' and _incl(a.d3,d3)',line)
    call dcl_uproc(parser,'contains(a:grid4d,d1,d2,d3,d4)='//&
         '_incl(a.d1,d1) and _incl(a.d2,d2)'//&
        ' and _incl(a.d3,d3) and _incl(a.d4,d4)',line)
    call dcl_uproc(parser,'contains(a:grid5d,d1,d2,d3,d4,d5)='//&
         '_incl(a.d1,d1) and _incl(a.d2,d2)'//&
         ' and _incl(a.d3,d3) and _incl(a.d4,d4) and _incl(a.d5,d5) ',line)
    call dcl_uproc(parser,'contains(a:grid6d,d1,d2,d3,d4,d5,d6)='//&
         '_incl(a.d1,d1) and _incl(a.d2,d2)'//&
         ' and _incl(a.d3,d3) and _incl(a.d4,d4) and _incl(a.d5,d5) '//&
         ' and _incl(a.d6,d6) ',line)
    call dcl_uproc(parser,'contains(a:grid7d,d1,d2,d3,d4,d5,d6,d7)='//&
         '_incl(a.d1,d1) and _incl(a.d2,d2)'//&
         ' and _incl(a.d3,d3) and _incl(a.d4,d4) and _incl(a.d5,d5) '//&
        ' and _incl(a.d6,d6) and _incl(a.d7,d7) ',line)
    call dcl_uproc(parser,'contains(a:vector,d1)=_incl(a._d,d1)',line)
    call dcl_uproc(parser,'contains(a:matrix,d1,d2)=contains(a._d,d1,d2)',line)
        
    ! Domain types
    call dcl_type(parser,&
         'dom includes vector,matrix,grid',line)
    call dcl_uproc(parser,'(//)(x:dom,arg...:subs) do '//&
         ' check contains(x,arg...); result=get_slice(x,arg...) endproc',line)
    
   ! Array types
   call dcl_type(parser,'array{e,d}',line)
   call dcl_type(parser,'array{e,d} '//&
        ' also includes e#d,e#_slice{d},_dim{e,d}',line)
   call dcl_type(parser,'array{e,d:grid_base} '//&
        ' also includes e#grid{d},e#_slice{grid{d}},_dim{e,grid{d}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2}'//&
        ' is e#grid{d1,d2},e#_slice2d{grid{d1,d2}},_dim{e,grid{d1,d2}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2,d3}'//&
        ' is e#grid{d1,d2,d3},e#_slice3d{grid{d1,d2,d3}},'//&
        '_dim{e,grid{d1,d2,d3}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2,d3,d4}'//&
        ' is e#grid{d1,d2,d3,d4},e#_slice4d{grid{d1,d2,d3,d4}},'//&
        '_dim{e,grid{d1,d2,d3,d4}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2,d3,d4,d5}'//&
        ' is e#grid{d1,d2,d3,d4,d5},e#_slice5d{grid{d1,d2,d3,d4,d5}},'//&
        '_dim{e,grid{d1,d2,d3,d4,d5}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2,d3,d4,d5,'//&
        'd6}'//&
        ' is e#grid{d1,d2,d3,d4,d5,d6},e#_slice6d{grid{d1,d2,d3,d4,d5,d6}},'//&
        '_dim{e,grid{d1,d2,d3,d4,d5,d6}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2,d3,d4,d5,'//&
        'd6,d7}'//&
        ' is e#grid{d1,d2,d3,d4,d5,d6,d7},'//&
        'e#_slice7d{grid{d1,d2,d3,d4,d5,d6,d7}},'//&
        ' _dim{e,grid{d1,d2,d3,d4,d5,d6,d7}}',line)

   ! Array operations
   call dcl_uproc(parser,'arb(x:any#dom)=PM__PM__get_elem(x,0l)',line)
   call dcl_uproc(parser,'size(x:any#dom)=size(dom(x))',line)
   call dcl_proc(parser,'_array(x:any,y:any,s:any)->dim x,y',&
        op_array,0_pm_i16,line,proc_needs_type,&
        ' ',ftn_dim_array)
   call dcl_proc(parser,&
         '_makearray(x:any,y:any,n:any)->dim x,y',&
         op_make_array,0_pm_i16,line,proc_needs_type,'',ftn_make_array)
   call dcl_proc(parser,&
         '_redim(x:any,y:any)->over x,y',&
         op_redim,0_pm_i16,line,proc_needs_type,'',ftn_redim)
   call dcl_proc(parser,'dom(x:any#dom)->#x',op_get_dom,0_pm_i16,line,0,&
        '',ftn_array_dom)
   call dcl_proc(parser,'_dom(x:any#any)->#x',op_get_dom,0_pm_i16,line,0,&
        '',ftn_array_dom)
   call dcl_uproc(parser,'redim(x:any#any,y)='//&
        '_redim(x,y)'//&
        ' check "New domain does not have same size in redim" :'//&
        ' size(y)==size(dom(x))',&
        line)
   call dcl_proc(parser,'PM__extractelm(x:any)->*x',&
        op_extractelm,0_pm_i16,line,0,'',ftn_extract_array_elem)
   
   call dcl_uproc(parser,'PM__over(x:any#any,y:dom)='//&
        '_redim(x,y) '//&
        ' check "New domain does not conform in over":conform(dom(x),y)',&
        line)
   
   call dcl_uproc(parser,'(//)(a:any#dom,arg...:index) do'//&
        ' check "subscript out of range":contains(dom(a),arg...)'//&
        ' result=PM__PM__get_elem(a,index(dom(a),arg...)) endproc',line)
   
   call dcl_uproc(parser,'(//)(&a:any#dom,v,arg...:index)'//&
        ' do check "subscript out of range":contains(dom(a),arg...);'//&
        ' check "Type mismatch in []=":same_type(arb(a),v);'//&
        ' PM__setaelem(&a,index(dom(a),arg...),v) endproc',line)
   call dcl_uproc(parser,'(//)(&a:any#dom,v,aa:index,arg...:index)'//&
        ' do check "subscript out of range":contains(dom(a),aa,arg...);'//&
        ' check "Type mismatch in []=":same_type(arb(a),v);'//&
        ' PM__setaelem(&a,index(dom(a),aa,arg...),v) endproc',line)

   call dcl_uproc(parser,'(//)(&a:any#dom,v,arg...:subs) '//&
        ' do _ass_s(&a,v,arg...) endproc',line)
   call dcl_uproc(parser,'_ass_s(&a,v,s) do '//&
        ' PM__assign(&a(/s/),v) endproc',line)
   call dcl_uproc(parser,'_ass_s(&a,v,s,arg...) do'//&
        ' PM__assign(&a(/tuple(s,arg...)/),v) endproc',line)

   call dcl_uproc(parser,'PM__assign(&a:any#dom,v:any#dom)'//&
        'do _assign(&a,v) endproc',line)
   call dcl_uproc(parser,'PM__assign_var(&a:any#dom,v:any#dom)'//&
        'do _assign(&a,v) endproc',line)
   
   call dcl_uproc(parser,'PM__assign(&a:any#dom,v) do'//&
        ' for i in a conc do _assign(&i,v) endfor endproc ',line)
   call dcl_uproc(parser,'PM__assign_var(&a:any#dom,v) do'//&
        ' for i in a conc do _assign(&i,v) endfor endproc ',line)

   call dcl_type(parser,'_nest is any#dom',line)
   call dcl_uproc(parser,'PM__assign(&a:_nest#dom,v:any#dom)'//&
        'do _nested_array_assign(&a,v,same_type(arb(a),arb(v))) endproc',&
        line)
   call dcl_uproc(parser,'PM__assign_var(&a:_nest#dom,v:any#dom)'//&
        'do _nested_array_assign(&a,v,same_type(arb(a),arb(v))) endproc',&
        line)
   call dcl_uproc(parser,'_nested_array_assign(&a,v,x:$false) '//&
        'do for i in a conc do _assign(&i,v) endfor endproc ',line)
   call dcl_uproc(parser,'_nested_array_assign(&a,v,x:$true)'//&
        'do check_conform(a,v);_assign(&a,v) endproc',line)
   
   call dcl_uproc(parser,'PM__get_elem(a:any#dom,arg...:index)='//&
        'PM__PM__get_elem(a,index_shape(dom(a),arg...))',line)
   call dcl_uproc(parser,'PM__set_elem(a:any#dom,v,arg...:index)'//&
        'do PM__setaelem(a,index_shape(dom(a),arg...),v) endproc',line)
   
   call dcl_proc(parser,'_make_subref(a:any#dom,any)->*a',&
        op_make_rf,0_pm_i16,line,0,'',ftn_subref)
   call dcl_uproc(parser,'PM__subref(a:any#dom,arg...:index) do '//&
        ' check contains(dom(a),arg...);'//&
        ' result=_make_subref(a,index(dom(a),arg...)) endproc',line)
   call dcl_uproc(parser,'PM__subref(a,arg...:subs)=proc{[]}(a,arg...)',line)
 
   call dcl_proc(parser,'PM__PM__get_elem(x:any#any,y:long)->*x',&
        op_array_get_elem,0_pm_i16,line,0,'',ftn_get_elem)
   call dcl_proc(parser,'PM__setaelem(x:any#any,y:long,z:any)',&
        op_array_set_elem,0_pm_i16,line,0,'',ftn_set_elem)
   call dcl_proc(parser,'PM__setaelem(&x:any#any,y:long,z:any)',&
           op_array_set_elem,0_pm_i16,line,0,'',ftn_set_elem)

   
   ! Grid indexing
   call dcl_uproc(parser,'index(g:grid1d,s1)=index(g.d1,s1)',line)
   call dcl_uproc(parser,'index(g:grid2d,s1,s2)='//&
        'index(g.d1,s1)+size(g.d1)*'//&
        'index(g.d2,s2)',line)
   call dcl_uproc(parser,'index(g:grid3d,s1,s2,s3)='//&
        'index(g.d1,s1)+size(g.d1)*'//&
        '(index(g.d2,s2)+size(g.d2)*'//&
        'index(g.d3,s3))',line)
   call dcl_uproc(parser,&
        'index(g:grid4d,s1,s2,s3,s4)='//&
        ' index(g.d1,s1)+size(g.d1)*'//&
        ' (index(g.d2,s2)+size(g.d2)*'//&
        ' (index(g.d3,s3)+size(g.d3)*'//&
        ' index(g.d4,s4)))',line)
   call dcl_uproc(parser,&
        'index(g:grid5d,s1,s2,s3,s4,s5)='//&
        ' index(g.d1,s1)+size(g.d1)*'//&
        ' (index(g.d2,s2)+size(g.d2)*'//&
        ' (index(g.d3,s3)+size(g.d3)*'//&
        ' (index(g.d4,s4)+size(g.d4)*'//&
        ' index(g.d5,s5))))',line)
   call dcl_uproc(parser,&
        'index(g:grid6d,s1,s2,s3,s4,s5,s6)='//&
        ' index(g.d1,s1)+size(g.d1)*'//&
        ' (index(g.d2,s2)+size(g.d2)*'//&
        ' (index(g.d3,s3)+size(g.d3)*'//&
        ' (index(g.d4,s4)+size(g.d4)*'//&
        ' (index(g.d5,s5)+size(g.d5)*'//&
        ' index(g.d6,s6)))))',line)
   call dcl_uproc(parser,&
        'index(g:grid7d,s1,s2,s3,s4,'//&
        's5,s6,s7)='//&
        ' index(g.d1,s1)+size(g.d1)*'//&
        ' (index(g.d2,s2)+size(g.d2)*'//&
        ' (index(g.d3,s3)+size(g.d3)*'//&
        ' (index(g.d4,s4)+size(g.d4)*'//&
        ' (index(g.d5,s5)+size(g.d5)*'//&
        ' (index(g.d6,s6)+size(g.d6)*'//&
        ' (index(g.d7,s7)))))))',line)
   
   call dcl_uproc(parser,'index(x:vector,y)=index(x._d,y)',line)
   call dcl_uproc(parser,'index(x:matrix,y)=index(x._d,y)',line)
   call dcl_uproc(parser,'index(x:matrix,y,z)=index(x._d,y,z)',line)
   
   ! Tuple index of grid
   call dcl_uproc(parser,&
        'index(g:grid1d,t:tuple1d)=index(g,t.d1)',line)
   call dcl_uproc(parser,&
        'index(g:grid2d,t:tuple2d)=index(g,t.d1,t.d2)',line)
   call dcl_uproc(parser,&
        'index(g:grid3d,t:tuple3d)=index(g,t.d1,t.d2,t.d3)',line)
   call dcl_uproc(parser,&
        'index(g:grid4d,t:tuple4d)=index(g,t.d1,t.d2,t.d3,t.d4)',line)
   call dcl_uproc(parser,&
        'index(g:grid5d,t:tuple5d)=index(g,t.d1,t.d2,t.d3,t.d4,t.d5)',line)
   call dcl_uproc(parser,&
        'index(g:grid6d,t:tuple6d)=index(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6)',&
        line)
   call dcl_uproc(parser,&
        'index(g:grid7d,t:tuple7d)='//&
        'index(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6,t.d7)',&
        line)

   ! Grid indexing (zero base,unit stride)
   call dcl_uproc(parser,&
        'index_shape(x:vector,s1)=s1',line)
   call dcl_uproc(parser,&
        'index_shape(x:matrix,s1,s2)=index_shape(x._d,s1,s2)',line)
   
   ! Tuple index of grid (zero base,unit stride)
   call dcl_uproc(parser,&
        'index_shape(g:grid1d,t:tuple1d)=index_shape(g,t.d1)',line)
   call dcl_uproc(parser,&
        'index_shape(g:grid2d,t:tuple2d)=index_shape(g,t.d1,t.d2)',line)
   call dcl_uproc(parser,&
        'index_shape(g:grid3d,t:tuple3d)='//&
        'index_shape(g,t.d1,t.d2,t.d3)',line)
   call dcl_uproc(parser,&
        'index_shape(g:grid4d,t:tuple4d)='//&
        'index_shape(g,t.d1,t.d2,t.d3,t.d4)',line)
   call dcl_uproc(parser,&
        'index_shape(g:grid5d,t:tuple5d)='//&
        'index_shape(g,t.d1,t.d2,t.d3,t.d4,t.d5)',line)
   call dcl_uproc(parser,&
        'index_shape(g:grid6d,t:tuple6d)='//&
        'index_shape(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6)',&
        line)
   call dcl_uproc(parser,&
        'index_shape(g:grid7d,t:tuple7d)='//&
        'index_shape(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6,t.d7)',line)
   call dcl_uproc(parser,&
        'index_shape(g:vector,t:tuple1d)=index_shape(g,t.d1)',line)
   call dcl_uproc(parser,&
        'index_shape(g:matrix,t:tuple2d)=index_shape(g,t.d1,t.d2)',line)
   call dcl_uproc(parser,'index_shape(g:grid1d,s1)=s1',line)
   call dcl_uproc(parser,'index_shape(g:grid2d,s1,s2)='//&
        's1+size(g.d1)*'//&
        's2',line)
   call dcl_uproc(parser,'index_shape(g:grid3d,s1,s2,s3)='//&
        's1+size(g.d1)*'//&
        '(s2+size(g.d2)*'//&
        's3)',line)
   call dcl_uproc(parser,&
        'index_shape(g:grid4d,s1,s2,s3,s4)='//&
        ' s1+size(g.d1)*'//&
        ' (s2+size(g.d2)*'//&
        ' (s3+size(g.d3)*'//&
        ' s4))',line)
   call dcl_uproc(parser,&
        'index_shape(g:grid5d,s1,s2,s3,s4,s5)='//&
        ' s1+size(g.d1)*'//&
        ' (s2+size(g.d2)*'//&
        ' (s3+size(g.d3)*'//&
        ' (s4+size(g.d4)*'//&
        ' s5)))',line)
   call dcl_uproc(parser,&
        'index_shape(g:grid6d,s1,s2,s3,s4,s5,s6)='//&
        ' s1+size(g.d1)*'//&
        ' (s2+size(g.d2)*'//&
        ' (s3+size(g.d3)*'//&
        ' (s4+size(g.d4)*'//&
        ' (s5+size(g.d5)*'//&
        ' s6))))',line)
   call dcl_uproc(parser,&
        'index_shape(g:grid7d,s1,s2,s3,s4,'//&
        's5,s6,s7)='//&
        ' s1+size(g.d1)*'//&
        ' (s2+size(g.d2)*'//&
        ' (s3+size(g.d3)*'//&
        ' (s4+size(g.d4)*'//&
        ' (s5+size(g.d5)*'//&
        ' (s6+size(g.d6)*'//&
        ' (s7))))))',line)

    ! Slices
    call dcl_type(parser,'_slice{d} is rec _slice{_s,_d:d,_dm}',line)
    call dcl_uproc(parser,'arb(x:any#_slice{})=PM__PM__get_elem(x,0l)',line)
    call dcl_uproc(parser,'size(x:any#_slice{})=size(_dom(x)._d)',line)
    call dcl_uproc(parser,'dom(x:any#_slice{})=_dom(x)._d',line)
    call dcl_uproc(parser,'redim(x:any#_slice{},y:dom) '//&
         'do a:=x;result=redim(a,y) endproc',line)
    call dcl_uproc(parser,'PM__over(x:any#_slice{},y:dom) '//&
         'do a:=x;result=redim(a,y) endproc',line)
    
    call dcl_uproc(parser,&
         '(//)(a:any#dom,arg...:slice) do'//&
         ' result=_redim(a,rec _slice{_s=_sliceit(dm,arg...),_d=d,_dm=dm}) '//&
         ' check "slice out of range": contains(dm,arg...) '//&
         ' where d=get_slice(dm,arg...) where dm=dom(a) endproc',line)
    call dcl_uproc(parser,&
         '(//)(a:any#dom,arg...:subs)='//&
         ' _redim(a,'//&
         '   rec _slice{_s=_sliceit(dm,arg...),_d=_shrinkit(d,arg...),_dm=dm})'//&
         ' check "slice out of range": contains(dm,arg...)'//&
         ' where d=get_slice(dm,arg...) where dm=dom(a)',line)
    call dcl_uproc(parser,&
         '(//)(a:any#_slice{},arg...:index) do '//&
         ' t:=_dom(a);check "subscript out of range": contains(t._d,arg...); '//&
         ' result=PM__PM__get_elem(a,index(t._s,t._dm,arg...)) endproc',&
         line)
    call dcl_uproc(parser,&
         '(//)(a:any#_slice{},arg...:slice)='//&
         '_redim(a,rec _slice{_s=_reslice(t._s,t._d,arg...),_d=d,_dm=t._dm}) '//&
         'check "slice out of range": contains(t._d,arg...) '//&
         'where d=get_slice(t._d,arg...) where t=_dom(a)',line)
    call dcl_uproc(parser,&
         '(//)(a:any#_slice{},arg...:subs)= '//&
         ' _redim(a,rec _slice{_s=_reslice(t._s,t._d,arg...),'//&
         ' _d=_shrinkit(get_slice(t._d,arg...),arg...),_dm=t._dm})'//&
         ' check "subscript out of range": contains(t._d,arg...) '//&
         ' where t=_dom(a)',line)    
    call dcl_uproc(parser,&
         '(//)(&a:any#_slice{},v,arg...:index) do'//&
         ' t:=_dom(a);check "subscript out of range":contains(t._d,arg...);'//&
         ' check "Type mismatch in []=":same_type(arb(a),v);'//&
         ' PM__setaelem(a,index(t._s,t._dm,arg...),v) endproc',line)
 
    call dcl_uproc(parser,'PM__assign(&a:any#_slice{},b) do'//&
         ' for i in a conc do i=b endfor endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:any#_slice{},b) do'//&
         ' for i in a conc do i=b endfor endproc',line)
        call dcl_uproc(parser,'PM__assign(&a:any#_slice{},b:any#any) do'//&
         ' for i in a,j in b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:any#_slice{},b:any#any) do'//&
         ' for i in a,j in b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign(&a:any#dom,b:any#_slice{}) do'//&
         ' for i in a, j in b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:any#dom,b:any#_slice{}) do'//&
         ' for i in a ,j in b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign(&a:any#_slice{},b:any#_slice{}) do'//&
         ' for i in a, j in b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:any#_slice{},b:any#_slice{}) do'//&
         ' for i in a, j in b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign(&a:_nest#dom,b:any#_slice{}) do'//&
         ' _assign_a_slice_nested(&a,b,same_type(arb(a),arb(b))) '//&
         'endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:_nest#dom,b:any#_slice{}) do'//&
         ' _assign_a_slice_nested(&a,b,same_type(arb(a),arb(b))) '//&
         'endproc',line)
    call dcl_uproc(parser,'PM__assign(&a:_nest#_slice{},b:any#_slice{}) do'//&
         ' _assign_a_slice_nested(&a,b,same_type(arb(a),arb(b))) '//&
         'endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:_nest#_slice{},b:_slice{}) '//&
         'do'//&
         ' _assign_a_slice_nested(&a,b,same_type(arb(a),arb(b))) '//&
         'endproc',line)
    call dcl_uproc(parser,'_assign_a_slice_nested(&a,b,v:$false)'//&
         ' do for i in a conc do i=b endfor endproc',line)
    call dcl_uproc(parser,'_assign_a_slice_nested(&a,b,v:$true)'//&
         ' do for i in a,j in b conc do i=j endfor endproc',line)
    
    call dcl_uproc(parser,'PM__dup(x:any#_slice{}) '//&
         ' do y:=arb(x) dim dom(x);y=x;result=y endproc',line)
    call dcl_uproc(parser,'PM__pdup(x:any#_slice{}) '//&
         ' do y:=arb(x) dim dom(x);y=x;result=y endproc',line)
    
    call dcl_uproc(parser,'PM__get_elem(a:any#_slice{},arg...:index)='//&
         'PM__PM__get_elem(a,index(t._s,t._dm,PM__get_elem(t._d,arg...))) '//&
         '  where t=_dom(a)',line)
    call dcl_uproc(parser,'PM__set_elem(a:any#_slice{},v,arg...:index) do '//&
         'PM__setaelem(a,index(t._s,t._dm,PM__get_elem(t._d,arg...)),v) '//&
         '  where t=_dom(a) endproc',line)
 
    call dcl_uproc(parser,'PM__subref(a:any#_slice{},arg...:index) do '//&
        ' t:=_dom(a);check contains(t._d,arg...);'//&
        ' result=_make_subref(a,index(t._s,t._d,arg...))  endproc',line)
    
    call dcl_type(parser,'_slice1d is rec _slice1d{o,s1}',line)
    call dcl_type(parser,'_slice2d is rec _slice2d{o,s1,s2}',line)
    call dcl_type(parser,'_slice3d is rec _slice3d{o,s1,s2,s3}',line)
    call dcl_type(parser,'_slice4d is rec _slice4d{o,s1,s2,s3,s4}',line)
    call dcl_type(parser,'_slice5d is rec _slice5d{o,s1,s2,s3,s4,s5}',line)
    call dcl_type(parser,&
         '_slice6d is rec _slice6d{o,s1,s2,s3,s4,s5,s6}',&
         line)
    call dcl_type(parser,&
         '_slice7d is rec _slice7d{o,s1,s2,s3,s4,s5,s6,s7}',&
         line)

    ! Index of element in slice
    call dcl_uproc(parser,&
         'index(g:_slice1d,z,t:tuple1d)=index(g,z,t.d1)',line)
    call dcl_uproc(parser,&
         'index(g:_slice2d,z,t:tuple2d)=index(g,z,t.d1,t.d2)',line)
    call dcl_uproc(parser,&
         'index(g:_slice3d,z,t:tuple3d)=index(g,z,t.d1,t.d2,t.d3)',line)
    call dcl_uproc(parser,&
         'index(g:_slice4d,z,t:tuple4d)=index(g,z,t.d1,t.d2,t.d3,t.d4)',line)
    call dcl_uproc(parser,&
         'index(g:_slice5d,z,t:tuple5d)=index(g,z,t.d1,t.d2,t.d3,t.d4,t.d5)',&
         line)
    call dcl_uproc(parser,&
         'index(g:_slice6d,z,t:tuple6d)='//&
         'index(g,z,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6)',line)
    call dcl_uproc(parser,&
         'index(g:_slice7d,z,t:tuple7d)='//&
         'index(g,z,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6,t.d7)',line)    
    call dcl_uproc(parser,'index(x:_slice1d,z,y)=x.o+x.s1*index(z.d1,y)',line)
    call dcl_uproc(parser,&
         'index(x:_slice2d,z,y1,y2)='//&
         'x.o+x.s1*index(z.d1,y1)+x.s2*index(z.d2,y2)',line)
    call dcl_uproc(parser,'index(x:_slice3d,z,y1,y2,y3)=x.o'//&
         '+x.s1*index(z.d1,y1)+x.s2*index(z.d2,y2)+x.s3*index(z.d3,y3)',line)
    call dcl_uproc(parser,'index(x:_slice4d,z,y1,y2,y3,y4)=x.o'//&
         '+x.s1*index(z.d1,y1)+x.s2*index(z.d2,y2)+'//&
         'x.s3*index(z.d3,y3)+x.s4*index(z.d4,y4)',line)
    call dcl_uproc(parser,'index(x:_slice5d,z,y1,y2,y3,y4,y5)=x.o'//&
         '+x.s1*index(z.d1,y1)+x.s2*index(z.d2,y2)+'//&
         'x.s3*index(z.d3,y3)+x.s4*index(z.d4,y4)+x.s5*index(z.d5,y5)',line)
    call dcl_uproc(parser,'index(x:_slice6d,z,y1,y2,y3,y4,y5,y6)=x.o'//&
         '+x.s1*index(z.d1,y1)+x.s2*index(z.d2,y2)+'//&
         'x.s3*index(z.d3,y3)+x.s4*index(z.d4,y4)+x.s5*index(z.d5,y5)'//&
         '+x.s6*index(z.d6,y6)',line)
    call dcl_uproc(parser,'index(x:_slice7d,z,y1,y2,y3,y4,y5,y6,y7)=x.o'//&
         '+x.s1*index(z.d1,y1)+x.s2*index(z.d2,y2)+'//&
         'x.s3*index(z.d3,y3)+x.s4*index(z.d4,y4)+x.s5*index(z.d5,y5)'//&
         '+x.s6*index(z.d6,y6)+x.s7*index(z.d7,y7)',line)

    call dcl_uproc(parser,'_slice(o,s1)=rec _slice1d{o=o,s1=s1}',line)
    call dcl_uproc(parser,&
         '_slice(o,s1,s2)=rec _slice2d{o=o,s1=s1,s2=s2}',line)
    call dcl_uproc(parser,'_slice(o,s1,s2,s3)='//&
         'rec _slice3d{o=o,s1=s1,s2=s2,s3=s3}',line)
    call dcl_uproc(parser,'_slice(o,s1,s2,s3,s4)='//&
         'rec _slice4d{o=o,s1=s1,s2=s2,s3=s3,s4=s4}',line)
    call dcl_uproc(parser,'_slice(o,s1,s2,s3,s4,s5)='//&
         'rec _slice5d{o=o,s1=s1,s2=s2,s3=s3,s4=s4,s5=s5}',line)
    call dcl_uproc(parser,'_slice(o,s1,s2,s3,s4,s5,s6)='//&
         'rec _slice6d{o=o,s1=s1,s2=s2,s3=s3,s4=s4,'//&
         's5=s5,s6=s6}',line)
    call dcl_uproc(parser,'_slice(o,s1,s2,s3,s4,s5,s6,s7)='//&
         'rec _slice7d{o=o,s1=s1,s2=s2,s3=s3,s4=s4,'//&
         's5=s5,s6=s6,s7=s7}',line)

    call dcl_uproc(parser,'_shr(a,c)=a',line)
    call dcl_proc(parser,'_shr(a:any,b:index)->PM__tinyint',&
         op_miss_arg,0_pm_i16,line,0,'',ftn_miss_arg)
    call dcl_uproc(parser,'_doslice(arg...)=_slice(arg...)',line)
    call dcl_uproc(parser,'_shl(d,m,i:index)=index(d,i)*m',line)
    call dcl_uproc(parser,'_shl(d,m,i)=0l',line)
    call dcl_uproc(parser,'_shx(d,i)=_sliceit(d,i)',line)
    call dcl_proc(parser,'_shx(d:any,i:index)->PM__tinyint',&
         op_miss_arg,0_pm_i16,line,0,'',ftn_miss_arg)

    call dcl_uproc(parser,&
         '_sliceit(d:grid1d,i:grid1d)=_sliceit(d,i.d1)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid2d,i:grid2d)=_sliceit(d,i.d1,i.d2)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid3d,i:grid3d)=_sliceit(d,i.d1,i.d2,i.d3)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid4d,i:grid4d)=_sliceit(d,i.d1,i.d2,i.d3,i.d4)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid5d,i:grid5d)=_sliceit(d,i.d1,i.d2,i.d3,i.d4,i.d5)',&
         line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid6d,i:grid6d)='//&
         '_sliceit(d,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6)',&
         line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid7d,i:grid7d)='//&
         '_sliceit(d,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6.i.d7)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid1d,i:tuple1d)=_sliceit(d,i.d1)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid2d,i:tuple2d)=_sliceit(d,i.d1,i.d2)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid3d,i:tuple3d)=_sliceit(d,i.d1,i.d2,i.d3)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid4d,i:tuple4d)=_sliceit(d,i.d1,i.d2,i.d3,i.d4)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid5d,i:tuple5d)=_sliceit(d,i.d1,i.d2,i.d3,i.d4,i.d5)',&
         line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid6d,i:tuple6d)='//&
         '_sliceit(d,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid7d,i:tuple7d)='//&
         '_sliceit(d,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6.i.d7)',line)

    call dcl_uproc(parser,&
         '_sliceit(d:vector,i1)=_doslice(index(d.d1,low(i1)),1l)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:vector,i1:tuple1d)=_doslice(index(d.d1,low(i1.d1)),1l)',&
         line)
    call dcl_uproc(parser,&
         '_sliceit(d:matrix,i1,i2)=_sliceit(d._d,i1,i2)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:matrix,i1:tuple2d)=_sliceit(d._d,i1.d1,i1.d2)',line)

    call dcl_uproc(parser,&
         '_sliceit(d:grid1d,i1)=_doslice(0l,1l)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid2d,i1,i2)='//&
         ' _doslice(_shl(d.d1,1l,i1)+_shl(d.d2,m2,i2),'//&
         ' _shr(1l,i1),_shr(m2,i2)) where m2=size(d.d1)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid3d,i1,i2,i3)='//&
         ' _doslice(_shl(d.d2,1l,i1)+_shl(d.d3,m2,i2)+_shl(d.d3,m3,i3),'//&
         ' _shr(1l,i1),_shr(m2,i2),'//&
         ' _shr(m3,i3))'//&
         ' where m3=size(d.d2)*m2 where m2=size(d.d1)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid4d,i1,i2,i3,i4)=_doslice('//&
         ' _shl(d.d1,1l,i1)+_shl(d.d2,m2,i2)+'//&
         ' _shl(d.d3,m3,i3)+_shl(d.d4,m4,i4),'//&
         ' _shr(1l,i1),_shr(m2,i2),'//&
         ' _shr(m3,i3),_shr(m4,i4))'//&
         ' where m4=size(d.d3)*m3'//&
         ' where m3=size(d.d2)*m2 where m2=size(d.d1)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid5d,i1,i2,i3,i4,i5)=_doslice('//&
         ' _shl(d.d1,1l,i1)+_shl(d.d2,m2,i2)+_shl(d.d3,m3,i3)+'//&
         ' _shl(d.d4,m4,i4)+_shl(d.d5,m5,i5),'//&
         ' _shr(1l,i1),_shr(m2,i2),'//&
         ' _shr(m3,i3),_shr(m4,i4),_shr(m5,i5))'//&
         ' where m5=size(d.d4)*m4 where m4=size(d.d3)*m3 '//&
         ' where m3=size(d.d2)*m2 where m2=size(d.d1)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid6d,i1,i2,i3,i4,i5,i6)=_doslice('//&
         ' _shl(d.d1,1l,i1)+_shl(d.d2,m2,i2)+_shl(d.d3,m3,i3)+'//&
         ' _shl(d.d4,m4,i4)+_shl(d.d5,m5,i5)+_shl(d.d6,i6),'//&
         ' _shr(1l,i1),_shr(m2,i2),'//&
         ' _shr(m3,i3),_shr(m4,i4),'//&
         ' _shr(m5,i5),_shr(m6,i6))'//&
         ' where m6=size(d.d5)*m5 where m5=size(d.d4)*m4 '//&
         ' where m4=size(d.d3)*m3'//&
         ' where m3=size(d.d2)*m2 where m2=size(d.d1)',line)
    call dcl_uproc(parser,&
         '_sliceit(d:grid7d,i1,i2,i3,i4,i5,i6,i7)=_doslice('//&
         ' _shl(d.d1,1l,i1)+_shl(d.d2,m2,i2)+_shl(d.d3,m3,i3)+'//&
         ' _shl(d.d4,m4,i4)+_shl(d.d5,m5,i5)+_shl(d.d6,i6)+_shl(d.d7,i7),'//&
         ' _shr(1l,i1),_shr(m2,i2),'//&
         ' _shr(m3,i3),_shr(m4,i4),'//&
         ' _shr(m5,i5),_shr(m6,i6),_shr(m7,i7))'//&
         ' where m7=size(d.d6)*m6 where m6=size(d.d5)*m5 '//&
         ' where m5=size(d.d4)*m4 where m4=size(d.d3)*m3'//&
         ' where m3=size(d.d2)*m2 where m2=size(d.d1)',line)

    call dcl_uproc(parser,&
         '_reslice(a:_slice1d,d,i1)=_doslice(a.o+_shl(d.d1,a.s1,i1),a.s1)',line)
    call dcl_uproc(parser,&
         '_reslice(a:_slice2d,d,i1,i2)=_doslice(a.o'//&
         '+_shl(d.d1,a.s1,i1)+_shl(d.d2,a.s2,i2),'//&
         '_shr(a.s1,i1),_shr(a.s2,i2))',line)
    call dcl_uproc(parser,&
         '_reslice(a:_slice3d,d,i1,i2,i3)=_doslice(a.o+'//&
         '_shl(d.d1,a.s1,i1)+_shl(d.d2,a.s2,i2)+_shl(d.d3,a.s3,i3),'//&
         '_shr(a.s1,i1),_shr(a.s2,i2),'//&
         '_shr(a.s3,i3))',line)
    call dcl_uproc(parser,&
         '_reslice(a:_slice4d,d,i1,i2,i3,i4)=_doslice(a.o+'//&
         '_shl(d.d1,a.s1,i1)+_shl(d.d2,a.s2,i2)+_shl(d.d3,a.s3,i3)+'//&
         '_shl(d.d4,a.s4,i4),'//&
         '_shr(a.s1,i1),'//&
         '_shr(a.s2,i2),_shr(a.s3,i3),'//&
         '_shr(a.s4,i4))',line)
    call dcl_uproc(parser,&
         '_reslice(a:_slice4d,d,i1,i2,i3,i4,i5)=_doslice(a.o+'//&
         '_shl(d.d1,a.s1,i1)+_shl(d.d2,a.s2,i2)+_shl(d.d3,a.s3,i3)+'//&
         '_shl(d.d4,a.s4,i4)+_shl(d.d5,a.s5,i5),'//&
         '_shr(a.s1,i1),_shr(a.s2,i2),'//&
         '_shr(a.s3,i3),_shr(a.s4,i4),_shr(a.s5,i5))',line)
    call dcl_uproc(parser,&
         '_reslice(a:_slice4d,d,i1,i2,i3,i4,i5,i6)=_doslice(a.o+'//&
         '_shl(d.d1,a.s1,i1)+_shl(d.d2,a.s2,i2)+'//&
         '_shl(d.d3,a.s3,i3)+_shl(d.d4,a.s4,i4)+'//&
         '_shl(d.d5,a.s5,i5)+_shl(d.d6,a.s6,i6),'//&
         '_shr(a.s1,i1),_shr(a.s2,i2),'//&
         '_shr(a.s3,i3),'//&
         '_shr(a.s4,i4),_shr(a.s5,i5),'//&
         '_shr(a.s6,i6))',line)
    call dcl_uproc(parser,&
         '_reslice(a:_slice4d,d,i1,i2,i3,i4,i5,i6,i7)=_doslice(a.o+'//&
         '_shl(d.d1,a.s1,i1)+_shl(d.d2,a.s2,i2)+_shl(d.d3,a.s3,i3)+'//&
         '_shl(d.d4,a.s4,i4)+_shl(d.d5,a.s5,i5)+_shl(d.d6,a.s6,i6)+'//&
         '_shl(d.d7,a.s7,i7),'//&
         '_shr(a.s1,i1),_shr(a.s2,i2),'//&
         '_shr(a.s3,i3),'//&
         '_shr(a.s4,i4),_shr(a.s5,i5),'//&
         '_shr(a.s6,i6)//'//&
         '_shr(a.s7,i7))',line)
    
    call dcl_uproc(parser,'_grid(arg...)=grid(arg...)',line)
    call dcl_uproc(parser,'_shrink(g:grid1d,t)=g',line)
    call dcl_uproc(parser,&
         '_shrink(g:grid2d,t1,t2)=_grid(_shr(g.d1,t1),_shr(g.d2,t2))',line)
    call dcl_uproc(parser,&
         '_shrink(g:grid3d,t1,t2,t3)='//&
         '_grid(_shr(g.d1,t1),_shr(g.d2,t2),_shr(g.d3,t3))',line)
    call dcl_uproc(parser,&
         '_shrink(g:grid4d,t1,t2,t3,t4)=_grid(_shr(g.d1,t1),_shr(g.d2,t2),'//&
         '_shr(g.d3,t3),_shr(g.d4,t4))',line)
    call dcl_uproc(parser,&
         '_shrink(g:grid5d,t1,t2,t3,t4,t5)=_grid(_shr(g.d1,t1),'//&
         '_shr(g.d2,t2),'//&
         '_shr(g.d3,t3),_shr(g.d4,t4),_shr(g.d5,t5))',line)
    call dcl_uproc(parser,&
         '_shrink(g:grid6d,t1,t2,t3,t4,t5,t6)='//&
         '_grid(_shr(g.d1,t1),_shr(g.d2,t2),'//&
         '_shr(g.d3,t3),_shr(g.d4,t4),_shr(g.d5,t5),_shr(g.d6,t6))',line)
    call dcl_uproc(parser,&
         '_shrink(g:grid7d,t1,t2,t3,t4,t5,t6,t7)='//&
         '_grid(_shr(g.d1,t1),_shr(g.d2,t2),'//&
         '_shr(g.d3,t3),_shr(g.d4,t4),_shr(g.d5,t5),'//&
         '_shr(g.d6,t6),_shr(g.d7,t7))',line)
    
    call dcl_uproc(parser,&
         '_shrink(g:matrix,t1:index,t2:slice)=vector(g._d.d2)',line)
    call dcl_uproc(parser,&
         '_shrink(g:matrix,t1:slice,t2:index)=vector(g._d.d1)',line)
    call dcl_uproc(parser,&
         '_shrink(g:matrix,t1:slice,t2:slice)=g',line)
    
    call dcl_uproc(parser,'_shy(a,c)=get_slice(a,c)',line)
    call dcl_proc(parser,&
         '_shy(a:any,b:index)->PM__tinyint',op_miss_arg,0_pm_i16,line,0,&
         '',ftn_miss_arg)
    call dcl_uproc(parser,&
         '_shrinkit(g:grid2d,t1,t2)=_grid(_shy(g.d1,t1),_shy(g.d2,t2))',line)
    call dcl_uproc(parser,&
         '_shrinkit(g:grid3d,t1,t2,t3)='//&
         '_grid(_shy(g.d1,t1),_shy(g.d2,t2),_shy(g.d3,t3))',line)
    call dcl_uproc(parser,&
         '_shrinkit(g:grid4d,t1,t2,t3,t4)=_grid(_shy(g.d1,t1),_shy(g.d2,t2),'//&
         '_shy(g.d3,t3),_shy(g.d4,t4))',line)
    call dcl_uproc(parser,&
         '_shrinkit(g:grid5d,t1,t2,t3,t4,t5)='//&
         '_grid(_shy(g.d1,t1),_shy(g.d2,t2),'//&
         '_shy(g.d3,t3),_shy(g.d4,t4),_shy(g.d5,t5))',line)
    call dcl_uproc(parser,&
         '_shrinkit(g:grid6d,t1,t2,t3,t4,t5,t6)='//&
         '_grid(_shy(g.d1,t1),_shy(g.d2,t2),'//&
         '_shy(g.d3,t3),_shy(g.d4,t4),_shy(g.d5,t5),_shy(g.d6,t6))',line)
    call dcl_uproc(parser,&
         '_shrinkit(g:grid7d,t1,t2,t3,t4,t5,t6,t7)='//&
         '_grid(_shy(g.d1,t1),_shy(g.d2,t2),'//&
         '_shy(g.d3,t3),_shy(g.d4,t4),_shy(g.d5,t5),'//&
         '_shy(g.d6,t6),_shy(g.d7,t7))',line)
    
    call dcl_uproc(parser,&
         '_shrinkit(g:matrix,t1:index,t2:slice)='//&
         'vector(get_slice(g._d.d2,t2))',line)
    call dcl_uproc(parser,&
         '_shrinkit(g:matrix,t1:slice,t2:index)='//&
         'vector(get_slice(g._d.d1,t1))',line)
    call dcl_uproc(parser,&
         '_shrinkit(g:matrix,t1:slice,t2:slice)='//&
         'matrix(get_slice(g._d.d1,t1),get_slice(g._d.d2,t2))',line)

    ! Dimensions
    call dcl_type(parser,'_dim{a,d} is rec _dim{_a:a,_d:d}',line)
    call dcl_uproc(parser,'arb(a:_dim{,})=a._a',line)
    call dcl_uproc(parser,'dom(a:_dim{,})=a._d',line)
    call dcl_uproc(parser,'size(a:_dim{,})=size(a._d)',line)
    call dcl_uproc(parser,'redim(a:_dim{,},d:dom)=rec _dim{ _a=a,_d=d}'//&
         ' check "New domain does not have same size in redim":'//&
         'size(d)==size(a._d)',line)
    call dcl_uproc(parser,'PM__over(a:_dim{,},d:dom)=rec _dim{ _a=a,_d=d}'//&
         ' check "New domain does not conform in over":conform(d,a._d)',line)
    call dcl_uproc(parser,'dim(a:any,d:dom)=rec _dim{_a=a,_d=d}',line)
    call dcl_uproc(parser,'PM__do_dim(a:any,d:any)='//&
         '_array(a,d,size(d))',&
         line)
    
    call dcl_uproc(parser,'(//)(a:_dim{,},arg...:index)='//&
         'a._a check "index out of range" : contains(a._d,arg...)',line)    
    call dcl_uproc(parser,'(//)(a:_dim{,},arg...:slice) do'//&
         ' check "slice out of range":contains(a._d,arg...);'//&
         ' result=rec _dim{_a=a._a,_d=get_slice(a._d,arg...)} endproc',line)
    call dcl_uproc(parser,'(//)(a:_dim{,},arg...:subs) do'//&
         ' check "slice out of range":contains(a._d,arg...);'//& 
         ' result=rec _dim{_a=a._a,_d=_shrinkit(a._d,arg...)} endproc',line)
    call dcl_uproc(parser,'PM__get_elem(a:_dim{,},arg...)=a._a',line)
    
    call dcl_uproc(parser,'PM__dup(a:_dim{,})='//&
         '_array(a._a,a._d,size(a._d))',line)
    call dcl_uproc(parser,'PM__pdup(a:_dim{,})='//&
         '_array(a._a,a._d,size(a._d))',line)
    
    call dcl_uproc(parser,'PM__assign(a:[],b:_dim{,}) do a=b._a endproc',line)
    call dcl_uproc(parser,'PM__assign_var(a:[],b:_dim{,}) '//&
         'do a=b._a endproc',line)
    call dcl_uproc(parser,'PM__assign(a:_nest[],b:_dim{,}) '//&
         'do _assign_nested_dim(a,b,same_type(arb(a),b._a)) endproc',line)
    call dcl_uproc(parser,'PM__assign_var(a:_nest[],b:_dim{,}) '//&
         'do _assign_nested_dim(a,b,same_type(arb(a),b._a)) endproc',line)
    call dcl_uproc(parser,'_assign_nested_dim(a,b,x:$false)'//&
         'do for i in a do i=b endfor endproc',line)
    call dcl_uproc(parser,'_assign_nested_dim(a,b,x:$true) do a=b._a endproc',&
         line)

    ! Distributions
    call dcl_type(parser,'dist includes block',line)
    
    call dcl_uproc(parser,&
         'index2point(i:any_int,s1:any_int)='//&
         'i1,i2 where i1=i-i2*s1 where i2=i/s1',line)
    call dcl_uproc(parser,'index2point(i:any_int,s1:any_int,s2:any_int)='//&
         'i1,i2,i3 where i1=i-j2*s1 '//&
         'where i2=j2-i3*s2 where i3=j2/s2 where j2=i/s1',line)
   call dcl_uproc(parser,&
         'index2point(i:any_int,s1:any_int,s2:any_int,s3:any_int)='//&
         'i1,i2,i3,i4'//&
         ' where i1=i-j2*s1 where i2=j2-j3*s2 where i3=j3-i4*s3 where i4=j3/s3'//&
         ' where j3=j2/s2 where j2=i/s1',line)
    call dcl_uproc(parser,&
         'index2point(i:any_int,s1:any_int,s2:any_int,'//&
         's3:any_int,s4:any_int)='//&
         'i1,i2,i3,i4,i5'//&
         ' where i1=i-j2*s1 where i2=j2-j3*s2 '//&
         ' where i3=j3-j4*s3 where i4=j4-i5*s4 '//&
         ' where i5=j4/s4'//&
         ' where j4=j3/s3 where j3=j2/s2 where j2=i/s1',line)
    call dcl_uproc(parser,'index2point(i:any_int,s1:any_int,s2:any_int,'//&
         's3:any_int,s4:any_int,s5:any_int)='//&
         'i1,i2,i3,i4,i5,i6'//&
         ' where i1=i-j2*s1 where i2=j2-j3*s2 '//&
         ' where i3=j3-j4*s3 where i4=j4-j5*s4 '//&
         ' where i5=j5-i6*s5 where i6=j5/s5'//&
         ' where j5=j4/s4 where j4=j3/s3 where j3=j2/s2 where j2=i/s1',line)
    call dcl_uproc(parser,'index2point(i:any_int,s1:any_int,s2:any_int,'//&
         's3:any_int,s4:any_int,s5:any_int,s6:any_int)='//&
         'i1,i2,i3,i4,i5,i6,i7'//&
         ' where i1=i-j2*s1 where i2=j2-j3*s2 '//&
         ' where i3=j3-j4*s3 where i4=j4-j5*s4 '//&
         ' where i5=j5-j6*s5 where i6=j6-i7*s6 where i7=j6/s6'//&
         ' where j6=j5/s5 where j5=j4/s4 where j4=j3/s3'//&
         ' where j3=j2/s2 where j2=i/s1',line)

    call dcl_uproc(parser,'point2index(b:tuple1d,t:any_int)=long(t)',line)
    call dcl_uproc(parser,'point2index(b:tuple1d,t:tuple1d)=t.d1',line)
    call dcl_uproc(parser,'point2index(b:tuple2d,t:tuple2d)=t.d1*b.d2+t.d2',line)
    call dcl_uproc(parser,'point2index(b:tuple3d,t:tuple3d)='//&
         '(t.d1*b.d2)*b.d3+t.d3',line)
    call dcl_uproc(parser,'point2index(b:tuple4d,t:tuple4d)='//&
         '((t.d1*b.d2)*b.d3+t.d3)*b.d4+t.d4',line)
    call dcl_uproc(parser,'point2index(b:tuple5d,t:tuple5d)='//&
         '(((t.d1*b.d2)*b.d3+t.d3)*b.d4+t.d4)*b.d5+t.d5',line)
    call dcl_uproc(parser,'point2index(b:tuple6d,t:tuple6d)='//&
         '((((t.d1*b.d2)*b.d3+t.d3)*b.d4+t.d4)*b.d5+t.d5)'//&
         '*b.d6+t.d6',line)
    call dcl_uproc(parser,'point2index(b:tuple7d,t:tuple7d)='//&
         '(((((t.d1*b.d2)*b.d3+t.d3)*b.d4+t.d4)*b.d5+t.d5)'//&
         '*b.d6+t.d6)'//&
         '*b.d7+t.d7',line)

    
    ! Block distribution
    call dcl_type(parser,&
         'block is block1d,block2d,block3d,block4d,block5d,block6d,block7d',&
         line)
    call dcl_type(parser,&
         'block1d is rec _block{_hi,_p:tuple1d,_n}',line)
    call dcl_type(parser,&
         'block2d is rec _block{_hi,_p:tuple2d,_n}',line)
    call dcl_type(parser,&
         'block3d is rec _block{_hi,_p:tuple3d,_n}',line)
    call dcl_type(parser,&
         'block4d is rec _block{_hi,_p:tuple4d,_n}',line)
    call dcl_type(parser,&
         'block5d is rec _block{_hi,_p:tuple5d,_n}',line)
    call dcl_type(parser,&
         'block6d is rec _block{_hi,_p:tuple6d,_n}',line)
    call dcl_type(parser,&
         'block7d is rec _block{_hi,_p:tuple7d,_n}',line)
    
    call dcl_uproc(parser,&
         'block(g:tuple{long},d:tuple{long})='//&
         'rec _block{_hi=g,_p=d,_n=d.d1}',line)
    call dcl_uproc(parser,&
         'block(g:tuple{long,long},d:tuple{long,long})='//&
         'rec _block{_hi=g,_p=d,_n=d.d1*d.d2}',line)
    call dcl_uproc(parser,&
         'block(g:tuple{long,long,long},d:tuple{long,long,long})='//&
         'rec _block{_hi=g,_p=d,_n=d.d1*d.d2*d.d3}',line)
    call dcl_uproc(parser,&
         'block(g:tuple{long,long,long,long},d:tuple{long,long,long,long})='//&
         'rec _block{_hi=d,_p=d,_n=d.d1*d.d2*d.d3*d.d4}',line)
    call dcl_uproc(parser,&
         'block(g:tuple{long,long,long,long,long},'//&
         ' d:tuple{long,long,long,long,long})='//&
         'rec _block{_hi=g,_p=d,_n=d.d1*d.d2*d.d3*d.d4*d.d5}',line)
    call dcl_uproc(parser,&
         'block(g:tuple{long,long,long,long,long,long},'//&
         '  d:tuple{long,long,long,long,long,long})='//&
         'rec _block{_hi=g,_p=d,_n=d.d1*d.d2*d.d3*d.d4*d.d5*d.d6}',line)
    call dcl_uproc(parser,&
         'block(g:tuple{long,long,long,long,long,long,long},'//&
         '  d:tuple{long,long,long,long,long,long,long})='//&
         'rec _block{_hi=g,_p=d,_n=d.d1*d.d2*d.d3*d.d4*d.d5*d.d6*d.d7}',line)

    call dcl_uproc(parser,'dom(b:block)=grid(0l..b._n-1l)',line)

    call dcl_uproc(parser,'topo(b:block)=b._p',line)
    
    call dcl_uproc(parser,'[](b:block,i:any_int)='//&
         'PM__get_elem(b,i) check "Index out of bounds": i<b._n',line)
 
        
    call dcl_uproc(parser,&
         'PM__get_elem(b:block1d,t:tuple1d)=PM__get_elem(b,t.d1)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block2d,t:tuple2d)=PM__get_elem(b,t.d1,t.d2)',line)
    call dcl_uproc(parser,'PM__get_elem(b:block3d,t:tuple3d)='//&
         'PM__get_elem(b,t.d1,t.d2,t.d3)',line)
    call dcl_uproc(parser,'PM__get_elem(b:block4d,t:tuple4d)='//&
         'PM__get_elem(b,t.d1,t.d2,t.d3,t.d4)',line)
    call dcl_uproc(parser,'PM__get_elem(b:block5d,t:tuple5d)='//&
         'PM__get_elem(b,t.d1,t.d2,t.d3,t.d4,t.d5)',line)
    call dcl_uproc(parser,'PM__get_elem(b:block6d,t:tuple6d)='//&
         'PM__get_elem(b,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6)',line)
    call dcl_uproc(parser,'PM__get_elem(b:block7d,t:tuple7d)='//&
         'PM__get_elem(b,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6,t.d7)',line)
    
    call dcl_uproc(parser,&
         'PM__get_elem(b:block1d,i:any_int)=grid(start..finish)'//&
         ' where finish=(ii+1l)*b._hi.d1/b._p.d1-1l'//&
         ' where start=ii*b._hi.d1/b._p.d1 where ii=long(i)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block2d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2'//&
         ' where i2,i1=index2point(ii,b._p.d2)'//&
         ' where ii=long(i)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block2d,i1:any_int,i2:any_int)='//&
         ' grid(start1..finish1,start2..finish2)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block3d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l,'//&
         '       finish3=(i3+1l)*b._hi.d3/b._p.d3-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2,'//&
         '       start3=i3*b._hi.d3/b._p.d3'//&
         ' where i3,i2,i1=index2point(ii,b._p.d3,b._p.d2)'//&
         ' where ii=long(i)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block3d,i1:any_int,i2:any_int,i3:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l,'//&
         '       finish3=(i3+1l)*b._hi_d3/b._p.d2-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2,'//&
         '       start3=i3*b._hi.d3/b._p.d3',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block4d,i1:any_int,i2:any_int,i3:any_int,i4:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,start4..finish4)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l,'//&
         '       finish3=(i3+1l)*b._hi.d3/b._p.d3-1l,'//&
         '       finish4=(i4+1l)*b._hi.d4/b._p.d4-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2,'//&
         '       start3=i3*b._hi.d3/b._p.d3,'//&
         '       start4=i4*b._hi.d4/b._p.d4',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block4d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,start4..finish4)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l,'//&
         '       finish3=(i3+1l)*b._hi_d3/b._p.d3-1l,'//&
         '       finish4=(i4+1l)*b._hi_d4/b._p.d4-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2,'//&
         '       start3=i3*b._hi.d3/b._p.d3,'//&
         '       start4=i4*b._hi.d4/b._p.d4'//&         
         ' where i4,i3,i2,i1=index2point(ii,b._p.d4,b._p.d3,b._p.d2)'//&
         ' where ii=long(i)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block5d,'//&
         ' i1:any_int,i2:any_int,i3:any_int,i4:any_int,i5:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '    start4..finish4,start5..finish5)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l,'//&
         '       finish3=(i3+1l)*b._hi.d3/b._p.d3-1l,'//&
         '       finish4=(i4+1l)*b._hi.d4/b._p.d4-1l,'//&
         '       finish5=(i5+1l)*b._hi.d5/b._p.d5-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2,'//&
         '       start3=i3*b._hi.d3/b._p.d3,'//&
         '       start4=i4*b._hi.d4/b._p.d4,'//&
         '       start5=i5*b._hi.d5/b._p.d5',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block5d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '        start4..finish4,start5..finish5)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l,'//&
         '       finish3=(i3+1l)*b._hi_d3/b._p.d3-1l,'//&
         '       finish4=(i4+1l)*b._hi_d4/b._p.d4-1l,'//&
         '       finish5=(i5+1l)*b._hi_d5/b._p.d5-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2,'//&
         '       start3=i3*b._hi.d3/b._p.d3,'//&
         '       start4=i4*b._hi.d4/b._p.d4,'//&
         '       start5=i5*b._hi.d5/b._p.d5'//&
         ' where i5,i4,i3,i2,i1=index2point(ii,b._p.d5,b._p.d4,b._p.d3,b._p.d2)'//&
         ' where ii=long(i)',line)
       call dcl_uproc(parser,&
         'PM__get_elem(b:block6d,'//&
         ' i1:any_int,i2:any_int,i3:any_int,i4:any_int,i5:any_int,i6:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '    start4..finish4,start5..finish5,start6..finish6)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l,'//&
         '       finish3=(i3+1l)*b._hi.d3/b._p.d3-1l,'//&
         '       finish4=(i4+1l)*b._hi.d4/b._p.d4-1l,'//&
         '       finish5=(i5+1l)*b._hi.d5/b._p.d5-1l,'//&
         '       finish6=(i6+1l)*b._hi.d6/b._p.d6-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2,'//&
         '       start3=i3*b._hi.d3/b._p.d3,'//&
         '       start4=i4*b._hi.d4/b._p.d4,'//&
         '       start5=i5*b._hi.d5/b._p.d5,'//&
         '       start6=i6*b._hi.d6/b._p.d6',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block6d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '      start4..finish4,start5..finish5,start6..finish6)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l,'//&
         '       finish3=(i3+1l)*b._hi_d3/b._p.d3-1l,'//&
         '       finish4=(i4+1l)*b._hi_d4/b._p.d4-1l,'//&
         '       finish5=(i5+1l)*b._hi_d5/b._p.d5-1l,'//&
         '       finish6=(i6+1l)*b._hi_d6/b._p.d6-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2,'//&
         '       start3=i3*b._hi.d3/b._p.d3,'//&
         '       start4=i4*b._hi.d4/b._p.d4,'//&
         '       start5=i5*b._hi.d5/b._p.d5,'//&
         '       start6=i6*b._hi.d6/b._p.d6'//&         
         ' where i6,i5,i4,i3,i2,i1='//&
         ' index2point(ii,b._p.d6,b._p.d5,b._p.d4,b._p.d3,b._p.d2)'//&
         ' where ii=long(i)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block7d,'//&
         ' i1:any_int,i2:any_int,i3:any_int,i4:any_int,i5:any_int,'//&
         ' i6:any_int,i7:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '    start4..finish4,start5..finish5,start6..finish6,start7..finish7)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l,'//&
         '       finish3=(i3+1l)*b._hi.d3/b._p.d3-1l,'//&
         '       finish4=(i4+1l)*b._hi.d4/b._p.d4-1l,'//&
         '       finish5=(i5+1l)*b._hi.d5/b._p.d5-1l,'//&
         '       finish6=(i6+1l)*b._hi.d6/b._p.d6-1l,'//&
         '       finish7=(i6+1l)*b._hi.d7/b._p.d7-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2,'//&
         '       start3=i3*b._hi.d3/b._p.d3,'//&
         '       start4=i4*b._hi.d4/b._p.d4,'//&
         '       start5=i5*b._hi.d5/b._p.d5,'//&
         '       start6=i6*b._hi.d6/b._p.d6,'//&         
         '       start7=i7*b._hi.d7/b._p.d7',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:block7d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '      start4..finish4,start5..finish5,start6..finish6,start6..finish7)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l,'//&
         '       finish3=(i3+1l)*b._hi_d3/b._p.d3-1l,'//&
         '       finish4=(i4+1l)*b._hi_d4/b._p.d4-1l,'//&
         '       finish5=(i5+1l)*b._hi_d5/b._p.d5-1l,'//&
         '       finish6=(i6+1l)*b._hi_d6/b._p.d6-1l,'//&
         '       finish7=(i7+1l)*b._hi_d7/b._p.d7-1l'//&         
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2,'//&
         '       start3=i3*b._hi.d3/b._p.d3,'//&
         '       start4=i4*b._hi.d4/b._p.d4,'//&
         '       start5=i5*b._hi.d5/b._p.d5,'//&
         '       start6=i6*b._hi.d6/b._p.d6,'//&
         '       start7=i7*b._hi.d7/b._p.d7'//&
         ' where i7,i6,i5,i4,i3,i2,i1='//&
         ' index2point(ii,b._p.d7,b._p.d6,b._p.d5,b._p.d4,b._p.d3,b._p.d2)'//&
         ' where ii=long(i)',line)

    call dcl_uproc(parser,&
         'empty(b:block1d)=grid(1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:block2d)=grid(1..0,1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:block3d)=grid(1..0,1..0,1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:block4d)=grid(1..0,1..0,1..0,1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:block5d)=grid(1..0,1..0,1..0,1..0,1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:block6d)=grid(1..0,1..0,1..0,1..0,1..0,1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:block7d)=grid(1..0,1..0,1..0,1..0,1..0,1..0,1..0)',line)

    call dcl_uproc(parser,'prc_for_grid(b:block1d,g:grid1d)='//&
         'grid(lo1..hi1) where'//&
         ' lo1=(b._p.d1*(low(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' hi1=(b._p.d1*(high(g.d1)+1l)-1l)/b._hi.d1',line)
    call dcl_uproc(parser,'prc_for_grid(b:block2d,g:grid2d)='//&
         'grid(lo1..hi1,lo2..hi2) where'//&
         ' lo1=(b._p.d1*(low(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' hi1=(b._p.d1*(high(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' lo2=(b._p.d2*(low(g.d2)+1l)-1l)/b._hi.d2,'//&
         ' hi2=(b._p.d2*(high(g.d2)+1l)-1l)/b._hi.d2',line)
    call dcl_uproc(parser,'prc_for_grid(b:block3d,g:grid3d)='//&
         'grid(lo1..hi1,lo2..hi2,lo3..hi3) where'//&
         ' lo1=(b._p.d1*(low(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' hi1=(b._p.d1*(high(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' lo2=(b._p.d2*(low(g.d2)+1l)-1l)/b._hi.d2,'//&
         ' hi2=(b._p.d2*(high(g.d3)+1l)-1l)/b._hi.d2,'//&
         ' lo3=(b._p.d3*(low(g.d3)+1l)-1l)/b._hi.d3,'//&
         ' hi3=(b._p.d3*(high(g.d3)+1l)-1l)/b._hi.d3',line)
    call dcl_uproc(parser,'prc_for_grid(b:block4d,g:grid4d)='//&
         'grid(lo1..hi1,lo2..hi2,lo3..hi3,lo4..hi4) where'//&
         ' lo1=(b._p.d1*(low(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' hi1=(b._p.d1*(high(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' lo2=(b._p.d2*(low(g.d2)+1l)-1l)/b._hi.d2,'//&
         ' hi2=(b._p.d2*(high(g.d3)+1l)-1l)/b._hi.d2,'//&
         ' lo3=(b._p.d3*(low(g.d3)+1l)-1l)/b._hi.d3,'//&
         ' hi3=(b._p.d3*(high(g.d3)+1l)-1l)/b._hi.d3,'//&
         ' lo4=(b._p.d4*(low(g.d4)+1l)-1l)/b._hi.d4,'//&
         ' hi4=(b._p.d4*(high(g.d4)+1l)-1l)/b._hi.d4',line)
    call dcl_uproc(parser,'prc_for_grid(b:block5d,g:grid5d)='//&
         'grid(lo1..hi1,lo2..hi2,lo3..hi3,lo4..hi4,lo5..hi5) where'//&
         ' lo1=(b._p.d1*(low(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' hi1=(b._p.d1*(high(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' lo2=(b._p.d2*(low(g.d2)+1l)-1l)/b._hi.d2,'//&
         ' hi2=(b._p.d2*(high(g.d3)+1l)-1l)/b._hi.d2,'//&
         ' lo3=(b._p.d3*(low(g.d3)+1l)-1l)/b._hi.d3,'//&
         ' hi3=(b._p.d3*(high(g.d3)+1l)-1l)/b._hi.d3,'//&
         ' lo4=(b._p.d4*(low(g.d4)+1l)-1l)/b._hi.d4,'//&
         ' hi4=(b._p.d4*(high(g.d4)+1l)-1l)/b._hi.d4,'//&
         ' lo5=(b._p.d5*(low(g.d5)+1l)-1l)/b._hi.d5,'//&
         ' hi5=(b._p.d5*(high(g.d5)+1l)-1l)/b._hi.d5',line)
    call dcl_uproc(parser,'prc_for_grid(b:block6d,g:grid6d)='//&
         'grid(lo1..hi1,lo2..hi2,lo3..hi3,lo4..hi4,lo5..hi5,lo6..hi6) where'//&
         ' lo1=(b._p.d1*(low(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' hi1=(b._p.d1*(high(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' lo2=(b._p.d2*(low(g.d2)+1l)-1l)/b._hi.d2,'//&
         ' hi2=(b._p.d2*(high(g.d3)+1l)-1l)/b._hi.d2,'//&
         ' lo3=(b._p.d3*(low(g.d3)+1l)-1l)/b._hi.d3,'//&
         ' hi3=(b._p.d3*(high(g.d3)+1l)-1l)/b._hi.d3,'//&
         ' lo4=(b._p.d4*(low(g.d4)+1l)-1l)/b._hi.d4,'//&
         ' hi4=(b._p.d4*(high(g.d4)+1l)-1l)/b._hi.d4,'//&
         ' lo5=(b._p.d5*(low(g.d5)+1l)-1l)/b._hi.d5,'//&
         ' hi5=(b._p.d5*(high(g.d5)+1l)-1l)/b._hi.d5,'//&
         ' lo6=(b._p.d6*(low(g.d6)+1l)-1l)/b._hi.d6,'//&
         ' hi6=(b._p.d6*(high(g.d6)+1l)-1l)/b._hi.d6',line)
    call dcl_uproc(parser,'prc_for_grid(b:block7d,g:grid7d)='//&
         'grid(lo1..hi1,lo2..hi2,lo3..hi3,lo4..hi4,'//&
         'lo5..hi5,lo6..hi6,lo7..hi7) where'//&
         ' lo1=(b._p.d1*(low(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' hi1=(b._p.d1*(high(g.d1)+1l)-1l)/b._hi.d1,'//&
         ' lo2=(b._p.d2*(low(g.d2)+1l)-1l)/b._hi.d2,'//&
         ' hi2=(b._p.d2*(high(g.d3)+1l)-1l)/b._hi.d2,'//&
         ' lo3=(b._p.d3*(low(g.d3)+1l)-1l)/b._hi.d3,'//&
         ' hi3=(b._p.d3*(high(g.d3)+1l)-1l)/b._hi.d3,'//&
         ' lo4=(b._p.d4*(low(g.d4)+1l)-1l)/b._hi.d4,'//&
         ' hi4=(b._p.d4*(high(g.d4)+1l)-1l)/b._hi.d4,'//&
         ' lo5=(b._p.d5*(low(g.d5)+1l)-1l)/b._hi.d5,'//&
         ' hi5=(b._p.d5*(high(g.d5)+1l)-1l)/b._hi.d5,'//&
         ' lo6=(b._p.d6*(low(g.d6)+1l)-1l)/b._hi.d6,'//&
         ' hi6=(b._p.d6*(high(g.d6)+1l)-1l)/b._hi.d6,'//&
         ' lo7=(b._p.d7*(low(g.d7)+1l)-1l)/b._hi.d7,'//&
         ' hi7=(b._p.d7*(high(g.d7)+1l)-1l)/b._hi.d7',line)

    call dcl_uproc(parser,'prc_for(b:block1d,j:any_int)=p'//&
         ' where p=(b._p.d1*(jj+1l)-1l)/b._hi.d1 where jj=long(j)',line)
    call dcl_uproc(parser,'prc_for(b:block1d,j:tuple1d)=p'//&
         ' where p=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1',line)
    call dcl_uproc(parser,&
         'prc_for(b:block2d,j:tuple2d)=p'//&
         ' where p=p1*b._p.d2+p2'//&
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2',line)
    call dcl_uproc(parser,'prc_for(b:block3d,j:tuple3d)=p'//&
         ' where p=(p1*b._p.d2+p2)*b._p.d3+p3'//&
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2'//&
         ' where p3=(b._p.d3*(j.d3+1l)-1l)/b._hi.d3',line)
    call dcl_uproc(parser,'prc_for(b:block4d,j:tuple4d)=p'//&
         ' where p=((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4 '//&
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2'//&
         ' where p3=(b._p.d3*(j.d3+1l)-1l)/b._hi.d3'//&
         ' where p4=(b._p.d4*(j.d4+1l)-1l)/b._hi.d4',&
         line)
    call dcl_uproc(parser,'prc_for(b:block5d,j:tuple5d)=p'//&
         ' where p=(((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)*b._p.d5+p5'//&    
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2'//&
         ' where p3=(b._p.d3*(j.d3+1l)-1l)/b._hi.d3'//&
         ' where p4=(b._p.d4*(j.d4+1l)-1l)/b._hi.d4'//&
         ' where p5=(b._p.d5*(j.d5+1l)-1l)/b._hi.d5',&
         line)
    call dcl_uproc(parser,'prc_for(b:block6d,j:tuple6d)=p'//&
         ' where p=((((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)'//&
         '*b._p.d5+p5)*b._p.d6+p6'//&       
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2'//&
         ' where p3=(b._p.d3*(j.d3+1l)-1l)/b._hi.d3'//&
         ' where p4=(b._p.d4*(j.d4+1l)-1l)/b._hi.d4'//&
         ' where p5=(b._p.d5*(j.d5+1l)-1l)/b._hi.d5'//&
         ' where p6=(b._p.d6*(j.d6+1l)-1l)/b._hi.d6',&
         line)
    call dcl_uproc(parser,'prc_for(b:block7d,j:tuple7d)=p'//&
         ' where p=(((((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)*b._p.d5+p5)'//&
         '*b._p.d6+p6)*b._p.d7+p7'//&        
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2'//&
         ' where p3=(b._p.d3*(j.d3+1l)-1l)/b._hi.d3'//&
         ' where p4=(b._p.d4*(j.d4+1l)-1l)/b._hi.d4'//&
         ' where p5=(b._p.d5*(j.d5+1l)-1l)/b._hi.d5'//&
         ' where p6=(b._p.d6*(j.d6+1l)-1l)/b._hi.d6'//&
         ' where p7=(b._p.d7*(j.d7+1l)-1l)/b._hi.d7',&
         line)    

    call dcl_uproc(parser,'prc_and_index(b:block1d,j:any_int)=p,i'//&
         ' where i=jj-s1'//&
         ' where s1=p*b._hi.d1/b._p.d1'//&
         ' where p=(b._p.d1*(jj+1l)-1l)/b._hi.d1 where jj=long(j)',line)
    call dcl_uproc(parser,'prc_and_index(b:block1d,j:tuple1d)=p,i'//&
         ' where i=j.d1-s1'//&
         ' where s1=p*b._hi.d1/b._p.d1'//&
         ' where p=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1',line)
    call dcl_uproc(parser,&
         'prc_and_index(b:block2d,j:tuple2d)=p,i'//&
         ' where p=p1*b._p.d2+p2'//&
         ' where i=(j.d2-s2)*(f1-s1)+(j.d1-s1)'//&
         ' where s1=p1*b._hi.d1/b._p.d1'//&
         ' where s2=p2*b._hi.d2/b._p.d2'//&
         ' where f1=(p1+1l)*b._hi.d1/b._p.d1'//&
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2',line)
    call dcl_uproc(parser,'prc_and_index(b:block3d,j:tuple3d)=p,i'//&
         ' where p=(p1*b._p.d2+p2)*b._p.d3+p3'//&
         ' where i=((j.d2-s3)*(f2-s2)+j.d2-s2)*(f1-s1)+(j.d1-s1)'//&
         ' where s1=p1*b._hi.d1/b._p.d1'//&
         ' where s2=p2*b._hi.d2/b._p.d2'//&
         ' where s3=p3*b._hi.d3/b._p.d3'//&         
         ' where f1=(p1+1l)*b._hi.d1/b._p.d1'//&
         ' where f2=(p2+1l)*b._hi.d2/b._p.d2'//&
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2'//&
         ' where p3=(b._p.d3*(j.d3+1l)-1l)/b._hi.d3',line)
    call dcl_uproc(parser,'prc_and_index(b:block4d,j:tuple4d)=p,i'//&
         ' where p=((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4 '//&
         ' where i=(((j.d4-s4)*(f3-s3)+(j.d3-s3))*'//&
         '(f2-s2)+(j.d2-s2))*(f1-s1)+(j.d1-s1)'//&
         ' where s1=p1*b._hi.d1/b._p.d1'//&
         ' where s2=p2*b._hi.d2/b._p.d2'//&
         ' where s3=p3*b._hi.d3/b._p.d3'//&
         ' where s4=p3*b._hi.d3/b._p.d4'//&
         ' where f1=(p1+1l)*b._hi.d1/b._p.d1'//&
         ' where f2=(p2+1l)*b._hi.d2/b._p.d2'//&
         ' where f3=(p3+1l)*b._hi.d3/b._p.d3'//&
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2'//&
         ' where p3=(b._p.d3*(j.d3+1l)-1l)/b._hi.d3'//&
         ' where p4=(b._p.d4*(j.d4+1l)-1l)/b._hi.d4',&
         line)
    call dcl_uproc(parser,'prc_and_index(b:block5d,j:tuple5d)=p,i'//&
         ' where p=(((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)*b._p.d5+p5'//&
         ' where i=((((j.d5-s5)*(f4-s4)+(j.d4-s4))*(f3-s3)+(j.d3-s3))*'//&
         '(f2-s2)+(j.d2-s2))*(f1-s1)+(j.d1-s1)'//&
         ' where s1=p1*b._hi.d1/b._p.d1'//&
         ' where s2=p2*b._hi.d2/b._p.d2'//&
         ' where s3=p3*b._hi.d3/b._p.d3'//&
         ' where s4=p4*b._hi.d4/b._p.d4'//&
         ' where s5=p5*b._hi.d5/b._p.d5'//&        
         ' where f1=(p1+1l)*b._hi.d1/b._p.d1'//&
         ' where f2=(p2+1l)*b._hi.d2/b._p.d2'//&
         ' where f3=(p3+1l)*b._hi.d3/b._p.d3'//&
         ' where f4=(p4+1l)*b._hi.d4/b._p.d4'//&        
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2'//&
         ' where p3=(b._p.d3*(j.d3+1l)-1l)/b._hi.d3'//&
         ' where p4=(b._p.d4*(j.d4+1l)-1l)/b._hi.d4'//&
         ' where p5=(b._p.d5*(j.d5+1l)-1l)/b._hi.d5',&
         line)
    call dcl_uproc(parser,'prc_and_index(b:block6d,j:tuple6d)=p,i'//&
         ' where p=((((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)'//&
         '*b._p.d5+p5)*b._p.d6+p6'//&
         ' where i=(((((j.d6-s6)*(f5-s5)+(j.d5-s5))*(f4-s4)+(j.d4-s4))*'//&
         '(f3-s3)+(j.d3-s3))*'//&
         '(f2-s2)+(j.d2-s2))*(f1-s1)+(j.d1-s1)'//&
         ' where s1=p1*b._hi.d1/b._p.d1'//&
         ' where s2=p2*b._hi.d2/b._p.d2'//&
         ' where s3=p3*b._hi.d3/b._p.d3'//&
         ' where s4=p4*b._hi.d4/b._p.d4'//&
         ' where s5=p5*b._hi.d5/b._p.d5'//&
         ' where s6=p6*b._hi.d6/b._p.d6'//&
         ' where f1=(p1+1l)*b._hi.d1/b._p.d1'//&
         ' where f2=(p2+1l)*b._hi.d2/b._p.d2'//&
         ' where f3=(p3+1l)*b._hi.d3/b._p.d3'//&
         ' where f4=(p4+1l)*b._hi.d4/b._p.d4'//&
         ' where f5=(p5+1l)*b._hi.d5/b._p.d5'//&         
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2'//&
         ' where p3=(b._p.d3*(j.d3+1l)-1l)/b._hi.d3'//&
         ' where p4=(b._p.d4*(j.d4+1l)-1l)/b._hi.d4'//&
         ' where p5=(b._p.d5*(j.d5+1l)-1l)/b._hi.d5'//&
         ' where p6=(b._p.d6*(j.d6+1l)-1l)/b._hi.d6',&
         line)
    call dcl_uproc(parser,'prc_and_index(b:block7d,j:tuple7d)=p,i'//&
         ' where p=(((((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)*b._p.d5+p5)'//&
         '*b._p.d6+p6)*b._p.d7+p7'//&
         ' where i=((((((j.d7-s7)*(f6-s6)+(j.d6-s6))*(f5-s5)+(j.d5-s5))*'//&
         '(f4-s4)+(j.d4-s4))*(f3-s3)+(j.d3-s3))*'//&
         '(f2-s2)+(j.d2-s2))*(f1-s1)+(j.d1-s1)'//&
         ' where s1=p1*b._hi.d1/b._p.d1'//&
         ' where s2=p2*b._hi.d2/b._p.d2'//&
         ' where s3=p3*b._hi.d3/b._p.d3'//&
         ' where s4=p4*b._hi.d4/b._p.d4'//&
         ' where s5=p5*b._hi.d5/b._p.d5'//&
         ' where s6=p6*b._hi.d6/b._p.d6'//&
         ' where s7=p7*b._hi.d7/b._p.d7'//&
         ' where f1=(p1+1l)*b._hi.d1/b._p.d1'//&
         ' where f2=(p2+1l)*b._hi.d2/b._p.d2'//&
         ' where f3=(p3+1l)*b._hi.d3/b._p.d3'//&
         ' where f4=(p4+1l)*b._hi.d4/b._p.d4'//&
         ' where f5=(p5+1l)*b._hi.d5/b._p.d5'//&
         ' where f6=(p6+1l)*b._hi.d6/b._p.d6'//&         
         ' where p1=(b._p.d1*(j.d1+1l)-1l)/b._hi.d1'//&
         ' where p2=(b._p.d2*(j.d2+1l)-1l)/b._hi.d2'//&
         ' where p3=(b._p.d3*(j.d3+1l)-1l)/b._hi.d3'//&
         ' where p4=(b._p.d4*(j.d4+1l)-1l)/b._hi.d4'//&
         ' where p5=(b._p.d5*(j.d5+1l)-1l)/b._hi.d5'//&
         ' where p6=(b._p.d6*(j.d6+1l)-1l)/b._hi.d6'//&
         ' where p7=(b._p.d7*(j.d7+1l)-1l)/b._hi.d7',&
         line)

    ! Fixed block distribution
    call dcl_type(parser,&
         'fblock is fblock1d,fblock2d,fblock3d,fblock4d,fblock5d,fblock6d,fblock7d',&
         line)
    call dcl_type(parser,&
         'fblock1d is rec _fblock{_b,_s,_p:tuple1d,_n}',line)
    call dcl_type(parser,&
         'fblock2d is rec _fblock{_b,_s,_p:tuple2d,_n}',line)
    call dcl_type(parser,&
         'fblock3d is rec _fblock{_b,_s,_p:tuple3d,_n}',line)
    call dcl_type(parser,&
         'fblock4d is rec _fblock{_b,_s,_p:tuple4d,_n}',line)
    call dcl_type(parser,&
         'fblock5d is rec _fblock{_b,_s,_p:tuple5d,_n}',line)
    call dcl_type(parser,&
         'fblock6d is rec _fblock{_b,_s,_p:tuple6d,_n}',line)
    call dcl_type(parser,&
         'fblock7d is rec _fblock{_b,_s,_p:tuple7d,_n}',line)

    call dcl_uproc(parser,&
         'fblock(s:tuple{long},d:tuple{long},b:tuple{long})='//&
         'rec _fblock{_b=b,_s=s,_p=d,_n=s.d1}',line)
    call dcl_uproc(parser,&
         'fblock(s:tuple{long,long},p:tuple{long,long},b:tuple{long,long})='//&
         'rec _fblock{_b=b,_s=s,_p=p,_n=s.d1*s.d2}',line)
    call dcl_uproc(parser,&
         'fblock(s:tuple{long,long,long},p:tuple{long,long,long},'//&
         '  b:tuple{long,long,long})='//&
         'rec _fblock{_b=b,_s=s,_p=p,_n=s.d1*s.d2*s.d3}',line)
    call dcl_uproc(parser,&
         'fblock(s:tuple{long,long,long,long},p:tuple{long,long,long,long},'//&
         'b:tuple{long,long,long,long})='//&
         'rec _fblock{_b=b,_s=s,_p=p,_n=s.d1*s.d2*s.d3*s.d4}',line)
    call dcl_uproc(parser,&
         'fblock(s:tuple{long,long,long,long,long},'//&
         ' p:tuple{long,long,long,long,long},'//&
         ' b:tuple{long,long,long,long,long})='//&
         'rec _fblock{_b=b,_s=s,_p=p,_n=s.d1*s.d2*s.d3*s.d4*s.d5}',line)
    call dcl_uproc(parser,&
         'fblock(s:tuple{long,long,long,long,long,long},'//&
         '  p:tuple{long,long,long,long,long,long},'//&
         '  b:tuple{long,long,long,long,long,long})='//&
         'rec _fblock{_b=b,_s=s,_p=p,_n=s.d1*s.d2*s.d3*s.d4*s.d5*s.d6}',line)
    call dcl_uproc(parser,&
         'fblock(s:tuple{long,long,long,long,long,long,long},'//&
         '  p:tuple{long,long,long,long,long,long,long},'//&
         '  b:tuple{long,long,long,long,long,long,long})='//&
         'rec _fblock{_b=b,_s=s,_p=p,_n=s.d1*s.d2*s.d3*s.d4*s.d5*s.d6*s.d7}',line)
    
    call dcl_uproc(parser,'dom(b:fblock)=grid(0l..b._n-1l)',line)
    call dcl_uproc(parser,'topo(b:fblock)=b._p',line)
    call dcl_uproc(parser,'[](b:fblock,i:any_int)='//&
         'PM__get_elem(b,i) check "Index out of bounds": i<b._n',line)
         
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock1d,t:tuple1d)=PM__get_elem(b,t.d1)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock2d,t:tuple2d)=PM__get_elem(b,t.d1,t.d2)',line)
    call dcl_uproc(parser,'PM__get_elem(b:fblock3d,t:tuple3d)='//&
         'PM__get_elem(b,t.d1,t.d2,t.d3)',line)
    call dcl_uproc(parser,'PM__get_elem(b:fblock4d,t:tuple4d)='//&
         'PM__get_elem(b,t.d1,t.d2,t.d3,t.d4)',line)
    call dcl_uproc(parser,'PM__get_elem(b:fblock5d,t:tuple5d)='//&
         'PM__get_elem(b,t.d1,t.d2,t.d3,t.d4,t.d5)',line)
    call dcl_uproc(parser,'PM__get_elem(b:fblock6d,t:tuple6d)='//&
         'PM__get_elem(b,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6)',line)
    call dcl_uproc(parser,'PM__get_elem(b:fblock7d,t:tuple7d)='//&
         'PM__get_elem(b,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6,t.d7)',line)
    
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock1d,i:any_int)=grid(start..finish)'//&
         ' where finish=min(b._s.d1,(ii+1l)*b._b.d1)'//&
         ' where start=ii*b._b where ii=long(i)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock2d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2)'//&
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2'//&
         ' where i2,i1=index2point(ii,b._p.d2)'//&
         ' where ii=long(i)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock2d,i1:any_int,i2:any_int)='//&
         ' grid(start1..finish1,start2..finish2)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2)'//&
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock3d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2),'//&
         '       finish3=min(b._s.d3,(i3+1l)*b._b.d2)'//&
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2,'//&
         '       start3=i3*b._b.d3'//&
         ' where i3,i2,i1=index2point(ii,b._p.d3,b._p.d2)'//&
         ' where ii=long(i)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock3d,i1:any_int,i2:any_int,i3:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2),'//&
         '       finish3=min(b._s.d3,(i3+1l)*b._b.d3)'//&
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2,'//&
         '       start3=i3*b._b.d3',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock4d,i1:any_int,i2:any_int,i3:any_int,i4:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,start4..finish4)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2),'//&
         '       finish3=min(b._s.d3,(i3+1l)*b._b.d3),'//&
         '       finish4=min(b._s.d4,(i4+1l)*b._b.d4)'//&
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2,'//&
         '       start3=i3*b._b.d3,'//&
         '       start4=i4*b._b.d4',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock4d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,start4..finish4)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2),'//&
         '       finish3=min(b._s.d3,(i3+1l)*b._b.d3),'//&
         '       finish4=min(b._s.d4,(i4+1l)*b._b.d4)'//&
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2,'//&
         '       start3=i3*b._b.d3,'//&
         '       start4=i4*b._b.d4'//&         
         ' where i4,i3,i2,i1=index2point(ii,b._p.d4,b._p.d3,b._p.d2)'//&
         ' where ii=long(i)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock5d,'//&
         ' i1:any_int,i2:any_int,i3:any_int,i4:any_int,i5:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '    start4..finish4,start5..finish5)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2),'//&
         '       finish3=min(b._s.d3,(i3+1l)*b._b.d3),'//&
         '       finish4=min(b._s.d4,(i4+1l)*b._b.d4),'//&
         '       finish5=min(b._s.d5,(i5+1l)*b._b.d5)'//&
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2,'//&
         '       start3=i3*b._b.d3,'//&
         '       start4=i4*b._b.d4,'//&
         '       start5=i5*b._b.d5',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock5d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '        start4..finish4,start5..finish5)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2),'//&
         '       finish3=min(b._s.d3,(i3+1l)*b._b_d3),'//&
         '       finish4=min(b._s.d4,(i4+1l)*b._b_d4),'//&
         '       finish5=min(b._s.d5,(i5+1l)*b._b_d5)'//&
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2,'//&
         '       start3=i3*b._b.d3,'//&
         '       start4=i4*b._b.d4,'//&
         '       start5=i5*b._b.d5'//&
         ' where i5,i4,i3,i2,i1=index2point(ii,b._p.d5,b._p.d4,b._p.d3,b._p.d2)'//&
         ' where ii=long(i)',line)
       call dcl_uproc(parser,&
         'PM__get_elem(b:fblock6d,'//&
         ' i1:any_int,i2:any_int,i3:any_int,i4:any_int,i5:any_int,i6:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '    start4..finish4,start5..finish5,start6..finish6)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2),'//&
         '       finish3=min(b._s.d3,(i3+1l)*b._b.d3),'//&
         '       finish4=min(b._s.d4,(i4+1l)*b._b.d4),'//&
         '       finish5=min(b._s.d5,(i5+1l)*b._b.d5),'//&
         '       finish6=min(b._s.d6,(i6+1l)*b._b.d6)'//&
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2,'//&
         '       start3=i3*b._b.d3,'//&
         '       start4=i4*b._b.d4,'//&
         '       start5=i5*b._b.d5,'//&
         '       start6=i6*b._b.d6',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock6d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '      start4..finish4,start5..finish5,start6..finish6)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2),'//&
         '       finish3=min(b._s.d3,(i3+1l)*b._b_d3),'//&
         '       finish4=min(b._s.d4,(i4+1l)*b._b_d4),'//&
         '       finish5=min(b._s.d5,(i5+1l)*b._b_d5),'//&
         '       finish6=min(b._s.d6,(i6+1l)*b._b_d6)'//&
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2,'//&
         '       start3=i3*b._b.d3,'//&
         '       start4=i4*b._b.d4,'//&
         '       start5=i5*b._b.d5,'//&
         '       start6=i6*b._b.d6'//&         
         ' where i6,i5,i4,i3,i2,i1='//&
         ' index2point(ii,b._p.d6,b._p.d5,b._p.d4,b._p.d3,b._p.d2)'//&
         ' where ii=long(i)',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock7d,'//&
         ' i1:any_int,i2:any_int,i3:any_int,i4:any_int,i5:any_int,'//&
         ' i6:any_int,i7:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '    start4..finish4,start5..finish5,start6..finish6,start7..finish7)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2),'//&
         '       finish3=min(b._s.d3,(i3+1l)*b._b.d3),'//&
         '       finish4=min(b._s.d4,(i4+1l)*b._b.d4),'//&
         '       finish5=min(b._s.d5,(i5+1l)*b._b.d5),'//&
         '       finish6=min(b._s.d6,(i6+1l)*b._b.d6),'//&
         '       finish7=min(b._s.d7,(i6+1l)*b._b.d7)'//&
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2,'//&
         '       start3=i3*b._b.d3,'//&
         '       start4=i4*b._b.d4,'//&
         '       start5=i5*b._b.d5,'//&
         '       start6=i6*b._b.d6,'//&         
         '       start7=i7*b._b.d7',line)
    call dcl_uproc(parser,&
         'PM__get_elem(b:fblock7d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2,start3..finish3,'//&
         '      start4..finish4,start5..finish5,start6..finish6,start6..finish7)'//&
         ' where finish1=min(b._s.d1,(i1+1l)*b._b.d1),'//&
         '       finish2=min(b._s.d2,(i2+1l)*b._b.d2),'//&
         '       finish3=min(b._s.d3,(i3+1l)*b._b_d3),'//&
         '       finish4=min(b._s.d4,(i4+1l)*b._b_d4),'//&
         '       finish5=min(b._s.d5,(i5+1l)*b._b_d5),'//&
         '       finish6=min(b._s.d6,(i6+1l)*b._b_d6),'//&
         '       finish7=min(b._s.d7,(i7+1l)*b._b_d7)'//&         
         ' where start1=i1*b._b.d1,'//&
         '       start2=i2*b._b.d2,'//&
         '       start3=i3*b._b.d3,'//&
         '       start4=i4*b._b.d4,'//&
         '       start5=i5*b._b.d5,'//&
         '       start6=i6*b._b.d6,'//&
         '       start7=i7*b._b.d7'//&
         ' where i7,i6,i5,i4,i3,i2,i1='//&
         ' index2point(ii,b._p.d7,b._p.d6,b._p.d5,b._p.d4,b._p.d3,b._p.d2)'//&
         ' where ii=long(i)',line)

    call dcl_uproc(parser,&
         'empty(b:fblock1d)=grid(1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:fblock2d)=grid(1..0,1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:fblock3d)=grid(1..0,1..0,1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:fblock4d)=grid(1..0,1..0,1..0,1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:fblock5d)=grid(1..0,1..0,1..0,1..0,1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:fblock6d)=grid(1..0,1..0,1..0,1..0,1..0,1..0)',line)
    call dcl_uproc(parser,&
         'empty(b:fblock7d)=grid(1..0,1..0,1..0,1..0,1..0,1..0,1..0)',line)

    call dcl_uproc(parser,'prc_for_grid(b:fblock1d,g:grid1d)='//&
         'grid(lo1..hi1) where'//&
         ' lo1=low(g.d1)/b._b.d1,'//&
         ' hi1=(b._p.d1*(high(g.d1)+1l)-1l)/b._hi.d1',line)
    call dcl_uproc(parser,'prc_for_grid(b:fblock2d,g:grid2d)='//&
         'grid(lo1..hi1,lo2..hi2) where'//&
         ' lo1=low(g.d1)/b._b.d1,'//&
         ' hi1=high(g.d1)/b._b.d1,'//&
         ' lo2=low(g.d2)/b._b.d2,'//&
         ' hi2=high(g.d2)/b._b.d2',line)
    call dcl_uproc(parser,'prc_for_grid(b:fblock3d,g:grid3d)='//&
         'grid(lo1..hi1,lo2..hi2,lo3..hi3) where'//&
         ' lo1=low(g.d1)/b._b.d1,'//&
         ' hi1=high(g.d1)/b._b.d1,'//&
         ' lo2=low(g.d2)/b._b.d2,'//&
         ' hi2=high(g.d2)/b._b.d2,'//&
         ' lo3=low(g.d3)/b._b.d3,'//&
         ' hi3=high(g.d3)/b._b.d3',line)
    call dcl_uproc(parser,'prc_for_grid(b:fblock4d,g:grid4d)='//&
         'grid(lo1..hi1,lo2..hi2,lo3..hi3,lo4..hi4) where'//&
         ' lo1=low(g.d1)/b._b.d1,'//&
         ' hi1=high(g.d1)/b._b.d1,'//&
         ' lo2=low(g.d2)/b._b.d2,'//&
         ' hi2=high(g.d2)/b._b.d2,'//&
         ' lo3=low(g.d3)/b._b.d3,'//&
         ' hi3=high(g.d3)/b._b.d3,'//&
         ' lo4=low(g.d4)/b._b.d4,'//&
         ' hi4=high(g.d4)/b._b.d4',line)
    call dcl_uproc(parser,'prc_for_grid(b:fblock5d,g:grid5d)='//&
         'grid(lo1..hi1,lo2..hi2,lo3..hi3,lo4..hi4,lo5..hi5) where'//&
         ' lo1=low(g.d1)/b._b.d1,'//&
         ' hi1=high(g.d1)/b._b.d1,'//&
         ' lo2=low(g.d2)/b._b.d2,'//&
         ' hi2=high(g.d2)/b._b.d2,'//&
         ' lo3=low(g.d3)/b._b.d3,'//&
         ' hi3=high(g.d3)/b._b.d3,'//&
         ' lo4=low(g.d4)/b._b.d4,'//&
         ' hi4=high(g.d4)/b._b.d4,'//&
         ' lo5=low(g.d5)/b._b.d5,'//&
         ' hi5=high(g.d5)/b._b.d5',line)
    call dcl_uproc(parser,'prc_for_grid(b:fblock6d,g:grid6d)='//&
         'grid(lo1..hi1,lo2..hi2,lo3..hi3,lo4..hi4,lo5..hi5,lo6..hi6) where'//&
         ' lo1=low(g.d1)/b._b.d1,'//&
         ' hi1=high(g.d1)/b._b.d1,'//&
         ' lo2=low(g.d2)/b._b.d2,'//&
         ' hi2=high(g.d2)/b._b.d2,'//&
         ' lo3=low(g.d3)/b._b.d3,'//&
         ' hi3=high(g.d3)/b._b.d3,'//&
         ' lo4=low(g.d4)/b._b.d4,'//&
         ' hi4=high(g.d4)/b._b.d4,'//&
         ' lo5=low(g.d5)/b._b.d5,'//&
         ' hi5=high(g.d5)/b._b.d5,'//&
         ' lo6=low(g.d6)/b._b.d6,'//&
         ' hi6=high(g.d6)/b._b.d6',line)
    call dcl_uproc(parser,'prc_for_grid(b:fblock7d,g:grid7d)='//&
         'grid(lo1..hi1,lo2..hi2,lo3..hi3,lo4..hi4,'//&
         'lo5..hi5,lo6..hi6,lo7..hi7) where'//&
         ' lo1=low(g.d1)/b._b.d1,'//&
         ' hi1=high(g.d1)/b._b.d1,'//&
         ' lo2=low(g.d2)/b._b.d2,'//&
         ' hi2=high(g.d2)/b._b.d2,'//&
         ' lo3=low(g.d3)/b._b.d3,'//&
         ' hi3=high(g.d3)/b._b.d3,'//&
         ' lo4=low(g.d4)/b._b.d4,'//&
         ' hi4=high(g.d4)/b._b.d4,'//&
         ' lo5=low(g.d5)/b._b.d5,'//&
         ' hi5=high(g.d5)/b._b.d5,'//&
         ' lo6=low(g.d6)/b._b.d6,'//&
         ' hi6=high(g.d6)/b._b.d6,'//&
         ' lo7=low(g.d7)/b._b.d7,'//&
         ' hi7=high(g.d7)/b._b.d7',line)

    call dcl_uproc(parser,'prc_for(b:fblock1d,j:any_int)=p'//&
         ' where p=j/b._b.d1 where jj=long(j)',line)
    call dcl_uproc(parser,'prc_for(b:fblock1d,j:tuple1d)=p'//&
         ' where p=j.d1/b._b.d1',line)
    call dcl_uproc(parser,&
         'prc_for(b:fblock2d,j:tuple2d)=p'//&
         ' where p=p1*b._p.d2+p2'//&
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2',line)
    call dcl_uproc(parser,'prc_for(b:fblock3d,j:tuple3d)=p'//&
         ' where p=(p1*b._p.d2+p2)*b._p.d3+p3'//&
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2'//&
         ' where p3=j.d3/b._b.d3',line)
    call dcl_uproc(parser,'prc_for(b:fblock4d,j:tuple4d)=p'//&
         ' where p=((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4 '//&
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2'//&
         ' where p3=j.d3/b._b.d3'//&
         ' where p4=j.d4/b._b.d4',&
         line)
    call dcl_uproc(parser,'prc_for(b:fblock5d,j:tuple5d)=p'//&
         ' where p=(((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)*b._p.d5+p5'//&    
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2'//&
         ' where p3=j.d3/b._b.d3'//&
         ' where p4=j.d4/b._b.d4'//&
         ' where p5=j.d5/b._b.d5',&
         line)
    call dcl_uproc(parser,'prc_for(b:fblock6d,j:tuple6d)=p'//&
         ' where p=((((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)'//&
         '*b._p.d5+p5)*b._p.d6+p6'//&       
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2'//&
         ' where p3=j.d3/b._b.d3'//&
         ' where p4=j.d4/b._b.d4'//&
         ' where p5=j.d5/b._b.d5'//&
         ' where p6=j.d6/b._b.d6',&
         line)
    call dcl_uproc(parser,'prc_for(b:fblock7d,j:tuple7d)=p'//&
         ' where p=(((((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)*b._p.d5+p5)'//&
         '*b._p.d6+p6)*b._p.d7+p7'//&        
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2'//&
         ' where p3=j.d3/b._b.d3'//&
         ' where p4=j.d4/b._b.d4'//&
         ' where p5=j.d5/b._b.d5'//&
         ' where p6=j.d6/b._b.d6'//&
         ' where p7=j.d7/b._b.d7',&
         line)    

    call dcl_uproc(parser,'prc_and_index(b:fblock1d,j:any_int)=p,i'//&
         ' where i=jj-s1'//&
         ' where s1=p*b._b.d1'//&
         ' where p=jj/b._b.d1 where jj=long(j)',line)
    call dcl_uproc(parser,'prc_and_index(b:fblock1d,j:tuple1d)=p,i'//&
         ' where i=j.d1-s1'//&
         ' where s1=p*b._b.d1'//&
         ' where p=j.d1/b._b.d1',line)
    call dcl_uproc(parser,&
         'prc_and_index(b:fblock2d,j:tuple2d)=p,i'//&
         ' where p=p1*b._p.d2+p2'//&
         ' where i=(j.d2-s2)*b._b.d1+(j.d1-s1)'//&
         ' where s1=p1*b._b.d1'//&
         ' where s2=p2*b._b.d2'//&
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2',line)
    call dcl_uproc(parser,'prc_and_index(b:fblock3d,j:tuple3d)=p,i'//&
         ' where p=(p1*b._p.d2+p2)*b._p.d3+p3'//&
         ' where i=((j.d2-s3)*b._b.d2+j.d2-s2)*b._b.d1+(j.d1-s1)'//&
         ' where s1=p1*b._b.d1'//&
         ' where s2=p2*b._b.d2'//&
         ' where s3=p3*b._b.d3'//&         
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2'//&
         ' where p3=j.d3/b._b.d3',line)
    call dcl_uproc(parser,'prc_and_index(b:fblock4d,j:tuple4d)=p,i'//&
         ' where p=((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4 '//&
         ' where i=(((j.d4-s4)*b._b.d3+(j.d3-s3))*'//&
         'b._b.d2+(j.d2-s2))*b._b.d1+(j.d1-s1)'//&
         ' where s1=p1*b._b.d1'//&
         ' where s2=p2*b._b.d2'//&
         ' where s3=p3*b._b.d3'//&
         ' where s4=p4*b._b.d4'//&
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2'//&
         ' where p3=j.d3/b._b.d3'//&
         ' where p4=j.d4/b._b.d4',&
         line)
    call dcl_uproc(parser,'prc_and_index(b:fblock5d,j:tuple5d)=p,i'//&
         ' where p=(((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)*b._p.d5+p5'//&
         ' where i=((((j.d5-s5)*b._b.d4+(j.d4-s4))*b._b.d3+(j.d3-s3))*'//&
         'b._b.d2+(j.d2-s2))*b._b.d1+(j.d1-s1)'//&
         ' where s1=p1*b._b.d1'//&
         ' where s2=p2*b._b.d2'//&
         ' where s3=p3*b._b.d3'//&
         ' where s4=p4*b._b.d4'//&
         ' where s5=p5*b._b.d5'//&        
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2'//&
         ' where p3=j.d3/b._b.d3'//&
         ' where p4=j.d4/b._b.d4'//&
         ' where p5=j.d5/b._b.d5',&
         line)
    call dcl_uproc(parser,'prc_and_index(b:fblock6d,j:tuple6d)=p,i'//&
         ' where p=((((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)'//&
         '*b._p.d5+p5)*b._p.d6+p6'//&
         ' where i=(((((j.d6-s6)*b._b.d5+(j.d5-s5))*b._b.d4+(j.d4-s4))*'//&
         'b._b.d3+(j.d3-s3))*'//&
         'b._b.d2+(j.d2-s2))*b._b.d1+(j.d1-s1)'//&
         ' where s1=p1*b._b.d1'//&
         ' where s2=p2*b._b.d2'//&
         ' where s3=p3*b._b.d3'//&
         ' where s4=p4*b._b.d4'//&
         ' where s5=p5*b._b.d5'//&
         ' where s6=p6*b._b.d6'//&    
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2'//&
         ' where p3=j.d3/b._b.d3'//&
         ' where p4=j.d4/b._b.d4'//&
         ' where p5=j.d5/b._b.d5'//&
         ' where p6=j.d6/b._b.d6',&
         line)
    call dcl_uproc(parser,'prc_and_index(b:fblock7d,j:tuple7d)=p,i'//&
         ' where p=(((((p1*b._p.d2+p2)*b._p.d3+p3)*b._p.d4+p4)*b._p.d5+p5)'//&
         '*b._p.d6+p6)*b._p.d7+p7'//&
         ' where i=((((((j.d7-s7)*b._b.d6+(j.d6-s6))*b._b.d5+(j.d5-s5))*'//&
         'b._b.d4+(j.d4-s4))*b._b.d3+(j.d3-s3))*'//&
         'b._b.d2+(j.d2-s2))*b._b.d1+(j.d1-s1)'//&
         ' where s1=p1*b._b.d1'//&
         ' where s2=p2*b._b.d2'//&
         ' where s3=p3*b._b.d3'//&
         ' where s4=p4*b._b.d4'//&
         ' where s5=p5*b._b.d5'//&
         ' where s6=p6*b._b.d6'//&
         ' where s7=p7*b._b.d7'//&      
         ' where p1=j.d1/b._b.d1'//&
         ' where p2=j.d2/b._b.d2'//&
         ' where p3=j.d3/b._b.d3'//&
         ' where p4=j.d4/b._b.d4'//&
         ' where p5=j.d5/b._b.d5'//&
         ' where p6=j.d6/b._b.d6'//&
         ' where p7=j.d7/b._b.f7',&
         line)

    ! Implementation of for statements
    call dcl_uproc(parser,'iter_dom(x:dom)=x',line)
    call dcl_uproc(parser,'iter_dom(x)=dom(x)',line)
    call dcl_uproc(parser,'PM__generate(x,n)=_elts(x,1l,n)',line)
    call dcl_proc(parser,'PM__import_val(x:any)->=x',&
         op_import_val,0_pm_i16,line,0,'',ftn_import_val)
    call dcl_proc(parser,'PM__importvarg(x:any)->=x',&
         op_import_varg,0_pm_i16,line,0,'',ftn_import_varg)
    
    call dcl_proc(parser,'PM__impscalar(x:any)->=x',&
         op_import_scalar,0_pm_i16,line,0,'',ftn_import_scalar)
    call dcl_proc(parser,'PM__export(x:any)->=x',&
         op_export,0_pm_i16,line,0,'',ftn_export)
    call dcl_proc(parser,'PM__extract(&y:any,x:any,y:any)',&
         op_extract,0_pm_i16,line,0,'',ftn_extract)
    call dcl_proc(parser,'PM__extract1st(x:any)->=x',&
         op_extract_first,0_pm_i16,line,0,'',ftn_extract_first)
    call dcl_uproc(parser,&
         'PM__makearray(x,d)=_makearray(x,dom(d),size(d))',line)
    call dcl_proc(parser,&
         '_export_array(any,any,any)',op_export_array,0_pm_i16,line,0,&
         '',ftn_export_array)
    call dcl_uproc(parser,&
         'check_conform(a,b) check "Domains do not conform":'//&
         'conform(dom(a),dom(b))'//&
        ' do endproc',line)
    call dcl_proc(parser,&
        '_iota(siz:long,start:long,finish:long,incr:long,totsiz:long)->long',&
        op_iota,0_pm_i16,line,&
        proc_is_generator,'',ftn_iota)
    call dcl_proc(parser,&
         '_indices(any)->long',op_indices,0_pm_i16,line,0,'',ftn_indices)
    call dcl_proc(parser,'PM__make_mask()->bool',op_make_mask,0_pm_i16,line,0,&
         '',ftn_make_mask)

 !   call dcl_uproc(parser,'indices(x)=_indices(x)',line)

    ! Parallel processing
    call dcl_proc(parser,'sys_prc()->long',op_sys_prc,0_pm_i16,line,0,&
         '$1=SYS_PRC',ftn_simple)
    call dcl_proc(parser,'sys_nprc()->long',op_sys_nprc,0_pm_i16,line,0,&
         '$1=SYS_NPRC',ftn_simple)
    call dcl_proc(parser,'this_prc()->long',op_this_prc,0_pm_i16,line,0,&
         '$1=PRC_FRAME(PRC_DEPTH)%THIS_PRC',ftn_simple)
    call dcl_proc(parser,'this_nprc()->long',op_this_nprc,0_pm_i16,line,0,&
         '$1=PRC_FRAME(PRC_DEPTH)%THIS_NPRC',ftn_simple)
    call dcl_proc(parser,'shared_prc()->long',op_shared_prc,0_pm_i16,line,0,&
         '$1=PRC_FRAME(PRC_DEPTH)%SHARED_PRC',ftn_simple)
    call dcl_proc(parser,'shared_nprc()->long',op_shared_nprc,0_pm_i16,line,0,&
         '$1=PRC_FRAME(PRC_DEPTH)%SHARED_NPRC',ftn_simple)
    call dcl_proc(parser,'is_shared()->bool',op_is_shared,0_pm_i16,line,0,&
         '$1=PRC_FRAME(PRC_DEPTH)%SHARED_NPRC>1',ftn_simple)
    call dcl_proc(parser,'is_shared(any)->bool',op_is_shared,0_pm_i16,line,0,&
         '$1=PRC_FRAME(PRC_DEPTH)%SHARED_NPRC>1',ftn_simple)
    call dcl_proc(parser,'is_par()->bool',op_is_par,0_pm_i16,line,0,&
         '$1=PRC_FRAME(PRC_DEPTH)%THIS_NPRC>1',ftn_simple)

    call dcl_proc(parser,'_push_prc_grid(arg...:any)',&
         op_push_prc_grid,0_pm_i16,line,0,&
         'CALL PUSH_PRC_GRID_1D($1,$2)',ftn_simple)
    call dcl_uproc(parser,'_push_prc(d:grid1d,t:tuple1d) do '//&
         '_push_prc_grid(is_cyclic(d.d1),t.d1) endproc',line)
    call dcl_uproc(parser,'_push_prc(d:grid2d,t:tuple2d) do '//&
         '_push_prc_grid(is_cyclic(d.d1),is_cyclic(d.d2),t.d1,t.d2) endproc',&
         line)
    call dcl_uproc(parser,'_push_prc(d:grid3d,t:tuple3d) do '//&
         '_push_prc_grid(is_cyclic(d.d1),is_cyclic(d.d2),is_cyclic(d.d3),'//&
         't.d1,t.d2,t.d3) endproc',line)
    call dcl_uproc(parser,'_push_prc(d:grid4d,t:tuple4d) do '//&
         '_push_prc_grid(is_cyclic(d.d1),is_cyclic(d.d2),is_cyclic(d.d3),'//&
         'is_cyclic(d.d4),'//&
         't.d1,t.d2,t.d3,t.d4) endproc',line)
    call dcl_uproc(parser,'_push_prc(d:grid5d,t:tuple5d) do '//&
         '_push_prc_grid(is_cyclic(d.d1),is_cyclic(d.d2),is_cyclic(d.d3),'//&
         'is_cyclic(d.d4),is_cyclic(d.d5),'//&
         't.d1,t.d2,t.d3,t.d4,t.d5) endproc',line)
    call dcl_uproc(parser,'_push_prc(d:grid6d,t:tuple6d) do '//&
         '_push_prc_grid(is_cyclic(d.d1),is_cyclic(d.d2),is_cyclic(d.d3),'//&
         'is_cyclic(d.d4),is_cyclic(d.d5),is_cyclic(d.d6),'//&
         't.d1,t.d2,t.d3,t.d4,t.d5,t.d6) endproc',line)
    call dcl_uproc(parser,'_push_prc(d:grid7d,t:tuple7d) do '//&
         '_push_prc_grid(is_cyclic(d.d1),is_cyclic(d.d2),is_cyclic(d.d3),'//&
         'is_cyclic(d.d4),is_cyclic(d.d5),is_cyclic(d.d6),is_cyclic(d.d7),'//&
         't.d1,t.d2,t.d3,t.d4,t.d5,t.d6,t.d7) endproc',line)

    call dcl_proc(parser,'_get_dims(long,long)->long',op_get_dims,0_pm_i16,&
         line,0,&
         'CALL GET_DIMS1D($1,$2,$3)',ftn_simple)
    call dcl_proc(parser,'_get_dims(long,long,long)->long,long',&
         op_get_dims,0_pm_i16,&
         line,0,&
         'CALL GET_DIMS2D($1,$2,$3,$4,$5)',ftn_simple)
    call dcl_proc(parser,'_get_dims(long,long,long,long)->long,long,long',&
         op_get_dims,0_pm_i16,&
         line,0,&
         'CALL GET_DIMS3D($1,$2,$3,$4,$5,$6,$7)',ftn_simple)
    call dcl_proc(parser,&
         '_get_dims(long,long,long,long,long)->long,long,long,long',&
         op_get_dims,0_pm_i16,&
         line,0,'CALL GET_DIMS4D($1,$2,$3,$4,$5,$6,$7,$8,$9)',ftn_simple)
    call dcl_proc(parser,'_get_dims(long,long,long,long,long,long)->'//&
         'long,long,long,long,long',&
         op_get_dims,0_pm_i16,&
         line,0,&
         'CALL GET_DIMS5D($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)',ftn_simple)
    call dcl_proc(parser,'_get_dims(long,long,long,long,long,long,long)->'//&
         'long,long,long,long,long,long',&
         op_get_dims,0_pm_i16,&
         line,0,&
         'CALL GET_DIMS6D($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13)',&
         ftn_simple)
    call dcl_proc(parser,&
         '_get_dims(long,long,long,long,long,long,long,long)->'//&
         'long,long,long,long,long,long,long',&
         op_get_dims,0_pm_i16,&
         line,0,&
         'CALL GET_DIMS7D($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15)',&
         ftn_simple)

    call dcl_uproc(parser,&
         'cart_topo(dd:grid1d,n:long)=tuple(_get_dims(n,0l))',line)
    call dcl_uproc(parser,'cart_topo(dd:grid2d,n:long)=tuple(a,b) '//&
         'where a,b=_get_dims(n,0l,0l)',line)
    call dcl_uproc(parser,'cart_topo(dd:grid3d,n:long)=tuple(a,b,c) '//&
         'where a,b,c=_get_dims(n,0l,0l,0l)',line)
    call dcl_uproc(parser,'cart_topo(dd:grid4d,n:long)=tuple(a,b,c,d) '//&
         'where a,b,c,d=_get_dims(n,0l,0l,0l,0l)',line)
    call dcl_uproc(parser,'cart_topo(dd:grid5d,n:long)=tuple(a,b,c,d,e) '//&
         'where a,b,c,d,e=_get_dims(n,0l,0l,0l,0l,0l)',line)
    call dcl_uproc(parser,'cart_topo(dd:grid6d,n:long)=tuple(a,b,c,d,e,f) '//&
         'where a,b,c,d,e,f=_get_dims(n,0l,0l,0l,0l,0l,0l)',line)
    call dcl_uproc(parser,'cart_topo(dd:grid7d,n:long)=tuple(a,b,c,d,e,f,g) '//&
         'where a,b,c,d,e,f,g=_get_dims(n,0l,0l,0l,0l,0l,0l,0l)',line)
   
    call dcl_proc(parser,'_push_prc_split(long)',op_push_prc_split,&
         0_pm_i16,&
         line,0,'CALL PUSH_PRC_SPLIT($1)',ftn_simple)
    call dcl_proc(parser,'_push_prc_conc()',op_push_prc_conc,0_pm_i16,&
         line,0,'CALL PUSH_PRC_CONC',ftn_simple)
    call dcl_proc(parser,'PM__pop_prc()',op_pop_prc,0_pm_i16,line,0,&
         'CALL POP_PRC($1)',ftn_simple)
    call dcl_proc(parser,'PM__pop_conc(bool)',op_pop_prc_conc,&
         0_pm_i16,line,0,'CALL POP_CONC($1)',ftn_simple)
    call dcl_proc(parser,'_prc_test_push()->bool',op_prc_test_push,&
         0_pm_i16,line,0,'$1=TEST_PUSH()',ftn_simple)
    call dcl_proc(parser,'_push_prc_dist()',op_push_prc_distr,0_pm_i16,line,0,&
         'CALL PUSH_PRC_DIST()',ftn_simple)

    call dcl_uproc(parser,'PM__distr(d:null)='//&
         'rec _dist{_e=null,_d=null,_a=null,_p=null,_t=null,_b=null}',&
         line)
    call dcl_uproc(parser,'PM__distr(d:null,a,p,t,b)='//&
         'rec _dist{_e=null,_d=d,_a=a,_p=p,_t=t,_b=b}',line)

    call dcl_uproc(parser,'PM__distr(d:dom)='//&
         'rec _dist{_e=null,_d=d,_a=null,_p=null,_t=null,_b=null}',&
         line)
    call dcl_uproc(parser,'PM__distr(d:dom,a,p,t,b)='//&
         'rec _dist{_e=null,_d=d,_a=a,_p=p,_t=t,_b=b}',line)
    
    call dcl_uproc(parser,'PM__distr(d:_dim{,})='//&
         'rec _dist{_e=d._a,_d=d._d,_a=null,_p=null,_t=null,_b=null}',&
         line)
    call dcl_uproc(parser,'PM__distr(d:_dim{,},a,p,t,b)='//&
         'rec _dist{_e=d._a,_d=d._d,_a=a,_p=p,_t=t,_b=b}',line)
    
    call dcl_uproc(parser,'_fx(d,r)=r',line)
    call dcl_uproc(parser,'_fx(d,r:null)=d',line)
    call dcl_uproc(parser,'PM__distr(d:rec _dist{_e,_d,_a,_p,_t,_b},a,p,t,b)='//&
         'rec _dist{_e=d._e,_d=d._d,_a=_fx(a,d._a),_p=_fx(p,d._p),'//&
         '_t=_fx(t,d._t),_b=_fx(b,d.b)}',&
         line)
    
    call dcl_uproc(parser,'dim(e,d:rec _dist{_e,_d,_a,_p,_t,_b})='//&
         'rec _dist{_e=e,_d=d._d,_a=d._a,_p=d._p,_t=d._t,_b=d._b}',line)

    call dcl_uproc(parser,&
         'PM__dup(d:rec _dist{_e:null,_d,_a:null,_p,_t,_b})=d',line)
    call dcl_uproc(parser,&
         'PM__dup(d:rec _dist{_e:null,_d,_a,_p,_t,_b})=d',line)
    
    call dcl_uproc(parser,&
         'PM__dup(d:rec _dist{_e,_d,_a:null,_p,_t,_b}) do '//&
         ' topo:=topology(d._t,d._p,d._d,'//&
         '       min(size(d._d),shared_nprc()));'//&
         ' n:=size(topo) check "topology too big":n<=shared_nprc();'//&
         ' dist:= distribute(d._p,d._d,topo,d._b);'//&
         ' p:= shared_prc();'//&
         ' elem:=empty(dist);'//&
         ' if p<n then elem=PM__get_elem(dist,p) endif;'//&
         ' dom:=rec _ddom {align=null,dom=d._d,dist=dist,tile=elem};'//&
         ' result=_array(d._e,dom,size(elem)) endproc',line)

    call dcl_uproc(parser,&
         'PM__dup(d:rec _dist{_e,_d,_a,_p,_t,_b}) do '//&
         ' topo:=topology(d._t,d._p,d._a,'//&
         '       min(size(d._a),shared_nprc()));'//&
         ' n:=size(topo) check "topology too big":n<=shared_nprc();'//&
         ' dist:= distribute(d._p,d._a,topo,d._b);'//&
         ' p:= shared_prc();'//&
         ' elem:=empty(dist);'//&
         ' if p<n then elem=PM__get_elem(dist,p) endif;'//&
         ' elem=overlap(elem,d._d);'//&
         ' dom:=rec _ddom {align=d._a,dom=d._d,dist=dist,tile=elem};'//&
         ' result=_array(d._e,dom,size(elem)) endproc',line)

    call dcl_uproc(parser,&
         'PM__partition(pp,dd:any#rec _ddom{align:null,dom,dist,tile})='//&
         ' dd.distr.tile,dd.distr.dist do _push_prc_dist()'//&
         ' endproc  ' ,line)
    
    call dcl_uproc(parser,&
         'PM__partition(pp:null,dd:any#rec _ddom{align:null,dom,dist,tile})='//&
         ' dd.distr.tile,dd.distr.dist',line)

    call dcl_uproc(parser,'PM__pardom(dd)=dom(dd)',line)
    call dcl_uproc(parser,&
         'PM__pardom(dd:any#rec _ddom{align,dom,dist,tile})=dd.distr.dom',line)
    call dcl_uproc(parser,'PM__seqdom(x)=dom(x)',line)
    call dcl_uproc(parser,&
         'PM__seqdom(dd:any#rec _ddom{align,dom,dist,tile})=dd.distr.dom'//&
         ' check "Cannot have sequential loop over distributed array":$false',line)
    call dcl_uproc(parser,&
         'arb(dd:any#rec _ddom{align,dom,dist,tile})=PM__PM__get_elem(dd,0l)',line)
    call dcl_uproc(parser,'PM__checkimp(x) each(x) do endproc',line)
    call dcl_uproc(parser,&
         'PM__checkimp(x:any#rec _ddom{align,dom,dist,tile}) do'//&
         ' check "Cannot import distributed array into parallel statement":'//&
         '   $false '//&
         ' endproc',line)
    call dcl_uproc(parser,'PM__checkrtn(x) each(x) do endproc',line)
    call dcl_uproc(parser,'PM__checkrtn(x:any#rec _ddom{align,dom,dist,tile}) do'//&
         ' check "Cannot return distributed array from parallel statement":'//&
         '   $false '//&
         ' endproc',line)
    call dcl_uproc(parser,&
         'PM__get_elem(dd:any#rec _ddom{align,dom,dist,tile},arg...:index)='//&
         'PM__PM__get_elem(dd,_indices(dd))',line)
    call dcl_uproc(parser,&
         'PM__set_elem(dd:any#rec _ddom{align,dom,dist,tile},v,arg...:index) '//&
         'do PM__setaelem(dd,_indices(dd),v) endproc',line)

    call dcl_uproc(parser,&
         'check_conform(x:any#rec _ddom{align,dom,dist,tile},y) do '//&
         ' check_conform(x.distr.dom,dom(y)) endproc',line)
    call dcl_uproc(parser,&
         'check_conform(x:dom,y:any#rec _ddom{align,dom,dist,tile}) do '//&
         'check "A distributed array connot conform to a non-distributed array":'//&
         ' $false endproc',line)
    call dcl_uproc(parser,&
         'check_conform(x:any#rec _ddom{align,dom,dist,tile},'//&
         ' y:any#rec _ddom{align,dom,dist,tile}) do '//&
         ' check_conform(x._d,y);'//&
         ' check "Arrays have different distributions":x._ds/=y._ds endproc',line)

    call dcl_proc(parser,'PM__get_distr(a:any#rec _ddom{align,dom,dist,tile})->#a',&
         op_get_dom,0_pm_i16,line,0,&
         '',ftn_array_dom)
    call dcl_uproc(parser,'PM__get_local(a:any#rec _ddom{align,dom,dist,tile})='//&
         '_redim(a,a.distr.tile)',line)
    call dcl_proc(parser,'PM__get_darray(a:any#rec _ddom{align,dom,dist,tile})->=a',&
         op_setref,0_pm_i16,line,0,'$1',ftn_simple)
    
    call dcl_uproc(parser,&
         'assemble(&a:any#rec _ddom{align,dom,dist,tile},dist) do endproc',line)
    
    call dcl_uproc(parser,'PM__partition(pp,dd)=e,d '//&
         'where e,d=PM__partition(pp,dd,null,null,null,null,null)',line)
    
    call dcl_uproc(parser,'PM__partition(pp,dd,part,topol,block,wshare,work) do'//&
         ' d:=dom(dd);'//&
         ' topo:=topology(topol,part,d,min(size(d),shared_nprc()));'//&
         ' dist:=distribute(part,d,topo,block); '//&
         ' p:=shared_prc();s:=size(topo) '//&
         '     check "topology too big":s<=shared_nprc(); '//&
         ' if s<shared_nprc() then '//&
         '     if p>=s then '//&
         '      p=workshare(wshare,d,dist,s,p-s,shared_nprc()-s,work) '//&
         '     endif; '//&
         '     _push_prc_split(p) '//&
         ' else '//&
         '     _push_prc(d,topo) '//&
         ' endif; '//&
         ' elem:=PM__get_elem(dist,p); '//&
         ' result=elem,dist endproc',line)
         
    call dcl_uproc(parser,'PM__partition(p:null,d)=dom(shape(dom(d))),null ',&
         line)
         
    call dcl_uproc(parser,&
         'topology(tp:null,dis,d:grid,l:long)=cart_topo(d,l)',line)
    call dcl_uproc(parser,&
         'topology(tp:tuple1d,dis,d:grid1d,l:long)=tp',line)
    call dcl_uproc(parser,&
         'topology(tp:tuple2d,dis,d:grid2d,l:long)=tp',line)
    call dcl_uproc(parser,&
         'topology(tp:tuple3d,dis,d:grid3d,l:long)=tp',line)
    call dcl_uproc(parser,&
         'topology(tp:tuple4d,dis,d:grid4d,l:long)=tp',line)
    call dcl_uproc(parser,&
         'topology(tp:tuple5d,dis,d:grid5d,l:long)=tp',line)
    call dcl_uproc(parser,&
         'topology(tp:tuple6d,dis,d:grid6d,l:long)=tp',line)
    call dcl_uproc(parser,&
         'topology(tp:tuple7d,dis,d:grid7d,l:long)=tp',line)
    
    call dcl_uproc(parser,&
         'distribute(dis:null,d:grid,t:tuple,b:null)='//&
         'block(shape(d),t)',line)
    call dcl_uproc(parser,&
         'distribute(dis:null,d:grid,t:tuple,b:tuple)='//&
         'fblock(shape(d),t,b)',line)
    call dcl_uproc(parser,&
         'distribute(dis:$fblock,d:grid,t:tuple,b:tuple)='//&
         'fblock(shape(d),t,b)',line)
    call dcl_uproc(parser,&
         'workshare(ws:null,d,dist,nprc:long,'//&
         'sprc:long,nsprc:long,work:null)='//&
         'nprc*(2l*sprc+1l)/(2l*nsprc)',line)
    call dcl_uproc(parser,&
         'workshare(ws:null,d,dist,nprc:long,sprc:long,nsprc:long,work:[])'//&
         'check "work array does not conform to domain":'//&
         'conform(dom(work),d) '//&
         'do wk:=work;result=_wshare(wk,nprc,sprc,nsprc) endproc',line)
    call dcl_proc(parser,&
         '_wshare(any#any,long,long,long)->long',op_wshare,0_pm_i16,line,0,&
         '',ftn_simple)
    
    call dcl_uproc(parser,'assemble(&a,dist) do'//&
         ' for each i in dom(dist) do'//&
         '   tile:=dist[i];'//&
         '   if i==this_prc() then '//&
         '     for j in tile conc do '//&
         '       k:=PM__get_elem(a,j); '//&
         '       broadcast(&k,i)'//&
         '     endfor'//&
         '   else '//&
         '     for j in tile conc do '//&
         '       k:=arb(a);'//&
         '       broadcast(&k,i);'//&
         '       PM__set_elem(a,k,j)'//&
         '     endfor'//&
         '   endif'//&
         ' endfor endproc',&
         line)

    call dcl_uproc(parser,'_tup(x:tuple)=x',line)
    call dcl_uproc(parser,'_tup(arg...)=tuple(arg...)',line)
    
    call dcl_proc(parser,'broadcast(&b:any,long)',op_broadcast,&
         0_pm_i16,line,0,'',ftn_broadcast)
    call dcl_proc(parser,'broadcast(b:any,long)->=b',op_broadcast_val,&
         0_pm_i16,line,0,'',ftn_broadcast_val)
    call dcl_proc(parser,&
         'get_remote%[a:any](b:long,c:long)->=a',op_get_remote,&
         0_pm_i16,line,0,'',ftn_get_remote)
    call dcl_proc(parser,&
         'put_remote%[&a:any](b:any,c:long,d:long)',op_put_remote,&
         0_pm_i16,line,0,'',ftn_put_remote)
    
    ! Concurrent versions of communicating operators
    call dcl_uproc(parser,'gather::<this_distr:null>[x] '//&
         'local do result=redim(x,this_dom) endproc',line)
    call dcl_uproc(parser,'{}%<this_distr:null>[&x](v,arg...:index) '//&
         ' do check "Index out of range": contains(this_dom,arg...);'//&  
         ' _put_local%[&x](v,i) where i=index(this_dom,arg...) endproc',line)
    call dcl_uproc(parser,'{}%<this_distr:null>[&x](v,arg...:subs) do '//&
         ' check "Slice out of range":contains(this_dom,arg...);'//&
         ' _put_elems%[&x](v,same_type(arb(x),v),arg...) endproc',line)
    call dcl_uproc(parser,'_put_elems%(x,v,same:$false,arg...) do'//&
         ' a:=get_slice(this_dom,arg...);'//&
         ' for each i in a, j in v do x%{i}=j endfor endproc',line)
    call dcl_uproc(parser,'_put_elems%[&x](v,same:$true,arg...) do'//&
         ' a:=get_slice(this_dom,arg...);'//&
         ' for each i in a do x%{i}=v endfor endproc',line)
    call dcl_uproc(parser,&
         '_put_local%<this_distr:null>[&x](v:any,i:long) local do'//&
         ' check "Type mismatch in %{}=":same_type(arb(x),arb(v));'//&
         ' for j in i,w in v,m in this_mask do '//&
         ' if m then PM__setaelem(x,j,w) endif endfor endproc',line)
    call dcl_uproc(parser,'{}%<this_distr:null>[x](arg...:subs)='//&
         'proc{[]}(gather::x,arg...)',line)
    call dcl_uproc(parser,'@{}%<this_distr:null>[x](arg...:index)=y do'//&
         ' here:=PM__get_elem(this_dom,this_index);'//&
         ' j:=displace(this_dom,here,_tup(arg...));y:=null(x);'//&
         ' if contains(this_dom,j) then y=opt(proc{[]}(gather::x,j)) endif'//&
         ' endproc',&
         line)
    call dcl_uproc(parser,'@{}|%<this_distr:null>[x](v,arg...:index)=y do'//&
         ' here:=PM__get_elem(this_dom,this_index);'//&
         ' j:=displace(this_dom,here,_tup(arg...));y:=v;'//&
         ' if contains(this_dom,j) then y=proc{[]}(gather::x,j) else '//&
         '  sync x endif endproc',line)
    call dcl_uproc(parser,&
         '@{}%<this_distr:null>[x](arg...:slice)=v '//&
         ' do here:=PM__get_elem(this_dom,this_index);'//&
         ' a:=get_disp(this_dom,here,arg...);'//&
         ' v:=x dim a;'//&
         ' for each j in a do'//&
         '    v[j]=(gather::x)[displace(this_dom,here,j)]'//&
         ' endfor endproc ',line)
    call dcl_uproc(parser,&
         '@{}%<this_distr:null>[x](arg...:subs)'//&
         ' do here:=PM__get_elem(this_dom,this_index);'//&
         ' a:=get_disp(this_dom,here,arg...);'//&
         ' v:=x dim a;'//&
         ' for each j in a do'//&
         '    v[j]=(gather::x)[displace(this_dom,here,j)]'//&
         ' endfor; '//&
         ' result=redim(v,_shrink(dom(v),arg...)) endproc',line)

    ! Distributed gather
    call dcl_uproc(parser,'gather::<this_distr:dist>[x] local do'//&
         ' y:=arb(x) dim this_dom; '//&
         ' for j in this_tile,v in x conc do PM__set_elem(y,v,j) endfor;'//&
         ' if is_par() then assemble(&y,this_distr) endif; '//&
         ' result=y endproc ',line)

    ! Distributed versions of binary {} operators
    call dcl_uproc(parser,'{}%<this_distr:dist>[&x](v,arg...:index) do '//&
         'check "Index out of range":contains(this_dom,arg...);'//&
         'check "Type mismatch in %{}=":same_type(x,v);'//&
         'put_remote%[&x](v,p,i) '//&
         ' where p,i=prc_and_index(this_distr,'//&
         '  loc_in_shape(this_dom,_tup(arg...))) '//&
         ' endproc',&
         line)
    call dcl_uproc(parser,'{}%<this_distr:dist>[&x](v,arg...:subs) do '//&
         ' check "Slice out of range":contains(this_dom,arg...);'//&
         ' a:=get_slice(this_dom,arg...);'//&
         ' _put_elems%[&x](v,same_type(x,v),arg...) endproc',line)
    call dcl_uproc(parser,&
         '{}%<this_distr:dist>[x](arg...:index)=get_remote%x(p,i) '//&
         ' where p,i=prc_and_index(this_distr,'//&
         '  loc_in_shape(this_dom,_tup(arg...))) '//&
         ' do check "Index out of range":contains(this_dom,arg...) endproc',&
         line)
    call dcl_uproc(parser,'@{}%<this_distr:dist>[x](arg...:index) do'//&
         ' j:=displace(this_dom,'//&
         '      PM__get_elem(this_dom,this_index),arg...);'//&
         ' p:=this_prc();i:=0l;ok:=false;'//&
         ' if contains(this_dom,j) then'//&
         '   p,i=prc_and_index(this_distr,'//&
         '     loc_in_shape(this_dom,j));ok=true'//&
         ' endif; '//&
         ' result=opt(get_remote%x(p,i),ok) '//&
         ' endproc',&
         line)    
    call dcl_uproc(parser,'@{}|%<this_distr:dist>[x](v,arg...:index) do'//&
         ' j:=displace(this_dom,'//&
         '      PM__get_elem(this_dom,this_index),arg...);'//&
         ' p:=this_prc();i:=0l;ok:=false;'//&
         ' if contains(this_dom,j) then'//&
         '   p,i=prc_and_index(this_distr,'//&
         '     loc_in_shape(this_dom,j));ok=true'//&
         ' endif; '//&
         ' result=ok=>get_remote%x(p,i)||v '//&
         ' endproc',&
         line) 
    call dcl_uproc(parser,'@{}%<this_distr:dist>[x](arg...:slice)=v do '//&
         ' here:=PM__get_elem(this_dom,this_index);'//&
         ' a:=get_disp(this_dom,here,arg...);'//&
         ' v:=x dim a;'//&
         ' for each j in a do'//&
         '   v[j]=x%{displace(this_dom,here,j)}'//&
         ' endfor '//&
         'endproc ',&
         line)
    call dcl_uproc(parser,'@{}%<this_distr:dist>[x](arg...:subs) do '//&
         ' here:=PM__get_elem(this_dom,this_index);'//&
         ' a:=get_disp(this_dom,here,arg...); '//&
         ' v:=x dim a;'//&
         ' for each j in a do'//&
         '   v[j]=x%{displace(this_dom,here,j)}'//&
         ' endfor; '//&
         ' result=redim(v,_shrink(a,arg...))'//&
         'endproc ',&
         line)
   call dcl_uproc(parser,'{}%<this_distr:dist>[x](arg...:slice) do '//&
         ' check "slice out of range":contains(this_dom,arg...); '//&
         ' a:=get_slice(this_dom,arg...); '//&
         ' v:=x dim a;'//&
         ' for each j in a do'//&
         '   v[j]=x%{j}'//&
         ' endfor; '//&
         ' result=v '//&
         'endproc ',&
         line)
    call dcl_uproc(parser,'{}%<this_distr:dist>[x](arg...:subs) do '//&
         ' check "slice out of range":contains(this_dom,arg...); '//&
         ' a:=get_slice(this_dom,arg...); '//&
         ' v:=x dim a;'//&
         ' for each j in a do'//&
         '   v[j]=x%{j}'//&
         ' endfor; '//&
         ' result=redim(v,_shrink(a,arg...))'//&
         'endproc ',&
         line)
    
    ! Distributed binary '@' with invariant args
    call dcl_uproc(parser,&
         '@{}%<this_distr:dist>[x](arg...:invar index)=_ivix%[x](arg...)',line)
    call dcl_uproc(parser,&
         '@{}%<this_distr:dist>[x](arg...:invar slice)=_ivsl%[x](arg...)',line)
    call dcl_uproc(parser,&
         '@{}%<this_distr:dist>[x](arg...:invar subs)=_ivsu%[x](arg...)',line)
    
    call dcl_uproc(parser,&
         '_ivix%<this_distr:dist>[x](arg...:invar index) do'//&
         ' here:=PM__get_elem(this_dom,this_index);y:=x;ok:=false;'//&
         ' t:=_tup(arg...);j:=displace(this_dom,here,t);'//&
         ' if invar is_shared(1) or '//&
         '     not halo_intact(this_dom,this_tile,arg...) then '//&
         '   if contains(this_dom,j) then y=x%{j} else sync x endif'//&
         ' else '//&
         '   const a=local_tile_with_halo::x(this_dom,arg...);'//&
         '   if contains(this_dom,j) then'//&
         '    y=a[loc_in_shape(this_dom,j)];'//&
         '    ok=true'//&
         '   endif; '//&
         ' endif '//& 
         ' result=opt(y,ok) '//&
         ' endproc',&
         line)
    call dcl_uproc(parser,&
         '_ivsl%<this_distr:dist>[x](arg...:invar slice) do '//&
         ' here:=PM__get_elem(this_dom,this_index); '//&
         ' a:=get_disp(this_dom,here,arg...); '//&
         ' v:=x dim a;'//&
         ' if invar is_shared(1)'//&
         '    or not halo_intact(this_dom,this_tile,arg...) then '//&
         '   for each j in a do'//&
         '     v[j]=x%{displace(this_dom,here,j)}'//&
         '   endfor'//&
         ' else '//&
         '   y:=local_tile_with_halo::x(this_dom,arg...);'//&
         '   for each j in a do'//&
         '    v[j]=y[displace(this_dom,here,j)]'//&
         '   endfor '//&
         ' endif; '//&
         ' result=v '//&
         'endproc ',&
         line)
    call dcl_uproc(parser,&
         '_ivsu%<this_distr:dist>[x](arg...:invar subs) do '//&
         ' here:=PM__get_elem(this_dom,this_index); '//&
         ' a:=get_disp(this_dom,here,arg...); '//&
         ' v:=x dim a;'//&
         ' if invar is_shared(1) '//&
         '      or not halo_intact(this_dom,this_tile,arg...) then '//&
         '   for each j in a do'//&
         '     v[j]=x%{displace(this_dom,here,j)}'//&
         '   endfor'//&
         ' else '//&
         '   y:=local_tile_with_halo::x(this_dom,arg...);'//&
         '   for each j in a do'//&
         '    v[j]=y[displace(this_dom,here,j)]'//&
         '   endfor '//&
         ' endif '//&
         ' result=redim(v,_shrink(a,arg...))'//&
         'endproc ',&
         line)
    call dcl_uproc(parser,&
         '@{}|%<this_distr:dist>[x](v,arg...:invar index)='//&
         'proc{@{}}%x(arg...)|v',line)
    
    ! Return local tile with halo cells 
    call dcl_uproc(parser,&
         'local_tile_with_halo::[x](d:invar,arg...:invar) local do '//&
         ' this_tile_x:=get_disp_halo(d,this_tile,arg...);'//&
         ' a:=arb(x) dim this_tile_x;'//&
         ' for each i in prc_for_grid(this_distr,this_tile_x) do'//&
         '   other_tile:=PM__get_elem(this_distr,i);'//&
         '   other_tile_x:=get_disp_halo(d,other_tile,arg...);'//&
         '   p:=point2index(topo(this_distr),i);'//&
         '   if p/=this_prc() then'//&
         '    recv_slice(p,a,overlap(this_tile_x,other_tile));'//&
         '   endif '//&
         ' endfor;'//&
         ' for each i in '//&
         '  prc_for_grid(this_distr,get_anti_halo(d,this_tile,arg...)) do'//&
         '   other_tile:=PM__get_elem(this_distr,i);'//&
         '   other_tile_x:=get_disp_halo(d,other_tile,arg...);'//&
         '   p:=point2index(topo(this_distr),i);'//&
         '   if p/=this_prc() then'//&
         '    send_slice(p,x,overlap(this_tile,other_tile_x));'//&
         '   endif '//&
         ' endfor;'//&
         ' o:=overlap(this_tile_x,this_tile);a[o]=x[o];'//&
         ' sync_messages();result=a endproc',line)
    
    call dcl_uproc(parser,'{}%<this_distr:dist>[x](arg...:invar index)='//&
         '_gslice::x(arg...) ',line)
    call dcl_uproc(parser,'_gslice::[x](arg...:invar index) local do '//&
         ' check "Index out of range": contains(this_dom,arg...); '//&
         ' p,i:= prc_and_index(this_distr,'//&
         '   loc_in_shape(this_dom,_tup(arg...))); '//&
         ' y:=arb(x); if p==this_prc() then y=PM__PM__get_elem(x,i) endif;'//&
         ' broadcast(&y,p); result=y endproc ',line)

    call dcl_uproc(parser,'{}%<this_distr:dist>[x](arg...:invar slice)='//&
         '_gslice::x(arg...)',line)
    call dcl_uproc(parser,'_gslice::[x](arg...:invar slice) local do '//&
         ' check "slice out of range z":contains(this_dom,arg...); '//&
         ' a:=get_slice(this_dom,arg...); '//&
         ' v:=arb(x) dim a;tile:=PM__get_elem(this_dom,this_tile);'//&
         ' u:=overlap(a,PM__get_elem(this_dom,this_tile));'//&
         ' if size(u)>0 then '//&
         '    for i in u,j in v[u] conc do '//&
         '    j=PM__PM__get_elem(x,index(tile,i)) endfor;'//& 
         ' endif; assemble_subgrid(&v,this_dom,this_distr,a);'//&
         ' result=v '//&
         'endproc ',&
         line)
    
    call dcl_uproc(parser,'{}%<this_distr:dist>[x](arg...:invar subs)='//&
         '_gslice::x(arg...)',line)
    call dcl_uproc(parser,'_gslice::[x](arg...:invar subs) local do '//&
           ' check "slice out of range":contains(this_dom,arg...); '//&
         ' a:=get_slice(this_dom,arg...); '//&
         ' v:=arb(x) dim a;tile:=PM__get_elem(this_dom,this_tile);'//&
         ' u:=overlap(a,PM__get_elem(this_dom,this_tile));'//&
         ' for i in u,j in v[u] conc do'//&
         ' j=PM__PM__get_elem(x,index(tile,u)) endfor; '//& 
         ' assemble_subgrid(&v,this_dom,this_distr,a);'//&
         ' result=redim(v,_shrink(a,arg...))'//&
         'endproc ',&
         line)
 
    call dcl_uproc(parser,'assemble_subgrid(&a,dom,dist,tile) do'//&
         '  for each p in prc_for_grid(dist,tile) do '//&
         '     t:=overlap(tile,PM__get_elem(dom,PM__get_elem(dist,p))); '//&
         '     if size(t)>0 then '//&
         '        if this_prc()==p then '//&
         '           for j in t conc do '//&
         '              k:=a[j]; '//&
         '              broadcast(&k,p)'//&
         '           endfor'//&
         '        else '//&
         '          for j in a[t] conc do '//&
         '            k:=arb(a);'//&
         '            broadcast(&k,p);'//&
         '            j=k'//&
         '          endfor'//&
         '        endif'//&
         '    endif'//&
         ' endfor endproc',&
         line)
    
    call dcl_uproc(parser,&
         'send_slice(p,x,d) do '//&
         'for i in d conc do _isend%j(p,x) '//&
         'where j=index(dom(x),i) endfor endproc',line)
    call dcl_uproc(parser,&
         'recv_slice(p,x,d) do '//&
         'for i in d conc do _irecv%j(p,x) '//&
         'where j=index(dom(x),i) endfor endproc',line)

    call dcl_proc(parser,'_isend%[x:any](p:any,j:any)',&
         op_isend,0_pm_i16,line,0,'',ftn_simple)
    call dcl_proc(parser,'_irecv%[x:any](p:any,j:any)',&
         op_irecv,0_pm_i16,line,0,'',ftn_simple)
    call dcl_proc(parser,'sync_messages()',op_sync_mess,&
         0_pm_i16,line,0,'',ftn_simple)

    ! Communicating operations on distributed arrays
    call dcl_uproc(parser,&
         '{}%(x:invar any#rec _ddom{align,dom,dist:null,tile},arg...:index)='//&
         'proc{[]}(x.local,arg...)',line)
    call dcl_uproc(parser,&
         '{}%(&x:invar any#rec _ddom{align,dom,dist:null,tile},v,arg...:index) do'//&
         ' check "Type mismatch in {}=":same_type(arb(x.#),v);'//&
         ' PM__setaelem(x,index(x.distr.dom,arg...),v) endproc',line)
    call dcl_uproc(parser,&
         '{}%(x:invar any#rec _ddom{align,dom,dist,tile},arg...:index) do'//&
         ' check "Index out of range":contains(x.distr.dom,arg...);'//&
         ' result=get_remote%(x.#,p,i) where p,i=prc_and_index(x.distr.dist, '//&
         '   loc_in_shape(x.distr.dom,_tup(arg...))) endproc',line)
     call dcl_uproc(parser,&
          '{}%(x:invar any#rec _ddom{align,dom,dist,tile},arg...:slice) do'//&
          ' check "slice out of range":contains(x.distr.dom,arg...); '//&
          ' a:=get_slice(x.distr.dom,arg...); v:=arb(x) dim a; '//&
          ' for each j in a do v[j]=x{j} endfor; '//&
          ' result=v endproc',line)
     call dcl_uproc(parser,&
          '{}%(x:invar any#rec _ddom{align,dom,dist,tile},arg...:subs) do'//&
          ' check "slice out of range":contains(x.distr.dom,arg...); '//&
          ' a:=get_slice(x.distr.dom,arg...); v:=arb(x) dim a; '//&
          ' for each j in a do v[j]=x{j} endfor; '//&
          ' result=redim(v,_shrink(a,arg...)) endproc',line)
     call dcl_uproc(parser,&
         '{}%(&x:invar any#rec _ddom{align,dom,dist,tile},v,arg...:index) do '//&
         ' check "Types do not match in {}=": same_type(arb(x.#),v);'//&
         ' check "Index out of range":contains(x.distr.dom,arg...);'//&
         ' put_remote%(x.#,v,p,i) where p,i=prc_and_index(x.distr.dist, '//&
         '   loc_in_shape(x.distr.dom,_tup(arg...))) endproc',line)
    call dcl_uproc(parser,&
         '{}%(&x:invar any#rec _ddom{align,dom,dist,tile},v,arg...:subs) do '//&
         ' check "Slice out of range":contains(x.distr.dom,arg...);'//&
         ' _comm_set_elements%(x.#,v,same_type(arb(x.#),v),arg...) endproc',line)
    call dcl_uproc(parser,&
         ' _comm_set_elements%(x,v,same:$false,arg...) do'//&
         ' a:=get_slice(x.distr.dom,arg...); '//&
         ' for each j in a,k in v do x{j}=k endfor endproc',line)
    call dcl_uproc(parser,&
         ' _comm_set_elements%(x,v,same:$true,arg...) do'//&
         ' a:=get_slice(x.distr.dom,arg...); '//&
         ' for each j in a do x{j}=v endfor endproc',line)
    call dcl_proc(parser,&
         'get_remote%(a:invar any#rec _ddom{align,dom,dist,tile},b:long,c:long)->*a',&
         op_get_remote_distr,&
         0_pm_i16,line,0,'',ftn_get_remote_distr)
    call dcl_proc(parser,&
         'put_remote%(a:invar any#rec _ddom{align,dom,dist,tile},'//&
         'b:any,c:long,d:long)',&
         op_put_remote_distr,&
         0_pm_i16,line,0,'',ftn_put_remote_distr)

    ! Communicating operations on invariant arrays
    call dcl_uproc(parser,'{}%(v,arg...)=proc{[]}(v,arg...)',line)
    call dcl_uproc(parser,'{}%<this_distr:null>(&v:any#dom,e,arg...:index) do'//&
         ' check "Types do not match in {}=": same_type(arb(v),e);'//&
         ' PM__setaelem(v,index(dom(v),arg...),e) endproc',line)
    call dcl_uproc(parser,'{}%<this_distr:dist>(&v:any#dom,e,arg...:index) do'//&
         ' check "Types do not match in {}=": same_type(arb(v),e);'//&
         ' i:=index(dom(v),arg...);PM__setaelem(v,i,e);'//&
         ' _up_aset%(v,i,e) endproc',line)
    call dcl_uproc(parser,'{}%(&x:any#dom,v,arg...:subs) do'//&
         ' check "Slice out of range":contains(x.distr.dom,arg...);'//&
         ' _comm_set_elements%(x,v,same_type(arb(x),v),arg...) endproc',line)
    call dcl_uproc(parser,'_up_aset%(v:invar,i,e) local do '//&
         ' for each j in 0..this_nprc()-1l do '//&
         '    k:=broadcast(i,j);f:=broadcast(e,j); '//&
         '    for m in k,g in f do '//&
         '        PM__setaelem(v,m,g) '//&
         '     endfor '//&
         '  endfor endproc',line)

    call dcl_proc(parser,'_pack(v:any,m:any,n:any,d:any)->over v,d',&
         op_pack,0_pm_i16,line,0,'',ftn_simple)
    call dcl_uproc(parser,'pack(v:any#dom,m:bool#dom) do '//&
         ' check "arrays do not conform":conform(dom(v),dom(m)); '//&
         ' result =_pack(v,m,n,grid(0..n-1l)) where n=count(m) endproc',line)
    call dcl_uproc(parser,'pack(vv:[],mm:[]) do'//&
         ' v:=vv;m:=mm; '//&
         ' result=_pack(v,m,n,grid(0..n-1l))'//&
         ' where n=count(m) endproc',line)

    ! Intrinsic reduction procedures
    call dcl_uproc(parser,'sum::[x] reduce(y)=x+y',line)
    call dcl_uproc(parser,'prod::[x] reduce(y)=x*y',line)
    call dcl_uproc(parser,'maxval::[x] reduce(y)=max(x,y)',line)
    call dcl_uproc(parser,'minval::[x] reduce(y)=min(x,y)',line)
    call dcl_uproc(parser,'allof::[x] reduce(y)=x and y',line)
    call dcl_uproc(parser,'anyof::[x] reduce(y)=x or y',line)
    call dcl_uproc(parser,'count::[x] = sum::z where z=(x=>1l||0l)',line)

    ! Array versions of intrinsic reductions
    call dcl_uproc(parser,'sum(x) '//&
         'do for i in x conc return y::=sum::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'prod(x) '//&
         'do for i in x conc return y::=prod::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'maxval(x) '//&
         'do for i in x conc return y::=maxval::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'minval(x) '//&
         'do for i in x conc return y::=minval::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'allof(x) '//&
         'do for i in x conc return y::=allof::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'anyof(x) '//&
         'do for i in x conc return y::=anyof::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'count(x) '//&
         'do for i in x conc return y::=count::i endfor;result=y endproc',line) 
    
    ! Keyword arguments
    call dcl_uproc(parser,'PM__getkey(x:any,y:any)=convert(x,y)',line)
    call dcl_uproc(parser,'PM__getkey(x:null,y:any)=y',line)
    call dcl_proc(parser,'PM__makekeys(arg...:any)->any',&
         op_makekeys,0_pm_i16,line,0,'',ftn_makekeys)
    call dcl_proc(parser,'PM__delkeys(arg...:any)->any',&
         op_delkeys,0_pm_i16,line,0,'',ftn_delkeys)
    call dcl_proc(parser,'PM__checkkeys(arg...:any)',&
         op_checkkeys,0_pm_i16,line,0,'',ftn_checkkeys)
    call dcl_proc(parser,'PM__getvkey(arg...:any)->any',&
         op_getvkey,0_pm_i16,line,0,'',ftn_getvkey)

    ! Select statement
    call dcl_uproc(parser,&
         'check_case(x,y,arg...)=e do e:=match(x,y); '//&
         'if not e then e=check_case(x,arg...) endif endproc',line)
    call dcl_uproc(parser,'check_case(x,y)= match(x,y)',line)
    call dcl_uproc(parser,'match(x,y)=x==y',line)
    call dcl_uproc(parser,&
         'match(x:num,y:range{real_num})=x>=y._lo and x<=y._hi',&
         line)

    ! Conditional operator
    call dcl_uproc(parser,&
         '=>(x,y,z) check "Incompatible types in ''=>''": '//&
         'same_type(y,z) do r:=z; if x then r=y endif;result=r endproc',&
         line)
        
    ! Variables
    call dcl_uproc(parser,&
         'PM__assign(&a:any,b:any) do _assign(&a,b); '//&
         'check "Cannot assign values of different types":same_type(a,b) endproc',&
         line)
    call dcl_uproc(parser,&
         'PM__assign_var(&a:any,b:any) do _assign(&a,b); '//&
         'check "Cannot assign values of different types":same_type(a,b) endproc',&
         line)
    call dcl_proc(parser,'_assign(&any,any)',op_assign,0_pm_i16,line,0,&
         '',ftn_assign)
    call dcl_proc(parser,'_clone(x:any)->=x',op_clone,0_pm_i16,line,0,&
         '',ftn_dup)
    call dcl_uproc(parser,'PM__dup(x) each(x)=_clone(x)',line)
    call dcl_uproc(parser,'PM__pdup(x)=x',line)
    call dcl_proc(parser,'PM__getref(x:any)->=x',op_get_rf,0_pm_i16,line,0,&
         '',ftn_getref)
    call dcl_proc(parser,'same_type(x:any,y:any)->==x,y',&
         op_has_same_type,0_pm_i16,line,0,'',ftn_has_same_type)
    call dcl_proc(parser,'==(any,any)->bool',op_eq,0_pm_i16,line,0,'',ftn_eq)
    call dcl_proc(parser,'/=(any,any)->bool',op_ne,0_pm_i16,line,0,'',ftn_ne)

    call dcl_uproc(parser,'next_enum(x:any_int)=x+convert(1,x)',line)
    call dcl_uproc(parser,'next_enum(x:any_int,y:any_int)=x+convert(y,x)',line)
    
  end subroutine sysdefs

end module pm_sysdefs
