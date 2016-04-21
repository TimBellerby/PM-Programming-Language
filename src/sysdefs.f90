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

module pm_sysdefs
  use pm_kinds
  use pm_memory
  use pm_parser
  use pm_types
  implicit none

  integer,parameter:: pm_max_stack=127
  integer(pm_p),parameter:: proc_is_pass = 1
  integer(pm_p),parameter:: proc_global_return = 2
  integer(pm_p),parameter:: proc_is_generator = 3

  integer(pm_i16),parameter:: op_call=0
  integer(pm_i16),parameter:: op_poly_call=1
  integer(pm_i16),parameter:: first_jmp_op=3
  integer(pm_i16),parameter:: op_jmp=3
  integer(pm_i16),parameter:: op_jmp_any_ve=4
  integer(pm_i16),parameter:: op_jmp_empty_ve=5
  integer(pm_i16),parameter:: op_and_jmp_none=6
  integer(pm_i16),parameter:: op_andnot_jmp_none=7
  integer(pm_i16),parameter:: op_and_jmp_any=8
  integer(pm_i16),parameter:: op_andnot_jmp_any=9

  integer(pm_i16),parameter:: last_jmp_op=9

  integer(pm_i16),parameter:: index_op = last_jmp_op
  integer(pm_i16),parameter:: op_struct = index_op +1
  integer(pm_i16),parameter:: op_rec = index_op +2
  integer(pm_i16),parameter:: op_array= index_op + 3
  integer(pm_i16),parameter:: op_any= index_op + 5
  integer(pm_i16),parameter:: op_elem= index_op + 7
  integer(pm_i16),parameter:: op_set_elem= index_op + 8
  integer(pm_i16),parameter:: op_set_elem_idx= index_op + 9
  integer(pm_i16),parameter:: op_poly_elem= index_op + 11
  integer(pm_i16),parameter:: op_set_poly_elem= index_op + 13
  integer(pm_i16),parameter:: op_set_poly_elem_idx= index_op + 15

  integer(pm_i16),parameter:: op_misc = op_set_poly_elem_idx
  integer(pm_i16),parameter:: op_import= op_misc + 1
  integer(pm_i16),parameter:: op_export= op_misc + 2
  integer(pm_i16),parameter:: op_extract = op_misc + 3
  integer(pm_i16),parameter:: op_return= op_misc + 4
  integer(pm_i16),parameter:: op_par_loop_end= op_misc + 5
  integer(pm_i16),parameter:: op_vcall = op_misc + 6

  integer(pm_i16),parameter:: op_check= op_misc + 7
  integer(pm_i16),parameter:: op_dump= op_misc + 8
  integer(pm_i16),parameter:: op_print= op_misc + 10
  integer(pm_i16),parameter:: op_concat= op_misc + 12
  integer(pm_i16),parameter:: op_lookup_error = op_misc + 14
  integer(pm_i16),parameter:: op_makekeys = op_misc + 15
  integer(pm_i16),parameter:: op_delkeys = op_misc + 17
  integer(pm_i16),parameter:: op_checkkeys = op_misc + 19
  integer(pm_i16),parameter:: op_getvkey = op_misc + 21
  integer(pm_i16),parameter:: op_var_call = op_misc + 23
  integer(pm_i16),parameter:: op_make_dr = op_misc + 24
  integer(pm_i16),parameter:: op_get_dr = op_misc + 26
  integer(pm_i16),parameter:: op_set_dr = op_misc + 28
  integer(pm_i16),parameter:: op_make_rf = op_misc + 30
  integer(pm_i16),parameter:: op_get_rf = op_misc + 32
  integer(pm_i16),parameter:: op_set_rf = op_misc + 34
  integer(pm_i16),parameter:: op_index = op_misc + 40
  integer(pm_i16),parameter:: op_check_logical = op_misc + 42

  integer(pm_i16),parameter:: op_array_get_elem = op_misc + 43
  integer(pm_i16),parameter:: op_array_set_elem = op_misc + 44
  integer(pm_i16),parameter:: op_array_elems = op_misc + 45
  integer(pm_i16),parameter:: op_make_array = op_misc + 46
  integer(pm_i16),parameter:: op_import_val = op_misc + 47
  integer(pm_i16),parameter:: op_extractelm = op_misc + 48
  integer(pm_i16),parameter:: op_iota = op_misc + 49
  integer(pm_i16),parameter:: op_get_key = op_misc + 50
  integer(pm_i16),parameter:: op_get_key2 = op_misc + 51
  integer(pm_i16),parameter:: op_export_array = op_misc + 52
  integer(pm_i16),parameter:: op_default = op_misc + 53
  
  integer(pm_i16),parameter:: first_assign_op= op_misc + 54
  integer(pm_i16),parameter:: op_par_loop= first_assign_op
  integer(pm_i16),parameter:: op_setref= first_assign_op + 2
  integer(pm_i16),parameter:: op_clone= first_assign_op + 4
  integer(pm_i16),parameter:: op_clone_ve = first_assign_op + 5
  integer(pm_i16),parameter:: op_nullify_ve = first_assign_op + 6
  integer(pm_i16),parameter:: op_assign = first_assign_op + 7
  integer(pm_i16),parameter:: last_assign_op = first_assign_op+7
  
  integer(pm_i16),parameter:: op_string_l = last_assign_op +1
  integer(pm_i16),parameter:: op_and = last_assign_op + 2
  integer(pm_i16),parameter:: op_or = last_assign_op + 3
  integer(pm_i16),parameter:: op_not = last_assign_op + 4
  integer(pm_i16),parameter:: op_assign_l = last_assign_op +5

  integer(pm_i16),parameter:: op_start_i=op_assign_l
  integer(pm_i16),parameter:: op_add_i=op_start_i+1
  integer(pm_i16),parameter:: op_sub_i=op_start_i+2
  integer(pm_i16),parameter:: op_mult_i=op_start_i+3
  integer(pm_i16),parameter:: op_divide_i=op_start_i+4
  integer(pm_i16),parameter:: op_div_i=op_start_i+5
  integer(pm_i16),parameter:: op_mod_i=op_start_i+6
  integer(pm_i16),parameter:: op_pow_i=op_start_i+7
  integer(pm_i16),parameter:: op_uminus_i=op_start_i+8
  integer(pm_i16),parameter:: op_eq_i=op_start_i+9
  integer(pm_i16),parameter:: op_ne_i=op_start_i+10
  integer(pm_i16),parameter:: op_gt_i=op_start_i+11
  integer(pm_i16),parameter:: op_ge_i=op_start_i+12
  integer(pm_i16),parameter:: op_string_i=op_start_i+13
  integer(pm_i16),parameter:: op_get_elt_i=op_start_i+14
  integer(pm_i16),parameter:: op_set_elt_i=op_start_i+15
  integer(pm_i16),parameter:: op_long_i = op_start_i+16
  integer(pm_i16),parameter:: op_max_i = op_start_i+17
  integer(pm_i16),parameter:: op_min_i = op_start_i+18
  integer(pm_i16),parameter:: op_assign_i = op_start_i + 19

  integer(pm_i16),parameter:: op_start_ln =op_assign_i
  integer(pm_i16),parameter:: op_add_ln=op_start_ln+1
  integer(pm_i16),parameter:: op_sub_ln=op_start_ln+2
  integer(pm_i16),parameter:: op_mult_ln=op_start_ln+3
  integer(pm_i16),parameter:: op_divide_ln=op_start_ln+4
  integer(pm_i16),parameter:: op_div_ln=op_start_ln+5
  integer(pm_i16),parameter:: op_mod_ln=op_start_ln+6
  integer(pm_i16),parameter:: op_pow_ln=op_start_ln+7
  integer(pm_i16),parameter:: op_uminus_ln=op_start_ln+8
  integer(pm_i16),parameter:: op_eq_ln=op_start_ln+9
  integer(pm_i16),parameter:: op_ne_ln=op_start_ln+10
  integer(pm_i16),parameter:: op_gt_ln=op_start_ln+11
  integer(pm_i16),parameter:: op_ge_ln=op_start_ln+12
  integer(pm_i16),parameter:: op_string_ln=op_start_ln+13
  integer(pm_i16),parameter:: op_get_elt_ln=op_start_ln+14
  integer(pm_i16),parameter:: op_set_elt_ln=op_start_ln+15
  integer(pm_i16),parameter:: op_int_ln = op_start_ln+16
  integer(pm_i16),parameter:: op_max_ln = op_start_ln+17
  integer(pm_i16),parameter:: op_min_ln = op_start_ln+18
  integer(pm_i16),parameter:: op_assign_ln = op_start_ln+19

  integer(pm_i16),parameter:: op_get_elt=op_assign_ln+1
  integer(pm_i16),parameter:: op_set_elt=op_assign_ln+2
  integer,parameter:: num_op=op_set_elt

  integer,parameter:: hook = last_parser_hook
  integer,parameter:: sym_pm_system = hook+1
  integer,parameter:: sym_get_element = hook+2
  integer,parameter:: sym_set_element = hook+3
  integer,parameter:: sym_num_elements= hook+4
  integer,parameter:: sym_import_val = hook+5
  integer,parameter:: sym_import = hook+6
  integer,parameter:: sym_export = hook+7 
  integer,parameter:: sym_partition = hook+8
  integer,parameter:: sym_check_conform = hook+9
  integer,parameter:: sym_dup = hook + 10
  integer,parameter:: sym_sync_it = hook + 11
  integer,parameter:: sym_prc_grid = hook + 12
  integer,parameter:: sym_this_prc = hook + 13
  integer,parameter:: sym_grid = hook + 14
  integer,parameter:: sym_indices = hook + 15
  integer,parameter:: sym_make_ref = hook + 16
  integer,parameter:: sym_get_ref = hook + 17
  integer,parameter:: sym_set_ref = hook + 18
  integer,parameter:: sym_make_dotref = hook + 19
  integer,parameter:: sym_make_subref = hook + 20
  integer,parameter:: sym_make_openref = hook + 21
  integer,parameter:: sym_index = hook + 22
  integer,parameter:: sym_convert = hook + 23
  integer,parameter:: sym_d1= hook + 24
  integer,parameter:: sym_d2= hook + 25
  integer,parameter:: sym_d3= hook + 26
  integer,parameter:: sym_d4= hook + 27
  integer,parameter:: sym_d5= hook + 28
  integer,parameter:: sym_d6= hook + 29
  integer,parameter:: sym_d7= hook + 30
  integer,parameter:: sym_getkey = hook + 31
  integer,parameter:: sym_makekeys = hook + 32
  integer,parameter:: sym_delkeys = hook + 33
  integer,parameter:: sym_checkkeys = hook + 34
  integer,parameter:: sym_getvkey = hook + 35
  integer,parameter:: sym_assignment = hook + 36
  integer,parameter:: sym_first = hook + 37
  integer,parameter:: sym_next = hook + 38
  integer,parameter:: sym_checkcase = hook + 39
  integer,parameter:: sym_dim1= hook + 40
  integer,parameter:: sym_dim2= hook + 41
  integer,parameter:: sym_dim3= hook + 42
  integer,parameter:: sym_dim4= hook + 43
  integer,parameter:: sym_dim5= hook + 44
  integer,parameter:: sym_dim6= hook + 45
  integer,parameter:: sym_dim7= hook + 46
  integer,parameter:: sym_import2 = hook + 47
  integer,parameter:: sym_at1= hook + 48
  integer,parameter:: sym_at1_sub= hook + 49
  integer,parameter:: sym_at1_open= hook + 50
  integer,parameter:: sym_at2= hook + 51
  integer,parameter:: sym_at2_gbl= hook + 52
  integer,parameter:: sym_at2_reduce= hook + 53
  integer,parameter:: sym_at2_reduce_gbl= hook + 54
  integer,parameter:: sym_start_find = hook + 55
  integer,parameter:: sym_test_find = hook + 56
  integer,parameter:: sym_first_found = hook + 57
  integer,parameter:: sym_sync_find = hook + 58
  integer,parameter:: sym_generate = hook + 59
  integer,parameter:: sym_extract = hook + 60
  integer,parameter:: sym_broadcast = hook + 61
  integer,parameter:: sym_single = hook + 62
  integer,parameter:: sym_pop_prc = hook + 63
  integer,parameter:: sym_set_param = hook + 64
  integer,parameter:: sym_get_param = hook + 65
  integer,parameter:: sym_extract_elems = hook + 66
  integer,parameter:: sym_make_array = hook + 67
  integer,parameter:: sym_this_shape = hook + 68
  integer,parameter:: sym_this_distr = hook + 69
  integer,parameter:: sym_this_tile = hook + 70
  integer,parameter:: sym_this_tile_size = hook + 71
  integer,parameter:: sym_concurrent = hook + 72
  integer,parameter:: sym_pop_conc = hook + 73
  integer,parameter:: sym_export_array = hook + 74
  integer,parameter:: sym_opt_num = hook + 75
  integer,parameter:: num_syshook = 75

  character(len=20),dimension(0:num_op):: op_names
  character(len=14),dimension(num_syshook),parameter:: syshook = (/ &
       'PM__system    ','PM__get_elem  ','PM__set_elem  ','num_elem      ',&
       'PM__import_val','PM__import    ','PM__export    ','PM__partition ',&
       'check_conform ','PM__dup       ','PM__sync      ','prc_grid      ',&
       'this_prc      ','grid          ','indices       ','PM__ref       ',&
       'PM__getref    ','PM__setref    ','PM__dotref    ','PM__subref    ',&
       'PM__openref   ','index         ','convert       ',&
       'd1            ','d2            ','d3            ','d4            ',&
       'd5            ','d6            ','d7            ',&
       'PM__getkey    ','PM__makekeys  ','PM__delkeys   ','PM__checkkeys ',&
       'PM__getvkey   ',&
       'PM__assign    ','first         ','next          ','check_case    ',&
       'dim1          ','dim2          ','dim3          ','dim4          ',&
       'dim5          ','dim6          ','dim7          ','PM__import2   ',&
       'PM__at1       ','PM__at1_sub   ','PM__at1_open  ','PM__at2       ',&
       'PM__at2_gbl   ','PM__at2_reduce','PM__at2_redgbl','PM__start_find',&
       'PM__test_find ','PM__firstfound','PM__sync_find ','PM__generate  ',&
       'PM__extract   ','broadcast     ','single_elem   ','PM__pop_prc   ',&
       'PM__set_param ','PM__get_param ','PM__extractelm','PM__makearray ',&
       'this_shape    ','this_distr    ','this_tile     ','this_tile_size',&
       'PM__conc      ','PM__pop_conc  ','PM__exparray  ','PM__opt_num   '/)

contains

  subroutine syshooks(parser)
    type(parse_state):: parser
    integer:: i,j
    do i=1,num_syshook
       if(syshook(i)(1:1)=='_') then
          j=lname_entry(parser,trim(syshook(i)(2:)))
          if(i+hook/=j) then
             write(*,*) 'L',i,i+num_sym,j,num_sym
             call pm_panic('Setting syshooks')
          endif
       else
          j=name_entry(parser,trim(syshook(i)))
          if(i+hook/=j) then
             write(*,*) i,i+hook,j,hook,trim(syshook(i))
             call pm_panic('Setting syshooks')
          endif
       endif
    enddo
  end subroutine syshooks
 
  subroutine sysdefs(parser)
    type(parse_state):: parser
    integer:: line
    line=1

    call dcl_module(parser,'PM__system')
    parser%sysmodl=parser%modl
    call syshooks(parser)
    call init_typ(parser%context)
 
    ! String type
    call dcl_proc(parser,'print(string)',op_print,0_pm_i16,line,0)
    call dcl_uproc(parser,'print(x) do print(string(x)) endproc',line)
    call dcl_proc(parser,'//(string,string)->string',op_concat,0_pm_i16,line,0)
    call dcl_uproc(parser,'//(x:string,y)=proc //(x,string(y))',line)
    call dcl_uproc(parser,'//(x,y)=proc //(string(x),string(y))',line)
    call dcl_uproc(parser,'string(x:string)=x',line)
    call dcl_uproc(parser,'string(x:null)="null"',line)

    ! Int type
    call dcl_proc(parser,'PM__assign(&int,int)',op_assign_i,0_pm_i16,line,0)
    call dcl_proc(parser,'mod(int,int)->int',op_mod_i,0_pm_i16,line,0)
    call dcl_proc(parser,'==(int,int)->bool',op_eq_i,0_pm_i16,line,0)
    call dcl_proc(parser,'/=(int,int)->bool',op_ne_i,0_pm_i16,line,0)
    call dcl_proc(parser,'>=(int,int)->bool',op_ge_i,0_pm_i16,line,0)
    call dcl_proc(parser,'>(int,int)->bool',op_gt_i,0_pm_i16,line,0)
    call dcl_proc(parser,'+(int,int)->int',op_add_i,0_pm_i16,line,0)
    call dcl_proc(parser,'-(int,int)->int',op_sub_i,0_pm_i16,line,0)
    call dcl_proc(parser,'*(int,int)->int',op_mult_i,0_pm_i16,line,0)
    call dcl_proc(parser,'/(int,int)->int',op_divide_i,0_pm_i16,line,0)
    call dcl_proc(parser,'**(int,int)->int',op_pow_i,0_pm_i16,line,0)
    call dcl_proc(parser,'max(int,int)->int',op_max_i,0_pm_i16,line,0)
    call dcl_proc(parser,'min(int,int)->int',op_min_i,0_pm_i16,line,0)
    call dcl_proc(parser,'-(int)->int',op_uminus_i,0_pm_i16,line,0)
    call dcl_proc(parser,'string(int)->string',op_string_i,0_pm_i16,line,0)
    call dcl_proc(parser,'long(int)->long',op_long_i,0_pm_i16,line,0)
    call dcl_uproc(parser,'int(x:int)=x',line)

    ! Long type
    call dcl_proc(parser,'PM__assign(&long,long)',op_assign_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'mod(long,long)->long',op_mod_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'==(long,long)->bool',op_eq_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'/=(long,long)->bool',op_ne_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'>=(long,long)->bool',op_ge_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'>(long,long)->bool',op_gt_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'+(long,long)->long',op_add_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'-(long,long)->long',op_sub_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'*(long,long)->long',op_mult_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'/(long,long)->long',op_divide_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'**(long,long)->long',op_pow_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'max(long,long)->long',op_max_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'min(long,long)->long',op_min_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'-(long)->long',op_uminus_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'string(long)->string',op_string_ln,0_pm_i16,line,0)
    call dcl_proc(parser,'int(long)->int',op_int_ln,0_pm_i16,line,0)
    call dcl_uproc(parser,'long(x:long)=x',line)

    ! Bool type
    call dcl_proc(parser,'PM__assign(&bool,bool)',op_assign_l,0_pm_i16,line,0)
    call dcl_proc(parser,'string(bool)->string',op_string_l,0_pm_i16,line,0)
    call dcl_proc(parser,'and(bool,bool)->bool',op_and,0_pm_i16,line,0)
    call dcl_proc(parser,'or(bool,bool)->bool',op_or,0_pm_i16,line,0)
    call dcl_proc(parser,'not(bool)->bool',op_not,0_pm_i16,line,0)

    call dcl_proc(parser,'dump(any)',op_dump,0_pm_i16,line,0)
    call dcl_uproc(parser,'dumpit(a)=a do dump(a) endproc',line)
    
    ! Abstract numeric types
    call dcl_type(parser,'std_int is int,long',line)
    call dcl_type(parser,'any_int is std_int,int8,int16,int32,int64,int128',line)
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
    call dcl_uproc(parser,'convert(x,y:long)=long(x)',line)
    call dcl_uproc(parser,'convert(x,y:int)=int(x)',line)

    ! Optional numeric/bool types
    call dcl_type(parser,'_base is num,bool',line)
    call dcl_type(parser,'optional{x} includes #<x>',line)
    call dcl_type(parser,'optional{x:_base} also includes struct _opt{_val:x,_there:bool}',line)
    call dcl_uproc(parser,'PM__opt_num(x,y)=struct _opt{_val=x,_there=true}',line)
    call dcl_uproc(parser,'PM__opt_num(x:null,y)=struct _opt{_val=y,_there=false}',line)
    call dcl_uproc(parser,'://(x:struct _opt{_val,_there},y:num) do z:=y;'//&
         'if x._there then z=x._val endif; result=z endproc ',line)
    call dcl_uproc(parser,&
         '://(x:struct _opt{_val,_there}#any,y:num)=z where z=for i in x do build @w where w=i://y endfor',line)
    call dcl_uproc(parser,'=>(there:bool,val:_base)=struct _opt{_val=val,_there=there}',line)
    call dcl_uproc(parser,'*(x:struct _opt{_val,_there})=x._val check x._there',line)
    call dcl_uproc(parser,'opt(x:_base)=struct _opt{_val=x,_there=true}',line)
    call dcl_uproc(parser,'null(x:_base)=struct _opt{_val=x,_there=false}',line)
    
    ! Tuple types
    call dcl_type(parser,'tuple{t1} is rec _tuple {d1:t1 }',line)
    call dcl_type(parser,'tuple{t1,t2} is rec _tuple{d1:t1,d2:t2}',line)
    call dcl_type(parser,&
         'tuple{t1,t2,t3} is'//&
         ' rec _tuple{d1:t1,d2:t2,d3:t3}',line)
    call dcl_type(parser,&
         'tuple{t1,t2,t3,t4} is'//&
         ' rec _tuple{d1:t1,d2:t2,d3:t3,d4:t4}',line)
    call dcl_type(parser,&
         'tuple{t1,t2,t3,t4,t5} is'//&
         ' rec _tuple{d1:t1,d2:t2,d3:t3,d4:t4,d5:t5}',line)
    call dcl_type(parser,&
         'tuple{t1,t2,t3,t4,'//&
         't5,t6} is'//&
         ' rec _tuple{d1:t1,d2:t2,d3:t3,d4:t4,d5:t5,d6:t6}',line)
    call dcl_type(parser,&
         'tuple{t1,t2,t3,t4,t5,'//&
         't6,t7} is'//&
         ' rec _tuple{d1:t1,d2:t2,d3:t3,d4:t4,d5:t5,d6:t6,d7:t7}',line)

    call dcl_type(parser,'tuple1d is tuple{}',line)
    call dcl_type(parser,'tuple2d is tuple{,}',line)
    call dcl_type(parser,'tuple3d is tuple{,,}',line)
    call dcl_type(parser,'tuple4d is tuple{,,,}',line)
    call dcl_type(parser,'tuple5d is tuple{,,,,}',line)
    call dcl_type(parser,'tuple6d is tuple{,,,,,}',line)
    call dcl_type(parser,'tuple7d is tuple{,,,,,,}',line)
    call dcl_type(parser,'tuple is tuple2d,tuple3d,tuple4d,tuple5d,tuple6d,tuple7d',line)
    call dcl_uproc(parser,'tuple(x)=rec _tuple{d1=x}',line)
    call dcl_uproc(parser,'tuple(x,y)=rec _tuple{d1=x,d2=y}',line)
    call dcl_uproc(parser,'tuple(x,y,z)=rec _tuple{d1=x,d2=y,d3=z}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t)=rec _tuple{d1=x,d2=y,d3=z,d4=t}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u)=rec _tuple{d1=x,d2=y,d3=z,d4=t,d5=u}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u,v)=rec _tuple{d1=x,d2=y,d3=z,d4=t,d5=u,d6=v}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u,v,w)='//&
         'rec _tuple{d1=x,d2=y,d3=z,d4=t,d5=u,d6=v,d7=w}',line)

   call dcl_uproc(parser,'dim1(x:tuple)=x.d1',line)
   call dcl_uproc(parser,'dim2(x:tuple)=x.d2',line)
   call dcl_uproc(parser,'dim3(x:tuple)=x.d3',line)
   call dcl_uproc(parser,'dim4(x:tuple)=x.d4',line)
   call dcl_uproc(parser,'dim5(x:tuple)=x.d5',line)
   call dcl_uproc(parser,'dim6(x:tuple)=x.d6',line)
   call dcl_uproc(parser,'dim7(x:tuple)=x.d7',line)

    ! Range types
    call dcl_type(parser,'range_base includes real_num',line)
    call dcl_type(parser,'range{t:range_base} is rec _range{_lo:t,_hi:t}',line)
    call dcl_uproc(parser,'..(x:range_base,y:range_base)= rec _range{_lo=x,_hi=y}',line)
    call dcl_uproc(parser,'low(x:range{})=x._lo',line)
    call dcl_uproc(parser,'high(x:range{})=x._hi',line)
    call dcl_uproc(parser,'step(x:range{})=convert(1,x._lo)',line)
    call dcl_uproc(parser,'dom(x:range{any_int})=x',line)

    call dcl_uproc(parser,'in(x:range_base,y:range{})=x>=y._lo and x<=y._hi',line)
    call dcl_uproc(parser,'includes(x:range{},y:range{})=y._lo>=x._lo and y._hi<=x._hi',line)
    call dcl_uproc(parser,'num_elem(x:range{any_int})=long(x._hi-x._lo)+1l',line)
    call dcl_uproc(parser,'index(y:range{any_int},x:any_int)=long(x-y._lo)',line)
    call dcl_uproc(parser,&
         'from_index(y:range{any_int},x:long)=y._lo+convert(x,y._lo)',line)
    call dcl_uproc(parser,'first(x:range{any_int})=x._lo,null,x._lo<=x._hi',line)
    call dcl_uproc(parser,'next(x:range{any_int},y,z)=zz,null,zz<=x._hi where zz=z+1 ',line)
    call dcl_uproc(parser,'shape(x:range{any_int})=long(x._hi-x._lo)+1l',line)
    call dcl_uproc(parser,'(%%)(x:range{any_int},y:long)=z check x._lo<=z and z<=x._hi where '//&
         'z=x._lo+convert(y,x._lo)',line)
    
    ! Sequence types
    call dcl_type(parser,'seq{t:range_base} is rec _seq{_lo:t,_hi:t,_st:t,_n}',line)
    call dcl_uproc(parser,'by(x:range{},y)='//&
         'rec _seq{ _lo=x._lo,_hi=x._hi,_st=st,_n=long((x._hi-x._lo)/ st)+1l}'//&
         'where st=convert(y,x._lo)',line)
    call dcl_uproc(parser,'low(x:seq{})=x._lo',line)
    call dcl_uproc(parser,'high(x:seq{})=x._hi',line)
    call dcl_uproc(parser,'step(x:seq{})=x._st',line)
    call dcl_uproc(parser,'dom(x:seq{})=x',line)
    call dcl_uproc(parser,'num_elem(x:seq{})=x._n)',line)
    call dcl_uproc(parser,'in(x:range_base,y:seq{})=within do within:=true;'//&
         'if y._st>0 then within=x>=y._lo and x<=y._hi and (x-y._lo) mod y._st ==0 '//&
         'else within=x<=y._lo and x>=y._hi and (x-y._hi) mod -y._st ==0 endif endproc',line)
    call dcl_uproc(parser,&
         'includes(x:seq{},y:seq{})=y._lo in x and y._hi in x and y._lo+y._st in x',line)
    call dcl_uproc(parser,&
         'index(y:seq{},x:range_base)=long((x-y._lo)/ y._st)',line)
    call dcl_uproc(parser,&
         'from_index(y:seq{},x:long)=y._lo+convert(x,y._lo)*y._st',line)
    call dcl_uproc(parser,'first(x:seq{})=x._lo,null,x.lo<=x.hi',line)
    call dcl_uproc(parser,'next(x:seq{},y,z)=zz,null,zz<=x.hi where zz=z+x._st',line)
    call dcl_uproc(parser,'shape(x:seq{})=x._n',line)
    call dcl_uproc(parser,'(%%)(x:seq{},y:long)=z check x._lo<=z and z<=x._hi where '//&
         'z=x._lo+x._step*convert(y,x._lo)',line)
        
    ! Simple domains (0..n-1 long integer)
    call dcl_uproc(parser,'num_elem(x:long)=x',line)
    call dcl_uproc(parser,'shape(x:long)=x',line)
    call dcl_uproc(parser,'index(x:long,y:long)=y',line)
    call dcl_uproc(parser,'index(x:long,y:int)=long(y)',line)
    call dcl_uproc(parser,'from_index(x:long,y:long)=y',line)
    call dcl_uproc(parser,'first(x:long)=0l,null,x>0l',line)
    call dcl_uproc(parser,'next(x:long,s,i:long)=y,null,y<x where y=i+1l',line)
    call dcl_uproc(parser,'dom(x:long)=x',line)
    call dcl_uproc(parser,'low(x:long)=0l',line)
    call dcl_uproc(parser,'high(x:long)=x-1l',line)
    call dcl_uproc(parser,'step(x:long)=1l',line)
    call dcl_uproc(parser,'(%%)(x:long,y:long)=y check y>=0l and y<x',line)
    call dcl_uproc(parser,'(%%)(x:long,y:int)=z check z>=0l and z<x where z=long(y)',line)
    call dcl_uproc(parser,'in(x:long,y:long)=x>=0l and x<y',line)
    call dcl_uproc(parser,'in(x:int,y:long)=x>0 and long(x)<y',line)

    ! Simple domains (0..n-1 std integer)
    call dcl_uproc(parser,'num_elem(x:int)=long(x)',line)
    call dcl_uproc(parser,'shape(x:int)=long(x)',line)
    call dcl_uproc(parser,'index(x:int,y:int)=y',line)
    call dcl_uproc(parser,'from_index(x:int,y:long)=int(y)',line)
    call dcl_uproc(parser,'first(x:int)=0,null,x>0',line)
    call dcl_uproc(parser,'next(x:int,s,i:int)=y,null,y<x where y=i+1',line)
    call dcl_uproc(parser,'dom(x:int)=x',line)
    call dcl_uproc(parser,'low(x:int)=0',line)
    call dcl_uproc(parser,'high(x:int)=x-1',line)
    call dcl_uproc(parser,'step(x:int)=1',line)
    call dcl_uproc(parser,'(%%)(x:int,y:long)=int(y) check y>=0l and int(y)<x',line)
    call dcl_uproc(parser,'in(x:int,y:int)=x>=0 and x<y',line)
    
    ! Domain types
    call dcl_type(parser,&
         'dom includes std_int,range{any_int},seq{},grid,mat_dom{},vect_dom{}',line)
    call dcl_type(parser,'vect_dom{t:std_int} is rec _vect{_n,_s,d1:t}',line)
    call dcl_type(parser,'mat_dom{t:std_int} is rec _mat{_n,_s,d1:t,d2:t}',line)
    call dcl_type(parser,'_t{xx} includes xx',line)
    call dcl_type(parser,'_t{xx:any_int} also includes range{xx}',line)
    call dcl_type(parser,'_t{xx:range_base} also includes seq{xx}',line)

    call dcl_type(parser,'grid{t1:dom} includes rec _grid {_n,_s,d1:_t{t1}}',line)
    call dcl_type(parser,'grid{t1:std_int} also includes vect_dom{t1}',line)
    call dcl_type(parser,'grid{t1:dom,t2:dom} includes'//&
         ' rec _grid{_n,_s,d1:_t{t1},d2:_t{t2}}',line)
    call dcl_type(parser,'grid{t1:int,t2:int} also includes'//&
         ' mat_dom{int}',line)
    call dcl_type(parser,'grid{t1:long,t2:long} also includes'//&
         ' mat_dom{long}',line)
    call dcl_type(parser,&
         'grid{t1:dom,t2:dom,t3:dom} includes'//&
         ' rec _grid{_n,_s,d1:_t{t1},d2:_t{t2},d3:_t{t3}}',line)
    call dcl_type(parser,&
         'grid{t1:dom,t2:dom,t3:dom,t4:dom} includes'//&
         ' rec _grid{_n,_s,d1:_t{t1},d2:_t{t2},d3:_t{t3},d4:_t{t4}}',line)
    call dcl_type(parser,&
         'grid{t1:dom,t2:dom,t3:dom,t4:dom,t5:dom} includes'//&
         ' rec _grid{_n,_s,d1:_t{t1},d2:_t{t2},d3:_t{t3},d4:_t{t4},d5:_t{t5}}',line)
    call dcl_type(parser,&
         'grid{t1:dom,t2:dom,t3:dom,t4:dom,'//&
         't5:dom,t6:dom} includes'//&
         ' rec _grid{_n,_s,d1:_t{t1},d2:_t{t2},d3:_t{t3},d4:_t{t4},d5:_t{t5},d6:_t{t6}}',line)
    call dcl_type(parser,&
         'grid{t1:dom,t2:dom,t3:dom,t4:dom,t5:dom,'//&
         't6:dom,t7:dom} includes'//&
         ' rec _grid{_n,_s,d1:_t{t1},d2:_t{t2},d3:_t{t3},'//&
         'd4:_t{t4},d5:_t{t5},d6:_t{t6},d7:_t{t7}}',line)

    call dcl_uproc(parser,'grid(t1:dom)=rec _grid {_s=s,d1=t1,_n=n1}'//&
         ' where s=rec _grid{_s=null,d1=n1,_n=n1} where n1=shape(t1)',line)
    call dcl_uproc(parser,'grid(t1:dom,t2:dom)=rec _grid{_s=s,d1=t1,d2=t2,_n=nn}'//&
         ' where s=rec _grid{_s=null,d1=n1,d2=n2,_n=nn} where nn=num_elem(n1)*num_elem(n2) '//&
         ' where n1=shape(t1),n2=shape(t2)}',line)
    call dcl_uproc(parser,&
         'grid(t1:dom,t2:dom,t3:dom)='//&
         ' rec _grid{_s=s,d1=t1,d2=t2,d3=t3,_n=nn}'//&
         ' where s=rec _grid{_s=null,d1=n1,d2=n2,d3=n3,_n=nn} '//&
         ' where nn=num_elem(n1)*num_elem(n2)*num_elem(n3) where'//&
         ' n1=shape(t1),n2=shape(t2),n3=shape(t3)}',line)
    call dcl_uproc(parser,&
         'grid(t1:dom,t2:dom,t3:dom,t4:dom)='//&
         ' rec _grid{_s=s,d1=t1,d2=t2,d3=t3,d4=t4,_n=nn}'//&
         ' where s=rec _grid{_s=null,d1=n1,d2=n2,d3=n3,d4=n4,_n=nn}'//&
         ' where nn=num_elem(n1)*num_elem(n2)*num_elem(n3)*num_elem(n4)'//&
         ' where n1=shape(t1),n2=shape(t2),n3=shape(t3),n4=shape(t4)}',line)
   call dcl_uproc(parser,&
         'grid(t1:dom,t2:dom,t3:dom,t4:dom,t5:dom)='//&
         ' rec _grid{_s=s,d1=t1,d2=t2,d3=t3,d4=t4,d5=t5,_n=nn}'//&
         ' where s=rec _grid{_s=null,d1=n1,d2=n2,d3=n3,d4=n4,d5=n5,_n=nn}'//&
         ' where nn=num_elem(n1)*num_elem(n2)*num_elem(n3)*num_elem(n4)*num_elem(n5)'//&
         ' where n1=shape(t1),n2=shape(t2),n3=shape(t3),n4=shape(t4),n5=shape(t5)}',line)
   call dcl_uproc(parser,&
         'grid(t1:dom,t2:dom,t3:dom,t4:dom,t5:dom,t6:dom)='//&
         ' rec _grid{_s=s,d1=t1,d2=t2,d3=t3,d4=t4,d5=t5,d6=t6,_n=nn}'//&
         ' where s=rec _grid{_s=null,d1=n1,d2=n2,d3=n3,d4=n4,d5=n5,d6=n6,_n=nn}'//&
         ' where nn=num_elem(n1)*num_elem(n2)*num_elem(n3)*num_elem(n4)*num_elem(n5)*num_elem(n6)'//&
         ' where n1=shape(t1),n2=shape(t2),n3=shape(t3),'//&
         ' n4=shape(t4),n5=shape(t5),n6=shape(t6)}',line)
   call dcl_uproc(parser,&
         'grid(t1:dom,t2:dom,t3:dom,t4:dom,t5:dom,t6:dom,t7:dom)='//&
         ' rec _grid{_s=s,d1=t1,d2=t2,d3=t3,d4=t4,d5=t5,d6=t6,d7=t7,_n=nn}'//&
         ' where s=rec _grid{_s=null,d1=n1,d2=n2,d3=n3,d4=n4,d5=n5,d6=n6,d7=t7,_n=nn}'//&
         ' where nn=num_elem(n1)*num_elem(n2)*num_elem(n3)*num_elem(n4)'//&
         '  *num_elem(n5)*num_elem(n6)*num_elem(n7)'//&
         ' where n1=shape(t1),n2=shape(t2),n3=shape(t3),n4=shape(t4),'//&
         ' n5=shape(t5),n6=shape(t6),n7=shape(t7)}',line)

   call dcl_uproc(parser,'dom(x:grid)=x',line)
   call dcl_uproc(parser,'dim1(x:dom)=x',line)
   call dcl_uproc(parser,'dim1(x:grid)=x.d1',line)
   call dcl_uproc(parser,'dim2(x:grid)=x.d2',line)
   call dcl_uproc(parser,'dim3(x:grid)=x.d3',line)
   call dcl_uproc(parser,'dim4(x:grid)=x.d4',line)
   call dcl_uproc(parser,'dim5(x:grid)=x.d5',line)
   call dcl_uproc(parser,'dim6(x:grid)=x.d6',line)
   call dcl_uproc(parser,'dim7(x:grid)=x.d7',line)

   call dcl_uproc(parser,&
        'vect_dom(rows)=rec _vect{_s=rec _vect{_s=null,d1=long(rows)},d1=rows}',line)
   call dcl_uproc(parser,'dom(x:vect_dom{})=x',line)
   call dcl_uproc(parser,'low(x:vect_dom{})=low(x.d1)',line)
   call dcl_uproc(parser,'high(x:vect_dom{})=high(x.d1)',line)
   call dcl_uproc(parser,'step(x:vect_dom{})=step(x.d1)',line)
   
   call dcl_uproc(parser,&
        'mat_dom(rows,cols)=rec _mat{_s=rec _mat{_s=null,d1=long(rows),d2=long(cols)},'//&
        'd1=rows,d2=cols}',line)
   call dcl_uproc(parser,'low(x:mat_dom{})=low(x.d1)',line)
   call dcl_uproc(parser,'high(x:mat_dom{})=high(x.d1)',line)
   call dcl_uproc(parser,'step(x:mat_dom{})=step(x.d1)',line)
   
   call dcl_type(parser,'grid1d is grid{}',line)
   call dcl_type(parser,'grid2d is grid{,}',line)
   call dcl_type(parser,'grid3d is grid{,,}',line)
   call dcl_type(parser,'grid4d is grid{,,,}',line)
   call dcl_type(parser,'grid5d is grid{,,,,}',line)
   call dcl_type(parser,'grid6d is grid{,,,,,}',line)
   call dcl_type(parser,'grid7d is grid{,,,,,,}',line)
   call dcl_type(parser,'grid is grid1d,grid2d,grid3d,grid4d,grid5d,grid6d,grid7d',line)

   call dcl_uproc(parser,'shape(x:grid)=x._s://x',line)
   call dcl_uproc(parser,'num_elem(x:grid)=x._n',line)
   
   ! Array indexing
   call dcl_proc(parser,'_idx(arg...:long)->long',op_index,0_pm_i16,line,0)
   call dcl_uproc(parser,'index(g:grid1d,s1)=index(g.d1,s1)',line)
   call dcl_uproc(parser,'index(g:grid2d,s1,s2)='//&
        '_idx(index(g.d1,s1),gs.d1,index(g.d2,s2),gs.d2) where gs=g._s',line)
   call dcl_uproc(parser,'index(g:grid3d,s1,s2,s3)='//&
        '_idx(index(g.d1,s1),gs.d1,index(g.d2,s2),gs.d2,index(g.d3,s3),gs.d3) where gs=g._s'&
        ,line)
   call dcl_uproc(parser,&
        'index(g:grid4d,s1,s2,s3,s4)='//&
        '_idx(index(g.d1,s1),gs.d1,index(g.d2,s2),gs.d2,index(g.d3,s3),gs.d3,'//&
        'index(g.d4,s4),gs.d4) where gs=g._s',line)
   call dcl_uproc(parser,&
        'index(g:grid5d,s1,s2,s3,s4,'//&
        's5)='//&
        '_idx(index(g.d1,s1),gs.d1,index(g.d2,s2),gs.d2,index(g.d3,s3),gs.d3,'//&
         'index(g.d4,s4),gs.d4,index(g.d5,s5),gs.d5) where gs=g._s',line)
   call dcl_uproc(parser,&
        'index(g:grid6d,s1,s2,s3,s4,'//&
        's5,s6)='//&
        '_idx(index(g.d1,s1),gs.d1,index(g.d2,s2),gs.d2,index(g.d3,s3),gs.d3,'//&
        'index(g.d4,s4),gs.d4,index(g.d5,s5),gs.d5,index(g.d6,s6),gs.d6) where gs=g._s',line)
   call dcl_uproc(parser,&
        'index(g:grid7d,s1,s2,s3,s4,'//&
        's5,s6,s7)='//&
        '_idx(index(g.d1,s1),gs.d1,index(g.d2,s2),gs.d2,index(g.d3,s3),gs.d3,'//&
        'index(g.d4,s4),gs.d4,index(g.d5,s5),gs.d5,index(g.d6,s6),gs.d6,'//&
        'index(g.d7,s7),gs.d7) where gs=g._s',line)
   
   call dcl_uproc(parser,'first(d:grid1d)=(j1),rec _gs{d1=s1},num_elem(d)>0'//&
        ' where j1,s1,_=first(d.d1)',line)
   call dcl_uproc(parser,'first(d:grid2d)=(j1,j2),rec _gs{d1=s1,d2=s2},num_elem(d)>0'//&
        ' where j1,s1,_=first(d.d1),j2,s2,_=first(d.d2)',line)

   call dcl_uproc(parser,'(%%)(x:grid1d,y:tuple1d)=( x.d1(% y.d1 %) )',line)
   call dcl_uproc(parser,'(%%)(x:grid2d,y:tuple2d)=( x.d1(% y.d1 %), x.d2(% y.d2 %) )',line)
   call dcl_uproc(parser,'(%%)(x:grid3d,y:tuple3d)=( x.d1(% y.d1 %), x.d2(% y.d2 %) , x.d3(% y.d3 %) )',line)
   call dcl_uproc(parser,'(%%)(x:grid4d,y:tuple4d)=( x.d1(% y.d1 %), x.d2(% y.d2 %) ,'//&
        ' x.d3(% y.d3 %) , x.d4(% y.d4 %) )',line)
   call dcl_uproc(parser,'(%%)(x:grid5d,y:tuple5d)=( x.d1(% y.d1 %), x.d2(% y.d2 %) ,'//&
        ' x.d3(% y.d3 %) , x.d4(% y.d4 %), x.d5 (% y.d5 %) )',line)
   call dcl_uproc(parser,'(%%)(x:grid6d,y:tuple6d)=( x.d1(% y.d1 %), x.d2(% y.d2 %) ,'//&
        ' x.d3(% y.d3 %) , x.d4(% y.d4 %), x.d5 (% y.d5 %), x.d6 (% y.d6 %) )',line)
   call dcl_uproc(parser,'(%%)(x:grid7d,y:tuple7d)=( x.d1(% y.d1 %), x.d2(% y.d2 %) ,'//&
        ' x.d3(% y.d3 %) , x.d4(% y.d4 %), x.d5 (% y.d5 %), x.d6(% y.d6 %), x.d7 (% y.d7 %) )',line)

   call dcl_uproc(parser,'next(d:grid1d,g:rec _gs{d1},i:tuple1d)'//&
        '=j,s,e where j,s,e=next(d.d1,g.d1,i.d1)',line)
   call dcl_uproc(parser,'next(d:grid2d,g:rec _gs{d1,d2},i:tuple2d) do '//&
        'j1,s1,e:=next(d.d1,g.d1,i.d1);j2:=i.d2;s2:=g.d2;'//&
        'if not e then j1,s1,_=first(d.d1);j2,s2,e=next(d.d2,g.d2,i.d2) endif;'//&
        'result=(j1,j2),rec _gs{d1=s1,d2=s2},e endproc',line)

   call dcl_proc(parser,&
        '_iota(siz:long,start:long,finish:long,incr:long,totsiz:long)->long',op_iota,0_pm_i16,line,&
        proc_is_generator)

   call dcl_uproc(parser,'_gridit(d:int,x:int)=x..x',line)
   call dcl_uproc(parser,'_gridit(d:long,x:long)=x..x',line)
   call dcl_uproc(parser,'_gridit(d:long,x:int)=x..x',line)
   call dcl_uproc(parser,'_gridit(d:range{int},x:int)=x..x',line)
   call dcl_uproc(parser,'_gridit(d:range{long},x:long)=x..x',line)
   call dcl_uproc(parser,'_gridit(d:range{long},x:int)=x..x',line)
   call dcl_uproc(parser,'_gridit(d:seq{},x)=y..y by step(d) where y=convert(x,low(d))',line)
   call dcl_uproc(parser,'_gridit(d:int,x:range{int})=x',line)
   call dcl_uproc(parser,'_gridit(d:long,x:range{long})=x',line)
   call dcl_uproc(parser,'_gridit(d:long,x:range{int})=low(x)..high(x)',line)
   call dcl_uproc(parser,'_gridit(d:range{int},x:range{int})=x',line)
   call dcl_uproc(parser,'_gridit(d:range{long},x:range{long})=x',line)
   call dcl_uproc(parser,'_gridit(d:range{long},x:range{int})=x',line) 
   call dcl_uproc(parser,'_gridit(d:seq{},x:range{})=x by step(d)',line)
   call dcl_uproc(parser,'_gridit(d:int,x:seq{int})=x',line)
   call dcl_uproc(parser,'_gridit(d:long,x:seq{long})=x',line)
   call dcl_uproc(parser,'_gridit(d:long,x:seq{int})=x',line)
   call dcl_uproc(parser,'_gridit(d:range{int},x:seq{int})=x',line)
   call dcl_uproc(parser,'_gridit(d:range{long},x:seq{long})=x',line)
   call dcl_uproc(parser,'_gridit(d:range{long},x:seq{int})=x',line)
   call dcl_uproc(parser,'_gridit(d:seq{},x:seq{})=x',line)
   call dcl_uproc(parser,'_gridit(d,x,y)=grid(_gridit(d,x),_gridit(d,y))',line)
   call dcl_uproc(parser,'_gridit(d,x,y,z)=grid(_gridit(d,x),_gridit(d,y),_gridit(d,z))',line)
   call dcl_uproc(parser,'_gridit(d,x,y,z,a)=grid(_gridit(d,x),_gridit(d,y),_gridit(d,z),_gridit(d,a))',line)
   call dcl_uproc(parser,'_gridit(d,x,y,z,a,b)=grid(_gridit(d,x),_gridit(d,y),_gridit(d,z),_gridit(d,a),'//&
        '_gridit(d,b))',line)
   call dcl_uproc(parser,'_gridit(d,x,y,z,a,b,c)=grid(_gridit(d,x),_gridit(d,y),_gridit(d,z),'//&
        '_gridit(d,a),_gridit(d,b),_gridit(d,c))',line)
   call dcl_uproc(parser,'_gridit(d,x,y,z,a,b,c,e)=grid(_gridit(d,x),_gridit(d,y),_gridit(d,z),'//&
        '_gridit(d,a),_gridit(d,b),_gridit(d,c),_gridit(d,e))',line)
   
   call dcl_uproc(parser,'_elts(x:long,siz,tot)=_iota(siz,0l,x-1l,1l,tot)',line)
   call dcl_uproc(parser,'_elts(x:range{long},siz,tot)=_iota(siz,x._lo,x._hi,1l,tot)',line)
   call dcl_uproc(parser,&
        '_elts(x:seq{long},siz,tot)=_iota(siz,x._lo,x._hi,x._st,tot)',line)
   call dcl_uproc(parser,&
        '_elts(x:grid2d,siz,tot)=( _elts( x.d1, siz, tot), _elts( x.d2, siz*num_elem(x.d1), tot ) )',line)
   call dcl_uproc(parser,&
        '_elts(x:grid3d,siz,tot)=( _elts( x.d1, siz, tot), _elts( x.d2, s, tot ),'//&
        '_elts( x.d3, s*num_elem(x.d2), tot)) where s=siz*num_elem(x.d1)',line)
   
   call dcl_uproc(parser,'PM__generate(x,n)=_elts(x,1l,n)',line)
   
   call dcl_uproc(parser,'check_conform(a)=shape(a)',line)
   call dcl_uproc(parser,&
        'check_conform(a,b)=shape(a) check shape(a)==shape(b)',line)
   call dcl_uproc(parser,&
        'check_conform(a,b,c)=check_conform(check_conform(a,b),c)',line)
   call dcl_uproc(parser,&
        'check_conform(a,b,c,arg...)='//&
        'check_conform(check_conform(check_conform(a,b),c),arg...)',line)
   
   ! Tuple index of grid
   call dcl_uproc(parser,'index(g:grid1d,t:tuple1d)=index(g,t.d1)',line)
   call dcl_uproc(parser,'index(g:grid2d,t:tuple2d)=index(g,t.d1,t.d2)',line)
   call dcl_uproc(parser,'index(g:grid3d,t:tuple3d)=index(g,t.d1,t.d2,t.d3)',line)
   call dcl_uproc(parser,'index(g:grid4d,t:tuple4d)=index(g,t.d1,t.d2,t.d3,t.d4)',line)
   call dcl_uproc(parser,'index(g:grid5d,t:tuple5d)=index(g,t.d1,t.d2,t.d3,t.d4,t.d5)',line)
   call dcl_uproc(parser,&
        'index(g:grid6d,t:tuple6d)=index(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6)',line)
   call dcl_uproc(parser,&
        'index(g:grid7d,t:tuple7d)=index(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6,t.d7)',line)
    
   ! Vector index of a grid


   ! Neighborhoods
   call dcl_uproc(parser,'nbd(x:long,y:range{long})=x+y._lo..x+y._hi',line)
   call dcl_uproc(parser,'nbd(x:long,y:range{int})=x+long(y._lo)..x+long(y._hi)',line)
   call dcl_uproc(parser,'nbd(x:int,y:range{int})=x+y._lo..x+y._hi',line)
   call dcl_uproc(parser,'nbd(x,y:range{})=x+y._lo..x+y._hi',line)
   call dcl_uproc(parser,'nbd(x:long,y:seq{long})=x+y._lo..x+y._hi by y._st',line)
   call dcl_uproc(parser,'nbd(x:long,y:seq{int})=x+long(y._lo)..x+long(y._hi) by long(y._st)',line)
   call dcl_uproc(parser,'nbd(x:int,y:seq{int})=x+y._lo..x+y._hi by y._st',line)
   call dcl_uproc(parser,'nbd(x,y:seq{})=x+y._lo..x+y._hi by y._st',line)
   call dcl_uproc(parser,'nbd(x:tuple2d,y:grid2d)=( nbd(x.d1,y.d1),nbd(x.d2,y.d2) )',line)
   call dcl_uproc(parser,'nbd(x:tuple3d,y:grid3d)=( nbd(x.d1,y.d1),nbd(x.d2,y.d2),nbd(x.d3,y.d3) )',line)
   call dcl_uproc(parser,'nbd(x:tuple4d,y:grid4d)=( nbd(x.d1,y.d1),nbd(x.d2,y.d2),nbd(x.d3,y.d3),nbd(x.d4,y.d4) )',line)
   call dcl_uproc(parser,'nbd(x:tuple5d,y:grid5d)=( nbd(x.d1,y.d1),nbd(x.d2,y.d2),nbd(x.d3,y.d3),nbd(x.d4,y.d4),'//&
        'nbd(x.d5,y.d5) )',line)
   call dcl_uproc(parser,'nbd(x:tuple6d,y:grid6d)=( nbd(x.d1,y.d1),nbd(x.d2,y.d2),nbd(x.d3,y.d3),nbd(x.d4,y.d4),'//&
        'nbd(x.d5,y.d5),nbd(x.d6,y.d6) )',line)
   call dcl_uproc(parser,'nbd(x:tuple7d,y:grid7d)=( nbd(x.d1,y.d1),nbd(x.d2,y.d2),nbd(x.d3,y.d3),nbd(x.d4,y.d4),'//&
        'nbd(x.d5,y.d5),nbd(x.d6,y.d6), nbd(x.d7,y.d7) )',line)

   ! Displacement
   call dcl_type(parser,'_intd is int,range{int},seq{int}',line)
   call dcl_type(parser,'_longd is long,range{long},seq{long}',line)
   call dcl_uproc(parser,'displace(d:_longd,x:long,y:long)=x+y',line)
   call dcl_uproc(parser,'displace(d:_longd,x:long,y:int)=x+long(y)',line)
   call dcl_uproc(parser,'displace(d:_intd,x:int,y:int)=x+y',line)
   call dcl_uproc(parser,'displace(d:grid2d,x:tuple2d,y:tuple2d)=(displace(x.d1,y.d1),displace(x.d2,y.d2)) ',line)
   call dcl_uproc(parser,'displace(d:grid3d,x:tuple3d,y:tuple3d)=(displace(x.d1,y.d1),displace(x.d2,y.d2), '//&
        'displace(x.d3,y.d3) ) ',line)
   call dcl_uproc(parser,'displace(d:grid4d,x:tuple4d,y:tuple4d)=(displace(x.d1,y.d1),displace(x.d2,y.d2), '//&
        'displace(x.d3,y.d3),displace(x.d4,y.d4) ) ',line)
   call dcl_uproc(parser,'displace(d:grid5d,x:tuple5d,y:tuple5d)=(displace(x.d1,y.d1),displace(x.d2,y.d2), '//&
        'displace(x.d3,y.d3),displace(x.d4,y.d4),displace(x.d5,y.d5) ) ',line)
   call dcl_uproc(parser,'displace(d:grid6d,x:tuple6d,y:tuple6d)=(displace(x.d1,y.d1),displace(x.d2,y.d2), '//&
        'displace(x.d3,y.d3),displace(x.d4,y.d4),displace(x.d5,y.d5),displace(x.d6,y.d6) ) ',line)
   call dcl_uproc(parser,'displace(d:grid7d,x:tuple7d,y:tuple7d)=(displace(x.d1,y.d1),displace(x.d2,y.d2), '//&
        'displace(x.d3,y.d3),displace(x.d4,y.d4),displace(x.d5,y.d5),displace(x.d6,y.d6),displace(x.d7,y.d7) ) ',line)

   ! Intersection of domains
   call dcl_uproc(parser,'intersect(x:long,y:long)=min(x,y)',line)
   call dcl_uproc(parser,'intersect(x:long,y:range{long})=max(0l,y._lo)..min(x,y._hi)',line)
   call dcl_uproc(parser,'intersect(x:long,y:seq{long})=max(0l,y._lo)..min(x,y._hi) by y._st',line)
   call dcl_uproc(parser,'intersect(x:range{long},y:long)=max(0l,x._lo)..min(x._hi,y)',line)
   call dcl_uproc(parser,'intersect(x:range{long},y:range{long})=max(x._lo,y._lo)..min(x._hi,y._hi)',line)
   call dcl_uproc(parser,'intersect(x:range{long},y:seq{long})=max(x._lo,y._lo)..min(x._hi,y._hi) by y._st',line)
   call dcl_uproc(parser,'intersect(x:seq{long},y:long)=max(x._lo,0l)..min(x._hi,y) by x._st',line)
   call dcl_uproc(parser,'intersect(x:seq{long},y:range{long})=max(x._lo,y._lo)..min(x._hi,y._hi) by x._st',line)
   call dcl_uproc(parser,'intersect(x:seq{long},y:seq{long})=max(x._lo,y._lo)..min(x._hi,y._hi) by max(x._st,y._st)',&
        line)

   call dcl_uproc(parser,'intersect(x:int,y:int)=min(x,y)',line)
   call dcl_uproc(parser,'intersect(x:int,y:range{int})=max(0l,y._lo)..min(x,y._hi)',line)
   call dcl_uproc(parser,'intersect(x:int,y:seq{int})=max(0l,y._lo)..min(x,y._hi) by y._st',line)
   call dcl_uproc(parser,'intersect(x:range{int},y:int)=max(0l,x._lo)..min(x._hi,y)',line)
   call dcl_uproc(parser,'intersect(x:range{int},y:range{int})=max(x._lo,y._lo)..min(x._hi,y._hi)',line)
   call dcl_uproc(parser,'intersect(x:range{int},y:seq{int})=max(x._lo,y._lo)..min(x._hi,y._hi) by y._st',line)
   call dcl_uproc(parser,'intersect(x:seq{int},y:int)=max(x._lo,0l)..min(x._hi,y) by x._st',line)
   call dcl_uproc(parser,'intersect(x:seq{int},y:range{int})=max(x._lo,y._lo)..min(x._hi,y._hi) by x._st',line)
   call dcl_uproc(parser,'intersect(x:seq{int},y:seq{int})=max(x._lo,y._lo)..min(x._hi,y._hi) by max(x._st,y._st)',&
        line)

   call dcl_uproc(parser,'intersect(x:tuple2d,y:tuple2d)=(intersect(x.d1,y.d1),intersect(x.d2,y.d2)) ',line)
   call dcl_uproc(parser,'intersect(x:tuple3d,y:tuple3d)=(intersect(x.d1,y.d1),intersect(x.d2,y.d2), '//&
        'intersect(x.d3,y.d3) ) ',line)
   call dcl_uproc(parser,'intersect(x:tuple4d,y:tuple4d)=(intersect(x.d1,y.d1),intersect(x.d2,y.d2), '//&
        'intersect(x.d3,y.d3),intersect(x.d4,y.d4) ) ',line)
   call dcl_uproc(parser,'intersect(x:tuple5d,y:tuple5d)=(intersect(x.d1,y.d1),intersect(x.d2,y.d2), '//&
        'intersect(x.d3,y.d3),intersect(x.d4,y.d4),intersect(x.d5,y.d5) ) ',line)
   call dcl_uproc(parser,'intersect(x:tuple6d,y:tuple6d)=(intersect(x.d1,y.d1),intersect(x.d2,y.d2), '//&
        'intersect(x.d3,y.d3),intersect(x.d4,y.d4),intersect(x.d5,y.d5),intersect(x.d6,y.d6) ) ',line)
   call dcl_uproc(parser,'intersect(x:tuple7d,y:tuple7d)=(intersect(x.d1,y.d1),intersect(x.d2,y.d2), '//&
        'intersect(x.d3,y.d3),intersect(x.d4,y.d4),intersect(x.d5,y.d5),intersect(x.d6,y.d6),'//&
        'intersect(x.d7,y.d7) ) ',line)
   
   ! Array types
    call dcl_type(parser,'array{e,d:dom} is e#d,_slice{e,d},e#grid{d},_slice{e,grid{d}}',line)
    call dcl_type(parser,&
         'array{e,d1:dom,d2:dom}'//&
         ' is e#grid{d1,d2},_slice{e,grid{d1,d2}}',line)
    call dcl_type(parser,&
         'array{e,d1:dom,d2:dom,d3:dom}'//&
         ' is e#grid{d1,d2,d3},_slice{e,grid{d1,d2,d3}}',line)
    call dcl_type(parser,&
         'array{e,d1:dom,d2:dom,d3:dom,d4:dom}'//&
         ' is e#grid{d1,d2,d3,d4},_slice{e,grid{d1,d2,d3,d4}}',line)
    call dcl_type(parser,&
         'array{e,d1:dom,d2:dom,d3:dom,d4:dom,d5:dom}'//&
         ' is e#grid{d1,d2,d3,d4,d5},_slice{e,grid{d1,d2,d3,d4,d5}}',line)
    call dcl_type(parser,&
         'array{e,d1:dom,d2:dom,d3:dom,d4:dom,d5:dom,'//&
         'd6:dom}'//&
         ' is e#grid{d1,d2,d3,d4,d5,d6},_slice{e,grid{d1,d2,d3,d4,d5,d6}}',line)
    call dcl_type(parser,&
         'array{e,d1:dom,d2:dom,d3:dom,d4:dom,d5:dom,'//&
         'd6:dom,d7:dom}'//&
         ' is e#grid{d1,d2,d3,d4,d5,d6,d7},_slice{e,grid{d1,d2,d3,d4,d5,d6,d7}}',line)

    call dcl_uproc(parser,'(//)(a:any#any,arg...)='//&
         'PM__get_elem(a,index(dom(a),arg...))',line)
    call dcl_uproc(parser,'(//)=(&a:any#any,v,arg...)'//&
         ' do PM__set_elem(&a,index(dom(a),arg...),v) endproc',line)
    call dcl_uproc(parser,'(%%)(a:any#any,arg...)='//&
         'PM__get_elem(a,index(shape(a),arg...))',line)
    call dcl_uproc(parser,'(%%)=(&a:any#any,v,arg...:any) '//&
         'do PM__set_elem(&a,index(shape(a),arg...),v) endproc',line)

    call dcl_proc(parser,'PM__get_elem(x:any#any,y:long)->*x',op_array_get_elem,0_pm_i16,line,0)
    call dcl_proc(parser,'PM__set_elem(&x:any#any,y:long,z:any)',op_array_set_elem,0_pm_i16,line,0)

    call dcl_uproc(parser,'arb(x)=PM__get_elem(x,0l)',line)
    call dcl_uproc(parser,'num_elem(x:any#any)=num_elem(dom(x))',line)
    call dcl_uproc(parser,'dim(x:any,y:any)=_array(x,d,num_elem(d)) where d=dom(y)',line)
    call dcl_proc(parser,'_array(x:any,y:any,z:any)->dim x,y',op_array,0_pm_i16,line,0)
    call dcl_proc(parser,'dom(x:any#any)->#x',op_elem,3_pm_i16,line,0)
    call dcl_uproc(parser,'shape(x:any#any)=shape(dom(x))',line)
    call dcl_proc(parser,'_elts(x:any#any)->*x',op_elem,1_pm_i16,line,0)
    call dcl_proc(parser,'_set_elts(x:any#any),y)',op_set_elem,1_pm_i16,line,0)

    ! Keyword arguments
    call dcl_uproc(parser,'PM__getkey(x:any,y:any)=convert(x,y)',line)
    call dcl_uproc(parser,'PM__getkey(x:null,y:any)=y',line)
    call dcl_proc(parser,'PM__makekeys(arg...:any)->any',op_makekeys,0_pm_i16,line,0)
    call dcl_proc(parser,'PM__delkeys(arg...:any)->any',op_delkeys,0_pm_i16,line,0)
    call dcl_proc(parser,'PM__checkkeys(arg...:any)',op_checkkeys,0_pm_i16,line,0)
    call dcl_proc(parser,'PM__getvkey(arg...:any)->any',op_getvkey,0_pm_i16,line,0)

    ! Select statement
    call dcl_uproc(parser,&
         'check_case(x,y,arg...)=e do e:=match(x,y); '//&
         'if not e then e=check_case(x,arg...) endif endproc',line)
    call dcl_uproc(parser,'check_case(x,y)= match(x,y)',line)
    call dcl_uproc(parser,'match(x,y)=x==y',line)
    call dcl_uproc(parser,'match(x:num,y:range{num})=x>=y._lo and x<=y._hi',line)
    
    ! Variables
    call dcl_uproc(parser,'PM__assign(&a:any,b:any) do _assign(&a,b); check "Cannot assign":a?=b endproc',line)
    call dcl_proc(parser,'_assign(&any,any)',op_assign,0_pm_i16,line,0)
    call dcl_proc(parser,'PM__dup(x:any)->=x',op_clone,0_pm_i16,line,0)
    call dcl_uproc(parser,'PM__getref(x)=x',line)

    ! Implementation of for statements
    call dcl_proc(parser,'PM__import_val(x:any)->=x',op_import_val,0_pm_i16,line,0)
    call dcl_proc(parser,'PM__export(x:any)->=x',op_export,0_pm_i16,line,0)
    call dcl_proc(parser,'_export_array(any,any,any)',op_export_array,0_pm_i16,line,0)
    call dcl_proc(parser,'PM__extract(x:any)->=x',op_extract,0_pm_i16,line,0)
    call dcl_proc(parser,'PM__extractelm(x:any)->*x',op_extractelm,0_pm_i16,line,0)
    call dcl_proc(parser,'_makearray(x:any,y:any,n:any)->dim x,y',op_make_array,0_pm_i16,line,0)
    call dcl_uproc(parser,'PM__makearray(x,y)=_makearray(x,d,num_elem(d)) where d=dom(y)',line)
    call dcl_uproc(parser,&
         'PM__exparray(a:any#any,n:any,v:any) do _export_array(a,index(shape(a),n),v) endproc',line)

    ! Parallel aspects of for statements (just hooks here)
    call dcl_uproc(parser,'PM__sync(&w,x,y) do endproc',line)
    call dcl_uproc(parser,'PM__partition(w,grid=1)=1l,w dim 2l',line)
    call dcl_uproc(parser,'single_elem(w)=w',line)
    call dcl_uproc(parser,'PM__pop_prc(w) do print(w) endproc',line)
    call dcl_uproc(parser,'this_prc()=1l',line)
    call dcl_uproc(parser,'prc_grid()=1..2',line)
    call dcl_uproc(parser,'broadcast(&x,y) do endproc',line)
    call dcl_uproc(parser,'PM__start_find()=1',line)
    call dcl_uproc(parser,'PM__test_find(x) do endproc',line)
    call dcl_uproc(parser,'PM__sync_find(x)=x',line)
    call dcl_uproc(parser,'PM__conc()=false,1l',line)
    call dcl_uproc(parser,'PM__pop_conc(x) do endproc',line)

    ! Communicating operators
    call dcl_uproc(parser,'PM__at1::(x) local do result=x endproc',line)
    call dcl_uproc(parser,'PM__at1_sub%(x;arg...)=proc[](@x,arg...)',line)
    call dcl_uproc(parser,'PM__at1_open%(x;arg...)=proc{}(@x,arg...)',line)
    call dcl_uproc(parser,'PM__at2%(x;v1) local do d:=dom(x);r:=for i in this_tile do '//&
         'a:=_gridit(d,v1{i});v:=null(arb(x)) dim a;for j in a seq do '//&
         'v[j]=get_displaced(x,i,j) endfor build @v endfor;'//&
         'result=r endproc',line)
    call dcl_uproc(parser,'get_displaced(x,i,j)=r do sh:=shape(x);di:=displace(sh,i,j);'//&
         'r:=null(arb(x));if di in sh then r=opt(x{di}) endif endproc',line)
    
  end subroutine sysdefs

  subroutine set_op_names
    op_names='??'
    op_names(op_call)='op_call'
    op_names(op_poly_call)='op_poly_call'
    op_names(first_jmp_op)='first_jmp_op'
    op_names(op_jmp)='op_jmp'
    op_names(op_jmp_any_ve)='op_jmp_any_ve'
    op_names(op_jmp_empty_ve)='op_jmp_empty_ve'
    op_names(op_and_jmp_none)='op_and_jmp_none'
    op_names(op_andnot_jmp_none)='op_andnot_jmp_none'
    op_names(op_and_jmp_any)='op_and_jmp_any'
    op_names(op_andnot_jmp_any)='op_andnot_jmp_any'
    
    op_names(op_struct )='op_struct '
    op_names(op_rec )='op_rec '
    op_names(op_array)='op_array'
    op_names(op_any)='op_any'
    op_names(op_elem)='op_elem'
    op_names(op_set_elem)='op_set_elem'
    op_names(op_set_elem_idx)='op_set_elem_idx'
    op_names(op_poly_elem)='op_poly_elem'
    op_names(op_set_poly_elem)='op_set_poly_elem'
    op_names(op_set_poly_elem_idx)='op_set_poly_elem_idx'
    
    op_names(op_import)='op_import'
    op_names(op_export)='op_export'
    op_names(op_extract)='op_extract'
    op_names(op_return)='op_return'
    op_names(op_par_loop_end)='op_par_loop_end'
    op_names(op_vcall )='op_vcall '
    
    op_names(op_check)='op_check'
    op_names(op_dump)='op_dump'
    op_names(op_print)='op_print'
    op_names(op_concat)='op_concat'
    op_names(op_lookup_error )='op_lookup_error '
    op_names(op_makekeys )='op_makekeys '
    op_names(op_delkeys )='op_delkeys '
    op_names(op_checkkeys )='op_checkkeys '
    op_names(op_getvkey )='op_getvkey '
    op_names(op_var_call )='op_var_call '
    op_names(op_make_dr )='op_make_dr '
    op_names(op_get_dr )='op_get_dr '
    op_names(op_set_dr )='op_set_dr '
    op_names(op_make_rf )='op_make_rf '
    op_names(op_get_rf )='op_get_rf '
    op_names(op_set_rf )='op_set_rf '
    op_names(op_index )='op_index '
    op_names(op_check_logical )='op_check_logical '

    op_names(op_assign)='op_assign'
    
    op_names(op_array_get_elem )='op_array_get_elem '
    op_names(op_array_set_elem )='op_array_set_elem '
    op_names(op_array_elems )='op_array_elems '
    op_names(op_make_array )='op_make_array'
    op_names(op_import_val )='op_import_val'
    op_names(op_extractelm )='op_extractelm'
    op_names(op_iota )='op_iota'
    op_names(op_get_key )='op_get_key'
    op_names(op_get_key2 )='op_get_key2'
    op_names(op_export_array )='op_export_array'

    op_names(op_par_loop)='op_par_loop'
    op_names(op_setref)='op_setref'
    op_names(op_clone)='op_clone'
    
    op_names(op_and )='op_and '
    op_names(op_or )='op_or '
    op_names(op_not )='op_not '
    
    op_names(op_add_i)='op_add_i'
    op_names(op_sub_i)='op_sub_i'
    op_names(op_mult_i)='op_mult_i'
    op_names(op_divide_i)='op_divide_i'
    op_names(op_div_i)='op_div_i'
    op_names(op_mod_i)='op_mod_i'
    op_names(op_pow_i)='op_pow_i'
    op_names(op_uminus_i)='op_uminus_i'
    op_names(op_eq_i)='op_eq_i'
    op_names(op_ne_i)='op_ne_i'
    op_names(op_gt_i)='op_gt_i'
    op_names(op_ge_i)='op_ge_i'
    op_names(op_string_i)='op_string_i'
    op_names(op_get_elt_i)='op_get_elt_i'
    op_names(op_set_elt_i)='op_set_elt_i'
    op_names(op_long_i )='op_long_i '
    op_names(op_assign_i )='op_assign_i '
    
    op_names(op_add_ln)='op_add_ln'
    op_names(op_sub_ln)='op_sub_ln'
    op_names(op_mult_ln)='op_mult_ln'
    op_names(op_divide_ln)='op_divide_ln'
    op_names(op_div_ln)='op_div_ln'
    op_names(op_mod_ln)='op_mod_ln'
    op_names(op_pow_ln)='op_pow_ln'
    op_names(op_uminus_ln)='op_uminus_ln'
    op_names(op_eq_ln)='op_eq_ln'
    op_names(op_ne_ln)='op_ne_ln'
    op_names(op_gt_ln)='op_gt_ln'
    op_names(op_ge_ln)='op_ge_ln'
    op_names(op_string_ln)='op_string_ln'
    op_names(op_get_elt_ln)='op_get_elt_ln'
    op_names(op_set_elt_ln)='op_set_elt_ln'
    op_names(op_int_ln )='op_int_ln '

    op_names(op_get_elt)='op_get_elt'
    op_names(op_set_elt)='op_set_elt'

  end subroutine set_op_names

  function proc_get_name(proc) result(name)
    type(pm_ptr),intent(in):: proc
    integer(pm_p):: name
    type(pm_ptr):: p
    p=proc%data%ptr(proc%offset)
    name=p%data%i16(p%offset+2)
  end function proc_get_name

  subroutine proc_line_module(proc,offset,line,modl)
    type(pm_ptr),intent(in):: proc
    integer,intent(in):: offset
    integer(pm_p),intent(out):: line,modl
    integer:: j
    integer(pm_i16):: k
    type(pm_ptr):: p
    p=proc%data%ptr(proc%offset+1)
    j=p%offset+pm_fast_esize(p)
    do
       k=p%data%i16(j)
       if(k==0) then
          j=j-p%data%i16(j-1)*2-4
       else
          if(p%data%i16(j-1)>offset) return
          if(k>0) then
             modl=k
          else 
             line=-k
          endif
          j=j-2
       endif
       if(j<p%offset) exit
    enddo
  contains
    include 'fesize.inc'
  end subroutine  proc_line_module

  function proc_slot_name(prc,offset,slot) result(name)
    type(pm_ptr):: prc
    integer:: offset,slot
    integer(pm_p):: name
    integer:: i,j,start,finish,n
    type(pm_ptr):: p
    integer(pm_i16):: k
    name=0
    p=prc%data%ptr(prc%offset+1)
    j=p%offset+pm_fast_esize(p)
    do
       k=p%data%i16(j)
       if(k/=0) then
          j=j-2
       else
          n=p%data%i16(j-1)
          start=p%data%i16(j-2)
          finish=p%data%i16(j-3)
          j=j-n*2-4
          if(offset>=start.and.offset<=finish) then
             do i=1,n
                if(p%data%i16(j+i*2-1)==slot) then
                   name=p%data%i16(j+i*2)
                   exit
                endif
             enddo
          endif
       endif
       if(j<p%offset) exit
    enddo
  contains
      include 'fesize.inc'
  end function proc_slot_name
  
end module pm_sysdefs
