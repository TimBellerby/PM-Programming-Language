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
  integer,parameter:: proc_is_pass = 1
  integer,parameter:: proc_global_return = 2
  integer,parameter:: proc_is_generator = 3
  integer,parameter:: proc_needs_type = 4

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
  integer(pm_i16),parameter:: op_reduce=10
  integer(pm_i16),parameter:: op_jmp_nopar=11

  integer(pm_i16),parameter:: last_jmp_op=11

  integer(pm_i16),parameter:: index_op = last_jmp_op
  integer(pm_i16),parameter:: op_struct = index_op +1
  integer(pm_i16),parameter:: op_rec = index_op +2
  integer(pm_i16),parameter:: op_array= index_op + 3
  integer(pm_i16),parameter:: op_any= index_op + 5
  integer(pm_i16),parameter:: op_get_dom = index_op + 6
  integer(pm_i16),parameter:: op_elem= index_op + 7
  integer(pm_i16),parameter:: op_elem_ref = index_op + 8
  integer(pm_i16),parameter:: op_poly_elem= index_op + 11
  integer(pm_i16),parameter:: op_set_poly_elem= index_op + 13
  integer(pm_i16),parameter:: op_set_poly_elem_idx= index_op + 15

  integer(pm_i16),parameter:: op_misc = op_set_poly_elem_idx
  integer(pm_i16),parameter:: op_import= op_misc + 1
  integer(pm_i16),parameter:: op_export= op_misc + 2
  integer(pm_i16),parameter:: op_extract = op_misc + 3
  integer(pm_i16),parameter:: op_return= op_misc + 4
  integer(pm_i16),parameter:: op_par_loop_end= op_misc + 5
  integer(pm_i16),parameter:: op_par_find_end= op_misc + 6
  integer(pm_i16),parameter:: op_vcall = op_misc + 7

  integer(pm_i16),parameter:: op_check= op_misc + 8
  integer(pm_i16),parameter:: op_dump= op_misc + 9
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
  integer(pm_i16),parameter:: op_extract_first = op_misc + 35
  integer(pm_i16),parameter:: op_check_logical = op_misc + 41

  integer(pm_i16),parameter:: op_array_get_elem = op_misc + 42
  integer(pm_i16),parameter:: op_array_set_elem = op_misc + 43
  integer(pm_i16),parameter:: op_array_elems = op_misc + 44
  integer(pm_i16),parameter:: op_make_array = op_misc + 45
  integer(pm_i16),parameter:: op_import_val = op_misc + 46
  integer(pm_i16),parameter:: op_import_varg = op_misc + 47
  integer(pm_i16),parameter:: op_extractelm = op_misc + 48
  integer(pm_i16),parameter:: op_iota = op_misc + 49
  integer(pm_i16),parameter:: op_get_key = op_misc + 50
  integer(pm_i16),parameter:: op_get_key2 = op_misc + 51
  integer(pm_i16),parameter:: op_export_array = op_misc + 52
  integer(pm_i16),parameter:: op_miss_arg = op_misc + 53
  integer(pm_i16),parameter:: op_default = op_misc + 54
  integer(pm_i16),parameter:: op_dump_id = op_misc + 55
  integer(pm_i16),parameter:: op_import_scalar = op_misc + 56
  integer(pm_i16),parameter:: op_check_assign = op_misc + 57
  integer(pm_i16),parameter:: op_same_type = op_misc + 58
  integer(pm_i16),parameter:: op_has_same_type = op_misc + 59

  integer(pm_i16),parameter:: op_this_prc = op_misc + 60
  integer(pm_i16),parameter:: op_this_nprc = op_misc + 61
  integer(pm_i16),parameter:: op_shared_prc = op_misc + 62
  integer(pm_i16),parameter:: op_shared_nprc = op_misc + 63
  integer(pm_i16),parameter:: op_is_shared = op_misc + 64
  integer(pm_i16),parameter:: op_is_par = op_misc + 65
  integer(pm_i16),parameter:: op_push_prc_grid = op_misc + 66
  integer(pm_i16),parameter:: op_push_prc_split = op_misc + 67
  integer(pm_i16),parameter:: op_pop_prc_conc = op_misc + 68
  integer(pm_i16),parameter:: op_push_prc_conc = op_misc + 69
  integer(pm_i16),parameter:: op_pop_prc    = op_misc + 70
  integer(pm_i16),parameter:: op_broadcast = op_misc +  71
  integer(pm_i16),parameter:: op_get_remote = op_misc + 72
  integer(pm_i16),parameter:: op_stencil = op_misc + 73
  integer(pm_i16),parameter:: op_prc_test_push = op_misc + 74
  integer(pm_i16),parameter:: op_sys_prc = op_misc + 75
  integer(pm_i16),parameter:: op_sys_nprc = op_misc + 76
  integer(pm_i16),parameter:: op_reduce_ve = op_misc + 77
  integer(pm_i16),parameter:: op_start_loop_sync = op_misc + 78
  integer(pm_i16),parameter:: op_broadcast_val = op_misc + 79
  integer(pm_i16),parameter:: op_gather = op_misc + 80
  
  integer(pm_i16),parameter:: first_assign_op= op_gather
  integer(pm_i16),parameter:: op_par_loop= first_assign_op +1
  integer(pm_i16),parameter:: op_par_find = first_assign_op + 2
  integer(pm_i16),parameter:: op_setref= first_assign_op + 3
  integer(pm_i16),parameter:: op_clone= first_assign_op + 4
  integer(pm_i16),parameter:: op_clone_ve = first_assign_op + 5
  integer(pm_i16),parameter:: op_nullify_ve = first_assign_op + 6
  integer(pm_i16),parameter:: op_assign = first_assign_op + 7
  integer(pm_i16),parameter:: last_assign_op = first_assign_op+7

  integer(pm_i16),parameter:: op_eq = last_assign_op +1
  integer(pm_i16),parameter:: op_ne = last_assign_op +2
  
  integer(pm_i16),parameter:: op_string_l = last_assign_op +3
  integer(pm_i16),parameter:: op_and = last_assign_op + 4
  integer(pm_i16),parameter:: op_or = last_assign_op + 5
  integer(pm_i16),parameter:: op_not = last_assign_op + 6
  integer(pm_i16),parameter:: op_assign_l = last_assign_op +7

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
  integer(pm_i16),parameter:: op_max_i = op_start_i+16
  integer(pm_i16),parameter:: op_min_i = op_start_i+17
  integer(pm_i16),parameter:: op_assign_i = op_start_i + 18
  integer(pm_i16),parameter:: op_long_i = op_start_i+19
  integer(pm_i16),parameter:: op_real_i = op_start_i+20
  integer(pm_i16),parameter:: op_double_i = op_start_i+21
  integer(pm_i16),parameter:: op_stop_i = op_start_i+21

  integer(pm_i16),parameter:: op_start_ln=op_stop_i
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
  integer(pm_i16),parameter:: op_max_ln = op_start_ln+16
  integer(pm_i16),parameter:: op_min_ln = op_start_ln+17
  integer(pm_i16),parameter:: op_assign_ln = op_start_ln + 18
  integer(pm_i16),parameter:: op_int_ln = op_start_ln+19
  integer(pm_i16),parameter:: op_real_ln = op_start_ln+20
  integer(pm_i16),parameter:: op_double_ln = op_start_ln+21
  integer(pm_i16),parameter:: op_stop_ln = op_start_ln+21

  integer(pm_i16),parameter:: op_start_r =op_stop_ln
  integer(pm_i16),parameter:: op_add_r=op_start_r+1
  integer(pm_i16),parameter:: op_sub_r=op_start_r+2
  integer(pm_i16),parameter:: op_mult_r=op_start_r+3
  integer(pm_i16),parameter:: op_divide_r=op_start_r+4
  integer(pm_i16),parameter:: op_div_r=op_start_r+5
  integer(pm_i16),parameter:: op_mod_r=op_start_r+6
  integer(pm_i16),parameter:: op_pow_r=op_start_r+7
  integer(pm_i16),parameter:: op_uminus_r=op_start_r+8
  integer(pm_i16),parameter:: op_eq_r=op_start_r+9
  integer(pm_i16),parameter:: op_ne_r=op_start_r+10
  integer(pm_i16),parameter:: op_gt_r=op_start_r+11
  integer(pm_i16),parameter:: op_ge_r=op_start_r+12
  integer(pm_i16),parameter:: op_string_r=op_start_r+13
  integer(pm_i16),parameter:: op_get_elt_r=op_start_r+14
  integer(pm_i16),parameter:: op_set_elt_r=op_start_r+15
  integer(pm_i16),parameter:: op_max_r = op_start_r+16
  integer(pm_i16),parameter:: op_min_r = op_start_r+17
  integer(pm_i16),parameter:: op_assign_r = op_start_r+18
  integer(pm_i16),parameter:: op_int_r = op_start_r+19
  integer(pm_i16),parameter:: op_long_r = op_start_r+20
  integer(pm_i16),parameter:: op_double_r = op_start_r+21
  integer(pm_i16),parameter:: op_stop_r = op_start_r+21

  integer(pm_i16),parameter:: op_start_d =op_stop_r
  integer(pm_i16),parameter:: op_add_d=op_start_d+1
  integer(pm_i16),parameter:: op_sub_d=op_start_d+2
  integer(pm_i16),parameter:: op_mult_d=op_start_d+3
  integer(pm_i16),parameter:: op_divide_d=op_start_d+4
  integer(pm_i16),parameter:: op_div_d=op_start_d+5
  integer(pm_i16),parameter:: op_mod_d=op_start_d+6
  integer(pm_i16),parameter:: op_pow_d=op_start_d+7
  integer(pm_i16),parameter:: op_uminus_d=op_start_d+8
  integer(pm_i16),parameter:: op_eq_d=op_start_d+9
  integer(pm_i16),parameter:: op_ne_d=op_start_d+10
  integer(pm_i16),parameter:: op_gt_d=op_start_d+11
  integer(pm_i16),parameter:: op_ge_d=op_start_d+12
  integer(pm_i16),parameter:: op_string_d=op_start_d+13
  integer(pm_i16),parameter:: op_get_elt_d=op_start_d+14
  integer(pm_i16),parameter:: op_set_elt_d=op_start_d+15
  integer(pm_i16),parameter:: op_max_d = op_start_d+16
  integer(pm_i16),parameter:: op_min_d = op_start_d+17
  integer(pm_i16),parameter:: op_assign_d = op_start_d+18
  integer(pm_i16),parameter:: op_int_d = op_start_d+19
  integer(pm_i16),parameter:: op_long_d = op_start_d+20
  integer(pm_i16),parameter:: op_real_d = op_start_d+21
  integer(pm_i16),parameter:: op_stop_d = op_start_d+21
  
  integer,parameter:: num_op=op_stop_d

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
  integer,parameter:: ftn_makekeys=18
  integer,parameter:: ftn_delkeys=19
  integer,parameter:: ftn_checkkeys=20
  integer,parameter:: ftn_getvkey=21
  integer,parameter:: ftn_assign=22
  integer,parameter:: ftn_dup=23
  integer,parameter:: ftn_getref=24
  integer,parameter:: ftn_same_type=25
  integer,parameter:: ftn_has_same_type=26
  integer,parameter:: ftn_eq=27
  integer,parameter:: ftn_ne=28
  integer,parameter:: ftn_array_dom=29
  integer,parameter:: ftn_export_array=30
  integer,parameter:: ftn_iota=31
  
  integer,parameter:: hook = last_parser_hook
  integer,parameter:: sym_pm_system = hook+1
  integer,parameter:: sym_get_element = hook+2
  integer,parameter:: sym_set_element = hook+3
  integer,parameter:: sym_num_elements= hook+4
  integer,parameter:: sym_import_val = hook+5
  integer,parameter:: sym_import_varg = hook+6
  integer,parameter:: sym_export = hook+7 
  integer,parameter:: sym_partition = hook+8
  integer,parameter:: sym_check_conform = hook+9
  integer,parameter:: sym_dup = hook + 10
  integer,parameter:: sym_assemble = hook + 11
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
  integer,parameter:: sym_extract_first = hook + 47
  integer,parameter:: sym_generate = hook + 48
  integer,parameter:: sym_extract = hook + 49
  integer,parameter:: sym_broadcast = hook + 50
  integer,parameter:: sym_single = hook + 51
  integer,parameter:: sym_pop_prc = hook + 52
  integer,parameter:: sym_set_param = hook + 53
  integer,parameter:: sym_get_param = hook + 54
  integer,parameter:: sym_extract_elems = hook + 55
  integer,parameter:: sym_make_array = hook + 56
  integer,parameter:: sym_concurrent = hook + 57
  integer,parameter:: sym_pop_conc = hook + 58
  integer,parameter:: sym_export_array = hook + 59
  integer,parameter:: sym_opt_num = hook + 60
  integer,parameter:: sym_make_mask = hook + 61
  integer,parameter:: sym_prc_for = hook + 62
  integer,parameter:: sym_import_scalar = hook + 63
  integer,parameter:: sym_set_elem = hook + 64
  integer,parameter:: sym_assign_var = hook + 65
  integer,parameter:: sym_vector = hook + 66
  integer,parameter:: sym_matrix = hook + 67
  integer,parameter:: sym_pdup = hook + 68
  integer,parameter:: sym_do_dim = hook + 69
  integer,parameter:: sym_iter_dom = hook + 70
  integer,parameter:: num_syshook = 70

  character(len=20),dimension(0:num_op):: op_names
  character(len=14),dimension(num_syshook),parameter:: syshook = (/ &
       'PM__system    ','get_elem      ','set_elem      ','size          ',&
       'PM__import_val','PM__importvarg','PM__export    ','PM__partition ',&
       'check_conform ','PM__dup       ','assemble      ','prc_grid      ',&
       'this_prc      ','grid          ','indices       ','PM__ref       ',&
       'PM__getref    ','PM__setref    ','PM__dotref    ','PM__subref    ',&
       'PM__openref   ','index         ','convert       ',&
       'd1            ','d2            ','d3            ','d4            ',&
       'd5            ','d6            ','d7            ',&
       'PM__getkey    ','PM__makekeys  ','PM__delkeys   ','PM__checkkeys ',&
       'PM__getvkey   ','PM__assign    ','first         ','next          ',&
       'check_case    ','dim1          ','dim2          ','dim3          ',&
       'dim4          ','dim5          ','dim6          ','dim7          ',&
       'PM__extract1st','PM__generate  ','PM__extract   ','broadcast     ',&
       'single_elem   ','PM__pop_prc   ','PM__set_param ','PM__get_param ',&
       'PM__extractelm','PM__makearray ','PM__conc      ','PM__pop_conc  ',&
       'PM__exparray  ','PM__opt_num   ','PM__make_mask ','prc_for       ',&
       'PM__impscalar ','PM__set_elem  ','PM__assign_var','vector        ',&
       'matrix        ','PM__pdup      ','PM__do_dim    ','iter_dom      '&
       /)

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
    call dcl_proc(parser,'print(string)',op_print,0_pm_i16,line,0,&
         'IF PRC_FRAME(PRC_DEPTH)%SHARED_PRC==0 THEN WRITE(*,*) $s1',ftn_simple)
    call dcl_uproc(parser,'print(x) do print(string(x)) endproc',line)
    call dcl_proc(parser,'//(string,string)->string',op_concat,0_pm_i16,line,0,&
         '$1=PM_CONCAT_STRING(PM_CONTEXT,$2,$3)',ftn_simple)
    call dcl_uproc(parser,'//(x:string,y)=proc //(x,string(y))',line)
    call dcl_uproc(parser,'//(x,y)=proc //(string(x),string(y))',line)
    call dcl_uproc(parser,'string(x:string)=x',line)
    call dcl_uproc(parser,'string(x:null)="null"',line)

    ! Int type
    call dcl_proc(parser,'PM__assign_var(&int,int)',op_assign_i,0_pm_i16,line,0,&
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
         '$1=VECTOR_MAKE_STRING(PM_CONTEXT,PM_NULL_OBJ,$p1,FMT_LN_SIZE,FMT_LN)',&
         ftn_simple)
    call dcl_proc(parser,'int(long)->int',op_int_ln,0_pm_i16,line,0,&
         '$1=INT($2)',ftn_simple)
    call dcl_proc(parser,'real(long)->real',op_real_ln,0_pm_i16,line,0,&
         '$1=REAL($2)',ftn_simple)
    call dcl_proc(parser,'double(long)->double',op_double_ln,0_pm_i16,line,0,&
         '$1=DOUBLE($2)',ftn_simple)
    call dcl_uproc(parser,'long(x:long)=x',line)

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
         '$1=VECTOR_MAKE_STRING(PM_CONTEXT,PM_NULL_OBJ,$p2,FMT_R_LENGTH,FMT_R)',&
         ftn_simple)
    call dcl_proc(parser,'int(real)->int',op_int_r,0_pm_i16,line,0,&
         '$1=INT($2)',ftn_simple)
    call dcl_proc(parser,'long(real)->real',op_long_r,0_pm_i16,line,0,&
         '$1=REAL($2)',ftn_simple)
    call dcl_proc(parser,'double(real)->double',op_double_r,0_pm_i16,line,0,&
         '$1=DOUBLE($2)',ftn_simple)
    call dcl_uproc(parser,'real(x:real)=x',line)

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
         '$1=VECTOR_MAKE_STRING(PM_CONTEXT,PM_NULL_OBJ,$p1,FMT_D_LENGTH,FMT_D)',&
         ftn_simple)
    call dcl_proc(parser,'int(double)->int',op_int_d,0_pm_i16,line,0,&
         '$1=INT($2)',ftn_simple)
    call dcl_proc(parser,'long(double)->double',op_long_d,0_pm_i16,line,0,&
         '$1=LONG($2)',ftn_simple)
    call dcl_proc(parser,'real(double)->double',op_real_d,0_pm_i16,line,0,&
         '$1=REAL($2)',ftn_simple)
    call dcl_uproc(parser,'double(x:double)=x',line)

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
    
    ! Optional numeric/bool types
    call dcl_type(parser,'optional{x} is '//&
         'struct _opt{_val:x,_there:bool}',line)
    call dcl_uproc(parser,&
         'PM__opt_num(x,y)=struct _opt{_val=x,_there=true}',line)
    call dcl_uproc(parser,&
         'PM__opt_num(x:null,y)=struct _opt{_val=y,_there=false}',line)
    call dcl_uproc(parser,':/(x:struct _opt{_val,_there},y:num) do z:=y;'//&
         'if x._there then z=x._val endif; result=z endproc ',line)
    call dcl_uproc(parser,&
         ':/(x:struct _opt{_val,_there}#any,y:num)=z '//&
         'do for i in x conc let z=@w where w=i:/y endfor endproc',line)
    call dcl_uproc(parser,&
         'opt(val,there:bool)=struct _opt{_val=val,_there=there}',&
         line)
    call dcl_uproc(parser,&
         '^(x:struct _opt{_val,_there})=x._val'//&
         ' check "^ applied to null optional value":x._there',line)
    call dcl_uproc(parser,'opt(x)=struct _opt{_val=x,_there=true}',line)
    call dcl_uproc(parser,'null(x)=struct _opt{_val=x,_there=false}',line)
    
    ! Tuple types
    call dcl_type(parser,&
         'tuple{t1} is rec _tuple{d1:t1}',line)
    call dcl_type(parser,&
         'tuple{t1,t2} is rec _tuple{d1:t1,d2:t2}',line)
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
         'tuple{t1,t2,t3,t4,t5,t6} is'//&
         ' rec _tuple{d1:t1,d2:t2,d3:t3,d4:t4,d5:t5,d6:t6}',line)
    call dcl_type(parser,&
         'tuple{t1,t2,t3,t4,t5,t6,t7} is'//&
         ' rec _tuple{d1:t1,d2:t2,d3:t3,d4:t4,d5:t5,d6:t6,d7:t7}',line)

    call dcl_type(parser,'tuple1d is tuple{}',line)
    call dcl_type(parser,'tuple2d is tuple{,}',line)
    call dcl_type(parser,'tuple3d is tuple{,,}',line)
    call dcl_type(parser,'tuple4d is tuple{,,,}',line)
    call dcl_type(parser,'tuple5d is tuple{,,,,}',line)
    call dcl_type(parser,'tuple6d is tuple{,,,,,}',line)
    call dcl_type(parser,'tuple7d is tuple{,,,,,,}',line)
    call dcl_type(parser,'tuple is '//&
         'tuple1d,tuple2d,tuple3d,tuple4d,tuple5d,tuple6d,tuple7d',line)
    
    call dcl_uproc(parser,'tuple(x)=rec _tuple{d1=x}',line)
    call dcl_uproc(parser,'tuple(x,y)='//&
         'rec _tuple{d1=x,d2=y}',line)
    call dcl_uproc(parser,'tuple(x,y,z)='//&
         'rec _tuple{d1=x,d2=y,d3=z}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t)='//&
         'rec _tuple{d1=x,d2=y,d3=z,d4=t}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u)='//&
         'rec _tuple{d1=x,d2=y,d3=z,d4=t,d5=u}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u,v)='//&
         'rec _tuple{d1=x,d2=y,d3=z,d4=t,d5=u,d6=v}',line)
    call dcl_uproc(parser,'tuple(x,y,z,t,u,v,w)='//&
         'rec _tuple{d1=x,d2=y,d3=z,d4=t,d5=u,d6=v,d7=w}',line)
   
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
         'size(x:range{any_int})=max(long(x._hi-x._lo),-1l)+1l',line)
    call dcl_uproc(parser,&
         'index(y:range{any_int},x:any_int)=long(x-y._lo)',line)
    call dcl_uproc(parser,&
         'loc_in_shape(y:range{any_int},x:any_int)=long(x-y._lo)',line)
    call dcl_uproc(parser,&
         'first(x:range{any_int})=x._lo,null,x._lo<=x._hi',line)
    call dcl_uproc(parser,&
         'next(x:range{any_int},y,z)=zz,null,zz<=x._hi'//&
         ' where zz=z+convert(1,x._lo)',line)
    call dcl_uproc(parser,'get_elem(x:range{any_int},y:any_int)='//&
         'x._lo+convert(y,x._lo) ',line)
    call dcl_uproc(parser,'get_elem(x:range{any_int},y:range{any_int})= '//&
         ' convert(y._lo,x._lo)+x._lo..convert(y._hi,x._lo)+x._lo',line)
    call dcl_uproc(parser,'get_elem(x:range{any_int},y:seq{any_int})= '//&
         ' convert(first(y),x._lo)+x._lo..convert(last(y),x._lo)+x._lo by y._st',line)
    call dcl_uproc(parser,'get_elem(x:range{any_int},y:range_below{any_int})= '//&
         ' x._lo..convert(y._t,x._lo)+x._lo',line)
    call dcl_uproc(parser,'get_elem(x:range{any_int},y:range_above{any_int})= '//&
         ' convert(y._lo,x._lo)+x._lo..x._hi',line)
    call dcl_uproc(parser,'get_elem(x:range{any_int},y:strided_range_below{any_int})= '//&
         ' x._lo..convert(y._t,x._lo)+x._lo by y._s',line)
    call dcl_uproc(parser,'get_elem(x:range{any_int},y:strided_range_above{any_int})= '//&
         ' convert(y._lo,x._lo)+x._lo..x._hi by y._s',line)
    call dcl_uproc(parser,'get_elem(x:range{any_int},y:stride{any_int})= '//&
         ' x by y._s ',line)
    call dcl_uproc(parser,'get_elem(x:range{any_int},y:null)=x ',line)
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
         ' _f=lo,_n=max(0l,long(m)+1)}'//&
         'endproc',line)
    call dcl_uproc(parser,'by(x:seq{},y:range_base)='//&
         ' rec _seq{_lo=x._lo,_hi=x._hi,_f=x._f,_st=x._st*y,_n=x._n/y}',line)
    call dcl_uproc(parser,'is_cyclic(x:seq{})=false',line)
    call dcl_uproc(parser,'low(x:seq{})=x._lo',line)
    call dcl_uproc(parser,'high(x:seq{})=x._hi',line)
    call dcl_uproc(parser,'step(x:seq{})=x._st',line)
    call dcl_uproc(parser,'size(x:seq{})=x._n',line)
    call dcl_uproc(parser,'first(x:seq{})=x._f',line)
    call dcl_uproc(parser,'last(x:seq{})=x._f+x._n*x._st',line)
    call dcl_uproc(parser,'initial(x:seq{})=x.st>0=>x._lo||x._hi',line)
    call dcl_uproc(parser,'final(x:seq{})=x.st<0=>x._lo||x._hi',line)
    call dcl_uproc(parser,'dom(x:seq{})=grid(0l..x._n-1l)',line)
    call dcl_uproc(parser,'convert(x:seq{},y:range_base)='//&
         'rec _seq{_lo=convert(x._lo,y),_hi=convert(x._hi,y),'//&
         '_f=convert(x.f,y),_n=x._n}',line)
    call dcl_uproc(parser,'long(x:seq{})='//&
         'rec _seq{_lo=long(x._lo),_hi=long(x._hi),_f=long(x._f),_st=long(x._st),_n=x._n}',line)
    call dcl_uproc(parser,&
         'in(x:any_int,y:seq{any_int})='//&
         'y._lo<=x and x<=y._hi and (x-y._f) mod y._st==0',line)
    call dcl_uproc(parser,&
         'includes(x:seq{any_int},y:seq{any_int})='//&
         'y._lo in x and y._hi in x and y._f+y._st in x',line)
    call dcl_uproc(parser,&
         'index(y:seq{long},x:any_int)=(long(x)-y._f)/y._st',line)
    call dcl_uproc(parser,&
         'loc_in_shape(y:seq{long},x:any_int)=(long(x)-y._f)/y._st',line)
    call dcl_uproc(parser,'first(x:seq{})=x._f,0l,x._n>0l',line)
    call dcl_uproc(parser,&
         'next(x:seq{},y,z)=z+x._st,yy,yy<x._n where yy=y+1l',line)
    call dcl_uproc(parser,'get_elem(x:seq{},y:any_int)= '//&
         'x._f+x._st*convert(y,x._lo)',line)
    call dcl_uproc(parser,'get_elem(x:seq{},y:range{any_int}) do'//&
         ' first:=get_elem(x,y._lo);n:=size(y);'//&
         ' last:=first+convert(n,x._lo)*x._st;'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         '_f=first,_st=x._st,_n=n} '//&
         'endproc',line)
    call dcl_uproc(parser,'get_elem(x:seq{},y:seq{any_int}) do'//&
         ' first:=get_elem(x,y._lo);st:=x._st*convert(y._st,x._st);'//&
         ' n:=size(y);last:=first+convert(n,x._lo)*st;'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         ' _f=first,_st=st,_n=n} '//&
         'endproc',line)
    call dcl_uproc(parser,'get_elem(x:seq{},y:range_below{any_int}) do'//&
         ' first:=x._f;last:=get_elem(x,y._t);n:=long(y._t)+1l'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         ' _f=first,_st=x._st,_n=n} '//&
         'endproc',line)
    call dcl_uproc(parser,'get_elem(x:seq{},y:range_above{any_int}) do'//&
         ' first:=get_elem(x,y._t);last:=x._f+x._n*x._st;n:=x._n-long(y._t)'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         ' _f=first,_st=x._st,_n=n} '//&
         'endproc',line)
    call dcl_uproc(parser,'get_elem(x:seq{},y:strided_range_below{any_int}) do'//&
         ' first:=x._f;last:=get_elem(x,y._t);n:=long(y._t)+1l'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         ' _f=first,_st=x._st*y._s,_n=n/y._s} '//&
         'endproc',line)
    call dcl_uproc(parser,'get_elem(x:seq{},y:strided_range_above{any_int}) do'//&
         ' first:=get_elem(x,y._t);last:=x._f+x._n*x._st;n:=x._n-long(y._t)'//&
         ' result=rec _seq{_lo=min(first,last),_hi=max(first,last),'//&
         ' _f=first,_st=x._st*y._s,_n=n/y._s} '//&
         'endproc',line)
    call dcl_uproc(parser,'get_elem(x:seq{},y:stride{any_int}) do'//&
         ' result=rec _seq{_lo=x._lo,_hi=x._hi,'//&
         ' _f=x._f,_st=x._st*y._t,_n=x._n/y._t} '//&
         'endproc',line)
    call dcl_uproc(parser,'get_elem(x:seq,y:null)=x',line)
    call dcl_uproc(parser,'expand(x:seq{},y:range{})='//&
         'rec _range{_lo=x._lo+y._lo,_hi=x._hi+y._hi}',line)
    call dcl_uproc(parser,'expand(x:seq{},y:seq{})='//&
         'rec _range{_lo=x._lo+y._lo,_hi=x._hi+y._hi}',line)
    call dcl_uproc(parser,'contract(x:seq{},y:range{})='//&
         'rec _range{_lo=x._lo-y._lo,_hi=x._hi-y._hi}',line)
    call dcl_uproc(parser,'contract(x:seq{},y:seq{})='//&
         'rec _range{_lo=x._lo-y._lo,_hi=x._hi-y._hi}',line)

    ! Cyclic types
    call dcl_type(parser,'cycle is _cycle{}',line)
    call dcl_type(parser,'cycle{t} is _cycle{t}',line)
    call dcl_type(parser,'_cycle{t} includes rec _cycle{_x:t}',line)
    call dcl_type(parser,'_cycle{t:range_base} also includes rec _cycle{_x:bd_seq{t}}',line) 
    call dcl_uproc(parser,'cycle(x:range{})=rec _cycle{_x=x}',line)
    call dcl_uproc(parser,'cycle(x:seq{})=rec _cycle{_x=x}',line)
    call dcl_uproc(parser,'is_cyclic(x:cycle{})=true',line)
    call dcl_uproc(parser,'size(x:cycle)=size(x._x)',line)
    call dcl_uproc(parser,'dom(x:cycle)=dom(x._x)',line)
    call dcl_uproc(parser,'convert(x:cycle,y:range_base)=rec _cycle{_x=convert(x._x,y)}',line)
    call dcl_uproc(parser,'long(x:cycle)=rec _cycle{_x=long(x._x)}',line)
    call dcl_uproc(parser,'in(x:range_base,y:cycle)=true',line)
    call dcl_uproc(parser,'includes(x:cycle,y:cycle)=x._x includes y._x',line)
    call dcl_uproc(parser,'includes(x:cycle,y)=x._x includes y',line)
    call dcl_uproc(parser,'low(x:cycle)=low(x._x)',line)
    call dcl_uproc(parser,'high(x:cycle)=high(x._x)',line)
    call dcl_uproc(parser,'first(x:cycle)=first(x._x)',line)
    call dcl_uproc(parser,'last(x:cycle)=last(x._x)',line)
    call dcl_uproc(parser,'initial(x:cycle)=initial(x._x)',line)
    call dcl_uproc(parser,'final(x:cycle)=final(x._x)',line)
    call dcl_uproc(parser,'step(x:cycle)=step(x._x)',line)
    call dcl_uproc(parser,'get_elem(x:cycle,y:index)=get_elem(x._x,y)',line)
    call dcl_uproc(parser,'get_elem(x:cycle,y:subs)=rec _cycle{x=get_elem(x._x,y)}',line)
    call dcl_uproc(parser,'overlap(x:cycle,y:bd_seq)=y',line)
    call dcl_uproc(parser,'overlap(x:bd_seq,y:cycle)=overlap(x,y._x)',line)
    call dcl_uproc(parser,'overlap(x:cycle,y:cycle)=overlap(x._x,y._x)',line)
    call dcl_uproc(parser,'index(x:cycle{long},y:any_int)=index(x._x,y) mod size(x._x)',line)
    call dcl_uproc(parser,'loc_in_shape(x:cycle{long},y:any_int)=index(x._x,y) mod size(x._x)',line)
    call dcl_uproc(parser,'first(x:cycle)=u,v,w where u,v,w=first(x._x)',line)
    call dcl_uproc(parser,'next(x:cycle,y,z)=u,v,w where u,v,w=next(x._x,y,z)',line) 
    
    ! Vector type
    call dcl_type(parser,'vector is rec _vect{_d}',line)
    call dcl_uproc(parser,&
         'vector(rows:any_seq{std_int})=rec _vect{_d=rows,_n=n} '//&
         ' where n=size(lrows) where lrows=long(rows)',line)
    call dcl_uproc(parser,'size(x:vector)=x._d._n',line)
    call dcl_uproc(parser,'dom(x:vector)=rec _vect{_d=0l..size(x._d)}',line)
    call dcl_uproc(parser,'in(x:any_int,y:vector)=x in y._d',line)
    
    call dcl_uproc(parser,'includes(x:vector,y:vector)=x._d includes y._d',line)
    call dcl_uproc(parser,'first(x:vector)=u,v,w where u,v,w=first(x._d)',line)
    call dcl_uproc(parser,'next(x:vector,y,z)=u,v,w where u,v,w=next(x._d,y,z)',line)
    call dcl_uproc(parser,'get_elem(x:vector,y)=get_elem(x._d,y)',line)
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
    call dcl_uproc(parser,'dom(x:matrix)=rec _matrix{_d=dom(x._d)}',line)
    call dcl_uproc(parser,'in(x:matrix,y:matrix)=x._d in y._d',line)
    call dcl_uproc(parser,'includes(x:matrix,y:matrix)=x._d includes y._d',line)
    call dcl_uproc(parser,'first(x:matrix)=u,v,w where u,v,w=first(x._d)',line)
    call dcl_uproc(parser,'next(x:matrix,y,z)=u,v,w where u,v,w=next(x._d,y,z)',line)
    call dcl_uproc(parser,'get_elem(x:matrix,y)=get_elem(x._d,y)',line)
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
    call dcl_type(parser,'grid{t1:grid_base} is rec _grid{_n,d1:t1}',line)
    call dcl_type(parser,'grid{t1:grid_base,t2:grid_base} is'//&
         ' rec _grid{_n,d1:t1,d2:t2}',line)
    call dcl_type(parser,&
         'grid{t1:grid_base,t2:grid_base,t3:grid_base} is'//&
         ' rec _grid{_n,d1:t1,d2:t2,d3:t3}',line)
    call dcl_type(parser,&
         'grid{t1:grid_base,t2:grid_base,t3:grid_base,t4:grid_base} is'//&
         ' rec _grid{_n,d1:t1,d2:t2,d3:t3,d4:t4}',line)
    call dcl_type(parser,&
         'grid{t1:grid_base,t2:grid_base,t3:grid_base,t4:grid_base,'//&
         't5:grid_base}'//&
         'is rec _grid{_n,d1:t1,d2:t2,d3:t3,d4:t4,d5:t5}',line)
    call dcl_type(parser,&
         'grid{t1:grid_base,t2:grid_base,t3:grid_base,t4:grid_base,'//&
         't5:grid_base,t6:grid_base} is'//&
         ' rec _grid{_n,d1:t1,d2:t2,d3:t3,d4:t4,d5:t5,d6:t6}',line)
    call dcl_type(parser,&
        'grid{t1:grid_base,t2:grid_base,t3:grid_base,t4:grid_base,'//&
        't5:grid_base,'//&
        't6:grid_base,t7:grid_base} is'//&
        ' rec _grid{_n,d1:t1,d2:t2,d3:t3,'//&
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
   call dcl_uproc(parser,'grid(d1:_garg)=rec _grid{d1=t1,_n=nn}'//&
        ' where nn=size(d1) '//&
        ' where t1=long(d1) }',line)
   call dcl_uproc(parser,'grid(d1:_garg,d2:_garg)='//&
        'rec _grid{d1=t1,d2=t2,_n=nn}'//&
        ' where s=rec _grid{_s=null,d1=t1,d2=t2,_n=nn}'//&
        ' where nn=size(d1)*size(d2) '//&
        ' where t1=long(d1),t2=long(d2)}',line)
   call dcl_uproc(parser,&
        'grid(d1:_garg,d2:_garg,d3:_garg)='//&
        ' rec _grid{d1=t1,d2=t2,d3=t3,_n=nn}'//&
        ' where nn=size(d1)*size(d2)*size(d3) where'//&
        ' t1=long(d1),t2=long(d2),t3=long(d3)}',line)
   call dcl_uproc(parser,&
        'grid(d1:_garg,d2:_garg,d3:_garg,d4:_garg)='//&
        ' rec _grid{d1=t1,d2=t2,d3=t3,d4=t4,_n=nn}'//&
        ' where nn=size(d1)*size(d2)*size(d3)*size(d4)'//&
        ' where t1=long(d1),t2=long(d2),t3=long(d3),t4=long(d4)}',line)
   call dcl_uproc(parser,&
        'grid(d1:_garg,d2:_garg,d3:_garg,d4:_garg,d5:_garg)='//&
        ' rec _grid{d1=t1,d2=t2,d3=t3,d4=t4,d5=t5,_n=nn}'//&
        ' where nn=size(d1)*size(d2)*size(d3)*size(d4)*size(d5)'//&
        ' where t1=long(d1),t2=long(d2),t3=long(d3),t4=long(d4),t5=long(d5)}',&
        line)
   call dcl_uproc(parser,&
        'grid(d1:_garg,d2:_garg,d3:_garg,d4:_garg,d5:_garg,d6:_garg)='//&
        ' rec _grid{d1=t1,d2=t2,d3=t3,d4=t4,d5=t5,d6=t6,_n=nn}'//&
        ' where nn=size(d1)*size(d2)*size(d3)*size(d4)*size(d5)*size(d6)'//&
        ' where t1=long(d1),t2=long(d2),t3=long(d3),'//&
        ' t4=long(d4),t5=long(d5),t6=long(d6)}',line)
   call dcl_uproc(parser,&
        'grid(d1:_garg,d2:_garg,d3:_garg,d4:_garg,d5:_garg,'//&
        ' d6:_garg,d7:_garg)='//&
        ' rec _grid{d1=t1,d2=t2,d3=t3,d4=t4,d5=t5,d6=t6,d7=t7,_n=nn}'//&
        ' where nn=size(d1)*size(d2)*size(d3)*size(d4)'//&
        '  *size(d5)*size(d6)*size(d7)'//&
        ' where t1=long(d1),t2=long(d2),t3=long(d3),t4=long(d4),'//&
        ' t5=long(d5),t6=long(d6),t7=long(d7)}',line)
   
   call dcl_uproc(parser,'size(x:grid)=x._n',line)

   ! Subscript a grid
   call dcl_uproc(parser,'get_elem(x:grid1d,y)='//&
        'get_elem(x.d1,long(y))',line)
   call dcl_uproc(parser,'get_elem(x:grid1d,y:tuple1d)=tuple('//&
        'get_elem(x.d1,y.d1))',line)
   call dcl_uproc(parser,'get_elem(x:grid2d,y:tuple2d)=tuple('//&
        'get_elem(x.d1,y.d1),get_elem(x.d2,y.d2))',line)
   call dcl_uproc(parser,'get_elem(x:grid3d,y:tuple3d)=tuple('//&
        'get_elem(x.d1,y.d1),get_elem(x.d2,y.d2),'//&
        'get_elem(x.d3,y.d3))',line)
   call dcl_uproc(parser,'get_elem(x:grid4d,y:tuple4d)=tuple('//&
        'get_elem(x.d1,y.d1),get_elem(x.d2,y.d2),'//&
        'get_elem(x.d3,y.d3),get_elem(x.d4,y.d4))',line)
   call dcl_uproc(parser,'get_elem(x:grid5d,y:tuple5d)=tuple('//&
        'get_elem(x.d1,y.d1),get_elem(x.d2,y.d2),'//&
        'get_elem(x.d3,y.d3),get_elem(x.d4,y.d4),'//&
        'get_elem(x.d5,y.d5))',line)
   call dcl_uproc(parser,'get_elem(x:grid6d,y:tuple6d)=tuple('//&
        'get_elem(x.d1,y.d1),get_elem(x.d2,y.d2),'//&
        'get_elem(x.d3,y.d3),get_elem(x.d4,y.d4),'//&
        'get_elem(x.d5,y.d5),get_elem(x.d6,y.d6))',line)
   call dcl_uproc(parser,'get_elem(x:grid7d,y:tuple7d)=tuple('//&
        'get_elem(x.d1,y.d1),get_elem(x.d2,y.d2),'//&
        'get_elem(x.d3,y.d3),get_elem(x.d4,y.d4),'//&
        'get_elem(x.d5,y.d5),get_elem(x.d6,y.d6),'//&
        'get_elem(x.d7,y.d7))',line)

   ! Check grid inclusion (subgrid)
   call dcl_uproc(parser,'includes(x:grid1d,y:grid1d)='//&
        'x.d1 includes y.d1',line)
   call dcl_uproc(parser,'includes(x:grid2d,y:grid2d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2',line)
   call dcl_uproc(parser,'includes(x:grid3d,y:grid3d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2 '//&
        'x.d3 includes y.d3',line)
   call dcl_uproc(parser,'includes(x:grid4d,y:grid4d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2 '//&
        'x.d3 includes y.d3 and x.d4. includes y.d4',line)
   call dcl_uproc(parser,'includes(x:grid5d,y:grid5d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2 '//&
        'x.d3 includes y.d3 and x.d4. includes y.d4 and x.d5 includes y.d5',&
        line)
   call dcl_uproc(parser,'includes(x:grid6d,y:grid6d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2 '//&
        'x.d3 includes y.d3 and x.d4. includes y.d4 and x.d5 includes y.d5'//&
        ' and x.d6 includes y.d6',line)
   call dcl_uproc(parser,'includes(x:grid7d,y:grid7d)='//&
        'x.d1 includes y.d1 and x.d2 includes y.d2 '//&
        'x.d3 includes y.d3 and x.d4. includes y.d4 and x.d5 includes y.d5 '//&
        'and x.d6 includes y.d6 '//&
        'x.d7 includes y.d7',line)

   ! Grid iteration

   ! - first element
   call dcl_uproc(parser,'first(d:grid1d)=i,s,e where i,s,e=first(d.d1)',line)
   call dcl_uproc(parser,'first(d:grid2d)='//&
        'tuple(j1,j2),rec _gs{d1=s1,d2=s2},size(d)>0l'//&
        ' where j1,s1,_=first(d.d1),j2,s2,_=first(d.d2)',line)
   call dcl_uproc(parser,'first(d:grid3d)='//&
        'tuple(j1,j2,j3),rec _gs{d1=s1,d2=s2,d3=s3},size(d)>0l'//&
        ' where j1,s1,_=first(d.d1),j2,s2,_=first(d.d2),j3,s3,_=first(d.d3)',&
        line)
   call dcl_uproc(parser,'first(d:grid4d)='//&
        'tuple(j1,j2,j3,j4),rec _gs{d1=s1,d2=s2,d3=s3,d4=s4},size(d)>0l'//&
        ' where j1,s1,_=first(d.d1),j2,s2,_=first(d.d2),'//&
        'j3,s3,_=first(d.d3),j4,s4,_=first(d.d4)',line)
   call dcl_uproc(parser,'first(d:grid5d)=tuple(j1,j2,j3,j4,j5),'//&
        ' rec _gs{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5},size(d)>0l'//&
        ' where j1,s1,_=first(d.d1),j2,s2,_=first(d.d2),j3,s3,'//&
        '_=first(d.d3),j4,s4,_=first(d.d4),'//&
        ' j5,s5,_=first(d.d5)',line)
   call dcl_uproc(parser,'first(d:grid6d)=tuple(j1,j2,j3,j4,j5,j6),'//&
        'rec _gs{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5,d6=s6},'//&
        'size(d)>0l'//&
        ' where j1,s1,_=first(d.d1),j2,s2,_=first(d.d2),'//&
        'j3,s3,_=first(d.d3),j4,s4,_=first(d.d4),'//&
        ' j5,s5,_=first(d.d5),j6,s6,_=first(d.d6)',line)
   call dcl_uproc(parser,'first(d:grid7d)=tuple(j1,j2,j3,j4,j5,j6,j7),'//&
        'rec _gs{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5,d6=s6,d7=s7},'//&
        'size(d)>0l'//&
        ' where j1,s1,_=first(d.d1),j2,s2,'//&
        '_=first(d.d2),j3,s3,_=first(d.d3),j4,s4,_=first(d.d4),'//&
        ' j5,s5,_=first(d.d5),j6,s6,_=first(d.d6),j7,s7,_=first(d.d7)',line)

   ! - subsequent elements
   call dcl_uproc(parser,&
        'next(d:grid1d,g,i)=j,s,e where j,s,e=next(d.d1,g,i)',line)
   call dcl_uproc(parser,'next(d:grid2d,g:rec _gs{d1,d2},i:tuple2d) do '//&
        'j1,s1,e:=next(d.d1,g.d1,i.d1);j2:=i.d2;s2:=g.d2;'//&
        'if not e then '//&
        '  j1,s1,_=first(d.d1);j2,s2,e=next(d.d2,g.d2,i.d2) endif;'//&
        'result=tuple(j1,j2),rec _gs{d1=s1,d2=s2},e endproc',line)
   call dcl_uproc(parser,'next(d:grid3d,g:rec _gs{d1,d2,d3},i:tuple3d) do '//&
        'j1,s1,e:=next(d.d1,g.d1,i.d1);j2:=i.d2;s2:=g.d2;j3:=i.d3;s3:=g.d3;'//&
        'if not e then j1,s1,_=first(d.d1);'//&
        'j2,s2,e=next(d.d2,g.d2,i.d2) endif;'//&
        'if not e then j2,s2,_=first(d.d2);'//&
        'j3,s3,e=next(d.d3,g.d3,i.d3) endif;'//&
        'result=tuple(j1,j2,j3),rec _gs{d1=s1,d2=s2,d3=s3},e endproc',line)
   call dcl_uproc(parser,&
        'next(d:grid4d,g:rec _gs{d1,d2,d3,d4},i:tuple4d) do '//&
        'j1,s1,e:=next(d.d1,g.d1,i.d1);j2:=i.d2;s2:=g.d2;'//&
        'j3:=i.d3;s3:=g.d3;j4:=i.d4;s4:=g.d4;'//&
        'if not e then j1,s1,_=first(d.d1);'//&
        'j2,s2,e=next(d.d2,g.d2,i.d2) endif;'//&
        'if not e then j2,s2,_=first(d.d2);'//&
        'j3,s3,e=next(d.d3,g.d3,i.d3) endif;'//&
        'if not e then j3,s3,_=first(d.d3);'//&
        'j4,s4,e=next(d.d4,g.d4,i.d4) endif;'//&
        'result=tuple(j1,j2,j3,j4),rec _gs{d1=s1,d2=s2,d3=s3,d4=s4},e'//&
        ' endproc',line)
   call dcl_uproc(parser,&
        'next(d:grid5d,g:rec _gs{d1,d2,d3,d4,d5},i:tuple5d) do '//&
        'j1,s1,e:=next(d.d1,g.d1,i.d1);'//&
        'j2:=i.d2;s2:=g.d2;j3:=i.d3;s3:=g.d3;j4:=i.d4;'//&
        's4:=g.d4;j5:=i.d5;s5:=g.d5;'//&
        'if not e then j1,s1,_=first(d.d1);'//&
        'j2,s2,e=next(d.d2,g.d2,i.d2) endif;'//&
        'if not e then j2,s2,_=first(d.d2);'//&
        'j3,s3,e=next(d.d3,g.d3,i.d3) endif;'//&
        'if not e then j3,s3,_=first(d.d3);'//&
        'j4,s4,e=next(d.d4,g.d4,i.d4) endif;'//&
        'if not e then j4,s4,_=first(d.d4);'//&
        'j5,s5,e=next(d.d5,g.d5,i.d5) endif;'//&
        'result=tuple(j1,j2,j3,j4,j5),'//&
        'rec _gs{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5},e'//&
        ' endproc',line)
   call dcl_uproc(parser,&
        'next(d:grid6d,g:rec _gs{d1,d2,d3,d4,d5,d6},i:tuple6d) do '//&
        'j1,s1,e:=next(d.d1,g.d1,i.d1);'//&
        'j2:=i.d2;s2:=g.d2;j3:=i.d3;s3:=g.d3;'//&
        'j4:=i.d4;s4:=g.d4;j5:=i.d5;s5:=g.d5;j6:=i.d6;s6:=g.d6;'//&
        'if not e then j1,s1,_=first(d.d1);'//&
        'j2,s2,e=next(d.d2,g.d2,i.d2) endif;'//&
        'if not e then j2,s2,_=first(d.d2);'//&
        'j3,s3,e=next(d.d3,g.d3,i.d3) endif;'//&
        'if not e then j3,s3,_=first(d.d3);'//&
        'j4,s4,e=next(d.d4,g.d4,i.d4) endif;'//&
        'if not e then j4,s4,_=first(d.d4);'//&
        'j5,s5,e=next(d.d5,g.d5,i.d5) endif;'//&
        'if not e then j5,s5,_=first(d.d5);'//&
        'j6,s6,e=next(d.d5,g.d6,i.d6) endif;'//&
        'result=tuple(j1,j2,j3,j4,j5,j6),'//&
        'rec _gs{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5,d6=s6},e endproc',line)
   call dcl_uproc(parser,&
        'next(d:grid7d,g:rec _gs{d1,d2,d3,d4,d5,d6,d7},i:tuple7d) do '//&
        'j1,s1,e:=next(d.d1,g.d1,i.d1);'//&
        'j2:=i.d2;s2:=g.d2;j3:=i.d3;s3:=g.d3;j4:=i.d4;s4:=g.d4;'//&
        'j5:=i.d5;s5:=g.d5;j6:=i.d6;s6:=g.d6;j7:=i.d7;s7:=g.d7;'//&
        'if not e then j1,s1,_=first(d.d1);'//&
        'j2,s2,e=next(d.d2,g.d2,i.d2) endif;'//&
        'if not e then j2,s2,_=first(d.d2);'//&
        'j3,s3,e=next(d.d3,g.d3,i.d3) endif;'//&
        'if not e then j3,s3,_=first(d.d3);'//&
        'j4,s4,e=next(d.d4,g.d4,i.d4) endif;'//&
        'if not e then j4,s4,_=first(d.d4);'//&
        'j5,s5,e=next(d.d5,g.d5,i.d5) endif;'//&
        'if not e then j5,s5,_=first(d.d5);'//&
        'j6,s6,e=next(d.d5,g.d6,i.d6) endif;'//&
        'if not e then j6,s6,_=first(d.d6);'//&
        'j7,s7,e=next(d.d7,g.d7,i.d7) endif;'//&
        'result=tuple(j1,j2,j3,j4,j5,j6,j7),'//&
        'rec _gs{d1=s1,d2=s2,d3=s3,d4=s4,d5=s5,d6=s6,d7=s7},e endproc',line)

   ! Domain of a grid
   call dcl_uproc(parser,&
        'dom(x:grid1d)=rec _grid{_n=x._n,d1=0l..size(x.d1)-1l}',line)
   call dcl_uproc(parser,'dom(x:grid2d)=rec _grid{'//&
        '_n=x._n,d1=0l..size(x.d1)-1l,d2=0l..size(x.d2)-1l}',line)
   call dcl_uproc(parser,'dom(x:grid3d)=rec _grid{'//&
        '_n=x._n,d1=0l..size(x.d1)-1l,d2=0l..size(x.d2)-1l,'//&
        'd3=0l..size(x.d3)-1l}',line)
   call dcl_uproc(parser,'dom(x:grid4d)=rec _grid{'//&
        '_n=x._n,d1=0l..size(x.d1)-1l,d2=0l..size(x.d2)-1l,'//&
        'd3=0l..size(x.d3)-1l,d4=0l..size(x.d4)-1l}',line)
   call dcl_uproc(parser,'dom(x:grid5d)=rec _grid{'//&
        '_n=x._n,d1=0l..size(x.d1)-1l,d2=0l..size(x.d2)-1l,'//&
        'd3=0l..size(x.d3)-1l,'//&
        'd4=0l..size(x.d4)-1l,d5=0l..size(x.d5)-1l}',line)
   call dcl_uproc(parser,'dom(x:grid6d)=rec _grid{'//&
        '_n=x._n,d1=0l..size(x.d1)-1l,d2=0l..size(x.d2)-1l,'//&
        'd3=0l..size(x.d3)-1l,'//&
        'd4=0l..size(x.d4)-1l,d5=0l..size(x.d5)-1l,d6=0l..size(x.d6)-1l}',line)
   call dcl_uproc(parser,'dom(x:grid7d)=rec _grid{'//&
        '_n=x._n,d1=0l..size(x.d1)-1l,d2=0l..size(x.d2)-1l,'//&
        'd3=0l..size(x.d3)-1l,'//&
        'd4=0l..size(x.d4)-1l,d5=0l..size(x.d5)-1l,'//&
        'd6=0l..size(x.d6)-1l,d7=0l..size(x.d7)-1l}',line)

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
   
   ! Index type
   call dcl_type(parser,'index includes any_int,'//&
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
   call dcl_uproc(parser,'get_slice(d:range{long},x:strided_range_below{any_int})='//&
        ' d._lo .. long(x._t) by x._s',line)
   call dcl_uproc(parser,'get_slice(d:range{long},x:strided_range_above{any_int})='//&
        ' long(x._t) .. d._hi by x._s',line)
   call dcl_uproc(parser,&
        'get_slice(d:seq{long},x:range_base)=long(x)',line)
   call dcl_uproc(parser,'get_slice(d:seq{long},x:range{any_int}) do'//&
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
        ' endif; result=x._st>0=>lo..hi by d._st*x._st||hi..lo by d._st*x._st endproc',&
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
   call dcl_uproc(parser,'get_slice(d:seq{long},x:strided_range_above{any_int}) do'//&
        ' lo:=0l;hi:=0l;'//&
        ' if d._st>0 then '//&
        '  lo=(long(x._t)-d._f+d._st-1)/d._st*d._st+d._f;'//&
        '  hi=d._hi '//&
        ' else '//&
        '  lo=d._hi;'//&
        '  hi=d._f-(d._f-long(x._t))/d._st*d._st '//&
        ' endif; result=x._s>0=>lo..hi by d._st*x._s||hi..lo by d._st*x._s endproc',&
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
   call dcl_uproc(parser,'get_slice(d:seq{long},x:strided_range_below{any_int}) do'//&
        ' lo:=0l;hi:=0l;'//&
        ' if d._st>0 then '//&
        '  lo=d._lo;'//&
        '  hi=(long(x._t)-d._f)/d._st*d._st+d._f '//&
        ' else '//&
        '  lo=d._f-(d._f-long(x._t)-d._st-1)/d._st*d._st;'//&
        '  hi=d._lo '//&
        ' endif; result=x._s>0=>lo..hi by d._st*x._st||hi..lo by d._st*x._s endproc',&
        line)
   call dcl_uproc(parser,'get_slice(d:seq{long},x:stride{any_int})=d by x',line)
   call dcl_uproc(parser,'get_slice(d:range{long},x:null)=d',line)
   call dcl_uproc(parser,'get_slice(d:seq{long},x:null)=d',line)
   call dcl_uproc(parser,'get_slice(d:cycle,x:index)=get_slice(d._x,x)',line)
   call dcl_uproc(parser,'get_slice(d:cycle,x:subs)=rec _slice{x=get_slice(d._x,x)}',line)
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
   ! Arguments - domain / location (shape units) / displacement (domain units)
   ! Result: displacement grid (domain units)
   call dcl_uproc(parser,'get_disp(d:cycle{long},j:long,x:any_int)='//&
        ' long(x) ',line)
   call dcl_uproc(parser,'get_disp(d:cycle{long},j:long,x:range{any_int})='//&
        ' long(x) ',line)
   call dcl_uproc(parser,'get_disp(d:cycle{long},j:long,x:seq{any_int})='//&
        ' long(x) ',line)
   call dcl_uproc(parser,'get_disp(d:cycle{range{long}},j:long,x:range_above{any_int})='//&
        ' -j..long(x._t) ',line)
   call dcl_uproc(parser,'get_disp(d:cycle{seq{long}},j:long,x:range_above{any_int})='//&
        ' -j*d._x._st..long(x._t) ',line)
   call dcl_uproc(parser,'get_disp(d:cycle{range{long}},j:long,x:range_below{any_int})='//&
        ' long(x._t)..d._x._hi-d._x._lo-j ',line)
   call dcl_uproc(parser,'get_disp(d:cycle{seq{long}},j:long,x:range_below{any_int})='//&
        ' long(x._t)..d._x._hi-d._x._lo-j*d._x._st ',line)
   call dcl_uproc(parser,'get_disp(d:cycle{range{long}},j:long,x:stride{any_int})='//&
        ' -j..d._x._hi-d._x._lo-j by x._t',line)
   call dcl_uproc(parser,'get_disp(d:cycle{seq{long}},j:long,x:stride{any_int})='//&
        ' -jj..d._x._hi-d._x._lo-jj by d._x._st*x._t where jj=j*d._x._st',line)
   call dcl_uproc(parser,'get_disp(d:cycle{range{long}},j:long,x:null)='//&
        ' -j..d._x._hi-d._x._lo-j',line)
   call dcl_uproc(parser,'get_disp(d:cycle{seq{long}},j:long,x:null)='//&
        ' -jj..d._x._hi-d._x._lo-jj by d._x._st where jj=j*d._x._st',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:any_int)='//&
        ' max(-j,xx)..min(d._hi-d._lo-j,xx) where xx=long(x)',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:range{any_int})='//&
        ' max(-j,long(x._lo))..min(d._hi-d._lo-j,long(x._hi))',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:seq{any_int})='//&
        ' max(-j,long(x._lo))..min(d._hi-d._lo-j,long(x._hi)) by x._st',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:range_below{any_int})='//&
        ' -j..min(d._hi-d._lo-j,long(x._t))',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:range_above{any_int})='//&
        ' max(-j,long(x._t))..d._hi-d._lo-j)',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:strided_range_below{any_int})='//&
        ' -j..min(d._hi-d._lo-j,long(x._t)) by x._s',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:strided_range_above{any_int})='//&
        ' max(-j,long(x._t))..d._hi-d._lo-j) by x._s',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:stride{any_int})='//&
        ' -j..d._hi-d._lo-j by long(x._t) ',line)
   call dcl_uproc(parser,'get_disp(d:range{long},j:long,x:null)='//&
        ' -j..d._hi-d._lo-j',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:any_int)'//&
        '=max(-jj,xx)..min(d._hi-d._lo-jj),xx) where xx=long(x),jj=j*d._st',&
        line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:range{any_int})'//&
        '=max(-jj,long(x._lo))..min(d._hi-d._lo-jj,long(x._hi))'//&
        ' where jj=j.d._st',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:seq{any_int})'//&
        '=max(-jj,long(x._lo))..min(d._hi-d._lo-jj,long(x._hi)) by x._st '//&
        'where jj=j*d._st',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:range_below{any_int})='//&
        ' -jj..min(d._hi-d._lo-jj,long(x._t)) where jj=j*d._st',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:range_above{any_int})='//&
        ' max(-jj,long(x._t))..d._hi-d._lo-jj where jj=j*d._st',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:strided_range_below{any_int})='//&
        ' -jj..min(d._hi-d._lo-jj,long(x._t)) by x._s where jj=j*d._st',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:strided_range_above{any_int})='//&
        ' max(-jj,long(x._t))..d._hi-d._lo-jj by x._s where jj=j*d._st',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:stride{any_int})='//&
        ' -jj..d._hi-d._lo-jj by long(x._t) where jj=j*d._st ',line)
   call dcl_uproc(parser,'get_disp(d:seq{long},j:long,x:null)='//&
        ' -jj..d._hi-d._lo-jj where jj=j*d._st ',line)
   
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
   
   ! Displacement slicing
   ! Args: domain / location (shape units) / displacement slice (shape units)
   ! Result: displacement grid (shape units)
   call dcl_uproc(parser,&
        'get_disp_shape(d:cycle{long},j:long,x:any_int)='//&
        ' long(x) ',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:cycle{long},j:long,x:range{any_int})='//&
        ' long(x) ',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:cycle{long},j:long,x:seq{any_int})='//&
        ' long(x) ',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:cycle{range{long}},j:long,x:range_above{any_int})='//&
        ' -j..long(x._t) ',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:cycle{seq{long}},j:long,x:range_above{any_int})='//&
        ' -j..long(x._t) ',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:cycle{range{long}},j:long,x:range_below{any_int})='//&
        ' long(x._t)..d._x._n-j ',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:cycle{seq{long}},j:long,x:range_below{any_int})='//&
        ' long(x._t)..d._x._n-j*d._x._st ',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:cycle{range{long}},j:long,x:stride{any_int})='//&
        ' -j..d._x._n-j by x._t',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:cycle{range{long}},j:long,x:stride{any_int})='//&
        ' -j..d._x._n-j by x._t',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:cycle{range{long}},j:long,x:null)='//&
        ' -j..d._x._n-j',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:cycle{range{long}},j:long,x:null)='//&
        ' -j..d._x._n-j',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:range{long},j:long,x:any_int)='//&
        ' max(-j,xx)..min(d._hi-d._lo-j,xx) where xx=long(x)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:range{long},j:long,x:range{any_int})='//&
        ' max(-j,long(x._lo))..min(d._hi-d._lo-j,long(x._hi))  ',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:range{long},j:long,x:seq{any_int})='//&
        ' max(-j,long(x._lo))..min(d._hi-d._lo-j,long(x._hi)) by x._st',line)
   call dcl_uproc(parser,'get_disp_shape(d:range{long},j:long,x:range_below{any_int})='//&
        ' -j..min(d._hi-d._lo-j,long(x._hi))',line)
   call dcl_uproc(parser,'get_disp_shape(d:range{long},j:long,x:range_above{any_int})='//&
        ' max(-j,long(x._t))..d._hi-d._lo-j)',line)
   call dcl_uproc(parser,'get_disp_shape(d:range{long},j:long,x:strided_range_below{any_int})='//&
        ' -j..min(d._hi-d._lo-j,long(x._t)) by x._s',line)
   call dcl_uproc(parser,'get_disp_shape(d:range{long},j:long,x:strided_range_above{any_int})='//&
        ' max(-j,long(x._t))..d._hi-d._lo-j) by x._s',line)
   call dcl_uproc(parser,'get_disp_shape(d:range{long},j:long,x:stride{any_int})='//&
        ' -j..d._hi-d._lo-j by long(x._t) ',line)
   call dcl_uproc(parser,'get_disp_shape(d:range{long},j:long,x:null)='//&
        ' -j..d._hi-d._lo-j ',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:seq{long},j:long,x:any_int)'//&
        '=max(-j,xx)..min(d._n-j,xx) where xx=long(x)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:seq{long},j:long,x:range{any_int})='//&
        '=max(-j,long(x._lo))..min(d._n-j,long(x._hi))  ',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:seq{long},j:long,x:seq{any_int})='//&
        '=max(-j,long(x._lo))..min(d._n-j,long(x._hi)) by x._st',line)
   call dcl_uproc(parser,'get_disp_shape(d:seq{long},j:long,x:range_below{any_int})='//&
        ' -j..min(d._n-j,long(x._hi))',line)
   call dcl_uproc(parser,'get_disp_shape(d:seq{long},j:long,x:range_above{any_int})='//&
        ' max(-j,long(x._t))..d._n-j)',line)
   call dcl_uproc(parser,'get_disp_shape(d:seq{long},j:long,x:strided_range_below{any_int})='//&
        ' -j..min(d._n-j,long(x._t)) by long(x._s)',line)
   call dcl_uproc(parser,'get_disp_shape(d:seq{long},j:long,x:strided_range_above{any_int})='//&
        ' max(-j,long(x._t))..d._n-j by long(x._s)',line)
   call dcl_uproc(parser,'get_disp_shape(d:seq{long},j:long,x:stride{any_int})='//&
        ' -j..d._n-j by long(x._t) ',line)
   call dcl_uproc(parser,'get_disp_shape(d:seq{long},j:long,x:null)='//&
        ' -j..d._n-j  ',line)
   call dcl_uproc(parser,&
        'get_disp_shape(x:vector,j,y)=get_disp_shape(x._d,j,y)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(x:matrix,j,y,z)=get_disp_shape(x._d,j,y,z)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:grid1d,j:tuple1d,i:tuple1d)='//&
        'get_disp_shape(d,j.d1,i.d1)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:grid1d,j:long,i:tuple1d)='//&
        'get_disp_shape(d,j,i.d1)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:grid2d,j:tuple2d,i:tuple2d)='//&
        'get_disp_shape(d,j,i.d1,i.d2)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:grid3d,j:tuple3d,i:tuple3d)='//&
        'get_disp_shape(d,j,i.d1,i.d2,i.d3)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:grid4d,j:tuple4d,i:tuple4d)='//&
        'get_disp_shape(d,j,i.d1,i.d2,i.d3,i.d4)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:grid5d,j:tuple5d,i:tuple5d)='//&
         'get_disp_shape(d,j,i.d1,i.d2,i.d3,i.d4,i.d5)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:grid6d,j:tuple6d,i:tuple6d)='//&
        'get_disp_shape(d,j,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:grid7d,j:tuple7d,i:tuple7d)='//&
        'get_disp_shape(d,j,i.d1,i.d2,i.d3,i.d4,i.d5,i.d6.i.d7)',line)
   
   call dcl_uproc(parser,&
        'get_disp_shape(d:vector,j:long,i:tuple1d)='//&
        'get_disp_shape(d,j,i.d1)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:vector,j:tuple1d,i:tuple1d)='//&
        'get_disp_shape(d,j.d1,i.d1)',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d:matrix,j:tuple2d,i:tuple2d)='//&
        'get_disp_shape(d,j,i.d1,i.d2)',line)
   
   call dcl_uproc(parser,'get_disp_shape(d,j,x)='//&
        'grid(get_disp_shape(d.d1,j,x))',line)
   call dcl_uproc(parser,'get_disp_shape(d,j,x,y)='//&
        'grid(get_disp_shape(d.d1,j,x),get_disp_shape(d.d2,j,y))',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d,j,x,y,z)='//&
        'grid(get_disp_shape(d.d1,j,x),get_disp_shape(d.d2,j,y),'//&
        'get_disp_shape(d.d3,j,z))',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d,j,x,y,z,a)='//&
        'grid(get_disp_shape(d.d1,j,x),get_disp_shape(d.d2,j,y),'//&
        'get_disp_shape(d.d3,j,z),get_disp_shape(d.d4,j,a))',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d,j,x,y,z,a,b)=grid(get_disp_shape(d.d1,j,x),'//&
        'get_disp_shape(d.d2,j,y),get_disp_shape(d.d3,j,z),'//&
        'get_disp_shape(d.d4,j,a),get_disp_shape(d.d5,j,b))',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d,j,x,y,z,a,b,c)=grid(get_disp_shape(d.d1,j,x),'//&
        'get_disp_shape(d.d2,j,y),get_disp_shape(d.d3,j,z),'//&
        'get_disp_shape(d.d4,j,a),get_disp_shape(d.d5,j,b),'//&
        'get_disp_shape(d.d6,j,c))',line)
   call dcl_uproc(parser,&
        'get_disp_shape(d,j,x,y,z,a,b,c,e)=grid(get_disp_shape(d.d1,j,x),'//&
        'get_disp_shape(d.d2,j,y),get_disp_shape(d.d3,j,z),'//&
        'get_disp_shape(d.d4,j,a),get_disp_shape(d.d5,j,b),'//&
        'get_disp_shape(d.d6,j,c),get_disp_shape(d.d7,j,e))',line)
   
   ! Displacement of a point in a domain
   ! Args: domain / location (shape units) / displacement (domain units)
   ! Result: displaced location in shape units
   call dcl_uproc(parser,'displace(d:cycle{long},x,y)=displace(d._x,x,y) mod size(d._x)',line)
   call dcl_uproc(parser,'displace(d:range{long},x:long,y:any_int)='//&
        'x+long(y)',line)
   call dcl_uproc(parser,'displace(d:range{long},x:tuple{long},y:any_int)='//&
        'x.d1+long(y)',line)
   call dcl_uproc(parser,'displace(d:range{long},x:long,y:tuple{any_int})='//&
        'x+long(y.d1)',line)
   call dcl_uproc(parser,&
        'displace(d:range{long},x:tuple{long},y:tuple{any_int})='//&
        'x.d1+long(y.d1)',line)
   call dcl_uproc(parser,'displace(d:seq{long},x:long,y:any_int)='//&
        'x+long(y)/d._st',line)
   call dcl_uproc(parser,'displace(d:seq{long},x:tuple{long},y:any_int)='//&
        'x.d1+long(y)/d._st',line)
   call dcl_uproc(parser,'displace(d:seq{long},x:long,y:tuple{any_int})='//&
        'x+long(y.d1)/d._st',line)
   call dcl_uproc(parser,&
        'displace(d:seq{long},x:tuple{long},y:tuple{any_int})='//&
        'x.d1+long(y.d1)/d._st',line)
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
    
    ! Displacement of a point in a domain (shape units)
    ! Args: domain / location (shape units) / displacement (shape units)
    ! Result: displaced location in shape units
    call dcl_uproc(parser,'displace_shape(d:cycle{long},x,y)=displace_shape(d._x,x,y) mod size(d._x)',line)
    call dcl_uproc(parser,'displace_shape(d:range{long},x:long,y:any_int)='//&
         'x+long(y)',line)
    call dcl_uproc(parser,&
         'displace_shape(d:range{long},x:tuple{long},y:any_int)='//&
         'x.d1+long(y)',line)
    call dcl_uproc(parser,&
         'displace_shape(d:range{long},x:long,y:tuple{any_int})='//&
         'x+long(y.d1)',line)
    call dcl_uproc(parser,&
         'displace_shape(d:range{long},x:tuple{long},y:tuple{any_int})='//&
         'x.d1+long(y.d1)',line)
    call dcl_uproc(parser,&
         'displace_shape(d:seq{long},x:long,y:any_int)='//&
         'x+long(y)',line)
    call dcl_uproc(parser,&
         'displace_shape(d:seq{long},x:tuple{long},y:any_int)='//&
         'x.d1+long(y)',line)
    call dcl_uproc(parser,&
         'displace_shape(d:seq{long},x:long,y:tuple{any_int})='//&
        'x+long(y.d1)',line)
    call dcl_uproc(parser,&
         'displace_shape(d:seq{long},x:tuple{long},y:tuple{any_int})='//&
         'x.d1+long(y.d1)',line)
    call dcl_uproc(parser,&
         'displace_shape(x:vector,y,z)=displace_shape(x._d,y,z)',line)
    call dcl_uproc(parser,&
         'displace_shape(x:matrix,y,z)=displace_shape(x._d,y,z)',line)
    call dcl_uproc(parser,'displace_shape(d:grid1d,x:any_int,y:any_int)='//&
         'displace_shape(d.d1,x,y)',line)
    call dcl_uproc(parser,'displace_shape(d:grid1d,x:tuple1d,y:tuple1d)='//&
         'tuple(displace_shape(d.d1,x.d1,y.d1))',line)
    call dcl_uproc(parser,'displace_shape(d:grid1d,x:any_int,y:tuple1d)='//&
         'tuple(displace_shape(d.d1,x,y.d1))',line)
    call dcl_uproc(parser,'displace_shape(d:grid1d,x:tuple1d,y:any_int)='//&
         'tuple(displace_shape(d.d1,x.d1,y))',line)
    call dcl_uproc(parser,'displace_shape(d:grid2d,x:tuple2d,y:tuple2d)='//&
         'tuple(displace_shape(d.d1,x.d1,y.d1),'//&
         'displace_shape(d.d2,x.d2,y.d2))',&
         line)
    call dcl_uproc(parser,'displace_shape(d:grid3d,x:tuple3d,y:tuple3d)='//&
         'tuple(displace_shape(d.d1,x.d1,y.d1),'//&
         'displace_shape(d.d2,x.d2,y.d2), '//&
         'displace_shape(d.d3,x.d3,y.d3) ) ',line)
    call dcl_uproc(parser,'displace_shape(d:grid4d,x:tuple4d,y:tuple4d)='//&
         'tuple(displace_shape(d.d1,x.d1,y.d1),'//&
         'displace_shape(d.d2,x.d2,y.d2), '//&
         'displace_shape(d.d3,x.d3,y.d3),'//&
         'displace_shape(d.d4,x.d4,y.d4) ) ',line)
    call dcl_uproc(parser,'displace_shape(d:grid5d,x:tuple5d,y:tuple5d)='//&
         'tuple(displace_shape(d.d1,x.d1,y.d1),'//&
         'displace_shape(d.d2,x.d2,y.d2), '//&
         'displace_shape(d.d3,x.d3,y.d3),displace_shape(d.d4,x.d4,y.d4),'//&
         'displace_shape(d.d5,x.d5,y.d5) ) ',line)
    call dcl_uproc(parser,'displace_shape(d:grid6d,x:tuple6d,y:tuple6d)='//&
         'tuple(displace_shape(d.d1,x.d1,y.d1),'//&
         'displace_shape(d.d2,x.d2,y.d2), '//&
         'displace_shape(d.d3,x.d3,y.d3),displace_shape(d.d4,x.d4,y.d4),'//&
         'displace_shape(d.d5,x.d5,y.d5),'//&
         'displace_shape(d.d6,x.d6,y.d6) ) ',line)
    call dcl_uproc(parser,'displace_shape(d:grid7d,x:tuple7d,y:tuple7d)='//&
         'tuple( displace_shape(d.d1,x.d1,y.d1),'//&
         'displace_shape(d.d2,x.d2,y.d2), '//&
         'displace_shape(d.d3,x.d3,y.d3),displace_shape(d.d4,x.d4,y.d4),'//&
         'displace_shape(d.d5,x.d5,y.d5),'//&
         'displace_shape(d.d6,x.d6,y.d6),'//&
         'displace_shape(d.d7,x.d7,y.d7) ) ',line)
    
    ! Index checking
    call dcl_uproc(parser,'_in(x,y)=low(y)<=x and x<=high(y)',line) 
    call dcl_uproc(parser,'_incl(x,y:any_int)= y in x',line)
    call dcl_uproc(parser,'_incl(x,y:range{any_int})= _in(y._lo,x) and _in(y._hi,x)',line)
    call dcl_uproc(parser,'_incl(x,y:seq{any_int})=  _in(y._lo,x) and _in(y._hi,x)',line)
    call dcl_uproc(parser,'_incl(x,y:range_below{any_int})= _in(y._t,x)',line)
    call dcl_uproc(parser,'_incl(x,y:range_above{any_int})= _in(y._t,x)',line)
    call dcl_uproc(parser,'_incl(x,y:strided_range_below{any_int})= _in(y._t,x) and y._s/=0',line)
    call dcl_uproc(parser,'_incl(x,y:strided_range_above{any_int})= _in(y._t,x) and y._s/=0',line)
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
    
    ! Index checking (zero base,unit stride) 
    call dcl_uproc(parser,&
         '_in_range(x,y:any_int)= yy>=0l and yy<size(x) where yy=long(y)',line)
    call dcl_uproc(parser,&
         '_in_range(x,y:range{any_int})='//&
         ' long(y._lo)>=0l and long(y._hi)<size(x)',line)
    call dcl_uproc(parser,&
         '_in_range(x,y:seq{any_int})= long(y._lo)>=0l and long(y._hi)<size(x)',&
         line)
    call dcl_uproc(parser,&
         '_in_range(x,y:range_below{any_int})='//&
         ' long(y._t)>=0l and long(y._t)<size(x)',line)
    call dcl_uproc(parser,&
         '_in_range(x,y:range_above{any_int})='//&
         ' long(y._t)>=0l and long(y._t)<size(x)',line)
    call dcl_uproc(parser,&
         '_in_range(x,y:strided_range_below{any_int})='//&
         ' long(y._t)>=0l and long(y._t)<size(x) and y._s/=0',line)
    call dcl_uproc(parser,&
         '_in_range(x,y:strided_range_above{any_int})='//&
         ' long(y._t)>=0l and long(y._t)<size(x) and y._s/=0',line)
    call dcl_uproc(parser,&
         '_in_range(x,y:stride{any_int})='//&
         ' y._t/=0',line)
    call dcl_uproc(parser,&
         '_in_range(x,y:null)=true',line)
    call dcl_uproc(parser,&
         'shape_contains(g:grid1d,t:tuple1d)='//&
         'shape_contains(g,t.d1)',line)
    call dcl_uproc(parser,&
         'shape_contains(g:grid2d,t:tuple2d)='//&
         'shape_contains(g,t.d1,t.d2)',line)
    call dcl_uproc(parser,&
         'shape_contains(g:grid3d,t:tuple3d)='//&
         'shape_contains(g,t.d1,t.d2,t.d3)',line)
    call dcl_uproc(parser,&
         'shape_contains(g:grid4d,t:tuple4d)='//&
         'shape_contains(g,t.d1,t.d2,t.d3,t.d4)',line)
    call dcl_uproc(parser,&
         'shape_contains(g:grid5d,t:tuple5d)='//&
         'shape_contains(g,t.d1,t.d2,t.d3,t.d4,t.d5)',line)
    call dcl_uproc(parser,&
         'shape_contains(g:grid6d,t:tuple6d)='//&
         'shape_contains(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6)',&
         line)
    call dcl_uproc(parser,&
         'shape_contains(g:grid7d,t:tuple7d)='//&
         'shape_contains(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6,t.d7)',line)
    call dcl_uproc(parser,'shape_contains(a:grid1d,d1)='//&
         '_in_range(a.d1,d1)',line)
    call dcl_uproc(parser,'shape_contains(a:grid2d,d1,d2)='//&
         ' _in_range(a.d1,d1) and _in_range(a.d2,d2)',line)
    call dcl_uproc(parser,'shape_contains(a:grid3d,d1,d2,d3)='//&
         '_in_range(a.d1,d1) and _in_range(a.d2,d2)'//&
         ' and _in_range(a.d3,d3)',line)
    call dcl_uproc(parser,'shape_contains(a:grid4d,d1,d2,d3,d4)='//&
         '_in_range(a.d1,d1) and _in_range(a.d2,d2)'//&
         ' and _in_range(a.d3,d3) and _in_range(a.d4,d4)',line)
    call dcl_uproc(parser,'shape_contains(a:grid5d,d1,d2,d3,d4,d5)='//&
         '_in_range(a.d1,d1) and _in_range(a.d2,d2)'//&
         ' and _in_range(a.d3,d3) and _in_range(a.d4,d4) '//&
         ' and _in_range(a.d5,d5) ',line)
    call dcl_uproc(parser,'shape_contains(a:grid6d,d1,d2,d3,d4,d5,d6)='//&
         '_in_range(a.d1,d1) and _in_range(a.d2,d2)'//&
         ' and _in_range(a.d3,d3) and _in_range(a.d4,d4) and'//&
         ' _in_range(a.d5,d5) '//&
         ' and _in_range(a.d6,d6) ',line)
    call dcl_uproc(parser,'shape_contains(a:grid7d,d1,d2,d3,d4,d5,d6,d7)='//&
         '_in_range(a.d1,d1) and _in_range(a.d2,d2)'//&
         ' and _in_range(a.d3,d3) and _in_range(a.d4,d4) '//&
         ' and _in_range(a.d5,d5) '//&
         ' and _in_range(a.d6,d6) and _in_range(a.d7,d7) ',line)
    call dcl_uproc(parser,&
         'shape_contains(a:vector,d1)=_in_range(a._d,d1)',line)
    call dcl_uproc(parser,&
         'shape_contains(a:matrix,d1)=shape_contains(a._d,d1)',line)
    call dcl_uproc(parser,&
         'shape_contains(a:matrix,d1,d2)=shape_contains(a._d,d1,d2)',line)
    
    ! Domain types
    call dcl_type(parser,&
         'dom includes vector,matrix,grid',line)
    call dcl_uproc(parser,'(//)(x:dom,arg...:subs) do '//&
         ' check contains(x,arg...); result=get_slice(x,arg...) endproc',line)
    call dcl_uproc(parser,'(%%)(x:dom,arg...:subs) do '//&
         ' check shape_contains(x,arg...); '//&
         'result=get_elem(x,arg...) endproc',line)
    
   ! Array types
   call dcl_type(parser,'array{e,d}',line)
   call dcl_type(parser,'array{e,d} '//&
        ' also includes e#d,_slice{e,d},_dim{e,d}',line)
   call dcl_type(parser,'array{e,d:grid_base} '//&
        ' also includes e#grid{d},_slice{e,grid{d}},_dim{e,grid{d}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2}'//&
        ' is e#grid{d1,d2},_slice2d{e,grid{d1,d2}},_dim{e,grid{d1,d2}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2,d3}'//&
        ' is e#grid{d1,d2,d3},_slice3d{e,grid{d1,d2,d3}},'//&
        '_dim{e,grid{d1,d2,d3}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2,d3,d4}'//&
        ' is e#grid{d1,d2,d3,d4},_slice4d{e,grid{d1,d2,d3,d4}},'//&
        '_dim{e,grid{d1,d2,d3,d4}}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2,d3,d4,d5}'//&
        ' is e#grid{d1,d2,d3,d4,d5},_slice5d{e,grid{d1,d2,d3,d4,d5}},'//&
        '_dim{e,grid{d1,d2,d3,d4,d5}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2,d3,d4,d5,'//&
        'd6}'//&
        ' is e#grid{d1,d2,d3,d4,d5,d6},_slice6d{e,grid{d1,d2,d3,d4,d5,d6}},'//&
        '_dim{e,grid{d1,d2,d3,d4,d5,d6}}',line)
   call dcl_type(parser,&
        'array{e,d1,d2,d3,d4,d5,'//&
        'd6,d7}'//&
        ' is e#grid{d1,d2,d3,d4,d5,d6,d7},'//&
        '_slice7d{e,grid{d1,d2,d3,d4,d5,d6,d7}},'//&
        ' _dim{e,grid{d1,d2,d3,d4,d5,d6,d7}}',line)

   ! Array operations
   call dcl_uproc(parser,'arb(x:any#dom)=PM__get_elem(x,0l)',line)
   call dcl_uproc(parser,'size(x:any#dom)=size(dom(x))',line)
   call dcl_proc(parser,'_array(x:any,y:any,z:any,s:any)->dim x,y',&
        op_array,0_pm_i16,line,proc_needs_type,&
        ' ',ftn_dim_array)
   call dcl_proc(parser,'PM__extractelm(x:any)->*x',&
        op_extractelm,0_pm_i16,line,0,'',ftn_extract_array_elem)
   call dcl_proc(parser,&
         '_makearray(x:any,y:any,z:any,n:any)->dim x,y',&
         op_make_array,0_pm_i16,line,proc_needs_type,'',ftn_make_array)
   call dcl_proc(parser,'dom(x:any#dom)->#x',op_get_dom,0_pm_i16,line,0,&
        '',ftn_array_dom)
   call dcl_uproc(parser,'redim(x:any#any,y:dom)='//&
        '_makearray(PM__extractelm(x),y,dom(y),sy)'//&
        ' check "New domain does not have same size in redim" :'//&
        ' sy==size(dom(x))'//&
        ' where sy=size(y)',&
        line)
   call dcl_uproc(parser,'over(x:any#any,y:dom)='//&
        '_makearray(PM__extractelm(x),y,dom(y),size(y))'//&
        ' check "New domain does not conform in over":conform(dom(x),y)',&
        line)
   
   call dcl_uproc(parser,'(//)(a:any#dom,arg...:index) do'//&
        ' check "subscript out of range":contains(dom(a),arg...);'//&
        ' result=PM__get_elem(a,index(dom(a),arg...)) endproc',line)
   call dcl_uproc(parser,'(%%)(a:any#dom,arg...:index) do'//&
        ' check  "subscript out of range":shape_contains(dom(a),arg...);'//&
        ' result=PM__get_elem(a,index_shape(dom(a),arg...)) endproc',line)
   
   call dcl_uproc(parser,'(//)=(&a:[],v,arg...:index)'//&
        ' do check "subscript out of range":contains(dom(a),arg...);'//&
        ' PM__set_elem(a,index(dom(a),arg...),v) endproc',line)
   call dcl_uproc(parser,'(%%)=(&a:[],v,arg...:index) do '//&
        ' check  "subscript out of range":shape_contains(dom(a),arg...);'//&
        ' PM__set_elem(a,index_shape(dom(a),arg...),v) endproc',line)
    
   call dcl_uproc(parser,'(//)=(&a:[],v,arg...:subs)'//&
        ' do PM__assign(&a(/tuple(arg...)/),v) endproc',line)
   call dcl_uproc(parser,'(%%)=(&a:[],v,arg...:subs)'//&
        ' do PM__assign(&a(%tuple(arg...)%),v) endproc',line)

   call dcl_uproc(parser,'PM__assign(&a:any#dom,v:any#dom)'//&
        'do check_conform(a,v);_assign(&a,v) endproc',line)
   call dcl_uproc(parser,'PM__assign_var(&a:any#dom,v:any#dom)'//&
        'do check_conform(a,v);_assign(&a,v) endproc',line)
   
   call dcl_uproc(parser,'PM__assign(&a:any#dom,v) do'//&
        ' for i in a conc do _assign(&i,v) endfor endproc ',line)
   call dcl_uproc(parser,'PM__assign_var(&a:any#dom,v) do'//&
        ' for i in a conc do _assign(&i,v) endfor endproc ',line)

   call dcl_type(parser,'_nest is any#dom',line)
   call dcl_uproc(parser,'PM__assign(&a:_nest#dom,v:any#dom)'//&
        'do _nested_array_assign(&a,v,has_same_type(arb(a),arb(v))) endproc',&
        line)
   call dcl_uproc(parser,'PM__assign_var(&a:_nest#dom,v:any#dom)'//&
        'do _nested_array_assign(&a,v,has_same_type(arb(a),arb(v))) endproc',&
        line)
   call dcl_uproc(parser,'_nested_array_assign(&a,v,x:null) '//&
        'do for i in a conc do _assign(&i,v) endfor endproc ',line)
   call dcl_uproc(parser,'_nested_array_assign(&a,v,x:affirm)'//&
        'do check_conform(a,v);_assign(&a,v) endproc',line)
   
   call dcl_uproc(parser,'get_elem(a:any#dom,arg...:index)='//&
        'PM__get_elem(a,index_shape(dom(a),arg...))',line)
   call dcl_uproc(parser,'set_elem(a:any#dom,v,arg...:index)'//&
        'do PM__set_elem(a,index_shape(dom(a),arg...),v) endproc',line)
   call dcl_uproc(parser,'get_subs(a:any#dom,arg...:index)='//&
           'PM__get_elem(a,index(dom(a),arg...))',line)
   call dcl_uproc(parser,'set_subs(a:any#dom,v,arg...:index)'//&
        'do PM__set_elem(a,index(dom(a),arg...),v) endproc',line)
   
   call dcl_proc(parser,'_make_subref(a:any#dom,any)->*a',&
        op_make_rf,0_pm_i16,line,0,'',ftn_subref)
   call dcl_uproc(parser,'PM__subref(a:any#dom,arg...:index) do '//&
        ' check contains(dom(a),arg...);'//&
        ' result=_make_subref(a,index(dom(a),arg...)) endproc',line)
   call dcl_uproc(parser,'PM__openref(a:any#dom,arg...:index) do '//&
        ' check shape_contains(dom(a),arg...);'//&
        ' result=_make_subref(a,index_shape(dom(a),arg...)) endproc',line)
   call dcl_uproc(parser,'PM__subref(a,arg...:subs)=proc[](a,arg...)',line)
   call dcl_uproc(parser,'PM__openref(a,arg...:subs)=proc[](a,arg...)',line)

   call dcl_proc(parser,'PM__get_elem(x:any#dom,y:long)->*x',&
        op_array_get_elem,0_pm_i16,line,0,'',ftn_get_elem)
   call dcl_proc(parser,'PM__set_elem(x:any#dom,y:long,z:any)',&
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

   
   ! Grid element generation
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
        '_elts(x.d3,s2,tot),_elts(x.d4,s3,tot)),'//&
        '_elts(x.d5,s3*size(x.d4), tot)) '//&
        'where s3=s2*size(x.d3) where s2=s1*size(x.d2) '//&
        'where s1=siz*size(x.d1)',&
        line)
   call dcl_uproc(parser,&
        '_elts(x:grid6d,siz,tot)=tuple(_elts( x.d1,siz,tot),'//&
        '_elts(x.d2,s1,tot),'//&
        '_elts(x.d3,s2,tot),_elts(x.d4,s3,tot)),_elts(x.d5,s4,tot),'//&
        '_elts(x.d6,s4*size(x.d5), tot)) '//&
        'where s4=s3*size(x.d4) where s3=s2*size(x.d3) '//&
        'where s2=s1*size(x.d2) where s1=siz*size(x.d1)',line)
   call dcl_uproc(parser,&
        '_elts(x:grid7d,siz,tot)=tuple(_elts( x.d1,siz,tot),'//&
        '_elts(x.d2,s1,tot),'//&
        '_elts(x.d3,s2,tot),_elts(x.d4,s3,tot)),'//&
        '_elts(x.d5,s4,tot),_elts(x.d6,s5,tot),'//&
        '_elts(x.d7,s5*size(x.d6), tot)) '//&
        'where s5=s4*size(x.d5) where s4=s3*size(x.d4) '//&
        'where s3=s2*size(x.d3) '//&
        'where s2=s1*size(x.d2) where s1=siz*size(x.d1)',line)
   
    ! Slices
    call dcl_type(parser,'_slice{e,d} is rec _slice{_a:e#any,_s,_d:d}',line)
    call dcl_uproc(parser,'arb(x:_slice{,})=PM__get_elem(x._a,0l)',line)
    call dcl_uproc(parser,'size(x:_slice{,})=size(x._d)',line)
    call dcl_uproc(parser,'dom(x:_slice{,})=x._d',line)
    call dcl_uproc(parser,'redim(x:_slice{,},y:dom) '//&
         'do a:=x;result=redim(a,y) endproc',line)
    call dcl_uproc(parser,'over(x:_slice{,},y:dom) '//&
         'do a:=x;result=a over y endproc',line)
    
    call dcl_uproc(parser,&
         '(//)(a:any#dom,arg...:slice) do'//&
         ' result=rec _slice{_a=a,_s=_sliceit(dm,arg...),_d=d} '//&
         ' check "slice out of range": contains(dm,arg...) '//&
         ' where d=get_slice(dm,arg...) where dm=dom(a) endproc',line)
    call dcl_uproc(parser,&
         '(//)(a:any#dom,arg...:subs)='//&
         ' rec _slice{_a=a,_s=_sliceit(dm,arg...),_d=_shrinkit(d,arg...)}'//&
         ' check "slice out of range": contains(dm,arg...)'//&
         ' where d=get_slice(dm,arg...) where dm=dom(a)',line)
    call dcl_uproc(parser,&
         '(%%)(a:any#dom,arg...:slice) do'//&
         ' check "slice out of range": shape_contains(dom(a),arg...) '//&
         ' result=rec _slice{_a=a,_s=_sliceit(dm,arg...),_d=d} '//&
         ' where d=get_elem(dm,arg...) where dm=dom(a) endproc',line)
    call dcl_uproc(parser,&
         '(%%)(a:any#dom,arg...:subs)='//&
         ' rec _slice{_a=a,_s=_sliceit(dm,arg...),_d=_shrinkit(d,arg...)}'//&
         ' check "slice out of range": shape_contains(dm,arg...) '//&
         ' where d=get_elem(dm,arg...) where dm=dom(a)',line)
    
    call dcl_uproc(parser,&
         '(//)(a:_slice{,},arg...:index) do '//&
         ' check "subscript out of range": contains(a._d,arg...); '//&
         ' result=PM__get_elem(a._a,index(a._s,a._d,arg...)) endproc',&
         line)
    
    call dcl_uproc(parser,&
         '(//)(a:_slice{,},arg...:slice)='//&
         'rec _slice{_a=a._a,_s=_reslice(a._s,a._d,arg...),_d=d} '//&
         'check "slice out of range": contains(a._d,arg...) '//&
         'where d=get_slice(a._d,arg...)',line)
    call dcl_uproc(parser,&
         '(//)(a:_slice{,},arg...:subs)= '//&
         ' rec _slice{_a=a,_s=_reslice(a._s,a._d,arg...),'//&
         ' _d=_shrinkit(get_slice(a._d,arg...),arg...)}'//&
         ' check "subscript out of range": contains(a._d,arg...)',line)
    call dcl_uproc(parser,&
         '(%%)(a:_slice{,},arg...:index) do '//&
         ' check "subscript out of range": shape_contains(a._d,arg...) ; '//&
         ' result=PM__get_elem(a._a,index_shape(a._s,arg...)) endproc',&
         line)
    call dcl_uproc(parser,&
         '(%%)(a:_slice{,},arg...:slice)='//&
         ' rec _slice{_a=a._a,_s=_reslice(a._s,a._d,arg...),_d=d} '//&
         ' check "slice out of range": shape_contains(a._d,arg...) '//&
         ' where d=get_elem(a._d,arg...)',line)
    call dcl_uproc(parser,&
         '(%%)(a:_slice{,},arg...) do '//&
         ' check "slice out of range": shape_contains(a._d,arg...) '//&
         ' result=rec _slice{_a=a,_s=_reslice(a._s,a._d,arg...),'//&
         '_d=_shrinkit(get_elem(a._d,arg...),arg...)} endproc',line)

    call dcl_uproc(parser,&
         '(//)=(a:_slice{,},v,arg...:index) do'//&
         ' check "subscript out of range":contains(a._d,arg...);'//&
         ' PM__set_elem(a._a,index(a._s,a._d,arg...),v) endproc',line)
    call dcl_uproc(parser,&
         '(%%)=(a:_slice{,},v,arg...:index) do'//&
         ' check "subscript out of range":shape_contains(a._d,arg...);'//&
         ' PM__set_elem(a._a,index_shape(a._s,arg...),v) endproc',line)

    call dcl_uproc(parser,'PM__assign(&a:_slice{,},b) do'//&
         ' for i in a conc do i=b endfor endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:_slice{,},b) do'//&
         ' for i in a conc do i=b endfor endproc',line)
        call dcl_uproc(parser,'PM__assign(&a:_slice{,},b:any#any) do'//&
         ' for i,j in a,b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:_slice{,},b:any#any) do'//&
         ' for i,j in a,b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign(&a:any#dom,b:_slice{,}) do'//&
         ' for i,j in a,b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:any#dom,b:_slice{,}) do'//&
         ' for i,j in a,b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign(&a:_slice{,},b:_slice{,}) do'//&
         ' for i,j in a,b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:_slice{,},b:_slice{,}) do'//&
         ' for i,j in a,b conc do i=j endfor endproc',line)
    call dcl_uproc(parser,'PM__assign(&a:_nest#dom,b:_slice{,}) do'//&
         ' _assign_a_slice_nested(&a,b,has_same_type(arb(a),arb(b))) '//&
         'endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:_nest#dom,b:_slice{,}) do'//&
         ' _assign_a_slice_nested(&a,b,has_same_type(arb(a),arb(b))) '//&
         'endproc',line)
    call dcl_uproc(parser,'PM__assign(&a:_slice{_nest,any},b:_slice{,}) do'//&
         ' _assign_a_slice_nested(&a,b,has_same_type(arb(a),arb(b))) '//&
         'endproc',line)
    call dcl_uproc(parser,'PM__assign_var(&a:_slice{_nest,any},b:_slice{,}) '//&
         'do'//&
         ' _assign_a_slice_nested(&a,b,has_same_type(arb(a),arb(b))) '//&
         'endproc',line)
    call dcl_uproc(parser,'_assign_a_slice_nested(&a,b,v:null)'//&
         ' do for i in a conc do i=b endfor endproc',line)
    call dcl_uproc(parser,'_assign_a_slice_nested(&a,b,v:affirm)'//&
         ' do for i,j in a,b conc do i=j endfor endproc',line)
    
    call dcl_uproc(parser,'PM__dup(x:_slice{,}) '//&
         ' do y:=arb(x) dim dom(x);y=x;result=y endproc',line)
    
    call dcl_uproc(parser,'get_elem(a:_slice{,},arg...:index)='//&
         'PM__get_elem(a._a,index_shape(a._s,arg...))',line)
    call dcl_uproc(parser,'set_elem(a:_slice{,},v,arg...:index)'//&
        'do PM__set_elem(a._a,index_shape(a._s,arg...),v) endproc',line)
 
    call dcl_uproc(parser,'PM__subref(a:_slice{,},arg...:index) do '//&
        ' check contains(a._d,arg...);'//&
        ' result=_make_subref(a._a,index(a._s,arg...)) endproc',line)
    call dcl_uproc(parser,'PM__openref(a:_slice{,},arg...:index) do '//&
        ' check shape_contains(a._d,arg...);'//&
        ' result=_make_subref(a,index_shape(a._s,arg...)) endproc',line)
    
    call dcl_type(parser,'_slice1d is rec _slice{o,s1}',line)
    call dcl_type(parser,'_slice2d is rec _slice{o,s1,s2}',line)
    call dcl_type(parser,'_slice3d is rec _slice{o,s1,s2,s3}',line)
    call dcl_type(parser,'_slice4d is rec _slice{o,s1,s2,s3,s4}',line)
    call dcl_type(parser,'_slice5d is rec _slice{o,s1,s2,s3,s4,s5}',line)
    call dcl_type(parser,&
         '_slice6d is rec _slice{o,s1,s2,s3,s4,s5,s6}',&
         line)
    call dcl_type(parser,&
         '_slice7d is rec _slice{o,s1,s2,s3,s4,s5,s6,s7}',&
         line)

    ! Tuple index of slice
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

    call dcl_uproc(parser,&
         'index_shape(g:_slice1d,t:tuple1d)=index_shape(g,t.d1)',line)
    call dcl_uproc(parser,&
         'index_shape(g:_slice2d,t:tuple2d)='//&
         'index_shape(g,t.d1,t.d2)',line)
    call dcl_uproc(parser,&
         'index_shape(g:_slice3d,t:tuple3d)='//&
         'index_shape(g,t.d1,t.d2,t.d3)',line)
    call dcl_uproc(parser,&
         'index_shape(g:_slice4d,t:tuple4d)='//&
         'index_shape(g,t.d1,t.d2,t.d3,t.d4)',line)
    call dcl_uproc(parser,&
         'index_shape(g:_slice5d,t:tuple5d)='//&
         'index_shape(g,t.d1,t.d2,t.d3,t.d4,t.d5)',&
         line)
    call dcl_uproc(parser,&
         'index_shape(g:_slice6d,t:tuple6d)='//&
         'index_shape(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6)',line)
    call dcl_uproc(parser,&
         'index_shape(g:_slice7d,t:tuple7d)='//&
         'index_shape(g,t.d1,t.d2,t.d3,t.d4,t.d5,t.d6,t.d7)',line)
    
    call dcl_uproc(parser,'index_shape(x:_slice1d,y)=x.o+x.s1*y',line)
    call dcl_uproc(parser,&
         'index_shape(x:_slice2d,y1,y2)=x.o+x.s1*y1+x.s2*y2',line)
    call dcl_uproc(parser,'index_shape(x:_slice3d,y1,y2,y3)=x.o'//&
         '+x.s1*y1+x.s2*y2+x.s3*y3',line)
    call dcl_uproc(parser,'index_shape(x:_slice4d,y1,y2,y3,y4)=x.o'//&
         '+x.s1*y1+x.s2*y2+'//&
         'x.s3*y3+x.s4*y4',line)
    call dcl_uproc(parser,'index_shape(x:_slice5d,y1,y2,y3,y4,y5)=x.o'//&
         '+x.s1*y1+x.s2*y2+'//&
         'x.s3*y3+x.s4*y4+x.s5*y5',line)
    call dcl_uproc(parser,'index_shape(x:_slice6d,y1,y2,y3,y4,y5,y6)=x.o'//&
         '+x.s1*y1+x.s2*y2+'//&
         'x.s3*y3+x.s4*y4+x.s5*y5'//&
         '+x.s6*y6',line)
    call dcl_uproc(parser,'index_shape(x:_slice6d,y1,y2,y3,y4,y5,y6,y7)=x.o'//&
         '+x.s1*y1+x.s2*y2+'//&
         'x.s3*y3+x.s4*y4+x.s5*y5'//&
         '+x.s6*y6+x.s7*y7',line)
    
    call dcl_uproc(parser,'_slice(o,s1)=rec _slice{o=o,s1=s1}',line)
    call dcl_uproc(parser,&
         '_slice(o,s1,s2)=rec _slice{o=o,s1=s1,s2=s2}',line)
    call dcl_uproc(parser,'_slice(o,s1,s2,s3)='//&
         'rec _slice{o=o,s1=s1,s2=s2,s3=s3}',line)
    call dcl_uproc(parser,'_slice(o,s1,s2,s3,s4)='//&
         'rec _slice{o=o,s1=s1,s2=s2,s3=s3,s4=s4}',line)
    call dcl_uproc(parser,'_slice(o,s1,s2,s3,s4,s5)='//&
         'rec _slice{o=o,s1=s1,s2=s2,s3=s3,s4=s4,s5=s5}',line)
    call dcl_uproc(parser,'_slice(o,s1,s2,s3,s4,s5,s6)='//&
         'rec _slice{o=o,s1=s1,s2=s2,s3=s3,s4=s4,'//&
         's5=s5,s6=s6}',line)
    call dcl_uproc(parser,'_slice(o,s1,s2,s3,s4,s5,s6,s7)='//&
         'rec _slice{o=o,s1=s1,s2=s2,s3=s3,s4=s4,'//&
         's5=s5,s6=s6,s7=s7}',line)

    call dcl_uproc(parser,'_shr(a,c)=a',line)
    call dcl_proc(parser,'_shr(a:any,b:index)->PM__tinyint',&
         op_miss_arg,0_pm_i16,line,0,'',ftn_miss_arg)
    call dcl_uproc(parser,'_doslice(arg...)=_slice(arg...)',line)
    call dcl_uproc(parser,'_shl(d,m,i:index)=index(d,i)*m',line)
    call dcl_uproc(parser,'_shl(d,m,i)=index(d,low(i))*m',line)
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
         '_sliceit(d:grid1d,i1)=_doslice(index(d.d1,low(i1)),1l)',line)
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
    call dcl_uproc(parser,'over(a:_dim{,},d:dom)=rec _dim{ _a=a,_d=d}'//&
         ' check "New domain does not conform in over":conform(d,a._d)',line)
    call dcl_uproc(parser,'dim(a:any,d:dom)=rec _dim{_a=a,_d=d}',line)
    call dcl_uproc(parser,'PM__do_dim(a:any,d:any)='//&
         '_array(a,d,dom(d),size(d))',&
         line)
    
    call dcl_uproc(parser,'(//)(a:_dim{,},arg...:index)='//&
         'a._a check "index out of range" contains(a._d,arg...)',line)
    call dcl_uproc(parser,'(%%)(a:_dim{,},arg...:index)='//&
         'a._a check "index out of range" shape_contains(a._d,arg...)',line)
    
    call dcl_uproc(parser,'(//)(a:_dim{,},arg...:slice) do'//&
         ' check "slice out of range":contains(a._d,arg...);'//&
         ' result=rec _dim{_a=a._a,_d=get_slice(a._d,arg...)} endproc',line)
    call dcl_uproc(parser,'(%%)(a:_dim{,},arg...:slice) do'//&
         ' check "slice out of range":shape_contains(a._d,arg...);'//& 
         ' result=rec _dim{_a=a.a,_d=get_elem(a._d,arg...)} endproc',line)
    call dcl_uproc(parser,'(//)(a:_dim{,},arg...:subs) do'//&
         ' check "slice out of range":shape_contains(a._d,arg...);'//& 
         ' result=rec _dim{_a=a._a,_d=_shrinkit(a._d,arg...)} endproc',line)
    call dcl_uproc(parser,'(%%)(a:_dim{,},arg...:subs) do'//&
         ' check "slice out of range":shape_contains(a._d,arg...);'//& 
         ' result=rec _dim{_a=a.a,'//&
         ' _d=_shrinkit(get_elem(a._d,arg...),arg...)} endproc',line)
    call dcl_uproc(parser,'get_elem(a:_dim{,},arg...)=a._a',line)
    
    call dcl_uproc(parser,'PM__dup(a:_dim{,})='//&
         '_array(a._a,a._d,dom(a._d),size(a._d))',line)
    call dcl_uproc(parser,'PM__pdup(a:_dim{,})='//&
         '_array(a._a,a._d,dom(a._d),size(a._d))',line)
    
    call dcl_uproc(parser,'PM__assign(a:[],b:_dim{,}) do a=b._a endproc',line)
    call dcl_uproc(parser,'PM__assign_var(a:[],b:_dim{,}) '//&
         'do a=b._a endproc',line)
    call dcl_uproc(parser,'PM__assign(a:_nest[],b:_dim{,}) '//&
         'do _assign_nested_dim(a,b,has_same_type(arb(a),b._a)) endproc',line)
    call dcl_uproc(parser,'PM__assign_var(a:_nest[],b:_dim{,}) '//&
         'do _assign_nested_dim(a,b,has_same_type(arb(a),b._a)) endproc',line)
    call dcl_uproc(parser,'_assign_nested_dim(a,b,x:null)'//&
         'do for i in a do i=b endfor endproc',line)
    call dcl_uproc(parser,'_assign_nested_dim(a,b,x:affirm) do a=b._a endproc',&
         line)
 
    ! Block partition
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
         'block(g:grid1d,d:tuple{long})='//&
         'rec _block{_hi=tuple(size),_p=d,_n=d.d1}'//&
         ' where size=size(g.d1)',line)
    call dcl_uproc(parser,&
         'block(g:grid2d,d:tuple{long,long})='//&
         'rec _block{_hi=tuple(size1,size2),'//&
         ' _p=d,_n=d.d1*d.d2}'//&
         ' where size1=size(g.d1),size2=size(g.d2)',line)
    call dcl_uproc(parser,&
         'block(g:grid3d,d:tuple{long,long,long})='//&
         'rec _block{_hi=tuple(size1,size2,size3),'//&
         ' _p=d,_n=d.d1*d.d2*d.d3}'//&
         ' where size1=size(g.d1),size2=size(g.d2),size3=size(g.d3)',line)
    call dcl_uproc(parser,&
         'block(g:grid4d,d:tuple{long,long,long,long})='//&
         'rec _block{_hi=tuple(size1,size2,size3,size4),'//&
         ' _p=d,_n=d.d1*d.d2*d.d3*d.d4}'//&
         ' where size1=size(g.d1),size2=size(g.d2),'//&
         'size3=size(g.d3),size4=size(g.d4)',line)
    call dcl_uproc(parser,&
         'block(g:grid5d,d:tuple{long,long,long,long,long})='//&
         'rec _block{_hi=tuple(size1,size2,size3,size4,size5),'//&
         ' _p=d,_n=d.d1*d.d2*d.d3*d.d4*d.d5}'//&
         ' where size1=size(g.d1),size2=size(g.d2),size3=size(g.d3),'//&
         ' size4=size(g.d4),size5=size(g.d5)',line)
    call dcl_uproc(parser,&
         'block(g:grid6d,d:tuple{long,long,long,long,long,long})='//&
         'rec _block{_hi=tuple(size1,size2,size3,size4,size5,size6),'//&
         ' _p=d,_n=d.d1*d.d2*d.d3*d.d4*d.d5*d.d6}'//&
         ' where size1=size(g.d1),size2=size(g.d2),size3=size(g.d3),'//&
         ' size4=size(g.d4),size5=size(g.d5),size6=size(g.d6)',line)
    call dcl_uproc(parser,&
         'block(g:grid7d,d:tuple{long,long,long,long,long,long,long})='//&
         'rec _block{_hi=tuple(size1,size2,size3,size4,size5,size6,size7),'//&
         ' _p=d,_n=d.d1*d.d2*d.d3*d.d4*d.d5*d.d6*d.d7}'//&
         ' where size1=size(g.d1),size2=size(g.d2),size3=size(g.d3),'//&
         ' size4=size(g.d4),size5=size(g.d5),'//&
         ' size6=size(g.d6),size7=size(g.d7)',line)
    
    call dcl_uproc(parser,&
         'block(g:grid,arg...:long)='//&
         ' block(g,tuple(arg...))',line)

    call dcl_uproc(parser,'dom(b:block)=grid(0l..b._n-1l)',line)

    call dcl_uproc(parser,'distr_single(d:grid1d)='//&
         'rec _block{_hi=tuple(s),'//&
         '_p=tuple(1l),_n=1l} '//&
         'where s=size(d.d1)',line)
    call dcl_uproc(parser,'distr_single(d:grid2d)='//&
         'rec _block{_hi=tuple(s1,s2),_p=tuple(1l,1l),_n=1l}'//&
         'where s1=size(d.d1),s2=size(d.d2)',line)
    call dcl_uproc(parser,'distr_single(d:grid3d)='//&
         'rec _block{_hi=tuple(s1,s2,s3),'//&
         '_p=tuple(1l,1l,1l),_n=1l}'//&
         'where s1=size(d.d1),s2=size(d.d2),s3=size(d.d3)',line)
    call dcl_uproc(parser,'distr_single(d:grid4d)='//&
         'rec _block{_hi=tuple(s1,s2,s3,s4),'//&
         '_p=tuple(1l,1l,1l,1l),_n=1l}'//&
         'where s1=size(d.d1),s2=size(d.d2),s3=size(d.d3),s4=size(d.d4)',line)
    call dcl_uproc(parser,'distr_single(d:grid5d)='//&
         'rec _block{_hi=tuple(s1,s2,s3,s4,s5),'//&
         '_p=tuple(1l,1l,1l,1l,1l),_n=1l}'//&
         'where s1=size(d.d1),s2=size(d.d2),s3=size(d.d3),'//&
         's4=size(d.d4),s5=size(d.d5)',line)
    call dcl_uproc(parser,'distr_single(d:grid6d)='//&
         'rec _block{_hi=tuple(s1,s2,s3,s4,s5,s6),'//&
         '_p=tuple(1l,1l,1l,1l,1l,1l),_n=1l}'//&
         'where s1=size(d.d1),s2=size(d.d2),s3=size(d.d3),'//&
         's4=size(d.d4),s5=size(d.d5),s6=size(d.d6)',line)
    call dcl_uproc(parser,'distr_single(d:grid7d)='//&
         'rec _block{_hi=tuple(s1,s2,s3,s4,s5,s6,s7),'//&
         '_p=tuple(1l,1l,1l,1l,1l,1l,1l),_n=1l}'//&
         'where s1=size(d.d1),s2=size(d.d2),s3=size(d.d3),'//&
         's4=size(d.d4),s5=size(d.d5),s6=size(d.d6),s7=size(d.d7)',line)
    
    call dcl_uproc(parser,'distr_elem(d:grid1d)='//&
         'rec _block{_hi=tuple(s),_p=tuple(s),_n=s} '//&
         'where s=size(d.d1)',line)
    call dcl_uproc(parser,'distr_elem(d:grid2d)='//&
         'rec _block{_hi=tuple(s1,s2),_p=tuple(s1,s2),_n=size(d)}'//&
         'where s1=size(d.d1),s2=size(d.d2)',line)
    call dcl_uproc(parser,'distr_elem(d:grid3d)='//&
         'rec _block{_hi=tuple(s1,s2,s3),'//&
         '_p=tuple(s1,s2,s3),_n=size(d)}'//&
         'where s1=size(d.d1),s2=size(d.d2),s3=size(d.d3)',line)
    call dcl_uproc(parser,'distr_elem(d:grid4d)='//&
         'rec _block{_hi=tuple(s1,s2,s3,s4),'//&
         '_p=tuple(s1,s2,s3,s4),_n=size(d)}'//&
         'where s1=size(d.d1),s2=size(d.d2),s3=size(d.d3),s4=size(d.d4)',line)
    call dcl_uproc(parser,'distr_elem(d:grid5d)='//&
         'rec _block{_hi=tuple(s1,s2,s3,s4,s5),'//&
         '_p=tuple(s1,s2,s3,s4,s5),_n=size(d)}'//&
         'where s1=size(d.d1),s2=size(d.d2),s3=size(d.d3),s4=size(d.d4),'//&
         ' s5=size(d.d5)',line)
    call dcl_uproc(parser,'distr_elem(d:grid6d)='//&
         'rec _block{_hi=tuple(s1,s2,s3,s4,s5,s6),'//&
         '_p=tuple(s1,s2,s3,s4,s5,s6),_n=size(d)}'//&
         'where s1=size(d.d1),s2=size(d.d2),s3=size(d.d3),s4=size(d.d4),'//&
         ' s5=size(d.d5),s6=size(d.d6)',line)
   call dcl_uproc(parser,'distr_elem(d:grid7d)='//&
        'rec _block{_hi=tuple(s1,s2,s3,s4,s5,s6,s7),'//&
        '_p=tuple(s1,s2,s3,s4,s5,s6,s7),_n=size(d)}'//&
        'where s1=size(d.d1),s2=size(d.d2),s3=size(d.d3),s4=size(d.d4),'//&
        ' s5=size(d.d5),s6=size(d.d6),s7=size(d.d7)',line)
    
    call dcl_uproc(parser,'[](b:block,i:any_int)='//&
         'get_elem(b,i) check "Index out of bounds": i<b._n',line)
    call dcl_uproc(parser,'{}(b:block,i:any_int)='//&
         'get_elem(b,i) check "Index out of bounds": i<b._n',line)
    
    call dcl_uproc(parser,&
         'get_elem(b:block1d,i:any_int)=grid(start..finish)'//&
         ' where finish=(ii+1l)*b._hi.d1/b._p.d1-1l'//&
         ' where start=ii*b._hi.d1/b._p.d1 where ii=long(i)',line)
    call dcl_uproc(parser,&
         'get_elem(b:block2d,i:any_int)='//&
         ' grid(start1..finish1,start2..finish2)'//&
         ' where finish1=(i1+1l)*b._hi.d1/b._p.d1-1l,'//&
         '       finish2=(i2+1l)*b._hi.d2/b._p.d2-1l'//&
         ' where start1=i1*b._hi.d1/b._p.d1,'//&
         '       start2=i2*b._hi.d2/b._p.d2'//&
         ' where i2=ii-b._p.d2*i1 where i1=ii/b._p.d2'//&
         ' where ii=long(i)',line)
    
    call dcl_uproc(parser,'prc_for(b:block1d,j:any_int)='//&
         '(b._p.d1*(long(j)+1)-1)/b._hi.d1',line)
    call dcl_uproc(parser,'prc_for(b:block1d,j:tuple1d)='//&
         '(b._p.d1*(long(j.d1)+1)-1)/b._hi.d1',line)
    call dcl_uproc(parser,'prc_for(b:block2d,j:tuple2d)='//&
         'b._p.d2*((b._p.d1*(long(j.d1)+1l)-1l)/b._hi.d1)+'//&
         '(b._p.d2*(long(j.d2)+1l)-1l)/b._hi.d2',line)

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
    call dcl_proc(parser,'PM__extract1st(x:any,y:any)->=x',&
         op_extract_first,0_pm_i16,line,0,'',ftn_extract_first)
    call dcl_uproc(parser,&
         'PM__makearray(x,d)=_makearray(x,d,dom(d),size(d))',line)
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
    call dcl_proc(parser,'is_par()->bool',op_is_par,0_pm_i16,line,0,&
         '$1=PRC_FRAME(PRC_DEPTH)%THIS_NPRC>1',ftn_simple)

    call dcl_proc(parser,'_push_prc_grid(bool)->long',&
         op_push_prc_grid,0_pm_i16,line,0,&
         'CALL PUSH_PRC_GRID_1D($1,$2)',ftn_simple)
    call dcl_proc(parser,'_push_prc_grid(bool,bool)->'//&
         'long,long',&
         op_push_prc_grid,0_pm_i16,line,0,&
         'CALL PUSH_PRC_GRID_2D($1,$2,$3,$4)',ftn_simple)
    call dcl_proc(parser,'_push_prc_grid(bool,bool,bool)->'//&
         'long,long,long',&
         op_push_prc_grid,0_pm_i16,line,0,&
         'CALL PUSH_PRC_GRID_3D($1,$2,$3,$4,$5,$6)',ftn_simple)
    call dcl_proc(parser,'_push_prc_grid(bool,bool,bool,bool)->'//&
         'long,long,long,long',&
         op_push_prc_grid,0_pm_i16,line,0,&
         'CALL PUSH_PRC_GRID_4D($1,$2,$3,$4,$5,$6,$7,$8)',ftn_simple)
    call dcl_proc(parser,'_push_prc_grid(bool,bool,bool,bool,bool)->'//&
         'long,long,long,long,long',&
         op_push_prc_grid,0_pm_i16,line,0,&
         'CALL PUSH_PRC_GRID_5D($1,$2,$3,$4,$5,$6,$7,$8,$9,$10)',ftn_simple)
    call dcl_proc(parser,'_push_prc_grid(bool,bool,bool,bool,bool,bool)->'//&
         'long,long,long,long,long,long',&
         op_push_prc_grid,0_pm_i16,line,0,&
         'CALL PUSH_PRC_GRID_6D($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)',&
         ftn_simple)
    call dcl_proc(parser,'_push_prc_grid(bool,bool,bool,'//&
         'bool,bool,bool,bool)->'//&
         'long,long,long,long,long,long,long',&
         op_push_prc_grid,0_pm_i16,line,0,&
         'CALL PUSH_PRC_GRID_7D($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14)'&
         ,ftn_simple)
    call dcl_proc(parser,'_push_prc_split(long)',op_push_prc_split,&
         0_pm_i16,&
         line,0,'CALL PUSH_PRC_SPLIT($1)',ftn_simple)
    call dcl_proc(parser,'_push_prc_conc()',op_push_prc_conc,0_pm_i16,&
         line,0,'CALL PUSH_PRC_CONC',ftn_simple)
    call dcl_proc(parser,'PM__pop_prc(bool)',op_pop_prc,0_pm_i16,line,0,&
         'CALL POP_PRC($1)',ftn_simple)
    call dcl_proc(parser,'PM__pop_conc(bool)',op_pop_prc_conc,&
         0_pm_i16,line,0,'CALL POP_CONC($1)',ftn_simple)
    call dcl_proc(parser,'_prc_test_push()->bool',op_prc_test_push,&
         0_pm_i16,line,0,'$1=TEST_PUSH()',ftn_simple)
    
    call dcl_uproc(parser,'PM__partition(d) do'//&
         ' shape:=dom(d); '//&
         ' elem:= shape;   '//&
         ' distr:= distr_single(elem); '//&
         ' prc:= false; '//&
         ' if _prc_test_push() then       '//&
         '   if shared_nprc()>size(shape) then '//&
         '     distr=distr_elem(d); '//&
         '     n:=shared_prc() mod size(d); '//&
         '     elem=get_elem(distr,n); '//&
         '     _push_prc_split(n);'//&
         '   else  '//&
         '     elem,distr=_distribute(shape)'//&
         '   endif;'//&
         '   prc=true'//&
         ' endif;'//&
         ' result=elem,distr,prc endproc',line)
    
    call dcl_uproc(parser,'PM__conc(d)=dom(d),null,false '//&
         'do _push_prc_conc() endproc',&
         line)
    
    call dcl_uproc(parser,'_distribute(d:grid1d)=get_elem(distr,this_prc()),'//&
         'distr where distr=block(d,tuple(d1))'//&
         'where d1=_push_prc_grid(is_cyclic(d.d1))',line)
    call dcl_uproc(parser,'_distribute(d:grid2d)=get_elem(distr,this_prc()),'//&
         'distr where distr=block(d,tuple(d1,d2))'//&
         'where d1,d2=_push_prc_grid(is_cyclic(d.d1),is_cyclic(d.d2))',line)
    
    call dcl_uproc(parser,'assemble(&a,distr) do'//&
         ' for each i in dom(distr) do'//&
         '   tile:=distr[i];'//&
         '   if i==this_prc() then '//&
         '     for j in tile conc do '//&
         '       k:=get_elem(a,j); '//&
         '       broadcast(&k,i)'//&
         '     endfor'//&
         '   else '//&
         '     for j in tile conc do '//&
         '       k:=arb(a);'//&
         '       broadcast(&k,i);'//&
         '       set_elem(a,k,j)'//&
         '     endfor'//&
         '   endif'//&
         ' endfor endproc',&
         line)
    
    call dcl_proc(parser,'broadcast(&b:any,long)',op_broadcast,&
         0_pm_i16,line,0,'',ftn_broadcast)
    call dcl_proc(parser,'broadcast(b:any,long)->=b',op_broadcast_val,&
         0_pm_i16,line,0,'',ftn_broadcast_val)
    call dcl_proc(parser,'get_remote%(a:any;b:long,c:long)->=a',op_get_remote,&
         0_pm_i16,line,0,'',ftn_get_remote)

!!$    ! Parallel aspects of for statements (just hooks here)
       call dcl_uproc(parser,'PM__make_mask()=true',line)
!!$    call dcl_uproc(parser,'prc_grid()=1..2',line)
!!$    call dcl_uproc(parser,'PM__pop_conc(x) do endproc',line)
!!$    call dcl_uproc(parser,'prc_for(d,x)=0',line)

    ! Communicating operators
    call dcl_uproc(parser,'local_tile%(x) local do result=x endproc',line)   
    call dcl_uproc(parser,'@::(this_distr:null,x) '//&
         'local do result=redim(x,this_dom) endproc',line)
    call dcl_uproc(parser,'@::(x) local do'//&
         ' y:=arb(x) dim this_dom; '//&
         ' for j,v in this_tile,x conc do set_elem(y,v,j) endfor;'//&
         ' if is_par() then assemble(&y,this_distr) endif; '//&
         ' result=y endproc ',line)

    ! Concurrent versions of binary "@"
    call dcl_uproc(parser,'@_[]%(this_distr:null,x;arg...:index)='//&
         'proc[](@x,arg...)',line)
    call dcl_uproc(parser,'@_{}%(this_distr:null,x;arg...:index)='//&
         'proc{}(@x,arg...)',line)
    call dcl_uproc(parser,'@[]%(this_distr:null,x;arg...:index)=y do'//&
         ' y:=null(x);j:=displace(this_dom,this_index,tuple(arg...));'//&
         ' xx:=@x;'//&
         ' if shape_contains(this_dom,j) then '//&
         '      y=opt(get_elem(xx,j)) '//&
         ' endif endproc',line)
    call dcl_uproc(parser,'@{}%(this_distr:null,x;arg...:index)=y do'//&
         ' y:=null(x);j:=displace_shape(this_dom,this_index,tuple(arg...));'//&
         ' xx:=@x;'//&
         ' if shape_contains(this_dom,j) then '//&
         '    y=opt(get_elem(xx,j)) '//&
         ' endif endproc',line)
    call dcl_uproc(parser,'@[]|%(this_distr:null,x;v,arg...:index)=y do'//&
         ' y:=v;j:=displace(this_dom,this_index,tuple(arg...));'//&
         ' xx:=@x; '//&
         ' if shape_contains(this_dom,j) then '//&
         '      y=get_elem(xx,j)'//&
         ' endif endproc',line)
    call dcl_uproc(parser,'@{}|%(this_distr:null,x;v,arg...:index)=y do'//&
         ' y:=v;j:=displace_shape(this_dom,this_index,tuple(arg...));'//&
         ' xx:=@x; '//&
         ' if shape_contains(this_dom,j) then '//&
         '   y=get_elem(@x,j) '//&
         ' endif endproc',line)
    
    call dcl_uproc(parser,&
         '@[]%(this_distr:null,x;arg...:slice)=v '//&
         ' do a:=get_disp(this_dom,this_index,arg...);'//&
         ' xx:=@x;v:=x dim a;'//&
         ' for each j in a do'//&
         '    v[j]=get_elem(xx,displace(this_dom,this_index,j))'//&
         ' endfor endproc ',line)
    call dcl_uproc(parser,&
         '@{}%(this_distr:null,x;arg...:slice)=v '//&
         'do a:=get_disp_shape(this_shape,this_index,arg...);'//&
         ' xx:=@x;v:=arb(x) dim a;'//&
         ' for each j in a do'//&
         '    v[j]=get_elem(xx,displace_shape(this_dom,this_index,j))'//&
         ' endfor endproc ',line)
    call dcl_uproc(parser,&
         '@[]%(this_distr:null,x;arg...:subs)'//&
         ' do a:=get_disp(this_dom,this_index,arg...);'//&
         ' xx:=@x;v:=x dim a;'//&
         ' for each j in a do'//&
         '    v[j]=get_elem(xx,displace(this_dom,this_index,j))'//&
         ' endfor; '//&
         ' result=redim(v,_shrink(dom(v),arg...)) endproc',line)
    call dcl_uproc(parser,&
         '@{}%(this_distr:null,x;arg...:subs)'//&
         'do a:=get_disp_shape(this_shape,this_index,arg...);'//&
         ' xx:=@x;v:=x dim a;'//&
         ' for each j in a do'//&
         '    v[j]=get_elem(xx,displace(this_dom,this_index,j))'//&
         ' endfor; '//&
         ' result=redim(v,_shrink(dom(v),arg...)) endproc',line)
   
    ! Parallel versions of binary "@" - scalar subscript
    call dcl_uproc(parser,'@_[]%(x;arg...:index)=get_remote%(x;p,i) '//&
         ' where p,i=prc_and_index(this_distr,'//&
         '  loc_in_shape(this_dom,tuple(arg...))) '//&
         ' do check "Index out of range":contains(this_dom,arg...) endproc',&
         line)
    call dcl_uproc(parser,'@_{}%(x;arg...:index)=get_remote%(x;p,i) '//&
         ' where p,i=prc_and_index(this_distr,'//&
         '  tuple(arg...)) '//&
         ' do check "Index out of range":'//&
         'shape_contains(this_dom,arg...) endproc',line)
    call dcl_uproc(parser,'@[]%(x;arg...:index) do'//&
         ' j:=displace(this_dom,this_index,tuple(arg...));'//&
         ' p:=this_prc();i:=0l;ok:=false;'//&
         ' if(shape_contains(this_dom,j)) then'//&
         '   p,i=prc_and_index(this_distr,'//&
         '     j);ok=true'//&
         ' endif; '//&
         ' result=opt(get_remote%(x;p,i),ok) '//&
         ' endproc',&
         line)    
    call dcl_uproc(parser,'@{}%(x;arg...:index) do'//&
         ' j:=displace_shape(this_dom,this_index,tuple(arg...));'//&
         ' p:=this_prc(); i:=0l;ok:=false; '//&
         ' if(shape_contains(this_dom,j)) then'//&
         '      p,i=prc_and_index(this_distr,j)'//&
         ' endif '//&
         ' result=opt(get_remote%(x;p,i),ok)'//&
         ' endproc',&
         line)
    call dcl_uproc(parser,'@[]|%(x;v,arg...:index) do'//&
         ' j:=displace(this_dom,this_index,tuple(arg...));'//&
         ' p:=sys_prc();i:=0l;ok:=false; '//&
         ' if(shape_contains(this_dom,j)) then'//&
         '   p,i=prc_and_index(this_distr,'//&
         '     j);ok=true'//&
         ' endif; '//&
         ' result=ok=>get_remote%(x;p,i)||v '//&
         ' endproc',&
         line)
    call dcl_uproc(parser,'@{}|%(x;v,arg...:index) do'//&
         ' j:=displace_shape(this_dom,this_index,tuple(arg...));'//&
         ' p:=sys_prc();i:=0l;ok:=false; '//&
         ' if(shape_contains(this_dom,j)) then'//&
         '   p,i=prc_and_index(this_distr,j);ok=true'//&
         ' endif; '//&
         ' result=ok=>get_remote%(x;p,i)||v '//&
         ' endproc',&
         line)
    
    ! Parallel versions of binary "@" - slice subscript
    call dcl_uproc(parser,'@[]%(x;arg...:slice)=v do '//&
         ' a:=get_disp(this_dom,this_index,arg...);'//&
         ' v:=x dim a;'//&
         ' for each j in a do'//&
         '   v[j]=@x{displace(this_dom,this_index,j)}'//&
         ' endfor '//&
         'endproc ',&
         line)
     call dcl_uproc(parser,'@{}%(x;arg...:slice)=v do '//&
         ' a:=get_disp_shape(this_dom,this_index,arg...); '//&
         ' v:=x dim a;'//&
         ' for each j in a do'//&
         '   v[j]=@x{displace_shape(this_dom,this_index,j)}'//&
         ' endfor '//&
         'endproc ',&
         line)
    call dcl_uproc(parser,'@[]%(x;arg...:subs) do '//&
         ' a:=get_disp(this_dom,this_index,arg...); '//&
         ' v:=x dim a;'//&
         ' for each j in a do'//&
         '   v[j]=@x{displace(this_dom,this_index,j)}'//&
         ' endfor; '//&
         ' result=redim(v,_shrink(a,arg...))'//&
         'endproc ',&
         line)
     call dcl_uproc(parser,'@{}%(x;arg...:subs) do '//&
         ' a:=get_disp_shape(this_dom,this_index,arg...); '//&
         ' v:=x dim a;'//&
         ' for each j in a do'//&
         '   v[j]=@x{displace_shape(this_dom,this_index,j)}'//&
         ' endfor; '//&
         ' result=redim(v,_shrink(a,arg...))'//&
         'endproc ',&
         line)
     
    ! Intrinsic reduction procedures
    call dcl_uproc(parser,'sum::(x) reduce(y)=x+y',line)
    call dcl_uproc(parser,'prod::(x) reduce(y)=x*y',line)
    call dcl_uproc(parser,'maxval::(x) reduce(y)=max(x,y)',line)
    call dcl_uproc(parser,'minval::(x) reduce(y)=min(x,y)',line)
    call dcl_uproc(parser,'allof::(x) reduce(y)=x and y',line)
    call dcl_uproc(parser,'anyof::(x) reduce(y)=x or y',line)
    call dcl_uproc(parser,'count::(x)=sum::z where z=(x=>1||0)',line)

    ! Array versions of intrinsic reductions
    call dcl_uproc(parser,'sum(x) '//&
         'do for i in x conc let y=sum::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'prod(x) '//&
         'do for i in x conc let y=prod::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'maxval(x) '//&
         'do for i in x conc let y=maxval::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'minval(x) '//&
         'do for i in x conc let y=minval::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'allof(x) '//&
         'do for i in x conc let y=allof::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'anyof(x) '//&
         'do for i in x conc let y=anyof::i endfor;result=y endproc',line)
    call dcl_uproc(parser,'count(x) '//&
         'do for i in x conc let y=count::i endfor;result=y endproc',line)

    
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
    call dcl_uproc(parser,'match(x:num,y:range{num})=x>=y._lo and x<=y._hi',&
         line)

    ! Conditional operator
    call dcl_uproc(parser,&
         '=>(x,y,z) check y?=z do r:=z; if x then r=y endif;result=r endproc',&
         line)
        
    ! Variables
    call dcl_uproc(parser,&
         'PM__assign(&a:any,b:any) do _assign(&a,b); '//&
         'check "Cannot assign values of different types":a?=b endproc',line)
    call dcl_uproc(parser,&
         'PM__assign_var(&a:any,b:any) do _assign(&a,b); '//&
         'check "Cannot assign values of different types":a?=b endproc',line)
    call dcl_proc(parser,'_assign(&any,any)',op_assign,0_pm_i16,line,0,&
         '',ftn_assign)
    call dcl_proc(parser,'PM__dup(x:any)->=x',op_clone,0_pm_i16,line,0,&
         '',ftn_dup)
    call dcl_uproc(parser,'PM__pdup(x)=x',line)
    call dcl_proc(parser,'PM__getref(x:any)->=x',op_get_rf,0_pm_i16,line,0,&
         '',ftn_getref)
    call dcl_proc(parser,'?=(x:any,y:any)->bool',&
         op_same_type,0_pm_i16,line,0,'',ftn_same_type)
    call dcl_proc(parser,'has_same_type(x:any,y:any)->==x,y',&
         op_has_same_type,0_pm_i16,line,0,'',ftn_has_same_type)
    call dcl_proc(parser,'==(any,any)->bool',op_eq,0_pm_i16,line,0,'',ftn_eq)
    call dcl_proc(parser,'/=(any,any)->bool',op_ne,0_pm_i16,line,0,'',ftn_ne)
    
  end subroutine sysdefs

  subroutine set_op_names
    op_names='??'
    op_names(op_call)='op_call'
    op_names(op_poly_call)='op_poly_call'
    op_names(op_jmp)='op_jmp'
    op_names(op_jmp_any_ve)='op_jmp_any_ve'
    op_names(op_jmp_empty_ve)='op_jmp_empty_ve'
    op_names(op_and_jmp_none)='op_and_jmp_none'
    op_names(op_andnot_jmp_none)='op_andnot_jmp_none'
    op_names(op_and_jmp_any)='op_and_jmp_any'
    op_names(op_andnot_jmp_any)='op_andnot_jmp_any'
    op_names(op_reduce)='op_reduce'
    op_names(op_jmp_nopar)='op_jmp_nopar'
    op_names(op_struct)='op_struct'
    op_names(op_rec)='op_rec'
    op_names(op_array)='op_array'
    op_names(op_any)='op_any'
    op_names(op_get_dom)='op_get_dom'
    op_names(op_elem)='op_elem'
    op_names(op_elem_ref)='op_elem_ref'
    op_names(op_poly_elem)='op_poly_elem'
    op_names(op_set_poly_elem)='op_set_poly_elem'
    op_names(op_set_poly_elem_idx)='op_set_poly_elem_idx'
    op_names(op_import)='op_import'
    op_names(op_export)='op_export'
    op_names(op_extract)='op_extract'
    op_names(op_return)='op_return'
    op_names(op_par_loop_end)='op_par_loop_end'
    op_names(op_par_find_end)='op_par_find_end'
    op_names(op_vcall)='op_vcall'
    op_names(op_check)='op_check'
    op_names(op_dump)='op_dump'
    op_names(op_print)='op_print'
    op_names(op_concat)='op_concat'
    op_names(op_lookup_error)='op_lookup_error'
    op_names(op_makekeys)='op_makekeys'
    op_names(op_delkeys)='op_delkeys'
    op_names(op_checkkeys)='op_checkkeys'
    op_names(op_getvkey)='op_getvkey'
    op_names(op_var_call)='op_var_call'
    op_names(op_make_dr)='op_make_dr'
    op_names(op_get_dr)='op_get_dr'
    op_names(op_set_dr)='op_set_dr'
    op_names(op_make_rf)='op_make_rf'
    op_names(op_get_rf)='op_get_rf'
    op_names(op_set_rf)='op_set_rf'
    op_names(op_check_logical)='op_check_logical'
    op_names(op_extract_first)='op_extract_first'
    
    op_names(op_array_get_elem)='op_array_get_elem'
    op_names(op_array_set_elem)='op_array_set_elem'
    op_names(op_array_elems)='op_array_elems'
    op_names(op_make_array)='op_make_array'
    op_names(op_import_val)='op_import_val'
    op_names(op_import_varg)='op_import_varg'
    op_names(op_extractelm)='op_extractelm'
    op_names(op_iota)='op_iota'
    op_names(op_get_key)='op_get_key'
    op_names(op_get_key2)='op_get_key2'
    op_names(op_export_array)='op_export_array'
    op_names(op_miss_arg)='op_miss_arg'
    op_names(op_default)='op_default'
    op_names(op_dump_id)='op_dump_id'
    op_names(op_import_scalar)='op_import_scalar'
    op_names(op_check_assign)='op_check_assign'
    op_names(op_same_type)='op_same_type'
    op_names(op_has_same_type)='op_has_same_type'

    op_names(op_this_prc)='op_this_prc'
    op_names(op_this_nprc)='op_this_nprc'
    op_names(op_shared_prc)='op_shared_prc'
    op_names(op_shared_nprc)='op_shared_nprc'
    op_names(op_is_shared)='op_is_shared'
    op_names(op_is_par)='op_is_par'
    op_names(op_push_prc_grid)='op_push_prc_grid'
    op_names(op_push_prc_split)='op_push_prc_split'
    op_names(op_pop_prc_conc)='op_pop_prc_conc'
    op_names(op_pop_prc)='op_pop_prc'
    op_names(op_broadcast)='op_broadcast'
    op_names(op_get_remote)='op_get_remote'
    op_names(op_stencil)='op_stencil'
    op_names(op_prc_test_push)='op_prc_test_push'
    op_names(op_this_prc)='op_sys_prc'
    op_names(op_this_nprc)='op_sys_nprc'
    op_names(op_reduce_ve)='op_reduce_ve'
    op_names(op_start_loop_sync)='op_start_loop_sync'
    op_names(op_gather)='op_gather'
      
    op_names(op_par_loop)='op_par_loop'
    op_names(op_par_find)='op_par_find'
    op_names(op_setref)='op_setref'
    op_names(op_clone)='op_clone'
    op_names(op_clone_ve)='op_clone_ve'
    op_names(op_nullify_ve)='op_nullify_ve'
    op_names(op_assign)='op_assign'

    op_names(op_eq)='op_eq'
    op_names(op_ne)='op_ne'

    op_names(op_string_l)='op_string_l'
    op_names(op_and)='op_and'
    op_names(op_or)='op_or'
    op_names(op_not)='op_not'
    op_names(op_assign_l)='op_assign_l'

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
    op_names(op_max_i)='op_max_i'
    op_names(op_min_i)='op_min_i'
    op_names(op_assign_i)='op_assign_i'
    op_names(op_long_i)='op_long_i'
    op_names(op_real_i)='op_real_i'
    op_names(op_double_i)='op_double_i'

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
    op_names(op_max_ln)='op_max_ln'
    op_names(op_min_ln)='op_min_ln'
    op_names(op_assign_ln)='op_assign_ln'
    op_names(op_int_ln)='op_int_ln'
    op_names(op_real_ln)='op_real_ln'
    op_names(op_double_ln)='op_double_ln'
   
    op_names(op_add_r)='op_add_r'
    op_names(op_sub_r)='op_sub_r'
    op_names(op_mult_r)='op_mult_r'
    op_names(op_divide_r)='op_divide_r'
    op_names(op_div_r)='op_div_r'
    op_names(op_mod_r)='op_mod_r'
    op_names(op_pow_r)='op_pow_r'
    op_names(op_uminus_r)='op_uminus_r'
    op_names(op_eq_r)='op_eq_r'
    op_names(op_ne_r)='op_ne_r'
    op_names(op_gt_r)='op_gt_r'
    op_names(op_ge_r)='op_ge_r'
    op_names(op_string_r)='op_string_r'
    op_names(op_get_elt_r)='op_get_elt_r'
    op_names(op_set_elt_r)='op_set_elt_r'
    op_names(op_max_r)='op_max_r'
    op_names(op_min_r)='op_min_r'
    op_names(op_assign_r)='op_assign_r'
    op_names(op_int_r)='op_int_r'
    op_names(op_long_r)='op_long_r'
    op_names(op_double_r)='op_double_r'
    
    op_names(op_add_d)='op_add_d'
    op_names(op_sub_d)='op_sub_d'
    op_names(op_mult_d)='op_mult_d'
    op_names(op_divide_d)='op_divide_d'
    op_names(op_div_d)='op_div_d'
    op_names(op_mod_d)='op_mod_d'
    op_names(op_pow_d)='op_pow_d'
    op_names(op_uminus_d)='op_uminus_d'
    op_names(op_eq_d)='op_eq_d'
    op_names(op_ne_d)='op_ne_d'
    op_names(op_gt_d)='op_gt_d'
    op_names(op_ge_d)='op_ge_d'
    op_names(op_string_d)='op_string_d'
    op_names(op_get_elt_d)='op_get_elt_d'
    op_names(op_set_elt_d)='op_set_elt_d'
    op_names(op_max_d)='op_max_d'
    op_names(op_min_d)='op_min_d'
    op_names(op_assign_d)='op_assign_d'
    op_names(op_int_d)='op_int_d'
    op_names(op_long_d)='op_long_d'
    op_names(op_real_d)='op_real_d'

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
