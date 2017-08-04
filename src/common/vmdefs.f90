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

module pm_vmdefs
  use pm_kinds
  use pm_memory
  use pm_parser
  use pm_types
  implicit none

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
  integer(pm_i16),parameter:: op_jmp_noshare=12

  integer(pm_i16),parameter:: last_jmp_op=12

  integer(pm_i16),parameter:: index_op = last_jmp_op
  integer(pm_i16),parameter:: op_struct = index_op +1
  integer(pm_i16),parameter:: op_rec = index_op +2
  integer(pm_i16),parameter:: op_array= index_op + 3
  integer(pm_i16),parameter:: op_get_dom = index_op + 6
  integer(pm_i16),parameter:: op_elem= index_op + 7
  integer(pm_i16),parameter:: op_elem_ref = index_op + 8
  integer(pm_i16),parameter:: op_dash = index_op + 9
  integer(pm_i16),parameter:: op_make_poly = index_op+10
  integer(pm_i16),parameter:: op_get_poly = index_op+11
  integer(pm_i16),parameter:: op_get_poly2 = index_op+12
  integer(pm_i16),parameter:: op_get_poly_or = index_op+13
  integer(pm_i16),parameter:: op_any = index_op + 14

  integer(pm_i16),parameter:: op_misc = op_any
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
  integer(pm_i16),parameter:: op_indices = op_misc + 50
  integer(pm_i16),parameter:: op_get_key = op_misc + 51
  integer(pm_i16),parameter:: op_get_key2 = op_misc + 52
  integer(pm_i16),parameter:: op_export_array = op_misc + 53
  integer(pm_i16),parameter:: op_miss_arg = op_misc + 54
  integer(pm_i16),parameter:: op_default = op_misc + 55
  integer(pm_i16),parameter:: op_dump_id = op_misc + 56
  integer(pm_i16),parameter:: op_import_scalar = op_misc + 57
  integer(pm_i16),parameter:: op_check_assign = op_misc + 58
  integer(pm_i16),parameter:: op_same_type = op_misc + 59
  integer(pm_i16),parameter:: op_has_same_type = op_misc + 60
  integer(pm_i16),parameter:: op_redim = op_misc + 61
  integer(pm_i16),parameter:: op_make_mask = op_misc + 62
  integer(pm_i16),parameter:: op_wshare = op_misc + 63
  integer(pm_i16),parameter:: op_pack = op_misc + 64

  integer(pm_i16),parameter:: op_comm = op_pack + 1

  integer(pm_i16),parameter:: op_this_prc = op_comm + 0
  integer(pm_i16),parameter:: op_this_nprc = op_comm + 1
  integer(pm_i16),parameter:: op_shared_prc = op_comm + 2
  integer(pm_i16),parameter:: op_shared_nprc = op_comm + 3
  integer(pm_i16),parameter:: op_is_shared = op_comm + 4
  integer(pm_i16),parameter:: op_is_par = op_comm + 5
  integer(pm_i16),parameter:: op_push_prc_grid = op_comm + 6
  integer(pm_i16),parameter:: op_push_prc_split = op_comm + 7
  integer(pm_i16),parameter:: op_pop_prc_conc = op_comm + 8
  integer(pm_i16),parameter:: op_push_prc_conc = op_comm + 9
  integer(pm_i16),parameter:: op_push_prc_distr= op_comm + 10
  integer(pm_i16),parameter:: op_pop_prc    = op_comm + 11
  integer(pm_i16),parameter:: op_broadcast = op_comm +  12
  integer(pm_i16),parameter:: op_get_remote = op_comm + 13
  integer(pm_i16),parameter:: op_stencil = op_comm + 14
  integer(pm_i16),parameter:: op_prc_test_push = op_comm + 15
  integer(pm_i16),parameter:: op_sys_prc = op_comm + 16
  integer(pm_i16),parameter:: op_sys_nprc = op_comm + 17
  integer(pm_i16),parameter:: op_reduce_ve = op_comm + 18
  integer(pm_i16),parameter:: op_start_loop_sync = op_comm + 19
  integer(pm_i16),parameter:: op_broadcast_val = op_comm + 20
  integer(pm_i16),parameter:: op_gather = op_comm + 21
  integer(pm_i16),parameter:: op_isend = op_comm + 22
  integer(pm_i16),parameter:: op_irecv = op_comm + 23
  integer(pm_i16),parameter:: op_sync_mess = op_comm + 24
  integer(pm_i16),parameter:: op_get_dims = op_comm + 25
  integer(pm_i16),parameter:: op_put_remote = op_comm + 26
  integer(pm_i16),parameter:: op_get_remote_distr = op_comm + 27
  integer(pm_i16),parameter:: op_put_remote_distr = op_comm + 28
  
  integer(pm_i16),parameter:: first_assign_op= op_put_remote_distr
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
  integer(pm_i16),parameter:: op_abs_i = op_start_i+22
  integer(pm_i16),parameter:: op_band_i = op_start_i+23
  integer(pm_i16),parameter:: op_bor_i = op_start_i+24
  integer(pm_i16),parameter:: op_bxor_i = op_start_i+25
  integer(pm_i16),parameter:: op_bshift_i = op_start_i+26
  integer(pm_i16),parameter:: op_bnot_i = op_start_i+27
  
  integer(pm_i16),parameter:: op_stop_i = op_start_i+27

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
  integer(pm_i16),parameter:: op_abs_ln = op_start_ln+22
  integer(pm_i16),parameter:: op_band_ln = op_start_ln+23
  integer(pm_i16),parameter:: op_bor_ln = op_start_ln+24
  integer(pm_i16),parameter:: op_bxor_ln = op_start_ln+25
  integer(pm_i16),parameter:: op_bshift_ln = op_start_ln+26
  integer(pm_i16),parameter:: op_bnot_ln = op_start_ln+27
  integer(pm_i16),parameter:: op_stop_ln = op_start_ln+27

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
  integer(pm_i16),parameter:: op_abs_r = op_start_r+22
  integer(pm_i16),parameter:: op_acos_r = op_start_r+23
  integer(pm_i16),parameter:: op_asin_r = op_start_r+24
  integer(pm_i16),parameter:: op_atan_r = op_start_r+25
  integer(pm_i16),parameter:: op_atan2_r = op_start_r+26
  integer(pm_i16),parameter:: op_cos_r = op_start_r+27
  integer(pm_i16),parameter:: op_cosh_r = op_start_r+28
  integer(pm_i16),parameter:: op_exp_r = op_start_r+29
  integer(pm_i16),parameter:: op_log_r = op_start_r+30
  integer(pm_i16),parameter:: op_log10_r = op_start_r+31
  integer(pm_i16),parameter:: op_sin_r = op_start_r+32
  integer(pm_i16),parameter:: op_sinh_r = op_start_r+33
  integer(pm_i16),parameter:: op_sqrt_r = op_start_r+34
  integer(pm_i16),parameter:: op_tan_r = op_start_r+35
  integer(pm_i16),parameter:: op_tanh_r = op_start_r+36
  integer(pm_i16),parameter:: op_floor_r = op_start_r+37
  integer(pm_i16),parameter:: op_ceil_r = op_start_r+38
  integer(pm_i16),parameter:: op_stop_r = op_start_r+38

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
  integer(pm_i16),parameter:: op_abs_d = op_start_d+22
  integer(pm_i16),parameter:: op_acos_d = op_start_d+23
  integer(pm_i16),parameter:: op_asin_d = op_start_d+24
  integer(pm_i16),parameter:: op_atan_d = op_start_d+25
  integer(pm_i16),parameter:: op_atan2_d = op_start_d+26
  integer(pm_i16),parameter:: op_cos_d = op_start_d+27
  integer(pm_i16),parameter:: op_cosh_d = op_start_d+28
  integer(pm_i16),parameter:: op_exp_d = op_start_d+29
  integer(pm_i16),parameter:: op_log_d = op_start_d+30
  integer(pm_i16),parameter:: op_log10_d = op_start_d+31
  integer(pm_i16),parameter:: op_sin_d = op_start_d+32
  integer(pm_i16),parameter:: op_sinh_d = op_start_d+33
  integer(pm_i16),parameter:: op_sqrt_d = op_start_d+34
  integer(pm_i16),parameter:: op_tan_d = op_start_d+35
  integer(pm_i16),parameter:: op_tanh_d = op_start_d+36
  integer(pm_i16),parameter:: op_floor_d = op_start_d+37
  integer(pm_i16),parameter:: op_ceil_d = op_start_d+38
  integer(pm_i16),parameter:: op_stop_d = op_start_d+38
  
  integer,parameter:: num_op=op_stop_d
  
  character(len=20),dimension(0:num_op):: op_names
 
contains

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
    op_names(op_jmp_noshare)='op_jmp_noshare'
    op_names(op_struct)='op_struct'
    op_names(op_rec)='op_rec'
    op_names(op_array)='op_array'
    op_names(op_get_dom)='op_get_dom'
    op_names(op_elem)='op_elem'
    op_names(op_elem_ref)='op_elem_ref'
    op_names(op_dash)='op_dash'
    op_names(op_make_poly)='op_make_poly'
    op_names(op_get_poly)='op_get_poly'
    op_names(op_get_poly2)='op_get_poly2'
    op_names(op_get_poly_or)='op_get_poly_or'
    op_names(op_any)='op_any'
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
    op_names(op_indices)='op_indices'
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
    op_names(op_redim)='op_redim'
    op_names(op_make_mask)='op_make_mask'
    op_names(op_wshare)='op_wshare'
    op_names(op_pack)='op_pack'

    op_names(op_this_prc)='op_this_prc'
    op_names(op_this_nprc)='op_this_nprc'
    op_names(op_shared_prc)='op_shared_prc'
    op_names(op_shared_nprc)='op_shared_nprc'
    op_names(op_is_shared)='op_is_shared'
    op_names(op_is_par)='op_is_par'
    op_names(op_push_prc_grid)='op_push_prc_grid'
    op_names(op_push_prc_split)='op_push_prc_split'
    op_names(op_push_prc_distr)='op_push_prc_distr'
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
    op_names(op_isend)='op_isend'
    op_names(op_irecv)='op_irecv'
    op_names(op_sync_mess)='op_sync_mess'
    op_names(op_get_dims)='op_get_dims'
    op_names(op_put_remote)='op_put_remote'
    op_names(op_get_remote_distr)='op_get_remote_distr'
    op_names(op_put_remote_distr)='op_put_remote_distr'
      
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
  
end module pm_vmdefs
