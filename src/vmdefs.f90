!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2020
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
  use pm_types
  implicit none

  integer,parameter:: pm_max_stack=2**14-1

  integer,parameter:: pm_ext_mult=2**15/(pm_max_stack+1)
  integer,parameter:: pm_jump_offset=pm_ext_mult*2**14

  integer,parameter:: op_call=0
  integer,parameter:: op_comm_call=1
  
  integer,parameter:: first_jmp_op=2
  integer,parameter:: op_jmp=2
  integer,parameter:: op_jmp_any_ve=3
  integer,parameter:: op_jmp_any_ve_par=4
  integer,parameter:: op_and_ve=5
  integer,parameter:: op_andnot_ve=6
  integer,parameter:: op_and_jmp_any=7
  integer,parameter:: op_andnot_jmp_any=8
  integer,parameter:: op_reduce=9
  integer,parameter:: op_jmp_nopar=10
  integer,parameter:: op_jmp_noshare=11
  integer,parameter:: op_skip_empty=12
  integer,parameter:: op_skip_empty_local=13
  integer,parameter:: op_head_node=14
  integer,parameter:: op_skip_comms=15

  integer,parameter:: last_jmp_op=15

  integer,parameter:: index_op = last_jmp_op
  integer,parameter:: op_struct = index_op +1
  integer,parameter:: op_rec = index_op +2
  integer,parameter:: op_array= index_op + 3
  integer,parameter:: op_var_array = index_op + 4
  integer,parameter:: op_array_noinit = index_op + 5
  integer,parameter:: op_get_dom = index_op + 6
  integer,parameter:: op_elem= index_op + 7
  integer,parameter:: op_elem_ref = index_op + 8
  integer,parameter:: op_as = index_op + 9
  integer,parameter:: op_make_poly = index_op+10
  integer,parameter:: op_get_poly = index_op+11
  integer,parameter:: op_get_poly2 = index_op+12
  integer,parameter:: op_get_poly_or = index_op+13
  integer,parameter:: op_any = index_op + 14
  integer,parameter:: op_tag = index_op + 15
  integer,parameter:: op_tag_val = index_op + 16
  integer,parameter:: op_get_tag = index_op + 17
  integer,parameter:: op_get_size = index_op + 18
  integer,parameter:: op_make_type_val = index_op + 19

  integer,parameter:: op_misc = op_make_type_val
  integer,parameter:: op_import= op_misc + 1
  integer,parameter:: op_export= op_misc + 2
  integer,parameter:: op_export_param = op_misc +3
  integer,parameter:: op_extract = op_misc + 4
  integer,parameter:: op_return= op_misc + 5
  integer,parameter:: op_par_loop_end= op_misc + 6
  integer,parameter:: op_par_find_end= op_misc + 7
  integer,parameter:: op_check= op_misc + 8
  integer,parameter:: op_dump= op_misc + 9
  integer,parameter:: op_print= op_misc + 10
  integer,parameter:: op_concat= op_misc + 11
  integer,parameter:: op_lookup_error = op_misc + 12
  integer,parameter:: op_makekeys = op_misc + 13
  integer,parameter:: op_delkeys = op_misc + 14
  integer,parameter:: op_checkkeys = op_misc + 15
  integer,parameter:: op_getvkey = op_misc + 16
  integer,parameter:: op_var_call = op_misc + 17
  integer,parameter:: op_make_dr = op_misc + 18
  integer,parameter:: op_get_dr = op_misc + 19
  integer,parameter:: op_set_dr = op_misc + 20
  integer,parameter:: op_make_rf = op_misc + 21
  integer,parameter:: op_get_rf = op_misc + 22
  integer,parameter:: op_set_rf = op_misc + 23
  integer,parameter:: op_extract_first = op_misc + 24
  integer,parameter:: op_check_logical = op_misc + 25
  integer,parameter:: op_chan_array_elem = op_misc + 26
  integer,parameter:: op_chan_array_vect = op_misc + 27

  integer,parameter:: op_misc2=op_chan_array_vect
  
  integer,parameter:: op_array_get_elem = op_misc2 + 1
  integer,parameter:: op_array_set_elem = op_misc2 + 2
  integer,parameter:: op_array_elems = op_misc2 + 3
  integer,parameter:: op_make_array = op_misc2 + 4
  integer,parameter:: op_import_val = op_misc2 + 5
  integer,parameter:: op_import_back=  op_misc2 + 6
  integer,parameter:: op_import_varg = op_misc2 + 7
  integer,parameter:: op_extractelm = op_misc2 + 8
  integer,parameter:: op_iota = op_misc2 + 9
  integer,parameter:: op_indices = op_misc2 + 10
  integer,parameter:: op_get_key = op_misc2 + 11
  integer,parameter:: op_get_key2 = op_misc2 + 12
  integer,parameter:: op_export_array = op_misc2 + 13
  integer,parameter:: op_miss_arg = op_misc2 + 14
  integer,parameter:: op_default = op_misc2 + 15
  integer,parameter:: op_dump_id = op_misc2 + 16
  integer,parameter:: op_import_scalar = op_misc2 + 17
  integer,parameter:: op_check_assign = op_misc2 + 18
  integer,parameter:: op_same_type = op_misc2 + 19
  integer,parameter:: op_logical_return = op_misc2 + 20
  integer,parameter:: op_redim = op_misc2 + 21
  integer,parameter:: op_wshare = op_misc2 + 22
  integer,parameter:: op_block_cyclic = op_misc2 + 23

  integer,parameter:: op_misc3 = op_block_cyclic
  
  integer,parameter:: op_get_esize = op_misc3 + 1
  integer,parameter:: op_pack = op_misc3 + 2
  integer,parameter:: op_dref = op_misc3 + 3
  integer,parameter:: op_dref_elem = op_misc3 + 4
  integer,parameter:: op_import_dref = op_misc3 + 5
  integer,parameter:: op_advance = op_misc3 + 6
  integer,parameter:: op_advance_and = op_misc3 + 7
  integer,parameter:: op_init_loop= op_misc3 + 8
  integer,parameter:: op_intersect_seq = op_misc3 + 9
  integer,parameter:: op_intersect_aseq = op_misc3 + 10
  integer,parameter:: op_expand_aseq = op_misc3 + 11
  integer,parameter:: op_intersect_bseq = op_misc3 + 12
  integer,parameter:: op_gcd = op_misc3 + 13
  integer,parameter:: op_chan = op_misc3 + 14
  integer,parameter:: op_active = op_misc3 + 15
  integer,parameter:: op_new_dump = op_misc3 + 16
  integer,parameter:: op_show = op_misc3 + 17
  integer,parameter:: op_show_stack = op_misc3 + 18

  integer,parameter:: op_comm = op_show_stack + 1

  integer,parameter:: op_this_node = op_comm + 0
  integer,parameter:: op_this_nnode = op_comm + 1
  integer,parameter:: op_shared_node = op_comm + 2
  integer,parameter:: op_shared_nnode = op_comm + 3
  integer,parameter:: op_is_shared = op_comm + 4
  integer,parameter:: op_is_par = op_comm + 5
  integer,parameter:: op_push_node_grid = op_comm + 6
  integer,parameter:: op_push_node_split = op_comm + 7
  integer,parameter:: op_pop_node_conc = op_comm + 8
  integer,parameter:: op_push_node_conc = op_comm + 9
  integer,parameter:: op_push_node_distr= op_comm + 10
  integer,parameter:: op_pop_node    = op_comm + 11
  integer,parameter:: op_broadcast = op_comm +  12
  integer,parameter:: op_get_remote = op_comm + 13
  integer,parameter:: op_pop_off_node = op_comm + 14
  integer,parameter:: op_push_node_back = op_comm + 15
  integer,parameter:: op_sys_node = op_comm + 16
  integer,parameter:: op_sys_nnode = op_comm + 17
  integer,parameter:: op_reduce_ve = op_comm + 18
  
  integer,parameter:: op_start_loop_sync = op_comm + 19
  integer,parameter:: op_broadcast_val = op_comm + 20
  integer,parameter:: op_gather = op_comm + 21
  integer,parameter:: op_isend_offset = op_comm + 22
  integer,parameter:: op_irecv_offset = op_comm + 23
  integer,parameter:: op_sync_mess = op_comm + 24
  integer,parameter:: op_get_dims = op_comm + 25
  integer,parameter:: op_put_remote = op_comm + 26
  integer,parameter:: op_get_remote_distr = op_comm + 27
  integer,parameter:: op_put_remote_distr = op_comm + 28
  integer,parameter:: op_remote_call = op_comm + 29
  integer,parameter:: op_remote_send_call = op_comm + 30
  integer,parameter:: op_collect_call = op_comm + 31
  integer,parameter:: op_server_call = op_comm + 32
  integer,parameter:: op_bcast_call = op_comm + 33
  integer,parameter:: op_recv_req_call = op_comm + 34
  integer,parameter:: op_recv_assn_call = op_comm + 35
  integer,parameter:: op_isend_req = op_comm + 36
  integer,parameter:: op_recv_reply = op_comm + 37
  integer,parameter:: op_recv_offset = op_comm + 38
  integer,parameter:: op_isend_assn = op_comm + 39
  integer,parameter:: op_isend = op_comm + 40
  integer,parameter:: op_irecv = op_comm + 41
  integer,parameter:: op_recv = op_comm + 42
  integer,parameter:: op_isend_reply = op_comm + 43
  integer,parameter:: op_do_at = op_comm + 44
  integer,parameter:: op_root_node = op_comm + 45
  integer,parameter:: op_broadcast_disp = op_comm + 46
  integer,parameter:: op_broadcast_shared = op_comm + 47
  integer,parameter:: op_isend_grid = op_comm + 48
  integer,parameter:: op_irecv_grid = op_comm + 49
  integer,parameter:: op_recv_grid = op_comm + 50
  integer,parameter:: op_recv_grid_resend = op_comm + 51
  integer,parameter:: op_recv_offset_resend = op_comm + 52

  integer,parameter:: first_file_op = op_recv_offset_resend
  integer,parameter:: op_open_file=first_file_op+1
  integer,parameter:: op_close_file=first_file_op+2
  integer,parameter:: op_seek_file=first_file_op+3
  integer,parameter:: op_read_file=first_file_op+4
  integer,parameter:: op_write_file=first_file_op+5
  integer,parameter:: op_read_file_array=first_file_op+6
  integer,parameter:: op_write_file_array=first_file_op+7
  integer,parameter:: op_read_file_tile=first_file_op+8
  integer,parameter:: op_write_file_tile=first_file_op+9
  integer,parameter:: op_io_error_string=first_file_op+10
  integer,parameter:: op_last_file_op=first_file_op+10
  
  integer,parameter:: first_assign_op= op_last_file_op
  integer,parameter:: op_par_loop= first_assign_op +1
  integer,parameter:: op_par_find = first_assign_op + 2
  integer,parameter:: op_setref= first_assign_op + 3
  integer,parameter:: op_clone= first_assign_op + 4
  integer,parameter:: op_clone_ve = first_assign_op + 5
  integer,parameter:: op_nullify = first_assign_op + 6
  integer,parameter:: op_assign = first_assign_op + 7
  integer,parameter:: op_fill = first_assign_op + 8
  integer,parameter:: last_assign_op = first_assign_op+8

  integer,parameter:: op_eq = last_assign_op +1
  integer,parameter:: op_ne = last_assign_op +2
  
  integer,parameter:: op_string_l = last_assign_op +3
  integer,parameter:: op_and = last_assign_op + 4
  integer,parameter:: op_or = last_assign_op + 5
  integer,parameter:: op_not = last_assign_op + 6
  integer,parameter:: op_assign_l = last_assign_op +7
  integer,parameter:: op_eq_l = last_assign_op + 8
  integer,parameter:: op_ne_l = last_assign_op + 9

  integer,parameter:: op_start_i=op_ne_l
  integer,parameter:: op_add_i=op_start_i+1
  integer,parameter:: op_sub_i=op_start_i+2
  integer,parameter:: op_mult_i=op_start_i+3
  integer,parameter:: op_divide_i=op_start_i+4
  integer,parameter:: op_div_i=op_start_i+5
  integer,parameter:: op_mod_i=op_start_i+6
  integer,parameter:: op_pow_i=op_start_i+7
  integer,parameter:: op_uminus_i=op_start_i+8
  integer,parameter:: op_eq_i=op_start_i+9
  integer,parameter:: op_ne_i=op_start_i+10
  integer,parameter:: op_gt_i=op_start_i+11
  integer,parameter:: op_ge_i=op_start_i+12
  integer,parameter:: op_string_i=op_start_i+13
  integer,parameter:: op_max_i = op_start_i+16
  integer,parameter:: op_min_i = op_start_i+17
  integer,parameter:: op_assign_i = op_start_i + 18
  integer,parameter:: op_long_i = op_start_i+19
  integer,parameter:: op_real_i = op_start_i+20
  integer,parameter:: op_double_i = op_start_i+21
  integer,parameter:: op_abs_i = op_start_i+22
  integer,parameter:: op_band_i = op_start_i+23
  integer,parameter:: op_bor_i = op_start_i+24
  integer,parameter:: op_bxor_i = op_start_i+25
  integer,parameter:: op_bshift_i = op_start_i+26
  integer,parameter:: op_bnot_i = op_start_i+27
  integer,parameter:: op_pdiff_i = op_start_i+28
  integer,parameter:: op_sign_i = op_start_i+29
  integer,parameter:: op_modulo_i = op_start_i+30
  integer,parameter:: op_i8_i = op_start_i+31
  integer,parameter:: op_i16_i = op_start_i+32
  integer,parameter:: op_i32_i = op_start_i+33
  integer,parameter:: op_i64_i = op_start_i+34
  integer,parameter:: op_offset_i = op_start_i+35
  integer,parameter:: op_stop_i = op_start_i+35

  integer,parameter:: op_start_ln=op_stop_i
  integer,parameter:: op_add_ln=op_start_ln+1
  integer,parameter:: op_sub_ln=op_start_ln+2
  integer,parameter:: op_mult_ln=op_start_ln+3
  integer,parameter:: op_divide_ln=op_start_ln+4
  integer,parameter:: op_div_ln=op_start_ln+5
  integer,parameter:: op_mod_ln=op_start_ln+6
  integer,parameter:: op_pow_ln=op_start_ln+7
  integer,parameter:: op_uminus_ln=op_start_ln+8
  integer,parameter:: op_eq_ln=op_start_ln+9
  integer,parameter:: op_ne_ln=op_start_ln+10
  integer,parameter:: op_gt_ln=op_start_ln+11
  integer,parameter:: op_ge_ln=op_start_ln+12
  integer,parameter:: op_string_ln=op_start_ln+13
  integer,parameter:: op_max_ln = op_start_ln+16
  integer,parameter:: op_min_ln = op_start_ln+17
  integer,parameter:: op_assign_ln = op_start_ln + 18
  integer,parameter:: op_int_ln = op_start_ln+19
  integer,parameter:: op_real_ln = op_start_ln+20
  integer,parameter:: op_double_ln = op_start_ln+21
  integer,parameter:: op_abs_ln = op_start_ln+22
  integer,parameter:: op_band_ln = op_start_ln+23
  integer,parameter:: op_bor_ln = op_start_ln+24
  integer,parameter:: op_bxor_ln = op_start_ln+25
  integer,parameter:: op_bshift_ln = op_start_ln+26
  integer,parameter:: op_bnot_ln = op_start_ln+27
  integer,parameter:: op_pdiff_ln = op_start_ln+28
  integer,parameter:: op_sign_ln = op_start_ln+29
  integer,parameter:: op_modulo_ln = op_start_ln+30
  integer,parameter:: op_i8_ln = op_start_ln+31
  integer,parameter:: op_i16_ln = op_start_ln+32
  integer,parameter:: op_i32_ln = op_start_ln+33
  integer,parameter:: op_i64_ln = op_start_ln+34
  integer,parameter:: op_offset_ln = op_start_ln + 35
  integer,parameter:: op_stop_ln = op_start_ln+35

  integer,parameter:: op_start_offset=op_stop_ln
  integer,parameter:: op_add_offset=op_start_offset+1
  integer,parameter:: op_sub_offset=op_start_offset+2
  integer,parameter:: op_mult_offset=op_start_offset+3
  integer,parameter:: op_divide_offset=op_start_offset+4
  integer,parameter:: op_div_offset=op_start_offset+5
  integer,parameter:: op_mod_offset=op_start_offset+6
  integer,parameter:: op_pow_offset=op_start_offset+7
  integer,parameter:: op_uminus_offset=op_start_offset+8
  integer,parameter:: op_eq_offset=op_start_offset+9
  integer,parameter:: op_ne_offset=op_start_offset+10
  integer,parameter:: op_gt_offset=op_start_offset+11
  integer,parameter:: op_ge_offset=op_start_offset+12
  integer,parameter:: op_string_offset=op_start_offset+13
  integer,parameter:: op_max_offset = op_start_offset+16
  integer,parameter:: op_min_offset = op_start_offset+17
  integer,parameter:: op_assign_offset = op_start_offset + 18
  integer,parameter:: op_int_offset = op_start_offset+19
  integer,parameter:: op_real_offset = op_start_offset+20
  integer,parameter:: op_double_offset = op_start_offset+21
  integer,parameter:: op_abs_offset = op_start_offset+22
  integer,parameter:: op_band_offset = op_start_offset+23
  integer,parameter:: op_bor_offset = op_start_offset+24
  integer,parameter:: op_bxor_offset = op_start_offset+25
  integer,parameter:: op_bshift_offset = op_start_offset+26
  integer,parameter:: op_bnot_offset = op_start_offset+27
  integer,parameter:: op_pdiff_offset = op_start_offset+28
  integer,parameter:: op_sign_offset = op_start_offset+29
  integer,parameter:: op_modulo_offset = op_start_offset+30
  integer,parameter:: op_i8_offset = op_start_offset+31
  integer,parameter:: op_i16_offset = op_start_offset+32
  integer,parameter:: op_i32_offset = op_start_offset+33
  integer,parameter:: op_i64_offset = op_start_offset+34
  integer,parameter:: op_long_offset = op_start_offset+35
  integer,parameter:: op_stop_offset = op_start_offset+35

  integer,parameter:: op_start_i8=op_stop_offset
  integer,parameter:: op_add_i8=op_start_i8+1
  integer,parameter:: op_sub_i8=op_start_i8+2
  integer,parameter:: op_mult_i8=op_start_i8+3
  integer,parameter:: op_divide_i8=op_start_i8+4
  integer,parameter:: op_div_i8=op_start_i8+5
  integer,parameter:: op_mod_i8=op_start_i8+6
  integer,parameter:: op_pow_i8=op_start_i8+7
  integer,parameter:: op_uminus_i8=op_start_i8+8
  integer,parameter:: op_eq_i8=op_start_i8+9
  integer,parameter:: op_ne_i8=op_start_i8+10
  integer,parameter:: op_gt_i8=op_start_i8+11
  integer,parameter:: op_ge_i8=op_start_i8+12
  integer,parameter:: op_string_i8=op_start_i8+13
  integer,parameter:: op_max_i8 = op_start_i8+16
  integer,parameter:: op_min_i8 = op_start_i8+17
  integer,parameter:: op_assign_i8 = op_start_i8 + 18
  integer,parameter:: op_int_i8 = op_start_i8+19
  integer,parameter:: op_real_i8 = op_start_i8+20
  integer,parameter:: op_double_i8 = op_start_i8+21
  integer,parameter:: op_abs_i8 = op_start_i8+22
  integer,parameter:: op_band_i8 = op_start_i8+23
  integer,parameter:: op_bor_i8 = op_start_i8+24
  integer,parameter:: op_bxor_i8 = op_start_i8+25
  integer,parameter:: op_bshift_i8 = op_start_i8+26
  integer,parameter:: op_bnot_i8 = op_start_i8+27
  integer,parameter:: op_pdiff_i8 = op_start_i8+28
  integer,parameter:: op_sign_i8 = op_start_i8+29
  integer,parameter:: op_modulo_i8 = op_start_i8+30
  integer,parameter:: op_i16_i8 = op_start_i8+31
  integer,parameter:: op_i32_i8 = op_start_i8+32
  integer,parameter:: op_i64_i8 = op_start_i8+33
  integer,parameter:: op_long_i8 = op_start_i8+34
  integer,parameter:: op_offset_i8 = op_start_i8+35
  integer,parameter:: op_stop_i8 = op_start_i8+35

  integer,parameter:: op_start_i16=op_stop_i8
  integer,parameter:: op_add_i16=op_start_i16+1
  integer,parameter:: op_sub_i16=op_start_i16+2
  integer,parameter:: op_mult_i16=op_start_i16+3
  integer,parameter:: op_divide_i16=op_start_i16+4
  integer,parameter:: op_div_i16=op_start_i16+5
  integer,parameter:: op_mod_i16=op_start_i16+6
  integer,parameter:: op_pow_i16=op_start_i16+7
  integer,parameter:: op_uminus_i16=op_start_i16+8
  integer,parameter:: op_eq_i16=op_start_i16+9
  integer,parameter:: op_ne_i16=op_start_i16+10
  integer,parameter:: op_gt_i16=op_start_i16+11
  integer,parameter:: op_ge_i16=op_start_i16+12
  integer,parameter:: op_string_i16=op_start_i16+13
  integer,parameter:: op_max_i16 = op_start_i16+16
  integer,parameter:: op_min_i16 = op_start_i16+17
  integer,parameter:: op_assign_i16 = op_start_i16 + 18
  integer,parameter:: op_int_i16 = op_start_i16+19
  integer,parameter:: op_real_i16 = op_start_i16+20
  integer,parameter:: op_double_i16 = op_start_i16+21
  integer,parameter:: op_abs_i16 = op_start_i16+22
  integer,parameter:: op_band_i16 = op_start_i16+23
  integer,parameter:: op_bor_i16 = op_start_i16+24
  integer,parameter:: op_bxor_i16 = op_start_i16+25
  integer,parameter:: op_bshift_i16 = op_start_i16+26
  integer,parameter:: op_bnot_i16 = op_start_i16+27
  integer,parameter:: op_pdiff_i16 = op_start_i16+28
  integer,parameter:: op_sign_i16 = op_start_i16+29
  integer,parameter:: op_modulo_i16 = op_start_i16+30
  integer,parameter:: op_i8_i16 = op_start_i16+31
  integer,parameter:: op_i32_i16 = op_start_i16+32
  integer,parameter:: op_i64_i16 = op_start_i16+33
  integer,parameter:: op_long_i16 = op_start_i16+34
  integer,parameter:: op_offset_i16 = op_start_i16+35
  integer,parameter:: op_stop_i16 = op_start_i16+35

  integer,parameter:: op_start_i32=op_stop_i16
  integer,parameter:: op_add_i32=op_start_i32+1
  integer,parameter:: op_sub_i32=op_start_i32+2
  integer,parameter:: op_mult_i32=op_start_i32+3
  integer,parameter:: op_divide_i32=op_start_i32+4
  integer,parameter:: op_div_i32=op_start_i32+5
  integer,parameter:: op_mod_i32=op_start_i32+6
  integer,parameter:: op_pow_i32=op_start_i32+7
  integer,parameter:: op_uminus_i32=op_start_i32+8
  integer,parameter:: op_eq_i32=op_start_i32+9
  integer,parameter:: op_ne_i32=op_start_i32+10
  integer,parameter:: op_gt_i32=op_start_i32+11
  integer,parameter:: op_ge_i32=op_start_i32+12
  integer,parameter:: op_string_i32=op_start_i32+13
  integer,parameter:: op_max_i32 = op_start_i32+16
  integer,parameter:: op_min_i32 = op_start_i32+17
  integer,parameter:: op_assign_i32 = op_start_i32 + 18
  integer,parameter:: op_int_i32 = op_start_i32+19
  integer,parameter:: op_real_i32 = op_start_i32+20
  integer,parameter:: op_double_i32 = op_start_i32+21
  integer,parameter:: op_abs_i32 = op_start_i32+22
  integer,parameter:: op_band_i32 = op_start_i32+23
  integer,parameter:: op_bor_i32 = op_start_i32+24
  integer,parameter:: op_bxor_i32 = op_start_i32+25
  integer,parameter:: op_bshift_i32 = op_start_i32+26
  integer,parameter:: op_bnot_i32 = op_start_i32+27
  integer,parameter:: op_pdiff_i32 = op_start_i32+28
  integer,parameter:: op_sign_i32 = op_start_i32+29
  integer,parameter:: op_modulo_i32 = op_start_i32+30
  integer,parameter:: op_i8_i32 = op_start_i32+31
  integer,parameter:: op_i16_i32 = op_start_i32+32
  integer,parameter:: op_i64_i32 = op_start_i32+33
  integer,parameter:: op_long_i32 = op_start_i32+34
  integer,parameter:: op_offset_i32 = op_start_i32+35
  integer,parameter:: op_stop_i32 = op_start_i32+35

  integer,parameter:: op_start_i64=op_stop_i32
  integer,parameter:: op_add_i64=op_start_i64+1
  integer,parameter:: op_sub_i64=op_start_i64+2
  integer,parameter:: op_mult_i64=op_start_i64+3
  integer,parameter:: op_divide_i64=op_start_i64+4
  integer,parameter:: op_div_i64=op_start_i64+5
  integer,parameter:: op_mod_i64=op_start_i64+6
  integer,parameter:: op_pow_i64=op_start_i64+7
  integer,parameter:: op_uminus_i64=op_start_i64+8
  integer,parameter:: op_eq_i64=op_start_i64+9
  integer,parameter:: op_ne_i64=op_start_i64+10
  integer,parameter:: op_gt_i64=op_start_i64+11
  integer,parameter:: op_ge_i64=op_start_i64+12
  integer,parameter:: op_string_i64=op_start_i64+13
  integer,parameter:: op_max_i64 = op_start_i64+16
  integer,parameter:: op_min_i64 = op_start_i64+17
  integer,parameter:: op_assign_i64 = op_start_i64 + 18
  integer,parameter:: op_int_i64 = op_start_i64+19
  integer,parameter:: op_real_i64 = op_start_i64+20
  integer,parameter:: op_double_i64 = op_start_i64+21
  integer,parameter:: op_abs_i64 = op_start_i64+22
  integer,parameter:: op_band_i64 = op_start_i64+23
  integer,parameter:: op_bor_i64 = op_start_i64+24
  integer,parameter:: op_bxor_i64 = op_start_i64+25
  integer,parameter:: op_bshift_i64 = op_start_i64+26
  integer,parameter:: op_bnot_i64 = op_start_i64+27
  integer,parameter:: op_pdiff_i64 = op_start_i64+28
  integer,parameter:: op_sign_i64 = op_start_i64+29
  integer,parameter:: op_modulo_i64 = op_start_i64+30
  integer,parameter:: op_i8_i64 = op_start_i64+31
  integer,parameter:: op_i16_i64 = op_start_i64+32
  integer,parameter:: op_i32_i64 = op_start_i64+33
  integer,parameter:: op_long_i64 = op_start_i64+34
  integer,parameter:: op_offset_i64 = op_start_i64+35
  integer,parameter:: op_stop_i64 = op_start_i64+35
  
  integer,parameter:: op_start_r =op_stop_i64
  integer,parameter:: op_add_r=op_start_r+1
  integer,parameter:: op_sub_r=op_start_r+2
  integer,parameter:: op_mult_r=op_start_r+3
  integer,parameter:: op_divide_r=op_start_r+4
  integer,parameter:: op_div_r=op_start_r+5
  integer,parameter:: op_mod_r=op_start_r+6
  integer,parameter:: op_pow_r=op_start_r+7
  integer,parameter:: op_uminus_r=op_start_r+8
  integer,parameter:: op_eq_r=op_start_r+9
  integer,parameter:: op_ne_r=op_start_r+10
  integer,parameter:: op_gt_r=op_start_r+11
  integer,parameter:: op_ge_r=op_start_r+12
  integer,parameter:: op_string_r=op_start_r+13
  integer,parameter:: op_max_r = op_start_r+16
  integer,parameter:: op_min_r = op_start_r+17
  integer,parameter:: op_assign_r = op_start_r+18
  integer,parameter:: op_int_r = op_start_r+19
  integer,parameter:: op_long_r = op_start_r+20
  integer,parameter:: op_double_r = op_start_r+21
  integer,parameter:: op_abs_r = op_start_r+22
  integer,parameter:: op_acos_r = op_start_r+23
  integer,parameter:: op_asin_r = op_start_r+24
  integer,parameter:: op_atan_r = op_start_r+25
  integer,parameter:: op_atan2_r = op_start_r+26
  integer,parameter:: op_cos_r = op_start_r+27
  integer,parameter:: op_cosh_r = op_start_r+28
  integer,parameter:: op_exp_r = op_start_r+29
  integer,parameter:: op_log_r = op_start_r+30
  integer,parameter:: op_log10_r = op_start_r+31
  integer,parameter:: op_sin_r = op_start_r+32
  integer,parameter:: op_sinh_r = op_start_r+33
  integer,parameter:: op_sqrt_r = op_start_r+34
  integer,parameter:: op_tan_r = op_start_r+35
  integer,parameter:: op_tanh_r = op_start_r+36
  integer,parameter:: op_floor_r = op_start_r+37
  integer,parameter:: op_ceil_r = op_start_r+38
  integer,parameter:: op_modulo_r = op_start_r+39
  integer,parameter:: op_sign_r = op_start_r+40
  integer,parameter:: op_pdiff_r = op_start_r+41
  integer,parameter:: op_offset_r = op_start_r+42
  integer,parameter:: op_r32_r = op_start_r+43
  integer,parameter:: op_r64_r = op_start_r+44
  integer,parameter:: op_complex_r = op_start_r+45
  integer,parameter:: op_complex2_r = op_start_r+46
  integer,parameter:: op_stop_r = op_start_r+46

  integer,parameter:: op_start_d =op_stop_r
  integer,parameter:: op_add_d=op_start_d+1
  integer,parameter:: op_sub_d=op_start_d+2
  integer,parameter:: op_mult_d=op_start_d+3
  integer,parameter:: op_divide_d=op_start_d+4
  integer,parameter:: op_div_d=op_start_d+5
  integer,parameter:: op_mod_d=op_start_d+6
  integer,parameter:: op_pow_d=op_start_d+7
  integer,parameter:: op_uminus_d=op_start_d+8
  integer,parameter:: op_eq_d=op_start_d+9
  integer,parameter:: op_ne_d=op_start_d+10
  integer,parameter:: op_gt_d=op_start_d+11
  integer,parameter:: op_ge_d=op_start_d+12
  integer,parameter:: op_string_d=op_start_d+13
  integer,parameter:: op_max_d = op_start_d+16
  integer,parameter:: op_min_d = op_start_d+17
  integer,parameter:: op_assign_d = op_start_d+18
  integer,parameter:: op_int_d = op_start_d+19
  integer,parameter:: op_long_d = op_start_d+20
  integer,parameter:: op_real_d = op_start_d+21
  integer,parameter:: op_abs_d = op_start_d+22
  integer,parameter:: op_acos_d = op_start_d+23
  integer,parameter:: op_asin_d = op_start_d+24
  integer,parameter:: op_atan_d = op_start_d+25
  integer,parameter:: op_atan2_d = op_start_d+26
  integer,parameter:: op_cos_d = op_start_d+27
  integer,parameter:: op_cosh_d = op_start_d+28
  integer,parameter:: op_exp_d = op_start_d+29
  integer,parameter:: op_log_d = op_start_d+30
  integer,parameter:: op_log10_d = op_start_d+31
  integer,parameter:: op_sin_d = op_start_d+32
  integer,parameter:: op_sinh_d = op_start_d+33
  integer,parameter:: op_sqrt_d = op_start_d+34
  integer,parameter:: op_tan_d = op_start_d+35
  integer,parameter:: op_tanh_d = op_start_d+36
  integer,parameter:: op_floor_d = op_start_d+37
  integer,parameter:: op_ceil_d = op_start_d+38
  integer,parameter:: op_modulo_d = op_start_d+39
  integer,parameter:: op_sign_d = op_start_d+40
  integer,parameter:: op_pdiff_d= op_start_d+41
  integer,parameter:: op_offset_d = op_start_d+42
  integer,parameter:: op_r32_d= op_start_d+43
  integer,parameter:: op_r64_d= op_start_d+44
  integer,parameter:: op_complex_d = op_start_d+45
  integer,parameter:: op_complex2_d = op_start_d+46
  integer,parameter:: op_stop_d = op_start_d+46

  integer,parameter:: op_start_c =op_stop_d
  integer,parameter:: op_add_c=op_start_c+1
  integer,parameter:: op_sub_c=op_start_c+2
  integer,parameter:: op_mult_c=op_start_c+3
  integer,parameter:: op_divide_c=op_start_c+4
  integer,parameter:: op_rpow_c=op_start_c+6
  integer,parameter:: op_pow_c=op_start_c+7
  integer,parameter:: op_uminus_c=op_start_c+8
  integer,parameter:: op_eq_c=op_start_c+9
  integer,parameter:: op_ne_c=op_start_c+10
  integer,parameter:: op_string_c=op_start_c+13
  integer,parameter:: op_assign_c = op_start_c+18
  integer,parameter:: op_real_c = op_start_c+21
  integer,parameter:: op_abs_c = op_start_c+22
  integer,parameter:: op_acos_c = op_start_c+23
  integer,parameter:: op_asin_c = op_start_c+24
  integer,parameter:: op_atan_c = op_start_c+25
  integer,parameter:: op_atan2_c = op_start_c+26
  integer,parameter:: op_cos_c = op_start_c+27
  integer,parameter:: op_cosh_c = op_start_c+28
  integer,parameter:: op_exp_c = op_start_c+29
  integer,parameter:: op_log_c = op_start_c+30
  integer,parameter:: op_log10_c = op_start_c+31
  integer,parameter:: op_sin_c = op_start_c+32
  integer,parameter:: op_sinh_c = op_start_c+33
  integer,parameter:: op_sqrt_c = op_start_c+34
  integer,parameter:: op_tan_c = op_start_c+35
  integer,parameter:: op_tanh_c = op_start_c+36
  integer,parameter:: op_imag_c = op_start_c+37
  integer,parameter:: op_conj_c = op_start_c+38
  integer,parameter:: op_stop_c = op_start_c+44

  integer,parameter:: op_start_dc =op_stop_c
  integer,parameter:: op_add_dc=op_start_dc+1
  integer,parameter:: op_sub_dc=op_start_dc+2
  integer,parameter:: op_mult_dc=op_start_dc+3
  integer,parameter:: op_divide_dc=op_start_dc+4
  integer,parameter:: op_dpow_dc=op_start_dc+6
  integer,parameter:: op_pow_dc=op_start_dc+7
  integer,parameter:: op_uminus_dc=op_start_dc+8
  integer,parameter:: op_eq_dc=op_start_dc+9
  integer,parameter:: op_ne_dc=op_start_dc+10
  integer,parameter:: op_string_dc=op_start_dc+13
  integer,parameter:: op_assign_dc = op_start_dc+18
  integer,parameter:: op_real_dc = op_start_dc+21
  integer,parameter:: op_abs_dc = op_start_dc+22
  integer,parameter:: op_acos_dc = op_start_dc+23
  integer,parameter:: op_asin_dc = op_start_dc+24
  integer,parameter:: op_atan_dc = op_start_dc+25
  integer,parameter:: op_atan2_dc = op_start_dc+26
  integer,parameter:: op_cos_dc = op_start_dc+27
  integer,parameter:: op_cosh_dc = op_start_dc+28
  integer,parameter:: op_exp_dc = op_start_dc+29
  integer,parameter:: op_log_dc = op_start_dc+30
  integer,parameter:: op_log10_dc = op_start_dc+31
  integer,parameter:: op_sin_dc = op_start_dc+32
  integer,parameter:: op_sinh_dc = op_start_dc+33
  integer,parameter:: op_sqrt_dc = op_start_dc+34
  integer,parameter:: op_tan_dc = op_start_dc+35
  integer,parameter:: op_tanh_dc = op_start_dc+36
  integer,parameter:: op_imag_dc = op_start_dc+37
  integer,parameter:: op_conj_dc = op_start_dc+38
  integer,parameter:: op_stop_dc = op_start_dc+44

  integer,parameter:: op_start_comp=op_stop_dc
  integer,parameter:: op_do_loop=op_start_comp+1
  integer,parameter:: op_mask=op_start_comp+2
  integer,parameter:: op_if=op_start_comp+3
  integer,parameter:: op_if_shared=op_start_comp+4
  integer,parameter:: op_if_shared_node=op_start_comp+5
  integer,parameter:: op_if_restart=op_start_comp+6
  integer,parameter:: op_loop=op_start_comp+7
  integer,parameter:: op_comm_loop=op_start_comp+8
  integer,parameter:: op_comm_block=op_start_comp+9
  integer,parameter:: op_comm_proc=op_start_comp+10
  integer,parameter:: op_comm_inline=op_start_comp+11
  integer,parameter:: op_allocate=op_start_comp+12
  integer,parameter:: op_deallocate=op_start_comp+13
  integer,parameter:: op_set_extent=op_start_comp+14
  integer,parameter:: op_break_loop=op_start_comp+15
  integer,parameter:: op_nested_loop=op_start_comp+16
  integer,parameter:: op_blocked_loop=op_start_comp+17
  integer,parameter:: op_over=op_start_comp+18
  integer,parameter:: op_comm_loop_par=op_start_comp+19
  integer,parameter:: op_inline_shared=op_start_comp+20
  integer,parameter:: op_assign_farray=op_start_comp+21
  integer,parameter:: op_init_farray=op_start_comp+22
  integer,parameter:: op_wrap=op_start_comp+23
  integer,parameter:: op_stop_comp=op_init_farray
  
  integer,parameter:: num_op=op_stop_comp
  
  character(len=20),dimension(:),allocatable:: op_names
 
contains

  subroutine set_op_names
    integer:: i
    if(allocated(op_names)) return
    allocate(op_names(0:num_op+1))
    op_names='??'
    op_names(op_call)='op_call'
    op_names(op_comm_call)='op_comm_call'
    op_names(op_jmp)='op_jmp'
    op_names(op_jmp_any_ve)='op_jmp_any_ve'
    op_names(op_jmp_any_ve_par)='op_jmp_any_ve_par'
    op_names(op_and_ve)='op_and_ve'
    op_names(op_andnot_ve)='op_andnot_ve'
    op_names(op_and_jmp_any)='op_and_jmp_any'
    op_names(op_andnot_jmp_any)='op_andnot_jmp_any'
    op_names(op_reduce)='op_reduce'
    op_names(op_jmp_nopar)='op_jmp_nopar'
    op_names(op_jmp_noshare)='op_jmp_noshare'
    op_names(op_head_node)='op_head_node'
    op_names(op_skip_comms)='op_skip_comms'
    op_names(op_struct)='op_struct'
    op_names(op_rec)='op_rec'
    op_names(op_array)='op_array'
    op_names(op_array_noinit)='op_array_noinit'
    op_names(op_var_array)='op_var_array'
    op_names(op_get_dom)='op_get_dom'
    op_names(op_elem)='op_elem'
    op_names(op_elem_ref)='op_elem_ref'
    op_names(op_as)='op_as'
    op_names(op_make_poly)='op_make_poly'
    op_names(op_get_poly)='op_get_poly'
    op_names(op_get_poly2)='op_get_poly2'
    op_names(op_get_poly_or)='op_get_poly_or'
    op_names(op_any)='op_any'
    op_names(op_tag)='op_tag'
    op_names(op_tag_val)='op_tag_val'
    op_names(op_get_tag)='op_get_tag'
    op_names(op_get_size)='op_get_size'
    op_names(op_make_type_val)='op_make_type_val'
    op_names(op_import)='op_import'
    op_names(op_export)='op_export'
    op_names(op_export_param)='op_export_param'
    op_names(op_extract)='op_extract'
    op_names(op_return)='op_return'
    op_names(op_par_loop_end)='op_par_loop_end'
    op_names(op_par_find_end)='op_par_find_end'
    op_names(op_skip_empty)='op_skip_empty'
    op_names(op_skip_empty_local)='op_skip_empty_local'
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
    op_names(op_chan_array_elem)='op_chan_array_elem'
    op_names(op_chan_array_vect)='op_chan_array_vect'
    
    op_names(op_array_get_elem)='op_array_get_elem'
    op_names(op_array_set_elem)='op_array_set_elem'
    op_names(op_array_elems)='op_array_elems'
    op_names(op_make_array)='op_make_array'
    op_names(op_import_val)='op_import_val'
    op_names(op_import_back)='op_import_back'
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
    op_names(op_logical_return)='op_logical_return'
    op_names(op_redim)='op_redim'
    op_names(op_wshare)='op_wshare'
    op_names(op_block_cyclic)='op_block_cyclic'
    op_names(op_pack)='op_pack'
    op_names(op_dref)='op_dref'
    op_names(op_dref_elem)='op_dref_elem'
    op_names(op_import_dref)='op_import_dref'
    op_names(op_advance)='op_advance'
    op_names(op_advance_and)='op_advance_and'
    op_names(op_init_loop)='op_init_loop'

    op_names(op_intersect_seq)='op_intersect_seq'
    op_names(op_intersect_aseq)='op_intersect_aseq'
    op_names(op_expand_aseq)='op_expand_aseq'
    op_names(op_intersect_bseq)='op_intersect_bseq'
    op_names(op_gcd)='op_gcd'
    op_names(op_chan)='op_chan'
    op_names(op_active)='op_active'
    op_names(op_new_dump)='op_new_dump'
    op_names(op_show)='op_show'
    op_names(op_show_stack)='op_show_stack'
    
    op_names(op_this_node)='op_this_node'
    op_names(op_this_nnode)='op_this_nnode'
    op_names(op_shared_node)='op_shared_node'
    op_names(op_shared_nnode)='op_shared_nnode'
    op_names(op_is_shared)='op_is_shared'
    op_names(op_is_par)='op_is_par'
    op_names(op_push_node_grid)='op_push_node_grid'
    op_names(op_push_node_split)='op_push_node_split'
    op_names(op_push_node_distr)='op_push_node_distr'
    op_names(op_pop_node_conc)='op_pop_node_conc'
    op_names(op_push_node_conc)='op_push_node_conc'
    op_names(op_pop_node)='op_pop_node'
    op_names(op_broadcast)='op_broadcast'
    op_names(op_get_remote)='op_get_remote'
    op_names(op_pop_off_node)='op_pop_off_node'
    op_names(op_push_node_back)='op_push_node_back'
    op_names(op_sys_node)='op_sys_node'
    op_names(op_sys_nnode)='op_sys_nnode'
    op_names(op_this_node)='op_this_node'
    op_names(op_this_nnode)='op_sys_nnode'
    op_names(op_reduce_ve)='op_reduce_ve'
    op_names(op_start_loop_sync)='op_start_loop_sync'
    op_names(op_broadcast_val)='op_broadcast_val'
    op_names(op_gather)='op_gather'
    op_names(op_isend_offset)='op_isend_offset'
    op_names(op_irecv_offset)='op_irecv_offset'
    op_names(op_sync_mess)='op_sync_mess'
    op_names(op_get_dims)='op_get_dims'
    op_names(op_put_remote)='op_put_remote'
    op_names(op_get_remote_distr)='op_get_remote_distr'
    op_names(op_put_remote_distr)='op_put_remote_distr'
    op_names(op_remote_call)='op_remote_call'
    op_names(op_remote_send_call)='op_remote_send_call'
    op_names(op_collect_call)='op_collect_call'
    op_names(op_server_call)='op_server_call'
    op_names(op_bcast_call)='op_bcast_call'
    op_names(op_isend_req)='op_isend_req'
    op_names(op_recv_reply)='op_recv_reply'
    op_names(op_recv_offset)='op_recv_offset'
    op_names(op_recv_req_call)='op_recv_req_call'
    op_names(op_recv_assn_call)='op_recv_assn_call'
    op_names(op_isend_assn)='op_isend_assn'
    op_names(op_isend)='op_isend'
    op_names(op_irecv)='op_irecv'
    op_names(op_recv)='op_recv'
    op_names(op_isend_reply)='op_isend_reply'
    op_names(op_do_at)='op_do_at'
    op_names(op_root_node)='op_root_node'
    op_names(op_broadcast_disp)='op_broadcast_disp'
    op_names(op_broadcast_shared)='op_broadcast_shared'
    op_names(op_isend_grid)='op_isend_grid'
    op_names(op_irecv_grid)='op_irecv_grid'
    op_names(op_recv_grid)='op_recv_grid'
    op_names(op_recv_grid_resend)='op_recv_grid_resend'
    op_names(op_recv_offset_resend)='op_recv_offset_resend'

    op_names(op_open_file)='op_open_file'
    op_names(op_close_file)='op_close_file'
    op_names(op_seek_file)='op_seek_file'
    op_names(op_read_file)='op_read_file'
    op_names(op_write_file)='op_write_file'
    op_names(op_read_file_array)='op_read_file_array'
    op_names(op_write_file_array)='op_write_file_array'
    op_names(op_read_file_tile)='op_read_file_tile'
    op_names(op_write_file_tile)='op_write_file_tile'
    op_names(op_io_error_string)='op_io_error_string'

    op_names(op_par_loop)='op_par_loop'
    op_names(op_par_find)='op_par_find'
    op_names(op_setref)='op_setref'
    op_names(op_clone)='op_clone'
    op_names(op_clone_ve)='op_clone_ve'
    op_names(op_get_esize)='op_get_esize'
    op_names(op_nullify)='op_nullify'
    op_names(op_assign)='op_assign'
    op_names(op_fill)='op_fill'

    op_names(op_eq)='op_eq'
    op_names(op_ne)='op_ne'

    op_names(op_string_l)='op_string_l'
    op_names(op_and)='op_and'
    op_names(op_or)='op_or'
    op_names(op_not)='op_not'
    op_names(op_assign_l)='op_assign_l'
    op_names(op_eq_l)='op_eq_l'
    op_names(op_ne_l)='op_ne_l'

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
    op_names(op_max_i)='op_max_i'
    op_names(op_min_i)='op_min_i'
    op_names(op_assign_i)='op_assign_i'
    op_names(op_long_i)='op_long_i'
    op_names(op_real_i)='op_real_i'
    op_names(op_double_i)='op_double_i'
    op_names(op_abs_i)='op_abs_i'
    op_names(op_band_i)='op_band_i'
    op_names(op_bor_i)='op_bor_i'
    op_names(op_bxor_i)='op_bxor_i'
    op_names(op_bshift_i)='op_bshift_i'
    op_names(op_bnot_i)='op_bnot_i'
    op_names(op_pdiff_i)='op_pdiff_i'
    op_names(op_sign_i)='op_sign_i'
    op_names(op_modulo_i)='op_modulo_i'
    op_names(op_i8_i)='op_i8_i'
    op_names(op_i16_i)='op_i16_i'
    op_names(op_i32_i)='op_i32_i'
    op_names(op_i64_i)='op_i64_i'
    op_names(op_offset_i)='op_offset_i'

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
    op_names(op_max_ln)='op_max_ln'
    op_names(op_min_ln)='op_min_ln'
    op_names(op_assign_ln)='op_assign_ln'
    op_names(op_int_ln)='op_int_ln'
    op_names(op_real_ln)='op_real_ln'
    op_names(op_double_ln)='op_double_ln'
    op_names(op_abs_ln)='op_abs_ln'
    op_names(op_band_ln)='op_band_ln'
    op_names(op_bor_ln)='op_bor_ln'
    op_names(op_bxor_ln)='op_bxor_ln'
    op_names(op_bshift_ln)='op_bshift_ln'
    op_names(op_bnot_ln)='op_bnot_ln'
    op_names(op_pdiff_ln)='op_pdiff_ln'
    op_names(op_sign_ln)='op_sign_ln'
    op_names(op_modulo_ln)='op_modulo_ln'
    op_names(op_i8_ln)='op_i8_ln'
    op_names(op_i16_ln)='op_i16_ln'
    op_names(op_i32_ln)='op_i32_ln'
    op_names(op_i64_ln)='op_i64_ln'
    op_names(op_offset_ln)='op_offset_ln'

    op_names(op_add_offset)='op_add_offset'
    op_names(op_sub_offset)='op_sub_offset'
    op_names(op_mult_offset)='op_mult_offset'
    op_names(op_divide_offset)='op_divide_offset'
    op_names(op_div_offset)='op_div_offset'
    op_names(op_mod_offset)='op_mod_offset'
    op_names(op_pow_offset)='op_pow_offset'
    op_names(op_uminus_offset)='op_uminus_offset'
    op_names(op_eq_offset)='op_eq_offset'
    op_names(op_ne_offset)='op_ne_offset'
    op_names(op_gt_offset)='op_gt_offset'
    op_names(op_ge_offset)='op_ge_offset'
    op_names(op_string_offset)='op_string_offset'
    op_names(op_max_offset)='op_max_offset'
    op_names(op_min_offset)='op_min_offset'
    op_names(op_assign_offset)='op_assign_offset'
    op_names(op_int_offset)='op_int_offset'
    op_names(op_real_offset)='op_real_offset'
    op_names(op_double_offset)='op_double_offset'
    op_names(op_abs_offset)='op_abs_offset'
    op_names(op_band_offset)='op_band_offset'
    op_names(op_bor_offset)='op_bor_offset'
    op_names(op_bxor_offset)='op_bxor_offset'
    op_names(op_bshift_offset)='op_bshift_offset'
    op_names(op_bnot_offset)='op_bnot_offset'
    op_names(op_pdiff_offset)='op_pdiff_offset'
    op_names(op_sign_offset)='op_sign_offset'
    op_names(op_modulo_offset)='op_modulo_offset'
    op_names(op_i8_offset)='op_i8_offset'
    op_names(op_i16_offset)='op_i16_offset'
    op_names(op_i32_offset)='op_i32_offset'
    op_names(op_i64_offset)='op_i64_offset'

    op_names(op_add_i8)='op_add_i8'
    op_names(op_sub_i8)='op_sub_i8'
    op_names(op_mult_i8)='op_mult_i8'
    op_names(op_divide_i8)='op_divide_i8'
    op_names(op_div_i8)='op_div_i8'
    op_names(op_mod_i8)='op_mod_i8'
    op_names(op_pow_i8)='op_pow_i8'
    op_names(op_uminus_i8)='op_uminus_i8'
    op_names(op_eq_i8)='op_eq_i8'
    op_names(op_ne_i8)='op_ne_i8'
    op_names(op_gt_i8)='op_gt_i8'
    op_names(op_ge_i8)='op_ge_i8'
    op_names(op_string_i8)='op_string_i8'
    op_names(op_max_i8)='op_max_i8'
    op_names(op_min_i8)='op_min_i8'
    op_names(op_assign_i8)='op_assign_i8'
    op_names(op_int_i8)='op_int_i8'
    op_names(op_real_i8)='op_real_i8'
    op_names(op_double_i8)='op_double_i8'
    op_names(op_abs_i8)='op_abs_i8'
    op_names(op_band_i8)='op_band_i8'
    op_names(op_bor_i8)='op_bor_i8'
    op_names(op_bxor_i8)='op_bxor_i8'
    op_names(op_bshift_i8)='op_bshift_i8'
    op_names(op_bnot_i8)='op_bnot_i8'
    op_names(op_pdiff_i8)='op_pdiff_i8'
    op_names(op_sign_i8)='op_sign_i8'
    op_names(op_modulo_i8)='op_modulo_i8'
    op_names(op_i16_i8)='op_i16_i8'
    op_names(op_i32_i8)='op_i32_i8'
    op_names(op_i64_i8)='op_i64_i8'
    op_names(op_offset_i8)='op_offset_i8'
    op_names(op_long_i8)='op_long_i8'

    op_names(op_add_i16)='op_add_i16'
    op_names(op_sub_i16)='op_sub_i16'
    op_names(op_mult_i16)='op_mult_i16'
    op_names(op_divide_i16)='op_divide_i16'
    op_names(op_div_i16)='op_div_i16'
    op_names(op_mod_i16)='op_mod_i16'
    op_names(op_pow_i16)='op_pow_i16'
    op_names(op_uminus_i16)='op_uminus_i16'
    op_names(op_eq_i16)='op_eq_i16'
    op_names(op_ne_i16)='op_ne_i16'
    op_names(op_gt_i16)='op_gt_i16'
    op_names(op_ge_i16)='op_ge_i16'
    op_names(op_string_i16)='op_string_i16'
    op_names(op_max_i16)='op_max_i16'
    op_names(op_min_i16)='op_min_i16'
    op_names(op_assign_i16)='op_assign_i16'
    op_names(op_int_i16)='op_int_i16'
    op_names(op_real_i16)='op_real_i16'
    op_names(op_double_i16)='op_double_i16'
    op_names(op_abs_i16)='op_abs_i16'
    op_names(op_band_i16)='op_band_i16'
    op_names(op_bor_i16)='op_bor_i16'
    op_names(op_bxor_i16)='op_bxor_i16'
    op_names(op_bshift_i16)='op_bshift_i16'
    op_names(op_bnot_i16)='op_bnot_i16'
    op_names(op_pdiff_i16)='op_pdiff_i16'
    op_names(op_sign_i16)='op_sign_i16'
    op_names(op_modulo_i16)='op_modulo_i16'
    op_names(op_i8_i16)='op_i8_i16'
    op_names(op_i32_i16)='op_i32_i16'
    op_names(op_i64_i16)='op_i64_i16'
    op_names(op_offset_i16)='op_offset_i16'
    op_names(op_long_i16)='op_long_i16'

    
    op_names(op_add_i32)='op_add_i32'
    op_names(op_sub_i32)='op_sub_i32'
    op_names(op_mult_i32)='op_mult_i32'
    op_names(op_divide_i32)='op_divide_i32'
    op_names(op_div_i32)='op_div_i32'
    op_names(op_mod_i32)='op_mod_i32'
    op_names(op_pow_i32)='op_pow_i32'
    op_names(op_uminus_i32)='op_uminus_i32'
    op_names(op_eq_i32)='op_eq_i32'
    op_names(op_ne_i32)='op_ne_i32'
    op_names(op_gt_i32)='op_gt_i32'
    op_names(op_ge_i32)='op_ge_i32'
    op_names(op_string_i32)='op_string_i32'
    op_names(op_max_i32)='op_max_i32'
    op_names(op_min_i32)='op_min_i32'
    op_names(op_assign_i32)='op_assign_i32'
    op_names(op_int_i32)='op_int_i32'
    op_names(op_real_i32)='op_real_i32'
    op_names(op_double_i32)='op_double_i32'
    op_names(op_abs_i32)='op_abs_i32'
    op_names(op_band_i32)='op_band_i32'
    op_names(op_bor_i32)='op_bor_i32'
    op_names(op_bxor_i32)='op_bxor_i32'
    op_names(op_bshift_i32)='op_bshift_i32'
    op_names(op_bnot_i32)='op_bnot_i32'
    op_names(op_pdiff_i32)='op_pdiff_i32'
    op_names(op_sign_i32)='op_sign_i32'
    op_names(op_modulo_i32)='op_modulo_i32'
    op_names(op_i8_i32)='op_i8_i32'
    op_names(op_i16_i32)='op_i16_i32'
    op_names(op_i64_i32)='op_i64_i32'
    op_names(op_offset_i32)='op_offset_i32'
    op_names(op_long_i32)='op_long_i32'

    
    op_names(op_add_i64)='op_add_i64'
    op_names(op_sub_i64)='op_sub_i64'
    op_names(op_mult_i64)='op_mult_i64'
    op_names(op_divide_i64)='op_divide_i64'
    op_names(op_div_i64)='op_div_i64'
    op_names(op_mod_i64)='op_mod_i64'
    op_names(op_pow_i64)='op_pow_i64'
    op_names(op_uminus_i64)='op_uminus_i64'
    op_names(op_eq_i64)='op_eq_i64'
    op_names(op_ne_i64)='op_ne_i64'
    op_names(op_gt_i64)='op_gt_i64'
    op_names(op_ge_i64)='op_ge_i64'
    op_names(op_string_i64)='op_string_i64'
    op_names(op_max_i64)='op_max_i64'
    op_names(op_min_i64)='op_min_i64'
    op_names(op_assign_i64)='op_assign_i64'
    op_names(op_int_i64)='op_int_i64'
    op_names(op_real_i64)='op_real_i64'
    op_names(op_double_i64)='op_double_i64'
    op_names(op_abs_i64)='op_abs_i64'
    op_names(op_band_i64)='op_band_i64'
    op_names(op_bor_i64)='op_bor_i64'
    op_names(op_bxor_i64)='op_bxor_i64'
    op_names(op_bshift_i64)='op_bshift_i64'
    op_names(op_bnot_i64)='op_bnot_i64'
    op_names(op_pdiff_i64)='op_pdiff_i64'
    op_names(op_sign_i64)='op_sign_i64'
    op_names(op_modulo_i64)='op_modulo_i64'
    op_names(op_i8_i64)='op_i8_i64'
    op_names(op_i16_i64)='op_i16_i64'
    op_names(op_i32_i64)='op_i32_i64'
    op_names(op_offset_i8)='op_offset_i64'
    op_names(op_long_i8)='op_long_i64'
    
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
    op_names(op_max_r)='op_max_r'
    op_names(op_min_r)='op_min_r'
    op_names(op_assign_r)='op_assign_r'
    op_names(op_int_r)='op_int_r'
    op_names(op_long_r)='op_long_r'
    op_names(op_double_r)='op_double_r'
    op_names(op_abs_r)='op_abs_r'
    op_names(op_acos_r)='op_acos_r'
    op_names(op_asin_r)='op_asin_r'
    op_names(op_atan_r)='op_atan_r'
    op_names(op_atan2_r)='op_atan2_r'
    op_names(op_cos_r)='op_cos_r'
    op_names(op_cosh_r)='op_cosh_r'
    op_names(op_exp_r)='op_exp_r'
    op_names(op_log_r)='op_log_r'
    op_names(op_log10_r)='op_log10_r'
    op_names(op_sin_r)='op_sin_r'
    op_names(op_sinh_r)='op_sinh_r'
    op_names(op_sqrt_r)='op_sqrt_r'
    op_names(op_tan_r)='op_tan_r'
    op_names(op_tanh_r)='op_tanh_r'
    op_names(op_floor_r)='op_floor_r'
    op_names(op_ceil_r)='op_ceil_r'
    op_names(op_modulo_r)='op_modulo_r'
    op_names(op_sign_r)='op_sign_r'
    op_names(op_pdiff_r)='op_pdiff_r'
    op_names(op_offset_r)='op_offset_r'
    op_names(op_r32_r)='op_r32_r'
    op_names(op_r64_r)='op_r64_r'
    op_names(op_complex_r)='op_complex_r'
    op_names(op_complex2_r)='op_complex2_r'

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
    op_names(op_max_d)='op_max_d'
    op_names(op_min_d)='op_min_d'
    op_names(op_assign_d)='op_assign_d'
    op_names(op_int_d)='op_int_d'
    op_names(op_long_d)='op_long_d'
    op_names(op_real_d)='op_real_d'
    op_names(op_abs_d)='op_abs_d'
    op_names(op_acos_d)='op_acos_d'
    op_names(op_asin_d)='op_asin_d'
    op_names(op_atan_d)='op_atan_d'
    op_names(op_atan2_d)='op_atan2_d'
    op_names(op_cos_d)='op_cos_d'
    op_names(op_cosh_d)='op_cosh_d'
    op_names(op_exp_d)='op_exp_d'
    op_names(op_log_d)='op_log_d'
    op_names(op_log10_d)='op_log10_d'
    op_names(op_sin_d)='op_sin_d'
    op_names(op_sinh_d)='op_sinh_d'
    op_names(op_sqrt_d)='op_sqrt_d'
    op_names(op_tan_d)='op_tan_d'
    op_names(op_tanh_d)='op_tanh_d'
    op_names(op_floor_d)='op_floor_d'
    op_names(op_ceil_d)='op_ceil_d'
    op_names(op_modulo_d)='op_modulo_d'
    op_names(op_sign_d)='op_sign_d'
    op_names(op_pdiff_d)='op_pdiff_d'
    op_names(op_offset_d)='op_offset_d'
    op_names(op_r32_d)='op_r32_d'
    op_names(op_r64_d)='op_r64_d'
    op_names(op_complex_d)='op_complex_d'
    op_names(op_complex2_d)='op_complex2_d'

    op_names(op_add_c)='op_add_c'
    op_names(op_sub_c)='op_sub_c'
    op_names(op_mult_c)='op_mult_c'
    op_names(op_divide_c)='op_divide_c'
    op_names(op_rpow_c)='op_rpow_c'
    op_names(op_pow_c)='op_pow_c'
    op_names(op_uminus_c)='op_uminus_c'
    op_names(op_eq_c)='op_eq_c'
    op_names(op_ne_c)='op_ne_c'
    op_names(op_string_c)='op_string_c'
    op_names(op_assign_c)='op_assign_c'
    op_names(op_real_c)='op_real_c'
    op_names(op_abs_c)='op_abs_c'
    op_names(op_acos_c)='op_acos_c'
    op_names(op_asin_c)='op_asin_c'
    op_names(op_atan_c)='op_atan_c'
    op_names(op_atan2_c)='op_atan2_c'
    op_names(op_cos_c)='op_cos_c'
    op_names(op_cosh_c)='op_cosh_c'
    op_names(op_exp_c)='op_exp_c'
    op_names(op_log_c)='op_log_c'
    op_names(op_log10_c)='op_log10_c'
    op_names(op_sin_c)='op_sin_c'
    op_names(op_sinh_c)='op_sinh_c'
    op_names(op_sqrt_c)='op_sqrt_c'
    op_names(op_tan_c)='op_tan_c'
    op_names(op_tanh_c)='op_tanh_c'
    op_names(op_imag_c)='op_imag_c'
    op_names(op_conj_c)='op_conj_c'

    op_names(op_add_dc)='op_add_dc'
    op_names(op_sub_dc)='op_sub_dc'
    op_names(op_mult_dc)='op_mult_dc'
    op_names(op_divide_dc)='op_divide_dc'
    op_names(op_dpow_dc)='op_dpow_dc'
    op_names(op_pow_dc)='op_pow_dc'
    op_names(op_uminus_dc)='op_uminus_dc'
    op_names(op_eq_dc)='op_eq_dc'
    op_names(op_ne_dc)='op_ne_dc'
    op_names(op_string_dc)='op_string_dc'
    op_names(op_assign_dc)='op_assign_dc'
    op_names(op_real_dc)='op_real_dc'
    op_names(op_abs_dc)='op_abs_dc'
    op_names(op_acos_dc)='op_acos_dc'
    op_names(op_asin_dc)='op_asin_dc'
    op_names(op_atan_dc)='op_atan_dc'
    op_names(op_atan2_dc)='op_atan2_dc'
    op_names(op_cos_dc)='op_cos_dc'
    op_names(op_cosh_dc)='op_cosh_dc'
    op_names(op_exp_dc)='op_exp_dc'
    op_names(op_log_dc)='op_log_dc'
    op_names(op_log10_dc)='op_log10_dc'
    op_names(op_sin_dc)='op_sin_dc'
    op_names(op_sinh_dc)='op_sinh_dc'
    op_names(op_sqrt_dc)='op_sqrt_dc'
    op_names(op_tan_dc)='op_tan_dc'
    op_names(op_tanh_dc)='op_tanh_dc'
    op_names(op_imag_dc)='op_imag_dc'
    op_names(op_conj_dc)='op_conj_dc'

    op_names(op_do_loop)='op_do_loop'
    op_names(op_mask)='op_mask'    
    op_names(op_if)='op_if'
    op_names(op_if_shared)='op_if_shared'
    op_names(op_if_shared_node)='op_if_shared_node'
    op_names(op_if_restart)='op_if_restart'
    op_names(op_loop)='op_loop'
    op_names(op_comm_loop)='op_comm_loop'
    op_names(op_comm_block)='op_comm_block'
    op_names(op_comm_proc)='op_comm_proc'
    op_names(op_comm_inline)='op_comm_inline'
    op_names(op_allocate)='op_allocate'
    op_names(op_deallocate)='op_deallocate'
    op_names(op_set_extent)='op_set_extent'
    op_names(op_break_loop)='op_break_loop'
    op_names(op_nested_loop)='op_nested_loop'
    op_names(op_blocked_loop)='op_blocked_loop'
    op_names(op_over)='op_over'
    op_names(op_comm_loop_par)='op_comm_loop_par'
    op_names(op_inline_shared)='op_inline_shared'
    op_names(op_assign_farray)='op_assign_farray'
    op_names(op_init_farray)='op_init_farray'
    op_names(op_wrap)='op_wrap'

!!$    do i=op_call,op_comm_loop_par
!!$       if(op_names(i)=='??')then
!!$          write(*,*) 'MISSING OP NAME>>>>',i,'after',op_names(i-1)
!!$          stop
!!$       endif
!!$    enddo
  end subroutine set_op_names

  function proc_get_name(proc) result(name)
    type(pm_ptr),intent(in):: proc
    integer:: name
    type(pm_ptr):: p
    p=proc%data%ptr(proc%offset)
    name=p%data%i16(p%offset+2)
  end function proc_get_name

  subroutine proc_line_module(proc,offset,line,modl)
    type(pm_ptr),intent(in):: proc
    integer,intent(in):: offset
    integer,intent(out):: line,modl
    integer:: j
    integer:: k
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

  function proc_slot_name(proc,offset,slot) result(name)
    type(pm_ptr):: proc
    integer:: offset,slot
    integer:: name
    integer:: i,j,start,finish,n
    type(pm_ptr):: p
    integer:: k
    name=0
    if(pm_is_compiling) return
    p=proc%data%ptr(proc%offset+1)
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
