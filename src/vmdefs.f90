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
  use pm_sysdep
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_types
  implicit none

  integer,parameter:: wcode_file_cols=120
  
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
  integer,parameter:: op_skip_any=12
  integer,parameter:: op_skip_empty=13
  integer,parameter:: op_skip_empty_local=14
  integer,parameter:: op_head_node=15
  integer,parameter:: op_skip_comms=16

  integer,parameter:: last_jmp_op=16

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
  integer,parameter:: op_includes_aseq = op_misc3 + 13
  integer,parameter:: op_in_aseq = op_misc3 +14
  integer,parameter:: op_index_aseq = op_misc3 + 15
  integer,parameter:: op_gcd = op_misc3 + 16
  integer,parameter:: op_chan = op_misc3 + 17
  integer,parameter:: op_active = op_misc3 + 18
  integer,parameter:: op_new_dump = op_misc3 + 19
  integer,parameter:: op_show = op_misc3 + 20
  integer,parameter:: op_show_stack = op_misc3 + 21

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
  integer,parameter:: op_bcast_shared_offset = op_comm + 46
  integer,parameter:: op_bcast_shared_grid = op_comm + 47
  integer,parameter:: op_broadcast_shared = op_comm + 48
  integer,parameter:: op_isend_grid = op_comm + 49
  integer,parameter:: op_irecv_grid = op_comm + 50
  integer,parameter:: op_recv_grid = op_comm + 51
  integer,parameter:: op_recv_grid_resend = op_comm + 52
  integer,parameter:: op_recv_offset_resend = op_comm + 53

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
  integer,parameter:: op_sync=op_start_comp+24
  integer,parameter:: op_init_var=op_start_comp+25
  integer,parameter:: op_stop_comp=op_init_var

  integer,parameter:: num_op=op_stop_comp

  integer,dimension(0:num_op):: op_flags
  integer,parameter:: op_is_call=1
  integer,parameter:: op_is_jump=2
  integer,parameter:: op_is_gate=4
  integer,parameter:: op_takes_type=8
  integer,parameter:: op_1_block=16
  integer,parameter:: op_2_blocks=32
  integer,parameter:: op_prints_out=64
  integer,parameter:: op_is_comm=128
  integer,parameter:: op_is_send=256
  integer,parameter:: op_is_recv=512
  integer,parameter:: op_is_sync_recv=1024
  integer,parameter:: op_is_arith=2048
  integer,parameter:: op_is_file=4096
  integer,parameter:: op_precedes_loop=8192
  integer,parameter:: op_is_sync=16384
  integer,parameter:: op_is_fixed=32768
  integer,parameter:: op_sets_cstack=65536

  integer,parameter:: op_is_gate_and_jump=op_is_gate+op_is_jump
  integer,parameter:: op_is_comm_1_block=op_is_comm+op_1_block
  integer,parameter:: op_is_jump_1_block=op_is_jump+op_1_block
  integer,parameter:: op_is_recv_1_block=op_is_sync_recv+op_1_block


  data op_flags(op_call)            /op_is_call/
  data op_flags(op_comm_call)       /op_is_call/
  data op_flags(op_jmp)             /op_is_jump/
  data op_flags(op_jmp_any_ve)      /op_is_jump/
  data op_flags(op_jmp_any_ve_par)  /op_is_jump/
  data op_flags(op_and_ve)          /op_is_gate/
  data op_flags(op_andnot_ve)       /op_is_gate/
  data op_flags(op_and_jmp_any)     /op_is_gate_and_jump/
  data op_flags(op_andnot_jmp_any)  /op_is_gate_and_jump/
  data op_flags(op_reduce)          /0/
  data op_flags(op_jmp_nopar)       /op_is_jump/
  data op_flags(op_jmp_noshare)     /op_is_jump/
  data op_flags(op_head_node)       /op_is_jump_1_block/
  data op_flags(op_skip_comms)      /op_is_jump/
  data op_flags(op_struct)          /op_takes_type/
  data op_flags(op_rec)             /op_takes_type/
  data op_flags(op_array)           /op_takes_type/
  data op_flags(op_array_noinit)    /op_takes_type/
  data op_flags(op_var_array)       /op_takes_type/
  data op_flags(op_get_dom)         /0/
  data op_flags(op_elem)            /0/
  data op_flags(op_elem_ref)        /0/
  data op_flags(op_as)              /0/
  data op_flags(op_make_poly)       /op_takes_type/
  data op_flags(op_get_poly)        /0/
  data op_flags(op_get_poly2)       /0/
  data op_flags(op_get_poly_or)     /0/
  data op_flags(op_any)             /op_takes_type/
  data op_flags(op_tag)             /0/       ! Obsolete
  data op_flags(op_tag_val)         /0/   ! Obsolete
  data op_flags(op_get_tag)         /0/   ! Obsolete
  data op_flags(op_get_size)        /0/
  data op_flags(op_make_type_val)   /op_takes_type/
  data op_flags(op_import)          /0/
  data op_flags(op_export)          /0/
  data op_flags(op_export_param)    /0/
  data op_flags(op_extract)         /0/
  data op_flags(op_return)          /0/
  data op_flags(op_par_loop_end)    /0/
  data op_flags(op_par_find_end)    /0/
  data op_flags(op_skip_empty)      /op_1_block/
  data op_flags(op_skip_any)        /0/
  data op_flags(op_skip_empty_local) /0/
  data op_flags(op_check)           /0/
  data op_flags(op_dump)            /0/
  data op_flags(op_print)           /op_prints_out/
  data op_flags(op_concat)          /0/
  data op_flags(op_lookup_error)    /0/
  data op_flags(op_makekeys)        /0/
  data op_flags(op_delkeys)         /0/
  data op_flags(op_checkkeys)       /0/
  data op_flags(op_getvkey)         /0/
  data op_flags(op_var_call)        /0/     ! Obsolete
  data op_flags(op_make_dr)         /0/
  data op_flags(op_get_dr)          /0/
  data op_flags(op_set_dr)          /0/
  data op_flags(op_make_rf)         /0/
  data op_flags(op_get_rf)          /0/
  data op_flags(op_set_rf)          /0/
  data op_flags(op_check_logical)   /0/
  data op_flags(op_extract_first)   /0/
  data op_flags(op_chan_array_elem) /0/
  data op_flags(op_chan_array_vect) /0/

  data op_flags(op_array_get_elem)  /0/
  data op_flags(op_array_set_elem)  /0/
  data op_flags(op_array_elems)     /0/
  data op_flags(op_make_array)      /0/
  data op_flags(op_import_val)      /0/
  data op_flags(op_import_back)     /0/
  data op_flags(op_import_varg)     /0/
  data op_flags(op_extractelm)      /0/
  data op_flags(op_iota)            /0/
  data op_flags(op_indices)         /0/
  data op_flags(op_get_key)         /0/
  data op_flags(op_get_key2)        /0/
  data op_flags(op_export_array)    /0/
  data op_flags(op_miss_arg)        /0/
  data op_flags(op_default)         /0/
  data op_flags(op_dump_id)         /0/
  data op_flags(op_import_scalar)   /0/
  data op_flags(op_check_assign)    /0/
  data op_flags(op_same_type)       /0/
  data op_flags(op_logical_return)  /0/
  data op_flags(op_redim)           /0/
  data op_flags(op_wshare)          /0/
  data op_flags(op_block_cyclic)    /0/
  data op_flags(op_pack)            /0/
  data op_flags(op_dref)            /0/
  data op_flags(op_dref_elem)       /0/
  data op_flags(op_import_dref)     /0/
  data op_flags(op_advance)         /0/
  data op_flags(op_advance_and)     /0/
  data op_flags(op_init_loop)       /0/

  data op_flags(op_intersect_seq)   /0/
  data op_flags(op_intersect_aseq)  /0/
  data op_flags(op_expand_aseq)     /0/
  data op_flags(op_intersect_bseq)  /0/
  data op_flags(op_includes_aseq)   /0/
  data op_flags(op_in_aseq)         /0/
  data op_flags(op_index_aseq)      /0/
  data op_flags(op_gcd)             /0/
  data op_flags(op_chan)            /0/
  data op_flags(op_active)          /0/
  data op_flags(op_new_dump)        /0/
  data op_flags(op_show)            /0/
  data op_flags(op_show_stack)      /0/

  data op_flags(op_this_node)       /0/
  data op_flags(op_this_nnode)      /0/
  data op_flags(op_shared_node)     /0/
  data op_flags(op_shared_nnode)    /0/
  data op_flags(op_is_shared)       /0/
  data op_flags(op_is_par)          /0/
  data op_flags(op_push_node_grid)  /op_sets_cstack/
  data op_flags(op_push_node_split) /op_sets_cstack/
  data op_flags(op_push_node_distr) /op_sets_cstack/
  data op_flags(op_pop_node_conc)   /op_sets_cstack/
  data op_flags(op_push_node_conc)  /op_sets_cstack/
  data op_flags(op_pop_node)        /op_sets_cstack/
  data op_flags(op_broadcast)       /op_is_comm/
  data op_flags(op_get_remote)      /op_is_comm/
  data op_flags(op_pop_off_node)    /op_is_comm/
  data op_flags(op_push_node_back)  /op_is_comm/
  data op_flags(op_sys_node)        /0/
  data op_flags(op_sys_nnode)       /0/
  data op_flags(op_this_node)       /0/
  data op_flags(op_this_nnode)      /0/
  data op_flags(op_reduce_ve)       /0/
  data op_flags(op_start_loop_sync) /0/
  data op_flags(op_broadcast_val)   /op_is_comm/
  data op_flags(op_gather)          /op_is_comm/
  data op_flags(op_isend_offset)    /op_is_send/
  data op_flags(op_irecv_offset)    /op_is_recv/
  data op_flags(op_sync_mess)       /op_is_sync/
  data op_flags(op_get_dims)        /0/
  data op_flags(op_put_remote)      /op_is_comm/
  data op_flags(op_get_remote_distr) /op_is_comm/
  data op_flags(op_put_remote_distr) /op_is_comm/
  data op_flags(op_remote_call)     /op_is_comm_1_block/
  data op_flags(op_remote_send_call) /op_is_comm_1_block/
  data op_flags(op_collect_call)    /op_is_comm_1_block/
  data op_flags(op_server_call)     /op_is_comm_1_block/
  data op_flags(op_bcast_call)      /op_is_comm_1_block/
  data op_flags(op_isend_req)       /op_is_send/
  data op_flags(op_recv_reply)      /op_is_sync_recv/
  data op_flags(op_recv_offset)     /op_is_sync_recv/
  data op_flags(op_recv_req_call)   /op_is_recv_1_block/
  data op_flags(op_recv_assn_call)  /op_is_recv_1_block/
  data op_flags(op_isend_assn)      /op_is_send/
  data op_flags(op_isend)           /op_is_send/
  data op_flags(op_irecv)           /op_is_send/
  data op_flags(op_recv)            /op_is_sync_recv/
  data op_flags(op_isend_reply)     /op_is_send/
  data op_flags(op_do_at)           /op_1_block/
  data op_flags(op_root_node)       /0/
  data op_flags(op_bcast_shared_offset) /op_is_comm/
  data op_flags(op_bcast_shared_grid) /op_is_comm/
  data op_flags(op_broadcast_shared) /op_is_comm/
  data op_flags(op_isend_grid)      /op_is_send/
  data op_flags(op_irecv_grid)      /op_is_recv/
  data op_flags(op_recv_grid)       /op_is_sync_recv/
  data op_flags(op_recv_grid_resend) /op_is_sync_recv/
  data op_flags(op_recv_offset_resend) /op_is_sync_recv/

  data op_flags(op_open_file)        /op_is_file/
  data op_flags(op_close_file)       /op_is_file/
  data op_flags(op_seek_file)        /op_is_file/
  data op_flags(op_read_file)        /op_is_file/
  data op_flags(op_write_file)       /op_is_file/
  data op_flags(op_read_file_array)  /op_is_file/
  data op_flags(op_write_file_array) /op_is_file/
  data op_flags(op_read_file_tile)   /op_is_file/
  data op_flags(op_write_file_tile)  /op_is_file/
  data op_flags(op_io_error_string)  /op_is_file/

  data op_flags(op_par_loop)         /0/
  data op_flags(op_par_find)         /0/
  data op_flags(op_setref)           /0/
  data op_flags(op_clone)            /0/
  data op_flags(op_clone_ve)         /0/
  data op_flags(op_get_esize)        /0/
  data op_flags(op_nullify)          /0/
  data op_flags(op_assign)           /0/
  data op_flags(op_fill)             /0/

  data op_flags(op_eq)               /op_is_arith/
  data op_flags(op_ne)               /op_is_arith/

  data op_flags(op_string_l)         /op_is_arith/
  data op_flags(op_and)              /op_is_arith/
  data op_flags(op_or)               /op_is_arith/
  data op_flags(op_not)              /op_is_arith/
  data op_flags(op_assign_l)         /op_is_arith/
  data op_flags(op_eq_l)             /op_is_arith/
  data op_flags(op_ne_l)             /op_is_arith/

  data op_flags(op_add_i)            /op_is_arith/
  data op_flags(op_sub_i)            /op_is_arith/
  data op_flags(op_mult_i)           /op_is_arith/
  data op_flags(op_divide_i)         /op_is_arith/
  data op_flags(op_div_i)            /op_is_arith/
  data op_flags(op_mod_i)            /op_is_arith/
  data op_flags(op_pow_i)            /op_is_arith/
  data op_flags(op_uminus_i)         /op_is_arith/
  data op_flags(op_eq_i)             /op_is_arith/
  data op_flags(op_ne_i)             /op_is_arith/
  data op_flags(op_gt_i)             /op_is_arith/
  data op_flags(op_ge_i)             /op_is_arith/
  data op_flags(op_string_i)         /op_is_arith/
  data op_flags(op_max_i)            /op_is_arith/
  data op_flags(op_min_i)            /op_is_arith/
  data op_flags(op_assign_i)         /op_is_arith/
  data op_flags(op_long_i)           /op_is_arith/
  data op_flags(op_real_i)           /op_is_arith/
  data op_flags(op_double_i)         /op_is_arith/
  data op_flags(op_abs_i)            /op_is_arith/
  data op_flags(op_band_i)           /op_is_arith/
  data op_flags(op_bor_i)            /op_is_arith/
  data op_flags(op_bxor_i)           /op_is_arith/
  data op_flags(op_bshift_i)         /op_is_arith/
  data op_flags(op_bnot_i)           /op_is_arith/
  data op_flags(op_pdiff_i)          /op_is_arith/
  data op_flags(op_sign_i)           /op_is_arith/
  data op_flags(op_modulo_i)         /op_is_arith/
  data op_flags(op_i8_i)             /op_is_arith/
  data op_flags(op_i16_i)            /op_is_arith/
  data op_flags(op_i32_i)            /op_is_arith/
  data op_flags(op_i64_i)            /op_is_arith/
  data op_flags(op_offset_i)         /op_is_arith/

  data op_flags(op_add_ln)           /op_is_arith/
  data op_flags(op_sub_ln)           /op_is_arith/
  data op_flags(op_mult_ln)          /op_is_arith/
  data op_flags(op_divide_ln)        /op_is_arith/
  data op_flags(op_div_ln)           /op_is_arith/
  data op_flags(op_mod_ln)           /op_is_arith/
  data op_flags(op_pow_ln)           /op_is_arith/
  data op_flags(op_uminus_ln)        /op_is_arith/
  data op_flags(op_eq_ln)            /op_is_arith/
  data op_flags(op_ne_ln)            /op_is_arith/
  data op_flags(op_gt_ln)            /op_is_arith/
  data op_flags(op_ge_ln)            /op_is_arith/
  data op_flags(op_string_ln)        /op_is_arith/
  data op_flags(op_max_ln)           /op_is_arith/
  data op_flags(op_min_ln)           /op_is_arith/
  data op_flags(op_assign_ln)        /op_is_arith/
  data op_flags(op_int_ln)           /op_is_arith/
  data op_flags(op_real_ln)          /op_is_arith/
  data op_flags(op_double_ln)        /op_is_arith/
  data op_flags(op_abs_ln)           /op_is_arith/
  data op_flags(op_band_ln)          /op_is_arith/
  data op_flags(op_bor_ln)           /op_is_arith/
  data op_flags(op_bxor_ln)          /op_is_arith/
  data op_flags(op_bshift_ln)        /op_is_arith/
  data op_flags(op_bnot_ln)          /op_is_arith/
  data op_flags(op_pdiff_ln)         /op_is_arith/
  data op_flags(op_sign_ln)          /op_is_arith/
  data op_flags(op_modulo_ln)        /op_is_arith/
  data op_flags(op_i8_ln)            /op_is_arith/
  data op_flags(op_i16_ln)           /op_is_arith/
  data op_flags(op_i32_ln)           /op_is_arith/
  data op_flags(op_i64_ln)           /op_is_arith/
  data op_flags(op_offset_ln)        /op_is_arith/

  data op_flags(op_add_offset)       /op_is_arith/
  data op_flags(op_sub_offset)       /op_is_arith/
  data op_flags(op_mult_offset)      /op_is_arith/
  data op_flags(op_divide_offset)    /op_is_arith/
  data op_flags(op_div_offset)       /op_is_arith/
  data op_flags(op_mod_offset)       /op_is_arith/
  data op_flags(op_pow_offset)       /op_is_arith/
  data op_flags(op_uminus_offset)    /op_is_arith/
  data op_flags(op_eq_offset)        /op_is_arith/
  data op_flags(op_ne_offset)        /op_is_arith/
  data op_flags(op_gt_offset)        /op_is_arith/
  data op_flags(op_ge_offset)        /op_is_arith/
  data op_flags(op_string_offset)    /op_is_arith/
  data op_flags(op_max_offset)       /op_is_arith/
  data op_flags(op_min_offset)       /op_is_arith/
  data op_flags(op_assign_offset)    /op_is_arith/
  data op_flags(op_int_offset)       /op_is_arith/
  data op_flags(op_real_offset)      /op_is_arith/
  data op_flags(op_double_offset)    /op_is_arith/
  data op_flags(op_abs_offset)       /op_is_arith/
  data op_flags(op_band_offset)      /op_is_arith/
  data op_flags(op_bor_offset)       /op_is_arith/
  data op_flags(op_bxor_offset)      /op_is_arith/
  data op_flags(op_bshift_offset)    /op_is_arith/
  data op_flags(op_bnot_offset)      /op_is_arith/
  data op_flags(op_pdiff_offset)     /op_is_arith/
  data op_flags(op_sign_offset)      /op_is_arith/
  data op_flags(op_modulo_offset)    /op_is_arith/
  data op_flags(op_i8_offset)        /op_is_arith/
  data op_flags(op_i16_offset)       /op_is_arith/
  data op_flags(op_i32_offset)       /op_is_arith/
  data op_flags(op_i64_offset)       /op_is_arith/

  data op_flags(op_add_i8)           /op_is_arith/
  data op_flags(op_sub_i8)           /op_is_arith/
  data op_flags(op_mult_i8)          /op_is_arith/
  data op_flags(op_divide_i8)        /op_is_arith/
  data op_flags(op_div_i8)           /op_is_arith/
  data op_flags(op_mod_i8)           /op_is_arith/
  data op_flags(op_pow_i8)           /op_is_arith/
  data op_flags(op_uminus_i8)        /op_is_arith/
  data op_flags(op_eq_i8)            /op_is_arith/
  data op_flags(op_ne_i8)            /op_is_arith/
  data op_flags(op_gt_i8)            /op_is_arith/
  data op_flags(op_ge_i8)            /op_is_arith/
  data op_flags(op_string_i8)        /op_is_arith/
  data op_flags(op_max_i8)           /op_is_arith/
  data op_flags(op_min_i8)           /op_is_arith/
  data op_flags(op_assign_i8)        /op_is_arith/
  data op_flags(op_int_i8)           /op_is_arith/
  data op_flags(op_real_i8)          /op_is_arith/
  data op_flags(op_double_i8)        /op_is_arith/
  data op_flags(op_abs_i8)           /op_is_arith/
  data op_flags(op_band_i8)          /op_is_arith/
  data op_flags(op_bor_i8)           /op_is_arith/
  data op_flags(op_bxor_i8)          /op_is_arith/
  data op_flags(op_bshift_i8)        /op_is_arith/
  data op_flags(op_bnot_i8)          /op_is_arith/
  data op_flags(op_pdiff_i8)         /op_is_arith/
  data op_flags(op_sign_i8)          /op_is_arith/
  data op_flags(op_modulo_i8)        /op_is_arith/
  data op_flags(op_i16_i8)           /op_is_arith/
  data op_flags(op_i32_i8)           /op_is_arith/
  data op_flags(op_i64_i8)           /op_is_arith/
  data op_flags(op_offset_i8)        /op_is_arith/
  data op_flags(op_long_i8)          /op_is_arith/

  data op_flags(op_add_i16)          /op_is_arith/
  data op_flags(op_sub_i16)          /op_is_arith/
  data op_flags(op_mult_i16)         /op_is_arith/
  data op_flags(op_divide_i16)       /op_is_arith/
  data op_flags(op_div_i16)          /op_is_arith/
  data op_flags(op_mod_i16)          /op_is_arith/
  data op_flags(op_pow_i16)          /op_is_arith/
  data op_flags(op_uminus_i16)       /op_is_arith/
  data op_flags(op_eq_i16)           /op_is_arith/
  data op_flags(op_ne_i16)           /op_is_arith/
  data op_flags(op_gt_i16)           /op_is_arith/
  data op_flags(op_ge_i16)           /op_is_arith/
  data op_flags(op_string_i16)       /op_is_arith/
  data op_flags(op_max_i16)          /op_is_arith/
  data op_flags(op_min_i16)          /op_is_arith/
  data op_flags(op_assign_i16)       /op_is_arith/
  data op_flags(op_int_i16)          /op_is_arith/
  data op_flags(op_real_i16)         /op_is_arith/
  data op_flags(op_double_i16)       /op_is_arith/
  data op_flags(op_abs_i16)          /op_is_arith/
  data op_flags(op_band_i16)         /op_is_arith/
  data op_flags(op_bor_i16)          /op_is_arith/
  data op_flags(op_bxor_i16)         /op_is_arith/
  data op_flags(op_bshift_i16)       /op_is_arith/
  data op_flags(op_bnot_i16)         /op_is_arith/
  data op_flags(op_pdiff_i16)        /op_is_arith/
  data op_flags(op_sign_i16)         /op_is_arith/
  data op_flags(op_modulo_i16)       /op_is_arith/
  data op_flags(op_i8_i16)           /op_is_arith/
  data op_flags(op_i32_i16)          /op_is_arith/
  data op_flags(op_i64_i16)          /op_is_arith/
  data op_flags(op_offset_i16)       /op_is_arith/
  data op_flags(op_long_i16)         /op_is_arith/

  data op_flags(op_add_i32)          /op_is_arith/
  data op_flags(op_sub_i32)          /op_is_arith/
  data op_flags(op_mult_i32)         /op_is_arith/
  data op_flags(op_divide_i32)       /op_is_arith/
  data op_flags(op_div_i32)          /op_is_arith/
  data op_flags(op_mod_i32)          /op_is_arith/
  data op_flags(op_pow_i32)          /op_is_arith/
  data op_flags(op_uminus_i32)       /op_is_arith/
  data op_flags(op_eq_i32)           /op_is_arith/
  data op_flags(op_ne_i32)           /op_is_arith/
  data op_flags(op_gt_i32)           /op_is_arith/
  data op_flags(op_ge_i32)           /op_is_arith/
  data op_flags(op_string_i32)       /op_is_arith/
  data op_flags(op_max_i32)          /op_is_arith/
  data op_flags(op_min_i32)          /op_is_arith/
  data op_flags(op_assign_i32)       /op_is_arith/
  data op_flags(op_int_i32)          /op_is_arith/
  data op_flags(op_real_i32)         /op_is_arith/
  data op_flags(op_double_i32)       /op_is_arith/
  data op_flags(op_abs_i32)          /op_is_arith/
  data op_flags(op_band_i32)         /op_is_arith/
  data op_flags(op_bor_i32)          /op_is_arith/
  data op_flags(op_bxor_i32)         /op_is_arith/
  data op_flags(op_bshift_i32)       /op_is_arith/
  data op_flags(op_bnot_i32)         /op_is_arith/
  data op_flags(op_pdiff_i32)        /op_is_arith/
  data op_flags(op_sign_i32)         /op_is_arith/
  data op_flags(op_modulo_i32)       /op_is_arith/
  data op_flags(op_i8_i32)           /op_is_arith/
  data op_flags(op_i16_i32)          /op_is_arith/
  data op_flags(op_i64_i32)          /op_is_arith/
  data op_flags(op_offset_i32)       /op_is_arith/
  data op_flags(op_long_i32)         /op_is_arith/


  data op_flags(op_add_i64)          /op_is_arith/
  data op_flags(op_sub_i64)          /op_is_arith/
  data op_flags(op_mult_i64)         /op_is_arith/
  data op_flags(op_divide_i64)       /op_is_arith/
  data op_flags(op_div_i64)          /op_is_arith/
  data op_flags(op_mod_i64)          /op_is_arith/
  data op_flags(op_pow_i64)          /op_is_arith/
  data op_flags(op_uminus_i64)       /op_is_arith/
  data op_flags(op_eq_i64)           /op_is_arith/
  data op_flags(op_ne_i64)           /op_is_arith/
  data op_flags(op_gt_i64)           /op_is_arith/
  data op_flags(op_ge_i64)           /op_is_arith/
  data op_flags(op_string_i64)       /op_is_arith/
  data op_flags(op_max_i64)          /op_is_arith/
  data op_flags(op_min_i64)          /op_is_arith/
  data op_flags(op_assign_i64)       /op_is_arith/
  data op_flags(op_int_i64)          /op_is_arith/
  data op_flags(op_real_i64)         /op_is_arith/
  data op_flags(op_double_i64)       /op_is_arith/
  data op_flags(op_abs_i64)          /op_is_arith/
  data op_flags(op_band_i64)         /op_is_arith/
  data op_flags(op_bor_i64)          /op_is_arith/
  data op_flags(op_bxor_i64)         /op_is_arith/
  data op_flags(op_bshift_i64)       /op_is_arith/
  data op_flags(op_bnot_i64)         /op_is_arith/
  data op_flags(op_pdiff_i64)        /op_is_arith/
  data op_flags(op_sign_i64)         /op_is_arith/
  data op_flags(op_modulo_i64)       /op_is_arith/
  data op_flags(op_i8_i64)           /op_is_arith/
  data op_flags(op_i16_i64)          /op_is_arith/
  data op_flags(op_i32_i64)          /op_is_arith/
  data op_flags(op_offset_i8)        /op_is_arith/
  data op_flags(op_long_i8)          /op_is_arith/

  data op_flags(op_add_r)            /op_is_arith/
  data op_flags(op_sub_r)            /op_is_arith/
  data op_flags(op_mult_r)           /op_is_arith/
  data op_flags(op_divide_r)         /op_is_arith/
  data op_flags(op_div_r)            /op_is_arith/
  data op_flags(op_mod_r)            /op_is_arith/
  data op_flags(op_pow_r)            /op_is_arith/
  data op_flags(op_uminus_r)         /op_is_arith/
  data op_flags(op_eq_r)             /op_is_arith/
  data op_flags(op_ne_r)             /op_is_arith/
  data op_flags(op_gt_r)             /op_is_arith/
  data op_flags(op_ge_r)             /op_is_arith/
  data op_flags(op_string_r)         /op_is_arith/
  data op_flags(op_max_r)            /op_is_arith/
  data op_flags(op_min_r)            /op_is_arith/
  data op_flags(op_assign_r)         /op_is_arith/
  data op_flags(op_int_r)            /op_is_arith/
  data op_flags(op_long_r)           /op_is_arith/
  data op_flags(op_double_r)         /op_is_arith/
  data op_flags(op_abs_r)            /op_is_arith/
  data op_flags(op_acos_r)           /op_is_arith/
  data op_flags(op_asin_r)           /op_is_arith/
  data op_flags(op_atan_r)           /op_is_arith/
  data op_flags(op_atan2_r)          /op_is_arith/
  data op_flags(op_cos_r)            /op_is_arith/
  data op_flags(op_cosh_r)           /op_is_arith/
  data op_flags(op_exp_r)            /op_is_arith/
  data op_flags(op_log_r)            /op_is_arith/
  data op_flags(op_log10_r)          /op_is_arith/
  data op_flags(op_sin_r)            /op_is_arith/
  data op_flags(op_sinh_r)           /op_is_arith/
  data op_flags(op_sqrt_r)           /op_is_arith/
  data op_flags(op_tan_r)            /op_is_arith/
  data op_flags(op_tanh_r)           /op_is_arith/
  data op_flags(op_floor_r)          /op_is_arith/
  data op_flags(op_ceil_r)           /op_is_arith/
  data op_flags(op_modulo_r)         /op_is_arith/
  data op_flags(op_sign_r)           /op_is_arith/
  data op_flags(op_pdiff_r)          /op_is_arith/
  data op_flags(op_offset_r)         /op_is_arith/
  data op_flags(op_r32_r)            /op_is_arith/
  data op_flags(op_r64_r)            /op_is_arith/
  data op_flags(op_complex_r)        /op_is_arith/
  data op_flags(op_complex2_r)       /op_is_arith/

  data op_flags(op_add_d)            /op_is_arith/
  data op_flags(op_sub_d)            /op_is_arith/
  data op_flags(op_mult_d)           /op_is_arith/
  data op_flags(op_divide_d)         /op_is_arith/
  data op_flags(op_div_d)            /op_is_arith/
  data op_flags(op_mod_d)            /op_is_arith/
  data op_flags(op_pow_d)            /op_is_arith/
  data op_flags(op_uminus_d)         /op_is_arith/
  data op_flags(op_eq_d)             /op_is_arith/
  data op_flags(op_ne_d)             /op_is_arith/
  data op_flags(op_gt_d)             /op_is_arith/
  data op_flags(op_ge_d)             /op_is_arith/
  data op_flags(op_string_d)         /op_is_arith/
  data op_flags(op_max_d)            /op_is_arith/
  data op_flags(op_min_d)            /op_is_arith/
  data op_flags(op_assign_d)         /op_is_arith/
  data op_flags(op_int_d)            /op_is_arith/
  data op_flags(op_long_d)           /op_is_arith/
  data op_flags(op_real_d)           /op_is_arith/
  data op_flags(op_abs_d)            /op_is_arith/
  data op_flags(op_acos_d)           /op_is_arith/
  data op_flags(op_asin_d)           /op_is_arith/
  data op_flags(op_atan_d)           /op_is_arith/
  data op_flags(op_atan2_d)          /op_is_arith/
  data op_flags(op_cos_d)            /op_is_arith/
  data op_flags(op_cosh_d)           /op_is_arith/
  data op_flags(op_exp_d)            /op_is_arith/
  data op_flags(op_log_d)            /op_is_arith/
  data op_flags(op_log10_d)          /op_is_arith/
  data op_flags(op_sin_d)            /op_is_arith/
  data op_flags(op_sinh_d)           /op_is_arith/
  data op_flags(op_sqrt_d)           /op_is_arith/
  data op_flags(op_tan_d)            /op_is_arith/
  data op_flags(op_tanh_d)           /op_is_arith/
  data op_flags(op_floor_d)          /op_is_arith/
  data op_flags(op_ceil_d)           /op_is_arith/
  data op_flags(op_modulo_d)         /op_is_arith/
  data op_flags(op_sign_d)           /op_is_arith/
  data op_flags(op_pdiff_d)          /op_is_arith/
  data op_flags(op_offset_d)         /op_is_arith/
  data op_flags(op_r32_d)            /op_is_arith/
  data op_flags(op_r64_d)            /op_is_arith/
  data op_flags(op_complex_d)        /op_is_arith/
  data op_flags(op_complex2_d)       /op_is_arith/

  data op_flags(op_add_c)            /op_is_arith/
  data op_flags(op_sub_c)            /op_is_arith/
  data op_flags(op_mult_c)           /op_is_arith/
  data op_flags(op_divide_c)         /op_is_arith/
  data op_flags(op_rpow_c)           /op_is_arith/
  data op_flags(op_pow_c)            /op_is_arith/
  data op_flags(op_uminus_c)         /op_is_arith/
  data op_flags(op_eq_c)             /op_is_arith/
  data op_flags(op_ne_c)             /op_is_arith/
  data op_flags(op_string_c)         /op_is_arith/
  data op_flags(op_assign_c)         /op_is_arith/
  data op_flags(op_real_c)           /op_is_arith/
  data op_flags(op_abs_c)            /op_is_arith/
  data op_flags(op_acos_c)           /op_is_arith/
  data op_flags(op_asin_c)           /op_is_arith/
  data op_flags(op_atan_c)           /op_is_arith/
  data op_flags(op_atan2_c)          /op_is_arith/
  data op_flags(op_cos_c)            /op_is_arith/
  data op_flags(op_cosh_c)           /op_is_arith/
  data op_flags(op_exp_c)            /op_is_arith/
  data op_flags(op_log_c)            /op_is_arith/
  data op_flags(op_log10_c)          /op_is_arith/
  data op_flags(op_sin_c)            /op_is_arith/
  data op_flags(op_sinh_c)           /op_is_arith/
  data op_flags(op_sqrt_c)           /op_is_arith/
  data op_flags(op_tan_c)            /op_is_arith/
  data op_flags(op_tanh_c)           /op_is_arith/
  data op_flags(op_imag_c)           /op_is_arith/
  data op_flags(op_conj_c)           /op_is_arith/

  data op_flags(op_add_dc)           /op_is_arith/
  data op_flags(op_sub_dc)           /op_is_arith/
  data op_flags(op_mult_dc)          /op_is_arith/
  data op_flags(op_divide_dc)        /op_is_arith/
  data op_flags(op_dpow_dc)          /op_is_arith/
  data op_flags(op_pow_dc)           /op_is_arith/
  data op_flags(op_uminus_dc)        /op_is_arith/
  data op_flags(op_eq_dc)            /op_is_arith/
  data op_flags(op_ne_dc)            /op_is_arith/
  data op_flags(op_string_dc)        /op_is_arith/
  data op_flags(op_assign_dc)        /op_is_arith/
  data op_flags(op_real_dc)          /op_is_arith/
  data op_flags(op_abs_dc)           /op_is_arith/
  data op_flags(op_acos_dc)          /op_is_arith/
  data op_flags(op_asin_dc)          /op_is_arith/
  data op_flags(op_atan_dc)          /op_is_arith/
  data op_flags(op_atan2_dc)         /op_is_arith/
  data op_flags(op_cos_dc)           /op_is_arith/
  data op_flags(op_cosh_dc)          /op_is_arith/
  data op_flags(op_exp_dc)           /op_is_arith/
  data op_flags(op_log_dc)           /op_is_arith/
  data op_flags(op_log10_dc)         /op_is_arith/
  data op_flags(op_sin_dc)           /op_is_arith/
  data op_flags(op_sinh_dc)          /op_is_arith/
  data op_flags(op_sqrt_dc)          /op_is_arith/
  data op_flags(op_tan_dc)           /op_is_arith/
  data op_flags(op_tanh_dc)          /op_is_arith/
  data op_flags(op_imag_dc)          /op_is_arith/
  data op_flags(op_conj_dc)          /op_is_arith/

  data op_flags(op_do_loop)          /op_is_fixed/
  data op_flags(op_mask)             /0/
  data op_flags(op_if)               /op_2_blocks/
  data op_flags(op_if_shared)        /op_2_blocks/
  data op_flags(op_if_shared_node)   /op_2_blocks/
  data op_flags(op_if_restart)       /op_2_blocks/
  data op_flags(op_loop)             /op_1_block/
  data op_flags(op_comm_loop)        /op_1_block/
  data op_flags(op_comm_block)       /op_1_block/
  data op_flags(op_comm_proc)        /op_1_block/
  data op_flags(op_comm_inline)      /op_1_block/
  data op_flags(op_allocate)         /0/
  data op_flags(op_deallocate)       /0/
  data op_flags(op_set_extent)       /0/     ! Obsolete
  data op_flags(op_break_loop)       /op_precedes_loop/
  data op_flags(op_nested_loop)      /op_precedes_loop/
  data op_flags(op_blocked_loop)     /op_precedes_loop/
  data op_flags(op_over)             /op_1_block/          
  data op_flags(op_comm_loop_par)    /op_1_block/
  data op_flags(op_inline_shared)    /op_1_block/
  data op_flags(op_assign_farray)    /0/
  data op_flags(op_init_farray)      /0/
  data op_flags(op_wrap)             /0/
  data op_flags(op_sync)             /0/
  data op_flags(op_init_var)         /0/

  character(len=20),dimension(:),allocatable:: op_names

  
  ! **************************************
  ! For compiler wordcodes only
  ! **************************************
  
  ! Start of wordcodes in vector
  integer,parameter:: comp_op_start=1
  
  ! Displacements within a compiler wordcode
  integer,parameter:: comp_op_link=0
  integer,parameter:: comp_op_line=1
  integer,parameter:: comp_op_opcode=2
  integer,parameter:: comp_op_opcode2=3
  integer,parameter:: comp_op_nargs=4
  integer,parameter:: comp_op_arg0=5

  ! Decoding other information in comp_op_nargs slot (nret and flags)
  integer,parameter:: comp_op_nargs_mask=pm_max_args
  integer,parameter:: comp_op_nret_mask=(pm_max_args+1)*pm_max_args
  integer,parameter:: comp_op_nret_div=pm_max_args+1
  integer,parameter:: comp_op_flags=(pm_max_args+1)**2
  integer,parameter:: comp_op_shared=comp_op_flags

  ! Variable types (compiler only)
  integer,parameter:: v_is_undef=0
  integer,parameter:: v_is_basic=1
  integer,parameter:: v_is_group=2
  integer,parameter:: v_is_sub=3
  integer,parameter:: v_is_elem=4
  integer,parameter:: v_is_alias=5
  integer,parameter:: v_is_vsub=6
  integer,parameter:: v_is_const=7
  integer,parameter:: v_is_ve=8
  integer,parameter:: v_is_cove=9
  integer,parameter:: v_is_parve=10
  integer,parameter:: v_is_parstmt_ve=11
  integer,parameter:: v_is_ctime_const=12
  integer,parameter:: v_is_chan_vect=13
  integer,parameter:: v_is_unit_elem=14
  integer,parameter:: v_is_vect_wrapped=15

  integer,parameter:: cvar_flag_mask=31
  integer,parameter:: cvar_flag_mult=cvar_flag_mask+1
  integer,parameter:: modl_mult = 1024*64
  
  ! Variable flags (compiler only)
  integer,parameter:: v_is_used=1
  integer,parameter:: v_is_poly=2
  integer,parameter:: v_is_param=4
  integer,parameter:: v_is_shared=8
  integer,parameter:: v_is_ref=16
  integer,parameter:: v_is_result=32
  integer,parameter:: v_is_key=64
  integer,parameter:: v_is_par=128
  integer,parameter:: v_is_vect=256
  integer,parameter:: v_is_in_dref=512
  integer,parameter:: v_is_chan=1024
  integer,parameter:: v_is_array_par_vect=2048
  integer,parameter:: v_is_array_par_dom=4096
  integer,parameter:: v_is_farray=8192
 
  ! Variable group types (compiling only)
  integer,parameter:: v_is_var_array=0
  integer,parameter:: v_is_array=1
  integer,parameter:: v_is_struct=2
  integer,parameter:: v_is_dref=3
  integer,parameter:: v_is_shared_dref=4
  integer,parameter:: v_is_storageless=5
  integer,parameter:: v_is_tuple=6

  integer,parameter:: shared_op_flag=-32767
  
contains

  subroutine set_op_names
    integer:: i
    if(allocated(op_names)) return
    allocate(op_names(0:num_op+1))
    op_names='??'
    op_names(op_call)='call'
    op_names(op_comm_call)='comm_call'
    op_names(op_jmp)='jmp'
    op_names(op_jmp_any_ve)='jmp_any_ve'
    op_names(op_jmp_any_ve_par)='jmp_any_ve_par'
    op_names(op_and_ve)='and_ve'
    op_names(op_andnot_ve)='andnot_ve'
    op_names(op_and_jmp_any)='and_jmp_any'
    op_names(op_andnot_jmp_any)='andnot_jmp_any'
    op_names(op_reduce)='reduce'
    op_names(op_jmp_nopar)='jmp_nopar'
    op_names(op_jmp_noshare)='jmp_noshare'
    op_names(op_head_node)='head_node'
    op_names(op_skip_comms)='skip_comms'
    op_names(op_struct)='struct'
    op_names(op_rec)='rec'
    op_names(op_array)='array'
    op_names(op_array_noinit)='array_noinit'
    op_names(op_var_array)='var_array'
    op_names(op_get_dom)='get_dom'
    op_names(op_elem)='elem'
    op_names(op_elem_ref)='elem_ref'
    op_names(op_as)='as'
    op_names(op_make_poly)='make_poly'
    op_names(op_get_poly)='get_poly'
    op_names(op_get_poly2)='get_poly2'
    op_names(op_get_poly_or)='get_poly_or'
    op_names(op_any)='any'
    op_names(op_tag)='tag'
    op_names(op_tag_val)='tag_val'
    op_names(op_get_tag)='get_tag'
    op_names(op_get_size)='get_size'
    op_names(op_make_type_val)='make_type_val'
    op_names(op_import)='import'
    op_names(op_export)='export'
    op_names(op_export_param)='export_param'
    op_names(op_extract)='extract'
    op_names(op_return)='return'
    op_names(op_par_loop_end)='par_loop_end'
    op_names(op_par_find_end)='par_find_end'
    op_names(op_skip_empty)='skip_empty'
    op_names(op_skip_any)='skip_any'
    op_names(op_skip_empty_local)='skip_empty_local'
    op_names(op_check)='check'
    op_names(op_dump)='dump'
    op_names(op_print)='print'
    op_names(op_concat)='concat'
    op_names(op_lookup_error)='lookup_error'
    op_names(op_makekeys)='makekeys'
    op_names(op_delkeys)='delkeys'
    op_names(op_checkkeys)='checkkeys'
    op_names(op_getvkey)='getvkey'
    op_names(op_var_call)='var_call'
    op_names(op_make_dr)='make_dr'
    op_names(op_get_dr)='get_dr'
    op_names(op_set_dr)='set_dr'
    op_names(op_make_rf)='make_rf'
    op_names(op_get_rf)='get_rf'
    op_names(op_set_rf)='set_rf'
    op_names(op_check_logical)='check_logical'
    op_names(op_extract_first)='extract_first'
    op_names(op_chan_array_elem)='chan_array_elem'
    op_names(op_chan_array_vect)='chan_array_vect'

    op_names(op_array_get_elem)='array_get_elem'
    op_names(op_array_set_elem)='array_set_elem'
    op_names(op_array_elems)='array_elems'
    op_names(op_make_array)='make_array'
    op_names(op_import_val)='import_val'
    op_names(op_import_back)='import_back'
    op_names(op_import_varg)='import_varg'
    op_names(op_extractelm)='extractelm'
    op_names(op_iota)='iota'
    op_names(op_indices)='indices'
    op_names(op_get_key)='get_key'
    op_names(op_get_key2)='get_key2'
    op_names(op_export_array)='export_array'
    op_names(op_miss_arg)='miss_arg'
    op_names(op_default)='default'
    op_names(op_dump_id)='dump_id'
    op_names(op_import_scalar)='import_scalar'
    op_names(op_check_assign)='check_assign'
    op_names(op_same_type)='same_type'
    op_names(op_logical_return)='logical_return'
    op_names(op_redim)='redim'
    op_names(op_wshare)='wshare'
    op_names(op_block_cyclic)='block_cyclic'
    op_names(op_pack)='pack'
    op_names(op_dref)='dref'
    op_names(op_dref_elem)='dref_elem'
    op_names(op_import_dref)='import_dref'
    op_names(op_advance)='advance'
    op_names(op_advance_and)='advance_and'
    op_names(op_init_loop)='init_loop'

    op_names(op_intersect_seq)='intersect_seq'
    op_names(op_intersect_aseq)='intersect_aseq'
    op_names(op_expand_aseq)='expand_aseq'
    op_names(op_intersect_bseq)='intersect_bseq'
    op_names(op_includes_aseq)='includes_aseq'
    op_names(op_in_aseq)='in_aseq'
    op_names(op_index_aseq)='index_aseq'
    op_names(op_gcd)='gcd'
    op_names(op_chan)='chan'
    op_names(op_active)='active'
    op_names(op_new_dump)='new_dump'
    op_names(op_show)='show'
    op_names(op_show_stack)='show_stack'

    op_names(op_this_node)='this_node'
    op_names(op_this_nnode)='this_nnode'
    op_names(op_shared_node)='shared_node'
    op_names(op_shared_nnode)='shared_nnode'
    op_names(op_is_shared)='is_shared'
    op_names(op_is_par)='is_par'
    op_names(op_push_node_grid)='push_node_grid'
    op_names(op_push_node_split)='push_node_split'
    op_names(op_push_node_distr)='push_node_distr'
    op_names(op_pop_node_conc)='pop_node_conc'
    op_names(op_push_node_conc)='push_node_conc'
    op_names(op_pop_node)='pop_node'
    op_names(op_broadcast)='broadcast'
    op_names(op_get_remote)='get_remote'
    op_names(op_pop_off_node)='pop_off_node'
    op_names(op_push_node_back)='push_node_back'
    op_names(op_sys_node)='sys_node'
    op_names(op_sys_nnode)='sys_nnode'
    op_names(op_this_node)='this_node'
    op_names(op_this_nnode)='sys_nnode'
    op_names(op_reduce_ve)='reduce_ve'
    op_names(op_start_loop_sync)='start_loop_sync'
    op_names(op_broadcast_val)='broadcast_val'
    op_names(op_gather)='gather'
    op_names(op_isend_offset)='isend_offset'
    op_names(op_irecv_offset)='irecv_offset'
    op_names(op_sync_mess)='sync_mess'
    op_names(op_get_dims)='get_dims'
    op_names(op_put_remote)='put_remote'
    op_names(op_get_remote_distr)='get_remote_distr'
    op_names(op_put_remote_distr)='put_remote_distr'
    op_names(op_remote_call)='remote_call'
    op_names(op_remote_send_call)='remote_send_call'
    op_names(op_collect_call)='collect_call'
    op_names(op_server_call)='server_call'
    op_names(op_bcast_call)='bcast_call'
    op_names(op_isend_req)='isend_req'
    op_names(op_recv_reply)='recv_reply'
    op_names(op_recv_offset)='recv_offset'
    op_names(op_recv_req_call)='recv_req_call'
    op_names(op_recv_assn_call)='recv_assn_call'
    op_names(op_isend_assn)='isend_assn'
    op_names(op_isend)='isend'
    op_names(op_irecv)='irecv'
    op_names(op_recv)='recv'
    op_names(op_isend_reply)='isend_reply'
    op_names(op_do_at)='do_at'
    op_names(op_root_node)='root_node'
    op_names(op_bcast_shared_offset)='bcast_shared_offset'
    op_names(op_bcast_shared_grid)='bcast_shared_grid'
    op_names(op_broadcast_shared)='broadcast_shared'
    op_names(op_isend_grid)='isend_grid'
    op_names(op_irecv_grid)='irecv_grid'
    op_names(op_recv_grid)='recv_grid'
    op_names(op_recv_grid_resend)='recv_grid_resend'
    op_names(op_recv_offset_resend)='recv_offset_resend'

    op_names(op_open_file)='open_file'
    op_names(op_close_file)='close_file'
    op_names(op_seek_file)='seek_file'
    op_names(op_read_file)='read_file'
    op_names(op_write_file)='write_file'
    op_names(op_read_file_array)='read_file_array'
    op_names(op_write_file_array)='write_file_array'
    op_names(op_read_file_tile)='read_file_tile'
    op_names(op_write_file_tile)='write_file_tile'
    op_names(op_io_error_string)='io_error_string'

    op_names(op_par_loop)='par_loop'
    op_names(op_par_find)='par_find'
    op_names(op_setref)='setref'
    op_names(op_clone)='clone'
    op_names(op_clone_ve)='clone_ve'
    op_names(op_get_esize)='get_esize'
    op_names(op_nullify)='nullify'
    op_names(op_assign)='assign'
    op_names(op_fill)='fill'

    op_names(op_eq)='eq'
    op_names(op_ne)='ne'

    op_names(op_string_l)='string_l'
    op_names(op_and)='and'
    op_names(op_or)='or'
    op_names(op_not)='not'
    op_names(op_assign_l)='assign_l'
    op_names(op_eq_l)='eq_l'
    op_names(op_ne_l)='ne_l'

    op_names(op_add_i)='add_i'
    op_names(op_sub_i)='sub_i'
    op_names(op_mult_i)='mult_i'
    op_names(op_divide_i)='divide_i'
    op_names(op_div_i)='div_i'
    op_names(op_mod_i)='mod_i'
    op_names(op_pow_i)='pow_i'
    op_names(op_uminus_i)='uminus_i'
    op_names(op_eq_i)='eq_i'
    op_names(op_ne_i)='ne_i'
    op_names(op_gt_i)='gt_i'
    op_names(op_ge_i)='ge_i'
    op_names(op_string_i)='string_i'
    op_names(op_max_i)='max_i'
    op_names(op_min_i)='min_i'
    op_names(op_assign_i)='assign_i'
    op_names(op_long_i)='long_i'
    op_names(op_real_i)='real_i'
    op_names(op_double_i)='double_i'
    op_names(op_abs_i)='abs_i'
    op_names(op_band_i)='band_i'
    op_names(op_bor_i)='bor_i'
    op_names(op_bxor_i)='bxor_i'
    op_names(op_bshift_i)='bshift_i'
    op_names(op_bnot_i)='bnot_i'
    op_names(op_pdiff_i)='pdiff_i'
    op_names(op_sign_i)='sign_i'
    op_names(op_modulo_i)='modulo_i'
    op_names(op_i8_i)='i8_i'
    op_names(op_i16_i)='i16_i'
    op_names(op_i32_i)='i32_i'
    op_names(op_i64_i)='i64_i'
    op_names(op_offset_i)='offset_i'

    op_names(op_add_ln)='add_ln'
    op_names(op_sub_ln)='sub_ln'
    op_names(op_mult_ln)='mult_ln'
    op_names(op_divide_ln)='divide_ln'
    op_names(op_div_ln)='div_ln'
    op_names(op_mod_ln)='mod_ln'
    op_names(op_pow_ln)='pow_ln'
    op_names(op_uminus_ln)='uminus_ln'
    op_names(op_eq_ln)='eq_ln'
    op_names(op_ne_ln)='ne_ln'
    op_names(op_gt_ln)='gt_ln'
    op_names(op_ge_ln)='ge_ln'
    op_names(op_string_ln)='string_ln'
    op_names(op_max_ln)='max_ln'
    op_names(op_min_ln)='min_ln'
    op_names(op_assign_ln)='assign_ln'
    op_names(op_int_ln)='int_ln'
    op_names(op_real_ln)='real_ln'
    op_names(op_double_ln)='double_ln'
    op_names(op_abs_ln)='abs_ln'
    op_names(op_band_ln)='band_ln'
    op_names(op_bor_ln)='bor_ln'
    op_names(op_bxor_ln)='bxor_ln'
    op_names(op_bshift_ln)='bshift_ln'
    op_names(op_bnot_ln)='bnot_ln'
    op_names(op_pdiff_ln)='pdiff_ln'
    op_names(op_sign_ln)='sign_ln'
    op_names(op_modulo_ln)='modulo_ln'
    op_names(op_i8_ln)='i8_ln'
    op_names(op_i16_ln)='i16_ln'
    op_names(op_i32_ln)='i32_ln'
    op_names(op_i64_ln)='i64_ln'
    op_names(op_offset_ln)='offset_ln'

    op_names(op_add_offset)='add_offset'
    op_names(op_sub_offset)='sub_offset'
    op_names(op_mult_offset)='mult_offset'
    op_names(op_divide_offset)='divide_offset'
    op_names(op_div_offset)='div_offset'
    op_names(op_mod_offset)='mod_offset'
    op_names(op_pow_offset)='pow_offset'
    op_names(op_uminus_offset)='uminus_offset'
    op_names(op_eq_offset)='eq_offset'
    op_names(op_ne_offset)='ne_offset'
    op_names(op_gt_offset)='gt_offset'
    op_names(op_ge_offset)='ge_offset'
    op_names(op_string_offset)='string_offset'
    op_names(op_max_offset)='max_offset'
    op_names(op_min_offset)='min_offset'
    op_names(op_assign_offset)='assign_offset'
    op_names(op_int_offset)='int_offset'
    op_names(op_real_offset)='real_offset'
    op_names(op_double_offset)='double_offset'
    op_names(op_abs_offset)='abs_offset'
    op_names(op_band_offset)='band_offset'
    op_names(op_bor_offset)='bor_offset'
    op_names(op_bxor_offset)='bxor_offset'
    op_names(op_bshift_offset)='bshift_offset'
    op_names(op_bnot_offset)='bnot_offset'
    op_names(op_pdiff_offset)='pdiff_offset'
    op_names(op_sign_offset)='sign_offset'
    op_names(op_modulo_offset)='modulo_offset'
    op_names(op_i8_offset)='i8_offset'
    op_names(op_i16_offset)='i16_offset'
    op_names(op_i32_offset)='i32_offset'
    op_names(op_i64_offset)='i64_offset'

    op_names(op_add_i8)='add_i8'
    op_names(op_sub_i8)='sub_i8'
    op_names(op_mult_i8)='mult_i8'
    op_names(op_divide_i8)='divide_i8'
    op_names(op_div_i8)='div_i8'
    op_names(op_mod_i8)='mod_i8'
    op_names(op_pow_i8)='pow_i8'
    op_names(op_uminus_i8)='uminus_i8'
    op_names(op_eq_i8)='eq_i8'
    op_names(op_ne_i8)='ne_i8'
    op_names(op_gt_i8)='gt_i8'
    op_names(op_ge_i8)='ge_i8'
    op_names(op_string_i8)='string_i8'
    op_names(op_max_i8)='max_i8'
    op_names(op_min_i8)='min_i8'
    op_names(op_assign_i8)='assign_i8'
    op_names(op_int_i8)='int_i8'
    op_names(op_real_i8)='real_i8'
    op_names(op_double_i8)='double_i8'
    op_names(op_abs_i8)='abs_i8'
    op_names(op_band_i8)='band_i8'
    op_names(op_bor_i8)='bor_i8'
    op_names(op_bxor_i8)='bxor_i8'
    op_names(op_bshift_i8)='bshift_i8'
    op_names(op_bnot_i8)='bnot_i8'
    op_names(op_pdiff_i8)='pdiff_i8'
    op_names(op_sign_i8)='sign_i8'
    op_names(op_modulo_i8)='modulo_i8'
    op_names(op_i16_i8)='i16_i8'
    op_names(op_i32_i8)='i32_i8'
    op_names(op_i64_i8)='i64_i8'
    op_names(op_offset_i8)='offset_i8'
    op_names(op_long_i8)='long_i8'

    op_names(op_add_i16)='add_i16'
    op_names(op_sub_i16)='sub_i16'
    op_names(op_mult_i16)='mult_i16'
    op_names(op_divide_i16)='divide_i16'
    op_names(op_div_i16)='div_i16'
    op_names(op_mod_i16)='mod_i16'
    op_names(op_pow_i16)='pow_i16'
    op_names(op_uminus_i16)='uminus_i16'
    op_names(op_eq_i16)='eq_i16'
    op_names(op_ne_i16)='ne_i16'
    op_names(op_gt_i16)='gt_i16'
    op_names(op_ge_i16)='ge_i16'
    op_names(op_string_i16)='string_i16'
    op_names(op_max_i16)='max_i16'
    op_names(op_min_i16)='min_i16'
    op_names(op_assign_i16)='assign_i16'
    op_names(op_int_i16)='int_i16'
    op_names(op_real_i16)='real_i16'
    op_names(op_double_i16)='double_i16'
    op_names(op_abs_i16)='abs_i16'
    op_names(op_band_i16)='band_i16'
    op_names(op_bor_i16)='bor_i16'
    op_names(op_bxor_i16)='bxor_i16'
    op_names(op_bshift_i16)='bshift_i16'
    op_names(op_bnot_i16)='bnot_i16'
    op_names(op_pdiff_i16)='pdiff_i16'
    op_names(op_sign_i16)='sign_i16'
    op_names(op_modulo_i16)='modulo_i16'
    op_names(op_i8_i16)='i8_i16'
    op_names(op_i32_i16)='i32_i16'
    op_names(op_i64_i16)='i64_i16'
    op_names(op_offset_i16)='offset_i16'
    op_names(op_long_i16)='long_i16'


    op_names(op_add_i32)='add_i32'
    op_names(op_sub_i32)='sub_i32'
    op_names(op_mult_i32)='mult_i32'
    op_names(op_divide_i32)='divide_i32'
    op_names(op_div_i32)='div_i32'
    op_names(op_mod_i32)='mod_i32'
    op_names(op_pow_i32)='pow_i32'
    op_names(op_uminus_i32)='uminus_i32'
    op_names(op_eq_i32)='eq_i32'
    op_names(op_ne_i32)='ne_i32'
    op_names(op_gt_i32)='gt_i32'
    op_names(op_ge_i32)='ge_i32'
    op_names(op_string_i32)='string_i32'
    op_names(op_max_i32)='max_i32'
    op_names(op_min_i32)='min_i32'
    op_names(op_assign_i32)='assign_i32'
    op_names(op_int_i32)='int_i32'
    op_names(op_real_i32)='real_i32'
    op_names(op_double_i32)='double_i32'
    op_names(op_abs_i32)='abs_i32'
    op_names(op_band_i32)='band_i32'
    op_names(op_bor_i32)='bor_i32'
    op_names(op_bxor_i32)='bxor_i32'
    op_names(op_bshift_i32)='bshift_i32'
    op_names(op_bnot_i32)='bnot_i32'
    op_names(op_pdiff_i32)='pdiff_i32'
    op_names(op_sign_i32)='sign_i32'
    op_names(op_modulo_i32)='modulo_i32'
    op_names(op_i8_i32)='i8_i32'
    op_names(op_i16_i32)='i16_i32'
    op_names(op_i64_i32)='i64_i32'
    op_names(op_offset_i32)='offset_i32'
    op_names(op_long_i32)='long_i32'


    op_names(op_add_i64)='add_i64'
    op_names(op_sub_i64)='sub_i64'
    op_names(op_mult_i64)='mult_i64'
    op_names(op_divide_i64)='divide_i64'
    op_names(op_div_i64)='div_i64'
    op_names(op_mod_i64)='mod_i64'
    op_names(op_pow_i64)='pow_i64'
    op_names(op_uminus_i64)='uminus_i64'
    op_names(op_eq_i64)='eq_i64'
    op_names(op_ne_i64)='ne_i64'
    op_names(op_gt_i64)='gt_i64'
    op_names(op_ge_i64)='ge_i64'
    op_names(op_string_i64)='string_i64'
    op_names(op_max_i64)='max_i64'
    op_names(op_min_i64)='min_i64'
    op_names(op_assign_i64)='assign_i64'
    op_names(op_int_i64)='int_i64'
    op_names(op_real_i64)='real_i64'
    op_names(op_double_i64)='double_i64'
    op_names(op_abs_i64)='abs_i64'
    op_names(op_band_i64)='band_i64'
    op_names(op_bor_i64)='bor_i64'
    op_names(op_bxor_i64)='bxor_i64'
    op_names(op_bshift_i64)='bshift_i64'
    op_names(op_bnot_i64)='bnot_i64'
    op_names(op_pdiff_i64)='pdiff_i64'
    op_names(op_sign_i64)='sign_i64'
    op_names(op_modulo_i64)='modulo_i64'
    op_names(op_i8_i64)='i8_i64'
    op_names(op_i16_i64)='i16_i64'
    op_names(op_i32_i64)='i32_i64'
    op_names(op_offset_i8)='offset_i64'
    op_names(op_long_i8)='long_i64'

    op_names(op_add_r)='add_r'
    op_names(op_sub_r)='sub_r'
    op_names(op_mult_r)='mult_r'
    op_names(op_divide_r)='divide_r'
    op_names(op_div_r)='div_r'
    op_names(op_mod_r)='mod_r'
    op_names(op_pow_r)='pow_r'
    op_names(op_uminus_r)='uminus_r'
    op_names(op_eq_r)='eq_r'
    op_names(op_ne_r)='ne_r'
    op_names(op_gt_r)='gt_r'
    op_names(op_ge_r)='ge_r'
    op_names(op_string_r)='string_r'
    op_names(op_max_r)='max_r'
    op_names(op_min_r)='min_r'
    op_names(op_assign_r)='assign_r'
    op_names(op_int_r)='int_r'
    op_names(op_long_r)='long_r'
    op_names(op_double_r)='double_r'
    op_names(op_abs_r)='abs_r'
    op_names(op_acos_r)='acos_r'
    op_names(op_asin_r)='asin_r'
    op_names(op_atan_r)='atan_r'
    op_names(op_atan2_r)='atan2_r'
    op_names(op_cos_r)='cos_r'
    op_names(op_cosh_r)='cosh_r'
    op_names(op_exp_r)='exp_r'
    op_names(op_log_r)='log_r'
    op_names(op_log10_r)='log10_r'
    op_names(op_sin_r)='sin_r'
    op_names(op_sinh_r)='sinh_r'
    op_names(op_sqrt_r)='sqrt_r'
    op_names(op_tan_r)='tan_r'
    op_names(op_tanh_r)='tanh_r'
    op_names(op_floor_r)='floor_r'
    op_names(op_ceil_r)='ceil_r'
    op_names(op_modulo_r)='modulo_r'
    op_names(op_sign_r)='sign_r'
    op_names(op_pdiff_r)='pdiff_r'
    op_names(op_offset_r)='offset_r'
    op_names(op_r32_r)='r32_r'
    op_names(op_r64_r)='r64_r'
    op_names(op_complex_r)='complex_r'
    op_names(op_complex2_r)='complex2_r'

    op_names(op_add_d)='add_d'
    op_names(op_sub_d)='sub_d'
    op_names(op_mult_d)='mult_d'
    op_names(op_divide_d)='divide_d'
    op_names(op_div_d)='div_d'
    op_names(op_mod_d)='mod_d'
    op_names(op_pow_d)='pow_d'
    op_names(op_uminus_d)='uminus_d'
    op_names(op_eq_d)='eq_d'
    op_names(op_ne_d)='ne_d'
    op_names(op_gt_d)='gt_d'
    op_names(op_ge_d)='ge_d'
    op_names(op_string_d)='string_d'
    op_names(op_max_d)='max_d'
    op_names(op_min_d)='min_d'
    op_names(op_assign_d)='assign_d'
    op_names(op_int_d)='int_d'
    op_names(op_long_d)='long_d'
    op_names(op_real_d)='real_d'
    op_names(op_abs_d)='abs_d'
    op_names(op_acos_d)='acos_d'
    op_names(op_asin_d)='asin_d'
    op_names(op_atan_d)='atan_d'
    op_names(op_atan2_d)='atan2_d'
    op_names(op_cos_d)='cos_d'
    op_names(op_cosh_d)='cosh_d'
    op_names(op_exp_d)='exp_d'
    op_names(op_log_d)='log_d'
    op_names(op_log10_d)='log10_d'
    op_names(op_sin_d)='sin_d'
    op_names(op_sinh_d)='sinh_d'
    op_names(op_sqrt_d)='sqrt_d'
    op_names(op_tan_d)='tan_d'
    op_names(op_tanh_d)='tanh_d'
    op_names(op_floor_d)='floor_d'
    op_names(op_ceil_d)='ceil_d'
    op_names(op_modulo_d)='modulo_d'
    op_names(op_sign_d)='sign_d'
    op_names(op_pdiff_d)='pdiff_d'
    op_names(op_offset_d)='offset_d'
    op_names(op_r32_d)='r32_d'
    op_names(op_r64_d)='r64_d'
    op_names(op_complex_d)='complex_d'
    op_names(op_complex2_d)='complex2_d'

    op_names(op_add_c)='add_c'
    op_names(op_sub_c)='sub_c'
    op_names(op_mult_c)='mult_c'
    op_names(op_divide_c)='divide_c'
    op_names(op_rpow_c)='rpow_c'
    op_names(op_pow_c)='pow_c'
    op_names(op_uminus_c)='uminus_c'
    op_names(op_eq_c)='eq_c'
    op_names(op_ne_c)='ne_c'
    op_names(op_string_c)='string_c'
    op_names(op_assign_c)='assign_c'
    op_names(op_real_c)='real_c'
    op_names(op_abs_c)='abs_c'
    op_names(op_acos_c)='acos_c'
    op_names(op_asin_c)='asin_c'
    op_names(op_atan_c)='atan_c'
    op_names(op_atan2_c)='atan2_c'
    op_names(op_cos_c)='cos_c'
    op_names(op_cosh_c)='cosh_c'
    op_names(op_exp_c)='exp_c'
    op_names(op_log_c)='log_c'
    op_names(op_log10_c)='log10_c'
    op_names(op_sin_c)='sin_c'
    op_names(op_sinh_c)='sinh_c'
    op_names(op_sqrt_c)='sqrt_c'
    op_names(op_tan_c)='tan_c'
    op_names(op_tanh_c)='tanh_c'
    op_names(op_imag_c)='imag_c'
    op_names(op_conj_c)='conj_c'

    op_names(op_add_dc)='add_dc'
    op_names(op_sub_dc)='sub_dc'
    op_names(op_mult_dc)='mult_dc'
    op_names(op_divide_dc)='divide_dc'
    op_names(op_dpow_dc)='dpow_dc'
    op_names(op_pow_dc)='pow_dc'
    op_names(op_uminus_dc)='uminus_dc'
    op_names(op_eq_dc)='eq_dc'
    op_names(op_ne_dc)='ne_dc'
    op_names(op_string_dc)='string_dc'
    op_names(op_assign_dc)='assign_dc'
    op_names(op_real_dc)='real_dc'
    op_names(op_abs_dc)='abs_dc'
    op_names(op_acos_dc)='acos_dc'
    op_names(op_asin_dc)='asin_dc'
    op_names(op_atan_dc)='atan_dc'
    op_names(op_atan2_dc)='atan2_dc'
    op_names(op_cos_dc)='cos_dc'
    op_names(op_cosh_dc)='cosh_dc'
    op_names(op_exp_dc)='exp_dc'
    op_names(op_log_dc)='log_dc'
    op_names(op_log10_dc)='log10_dc'
    op_names(op_sin_dc)='sin_dc'
    op_names(op_sinh_dc)='sinh_dc'
    op_names(op_sqrt_dc)='sqrt_dc'
    op_names(op_tan_dc)='tan_dc'
    op_names(op_tanh_dc)='tanh_dc'
    op_names(op_imag_dc)='imag_dc'
    op_names(op_conj_dc)='conj_dc'

    op_names(op_do_loop)='do_loop'
    op_names(op_mask)='mask'    
    op_names(op_if)='if'
    op_names(op_if_shared)='if_shared'
    op_names(op_if_shared_node)='if_shared_node'
    op_names(op_if_restart)='if_restart'
    op_names(op_loop)='loop'
    op_names(op_comm_loop)='comm_loop'
    op_names(op_comm_block)='comm_block'
    op_names(op_comm_proc)='comm_proc'
    op_names(op_comm_inline)='comm_inline'
    op_names(op_allocate)='allocate'
    op_names(op_deallocate)='deallocate'
    op_names(op_set_extent)='set_extent'
    op_names(op_break_loop)='break_loop'
    op_names(op_nested_loop)='nested_loop'
    op_names(op_blocked_loop)='blocked_loop'
    op_names(op_over)='over'
    op_names(op_comm_loop_par)='comm_loop_par'
    op_names(op_inline_shared)='inline_shared'
    op_names(op_assign_farray)='assign_farray'
    op_names(op_init_farray)='init_farray'
    op_names(op_wrap)='wrap'
    op_names(op_sync)='sync'
    op_names(op_init_var)='init_var'

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

  subroutine print_comp_procs(context,iunit,funcs)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: funcs
    type(pm_ptr):: p,q,qq
    integer(pm_ln):: idx
    do idx=1_pm_ln,pm_dict_size(context,funcs)
       p=pm_dict_val(context,funcs,idx)
       if(pm_fast_isnull(p)) cycle
       q=p%data%ptr(p%offset)
       qq=p%data%ptr(p%offset+1)
       call print_comp_proc(context,iunit,q%data%i(q%offset+2),int(idx),&
            q%data%i(q%offset),q%data%i(q%offset+3),q%data%i(q%offset+1),&
            q%data%i(q%offset+4:),1,qq%data%i(qq%offset:),context%funcs,p%data%ptr(p%offset:),2,.true.)
    enddo
  contains
    include 'fisnull.inc'
  end subroutine print_comp_procs

  subroutine print_comp_proc(context,iunit,name,index,rvar,vevar,pvar,&
       op,first_index,vars,dict,values,depth,masked)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    integer,dimension(*):: op,vars
    integer,intent(in):: name,index,rvar,vevar,pvar,first_index,depth
    type(pm_ptr),intent(in):: dict
    type(pm_ptr),dimension(*),intent(in):: values
    logical,intent(in):: masked

    character(len=wcode_file_cols):: line
    integer:: i

    line='proc '//trim(pm_name_as_string(context,name))//'#'//trim(pm_int_as_string(index-1))
    i=len_trim(line)+1
    if(rvar>0) then
       call print_cvar(context,iunit,vars,rvar,values,.true.,depth,line,i)
       call append_to(iunit,line,i,' <- ',.false.,depth)
    endif
    if(vevar>0) then
       call print_cvar(context,iunit,vars,max(vevar,0),values,.true.,depth,line,i)
    endif
    if(pvar>0) then
       call print_cvar(context,iunit,vars,max(pvar,0),values,.true.,depth,line,i)
       call append_to(iunit,line,i,'{',.true.,depth)
    else
       call append_to(iunit,line,i,'{',.true.,depth)
    endif
    call print_comp_op_block(context,iunit,op,first_index,vars,dict,values,2,masked)
    write(iunit,'(a)') '}'
    
  end subroutine print_comp_proc

  subroutine print_comp_op(context,iunit,op,index,vars,dict,values,depth,masked)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    integer,dimension(*):: op,vars
    integer,intent(in):: index,depth
    type(pm_ptr),intent(in):: dict
    type(pm_ptr),dimension(*),intent(in):: values
    logical,intent(in):: masked
    character(len=wcode_file_cols):: line
    character(len=wcode_file_cols):: location
    integer:: nargs,nret,opcode,opcode2,flags,last_arg,arg1,i,nblocks,j
    logical:: shared
    type(pm_ptr):: p
    opcode=op(comp_op_opcode+index)
    opcode2=op(comp_op_opcode2+index)

    flags=op_flags(opcode)
    nargs=iand(op(comp_op_nargs+index),comp_op_nargs_mask)
    nret=iand(op(comp_op_nargs+index),comp_op_nret_mask)/comp_op_nret_div
    shared=iand(op(comp_op_nargs+index),comp_op_shared)/=0
    location=trim(pm_name_as_string(context,iand(op(index+comp_op_line),modl_mult-1)))//&
         ':'//pm_int_as_string(op(index+comp_op_line)/modl_mult)

!    write(*,*) 'NAME=',iand(op(index+comp_op_line),modl_mult-1)
!!$    write(*,*) 'op->',op_names(opcode),opcode,opcode2,op(index+comp_op_nargs),&
!!$         (op(index+comp_op_arg0+i),i=1,nargs-1),'#',nret
    
    if(iand(flags,op_1_block)/=0) then
       nblocks=1
    elseif(iand(flags,op_2_blocks)/=0) then
       nblocks=2
    else
       nblocks=0
    endif

    if(masked.and.op(index+comp_op_arg0)>0) then
       line=' '
       j=depth
       call print_cvar(context,iunit,vars,op(index+comp_op_arg0),values,.false.,depth,line,j)
       call append_to(iunit,line,j,merge(' ?!',' ? ',shared)//trim(op_names(opcode)),.false.,depth)
       j=j+1
    else
       line=repeat(' ',depth-1)//merge('!',' ',shared)//&
            op_names(opcode)
       j=depth+1+len_trim(op_names(opcode))
    endif
    if(opcode2/=0) then
       if(iand(flags,op_takes_type)/=0) then
          continue
       elseif(iand(flags,op_is_call)/=0) then
          p=pm_dict_val(context,dict,int(opcode2+1,pm_ln))
          p=p%data%ptr(p%offset)
          call append_to(iunit,line,j,' /'//&
               trim(pm_name_as_string(context,p%data%i(p%offset+2)))//'#'//&
               trim(pm_int_as_string(opcode2))//'/ ',.false.,depth)
       else
          call append_to(iunit,line,j,' /'//trim(pm_int_as_string(opcode2))//'/ ',.false.,depth)
       endif
    endif
    do i=1+nblocks,nargs-1
       call print_cvar(context,iunit,vars,op(index+comp_op_arg0+i),values,.true.,depth,line,j)
       if(i==nblocks+nret) then
          call append_to(iunit,line,j,' <- ',.false.,depth)
       elseif(i<nargs-1) then
          call append_to(iunit,line,j,' ',.false.,depth)
       endif
    enddo
    if(nblocks>0) call append_to(iunit,line,j,' {',.false.,depth)
    if(line(len(line)-len_trim(location):)/=' ') then
       write(iunit,'(a)') line
       line=' '
    endif
    line(len(line)-len_trim(location)+1:)=location
    write(iunit,'(a)') line
    if(nblocks>0) then
       call print_comp_op_block(context,iunit,op,op(index+comp_op_arg0+1),&
            vars,dict,values,min(20,depth+2),masked)
       if(nblocks>1) then
          write(iunit,'(a)') repeat(' ',depth)//'} --- {'
          call print_comp_op_block(context,iunit,op,op(index+comp_op_arg0+2),&
               vars,dict,values,min(20,depth+2),masked)
       endif
       write(iunit,'(a)') repeat(' ',depth)//'}'
    endif
  end subroutine print_comp_op

  subroutine print_comp_op_block(context,iunit,op,index,vars,dict,values,depth,masked)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    integer,dimension(*),intent(in):: op,vars
    type(pm_ptr),intent(in):: dict
    type(pm_ptr),dimension(*),intent(in):: values
    integer,intent(in):: index,depth
    logical,intent(in):: masked
    integer::i
    !write(*,*) 'BLOCK>',index
    i=index
    do while(i>0)
       call  print_comp_op(context,iunit,op,i,vars,dict,values,depth,masked)
       i=op(i+comp_op_link)
    enddo
  end subroutine print_comp_op_block

  subroutine print_cvar(context,iunit,var,index,values,addtype,depth,str,i)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    integer,dimension(*),intent(in):: var
    integer,intent(in):: index,depth
    type(pm_ptr),dimension(*),intent(in):: values 
    logical,intent(in):: addtype
    character(len=*),intent(inout):: str
    integer,intent(inout):: i

    call printv(index,addtype)

  contains

    recursive subroutine printv(index,addtype)
      integer,intent(in):: index
      logical,intent(in):: addtype

      integer:: kind,v1,v2,tno,tno2,tk,ename
      logical:: is_shared

      if(index<0) then
         call append('&')
         call printv(-index,addtype)
         return
      elseif(index==0) then
         call append('^')
         return
      end if

      !write(*,*) 'index=',index

      kind=iand(var(index),cvar_flag_mask)
      v1=var(index)/cvar_flag_mult
      v2=var(index+1)/cvar_flag_mult
      tno=var(index+2)/cvar_flag_mult
      select case(kind)
      case(v_is_undef)
         call append('??')
      case(v_is_basic)
         is_shared=iand(v2,v_is_shared)/=0
         if(v1/=0) then
            if(.not.append_if_name(trim(pm_name_as_string(context,v1))//&
                 merge('!','#',is_shared)//&
                 pm_int_as_string(index))) then
               call append(merge('!','#',is_shared)//&
                    pm_int_as_string(index))
            endif
         else
            call append(merge('!','#',is_shared)//&
                 pm_int_as_string(index))
         endif
      case(v_is_sub)
         call printv(v1,.false.)
         call append('[')
         call printv(v2,.false.)
         call append(']')
      case(v_is_vsub)
         call printv(v1,.false.)
         call append('[:')
         call printv(v2,.false.)
         call append(']')
      case(v_is_elem)
         call append('(')
         call printv(v1,.true.)
         call append(')')
         tno2=var(v1+2)/cvar_flag_mult
         tk=pm_typ_kind(context,tno2)
         if(tk==pm_typ_is_struct.or.tk==pm_typ_is_rec) then
            ename=abs(pm_typ_elem_name(context,tno2,v2))
            if(ename>=sym_d1.and.ename<=sym_d7) then
               call append('.'//pm_int_as_string(v2))
            else
               call append('.'//pm_name_as_string(context,ename))
            endif
         else
            call append('.'//pm_int_as_string(v2))
         endif
      case(v_is_alias)
         call append('*')
         call printv(v1,.false.)
      case(v_is_const)
         call append(pm_value_as_string(context,values(v1+3)))
      case(v_is_ve)
         call append('$'//pm_int_as_string(index))
      case(v_is_cove)
         call append('$~'//pm_int_as_string(v2))
      case(v_is_parve)
         call append('$@'//pm_int_as_string(index))
      case(v_is_parstmt_ve)
         call append('$@@'//pm_int_as_string(index))
      case(v_is_ctime_const)
         call append("'"//pm_value_as_string(context,values(v1+3)))
      case(v_is_chan_vect)
         call append('%')
         call printv(v1,.false.)
      case(v_is_unit_elem)
         call printv(v1,.false.)
         call append('.1')
      case(v_is_vect_wrapped)
         call append('%')
         call printv(v1,.false.)
      case(v_is_group)
         select case(v2)
         case(v_is_var_array)
            call group(index,v1,v2,'<:','>',.false.)
         case(v_is_array)
            call group(index,v1,v2,'<','>',.false.)
         case(v_is_struct)
            tk=pm_typ_kind(context,tno)
            if(tk==pm_typ_is_struct.or.tk==pm_typ_is_rec) then
               call group(index,v1,v2,trim(pm_name_as_string(context,pm_typ_elem_name(context,tno,0)))//&
                    '{','}',.false.)
            else
               call group(index,v1,v2,'{','}',.true.)
            endif
         case(v_is_dref)
            call group(index,v1,v2,'^(',')',.true.)
            return
         case(v_is_shared_dref)
            call group(index,v1,v2,'^!(',')',.true.)
            return
         case(v_is_storageless)
            call append('()')
         case(v_is_tuple)
            call group(index,v1,v2,'(',')',.true.)
            return
         case default
            call append('?g?'//pm_int_as_string(v2))
         end select
      case default
         call append('???'//pm_int_as_string(kind))
      end select
      if(addtype) then
         call append(':'//pm_typ_as_string(context,tno))
      endif

    end subroutine printv
    
    subroutine group(index,v1,v2,start,finish,add_type)
      integer,intent(in):: index,v1,v2
      character(len=*),intent(in):: start,finish
      logical,intent(in):: add_type
      integer:: j
      !write(*,*) '(',var(index+3:index+2+v1)/cvar_flag_mult,')'
      call append(start)
      call printv(var(index+3)/cvar_flag_mult,add_type)
      do j=2,v1
         call append(',')
         call printv(var(index+2+j)/cvar_flag_mult,add_type)
      enddo
      call append(finish)
    end subroutine group

    function append_if_name(part) result(ok)
      character(len=*),intent(in):: part
      logical:: ok
      if(part(1:1)=='_'.or.iachar(part(1:1))>=iachar('a').and.iachar(part(1:1))<=iachar('z').or.&
           iachar(part(1:1))>=iachar('A').and.iachar(part(1:1))<=iachar('Z')) then
         call append(part)
         ok=.true.
      else
         ok=.false.
      endif
    end function append_if_name
    
    subroutine append(part)
      character(len=*),intent(in):: part
      integer:: n
      n=len_trim(part)
      if(i+n>len(str)) then
         write(iunit,'(a)') str
         str=repeat(' ',depth+1)//part
         i=min(len(str),depth+1+n)
      else
         str(i+1:i+n)=part(1:n)
         i=i+n
      endif
    end subroutine append
      
  end subroutine print_cvar

  subroutine append_to(iunit,str,i,part,break,depth)
    integer,intent(in):: iunit
    character(len=*),intent(inout):: str
    integer,intent(inout):: i
    character(len=*),intent(in):: part
    logical,intent(in):: break
    integer,intent(in):: depth
    integer:: n
    n=len(part)
    if(i+n>len(str)) then
       write(iunit,'(a)') str(1:i)
       str=repeat(' ',depth+1)//part
       i=depth+1+n
    else
       str(i+1:i+n)=part(1:n)
       i=i+n
    endif
    if(break) write(iunit,'(a)') str(1:i)
  end subroutine append_to
  
end module pm_vmdefs


