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

! Symbols and names
module pm_symbol
  use pm_sysdep
  use pm_compbase
  use pm_kinds
  use pm_memory
  use pm_hash
  use pm_options
  implicit none
  
  ! ============================================================
  ! These constants represent keywords, other symbols and 
  ! also represent a range of abstract syntax node types
  ! ============================================================

  ! End of file
  integer,parameter:: sym_eof =0

  ! Non-operator symbols
  integer,parameter:: sym_at = 1
  integer,parameter:: sym_dollar = 2
  integer,parameter:: sym_semi = 3
  integer,parameter:: sym_open_square = 4
  integer,parameter:: sym_close_square = 5
  integer,parameter:: sym_open_brace = 6
  integer,parameter:: sym_close_brace = 7
  integer,parameter:: sym_colon = 8
  integer,parameter:: sym_dotdotdot = 9
  integer,parameter:: sym_ddollar = 10
  integer,parameter:: sym_comma = 11
  integer,parameter:: sym_dot = 12
  integer,parameter:: sym_assign = 13
  integer,parameter:: sym_underscore = 14 
  integer,parameter:: sym_open_attr = 15
  integer,parameter:: sym_close_attr = 16
  integer,parameter:: sym_query = 17
  integer,parameter:: sym_arrow = 18
  integer,parameter:: sym_pct = 19
  integer,parameter:: sym_dash = 20
  integer,parameter:: sym_caret = 21
  integer,parameter:: sym_dcaret = 22
  integer,parameter:: sym_dcolon = 23
  integer,parameter:: sym_define = 24
  integer,parameter:: sym_cond = 25
  integer,parameter:: sym_string = 26
  integer,parameter:: sym_number = 27

  ! Operators
  integer,parameter:: sym1 = sym_number
  integer,parameter:: sym_open = sym1 + 1
  integer,parameter:: sym_close = sym1 + 2
  integer,parameter:: sym_le = sym1 + 3
  integer,parameter:: sym_lt = sym1 + 4
  integer,parameter:: sym_ustar = sym1 + 5
  integer,parameter:: sym_uhash = sym1 + 6
  
  integer,parameter:: sym_from_range = sym1 + 7
  integer,parameter:: first_operator = sym_from_range
  integer,parameter:: sym_to_range = sym1 + 8
  integer,parameter:: sym2 = sym_to_range
  integer,parameter:: sym_concat = sym2 + 1
  integer,parameter:: sym_eq = sym2 + 2
  integer,parameter:: sym_ne = sym2 + 3
  integer,parameter:: sym_ge = sym2 + 4
  integer,parameter:: sym_gt = sym2 + 5
  integer,parameter:: sym_plus = sym2 + 6
  integer,parameter:: sym_minus = sym2 + 7
  integer,parameter:: sym_mult = sym2 + 8
  integer,parameter:: sym_divide = sym2 + 9
  integer,parameter:: sym_pow = sym2 + 10
  integer,parameter:: sym_dotdot = sym2 + 11
  integer,parameter:: sym_bar = sym2 + 12
  integer,parameter:: sym_hash= sym2 + 13
  integer,parameter:: sym_amp = sym2 + 14
  integer,parameter:: sym_pling = sym2 + 15

  ! These keywords and symbols are binary operators
  integer,parameter:: first_key = sym_pling
  integer,parameter:: sym_in = first_key + 1
  integer,parameter:: sym_and = first_key + 2
  integer,parameter:: sym_or = first_key + 3
  integer,parameter:: sym_xor = first_key + 4
  integer,parameter:: sym_shift = first_key + 5
  integer,parameter:: sym_fmt = first_key + 6
  integer,parameter:: sym_by = first_key + 7
  integer,parameter:: sym_mod = first_key + 8
  integer,parameter:: sym_except = first_key + 9
  integer,parameter:: sym_includes = first_key + 10
  integer,parameter:: sym_ortho = first_key + 11
  integer,parameter:: sym_is = first_key + 12
  integer,parameter:: sym_as = first_key + 13

  ! Unary operators 
  integer,parameter:: sym_not = first_key + 14
  integer,parameter:: last_operator = sym_not

  ! Statement / expression general keywords
  integer,parameter:: sym_null = last_operator + 1
  integer,parameter:: sym_key = last_operator + 2
  integer,parameter:: sym_arg = last_operator + 3
  integer,parameter:: sym_true = last_operator + 4
  integer,parameter:: sym_false = last_operator + 5
  integer,parameter:: sym_struct = last_operator + 6
  integer,parameter:: sym_rec = last_operator + 7
  integer,parameter:: sym_any = last_operator + 8
  integer,parameter:: sym_present = last_operator + 9
  integer,parameter:: sym_unique = last_operator + 10
  integer,parameter:: sym_fix = last_operator + 11
  integer,parameter:: sym_new = last_operator + 12
  integer,parameter:: sym_of = last_operator + 13
  integer,parameter:: sym_bounds = last_operator + 14
  integer,parameter:: last_expr = sym_bounds
  integer,parameter:: last_word = last_expr

  ! Modes
  integer,parameter:: sym_private  =      last_word + 1
  integer,parameter:: first_mode = sym_private
  integer,parameter:: sym_invar    =      last_word + 2
  integer,parameter:: sym_complete =      last_word + 3
  integer,parameter:: sym_universal=      last_word + 4
  integer,parameter:: sym_local    =      last_word + 5
  integer,parameter:: sym_partial  =      last_word + 6
  integer,parameter:: sym_coherent =      last_word + 7
  integer,parameter:: sym_chan     =      last_word + 8
  integer,parameter:: sym_mirrored =      last_word + 9
  integer,parameter:: sym_shared   =      last_word + 10
  integer,parameter:: last_mode =         sym_shared
  integer,parameter:: last_key =          sym_shared
  
  ! Declaration keywords
  integer,parameter:: sym_use = last_key + 1
  integer,parameter:: first_decl = sym_use
  integer,parameter:: sym_proc = last_key + 2
  integer,parameter:: sym_param = last_key + 3
  integer,parameter:: sym_type = last_key + 4
  integer,parameter:: sym_render = last_key + 5
  integer,parameter:: last_decl = sym_render

  ! Statement keywords 
  ! -- anything that can follow the end of a statement
  integer,parameter:: first_stmt = last_decl +1
  integer,parameter:: sym_check = last_decl + 1
  integer,parameter:: sym_over  = last_decl + 2
  integer,parameter:: sym_else = last_decl + 3
  integer,parameter:: sym_elseif = last_decl + 4 
  integer,parameter:: sym_case = last_decl + 5
  integer,parameter:: sym_for = last_decl + 6
  integer,parameter:: sym_if = last_decl + 7
  integer,parameter:: sym_par = last_decl + 8 
  integer,parameter:: sym_switch = last_decl + 9
  integer,parameter:: sym_until = last_decl + 10
  integer,parameter:: sym_using = last_decl + 11
  integer,parameter:: sym_sync = last_decl + 12      
  integer,parameter:: sym_while = last_decl + 13
  integer,parameter:: sym_return = last_decl + 14
  integer,parameter:: sym_also = last_decl + 15
  integer,parameter:: sym_do = last_decl + 16
  integer,parameter:: sym_nhd = last_decl + 17
  integer,parameter:: sym_test = last_decl + 18
  integer,parameter:: sym_default = last_decl + 19
  integer,parameter:: sym_task = last_decl + 20
  integer,parameter:: sym_extern = last_decl + 21
  integer,parameter:: sym_var = last_decl + 22
  integer,parameter:: sym_const = last_decl + 23
  integer,parameter:: sym_each = last_decl + 24
  integer,parameter:: sym_where = last_decl + 25
  integer,parameter:: sym_with = last_decl + 26
  integer,parameter:: sym_conc = last_decl + 27
  integer,parameter:: sym_interface = last_decl + 28
  integer,parameter:: sym_if_invar = last_decl + 29
  integer,parameter:: sym_while_invar = last_decl + 30
  integer,parameter:: sym_until_invar = last_decl + 31
  integer,parameter:: sym_foreach_invar = last_decl + 32
  integer,parameter:: sym_switch_invar = last_decl + 33
  integer,parameter:: sym_proceed= last_decl + 34
  integer,parameter:: sym_after= last_decl + 35
  integer,parameter:: sym_any_invar= last_decl + 36
  integer,parameter:: last_resv = sym_any_invar

  ! Names used by internal system
  integer,parameter:: sym_pm_send = last_resv + 1
  integer,parameter:: sym_pm_recv = last_resv + 2
  integer,parameter:: sym_pm_collect = last_resv + 3
  integer,parameter:: sym_pm_serve = last_resv + 4 
  integer,parameter:: sym_pm_bcast = last_resv + 5
  integer,parameter:: sym_pm_recv_req = last_resv + 6
  integer,parameter:: sym_pm_recv_assn = last_resv + 7
  
  integer,parameter:: sym_pm_dref = last_resv + 8
  integer,parameter:: sym_pm_dref_shared = last_resv + 9
  integer,parameter:: sym_pm_dref_slice = last_resv + 10
  integer,parameter:: sym_pm_dref_shared_slice = last_resv + 11
  integer,parameter:: sym_pm_dref_here = last_resv + 12
  integer,parameter:: sym_pm_ref = last_resv + 13
  integer,parameter:: sym_pm_head_node = last_resv + 14
  integer,parameter:: sym_pm_do_at = last_resv + 15
  integer,parameter:: sym_pm_do = last_resv + 16
  integer,parameter:: last_stmt = sym_pm_do
  integer,parameter:: num_sym = last_stmt

  ! Non-reserved words that the compiler needs to know about
  integer,parameter:: sym_array = num_sym + 1
  integer,parameter:: sym_array_var = num_sym + 2
  integer,parameter:: sym_idx = num_sym + 3
  integer,parameter:: sym_tuple = num_sym + 4

  ! These are for/par statement attributes
  integer,parameter:: sym_distr= num_sym + 5
  integer,parameter:: sym_topo = num_sym + 6
  integer,parameter:: sym_simplify = num_sym + 7
  integer,parameter:: sym_work = num_sym + 8
  integer,parameter:: sym_sched = num_sym + 9
  integer,parameter:: sym_block = num_sym + 10
  integer,parameter:: num_using_clauses = sym_block-sym_distr+1
  
  ! Procedure/call attributes
  integer,parameter:: sym_always = num_sym + 11
  integer,parameter:: sym_inline = num_sym + 12
  integer,parameter:: sym_no_inline = num_sym + 13
  integer,parameter:: sym_cond_attr = num_sym + 14
  integer,parameter:: sym_uncond = num_sym + 15
  integer,parameter:: sym_ignore_rules = num_sym + 16

  ! filesystem
  integer,parameter:: sym_filesystem = num_sym + 17
  
  ! Symbols used as node types (actual name not really used)
  integer,parameter:: node0 = num_sym + 17
  integer,parameter:: sym_iter = node0 + 1
  integer,parameter:: sym_list = node0 + 2
  integer,parameter:: sym_builtin = node0 + 3
  integer,parameter:: sym_each_proc =  node0 + 4
  integer,parameter:: sym_mode = node0 + 5
  integer,parameter:: sym_for_stmt = node0 + 6
  integer,parameter:: sym_for_each = node0 + 7
  integer,parameter:: sym_reduce_at = node0 + 8
  integer,parameter:: sym_dot_ref = node0 + 9
  integer,parameter:: sym_result = node0 + 10
  integer,parameter:: sym_sub =  node0 + 11
  integer,parameter:: sym_method_call = node0 + 12
  integer,parameter:: sym_dot_sub = node0 + 13
  integer,parameter:: sym_unused_node = node0 +14
  integer,parameter:: sym_array_former = node0 + 15
  integer,parameter:: sym_matrix_former = node0 + 16
  integer,parameter:: sym_do_stmt = node0 + 17
  integer,parameter:: sym_loop= node0 + 18
  integer,parameter:: sym_start_loop = node0 + 19
  integer,parameter:: sym_end_loop = node0 + 20
  integer,parameter:: sym_loop_body = node0 + 21
  integer,parameter:: sym_name = node0 + 22
  integer,parameter:: sym_get_dot = node0 + 23
  integer,parameter:: sym_get_dot_ref = node0 + 24
  integer,parameter:: sym_binary_dash = node0 + 25
  integer,parameter:: sym_proc_typ_par = node0 + 26
  integer,parameter:: sym_local_sub = node0 + 27
  integer,parameter:: sym_set_mode = node0 + 28
  integer,parameter:: sym_export = node0 + 29
  integer,parameter:: sym_export_param = node0 + 30
  integer,parameter:: sym_export_as_new = node0 + 31
  integer,parameter:: sym_amp_error = node0 + 32
  integer,parameter:: sym_pval = node0 + 33
  integer,parameter:: sym_change_mode = node0 + 34
  integer,parameter:: sym_dot_amp = node0 + 35
  integer,parameter:: sym_type_val = node0 + 36
  integer,parameter:: sym_var_set_mode = node0 + 37
  integer,parameter:: sym_if_expr= node0 + 38
  integer,parameter:: sym_switch_expr = node0 + 39
  integer,parameter:: sym_cast = node0 + 40
  integer,parameter:: sym_dim = node0 + 41
  integer,parameter:: sym_vdim = node0 + 42
  integer,parameter:: sym_sync_assign = node0 + 43
  integer,parameter:: sym_get_filesystem = node0 + 44
  integer,parameter:: sym_nested_loop = node0 + 45
  integer,parameter:: sym_assign_list = node0 + 46

  ! Misc. other symbols that need to be referenced by the compiler
  integer,parameter:: hook = node0 + 47
  integer,parameter:: sym_pval_as = hook
  integer,parameter:: sym_pm_system = hook+1
  integer,parameter:: sym_get_element = hook+2
  integer,parameter:: sym_set_element = hook+3
  integer,parameter:: sym_num_elements= hook+4
  integer,parameter:: sym_import_val = hook+5
  integer,parameter:: sym_import_varg = hook+6
  integer,parameter:: sym_import_shared = hook+7 
  integer,parameter:: sym_partition = hook+8
  integer,parameter:: sym_check_conform = hook+9
  integer,parameter:: sym_dup = hook + 10
  integer,parameter:: sym_assemble = hook + 11
  integer,parameter:: sym_node_grid = hook + 12
  integer,parameter:: sym_this_node = hook + 13
  integer,parameter:: sym_grid = hook + 14
  integer,parameter:: sym_indices = hook + 15
  integer,parameter:: sym_get_ref = hook + 16
  integer,parameter:: sym_set_ref = hook + 17
  integer,parameter:: sym_make_subref = hook + 18
  integer,parameter:: sym_make_sublhs = hook + 19
  integer,parameter:: sym_make_sublhs_amp = hook + 20
  integer,parameter:: sym_make_noderef = hook + 21
  integer,parameter:: sym_make_nodelhs = hook + 22
  integer,parameter:: sym_make_nodelhs_amp = hook + 23
  integer,parameter:: sym_invar_dim = hook + 24
  integer,parameter:: sym_fix_dim = hook + 25
  integer,parameter:: sym_sync_messages = hook + 26
  integer,parameter:: hook1 = hook + 26
  
  integer,parameter:: sym_d1= hook1 + 1
  integer,parameter:: sym_d2= hook1 + 2
  integer,parameter:: sym_d3= hook1 + 3
  integer,parameter:: sym_d4= hook1 + 4
  integer,parameter:: sym_d5= hook1 + 5
  integer,parameter:: sym_d6= hook1 + 6
  integer,parameter:: sym_d7= hook1 + 7
  integer,parameter:: sym_copy_out = hook1 + 8
  integer,parameter:: sym_copy_back = hook1 + 9
  integer,parameter:: sym_assignment = hook1 + 10
  integer,parameter:: sym_aliased_assign = hook1 + 11
  integer,parameter:: sym_first = hook1 + 12
  integer,parameter:: sym_next = hook1 + 13
  integer,parameter:: sym_checkcase = hook1 + 14
  integer,parameter:: sym_dim1= hook1 + 15
  integer,parameter:: sym_dim2= hook1 + 16
  integer,parameter:: sym_dim3= hook1 + 17
  integer,parameter:: sym_dim4= hook1 + 18
  integer,parameter:: sym_dim5= hook1 + 19
  integer,parameter:: sym_dim6= hook1 + 20
  integer,parameter:: sym_dim7= hook1 + 21
  integer,parameter:: hook2= hook1+21

  integer,parameter:: sym_generate = hook2 + 1
  integer,parameter:: sym_broadcast = hook2 +2
  integer,parameter:: sym_pop_node = hook2 + 3
  integer,parameter:: sym_make_array = hook2 + 4
  integer,parameter:: sym_export_array = hook2 + 5
  integer,parameter:: sym_get_tile_sz = hook2 + 6
  integer,parameter:: sym_make_mask = hook2 + 7
  integer,parameter:: sym_node_for = hook2 + 8 
  integer,parameter:: sym_import_param = hook2 + 9
  integer,parameter:: sym_set_elem = hook2 + 10
  integer,parameter:: sym_assign_var = hook2 + 11
  integer,parameter:: sym_vector = hook2 + 12
  integer,parameter:: sym_matrix = hook2 + 13
  integer,parameter:: sym_pdup = hook2 + 14
  integer,parameter:: sym_active = hook2 + 15
  integer,parameter:: hook3 = hook2 + 15
  
  integer,parameter:: sym_do_dim = hook3 + 1
  integer,parameter:: sym_shape = hook3 + 2
  integer,parameter:: sym_poly= hook3 + 3
  integer,parameter:: sym_pm_over = hook3 + 4
  integer,parameter:: sym_next_enum = hook3 + 5
  integer,parameter:: sym_make_distr = hook3 + 6
  integer,parameter:: sym_check_import = hook3 + 7
  integer,parameter:: sym_for_get_element = hook3 + 8
  integer,parameter:: sym_for_set_element = hook3 + 9
  integer,parameter:: sym_get_distr = hook3 + 10
  integer,parameter:: sym_get_darray = hook3 + 11
  integer,parameter:: sym_make_local = hook3 + 12
  integer,parameter:: hook4 = hook3 + 12
  
  integer,parameter:: sym_make_ldist = hook4 + 1
  integer,parameter:: sym_make_global = hook4 + 2
  integer,parameter:: sym_shared_to_local = hook4 + 3
  integer,parameter:: sym_clone = hook4 + 4
  integer,parameter:: sym_contains = hook4 + 5
  integer,parameter:: sym_this = hook4 + 6
  integer,parameter:: sym_here = hook4 + 7
  integer,parameter:: sym_here_in_tile = hook4 + 8
  integer,parameter:: sym_subregion = hook4 + 9
  integer,parameter:: sym_region = hook4 + 10
  integer,parameter:: sym_distr_tag = hook4 + 11
  integer,parameter:: sym_tag = hook4 + 12
  integer,parameter:: sym_varray= hook4 + 13
  integer,parameter:: sym_map_array= hook4 + 14
  integer,parameter:: sym_map_varray= hook4 + 15
  integer,parameter:: sym_get_val_ref = hook4 + 16
  integer,parameter:: sym_local_distr = hook4 + 17
  integer,parameter:: sym_pm_chan = hook4 + 18
  integer,parameter:: sym_get_chan = hook4 + 19
  integer,parameter:: sym_pm_local = hook4 + 20
  integer,parameter:: sym_casts_to = hook4 + 21
  integer,parameter:: sym_combine_indices = hook4 + 22
  integer,parameter:: hook5 = hook4 + 22
  
  integer,parameter:: sym_pm_ref_type =   hook5 + 1
  integer,parameter:: sym_pling_local =   hook5 + 2
  integer,parameter:: sym_array_proc  =   hook5 + 3
  integer,parameter:: sym_dims        =   hook5 + 4
  integer,parameter:: sym_make_dollar =   hook5 + 5
  integer,parameter:: sym_make_dtuple =   hook5 + 6
  integer,parameter:: sym_stretch_dim =   hook5 + 7
  integer,parameter:: sym_pm_nhd      =   hook5 + 8
  integer,parameter:: sym_envelope    =   hook5 + 9
  integer,parameter:: sym_set_nhd     =   hook5 + 10
  integer,parameter:: sym_send_nhd    =   hook5 + 11
  integer,parameter:: sym_recv_nhd    =   hook5 + 12
  integer,parameter:: sym_check_bounds=   hook5 + 13
  integer,parameter:: sym_set_edge    =   hook5 + 14
  integer,parameter:: sym_make_over   =   hook5 + 15
  integer,parameter:: sym_do_over     =   hook5 + 16
  integer,parameter:: sym_check_alias =   hook5 + 17
  integer,parameter:: sym_cat         =   hook5 + 18
  integer,parameter:: sym_interior    =   hook5 + 19
  integer,parameter:: sym_in_interior =   hook5 + 20
  integer,parameter:: sym_nhd_var     =   hook5 + 21
  integer,parameter:: sym_nhd_join    =   hook5 + 22
  integer,parameter:: sym_nhd_active  =   hook5 + 23
  integer,parameter:: sym_bcast_nhd   =   hook5 + 24
  integer,parameter:: sym_blocking    =   hook5 + 25
  integer,parameter:: sym_chunks      =   hook5 + 26
  integer,parameter:: sym_get_chunk   =   hook5 + 27
  integer,parameter:: sym_pm_at       =   hook5 + 28
  integer,parameter:: sym_push_mess   =   hook5 + 29
  integer,parameter:: sym_pop_sync_mess = hook5 + 30
  integer,parameter:: sym_join_param  =   hook5 + 31
  integer,parameter:: sym_split_param =   hook5 + 32
  integer,parameter:: sym_dim_noinit  =   hook5 + 33
  integer,parameter:: sym_pm_node     =   hook5 + 34
  integer,parameter:: sym_init_var    =   hook5 + 35
  integer,parameter:: sym_pm_dump     =   hook5 + 36

  integer,parameter:: hook6 = 36 + hook5
  integer,parameter:: first_pragma = hook6 + 1
  integer,parameter:: sym_infer_stack = hook6 + 1
  integer,parameter:: sym_infer_type = hook6 + 2
  integer,parameter:: sym_infer_type_and_stack = hook6 + 3
  integer,parameter:: sym_show_stack = hook6 + 4
  integer,parameter:: sym_show = hook6 + 5
  integer,parameter:: sym_dump = hook6 + 6
  integer,parameter:: last_pragma = hook6 + 6
  
  integer,parameter:: num_syshook = hook6 + 6
  
  !==============================================

  character(len=20),dimension(0:num_syshook)::sym_names
  data sym_names(0)                    /'<void>'/
  data sym_names(sym_at)               /'@'/
  data sym_names(sym_dollar)           /'$'/
  data sym_names(sym_semi)             /';'/
  data sym_names(sym_open_square)      /'['/
  data sym_names(sym_close_square)     /']'/
  data sym_names(sym_open_brace)       /'{'/
  data sym_names(sym_close_brace)      /'}'/
  data sym_names(sym_colon)            /':'/
  data sym_names(sym_dotdotdot)        /'...'/
  data sym_names(sym_ddollar)          /'$$'/
  data sym_names(sym_comma)            /','/
  data sym_names(sym_dot)              /'.'/
  data sym_names(sym_assign)           /':='/
  data sym_names(sym_underscore)       /'_'/
  data sym_names(sym_open_attr)        /'<<'/
  data sym_names(sym_close_attr)       /'>>'/
  data sym_names(sym_query)            /'?'/
  data sym_names(sym_arrow)            /'->'/
  data sym_names(sym_pct)              /'%'/
  data sym_names(sym_dash)             /''''/
  data sym_names(sym_caret)            /'^'/
  data sym_names(sym_dcaret)           /'^^'/
  data sym_names(sym_dcolon)           /'::'/
  data sym_names(sym_define)           /'='/
  data sym_names(sym_cond)             /'=>'/
  
  data sym_names(sym_string)           /'<string>'/
  data sym_names(sym_number)           /'<number>'/

  ! Operators
  data sym_names(sym_open)             /'('/
  data sym_names(sym_close)            /')'/
  data sym_names(sym_le)               /'<='/
  data sym_names(sym_lt)               /'<'/
  data sym_names(sym_ustar)            /'unary *'/

  
  data sym_names(sym_from_range)       /'_...'/
  data sym_names(sym_to_range)         /'..._'/
  data sym_names(sym_concat)           /'++'/
  data sym_names(sym_eq)               /'=='/
  data sym_names(sym_ne)               /'/='/
  data sym_names(sym_ge)               /'>='/
  data sym_names(sym_gt)               /'>'/
  data sym_names(sym_plus)             /'+'/
  data sym_names(sym_minus)            /'-'/
  data sym_names(sym_mult)             /'*'/
  data sym_names(sym_divide)           /'/'/
  data sym_names(sym_pow)              /'**'/
  data sym_names(sym_dotdot)           /'..'/
  data sym_names(sym_bar)              /'|'/
  data sym_names(sym_hash)             /'#'/
  data sym_names(sym_amp)              /'&'/
  data sym_names(sym_pling)            /'!'/
  
  data sym_names(sym_in)               /'in'/
  data sym_names(sym_and)              /'and'/
  data sym_names(sym_or)               /'or'/
  data sym_names(sym_xor)              /'xor'/
  data sym_names(sym_shift)            /'shift'/
  data sym_names(sym_fmt)              /'fmt'/
  data sym_names(sym_by)               /'by'/
  data sym_names(sym_mod)              /'mod'/
  data sym_names(sym_except)           /'except'/
  data sym_names(sym_includes)         /'inc'/
  data sym_names(sym_ortho)            /'ortho'/
  data sym_names(sym_is)               /'is'/
  data sym_names(sym_as)               /'as'/
  data sym_names(sym_not)              /'not'/

  ! Statement / expression general keywords
  data sym_names(sym_null)             /'null'/
  data sym_names(sym_key)              /'key'/
  data sym_names(sym_arg)              /'arg'/
  data sym_names(sym_true)             /'true'/
  data sym_names(sym_false)            /'false'/
  data sym_names(sym_struct)           /'struct'/
  data sym_names(sym_rec)              /'rec'/
  data sym_names(sym_any)              /'any'/
  data sym_names(sym_present)          /'present'/
  data sym_names(sym_unique)           /'unique'/
  data sym_names(sym_fix)              /'fix'/
  data sym_names(sym_new)              /'new'/
  data sym_names(sym_bounds)           /'bounds'/
  data sym_names(sym_of)               /'of'/
  
  data sym_names(sym_private)          /'priv'/
  data sym_names(sym_invar)            /'invar'/
  data sym_names(sym_complete)         /'complete'/
  data sym_names(sym_universal)        /'universal'/
  data sym_names(sym_local)            /'local'/
  
  data sym_names(sym_partial)          /'partial'/
  data sym_names(sym_coherent)         /'coherent'/
  data sym_names(sym_chan)             /'chan'/
  data sym_names(sym_mirrored)         /'uniform'/
  data sym_names(sym_shared)           /'shared'/

  ! Declaration keywords
  data sym_names(sym_use)              /'use'/
  data sym_names(sym_proc)             /'proc'/
  data sym_names(sym_param)            /'param'/
  data sym_names(sym_type)             /'type'/
  data sym_names(sym_render)           /'render'/

  ! Statement keywords 
  ! -- anything that can follow the end of a statement
  data sym_names(sym_check)            /'check'/
  data sym_names(sym_over)             /'over'/
  data sym_names(sym_else)             /'else'/
  data sym_names(sym_elseif)           /'elseif'/
  data sym_names(sym_case)             /'case'/
  data sym_names(sym_for)              /'for'/
  data sym_names(sym_if)               /'if'/
  data sym_names(sym_par)              /'par'/
  data sym_names(sym_switch)           /'switch'/
  data sym_names(sym_until)            /'until'/
  data sym_names(sym_using)            /'using'/
  data sym_names(sym_sync)             /'sync'/
  data sym_names(sym_while)            /'while'/
  data sym_names(sym_return)           /'return'/
  data sym_names(sym_do)               /'do'/
  data sym_names(sym_nhd)              /'nhd'/
  data sym_names(sym_test)             /'test'/
  data sym_names(sym_default)          /'default'/
  data sym_names(sym_task)             /'task'/
  data sym_names(sym_extern)           /'extern'/
  data sym_names(sym_var)              /'var'/
  data sym_names(sym_const)            /'const'/
  data sym_names(sym_each)             /'foreach'/
  data sym_names(sym_where)            /'where'/
  data sym_names(sym_with)             /'with'/
  data sym_names(sym_conc)             /'forall'/
  data sym_names(sym_interface)        /'interface'/
  data sym_names(sym_switch_invar)     /'iswitch'/
  data sym_names(sym_if_invar)         /'<if invar>'/
  data sym_names(sym_while_invar)      /'<while invar>'/
  data sym_names(sym_until_invar)      /'<until invar>'/
  data sym_names(sym_foreach_invar)    /'<foreach invar>'/
  data sym_names(sym_proceed)          /'proceed'/
  data sym_names(sym_after)            /'after'/
  data sym_names(sym_any_invar)        /'<any invar>'/
  
  data sym_names(sym_pm_send)          /'PM__send'/
  data sym_names(sym_pm_recv)          /'PM__recv'/
  data sym_names(sym_pm_collect)       /'PM__collect'/
  data sym_names(sym_pm_serve)         /'PM__serve'/
  data sym_names(sym_pm_bcast)         /'PM__bcast'/
  data sym_names(sym_pm_recv_req)      /'PM__recv_req'/
  data sym_names(sym_pm_recv_assn)     /'PM__recv_assn'/
  data sym_names(sym_pm_dref)          /'PM__dref'/
  data sym_names(sym_pm_dref_shared)   /'PM__drefi'/
  data sym_names(sym_pm_dref_slice)    /'PM__drefs'/
  data sym_names(sym_pm_dref_shared_slice) /'PM__dref_is'/
  data sym_names(sym_pm_dref_here)     /'PM__drefhere'/
  data sym_names(sym_pm_ref)           /'PM__ref'/
  data sym_names(sym_pm_head_node)     /'PM__head_node'/
  data sym_names(sym_pm_do_at)         /'PM__do_at'/
  data sym_names(sym_pm_do)            /'PM__do'/

  !===============================================================

  data sym_names(sym_array)            /'PM__array'/
  data sym_names(sym_array_var)        /'PM__array_v'/
  data sym_names(sym_idx)              /'indexed'/
  data sym_names(sym_tuple)            /'tuple'/

  data sym_names(sym_distr)            /'distr'/
  data sym_names(sym_topo)             /'topo'/
  data sym_names(sym_simplify)         /'simplify'/
  data sym_names(sym_sched)            /'sched'/
  data sym_names(sym_work)             /'work'/
  data sym_names(sym_block)            /'blocking'/

  data sym_names(sym_always)           /'always'/
  data sym_names(sym_inline)           /'inline'/
  data sym_names(sym_no_inline)        /'no_inline'/
  data sym_names(sym_cond_attr)        /'cond'/
  data sym_names(sym_uncond)           /'uncond'/
  data sym_names(sym_ignore_rules)     /'PM__ignore'/


  data sym_names(sym_filesystem)       /'filesystem'/

  ! Symbols that are node names only
  data sym_names(sym_iter)             /'<iter>'/
  data sym_names(sym_list)             /'<list>'/
  data sym_names(sym_builtin)          /'<builtin>'/
  data sym_names(sym_each_proc)        /'<each-proc>'/
  data sym_names(sym_mode)             /'<mode>'/
  data sym_names(sym_for_stmt)         /'<for-stmt>'/
  data sym_names(sym_for_each)         /'for each'/
  data sym_names(sym_reduce_at)        /'<reduce-at>'/
  data sym_names(sym_dot_ref)          /'<dot-ref>'/
  data sym_names(sym_result)           /'<result>'/
  data sym_names(sym_sub)              /'[]'/
  data sym_names(sym_method_call)      /'<method-call>'/
  data sym_names(sym_dot_sub)          /'<dot-sub>'/
  data sym_names(sym_unused_node)      /'<unused_node>'/
  data sym_names(sym_array_former)     /'<array-former>'/
  data sym_names(sym_matrix_former)    /'PM__matrix'/
  data sym_names(sym_do_stmt)          /'<do-statement>'/
  data sym_names(sym_loop)             /'<loop>'/
  data sym_names(sym_start_loop)       /'<start-loop>'/
  data sym_names(sym_end_loop)         /'<end-loop>'/
  data sym_names(sym_loop_body)        /'<loop-body>'/
  data sym_names(sym_name)             /'<name>'/
  data sym_names(sym_get_dot)          /'<get-dot>'/
  data sym_names(sym_get_dot_ref)      /'<get-dot-ref>'/
  data sym_names(sym_binary_dash)      /'<binary-dash>'/
  data sym_names(sym_proc_typ_par)     /'<proc-typ-par>'/
  data sym_names(sym_local_sub)        /'<local-sub>'/
  data sym_names(sym_set_mode)         /'<set-mode>'/
  data sym_names(sym_export)           /'<export>'/
  data sym_names(sym_export_param)     /'<export-param>'/
  data sym_names(sym_export_as_new)    /'<export-as-new>'/
  data sym_names(sym_amp_error)        /'<amp-error>'/
  data sym_names(sym_also)             /'<also>'/
  data sym_names(sym_pval)             /'PM__pval'/
  data sym_names(sym_pval_as)          /'PM__pval_as'/
  data sym_names(sym_change_mode)      /'<change_mode>'/
  data sym_names(sym_dot_amp)          /'<dot-amp>'/
  data sym_names(sym_type_val)         /'<type-val>'/
  data sym_names(sym_var_set_mode)     /'<var-set-mode>'/
  data sym_names(sym_if_expr)          /'PM__if'/
  data sym_names(sym_switch_expr)      /'PM__switch'/
  data sym_names(sym_cast)             /'PM__cast'/
  data sym_names(sym_dim)              /'PM__dim'/
  data sym_names(sym_vdim)             /'PM__vdim'/
  data sym_names(sym_sync_assign)      /'<sync-assign>'/
  data sym_names(sym_get_filesystem)   /'PM__filesys'/
  data sym_names(sym_nested_loop)      /'PM__nested_loop'/
  data sym_names(sym_assign_list)      /'<assign-list>'/
  
  ! Misc. symbols referenced by compiler
  
  data sym_names(sym_pm_system)        /'PM__system'/
  data sym_names(sym_get_element)      /'PM__getelem'/
  data sym_names(sym_set_element)      /'PM__setelem'/
  data sym_names(sym_num_elements)     /'size'/
  data sym_names(sym_import_val)       /'PM__import_val'/
  data sym_names(sym_import_varg)      /'PM__importvarg'/
  data sym_names(sym_import_shared)    /'PM__importshrd'/
  data sym_names(sym_partition)        /'PM__partition'/
  data sym_names(sym_check_conform)    /'check_conform'/
  data sym_names(sym_dup)              /'PM__dup'/
  data sym_names(sym_assemble)         /'PM__assemble'/
  data sym_names(sym_node_grid)        /'node_grid'/
  data sym_names(sym_this_node)        /'this_node'/
  data sym_names(sym_grid)             /'grid'/
  data sym_names(sym_indices)          /'indices'/
  data sym_names(sym_get_ref)          /'PM__getref'/
  data sym_names(sym_set_ref)          /'PM__set_ref'/
  data sym_names(sym_make_subref)      /'PM__subref'/
  data sym_names(sym_make_sublhs)      /'PM__sublhs'/
  data sym_names(sym_make_sublhs_amp)  /'PM__sublhsamp'/
  data sym_names(sym_make_noderef)     /'PM__noderef'/
  data sym_names(sym_make_nodelhs)     /'PM__nodelhs'/
  data sym_names(sym_make_nodelhs_amp) /'PM__nodeamp'/
  data sym_names(sym_invar_dim)        /'PM__invar_dim'/
  data sym_names(sym_fix_dim)          /'PM__fix_dim'/
  data sym_names(sym_sync_messages)    /'PM__sync_messages'/
  
  data sym_names(sym_d1)               /'PM__d1'/
  data sym_names(sym_d2)               /'PM__d2'/
  data sym_names(sym_d3)               /'PM__d3'/
  data sym_names(sym_d4)               /'PM__d4'/
  data sym_names(sym_d5)               /'PM__d5'/
  data sym_names(sym_d6)               /'PM__d6'/
  data sym_names(sym_d7)               /'PM__d7'/

  data sym_names(sym_copy_out)         /'PM__copy_out'/
  data sym_names(sym_copy_back)        /'PM__copy_back'/
  data sym_names(sym_assignment)       /'PM__assign'/
  data sym_names(sym_aliased_assign)   /'PM__aliased_assign'/
  data sym_names(sym_first)            /'PM__first'/
  data sym_names(sym_next)             /'PM__next'/
  data sym_names(sym_checkcase)        /'PM__checkcase'/
  
  data sym_names(sym_dim1)             /'tuple1d'/
  data sym_names(sym_dim2)             /'tuple2d'/
  data sym_names(sym_dim3)             /'tuple3d'/
  data sym_names(sym_dim4)             /'tuple4d'/
  data sym_names(sym_dim5)             /'tuple5d'/
  data sym_names(sym_dim6)             /'tuple6d'/
  data sym_names(sym_dim7)             /'tuple7d'/
  
  data sym_names(sym_generate)         /'PM__generate'/
  data sym_names(sym_broadcast)        /'PM__broadcast'/
  data sym_names(sym_pop_node)         /'PM__pop_node'/
  data sym_names(sym_make_array)       /'PM__makearray'/
  data sym_names(sym_export_array)     /'PM__exparray'/
  data sym_names(sym_get_tile_sz)      /'PM__get_tilesz'/
  data sym_names(sym_make_mask)        /'PM__make_mask'/
  data sym_names(sym_node_for)         /'node_for'/
  data sym_names(sym_import_param)     /'PM__impparam'/
  data sym_names(sym_set_elem)         /'PM__setaelem'/
  data sym_names(sym_assign_var)       /'PM__assign_var'/
  data sym_names(sym_vector)           /'vector'/
  data sym_names(sym_matrix)           /'matrix'/
  data sym_names(sym_pdup)             /'PM__pdup'/
  data sym_names(sym_active)           /'PM__active'/
  
  data sym_names(sym_do_dim)           /'PM__do_dim'/
  data sym_names(sym_shape)            /'shape'/
  data sym_names(sym_poly)             /'poly'/
  data sym_names(sym_pm_over)          /'PM__over'/
  data sym_names(sym_next_enum)        /'next_enum'/
  data sym_names(sym_make_distr)       /'PM__distr'/
  data sym_names(sym_check_import)     /'PM__checkimp'/
  data sym_names(sym_for_get_element)  /'PM__get_elem'/
  data sym_names(sym_for_set_element)  /'PM__set_elem'/
  data sym_names(sym_get_distr)        /'PM__get_distr'/
  data sym_names(sym_get_darray)       /'PM__get_darray'/
  data sym_names(sym_make_local)       /'PM__make_local'/
  
  data sym_names(sym_make_ldist)       /'PM__make_ldist'/
  data sym_names(sym_make_global)      /'PM__make_glob'/
  data sym_names(sym_shared_to_local)  /'PM__shared_to_local'/
  data sym_names(sym_clone)            /'PM__clone'/
  data sym_names(sym_contains)         /'contains'/
  data sym_names(sym_this)             /'this'/
  data sym_names(sym_here)             /'here'/
  data sym_names(sym_here_in_tile)     /'here_in_tile'/
  data sym_names(sym_subregion)        /'schedule'/
  data sym_names(sym_region)           /'region'/
  data sym_names(sym_distr_tag)        /'PM__distr_tag'/
  data sym_names(sym_tag)              /'PM__tag'/
  data sym_names(sym_varray)           /'PM__varray'/
  data sym_names(sym_map_array)        /'PM__maparray'/
  data sym_names(sym_map_varray)       /'PM__mapvarray'/
  data sym_names(sym_get_val_ref)      /'PM__valref'/
  data sym_names(sym_local_distr)      /'local_distr'/
  data sym_names(sym_pm_chan)          /'PM__chan'/
  data sym_names(sym_get_chan)         /'PM__getchan'/
  data sym_names(sym_pm_local)         /'PM__local'/
  data sym_names(sym_casts_to)         /'casts_to'/
  data sym_names(sym_combine_indices)  /'PM__cmbidx'/
  
  data sym_names(sym_pm_ref_type)      /'PM__reftype'/
  data sym_names(sym_pling_local)      /'PM__atlcl'/
  data sym_names(sym_array_proc)       /'array'/
  data sym_names(sym_dims)             /'dims'/
  data sym_names(sym_make_dollar)      /'PM__makeidxdim'/
  data sym_names(sym_make_dtuple)      /'PM__makeidx'/
  data sym_names(sym_stretch_dim)      /'PM__strdim'/
  data sym_names(sym_pm_nhd)           /'PM__nhd'/
  data sym_names(sym_envelope)         /'envelope'/
  data sym_names(sym_set_nhd)          /'PM__set_nhd'/
  data sym_names(sym_send_nhd)         /'PM__send_nhd'/
  data sym_names(sym_recv_nhd)         /'PM__recv_nhd'/
  data sym_names(sym_check_bounds)     /'PM__check_bounds'/
  data sym_names(sym_set_edge)         /'PM__set_edge'/
  data sym_names(sym_make_over)        /'PM__make_over'/
  data sym_names(sym_do_over)          /'PM__do_over'/
  data sym_names(sym_check_alias)      /'PM__check_alias'/
  data sym_names(sym_cat)              /'concat'/
  data sym_names(sym_interior)         /'interior'/
  data sym_names(sym_in_interior)      /'in_interior'/
  data sym_names(sym_pm_dump)          /'PM__dump'/
  data sym_names(sym_nhd_var)          /'PM__nhd_var'/
  data sym_names(sym_nhd_join)         /'PM__nhd_join'/
  data sym_names(sym_nhd_active)       /'PM__nhd_active'/
  data sym_names(sym_bcast_nhd)        /'PM__bcast_nhd'/
  data sym_names(sym_blocking)         /'PM__blocking'/
  data sym_names(sym_push_mess)        /'PM__push_mess'/
  data sym_names(sym_pop_sync_mess)    /'PM__pop_sync_mess'/
  data sym_names(sym_join_param)       /'PM__join_param'/
  data sym_names(sym_split_param)      /'PM__split_param'/
  data sym_names(sym_dim_noinit)       /'PM__dim_noinit'/
  data sym_names(sym_chunks)           /'chunks'/
  data sym_names(sym_get_chunk)        /'chunk'/
  data sym_names(sym_pm_at)            /'PM__at'/
  data sym_names(sym_pm_node)          /'PM__node'/
  data sym_names(sym_init_var)         /'PM__init_var'/

  data sym_names(sym_infer_stack)      /'infer_stack'/
  data sym_names(sym_infer_type)       /'infer_type'/
  data sym_names(sym_infer_type_and_stack)  &
                                       /'infer_type_and_stack'/
  data sym_names(sym_show_stack)       /'show_stack'/
  data sym_names(sym_show)             /'show'/
  data sym_names(sym_dump)             /'dump'/
  
contains

  !============================================
  ! Initialise global symbol+name table
  !============================================
  subroutine pm_init_names(context)
    type(pm_context),pointer:: context
    integer:: i,j
    
    context%names=pm_set_new(context,128_pm_ln)
    do i=1,num_syshook
       !write(*,*) i,'{"',trim(sym_names(i)),'"}'
       if(sym_names(i)(1:1)=='_'.and.sym_names(i)(2:2)/=' '.and.sym_names(i)(2:2)/='.') then
          j=pm_lname_entry(context,sym_pm_system,trim(sym_names(i)(2:)))
          if(i/=j) then
             write(*,*) 'L',i,j,'{',trim(sym_names(i)),'}'
             call pm_panic('Setting name table -Lcl')
          endif
       else
          j=pm_name_entry(context,trim(sym_names(i)))
          if(i/=j) then
             write(*,*) i,j,'{',trim(sym_names(i)),'}'
             call pm_panic('Setting name table - Non-Lcl')
          endif
       endif
    enddo
  end subroutine pm_init_names

  !=========================================
  ! Add name to global dictionary
  !=========================================
  function pm_name_entry(context,string) result(val)
    type(pm_context),pointer:: context
    character(len=*):: string
    integer:: val
    integer:: sym
    context%temp_obj3=pm_new_string(context,string)
    sym=pm_set_lookup(context,context%names,context%temp_obj3)
    if(sym==0) then
       sym=pm_set_add(context,context%names,context%temp_obj3)
    endif
    val=sym
  end function pm_name_entry

  !================================================
  ! Add local name (_name) to global dictionary
  !================================================
  function pm_lname_entry(context,modl_name,string) result(val)
    type(pm_context),pointer:: context
    integer:: modl_name
    character(len=*),intent(in):: string
    integer:: val
    integer:: sym
    sym=pm_name_entry(context,string)
    context%temp_obj3=pm_fast_newnc(context,pm_int32,2)
    context%temp_obj3%data%i32(context%temp_obj3%offset)=modl_name
    context%temp_obj3%data%i32(context%temp_obj3%offset+1)=sym
    sym=pm_set_lookup(context,context%names,context%temp_obj3)
    if(sym==0) then
       sym=pm_set_add(context,context%names,context%temp_obj3)
    endif
    val=sym
  contains
    include 'fnewnc.inc'
  end function pm_lname_entry

  !=========================================
  ! Create a two component composite name
  !=========================================
  function pm_name2(context,name1,name2) result(sym)
    type(pm_context),pointer:: context
    integer,intent(in):: name1,name2
    integer:: sym
    integer,dimension(2):: namevec
    namevec(1)=name1
    namevec(2)=name2
    sym=pm_name_vector(context,namevec,0,2)
  contains
    include 'fnewnc.inc'
  end function  pm_name2

  !=====================================
  ! Create vector of names
  !=====================================
  function pm_name_vector(context,vect,base,n) result(sym)
    type(pm_context),pointer:: context
    integer,intent(in),dimension(*):: vect
    integer:: sym
    integer,intent(in):: base,n
    context%temp_obj3=pm_fast_newnc(context,pm_int,n-base)
    context%temp_obj3%data%i(context%temp_obj3%offset:&
         context%temp_obj3%offset+n-base-1)=vect(base+1:n)
    sym=pm_set_lookup(context,context%names,context%temp_obj3)
    if(sym==0) then
       sym=pm_set_add(context,context%names,context%temp_obj3)
    endif
  contains
    include 'fnewnc.inc'
  end function  pm_name_vector

  !=========================================
  ! Create a name value from a string
  !=========================================
  function pm_intern(context,str) result(n)
    type(pm_context),pointer:: context
    character(len=*),intent(in):: str
    integer:: n
    type(pm_root),pointer:: root
    type(pm_ptr):: string
    string=pm_new_string(context,str)
    n=pm_set_lookup(context,context%names,string)
    if(n==0) then
       root=>pm_add_root(context,string)
       n=pm_set_add(context,context%names,string)
       call pm_delete_root(context,root)
    endif
  end function pm_intern

  !=============================================
  ! Intern a value (add to global dictionary)
  !=============================================
  function pm_intern_val(context,val) result(n)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: val
    integer:: n
    n=pm_set_lookup(context,context%names,val)
    if(n==0) then
       n=pm_set_add(context,context%names,val)
    endif
  end function pm_intern_val

  !==================================
  ! Is name local to a module?
  !==================================
  function pm_name_is_local(context,name) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: name
    logical::ok
    ok=pm_fast_vkind(pm_name_val(context,name))==pm_int32
  contains
    include 'fvkind.inc'
  end function pm_name_is_local

  !===================================
  ! Is name a dotted list of names?
  !===================================
  function pm_name_is_vector(context,name) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: name
    logical::ok
    ok=pm_fast_vkind(pm_name_val(context,name))==pm_int
  contains
    include 'fvkind.inc'
  end function pm_name_is_vector

  !=======================
  ! Get name value
  !=======================
  function pm_name_val(context,m) result(val)
    type(pm_context),pointer:: context
    integer,intent(in):: m
    type(pm_ptr):: val
    type(pm_ptr):: keys
    if(m==0) then
       val=pm_null_obj
       return
    endif
    keys=pm_set_keys(context,context%names)
    if(pm_debug_checks) then
       if(m<1.or.m>pm_fast_esize(keys)) then
          write(*,*) 'm=',m
          call pm_panic('name_val')
       endif
    endif
    val=keys%data%ptr(keys%offset+abs(m)-1)
  contains
    include 'fesize.inc'
  end function pm_name_val

  !==========================================
  ! Return name unqualified by module
  !==========================================
  function pm_name_stem(context,m) result(name)
    type(pm_context),pointer:: context
    integer,intent(in):: m
    integer:: name
    type(pm_ptr):: val
    integer:: vkind
    val=pm_name_val(context,m)
    vkind=pm_fast_vkind(val)
    if(vkind==pm_int) then
       name=val%data%i(val%offset+1)
    else
       name=m
    endif
  contains
    include 'fvkind.inc'
  end function pm_name_stem

  !===================================
  ! Returns name as a string
  !===================================
  function pm_name_as_string(context,m) result(str)
    type(pm_context),pointer:: context
    integer:: m
    character(len=300):: str
    str=''
    call pm_name_string(context,m,str)
  end function pm_name_as_string

  !==============================================================
  ! Returns vector of names as string with names "." separated
  !==============================================================
  function pm_name_vect_as_string(context,vect,step) result(str)
    type(pm_context),pointer:: context
    integer,dimension(:),intent(in):: vect
    integer,intent(in):: step
    character(len=300):: str,str2
    integer:: i
    str=''
    do i=1,size(vect),2
       call pm_name_string(context,abs(vect(i)),str2)
       if(i==0) then
          str=trim(str2)
       else
          str=trim(str)//'.'//trim(str2)
       endif
    enddo
  end function pm_name_vect_as_string

  !=========================================
  ! Get name string from name entry
  !=========================================
  recursive subroutine pm_name_string(context,m,str)
    type(pm_context),pointer:: context
    integer,intent(in):: m
    character(len=*),intent(out):: str
    integer:: n
    character(len=100):: str2
    type(pm_ptr):: keys,vals,ptr
    integer:: i,first,second
    str=' '
    n=m
    if(n>0) then
       keys=pm_set_keys(context,context%names)
       if(n<=pm_fast_esize(keys)) then
          ptr=keys%data%ptr(keys%offset+n-1)
          if(pm_fast_vkind(ptr)==pm_string) then
             call pm_strval(ptr,str2)
             str=trim(str2)
          else if(pm_fast_vkind(ptr)==pm_int) then
             first=ptr%data%i(ptr%offset)
             if(pm_fast_esize(ptr)>0) then
                second=ptr%data%i(ptr%offset+1_pm_p)
                if(first==sym_gt) then
                   str(1:1)='>'
                   call pm_name_string(context,&
                        second,str(2:))
                elseif(first<0) then
                   if(first==-sym_pm_system.or.&
                        (first==-pm_main_module)) then
                      call pm_name_string(context,&
                           second,str)
                      if(pm_fast_esize(ptr)>1) then
                         do i=2,pm_fast_esize(ptr)
                            call pm_name_string(context,ptr%data%i(ptr%offset+i),str2)
                            str=trim(str)//''''//str2
                         enddo
                      endif
                   else
                      call pm_name_string(context,&
                           -first,str)
                      call pm_name_string(context,&
                           second,str2)
                      str=trim(str)//''''//str2
                   endif
                else
                   if(pm_fast_esize(ptr)==1.and.ptr%data%i(ptr%offset)==sym_pm_system) then
                      call pm_name_string(context,&
                              ptr%data%i(ptr%offset+1),str)
                   else
                      do i=0,pm_fast_esize(ptr)
                         call pm_name_string(context,&
                              ptr%data%i(ptr%offset+i),str2)
                         if(i==0) then
                            str=trim(str2)
                         else
                            str=trim(str)//''''//trim(str2)
                         endif
                      enddo
                   endif
                endif
             else
                call pm_name_string(context,first,str)
             endif
          else if(pm_fast_vkind(ptr)==pm_int32) then
             first=ptr%data%i32(ptr%offset)
             call pm_name_string(context,&
                  ptr%data%i32(ptr%offset+1),str2)
             if(first==sym_pm_system) then
                str='_'//trim(str2)
             else
                call pm_name_string(context,first,str)
                str=trim(str)//'''_'//trim(str2)
             endif
          else
             str='?type'
          endif
       else
          write(str,'(1h?,i8,1h?)') n
       endif
    else if(n<0) then
       str='use '
       call pm_name_string(context,-n,str(5:))
    else
       str='EOF'
    endif
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end subroutine pm_name_string


end module pm_symbol
