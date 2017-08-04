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

! Symbols and names
module pm_symbol
  use pm_sysdep
  use pm_compbase
  use pm_kinds
  use pm_memory
  use pm_lib
  implicit none
  
  ! ============================================================
  ! These constants represent keywords, other symbols and 
  ! also represent a range of abstract syntax node types
  ! ============================================================

  ! End of file
  integer,parameter:: sym_eof =0

  ! Non-operator symbols
  integer,parameter:: sym_dollar = 1
  integer,parameter:: sym_at = 2
  integer,parameter:: sym_semi = 3
  integer,parameter:: sym_open_square = 4
  integer,parameter:: sym_close_square = 5
  integer,parameter:: sym_open_brace = 6
  integer,parameter:: sym_close_brace = 7
  integer,parameter:: sym_colon = 8
  integer,parameter:: sym_dotdotdot = 9
  integer,parameter:: sym_define = 10
  integer,parameter:: sym_comma = 11
  integer,parameter:: sym_dot = 12
  integer,parameter:: sym_assign = 13
  integer,parameter:: sym_underscore = 14 
  integer,parameter:: sym_amp = 15
  integer,parameter:: sym_dcolon = 16 
  integer,parameter:: sym_query = 17
  integer,parameter:: sym_arrow = 18
  integer,parameter:: sym_pct = 19
  integer,parameter:: sym_define_const = 20
  integer,parameter:: sym_dash = 21
  integer,parameter:: sym_caret = 22
  integer,parameter:: sym_string = 23
  integer,parameter:: sym_number = 24

  ! Node types not directly associated with a single symbol
  integer,parameter:: node0 = sym_number
  integer,parameter:: sym_iter = node0 + 1
  integer,parameter:: sym_list = node0 + 2
  integer,parameter:: sym_pct_brace = node0 + 3
  integer,parameter:: sym_at_brace = node0 + 4
  integer,parameter:: sym_at_brace_or = node0 + 5
  integer,parameter:: sym_brace_at = node0 + 6
  integer,parameter:: sym_reduce_at = node0 + 7
  integer,parameter:: sym_dotref = node0 + 8
  integer,parameter:: sym_sub = node0 + 9
  integer,parameter:: sym_array_former = node0 + 10
  integer,parameter:: sym_matrix_former = node0 + 11

  ! Operators
  integer,parameter:: sym1 = sym_matrix_former
  integer,parameter:: sym_open = sym1 + 1
  integer,parameter:: first_opr = sym_open
  integer,parameter:: sym_close = sym1 + 2
  integer,parameter:: sym_uminus = sym1 + 3
  integer,parameter:: sym_from_range = sym1 + 4
  integer,parameter:: sym_to_range = sym1 + 5

  integer,parameter:: sym2 = sym_to_range
  integer,parameter:: first_unary = sym_uminus

  integer,parameter:: sym_concat = sym2 + 1
  integer,parameter:: first_binary = sym_concat
  integer,parameter:: sym_eq = sym2 + 2
  integer,parameter:: sym_ne = sym2 + 3
  integer,parameter:: sym_ge = sym2 + 4
  integer,parameter:: sym_gt = sym2 + 5
  integer,parameter:: sym_le = sym2 + 6
  integer,parameter:: sym_lt = sym2 + 7
  integer,parameter:: sym_plus = sym2 + 8
  integer,parameter:: sym_minus = sym2 + 9
  integer,parameter:: sym_mult = sym2 + 10
  integer,parameter:: sym_divide = sym2 + 11
  integer,parameter:: sym_pow = sym2 + 12
  integer,parameter:: sym_dotdot = sym2 + 13
  integer,parameter:: sym_cond = sym2 + 14
  integer,parameter:: sym_bar = sym2 + 15
  integer,parameter:: sym_hash= sym2 + 16
  integer,parameter:: sym_dbar = sym2 + 17

  ! These keywords and symbols are binary operators
  integer,parameter:: first_key = sym_dbar
  integer,parameter:: sym_in = first_key + 1
  integer,parameter:: sym_and = first_key + 2
  integer,parameter:: sym_or = first_key + 3
  integer,parameter:: sym_dim = first_key + 4
  integer,parameter:: sym_by = first_key + 5
  integer,parameter:: sym_includes = first_key + 6
  integer,parameter:: sym_mod = first_key + 7
  integer,parameter:: last_binary = sym_mod

  ! Unary operators 
  integer,parameter:: sym_not = first_key + 8
  integer,parameter:: sym_uby = first_key + 9
  integer,parameter:: last_opr = sym_uby
  integer,parameter:: last_unary = sym_uby

  ! Statement / expression general keywords
  integer,parameter:: sym_opt = last_opr + 1
  integer,parameter:: sym_null = last_opr + 2
  integer,parameter:: sym_key = last_opr + 3
  integer,parameter:: sym_arg = last_opr + 4
  integer,parameter:: sym_argc = last_opr + 5
  integer,parameter:: sym_true = last_opr + 6
  integer,parameter:: sym_false = last_opr + 7
  integer,parameter:: sym_struct = last_opr + 8
  integer,parameter:: sym_rec = last_opr + 9
  integer,parameter:: sym_any = last_opr + 10
  integer,parameter:: sym_distr = last_opr + 11
  integer,parameter:: sym_over = last_opr + 12
  integer,parameter:: sym_present = last_opr + 13
  integer,parameter:: last_expr = sym_present

  integer,parameter:: sym_reduce = last_expr + 1
  integer,parameter:: sym_each = last_expr + 2
  integer,parameter:: sym_then = last_expr + 3
  integer,parameter:: sym_where = last_expr + 4
  integer,parameter:: sym_is = last_expr + 5
  integer,parameter:: sym_with = last_expr + 6   
  integer,parameter:: sym_local = last_expr + 7  
  integer,parameter:: sym_invar = last_expr + 8
  integer,parameter:: sym_conc = last_expr + 9
  integer,parameter:: sym_default = last_expr + 10
  integer,parameter:: sym_global = last_expr + 11
  integer,parameter:: last_key = sym_global

  ! Declaration keywords
  integer,parameter:: sym_include = last_key + 1
  integer,parameter:: first_decl = sym_include
  integer,parameter:: sym_proc = last_key + 2
  integer,parameter:: sym_param = last_key + 3
  integer,parameter:: sym_type = last_key + 4
  integer,parameter:: sym_render = last_key + 5
  integer,parameter:: last_decl = sym_render

  ! Statement keywords 
  ! -- anything that can follow the end of a statement
  integer,parameter:: sym_var = last_decl + 1
  integer,parameter:: first_stmt = sym_var
  integer,parameter:: sym_check = last_decl + 2
  integer,parameter:: sym_const = last_decl + 3
  integer,parameter:: sym_debug  = last_decl + 4
  integer,parameter:: sym_else = last_decl + 5
  integer,parameter:: sym_elseif = last_decl + 6 
  integer,parameter:: sym_enddebug = last_decl + 7 
  integer,parameter:: sym_enddo = last_decl + 8
  integer,parameter:: sym_endif = last_decl + 9
  integer,parameter:: sym_endfind = last_decl + 10
  integer,parameter:: sym_endfor = last_decl + 11
  integer,parameter:: sym_endproc = last_decl + 12
  integer,parameter:: sym_endselect = last_decl + 13
  integer,parameter:: sym_endtype = last_decl + 14
  integer,parameter:: sym_endwhile = last_decl + 15
  integer,parameter:: sym_for = last_decl + 16
  integer,parameter:: sym_find= last_decl + 17
  integer,parameter:: sym_if = last_decl + 18
  integer,parameter:: sym_repeat = last_decl + 19 
  integer,parameter:: sym_result = last_decl + 20
  integer,parameter:: sym_select = last_decl + 21
  integer,parameter:: sym_until = last_decl + 22
  integer,parameter:: sym_using = last_decl + 23
  integer,parameter:: sym_otherwise = last_decl + 24
  integer,parameter:: sym_sync = last_decl + 25        
  integer,parameter:: sym_while = last_decl + 26
  integer,parameter:: sym_return = last_decl + 27
  integer,parameter:: sym_also = last_decl + 28
  integer,parameter:: sym_do = last_decl + 29
  integer,parameter:: sym_when = last_decl + 30
  integer,parameter:: sym_endany = last_decl + 31
  integer,parameter:: last_stmt = sym_endany
  integer,parameter:: num_sym = sym_endany
  

  ! Names of symbols
  character(len=12),parameter,dimension(0:num_sym):: &
       sym_names  = (/ 'EOF         ','$           ','@           ',&
       ';           ','[           ',']           ','{           ',&
       '}           ',':           ','...         ',':=          ',&
       ',           ','.           ','=           ','_           ',&
       '&           ','::          ','?           ','->          ',&
       '%           ','::=         ','''           ','^           ',&
       '<string>    ','<number>    ','<iter>      ','<list>      ',&
       '%{}         ','@{}         ','@{}|        ','{}          ',&
       '<reduce-at> ','<dotref>    ','[]          ','{...}       ',&
       '(...)       ','(           ',')           ','<unary ->   ',&
       '..._        ','_...        ','//          ',&
       '==          ','/=          ','>=          ','>           ',&
       '<=          ','<           ','+           ','-           ',&
       '*           ','/           ','**          ','..          ',&
       '=>          ','|           ','#           ','||          ',&
       'in          ','and         ','or          ',&
       'dim         ','by          ','includes    ','mod         ',&
       'not         ','<unary by>  ','<opt>       ','null        ',&
       'key         ','arg         ','argc        ','true        ',&
       'false       ','struct      ','rec         ','any         ',&
       'distr       ','over        ','present     ','reduce      ',&
       'each        ','then        ','where       ',&
       'is          ','with        ','local       ','invar       ',&
       'conc        ','default     ','global      ','include     ',&
       'proc        ','param       ','type        ',&
       'render      ','var         ','check       ','const       ',&
       'debug       ','else        ','elseif      ','enddebug    ',&
       'enddo       ','endif       ','endfind     ','endfor      ',&
       'endproc     ','endselect   ','endtype     ','endwhile    ',&
       'for         ','find        ','if          ','repeat      ',&
       'result      ','select      ','until       ','using       ',&
       'otherwise   ','sync        ','while       ','return      ',&
       'also        ','do          ','when        ','endany      '&
       /)

  ! Non-reserved words that the compiler needs to know about
  integer,parameter:: sym_array = num_sym + 1
  integer,parameter:: sym_dom = num_sym + 2
  integer,parameter:: sym_tuple = num_sym + 3
  integer,parameter:: sym_optional = num_sym + 4
  integer,parameter:: sym_this_dom = num_sym + 5
  integer,parameter:: sym_this_distr = num_sym + 6
  integer,parameter:: sym_this_tile = num_sym + 7
  integer,parameter:: sym_this_index = num_sym + 8
  integer,parameter:: sym_this_mask = num_sym + 9
  
  integer,parameter:: sym_align = num_sym + 10
  integer,parameter:: sym_part= num_sym + 11
  integer,parameter:: sym_topo = num_sym + 12
  integer,parameter:: sym_block = num_sym + 13
  integer,parameter:: sym_wshare = num_sym + 14
  integer,parameter:: sym_work = num_sym + 15

  integer,parameter:: num_using_clauses = sym_work-sym_part+1
  
  integer,parameter:: hook = num_sym+15
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
  integer,parameter:: sym_shape = hook + 70
  integer,parameter:: sym_yes= hook + 71
  integer,parameter:: sym_no = hook + 72
  integer,parameter:: sym_make_chan = hook + 73
  integer,parameter:: sym_chan = hook + 74
  integer,parameter:: sym_channel = hook + 75
  integer,parameter:: sym_get_over = hook + 76
  integer,parameter:: sym_next_enum = hook + 77
  integer,parameter:: sym_make_distr = hook + 78
  integer,parameter:: sym_check_import = hook + 79
  integer,parameter:: sym_check_return = hook + 80
  integer,parameter:: sym_get_local = hook + 81
  integer,parameter:: sym_get_distr = hook + 82
  integer,parameter:: sym_get_darray = hook + 83
  integer,parameter:: sym_par_dom = hook + 84
  integer,parameter:: sym_seq_dom = hook + 85
  integer,parameter:: num_syshook = 85 + hook-num_sym

  integer,parameter:: loop_call_extra_args=sym_this_index -sym_this_dom+1
   
  character(len=14),dimension(num_syshook),parameter:: syshook = (/ &
       'array         ','dom           ','tuple         ','optional      ',&
       'this_dom      ','this_distr    ','this_tile     ','this_index    ',&
       'this_mask     ','align         ','part          ','topo          ',&
       'block         ','wshare        ','work          ',&
       'PM__system    ','PM__get_elem  ','PM__set_elem  ','size          ',&
       'PM__import_val','PM__importvarg','PM__export    ','PM__partition ',&
       'check_conform ','PM__dup       ','assemble      ','prc_grid      ',&
       'this_prc      ','grid          ','indices       ','PM__ref       ',&
       'PM__getref    ','PM__setref    ','PM__dotref    ','PM__subref    ',&
       'PM__openref   ','index         ','convert       ',&
       'd1            ','d2            ','d3            ','d4            ',&
       'd5            ','d6            ','d7            ',&
       'PM__getkey    ','PM__makekeys  ','PM__delkeys   ','PM__checkkeys ',&
       'PM__getvkey   ','PM__assign    ','PM__first     ','PM__next      ',&
       'check_case    ','dim1          ','dim2          ','dim3          ',&
       'dim4          ','dim5          ','dim6          ','dim7          ',&
       'PM__extract1st','PM__generate  ','PM__extract   ','broadcast     ',&
       'single_elem   ','PM__pop_prc   ','PM__set_param ','PM__get_param ',&
       'PM__extractelm','PM__makearray ','PM__conc      ','PM__pop_conc  ',&
       'PM__exparray  ','PM__opt_num   ','PM__make_mask ','prc_for       ',&
       'PM__impscalar ','PM__setaelem  ','PM__assign_var','vector        ',&
       'matrix        ','PM__pdup      ','PM__do_dim    ','shape         ',&
       'yes           ','no            ','PM__make_chan ','PM__chan      ',&
       'PM__channel   ','PM__over      ','next_enum     ','PM__distr     ',&
       'PM__checkimp  ','PM__checkrtn  ','PM__get_local ','PM__get_distr ',&
       'PM__get_darray','PM__pardom    ','PM__seqdom    '&
       /)
  
contains

  ! Initialise global symbol+name table
  subroutine pm_init_names(context)
    type(pm_context),pointer:: context
    integer:: i,j
    
    context%names=pm_set_new(context,128_pm_ln)
    do i=1,num_sym
       j=pm_name_entry(context,trim(sym_names(i)))
       if(i/=j) then
          write(*,*) i,i+hook,j,hook,trim(syshook(i))
          call pm_panic('Setting name table -1')
       endif
    enddo
    do i=1,num_syshook
       if(syshook(i)(1:1)=='_') then
          j=pm_lname_entry(context,int(sym_pm_system,pm_p),&
               trim(syshook(i)(2:)))
          if(i+num_sym/=j) then
             write(*,*) 'L',i,i+num_sym,j,num_sym
             call pm_panic('Setting name table -2')
          endif
       else
          j=pm_name_entry(context,trim(syshook(i)))
          if(i+num_sym/=j) then
             write(*,*) i,i+num_sym,j,trim(syshook(i))
             call pm_panic('Setting name table - 3')
          endif
       endif
    enddo
  end subroutine pm_init_names
  
  ! Add name to global dictionary
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

  ! Add local name (_name) to global dictionary
  function pm_lname_entry(context,modl_name,string) result(val)
    type(pm_context),pointer:: context
    integer(pm_p):: modl_name
    character(len=*),intent(in):: string
    integer:: val
    integer:: sym
    sym=pm_name_entry(context,string)
    context%temp_obj3=pm_fast_newnc(context,pm_int,&
         2_pm_p)
    context%temp_obj3%data%i(context%temp_obj3%offset)=modl_name
    context%temp_obj3%data%i(context%temp_obj3%offset+1)=sym
    sym=pm_set_lookup(context,context%names,context%temp_obj3)
    if(sym==0) then
       sym=pm_set_add(context,context%names,context%temp_obj3)
    endif
    val=sym
  contains
    include 'fnewnc.inc'
  end function pm_lname_entry

  ! Create vector of names 
  function pm_name_vector(context,vect,base,n) result(sym)
    type(pm_context),pointer:: context
    integer,intent(in),dimension(*):: vect
    integer(pm_p):: sym
    integer,intent(in):: base,n
    context%temp_obj3=pm_fast_newnc(context,pm_int16,&
         int(n-base,pm_p))
    context%temp_obj3%data%i16(context%temp_obj3%offset:&
         context%temp_obj3%offset+n-base-1)=vect(base+1:n)
    sym=pm_set_lookup(context,context%names,context%temp_obj3)
    if(sym==0) then
       sym=pm_set_add(context,context%names,context%temp_obj3)
    endif
  contains
    include 'fnewnc.inc'
  end function  pm_name_vector
  
  ! Create a name value from a string
  function pm_intern(context,str) result(n)
    type(pm_context),pointer:: context
    character(len=*),intent(in):: str
    integer(pm_p):: n
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

  ! Intern a value
  function pm_intern_val(context,val) result(n)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: val
    integer(pm_p):: n
    n=pm_set_lookup(context,context%names,val)
    if(n==0) then
       n=pm_set_add(context,context%names,val)
    endif
  end function pm_intern_val

  ! Is name local to a module
  function pm_name_is_local(context,name) result(ok)
    type(pm_context),pointer:: context
    integer(pm_p),intent(in):: name
    logical::ok
    ok=pm_fast_vkind(pm_name_val(context,name))==pm_int
  contains
    include 'fvkind.inc'
  end function pm_name_is_local

  ! Is name local to a module
  function pm_name_is_vector(context,name) result(ok)
    type(pm_context),pointer:: context
    integer(pm_p),intent(in):: name
    logical::ok
    ok=pm_fast_vkind(pm_name_val(context,name))==pm_int16
  contains
    include 'fvkind.inc'
  end function pm_name_is_vector

  ! Get name value
  function pm_name_val(context,m) result(val)
    type(pm_context),pointer:: context
    integer(pm_p):: m
    type(pm_ptr):: val
    type(pm_ptr):: keys
    keys=pm_set_keys(context,context%names)
    if(pm_debug_level>0) then
       if(m<1.or.m>pm_fast_esize(keys)) &
            call pm_panic('name_val')
    endif
    val=keys%data%ptr(keys%offset+abs(m)-1)
  contains
    include 'fesize.inc'
  end function pm_name_val

  ! Get name string from name entry
  recursive subroutine pm_name_string(context,m,str)
    type(pm_context),pointer:: context
    integer(pm_p),intent(in):: m
    character(len=*),intent(out):: str
    integer(pm_p):: n
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
             call pm_strval(ptr,str)
          else if(pm_fast_vkind(ptr)==pm_int16) then
             first=ptr%data%i16(ptr%offset)
             if(pm_fast_esize(ptr)>0) then
                second=ptr%data%i16(ptr%offset+1_pm_p)
                if(first==sym_pct) then
                   call pm_name_string(context,&
                        int(second,pm_p),str2)
                   str=trim(str2)//'%'
                elseif(first==sym_dcolon) then
                   call pm_name_string(context,&
                        int(second,pm_p),str2)
                   str=trim(str2)//'::'
                elseif(first==sym_gt) then
                   call pm_name_string(context,&
                        int(second,pm_p),str)
                elseif(first==sym_reduce) then
                   i=second
                   str='reduce.'//'0123456789'(i/10+1:i/10+1)//&
                        '0123456789'(mod(i,10):mod(i,10))
                elseif(first>0.and.second<64.and.second>0) then
                   call pm_name_string(context,&
                        int(first,pm_p),str2)
                   str=trim(str2)//'{'//repeat(',',&
                        second-1)//'}'
                elseif(first<0) then
                   if(first==-sym_pm_system.or.&
                        first==-pm_main_module) then
                      call pm_name_string(context,&
                           int(second,pm_p),str)
                   else
                      call pm_name_string(context,&
                           int(-first,pm_p),str)
                      call pm_name_string(context,&
                           int(second,pm_p),str2)
                      str=trim(str)//'.'//str2
                   endif
                else
                   do i=0,pm_fast_esize(ptr)
                      call pm_name_string(context,&
                           int(ptr%data%i16(ptr%offset+i),pm_p),str2)
                      if(i==0) then
                         str=trim(str2)
                      else
                         str=trim(str)//'.'//trim(str2)
                      endif
                   enddo
                endif
             else
                call pm_name_string(context,&
                     int(first,pm_p),str)
             endif
          else if(pm_fast_vkind(ptr)==pm_int) then
             first=ptr%data%i(ptr%offset)
             call pm_name_string(context,&
                  int(ptr%data%i(ptr%offset+1),pm_p),str2)
             if(first==sym_pm_system) then
                str='PM__system._'//trim(str2)
             else
                str='_'//trim(str2)
             endif
          else
             str='?type'
          endif
       else
          str='???'
       endif
    else if(n<0) then
       str='include '
       call pm_name_string(context,-n,str(9:))
    else
       str='EOF'
    endif
  contains
    include 'fvkind.inc'
    include 'fesize.inc'
  end subroutine pm_name_string

  ! Returns name as a string
  function pm_name_as_string(context,m) result(str)
    type(pm_context),pointer:: context
    integer(pm_p):: m
    character(len=300):: str
    str=''
    call pm_name_string(context,m,str)
  end function pm_name_as_string

end module pm_symbol
