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

! Lexical analysis and Parser
! Builds a parse tree from input text
module pm_parser
  use pm_kinds
  use pm_memory
  use pm_lib
  implicit none

  ! Print out lots of parser debugging info
  logical,parameter:: parser_xtra_debug=.false.

  ! Check if memory manager attempts to reuse a node
  ! (this should not happen so is one test of gc)
  logical,parameter:: check_node_reuse=.false.

  integer,parameter:: pm_max_args=32
  
  ! Offsets into module objects
  integer,parameter:: modl_name=1
  integer,parameter:: modl_link=2
  integer,parameter:: modl_stmts=3
  integer,parameter:: modl_include=4
  integer,parameter:: modl_proc=5
  integer,parameter:: modl_type=6
  integer,parameter:: modl_default=7
  integer,parameter:: modl_param=8
  integer,parameter:: modl_local=4

  ! Offsets into parser node objects of various kinds
  integer,parameter:: node_magic=0
  integer,parameter:: node_symbol=1
  integer,parameter:: node_modl=2
  integer,parameter:: node_lineno=3
  integer,parameter:: node_args=4

  ! Type parse nodes
  integer,parameter:: typ_name=node_args
  integer,parameter:: typ_number=node_args+1
  integer,parameter:: typ_module=node_args+2
  integer,parameter:: typ_params=node_args+3
  integer,parameter:: typ_link=node_args+4
  integer,parameter:: typ_includes=node_args+5
  integer,parameter:: typ_num_args=6

  ! Proc parse nodes
  integer,parameter:: proc_name=node_args
  integer,parameter:: proc_link=node_args+1
  integer,parameter:: proc_module=node_args+2
  integer,parameter:: proc_flags=node_args+3
  integer,parameter:: proc_params=node_args+4
  integer,parameter:: proc_keys=node_args+5
  integer,parameter:: proc_amplocs=node_args+6
  integer,parameter:: proc_numloop=node_args+7
  integer,parameter:: proc_coded_params=node_args+8
  integer,parameter:: proc_numret=node_args+9

  ! Alternative final sections for 'proc' parse nodes
  
  !   - user functions
  integer,parameter:: proc_reduce=node_args+10
  integer,parameter:: proc_check=node_args+11
  integer,parameter:: proc_result=node_args+12
  integer,parameter:: proc_stmts=node_args+13
  integer,parameter:: proc_code_tree=node_args+14
  integer,parameter:: proc_num_args=15

  !   - built in functions
  integer,parameter:: proc_rettypes=node_args+10
  integer,parameter:: proc_retas=node_args+11
  integer,parameter:: proc_opcode=node_args+12
  integer,parameter:: proc_opcode2=node_args+13
  integer,parameter:: proc_data=node_args+14
  integer,parameter:: proc_coded_builtin=node_args+15
  integer,parameter:: sysproc_num_args=16

  ! Flags for proc objects
  integer,parameter:: proc_is_reduce=1
  integer,parameter:: proc_is_loop_proc=2
  integer,parameter:: proc_has_vkeys=4
  integer,parameter:: proc_is_var=8

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
  integer,parameter:: sym_bar = 15
  integer,parameter:: sym_amp = 16
  integer,parameter:: sym_dcolon = 17 
  integer,parameter:: sym_query = 18
  integer,parameter:: sym_arrow = 19
  integer,parameter:: sym_hash = 20
  integer,parameter:: sym_pct = 21
  integer,parameter:: sym_string = 22
  integer,parameter:: sym_number = 23

  ! Node types not directly associated with a symbol
  integer,parameter:: node0 = sym_number
  integer,parameter:: sym_iter = node0 + 1
  integer,parameter:: sym_list = node0 + 2
  integer,parameter:: sym_square_at = node0 + 3
  integer,parameter:: sym_brace_at = node0 + 4
  integer,parameter:: sym_reduce_at = node0 + 5
  integer,parameter:: sym_dotref = node0 + 6

  ! Operators
  integer,parameter:: sym1 = sym_dotref
  integer,parameter:: sym_open = sym1 + 1
  integer,parameter:: first_opr = sym_open
  integer,parameter:: sym_close = sym1 + 2
  integer,parameter:: sym_ustar = sym1 + 3
  integer,parameter:: sym_uminus = sym1 + 4
  integer,parameter:: sym2 = sym_uminus
  integer,parameter:: first_unary = sym_ustar

  integer,parameter:: sym_concat = sym2 + 1
  integer,parameter:: first_binary = sym_concat
  integer,parameter:: sym_query_assign = sym2 + 2
  integer,parameter:: sym_eq = sym2 + 3
  integer,parameter:: sym_ne = sym2 + 4
  integer,parameter:: sym_ge = sym2 + 5
  integer,parameter:: sym_gt = sym2 + 6
  integer,parameter:: sym_le = sym2 + 7
  integer,parameter:: sym_lt = sym2 + 8
  integer,parameter:: sym_plus = sym2 + 9
  integer,parameter:: sym_minus = sym2 + 10
  integer,parameter:: sym_mult = sym2 + 11
  integer,parameter:: sym_divide = sym2 + 12
  integer,parameter:: sym_pow = sym2 + 13
  integer,parameter:: sym_dotdot = sym2 + 14
  integer,parameter:: sym_ltdotdot = sym2 + 15
  integer,parameter:: sym_dotdotlt = sym2 + 16
  integer,parameter:: sym_ltdotdotlt = sym2 + 17
  integer,parameter:: sym_cond = sym2 + 18
  integer,parameter:: sym_dbar = sym2 + 19

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
  integer,parameter:: last_opr = sym_not
  integer,parameter:: last_unary = sym_not

  ! Statement / expression general keywords
  integer,parameter:: sym_opt = last_opr + 1
  integer,parameter:: sym_null = last_opr + 2
  integer,parameter:: sym_key = last_opr + 3
  integer,parameter:: sym_arg = last_opr + 4
  integer,parameter:: sym_argc = last_opr + 5
  integer,parameter:: sym_true = last_opr + 6
  integer,parameter:: sym_false = last_opr + 7
  integer,parameter:: sym_lo = last_opr + 8
  integer,parameter:: sym_hi = last_opr + 9
  integer,parameter:: sym_step = last_opr + 10
  integer,parameter:: sym_struct = last_opr + 11
  integer,parameter:: sym_rec = last_opr + 12
  integer,parameter:: sym_from = last_opr + 13
  integer,parameter:: sym_follow = last_opr + 14
  integer,parameter:: sym_any = last_opr + 15
  integer,parameter:: sym_conc = last_opr + 16
  integer,parameter:: last_expr = sym_conc

  integer,parameter:: sym_reduce = last_expr + 1
  integer,parameter:: sym_seq = last_expr + 2
  integer,parameter:: sym_then = last_expr + 3
  integer,parameter:: sym_loop = last_expr + 4
  integer,parameter:: sym_par_loop = last_expr + 5
  integer,parameter:: sym_conc_loop = last_expr + 6
  integer,parameter:: sym_where = last_expr + 7
  integer,parameter:: sym_when = last_expr + 8     
  integer,parameter:: sym_is = last_expr + 9
  integer,parameter:: sym_with = last_expr + 10   
  integer,parameter:: sym_local = last_expr + 11  
  integer,parameter:: sym_invar = last_expr + 12
  integer,parameter:: last_key = sym_invar

  ! Declaration keywords
  integer,parameter:: sym_use = last_key + 1
  integer,parameter:: first_decl = sym_use
  integer,parameter:: sym_proc = last_key + 2
  integer,parameter:: sym_param = last_key + 3
  integer,parameter:: sym_type = last_key + 4
  integer,parameter:: sym_default = last_key + 5
  integer,parameter:: sym_render = last_key + 6
  integer,parameter:: last_decl = sym_render

  ! Statement keywords 
  ! -- anything that can follow the end of a statement
  integer,parameter:: sym_case = last_decl + 1
  integer,parameter:: first_stmt = sym_case
  integer,parameter:: sym_check = last_decl + 2
  integer,parameter:: sym_const = last_decl + 3
  integer,parameter:: sym_debug  = last_decl + 4
  integer,parameter:: sym_else = last_decl + 5
  integer,parameter:: sym_elseif = last_decl + 6 
  integer,parameter:: sym_enddebug = last_decl + 7 
  integer,parameter:: sym_enddo = last_decl + 8
  integer,parameter:: sym_endif = last_decl + 9
  integer,parameter:: sym_endfor = last_decl + 10
  integer,parameter:: sym_endproc = last_decl + 11
  integer,parameter:: sym_endselect = last_decl + 12
  integer,parameter:: sym_endtype = last_decl + 13
  integer,parameter:: sym_endwhile = last_decl + 14
  integer,parameter:: sym_for = last_decl + 15
  integer,parameter:: sym_find= last_decl + 16
  integer,parameter:: sym_par_find = last_decl + 17
  integer,parameter:: sym_conc_find = last_decl + 18
  integer,parameter:: sym_if = last_decl + 19
  integer,parameter:: sym_repeat = last_decl + 20 
  integer,parameter:: sym_result = last_decl + 21
  integer,parameter:: sym_select = last_decl + 22
  integer,parameter:: sym_until = last_decl + 23
  integer,parameter:: sym_using = last_decl + 24
  integer,parameter:: sym_otherwise = last_decl + 25
  integer,parameter:: sym_sync = last_decl + 26        
  integer,parameter:: sym_while = last_decl + 27
  integer,parameter:: sym_build = last_decl + 28
  integer,parameter:: sym_also = last_decl + 29
  integer,parameter:: sym_do = last_decl + 30
  integer,parameter:: last_stmt = sym_do
  integer,parameter:: num_sym = sym_do
  integer,parameter:: sym_array = num_sym + 1
  integer,parameter:: sym_dom = num_sym + 2
  integer,parameter:: sym_tuple = num_sym + 3
  integer,parameter:: sym_poly = num_sym + 4
  integer,parameter:: last_parser_hook = num_sym + 4
  
  integer,parameter:: max_string=100
  integer,parameter:: max_line=1001
  integer,parameter:: max_parse_stack = 1024
  integer,parameter:: max_errors=20
  character(len=1),parameter:: eof_char=achar(0)

  ! Priority for operators (-ve is right associative)
  integer,parameter,dimension(sym_open:last_opr):: priority= (/ &
       15, &  !sym_open
       15, &  !sym_close
       1, &   !sym_ustar
       1, &   !sym_uminus
       10,&   !sym_concat 
       -6, &  !sym_query_assign 
       -6,&   !sym_eq 
       -6,&   !sym_ne 
       -6,&   !sym_ge 
       -6,&   !sym_gt 
       -6,&   !sym_le 
       -6,&   !sym_lt 
       5,&    !sym_plus 
       5,&    !sym_minus 
       3,&    !sym_mult 
       3,&    !sym_divide 
       2,&    !sym_pow 
       -10,&  !sym_dotdot 
       -10,&  !sym_ltdotdot
       -10,&  !sym_dotdotlt
       -10,&  !sym_ltdotdotlt
       -12,&  !sym_cond 
       12,&   !sym_dbar
       -6, &  !sym_in
       8,&    !sym_and 
       9,&    !sym_or 
       13,&   !sym_dim 
       -11,&  !sym_by 
       -6,&   !sym_includes
       4,&    !sym_mod
       7   &  !sym_not
       /)

  ! Names of symbols
  character(len=12),parameter,dimension(0:num_sym):: &
       sym_names  = (/ 'EOF         ','$           ','at          ',&
       ';           ','(/          ','/)          ','(%          ',&
       '%)          ',':           ','...         ',':=          ',&
       ',           ','.           ','=           ','_           ',&
       ':/          ','&           ','::          ','?           ',&
       '->          ','hash        ','%           ','<string>    ',&
       '<number>    ','<iter>      ','<list>      ',&
       '<square-at> ','<brace-at>  ','<reduce-at> ','<dotref>    ',&
       '(           ',')           ','unary *     ','unary -     ',&
       '//          ','?=          ','==          ','/=          ',&
       '>=          ','>           ','<=          ','<           ',&
       '+           ','-           ','*           ','/           ',&
       '**          ','..          ','<..         ','..<         ',&
       '<..<        ','=>          ','://         ','in          ',&
       'and         ','or          ','dim         ','by          ',&
       'includes    ','mod         ','not         ','opt         ',&
       'null        ','key         ','arg         ','argc        ',&
       'true        ','false       ','low         ','high        ',&
       'step        ','struct      ','rec         ','from        ',&
       'follow      ','any         ','conc        ','reduce      ',&
       'seq         ','then        ','<loop>      ','<par-loop>  ',&
       '<conc-loop> ','where       ','when        ','is          ',&
       'with        ','local       ','invar       ','use         ',&
       'proc        ','param       ','type        ','default     ',&
       'render      ','case        ','check       ','const       ',&
       'debug       ','else        ','elseif      ','enddebug    ',&
       'enddo       ','endif       ','endfor      ','endproc     ',&
       'endselect   ','endtype     ','endwhile    ','for         ',&
       'find        ','<par-find>  ','<conc-find> ',&
       'if          ','repeat      ','result      ','select      ',&
       'until       ','using       ','otherwise   ','sync        ',&
       'while       ','build       ','also        ','do          '&
       /)

  ! Parser state
  type parse_state
     type(pm_context),pointer:: context
     type(pm_ptr):: modl,modls,modl_dict,sysmodl
     character(len=max_line),dimension(2):: line
     integer:: ls,lineno
     logical:: newline,atstart
     integer:: n, nsym, last, iunit
     type(pm_ptr):: temp, lexval
     integer:: sym, pushback
     integer,dimension(max_parse_stack):: stack
     integer:: top
     type(pm_ptr),dimension(max_parse_stack):: vstack
     integer(pm_i16),dimension(max_parse_stack):: vline
     integer:: vtop
     integer:: error_count
     type(pm_reg),pointer:: reg
  end type parse_state
  
contains

  ! Start a module declaration with the given name
  subroutine dcl_module(parser,name)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: name
    call new_modl(parser,name_entry(parser,name))
    parser%modl=parser%modls
    parser%modls=parser%modls%data%ptr(parser%modls%offset+modl_link)
  end subroutine dcl_module

  ! Declare an internally implemented procedure 
  subroutine dcl_proc(parser,def,opcode,opcode2,line,flags)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: def
    integer(pm_i16),intent(in):: opcode,opcode2
    integer,intent(inout):: line
    integer,intent(in):: flags

    call parse_from_string(parser,def)
    parser%lineno=line
    if(pm_debug_level>1) &
         write(*,*) 'Parse intrinsic def(',line,') ',trim(def)
    line=line+1
    if(builtin(parser,opcode,opcode2,pm_null_obj,flags)) &
         call pm_panic('bad intrinsic module')
  end subroutine dcl_proc

  ! Declare an internally implemented procedure 
  subroutine dcl_uproc(parser,def,line)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: def
    integer,intent(inout):: line
    call parse_from_string(parser,def)
    parser%lineno=line
    if(pm_debug_level>1) &
         write(*,*) 'Parse sysem user proc def (',line,'):',trim(def)
    write(45,*) line,'proc:',trim(def)
    line=line+1
    if(proc(parser)) &
         call pm_panic('bad intrinsic module')
  end subroutine dcl_uproc

  ! Declare a type
  subroutine dcl_type(parser,def,line)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: def
    integer,intent(inout):: line
    call parse_from_string(parser,def)
    parser%lineno=line
    if(pm_debug_level>1) &
         write(*,*) 'Parse sysem user type def (',line,'):',trim(def)
    write(45,*)line,'type:',trim(def)
    line=line+1
    if(dectype(parser)) &
         call pm_panic('bad intrinsic type')
  end subroutine dcl_type

  ! Start parsing PM code from a string
  subroutine parse_from_string(parser,str)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: str
    parser%ls=1
    parser%line(parser%ls)=str
    parser%line(2)=""
    parser%last=len_trim(parser%line(parser%ls))+1
    parser%n=1
    parser%lineno=1
    parser%newline=.true.
  end subroutine parse_from_string

  ! Initialise the PM parser
  subroutine init_parser(parser,context)
    type(parse_state),intent(inout)::parser
    type(pm_context),pointer:: context
    integer:: i
    type(pm_ptr)::val
    parser%context=>context
    parser%line(1)=' '
    parser%line(2)=' '
    parser%ls=1
    parser%lineno=0
    parser%newline=.false.
    parser%atstart=.false.
    parser%n=1
    parser%last=1
    parser%sym=sym_eof
    parser%pushback=-1
    parser%top=1
    parser%error_count=0
    parser%vtop=max_parse_stack
    parser%reg=>pm_register(context,'parser',&
         parser%modl,parser%modls,parser%modl_dict,&
         parser%temp,parser%sysmodl,parser%lexval, &
         array=parser%vstack, &
         array_size=parser%vtop)
    parser%context%names=pm_set_new(context,128_pm_ln)
    parser%context%lnames=pm_set_new(context,128_pm_ln)
    parser%modl_dict=pm_dict_new(context,128_pm_ln)
    parser%modl=pm_null_obj
    parser%modls=pm_null_obj
    parser%vtop=0
    do i=1,num_sym
       !write(*,*) 'Init parser',i,sym_names(i)
       if(i<=first_key) then
          parser%temp=pm_new_string(context,'"'//trim(sym_names(i))//'"')
       else
          parser%temp=pm_new_string(context,trim(sym_names(i)))
       endif
       if(pm_set_add(context,parser%context%names,parser%temp)&
            /=i) then
          call pm_panic('panic forming key table')
       endif
    enddo
    parser%temp=pm_new_string(context,'array')
    if(pm_set_add(context,parser%context%names,parser%temp)&
         /=sym_array) then
       call pm_panic('panic forming key table -- array')
    endif
    parser%temp=pm_new_string(context,'dom')
    if(pm_set_add(context,parser%context%names,parser%temp)&
         /=sym_dom) then
       call pm_panic('panic forming key table -- dom')
    endif
    parser%temp=pm_new_string(context,'tuple')
    if(pm_set_add(context,parser%context%names,parser%temp)&
         /=sym_tuple) then
       call pm_panic('panic forming key table -- tuple')
    endif
    parser%temp=pm_new_string(context,'poly')
    if(pm_set_add(context,parser%context%names,parser%temp)&
         /=sym_poly) then
       call pm_panic('panic forming key table -- poly')
    endif
  end subroutine init_parser

  ! Terminate the PM parser
  subroutine term_parser(parser)
    type(parse_state),intent(inout):: parser
    call pm_delete_register(parser%context,parser%reg)
  end subroutine term_parser

  ! Get next line of PM code
  subroutine next_line(parser)
    type(parse_state),intent(inout):: parser
    integer:: ios
    parser%ls=3-parser%ls
    parser%lineno=parser%lineno+1
    if(parser_xtra_debug) write(*,*) 'Now at line',parser%lineno
    if(parser%iunit>=0) then
       read(unit=parser%iunit,fmt='(a1000)',&
            iostat=ios,err=10,end=10) parser%line(parser%ls)
       if(ios/=0) parser%line(parser%ls)=eof_char
       parser%last=len_trim(parser%line(parser%ls))+1
       parser%n=1
       parser%newline=.true.
       return
    endif
    10 continue
    parser%n=1
    parser%atstart=.false.
    parser%newline=.false.
    parser%line(parser%ls)=eof_char
  end subroutine next_line

  ! Push back scanned token
  subroutine push_back(parser,sym)
    type(parse_state),intent(inout):: parser
    integer:: sym
    parser%pushback=parser%sym
    parser%sym=sym
  end subroutine push_back

  ! Get next lexical token
  subroutine scan(parser)
    type(parse_state),intent(inout):: parser
    integer:: sym
    type(pm_ptr):: val
    character(len=1):: c
    character(len=max_string):: buffer, dbx
    integer:: n

    ! If token pushed back then return it again
    if(parser%pushback>0) then
       parser%sym=parser%pushback
       parser%pushback=0
       return
    endif
    ! Skip white space and comments
5   continue

    c=getchar()
    do
       do while(isspace(c)) 
          c=getchar()
       enddo
       if(c=='!') then
          call next_line(parser)
          c=getchar()
       endif
       if((.not.isspace(c)).and.(.not.c=='!')) exit
    enddo

    ! Identify token
    parser%nsym=parser%n-1
    select case(c)
    case(eof_char)
       sym=sym_eof
    case('a','b','c','d','e','f','g','h','i','j','k','l','m','n',&
         'o','p','q','r','s','t','u','v','w','x','y','z',&
         'A','B','C','D','E','F','G','H','I','J','K','L','M','N',&
         'O','P','Q','R','S','T','U','V','W','X','Y','Z','_')
       ! PM names
       n=1
       do 
          buffer(n:n)=c
          c=peekchar()
          if(.not.(isalpha(c).or.c=='_'.or.isdigit(c))) exit
          c=getchar()
          n=n+1
       end do
       if(buffer(1:1)=='_') then
          if(n==1) then
             sym=sym_underscore
          else
             sym=lname_entry(parser,buffer(2:n))
          endif
       else
          sym=name_entry(parser,buffer(1:n))
       endif
    case('0','1','2','3','4','5','6','7','8','9')
       call numeric()
    case(',')
       sym=sym_comma
    case(';')
       sym=sym_semi
    case('(')
       if(peekchar()=='/') then
          c=getchar()
          sym=sym_open_square
       else if(peekchar()=='%') then
          c=getchar()
          sym=sym_open_brace
       else
          sym=sym_open
       endif
    case(')')
       sym=sym_close
    case('+')
       sym=sym_plus
    case('-')
       if(peekchar()=='>') then
          c=getchar()
          sym=sym_arrow
       else
          sym=sym_minus
       endif
    case('*')
       if(peekchar()=='*') then
          c=getchar()
          sym=sym_pow
       else
          sym=sym_mult
       endif
    case('/')
       if(peekchar()=='/') then
          c=getchar()
          sym=sym_concat
       elseif(peekchar()=='=') then
          c=getchar()
          sym=sym_ne
       else if(peekchar()==')') then
          c=getchar()
          sym=sym_close_square
       else
          sym=sym_divide
       endif
    case(':')
       if(peekchar()=='=') then
          c=getchar()
          sym=sym_define
       else if(peekchar()=='/') then
          c=getchar()
          if(peekchar()=='/') then
             c=getchar()
             sym=sym_dbar
          else
             sym=sym_bar
          endif
       else if(peekchar()==':') then
          c=getchar()
          sym=sym_dcolon
       else
          sym=sym_colon
       endif
    case('=')
       if(peekchar()=='=') then
          c=getchar()
          sym=sym_eq
       else if(peekchar()=='>') then
          c=getchar()
          sym=sym_cond
       else
          sym=sym_assign
       endif
    case('>')
       if(peekchar()=='=') then
          c=getchar()
          sym=sym_ge
       else
          sym=sym_gt
       endif
    case('<')
       if(peekchar()=='=') then
          c=getchar()
          sym=sym_le
       else
          sym=sym_lt
       endif
    case('%') 
       if(peekchar()=='%') then
          c=getchar()
          sym=sym_at
       elseif(peekchar()==':') then
          c=getchar()
          sym=sym_hash
       elseif(peekchar()==')') then
          c=getchar()
          sym=sym_close_brace
       else
          sym=sym_pct
       endif
    case('.')
       if(peekchar()=='.') then
          c=getchar()
          if(peekchar()=='.') then
             c=getchar()
             sym=sym_dotdotdot
          else
             sym=sym_dotdot
          endif
       else
          sym=sym_dot
       endif
    case('?')
       if(peekchar()=='=') then
          c=getchar()
          sym=sym_query_assign
       else
          sym=sym_query
       endif
    case('"')
       n=0
       c=getchar()
       do while(c/='"')
          n=n+1
          buffer(n:n)=c
          if(n>=max_string) exit
          c=getchar()
       enddo
       val=pm_new_string(parser%context,buffer(1:n))
       sym=sym_string
       parser%lexval=val
    case('&')
       sym=sym_amp
    case('$')
       sym=sym_dollar
       ! *********************************************    
       ! These options require extended character set 
       !  delete if not available
       ! *********************************************
    case('[')
       sym=sym_open_square
    case(']')
       sym=sym_close_square
    case('{')
       sym=sym_open_brace
    case('}')
       sym=sym_close_brace  
    case('|')       
       if(peekchar()=='|') then
          sym=sym_dbar
          c=getchar()
       else
          sym=sym_bar
       endif
    case('@')
       sym=sym_at
    case('#')
       sym=sym_hash   
       ! ****************************************
       ! End of extended character set options
       ! ****************************************
    case default
       write(*,*) 'Error: Unexpected character "',c,'" ascii code=',iachar(c)
       c=getchar()
       goto 5
    end select
10  continue
    if(parser%newline) then
       parser%newline=.false.
       parser%atstart=.true.
    else
       parser%atstart=.false.
    endif
    parser%sym=sym
    if(parser_xtra_debug) then
       if(parser%sym>=0.and.parser%sym<=num_sym) then
          write(*,*) 'scan:',parser%sym,sym_names(parser%sym)
       else
          call pm_name_string(parser%context,int(parser%sym,pm_p),dbx)
          write(*,*) 'scan:',parser%sym,trim(dbx)
       endif
    endif
  contains
    
    include 'fnew.inc'

    ! Get next character from current line and advance
    function getchar() result(ch)
      character(len=1):: ch
      ch=parser%line(parser%ls)(parser%n:parser%n)
      parser%n=parser%n+1
      if(parser%n>parser%last) call next_line(parser)
    end function getchar
    
    ! Look at next character on line but do not advance
    function peekchar() result(ch)
      character(len=1):: ch
      ch=parser%line(parser%ls)(parser%n:parser%n)
    end function peekchar

    ! Look at next 2 characters on line but do not advance
    function peekchar2() result(ch)
      character(len=2):: ch
      ch=parser%line(parser%ls)(parser%n:parser%n+1)
    end function peekchar2

    ! Is character a letter or _
    function isalpha(c) result(yes)
      character(len=1),intent(in):: c
      logical:: yes
      yes=(iachar(c)>=iachar('a').and. &
           iachar(c)<=iachar('z')).or.&
           (iachar(c)>=iachar('A').and.&
           iachar(c)<=iachar('Z'))&
           .or. c=='_'
    end function isalpha

    ! Is character a digit
    function isdigit(c) result(yes)
      character(len=1),intent(in):: c
      logical:: yes
      yes=(iachar(c)>=iachar('0').and.&
           iachar(c)<=iachar('9'))
    end function isdigit

    ! Is character white space?
    function isspace(c) result(yes)
      character(len=1),intent(in):: c
      logical:: yes
      yes=(c==' ')
    end function isspace
    
    ! Numerical constants
    subroutine numeric
      integer:: n,ibase,ibits,ios
      logical:: isreal,issingle,iscomplex
      integer(pm_i128):: inumber
      real:: rv
      real(pm_d) :: dv
      real(pm_r32) :: r32
      real(pm_r64) :: r64
      real(pm_r128) :: r128
      n=0
      isreal=.false.
      issingle=.true.
      iscomplex=.false.
      do 
         n=n+1
         buffer(n:n)=c
         if(.not.isdigit(peekchar())) exit
         c=getchar()
      end do
      c=peekchar()
      if(c=='r') then
         c=getchar()
         read(unit=buffer,fmt='(G3.0)') ibase
         if(ibase>36) then
            call parse_error(parser, &
                 "Bad numeric base for non-decimal integer")
         else
            inumber=0
            do
               c=peekchar()
               if(.not.(isalpha(c).or.isdigit(c))) exit
               c=getchar()
               n=iachar(c)
               if(n>=iachar('a')) then
                  n=n-iachar('a')+10
               else if(n>=iachar('A')) then
                  n=n-iachar('A')+10
               else
                  n=n-iachar('0')
               endif
               if(n>=ibase) then
                  call parse_error(parser,"Bad digit for this base")
                  inumber=0
                  exit
               endif
               inumber=ibase*inumber+n
            enddo
            write(unit=buffer,fmt='(i40)') inumber
            n=len_trim(buffer)+1
         endif
       else if(c/='l') then
          if(c=='.') then
             if(peekchar2()=='..') goto 10
             c=getchar()
             isreal=.true.
             n=n+1
             buffer(n:n)=c
             do
                if(.not.isdigit(peekchar())) exit
                c=getchar()
                n=n+1
                buffer(n:n)=c
             enddo
             c=peekchar()
          end if
          if(c=='e'.or.c=='d'.or.c=='f') then
             c=getchar()
             if(c=='d') issingle=.false.
             c='e'
             isreal=.true.
             n=n+1
             buffer(n:n)=c
             c=peekchar()
             ibase=n
             if(c=='+'.or.c=='-') then
                c=getchar()
                n=n+1
                buffer(n:n)=c
             endif
             do while(isdigit(peekchar()))
                c=getchar()
                n=n+1
                buffer(n:n)=c
             enddo
             c=peekchar()
             if(n==ibase) n=n-1
          end if
          if(c=='i'.or.c=='j') then
             c=getchar()
             isreal=.true.
             iscomplex=.true.
          endif
       else
          issingle=.false.
          c=getchar()
       endif
10     continue
       ios=0
       c=peekchar()
       if(c=='_') then
          c=getchar()
          ibase=n+1
          buffer(ibase:ibase)=' '
          do
             if(.not.isdigit(peekchar())) exit
             c=getchar()
             n=n+1
             buffer(n:n)=c
          enddo
          read(unit=buffer(ibase:n),fmt='(G3.0)',iostat=ios) ibits
          if(ibase<40) buffer(ibase:40)=' '
          if(iscomplex) then
             select case(ibits)
             case(64)
                val=pm_new_small(parser%context,pm_complex64,1_pm_p)
                read(unit=buffer,fmt='(G40.0)', &
                     iostat=ios) r32
                val%data%c64(val%offset)= &
                     cmplx(0.0,r32,kind=pm_r32)
             case(128)
                val=pm_new_small(parser%context,pm_complex128,1_pm_p)
                read(unit=buffer,fmt='(G40.0)', &
                     iostat=ios) r64
                val%data%c128(val%offset)= &
                     cmplx(0.0,r32,kind=pm_r64)
             case(256)
                val=pm_new_small(parser%context,pm_complex256,1_pm_p)
                read(unit=buffer,fmt='(G40.0)', &
                     iostat=ios) r128
                val%data%c256(val%offset)= &
                     cmplx(0.0,r128,kind=pm_r128)
             case default
                call parse_error(parser, &
                     'Incorrect complex value bit size')
                val=pm_null_obj
             end select
          else if(isreal) then
             select case(ibits)
             case(32)
                val=pm_new_small(parser%context,pm_real32,1_pm_p)
                read(unit=buffer,fmt='(G40.0)', &
                     iostat=ios) val%data%r32(val%offset)
             case(64)
                val=pm_new_small(parser%context,pm_real64,1_pm_p)
                read(unit=buffer,fmt='(G40.0)', &
                     iostat=ios) val%data%r64(val%offset)
             case(128)
                val=pm_new_small(parser%context,pm_real128,1_pm_p)
                read(unit=buffer,fmt='(G40.0)', &
                     iostat=ios) val%data%r128(val%offset)
             case default
                call parse_error(parser, &
                     'Incorrect real value bit size')
                val=pm_null_obj
             end select
          else 
             select case(ibits)
             case(8)
                val=pm_new_small(parser%context,pm_int8,1_pm_p)
                read(unit=buffer,fmt='(G40.0)', &
                     iostat=ios) val%data%i8(val%offset)
             case(16)
                val=pm_new_small(parser%context,pm_int16,1_pm_p)
                read(unit=buffer,fmt='(G40.0)', &
                     iostat=ios) val%data%i16(val%offset)
             case(32)
                val=pm_new_small(parser%context,pm_int32,1_pm_p)
                read(unit=buffer,fmt='(G40.0)', &
                     iostat=ios) val%data%i32(val%offset)
             case(64)
                val=pm_new_small(parser%context,pm_int64,1_pm_p)
                read(unit=buffer,fmt='(G40.0)', &
                     iostat=ios) val%data%i64(val%offset)
             case(128)
                val=pm_new_small(parser%context,pm_int128,1_pm_p)
                read(unit=buffer,fmt='(G40.0)', &
                     iostat=ios) val%data%i128(val%offset)
             case default
                call parse_error(parser, &
                     'Incorrect integer value bit size')
                val=pm_null_obj
             end select
          endif
       else
          buffer(n+1:)=' '
          if(iscomplex) then
             if(issingle) then
                val=pm_new_small(parser%context, &
                     pm_single_complex,1_pm_p)
                read(unit=buffer,fmt='(G40.0)',iostat=ios) rv
                val%data%c(val%offset)=cmplx(0.0,rv)
             else
                val=pm_new_small(parser%context, &
                     pm_double_complex,1_pm_p)
                read(unit=buffer,fmt='(G40.0)',iostat=ios) dv
                val%data%dc(val%offset)=cmplx(0.0,dv,kind=pm_d)
             endif
          else if(isreal) then
             if(issingle) then
                val=pm_new_small(parser%context,pm_single,1_pm_p)
                read(unit=buffer,fmt='(G40.0)',iostat=ios) &
                     val%data%r(val%offset)
             else
                val=pm_new_small(parser%context,pm_double,1_pm_p)
                read(unit=buffer,fmt='(G40.0)',iostat=ios) &
                     val%data%d(val%offset)
             endif
          else
             if(issingle) then
                val=pm_new_small(parser%context,pm_int,1_pm_p)
                read(unit=buffer,fmt='(G40.0)',iostat=ios) &
                     val%data%i(val%offset)
             else
                val=pm_new_small(parser%context,pm_long,1_pm_p)
                read(unit=buffer,fmt='(G40.0)',iostat=ios) &
                     val%data%ln(val%offset)
             endif
          endif
       endif
       if(ios/=0) call parse_error(parser, &
            'Numeric constant out of range')
       parser%lexval=val
       sym=sym_number
     end subroutine numeric

  end subroutine  scan

  ! Next token must be specific token or error raised
  function expect(parser,sym,mess) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    character(len=*),intent(in),optional:: mess
    logical:: iserr
    if(parser%sym==sym) then
       iserr=.false.
       call scan(parser)
    else
       iserr=.true.
       if(present(mess)) then
          call parse_error(parser,'Expected '//mess)
       else
          call parse_error(parser,'Expected "'//trim(sym_names(sym))//'"')
       endif
    endif
  end function expect

  ! Nexr token must be a name
  function expect_name(parser,mess) result(iserr)
    type(parse_state),intent(inout)::parser
    character(len=*),intent(in),optional:: mess
    logical:: iserr
    if(parser%sym>=0.and.parser%sym<=num_sym) then
       iserr=.true.
       if(present(mess)) then
          call parse_error(parser,'Expected: '//trim(mess))
       else
          call parse_error(parser,'Expected name')
       endif
    else
       call push_sym_val(parser,parser%sym)
       call scan(parser)
       iserr=.false.
    endif
  end function expect_name

 ! If next token is a name, push its value and scan
  function is_name(parser) result(ok)
    type(parse_state),intent(inout)::parser
    logical:: ok
    if(parser%sym>=0.and.parser%sym<=num_sym) then
       ok=.false.
    else
       call push_sym_val(parser,parser%sym)
       call scan(parser)
       ok=.true.
    endif
  end function is_name

 ! If next token is a name, return its value and scan
  function check_name(parser,sym) result(ok)
    type(parse_state),intent(inout)::parser
    integer,intent(out):: sym
    logical:: ok
    if(parser%sym>=0.and.parser%sym<=num_sym) then
       ok=.false.
    else
       sym=parser%sym
       call scan(parser)
       ok=.true.
    endif
  end function check_name

  ! *****************************************************************
  ! Following functions parse PM grammer in mostly top-down fashion
  ! Most parsing routines return true on failure
  ! *****************************************************************

  ! Make a simple call node (no keys or &args)
  ! Name and args must be on stack
  subroutine simple_call(parser,nargs,nloop)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: nargs, nloop
    call make_node(parser,sym_list,nargs)
    call push_null_val(parser)      ! keys
    call push_null_val(parser)      ! amps
    call push_num_val(parser,nloop) ! num loop args
    call make_node(parser,sym_open,5)    
  end subroutine simple_call

  ! Procedure call (scanner on token *after* name)
  recursive function proccall(parser,name) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    logical:: iserr
    iserr=.true.
    if(parser%sym==sym_pct) then
       call push_sym(parser,sym_pct)
       call push_sym(parser,name)
       call name_vector(parser,parser%top-2)
       call scan(parser)
       if(arglist(parser,sym_pct,.true.)) return
    elseif(parser%sym==sym_dcolon) then
       call push_sym(parser,sym_dcolon)
       call push_sym(parser,name)
       call name_vector(parser,parser%top-2)
       call scan(parser)
       if(parser%sym==sym_open) then
          if(arglist(parser,sym_dcolon,.true.)) return
       else
          if(expect_name(parser)) return
          call simple_call(parser,1,1)
       endif
       if(parser%sym==sym_at) then
          call scan(parser)
          if(expect(parser,sym_open_brace)) return
          if(sexprlist(parser)) return
          if(expect(parser,sym_close_brace)) return
          call make_node(parser,sym_reduce_at,2)
       endif
    else
       if(parser%sym/=sym_open) then
          call parse_error(parser,'Expected procedure call')
       endif
       call push_sym_val(parser,name)
       if(arglist(parser,sym_open,.false.)) return
    endif
    iserr=.false.
  end function proccall

  ! Argument lists for procedure calls
  recursive function arglist(parser,clsym,is_loop) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: clsym
    logical,intent(in):: is_loop
    logical:: iserr
    integer m,n,nloop,base,sym
    type(pm_ptr):: temp
    logical:: loop_par
    loop_par=is_loop
    iserr=.true.
    m=0
    n=0
    nloop=0
    base=parser%top
    call scan(parser)
    if(parser%sym==sym_close) then
       call push_null_val(parser) !args
       call push_null_val(parser) !keys
       call push_null_val(parser) !amps
       call push_null_val(parser) !num loop args
       call make_node(parser,clsym,5)
       call scan(parser)
       iserr=.false.
       return
    endif
    do 
       if(parser%sym==sym_amp) then
          call scan(parser)
          if(expect_name(parser)) return
          if(qual(parser)) return
          m=m+1
          if(m>pm_max_args) then
             call parse_error(parser,'"&" too late in long argument list')
          else
             if(parser%top>=max_parse_stack) then
                call parse_error(parser,'expression too complex')
                return
             endif
             call push_sym(parser,m)
          endif
       elseif(parser%sym==sym_key) then
          call make_node(parser,sym_list,m)
          exit
       else if(parser%sym==sym_arg) then
          if(loop_par) then
             call parse_error(parser,&
                  '"arg..." must follow ":" in loop parameter list')
          endif
          call scan(parser)
          if(parser%sym==sym_dotdotdot) then
             call push_sym_val(parser,sym_arg)
             call make_node(parser,sym_dotdotdot,m+1)
             call scan(parser)
             exit
          else
             call push_back(parser,sym_arg)
          endif
       else
          if(check_name(parser,sym)) then
             if(parser%sym==sym_assign) then
                call make_node(parser,sym_list,m)
                call push_sym_val(parser,sym)
                call scan(parser)
                if(expr(parser)) return
                n=1
                exit
             else
                call push_back(parser,sym)
             endif   
          endif
          if(loop_par) then
             if(expect_name(parser)) return
             if(parser%sym/=sym_close.and.&
                  parser%sym/=sym_semi.and.parser%sym/=sym_comma) then
                call parse_error(parser,'Loop argument must be single variable name')
                return
             endif
             nloop=nloop+1
          else
             if(expr(parser)) return
          endif
          m=m+1
       endif
       if(loop_par.and.parser%sym==sym_semi) then
          loop_par=.false.
       elseif(parser%sym/=sym_comma) then
          call make_node(parser,sym_list,m)
          exit
       endif
       call scan(parser)
    enddo
    do while(parser%sym==sym_comma)
       call scan(parser)
       if(parser%sym==sym_key) exit
       if(expect_name(parser,&
            'optional argument name')) return
       if(expect(parser,sym_assign,&
            'optional argument "="')) return
       if(expr(parser)) return
       n=n+1
    enddo
    if(parser%sym==sym_key) then
       call scan(parser)
       if(expect(parser,sym_dotdotdot)) return
       call make_node(parser,sym_dotdotdot,n*2)
    else if(n>0) then
       call make_node(parser,sym_list,n*2)
    else
       call push_null_val(parser)
    endif 
    if(parser%top>base) then
       call name_vector(parser,base)
    else
       call push_null_val(parser)
    endif
    call push_num_val(parser,nloop)
    call make_node(parser,clsym,5)
    if(expect(parser,sym_close)) return
    iserr=.false.
  end function arglist

  ! Qualifiers ( .name or name[explist] or name{explist} )
  recursive function qual(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: inass
    logical:: iserr
    integer:: m, csym
    type(pm_ptr):: p1,p2
    iserr=.true.
    do 
       if(parser%sym==sym_dot) then
          call scan(parser)
          if(expect_name(parser)) return
          call make_node(parser,sym_dot,2)
       else if(parser%sym==sym_open_square) then
          call scan(parser)
          if(sexprlist(parser)) return
          call make_node(parser,sym_open_square,2)
          if(expect(parser,sym_close_square)) return
       else if(parser%sym==sym_open_brace) then
          call scan(parser)
          if(sexprlist(parser)) return
          call make_node(parser,sym_open_brace,2)
          if(expect(parser,sym_close_brace)) return
       else
          exit
       endif
    enddo
    iserr=.false.
  end function qual

  ! Array /set/ vector matrix generator ( expr : expr | expr )
  recursive function generator(parser,sym)result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    logical:: iserr
    integer:: m
    iserr=.true.

    call scan(parser)
    m=0
    do
       if(expect_name(parser)) return
       if(expect(parser,sym_in)) return
       if(expr(parser)) return
       m=m+1
       if(parser%sym/=sym_comma) exit
    enddo
    call make_node(parser,sym_list,m*2)
    if(parser%sym==sym_bar) then
       call scan(parser)
       if(expr(parser)) return
    else
       call push_null_val(parser)
    endif
    call make_node(parser,sym,3)
    if(expect(parser,sym,'closing '//sym_names(sym))) return
    iserr=.false.
  end function generator

  ! Array/matrix list:  ( expr, expr ... ; expr , expr ... ; ...)
  recursive function matrix_former(parser,sym,row,col) &
       result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    logical,intent(in):: row,col
    logical:: iserr
    integer:: m,n,oldm
    if(row) then
       n=1
       oldm=1
    else
       n=0
       oldm=-1
    endif
    do
       m=0
       if(n==0.and.col) m=1
       do
          if(expr(parser)) return
          m=m+1
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       if(oldm>0.and.m/=oldm) then
          call parse_error(parser,'Rows of different lengths in [ ]')
          return
       endif
       oldm=m
       n=n+1
       if(parser%sym==sym_semi) then
          call scan(parser)
       else if(parser%sym==sym) then
          exit
       else if(.not.parser%atstart) then
          exit
       endif
    enddo
    call make_node(parser,sym_list,m*n)
    call push_num_val(parser,n)
    call push_num_val(parser,m)
    call make_node(parser,sym,3)
    if(expect(parser,sym,'closing '//sym_names(sym))) return
    iserr=.false.
  end function matrix_former

  recursive function from_follow(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: m
    iserr=.true.
    m=1
    call scan(parser)
    if(expr(parser)) return
    do
       if(expect(parser,sym_follow))  return
       if(exprlist(parser)) return
       if(expect(parser,sym_when)) return
       if(expr(parser)) return
       if(parser%sym==sym_comma) then
          call scan(parser)
       else
          exit
       endif
    enddo
    call make_node(parser,sym_follow,m)
    iserr=.false.
  end function from_follow

  ! Array formers [ ... ] or { ... }
  recursive function array_former(parser,sym) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    logical:: iserr
    iserr=.true.
    call scan(parser)
    if(parser%sym==sym_from) then
       if(from_follow(parser)) return
       call make_node(parser,sym,1)
       if(expect(parser,sym)) return
       iserr=.false.
       return
    endif
    if(sym==sym_close_brace.and.parser%sym==sym) then
       call make_node(parser,sym,0)
       call scan(parser)
       iserr=.false.
    else
       if(expr(parser)) return
       if(parser%sym==sym_colon) then
          if(generator(parser,sym)) return
       else if(parser%sym==sym_comma) then
          call scan(parser)
          if(matrix_former(parser,sym,.false.,.true.)) return
       else if(parser%sym==sym_semi) then
          call scan(parser)
          if(matrix_former(parser,sym,.false.,.true.)) return
       else if(parser%sym/=sym) then
          if(parser%atstart) then
             if(matrix_former(parser,sym,.true.,.false.)) return
          else
             call parse_error(parser,'Expected: '//sym_names(sym))
          endif
       else
          if(sym/=sym_close) call make_node(parser,sym,1)
          call scan(parser)
       endif
    endif
    iserr=.false.
  end function array_former

  ! Operator symbols within proc 
  recursive function op(parser,sym) result(iserr)
    type(parse_state):: parser
    integer,intent(out):: sym
    logical:: iserr
    iserr=.true.
    select case (parser%sym)
    case(first_unary:last_opr,sym_assign,sym_hi,sym_lo,sym_step)
       sym=parser%sym
       call scan(parser)
    case(sym_open_square)
       call scan(parser)
       if(expect(parser,sym_close_square)) return
       sym=sym_open_square
    case(sym_open_brace)
       call scan(parser)
       if(expect(parser,sym_close_brace)) return
       sym=sym_open_brace
    case(sym_at)
       call scan(parser)
       if(parser%sym==sym_open_square) then
          call scan(parser)
          if(expect(parser,sym_close_square)) return
          sym=sym_square_at
       else if(parser%sym==sym_open_brace) then
          call scan(parser)
          if(expect(parser,sym_close_brace)) return
          sym=sym_brace_at
       else
          sym=sym_at
       endif
    case default
       if(.not.check_name(parser,sym)) then
          call parse_error(parser,'Malformed "proc" identifier')
          return
       endif
    end select
    iserr=.false.
  end function op

  ! Structure expression struct [name] { name=.. }
  recursive function struct_gen(parser,sym) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    logical:: iserr
    integer:: base,vbase,name,i
    iserr=.true.
    base=parser%top
    vbase=parser%vtop
    call scan(parser)
    if(parser%sym==sym_open_brace) then
       call scan(parser)
       call push_sym(parser,0)
    else if(check_name(parser,name)) then
       call push_sym(parser,name)
       if(expect(parser,sym_open_brace)) return
    else
       call parse_error(parser,'Expected structure name')
    endif
    do
       if(check_name(parser,name)) then
          call push_sym(parser,name)
       else
          call parse_error(parser,&
               'Expected name of struct element')
       endif
       if(expect(parser,sym_assign)) return
       if(expr(parser)) return
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    call struct_sort(parser,base+1,vbase)
    call make_node(parser,sym_list,parser%vtop-vbase)
    call name_vector(parser,base)
    call make_node(parser,sym,2)
    if(expect(parser,sym_close_brace)) return
    iserr=.false.
  end function struct_gen

  subroutine struct_sort(parser,base,vbase)
    type(parse_state):: parser
    integer:: base,vbase
    type(pm_ptr):: v
    integer:: n,i,j,jj
    do i=base+2,parser%top
       j=i
       jj=i+vbase-base
       n=parser%stack(j)
       v=parser%vstack(jj)
       if(parser%stack(i-1)<n) cycle
       do while(j>base+1.and.parser%stack(j-1)>n)
          parser%stack(j)=parser%stack(j-1)
          parser%vstack(jj)=parser%vstack(jj-1)
          j=j-1
          jj=jj-1
       enddo
       parser%stack(j)=n
       parser%vstack(jj)=v
    enddo
  end subroutine struct_sort

  ! Term in an expression
  recursive function term(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: m,name,sym
    iserr=.true.
    sym=parser%sym
    select case(sym)
    case(sym_struct,sym_rec)
       if(struct_gen(parser,sym)) return
    case(sym_lt)
       call scan(parser)
       if(parser%sym==sym_gt) then
          call scan(parser)
          call push_null_val(parser)
       else
          if(typ(parser)) return
          if(expect(parser,sym_gt)) return
       endif
       if(term(parser)) return
       call make_node(parser,sym_any,2)
    case(sym_open)
       call scan(parser)
       if(expr(parser)) return
       if(expect(parser,sym_close)) return
    case(sym_open_square)
       if(array_former(parser,sym_close_square)) return
    case(sym_open_brace)
       if(array_former(parser,sym_close_brace)) return
    case(sym_arg)
       call scan(parser)
       if(expect(parser,sym_open_square)) return
       if(expr(parser)) return
       call make_node(parser,sym_arg,1)
       if(expect(parser,sym_close_square)) return
    case(sym_number,sym_string)
       call push_val(parser,parser%lexval)
       call scan(parser)
    case(sym_proc)
       call scan(parser)
       if(op(parser,name)) return
       if(parser%sym==sym_open.or.parser%sym==sym_dcolon&
            .or.parser%sym==sym_pct) then
          if(proccall(parser,name)) return
       else
          call make_node(parser,sym_proc,1)
       endif
    case(sym_hi,sym_lo,sym_step)
       call scan(parser)
       if(parser%sym==sym_open.or.parser%sym==sym_dcolon&
            .or.parser%sym==sym_pct) then
          if(proccall(parser,sym)) return
       else
          call push_sym_val(parser,sym)
       endif
    case(sym_true,sym_false,sym_null,sym_argc) 
       call push_sym_val(parser,parser%sym)
       call scan(parser)
       goto 20
    case(sym_dollar)
       call scan(parser)
       if(expect_name(parser)) return
       call make_node(parser,sym,1)
    case(sym_at)
       call scan(parser)
       if(expect_name(parser)) return
       if(parser%sym==sym_open_square) then
          call scan(parser)
          if(sexprlist(parser)) return
          call make_node(parser,sym_square_at,2)
          if(expect(parser,sym_close_square)) return
       else if(parser%sym==sym_open_brace) then
          call scan(parser)
          if(sexprlist(parser)) return
          call make_node(parser,sym_brace_at,2)
          if(expect(parser,sym_close_brace)) return
       else
          call make_node(parser,sym_at,1)
       endif
    case default
       if(check_name(parser,name)) then
          if(parser%sym==sym_open.or.parser%sym==sym_dcolon&
               .or.parser%sym==sym_pct) then
             if(proccall(parser,name)) return
          elseif(parser%sym==sym_at) then
             call push_sym_val(parser,name)
             call scan(parser)
             if(expect(parser,sym_open_brace)) return
             if(sexprlist(parser)) return
             if(expect(parser,sym_close_brace)) return
             call make_node(parser,sym_at,2)
          else
             call push_sym_val(parser,name)
          endif
       else
          call parse_error(parser,'Malformed expression')
          return
       endif
    end select
    10 continue
    if(qual(parser)) return
    20 continue
    iserr=.false.
  end function term
  
  ! Expression
  recursive function expr(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: base,nesting,s,sym,ntuple
    iserr=.true.
    base=parser%top
    nesting=0        
    do
       do 
          if(parser%sym==sym_minus) then
             parser%sym=sym_uminus
          else if(parser%sym==sym_mult) then
             parser%sym=sym_ustar
          elseif(parser%sym/=sym_not) then
             exit
          endif
          call push()
          call scan(parser)
       enddo

       do while(parser%sym==sym_open) 
          call push()
          call scan(parser)
          nesting=nesting+1
       enddo

       if(term(parser)) goto 999

       do while(nesting>0.and.parser%sym==sym_close)
          if(popto(sym_close)) goto 999
          nesting=nesting-1
          call scan(parser)
          if(qual(parser)) return
       enddo

       if(binary(parser%sym)) then
          s=parser%sym
          call scan(parser)
          if(s==sym_lt) then
             if(parser%sym==sym_dotdot) then
                call scan(parser)
                if(parser%sym==sym_lt) then
                   call scan(parser)
                   s=sym_ltdotdotlt
                else
                   s=sym_ltdotdot
                endif
             endif
          else if(s==sym_dotdot)then
             if(parser%sym==sym_lt) then
                call scan(parser)
                s=sym_dotdotlt
             else
                s=sym_dotdot
             endif
          endif
          if(popto(s)) goto 999
          call push_s(s)
       else if(nesting>0) then
          if(popto(sym_open)) goto 999
          if(parser%sym==sym_comma) then
             call push_sym_val(parser,sym_tuple)
             call swap_vals(parser)
             ntuple=1
             do 
                call scan(parser)
                if(expr(parser)) goto 999
                ntuple=ntuple+1
                if(parser%sym/=sym_comma) exit
             enddo
             call simple_call(parser,ntuple,0)
             if(expect(parser,sym_close)) return
          else
             call parse_error(parser,'Too many "(" in expression')
             goto 999
          endif
          nesting=nesting-1
          if(binary(parser%sym)) then
             if(popto(parser%sym)) goto 999
             call push()
             call scan(parser)
          else
             exit
          endif
       else
          exit
       endif
    enddo
    if(popto(sym_open)) goto 999
    if(parser%top/=base.or.nesting/=0) then
       write(*,*) parser%top,base,nesting
       call parse_error(parser,'Mismatched parentheses')
    else
       iserr=.false.
    endif
    return

 999 continue
    call skip_past_error(parser,.true.)

  contains

    ! Push current symbol onto operator stack
    subroutine push()
      parser%top=parser%top+1
      parser%stack(parser%top)=parser%sym
    end subroutine push

    ! Push symbol onto operator stack
    subroutine push_s(s)
      integer:: s
      parser%top=parser%top+1
      parser%stack(parser%top)=s
    end subroutine push_s

    ! Look at top of operator stack
    function top() result(sym)
      integer:: sym
      sym=parser%stack(parser%top)
    end function top

    ! Pop operator stack
    function pop() result(sym)
      integer:: sym
      sym=parser%stack(parser%top)
      parser%top=parser%top-1
    end function pop
    
    ! Pop operator stack down to priority of a given symbol
    function popto(popsym) result(iserr)
      integer:: popsym
      logical:: iserr
      integer:: sym,n,popp,p,oldp
      logical:: noassoc
      iserr=.true.
      if(popsym==sym_pow) then
         popp=3
      else
         popp=priority(popsym)
         if(popp<0) then
            popp=-popp
            noassoc=.true.
         else
            noassoc=.false.
         endif
      endif
      if(parser%top>base) then
         p=abs(priority(top()))
         do while(popp>=p)
            sym=pop()
            if(sym==sym_open) exit
            if(popp==p.and.noassoc) then
               call parse_error(parser,&
                    'Cannot repeat: "'//sym_names(sym)//'"')
               return
            else if(.not.binary(sym)) then
               n=1
            else
               n=2
            endif
            call make_node(parser,sym,n)
            if(parser%top<=base) exit
            p=abs(priority(top()))
         enddo
      endif
      iserr=.false.
    end function popto

    ! Is sym a binary operator?
    function binary(sym) result(isbin)
      integer:: sym
      logical:: isbin
      isbin = sym>=first_binary .and. sym<=last_binary
    end function binary

  end function expr

  ! Comma separated list of expr
  recursive function exprlist(parser,length) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out),optional:: length
    logical:: iserr
    integer:: n
    iserr=.true.
    n=0
    do
       if(expr(parser)) return
       n=n+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    call make_node(parser,sym_list,n)
    if(present(length)) length=n
    iserr=.false.
  end function exprlist

  ! Subscript expression (includes ...e and e...)
  function sexpr(parser) result(iserr)
    type(parse_state):: parser
    logical:: iserr
    integer:: s
    type(pm_ptr):: junk
    iserr=.true.
    if(parser%sym==sym_dotdotdot) then
       call push_sym_val(parser,sym_lo)
       call scan(parser)
       if(parser%sym==sym_comma.or.&
            parser%sym==sym_close_brace.or.&
            parser%sym==sym_close_square) then
          junk=pop_val(parser)
          call push_sym_val(parser,sym_dotdotdot)
       else
          if(expr(parser)) return
          call make_node(parser,sym_dotdot,2)
       endif
    else
       if(expr(parser)) return
       if(parser%sym==sym_dotdotdot) then
          call push_sym_val(parser,sym_hi)
          call make_node(parser,sym_dotdot,2)
          call scan(parser)
       endif
    endif
    iserr=.false.
  end function sexpr

  ! Comma separated list of sexpr
  recursive function sexprlist(parser,length) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out),optional:: length
    logical:: iserr
    integer:: n
    iserr=.true.
    n=0
    do
       if(sexpr(parser)) return
       n=n+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    call make_node(parser,sym_list,n)
    if(present(length)) length=n
    iserr=.false.
  end function sexprlist

  ! Assignment (=) definition (:=) 
  recursive function assn(parser,sym) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    logical:: iserr
    integer:: n,name
    iserr=.true.
    n=0
    do 
       if(lhs()) return
       n=n+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
 10 continue
    if(sym==sym_assign.and.parser%sym==sym_define) then
       call scan(parser)
       if(rhs()) return
       call make_node(parser,sym_define,n+1)
    else 
       if(expect(parser,sym_assign)) return
       if(rhs()) return
       call make_node(parser,sym,n+1)
    endif
    iserr=.false.
    return

  contains

    function lhs() result(is_err)
      logical:: is_err
      is_err=.true.
      if(parser%sym==sym_underscore) then
         call make_node(parser,sym_underscore,0)
         call scan(parser)
      else if(is_name(parser)) then
         if(qual(parser)) return
      else
         call parse_error(parser,&
              'Malformed left hand side of assignmentment/definition')
      endif
      is_err=.false.
    end function lhs

    function rhs() result(is_err)
      logical:: is_err
      is_err=.true.
      if(parser%sym==sym_for) then
         call push_null_val(parser)
         if(for_loop(parser,n)) return
      elseif(parser%sym==sym_using) then
         if(using_clause(parser)) return
         if(parser%sym/=sym_for) then
            call parse_error(parser,&
                 'Expected a "for" statement after ".. = using ..."')
            return
         endif
         if(for_loop(parser,n)) return
      elseif(n==1) then
         if(expr(parser)) return
      else
         if(check_name(parser,name)) then
            if(proccall(parser,name)) return
         else
            call parse_error(parser,'Expected procedure name')
         endif
      endif
      is_err=.false.
    end function rhs
    
  end function assn

  ! Comma separated list of assignments
  function assn_list(parser,sym) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    logical:: iserr
    integer:: n
    iserr=.true.
    n=0
    do
       if(assn(parser,sym)) return
       n=n+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    call make_node(parser,sym,n)
    iserr=.false.
  end function assn_list

  ! Sub-expressions (check exprlist where name = exp ... )
  subroutine subexp(parser)
    type(parse_state),intent(inout):: parser
    integer:: n
    logical:: ignore_err
    if(parser%sym==sym_check) then
       call scan(parser)
       if(parser%sym==sym_string) then
          call scan(parser)
          if(parser%sym/=sym_colon) then
             call push_back(parser,parser%sym)
             call push_null_val(parser)
          else
             call push_val(parser,parser%lexval)
             call scan(parser)
          endif
       else
          call push_null_val(parser)
       endif
       ignore_err=exprlist(parser)
       call make_node(parser,sym_check,3)
    endif
    do while(parser%sym==sym_where)
       call scan(parser)
       if(assn_list(parser,sym_where)) exit
       call make_node(parser,sym_where,2)
    enddo
  end subroutine subexp

  ! Extended expression (expr subexp)
  subroutine xexpr(parser)
    type(parse_state),intent(inout):: parser
    logical iserr
    iserr=expr(parser)
    call subexp(parser)
  end subroutine xexpr

  ! Extended expression list ( expr, expr... subexp)
  subroutine xexprlist(parser,length)
    type(parse_state),intent(inout):: parser
    integer,intent(out),optional:: length
    integer:: m
    if(exprlist(parser,m)) return
    if(present(length)) length=m
    call subexp(parser)
  contains
    include 'fesize.inc'
  end subroutine xexprlist

  ! Select statement
  recursive function sel_statement(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: n
    iserr=.true.
    call scan(parser)
    call xexpr(parser)
    n=0
    do while(parser%sym==sym_case)
       call scan(parser)
       call xexprlist(parser)
       if(expect(parser,sym_do)) return
       call statement_list(parser)
       n=n+2
    enddo
    if(n==0) then
       call parse_error(parser,'No "case" clauses in "select" statement')
       return
    endif
    if(parser%sym==sym_otherwise) then
       call scan(parser)
       call statement_list(parser)
    else
       call push_null_val(parser)
    endif
    call make_node(parser,sym_select,n+2)
    if(expect(parser,sym_endselect)) return
    iserr=.false.
  end function sel_statement

  ! Iteratator clause
  recursive function iter(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: i,m
    iserr=.true.
    if(expect_name(parser,'iteration variable')) return
    m=1
    if(parser%sym==sym_from) then
       if(from_follow(parser)) return
    else
       do while(parser%sym==sym_comma)
          call scan(parser)
          if(expect_name(parser,'variable name in "for" loop')) return
          m=m+1
       enddo
       if(expect(parser,sym_in)) return
       if(expr(parser)) return
       do i=2,m
          if(expect(parser,sym_comma)) return
          if(expr(parser)) return
       end do
    endif
    call make_node(parser,sym_iter,m*2)
    iserr=.false.
  end function iter

  recursive function for_loop(parser,nret) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: nret
    logical:: iserr
    integer:: m,loop,par
    iserr=.true.

    call scan(parser)
    if(iter(parser)) return
    call subexp(parser)

    if(parser%sym==sym_seq) then
       call scan(parser)
       par=0
    elseif(parser%sym==sym_conc) then
       call scan(parser)
       par=2
    else
       par=1
    endif

    if(parser%sym==sym_find.or.&
         parser%sym==sym_build) then
       call push_null_val(parser)
    else
       if(expect(parser,sym_do)) return
       call statement_list(parser)
    endif

    if(parser%sym==sym_find) then
       call scan(parser)
       call xexprlist(parser,m)
       if(m/=nret) then
          call parse_error(parser,'"find" returns incorrect number of values')
         endif
       if(expect(parser,sym_when)) return
       call xexpr(parser)
       if(expect(parser,sym_otherwise)) return
       call xexprlist(parser,m)
       if(m/=nret) &
            call parse_error(parser,'"otherwise" returns incorrect number of values')
       call make_node(parser,sym_find+par,6)
    elseif(parser%sym==sym_build) then
       if(par==0) &
            call parse_error(parser,'cannot have "build" clause in sequential loop')
       call scan(parser)
       call xexprlist(parser,m)
       if(m/=nret) &
            call parse_error(parser,'"build" returns incorrect number of values')
       call make_node(parser,sym_loop+par,4)
    else
       if(nret/=0) &
            call parse_error(parser,'"build" clause required to return values')
       call make_node(parser,sym_loop+par,3)
    endif
    if(expect(parser,sym_endfor)) return
    iserr=.false.
  end function for_loop

  ! Using clause - leaves two items on stack
  function using_clause(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: n,i
    iserr=.true.
    call scan(parser)
    n=0
    do while(parser%sym>num_sym)
       call push_sym_val(parser,parser%sym)
       call scan(parser)
       if(expect(parser,sym_assign)) return
       if(expr(parser)) return
       n=n+2
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    call make_node(parser,sym_list,n)
    iserr=.false.
  end function using_clause

  ! List of statements
  recursive subroutine statement_list(parser)
    type(parse_state),intent(inout):: parser
    logical:: ok
    integer:: i,n,m,k,name
    k=0
    do
       select case(parser%sym)
       case(sym_if)
          n=0
          do
             call scan(parser)
             call xexpr(parser)
             if(expect(parser,sym_then)) goto 999
             call statement_list(parser)
             n=n+1
             if(parser%sym/=sym_elseif) exit
          enddo
          if(parser%sym==sym_else) then
             call scan(parser)
             call statement_list(parser)
          else
             call push_null_val(parser)
          endif
          do while(n>1)
             call make_node(parser,sym_if,3)
             call make_node(parser,sym_list,1)
             n=n-1
          enddo
          call make_node(parser,sym_if,3)
          if(expect(parser,sym_endif)) goto 999
       case(sym_select)
          if(sel_statement(parser)) goto 999
       case(sym_while)
          call scan(parser)
          call xexpr(parser)
          if(expect(parser,sym_do)) goto 999
          call statement_list(parser)
          if(expect(parser,sym_endwhile)) goto 999
          call make_node(parser,sym_while,2)
       case(sym_repeat)
          call scan(parser)
          call statement_list(parser)
          if(expect(parser,sym_until)) goto 999
          call xexpr(parser)
          call make_node(parser,sym_repeat,2)
       case(sym_check)
          call push_null_val(parser)
          call subexp(parser)
       case(sym_debug)
          call scan(parser)
          if(parser%sym==sym_string) then
             call push_val(parser,parser%lexval)
             call scan(parser)
          else
             call push_null_val(parser)
          endif
          call statement_list(parser)
          call make_node(parser,sym_debug,2)
          if(expect(parser,sym_enddebug)) goto 999
       case(sym_for)
          call push_null_val(parser) ! No using
          if(for_loop(parser,0)) goto 999
       case(sym_using)
          if(using_clause(parser)) goto 999
          if(parser%sym==sym_for) then
             if(for_loop(parser,0)) goto 999
          else
             if(do_stmt()) goto 999
          endif
       case(sym_do)
          call push_null_val(parser) ! No using
          if(do_stmt()) goto 999
       case(sym_underscore)
          if(assn(parser,sym_assign)) goto 999
       case(sym_const)
          call scan(parser)
          if(assn_list(parser,sym_const)) goto 999
          call make_node(parser,sym_const,1)
       case(sym_result)
          exit
       case(sym_sync)
          n=0
          do
             call scan(parser)
             if(expect_name(parser)) goto 999
             n=n+1
             if(parser%sym/=sym_comma) exit
          enddo
          call make_node(parser,sym_sync,n)
       case default
         if(check_name(parser,name)) then
            if(parser%sym==sym_colon) then
               call push_sym_val(parser,name)
               call make_node(parser,sym_colon,1)
               call scan(parser)
               k=k+1
               cycle
            elseif(parser%sym==sym_open.or.parser%sym==sym_pct) then
               if(proccall(parser,name)) goto 999
               call subexp(parser)
            else
               call push_back(parser,name)
               if(assn(parser,sym_assign)) goto 999
               call subexp(parser)
            endif
         else
            if(parser%sym>0.and.parser%sym<=last_decl) then
               call parse_error(parser,'Expected start of statement')
               goto 999
            else
               exit
            endif
         endif
       end select
888    continue
       k=k+1
       if(parser%sym==sym_semi) then
          call scan(parser)
       else if(.not.parser%atstart) then
          if(parser%sym>0.and.parser%sym<=last_decl) then
             call parse_error(parser,'Expected end of statement list')
             goto 999
          else
             exit
          endif
       endif
       cycle
999    continue
       call skip_past_error(parser,.false.)
    enddo
    call make_node(parser,sym_list,k)
    
  contains

    function do_stmt() result(is_error)
      logical:: is_error
      integer:: n
      is_error=.true.
      if(parser%sym==sym_with) then
         call scan(parser)
         call statement_list(parser)
      else
         call push_null_val(parser)
      endif
      if(expect(parser,sym_do)) return
      call statement_list(parser)
      n=3
      do while(parser%sym==sym_also)
         call scan(parser)
         if(expect(parser,sym_do)) return
         call statement_list(parser)
         n=n+1
      enddo
      call make_node(parser,sym_also,n)
      if(expect(parser,sym_enddo)) return
      is_error=.false.
    end function do_stmt

  end subroutine statement_list

  ! Type
  recursive function typ(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: m,name,i,base,vbase,sym
    iserr=.true.
    sym=parser%sym
    select case(sym)
    case(sym_struct,sym_rec)
       sym=parser%sym
       call scan(parser)
       base=parser%top
       vbase=parser%vtop
       if(parser%sym==sym_open_brace) then
          call scan(parser)
          call push_sym(parser,0)
       else
          if(check_name(parser,name)) then
             call push_sym(parser,name)
          else
             call parse_error(parser,&
                  'Expected stucture name')
          endif
          if(expect(parser,sym_open_brace)) return
       endif
       do
          if(check_name(parser,name)) then
             call push_sym(parser,name)
          else
             call parse_error(parser,&
                  'Expected name of structure element')
             return
          endif
          if(parser%sym==sym_colon) then
             call scan(parser)
             if(typ(parser)) return
          else
             call push_null_val(parser)
          endif
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       call struct_sort(parser,base+1,vbase)
       call make_node(parser,sym_list,parser%vtop-vbase)
       call name_vector(parser,base)
       call make_node(parser,sym,2)
       if(expect(parser,sym_close_brace)) return
    case(sym_opt)
       call scan(parser)
       if(parser%sym==sym_open_brace) then
          call scan(parser)
          if(typ(parser)) return
          if(expect(parser,sym_close_brace)) return
       else
          if(typ(parser)) return
       endif
       call make_node(parser,sym_opt,1)
    case(sym_lt)
       if(parser%sym==sym_gt) then
          call make_node(parser,sym_any,0)
          call scan(parser)
       else
          if(typ(parser)) return
          if(expect(parser,sym_gt)) return
       endif
       base=parser%top
       call push_sym(parser,sym_poly)
       call push_sym(parser,11)
       call name_vector(parser,base)
       call make_node(parser,sym_type,2)
    case(sym_hash)
       call scan(parser)
       if(expect(parser,sym_lt)) return
       if(parser%sym==sym_gt) then
          call make_node(parser,sym_any,0)
          call scan(parser)
       else
          if(typ(parser)) return
          if(expect(parser,sym_gt)) return
       endif
       call make_node(parser,sym_any,1)
    case(sym_any)
       call make_node(parser,sym_any,0)
       call scan(parser)
    case(sym_open_square)
       call make_node(parser,sym_any,0)
    case default
       if(.not.check_name(parser,name)) then
          if(parser%sym/=sym_seq.and.parser%sym/=sym_null) then
             call parse_error(parser,'Expected type')
             return
          else
             name=parser%sym
             call scan(parser)
          endif
       endif
       if(parser%sym==sym_open_brace) then
          if(opt_typ_list()) return
          base=parser%top
          call push_sym(parser,name)
          call push_sym(parser,m)
          call name_vector(parser,base)
          call make_node(parser,sym_type,m+1)
          if(expect(parser,sym_close_brace)) return
       else
          call push_sym_val(parser,name)
          call make_node(parser,sym_type,1)
       endif
    end select
    do while(parser%sym==sym_hash.or.&
         parser%sym==sym_open_square)
       if(parser%sym==sym_hash) then
          call scan(parser)
          if(typ(parser)) return
          call make_node(parser,sym_hash,2)
       elseif(parser%sym==sym_open_square) then
          if(opt_typ_list()) return
          if(m==1.and.&
               pm_fast_isnull(parser%vstack(parser%vtop))) then
            parser%vstack(parser%vtop)=&
                 pm_fast_name(parser%context,int(sym_dom,pm_p))
            call make_node(parser,sym_type,1)
          endif
          base=parser%top
          call push_sym(parser,sym_array)
          call push_sym(parser,m+1)
          call name_vector(parser,base)
          call make_node(parser,sym_type,m+2)
          if(expect(parser,sym_close_square)) return
       endif
    enddo
    iserr=.false.

  contains
    include 'fname.inc'
    include 'fisnull.inc'

    function opt_typ_list() result(is_err)
      logical:: is_err
      is_err=.true.
      m=0
      do
         call scan(parser)
         if(parser%sym==sym_comma.or.&
              parser%sym==sym_close_brace.or.&
              parser%sym==sym_close_square) then
            call push_null_val(parser)
         else
            if(typ(parser)) return
         endif
         m=m+1
         if(parser%sym/=sym_comma) exit
      enddo
      is_err=.false.
    end function  opt_typ_list
  end function typ
  
  ! Parameter list for procedure declaration
  recursive function param_list(parser,isloop,nloop,ampargs) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: isloop
    integer,intent(out):: nloop
    logical,intent(out):: ampargs
    logical:: iserr
    integer:: m,n,base,sym,name,numloop
    type(pm_ptr):: temp
    logical:: loop_par
    base=parser%top
    iserr=.true.
    nloop=0
    ampargs=.false.
    m=0
    n=0
    numloop=0
    loop_par=isloop
    if(expect(parser,sym_open)) return
    if(parser%sym==sym_close) then
       call push_null_val(parser)
       call push_null_val(parser)
       call push_null_val(parser)
       call push_num_val(parser,0)
       numloop=0
       call scan(parser)
       iserr=.false.
       return
    endif
    do
       if(parser%sym==sym_arg) then
          call scan(parser)
          if(expect(parser,sym_dotdotdot)) return
          if(loop_par) then
             call parse_error(parser,&
                  '"arg..." must follow ":" in loop procedure parameters') 
          endif
          call push_sym_val(parser,sym_arg)
          if(parser%sym==sym_colon) then
             call scan(parser)
             if(isloop.and.parser%sym==sym_invar) then
                call scan(parser)
                if(typ(parser)) return
                call make_node(parser,1,sym_invar)
             else
                if(typ(parser)) return
             endif
          else
             call push_null_val(parser)
          endif
          call make_node(parser,sym_dotdotdot,m*2+2)
          exit
       else if(parser%sym==sym_amp) then
          call scan(parser)
          if(expect_name(parser,'argument name')) return
          if(parser%sym==sym_assign) then
             call parse_error(parser,'Cannot have "=" after "&name"')
             return
          endif
          m=m+1
          parser%top=parser%top+1
          parser%stack(parser%top)=m
       else if(parser%sym==sym_key) then
          call make_node(parser,sym_list,m*2)
          call push_back(parser,sym_comma)
          exit
       else
          if(check_name(parser,name)) then
             if(parser%sym==sym_assign) then
                call make_node(parser,sym_list,m*2)
                call push_sym_val(parser,name)
                call scan(parser)
                if(expr(parser)) return
                n=1
                exit
             else
                call push_sym_val(parser,name)
             endif
          else
             call parse_error(parser,'Expected argument')
          endif
          m=m+1
       endif
       if(parser%sym==sym_colon) then
          call scan(parser)
          if(isloop.and.parser%sym==sym_invar) then
             if(loop_par) &
                  call parse_error(parser,'"invar" cannot be applied to loop parameter')
             call scan(parser)
             if(typ(parser)) return
             call make_node(parser,sym_invar,1)
          else
             if(typ(parser)) return
          endif
       else
          call push_null_val(parser)
       endif
       if(loop_par.and.parser%sym==sym_semi) then
          loop_par=.false.
          numloop=m
       elseif(parser%sym/=sym_comma) then
          call make_node(parser,sym_list,m*2)
          exit
       endif
       call scan(parser)
    enddo
    if(loop_par) numloop=m
    if(parser%sym==sym_comma) then
       do
          call scan(parser)
          if(parser%sym==sym_key) then
             call scan(parser)
             if(expect(parser,sym_dotdotdot)) return
             call make_node(parser,sym_dotdotdot,n*2)
             exit
          else
             if(expect_name(parser, &
                  'optional argument name')) return
             if(expect(parser,sym_assign)) return
             if(expr(parser)) return
             n=n+1
          endif
          if(parser%sym/=sym_comma) then
             call make_node(parser,sym_list,n*2)
             exit
          endif
       enddo
    else
       if(n>0) then
          call make_node(parser,sym_list,n*2)
       else
          call push_null_val(parser)
       endif
    endif
    if(parser%top>base) then
       call name_vector(parser,base)
       ampargs=.true.
    else
       call push_null_val(parser)
       ampargs=.false.
    endif
    call push_num_val(parser,numloop)
    if(expect(parser,sym_close)) return
    nloop=numloop
    iserr=.false.
    return
  end function param_list

  ! Procedure declaration
  function proc(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    type(pm_ptr):: ptr,p
    type(pm_reg),pointer:: reg
    integer:: name,nret,base,flags,sbase,scount,nloop,sym,m
    logical:: ampargs
    reg=>pm_register(parser%context,'proc',ptr)
    iserr=.true.
    nret=0
    sbase=parser%vtop
    scount=parser%error_count
    call scan(parser)
10  continue
    if(.not.check_name(parser,name)) then
       if(op(parser,name)) goto 999
    endif
    if(parser%sym==sym_dcolon) then
       call push_sym(parser,sym_dcolon)
       call push_sym(parser,name)
       call name_vector(parser,parser%top-2)
       call scan(parser)
       sym=sym_dcolon
    elseif(parser%sym==sym_pct) then
       call push_sym(parser,sym_pct)
       call push_sym(parser,name)
       call name_vector(parser,parser%top-2)
       call scan(parser)
       sym=sym_pct
    else
       call push_sym_val(parser,name)
       sym=sym_proc
    endif
    p=top_val(parser)
    name=p%offset
    ptr=decl_entry(parser,name,modl_proc)
    base=parser%vtop
    call push_val(parser,top_val(parser))
    if(pm_fast_isnull(ptr)) then
       call push_null_val(parser)
    else
       call push_val(parser,ptr%data%ptr(ptr%offset+node_args+1))
    endif
    call push_val(parser,parser%modl)
    call push_num_val(parser,-1) ! flags
    if(param_list(parser,sym/=sym_proc,nloop,ampargs)) goto 999
    if(ampargs.and.sym==sym_dcolon) then
       call parse_error(parser,'"::" procedure cannot have "&" parameters')
    endif
    call push_num_val(parser,-777) ! coded params
    call push_num_val(parser,-777) ! nret
    flags=0
    nret=0
    if(sym==sym_dcolon.and.parser%sym==sym_reduce) then
       sym=sym_reduce
       call scan(parser)
       if(expect(parser,sym_open)) goto 999
       m=0
       do 
          if(expect_name(parser)) goto 999
          m=m+1
          if(parser%sym/=sym_comma) exit
       enddo
       call make_node(parser,sym_list,m)
       if(m/=nloop) then
          call parse_error(parser,&
               'Number of reduction variables does not match number of arguments')
       endif
       if(expect(parser,sym_close)) goto 999
    elseif(parser%sym==sym_local) then
       if(sym==sym_pct) then
          sym=sym_local
       elseif(sym==sym_dcolon) then
          sym=sym_reduce_at
       else
          call parse_error(parser,&
               '"local" in non-loop "proc" definition')
       endif
       call scan(parser)
       call make_node(parser,sym_local,0)
    else
       call push_null_val(parser)
    endif
    if(parser%sym==sym_check) then
       call push_null_val(parser)
       call subexp(parser)
       call push_null_val(parser)
    else
       call push_null_val(parser)
       if(parser%sym==sym_assign) then
          call scan(parser)
          call xexprlist(parser,nret)
       else
          call push_null_val(parser)
       endif
    endif
    if(parser%sym==sym_do) then
       call scan(parser)
       call statement_list(parser)
       if(parser%sym==sym_result) then
          if(parser%error_count==scount) then 
             if(.not.pm_fast_isnull(parser%vstack(parser%vtop-1))) then
                call parse_error(parser,&
                     'Cannot have both "proc f(x)=.." and "result =" in proc')
             endif
          endif
          call scan(parser)
          if(expect(parser,sym_assign)) goto 999
          call xexprlist(parser,nret)
          if(parser%error_count>scount) then
             parser%vtop=sbase
             goto 999
          endif
          ! Assign to proc_result slot
          parser%vstack(parser%vtop-2)=parser%vstack(parser%vtop)
          parser%vtop=parser%vtop-1
       endif
       if(expect(parser,sym_endproc)) goto 999
    else
       call push_null_val(parser)
    endif
    call push_null_val(parser) ! Code tree
    ! Assign flags to proc_flags slot
    parser%vstack(parser%vtop-&
         proc_num_args-node_args+proc_flags+1)%offset=flags
    ! Assign number of returns to proc_numret slot
    parser%vstack(parser%vtop-&
         proc_num_args-node_args+proc_numret+1)%offset=nret
    if(sym==sym_reduce.and.nret/=nloop) then
       call parse_error(parser,&
            'Number of arguments not equal to number of returns in "reduce" procedure')
    endif
    if(parser%error_count>scount) then
       parser%vtop=sbase
       goto 999
    endif
    if(pm_debug_level>0) then
       if(parser%vtop-base/=proc_num_args) then
          write(*,*) '========='
          do flags=base+1,parser%vtop
             call dump_parse_tree(parser%context,6,parser%vstack(flags),2)
             write(*,*) '==='
          enddo
          write(*,*) parser%vtop,base,parser%vtop-base,proc_num_args
          call pm_panic('parse proc')
       endif
    endif
    call make_node(parser,sym,proc_num_args)
    if(parser_xtra_debug) then
       write(*,*) 'PROC DECL>----------------'
       call dump_parse_tree(parser%context,44,top_val(parser),2)
       write(*,*) 'PROC-DECL----------------'
    endif
    if(pm_fast_isnull(ptr)) then
       call push_val(parser,top_val(parser))
       call push_null_val(parser)
       call make_node(parser,sym_proc,4)
       call new_decl(parser,name,modl_proc,.true.)
    else
       call pm_ptr_assign(parser%context,ptr,int(node_args+1,pm_ln),&
            top_val(parser))
       ptr=pop_val(parser)
       ptr=pop_val(parser)
    endif
    iserr=.false.
999 call pm_delete_register(parser%context,reg)
  contains
    include 'fisnull.inc'
  end function proc

  ! Procedure signature (...)[]->(...)
  recursive function proc_sig(parser,isloop) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: isloop
    logical:: iserr
    integer:: m,n,nloop,base,name,sym
    type(pm_ptr):: temp
    logical:: loop_par
    base=parser%top
    iserr=.true.
    m=0
    n=0
    nloop=0
    loop_par=isloop
    if(expect(parser,sym_open)) return
    if(parser%sym==sym_eq) then
       call scan(parser)
       call make_node(parser,sym_eq,0)
       call push_null_val(parser)
       if(parser%sym==sym_amp) then
          call scan(parser)
          parser%top=parser%top+1
          parser%stack(parser%top)=1
          call name_vector(parser,base)
       else
          call push_null_val(parser)
       endif
       if(expect(parser,sym_close)) return
    elseif(parser%sym==sym_close) then
       call push_null_val(parser)
       call push_null_val(parser)
       call push_null_val(parser)
       call scan(parser)
    else
       do
          m=m+1
          if(parser%sym==sym_amp) then
             call scan(parser)
             parser%top=parser%top+1
             parser%stack(parser%top)=m
          endif
          if(check_name(parser,name)) then
             if(parser%sym==sym_assign) then
                call make_node(parser,sym_list,m*2)
                call push_sym_val(parser,name)
                call scan(parser)
                if(typ(parser)) return
                n=1
                exit
             else if(parser%sym==sym_colon) then
                call push_sym_val(parser,name)
                call scan(parser)
             else
                call push_back(parser,name)
                call push_null_val(parser)
             endif
          else if(parser%sym==sym_arg) then
             call push_sym_val(parser,sym_arg)
             call scan(parser)
             if(expect(parser,sym_dotdotdot)) return
             if(expect(parser,sym_colon)) return
             if(parser%sym==sym_invar) then
                call scan(parser)
                if(typ(parser)) return
                call make_node(parser,sym_invar,1)
             else
                if(typ(parser)) return
             endif
             call make_node(parser,sym_dotdotdot,m*2)
             exit
          else if(parser%sym==sym_key) then
             call make_node(parser,sym_list,m*2)
             exit
          else
             call push_null_val(parser)
          endif

          if(parser%sym==sym_invar) then
             call scan(parser)
             if(typ(parser)) return
             call make_node(parser,sym_invar,1)
          else
             if(typ(parser)) return
          endif

          if(parser%sym==sym_dotdotdot) then
             call scan(parser)
             call make_node(parser,sym_dotdotdot,m*2)
             exit
          endif
          if(parser%sym==sym_semi) then
             loop_par=.false.
             nloop=m
          elseif(parser%sym/=sym_comma) then
             call make_node(parser,sym_list,m*2)
             exit
          endif
          call scan(parser)
       enddo
       if(loop_par) nloop=m
       do while(parser%sym==sym_comma)
          call scan(parser)
          if(parser%sym==sym_key) exit
          if(typ(parser)) return
          if(expect_name(parser,&
               'optional parameter name')) return
          if(expect(parser,sym_assign)) return
          n=n+1
       enddo
       if(parser%sym==sym_key) then
          call make_node(parser,sym_dotdotdot,n*2)
          call scan(parser)
          if(expect(parser,sym_dotdotdot)) return
       else if(n>0) then
          call make_node(parser,sym_list,n*2)
       else
          call push_null_val(parser)
       endif
       if(parser%top>base) then
          call name_vector(parser,base)
       else
          call push_null_val(parser)
       endif
       if(expect(parser,sym_close)) return
    endif
    call push_num_val(parser,nloop) ! nloop
    call push_num_val(parser,-1) ! Coded params
    if(parser%sym==sym_arrow) then
       call scan(parser)
       sym=parser%sym
       if(sym==sym_mult.or.sym==sym_assign.or.&
            sym==sym_hash.or.sym==sym_gt.or.&
            sym==sym_dim.or.sym==sym_dot) then
          call scan(parser)
          if(exprlist(parser,m)) return
          parser%temp=pop_val(parser)
          if(sym==sym_gt.or.sym==sym_dim) m=1
          call push_num_val(parser,m)
          call push_null_val(parser)
          call push_val(parser,parser%temp)
          call make_node(parser,sym,1)
       else
          m=0
          do
             if(typ(parser)) return
             if(parser%sym==sym_at) then
                call make_node(parser,sym_at,1)
                call scan(parser)
             endif
             m=m+1
             if(parser%sym/=sym_comma) exit
             call scan(parser)
          enddo
          call make_node(parser,sym_result,m)
          call push_num_val(parser,m)
          call swap_vals(parser)
          call push_null_val(parser)
       endif
    else
       call push_num_val(parser,0)
       call push_null_val(parser)
       call push_null_val(parser)
    endif
    iserr=.false.
    return
  end function proc_sig

  ! Built in procedure definition
  function builtin(parser,opcode,opcode2,pdata,flags) result(iserr)
    type(parse_state),intent(inout):: parser
    integer(pm_i16),intent(in):: opcode,opcode2
    type(pm_ptr),intent(in):: pdata
    integer,intent(in):: flags
    logical:: iserr
    type(pm_ptr),target:: ptr
    type(pm_ptr)::p
    type(pm_reg),pointer:: reg
    integer:: name
    logical:: isloop
    reg=>pm_register(parser%context,'builtin',ptr)
    iserr=.true.
    isloop=.false.
    call scan(parser)
10  continue
    if(.not.check_name(parser,name)) then
       if(op(parser,name)) goto 999
    endif
    if(parser%sym==sym_dcolon) then
       call push_sym(parser,sym_dcolon)
       call push_sym(parser,name)
       call name_vector(parser,parser%top-2)
       isloop=.true.
    elseif(parser%sym==sym_pct) then
       call push_sym(parser,sym_pct)
       call push_sym(parser,name)
       call name_vector(parser,parser%top-2)
       isloop=.true.
    else
       call push_sym_val(parser,name)
    endif
    ptr=top_val(parser)
    ptr=decl_entry(parser,int(ptr%offset),modl_proc)
    call push_val(parser,top_val(parser))
    if(pm_fast_isnull(ptr)) then
       call push_null_val(parser)
    else
       call push_val(parser,ptr%data%ptr(ptr%offset+node_args+1))
    endif
    call push_val(parser,parser%modl)
    call push_num_val(parser,flags)
    if(proc_sig(parser,isloop)) return
    call push_num_val(parser,int(opcode))
    call push_num_val(parser,int(opcode2))
    call push_val(parser,pdata)
    call push_null_val(parser)
    call make_node(parser,sym_endproc,sysproc_num_args)
    if(parser_xtra_debug) then
       write(*,*) 'BUILTIN DECL>----------------'
       call dump_parse_tree(parser%context,6,top_val(parser),2)
       write(*,*) 'BI-DECL-------------'
    endif
    if(pm_fast_isnull(ptr)) then
       call push_val(parser,top_val(parser))
       call push_null_val(parser)
       call make_node(parser,sym_proc,4)
       call new_decl(parser,name,modl_proc,.true.)
    else
       call pm_ptr_assign(parser%context,ptr,&
            int(node_args+1,pm_ln),&
            top_val(parser))
       ptr=pop_val(parser)
       ptr=pop_val(parser)
    endif
    iserr=.false.
999 call pm_delete_register(parser%context,reg)
  contains
    include 'fisnull.inc'
  end function builtin

  ! Type declaration
  function dectype(parser) result(iserr)
    type(parse_state):: parser
    logical:: iserr
    integer:: sym,m,name,namein,base,serror
    type(pm_ptr),target:: ptr,ptrin
    type(pm_reg),pointer:: reg
    logical:: constr
    serror=parser%error_count
    reg=>pm_register(parser%context,'dectype',ptr,ptrin)
    iserr=.true.
    constr=.false.
    sym=sym_includes
    call scan(parser)
    if(.not.check_name(parser,name)) then
       if(parser%sym/=sym_seq) then
          call parse_error(parser,'Expected type name')
          goto 999
       else
          name=parser%sym
          call scan(parser)
       endif
    endif
    call push_sym_val(parser,name)
    call push_null_val(parser)        ! number
    call push_val(parser,parser%modl) ! module
    if(parser%sym==sym_open_brace) then
       call scan(parser)
       m=0
       do
          if(expect_name(parser)) goto 999
          m=m+1
          if(parser%sym==sym_colon) then
             call scan(parser)
             if(typ(parser)) goto 999
             constr=.true.
          else
             call push_null_val(parser)
          endif
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       call make_node(parser,sym_list,m*2)
       base=parser%top
       call push_sym(parser,name)
       call push_sym(parser,m)
       call name_vector(parser,base)
       ptrin=pop_val(parser)
       name=ptrin%offset
       if(expect(parser,sym_close_brace)) goto 999
    else
       call push_null_val(parser)
    endif
    ptr=decl_entry(parser,name,modl_type)
    if(pm_fast_isnull(ptr)) then
       call push_null_val(parser)
    else
       call push_val(parser,ptr%data%ptr(ptr%offset+node_args+1))  ! link
    endif
    if(parser%sym==sym_in) then
       do
          call scan(parser)
          if(check_name(parser,namein)) then
             ptrin=decl_entry(parser,namein,modl_type)
             if(pm_fast_isnull(ptrin)) then
                call push_null_val(parser)
             else
                call push_val(parser,&
                     ptrin%data%ptr(ptrin%offset+node_args+1))
             endif
             call push_sym_val(parser,name)
             call make_node(parser,sym_type,1)
             call make_node(parser,sym_in,2)
             call add_decl(namein,ptrin)
          endif
          if(parser%sym/=sym_comma) exit
       enddo
    endif
    if(parser%sym==sym_is) then
       sym=sym_is
       call scan(parser)
       if(typlist()) goto 999
       m=1
    else if(parser%sym==sym_also) then
       sym=sym_also
       call scan(parser)
       if(expect(parser,sym_includes)) goto 999
       if(typlist()) goto 999
    else if(parser%sym==sym_includes) then
       call scan(parser)
       if(typlist()) return
    else
       call push_null_val(parser)
    endif
    call make_node(parser,sym,typ_num_args)
    if(parser_xtra_debug) then
       write(*,*) 'TYPEDECL>----------------'
       call dump_parse_tree(parser%context,44,top_val(parser),2)
       write(*,*) 'END TYPEDECL-------------'
    endif
    if(parser%error_count==serror) call add_decl(name,ptr)
    iserr=.false.
 999 continue
    call pm_delete_register(parser%context,reg)
  contains

    include 'fisnull.inc'

    subroutine add_decl(nam,p)
      integer,intent(in):: nam
      type(pm_ptr),intent(in):: p
      type(pm_ptr):: q
      if(pm_fast_isnull(p)) then
         q=top_val(parser)
         call push_sym_val(parser,nam)
         call push_val(parser,q)
         call push_val(parser,q)
         call push_null_val(parser)
         call make_node(parser,sym_type,4)
         call new_decl(parser,nam,modl_type,.false.)
         q=pop_val(parser)
      else
         call pm_ptr_assign(parser%context,p,&
              int(node_args+1,pm_ln),top_val(parser))
         q=pop_val(parser)
      endif
      iserr=.false.
    end subroutine add_decl

    function typlist() result(is_err)
      logical:: is_err
      integer:: n
      is_err=.true.
      n=0
      do
         if(typ(parser)) return
         n=n+1
         if(parser%sym/=sym_comma) exit
         call scan(parser)
      enddo
      call make_node(parser,sym_list,n)
      is_err=.false.
    end function typlist

  end function dectype

  ! Declarations
  subroutine decl(parser)
    type(parse_state),intent(inout):: parser
    type(pm_ptr):: modl
    integer:: dt
    type(pm_ptr):: old,p
    integer:: m,sym,name,base
    integer:: serror
    call push_sym_val(parser,name_entry(parser,'PM system'))
    call push_val(parser,parser%sysmodl)
    call push_null_val(parser)
    call make_node(parser,sym_use,3)
    call new_decl(parser,name_entry(parser,'PM system'),modl_include,.false.)
    do 
       if(parser%sym==sym_use) then
          call scan(parser)
       else
          exit
       endif
       base=parser%top
       do 
          if(check_name(parser,name)) then
             call push_sym(parser,name)
          else
             call parse_error(parser,'Expected module name')
          endif
          if(parser%sym==sym_dot) then
             call scan(parser)
          else
             exit
          endif
       enddo 
       if(parser%top>base+1) then
          call name_vector(parser,base)
          p=top_val(parser)
          name=p%offset
       else
          call push_sym_val(parser,name)
       endif
       call new_modl(parser,name)
       sym=sym_use
       if(parser%sym==sym_open_brace) then
          m=0
          do
             call scan(parser)
             sym=parser%sym
             if(sym==sym_type) then
                call push_num_val(parser,modl_type) 
                call scan(parser)
             else if(sym==sym_param) then
                call push_num_val(parser,modl_param)
                call scan(parser)
             else if(sym==sym_default) then
                call push_num_val(parser,modl_default)
                call scan(parser)
             else
                if(sym==sym_proc) call scan(parser)
                call push_num_val(parser,modl_proc)
             endif
             if(expect_name(parser)) goto 999
             if(parser%sym==sym_cond) then
                call scan(parser)
                if(expect_name(parser)) goto 999
             else
                call push_null_val(parser)
             endif
             m=m+1
             if(parser%sym/=sym_comma) exit
          enddo
          call make_node(parser,sym_list,m*3)
          if(expect(parser,sym_close_brace)) goto 999
          sym=sym_open_brace
       else
          call push_null_val(parser)
       endif
       call make_node(parser,sym,3)
       call new_decl(parser,name,modl_include,.false.)
       if(parser%sym==sym_semi) then
          call scan(parser)
       elseif(.not.parser%atstart) then
          call parse_error(parser,&
               'Expected ";" or newline after include statement')
          goto 999
       endif
    enddo
    do
       select case(parser%sym)
       case(sym_proc)
          if(proc(parser)) goto 999
       case(sym_type)
          if(dectype(parser)) goto 999
       case(sym_param,sym_default)
          call scan(parser)
          if(check_name(parser,name)) goto 999
          if(expect(parser,sym_assign)) goto 999
          serror=parser%error_count
          call xexpr(parser)
          if(parser%error_count>serror) goto 999
          call make_node(parser,parser%sym,2)
          if(parser%sym==sym_param) then
             call new_decl(parser,name,modl_param,.false.)
          else
             call new_decl(parser,name,modl_default,.false.)
          endif
       case default
          call statement_list(parser)
          if(parser%sym==sym_eof) then
             if(parser%error_count==0) then
                parser%modl%data%ptr(parser%modl%offset&
                     +modl_stmts)=pop_val(parser)
             endif
          else
             call parse_error(parser,'Malformed declaration or statement')
             call skip_past_error(parser,.false.)
             cycle
          endif
          exit
       end select
       if(parser%sym==sym_semi) then
          call scan(parser)
       else if(parser%sym==sym_eof.or..not.parser%atstart) then
          exit
       endif
    enddo
    if(parser%sym/=sym_eof) then
       call parse_error(parser,'Malformed declaration')
    endif
    return
    999 continue
  end subroutine decl

  ! Skip tokens until out of expr or statement
  subroutine skip_past_error(parser,expr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: expr
    if(parser_xtra_debug) write(*,*) 'Skipping',expr
    do
       if(expr) then
          do while(parser%sym/=sym_eof.and.(parser%sym>num_sym.or.&
               parser%sym<=last_expr))
             if(parser%sym==sym_assign.or.parser%sym==sym_define) then
                call push_back(parser,sym_dom)
                exit
             endif
             call scan(parser)
          enddo
       else
          do while(parser%sym/=sym_eof&
               .and.(parser%sym>num_sym.or.parser%sym<=last_key))
             call scan(parser)
          enddo
       endif
       if(parser%sym>last_key.and.parser%sym<=last_decl) then
          if(parser%atstart) exit
       else
          exit
       endif
    enddo
    if(parser_xtra_debug) write(*,*) 'Skipped'
  end subroutine skip_past_error

  ! Push onto value stack
  subroutine push_val(parser,val)
    type(parse_state),intent(inout):: parser
    type(pm_ptr),intent(in):: val
    parser%vtop=parser%vtop+1
    if(parser%vtop>max_parse_stack) then
       call parse_error(parser,"Syntax too complex")
       stop "Parser terminated"
    endif
    parser%vstack(parser%vtop)=val
    parser%vline(parser%vtop)=parser%lineno
  end subroutine push_val

  ! Pop off value stack
  function pop_val(parser) result(val)
    type(parse_state),intent(inout):: parser
    type(pm_ptr):: val
    if(parser%error_count>0) then
       val=pm_null_obj
       if(parser%vtop>0) parser%vtop=parser%vtop-1
    else
       val=parser%vstack(parser%vtop)
       parser%vtop=parser%vtop-1
    endif
  end function pop_val
  
  ! Top of value stack
  function top_val(parser) result(val)
    type(parse_state),intent(inout):: parser
    type(pm_ptr):: val
    if(parser%error_count>0) then
       val=pm_null_obj
    else
       val=parser%vstack(parser%vtop)
    endif
  end function top_val

  ! Swap top two entries on value stack
  subroutine swap_vals(parser)
    type(parse_state),intent(inout):: parser
    type(pm_ptr):: temp
    integer(pm_i16):: templine
    if(parser%error_count>0) return
    temp=top_val(parser)
    parser%vstack(parser%vtop)=parser%vstack(parser%vtop-1)
    parser%vstack(parser%vtop-1)=temp
    templine=parser%vline(parser%vtop)
    parser%vline(parser%vtop)=parser%vline(parser%vtop-1)
    parser%vline(parser%vtop-1)=templine
  end subroutine swap_vals
  
  ! Push a null value onto value stack
  subroutine push_null_val(parser)
    type(parse_state),intent(inout):: parser
    call push_val(parser,pm_null_obj)
  end subroutine push_null_val
  
  ! Push a small integer (stored as a null)
  subroutine push_num_val(parser,n)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: n
    type(pm_ptr):: val
    val=pm_fast_tinyint(parser%context,int(n,pm_p))
    call push_val(parser,val)
  contains
    include 'ftiny.inc'
  end subroutine push_num_val

  ! Push token on to value stack
  subroutine push_sym_val(parser,n)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: n
    type(pm_ptr):: val
    val=pm_fast_name(parser%context,int(n,pm_p))
    call push_val(parser,val)
  contains
    include 'fname.inc'  
  end subroutine push_sym_val

  ! Push symbol onto symbol stack
  subroutine push_sym(parser,sym)
    type(parse_state),intent(inout):: parser
    integer:: sym
    if(parser%top>=max_parse_stack) then
       call parse_error(parser,'Expression too complex')
    else
       parser%top=parser%top+1
       parser%stack(parser%top)=sym
    endif
  end subroutine push_sym
  
  ! Make node from top n elements of value stack
  ! optionally ignore the top value
  subroutine make_node(parser,typeno,n)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: typeno,n
    type(pm_ptr):: val
    integer:: i
    logical:: reuse
    if(parser%error_count>0) then
       parser%vtop=parser%vtop-n+1
       if(parser%vtop<0) parser%vtop=0
       return
    endif
    val=pm_fast_newnc(parser%context,pm_pointer,int(n+4,pm_p))
    if(parser_xtra_debug) &
         write(*,*) 'make node:',sym_names(typeno),&
         parser%vtop,n,val%data%esize
    val%data%ptr(val%offset)%data=>pm_null_obj%data

    if(check_node_reuse.and.val%data%ptr(val%offset)%offset==987654) then
       ! Flag reused nodes if required (for debugging)
       val%data%ptr(val%offset)%offset=987652
    else
       ! Magic number
       val%data%ptr(val%offset)%offset=987654
    endif
    
    val%data%ptr(val%offset+1)%data=>pm_null_obj%data
    val%data%ptr(val%offset+1)%offset=typeno
    val%data%ptr(val%offset+2)=parser%modl
    val%data%ptr(val%offset+3)%data=>pm_null_obj%data
    val%data%ptr(val%offset+3)%offset=parser%vline(parser%vtop-n+1)
    if(val%offset+n+3>4096) call pm_panic('make_node')
    do i=1,n
       val%data%ptr(val%offset+i+3)=parser%vstack(parser%vtop+i-n)
    enddo
    parser%vtop=parser%vtop-n+1
    parser%vstack(parser%vtop)=val
    if(parser_xtra_debug) then
       write(*,*) '------New node------'
       call dump_parse_tree(parser%context,6,val,2)
       write(*,*) '--------------------'
    endif
  contains
    include 'fnewnc.inc'
  end subroutine make_node
  
  ! Create vector of locations of & in parameter list
  ! Locations taken from token stack above base
  subroutine name_vector(parser,base)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: base
    integer:: sym
    parser%temp=pm_fast_newnc(parser%context,pm_int16,&
         int(parser%top-base,pm_p))
    parser%temp%data%i16(parser%temp%offset:&
         parser%temp%offset+parser%top-base-1)= &
         parser%stack(base+1:parser%top)
    sym=pm_set_lookup(parser%context,parser%context%names,parser%temp)
    if(sym==0) then
       sym=pm_set_add(parser%context,parser%context%names,parser%temp)
    endif
    call push_sym_val(parser,sym)
    parser%top=base
  contains
    include 'fnewnc.inc'
  end subroutine name_vector

  ! Add name to global dictionary
  function name_entry(parser,string) result(val)
    type(parse_state),intent(inout)::parser
    character(len=*):: string
    integer:: val
    integer:: sym
    parser%temp=pm_new_string(parser%context,string)
    sym=pm_set_lookup(parser%context,parser%context%names,parser%temp)
    if(sym==0) then
       sym=pm_set_add(parser%context,parser%context%names,parser%temp)
    endif
    val=sym
  end function name_entry

  ! Add local name (_name) to global dictionary
  function lname_entry(parser,string) result(val)
    type(parse_state),intent(inout)::parser
    character(len=*):: string
    integer:: val
    integer:: sym
    sym=name_entry(parser,string)
    parser%temp=pm_fast_newnc(parser%context,pm_int,&
         2_pm_p)
    parser%temp%data%i(parser%temp%offset)=&
         parser%modl%data%ptr(parser%modl%offset+modl_name)%offset
    parser%temp%data%i(parser%temp%offset+1)=sym
    sym=pm_set_lookup(parser%context,parser%context%names,parser%temp)
    if(sym==0) then
       sym=pm_set_add(parser%context,parser%context%names,parser%temp)
    endif
    val=sym
  contains
    include 'fnewnc.inc'
  end function lname_entry


  ! Create new module object
  subroutine new_modl(parser,name)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    type(pm_ptr):: modl
    integer:: i
    integer,parameter:: siz=modl_param+modl_local
    type(pm_ptr):: nameval
    logical:: ok
    nameval=pm_fast_name(parser%context,int(name,pm_p))
    modl=pm_dict_lookup(parser%context,parser%modl_dict,&
         nameval)
    if(pm_fast_isnull(modl)) then
       call push_sym_val(parser,name)
       call push_val(parser,parser%modls)
       call push_null_val(parser)
       do i=modl_include,modl_param+modl_local
          call push_val(parser,pm_dict_new(parser%context,4_pm_ln))
       enddo
       modl=pm_fast_newnc(parser%context,pm_pointer,int(siz+1,pm_p))
       modl%data%ptr(modl%offset)%data=>pm_null_obj%data
       modl%data%ptr(modl%offset)%offset=0
       modl%data%ptr(modl%offset+1:modl%offset+siz)=&
            parser%vstack(parser%vtop-siz+1:parser%vtop)
       parser%vtop=parser%vtop-siz+1
       parser%modls=modl
       call pm_dict_set(parser%context,parser%modl_dict,&
            nameval,modl,.true.,.true.,ok)
    endif
    parser%vstack(parser%vtop+1)=modl
    parser%vtop=parser%vtop+1
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'fname.inc'
  end subroutine new_modl

  ! Return module name
  function get_modl_name(ptr) result(name)
    type(pm_ptr):: ptr
    integer:: name
    name=ptr%data%ptr(ptr%offset+modl_name)%offset
  end function get_modl_name

  ! Enter a new declaration into current module
  ! slot = modl_type, modl_proc, modl_param
  ! optionally overwrite current definition
  subroutine new_decl(parser,name,slot,overwrt)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    integer,intent(in):: slot
    logical,intent(in):: overwrt
    integer:: m
    type(pm_ptr):: node,modl,nameval,keys
    character(len=100):: str
    logical:: ok
    m=slot
    if(parser%error_count>0) return
    node=pop_val(parser)
    modl=parser%modl
    nameval=pm_fast_name(parser%context,int(name,pm_p))
    if(name>first_key) then
       keys=pm_set_keys(parser%context,parser%context%names)
       if(pm_fast_vkind(keys%data%ptr(&
            keys%offset+name-1))==pm_int) m=m+modl_local
    endif
    call pm_name_string(parser%context,int(name,pm_p),str)
    call pm_dict_set(parser%context,modl%data%ptr(modl%offset+m),&
         nameval,node,.true.,overwrt,ok)
    if(.not.ok) then
       call pm_name_string(parser%context,int(name,pm_p),str)
       call parse_error(parser,'Redefinition not allowed: '//trim(str))
    endif
  contains
    include 'fname.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'
  end subroutine new_decl

  ! Return declaration for a given name/slot
  function decl_entry(parser,name,slot) result(ptr)
    type(parse_state),intent(in):: parser
    integer,intent(in):: name,slot
    type(pm_ptr):: ptr
    integer:: m
    type(pm_ptr):: modl,nameval,keys
    character*100 str
    m=slot
    modl=parser%modl
    call pm_name_string(parser%context,int(name,pm_p),str)
    nameval=pm_fast_name(parser%context,int(name,pm_p))
    if(name>first_key) then
       keys=pm_set_keys(parser%context,parser%context%names)
       if(pm_fast_vkind(keys%data%ptr(&
            keys%offset+name-1))==pm_int) m=m+modl_local
    endif
    ptr=pm_dict_lookup(parser%context,modl%data%ptr(modl%offset+m),nameval)
  contains
    include 'fname.inc'  
    include 'fvkind.inc'
  end function decl_entry

  ! Dump a module (debugging)
  subroutine dump_module(context,iunit,ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: ptr
    character(len=100):: str
    character(len=7),dimension(0:4):: dnames = &
         (/ 'include','proc   ','type   ','param  ','default' /)
    integer:: i,j,k,m
    type(pm_ptr):: keys,vals
    call pm_name_string(context,ptr%data%ptr(ptr%offset+1)%offset,str)
    write(iunit,*) 'Module: ',trim(str)
    write(iunit,*) 'Stmts:'
    call dump_parse_tree(context,iunit,ptr%data%ptr(ptr%offset+modl_stmts),2)
    do k=0,modl_local,modl_local
       if(k==modl_local) then
          write(iunit,*) 'Local:'
          m=modl_proc
       else
          m=modl_include
       endif
       do j=m,modl_param
          write(iunit,*) dnames(j-modl_include),marked(ptr%data%ptr(ptr%offset+j+k)),'::'
          keys=pm_dict_keys(context,ptr%data%ptr(ptr%offset+j+k))
          vals=pm_dict_vals(context,ptr%data%ptr(ptr%offset+j+k))
          write(iunit,*) marked(keys),marked(vals)
          do i=1,pm_dict_size(context,ptr%data%ptr(ptr%offset+j+k))
             call pm_name_string(context,&
                  keys%data%ptr(keys%offset+i-1)%offset,str)
             write(iunit,*) ' ',trim(str),'::'
             write(iunit,*) marked(vals%data%ptr(vals%offset+i-1))
             call dump_parse_tree(context,iunit,&
                  vals%data%ptr(vals%offset+i-1),2)
          enddo
       enddo
    enddo
  end subroutine dump_module

  ! Dump a parser tree (debugging)
  recursive subroutine dump_parse_tree(context,iunit,ptr,depth)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: ptr
    integer,intent(in):: depth
    integer:: i, sym
    character(len=80),parameter:: spaces = ' '
    character(len=100):: str
    if(depth>30) then
       write(iunit,*) spaces(:depth*2),'>>>'
       return
    endif
    if(pm_fast_vkind(ptr)==pm_pointer) then
       if(ptr%data%ptr(ptr%offset)%offset/=987654) then
          if(ptr%data%ptr(ptr%offset)%offset==987652) then
             write(iunit,*) spaces(1:depth*2),'REUSED NODE',&
                  ptr%offset,ptr%data%hash,ptr%data%esize
          else
             write(iunit,*) spaces(1:depth*2),'INVALID NODE'
             return
          endif
       endif
       sym=ptr%data%ptr(ptr%offset+1)%offset
       if(sym>0.and.sym<=num_sym) then
          write(iunit,*) spaces(1:depth*2),sym_names(sym),ptr%data%esize,&
               'line',node_get_lineno(ptr),'Marked:',marked(ptr),ptr%data%hash,ptr%offset
       else if(sym==0) then
          call pm_name_string(context,ptr%data%ptr(ptr%offset+1)%offset,str)
          write(iunit,*) spaces(1:depth*2),'Module: ',trim(str)
          return
       else
          write(iunit,*) spaces(1:depth*2),'???'
          return
       endif
       do i=node_args,ptr%data%esize
          call dump_parse_tree(context,iunit,ptr%data%ptr(ptr%offset+i),&
               depth+1)
       enddo
    else if(pm_fast_isnull(ptr)) then
       write(iunit,*) spaces(1:depth*2),'NULL'
    else if(pm_fast_isname(ptr)) then
       call pm_name_string(context,ptr%offset,str)
       write(iunit,*) spaces(1:depth*2),'Name:',trim(str)
    else if(pm_fast_istiny(ptr)) then
       write(iunit,*) spaces(1:depth*2),'Tiny number:',ptr%offset
    else
       call pm_dump_tree(context,iunit,ptr,depth)
    endif
  contains
    include 'fvkind.inc'
    include 'fisnull.inc'
    include 'fisname.inc'
    include 'fistiny.inc'
  end subroutine dump_parse_tree

  ! Syntax error - print message 
  ! and stop building parse tree
  subroutine parse_error(parser,emess)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: emess
    character(len=67):: caret
    character(len=100):: modname
    integer:: i,n
    if(parser_xtra_debug) write(*,*) '*****Error::',trim(emess)
    call pm_name_string(parser%context,&
         parser%modl%data%ptr(parser%modl%offset+modl_name)%offset,modname)
    write(*,*) 
    write(*,*) 'Syntax error: ',trim(modname)
    write(*,*)
    i=1
    n=parser%nsym
    do while(n>67) 
       i=i+60
       n=n-60
    enddo
    write(*,'(i4,a2,a67)') parser%lineno-1,': ',&
         parser%line(3-parser%ls)(i:)
    write(*,'(i4,a2,a67)') parser%lineno,': ',&
         parser%line(parser%ls)(i:)
    caret=" "
    caret(n:n)="!"
    write(*,'(6x,a67)') caret
    caret=emess
    write(*,'(6x,a67)') caret
    parser%error_count=parser%error_count+1
    write(*,*)
    if(parser%error_count>max_errors) then
       stop 'Parser Error count exceeded'
    endif
  end subroutine parse_error

  subroutine check_node(node)
    type(pm_ptr),intent(in):: node
    if(pm_fast_vkind(node)==pm_pointer) then
       if(node%data%ptr(node%offset)%offset/=987654) then
          call pm_panic('Bad parse node')
       endif
    endif
  contains
    include 'fvkind.inc'
  end subroutine check_node

  subroutine check_ptr_node(node)
    type(pm_ptr),intent(in):: node
    if(pm_fast_vkind(node)==pm_pointer) then
       if(node%data%ptr(node%offset)%offset/=987654) then
          call pm_panic('Bad parse node')
       endif
    else
       call pm_panic('not ptr parser node')
    endif
  contains
    include 'fvkind.inc'
  end subroutine check_ptr_node

  function node_sym(node) result(n)
    type(pm_ptr),intent(in):: node
    integer:: n
    if(pm_fast_vkind(node)/=pm_pointer) then
       n=0
    else
       if(pm_debug_level>0) call check_node(node)
       n=node%data%ptr(node%offset+node_symbol)%offset
    endif
  contains
    include 'fvkind.inc'
  end function node_sym

  function node_numargs(node) result(n)
    type(pm_ptr),intent(in):: node
    integer:: n
    if(pm_fast_vkind(node)/=pm_pointer) then
       n=0
    else
       if(pm_debug_level>0) call check_node(node)
       n=pm_fast_esize(node)-node_args+1
    endif
  contains
    include 'fesize.inc'
    include 'fvkind.inc'
  end function node_numargs

  function node_arg(node,n) result(p)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    type(pm_ptr):: p
    if(pm_debug_level>0) &
       call check_ptr_node(node)
    p=node%data%ptr(node%offset+node_args+n-1)
  end function node_arg

  function node_get(node,n) result(p)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    type(pm_ptr):: p
    if(pm_debug_level>0) &
       call check_ptr_node(node)
    p=node%data%ptr(node%offset+n)
  end function node_get

  subroutine node_set(node,n,p)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    type(pm_ptr),intent(in):: p
    if(pm_debug_level>0) &
       call check_ptr_node(node)
    node%data%ptr(node%offset+n)=p
  end subroutine node_set

  function node_get_num(node,n) result(num)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    integer(pm_p):: num
    type(pm_ptr):: p
    if(pm_debug_level>0) &
         call check_ptr_node(node)
    p=node%data%ptr(node%offset+n)
    num=p%offset
  end function node_get_num

  subroutine node_set_num(node,n,num)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    integer(pm_p),intent(in):: num
    if(pm_debug_level>0) &
         call check_ptr_node(node)
    node%data%ptr(node%offset+n)%offset=num
  end subroutine node_set_num

  function node_get_lineno(node) result(n)
    type(pm_ptr),intent(in):: node
    integer:: n
    if(pm_debug_level>0) &
         call check_ptr_node(node)
    n=node%data%ptr(node%offset+node_lineno)%offset
  end function node_get_lineno

  function node_get_modl(node) result(modl)
    type(pm_ptr),intent(in):: node
    type(pm_ptr):: modl
    if(pm_debug_level>0) &
       call check_ptr_node(node)
    modl=node%data%ptr(node%offset+node_modl)
  contains
    include 'fvkind.inc'
  end function node_get_modl

  function node_get_modl_name(node) result(name)
    type(pm_ptr),intent(in):: node
    integer(pm_p):: name
    type(pm_ptr):: modl
    if(pm_debug_level>0) &
       call check_ptr_node(node)
    modl=node_get_modl(node)
    name=modl%data%ptr(modl%offset+modl_name)%offset
  end function node_get_modl_name

end module pm_parser
