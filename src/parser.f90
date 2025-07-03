!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2024
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
  use pm_sysdep
  use pm_compbase
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_options
  use pm_hash
  use pm_symbol
  use pm_vmdefs
  use pm_types
  use pm_ast
  implicit none

  ! Print out lots of parser debugging info
  logical,parameter:: debug_parser=.false.

  ! Check if memory manager attempts to reuse a node
  ! (this should not happen so is one test of gc)
  logical,parameter:: check_node_reuse=.false.

  integer,parameter:: max_string=100
  integer,parameter:: max_line=2001
  integer,parameter:: max_parse_stack = 1024
  integer,parameter:: max_errors=20

  ! Parser state
  type parse_state
     type(pm_context),pointer:: context
     type(pm_ptr):: modl,modls,modl_dict,sysmodl,visibility
     character(len=max_line),dimension(2):: line
     integer:: ls,lineno,sym_lineno,name_lineno,old_sym_lineno
     logical:: newline,atstart
     integer:: n, sym_n, name_sym_n,old_sym_n, last, iunit
     type(pm_ptr):: temp
     integer:: sym, pushback, lexval
     integer,dimension(max_parse_stack):: stack
     integer:: top
     type(pm_ptr),dimension(max_parse_stack):: vstack
     integer,dimension(max_parse_stack):: vline,vchar
     integer:: vtop
     type(pm_ptr):: op_names
     integer:: error_count
     type(pm_reg),pointer:: reg
  end type parse_state

contains

  !****************************************************
  ! MAIN DRIVER ROUTINES
  !****************************************************


  !======================================================
  ! Initialise the PM parser
  !======================================================
  subroutine init_parser(parser,context)
    type(parse_state),intent(inout)::parser
    type(pm_context),pointer:: context
    integer:: i
    type(pm_ptr)::val
    logical:: ok
    parser%context=>context
    parser%line(1)=' '
    parser%line(2)=' '
    parser%ls=1
    parser%lineno=0
    parser%sym_lineno=0
    parser%newline=.false.
    parser%atstart=.false.
    parser%n=1
    parser%sym_n=0
    parser%last=1
    parser%sym=sym_eof
    parser%pushback=-1
    parser%top=1
    parser%error_count=0
    parser%vtop=max_parse_stack
    parser%reg=>pm_register(context,'parser',&
         parser%modl,parser%modls,parser%modl_dict,&
         parser%temp,parser%sysmodl, &
         parser%visibility,parser%op_names,&
         array=parser%vstack, &
         array_size=parser%vtop)
    parser%modl_dict=pm_dict_new(context,128_pm_ln)
    parser%modl=pm_null_obj
    parser%modls=pm_null_obj
    parser%visibility=pm_set_new(context,128_pm_ln)
    parser%vtop=0
    parser%op_names=pm_dict_new(context,int(num_op-min_op+1,pm_ln))
    ! Create dictionary of operator names (for PM__intrinsic)
    do i=min_op,num_op
!!$       write(*,*) '@>>',op_names(i),'{',op_names(op_offset_i8),'}',op_long_i8,op_long_i64
       call pm_dict_set(parser%context,parser%op_names,&
            pm_new_string(parser%context,trim(op_names(i))),&
            pm_fast_tinyint(parser%context,i),&
            .true.,.false.,ok)
    enddo

  contains
    include 'ftiny.inc'
  end subroutine init_parser

  !======================================================
  ! Terminate the PM parser
  !======================================================
  subroutine term_parser(parser)
    type(parse_state),intent(inout):: parser
    call pm_delete_register(parser%context,parser%reg)
  end subroutine term_parser

  !======================================================
  ! Parse decarations in file opened on iunit
  !======================================================
  subroutine parse_file_on_unit(parser,iunit,is_main_module)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: iunit
    logical,intent(in):: is_main_module
    parser%iunit=iunit
    parser%lineno=0
    parser%sym_lineno=1
    parser%sym_n=1
    parser%name_lineno=1
    parser%name_sym_n=1
    parser%old_sym_lineno=1
    parser%old_sym_n=1
    call next_line(parser)
    call scan(parser)
    call decl(parser,is_main_module)
  end subroutine parse_file_on_unit

  !======================================================
  ! Start a module declaration with the given name
  !======================================================
  subroutine dcl_module(parser,name)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: name
    call new_modl(parser,pm_name_entry(parser%context,name))
    parser%modl=parser%modls
    parser%modls=parser%modls%data%ptr(parser%modls%offset+modl_link)
  end subroutine dcl_module

  !======================================================
  ! Declare an internally implemented procedure
  !======================================================
  subroutine dcl_proc(parser,def,opcode,opcode2,line,flags)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: def
    integer,intent(in):: opcode
    integer,intent(in):: opcode2
    integer,intent(inout):: line
    integer,intent(in):: flags

    call parse_from_string(parser,def)
    parser%lineno=line
    if(debug_parser) then
       write(*,*) 'Parse intrinsic def(',line,') ',trim(def)
    endif
    if(pm_opts%out_sysmod) write(45,'(I4,A7,A)') line,'proc:',trim(def)
    line=line+1
    if(builtin(parser,opcode,opcode2,pm_null_obj,flags)) then
       call pm_panic('bad intrinsic module')
    endif
    if(flags/=0) write(*,*) 'FLAGS=',flags
  end subroutine dcl_proc

  !======================================================
  ! Declare a user defined procedure
  !======================================================
  subroutine dcl_uproc(parser,def,line)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: def
    integer,intent(inout):: line
    call parse_from_string(parser,def)
    parser%lineno=line
    if(debug_parser) then
       write(*,*) 'Parse sysem user proc def (',line,'):',trim(def)
    endif
    if(pm_opts%out_sysmod) write(45,'(I4,A7,A)') line,'uproc:',trim(def)
    line=line+1
    if(proc_decl(parser)) then
       write(*,*) def
       call pm_panic('bad intrinsic module')
    endif
    if(parser%sym/=sym_eof) then
       write(*,*) trim(def)
       write(*,*) trim(parser%line(1))
       write(*,*) trim(parser%line(2))
       write(*,*) trim(pm_name_as_string(parser%context,parser%sym))
       call pm_panic('uproc ends badly')
    endif
  end subroutine dcl_uproc

  !======================================================
  ! Declare a type
  !======================================================
  subroutine dcl_type(parser,def,line)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: def
    integer,intent(inout):: line
    call parse_from_string(parser,def)
    parser%lineno=line
    if(debug_parser) then
       write(*,*) 'Parse sysem user type def (',line,'):',trim(def)
    endif
    if(pm_opts%out_sysmod) write(45,'(I4,A7,A)') line,'type:',trim(def)
    line=line+1
    if(typ_decl(parser)) then
       write(*,*) def
       call pm_panic('bad intrinsic type')
    endif
    if(parser%sym/=sym_eof) then
       write(*,*) def
       write(*,*) pm_name_as_string(parser%context,parser%sym)
       call pm_panic('type ends badly')
    endif
  end subroutine dcl_type

  !======================================================
  ! Parse expression in string (for REPL)
  !======================================================
  subroutine parse_expr_from_string(parser,line,use_sysmod)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: line
    logical,intent(in):: use_sysmod
    if(use_sysmod) then
       call push_sym_val(parser,sym_pm_system)
       call push_val(parser,parser%sysmodl)
       call push_null_val(parser)
       call make_node(parser,sym_use,3)
       call new_import(parser,sym_pm_system,pop_val(parser))
    endif
    call parse_from_string(parser,line)
    call scan(parser)
    call xexpr(parser)
    call make_node(parser,sym_repl_line,1)
    call make_node(parser,sym_list,1)
    parser%modl%data%ptr(parser%modl%offset&
         +modl_stmts)=pop_val(parser)
  end subroutine parse_expr_from_string

  !======================================================
  ! Start parsing PM code from a string
  !======================================================
  subroutine parse_from_string(parser,str)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: str
    parser%ls=1
    if(len(str)>len(parser%line(parser%ls))) then
       write(*,*) len(str),len(parser%line(parser%ls)),trim(str)
       call pm_panic('uproc too long')
    endif
    parser%line(parser%ls)=str
    parser%line(2)=""
    parser%last=len_trim(parser%line(parser%ls))+1
    parser%n=1
    parser%lineno=1
    parser%newline=.true.
  end subroutine parse_from_string


  !************************************************************
  ! LEXICAL ANALYSIS
  !************************************************************


  !======================================================
  ! Get next line of PM code
  !======================================================
  subroutine next_line(parser)
    type(parse_state),intent(inout):: parser
    integer:: ios
    if(parser%line(parser%ls)/=" ") then
       parser%ls=3-parser%ls
    endif
    parser%lineno=parser%lineno+1
    if(debug_parser) write(*,*) 'Now at line',parser%lineno
    if(parser%iunit>=0) then
       call pm_read_line(parser%iunit,parser%line(parser%ls),ios)
       if(ios/=0) goto 10
       parser%last=len_trim(parser%line(parser%ls))+1
       parser%n=1
       parser%newline=.true.
       return
    endif
10  continue
    parser%n=1
    parser%atstart=.false.
    parser%newline=.false.
    parser%line(parser%ls)=pm_eof_char
  end subroutine next_line

  !======================================================
  ! Push back scanned token
  !======================================================
  subroutine push_back(parser,sym)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    parser%pushback=parser%sym
    parser%sym=sym
    parser%old_sym_n=parser%sym_n
    parser%old_sym_lineno=parser%sym_lineno
  end subroutine push_back

  !======================================================
  ! Push back scanned token and associate with line/pos
  !======================================================
  subroutine push_back_at(parser,sym,line,pos)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym,line,pos
    parser%pushback=parser%sym
    parser%sym=sym
    parser%old_sym_n=parser%sym_n
    parser%old_sym_lineno=parser%sym_lineno
    parser%sym_n=pos
    parser%sym_lineno=line
  end subroutine push_back_at

  !======================================================
  ! Push back scanned token and name line/pos info
  !======================================================
  subroutine push_back_name(parser,sym)
    type(parse_state),intent(inout):: parser
    integer:: sym
    parser%pushback=parser%sym
    parser%sym=sym
    parser%old_sym_n=parser%sym_n
    parser%old_sym_lineno=parser%sym_lineno
    parser%sym_n=parser%name_sym_n
    parser%sym_lineno=parser%name_lineno
  end subroutine push_back_name

  !======================================================
  ! Get next lexical token
  !======================================================
  subroutine scan(parser)
    type(parse_state),intent(inout):: parser
    integer:: sym
    type(pm_ptr):: val
    character(len=1):: c
    character(len=max_string+1):: buffer, dbx
    integer:: n

    ! If token pushed back then return it again
    if(parser%pushback>0) then
       parser%sym=parser%pushback
       parser%sym_n=parser%old_sym_n
       parser%sym_lineno=parser%old_sym_lineno
       parser%pushback=0       
       return
    endif

    parser%sym_n=parser%n-1
    parser%sym_lineno=parser%lineno

    ! Skip white space and comments
5   continue

    c=getchar()
    do
       do while(isspace(c)) 
          c=getchar()
       enddo
       if(c=='/') then
          if(peekchar()=='/') then
             parser%line(parser%ls)(parser%n-1:)=' '
             call next_line(parser)
             c=getchar()
          elseif(peekchar()=='*') then
             call skip_nested_comments(c)
          else
             exit
          endif
       endif
       if((.not.isspace(c)).and.(.not.c=='/')) exit
    enddo

    ! Line and character position of symbol
    if(c/=pm_eof_char) then
       parser%sym_n=parser%n-1
       parser%sym_lineno=parser%lineno
    endif
    
    ! Identify token
    select case(c)
    case(pm_eof_char)
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
             sym=pm_lname_entry(parser%context,&
                  int(parser%modl%data%ptr(parser%modl%offset+modl_name)%offset),&
                  buffer(2:n))
          endif
       else
          sym=pm_name_entry(parser%context,buffer(1:n))
       endif
    case('0','1','2','3','4','5','6','7','8','9')
       call numeric
    case(',')
       sym=sym_comma
    case(';')
       sym=sym_semi
    case('(')
       select case(peekchar())
       case('%')
          if(peekchar_plus(1)/=':') then
             c=getchar()
             sym=sym_open_brace
          else
             sym=sym_open
          endif
       case('/') 
          c=getchar()
          sym=sym_open_square
       case default
          sym=sym_open
       end select
    case(')')
       sym=sym_close
    case('+')
       if(peekchar()=='+') then
          c=getchar()
          sym=sym_concat
       else
          sym=sym_plus
       endif
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
    case('!')
       if(peekchar()=='=') then
          c=getchar()
          sym=sym_ne
       elseif(peekchar()=='!') then
          c=getchar()
          sym=sym_tilde
       else
          sym=sym_pling
       endif
    case('/')
       if(peekchar()=='=') then
          c=getchar()
          sym=sym_ne
       elseif(peekchar()==':') then
          c=getchar()
          sym=sym_bar
       elseif(peekchar()==')') then
          c=getchar()
          sym=sym_close_square
       else
          sym=sym_divide
       endif
    case(':')
       select case(peekchar())
       case(':')
          c=getchar()
          sym=sym_dcolon
       case default
          sym=sym_colon
       end select
    case('=')
       if(peekchar()=='=') then
          c=getchar()
          sym=sym_eq
       elseif(peekchar()=='>') then
          c=getchar()
          sym=sym_cond
       else
          sym=sym_assign
       endif
    case('>')
       if(peekchar()=='=') then
          c=getchar()
          sym=sym_ge
       elseif(peekchar()=='>') then
          c=getchar()
          sym=sym_close_attr
       else
          sym=sym_gt
       endif
    case('<')
       if(peekchar()=='<') then
          c=getchar()
          sym=sym_open_attr
       elseif(peekchar2()=='--') then
          c=getchar()
          c=getchar()
          sym=sym_move
       elseif(peekchar2()=='->') then
          c=getchar()
          c=getchar()
          sym=sym_swap
       elseif(peekchar2()=='==') then
          c=getchar()
          c=getchar()
          sym=sym_move_all
       elseif(peekchar()=='=') then
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
       elseif(iachar(peekchar())>=iachar('1').and.iachar(peekchar())<=iachar('7')) then
          c=getchar()
          sym=sym_d1+iachar(c)-iachar('1')
       else
          sym=sym_dot
       endif
    case('?')
       sym=sym_query
    case('"')
       n=0
       c=getstrchar()
       outer: do
          do while(c/='"')
             n=n+1
             buffer(n:n)=c
             if(n>=max_string) then
                call parse_error(parser,'String constant too long')
                call skip_rest_of_string
                exit outer
             endif
             c=getstrchar()
          enddo
          if(peekchar()=='"') then
             c=getchar()
             n=n+1
             buffer(n:n)='"'
             if(n>=max_string) then
                call parse_error(parser,'String constant too long')
                call skip_rest_of_string
                exit outer
             endif
             c=getstrchar()
          else
             exit
          endif
       enddo outer
!!$       n=n+1
!!$       buffer(n:n)='"'
       if(n==0) then
          val=pm_empty_obj(pm_string)
       else
          val=pm_new_string(parser%context,buffer(1:n))
       endif
       sym=sym_string
       parser%lexval=pm_new_literal_value_type(parser%context,val)
    case('''')
       if(peekchar()=='''') then
          sym=sym_caret
       else
          sym=sym_dash
       endif
    case('&')
       sym=sym_amp
    case('$')
       if(peekchar()=='$') then
          c=getchar()
          sym=sym_ddollar
       else
          sym=sym_dollar
       endif
       ! *********************************************    
       ! These symbols may not be convenient for all
       ! local character sets and/or keyboards
       ! Alternatives are available
       ! *********************************************
    case('[')                 ! Alternative (/
       sym=sym_open_square
    case(']')                 ! Alternative /)
       sym=sym_close_square
    case('{')                 ! Alternative (.
       sym=sym_open_brace     
    case('}')                 ! Alternative .)
       sym=sym_close_brace    
    case('|')                 ! Alternative /:
       sym=sym_bar            
    case('@')                 ! Alternative %%
       sym=sym_at      
    case('#')                 ! Alternative %:
       sym=sym_hash
    case('~')                 ! Alternative -:
       sym=sym_tilde
    case('^')                 ! Only used for internal system purposes
       if(.not.(parser%modl==parser%sysmodl)) then
          call parse_error(parser,'Error: Unexpected character "'//c//'"')
          goto 5
       elseif(peekchar()=='^') then
          c=getchar()
          sym=sym_dcaret
       else
          sym=sym_caret
       endif
       ! ****************************************
       ! End of extended character set symbols
       ! ****************************************
    case default
       call parse_error(parser,'Error: Unexpected character "'//c//'"')
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
    if(debug_parser) then
       if(parser%sym>=0.and.parser%sym<=num_sym) then
          write(*,*) 'scan::',parser%sym,sym_names(parser%sym)
       else
          call pm_name_string(parser%context,parser%sym,dbx)
          write(*,*) 'scan:',parser%sym,trim(dbx)
       endif
    endif
  contains

    include 'fnewnc.inc'
    include 'ftypeno.inc'
    include 'fname.inc'

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

    ! Look at next character but m on line but do not advance
    function peekchar_plus(m) result(ch)
      integer,intent(in)::m
      character(len=1):: ch
      ch=parser%line(parser%ls)(parser%n+m:parser%n+m)
    end function peekchar_plus

    ! Look at next 2 characters on line but do not advance
    function peekchar2() result(ch)
      character(len=2):: ch
      ch=parser%line(parser%ls)(parser%n:parser%n+1)
    end function peekchar2

    ! Get next character from string and advance
    function getstrchar() result(ch)
      character(len=1):: ch
      ch=parser%line(parser%ls)(parser%n:parser%n)
      parser%n=parser%n+1
      if(parser%n>parser%last) then
         if(ch/='"') then
            call parse_error(parser,'String does not terminate on line')
            ch='"'
         endif
         call next_line(parser)
      endif
    end function getstrchar

    ! Skip to end of string
    subroutine skip_rest_of_string
      integer::i
      i=index(parser%line(parser%ls)(parser%n:),'"')
      if(i==0) then
         call next_line(parser)
      else
         parser%n=i+1
         if(parser%n>parser%last) call next_line(parser)
      endif
    end subroutine skip_rest_of_string

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
      integer:: n,m,ibase,ios,type
      logical:: isreal,iscomplex,isshort,islong
      integer(pm_lln):: inumber
      real(pm_d) :: rnumber
      n=0
      isreal=.false.
      iscomplex=.false.
      isshort=.false.
      type=0
      do 
         n=n+1
         buffer(n:n)=c
         c=peekchar()
         if(c=='_') then
            if(.not.isdigit(peekchar_plus(1))) goto 20
            c=getchar()
            c=peekchar()
         endif
         if(.not.isdigit(c)) exit
         c=getchar()
      end do
      c=peekchar()
      if(c=='r'.or.c=='R') then
         c=getchar()
         read(unit=buffer(1:n),fmt='(G3.0)') ibase
         if(ibase/=2.and.ibase/=8.and.ibase/=10.and.ibase/=16) then
            call parse_error(parser, &
                 "Bad numeric base for non-decimal integer")
         else
            inumber=0
            do
               c=peekchar()
               if(peekchar()=='_') then
                  c=getchar()
                  c=peekchar()
               endif
               if(.not.(isalpha(c).or.isdigit(c))) exit
               c=getchar()
               n=iachar(c)
               if(n>=iachar('a')) then
                  n=n-iachar('a')+10
               elseif(n>=iachar('A')) then
                  n=n-iachar('A')+10
               else
                  n=n-iachar('0')
               endif
               if(n>=ibase) then
                  call parse_error(parser,"Bad digit for this base: "//c)
                  inumber=0
                  exit
               endif
               inumber=ibase*inumber+n
            enddo
            write(unit=buffer,fmt='(i40)') inumber
         endif
      else
         if(c=='.') then
            if(peekchar2()=='..') goto 10
            c=getchar()
            isreal=.true.
            n=n+1
            buffer(n:n)=c
            do
               c=peekchar()
               if(c=='_') then
                  if(.not.isdigit(peekchar_plus(1))) goto 20
                  c=getchar()
                  c=peekchar()
               else
                  if(.not.isdigit(c)) exit
               endif
               c=getchar()
               n=n+1
               buffer(n:n)=c
            enddo
            c=peekchar()
         end if
         if(c=='e'.or.c=='d'.or.c=='E'.or.c=='D') then
            c=getchar()
            if(c=='e'.or.c=='E') isshort=.true.
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
               if(peekchar()=='_') then
                  c=getchar()
               endif
            enddo
            c=peekchar()
            if(n==ibase) n=n-1
         end if
         if(c=='i'.or.c=='j'.or.c=='I'.or.c=='J') then
            c=getchar()
            isreal=.true.
            iscomplex=.true.
         endif
10       continue
         buffer(n+1:)=' '
         if(isreal) then
            read(unit=buffer,fmt='(G40.0)',iostat=ios) rnumber
         else
            read(unit=buffer,fmt='(G40.0)',iostat=ios) inumber          
         endif
         if(ios/=0) call parse_error(parser, &
              'Numeric constant out of range')
      endif
      c=peekchar()
      if(iscomplex) then
         val=pm_fast_newnc(parser%context, &
              pm_double_complex,1)
         val%data%dc(val%offset)=cmplx(0.0,rnumber,kind=pm_d)
      else if(isreal) then
         val=pm_fast_newnc(parser%context,pm_double,1)
         val%data%d(val%offset)=rnumber
      else
         val=pm_fast_newnc(parser%context,pm_long,1)
         val%data%ln(val%offset)=inumber
      endif
      parser%lexval=pm_new_literal_value_type(parser%context,val)
      sym=sym_number
      return
20    continue
      call parse_error(parser,'Malformed numeric constant')
    end subroutine numeric

    subroutine skip_nested_comments(cc)
      character(len=1),intent(out):: cc
      character(len=1)::c
      integer:: depth
      depth=1
      c=getchar()
      do
         c=getchar()
         if(c==pm_eof_char) then
            call parse_error(parser,'End of file encountered inside "/* ... */"')
            return
         endif
         if(c=='*') then
            c=getchar()
            if(c=='/') then
               c=getchar()
               depth=depth-1
               if(depth==0) exit
            endif
         elseif(c=='/') then
            c=getchar()
            if(c=='*') then
               depth=depth+1
               c=getchar()
            endif
         endif
      enddo
      cc=c
    end subroutine skip_nested_comments

  end subroutine  scan
   
  !======================================================
  ! Get start position of current symbol
  !======================================================
  subroutine get_sym_pos(parser,line,pos)
    type(parse_state),intent(in):: parser
    integer,intent(out):: line,pos
    line=parser%sym_lineno
    pos=parser%sym_n
  end subroutine get_sym_pos
  
  !======================================================
  ! Get start line of current symbol
  !======================================================
  function get_sym_line(parser) result(line)
    type(parse_state),intent(in):: parser
    integer:: line
    line=parser%sym_lineno
  end function get_sym_line
  
  !======================================================
  ! Get current position
  !======================================================
  subroutine get_pos(parser,line,pos)
    type(parse_state),intent(in):: parser
    integer,intent(out):: line,pos
    line=parser%lineno
    pos=parser%n
  end subroutine get_pos
   
  !======================================================
  ! Next token must be specific token or error raised
  ! (moves past the token)
  !======================================================
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
          if(parser%sym<=num_sym) then
             call parse_error(parser,'Expected "'//&
                  trim(sym_names(sym))//'" got "'//trim(sym_names(parser%sym))//'"')
          else
             call parse_error(parser,'Expected "'//&
                  trim(sym_names(sym))//'"')
          endif
       endif
    endif
  end function expect

  !======================================================
  ! Next token must be specific token or error raised
  ! (does not move past the token)
  !======================================================
  function require(parser,sym,mess) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    character(len=*),intent(in),optional:: mess
    logical:: iserr
    if(parser%sym==sym) then
       iserr=.false.
    else
       iserr=.true.
       if(present(mess)) then
          call parse_error(parser,'Expected '//mess)
       else
          call parse_error(parser,'Expected "'//&
               trim(sym_names(sym))//'"')
       endif
    endif
  end function require

  !======================================================
  ! Next token must be a name
  !======================================================
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

  !======================================================
  ! Next token must be a name, which is both pushed
  ! and returned
  !======================================================
  function expect_and_get_name(parser,name,mess) result(iserr)
    type(parse_state),intent(inout)::parser
    integer,intent(out):: name
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
       name=parser%sym
       call push_sym_val(parser,parser%sym)
       call scan(parser)
       iserr=.false.
    endif
  end function expect_and_get_name

  !======================================================
  ! If next token is a name, push its value and scan
  !======================================================
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

  !======================================================
  ! If next token is a name, return its value and scan
  !======================================================
  function check_name(parser,sym) result(ok)
    type(parse_state),intent(inout)::parser
    integer,intent(out):: sym
    logical:: ok
    if(parser%sym>=0.and.parser%sym<=num_sym) then
       ok=.false.
    else
       sym=parser%sym
       parser%name_lineno=parser%sym_lineno
       parser%name_sym_n=parser%sym_n
       call scan(parser)
       ok=.true.
    endif
  end function check_name

  !============================================================================
  ! If next token is a name, return its value and location in source and scan
  !============================================================================
  function check_name_pos(parser,sym,line,pos) result(ok)
    type(parse_state),intent(inout)::parser
    integer,intent(out):: sym
    integer,intent(out):: line,pos
    logical:: ok
    if(parser%sym>=0.and.parser%sym<=num_sym) then
       ok=.false.
    else
       sym=parser%sym
       line=parser%sym_lineno
       pos=parser%sym_n
       parser%name_lineno=parser%sym_lineno
       parser%name_sym_n=parser%sym_n
       call scan(parser)
       ok=.true.
    endif
  end function check_name_pos


  !======================================================
  ! Check next token is a name
  ! and that it is not equal to any in stack(base+1:top)
  !======================================================
  function check_name_no_repeat(parser,name,base) result(ok)
    type(parse_state),intent(inout):: parser
    integer,intent(out):: name
    integer,intent(in):: base
    logical:: ok
    integer:: i
    if(parser%sym<=num_sym) then
       ok=.false.
       return
    endif
    ok=.true.
    name=parser%sym
    do i=base+1,parser%top
       if(abs(parser%stack(i))==name) then
          call parse_error(parser,'Repeated element name: '//&
               trim(pm_name_as_string(parser%context,name)))
       endif
    enddo
    call scan(parser)
  end function check_name_no_repeat
  

  ! *****************************************************************
  ! FOLLOWING FUNCTIONS PARSE PM GRAMMER IN MOSTLY TOP-DOWN FASHION
  ! MOST PARSING ROUTINES RETURN TRUE ON FAILURE
  ! *****************************************************************

  !======================================================
  ! Make a simple call node (no keys or &args)
  ! Name and args must be top two items on vstack
  !======================================================
  subroutine simple_call(parser)
    type(parse_state),intent(inout):: parser
    call push_null_val(parser)      ! amps
    call push_null_val(parser)      ! keys
    call push_null_val(parser)      ! key names
    call push_num_val(parser,0)     ! flags
    call make_node(parser,sym_open,6)    
  end subroutine simple_call

  !======================================================
  ! Procedure call (scanner must be on token *after* name
  !======================================================)
  recursive function proccall(parser,name) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    logical:: iserr
    integer:: i
    iserr=.true.
    call push_name_val(parser,name)
    if(arglist(parser)) return
    iserr=.false.
  end function proccall

  !======================================================
  ! Argument lists for procedure calls
  !======================================================
  recursive function arglist(parser,yield,dot) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in),optional:: yield
    type(pm_ptr),intent(in),optional:: dot
    logical:: iserr
    integer m,n,base,sym,msym,flags,line,pos
    type(pm_ptr):: temp
    iserr=.true.
    call get_sym_pos(parser,line,pos)
    n=0
    base=parser%top

    if(parser%sym==sym_pct) then
       flags=proccall_is_comm
       call scan(parser)
    elseif(parser%sym==sym_dash) then
       flags=proccall_is_comm+proccall_is_general
       call scan(parser)
    elseif(present(yield)) then
       if(yield) then
          flags=proccall_is_yield
       else
          flags=proccall_is_block
       endif
    elseif(present(dot)) then
       flags=proccall_is_comm+proccall_is_ref+proccall_is_general
    else
       flags=0
    endif

    call push_sym_val(parser,sym_topology)
    call make_node(parser,sym_name,1)
    m=1
    if(iand(flags,proccall_is_comm)/=0) then
       call push_sym_val(parser,sym_outer)
       call make_node(parser,sym_name,1)
       call push_sym_val(parser,sym_region)
       call make_node(parser,sym_name,1)
       call push_sym_val(parser,sym_subregion)
       call make_node(parser,sym_name,1)
       call push_sym_val(parser,sym_here_in_tile)
       call make_node(parser,sym_name,1)
       call push_sym_val(parser,sym_mask)
       call make_node(parser,sym_name,1)
       m=num_comm_args
    endif

    if(present(yield)) then
       if(yield) then
          call push_sym_val(parser,sym_block_inouts)
          call push_sym(parser,m+1)
          call make_node(parser,sym_name,1)
          call push_sym_val(parser,sym_block_ins)
          call make_node(parser,sym_name,1)
          m=m+2
       else
          call push_sym_val(parser,sym_block_proc_a)
          call make_node(parser,sym_name,1)
          call push_sym_val(parser,sym_block_inouts_a)
          call make_node(parser,sym_name,1)
          call push_sym(parser,m+2)
          call push_sym_val(parser,sym_block_ins_a)
          call make_node(parser,sym_name,1)
          m=m+3
       endif
    elseif(present(dot)) then
       call push_val(parser,dot)
       m=m+1
       call push_sym(parser,m)
    endif

    if(expect(parser,sym_open)) return

    ! Call attributes but no arguments
    if(parser%sym==sym_open_attr) then
       if(call_attr(parser,.true.,flags)) return
       if(parser%sym/=sym_close) then
          if(expect(parser,sym_close)) return
       endif
    endif

    ! Call with no arguments
    if(parser%sym==sym_close) then
       call make_node(parser,sym_list,m)  ! args
       if(parser%top>base) then           ! amps
          call name_vector(parser,base)
       else
          call push_null_val(parser)
       endif
       call push_null_val(parser)         ! keys
       call push_null_val(parser)         ! key names
       call push_num_val(parser,flags)    ! flags
       call make_node_at(parser,sym_open,6,line,pos)
       call scan(parser)
       iserr=.false.
       return
    endif

    ! Positional argument
    do 
       if(parser%sym==sym_amp) then
          call scan(parser)
          if(valref(parser)) return
          m=m+1
          call push_sym(parser,m)
       else if(parser%sym==sym_dotdotdot) then
          call push_sym_val(parser,sym_dotdotdot)
          call make_node(parser,sym_dotdotdot,1)
          call make_node(parser,sym_dotdotdot,m+1)
          if(parser%top>base) then
             call name_vector(parser,base)
          else
             call push_null_val(parser)
          endif
          base=parser%top
          call scan(parser)
          exit
       else
          if(check_name(parser,sym)) then
             if(parser%sym==sym_assign) then
                call make_node(parser,sym_list,m)
                if(parser%top>base) then
                   call name_vector(parser,base)
                else
                   call push_null_val(parser)
                endif
                base=parser%top
                call push_sym(parser,sym)
                call scan(parser)
                if(expr(parser)) return
                n=1
                exit
             else
                call push_back(parser,sym)
             endif
          endif
          if(expr(parser)) return
          m=m+1
       endif
       if(parser%sym/=sym_comma) then
          call make_node(parser,sym_list,m)
          if(parser%top>base) then
             call name_vector(parser,base)
          else
             call push_null_val(parser)
          endif
          exit
       endif
       call scan(parser)
    enddo

    ! Remaining keyword arguments
    do while(parser%sym==sym_comma)
       call scan(parser)
       if(check_name(parser,sym)) then
          call push_sym(parser,sym)
       else
          if(expect_name(parser,&
               'keyword argument name')) return
       endif
       if(expect(parser,sym_assign,&
            'keyword argument "="')) return
       if(expr(parser)) return
       n=n+1
    enddo

    ! List of keyword expressions
    if(n>0) then
       call make_node(parser,sym_list,n)
    else
       call push_null_val(parser)
    endif

    ! Vector of keyword names
    if(parser%top>base) then
       call name_vector(parser,base)
    else
       call push_null_val(parser)
    endif

    ! Call attributes if present
    if(parser%sym==sym_open_attr) then
       if(call_attr(parser,.true.,flags)) return
    endif

    call push_num_val(parser,flags)
    call make_node_at(parser,sym_open,6,line,pos)

    if(m+n>pm_max_args) then
       call parse_error(parser,&
            'Too many arguments to proc call - maximum is:'//trim(pm_int_as_string(pm_max_args)))
    endif
    if(expect(parser,sym_close)) return
    iserr=.false.
  end function arglist

   !======================================================
  ! Procedure/call attributes
  !======================================================
  recursive function call_attr(parser,iscall,flags) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: iscall
    integer,intent(inout):: flags
    logical:: iserr
    integer:: m
    iserr=.true.
    call scan(parser)
    do
       select case(parser%sym) 
       case(sym_inline)
          call set_flags(proccall_is_inline)
          call scan(parser)
       case(sym_no_inline)
          call set_flags(proccall_is_no_inline)
          call scan(parser)
       case(sym_ignore_rules)
          call set_flags(call_ignore_rules)
          call scan(parser)
       case(sym_keep_literals)   !!! IS THIS TO BE USED?
          call set_flags(call_is_fixed)
          call scan(parser)
       case(sym_pm_ref)
          call set_flags(proccall_is_ref)
          call scan(parser)
       end select
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    if(iand(flags,proccall_is_inline+proccall_is_no_inline)==&
         proccall_is_inline+proccall_is_no_inline) then
       call parse_error(parser,&
            'Cannot have both "<<inline>>" and "<<no_inline>>" attributes together')
    endif
    if(expect(parser,sym_close_attr)) return
    iserr=.false.
  contains
    subroutine set_flags(new_flags)
      integer,intent(in):: new_flags
      if(iand(flags,new_flags)/=0) then
         call parse_error(parser,&
              'Cannot repeat attribute "'//trim(sym_names(parser%sym))//'"')
      endif
      flags=ior(flags,new_flags)
    end subroutine set_flags
  end function call_attr

  
  !====================================================================
  ! Qualifiers
  ! .name .digit .{}  [] .name() .{}() .() .'() .%()
  ! Will immediately return true in dot_call if dot_call is present
  ! and parser just encountered a .() or .'() or .%() call
  ! Will return true in last_is_method if this present
  ! and qualifier finishes on a .name() or .{}() method call
  !====================================================================
  recursive function qual(parser,dot_call,last_is_method) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(inout),optional:: dot_call,last_is_method
    logical:: iserr
    integer:: sym,line,pos,n,m
    logical:: finish_on_method
    iserr=.true.
    n=1
    if(parser%sym==sym_pling) then
       call scan(parser)
       call make_node(parser,sym_pling,0)
       n=n+1
    elseif(parser%sym==sym_at) then
       call scan(parser)
       call make_node(parser,sym_at,0)
       n=n+1
    endif
    finish_on_method=.false.
    do
       select case(parser%sym)
       case(sym_dot)
          call get_sym_pos(parser,line,pos)
          call scan(parser)
          select case(parser%sym)
          case(sym_open,sym_pct,sym_dash)
             if(n>1) call make_node_at(parser,sym_reference,n,line,pos)
             call make_node_at(parser,sym_dot,1,line,pos)
             if(arglist(parser)) return
             if(present(dot_call)) then
                dot_call=.true.
                iserr=.false.
                return
             endif
             n=1
          case(sym_open_brace)
             call scan(parser)
             if(expr(parser)) return
             if(expect(parser,sym_close_brace)) return
             if(parser%sym==sym_open) then
                call scan(parser)
                if(exprlist(parser,sym=sym_pm_list)) return
                if(expect(parser,sym_close)) return
                call make_node_at(parser,sym_open,2,line,pos)
                finish_on_method=.true.
             else
                call make_node_at(parser,sym_open_brace,1,line,pos)
                finish_on_method=.false.
             endif
             n=n+1
          case(sym_caret)
             call scan(parser)
             if(expect_name(parser)) return
             if(expect(parser,sym_open)) return
             if(exprlist(parser,m,nolist=.true.)) return
             call make_node_at(parser,sym_caret,m+1,line,pos)
          case default
             if(expect_name(parser)) return
             sym=parser%sym
             if(sym==sym_dcolon) then
                call scan(parser)
                if(expect_name(parser)) return
                call make_node(parser,sym_proc,2)
                if(expect(parser,sym_open)) return
                if(exprlist(parser,sym=sym_pm_list)) return
                call make_node(parser,sym_open,2)
                if(expect(parser,sym_close)) return
                finish_on_method=.true.
             elseif(sym==sym_open) then
                call make_node(parser,sym_proc,1)
                call scan(parser)
                if(exprlist(parser,sym=sym_pm_list)) return
                call make_node(parser,sym_open,2)
                if(expect(parser,sym_close)) return
                finish_on_method=.true.
             else
                call make_node_at(parser,sym_dot,1,line,pos)
                finish_on_method=.false.
             endif
             n=n+1
          end select
       case(sym_d1:sym_d7)
          call get_sym_pos(parser,line,pos)
          call push_sym_val(parser,parser%sym)
          call make_node_at(parser,sym_dot,1,line,pos)
          call scan(parser)
          finish_on_method=.false.
          n=n+1
       case(sym_open_square)
          call get_sym_pos(parser,line,pos)
          call push_sym_val(parser,sym_tuple)
          if(subscript(parser)) return
          call simple_call(parser)
          call make_node_at(parser,sym_sub,1,line,pos)
          finish_on_method=.false.
          n=n+1
       case default
          if(n>1) call make_node(parser,sym_reference,n)
          exit
       end select
    enddo
    if(present(last_is_method)) last_is_method=finish_on_method
    iserr=.false.
  end function qual

  !==================================================================
  ! Array/matrix list:  ( expr, expr ... ; expr , expr ... ; ...)
  !==================================================================
  recursive function matrix_former(parser,symb,sym,row,col) &
       result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: symb,sym
    logical,intent(in):: row,col
    logical:: iserr
    integer:: m,n,oldm,mstart,nstart
    mstart=0
    nstart=0
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
          if(symb==sym_close_brace.and.parser%sym==sym_colon) then
             call scan(parser)
             if(mstart/=0.or.nstart/=0) then
                call parse_error(parser,&
                     'Cannot have multiple ":" entries in array or matrix expression')
             endif
             mstart=-m
             nstart=-n
             if(expr(parser)) return
             if(expect(parser,sym_colon)) return
          else
             if(expr(parser)) return
          endif
          m=m+1
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       if(oldm>0.and.m/=oldm) then
          call parse_error(parser,&
               'Rows of different lengths in [ ]')
          return
       endif
       oldm=m
       n=n+1
       if(parser%sym==sym_semi) then
          call scan(parser)
       else if(parser%sym==symb) then
          exit
       else if(.not.parser%atstart) then
          exit
       endif
    enddo
    call make_node(parser,sym_list,m*n)
    call push_num_val(parser,m)
    call push_num_val(parser,n)
    call push_num_val(parser,mstart)
    call push_num_val(parser,nstart)
    call make_node(parser,sym,5)
    if(expect(parser,symb,'closing '//sym_names(symb))) return
    iserr=.false.
  end function matrix_former

  !======================================================
  ! Array former { ... }
  !======================================================
  recursive function array_former(parser,symb) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: symb
    integer:: sym
    logical:: iserr
    iserr=.true.
    if(symb==sym_close_brace) then
       sym=sym_array_former
    else
       sym=sym_matrix_former
    endif
    call scan(parser)
    if(parser%sym==sym_colon) call scan(parser)
    if(expr(parser)) return
    if(parser%sym==sym_comma) then
       call scan(parser)
       if(matrix_former(parser,symb,sym,.false.,.true.)) return
    else if(parser%sym==sym_semi) then
       call scan(parser)
       if(matrix_former(parser,symb,sym,.true.,.false.)) return
    elseif(parser%sym==sym_colon) then
       !write(*,*)'>>',trim(parser%line(parser%ls))
       call scan(parser)
       if(iter(parser,.true.)) return
       call make_node(parser,sym_for,2)
       if(expect(parser,symb)) return
    else if(parser%sym/=symb) then
       if(parser%atstart) then
          if(matrix_former(parser,symb,sym,.true.,.false.)) return
       else
          call parse_error(parser,'Expected: '//sym_names(symb))
       endif
    else
       if(sym/=sym_close) then
          call make_node(parser,sym_list,1)
          call push_num_val(parser,1)
          call push_num_val(parser,1)
          call push_num_val(parser,0)
          call push_num_val(parser,0)
          call make_node(parser,sym,5)
       endif
       call scan(parser)
    endif
    iserr=.false.
  end function array_former

  !======================================================
  ! Operator symbols within proc
  !======================================================
  recursive function op(parser,sym,isconst,istype) result(iserr)
    type(parse_state):: parser
    integer,intent(out):: sym
    logical,intent(in):: isconst,istype
    logical:: iserr
    iserr=.true.
    select case(parser%sym)
    case(first_operator:last_operator,sym_as)
       sym=parser%sym
       call scan(parser)
    case(sym_open_square)
       call scan(parser)
       if(expect(parser,sym_close_square)) return
       sym=sym_sub
    case default
       if(.not.check_name(parser,sym)) then
          call parse_error(parser,'Malformed "proc" identifier')
          return
       endif
    end select
    iserr=.false.
  end function op

  !==========================================================
  ! Structure expression new name { name=.. }
  ! Parse node contains full_type/ list_of_expr / name / tag
  !==========================================================
  recursive function rec_gen(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: base,vbase,i,name,line,pos,line1,pos1,tag
    iserr=.true.
    call scan(parser)
    base=parser%top

    if(parser%sym<=num_sym) then
       call parse_error(parser,'Expected struct/rec name')
       return
    endif
    call get_sym_pos(parser,line1,pos1)
    tag=parser%sym
    call push_sym(parser,tag)
    call scan(parser)
    if(parser%sym==sym_open) then
       call push_back(parser,tag)
       if(typ(parser)) return
    else
       call push_null_val(parser)
    endif
    if(expect(parser,sym_open_brace)) return
    vbase=parser%vtop
    if(parser%sym/=sym_close_brace) then
       do
          if(check_name_pos(parser,name,line,pos)) then
             do i=base+2,parser%top
                if(parser%stack(i)==name) then
                   call parse_error(parser,'Repeated element name: '//&
                        trim(pm_name_as_string(parser%context,name)))
                endif
             enddo
             call push_sym(parser,name)
          else
             call parse_error(parser,&
                  'Expected name of struct or rec element')
             return
          endif
          if(expect(parser,sym_assign)) return
          if(expr(parser)) return
          call make_node_at(parser,sym_assign,1,line,pos)
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
    endif
    if(expect(parser,sym_close_brace)) return
    call make_node_at(parser,sym_list,parser%vtop-vbase,line1,pos1)
    call name_vector(parser,base)
    call push_sym_val(parser,tag)
    call make_node_at(parser,sym_rec,4,line1,pos1)
    iserr=.false.
  end function rec_gen

  !======================================================
  ! Tuple former
  !======================================================
  recursive function tuple(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    iserr=.true.
    call push_sym_val(parser,sym_tuple)
    if(subscript(parser)) return
    call simple_call(parser)
    iserr=.false.
  end function tuple
  
  !======================================================
  ! Term in an expression
  !======================================================
  recursive function term(parser,checkqual) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: checkqual
    logical:: iserr
    integer:: i,m,name,sym,base,line,pos
    logical:: atstart,dot_call
    iserr=.true.
    sym=parser%sym
    select case(sym)
    case(sym_if)
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(expr(parser)) return
       if(expect(parser,sym_cond)) return
       if(expr(parser)) return
       if(expect(parser,sym_comma)) return
       m=1
       do
          if(expr(parser)) return
          if(parser%sym/=sym_cond) exit
          call scan(parser)
          if(expr(parser)) return
          m=m+1
          if(expect(parser,sym_comma)) return
       enddo
       if(expect(parser,sym_close)) return
       do i=1,m
          call make_node(parser,sym_if_expr,3)
       enddo
    case(sym_switch)
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(expr(parser)) return
       if(sym==sym_switch) then
          if(expect(parser,sym_comma)) return
          if(expr(parser)) return
          m=3
       else
          m=2
       endif
       if(expect(parser,sym_cond)) return
       if(expr(parser)) return
       if(expect(parser,sym_comma)) return
       do
          if(expr(parser)) return
          m=m+1
          if(parser%sym/=sym_cond) exit
          call scan(parser)
          if(expr(parser)) return
          m=m+1
          if(expect(parser,sym_comma)) return
       enddo
       if(expect(parser,sym_close)) return
       call make_node(parser,sym_switch_expr,m)
    case(sym_yield)
       if(yield(parser,.true.)) return
    case(sym_pm_yield)
       if(yield(parser,.false.)) return
    case(sym_open)
       call scan(parser)
       if(expr(parser)) return
       if(parser%sym/=sym_close) then
          if(parser%sym==sym_comma) then
             call scan(parser)
             if(matrix_former(parser,sym_close,&
                  sym_matrix_former,.false.,.true.)) &
                  return
          elseif(parser%sym==sym_semi) then
             call scan(parser)
             if(matrix_former(parser,sym_close,&
                  sym_matrix_former,.true.,.false.)) &
                  return
          elseif(parser%atstart) then
             if(matrix_former(parser,sym_close,&
                  sym_matrix_former,.true.,.false.)) &
                  return
          else
             if(expect(parser,sym_close)) return
          endif
       else
          call scan(parser)
       endif
    case(sym_open_square)
       if(tuple(parser)) return
    case(sym_open_brace)
       if(array_former(parser,sym_close_brace)) return
    case(sym_rec)
       if(rec_gen(parser)) return
    case(sym_present)
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(expect_name(parser)) return
       call make_node(parser,sym_present,1)
       if(expect(parser,sym_close)) return
    case(sym_number,sym_string)
       call push_num_val(parser,parser%lexval)
       call make_node(parser,sym,1)
       call scan(parser)
    case(sym_dollar)
       call scan(parser)
       if(op(parser,name,.true.,.false.)) return
       call push_sym_val(parser,name)
       if(parser%sym==sym_dcolon.and.name>num_sym) then
          call scan(parser)
          if(expect_name(parser)) return
          call make_node(parser,sym_proc,2)
       else
          call make_node(parser,sym_proc,1)
       endif
       if(parser%sym==sym_open.or.parser%sym==sym_pct.or.parser%sym==sym_dash) then
          if(arglist(parser)) return
       endif
    case(sym_param)
       call scan(parser)
       if(expect(parser,sym_open_brace)) return
       if(expect_name(parser)) return
       if(parser%sym==sym_dcolon) then
          call scan(parser)
          if(expect_name(parser)) return
          call make_node(parser,sym_param,2)
       else
          call make_node(parser,sym_param,1)
       endif
       if(expect(parser,sym_close_brace)) return
    case(sym_lt)
       call scan(parser)
       if(typ(parser)) return
       if(expect(parser,sym_gt)) return
       call make_node(parser,sym_type_val,1)
    case(sym_pm_list)
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(parser%sym==sym_close) then
          call scan(parser)
          m=0
       else
          if(exprlist(parser,m,nolist=.true.)) return
          if(expect(parser,sym_close)) return
       endif
       call make_node(parser,sym_pm_list,m)
    case(sym_fix,sym_literal)
       call scan(parser)
       if(parser%sym==sym_open_square) then
          call push_sym_val(parser,sym_tuple)
          if(subscript(parser)) return
          call simple_call(parser)
          call make_node(parser,sym,1)
       else
          if(expect(parser,sym_open)) return
          if(expr(parser)) return
          if(expect(parser,sym_close)) return
          call make_node(parser,sym,1)
       endif
    case(sym_null)
       if(parser%sym==sym_open) then
          call scan(parser)
          if(proccall(parser,sym)) return
       else
          call make_node(parser,sym,0)
          call scan(parser)
       endif
    case(sym_true,sym_false,sym_underscore) 
       call make_node(parser,sym,0)
       call scan(parser)
       goto 20
  
       ! ** These are for internal use by the compiler only **
    case(sym_caret)
       call scan(parser)
       if(parser%sym==sym_query) then
          call scan(parser)
          if(parser%sym==sym_query) then
             call push_null_val(parser)
             call scan(parser)
          elseif(parser%sym==sym_open) then
             call scan(parser)
             if(expr(parser)) return
             if(expect(parser,sym_close)) return
          else
             if(expect_name(parser)) return
             call make_node(parser,sym_name,1)
          endif
          call make_node(parser,sym_query,1)
       elseif(parser%sym==sym_lt) then
          call scan(parser)
          if(term(parser,.false.)) return
          if(parser%sym==sym_colon) then
             call scan(parser)
             if(term(parser,.false.)) return
             call make_node(parser,sym_pval,2)
          else
             if(expect(parser,sym_comma)) return
             if(term(parser,.false.)) return
             call make_node(parser,sym_pval_as,2)
          endif
          if(expect(parser,sym_gt)) return
       else
          if(expect(parser,sym_open)) return
          if(parser%sym==sym_amp) then
             call scan(parser)
             if(valref(parser)) return
             call make_node(parser,sym_caret,1)
             if(expect(parser,sym_close)) return
          else
             if(expr(parser)) return
             if(parser%sym==sym_comma) then
                call scan(parser)
                if(parser%sym>=first_mode.and.&
                     parser%sym<=last_mode) then
                   call push_sym_val(parser,parser%sym)
                   call scan(parser)
                else
                   call parse_error(parser,'Need mode name')
                   return
                endif
             else
                call push_null_val(parser)
             endif
             if(expect(parser,sym_close)) return
             call make_node(parser,sym_caret,2)
          endif
       endif
   case(sym_cast)
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(expr(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       if(expect(parser,sym_close)) return
       call make_node(parser,sym,2)
    case(sym_dcaret)
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(expr(parser)) return
       if(expect(parser,sym_close)) return
       call make_node(parser,sym_dcaret,1)
    case(sym_pm_dref:sym_pm_ref)
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(exprlist(parser,m,nolist=.true.)) return
       if(expect(parser,sym_close)) return
       if(m/=3) then
          call parse_error(parser,'Wrong number of args to: '//sym_names(sym))
          return
       endif
       call make_node(parser,sym,m)
    case(sym_pm_each_index)
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(expect_name(parser)) return
       if(expect(parser,sym_in)) return
       if(expr(parser)) return
       if(expect(parser,sym_colon)) return
       if(expr(parser)) return
       if(expect(parser,sym_close)) return
       call make_node(parser,sym_pm_each_index,3)
    case default
       if(check_name_pos(parser,name,line,pos)) then
          select case(parser%sym)
          case(sym_open,sym_pct,sym_dash)
             if(proccall(parser,name)) return
          case(sym_dcolon)
             call scan(parser)
             call push_name_val_at(parser,name,line,pos)
             if(expect_name(parser)) return
             call make_node(parser,sym_use,2)
             if(parser%sym==sym_open.or.&
                  parser%sym==sym_pct.or.parser%sym==sym_dash) then
                if(arglist(parser)) return
             endif
          case(sym_dot,sym_open_square,sym_d1:sym_d7,sym_at)
             call push_name_val_at(parser,name,line,pos)
             call make_node(parser,sym_name,1)
             dot_call=.false.
             if(qual(parser,dot_call)) return
             iserr=.false.
             return
          case default
             call push_name_val_at(parser,name,line,pos)
             call make_node(parser,sym_name,1)
          end select
       else
          call parse_error(parser,'Malformed expression')
          return
       endif
    end select
10  continue
    if(checkqual) then
        if(qual(parser)) return
     endif
20  continue
    iserr=.false.
  contains
    include 'fisnull.inc'
    include 'fvkind.inc'
  end function term

  ! =========================================
  ! Yield statement or expression
  ! =========================================
  recursive function yield(parser,isyield) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: isyield
    logical:: iserr
    iserr=.true.
    call scan(parser)
    call push_name_val(parser,sym_block_proc)
    call make_node(parser,sym_name,1)
    call make_node(parser,sym_dot,1)
    call push_back(parser,sym_dash)
    if(isyield) then
       if(arglist(parser,yield=.true.)) return
    else
       if(arglist(parser)) return
    endif
    iserr=.false.
  end function yield
  

  !======================================================
  ! Expression
  !======================================================
  recursive function expr(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: n
    iserr=.true.
    if(expr1(parser,100)) return
    iserr=.false.
  end function expr
    
  recursive function expr1(parser,priority) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: priority
    logical:: iserr

    integer,parameter:: priority_uhash=1     ! # (unary)
    integer,parameter:: priority_pow=2       ! **
    integer,parameter:: priority_mult=3      ! * /
    integer,parameter:: priority_uminus=4    ! - (unary)  ~
    integer,parameter:: priority_hash=5      ! #
    integer,parameter:: priority_as=6        ! as
    integer,parameter:: priority_bitshift=7  ! shift
    integer,parameter:: priority_bitand=8    ! &
    integer,parameter:: priority_bitxor=9    ! ~
    integer,parameter:: priority_bitor=10    ! |
    integer,parameter:: priority_mod=11      ! mod
    integer,parameter:: priority_add=12      ! + -
    integer,parameter:: priority_dotdot=13   ! .. ...
    integer,parameter:: priority_by=14       ! by
    integer,parameter:: priority_gt=15       ! < <= > >= inc in is
    integer,parameter:: priority_eq=16       ! == !=
    integer,parameter:: priority_not=17      ! not
    integer,parameter:: priority_and=18      ! and
    integer,parameter:: priority_or=19       ! or
    integer,parameter:: priority_ortho=20    ! ortho
    integer,parameter:: priority_fmt=21      ! fmt
    integer,parameter:: priority_concat=22   ! ++
    
    integer:: line,pos,last_priority,sym
    
    iserr=.true.
    ! Unary / prefix expressions
    sym=parser%sym
    select case(sym)
    case(sym_minus)
       if(unary(priority_uminus,sym_minus)) return
    case(sym_plus)
       if(unary(priority_uminus,sym_plus)) return
    case(sym_mult)
       if(unary(priority_uminus,sym_ustar)) return
    case(sym_tilde)
       if(unary(priority_uminus,sym_tilde)) return
    case(sym_hash)
       if(unary(priority_uhash,sym_uhash)) return
    case(sym_not)
       if(unary(priority_not,sym_not)) return
    case(sym_by)
       if(unary(priority_by,sym_by)) return
    case(sym_ortho)
       if(unary(priority_ortho,sym_ortho)) return
    case default
       if(term(parser,.true.)) return
    end select

    ! Binary or postfix expressions
    last_priority=0
    do
       sym=parser%sym
       select case(sym)
       case(sym_concat)
          if(binary(priority_concat)) return
       case(sym_fmt)
          if(binary_none(priority_fmt)) return
       case(sym_by)
          if(binary_none(priority_by)) return
       case(sym_dotdot)
          if(binary_none(priority_dotdot)) return
       case(sym_or)
          if(binary(priority_or)) return
       case(sym_and)
          if(binary(priority_and)) return
       case(sym_eq,sym_ne)
          if(binary_none(priority_eq)) return
       case(sym_gt,sym_ge,sym_lt,sym_le,sym_in,sym_not_in,&
            sym_includes,sym_not_includes,sym_is)
          if(binary_none(priority_gt)) return
       case(sym_mod,sym_div)
          if(binary(priority_mod)) return
       case(sym_plus,sym_minus)
          if(binary(priority_add)) return
       case(sym_mult,sym_divide)
          if(binary(priority_mult)) return
       case(sym_pow)
          if(binary(priority_pow,.true.)) return
       case(sym_bar)
          if(binary(priority_bitor)) return
       case(sym_amp)
          if(binary(priority_bitand)) return
       case(sym_tilde)
          if(binary(priority_bitxor)) return
       case(sym_shift)
          if(binary(priority_bitshift)) return
       case(sym_hash)
          if(binary_none(priority_hash)) return
       case(sym_as)
          if(binary_none(priority_as)) return
       case(sym_ortho)
          if(binary_none(priority_ortho)) return
       case default
          exit
       end select
    enddo
    iserr=.false.
  contains
    
    recursive function unary(new_priority,usym) result(leave)
      integer,intent(in):: new_priority,usym
      logical:: leave
      integer:: sym,line,pos
      call get_sym_pos(parser,line,pos)
      sym=parser%sym
      if(new_priority>priority) then
         call parse_error(parser,'"'//trim(sym_names(sym))//&
              '" cannot follow an operator with higher precedence')
         leave=.true.
         return
      endif
      call scan(parser)
      leave=expr1(parser,new_priority)
      if(.not.leave) call make_node_at(parser,usym,1,line,pos)
    end function unary
    
    recursive function binary_none(new_priority) result(leave)
      integer,intent(in):: new_priority
      logical:: leave
      call no_repeat(new_priority)
      leave=binary(new_priority)
    end function binary_none

    recursive function binary(new_priority,isright) result(leave)
      integer,intent(in):: new_priority
      logical,intent(in),optional:: isright
      logical:: leave
      integer:: sym,line,pos
      call get_sym_pos(parser,line,pos)
      sym=parser%sym
      if(new_priority>=priority) then
         iserr=.false.
         leave=.true.
         return
      endif
      call scan(parser)
      leave=expr1(parser,new_priority+merge(1,0,present(isright)))
      if(.not.leave) call make_node_at(parser,sym,2,line,pos)
    end function binary

    subroutine no_repeat(new_priority)
      integer,intent(in):: new_priority
      if(last_priority==new_priority) then
         call parse_error(parser,'Cannot repeat "'//&
              trim(sym_names(parser%sym))//'" one after another')
      else
         last_priority=new_priority
      endif
    end subroutine no_repeat

  end function expr1

  !======================================================
  ! Comma separated list of expr
  !======================================================
  recursive function exprlist(parser,length,nolist,sym) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out),optional:: length
    logical,intent(in),optional:: nolist
    integer,intent(in),optional:: sym
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
    if(.not.present(nolist)) then
       if(present(sym)) then
          call make_node(parser,sym,n)
       else
          call make_node(parser,sym_list,n)
       end if
    end if
    if(present(length)) length=n
    iserr=.false.
  end function exprlist

  !======================================================
  ! Subscript / tuple
  !======================================================
  recursive function subscript(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: n,sym
    iserr=.true.
    call scan(parser)
    n=1
    call push_sym_val(parser,sym_topology)
    call make_node(parser,sym_name,1)
    sym=sym_list
    do
       if(parser%sym==sym_dotdotdot) then
          call push_sym_val(parser,sym_dotdotdot)
          call make_node(parser,sym_dotdotdot,1)
          call scan(parser)
          sym=sym_dotdotdot
          n=n+1
          exit
       endif
       if(sexpr()) return
       n=n+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    if(n>8) then
       call parse_error(parser,'Cannot have more than seven dimensions in tuple or subscript')
    endif
    call make_node(parser,sym,n)
    if(expect(parser,sym_close_square)) return
    iserr=.false.
    
  contains

    ! Single subscript
    recursive function sexpr() result(iserr)
      logical:: iserr
      integer:: s
      type(pm_ptr):: junk
      iserr=.true.
      if(parser%sym==sym_comma.or.parser%sym==sym_close_square) then
         call make_node(parser,sym_null,0)
      elseif(parser%sym==sym_underscore) then
         call make_node(parser,sym_underscore,0)
         call scan(parser)
      else
         if(expr(parser)) return
      endif
      iserr=.false.
    end function sexpr
    
  end function subscript

  !=============================================================
  ! Left hand side of an assignment or zero return call
  ! { ( name [ qual ] [op] | _ ) ,}
  ! Returns number of elements, number of underscores
  ! is_call if just a call
  ! cannot_be_move if operators present
  !==============================================================
  function lhs(parser,n,nu,is_call,cannot_be_move,last_is_method) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out):: n,nu
    logical,intent(inout),optional:: is_call,cannot_be_move,last_is_method
    logical:: iserr
    logical:: dotcall
    iserr=.true.
    n=0
    nu=0

    ! ( name [ qual ] | _ )*
    do
       if(parser%sym==sym_underscore) then
          call make_node(parser,sym_underscore,0)
          call scan(parser)
          nu=nu+1
          n=n+1
          if(parser%sym/=sym_comma) exit
          call scan(parser)
          cycle
       else
          n=n+1
          if(expect_name(parser)) return
          select case(parser%sym)
          case(sym_open,sym_pct,sym_dcolon,sym_dash)
             if(parser%sym==sym_dcolon) then
                call scan(parser)
                if(expect_name(parser)) return
                call make_node(parser,sym_use,2)
             endif
             if(n>1.or.nu>0.or..not.present(is_call)) then
                call parse_error(parser,&
                     'Unexpected symbol in what seems to be a left hand side expression')
                return
             endif
             if(arglist(parser)) return
             call make_node(parser,sym_do,1)
             if(parser%sym==sym_comma) then
                call parse_error(parser,'Cannot follow a call with a comma')
                return
             endif
             is_call=.true.
             iserr=.false.
             return
          case(sym_dot,sym_d1:sym_d7,sym_open_square,sym_at,sym_pling)
             dotcall=.false.
             call make_node(parser,sym_name,1)
             if(qual(parser,dotcall,last_is_method)) return
             if(dotcall) then
                if(n==1.and.present(is_call)) then
                   call make_node(parser,sym_do,1)
                   if(parser%sym==sym_comma) then
                      call parse_error(parser,'Cannot follow a call with a comma')
                      return
                   endif
                   is_call=.true.
                   iserr=.false.
                   return
                else
                   call parse_error(parser,&
                        'Unexpected call in what seems to be left hand side expression')
                   return
                endif
             endif
          case default
             call make_node(parser,sym_name,1)
          end select
       endif

       select case(parser%sym)
       case(sym_plus,sym_minus,sym_mult,sym_and,sym_or,sym_amp,sym_bar,sym_tilde,sym_concat)
          call push_sym_val(parser,parser%sym)
          call make_node(parser,sym_proc,1)
          call make_node(parser,sym_open_brace,2)
          call scan(parser)
          if(present(cannot_be_move)) cannot_be_move=.true.
       case(sym_open_brace)
          call scan(parser)
          if(expr(parser)) return
          if(expect(parser,sym_close_brace)) return
          call make_node(parser,sym_open_brace,2)
          if(present(cannot_be_move)) cannot_be_move=.true.
       end select
       if(parser%sym/=sym_comma) exit
       call scan(parser)
       if(parser%sym/=sym_underscore.and.parser%sym<=num_sym) then
          call push_back(parser,sym_comma)
          exit
       endif
    enddo
    iserr=.false.
  end function lhs

  !======================================================
  ! Assignment/definition: lhs, lhs... "="  rhs
  ! or call with no return values
  !======================================================
  recursive function assn_or_call(parser,call_ok,assign_ok,define_ok) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: call_ok,assign_ok,define_ok
    logical:: iserr
    integer:: n,nu,name,sym
    logical:: dotcall,cannot_be_move,last_is_method
    iserr=.true.

    dotcall=.false.
    cannot_be_move=.false.
    last_is_method=.false.
    if(lhs(parser,n,nu,dotcall,cannot_be_move,last_is_method)) return

    if(dotcall) then
       iserr=.false.
       return
    endif
    
    sym=parser%sym
    if(sym==sym_move.or.sym==sym_move_all.or.sym==sym_swap) then
       if(n/=1) then
          call parse_error(parser,'Cannot have multiple left hand sides before "'//&
               trim(sym_names(sym))//'"')
       elseif(nu>0) then
          call parse_error(parser,'Cannot have "_" as the left hand side of "'//&
               trim(sym_names(sym))//'"')
       elseif(cannot_be_move) then
          call parse_error(parser,'Cannot have operators before "'//&
               trim(sym_names(sym))//'"')
       endif
       call scan(parser)
       if(valref(parser)) return
       call make_node(parser,sym,2)
    elseif(parser%sym==sym_assign.or.n>1.or..not.last_is_method) then
       call make_node(parser,sym_assign,n)
       if(expect(parser,sym_assign)) return
       if(rhs(parser,n)) return
       call make_node(parser,sym_assign,2)
    else
       call make_node(parser,sym_do,1)
    endif
    iserr=.false.
  end function assn_or_call

  !======================================================
  ! Right hand side of defintion or assignment
  !======================================================
  recursive function rhs(parser,n) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: n
    logical:: iserr
    integer:: name
    logical:: dotcall
    iserr=.true.

    if(parser%sym==sym_do) then
       if(do_stmt(parser)) return
    elseif(parser%sym==sym_yield) then
       if(yield(parser,.true.)) return
    elseif(parser%sym==sym_pm_yield) then
       if(yield(parser,.false.)) return
    elseif(n==1) then
       if(expr(parser)) return
    else
       if(check_name(parser,name)) then
          if(parser%sym==sym_open.or.parser%sym==sym_pct.or.parser%sym==sym_dash) then
             if(proccall(parser,name)) return
          else
             call push_name_val(parser,name)
             call make_node(parser,sym_name,1)
             dotcall=.false.
             if(qual(parser,dotcall)) return
             call make_node(parser,sym_assign,1)
          endif
       elseif(parser%sym==sym_number.or.parser%sym==sym_string.or.parser%sym==sym_dcolon) then
          if(term(parser,.false.)) return
          call make_node(parser,sym_assign,1)
       else
          call parse_error(parser,&
               'Expected procedure call, name or constant after multiple left-hand sides')
       endif
    endif
    iserr=.false.
  end function rhs

  !======================================================
  ! Reference to variable (or component of a variable)
  !======================================================
  recursive function valref(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    iserr=.true.
    if(expect_name(parser)) return
    if(parser%sym==sym_dcolon) then
       call scan(parser)
       if(expect_name(parser)) return
       call make_node(parser,sym_use,2)
    else
       call make_node(parser,sym_name,1)
    end if
    if(qual(parser)) return
    iserr=.false.
  end function valref

  !======================================================
  ! name { "," name }
  !======================================================
  function name_list(parser,n) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out):: n
    logical:: iserr
    iserr=.true.
    n=0
    do
       if(expect_name(parser)) return
       n=n+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    iserr=.false.
  end function name_list

  !======================================================
  ! sub-expressions : check exprlist where name = exp ...
  !======================================================
  function subexpr(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    integer:: n,m,sym
    logical:: iserr
    iserr=.true.
    if(parser%sym==sym_check) then
       call scan(parser)
       n=1
       do
          if(expr(parser)) return
          if(parser%sym==sym_cond) then
             call scan(parser)
             if(expr(parser)) return
          else
             call push_null_val(parser)
             call swap_vals(parser)
          endif
          n=n+2
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       call make_node(parser,sym_check,n)
    endif
    if(parser%sym==sym_split) then
       call scan(parser)
       m=0
       do
          if(expect_name(parser)) return
          if(expect(parser,sym_assign)) return
          if(valref(parser)) return
          m=m+2
          if(parser%sym==sym_comma) then
             call scan(parser)
          else
             exit
          endif
       enddo
       call make_node(parser,sym_split,m+1)
    end if
    do while(parser%sym==sym_where)
       call scan(parser)
       m=0
       do
          n=0
          do
             if(parser%sym==sym_underscore) then
                call make_node(parser,sym_underscore,0)
                call scan(parser)
             else
                if(expect_name(parser)) return
             endif
             n=n+1
             if(parser%sym/=sym_comma) exit
             call scan(parser)
          enddo
          if(expect(parser,sym_assign)) return
          call make_node(parser,sym_where,n)
          if(rhs(parser,n)) return
          call make_node(parser,sym_assign,2)
          m=m+1
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       call make_node(parser,sym_where,m)
       call make_node(parser,sym_where,2)
    enddo
    iserr=.false.
  end function  subexpr

  !======================================================
  ! Extended expression (expr subexpr)
  !======================================================
  subroutine xexpr(parser)
    type(parse_state),intent(inout):: parser
    logical iserr
    iserr=expr(parser)
    iserr=subexpr(parser)
  end subroutine xexpr

  !======================================================
  ! Extended expression list ( expr, expr... subexpr)
  !======================================================
  subroutine xexprlist(parser,length,sym)
    type(parse_state),intent(inout):: parser
    integer,intent(out),optional:: length
    integer,intent(in),optional:: sym
    integer:: m
    if(exprlist(parser,m,sym=sym)) return
    if(present(length)) length=m
    if(subexpr(parser)) return
  contains
    include 'fesize.inc'
  end subroutine xexprlist

  !======================================================
  ! While statement
  !======================================================
  recursive function while_stmt(parser) result(is_err)
    type(parse_state),intent(inout):: parser
    logical:: is_err
    integer:: line,pos,sym
    is_err=.true.
    call get_sym_pos(parser,line,pos)
    sym=sym_while
    call scan(parser)
    if(parser%sym==sym_invar) then
       sym=sym_while_invar
       call scan(parser)
    endif
    call xexpr(parser)
    if(block_or_single_stmt(parser,sym_while,0,line)) return
    call make_node_at(parser,sym,2,line,pos)
    is_err=.false.
  end function while_stmt

  !======================================================
  ! Until statement
  !======================================================
  recursive function until_stmt(parser) result(is_err)
    type(parse_state),intent(inout):: parser
    logical:: is_err
    integer:: line,pos,sym
    is_err=.true.
    call get_sym_pos(parser,line,pos)
    sym=sym_until
    call scan(parser)
    if(parser%sym==sym_invar) then
       sym=sym_until_invar
       call scan(parser)
    endif
    call xexpr(parser)
    if(block_or_single_stmt(parser,sym_until,0,line)) return
    call make_node_at(parser,sym,2,line,pos)
    is_err=.false.
  end function until_stmt
  
  !======================================================
  ! do statement
  !======================================================
  recursive function do_stmt(parser) result(is_err)
    type(parse_state),intent(inout):: parser
    logical:: is_err
    integer:: line,pos,sym,lsym,n,base
    is_err=.true.
    call get_sym_pos(parser,line,pos)
    call scan(parser)
    if(parser%sym==sym_colon.or.parser%sym==sym_open_brace) then
       if(block_or_single_stmt(parser,sym_do,0,line)) return
       call make_node_at(parser,sym_do_stmt,1,line,pos)
    else
       base=parser%top
       lsym=sym_list
       call push_sym(parser,7)
       if(parser%sym==sym_amp) then
          call scan(parser)
          if(expect_name(parser)) return
          call push_sym(parser,9)
       elseif(parser%sym==sym_dotdotdot) then
          call push_sym_val(parser,sym_dotdotdot)
          lsym=sym_dotdotdot
       else
          if(expect_name(parser)) return
       endif
       if(parser%sym==sym_comma.or.parser%sym==sym_assign) then
          n=1
          do while(parser%sym==sym_comma)
             if(lsym==sym_dotdotdot) then
                call parse_error(parser,'"," not expected after "..."')
             endif
             call scan(parser)
             n=n+1
             if(parser%sym==sym_amp) then
                call scan(parser)
                if(expect_name(parser)) return
                call push_sym(parser,n+8)
             elseif(parser%sym==sym_dotdotdot) then
                call push_sym_val(parser,sym_dotdotdot)
                call scan(parser)
                lsym=sym_dotdotdot
             else
                if(expect_name(parser)) return
             endif
          enddo
          call make_node(parser,lsym,n)
          if(parser%top>base) then
             call name_vector(parser,base)
          else
             call push_null_val(parser)
          endif
          if(expect(parser,sym_assign)) return
          if(expect_name(parser)) return
       elseif(lsym==sym_dotdotdot.or.parser%top>base) then
          if(expect(parser,sym_assign)) return
       else
          call make_node(parser,sym_list,0)
          call push_null_val(parser)
       endif
       if(arglist(parser,yield=.false.)) return
       if(parser%sym==sym_colon) then
          call scan(parser)
          if(parser%sym==sym_return) then
             call scan(parser)
             call push_null_val(parser)
             call xexprlist(parser)
          else
             call stmt_list(parser,single=.true.)
             call push_null_val(parser)
          endif
       else
          if(expect(parser,sym_open_brace)) return
          call stmt_list(parser)
          if(parser%sym==sym_return) then
             call scan(parser)
             call xexprlist(parser)
          else
             call push_null_val(parser)
          endif
          if(close_block(parser,sym_do,0,line)) return
       endif
       call push_null_val(parser)
       if(subexpr(parser)) return
       call make_node_at(parser,sym_do_stmt,6,line,pos)
    end if
    is_err=.false.
  end function do_stmt

  !======================================================
  ! foreach statement
  !======================================================
  recursive function for_each_stmt(parser,name) result(is_err)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    logical:: is_err
    integer:: sym,var_name,line
    is_err=.true.
    line=get_sym_line(parser)
    sym=sym_each
    call scan(parser)
    if(parser%sym==sym_invar) then
       sym=sym_foreach_invar
       call scan(parser)
    endif
    if(iter(parser,.false.,var_name)) return
    if(parser%sym==sym_while) then
       call scan(parser)
       if(expr(parser)) return
       call make_node(parser,sym_while,1)
    elseif(parser%sym==sym_until) then
       call scan(parser)
       if(expr(parser)) return
       call make_node(parser,sym_until,1)
    else
       call push_null_val(parser)
    endif
    call push_null_val(parser)
    if(subexpr(parser)) return
    if(block_or_single_stmt(parser,name,var_name,line)) return
 
    ! Make for-each node: iter while-until block label 
    call make_node(parser,sym,4)
    is_err=.false.
    return
  end function for_each_stmt

  !======================================================
  ! if statement
  !======================================================
  recursive function if_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: n,sym,line
    iserr=.true.
    n=0
    line=get_sym_line(parser)
    sym=sym_if
    call scan(parser)
    if(parser%sym==sym_invar) then
       sym=sym_if_invar
       call scan(parser)
    endif
    do
       call xexpr(parser)
       if(block_or_single_stmt(parser,sym_if,0,line)) return
       n=n+1
       if(parser%sym/=sym_elseif) exit
       call scan(parser)
    enddo
    if(parser%sym==sym_else) then
       call scan(parser)
       if(block_or_single_stmt(parser,sym_if,0,line)) return
    else
       call push_null_val(parser)
    endif
    do while(n>1)
       call make_node(parser,sym_if,3)
       call make_node(parser,sym_list,1)
       n=n-1
    enddo
    call make_node(parser,sym,3)
    iserr=.false.
  end function if_stmt

  !==============================================================
  ! { (var | let | assign ) { name | _ } [ : type ] , } [ = expr ]
  !==============================================================
  recursive function var_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: n,nu,ntot,m,vsym,mode
    logical:: dotcall,has_dotdotdot
    iserr=.true.
    mode=0
    m=0
    ntot=0
    do
       select case(parser%sym)
       case(sym_var,sym_const)
          vsym=parser%sym
          call scan(parser)
       case(sym_invar,sym_nhd,sym_chan,sym_shared)
          mode=parser%sym
          call scan(parser)
          if(expect(parser,sym_var)) return
          vsym=sym_var
       case(sym_assignment)
          call scan(parser)
          if(lhs(parser,n,nu)) return
          call make_node(parser,sym_assign,n)
          goto 10
       case default
          call parse_error(parser,'Expected "var", "let" or "assign"')
          return
       end select
       n=0
       nu=0
       has_dotdotdot=.false.
       do
          if(parser%sym==sym_underscore) then
             call scan(parser)
             call make_node(parser,sym_underscore,0)
             nu=nu+1
          elseif(parser%sym==sym_dotdotdot) then
             call scan(parser)
             if(expect_name(parser)) return
             call make_node(parser,sym_dotdotdot,1)
             has_dotdotdot=.true.
          else
             if(expect_name(parser)) return
          endif
          n=n+1
          if(parser%sym==sym_comma) then
             call scan(parser)
             if(parser%sym/=sym_underscore.and.parser%sym<=num_sym) then
                call push_back(parser,sym_comma)
                exit
             endif
          else
             exit
          endif
       enddo
       if(mode/=0) then
          call push_sym_val(parser,mode)
       else
          call push_null_val(parser)
       endif
       if(parser%sym==sym_colon) then
          if(has_dotdotdot) then
             call parse_error(parser,&
                  'Cannot give a new type to an object being initialised with "..."')
          endif
          call scan(parser)
          if(typ(parser)) return
       else
          call push_null_val(parser)
       endif
       call make_node(parser,vsym,n+2)
       if(nu==n.and.vsym/=sym_assignment) then
          call parse_error(parser,&
               'A "'//trim(sym_names(vsym))//&
               '" clause must define at least one object')
       endif
10     continue
       m=m+1
       ntot=ntot+n
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    if(m>1) call make_node(parser,sym_assign_list,m)

    if(parser%sym/=sym_dotdotdot) then
       if(expect(parser,sym_assign)) return
       if(rhs(parser,ntot)) return
       call make_node(parser,sym_assign,2)
       if(subexpr(parser)) return
    else
       if(m>1) then
          call parse_error(parser,'Cannot have multiple left hand side elements before "..."')
       elseif(mode/=0) then
          call parse_error(parser,'"'//trim(sym_names(mode))//' var" must have an initialiser')
       elseif(nu>0) then
          call parse_error(parser,'Cannot have "_" in unitialised "'//&
               trim(sym_names(vsym))//'" declaration')
       endif
       call scan(parser)
    endif
    iserr=.false.
  end function var_stmt


  !==========================================================
  ! switch [ xexpr ] { case xexprlist : statement_list ... }
  !==========================================================
  recursive function switch_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: n,line,sym
    iserr=.true.
    sym=sym_switch
    line=get_sym_line(parser)
    call scan(parser)
    if(parser%sym==sym_invar) then
       call scan(parser)
       sym=sym_switch_invar
    endif
    if(parser%sym/=sym_open_brace) then
       call xexpr(parser)
       if(expect(parser,sym_open_brace)) return
    else
       call make_node(parser,sym_true,0)
       call scan(parser)
    endif
    n=0
    do while(parser%sym==sym_case)
       call scan(parser)
       call xexprlist(parser,sym=sym_case)
       if(expect(parser,sym_colon)) return
       call stmt_list(parser)
       n=n+2
    enddo
    if(n==0) then
       call parse_error(parser,'No "case" clauses in "switch" statement')
       return
    endif
    if(parser%sym==sym_default) then
       call scan(parser)
       if(expect(parser,sym_colon)) return
       call stmt_list(parser)
    else
       call push_null_val(parser)
    endif
    call make_node(parser,sym_switch,n+2)
    if(close_block(parser,sym_switch,0,line)) return
    iserr=.false.
  end function switch_stmt

  !==============================================================================
  ! any name [ = expr ] ( : stmt |  { stmts }  | { case typelist : stmts ... } )
  !==============================================================================
  recursive function any_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: name,line,n,sym
    iserr=.true.
    line=get_sym_line(parser)
    call scan(parser)
    sym=sym_any
    if(parser%sym==sym_invar) then
       call scan(parser)
       sym=sym_any_invar
    endif
    if(expect_and_get_name(parser,name)) return
    call make_node(parser,sym_name,1)
    if(parser%sym==sym_assign) then
       call scan(parser)
       call xexpr(parser)
    else
       call push_null_val(parser)
    endif
    if(parser%sym==sym_open_brace) then
       call scan(parser)
       if(parser%sym==sym_case) then
          call make_node(parser,sym_true,0)
          n=0
          do while(parser%sym==sym_case)
             call scan(parser)
             call push_sym_val(parser,name)
             call make_node(parser,sym_name,1)
             if(typ(parser)) return
             call make_node(parser,sym_is,2)
             call make_node(parser,sym_list,1)
             if(expect(parser,sym_colon)) return
             call stmt_list(parser)
             n=n+2
          enddo
          if(parser%sym==sym_default) then
             call scan(parser)
             if(expect(parser,sym_colon)) return
             call stmt_list(parser)
          else
             call push_null_val(parser)
          endif
          call make_node(parser,sym_switch,n+2)
          call make_node(parser,sym_list,1)
          if(expect(parser,sym_close_brace)) return
       else
          call push_back(parser,sym_open_brace)
          if(block_or_single_stmt(parser,sym_any,name,line)) return
       endif
    else
       if(block_or_single_stmt(parser,sym_any,name,line)) return
    endif
    call make_node(parser,sym,3)
    iserr=.false.
  end function any_stmt

  !====================================================================
  ! [ for | forall ] iter [ << attrs >> ] { statements }
  !====================================================================
  recursive function for_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: sym,name,line
    iserr=.true.
    line=get_sym_line(parser)
    sym=parser%sym
    call scan(parser)
    if(iter(parser,.true.,name)) return
    if(parser%sym==sym_open_attr) then
       if(par_attr(parser,sym_distr,sym_block,sym)) return
    else
       call push_null_val(parser)
    endif
    call push_null_val(parser)
    if(subexpr(parser)) return
    if(block_or_single_stmt(parser,sym,name,line)) return
    ! attr iter block
    call make_node(parser,sym,4)
    iserr=.false.
  end function for_stmt

  !====================================================================================
  ! par [ << attrs >> ] { statements ( task name [ << attrs >> ] : statements ...) }
  !====================================================================================
  recursive function par_stmt(parser) result(is_error)
    type(parse_state),intent(inout):: parser
    logical:: is_error
    integer:: i,k,n,name,base,line
    logical:: has_work
    type(pm_ptr):: p,using
    is_error=.true.
    has_work=.false.
    base=parser%vtop
    line=get_sym_line(parser)
    call scan(parser)
    if(parser%sym==sym_open_attr) then
       if(par_attr(parser,sym_distr,sym_work)) return
    else
       call push_null_val(parser)
    endif
    call push_null_val(parser)
    if(subexpr(parser)) return
    if(expect(parser,sym_open_brace)) return
    call stmt_list(parser)
    k=3
    n=0
    if(expect(parser,sym_task)) return
    do
       if(expect_name(parser)) return
       call make_node(parser,sym_name,1)
       if(parser%sym==sym_open_attr) then
          call scan(parser)
          if(expect(parser,sym_work)) return
          if(expect(parser,sym_assign)) return
          call xexpr(parser)
          if(expect(parser,sym_close_attr)) return
          has_work=.true.
       else
          call make_node(parser,sym_par,0)
       endif
       if(expect(parser,sym_colon)) return
       call stmt_list(parser)
       k=k+3
       n=n+1
       if(parser%sym/=sym_task) exit
       call scan(parser)
    enddo
    if(close_block(parser,sym_par,0,line)) return
    if(has_work) then
       do i=base+5,base+k,3
          call push_val(parser,parser%vstack(i))
       enddo
       call make_node(parser,sym_list,n)
       call push_num_val(parser,n)
       call push_num_val(parser,1)
       call push_num_val(parser,0)
       call push_num_val(parser,0)
       call make_node(parser,sym_array_former,5)
       using=parser%vstack(base+1)
       if(pm_fast_isnull(using)) then
          do i=sym_distr,sym_block
             call make_node(parser,sym_null,0)
          enddo
          call make_node(parser,sym_list,sym_block-sym_distr+1)
          using=pop_val(parser)
          parser%vstack(base+1)=using
       elseif(parser%error_count==0) then
          p=node_arg(using,sym_work-sym_distr+1)
          if(node_sym(p)/=sym_null) then
             call parse_error(parser,&
                  'Cannot have "work=" at both start'//&
                  ' of "par" statement and in "task :" clauses')
             return
          endif
       endif
       if(parser%error_count==0) then
          call pm_ptr_assign(parser%context,using,&
               int(node_args+sym_work-sym_distr,pm_ln),top_val(parser))
       endif
    endif
    if(parser%error_count>0) then
       parser%vtop=base
       is_error=.false.
       return
    endif
    call push_val(parser,parser%vstack(base+1))
    call push_val(parser,parser%vstack(base+2))
    call push_val(parser,parser%vstack(base+3))
    do i=4,k,3
       call push_val(parser,parser%vstack(base+i))
       call push_val(parser,parser%vstack(base+i+2))
    enddo
    call make_node(parser,sym_task,n*2+1)
    call make_node(parser,sym_list,1)
    call make_node(parser,sym_par,3)
    k=parser%vtop
    parser%vtop=base+1
    parser%vstack(parser%vtop)=parser%vstack(k)
    is_error=.false.
  contains
    include 'fisnull.inc'
    include 'fesize.inc'
  end function par_stmt

  !======================================================
  ! Iteratator clause ( name in expr ...)
  !======================================================
  recursive function iter(parser,star_ok,first_name) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: star_ok
    integer,intent(out),optional:: first_name
    logical:: iserr
    integer:: i,m,name
    logical:: star,amp
    iserr=.true.
    m=0
    do
       star=.false.
       amp=.false.
       if(parser%sym==sym_mult.and.star_ok) then
          call scan(parser)
          star=.true.
       elseif(parser%sym==sym_amp) then
          call scan(parser)
          amp=.true.
       endif
       if(check_name(parser,name)) then
          call push_name_val(parser,name)
          if(present(first_name).and.m==0) first_name=name
          if(expect(parser,sym_in)) return
          if(star) then
             call make_node(parser,sym_mult,1)
          elseif(amp) then
             call make_node(parser,sym_amp,1)
          endif
          if(expr(parser)) return
          m=m+1
       else
          if(expect_name(parser)) return
       endif
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    call make_node(parser,sym_iter,m*2)
    iserr=.false.
  end function iter

  !======================================================
  ! Attributes for "for" or "par" statement
  !======================================================
  recursive function par_attr(parser,start,finish,sym) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: start,finish
    integer,intent(inout),optional:: sym
    logical:: iserr
    integer:: i,base,vbase,name,high
    iserr=.true.
    base=parser%top
    vbase=parser%vtop
    high=max(sym_block,finish)
    do while(parser%sym==sym_open_attr)
       call scan(parser)
       do while(parser%sym>num_sym)
          call push_sym(parser,parser%sym)
          call scan(parser)
          if(expect(parser,sym_assign)) return
          if(expr(parser)) return
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       if(expect(parser,sym_close_attr)) return
    enddo
    if(parser%vtop>vbase) then
       call make_node(parser,sym_list,parser%vtop-vbase)
       call name_vector(parser,base)
       call make_node(parser,sym_open_attr,2)
    else
       call push_null_val(parser)
    endif
    iserr=.false.
  contains
    include 'fisnull.inc'
  end function par_attr

  !======================================================
  ! test [ expr ] [ block ]
  !======================================================
  recursive function test_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: n,line
    iserr=.true.
    line=get_sym_line(parser)
    call scan(parser)
    if(parser%sym==sym_open_brace.or.parser%sym==sym_colon) then
       call push_null_val(parser)
       if(block_or_single_stmt(parser,sym_test,0,line)) return
    else
       call push_null_val(parser)
       n=1
       do
          if(expr(parser)) return
          if(parser%sym==sym_cond) then
             call scan(parser)
             if(expr(parser)) return
          else
             call push_null_val(parser)
             call swap_vals(parser)
          endif
          n=n+2
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       call make_node(parser,sym_test,n)
       if(subexpr(parser)) return
       if(parser%sym==sym_open_brace.or.parser%sym==sym_colon) then
          if(block_or_single_stmt(parser,sym_test,0,line)) return
       else
          call push_null_val(parser)
       endif
    endif
    call make_node(parser,sym_test,2)
    iserr=.false.
  end function test_stmt

  !======================================================
  ! over expr [ attr ] [ subexp ] block
  !======================================================
  function over_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: line
    iserr=.true.
    line=get_sym_line(parser)
    call scan(parser)
    if(expr(parser)) return
    if(par_attr(parser,sym_block,sym_block)) return
    call push_null_val(parser)
    if(subexpr(parser)) return
    if(block_or_single_stmt(parser,sym_over,0,line)) return
    call make_node(parser,sym_over,4)
    iserr=.false.
  end function over_stmt

  !======================================================
  ! sync ( [ while ] name)  block | assignment | call 
  !======================================================
  function sync_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: line,name,name2,sym,n,nu
    logical:: is_call,is_assign,is_labelled
    iserr=.true.
    line=get_sym_line(parser)
    call scan(parser)
    sym=sym_sync
    if(parser%sym==sym_open) then
       call scan(parser)
       if(parser%sym==sym_while) then
          call scan(parser)
          sym=sym_sync_while
       endif
       if(expect_and_get_name(parser,name)) return
       if(expect(parser,sym_close)) return
       is_labelled=.true.
    else
       call push_null_val(parser)
       is_labelled=.false.
    endif
    if(parser%sym==sym_colon.or.parser%sym==sym_open_brace) then
       if(.not.is_labelled) then
          call parse_error(parser,&
               'A "sync" statement that is not of the form "sync(...)"'//&
               ' cannot be applied to a block of statements') 
       endif
       if(block_or_single_stmt(parser,sym_sync,name,line)) return
       call make_node(parser,sym,2)
    else
       if(parser%sym==sym_dollar) then
          call scan(parser)
          if(op(parser,name2,.true.,.false.)) return
          call push_sym_val(parser,name2)
          call make_node(parser,sym_proc,1)
          if(parser%sym==sym_dot) call scan(parser)
          if(arglist(parser)) return
          call make_node(parser,sym_open,1)
       else
          if(parser%sym==sym_assignment) then
             call scan(parser)
             is_assign=.true.
          else
             is_assign=.false.
          endif
          is_call=.false.
          if(lhs(parser,n,nu,is_call)) return
          if(is_call) then
             if(is_assign) then
                call parse_error(parser,&
                     'Left hand side of "sync assign" appears to contain a procedure call')
                return
             else
                call make_node(parser,sym_open,1)
             endif
          elseif(n>1) then
             call parse_error(parser,'"sync" assignment can only have one left hand side')
             return
          elseif(nu>1) then
             call parse_error(parser,'Left hand side of "sync" assignment cannot be "_"')
             return
          else
             if(expect(parser,sym_assign)) return
             if(expr(parser)) return
             call make_node(parser,sym_assign,2)
          endif
       endif
       call push_null_val(parser)
       if(subexpr(parser)) return
       call make_node(parser,sym,3)
    endif
    iserr=.false.
  end function sync_stmt

  !======================================================
  ! List of statements
  !======================================================
  recursive subroutine stmt_list(parser,single,num_to_include)
    type(parse_state),intent(inout):: parser
    logical,intent(in),optional:: single
    integer,intent(in),optional:: num_to_include
    logical:: ok
    integer:: i,n,m,k,name,sym,label,line,pos
    type(pm_ptr):: p
    k=0
    if(present(num_to_include)) k=num_to_include
    do
       sym=parser%sym
       select case(sym)

          ! These statements are only used internally by the compiler
       case(sym_pm_send:sym_pm_serve)
          if(send_stmt()) goto 999
       case(sym_pm_bcast)
          if(bcast_stmt()) goto 999
       case(sym_pm_recv_req)
          if(recv_req_stmt()) goto 999
       case(sym_pm_recv_assn)
          if(recv_assn_stmt()) goto 999
       case(sym_pm_do,sym_pm_do_at)
          if(pm_do_stmt()) goto 999
       case(sym_pm_head_node)
          if(head_node_stmt()) goto 999
       case(sym_pm_each_index)
          if(each_index_stmt()) goto 999
       case(sym_pm_for,sym_pm_foreach,sym_pm_over)
          if(pm_for_stmt()) goto 999
       case(sym_pm_context)
          if(pm_context_stmt()) goto 999
       case(sym_pm_shared,sym_pm_shared_always,sym_pm_chan,sym_pm_chan_always)
          call scan(parser)
          if(expect(parser,sym_open_brace)) goto 999
          call stmt_list(parser)
          if(expect(parser,sym_close_brace)) goto 999
          call make_node(parser,sym,1)
       case(sym_pm_set_dotdotdot)
          call scan(parser)
          if(expect(parser,sym_open)) goto 999
          if(expr(parser)) goto 999
          if(expect(parser,sym_close)) goto 999
          call make_node(parser,sym_pm_set_dotdotdot,1)

          ! Pragma's -- start with $$
       case(sym_ddollar)
          if(pragma()) goto 999
          
          ! Statements that are actually part of the language
       case(sym_if)
          if(if_stmt(parser)) goto 999
       case(sym_switch)
          if(switch_stmt(parser)) goto 999
       case(sym_while)
          if(while_stmt(parser)) goto 999
       case(sym_until)
          if(until_stmt(parser)) goto 999
       case(sym_do)
          if(do_stmt(parser)) goto 999
       case(sym_test)
          if(test_stmt(parser)) goto 999
       case(sym_for,sym_forall)
          if(for_stmt(parser)) goto 999
       case(sym_each)
          if(for_each_stmt(parser,0)) goto 999
       case(sym_par)
          if(par_stmt(parser)) goto 999
       case(sym_any)
          if(any_stmt(parser)) goto 999
       case(sym_over)
          if(over_stmt(parser)) goto 999
       case(sym_underscore)
          if(assn_or_call(parser,.false.,.true.,.true.)) goto 999
          if(subexpr(parser)) goto 999
       case(sym_var,sym_const,sym_assignment,sym_invar,sym_chan,sym_nhd,sym_shared)
          if(var_stmt(parser)) goto 999
          if(subexpr(parser)) goto 999
       case(sym_dollar)
          if(proc_val_call()) goto 999
       case(sym_sync)
           if(sync_stmt(parser)) goto 999
       case(sym_return)
          call make_node(parser,sym_list,k)
          return
       case(sym_yield,sym_pm_yield)
          if(yield(parser,sym==sym_yield)) goto 999
          call make_node(parser,sym_yield,1)
       case default
          if(parser%sym>num_sym) then
             if(assn_or_call(parser,.true.,.true.,.true.)) goto 999
             if(subexpr(parser)) goto 999
          else
             if(parser%sym>0.and.parser%sym/=sym_close_brace&
                  .and.parser%sym<=last_decl) then
                call parse_error(parser,'Expected start of statement')
                goto 999
             else
                exit
             endif
          endif
       end select
       k=k+1
       if(present(single)) exit
       if(parser%sym==sym_semi) then
          call scan(parser)
       else if(.not.parser%atstart) then
          if(parser%sym>0.and.parser%sym/=sym_close_brace.and.&
               parser%sym<=last_decl) then
             call parse_error(parser,'Expected end of statement')
             goto 999
          else
             exit
          endif
       endif
       cycle
999    continue
       call skip_past_error(parser,.false.)
       if(present(single)) exit
    enddo
    call make_node(parser,sym_list,k)

  contains

    ! $op(args) or $op.(args)
    function proc_val_call() result(iserr)
      logical:: iserr
      integer:: name
      iserr=.true.
      call scan(parser)
      if(op(parser,name,.true.,.false.)) return
      call push_sym_val(parser,name)
      call make_node(parser,sym_proc,1)
      if(parser%sym==sym_dot) call scan(parser)
      if(arglist(parser)) return
      call make_node(parser,sym_do,1)
      if(subexpr(parser)) return
      iserr=.false.
    end function  proc_val_call

     ! Pragma: $$ name [ '(' exprlist ')' ]
     function pragma() result(iserr)
       logical:: iserr
       integer:: m
       iserr=.true.
       call scan(parser)
       if(expect_name(parser)) return
       m=0
       if(parser%sym==sym_open) then
          call scan(parser)
          if(exprlist(parser,m,nolist=.true.)) return
          if(expect(parser,sym_close)) return
       endif
       call make_node(parser,sym_ddollar,m+1)
       iserr=.false.
     end function pragma
     
     ! *****************************************************************
     ! The following statements are for **internal** compiler use only:
     !******************************************************************
     
     ! PM__recv prc_out,dref_out,val_out,dref_in,prc,at,expr
     !  - send dref_in to dref_out on prc,
     !    execute expr on prc and send result back to val_out
     ! PM__send prc_out,dref_out,val_out,dref_in,prc,val_in,at { stmts }
     !  - send dref_in, val_in to dref_out,val_out on prc
     !    and execute stmts on prc
     recursive function send_stmt() result(iserr)
       logical:: iserr
       integer:: sym
       iserr=.true.
       sym=parser%sym
       call scan(parser)
       if(expect_name(parser)) return
       if(expect(parser,sym_comma)) return
       if(expect_name(parser)) return
       if(expect(parser,sym_comma)) return
       if(expect_name(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       if(sym==sym_pm_send.or.sym==sym_pm_collect) then
          if(expect(parser,sym_comma)) return
          if(expr(parser)) return
          if(expect(parser,sym_open_brace)) return
          call stmt_list(parser)
          if(expect(parser,sym_close_brace)) return
          call make_node(parser,sym,8)
       else
          if(expect(parser,sym_comma)) return
          if(expr(parser)) return
          call make_node(parser,sym,7)
       endif
       iserr=.false.
     end function send_stmt

     ! PM__bcast x_out,y_out,x_in,y_in,prc { stmts }
     ! - broadcast x_in, y_in from prc and execute stmts on all procs
     recursive function bcast_stmt() result(iserr)
       logical:: iserr
       iserr=.true.
       call scan(parser)
       if(expect_name(parser)) return
       if(expect(parser,sym_comma)) return
       if(expect_name(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       if(expect(parser,sym_open_brace)) return
       call stmt_list(parser)
       if(expect(parser,sym_close_brace)) return
       call make_node(parser,sym_pm_bcast,6)
       iserr=.false.
     end function bcast_stmt

     ! PM__recv_req prc_out,dref_out,dref_local_template,expr
     ! - Receive a request and send back reply
     ! - Request sent by _isend_req% / _send_slice_req() 
     recursive function recv_req_stmt() result(iserr)
       logical:: iserr
       iserr=.true.
       call scan(parser)
       if(expect_name(parser)) return
       if(expect(parser,sym_comma)) return
       if(expect_name(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       call make_node(parser,sym_pm_recv_req,4)
       iserr=.false.
     end function recv_req_stmt

     ! PM__recv_assn prc_out,dref_out,val_out,local_dref_template,local_val_template { stmts }
     ! - Receive an assignment request and execute statements
     ! - Request sent by _isend_assn% / _send_slice_assn()
     recursive function recv_assn_stmt() result(iserr)
       logical:: iserr
       integer:: sym
       iserr=.true.
       sym=parser%sym
       call scan(parser)
       if(expect_name(parser)) return
       if(expect(parser,sym_comma)) return
       if(expect_name(parser)) return
       if(expect(parser,sym_comma)) return
       if(expect_name(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       if(expect(parser,sym_open_brace)) return
       call stmt_list(parser)
       if(expect(parser,sym_close_brace)) return
       call make_node(parser,sym_pm_recv_assn,7)
       iserr=.false.
     end function recv_assn_stmt

     ! PM__do locations { stmts }
     recursive function pm_do_stmt() result(iserr)
       logical:: iserr
       integer:: n,sym
       iserr=.true.
       sym=parser%sym
       call scan(parser)
       if(sym==sym_pm_do) then
          if(expect_name(parser)) return
       else
          if(expr(parser)) return
       endif
       if(expect(parser,sym_comma)) return
       if(expr(parser)) return
       n=0
       do while(parser%sym==sym_comma)
          call scan(parser)
          if(expect_name(parser)) return
          if(expect(parser,sym_comma)) return
          if(expr(parser)) return
          n=n+2
       enddo
       if(expect(parser,sym_open_brace)) return
       call stmt_list(parser)
       if(expect(parser,sym_close_brace)) return
       call make_node(parser,sym,n+3)
       iserr=.false.
     end function pm_do_stmt

     ! PM__head_node { stmts }
     recursive function head_node_stmt() result(iserr)
       logical:: iserr
       iserr=.true.
       call scan(parser)
       if(expect(parser,sym_open_brace)) return
       call stmt_list(parser)
       if(expect(parser,sym_close_brace)) return
       call make_node(parser,sym_pm_head_node,1)
       iserr=.false.
     end function head_node_stmt

     function each_index_stmt() result(iserr)
       logical:: iserr
       iserr=.true.
       call scan(parser)
       if(expect_name(parser)) return
       if(expect(parser,sym_in)) return
       if(expr(parser)) return
       if(expect(parser,sym_open_brace)) return
       call stmt_list(parser)
       if(expect(parser,sym_close_brace)) return
       call make_node(parser,sym_pm_each_index,3)
       iserr=.false.
     end function each_index_stmt

     ! PM__for PM__foreach PM__over 
     function pm_for_stmt() result(iserr)
       logical:: iserr
       integer:: sym
       iserr=.true.
       sym=parser%sym
       call scan(parser)
       call xexpr(parser)
       if(expect(parser,sym_open_brace)) return
       call stmt_list(parser)
       if(expect(parser,sym_close_brace)) return
       call make_node(parser,sym,2)
       iserr=.false.
     end function pm_for_stmt

     function pm_context_stmt() result(iserr)
       logical:: iserr
       integer:: i
       iserr=.true.
       call scan(parser)
       do i=1,num_comm_args
          if(i>1) then
             if(expect(parser,sym_comma)) return
          endif
          if(expect_name(parser)) return
       enddo
       if(expect(parser,sym_open_brace)) return
       call stmt_list(parser)
       if(expect(parser,sym_close_brace)) return
       call make_node(parser,sym_pm_context,num_comm_args+1)
       iserr=.false.
     end function pm_context_stmt
     
  end subroutine stmt_list

  !======================================================
  ! :statement | { statement list }
  !======================================================
  recursive function block_or_single_stmt(parser,name1,name2,line) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name1,name2,line
    logical:: iserr
    iserr=.true.
    if(parser%sym==sym_colon) then
       call scan(parser)
       call stmt_list(parser,single=.true.)
    else
       if(expect(parser,sym_open_brace)) return
       call stmt_list(parser)
       if(close_block(parser,name1,name2,line)) return
    endif
    iserr=.false.
  end function block_or_single_stmt

  !======================================================
  ! Close a block using "}" or "}--name" or "}--keyword"
  ! name/keyword must match name1 or name2
  !======================================================
  function close_block(parser,name1,name2,line) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name1,name2,line
    logical:: iserr
    integer:: name
    character(len=10):: linestr
    iserr=.true.
    if(expect(parser,sym_close_brace)) return
    if(parser%sym==sym_minus) then
       call scan(parser)
       if(expect(parser,sym_minus)) return
       name=parser%sym
       if(name/=name1.and.name/=name2) then
          if((name1>num_sym.or.name1==0).and.name2>num_sym) then
             ! Special treatment for "foreach"
             ! in this case name1 and name2 are label and index variable
             ! and "for each" is implied
             if(name/=sym_each) then
                write(linestr,'(I6)') line
                if(name1>0) then
                   call parse_error(parser,'"}--'//&
                        trim(pm_name_as_string(parser%context,name))//&
                        '" does not match "'//&
                        trim(pm_name_as_string(parser%context,name1))//&
                        ': foreach '//&
                        trim(pm_name_as_string(parser%context,name2))//&
                        '" on line '//trim(adjustl(linestr)))
                else
                   call parse_error(parser,'"}--'//&
                     trim(pm_name_as_string(parser%context,name))//&
                     '" does not match "foreach '//&
                     trim(pm_name_as_string(parser%context,&
                     name2))//'" on line '//trim(adjustl(linestr)))
                endif
                return
             else
                call scan(parser)
                if(parser%sym/=sym_each) then
                   write(linestr,'(I6)') line
                   call parse_error(parser,'"}--foreach" does not match "foreach" at line '//&
                        trim(adjustl(linestr)))
                endif
             endif
          else
             write(linestr,'(I6)') line
             if(name1>0.and.name2>0) then
                call parse_error(parser,'"}--'//&
                     trim(pm_name_as_string(parser%context,name))//&
                     '" does not match "'//&
                     trim(pm_name_as_string(parser%context,name1))//&
                     merge(':',' ',name1>num_sym)//&
                     trim(pm_name_as_string(parser%context,name2))//&
                     '" on line '//trim(adjustl(linestr)))
             else
                call parse_error(parser,'"}--'//&
                     trim(pm_name_as_string(parser%context,name))//&
                     '" does not match "'//&
                     trim(pm_name_as_string(parser%context,&
                     max(name1,name2)))//&
                     '" on line '//trim(adjustl(linestr)))
             endif
             return
          endif
       endif
       call scan(parser)
       if(parser%sym==sym_else.or.parser%sym==sym_elseif) then
          call parse_error(parser,'Cannot have "'//&
               trim(sym_names(parser%sym))//'" after "}--"')
          return
       endif
    endif
    iserr=.false.
  end function close_block

  !======================================================
  ! [ mode ] type
  ! Mode only accepted in modes_ok is true
  !======================================================
  recursive function moded_typ(parser,modes_ok,type_needed) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: modes_ok,type_needed
    logical:: iserr
    integer:: sym,msym
    iserr=.true.
    sym=parser%sym
    if(sym>=first_mode.and.sym<=last_mode) then
       if(.not.modes_ok) then
          call parse_error(parser,'Cannot have "'//&
               trim(sym_names(sym))//&
               '" on a parameter in a non-communicating procedure')
       endif
       msym=sym
       call scan(parser)
       if(.not.type_needed .and.(parser%sym==sym_comma.or.&
            parser%sym==sym_close.or.&
            parser%sym==sym_assign.or.&
            parser%sym==sym_open_attr)) then
          call push_null_val(parser)
       else
          if(typ(parser)) return
       endif
       call push_sym_val(parser,msym)
       call make_node(parser,sym_mode,2)
    elseif(sym==sym_idx.and.modes_ok) then
       if(typ(parser)) return
       call push_sym_val(parser,sym_invar)
       call make_node(parser,sym_mode,2)
    else
       if(typ(parser)) return
    endif
    iserr=.false.
  end function moded_typ

  !======================================================
  ! type [ except type ]
  !======================================================
  recursive function typ(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    iserr=.true.
    if(typunion(parser)) return
    if(parser%sym==sym_except) then
       call scan(parser)
       if(typunion(parser)) return
       call make_node(parser,sym_except,2)
    endif
    iserr=.false.
  end function typ

  !======================================================
  ! type { or type }
  !======================================================
  recursive function typunion(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: n
    iserr=.true.
    if(typconj(parser)) return
    n=1
    do while(parser%sym==sym_or)
       call scan(parser)
       if(typconj(parser)) return
       n=n+1
    enddo
    if(n>1) call make_node(parser,sym_or,n)
    iserr=.false.
  end function typunion

  !======================================================
  ! type { and type }
  !======================================================
  recursive function typconj(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: n
    iserr=.true.
    if(typinc(parser)) return
    n=1
    do while(parser%sym==sym_and)
       call scan(parser)
       if(typinc(parser)) return
       n=n+1
    enddo
    if(n>1) call make_node(parser,sym_and,n)
    iserr=.false.
  end function typconj

  !======================================================
  ! type [ inc type ]
  !======================================================
  recursive function typinc(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    iserr=.true.
    if(typunary(parser)) return
    if(parser%sym==sym_includes) then
       call scan(parser)
       if(typunary(parser)) return
       call make_node(parser,sym_includes,2)
    endif
    iserr=.false.
  end function typinc

  !======================================================
  ! * type | . type | inc type
  !======================================================
  recursive function typunary(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    iserr=.true.
    if(parser%sym==sym_dot) then
       call scan(parser)
       if(typunary(parser)) return
       call make_node(parser,sym_casts_to,1)
    elseif(parser%sym==sym_mult) then
       call scan(parser)
       if(typunary(parser)) return
       call make_node(parser,sym_pval,1)
    elseif(parser%sym==sym_includes) then
       call scan(parser)
       call make_node(parser,sym_any,0)
       if(typunary(parser)) return
       call make_node(parser,sym_includes,2)
    else
       if(typval(parser)) return
    endif
    iserr=.false.
  end function typunary

  !======================================================
  ! Type value
  !======================================================
  recursive function typval(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: m,name,name2,i,base,vbase,sym,tag,line,pos
    logical:: varg
    iserr=.true.
    sym=parser%sym
    select case(sym)
    case(sym_open)
       call scan(parser)
       if(typ(parser)) return
       if(expect(parser,sym_close)) return
    case(sym_open_square)
       call scan(parser)
       if(typ_list(parser,m)) return
       if(m>7) then
          call parse_error(parser,&
               'tuple "[]" type cannot have more that 7 arguments')
       endif
       call push_sym_val(parser,sym_dim1+m-1)
       call make_node(parser,sym_type,m+1)
       if(expect(parser,sym_close_square)) return
    case(sym_pm_list)
       call make_node(parser,sym_pm_list,0)
       call scan(parser)
    case(sym_lt)
       call scan(parser)
       if(typ(parser)) return
       call make_node(parser,sym_type_val,1)
       if(expect(parser,sym_gt)) return
    case(sym_any)
       call scan(parser)
       call make_node(parser,sym_any,0)
    case(sym_number)
       call push_num_val(parser,parser%lexval)
       call make_node(parser,sym_number,1)
       call scan(parser)
       call make_node(parser,sym_literal,1)
    case(sym_fix,sym_literal)
       call scan(parser)
       if(sym==sym_fix.and.parser%sym==sym_open_square) then
          call scan(parser)
          m=0
          do
             if(expect(parser,sym_number)) return
             call push_num_val(parser,parser%lexval)
             call make_node(parser,sym_number,1)
             call make_node(parser,sym_fix,1)
             if(parser%sym==sym_dotdot) then
                call scan(parser)
                if(expect(parser,sym_number)) return
                call push_num_val(parser,parser%lexval)
                call make_node(parser,sym_number,1)
                call make_node(parser,sym_fix,1)
                call push_sym_val(parser,sym_range)
                call make_node(parser,sym_type,3)
             endif
             m=m+1
             if(parser%sym/=sym_comma) exit
             call scan(parser)
          enddo
          call push_sym_val(parser,sym_dim1+m-1)
          call make_node(parser,sym_type,m+1)
          if(expect(parser,sym_close_square)) return
       elseif(sym==sym_literal.and.parser%sym/=sym_open) then
          call make_node(parser,sym_any,0)
          call make_node(parser,sym_literal,1)
       else
          if(parser%sym==sym_open) then
             call scan(parser)
             if(parser%sym==sym_number) then
                call push_num_val(parser,parser%lexval)
                call make_node(parser,sym_number,1)
                call scan(parser)
                call make_node(parser,sym,1)
             elseif(parser%sym==sym_true.or.parser%sym==sym_false) then
                call make_node(parser,parser%sym,0)
                call scan(parser)
                call make_node(parser,sym,1)
             elseif(parser%sym==sym_string) then
                call push_num_val(parser,parser%lexval)
                call make_node(parser,sym_string,1)
                call scan(parser)
                call make_node(parser,sym,1)
             else
                if(typ(parser)) return
                call make_node(parser,sym,1)
             endif
             if(expect(parser,sym_close)) return
          else
             call push_null_val(parser)
             call make_node(parser,sym,1)
          endif
       endif
    case(sym_dollar)
       call scan(parser)
       if(op(parser,name,.true.,.true.)) return
       call push_sym_val(parser,name)
       if(parser%sym==sym_dcolon.and.name>num_sym) then
          call scan(parser)
          if(expect_name(parser)) return
          call make_node(parser,sym_proc,2)
       else
          call make_node(parser,sym_proc,1)
       endif
    case(sym_proc)
       call scan(parser)
       if(parser%sym==sym_open.or.parser%sym==sym_pct.or.parser%sym==sym_dash) then
          if(proctyp(parser)) return
       else
          call push_sym_val(parser,sym_proc)
          call make_node(parser,sym_type,1)
       endif
    case(sym_contains)
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(typ(parser)) return
       call make_node(parser,sym_contains,1)
       if(expect(parser,sym_close)) return
    case(sym_pm_dref:sym_pm_dref_any_slice)
       call scan(parser)
       m=sym_pm_dref-sym-1
       call push_num_val(parser,m)
       if(expect(parser,sym_open)) return
       if(opt_typ_list(parser,m)) return
       if(expect(parser,sym_close)) return
       call make_node(parser,sym_pm_dref,m+1)
    case(sym_assign_or_init)
       call make_node(parser,sym_assign_or_init,0)
       call scan(parser)
    case(sym_dcaret)
       call scan(parser)
       if(parser%sym==sym_caret) then
          call scan(parser)
          if(typval(parser)) return
          call make_node(parser,sym_const,1)
       elseif(parser%sym==sym_dcaret) then
          call scan(parser)
          call make_node(parser,sym_assign_or_init,0)
       else
          if(expect(parser,sym_open)) return
          if(typ(parser)) return
          if(expect(parser,sym_close)) return
          call make_node(parser,sym_dcaret,1)
       endif
    case(sym_underscore)
       call scan(parser)
       call make_node(parser,sym_underscore,0)
    case default
       if(.not.check_name_pos(parser,name,line,pos)) then
          if(parser%sym/=sym_null) then
             call parse_error(parser,'Expected type')
             return
          else
             name=parser%sym
             call get_sym_pos(parser,line,pos)
             call scan(parser)
          endif
       endif
       if(parser%sym==sym_dcolon) then
          call scan(parser)
          if(.not.check_name(parser,name2)) then
             call parse_error(parser,'Expected name')
             return
          endif
       else
          name2=0
       endif
       if(parser%sym==sym_open) then
          call scan(parser)
          if(opt_typ_list(parser,m)) return
          call push_sym_val(parser,name)
          if(name2/=0) then
             call push_sym_val(parser,name2)
             call make_node(parser,sym_use,2)
          endif
          call make_node_at(parser,sym_type,m+1,line,pos)
          if(expect(parser,sym_close)) return
       else
          call push_sym_val(parser,name)
          if(name2/=0) then
             call push_sym_val(parser,name2)
             call make_node(parser,sym_use,2)
          endif
          call make_node_at(parser,sym_type,1,line,pos)
       endif
    end select
    if(parser%sym==sym_caret) then
       call scan(parser)
       if(parser%sym==sym_var.or.parser%sym==sym_const.or.&
            parser%sym==sym_invar.or.parser%sym==sym_fix) then
          call push_sym_val(parser,parser%sym)
          call scan(parser)
       else
          call push_sym_val(parser,0)
       endif
       if(typval(parser)) return
       call make_node(parser,sym_caret,3)
    end if
    iserr=.false.

  contains
    include 'fname.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'
  end function typval

  !====================================================================
  ! proc ( args... ) -> (type,type...)
  !====================================================================
  recursive function proctyp(parser,yield) result(iserr)
    type(parse_state):: parser
    logical,intent(in),optional:: yield
    logical:: iserr
    integer:: i,base,n,m,sym,npar
    logical:: iscomm
    iserr=.true.

    base=parser%top
    
    call push_null_val(parser)
    sym=parser%sym
    if(sym==sym_pct.or.sym==sym_dash.or.sym==sym_dot) then
       iscomm=.true.
       call push_sym_val(parser,sym)
       call scan(parser)
       do i=1,num_comm_args
          call make_node(parser,sym_any,0)
       enddo
       m=num_comm_args
    else
       call push_sym_val(parser,sym_proc)
       call make_node(parser,sym_any,0)
       m=1
    endif

    if(present(yield)) then
       call make_node(parser,sym_any,0)
       call push_sym(parser,m+1)
       call make_node(parser,sym_any,0)
       m=m+2
   endif
   
   if(expect(parser,sym_open)) return
   
   if(parser%sym/=sym_close) then
      do 
          sym=parser%sym
          m=m+1
          if(sym==sym_amp) then
             call scan(parser)
             call push_sym(parser,m)
             if(moded_typ(parser,iscomm,.false.)) return
          elseif(sym==sym_var.or.sym==sym_const) then
             call scan(parser)
             if(moded_typ(parser,iscomm,.false.)) return
             call make_node(parser,sym,1)
          else
             if(moded_typ(parser,iscomm,.false.)) return
          endif
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       if(parser%sym==sym_dotdotdot) then
          call scan(parser)
          call make_node(parser,sym_dotdotdot,m)
       else
          call make_node(parser,sym_list,m)
        endif
    else
       call make_node(parser,sym_list,m)
    endif
    
    if(expect(parser,sym_close)) return
    if(parser%sym==sym_arrow) then
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(moded_typ_list(parser,iscomm,m)) return
       call make_node(parser,sym_list,m)
       if(expect(parser,sym_close)) return
    else
       call push_null_val(parser)
    endif
    
    if(parser%top>base) then
       call name_vector(parser,base)
    else
       call push_null_val(parser)
    endif

    if(.not.present(yield).and.parser%sym==sym_yield) then
       call scan(parser)
       call push_back(parser,sym_dash)
       if(proctyp(parser,yield=.true.)) return
    else
       call push_null_val(parser)
    endif
    
    call make_node(parser,sym_proc,6)
    iserr=.false.
  end function proctyp

  !======================================================
  ! Simple comma-separated type list (no blank entries)
  !======================================================
  recursive function typ_list(parser,m) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out):: m
    logical:: iserr
    iserr=.true.
    m=0
    do
       if(parser%sym==sym_dotdotdot) then
          if(m>0) call push_back(parser,sym_comma)
          iserr=.false.
          return
       endif
       if(typ(parser)) return
       m=m+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    iserr=.false.
  end function  typ_list

  !==============================================================
  ! Simple comma-separated mode&type list (no blank entries)
  !==============================================================
  recursive function moded_typ_list(parser,modes_ok,m) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out):: m
    logical,intent(in):: modes_ok
    logical:: iserr
    iserr=.true.
    m=0
    do
       if(moded_typ(parser,modes_ok,.true.)) return
       m=m+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    iserr=.false.
  end function  moded_typ_list

  !======================================================
  ! Comma separated list of types
  ! any of which may be omitted
  !======================================================
  recursive function opt_typ_list(parser,m) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out):: m
    logical:: iserr
    iserr=.true.
    m=0
    do
       if(parser%sym==sym_comma.or.&
            parser%sym==sym_close_brace.or.&
            parser%sym==sym_close_square.or.&
            parser%sym==sym_close) then
          call push_null_val(parser)
       else
          if(typ(parser)) return
       endif
       m=m+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    iserr=.false.
  end function  opt_typ_list


   !======================================================
  ! Comma separated list of types
  ! any of which may be omitted
  !======================================================
  recursive function opt_moded_typ_list(parser,m,varg) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out):: m
    logical,intent(out),optional:: varg
    logical:: iserr
    iserr=.true.
    m=0
    do
       if(parser%sym==sym_comma.or.&
            parser%sym==sym_dotdotdot) then
          call push_null_val(parser)
       else
          if(moded_typ(parser,.true.,.false.)) return
       endif
       m=m+1
       if(present(varg).and.parser%sym==sym_dotdotdot) then
          call scan(parser)
          varg=.true.
          exit
       endif
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    iserr=.false.
  end function  opt_moded_typ_list
  
  !======================================================
  ! Parameter list for procedure declaration
  !======================================================
  recursive function param_list(parser,iscomm,dot_name,dot_type,close) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: iscomm
    type(pm_ptr),intent(in):: dot_name,dot_type
    integer,intent(in):: close
    logical:: iserr
    integer:: m,n,i,base,last,vbase,sym,msym,name,numloop
    type(pm_ptr):: temp,dom
    base=parser%top
    iserr=.true.
    m=1
    n=0

    ! All procedure calls implicitly pass topology
    call push_sym_val(parser,sym_topology)
    if(parser%sym==sym_topology) then
       call scan(parser)
       if(expect(parser,sym_colon)) return
       if(typ(parser)) return
       if(parser%sym/=close) then
          if(expect(parser,sym_comma)) return
       endif
    else
       call push_null_val(parser)
    endif

    ! For communicating procedures implicit "region" and "subregion" parameters
    if(iscomm) then
       call push_sym_val(parser,sym_outer)
       if(parser%sym==sym_outer) then
          call scan(parser)
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
          if(parser%sym/=close) then
             if(expect(parser,sym_comma)) return
          endif
       else
          call push_null_val(parser)
       endif
       call push_sym_val(parser,sym_region)
       if(parser%sym==sym_region) then
          call scan(parser)
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
          if(parser%sym/=close) then
             if(expect(parser,sym_comma)) return
          endif
       else
          call push_null_val(parser)
       endif
       call push_sym_val(parser,sym_subregion)
       if(parser%sym==sym_subregion) then
          call scan(parser)
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
          if(parser%sym/=close) then
             if(expect(parser,sym_comma)) return
          endif
       else
          call push_null_val(parser)
       endif
       ! Assuming for the momenent that here_in_tile has same type as here
       call push_sym_val(parser,sym_here_in_tile)
       if(parser%sym==sym_here) then
          call scan(parser)
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
          if(parser%sym/=close) then
             if(expect(parser,sym_comma)) return
          endif
       else
          call push_null_val(parser)
       endif
       call push_sym_val(parser,sym_mask)
       if(parser%sym==sym_mask) then
          call scan(parser)
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
          if(parser%sym/=close) then
             if(expect(parser,sym_comma)) return
          endif
       else
          call push_null_val(parser)
       endif
       m=num_comm_args
    endif

    if(.not.pm_fast_isnull(dot_name)) then
       call push_val(parser,dot_name)
       call push_val(parser,dot_type)
       m=m+1
    endif
    
    ! Empty argument list
    if(parser%sym==close) then
       call make_node(parser,sym_list,m*2)
       call push_null_val(parser)
       call push_null_val(parser)
       call push_null_val(parser)
       call scan(parser)
       iserr=.false.
       return
    endif

    ! Standard (non-keyword) arguments
    do
       if(parser%sym==sym_dotdotdot) then
          call scan(parser)
          call push_sym_val(parser,sym_dotdotdot)
          if(arg_typ_with_mode(iscomm)) return
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
          call push_sym(parser,m)
          if(arg_typ_with_mode(iscomm)) return
       elseif(parser%sym==sym_var.or.parser%sym==sym_const) then
          msym=parser%sym
          call scan(parser)
          if(expect_name(parser)) return
          if(parser%sym==sym_colon) then
             call scan(parser)
             if(arg_typ_with_mode(iscomm)) return
          else
             call push_null_val(parser)
          endif
          call make_node(parser,msym,1)
       else
          if(check_name(parser,name)) then
             if(parser%sym==sym_assign) then
                call make_node(parser,sym_list,m*2)
                call push_sym_val(parser,name)
                call scan(parser)
                call push_null_val(parser)
                if(expr(parser)) return
                n=1
                exit
             else
                call push_sym_val(parser,name)
             endif
          else
             call parse_error(parser,'Expected argument')
          endif
          if(arg_typ_with_mode(iscomm)) return
          if(parser%sym==sym_assign) then
             parser%temp=pop_val(parser)
             call drop_val(parser)
             call make_node(parser,sym_list,m*2)
             call push_sym_val(parser,name)
             call push_val(parser,parser%temp)
             call scan(parser)
             if(expr(parser)) return
             n=1
             exit
          else
             m=m+1
          endif
       endif
       if(parser%sym/=sym_comma) then
          call make_node(parser,sym_list,m*2)
          exit
       endif
       call scan(parser)
    enddo

    ! Keyword arguments
    if(parser%sym==sym_comma) then
       do
          call scan(parser)
          if(expect_name(parser, &
               'optional argument name')) return
          if(arg_typ_with_mode(iscomm)) return
          if(expect(parser,sym_assign)) return
          if(expr(parser)) return
          n=n+1
          if(parser%sym/=sym_comma) then
             call make_node(parser,sym_list,n*3)
             exit
          endif
       enddo
    else
       if(n>0) then
          if(.not.pm_fast_isnull(dot_name)) then
             call parse_error(parser,'A method cannot have keyword arguments')
          endif
          call make_node(parser,sym_list,n*3)
       else
          call push_null_val(parser)
       endif
    endif

    if(parser%top>base) then
       call name_vector(parser,base)
    else
       call push_null_val(parser)
    endif
    
    if(parser%sym==sym_when) then
       call scan(parser)
       call xexpr(parser)
    else
       call push_null_val(parser)
    endif
    
    if(expect(parser,close)) return
 
    iserr=.false.
    return
  contains

    include 'fisnull.inc'
    
    function arg_typ_with_mode(iscomm) result(iserr)
      logical,intent(in):: iscomm
      logical:: iserr
      iserr=.true.
      if(parser%sym==sym_colon) then
         call scan(parser)
         if(moded_typ(parser,iscomm,.false.)) return
      else
         call push_null_val(parser)
      endif
      iserr=.false.
    end function arg_typ_with_mode

  end function param_list


  !======================================================
  ! Procedure/call attributes
  !======================================================
  recursive function proc_attr(parser,iscall,flags) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: iscall
    integer,intent(inout):: flags
    logical:: iserr
    integer:: m
    iserr=.true.
    call scan(parser)
    do
       select case(parser%sym) 
       case(sym_inline)
          call set_flags(proccall_is_inline)
          call scan(parser)
       case(sym_no_inline)
          call set_flags(proccall_is_no_inline)
          call scan(parser)
       case(sym_always)
          call set_flags(proc_run_always)
          call scan(parser)
       end select
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    if(iand(flags,proccall_is_inline+proccall_is_no_inline)==&
         proccall_is_inline+proccall_is_no_inline) then
       call parse_error(parser,&
            'Cannot have both "<<inline>>" and "<<no_inline>>" attributes together')
    endif
    if(expect(parser,sym_close_attr)) return
    iserr=.false.
  contains
    subroutine set_flags(new_flags)
      integer,intent(in):: new_flags
      if(iand(flags,new_flags)/=0) then
         call parse_error(parser,&
              'Cannot repeat attribute "'//trim(sym_names(parser%sym))//'"')
      endif
      flags=ior(flags,new_flags)
    end subroutine set_flags
  end function proc_attr
  
  !======================================================
  ! Procedure declaration
  !======================================================
  function proc_decl(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    type(pm_ptr),target::ptr,dom,dparams,rtypes,dot_name,dot_type
    type(pm_ptr):: p,params,link
    type(pm_reg),pointer:: reg
    integer:: name,callname,this,thispar
    integer:: nret,base,flags,sbase,scount,m,nreduce,sym
    integer:: line,pos,nerrors,open,close
    logical:: ampargs,iscall,iscomm,isref,isshared,islocal,ischan,have_rtn
    nerrors=parser%error_count
    reg=>pm_register(parser%context,'proc',ptr,dom,dparams,rtypes,dot_name,dot_type)
    iserr=.true.
    sym=sym_proc
    nret=0
    flags=0
    sbase=parser%vtop
    scount=parser%error_count
    dom=pm_null_obj
    dparams=pm_null_obj
    thispar=-1
    open=sym_open
    close=sym_close
    
    ! Line and position of procedure start
    call get_sym_pos(parser,line,pos)
    call scan(parser)
   
    iscomm=.false.
    isref=.false.
    if(parser%sym==sym_open) then
       ! Reference procedure proc (name:type).name(...)
       call scan(parser)
       if(parser%sym==sym_amp) then
          flags=ior(flags,proccall_is_lhs)
          call scan(parser)
       endif
       if(expect_name(parser)) goto 999
       dot_name=pop_val(parser)
       if(parser%sym==sym_colon) then
          call scan(parser)
          if(moded_typ(parser,.true.,.false.)) goto 999
          dot_type=pop_val(parser)
       else
          dot_type=pm_null_obj
       endif
       if(expect(parser,sym_close)) goto 999
       if(parser%sym==sym_open_square) then
          name=sym_sub
          open=sym_open_square
          close=sym_close_square
       else
          if(expect(parser,sym_dot)) goto 999
          if(expect_name(parser)) goto 999
          name=pop_num_val(parser)
       endif
       flags=ior(flags,proccall_is_ref+proccall_is_comm+proccall_is_general)
       iscomm=.true.
       isref=.true.
    else
       
       ! Procedure name
       if(.not.check_name(parser,name)) then
          if(.not.isref) then
             if(op(parser,name,.false.,.false.)) goto 999
          endif
       endif

       dot_name=pm_null_obj
       dot_type=pm_null_obj
       
    endif
    
    ! Communicating proc flags
    if(.not.isref) then
       if(parser%sym==sym_pct) then
          call scan(parser)
          flags=ior(flags,proccall_is_comm)
          iscomm=.true.
       elseif(parser%sym==sym_dash) then
          call scan(parser)
          flags=ior(flags,proccall_is_comm+proccall_is_general)
          iscomm=.true.
       endif
    endif

    ! Start of parameters
    if(expect(parser,open)) goto 999

10  continue

    ! Create fully qualified (module!name) procedure name
    call make_qualified_name(parser,name)

    ! Start of procedure delaration node
    base=parser%vtop
    call push_val(parser,top_val(parser)) ! name

    ! Link procedure into list for given name (if already exists)
    ptr=decl_entry(parser,name,modl_proc,link)
    call push_val(parser,link)

    ! Push some more entries in the procedure node (some get values later)
    call push_val(parser,parser%modl)
    call push_num_val(parser,-12345)      ! flags
    if(param_list(parser,iscomm,dot_name,dot_type,close)) goto 999

    params=parser%vstack(parser%vtop-2)
    call push_num_val(parser,-777)        ! coded returns
    call push_num_val(parser,-777)        ! coded type
    call push_num_val(parser,-777)        ! nret

    ! Return types ->(typelist)
    if(parser%sym==sym_arrow) then
       call scan(parser)
       if(expect(parser,sym_open)) goto 999
       if(parser%sym==sym_close) then
          nret=0
       else
          if(moded_typ_list(parser,iscomm,nret)) goto 999
          if(expect(parser,sym_close)) return
       endif
       call make_node(parser,sym_list,nret)
       rtypes=top_val(parser)
       have_rtn=.true.
    else
       have_rtn=.false.
       call push_null_val(parser)
       nret=-1
    endif

    if(parser%sym==sym_yield) then
       if(yield_clause()) goto 999
    endif

    if(iscomm) then

       if(parser%sym==sym_uncond) then
          flags=ior(flags,proc_is_uncond)
          call scan(parser)
       elseif(parser%sym==sym_cond) then
          flags=ior(flags,proc_is_cond)
          call scan(parser)
       endif

       if(parser%sym==sym_global) then
          flags=ior(flags,proc_run_shared)
          call scan(parser)
       elseif(parser%sym==sym_pm_shared) then
          flags=ior(flags,proc_run_local)
          call scan(parser)
       elseif(parser%sym==sym_complete) then
          if(iand(flags,proc_is_uncond)/=0) then
             call parse_error(parser,'Cannot combine "cplt" and "uncond"')
          endif
          flags=ior(flags,proc_run_complete)
          call scan(parser)
       endif

    endif

    ! Attributes
    if(parser%sym==sym_open_attr) then
       if(proc_attr(parser,.false.,flags)) goto 999
       call push_null_val(parser)
    else
       call push_null_val(parser)
    endif

    ! ... flags extensibility beyond module
    if(parser%sym==sym_dotdotdot) then
       call scan(parser)
       flags=ior(flags,proc_is_open)
    endif

    
    ! = expr or  [ check expr ] block
    if(parser%sym==sym_assign.and.nret==-1) then

       call push_null_val(parser)
       call scan(parser)

       m=0
       do
          if(iand(flags,proccall_is_lhs)/=0) then
             if(m>0) then
                call parse_error(parser,&
                     'Cannot return more than one value from reference "." procedure')
                if(expr(parser)) goto 999
             else
                if(parser%sym>=sym_pm_dref.and.parser%sym<=sym_pm_ref) then
                   if(term(parser,.false.)) goto 999
                else
                   if(valref(parser)) goto 999
                endif
             endif
          else
             if(expr(parser)) goto 999
          endif
          call push_null_val(parser)
          m=m+2
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       call make_node(parser,sym_result,m)
       nret=m/2
       if(subexpr(parser)) goto 999
       if(parser%sym==sym_colon.or.parser%sym==sym_open_brace) then
          if(block_or_single_stmt(parser,sym_proc,name,line)) goto 999
       else
          call push_null_val(parser)
       endif
    else
       if(parser%sym==sym_check) then
          call push_null_val(parser)
          if(subexpr(parser)) goto 999
          call push_null_val(parser)
       else
          call push_null_val(parser)
          call push_null_val(parser)
       endif
       if(parser%sym==sym_colon) then
          call scan(parser)
          if(parser%sym==sym_return) then
             call make_node(parser,sym_list,0)
             if(return_stmt()) goto 999
          else
             call stmt_list(parser,single=.true.)
             if(nret>0) then
                call parse_error(parser,&
                     "Missing return statement")
             endif
             nret=0
          endif
       elseif(parser%sym==sym_open_brace) then
          call scan(parser)
          call stmt_list(parser)
          if(parser%sym==sym_return) then
             if(return_stmt()) goto 999
          else
             if(nret>0) then
                call parse_error(parser,&
                     "Missing return statement")
             endif
             nret=0
          endif
          if(close_block(parser,sym_proc,name,line)) goto 999
       else
          if(iand(flags,proc_is_open)/=0.and.have_rtn) then
             call push_null_val(parser)
             flags=ior(flags,proc_is_abstract)
          else
             call parse_error(parser,&
                  'Expecting a block of statements "{...}" or ":..."') 
          endif
       endif
    endif
    call push_null_val(parser) ! Code tree
    
    if(parser%error_count>0) then
       parser%vtop=sbase
       if(parser%error_count==scount) iserr=.false.
       goto 999
    endif


    ! Assign flags to proc_flags slot
    parser%vstack(parser%vtop-&
         proc_num_args-node_args+proc_flags+1)%offset=flags

    ! Assign number of returns to proc_numret slot
    parser%vstack(parser%vtop-&
         proc_num_args-node_args+proc_numret+1)%offset=nret

    if(pm_debug_checks) then
       if(parser%vtop-base/=proc_num_args) then
          write(*,*) '=========',parser%vtop-base,proc_num_args
          do flags=base+1,parser%vtop
             call dump_parse_tree(parser%context,6,parser%vstack(flags),2)
             write(*,*) '==='
          enddo
          write(*,*) parser%vtop,base,parser%vtop-base,proc_num_args
          call parse_error(parser,'Here')
          call pm_panic('parse proc')
       endif
    endif

    call make_node_at(parser,sym,proc_num_args,line,pos)

    if(debug_parser) then
       write(*,*) 'PROC DECL>----------------'
       call dump_parse_tree(parser%context,44,top_val(parser),2)
       write(*,*) 'PROC-DECL----------------'
    endif

    call add_proc_decl(parser,name,ptr)

    iserr=.false.
999 continue
    call pm_delete_register(parser%context,reg)
    return
  contains
    include 'fisnull.inc'
    include 'fesize.inc'

    function yield_clause() result(iserr)
      logical:: iserr
      integer:: m,n,i,k,base,first
      type(pm_ptr):: params,amps
      iserr=.true.
      
      if(parser%error_count>0) then
         call scan(parser)
         if(proctyp(parser,yield=.true.)) return
         iserr=.false.
         return
      endif
      
      if(iand(flags,proccall_is_ref)/=0) then
         call parse_error(parser,'Cannot have a "yield" clause in a method')
      elseif(iand(flags,proccall_is_comm)/=0) then
         first=num_comm_args+1
      else
         first=2
      endif
      params=parser%vstack(parser%vtop-7)
      n=node_numargs(params)/2
      amps=parser%vstack(parser%vtop-5)
      do i=1,first-1
         call push_val(parser,node_arg(params,i*2-1))
         call push_val(parser,node_arg(params,i*2))
      enddo
      base=parser%top
      m=first
      call push_sym_val(parser,sym_block_proc)
      call scan(parser)
      call push_back(parser,sym_dash)
      if(proctyp(parser,yield=.true.)) return
      m=m+1
      call push_sym_val(parser,sym_block_inouts)
      call push_null_val(parser)
      call push_sym(parser,m)
      m=m+1
      call push_sym_val(parser,sym_block_ins)
      call push_null_val(parser)
      m=m+1
      if(pm_fast_isnull(amps))then
         do i=first,n
            call push_val(parser,node_arg(params,i*2-1))
            call push_val(parser,node_arg(params,i*2))
         enddo
      else
         amps=pm_name_val(parser%context,int(amps%offset))
         k=0
         do i=first,n
            call push_val(parser,node_arg(params,i*2-1))
            call push_val(parser,node_arg(params,i*2))
            if(amps%data%i(amps%offset+k)==i) then
               call push_sym(parser,m)
               k=min(k+1,pm_fast_esize(amps))
            endif
            m=m+1
         enddo
      endif
      call make_node(parser,node_sym(params),n*2+num_comm_args)
      parser%vstack(parser%vtop-8)=top_val(parser)
      call drop_val(parser)

      call name_vector(parser,base)
      parser%vstack(parser%vtop-6)=top_val(parser)
      call drop_val(parser)

      iserr=.false.
    end function yield_clause

    function return_stmt() result(iserr)
      logical:: iserr
      integer:: m
      iserr=.true.
      call scan(parser)
      m=0
      do
         if(iand(flags,proccall_is_lhs)/=0) then
            if(m>0) then
               call parse_error(parser,&
                    'Cannot return more than one value from reference "." procedure')
               if(expr(parser)) return
            else
               if(parser%sym>=sym_pm_dref.and.parser%sym<=sym_pm_ref) then
                  if(term(parser,.false.)) return
               else
                  if(valref(parser)) return
               endif
            endif
         else
            if(expr(parser)) return
         endif
         m=m+2
         if(nret>=m/2.and..not.pm_fast_isnull(rtypes)) then
            call push_val(parser,node_arg(rtypes,m/2))
         else
            call push_null_val(parser)
         endif
         if(parser%sym/=sym_comma) exit
         call scan(parser)
      enddo
      call make_node(parser,sym_result,m)
      if(subexpr(parser)) return
      if(nret>0.and.nret>m/2) then
         call parse_error(parser,&
              "Different number of return values and return types")
      endif
      nret=m/2
      parser%vstack(parser%vtop-2)=parser%vstack(parser%vtop)
      parser%vtop=parser%vtop-1
      iserr=.false.
    end function  return_stmt

  end function proc_decl

  !======================================================
  ! Procedure signature (...)->... used for builtin procs
  !======================================================
  recursive function proc_sig(parser,iscomm) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: iscomm
    logical:: iserr
    integer:: m,n,base,name,sym
    type(pm_ptr):: temp
    base=parser%top
    iserr=.true.
    m=1
    n=0
    call push_sym_val(parser,sym_topology)
    if(parser%sym==sym_topology) then
       call scan(parser)
       if(expect(parser,sym_colon)) return
       if(typ(parser)) return
    else
       call push_null_val(parser)
    endif
    if(iscomm) then
       call push_sym_val(parser,sym_outer)
       m=m+1
       if(parser%sym==sym_outer) then
          call scan(parser)
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
       else
          call push_null_val(parser)
       endif
       call push_sym_val(parser,sym_region)
       m=m+1
       if(parser%sym==sym_topology) then
          call scan(parser)
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
       else
          call push_null_val(parser)
       endif
       call push_sym_val(parser,sym_subregion)
       m=m+1
       if(parser%sym==sym_subregion) then
          call scan(parser)
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
       else
          call push_null_val(parser)
       endif
       call push_sym_val(parser,sym_here_in_tile)
       m=m+1
       if(parser%sym==sym_here_in_tile) then
          call scan(parser)
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
       else
          call push_null_val(parser)
       endif
       call push_sym_val(parser,sym_mask)
       m=m+1
       if(parser%sym==sym_mask) then
          call scan(parser)
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
       else
          call push_null_val(parser)
       endif
    endif
    if(parser%sym==sym_close) then
       call make_node(parser,sym_list,2)
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
                call push_back_name(parser,name)
                call push_null_val(parser)
             endif
          else if(parser%sym==sym_dotdotdot) then
             call push_sym_val(parser,sym_dotdotdot)
             call scan(parser)
             if(expect(parser,sym_colon)) return
             if(moded_typ(parser,.true.,.false.)) return
             call make_node(parser,sym_dotdotdot,m*2)
             exit
          else
             call push_null_val(parser)
          endif

          if(moded_typ(parser,.true.,.false.)) return

          if(parser%sym==sym_dotdotdot) then
             call scan(parser)
             call make_node(parser,sym_dotdotdot,m*2)
             exit
          endif
          if(parser%sym/=sym_comma) then
             call make_node(parser,sym_list,m*2)
             exit
          endif
          call scan(parser)
       enddo
       do while(parser%sym==sym_comma)
          call scan(parser)
          if(typ(parser)) return
          if(expect_name(parser,&
               'optional parameter name')) return
          if(expect(parser,sym_assign)) return
          n=n+1
       enddo
       if(n>0) then
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
    call push_num_val(parser,-1) ! Coded params
    call push_num_val(parser,-1) ! Coded returns
    call push_num_val(parser,-1) ! Coded type

    ! Special forms of return type which compute it based on arguments
    if(parser%sym==sym_arrow) then
       call scan(parser)
       if(expect(parser,sym_open)) return
       sym=parser%sym
       select case(sym)
       case(sym_gt,sym_dim,sym_vdim,sym_invar_dim,sym_fix_dim,&
            sym_eq,sym_over,sym_pling,sym_includes)
          ! These return single type based on types of a
          ! list of expressions
          call scan(parser)
          if(exprlist(parser,m)) return
          parser%temp=pop_val(parser)
          call push_num_val(parser,1)
          call push_null_val(parser)
          call push_val(parser,parser%temp)
          call make_node(parser,sym,1)
       case(sym_pct,sym_assign,sym_dot,sym_query,sym_amp,&
            sym_hash,sym_caret,sym_dcaret,sym_d1:sym_d7,sym_invar,sym_shared,&
            sym_type,sym_tilde,sym_bar)
          ! These return N types based on types of a
          ! list of N expressions
          call scan(parser)
          if(parser%sym==sym_pct.and.sym>=sym_d1.and.sym<=sym_d7) then
             call scan(parser)
             sym=sym-sym_d1+sym_dim1
          endif
          if(exprlist(parser,m)) return
          parser%temp=pop_val(parser)
          call push_num_val(parser,m)
          call push_null_val(parser)
          call push_val(parser,parser%temp)
          call make_node(parser,sym,1)
       case(sym_dcolon)
          call scan(parser)
          if(typ_list(parser,m)) return
          call make_node(parser,sym_result,m)
          call make_node(parser,sym_dcolon,1)
          call push_num_val(parser,m)
          call swap_vals(parser)
          call push_null_val(parser)
       case default
          if(typ_list(parser,m)) return
          call make_node(parser,sym_result,m)
          call push_num_val(parser,m)
          call swap_vals(parser)
          call push_null_val(parser)
       end select
       if(expect(parser,sym_close)) return
    else
       call push_num_val(parser,0)
       call push_null_val(parser)
       call push_null_val(parser)
    endif
    iserr=.false.
    return
  end function proc_sig

  function builtin_flags(parser,flags) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out):: flags
    logical:: iserr
    iserr=.true.
    flags=0
    if(parser%sym==sym_open_attr) then
       call scan(parser)
       do
          select case(parser%sym)
          case(sym_proc_is_generator)
             flags=ior(flags,proc_is_generator)
          case(sym_proc_is_impure)
             flags=ior(flags,proc_is_impure)
          case(sym_proc_has_for)
             flags=ior(flags,proc_has_for)
          case(sym_proc_is_dcomm)
             flags=ior(flags,proc_is_dcomm)
          case(sym_proc_is_file)
             flags=ior(flags,proc_is_file)
          case(sym_proc_is_not_inlinable)
             flags=ior(flags,proc_is_not_inlinable)
          case(sym_proc_needs_type)
             flags=ior(flags,proc_needs_type)
          case default
             call parse_error(parser,'Bad PM__intrinsic attribute')
             return
          end select
          call scan(parser)
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       if(expect(parser,sym_close_attr)) return
    endif
    iserr=.false.
  end function builtin_flags

  !======================================================
  ! Intrinsic procedure definition
  !======================================================
  function intrinsic(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    type(pm_ptr),target:: ptr
    type(pm_ptr)::p,link
    type(pm_reg),pointer:: reg
    integer:: name,sym,opcode,opcode2,flags
    reg=>pm_register(parser%context,'builtin',ptr)
    iserr=.true.

    call scan(parser)
    if(builtin_flags(parser,flags)) goto 999

    if(parser%sym==sym_dot) then
       call scan(parser)
       flags=ior(flags,proccall_is_comm+proccall_is_general+proccall_is_ref)
    endif
    
    if(.not.check_name(parser,name)) then
       if(op(parser,name,.false.,.false.)) goto 999
       call push_sym_val(parser,name)
    else
       call push_sym_val(parser,name)
    endif

    if(iand(flags,proccall_is_ref)==0) then
       if(parser%sym==sym_pct) then
          call scan(parser)
          flags=ior(flags,proccall_is_comm)
       elseif(parser%sym==sym_dash) then
          call scan(parser)
          flags=ior(flags,proccall_is_comm+proccall_is_general)
       endif
    endif

    ! Create full name: module!name
    call push_sym(parser,&
         -get_modl_name(parser%modl))
    ptr=top_val(parser)
    name=ptr%offset
    call push_sym(parser,name)
    call name_vector(parser,parser%top-2)

    ! Link into list of delarations for this name
    ptr=decl_entry(parser,int(ptr%offset),modl_proc,link)
    call push_val(parser,link)
    call push_val(parser,parser%modl)  ! module
    call push_num_val(parser,flags)    ! flags
    if(expect(parser,sym_open)) goto 999
    if(proc_sig(parser,iand(flags,proccall_is_comm)/=0)) return

    if(expect(parser,sym_colon)) goto 999
    if(parser%sym/=sym_string) then
       call parse_error(parser,'Expected string operation name in "PM__intrinsic"')
       goto 999
    endif
    p=pm_dict_lookup(parser%context,parser%op_names,&
         pm_type_val(parser%context,&
         parser%lexval))
    if(pm_fast_isnull(p)) then
       call parse_error(parser,'Bad intrinsic operation'//&
            pm_value_as_string(parser%context,pm_type_val(parser%context,parser%lexval)))
       goto 999
    endif
    call scan(parser)
    opcode=p%offset
    
    if(parser%sym==sym_open) then
       call scan(parser)
       if(expect(parser,sym_number)) goto 999
       p=pm_type_val(parser%context,parser%lexval)
       opcode2=p%data%ln(p%offset)
       if(expect(parser,sym_close)) goto 999
    elseif(parser%sym>=first_mode.and.parser%sym<=last_mode) then
       opcode2=parser%sym
       call scan(parser)
    else
       opcode2=0
    endif
    
    call push_num_val(parser,int(opcode))
    call push_num_val(parser,int(opcode2))
    call push_val(parser,pm_null_obj)
    call push_null_val(parser)
    call make_node(parser,sym_builtin,sysproc_num_args)
    if(debug_parser) then
       write(*,*) 'BUILTIN DECL>----------------'
       call dump_parse_tree(parser%context,6,top_val(parser),2)
       write(*,*) 'BI-DECL-------------'
    endif
    call add_proc_decl(parser,name,ptr)
    iserr=.false.
999 call pm_delete_register(parser%context,reg)
  contains
    include 'fisnull.inc'
  end function intrinsic


  function builtin(parser,opcode,opcode2,pdata,pflags) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: opcode
    integer,intent(in):: opcode2
    type(pm_ptr),intent(in):: pdata
    integer,intent(in):: pflags
    logical:: iserr
    type(pm_ptr),target:: ptr
    type(pm_ptr)::p,link
    type(pm_reg),pointer:: reg
    integer:: name,sym,flags
    reg=>pm_register(parser%context,'builtin',ptr)
    iserr=.true.
    call scan(parser)
10  continue
    if(.not.check_name(parser,name)) then
       if(op(parser,name,.false.,.false.)) goto 999
       call push_sym_val(parser,name)
    else
       call push_sym_val(parser,name)
    endif
    if(parser%sym==sym_pct) then
       call scan(parser)
       flags=ior(pflags,proccall_is_comm)
    else
       flags=pflags
    endif

    ! Create full name: module!name
    call push_sym(parser,&
         -get_modl_name(parser%modl))
    ptr=top_val(parser)
    name=ptr%offset
    call push_sym(parser,name)
    call name_vector(parser,parser%top-2)

    ! Link into list of delarations for this name
    ptr=decl_entry(parser,int(ptr%offset),modl_proc,link)
    call push_val(parser,link)
    
    call push_val(parser,parser%modl)  ! module
    call push_num_val(parser,flags)    ! flags
    if(expect(parser,sym_open)) goto 999
    if(proc_sig(parser,iand(flags,proccall_is_comm)/=0)) return        
    call push_num_val(parser,int(opcode))
    call push_num_val(parser,int(opcode2))
    call push_val(parser,pdata)
    call push_null_val(parser)
    call make_node(parser,sym_builtin,sysproc_num_args)
    if(debug_parser) then
       write(*,*) 'BUILTIN DECL>----------------'
       call dump_parse_tree(parser%context,6,top_val(parser),2)
       write(*,*) 'BI-DECL-------------'
    endif
    call add_proc_decl(parser,name,ptr)
    iserr=.false.
999 call pm_delete_register(parser%context,reg)
  contains
    include 'fisnull.inc'
  end function builtin


  !==============================================================
  ! Add top of stack as declaration of procedure name
  ! Stack must contain <name> <procedure decl> as top 2 entries
  !==============================================================
  subroutine add_proc_decl(parser,name,ptr)
    type(parse_state):: parser
    integer,intent(in):: name
    type(pm_ptr),intent(in):: ptr
    if(pm_fast_isnull(ptr)) then
       call push_val(parser,top_val(parser))
       call push_null_val(parser)
       call make_node(parser,sym_proc,4)
       call new_decl(parser,name,modl_proc,.true.)
    else
       call pm_ptr_assign(parser%context,ptr,&
            int(node_args+1,pm_ln),&
            top_val(parser))
       call drop_val(parser)
       call drop_val(parser)
    endif
  contains
    include 'fisnull.inc'
  end subroutine add_proc_decl

  !======================================================
  ! Type declaration
  !======================================================
  function typ_decl(parser) result(iserr)
    type(parse_state):: parser
    logical:: iserr
    integer:: sym,m,n,name,basename,namein,base,nextra
    type(pm_ptr),target:: ptr
    type(pm_reg),pointer:: reg
    type(pm_ptr):: params,p,link
    integer:: sbase,svbase,pname
    sbase=parser%top
    svbase=parser%vtop
    reg=>pm_register(parser%context,'typ_decl',ptr)
    iserr=.true.
    nextra=0
    sym=sym_includes
    call scan(parser)
    if(.not.check_name(parser,name)) then
       call parse_error(parser,'Expected type name')
       goto 999
    endif
    basename=name
    call push_name_val(parser,name)
    call push_null_val(parser)        ! number
    call push_val(parser,parser%modl) ! module

    ! Type parameters
    if(typ_params(parser,m)) goto 999   
    params=top_val(parser)

    ! in typelist
    if(parser%sym==sym_in) then
       call scan(parser)
       if(typ_list(parser,m)) return
       call make_node(parser,sym_list,m)
    else
       call push_null_val(parser)
    endif

    ! Get current declaration to potentially add to link
    ! list of declarations for that type
    ptr=decl_entry(parser,name,modl_type,link)
    call push_val(parser,link)

    ! [ : typelist ]
    if(typ_inclusions(parser,name)) goto 999

    ! Body of declaration, either :
    !    struct or rec
    !    unique
    !    list of types
    if(parser%sym==sym_is) then
       sym=sym_is
       call scan(parser)
       if(parser%sym==sym_rec) then
          if(rec(parser,params,basename,name,m)) goto 999
          call make_node(parser,sym_list,1)
          m=1
       elseif(parser%sym==sym_unique) then
          if(unique(parser,name)) goto 999
          m=1
       else
          ! "type_list | ...type_list | type_list ..."
          sym=sym_includes
          if(parser%sym==sym_dotdotdot) then
             call scan(parser)
             sym=sym_also
             if(parser%sym==sym_comma) then
                call scan(parser)
             else
                call make_node(parser,sym_list,0)
                goto 10
             endif
          endif
          if(typ_list(parser,n)) return
          call make_node(parser,sym_list,n)
          if(parser%sym==sym_comma) then
             call scan(parser)
             if(sym==sym_also) then
                call parse_error(parser,&
                     'Cannot have "type is ...," ending with " ,..."')
             endif
             if(expect(parser,sym_dotdotdot)) return
             sym=sym_dotdotdot
          endif
       endif
    else
       call push_null_val(parser)
    endif
10  continue
    call make_node(parser,sym,type_num_args+nextra)
    if(debug_parser) then
       write(*,*) 'TYPEDECL>----------------'
       call dump_parse_tree(parser%context,6,top_val(parser),2)
       write(*,*) 'END TYPEDECL-------------'
    endif
    call add_typ_decl(parser,name,ptr)
    iserr=.false.
999 continue
    parser%top=sbase
    parser%vtop=svbase
    call pm_delete_register(parser%context,reg)
  contains

    include 'fisnull.inc'

  end function typ_decl

  !======================================================
  ! Parameters to a type declaration
  !======================================================
  function typ_params(parser,m) result(iserr)
    type(parse_state):: parser
    integer,intent(out):: m
    logical:: iserr
    iserr=.true.
    if(parser%sym==sym_open) then
       call scan(parser)
       m=0
       do
          if(expect_name(parser)) return
          m=m+1
          if(parser%sym==sym_colon) then
             call scan(parser)
             if(typ(parser)) return
          else
             call push_null_val(parser)
          endif
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       call make_node(parser,sym_list,m*2)
       if(expect(parser,sym_close)) return
    else
       m=0
       call push_null_val(parser)
    endif
    iserr=.false.
  end function typ_params


  !======================================================
  ! : typelist
  !======================================================
  function typ_inclusions(parser,name) result(iserr)
    type(parse_state):: parser
    integer,intent(in):: name
    logical:: iserr
    integer:: j,namein
    
    iserr=.true.
    ! "[ : typelist ]" clause
    if(parser%sym==sym_colon) then
       j=0
       do
          call scan(parser)
          if(check_name(parser,namein)) then
             ! An in declaration creates an entry in the
             ! named parent type
             call add_typ_in_decl(parser,namein,name)
          else
             call parse_error(parser,'Expecting type name')
             return
          endif
          call push_sym_val(parser,namein)
          call make_node(parser,sym_type,1)
          j=j+1
          if(parser%sym/=sym_comma) exit
       enddo
       ! Also include a list of "in" types for this type
       call make_node(parser,sym_list,j)
    else
       call push_null_val(parser)
    endif
    iserr=.false.
  contains
    include 'fisnull.inc'
  end function typ_inclusions

  !======================================================
  ! Add a declaration that type 'namein' is
  ! included in type 'name'
  !======================================================
  subroutine add_typ_in_decl(parser,namein,name)
    type(parse_state):: parser
    integer,intent(in):: namein,name
    type(pm_ptr):: ptrin
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
    call add_typ_decl(parser,namein,ptrin)
  contains
    include 'fisnull.inc'
  end subroutine add_typ_in_decl

  !======================================================
  ! Add type declaration on top of vstack under name nam
  !======================================================
  subroutine add_typ_decl(parser,nam,p)
    type(parse_state):: parser
    integer,intent(in):: nam
    type(pm_ptr),intent(in):: p
    type(pm_ptr):: q
    if(parser%error_count>0) then
       call drop_val(parser)
       return
    endif
    if(pm_fast_isnull(p)) then
       call push_sym(parser,-get_modl_name(parser%modl))
       call push_sym(parser,nam)
       q=top_val(parser)
       call name_vector(parser,parser%top-2)
       call push_val(parser,q)
       call push_val(parser,q)
       call push_null_val(parser)
       call push_null_val(parser)
       call make_node(parser,sym_type,5)
       call new_decl(parser,nam,modl_type,.false.)
       call drop_val(parser)
    else
       call pm_ptr_assign(parser%context,p,&
            int(node_args+1,pm_ln),top_val(parser))
       call drop_val(parser)
    endif
  contains
    include 'fisnull.inc'
  end subroutine add_typ_decl

  !======================================================
  ! type .. is unique
  !======================================================
  function unique(parser,name) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    logical:: iserr
    integer:: mname,pname
    iserr=.true.

    call scan(parser)
    
    ! Declare a parameter with this unique value
    call push_null_val(parser) 
    call make_qualified_name(parser,name)
    call make_node(parser,sym_unique,1)
    call make_node(parser,sym_param,2)
    if(parser%sym==sym_open_brace) then
       ! Name of parameter specified by "{name}"
       call scan(parser)
       if(check_name(parser,pname)) then
          call new_decl(parser,pname,modl_param,.false.)
          if(expect(parser,sym_close_brace)) return
       else
          if(expect_name(parser)) return
       endif
    else
       ! Name of parameter same as type
       call new_decl(parser,name,modl_param,.false.)
    endif
    
    ! Create node for type
    call make_qualified_name(parser,name)
    call make_node(parser,sym_unique,1)
    call make_node(parser,sym_list,1)
    
    iserr=.false.
  end function unique

  !======================================================
  ! Structure or record declaration
  !======================================================
  recursive function rec(parser,params,basename,typname,nargs) result(iserr)
    type(parse_state),intent(inout):: parser
    type(pm_ptr),intent(in):: params
    integer,intent(in):: basename,typname,nargs
    logical:: iserr
    integer:: i,tag,name,sym,base,vbase,line,pos,n,flags
    logical:: hasvar
    type(pm_ptr):: p
    iserr=.true.
    call make_qualified_name(parser,basename)
    p=pop_val(parser)
    tag=p%offset
    sym=parser%sym
    call scan(parser)
    base=parser%top
    vbase=parser%vtop
    flags=0
    hasvar=.false.
    if(parser%sym==sym_caret) then
       call scan(parser)
       flags=pm_type_is_soa
    endif
    if(parser%sym==sym_open_brace) then
       call scan(parser)
    else
       if(parser%sym==sym_var) then
          hasvar=.true.
          call scan(parser)
       else
          call parse_error(parser,&
               'Expected "{" or "var"')
       endif
       if(expect(parser,sym_open_brace)) return
    endif
    call push_sym(parser,tag)
    n=0
    do
       if(parser%sym==sym_var) then
          if(hasvar) then
             call parse_error(parser,'Cannot have "var" element in "rec var{...}"')
          endif
          call scan(parser)
          if(check_name_no_repeat(parser,name,base+1)) then
             call push_sym(parser,-name)
          else
             call parse_error(parser,&
                  'Expected name of element')
             return
          endif
       elseif(check_name_no_repeat(parser,name,base+1)) then
          call push_sym(parser,merge(-name,name,hasvar))
       else
          call parse_error(parser,&
               'Expected name of element')
          return
       endif
       n=n+1
       if(parser%sym==sym_colon) then
          call scan(parser)
          if(typ(parser)) return
       else
          call push_null_val(parser)
       endif
       if(parser%sym==sym_assign) then
          call scan(parser)
          if(expr(parser)) return
          call make_node(parser,sym_assign,2)
       endif
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    
    ! Rec definition record is: list{type},tag,typname,nargs,params,keys
    call make_node(parser,sym_list,parser%vtop-vbase)
    call name_vector(parser,base)
    !if(hasuse) parser%vstack(parser%vtop)%offset=-parser%vstack(parser%vtop)%offset
    call push_num_val(parser,tag)
    call push_num_val(parser,nargs)
    call push_val(parser,params)
    call push_null_val(parser)
  !  if(hasuse) flags=ior(flags,pm_type_has_embedded)
    call push_num_val(parser,flags)
    call make_node(parser,sym,7)
    if(expect(parser,sym_close_brace)) return
    iserr=.false.
  contains
    include 'fisnull.inc'
  end function rec

  !======================================================
  ! Parameter declarations
  !======================================================
  function param_decl(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: name,name2,m,base,top,serror
    iserr=.true.
    call scan(parser)
    do
       call push_null_val(parser)
       if(.not.check_name(parser,name)) return
       if(expect(parser,sym_assign)) return
       serror=parser%error_count
       call xexpr(parser)
       if(parser%error_count>serror) return
       call make_node(parser,sym_param,2)
       call new_decl(parser,name,modl_param,.false.)
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    iserr=.false.
    return
  end function param_decl

  !======================================================  
  ! Declarations
  !======================================================
  subroutine decl(parser,is_root_module)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: is_root_module
    type(pm_ptr):: modl
    integer:: dt
    type(pm_ptr):: old,p
    integer:: m,sym,name,name2,base,top,kind,line,pos
    integer:: serror,num_tests
    logical:: ok
    if(.not.(parser%modl==parser%sysmodl)) then
       call push_sym_val(parser,sym_pm_system)
       call push_val(parser,parser%sysmodl)
       call push_null_val(parser)
       call make_node(parser,sym_use,3)
       call new_import(parser,sym_pm_system,pop_val(parser))
    endif
    do while(parser%sym==sym_use)
       call use_stmt
       if(parser%sym==sym_semi) then
          call scan(parser)
       elseif(.not.parser%atstart) then
          if(parser%sym==sym_eof) then
             call parse_error(parser,&
                  'Unexpected end of file after "use" statements')
          else
             call parse_error(parser,&
                  'Expected ";" or newline after "use" statement')
          endif
          call skip_past_error(parser,.false.)
       endif
    enddo
    num_tests=0
    do
       select case(parser%sym)
       case(sym_proc)
          if(proc_decl(parser)) goto 999
       case(sym_type)
          if(typ_decl(parser)) goto 999
       case(sym_param)
          if(param_decl(parser)) goto 999
       case(sym_test)
          if(test_stmt(parser)) goto 999
          num_tests=num_tests+1
       case(sym_pm_if_compiling)
          call scan(parser)
          if(.not.pm_is_compiling) then
             do while(parser%sym/=sym_pm_else)
                call scan(parser)
             enddo
             call scan(parser)
          endif
       case(sym_pm_else)
          call scan(parser)
          do while(parser%sym/=sym_pm_endif)
             call scan(parser)
          enddo
          call scan(parser)
       case(sym_pm_endif)
          call scan(parser)
       case(sym_pm_intrinsic)
          if(intrinsic(parser)) goto 999
       case default
          exit
       end select
       if(parser%sym==sym_eof) goto 10
       if(parser%sym==sym_semi) then
          call scan(parser)
       else if(parser%sym==sym_eof.or..not.parser%atstart) then
          exit
       endif
       cycle
999    call scan(parser)
       call skip_past_error(parser,.false.)
    enddo
    if(is_root_module) then
       call stmt_list(parser,num_to_include=num_tests)
    else
       if(parser%sym/=sym_eof) then
          call parse_error(parser,&
               'A library module cannot contain executable statements apart from "test"')
       end if
       call push_null_val(parser)
    end if
    if(parser%sym/=sym_eof) then
       call parse_error(parser,'Expected end of module')
    endif
    if(parser%error_count==0) then
       parser%modl%data%ptr(parser%modl%offset&
            +modl_stmts)=pop_val(parser)
    endif
10  continue

  contains

    include 'fesize.inc'
    include 'fvkind.inc'

    ! use [ . ] name (. name ...) [ => name ] [ { modifiers } ]
    subroutine use_stmt()
      logical:: iserr
      call scan(parser)
       base=parser%top
       if(parser%sym==sym_dot) then
          call scan(parser)
          call push_current_path()
       endif
       do 
          if(check_name_pos(parser,name,line,pos)) then
             call push_sym(parser,name)
          else
             call parse_error(parser,'Expected module name')
             call skip_past_error(parser,.false.)
             name=sym_use
             exit
          endif
          if(parser%sym==sym_dot) then
             call scan(parser)
          else
             exit
          endif
       enddo
       name2=name
       if(parser%top>base+1) then
          call name_vector(parser,base)
          p=parser%vstack(parser%vtop)
          name=p%offset
       else
          call push_name_val(parser,name)
       endif
       call push_back_at(parser,name,line,pos)
       call new_modl(parser,name)
       call scan(parser)
       sym=sym_use
       if(parser%sym==sym_cond) then
          call scan(parser)
          if(expect_name(parser)) goto 998
          p=pop_val(parser)
          name2=p%offset
       endif
       if(parser%sym==sym_open_brace) then
          if(import_list(sym)) goto 998
       else
          call push_null_val(parser)
       endif
       call make_node_at(parser,sym,3,line,pos)
       call new_import(parser,name2,pop_val(parser))
       return
998    call skip_past_error(parser,.false.)
     end subroutine use_stmt

    ! { ( type | proc | param ) name  ... }
    function import_list(sym) result(iserr)
      integer,intent(inout):: sym
      logical:: iserr
      type(pm_ptr):: dict
      integer:: key(2)
      integer:: name,junk,msym,line,pos
      iserr=.true.
      sym=sym_open_brace
      call scan(parser)
      call push_val(parser,pm_dict_new(parser%context,8_pm_ln))
      dict=top_val(parser)
      m=0
      do
         msym=sym_use
         select case(parser%sym)
         case(sym_type)
            kind=modl_type 
            call scan(parser)
         case(sym_param) 
            kind=modl_param
            call scan(parser)
         case(sym_proc)
            kind=modl_proc
            call scan(parser)
         case(sym_close_brace)
            exit
         case default
            call parse_error(parser,'Invalid definition modifier')
            return
         end select
         key(1)=kind
         do
            if(check_name_pos(parser,name,line,pos)) then
               key(2)=name
            else
               call parse_error(parser,'Expected name')
               return
            endif
            if(parser%error_count==0) then
               if(pm_ivect_lookup(parser%context,dict,key,2)/=0) then
                  call parse_error(parser,'Name is repeated in modifier list')
                  return
               endif
               call make_node_at(parser,msym,0,line,pos)
               junk=pm_idict_add(parser%context,dict,key,2,pop_val(parser))
            endif
            if(parser%sym/=sym_comma) exit
            call scan(parser)
         enddo
         if(parser%sym==sym_close_brace) exit
         if(parser%sym==sym_semi) then
            call scan(parser)
         else
            if(.not.parser%atstart) then
               if(expect(parser,sym_semi)) return
            endif
         endif
      enddo
      if(expect(parser,sym_close_brace)) return
      iserr=.false.
    end function  import_list

    subroutine push_current_path()
      type(pm_ptr):: module_name
      integer:: i
      module_name=parser%modl
      module_name=module_name%data%ptr(module_name%offset+modl_name)
      module_name=pm_name_val(parser%context,int(module_name%offset))
      if(pm_fast_vkind(module_name)==pm_int) then
         do i=0,pm_fast_esize(module_name)-1
            call push_sym(parser,int(module_name%data%i(module_name%offset+i)))
         enddo
      endif
    end subroutine push_current_path
    
  end subroutine decl


  !*******************************************************
  ! SERVICE ROUTINES
  !*******************************************************

  !======================================================
  ! Skip tokens until out of expr or statement
  !======================================================
  recursive subroutine skip_past_error(parser,expr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: expr
    if(debug_parser) write(*,*) 'Skipping',expr
    do
       if(expr) then
          if(parser%sym<=num_sym.and.parser%sym>last_expr) then
             call skip_past_error(parser,.false.)
             exit
          endif
          do while(parser%sym/=sym_eof.and.(parser%sym>num_sym.or.&
               parser%sym<=last_expr))
             if(parser%sym==sym_assign) then
                call push_back(parser,sym_array)
                exit
             endif
             call scan(parser)
          enddo
       else
          if(parser%sym<=num_sym.and.parser%sym>last_key) then
             call scan(parser)
             exit
          endif
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
    if(debug_parser) write(*,*) 'Skipped'
  end subroutine skip_past_error

  !======================================================
  ! Push onto value stack
   !======================================================
  subroutine push_val(parser,val)
    type(parse_state),intent(inout):: parser
    type(pm_ptr),intent(in):: val
    parser%vtop=parser%vtop+1
    if(parser%vtop>max_parse_stack) then
       call parse_error(parser,"Syntax too complex")
       stop "Parser terminated"
    endif
    parser%vstack(parser%vtop)=val
    parser%vline(parser%vtop)=parser%sym_lineno
    parser%vchar(parser%vtop)=parser%sym_n
  end subroutine push_val

  !======================================================
  ! Pop off value stack
  !======================================================
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

  !======================================================
  ! Drop top of value stack
  !======================================================
  subroutine drop_val(parser)
    type(parse_state),intent(inout):: parser
    parser%vtop=parser%vtop-1
  end subroutine drop_val

  !======================================================
  ! Replace second-to-top value on stack with top value
  ! decreasing stack ptr by 1
  !======================================================
  subroutine push_down_val(parser)
    type(parse_state),intent(inout):: parser
    parser%vtop=parser%vtop-1
    parser%vstack(parser%vtop)=parser%vstack(parser%vtop+1)
  end subroutine push_down_val

  !======================================================
  ! Top of value stack
  !======================================================
  function top_val(parser) result(val)
    type(parse_state),intent(inout):: parser
    type(pm_ptr):: val
    if(parser%error_count>0) then
       val=pm_null_obj
    else
       val=parser%vstack(parser%vtop)
    endif
  end function top_val

  !======================================================
  ! Swap top two entries on value stack
  !======================================================
  subroutine swap_vals(parser)
    type(parse_state),intent(inout):: parser
    type(pm_ptr):: temp
    integer:: templine,tempchar
    if(parser%error_count>0) return
    temp=parser%vstack(parser%vtop)
    parser%vstack(parser%vtop)=parser%vstack(parser%vtop-1)
    parser%vstack(parser%vtop-1)=temp
    templine=parser%vline(parser%vtop)
    parser%vline(parser%vtop)=parser%vline(parser%vtop-1)
    parser%vline(parser%vtop-1)=templine
    tempchar=parser%vchar(parser%vtop)
    parser%vchar(parser%vtop)=parser%vchar(parser%vtop-1)
    parser%vchar(parser%vtop-1)=tempchar
  end subroutine swap_vals

  !======================================================
  ! Duplicate value on top of the value stack
  !======================================================
  subroutine dup_val(parser)
    type(parse_state),intent(inout):: parser
    parser%vtop=parser%vtop+1
    parser%vstack(parser%vtop)=parser%vstack(parser%vtop-1)
    parser%vline(parser%vtop)=parser%vline(parser%vtop-1)
    parser%vchar(parser%vtop)=parser%vchar(parser%vtop-1)
  end subroutine dup_val

  !======================================================
  ! Push a null value onto value stack
  !======================================================
  subroutine push_null_val(parser)
    type(parse_state),intent(inout):: parser
    call push_val(parser,pm_null_obj)
  end subroutine push_null_val

  !======================================================
  ! Push a tiny integer onto the value stack
  !======================================================
  subroutine push_num_val(parser,n)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: n
    type(pm_ptr):: val
    val=pm_fast_tinyint(parser%context,n)
    call push_val(parser,val)
  contains
    include 'ftiny.inc'
  end subroutine push_num_val

  !======================================================
  ! Pop a tiny integer from the value stack
  !======================================================
  function pop_num_val(parser) result(num)
    type(parse_state),intent(inout):: parser
    integer:: num
    num=parser%vstack(parser%vtop)%offset
    parser%vtop=parser%vtop-1
  end function pop_num_val

  !======================================================
  ! Push token on to value stack
  !======================================================
  subroutine push_sym_val(parser,n)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: n
    type(pm_ptr):: val
    val=pm_fast_name(parser%context,n)
    call push_val(parser,val)
  contains
    include 'fname.inc'  
  end subroutine push_sym_val

  !======================================================
  ! Push name on to value stack
  !======================================================
  subroutine push_name_val(parser,n)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: n
    type(pm_ptr):: val
    val=pm_fast_name(parser%context,n)
    call push_val(parser,val)
    parser%vline(parser%vtop)=parser%name_lineno
    parser%vchar(parser%vtop)=parser%name_sym_n
  contains
    include 'fname.inc'  
  end subroutine push_name_val

  !=============================================================
  ! Push name on to value stack with given location information
  !==============================================================
  subroutine push_name_val_at(parser,n,line,pos)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: n,line,pos
    type(pm_ptr):: val
    val=pm_fast_name(parser%context,n)
    call push_val(parser,val)
    parser%vline(parser%vtop)=line
    parser%vchar(parser%vtop)=pos
  contains
    include 'fname.inc'  
  end subroutine push_name_val_at

  !======================================================
  ! Push symbol onto symbol stack
  !======================================================
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

  !======================================================
  ! Pop symbol from symbol stack
  !======================================================
  function pop_sym(parser) result(sym)
    type(parse_state),intent(inout):: parser
    integer:: sym
    sym=parser%stack(parser%top)
    parser%top=parser%top-1
  end function pop_sym

  !======================================================
  ! Make a qualified name:  name:: name
  !======================================================
  subroutine make_qualified_name(parser,name)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    integer:: mname
    mname=-get_modl_name(parser%modl)
    call push_sym(parser,mname)
    call push_sym(parser,name)
    call name_vector(parser,parser%top-2)
  end subroutine make_qualified_name

  !======================================================
  ! Make node from top n elements of value stack
  ! optionally ignore the top value
  !======================================================
  subroutine make_node(parser,typeno,n,m)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: typeno,n
    integer,intent(in),optional:: m
    integer:: mm
    mm=n
    if(present(m)) mm=m
    if(mm==0) then
       call make_node_at(parser,typeno,n,parser%sym_lineno,parser%sym_n)
    else
       call make_node_at(parser,typeno,n,int(parser%vline(parser%vtop-mm+1)),&
            int(parser%vchar(parser%vtop-mm+1)))
    endif
  end subroutine make_node

  !======================================================
  ! Make node with given file/position info
  !======================================================
  subroutine make_node_at(parser,typeno,n,line,pos)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: typeno,n
    integer,intent(in):: line,pos
    type(pm_ptr):: val
    integer:: i
    logical:: reuse

    if(parser%error_count>0) then
       parser%vtop=parser%vtop-n+1
       if(parser%vtop<1) parser%vtop=1
       parser%vstack(parser%vtop)=pm_null_obj
       return
    endif
    
    val=pm_fast_newnc(parser%context,pm_pointer,n+5)
    
    if(debug_parser) then
       write(*,*) 'make node:',sym_names(typeno),&
            parser%vtop,n,val%data%esize
    endif
    
    val%data%ptr(val%offset)%data=>pm_null_obj%data

    if(check_node_reuse.and.val%data%ptr(val%offset)%offset==9876) then
       ! Flag reused nodes if required (for debugging)
       val%data%ptr(val%offset)%offset=9875
    else
       ! Magic number
       val%data%ptr(val%offset)%offset=9876
    endif
    
    val%data%ptr(val%offset+1)%data=>pm_null_obj%data
    val%data%ptr(val%offset+1)%offset=typeno
    val%data%ptr(val%offset+2)=parser%modl
    val%data%ptr(val%offset+3)%data=>pm_null_obj%data
    val%data%ptr(val%offset+3)%offset=line
    val%data%ptr(val%offset+4)%data=>pm_null_obj%data
    val%data%ptr(val%offset+4)%offset=pos
    if(val%offset+n+4>4096) call pm_panic('make_node')
    do i=1,n
       val%data%ptr(val%offset+i+4)=parser%vstack(parser%vtop+i-n)
    enddo
    parser%vtop=parser%vtop-n+1
    parser%vstack(parser%vtop)=val
    parser%vline(parser%vtop)=line
    parser%vchar(parser%vtop)=pos
    if(debug_parser) then
       write(*,*) '------New node------',n+5
       call dump_parse_tree(parser%context,6,val,2)
       write(*,*) '--------------------'
    endif
  contains
    include 'fnewnc.inc'
  end subroutine make_node_at

  !======================================================
  ! Create vector names/numbers
  ! Values taken from token stack above base
  !======================================================
  subroutine name_vector(parser,base)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: base
    integer:: sym
    sym=pm_name_vector(parser%context,parser%stack,base,parser%top)
    call push_sym_val(parser,sym)
    parser%top=base
  contains
    include 'fnewnc.inc'
  end subroutine name_vector

  !======================================================
  ! Create new module object
  !======================================================
  subroutine new_modl(parser,name)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    type(pm_ptr):: modl
    integer:: i
    integer,parameter:: siz=modl_param+modl_local
    type(pm_ptr):: nameval
    logical:: ok
    character(len=pm_max_filename_size):: str,str2
    nameval=pm_fast_name(parser%context,name)
    modl=pm_dict_lookup(parser%context,parser%modl_dict,&
         nameval)
    if(pm_fast_isnull(modl)) then
       if(pm_main_process.and.name/=sym_pm_system.and..false.) then
          call pm_name_string(parser%context,&
            int(nameval%offset),str)
          call pm_module_filename(str,str2,pm_opts%lib_path_set,pm_opts%lib_path)
          inquire(file=trim(str2),exist=ok)
          if(.not.ok) then
             call parse_error(parser,'module does not correspond to a source file, need: '//&
                  trim(str2))
          endif
       endif
       call push_sym_val(parser,name)
       call push_val(parser,parser%modls)
       call push_val(parser,parser%modl)
       call push_null_val(parser)
       do i=modl_include,modl_param+modl_local
          call push_val(parser,pm_dict_new(parser%context,4_pm_ln))
       enddo
       modl=pm_fast_newnc(parser%context,pm_pointer,siz+1)
       modl%data%ptr(modl%offset)%data=>pm_null_obj%data
       modl%data%ptr(modl%offset)%offset=0
       modl%data%ptr(modl%offset+1:modl%offset+siz)=&
            parser%vstack(parser%vtop-siz+1:parser%vtop)
       parser%vtop=parser%vtop-siz+1
       parser%modls=modl
       call pm_dict_set(parser%context,parser%modl_dict,&
            nameval,modl,.true.,.true.,ok)
    else
       if(modl%data%ptr(modl%offset+modl_last)==parser%modl) then
          call parse_error(parser,&
               'The same module name+path cannot occur in more than one use statement: '//&
               trim(pm_name_as_string(parser%context,name)))
       endif
    endif
    parser%vstack(parser%vtop+1)=modl
    parser%vtop=parser%vtop+1
  contains
    include 'fisnull.inc'
    include 'fnewnc.inc'
    include 'fname.inc'
  end subroutine new_modl

  !======================================================
  ! Return module name from a node
  !======================================================
  function get_modl_name(ptr) result(name)
    type(pm_ptr):: ptr
    integer:: name
    name=ptr%data%ptr(ptr%offset+modl_name)%offset
  end function get_modl_name

  !======================================================
  ! Enter a new declaration into current module
  ! slot = modl_type, modl_proc, modl_param
  ! optionally overwrite current definition
  !======================================================
  subroutine new_decl(parser,name,slot,overwrt)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    integer,intent(in):: slot
    logical,intent(in):: overwrt
    integer:: m
    type(pm_ptr):: node,modl,nameval,keys
    logical:: ok
    m=slot
    if(parser%error_count>0) return
    node=pm_fast_newnc(parser%context,pm_pointer,2)
    node%data%ptr(node%offset)=pop_val(parser)
    node%data%ptr(node%offset+1)=node
    modl=parser%modl
    nameval=pm_fast_name(parser%context,name)
    if(pm_name_is_local(parser%context,name)) m=m+modl_local
    call pm_dict_set(parser%context,modl%data%ptr(modl%offset+m),&
         nameval,node,.true.,overwrt,ok)
    if(.not.ok) then
       call parse_error(parser,'Redefinition not allowed: '//&
            trim(pm_name_as_string(parser%context,name)))
    endif
  contains
    include 'fname.inc'
    include 'fisnull.inc'
    include 'fvkind.inc'
    include 'fnewnc.inc'
  end subroutine new_decl

  !======================================================
  ! Return declaration for a given name/slot
  !======================================================
  function decl_entry(parser,name,slot,link) result(ptr)
    type(parse_state),intent(in):: parser
    integer,intent(in):: name,slot
    type(pm_ptr),intent(out),optional:: link
    type(pm_ptr):: ptr
    integer:: m
    type(pm_ptr):: modl,nameval,val
    character(len=100):: str
    if(parser%error_count>0) then
       ptr=pm_null_obj
       if(present(link)) link=pm_null_obj
       return
    endif
    m=slot
    modl=parser%modl
    call pm_name_string(parser%context,name,str)
    nameval=pm_fast_name(parser%context,name)
    if(pm_name_is_local(parser%context,name)) m=m+modl_local
    val=pm_dict_lookup(parser%context,modl%data%ptr(modl%offset+m),nameval)
    if(pm_fast_isnull(val)) then
       ptr=val
       if(present(link)) link=val
    else
       ptr=val%data%ptr(val%offset)
       if(present(link)) link=ptr%data%ptr(ptr%offset+node_args+1)
    endif
  contains
    include 'fname.inc'  
    include 'fvkind.inc'
    include 'fisnull.inc'
  end function decl_entry


  !======================================================
  ! New import of a module into this one
  !======================================================
  subroutine new_import(parser,name,node)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    type(pm_ptr),intent(in):: node
    type(pm_ptr):: modl,nameval
    logical:: ok
    integer,dimension(2):: key
    integer:: junk
    if(parser%error_count>0) return
    modl=parser%modl
    nameval=pm_fast_name(parser%context,name)
    call pm_dict_set(parser%context,modl%data%ptr(modl%offset+modl_include),&
         nameval,node,.true.,.false.,ok)
    if(.not.ok) then
       call parse_error(parser,&
            'Cannot have same module name in multiple use statements: '//&
            trim(pm_name_as_string(parser%context,name)))
    endif
    key(1)=get_modl_name(parser%modl)
    if(key(1)/=sym_pm_system) then
       key(2)=name
       junk=pm_iset_add(parser%context,parser%visibility,key,2)
    endif
  contains
    include 'fname.inc'
  end subroutine new_import

  !===================================================================
  ! Has a module been imported into this module with then given name
  !====================================================================
  function is_import(parser,name) result(ok)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    logical:: ok
    type(pm_ptr):: modl,nameval
    modl=parser%modl
    nameval=pm_fast_name(parser%context,name)
    ok=.not.pm_fast_isnull(pm_dict_lookup(parser%context,&
         modl%data%ptr(modl%offset+modl_include),nameval))
  contains
    include 'fisnull.inc'
    include 'fname.inc'
  end function is_import


  !======================================================
  ! Syntax error - print message 
  ! and stop building parse tree
  !======================================================
  subroutine parse_error(parser,emess)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: emess
    character(len=67):: caret
    character(len=100):: modname
    character(len=7):: lbuffer,lbuffer2
    integer:: i,n
    if(pm_main_process) then
       if(debug_parser) write(*,*) '*****Error::',trim(emess)
       call pm_name_string(parser%context,&
            int(parser%modl%data%ptr(parser%modl%offset+modl_name)%offset),modname)
       write(lbuffer,'(I7)') parser%sym_lineno
       write(lbuffer2,'(I7)') parser%sym_n
       write(*,*)
       if(pm_opts%colour) then
          write(*,'(A,A,A,A,A,A,A)') pm_loc_start,trim(modname),&
                 trim(adjustl(lbuffer)),':',adjustl(lbuffer2),pm_loc_end
       else
          write(*,'(A,A,A,A,A)') trim(modname),':',&
               trim(adjustl(lbuffer)),':',adjustl(lbuffer2)
       endif
       write(*,*)
       i=1
       n=parser%sym_n
       if(n==0) n=1
       do while(n>67) 
          i=i+60
          n=n-60
       enddo
       if(parser%lineno==parser%sym_lineno+1) then
          write(*,'(3x,A67)') parser%line(3-parser%ls)(i:)
       elseif(parser%lineno==parser%sym_lineno) then
          write(*,'(3x,A67)') parser%line(parser%ls)(i:)
       else
          goto 10
       endif
       caret=" "
       caret(n:n)="!"
       if(pm_opts%colour) then
          write(*,'(3X,A,A67,A)') pm_opts%error_start,caret,pm_error_end
       else
          write(*,'(3X,A67)') caret
       endif
10     continue
       write(*,'(A,X,A)') trim(pm_opts%error),trim(emess)
    endif
    parser%error_count=parser%error_count+1
    if(parser%error_count>max_errors) then
       call pm_stop('Too many syntax errors - compilation terminated')
    endif
  end subroutine parse_error

  
end module pm_parser
