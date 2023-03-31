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
  implicit none

  ! Print out lots of parser debugging info
  logical,parameter:: debug_parser=.false.
  logical,parameter:: debug_parser_extra=.false.

  ! Check if memory manager attempts to reuse a node
  ! (this should not happen so is one test of gc)
  logical,parameter:: check_node_reuse=.false.

  ! Maximum arguments to a PM procedure (must be 2**m-1 for some m)
  integer,parameter:: pm_max_args=31
  character(len=3),parameter:: maxargs_str=' 31'

  ! Offsets into module objects
  integer,parameter:: modl_name=1
  integer,parameter:: modl_link=2
  integer,parameter:: modl_last=3
  integer,parameter:: modl_stmts=4
  integer,parameter:: modl_include=5
  integer,parameter:: modl_proc=6
  integer,parameter:: modl_type=7
  integer,parameter:: modl_param=8
  integer,parameter:: modl_local=3

  ! Offsets into parser node objects of various kinds
  integer,parameter:: node_magic=0
  integer,parameter:: node_symbol=1
  integer,parameter:: node_modl=2
  integer,parameter:: node_lineno=3
  integer,parameter:: node_charno=4
  integer,parameter:: node_args=5

  ! Type parse nodes
  integer,parameter:: typ_name=node_args
  integer,parameter:: typ_number=node_args+1
  integer,parameter:: typ_module=node_args+2
  integer,parameter:: typ_params=node_args+3
  integer,parameter:: typ_constraints=node_args+4
  integer,parameter:: typ_link=node_args+5
  integer,parameter:: typ_ins=node_args+6
  integer,parameter:: typ_includes=node_args+7
  integer,parameter:: typ_num_args=8
  integer,parameter:: typ_interface=node_args+8

  ! Proc parse nodes
  integer,parameter:: proc_name=node_args
  integer,parameter:: proc_link=node_args+1
  integer,parameter:: proc_module=node_args+2
  integer,parameter:: proc_flags=node_args+3
  integer,parameter:: proc_params=node_args+4
  integer,parameter:: proc_keys=node_args+5
  integer,parameter:: proc_amplocs=node_args+6
  integer,parameter:: proc_coded_params=node_args+7
  integer,parameter:: proc_coded_results=node_args+8
  integer,parameter:: proc_coded_type=node_args+9
  integer,parameter:: proc_numret=node_args+10
  integer,parameter:: proc_result_types=node_args+11

  ! Alternative final sections for 'proc' parse nodes

  !   - user functions
  integer,parameter:: proc_reduce=node_args+12
  integer,parameter:: proc_check=node_args+13
  integer,parameter:: proc_result=node_args+14
  integer,parameter:: proc_stmts=node_args+15
  integer,parameter:: proc_code_tree=node_args+16
  integer,parameter:: proc_num_args=17

  !   - built in functions
  integer,parameter:: proc_retas=node_args+12
  integer,parameter:: proc_opcode=node_args+13
  integer,parameter:: proc_opcode2=node_args+14
  integer,parameter:: proc_data=node_args+15
  integer,parameter:: proc_coded_builtin=node_args+16
  integer,parameter:: sysproc_num_args=17

  ! Values for flags (other values defined in sysdefs)
  integer,parameter:: proc_is_comm=1
  integer,parameter:: proc_run_complete=2
  integer,parameter:: proc_run_local=4
  integer,parameter:: proc_run_shared=8
  integer,parameter:: proc_run_always=16
  integer,parameter:: proc_inline=32
  integer,parameter:: proc_no_inline=64
  integer,parameter:: proc_is_open=128
  integer,parameter:: proc_is_each_proc=256
  integer,parameter:: proc_is_cond=512
  integer,parameter:: proc_is_uncond=1024
  integer,parameter:: proc_is_abstract=2048

  ! Corresponding flags in proc calls (must be same as for proc_is)
  integer,parameter:: call_is_comm=1
  integer,parameter:: call_ignore_rules=256

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
     type(pm_ptr):: temp, lexval
     integer:: sym, pushback
     integer,dimension(max_parse_stack):: stack
     integer:: top
     type(pm_ptr),dimension(max_parse_stack):: vstack
     integer,dimension(max_parse_stack):: vline,vchar
     integer:: vtop
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
         parser%temp,parser%sysmodl,parser%lexval, &
         parser%visibility,&
         array=parser%vstack, &
         array_size=parser%vtop)
    parser%modl_dict=pm_dict_new(context,128_pm_ln)
    parser%modl=pm_null_obj
    parser%modls=pm_null_obj
    parser%visibility=pm_set_new(context,128_pm_ln)
    parser%vtop=0
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
    if(type_decl(parser)) then
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
    if(debug_parser_extra) write(*,*) 'Now at line',parser%lineno
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
       if(peekchar()=='.') then
          c=getchar()
          sym=sym_open_square
       elseif(peekchar()=='%') then
          c=getchar()
          sym=sym_open_brace
       else
          sym=sym_open
       endif
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
       else
          sym=sym_pling
       endif
    case('/')
       if(peekchar()=='=') then
          c=getchar()
          sym=sym_ne
       else
          sym=sym_divide
       endif
    case(':')
       select case(peekchar())
!!$       case('!')
!!$          c=getchar()
!!$          sym=sym_bar
       case(':')
          c=getchar()
          sym=sym_dcolon
       case('=') 
          c=getchar()
          sym=sym_assign
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
          sym=sym_define
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
       if(peekchar()=='=') then
          c=getchar()
          sym=sym_le
       elseif(peekchar()=='<') then
          c=getchar()
          sym=sym_open_attr
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
       elseif(peekchar()==')') then
          c=getchar()
          sym=sym_close_square
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
       n=n+1
       buffer(n:n)='"'
       val=pm_new_string(parser%context,buffer(1:n))
       sym=sym_string
       parser%lexval=val
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
    case('[')                 ! Alternative (.
       sym=sym_open_square
    case(']')                 ! Alternative .)
       sym=sym_close_square
    case('{')                 ! Alternative (%
       sym=sym_open_brace     
    case('}')                 ! Alternative %)
       sym=sym_close_brace    
    case('|')                 ! Alternative /:
       sym=sym_bar            
    case('@')                 ! Alternative %%
       sym=sym_at      
    case('#')                 ! Alternative %:
       sym=sym_hash
    case('^')                 ! Only used for internal system purposes
       if(.not.(parser%modl==parser%sysmodl).and..false.) then
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
    if(debug_parser_extra) then
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

    ! Look at next character but one on line but do not advance
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
      if(c=='''') then
         c=getchar()
         c=getchar()
         select case(c)
         case('s')
            if(iscomplex) then
               val=pm_fast_newnc(parser%context, &
                    pm_single_complex,1)
               val%data%c(val%offset)=cmplx(0.0,rnumber)
            elseif(isreal) then
               val=pm_fast_newnc(parser%context, &
                    pm_single,1)
               val%data%r(val%offset)=rnumber
            else
               val=pm_fast_newnc(parser%context, &
                    pm_int,1)
               val%data%i(val%offset)=inumber
            endif
         case('l')
            if(isreal) then
               call parse_error(parser,'Long real not available')
            else
               val=pm_fast_newnc(parser%context, &
                    pm_longlong,1)
               val%data%lln(val%offset)=inumber
            endif
         case('8')
            if(isreal) goto 20
            if(isdigit(peekchar())) goto 20
            if(inumber<-127.or.inumber>127) goto 20
            val=pm_fast_newnc(parser%context, &
                 pm_int8,1)
            val%data%i8(val%offset)=inumber
         case('1')
            if(isreal) goto 20
            if(peekchar()/='6'.or.isdigit(peekchar_plus(1))) goto 20
            if(inumber<-16383.or.inumber>16383) goto 20
            val=pm_fast_newnc(parser%context, &
                 pm_int16,1)
            val%data%i16(val%offset)=inumber
            c=getchar()
         case('3')
            if(isreal) goto 20
            if(peekchar()/='2'.or.isdigit(peekchar_plus(1))) goto 20
            if(inumber<-2147483647.or.inumber>2147483647) goto 20
            val=pm_fast_newnc(parser%context, &
                 pm_int32,1)
            val%data%i32(val%offset)=inumber
            c=getchar()
         case('6')
            if(isreal) goto 20
            if(peekchar()/='4'.or.isdigit(peekchar_plus(1))) goto 20
            val=pm_fast_newnc(parser%context, &
                 pm_int64,1)
            val%data%i64(val%offset)=inumber
            c=getchar()
         end select
      else
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
      endif
      parser%lexval=val
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
    call push_null_val(parser)      ! keys
    call push_null_val(parser)      ! amps
    call push_num_val(parser,0)     ! flags
    call make_node(parser,sym_open,5)    
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
  recursive function arglist(parser,object) result(iserr)
    type(parse_state),intent(inout):: parser
    type(pm_ptr),intent(in),optional:: object
    logical:: iserr
    integer m,n,base,sym,flags,line,pos
    type(pm_ptr):: temp
    iserr=.true.
    call get_sym_pos(parser,line,pos)
    m=0
    n=0
    base=parser%top
  
    if(parser%sym==sym_pct) then
       flags=call_is_comm
       call scan(parser)
    else
       flags=0
    endif
    if(flags/=0) then
       call push_sym_val(parser,sym_region)
       call make_node(parser,sym_name,1)
       call push_sym_val(parser,sym_subregion)
       call make_node(parser,sym_name,1)
       call push_sym_val(parser,sym_here_in_tile)
       call make_node(parser,sym_name,1)
       m=3
    endif
    if(present(object)) then
       call push_val(parser,object)
       m=m+1
    endif
    if(parser%sym/=sym_open) then
       if(expect(parser,sym_open)) return
    else
       call scan(parser)
    endif
    ! Call attributes but no arguments
    if(parser%sym==sym_open_attr) then
       if(proc_call_attr(parser,.true.,flags)) return
       if(parser%sym/=sym_close) then
          if(expect(parser,sym_close)) return
       endif
    endif
    if(parser%sym==sym_close) then
       if(m==0) then
          call push_null_val(parser)
       else
          call make_node(parser,sym_list,m)
       endif                           ! args
       call push_null_val(parser)      ! keys
       call push_null_val(parser)      ! amps
       call push_num_val(parser,flags) ! flags
       call make_node_at(parser,sym_open,5,line,pos)
       call scan(parser)
       iserr=.false.
       return
    endif
    do 
       if(parser%sym==sym_amp) then
          call scan(parser)
          if(parser%sym==sym_amp) then
             call scan(parser)
             if(valref(parser)) return
          else
             if(valref(parser)) return
             call make_node(parser,sym_amp,1)
          endif
          m=m+1
          call push_sym(parser,m)
       elseif(parser%sym==sym_key) then
          call make_node(parser,sym_list,m)
          exit
       else if(parser%sym==sym_arg) then
          call scan(parser)
          if(parser%sym==sym_dotdotdot) then
             call push_sym_val(parser,sym_arg)
             call make_node(parser,sym_arg,1)
             call make_node(parser,sym_dotdotdot,m+1)
             call scan(parser)
             exit
          else
             call push_back(parser,sym_arg)
          endif
       else
          if(check_name(parser,sym)) then
             if(parser%sym==sym_define) then
                call make_node(parser,sym_list,m)
                call push_name_val(parser,sym)
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
          exit
       endif
       call scan(parser)
    enddo
    do while(parser%sym==sym_comma)
       call scan(parser)
       if(parser%sym==sym_key) exit
       if(expect_name(parser,&
            'optional argument name')) return
       if(expect(parser,sym_define,&
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

    ! Call attributes if present
    if(parser%sym==sym_open_attr) then
       if(proc_call_attr(parser,.true.,flags)) return
    endif
    
    call push_num_val(parser,flags)
    call make_node_at(parser,sym_open,5,line,pos)
    if(m+n>pm_max_args) then
       call parse_error(parser,&
            'Too many arguments to proc call - maximum is:'//maxargs_str)
    endif
    if(expect(parser,sym_close)) return
    iserr=.false.
  end function arglist

  !======================================================
  ! Qualifiers
  ! .name [ ]
  !======================================================
  recursive function qual(parser,dot_call) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(inout),optional:: dot_call
    logical:: iserr
    integer:: sym
    iserr=.true.
    if(parser%sym==sym_at) then
       call scan(parser)
       call make_node(parser,sym_at,1)
    endif
    do
       select case(parser%sym)
       case(sym_dot)
          call scan(parser)
          select case(parser%sym)
          case(sym_caret)
             call scan(parser)
             if(parser%sym==sym_amp) then
                call scan(parser)
                if(expect(parser,sym_open)) return
                if(expr(parser)) return
                if(expect(parser,sym_close)) return
                call make_node(parser,sym_get_dot_ref,2)
             else
                if(expect(parser,sym_open)) return
                if(expr(parser)) return
                if(expect(parser,sym_close)) return
                call make_node(parser,sym_get_dot,2)
             endif
          case(sym_open_square)
             if(subscript(parser)) return
             call make_node(parser,sym_dot_sub,2)
          case(sym_open,sym_pct)
             call make_node(parser,sym_dot,1)
             if(arglist(parser)) return
             if(present(dot_call)) then
                dot_call=.true.
                iserr=.false.
                return
             endif
          case default
             if(expect_name(parser)) return
             sym=parser%sym
             if(sym==sym_open.or.sym==sym_pct) then
                parser%temp=parser%vstack(parser%vtop-1)
                call make_node(parser,sym_method_call,1)
                call swap_vals(parser)
                call drop_val(parser)
                if(arglist(parser,parser%temp)) return
                if(present(dot_call)) then
                   dot_call=.true.
                   iserr=.false.
                   return
                endif

                ! This supports implementation of lhs calls at some point
!!$                select case(parser%sym)
!!$                case(sym_dot,sym_open_square,sym_d1:sym_d7,sym_pling,sym_at,&
!!$                     sym_assign,sym_comma)
!!$                   call push_val(parser,parser%temp)
!!$                   call make_node(parser,sym_dot_amp,2)
!!$                case default
!!$                   if(present(dot_call)) then
!!$                      dot_call=.true.
!!$                      iserr=.false.
!!$                      return
!!$                   endif
!!$                end select

             else
                call make_node(parser,sym_dot,2)
             endif
          end select
       case(sym_d1:sym_d7)
          call push_sym_val(parser,parser%sym)
          call make_node(parser,sym_dot,2)
          call scan(parser)
       case(sym_open_square)
          if(subscript(parser)) return
          call make_node(parser,sym_sub,2)
!!$       case(sym_at)
!!$          call scan(parser)
!!$          call make_node(parser,sym_at,1)
       case default
          exit
       end select
    enddo
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
          if(symb==sym_close_brace.and.parser%sym==sym_mult) then
             call scan(parser)
             if(mstart/=0.or.nstart/=0) then
                call parse_error(parser,&
                     'Cannot have multiple "*" entries in array or matrix expression')
             endif
             mstart=-m
             nstart=-n
          endif
          if(expr(parser)) return
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
    if(parser%sym==sym_mult) call scan(parser)
    if(expr(parser)) return
    if(parser%sym==sym_comma) then
       call scan(parser)
       if(matrix_former(parser,symb,sym,.false.,.true.)) return
    else if(parser%sym==sym_semi) then
       call scan(parser)
       if(matrix_former(parser,symb,sym,.true.,.false.)) return
    elseif(parser%sym==sym_colon) then
       call scan(parser)
       if(iter(parser)) return
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
    case(first_operator:last_operator)
       sym=parser%sym
       call scan(parser)
    case(sym_open_square)
       call scan(parser)
       if(expect(parser,sym_close_square)) return
       sym=sym_sub
    case(sym_dotdotdot)
       call scan(parser)
       if(expect(parser,sym_underscore)) return
       sym=sym_to_range
    case(sym_underscore)
       call scan(parser)
       if(expect(parser,sym_dotdotdot)) return
       sym=sym_from_range
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
  recursive function struct_gen(parser) result(iserr)
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
          if(expect(parser,sym_define)) return
          if(expr(parser)) return
          call make_node_at(parser,sym_define,1,line,pos)
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
    endif
    if(expect(parser,sym_close_brace)) return
    call make_node_at(parser,sym_list,parser%vtop-vbase,line1,pos1)
    call name_vector(parser,base)
    call push_sym_val(parser,tag)
    call make_node_at(parser,sym_struct,4,line1,pos1)
    iserr=.false.
  end function struct_gen

  !======================================================
  ! Term in an expression
  !======================================================
  recursive function term(parser,checkqual) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: checkqual
    logical:: iserr
    integer:: m,name,sym,base,line,pos
    logical:: atstart,dot_call
    iserr=.true.
    sym=parser%sym
    select case(sym)
    case(sym_if,sym_switch)
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
       call make_node(parser,merge(sym_if_expr,sym_switch_expr,sym==sym_if),m)
    case(sym_coherent,sym_mirrored,sym_shared)
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(expr(parser)) return
       call push_sym_val(parser,sym)
       if(parser%sym==sym_open_attr) then
          call scan(parser)
          if(expect(parser,sym_always)) return
          if(expect(parser,sym_close_attr)) return
          call make_node(parser,sym_always,2)
       else
          call make_node(parser,sym_mode,2)
       endif
       if(expect(parser,sym_close)) return
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
       call push_sym_val(parser,sym_tuple)
       if(subscript(parser)) return
       call simple_call(parser)
    case(sym_open_brace)
       if(array_former(parser,sym_close_brace)) return
    case(sym_new)
       if(struct_gen(parser)) return
    case(sym_present)
       call scan(parser)
       if(expect_name(parser)) return
       call make_node(parser,sym_present,1)
    case(sym_number,sym_string)
       call push_val(parser,parser%lexval)
       call make_node(parser,sym,1)
       call scan(parser)
    case(sym_dollar)
       call scan(parser)
       if(op(parser,name,.true.,.false.)) return
       call push_sym_val(parser,name)
       if(parser%sym==sym_dash.and.name>num_sym) then
          call scan(parser)
          if(expect_name(parser)) return
          call make_node(parser,sym_proc,2)
       else
          call make_node(parser,sym_proc,1)
       endif
       if(parser%sym==sym_open.or.parser%sym==sym_pct) then
          if(arglist(parser)) return
       endif
    case(sym_param)
       call scan(parser)
       if(expect(parser,sym_open_brace)) return
       if(expect_name(parser)) return
       if(parser%sym==sym_dash) then
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
    case(sym_dash)
       call scan(parser)
       if(parser%sym==sym_number) then
          if(pm_fast_vkind(parser%lexval)/=pm_long) then
             call parse_error(parser,&
                  '"''" cannot precede non-default integer constant')
          endif
          call push_val(parser,parser%lexval)
          call scan(parser)
          call make_node(parser,sym_dash,1)
       elseif(parser%sym==sym_true.or.&
            parser%sym==sym_false) then
          call push_sym_val(parser,parser%sym)
          call scan(parser)
          call make_node(parser,sym_dash,1)
       elseif(parser%sym==sym_open) then
          call scan(parser)
          if(expr(parser)) return
          if(expect(parser,sym_close)) return
          call make_node(parser,sym_fix,1)
       elseif(parser%sym>num_sym) then
          call push_sym_val(parser,parser%sym)
          call scan(parser)
          call make_node(parser,sym_fix,1)
       else
          call parse_error(parser,'"''" must be followed by constant value')
       endif
    case(sym_null)
       if(parser%sym==sym_open) then
          call scan(parser)
          if(proccall(parser,sym)) return
       else
          call make_node(parser,sym,0)
          call scan(parser)
       endif
    case(sym_true,sym_false) 
       call make_node(parser,sym,0)
       call scan(parser)
       goto 20
!!$    case(sym_dollar)
!!$       call scan(parser)
!!$       if(expect_name(parser)) return
!!$       if(parser%sym>=sym_d1.and.parser%sym<=sym_d7) then
!!$          call push_sym_val(parser,parser%sym)
!!$          call scan(parser)
!!$       else
!!$          call push_null_val(parser)
!!$       endif
!!$       call make_node(parser,sym_dollar,2)
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
       if(m/=3.and.m/=5) then
          call parse_error(parser,'Wrong number of args to: '//sym_names(sym))
          return
       endif
       call make_node(parser,sym,m)
    case default
       if(check_name_pos(parser,name,line,pos)) then
          select case(parser%sym)
          case(sym_open,sym_pct)
             if(proccall(parser,name)) return
          case(sym_dash)
             if(expect_name(parser)) return
             call make_node(parser,sym_use,2)
             if(parser%sym==sym_open.or.&
                  parser%sym==sym_pct) then
                if(arglist(parser)) return
             endif
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


  !======================================================
  ! Expression
  !======================================================
  recursive function expr(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    iserr=expr1(parser,100)
  end function expr
    
  recursive function expr1(parser,priority) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: priority
    logical:: iserr

    integer,parameter:: priority_uhash=1     ! # (unary)
    integer,parameter:: priority_pow=2       ! **
    integer,parameter:: priority_mult=3      ! * /
    integer,parameter:: priority_uminus=4    ! - (unary)  !
    integer,parameter:: priority_hash=5      ! #
    integer,parameter:: priority_as=6        ! as
    integer,parameter:: priority_bitshift=7  ! shift
    integer,parameter:: priority_bitand=8    ! &
    integer,parameter:: priority_bitxor=9    ! xor
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
    case(sym_pling)
       if(unary(priority_uminus,sym_pling)) return
    case(sym_hash)
       if(unary(priority_uhash,sym_uhash)) return
    case(sym_not)
       if(unary(priority_not,sym_not)) return
    case(sym_by)
       if(unary(priority_by,sym_by)) return
    case(sym_ortho)
       if(unary(priority_ortho,sym_ortho)) return
    case(sym_dotdotdot)
       parser%sym=sym_to_range
       if(unary(priority_dotdot,sym_to_range)) return
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
       case(sym_gt,sym_ge,sym_lt,sym_le,sym_in,&
            sym_includes,sym_is)
          if(binary_none(priority_gt)) return
       case(sym_mod)
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
       case(sym_xor)
          if(binary(priority_bitxor)) return
       case(sym_shift)
          if(binary(priority_bitshift)) return
       case(sym_hash)
          if(binary_none(priority_hash)) return
       case(sym_as)
          if(binary_none(priority_as)) return
       case(sym_ortho)
          if(binary_none(priority_ortho)) return
       case(sym_dotdotdot)
          if(priority<priority_dotdot) return
          call no_repeat(priority_dotdot)
          call get_sym_pos(parser,line,pos)
          call make_node_at(parser,sym_from_range,1,line,pos)
          call scan(parser)
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
  recursive function exprlist(parser,length,nolist) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out),optional:: length
    logical,intent(in),optional:: nolist
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
    if(.not.present(nolist)) &
         call make_node(parser,sym_list,n)
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
    n=0
    sym=sym_list
    do
       if(parser%sym==sym_arg) then
          call push_sym_val(parser,sym_arg)
          call make_node(parser,sym_arg,1)
          call scan(parser)
          if(expect(parser,sym_dotdotdot)) return
          sym=sym_dotdotdot
          n=n+1
          exit
       endif
       if(sexpr()) return
       n=n+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    if(n>7) then
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

  !======================================================
  ! Assignment/definition: lhs, lhs... [ ":=" | "=" ] rhs
  ! or call with no return values
  !======================================================
  recursive function assn_or_call(parser,call_ok,assign_ok,define_ok) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: call_ok,assign_ok,define_ok
    logical:: iserr
    integer:: n,nu,name
    logical:: dotcall,must_be_assignment
    iserr=.true.
    n=0
    nu=0
    must_be_assignment=.false.
    outer: do
       if(parser%sym==sym_underscore) then
          call make_node(parser,sym_underscore,0)
          call scan(parser)
          nu=nu+1
       else
          if(expect_name(parser)) return
       endif
       n=n+1
       do
          select case(parser%sym)
          case(sym_open,sym_pct)
             if(n>1.or.nu>0.or..not.call_ok) then
                call parse_error(parser,&
                     'Unexpected symbol in what seems to be a left hand side expression')
                return
             endif
             if(arglist(parser)) return
             call make_node(parser,sym_do,1)
             iserr=.false.
             return
          case(sym_assign)
             call make_node(parser,sym_assign,n)
             call scan(parser)
             must_be_assignment=.true.
             exit outer
          case(sym_define)
             call push_null_val(parser)
             call make_node(parser,sym_const,n+1)
             call scan(parser)
             if(.not.define_ok) then
                call parse_error(parser,&
                     'Expecting ":=", not "="')
             elseif(must_be_assignment.or.nu==n) then
                call parse_error(parser,&
                     'Incorrect left hand side before "=", probably meaning this should be ":="')
             endif
             exit outer
          case(sym_plus,sym_minus,sym_mult,sym_and,sym_or,sym_amp,sym_bar,sym_xor,sym_concat)
             call push_sym_val(parser,parser%sym)
             call make_node(parser,sym_proc,1)
             call make_node(parser,sym_lt,n+1)
             call scan(parser)
             if(expect(parser,sym_define)) return
             must_be_assignment=.true.
             exit outer
          case(sym_open_brace)
             call scan(parser)
             if(expr(parser)) return
             if(expect(parser,sym_close_brace)) return
             call make_node(parser,sym_lt,n+1)
             if(expect(parser,sym_define)) return
             must_be_assignment=.true.
             exit outer
          case(sym_comma)
             call scan(parser)
             exit
          case(sym_dot,sym_d1:sym_d7,sym_open_square,sym_at)
             must_be_assignment=.true.
             dotcall=.false.
             call make_node(parser,sym_name,1)
             if(qual(parser,dotcall)) return
             if(dotcall) then
                if(n==1.and.call_ok) then
                   call make_node(parser,sym_do,1)
                   iserr=.false.
                   return
                else
                   call parse_error(parser,&
                        'Unexpected call in what seems to be left hand side expression')
                   return
                endif
             endif
             if(parser%sym==sym_comma) then
                call scan(parser)
                exit
             endif
          case default
             call parse_error(parser,'Badly formed left-hand side expression')
             return
          end select
       enddo
    enddo outer
    if(must_be_assignment.and..not.assign_ok) then
       call parse_error(parser,'Cannot have an assignment here')
    endif
    if(rhs(parser,n)) return
    call make_node(parser,sym_define,2)
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
    
    if(n==1) then
       if(expr(parser)) return
    else
       if(check_name(parser,name)) then
          if(parser%sym==sym_open.or.parser%sym==sym_pct) then
             if(proccall(parser,name)) return
          else
             call push_name_val(parser,name)
             call make_node(parser,sym_name,1)
             dotcall=.false.
             if(qual(parser,dotcall)) return
             if(.not.dotcall) then
                call make_node(parser,sym_define,1)
             endif
          endif
       elseif(parser%sym==sym_number.or.parser%sym==sym_string.or.parser%sym==sym_dash) then
          if(term(parser,.false.)) return
          call make_node(parser,sym_define,1)
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
    if(parser%sym==sym_caret) then
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(expr(parser)) return
       if(expect(parser,sym_close)) return
       call make_node(parser,sym_caret,1)
    else
       if(expect_name(parser)) return
       if(parser%sym==sym_dash) then
          call scan(parser)
          if(expect_name(parser)) return
          call make_node(parser,sym_use,2)
       else
          call make_node(parser,sym_name,1)
       end if
    endif
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
    do while(parser%sym==sym_where)
       call scan(parser)
       m=0
       do
          if(name_list(parser,n)) return
          if(expect(parser,sym_define)) return
          call push_null_val(parser)
          call make_node(parser,sym_const,n+1)
          if(rhs(parser,n)) return
          call make_node(parser,sym_define,2)
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
  subroutine xexprlist(parser,length)
    type(parse_state),intent(inout):: parser
    integer,intent(out),optional:: length
    integer:: m
    if(exprlist(parser,m)) return
    if(present(length)) length=m
    if(subexpr(parser)) return
  contains
    include 'fesize.inc'
  end subroutine xexprlist

  !======================================================
  ! While statement
  !======================================================
  recursive function while_stmt(parser,name) result(is_err)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    logical:: is_err
    integer:: line,sym
    is_err=.true.
    line=get_sym_line(parser)
    sym=parser%sym
    call scan(parser)
    call xexpr(parser)
    if(block_or_single_stmt(parser,name,sym,line)) return
    call make_node(parser,sym,3)
    is_err=.false.
  end function while_stmt

  !======================================================
  ! until statement
  !======================================================
  recursive function until_stmt(parser,name) result(is_err)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    logical:: is_err
    integer:: line,sym
    is_err=.true.
    line=get_sym_line(parser)
    sym=parser%sym
    call scan(parser)
    if(sym==sym_do) then
       call make_node(parser,sym_false,0)
    else
       call xexpr(parser)
    endif
    if(block_or_single_stmt(parser,name,sym,line)) return
    call make_node(parser,sym,3)
    is_err=.false.
  end function until_stmt

  !======================================================
  ! do statement
  !======================================================
  recursive function do_stmt(parser) result(is_err)
    type(parse_state),intent(inout):: parser
    logical:: is_err
    integer:: line
    is_err=.true.
    line=get_sym_line(parser)
    call scan(parser)
    if(block_or_single_stmt(parser,sym_do,0,line)) return 
    call make_node(parser,sym_do_stmt,1)
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
    sym=parser%sym
    call scan(parser)
    if(iter(parser,var_name)) return
    if(subexpr(parser)) return
    if(parser%sym==sym_while) then
       call scan(parser)
       call xexpr(parser)
       call make_node(parser,sym_while,1)
    elseif(parser%sym==sym_until) then
       call scan(parser)
       call xexpr(parser)
       call make_node(parser,sym_until,1)
    else
       call push_null_val(parser)
    endif
    if(block_or_single_stmt(parser,name,var_name,line)) return
    call push_sym_val(parser,name)
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
    sym=parser%sym
    call scan(parser)
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
  ! (var | const) { name | _ | exception) } [ : type ] [ = expr ]
  !==============================================================
  recursive function var_stmt(parser,moded_stmt) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in),optional:: moded_stmt
    logical:: iserr
    integer:: n,nu,ne,sym,symi
    logical:: dotcall
    iserr=.true.
    sym=parser%sym
    call scan(parser)
    n=0
    nu=0
    ne=0
    do
       if(parser%sym==sym_underscore) then
          call scan(parser)
          call make_node(parser,sym_underscore,0)
          nu=nu+1
       elseif(parser%sym==sym_open) then
          call scan(parser)
          if(parser%sym==sym_var.or.parser%sym==sym_const) then
             symi=parser%sym
             call scan(parser)
             if(expect_name(parser)) return
             if(parser%sym==sym_colon) then
                call scan(parser)
                if(typ(parser)) return
             else
                call push_null_val(parser)
             endif
             call make_node(parser,symi,2)
          else
             if(expect_name(parser)) return
             if(parser%sym==sym_colon) then
                call scan(parser)
                if(typ(parser)) return
                call make_node(parser,sym,2)
             else
                dotcall=.false.
                if(qual(parser,dotcall)) return
                if(dotcall) then
                   call parse_error(parser,"Unexpected call")
                endif
                select case(parser%sym)
                case(sym_plus,sym_minus,sym_mult,sym_and,&
                     sym_or,sym_amp,sym_bar,sym_xor,sym_concat)
                   call push_sym_val(parser,parser%sym)
                   call make_node(parser,sym_proc,1)
                   call make_node(parser,sym_lt,2)
                   call scan(parser)
                   if(expect(parser,sym_define)) return
                case(sym_open_brace)
                   call scan(parser)
                   if(expr(parser)) return
                   if(expect(parser,sym_close_brace)) return
                   call make_node(parser,sym_lt,2)
                   if(expect(parser,sym_define)) return
                case default
                   call make_node(parser,sym_assign,1)
                   if(expect(parser,sym_assign)) return
                end select
             endif
          endif
          if(expect(parser,sym_close)) return
          ne=ne+1
       else
          if(expect_name(parser)) return
       endif
       n=n+1
       if(parser%sym==sym_comma) then
          call scan(parser)
       else
          exit
       endif
    enddo
    if(parser%sym==sym_colon) then
       if(ne>0) then
          call parse_error(parser,&
               'Cannot have ":type" at the end of a "'//&
               trim(sym_names(sym))//'" list with "(...)" items')
       endif
       call scan(parser)
       if(typ(parser)) return
    else
       call push_null_val(parser)
    endif
    call make_node(parser,sym,n+1)
    if(ne>0.and.sym/=sym_var) then
       call parse_error(parser,'Cannot have "(...)" left-hand-side in a "const" statement')
    endif
    if(ne>0.and.nu+ne/=n) then
       call parse_error(parser,'Incorrect left hand side for "var (...)" statement')
    endif
    if(parser%sym==sym_define) then
       call scan(parser)
       if(rhs(parser,n)) return
       call make_node(parser,sym_define,2)
       if(subexpr(parser)) return
    elseif(present(moded_stmt)) then
       call parse_error(parser,'Must include an initialising expression in a "'//&
            sym_names(sym)//' '//sym_names(moded_stmt)//' statement')
    elseif(nu+ne>0) then
       call parse_error(parser,'Cannot have "_" or "(...)" in unitialised '//&
            trim(sym_names(sym))//' declaration')
    endif
    iserr=.false.
  end function var_stmt

  !==============================================================
  ! ( coherent | mirrored | shared ) [ var | const ] name = expr
  ! ( coherent | shared) call
  !==============================================================
  recursive function mode_stmt(parser,sym) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    logical:: iserr
    iserr=.true.
    if(parser%sym==sym_var.or.parser%sym==sym_const) then
       if(var_stmt(parser,moded_stmt=sym)) return
    else
       if(assn_or_call(parser,&
            sym==sym_coherent.or.sym==sym_shared,.false.,.true.)) return
       if(subexpr(parser)) return
    endif
    call make_node(parser,sym_list,1)
    call push_sym_val(parser,sym)
    call make_node(parser,sym_mode,2)
    iserr=.false.
  end function mode_stmt

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
       call xexprlist(parser)
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
    integer:: name,line,n
    iserr=.true.
    line=get_sym_line(parser)
    call scan(parser)
    if(expect_and_get_name(parser,name)) return
    call make_node(parser,sym_name,1)
    if(parser%sym==sym_define) then
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
    call make_node(parser,sym_any,3)
    iserr=.false.
  end function any_stmt

  !====================================================================
  ! [ for | tfor | cofor ] iter [ << attrs >> ] { statements }
  !====================================================================
  recursive function for_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: sym,name,line
    iserr=.true.
    line=get_sym_line(parser)
    sym=parser%sym
    call scan(parser)
    if(iter(parser,name)) return
    if(parser%sym==sym_open_attr) then
       if(par_attr(parser,sym_distr,sym_work)) return
    else
       call push_null_val(parser)
    endif
    call swap_vals(parser)
    if(subexpr(parser)) return
    call push_sym_val(parser,sym)
    if(block_or_single_stmt(parser,sym_for,name,line)) return
    call make_node(parser,sym_for,4)
    iserr=.false.
  end function for_stmt

  !====================================================================================
  ! par [ << attrs >> ] { statements ( do name [ << attrs >> ] : statements ...) }
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
    if(expect(parser,sym_open_brace)) return
    call stmt_list(parser)
    k=2
    n=0
    if(expect(parser,sym_task)) return
    do
       if(expect_name(parser)) return
       if(parser%sym==sym_open_attr) then
          call scan(parser)
          if(expect(parser,sym_work)) return
          if(expect(parser,sym_define)) return
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
       do i=base+4,base+k,3
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
          do i=sym_distr,sym_work
             call make_node(parser,sym_null,0)
          enddo
          call make_node(parser,sym_list,sym_work-sym_distr+1)
          using=pop_val(parser)
          parser%vstack(base+1)=using
       elseif(parser%error_count==0) then
          p=node_arg(using,node_numargs(using))
          if(.not.pm_fast_isnull(p)) then
             call parse_error(parser,&
                  'Cannot have "work=" at both start'//&
                  ' of "par" statement and in "do :" clauses')
             return
          endif
       endif
       if(parser%error_count==0) then
          call pm_ptr_assign(parser%context,using,&
               pm_fast_esize(using),top_val(parser))
       endif
    endif
    if(parser%error_count>0) then
       parser%vtop=base
       is_error=.false.
       return
    endif
    call push_val(parser,parser%vstack(base+1))
    call push_val(parser,parser%vstack(base+2))
    do i=3,k,3
       call push_val(parser,parser%vstack(base+i))
       call push_val(parser,parser%vstack(base+i+2))
    enddo
    call make_node(parser,sym_par,2+n*2)
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
  recursive function iter(parser,first_name) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out),optional:: first_name
    logical:: iserr
    integer:: i,m,name
    iserr=.true.
    m=0
    do
       if(check_name(parser,name)) then
          call push_name_val(parser,name)
          if(present(first_name).and.m==0) first_name=name
          if(parser%sym==sym_define) then
             call scan(parser)
             call make_node(parser,sym_define,1)
             if(expr(parser)) return
          else
             if(expect(parser,sym_in)) return
             if(expr(parser)) return
          endif
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
  recursive function par_attr(parser,start,finish) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: start,finish
    logical:: iserr
    integer:: n,i,base,vbase,vbase2,name,high
    iserr=.true.
    n=0
    base=parser%top
    vbase=parser%vtop
    high=max(sym_work,finish)
    do while(parser%sym==sym_open_attr)
       call scan(parser)
       do while(parser%sym>num_sym)
          call push_sym(parser,parser%sym)
          call scan(parser)
          if(expect(parser,sym_define)) return
          if(expr(parser)) return
          n=n+1
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       if(expect(parser,sym_close_attr)) return
    enddo
    if(parser%error_count==0.and.n>0) then
       vbase2=parser%vtop
       do i=start,high
          call make_node(parser,sym_null,0)
       enddo
       do i=1,n
          name=parser%stack(base+i)
          if(name>=start.and.&
               name<=finish) then
             name=name-start+1
             if(node_sym(parser%vstack(vbase+i))/=sym_null) then
                parser%vstack(vbase2+name)=parser%vstack(vbase+i)
             else
                call parse_error(parser,'Repeated "<<'//&
                     trim(pm_name_as_string(parser%context,&
                     name+start-1))//'= >>" attribute')
             endif
          else
             call parse_error(parser,'Not an allowed attribute: "<<'//&
                  trim(pm_name_as_string(parser%context,&
                  name))//'=>>"')
          endif
       enddo
       call make_node(parser,sym_list,high-start+1)
       parser%top=base
       parser%vstack(vbase+1)=top_val(parser)
       parser%vtop=vbase+1
    else
       call push_null_val(parser)
    endif
    iserr=.false.
  contains
    include 'fisnull.inc'
  end function par_attr

  !=============================================================
  ! nhd ( tuple of {name in expr} [ bounds expr ] ) block
  !=============================================================
  function nhd_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: n,m,line,line2,var_name,list_sym
    iserr=.true.
    line=get_sym_line(parser)
    m=0
    do
       if(expect(parser,sym_nhd)) return
       if(expr(parser)) return
       if(expect(parser,sym_of)) return
       n=0
       do
          if(expect_name(parser)) return
          call make_node(parser,sym_of,1)
          if(expect(parser,sym_in)) return
          if(expr(parser)) return
          n=n+2
          if(parser%sym/=sym_comma) exit
          call scan(parser)
          if(parser%sym==sym_nhd) then
             call push_back(parser,sym_comma)
             exit
          endif
       enddo
       call make_node(parser,list_sym,n)
       if(parser%sym==sym_bounds) then
          call scan(parser)
          if(expr(parser)) return
       else
          call make_node(parser,sym_null,0)
       endif
       call make_node(parser,sym_bounds,1)
       m=m+3
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    call make_node(parser,sym_list,m)

    ! Attributes
    if(parser%sym==sym_open_attr) then
       call scan(parser)
       if(parser%sym==sym_block) then
          call scan(parser)
          if(expect(parser,sym_define)) return
          if(expr(parser)) return
          call make_node(parser,sym_block,1)
       endif
       if(expect(parser,sym_close_attr)) return
    else
       call push_null_val(parser)
    endif

    ! Subexpressions
    call make_node(parser,sym_nhd,0)
    if(subexpr(parser)) return

    ! Blocks
    if(block_or_single_stmt(parser,sym_nhd,-1,line)) return
    
    ! Make node for nhd statement
    !  (nhd (name expr)* edges edge_default bounds)*,attr,subexp,block
    call make_node(parser,sym_nhd,4)
    iserr=.false.
  end function nhd_stmt

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
  ! List of statements
  !======================================================
  recursive subroutine stmt_list(parser,single)
    type(parse_state),intent(inout):: parser
    logical,intent(in),optional:: single
    logical:: ok
    integer:: i,n,m,k,name,sym,label,line,pos
    type(pm_ptr):: p
    k=0
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
          ! Pragma's -- start with $$
       case(sym_ddollar)
          if(pragma()) goto 999          
          ! Statements that are actually part of the language
       case(sym_if,sym_if_invar)
          if(if_stmt(parser)) goto 999
       case(sym_switch,sym_switch_invar)
          if(switch_stmt(parser)) goto 999
       case(sym_while,sym_while_invar)
          call push_null_val(parser)
          if(while_stmt(parser,0)) goto 999
       case(sym_until,sym_until_invar)
          call push_null_val(parser)
          if(until_stmt(parser,0)) goto 999
       case(sym_do)
          call push_null_val(parser)
          if(do_stmt(parser)) goto 999
       case(sym_test)
          if(test_stmt(parser)) goto 999
       case(sym_for,sym_tile,sym_conc)
          if(for_stmt(parser)) goto 999
       case(sym_each,sym_foreach_invar)
          if(for_each_stmt(parser,0)) goto 999
       case(sym_par)
          if(par_stmt(parser)) goto 999
       case(sym_any)
          if(any_stmt(parser)) goto 999
       case(sym_over)
          line=get_sym_line(parser)
          call scan(parser)
          call xexpr(parser)
          if(block_or_single_stmt(parser,sym_over,0,line)) goto 999
          call make_node(parser,sym_over,2)
       case(sym_nhd)
          if(nhd_stmt(parser)) return
       case(sym_with)
          line=get_sym_line(parser)
          call scan(parser)
          if(assn_or_call(parser,.false.,.false.,.true.)) goto 999
          if(parser%sym==sym_check.or.parser%sym==sym_where) then
             if(subexpr(parser)) goto 999
          endif
          call make_node(parser,sym_list,1)
          if(block_or_single_stmt(parser,sym_with,0,line)) goto 999
          call make_node(parser,sym_with,2)
       case(sym_underscore)
          if(assn_or_call(parser,.false.,.true.,.true.)) goto 999
          if(parser%sym==sym_check.or.parser%sym==sym_where) then
             if(subexpr(parser)) goto 999
          endif
       case(sym_var,sym_const)
          if(var_stmt(parser)) goto 999
       case(sym_coherent,sym_chan,sym_mirrored,sym_shared)
          call scan(parser)
          if(mode_stmt(parser,sym)) goto 999
       case(sym_dollar)
          if(proc_val_call()) goto 999
       case(sym_proceed)
          call scan(parser)
       case(sym_sync)
          call scan(parser)
          if(assn_or_call(parser,.false.,.true.,.false.)) goto 999
          call make_node(parser,sym_sync_assign,1)
          if(subexpr(parser)) return
       case(sym_return)
          call make_node(parser,sym_list,k)
          return
       case default
          if(check_name_pos(parser,name,line,pos)) then
             ! Labelled statements
             if(parser%sym==sym_colon) then
                call scan(parser)
                sym=parser%sym
                select case(sym)
                case(sym_while)
                   call push_sym_val(parser,name)
                   if(while_stmt(parser,name)) goto 999
                case(sym_until)
                   call push_sym_val(parser,name)
                   if(until_stmt(parser,name)) goto 999
                case(sym_each)
                   if(for_each_stmt(parser,name)) goto 999
                case(sym_do)
                   call push_sym_val(parser,name)
                   if(until_stmt(parser,name)) goto 999
                case(sym_while_invar,sym_until_invar,sym_foreach_invar,&
                     sym_if,sym_if_invar,sym_test,sym_any,&
                     sym_for,sym_tile,sym_conc,sym_par)
                   call parse_error(parser,'Cannot label a "'//trim(sym_names(sym))//'" statement')
                   goto 999
                case(sym_proceed)
                   call scan(parser)
                   call push_null_val(parser)
                   call push_sym_val(parser,name)
                   call make_node_at(parser,sym_sync,2,line,pos)
                case default
                   select case(sym)
                   case(sym_var,sym_const)
                      if(var_stmt(parser)) goto 999
                   case(sym_coherent,sym_chan,sym_mirrored,sym_shared)
                      call scan(parser)
                      if(mode_stmt(parser,sym)) goto 999
                   case(sym_sync)
                      call scan(parser)
                      if(assn_or_call(parser,.false.,.true.,.false.)) goto 999
                      call make_node(parser,sym_sync_assign,1)
                      if(subexpr(parser)) return
                   case(sym_dollar)
                      if(proc_val_call()) goto 999
                   case default
                      if(assn_or_call(parser,.true.,.true.,.true.)) goto 999
                      if(subexpr(parser)) goto 999
                   end select
                   call make_node(parser,sym_list,1)
                   call push_sym_val(parser,name)
                   call make_node_at(parser,sym_sync,2,line,pos)
                end select
             elseif(parser%sym==sym_dash) then
                call scan(parser)
                if(expect_name(parser)) goto 999
                call make_node(parser,sym_use,2)
                if(arglist(parser)) goto 999
                call make_node(parser,sym_do,1)
                if(subexpr(parser)) goto 999
             elseif(parser%sym==sym_open.or.parser%sym==sym_pct) then
                call push_name_val(parser,name)
                if(arglist(parser)) goto 999
                call make_node(parser,sym_do,1)
                if(subexpr(parser)) goto 999
             else
                call push_back_name(parser,name)
                if(assn_or_call(parser,.true.,.true.,.true.)) goto 999
                if(subexpr(parser)) goto 999
            endif
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
    elseif(parser%sym==sym_dcolon) then
       call scan(parser)
       call stmt_list(parser)
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
            parser%sym==sym_define.or.&
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
    elseif(parser%sym==sym_fix) then
       call scan(parser)
       if(typunary(parser)) return
       call make_node(parser,sym_const,1)
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
    case(sym_lt)
       call scan(parser)
       if(typ(parser)) return
       call make_node(parser,sym_type_val,1)
       if(expect(parser,sym_gt)) return
    case(sym_any)
       call scan(parser)
       call make_node(parser,sym_any,0)
    case(sym_dash)
       call scan(parser)
       if(parser%sym==sym_number) then
          if(pm_fast_vkind(parser%lexval)/=&
               pm_long) then
             call parse_error(parser,&
                  'Cannot have "''" before non-default integer constant')
          endif
          call push_val(parser,parser%lexval)
          call scan(parser)
          call make_node(parser,sym_dash,1)
       elseif(parser%sym==sym_true.or.parser%sym==sym_false) then
          call push_sym_val(parser,parser%sym)
          call scan(parser)
          call make_node(parser,sym_dash,1)
       else
          call parse_error(parser,'Expected number,"true" or "false"')
       endif
    case(sym_dollar)
       call scan(parser)
       if(op(parser,name,.true.,.true.)) return
       call push_sym_val(parser,name)
       if(parser%sym==sym_dash.and.name>num_sym) then
          call scan(parser)
          if(expect_name(parser)) return
          call make_node(parser,sym_proc,2)
       else
          call make_node(parser,sym_proc,1)
       endif
    case(sym_proc)
       call scan(parser)
       if(parser%sym==sym_open.or.parser%sym==sym_pct) then
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
    case(sym_caret)
       call scan(parser)
       select case(parser%sym)
       case(sym_shared)
          call scan(parser)
          m=pm_dref_is_shared
       case(sym_dot)
          call scan(parser)
          m=pm_dref_is_dot
       case(sym_mult)
          call scan(parser)
          m=pm_dref_is_any
       case(sym_hash)
          call scan(parser)
          if(parser%sym==sym_shared) then
             call scan(parser)
             m=pm_dref_is_shared_slice
          elseif(parser%sym==sym_mult) then
             call scan(parser)
             m=pm_dref_is_any_slice
          else
             m=pm_dref_is_slice
          endif
       case(sym_here)
          call scan(parser)
          m=pm_dref_is_here
       case(sym_pling)
          call scan(parser)
          m=pm_dref_is_ref
       case default
          m=pm_dref_is_var
       end select
       call push_num_val(parser,m)
       if(expect(parser,sym_open)) return
       if(opt_typ_list(parser,m)) return
       if(expect(parser,sym_close)) return
       call make_node(parser,sym_pm_dref,m+1)
    case(sym_dcaret)
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(typ(parser)) return
       if(expect(parser,sym_close)) return
       call make_node(parser,sym_dcaret,1)
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
       if(parser%sym==sym_dash) then
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
  !
  ! Also used in methods and method signatures
  ! For a method signature tname and params must be present
  ! For a method, tname, params and is_method must be present
  !  -- in this case parameter names are parsed and
  !     pushed onto the symbol stack
  !====================================================================
  recursive function proctyp(parser,tname,params,is_method) result(iserr)
    type(parse_state):: parser
    integer,intent(in),optional:: tname
    type(pm_ptr),intent(in),optional:: params
    logical,intent(in),optional:: is_method
    logical:: iserr
    integer:: i,base,base2,n,m,sym,npar
    logical:: iscomm
    iserr=.true.
    
    call push_null_val(parser)
    sym=parser%sym
    iscomm=sym==sym_pct
    if(iscomm) then
       call push_sym_val(parser,sym)
       call scan(parser)
       call make_node(parser,sym_any,0)
       call make_node(parser,sym_any,0)
       call make_node(parser,sym_any,0)
       m=3
    else
       call push_sym_val(parser,sym_proc)
       m=0
    endif
    if(expect(parser,sym_open)) return
    
    if(present(tname)) then
       ! First argument "this" for methods and method signatures
       ! Either T(u,v) for method or *T(u,v) for method signature
       if(present(is_method)) then
!!$          npar=node_numargs(params)
!!$          do i=1,npar,2
!!$             call push_val(parser,node_arg(params,i))
!!$             call make_node(parser,sym_type,1)
!!$          enddo
          call push_sym_val(parser,tname)
          call make_node(parser,sym_type,1)
       else
          call make_node(parser,sym_mult,0)
       endif
       m=m+1
    endif

    
    base=parser%top
    if(parser%sym/=sym_close) then
       do 
          sym=parser%sym
          m=m+1
          if(sym==sym_amp) then
             call scan(parser)
             if(present(is_method)) then
                if(expect_name(parser)) return
                call push_sym(parser,-int(parser%vstack(parser%vtop)%offset))
                call drop_val(parser)
                if(expect(parser,sym_colon)) return
             else
                call push_sym(parser,m)
             endif
             if(moded_typ(parser,iscomm,.false.)) return
          else
             if(present(is_method)) then
                if(expect_name(parser)) return
                call push_sym(parser,int(parser%vstack(parser%vtop)%offset))
                call drop_val(parser)
                if(expect(parser,sym_colon)) return
             endif
             if(moded_typ(parser,iscomm,.false.)) return
          endif
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       if(parser%sym==sym_dotdotdot) then
          call scan(parser)
          call make_node(parser,sym_dotdotdot,m)
          if(present(is_method)) call push_sym(parser,sym_dotdotdot)
       else
          call make_node(parser,sym_list,m)
          if(present(is_method)) call push_sym(parser,sym_list)
       endif
    else
       call make_node(parser,sym_list,m)
       if(present(is_method)) call push_sym(parser,sym_list)
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
    if(present(is_method)) then
       base2=parser%top
       do i=base+1,base2-1
          if(parser%stack(i)<0) then
             call push_sym(parser,i)
          endif
       enddo
    else
       base2=base
    endif
    if(parser%top>base2) then
       call name_vector(parser,base2)
    else
       call push_null_val(parser)
    endif
    call make_node(parser,sym_proc,5)
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
  ! Parameter list for procedure declaration
  !======================================================
  recursive function param_list(parser,iscomm,param_base) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: iscomm
    integer,intent(in),optional:: param_base
    logical:: iserr
    integer:: m,n,i,base,last,vbase,sym,name,numloop
    type(pm_ptr):: temp,dom
    base=parser%top
    iserr=.true.
    m=0
    n=0
    
    ! For communicating procedures implicit "region" and "subregion" parameters
    if(iscomm) then
       call push_sym_val(parser,sym_region)
       if(parser%sym==sym_region) then
          call scan(parser)
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
          if(parser%sym/=sym_close) then
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
          if(parser%sym/=sym_close) then
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
          if(parser%sym/=sym_close) then
             if(expect(parser,sym_comma)) return
          endif
       else
          call push_null_val(parser)
       endif
       m=3
    endif

    ! For methods, the parameter list is coded on the stack
    if(present(param_base)) then
       last=parser%top
       call push_sym_val(parser,sym_this)
       call push_null_val(parser)
       m=m+1
       do i=param_base+1,last-1
          call push_sym_val(parser,abs(parser%stack(i)))
          call push_null_val(parser)
          m=m+1
          if(parser%stack(i)<0) call push_sym(parser,m)
       enddo
       call make_node(parser,parser%stack(last),m*2)
       call push_null_val(parser)
       if(parser%top>base) then
          call name_vector(parser,base)
       else
          call push_null_val(parser)
       endif
       iserr=.false.
       return
    endif

    ! Empty argument list
    if(parser%sym==sym_close) then
       call make_node(parser,sym_list,m*2)
       call push_null_val(parser)
       call push_null_val(parser)
       call scan(parser)
       iserr=.false.
       return
    endif

    ! Standard (non-keyword) arguments
    do
       if(parser%sym==sym_arg) then
          call scan(parser)
          if(expect(parser,sym_dotdotdot)) return
          call push_sym_val(parser,sym_arg)
          if(arg_type_with_mode(iscomm)) return
          call make_node(parser,sym_dotdotdot,m*2+2)
          exit
       else if(parser%sym==sym_amp) then
          call scan(parser)
          if(expect_name(parser,'argument name')) return
          if(parser%sym==sym_define) then
             call parse_error(parser,'Cannot have "=" after "&name"')
             return
          endif
          m=m+1
          call push_sym(parser,m)
          if(arg_type_with_mode(iscomm)) return
       else if(parser%sym==sym_key) then
          call make_node(parser,sym_list,m*2)
          call push_back(parser,sym_comma)
          exit
       else
          if(check_name(parser,name)) then
             if(parser%sym==sym_define) then
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
          if(arg_type_with_mode(iscomm)) return
          if(parser%sym==sym_define) then
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
          if(parser%sym==sym_key) then
             call scan(parser)
             if(expect(parser,sym_dotdotdot)) return
             call make_node(parser,sym_dotdotdot,n*3)
             exit
          else
             if(expect_name(parser, &
                  'optional argument name')) return
             if(arg_type_with_mode(iscomm)) return
             if(expect(parser,sym_define)) return
             if(expr(parser)) return
             n=n+1
          endif
          if(parser%sym/=sym_comma) then
             call make_node(parser,sym_list,n*3)
             exit
          endif
       enddo
    else
       if(n>0) then
          call make_node(parser,sym_list,n*3)
       else
          call push_null_val(parser)
       endif
    endif
    
    if(expect(parser,sym_close)) return
    if(parser%top>base) then
       call name_vector(parser,base)
    else
       call push_null_val(parser)
    endif
    iserr=.false.
    return
  contains

    function arg_type_with_mode(iscomm) result(iserr)
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
    end function arg_type_with_mode

  end function param_list

  !======================================================
  ! Procedure/call attributes
  !======================================================
  recursive function proc_call_attr(parser,iscall,flags) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: iscall
    integer,intent(inout):: flags
    logical:: iserr
    logical:: iscomm
    integer:: m
    iserr=.true.
    call scan(parser)
    iscomm=iand(flags,proc_is_comm)/=0
    do
       select case(parser%sym) 
       case(sym_each)
          if(iscall) then
             call bad_attr
             exit
          endif
          call scan(parser)
          if(expect(parser,sym_open)) return
          m=0
          do
             if(expect_name(parser)) return
             m=m+1
             if(parser%sym/=sym_comma) exit
             call scan(parser)
          enddo
          call make_node(parser,sym_each,m)
          if(expect(parser,sym_close)) return
          flags=ior(flags,proc_is_each_proc)
       case(sym_shared)
          if(iscomm.eqv.iscall) then
             call bad_attr
          endif
          call scan(parser)
          flags=ior(flags,proc_run_shared)
       case(sym_pm_node)
          if(iscomm.eqv.iscall) then
             call bad_attr
          endif
          call scan(parser)
          flags=ior(flags,proc_run_local+proc_run_always)
       case(sym_complete)
          if(iscomm.eqv.iscall) then
             call bad_attr
          endif
          call scan(parser)
          flags=ior(flags,proc_run_complete)
       case(sym_always)
          if((.not.iscomm).and.(.not.iscall)) then
             call bad_attr
          endif
          call scan(parser)
          flags=ior(flags,proc_run_always)
       case(sym_inline)
          call scan(parser)
          flags=ior(flags,proc_inline)
       case(sym_no_inline)
          call scan(parser)
          flags=ior(flags,proc_no_inline)
       case(sym_cond_attr)
          call scan(parser)
          if(iscall.or..not.iscomm) call bad_attr
          flags=ior(flags,proc_is_cond)
       case(sym_uncond)
          call scan(parser)
          if(iscall.or..not.iscomm) call bad_attr
          flags=ior(flags,proc_is_uncond)
       case(sym_ignore_rules)
          call scan(parser)
          flags=ior(flags,call_ignore_rules)
       end select
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    if(iand(flags,proc_inline+proc_no_inline)==&
         proc_inline+proc_no_inline) then
       call parse_error(parser,&
            'Cannot have both "<<inline>>" and "<<no_inline>>" attributes together')
    endif
    if(iand(flags,proc_is_cond+proc_is_uncond)==&
         proc_is_cond+proc_is_uncond) then
       call parse_error(parser,&
            'Cannot have both "<<cond>>" and "<<uncond>>" attributes together')
    endif
    if(expect(parser,sym_close_attr)) return
    iserr=.false.
  contains
    subroutine bad_attr
      if(iscall) then
         call parse_error(parser,&
              'Cannot have "'//trim(sym_names(parser%sym))//&
              '" attribute in a communicating call') 
      else
         call parse_error(parser,&
              'Cannot have "'//trim(sym_names(parser%sym))//&
              '" attribute in a non-communicating procedure') 
      endif
    end subroutine bad_attr
  end function proc_call_attr
  
  !======================================================
  ! Procedure declaration
  !======================================================
  function proc_decl(parser,method_name,param_base) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in),optional:: method_name,param_base
    logical:: iserr
    type(pm_ptr),target::ptr,dom,dparams,rtypes
    type(pm_ptr):: p,params,link
    type(pm_reg),pointer:: reg
    integer:: name,callname,this,thispar
    integer:: nret,base,flags,sbase,scount,m,nreduce,sym
    integer:: line,pos,nerrors
    logical:: ampargs,iscall,iscomm,isshared,islocal,ischan,ismethod,have_rtn
    nerrors=parser%error_count
    reg=>pm_register(parser%context,'proc',ptr,dom,dparams,rtypes)
    iserr=.true.
    ismethod=present(method_name)
    sym=sym_proc
    nret=0
    sbase=parser%vtop
    scount=parser%error_count
    dom=pm_null_obj
    dparams=pm_null_obj
    thispar=-1

    ! Procedure name
    if(ismethod) then
       name=method_name
    else
       call scan(parser)
       if(.not.check_name(parser,name)) then
          if(op(parser,name,.false.,.false.)) goto 999
       endif
    endif

    ! Line and position of procedure start
    call get_sym_pos(parser,line,pos)
    
    ! Communicating proc flag
    iscomm=.false.
    if(parser%sym==sym_pct) then
       call scan(parser)
       iscomm=.true.
    endif

    ! Start of parameters
    if(.not.present(param_base)) then
       if(expect(parser,sym_open)) goto 999
    endif
   
10  continue

    ! Create fully qualified (module!name) procedure name
    if(ismethod) then
       call push_sym_val(parser,name)
    else
       call make_qualified_name(parser,name)
    endif

    ! Start of procedure delaration node
    base=parser%vtop
    call push_val(parser,top_val(parser)) ! name

    ! Link procedure into list for given name (if already exists)
    ptr=decl_entry(parser,name,modl_proc,link)
    call push_val(parser,link)

    ! Push some more entries in the procedure node (some get values later)
    call push_val(parser,parser%modl)
    call push_num_val(parser,-12345)      ! flags
    if(param_list(parser,iscomm,param_base)) goto 999
    params=parser%vstack(parser%vtop-2)
    call push_num_val(parser,-777)        ! coded params
    call push_num_val(parser,-777)        ! coded returns
    call push_num_val(parser,-777)        ! coded type
    call push_num_val(parser,-777)        ! nret

    ! Start computing flags for this procedure node
    if(iscomm) then
       flags=proc_is_comm
    else
       flags=0
    endif

    ! Return types ->(typelist)
    if(parser%sym==sym_arrow) then
       call scan(parser)
       if(expect(parser,sym_open)) return
       if(parser%sym==sym_close) then
          nret=0
       else
          if(moded_typ_list(parser,iscomm,nret)) return
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

    ! ... flags extensibility beyond module
    if(parser%sym==sym_dotdotdot) then
       call scan(parser)
       flags=ior(flags,proc_is_open)
    endif

    ! Attributes
    if(parser%sym==sym_open_attr) then
       if(proc_call_attr(parser,.false.,flags)) goto 999
       if(iand(flags,proc_is_each_proc)==0) then
          call push_null_val(parser)
       endif
    else
       call push_null_val(parser)
    endif
    
    ! = expr or  [ check expr ] block
    if(parser%sym==sym_define.and.nret==-1) then
       
       call push_null_val(parser)
       call scan(parser)
       m=0
       do
          if(expr(parser)) goto 999
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

    if(parser%error_count>scount) then
       parser%vtop=sbase
       goto 999
    endif
    if(parser%error_count>0) goto 999
  
    ! Assign flags to proc_flags slot
    parser%vstack(parser%vtop-&
         proc_num_args-node_args+proc_flags+1)%offset=flags
    
    ! Assign number of returns to proc_numret slot
    parser%vstack(parser%vtop-&
         proc_num_args-node_args+proc_numret+1)%offset=nret
    
    if(pm_debug_checks) then
       if(parser%vtop-base/=proc_num_args) then
          write(*,*) '========='
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
    
    if(debug_parser_extra) then
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
    
    function return_stmt() result(iserr)
      logical:: iserr
      integer:: m
      iserr=.true.
      call scan(parser)
      m=0
      do
         if(expr(parser)) return
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
  recursive function proc_sig(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: m,n,base,name,sym
    type(pm_ptr):: temp
    base=parser%top
    iserr=.true.
    m=0
    n=0
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
       call make_node(parser,sym_list,0)
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
             if(parser%sym==sym_define) then
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
          else if(parser%sym==sym_arg) then
             call push_sym_val(parser,sym_arg)
             call scan(parser)
             if(expect(parser,sym_dotdotdot)) return
             if(expect(parser,sym_colon)) return
             if(moded_typ(parser,.true.,.false.)) return
             call make_node(parser,sym_dotdotdot,m*2)
             exit
          else if(parser%sym==sym_key) then
             call make_node(parser,sym_list,m*2)
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
          if(parser%sym==sym_key) exit
          if(typ(parser)) return
          if(expect_name(parser,&
               'optional parameter name')) return
          if(expect(parser,sym_define)) return
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
    call push_num_val(parser,-1) ! Coded params
    call push_num_val(parser,-1) ! Coded returns
    call push_num_val(parser,-1) ! Coded type

    ! Special forms of return type which compute it based on arguments
    if(parser%sym==sym_arrow) then
       call scan(parser)
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
       case(sym_pct,sym_define,sym_dot,sym_query,sym_amp,&
            sym_hash,sym_caret,sym_dcaret,sym_d1:sym_d7,sym_invar,sym_shared,&
            sym_type)
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
       case(sym_dash)
          call scan(parser)
          if(typ_list(parser,m)) return
          call make_node(parser,sym_result,m)
          call make_node(parser,sym_dash,1)
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
    else
       call push_num_val(parser,0)
       call push_null_val(parser)
       call push_null_val(parser)
    endif
    iserr=.false.
    return
  end function proc_sig

  !======================================================
  ! Built in procedure definition
  !======================================================
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
       flags=ior(pflags,call_is_comm)
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
    if(proc_sig(parser)) return        
    call push_num_val(parser,int(opcode))
    call push_num_val(parser,int(opcode2))
    call push_val(parser,pdata)
    call push_null_val(parser)
    call make_node(parser,sym_builtin,sysproc_num_args)
    if(debug_parser_extra) then
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
  function type_decl(parser) result(iserr)
    type(parse_state):: parser
    logical:: iserr
    integer:: sym,m,n,name,basename,namein,base,nextra
    type(pm_ptr),target:: ptr
    type(pm_reg),pointer:: reg
    type(pm_ptr):: params,p,link
    integer:: sbase,svbase,pname
    sbase=parser%top
    svbase=parser%vtop
    reg=>pm_register(parser%context,'type_decl',ptr)
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
    if(type_params(parser,m)) goto 999   
    params=top_val(parser)

    ! <: typelist
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
    if(type_inclusions(parser,name)) goto 999

    ! Body of declaration, either :
    !    struct or rec
    !    unique
    !    list of types
    if(parser%sym==sym_is) then
       sym=sym_is
       call scan(parser)
       if(parser%sym==sym_struct.or.parser%sym==sym_rec) then
          if(structrec(parser,params,basename,name,m)) goto 999
          call make_node(parser,sym_list,1)
          m=1
       elseif(parser%sym==sym_unique) then
          if(unique(parser,name)) goto 999
          m=1
       elseif(parser%sym==sym_interface) then
          call push_null_val(parser)
          if(interface(parser,name,params)) goto 999
          m=0
          nextra=1
          sym=sym_interface
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
    call make_node(parser,sym,typ_num_args+nextra)
    if(debug_parser_extra) then
       write(*,*) 'TYPEDECL>----------------'
       call dump_parse_tree(parser%context,6,top_val(parser),2)
       write(*,*) 'END TYPEDECL-------------'
    endif
    call add_type_decl(parser,name,ptr)
    iserr=.false.
999 continue
    parser%top=sbase
    parser%vtop=svbase
    call pm_delete_register(parser%context,reg)
  contains

    include 'fisnull.inc'

  end function type_decl

  !======================================================
  ! Parameters to a type declaration
  !======================================================
  function type_params(parser,m) result(iserr)
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
  end function type_params


  !======================================================
  ! : typelist
  !======================================================
  function type_inclusions(parser,name) result(iserr)
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
             call add_typein_decl(parser,namein,name)
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
  end function type_inclusions

  !======================================================
  ! Add a declaration that type 'namein' is
  ! included in type 'name'
  !======================================================
  subroutine add_typein_decl(parser,namein,name)
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
    call add_type_decl(parser,namein,ptrin)
  contains
    include 'fisnull.inc'
  end subroutine add_typein_decl

  !======================================================
  ! Add type declaration on top of vstack under name nam
  !======================================================
  subroutine add_type_decl(parser,nam,p)
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
  end subroutine add_type_decl

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
  recursive function structrec(parser,params,basename,typname,nargs) result(iserr)
    type(parse_state),intent(inout):: parser
    type(pm_ptr),intent(in):: params
    integer,intent(in):: basename,typname,nargs
    logical:: iserr
    integer:: i,tag,name,sym,base,vbase,line,pos,n,flags
    logical:: hasuse
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
    if(parser%sym==sym_caret) then
       call scan(parser)
       flags=pm_typ_is_soa
    endif
    if(parser%sym==sym_open_brace) then
       call scan(parser)
    else
       if(parser%sym==sym_dotdotdot) then
          call scan(parser)
       else
          call parse_error(parser,&
               'Expected "{" or "..."')
       endif
       if(expect(parser,sym_open_brace)) return
    endif
    call push_sym(parser,tag)
    hasuse=.false.
    n=0
    do
       if(parser%sym==sym_use) then
          call scan(parser)
          if(check_name_no_repeat(parser,name,base+1)) then
             call push_sym(parser,-name)
             hasuse=.true.
          else
             call parse_error(parser,&
                  'Expected name of '//sym_names(sym)//' element')
             return
          endif
       elseif(parser%sym==sym_proc) then
          if(method(parser,typname,params,base)) return
          n=n+1
          if(parser%sym/=sym_comma) exit
          call scan(parser)
          cycle
       elseif(check_name_no_repeat(parser,name,base+1)) then
          call push_sym(parser,name)
       else
          call parse_error(parser,&
               'Expected name of '//sym_names(sym)//' element')
          return
       endif
       n=n+1
       if(parser%sym==sym_colon) then
          call scan(parser)
          if(typ(parser)) return
       else
          call push_null_val(parser)
       endif
       if(parser%sym==sym_define) then
          call scan(parser)
          if(expr(parser)) return
          call make_node(parser,sym_define,2)
       endif
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    
    ! Structure definition record is: list{type},tag,typname,nargs,params,keys
    call make_node(parser,sym_list,parser%vtop-vbase)
    call name_vector(parser,base)
    !if(hasuse) parser%vstack(parser%vtop)%offset=-parser%vstack(parser%vtop)%offset
    call push_num_val(parser,tag)
    call push_num_val(parser,nargs)
    call push_val(parser,params)
    call push_null_val(parser)
    if(hasuse) flags=ior(flags,pm_typ_has_embedded)
    call push_num_val(parser,flags)
    call make_node(parser,sym,7)
    if(expect(parser,sym_close_brace)) return
    iserr=.false.
  contains
    include 'fisnull.inc'
  end function structrec

  !======================================================
  ! Method definition proc name(...) { ... }
  ! in struct, rec or interface
  !======================================================
  recursive function method(parser,typname,params,base) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: typname,base
    type(pm_ptr),intent(in):: params
    logical:: iserr
    type(pm_ptr):: p
    integer:: mname,name,tname,pbase
    logical:: iscomm
    iserr=.true.
    call scan(parser)
    if(check_name_no_repeat(parser,name,base+1)) then
       p=parser%modl%data%ptr(parser%modl%offset+modl_name)
       mname=-p%offset
       call push_sym(parser,mname)
       call push_sym(parser,typname)
       call push_sym(parser,name)
       call name_vector(parser,parser%top-3)
       tname=parser%vstack(parser%vtop)%offset
       parser%vtop=parser%vtop-1
       pbase=parser%top
       iscomm=parser%sym==sym_pct
       if(proctyp(parser,typname,params,.true.)) return
       if(iscomm) call push_back(parser,sym_pct)
       if(parser%sym/=sym_open_brace.and.parser%sym/=sym_colon) then
          if(expect(parser,sym_open_brace)) return
       endif
       if(proc_decl(parser,tname,pbase)) return
       call push_sym_val(parser,tname)
       call make_node(parser,sym_proc,1)
       call make_node(parser,sym_define,2)
       parser%top=pbase
       call push_sym(parser,name)
    else
       call parse_error(parser,'Expected method name')
       return
    endif
    iserr=.false.
  end function method

  !======================================================
  ! interface { ... }
  !======================================================
  recursive function interface(parser,tname,params) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: tname
    type(pm_ptr),intent(in):: params
    logical:: iserr
    logical isvar
    integer:: i,m,base,vbase,name,line,pos
    type(pm_ptr):: tag
    call get_sym_pos(parser,line,pos)
    iserr=.true.
    call scan(parser)
    if(expect(parser,sym_open_brace)) return
    base=parser%top
    vbase=parser%vtop
    call make_qualified_name(parser,tname)
    tag=pop_val(parser)
    call push_sym(parser,int(tag%offset))
    do
       if(parser%sym==sym_proc) then
          call scan(parser)
          if(check_name_no_repeat(parser,name,base+1)) then
             call push_sym(parser,name)
          else
             call parse_error(parser,'Expected method name')
             return
          endif
          if(proctyp(parser,tname,params)) return
       else
          isvar=parser%sym==sym_var
          if(isvar.or.parser%sym==sym_const) call scan(parser)
          if(check_name_no_repeat(parser,name,base+1)) then
             call push_sym(parser,merge(-name,name,isvar))
           else
             call parse_error(parser,'Expected element name')
             return
          endif
          if(expect(parser,sym_colon)) return
          if(typ(parser)) return
       endif
       if(parser%sym==sym_close_brace) exit
       if(expect(parser,sym_comma)) return
    enddo
    call make_node(parser,sym_list,parser%vtop-vbase)
    call name_vector(parser,base)
    if(expect(parser,sym_close_brace)) return
    call make_node_at(parser,sym_interface,2,line,pos)
    iserr=.false.
  end function interface


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
       if(expect(parser,sym_define)) return
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
    integer:: serror
    logical:: ok
    call push_sym_val(parser,sym_pm_system)
    call push_val(parser,parser%sysmodl)
    call push_null_val(parser)
    call make_node(parser,sym_use,3)
    call new_import(parser,sym_pm_system,pop_val(parser))
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
    do
       select case(parser%sym)
       case(sym_proc)
          call scan(parser)
          if(parser%sym==sym_open) then
             call push_back(parser,sym_proc)
             exit
          else
             call push_back(parser,sym_proc)
          endif
          if(proc_decl(parser)) goto 999
       case(sym_type)
          if(type_decl(parser)) goto 999
       case(sym_param)
          if(param_decl(parser)) goto 999
       case(sym_test)
          if(test_stmt(parser)) goto 999
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
       call stmt_list(parser)
    elseif(parser%sym/=sym_eof) then
          call parse_error(parser,&
               'Library module cannot contain non-"debug" statement')
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
    if(debug_parser_extra) write(*,*) 'Skipping',expr
    do
       if(expr) then
          if(parser%sym<=num_sym.and.parser%sym>last_expr) then
             call skip_past_error(parser,.false.)
             exit
          endif
          do while(parser%sym/=sym_eof.and.(parser%sym>num_sym.or.&
               parser%sym<=last_expr))
             if(parser%sym==sym_define) then
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
    if(debug_parser_extra) write(*,*) 'Skipped'
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
    
    if(debug_parser_extra) then
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
    if(debug_parser_extra) then
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
       if(pm_main_process.and.name/=sym_pm_system) then
          call pm_name_string(parser%context,&
            int(nameval%offset),str)
          call pm_module_filename(str,str2)
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
  ! Check that a node is valid
  !======================================================
  subroutine check_node(node)
    type(pm_ptr),intent(in):: node
    if(pm_fast_vkind(node)==pm_pointer) then
       if(node%data%ptr(node%offset)%offset/=9876) then
          call pm_panic('Bad parse node')
       endif
    endif
  contains
    include 'fvkind.inc'
  end subroutine check_node

  !=======================================================
  ! Check that a node is valid and not a tiny int or value
  !=======================================================
  subroutine check_ptr_node(node)
    type(pm_ptr),intent(in):: node
    if(pm_fast_vkind(node)==pm_pointer) then
       if(node%data%ptr(node%offset)%offset/=9876) then
          call pm_panic('Bad parse node')
       endif
    else
       call pm_panic('not ptr parser node')
    endif
  contains
    include 'fvkind.inc'
  end subroutine check_ptr_node

  !======================================================
  ! Return symbol associated with a node
  !======================================================
  function node_sym(node) result(n)
    type(pm_ptr),intent(in):: node
    integer:: n
    if(pm_fast_vkind(node)/=pm_pointer) then
       n=0
    else
       if(pm_debug_checks) call check_node(node)
       n=node%data%ptr(node%offset+node_symbol)%offset
    endif
  contains
    include 'fvkind.inc'
  end function node_sym

  !======================================================
  ! Number of arguments in a node
  !======================================================
  function node_numargs(node) result(n)
    type(pm_ptr),intent(in):: node
    integer:: n
    if(pm_fast_vkind(node)/=pm_pointer) then
       n=0
    else
       if(pm_debug_checks) call check_node(node)
       n=pm_fast_esize(node)-node_args+1
    endif
  contains
    include 'fesize.inc'
    include 'fvkind.inc'
  end function node_numargs

  !======================================================
  ! Return n-th argument of a node
  !======================================================
  function node_arg(node,n) result(p)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    type(pm_ptr):: p
    if(pm_debug_checks) then
       call check_ptr_node(node)
       if(n<0.or.node_args+n-1>pm_fast_esize(node)) &
            call pm_panic('node_arg - n out of range')
    endif
    p=node%data%ptr(node%offset+node_args+n-1)
  contains
    include 'fesize.inc'
  end function node_arg

  !======================================================
  ! Return n-th argument of a node as a number
  ! (that argument should be tiny-int)
  !======================================================
  function node_num_arg(node,n) result(num)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    integer:: num
    type(pm_ptr):: p
    if(pm_debug_checks) then
       call check_ptr_node(node)
       if(n<0.or.node_args+n-1>pm_fast_esize(node)) &
            call pm_panic('node_arg - n out of range')
    endif
    p=node%data%ptr(node%offset+node_args+n-1)
    num=p%offset
  contains
    include 'fesize.inc'
  end function node_num_arg

  !======================================================
  ! Return n-th slot in a node (not the same as argument)
  !======================================================
  function node_get(node,n) result(p)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    type(pm_ptr):: p
    if(pm_debug_checks) then
       call check_ptr_node(node)
       if(n<0.or.n>pm_fast_esize(node)) &
            call pm_panic('node_get - n out of range')
    endif
    p=node%data%ptr(node%offset+n)
  contains
    include 'fesize.inc'
  end function node_get

  !======================================================
  ! Return n-th slot in a node (not the same as argument)
  ! as a number (must be tiny int)
  !======================================================
  function node_get_num(node,n) result(num)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    integer:: num
    type(pm_ptr):: p
    if(pm_debug_checks) then
       call check_ptr_node(node)
       if(n<0.or.n>pm_fast_esize(node)) &
            call pm_panic('node_get_num - n out of range')
    endif
    p=node%data%ptr(node%offset+n)
    num=p%offset
  contains
    include 'fesize.inc'
  end function node_get_num

  !======================================================
  ! Set n-th slot in a node (not the same as argument)
  ! to a number (tiny int)
  !======================================================
  subroutine node_set_num(node,n,num)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    integer,intent(in):: num
    if(pm_debug_checks) then
       call check_ptr_node(node)
       if(n<0.or.n>pm_fast_esize(node)) &
            call pm_panic('node_get_num - n out of range')
    endif
    node%data%ptr(node%offset+n)%offset=num
  contains
    include 'fesize.inc'
  end subroutine node_set_num

  !======================================================
  ! Get the line number associated with a node
  !======================================================
  function node_get_lineno(node) result(n)
    type(pm_ptr),intent(in):: node
    integer:: n
    if(pm_debug_checks) &
         call check_ptr_node(node)
    n=node%data%ptr(node%offset+node_lineno)%offset
  end function node_get_lineno

  !======================================================
  ! Get the character position (in source) associated
  ! with a node
  !======================================================
  function node_get_charno(node) result(n)
    type(pm_ptr),intent(in):: node
    integer:: n
    if(pm_debug_checks) &
         call check_ptr_node(node)
    n=node%data%ptr(node%offset+node_charno)%offset
  end function node_get_charno

  !======================================================
  ! Get the module object associated with a node
  !======================================================
  function node_get_modl(node) result(modl)
    type(pm_ptr),intent(in):: node
    type(pm_ptr):: modl
    if(pm_debug_checks) &
       call check_ptr_node(node)
    modl=node%data%ptr(node%offset+node_modl)
  contains
    include 'fvkind.inc'
  end function node_get_modl

  !======================================================
  ! Get the module name associated with a node
  !======================================================
  function node_get_modl_name(node) result(name)
    type(pm_ptr),intent(in):: node
    integer:: name
    type(pm_ptr):: modl
    if(pm_debug_checks) &
       call check_ptr_node(node)
    modl=node_get_modl(node)
    name=modl%data%ptr(modl%offset+modl_name)%offset
  end function node_get_modl_name


  !======================================================
  ! Dump a module (debugging)
  !======================================================
  subroutine dump_module(context,iunit,ptr)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit
    type(pm_ptr),intent(in):: ptr
    character(len=100):: str
    character(len=7),dimension(modl_include:modl_param):: dnames = &
         (/ &
         'include',&
         'proc   ',&
         'type   ',&
         'param  '/)
    integer:: i,j,k,m
    type(pm_ptr):: keys,vals,p
    call pm_name_string(context,int(ptr%data%ptr(ptr%offset+1)%offset),str)
    write(iunit,*) 'Module: ',trim(str)
    write(iunit,*) 'Stmts:'
    call dump_parse_tree(context,iunit,ptr%data%ptr(ptr%offset+modl_stmts),2)
    do k=0,modl_local,modl_local
       if(k==modl_local) then
          write(iunit,*) 'Local:'
          m=modl_proc
       else
          ! m=modl_include
          m=modl_proc
       endif
       do j=m,modl_param
          write(iunit,*) dnames(j),&
               marked(ptr%data%ptr(ptr%offset+j+k)),'::'
          keys=pm_dict_keys(context,ptr%data%ptr(ptr%offset+j+k))
          vals=pm_dict_vals(context,ptr%data%ptr(ptr%offset+j+k))
          write(iunit,*) marked(keys),marked(vals)
          do i=1,pm_dict_size(context,ptr%data%ptr(ptr%offset+j+k))
             call pm_name_string(context,&
                  int(keys%data%ptr(keys%offset+i-1)%offset),str)
             write(iunit,*) ' ',trim(str),'::'
             write(iunit,*) marked(vals%data%ptr(vals%offset+i-1))
             p=vals%data%ptr(vals%offset+i-1)
             call dump_parse_tree(context,iunit,&
                  p%data%ptr(p%offset),2)
          enddo
       enddo
    enddo
  end subroutine dump_module

  !======================================================
  ! Dump a parser tree (debugging)
  !======================================================
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
       if(ptr%data%ptr(ptr%offset)%offset/=9876) then
          if(ptr%data%ptr(ptr%offset)%offset==9875) then
             write(iunit,*) spaces(1:depth*2),'REUSED NODE',&
                  ptr%offset,ptr%data%hash,ptr%data%esize
          else
             write(iunit,*) spaces(1:depth*2),'INVALID NODE'
             return
          endif
       endif
       sym=ptr%data%ptr(ptr%offset+1)%offset
       if(sym>0.and.sym<=num_syshook) then
          write(iunit,*) spaces(1:depth*2),sym_names(sym),ptr%data%esize,&
               'line',node_get_lineno(ptr),&
               'Marked:',marked(ptr),&
               ptr%data%hash,ptr%offset,ptr%offset+ptr%data%esize
       else if(sym==0) then
          call pm_name_string(context,int(ptr%data%ptr(ptr%offset+1)%offset),str)
          write(iunit,*) spaces(1:depth*2),'Module: ',trim(str)
          return
       else
          write(iunit,*) spaces(1:depth*2),'???',trim(pm_name_as_string(context,sym))
          return
       endif
       do i=node_args,ptr%data%esize
          call dump_parse_tree(context,iunit,ptr%data%ptr(ptr%offset+i),&
               depth+1)
       enddo
    else if(pm_fast_isnull(ptr)) then
       write(iunit,*) spaces(1:depth*2),'NULL'
    else if(pm_fast_isname(ptr)) then
       call pm_name_string(context,int(ptr%offset),str)
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
       if(debug_parser_extra) write(*,*) '*****Error::',trim(emess)
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
          write(*,'(3X,A,A67,A)') pm_error_start,caret,pm_error_end
       else
          write(*,'(3X,A67)') caret
       endif
10     continue
       write(*,'(A,A)') trim(pm_opts%error),trim(emess)
    endif
    parser%error_count=parser%error_count+1
    if(parser%error_count>max_errors) then
       call pm_stop('Too many syntax errors - compilation terminated')
    endif
  end subroutine parse_error

  
end module pm_parser
