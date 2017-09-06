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

! Lexical analysis and Parser
! Builds a parse tree from input text
module pm_parser
  use pm_sysdep
  use pm_compbase
  use pm_kinds
  use pm_memory
  use pm_lib
  use pm_symbol
  implicit none
  
  ! Print out lots of parser debugging info
  logical,parameter:: parser_xtra_debug=.false.

  ! Check if memory manager attempts to reuse a node
  ! (this should not happen so is one test of gc)
  logical,parameter:: check_node_reuse=.false.

  ! Maximum arguments to a PM procedure
  integer,parameter:: pm_max_args=32
  
  ! Offsets into module objects
  integer,parameter:: modl_name=1
  integer,parameter:: modl_link=2
  integer,parameter:: modl_stmts=3
  integer,parameter:: modl_include=4
  integer,parameter:: modl_proc=5
  integer,parameter:: modl_type=6
  integer,parameter:: modl_tag=7
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
  integer,parameter:: proc_coded_params=node_args+7
  integer,parameter:: proc_numret=node_args+8

  ! Alternative final sections for 'proc' parse nodes
  
  !   - user functions
  integer,parameter:: proc_reduce=node_args+9
  integer,parameter:: proc_check=node_args+10
  integer,parameter:: proc_result=node_args+11
  integer,parameter:: proc_stmts=node_args+12
  integer,parameter:: proc_code_tree=node_args+13
  integer,parameter:: proc_num_args=14

  !   - built in functions
  integer,parameter:: proc_rettypes=node_args+9
  integer,parameter:: proc_retas=node_args+10
  integer,parameter:: proc_opcode=node_args+11
  integer,parameter:: proc_opcode2=node_args+12
  integer,parameter:: proc_data=node_args+13
  integer,parameter:: proc_coded_builtin=node_args+14
  integer,parameter:: proc_ftn_code=node_args+15
  integer,parameter:: proc_ftn_action=node_args+16
  integer,parameter:: sysproc_num_args=17

  ! Flags for proc objects
  integer,parameter:: proc_is_reduce=1
  integer,parameter:: proc_is_loop_proc=2
  integer,parameter:: proc_has_vkeys=4
  integer,parameter:: proc_is_var=8
  
  integer,parameter:: max_string=100
  integer,parameter:: max_line=2001
  integer,parameter:: max_parse_stack = 1024
  integer,parameter:: max_errors=20


  ! Priority for operators (-ve is non-associative)
  integer,parameter,dimension(sym_open:last_opr):: priority= (/ &
       20, &  !sym_open
       20, &  !sym_close
       5, &   !sym_uminus
       -8,&   !sym_from_range
       -8,&   !sym_to_range
       17,&   !sym_concat 
       -12,&  !sym_eq 
       -12,&  !sym_ne 
       -12,&  !sym_ge 
       -12,&  !sym_gt 
       -12,&  !sym_le 
       -12,&  !sym_lt 
       7,&    !sym_plus 
       7,&    !sym_minus 
       4,&    !sym_mult 
       4,&    !sym_divide 
       3,&    !sym_pow 
       -8,&   !sym_dotdot 
       -18,&  !sym_cond
       2,&    !sym_bar
       16,&   !sym_hash
       -18,&  !sym_dbar
       -12, & !sym_in
       14,&   !sym_and 
       15,&   !sym_or 
       10,&   !sym_dim 
       -9,&   !sym_by 
       -12,&  !sym_includes
       6,&    !sym_mod
       13, &  !sym_not
       -8&    !sym_uby
       /)
  
  ! Parser state
  type parse_state
     type(pm_context),pointer:: context
     type(pm_ptr):: modl,modls,modl_dict,sysmodl,structs
     character(len=max_line),dimension(2):: line
     integer:: ls,lineno,sym_lineno
     logical:: newline,atstart
     integer:: n, sym_n, last, iunit
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
         parser%temp,parser%sysmodl,parser%lexval,parser%structs, &
         array=parser%vstack, &
         array_size=parser%vtop)
    parser%modl_dict=pm_dict_new(context,128_pm_ln)
    parser%modl=pm_null_obj
    parser%modls=pm_null_obj
    parser%structs=pm_dict_new(context,128_pm_ln)
    parser%vtop=0
  end subroutine init_parser
  
  ! Terminate the PM parser
  subroutine term_parser(parser)
    type(parse_state),intent(inout):: parser
    call pm_delete_register(parser%context,parser%reg)
  end subroutine term_parser
  
  ! Start a module declaration with the given name
  subroutine dcl_module(parser,name)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: name
    call new_modl(parser,pm_name_entry(parser%context,name))
    parser%modl=parser%modls
    parser%modls=parser%modls%data%ptr(parser%modls%offset+modl_link)
  end subroutine dcl_module

  ! Declare an internally implemented procedure 
  subroutine dcl_proc(parser,def,opcode,opcode2,line,flags,ftn_code,ftn_action)
    type(parse_state),intent(inout):: parser
    character(len=*),intent(in):: def
    integer(pm_i16),intent(in):: opcode,opcode2
    integer,intent(inout):: line
    integer,intent(in):: flags
    character(len=*),intent(in):: ftn_code
    integer,intent(in):: ftn_action

    call parse_from_string(parser,def)
    parser%lineno=line
    if(pm_debug_level>1) &
         write(*,*) 'Parse intrinsic def(',line,') ',trim(def)
    line=line+1
    if(builtin(parser,opcode,opcode2,pm_null_obj,flags,ftn_code,ftn_action)) &
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
    if(proc(parser)) then
       write(*,*) def
       call pm_panic('bad intrinsic module')
    endif
    if(parser%sym/=sym_eof) then
       write(*,*) def
       write(*,*) pm_name_as_string(parser%context,int(parser%sym,pm_p))
       call pm_panic('uproc ends badly')
    endif
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
    if(parser%sym/=sym_eof) then
       write(*,*) def
       write(*,*) pm_name_as_string(parser%context,int(parser%sym,pm_p))
       call pm_panic('type ends badly')
    endif
  end subroutine dcl_type

  ! Start parsing PM code from a string
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

  ! Get next line of PM code
  subroutine next_line(parser)
    type(parse_state),intent(inout):: parser
    integer:: ios
    if(parser%line(parser%ls)/=" ") then
       parser%ls=3-parser%ls
    endif
    parser%lineno=parser%lineno+1
    if(parser_xtra_debug) write(*,*) 'Now at line',parser%lineno
    if(parser%iunit>=0) then
       call pm_read_line(parser%iunit,parser%line(parser%ls),ios)
       if(ios/=0) goto 10
       parser%last=len_trim(parser%line(parser%ls))+1
       parser%n=1
       parser%newline=.true.
       return
    endif
    10 continue
    parser%n=1
    parser%atstart=.false.
    parser%newline=.false.
    parser%line(parser%ls)=pm_eof_char
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
    character(len=max_string+1):: buffer, dbx
    integer:: n

    ! If token pushed back then return it again
    if(parser%pushback>0) then
       parser%sym=parser%pushback
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
       if(c=='!') then
          parser%line(parser%ls)(parser%n-1:)=' '
          call next_line(parser)
          c=getchar()
       endif
       if((.not.isspace(c)).and.(.not.c=='!')) exit
    enddo

    ! Identify token
    if(c/=pm_eof_char) then
       parser%sym_n=parser%n-1
       parser%sym_lineno=parser%lineno
    endif
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
                  parser%modl%data%ptr(parser%modl%offset+modl_name)%offset,&
                  buffer(2:n))
          endif
       else
          sym=pm_name_entry(parser%context,buffer(1:n))
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
       else if(peekchar()==':') then
          c=getchar()
          if(peekchar()=='=') then
             c=getchar()
             sym=sym_define_const
          else
             sym=sym_dcolon
          endif
       else if(peekchar()=='/') then
          c=getchar()
          if(peekchar()=='/') then
             c=getchar()
             sym=sym_dbar
          else
             sym=sym_bar
          endif
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
          sym=sym_hash
       else
          sym=sym_dollar
       endif
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
    case('^')
       sym=sym_caret
       ! ****************************************
       ! End of extended character set options
       ! ****************************************
    case default
       call parse_error(parser,'Error: Unexpected character "'//c//'"')
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
      integer:: n,ibase,ibits,ios
      logical:: isreal,issingle,iscomplex,islonglong
      integer(pm_i128):: inumber
      real:: rv
      real(pm_d) :: dv
      real(pm_r32) :: r32
      real(pm_r64) :: r64
      real(pm_r128) :: r128
      n=0
      isreal=.false.
      issingle=.true.
      islonglong=.false.
      iscomplex=.false.
      do 
         n=n+1
         buffer(n:n)=c
         if(.not.isdigit(peekchar())) exit
         c=getchar()
      end do
      c=peekchar()
      if(c=='r'.or.c=='R') then
         c=getchar()
         read(unit=buffer(1:n),fmt='(G3.0)') ibase
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
               else
                  n=n-iachar('0')
               endif
               if(n>=ibase) then
                  call parse_error(parser,"Bad digit for this base: "//c)
                  inumber=0
                  exit
               endif
               inumber=ibase*inumber+n
               if(peekchar()=='L') then
                  c=getchar()
                  issingle=.false.
                  if(peekchar()=='L') then
                     c=getchar()
                     islonglong=.true.
                  endif
               endif
            enddo
            write(unit=buffer,fmt='(i40)') inumber
            n=len_trim(buffer)+1
         endif
       else if(c/='l'.and.c/='L') then
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
          if(c=='e'.or.c=='d'.or.c=='E'.or.c=='D') then
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
          if(c=='i'.or.c=='j'.or.c=='I'.or.c=='J') then
             c=getchar()
             isreal=.true.
             iscomplex=.true.
          endif
       else
          c=getchar()
          if(c==peekchar()) then
             c=getchar()
             islonglong=.true.
          endif
          issingle=.false.
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
             if(islonglong) then
                val=pm_new_small(parser%context,pm_longlong,1_pm_p)
                read(unit=buffer,fmt='(G40.0)',iostat=ios) &
                     val%data%lln(val%offset)
             elseif(issingle) then
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
          call parse_error(parser,'Expected "'//&
               trim(sym_names(sym))//'"')
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
  subroutine simple_call(parser,nargs,sym)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: nargs
    integer,optional,intent(in):: sym
    integer:: csym
    csym=sym_open
    if(present(sym)) csym=sym
    call make_node(parser,sym_list,nargs)
    call push_null_val(parser)      ! keys
    call push_null_val(parser)      ! amps
    call make_node(parser,csym,4)    
  end subroutine simple_call

  ! Procedure call (scanner must be on token *after* name)
  recursive function proccall(parser,name) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: name
    logical:: iserr
    integer:: i
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
       if(arglist(parser,sym_dcolon,.true.)) return
       if(parser%sym==sym_at) then
          call scan(parser)
          if(expect(parser,sym_open_brace)) return
          if(sexprlist(parser,sym=sym_at_brace)) return
          if(expect(parser,sym_close_brace)) return
          call make_node(parser,sym_reduce_at,2)
       endif
    else
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
    integer m,n,base,sym,nextra
    type(pm_ptr):: temp
    iserr=.true.
    m=0
    n=0
    base=parser%top
    if(is_loop) then
       nextra=loop_call_extra_args
    else
       nextra=0
    endif
    if(is_loop.and.parser%sym>num_sym) then
       call push_sym_val(parser,parser%sym)
       call scan(parser)
       call make_node(parser,sym_list,1)
       call push_null_val(parser)
    elseif(is_loop.and.parser%sym==sym_amp) then
       call scan(parser)
       call push_sym(parser,1+nextra)
       if(expect_name(parser)) return
       call make_node(parser,sym_list,1)
       call push_null_val(parser)
    else
       if(expect(parser,sym_open)) return
       if(parser%sym==sym_close) then
          call push_null_val(parser) !args
          call push_null_val(parser) !keys
          call push_null_val(parser) !amps
          call make_node(parser,clsym,4)
          call scan(parser)
          iserr=.false.
          return
       endif
       do 
          if(parser%sym==sym_amp) then
             call scan(parser)
             if(expect_name(parser)) return
             if(qual(parser,.true.)) return
             m=m+1
             call push_sym(parser,m+nextra)
          elseif(parser%sym==sym_key) then
             call make_node(parser,sym_list,m)
             exit
          else if(parser%sym==sym_arg) then
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
       if(expect(parser,sym_close)) return
    endif
    if(parser%top>base) then
       call name_vector(parser,base)
    else
       call push_null_val(parser)
    endif
    call make_node(parser,clsym,4)
    iserr=.false.
  end function arglist

  ! Qualifiers ( .name or name[explist] or 'term )
  recursive function qual(parser,islhs) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: islhs
    logical:: inass
    logical:: iserr
    integer:: m, csym
    type(pm_ptr):: p1,p2
    logical:: dotonly
    iserr=.true.
    dotonly=.true.
    do
       select case(parser%sym)
       case(sym_dot)
          call scan(parser)
          if(parser%sym==sym_local.or.parser%sym==sym_distr.or.&
               parser%sym==sym_hash) then
             if(.not.dotonly) then
                call parse_error(parser,'".'//sym_names(parser%sym)//&
                     '" cannot be applied in this context')
             endif
             if(parser%sym==sym_hash) then
                call push_sym_val(parser,sym_get_darray)
             elseif(parser%sym==sym_local) then
                call push_sym_val(parser,sym_get_local)
             else
                call push_sym_val(parser,sym_get_distr)
             endif
             call make_node(parser,sym_local,2)
             call scan(parser)
          else
             if(expect_name(parser)) return
             call make_node(parser,sym_dot,2)
          endif
       case(sym_dash)
          call scan(parser)
          if(term(parser,.false.)) return
          call make_node(parser,sym_dash,2)
          dotonly=.false.
       case(sym_over)
          call scan(parser)
          if(term(parser,.false.)) return
          call make_node(parser,sym_over,2)
          dotonly=.false.
       case(sym_open_square)
          call scan(parser)
          if(sexprlist(parser)) return
          call make_node(parser,sym_open_square,2)
          if(expect(parser,sym_close_square)) return
          dotonly=.false.
       case(sym_open_brace) 
          call scan(parser)
          if(sexprlist(parser)) return
          if(expect(parser,sym_close_brace)) return
          call make_node(parser,sym_brace_at,2)
          if(islhs) then
             if(parser%sym/=sym_assign) then
                call parse_error(parser,&
                     '"{}" must be immediately followed by "=" in assignment')
             endif
          endif
          if(.not.dotonly) then
             call parse_error(parser,'"{}" cannot be applied in this context')
          endif
          dotonly=.false.
       case default
          exit
       end select
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
  recursive function matrix_former(parser,symb,sym,row,col) &
       result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: symb,sym
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
    call make_node(parser,sym,3)
    if(expect(parser,symb,'closing '//sym_names(symb))) return
    iserr=.false.
  end function matrix_former

  ! Array former { ... }
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
    if(symb==sym_close_brace.and.parser%sym==symb) then
       call make_node(parser,sym,0)
       call scan(parser)
       iserr=.false.
    else
       if(expr(parser)) return
       if(parser%sym==sym_colon) then
          if(generator(parser,sym)) return
       else if(parser%sym==sym_comma) then
          call scan(parser)
          if(matrix_former(parser,symb,sym,.false.,.true.)) return
       else if(parser%sym==sym_semi) then
          call scan(parser)
          if(matrix_former(parser,symb,sym,.false.,.true.)) return
       else if(parser%sym/=symb) then
          if(parser%atstart) then
             if(matrix_former(parser,symb,sym,.true.,.false.)) return
          else
             call parse_error(parser,'Expected: '//sym_names(symb))
          endif
       else
          if(sym/=sym_close) call make_node(parser,sym,1)
          call scan(parser)
       endif
    endif
    iserr=.false.
  end function array_former

  ! Operator symbols within proc 
  recursive function op(parser,sym,isconst,istype) result(iserr)
    type(parse_state):: parser
    integer,intent(out):: sym
    logical,intent(in):: isconst,istype
    logical:: iserr
    logical:: comm
    iserr=.true.
    comm=.false.
    select case (parser%sym)
    case(first_unary:sym_le-1,sym_lt+1:last_opr,sym_opt,sym_null)
       sym=parser%sym
       call scan(parser)
    case(sym_open_square)
       call scan(parser)
       if(expect(parser,sym_close_square)) return
       sym=sym_sub
    case(sym_open_brace)
       call scan(parser)
       if(expect(parser,sym_close_brace)) return
       sym=sym_brace_at
       comm=.true.
    case(sym_at)
       call scan(parser)
       if(parser%sym==sym_open_brace) then
          call scan(parser)
          if(expect(parser,sym_close_brace)) return
          if(parser%sym==sym_bar) then
             call scan(parser)
             sym=sym_at_brace_or
          else
             sym=sym_at_brace
          endif
       else
          sym=sym_at
       endif
       comm=.true.
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

  ! Structure expression struct [name] { name=.. }
  recursive function struct_gen(parser,sym) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    logical:: iserr
    integer:: base,vbase,name,i,tag
    iserr=.true.
    base=parser%top
    vbase=parser%vtop
    tag=0
    call scan(parser)
    if(parser%sym==sym_open_brace) then
       call scan(parser)
       call push_sym(parser,0)
    else if(check_name(parser,name)) then
       tag=name
       call push_sym(parser,name)
       if(expect(parser,sym_open_brace)) return
    else
       call parse_error(parser,'Expected structure name')
    endif
    do
       if(parser%sym==sym_include) then
          call scan(parser)
          if(check_name(parser,name)) then
             call push_sym(parser,-name)
          else
             call parse_error(parser,&
                  'Expected name of struct or rec element')
          endif
       elseif(check_name(parser,name)) then
          call push_sym(parser,name)
       else
          call parse_error(parser,&
               'Expected name of struct or rec element')
       endif
       if(expect(parser,sym_assign)) return
       if(expr(parser)) return
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    call struct_sort(parser,base+1,vbase)
    call make_node(parser,sym_list,parser%vtop-vbase)
    call name_vector(parser,base)
    if(tag/=0) call check_struct(parser,tag,top_val(parser))
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
       if(abs(parser%stack(i-1)) < abs(n)) cycle
       do while(j>base+1.and.abs(parser%stack(j-1))>abs(n))
          parser%stack(j)=parser%stack(j-1)
          parser%vstack(jj)=parser%vstack(jj-1)
          j=j-1
          jj=jj-1
       enddo
       parser%stack(j)=n
       parser%vstack(jj)=v
    enddo
  end subroutine struct_sort

  ! Check that a structure tag is always associated with same combination
  ! of names
  subroutine check_struct(parser,tag,name)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: tag
    type(pm_ptr),intent(in):: name
    integer(pm_i16),dimension(1):: key
    integer(pm_ln):: k
    type(pm_ptr):: p
    p=decl_entry(parser,tag,modl_tag)
    if(pm_fast_isnull(p)) then
       call push_val(parser,name)
       call new_decl(parser,tag,modl_tag,.true.)
    else
       if(p%offset/=name%offset) then
          call parse_error(parser,&
               'struct/rec tag '//&
               trim(pm_name_as_string(parser%context,tag))//&
               ' not associated with consistent elements')
       endif
    endif
  contains
    include 'fisnull.inc'
  end subroutine check_struct

  ! Term in an expression
  recursive function term(parser,checkqual) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: checkqual
    logical:: iserr
    integer:: m,name,sym,base
    iserr=.true.
    sym=parser%sym
    select case(sym)
    case(sym_struct,sym_rec)
       if(struct_gen(parser,sym)) return
    case(sym_distr)
       call scan(parser)
       if(expect(parser,sym_open_brace)) return
       if(parser%sym/=sym_using) then
          if(expr(parser)) return
       else
          call push_sym_val(parser,sym_null)
       endif
       if(using_clause(parser,sym_align,sym_block)) return
       call make_node(parser,sym_distr,2)
       if(expect(parser,sym_close_brace)) return
    case(sym_lt)
       call scan(parser)
       if(parser%sym==sym_gt) then
          call make_node(parser,sym_any,0)
          call scan(parser)
       else
          if(typ(parser)) return
          if(expect(parser,sym_gt)) return
       endif
       if(term(parser,.true.)) return
       call make_node(parser,sym_any,2)
    case(sym_open)
       call scan(parser)
       if(expr(parser)) return
       if(expect(parser,sym_close)) return
    case(sym_open_square)
       call scan(parser)
       call push_sym_val(parser,sym_tuple)
       if(exprlist(parser,m,.true.)) return
       if(m>7) then
          call parse_error(parser,&
               'More than seven items in tuple/grid former')
       endif
       call simple_call(parser,m)
       if(expect(parser,sym_close_square)) return
    case(sym_open_brace)
       if(array_former(parser,sym_close_brace)) return
    case(sym_present)
       call scan(parser)
       if(expect_name(parser)) return
       call make_node(parser,sym_present,1)
    case(sym_number,sym_string)
       call push_val(parser,parser%lexval)
       call scan(parser)
    case(sym_proc)
       call scan(parser)
       if(expect(parser,sym_open_brace)) return
       if(op(parser,name,.true.,.false.)) return
       if(expect(parser,sym_close_brace)) return
       if(parser%sym==sym_open.or.parser%sym==sym_dcolon&
            .or.parser%sym==sym_pct) then
          if(proccall(parser,name)) return
       else
          call push_sym_val(parser,name)
          call make_node(parser,sym_proc,1)
       endif
    case(sym_dollar)
       call scan(parser)
       if(parser%sym==sym_true.or.parser%sym==sym_false) then
          call push_sym_val(parser,parser%sym)
          call scan(parser)
       else
          if(expect_name(parser)) return
       endif
       call make_node(parser,sym_dollar,1)
    case(sym_null)
       call scan(parser)
       if(parser%sym==sym_open) then
          if(proccall(parser,sym)) return
       else
          call push_sym_val(parser,sym)
       endif
    case(sym_opt)
       call scan(parser)
       if(parser%sym/=sym_open) then
          if(expect(parser,sym_open)) return
       endif
       if(proccall(parser,sym)) return
    case(sym_true,sym_false,sym_argc) 
       call push_sym_val(parser,parser%sym)
       call scan(parser)
       goto 20
    case default
       if(check_name(parser,name)) then
          select case(parser%sym)
          case(sym_open,sym_dcolon,sym_pct)
             if(proccall(parser,name)) return
          case(sym_at)
             call scan(parser)
             call push_sym_val(parser,name)
             if(expect(parser,sym_open_brace)) return
             if(sexprlist(parser)) return
             if(expect(parser,sym_close_brace)) return
             ! | [-] term
             if(parser%sym==sym_bar) then
                call scan(parser)
                if(parser%sym==sym_minus) then
                   call scan(parser)
                   if(parser%sym/=sym_number) then
                      call parse_error(parser,'"-" operator after "|"')
                      return
                   else
                      call push_val(parser,parser%lexval)
                      call scan(parser)
                   endif
                   call make_node(parser,sym_uminus,1)
                else
                   if(term(parser,.true.)) return
                endif
                call make_node(parser,sym_at_brace_or,3)
             else
                call make_node(parser,sym_at_brace,2)
             endif
          case default
             call push_sym_val(parser,name)
          end select
       else
          call parse_error(parser,'Malformed expression')
          return
       endif
    end select
10  continue
    if(checkqual) then
       if(qual(parser,.false.)) return
    endif
20  continue
    iserr=.false.
  end function term
  
  ! Expression
  recursive function expr(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: base,nesting,s,sym,ntuple
    logical:: row
    iserr=.true.
    base=parser%top
    nesting=0

    do 
       if(parser%sym==sym_minus) then
          parser%sym=sym_uminus
       elseif(parser%sym==sym_by) then
          parser%sym=sym_uby
       elseif(parser%sym==sym_dotdotdot) then
          parser%sym=sym_to_range
       else if(parser%sym/=sym_not) then
          exit
       endif
       call push()
       call scan(parser)
    enddo
    
    do

       do while(parser%sym==sym_open) 
          call push()
          call scan(parser)
          nesting=nesting+1
          do 
             if(parser%sym==sym_minus) then
                parser%sym=sym_uminus
             elseif(parser%sym==sym_by) then
                parser%sym=sym_uby
             elseif(parser%sym==sym_dotdotdot) then
                parser%sym=sym_to_range
             else if(parser%sym/=sym_not) then
                exit
             endif
             call push()
             call scan(parser)
          enddo
       enddo

       if(term(parser,.true.)) goto 999

       do while(nesting>0.and.parser%sym==sym_close)
          if(popto(sym_close)) goto 999
          nesting=nesting-1
          call scan(parser)
          if(qual(parser,.false.)) return
       enddo

       if(parser%sym==sym_dotdotdot) then
          if(popto(sym_from_range)) goto 999
          call push_s(sym_from_range)
          call scan(parser)
       endif
       
       if(binary(parser%sym)) then
          s=parser%sym
          call scan(parser)
          if(popto(s)) goto 999
          call push_s(s)
       else if(nesting>0) then
          if(popto(sym_open)) goto 999
          if(parser%sym==sym_comma) then
             call scan(parser)
             row=.false.
          else if(parser%sym==sym_semi) then
             call scan(parser)
             row=.true.
          else if(parser%atstart) then
             row=.true.
          else
             call parse_error(parser,'Too many "(" in expression')
             goto 999
          endif
          if(matrix_former(parser,sym_matrix_former,sym_close,row,.not.row)) &
               goto 999
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
        if(parser%sym==sym_not) then
          if(priority(top())>priority(sym_not)) then
             call push()
             call scan(parser)
          else
             call parse_error(parser,&
                  '"not" cannot follow operator of higher precedence')
             return
          endif
       elseif(parser%sym==sym_minus) then
          if(priority(top())>priority(sym_minus)) then
             call push_s(sym_uminus)
             call scan(parser)
          else
             call push_s(sym_uminus)
             call scan(parser)
             if(parser%sym/=sym_number) then
                call parse_error(parser,&
                     '"-" cannot follow high precedence operator')
                return
             endif
          endif
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
               if(sym==sym_cond.and.popsym==sym_dbar) then
                  call push_s(sym_cond)
                  iserr=.false.
                  return
               else
                  call parse_error(parser,&
                       'Cannot repeat: "'//trim(sym_names(sym))//'"')
                  return
               endif
            elseif(sym==sym_dbar) then
               if(pop()/=sym_cond) then
                  call parse_error(parser,'Malformed "=> ||" expression')
                  return
               endif
               sym=sym_cond
               n=3
            elseif(sym==sym_cond) then
               call parse_error(parser,'Malformed "=> ||" expression')
               return
            else if(.not.binary(sym)) then
               n=1
            else
               n=2
            endif
            call make_node(parser,sym,n,max(1,n-1))
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

  ! Subscript expression (includes ...e and e...)
  function sexpr(parser) result(iserr)
    type(parse_state):: parser
    logical:: iserr
    integer:: s
    type(pm_ptr):: junk
    iserr=.true.
    if(parser%sym==sym_comma.or.parser%sym==sym_close_square&
         .or.parser%sym==sym_close_brace) then
       call push_sym_val(parser,sym_null)
    else
       if(expr(parser)) return
    endif
    iserr=.false.
  end function sexpr

  ! Comma separated list of sexpr
  recursive function sexprlist(parser,length,sym) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(out),optional:: length
    integer,intent(in),optional:: sym
    logical:: iserr
    integer:: n,csym
    iserr=.true.
    n=0
    do
       if(sexpr(parser)) return
       n=n+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    csym=sym_list
    if(present(sym)) csym=sym
    call make_node(parser,csym,n)
    if(present(length)) length=n
    iserr=.false.
  end function sexprlist

  ! Assignment (=) definition (:=) 
  recursive function assn(parser,sym) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: sym
    logical:: iserr
    integer:: name,n,asym
    logical:: must_be_assign
    must_be_assign=.false.
    iserr=.true.
    if(lhs()) return
    if(parser%sym/=sym_assign) then
       if((parser%sym==sym_define.or.parser%sym==sym_define_const)&
            .and.(sym==sym_assign.or.sym==sym_with)) then
          asym=parser%sym
          call scan(parser)
          if(rhs()) return
          if(must_be_assign) then
             call parse_error(parser,'Incorrect left hand side to ":="')
          endif
          call make_node(parser,asym,n+1)
          iserr=.false.
          return
       else
          call parse_error(parser,'Expected "="')
          return
       endif
    endif
    call scan(parser)
    if(sym/=sym_assign.and.must_be_assign) then
       call parse_error(parser,'Incorrect left hand side in definition')
    endif
    if(rhs()) return
    call make_node(parser,sym,n+1)
    iserr=.false.
    return

  contains

    function lhs() result(is_err)
      logical:: is_err
      integer:: m
      is_err=.true.
      n=0
      do
         select case(parser%sym)
         case(sym_underscore)
            call make_node(parser,sym_underscore,0)
            call scan(parser)
            n=n+1
         case(sym_var)
            call scan(parser)
            if(parser%sym==sym_open_brace) then
               call scan(parser)
               m=0
               do
                  if(expect_name(parser,'variable name')) return
                  m=m+1
                  if(parser%sym/=sym_comma) exit
                  call scan(parser)
               enddo
               if(expect(parser,sym_close_brace)) return
            else
               if(expect_name(parser,'variable name')) return
               m=1
            endif
            if(parser%sym==sym_colon) then
               call scan(parser)
               if(typ(parser)) return
            else
               call push_null_val(parser)
            endif
            call make_node(parser,sym_var,m+1)
            n=n+1
            must_be_assign=.true.
         case(sym_const)
            call scan(parser)
            if(expect_name(parser,'constant name')) return
            if(parser%sym==sym_colon) then
               call scan(parser)
               if(typ(parser)) return
            else
               call push_null_val(parser)
            endif
            call make_node(parser,sym_const,2)
            n=n+1
            must_be_assign=.true.
         case default
            if(is_name(parser)) then
               n=n+1
               select case(parser%sym)
               case(sym_comma)
                  call scan(parser)
                  cycle
               case(sym_assign,sym_define,sym_define_const)
                  exit
               case default
                  if(sym/=sym_with) then
                     if(qual(parser,.true.)) return
                     must_be_assign=.true.
                  endif
               end select
            else
               call parse_error(parser,&
                    'Malformed left hand side of assignmentment/definition')
            endif
         end select
         if(parser%sym==sym_comma) then
            call scan(parser)
         else
            exit
         endif
      enddo
      is_err=.false.
    end function lhs

    function rhs() result(is_err)
      logical:: is_err
      is_err=.true.
      if(n==1) then
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

  ! Reference to variable
  recursive function valref(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    iserr=.true.
    if(expect_name(parser)) return
    if(qual(parser,.true.)) return
    iserr=.false.
  end function valref
 
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
    if(parser%sym/=sym_when) then
       call xexpr(parser)
    else
       call push_sym_val(parser,sym_true)
    endif
    n=0
    do while(parser%sym==sym_when)
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
  recursive function iter(parser,inok) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: inok
    logical:: iserr
    integer:: i,m,name
    iserr=.true.
    m=0
    do
       if(check_name(parser,name)) then
          if(expect(parser,sym_in)) return
          call push_sym_val(parser,name)
          if(expr(parser)) return
          m=m+1
       else
          if(expect_name(parser,'loop variable')) return
       endif
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    call make_node(parser,sym_iter,m*2)
    iserr=.false.
  end function iter

  recursive function for_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: m,n,name
    iserr=.true.
    
    if(iter(parser,.true.)) return
    call subexp(parser)
    
    if(using_clause(parser,sym_part,sym_work)) return
    call swap_vals(parser)
    
    if(parser%sym==sym_conc) then
       call scan(parser)
       call push_sym_val(parser,sym_conc)
    else
       call push_sym_val(parser,sym_for)
    endif

    if(parser%sym==sym_return) then
       call push_null_val(parser)
    else
       if(expect(parser,sym_do)) return
       call statement_list(parser)
    endif

    m=4
    if(parser%sym==sym_return) then
       call scan(parser)
       if(return_clause(parser,m,sym_endfor)) return
    endif
    call make_node(parser,sym_for,m)
    if(expect(parser,sym_endfor)) return
    iserr=.false.
  end function for_stmt

  recursive function find_stmt(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: m,n,nn,name
    iserr=.true.

    call scan(parser)
    if(iter(parser,.true.)) return
    call subexp(parser)

    if(using_clause(parser,sym_part,sym_work)) return
    call swap_vals(parser)
    
    if(parser%sym==sym_conc) then
       call scan(parser)
       call push_sym_val(parser,sym_conc)
    else
       call push_sym_val(parser,sym_for)
    endif

    if(parser%sym==sym_do) then
       call scan(parser)
       call statement_list(parser)
    else
       call push_null_val(parser)
    endif

    if(expect(parser,sym_when)) return
    call xexpr(parser)
    if(expect(parser,sym_return)) return
    m=5
    if(return_clause(parser,m,sym_endfind)) return
    call make_node(parser,sym_find,m)
    if(expect(parser,sym_endfind)) return
    iserr=.false.
  end function find_stmt

  function return_clause(parser,m,endsym) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(inout):: m
    integer,intent(in):: endsym
    logical:: iserr
    integer:: i,n,nu,nn,name
    iserr=.true.
    do
       n=0
       nu=0
       if(parser%sym==sym_var.or.parser%sym==sym_const) then
          do
             if(parser%sym==sym_underscore) then
                call scan(parser)
                call push_sym_val(parser,0)
                nu=nu+1
             elseif(parser%sym==sym_var) then
                call scan(parser)
                if(expect_name(parser)) return
             else
                if(expect(parser,sym_const)) return
                if(.not.check_name(parser,name)) then
                   call parse_error(parser,'Expected name')
                   return
                endif
                call push_sym_val(parser,-name)
             endif
             n=n+1
             if(parser%sym/=sym_comma) exit
             call scan(parser)
          enddo
          if(expect(parser,sym_assign)) return
       else
          do
             if(parser%sym==sym_underscore) then
                call scan(parser)
                call push_sym_val(parser,0)
                nu=nu+1
             else
                if(expect_name(parser)) return
             endif
             n=n+1
             if(parser%sym/=sym_comma) exit
             call scan(parser)
          enddo
          if(parser%sym==sym_define_const) then
             call scan(parser)
             do i=parser%vtop-n+1,parser%vtop
                parser%vstack(i)%offset=-parser%vstack(i)%offset
             enddo
          else
             if(expect(parser,sym_define)) return
          endif
       endif
       if(nu==n) then
          call parse_error(parser,&
               'return assignment must create at least one var or const')
       endif
       call make_node(parser,sym_list,n)
       if(n==1) then
          call xexpr(parser)
       else
          if(check_name(parser,name)) then
             if(proccall(parser,name)) return
             call push_num_val(parser,n)
             call make_node(parser,sym_return,2)
          else
             call parse_error(parser,'Expected procedure name')
          endif
       endif
       m=m+2
       if(endsym==sym_endfind) then
          if(expect(parser,sym_default)) return
          if(n==1) then
             call xexpr(parser)
          else
             if(parser%sym==sym_open) then
                call scan(parser)
                if(exprlist(parser,nn)) return
                if(nn/=n) then
                   call parse_error(parser,&
                        'Incorrect number of items in default list')
                endif
                call make_node(parser,sym_list,n)
                if(expect(parser,sym_close)) return
                call subexp(parser)
             elseif(check_name(parser,name)) then
                if(proccall(parser,name)) return
                call push_num_val(parser,n)
                call make_node(parser,sym_return,2)
                call subexp(parser)
             else
                call parse_error(parser,'Expected procedure name')
             endif
          endif
          m=m+1
       endif
       if(parser%sym==endsym.or.parser%sym==sym_also) then
          exit
       elseif(.not.parser%atstart) then
          if(expect(parser,sym_semi)) return
       endif
    enddo
    iserr=.false.
  end function return_clause

  ! Using clause - leaves two items on stack
  function using_clause(parser,start,finish) result(iserr)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: start,finish
    logical:: iserr
    integer:: n,i,base,vbase,vbase2,name
    iserr=.true.
    if(parser%sym/=sym_using) then
       call push_null_val(parser)
       iserr=.false.
       return
    endif
    call scan(parser)
    n=0
    base=parser%top
    vbase=parser%vtop
    do while(parser%sym>num_sym)
       call push_sym(parser,parser%sym)
       call scan(parser)
       if(expect(parser,sym_assign)) return
       if(expr(parser)) return
       n=n+1
       if(parser%sym/=sym_comma) exit
       call scan(parser)
    enddo
    if(n>0) then
       vbase2=parser%vtop
       do i=start,finish
          call push_sym_val(parser,sym_null)
       enddo
       do i=1,n
          name=parser%stack(base+i)
          if(name>=start.and.&
               name<=finish) then
             name=name-start+1
             if(node_sym(parser%vstack(vbase+i))/=sym_null) then
                parser%vstack(vbase2+name)=parser%vstack(vbase+i)
             else
                call parse_error(parser,'Repeated "using" clause: '//&
                     trim(pm_name_as_string(parser%context,&
                     int(name+start-1,pm_p))))
             endif
          else
             call parse_error(parser,'Not an allowed "using" clause: '//&
                  trim(pm_name_as_string(parser%context,&
                  int(name,pm_p))))
          endif
       enddo
       call make_node(parser,sym_list,finish-start+1)
       parser%top=base
       parser%vstack(vbase+1)=top_val(parser)
       parser%vtop=vbase+1
    else
       call push_null_val(parser)
    endif
    iserr=.false.
  contains
    include 'fisnull.inc'
  end function using_clause

  function do_stmt(parser) result(is_error)
    type(parse_state),intent(inout):: parser
    logical:: is_error
    integer:: i,k,n,m,name
    is_error=.true.
    if(using_clause(parser,sym_part,sym_work)) return
    if(parser%sym==sym_with) then
       call scan(parser)
       if(assn(parser,sym_with)) return
       call make_node(parser,sym_list,1)
    else
       call push_null_val(parser)
    endif
    k=2
    do 
       if(expect(parser,sym_do)) return
       call statement_list(parser)
       if(parser%sym==sym_return) then
          call scan(parser)
          m=0
          if(return_clause(parser,m,sym_enddo)) return
          call make_node(parser,sym_list,m)
 
       else
          call push_null_val(parser)
       endif
       k=k+2
       if(parser%sym/=sym_also) exit
       call scan(parser)
    enddo
    call make_node(parser,sym_also,k)
    if(expect(parser,sym_enddo)) return
    is_error=.false.
  end function do_stmt
  
  ! List of statements
  recursive subroutine statement_list(parser)
    type(parse_state),intent(inout):: parser
    logical:: ok
    integer:: i,n,m,k,name,sym,label
    type(pm_ptr):: p
    k=0
    do
       select case(parser%sym)
       case(sym_if)
          n=0
          sym=sym_if
          do
             call scan(parser)
             if(parser%sym==sym_invar) then
                if(n>0.and.sym/=sym_invar) then
                   call parse_error(parser,&
                        'All branches of "if" must be "invar"')
                endif
                call scan(parser)
                sym=sym_invar
             else
                if(sym==sym_invar) then
                   call parse_error(parser,&
                        'All branches of "if" must be "invar"')
                endif
             endif
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
             call make_node(parser,sym,3)
             call make_node(parser,sym_list,1)
             n=n-1
          enddo
          call make_node(parser,sym,3)
          if(expect(parser,sym_endif)) goto 999
       case(sym_select)
          if(sel_statement(parser)) goto 999
       case(sym_while)
          call push_null_val(parser)
          if(while_stmt()) goto 999
       case(sym_repeat)
          call push_null_val(parser)
          if(repeat_stmt()) goto 999
       case(sym_check)
          call push_null_val(parser)
          call subexp(parser)
       case(sym_debug)
          call scan(parser)
          if(parser%sym==sym_string) then
             call push_val(parser,parser%lexval)
             call scan(parser)
             if(expect(parser,sym_colon)) return
          else
             call push_null_val(parser)
          endif
          call statement_list(parser)
          call make_node(parser,sym_debug,2)
          if(expect(parser,sym_enddebug)) goto 999
       case(sym_for)
          call scan(parser)
          if(parser%sym==sym_each) then
             call scan(parser)
             if(for_each_stmt(0)) goto 999
          else
             if(for_stmt(parser)) goto 999
          endif
       case(sym_find)
          if(find_stmt(parser)) goto 999
       case(sym_do,sym_with,sym_using)
          if(do_stmt(parser)) goto 999
       case(sym_any)
          call scan(parser)
          if(expect_name(parser)) goto 999
          if(parser%sym==sym_assign) then
             call scan(parser)
             call xexpr(parser)
          else
             call push_null_val(parser)
          endif
          if(parser%sym==sym_return) then
             call push_null_val(parser)
          else
             if(expect(parser,sym_do)) goto 999
             call statement_list(parser)
          endif
          m=3
          if(parser%sym==sym_return) then
             call scan(parser)
             if(return_clause(parser,m,sym_endany)) goto 999
          endif
          if(expect(parser,sym_endany)) goto 999
          call make_node(parser,sym_any,m)
       case(sym_underscore,sym_var,sym_const)
          if(assn(parser,sym_assign)) goto 999
          if(parser%sym==sym_check.or.parser%sym==sym_when) then
             call subexp(parser)
          endif
       case(sym_result)
          exit
       case(sym_proc)
          call scan(parser)
          if(expect(parser,sym_open_brace)) goto 999
          if(op(parser,name,.true.,.false.)) goto 999
          if(expect(parser,sym_close_brace)) goto 999
          if(proccall(parser,name)) goto 999
          if(parser%sym==sym_where.or.parser%sym==sym_check) then
             call make_node(parser,sym_do,1)
             call subexp(parser)
          endif
       case default
          if(check_name(parser,name)) then
             ! Labelled statements
             if(parser%sym==sym_colon) then
               call scan(parser)
               select case(parser%sym)
               case(sym_underscore,sym_var,sym_const)
                  if(assn(parser,sym_assign)) goto 999
                  call subexp(parser)
                  call push_sym_val(parser,name)
                  call make_node(parser,sym_sync,2)
                  goto 888
               case(sym_sync) 
                  call scan(parser)
                  call push_null_val(parser)
                  call push_sym_val(parser,name)
                  call make_node(parser,sym_sync,2)
                  goto 888
               case(sym_while)
                  call push_sym_val(parser,name)
                  if(while_stmt()) goto 999
                  goto 888
               case(sym_repeat)
                  call push_sym_val(parser,name)
                  if(repeat_stmt()) goto 999
               case(sym_for)
                  call scan(parser)
                  if(expect(parser,sym_each)) goto 999
                  if(for_each_stmt(name)) goto 999
                  goto 888
               end select
               label=name
               if(.not.check_name(parser,name)) then
                  if(expect_name(parser)) goto 999
               endif
            else
               label=0
            endif
            if(parser%sym==sym_open) then
               if(proccall(parser,name)) goto 999
               if(parser%sym==sym_where.or.parser%sym==sym_check) then
                  call make_node(parser,sym_do,1)
                  call subexp(parser)
               endif
            elseif(parser%sym==sym_pct) then
               call scan(parser)
               if(parser%sym==sym_open_brace) then
                  call push_sym_val(parser,name)
                  if(assn(parser,sym_assign)) goto 999
               else
                  call push_back(parser,sym_pct)
                  if(proccall(parser,name)) goto 999
                  if(parser%sym==sym_where.or.parser%sym==sym_check) then
                     call make_node(parser,sym_do,1)
                     call subexp(parser)
                  endif
               endif
            else
               call push_back(parser,name)
               if(assn(parser,sym_assign)) goto 999
               call subexp(parser)
            endif
            if(label/=0) then
               call make_node(parser,sym_list,1)
               call push_sym_val(parser,label)
               call make_node(parser,sym_sync,2)
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

    function while_stmt() result(is_err)
      logical:: is_err
      is_err=.true.
      call scan(parser)
      call xexpr(parser)
      if(expect(parser,sym_do)) return
      call statement_list(parser)
      if(expect(parser,sym_endwhile)) return
      call make_node(parser,sym_while,3)
      is_err=.false.
    end function while_stmt

    function repeat_stmt() result(is_err)
      logical:: is_err
      is_err=.true.
      call scan(parser)
      call statement_list(parser)
      if(expect(parser,sym_until)) return
      call xexpr(parser)
      call make_node(parser,sym_repeat,3)
      is_err=.false.
    end function repeat_stmt
    
    function for_each_stmt(name) result(is_err)
      integer,intent(in):: name
      logical:: is_err
      is_err=.true.
      if(iter(parser,.false.)) return
      call subexp(parser)
      if(parser%sym==sym_until) then
         call scan(parser)
         call xexpr(parser)
      elseif(parser%sym==sym_while) then
          call scan(parser)
          call xexpr(parser)
          call make_node(parser,sym_while,1)
       else
          call push_null_val(parser)
       endif
       if(expect(parser,sym_do)) return
       call statement_list(parser)
       if(expect(parser,sym_endfor)) return
       call push_sym_val(parser,name)
       call make_node(parser,sym_each,4)
       is_err=.false.
       return
     end function for_each_stmt
    
  end subroutine statement_list

  ! Type
  recursive function typ(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: m,name,i,base,vbase,sym,tag
    iserr=.true.
    sym=parser%sym
    select case(sym)
    case(sym_struct,sym_rec)
       sym=parser%sym
       tag=0
       call scan(parser)
       base=parser%top
       vbase=parser%vtop
       if(parser%sym==sym_open_brace) then
          call scan(parser)
          call push_sym(parser,0)
       else
          if(check_name(parser,name)) then
             tag=name
             call push_sym(parser,name)
          else
             call parse_error(parser,&
                  'Expected stucture name')
          endif
          if(expect(parser,sym_open_brace)) return
       endif
       do
          if(parser%sym==sym_include) then
             call scan(parser)
             if(check_name(parser,name)) then
                call push_sym(parser,-name)
             else
                call parse_error(parser,&
                     'Expected name of struct/rec element')
                return
             endif
          elseif(check_name(parser,name)) then
             call push_sym(parser,name)
          else
             call parse_error(parser,&
                  'Expected name of struct/rec element')
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
       if(tag/=0) call check_struct(parser,tag,top_val(parser))
       call make_node(parser,sym,2)
       if(expect(parser,sym_close_brace)) return
    case(sym_lt)
       call scan(parser)
       if(parser%sym==sym_gt) then
          call make_node(parser,sym_any,0)
          call make_node(parser,sym_any,1)
          call scan(parser)
       else
          if(typ(parser)) return
          if(expect(parser,sym_gt)) return
          call make_node(parser,sym_any,1)
       endif
    case(sym_any)
       call make_node(parser,sym_any,0)
       call scan(parser)
    case(sym_open_square)
       call make_node(parser,sym_any,0)
    case(sym_dollar)
       call scan(parser)
       if(parser%sym==sym_true.or.parser%sym==sym_false) then
          call push_sym_val(parser,parser%sym)
          call scan(parser)
       else
          if(expect_name(parser)) return
       endif
       call make_node(parser,sym_dollar,1)
    case(sym_proc)
       call scan(parser)
       if(parser%sym==sym_open_brace) then
          call scan(parser)
          if(op(parser,name,.true.,.true.)) return
          call push_sym_val(parser,name)
          call make_node(parser,sym_proc,1)
          if(expect(parser,sym_close_brace)) return
       else
          call push_sym_val(parser,sym_proc)
          call make_node(parser,sym_type,1)
       endif
    case default
       if(.not.check_name(parser,name)) then
          if(parser%sym/=sym_null.and.parser%sym/=sym_dim) then
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
  recursive function param_list(parser,isloop,ampargs) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: isloop
    logical,intent(out):: ampargs
    logical:: iserr
    integer:: m,n,i,base,vbase,sym,name,numloop,nextra
    type(pm_ptr):: temp
    base=parser%top
    iserr=.true.
    ampargs=.false.
    m=0
    n=0
    if(isloop) then
       vbase=parser%vtop
       do sym=sym_this_dom,sym_this_index
          call push_sym_val(parser,sym)
          call push_null_val(parser)
          m=m+1
       enddo
       if(parser%sym==sym_lt) then
          call scan(parser)
          do
             if(parser%sym>=sym_this_dom.and.parser%sym<=sym_this_tile) then
                i=parser%sym-sym_this_dom+1
                call scan(parser)
                if(expect(parser,sym_colon)) return
                if(typ(parser)) return
                parser%vstack(vbase+i*2)=&
                     top_val(parser)
                temp=pop_val(parser)
             endif
             if(parser%sym==sym_comma) then
                call scan(parser)
             elseif(parser%sym/=sym_gt) then
                if(expect(parser,sym_comma)) return
             else
                exit
             endif
          enddo
          if(expect(parser,sym_gt)) return
       endif
       nextra=m
    else
       nextra=0
    endif
    if(expect(parser,sym_open)) return
    if(m==0.and.parser%sym==sym_close) then
       call push_null_val(parser)
       call push_null_val(parser)
       call push_null_val(parser)
       call scan(parser)
       iserr=.false.
       return
    endif
    do
       if(parser%sym==sym_arg) then
          call scan(parser)
          if(expect(parser,sym_dotdotdot)) return
          call push_sym_val(parser,sym_arg)
          if(parser%sym==sym_colon) then
             call scan(parser)
             if(isloop.and.parser%sym==sym_invar) then
                call scan(parser)
                if(parser%sym==sym_comma.or.parser%sym==sym_close) then
                   call make_node(parser,sym_any,0)
                else
                   if(typ(parser)) return
                endif
                call make_node(parser,sym_invar,1)
             elseif(isloop.and.parser%sym==sym_chan) then
                call scan(parser)
                if(parser%sym==sym_comma.or.parser%sym==sym_close) then
                   call make_node(parser,sym_any,0)
                else
                   if(typ(parser)) return
                endif
                call make_node(parser,sym_chan,1)
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
          call push_sym(parser,m)
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
             call scan(parser)
             if(parser%sym==sym_comma.or.parser%sym==sym_close) then
                call make_node(parser,sym_any,0)
             else
                if(typ(parser)) return
             endif
             call make_node(parser,sym_invar,1)
          elseif(isloop.and.parser%sym==sym_chan) then
             call scan(parser)
             if(parser%sym==sym_comma.or.parser%sym==sym_close) then
                call make_node(parser,sym_any,0)
             else
                if(typ(parser)) return
             endif
             call make_node(parser,sym_chan,1)
          else
             if(typ(parser)) return
          endif
       else
          call push_null_val(parser)
       endif
       if(parser%sym/=sym_comma) then
          call make_node(parser,sym_list,m*2)
          exit
       endif
       call scan(parser)
    enddo
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
    if(expect(parser,sym_close)) return
    if(parser%top>base) then
       call name_vector(parser,base)
       ampargs=.true.
    else
       call push_null_val(parser)
       ampargs=.false.
    endif
    iserr=.false.
    return
  end function param_list

  ! Procedure declaration
  function proc(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    type(pm_ptr):: ptr,p
    type(pm_reg),pointer:: reg
    integer:: name,nret,base,flags,sbase,scount,sym,m,nreduce
    logical:: ampargs
    reg=>pm_register(parser%context,'proc',ptr)
    iserr=.true.
    nret=0
    sbase=parser%vtop
    scount=parser%error_count
    call scan(parser)
10  continue
    if(.not.check_name(parser,name)) then
       if(op(parser,name,.false.,.false.)) goto 999
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
    p=parser%modl%data%ptr(parser%modl%offset+modl_name)
    name=-p%offset
    call push_sym(parser,name)
    p=pop_val(parser)
    name=p%offset
    call push_sym(parser,name)
    call name_vector(parser,parser%top-2)
    if(parser%error_count>0) then
       ptr=pm_null_obj
    else
       ptr=decl_entry(parser,name,modl_proc)
    endif
    base=parser%vtop
    call push_val(parser,top_val(parser))
    if(pm_fast_isnull(ptr)) then
       call push_null_val(parser)
    else
       call push_val(parser,ptr%data%ptr(ptr%offset+node_args+1))
    endif
    call push_val(parser,parser%modl)
    call push_num_val(parser,-1) ! flags
    if(param_list(parser,sym/=sym_proc,ampargs)) goto 999
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
       nreduce=m
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
    elseif(parser%sym==sym_each) then
       call scan(parser)
       if(expect(parser,sym_open)) return
       m=0
       do
          if(expect_name(parser)) return
          m=m+1
          if(parser%sym/=sym_comma) exit
          call scan(parser)
       enddo
       call make_node(parser,sym,m)
       if(expect(parser,sym_close)) return
       sym=sym_each
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
    if(sym==sym_reduce.and.nret/=nreduce) then
       call parse_error(parser,&
            'Number of arguments not equal to number of returns'//&
            ' in "reduce" procedure')
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
    call pm_delete_register(parser%context,reg)
    return
999 call pm_delete_register(parser%context,reg)
    call parse_error(parser,'Procedure '//&
         trim(pm_name_as_string(parser%context,int(name,pm_p)))//&
         ' has errors')
  contains
    include 'fisnull.inc'
  end function proc

  ! Procedure signature (...)[]->(...)
  recursive function proc_sig(parser,isloop) result(iserr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: isloop
    logical:: iserr
    integer:: m,n,base,name,sym
    type(pm_ptr):: temp
    base=parser%top
    iserr=.true.
    m=0
    n=0
    if(isloop) then
       do sym=sym_this_dom,sym_this_index
          call push_sym_val(parser,sym)
          call push_null_val(parser)
          m=m+1
       enddo
    endif
    if(parser%sym==sym_open) then
       call scan(parser)
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
                elseif(parser%sym==sym_chan) then
                   call scan(parser)
                   if(typ(parser)) return
                   call make_node(parser,sym_chan,1)
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
             else if(parser%sym==sym_chan) then
                call scan(parser)
                if(typ(parser)) return
                call make_node(parser,sym_chan,1)
             else
                if(typ(parser)) return
             endif
             
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
    else
       call make_node(parser,sym_list,m)
       call push_null_val(parser)
       call push_null_val(parser)
    endif
    call push_num_val(parser,-1) ! Coded params
    if(parser%sym==sym_arrow) then
       call scan(parser)
       sym=parser%sym
       select case(sym)
       case(sym_mult,sym_assign,sym_hash,sym_gt,sym_dim,&
            sym_dot,sym_query,sym_amp,sym_eq,sym_over) 
          call scan(parser)
          if(exprlist(parser,m)) return
          parser%temp=pop_val(parser)
          if(sym==sym_gt.or.sym==sym_dim.or.sym==sym_eq.or.sym==sym_over) m=1
          call push_num_val(parser,m)
          call push_null_val(parser)
          call push_val(parser,parser%temp)
          call make_node(parser,sym,1)
       case default
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
       end select
    else
       call push_num_val(parser,0)
       call push_null_val(parser)
       call push_null_val(parser)
    endif
    iserr=.false.
    return
  end function proc_sig

  ! Built in procedure definition
  function builtin(parser,opcode,opcode2,pdata,flags,&
       ftn_code,ftn_action) result(iserr)
    type(parse_state),intent(inout):: parser
    integer(pm_i16),intent(in):: opcode,opcode2
    type(pm_ptr),intent(in):: pdata
    integer,intent(in):: flags
    character(len=*),intent(in):: ftn_code
    integer,intent(in):: ftn_action
    logical:: iserr
    type(pm_ptr),target:: ptr
    type(pm_ptr)::p
    type(pm_reg),pointer:: reg
    integer:: name,sym
    logical:: isloop
    reg=>pm_register(parser%context,'builtin',ptr)
    iserr=.true.
    isloop=.false.
    call scan(parser)
10  continue
    if(.not.check_name(parser,name)) then
       if(op(parser,name,.false.,.false.)) goto 999
    endif
    if(parser%sym==sym_dcolon) then
       call push_sym(parser,sym_dcolon)
       call push_sym(parser,name)
       call name_vector(parser,parser%top-2)
       call scan(parser)
       isloop=.true.
    elseif(parser%sym==sym_pct) then
       call push_sym(parser,sym_pct)
       call push_sym(parser,name)
       call name_vector(parser,parser%top-2)
       call scan(parser)
       isloop=.true.
    else
       call push_sym_val(parser,name)
    endif
    ptr=top_val(parser)
    name=ptr%offset
    ptr=decl_entry(parser,int(ptr%offset),modl_proc)

    p=parser%modl%data%ptr(parser%modl%offset+modl_name)
    name=-p%offset
    call push_sym(parser,name)
    p=pop_val(parser)
    name=p%offset
    call push_sym(parser,name)
    call name_vector(parser,parser%top-2)
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
    call push_val(parser,pm_new_string(parser%context,ftn_code))
    call push_num_val(parser,ftn_action)
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
       if(parser%sym==sym_dim) then
          call scan(parser)
          name=sym_dim
       else
          call parse_error(parser,'Expected type name')
          goto 999
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
    if(parser%error_count==0) then
       ptr=decl_entry(parser,name,modl_type)
       if(pm_fast_isnull(ptr)) then
          call push_null_val(parser)
       else
          call push_val(parser,ptr%data%ptr(ptr%offset+node_args+1))  ! link
       endif
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
         q=parser%modl%data%ptr(parser%modl%offset+modl_name)
         call push_sym(parser,-int(q%offset))
         call push_sym(parser,nam)
         q=top_val(parser)
         call name_vector(parser,parser%top-2)
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

  ! Parameter declarations
  function decparam(parser) result(iserr)
    type(parse_state),intent(inout):: parser
    logical:: iserr
    integer:: name,name2,m,base,top,serror
    iserr=.true.
    call scan(parser)
    do
       call push_null_val(parser)
       if(.not.check_name(parser,name)) return
       if(parser%sym==sym_comma) then
          base=parser%top
          do 
             call scan(parser)
             if(check_name(parser,name2)) then
                call push_sym(parser,name2)
             else
                call parse_error(parser,'Expected name')
                return
             endif
             if(parser%sym/=sym_comma) exit
          enddo
          top=parser%top
          if(expect(parser,sym_assign)) return
          if(term(parser,.false.)) return
          if(expect(parser,sym_dotdotdot)) return
          call make_node(parser,sym_param,2)
          call new_decl(parser,name,modl_param,.false.)
          if(parser%sym==sym_by) then
             call scan(parser)
             if(term(parser,.false.)) return
             do m=base+1,top
                name2=parser%stack(m)
                call push_null_val(parser)
                call push_sym_val(parser,name)
                call push_val(parser,parser%vstack(parser%vtop-2))
                call make_node(parser,sym_dotdotdot,3)
                call new_decl(parser,name2,modl_param,.false.)
                name=name2
             enddo
          else
             do m=base+1,top
                name2=parser%stack(m)
                call push_null_val(parser)
                call push_sym_val(parser,name)
                call make_node(parser,sym_dotdotdot,2)
                call new_decl(parser,name2,modl_param,.false.)
                name=name2
             enddo
          endif
          parser%top=base
          if(parser%sym/=sym_comma) exit
          call scan(parser)
          cycle
       end if
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
  end function decparam

  ! Declarations
  subroutine decl(parser)
    type(parse_state),intent(inout):: parser
    type(pm_ptr):: modl
    integer:: dt
    type(pm_ptr):: old,p
    integer:: m,sym,name,name2,base,top
    integer:: serror
    call push_sym_val(parser,sym_pm_system)
    call push_val(parser,parser%sysmodl)
    call push_null_val(parser)
    call make_node(parser,sym_include,3)
    call new_decl(parser,sym_pm_system,modl_include,.false.)
    do 
       if(parser%sym==sym_include) then
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
             call skip_past_error(parser,.false.)
             exit
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
       sym=sym_include
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
             else
                if(sym==sym_proc) call scan(parser)
                call push_num_val(parser,modl_proc)
             endif
             if(expect_name(parser)) goto 998
             if(parser%sym==sym_cond) then
                call scan(parser)
                if(expect_name(parser)) goto 998
             else
                call push_null_val(parser)
             endif
             m=m+1
             if(parser%sym/=sym_comma) exit
          enddo
          call make_node(parser,sym_list,m*3)
          if(expect(parser,sym_close_brace)) goto 998
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
          goto 998
       endif
       cycle
998    call skip_past_error(parser,.false.)
    enddo
    do
       select case(parser%sym)
       case(sym_proc)
          call scan(parser)
          if(parser%sym==sym_dot) then
             call push_back(parser,sym_proc)
             exit
          else
             call push_back(parser,sym_proc)
          endif
          if(proc(parser)) goto 999
       case(sym_type)
          if(dectype(parser)) goto 999
       case(sym_param)
          if(decparam(parser)) goto 999
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
    call statement_list(parser)
    if(parser%sym/=sym_eof) then
       call parse_error(parser,'Malformed module')
    endif
    if(parser%error_count==0) then
       parser%modl%data%ptr(parser%modl%offset&
            +modl_stmts)=pop_val(parser)
    endif
10  continue

  end subroutine decl

  ! Skip tokens until out of expr or statement
  recursive subroutine skip_past_error(parser,expr)
    type(parse_state),intent(inout):: parser
    logical,intent(in):: expr
    if(parser_xtra_debug) write(*,*) 'Skipping',expr
    do
       if(expr) then
          if(parser%sym<=num_sym.and.parser%sym>last_expr) then
             call skip_past_error(parser,.false.)
             exit
          endif
          do while(parser%sym/=sym_eof.and.(parser%sym>num_sym.or.&
               parser%sym<=last_expr))
             if(parser%sym==sym_assign.or.parser%sym==sym_define) then
                call push_back(parser,sym_dom)
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
    parser%vline(parser%vtop)=parser%sym_lineno
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

  ! Drop top of value stack
  subroutine drop_val(parser)
    type(parse_state),intent(inout):: parser
    parser%vtop=parser%vtop-1
  end subroutine drop_val
  
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
    temp=parser%vstack(parser%vtop)
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
  subroutine make_node(parser,typeno,n,m)
    type(parse_state),intent(inout):: parser
    integer,intent(in):: typeno,n
    integer,intent(in),optional:: m
    type(pm_ptr):: val
    integer:: i,mm
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

    if(check_node_reuse.and.val%data%ptr(val%offset)%offset==9876) then
       ! Flag reused nodes if required (for debugging)
       val%data%ptr(val%offset)%offset=9875
    else
       ! Magic number
       val%data%ptr(val%offset)%offset=9876
    endif

    mm=n
    if(present(m)) mm=m
    
    val%data%ptr(val%offset+1)%data=>pm_null_obj%data
    val%data%ptr(val%offset+1)%offset=typeno
    val%data%ptr(val%offset+2)=parser%modl
    val%data%ptr(val%offset+3)%data=>pm_null_obj%data
    val%data%ptr(val%offset+3)%offset=parser%vline(parser%vtop-mm+1)
    if(val%offset+n+3>4096) call pm_panic('make_node')
    do i=1,n
       val%data%ptr(val%offset+i+3)=parser%vstack(parser%vtop+i-n)
    enddo
    parser%vtop=parser%vtop-n+1
    parser%vstack(parser%vtop)=val
    if(parser_xtra_debug) then
       write(*,*) '------New node------',n+4
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
    sym=pm_name_vector(parser%context,parser%stack,base,parser%top)
    call push_sym_val(parser,sym)
    parser%top=base
  contains
    include 'fnewnc.inc'
  end subroutine name_vector

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
    logical:: ok
    m=slot
    if(parser%error_count>0) return
    node=pop_val(parser)
    modl=parser%modl
    nameval=pm_fast_name(parser%context,int(name,pm_p))
    if(pm_name_is_local(parser%context,int(name,pm_p))) m=m+modl_local
    call pm_dict_set(parser%context,modl%data%ptr(modl%offset+m),&
         nameval,node,.true.,overwrt,ok)
    if(.not.ok) then
       call parse_error(parser,'Redefinition not allowed: '//&
            trim(pm_name_as_string(parser%context,int(name,pm_p))))
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
    character(len=100):: str
    m=slot
    modl=parser%modl
    call pm_name_string(parser%context,int(name,pm_p),str)
    nameval=pm_fast_name(parser%context,int(name,pm_p))
    if(pm_name_is_local(parser%context,int(name,pm_p))) m=m+modl_local
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
         (/ 'include','proc   ','type   ','default','param  '/)
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
          write(iunit,*) dnames(j-modl_include),&
               marked(ptr%data%ptr(ptr%offset+j+k)),'::'
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
       if(sym>0.and.sym<=num_sym) then
          write(iunit,*) spaces(1:depth*2),sym_names(sym),ptr%data%esize,&
               'line',node_get_lineno(ptr),&
               'Marked:',marked(ptr),&
               ptr%data%hash,ptr%offset,ptr%offset+ptr%data%esize
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
    if(pm_main_process) then
       if(parser_xtra_debug) write(*,*) '*****Error::',trim(emess)
       call pm_name_string(parser%context,&
            parser%modl%data%ptr(parser%modl%offset+modl_name)%offset,modname)
       write(*,*) 
       write(*,'(x,A,A,A,I4)') 'Error: ',trim(modname),' line:',parser%sym_lineno
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
       write(*,'(3x,a67)') caret
10     continue
       write(*,*) trim(emess)
       write(*,*)
    endif
    parser%error_count=parser%error_count+1
    if(parser%error_count>max_errors) then
       call pm_stop('Too many syntax errors - compilation terminated')
    endif
  end subroutine parse_error

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
    integer:: num
    type(pm_ptr):: p
    if(pm_debug_level>0) &
         call check_ptr_node(node)
    p=node%data%ptr(node%offset+n)
    num=p%offset
  end function node_get_num

  subroutine node_set_num(node,n,num)
    type(pm_ptr),intent(in):: node
    integer,intent(in):: n
    integer,intent(in):: num
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
