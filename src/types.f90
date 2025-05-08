!
! PM (Parallel Models) Programming Language
!
! Released under the MIT License (MIT)
!
! Copyright (c) Tim Bellerby, 2025
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

module pm_types
  use pm_kinds
  use pm_memory
  use pm_hash
  use pm_lib
  use pm_symbol
  use pm_options
  implicit none

  logical,parameter:: pm_type_extra_debug=.false.
  integer,parameter:: pm_max_type_args=128

  ! Flags for types
  integer,parameter:: pm_type_has_storage=32
  integer,parameter:: pm_type_has_distributed=64
  integer,parameter:: pm_type_has_array=128
  integer,parameter:: pm_type_has_poly=256
  integer,parameter:: pm_type_has_generic=512
  integer,parameter:: pm_type_has_vect=1024
  integer,parameter:: pm_type_has_fix_or_literal=2048
  integer,parameter:: pm_type_has_params=4096
  integer,parameter:: pm_type_is_soa=8192
  integer,parameter:: pm_type_is_aos=16384
  integer,parameter:: pm_type_is_seq=32768
  integer,parameter:: pm_type_leaves=65536

  integer,parameter:: pm_type_is_when=8192
  integer,parameter:: pm_type_is_yield=16384
  integer,parameter:: pm_type_is_list=32768

  ! Bitwise-or of flags which are not taints (only one so far)
  integer,parameter:: pm_type_flags_untainting = pm_type_is_list + pm_type_is_when + pm_type_is_yield + &
       pm_type_is_soa + pm_type_is_aos + pm_type_is_seq

  ! Type kind + default flags
  integer,parameter:: pm_type_new_user=1
  integer,parameter:: pm_type_new_error=2
  integer,parameter:: pm_type_new_rec=3
  integer,parameter:: pm_type_new_array=4+pm_type_has_array
  integer,parameter:: pm_type_new_tuple=5
  integer,parameter:: pm_type_new_vtuple=6
  integer,parameter:: pm_type_new_single_name=7
  integer,parameter:: pm_type_new_proc=8
  integer,parameter:: pm_type_new_all=9
  integer,parameter:: pm_type_new_any=10+pm_type_has_generic
  integer,parameter:: pm_type_new_poly=11+pm_type_has_poly+&
       pm_type_has_storage
  integer,parameter:: pm_type_new_fix_value=12
  integer,parameter:: pm_type_new_contains=13
  integer,parameter:: pm_type_new_fix=14        
  integer,parameter:: pm_type_new_dref=15
  integer,parameter:: pm_type_new_par_kind=16
  integer,parameter:: pm_type_new_proc_sig=17
  integer,parameter:: pm_type_new_undef_result=18
  integer,parameter:: pm_type_new_literal_value=19
  integer,parameter:: pm_type_new_except=20
  integer,parameter:: pm_type_new_param=21+pm_type_has_params

  integer,parameter:: pm_type_new_has=23
  integer,parameter:: pm_type_new_vect=24+pm_type_has_vect
  integer,parameter:: pm_type_new_params=25
  integer,parameter:: pm_type_new_type=26
  integer,parameter:: pm_type_new_category=27
  integer,parameter:: pm_type_new_bottom=28
  integer,parameter:: pm_type_new_includes=29
  integer,parameter:: pm_type_new_unfixed=30
  integer,parameter:: pm_type_new_uninitialised=31

  ! Type kinds
  integer,parameter:: pm_type_is_basic=0
  integer,parameter:: pm_type_is_user=1
  integer,parameter:: pm_type_is_error=2
  integer,parameter:: pm_type_is_rec=3
  integer,parameter:: pm_type_is_array=4
  integer,parameter:: pm_type_is_tuple=5
  integer,parameter:: pm_type_is_vtuple=6
  integer,parameter:: pm_type_is_single_name=7
  integer,parameter:: pm_type_is_proc=8
  integer,parameter:: pm_type_is_all=9
  integer,parameter:: pm_type_is_any=10
  integer,parameter:: pm_type_is_poly=11
  integer,parameter:: pm_type_is_fix_value=12
  integer,parameter:: pm_type_is_contains=13
  integer,parameter:: pm_type_is_fix=14
  integer,parameter:: pm_type_is_dref=15
  integer,parameter:: pm_type_is_par_kind=16
  integer,parameter:: pm_type_is_proc_sig=17
  integer,parameter:: pm_type_is_undef_result=18
  integer,parameter:: pm_type_is_literal_value=19
  integer,parameter:: pm_type_is_except=20
  integer,parameter:: pm_type_is_param=21
 !
  integer,parameter:: pm_type_is_has=23
  integer,parameter:: pm_type_is_vect=24
  integer,parameter:: pm_type_is_params=25
  integer,parameter:: pm_type_is_type=26
  integer,parameter:: pm_type_is_category=27
  integer,parameter:: pm_type_is_bottom=28
  integer,parameter:: pm_type_is_includes=29
  integer,parameter:: pm_type_is_literal=30
  integer,parameter:: pm_type_is_uninitialised=31

  integer,parameter:: pm_type_kind_mask=31
  integer,parameter:: pm_type_max_leaves=255

  ! Mode for type inclusion testing (type<>value,type<>type,type==type)
  integer,parameter:: pm_type_incl_val=1
  integer,parameter:: pm_type_incl_type=2
  integer,parameter:: pm_type_incl_equiv=4
  integer,parameter:: pm_type_incl_indirect=8
  integer,parameter:: pm_type_incl_nomatch=16
  integer,parameter:: pm_type_incl_extract=32

  integer,parameter:: pm_type_dref_offset=2000

  ! Return from struct/rec element lookup
  integer,parameter:: pm_elem_found=0
  integer,parameter:: pm_elem_not_found=1
  integer,parameter:: pm_elem_clash=2


  ! Error codes from type testing
  integer,parameter:: pm_type_err_none=0
  integer,parameter:: pm_type_err_elem=1
  integer,parameter:: pm_type_err_param=2
  integer,parameter:: pm_type_err_ambig=4
  integer,parameter:: pm_type_err_not_set=8
  integer,parameter:: pm_type_err_elem_clash=16
  integer,parameter:: pm_type_err_elem_not_found=32
  integer,parameter:: pm_type_err_elem_bad_type=64
  integer,parameter:: pm_type_err_elem_not_in_interface=128

  ! Maximum nesting of "type is" declarations
  integer,private,parameter:: max_user_nesting = 64

  ! System types (mainly used by VM)
  integer(pm_p),public,parameter:: pm_prc_type = pm_last_lib_type+1
  integer(pm_p),public,parameter:: pm_string_type=pm_last_lib_type+2
  integer(pm_p),public,parameter:: pm_poly_type=pm_last_lib_type+3
  integer(pm_p),public,parameter:: pm_rec_type=pm_last_lib_type+4
  integer(pm_p),public,parameter:: pm_polyref_type=pm_last_lib_type+5
  integer(pm_p),public,parameter:: pm_array_type=pm_last_lib_type+6
  integer(pm_p),public,parameter:: pm_const_array_type=pm_last_lib_type+7
  integer(pm_p),public,parameter:: pm_dref_type=pm_last_lib_type+8
  integer(pm_p),public,parameter:: pm_dref_shared_type=pm_last_lib_type+9
  integer(pm_p),public,parameter:: pm_elemref_type=pm_last_lib_type+10
  integer(pm_p),public,parameter:: pm_last_sys_type=pm_elemref_type

  ! Categorical types
  integer,public,parameter:: pm_a_rec_type = pm_last_sys_type + 1
  integer,public,parameter:: pm_a_unique_type = pm_last_sys_type + 2
  integer,public,parameter:: pm_a_fix_type = pm_last_sys_type + 3
  integer,public,parameter:: pm_a_literal_type = pm_last_sys_type + 4
  integer,public,parameter:: pm_a_basic_type = pm_last_sys_type + 5
  integer,public,parameter:: pm_last_category_type = pm_a_basic_type

  ! Literal types
  integer,public,parameter:: pm_int_literal_type = pm_last_category_type + 1
  integer,public,parameter:: pm_real_literal_type = pm_last_category_type + 2
  integer,public,parameter:: pm_bool_literal_type = pm_last_category_type + 3
  integer,public,parameter:: pm_string_literal_type = pm_last_category_type + 4
  integer,public,parameter:: pm_last_literal_type = pm_string_literal_type
  
    
  ! Kind of dref type (internal type describing references)
  integer,public,parameter:: pm_dref_is_dot=0
  integer,public,parameter:: pm_dref_is_var=-1
  integer,public,parameter:: pm_dref_is_shared=-2
  integer,public,parameter:: pm_dref_is_slice=-3
  integer,public,parameter:: pm_dref_is_shared_slice=-4
  integer,public,parameter:: pm_dref_is_here=-5
  integer,public,parameter:: pm_dref_is_ref=-6
  integer,public,parameter:: pm_dref_is_any=-7
  integer,public,parameter:: pm_dref_is_any_slice=-8

  integer,private,parameter:: mode_mask=1023

  integer,public,parameter:: pm_partial_mode = (mode_mask+1)**2
  integer,public,parameter:: pm_complete_mode = 2*(mode_mask+1)**2
  
contains

  ! Initialise type system
  subroutine pm_init_types(context)
    type(pm_context),pointer:: context
    integer:: i,j
    integer,dimension(3):: key
    integer:: flags
    character(len=15),dimension(pm_last_category_type),parameter:: base_types= (/&
    'PM__tinyint   ','proc          ','type          ','name          ',&
    'null          ','sint          ','int           ','lint          ',&
    'int8          ','int16         ','int32         ','int64         ',&
    '<int128>      ','sreal         ','real          ','<real32>      ',&
    '<real64>      ','<real128>     ','scpx          ','cpx           ',&
    '<cpx64>       ','<cpx128>      ','<cpx256>      ','bool          ',&
    '<packbool>    ','<ext>         ','<char>        ','<pointer>     ',&
    '<stack>       ','<usr>         ','<dict>        ','<set>         ',&
    'prc_info      ','string        ','<poly>        ',&
    '<rec>         ','<polyref>     ','<array>       ','<cstarray>    ',&
    '<dref>        ','<dref-inv>    ','<elemref>     ',&
    'a_rec         ','a_unique      ','a_fix         ','a_literal     ',&
    'a_basic       '/)
    
    context%tcache=pm_dict_new(context,128_pm_ln)
    context%pcache=pm_dict_new(context,1024_pm_ln)

    key(1)=pm_type_is_basic
    do i=1,pm_null
       key(2)=pm_intern(context,trim(base_types(i)))
       if(pm_debug_level>2) then
          write(*,*) 'Init types(',trim(base_types(i)),')',key(2)
       endif
       j=pm_idict_add(context,context%tcache,key,2,&
            pm_null_obj)
       if(j/=i) call pm_panic('init_type')
    enddo
    key(1)=pm_type_is_basic+pm_type_has_storage+pm_type_leaves
    do i=pm_null+1,pm_last_category_type
       key(2)=pm_intern(context,trim(base_types(i)))
       if(pm_debug_level>2) then
          write(*,*) 'Init types(',trim(base_types(i)),')',key(2)
       endif
       j=pm_idict_add(context,context%tcache,key,2,&
            pm_null_obj)
       if(j/=i) call pm_panic('init_type')
       if(i==pm_last_sys_type) then
          key(1)=pm_type_new_category
       endif
    enddo
    key(1)=pm_type_new_unfixed
    key(2)=0
    key(3)=pm_long
    j=pm_idict_add(context,context%tcache,key,3,&
         pm_null_obj)
    key(3)=pm_single
    j=pm_idict_add(context,context%tcache,key,3,&
         pm_null_obj)
    key(3)=pm_logical
    j=pm_idict_add(context,context%tcache,key,3,&
         pm_null_obj)
    key(3)=pm_string_type
    j=pm_idict_add(context,context%tcache,key,3,&
         pm_null_obj)
    
    key(1)=pm_type_is_user
    do i=1,pm_last_category_type
       if(base_types(i)(1:1)/='<') then
          key(2)=pm_intern(context,trim(base_types(i)))
          j=pm_idict_add(context,context%tcache,key,2,&
               pm_fast_typeno(context,i))
       endif
    enddo
    if(pm_debug_level>2) write(*,*) 'Types inited'
  contains
    include 'ftypeno.inc'
  end subroutine pm_init_types

  !============================================================
  ! Make type description node
  ! arr must contain type_kind type_name arg1 arg2 ...
  ! Basic version that does not accumulate flags
  ! - flags can be specified in an optional argument
  ! Val optional argument gives value associated with the type
  !============================================================
  function pm_new_basic_type(context,arr,val,flags) result(tno)
    type(pm_context),pointer:: context
    integer,dimension(:),intent(inout):: arr
    type(pm_ptr),intent(in),optional:: val
    integer,intent(in),optional:: flags
    integer:: tno
    integer:: k
    type(pm_ptr):: tval
    if(present(val)) then
       tval=val
    else
       tval=pm_fast_tinyint(context,0)
    endif
    if(arr(1)<0) then
       write(*,*) 'bad',arr
       call pm_panic('bad kind')
    endif
    if(present(flags)) then
       arr(1)=ior(arr(1),iand(flags,not(pm_type_kind_mask+pm_type_flags_untainting)))
    endif
    k=pm_ivect_lookup(context,context%tcache, &
         arr,size(arr))
    if(k==0) k=pm_idict_add(context,context%tcache,&
         arr,size(arr),tval)
    tno=k
  contains
    include 'ftiny.inc'
  end function pm_new_basic_type
  
  !=============================================================
  ! Make type description record returning type number
  ! Types are hashed so the same kind/name/args always gives the
  ! same number
  ! arr must contain type_kind type_name arg1 arg2 ...
  ! val optional argument gives value associated with the type
  !=============================================================
  function pm_new_type(context,arr,val) result(tno)
    type(pm_context),pointer:: context
    integer,dimension(:),intent(inout):: arr
    type(pm_ptr),intent(in),optional:: val
    integer:: tno
    integer:: k
    type(pm_ptr):: tval
    integer:: tflags,nleaves,flags
    if(present(val)) then
       tval=val
    else
       tval=pm_fast_tinyint(context,0)
    endif
    if(arr(1)<0) then
       write(*,*) 'bad',arr
       call pm_panic('bad kind')
    endif

    ! Calculate combined flags and also total number of leaves
    tflags=0
    nleaves=0
    do k=3,size(arr)
       flags=pm_type_flags(context,arr(k))
       tflags=ior(tflags,flags)
       nleaves=max(nleaves+flags/pm_type_leaves,pm_type_max_leaves)
    enddo
    arr(1)=ior(arr(1),iand(tflags,&
         iand(pm_type_leaves-1,not(pm_type_kind_mask+pm_type_flags_untainting))))+&
         nleaves*pm_type_leaves
    k=pm_ivect_lookup(context,context%tcache, &
         arr,size(arr))
    if(k==0) k=pm_idict_add(context,context%tcache,&
         arr,size(arr),tval)
    tno=k
  contains
    include 'ftiny.inc'
  end function pm_new_type


  !==========================================================================
  ! Associate a type with a name (not necessarily the same as the type name)
  ! Used by struct/rec declarations
  !==========================================================================
  subroutine pm_type_record_by_name(context,name,typ)
    type(pm_context),pointer:: context
    integer,intent(in):: name,typ
    integer(pm_ln):: k
    type(pm_ptr):: key,val
    logical:: ok
    key=pm_fast_tinyint(context,name)
    val=pm_fast_tinyint(context,typ)
    call pm_dict_set(context,context%pcache,key,val,.true.,.true.,ok)
  contains
    include 'ftiny.inc'
  end subroutine pm_type_record_by_name

  !==============================================================================
  ! Retrieve type associated with a given name (again not same as the type name)
  ! Used by struct/rec declarations
  !==============================================================================
  function pm_type_from_recorded_name(context,name) result(typ)
    type(pm_context),pointer:: context
    integer,intent(in):: name
    integer:: typ
    type(pm_ptr):: key,val
    key=pm_fast_tinyint(context,name)
    val=pm_dict_lookup(context,context%pcache,key)
    typ=val%offset
  contains
    include 'ftiny.inc'
  end function pm_type_from_recorded_name

  !====================================================
  ! New parameters type with n parameters
  !====================================================
  function pm_new_params_type(context,n,typ) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: n,typ
    integer:: tno
    integer:: arr(3)
    arr(1)=pm_type_new_params
    arr(2)=n
    arr(3)=typ
    tno=pm_new_basic_type(context,arr,&
         flags=iand(pm_type_flags(context,typ),not(pm_type_has_params)))   
  end function pm_new_params_type

  !=======================================
  ! Create new user type with body tno2
  !=======================================
  function pm_new_user_type(context,arr,tno2) result(tno)
    type(pm_context),pointer:: context
    integer,dimension(:),intent(inout):: arr
    integer:: tno2
    integer:: tno
    tno=pm_new_basic_type(context,arr,&
         val=pm_fast_typeno(context,tno2))
  contains
    include 'ftypeno.inc'
  end function pm_new_user_type

  !========================================
  ! Create new array type: etyp[dtyp]
  !========================================
  function pm_new_arr_type(context,kind,etyp,dtyp,styp) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: kind
    integer,intent(in):: etyp
    integer,intent(in):: dtyp
    integer,intent(in):: styp
    integer:: tno
    integer,dimension(5):: args
    integer:: flags
    flags=pm_type_is_array+merge(pm_type_has_array,0,kind/=sym_fix)
    args(1)=flags
    args(2)=kind
    args(3)=etyp
    args(4)=dtyp
    args(5)=styp 
    tno=pm_new_type(context,args)
  end function pm_new_arr_type

  !=========================
  ! Create type a or b
  !=========================
  function pm_type_combine(context,a,b) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: a,b
    integer:: tno
    integer,dimension(4):: args
 
    if(a==b) then
       tno=a
       return
    elseif(pm_type_includes(context,a,b,pm_type_incl_type)) then
       tno=a
       return
    elseif(pm_type_includes(context,b,a,pm_type_incl_type)) then
       tno=b
       return
    endif
    args(1)=pm_type_new_any
    args(2)=0
    args(3)=a
    args(4)=b
    tno=pm_new_type(context,args)
  end function pm_type_combine

  !==========================================
  ! Create new polymorphic type: @etype
  !==========================================
  function pm_new_poly_type(context,etyp) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: etyp
    integer:: tno
    integer,dimension(3):: args
    args(1)=pm_type_new_poly
    args(2)=0
    args(3)=etyp
    tno=pm_new_basic_type(context,args)
  end function pm_new_poly_type

  !==========================================
  !  Create new type-value type: <type>
  !==========================================
  function pm_new_type_type(context,etyp) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: etyp
    integer:: tno
    integer,dimension(3):: args
    args(1)=pm_type_new_type
    args(2)=0
    args(3)=etyp
    tno=pm_new_basic_type(context,args)
  end function pm_new_type_type

  
  !===============================================
  ! Create new includes: type inc type
  !================================================
  function pm_new_includes_type(context,etyp,mtyp) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: etyp,mtyp
    integer:: tno
    integer,dimension(4):: args
    args(1)=pm_type_new_includes
    args(2)=0
    args(3)=etyp
    args(4)=mtyp
    tno=pm_new_basic_type(context,args)
  end function pm_new_includes_type

  !==============================================
  ! Create new compile time name value type
  !==============================================
  function pm_new_name_type(context,name) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: name
    integer:: tno
    integer,dimension(2):: args
    args(1)=pm_type_new_single_name
    if(pm_name_stem(context,name)==sym_distr_tag)  then
       args(1)=ior(args(1),pm_type_has_distributed)
    endif
    args(2)=name
    tno=pm_new_type(context,args)
  end function pm_new_name_type

  !==========================================
  ! Create new compile time value type
  !==========================================
  function pm_new_fix_value_type(context,val,vindex) result(tno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: val
    integer,intent(in),optional:: vindex
    integer:: tno
    integer,dimension(3):: args
    args(1)=pm_type_new_fix_value
    if(present(vindex)) then
       args(2)=vindex
    else
       args(2)=pm_set_add(context,context%names,val)
    endif
    args(3)=pm_fast_typeof(val)
    if(args(3)==pm_string) args(3)=pm_string_type
    tno=pm_new_basic_type(context,args,val)
  contains
    include 'ftypeof.inc'
  end function pm_new_fix_value_type

  function pm_fix_value_type_from_literal(context,tno) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: tno2
    integer:: args(3)
    type(pm_ptr):: tv
    tv=pm_type_vect(context,tno)
    args(1)=pm_type_new_fix_value
    args(2)=pm_tv_name(tv)
    args(3)=pm_tv_arg(tv,1)
    tno2=pm_new_basic_type(context,args,pm_type_val(context,tno))
  end function pm_fix_value_type_from_literal

  !==========================================
  ! Create new compile time value type
  !==========================================
  function pm_new_literal_value_type(context,val,vindex) result(tno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: val
    integer,intent(in),optional:: vindex
    integer:: tno
    integer,dimension(3):: args
    args(1)=pm_type_new_literal_value
    if(present(vindex)) then
       args(2)=vindex
    else
       args(2)=pm_set_add(context,context%names,val)
    endif
    args(3)=pm_fast_typeof(val)
    if(args(3)==pm_string) args(3)=pm_string_type
    tno=pm_new_basic_type(context,args,val)
  contains
    include 'ftypeof.inc'
  end function pm_new_literal_value_type

  !==============================================
  ! Create new pending error type
  !==============================================
  function pm_new_error_type(context,val) result(tno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: val
    integer:: tno
    integer,dimension(2):: args
    args(1)=pm_type_new_error
    args(2)=pm_set_add(context,context%names,val)
    tno=pm_new_basic_type(context,args,val)
  end function pm_new_error_type

  !==============================================
  ! Create new pending error type from a string
  !==============================================
  function pm_error_type_from_string(context,str) result(tno)
    type(pm_context),pointer:: context
    character(len=*):: str
    integer:: tno
    tno=pm_new_error_type(context,pm_new_string(context,str))
  end function pm_error_type_from_string

  
  !=============================================
  ! Create new compile time proc value type
  !=============================================
  function pm_new_proc_type(context,name) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: name
    integer:: tno
    integer,dimension(2):: args
    args(1)=pm_type_new_proc
    args(2)=name
    tno=pm_new_type(context,args)
  end function pm_new_proc_type
  
  !==========================================
  ! Create internal vector type ^^(T)
  !==========================================
  function pm_new_vect_type(context,tno) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: tno2
    integer,dimension(3):: args
    args(1)=pm_type_new_vect
    args(2)=0
    args(3)=tno
    tno2=pm_new_type(context,args)
  end function pm_new_vect_type
  
  !=================================================
  ! Change the value associated with a type
  ! (used for user and value types)
  !=================================================
  subroutine pm_type_set_val(context,tno,val)
    type(pm_context),pointer:: context
    integer:: tno
    type(pm_ptr),intent(in):: val
    call pm_dict_set_val(context,context%tcache,int(tno,pm_ln),val)
  end subroutine pm_type_set_val

  !===================================
  ! Return the kind of a given type
  !===================================
  function pm_type_kind(context,tno) result(kind)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: kind
    if(tno<=0) then
       kind=0
    else
       kind=pm_tv_kind(pm_type_vect(context,tno))
    endif
  end function pm_type_kind

  !=========================================
  ! Return flags for a given type
  !=========================================
  function pm_type_flags(context,tno) result(flags)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: flags
    integer:: tno2
    type(pm_ptr):: tv
    if(tno<=0) then
       flags=pm_type_has_generic
       return
    else
       tv=pm_type_vect(context,tno)
       flags=pm_tv_flags(tv)
       tno2=tno
       do while(iand(flags,pm_type_kind_mask)==pm_type_is_user)
          tv=pm_dict_val(context,context%tcache,int(tno2,pm_ln))
          tno2=tv%offset
          if(tno2/=0) then
             tv=pm_type_vect(context,tno2)
             flags=pm_tv_flags(tv)
          else
             flags=pm_type_has_generic
          endif
       enddo
    endif
  contains
    include 'fvkind.inc'
  end function pm_type_flags

  !=================================================
  ! Return number of leaves associated with a type
  !=================================================
  function pm_type_needs_storage(context,tno) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    logical:: ok
    ok=iand(pm_type_flags(context,tno),pm_type_has_storage)/=0
  end function pm_type_needs_storage

  !=================================================
  ! Return number of leaves associated with a type
  !=================================================
  function pm_type_num_leaves(context,tno) result(n)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: n
    n=pm_type_flags(context,tno)/pm_type_leaves
  end function pm_type_num_leaves

  !=================================================
  ! Return number of arguments associated with a type
  !=================================================
  function pm_type_numargs(context,tno) result(n)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: n
    n=pm_fast_esize(pm_type_vect(context,tno))-1
  contains
    include 'fesize.inc'
  end function pm_type_numargs

  !=================================================
  ! Return argument #n of type tno
  !=================================================
  function pm_type_arg(context,tno,n) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,n
    integer:: tno2
    type(pm_ptr):: tv
    tv=pm_type_vect(context,tno)
    tno2=pm_tv_arg(tv,n)
  end function pm_type_arg

  !=================================================
  ! Return name associated with type tno
  !=================================================
  function pm_type_name(context,tno) result(name)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: name
    type(pm_ptr):: tv
    tv=pm_type_vect(context,tno)
    name=pm_tv_name(tv)
  end function pm_type_name

  !=====================================================
  ! Is type tuple with when?
  !==============================---===================
  function pm_type_has_when(context,tno) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    logical:: ok
    type(pm_ptr):: tv
    tv=pm_type_vect(context,tno)
    ok=iand(pm_tv_flags(tv),pm_type_is_when)/=0
  end function pm_type_has_when

  !=====================================================
  ! Return amp locs for tuple type
  !==============================---===================
  function pm_type_amp(context,tno) result(amp)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: amp
    type(pm_ptr):: tv
    tv=pm_type_vect(context,tno)
    amp=pm_tv_name(tv)
  end function pm_type_amp

  !=====================================================
  ! Return name of element #n associated with type tno
  !==============================---===================
  function pm_type_elem_name(context,tno,n) result(name)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,n
    integer:: name,kind
    type(pm_ptr):: tv,namev
    tv=pm_type_vect(context,tno)
    if(pm_debug_checks) then
       kind=pm_tv_kind(tv)
       if(kind/=pm_type_is_rec) then
          write(*,*) 'tno=',tno,'kind=',kind
          call pm_panic('typ_elem_name not rec')
       endif
    endif
    name=pm_tv_name(tv)
    namev=pm_name_val(context,name)
    name=namev%data%i(namev%offset+n)
  end function pm_type_elem_name
  
  !==========================================
  ! Look up type and return number
  ! (returns zero if not found)
  !==========================================
  function pm_type_lookup(context,arr) result(tno)
    type(pm_context),pointer:: context
    integer,dimension(:):: arr
    integer:: tno
    tno=pm_ivect_lookup(context,context%tcache, &
         arr,size(arr))
  end function pm_type_lookup

  !===============================================================
  ! Look up user type and return type number it is defined to be
  !===============================================================
  function pm_user_type_lookup(context,arr) result(tno)
    type(pm_context),pointer:: context
    integer,dimension(:):: arr
    integer:: tno
    type(pm_ptr):: p
    tno=pm_ivect_lookup(context,context%tcache, &
         arr,size(arr))
    if(tno>0) then
       p=pm_dict_val(context,context%tcache,int(tno,pm_ln))
       tno=p%offset
    else
       tno=-1
    endif
  end function pm_user_type_lookup

  !====================================================
  ! Lookup parameterless user type with given name
  !====================================================
  function pm_user_type_lookup_by_name(context,name) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: name
    integer:: tno
    integer:: arr(2)
    arr(1)=pm_type_new_user
    arr(2)=name
    tno=pm_user_type_lookup(context,arr)
  end function pm_user_type_lookup_by_name

  !====================================================
  ! Get type number of body of user type definition
  !====================================================
  function pm_user_type_body(context,typ) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer:: tno
    type(pm_ptr):: v
    v=pm_dict_val(context,context%tcache,int(typ,pm_ln))
    tno=v%offset
  end function pm_user_type_body

  !==============================================
  ! Set the body of a user type declaration
  !==============================================
  subroutine pm_user_type_set_body(context,typ,tno)
    type(pm_context),pointer:: context
    integer,intent(in):: typ,tno
    call pm_type_set_val(context,typ,&
         pm_fast_typeno(context,tno))
  contains
    include 'ftypeno.inc'
  end subroutine pm_user_type_set_body

  !=======================================================
  ! If basic numeric type return name, else return -1
  ! (mainly used for casting)
  !=======================================================
  function pm_type_numeric_name(context,typ) result(name)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer:: name,tno
    type(pm_ptr):: tv
    tno=typ
    if(tno==0) then
       name=-1
       return
    endif
    tv=pm_type_vect(context,typ)
    if(pm_tv_kind(tv)==pm_type_is_basic) then
       name=pm_tv_name(tv)
    elseif(pm_tv_kind(tv)==pm_type_is_user) then
       tno=pm_user_type_body(context,typ)
       tv=pm_type_vect(context,tno)
       if(pm_tv_kind(tv)==pm_type_is_basic) then
          name=pm_tv_name(tv)
       else
          name=-1
       endif
    else
       name=-1
    endif
    if(tno<pm_int.or.tno>=pm_logical) name=-1
  end function pm_type_numeric_name

  !===============================================
  ! Value associate with value or const type
  !===============================================
  function pm_type_val(context,typ) result(v)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    type(pm_ptr):: v
    v=pm_dict_val(context,context%tcache,int(typ,pm_ln))
  end function pm_type_val

  !===============================================
  ! Strip off non-storage elements of a type
  !===============================================
  recursive function pm_type_strip_to_basic(context,typ) result(typ2)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer:: typ2
    type(pm_ptr):: tv
    integer:: kind
    if(typ==0) then
       typ2=0
       return
    endif
    tv=pm_type_vect(context,typ)
    kind=pm_tv_kind(tv)
    select case(kind)
    case(pm_type_is_all,pm_type_is_vect,&
         pm_type_is_param,&
         pm_type_is_fix_value,pm_type_is_literal_value)
       typ2=pm_type_strip_to_basic(context,pm_tv_arg(tv,1))
    case(pm_type_is_user)
       typ2=pm_user_type_body(context,typ)
    case default
       typ2=typ
    end select
  end function pm_type_strip_to_basic

  !==============================================
  ! Get mode from type (default private)
  !==============================================
  function pm_type_get_mode(context,typ) result(mode)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer:: mode
    integer:: tk
    type(pm_ptr):: tv
    if(typ<=0) then
       mode=sym_invar
       return
    endif
    tv=pm_type_vect(context,typ)
    if(pm_tv_kind(tv)==pm_type_is_par_kind) then
       mode=iand(pm_tv_name(tv),mode_mask)
    else
       mode=sym_private
    endif
  end function pm_type_get_mode

  !=========================================================================
  ! Strip mode information, mode, from type typ yielding unmoded type typ2
  !=========================================================================
  function pm_type_strip_mode(context,typ,mode) result(typ2)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer,intent(out):: mode
    integer:: typ2
    integer:: tk
    type(pm_ptr):: tv
    if(typ<=0) then
       typ2=typ
       mode=merge(sym_private,sym_invar,typ==0)
       return
    endif
    tv=pm_type_vect(context,typ)
    if(pm_tv_kind(tv)==pm_type_is_par_kind) then
       mode=iand(pm_tv_name(tv),mode_mask)
       typ2=pm_tv_arg(tv,1)
    else
       mode=sym_private
       typ2=typ
    endif
  end function pm_type_strip_mode

  !=============================================
  ! Add mode information to an unmoded type
  !=============================================
  function pm_type_add_mode(context,typ,mode,istype) result(typ2)
    type(pm_context),pointer:: context
    integer,intent(in):: typ,mode
    logical,intent(in),optional:: istype
    integer:: typ2,mode2,typ3
    integer:: array(3)
    if(typ<0) then
       typ2=typ
       return
    endif
    typ3=pm_type_strip_mode(context,typ,mode2)
    if(pm_debug_checks) then
       if(mode2/=sym_private) then
          write(*,*) trim(sym_names(mode2))
          call pm_panic('add-mode to moded type')
       endif
    endif
    if(mode==sym_private.and..not.present(istype)) then
       typ2=typ
    else
       array(1)=pm_type_new_par_kind
       array(2)=mode
       array(3)=typ
       typ2=pm_new_type(context,array)
    endif
  end function pm_type_add_mode

  !========================================================
  ! Replace mode information in a (possibly) moded type
  !========================================================
  function pm_type_replace_mode(context,typ1,mode) result(typ2)
    type(pm_context),pointer:: context
    integer,intent(in):: typ1,mode
    integer:: typ2
    integer:: array(3),typ
    type(pm_ptr):: tv
    if(typ1<=0) then
       typ2=typ1
       return
    endif
    tv=pm_type_vect(context,typ1)
    if(pm_tv_kind(tv)==pm_type_is_par_kind) then
       typ=pm_tv_arg(tv,1)
    else
       typ=typ1
    endif
    if(mode==sym_private) then
       typ2=typ
    else
       array(1)=pm_type_new_par_kind
       array(2)=mode
       array(3)=typ
       typ2=pm_new_type(context,array)
    endif
  end function pm_type_replace_mode

  
  !=============================================================================================
  ! Rules for combining modes in a standard procedure call or stucture creation
  !
  ! Error codes:
  !   combined_mode=-1,-2...
  !        Shared distributed value not allowed for position -combined_mode
  !  shared_ok -- permissible to have an argumnet with 'shared' mode
  !============================================================================================
  function pm_type_combine_modes(context,array,is_cond,shared_ok) result(combined_mode)
    type(pm_context),pointer:: context
    integer,intent(in),dimension(:):: array
    logical,intent(in):: is_cond,shared_ok
    integer:: combined_mode
    integer:: i,mode,cmode,tno
    cmode=sym_invar
    do i=1,size(array)
       tno=pm_type_strip_mode(context,array(i),mode)
       if(mode==sym_shared.and..not.shared_ok) then
          combined_mode=-i
          return
       endif
       cmode=min(cmode,mode)
    enddo
    if(cmode<sym_uniform) cmode=sym_private
    if(is_cond.and.cmode==sym_invar) cmode=sym_uniform
    combined_mode=cmode
  end function pm_type_combine_modes

 !=====================================================================
 ! Rules for mixing modes in a list or reference
 !=====================================================================
  function pm_type_mix_modes(context,array) result(mixed_mode)
    type(pm_context),pointer:: context
    integer,intent(in),dimension(:):: array
    integer:: mixed_mode
    integer:: i,mode,cmax,cmin,tno
    cmax=sym_private
    cmin=sym_shared
    do i=1,size(array)
       tno=pm_type_strip_mode(context,array(i),mode)
       cmin=min(cmin,mode)
       cmax=max(cmax,mode)
    enddo
    if(cmin>=sym_joint) then
       mixed_mode=cmin
    elseif(cmax>=sym_joint) then
       mixed_mode=sym_joint
    else
       mixed_mode=sym_private
    endif
  end function pm_type_mix_modes
  
  !===================================
  ! Does mode1 include mode2 ?
  !===================================
  function pm_mode_includes(mode1,mode2) result(ok)
    integer,intent(in):: mode1,mode2
    logical:: ok
    if(mode1<sym_private.or.mode1==sym_uniform) then
       select case(mode1)
       case(sym_local)
          ok=mode2==sym_individual.or.&
               mode2>=sym_private.and.mode2<=sym_invar
       case(sym_global)
          ok=mode2>=sym_invar
       case(sym_complete)
          ok=mode2>=sym_chan.and.mode2/=sym_uniform.and.mode2/=sym_joint
       case(sym_connected)
          ok=mode2>sym_private.or.mode2==sym_global&
               .or.mode2==sym_complete
       case(sym_individual) 
          ok=mode2>=sym_private.and.mode2<sym_uniform
       case(sym_uniform) 
          ok=mode2==sym_uniform.or.mode2==sym_invar
       case default
          call pm_panic('pm_mode_includes')
       end select
    endif
    ok=mode1==mode2
  end function pm_mode_includes

  !==========================================
  ! Is mode2 compatable with run-mode mode1
  !==========================================
  function pm_mode_compatable(mode1,mode2) result(ok)
    integer,intent(in):: mode1,mode2
    logical:: ok
    if(mode2>=mode1) then
       ok=.true.
    elseif(mode2==sym_invar) then
       ok=.true.
    endif
  end function pm_mode_compatable

  
  !==========================================================
  ! Remove both mode information and internal vector type
  !==========================================================
  function pm_type_strip_mode_and_vect(context,tno) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: tno2
    type(pm_ptr):: tv
    integer:: mode
    tno2=pm_type_strip_mode(context,tno,mode)
    tv=pm_type_vect(context,tno2)
    if(pm_tv_kind(tv)==pm_type_is_vect) then
       tno2=pm_type_strip_mode(context,pm_tv_arg(tv,1),mode)
    endif
  end function pm_type_strip_mode_and_vect

  !=============================================================================
  ! Check if two concrete types equal (ignoring modes and vector type wrappers)
  !=============================================================================
  function pm_type_equal(context,tno1,tno2) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno1,tno2
    logical:: ok
    type(pm_ptr):: tv1,tv2
    integer:: typ1,typ2,tk1,tk2
    if(tno1==tno2) then
       ok=.true.
    else
       tv1=pm_type_vect(context,tno1)
       tv2=pm_type_vect(context,tno2)
       tk1=pm_tv_kind(tv1)
       tk2=pm_tv_kind(tv2)
       typ1=tno1
       typ2=tno2
       if(tk1==pm_type_is_par_kind.or.tk1==pm_type_is_vect) then
          typ1=pm_tv_arg(tv1,1)
       endif
       if(tk2==pm_type_is_par_kind.or.tk2==pm_type_is_vect) then
          typ2=pm_tv_arg(tv2,1)
       endif
       ok=typ1==typ2
       if(.not.ok) then
          tv1=pm_type_vect(context,typ1)
          tv2=pm_type_vect(context,typ2)
          tk1=pm_tv_kind(tv1)
          tk2=pm_tv_kind(tv2)
          if(tk1==pm_type_is_par_kind.or.tk1==pm_type_is_vect) then
             typ1=pm_tv_arg(tv1,1)
          endif
          if(tk2==pm_type_is_par_kind.or.tk2==pm_type_is_vect) then
             typ2=pm_tv_arg(tv2,1)
          endif
          ok=typ1==typ2
       endif
    endif
  end function pm_type_equal

  !===================================================================
  ! Given a struct/rec template and type, return type parameters
  !==================================================================
  function pm_type_extract_params(context,templ,typ,params) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: templ,typ
    integer,intent(inout),dimension(:):: params
    logical:: ok
    integer:: ubase
    integer,dimension(max_user_nesting):: user
    ubase=1
    ok=pm_test_type_includes(context,templ,typ,&
         pm_type_incl_val+pm_type_incl_extract,params,1,user,ubase)
  end function pm_type_extract_params

  !======================================
  ! Does supertype include subtype?
  !======================================
  function pm_type_includes(context,supertype,subtype,&
       mode) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: supertype,subtype
    integer,intent(in):: mode
    logical:: ok
    integer:: ubase
    integer,dimension(max_user_nesting):: user,params

    if(pm_type_extra_debug) then
       write(*,*) 'CHECK',trim(pm_type_as_string(context,supertype)),'>>',&
            trim(pm_type_as_string(context,subtype))
    endif
    ubase=1
    
    ! This deals with rare problem of *(..T..) where T is struct/rec parameter
    if(iand(mode,pm_type_incl_indirect)/=0) then
       if(iand(pm_type_flags(context,supertype),pm_type_has_params)/=0) then
          params=-1
       endif
    endif

    ok=pm_test_type_includes(context,supertype,subtype,&
         mode,params,1,user,ubase)
    if(pm_type_extra_debug) then
       write(*,*) 'CHECKED ',ok,trim(pm_type_as_string(context,supertype)),'>>',&
            trim(pm_type_as_string(context,subtype))
    endif
  end function pm_type_includes

  !======================================================
  ! Does supertype include subtype?
  ! This routine does the work
  ! Working space:
  !    params(base:) storage for matching parameters
  !    user(ubase:)  maintains a stack of active user types to
  !                  prevent runaway recursion
  !======================================================
  recursive function pm_test_type_includes(context,supertype,subtype,&
       mode,params,base,user,ubase)&
       result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: supertype,subtype
    integer,intent(in):: mode
    integer,dimension(:),intent(inout):: params
    integer,intent(in):: base
    integer,dimension(:),intent(inout):: user
    integer,intent(in):: ubase
    logical:: ok
    integer:: p,q,s
    type(pm_ptr):: t,u,r
    integer:: i,j,tk,uk,nt,nu
    logical:: has_d

    if(pm_type_extra_debug) then
       write(*,*) '=================='
       write(*,*) 'Test incl',supertype,subtype
       write(*,*) trim(pm_type_as_string(context,supertype)),' ',&
            trim(pm_type_as_string(context,subtype))
       write(*,*) 'base=',base
       write(*,*) '=================='
    endif

    p=supertype
    q=subtype

    ! Check for case where one of the type is any
    if(p==0.or.q==0) then
       if(p==0) then
          ok=.true.
       else
          t=pm_type_vect(context,p)
          tk=pm_tv_kind(t)
          if(tk==pm_type_is_user) then
             r=pm_dict_val(context,context%tcache,int(p,pm_ln))
             ok=pm_test_type_includes(context,int(r%offset),q,&
                  mode,params,base,user,ubase)
          elseif(tk==pm_type_is_any) then
             do i=1,pm_tv_numargs(t)
                if(pm_test_type_includes(context,pm_tv_arg(t,i),q,&
                     mode,params,base,user,ubase)) then
                   ok=.true.
                   return
                endif
             enddo
             ok=.false.
          else
             ok=.false.
          endif
       endif
       return
    endif

    ! Always true if types are equal
    if(p==q) then
       ok=.true.
       return
    endif
    
    t=pm_type_vect(context,p)
    u=pm_type_vect(context,q)
    uk=pm_tv_kind(u)
    tk=pm_tv_kind(t)

    if(tk==pm_type_is_includes) then
       if(uk==pm_type_is_includes) then
          ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               ior(mode,pm_type_incl_nomatch),params,base,user,ubase)
          if(ok) then
             ok=ok.and.pm_test_type_includes(context,&
                  pm_tv_arg(u,2),pm_tv_arg(t,2),&
                  pm_type_incl_equiv,params,base,user,ubase)
          endif
       else
          ok=pm_test_type_includes(context,pm_tv_arg(t,1),q,&
               ior(mode,pm_type_incl_nomatch),params,base,user,ubase)
          if(ok) then
             ok=ok.and.pm_test_type_includes(context,&
                  q,pm_tv_arg(t,2),&
                  ior(mode,pm_type_incl_nomatch),params,base,user,ubase)
          endif
       endif
       return
    endif

    
    ! Cases where the second type needs to be checked first
    select case(uk)
    case(pm_type_is_proc)
       if(p==pm_proc) then
          ok=.true.
          return
       endif
    case(pm_type_is_single_name)
       if(p==pm_name) then
          ok=.true.
          return
       endif
    case(pm_type_is_fix_value)
       select case(tk)
       case(pm_type_is_fix)
          ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),mode,&
               params,base,user,ubase)
          return
       case(pm_type_is_fix_value)
          ok=pm_tv_name(t)==pm_tv_name(u)
          return
       case(pm_type_is_basic)
          ok=pm_test_type_includes(context,p,pm_tv_arg(u,1),mode,&
               params,base,user,ubase)
          return
       end select
    case(pm_type_is_literal_value)
       if(tk==pm_type_is_literal) then
          ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),mode,&
               params,base,user,ubase)
          return
       elseif(tk==pm_type_is_literal_value) then
          ok=pm_tv_name(t)==pm_tv_name(u)
          return
       end if
    case(pm_type_is_fix,pm_type_is_literal)
       if(tk==uk.or.tk==pm_type_is_fix) then
          ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),mode,&
               params,base,user,ubase)
          return
       elseif(tk/=pm_type_is_user) then
          ok=pm_test_type_includes(context,p,pm_tv_arg(u,1),mode,&
               params,base,user,ubase)
          return
       endif
    case(pm_type_is_user)
       if(tk/=pm_type_is_user) then
          do i=2,ubase,2
             if(user(i)==p.and.user(i+1)==q) then
                ok=.true.
                return
             endif
          enddo
          if(ubase+2>size(user)) then
             call pm_panic('Program too complex - nested type defs')
          endif
          user(ubase+1)=p
          user(ubase+2)=q
          r=pm_dict_val(context,context%tcache,int(q,pm_ln))
          ok=pm_test_type_includes(context,p,int(r%offset),&
               mode,params,base,user,ubase+2)
          return
       endif
    case(pm_type_is_any)
       do i=1,pm_tv_numargs(u)
           if(.not.pm_test_type_includes(context,p,pm_tv_arg(u,i),&
               ior(mode,pm_type_incl_nomatch),params,base,user,ubase)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
       return
    case(pm_type_is_all)
       do i=1,pm_tv_numargs(u)
          if(pm_test_type_includes(context,p,pm_tv_arg(u,i),&
               mode,params,base,user,ubase)) then
             ok=.true.
             return
          endif
       enddo
       ok=.false.
       return
    case(pm_type_is_except)
       if(tk==pm_type_is_except) then
          ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               mode,params,base,user,ubase).and.&
               pm_test_type_includes(context,pm_tv_arg(u,2),pm_tv_arg(t,2),&
               ior(mode,pm_type_incl_nomatch),params,base,user,ubase)
          return
       else
          ok=pm_test_type_includes(context,p,pm_tv_arg(u,1),&
               ior(mode,pm_type_incl_nomatch),params,base,user,ubase)
          return
       endif
    case(pm_type_is_includes)
       ok=pm_test_type_includes(context,p,pm_tv_arg(u,1),&
            mode,params,base,user,ubase)
       return
    case(pm_type_is_undef_result)
       ok=.false.
       return
    case(pm_type_is_par_kind)
       nu=pm_tv_name(u)
       if(tk==pm_type_is_par_kind) then
          nt=pm_tv_name(t)
          if(iand(mode,pm_type_incl_val)/=0) then
             ok=pm_mode_includes(nt,nu)
             if(ok) then
                ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
                     mode,params,base,user,ubase)
             endif
          else
             if(nt==nu) then
                ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
                     mode,params,base,user,ubase)
             else
                ok=pm_mode_includes(nt,nu)
             endif
          endif
          return
       else
          ok=pm_test_type_includes(context,p,pm_tv_arg(u,1),&
                  mode,params,base,user,ubase)
          return
       endif
    case(pm_type_is_param)
       ok=pm_test_type_includes(context,p,pm_tv_arg(u,1),&
            mode,params,base,user,ubase)
       return
    case(pm_type_is_bottom)
       ok=.true.
       return
    end select

    ! Now do tests that look at 1st type first
    select case(tk)
    case(pm_type_is_basic)
       ok=.false.
    case(pm_type_is_dref)
       if(tk/=uk) then
          ok=.false.
          return
       endif
       nt=pm_tv_name(t)
       nu=pm_tv_name(u)
       if(nt==pm_dref_is_any) then
          if(nu/=pm_dref_is_any.and.iand(mode,pm_type_incl_type)/=0) then
             ok=.true.
             return
          endif
       elseif(.not.(nt==nu.or.&
            (nt==pm_dref_is_dot.and.nu>0).or.&
            (nt==pm_dref_is_any_slice.and.(nu==pm_dref_is_slice.or.&
            nu==pm_dref_is_shared_slice)).or.&
            nt==pm_dref_is_shared.and.nu==pm_dref_is_ref)) then
          ok=.false.
          return
       endif
       do i=1,pm_tv_numargs(t)
          if(.not.pm_test_type_includes(context,pm_tv_arg(t,i),&
               pm_tv_arg(u,i),mode,params,base,user,ubase)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
    case(pm_type_is_rec)
       if(tk/=uk) then
          ok=.false.
          return
       endif
       if(.not.pm_tv_name(t)==pm_tv_name(u)) then
          ok=.false.
          return
       endif
       do i=1,pm_tv_numargs(t)
          if(.not.pm_test_type_includes(context,pm_tv_arg(t,i),&
               pm_tv_arg(u,i),mode,params,base,user,ubase)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
    case(pm_type_is_array)
       if(uk/=pm_type_is_array) then
          ok=.false.
       else
          if(.not.(pm_tv_name(t)==pm_tv_name(u).or.pm_tv_name(t)==0)) then
             ok=.false.
          else
             ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
                  mode,params,base,user,ubase).and.&
                  pm_test_type_includes(context,pm_tv_arg(t,2),pm_tv_arg(u,2),&
                  mode,params,base,user,ubase)
          endif
       endif
    case(pm_type_is_type,pm_type_is_poly)
       if(uk/=tk) then
          ok=.false.
       else
          ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               ior(mode,pm_type_incl_nomatch),params,base,user,ubase)
       endif
    case(pm_type_is_tuple,pm_type_is_vtuple)
       if(uk/=pm_type_is_tuple.and.uk/=pm_type_is_vtuple) then
          ok=.false.
       elseif(tk==pm_type_is_tuple.and.uk==pm_type_is_vtuple) then
          ok=.false.
       elseif(pm_tv_name(t)/=pm_tv_name(u)) then
          ok=.false.
       elseif(iand(pm_tv_flags(t),pm_type_is_when)/=0.and.iand(pm_tv_flags(u),pm_type_is_when)==0) then
          !(  when) does not include (  )
          ok=.false.
       elseif(iand(pm_tv_flags(t),pm_type_is_yield+pm_type_is_list)/=&
            iand(pm_tv_flags(u),pm_type_is_yield+pm_type_is_list)) then
          ok=.false.
       else
          nt=pm_tv_numargs(t)
          nu=pm_tv_numargs(u)
          if(nt>nu.and.uk/=pm_type_is_vtuple) then
             ok=.false.
             return
          endif
          if(nu>nt.and.tk/=pm_type_is_vtuple) then
             ok=.false.
             return
          endif
          j=1
          if(nt==nu) then
             do while(j<=nt)
                if(pm_tv_arg(t,j)==pm_tv_arg(u,j)) then
                   j=j+1
                else
                   exit
                endif
             enddo
             if(j==nt+1) then
                ok=pm_tv_name(t)<=pm_tv_name(u)
                return
             endif
          endif
          do i=j,min(nt,nu)
             if(.not.pm_test_type_includes(context,pm_tv_arg(t,i),&
                  pm_tv_arg(u,i),mode,params,base,user,ubase)) then
                ok=.false.
                return
             endif
          enddo
          if(nu>nt) then
             do i=nt+1,nu
                if(.not.pm_test_type_includes(context,pm_tv_arg(t,nt),&
                     pm_tv_arg(u,i),mode,params,base,&
                     user,ubase)) then
                   ok=.false.
                   return
                endif
             enddo
          else
             do i=nu+1,nt
                if(.not.pm_test_type_includes(context,pm_tv_arg(t,nt),&
                     pm_tv_arg(u,i),mode,params,base,&
                     user,ubase)) then
                   ok=.false.
                   return
                endif
             enddo
          endif
          ok=.true.
       endif
    case(pm_type_is_user)
       if(uk==pm_type_is_user) then
          ! Check P(p1,p2) < Q(q1,q2) <=> p1<q1, p2<q2 if P==Q
          if(pm_tv_name(t)==pm_tv_name(u)) then
             nt=pm_tv_numargs(t)
             nu=pm_tv_numargs(u)
             if(nt==nu) then
                do i=1,nt
                   ok=pm_test_type_includes(context,&
                        pm_tv_arg(t,i),pm_tv_arg(u,i),&
                        mode,params,base,user,ubase)
                   if(.not.ok) return
                enddo
                ok=.true.
                return
             endif
          endif
       endif
       if(iand(mode,pm_type_incl_val)==0) then
          ! Check for recursion
          do i=2,ubase,2
             if(user(i)==p.and.user(i+1)==q) then
                ok=.true.
                return
             endif
          enddo
          if(ubase+2>size(user)) then
             call pm_panic('Program too complex - nested type defs')
          endif
          user(ubase+1)=p
          user(ubase+2)=q
          r=pm_dict_val(context,context%tcache,int(p,pm_ln))
          ok=pm_test_type_includes(context,int(r%offset),q,&
               mode,params,base,user,ubase+2)
       else
          r=pm_dict_val(context,context%tcache,int(p,pm_ln))
          ok=pm_test_type_includes(context,int(r%offset),q,&
               mode,params,base,user,ubase)
       endif
    case(pm_type_is_any)
       do i=1,pm_tv_numargs(t)
          if(pm_test_type_includes(context,pm_tv_arg(t,i),q,&
               ior(mode,pm_type_incl_nomatch),params,base,user,ubase)) then
             ok=.true.
             return
          endif
      enddo
      ok=.false.
    case(pm_type_is_all)
       do i=1,pm_tv_numargs(t)
          if(.not.pm_test_type_includes(context,pm_tv_arg(t,i),q,&
               mode,params,base,user,ubase)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
    case(pm_type_is_single_name)
       ok=.false.
    case(pm_type_is_proc)
       if(uk/=pm_type_is_proc) then
          ok=.false.
          return
       endif
       nt=pm_tv_name(t)
       nu=pm_tv_name(u)
       if(nt/=0) then
          ok=abs(nt)==abs(nu)
       elseif(pm_tv_numargs(u)>1) then
          ok=.false.
       else
          ok=pm_test_type_includes(context,&
               pm_tv_arg(t,1),pm_tv_arg(u,1),&
               pm_type_incl_type+pm_type_incl_nomatch,&
               params,base,user,ubase)
       endif
    case(pm_type_is_proc_sig)
       if(uk/=pm_type_is_proc_sig) then
          ok=.false.
          return
       endif
       if(pm_tv_name(t)/=pm_tv_name(u)) then
          ok=.false.
          return
       endif
       ok=pm_test_type_includes(context,&
               pm_tv_arg(u,1),pm_tv_arg(t,1),&
               pm_type_incl_type+pm_type_incl_nomatch,&
               params,base,user,ubase).and.&
               pm_test_type_includes(context,&
               pm_tv_arg(t,2),pm_tv_arg(u,2),&
               pm_type_incl_type+pm_type_incl_nomatch,&
               params,base,user,ubase)
    case(pm_type_is_par_kind)
       ! Most cases catered for by uk switch - remaining case
       ok=iand(mode,pm_type_incl_val)/=0.and.&
            pm_mode_includes(pm_tv_name(t),sym_private).and.&
            pm_test_type_includes(context,pm_tv_arg(t,1),q,&
            mode,params,base,user,ubase)
    case(pm_type_is_undef_result)
       ok=.false.
    case(pm_type_is_contains)
       if(uk==pm_type_is_contains) then
          ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               ior(mode,pm_type_incl_nomatch),params,base,user,ubase)
       else
          ok=pm_type_contains_elem(context,pm_tv_arg(t,1),q,&
               ior(mode,pm_type_incl_nomatch),params,base,user,ubase)
       endif
    case(pm_type_is_fix_value,pm_type_is_literal_value)
       ok=.false.
    case(pm_type_is_fix)
       if(pm_tv_name(t)>0) then
          ! fix?
          ok=pm_test_type_includes(context,pm_tv_arg(t,1),q,&
            mode,params,base,user,ubase)
       else
          ok=.false.
       endif
    case(pm_type_is_literal)
       if(tk==uk) then
          ok=pm_tv_arg(t,1)==0.or.&
               pm_tv_arg(t,1)==pm_tv_arg(u,1)
       elseif(pm_tv_name(t)>0) then
          ! literal?
          ok=pm_tv_arg(t,1)==q
       else
          ok=.false.
       endif
    case(pm_type_is_except)
       ok=pm_test_type_includes(context,pm_tv_arg(t,1),q,&
            mode,params,base,user,ubase)
       if(ok) then
          ok=.not.pm_test_type_includes(context,pm_tv_arg(t,2),q,&
            mode,params,base,user,ubase)
       endif
    case(pm_type_is_params)
       nt=pm_tv_name(t)
       if(base+nt>size(params)) then
          call pm_panic('Program too complex - Excessive type nesting')
       endif
       params(base:base+nt)=-1
       ok=pm_test_type_includes(context,pm_tv_arg(t,1),q,&
            mode,params,base+nt,user,ubase)
    case(pm_type_is_param)
       ok=pm_test_type_includes(context,pm_tv_arg(t,1),q,&
            mode,params,base,user,ubase)
       if(ok.and.iand(mode,pm_type_incl_extract)/=0) then
          if(iand(mode,pm_type_incl_nomatch)/=0) return
          nt=pm_tv_name(t)
          if(params(nt)==-1) then
             params(nt)=q
          else
             params(nt)=pm_type_combine(context,params(nt),q)
          endif
       endif
    case(pm_type_is_vect,pm_type_is_uninitialised)
       ok=tk==uk
       if(ok) ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
            mode,params,base,user,ubase)
    case(pm_type_is_bottom)
       ok=.false.
    case(pm_type_is_category)
       select case(p)
       case(pm_a_rec_type)
          ok=uk==pm_type_is_rec
       case(pm_a_unique_type)
          ok=uk==pm_type_is_single_name
       case(pm_a_literal_type)
          ok=uk==pm_type_is_literal
       case(pm_a_fix_type)
          ok=uk==pm_type_is_fix
       case(pm_a_basic_type)
          ok=uk==pm_type_is_basic
       case default
          call pm_panic('test-includes,category')
       end select
    case default
       write(*,*) 'Type=',p
       write(*,*) 'Kind=',pm_tv_kind(t)
       write(*,*) 'Name=',pm_tv_name(t)
       do i=1,pm_tv_numargs(t)
          write(*,*) 'Arg=',pm_tv_arg(t,i)
       enddo
       call pm_panic('pm_test_type_includes bad type kind')
    end select
    
  contains

    include 'fesize.inc'
    include 'fisnull.inc'
    include 'ftypeno.inc'
    
  end function pm_test_type_includes


  ! Does type correspond to only one concrete type
  recursive function pm_type_is_concrete(context,tno) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    logical:: ok
    ok=iand(pm_type_flags(context,tno),pm_type_has_generic)==0
  contains
    include 'fisnull.inc'
  end function pm_type_is_concrete

  ! Does a type directly include itself (not as element of
  ! embedded struct/rec or array)
  recursive function pm_type_is_recur(context,rno,tno) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: rno,tno
    logical:: ok
    type(pm_ptr):: tv,r
    integer:: tno2
    integer:: j
    ok=.false.
    if(tno==0) return
    tv=pm_type_vect(context,tno)
    if(pm_tv_kind(tv)==pm_type_is_any.or.&
         pm_tv_kind(tv)==pm_type_is_all) then
       do j=1,pm_tv_numargs(tv)
          tno2=pm_tv_arg(tv,j)
          if(tno2==rno) then
             ok=.true.
             return
          elseif(pm_type_is_recur(context,rno,tno2)) then
             ok=.true.
             return
          endif
       enddo
    endif
  end function pm_type_is_recur

  ! Does a type contain an element (structure/rec component,
  ! array domain or values, applied recursively) of a given
  ! type?
  recursive function pm_type_contains_elem(context,p,q,&
       mode,params,base,user,ubase) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: p,q
    integer,intent(in):: mode
    integer,dimension(:),intent(inout):: params
    integer,intent(in):: base
    integer,dimension(:),intent(inout):: user
    integer,intent(in):: ubase
    type(pm_ptr):: u
    logical:: ok
    integer:: i,k,uk
    if(pm_test_type_includes(context,p,q,mode,&
         params,base,user,ubase)) then
       ok=.true.
       return
    endif
    if(q==0) then
       ok=.false.
       return
    endif
    u=pm_type_vect(context,q)
    uk=pm_tv_kind(u)
    select case(uk)
    case(pm_type_is_all)
       do i=1,pm_tv_numargs(u)
          if(pm_type_contains_elem(context,p,pm_tv_arg(u,i),&
               mode,params,base,user,ubase)) then
             ok=.true.
             return
          endif
       enddo
       ok=.false.
    case(pm_type_is_any)
       do i=1,pm_tv_numargs(u)
          if(.not.pm_type_contains_elem(context,p,pm_tv_arg(u,i),&
               mode,params,base,user,ubase)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
    case(pm_type_is_except)
       ok=pm_type_contains_elem(context,p,pm_tv_arg(u,1),&
            mode,params,base,user,ubase)
       if(ok) then
          ok=.not.pm_type_includes(context,pm_tv_arg(u,2),&
               p,pm_type_incl_type)
       endif
    case(pm_type_is_array)
       if(pm_type_contains_elem(context,p,pm_tv_arg(u,1),&
            mode,params,base,user,ubase)) then
          ok=.true.
          return
       elseif(pm_type_contains_elem(context,p,pm_tv_arg(u,2),&
            mode,params,base,user,ubase)) then
          ok=.true.
          return
       else
          ok=.false.
       endif
    case(pm_type_is_rec,pm_type_is_tuple,pm_type_is_vtuple)
       do i=1,pm_tv_numargs(u)
          if(pm_type_contains_elem(context,p,pm_tv_arg(u,i),&
               mode,params,base,user,ubase)) then
             ok=.true.
             return
          endif
       enddo
       ok=.false.
    case(pm_type_is_dref)
       ok=pm_type_contains_elem(context,p,pm_tv_arg(u,3),&
            mode,params,base,user,ubase)
       if(.not.ok) then
          i=pm_tv_arg(u,2)
          k=pm_type_kind(context,i)
          if(k==pm_type_is_par_kind) then
             u=pm_type_vect(context,i)
             i=pm_tv_arg(u,1)
             k=pm_type_kind(context,i)
          endif
          if(k==pm_type_is_dref) then
             ok=pm_type_contains_elem(context,p,i,&
                  mode,params,base,user,ubase)
          endif
       endif
    case(pm_type_is_par_kind,pm_type_is_vect,&
         pm_type_is_contains,pm_type_is_has,&
         pm_type_is_params,pm_type_is_param)
       ok=pm_type_contains_elem(context,p,pm_tv_arg(u,1),&
               mode,params,base,user,ubase)
    case default
       ok=.false.
    end select
  end function pm_type_contains_elem


  !===============================================
  ! Perform enveloping conversions if possible
  ! Returns -1 if not possible
  !==============================================
  function pm_type_convert(context,partyp,argtyp,doliteral,doproc,dopoly) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: partyp,argtyp
    logical,intent(in):: doliteral,doproc,dopoly
    integer:: ctyp
    integer:: tk,ptyp,atyp,pmode,amode
    type(pm_ptr):: tv
!!$    write(*,*) 'Convert',trim(pm_type_as_string(context,partyp)),&
!!$         '<-',trim(pm_type_as_string(context,argtyp)),doliteral,doproc
    ctyp=-1
    if(partyp<0.or.argtyp<0) then
       return
    endif
    ptyp=partyp
    atyp=pm_type_strip_mode(context,argtyp,amode)
    tk=pm_type_kind(context,ptyp)
    if(tk==pm_type_is_par_kind) then
       if(.not.pm_mode_includes(pm_type_name(context,ptyp),amode)) return
       ptyp=pm_type_arg(context,ptyp,1)
       tk=pm_type_kind(context,ptyp)
    endif
    do while(tk==pm_type_is_user)
       ptyp=pm_user_type_body(context,ptyp)
       tk=pm_type_kind(context,ptyp)
    enddo
    if(doliteral.and.pm_type_kind(context,atyp)==pm_type_is_literal_value) then
       ctyp=pm_literal_type_convert(context,ptyp,atyp)
    endif
     if(ctyp<0.and.doproc.and.tk==pm_type_is_proc) then
       ctyp=pm_proc_type_convert(context,ptyp,atyp)
    endif
    if(ctyp<0.and.dopoly.and.tk==pm_type_is_poly) then
       ctyp=pm_poly_type_convert(context,ptyp,atyp)
    endif
    ctyp=pm_type_add_mode(context,ctyp,amode)
  end function pm_type_convert


  !================================================================
  ! Autoconversion of a literal type 
  !================================================================
  function pm_literal_type_convert(context,partyp,argtyp) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: partyp,argtyp
    integer:: ctyp
    integer:: tk
    ctyp=pm_type_arg(context,argtyp,1)
    tk=pm_type_kind(context,partyp)
    if(tk==pm_type_is_fix) then
       if(pm_type_includes(context,pm_type_arg(context,partyp,1),ctyp,&
            pm_type_incl_val)) then
          ctyp=pm_new_fix_value_type(context,&
               pm_type_val(context,argtyp),pm_type_name(context,argtyp))
       endif
    elseif(tk==pm_type_is_fix_value) then
       if(pm_type_name(context,partyp)==pm_type_name(context,argtyp)) then
          ctyp=partyp
       endif
    elseif(tk==pm_type_is_literal) then
       ctyp=argtyp
    endif
  end function pm_literal_type_convert

  !=========================================================
  ! Convert a moded literal type to a moded non-literal type
  !=========================================================
  function pm_type_strip_literal(context,typ) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer:: ctyp
    integer:: tno,tk,mode
    type(pm_ptr):: tv
    ctyp=typ
    if(typ<=0) return
    tno=pm_type_strip_mode(context,typ,mode)
    tv=pm_type_vect(context,tno)
    tk=pm_tv_kind(tv)
    if(tk==pm_type_is_literal_value.or.tk==pm_type_is_fix_value) then
       ctyp=pm_type_add_mode(context,pm_tv_arg(tv,1),mode)
    endif
  end function pm_type_strip_literal

  !================================================================
  ! Autoconversion to broader poly type
  ! Returns -1 if not possible
  !================================================================
  function pm_poly_type_convert(context,partyp,argtyp) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: partyp,argtyp
    integer:: ctyp
    type(pm_ptr):: tv1,tv2
    ctyp=-1
    tv1=pm_type_vect(context,partyp)
    tv2=pm_type_vect(context,argtyp)
    if(pm_tv_kind(tv1)==pm_type_is_poly.and.pm_tv_kind(tv2)==pm_type_is_poly) then
       if(pm_type_includes(context,pm_tv_arg(tv1,1),pm_tv_arg(tv2,1),&
            pm_type_incl_type)) then
          ctyp=partyp
       endif
    endif
  end function pm_poly_type_convert
  

  !==========================================
  ! Autoconversion to proc signature type
  ! Returns -1 if not possible
  !==========================================
  function pm_proc_type_convert(context,ptyp,argtyp) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: ptyp,argtyp
    integer:: ctyp
    type(pm_ptr):: tv,tv2
    integer:: i,tno
    integer,dimension(3):: arr
    tv=pm_type_vect(context,ptyp)
    tv2=pm_type_vect(context,argtyp)
    if(pm_tv_kind(tv)/=pm_type_is_proc.or.&
         pm_tv_kind(tv2)/=pm_type_is_proc) then
       ctyp=-1
       return
    endif
    tno=pm_tv_arg(tv,1)
    do i=1,pm_tv_numargs(tv2)
       if(pm_proc_type_conforms(context,tno,&
            pm_tv_arg(tv2,i))) then
          arr(1)=pm_type_new_proc
          arr(2)=-abs(pm_tv_name(tv2))
          arr(3)=tno
          ctyp=pm_new_type(context,arr)
          return
       endif
    enddo
    ctyp=-1
    return
  end function pm_proc_type_convert

  !===========================================
  ! Check that two proc_sig types conform
  !===========================================
  function pm_proc_type_conforms(context,tno,tno2) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,tno2
    logical:: ok
    type(pm_ptr):: tv,tv2,tv_res,tv_res2
    integer:: i,tno_res,tno_res2
    
    tv=pm_type_vect(context,tno)
    tv2=pm_type_vect(context,tno2)
    
    
    if(pm_tv_name(tv)/=pm_tv_name(tv2)) then
       ok=.false.
       return
    endif

    if(.not.pm_type_includes(context,pm_tv_arg(tv2,1),&
         pm_tv_arg(tv,1),pm_type_incl_type)) then
       ok=.false.
       return
    endif

    tno_res=pm_tv_arg(tv,2)
    tno_res2=pm_tv_arg(tv2,2)
    tv_res=pm_type_vect(context,tno_res)
    tv_res2=pm_type_vect(context,tno_res2)
    if(pm_tv_kind(tv_res2)==pm_type_is_undef_result) then
       ok=pm_tv_numargs(tv_res)==pm_tv_name(tv_res2)
       return
    else
       if(.not.pm_type_includes(context,tno_res,&
            tno_res2,pm_type_incl_equiv)) then
          ok=.false.
          return
       endif
    endif
    ok=.true.
  end function pm_proc_type_conforms

  !=================================================================
  ! Find element "name" in type "tno"
  ! If change is true then element must be able to be modified
  ! Returns
  !      offset==0   Error
  !      offset>0    This is the offset of the element in the type
  !      offset<0    Returns dref rather than sub-element
  ! If offset/=0 then etype returns the type of the element
  !=================================================================
  recursive function pm_type_find_elem(context,tno,nametype,change,etype) result(offset)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,nametype
    logical,intent(in):: change
    integer,intent(out):: etype
    integer:: offset,ptype,mode
    type(pm_ptr):: tv
    integer:: tk,i,name
    name=pm_type_name(context,nametype)
    if(tno<0) then
       offset=0
       return
    endif
    if(tno==0) then
       offset=0
    endif
    tv=pm_type_vect(context,tno)
    tk=pm_tv_kind(tv)
    select case(tk)
    case(pm_type_is_all)
       do i=1,pm_tv_numargs(tv)
          offset=pm_type_find_elem(context,pm_tv_arg(tv,i),name,change,etype)
          if(offset/=0) return
       enddo
       offset=0
       return
    case(pm_type_is_rec)
       call pm_type_elem_offset(context,tv,name,change,offset,etype)
    case default
       offset=0
       return
    end select
  end function pm_type_find_elem

  ! Find offset and type for named element in struct/rec type
  ! Returns offset and type of element
  ! If no such element offset=0
  subroutine pm_type_elem_offset(context,tv,name,change,offset,etyp)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: tv
    integer,intent(in):: name
    logical,intent(in):: change
    integer,intent(out):: offset,etyp
    integer:: j
    integer:: name2
    type(pm_ptr):: nv
    offset=0
    etyp=0
    nv=pm_name_val(context,pm_tv_name(tv))
    do j=1,pm_fast_esize(nv)
       name2=nv%data%i(nv%offset+j)
       if(abs(name2)==name) then
          if(change.and.name2>0) then
             offset=0
             return
          endif
          etyp=pm_tv_arg(tv,j)
          offset=j
          return
       endif
    enddo
  contains
    include 'fesize.inc'
  end subroutine pm_type_elem_offset


  ! Concrete only version of a type (used/usable only for returns from builtin functions)
  recursive function pm_type_as_concrete(context,tno,params,isstatic,iserr) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer,dimension(:),intent(in):: params
    logical,intent(out):: isstatic
    logical,intent(out),optional:: iserr
    integer:: tno2
    type(pm_ptr):: tv
    integer:: tk,nt
    isstatic=.true.
    if(present(iserr)) iserr=.false.
    tv=pm_type_vect(context,tno)
    tk=pm_tv_kind(tv)
    select case(tk)
    case(pm_type_is_basic,pm_type_is_single_name,&
         pm_type_is_proc,pm_type_is_fix_value,pm_type_is_fix,&
         pm_type_is_undef_result,pm_type_is_poly)
       tno2=tno
    case(pm_type_is_user)
       tno2=pm_user_type_body(context,tno)
    case(pm_type_is_any,pm_type_is_all,pm_type_is_contains)
       if(present(iserr)) then
          iserr=.true.
          isstatic=.true.
       else
          call pm_panic('cant make concrete')
       endif
    case default
       call remake(pm_tv_numargs(tv))
    end select
  contains
    recursive subroutine remake(n)
      integer,intent(in):: n
      integer,dimension(n+2):: a
      integer:: i
      a(1)=tk
      a(2)=pm_tv_name(tv)
      if(present(iserr)) then
         do i=1,n
            a(i+2)=pm_type_as_concrete(context,pm_tv_arg(tv,i),params,isstatic,iserr)
            if(iserr) return
         enddo
      else
         do i=1,n
            a(i+2)=pm_type_as_concrete(context,pm_tv_arg(tv,i),params,isstatic)
         enddo
      endif
      tno2=pm_new_type(context,a)
    end subroutine remake
  end function pm_type_as_concrete

  ! Create a new type with with all literal values replaced by fix (if tofix is true)
  ! Otherwise all fix values are changed to literal values
  recursive function pm_type_change_fix_literal(context,tno,tofix) result(typ)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    logical,intent(in):: tofix
    integer:: typ
    type(pm_ptr):: tv
    integer:: tk
    typ=tno
    tv=pm_type_vect(context,tno)
    if(iand(pm_tv_flags(tv),pm_type_has_fix_or_literal)==0) return
    tk=pm_tv_kind(tv)
    select case(tk)
    case(pm_type_is_user)
       typ=pm_user_type_body(context,tno)
    case(pm_type_is_rec)
       call remake(pm_tv_numargs(tv))
    case(pm_type_is_literal_value)
       if(tofix) typ=pm_new_fix_value_type(context,pm_type_val(context,tno),&
            pm_tv_name(tv))
    case(pm_type_is_fix_value)
       if(.not.tofix) typ=pm_new_literal_value_type(context,pm_type_val(context,tno),&
            pm_tv_name(tv))
    end select
  contains
    recursive subroutine remake(n)
      integer,intent(in):: n
      integer,dimension(n+2):: a
      integer:: i
      a(1)=tk
      a(2)=pm_tv_name(tv)
      do i=1,n
         a(i+2)=pm_type_change_fix_literal(context,pm_tv_arg(tv,i),tofix)
      enddo
      typ=pm_new_type(context,a)
    end subroutine remake
  end function pm_type_change_fix_literal
  
   ! Get vector of integer representation of type
  function pm_type_vect(context,tno) result(typ)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    type(pm_ptr):: typ,dict
    integer(pm_ln):: t
    dict=context%heap%tcache
    t=tno
    if(pm_debug_level>0) then
       if(t<1.or.t>pm_dict_size(context,dict)) then
          write(*,*) 'tno=',t,tno,pm_dict_size(context,dict)
          call pm_panic('pm_type_vect')
       endif
    endif
    typ=pm_dict_key(context,dict,t)
  end function pm_type_vect

  ! Type kind from integer type vector
  function pm_tv_kind(typ) result(k)
    type(pm_ptr),intent(in):: typ
    integer:: k
    k=iand(typ%data%i(typ%offset),pm_type_kind_mask)
  end function pm_tv_kind

   ! Type kind from integer type vector
  function pm_tv_flags(typ) result(k)
    type(pm_ptr),intent(in):: typ
    integer:: k
    k=typ%data%i(typ%offset)
  end function pm_tv_flags

  ! Name from integer type vector
  function pm_tv_name(typ) result(name)
    type(pm_ptr),intent(in):: typ
    integer:: name
    name=typ%data%i(typ%offset+1_pm_p)
  end function pm_tv_name

  ! Argument m from integer type vector
  function pm_tv_arg(typ,m) result(arg)
    type(pm_ptr),intent(in):: typ
    integer,intent(in):: m
    integer:: arg
    if(pm_debug_checks) then
       if(m>pm_tv_numargs(typ)) then
          call pm_panic('pm_tv_arg')
       endif
    endif
    arg=typ%data%i(typ%offset+m+1)
  end function pm_tv_arg
  
  ! Number of arguments in integer type vector
  function pm_tv_numargs(typ) result(num)
    type(pm_ptr),intent(in):: typ
    integer:: num
    num=pm_fast_esize(typ)-1
  contains
    include 'fesize.inc'
  end function pm_tv_numargs
  
  ! Display type as user-readable string
  function pm_type_as_string(context,tno,distr) result(str)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    logical,intent(in),optional:: distr
    character(len=256):: str
    integer:: n
    str=''
    if(tno==0) then
       str='any'
    else
       n=1
       call pm_type_to_string(context,tno,str,n,tuple=.false.,distr=distr)
    endif
  end function  pm_type_as_string

  recursive subroutine pm_type_to_string(context,typno,str,n,distr,tuple,noequiv,tuple_start)
    type(pm_context),pointer:: context
    integer,intent(in):: typno
    character(len=256),intent(inout):: str
    integer,intent(inout):: n
    logical,intent(in),optional:: distr,tuple,noequiv
    integer,intent(in),optional:: tuple_start
    type(pm_ptr):: tv,tv2,nv,nv2
    integer:: tk,narg,tno2
    integer:: name,name2
    character(len=1),parameter:: open_brace = '{'
    character(len=1),parameter:: close_brace = '}'
    character(len=1),parameter:: open_square = '['
    character(len=1),parameter:: close_square = ']'
    integer:: i,j,istart,tno,tk2
    type(pm_ptr):: amps
    logical:: ok
    if(n>len(str)-10) return
    tno=typno
    if(tno==0) then
       if(add_char('any')) return
       return
    endif
    if(tno<0) then
       if(add_char('*Internal error(<0)*')) return
       return
    endif
    if(tno>pm_dict_size(context,context%heap%tcache)) then
       if(add_char('*Internal error(>size)*')) return
       return
    endif
    tv=pm_type_vect(context,tno)
    tk=pm_tv_kind(tv)
    nv=pm_dict_val(context,context%tcache,int(tno,pm_ln))
    narg=pm_tv_numargs(tv)
    select case(tk)
    case(pm_type_is_user,pm_type_is_basic,pm_type_is_category)
       name=pm_tv_name(tv)
       if(name<0) then
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
          return
       endif
       name=pm_name_stem(context,name)
       if(name>=sym_dim1.and.name<=sym_dim7.and.narg==name-sym_dim1+1) then
          if(present(distr)) then
             if(.not.distr) return
          endif
          if(present(tuple)) then
             tno2=pm_tv_arg(tv,1)
             ok=.true.
             do i=2,narg
                ok=ok.and.pm_tv_arg(tv,i)==tno2
             enddo
          else
             ok=.false.
          endif
          if(ok) then
             call pm_name_string(context,name,str(n:))
             n=len_trim(str)+1
             if(narg==1) then
                if(add_char('(')) return
             else
                if(add_char('_of(')) return
             endif
             call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
             if(add_char(')')) return
          else
             if(add_char('[')) return
             do i=1,narg-1
                call pm_type_to_string(context,pm_tv_arg(tv,i),str,n)
                if(add_char(',')) return
             enddo
             call pm_type_to_string(context,pm_tv_arg(tv,narg),str,n)
             if(add_char(']')) return
          endif
       elseif(name==sym_pm_ref_type) then
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       else
          call pm_name_string(context,name,str(n:))
          n=len_trim(str)+1
          if(n>len(str)-10) return
          narg=pm_tv_numargs(tv)
          if(narg>0) then
             if(add_char('(')) return
             do i=1,narg-1
                call pm_type_to_string(context,pm_tv_arg(tv,i),str,n)
                if(add_char(',')) return
             enddo
             call pm_type_to_string(context,pm_tv_arg(tv,narg),str,n)
             if(add_char(')')) return
          endif
          if(tk==pm_type_is_user.and.(pm_opts%show_members)) then
             nv2=pm_dict_val(context,context%tcache,int(tno,pm_ln))
             tno2=int(nv2%offset)
             if(tno2>0.and.tno2<pm_dict_size(context,context%heap%tcache)) then
                tv=pm_type_vect(context,tno2)
                if(pm_tv_kind(tv)/=pm_type_is_basic) then
                   if(add_char(' {')) return
                   call pm_type_to_string(context,tno2,str,n)
                   if(add_char('}')) return
                endif
             else
                if(add_char('{???}')) return
             endif
          endif
       endif
    case(pm_type_is_tuple,pm_type_is_vtuple)
       istart=1
       if(present(tuple_start)) istart=tuple_start
       if(iand(pm_tv_flags(tv),pm_type_is_list)/=0) then
          if(add_char('PM__list(')) return
       else
          if(add_char('(')) return
       endif
       narg=pm_tv_numargs(tv)
       if(narg==0) then
          if(add_char(')')) return
          return
       endif
       if(pm_tv_name(tv)/=0) then
          amps=pm_name_val(context,pm_tv_name(tv))
          j=0
          do while(amps%data%i(amps%offset+j)<istart)
             if(j<pm_fast_esize(amps)) j=j+1
          enddo
          do i=istart,narg-1
             if(amps%data%i(amps%offset+j)==i) then
                if(j<pm_fast_esize(amps)) j=j+1
                if(add_char('&')) return
             endif
             call pm_type_to_string(context,pm_tv_arg(tv,i),str,n)
             if(add_char(',')) return
          enddo
          if(amps%data%i(amps%offset+j)==narg) then
             if(add_char('&')) return
          endif
          call pm_type_to_string(context,pm_tv_arg(tv,narg),str,n)
       else
          do i=istart,narg-1
             call pm_type_to_string(context,pm_tv_arg(tv,i),str,n)
             if(add_char(',')) return
          enddo
          call pm_type_to_string(context,pm_tv_arg(tv,narg),str,n)
       endif
       if(tk==pm_type_is_vtuple) then
          if(add_char('...')) return
       endif
       if(iand(pm_tv_flags(tv),pm_type_is_when)/=0) then
          if(add_char(' when')) return
       endif
       if(add_char(')')) return
    case(pm_type_is_rec)
       nv=pm_name_val(context,pm_tv_name(tv))
       name=nv%data%i(nv%offset)
       tno2=pm_type_from_recorded_name(context,int(name))
       if(tno2>0.and..not.present(noequiv)) then
          if(show_equiv(int(name),tno2,tno)) return
       endif
       if(add_char('rec ')) return
       call pm_name_string(context,name,str(n:))
       n=len_trim(str)+1
       if(n>len(str)-10) return
       if(add_char(open_brace)) return
       narg=pm_tv_numargs(tv)
       do i=1,narg
          name=nv%data%i(nv%offset+i)
          if(name<0) then
             if(add_char('var ')) return
             name=-name
          endif
          nv2=pm_name_val(context,name)
          if(pm_fast_vkind(nv2)==pm_int32) then
             if(add_char('_')) return
             call pm_name_string(context,&
                  nv2%data%i32(nv2%offset+1_pm_p),str(n:))
          else   
             call pm_name_string(context,name,str(n:))
          endif
          n=len_trim(str)+1
          if(n>len(str)-10) return
          if(add_char(':')) return
          call pm_type_to_string(context,pm_tv_arg(tv,i),str,n)
          if(i<narg) then
             if(add_char(',')) return
          endif
       enddo
       if(add_char(close_brace)) return
    case(pm_type_is_single_name)
       name=pm_tv_name(tv)
       nv2=pm_name_val(context,name)
       if(pm_fast_vkind(nv2)==pm_int32) then
          if(add_char('_')) return
          call pm_name_string(context,&
               nv2%data%i32(nv2%offset+1_pm_p),str(n:))
       else   
          call pm_name_string(context,name,str(n:))
       endif
       n=len_trim(str)+1
       if(n>len(str)-10) return
       if(iand(pm_tv_flags(tv),pm_type_has_distributed)/=0) then
          if(add_char('*distr*')) return
       endif
    case(pm_type_is_dref)
       if(pm_opts%show_all_ref) then
          if(pm_tv_name(tv)==pm_dref_is_dot) then
             if(add_char('^.(')) return
          elseif(pm_tv_name(tv)==pm_dref_is_shared) then
             if(add_char('^shrd(')) return
          elseif(pm_tv_name(tv)==pm_dref_is_var) then
             if(add_char('^(')) return
          elseif(pm_tv_name(tv)==pm_dref_is_any) then
             if(add_char('^*(')) return
          elseif(pm_tv_name(tv)==pm_dref_is_here) then
             if(add_char('^here(')) return
          elseif(pm_tv_name(tv)==pm_dref_is_slice) then
             if(add_char('^#(')) return
          elseif(pm_tv_name(tv)==pm_dref_is_shared_slice) then
             if(add_char('^#shrd(')) return
          elseif(pm_tv_name(tv)==pm_dref_is_ref) then
             if(add_char('^ref(')) return
          else
             if(add_char('^')) return
             call pm_name_string(context,pm_tv_name(tv),str(n:))
             n=len_trim(str)+1
             if(add_char('(')) return
          endif
          do i=1,pm_tv_numargs(tv)-1
             call pm_type_to_string(context,pm_tv_arg(tv,i),str,n)
             if(add_char(',')) return
          enddo
          call pm_type_to_string(context,pm_tv_arg(tv,pm_tv_numargs(tv)),str,n)
          if(add_char(')')) return
       else
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       endif
    case(pm_type_is_array)
       name=pm_tv_name(tv)
       if(name==sym_var) then
          if(add_char('varray(')) return
       elseif(name==sym_const) then
          if(add_char('farray(')) return
       else
          if(add_char('array(')) return
       endif
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       if(add_char(',')) return
       call pm_type_to_string(context,pm_tv_arg(tv,2),str,n)
       if(add_char(')')) return
    case(pm_type_is_poly)
       if(add_char('*')) return
       call bracket(1,pm_type_is_includes,pm_type_is_all,pm_type_is_any,pm_type_is_except)
    case(pm_type_is_fix_value,pm_type_is_literal_value)
       if(tk==pm_type_is_fix_value) then
          if(add_char('fix(')) return
       else
          if(add_char('literal(')) return
       endif
       if(pm_tv_name(tv)==0) then
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       else
          nv=pm_dict_val(context,context%tcache,int(tno,pm_ln))
          if(pm_fast_vkind(nv)==pm_logical) then
             if(nv%data%l(nv%offset)) then
                if(add_char('true')) return
             else
                if(add_char('false')) return
             endif
          elseif(pm_fast_vkind(nv)==pm_string) then
             str(n:n)='"'
             call pm_strval(nv,str(n+1:))
             n=n+pm_fast_esize(nv)+2
             str(n:n)='"'
             n=n+1
          else
             str(n:)=pm_value_as_string(context,nv)
          endif
          n=len_trim(str)+1
          if(add_char(')')) return
       endif
    case(pm_type_is_fix)
       if(add_char('fix(')) return
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       if(add_char(')')) return
    case(pm_type_is_literal)
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       if(add_char('_literal')) return
    case(pm_type_is_except)
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       if(add_char(' except ')) return
       call pm_type_to_string(context,pm_tv_arg(tv,2),str,n)
    case(pm_type_is_any)
       call bracket(1,pm_type_is_except,pm_type_is_except,pm_type_is_except,pm_type_is_except)
       do i=2,pm_tv_numargs(tv)
          if(add_char(' or ')) return
          call bracket(i,pm_type_is_except,pm_type_is_except,pm_type_is_except,pm_type_is_except)
       enddo
    case(pm_type_is_all)
       call bracket(1,pm_type_is_any,pm_type_is_except,pm_type_is_except,pm_type_is_except)
       do i=2,pm_tv_numargs(tv)
          if(add_char(' and ')) return
          call bracket(i,pm_type_is_any,pm_type_is_except,pm_type_is_except,pm_type_is_except)
       enddo
    case(pm_type_is_includes)
       call bracket(1,pm_type_is_any,pm_type_is_all,pm_type_is_except,pm_type_is_except)
       if(add_char(' inc ')) return
       call bracket(2,pm_type_is_any,pm_type_is_all,pm_type_is_except,pm_type_is_except)
    case(pm_type_is_contains)
       if(add_char('contains(')) return
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       if(add_char(')')) return
    case(pm_type_is_has)
       if(add_char('.')) return
       call bracket(1,pm_type_is_includes,pm_type_is_all,pm_type_is_any,pm_type_is_except)
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
    case(pm_type_is_proc)
       name=pm_tv_name(tv)
       if(name>0) then
          if(add_char('$')) return
          nv2=pm_name_val(context,name)
          if(pm_fast_vkind(nv2)==pm_int32) then
             if(add_char('_')) return
             call pm_name_string(context,&
                  nv2%data%i32(nv2%offset+1_pm_p),str(n:))
          else   
             call pm_name_string(context,name,str(n:))
          endif
          n=len_trim(str)+1
          if(n>len(str)-10) return
          if(pm_opts%show_variants) then
             if(add_char(' -- {')) return
             do i=1,pm_tv_numargs(tv)-1
                call pm_type_to_string(context,pm_tv_arg(tv,i),str,n)
                if(add_char(',')) return
             enddo
             call pm_type_to_string(context,pm_tv_arg(tv,pm_tv_numargs(tv)),str,n)
             if(add_char('}')) return
          endif
       elseif(name==0) then
          if(add_char('proc')) return
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       else
          if(add_char('proc ')) return
          nv2=pm_name_val(context,-name)
          if(pm_fast_vkind(nv2)==pm_int32) then
             if(add_char('_')) return
             call pm_name_string(context,&
                  nv2%data%i32(nv2%offset+1_pm_p),str(n:))
          else   
             call pm_name_string(context,-name,str(n:))
          endif
          n=len_trim(str)+1
          if(n>len(str)-10) return
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       endif
    case(pm_type_is_proc_sig)
       name=pm_tv_name(tv)
       if(name/=sym_proc) then
          if(add_char(trim(pm_name_as_string(context,name)))) return
          istart=7
       else
          istart=2
       endif
       do i=1,istart
          if(pm_type_arg(context,pm_tv_arg(tv,1),i)/=0) then
             if(add_char('^')) return
             istart=1
             exit
          endif
       enddo
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,tuple_start=istart)
       if(add_char('->')) return
       call pm_type_to_string(context,pm_tv_arg(tv,2),str,n)
       if(iand(pm_tv_flags(tv),pm_type_is_yield)/=0) then
          if(add_char('yield(')) return
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
          if(add_char(')')) return
       endif
    case(pm_type_is_undef_result)
       name=pm_tv_name(tv)
       if(add_char('(')) return
       if(name>0) then
          do i=1,name-1
             if(add_char('_,')) return
          enddo
          if(add_char('_')) return
       endif
       if(add_char(')')) return
    case(pm_type_is_vect)
       if(add_char('^^(')) return
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       if(add_char(')')) return
    case(pm_type_is_par_kind)
       name=pm_tv_name(tv)
       if(add_char(trim(sym_names(name)))) return
       if(add_char(' ')) return
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
    case(pm_type_is_param,pm_type_is_params)
       if(add_char('$')) return
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,noequiv=.true.)
    case(pm_type_is_type)
       if(add_char('<')) return
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
       if(add_char('>')) return
    case(pm_type_is_uninitialised)
       if(add_char('UNINIT:')) return
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n)
    case(pm_type_is_bottom)
       if(add_char(' _ ')) return
    case default
       if(add_char('?')) return
       write(str(n:n+3),'(i4)') tk
       n=len_trim(str)+1
    end select
  contains
    include 'fvkind.inc'
    include 'fisnull.inc'
    include 'ftiny.inc'
    include 'fesize.inc'
    
    function add_char(c) result(term)
      character(len=*),intent(in):: c
      logical:: term
      if(n>len(str)-10-len(c)) then
         if(n<len(str)-3) then
            str(n:n+2)='...'
         else
            str(len(str)-2:len(str))='...'
         endif
         term=.true.
      else
         str(n:n+len(c)-1)=c
         n=n+len(c)
         term=.false.
      endif
    end function add_char

    ! Print out type in argument #i
    ! Add brackets if the kind of this type
    ! is equal to one of tk1..tk4
    subroutine bracket(i,tk1,tk2,tk3,tk4)
      integer,intent(in):: i,tk1,tk2,tk3,tk4
      integer:: tk
      tk=pm_type_kind(context,pm_tv_arg(tv,i))
      if(tk==tk1.or.tk==tk2.or.tk==tk3.or.tk==tk4) then
         if(add_char('(')) return
         call pm_type_to_string(context,pm_tv_arg(tv,i),str,n)
         if(add_char(')')) return
      else
         call pm_type_to_string(context,pm_tv_arg(tv,i),str,n)
      endif
    end subroutine bracket

    function show_equiv(name,templ,typ) result(ok)
      integer,intent(in):: name,templ,typ
      logical:: ok
      integer,dimension(pm_max_type_args):: params
      integer:: i,m,name2
      logical:: tuple
      params=-1
      ok=pm_type_extract_params(context,templ,typ,params)
      if(ok) then
         m=0
         do i=1,pm_max_type_args
            if(params(i)>0) m=i
         enddo
         name2=pm_name_stem(context,name)
         tuple=name2>=sym_dim1.and.name2<=sym_dim7
         if(.not.tuple) then
            call pm_name_string(context,name,str(n:))
         endif
         n=len_trim(str)+1
         if(n>len(str)-10) return
         if(m>0) then
            if(add_char(merge('[','(',tuple))) return
            do i=1,m
               if(params(i)>0) then
                  call pm_type_to_string(context,params(i),str,n)
               endif
               if(i<m) then
                  if(add_char(',')) return
               endif
            enddo
            if(add_char(merge(']',')',tuple))) return
            n=len_trim(str)+1
         elseif(tuple) then
            if(add_char('[]')) return
            n=len_trim(str)+1
         endif
      endif
    end function show_equiv

  end subroutine pm_type_to_string

  subroutine dump_type(context,iunit,tno)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,tno
    type(pm_ptr):: tv
    integer:: i
    tv=pm_type_vect(context,tno)
    write(iunit,*) pm_tv_kind(tv),pm_tv_flags(tv),pm_tv_name(tv),&
         '#',(pm_tv_arg(tv,i),i=1,pm_tv_numargs(tv)),'#',&
         iand(pm_tv_flags(tv),pm_type_has_generic)/=0,&
         iand(pm_tv_flags(tv),pm_type_has_distributed)
  end subroutine dump_type

  
end module pm_types
