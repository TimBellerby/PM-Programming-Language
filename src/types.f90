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

module pm_types
  use pm_kinds
  use pm_memory
  use pm_hash
  use pm_lib
  use pm_symbol
  use pm_options
  implicit none

  logical,parameter:: pm_typ_extra_debug=.false.
  integer,parameter:: pm_max_typ_args=128

  ! Flags for types
  integer,parameter:: pm_typ_has_storage=32
  integer,parameter:: pm_typ_has_distributed=64
  integer,parameter:: pm_typ_has_array=128
  integer,parameter:: pm_typ_has_poly=256
  integer,parameter:: pm_typ_has_generic=512
  integer,parameter:: pm_typ_has_vect=1024
  integer,parameter:: pm_typ_has_embedded=2048
  integer,parameter:: pm_typ_has_params=4096
  integer,parameter:: pm_typ_is_soa=8192
  integer,parameter:: pm_typ_is_aos=16384
  integer,parameter:: pm_typ_is_seq=32768
  integer,parameter:: pm_typ_leaves=65536

  ! Bitwise-or of flags which are not taints (only one so far)
  integer,parameter:: pm_typ_flags_untainting = pm_typ_has_embedded

  ! Type kind + default flags
  integer,parameter:: pm_typ_new_user=1
  integer,parameter:: pm_typ_new_struct=2
  integer,parameter:: pm_typ_new_rec=3
  integer,parameter:: pm_typ_new_array=4+pm_typ_has_array
  integer,parameter:: pm_typ_new_tuple=5
  integer,parameter:: pm_typ_new_vtuple=6
  integer,parameter:: pm_typ_new_single_name=7
  integer,parameter:: pm_typ_new_proc=8
  integer,parameter:: pm_typ_new_all=9
  integer,parameter:: pm_typ_new_any=10+pm_typ_has_generic
  integer,parameter:: pm_typ_new_poly=11+pm_typ_has_poly+&
       pm_typ_has_storage
  integer,parameter:: pm_typ_new_value=12
  integer,parameter:: pm_typ_new_contains=13
  integer,parameter:: pm_typ_new_const=14+pm_typ_has_storage
  integer,parameter:: pm_typ_new_dref=15
  integer,parameter:: pm_typ_new_par_kind=16
  integer,parameter:: pm_typ_new_proc_sig=17
  integer,parameter:: pm_typ_new_undef_result=18
  integer,parameter:: pm_typ_new_interface=19
  integer,parameter:: pm_typ_new_except=20
  integer,parameter:: pm_typ_new_param=21+pm_typ_has_params
  integer,parameter:: pm_typ_new_amp=22
  integer,parameter:: pm_typ_new_has=23
  integer,parameter:: pm_typ_new_vect=24+pm_typ_has_vect
  integer,parameter:: pm_typ_new_params=25
  integer,parameter:: pm_typ_new_type=26
  integer,parameter:: pm_typ_new_enveloped=27
  integer,parameter:: pm_typ_new_bottom=28
  integer,parameter:: pm_typ_new_includes=29

  ! Type kinds
  integer,parameter:: pm_typ_is_basic=0
  integer,parameter:: pm_typ_is_user=1
  integer,parameter:: pm_typ_is_struct=2
  integer,parameter:: pm_typ_is_rec=3
  integer,parameter:: pm_typ_is_array=4
  integer,parameter:: pm_typ_is_tuple=5
  integer,parameter:: pm_typ_is_vtuple=6
  integer,parameter:: pm_typ_is_single_name=7
  integer,parameter:: pm_typ_is_proc=8
  integer,parameter:: pm_typ_is_all=9
  integer,parameter:: pm_typ_is_any=10
  integer,parameter:: pm_typ_is_poly=11
  integer,parameter:: pm_typ_is_value=12
  integer,parameter:: pm_typ_is_contains=13
  integer,parameter:: pm_typ_is_const=14
  integer,parameter:: pm_typ_is_dref=15
  integer,parameter:: pm_typ_is_par_kind=16
  integer,parameter:: pm_typ_is_proc_sig=17
  integer,parameter:: pm_typ_is_undef_result=18
  integer,parameter:: pm_typ_is_interface=19
  integer,parameter:: pm_typ_is_except=20
  integer,parameter:: pm_typ_is_param=21
  integer,parameter:: pm_typ_is_amp=22
  integer,parameter:: pm_typ_is_has=23
  integer,parameter:: pm_typ_is_vect=24
  integer,parameter:: pm_typ_is_params=25
  integer,parameter:: pm_typ_is_type=26
  integer,parameter:: pm_typ_is_enveloped=27
  integer,parameter:: pm_typ_is_bottom=28
  integer,parameter:: pm_typ_is_includes=29

  integer,parameter:: pm_typ_kind_mask=31
  integer,parameter:: pm_typ_max_leaves=255

  ! Mode for type inclusion testing (type<>value,type<>type,type==type)
  integer,parameter:: pm_typ_incl_val=1
  integer,parameter:: pm_typ_incl_typ=2
  integer,parameter:: pm_typ_incl_equiv=4
  integer,parameter:: pm_typ_incl_indirect=8
  integer,parameter:: pm_typ_incl_nomatch=16
  integer,parameter:: pm_typ_incl_extract=32

  integer,parameter:: pm_typ_dref_offset=2000

  ! Return from struct/rec element lookup
  integer,parameter:: pm_elem_found=0
  integer,parameter:: pm_elem_not_found=1
  integer,parameter:: pm_elem_clash=2

  ! Information on location and kind of non-match/type error
  type pm_typ_einfo
     integer:: kind
     integer:: index
     integer:: name,vname,typ1,typ2,vtyp1,vtyp2
  end type pm_typ_einfo

  ! Error codes from type testing
  integer,parameter:: pm_typ_err_none=0
  integer,parameter:: pm_typ_err_elem=1
  integer,parameter:: pm_typ_err_param=2
  integer,parameter:: pm_typ_err_ambig=4
  integer,parameter:: pm_typ_err_not_set=8
  integer,parameter:: pm_typ_err_interface=16
  integer,parameter:: pm_typ_err_interface_clash=32
  integer,parameter:: pm_typ_err_interface_mismatch=64
  integer,parameter:: pm_typ_err_interface_bad_typ=128
  integer,parameter:: pm_typ_err_interface_write=256
  integer,parameter:: pm_typ_err_interface_nesting=512
  integer,parameter:: pm_typ_err_interface_inconsistent=1024
  integer,parameter:: pm_typ_err_interface_elem=2048
  integer,parameter:: pm_typ_err_elem_clash=4096
  integer,parameter:: pm_typ_err_elem_not_found=8192
  integer,parameter:: pm_typ_err_elem_bad_typ=16384
  integer,parameter:: pm_typ_err_elem_not_in_interface=32768

  ! Maximum nesting of "type is" declarations
  integer,private,parameter:: max_user_nesting = 64

  ! System types (mainly used by VM)
  integer(pm_p),public,parameter:: pm_prc_type = pm_last_lib_type+1
  integer(pm_p),public,parameter:: pm_string_type=pm_last_lib_type+2
  integer(pm_p),public,parameter:: pm_poly_type=pm_last_lib_type+3
  integer(pm_p),public,parameter:: pm_struct_type=pm_last_lib_type+4
  integer(pm_p),public,parameter:: pm_rec_type=pm_last_lib_type+5
  integer(pm_p),public,parameter:: pm_polyref_type=pm_last_lib_type+6
  integer(pm_p),public,parameter:: pm_array_type=pm_last_lib_type+7
  integer(pm_p),public,parameter:: pm_const_array_type=pm_last_lib_type+8
  integer(pm_p),public,parameter:: pm_dref_type=pm_last_lib_type+9
  integer(pm_p),public,parameter:: pm_dref_shared_type=pm_last_lib_type+10
  integer(pm_p),public,parameter:: pm_elemref_type=pm_last_lib_type+11
  integer(pm_p),public,parameter:: pm_last_sys_type=pm_elemref_type

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
  subroutine init_typ(context)
    type(pm_context),pointer:: context
    integer:: i,j
    integer,dimension(2):: key
    integer:: flags
    character(len=12),dimension(pm_last_sys_type),parameter:: base_types= (/&
    'PM__tinyint','proc       ','type       ','name       ',&
    'null       ','sint       ','int        ','lint       ',&
    'int8       ','int16      ','int32      ','int64      ',&
    '<int128>   ','sreal      ','real       ','<real32>   ',&
    '<real64>   ','<real128>  ','scpx       ','cpx        ',&
    '<cpx64>    ','<cpx128>   ','<cpx256>   ','bool       ',&
    '<packbool> ','<ext>      ','<char>     ','<pointer>  ',&
    '<stack>    ','<usr>      ','<dict>     ','<set>      ',&
    'prc_info   ','string     ','<poly>     ','<struct>   ',&
    '<rec>      ','<polyref>  ','<array>    ','<cstarray> ',&
    '<dref>     ','<dref-inv> ','<elemref>  '/)
    
    context%tcache=pm_dict_new(context,128_pm_ln)
    context%pcache=pm_dict_new(context,1024_pm_ln)
    context%vcache=pm_set_new(context,1024_pm_ln)

    key(1)=pm_typ_is_basic
    do i=1,pm_null
       key(2)=pm_intern(context,trim(base_types(i)))
       if(pm_debug_level>2) then
          write(*,*) 'Init types(',trim(base_types(i)),')',key(2)
       endif
       j=pm_idict_add(context,context%tcache,key,2,&
            pm_null_obj)
       if(j/=i) call pm_panic('init_typ')
    enddo
    key(1)=pm_typ_is_basic+pm_typ_has_storage+pm_typ_leaves
    do i=pm_null+1,pm_last_sys_type
       key(2)=pm_intern(context,trim(base_types(i)))
       if(pm_debug_level>2) then
          write(*,*) 'Init types(',trim(base_types(i)),')',key(2)
       endif
       j=pm_idict_add(context,context%tcache,key,2,&
            pm_null_obj)
       if(j/=i) call pm_panic('init_typ')
    enddo
    key(1)=pm_typ_is_user
    do i=1,pm_last_sys_type
       if(base_types(i)(1:1)/='<') then
          key(2)=pm_intern(context,trim(base_types(i)))
          j=pm_idict_add(context,context%tcache,key,2,&
               pm_fast_typeno(context,i))
       endif
    enddo
    if(pm_debug_level>2) write(*,*) 'Types inited'
  contains
    include 'ftypeno.inc'
  end subroutine init_typ

  !============================================================
  ! Make type description node
  ! arr must contain type_kind type_name arg1 arg2 ...
  ! Basic version that does not accumulate flags
  ! - flags can be specified in an optional argument
  ! Val optional argument gives value associated with the type
  !============================================================
  function pm_new_basic_typ(context,arr,val,flags) result(tno)
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
       arr(1)=ior(arr(1),iand(flags,not(pm_typ_kind_mask+pm_typ_flags_untainting)))
    endif
    k=pm_ivect_lookup(context,context%tcache, &
         arr,size(arr))
    if(k==0) k=pm_idict_add(context,context%tcache,&
         arr,size(arr),tval)
    tno=k
  contains
    include 'ftiny.inc'
  end function pm_new_basic_typ
  
  !=============================================================
  ! Make type description record returning type number
  ! Types are hashed so the same kind/name/args always gives the
  ! same number
  ! arr must contain type_kind type_name arg1 arg2 ...
  ! val optional argument gives value associated with the type
  !=============================================================
  function pm_new_typ(context,arr,val) result(tno)
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
       flags=pm_typ_flags(context,arr(k))
       tflags=ior(tflags,flags)
       nleaves=max(nleaves+flags/pm_typ_leaves,pm_typ_max_leaves)
    enddo
    arr(1)=ior(arr(1),iand(tflags,&
         iand(pm_typ_leaves-1,not(pm_typ_kind_mask+pm_typ_flags_untainting))))+&
         nleaves*pm_typ_leaves
    k=pm_ivect_lookup(context,context%tcache, &
         arr,size(arr))
    if(k==0) k=pm_idict_add(context,context%tcache,&
         arr,size(arr),tval)
    tno=k
  contains
    include 'ftiny.inc'
  end function pm_new_typ


  !==========================================================================
  ! Associate a type with a name (not necessarily the same as the type name)
  ! Used by struct/rec declarations
  !==========================================================================
  subroutine pm_typ_record_by_name(context,name,typ)
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
  end subroutine pm_typ_record_by_name

  !==============================================================================
  ! Retrieve type associated with a given name (again not same as the type name)
  ! Used by struct/rec declarations
  !==============================================================================
  function pm_typ_from_recorded_name(context,name) result(typ)
    type(pm_context),pointer:: context
    integer,intent(in):: name
    integer:: typ
    type(pm_ptr):: key,val
    key=pm_fast_tinyint(context,name)
    val=pm_dict_lookup(context,context%pcache,key)
    typ=val%offset
  contains
    include 'ftiny.inc'
  end function pm_typ_from_recorded_name

  !====================================================
  ! New parameters type with n parameters
  !====================================================
  function pm_new_params_typ(context,n,typ) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: n,typ
    integer:: tno
    integer:: arr(3)
    arr(1)=pm_typ_new_params
    arr(2)=n
    arr(3)=typ
    tno=pm_new_basic_typ(context,arr,&
         flags=iand(pm_typ_flags(context,typ),not(pm_typ_has_params)))   
  end function pm_new_params_typ

  !=======================================
  ! Create new user type with body tno2
  !=======================================
  function pm_new_user_typ(context,arr,tno2) result(tno)
    type(pm_context),pointer:: context
    integer,dimension(:),intent(inout):: arr
    integer:: tno2
    integer:: tno
    tno=pm_new_basic_typ(context,arr,&
         val=pm_fast_typeno(context,tno2))
  contains
    include 'ftypeno.inc'
  end function pm_new_user_typ

  !========================================
  ! Create new array type: etyp[dtyp]
  !========================================
  function pm_new_arr_typ(context,kind,etyp,dtyp,styp) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: kind
    integer,intent(in):: etyp
    integer,intent(in):: dtyp
    integer,intent(in):: styp
    integer:: tno
    integer,dimension(5):: args
    integer:: flags
    flags=pm_typ_is_array+merge(pm_typ_has_array,0,kind/=sym_fix)
    args(1)=flags
    args(2)=kind
    args(3)=etyp
    args(4)=dtyp
    args(5)=styp 
    tno=pm_new_typ(context,args)
  end function pm_new_arr_typ

  !=========================
  ! Create type a or b
  !=========================
  function pm_typ_combine(context,a,b) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: a,b
    integer:: tno
    integer,dimension(4):: args
    type(pm_typ_einfo):: einfo
    if(a==b) then
       tno=a
       return
    elseif(pm_typ_includes(context,a,b,pm_typ_incl_typ,einfo)) then
       tno=a
       return
    elseif(pm_typ_includes(context,b,a,pm_typ_incl_typ,einfo)) then
       tno=b
       return
    endif
    args(1)=pm_typ_new_any
    args(2)=0
    args(3)=a
    args(4)=b
    tno=pm_new_typ(context,args)
  end function pm_typ_combine

  !==========================================
  ! Create new polymorphic type: @etype
  !==========================================
  function pm_new_poly_typ(context,etyp) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: etyp
    integer:: tno
    integer,dimension(3):: args
    args(1)=pm_typ_new_poly
    args(2)=0
    args(3)=etyp
    tno=pm_new_basic_typ(context,args)
  end function pm_new_poly_typ

  !==========================================
  !  Create new type-value type: <type>
  !==========================================
  function pm_new_type_typ(context,etyp) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: etyp
    integer:: tno
    integer,dimension(3):: args
    args(1)=pm_typ_new_type
    args(2)=0
    args(3)=etyp
    tno=pm_new_basic_typ(context,args)
  end function pm_new_type_typ

  
  !===============================================
  ! Create new includes: type inc type
  !================================================
  function pm_new_includes_typ(context,etyp,mtyp) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: etyp,mtyp
    integer:: tno
    integer,dimension(4):: args
    args(1)=pm_typ_new_includes
    args(2)=0
    args(3)=etyp
    args(4)=mtyp
    tno=pm_new_basic_typ(context,args)
  end function pm_new_includes_typ

  !==========================================
  ! Create new compile time value type
  !==========================================
  function pm_new_value_typ(context,val) result(tno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: val
    integer:: tno
    integer,dimension(3):: args
    args(1)=pm_typ_new_value
    args(2)=pm_set_add(context,context%vcache,val)
    args(3)=pm_fast_typeof(val)
    tno=pm_new_basic_typ(context,args,val)
  contains
    include 'ftypeof.inc'
  end function pm_new_value_typ

  !==============================================
  ! Create new compile time name value type
  !==============================================
  function pm_new_name_typ(context,name) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: name
    integer:: tno
    integer,dimension(2):: args
    args(1)=pm_typ_new_single_name
    if(pm_name_stem(context,name)==sym_distr_tag)  then
       args(1)=ior(args(1),pm_typ_has_distributed)
    endif
    args(2)=name
    tno=pm_new_typ(context,args)
  end function pm_new_name_typ
  
  !=============================================
  ! Create new compile time proc value type
  !=============================================
  function pm_new_proc_typ(context,name) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: name
    integer:: tno
    integer,dimension(2):: args
    args(1)=pm_typ_new_proc
    args(2)=name
    tno=pm_new_typ(context,args)
  end function pm_new_proc_typ
  
  !==========================================
  ! Create internal vector type ^^(T)
  !==========================================
  function pm_new_vect_typ(context,tno) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: tno2
    integer,dimension(3):: args
    args(1)=pm_typ_new_vect
    args(2)=0
    args(3)=tno
    tno2=pm_new_typ(context,args)
  end function pm_new_vect_typ
  
  !=================================================
  ! Change the value associated with a type
  ! (used for user and value types)
  !=================================================
  subroutine pm_typ_set_val(context,tno,val)
    type(pm_context),pointer:: context
    integer:: tno
    type(pm_ptr),intent(in):: val
    call pm_dict_set_val(context,context%tcache,int(tno,pm_ln),val)
  end subroutine pm_typ_set_val

  !===================================
  ! Return the kind of a given type
  !===================================
  function pm_typ_kind(context,tno) result(kind)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: kind
    if(tno<=0) then
       kind=0
    else
       kind=pm_tv_kind(pm_typ_vect(context,tno))
    endif
  end function pm_typ_kind

  !=========================================
  ! Return flags for a given type
  !=========================================
  function pm_typ_flags(context,tno) result(flags)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: flags
    integer:: tno2
    type(pm_ptr):: tv
    if(tno==0) then
       flags=pm_typ_has_generic
       return
    else
       tv=pm_typ_vect(context,tno)
       flags=pm_tv_flags(tv)
       tno2=tno
       do while(iand(flags,pm_typ_kind_mask)==pm_typ_is_user)
          tv=pm_dict_val(context,context%tcache,int(tno2,pm_ln))
          tno2=tv%offset
          if(tno2/=0) then
             tv=pm_typ_vect(context,tno2)
             flags=pm_tv_flags(tv)
          else
             flags=pm_typ_has_generic
          endif
       enddo
    endif
  contains
    include 'fvkind.inc'
  end function pm_typ_flags

  !=================================================
  ! Return number of leaves associated with a type
  !=================================================
  function pm_typ_needs_storage(context,tno) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    logical:: ok
    ok=iand(pm_typ_flags(context,tno),pm_typ_has_storage)/=0
  end function pm_typ_needs_storage

  !=================================================
  ! Return number of leaves associated with a type
  !=================================================
  function pm_typ_num_leaves(context,tno) result(n)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: n
    n=pm_typ_flags(context,tno)/pm_typ_leaves
  end function pm_typ_num_leaves

  !=================================================
  ! Return argument #n of type tno
  !=================================================
  function pm_typ_arg(context,tno,n) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,n
    integer:: tno2
    type(pm_ptr):: tv
    tv=pm_typ_vect(context,tno)
    tno2=pm_tv_arg(tv,n)
  end function pm_typ_arg

  !=================================================
  ! Return name associated with type tno
  !=================================================
  function pm_typ_name(context,tno) result(name)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: name
    type(pm_ptr):: tv
    tv=pm_typ_vect(context,tno)
    name=pm_tv_name(tv)
  end function pm_typ_name

  !=====================================================
  ! Return name of element #n associated with type tno
  !==============================---===================
  function pm_typ_elem_name(context,tno,n) result(name)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,n
    integer:: name,kind
    type(pm_ptr):: tv,namev
    tv=pm_typ_vect(context,tno)
    if(pm_debug_checks) then
       kind=pm_tv_kind(tv)
       if(kind/=pm_typ_is_struct.and.kind/=pm_typ_is_rec) then
          write(*,*) 'tno=',tno,'kind=',kind
          call pm_panic('typ_elem_name not struct/rec')
       endif
    endif
    name=pm_tv_name(tv)
    namev=pm_name_val(context,name)
    name=namev%data%i(namev%offset+n)
  end function pm_typ_elem_name
  
  !==========================================
  ! Look up type and return number
  ! (returns zero if not found)
  !==========================================
  function pm_typ_lookup(context,arr) result(tno)
    type(pm_context),pointer:: context
    integer,dimension(:):: arr
    integer:: tno
    tno=pm_ivect_lookup(context,context%tcache, &
         arr,size(arr))
  end function pm_typ_lookup

  !===============================================================
  ! Look up user type and return type number it is defined to be
  !===============================================================
  function pm_user_typ_lookup(context,arr) result(tno)
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
  end function pm_user_typ_lookup

  !====================================================
  ! Lookup parameterless user type with given name
  !====================================================
  function pm_user_typ_lookup_by_name(context,name) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: name
    integer:: tno
    integer:: arr(2)
    arr(1)=pm_typ_new_user
    arr(2)=name
    tno=pm_user_typ_lookup(context,arr)
  end function pm_user_typ_lookup_by_name

  !====================================================
  ! Get type number of body of user type definition
  !====================================================
  function pm_user_typ_body(context,typ) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer:: tno
    type(pm_ptr):: v
    v=pm_dict_val(context,context%tcache,int(typ,pm_ln))
    tno=v%offset
  end function pm_user_typ_body

  !==============================================
  ! Set the body of a user type declaration
  !==============================================
  subroutine pm_user_typ_set_body(context,typ,tno)
    type(pm_context),pointer:: context
    integer,intent(in):: typ,tno
    call pm_typ_set_val(context,typ,&
         pm_fast_typeno(context,tno))
  contains
    include 'ftypeno.inc'
  end subroutine pm_user_typ_set_body

  !=======================================================
  ! If basic numeric type return name, else return -1
  ! (mainly used for casting)
  !=======================================================
  function pm_typ_numeric_name(context,typ) result(name)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer:: name,tno
    type(pm_ptr):: tv
    tno=typ
    if(tno==0) then
       name=-1
       return
    endif
    tv=pm_typ_vect(context,typ)
    if(pm_tv_kind(tv)==pm_typ_is_basic) then
       name=pm_tv_name(tv)
    elseif(pm_tv_kind(tv)==pm_typ_is_user) then
       tno=pm_user_typ_body(context,typ)
       tv=pm_typ_vect(context,tno)
       if(pm_tv_kind(tv)==pm_typ_is_basic) then
          name=pm_tv_name(tv)
       else
          name=-1
       endif
    else
       name=-1
    endif
    if(tno<pm_int.or.tno>=pm_logical) name=-1
  end function pm_typ_numeric_name

  !===============================================
  ! Value associate with value or const type
  !===============================================
  function pm_typ_val(context,typ) result(v)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    type(pm_ptr):: v
    v=pm_dict_val(context,context%tcache,int(typ,pm_ln))
  end function pm_typ_val

  !===============================================
  ! Strip off non-storage elements of a type
  ! including one-element structs/recs
  !===============================================
  recursive function pm_typ_strip_to_basic(context,typ) result(typ2)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer:: typ2
    type(pm_ptr):: tv
    integer:: kind
    tv=pm_typ_vect(context,typ)
    kind=pm_tv_kind(tv)
    select case(kind)
    case(pm_typ_is_all,pm_typ_is_vect,pm_typ_is_enveloped,pm_typ_is_param)
       typ2=pm_typ_strip_to_basic(context,pm_tv_arg(tv,1))
    case(pm_typ_is_user)
       typ2=pm_user_typ_body(context,typ)
    case default
       typ2=typ
    end select
  end function pm_typ_strip_to_basic

  !==============================================
  ! Get mode from type (default private)
  !==============================================
  function pm_typ_get_mode(context,typ) result(mode)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer:: mode
    integer:: tk
    type(pm_ptr):: tv
    if(typ<=0) then
       mode=sym_mirrored
       return
    endif
    tv=pm_typ_vect(context,typ)
    if(pm_tv_kind(tv)==pm_typ_is_par_kind) then
       mode=iand(pm_tv_name(tv),mode_mask)
    else
       mode=sym_coherent
    endif
  end function pm_typ_get_mode

  !=========================================================================
  ! Strip mode information, mode, from type typ yielding unmoded type typ2
  !=========================================================================
  function pm_typ_strip_mode(context,typ,mode) result(typ2)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer,intent(out):: mode
    integer:: typ2
    integer:: tk
    type(pm_ptr):: tv
    if(typ<=0) then
       typ2=typ
       mode=merge(sym_coherent,sym_mirrored,typ==0)
       return
    endif
    tv=pm_typ_vect(context,typ)
    if(pm_tv_kind(tv)==pm_typ_is_par_kind) then
       mode=iand(pm_tv_name(tv),mode_mask)
       typ2=pm_tv_arg(tv,1)
    else
       mode=sym_coherent
       typ2=typ
    endif
  end function pm_typ_strip_mode

  !==========================================================================
  ! Strip mode information, mode, from type typ yielding unmoded type typ2
  ! Return in cond whether mode indicates a conditional context
  !==========================================================================
  function pm_typ_strip_mode_and_cond(context,typ,mode,cond) result(typ2)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer,intent(out):: mode
    logical,intent(out):: cond
    integer:: typ2
    integer:: tk
    type(pm_ptr):: tv
    if(typ<=0) then
       typ2=typ
       mode=merge(sym_coherent,sym_mirrored,typ==0)
       cond=.false.
       return
    endif
    tv=pm_typ_vect(context,typ)
    if(pm_tv_kind(tv)==pm_typ_is_par_kind) then
       mode=pm_tv_name(tv)
       typ2=pm_tv_arg(tv,1)
       cond=mode==sym_partial
    else
       mode=sym_coherent
       typ2=typ
       cond=.false.
    endif
  end function pm_typ_strip_mode_and_cond

  !=============================================
  ! Add mode information to an unmoded type
  !=============================================
  function pm_typ_add_mode(context,typ,mode,iscond,istyp) result(typ2)
    type(pm_context),pointer:: context
    integer,intent(in):: typ,mode
    logical,intent(in):: iscond
    logical,intent(in),optional:: istyp
    integer:: typ2,mode2,typ3
    integer:: array(3)
    if(typ<0) then
       typ2=typ
       return
    endif
    typ3=pm_typ_strip_mode(context,typ,mode2)
    if(mode2/=sym_coherent) then
       write(*,*) trim(sym_names(mode2))
       call pm_panic('add-mode to moded type')
    endif
    if(mode==sym_coherent.and..not.(iscond.or.present(istyp))) then
       typ2=typ
    else
       array(1)=pm_typ_new_par_kind
       array(2)=merge(sym_partial,mode,iscond)
       array(3)=typ
       typ2=pm_new_typ(context,array)
    endif
  end function pm_typ_add_mode


  !========================================================
  ! Replace mode information in a (possibly) moded type
  !========================================================
  function pm_typ_replace_mode(context,typ1,mode,iscond) result(typ2)
    type(pm_context),pointer:: context
    integer,intent(in):: typ1,mode
    logical,intent(in):: iscond
    integer:: typ2
    integer:: array(3),typ
    type(pm_ptr):: tv

    if(typ1<=0) then
       typ2=typ1
       return
    endif
    tv=pm_typ_vect(context,typ1)
    if(pm_tv_kind(tv)==pm_typ_is_par_kind) then
       typ=pm_tv_arg(tv,1)
    else
       typ=typ1
    endif
    if(mode==sym_coherent.and..not.iscond) then
       typ2=typ
    else
       array(1)=pm_typ_new_par_kind
       array(2)=merge(sym_partial,mode,iscond)
       array(3)=typ
       typ2=pm_new_typ(context,array)
    endif
  end function pm_typ_replace_mode

  
  !=============================================================================================
  ! Rules for combining modes in a standard procedure call or stucture creation
  !
  ! Error codes:
  !   combined_mode=-1,-2...
  !        Shared distributed value not allowed for position -combined_mode
  !   combined_mode=-1001,-1002,...
  !        Partial value not allowed in position -(combined_mode+1000)
  !  shared_ok -- permissible to have an argumnet with 'shared' mode
  !  complete  -- cannot have an argument mode associated with a conditional context
  !============================================================================================
  function pm_typ_combine_modes(context,array,shared_ok,complete,cond,unlabelled)&
       result(combined_mode)
    type(pm_context),pointer:: context
    integer,intent(in),dimension(:):: array
    logical,intent(in):: shared_ok,complete,cond,unlabelled
    integer:: combined_mode
    integer:: i,mode,cmode,tno
    if(cond) then
       combined_mode=sym_partial
       return
    endif
    !cmode=merge(sym_shared,sym_mirrored,shared_ok)
    cmode=sym_mirrored
    do i=1,size(array)
       tno=pm_typ_strip_mode(context,array(i),mode)
       if(complete) then
          if(mode==sym_partial) then
             combined_mode=-i-1000
          elseif(mode==sym_coherent&
            .and.unlabelled) then
             combined_mode=-i-2000
             return
          endif
       endif
       if(mode==sym_shared.and..not.shared_ok) then
          if(iand(pm_typ_flags(context,tno),pm_typ_has_distributed)/=0) then
             combined_mode=-i
             return
          endif
       endif
       cmode=min(cmode,mode)
    enddo
    if(cmode==sym_chan) cmode=sym_coherent
    combined_mode=cmode
  end function pm_typ_combine_modes

  !===================================
  ! Does mode1 include mode2 ?
  !===================================
  function pm_mode_includes(mode1,mode2) result(ok)
    integer,intent(in):: mode1,mode2
    logical:: ok
    if(mode1==mode2) then
       ok=.true.
    elseif(mode1==sym_private) then
       ok=mode2>=sym_partial.and.mode2<sym_mirrored
    elseif(mode1==sym_invar) then
       ok=mode2>=sym_mirrored
    elseif(mode1==sym_complete) then
       ok=mode2>sym_partial.and.mode2<sym_mirrored
    elseif(mode1==sym_universal) then
       ok=mode2/=sym_partial
    elseif(mode1==sym_local) then
       ok=mode2/=sym_shared
    else
       ok=.false.
    endif
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

  
  !===================================================
  ! Convert mode2 to mode1
  ! Returns -1 if conversion never possible 
  !         -2 if conversion not possible
  !            in a conditional context
  !===================================================
  function pm_typ_convert_mode(mode1,mode2,iscond) result(mode3)
    integer,intent(in):: mode1,mode2
    logical,intent(in):: iscond
    integer:: mode3
    mode3=-1
    if(mode1==mode2) then
       mode3=mode2
       return
    else
       select case(mode1)
       case(sym_private)
          mode3=merge(sym_partial,sym_coherent,iscond)
       case(sym_invar)
          if(mode2>=sym_mirrored) mode3=mode2
          if(iscond) mode3=-2
       case(sym_complete)
          mode3=mode1
          if(mode1/=mode2.and.iscond) mode3=-2
       case(sym_partial)
          mode3=mode1
       case(sym_coherent)
          mode3=mode1
          if(mode1/=mode2.and.iscond) mode3=-2
       case(sym_mirrored)
          if(mode2>=sym_mirrored) mode3=mode1
          if(mode1/=mode2.and.iscond) mode3=-2
       case(sym_shared)
          if(mode2>=sym_mirrored) mode3=mode1
          if(mode1/=mode2.and.iscond) mode3=-2
       end select
    endif
  end function pm_typ_convert_mode

  !==========================================================
  ! Remove both mode information and internal vector type
  !==========================================================
  function pm_typ_strip_mode_and_vect(context,tno) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: tno2
    type(pm_ptr):: tv
    integer:: mode
    tno2=pm_typ_strip_mode(context,tno,mode)
    tv=pm_typ_vect(context,tno2)
    if(pm_tv_kind(tv)==pm_typ_is_vect) then
       tno2=pm_typ_strip_mode(context,pm_tv_arg(tv,1),mode)
    endif
  end function pm_typ_strip_mode_and_vect

  !=============================================================================
  ! Check if two concrete types equal (ignoring modes and vector type wrappers)
  !=============================================================================
  function pm_typ_equal(context,tno1,tno2) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno1,tno2
    logical:: ok
    type(pm_ptr):: tv1,tv2
    integer:: typ1,typ2,tk1,tk2
    if(tno1==tno2) then
       ok=.true.
    else
       tv1=pm_typ_vect(context,tno1)
       tv2=pm_typ_vect(context,tno2)
       tk1=pm_tv_kind(tv1)
       tk2=pm_tv_kind(tv2)
       typ1=tno1
       typ2=tno2
       if(tk1==pm_typ_is_par_kind.or.tk1==pm_typ_is_vect) then
          typ1=pm_tv_arg(tv1,1)
       endif
       if(tk2==pm_typ_is_par_kind.or.tk2==pm_typ_is_vect) then
          typ2=pm_tv_arg(tv2,1)
       endif
       ok=typ1==typ2
       if(.not.ok) then
          tv1=pm_typ_vect(context,typ1)
          tv2=pm_typ_vect(context,typ2)
          tk1=pm_tv_kind(tv1)
          tk2=pm_tv_kind(tv2)
          if(tk1==pm_typ_is_par_kind.or.tk1==pm_typ_is_vect) then
             typ1=pm_tv_arg(tv1,1)
          endif
          if(tk2==pm_typ_is_par_kind.or.tk2==pm_typ_is_vect) then
             typ2=pm_tv_arg(tv2,1)
          endif
          ok=typ1==typ2
       endif
    endif
  end function pm_typ_equal

  !===================================================================
  ! Given a struct/rec template and type, return type parameters
  !==================================================================
  function pm_typ_extract_params(context,templ,typ,params) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: templ,typ
    integer,intent(inout),dimension(:):: params
    type(pm_typ_einfo):: einfo
    logical:: ok
    integer:: ubase
    integer,dimension(max_user_nesting):: user
    ubase=1
    ok=pm_test_typ_includes(context,templ,typ,&
         pm_typ_incl_val+pm_typ_incl_extract,einfo,params,1,user,ubase)
  end function pm_typ_extract_params

  !======================================
  ! Does supertype include subtype?
  !======================================
  function pm_typ_includes(context,supertype,subtype,&
       mode,einfo) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: supertype,subtype
    integer,intent(in):: mode
    type(pm_typ_einfo),intent(out):: einfo
    logical:: ok
    integer:: ubase
    integer,dimension(max_user_nesting):: user,params

    if(pm_typ_extra_debug) then
       write(*,*) 'CHECK',trim(pm_typ_as_string(context,supertype)),'>>',&
            trim(pm_typ_as_string(context,subtype))
    endif
    ubase=1
    
    ! This deals with rare problem of *(..T..) where T is struct/rec parameter
    if(iand(mode,pm_typ_incl_indirect)/=0) then
       if(iand(pm_typ_flags(context,supertype),pm_typ_has_params)/=0) then
          params=-1
       endif
    endif

    einfo%kind=pm_typ_err_none
    einfo%typ1=supertype
    einfo%typ2=subtype
    ok=pm_test_typ_includes(context,supertype,subtype,&
         mode,einfo,params,1,user,ubase)
    if(pm_typ_extra_debug) then
       write(*,*) 'CHECKED ',ok,trim(pm_typ_as_string(context,supertype)),'>>',&
            trim(pm_typ_as_string(context,subtype))
    endif
  end function pm_typ_includes

  !======================================================
  ! Does supertype include subtype?
  ! This routine does the work
  ! Working space:
  !    params(base:) storage for matching parameters
  !    user(ubase:)  maintains a stack of active user types to
  !                  prevent runaway recursion
  !======================================================
  recursive function pm_test_typ_includes(context,supertype,subtype,&
       mode,einfo,params,base,user,ubase)&
       result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: supertype,subtype
    integer,intent(in):: mode
    type(pm_typ_einfo),intent(out):: einfo
    integer,dimension(:),intent(inout):: params
    integer,intent(in):: base
    integer,dimension(:),intent(inout):: user
    integer,intent(in):: ubase
    logical:: ok
    integer:: p,q,s
    type(pm_ptr):: t,u,r
    integer:: i,j,tk,uk,nt,nu
    logical:: has_d

    if(pm_typ_extra_debug) then
       write(*,*) '=================='
       write(*,*) 'Test incl',supertype,subtype
       write(*,*) trim(pm_typ_as_string(context,supertype)),' ',&
            trim(pm_typ_as_string(context,subtype))
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
          t=pm_typ_vect(context,p)
          tk=pm_tv_kind(t)
          if(tk==pm_typ_is_user) then
             r=pm_dict_val(context,context%tcache,int(p,pm_ln))
             ok=pm_test_typ_includes(context,int(r%offset),q,&
                  mode,einfo,params,base,user,ubase)
          elseif(tk==pm_typ_is_any) then
             do i=1,pm_tv_numargs(t)
                if(pm_test_typ_includes(context,pm_tv_arg(t,i),q,&
                     mode,einfo,params,base,user,ubase)) then
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
    
    t=pm_typ_vect(context,p)
    u=pm_typ_vect(context,q)
    uk=pm_tv_kind(u)
    tk=pm_tv_kind(t)

    if(tk==pm_typ_is_includes) then
       if(uk==pm_typ_is_includes) then
          ok=pm_test_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               ior(mode,pm_typ_incl_nomatch),einfo,params,base,user,ubase)
          if(ok) then
             ok=ok.and.pm_test_typ_includes(context,&
                  pm_tv_arg(u,2),pm_tv_arg(t,2),&
                  pm_typ_incl_equiv,einfo,params,base,user,ubase)
          endif
       else
          ok=pm_test_typ_includes(context,pm_tv_arg(t,1),q,&
               ior(mode,pm_typ_incl_nomatch),einfo,params,base,user,ubase)
          if(ok) then
             ok=ok.and.pm_test_typ_includes(context,&
                  q,pm_tv_arg(t,2),&
                  ior(mode,pm_typ_incl_nomatch),einfo,params,base,user,ubase)
          endif
       endif
       return
    endif

    
    ! Cases where the second type needs to be checked first
    select case(uk)
    case(pm_typ_is_proc)
       if(p==pm_proc) then
          ok=.true.
          return
       endif
    case(pm_typ_is_single_name)
       if(p==pm_name) then
          ok=.true.
          return
       endif
    case(pm_typ_is_value)
       select case(tk)
       case(pm_typ_is_const)
          ok=pm_test_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),mode,einfo,&
               params,base,user,ubase)
          return
       case(pm_typ_is_value)
          ok=pm_tv_name(t)==pm_tv_name(u)
          return
       case(pm_typ_is_basic)
          ok=pm_test_typ_includes(context,p,pm_tv_arg(u,1),mode,einfo,&
               params,base,user,ubase)
          return
       end select
    case(pm_typ_is_const)
       ok=pm_test_typ_includes(context,p,pm_tv_arg(u,1),mode,einfo,&
            params,base,user,ubase)
       return
    case(pm_typ_is_user)
       if(tk/=pm_typ_is_user) then
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
          ok=pm_test_typ_includes(context,p,int(r%offset),&
               mode,einfo,params,base,user,ubase+2)
          return
       endif
    case(pm_typ_is_any)
       do i=1,pm_tv_numargs(u)
           if(.not.pm_test_typ_includes(context,p,pm_tv_arg(u,i),&
               ior(mode,pm_typ_incl_nomatch),einfo,params,base,user,ubase)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
       return
    case(pm_typ_is_all)
       do i=1,pm_tv_numargs(u)
          if(pm_test_typ_includes(context,p,pm_tv_arg(u,i),&
               mode,einfo,params,base,user,ubase)) then
             ok=.true.
             return
          endif
       enddo
       ok=.false.
       return
    case(pm_typ_is_except)
       if(tk==pm_typ_is_except) then
          ok=pm_test_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               mode,einfo,params,base,user,ubase).and.&
               pm_test_typ_includes(context,pm_tv_arg(u,2),pm_tv_arg(t,2),&
               ior(mode,pm_typ_incl_nomatch),einfo,params,base,user,ubase)
          return
       else
          ok=pm_test_typ_includes(context,p,pm_tv_arg(u,1),&
               ior(mode,pm_typ_incl_nomatch),einfo,params,base,user,ubase)
          return
       endif
    case(pm_typ_is_includes)
       ok=pm_test_typ_includes(context,p,pm_tv_arg(u,1),&
            mode,einfo,params,base,user,ubase)
       return
    case(pm_typ_is_undef_result)
       ok=.false.
       return
    case(pm_typ_is_par_kind)
       nu=pm_tv_name(u)
       if(tk==pm_typ_is_par_kind) then
          nt=pm_tv_name(t)
          if(iand(mode,pm_typ_incl_val)/=0) then
             ok=pm_mode_includes(nt,nu)
             if(ok) then
                ok=pm_test_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
                     mode,einfo,params,base,user,ubase)
             endif
          else
             if(nt==nu) then
                ok=pm_test_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
                     mode,einfo,params,base,user,ubase)
             else
                ok=pm_mode_includes(nt,nu)
             endif
          endif
          return
       else
          ok=pm_test_typ_includes(context,p,pm_tv_arg(u,1),&
                  mode,einfo,params,base,user,ubase)
          return
       endif
    case(pm_typ_is_param)
       ok=pm_test_typ_includes(context,p,pm_tv_arg(u,1),&
            mode,einfo,params,base,user,ubase)
       return
!!$    case(pm_typ_is_vect)
!!$       if(iand(pm_typ_flags(context,p),pm_typ_has_vect)==0) then
!!$             ok=pm_test_typ_includes(context,p,pm_tv_arg(u,1),&
!!$                  mode,einfo,params,base,user,ubase)
!!$          return
!!$       endif
    case(pm_typ_is_bottom)
       ok=.true.
       return
    end select
 
    select case(tk)
    case(pm_typ_is_basic)
       ok=.false.
    case(pm_typ_is_dref)
       if(tk/=uk) then
          ok=.false.
          return
       endif
       nt=pm_tv_name(t)
       nu=pm_tv_name(u)
       if(nt==pm_dref_is_any) then
          if(nu/=pm_dref_is_any.and.iand(mode,pm_typ_incl_typ)/=0) then
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
          if(.not.pm_test_typ_includes(context,pm_tv_arg(t,i),&
               pm_tv_arg(u,i),mode,einfo,params,base,user,ubase)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
    case(pm_typ_is_struct,pm_typ_is_rec)
       if(tk/=uk) then
          ok=.false.
          return
       endif
       if(.not.pm_tv_name(t)==pm_tv_name(u)) then
          ok=.false.
          return
       endif
       do i=1,pm_tv_numargs(t)
          if(.not.pm_test_typ_includes(context,pm_tv_arg(t,i),&
               pm_tv_arg(u,i),mode,einfo,params,base,user,ubase)) then
             ok=.false.
             einfo%kind=ior(einfo%kind,pm_typ_err_elem)
             einfo%name=pm_tv_name(t)
             einfo%index=i
             einfo%typ1=pm_tv_arg(t,i)
             einfo%typ2=pm_tv_arg(u,i)
             return
          endif
       enddo
       ok=.true.
    case(pm_typ_is_array)
       if(uk/=pm_typ_is_array) then
          ok=.false.
       else
          if(.not.(pm_tv_name(t)==pm_tv_name(u).or.pm_tv_name(t)==0)) then
             ok=.false.
          else
             ok=pm_test_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
                  mode,einfo,params,base,user,ubase).and.&
                  pm_test_typ_includes(context,pm_tv_arg(t,2),pm_tv_arg(u,2),&
                  mode,einfo,params,base,user,ubase)
          endif
       endif
    case(pm_typ_is_type,pm_typ_is_poly)
       if(uk/=tk) then
          ok=.false.
       else
          ok=pm_test_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               ior(mode,pm_typ_incl_nomatch),einfo,params,base,user,ubase)
       endif
    case(pm_typ_is_tuple,pm_typ_is_vtuple)
       if(uk/=pm_typ_is_tuple.and.uk/=pm_typ_is_vtuple) then
          ok=.false.
       elseif(tk==pm_typ_is_tuple.and.uk==pm_typ_is_vtuple) then
          ok=.false.
       elseif(pm_tv_name(t)/=pm_tv_name(u)) then
          ok=.false.
       else
          nt=pm_tv_numargs(t)
          nu=pm_tv_numargs(u)
          if(nt>nu.and.uk/=pm_typ_is_vtuple) then
             ok=.false.
             return
          endif
          if(nu>nt.and.tk/=pm_typ_is_vtuple) then
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
             if(.not.pm_test_typ_includes(context,pm_tv_arg(t,i),&
                  pm_tv_arg(u,i),mode,einfo,params,base,user,ubase)) then
                ok=.false.
                return
             endif
          enddo
          if(nu>nt) then
             do i=nt+1,nu
                if(.not.pm_test_typ_includes(context,pm_tv_arg(t,nt),&
                     pm_tv_arg(u,i),mode,einfo,params,base,&
                     user,ubase)) then
                   ok=.false.
                   return
                endif
             enddo
          else
             do i=nu+1,nt
                if(.not.pm_test_typ_includes(context,pm_tv_arg(t,nt),&
                     pm_tv_arg(u,i),mode,einfo,params,base,&
                     user,ubase)) then
                   ok=.false.
                   return
                endif
             enddo
          endif
          ok=.true.
       endif
    case(pm_typ_is_user)
       if(uk==pm_typ_is_user) then
          ! Check P(p1,p2) < Q(q1,q2) <=> p1<q1, p2<q2 if P==Q
          if(pm_tv_name(t)==pm_tv_name(u)) then
             nt=pm_tv_numargs(t)
             nu=pm_tv_numargs(u)
             if(nt==nu) then
                do i=1,nt
                   ok=pm_test_typ_includes(context,&
                        pm_tv_arg(t,i),pm_tv_arg(u,i),&
                        mode,einfo,params,base,user,ubase)
                   if(.not.ok) return
                enddo
                ok=.true.
                return
             endif
          endif
       endif
       if(iand(mode,pm_typ_incl_val)==0) then
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
          ok=pm_test_typ_includes(context,int(r%offset),q,&
               mode,einfo,params,base,user,ubase+2)
       else
          r=pm_dict_val(context,context%tcache,int(p,pm_ln))
          ok=pm_test_typ_includes(context,int(r%offset),q,&
               mode,einfo,params,base,user,ubase)
       endif
    case(pm_typ_is_any)
       do i=1,pm_tv_numargs(t)
          if(pm_test_typ_includes(context,pm_tv_arg(t,i),q,&
               ior(mode,pm_typ_incl_nomatch),einfo,params,base,user,ubase)) then
             ok=.true.
             return
          endif
      enddo
      ok=.false.
    case(pm_typ_is_all)
       do i=1,pm_tv_numargs(t)
          if(.not.pm_test_typ_includes(context,pm_tv_arg(t,i),q,&
               mode,einfo,params,base,user,ubase)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
    case(pm_typ_is_single_name)
       ok=.false.
    case(pm_typ_is_proc)
       if(uk/=pm_typ_is_proc) then
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
          ok=pm_test_typ_includes(context,&
               pm_tv_arg(t,1),pm_tv_arg(u,1),&
               pm_typ_incl_typ+pm_typ_incl_nomatch,&
               einfo,params,base,user,ubase)
       endif
    case(pm_typ_is_proc_sig)
       if(uk/=pm_typ_is_proc_sig) then
          ok=.false.
          return
       endif
       if(pm_tv_name(t)/=pm_tv_name(u)) then
          ok=.false.
          return
       endif
       ok=pm_test_typ_includes(context,&
               pm_tv_arg(u,1),pm_tv_arg(t,1),&
               pm_typ_incl_typ+pm_typ_incl_nomatch,&
               einfo,params,base,user,ubase).and.&
               pm_test_typ_includes(context,&
               pm_tv_arg(t,2),pm_tv_arg(u,2),&
               pm_typ_incl_typ+pm_typ_incl_nomatch,&
               einfo,params,base,user,ubase)
    case(pm_typ_is_par_kind)
       ! Most cases catered for by uk switch - remaining case
       ok=iand(mode,pm_typ_incl_val)/=0.and.&
            pm_mode_includes(pm_tv_name(t),sym_coherent).and.&
            pm_test_typ_includes(context,pm_tv_arg(t,1),q,&
            mode,einfo,params,base,user,ubase)
    case(pm_typ_is_undef_result)
       ok=.false.
    case(pm_typ_is_contains)
       if(uk==pm_typ_is_contains) then
          ok=pm_test_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               ior(mode,pm_typ_incl_nomatch),einfo,params,base,user,ubase)
       else
          ok=pm_typ_contains_elem(context,pm_tv_arg(t,1),q,&
               ior(mode,pm_typ_incl_nomatch),einfo,params,base,user,ubase)
       endif
    case(pm_typ_is_has)
       if(uk==pm_typ_is_has) then
          ok=pm_test_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               mode,einfo,params,base,user,ubase)
       else
          i=ubase+1
          r=pm_typ_vect(context,pm_tv_arg(t,1))
          if(pm_tv_kind(r)==pm_typ_is_interface) then
             ok=pm_test_typ_includes(context,pm_tv_arg(t,1),q,&
                  mode,einfo,params,base,user,ubase)
             if(ok) return
          elseif(pm_tv_kind(r)==pm_typ_is_proc.and.&
               pm_tv_kind(u)==pm_typ_is_proc) then
             do i=1,pm_tv_numargs(u)
                if(pm_proc_typ_conforms(context,pm_tv_arg(t,1),&
                     pm_tv_arg(u,i))) then
                   ok=.true.
                   return
                endif
             enddo
          endif
          call pm_indirect_include(context,pm_tv_arg(t,1),q,user,&
               size(user),i,einfo,j,s)
          ok=s==pm_elem_found
          if(ok) then
             ! This test does parameter checking in correct context
             ! (indirect_include checks in isolated context)
             ok=pm_test_typ_includes(context,&
                  pm_tv_arg(t,1),j,&
                  mode,einfo,params,base,user,ubase)
          endif
       endif
    case(pm_typ_is_value)
       if(uk/=pm_typ_is_value) then
          ok=.false.
       else
          ok=pm_tv_name(t)==pm_tv_name(u)
       endif
    case(pm_typ_is_const)
       if(uk/=pm_typ_is_const.and.uk/=pm_typ_is_value) then
          ok=pm_test_typ_includes(context,pm_tv_arg(t,1),q,&
               mode,einfo,params,base,user,ubase)
          if(iand(pm_typ_flags(context,q),pm_typ_has_storage)/=0) ok=.false.
       else
          ok=pm_test_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
               mode,einfo,params,base,user,ubase)
       endif
    case(pm_typ_is_interface,pm_typ_is_enveloped)
       if(uk==pm_typ_is_interface.or.uk==pm_typ_is_enveloped) then
          ok=pm_tv_name(t)==pm_tv_name(u)
       else
          ok=.false.
       endif
    case(pm_typ_is_except)
       ok=pm_test_typ_includes(context,pm_tv_arg(t,1),q,&
            mode,einfo,params,base,user,ubase)
       if(ok) then
          ok=.not.pm_test_typ_includes(context,pm_tv_arg(t,2),q,&
            mode,einfo,params,base,user,ubase)
       endif
    case(pm_typ_is_params)
       nt=pm_tv_name(t)
       if(base+nt>size(params)) then
          call pm_panic('Program too complex - Excessive type nesting')
       endif
       params(base:base+nt)=-1
       ok=pm_test_typ_includes(context,pm_tv_arg(t,1),q,&
            mode,einfo,params,base+nt,user,ubase)
    case(pm_typ_is_param)
       ok=pm_test_typ_includes(context,pm_tv_arg(t,1),q,&
            mode,einfo,params,base,user,ubase)
       if(ok.and.iand(mode,pm_typ_incl_extract)/=0) then
          if(iand(mode,pm_typ_incl_nomatch)/=0) return
          nt=pm_tv_name(t)
          if(params(nt)==-1) then
             params(nt)=q
          else
             params(nt)=pm_typ_combine(context,params(nt),q)
          endif
       endif
    case(pm_typ_is_amp,pm_typ_is_vect)
       ok=tk==uk
       if(ok) ok=pm_test_typ_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
            mode,einfo,params,base,user,ubase)
    case(pm_typ_is_bottom)
       ok=.false.
    case default
       write(*,*) 'Type=',p
       write(*,*) 'Kind=',pm_tv_kind(t)
       write(*,*) 'Name=',pm_tv_name(t)
       do i=1,pm_tv_numargs(t)
          write(*,*) 'Arg=',pm_tv_arg(t,i)
       enddo
       call pm_panic('pm_test_typ_includes bad type kind')
    end select
    
  contains

    include 'fesize.inc'
    include 'fisnull.inc'
    include 'ftypeno.inc'
    
  end function pm_test_typ_includes


  ! Does type correspond to only one concrete type
  recursive function pm_typ_is_concrete(context,tno) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    logical:: ok
    ok=iand(pm_typ_flags(context,tno),pm_typ_has_generic)==0
  contains
    include 'fisnull.inc'
  end function pm_typ_is_concrete

  ! Does a type directly include itself (not as element of
  ! embedded struct/rec or array)
  recursive function pm_typ_is_recur(context,rno,tno) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: rno,tno
    logical:: ok
    type(pm_ptr):: tv,r
    integer:: tno2
    integer:: j
    ok=.false.
    if(tno==0) return
    tv=pm_typ_vect(context,tno)
    if(pm_tv_kind(tv)==pm_typ_is_any.or.&
         pm_tv_kind(tv)==pm_typ_is_all) then
       do j=1,pm_tv_numargs(tv)
          tno2=pm_tv_arg(tv,j)
          if(tno2==rno) then
             ok=.true.
             return
          elseif(pm_typ_is_recur(context,rno,tno2)) then
             ok=.true.
             return
          endif
       enddo
    endif
  end function pm_typ_is_recur

  ! Does a type contain an element (structure/rec component,
  ! array domain or values, applied recursively) of a given
  ! type?
  recursive function pm_typ_contains_elem(context,p,q,&
       mode,einfo,params,base,user,ubase) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: p,q
    integer,intent(in):: mode
    type(pm_typ_einfo),intent(out):: einfo
    integer,dimension(:),intent(inout):: params
    integer,intent(in):: base
    integer,dimension(:),intent(inout):: user
    integer,intent(in):: ubase
    type(pm_ptr):: u
    logical:: ok
    integer:: i,k,uk
    if(pm_test_typ_includes(context,p,q,mode,einfo,&
         params,base,user,ubase)) then
       ok=.true.
       return
    endif
    if(q==0) then
       ok=.false.
       return
    endif
    u=pm_typ_vect(context,q)
    uk=pm_tv_kind(u)
    select case(uk)
    case(pm_typ_is_all)
       do i=1,pm_tv_numargs(u)
          if(pm_typ_contains_elem(context,p,pm_tv_arg(u,i),&
               mode,einfo,params,base,user,ubase)) then
             ok=.true.
             return
          endif
       enddo
       ok=.false.
    case(pm_typ_is_any)
       do i=1,pm_tv_numargs(u)
          if(.not.pm_typ_contains_elem(context,p,pm_tv_arg(u,i),&
               mode,einfo,params,base,user,ubase)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
    case(pm_typ_is_except)
       ok=pm_typ_contains_elem(context,p,pm_tv_arg(u,1),&
            mode,einfo,params,base,user,ubase)
       if(ok) then
          ok=.not.pm_typ_includes(context,pm_tv_arg(u,2),&
               p,pm_typ_incl_typ,einfo)
       endif
    case(pm_typ_is_array)
       if(pm_typ_contains_elem(context,p,pm_tv_arg(u,1),&
            mode,einfo,params,base,user,ubase)) then
          ok=.true.
          return
       elseif(pm_typ_contains_elem(context,p,pm_tv_arg(u,2),&
            mode,einfo,params,base,user,ubase)) then
          ok=.true.
          return
       else
          ok=.false.
       endif
    case(pm_typ_is_struct,pm_typ_is_rec,&
         pm_typ_is_tuple,pm_typ_is_vtuple)
       do i=1,pm_tv_numargs(u)
          if(pm_typ_contains_elem(context,p,pm_tv_arg(u,i),&
               mode,einfo,params,base,user,ubase)) then
             ok=.true.
             return
          endif
       enddo
       ok=.false.
    case(pm_typ_is_dref)
       ok=pm_typ_contains_elem(context,p,pm_tv_arg(u,3),&
            mode,einfo,params,base,user,ubase)
       if(.not.ok) then
          i=pm_tv_arg(u,2)
          k=pm_typ_kind(context,i)
          if(k==pm_typ_is_par_kind) then
             u=pm_typ_vect(context,i)
             i=pm_tv_arg(u,1)
             k=pm_typ_kind(context,i)
          endif
          if(k==pm_typ_is_dref) then
             ok=pm_typ_contains_elem(context,p,i,&
                  mode,einfo,params,base,user,ubase)
          endif
       endif
    case(pm_typ_is_par_kind,pm_typ_is_vect,&
         pm_typ_is_enveloped,pm_typ_is_interface,&
         pm_typ_is_contains,pm_typ_is_has,&
         pm_typ_is_params,pm_typ_is_param)
       ok=pm_typ_contains_elem(context,p,pm_tv_arg(u,1),&
               mode,einfo,params,base,user,ubase)
    case default
       ok=.false.
    end select
  end function pm_typ_contains_elem


  !===============================================
  ! Perform enveloping conversions if possible
  ! Returns -1 if not possible
  !==============================================
  function pm_typ_convert(context,partyp,argtyp,dopoly) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: partyp,argtyp
    logical,intent(in):: dopoly
    integer:: ctyp
    integer:: tk,ptyp
    type(pm_ptr):: tv
    ctyp=-1
    ptyp=partyp
    if(partyp<=0.or.argtyp<=0) then
       return
    endif
    tk=pm_typ_kind(context,ptyp)
    do while(tk==pm_typ_is_user)
       ptyp=pm_user_typ_body(context,ptyp)
       tk=pm_typ_kind(context,ptyp)
    enddo
    if(tk==pm_typ_is_interface) then
       ctyp=pm_interface_typ_convert(context,ptyp,argtyp)
    endif
    if(ctyp<0.and.tk==pm_typ_is_proc) then
       ctyp=pm_proc_typ_convert(context,ptyp,argtyp)
    endif
    if(ctyp<0.and.dopoly.and.tk==pm_typ_is_poly) then
       ctyp=pm_poly_typ_convert(context,ptyp,argtyp)
    endif
  end function pm_typ_convert

  !================================================================
  ! Autoconversion to broader poly type
  ! Returns -1 if not possible
  !================================================================
  function pm_poly_typ_convert(context,partyp,argtyp) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: partyp,argtyp
    integer:: ctyp
    type(pm_ptr):: tv1,tv2
    type(pm_typ_einfo):: einfo
    ctyp=-1
    tv1=pm_typ_vect(context,partyp)
    tv2=pm_typ_vect(context,argtyp)
    if(pm_tv_kind(tv1)==pm_typ_is_poly.and.pm_tv_kind(tv2)==pm_typ_is_poly) then
       if(pm_typ_includes(context,pm_tv_arg(tv1,1),pm_tv_arg(tv2,1),&
            pm_typ_incl_typ,einfo)) then
          ctyp=partyp
       endif
    endif
  end function pm_poly_typ_convert
  
  !================================================================
  ! Autoconversion to interface type, yielding enveloped type
  ! Returns -1 if not possible
  !================================================================
  function pm_interface_typ_convert(context,partyp,argtyp) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: partyp,argtyp
    integer:: ctyp
    integer,dimension(4):: args
    integer:: tno
    type(pm_ptr):: tv
    tv=pm_typ_vect(context,partyp)
    tno=partyp
    if(pm_tv_kind(tv)==pm_typ_is_user) then
       tno=pm_user_typ_body(context,partyp)
       tv=pm_typ_vect(context,tno)
    endif
    if(pm_tv_kind(tv)==pm_typ_is_interface) then
       args(1)=pm_typ_new_enveloped
       args(2)=pm_tv_name(tv)
       args(3)=argtyp
       args(4)=tno
       ctyp=pm_new_typ(context,args)
    else
       ctyp=-1
    endif
  end function pm_interface_typ_convert

  !===============================================================
  ! Check if type q conforms to interface p
  ! - if q is also an interface then add information that q:p
  !===============================================================
  recursive function pm_interface_typ_conforms(context,p,q,einfo,except) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: p,q
    type(pm_typ_einfo),intent(out):: einfo
    integer,intent(in),optional:: except
    logical:: ok
    integer:: s,tk,uk,i,j,k,nt,nu,elem,err,name
    type(pm_ptr):: t,u,r,names1,names2
    integer,dimension(max_user_nesting):: params
    logical:: ok2
    t=pm_typ_vect(context,p)
    u=pm_typ_vect(context,q)
    tk=pm_tv_kind(t)
    uk=pm_tv_kind(u)

    select case(uk)
    case(pm_typ_is_user)
       r=pm_dict_val(context,context%tcache,int(q,pm_ln))
       ok=pm_interface_typ_conforms(context,p,int(r%offset),&
            einfo,except)
    case(pm_typ_is_any)
       do i=1,pm_tv_numargs(u)
          if(present(except)) then
             if(pm_typ_includes(context,except,pm_tv_arg(u,i),&
                  pm_typ_incl_typ,einfo)) cycle
          endif
          if(.not.pm_interface_typ_conforms(context,p,pm_tv_arg(u,i),&
               einfo,except)) then
             ok=.false.
          endif
       enddo
       ok=.true.
    case(pm_typ_is_all)
       do i=1,pm_tv_numargs(u)
          if(pm_interface_typ_conforms(context,p,pm_tv_arg(u,i),&
               einfo,except)) then
             ok=.true.
          endif
       enddo
       ok=.false.
    case(pm_typ_is_except)
       if(present(except)) then
          ok=pm_interface_typ_conforms(context,p,pm_tv_arg(u,i),&
               einfo,pm_typ_combine(context,pm_tv_arg(u,2),except))
       else
          ok=pm_interface_typ_conforms(context,p,pm_tv_arg(u,i),&
               einfo,pm_tv_arg(u,2))
       endif
    case(pm_typ_is_param,pm_typ_is_params,&
         pm_typ_is_enveloped,pm_typ_is_contains,pm_typ_is_has,&
         pm_typ_is_vect)
       ok=pm_interface_typ_conforms(context,p,pm_tv_arg(u,1),&
            einfo,except)
    case(pm_typ_is_interface)
       names1=pm_name_val(context,pm_tv_name(t))
       names2=pm_name_val(context,pm_tv_name(u))
  
       ! Check consistency with any shadowing elements
       outer: do i=1,pm_fast_esize(names1)
          nt=names1%data%i(names1%offset+i)
          do j=1,pm_fast_esize(names2)
             nu=names2%data%i(names2%offset+j)
             if(nt==nu.or.(nt==-nu.and.nt>0)) then
                if(pm_typ_includes(context,&
                     pm_tv_arg(t,i+1),pm_tv_arg(u,j+1),&
                     pm_typ_incl_typ,einfo)) then
                   cycle outer
                else
                   einfo%kind=pm_typ_err_interface_nesting
                   einfo%typ1=p
                   einfo%typ2=q
                   einfo%vtyp1=pm_tv_arg(t,i+1)
                   einfo%vtyp2=pm_tv_arg(u,j+1)
                   einfo%vname=abs(nt)
                   ok=.false.
                   return
                endif
             elseif(nt==-nu.and.nt<0) then
                einfo%kind=pm_typ_err_interface_inconsistent
                einfo%typ1=p
                einfo%typ2=q
                ok=.false.
                return
             endif
          enddo
          einfo%kind=pm_typ_err_interface_elem
          einfo%typ1=p
          einfo%typ2=q
          einfo%vname=abs(nt)
          return
       enddo outer
       ok=.true.
    case(pm_typ_is_rec,pm_typ_is_struct)
       ok=.true.
       names1=pm_name_val(context,pm_tv_name(t))
       einfo%vtyp1=p
       einfo%vtyp2=q
       do i=1,pm_fast_esize(names1)
          nt=names1%data%i(names1%offset+i)
          einfo%vname=abs(nt)
          uk=1
          elem=pm_typ_find_elem(context,q,abs(nt),nt<0,&
               params,uk,size(params),s,einfo)
          if(elem==0) then
             if(einfo%kind/=pm_typ_err_elem_clash) then
                einfo%kind=merge(pm_typ_err_interface_write,&
                     pm_typ_err_interface,nt<0)
             endif
             ok=.false.
             return
          endif
          ok=pm_typ_includes(context,&
               pm_tv_arg(t,i+1),s,&
               pm_typ_incl_typ,einfo)
          if(.not.ok) then
             einfo%kind=pm_typ_err_interface_mismatch
             einfo%typ1=pm_tv_arg(t,i+1)
             einfo%typ2=s
             return
          endif
       enddo
   case default
       einfo%kind=pm_typ_err_interface_bad_typ
       einfo%vtyp1=q
       ok=.false.
    end select
  contains
    include 'fesize.inc'
    include 'ftypeof.inc'
    include 'ftypeno.inc'
    include 'ftiny.inc'
  end function pm_interface_typ_conforms

  !==========================================
  ! Autoconversion to proc signature type
  ! Returns -1 if not possible
  !==========================================
  function pm_proc_typ_convert(context,ptyp,argtyp) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: ptyp,argtyp
    integer:: ctyp
    type(pm_ptr):: tv,tv2
    integer:: i,tno
    integer,dimension(3):: arr
    tv=pm_typ_vect(context,ptyp)
    tv2=pm_typ_vect(context,argtyp)
    if(pm_tv_kind(tv)/=pm_typ_is_proc.or.&
         pm_tv_kind(tv2)/=pm_typ_is_proc) then
       ctyp=-1
       return
    endif
    tno=pm_tv_arg(tv,1)
    do i=1,pm_tv_numargs(tv2)
       if(pm_proc_typ_conforms(context,tno,&
            pm_tv_arg(tv2,i))) then
          arr(1)=pm_typ_new_proc
          arr(2)=-abs(pm_tv_name(tv2))
          arr(3)=tno
          ctyp=pm_new_typ(context,arr)
          return
       endif
    enddo
    ctyp=-1
    return
  end function pm_proc_typ_convert

  !===========================================
  ! Check that two proc_sig types conform
  !===========================================
  function pm_proc_typ_conforms(context,tno,tno2) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,tno2
    logical:: ok
    type(pm_ptr):: tv,tv2,tv_res,tv_res2
    type(pm_typ_einfo):: einfo
    integer:: i,tno_res,tno_res2
    
    tv=pm_typ_vect(context,tno)
    tv2=pm_typ_vect(context,tno2)
    
    
    if(pm_tv_name(tv)/=pm_tv_name(tv2)) then
       ok=.false.
       return
    endif

    if(.not.pm_typ_includes(context,pm_tv_arg(tv2,1),&
         pm_tv_arg(tv,1),pm_typ_incl_typ,einfo)) then
       ok=.false.
       return
    endif

    tno_res=pm_tv_arg(tv,2)
    tno_res2=pm_tv_arg(tv2,2)
    tv_res=pm_typ_vect(context,tno_res)
    tv_res2=pm_typ_vect(context,tno_res2)
    if(pm_tv_kind(tv_res2)==pm_typ_is_undef_result) then
       ok=pm_tv_numargs(tv_res)==pm_tv_name(tv_res2)
       return
    else
       if(.not.pm_typ_includes(context,tno_res,&
            tno_res2,pm_typ_incl_equiv,einfo)) then
          ok=.false.
          return
       endif
    endif
    ok=.true.
  end function pm_proc_typ_conforms

  !=================================================================
  ! Find element "name" in type "tno"
  ! If change is true then element must be able to be modified
  ! Returns
  !      offset==0   Error -- details in einfo
  !      offset>0    This is the offset of the element in the type
  !      offset<0    Nested offsets detailed in stack(old_top:top)
  ! If offset/=0 then etype returns the type of the element
  !=================================================================
  recursive function pm_typ_find_elem(context,tno,name,change,&
       stack,top,maxstack,etype,einfo) result(offset)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,name
    logical,intent(in):: change
    integer,dimension(:),intent(inout):: stack
    integer,intent(inout):: top
    integer,intent(in):: maxstack
    integer,intent(out):: etype
    type(pm_typ_einfo),intent(out):: einfo
    integer:: offset,ptype,mode
    type(pm_ptr):: tv,tv2,nameset,info
    integer:: tk,i,key(2),tno2,name2
    integer(pm_ln):: j
    logical:: found
    if(tno<0) then
       offset=0
       return
    endif
    if(tno==0) then
       offset=0
       einfo%kind=pm_typ_err_elem_bad_typ
    endif
    einfo%kind=0
    einfo%typ1=tno
    einfo%name=name
    tv=pm_typ_vect(context,tno)
    tk=pm_tv_kind(tv)
    select case(tk)
    case(pm_typ_is_all)
       do i=1,pm_tv_numargs(tv)
          offset=pm_typ_find_elem(context,pm_tv_arg(tv,i),name,change,&
               stack,top,maxstack,etype,einfo)
          if(offset/=0) return
       enddo
       offset=0
       einfo%kind=pm_typ_err_elem_not_found
       return
    case(pm_typ_is_dref)
       offset=pm_typ_find_elem(context,&
            pm_typ_strip_mode(context,pm_tv_arg(tv,1),mode),&
            name,change,stack,top,&
            maxstack,etype,einfo)
       if(offset==0) then
          return
       else
          offset=offset+pm_typ_dref_offset
       endif
       call push(pm_typ_new_dref)
       call push(name)
       call push(pm_typ_add_mode(context,etype,mode,.false.))
       call push(tno)
       do i=3,pm_tv_numargs(tv)
          call push(pm_tv_arg(tv,i))
       enddo
       etype=pm_new_typ(context,stack(top-pm_tv_numargs(tv)-1:top))
       top=top-pm_tv_numargs(tv)-2
    case(pm_typ_is_struct,pm_typ_is_rec)
       if(change.and.tk==pm_typ_is_rec) then
          einfo%kind=pm_typ_err_elem_not_found
          offset=0
          return
       endif
       call elem_offset(context,tv,name,change,offset,etype)
       if(offset>0) return
       if(offset<0) then
          call indirect_offset(context,tv,name,stack,&
               maxstack,top,change,etype,ptype,einfo)
          if(einfo%kind==0) then
             offset=-top
             return
          else
             offset=0
             return
          endif
       else
          einfo%kind=pm_typ_err_elem_not_found
          offset=0
       endif
    case(pm_typ_is_enveloped)
       tno2=pm_tv_arg(tv,2)
       tv2=pm_typ_vect(context,tno2)
       nameset=pm_name_val(context,pm_tv_name(tv2))
       found=.false.
       do i=1,pm_fast_esize(nameset)
          name2=nameset%data%i(nameset%offset+i)
          if(name==abs(name2)) then
             found=.true.
             exit
          endif
       enddo
       if(.not.found) then
          einfo%kind=pm_typ_err_elem_not_in_interface
          einfo%typ1=tno2
          offset=0
          return
       endif
       offset=pm_typ_find_elem(context,pm_tv_arg(tv,1),&
            name,change,stack,top,maxstack,etype,einfo)
    case default
       einfo%kind=pm_typ_err_elem_bad_typ
       offset=0
       return
    end select
  contains
    include 'ftypeof.inc'
    include 'fesize.inc'
    subroutine push(j)
      integer:: j
      top=top+1
      stack(top)=j
    end subroutine push
  end function pm_typ_find_elem

  ! Find offset and type for named element in struct/rec type
  ! Returns offset and type of element
  ! If no such element offset=0 (or offset=-1 if embedded structs exist)
  recursive subroutine elem_offset(context,tv,name,change,offset,etyp)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: tv
    integer,intent(in):: name
    logical,intent(in):: change
    integer,intent(out):: offset,etyp
    integer:: j,hi,lo
    integer:: name2
    type(pm_ptr):: nv
    offset=0
    etyp=0
    nv=pm_name_val(context,pm_tv_name(tv))
    do j=1,pm_fast_esize(nv)
       if(abs(nv%data%i(nv%offset+j))==name) then
          etyp=pm_tv_arg(tv,j)
          offset=j+1
          return
       endif
    enddo
    if(iand(pm_tv_flags(tv),pm_typ_has_embedded)/=0) offset=-1
  contains
    include 'fesize.inc'
  end subroutine elem_offset

  ! Find offsets to an embedded element "name" of tno
  recursive subroutine indirect_offset(context,tv,name,stack,&
       maxstack,top,change,etype,ptype,einfo)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: tv
    integer,intent(in):: name
    integer,intent(inout):: top
    integer,intent(in):: maxstack
    integer,dimension(maxstack),intent(inout):: stack
    logical,intent(in):: change
    integer,intent(out):: etype,ptype
    type(pm_typ_einfo),intent(out):: einfo
    type(pm_ptr):: nv,tv2
    integer:: i
    integer:: n,offset,new_etype,new_ptype,found_below,tk2
    integer:: name2
    logical:: found,clash_below
    name2=pm_tv_name(tv)
    nv=pm_name_val(context,name2)
    found=.false.
    found_below=0
    clash_below=.false.
    do i=1,pm_fast_esize(nv)
       n=nv%data%i(nv%offset+i)
       if(n<0) then
          new_ptype=pm_tv_arg(tv,i)
          tv2=pm_typ_vect(context,new_ptype)
          tk2=pm_tv_kind(tv2)
          if(tk2/=pm_typ_is_struct.and.tk2/=pm_typ_is_rec) cycle
          call elem_offset(context,tv2,name,change,offset,new_etype)
          if(offset>0) then
             if(top+4>maxstack) then
                call pm_panic('Structure embedding too complex')
             endif
             if(found) then
                einfo%kind=pm_typ_err_elem_clash
                einfo%vtyp1=new_ptype
                einfo%vtyp2=ptype
                return
             endif
             call push(i+1)
             call push(-n)
             call push(offset)
             call push(name)
             etype=new_etype
             ptype=new_ptype
             found=.true.
          elseif(offset<0.and..not.found) then
             if(top+2>maxstack) then
                call pm_panic('Internal limit reached - Structure embedding too complex')
             endif
             call push(i+1)
             call push(-n)
             call indirect_offset(context,tv2,name,stack,&
                  maxstack,top,change,new_etype,new_ptype,einfo)
             if(einfo%kind==0) then
                if(found_below>0) then
                   einfo%kind=pm_typ_err_elem_clash
                   einfo%vtyp1=new_ptype
                   einfo%vtyp2=ptype
                else
                   etype=new_etype
                   ptype=new_ptype
                endif
                found_below=found_below+1
             elseif(einfo%kind==pm_typ_err_elem_clash) then
                clash_below=.true.
                top=top-2
                return
             else ! Not found
                top=top-2
             endif
          endif
       endif
    enddo
    if(clash_below.and..not.found) then
       return
    elseif(found_below>1.and.found) then
       einfo%kind=0
    elseif(.not.found.and.found_below==0) then
       einfo%kind=pm_typ_err_elem_not_found
    endif
  contains
    include 'fesize.inc'
    subroutine push(j)
      integer,intent(in):: j
      top=top+1
      stack(top)=j
    end subroutine push
  end subroutine indirect_offset

  ! Check that tno includes an embedded (use..) element of tno2
  recursive subroutine pm_indirect_include(context,tno,tno2,stack,&
       maxstack,top,einfo,tno_match,status)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,tno2
    integer,intent(inout):: top
    integer,intent(in):: maxstack
    integer,dimension(maxstack),intent(inout):: stack
    type(pm_typ_einfo):: einfo
    integer,intent(out):: tno_match
    integer,intent(out):: status

    integer:: base,base2,tno_match1
    base=top
    ! Check for match via 'use' element
    if(test_indirect_include(context,+1,tno,tno2,stack,&
         maxstack,base,top,einfo,tno_match)) then
       base2=top
       tno_match1=tno_match
       ! Check for match in reverse order
       if(test_indirect_include(context,-1,tno,tno2,stack,&
            maxstack,base2,top,einfo,tno_match)) then
          if(any(stack(base+1:base2)/=stack(base2+1:top))) then
             ! Clashing elements - push error details onto stack
             status=pm_elem_clash
             if(top+3>maxstack) call pm_panic('Program too complex (indirect include)')
             stack(top+1)=base
             stack(top+2)=base2
             stack(top+3)=tno_match1
             top=top+3
          else
             top=base2
             status=pm_elem_found
          endif
       else
          call pm_panic('indirect_include')
       endif
    else
       status=pm_elem_not_found
    endif
  end subroutine pm_indirect_include

  ! Does the actual work for pm_indirect_include
  recursive function test_indirect_include(context,dir,tno,tno2,stack,&
       maxstack,base,top,einfo,tno_match) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,tno2
    integer,intent(inout):: top
    integer,intent(in):: dir,maxstack,base
    integer,dimension(maxstack),intent(inout):: stack
    type(pm_typ_einfo):: einfo
    integer,intent(out):: tno_match
    logical:: ok
    type(pm_ptr):: tv,name
    integer:: i,j,start,finish,tk
    integer:: n
    if(tno2==0) return
    tv=pm_typ_vect(context,tno2)
    tk=pm_tv_kind(tv)
    select case(tk)
    case(pm_typ_is_par_kind)
       ok=test_indirect_include(context,dir,tno,pm_tv_arg(tv,1),stack,&
            maxstack,base,top,einfo,tno_match)
       return
    case(pm_typ_is_any)
       do i=1,pm_tv_numargs(tv)
          if(.not.test_indirect_include(context,dir,tno,pm_tv_arg(tv,i),stack,&
               maxstack,base,top,einfo,tno_match)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
       return
    case(pm_typ_is_all)
       do i=1,pm_tv_numargs(tv)
          if(test_indirect_include(context,dir,tno,pm_tv_arg(tv,i),stack,&
               maxstack,base,top,einfo,tno_match)) then
             ok=.true.
             return
          endif
       enddo
       ok=.false.
       return
    case(pm_typ_is_except)
       if(test_indirect_include(context,dir,tno,pm_tv_arg(tv,1),stack,&
            maxstack,base,top,einfo,tno_match)) then
          ok=.not.test_indirect_include(context,dir,tno,pm_tv_arg(tv,2),stack,&
               maxstack,base,top,einfo,tno_match)
       else
          ok=.false.
       endif
       return
    case(pm_typ_is_struct,pm_typ_is_rec)
       continue
    case default
       ok=.false.
       return
    end select
    
    ok=.false.
    if(iand(pm_tv_flags(tv),pm_typ_has_embedded)/=0) then
       if(top+2>maxstack) call pm_panic('Program too complex (nested embeds)')
       top=top+2
       if(dir>0) then
          start=1
          finish=pm_tv_numargs(tv)
       else
          start=pm_tv_numargs(tv)
          finish=1
       endif
       name=pm_name_val(context,pm_tv_name(tv))
       outer: do i=start,finish,dir
          n=name%data%i(name%offset+i)
          stack(top-1)=i+1
          stack(top)=n
          if(n<0) then
             if(pm_typ_includes(context,tno,pm_tv_arg(tv,i),&
                  pm_typ_incl_indirect,einfo)) then
                ! Check that this element is not shadowed by name
                do j=base+2,top-2,2
                   if(stack(j)==n)  cycle outer
                enddo
                tno_match=pm_tv_arg(tv,i)
                ok=.true.
                return
             endif
          endif
       enddo outer
       do i=start,finish,dir
          n=name%data%i(name%offset+i)
          stack(top-1)=i+1
          stack(top)=n
          if(n<0) then
             if(test_indirect_include(context,dir,tno,pm_tv_arg(tv,i),stack,&
                  maxstack,base,top,einfo,tno_match)) then
                ok=.true.
                return
             endif
          endif
       enddo
       top=top-2
    endif
  end function test_indirect_include

  ! Concrete only version of a type (used/usable only for returns from builtin functions)
  recursive function pm_typ_as_concrete(context,tno,params,isstatic,iserr) result(tno2)
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
    tv=pm_typ_vect(context,tno)
    tk=pm_tv_kind(tv)
    select case(tk)
    case(pm_typ_is_basic,pm_typ_is_single_name,&
         pm_typ_is_proc,pm_typ_is_value,pm_typ_is_const,&
         pm_typ_is_undef_result,pm_typ_is_poly)
       tno2=tno
    case(pm_typ_is_user)
       tno2=pm_user_typ_body(context,tno)
    case(pm_typ_is_any,pm_typ_is_all,pm_typ_is_contains)
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
    subroutine remake(n)
      integer,intent(in):: n
      integer,dimension(n+2):: a
      integer:: i
      a(1)=tk
      a(2)=pm_tv_name(tv)
      if(present(iserr)) then
         do i=1,n
            a(i+2)=pm_typ_as_concrete(context,pm_tv_arg(tv,i),params,isstatic,iserr)
            if(iserr) return
         enddo
      else
         do i=1,n
            a(i+2)=pm_typ_as_concrete(context,pm_tv_arg(tv,i),params,isstatic)
         enddo
      endif
      tno2=pm_new_typ(context,a)
    end subroutine remake
  end function pm_typ_as_concrete

  recursive function pm_typ_remove_params(context,tno,params) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer,dimension(:),intent(in):: params
    integer:: tno2
    type(pm_ptr):: tv
    integer:: tk,argnum
    tv=pm_typ_vect(context,tno)
    tk=pm_tv_kind(tv)
    if(tk==pm_typ_is_user) then
       tno2=pm_user_typ_body(context,tno)
    elseif(tk==pm_typ_is_param) then
       argnum=pm_tv_name(tv)
       if(params(argnum)>=0) then
          tno2=params(argnum)
       else
          tno2=pm_tv_arg(tv,1)
       endif
    elseif(iand(pm_tv_flags(tv),pm_typ_has_params)/=0) then
       call remake(pm_tv_numargs(tv))
    else
       tno2=tno
    endif
  contains
    subroutine remake(n)
      integer,intent(in):: n
      integer,dimension(n+2):: a
      integer:: i
      a(1)=tk
      a(2)=pm_tv_name(tv)
      do i=1,n
         a(i+2)=pm_typ_remove_params(context,pm_tv_arg(tv,i),params)
      enddo
      tno2=pm_new_typ(context,a)
    end subroutine remake
  end function pm_typ_remove_params
  

   ! Get vector of integer representation of type
  function pm_typ_vect(context,tno) result(typ)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    type(pm_ptr):: typ,dict
    integer(pm_ln):: t
    dict=context%heap%tcache
    t=tno
    if(pm_debug_level>0) then
       if(t<1.or.t>pm_dict_size(context,dict)) then
          write(*,*) 'tno=',t,tno,pm_dict_size(context,dict)
          call pm_panic('pm_typ_vect')
       endif
    endif
    typ=pm_dict_key(context,dict,t)
  end function pm_typ_vect

  ! Type kind from integer type vector
  function pm_tv_kind(typ) result(k)
    type(pm_ptr),intent(in):: typ
    integer:: k
    k=iand(typ%data%i(typ%offset),pm_typ_kind_mask)
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
  function pm_typ_as_string(context,tno,distr) result(str)
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
       call typ_to_str(context,tno,str,n,tuple=.false.,distr=distr)
    endif
  end function  pm_typ_as_string

  recursive subroutine typ_to_str(context,typno,str,n,distr,tuple,noequiv)
    type(pm_context),pointer:: context
    integer,intent(in):: typno
    character(len=256),intent(inout):: str
    integer,intent(inout):: n
    logical,intent(in),optional:: distr,tuple,noequiv
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
    tv=pm_typ_vect(context,tno)
    tk=pm_tv_kind(tv)
    nv=pm_dict_val(context,context%tcache,int(tno,pm_ln))
    narg=pm_tv_numargs(tv)
    select case(tk)
    case(pm_typ_is_user,pm_typ_is_basic)
       name=pm_tv_name(tv)
       if(name<0) then
          call typ_to_str(context,pm_tv_arg(tv,1),str,n)
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
             call typ_to_str(context,pm_tv_arg(tv,1),str,n)
             if(add_char(')')) return
          else
             if(add_char('[')) return
             do i=1,narg-1
                call typ_to_str(context,pm_tv_arg(tv,i),str,n)
                if(add_char(',')) return
             enddo
             call typ_to_str(context,pm_tv_arg(tv,narg),str,n)
             if(add_char(']')) return
          endif
       elseif(name==sym_pm_ref_type) then
          call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       else
          call pm_name_string(context,name,str(n:))
          n=len_trim(str)+1
          if(n>len(str)-10) return
          narg=pm_tv_numargs(tv)
          if(narg>0) then
             if(add_char('(')) return
             do i=1,narg-1
                call typ_to_str(context,pm_tv_arg(tv,i),str,n)
                if(add_char(',')) return
             enddo
             call typ_to_str(context,pm_tv_arg(tv,narg),str,n)
             if(add_char(')')) return
          endif
          if(tk==pm_typ_is_user.and.(pm_opts%show_members)) then
             nv2=pm_dict_val(context,context%tcache,int(tno,pm_ln))
             tno2=int(nv2%offset)
             if(tno2>0.and.tno2<pm_dict_size(context,context%heap%tcache)) then
                tv=pm_typ_vect(context,tno2)
                if(pm_tv_kind(tv)/=pm_typ_is_basic) then
                   if(add_char(' {')) return
                   call typ_to_str(context,tno2,str,n)
                   if(add_char('}')) return
                endif
             else
                if(add_char('{???}')) return
             endif
          endif
       endif
    case(pm_typ_is_tuple,pm_typ_is_vtuple)
       if(add_char('(')) return
       narg=pm_tv_numargs(tv)
       if(narg==0) then
          if(add_char(')')) return
          return
       endif
       if(pm_tv_name(tv)/=0) then
          amps=pm_name_val(context,pm_tv_name(tv))
          j=0
          do i=1,narg-1
             if(amps%data%i(amps%offset+j)==i) then
                j=j+1
                if(add_char('&')) return
             endif
             call typ_to_str(context,pm_tv_arg(tv,i),str,n)
             if(add_char(',')) return
          enddo
          if(amps%data%i(amps%offset+j)==n) then
             if(add_char('&')) return
          endif
          call typ_to_str(context,pm_tv_arg(tv,narg),str,n)
       else
          do i=1,narg-1
             call typ_to_str(context,pm_tv_arg(tv,i),str,n)
             if(add_char(',')) return
          enddo
          call typ_to_str(context,pm_tv_arg(tv,narg),str,n)
       endif
       if(tk==pm_typ_is_vtuple) then
          if(add_char('...')) return
       endif
       if(add_char(')')) return
    case(pm_typ_is_struct,pm_typ_is_rec)
       nv=pm_name_val(context,pm_tv_name(tv))
       name=nv%data%i(nv%offset)
       tno2=pm_typ_from_recorded_name(context,int(name))
       if(tno2>0.and..not.present(noequiv)) then
          if(show_equiv(int(name),tno2,tno)) return
       endif
       if(tk==pm_typ_is_struct) then
          if(add_char('struct ')) return
       else
          if(add_char('rec ')) return
       endif
       call pm_name_string(context,name,str(n:))
       n=len_trim(str)+1
       if(n>len(str)-10) return
       if(add_char(open_brace)) return
       narg=pm_tv_numargs(tv)
       do i=1,narg
          name=nv%data%i(nv%offset+i)
          if(name<0) then
             if(add_char('use ')) return
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
          call typ_to_str(context,pm_tv_arg(tv,i),str,n)
          if(i<narg) then
             if(add_char(',')) return
          endif
       enddo
       if(add_char(close_brace)) return
    case(pm_typ_is_interface)
       name=pm_tv_name(tv)
       nv2=pm_name_val(context,name)
       call pm_name_string(context,nv2%data%i(nv2%offset),str(n:))
       n=len_trim(str)+1
    case(pm_typ_is_enveloped)
       call typ_to_str(context,pm_tv_arg(tv,2),str,n)
       if(add_char('{')) return
       call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       if(add_char('}')) return
    case(pm_typ_is_single_name)
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
       if(iand(pm_tv_flags(tv),pm_typ_has_distributed)/=0) then
          if(add_char('*distr*')) return
       endif
    case(pm_typ_is_dref)
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
             call typ_to_str(context,pm_tv_arg(tv,i),str,n)
             if(add_char(',')) return
          enddo
          call typ_to_str(context,pm_tv_arg(tv,pm_tv_numargs(tv)),str,n)
          if(add_char(')')) return
       else
          call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       endif
    case(pm_typ_is_array)
       name=pm_tv_name(tv)
       if(name==sym_var) then
          if(add_char('varray(')) return
       elseif(name==sym_const) then
          if(add_char('farray(')) return
       else
          if(add_char('array(')) return
       endif
       call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       if(add_char(',')) return
       call typ_to_str(context,pm_tv_arg(tv,2),str,n)
       if(add_char(')')) return
    case(pm_typ_is_poly)
       if(add_char('*')) return
       call bracket(1,pm_typ_is_includes,pm_typ_is_all,pm_typ_is_any,pm_typ_is_except)
    case(pm_typ_is_value)
       if(add_char('''')) return
       if(pm_tv_name(tv)==0) then
          call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       else
          nv=pm_dict_val(context,context%tcache,int(tno,pm_ln))
          if(pm_fast_vkind(nv)==pm_logical) then
             if(nv%data%l(nv%offset)) then
                if(add_char('true')) return
             else
                if(add_char('false')) return
             endif
          else
             str(n:)=pm_number_as_string(context,nv,0_pm_ln)
          endif
          n=len_trim(str)+1
       endif
    case(pm_typ_is_const)
       if(add_char('fix(')) return
       call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       if(add_char(')')) return
    case(pm_typ_is_except)
       call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       if(add_char(' except ')) return
       call typ_to_str(context,pm_tv_arg(tv,2),str,n)
    case(pm_typ_is_any)
       call bracket(1,pm_typ_is_except,pm_typ_is_except,pm_typ_is_except,pm_typ_is_except)
       do i=2,pm_tv_numargs(tv)
          if(add_char(' or ')) return
          call bracket(i,pm_typ_is_except,pm_typ_is_except,pm_typ_is_except,pm_typ_is_except)
       enddo
    case(pm_typ_is_all)
       call bracket(1,pm_typ_is_any,pm_typ_is_except,pm_typ_is_except,pm_typ_is_except)
       do i=2,pm_tv_numargs(tv)
          if(add_char(' and ')) return
          call bracket(i,pm_typ_is_any,pm_typ_is_except,pm_typ_is_except,pm_typ_is_except)
       enddo
    case(pm_typ_is_includes)
       call bracket(1,pm_typ_is_any,pm_typ_is_all,pm_typ_is_except,pm_typ_is_except)
       if(add_char(' inc ')) return
       call bracket(2,pm_typ_is_any,pm_typ_is_all,pm_typ_is_except,pm_typ_is_except)
    case(pm_typ_is_contains)
       if(add_char('contains(')) return
       call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       if(add_char(')')) return
    case(pm_typ_is_has)
       if(add_char('.')) return
       call bracket(1,pm_typ_is_includes,pm_typ_is_all,pm_typ_is_any,pm_typ_is_except)
       call typ_to_str(context,pm_tv_arg(tv,1),str,n)
    case(pm_typ_is_proc)
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
                call typ_to_str(context,pm_tv_arg(tv,i),str,n)
                if(add_char(',')) return
             enddo
             call typ_to_str(context,pm_tv_arg(tv,pm_tv_numargs(tv)),str,n)
             if(add_char('}')) return
          endif
       elseif(name==0) then
          if(add_char('proc')) return
          call typ_to_str(context,pm_tv_arg(tv,1),str,n)
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
          call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       endif
    case(pm_typ_is_proc_sig)
       name=pm_tv_name(tv)
       if(name/=sym_proc) then
          if(add_char(trim(pm_name_as_string(context,name)))) return
       endif
       call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       if(add_char('->')) return
       call typ_to_str(context,pm_tv_arg(tv,2),str,n)
    case(pm_typ_is_undef_result)
       name=pm_tv_name(tv)
       if(add_char('(')) return
       do i=1,name-1
          if(add_char('_,')) return
       enddo
       if(add_char('_')) return
       if(add_char(')')) return
    case(pm_typ_is_amp)
       if(add_char('&')) return
       call typ_to_str(context,pm_tv_arg(tv,1),str,n)
    case(pm_typ_is_vect)
       if(add_char('^^(')) return
       call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       if(add_char(')')) return
    case(pm_typ_is_par_kind)
       name=pm_tv_name(tv)
       if(add_char(trim(sym_names(name)))) return
       if(add_char(' ')) return
       call typ_to_str(context,pm_tv_arg(tv,1),str,n)
    case(pm_typ_is_param,pm_typ_is_params)
       if(add_char('$')) return
       call typ_to_str(context,pm_tv_arg(tv,1),str,n,noequiv=.true.)
    case(pm_typ_is_type)
       if(add_char('<')) return
       call typ_to_str(context,pm_tv_arg(tv,1),str,n)
       if(add_char('>')) return
    case(pm_typ_is_bottom)
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
      tk=pm_typ_kind(context,pm_tv_arg(tv,i))
      if(tk==tk1.or.tk==tk2.or.tk==tk3.or.tk==tk4) then
         if(add_char('(')) return
         call typ_to_str(context,pm_tv_arg(tv,i),str,n)
         if(add_char(')')) return
      else
         call typ_to_str(context,pm_tv_arg(tv,i),str,n)
      endif
    end subroutine bracket

    function show_equiv(name,templ,typ) result(ok)
      integer,intent(in):: name,templ,typ
      logical:: ok
      integer,dimension(pm_max_typ_args):: params
      integer:: i,m,name2
      logical:: tuple
      params=-1
      ok=pm_typ_extract_params(context,templ,typ,params)
      if(ok) then
         m=0
         do i=1,pm_max_typ_args
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
                  call typ_to_str(context,params(i),str,n)
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

  end subroutine typ_to_str

  subroutine dump_type(context,iunit,tno)
    type(pm_context),pointer:: context
    integer,intent(in):: iunit,tno
    type(pm_ptr):: tv
    integer:: i
    tv=pm_typ_vect(context,tno)
    write(iunit,*) pm_tv_kind(tv),pm_tv_flags(tv),pm_tv_name(tv),&
         '#',(pm_tv_arg(tv,i),i=1,pm_tv_numargs(tv)),'#',&
         iand(pm_tv_flags(tv),pm_typ_has_generic)/=0,&
         iand(pm_tv_flags(tv),pm_typ_has_distributed)
  end subroutine dump_type

  ! Print out type error encoded in einfo
  subroutine pm_typ_error(context,einfo)
    type(pm_context),pointer:: context
    type(pm_typ_einfo):: einfo
    type(pm_ptr):: val
    
    if(.not.pm_main_process) return

    select case(einfo%kind)
    case(pm_typ_err_elem)
       val=pm_name_val(context,einfo%name)
       call more_error(context,'Element: '//&
            trim(pm_name_as_string(context,val%data%i(val%offset+einfo%index))))
       call more_error(context,' expected: '//&
            trim(pm_typ_as_string(context,einfo%typ1)))
       if(einfo%typ2==pm_tiny_int) then
          call more_error(context,'got: undefined value')
       else
          call more_error(context,' got:      '//&
               trim(pm_typ_as_string(context,einfo%typ2)))
       endif
    case(pm_typ_err_interface)
       call more_error(context,&
            'Type: "'//trim(pm_typ_as_string(context,einfo%vtyp2))//&
            '" does not have element "'//&
            trim(pm_name_as_string(context,einfo%vname))//&
            '" required by interface "'//&
            trim(pm_typ_as_string(context,einfo%vtyp2)))
    case(pm_typ_err_interface_elem)
       call more_error(context,&
            'Interface: "'//trim(pm_typ_as_string(context,einfo%typ2))//&
            '" does not define element "'//&
            trim(pm_name_as_string(context,einfo%vname))//&
            '" required by interface "'//&
            trim(pm_typ_as_string(context,einfo%typ1))//'"')
    case(pm_typ_err_interface_write)
       call more_error(context,&
            'Type: "'//trim(pm_typ_as_string(context,einfo%vtyp1))//&
            '" does not have writable element "'//&
            trim(pm_name_as_string(context,einfo%vname))//&
            '" required by interface')
    case(pm_typ_err_interface_clash)
       call more_error(context,&
            'Type: "'//trim(pm_typ_as_string(context,einfo%vtyp1))//&
            '" has ambigous definition for element "'//&
            trim(pm_name_as_string(context,einfo%vname))//&
            '" required by interface ')
    case(pm_typ_err_interface_mismatch)
       call more_error(context,&
            'Element "'//&
            trim(pm_name_as_string(context,einfo%vname))//&
            '" of type "'//&
            trim(pm_typ_as_string(context,einfo%vtyp2))//&
            '" has type that does not conform to interface '//&
            trim(pm_typ_as_string(context,einfo%vtyp1)))
       call more_error(context,'Expected: '//&
            trim(pm_typ_as_string(context,einfo%typ1)))
       call more_error(context,'Got:      '//&
            trim(pm_typ_as_string(context,einfo%typ2)))
    case(pm_typ_err_interface_bad_typ)
       call more_error(context,&
            'Type "'//trim(pm_typ_as_string(context,einfo%vtyp1))//&
            '" cannot correspond to interface ')
    case(pm_typ_err_interface_nesting)
       call more_error(context,&
            'Element "'//trim(pm_name_as_string(context,&
            einfo%vname))//'" of interface "'//&
            trim(pm_typ_as_string(context,einfo%typ2))//&
            '" does conform to element in interface "'//&
            trim(pm_typ_as_string(context,einfo%typ1))//'"')
       call more_error(context,'Expected: '//&
            trim(pm_typ_as_string(context,einfo%vtyp1)))
       call more_error(context,'Got:      '//&
            trim(pm_typ_as_string(context,einfo%vtyp2)))
    case(pm_typ_err_interface_inconsistent)
       call more_error(context,&
            'Element "'//trim(pm_name_as_string(context,&
            einfo%vname))//&
            '" does not have consistent "var" attributes between "'//&
            trim(pm_typ_as_string(context,einfo%typ1))//&
            '" and "'//&
            trim(pm_typ_as_string(context,einfo%typ2))//'"')
    case(pm_typ_err_elem_not_found)
       call more_error(context,&
            'Cannot find an element named: '//&
            trim(pm_name_as_string(context,einfo%name)))
       call more_error(context,&
            'in type: '//&
            trim(pm_typ_as_string(context,einfo%typ1)))
    case(pm_typ_err_elem_not_in_interface)
              call more_error(context,&
            'Cannot find an element named: '//&
            trim(pm_name_as_string(context,einfo%name)))
       call more_error(context,&
            'in interface: '//&
            trim(pm_typ_as_string(context,einfo%typ1)))
       call more_error(context,'or any of its parent interfaces')
    case(pm_typ_err_elem_bad_typ)
       call more_error(context,&
            'Type: '//&
            trim(pm_typ_as_string(context,einfo%typ1)))
       call more_error(context,&
            'does not have any elements -- cannot apply "." operator')
    case(pm_typ_err_elem_clash)
       call more_error(context,&
            'Type: '//trim(pm_typ_as_string(context,einfo%typ1)))
       call more_error(context,&
            '    embeds: '//&
            trim(pm_typ_as_string(context,einfo%vtyp1))//'.'//&
            trim(pm_name_as_string(context,einfo%name)))
       call more_error(context,&
            '    also embeds: '//&
            trim(pm_typ_as_string(context,&
            einfo%vtyp2))//'.'//&
            trim(pm_name_as_string(context,einfo%name)))
    case default
       call more_error(context,   'Expected: '//&
            trim(pm_typ_as_string(context,einfo%typ1)))
       if(einfo%typ2==pm_tiny_int.or.einfo%typ2<0) then
          call more_error(context,'Got: undefined value')
       else
          call more_error(context,'Got:      '//&
               trim(pm_typ_as_string(context,einfo%typ2)))
       endif
    end select
  end subroutine pm_typ_error

  ! Error message for ambiguous match
  ! (assumes wstack holds results from pm_indirect_include)
  subroutine typ_ambiguous_match_error(context,pt,at,at2,wstack,wtop)
    type(pm_context),pointer:: context
    integer,intent(in):: pt,at,at2,wtop
    integer,intent(in),dimension(:):: wstack
    call more_error(context,'    expecting: '//&
         trim(pm_typ_as_string(context,pt)))
    call more_error(context,'          got: '//&
         trim(pm_typ_as_string(context,&
         pm_user_typ_body(context,at))))
    call more_error(context,'  first match: '//&
         trim(pm_name_vect_as_string(context,&
         wstack(wstack(wtop-1)+2:wtop-3),2)))
    call more_error(context,'      of type: '//&
         trim(pm_typ_as_string(context,at2)))
    call more_error(context,' second match: '//&
         trim(pm_name_vect_as_string(context,&
         wstack(wstack(wtop-2)+2:&
         wstack(wtop-1)),2)))
    call more_error(context,'      of type: '//&
         trim(pm_typ_as_string(context,wstack(wtop))))
  end subroutine typ_ambiguous_match_error
  
end module pm_types
