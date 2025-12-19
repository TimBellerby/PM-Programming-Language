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
  integer,parameter:: pm_type_has_storage=     2**5
  integer,parameter:: pm_type_has_distributed= 2**6
  integer,parameter:: pm_type_has_array=       2**7
  integer,parameter:: pm_type_has_poly=        2**8
  integer,parameter:: pm_type_has_generic=     2**9
  integer,parameter:: pm_type_has_vect=        2**10
  integer,parameter:: pm_type_has_fix=         2**11
  integer,parameter:: pm_type_has_literal=     2**12
  integer,parameter:: pm_type_has_params=      2**13
  integer,parameter:: pm_type_is_recursive=    2**14
  integer,parameter:: pm_type_is_soa=          2**15
  integer,parameter:: pm_type_is_aos=          2**16
  integer,parameter:: pm_type_is_seq=          2**17
  integer,parameter:: pm_type_leaves=          2**19

  integer,parameter:: pm_type_is_when=  2**14
  integer,parameter:: pm_type_is_yield= 2**15
  integer,parameter:: pm_type_is_cond=  2**16
  integer,parameter:: pm_type_is_uncond=2**17
  integer,parameter:: pm_type_is_list=  2**18

  ! Bitwise-or of flags which are not taints (only one so far)
  integer,parameter:: pm_type_flags_untainting = &
       ior( pm_type_is_list + pm_type_is_when + pm_type_is_yield + &
       pm_type_is_cond + pm_type_is_uncond, &
       pm_type_is_soa + pm_type_is_aos + pm_type_is_seq )

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
  integer,parameter:: pm_type_new_fix_value=12+pm_type_has_fix
  integer,parameter:: pm_type_new_contains=13
  integer,parameter:: pm_type_new_fix=14        
  integer,parameter:: pm_type_new_dref=15
  integer,parameter:: pm_type_new_par_kind=16
  integer,parameter:: pm_type_new_proc_sig=17
  integer,parameter:: pm_type_new_undef_result=18
  integer,parameter:: pm_type_new_literal_value=19+pm_type_has_literal
  integer,parameter:: pm_type_new_except=20
  integer,parameter:: pm_type_new_param=21+pm_type_has_params
  integer,parameter:: pm_type_new_gated=22
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
  integer,parameter:: pm_type_is_gated=22
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
  integer,public,parameter:: pm_a_uninit_type = pm_last_sys_type + 1
  integer,public,parameter:: pm_a_rec_type = pm_last_sys_type + 2
  integer,public,parameter:: pm_a_unique_type = pm_last_sys_type + 3
  integer,public,parameter:: pm_a_poly_type = pm_last_sys_type + 4
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
    '<dref>        ','<dref-inv>    ','<elemref>     ','PM__uninit    ',&
    'a_rec         ','a_unique      ','a_poly        ','a_basic       '/)
    
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
       nleaves=min(nleaves+flags/pm_type_leaves,pm_type_max_leaves)
    enddo
    arr(1)=ior(iand(arr(1),pm_type_leaves-1),iand(tflags,&
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

  !====================================
  ! Number of dimensions of array type
  !====================================
  function pm_arr_type_ndims(context,tno) result(ndim)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: ndim
    ndim=pm_type_numargs(context,pm_type_arg(context,tno,3))-1
  end function pm_arr_type_ndims

  !=======================================
  ! Type of element #elem of an SOA array
  !======================================
  function pm_type_soa_elem(context,typ,elem) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: typ,elem
    integer:: tno
    type(pm_ptr):: tv
    tv=pm_type_vect(context,typ)
    tno=pm_new_arr_type(context,pm_tv_name(tv),&
            pm_type_arg(context,pm_tv_arg(tv,1),elem),&
            pm_tv_arg(tv,3),pm_tv_arg(tv,3))
  end function pm_type_soa_elem

  function pm_type_is_soa_rec(context,typ) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    logical:: ok
    ok=.false.
    if(pm_type_kind(context,typ)==pm_type_is_rec) then
       if(iand(pm_type_flags(context,typ),pm_type_is_soa)/=0) then
          ok=.true.
       endif
    endif
  end function pm_type_is_soa_rec
  
  !=========================
  ! Create type (a or b)
  !=========================
  function pm_type_union(context,a,b) result(tno)
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
  end function pm_type_union

  !==========================================
  ! Create new polymorphic type: *etype
  !==========================================
  function pm_new_poly_type(context,etyp) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: etyp
    integer:: tno
    integer,dimension(2):: args
    args(1)=pm_type_new_poly
    args(2)=etyp
    tno=pm_new_basic_type(context,args)
  end function pm_new_poly_type

  
  !================================================
  ! Create new polymorphic value type: *etype=vtyp
  ! - assumes that vtyp conforms to etyp
  !================================================
  function pm_new_poly_val_type(context,etyp,vtyp) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: etyp,vtyp
    integer:: tno
    integer,dimension(3):: args
    integer:: recur
!!$    write(*,*) 'New poly val: ',trim(pm_type_as_string(context,etyp)),' : ',&
!!$         trim(pm_type_as_string(context,vtyp))
    args(1)=pm_type_new_poly
    args(2)=etyp
    recur=-1
    args(3)=pm_type_identify_recursive(context,vtyp,etyp,recur)
    tno=pm_new_basic_type(context,args)
    if(recur>=0) then
       call pm_type_set_recursive_ref(context,recur,tno)
    endif
!!$    write(*,*) 'Poly type is:',tno
  end function pm_new_poly_val_type

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

  !==========================================
  !  Create new uninitialised type
  !==========================================
  function pm_new_uninitialised_type(context,etyp) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: etyp
    integer:: tno
    integer,dimension(3):: arr
    arr(1)=pm_type_new_uninitialised
    arr(2)=0
    if(etyp<=0) then
       arr(3)=0
    else
       arr(3)=etyp
    endif
    tno=pm_new_basic_type(context,arr)
  end function pm_new_uninitialised_type
  
  
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

  !==============================================
  ! Create new compile time name value type from
  ! literal string value
  !==============================================
  function pm_name_type_from_literal_string(context,tno,modname) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,modname
    integer:: tno2
    type(pm_ptr):: str
    integer:: i,ic,name
    character(len=1):: c
    character(len=300):: strchars
    str=pm_type_val(context,tno)
    if(pm_debug_checks) then
       if(pm_fast_vkind(str)/=pm_string) then
          write(*,*) 'vkind=',pm_fast_vkind(str),pm_type_as_string(context,tno)
          call pm_panic('Type to literal string')
       endif
    endif
    do i=0,pm_fast_esize(str)
       c=str%data%s(str%offset+i)
       ic=iachar(c)
       if(.not.(c=='_'.or.ic>=iachar('a').and.ic<=iachar('z').or.&
            ic>=iachar('A').and.ic<=iachar('Z').or.&
            i>0.and.ic>=iachar('0').and.ic<=iachar('9'))) then
          tno2=-1
          return
       endif
    enddo
    name=pm_type_name(context,tno)
    if(str%data%s(str%offset)=='_') then
       if(pm_fast_esize(str)<1) then
          tno2=-1
          return
       endif
       strchars=pm_name_as_string(context,name)
       name=pm_lname_entry(context,modname,trim(strchars(2:)))
    endif
    tno2=pm_new_name_type(context,name)
  contains
    include 'fesize.inc'
    include 'fvkind.inc'
  end function pm_name_type_from_literal_string

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
  function pm_new_literal_value_type(context,val,vindex,typ) result(tno)
    type(pm_context),pointer:: context
    type(pm_ptr),intent(in):: val
    integer,intent(in),optional:: vindex,typ
    integer:: tno
    integer,dimension(3):: args
    args(1)=pm_type_new_literal_value
    if(present(vindex)) then
       args(2)=vindex
    else
       args(2)=pm_set_add(context,context%names,val)
    endif
    if(present(typ)) then
       args(3)=typ
    else
       args(3)=pm_fast_typeof(val)
       if(args(3)==pm_string.or.args(3)==pm_int32) args(3)=pm_string_type
    endif
    tno=pm_new_basic_type(context,args,val)
  contains
    include 'ftypeof.inc'
  end function pm_new_literal_value_type

  !=================================================
  ! Get the numerical value for literal or fix int
  !================================================
  function pm_type_int_value(context,tno,ok) result(n)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    logical,intent(out):: ok
    integer(pm_ln):: n
    type(pm_ptr):: p
    integer:: tk
    tk=pm_type_kind(context,tno)
    if(tk==pm_type_is_literal_value.or.tk==pm_type_is_fix_value) then
       p=pm_type_val(context,tno)
       n=p%data%ln(p%offset)
       ok=.true.
    else
       ok=.false.
    endif
  end function pm_type_int_value

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

  !===================================
  ! Return the kind of a given type
  !===================================
  function pm_type_base_kind(context,tno) result(kind)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: kind
    integer:: flags
    flags=pm_type_flags(context,tno)
    kind=iand(flags,pm_type_kind_mask)
  end function pm_type_base_kind
  
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
          if(tno2==tno) then
             flags=pm_type_is_recursive
             exit
          elseif(tno2/=0) then
             tv=pm_type_vect(context,tno2)
             flags=ior(iand(pm_type_is_recursive,flags),pm_tv_flags(tv))
          else
             flags=pm_type_has_generic
             exit
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
  function pm_user_type_lookup_by_name(context,mod,name) result(tno)
    type(pm_context),pointer:: context
    integer,intent(in):: mod,name
    integer:: tno
    integer:: arr(2)
    arr(1)=pm_type_new_user
    arr(2)=pm_name2(context,-mod,name)
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


  !===============================================
  ! Strip off parameter type 
  !===============================================
  function pm_type_strip_param(context,typ) result(typ2)
    type(pm_context),pointer:: context
    integer,intent(in):: typ
    integer:: typ2
    if(pm_type_kind(context,typ)==pm_type_is_param) then
       typ2=pm_type_arg(context,typ,1)
    else
       typ2=typ
    endif
  end function pm_type_strip_param
  
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
  ! Rules for combining modes in a standard procedure call or record creation
  !
  ! Error codes:
  !   combined_mode=-1,-2...
  !        Shared distributed value not allowed for position -combined_mode
  !  shared_ok -- permissible to have an argument with 'shared' mode
  !============================================================================================
  function pm_type_combine_modes(context,array,is_cond,shared_ok,mode0) result(combined_mode)
    type(pm_context),pointer:: context
    integer,intent(in),dimension(:):: array
    logical,intent(in):: is_cond,shared_ok
    integer,intent(in),optional:: mode0
    integer:: combined_mode
    integer:: i,mode,cmode,tno
    cmode=sym_invar
    if(present(mode0)) cmode=mode0
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
    if(size(array)==0) then
       ! Make sure zero length lists have private mode
       ! as they are used in & arguments
       mixed_mode=sym_private
       return
    endif
    cmax=sym_private
    cmin=sym_shared
    do i=1,size(array)
       tno=pm_type_strip_mode(context,array(i),mode)
       cmin=min(cmin,mode)
       cmax=max(cmax,mode)
    enddo
    if(cmin>=sym_mixed) then
       mixed_mode=cmin
    elseif(cmax>=sym_mixed) then
       mixed_mode=sym_mixed
    else
       mixed_mode=sym_private
    endif
  end function pm_type_mix_modes

  !=========================================
  ! Type and mode of an imported value
  ! - handles tuples with individual modes
  !=========================================
  function pm_type_imported(context,tno,ok) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    logical,intent(out):: ok
    integer:: tno2
    type(pm_ptr):: tv
    integer:: tk,mode
    ok=.true.
    tv=pm_type_vect(context,tno)
    tk=pm_tv_kind(tv)
    if(tk==pm_type_is_tuple.or.tk==pm_type_is_vtuple) then
       tno2=remake(pm_tv_numargs(tv))
    else
       tno2=import_mode(tno,mode)
    endif
  contains
    
    function remake(n) result(tno2)
      integer,intent(in):: n
      integer:: tno2
      integer:: arr(n+2)
      integer:: i,overall_mode,mode
      arr(1)=pm_tv_flags(tv)
      arr(2)=pm_tv_name(tv)
      overall_mode=sym_shared
      do i=1,n
         arr(i+2)=import_mode(pm_tv_arg(tv,i),mode)
         overall_mode=min(overall_mode,mode)
      enddo
      tno2=pm_type_add_mode(context,pm_new_type(context,arr),overall_mode)
    end function remake

    function import_mode(tno,new_mode) result(tno2)
      integer,intent(in):: tno
      integer,intent(out):: new_mode
      integer:: tno2
      integer:: typ,mode
      typ=pm_type_strip_mode(context,tno,mode)
      if(mode==sym_shared) ok=.false.
      if(iand(pm_type_flags(context,typ),pm_type_has_distributed)/=0) then
         new_mode=sym_shared
      else
         new_mode=sym_invar
      endif
      tno2=pm_type_add_mode(context,typ,new_mode)
    end function import_mode
      
  end function pm_type_imported
  
  !===================================
  ! Does mode1 include mode2 ?
  !===================================
  function pm_mode_includes(mode1,mode2) result(ok)
    integer,intent(in):: mode1,mode2
    logical:: ok
    if(mode1<0) then
       if(mode2<0) then
          ok=iand(-mode1,-mode2)==-mode2
       else
          ok=iand(-mode1,ishft(1,mode2-first_mode))/=0
       endif
    else
       if(mode2<0) then
          ok=iand(ishft(1,mode1-first_mode),-mode2)==-mode2
       else
          ok=mode1==mode2
       endif
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
    typ1=tno1
    typ2=tno2
    tv1=pm_type_vect(context,typ1)
    tv2=pm_type_vect(context,typ2)
    tk1=pm_tv_kind(tv1)
    tk2=pm_tv_kind(tv2)
    do while(tk1==pm_type_is_par_kind.or.tk1==pm_type_is_vect)
       typ1=pm_tv_arg(tv1,1)
       tv1=pm_type_vect(context,typ1)
       tk1=pm_tv_kind(tv1)
    enddo
    do while(tk2==pm_type_is_par_kind.or.tk2==pm_type_is_vect)
       typ2=pm_tv_arg(tv2,1)
       tv2=pm_type_vect(context,typ2)
       tk2=pm_tv_kind(tv2)
    enddo
    ok=tno1==tno2
    if(.not.ok) ok=pm_type_includes(context,tno1,tno2,pm_type_incl_equiv)
  end function pm_type_equal


  !===========================================================================================
  ! Check if two concrete types are the same record (ignoring modes and vector type wrappers)
  !============================================================================================
  function pm_type_same_rec(context,tno1,tno2) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno1,tno2
    logical:: ok
    type(pm_ptr):: tv1,tv2
    integer:: typ1,typ2,tk1,tk2
    typ1=tno1
    typ2=tno2
    tv1=pm_type_vect(context,typ1)
    tv2=pm_type_vect(context,typ2)
    tk1=pm_tv_kind(tv1)
    tk2=pm_tv_kind(tv2)
    do while(tk1==pm_type_is_par_kind.or.tk1==pm_type_is_vect)
       typ1=pm_tv_arg(tv1,1)
       tv1=pm_type_vect(context,typ1)
       tk1=pm_tv_kind(tv1)
    enddo
    do while(tk2==pm_type_is_par_kind.or.tk2==pm_type_is_vect)
       typ2=pm_tv_arg(tv2,1)
       tv2=pm_type_vect(context,typ2)
       tk2=pm_tv_kind(tv2)
    enddo
    if(tk1/=pm_type_is_rec.and.tk1/=pm_type_is_tuple.or.tk1/=tk2) then
       ok=.false.
    else
       ok=pm_tv_name(tv1)==pm_tv_name(tv2).and.pm_tv_numargs(tv1)==pm_tv_numargs(tv2)
    endif
  end function pm_type_same_rec
    
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
  recursive function pm_type_includes(context,supertype,subtype,&
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
          nt=pm_tv_name(t)
          ok=nt==pm_tv_name(u)
          if(ok.and.nt==0) then
             ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),mode,&
                  params,base,user,ubase)
          endif
          return
       end if
    case(pm_type_is_fix,pm_type_is_literal)
       if(tk==uk.or.tk==pm_type_is_fix) then
          ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),mode,&
               params,base,user,ubase)
          return
       elseif(tk==pm_type_is_basic) then
          ok=pm_test_type_includes(context,p,pm_tv_arg(u,1),mode,&
               params,base,user,ubase)
          return
       endif
    case(pm_type_is_user)
       if(tk/=pm_type_is_user) then
          if(iand(mode,pm_type_incl_extract)/=0) then
             if(iand(pm_tv_flags(u),pm_type_is_recursive)/=0) then
                goto 10
             endif
          endif
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
                mode,params,base,user,ubase)) then
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
    case(pm_type_is_gated)
       if(test_gated_type(context,q)) then
          ok=pm_test_type_includes(context,p,pm_tv_arg(u,pm_tv_numargs(u)),&
               mode,params,base,user,ubase)
       else
          ok=.true.
       endif
       return
    case(pm_type_is_dref)
       if(tk==pm_type_is_dref) then
          nt=pm_tv_name(t)
          nu=pm_tv_name(u)
          if(nt/=0.and.nt/=nu) then
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
       else
          ok=pm_test_type_includes(context,p,pm_tv_arg(u,1),&
               mode,params,base,user,ubase)
       endif
       return
    case(pm_type_is_bottom)
       ok=.true.
       return
    end select

10  continue
    
    ! Now do tests that look at 1st type first
    select case(tk)
    case(pm_type_is_basic)
       ok=.false.
    case(pm_type_is_dref)
       ok=.false.
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
                  pm_test_type_includes(context,pm_tv_arg(t,3),pm_tv_arg(u,3),&
                  mode,params,base,user,ubase)
          endif
       endif
    case(pm_type_is_poly)
       if(uk/=tk) then
          ok=.false.
       else
          ok=pm_test_type_includes(context,pm_tv_name(t),pm_tv_name(u),&
               ior(mode,pm_type_incl_nomatch),params,base,user,ubase)
          if(pm_tv_numargs(t)>0.and.pm_tv_numargs(u)>0.and.iand(mode,pm_type_incl_equiv)==0) then
             do i=1,pm_tv_numargs(u)
                do j=1,pm_tv_numargs(t)
                   ok=pm_test_type_includes(context,pm_tv_arg(t,j),pm_tv_arg(u,i),&
                        mode,params,base,user,ubase)
                   if(ok) exit
                enddo
                if(.not.ok) exit
             end do
          endif
       endif
    case(pm_type_is_type)
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
       elseif(iand(pm_tv_flags(t),pm_type_is_yield+pm_type_is_list)/=&
            iand(pm_tv_flags(u),pm_type_is_yield+pm_type_is_list)) then
          ok=.false.
       else
          if(iand(pm_tv_flags(t),pm_type_is_when)/=0.and.iand(pm_tv_flags(u),pm_type_is_when)==0) then
             ! ( T when) does not include ( U ) if U includes T
             ! - implements that when is more specific iff T==U
             if(pm_test_type_includes(context,q,p,&
                  mode,params,base,user,ubase)) then
                ok=.false.
                return
             endif
          endif
          nt=pm_tv_numargs(t)
          nu=pm_tv_numargs(u)
          !write(*,*) 'nt=',nt,'nu=',nu
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
          elseif(nt>nu) then
             ok=.false.
             return
             do i=nu+1,nt
                if(.not.pm_test_type_includes(context,pm_tv_arg(t,i),&
                     pm_tv_arg(u,nu),mode,params,base,&
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
       ok=.false.
       do i=1,pm_tv_numargs(t)
          if(pm_test_type_includes(context,pm_tv_arg(t,i),q,&
               mode,params,base,user,ubase)) then
             ok=.true.
             if(iand(mode,pm_type_incl_extract+pm_type_incl_nomatch)/=pm_type_incl_extract) then
                return
             endif
          endif
      enddo
    case(pm_type_is_all)
       do i=1,pm_tv_numargs(t)
          if(.not.pm_test_type_includes(context,pm_tv_arg(t,i),q,&
               ior(mode,pm_type_incl_nomatch),params,base,user,ubase)) then
             ok=.false.
             return
          endif
       enddo
       ! Just when matching - need to run all and match
       if(iand(mode,pm_type_incl_extract+pm_type_incl_nomatch)==pm_type_incl_extract) then
          do i=1,pm_tv_numargs(t)
             ok=pm_test_type_includes(context,pm_tv_arg(t,i),q,&
                mode,params,base,user,ubase)
          enddo
       endif
       ok=.true.
    case(pm_type_is_except)
       ok=pm_test_type_includes(context,pm_tv_arg(t,2),q,&
            ior(mode,pm_type_incl_nomatch),params,base,user,ubase)
       if(.not.ok) then
          ok=pm_test_type_includes(context,pm_tv_arg(t,1),q,&
               mode,params,base,user,ubase)
       else
          ok=.false.
       endif
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
             params(nt)=pm_type_union(context,params(nt),q)
          endif
       endif
    case(pm_type_is_vect,pm_type_is_uninitialised)
       ok=tk==uk
       if(ok) ok=pm_test_type_includes(context,pm_tv_arg(t,1),pm_tv_arg(u,1),&
            mode,params,base,user,ubase)
    case(pm_type_is_gated)
       if(test_gated_type(context,p)) then
          ok=pm_test_type_includes(context,pm_tv_arg(t,pm_tv_numargs(t)),q,&
               mode,params,base,user,ubase)
       else
          ok=.false.
       endif
    case(pm_type_is_bottom)
       ok=.false.
    case(pm_type_is_category)
       select case(p)
       case(pm_a_uninit_type)
          ok=uk==pm_type_is_uninitialised
       case(pm_a_rec_type)
          ok=uk==pm_type_is_rec
       case(pm_a_unique_type)
          ok=uk==pm_type_is_single_name
       case(pm_a_poly_type)
          ok=uk==pm_type_is_poly
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
       elseif(pm_type_contains_elem(context,p,pm_tv_arg(u,3),&
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
    case(pm_type_is_gated)
       if(test_gated_type(context,q)) then
          ok=pm_type_contains_elem(context,p,pm_tv_arg(u,pm_tv_numargs(u)),&
               mode,params,base,user,ubase)
       else
          ok=.true.
       endif
    case default
       ok=.false.
    end select
  end function pm_type_contains_elem

  recursive function pm_type_intersects(context,tno1,tno2,user,userbase) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno1,tno2,userbase
    integer,intent(inout):: user(:)
    logical:: ok
    integer:: tk1,tk2
    type(pm_ptr):: tv1,tv2,r
    integer:: n1,n2,i,j

    if(tno1==tno2) then
       ok=.true.
       return
    endif
    if(tno1==0.or.tno2==0) then
       ok=.true.
       return
    endif
    if(pm_type_includes(context,tno2,tno1,pm_type_incl_type)) then
       ok=.true.
       return
    endif
    if(pm_type_includes(context,tno1,tno2,pm_type_incl_type)) then
       ok=.true.
       return
    endif
    if(pm_type_is_concrete(context,tno1).or.pm_type_is_concrete(context,tno2)) then
       ok=.false.
       return
    endif
    tv1=pm_type_vect(context,tno1)
    tv2=pm_type_vect(context,tno2)
    tk1=pm_tv_kind(tv1)
    tk2=pm_tv_kind(tv2)
    n1=pm_tv_numargs(tv1)
    n2=pm_tv_numargs(tv2)
    
    select case(tk2)
    case(pm_type_is_user)
       if(tk1/=pm_type_is_user) then
          do i=2,userbase,2
             if(user(i)==tno1.and.user(i+1)==tno2) then
                ok=.true.
                return
             endif
          enddo
          if(userbase+2>size(user)) then
             call pm_panic('Program too complex - nested type defs')
          endif
          user(userbase+1)=tno1
          user(userbase+2)=tno2
          r=pm_dict_val(context,context%tcache,int(tno2,pm_ln))
          ok=pm_type_intersects(context,tno1,int(r%offset),user,userbase+2)
          return
       endif
       return
    case(pm_type_is_any)
       if(tk1==pm_type_is_any) then
          ok=.false.
          do i=1,n1
             do j=1,n2
                if(i/=j) then
                   if(pm_type_intersects(context,pm_tv_arg(tv1,1),pm_tv_arg(tv2,1),&
                        user,userbase)) then
                      ok=.true.
                      return
                   endif
                endif
             enddo
          enddo
       else
          ok=.false.
          do j=1,n2
             if(pm_type_intersects(context,tno1,pm_tv_arg(tv2,j),&
                  user,userbase)) then
                ok=.true.
                return
             endif
          enddo
       endif
       return
    case(pm_type_is_all)
       if(tk1==pm_type_is_all) then
          ok=.true.
          do i=1,n1
             do j=1,n2
                if(i/=j) then
                   if(.not.pm_type_intersects(context,pm_tv_arg(tv1,1),pm_tv_arg(tv2,1),&
                        user,userbase)) then
                      ok=.false.
                      return
                   endif
                endif
             enddo
          enddo
       else
          ok=.true.
          do j=1,n2
             if(.not.pm_type_intersects(context,tno1,pm_tv_arg(tv2,j),&
                  user,userbase)) then
                ok=.false.
                return
             endif
          enddo
       endif
       return
    case(pm_type_is_contains,pm_type_is_category,pm_type_is_bottom)
       ok=.true.
       return
    case(pm_type_is_gated)
       if(test_gated_type(context,tno2)) then
          ok=pm_type_intersects(context,tno1,pm_tv_arg(tv2,pm_tv_numargs(tv2)),&
               user,userbase)
       else
          ok=.true.
       endif
       return
    case(pm_type_is_par_kind,pm_type_is_vect,pm_type_is_has,&
         pm_type_is_params,pm_type_is_param)
       ok=pm_type_intersects(context,pm_tv_arg(tv2,1),tno2,user,userbase)
       return
    case(pm_type_is_except)
       if(tk1==pm_type_is_except) then
          if(pm_type_intersects(context,pm_tv_arg(tv1,1),pm_tv_arg(tv2,1),&
               user,userbase)) then
             if(pm_type_includes(context,pm_tv_arg(tv2,2),pm_tv_arg(tv1,1),pm_type_incl_type)) then
                ok=.false.
                return
             endif
             if(pm_type_includes(context,pm_tv_arg(tv1,2),pm_tv_arg(tv2,1),pm_type_incl_type)) then
                ok=.false.
                return
             endif
             ok=.true.
             return
          else
             ok=.false.
          endif
       else
          if(pm_type_intersects(context,tno1,pm_tv_arg(tv2,1),&
               user,userbase)) then
             if(pm_type_includes(context,pm_tv_arg(tv2,2),tno1,pm_type_incl_type)) then
                ok=.false.
                return
             endif
             ok=.true.
          else
             ok=.false.
          endif
       endif
       return
    end select
    
    
    select case(tk1)
    case(pm_type_is_user)
       do i=2,userbase,2
          if(user(i)==tno1.and.user(i+1)==tno2) then
             ok=.true.
             return
          endif
       enddo
       if(userbase+2>size(user)) then
          call pm_panic('Program too complex - nested type defs')
       endif
       user(userbase+1)=tno1
       user(userbase+2)=tno2
       r=pm_dict_val(context,context%tcache,int(tno1,pm_ln))
       ok=pm_type_intersects(context,int(r%offset),tno2,user,userbase+2)
    case(pm_type_is_any)
       ok=.false.
       do i=1,n1
          if(pm_type_intersects(context,tno1,pm_tv_arg(tv1,i),&
               user,userbase)) then
             ok=.true.
             return
          endif
       enddo
    case(pm_type_is_all)
       ok=.true.
       do i=1,n1
          if(pm_type_intersects(context,tno1,pm_tv_arg(tv1,i),&
               user,userbase)) then
             ok=.true.
             return
          endif
       enddo
    case(pm_type_is_except)
       if(pm_type_intersects(context,pm_tv_arg(tv1,1),tno2,&
            user,userbase)) then
          if(pm_type_includes(context,pm_tv_arg(tv2,2),tno1,pm_type_incl_type)) then
             ok=.false.
             return
          endif
          ok=.true.
       else
          ok=.false.
       endif
    case(pm_type_is_contains,pm_type_is_category,pm_type_is_bottom)
       ok=.true.
    case(pm_type_is_par_kind,pm_type_is_vect,pm_type_is_has,&
         pm_type_is_params,pm_type_is_param)
       ok=pm_type_intersects(context,pm_tv_arg(tv1,1),tno2,user,userbase)
    case(pm_type_is_gated)
       if(test_gated_type(context,tno1)) then
          ok=pm_type_intersects(context,pm_tv_arg(tv1,pm_tv_numargs(tv1)),tno2,&
               user,userbase)
       else
          ok=.true.
       endif
    case default
       if(tk1/=tk2) then
          ok=.false.
          return
       endif
       if(pm_tv_name(tv1)/=pm_tv_name(tv2)) then
          ok=.false.
          return
       endif
       if(n1/=n2) then
          ok=.false.
          return
       endif
       do i=1,n1
          if(.not.pm_type_intersects(context,pm_tv_arg(tv1,i),pm_tv_arg(tv2,i),&
               user,userbase)) then
             ok=.false.
             return
          endif
       enddo
       ok=.true.
    end select
  end function pm_type_intersects


  function test_gated_type(context,tno) result(ok)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    logical:: ok
    integer:: i
    type(pm_ptr):: tv
    integer::stack(max_user_nesting)
    tv=pm_type_vect(context,tno)
     do i=1,pm_tv_numargs(tv)-1,2
       if(.not.pm_type_intersects(context,pm_tv_arg(tv,i),pm_tv_arg(tv,i+1),stack,1)) then
          ok=.false.
          return
       endif
    enddo
    ok=.true.
  end function test_gated_type
 
  !===============================================
  ! Perform enveloping conversions if possible
  ! Returns -1 if not possible
  ! Set converted_to_poly if a poly conversion has
  ! been performed and the value needs boxing
  !==============================================
  function pm_type_convert(context,partyp,argtyp,doliteral,doproc,dopoly,converted_to_poly) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: partyp,argtyp
    logical,intent(in):: doliteral,doproc,dopoly
    logical,intent(out),optional:: converted_to_poly
    integer:: ctyp
    integer:: tk,ptyp,atyp,pmode,amode
    type(pm_ptr):: tv
!!$    write(*,*) 'Convert',trim(pm_type_as_string(context,partyp)),&
!!$         '::',trim(pm_type_as_string(context,argtyp)),doliteral,doproc
    ctyp=-1
    if(partyp<0.or.argtyp<0) then
       return
    endif
    if(present(converted_to_poly)) converted_to_poly=.false.
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
    if(tk==pm_type_is_param) then
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
       ctyp=pm_poly_type_convert(context,ptyp,atyp,converted_to_poly)
    endif
    ctyp=pm_type_add_mode(context,ctyp,amode)
!!$    write(*,*) 'To:',trim(pm_type_as_string(context,ctyp))
  end function pm_type_convert


  !================================================================
  ! Autoconversion of a literal type 
  !================================================================
  function pm_literal_type_convert(context,partyp,argtyp) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: partyp,argtyp
    integer:: ctyp
    integer:: tk
 
    ctyp=pm_type_for_var(context,pm_type_arg(context,argtyp,1))
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
    elseif(tk==pm_type_is_literal.or.tk==pm_type_is_literal_value) then
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
  ! Autoconversion to broader poly type or from
  ! monomorphic to polymorphic type
  ! Returns -1 if not possible
  !================================================================
  function pm_poly_type_convert(context,partyp,argtyp,converted_to_poly) result(ctyp)
    type(pm_context),pointer:: context
    integer,intent(in):: partyp,argtyp
    logical,intent(out),optional:: converted_to_poly
    integer:: ctyp
    type(pm_ptr):: tv1,tv2
    ctyp=-1
    tv1=pm_type_vect(context,partyp)
    tv2=pm_type_vect(context,argtyp)
    if(present(converted_to_poly)) converted_to_poly=.false.
    if(pm_tv_kind(tv1)==pm_type_is_poly) then
       if(pm_tv_kind(tv2)==pm_type_is_poly) then
          if(pm_type_includes(context,pm_tv_name(tv1),pm_tv_name(tv2),&
               pm_type_incl_type)) then
             if(pm_tv_numargs(tv2)>0) then
                call remake(pm_tv_numargs(tv2))
             else
                ctyp=partyp
             endif
          endif
       else
          if(pm_type_includes(context,pm_tv_name(tv1),argtyp,&
               pm_type_incl_type)) then
             ctyp=pm_new_poly_val_type(context,pm_tv_name(tv1),argtyp)
             if(present(converted_to_poly)) converted_to_poly=.true.
          endif
       endif
    endif
  contains
    subroutine remake(n)
      integer,intent(in)::n
      integer,dimension(n+2):: a
      integer:: i,recur
      a(1)=pm_type_new_poly
      a(2)=pm_tv_name(tv1)
      recur=-1
      do i=3,n+2
         a(i)=pm_tv_arg(tv2,i-2)
         if(iand(pm_type_flags(context,a(i)),&
              pm_type_has_poly+pm_type_is_recursive)/=0) then
            if(recur<0) then
               recur=pm_type_new_recursive_ref(context)
            endif
            a(i)=pm_type_move_recursive(context,a(i),recur)
         endif
      enddo
      ctyp=pm_new_type(context,a)
      if(recur>0) call pm_type_set_recursive_ref(context,recur,ctyp)
    end subroutine remake
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
    !write(*,*) 'CVPROC>',trim(pm_type_as_string(context,ptyp)),' to ',trim(pm_type_as_string(context,argtyp))
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
  recursive function pm_type_find_elem(context,value_type,name_type,change,islist,etype) result(offset)
    type(pm_context),pointer:: context
    integer,intent(in):: value_type,name_type
    logical,intent(in):: change,islist
    integer,intent(out):: etype
    integer:: offset,ptype,mode,nametype,tno
    type(pm_ptr):: tv,nameval,names
    integer:: tk,i,name
    nametype=pm_type_strip_mode(context,name_type,mode)
    tno=pm_type_strip_mode(context,value_type,mode)
    tk=pm_type_kind(context,nametype)
    if(tk==pm_type_is_literal_value.or.tk==pm_type_is_fix_value) then
       tv=pm_type_vect(context,tno)
       tk=pm_tv_kind(tv)
       nameval=pm_type_val(context,nametype)
       offset=nameval%data%ln(nameval%offset)
       if(tk==pm_type_is_rec.or.tk==pm_type_is_dref) then
          if(islist.or.offset<=0.or.offset>pm_type_numargs(context,tno)) then
             offset=0
          elseif(change) then
             names=pm_name_val(context,pm_tv_name(tv))
             if(names%data%i(names%offset+offset)>0) offset=0
          endif
          if(offset/=0) etype=pm_type_arg(context,tno,offset)
          if(offset/=0.and.tk==pm_type_is_dref) offset=offset-1
       elseif(tk==pm_type_is_tuple) then
          if((.not.islist).or.offset<=0.or.offset>pm_type_numargs(context,tno)) then
             offset=0
          else
             etype=pm_type_arg(context,tno,offset)
          endif
          return
       else
          offset=0
       endif
       if(offset>0) call add_mode
       return
    endif
    name=pm_type_name(context,nametype)
    if(tno<=0) then
       offset=0
       return
    endif
    tv=pm_type_vect(context,tno)
    tk=pm_tv_kind(tv)
    select case(tk)
    case(pm_type_is_all)
       do i=1,pm_tv_numargs(tv)
          offset=pm_type_find_elem(context,pm_tv_arg(tv,i),nametype,change,islist,etype)
          if(offset/=0) return
       enddo
       offset=0
       return
    case(pm_type_is_literal_value)
       offset=pm_type_find_elem(context,pm_tv_arg(tv,1),nametype,change,islist,etype)
       tv=pm_type_vect(context,etype)
       if(pm_tv_kind(tv)==pm_type_is_fix_value) then
          etype=pm_new_literal_value_type(context,&
               pm_type_val(context,etype),pm_type_name(context,etype))
       else
          etype=pm_new_literal_value_type(context,&
               pm_null_obj,0,etype)
       endif
    case(pm_type_is_rec)
       call pm_type_elem_offset(context,tv,name,change,offset,etype)
    case default
       offset=0
       return
    end select
    if(offset>0) call add_mode
  contains
    
    ! Add mode making sure that a non-distributed element of a shared value is invar
    subroutine add_mode
      if(mode==sym_shared) then
         if(iand(pm_type_flags(context,etype),pm_type_has_distributed)==0) mode=sym_invar
      endif
      etype=pm_type_add_mode(context,etype,mode)
    end subroutine add_mode
    
  end function pm_type_find_elem

  !================================================================
  ! Find offset and type for named element in struct/rec type
  ! Returns offset and type of element
  ! If no such element offset=0
  !================================================================
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

  !================================================================
  ! Concrete only version of a type (used/usable only for returns from builtin functions)
  !================================================================
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
      a(1)=iand(pm_tv_flags(tv),not(pm_type_has_generic))
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


  recursive function pm_type_replace(context,tno,oldtype,newtype) result(tno2)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,oldtype,newtype 
    integer:: tno2
    type(pm_ptr):: tv
    integer:: tk,oldtyp,newtyp
    if(pm_type_kind(context,oldtype)==pm_type_is_dref) then
       oldtyp=pm_type_arg(context,oldtype,1)
       newtyp=pm_type_arg(context,newtype,1)
    else
       oldtyp=oldtype
       newtyp=newtype
    endif
    if(tno==oldtyp) then
       tno2=newtyp
       return
    endif
    tv=pm_type_vect(context,tno)
    tk=pm_tv_kind(tv)
    select case(tk)
    case(pm_type_is_array)
       tno2=pm_new_arr_type(context,pm_tv_name(tv),&
            pm_type_replace(context,pm_tv_arg(tv,1),oldtyp,newtyp),&
            pm_tv_arg(tv,2),pm_tv_arg(tv,3))
    case(pm_type_is_dref)
       call remake_dref(pm_tv_numargs(tv))
    case(pm_type_is_rec,pm_type_is_tuple)
       call remake(pm_tv_numargs(tv))
    case default
       tno2=tno
    end select
  contains
    
    recursive subroutine remake(n)
      integer,intent(in):: n
      integer,dimension(n+2):: a
      integer:: i,etyp
      logical:: changed
      a(1)=pm_tv_flags(tv)
      a(2)=pm_tv_name(tv)
      changed=.false.
      do i=1,n
         etyp=pm_tv_arg(tv,i)
         a(i+2)=pm_type_replace(context,etyp,oldtyp,newtyp)
         changed=changed.or.a(i+2)/=etyp
      enddo
      if(changed) then
         tno2=pm_new_type(context,a)
      else
         tno2=tno
      endif
    end subroutine remake

    recursive subroutine remake_dref(n)
      integer,intent(in):: n
      integer,dimension(n+2):: a
      integer:: i,etyp
      logical:: changed
      a(1)=pm_tv_flags(tv)
      a(2)=pm_tv_name(tv)
      etyp=pm_tv_arg(tv,1)
      a(3)=pm_type_replace(context,etyp,oldtyp,newtyp)
      changed=a(3)/=etyp
      if(changed) then
         do i=2,n
            a(i+2)=pm_tv_arg(tv,i)
         enddo
         tno2=pm_new_type(context,a)
      else
         tno2=tno
      endif
    end subroutine remake_dref
    
  end function pm_type_replace

  
  

  !================================================================
  ! Create a new type with with all fix values converted
  ! to base type  - also strips off mode
  !================================================================
  recursive function pm_type_for_var(context,tno) result(typ)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: typ
    type(pm_ptr):: tv
    integer:: tk
    typ=tno
    tv=pm_type_vect(context,tno)
    tk=pm_tv_kind(tv)
    if(tk/=pm_type_is_par_kind.and.iand(pm_tv_flags(tv),pm_type_has_fix)==0) return
    select case(tk)
    case(pm_type_is_par_kind)
       typ=pm_type_for_var(context,pm_tv_arg(tv,1))
    case(pm_type_is_user)
       typ=pm_user_type_body(context,tno)
    case(pm_type_is_rec)
       call remake(pm_tv_numargs(tv))
    case(pm_type_is_fix_value)
       typ=pm_tv_arg(tv,1)
    end select
  contains
    recursive subroutine remake(n)
      integer,intent(in):: n
      integer,dimension(n+2):: a
      integer:: i
      a(1)=iand(pm_tv_flags(tv),not(pm_type_has_fix))
      a(2)=pm_tv_name(tv)
      do i=1,n
         a(i+2)=pm_type_for_var(context,pm_tv_arg(tv,i))
      enddo
      typ=pm_new_type(context,a)
    end subroutine remake
  end function pm_type_for_var

  !================================================================
  ! Combine two types for the same variable - must be the same type
  ! except for poly values which are merged
  !================================================================
  recursive function pm_type_combine(context,tno,tno2,ok,added) result(typ)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,tno2
    logical,intent(out):: ok,added
    integer:: typ
    type(pm_ptr):: tv,tv2
    integer:: tk,tk2,typ1,typ2

!!$    write(*,*) 'combine types: ',trim(pm_type_as_string(context,tno)),' with ',&
!!$         trim(pm_type_as_string(context,tno2))
    
    ok=.true.
    added=.false.
    if(tno<=0) then
       typ=tno2
       return
    endif
    typ=tno
    if(tno2<=0.or.tno==tno2) return
    if(pm_type_includes(context,tno,tno2,pm_type_incl_val)) return
    tv=pm_type_vect(context,tno)
    tv2=pm_type_vect(context,tno2)

    if(pm_tv_kind(tv)==pm_type_is_uninitialised.or.&
         pm_tv_kind(tv2)==pm_type_is_uninitialised) then
       typ1=tno
       typ2=tno2
       if(pm_tv_kind(tv)==pm_type_is_uninitialised) typ1=pm_tv_arg(tv,1)
       if(pm_tv_kind(tv2)==pm_type_is_uninitialised) typ2=pm_tv_arg(tv2,1)
       typ=pm_type_combine(context,tno,tno2,ok,added)
       if(ok) typ=pm_new_uninitialised_type(context,typ)
       return
    endif

    if(iand(pm_tv_flags(tv),pm_type_has_poly)==0.or.&
         iand(pm_tv_flags(tv2),pm_type_has_poly)==0) then
       ok=.false.
       return
    endif
    
    tk=pm_tv_kind(tv)
    tk2=pm_tv_kind(tv2)
    select case(tk2)
    case(pm_type_is_par_kind)
       if(tk==pm_type_is_par_kind) then
          typ=pm_type_add_mode(context,pm_type_combine(context,pm_tv_arg(tv,1),pm_tv_arg(tv2,1),ok,added),pm_tv_name(tv2))
       else
          typ=pm_type_add_mode(context,pm_type_combine(context,tno,pm_tv_arg(tv2,1),ok,added),pm_tv_name(tv2))
          return
       endif
    case(pm_type_is_user)
       typ=pm_type_combine(context,tno,pm_user_type_body(context,tno2),ok,added)
       return
    end select
    
    select case(tk)
    case(pm_type_is_par_kind)
       typ=pm_type_add_mode(context,pm_type_combine(context,&
            pm_tv_arg(tv,1),tno2,ok,added),pm_tv_name(tv))
    case(pm_type_is_user)
       typ=pm_type_combine(context,pm_user_type_body(context,tno),tno2,ok,added)
    case(pm_type_is_rec,pm_type_is_array,pm_type_is_tuple,pm_type_is_vtuple)
       if(tk/=tk2.or.pm_tv_name(tv)/=pm_tv_name(tv2)) then
          ok=.false.
          typ=-1
          return
       endif
       call remake(pm_tv_numargs(tv))
    case(pm_type_is_poly)
       if(tk/=tk2.or.pm_tv_name(tv)/=pm_tv_name(tv2)) then
          ok=.false.
          typ=-1
          return
       endif
       call combine_poly(pm_tv_numargs(tv),pm_tv_numargs(tv2))
!!$       write(*,*) 'Combined to: ',trim(pm_type_as_string(context,typ)),ok
    case default
       typ=-1
       ok=.false.
    end select
  contains

    recursive subroutine remake(n)
      integer,intent(in):: n
      integer,dimension(n+2):: a
      integer:: i
      a(1)=pm_tv_flags(tv)
      a(2)=pm_tv_name(tv)
      do i=1,n
         a(i+2)=pm_type_combine(context,pm_tv_arg(tv,i),pm_tv_arg(tv2,i),ok,added)
         if(.not.ok) then
            typ=-1
            return
         endif
      enddo
      typ=pm_new_type(context,a)
    end subroutine remake

    recursive subroutine combine_poly(n,n2)
      integer,intent(in):: n,n2
      integer,dimension(n+n2+2):: a
      logical,dimension(n):: mask
      integer:: i,j,m,recur,typ2
      logical:: elem_added,elem_ok

!!$      write(*,*) 'combine_poly',trim(pm_type_as_string(context,tno)),'<>',trim(pm_type_as_string(context,tno2))
!!$  
      a(1)=pm_type_new_poly
      a(2)=pm_tv_name(tv)
      do j=1,n
         a(2+j)=pm_tv_arg(tv,j)
      enddo

      ! Merge the two lists of concrete types
      m=2+n
      mask=.false.
      outer:do i=1,n2
         do j=1,n
            if(.not.mask(j)) then
!!$               write(*,*) 'Combining #',i,j
               typ2=pm_type_combine(context,a(2+j),pm_tv_arg(tv2,i),elem_ok,elem_added)
               if(elem_ok) then
                  added=added.or.elem_added
                  mask(j)=.true.
                  cycle outer
               endif
            endif
         enddo
         added=.true.
         m=m+1
         a(m)=pm_tv_arg(tv2,i)
      enddo outer

!!$      write(*,*) 'combine poly',m,added
!!$      
      ! Nothing added so just return
      if(.not.added) then
         typ=tno
         return
      endif
      
      ! Handle the merging of recursive poly types
      recur=-1
      do i=3,m
         if(iand(pm_type_flags(context,a(i)),&
              pm_type_has_poly+pm_type_is_recursive)/=0) then
            if(recur<0) then
               recur=pm_type_new_recursive_ref(context)
            endif
            a(i)=pm_type_move_recursive(context,a(i),recur)
         endif
      enddo

      ! Create new type
      typ=pm_new_type(context,a(1:m))
      if(recur>0) call pm_type_set_recursive_ref(context,recur,typ)
    end subroutine combine_poly
    
  end function pm_type_combine

  !=============================================
  ! Create new (incomplete) recursive reference
  !=============================================
  function pm_type_new_recursive_ref(context) result(tno)
    type(pm_context),pointer:: context
    integer:: tno
    integer,dimension(2):: arr
    arr(1)=pm_type_is_user+pm_type_is_recursive
    arr(2)=-pm_dict_size(context,context%tcache)
    tno=pm_new_basic_type(context,arr,&
         val=pm_fast_typeno(context,0))
  contains
    include 'ftypeno.inc'
  end function pm_type_new_recursive_ref

  !==============================================
  ! Make recursive reference point to given type
  !==============================================
  subroutine pm_type_set_recursive_ref(context,typ,tno)
    type(pm_context),pointer:: context
    integer,intent(in):: typ,tno
!!$    write(*,*) 'Set recursive',typ,tno
    call pm_type_set_val(context,typ,&
         pm_fast_typeno(context,tno))
  contains
    include 'ftypeno.inc'
  end subroutine pm_type_set_recursive_ref
  
  !================================================================
  ! Create a new type with with all fix values converted
  ! to base type and mode changed to new_mode
  !================================================================
  recursive function pm_type_move_recursive(context,tno,recur) result(typ)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,recur
    integer:: typ
    type(pm_ptr):: tv
    integer:: tk
    typ=tno
    if(tno<=0) return
    tv=pm_type_vect(context,tno)
    if(iand(pm_tv_flags(tv),pm_type_is_recursive)==0) return
    tk=pm_tv_kind(tv)
    select case(tk)
    case(pm_type_is_par_kind)
       typ=pm_type_add_mode(context,&
            pm_type_move_recursive(context,pm_tv_arg(tv,1),recur),pm_tv_name(tv))
    case(pm_type_is_user)
       typ=recur
    case(pm_type_is_rec,pm_type_is_array,pm_type_is_tuple,pm_type_is_vtuple)
       call remake(pm_tv_numargs(tv))
    end select
  contains
    recursive subroutine remake(n)
      integer,intent(in):: n
      integer,dimension(n+2):: a
      integer:: i
      a(1)=pm_tv_flags(tv)
      a(2)=pm_tv_name(tv)
      do i=1,n
         a(i+2)=pm_type_move_recursive(context,pm_tv_arg(tv,i),recur)
      enddo
      typ=pm_new_type(context,a)
    end subroutine remake
  end function pm_type_move_recursive


  !================================================================
  ! Create a new type with with all fix values converted
  ! to base type and mode changed to new_mode
  !================================================================
  recursive function pm_type_identify_recursive(context,tno,etyp,recur) result(typ)
    type(pm_context),pointer:: context
    integer,intent(in):: tno,etyp
    integer,intent(inout):: recur
    integer:: typ
    type(pm_ptr):: tv
    integer:: tk
    typ=tno
    if(tno<=0) return
    tv=pm_type_vect(context,tno)
    if(iand(pm_tv_flags(tv),pm_type_has_poly)==0) return
    tk=pm_tv_kind(tv)
    select case(tk)
    case(pm_type_is_par_kind)
       typ=pm_type_add_mode(context,&
            pm_type_identify_recursive(context,pm_tv_arg(tv,1),etyp,recur),pm_tv_name(tv))
    case(pm_type_is_rec,pm_type_is_array,pm_type_is_tuple,pm_type_is_vtuple)
       call remake(pm_tv_numargs(tv))
    case(pm_type_is_poly)
       if(pm_tv_name(tv)==etyp) then
          if(recur<0) then
             recur=pm_type_new_recursive_ref(context)
          endif
          typ=recur
!!$          write(*,*) 'Made recur',typ
       endif
    end select
  contains
    recursive subroutine remake(n)
      integer,intent(in):: n
      integer,dimension(n+2):: a
      integer:: i
      a(1)=pm_tv_flags(tv)
      a(2)=pm_tv_name(tv)
      do i=1,n
         a(i+2)=pm_type_identify_recursive(context,pm_tv_arg(tv,i),etyp,recur)
      enddo
      typ=pm_new_type(context,a)
!!$      write(*,*) 'remade to',typ,a
    end subroutine remake
  end function pm_type_identify_recursive
  
  !================================================================
  ! Strip all poly types in a given types down to just the constaint
  ! with no membership information
  !================================================================
  recursive function pm_type_strip_poly(context,tno) result(typ)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    integer:: typ
    type(pm_ptr):: tv
    integer:: tk,arr(2)
    typ=tno
    if(tno<=0) return
    tv=pm_type_vect(context,tno)
    if(iand(pm_tv_flags(tv),pm_type_has_poly)==0) return
    tk=pm_tv_kind(tv)
    select case(tk)
    case(pm_type_is_par_kind)
       typ=pm_type_add_mode(context,&
            pm_type_strip_poly(context,pm_tv_arg(tv,1)),pm_tv_name(tv))
    case(pm_type_is_user)
       typ=pm_user_type_body(context,tno)
    case(pm_type_is_rec,pm_type_is_array,pm_type_is_tuple,pm_type_is_vtuple)
       call remake(pm_tv_numargs(tv))
    case(pm_type_is_poly)
       arr(1)=pm_type_new_poly
       arr(2)=pm_tv_name(tv)
       typ=pm_new_type(context,arr)
    end select
  contains
    recursive subroutine remake(n)
      integer,intent(in):: n
      integer,dimension(n+2):: a
      integer:: i
      a(1)=pm_tv_flags(tv)
      a(2)=pm_tv_name(tv)
      do i=1,n
         a(i+2)=pm_type_strip_poly(context,pm_tv_arg(tv,i))
      enddo
      typ=pm_new_type(context,a)
    end subroutine remake
  end function pm_type_strip_poly

  !================================================================
  ! Get vector-of-integer representation of type
  !================================================================
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

  !================================================================
  ! Type kind from integer type vector
  !================================================================
  function pm_tv_kind(typ) result(k)
    type(pm_ptr),intent(in):: typ
    integer:: k
    k=iand(typ%data%i(typ%offset),pm_type_kind_mask)
  end function pm_tv_kind

  !================================================================
  ! Type flags from integer type vector
  !================================================================
  function pm_tv_flags(typ) result(k)
    type(pm_ptr),intent(in):: typ
    integer:: k
    k=typ%data%i(typ%offset)
  end function pm_tv_flags

  !================================================================
  ! Type name field from integer type vector
  !================================================================
  function pm_tv_name(typ) result(name)
    type(pm_ptr),intent(in):: typ
    integer:: name
    name=typ%data%i(typ%offset+1_pm_p)
  end function pm_tv_name

  !================================================================
  ! Type argument m from integer type vector
  !================================================================
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

  !================================================================
  ! Number of type arguments from integer type vector
  !================================================================
  function pm_tv_numargs(typ) result(num)
    type(pm_ptr),intent(in):: typ
    integer:: num
    num=pm_fast_esize(typ)-1
  contains
    include 'fesize.inc'
  end function pm_tv_numargs
  
  !================================================================
  ! Display type as user-readable string
  !================================================================
  function pm_type_as_string(context,tno) result(str)
    type(pm_context),pointer:: context
    integer,intent(in):: tno
    character(len=2048):: str
    integer:: n
    str=''
    if(tno==0) then
       str='any'
    else
       n=1
       call pm_type_to_string(context,tno,str,n)
    endif
  end function  pm_type_as_string

  recursive subroutine pm_type_to_string(context,typno,str,n,infix,noequiv,tuple_start)
    type(pm_context),pointer:: context
    integer,intent(in):: typno
    character(len=1024),intent(inout):: str
    integer,intent(inout):: n
    !logical,intent(in),optional:: distr,tuple,noequiv
    logical,intent(in),optional:: noequiv,infix
    integer,intent(in),optional:: tuple_start
    type(pm_ptr):: tv,tv2,nv,nv2
    integer:: tk,narg,tno2
    integer:: name,name2
    character(len=1),parameter:: open_brace = '{'
    character(len=1),parameter:: close_brace = '}'
    character(len=1),parameter:: open_square = '['
    character(len=1),parameter:: close_square = ']'
    integer:: i,j,m,istart,tno,tk2
    type(pm_ptr):: amps
    logical:: ok,isfix
    if(n>len(str)-10) return
    tno=typno
!!$    if(add_char('{'//trim(pm_int_as_string(tno))//'}')) return
    if(tno==0) then
       if(add_char('any')) return
       return
    endif
    if(tno<0) then
       if(add_char('*Internal-error(<0)*')) return
       return
    endif
    if(tno>pm_dict_size(context,context%heap%tcache)) then
       if(add_char('*Internal-error(>size)*')) return
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
          if(iand(pm_tv_flags(tv),pm_type_is_recursive)/=0) then
             if(add_char('{RECURSE}')) return
             return
          endif
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
          return
       endif
       name=pm_name_stem(context,name)
       if(name>=sym_dim1.and.name<=sym_dim7.and.narg==name-sym_dim1+1) then
          isfix=pm_type_includes(context,&
               pm_user_type_lookup_by_name(context,sym_pm_system,sym_fix_tuple),&
               tno,pm_type_incl_type)
          if(isfix) then
             if(add_char('fix')) return
          endif
          if(add_char('[')) return
          do i=1,narg-1
             call pm_type_to_string(context,pm_tv_arg(tv,i),str,n,isfix)
             if(add_char(',')) return
          enddo
          call pm_type_to_string(context,pm_tv_arg(tv,narg),str,n,isfix)
          if(add_char(']')) return
       elseif(name==sym_pm_ref_type) then
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       else
          if(name==sym_range.and.narg==2) then
             if(pm_tv_arg(tv,1)/=pm_tv_arg(tv,2)) then
                call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
                if(add_char('..')) return
                call pm_type_to_string(context,pm_tv_arg(tv,2),str,n,infix)
                return
             endif
          endif
          call pm_name_string(context,name,str(n:))
          n=len_trim(str)+1
          if(n>len(str)-10) return
          narg=pm_tv_numargs(tv)
          if(narg>0) then
             if(add_char('(')) return
             do i=1,narg-1
                call pm_type_to_string(context,pm_tv_arg(tv,i),str,n,infix)
                if(add_char(',')) return
             enddo
             call pm_type_to_string(context,pm_tv_arg(tv,narg),str,n,infix)
             if(add_char(')')) return
          endif
          if(tk==pm_type_is_user.and.(pm_opts%show_members)) then
             nv2=pm_dict_val(context,context%tcache,int(tno,pm_ln))
             tno2=int(nv2%offset)
             if(tno2>0.and.tno2<pm_dict_size(context,context%heap%tcache)) then
                tv=pm_type_vect(context,tno2)
                if(pm_tv_kind(tv)/=pm_type_is_basic) then
                   if(add_char(' {')) return
                   call pm_type_to_string(context,tno2,str,n,infix)
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
       if(iand(pm_tv_flags(tv),pm_type_is_list)/=0.and.pm_opts%show_details) then
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
          if(pm_fast_vkind(amps)==pm_int) then
             j=0
             do while(amps%data%i(amps%offset+j)<istart)
                if(j<pm_fast_esize(amps)) then
                   j=j+1
                else
                   exit
                endif
             enddo
             do i=istart,narg-1
                if(amps%data%i(amps%offset+j)==i) then
                   if(j<pm_fast_esize(amps)) j=j+1
                   if(add_char('&')) return
                endif
                call pm_type_to_string(context,pm_tv_arg(tv,i),str,n,infix)
                if(add_char(',')) return
             enddo
             if(amps%data%i(amps%offset+j)==narg) then
                if(add_char('&')) return
             endif
             call pm_type_to_string(context,pm_tv_arg(tv,narg),str,n,infix)
          else
             if(add_char('???'//trim(pm_int_as_string(pm_tv_name(tv))))) return
          endif
       else
          do i=istart,narg-1
             call pm_type_to_string(context,pm_tv_arg(tv,i),str,n,infix)
             if(add_char(',')) return
          enddo
          call pm_type_to_string(context,pm_tv_arg(tv,narg),str,n,infix)
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
          call pm_type_to_string(context,pm_tv_arg(tv,i),str,n,infix)
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
       if(pm_opts%show_details) then
          if(iand(pm_tv_flags(tv),pm_type_has_distributed)/=0) then
             if(add_char('*distr*')) return
          endif
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
             call pm_type_to_string(context,pm_tv_arg(tv,i),str,n,infix)
             if(add_char(',')) return
          enddo
          call pm_type_to_string(context,pm_tv_arg(tv,pm_tv_numargs(tv)),str,n,infix)
          if(add_char(')')) return
       else
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       endif
    case(pm_type_is_array)
       name=pm_tv_name(tv)
       if(name==sym_var) then
          if(add_char('varray(')) return
       elseif(name==sym_const) then
          if(add_char('farray(')) return
       elseif(name==0) then
          if(add_char('array(')) return
       else
          if(add_char('array'//trim(pm_int_as_string(name))//'(')) return
       endif
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       if(add_char(',')) return
       call pm_type_to_string(context,pm_tv_arg(tv,3),str,n,infix)
       if(add_char(')')) return
    case(pm_type_is_poly)
       if(add_char('*')) return
       call bracket(0,pm_type_is_includes,pm_type_is_all,pm_type_is_any,pm_type_is_except)
       if(pm_opts%show_details) then
          if(add_char('{')) return
          do i=1,pm_tv_numargs(tv)
             call pm_type_to_string(context,pm_tv_arg(tv,i),str,n,infix)
             if(i<pm_tv_numargs(tv)) then
                if(add_char(',')) return
             endif
          enddo
          if(add_char('}')) return
       endif
    case(pm_type_is_fix_value,pm_type_is_literal_value)
       isfix=.false.
       if(present(infix)) isfix=infix
       if(tk==pm_type_is_fix_value) then
          if(.not.isfix) then
             if(add_char('fix(')) return
          endif
       endif
       if(pm_tv_name(tv)==0) then
          if(tk==pm_type_is_literal_value) then
             if(add_char('literal(')) return
          endif
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,.true.)
          if(tk==pm_type_is_literal_value) then
             if(add_char(')')) return
          endif
       else
          if(pm_opts%show_details) then
             call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
             if(add_char('::')) return
          endif
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
       endif
       if(tk==pm_type_is_fix_value.and..not.isfix) then
          if(add_char(')')) return
       endif
    case(pm_type_is_fix)
       if(add_char('fix(')) return
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       if(add_char(')')) return
    case(pm_type_is_literal)
       if(add_char('literal(')) return
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       if(add_char(')')) return
    case(pm_type_is_except)
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       if(add_char(' except ')) return
       call pm_type_to_string(context,pm_tv_arg(tv,2),str,n,infix)
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
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       if(add_char(')')) return
    case(pm_type_is_has)
       if(add_char('.')) return
       call bracket(1,pm_type_is_includes,pm_type_is_all,pm_type_is_any,pm_type_is_except)
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
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
                call pm_type_to_string(context,pm_tv_arg(tv,i),str,n,infix)
                if(add_char(',')) return
             enddo
             call pm_type_to_string(context,pm_tv_arg(tv,pm_tv_numargs(tv)),str,n,infix)
             if(add_char('}')) return
          endif
       elseif(name==0) then
          if(add_char('proc')) return
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
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
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       endif
    case(pm_type_is_proc_sig)
       name=pm_tv_name(tv)
       if(name==sym_yield) then
          istart=num_comm_args+merge(4,3,iand(pm_tv_flags(tv),pm_type_is_yield)/=0)
       elseif(name/=sym_proc) then
          if(add_char(trim(pm_name_as_string(context,name)))) return
          istart=num_comm_args+1
       else
          istart=2
       endif
       if(pm_opts%show_hidden) then
          istart=1
       elseif(pm_opts%show_details) then
          do i=1,istart
             if(pm_type_arg(context,pm_tv_arg(tv,1),i)/=0) then
                if(add_char('^')) return
                istart=1
                exit
             endif
          enddo
!!$       elseif(istart>pm_type_numargs(context,pm_tv_arg(tv,1))) then
!!$          if(add_char('!!!'//trim(pm_int_as_string(istart))//'>'//&
!!$               trim(pm_int_as_string(pm_type_numargs(context,pm_tv_arg(tv,1)))))) return
!!$          istart=1
       endif
       tno2=pm_tv_arg(tv,1)
       if(istart>2) then
          call par_context_to_string(context,&
               pm_type_arg(context,tno2,2)/=0.and.pm_type_arg(context,tno2,2)/=pm_null,&
               pm_type_arg(context,tno2,2)==pm_null,&
               pm_type_arg(context,tno2,1),pm_type_arg(context,tno2,3),str,n)
       else
          call par_context_to_string(context,.false.,.false.,&
               pm_type_arg(context,tno2,1),0,str,n)
       endif
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,tuple_start=istart)
       if(add_char('->')) return
       call pm_type_to_string(context,pm_tv_arg(tv,2),str,n,infix)
       if(iand(pm_tv_flags(tv),pm_type_is_yield)/=0) then
          if(add_char(' yield ')) return
          call pm_type_to_string(context,&
               pm_type_arg(context,pm_type_arg(context,pm_tv_arg(tv,1),num_comm_args+1),1),str,n,infix)
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
       if(pm_opts%show_details) then
          if(add_char('^^(')) return
       endif
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       if(pm_opts%show_details) then
          if(add_char(')')) return
       endif
    case(pm_type_is_par_kind)
       name=pm_tv_name(tv)
       if(name>0) then
          if(add_char(trim(sym_names(name)))) return
          if(add_char(' ')) return
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       else
          name=-name
          j=0
          m=last_mode-first_mode
          do i=0,n
             if(iand(name,ishft(1,i))/=0) j=j+1
          enddo
          if(j>4) then
             if(add_char('~(')) return
             j=0
             do i=0,m
                if(iand(name,ishft(1,i))==0) then
                   if(j>0) then
                      if(add_char('|')) return
                   endif
                   if(add_char(trim(sym_names(first_mode+i)))) return
                   j=j+1
                endif
             enddo
             if(add_char(') ')) return
          else
             j=0
             do i=0,m
                if(iand(name,ishft(1,i))/=0) then
                   if(j>0) then
                      if(add_char('|')) return
                   endif
                   if(add_char(trim(sym_names(first_mode+i)))) return
                   j=j+1
                endif
             enddo
             if(add_char(' ')) return
          endif
          call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       endif
    case(pm_type_is_params)
       if(pm_opts%show_details) then
          if(add_char('[[=')) return
          if(add_char(trim(pm_int_as_string(pm_tv_name(tv))))) return
          if(add_char('::')) return
       endif
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,noequiv=.true.)
       if(pm_opts%show_details) then
          if(add_char('=]]')) return
       endif
    case(pm_type_is_param)
       if(pm_opts%show_details) then
          if(add_char('<<')) return
          if(add_char(trim(pm_int_as_string(pm_tv_name(tv))))) return
          if(add_char('--')) return
       endif
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,noequiv=.true.)
       if(pm_opts%show_details) then
          if(add_char('>>')) return
       endif
    case(pm_type_is_gated)
       if(pm_opts%show_details) then
          if(add_char('{')) return
          if(.not.test_gated_type(context,tno)) then
             if(add_char('~')) return
          endif
          call pm_type_to_string(context,pm_tv_arg(tv,pm_tv_numargs(tv)),str,n,noequiv=.true.)
          if(add_char(':')) return
          do i=1,pm_tv_numargs(tv)-2,2
             if(no_intersect(pm_tv_arg(tv,i),pm_tv_arg(tv,i+1))) then
                if(add_char('~')) return
             endif
             call pm_type_to_string(context,pm_tv_arg(tv,i),str,n,noequiv=.true.)
             if(add_char('^')) return
             call pm_type_to_string(context,pm_tv_arg(tv,i+1),str,n,noequiv=.true.)
             if(i<pm_tv_numargs(tv)-2) then
                if(add_char(',')) return
             endif
          enddo
          if(add_char('}')) return
       else
          if(test_gated_type(context,tno)) then
             call pm_type_to_string(context,pm_tv_arg(tv,pm_tv_numargs(tv)),str,n,noequiv=.true.)
          else
             if(add_char(' _ ')) return
          endif
       endif
    case(pm_type_is_type)
       if(add_char('<')) return
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
       if(add_char('>')) return
    case(pm_type_is_uninitialised)
       if(pm_opts%show_details) then
          if(add_char('UNINIT:')) return
       endif
       call pm_type_to_string(context,pm_tv_arg(tv,1),str,n,infix)
    case(pm_type_is_bottom)
       if(add_char(' _ ')) return
    case default
       if(add_char('*Internal-error(kind='//&
            trim(pm_int_as_string(tk))//')*')) return
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
    recursive subroutine bracket(i,tk1,tk2,tk3,tk4)
      integer,intent(in):: i,tk1,tk2,tk3,tk4
      integer:: tno,tk
      if(i==0) then
         tno=pm_tv_name(tv)
      else
         tno=pm_tv_arg(tv,i)
      endif
      tk=pm_type_kind(context,tno)
      if(tk==tk1.or.tk==tk2.or.tk==tk3.or.tk==tk4) then
         if(add_char('(')) return
         call pm_type_to_string(context,tno,str,n,infix)
         if(add_char(')')) return
      else
         call pm_type_to_string(context,tno,str,n,infix)
      endif
    end subroutine bracket

    recursive function show_equiv(name,templ,typ) result(ok)
      integer,intent(in):: name,templ,typ
      logical:: ok
      integer,dimension(pm_max_type_args):: params
      integer:: i,m,name2
      logical:: tuple
      params=-1

!!$      if(add_char('<%')) return
!!$      call pm_type_to_string(context,templ,str,n,infix)
!!$      if(add_char('%>')) return
!!$
!!$      ok=.false.
!!$      return

      ok=pm_type_extract_params(context,templ,typ,params)
      if(ok) then
         m=0
         do i=1,pm_max_type_args
            if(params(i)>0) m=i
         enddo
         name2=pm_name_stem(context,name)
         tuple=name2>=sym_dim1.and.name2<=sym_dim7
         if(name2==sym_range) then
            if(params(1)/=params(2)) then
               call pm_type_to_string(context,params(1),str,n,infix)
               if(add_char('..')) return
               call pm_type_to_string(context,params(2),str,n,infix)
            else
               if(add_char('range(')) return
               call pm_type_to_string(context,params(1),str,n,infix)
               if(add_char(')')) return
            endif
         else
            if(.not.tuple) then
               call pm_name_string(context,name,str(n:))
            endif
            n=len_trim(str)+1
            if(n>len(str)-10) return
            if(m>0) then
               isfix=tuple.and.iand(pm_tv_flags(tv),&
                    pm_type_has_storage+pm_type_has_fix)==pm_type_has_fix
               if(isfix) isfix=pm_type_includes(context,&
                    pm_user_type_lookup_by_name(context,sym_pm_system,sym_fix_tuple),&
                    tno,pm_type_incl_val)
               if(isfix) then
                  if(add_char('fix')) return
               endif
               if(add_char(merge('[','(',tuple))) return
               if(tuple) m=m-1
               do i=1,m
                  if(params(i)>0) then
                     call pm_type_to_string(context,params(i),str,n,isfix)
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
      endif
    end function show_equiv

    function no_intersect(tno1,tno2) result(ok)
      integer,intent(in):: tno1,tno2
      logical:: ok
      integer:: stack(max_user_nesting)
      ok=.not.pm_type_intersects(context,tno1,tno2,stack,1)
    end function no_intersect

  end subroutine pm_type_to_string


  recursive subroutine par_context_to_string(context,iscond,isuncond,ttyp,dtyp,string,n)
    type(pm_context),pointer:: context
    logical,intent(in):: iscond,isuncond
    integer,intent(in):: ttyp,dtyp
    character(len=*),intent(inout):: string
    integer,intent(inout)::n
    integer:: tno
    if(.not.iscond.and..not.isuncond.and.ttyp==0.and.dtyp==0) then
       return
    endif
    string(n:n)='['
    n=n+1
    if(ttyp/=0) then
       call pm_type_to_string(context,ttyp,string,n)
       string(n:n+1)='=>'
       n=n+2
    endif
    if(n+7>len(string)) return
    if(iscond) then
       string(n:n+3)='cond'
       n=n+4
    elseif(isuncond) then
       string(n:n+5)='uncond'
       n=n+6
    endif
    if(n+10>len(string)) return
    !call pm_type_to_string(context,dtyp,string,n)
    if(dtyp/=0) then
       if(string(n-1:n-1)=='d') then
          string(n:n)=':'
          n=n+1
       endif
       tno=pm_type_arg(context,dtyp,1)
       if(tno/=0) then
          call pm_type_to_string(context,tno,string,n)
       endif
       if(n+10>len(string)) return
       if(pm_type_numargs(context,dtyp)>1) then
          tno=pm_type_arg(context,dtyp,2)
          if(tno/=0) then
             string(n:n)=':'
             n=n+1
             call pm_type_to_string(context,tno,string,n)
          endif
       endif
    endif
    string(n:n)=']'
    n=n+1
  end subroutine par_context_to_string

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
