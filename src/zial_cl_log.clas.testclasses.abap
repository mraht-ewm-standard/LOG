"! <p class="shorttext synchronized">ABAP Unit Test: Template</p>
CLASS ltc_log DEFINITION FINAL
  CREATE PUBLIC
  FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES t_dummy TYPE STANDARD TABLE OF dummy WITH EMPTY KEY.

    TYPES: BEGIN OF s_tdc_data,
             t_dummy TYPE t_dummy,
           END OF s_tdc_data.

    CONSTANTS mc_tdc_cnt TYPE etobj_name VALUE 'ZIAL_TDC_LOG'.

    CLASS-DATA mo_aunit    TYPE REF TO zial_cl_aunit.
    CLASS-DATA ms_tdc_data TYPE s_tdc_data.

    CLASS-METHODS class_setup
      RAISING cx_ecatt_tdc_access.

    CLASS-METHODS class_teardown.

    METHODS setup.
    METHODS teardown.

    METHODS t0001 FOR TESTING RAISING cx_static_check.
    METHODS t0002 FOR TESTING RAISING cx_static_check.
    METHODS t0003 FOR TESTING RAISING cx_static_check.
    METHODS t0004 FOR TESTING RAISING cx_static_check.
    METHODS t0005 FOR TESTING RAISING cx_static_check.
    METHODS t0006 FOR TESTING RAISING cx_static_check.
    METHODS t0007 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_log IMPLEMENTATION.

  METHOD class_setup.

    mo_aunit = zial_cl_aunit=>on_class_setup( iv_tdc_cnt  = mc_tdc_cnt
                                              ir_tdc_data = REF #( ms_tdc_data )
                                              it_sql_data = VALUE #( ( tbl_name = 'ZIAL_T_DUMMY'
                                                                       tbl_data = REF #( ms_tdc_data-t_dummy ) ) ) ).

  ENDMETHOD.


  METHOD setup.

    mo_aunit->on_setup( ).

    zial_cl_log=>free( ).

  ENDMETHOD.


  METHOD teardown.

    mo_aunit->on_teardown( ).

  ENDMETHOD.


  METHOD class_teardown.

    mo_aunit->on_class_teardown( ).

  ENDMETHOD.


  METHOD t0001.

    zial_cl_log=>get( )->log_info( iv_msgtx = |LOG_INFO| ).
    cl_abap_unit_assert=>assert_not_initial( zial_cl_log=>mt_log_stack ).

    zial_cl_log=>save( ).
    cl_abap_unit_assert=>assert_initial( zial_cl_log=>mt_log_stack ).

  ENDMETHOD.


  METHOD t0002.

    zial_cl_log=>get( )->log_info( iv_msgtx = |LOG_INFO| ).
    zial_cl_log=>get( )->log_error( iv_msgtx = |LOG_ERROR| ).
    zial_cl_log=>get( )->log_success( iv_msgtx = |LOG_SUCCESS| ).
    zial_cl_log=>get( )->log_warning( iv_msgtx = |LOG_WARNING| ).

    DATA(lt_act_messages) = zial_cl_log=>get( )->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 5
                                        act = lines( lt_act_messages ) ).

  ENDMETHOD.


  METHOD t0003.

    MESSAGE s499(sy) WITH 'LGNUM' 'HUID' 'RSRC' 'NLPLA' INTO DATA(lv_exp_msgtx) ##NEEDED.
    DATA(lt_exp_messages) = zial_cl_log=>to_bapirets( ).
    zial_cl_log=>get( )->log_bapiret( lt_exp_messages ).

    DATA(lt_act_messages) = zial_cl_log=>get( )->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_act_messages ) ).

  ENDMETHOD.


  METHOD t0004.

    MESSAGE s499(sy) WITH 'LGNUM' 'HUID' 'RSRC' 'NLPLA' INTO DATA(lv_exp_msgtx) ##NEEDED.
    zial_cl_log=>get( )->log_message( it_msgde = VALUE #( ( fnam = 'TEST' low = '1234' ) ) ).

    DATA(lt_act_messages) = zial_cl_log=>get( )->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_act_messages ) ).

  ENDMETHOD.


  METHOD t0005.

    DATA(lv_act_components) = zial_cl_log=>get_components_from_msgde( VALUE #( ( fnam = 'LGNUM' )
                                                                               ( fnam = 'HUID' )
                                                                               ( fnam = 'RSRC' )
                                                                               ( fnam = 'NLPLA' ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = |LGNUM, HUID, RSRC, NLPLA|
                                        act = lv_act_components ).

  ENDMETHOD.


  METHOD t0006.

    MESSAGE s499(sy) WITH 'LGNUM' 'HUID' 'RSRC' 'NLPLA' INTO DATA(lv_exp_msgtx) ##NEEDED.
    DATA(lv_msgtx) = zial_cl_log=>to_string( ).

    cl_abap_unit_assert=>assert_equals( exp = |LGNUM HUID RSRC NLPLA|
                                        act = lv_msgtx ).

  ENDMETHOD.


  METHOD t0007.

    DATA(ls_exp_message) = zial_cl_log=>to_bapiret( iv_msgtx = |&1 &2 &3 &4|
                                                    iv_msgv1 = 'LGNUM'
                                                    iv_msgv2 = 'HUID'
                                                    iv_msgv3 = 'RSRC'
                                                    iv_msgv4 = 'NLPLA' ).

    cl_abap_unit_assert=>assert_not_initial( ls_exp_message ).

  ENDMETHOD.

ENDCLASS.
