"! <p class="shorttext synchronized">ABAP Unit Test: Template</p>
CLASS ltc_log_ewm DEFINITION FINAL
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

ENDCLASS.


CLASS ltc_log_ewm IMPLEMENTATION.

  METHOD class_setup.

    mo_aunit = zial_cl_aunit=>on_class_setup( iv_tdc_cnt    = mc_tdc_cnt
                                              iv_ign_errors = abap_true
                                              ir_tdc_data   = REF #( ms_tdc_data )
                                              it_sql_data   = VALUE #( ( tbl_name = 'ZIAL_T_DUMMY'
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

    CHECK mo_aunit->active( abap_true ).

    MESSAGE s499(sy) WITH 'LGNUM' 'HUID' 'RSRC' 'NLPLA' INTO DATA(lv_exp_msgtx) ##NEEDED.
    DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).
    DATA(ls_symsg_message) = zial_cl_log=>to_symsg( is_bapiret = ls_exp_message ).

    DATA(lo_api_message) = NEW /scwm/cl_api_message( ).
    lo_api_message->add_message( ls_symsg_message ).
    zial_cl_log=>get( )->log_api_message( lo_api_message ).

    DATA(lt_act_messages) = zial_cl_log=>get( )->get_messages( ).
    DELETE lt_act_messages TO 1.

    DATA(ls_act_message) = VALUE #( lt_act_messages[ id     = 'SY'
                                                     number = '499'
                                                     type   = 'S' ] OPTIONAL ).

    cl_abap_unit_assert=>assert_equals( exp = ls_exp_message
                                        act = ls_act_message ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_act_messages ) ).

  ENDMETHOD.


  METHOD t0002.

    CHECK mo_aunit->active( abap_true ).

    " IT_DATA as element with FNAM
    DATA(lt_lgpla) = VALUE /scwm/tt_lgpla( ( 'TEST1' )
                                           ( 'TEST2' ) ).
    DATA(lt_msgde) = zial_cl_log=>to_msgde( it_fnam = VALUE #( ( |LGPLA| ) )
                                            it_data = lt_lgpla ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_msgde ) ).

    " IT_DATA as element without FNAM
    lt_msgde = zial_cl_log=>to_msgde( it_data = lt_lgpla ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_msgde ) ).

    " IT_DATA as structure with FNAM
    DATA(lt_huident) = VALUE /scwm/tt_huident( ( lgnum = '0001' huident = '1234' )
                                               ( lgnum = '0002' huident = '5678' ) ).
    lt_msgde = zial_cl_log=>to_msgde( it_fnam = VALUE #( ( |HUIDENT| )
                                                         ( |LGNUM| ) )
                                      it_data = lt_huident ).
    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( lt_msgde ) ).

    " IT_DATA as structure without FNAM
    lt_msgde = zial_cl_log=>to_msgde( it_data = lt_huident ).
    cl_abap_unit_assert=>assert_equals( exp = 6
                                        act = lines( lt_msgde ) ).

    " IS_DATA without FNAM
    lt_msgde = zial_cl_log=>to_msgde( is_data = VALUE /scwm/s_huident( lgnum   = '0001'
                                                                       huident = '1234' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_msgde ) ).

    " IS_MSGDE
    lt_msgde = zial_cl_log=>to_msgde( is_msgde = VALUE #( fnam = 'TEST'
                                                          low  = '1234' ) ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lt_msgde ) ).

    " IT_DATA as range without FNAM
    lt_msgde = zial_cl_log=>to_msgde( iv_is_range = abap_true
                                      it_data     = VALUE rseloption( sign   = 'I'
                                                                      option = 'EQ'
                                                                      ( low = '1234' )
                                                                      ( low = '5678' ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_msgde ) ).

    " IT_DATA as range with FNAM
    lt_msgde = zial_cl_log=>to_msgde( iv_is_range = abap_true
                                      it_fnam     = VALUE #( ( |HUIDENT| ) )
                                      it_data     = VALUE rseloption( sign   = 'I'
                                                                      option = 'EQ'
                                                                      ( low = '1234' )
                                                                      ( low = '5678' ) ) ).
    cl_abap_unit_assert=>assert_equals( exp = 2
                                        act = lines( lt_msgde ) ).

  ENDMETHOD.


  METHOD t0003.

    CHECK mo_aunit->active( abap_true ).

    DATA(lo_cl_log) = NEW /scwm/cl_log( ).
    zial_cl_log=>get( )->log_saplog( lo_cl_log ).

  ENDMETHOD.


  METHOD t0004.

    CHECK mo_aunit->active( abap_true ).

    DATA(lo_api_message) = NEW /scwm/cl_api_message( ).
    zial_cl_log=>get( )->log_api_message( lo_api_message ).

  ENDMETHOD.


  METHOD t0005.

    CHECK mo_aunit->active( abap_true ).

    DATA(lt_dm_messages) = VALUE /scdl/dm_message_tab( ).
    zial_cl_log=>get( )->log_dm_messages( lt_dm_messages ).

  ENDMETHOD.

ENDCLASS.
