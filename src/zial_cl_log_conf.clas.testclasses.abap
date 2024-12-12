"! <p class="shorttext synchronized">ABAP Unit Test: Template</p>
CLASS ltc_log_conf DEFINITION FINAL
  CREATE PUBLIC
  FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES t_dummy TYPE STANDARD TABLE OF dummy WITH EMPTY KEY.

    TYPES: BEGIN OF s_tdc_data,
             t_log_conf TYPE zial_tt_log_conf,
           END OF s_tdc_data.

    CONSTANTS mc_tdc_cnt TYPE etobj_name VALUE 'ZIAL_TDC_LOG_CONF'.

    CLASS-DATA mo_aunit                 TYPE REF TO zial_cl_aunit.
    CLASS-DATA ms_tdc_data              TYPE s_tdc_data.

    CLASS-DATA mo_osql_test_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS class_setup
      RAISING cx_ecatt_tdc_access.

    CLASS-METHODS class_teardown.

    METHODS setup.
    METHODS teardown.

    METHODS t0001 FOR TESTING RAISING cx_static_check.
    METHODS t0002 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_log_conf IMPLEMENTATION.

  METHOD class_setup.

    mo_aunit = zial_cl_aunit=>on_class_setup( iv_tdc_cnt  = mc_tdc_cnt
                                              ir_tdc_data = REF #( ms_tdc_data )
                                              it_sql_data = VALUE #( ( tbl_name = 'ZIAL_T_LOG_CONF'
                                                                       tbl_data = REF #( ms_tdc_data-t_log_conf ) ) ) ).

  ENDMETHOD.


  METHOD setup.

    mo_aunit->on_setup( ).

  ENDMETHOD.


  METHOD teardown.

    mo_aunit->on_teardown( ).

  ENDMETHOD.


  METHOD class_teardown.

    mo_aunit->on_class_teardown( ).

  ENDMETHOD.


  METHOD t0001.

    CHECK mo_aunit->active( abap_true ).

    LOOP AT ms_tdc_data-t_log_conf ASSIGNING FIELD-SYMBOL(<ls_log_conf_exp>).

      DATA(ls_log_conf_act) = zial_cl_log_conf=>get( iv_object    = <ls_log_conf_exp>-object
                                                     iv_subobject = <ls_log_conf_exp>-subobject
                                                     iv_uname     = <ls_log_conf_exp>-uname ).

      cl_abap_unit_assert=>assert_equals( exp = <ls_log_conf_exp>
                                          act = ls_log_conf_act ).

    ENDLOOP.

  ENDMETHOD.


  METHOD t0002.

    CHECK mo_aunit->active( abap_true ).

    DATA(ls_log_conf_act) = zial_cl_log_conf=>get( iv_object    = space
                                                   iv_subobject = space
                                                   iv_uname     = space ).

    cl_abap_unit_assert=>assert_initial( act = ls_log_conf_act ).

  ENDMETHOD.

ENDCLASS.
