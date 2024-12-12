"! <p class="shorttext synchronized">ABAP Unit Test: Template</p>
CLASS ltc_log DEFINITION FINAL
  CREATE PUBLIC
  FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES t_dummy TYPE STANDARD TABLE OF dummy WITH EMPTY KEY.

    TYPES: BEGIN OF s_tdc_data,
             t_dummy TYPE t_dummy,
           END OF s_tdc_data.

    CONSTANTS mc_tdc_cnt TYPE etobj_name VALUE 'ZIAL_TDC_LOG_SAP'.

    CLASS-DATA mo_aunit    TYPE REF TO zial_cl_aunit.
    CLASS-DATA ms_tdc_data TYPE s_tdc_data.

    CLASS-METHODS class_setup
      RAISING cx_ecatt_tdc_access.

    CLASS-METHODS class_teardown.

    METHODS setup.
    METHODS teardown.

    METHODS t0001 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_log IMPLEMENTATION.

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

    CLEAR: sy-msgty,
           sy-msgid,
           sy-msgno.

    DATA(lo_log) = CAST zial_cl_log_sap( zial_cl_log=>get( ) ).
    DO zial_cl_log_conf=>mc_default-max_num_of_entries - 4 TIMES.
      INSERT VALUE #( ) INTO TABLE lo_log->mt_bapiret2.
    ENDDO.

    zial_cl_log=>get( )->log_message( iv_msgty = zial_cl_log=>mc_msgty-info
                                      iv_msgtx = |LOG_INFO| ).

    zial_cl_log=>get( )->save( ).

  ENDMETHOD.

ENDCLASS.
