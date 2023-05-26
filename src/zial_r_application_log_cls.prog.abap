*&---------------------------------------------------------------------*
*& Include zial_r_application_log_cls
*&---------------------------------------------------------------------*
CLASS lcx_error DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL .
    METHODS: get_message RETURNING VALUE(r_result) TYPE bapiret2.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: message TYPE bapiret2.

ENDCLASS.



CLASS lcx_error IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    message = VALUE #( id         = sy-msgid
                       number     = sy-msgno
                       type       = sy-msgty
                       message_v1 = sy-msgv1
                       message_v2 = sy-msgv2
                       message_v3 = sy-msgv3
                       message_v4 = sy-msgv4 ).

  ENDMETHOD.


  METHOD get_message.
    r_result = me->message.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_application DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_init,
      on_value_req_msgid,
      at_selection_screen.

  PRIVATE SECTION.
    CONSTANTS: mc_log_context TYPE string VALUE 'ZIAL_S_LOG_CONTEXT'.

    CLASS-METHODS is_valid_msg
      IMPORTING
        is_data          TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS has_valid_obligatory_attr
      IMPORTING
        is_data          TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS has_valid_optional_attr
      IMPORTING
        is_data          TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
    CLASS-METHODS build_display_profile
      RETURNING
        VALUE(rs_display_profile) TYPE bal_s_prof.
    CLASS-METHODS show_appl_log.
    CLASS-METHODS exp_excel.
    CLASS-METHODS sel_appl_log
      RETURNING
        VALUE(rt_messages) TYPE zial_tt_balm
      RAISING
        lcx_error.
    CLASS-METHODS export_to_excel
      IMPORTING
        it_messages TYPE zial_tt_balm
      RAISING
        lcx_error.
    CLASS-METHODS filter_messages
      CHANGING
        ct_messages TYPE zial_tt_balm.

ENDCLASS.


CLASS lcl_application IMPLEMENTATION.

  METHOD on_init.

    SET PF-STATUS 'MAIN'.

    l_datfr = TEXT-006.
    l_datto = TEXT-007.

  ENDMETHOD.


  METHOD on_value_req_msgid.

    CALL FUNCTION 'RS_HELP_HANDLING'
      EXPORTING
        dynpfield                 = 'S_MSGID-LOW'
        dynpname                  = sy-dynnr
        object                    = 'MI'
        suppress_selection_screen = abap_true
        progname                  = sy-repid.

  ENDMETHOD.


  METHOD at_selection_screen.

    CASE sy-ucomm.
      WHEN 'ONLI'.
        show_appl_log( ).

      WHEN 'EXP_EXCEL'.
        exp_excel( ).

    ENDCASE.

  ENDMETHOD.


  METHOD is_valid_msg.

    CHECK is_data IS NOT INITIAL.

    " Obligatory attributes
    rv_result = has_valid_obligatory_attr( is_data ).
    CHECK rv_result EQ abap_true.

    " Optional attributes
    rv_result = has_valid_optional_attr( is_data ).

  ENDMETHOD.


  METHOD has_valid_obligatory_attr.

    FIELD-SYMBOLS: <lt_r_msg_attr> TYPE ANY TABLE.

    DO 3 TIMES.

      DATA(lv_index) = sy-index.
      CASE lv_index.
        WHEN 1.
          DATA(lv_attr) = CONV fieldname( 'MSGID' ).
          ASSIGN s_msgid[] TO <lt_r_msg_attr>.

        WHEN 2.
          lv_attr = 'MSGNO'.
          ASSIGN s_msgno[] TO <lt_r_msg_attr>.

        WHEN 3.
          lv_attr = 'MSGTY'.
          ASSIGN s_msgty[] TO <lt_r_msg_attr>.

      ENDCASE.

      IF <lt_r_msg_attr> IS INITIAL.
        rv_result = abap_true.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT lv_attr OF STRUCTURE is_data TO FIELD-SYMBOL(<lv_value>).
      IF <lv_value> NOT IN <lt_r_msg_attr>.
        rv_result = abap_false.
        EXIT.
      ELSE.
        rv_result = abap_true.
      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD has_valid_optional_attr.

    FIELD-SYMBOLS: <lt_r_msg_attr> TYPE ANY TABLE.

    rv_result = abap_true.

    ASSIGN s_msgv[] TO <lt_r_msg_attr>.
    CHECK <lt_r_msg_attr> IS NOT INITIAL.

    DO 4 TIMES.

      DATA(lv_index) = sy-index.

      DATA(lv_attr) = CONV fieldname( |MSGV{ lv_index }| ).
      ASSIGN COMPONENT lv_attr OF STRUCTURE is_data TO FIELD-SYMBOL(<lv_value>).
      IF <lv_value> IN <lt_r_msg_attr>.
        rv_result = abap_true.
        EXIT.
      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD build_display_profile.

    CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
      IMPORTING
        e_s_display_profile = rs_display_profile.

    rs_display_profile-show_all = abap_true.

    APPEND VALUE #( ref_table = mc_log_context
                    ref_field = 'PROGRAM'
                    col_pos   = 3
                    coltext   = TEXT-009
                    outputlen = 30 ) TO rs_display_profile-mess_fcat.
    APPEND VALUE #( ref_table = mc_log_context
                    ref_field = 'INCLUDE'
                    col_pos   = 4
                    coltext   = TEXT-010
                    outputlen = 30 ) TO rs_display_profile-mess_fcat.
    APPEND VALUE #( ref_table = mc_log_context
                    ref_field = 'LINE'
                    col_pos   = 5
                    coltext   = TEXT-011
                    outputlen = 4 ) TO rs_display_profile-mess_fcat.

    ASSIGN rs_display_profile-mess_fcat[ ref_field = 'MSGID' ] TO FIELD-SYMBOL(<ls_mess_fcat>).
    IF sy-subrc EQ 0.
      <ls_mess_fcat>-no_out  = abap_false.
      <ls_mess_fcat>-col_pos = 6.
    ENDIF.

    ASSIGN rs_display_profile-mess_fcat[ ref_field = 'MSGNO' ] TO <ls_mess_fcat>.
    IF sy-subrc EQ 0.
      <ls_mess_fcat>-no_out  = abap_false.
      <ls_mess_fcat>-col_pos = 7.
    ENDIF.

  ENDMETHOD.


  METHOD show_appl_log.

    DATA(lv_probclass) = '1'.
    CASE abap_true.
      WHEN p_class2.
        lv_probclass = '2'.

      WHEN p_class3.
        lv_probclass = '3'.

      WHEN p_class4.
        lv_probclass = '4'.

    ENDCASE.

    DATA(ls_log_filter) = VALUE bal_s_lfil( ).
    CALL FUNCTION 'BAL_FILTER_CREATE'
      EXPORTING
        i_object       = p_obj
        i_subobject    = p_subobj
        i_extnumber    = p_extnum
        i_aldate_from  = p_datfr
        i_aldate_to    = p_datto
        i_altime_from  = p_timfr
        i_altime_to    = p_timto
        i_probclass_to = lv_probclass
        i_alprog       = p_prog
        i_altcode      = p_tcode
        i_aluser       = p_user
      IMPORTING
        e_s_log_filter = ls_log_filter
      EXCEPTIONS
        OTHERS         = 0.

    DATA(lt_log_header) = VALUE balhdr_t( ).
    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter = ls_log_filter
      IMPORTING
        e_t_log_header = lt_log_header
      EXCEPTIONS
        OTHERS         = 1.

    CHECK sy-subrc EQ 0.

    DATA(lt_log_handle_loaded) = VALUE bal_t_logh( ).
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header = lt_log_header
      IMPORTING
        e_t_log_handle = lt_log_handle_loaded
      EXCEPTIONS
        OTHERS         = 0.

    DATA(lt_r_lognumber) = CONV rseloption( s_lognum[] ).

    CASE p_appl.
      WHEN abap_true.
        FIELD-SYMBOLS: <ls_log_data> TYPE bal_s_gdat,
                       <gt_messages> TYPE ANY TABLE.

        ASSIGN ('(SAPLSBAL)G') TO <ls_log_data>.

        DATA(lt_log_handle) = VALUE bal_t_logh( ).
        LOOP AT lt_log_handle_loaded ASSIGNING FIELD-SYMBOL(<lv_log_handle_loaded>).

          READ TABLE <ls_log_data>-t_ldat ASSIGNING FIELD-SYMBOL(<ls_ldat>)
              WITH KEY log_handle = <lv_log_handle_loaded>.
          CHECK <ls_ldat> IS ASSIGNED
            AND <ls_ldat>-admin-lognumber IN lt_r_lognumber.

          LOOP AT <ls_ldat>-messages-t_mhdr ASSIGNING FIELD-SYMBOL(<ls_mhdr>) GROUP BY <ls_mhdr>-category.

            ASSIGN COMPONENT 'CATEGORY-VAR' OF STRUCTURE <ls_mhdr> TO FIELD-SYMBOL(<gv_var>).
            ASSIGN COMPONENT 'CATEGORY-CON' OF STRUCTURE <ls_mhdr> TO FIELD-SYMBOL(<gv_con>).
            ASSIGN COMPONENT 'CATEGORY-SRC' OF STRUCTURE <ls_mhdr> TO FIELD-SYMBOL(<gv_src>).
            ASSIGN COMPONENT 'CATEGORY-PAR' OF STRUCTURE <ls_mhdr> TO FIELD-SYMBOL(<gv_par>).
            DATA(lv_table_name) = CONV fieldname( 'MESSAGES-T_' && <gv_var> && <gv_con> && <gv_src> && <gv_par> ).
            ASSIGN COMPONENT lv_table_name OF STRUCTURE <ls_ldat> TO <gt_messages>.

            LOOP AT <gt_messages> ASSIGNING FIELD-SYMBOL(<gs_messages>).

              DATA(lv_has_valid_msg) = is_valid_msg( <gs_messages> ).
              IF lv_has_valid_msg EQ abap_true.
                EXIT.
              ENDIF.

            ENDLOOP.

            IF lv_has_valid_msg EQ abap_true.
              EXIT.
            ENDIF.

          ENDLOOP.

          IF lv_has_valid_msg EQ abap_true.
            INSERT <lv_log_handle_loaded> INTO TABLE lt_log_handle.
          ENDIF.

        ENDLOOP.

        IF lt_log_handle IS INITIAL.
          MESSAGE i368(00) WITH TEXT-008.
          RETURN.
        ENDIF.

      WHEN abap_false.
        DATA(ls_msg_filter) = VALUE bal_s_mfil( msgid = s_msgid[]
                                                msgno = s_msgno[]
                                                msgty = s_msgty[] ).

    ENDCASE.

    DATA(ls_profile) = build_display_profile( ).

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = ls_profile
        i_t_log_handle      = lt_log_handle
        i_s_msg_filter      = ls_msg_filter
      EXCEPTIONS
        no_authority        = 1
        OTHERS              = 2.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD exp_excel.

    TRY.
        DATA(lt_messages) = sel_appl_log( ).

        filter_messages(
          CHANGING
            ct_messages = lt_messages ).

        export_to_excel( lt_messages ).

      CATCH lcx_error INTO DATA(lo_error).
        DATA(lv_msgty) = lo_error->get_message( )-type.
        MESSAGE lo_error->get_text( ) TYPE 'S' DISPLAY LIKE lv_msgty.

    ENDTRY.

  ENDMETHOD.


  METHOD sel_appl_log.

    DATA(lv_probclass) = '1'.
    CASE abap_true.
      WHEN p_class2.
        lv_probclass = '2'.

      WHEN p_class3.
        lv_probclass = '3'.

      WHEN p_class4.
        lv_probclass = '4'.

    ENDCASE.

    CALL FUNCTION 'APPL_LOG_READ_DB'
      EXPORTING
        object           = p_obj
        subobject        = p_subobj
        external_number  = p_extnum
        date_from        = p_datfr
        date_to          = p_datto
        time_from        = p_timfr
        time_to          = p_timto
        log_class        = lv_probclass
        program_name     = p_prog
        transaction_code = p_tcode
        user_id          = p_user
      TABLES
        messages         = rt_messages.

    IF rt_messages IS INITIAL.
      MESSAGE w001(00) WITH TEXT-901 INTO DATA(lv_msg).
      RAISE EXCEPTION TYPE lcx_error
        MESSAGE ID sy-msgid NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD filter_messages.

    DELETE ct_messages WHERE lognumber NOT IN s_lognum[]
                          OR msgty     NOT IN s_msgty[]
                          OR msgid     NOT IN s_msgid[]
                          OR msgno     NOT IN s_msgno[]
                          OR (     msgv1 NOT IN s_msgv[]
                               AND msgv2 NOT IN s_msgv[]
                               AND msgv3 NOT IN s_msgv[]
                               AND msgv4 NOT IN s_msgv[] ).

  ENDMETHOD.


  METHOD export_to_excel.

    CONSTANTS: lc_export_format TYPE string VALUE 'xlsx'.

    TYPES: BEGIN OF s_data_tab,
             msgtx TYPE msgtxt_long,
             msgid TYPE msgid,
             msgno TYPE msgno,
             msgty TYPE msgty,
             msgv1 TYPE msgv1,
             msgv2 TYPE msgv2,
             msgv3 TYPE msgv3,
             msgv4 TYPE msgv4,
             msgtm TYPE baltimstmp,
           END OF s_data_tab,
           t_data_tab TYPE STANDARD TABLE OF s_data_tab WITH DEFAULT KEY.

    DATA(lv_default_file_name) = |ApplLog_{ sy-datlo }_{ sy-timlo }|.
    DATA(lv_file_filter) = |{ TEXT-009 } (*.{ lc_export_format })\|*.{ lc_export_format }\|{ cl_gui_frontend_services=>filetype_all }|.

    DATA(lv_filename)    = VALUE string( ).
    DATA(lv_path)        = VALUE string( ).
    DATA(lv_fullpath)    = VALUE string( ).
    DATA(lv_user_action) = VALUE i( ).

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title              = |{ TEXT-008 }|
        default_extension         = lc_export_format
        default_file_name         = lv_default_file_name
        file_filter               = lv_file_filter
      CHANGING
        filename                  = lv_filename
        path                      = lv_path
        fullpath                  = lv_fullpath
        user_action               = lv_user_action
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        MESSAGE ID sy-msgid NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSEIF lv_user_action EQ cl_gui_frontend_services=>action_cancel.
      MESSAGE w001(00) WITH TEXT-900 INTO DATA(lv_msg).
      RAISE EXCEPTION TYPE lcx_error
        MESSAGE ID sy-msgid NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA(lt_data_tab) = VALUE t_data_tab( ).
    LOOP AT it_messages ASSIGNING FIELD-SYMBOL(<ls_message>).
      APPEND INITIAL LINE TO lt_data_tab ASSIGNING FIELD-SYMBOL(<ls_data_tab>).
      <ls_data_tab> = CORRESPONDING #( <ls_message> MAPPING msgtm = time_stmp ).
      MESSAGE ID <ls_message>-msgid TYPE <ls_message>-msgty NUMBER <ls_message>-msgno
        WITH <ls_message>-msgv1 <ls_message>-msgv2 <ls_message>-msgv3 <ls_message>-msgv4
        INTO <ls_data_tab>-msgtx.
    ENDLOOP.

    TRY.
        DATA: lo_salv_table TYPE REF TO cl_salv_table.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = lo_salv_table
          CHANGING
            t_table        = lt_data_tab ).

      CATCH cx_salv_msg INTO DATA(lo_error).
        RAISE EXCEPTION TYPE lcx_error
          MESSAGE ID lo_error->msgid NUMBER lo_error->msgno
          WITH lo_error->msgv1 lo_error->msgv2 lo_error->msgv3 lo_error->msgv4.

    ENDTRY.

    " data -> xstring (bytes)
    DATA(lv_xml_bytes) = lo_salv_table->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).

    " xstring (bytes) -> raw
    cl_scp_change_db=>xstr_to_xtab(
      EXPORTING
        im_xstring = lv_xml_bytes
      IMPORTING
        ex_size    = DATA(lv_size)
        ex_xtab    = DATA(lt_raw_data) ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = lv_fullpath
        filetype                  = 'BIN'
        bin_filesize              = lv_size
      CHANGING
        data_tab                  = lt_raw_data
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        MESSAGE ID sy-msgid NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    MESSAGE s001(00) WITH TEXT-100.

  ENDMETHOD.

ENDCLASS.
