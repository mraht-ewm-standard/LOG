*&---------------------------------------------------------------------*
*& Include zial_r_application_log_cls
*&---------------------------------------------------------------------*
CLASS lcl_application DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_init,
      on_value_req_msgid,
      at_start_of_selection.

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

ENDCLASS.


CLASS lcl_application IMPLEMENTATION.

  METHOD on_init.

    l_datfr = TEXT-006.
    l_datto = TEXT-007.
    p_datfr = sy-datum.
    p_datto = sy-datum.
    p_timto = '235959'.

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


  METHOD at_start_of_selection.

    CASE abap_true.
      WHEN p_class1.
        DATA(lv_probclass) = '1'.

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
          CHECK sy-subrc EQ 0
            AND <ls_ldat>-admin IN lt_r_lognumber.

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

ENDCLASS.
