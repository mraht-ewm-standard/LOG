"! <p class="shorttext synchronized">Logging: General Log</p>
CLASS zial_cl_log_sap DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.
    TYPES t_spar TYPE STANDARD TABLE OF spar WITH DEFAULT KEY.

    CONSTANTS mc_class_name TYPE classname VALUE 'ZIAL_CL_LOG_SAP' ##NO_TEXT.
    CONSTANTS:
      BEGIN OF mc_msgde_callback_type,
        form     TYPE baluet VALUE ' ',
        function TYPE baluet VALUE 'F',
      END OF mc_msgde_callback_type.
    CONSTANTS:
      BEGIN OF mc_msgde_callback,
        baluef      TYPE baluef VALUE 'ZIAL_FM_LOG_CALLBACK',
        baluep      TYPE baluep VALUE 'ZIAL_R_BS_LOG_CALLBACK',
        baluep_form TYPE baluef VALUE 'ON_CLICK_MSG_DETAIL',
      END OF mc_msgde_callback.

    CLASS-METHODS on_log_callback
      IMPORTING it_params TYPE t_spar.

    "! Initialize log instance
    "!
    "! @parameter iv_object      | Log object
    "! @parameter iv_subobject   | Log subobject
    "! @parameter iv_extnumber   | External number / description for a log
    "! @parameter it_extnumber   | External number elements
    "! @parameter iv_log_part_id | ID for the new log as part of another log
    METHODS constructor
      IMPORTING iv_object      TYPE balobj_d  DEFAULT zial_cl_log=>mc_default-log_object
                iv_subobject   TYPE balsubobj DEFAULT zial_cl_log=>mc_default-log_subobject
                iv_extnumber   TYPE balnrext  OPTIONAL
                it_extnumber   TYPE stringtab OPTIONAL
                iv_log_part_id TYPE i         DEFAULT 0.

    METHODS get_log_handle
      RETURNING VALUE(rv_log_handle) TYPE balloghndl.

    "! Get all logged messages
    "!
    "! @parameter rt_messages | BAPI messages
    METHODS get_messages
      RETURNING VALUE(rt_messages) TYPE bapirettab.

    "! Log a message with optionally message details
    "!
    "! @parameter iv_msgty | Message type
    "! @parameter iv_msgtx | Message text
    "! @parameter iv_msgid | Message ID
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @parameter it_msgde | Message details
    METHODS log_message
      IMPORTING iv_msgty TYPE symsgty                 DEFAULT sy-msgty
                iv_msgtx TYPE bapi_msg                OPTIONAL
                iv_msgid TYPE symsgid                 DEFAULT sy-msgid
                iv_msgno TYPE symsgno                 DEFAULT sy-msgno
                iv_msgv1 TYPE symsgv                  DEFAULT sy-msgv1
                iv_msgv2 TYPE symsgv                  DEFAULT sy-msgv2
                iv_msgv3 TYPE symsgv                  DEFAULT sy-msgv3
                iv_msgv4 TYPE symsgv                  DEFAULT sy-msgv4
                it_msgde TYPE rsra_t_alert_definition OPTIONAL.

    "! Log exception
    "!
    "! @parameter io_exception | Exception object
    METHODS log_exception
      IMPORTING io_exception TYPE REF TO cx_root.

    "! Log symsg messages
    "!
    "! @parameter is_symsg | SAP system message structure
    METHODS log_symsg
      IMPORTING is_symsg TYPE symsg.

    "! Log a table of bapi messages
    "!
    "! @parameter it_bapiret | BAPI messages
    METHODS log_bapiret
      IMPORTING it_bapiret TYPE bapirettab.

    "! Log a horizontal line
    "!
    METHODS log_line.
    "! Log name of development object which called the function to be logged
    "!
    METHODS log_caller.

    METHODS has_error
      RETURNING VALUE(rv_result) TYPE abap_bool.

    "! Save log to application log and optionally close log instance
    "!
    "! @parameter iv_finalize | Finalize/close log? (Y/N)
    METHODS save
      IMPORTING iv_finalize TYPE abap_bool DEFAULT abap_true.

    METHODS is_log_part
      RETURNING VALUE(rv_result) TYPE abap_bool.

  PROTECTED SECTION.
    TYPES: BEGIN OF s_processing_control,
             has_error   TYPE abap_bool,
             save_error  TYPE abap_bool,
             log_part_id TYPE i,
           END OF s_processing_control.

    DATA ms_processing_control TYPE s_processing_control.

    DATA mv_process_bgn        TYPE timestampl.
    DATA mv_process_end        TYPE timestampl.
    DATA mv_caller             TYPE c LENGTH 200.
    DATA mt_bapiret2           TYPE bapiret2_t.

    DATA ms_log                TYPE zial_s_log_msg.

    DATA mv_msg_param_id       TYPE zial_cl_log=>v_message_param_id.
    DATA ms_msg_details        TYPE zial_s_msg_details.
    DATA mt_msg_details        TYPE zial_tt_msg_details.

    CLASS-METHODS error_handling
      IMPORTING iv_process        TYPE char4
                iv_subrc          TYPE sysubrc
                is_log_msg        TYPE zial_s_log_msg
      RETURNING VALUE(rt_bapiret) TYPE bapirettab.

    "! Initialize log
    "!
    "! @parameter iv_extnumber | External number / description for a log
    "! @parameter it_extnumber | External number elements
    METHODS init
      IMPORTING iv_extnumber TYPE balnrext
                it_extnumber TYPE stringtab.

    METHODS has_msg_added_to_log
      IMPORTING is_msg_handle    TYPE balmsghndl OPTIONAL
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS set_extnumber
      IMPORTING iv_extnumber TYPE balnrext  OPTIONAL
                it_extnumber TYPE stringtab OPTIONAL.

    METHODS handle_full_log
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS handle_error
      IMPORTING iv_process TYPE char4
                iv_subrc   TYPE sysubrc.

    METHODS set_priority.

    METHODS set_content
      IMPORTING iv_msgty              TYPE symsgty
                iv_msgtx              TYPE bapi_msg
                iv_msgid              TYPE symsgid
                iv_msgno              TYPE symsgno
                iv_msgv1              TYPE symsgv
                iv_msgv2              TYPE symsgv
                iv_msgv3              TYPE symsgv
                iv_msgv4              TYPE symsgv
      RETURNING VALUE(rs_log_message) TYPE zial_s_log_msg.

    METHODS set_log_part_id
      IMPORTING iv_log_part_id TYPE i.

    METHODS create_message
      IMPORTING iv_msgty        TYPE symsgty
                iv_msgtx        TYPE bapi_msg                OPTIONAL
                iv_msgid        TYPE symsgid                 OPTIONAL
                iv_msgno        TYPE symsgno                 OPTIONAL
                iv_msgv1        TYPE symsgv                  OPTIONAL
                iv_msgv2        TYPE symsgv                  OPTIONAL
                iv_msgv3        TYPE symsgv                  OPTIONAL
                iv_msgv4        TYPE symsgv                  OPTIONAL
                it_msgde        TYPE rsra_t_alert_definition OPTIONAL
                iv_is_dummy_msg TYPE abap_bool               DEFAULT abap_false.

    METHODS add_timestamp
      RETURNING VALUE(rv_time) TYPE symsgv.

    METHODS set_detail_level.
    METHODS set_expiry_date.

    METHODS set_context.

    METHODS set_callstack
      CHANGING ct_msgde TYPE rsra_t_alert_definition.

    METHODS add_msg_by_message_text
      IMPORTING is_log_msg           TYPE zial_s_log_msg
      RETURNING VALUE(rs_msg_handle) TYPE balmsghndl.

    METHODS add_msg_by_message_object
      IMPORTING is_log_msg           TYPE zial_s_log_msg
      RETURNING VALUE(rs_msg_handle) TYPE balmsghndl.

    METHODS log_duration.
    METHODS det_caller.

    METHODS save_log
      IMPORTING iv_log_handle TYPE balloghndl.

    METHODS set_detail
      IMPORTING it_msgde TYPE rsra_t_alert_definition.

    METHODS save_msgde
      IMPORTING it_new_lognumbers TYPE bal_t_lgnm.

    METHODS create_log
      IMPORTING is_log_header        TYPE bal_s_log
      RETURNING VALUE(rv_log_handle) TYPE balloghndl.

    METHODS create_and_save_error_log
      IMPORTING iv_process TYPE char4
                iv_subrc   TYPE sysubrc
                is_log_msg TYPE zial_s_log_msg.

    METHODS is_valid_detail_level
      IMPORTING iv_msgty         TYPE msgty
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS add_msg_to_appl_log.

  PRIVATE SECTION.
    METHODS is_log_getting_full
      RETURNING VALUE(rv_result) TYPE abap_bool.

ENDCLASS.


CLASS zial_cl_log_sap IMPLEMENTATION.

  METHOD set_callstack.

    lcl_session=>get_callstack( IMPORTING et_callstack = DATA(lt_callstack) ).
    DELETE lt_callstack WHERE mainprogram CS mc_class_name.

    DATA(lv_line) = repeat( val = '-'
                            occ = 80 ).
    INSERT VALUE #( low = lv_line ) INTO TABLE ct_msgde.
    INSERT LINES OF VALUE rsra_t_alert_definition( FOR <s_callstack> IN lt_callstack
                                                   ( low = |{ <s_callstack>-mainprogram }=>| &&
                                                           |{ <s_callstack>-event }, { TEXT-002 } | &&
                                                           |{ <s_callstack>-line }| ) ) INTO TABLE ct_msgde.

  ENDMETHOD.


  METHOD is_valid_detail_level.

    CHECK ms_log-hdr-detail_level GT 0.

    DATA(lv_detail_level) = SWITCH #( iv_msgty
                                      WHEN zial_cl_log=>mc_msgty-info    THEN zial_cl_log=>mc_detail_level-info
                                      WHEN zial_cl_log=>mc_msgty-success THEN zial_cl_log=>mc_detail_level-success
                                      WHEN zial_cl_log=>mc_msgty-warning THEN zial_cl_log=>mc_detail_level-warning
                                      WHEN zial_cl_log=>mc_msgty-error   THEN zial_cl_log=>mc_detail_level-error ).

    CHECK lv_detail_level LE ms_log-hdr-detail_level.
    rv_result = abap_true.

  ENDMETHOD.


  METHOD set_context.

    lcl_session=>get_context( IMPORTING ev_program   = DATA(lv_program)
                                        ev_blockname = DATA(lv_include)
                                        ev_line      = DATA(lv_line) ).

    ms_log-msg-context = VALUE #( value   = VALUE zial_s_log_context( program = lv_program
                                                                      include = lv_include
                                                                      line    = lv_line )
                                  tabname = zial_cl_log=>mc_log_context_struct ).

  ENDMETHOD.


  METHOD set_detail.

    CHECK it_msgde IS NOT INITIAL.

    " Add message identifier, s. include LSBAL_DETAILF02 (example: SBAL_CALLBACK)
    ms_log-msg-params-callback = VALUE #( userexitf = mc_msgde_callback-baluef
                                          userexitt = mc_msgde_callback_type-function ).

    mv_msg_param_id = mv_msg_param_id + 1.
    INSERT VALUE #( parname  = zial_cl_log=>mc_msg_ident
                    parvalue = mv_msg_param_id ) INTO TABLE ms_log-msg-params-t_par.

    INSERT VALUE #( v_id              = mv_msg_param_id
                    t_input_parameter = it_msgde ) INTO TABLE mt_msg_details.

  ENDMETHOD.


  METHOD add_msg_by_message_object.

    DATA(ls_bal_msg) = CORRESPONDING bal_s_msg( is_log_msg-msg ).
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING  i_log_handle     = is_log_msg-hdr-log_handle
                 i_s_msg          = ls_bal_msg
      IMPORTING  e_s_msg_handle   = rs_msg_handle
      EXCEPTIONS log_not_found    = 1
                 msg_inconsistent = 2
                 log_is_full      = 3
                 OTHERS           = 4.

    IF sy-subrc NE 0.
      handle_error( iv_process = zial_cl_log=>mc_log_process-add_msg
                    iv_subrc   = sy-subrc ).
    ENDIF.

  ENDMETHOD.


  METHOD add_msg_by_message_text.

    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING  i_log_handle     = is_log_msg-hdr-log_handle
                 i_msgty          = is_log_msg-msg-msgty
                 i_probclass      = is_log_msg-msg-probclass
                 i_s_context      = is_log_msg-msg-context
                 i_text           = is_log_msg-msg-msgtx
                 i_s_params       = is_log_msg-msg-params
      IMPORTING  e_s_msg_handle   = rs_msg_handle
      EXCEPTIONS log_not_found    = 1
                 msg_inconsistent = 2
                 log_is_full      = 3
                 OTHERS           = 4.

    IF sy-subrc NE 0.
      handle_error( iv_process = zial_cl_log=>mc_log_process-add_msg
                    iv_subrc   = sy-subrc ).
    ENDIF.

  ENDMETHOD.


  METHOD has_msg_added_to_log.

    IF is_msg_handle IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING  i_s_msg_handle = is_msg_handle
        EXCEPTIONS log_not_found  = 1
                   msg_not_found  = 2
                   OTHERS         = 3.
      CHECK sy-subrc EQ 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD add_timestamp.

    DATA lv_timestamp TYPE timestampl.

    GET TIME STAMP FIELD lv_timestamp.
    rv_time = |{ lv_timestamp }|.

    " DD.MM.YYYY, hh:mm:ss.ms
    rv_time = |{ rv_time+6(2) }.{ rv_time+4(2) }.{ rv_time(4) }, | &
              |{ rv_time+8(2) }:{ rv_time+10(2) }:{ rv_time+12(2) }{ rv_time+14 }|.

  ENDMETHOD.


  METHOD set_extnumber.

    DATA(lt_extnumber) = VALUE stringtab( ).

    IF iv_extnumber IS NOT INITIAL.

      INSERT CONV #( iv_extnumber ) INTO TABLE lt_extnumber.

    ELSEIF it_extnumber IS NOT INITIAL.

      lt_extnumber = it_extnumber.

    ENDIF.

    LOOP AT lt_extnumber ASSIGNING FIELD-SYMBOL(<lv_extnumber>).

      CASE sy-tabix.
        WHEN 1.
          ms_log-hdr-extnumber = |{ <lv_extnumber> }|.

        WHEN OTHERS.
          ms_log-hdr-extnumber = |{ ms_log-hdr-extnumber } { <lv_extnumber> }|.

      ENDCASE.

    ENDLOOP.

    IF lt_extnumber IS INITIAL.

      SELECT SINGLE subobjtxt FROM balsubt
        INTO @ms_log-hdr-extnumber
        WHERE spras     EQ @sy-langu
          AND object    EQ @ms_log-hdr-object
          AND subobject EQ @ms_log-hdr-subobject.

    ENDIF.

  ENDMETHOD.


  METHOD set_detail_level.

    DATA(ls_log_conf) = zial_cl_log_conf=>get( iv_object    = ms_log-hdr-object
                                               iv_subobject = ms_log-hdr-subobject
                                               iv_uname     = sy-uname ).
    IF ls_log_conf IS INITIAL.
      ms_log-hdr-detail_level = zial_cl_log_conf=>mc_default-detail_level.
    ELSE.
      ms_log-hdr-detail_level = ls_log_conf-detail_level.
    ENDIF.

  ENDMETHOD.


  METHOD set_expiry_date.

    DATA(lv_validity_period) = zial_cl_log_conf=>get( iv_object    = ms_log-hdr-object
                                                      iv_subobject = ms_log-hdr-subobject
                                                      iv_uname     = sy-uname )-validity_period.
    IF lv_validity_period LE 0.
      lv_validity_period = zial_cl_log_conf=>mc_default-validity_period.
    ENDIF.

    IF ms_log-hdr-aldate IS INITIAL.
      ms_log-hdr-aldate = sy-datum.
    ENDIF.
    ms_log-hdr-validity_period = lv_validity_period.
    ms_log-hdr-aldate_del      = ms_log-hdr-aldate + lv_validity_period.

  ENDMETHOD.


  METHOD create_message.

    set_content( iv_msgty = iv_msgty
                 iv_msgtx = iv_msgtx
                 iv_msgid = iv_msgid
                 iv_msgno = iv_msgno
                 iv_msgv1 = iv_msgv1
                 iv_msgv2 = iv_msgv2
                 iv_msgv3 = iv_msgv3
                 iv_msgv4 = iv_msgv4 ).

    IF iv_is_dummy_msg EQ abap_false.

      set_priority( ).

      set_context( ).

      DATA(lt_msgde) = it_msgde.
      set_callstack( CHANGING ct_msgde = lt_msgde ).

      set_detail( it_msgde ).

    ENDIF.

    add_msg_to_appl_log( ).

  ENDMETHOD.


  METHOD det_caller.

    CHECK mv_caller CO ' _0'.

    lcl_session=>get_callstack( IMPORTING ev_object  = DATA(lv_object)
                                          ev_routine = DATA(lv_routine) ).

    IF lv_object IS NOT INITIAL.
      mv_caller = lv_object.
    ELSE.
      RETURN.
    ENDIF.

    IF lv_routine IS NOT INITIAL.
      mv_caller = |{ mv_caller }=>{ lv_routine }|.
    ENDIF.

  ENDMETHOD.


  METHOD handle_full_log.

    CHECK is_log_getting_full( ) EQ abap_true.

    DATA(ls_log) = ms_log.
    ms_processing_control-log_part_id = zial_cl_log=>get_next_log_part_id( ).

    zial_cl_log=>save( ).

    DATA(lo_log) = CAST zial_cl_log_sap( zial_cl_log=>create( iv_object    = ls_log-hdr-object
                                                              iv_subobject = ls_log-hdr-subobject
                                                              iv_extnumber = ls_log-hdr-extnumber ) ).
    lo_log->set_log_part_id( ms_processing_control-log_part_id + 1 ).

    lo_log->log_message( iv_msgty = ls_log-msg-msgty
                         iv_msgtx = ls_log-msg-msgtx
                         iv_msgid = ls_log-msg-msgid
                         iv_msgno = ls_log-msg-msgno
                         iv_msgv1 = ls_log-msg-msgv1
                         iv_msgv2 = ls_log-msg-msgv2
                         iv_msgv3 = ls_log-msg-msgv3
                         iv_msgv4 = ls_log-msg-msgv4
                         it_msgde = ls_log-msg-msgde ).

    lo_log->set_log_part_id( 0 ).

    rv_result = abap_true.

  ENDMETHOD.


  METHOD set_log_part_id.

    ms_processing_control-log_part_id = iv_log_part_id.

    IF ms_processing_control-log_part_id GT 0.
      MESSAGE s020(zial_log) WITH zial_cl_log_conf=>mc_default-max_num_of_entries
                                  ms_processing_control-log_part_id INTO DATA(lv_msgtx) ##NEEDED.
      log_message( ).
    ENDIF.

  ENDMETHOD.


  METHOD handle_error.

    " Ensures that error handling is only executed once for each error to avoid endless loop
    CHECK ms_processing_control-has_error  EQ abap_false
      AND ms_processing_control-save_error EQ abap_false.

    ms_processing_control-has_error = abap_true.

    DATA(ls_log) = ms_log.

    " Try to add error messages regarding failed logging to old log
    MESSAGE e001(zial_log) INTO DATA(lv_msgtx) ##NEEDED.
    log_message( ).

    zial_cl_log=>save( ).

    create_and_save_error_log( iv_process = iv_process
                               iv_subrc   = iv_subrc
                               is_log_msg = ls_log ).

    " Error handling could be executed again
    ms_processing_control-has_error  = abap_false.
    ms_processing_control-save_error = abap_false.

  ENDMETHOD.


  METHOD error_handling.

    MESSAGE e017(zial_log) WITH iv_process INTO DATA(lv_msgtx) ##NEEDED.
    INSERT zial_cl_log=>to_bapiret( ) INTO TABLE rt_bapiret.

    " Log general log data
    DATA(lv_msg_txt_gen) = CONV bapi_msg( |OBJECT: { is_log_msg-hdr-object }; | &&
                                          |SUBOBJ: { is_log_msg-hdr-subobject }; | &&
                                          |EXTNUM: { is_log_msg-hdr-extnumber }; | &&
                                          |ALDDEL: { is_log_msg-hdr-aldate_del }| ).
    INSERT zial_cl_log=>to_bapiret( iv_msgty = zial_cl_log=>mc_msgty-error
                                    iv_msgtx = lv_msg_txt_gen ) INTO TABLE rt_bapiret.

    CASE iv_process.
      WHEN zial_cl_log=>mc_log_process-init.
        CASE iv_subrc.
          WHEN 1. MESSAGE e006(zial_log) WITH iv_subrc INTO lv_msgtx.
          WHEN OTHERS. MESSAGE e007(zial_log) WITH iv_subrc INTO lv_msgtx.
        ENDCASE.

      WHEN zial_cl_log=>mc_log_process-save.
        CASE iv_subrc.
          WHEN 1. MESSAGE e008(zial_log) WITH iv_subrc INTO lv_msgtx.
          WHEN 2. MESSAGE e009(zial_log) WITH iv_subrc INTO lv_msgtx.
          WHEN 3. MESSAGE e010(zial_log) WITH iv_subrc INTO lv_msgtx.
          WHEN OTHERS. MESSAGE e011(zial_log) WITH iv_subrc INTO lv_msgtx.
        ENDCASE.

      WHEN OTHERS.
        CASE sy-subrc.
          WHEN 1. MESSAGE e008(zial_log) WITH iv_subrc INTO lv_msgtx.
          WHEN 2. MESSAGE e012(zial_log) WITH iv_subrc INTO lv_msgtx.
          WHEN 3. MESSAGE e013(zial_log) WITH iv_subrc INTO lv_msgtx.
          WHEN OTHERS. MESSAGE e014(zial_log) WITH iv_subrc INTO lv_msgtx.
        ENDCASE.

    ENDCASE.

    INSERT zial_cl_log=>to_bapiret( ) INTO TABLE rt_bapiret.

    " Log process-specific log data
    CASE iv_process.
      WHEN zial_cl_log=>mc_log_process-init.
        " Nothing to log

      WHEN zial_cl_log=>mc_log_process-create.
        IF is_log_msg-msg-msgtx CN ' _0'.

          MESSAGE e015(zial_log) WITH is_log_msg-msg-msgty INTO lv_msgtx.
          INSERT zial_cl_log=>to_bapiret( ) INTO TABLE rt_bapiret.

        ELSEIF is_log_msg-msg-msgno CN ' _0'.

          MESSAGE e003(zial_log) WITH is_log_msg-msg-msgno
                                      is_log_msg-msg-msgid INTO lv_msgtx.
          INSERT zial_cl_log=>to_bapiret( ) INTO TABLE rt_bapiret.

          MESSAGE e004(zial_log) WITH is_log_msg-msg-msgv1
                                      is_log_msg-msg-msgv2
                                      is_log_msg-msg-msgv3
                                      is_log_msg-msg-msgv4 INTO lv_msgtx.
          INSERT zial_cl_log=>to_bapiret( ) INTO TABLE rt_bapiret.

        ELSE.

          MESSAGE e002(zial_log) INTO lv_msgtx.
          INSERT zial_cl_log=>to_bapiret( ) INTO TABLE rt_bapiret.

        ENDIF.

      WHEN zial_cl_log=>mc_log_process-save.
        " Nothing to log

    ENDCASE.

  ENDMETHOD.


  METHOD get_log_handle.

    rv_log_handle = ms_log-hdr-log_handle.

  ENDMETHOD.


  METHOD get_messages.

    rt_messages = mt_bapiret2.

  ENDMETHOD.


  METHOD constructor.

    GET TIME STAMP FIELD mv_process_bgn.
    ms_log-hdr = VALUE #( object    = iv_object
                          subobject = iv_subobject
                          aluser    = sy-uname
                          aldate    = sy-datum
                          altime    = sy-uzeit ).

    init( iv_extnumber = iv_extnumber
          it_extnumber = it_extnumber ).

    IF     iv_log_part_id IS SUPPLIED
       AND iv_log_part_id IS NOT INITIAL.
      ms_processing_control-log_part_id = iv_log_part_id.
    ENDIF.

  ENDMETHOD.


  METHOD init.

    set_extnumber( iv_extnumber = iv_extnumber
                   it_extnumber = it_extnumber ).

    set_detail_level( ).
    set_expiry_date( ).

    create_log( CORRESPONDING #( ms_log-hdr ) ).

    log_caller( ).

  ENDMETHOD.


  METHOD is_log_part.

    rv_result = xsdbool( ms_processing_control-log_part_id IS NOT INITIAL ).

  ENDMETHOD.


  METHOD log_bapiret.

    LOOP AT it_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapiret>).
      create_message( iv_msgid = <ls_bapiret>-id
                      iv_msgno = <ls_bapiret>-number
                      iv_msgty = <ls_bapiret>-type
                      iv_msgtx = <ls_bapiret>-message
                      iv_msgv1 = <ls_bapiret>-message_v1
                      iv_msgv2 = <ls_bapiret>-message_v2
                      iv_msgv3 = <ls_bapiret>-message_v3
                      iv_msgv4 = <ls_bapiret>-message_v4 ).
    ENDLOOP.

  ENDMETHOD.


  METHOD log_caller.

    " Returns one of the following return codes:
    " 0 - Everything's fine
    " 1 - Log could not be found
    " 2 - Message is inconsistent
    " 3 - Log is full
    " 4 - Other messages (not specified by SAP)
    " 5 - Message text is empty

    det_caller( ).

    create_message( iv_msgty        = zial_cl_log=>mc_msgty-success
                    iv_msgtx        = |***** { mv_caller } at { add_timestamp( ) } *****|
                    iv_is_dummy_msg = abap_true ).

  ENDMETHOD.


  METHOD log_duration.

    CHECK mv_process_bgn IS NOT INITIAL.

    IF mv_process_end IS INITIAL.
      GET TIME STAMP FIELD mv_process_end.
    ENDIF.

    TRY.
        DATA(lv_duration) = CONV tzntstmpl( cl_abap_tstmp=>subtract( tstmp1 = mv_process_end
                                                                     tstmp2 = mv_process_bgn ) * 1000 ).

        MESSAGE s018(zial_log) WITH lv_duration INTO DATA(lv_msgtx).
        create_message( iv_msgty        = zial_cl_log=>mc_msgty-success
                        iv_msgtx        = CONV #( lv_msgtx )
                        iv_is_dummy_msg = abap_true ).

      CATCH cx_root.
        " Duration couldn't be calculated

    ENDTRY.

  ENDMETHOD.


  METHOD log_exception.

    CHECK io_exception IS BOUND.

    DATA(lt_messages) = VALUE bapirettab( ).
    CASE TYPE OF io_exception.
      WHEN TYPE zcx_static_check INTO DATA(lx_static_check).
        lx_static_check->log_info( ).
        INSERT LINES OF lx_static_check->get_messages( ) INTO TABLE lt_messages.

      WHEN TYPE zcx_no_check INTO DATA(lx_no_check).
        lx_static_check->log_info( ).
        INSERT LINES OF lx_no_check->get_messages( ) INTO TABLE lt_messages.

      WHEN OTHERS.
        INSERT zial_cl_log=>to_bapiret( iv_msgtx = CONV #( io_exception->get_text( ) ) ) INTO TABLE lt_messages.

    ENDCASE.

    IF lt_messages IS INITIAL.
      RETURN.
    ENDIF.

    " Determine exception class name
    DATA(lo_exc_descr) = NEW cl_instance_description( io_exception ).
    ms_log-msg-params = VALUE #( altext = 'SBAL_EXCEPTION_01'
                                 t_par  = VALUE #( ( parname  = 'EXCEPTION'
                                                     parvalue = lo_exc_descr->class_name ) ) ).

    log_bapiret( lt_messages ).

  ENDMETHOD.


  METHOD log_line.

    create_message( iv_msgty        = zial_cl_log=>mc_msgty-success
                    iv_msgtx        = repeat( val = '-'
                                              occ = 255 )
                    iv_is_dummy_msg = abap_true ).

  ENDMETHOD.


  METHOD log_message.

    create_message( iv_msgty = iv_msgty
                    iv_msgtx = iv_msgtx
                    iv_msgid = iv_msgid
                    iv_msgno = iv_msgno
                    iv_msgv1 = iv_msgv1
                    iv_msgv2 = iv_msgv2
                    iv_msgv3 = iv_msgv3
                    iv_msgv4 = iv_msgv4
                    it_msgde = it_msgde ).

  ENDMETHOD.


  METHOD log_symsg.

    log_message( iv_msgty = is_symsg-msgty
                 iv_msgid = is_symsg-msgid
                 iv_msgno = is_symsg-msgno
                 iv_msgv1 = is_symsg-msgv1
                 iv_msgv2 = is_symsg-msgv2
                 iv_msgv3 = is_symsg-msgv3
                 iv_msgv4 = is_symsg-msgv4 ).

  ENDMETHOD.


  METHOD save.

    IF is_log_part( ) EQ abap_true.
      MESSAGE s021(zial_log) WITH zial_cl_log_conf=>mc_default-max_num_of_entries
                                  ms_processing_control-log_part_id INTO DATA(lv_msgtx) ##NEEDED.
      log_message( ).
    ENDIF.

    IF     lines( mt_bapiret2 ) GT 0
       AND (    ms_processing_control-has_error  EQ abap_false
             OR ms_processing_control-save_error EQ abap_true ).

      IF iv_finalize EQ abap_true.
        log_duration( ).
        log_line( ).
      ENDIF.

      save_log( ms_log-hdr-log_handle ).

      IF iv_finalize EQ abap_true.
        zial_cl_log_stack=>remove( ms_log-hdr-log_handle ).
      ENDIF.

    ELSE.

      zial_cl_log=>delete( ms_log-hdr-log_handle ).

    ENDIF.

  ENDMETHOD.


  METHOD save_log.

    DATA(lt_log_handles)    = VALUE bal_t_logh( ( iv_log_handle ) ).
    DATA(lt_new_lognumbers) = VALUE bal_t_lgnm( ).
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING  i_t_log_handle       = lt_log_handles
                 i_save_all           = abap_false
                 i_2th_connection     = abap_true
                 i_2th_connect_commit = abap_true
      IMPORTING  e_new_lognumbers     = lt_new_lognumbers
      EXCEPTIONS log_not_found        = 1
                 save_not_allowed     = 2
                 numbering_error      = 3
                 OTHERS               = 4.

    CASE sy-subrc.
      WHEN 0.
        save_msgde( lt_new_lognumbers ).
        COMMIT WORK.

      WHEN OTHERS.
        DATA(lv_subrc) = sy-subrc.
        zial_cl_log=>delete( ms_log-hdr-log_handle ).

        handle_error( iv_process = zial_cl_log=>mc_log_process-save
                      iv_subrc   = lv_subrc ).

    ENDCASE.

  ENDMETHOD.


  METHOD save_msgde.

    CHECK it_new_lognumbers IS NOT INITIAL.

    ASSIGN it_new_lognumbers[ lines( it_new_lognumbers ) ] TO FIELD-SYMBOL(<ls_new_lognumber>).
    CHECK mt_msg_details     IS NOT INITIAL
      AND <ls_new_lognumber> IS ASSIGNED.

    " EWM: /SCWM/DLV_EXPORT_LOG
    EXPORT msg_details FROM mt_msg_details TO DATABASE bal_indx(al) ID <ls_new_lognumber>-lognumber.

    CLEAR mt_msg_details.

  ENDMETHOD.


  METHOD set_content.

    CLEAR ms_log-msg.

    ms_log-msg-msgty = iv_msgty.
    IF ms_log-msg-msgty IS INITIAL.
      ms_log-msg-msgty = zial_cl_log=>mc_default-msgty.
    ENDIF.

    ms_log-msg-msgid = iv_msgid.
    IF ms_log-msg-msgid IS INITIAL.
      ms_log-msg-msgid = zial_cl_log=>mc_default-msgid.
    ENDIF.

    ms_log-msg-msgno = iv_msgno.
    IF ms_log-msg-msgno IS INITIAL.
      ms_log-msg-msgno = zial_cl_log=>mc_default-msgno.
    ENDIF.

    IF iv_msgtx IS NOT INITIAL.

      ms_log-msg-content_type = zial_cl_log=>mc_msg_content_type-txt.
      ms_log-msg-msgtx        = iv_msgtx.

      DO 4 TIMES.
        DATA(lv_msg_var) = SWITCH string( sy-index
                                          WHEN 1 THEN iv_msgv1
                                          WHEN 2 THEN iv_msgv2
                                          WHEN 3 THEN iv_msgv3
                                          WHEN 4 THEN iv_msgv4 ).
        IF lv_msg_var IS NOT INITIAL.
          REPLACE FIRST OCCURRENCE OF '&' IN ms_log-msg-msgtx WITH lv_msg_var.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

    ELSEIF iv_msgid IS NOT INITIAL
       AND iv_msgno IS NOT INITIAL.

      ms_log-msg-content_type = zial_cl_log=>mc_msg_content_type-obj.
      ms_log-msg-msgv1        = iv_msgv1.
      ms_log-msg-msgv2        = iv_msgv2.
      ms_log-msg-msgv3        = iv_msgv3.
      ms_log-msg-msgv4        = iv_msgv4.

    ELSE.

      RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD set_priority.

    ms_log-msg-probclass = SWITCH #( ms_log-msg-msgty
                                     WHEN zial_cl_log=>mc_msgty-info    THEN zial_cl_log=>mc_msgty_prio-info     " Additional information
                                     WHEN zial_cl_log=>mc_msgty-success THEN zial_cl_log=>mc_msgty_prio-success  " Medium important
                                     WHEN zial_cl_log=>mc_msgty-warning THEN zial_cl_log=>mc_msgty_prio-warning  " Important
                                     WHEN zial_cl_log=>mc_msgty-error   THEN zial_cl_log=>mc_msgty_prio-error ). " Very important

  ENDMETHOD.


  METHOD create_log.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING  i_s_log                 = is_log_header
      IMPORTING  e_log_handle            = ms_log-hdr-log_handle
      EXCEPTIONS log_header_inconsistent = 1
                 OTHERS                  = 2.

    IF     sy-subrc                        NE 0
       AND ms_processing_control-has_error EQ abap_false.
      handle_error( iv_process = zial_cl_log=>mc_log_process-init
                    iv_subrc   = sy-subrc ).
    ENDIF.

  ENDMETHOD.


  METHOD on_log_callback.

    CONSTANTS lc_log_number TYPE spo_par VALUE '%LOGNUMBER'.

    DATA lv_log_number     TYPE balognr.
    DATA lv_msg_param_id   TYPE zial_cl_log=>v_message_param_id.
    DATA ls_structure_name TYPE dd02l-tabname.

    FIELD-SYMBOLS <lt_outtab> TYPE STANDARD TABLE.

    " Find out the identifier for this message
    lv_log_number = VALUE #( it_params[ param = lc_log_number ]-value OPTIONAL ).
    CHECK lv_log_number IS NOT INITIAL.

    " Load specific message details from database
    DATA(lt_msg_details) = VALUE zial_tt_msg_details( ).
    IMPORT msg_details TO lt_msg_details FROM DATABASE bal_indx(al) ID lv_log_number.
    IF sy-subrc EQ 4.
      MESSAGE s019(zial_log) DISPLAY LIKE 'E'.
    ENDIF.

    CHECK lt_msg_details IS NOT INITIAL.

    lv_msg_param_id = VALUE #( it_params[ param = zial_cl_log=>mc_msg_ident ]-value OPTIONAL ).
    CHECK lv_msg_param_id IS NOT INITIAL.

    " Search for those entries which belong to this message
    ASSIGN lt_msg_details[ v_id = lv_msg_param_id ] TO FIELD-SYMBOL(<ls_msg_details>).
    CHECK <ls_msg_details> IS ASSIGNED.

    IF zial_cl_log=>mo_gui_alv_grid IS NOT INITIAL.
      zial_cl_log=>mo_gui_alv_grid->free( ).
      CLEAR zial_cl_log=>mo_gui_alv_grid.
    ENDIF.

    "    Show container if not visible
    " OR Hide container if detail to same message was again being selected
    IF     zial_cl_log=>mo_gui_docking_container IS BOUND
       AND zial_cl_log=>mv_sel_msg_param_id      EQ lv_msg_param_id.

      zial_cl_log=>mo_gui_docking_container->free( ).
      CLEAR: zial_cl_log=>mo_gui_docking_container,
             zial_cl_log=>mv_sel_msg_param_id.

    ELSEIF zial_cl_log=>mo_gui_docking_container IS NOT BOUND.

      zial_cl_log=>mo_gui_docking_container = NEW #( side      = cl_gui_docking_container=>dock_at_bottom
                                                     extension = '120' ).
      zial_cl_log=>mo_gui_docking_container->set_visible( abap_true ).

    ENDIF.

    CHECK zial_cl_log=>mo_gui_docking_container IS BOUND.

    zial_cl_log=>mo_gui_alv_grid = NEW #( i_parent = zial_cl_log=>mo_gui_docking_container ).

    DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt = 'X'
                                        sel_mode   = 'D' ).
    DATA(lt_alv_fcodes_excl) = VALUE ui_functions( ( cl_gui_alv_grid=>mc_fc_graph )
                                                   ( cl_gui_alv_grid=>mc_fc_info )
                                                   ( cl_gui_alv_grid=>mc_fc_excl_all ) ).

    IF <ls_msg_details>-t_input_parameter IS NOT INITIAL.
      ls_structure_name = '/SCWM/RSRA_S_PARAMETER'.
      ASSIGN <ls_msg_details>-t_input_parameter TO <lt_outtab>.
    ENDIF.

    CHECK <lt_outtab> IS ASSIGNED.

    zial_cl_log=>mo_gui_alv_grid->set_table_for_first_display( EXPORTING i_structure_name     = ls_structure_name
                                                                         is_layout            = ls_layout
                                                                         it_toolbar_excluding = lt_alv_fcodes_excl
                                                               CHANGING  it_outtab            = <lt_outtab> ).

    zial_cl_log=>mv_sel_msg_param_id = lv_msg_param_id.

  ENDMETHOD.


  METHOD has_error.

    rv_result = ms_processing_control-has_error.

  ENDMETHOD.


  METHOD create_and_save_error_log.

    DATA(lo_log_sap) = NEW zial_cl_log_sap( iv_object    = zial_cl_log=>mc_default-log_object
                                            iv_subobject = zial_cl_log=>mc_default-log_subobject
                                            iv_extnumber = TEXT-000 ).

    DATA(lt_bapiret) = error_handling( iv_process = iv_process
                                       iv_subrc   = iv_subrc
                                       is_log_msg = is_log_msg ).
    lo_log_sap->log_bapiret( lt_bapiret ).

    " Enable saving of the error log
    lo_log_sap->ms_processing_control-save_error = abap_true.

    " Save log for error logging
    lo_log_sap->save_log( ms_log-hdr-log_handle ).

  ENDMETHOD.


  METHOD add_msg_to_appl_log.

    CHECK handle_full_log( ) EQ abap_false.

    DATA(ls_msg_handle) = VALUE balmsghndl( ).
    IF is_valid_detail_level( ms_log-msg-msgty ) EQ abap_true.
      CASE ms_log-msg-content_type.
        WHEN zial_cl_log=>mc_msg_content_type-obj.
          ls_msg_handle = add_msg_by_message_object( ms_log ).

        WHEN zial_cl_log=>mc_msg_content_type-txt.
          ls_msg_handle = add_msg_by_message_text( ms_log ).

      ENDCASE.
    ENDIF.

    CASE has_msg_added_to_log( ls_msg_handle ).
      WHEN abap_true.
        INSERT zial_cl_log=>to_bapiret( iv_msgty = ms_log-msg-msgty
                                        iv_msgtx = ms_log-msg-msgtx
                                        iv_msgid = ms_log-msg-msgid
                                        iv_msgno = ms_log-msg-msgno
                                        iv_msgv1 = ms_log-msg-msgv1
                                        iv_msgv2 = ms_log-msg-msgv2
                                        iv_msgv3 = ms_log-msg-msgv3
                                        iv_msgv4 = ms_log-msg-msgv4 ) INTO TABLE mt_bapiret2.

      WHEN abap_false.
        handle_error( iv_process = zial_cl_log=>mc_log_process-add_msg
                      iv_subrc   = sy-subrc ).

    ENDCASE.

    CLEAR ms_log-msg-params.

  ENDMETHOD.


  METHOD is_log_getting_full.

    " 3 log entries are reserved for internal messages
    CHECK ms_processing_control-log_part_id IS INITIAL
      AND lines( mt_bapiret2 )              EQ ( zial_cl_log_conf=>mc_default-max_num_of_entries - 3 ).
    rv_result = abap_true.

  ENDMETHOD.

ENDCLASS.
