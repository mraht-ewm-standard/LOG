"! <p class="shorttext synchronized">Logging: General Log</p>
CLASS zial_cl_log_sap DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES t_spar TYPE STANDARD TABLE OF spar WITH DEFAULT KEY.

    CONSTANTS mc_class_name TYPE classname VALUE 'ZIAL_CL_LOG_SAP'.

    CONSTANTS: BEGIN OF mc_msgde_callback_type,
                 form     TYPE baluet VALUE ' ',
                 function TYPE baluet VALUE 'F',
               END OF mc_msgde_callback_type.

    CONSTANTS: BEGIN OF mc_msgde_callback,
                 baluef      TYPE baluef VALUE 'ZIAL_FM_LOG_CALLBACK',
                 baluep      TYPE baluep VALUE 'ZIAL_R_BS_LOG_CALLBACK',
                 baluep_form TYPE baluef VALUE 'ON_CLICK_MSG_DETAIL',
               END OF mc_msgde_callback.

    CLASS-METHODS on_log_callback
      IMPORTING it_params TYPE t_spar.

    "! Initialize log instance
    "!
    "! @parameter iv_object        | Log object
    "! @parameter iv_subobject     | Log subobject
    "! @parameter iv_extnumber     | External number / description for a log
    "! @parameter it_extnumber     | External number elements
    "! @parameter iv_callstack_lvl | Level of min. message type for which callstack is to be logged in message details
    METHODS constructor
      IMPORTING iv_object        TYPE balobj_d  DEFAULT zial_cl_log=>mc_default-log_object
                iv_subobject     TYPE balsubobj DEFAULT zial_cl_log=>mc_default-log_subobject
                iv_extnumber     TYPE balnrext  OPTIONAL
                it_extnumber     TYPE stringtab OPTIONAL
                iv_callstack_lvl TYPE numc1     DEFAULT zial_cl_log=>mc_callstack_lvl-info.

    "! Get all logged messages
    "!
    "! @parameter rt_messages | BAPI messages
    METHODS get_messages
      RETURNING VALUE(rt_messages) TYPE bapirettab.

    "! Log a message with optionally message details
    "!
    "! @parameter it_msgde | Message details
    METHODS log_message
      IMPORTING it_msgde TYPE rsra_t_alert_definition OPTIONAL.

    "! Log exception
    "!
    "! @parameter io_exception | Exception object
    METHODS log_exception
      IMPORTING io_exception TYPE REF TO cx_root.

    "! Log symsg messages
    "!
    "! @parameter is_symsg | SAP system message structure
    METHODS log_sy_message
      IMPORTING is_symsg TYPE symsg.

    "! Log a table of bapi messages
    "!
    "! @parameter it_bapiret | BAPI messages
    METHODS log_bapiret
      IMPORTING it_bapiret TYPE bapirettab.

    "! Log a horizontal line
    "!
    METHODS log_line.

    "! Log warning message
    "!
    "! @parameter iv_msgtx | Message text
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @parameter it_msgde | Message details
    METHODS log_warning
      IMPORTING iv_msgtx TYPE bapi_msg                OPTIONAL
                iv_msgno TYPE symsgno                 OPTIONAL
                iv_msgv1 TYPE symsgv                  OPTIONAL
                iv_msgv2 TYPE symsgv                  OPTIONAL
                iv_msgv3 TYPE symsgv                  OPTIONAL
                iv_msgv4 TYPE symsgv                  OPTIONAL
                it_msgde TYPE rsra_t_alert_definition OPTIONAL.

    "! Log name of development object which called the function to be logged
    "!
    METHODS log_caller.

    "! Log info message
    "!
    "! @parameter iv_msgtx | Message text
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @parameter it_msgde | Message details
    METHODS info
      IMPORTING iv_msgtx TYPE bapi_msg                OPTIONAL
                iv_msgno TYPE symsgno                 OPTIONAL
                iv_msgv1 TYPE symsgv                  OPTIONAL
                iv_msgv2 TYPE symsgv                  OPTIONAL
                iv_msgv3 TYPE symsgv                  OPTIONAL
                iv_msgv4 TYPE symsgv                  OPTIONAL
                it_msgde TYPE rsra_t_alert_definition OPTIONAL.

    "! Log success message
    "!
    "! @parameter iv_msgtx | Message text
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @parameter it_msgde | Message details
    METHODS log_success
      IMPORTING iv_msgtx TYPE bapi_msg                OPTIONAL
                iv_msgno TYPE symsgno                 OPTIONAL
                iv_msgv1 TYPE symsgv                  OPTIONAL
                iv_msgv2 TYPE symsgv                  OPTIONAL
                iv_msgv3 TYPE symsgv                  OPTIONAL
                iv_msgv4 TYPE symsgv                  OPTIONAL
                it_msgde TYPE rsra_t_alert_definition OPTIONAL.

    "! Log error message
    "!
    "! @parameter iv_msgtx | Message text
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @parameter it_msgde | Message details
    METHODS log_error
      IMPORTING iv_msgtx TYPE bapi_msg                OPTIONAL
                iv_msgno TYPE symsgno                 OPTIONAL
                iv_msgv1 TYPE symsgv                  OPTIONAL
                iv_msgv2 TYPE symsgv                  OPTIONAL
                iv_msgv3 TYPE symsgv                  OPTIONAL
                iv_msgv4 TYPE symsgv                  OPTIONAL
                it_msgde TYPE rsra_t_alert_definition OPTIONAL.

    "! Save log to application log and optionally close log instance
    "!
    "! @parameter iv_finalize | Finalize/close log? (Y/N)
    METHODS save
      IMPORTING iv_finalize TYPE abap_bool DEFAULT abap_true.

  PROTECTED SECTION.
    DATA mv_validity_in_days  TYPE i VALUE 180.
    DATA mv_process_bgn       TYPE timestampl.
    DATA mv_process_end       TYPE timestampl.

    DATA mv_caller            TYPE c LENGTH 200.
    DATA mv_callstack_lvl     TYPE numc1.

    DATA mv_msg_text          TYPE bapi_msg.
    DATA mv_msg_type          TYPE symsgty.
    DATA mv_msg_content_type  TYPE numc1.
    DATA mv_msg_class         TYPE symsgid.
    DATA ms_msg_params        TYPE bal_s_parm.
    DATA mv_msg_number        TYPE symsgno.
    DATA mv_msg_var1          TYPE symsgv.
    DATA mv_msg_var2          TYPE symsgv.
    DATA mv_msg_var3          TYPE symsgv.
    DATA mv_msg_var4          TYPE symsgv.
    DATA mv_msg_priority      TYPE balprobcl.
    DATA ms_msg_context       TYPE bal_s_cont.

    DATA mv_log_handle        TYPE balloghndl.
    DATA ms_log_header        TYPE bal_s_log.
    DATA mt_log_messages      TYPE bapirettab.
    DATA mv_log_counter       TYPE i.

    DATA mv_msg_param_id      TYPE zial_cl_log=>v_message_param_id.
    DATA ms_msg_details       TYPE zial_s_msg_details.
    DATA mt_msg_details       TYPE zial_tt_msg_details.
    DATA mt_msg_details_input TYPE rsra_t_alert_definition.

    CLASS-METHODS error_handling
      IMPORTING iv_process        TYPE char4
                iv_subrc          TYPE sysubrc
                io_exception      TYPE REF TO cx_root
                is_log_msg        TYPE zial_s_log_msg
      RETURNING VALUE(rt_bapiret) TYPE bapirettab.

    "! Initialize log
    "!
    "! @parameter iv_extnumber | External number / description for a log
    "! @parameter it_extnumber | External number elements
    METHODS init
      IMPORTING iv_extnumber TYPE balnrext
                it_extnumber TYPE stringtab.

    METHODS add_msg_to_log_protocol
      IMPORTING is_msg_handle TYPE balmsghndl.

    METHODS build_extnumber
      IMPORTING iv_extnumber TYPE balnrext  OPTIONAL
                it_extnumber TYPE stringtab OPTIONAL.

    METHODS handle_error
      IMPORTING iv_process   TYPE char4
                iv_subrc     TYPE sysubrc        OPTIONAL
                io_exception TYPE REF TO cx_root OPTIONAL.

    METHODS set_priority.

    METHODS set_content
      IMPORTING iv_msgid TYPE symsgid
                iv_msgty TYPE symsgty
                iv_msgtx TYPE bapi_msg
                iv_msgno TYPE symsgno
                iv_msgv1 TYPE symsgv
                iv_msgv2 TYPE symsgv
                iv_msgv3 TYPE symsgv
                iv_msgv4 TYPE symsgv.

    METHODS create_message
      IMPORTING iv_msgid        TYPE symsgid                 OPTIONAL
                iv_msgty        TYPE symsgty
                iv_msgtx        TYPE bapi_msg                OPTIONAL
                iv_msgno        TYPE symsgno                 OPTIONAL
                iv_msgv1        TYPE symsgv                  OPTIONAL
                iv_msgv2        TYPE symsgv                  OPTIONAL
                iv_msgv3        TYPE symsgv                  OPTIONAL
                iv_msgv4        TYPE symsgv                  OPTIONAL
                it_msgde        TYPE rsra_t_alert_definition OPTIONAL
                iv_is_dummy_msg TYPE abap_bool               DEFAULT abap_false.

    METHODS add_msg_by_message_text.
    METHODS add_msg_by_message_object.

    METHODS add_timestamp
      RETURNING VALUE(rv_time) TYPE symsgv.

    METHODS build_validity.
    METHODS add_message_context.
    METHODS add_message_callstack.
    METHODS log_duration.
    METHODS det_caller.
    METHODS save_log.
    METHODS add_message_detail.

    METHODS save_msgde
      IMPORTING it_new_lognumbers TYPE bal_t_lgnm.

    METHODS create_log.
    METHODS delete_log.

  PRIVATE SECTION.
    CLASS-DATA mv_has_error  TYPE abap_bool.
    CLASS-DATA mv_save_error TYPE abap_bool.

ENDCLASS.


CLASS zial_cl_log_sap IMPLEMENTATION.

  METHOD add_message_callstack.

    CHECK mv_callstack_lvl GT 0.

    CASE mv_msg_type.
      WHEN zial_cl_log=>mc_log_type-info.
        IF mv_callstack_lvl LT zial_cl_log=>mc_callstack_lvl-info.
          RETURN.
        ENDIF.

      WHEN zial_cl_log=>mc_log_type-success.
        IF mv_callstack_lvl LT zial_cl_log=>mc_callstack_lvl-success.
          RETURN.
        ENDIF.

      WHEN zial_cl_log=>mc_log_type-warning.
        IF mv_callstack_lvl LT zial_cl_log=>mc_callstack_lvl-warning.
          RETURN.
        ENDIF.

      WHEN zial_cl_log=>mc_log_type-error.
        IF mv_callstack_lvl LT zial_cl_log=>mc_callstack_lvl-error.
          RETURN.
        ENDIF.

    ENDCASE.

    zial_cl_session=>get_callstack( IMPORTING et_callstack = DATA(lt_callstack) ).
    DELETE lt_callstack WHERE mainprogram CS mc_class_name.

    DATA(lv_line) = repeat( val = '-'
                            occ = 80 ).
    APPEND VALUE #( low = lv_line ) TO mt_msg_details_input.
    APPEND LINES OF VALUE rsra_t_alert_definition( FOR <s_callstack> IN lt_callstack
                                                   ( low = |{ <s_callstack>-mainprogram }=>| &&
                                                           |{ <s_callstack>-event }, { TEXT-002 } | &&
                                                           |{ <s_callstack>-line }| ) ) TO mt_msg_details_input.

  ENDMETHOD.


  METHOD add_message_context.

    zial_cl_session=>get_context( IMPORTING ev_program   = DATA(lv_program)
                                            ev_blockname = DATA(lv_include)
                                            ev_line      = DATA(lv_line) ).

    ms_msg_context = VALUE #( value   = VALUE zial_s_log_context( program = lv_program
                                                                  include = lv_include
                                                                  line    = lv_line )
                              tabname = zial_cl_log=>mc_log_context_struct ).

  ENDMETHOD.


  METHOD add_message_detail.

    CHECK mt_msg_details_input IS NOT INITIAL.

    " Add message identifier, s. include LSBAL_DETAILF02 (example: SBAL_CALLBACK)
    ms_msg_params-callback = VALUE #( userexitf = mc_msgde_callback-baluef
                                      userexitt = mc_msgde_callback_type-function ).

    mv_msg_param_id = mv_msg_param_id + 1.
    APPEND VALUE #( parname  = zial_cl_log=>mc_msg_ident
                    parvalue = mv_msg_param_id ) TO ms_msg_params-t_par.

    APPEND VALUE #( v_id              = mv_msg_param_id
                    t_input_parameter = mt_msg_details_input ) TO mt_msg_details.

    CLEAR mt_msg_details_input.

  ENDMETHOD.


  METHOD add_msg_by_message_object.

    DATA(ls_msg) = VALUE bal_s_msg( msgty     = mv_msg_type
                                    probclass = mv_msg_priority
                                    context   = ms_msg_context
                                    params    = ms_msg_params
                                    msgid     = mv_msg_class
                                    msgno     = mv_msg_number
                                    msgv1     = mv_msg_var1
                                    msgv2     = mv_msg_var2
                                    msgv3     = mv_msg_var3
                                    msgv4     = mv_msg_var4 ).

    DATA(ls_msg_handle) = VALUE balmsghndl( ).
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING  i_log_handle     = mv_log_handle
                 i_s_msg          = ls_msg
      IMPORTING  e_s_msg_handle   = ls_msg_handle
      EXCEPTIONS log_not_found    = 1
                 msg_inconsistent = 2
                 log_is_full      = 3
                 OTHERS           = 4.

    CASE sy-subrc.
      WHEN 0.
        add_msg_to_log_protocol( ls_msg_handle ).

      WHEN OTHERS.
        handle_error( iv_process = zial_cl_log=>mc_log_process-create
                      iv_subrc   = sy-subrc ).

    ENDCASE.

  ENDMETHOD.


  METHOD add_msg_by_message_text.

    DATA(ls_msg_handle) = VALUE balmsghndl( ).
    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING  i_log_handle     = mv_log_handle
                 i_msgty          = mv_msg_type
                 i_probclass      = mv_msg_priority
                 i_s_context      = ms_msg_context
                 i_text           = mv_msg_text
                 i_s_params       = ms_msg_params
      IMPORTING  e_s_msg_handle   = ls_msg_handle
      EXCEPTIONS log_not_found    = 1
                 msg_inconsistent = 2
                 log_is_full      = 3
                 OTHERS           = 4.

    CASE sy-subrc.
      WHEN 0.
        add_msg_to_log_protocol( ls_msg_handle ).

      WHEN OTHERS.
        handle_error( iv_process = zial_cl_log=>mc_log_process-create
                      iv_subrc   = sy-subrc ).

    ENDCASE.

  ENDMETHOD.


  METHOD add_msg_to_log_protocol.

    DATA(ls_msg) = VALUE bal_s_msg( ).

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING  i_s_msg_handle = is_msg_handle
      IMPORTING  e_s_msg        = ls_msg
      EXCEPTIONS log_not_found  = 1
                 msg_not_found  = 2
                 OTHERS         = 3.

    CASE sy-subrc.
      WHEN 0.
        mv_log_counter = mv_log_counter + 1.

        DATA(ls_bapiret2) = CORRESPONDING bapiret2( ls_msg MAPPING id         = msgid
                                                                   type       = msgty
                                                                   number     = msgno
                                                                   message_v1 = msgv1
                                                                   message_v2 = msgv2
                                                                   message_v3 = msgv3
                                                                   message_v4 = msgv4 ).

        CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
          EXPORTING id         = ls_msg-msgid
                    number     = ls_msg-msgno
                    textformat = 'RTF'
                    message_v1 = ls_msg-msgv1
                    message_v2 = ls_msg-msgv2
                    message_v3 = ls_msg-msgv3
                    message_v4 = ls_msg-msgv4
          IMPORTING message    = ls_bapiret2-message.

        APPEND ls_bapiret2 TO mt_log_messages.

    ENDCASE.

  ENDMETHOD.


  METHOD add_timestamp.

    DATA lv_timestamp TYPE timestampl.

    GET TIME STAMP FIELD lv_timestamp.
    rv_time = |{ lv_timestamp }|.

    " DD.MM.YYYY, hh:mm:ss.ms
    rv_time = |{ rv_time+6(2) }.{ rv_time+4(2) }.{ rv_time(4) }, | &
              |{ rv_time+8(2) }:{ rv_time+10(2) }:{ rv_time+12(2) }{ rv_time+14 }|.

  ENDMETHOD.


  METHOD build_extnumber.

    DATA(lt_extnumber) = VALUE stringtab( ).

    IF iv_extnumber IS NOT INITIAL.

      APPEND iv_extnumber TO lt_extnumber.

    ELSEIF it_extnumber IS NOT INITIAL.

      lt_extnumber = it_extnumber.

    ENDIF.

    LOOP AT lt_extnumber ASSIGNING FIELD-SYMBOL(<lv_extnumber>).

      CASE sy-tabix.
        WHEN 1.
          ms_log_header-extnumber = |{ <lv_extnumber> }|.

        WHEN OTHERS.
          ms_log_header-extnumber = |{ ms_log_header-extnumber } { <lv_extnumber> }|.

      ENDCASE.

    ENDLOOP.

    IF lt_extnumber IS INITIAL.

      SELECT SINGLE subobjtxt FROM balsubt
        INTO @ms_log_header-extnumber
        WHERE spras     EQ @sy-langu
          AND object    EQ @ms_log_header-object
          AND subobject EQ @ms_log_header-subobject.

    ENDIF.

  ENDMETHOD.


  METHOD build_validity.

    CHECK mv_validity_in_days GT 0.

    ms_log_header-aldate_del = sy-datum + mv_validity_in_days.

  ENDMETHOD.


  METHOD create_message.

    set_content( iv_msgid = iv_msgid
                 iv_msgty = iv_msgty
                 iv_msgtx = iv_msgtx
                 iv_msgno = iv_msgno
                 iv_msgv1 = iv_msgv1
                 iv_msgv2 = iv_msgv2
                 iv_msgv3 = iv_msgv3
                 iv_msgv4 = iv_msgv4 ).

    IF iv_is_dummy_msg EQ abap_false.

      mt_msg_details_input = it_msgde.

      set_priority( ).

      add_message_context( ).

      add_message_callstack( ).

      add_message_detail( ).

    ENDIF.

    CASE mv_msg_content_type.
      WHEN zial_cl_log=>mc_msg_content_type-obj.
        add_msg_by_message_object( ).

      WHEN zial_cl_log=>mc_msg_content_type-txt.
        add_msg_by_message_text( ).

    ENDCASE.

    CLEAR ms_msg_params.

  ENDMETHOD.


  METHOD det_caller.

    CHECK mv_caller CO ' _0'.

    zial_cl_session=>get_callstack( IMPORTING ev_function = DATA(lv_function)
                                              ev_method   = DATA(lv_method)
                                              ev_class    = DATA(lv_class)
                                              ev_report   = DATA(lv_report) ).

    IF lv_function IS NOT INITIAL.

      mv_caller = lv_function.

    ELSEIF lv_class  IS NOT INITIAL
       AND lv_method IS NOT INITIAL.

      CONCATENATE lv_class '=>' lv_method INTO mv_caller.

    ELSEIF lv_report IS NOT INITIAL.

      mv_caller = lv_report.

    ELSE.

      RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD log_error.

    create_message( iv_msgty = zial_cl_log=>mc_log_type-error
                    iv_msgtx = iv_msgtx
                    iv_msgno = iv_msgno
                    iv_msgv1 = iv_msgv1
                    iv_msgv2 = iv_msgv2
                    iv_msgv3 = iv_msgv3
                    iv_msgv4 = iv_msgv4
                    it_msgde = it_msgde ).

  ENDMETHOD.


  METHOD handle_error.

    " Ensures that error handling is only executed once for each error to avoid endless loop
    CHECK mv_has_error  EQ abap_false
      AND mv_save_error EQ abap_false.
    mv_has_error = abap_true.

    " Backup input data
    DATA(ls_log_msg) = VALUE zial_s_log_msg( hdr = VALUE #( object     = ms_log_header-object
                                                            subobject  = ms_log_header-subobject
                                                            extnumber  = ms_log_header-extnumber
                                                            aldate_del = ms_log_header-aldate_del )
                                             msg = VALUE #( msgid = mv_msg_class
                                                            msgty = mv_msg_type
                                                            msgtx = mv_msg_text
                                                            msgno = mv_msg_number
                                                            msgv1 = mv_msg_var1
                                                            msgv2 = mv_msg_var2
                                                            msgv3 = mv_msg_var3
                                                            msgv4 = mv_msg_var4 ) ).

    " Try to add error messages regarding failed logging to old log
    MESSAGE e001(zial_log) INTO DATA(lv_msg) ##NEEDED.
    log_message( ).

    " Close existing log and create a new one for error handling
    save( ).

    DATA(lo_log_sap) = NEW zial_cl_log_sap( iv_object    = zial_cl_log=>mc_default-log_object
                                            iv_subobject = zial_cl_log=>mc_default-log_subobject
                                            iv_extnumber = TEXT-000 ).

    DATA(lt_bapiret) = error_handling( iv_process   = iv_process
                                       iv_subrc     = iv_subrc
                                       io_exception = io_exception
                                       is_log_msg   = ls_log_msg ).
    lo_log_sap->log_bapiret( lt_bapiret ).

    " Enable saving of the error log
    lo_log_sap->mv_save_error = abap_true.

    " Save log for error logging
    lo_log_sap->save_log( ).

    " Error handling could be executed again
    mv_has_error  = abap_false.
    mv_save_error = abap_false.

  ENDMETHOD.


  METHOD error_handling.

    MESSAGE e017(zial_log) WITH iv_process INTO DATA(lv_msg) ##NEEDED.
    APPEND zial_cl_log=>to_bapiret( ) TO rt_bapiret.

    " Log general log data
    DATA(lv_msg_txt_gen) = CONV bapi_msg( |; OBJECT: { is_log_msg-hdr-object }; | &&
                                          |SUBOBJ: { is_log_msg-hdr-subobject }| &&
                                          |; EXTNUM: { is_log_msg-hdr-extnumber }; | &&
                                          |ALDDEL: { is_log_msg-hdr-aldate_del }| ).
    APPEND zial_cl_log=>to_bapiret( iv_msgty = zial_cl_log=>mc_log_type-error
                                    iv_msgtx = lv_msg_txt_gen ) TO rt_bapiret.

    CASE iv_process.
      WHEN zial_cl_log=>mc_log_process-init.
        CASE iv_subrc.
          WHEN 1.
            MESSAGE e006(zial_log) WITH iv_subrc INTO lv_msg.

          WHEN OTHERS.
            MESSAGE e007(zial_log) WITH iv_subrc INTO lv_msg.

        ENDCASE.

      WHEN zial_cl_log=>mc_log_process-save.
        CASE iv_subrc.
          WHEN 1.
            MESSAGE e008(zial_log) WITH iv_subrc INTO lv_msg.

          WHEN 2.
            MESSAGE e009(zial_log) WITH iv_subrc INTO lv_msg.

          WHEN 3.
            MESSAGE e010(zial_log) WITH iv_subrc INTO lv_msg.

          WHEN OTHERS.
            MESSAGE e011(zial_log) WITH iv_subrc INTO lv_msg.

        ENDCASE.

      WHEN OTHERS.
        CASE sy-subrc.
          WHEN 1.
            MESSAGE e008(zial_log) WITH iv_subrc INTO lv_msg.

          WHEN 2.
            MESSAGE e012(zial_log) WITH iv_subrc INTO lv_msg.

          WHEN 3.
            MESSAGE e013(zial_log) WITH iv_subrc INTO lv_msg.

          WHEN OTHERS.
            MESSAGE e014(zial_log) WITH iv_subrc INTO lv_msg.

        ENDCASE.

    ENDCASE.

    APPEND zial_cl_log=>to_bapiret( ) TO rt_bapiret.

    " Log process-specific log data
    CASE iv_process.
      WHEN zial_cl_log=>mc_log_process-init.
        " Nothing to log

      WHEN zial_cl_log=>mc_log_process-create.
        IF is_log_msg-msg-msgtx CN ' _0'.

          MESSAGE e015(zial_log) WITH is_log_msg-msg-msgty INTO lv_msg.
          APPEND zial_cl_log=>to_bapiret( ) TO rt_bapiret.

        ELSEIF is_log_msg-msg-msgno CN ' _0'.

          MESSAGE e003(zial_log) WITH is_log_msg-msg-msgno
                                      is_log_msg-msg-msgid INTO lv_msg.
          APPEND zial_cl_log=>to_bapiret( ) TO rt_bapiret.

          MESSAGE e004(zial_log) WITH is_log_msg-msg-msgv1
                                      is_log_msg-msg-msgv2
                                      is_log_msg-msg-msgv3
                                      is_log_msg-msg-msgv4 INTO lv_msg.
          APPEND zial_cl_log=>to_bapiret( ) TO rt_bapiret.

        ELSE.

          MESSAGE e002(zial_log) INTO lv_msg.
          APPEND zial_cl_log=>to_bapiret( ) TO rt_bapiret.

        ENDIF.

      WHEN zial_cl_log=>mc_log_process-exception.
        DATA(lo_exc_descr) = NEW cl_instance_description( io_exception ).

        MESSAGE e005(zial_log) WITH lo_exc_descr->class_name INTO lv_msg.
        APPEND zial_cl_log=>to_bapiret( ) TO rt_bapiret.

      WHEN zial_cl_log=>mc_log_process-save.
        " Nothing to log

    ENDCASE.

  ENDMETHOD.


  METHOD get_messages.

    rt_messages = mt_log_messages.

  ENDMETHOD.


  METHOD info.

    create_message( iv_msgty = zial_cl_log=>mc_log_type-info
                    iv_msgtx = iv_msgtx
                    iv_msgno = iv_msgno
                    iv_msgv1 = iv_msgv1
                    iv_msgv2 = iv_msgv2
                    iv_msgv3 = iv_msgv3
                    iv_msgv4 = iv_msgv4
                    it_msgde = it_msgde ).

  ENDMETHOD.


  METHOD constructor.

    GET TIME STAMP FIELD mv_process_bgn.

    mv_callstack_lvl = iv_callstack_lvl.

    ms_log_header    = VALUE #( object    = iv_object
                                subobject = iv_subobject
                                aluser    = sy-uname
                                aldate    = sy-datum
                                altime    = sy-uzeit ).

    init( iv_extnumber = iv_extnumber
          it_extnumber = it_extnumber ).

  ENDMETHOD.


  METHOD init.

    GET TIME STAMP FIELD mv_process_bgn.

    build_extnumber( iv_extnumber = iv_extnumber
                     it_extnumber = it_extnumber ).

    build_validity( ).

    create_log( ).

    det_caller( ).
    log_caller( ).

  ENDMETHOD.


  METHOD log_bapiret.

    LOOP AT it_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapiret>).

      create_message( iv_msgty = <ls_bapiret>-type
                      iv_msgtx = <ls_bapiret>-message ).

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

    create_message( iv_msgty        = zial_cl_log=>mc_log_type-success
                    iv_msgtx        = |***** { mv_caller } at { add_timestamp( ) } *****|
                    iv_is_dummy_msg = abap_true ).

  ENDMETHOD.


  METHOD log_duration.

    IF    mv_process_end IS INITIAL
       OR mv_process_bgn IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        DATA(lv_duration) = cl_abap_tstmp=>subtract( tstmp1 = mv_process_end
                                                     tstmp2 = mv_process_bgn ) * 1000.

        MESSAGE s018(zial_log) WITH lv_duration INTO DATA(lv_msgtx).
        create_message( iv_msgty        = zial_cl_log=>mc_log_type-success
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
        lx_static_check->log_exception_raised( ).
        INSERT LINES OF lx_static_check->get_messages( ) INTO TABLE lt_messages.

      WHEN TYPE zcx_no_check INTO DATA(lx_no_check).
        lx_static_check->log_exception_raised( ).
        INSERT LINES OF lx_no_check->get_messages( ) INTO TABLE lt_messages.

      WHEN OTHERS.
        INSERT zial_cl_log=>to_bapiret( iv_msgtx = CONV #( io_exception->get_text( ) ) ) INTO TABLE lt_messages.

    ENDCASE.

    IF lt_messages IS INITIAL.
      RETURN.
    ENDIF.

    " Determine exception class name
    DATA(lo_exc_descr) = NEW cl_instance_description( io_exception ).
    ms_msg_params = VALUE #( altext = 'SBAL_EXCEPTION_01'
                             t_par  = VALUE #( ( parname  = 'EXCEPTION'
                                                 parvalue = lo_exc_descr->class_name ) ) ).

    log_bapiret( lt_messages ).

  ENDMETHOD.


  METHOD log_line.

    create_message( iv_msgty        = zial_cl_log=>mc_log_type-success
                    iv_msgtx        = repeat( val = '-'
                                              occ = 255 )
                    iv_is_dummy_msg = abap_true ).

  ENDMETHOD.


  METHOD log_message.

    create_message( iv_msgid = sy-msgid
                    iv_msgty = sy-msgty
                    iv_msgno = sy-msgno
                    iv_msgv1 = sy-msgv1
                    iv_msgv2 = sy-msgv2
                    iv_msgv3 = sy-msgv3
                    iv_msgv4 = sy-msgv4
                    it_msgde = it_msgde ).

  ENDMETHOD.


  METHOD log_sy_message.

    mv_msg_class = is_symsg-msgid.

    CASE is_symsg-msgty.
      WHEN zial_cl_log=>mc_log_type-info.
        info( iv_msgno = is_symsg-msgno
              iv_msgv1 = is_symsg-msgv1
              iv_msgv2 = is_symsg-msgv2
              iv_msgv3 = is_symsg-msgv3
              iv_msgv4 = is_symsg-msgv4 ).

      WHEN zial_cl_log=>mc_log_type-success.
        log_success( iv_msgno = is_symsg-msgno
                     iv_msgv1 = is_symsg-msgv1
                     iv_msgv2 = is_symsg-msgv2
                     iv_msgv3 = is_symsg-msgv3
                     iv_msgv4 = is_symsg-msgv4 ).

      WHEN zial_cl_log=>mc_log_type-warning.
        log_warning( iv_msgno = is_symsg-msgno
                     iv_msgv1 = is_symsg-msgv1
                     iv_msgv2 = is_symsg-msgv2
                     iv_msgv3 = is_symsg-msgv3
                     iv_msgv4 = is_symsg-msgv4 ).

      WHEN zial_cl_log=>mc_log_type-error.
        log_error( iv_msgno = is_symsg-msgno
                   iv_msgv1 = is_symsg-msgv1
                   iv_msgv2 = is_symsg-msgv2
                   iv_msgv3 = is_symsg-msgv3
                   iv_msgv4 = is_symsg-msgv4 ).

    ENDCASE.

  ENDMETHOD.


  METHOD save.

    IF     (    mv_has_error  EQ abap_false
             OR mv_save_error EQ abap_true )
       AND mv_log_counter GT 0.

      IF iv_finalize EQ abap_true.
        GET TIME STAMP FIELD mv_process_end.
        log_duration( ).
        log_line( ).
      ENDIF.

      save_log( ).

    ELSE.

      CALL FUNCTION 'BAL_LOG_DELETE'
        EXPORTING  i_log_handle = mv_log_handle
        EXCEPTIONS OTHERS       = 0.

    ENDIF.

    IF iv_finalize EQ abap_true.
      CLEAR: ms_log_header,
             mv_log_handle.
    ENDIF.

  ENDMETHOD.


  METHOD save_log.

    DATA(lt_log_handles)    = VALUE bal_t_logh( ( mv_log_handle ) ).
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
        delete_log( ).
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

    mv_msg_class = iv_msgid.
    mv_msg_type  = iv_msgty.

    IF iv_msgtx IS NOT INITIAL.

      mv_msg_text = iv_msgtx.

      DO 4 TIMES.

        DATA(lv_msg_var) = VALUE string( ).
        CASE sy-index.
          WHEN 1.
            lv_msg_var = iv_msgv1.

          WHEN 2.
            lv_msg_var = iv_msgv2.

          WHEN 3.
            lv_msg_var = iv_msgv3.

          WHEN 4.
            lv_msg_var = iv_msgv4.

          WHEN OTHERS.
            EXIT.

        ENDCASE.

        IF lv_msg_var IS NOT INITIAL.
          REPLACE FIRST OCCURRENCE OF '&' IN mv_msg_text WITH lv_msg_var.
        ELSE.
          EXIT.
        ENDIF.

      ENDDO.

      mv_msg_content_type = zial_cl_log=>mc_msg_content_type-txt.

    ELSEIF iv_msgno CN ' _'.

      mv_msg_number = iv_msgno.

      mv_msg_var1 = iv_msgv1.
      mv_msg_var2 = iv_msgv2.
      mv_msg_var3 = iv_msgv3.
      mv_msg_var4 = iv_msgv4.

      mv_msg_content_type = zial_cl_log=>mc_msg_content_type-obj.

    ELSE.

      RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD set_priority.

    CASE mv_msg_type.
      WHEN zial_cl_log=>mc_log_type-info.
        mv_msg_priority = zial_cl_log=>mc_log_type-info_prio.    " Additional information

      WHEN zial_cl_log=>mc_log_type-success.
        mv_msg_priority = zial_cl_log=>mc_log_type-success_prio. " Medium important

      WHEN zial_cl_log=>mc_log_type-warning.
        mv_msg_priority = zial_cl_log=>mc_log_type-warning_prio. " Important

      WHEN zial_cl_log=>mc_log_type-error.
        mv_msg_priority = zial_cl_log=>mc_log_type-error_prio.   " Very important

    ENDCASE.

  ENDMETHOD.


  METHOD log_success.

    create_message( iv_msgty = zial_cl_log=>mc_log_type-success
                    iv_msgtx = iv_msgtx
                    iv_msgno = iv_msgno
                    iv_msgv1 = iv_msgv1
                    iv_msgv2 = iv_msgv2
                    iv_msgv3 = iv_msgv3
                    iv_msgv4 = iv_msgv4
                    it_msgde = it_msgde ).

  ENDMETHOD.


  METHOD log_warning.

    create_message( iv_msgty = zial_cl_log=>mc_log_type-warning
                    iv_msgtx = iv_msgtx
                    iv_msgno = iv_msgno
                    iv_msgv1 = iv_msgv1
                    iv_msgv2 = iv_msgv2
                    iv_msgv3 = iv_msgv3
                    iv_msgv4 = iv_msgv4
                    it_msgde = it_msgde ).

  ENDMETHOD.


  METHOD create_log.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING  i_s_log                 = ms_log_header
      IMPORTING  e_log_handle            = mv_log_handle
      EXCEPTIONS log_header_inconsistent = 1
                 OTHERS                  = 2.

    IF     sy-subrc     NE 0
       AND mv_has_error EQ abap_false.
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


  METHOD delete_log.

    CALL FUNCTION 'BAL_LOG_DELETE'
      EXPORTING  i_log_handle = mv_log_handle
      EXCEPTIONS OTHERS       = 0.

    CLEAR: ms_log_header,
           mv_log_handle.

  ENDMETHOD.

ENDCLASS.
