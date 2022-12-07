"! <p class="shorttext synchronized" lang="en">Logging: General Log</p>
CLASS zial_cl_log_sap DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS: mc_class_name TYPE classname VALUE 'ZIAL_CL_LOG_SAP'.

    "! Initialise log instance
    "!
    "! @parameter iv_object | Log object
    "! @parameter iv_subobject | Log subobject
    "! @parameter iv_extnumber | External number / description for a log
    "! @parameter it_extnumber | External number elements
    "! @parameter iv_callstack_lvl | Level of minimum message type for which the callstack is to be logged in message details
    METHODS constructor
      IMPORTING
        !iv_object        TYPE balobj_d DEFAULT 'APPL_LOG' " mc_dflt_log_object
        !iv_subobject     TYPE balsubobj
        !iv_extnumber     TYPE balnrext OPTIONAL
        !it_extnumber     TYPE stringtab OPTIONAL
        !iv_callstack_lvl TYPE numc1 DEFAULT zial_cl_log=>mc_callstack_lvl-info.
    "! Initialise log
    "!
    "! @parameter iv_extnumber | External number / description for a log
    "! @parameter it_extnumber | External number elements
    METHODS init
      IMPORTING
        !iv_extnumber TYPE balnrext
        !it_extnumber TYPE stringtab.
    "! Get all logged messages
    "!
    "! @parameter rt_protocol | BAPI messages
    METHODS get_protocol
      RETURNING
        VALUE(rt_protocol) TYPE bapirettab .
    "! Log a message with optionally message details
    "!
    "! @parameter msgde | Message details
    METHODS log_message
      IMPORTING
        msgde TYPE zial_cl_log=>t_input_parameters OPTIONAL.
    "! Log exception
    "!
    "! @parameter io_exception | Exception object
    METHODS log_exception
      IMPORTING
        !io_exception TYPE REF TO cx_root .
    "! Log symsg messages
    "!
    "! @parameter is_symsg | SAP system message structure
    METHODS log_sy_message
      IMPORTING
        !is_symsg TYPE symsg.
    "! Log a table of bapi messages
    "!
    "! @parameter it_bapiret | BAPI messages
    METHODS log_bapiret
      IMPORTING
        !it_bapiret TYPE bapirettab .
    "! Log a horizontal line
    "!
    METHODS log_line.
    "! Log warning message
    "!
    "! @parameter msgtx | Message text
    "! @parameter msgno | Message number
    "! @parameter msgv1 | Message variable 1
    "! @parameter msgv2 | Message variable 2
    "! @parameter msgv3 | Message variable 3
    "! @parameter msgv4 | Message variable 4
    "! @parameter msgde | Message details
    METHODS warning
      IMPORTING
        !msgtx TYPE bapi_msg OPTIONAL
        !msgno TYPE symsgno OPTIONAL
        !msgv1 TYPE symsgv OPTIONAL
        !msgv2 TYPE symsgv OPTIONAL
        !msgv3 TYPE symsgv OPTIONAL
        !msgv4 TYPE symsgv OPTIONAL
        !msgde TYPE zial_cl_log=>t_input_parameters OPTIONAL.
    "! Log name of development object which called the function to be logged
    "!
    METHODS log_caller .
    "! Log info message
    "!
    "! @parameter msgtx | Message text
    "! @parameter msgno | Message number
    "! @parameter msgv1 | Message variable 1
    "! @parameter msgv2 | Message variable 2
    "! @parameter msgv3 | Message variable 3
    "! @parameter msgv4 | Message variable 4
    "! @parameter msgde | Message details
    METHODS info
      IMPORTING
        !msgtx TYPE bapi_msg OPTIONAL
        !msgno TYPE symsgno OPTIONAL
        !msgv1 TYPE symsgv OPTIONAL
        !msgv2 TYPE symsgv OPTIONAL
        !msgv3 TYPE symsgv OPTIONAL
        !msgv4 TYPE symsgv OPTIONAL
        !msgde TYPE zial_cl_log=>t_input_parameters OPTIONAL.
    "! Log success message
    "!
    "! @parameter msgtx | Message text
    "! @parameter msgno | Message number
    "! @parameter msgv1 | Message variable 1
    "! @parameter msgv2 | Message variable 2
    "! @parameter msgv3 | Message variable 3
    "! @parameter msgv4 | Message variable 4
    "! @parameter msgde | Message details
    METHODS success
      IMPORTING
        !msgtx TYPE bapi_msg OPTIONAL
        !msgno TYPE symsgno OPTIONAL
        !msgv1 TYPE symsgv OPTIONAL
        !msgv2 TYPE symsgv OPTIONAL
        !msgv3 TYPE symsgv OPTIONAL
        !msgv4 TYPE symsgv OPTIONAL
        !msgde TYPE zial_cl_log=>t_input_parameters OPTIONAL.
    "! Log error message
    "!
    "! @parameter msgtx | Message text
    "! @parameter msgno | Message number
    "! @parameter msgv1 | Message variable 1
    "! @parameter msgv2 | Message variable 2
    "! @parameter msgv3 | Message variable 3
    "! @parameter msgv4 | Message variable 4
    "! @parameter msgde | Message details
    METHODS error
      IMPORTING
        !msgtx TYPE bapi_msg OPTIONAL
        !msgno TYPE symsgno OPTIONAL
        !msgv1 TYPE symsgv OPTIONAL
        !msgv2 TYPE symsgv OPTIONAL
        !msgv3 TYPE symsgv OPTIONAL
        !msgv4 TYPE symsgv OPTIONAL
        !msgde TYPE zial_cl_log=>t_input_parameters OPTIONAL.
    "! Save log to application log and optionally close log instance
    "!
    "! @parameter iv_finalize | Finalize/close log? (Y/N)
    METHODS save
      IMPORTING
        !iv_finalize TYPE abap_bool DEFAULT abap_true.

  PROTECTED SECTION.
    TYPES: BEGIN OF s_msg_details,
             v_id              TYPE balmnr,
             t_input_parameter TYPE rsra_t_alert_definition,
           END OF s_msg_details,
           tt_msg_details TYPE STANDARD TABLE OF s_msg_details WITH DEFAULT KEY.

    DATA: mv_validity_in_days TYPE i VALUE 180,
          mv_process_bgn      TYPE timestampl,
          mv_process_end      TYPE timestampl.

    DATA: mv_caller        TYPE c LENGTH 200,
          mv_callstack_lvl TYPE numc1.

    DATA: mv_msg_text         TYPE bapi_msg,
          mv_msg_type         TYPE symsgty,
          mv_msg_content_type TYPE numc1,
          mv_msg_class        TYPE symsgid,
          ms_msg_params       TYPE bal_s_parm,
          mv_msg_number       TYPE symsgno,
          mv_msg_var1         TYPE symsgv,
          mv_msg_var2         TYPE symsgv,
          mv_msg_var3         TYPE symsgv,
          mv_msg_var4         TYPE symsgv,
          mv_msg_priority     TYPE balprobcl,
          ms_msg_context      TYPE bal_s_cont.

    DATA: mv_log_handle   TYPE balloghndl,
          ms_log_header   TYPE bal_s_log,
          mt_log_protocol TYPE bapirettab,
          mv_log_counter  TYPE i.

    DATA: mv_msg_param_id     TYPE zial_cl_log=>v_message_param_id,
          mt_msg_detail       TYPE tt_msg_details,
          mt_msg_detail_input TYPE zial_cl_log=>t_input_parameters.

    CLASS-METHODS error_handling
      IMPORTING
        !iv_process       TYPE char4
        !iv_subrc         TYPE sysubrc
        !io_exception     TYPE REF TO cx_root
        !is_log_msg       TYPE zial_s_log_msg
      RETURNING
        VALUE(rt_bapiret) TYPE bapirettab.

    METHODS add_msg_to_log_protocol
      IMPORTING
        !is_msg_handle TYPE balmsghndl .
    METHODS build_extnumber
      IMPORTING
        iv_extnumber TYPE balnrext OPTIONAL
        it_extnumber TYPE stringtab OPTIONAL.
    METHODS handle_error
      IMPORTING
        !iv_process   TYPE char4
        !iv_subrc     TYPE sysubrc OPTIONAL
        !io_exception TYPE REF TO cx_root OPTIONAL .
    METHODS set_priority .
    METHODS set_content
      IMPORTING
        !msgid TYPE symsgid
        !msgty TYPE symsgty
        !msgtx TYPE bapi_msg
        !msgno TYPE symsgno
        !msgv1 TYPE symsgv
        !msgv2 TYPE symsgv
        !msgv3 TYPE symsgv
        !msgv4 TYPE symsgv .
    METHODS create_message
      IMPORTING
        !iv_msgid        TYPE symsgid  OPTIONAL
        !iv_msgty        TYPE symsgty
        !iv_msgtx        TYPE bapi_msg OPTIONAL
        !iv_msgno        TYPE symsgno  OPTIONAL
        !iv_msgv1        TYPE symsgv   OPTIONAL
        !iv_msgv2        TYPE symsgv   OPTIONAL
        !iv_msgv3        TYPE symsgv   OPTIONAL
        !iv_msgv4        TYPE symsgv   OPTIONAL
        !it_msgde        TYPE zial_cl_log=>t_input_parameters OPTIONAL
        !iv_is_dummy_msg TYPE abap_bool DEFAULT abap_false.
    METHODS add_msg_by_message_text .
    METHODS add_msg_by_message_object .
    METHODS add_timestamp
      RETURNING
        VALUE(rv_time) TYPE symsgv .
    METHODS build_validity.
    METHODS add_message_context.
    METHODS add_message_callstack.
    METHODS log_duration.
    METHODS det_caller.
    METHODS save_log.
    METHODS add_message_detail.
    METHODS save_msgde
      IMPORTING
        it_new_lognumbers TYPE bal_t_lgnm.
    METHODS create_log.

  PRIVATE SECTION.
    CLASS-DATA: mv_has_error  TYPE abap_bool,
                mv_save_error TYPE abap_bool.

ENDCLASS.


CLASS zial_cl_log_sap IMPLEMENTATION.

  METHOD add_message_callstack.

    CHECK mv_callstack_lvl > 0.

    CASE mv_msg_type.
      WHEN zial_cl_log=>mc_log_type-info.
        CHECK mv_callstack_lvl >= zial_cl_log=>mc_callstack_lvl-info.

      WHEN zial_cl_log=>mc_log_type-success.
        CHECK mv_callstack_lvl >= zial_cl_log=>mc_callstack_lvl-success.

      WHEN zial_cl_log=>mc_log_type-warning.
        CHECK mv_callstack_lvl >= zial_cl_log=>mc_callstack_lvl-warning.

      WHEN zial_cl_log=>mc_log_type-error.
        CHECK mv_callstack_lvl >= zial_cl_log=>mc_callstack_lvl-error.

    ENDCASE.

    zial_cl_session=>get_callstack(
      IMPORTING
        et_callstack = DATA(lt_callstack) ).
    DELETE lt_callstack WHERE mainprogram CS mc_class_name.

    DATA(lv_line) = repeat( val = '-' occ = 80 ).
    APPEND VALUE #( low = lv_line ) TO mt_msg_detail_input.
    APPEND LINES OF VALUE rsra_t_alert_definition( FOR <s_callstack> IN lt_callstack
                                                     ( low  = |{ <s_callstack>-mainprogram }=>| &&
                                                              |{ <s_callstack>-event }, | &&
                                                              |{ TEXT-002 } { <s_callstack>-line }| ) ) TO mt_msg_detail_input.

  ENDMETHOD.


  METHOD add_message_context.

    zial_cl_session=>get_context(
      IMPORTING
        ev_program   = DATA(lv_program)
        ev_blockname = DATA(lv_include)
        ev_line      = DATA(lv_line) ).

    ms_msg_context = VALUE #( value   = VALUE zial_s_log_context( program = lv_program
                                                                  include = lv_include
                                                                  line    = lv_line )
                              tabname = zial_cl_log=>mc_log_context_struct ).

  ENDMETHOD.


  METHOD add_message_detail.

    CHECK mt_msg_detail_input IS NOT INITIAL.

    " Add message identifier, example SBAL_CALLBACK
    ms_msg_params-callback = VALUE #( userexitp = zial_cl_log=>mc_msgde_callback-report
                                      userexitf = zial_cl_log=>mc_msgde_callback-routine
                                      userexitt = space ).

    mv_msg_param_id = mv_msg_param_id + 1.
    APPEND VALUE #( parname  = zial_cl_log=>mc_msg_ident
                    parvalue = mv_msg_param_id ) TO ms_msg_params-t_par.

    APPEND VALUE #( v_id              = mv_msg_param_id
                    t_input_parameter = mt_msg_detail_input ) TO mt_msg_detail.

    CLEAR: mt_msg_detail_input.

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
      EXPORTING
        i_log_handle     = me->mv_log_handle
        i_s_msg          = ls_msg
      IMPORTING
        e_s_msg_handle   = ls_msg_handle
      EXCEPTIONS
        log_not_found    = 1
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
      EXPORTING
        i_log_handle     = mv_log_handle
        i_msgty          = mv_msg_type
        i_probclass      = mv_msg_priority
        i_s_context      = ms_msg_context
        i_text           = mv_msg_text
        i_s_params       = ms_msg_params
      IMPORTING
        e_s_msg_handle   = ls_msg_handle
      EXCEPTIONS
        log_not_found    = 1
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
      EXPORTING
        i_s_msg_handle = is_msg_handle
      IMPORTING
        e_s_msg        = ls_msg
      EXCEPTIONS
        log_not_found  = 1
        msg_not_found  = 2
        OTHERS         = 3.

    CASE sy-subrc.
      WHEN 0.
        ADD 1 TO me->mv_log_counter.

        DATA(ls_bapiret2) = CORRESPONDING bapiret2( ls_msg MAPPING id         = msgid
                                                                   type       = msgty
                                                                   number     = msgno
                                                                   message_v1 = msgv1
                                                                   message_v2 = msgv2
                                                                   message_v3 = msgv3
                                                                   message_v4 = msgv4 ).

        CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
          EXPORTING
            id         = ls_msg-msgid
            number     = ls_msg-msgno
            textformat = 'RTF'
            message_v1 = ls_msg-msgv1
            message_v2 = ls_msg-msgv2
            message_v3 = ls_msg-msgv3
            message_v4 = ls_msg-msgv4
          IMPORTING
            message    = ls_bapiret2-message.

        APPEND ls_bapiret2 TO mt_log_protocol.

    ENDCASE.

  ENDMETHOD.


  METHOD add_timestamp.

    DATA: lv_timestamp TYPE timestampl.

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

    LOOP AT lt_extnumber ASSIGNING FIELD-SYMBOL(<v_extnumber>).

      CASE sy-tabix.
        WHEN 1.
          me->ms_log_header-extnumber = |{ <v_extnumber> }|.

        WHEN OTHERS.
          me->ms_log_header-extnumber = |{ me->ms_log_header-extnumber } { <v_extnumber> }|.

      ENDCASE.

    ENDLOOP.

    IF lt_extnumber IS INITIAL.

      SELECT SINGLE subobjtxt
        FROM balsubt
        INTO @ms_log_header-extnumber
        WHERE spras     EQ @sy-langu
          AND object    EQ @ms_log_header-object
          AND subobject EQ @ms_log_header-subobject.

    ENDIF.

  ENDMETHOD.


  METHOD build_validity.

    CHECK mv_validity_in_days > 0.

    ms_log_header-aldate_del = sy-datum + mv_validity_in_days.

  ENDMETHOD.


  METHOD create_message.

    set_content( msgid = iv_msgid
                 msgty = iv_msgty
                 msgtx = iv_msgtx
                 msgno = iv_msgno
                 msgv1 = iv_msgv1
                 msgv2 = iv_msgv2
                 msgv3 = iv_msgv3
                 msgv4 = iv_msgv4 ).

    IF iv_is_dummy_msg EQ abap_false.

      mt_msg_detail_input = it_msgde.

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

    CLEAR: ms_msg_params.

  ENDMETHOD.


  METHOD det_caller.

    CHECK mv_caller CO ' _0'.

    zial_cl_session=>get_callstack(
      IMPORTING
        ev_function = DATA(lv_function)
        ev_method   = DATA(lv_method)
        ev_class    = DATA(lv_class)
        ev_report   = DATA(lv_report) ).

    IF lv_function IS NOT INITIAL.

      mv_caller = lv_function.

    ELSEIF lv_class IS NOT INITIAL
      AND lv_method IS NOT INITIAL.

      CONCATENATE lv_class '=>' lv_method INTO mv_caller.

    ELSEIF lv_report IS NOT INITIAL.

      mv_caller = lv_report.

    ELSE.

      RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD error.

    create_message( iv_msgty = zial_cl_log=>mc_log_type-error
                    iv_msgtx = msgtx
                    iv_msgno = msgno
                    iv_msgv1 = msgv1
                    iv_msgv2 = msgv2
                    iv_msgv3 = msgv3
                    iv_msgv4 = msgv4
                    it_msgde = msgde ).

  ENDMETHOD.


  METHOD handle_error.

    " Ensures that error handling is only executed once for each error to avoid endless loop
    CHECK mv_has_error  EQ abap_false
      AND mv_save_error EQ abap_false.
    mv_has_error = abap_true.

*** Backup input data
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
    MESSAGE e001(zial_log) INTO DATA(lv_msg).
    log_message( ).

*** Close existing log and create a new one for error handling
    save( ).

    DATA(lo_log_sap) = NEW zial_cl_log_sap( iv_object    = zial_cl_log=>mc_dflt_log_object
                                            iv_subobject = zial_cl_log=>mc_log_subobject_log
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

    MESSAGE e000(zial_log) WITH iv_process INTO DATA(lv_msg).
    APPEND zial_cl_log=>to_bapiret( iv_symsg = abap_true ) TO rt_bapiret.

*** Log general log data
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

    APPEND zial_cl_log=>to_bapiret( iv_symsg = abap_true ) TO rt_bapiret.

*** Log process-specific log data
    CASE iv_process.
      WHEN zial_cl_log=>mc_log_process-init.
        " Nothing to log

      WHEN zial_cl_log=>mc_log_process-create.
        IF is_log_msg-msg-msgtx CN ' _0'.

          MESSAGE e015(zial_log) WITH is_log_msg-msg-msgty INTO lv_msg.
          APPEND zial_cl_log=>to_bapiret( iv_symsg = abap_true ) TO rt_bapiret.

        ELSEIF is_log_msg-msg-msgno CN ' _0'.

          MESSAGE e003(zial_log) WITH is_log_msg-msg-msgno
                                      is_log_msg-msg-msgid INTO lv_msg.
          APPEND zial_cl_log=>to_bapiret( iv_symsg = abap_true ) TO rt_bapiret.

          MESSAGE e004(zial_log) WITH is_log_msg-msg-msgv1
                                      is_log_msg-msg-msgv2
                                      is_log_msg-msg-msgv3
                                      is_log_msg-msg-msgv4 INTO lv_msg.
          APPEND zial_cl_log=>to_bapiret( iv_symsg = abap_true ) TO rt_bapiret.

        ELSE.

          MESSAGE e002(zial_log) INTO lv_msg.
          APPEND zial_cl_log=>to_bapiret( iv_symsg = abap_true ) TO rt_bapiret.

        ENDIF.

      WHEN zial_cl_log=>mc_log_process-exception.
        DATA(lo_exc_descr) = NEW cl_instance_description( io_exception ).

        MESSAGE e005(zial_log) WITH lo_exc_descr->class_name INTO lv_msg.
        APPEND zial_cl_log=>to_bapiret( iv_symsg = abap_true ) TO rt_bapiret.

      WHEN zial_cl_log=>mc_log_process-save.
        " Nothing to log

    ENDCASE.

  ENDMETHOD.


  METHOD get_protocol.

    rt_protocol = mt_log_protocol.

  ENDMETHOD.


  METHOD info.

    create_message( iv_msgty = zial_cl_log=>mc_log_type-info
                    iv_msgtx = msgtx
                    iv_msgno = msgno
                    iv_msgv1 = msgv1
                    iv_msgv2 = msgv2
                    iv_msgv3 = msgv3
                    iv_msgv4 = msgv4
                    it_msgde = msgde ).

  ENDMETHOD.


  METHOD constructor.

    GET TIME STAMP FIELD me->mv_process_bgn.

    me->mv_callstack_lvl  = iv_callstack_lvl.

    me->ms_log_header = VALUE #( object    = iv_object
                                 subobject = iv_subobject
                                 aluser    = sy-uname
                                 aldate    = sy-datum
                                 altime    = sy-uzeit ).

  ENDMETHOD.


  METHOD init.

    GET TIME STAMP FIELD mv_process_bgn.

    me->build_extnumber( iv_extnumber = iv_extnumber
                         it_extnumber = it_extnumber ).

    me->build_validity( ).

    me->create_log( ).

    me->log_caller( ).

  ENDMETHOD.


  METHOD log_bapiret.

    LOOP AT it_bapiret ASSIGNING FIELD-SYMBOL(<ls_bapiret>).

      me->create_message( iv_msgty = <ls_bapiret>-type
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

    me->det_caller( ).

    create_message( iv_msgty        = zial_cl_log=>mc_log_type-success
                    iv_msgtx        = |***** { mv_caller } at { me->add_timestamp( ) } *****|
                    iv_is_dummy_msg = abap_true ).

  ENDMETHOD.


  METHOD log_duration.

    IF    me->mv_process_end IS NOT INITIAL
      AND me->mv_process_bgn IS NOT INITIAL.

      TRY.
          DATA(lv_duration) = cl_abap_tstmp=>subtract( tstmp1 = mv_process_end
                                                       tstmp2 = mv_process_bgn ) * 1000.

          me->create_message( iv_msgty        = zial_cl_log=>mc_log_type-success
                              iv_msgtx        = CONV #( zial_cl_text=>get_by_enc_text( |{ TEXT-001 } { lv_duration } ms| ) )
                              iv_is_dummy_msg = abap_true ).

        CATCH cx_root.
          " Duration couldn't be calculated

      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD log_exception.

    " Determine OTR-Text by exception class
    mv_msg_text = io_exception->get_text( ).

    " Determine exception class name
    DATA(lo_exc_descr) = NEW cl_instance_description( io_exception ).

    ms_msg_params = VALUE #( altext = 'SBAL_EXCEPTION_01'
                              t_par  = VALUE #( ( parname  = 'EXCEPTION'
                                                  parvalue = lo_exc_descr->class_name ) ) ).

    me->create_message( iv_msgty = zial_cl_log=>mc_log_type-error
                    iv_msgtx = mv_msg_text
                    iv_msgno = mv_msg_number
                    iv_msgv1 = mv_msg_var1
                    iv_msgv2 = mv_msg_var2
                    iv_msgv3 = mv_msg_var3
                    iv_msgv4 = mv_msg_var4 ).

  ENDMETHOD.


  METHOD log_line.

    create_message( iv_msgty        = zial_cl_log=>mc_log_type-success
                    iv_msgtx        = repeat( val = '-' occ = 255 )
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
                    it_msgde = msgde ).

  ENDMETHOD.


  METHOD log_sy_message.

    mv_msg_class = is_symsg-msgid.

    CASE is_symsg-msgty.
      WHEN zial_cl_log=>mc_log_type-info.
        info( msgno = is_symsg-msgno
              msgv1 = is_symsg-msgv1
              msgv2 = is_symsg-msgv2
              msgv3 = is_symsg-msgv3
              msgv4 = is_symsg-msgv4 ).

      WHEN zial_cl_log=>mc_log_type-success.
        success( msgno = is_symsg-msgno
                 msgv1 = is_symsg-msgv1
                 msgv2 = is_symsg-msgv2
                 msgv3 = is_symsg-msgv3
                 msgv4 = is_symsg-msgv4 ).

      WHEN zial_cl_log=>mc_log_type-warning.
        warning( msgno = is_symsg-msgno
                 msgv1 = is_symsg-msgv1
                 msgv2 = is_symsg-msgv2
                 msgv3 = is_symsg-msgv3
                 msgv4 = is_symsg-msgv4 ).

      WHEN zial_cl_log=>mc_log_type-error.
        error( msgno = is_symsg-msgno
               msgv1 = is_symsg-msgv1
               msgv2 = is_symsg-msgv2
               msgv3 = is_symsg-msgv3
               msgv4 = is_symsg-msgv4 ).

    ENDCASE.

  ENDMETHOD.


  METHOD save.

    IF    (    mv_has_error  EQ abap_false
            OR mv_save_error EQ abap_true )
      AND me->mv_log_counter > 0.

      IF iv_finalize EQ abap_true.
        GET TIME STAMP FIELD me->mv_process_end.
        me->log_duration( ).
        me->log_line( ).
      ENDIF.

      me->save_log( ).

    ELSE.

      CALL FUNCTION 'BAL_LOG_DELETE'
        EXPORTING
          i_log_handle = mv_log_handle
        EXCEPTIONS
          OTHERS       = 0.

    ENDIF.

    IF iv_finalize EQ abap_true.
      CLEAR: me->ms_log_header,
             me->mv_log_handle.
    ENDIF.

  ENDMETHOD.


  METHOD save_log.

    DATA(lt_log_handles)    = VALUE bal_t_logh( ( me->mv_log_handle ) ).
    DATA(lt_new_lognumbers) = VALUE bal_t_lgnm( ).

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle       = lt_log_handles
        i_save_all           = abap_false
        i_2th_connection     = abap_true
        i_2th_connect_commit = abap_true
      IMPORTING
        e_new_lognumbers     = lt_new_lognumbers
      EXCEPTIONS
        log_not_found        = 1
        save_not_allowed     = 2
        numbering_error      = 3
        OTHERS               = 4.

    CASE sy-subrc.
      WHEN 0.
        me->save_msgde( lt_new_lognumbers ).

      WHEN OTHERS.
        CALL FUNCTION 'BAL_LOG_DELETE'
          EXPORTING
            i_log_handle  = mv_log_handle
          EXCEPTIONS
            log_not_found = 1
            OTHERS        = 2.

        CLEAR: me->ms_log_header,
               me->mv_log_handle.

        me->handle_error( iv_process = zial_cl_log=>mc_log_process-save
                          iv_subrc   = sy-subrc ).

    ENDCASE.

  ENDMETHOD.


  METHOD save_msgde.

    CHECK it_new_lognumbers IS NOT INITIAL.

    ASSIGN it_new_lognumbers[ lines( it_new_lognumbers ) ] TO FIELD-SYMBOL(<ls_new_lognumber>).
    CHECK me->mt_msg_detail  IS NOT INITIAL
      AND <ls_new_lognumber> IS ASSIGNED.

    " EWM: /SCWM/DLV_EXPORT_LOG
    EXPORT mt_msg_detail FROM me->mt_msg_detail TO DATABASE bal_indx(al) ID <ls_new_lognumber>-lognumber.

    CLEAR: me->mt_msg_detail.

  ENDMETHOD.


  METHOD set_content.

    DATA: lv_msg_var TYPE string.

    mv_msg_class = msgid.
    mv_msg_type  = msgty.

    IF msgtx IS NOT INITIAL.

      mv_msg_text = msgtx.

      DO 4 TIMES.

        CLEAR: lv_msg_var.

        CASE sy-index.
          WHEN 1.
            lv_msg_var = msgv1.

          WHEN 2.
            lv_msg_var = msgv2.

          WHEN 3.
            lv_msg_var = msgv3.

          WHEN 4.
            lv_msg_var = msgv4.

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

    ELSEIF msgno CN ' _'.

      mv_msg_number = msgno.

      mv_msg_var1 = msgv1.
      mv_msg_var2 = msgv2.
      mv_msg_var3 = msgv3.
      mv_msg_var4 = msgv4.

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


  METHOD success.

    create_message( iv_msgty = zial_cl_log=>mc_log_type-success
                    iv_msgtx = msgtx
                    iv_msgno = msgno
                    iv_msgv1 = msgv1
                    iv_msgv2 = msgv2
                    iv_msgv3 = msgv3
                    iv_msgv4 = msgv4
                    it_msgde = msgde ).

  ENDMETHOD.


  METHOD warning.

    create_message( iv_msgty = zial_cl_log=>mc_log_type-warning
                    iv_msgtx = msgtx
                    iv_msgno = msgno
                    iv_msgv1 = msgv1
                    iv_msgv2 = msgv2
                    iv_msgv3 = msgv3
                    iv_msgv4 = msgv4
                    it_msgde = msgde ).

  ENDMETHOD.


  METHOD create_log.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = me->ms_log_header
      IMPORTING
        e_log_handle            = me->mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF    sy-subrc     NE 0
      AND mv_has_error EQ abap_false.
      me->handle_error( iv_process = zial_cl_log=>mc_log_process-init
                        iv_subrc   = sy-subrc ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
