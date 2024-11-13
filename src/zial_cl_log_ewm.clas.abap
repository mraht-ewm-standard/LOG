"! <p class="shorttext synchronized">Logging: EWM Log</p>
CLASS zial_cl_log_ewm DEFINITION
  PUBLIC
  INHERITING FROM zial_cl_log_sap
  CREATE PROTECTED
  GLOBAL FRIENDS zial_cl_log.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_lgnum       TYPE /scwm/lgnum         OPTIONAL
                io_sap_log     TYPE REF TO /scwm/cl_log OPTIONAL
                iv_object      TYPE balobj_d            DEFAULT '/SCWM/WME'
                iv_subobject   TYPE balsubobj           DEFAULT 'LOG_GENERAL'
                iv_extnumber   TYPE balnrext            OPTIONAL
                it_extnumber   TYPE stringtab           OPTIONAL
                iv_log_part_id TYPE i                   DEFAULT 0.

    "! Log SAP log messages
    "!
    "! @parameter io_log | SAP log object
    METHODS log_saplog
      IMPORTING io_log TYPE REF TO /scwm/cl_log.

    "! Log API messages
    "!
    "! @parameter io_api_message | API log object
    METHODS log_api_message
      IMPORTING io_api_message TYPE REF TO /scwm/if_api_message.

    "! Log delivery management messages
    "!
    "! @parameter it_dm_message | Delivery management log messages
    METHODS log_dm_message
      IMPORTING it_dm_message TYPE /scdl/dm_message_tab.

    "! Set warehouse number
    "!
    "! @parameter iv_lgnum | Warehouse number
    METHODS set_lgnum
      IMPORTING iv_lgnum TYPE /scwm/lgnum.

    CLASS-METHODS to_bapiret
      IMPORTING it_dm_messages       TYPE /scdl/dm_message_tab OPTIONAL
                it_wm_messages       TYPE /scwm/t_messages     OPTIONAL
      RETURNING VALUE(rt_bapirettab) TYPE bapirettab.

  PROTECTED SECTION.
    CLASS-DATA mo_instance TYPE REF TO zial_cl_log_ewm.

    DATA mv_lgnum   TYPE /scwm/lgnum.
    DATA mo_sap_log TYPE REF TO /scwm/cl_log.

    METHODS set_expiry_date           REDEFINITION.
    METHODS add_msg_by_message_object REDEFINITION.
    METHODS add_msg_by_message_text   REDEFINITION.

ENDCLASS.


CLASS zial_cl_log_ewm IMPLEMENTATION.

  METHOD add_msg_by_message_object.

    IF mo_sap_log IS BOUND.
      mo_sap_log->add_message( ip_msgty = ms_log-msg-msgty
                               ip_msgid = ms_log-msg-msgid
                               ip_msgno = ms_log-msg-msgno
                               ip_msgv1 = ms_log-msg-msgv1
                               ip_msgv2 = ms_log-msg-msgv2
                               ip_msgv3 = ms_log-msg-msgv3
                               ip_msgv4 = ms_log-msg-msgv4 ).
    ENDIF.

    super->add_msg_by_message_object( ms_log ).

  ENDMETHOD.


  METHOD add_msg_by_message_text.

    IF mo_sap_log IS BOUND.
      mo_sap_log->add_message( ip_msgty = ms_log-msg-msgty
                               ip_msg   = ms_log-msg-msgtx ).
    ENDIF.

    super->add_msg_by_message_text( ms_log ).

  ENDMETHOD.


  METHOD set_expiry_date.

    DATA(ls_log_act) = VALUE /scwm/log_act( ).

    DO 2 TIMES.

      CASE sy-index.
        WHEN 1.
          " Note: Configure Z-Subobject of Object
          " /SCWM/WME in Transaction/SCWM/ACTLOG
          CALL FUNCTION '/SCWM/LOG_ACT_READ_SINGLE'
            EXPORTING  iv_lgnum     = mv_lgnum
                       iv_subobject = ms_log-hdr-subobject
            IMPORTING  es_log_act   = ls_log_act
            EXCEPTIONS not_found    = 1
                       OTHERS       = 2.

        WHEN 2.
          ls_log_act-lgnum     = mv_lgnum.
          ls_log_act-subobject = ms_log-hdr-subobject.

      ENDCASE.

      IF     sy-subrc            EQ 0
         AND ls_log_act-validity GT 0.

        " Append valid expiration date
        CALL FUNCTION '/SCWM/APP_LOG_EXPIRY_DATE_DET'
          EXPORTING is_log_act = ls_log_act
          CHANGING  cs_log     = ms_log-hdr.

        EXIT.

      ENDIF.

    ENDDO.

    super->set_expiry_date( ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( iv_object      = iv_object
                        iv_subobject   = iv_subobject
                        iv_extnumber   = iv_extnumber
                        it_extnumber   = it_extnumber
                        iv_log_part_id = iv_log_part_id ).

    mv_lgnum   = iv_lgnum.
    mo_sap_log = io_sap_log.

  ENDMETHOD.


  METHOD set_lgnum.

    mv_lgnum = iv_lgnum.

  ENDMETHOD.


  METHOD log_api_message.

    io_api_message->get_messages( IMPORTING et_bapiret = DATA(lt_bapiret) ).

    log_bapiret( lt_bapiret ).

  ENDMETHOD.


  METHOD log_dm_message.

    LOOP AT it_dm_message ASSIGNING FIELD-SYMBOL(<ls_dm_message>).
      log_symsg( is_symsg = CORRESPONDING #( <ls_dm_message> ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD log_saplog.

    CHECK io_log IS BOUND.

    log_bapiret( io_log->get_prot( ) ).

  ENDMETHOD.


  METHOD to_bapiret.

    IF it_dm_messages IS SUPPLIED.
      rt_bapirettab = CORRESPONDING #( it_dm_messages MAPPING id         = msgid
                                                              type       = msgty
                                                              number     = msgno
                                                              message_v1 = msgv1
                                                              message_v2 = msgv2
                                                              message_v3 = msgv3
                                                              message_v4 = msgv4 ).
    ELSEIF it_wm_messages IS SUPPLIED.
      rt_bapirettab = CORRESPONDING #( it_wm_messages MAPPING id         = msgid
                                                              type       = msgty
                                                              number     = msgno
                                                              message_v1 = msgv1
                                                              message_v2 = msgv2
                                                              message_v3 = msgv3
                                                              message_v4 = msgv4 ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
