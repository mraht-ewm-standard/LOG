"! <p class="shorttext synchronized">Logging: EWM Log</p>
CLASS zial_cl_log_ewm DEFINITION
  PUBLIC
  INHERITING FROM zial_cl_log_sap
  CREATE PROTECTED
  GLOBAL FRIENDS zial_cl_log.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF mc_default_ewm,
                 class_name    TYPE seoclsname VALUE 'ZIAL_CL_LOG_EWM',
                 log_object    TYPE balobj_d   VALUE '/SCWM/WME',
                 log_subobject TYPE balsubobj  VALUE 'LOG_GENERAL',
               END OF mc_default_ewm.

    CLASS-METHODS class_constructor.

    CLASS-METHODS to_bapiret
      IMPORTING it_dm_messages       TYPE /scdl/dm_message_tab OPTIONAL
                it_wm_messages       TYPE /scwm/t_messages     OPTIONAL
      RETURNING VALUE(rt_bapirettab) TYPE bapirettab.

    METHODS set_expiry_date REDEFINITION.
    METHODS set_lgnum       REDEFINITION.
    METHODS log_api_message REDEFINITION.

    METHODS log_dm_messages REDEFINITION.
    METHODS log_saplog      REDEFINITION.

  PROTECTED SECTION.
    CLASS-DATA mo_instance TYPE REF TO zial_cl_log_ewm.

    DATA mv_lgnum   TYPE /scwm/lgnum.
    DATA mo_sap_log TYPE REF TO /scwm/cl_log.

    METHODS constructor
      IMPORTING iv_lgnum       TYPE /scwm/lgnum         OPTIONAL
                io_sap_log     TYPE REF TO /scwm/cl_log OPTIONAL
                iv_object      TYPE balobj_d            DEFAULT mc_default_ewm-log_object
                iv_subobject   TYPE balsubobj           DEFAULT mc_default_ewm-log_subobject
                iv_extnumber   TYPE balnrext            OPTIONAL
                it_extnumber   TYPE stringtab           OPTIONAL
                iv_log_part_id TYPE i                   DEFAULT 0.

    METHODS add_msg_by_message_object REDEFINITION.
    METHODS add_msg_by_message_text   REDEFINITION.

  PRIVATE SECTION.

ENDCLASS.


CLASS zial_cl_log_ewm IMPLEMENTATION.

  METHOD class_constructor.

    zial_cl_log=>set_default_log( iv_object    = mc_default_ewm-log_object
                                  iv_subobject = mc_default_ewm-log_subobject ).

  ENDMETHOD.


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

    " Note: Configure Z-Subobject of Object
    " /SCWM/WME in Transaction/SCWM/ACTLOG
    DATA(ls_log_act) = VALUE /scwm/log_act( ).
    CALL FUNCTION '/SCWM/LOG_ACT_READ_SINGLE'
      EXPORTING  iv_lgnum     = mv_lgnum
                 iv_subobject = ms_log-hdr-subobject
      IMPORTING  es_log_act   = ls_log_act
      EXCEPTIONS not_found    = 1
                 OTHERS       = 2.

    CASE sy-subrc.
      WHEN 0.
        super->set_expiry_date( ls_log_act-validity ).

      WHEN OTHERS.
        super->set_expiry_date( ).

    ENDCASE.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( iv_object      = iv_object
                        iv_subobject   = iv_subobject
                        iv_extnumber   = iv_extnumber
                        it_extnumber   = it_extnumber
                        iv_log_part_id = iv_log_part_id ).

    zial_cl_log=>set_default_log( iv_object    = mc_default_ewm-log_object
                                  iv_subobject = mc_default_ewm-log_subobject ).

    mv_lgnum   = iv_lgnum.
    mo_sap_log = io_sap_log.

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


  METHOD set_lgnum.

    CHECK ir_v_lgnum IS BOUND.

    mv_lgnum = ir_v_lgnum->*.

  ENDMETHOD.


  METHOD log_api_message.

    CHECK ir_o_api_message IS BOUND.

    ir_o_api_message->get_messages( IMPORTING et_bapiret = DATA(lt_bapiret) ).
    log_bapiret( lt_bapiret ).

  ENDMETHOD.


  METHOD log_saplog.

    CHECK ir_o_log IS BOUND.

    log_bapiret( ir_o_log->get_prot( ) ).

  ENDMETHOD.


  METHOD log_dm_messages.

    CHECK ir_t_dm_messages IS BOUND.

    LOOP AT ir_t_dm_messages->* ASSIGNING FIELD-SYMBOL(<ls_dm_message>).
      log_symsg( is_symsg = CORRESPONDING #( <ls_dm_message> ) ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
