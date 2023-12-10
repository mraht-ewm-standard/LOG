"! <p class="shorttext synchronized" lang="en">Log instance is missing</p>
CLASS zcx_log_instance_missing DEFINITION
  PUBLIC
  INHERITING FROM zcx_log
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_cx_class.

  PROTECTED SECTION.
    CLASS-DATA log_class_enabled TYPE cx_bool VALUE mc_log_enabled-undef.

    METHODS log_messages REDEFINITION.

ENDCLASS.


CLASS zcx_log_instance_missing IMPLEMENTATION.

  METHOD log_messages.

    super->log_messages( ).

    MESSAGE e016(zial_log) INTO DATA(lv_msg).
    me->message = zial_cl_log=>to_bapiret( iv_msgid = sy-msgid
                                           iv_msgno = sy-msgno ).
    zial_cl_log=>get( )->log_message( ).

  ENDMETHOD.


  METHOD zif_cx_class~enable_log.
    log_class_enabled = det_bool( iv_enable ).
  ENDMETHOD.


  METHOD zif_cx_class~is_log_enabled.
    rv_is_enabled = log_class_enabled.
  ENDMETHOD.

ENDCLASS.
