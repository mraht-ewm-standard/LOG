"! <p class="shorttext synchronized" lang="en">Log instance is missing</p>
CLASS zcx_log_instance_missing DEFINITION
  PUBLIC
  INHERITING FROM zcx_log
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS is_log_class_enabled
      RETURNING
        VALUE(rv_log) TYPE cx_bool.
    CLASS-METHODS enable_log_class
      IMPORTING
        log_enabled TYPE abap_bool.

  PROTECTED SECTION.
    CLASS-DATA log_class_enabled TYPE cx_bool VALUE zcx_root=>undef.

    METHODS on_construction REDEFINITION.

ENDCLASS.


CLASS zcx_log_instance_missing IMPLEMENTATION.

  METHOD on_construction.

    MESSAGE e016(zial_log) INTO DATA(lv_msg).
    me->message = zial_cl_log=>to_bapiret( iv_msgid = sy-msgid
                                           iv_msgno = sy-msgno ).

    object_type = mc_object_type.

  ENDMETHOD.


  METHOD enable_log_class.
    log_class_enabled = zcx_root=>det_bool( log_enabled ).
  ENDMETHOD.


  METHOD is_log_class_enabled.
    rv_log = log_class_enabled.
  ENDMETHOD.

ENDCLASS.
