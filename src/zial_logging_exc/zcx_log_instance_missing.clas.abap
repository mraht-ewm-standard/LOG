"! <p class="shorttext synchronized">Log instance is missing</p>
CLASS zcx_log_instance_missing DEFINITION
  PUBLIC
  INHERITING FROM zcx_log FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS log REDEFINITION.

ENDCLASS.


CLASS zcx_log_instance_missing IMPLEMENTATION.

  METHOD log.

    super->log( ).

    MESSAGE e016(zial_log) INTO DATA(lv_msg) ##NEEDED.
    zcx_if_check_class~message = zial_cl_log=>to_bapiret( iv_msgid = sy-msgid
                                                          iv_msgno = sy-msgno ).
    zial_cl_log=>get( )->log_message( ).

  ENDMETHOD.

ENDCLASS.
