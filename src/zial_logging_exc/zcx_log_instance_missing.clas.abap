"! <p class="shorttext synchronized" lang="en">Log instance is missing</p>
CLASS zcx_log_instance_missing DEFINITION
  PUBLIC
  INHERITING FROM cx_dynamic_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    METHODS constructor
      IMPORTING
        !previous LIKE previous OPTIONAL .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_log_instance_missing IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    message e016(zial_log) into data(lv_msg).
    if_t100_message~t100key = zial_cl_log=>to_textid( iv_msgid = sy-msgid
                                                      iv_msgno = sy-msgno ).

  ENDMETHOD.

ENDCLASS.
