"! <p class="shorttext synchronized" lang="en">Log instance is missing</p>
CLASS zcx_log_instance_missing DEFINITION
  PUBLIC
  INHERITING FROM zcx_log
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        textid     LIKE if_t100_message=>t100key OPTIONAL
        previous   LIKE previous OPTIONAL
        log        TYPE abap_bool OPTIONAL
        message    TYPE bapiret2 OPTIONAL
        subrc      TYPE sysubrc DEFAULT sy-subrc
        input_data TYPE rsra_t_alert_definition OPTIONAL .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_log_instance_missing IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( textid     = textid
                        previous   = previous
                        log        = abap_false
                        message    = message
                        subrc      = subrc
                        input_data = input_data ).

    MESSAGE e016(zial_log) INTO DATA(lv_msg).
    me->message = zial_cl_log=>to_bapiret( iv_msgid = sy-msgid
                                           iv_msgno = sy-msgno ).

    object_type = mc_object_type.

  ENDMETHOD.

ENDCLASS.
