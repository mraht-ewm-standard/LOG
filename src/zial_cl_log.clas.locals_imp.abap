*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS ltc_log DEFINITION DEFERRED.
CLASS zial_cl_log DEFINITION LOCAL FRIENDS ltc_log.

CLASS lcx_error DEFINITION INHERITING FROM cx_static_check.

  PUBLIC SECTION.
    INTERFACES if_t100_message.
    INTERFACES if_t100_dyn_msg.

ENDCLASS.
