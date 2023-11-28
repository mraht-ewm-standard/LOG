"! <p class="shorttext synchronized" lang="en">{@link ZIAL_CL_LOG}: Exceptions</p>
CLASS zcx_log DEFINITION
  PUBLIC
  INHERITING FROM zcx_static_check
  CREATE PUBLIC
  ABSTRACT.

  PUBLIC SECTION.
    INTERFACES: zif_cx_group.

  PROTECTED SECTION.
    CLASS-DATA log_group_enabled TYPE cx_bool VALUE mc_log_enabled-undef.

ENDCLASS.


CLASS zcx_log IMPLEMENTATION.

  METHOD zif_cx_group~enable_log.
    log_group_enabled = det_bool( iv_enable ).
  ENDMETHOD.


  METHOD zif_cx_group~is_log_enabled.
    rv_is_enabled = log_group_enabled.
  ENDMETHOD.

ENDCLASS.
