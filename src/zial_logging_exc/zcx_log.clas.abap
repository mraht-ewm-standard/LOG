"! <p class="shorttext synchronized" lang="en">Log-specific exception</p>
CLASS zcx_log DEFINITION
  PUBLIC
  INHERITING FROM zcx_static_check
  CREATE PUBLIC
  ABSTRACT.

  PUBLIC SECTION.
    CLASS-METHODS enable_log_parent
      IMPORTING
        iv_log_enabled TYPE abap_bool.
    CLASS-METHODS is_log_parent_enabled
      RETURNING
        VALUE(rv_log_enabled) TYPE cx_bool.

  PROTECTED SECTION.
    CLASS-DATA log_parent_enabled TYPE cx_bool VALUE mc_log_enabled-undef.

ENDCLASS.


CLASS zcx_log IMPLEMENTATION.

  METHOD enable_log_parent.
    log_parent_enabled = det_bool( iv_log_enabled ).
  ENDMETHOD.


  METHOD is_log_parent_enabled.
    rv_log_enabled = log_parent_enabled.
  ENDMETHOD.

ENDCLASS.
