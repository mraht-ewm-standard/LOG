"! <p class="shorttext synchronized" lang="en">Log-specific exception</p>
CLASS zcx_log DEFINITION
  PUBLIC
  INHERITING FROM zcx_root
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
    CONSTANTS: mc_object_type TYPE char50 VALUE 'LOG'.

    CLASS-DATA log_parent_enabled TYPE cx_bool VALUE zcx_root=>undef.

ENDCLASS.


CLASS zcx_log IMPLEMENTATION.

  METHOD enable_log_parent.
    log_parent_enabled = zcx_root=>det_bool( iv_log_enabled ).
  ENDMETHOD.


  METHOD is_log_parent_enabled.
    rv_log_enabled = log_parent_enabled.
  ENDMETHOD.

ENDCLASS.
