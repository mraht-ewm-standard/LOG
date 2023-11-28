*&---------------------------------------------------------------------*
*& Report zial_r_application_log
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zial_r_application_log.

INCLUDE zial_r_application_log_top.
INCLUDE zial_r_application_log_sel.
INCLUDE zial_r_application_log_cls.

INITIALIZATION.
  lcl_application=>on_init( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_msgid-low.
  lcl_application=>on_value_req_msgid( ).

AT SELECTION-SCREEN.
  lcl_application=>at_selection_screen( ).
