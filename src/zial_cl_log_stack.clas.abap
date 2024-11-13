CLASS zial_cl_log_stack DEFINITION
  PUBLIC FINAL
  CREATE PROTECTED.

  PUBLIC SECTION.
    TYPES: BEGIN OF s_log_stack,
             log_handle TYPE balloghndl,
             instance   TYPE zial_cl_log_const=>r_log_instance,
           END OF s_log_stack.
    TYPES t_log_stack TYPE SORTED TABLE OF s_log_stack WITH UNIQUE KEY log_handle.

    CLASS-METHODS free.

    CLASS-METHODS remove
      IMPORTING iv_log_handle TYPE balloghndl.

    CLASS-METHODS push
      IMPORTING io_instance TYPE zial_cl_log_const=>r_log_instance.

    CLASS-METHODS pop
      RETURNING VALUE(rs_log_stack) TYPE s_log_stack.

    CLASS-METHODS is_empty
      RETURNING VALUE(rv_result) TYPE abap_bool.

  PROTECTED SECTION.
    CLASS-DATA mt_log_stack TYPE t_log_stack. " LIFO: Last log initiated is first to be saved

ENDCLASS.


CLASS zial_cl_log_stack IMPLEMENTATION.

  METHOD remove.

    DELETE mt_log_stack WHERE log_handle EQ iv_log_handle.

  ENDMETHOD.


  METHOD push.

    INSERT VALUE #( log_handle = io_instance->get_log_handle( )
                    instance   = io_instance ) INTO TABLE mt_log_stack.

  ENDMETHOD.


  METHOD pop.

    rs_log_stack = VALUE #( mt_log_stack[ 1 ] OPTIONAL ).

  ENDMETHOD.


  METHOD is_empty.

    rv_result = xsdbool( mt_log_stack IS INITIAL ).

  ENDMETHOD.


  METHOD free.

    FREE mt_log_stack.

  ENDMETHOD.

ENDCLASS.
