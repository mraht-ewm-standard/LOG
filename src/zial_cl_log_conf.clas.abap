CLASS zial_cl_log_conf DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF mc_default,
                 "! Not configurable as SAP standard defines the maximum number of
                 "! entries hardcoded in include SBAL_CONSTANTS, CONST_BAL_MSGNUMBER_MAX
                 max_num_of_entries  TYPE i                              VALUE 999999,
                 detail_level        TYPE zial_de_log_detail_level       VALUE zial_cl_log=>mc_detail_level-info,
                 validity_period     TYPE zial_de_log_validity_period    VALUE 180,
                 "! Minimum detail level for which callstack is to be logged in message details
                 level_log_callstack TYPE zial_de_log_level_log_callstck VALUE zial_cl_log=>mc_detail_level-warning,
               END OF mc_default.

    CLASS-METHODS class_constructor.

    CLASS-METHODS get
      IMPORTING iv_object          TYPE balobj_d
                iv_subobject       TYPE balsubobj
                iv_uname           TYPE uname
      RETURNING VALUE(rs_log_conf) TYPE zial_t_log_conf.

  PROTECTED SECTION.
    CLASS-DATA mt_log_conf TYPE zial_tt_log_conf.

    CLASS-METHODS read.

ENDCLASS.


CLASS zial_cl_log_conf IMPLEMENTATION.

  METHOD class_constructor.

    read( ).

  ENDMETHOD.


  METHOD get.

    WHILE rs_log_conf IS INITIAL.

      DATA(lv_index) = sy-index.
      CASE lv_index.
        WHEN 1.
          rs_log_conf = VALUE #( mt_log_conf[ object    = iv_object
                                              subobject = iv_subobject
                                              uname     = iv_uname ] OPTIONAL ).

        WHEN 2.
          rs_log_conf = VALUE #( mt_log_conf[ object    = iv_object
                                              subobject = space
                                              uname     = iv_uname ] OPTIONAL ).

        WHEN 3.
          rs_log_conf = VALUE #( mt_log_conf[ object    = iv_object
                                              subobject = iv_subobject
                                              uname     = space ] OPTIONAL ).

        WHEN 4.
          rs_log_conf = VALUE #( mt_log_conf[ object    = iv_object
                                              subobject = space
                                              uname     = space ] OPTIONAL ).

        WHEN OTHERS.
          EXIT.

      ENDCASE.

    ENDWHILE.

  ENDMETHOD.


  METHOD read.

    SELECT FROM zial_t_log_conf
      FIELDS *
      INTO TABLE @mt_log_conf.

  ENDMETHOD.

ENDCLASS.
