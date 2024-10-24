*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
"! <p class="shorttext synchronized">Current Application Session/Thread</p>
CLASS lcl_session DEFINITION FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF s_abap_callstack_entry,
             mainprogram TYPE dbgsrepid,
             include     TYPE dbgsrepid,
             line        TYPE dbglinno,
             eventtype   TYPE dbglevtype,
             event       TYPE dbglevent,
             flag_system TYPE c LENGTH 1,
           END OF s_abap_callstack_entry,
           tt_abap_callstack TYPE STANDARD TABLE OF s_abap_callstack_entry WITH DEFAULT KEY.

    "! Get application context
    "!
    "! @parameter ev_program   | Program name
    "! @parameter ev_blockname | Source code block name, e. g. name of include
    "! @parameter ev_line      | Source code line
    CLASS-METHODS get_context
      EXPORTING ev_program   TYPE syrepid
                ev_blockname TYPE include
                ev_line      TYPE int4.

    "! Get application callstack
    "!
    "! @parameter iv_exclude_internal | Exclude internal functions? (Y/N)
    "! @parameter ev_method           | Method name
    "! @parameter ev_class            | CLass name
    "! @parameter ev_report           | Report name
    "! @parameter ev_function         | Function name
    "! @parameter et_callstack        | Callstack
    CLASS-METHODS get_callstack
      IMPORTING iv_exclude_internal TYPE abap_bool DEFAULT abap_true
      EXPORTING ev_method           TYPE dbglevent
                ev_class            TYPE dbgsrepid
                ev_report           TYPE dbgsrepid
                ev_function         TYPE dbglevent
                et_callstack        TYPE tt_abap_callstack.

ENDCLASS.


CLASS lcl_session IMPLEMENTATION.

  METHOD get_callstack.

    DATA ls_aut_callstack TYPE s_abap_callstack_entry.
    DATA lt_aut_callstack TYPE tt_abap_callstack.

    DATA lv_find_offset   TYPE i.
    DATA lv_mainprogram   TYPE dbgsrepid.

    FIELD-SYMBOLS <ls_aut_callstack> TYPE s_abap_callstack_entry.

    CALL 'ABAP_CALLSTACK' ID 'DEPTH' FIELD -10 ID 'CALLSTACK' FIELD lt_aut_callstack. "#EC CI_CCALL

    DATA(lt_r_excl_includes) = VALUE rseloption( sign   = 'I'
                                                 option = 'CP'
                                                 ( low = '*ZIAL_CL_LOG*' )
                                                 ( low = '*ZIAL_CL_SESSION*' )
                                                 ( low = '*LCL_SESSION*' )
                                                 ( low = '*CX_*' ) ).

    IF et_callstack IS SUPPLIED.
      et_callstack = lt_aut_callstack.
      IF iv_exclude_internal EQ abap_true.
        DELETE et_callstack WHERE include IN lt_r_excl_includes.
      ENDIF.
    ENDIF.

    LOOP AT lt_aut_callstack ASSIGNING <ls_aut_callstack> WHERE include NOT IN lt_r_excl_includes.
      ls_aut_callstack = <ls_aut_callstack>.
      EXIT.
    ENDLOOP.

    IF ls_aut_callstack-mainprogram CS '='.
      FIND FIRST OCCURRENCE OF '=' IN ls_aut_callstack-mainprogram  MATCH OFFSET lv_find_offset.
      lv_mainprogram = ls_aut_callstack-mainprogram+0(lv_find_offset).
    ELSE.
      lv_mainprogram = ls_aut_callstack-mainprogram.
    ENDIF.
    CASE ls_aut_callstack-eventtype.
      WHEN 'FUNCTION'.
        " Read function name
        ev_function = ls_aut_callstack-event.

      WHEN 'METHOD'.
        " Read class name
        ev_class = lv_mainprogram.

        " Read method name
        ev_method = ls_aut_callstack-event.

      WHEN 'EVENT' OR 'FORM'.
        " Read program name
        ev_report = lv_mainprogram.

      WHEN OTHERS.
        RETURN.

    ENDCASE.

  ENDMETHOD.


  METHOD get_context.

    DATA(lt_callstack) = VALUE abap_callstack( ).

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING callstack = lt_callstack.

    DELETE lt_callstack WHERE mainprogram CS 'CL_LOG'
                           OR mainprogram CS 'CL_SESSION'.

    ASSIGN lt_callstack[ 1 ] TO FIELD-SYMBOL(<ls_callstack>).
    ev_program   = <ls_callstack>-mainprogram.
    ev_blockname = <ls_callstack>-blockname.
    ev_line      = <ls_callstack>-line.

  ENDMETHOD.

ENDCLASS.
