"! <p class="shorttext synchronized">Logging</p>
CLASS zial_cl_log DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES v_message_param_id TYPE n LENGTH 10.
    TYPES v_input_component  TYPE c LENGTH 150.

    TYPES t_log_stack        TYPE TABLE OF zial_cl_log_const=>r_log_instance WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF mc_msg_content_type,
                 obj TYPE numc1 VALUE 1,
                 txt TYPE numc1 VALUE 2,
               END OF mc_msg_content_type.

    CONSTANTS: BEGIN OF mc_log_process,
                 create    TYPE char4 VALUE 'CREA' ##NO_TEXT,
                 init      TYPE char4 VALUE 'INIT' ##NO_TEXT,
                 save      TYPE char4 VALUE 'SAVE' ##NO_TEXT,
                 exception TYPE char4 VALUE 'EXCP' ##NO_TEXT,
               END OF mc_log_process.

    CONSTANTS: BEGIN OF mc_default,
                 log_object    TYPE balobj_d  VALUE 'SYSLOG' ##NO_TEXT,  " Adjust to your needs
                 log_subobject TYPE balsubobj VALUE 'GENERAL' ##NO_TEXT,
                 msgid         TYPE symsgid   VALUE 'SY',      " 0Q
                 msgno         TYPE symsgno   VALUE '499',     " 000
                 msgty         TYPE symsgty   VALUE 'I',
               END OF mc_default.

    CONSTANTS mc_msg_ident          TYPE c LENGTH 9 VALUE 'MSG_IDENT' ##NO_TEXT.
    CONSTANTS mc_log_context_struct TYPE baltabname VALUE 'ZIAL_S_LOG_CONTEXT' ##NO_TEXT.

    CONSTANTS: BEGIN OF mc_callstack_lvl,
                 none    TYPE numc1 VALUE 0,
                 error   TYPE numc1 VALUE 1,
                 warning TYPE numc1 VALUE 2,
                 success TYPE numc1 VALUE 3,
                 info    TYPE numc1 VALUE 4,
               END OF mc_callstack_lvl.

    CONSTANTS: BEGIN OF mc_log_type,
                 error        TYPE symsgty   VALUE 'E',
                 error_prio   TYPE balprobcl VALUE 1,
                 warning      TYPE symsgty   VALUE 'W',
                 warning_prio TYPE balprobcl VALUE 2,
                 success      TYPE symsgty   VALUE 'S',
                 success_prio TYPE balprobcl VALUE 3,
                 info         TYPE symsgty   VALUE 'I',
                 info_prio    TYPE balprobcl VALUE 4,
               END OF mc_log_type.

    CLASS-DATA mo_gui_docking_container TYPE REF TO cl_gui_docking_container.
    CLASS-DATA mo_gui_alv_grid          TYPE REF TO cl_gui_alv_grid.
    CLASS-DATA mv_sel_msg_param_id      TYPE v_message_param_id.

    CLASS-DATA mo_instance              TYPE zial_cl_log_const=>r_log_instance.
    CLASS-DATA mt_log_stack             TYPE t_log_stack. " LIFO: Last log initiated is first to be saved

    "! Get existing or create and return new log instance
    "!
    "! @parameter ro_instance | Instance
    CLASS-METHODS get
      RETURNING VALUE(ro_instance) LIKE mo_instance.

    "! Create new log instance
    "!
    "! @parameter iv_object        | Log object
    "! @parameter iv_subobject     | Log subobject
    "! @parameter iv_extnumber     | External number / description for a log
    "! @parameter it_extnumber     | External number elements
    "! @parameter iv_callstack_lvl | Level of min. message type for which callstack is to be logged in message details
    "! @parameter ro_instance      | Log instance
    CLASS-METHODS create
      IMPORTING iv_object          TYPE balobj_d  DEFAULT mc_default-log_object
                iv_subobject       TYPE balsubobj DEFAULT mc_default-log_subobject
                iv_extnumber       TYPE balnrext  OPTIONAL
                it_extnumber       TYPE stringtab OPTIONAL
                iv_callstack_lvl   TYPE numc1     DEFAULT mc_callstack_lvl-info
      RETURNING VALUE(ro_instance) LIKE mo_instance.

    "! Save log to application log and optionally close log instance
    "!
    "! @parameter iv_finalize | Finalize/close log? (Y/N)
    CLASS-METHODS save
      IMPORTING iv_finalize TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS to_bapiret
      IMPORTING iv_msgid          TYPE symsgid  DEFAULT sy-msgid
                iv_msgty          TYPE symsgty  DEFAULT sy-msgty
                iv_msgtx          TYPE bapi_msg OPTIONAL
                iv_msgno          TYPE symsgno  DEFAULT sy-msgno
                iv_msgv1          TYPE clike    DEFAULT sy-msgv1
                iv_msgv2          TYPE clike    DEFAULT sy-msgv2
                iv_msgv3          TYPE clike    DEFAULT sy-msgv3
                iv_msgv4          TYPE clike    DEFAULT sy-msgv4
      RETURNING VALUE(rs_bapiret) TYPE bapiret2.

    "! Convert bapiret structure to message string
    "!
    "! @parameter iv_msgid   | Message ID
    "! @parameter iv_msgty   | Message type
    "! @parameter iv_msgno   | Message number
    "! @parameter iv_msgv1   | Message variable 1
    "! @parameter iv_msgv2   | Message variable 2
    "! @parameter iv_msgv3   | Message variable 3
    "! @parameter iv_msgv4   | Message variable 4
    "! @parameter is_bapiret | Bapiret message
    "! @parameter rv_result  | Message as string
    CLASS-METHODS to_string
      IMPORTING iv_msgid         TYPE symsgid  DEFAULT sy-msgid
                iv_msgty         TYPE symsgty  DEFAULT sy-msgty
                iv_msgno         TYPE symsgno  DEFAULT sy-msgno
                iv_msgv1         TYPE symsgv   DEFAULT sy-msgv1
                iv_msgv2         TYPE symsgv   DEFAULT sy-msgv2
                iv_msgv3         TYPE symsgv   DEFAULT sy-msgv3
                iv_msgv4         TYPE symsgv   DEFAULT sy-msgv4
                is_bapiret       TYPE bapiret2 OPTIONAL
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS to_symsg
      IMPORTING iv_msgid        TYPE symsgid  DEFAULT sy-msgid
                iv_msgty        TYPE symsgty  DEFAULT sy-msgty
                iv_msgno        TYPE symsgno  DEFAULT sy-msgno
                iv_msgv1        TYPE symsgv   DEFAULT sy-msgv1
                iv_msgv2        TYPE symsgv   DEFAULT sy-msgv2
                iv_msgv3        TYPE symsgv   DEFAULT sy-msgv3
                iv_msgv4        TYPE symsgv   DEFAULT sy-msgv4
                is_bapiret      TYPE bapiret2 OPTIONAL
      RETURNING VALUE(rs_symsg) TYPE symsg.

    CLASS-METHODS show_msgtx
      IMPORTING iv_msgtx TYPE msgtx
                iv_msgv1 TYPE msgv1 OPTIONAL
                iv_msgv2 TYPE msgv2 OPTIONAL
                iv_msgv3 TYPE msgv3 OPTIONAL
                iv_msgv4 TYPE msgv4 OPTIONAL
                iv_msgty TYPE msgty DEFAULT 'S'
                iv_msgdl TYPE msgty OPTIONAL.

    "! Convert data dynamically into message details
    "! <p><strong>Note:</strong><br/>If you supply an element-wise table you'll have to provide
    "! at least one fieldname! You can use the fieldname table to define which attributes of a
    "! structure or table should to be logged.</p>
    "!
    "! @parameter it_fnam     | Fieldnames
    "! @parameter is_msgde    | Message details
    "! @parameter is_data     | Dynamic data as a structure
    "! @parameter it_data     | Dynamic data as a table
    "! @parameter iv_is_range | <p></p>
    "! @parameter rt_msgde    | Message details
    CLASS-METHODS to_msgde
      IMPORTING it_fnam         TYPE string_table     OPTIONAL
                is_msgde        TYPE rsra_s_parameter OPTIONAL
                is_data         TYPE data             OPTIONAL
                it_data         TYPE ANY TABLE        OPTIONAL
                iv_is_range     TYPE abap_bool        DEFAULT abap_false
      RETURNING VALUE(rt_msgde) TYPE rsra_t_alert_definition.

    "! Determine component names on base of input data
    "!
    "! @parameter it_input_data | Dynamic input data as table
    "! @parameter rv_components | Component names from input data
    CLASS-METHODS get_components_from_msgde
      IMPORTING it_input_data        TYPE rsra_t_alert_definition
      RETURNING VALUE(rv_components) TYPE v_input_component.

    "! Display messages in popup
    "!
    "! @parameter it_bapiret | List of bapiret2 messages
    CLASS-METHODS display_as_popup
      IMPORTING it_bapiret TYPE bapirettab.

    CLASS-METHODS free.

    "! <p class="shorttext synchronized">Check results for error</p>
    "!
    "! @parameter iv_severity | <p class="shorttext synchronized">Severity</p>
    "! @parameter it_bapiret  | <p class="shorttext synchronized">Protocol</p>
    "! @parameter rv_result   | <p class="shorttext synchronized">Error? (Y/N)</p>
    CLASS-METHODS has_error
      IMPORTING iv_severity      TYPE bapi_mtype OPTIONAL
                it_bapiret       TYPE bapirettab OPTIONAL
      RETURNING VALUE(rv_result) TYPE abap_bool.

  PROTECTED SECTION.
    CLASS-METHODS harmonize_msg
      IMPORTING iv_msgid      TYPE symsgid
                iv_msgno      TYPE symsgno
                iv_msgty      TYPE symsgty
                iv_msgv1      TYPE symsgv
                iv_msgv2      TYPE symsgv
                iv_msgv3      TYPE symsgv
                iv_msgv4      TYPE symsgv
                is_bapiret    TYPE bapiret2 OPTIONAL
      RETURNING VALUE(rs_msg) TYPE symsg.

  PRIVATE SECTION.
    CLASS-DATA ms_symsg TYPE symsg.

    CLASS-METHODS to_msgde_add_by_components
      IMPORTING io_struct_descr TYPE REF TO cl_abap_structdescr
                is_data         TYPE any
                it_fnam         TYPE string_table
      RETURNING VALUE(rt_msgde) TYPE rsra_t_alert_definition.

    CLASS-METHODS backup_sy_msg.

    CLASS-METHODS recover_sy_msg.

ENDCLASS.


CLASS zial_cl_log IMPLEMENTATION.

  METHOD create.

    backup_sy_msg( ).

    mo_instance = NEW #( iv_object        = iv_object
                         iv_subobject     = iv_subobject
                         iv_extnumber     = iv_extnumber
                         it_extnumber     = it_extnumber
                         iv_callstack_lvl = iv_callstack_lvl ).
    APPEND mo_instance TO mt_log_stack.

    recover_sy_msg( ).

    ro_instance = mo_instance.

  ENDMETHOD.


  METHOD display_as_popup.

    DATA(lt_bapiret) = it_bapiret.
    CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
      TABLES it_return = lt_bapiret.

  ENDMETHOD.


  METHOD get.

    mo_instance = VALUE #( mt_log_stack[ lines( mt_log_stack ) ] OPTIONAL ).
    IF mo_instance IS INITIAL.
      mo_instance = create( iv_object    = mc_default-log_object
                            iv_subobject = mc_default-log_subobject
                            iv_extnumber = CONV #( TEXT-001 ) ).
    ENDIF.

    ro_instance = mo_instance.

  ENDMETHOD.


  METHOD get_components_from_msgde.

    DATA(lt_input_data) = it_input_data.
    DELETE ADJACENT DUPLICATES FROM lt_input_data COMPARING fnam.

    LOOP AT lt_input_data ASSIGNING FIELD-SYMBOL(<ls_input_data>).

      CASE sy-tabix.
        WHEN 1.
          rv_components = <ls_input_data>-fnam.

        WHEN OTHERS.
          rv_components = |{ rv_components }, { <ls_input_data>-fnam }|.

      ENDCASE.

    ENDLOOP.

    IF rv_components IS INITIAL.
      rv_components = 'N/A'.
    ENDIF.

  ENDMETHOD.


  METHOD save.

    CHECK mt_log_stack IS NOT INITIAL.

    mo_instance = get( ).
    mo_instance->save( iv_finalize ).

    IF iv_finalize EQ abap_true.
      DELETE TABLE mt_log_stack FROM mo_instance.
      CLEAR mo_instance.
    ENDIF.

  ENDMETHOD.


  METHOD to_bapiret.

    TYPES: BEGIN OF s_msgvar,
             v1 TYPE symsgv,
             v2 TYPE symsgv,
             v3 TYPE symsgv,
             v4 TYPE symsgv,
           END OF s_msgvar.

    IF iv_msgtx IS SUPPLIED.

      DATA(ls_symsg) = harmonize_msg( iv_msgid = mc_default-msgid
                                      iv_msgno = mc_default-msgno
                                      iv_msgty = iv_msgty
                                      iv_msgv1 = CONV #( iv_msgv1 )
                                      iv_msgv2 = CONV #( iv_msgv2 )
                                      iv_msgv3 = CONV #( iv_msgv3 )
                                      iv_msgv4 = CONV #( iv_msgv4 ) ).

      rs_bapiret = VALUE #( id         = ls_symsg-msgid
                            number     = ls_symsg-msgno
                            type       = ls_symsg-msgty
                            message    = iv_msgtx
                            message_v1 = ls_symsg-msgv1
                            message_v2 = ls_symsg-msgv2
                            message_v3 = ls_symsg-msgv3
                            message_v4 = ls_symsg-msgv4 ).

      DO 8 TIMES.
        DATA(lv_index) = sy-index.

        IF rs_bapiret-message NS '&'.
          EXIT.
        ENDIF.

        " Index 1-4: &1-&4
        " Index 5-8: &
        DATA(lv_search_str) = COND #( WHEN lv_index LT 5 THEN |&{ lv_index }|
                                      WHEN lv_index GT 4 THEN |&| ).
        DATA(lv_msgvar) = SWITCH #( lv_index
                                    WHEN 1 OR 5 THEN ls_symsg-msgv1
                                    WHEN 2 OR 6 THEN ls_symsg-msgv2
                                    WHEN 3 OR 7 THEN ls_symsg-msgv3
                                    WHEN 4 OR 8 THEN ls_symsg-msgv4 ).
        REPLACE FIRST OCCURRENCE OF lv_search_str IN rs_bapiret-message WITH lv_msgvar.
      ENDDO.

    ELSEIF iv_msgty IS NOT INITIAL
       AND iv_msgid IS NOT INITIAL
       AND iv_msgno IS NOT INITIAL.

      rs_bapiret = VALUE #( type       = iv_msgty
                            id         = iv_msgid
                            number     = iv_msgno
                            message_v1 = iv_msgv1
                            message_v2 = iv_msgv2
                            message_v3 = iv_msgv3
                            message_v4 = iv_msgv4 ).
      MESSAGE ID iv_msgid TYPE iv_msgty NUMBER iv_msgno
              WITH iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4 INTO rs_bapiret-message.

    ELSE.

      RETURN.

    ENDIF.

    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET_STABLE'
      IMPORTING  own_logical_system = rs_bapiret-system
      EXCEPTIONS OTHERS             = 0.

  ENDMETHOD.


  METHOD to_msgde.

    DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.

    IF is_msgde IS NOT INITIAL.

      APPEND is_msgde TO rt_msgde.

    ELSEIF is_data IS NOT INITIAL.

      lo_struct_descr ?= cl_abap_typedescr=>describe_by_data( is_data ).

      rt_msgde = to_msgde_add_by_components( io_struct_descr = lo_struct_descr
                                             is_data         = is_data
                                             it_fnam         = it_fnam ).

    ELSEIF it_data IS NOT INITIAL.

      DATA(lo_table_descr) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( it_data ) ).
      DATA(lo_data_descr) = lo_table_descr->get_table_line_type( ).

      IF     it_fnam IS NOT INITIAL
         AND (    lo_data_descr->kind EQ cl_abap_typedescr=>kind_elem
               OR iv_is_range         EQ abap_true ).
        ASSIGN it_fnam[ 1 ] TO FIELD-SYMBOL(<lv_fnam>).
      ENDIF.

      CASE iv_is_range.
        WHEN abap_true.
          DATA(lt_r_range) = CONV rseloption( it_data ).
          IF <lv_fnam> IS ASSIGNED.
            rt_msgde = VALUE #( FOR <s_r_range> IN lt_r_range
                                ( fnam = <lv_fnam>
                                  sign = <s_r_range>-sign
                                  opt  = <s_r_range>-option
                                  low  = <s_r_range>-low
                                  high = <s_r_range>-high ) ).
          ELSE.
            rt_msgde = VALUE #( FOR <s_r_range> IN lt_r_range
                                ( sign = <s_r_range>-sign
                                  opt  = <s_r_range>-option
                                  low  = <s_r_range>-low
                                  high = <s_r_range>-high ) ).
          ENDIF.

        WHEN abap_false.
          LOOP AT it_data ASSIGNING FIELD-SYMBOL(<lx_data>).

            CASE lo_data_descr->kind.
              WHEN cl_abap_typedescr=>kind_elem.
                IF <lv_fnam> IS ASSIGNED.
                  INSERT VALUE #( fnam = <lv_fnam>
                                  low  = <lx_data> ) INTO TABLE rt_msgde.
                ELSE.
                  INSERT VALUE #( low = <lx_data> ) INTO TABLE rt_msgde.
                ENDIF.

              WHEN cl_abap_typedescr=>kind_struct.
                lo_struct_descr ?= lo_data_descr.
                INSERT LINES OF to_msgde_add_by_components( io_struct_descr = lo_struct_descr
                                                            is_data         = <lx_data>
                                                            it_fnam         = it_fnam ) INTO TABLE rt_msgde.

              WHEN OTHERS.
                EXIT.

            ENDCASE.

          ENDLOOP.

      ENDCASE.

    ELSE.

      RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD to_msgde_add_by_components.

    LOOP AT io_struct_descr->components ASSIGNING FIELD-SYMBOL(<ls_component>).

      IF         it_fnam IS NOT INITIAL
         AND NOT line_exists( it_fnam[ table_line = <ls_component>-name ] ).
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE is_data TO FIELD-SYMBOL(<lv_value>).
      IF <lv_value> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( fnam = <ls_component>-name
                      low  = <lv_value> ) TO rt_msgde.

    ENDLOOP.

  ENDMETHOD.


  METHOD to_string.

    DATA(ls_symsg) = harmonize_msg( iv_msgid   = iv_msgid
                                    iv_msgno   = iv_msgno
                                    iv_msgty   = iv_msgty
                                    iv_msgv1   = iv_msgv1
                                    iv_msgv2   = iv_msgv2
                                    iv_msgv3   = iv_msgv3
                                    iv_msgv4   = iv_msgv4
                                    is_bapiret = is_bapiret ).

    MESSAGE ID ls_symsg-msgid TYPE ls_symsg-msgty NUMBER ls_symsg-msgno
            WITH ls_symsg-msgv1 ls_symsg-msgv2 ls_symsg-msgv3 ls_symsg-msgv4
            INTO rv_result.

  ENDMETHOD.


  METHOD backup_sy_msg.

    ms_symsg = VALUE #( msgid = sy-msgid
                        msgno = sy-msgno
                        msgty = sy-msgty
                        msgv1 = sy-msgv1
                        msgv2 = sy-msgv2
                        msgv3 = sy-msgv3
                        msgv4 = sy-msgv4 ).

  ENDMETHOD.


  METHOD recover_sy_msg.

    CHECK mo_instance->has_error( ) EQ abap_false.

    sy-msgid = ms_symsg-msgid.
    sy-msgno = ms_symsg-msgno.
    sy-msgty = ms_symsg-msgty.
    sy-msgv1 = ms_symsg-msgv1.
    sy-msgv2 = ms_symsg-msgv2.
    sy-msgv3 = ms_symsg-msgv3.
    sy-msgv4 = ms_symsg-msgv4.

  ENDMETHOD.


  METHOD free.

    FREE: mo_instance,
          mt_log_stack.

  ENDMETHOD.


  METHOD to_symsg.

    rs_symsg = harmonize_msg( iv_msgid   = iv_msgid
                              iv_msgno   = iv_msgno
                              iv_msgty   = iv_msgty
                              iv_msgv1   = iv_msgv1
                              iv_msgv2   = iv_msgv2
                              iv_msgv3   = iv_msgv3
                              iv_msgv4   = iv_msgv4
                              is_bapiret = is_bapiret ).

  ENDMETHOD.


  METHOD show_msgtx.

    DATA(lv_msgdl) = iv_msgdl.
    IF lv_msgdl IS INITIAL.
      lv_msgdl = iv_msgty.
    ENDIF.

    DATA(lv_msgtx) = iv_msgtx.
    IF iv_msgv1 IS NOT INITIAL.
      REPLACE '&1' IN lv_msgtx WITH iv_msgv1.
    ENDIF.
    IF iv_msgv2 IS NOT INITIAL.
      REPLACE '&2' IN lv_msgtx WITH iv_msgv2.
    ENDIF.
    IF iv_msgv3 IS NOT INITIAL.
      REPLACE '&3' IN lv_msgtx WITH iv_msgv3.
    ENDIF.
    IF iv_msgv4 IS NOT INITIAL.
      REPLACE '&4' IN lv_msgtx WITH iv_msgv4.
    ENDIF.

    MESSAGE lv_msgtx TYPE iv_msgty DISPLAY LIKE lv_msgdl.

  ENDMETHOD.


  METHOD harmonize_msg.

    IF is_bapiret IS NOT INITIAL.

      DATA(lv_msgid) = is_bapiret-id.
      DATA(lv_msgno) = is_bapiret-number.
      DATA(lv_msgty) = is_bapiret-type.
      DATA(lv_msgv1) = is_bapiret-message_v1.
      DATA(lv_msgv2) = is_bapiret-message_v2.
      DATA(lv_msgv3) = is_bapiret-message_v3.
      DATA(lv_msgv4) = is_bapiret-message_v4.

    ELSE.

      lv_msgid = iv_msgid.
      lv_msgno = iv_msgno.
      lv_msgty = iv_msgty.
      lv_msgv1 = iv_msgv1.
      lv_msgv2 = iv_msgv2.
      lv_msgv3 = iv_msgv3.
      lv_msgv4 = iv_msgv4.

    ENDIF.

    IF    lv_msgid IS INITIAL
       OR lv_msgno IS INITIAL.
      lv_msgid = mc_default-msgid.
      lv_msgno = mc_default-msgno.
    ENDIF.

    IF lv_msgty IS INITIAL.
      lv_msgty = mc_log_type-success.
    ENDIF.

    rs_msg = VALUE #( msgid = lv_msgid
                      msgno = lv_msgno
                      msgty = lv_msgty
                      msgv1 = lv_msgv1
                      msgv2 = lv_msgv2
                      msgv3 = lv_msgv3
                      msgv4 = lv_msgv4 ).

  ENDMETHOD.


  METHOD has_error.

    IF iv_severity CA 'AEX'.
      rv_result = abap_true.
      RETURN.
    ENDIF.

    LOOP AT it_bapiret TRANSPORTING NO FIELDS WHERE type CA 'AEX'.
      rv_result = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
