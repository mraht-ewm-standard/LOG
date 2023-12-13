"! <p class="shorttext synchronized">Logging</p>
CLASS zial_cl_log DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES v_message_param_id TYPE n LENGTH 10.
    TYPES v_input_component  TYPE c LENGTH 150.

    TYPES r_log_instance     TYPE REF TO zial_cl_log_ewm.
    TYPES t_log_stack        TYPE TABLE OF r_log_instance WITH DEFAULT KEY.

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
                 log_object    TYPE balobj_d  VALUE 'ZIAL_LOG' ##NO_TEXT, " Adjust to your needs
                 log_subobject TYPE balsubobj VALUE 'ZIAL_LOG' ##NO_TEXT,
                 msgid         TYPE msgid     VALUE '0Q',
                 msgno         TYPE msgno     VALUE '000',
               END OF mc_default.

    CONSTANTS mc_msg_ident          TYPE c LENGTH 9 VALUE 'MSG_IDENT' ##NO_TEXT.
    CONSTANTS mc_log_context_struct TYPE baltabname VALUE 'ZIAL_S_LOG_CONTEXT' ##NO_TEXT.

    CONSTANTS: BEGIN OF mc_msgde_callback,
                 report  TYPE baluep VALUE 'ZIAL_R_BS_LOG_CALLBACK',
                 routine TYPE baluef VALUE 'ON_CLICK_MSG_DETAIL',
               END OF mc_msgde_callback.

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

    CLASS-DATA mo_instance              TYPE r_log_instance.
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
                iv_msgv1          TYPE symsgv   DEFAULT sy-msgv1
                iv_msgv2          TYPE symsgv   DEFAULT sy-msgv2
                iv_msgv3          TYPE symsgv   DEFAULT sy-msgv3
                iv_msgv4          TYPE symsgv   DEFAULT sy-msgv4
      RETURNING VALUE(rs_bapiret) TYPE bapiret2.

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

    CLASS-METHODS to_bapirettab
      IMPORTING it_dm_message        TYPE /scdl/dm_message_tab
      RETURNING VALUE(rt_bapirettab) TYPE bapirettab.

  PRIVATE SECTION.
    CLASS-METHODS to_msgde_add_by_components
      IMPORTING io_struct_descr TYPE REF TO cl_abap_structdescr
                is_data         TYPE any
                it_fnam         TYPE string_table
      RETURNING VALUE(rt_msgde) TYPE rsra_t_alert_definition.

ENDCLASS.


CLASS zial_cl_log IMPLEMENTATION.

  METHOD display_as_popup.

    DATA(lt_bapiret) = it_bapiret.
    CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
      TABLES it_return = lt_bapiret.

  ENDMETHOD.


  METHOD get.

    mo_instance = VALUE #( mt_log_stack[ lines( mt_log_stack ) ] OPTIONAL ).
    IF mo_instance IS INITIAL.
      mo_instance = NEW #( iv_object    = mc_default-log_object
                           iv_subobject = mc_default-log_subobject
                           iv_extnumber = CONV #( TEXT-001 ) ).
      APPEND mo_instance TO mt_log_stack.
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

  ENDMETHOD.


  METHOD create.

    mo_instance = NEW #( iv_object        = iv_object
                         iv_subobject     = iv_subobject
                         iv_extnumber     = iv_extnumber
                         it_extnumber     = it_extnumber
                         iv_callstack_lvl = iv_callstack_lvl ).

    mo_instance->init( iv_extnumber = iv_extnumber
                       it_extnumber = it_extnumber ).

    APPEND mo_instance TO mt_log_stack.

    ro_instance = mo_instance.

  ENDMETHOD.


  METHOD save.

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

      DATA(lv_msgid) = iv_msgid.
      IF lv_msgid IS INITIAL.
        lv_msgid = mc_default-msgid.
      ENDIF.

      DATA(lv_msgno) = iv_msgno.
      IF lv_msgno IS INITIAL.
        lv_msgno = mc_default-msgno.
      ENDIF.

      rs_bapiret = VALUE #( type    = iv_msgty
                            id      = lv_msgid
                            number  = lv_msgno
                            message = iv_msgtx ).

      DO 8 TIMES.

        DATA(lv_index) = sy-index.

        DATA(lv_search_str) = COND #( WHEN lv_index LT 5 THEN |&{ lv_index }|
                                      WHEN lv_index GT 4 THEN |&| ).

        DATA(lv_msgvar) = SWITCH #( lv_index
                                    WHEN 1 OR 5 THEN iv_msgv1
                                    WHEN 2 OR 6 THEN iv_msgv2
                                    WHEN 3 OR 7 THEN iv_msgv3
                                    WHEN 4 OR 8 THEN iv_msgv4 ).

        REPLACE ALL OCCURRENCES OF lv_search_str IN rs_bapiret-message WITH lv_msgvar.

      ENDDO.

      " Split message into variables of dynamic message
      DATA(ls_message) = CONV s_msgvar( rs_bapiret-message ).
      rs_bapiret = CORRESPONDING #( ls_message MAPPING message_v1 = v1
                                                       message_v2 = v2
                                                       message_v3 = v3
                                                       message_v4 = v4 ).

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

    ELSE.

      RETURN.

    ENDIF.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING type   = rs_bapiret-type
                cl     = rs_bapiret-id
                number = rs_bapiret-number
                par1   = rs_bapiret-message_v1
                par2   = rs_bapiret-message_v2
                par3   = rs_bapiret-message_v3
                par4   = rs_bapiret-message_v4
      IMPORTING return = rs_bapiret.

  ENDMETHOD.


  METHOD to_msgde.

    DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.

    IF is_msgde IS NOT INITIAL.

      APPEND is_msgde TO rt_msgde.

    ELSEIF is_data IS NOT INITIAL.

      lo_struct_descr ?= cl_abap_typedescr=>describe_by_data( is_data ).

      IF it_fnam IS NOT INITIAL.
        ASSIGN it_fnam[ 1 ] TO FIELD-SYMBOL(<lv_fnam>).
      ENDIF.

      rt_msgde = to_msgde_add_by_components( io_struct_descr = lo_struct_descr
                                             is_data         = is_data
                                             it_fnam         = it_fnam ).

    ELSEIF it_data IS NOT INITIAL.

      DATA(lo_table_descr) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( it_data ) ).
      DATA(lo_data_descr) = lo_table_descr->get_table_line_type( ).

      IF     it_fnam             IS NOT INITIAL
         AND lo_data_descr->kind EQ cl_abap_typedescr=>kind_elem.
        ASSIGN it_fnam[ 1 ] TO <lv_fnam>.
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
                ASSERT <lv_fnam> IS ASSIGNED.
                APPEND VALUE #( fnam = <lv_fnam>
                                low  = <lx_data> ) TO rt_msgde.

              WHEN cl_abap_typedescr=>kind_struct.
                lo_struct_descr ?= lo_data_descr.
                rt_msgde = to_msgde_add_by_components( io_struct_descr = lo_struct_descr
                                                       is_data         = <lx_data>
                                                       it_fnam         = it_fnam ).

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

    IF is_bapiret IS SUPPLIED.

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

    MESSAGE ID lv_msgid TYPE lv_msgty NUMBER lv_msgno
      WITH  lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4
      INTO rv_result.

  ENDMETHOD.


  METHOD to_bapirettab.

    rt_bapirettab = CORRESPONDING #( it_dm_message MAPPING id         = msgid
                                                           type       = msgty
                                                           number     = msgno
                                                           message_v1 = msgv1
                                                           message_v2 = msgv2
                                                           message_v3 = msgv3
                                                           message_v4 = msgv4 ).

  ENDMETHOD.

ENDCLASS.
