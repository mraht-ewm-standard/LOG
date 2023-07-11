"! <p class="shorttext synchronized" lang="en">Logging</p>
CLASS zial_cl_log DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: v_message_param_id TYPE n LENGTH 10 .
    TYPES: s_input_parameters TYPE rsra_s_parameter,
           t_input_parameters TYPE rsra_t_alert_definition.
    TYPES: de_char150 TYPE c LENGTH 150 .

    TYPES: o_log_instance TYPE REF TO zial_cl_log_sap,
           t_log_stack    TYPE TABLE OF o_log_instance WITH DEFAULT KEY.

    CONSTANTS: BEGIN OF mc_msg_content_type,
                 obj TYPE numc1 VALUE 1,
                 txt TYPE numc1 VALUE 2,
               END OF mc_msg_content_type .

    CONSTANTS: BEGIN OF mc_log_process,
                 create    TYPE char4 VALUE 'CREA' ##NO_TEXT,
                 init      TYPE char4 VALUE 'INIT' ##NO_TEXT,
                 save      TYPE char4 VALUE 'SAVE' ##NO_TEXT,
                 exception TYPE char4 VALUE 'EXCP' ##NO_TEXT,
               END OF mc_log_process .

    CONSTANTS: BEGIN OF mc_default,
                 log_object    TYPE balobj_d  VALUE 'ZIAL_LOG' ##NO_TEXT, " Adjust to your needs
                 log_subobject TYPE balsubobj VALUE 'ZIAL_LOG' ##NO_TEXT,
                 msgid         TYPE msgid     VALUE '0Q',
                 msgno         TYPE msgno     VALUE '000',
               END OF mc_default.

    CONSTANTS: mc_msg_ident          TYPE c LENGTH 9 VALUE 'MSG_IDENT' ##NO_TEXT,
               mc_log_context_struct TYPE baltabname VALUE 'ZIAL_S_LOG_CONTEXT' ##NO_TEXT.

    CONSTANTS: BEGIN OF mc_msgde_callback,
                 report  TYPE baluep VALUE 'ZIAL_R_BS_LOG_CALLBACK',
                 routine TYPE baluef VALUE 'ON_CLICK_MSG_DETAIL',
               END OF mc_msgde_callback .

    CONSTANTS: BEGIN OF mc_callstack_lvl,
                 none    TYPE numc1 VALUE 0,
                 error   TYPE numc1 VALUE 1,
                 warning TYPE numc1 VALUE 2,
                 success TYPE numc1 VALUE 3,
                 info    TYPE numc1 VALUE 4,
               END OF mc_callstack_lvl .

    CONSTANTS: BEGIN OF mc_log_type,
                 error        TYPE symsgty   VALUE 'E',
                 error_prio   TYPE balprobcl VALUE 1,
                 warning      TYPE symsgty   VALUE 'W',
                 warning_prio TYPE balprobcl VALUE 2,
                 success      TYPE symsgty   VALUE 'S',
                 success_prio TYPE balprobcl VALUE 3,
                 info         TYPE symsgty   VALUE 'I',
                 info_prio    TYPE balprobcl VALUE 4,
               END OF mc_log_type .

    CLASS-DATA: mo_gui_docking_container TYPE REF TO cl_gui_docking_container,
                mo_gui_alv_grid          TYPE REF TO cl_gui_alv_grid,
                mv_sel_msg_param_id      TYPE v_message_param_id.

    CLASS-DATA: mo_instance  TYPE o_log_instance,
                mt_log_stack TYPE t_log_stack .   " LIFO: Last log initiated is first to be saved

    "! Get existing or create and return new log instance
    "!
    "! @parameter ro_instance | Instance
    CLASS-METHODS get
      RETURNING
        VALUE(ro_instance) LIKE mo_instance .
    "! Initialise log instance
    "!
    "! @parameter iv_object | Log object
    "! @parameter iv_subobject | Log subobject
    "! @parameter iv_extnumber | External number / description for a log
    "! @parameter it_extnumber | External number elements
    "! @parameter iv_callstack_lvl | Level of minimum message type for which the callstack is to be logged in message details
    "! @parameter ro_instance | Log instance
    CLASS-METHODS init
      IMPORTING
        !iv_object         TYPE balobj_d  DEFAULT mc_default-log_object
        !iv_subobject      TYPE balsubobj DEFAULT mc_default-log_subobject
        !iv_extnumber      TYPE balnrext OPTIONAL
        !it_extnumber      TYPE stringtab OPTIONAL
        !iv_callstack_lvl  TYPE numc1 DEFAULT zial_cl_log=>mc_callstack_lvl-info
      RETURNING
        VALUE(ro_instance) LIKE mo_instance .
    "! Save log to application log and optionally close log instance
    "!
    "! @parameter iv_finalize | Finalize/close log? (Y/N)
    CLASS-METHODS save
      IMPORTING
        !iv_finalize TYPE abap_bool DEFAULT abap_true .
    CLASS-METHODS to_bapiret
      IMPORTING
        !iv_msgid         TYPE symsgid  DEFAULT sy-msgid
        !iv_msgty         TYPE symsgty  DEFAULT sy-msgty
        !iv_msgtx         TYPE bapi_msg OPTIONAL
        !iv_msgno         TYPE symsgno  DEFAULT sy-msgno
        !iv_msgv1         TYPE symsgv   DEFAULT sy-msgv1
        !iv_msgv2         TYPE symsgv   DEFAULT sy-msgv2
        !iv_msgv3         TYPE symsgv   DEFAULT sy-msgv3
        !iv_msgv4         TYPE symsgv   DEFAULT sy-msgv4
      RETURNING
        VALUE(rs_bapiret) TYPE bapiret2 .
    "! Convert data dynamically into message details
    "! <p><strong>Note:</strong><br/>If you supply an element-wise table you'll have to provide
    "! at least one fieldname! You can use the fieldname table to define which attributes of a
    "! structure or table should to be logged.</p>
    "!
    "! @parameter it_fnam | Fieldnames
    "! @parameter is_msgde | Message details
    "! @parameter is_data | Dynamic data as a structure
    "! @parameter it_data | Dynamic data as a table
    "! @parameter rt_msgde | Message details
    CLASS-METHODS to_msgde
      IMPORTING
        !it_fnam        TYPE string_table OPTIONAL
        !is_msgde       TYPE zial_cl_log=>s_input_parameters OPTIONAL
        !is_data        TYPE data OPTIONAL
        !it_data        TYPE ANY TABLE OPTIONAL
        !iv_is_range    TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rt_msgde) TYPE zial_cl_log=>t_input_parameters .
    "! Determine component names on base of input data
    "!
    "! @parameter it_input_data | Dynamic input data as table
    "! @parameter rv_components | Component names from input data
    CLASS-METHODS get_components_from_msgde
      IMPORTING
        !it_input_data       TYPE rsra_t_alert_definition
      RETURNING
        VALUE(rv_components) TYPE zial_cl_log=>de_char150 .
    "! Display message on screen
    "! @parameter iv_msgid | Message class
    "! @parameter iv_msgty | Message type
    "! @parameter iv_msgdt | Type of message to be displayed like
    "! @parameter iv_msgtx | Message text
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    CLASS-METHODS display_as_message
      IMPORTING
        !iv_msgid TYPE symsgid  DEFAULT sy-msgid
        !iv_msgty TYPE symsgty  DEFAULT sy-msgty
        !iv_msgdt TYPE symsgty  DEFAULT sy-msgty
        !iv_msgtx TYPE bapi_msg OPTIONAL
        !iv_msgno TYPE symsgno  DEFAULT sy-msgno
        !iv_msgv1 TYPE symsgv   DEFAULT sy-msgv1
        !iv_msgv2 TYPE symsgv   DEFAULT sy-msgv2
        !iv_msgv3 TYPE symsgv   DEFAULT sy-msgv3
        !iv_msgv4 TYPE symsgv   DEFAULT sy-msgv4.
    "! Display messages in popup
    "!
    "! @parameter it_bapiret | List of bapiret2 messages
    CLASS-METHODS display_as_popup
      IMPORTING
        it_bapiret TYPE bapirettab.
    "! Convert bapiret structure to message string
    "!
    "! @parameter is_bapiret | Bapiret message
    "! @parameter rv_result | Message as string
    CLASS-METHODS to_string
      IMPORTING
        is_bapiret       TYPE bapiret2
      RETURNING
        VALUE(rv_result) TYPE string.

  PRIVATE SECTION.
    CLASS-METHODS to_msgde_add_by_components
      IMPORTING
        io_struct_descr TYPE REF TO cl_abap_structdescr
        is_data         TYPE any
        it_fnam         TYPE string_table
      RETURNING
        VALUE(rt_msgde) TYPE zial_cl_log=>t_input_parameters.

ENDCLASS.



CLASS ZIAL_CL_LOG IMPLEMENTATION.


  METHOD display_as_message.

    DATA(lv_msgdt) = iv_msgdt.
    IF iv_msgdt IS INITIAL.
      lv_msgdt = iv_msgty.
    ENDIF.

    IF iv_msgtx IS SUPPLIED.

      DATA(lv_msgtx) = iv_msgtx.
      REPLACE: '&1' WITH iv_msgv1 INTO lv_msgtx,
               '&2' WITH iv_msgv2 INTO lv_msgtx,
               '&3' WITH iv_msgv3 INTO lv_msgtx,
               '&4' WITH iv_msgv4 INTO lv_msgtx.
      MESSAGE lv_msgtx TYPE iv_msgty DISPLAY LIKE lv_msgdt.

    ELSE.

      MESSAGE ID iv_msgid TYPE iv_msgty NUMBER iv_msgno
        WITH iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4 DISPLAY LIKE iv_msgdt.

    ENDIF.

  ENDMETHOD.


  METHOD display_as_popup.

    DATA(lt_bapiret) = it_bapiret.
    CALL FUNCTION 'RSCRMBW_DISPLAY_BAPIRET2'
      TABLES
        it_return = lt_bapiret.

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

    LOOP AT it_input_data ASSIGNING FIELD-SYMBOL(<ls_input_data>).

      CASE sy-tabix.
        WHEN 1.
          rv_components = <ls_input_data>-fnam.

        WHEN OTHERS.
          rv_components = |{ rv_components }, { <ls_input_data>-fnam }|.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD init.

    mo_instance = NEW #( iv_object        = iv_object
                         iv_subobject     = iv_subobject
                         iv_extnumber     = iv_extnumber
                         it_extnumber     = it_extnumber
                         iv_callstack_lvl = iv_callstack_lvl ).

    mo_instance->init( iv_extnumber = iv_extnumber
                       it_extnumber = it_extnumber ).

    APPEND mo_instance TO mt_log_stack.

  ENDMETHOD.


  METHOD save.

    mo_instance->save( iv_finalize ).

    IF iv_finalize EQ abap_true.
      DELETE TABLE mt_log_stack FROM mo_instance.
      CLEAR: mo_instance.
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

        DATA(lv_search_str) = COND #( WHEN lv_index < 5 THEN |&{ lv_index }|
                                      WHEN lv_index > 4 THEN |&| ).

        DATA(lv_msgvar) = SWITCH #( lv_index WHEN 1 OR 5 THEN iv_msgv1
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
      EXPORTING
        type   = rs_bapiret-type
        cl     = rs_bapiret-id
        number = rs_bapiret-number
        par1   = rs_bapiret-message_v1
        par2   = rs_bapiret-message_v2
        par3   = rs_bapiret-message_v3
        par4   = rs_bapiret-message_v4
      IMPORTING
        return = rs_bapiret.

  ENDMETHOD.


  METHOD to_msgde.

    DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr,
          lo_table_descr  TYPE REF TO cl_abap_tabledescr.

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

      lo_table_descr ?= cl_abap_typedescr=>describe_by_data( it_data ).
      DATA(lo_data_descr) = lo_table_descr->get_table_line_type( ).

      IF    it_fnam IS NOT INITIAL
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

      IF it_fnam IS NOT INITIAL.
        CHECK line_exists( it_fnam[ table_line = <ls_component>-name ] ).
      ENDIF.

      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE is_data TO FIELD-SYMBOL(<lv_value>).
      CHECK <lv_value> IS ASSIGNED.

      APPEND VALUE #( fnam = <ls_component>-name
                      low  = <lv_value> ) TO rt_msgde.

    ENDLOOP.

  ENDMETHOD.


  METHOD to_string.

    DATA(ls_bapiret) = is_bapiret.

    IF   ls_bapiret-id     IS INITIAL
      OR ls_bapiret-number IS INITIAL.
      ls_bapiret-id     = mc_default-msgid.
      ls_bapiret-number = mc_default-msgno.
    ENDIF.

    IF ls_bapiret-type IS INITIAL.
      ls_bapiret-type = mc_log_type-success.
    ENDIF.

    MESSAGE ID ls_bapiret-id TYPE ls_bapiret-type NUMBER ls_bapiret-number
      WITH ls_bapiret-message_v1 ls_bapiret-message_v2 ls_bapiret-message_v3 ls_bapiret-message_v4
      INTO rv_result.

  ENDMETHOD.
ENDCLASS.
