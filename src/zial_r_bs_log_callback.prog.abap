*&---------------------------------------------------------------------*
*& Report zial_r_bs_log_callback
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zial_r_bs_log_callback.

FORM on_click_msg_detail TABLES i_params STRUCTURE spar.

  CONSTANTS: lc_log_number TYPE spo_par VALUE '%LOGNUMBER'.

  DATA: lv_log_number     TYPE balognr,
        lv_msg_param_id   TYPE zial_cl_log=>v_message_param_id,
        ls_structure_name TYPE dd02l-tabname.

  FIELD-SYMBOLS: <lt_outtab> TYPE STANDARD TABLE.

  " Find out the identifier for this message
  lv_log_number = VALUE #( i_params[ param = lc_log_number ]-value OPTIONAL ).
  CHECK lv_log_number IS NOT INITIAL.

  SELECT SINGLE FROM tfdir
    FIELDS @abap_true
    WHERE funcname EQ '/SCWM/DLV_IMPORT_LOG'
    INTO @DATA(lv_func_exists).

  CHECK lv_func_exists EQ abap_true.

  " Load specific message details from database
  DATA(lt_msg_details) = VALUE zial_tt_msg_details( ).  IMPORT msg_details TO lt_msg_details FROM DATABASE bal_indx(al) ID lv_log_number.

  CHECK lt_msg_details IS NOT INITIAL.

  lv_msg_param_id = VALUE #( i_params[ param = zial_cl_log=>mc_msg_ident ]-value OPTIONAL ).
  CHECK lv_msg_param_id IS NOT INITIAL.

  " Search for those entries which belong to this message
  ASSIGN lt_msg_details[ v_id = lv_msg_param_id ] TO FIELD-SYMBOL(<ls_msg_details>).
  CHECK <ls_msg_details> IS ASSIGNED.

  IF zial_cl_log=>mo_gui_alv_grid IS NOT INITIAL.
    zial_cl_log=>mo_gui_alv_grid->free( ).
    CLEAR zial_cl_log=>mo_gui_alv_grid.
  ENDIF.

  "    Show container if not visible
  " OR Hide container if detail to same message was again being selected
  IF    zial_cl_log=>mo_gui_docking_container IS BOUND
    AND zial_cl_log=>mv_sel_msg_param_id EQ lv_msg_param_id.

    zial_cl_log=>mo_gui_docking_container->free( ).
    CLEAR: zial_cl_log=>mo_gui_docking_container,
           zial_cl_log=>mv_sel_msg_param_id.

  ELSEIF zial_cl_log=>mo_gui_docking_container IS NOT BOUND.

    zial_cl_log=>mo_gui_docking_container = NEW #( side      = cl_gui_docking_container=>dock_at_bottom
                                                   extension = '120' ).
    zial_cl_log=>mo_gui_docking_container->set_visible( abap_true ).

  ENDIF.

  CHECK zial_cl_log=>mo_gui_docking_container IS BOUND.

  zial_cl_log=>mo_gui_alv_grid = NEW #( i_parent = zial_cl_log=>mo_gui_docking_container ).

  DATA(ls_layout) = VALUE lvc_s_layo( cwidth_opt = 'X'
                                      sel_mode   = 'D' ).
  DATA(lt_alv_fcodes_excl) = VALUE ui_functions( ( cl_gui_alv_grid=>mc_fc_graph )
                                                 ( cl_gui_alv_grid=>mc_fc_info )
                                                 ( cl_gui_alv_grid=>mc_fc_excl_all ) ).

  IF <ls_msg_details>-t_input_parameter IS NOT INITIAL.
    ls_structure_name = '/SCWM/RSRA_S_PARAMETER'.
    ASSIGN <ls_msg_details>-t_input_parameter TO <lt_outtab>.
  ENDIF.

  CHECK <lt_outtab> IS ASSIGNED.

  zial_cl_log=>mo_gui_alv_grid->set_table_for_first_display(
    EXPORTING
      i_structure_name     = ls_structure_name
      is_layout            = ls_layout
      it_toolbar_excluding = lt_alv_fcodes_excl
    CHANGING
      it_outtab            = <lt_outtab> ).

  zial_cl_log=>mv_sel_msg_param_id = lv_msg_param_id.

ENDFORM.
