INTERFACE zial_if_log_ewm
  PUBLIC.

  "! <strong>[EWM]</strong> Log SAP log messages
  "!
  "! @parameter ir_o_log | TYPE REF TO /SCWM/CL_LOG (SAP log object)
  METHODS log_saplog
    IMPORTING ir_o_log TYPE zial_de_log_ewm_cl_log.

  "! <strong>[EWM]</strong> Log API messages
  "!
  "! @parameter ir_o_api_message | TYPE REF TO /SCWM/IF_API_MESSAGE (API log object)
  METHODS log_api_message
    IMPORTING ir_o_api_message TYPE zial_de_log_ewm_if_api_message.

  "! <strong>[EWM]</strong> Log delivery management messages
  "!
  "! @parameter ir_t_dm_messages | TYPE /SCDL/DM_MESSAGE_TAB (Delivery management log messages)
  METHODS log_dm_messages
    IMPORTING ir_t_dm_messages TYPE zial_de_log_ewm_dm_message_tab.

  "! <strong>[EWM]</strong> Set warehouse number
  "!
  "! @parameter ir_v_lgnum | TYPE /SCWM/LGNUM (Warehouse number)
  METHODS set_lgnum
    IMPORTING ir_v_lgnum TYPE zial_de_log_ewm_lgnum.

ENDINTERFACE.
