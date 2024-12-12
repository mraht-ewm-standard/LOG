INTERFACE zial_if_log_ewm
  PUBLIC.

  "! <strong>[EWM]</strong> Log SAP log messages
  "!
  "! @parameter io_log | SAP log object
  METHODS log_saplog
    IMPORTING io_log TYPE REF TO /scwm/cl_log.

  "! <strong>[EWM]</strong> Log API messages
  "!
  "! @parameter io_api_message | API log object
  METHODS log_api_message
    IMPORTING io_api_message TYPE REF TO /scwm/if_api_message.

  "! <strong>[EWM]</strong> Log delivery management messages
  "!
  "! @parameter it_dm_messages | Delivery management log messages
  METHODS log_dm_messages
    IMPORTING it_dm_messages TYPE /scdl/dm_message_tab.

  "! <strong>[EWM]</strong> Set warehouse number
  "!
  "! @parameter iv_lgnum | Warehouse number
  METHODS set_lgnum
    IMPORTING iv_lgnum TYPE /scwm/lgnum.

ENDINTERFACE.
