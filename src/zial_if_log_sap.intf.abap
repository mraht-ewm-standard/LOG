"! <p class="shorttext synchronized">Logging: General SAP log interface</p>
INTERFACE zial_if_log_sap
  PUBLIC.

  "! <strong>[SAP]</strong> Get handle of log
  "!
  "! @parameter rv_log_handle | Log handle
  METHODS get_log_handle
    RETURNING VALUE(rv_log_handle) TYPE balloghndl.

  "! <strong>[SAP]</strong> Get all logged messages
  "!
  "! @parameter rt_messages | BAPI messages
  METHODS get_messages
    RETURNING VALUE(rt_messages) TYPE bapirettab.

  "! <strong>[SAP]</strong> Log a message with optionally message details
  "!
  "! @parameter iv_msgty | Message type
  "! @parameter iv_msgtx | Message text
  "! @parameter iv_msgid | Message ID
  "! @parameter iv_msgno | Message number
  "! @parameter iv_msgv1 | Message variable 1
  "! @parameter iv_msgv2 | Message variable 2
  "! @parameter iv_msgv3 | Message variable 3
  "! @parameter iv_msgv4 | Message variable 4
  "! @parameter it_msgde | Message details
  METHODS log_message
    IMPORTING iv_msgty TYPE symsgty                 DEFAULT sy-msgty
              iv_msgtx TYPE bapi_msg                OPTIONAL
              iv_msgid TYPE symsgid                 DEFAULT sy-msgid
              iv_msgno TYPE symsgno                 DEFAULT sy-msgno
              iv_msgv1 TYPE symsgv                  DEFAULT sy-msgv1
              iv_msgv2 TYPE symsgv                  DEFAULT sy-msgv2
              iv_msgv3 TYPE symsgv                  DEFAULT sy-msgv3
              iv_msgv4 TYPE symsgv                  DEFAULT sy-msgv4
              it_msgde TYPE rsra_t_alert_definition OPTIONAL.

  "! <strong>[SAP]</strong> Log exception
  "!
  "! @parameter io_exception | Exception object
  METHODS log_exception
    IMPORTING io_exception TYPE REF TO cx_root.

  "! <strong>[SAP]</strong> Log symsg messages
  "!
  "! @parameter is_symsg | SAP system message structure
  METHODS log_symsg
    IMPORTING is_symsg TYPE symsg.

  "! <strong>[SAP]</strong> Log a table of bapi messages
  "!
  "! @parameter it_bapiret | BAPI messages
  METHODS log_bapiret
    IMPORTING it_bapiret TYPE bapirettab.

  "! <strong>[SAP]</strong> Log a horizontal line
  METHODS log_line.

  "! <strong>[SAP]</strong> Log name of development object which called the function to be logged
  METHODS log_caller.

  "! <strong>[SAP]</strong> Check if log has an error
  "!
  "! @parameter rv_result | Result
  METHODS has_error
    RETURNING VALUE(rv_result) TYPE abap_bool.

  "! <strong>[SAP]</strong> Save log to application log and optionally close log instance
  "!
  "! @parameter iv_finalize | Finalize/close log? (Y/N)
  METHODS save
    IMPORTING iv_finalize TYPE abap_bool DEFAULT abap_true.

  "! <strong>[SAP]</strong> Set log external number (description)
  "! @parameter iv_extnumber | External number as line
  "! @parameter it_extnumber | External numbers as table of strings
  METHODS set_extnumber
    IMPORTING iv_extnumber TYPE balnrext  OPTIONAL
              it_extnumber TYPE stringtab OPTIONAL.

  "! <strong>[SAP]</strong> Set log detail level
  "! @parameter iv_detail_level | Detail level
  METHODS set_detail_level
    IMPORTING iv_detail_level TYPE zial_de_log_detail_level OPTIONAL.

  "! <strong>[SAP]</strong> Set expiry date of log
  "!
  "! @parameter iv_validity_period | Validity period in days
  METHODS set_expiry_date
    IMPORTING iv_validity_period TYPE zial_de_log_validity_period OPTIONAL.

  "! <strong>[SAP]</strong> Set level of message type for which callstack is being logged
  "!
  "! @parameter iv_level | Level of message type
  METHODS set_level_log_callstack
    IMPORTING iv_level TYPE zial_de_log_detail_level OPTIONAL.

ENDINTERFACE.
