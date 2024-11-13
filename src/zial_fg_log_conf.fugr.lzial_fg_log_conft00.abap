*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIAL_V_LOG_CONF.................................*
TABLES: ZIAL_V_LOG_CONF, *ZIAL_V_LOG_CONF. "view work areas
CONTROLS: TCTRL_ZIAL_V_LOG_CONF
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZIAL_V_LOG_CONF. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZIAL_V_LOG_CONF.
* Table for entries selected to show on screen
DATA: BEGIN OF ZIAL_V_LOG_CONF_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZIAL_V_LOG_CONF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZIAL_V_LOG_CONF_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZIAL_V_LOG_CONF_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZIAL_V_LOG_CONF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZIAL_V_LOG_CONF_TOTAL.

*.........table declarations:.................................*
TABLES: BALOBJ                         .
TABLES: BALOBJT                        .
TABLES: BALSUB                         .
TABLES: BALSUBT                        .
TABLES: ZIAL_T_LOG_CONF                .
