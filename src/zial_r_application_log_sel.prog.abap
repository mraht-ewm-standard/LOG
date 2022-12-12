*&---------------------------------------------------------------------*
*& Include zial_r_application_log_sel
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_lognum FOR balhdr-lognumber NO INTERVALS.
  PARAMETERS: p_obj    TYPE balhdr-object,
              p_subobj TYPE balhdr-subobject,
              p_extnum TYPE balhdr-extnumber.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 18(14) l_datfr FOR FIELD p_datfr.
    PARAMETERS: p_datfr TYPE balhdr-aldate,
                p_timfr TYPE balhdr-altime.
  SELECTION-SCREEN: END OF LINE.

  SELECTION-SCREEN: BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 18(14) l_datto FOR FIELD p_datto.
    PARAMETERS: p_datto TYPE balhdr-aldate,
                p_timto TYPE balhdr-altime.
  SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_user  TYPE balhdr-aluser  DEFAULT '*',
              p_tcode TYPE balhdr-altcode DEFAULT '*',
              p_prog  TYPE balhdr-alprog  DEFAULT '*'.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  PARAMETERS: p_class1 TYPE balhdr-probclass RADIOBUTTON GROUP 1,
              p_class2 TYPE balhdr-probclass RADIOBUTTON GROUP 1,
              p_class3 TYPE balhdr-probclass RADIOBUTTON GROUP 1,
              p_class4 TYPE balhdr-probclass RADIOBUTTON GROUP 1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
  SELECT-OPTIONS: s_msgid FOR sy-msgid NO INTERVALS,
                  s_msgty FOR sy-msgty NO INTERVALS,
                  s_msgno FOR sy-msgno,
                  s_msgv  FOR sy-msgv1.
SELECTION-SCREEN END OF BLOCK b5.

PARAMETERS: p_appl AS CHECKBOX DEFAULT 'X'.
