*----------------------------------------------------------------------*
***INCLUDE ZOTCT_CD_P001_I03.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form create_fcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_fcat .

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE    =
      i_structure_name   = 'ZOTCT_CD_S001'
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_BYPASSING_BUFFER =
      i_internal_tabname = 'GT_OT'
    CHANGING
      ct_fieldcat        = gt_fcat[]
*   EXCEPTIONS
*     INCONSISTENT_INTERFACE       = 1
*     PROGRAM_ERROR      = 2
*     OTHERS             = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form modify_fcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM modify_fcat .
  LOOP AT gt_fcat INTO gs_fcat.
    gs_fcat-coltext = gs_fcat-fieldname.
    gs_fcat-scrtext_s = gs_fcat-fieldname.
    gs_fcat-scrtext_m = gs_fcat-fieldname.
    gs_fcat-scrtext_l = gs_fcat-fieldname.
    gs_fcat-outputlen = 10.
    MODIFY gt_fcat FROM gs_fcat.
  ENDLOOP.
ENDFORM.
