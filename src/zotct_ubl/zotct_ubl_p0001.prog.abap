*&---------------------------------------------------------------------*
*& Report ZOTCT_UBL_P0001
*&---------------------------------------------------------------------*
*& openTCT
*&
*& Developed by Egemen YILMAZ - egemen.yilmaz@opentct.com
*&
*& This report is developed as a test report for ZOTCT_UBL class
*&
*&---------------------------------------------------------------------*
REPORT zotct_ubl_p0001.

PARAMETERS: p_upload TYPE rlgrap-filename.

TYPES: BEGIN OF ty_filelist,
         filename TYPE string,
         content  TYPE string,
         nested   TYPE string,
       END OF ty_filelist.

TYPES: BEGIN OF ty_ot,
         filename TYPE string,
         filesize TYPE string,
         color(4) TYPE c,
       END OF ty_ot.


DATA: gt_filelist TYPE TABLE OF ty_filelist,
      gs_filelist TYPE ty_filelist,
      gt_bin_data TYPE TABLE OF tbl1024,
      gt_ot       TYPE TABLE OF ty_ot,
      gs_ot       TYPE ty_ot.

DATA: gc_zip TYPE REF TO cl_abap_zip.

FIELD-SYMBOLS: <ot> TYPE ty_ot.

INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upload.
  CALL FUNCTION '/SAPDMC/LSM_F4_FRONTEND_FILE'
* EXPORTING
*   PATHNAME               =
    CHANGING
      pathfile         = p_upload
    EXCEPTIONS
      canceled_by_user = 1
      system_error     = 2
      OTHERS           = 3.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

START-OF-SELECTION.

  PERFORM get_data.
  PERFORM set_data.
  PERFORM display_data.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data.
*  Read UBL file(s) from zip file

  DATA: lv_file_length TYPE i,
        lv_buffer      TYPE xstring,
        lv_filename    TYPE string.

  lv_filename = p_upload.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename   = lv_filename
      filetype   = 'BIN'
    IMPORTING
      filelength = lv_file_length
    TABLES
      data_tab   = gt_bin_data.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_file_length
*     FIRST_LINE   = 0
*     LAST_LINE    = 0
    IMPORTING
      buffer       = lv_buffer
    TABLES
      binary_tab   = gt_bin_data
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  CREATE OBJECT gc_zip.

  CALL METHOD gc_zip->load
    EXPORTING
      zip = lv_buffer.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_data.
  TYPES:
    ty_xline(2048) TYPE x, "BINARY FILES
    BEGIN OF ty_line,
      line(1024) TYPE c,
    END OF ty_line. "CONTENT

  DATA: ls_files   LIKE LINE OF  cl_abap_zip=>files,
        lv_outputx TYPE xstring,
        lr_conv    TYPE REF TO cl_abap_conv_in_ce.

  LOOP AT gc_zip->files INTO ls_files.
    CLEAR: gs_filelist.
    gs_filelist-filename = ls_files-name.
    gc_zip->get( EXPORTING
                name = gs_filelist-filename
                IMPORTING
                content = lv_outputx ).
    CALL METHOD cl_abap_conv_in_ce=>create
      EXPORTING
        input = lv_outputx    " Input Buffer (X, XSTRING)
      RECEIVING
        conv  = lr_conv.
    lr_conv->read( IMPORTING
        data    = gs_filelist-content ).    " Data Object To Be Read

    APPEND gs_filelist TO gt_filelist.
  ENDLOOP.


***  todo: run the test

  DATA : lc_ublfactory TYPE REF TO zotct_cl_ubl_f,
         lc_ef         TYPE REF TO zotcttr_cl_ef,
         lt_ubltab     TYPE zotct_tt0002,
         lv_xmlstr     TYPE string.

  CREATE OBJECT lc_ublfactory
    EXPORTING
      locale  = 'TR'
      product = 'EF'.

  lc_ef ?= lc_ublfactory->get_ubl( ) .

  LOOP AT gt_filelist INTO gs_filelist.
    lc_ef->set_xmlstr( xmlstr = gs_filelist-content ).

    lc_ef->flatten( ).
    lt_ubltab = lc_ef->get_node( ).
    lc_ef->set_node( ubltab = lt_ubltab ).
    lc_ef->nest( ).
    lv_xmlstr = lc_ef->get_xmlstr( ).

    APPEND INITIAL LINE TO gt_ot ASSIGNING <ot>.
    <ot>-filename = gs_filelist-filename.
    IF gs_filelist-content EQ lv_xmlstr.
      <ot>-color = 'C500'.
    ELSE.
      <ot>-color = 'C600'.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE LINE OF slis_t_fieldcat_alv,
        ls_layout   TYPE slis_layout_alv.

  ls_layout-info_fieldname = 'COLOR'.
  ls_layout-colwidth_optimize  = 'X'.

  CLEAR lt_fieldcat[].
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'FILENAME'.
  ls_fieldcat-seltext_m = 'File Name'.
  ls_fieldcat-col_pos = 0.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'FILESIZE'.
  ls_fieldcat-seltext_m = 'File Size'.
  ls_fieldcat-col_pos = 0.
  APPEND ls_fieldcat TO lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_user_command = 'USER_COMMAND'
      it_fieldcat             = lt_fieldcat[]
      i_save                  = 'X'
*     is_variant              = g_variant
      is_layout               = ls_layout
    TABLES
      t_outtab                = gt_ot
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.


FORM user_command USING p_ucomm LIKE sy-ucomm
              p_selfield TYPE slis_selfield.

ENDFORM.
