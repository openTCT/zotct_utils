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
       END OF ty_filelist.

DATA: gt_filelist TYPE TABLE OF ty_filelist,
      gs_filelist TYPE ty_filelist,
      gt_bin_data TYPE TABLE OF tbl1024.

DATA: gc_zip TYPE REF TO cl_abap_zip.

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
     FAILED       = 1
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

  DATA: ls_files    LIKE LINE OF  cl_abap_zip=>files,
        lv_outputx  TYPE xstring,
        lr_conv     TYPE REF TO cl_abap_conv_in_ce.

  LOOP AT gc_zip->files INTO ls_files.
    CLEAR: gs_filelist.
    gs_filelist-filename = ls_files-name.
    gc_zip->get( EXPORTING
                name = gs_filelist-filename " Example.txt (file in the zip file)
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
*** todo: display result
ENDFORM.
