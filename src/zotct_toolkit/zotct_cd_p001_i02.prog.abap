*----------------------------------------------------------------------*
***INCLUDE ZOTCT_CD_P001_I02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'GUI'.
* SET TITLEBAR 'xxx'.

  IF gr_cont IS NOT BOUND.
    CREATE OBJECT gr_cont
      EXPORTING
        container_name              = 'CONT1'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT gr_alvgrid
      EXPORTING
        i_parent          = gr_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM create_fcat.
    PERFORM modify_fcat.

    gs_layout-zebra = 'X'.
    gs_layout-edit = 'X'.

    CALL METHOD gr_alvgrid->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
      CHANGING
        it_fieldcatalog = gt_fcat
        it_outtab       = gt_ot.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  DATA: lt_rows   TYPE lvc_t_row,
        ls_rows   TYPE lvc_s_row,
        lv_answer TYPE char1,
        lv_delcn  TYPE p.
  gr_alvgrid->check_changed_data( ).
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANC' OR '&F03' OR '&F15' OR '&F12'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN '&ADD'.
      APPEND INITIAL LINE TO gt_ot.
    WHEN '&REMOVE'.
      gr_alvgrid->get_selected_rows( IMPORTING et_index_rows = lt_rows ).
      CLEAR lv_delcn.
      LOOP AT gt_ot INTO gs_ot.
        lv_delcn = lv_delcn + 1.
        READ TABLE lt_rows WITH KEY index = lv_delcn TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          DELETE gt_ot.
        ENDIF.
      ENDLOOP.
    WHEN '&CREATE'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = 'DDIC Objects will be created. Are you sure?'
          default_button        = '2'
          display_cancel_button = ''
        IMPORTING
          answer                = lv_answer.
      IF sy-subrc NE 0 OR
         lv_answer EQ '2'.
        EXIT.
      ENDIF.

      CREATE OBJECT gr_app.
      gr_app->initialize( EXPORTING it_objlist = gt_ot IMPORTING et_error = gt_error ).

      IF gt_error IS NOT INITIAL.
        " TODO show error log
      ENDIF.

  ENDCASE.

  gr_alvgrid->refresh_table_display( ).

ENDMODULE.
