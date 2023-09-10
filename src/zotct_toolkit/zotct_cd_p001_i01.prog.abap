*&---------------------------------------------------------------------*
*& Include          ZOTCT_CD_P001_I01
*&---------------------------------------------------------------------*

DATA: gt_ot    TYPE zotct_cd_tt001,
      gs_ot    TYPE zotct_cd_s001,

      gt_error TYPE zotct_cd_tt002,
      gs_error TYPE zotct_cd_s002.

DATA: gr_cont        TYPE REF TO cl_gui_custom_container,
      gr_alvgrid     TYPE REF TO cl_gui_alv_grid,
      gv_struct_name TYPE dd02l-tabname,
      gv_variant     TYPE disvariant,

      gs_layout      TYPE lvc_s_layo,
      gs_fcat        TYPE lvc_s_fcat,
      gt_fcat        TYPE lvc_t_fcat,








*gr_table       type ref to cl_salv_table,
*gr_selections  type ref to cl_salv_selections,
*gr_rowselector type ref to cl_salv_selections,
*gr_columns     type ref to cl_salv_columns_table,
*gr_column      type ref to cl_salv_column,
*g_okcode       type syucomm,
      gr_app         TYPE REF TO zotct_cl_create_ddic.
