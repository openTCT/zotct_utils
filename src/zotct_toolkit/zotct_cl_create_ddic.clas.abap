CLASS zotct_cl_create_ddic DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS initialize
      IMPORTING
        !it_objlist TYPE zotct_cd_tt001
      EXPORTING
        !et_error   TYPE zotct_cd_tt002 .
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_objlist,
             tablename  TYPE string,
             tabledesc  TYPE string,
             fieldname  TYPE string,
             fielddesc  TYPE string,
             dtel       TYPE string,
             doma       TYPE string,
             datatype   TYPE string,
             datalength TYPE string,
             decimals   TYPE string,
           END OF ty_objlist.

    DATA:
      gt_dtel TYPE TABLE OF rpy_dtel .
    DATA:
      gt_doma TYPE TABLE OF rpy_doma .
    DATA:
      gt_tabl TYPE TABLE OF rpy_tabl .
    DATA:
      gt_stru TYPE TABLE OF rpy_tabl .
    DATA:
      gt_ttyp TYPE TABLE OF dd40v .

    DATA: gt_dtel_list TYPE TABLE OF ty_objlist,
          gt_doma_list TYPE TABLE OF ty_objlist,
          gt_tabl_list TYPE TABLE OF ty_objlist,
          gt_stru_list TYPE TABLE OF ty_objlist,
          gt_ttyp_list TYPE TABLE OF ty_objlist.

    DATA gt_repetitive TYPE zotct_cd_tt002 .


    DATA gt_error TYPE zotct_cd_tt002 .

    TYPES: BEGIN OF ty_objlist_x.
             INCLUDE TYPE zotct_cd_s001.
    TYPES:   dtel_x TYPE char40,
             doma_x TYPE char40,
             tabl_x TYPE char40,
             stru_x TYPE char40,
             ttyp_x TYPE char40,
           END OF ty_objlist_x.

    DATA: gt_objlist_x TYPE TABLE OF ty_objlist_x.

    DATA: gv_prefix TYPE devclass.
private section.

  methods CHECK_REPETITIVE .
  methods CREATE_DTEL .
  methods CREATE_DOMA .
  methods CREATE_TABL .
  methods CREATE_STRU .
  methods CREATE_TTYP .
  methods SET_DTEL
    importing
      !IS_OBJLIST type TY_OBJLIST_X .
  methods SET_DOMA
    importing
      !IS_OBJLIST type TY_OBJLIST_X .
  methods SET_TABL
    importing
      !IS_OBJLIST type TY_OBJLIST_X .
  methods SET_STRU
    importing
      !IS_OBJLIST type TY_OBJLIST_X .
  methods SET_TTYP
    importing
      !IS_OBJLIST type TY_OBJLIST_X .
ENDCLASS.



CLASS ZOTCT_CL_CREATE_DDIC IMPLEMENTATION.


  METHOD check_repetitive.
*    DD01L domain
*    DD04L data element
*    DD02l table
*    DD40L table type

    DATA: lt_dd01l      TYPE TABLE OF dd01l,
          lt_dd04l      TYPE TABLE OF dd04l,
          lt_dd02l      TYPE TABLE OF dd02l,
          lt_dd02l_s    TYPE TABLE OF dd02l,
          lt_dd40l      TYPE TABLE OF dd40l,

          ls_dd01l      TYPE dd01l,
          ls_dd04l      TYPE dd04l,
          ls_dd02l      TYPE dd02l,
          ls_dd02l_s    TYPE dd02l,
          ls_dd40l      TYPE dd40l,

          ls_repetitive TYPE zotct_cd_s002.
    IF me->gt_doma[] IS NOT INITIAL.
      SELECT * INTO TABLE lt_dd01l
      FROM dd01l
      FOR ALL ENTRIES IN me->gt_doma
      WHERE domname EQ me->gt_doma-domaname.
    ENDIF.

    IF me->gt_dtel[] IS NOT INITIAL.
      SELECT * INTO TABLE lt_dd04l
      FROM dd04l
      FOR ALL ENTRIES IN me->gt_dtel
      WHERE rollname EQ me->gt_dtel-dtelname.
    ENDIF.

    IF me->gt_tabl[] IS NOT INITIAL.
      SELECT * INTO TABLE lt_dd02l
      FROM dd02l
      FOR ALL ENTRIES IN me->gt_tabl
      WHERE tabname EQ me->gt_tabl-tablname.
    ENDIF.

    IF me->gt_stru[] IS NOT INITIAL.
      SELECT * INTO TABLE lt_dd02l_s
      FROM dd02l
      FOR ALL ENTRIES IN me->gt_stru
      WHERE tabname EQ me->gt_stru-tablname.
    ENDIF.

    IF me->gt_ttyp[] IS NOT INITIAL.
      SELECT * INTO TABLE lt_dd40l
      FROM dd40l
      FOR ALL ENTRIES IN me->gt_ttyp
      WHERE typename EQ me->gt_ttyp-typename.
    ENDIF.

    LOOP AT lt_dd01l INTO ls_dd01l.
      CLEAR ls_repetitive.
      ls_repetitive-objtype = 'DOMA'.
      ls_repetitive-objname = ls_dd01l-domname.
      APPEND ls_repetitive TO me->gt_repetitive.
    ENDLOOP.

    LOOP AT lt_dd04l INTO ls_dd04l.
      CLEAR ls_repetitive.
      ls_repetitive-objtype = 'DTEL'.
      ls_repetitive-objname = ls_dd04l-rollname.
      APPEND ls_repetitive TO me->gt_repetitive.
    ENDLOOP.

    LOOP AT lt_dd02l INTO ls_dd02l.
      CLEAR ls_repetitive.
      ls_repetitive-objtype = 'TABL'.
      ls_repetitive-objname = ls_dd02l-tabname.
      APPEND ls_repetitive TO me->gt_repetitive.
    ENDLOOP.

    LOOP AT lt_dd02l_s INTO ls_dd02l_s.
      CLEAR ls_repetitive.
      ls_repetitive-objtype = 'STRU'.
      ls_repetitive-objname = ls_dd02l_s-tabname.
      APPEND ls_repetitive TO me->gt_repetitive.
    ENDLOOP.

    LOOP AT lt_dd40l INTO ls_dd40l.
      CLEAR ls_repetitive.
      ls_repetitive-objtype = 'TTYP'.
      ls_repetitive-objname = ls_dd40l-typename.
      APPEND ls_repetitive TO me->gt_repetitive.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_doma.
    DATA: ls_doma        LIKE LINE OF me->gt_doma,
          ls_error       TYPE zotct_cd_s002,
          lt_doma_values TYPE TABLE OF rpy_dval.

    LOOP AT me->gt_doma INTO ls_doma.

      CALL FUNCTION 'RPY_DOMAIN_INSERT'
        EXPORTING
          doma_name         = ls_doma-domaname
          doma_inf          = ls_doma
          language          = sy-langu
*         TRANSPORT_NUMBER  = ' '
          development_class = me->gv_prefix
*         WITH_DOCU         = ' '
*         DOCUTYPE          = 'T'
        TABLES
          doma_values       = lt_doma_values
*         DOCU_TABLE_USER   =
*         DOCU_TABLE_TECH   =
       EXCEPTIONS
         CANCELLED         = 1
         ALREADY_EXIST     = 2
         PERMISSION_ERROR  = 3
         NAME_NOT_ALLOWED  = 4
         NAME_CONFLICT     = 5
         INTERNAL_ERROR    = 6
         OTHERS            = 7
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      IF sy-subrc <> 0.
        CLEAR ls_error.
        ls_error-objname = ls_doma-domaname.
        ls_error-objtype = 'DOMA'.
        CASE sy-subrc.
          WHEN 1.
            ls_error-description = 'FM DDIF_DOMA_PUT exception: doma_not_found'.
          WHEN 2.
            ls_error-description = 'FM DDIF_DOMA_PUT exception: name_inconsistent'.
          WHEN 3.
            ls_error-description = 'FM DDIF_DOMA_PUT exception: doma_inconsistent'.
          WHEN 4.
            ls_error-description = 'FM DDIF_DOMA_PUT exception: put_failure'.
          WHEN 5.
            ls_error-description = 'FM DDIF_DOMA_PUT exception: put_refused'.
          WHEN 6.
            ls_error-description = 'FM DDIF_DOMA_PUT exception: others'.
          WHEN OTHERS.
            ls_error-description = 'FM DDIF_DOMA_PUT Unhandled exception'.
        ENDCASE.
        APPEND ls_error TO me->gt_error.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'DDIF_DOMA_ACTIVATE'
        EXPORTING
          name        = ls_doma-domaname
        EXCEPTIONS
          not_found   = 1
          put_failure = 2
          OTHERS      = 3.

      IF sy-subrc <> 0.
        CLEAR ls_error.
        ls_error-objname = ls_doma-domaname.
        ls_error-objtype = 'DOMA'.
        CASE sy-subrc.
          WHEN 1.
            ls_error-description = 'FM DDIF_DOMA_ACTIVATE exception: not_found'.
          WHEN 2.
            ls_error-description = 'FM DDIF_DOMA_ACTIVATE exception: put_failure'.
          WHEN 3.
            ls_error-description = 'FM DDIF_DOMA_ACTIVATE exception: others'.
          WHEN OTHERS.
            ls_error-description = 'FM DDIF_DOMA_ACTIVATE Unhandled exception'.
        ENDCASE.
        APPEND ls_error TO me->gt_error.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD create_dtel.
    DATA: ls_dtel           LIKE LINE OF me->gt_dtel,
          ls_error          TYPE zotct_cd_s002.

    LOOP AT me->gt_dtel INTO ls_dtel.
      CALL FUNCTION 'RPY_DATAELEMENT_INSERT'
        EXPORTING
          dataelement_name    = ls_dtel-dtelname
          dtel_inf            = ls_dtel
*         LANGUAGE            = SY-LANGU
*         WITH_DOCU           = ' '
*         DOCUTYPE            = 'T'
          development_class   = me->gv_prefix
          transport_number    = ''
* TABLES
*         DOCU_TABLE_USER     =
*         DOCU_TABLE_TECH     =
        EXCEPTIONS
          cancelled           = 1
          already_exist       = 2
          permission_error    = 3
          name_not_allowed    = 4
          name_conflict       = 5
          illegal_type        = 6
          object_inconsistent = 7
          db_access_error     = 8
          OTHERS              = 9.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.


      CALL FUNCTION 'DDIF_DTEL_ACTIVATE'
        EXPORTING
          name        = ls_dtel-dtelname
        EXCEPTIONS
          not_found   = 1
          put_failure = 2
          OTHERS      = 3.
      IF sy-subrc <> 0.
        CLEAR ls_error.
        ls_error-objname = ls_dtel-dtelname.
        ls_error-objtype = 'DTEL'.
        CASE sy-subrc.
          WHEN 1.
            ls_error-description = 'FM DDIF_DTEL_ACTIVATE exception: not_found'.
          WHEN 2.
            ls_error-description = 'FM DDIF_DTEL_ACTIVATE exception: put_failure'.
          WHEN 3.
            ls_error-description = 'FM DDIF_DTEL_ACTIVATE exception: others'.
          WHEN OTHERS.
            ls_error-description = 'FM DDIF_DTEL_ACTIVATE Unhandled exception'.
        ENDCASE.
        APPEND ls_error TO me->gt_error.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_stru.
    DATA: ls_stru      LIKE LINE OF me->gt_stru,
          ls_error     TYPE zotct_cd_s002,

          lt_tabl_fields    TYPE TABLE OF rpy_fiel_u,
          ls_tabl_fields    TYPE rpy_fiel_u,

          ls_objlist_x TYPE ty_objlist_x,
          lv_cnt       TYPE p.

    LOOP AT me->gt_stru INTO ls_stru.
      CLEAR: lt_tabl_fields,
             lv_cnt.
      LOOP AT me->gt_objlist_x INTO ls_objlist_x WHERE stru_x = ls_stru-tablname.
        lv_cnt = lv_cnt + 1.
        CLEAR ls_tabl_fields.
        ls_tabl_fields-tablname = ls_objlist_x-stru_x.
        ls_tabl_fields-fieldname = ls_objlist_x-fieldname.
        ls_tabl_fields-dtelname = ls_objlist_x-dtel.
        IF ls_objlist_x-dtel_x IS NOT INITIAL.
          ls_tabl_fields-dtelname = ls_objlist_x-dtel_x.
        ENDIF.
        ls_tabl_fields-checktable = ''.
        ls_tabl_fields-keyflag = ls_objlist_x-fieldkey.
        ls_tabl_fields-position = lv_cnt.
        ls_tabl_fields-reftable = ''.
        ls_tabl_fields-reffield = ''.
        ls_tabl_fields-inclname = ''.
        ls_tabl_fields-notnull = ''.
        APPEND ls_tabl_fields TO lt_tabl_fields.
      ENDLOOP.

      CALL FUNCTION 'RPY_TABLE_INSERT'
        EXPORTING
*         LANGUAGE          = SY-LANGU
          table_name        = ls_stru-tablname
*         WITH_DOCU         = ' '
*         DOCUTYPE          = 'T'
*         TRANSPORT_NUMBER  = ' '
          development_class = me->gv_prefix
          tabl_inf          = ls_stru
*         TABL_TECHNICS     = ' '
        TABLES
          tabl_fields       = lt_tabl_fields
*         DOCU_TABLE_USER   =
*         DOCU_TABLE_TECH   =
        EXCEPTIONS
          cancelled         = 1
          already_exist     = 2
          permission_error  = 3
          name_not_allowed  = 4
          name_conflict     = 5
          db_access_error   = 6
          OTHERS            = 7.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'DDIF_TABL_ACTIVATE'
        EXPORTING
          name        = ls_stru-tablname
        EXCEPTIONS
          not_found   = 1
          put_failure = 2
          OTHERS      = 3.
      IF sy-subrc <> 0.
        CLEAR ls_error.
        ls_error-objname = ls_stru-tablname.
        ls_error-objtype = 'STRU'.
        CASE sy-subrc.
          WHEN 1.
            ls_error-description = 'FM DDIF_TABL_ACTIVATE exception: not_found'.
          WHEN 2.
            ls_error-description = 'FM DDIF_TABL_ACTIVATE exception: put_failure'.
          WHEN 3.
            ls_error-description = 'FM DDIF_TABL_ACTIVATE exception: others'.
          WHEN OTHERS.
            ls_error-description = 'FM DDIF_TABL_ACTIVATE Unhandled exception'.
        ENDCASE.
        APPEND ls_error TO me->gt_error.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_tabl.
    DATA: ls_tabl           LIKE LINE OF me->gt_tabl,
          ls_error          TYPE zotct_cd_s002,

          lt_tabl_fields    TYPE TABLE OF rpy_fiel_u,
          ls_tabl_fields    TYPE rpy_fiel_u,

          ls_objlist_x      TYPE ty_objlist_x,
          lv_cnt            TYPE p.

    LOOP AT me->gt_tabl INTO ls_tabl.
      CLEAR: lt_tabl_fields,
             lv_cnt.
      LOOP AT me->gt_objlist_x INTO ls_objlist_x WHERE tabl_x = ls_tabl-tablname.
        lv_cnt = lv_cnt + 1.

        CLEAR ls_tabl_fields.
        ls_tabl_fields-tablname = ls_objlist_x-tabl_x.
        ls_tabl_fields-fieldname = ls_objlist_x-fieldname.
        ls_tabl_fields-dtelname = ls_objlist_x-dtel.
        ls_tabl_fields-checktable = ''.
        ls_tabl_fields-keyflag = ls_objlist_x-fieldkey.
        ls_tabl_fields-position = lv_cnt.
        ls_tabl_fields-reftable = ''.
        ls_tabl_fields-reffield = ''.
        ls_tabl_fields-inclname = ''.
        ls_tabl_fields-notnull = ''.
        APPEND ls_tabl_fields TO lt_tabl_fields.
      ENDLOOP.

      CALL FUNCTION 'RPY_TABLE_INSERT'
        EXPORTING
*         LANGUAGE          = SY-LANGU
          table_name        = ls_tabl-tablname
*         WITH_DOCU         = ' '
*         DOCUTYPE          = 'T'
*         TRANSPORT_NUMBER  = ' '
          development_class = me->gv_prefix
          tabl_inf          = ls_tabl
*         TABL_TECHNICS     = ' '
        TABLES
          tabl_fields       = lt_tabl_fields
*         DOCU_TABLE_USER   =
*         DOCU_TABLE_TECH   =
        EXCEPTIONS
          cancelled         = 1
          already_exist     = 2
          permission_error  = 3
          name_not_allowed  = 4
          name_conflict     = 5
          db_access_error   = 6
          OTHERS            = 7.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'DDIF_TABL_ACTIVATE'
        EXPORTING
          name        = ls_tabl-tablname
        EXCEPTIONS
          not_found   = 1
          put_failure = 2
          OTHERS      = 3.
      IF sy-subrc <> 0.
        CLEAR ls_error.
        ls_error-objname = ls_tabl-tablname.
        ls_error-objtype = 'TABL'.
        CASE sy-subrc.
          WHEN 1.
            ls_error-description = 'FM DDIF_TABL_ACTIVATE exception: not_found'.
          WHEN 2.
            ls_error-description = 'FM DDIF_TABL_ACTIVATE exception: put_failure'.
          WHEN 3.
            ls_error-description = 'FM DDIF_TABL_ACTIVATE exception: others'.
          WHEN OTHERS.
            ls_error-description = 'FM DDIF_TABL_ACTIVATE Unhandled exception'.
        ENDCASE.
        APPEND ls_error TO me->gt_error.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_ttyp.
    DATA: ls_ttyp         LIKE LINE OF me->gt_ttyp,
          ls_error        TYPE zotct_cd_s002,
          lv_settype_text TYPE comt_frgtype_text,
          lv_rowtype      TYPE ttrowtype,
          lv_template     TYPE comt_settype_template,
          lv_subrc        TYPE SY-SUBRC.

    LOOP AT me->gt_ttyp INTO ls_ttyp.
      lv_settype_text = ls_ttyp-typename.
      lv_rowtype = ls_ttyp-rowtype.
      lv_template = ls_ttyp-typename.
      CALL FUNCTION 'COM_DDIC_TTYPE_CREATE'
        EXPORTING
          iv_objecttype_id   = ls_ttyp-typename
          iv_settype_text    = lv_settype_text
          iv_rowtype         = lv_rowtype
          iv_langu           = sy-langu
          iv_replace_pattern = ''
          iv_template        = lv_template
          iv_name_suffix     = ''
          iv_devclass        = me->gv_prefix
       IMPORTING
         EV_SUBRC           = lv_subrc
*       CHANGING
*         CT_TRANSPORT       =
        .


      CALL FUNCTION 'DDIF_TTYP_ACTIVATE'
        EXPORTING
          name        = ls_ttyp-typename
        EXCEPTIONS
          not_found   = 1
          put_failure = 2
          OTHERS      = 3.
      IF sy-subrc <> 0.
        CLEAR ls_error.
        ls_error-objname = ls_ttyp-typename.
        ls_error-objtype = 'TTYP'.
        CASE sy-subrc.
          WHEN 1.
            ls_error-description = 'FM DDIF_TTYP_ACTIVATE exception: not_found'.
          WHEN 2.
            ls_error-description = 'FM DDIF_TTYP_ACTIVATE exception: put_failure'.
          WHEN 3.
            ls_error-description = 'FM DDIF_TTYP_ACTIVATE exception: others'.
          WHEN OTHERS.
            ls_error-description = 'FM DDIF_TTYP_ACTIVATE Unhandled exception'.
        ENDCASE.
        APPEND ls_error TO me->gt_error.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD initialize.
    FIELD-SYMBOLS: <dtel> LIKE LINE OF me->gt_dtel,
                   <doma> LIKE LINE OF me->gt_doma,
                   <tabl> LIKE LINE OF me->gt_tabl,
                   <stru> LIKE LINE OF me->gt_stru,
                   <ttyp> LIKE LINE OF me->gt_ttyp.

    DATA: ls_objlist      TYPE zotct_cd_s001,
          ls_dtel_objlist LIKE LINE OF me->gt_dtel_list,
          ls_doma_objlist LIKE LINE OF me->gt_doma_list,
          ls_tabl_objlist LIKE LINE OF me->gt_tabl_list,
          ls_stru_objlist LIKE LINE OF me->gt_stru_list,
          ls_ttyp_objlist LIKE LINE OF me->gt_ttyp_list,
          lv_cnt          TYPE p,
          lv_cnt_c        TYPE char3,
          ls_objlist_x    TYPE ty_objlist_x.

    CLEAR: me->gt_dtel[],
           me->gt_doma[],
           me->gt_tabl[],
           me->gt_stru[],
           me->gt_ttyp[].

    LOOP AT it_objlist INTO ls_objlist.
      CLEAR ls_objlist_x.
      MOVE-CORRESPONDING ls_objlist TO ls_objlist_x.
      APPEND ls_objlist_x TO me->gt_objlist_x.

      me->gv_prefix = ls_objlist-prefix.
    ENDLOOP.

    LOOP AT it_objlist INTO ls_objlist.
      CLEAR ls_dtel_objlist.
      ls_dtel_objlist-fielddesc   = ls_objlist-fielddesc.
*      ls_dtel_objlist-dtel        = ls_objlist-dtel.
      ls_dtel_objlist-doma        = ls_objlist-doma.
      ls_dtel_objlist-datatype    = ls_objlist-datatype.
      ls_dtel_objlist-datalength  = ls_objlist-datalength.
      ls_dtel_objlist-decimals    = ls_objlist-decimals.
      COLLECT ls_dtel_objlist INTO me->gt_dtel_list.

      IF ls_objlist-create_doma IS NOT INITIAL.
        CLEAR ls_doma_objlist.
        ls_doma_objlist-fielddesc   = ls_objlist-fielddesc.
        ls_doma_objlist-dtel        = ls_objlist-dtel.
        ls_doma_objlist-doma        = ls_objlist-doma.
        ls_doma_objlist-datatype    = ls_objlist-datatype.
        ls_doma_objlist-datalength  = ls_objlist-datalength.
        ls_doma_objlist-decimals    = ls_objlist-decimals.
        COLLECT ls_doma_objlist INTO me->gt_doma_list.
      ENDIF.

      CLEAR ls_tabl_objlist.
      ls_tabl_objlist-tablename = ls_objlist-tablename.
      ls_tabl_objlist-tabledesc = ls_objlist-tabledesc.
      COLLECT ls_tabl_objlist INTO me->gt_tabl_list.

      CLEAR ls_stru_objlist.
      ls_stru_objlist-tablename = ls_objlist-tablename.
      ls_stru_objlist-tabledesc = ls_objlist-tabledesc.
      COLLECT ls_stru_objlist INTO me->gt_stru_list.

      CLEAR ls_ttyp_objlist.
      ls_ttyp_objlist-tablename = ls_objlist-tablename.
      ls_ttyp_objlist-tabledesc = ls_objlist-tabledesc.
      COLLECT ls_ttyp_objlist INTO me->gt_ttyp_list.
    ENDLOOP.

    CLEAR: lv_cnt.
    LOOP AT me->gt_dtel_list INTO ls_dtel_objlist.
      LOOP AT me->gt_objlist_x INTO ls_objlist_x WHERE fielddesc  EQ ls_dtel_objlist-fielddesc
                                                   AND dtel       EQ ls_dtel_objlist-dtel
                                                   AND doma       EQ ls_dtel_objlist-doma
                                                   AND datatype   EQ ls_dtel_objlist-datatype
                                                   AND datalength EQ ls_dtel_objlist-datalength
                                                   AND decimals   EQ ls_dtel_objlist-decimals.
        lv_cnt = lv_cnt + 1.
        UNPACK lv_cnt TO lv_cnt_c.

        CONCATENATE ls_objlist_x-prefix '_D' lv_cnt_c INTO ls_objlist_x-dtel_x.
        MODIFY me->gt_objlist_x FROM ls_objlist_x.
      ENDLOOP.
    ENDLOOP.

    CLEAR: lv_cnt.
    LOOP AT me->gt_doma_list INTO ls_doma_objlist.
      LOOP AT me->gt_objlist_x INTO ls_objlist_x WHERE fielddesc  EQ ls_doma_objlist-fielddesc
                                                   AND doma       EQ ls_doma_objlist-doma
                                                   AND datatype   EQ ls_doma_objlist-datatype
                                                   AND datalength EQ ls_doma_objlist-datalength
                                                   AND decimals   EQ ls_doma_objlist-decimals.
        lv_cnt = lv_cnt + 1.
        UNPACK lv_cnt TO lv_cnt_c.

        CONCATENATE ls_objlist_x-prefix '_DO' lv_cnt_c INTO ls_objlist_x-doma_x.
        MODIFY me->gt_objlist_x FROM ls_objlist_x.
      ENDLOOP.
    ENDLOOP.

    CLEAR: lv_cnt.
    LOOP AT me->gt_tabl_list INTO ls_tabl_objlist.
      lv_cnt = lv_cnt + 1.
      UNPACK lv_cnt TO lv_cnt_c.

      LOOP AT me->gt_objlist_x INTO ls_objlist_x WHERE tablename EQ ls_tabl_objlist-tablename
                                                   AND tabledesc EQ ls_tabl_objlist-tabledesc.
        CONCATENATE ls_objlist_x-prefix '_T' lv_cnt_c INTO ls_objlist_x-tabl_x.
        MODIFY me->gt_objlist_x FROM ls_objlist_x.
      ENDLOOP.
    ENDLOOP.

    CLEAR: lv_cnt.
    LOOP AT me->gt_stru_list INTO ls_stru_objlist.
      lv_cnt = lv_cnt + 1.
      UNPACK lv_cnt TO lv_cnt_c.

      LOOP AT me->gt_objlist_x INTO ls_objlist_x WHERE tablename EQ ls_stru_objlist-tablename
                                                   AND tabledesc EQ ls_stru_objlist-tabledesc.
        CONCATENATE ls_objlist_x-prefix '_S' lv_cnt_c INTO ls_objlist_x-stru_x.
        MODIFY me->gt_objlist_x FROM ls_objlist_x.
      ENDLOOP.
    ENDLOOP.

    CLEAR: lv_cnt.
    LOOP AT me->gt_ttyp_list INTO ls_ttyp_objlist.
      lv_cnt = lv_cnt + 1.
      UNPACK lv_cnt TO lv_cnt_c.

      LOOP AT me->gt_objlist_x INTO ls_objlist_x WHERE tablename EQ ls_ttyp_objlist-tablename
                                                   AND tabledesc EQ ls_ttyp_objlist-tabledesc.
        CONCATENATE ls_objlist_x-prefix '_TT' lv_cnt_c INTO ls_objlist_x-ttyp_x.
        MODIFY me->gt_objlist_x FROM ls_objlist_x.
      ENDLOOP.
    ENDLOOP.

*** Set inputs
    LOOP AT me->gt_doma_list INTO ls_doma_objlist.
      READ TABLE me->gt_objlist_x INTO ls_objlist_x WITH KEY fielddesc  = ls_doma_objlist-fielddesc
                                                             datatype   = ls_doma_objlist-datatype
                                                             datalength = ls_doma_objlist-datalength
                                                             decimals   = ls_doma_objlist-decimals.
      IF sy-subrc EQ 0.
        me->set_doma( EXPORTING is_objlist = ls_objlist_x ).
      ENDIF.
    ENDLOOP.

    LOOP AT me->gt_dtel_list INTO ls_dtel_objlist.
      READ TABLE me->gt_objlist_x INTO ls_objlist_x WITH KEY fielddesc  = ls_dtel_objlist-fielddesc
                                                             doma       = ls_dtel_objlist-doma
                                                             datatype   = ls_dtel_objlist-datatype
                                                             datalength = ls_dtel_objlist-datalength
                                                             decimals   = ls_dtel_objlist-decimals.
      IF sy-subrc EQ 0.
        me->set_dtel( EXPORTING is_objlist = ls_objlist_x ).
      ENDIF.
    ENDLOOP.

    LOOP AT me->gt_stru_list INTO ls_stru_objlist.
      READ TABLE me->gt_objlist_x INTO ls_objlist_x WITH KEY tablename = ls_stru_objlist-tablename
                                                             tabledesc = ls_stru_objlist-tabledesc.
      IF sy-subrc EQ 0.
        me->set_stru( EXPORTING is_objlist = ls_objlist_x ).
      ENDIF.
    ENDLOOP.

    LOOP AT me->gt_tabl_list INTO ls_tabl_objlist.
      READ TABLE me->gt_objlist_x INTO ls_objlist_x WITH KEY tablename = ls_tabl_objlist-tablename
                                                             tabledesc = ls_tabl_objlist-tabledesc.
      IF sy-subrc EQ 0.
        me->set_tabl( EXPORTING is_objlist = ls_objlist_x ).
      ENDIF.
    ENDLOOP.

    LOOP AT me->gt_ttyp_list INTO ls_ttyp_objlist.
      READ TABLE me->gt_objlist_x INTO ls_objlist_x WITH KEY tablename = ls_ttyp_objlist-tablename
                                                             tabledesc = ls_ttyp_objlist-tabledesc.
      IF sy-subrc EQ 0.
        me->set_ttyp( EXPORTING is_objlist = ls_objlist_x ).
      ENDIF.
    ENDLOOP.

    me->check_repetitive( ).

    IF me->gt_repetitive[] IS NOT INITIAL.
      EXIT.
    ENDIF.

    me->create_doma( ).
    me->create_dtel( ).
    me->create_stru( ).
    me->create_tabl( ).
    me->create_ttyp( ).

  ENDMETHOD.


  METHOD set_doma.
    FIELD-SYMBOLS: <doma> LIKE LINE OF me->gt_doma.

    APPEND INITIAL LINE TO me->gt_doma ASSIGNING <doma>.
    <doma>-domaname = is_objlist-doma_x.
    <doma>-language = ''.
    <doma>-datatype = is_objlist-datatype.
    <doma>-length = is_objlist-datalength.
    <doma>-outputlen = is_objlist-datalength.
    <doma>-decimals = is_objlist-decimals.
    <doma>-lowercase = ''.
    <doma>-signflag = ''.
*    <doma>-langflag = ''.
    <doma>-valueexist = ''.
  ENDMETHOD.


  METHOD set_dtel.
    FIELD-SYMBOLS: <dtel> LIKE LINE OF me->gt_dtel.

    DATA: lv_domname TYPE DOMNAME.

    IF is_objlist-doma_x IS NOT INITIAL.
      lv_domname = is_objlist-doma_x.
    ENDIF.

    IF is_objlist-doma IS NOT INITIAL.
      lv_domname = is_objlist-doma.
    ENDIF.

    APPEND INITIAL LINE TO gt_dtel ASSIGNING <dtel>.
    <dtel>-dtelname = is_objlist-dtel_x.
    <dtel>-language = ''.
    <dtel>-domaname = lv_domname.
    <dtel>-scrtext_s = is_objlist-fieldscrtext_s.
    <dtel>-scrtext_m = is_objlist-fieldscrtext_m.
    <dtel>-scrtext_l = is_objlist-fieldscrtext_l.
  ENDMETHOD.


  METHOD set_stru.
    FIELD-SYMBOLS: <stru> LIKE LINE OF me->gt_stru.

    APPEND INITIAL LINE TO me->gt_stru ASSIGNING <stru>.
    <stru>-tablname = is_objlist-stru_x.
    <stru>-language = ''.
    <stru>-tablclass = 'INTTAB'.
    <stru>-sqltab = ''.
*    <stru>-datmin = ''.
*    <stru>-datmax = ''.
*    <stru>-datavg = ''.
*    <stru>-clidep = ''.
*    <stru>-buffered = ''.
*    <stru>-comprflag = ''.
*    <stru>-langdep = ''.
*    <stru>-ddtext = ''.
*    <stru>-actflag = ''.
*    <stru>-applclass = ''.
*    <stru>-authclass = ''.
*    <stru>-as4user = ''.
*    <stru>-as4date = ''.
*    <stru>-as4time = ''.
*    <stru>-masterlang = ''.
*    <stru>-mainflag = ''.
*    <stru>-contflag = ''.
*    <stru>-reservetab = ''.
*    <stru>-globalflag = ''.
*    <stru>-prozpuff = ''.
*    <stru>-viewclass = ''.
*    <stru>-viewgrant = ''.
*    <stru>-multiplex = ''.
*    <stru>-shlpexi = ''.
*    <stru>-proxytype = ''.
*    <stru>-exclass = ''.
*    <stru>-wrongcl = ''.
*    <stru>-alwaystrp = ''.
*    <stru>-alldataincl = ''.
*    <stru>-with_parameters = ''.
*    <stru>-exview_included = ''.
*    <stru>-keymax_feature = ''.
*    <stru>-keylen_feature = ''.
*    <stru>-tablen_feature = ''.
*    <stru>-nontrp_included = ''.
*    <stru>-viewref = ''.
*    <stru>-viewref_err = ''.
*    <stru>-viewref_pos_chg = ''.
*    <stru>-tbfunc_included = ''.
*    <stru>-is_gtt = ''.
*    <stru>-session_var_ex = ''.
*    <stru>-from_entity = ''.
*    <stru>-pk_is_invhash = ''.
*    <stru>-used_session_vars = ''.
*    <stru>-hdb_only_entity_included = ''.
  ENDMETHOD.


  METHOD set_tabl.
    FIELD-SYMBOLS: <tabl> LIKE LINE OF me->gt_tabl.

    APPEND INITIAL LINE TO gt_tabl ASSIGNING <tabl>.
    <tabl>-tablname = is_objlist-tabl_x.
    <tabl>-language = ''.
    <tabl>-tablclass = 'TRANSP'.
*    <tabl>-sqltab = ''.
*    <tabl>-datmin = ''.
*    <tabl>-datmax = ''.
*    <tabl>-datavg = ''.
*    <tabl>-clidep = ''.
*    <tabl>-buffered = ''.
*    <tabl>-comprflag = ''.
*    <tabl>-langdep = ''.
*    <tabl>-ddtext = ''.
*    <tabl>-actflag = ''.
*    <tabl>-applclass = ''.
*    <tabl>-authclass = ''.
*    <tabl>-as4user = ''.
*    <tabl>-as4date = ''.
*    <tabl>-as4time = ''.
*    <tabl>-masterlang = ''.
*    <tabl>-mainflag = ''.
*    <tabl>-contflag = ''.
*    <tabl>-reservetab = ''.
*    <tabl>-globalflag = ''.
*    <tabl>-prozpuff = ''.
*    <tabl>-viewclass = ''.
*    <tabl>-viewgrant = ''.
*    <tabl>-multiplex = ''.
*    <tabl>-shlpexi = ''.
*    <tabl>-proxytype = ''.
*    <tabl>-exclass = ''.
*    <tabl>-wrongcl = ''.
*    <tabl>-alwaystrp = ''.
*    <tabl>-alldataincl = ''.
*    <tabl>-with_parameters = ''.
*    <tabl>-exview_included = ''.
*    <tabl>-keymax_feature = ''.
*    <tabl>-keylen_feature = ''.
*    <tabl>-tablen_feature = ''.
*    <tabl>-nontrp_included = ''.
*    <tabl>-viewref = ''.
*    <tabl>-viewref_err = ''.
*    <tabl>-viewref_pos_chg = ''.
*    <tabl>-tbfunc_included = ''.
*    <tabl>-is_gtt = ''.
*    <tabl>-session_var_ex = ''.
*    <tabl>-from_entity = ''.
*    <tabl>-pk_is_invhash = ''.
*    <tabl>-used_session_vars = ''.
*    <tabl>-hdb_only_entity_included = ''.
  ENDMETHOD.


  METHOD set_ttyp.
    FIELD-SYMBOLS: <ttyp> LIKE LINE OF me->gt_ttyp.

    APPEND INITIAL LINE TO gt_ttyp ASSIGNING <ttyp>.
    <ttyp>-typename = is_objlist-ttyp_x.
    <ttyp>-ddlanguage = ''.
    <ttyp>-rowtype = is_objlist-stru_x.
    <ttyp>-rowkind = ''.
    <ttyp>-datatype = ''.
    <ttyp>-leng = ''.
    <ttyp>-decimals = ''.
    <ttyp>-accessmode = ''.
    <ttyp>-keydef = ''.
    <ttyp>-keykind = ''.
    <ttyp>-keyfdcount = ''.
    <ttyp>-generic = ''.
    <ttyp>-as4user = ''.
    <ttyp>-as4date = ''.
    <ttyp>-as4time = ''.
    <ttyp>-ddtext = ''.
    <ttyp>-typelen = ''.
    <ttyp>-actflag = ''.
    <ttyp>-ttypkind = ''.
    <ttyp>-range_ctyp = ''.
    <ttyp>-ctdatatype = ''.
    <ttyp>-ctleng = ''.
    <ttyp>-ctdecimals = ''.
    <ttyp>-reftype = ''.
    <ttyp>-occurs = ''.
    <ttyp>-proxytype = ''.
    <ttyp>-alias = ''.
    <ttyp>-furtherseckey = ''.
  ENDMETHOD.
ENDCLASS.
