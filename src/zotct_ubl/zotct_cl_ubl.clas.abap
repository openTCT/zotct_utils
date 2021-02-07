class ZOTCT_CL_UBL definition
  public
  create public .

public section.

  methods NEST .
  methods SET_NODE
    importing
      !UBLTAB type ZOTCT_TT0002 .
  methods GET_NODE .
  methods GET_XMLSTR
    returning
      value(XMLSTR) type STRINGVAL .
protected section.

  types:
    BEGIN OF ty_data ,
      data      TYPE REF TO data,
      xml_name  TYPE prx_ifrnam,
      abap_name TYPE prx_r3name,
      r3_name   TYPE prx_r3name,
      r3_objtyp TYPE prx_r3obj,
    END OF ty_data .
  types:
    BEGIN OF ty_nodemap,
           node TYPE string,
           id   TYPE string,
           obj  TYPE REF TO if_ixml_element,
           xmlkey TYPE string,
           parentnode TYPE string,
           r3_seqnum TYPE prx_seqnum,
           nestcnt   TYPE syindex,
         END OF ty_nodemap .

  data:
    gt_t0001   TYPE TABLE OF zotct_t0001 .
  data:
    gt_sproxdat TYPE TABLE OF sproxdat .
  data:
    gt_tadir_v TYPE TABLE OF sproxhdr_tadir_v .
  data:
    gt_ttyp TYPE TABLE OF ty_data .
  data:
    gt_tabl TYPE TABLE OF ty_data .
  data:
    gt_nodemap TYPE TABLE OF ty_nodemap .
  data GV_XMLSTR type STRINGVAL .
  data GS_UBL type ref to DATA .
  data GT_FLATTAB type ZOTCT_TT0001 .
  data GCL_DOCUMENT type ref to IF_IXML_DOCUMENT .
  data GCL_ROOT type ref to IF_IXML_ELEMENT .
  data GCL_IXML type ref to IF_IXML .

  methods SET_NAMESPACES .
  methods CREATE_NODEMAP .
  methods GET_PREFIX
    importing
      !IV_XMLKEY type STRING
    returning
      value(RV_PREFIX) type STRING .
private section.

  methods FLATTEN .
  methods GENERATE_NODES
    importing
      !IV_XMLKEY type STRING
    returning
      value(RT_NODES) type STRINGTAB .
ENDCLASS.



CLASS ZOTCT_CL_UBL IMPLEMENTATION.


  METHOD CREATE_NODEMAP.
    FIELD-SYMBOLS: <nodemap>  LIKE LINE OF me->gt_nodemap,
                   <flattab>  LIKE LINE OF me->gt_flattab,
                   <split>    TYPE string,
                   <sproxdat> LIKE LINE OF me->gt_sproxdat.

    DATA: lv_nodestr TYPE string,
          lv_counter TYPE p,
          lt_split   TYPE TABLE OF string,
          lv_lines TYPE p,
          lv_nestcnt TYPE syindex.

    CLEAR: lv_nodestr.

    LOOP AT me->gt_flattab ASSIGNING <flattab>.
      IF <flattab>-attrib IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      IF <flattab>-xmlval IS INITIAL.
        IF lv_nodestr IS NOT INITIAL.
          CONCATENATE lv_nodestr <flattab>-xmlkey INTO lv_nodestr SEPARATED BY '->'.
        ELSE.
          CONCATENATE lv_nodestr <flattab>-xmlkey INTO lv_nodestr.
        ENDIF.

      ELSE.
        APPEND INITIAL LINE TO me->gt_nodemap ASSIGNING <nodemap>.
        <nodemap>-node = lv_nodestr.

        <flattab>-parentkey = lv_nodestr.

        CLEAR: lv_nodestr.
      ENDIF.
    ENDLOOP.

*** Generate inter-nodes

    DATA: lt_collection TYPE TABLE OF string,
          ls_collection TYPE string,
          lt_return TYPE TABLE OF string.

    FIELD-SYMBOLS: <return> TYPE string,
                   <collection> TYPE string.

    LOOP AT me->gt_nodemap ASSIGNING <nodemap>.
      lt_return = me->generate_nodes( iv_xmlkey = <nodemap>-node ).

      LOOP AT lt_return ASSIGNING <return>.
        ls_collection = <return>.
        COLLECT ls_collection INTO lt_collection.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_collection ASSIGNING <collection>.
      APPEND INITIAL LINE TO me->gt_nodemap ASSIGNING <nodemap>.
      <nodemap>-node = <collection>.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM me->gt_nodemap COMPARING node.



    CLEAR: lv_counter.

    LOOP AT me->gt_nodemap ASSIGNING <nodemap>.
      lv_counter = lv_counter + 1.
      <nodemap>-id = lv_counter.
      CLEAR: lt_split[].
      SPLIT <nodemap>-node AT '->' INTO TABLE lt_split.
      LOOP AT lt_split ASSIGNING <split>.
        <nodemap>-xmlkey = <split>.
      ENDLOOP.
      CLEAR: lv_nestcnt.
      DESCRIBE TABLE lt_split LINES lv_nestcnt.
      <nodemap>-nestcnt = lv_nestcnt.
    ENDLOOP.

    LOOP AT me->gt_flattab ASSIGNING <flattab>.
      READ TABLE me->gt_nodemap ASSIGNING <nodemap> WITH KEY node = <flattab>-parentkey.
      IF sy-subrc EQ 0.
        <flattab>-parent = <nodemap>-id.
      ENDIF.
    ENDLOOP.

*    Find previous node
    LOOP AT me->gt_nodemap ASSIGNING <nodemap>.
      CLEAR: lt_split[],
             lv_lines,
             lv_counter.
      SPLIT <nodemap>-node AT '->' INTO TABLE lt_split.
      DESCRIBE TABLE lt_split LINES lv_lines.
      LOOP AT lt_split ASSIGNING <split>.
        lv_counter = lv_counter + 1.
        IF lv_counter EQ lv_lines.
          EXIT.
        ENDIF.
        IF <nodemap>-parentnode IS NOT INITIAL.
          CONCATENATE <nodemap>-parentnode <split> INTO <nodemap>-parentnode SEPARATED BY '->'.
        ELSE.
          CONCATENATE <nodemap>-parentnode <split> INTO <nodemap>-parentnode.
        ENDIF.
      ENDLOOP.

      LOOP AT me->gt_sproxdat ASSIGNING <sproxdat>
                              WHERE ifr_name EQ <nodemap>-xmlkey
                                AND r3_seqnum IS NOT INITIAL.
        <nodemap>-r3_seqnum = <sproxdat>-r3_seqnum.
        EXIT.
      ENDLOOP.
    ENDLOOP.

    SORT me->gt_nodemap BY node ASCENDING.

    DELETE ADJACENT DUPLICATES FROM me->gt_nodemap COMPARING node.

*    SORT me->gt_nodemap BY node ASCENDING
*                           r3_seqnum ASCENDING.

*    SORT me->gt_nodemap BY r3_seqnum ASCENDING
*                           id ASCENDING.

    SORT me->gt_nodemap BY nestcnt ASCENDING
                           r3_seqnum ASCENDING.

  ENDMETHOD.


  METHOD flatten.
  ENDMETHOD.


  METHOD generate_nodes.
    DATA: lt_split TYPE TABLE OF string,
          lv_string TYPE string.

    FIELD-SYMBOLS: <split> TYPE string,
                   <nodes> TYPE string.

    SPLIT iv_xmlkey AT '->' INTO TABLE lt_split.

    LOOP AT lt_split ASSIGNING <split>.
      IF lv_string IS INITIAL.
        CONCATENATE lv_string <split> INTO lv_string.
      ELSE.
        CONCATENATE lv_string <split> INTO lv_string SEPARATED BY '->'.
      ENDIF.

      APPEND INITIAL LINE TO rt_nodes ASSIGNING <nodes>.
      <nodes> = lv_string.
    ENDLOOP.
  ENDMETHOD.


  method GET_NODE.
  endmethod.


  METHOD get_prefix.

  ENDMETHOD.


  METHOD get_xmlstr.
    me->set_namespaces( ).
    xmlstr = me->gv_xmlstr.
  ENDMETHOD.


METHOD nest.
  DATA: lcl_ixml   TYPE REF TO if_ixml,

        lv_counter TYPE p,
        lv_times   TYPE p,
        lv_aseqnr  TYPE p,
        lv_prefix  TYPE string.

  FIELD-SYMBOLS : <ubl>      TYPE any,
                  <flattab>  TYPE zotct_s0001,
                  <tabl>     LIKE LINE OF me->gt_tabl,
                  <ttyp>     LIKE LINE OF me->gt_ttyp,
                  <sproxdat> LIKE LINE OF me->gt_sproxdat,
                  <tadir_v>  LIKE LINE OF me->gt_tadir_v.

*** Map ABAP Name
  LOOP AT gt_flattab ASSIGNING <flattab>.
    IF  <flattab>-table IS INITIAL.
      READ TABLE me->gt_tabl WITH KEY xml_name = <flattab>-xmlkey ASSIGNING <tabl>.
      IF sy-subrc EQ 0.
        <flattab>-abap_name = <tabl>-abap_name.
        <flattab>-r3_name   = <tabl>-r3_name.
        <flattab>-r3_objtyp = <tabl>-r3_objtyp.
      ENDIF.
    ELSE.
      READ TABLE me->gt_ttyp WITH KEY xml_name = <flattab>-xmlkey ASSIGNING <ttyp>.
      IF sy-subrc EQ 0.
        <flattab>-abap_name = <ttyp>-abap_name.
        <flattab>-r3_name   = <ttyp>-r3_name.
        <flattab>-r3_objtyp = <ttyp>-r3_objtyp.
      ENDIF.
    ENDIF.

    LOOP AT me->gt_sproxdat ASSIGNING <sproxdat>
                              WHERE ifr_name EQ <flattab>-xmlkey
                                AND r3_seqnum IS NOT INITIAL.
      <flattab>-r3_seqnum = <sproxdat>-r3_seqnum.
      EXIT.
    ENDLOOP.
  ENDLOOP.
*** Create XML
  TYPES: BEGIN OF ty_nodecoll,
           flattab LIKE gt_flattab,
         END OF ty_nodecoll.


  DATA: lv_seqnr     TYPE seqnr,
        lv_seqnr_max TYPE string,
        lt_nodecoll  TYPE TABLE OF ty_nodecoll,
        gt_flattab_d TYPE TABLE OF zotct_s0001.

  FIELD-SYMBOLS: <flattab_d> LIKE LINE OF gt_flattab_d,
                 <nodecoll>  LIKE LINE OF lt_nodecoll.

  CLEAR: lv_seqnr,
         lv_seqnr_max.

  CALL METHOD zotct_cl_itab_ext=>max
    EXPORTING
      iv_colname = 'SEQNR'
      it_table   = gt_flattab
    RECEIVING
      r_val      = lv_seqnr_max.

  DO lv_seqnr_max TIMES.
    lv_seqnr = lv_seqnr + 1.
    LOOP AT gt_flattab ASSIGNING <flattab> WHERE seqnr EQ lv_seqnr.
      APPEND INITIAL LINE TO gt_flattab_d ASSIGNING <flattab_d>.

      MOVE-CORRESPONDING <flattab> TO <flattab_d>.
    ENDLOOP.
    APPEND INITIAL LINE TO lt_nodecoll ASSIGNING <nodecoll>.
    <nodecoll>-flattab = gt_flattab_d.
    CLEAR: gt_flattab_d[].
  ENDDO.

  me->create_nodemap( ).

*** Render XML tree

  me->gcl_ixml     = cl_ixml=>create( ).
  me->gcl_document = me->gcl_ixml->create_document( ).

  FIELD-SYMBOLS: <nodemap> TYPE ty_nodemap.

  DATA: lcl_parent TYPE REF TO if_ixml_element.
*  Create Parent Node
  LOOP AT me->gt_nodemap ASSIGNING <nodemap>.

    CLEAR: lv_prefix.
    lv_prefix = me->get_prefix( iv_xmlkey = <nodemap>-xmlkey ).

    <nodemap>-obj = me->gcl_document->create_simple_element_ns( name = <nodemap>-xmlkey
                                                                parent = me->gcl_document
                                                                 ).

    LOOP AT me->gt_flattab ASSIGNING <flattab> WHERE xmlval IS NOT INITIAL
                                                   AND parent EQ <nodemap>-id.

      CLEAR: lv_prefix.
      lv_prefix = me->get_prefix( iv_xmlkey = <flattab>-xmlkey ).

      <flattab>-obj = me->gcl_document->create_simple_element_ns( name = <flattab>-xmlkey
                                                                value = <flattab>-xmlval
                                                                parent = <nodemap>-obj
                                                                prefix = lv_prefix ).
    ENDLOOP.
    EXIT.
  ENDLOOP.
*  Create Children - from nodemap
  FIELD-SYMBOLS: <parent>  TYPE ty_nodemap,
                 <aparent> TYPE zotct_s0001.

  LOOP AT me->gt_nodemap ASSIGNING <nodemap>.
    IF <nodemap>-id EQ 1.
      CONTINUE.
    ENDIF.
*    Find parent
    READ TABLE me->gt_nodemap ASSIGNING <parent> WITH KEY node = <nodemap>-parentnode.
    IF sy-subrc EQ 0.
      CLEAR: lv_prefix.
      lv_prefix = me->get_prefix( iv_xmlkey = <nodemap>-xmlkey ).

      <nodemap>-obj = me->gcl_document->create_simple_element_ns( name = <nodemap>-xmlkey
                                                                  parent = <parent>-obj
                                                                  prefix = lv_prefix ).

      LOOP AT me->gt_flattab ASSIGNING <flattab> WHERE xmlval IS NOT INITIAL
                                                   AND parent EQ <nodemap>-id.

        CLEAR: lv_prefix.
        lv_prefix = me->get_prefix( iv_xmlkey = <flattab>-xmlkey ).

        IF <flattab>-attrib IS INITIAL.
          <flattab>-obj = me->gcl_document->create_simple_element_ns( name = <flattab>-xmlkey
                                                                  value = <flattab>-xmlval
                                                                  parent = <nodemap>-obj
                                                                  prefix = lv_prefix ).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  LOOP AT me->gt_flattab ASSIGNING <flattab> WHERE attrib IS NOT INITIAL.
    CLEAR: lv_aseqnr.

    lv_aseqnr = <flattab>-seqnr - 1.

    READ TABLE me->gt_flattab WITH KEY nodnr = <flattab>-nodnr
                                       seqnr = lv_aseqnr
                             ASSIGNING <aparent>.
    IF sy-subrc EQ 0.
      READ TABLE me->gt_flattab WITH KEY seqnr = lv_aseqnr
                                         xmlkey = <aparent>-xmlkey
                               ASSIGNING <aparent>.
      IF sy-subrc EQ 0.
        IF <aparent>-obj IS INITIAL.
          CONTINUE.
        ENDIF.
        <aparent>-obj->set_attribute_ns( name = <flattab>-xmlkey value = <flattab>-xmlval ).
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMETHOD.


  method SET_NAMESPACES.
  endmethod.


  METHOD set_node.
    DATA: gt_split1 TYPE TABLE OF string,
          gt_split2 TYPE TABLE OF string,
          lv_splits TYPE p,
          lv_cnt    TYPE p,
          lv_reptxt TYPE string,
          lv_nodnr  TYPE seqnr.

    FIELD-SYMBOLS: <split>   TYPE string,
                   <split2>  TYPE string,
                   <flattab> LIKE LINE OF me->gt_flattab,
                   <ubltab>  TYPE zotct_s0002.

    CLEAR: lv_nodnr.

    LOOP AT ubltab ASSIGNING <ubltab>.
      lv_nodnr = lv_nodnr + 1.

      CLEAR: gt_split1[],
            gt_split2[],
            lv_splits,
            lv_cnt,
            lv_reptxt,
            gv_xmlstr.

      SPLIT <ubltab>-xmlkey AT '->' INTO TABLE gt_split1.
      DESCRIBE TABLE gt_split1 LINES lv_splits.
*      CLEAR me->gt_flattab[].

      LOOP AT gt_split1 ASSIGNING <split>.
        lv_cnt = lv_cnt + 1.
        APPEND INITIAL LINE TO me->gt_flattab ASSIGNING <flattab>.
        <flattab>-seqnr = lv_cnt.
        <flattab>-nodnr = lv_nodnr.

        IF <split> CA '[]'.
          SPLIT <split> AT '[' INTO TABLE gt_split2.

          READ TABLE gt_split2 ASSIGNING <split2> INDEX 2.
          IF sy-subrc EQ 0.
            REPLACE ALL OCCURRENCES OF ']' IN <split2> WITH space.

            <flattab>-index = <split2>.

            CONCATENATE '[' <split2> ']' INTO lv_reptxt.

            REPLACE ALL OCCURRENCES OF lv_reptxt IN <split> WITH space.
          ENDIF.
          <flattab>-table = abap_true.
        ELSE.
          <flattab>-index = 0.
        ENDIF.

        <flattab>-xmlkey = <split>.
        <flattab>-attrib = <ubltab>-attrib.

        IF lv_cnt EQ lv_splits.
          <flattab>-xmlval = <ubltab>-xmlval.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
