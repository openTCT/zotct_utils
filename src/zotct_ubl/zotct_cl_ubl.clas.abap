CLASS zotct_cl_ubl DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS nest .
    METHODS set_node
      IMPORTING
        !ubltab TYPE zotct_tt0002 .
    METHODS get_node .
    METHODS get_xmlstr
      RETURNING
        VALUE(xmlstr) TYPE stringval .
    METHODS set_xmlstr
      IMPORTING
        !xmlstr TYPE stringval .
    METHODS flatten .
  PROTECTED SECTION.

    DATA mt_t0001 TYPE zotct_tt0005 .
    DATA mt_sproxdat TYPE prx_t_sproxdat .
    DATA mt_tadir_v TYPE zotct_tt0006 .
    DATA mt_ttyp TYPE zotct_tt0007 .
    DATA mt_tabl TYPE zotct_tt0007 .
    DATA mt_nodemap TYPE zotct_tt0008 .
    DATA mv_xmlstr TYPE stringval .
    DATA mt_flattab TYPE zotct_tt0001 .
    DATA mo_document TYPE REF TO if_ixml_document .
    DATA mo_ixml TYPE REF TO if_ixml .

    METHODS set_namespaces .
    METHODS create_nodemap .
    METHODS get_prefix
      IMPORTING
        !xmlkey       TYPE string
      RETURNING
        VALUE(prefix) TYPE string .
  PRIVATE SECTION.

    METHODS generate_nodes
      IMPORTING
        !xmlkey      TYPE string
      RETURNING
        VALUE(nodes) TYPE stringtab .
ENDCLASS.



CLASS ZOTCT_CL_UBL IMPLEMENTATION.


  METHOD create_nodemap.
    FIELD-SYMBOLS: <nodemap>    LIKE LINE OF me->mt_nodemap,
                   <flattab>    LIKE LINE OF me->mt_flattab,
                   <split>      TYPE string,
                   <sproxdat>   LIKE LINE OF me->mt_sproxdat,
                   <return>     TYPE string,
                   <collection> TYPE string.

    DATA: lv_nodestr    TYPE string,
          lv_counter    TYPE p,
          lt_split      TYPE TABLE OF string,
          lv_lines      TYPE p,
          lv_nestcnt    TYPE syindex,
          lt_collection TYPE TABLE OF string,
          ls_collection TYPE string,
          lt_return     TYPE TABLE OF string.

    CLEAR: lv_nodestr.

    LOOP AT me->mt_flattab ASSIGNING <flattab>.
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
        APPEND INITIAL LINE TO me->mt_nodemap ASSIGNING <nodemap>.
        <nodemap>-node = lv_nodestr.

        <flattab>-parentkey = lv_nodestr.

        CLEAR: lv_nodestr.
      ENDIF.
    ENDLOOP.

*** Generate inter-nodes

    LOOP AT me->mt_nodemap ASSIGNING <nodemap>.
      lt_return = me->generate_nodes( xmlkey = <nodemap>-node ).

      LOOP AT lt_return ASSIGNING <return>.
        ls_collection = <return>.
        COLLECT ls_collection INTO lt_collection.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_collection ASSIGNING <collection>.
      APPEND INITIAL LINE TO me->mt_nodemap ASSIGNING <nodemap>.
      <nodemap>-node = <collection>.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM me->mt_nodemap COMPARING node.

    CLEAR: lv_counter.

    LOOP AT me->mt_nodemap ASSIGNING <nodemap>.
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

    LOOP AT me->mt_flattab ASSIGNING <flattab>.
      READ TABLE me->mt_nodemap ASSIGNING <nodemap> WITH KEY node = <flattab>-parentkey.
      IF sy-subrc EQ 0.
        <flattab>-parent = <nodemap>-id.
      ENDIF.
    ENDLOOP.

*    Find previous node
    LOOP AT me->mt_nodemap ASSIGNING <nodemap>.
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

      LOOP AT me->mt_sproxdat ASSIGNING <sproxdat>
                              WHERE ifr_name EQ <nodemap>-xmlkey
                                AND r3_seqnum IS NOT INITIAL.
        <nodemap>-r3_seqnum = <sproxdat>-r3_seqnum.
        EXIT.
      ENDLOOP.
    ENDLOOP.

    SORT me->mt_nodemap BY node ASCENDING.

    DELETE ADJACENT DUPLICATES FROM me->mt_nodemap COMPARING node.

    SORT me->mt_nodemap BY nestcnt ASCENDING
                           r3_seqnum ASCENDING.

  ENDMETHOD.


  METHOD flatten.

    TYPES: BEGIN OF ty_table,
             parent TYPE string,
             name   TYPE string,
             value  TYPE string,
           END OF ty_table.

    DATA: ixml           TYPE REF TO if_ixml,
          stream_factory TYPE REF TO if_ixml_stream_factory,
          istream        TYPE REF TO if_ixml_istream,
          document       TYPE REF TO if_ixml_document,
          parser         TYPE REF TO if_ixml_parser,
          gt_table       TYPE STANDARD TABLE OF ty_table,
          lv_name        TYPE string,
          lv_value       TYPE string,
          iterator       TYPE REF TO if_ixml_node_iterator,
          node           TYPE REF TO if_ixml_node,
          parent         TYPE REF TO if_ixml_node,
          lt_parent      TYPE STANDARD TABLE OF string,
          lv_parentname  TYPE string.

    FIELD-SYMBOLS: <table>  TYPE ty_table,
                   <parent> TYPE string.

    ixml = cl_ixml=>create( ).
    stream_factory = ixml->create_stream_factory( ).

    istream = stream_factory->create_istream_string( me->mv_xmlstr ).

    document = ixml->create_document( ).

    parser = ixml->create_parser( stream_factory = stream_factory
                         istream        = istream
                         document       = document ).

    parser->parse( ).

    iterator = document->create_iterator( ).
    node = iterator->get_next( ).
    WHILE node IS NOT INITIAL.
      node = iterator->get_next( ).

      IF node IS INITIAL.
        CONTINUE.
      ENDIF.
      CLEAR: lv_name,
             lv_value.

      lv_name = node->get_name( ).
      lv_value = node->get_value( ).

*    Get parent
      parent = node.
      CLEAR: lt_parent[].

      DO.
        parent = parent->get_parent( ).
        IF parent IS INITIAL.
          EXIT.
        ENDIF.
        lv_parentname = parent->get_name( ).


        INSERT INITIAL LINE INTO lt_parent ASSIGNING <parent> INDEX 1.
        <parent> = lv_parentname.
      ENDDO.

      CLEAR: lv_parentname.

      LOOP AT lt_parent ASSIGNING <parent>.
        IF lv_parentname IS INITIAL.
          lv_parentname = <parent>.
        ELSE.
          CONCATENATE lv_parentname <parent> INTO lv_parentname SEPARATED BY '->'.
        ENDIF.
      ENDLOOP.

      IF lv_name(1) NE '#'.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO gt_table ASSIGNING <table>.

      <table>-parent = lv_parentname.
      <table>-name   = lv_name.
      <table>-value  = lv_value.

    ENDWHILE.
  ENDMETHOD.


  METHOD generate_nodes.
    DATA: lt_split  TYPE TABLE OF string,
          lv_string TYPE string.

    FIELD-SYMBOLS: <split> TYPE string,
                   <nodes> TYPE string.

    SPLIT xmlkey AT '->' INTO TABLE lt_split.

    LOOP AT lt_split ASSIGNING <split>.
      IF lv_string IS INITIAL.
        CONCATENATE lv_string <split> INTO lv_string.
      ELSE.
        CONCATENATE lv_string <split> INTO lv_string SEPARATED BY '->'.
      ENDIF.

      APPEND INITIAL LINE TO nodes ASSIGNING <nodes>.
      <nodes> = lv_string.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_node.
  ENDMETHOD.


  METHOD get_prefix.

  ENDMETHOD.


  METHOD get_xmlstr.
    me->set_namespaces( ).
    xmlstr = me->mv_xmlstr.
  ENDMETHOD.


  METHOD nest.
    TYPES: BEGIN OF ty_nodecoll,
             flattab LIKE mt_flattab,
           END OF ty_nodecoll.

    DATA: lv_aseqnr    TYPE p,
          lv_anodnr    TYPE p,
          lv_prefix    TYPE string,
          lv_seqnr     TYPE seqnr,
          lv_seqnr_max TYPE string,
          lt_nodecoll  TYPE TABLE OF ty_nodecoll,
          gt_flattab_d TYPE TABLE OF zotct_s0001.

    FIELD-SYMBOLS: <flattab>   TYPE zotct_s0001,
                   <tabl>      LIKE LINE OF me->mt_tabl,
                   <ttyp>      LIKE LINE OF me->mt_ttyp,
                   <sproxdat>  LIKE LINE OF me->mt_sproxdat,
                   <flattab_d> LIKE LINE OF gt_flattab_d,
                   <nodecoll>  LIKE LINE OF lt_nodecoll,
                   <nodemap>   TYPE zotct_s0007,
                   <parent>    TYPE zotct_s0007,
                   <aparent>   TYPE zotct_s0001.

*** Map ABAP Name
    LOOP AT mt_flattab ASSIGNING <flattab>.
      IF <flattab>-table IS INITIAL.
        READ TABLE me->mt_tabl WITH KEY xml_name = <flattab>-xmlkey ASSIGNING <tabl>.
        IF sy-subrc EQ 0.
          <flattab>-abap_name = <tabl>-abap_name.
          <flattab>-r3_name   = <tabl>-r3_name.
          <flattab>-r3_objtyp = <tabl>-r3_objtyp.
        ENDIF.
      ELSE.
        READ TABLE me->mt_ttyp WITH KEY xml_name = <flattab>-xmlkey ASSIGNING <ttyp>.
        IF sy-subrc EQ 0.
          <flattab>-abap_name = <ttyp>-abap_name.
          <flattab>-r3_name   = <ttyp>-r3_name.
          <flattab>-r3_objtyp = <ttyp>-r3_objtyp.
        ENDIF.
      ENDIF.

      LOOP AT me->mt_sproxdat ASSIGNING <sproxdat>
                                WHERE ifr_name EQ <flattab>-xmlkey
                                  AND r3_seqnum IS NOT INITIAL.
        <flattab>-r3_seqnum = <sproxdat>-r3_seqnum.
        EXIT.
      ENDLOOP.
    ENDLOOP.
*** Create XML
    CLEAR: lv_seqnr,
           lv_seqnr_max.

    CALL METHOD zotct_cl_itab_ext=>max
      EXPORTING
        colname = 'SEQNR'
        table   = mt_flattab
      RECEIVING
        val     = lv_seqnr_max.

    DO lv_seqnr_max TIMES.
      lv_seqnr = lv_seqnr + 1.
      LOOP AT mt_flattab ASSIGNING <flattab> WHERE seqnr EQ lv_seqnr.
        APPEND INITIAL LINE TO gt_flattab_d ASSIGNING <flattab_d>.

        MOVE-CORRESPONDING <flattab> TO <flattab_d>.
      ENDLOOP.
      APPEND INITIAL LINE TO lt_nodecoll ASSIGNING <nodecoll>.
      <nodecoll>-flattab = gt_flattab_d.
      CLEAR: gt_flattab_d[].
    ENDDO.

    me->create_nodemap( ).

*** Render XML tree

    me->mo_ixml     = cl_ixml=>create( ).
    me->mo_document = me->mo_ixml->create_document( ).

*  Create Parent Node
    LOOP AT me->mt_nodemap ASSIGNING <nodemap>.
      CLEAR: lv_prefix.
      lv_prefix = me->get_prefix( xmlkey = <nodemap>-xmlkey ).

      <nodemap>-obj = me->mo_document->create_simple_element_ns( name = <nodemap>-xmlkey
                                                                  parent = me->mo_document ).

      LOOP AT me->mt_flattab ASSIGNING <flattab> WHERE xmlval IS NOT INITIAL
                                                     AND parent EQ <nodemap>-id.

        CLEAR: lv_prefix.
        lv_prefix = me->get_prefix( xmlkey = <flattab>-xmlkey ).

        <flattab>-obj = me->mo_document->create_simple_element_ns( name = <flattab>-xmlkey
                                                                  parent = <nodemap>-obj
                                                                  prefix = lv_prefix
                                                                  value = <flattab>-xmlval ).
      ENDLOOP.
      EXIT.
    ENDLOOP.
*  Create Children - from nodemap
    LOOP AT me->mt_nodemap ASSIGNING <nodemap>.
      IF <nodemap>-id EQ 1.
        CONTINUE.
      ENDIF.
*    Find parent
      READ TABLE me->mt_nodemap ASSIGNING <parent> WITH KEY node = <nodemap>-parentnode.
      IF sy-subrc EQ 0.
        CLEAR: lv_prefix.
        lv_prefix = me->get_prefix( xmlkey = <nodemap>-xmlkey ).

        <nodemap>-obj = me->mo_document->create_simple_element_ns( name = <nodemap>-xmlkey
                                                                    parent = <parent>-obj
                                                                    prefix = lv_prefix ).

        LOOP AT me->mt_flattab ASSIGNING <flattab> WHERE xmlval IS NOT INITIAL
                                                     AND parent EQ <nodemap>-id.

          CLEAR: lv_prefix.
          lv_prefix = me->get_prefix( xmlkey = <flattab>-xmlkey ).

          IF <flattab>-attrib IS INITIAL.
            <flattab>-obj = me->mo_document->create_simple_element_ns( name = <flattab>-xmlkey
                                                                    parent = <nodemap>-obj
                                                                    prefix = lv_prefix
                                                                    value = <flattab>-xmlval ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    LOOP AT me->mt_flattab ASSIGNING <flattab> WHERE attrib IS NOT INITIAL.
      CLEAR: lv_aseqnr,
             lv_anodnr.

      lv_aseqnr = <flattab>-seqnr - 1.
      lv_anodnr = <flattab>-nodnr - 1.

      READ TABLE me->mt_flattab WITH KEY nodnr = <flattab>-nodnr
                                         seqnr = lv_aseqnr
                               ASSIGNING <aparent>.
      IF sy-subrc EQ 0.
        READ TABLE me->mt_flattab WITH KEY nodnr = lv_anodnr
                                           seqnr = lv_aseqnr
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


  METHOD set_namespaces.
  ENDMETHOD.


  METHOD set_node.
    DATA: gt_split1 TYPE TABLE OF string,
          gt_split2 TYPE TABLE OF string,
          lv_splits TYPE p,
          lv_cnt    TYPE p,
          lv_reptxt TYPE string,
          lv_nodnr  TYPE seqnr.

    FIELD-SYMBOLS: <split>   TYPE string,
                   <split2>  TYPE string,
                   <flattab> LIKE LINE OF me->mt_flattab,
                   <ubltab>  TYPE zotct_s0002.

    CLEAR: lv_nodnr.

    LOOP AT ubltab ASSIGNING <ubltab>.
      lv_nodnr = lv_nodnr + 1.

      CLEAR: gt_split1[],
            gt_split2[],
            lv_splits,
            lv_cnt,
            lv_reptxt,
            mv_xmlstr.

      SPLIT <ubltab>-xmlkey AT '->' INTO TABLE gt_split1.
      DESCRIBE TABLE gt_split1 LINES lv_splits.

      LOOP AT gt_split1 ASSIGNING <split>.
        lv_cnt = lv_cnt + 1.
        APPEND INITIAL LINE TO me->mt_flattab ASSIGNING <flattab>.
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


  METHOD set_xmlstr.
    me->mv_xmlstr = xmlstr.
  ENDMETHOD.
ENDCLASS.
