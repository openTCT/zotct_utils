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
            data TYPE REF TO data ,
            xml_name TYPE prx_ifrnam ,
            abap_name TYPE prx_r3name ,
            r3_name TYPE prx_r3name ,
            r3_objtyp TYPE prx_r3obj ,
            END OF ty_data .

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
  data GV_XMLSTR type STRINGVAL .
  data GS_UBL type ref to DATA .
  data GT_FLATTAB type ZOTCT_TT0001 .
  data GCL_DOCUMENT type ref to IF_IXML_DOCUMENT .
  data GCL_ROOT type ref to IF_IXML_ELEMENT .
  data GCL_IXML type ref to IF_IXML .

  methods SET_NAMESPACES .
private section.

  methods FLATTEN .
ENDCLASS.



CLASS ZOTCT_CL_UBL IMPLEMENTATION.


  method FLATTEN.
  endmethod.


  method GET_NODE.
  endmethod.


  METHOD get_xmlstr.
    me->set_namespaces( ).
    xmlstr = me->gv_xmlstr.
  ENDMETHOD.


METHOD nest.
  TYPES: BEGIN OF ty_flattab_n,
           node TYPE REF TO if_ixml_node.
           INCLUDE TYPE zotct_s0001.
         TYPES END OF ty_flattab_n.

  DATA: gt_flattab_n TYPE TABLE OF ty_flattab_n,
        lv_stream    TYPE string,
        lv_name      TYPE string,
        lcl_iterator TYPE REF TO if_ixml_node_iterator,
        lcl_node     TYPE REF TO if_ixml_node,
        lcl_itenode  TYPE REF TO if_ixml_node,
        lcl_root     TYPE REF TO if_ixml_element,
        lcl_element  TYPE REF TO if_ixml_element,

        lv_counter   TYPE p,
        lv_times     TYPE p.

  DATA: lcl_prevnode TYPE REF TO if_ixml_node.

  FIELD-SYMBOLS : <ubl>       TYPE any,
                  <flattab_n> TYPE ty_flattab_n,
                  <flattab>   TYPE zotct_s0001,
                  <tabl>      LIKE LINE OF me->gt_tabl,
                  <ttyp>      LIKE LINE OF me->gt_ttyp.

  LOOP AT gt_flattab ASSIGNING <flattab>.
    APPEND INITIAL LINE TO gt_flattab_n ASSIGNING <flattab_n>.
    MOVE-CORRESPONDING <flattab> TO <flattab_n>.
  ENDLOOP.
*** Map ABAP Name
  LOOP AT gt_flattab_n ASSIGNING <flattab_n>.
    IF  <flattab_n>-table IS INITIAL.
      READ TABLE me->gt_tabl WITH KEY xml_name = <flattab_n>-xmlkey ASSIGNING <tabl>.
      IF sy-subrc EQ 0.
        <flattab_n>-abap_name = <tabl>-abap_name.
        <flattab_n>-r3_name   = <tabl>-r3_name.
        <flattab_n>-r3_objtyp = <tabl>-r3_objtyp.
      ENDIF.
    ELSE.
      READ TABLE me->gt_ttyp WITH KEY xml_name = <flattab_n>-xmlkey ASSIGNING <ttyp>.
      IF sy-subrc EQ 0.
        <flattab_n>-abap_name = <ttyp>-abap_name.
        <flattab_n>-r3_name   = <ttyp>-r3_name.
        <flattab_n>-r3_objtyp = <ttyp>-r3_objtyp.
      ENDIF.
    ENDIF.
  ENDLOOP.
*** Create XML
  TYPES: BEGIN OF ty_nodecoll,
           flattab LIKE gt_flattab_n,
         END OF ty_nodecoll.


  DATA: lv_seqnr     TYPE seqnr,
        lv_seqnr_max TYPE string,
        lt_nodecoll  TYPE TABLE OF ty_nodecoll,
        gt_flattab_d TYPE TABLE OF ty_flattab_n.

  FIELD-SYMBOLS: <flattab_d> LIKE LINE OF gt_flattab_d,
                 <nodecoll>  LIKE LINE OF lt_nodecoll.

  CLEAR: lv_seqnr,
         lv_seqnr_max.

  CALL METHOD zotct_cl_itab_ext=>max
    EXPORTING
      iv_colname = 'SEQNR'
      it_table   = gt_flattab_n
    RECEIVING
      r_val      = lv_seqnr_max.

  DO lv_seqnr_max TIMES.
    lv_seqnr = lv_seqnr + 1.
    LOOP AT gt_flattab_n ASSIGNING <flattab_n> WHERE seqnr EQ lv_seqnr.
      APPEND INITIAL LINE TO gt_flattab_d ASSIGNING <flattab_d>.

      MOVE-CORRESPONDING <flattab_n> TO <flattab_d>.
    ENDLOOP.
    APPEND INITIAL LINE TO lt_nodecoll ASSIGNING <nodecoll>.
    <nodecoll>-flattab = gt_flattab_d.
    CLEAR: gt_flattab_d[].
  ENDDO.

*** Render XML tree
  CLEAR: lcl_node.

  me->gcl_ixml = cl_ixml=>create( ).
  me->gcl_document = me->gcl_ixml->create_document( ).


  LOOP AT lt_nodecoll ASSIGNING <nodecoll>.
    LOOP AT <nodecoll>-flattab ASSIGNING <flattab_n>.

      IF <flattab_n>-seqnr EQ 1 AND
         <flattab_n>-nodnr EQ 1.
        lcl_node = me->gcl_document->create_element_ns( name = <flattab_n>-xmlkey ).
        IF <flattab_n>-xmlval IS NOT INITIAL.
          lcl_node->append_child( me->gcl_document->create_text( <flattab_n>-xmlval ) ).
        ENDIF.
        me->gcl_document->append_child( lcl_node ).
      ELSE.
*        lcl_node = lcl_element->get_prev( ).
        lcl_element = me->gcl_document->find_from_name_ns( name = <flattab_n>-xmlkey ).

        IF lcl_element IS INITIAL.
          lcl_element = me->gcl_document->create_element_ns( name = <flattab_n>-xmlkey ).
          IF <flattab_n>-xmlval IS NOT INITIAL.
            lcl_element->append_child( me->gcl_document->create_text( <flattab_n>-xmlval ) ).
          ENDIF.

        ELSE.
          CONTINUE.
        ENDIF.

        lcl_node->append_child( lcl_element ).
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  me->gcl_ixml->create_renderer( document = me->gcl_document
                                ostream  = me->gcl_ixml->create_stream_factory(
                                )->create_ostream_cstring( string = gv_xmlstr
                                ) )->render( ).

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
