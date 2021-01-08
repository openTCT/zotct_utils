class ZOTCT_CL_UBL definition
  public
  create public .

public section.

  methods SET_NODE
    importing
      !UBLTAB type ZOTCT_TT0002 .
  methods GET_NODE .
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
  data GC_DOCUMENT type ref to IF_IXML_DOCUMENT .
  data GC_ROOT type ref to IF_IXML_ELEMENT .
  data GC_IXML type ref to IF_IXML .

  methods FLATTEN .
  methods NEST .
private section.
ENDCLASS.



CLASS ZOTCT_CL_UBL IMPLEMENTATION.


  method FLATTEN.
  endmethod.


  method GET_NODE.
  endmethod.


  METHOD nest.
    TYPES: BEGIN OF ty_flattab_n,
             node TYPE REF TO if_ixml_node.
             INCLUDE TYPE zotct_s0001.
           TYPES END OF ty_flattab_n.

    DATA: gt_flattab_n TYPE TABLE OF ty_flattab_n,
          lv_stream    TYPE string,
          lv_name      TYPE string,
          iterator     TYPE REF TO if_ixml_node_iterator,
          lc_node      TYPE REF TO if_ixml_node.

    DATA: lc_prevnode TYPE REF TO if_ixml_node.

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
    IF me->gc_ixml IS INITIAL.
      me->gc_ixml     = cl_ixml=>create( ). " if_ixml
      me->gc_document = me->gc_ixml->create_document( ). "IF_IXML_DOCUMENT

      me->gc_root = me->gc_document->create_element_ns( name = 'abap'  "IF_IXML_ELEMENT
                                                "prefix = 'asx'
                                                ).

      me->gc_root->set_attribute_ns( name =  'version'
                              value = '1.0' ).
      me->gc_document->append_child( me->gc_root ).

      CLEAR: lc_prevnode.
      LOOP AT gt_flattab_n ASSIGNING <flattab_n>.

        <flattab_n>-node = me->gc_document->create_element_ns( name = <flattab_n>-xmlkey ).
        IF <flattab_n>-xmlval IS NOT INITIAL.
          <flattab_n>-node->append_child( me->gc_document->create_text( <flattab_n>-xmlval ) ).
        ENDIF.

        IF lc_prevnode IS INITIAL.
          me->gc_root->append_child( <flattab_n>-node ).
        ELSE.
          lc_prevnode->append_child( <flattab_n>-node ).
        ENDIF.

        lc_prevnode = <flattab_n>-node.
      ENDLOOP.
    ELSE.
      iterator = me->gc_document->create_iterator( ).

      DO 2 TIMES.
        <flattab_n>-node = iterator->get_next( ).
      ENDDO.

      LOOP AT gt_flattab_n ASSIGNING <flattab_n>.
        CLEAR: lv_name.
        <flattab_n>-node = iterator->get_next( ).

        lv_name = <flattab_n>-node->get_name( ).

        IF lv_name EQ <flattab_n>-xmlkey.
          lc_prevnode = <flattab_n>-node.
          CONTINUE.
        ELSE.
          <flattab_n>-node = me->gc_document->create_element_ns( name = <flattab_n>-xmlkey ).

          IF <flattab_n>-xmlval IS NOT INITIAL.
            <flattab_n>-node->append_child( me->gc_document->create_text( <flattab_n>-xmlval ) ).
          ENDIF.

          lc_prevnode->append_child( <flattab_n>-node ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    me->gc_ixml->create_renderer( document = me->gc_document
                                  ostream  = me->gc_ixml->create_stream_factory(
                                  )->create_ostream_cstring( string = gv_xmlstr
                                  ) )->render( ).
  ENDMETHOD.


  METHOD set_node.

    DATA: gt_split1 TYPE TABLE OF string,
          gt_split2 TYPE TABLE OF string,
          lv_splits TYPE p,
          lv_cnt    TYPE p,
          lv_reptxt TYPE string.

    FIELD-SYMBOLS: <split>   TYPE string,
                   <split2>  TYPE string,
                   <flattab> LIKE LINE OF me->gt_flattab,
                   <ubltab>  TYPE zotct_s0002.

    LOOP AT ubltab ASSIGNING <ubltab>.
      CLEAR: gt_split1[],
            gt_split2[],
            lv_splits,
            lv_cnt,
            lv_reptxt.

      SPLIT <ubltab>-xmlkey AT '->' INTO TABLE gt_split1.
      DESCRIBE TABLE gt_split1 LINES lv_splits.
      CLEAR me->gt_flattab[].

      LOOP AT gt_split1 ASSIGNING <split>.
        lv_cnt = lv_cnt + 1.
        APPEND INITIAL LINE TO me->gt_flattab ASSIGNING <flattab>.

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

      me->nest( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
