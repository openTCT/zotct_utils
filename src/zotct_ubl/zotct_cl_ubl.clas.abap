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
    DATA : gt_flattab_n TYPE zotct_tt0001.

    DATA :lo_mxml    TYPE REF TO cl_xml_document,
          m_document TYPE REF TO if_ixml_document,
          l_dom      TYPE REF TO if_ixml_element,
          l_ele      TYPE REF TO if_ixml_element,
          l_ele1     TYPE REF TO if_ixml_element,
          m_doctype  TYPE REF TO if_ixml_document_type,
          g_ixml     TYPE REF TO if_ixml,
          l_text     TYPE REF TO if_ixml_text,
          ls_srctab1 TYPE xstring,
          l_retcode  TYPE sysubrc.

    FIELD-SYMBOLS : <ubl>       TYPE any,
                    <flattab_n> TYPE zotct_s0001,
                    <tabl>      LIKE LINE OF me->gt_tabl,
                    <ttyp>      LIKE LINE OF me->gt_ttyp.

    gt_flattab_n[] = gt_flattab[].

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

    CREATE OBJECT lo_mxml.
    CLASS cl_ixml DEFINITION LOAD.
    g_ixml = cl_ixml=>create( ).
    m_document = g_ixml->create_document( ).

    CALL METHOD m_document->create_document_type
      EXPORTING
        name = 'cXML'
      RECEIVING
        rval = m_doctype.

    CALL METHOD m_document->set_document_type
      EXPORTING
        document_type = m_doctype.

    LOOP AT gt_flattab_n ASSIGNING <flattab_n>.
      IF <flattab_n>-attrib IS NOT INITIAL.
        CALL METHOD l_dom->set_attribute
          EXPORTING
            name  = 'payloadID'
            value = gv_xmlstr
*           node  = lo_node
          RECEIVING
            rval  = l_retcode.
      ELSE.
        l_dom = m_document->create_element( name = <flattab_n>-xmlkey ).
        IF <flattab_n>-xmlval IS NOT INITIAL.
          l_dom->set_value( value = <flattab_n>-xmlval ).
        ENDIF.
      ENDIF.

      l_retcode = m_document->append_child( l_dom ).
    ENDLOOP.

    CALL METHOD lo_mxml->create_with_dom
      EXPORTING
        document = m_document.

    DATA lv_stream TYPE string.

    CALL METHOD lo_mxml->render_2_string
      IMPORTING
        stream = lv_stream.
  ENDMETHOD.


  METHOD set_node.

    DATA : gt_split1 TYPE TABLE OF string,
           gt_split2 TYPE TABLE OF string,
           lv_splits TYPE p,
           lv_cnt    TYPE p,
           lv_reptxt TYPE string.

    FIELD-SYMBOLS : <split>   TYPE string,
                    <split2>  TYPE string,
                    <flattab> LIKE LINE OF me->gt_flattab,
                    <ubltab>  TYPE zotct_s0002.

    LOOP AT ubltab ASSIGNING <ubltab>.
      CLEAR : gt_split1[],
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
