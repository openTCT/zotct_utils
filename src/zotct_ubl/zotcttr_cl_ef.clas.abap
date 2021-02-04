class ZOTCTTR_CL_EF definition
  public
  inheriting from ZOTCT_CL_UBL
  final
  create public .

public section.

  methods CONSTRUCTOR
    exceptions
      CX_SCSM_CUSTOMIZING .
protected section.

  data GC_XMLNS type STRING value 'urn:oasis:names:specification:ubl:schema:xsd:Invoice-2' ##NO_TEXT.
  data GC_CAC type STRING value 'urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2' ##NO_TEXT.
  data GC_XADES type STRING value 'http://uri.etsi.org/01903/v1.3.2#' ##NO_TEXT.
  data GC_UDT type STRING value 'urn:un:unece:uncefact:data:specification:UnqualifiedDataTypesSchemaModule:2' ##NO_TEXT.
  data GC_CBC type STRING value 'urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2' ##NO_TEXT.
  data GC_CCTS type STRING value 'urn:un:unece:uncefact:documentation:2' ##NO_TEXT.
  data GC_UBLTR type STRING value 'urn:oasis:names:specification:ubl:schema:xsd:TurkishCustomizationExtensionComponents' ##NO_TEXT.
  data GC_QDT type STRING value 'urn:oasis:names:specification:ubl:schema:xsd:QualifiedDatatypes-2' ##NO_TEXT.
  data GC_EXT type STRING value 'urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2' ##NO_TEXT.
  data GC_DS type STRING value 'http://www.w3.org/2000/09/xmldsig#' ##NO_TEXT.
  data GC_XSI type STRING value 'http://www.w3.org/2001/XMLSchema-instance' ##NO_TEXT.
  data GC_SCHEMALOCATION type STRING value 'urn:oasis:names:specification:ubl:schema:xsd:Invoice-2 ../xsd/maindoc/UBL-Invoice-2.1.xsd' ##NO_TEXT.
  data GC_NS8 type STRING value 'urn:oasis:names:specification:ubl:schema:xsd:ApplicationResponse-2' ##NO_TEXT.
  data GC_DOCNAME type STRING value 'Invoice' ##NO_TEXT.

  methods SET_NAMESPACES
    redefinition .
  methods GET_PREFIX
    redefinition .
private section.
ENDCLASS.



CLASS ZOTCTTR_CL_EF IMPLEMENTATION.


  METHOD constructor.

    FIELD-SYMBOLS: <t0001> LIKE LINE OF me->gt_t0001.

    DATA: lv_prefixp TYPE tabname.

    super->constructor( ).

***  Read configuration data

    SELECT * INTO CORRESPONDING FIELDS OF TABLE me->gt_t0001
      FROM zotct_t0001
      WHERE locale EQ 'TR'
        AND product EQ 'EF'.

    READ TABLE me->gt_t0001 ASSIGNING <t0001> INDEX 1.

    IF sy-subrc NE 0.
      RAISE cx_scsm_customizing.
    ENDIF.

    CLEAR : lv_prefixp.

    CONCATENATE <t0001>-prefix '%' INTO lv_prefixp.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE me->gt_sproxdat
      FROM sproxdat
      WHERE obj_name LIKE lv_prefixp.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE me->gt_tadir_v
      FROM sproxhdr_tadir_v
      WHERE prefix EQ <t0001>-prefix.

*** Prepare structure and table type data

    DATA: data_type TYPE REF TO cl_abap_elemdescr,
          wf_ref    TYPE REF TO data.

    FIELD-SYMBOLS : <sproxdat> LIKE LINE OF me->gt_sproxdat,
                    <data>     TYPE ty_data,
                    <tadir_v>  LIKE LINE OF me->gt_tadir_v.

    LOOP AT me->gt_sproxdat ASSIGNING <sproxdat>.
      CASE <sproxdat>-object1.
        WHEN 'FIEL'.

          IF <sproxdat>-obj_name_r IS INITIAL.
            CONTINUE.
          ENDIF.

          CREATE DATA wf_ref TYPE (<sproxdat>-obj_name_r).

          IF <sproxdat>-object_r EQ 'TTYP'.
            APPEND INITIAL LINE TO me->gt_ttyp ASSIGNING <data>.
          ELSE.
            APPEND INITIAL LINE TO me->gt_tabl ASSIGNING <data>.
          ENDIF.

          <data>-data = wf_ref.
          <data>-abap_name = <sproxdat>-obj_name_r.

          IF <sproxdat>-ifr_name EQ 'base' OR
             <sproxdat>-ifr_name EQ 'Include' OR
             <sproxdat>-ifr_name EQ 'DateType' OR
             <sproxdat>-ifr_name EQ 'TimeType' OR
             <sproxdat>-ifr_name EQ 'TimeType.Content'.

            READ TABLE me->gt_tadir_v ASSIGNING <tadir_v> WITH KEY obj_name_m = <sproxdat>-obj_name_r.
            IF sy-subrc EQ 0.
              <data>-xml_name = <tadir_v>-ifr_name.
            ENDIF.
          ELSE.
            <data>-xml_name = <sproxdat>-ifr_name.
          ENDIF.

          <data>-r3_name = <sproxdat>-obj_name1.
          <data>-r3_objtyp = <sproxdat>-object_r.

        WHEN 'TABL'.

          IF <sproxdat>-obj_name1 IS INITIAL.
            CONTINUE.
          ENDIF.

          CREATE DATA wf_ref TYPE (<sproxdat>-obj_name1).
          IF <sproxdat>-object_r EQ 'TTYP'.
            APPEND INITIAL LINE TO me->gt_ttyp ASSIGNING <data>.
          ELSE.
            APPEND INITIAL LINE TO me->gt_tabl ASSIGNING <data>.
          ENDIF.
          <data>-data = wf_ref.
          <data>-abap_name = <sproxdat>-obj_name1.


          IF <sproxdat>-ifr_name EQ 'base' OR
             <sproxdat>-ifr_name EQ 'Include' OR
             <sproxdat>-ifr_name EQ 'DateType' OR
             <sproxdat>-ifr_name EQ 'TimeType' OR
             <sproxdat>-ifr_name EQ 'TimeType.Content'.

            READ TABLE me->gt_tadir_v ASSIGNING <tadir_v> WITH KEY obj_name_m = <sproxdat>-obj_name1.
            IF sy-subrc EQ 0.
              <data>-xml_name = <tadir_v>-ifr_name.
            ENDIF.
            <data>-xml_name = <sproxdat>-ifr_name.
          ELSE.

          ENDIF.

          <data>-r3_name = <sproxdat>-obj_name1.
          <data>-r3_objtyp = <sproxdat>-object_r.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.

    DATA: lv_tabname TYPE tabname.

    CLEAR: lv_tabname.

    SELECT SINGLE tabname INTO lv_tabname
    FROM dd03vv
    WHERE tabname LIKE lv_prefixp
      AND fieldname EQ 'STANDARD_BUSINESS_DOCUMENT'.

    CREATE DATA gs_ubl TYPE (lv_tabname).
  ENDMETHOD.


  METHOD get_prefix.
    DATA: lv_ifrname TYPE string.

    FIELD-SYMBOLS: <tadir_v> LIKE LINE OF me->gt_tadir_v.

    READ TABLE me->gt_tadir_v WITH KEY ifr_name = iv_xmlkey ASSIGNING <tadir_v>.
    IF sy-subrc EQ 0.
      rv_prefix = <tadir_v>-ifr_nspce.
    ELSE.
      CLEAR: lv_ifrname.
      CONCATENATE iv_xmlkey 'Type' INTO lv_ifrname.
      READ TABLE me->gt_tadir_v WITH KEY ifr_name = lv_ifrname ASSIGNING <tadir_v>.
      IF sy-subrc EQ 0.
        rv_prefix = <tadir_v>-ifr_nspce.
      ENDIF.
    ENDIF.

    CASE rv_prefix.
      WHEN gc_xmlns.
        rv_prefix = 'xmlns'.
      WHEN gc_cac.
        rv_prefix = 'cac'.
      WHEN gc_xades.
        rv_prefix = 'xades'.
      WHEN gc_udt.
        rv_prefix = 'udt'.
      WHEN gc_cbc.
        rv_prefix = 'cbc'.
      WHEN gc_ccts.
        rv_prefix = 'ccts'.
      WHEN gc_ubltr.
        rv_prefix = 'ubltr'.
      WHEN gc_qdt.
        rv_prefix = 'qdt'.
      WHEN gc_ext.
        rv_prefix = 'ext'.
      WHEN gc_ds.
        rv_prefix = 'ds'.
      WHEN gc_xsi.
        rv_prefix = 'xsi'.
      WHEN gc_schemalocation.
        rv_prefix = 'schemaLocation'.
      WHEN gc_ns8.
        rv_prefix = 'ns8'.
      WHEN gc_docname.
        rv_prefix = 'docname'.
    ENDCASE.
  ENDMETHOD.


  METHOD set_namespaces.
*CALL METHOD SUPER->SET_NAMESPACES
*    .

*** set namespaces redefinition
    DATA : lcl_iterator    TYPE REF TO if_ixml_node_iterator,
           lv_name     TYPE string,
           lcl_element TYPE REF TO if_ixml_element,
           lcl_node    TYPE REF TO if_ixml_node,
           lcl_attr    TYPE REF TO if_ixml_attribute.


    lcl_iterator = me->gcl_document->create_iterator( ).
    lcl_node = lcl_iterator->get_next( ).

    WHILE lcl_node IS NOT INITIAL.

      CASE lcl_node->get_type( ).

        WHEN if_ixml_node=>co_node_element.
          lcl_element ?= lcl_node.

          lv_name = lcl_element->get_name( ).
          IF lv_name EQ me->gc_docname.
            lcl_element->set_attribute( name = 'xmlns' value = gc_xmlns ).
            lcl_element->set_attribute( name = 'xmlns:cac' value = gc_cac ).
            lcl_element->set_attribute( name = 'xmlns:cbc' value = gc_cbc ).
            lcl_element->set_attribute( name = 'xmlns:ds' value = gc_ds ).
            lcl_element->set_attribute( name = 'xmlns:ext' value = gc_ext ).
            lcl_element->set_attribute( name = 'xmlns:ns8' value = gc_ns8 ).
            lcl_element->set_attribute( name = 'xmlns:xades' value = gc_xades ).
            lcl_element->set_attribute( name = 'xmlns:xsi' value = gc_xsi ).
            lcl_element->set_attribute( name = 'xmlns:schemaLocation' value = gc_schemalocation ).
            lcl_element->set_attribute( name = 'xmlns:ccts' value = gc_ccts ).
            lcl_element->set_attribute( name = 'xmlns:ubltr' value = gc_ubltr ).
            lcl_element->set_attribute( name = 'xmlns:qdt' value = gc_qdt ).
            lcl_element->set_attribute( name = 'xmlns:udt' value = gc_udt ).

            EXIT.
          ENDIF.
      ENDCASE.

      lcl_node = lcl_iterator->get_next( ).
    ENDWHILE.

    CLEAR me->gv_xmlstr.

    me->gcl_ixml->create_renderer( document = me->gcl_document
                                ostream  = me->gcl_ixml->create_stream_factory(
                                )->create_ostream_cstring( string = gv_xmlstr
                                ) )->render( ).


  ENDMETHOD.
ENDCLASS.
