CLASS zotcttr_cl_ef DEFINITION
  PUBLIC
  INHERITING FROM zotct_cl_ubl
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      EXCEPTIONS
        cx_scsm_customizing .

    METHODS get_xmlstr
        REDEFINITION .
  PROTECTED SECTION.

    DATA mc_xmlns
          TYPE string
          VALUE 'urn:oasis:names:specification:ubl:schema:xsd:Invoice-2' ##NO_TEXT.
    DATA mc_cac
          TYPE string
          VALUE 'urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2' ##NO_TEXT.
    DATA mc_xades
          TYPE string
          VALUE 'http://uri.etsi.org/01903/v1.3.2#' ##NO_TEXT.
    DATA mc_udt
          TYPE string
          VALUE 'urn:un:unece:uncefact:data:specification:UnqualifiedDataTypesSchemaModule:2' ##NO_TEXT.
    DATA mc_cbc
          TYPE string
          VALUE 'urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2' ##NO_TEXT.
    DATA mc_ccts
          TYPE string
          VALUE 'urn:un:unece:uncefact:documentation:2' ##NO_TEXT.
    DATA mc_ubltr
          TYPE string
          VALUE 'urn:oasis:names:specification:ubl:schema:xsd:TurkishCustomizationExtensionComponents' ##NO_TEXT.
    DATA mc_qdt
          TYPE string
          VALUE 'urn:oasis:names:specification:ubl:schema:xsd:QualifiedDatatypes-2' ##NO_TEXT.
    DATA mc_ext
          TYPE string
          VALUE 'urn:oasis:names:specification:ubl:schema:xsd:CommonExtensionComponents-2' ##NO_TEXT.
    DATA mc_ds
          TYPE string
          VALUE 'http://www.w3.org/2000/09/xmldsig#' ##NO_TEXT.
    DATA mc_xsi
          TYPE string
          VALUE 'http://www.w3.org/2001/XMLSchema-instance' ##NO_TEXT.
    DATA mc_schemalocation
          TYPE string
          VALUE 'urn:oasis:names:specification:ubl:schema:xsd:Invoice-2 UBL-Invoice-2.1.xsd' ##NO_TEXT.
    DATA mc_ns8
          TYPE string
          VALUE 'urn:oasis:names:specification:ubl:schema:xsd:ApplicationResponse-2' ##NO_TEXT.
    DATA mc_docname
          TYPE string
          VALUE 'Invoice' ##NO_TEXT.

    METHODS create_nodemap
        REDEFINITION .
    METHODS get_prefix
        REDEFINITION .
    METHODS set_namespaces
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZOTCTTR_CL_EF IMPLEMENTATION.


  METHOD constructor.
    FIELD-SYMBOLS: <t0001> LIKE LINE OF me->mt_t0001,
                   <sproxdat> LIKE LINE OF me->mt_sproxdat,
                   <data>     TYPE zotct_s0006,
                   <tadir_v>  LIKE LINE OF me->mt_tadir_v.

    DATA: lv_prefixp TYPE tabname,
          wf_ref     TYPE REF TO data.

    super->constructor( ).

***  Read configuration data

    SELECT * INTO CORRESPONDING FIELDS OF TABLE me->mt_t0001
      FROM zotct_t0001
      WHERE locale EQ 'TR'
        AND product EQ 'EF'.

    READ TABLE me->mt_t0001 ASSIGNING <t0001> INDEX 1.

    IF sy-subrc NE 0.
      RAISE cx_scsm_customizing.
    ENDIF.

    CLEAR: lv_prefixp.

    CONCATENATE <t0001>-prefix '%' INTO lv_prefixp.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE me->mt_sproxdat
      FROM sproxdat
      WHERE obj_name LIKE lv_prefixp.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE me->mt_tadir_v
      FROM sproxhdr_tadir_v
      WHERE prefix EQ <t0001>-prefix.

*** Prepare structure and table type data
    LOOP AT me->mt_sproxdat ASSIGNING <sproxdat>.
      CASE <sproxdat>-object1.
        WHEN 'FIEL'.

          IF <sproxdat>-obj_name_r IS INITIAL.
            CONTINUE.
          ENDIF.

          CREATE DATA wf_ref TYPE (<sproxdat>-obj_name_r).

          IF <sproxdat>-object_r EQ 'TTYP'.
            APPEND INITIAL LINE TO me->mt_ttyp ASSIGNING <data>.
          ELSE.
            APPEND INITIAL LINE TO me->mt_tabl ASSIGNING <data>.
          ENDIF.

          <data>-data = wf_ref.
          <data>-abap_name = <sproxdat>-obj_name_r.

          IF <sproxdat>-ifr_name EQ 'base' OR
             <sproxdat>-ifr_name EQ 'Include' OR
             <sproxdat>-ifr_name EQ 'DateType' OR
             <sproxdat>-ifr_name EQ 'TimeType' OR
             <sproxdat>-ifr_name EQ 'TimeType.Content'.

            READ TABLE me->mt_tadir_v ASSIGNING <tadir_v> WITH KEY obj_name_m = <sproxdat>-obj_name_r.
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
            APPEND INITIAL LINE TO me->mt_ttyp ASSIGNING <data>.
          ELSE.
            APPEND INITIAL LINE TO me->mt_tabl ASSIGNING <data>.
          ENDIF.
          <data>-data = wf_ref.
          <data>-abap_name = <sproxdat>-obj_name1.


          IF <sproxdat>-ifr_name EQ 'base' OR
             <sproxdat>-ifr_name EQ 'Include' OR
             <sproxdat>-ifr_name EQ 'DateType' OR
             <sproxdat>-ifr_name EQ 'TimeType' OR
             <sproxdat>-ifr_name EQ 'TimeType.Content'.

            READ TABLE me->mt_tadir_v ASSIGNING <tadir_v> WITH KEY obj_name_m = <sproxdat>-obj_name1.
            IF sy-subrc EQ 0.
              <data>-xml_name = <tadir_v>-ifr_name.
            ENDIF.
            <data>-xml_name = <sproxdat>-ifr_name.
          ENDIF.

          <data>-r3_name = <sproxdat>-obj_name1.
          <data>-r3_objtyp = <sproxdat>-object_r.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_nodemap.
    FIELD-SYMBOLS: <nodemap> LIKE LINE OF me->mt_nodemap.

    CALL METHOD super->create_nodemap.
***  Rearrange nodemap order for Turkey e-Fatura
    LOOP AT me->mt_nodemap ASSIGNING <nodemap>.
      CASE <nodemap>-node.
        WHEN 'Invoice->UBLExtensions'.
          <nodemap>-r3_seqnum = 1.
        WHEN 'Invoice->UBLVersionID'.
          <nodemap>-r3_seqnum = 2.
        WHEN 'Invoice->CustomizationID'.
          <nodemap>-r3_seqnum = 3.
        WHEN 'Invoice->ProfileID'.
          <nodemap>-r3_seqnum = 4.
        WHEN 'Invoice->ID'.
          <nodemap>-r3_seqnum = 5.
        WHEN 'Invoice->CopyIndicator'.
          <nodemap>-r3_seqnum = 6.
        WHEN 'Invoice->UUID'.
          <nodemap>-r3_seqnum = 7.
        WHEN 'Invoice->IssueDate'.
          <nodemap>-r3_seqnum = 8.
        WHEN 'Invoice->IssueTime'.
          <nodemap>-r3_seqnum = 9.
        WHEN 'Invoice->InvoiceTypeCode'.
          <nodemap>-r3_seqnum = 10.
        WHEN 'Invoice->Note'.
          <nodemap>-r3_seqnum = 11.
        WHEN 'Invoice->DocumentCurrencyCode'.
          <nodemap>-r3_seqnum = 12.
        WHEN 'Invoice->TaxCurrencyCode'.
          <nodemap>-r3_seqnum = 13.
        WHEN 'Invoice->PricingCurrencyCode'.
          <nodemap>-r3_seqnum = 14.
        WHEN 'Invoice->PaymentCurrencyCode'.
          <nodemap>-r3_seqnum = 15.
        WHEN 'Invoice->PaymentAlternativeCurrencyCode'.
          <nodemap>-r3_seqnum = 16.
        WHEN 'Invoice->AccountingCost'.
          <nodemap>-r3_seqnum = 17.
        WHEN 'Invoice->LineCountNumeric'.
          <nodemap>-r3_seqnum = 18.
        WHEN 'Invoice->InvoicePeriod'.
          <nodemap>-r3_seqnum = 19.
        WHEN 'Invoice->OrderReference'.
          <nodemap>-r3_seqnum = 20.
        WHEN 'Invoice->BillingReference'.
          <nodemap>-r3_seqnum = 21.
        WHEN 'Invoice->DespatchDocumentReference'.
          <nodemap>-r3_seqnum = 22.
        WHEN 'Invoice->ReceiptDocumentReference'.
          <nodemap>-r3_seqnum = 23.
        WHEN 'Invoice->OriginatorDocumentReference'.
          <nodemap>-r3_seqnum = 24.
        WHEN 'Invoice->ContractDocumentReference'.
          <nodemap>-r3_seqnum = 25.
        WHEN 'Invoice->AdditionalDocumentReference'.
          <nodemap>-r3_seqnum = 26.
        WHEN 'Invoice->Signature'.
          <nodemap>-r3_seqnum = 27.
        WHEN 'Invoice->AccountingSupplierParty'.
          <nodemap>-r3_seqnum = 28.
        WHEN 'Invoice->AccountingCustomerParty'.
          <nodemap>-r3_seqnum = 29.
        WHEN 'Invoice->BuyerCustomerParty'.
          <nodemap>-r3_seqnum = 30.
        WHEN 'Invoice->SellerSupplierParty'.
          <nodemap>-r3_seqnum = 31.
        WHEN 'Invoice->TaxRepresentativeParty'.
          <nodemap>-r3_seqnum = 32.
        WHEN 'Invoice->Delivery'.
          <nodemap>-r3_seqnum = 33.
        WHEN 'Invoice->PaymentMeans'.
          <nodemap>-r3_seqnum = 34.
        WHEN 'Invoice->PaymentTerms'.
          <nodemap>-r3_seqnum = 35.
        WHEN 'Invoice->AllowanceCharge'.
          <nodemap>-r3_seqnum = 36.
        WHEN 'Invoice->TaxExchangeRate'.
          <nodemap>-r3_seqnum = 37.
        WHEN 'Invoice->PricingExchangeRate'.
          <nodemap>-r3_seqnum = 38.
        WHEN 'Invoice->PaymentExchangeRate'.
          <nodemap>-r3_seqnum = 39.
        WHEN 'Invoice->PaymentAlternativeExchangeRate'.
          <nodemap>-r3_seqnum = 40.
        WHEN 'Invoice->TaxTotal'.
          <nodemap>-r3_seqnum = 41.
        WHEN 'Invoice->WithholdingTaxTotal'.
          <nodemap>-r3_seqnum = 42.
        WHEN 'Invoice->LegalMonetaryTotal'.
          <nodemap>-r3_seqnum = 43.
        WHEN 'Invoice->InvoiceLine'.
          <nodemap>-r3_seqnum = 44.
        WHEN 'Invoice->AccountingSupplierParty->Party->Contact' OR
             'Invoice->AccountingCustomerParty->Party->Contact' OR
             'Invoice->BuyerCustomerParty->Party->Contact' OR
             'Invoice->SellerSupplierParty->Party->Contact' OR
             'Invoice->TaxRepresentativeParty->Party->Contact'.
          <nodemap>-r3_seqnum = 8.
* InvoiceLine
        WHEN 'Invoice->InvoiceLine->ID'.
          <nodemap>-r3_seqnum = 1.
        WHEN 'Invoice->InvoiceLine->Note'.
          <nodemap>-r3_seqnum = 2.
        WHEN 'Invoice->InvoiceLine->InvoicedQuantity'.
          <nodemap>-r3_seqnum = 3.
        WHEN 'Invoice->InvoiceLine->LineExtensionAmount'.
          <nodemap>-r3_seqnum = 4.
        WHEN 'Invoice->InvoiceLine->OrderLineReference'.
          <nodemap>-r3_seqnum = 5.
        WHEN 'Invoice->InvoiceLine->DespatchLineReference'.
          <nodemap>-r3_seqnum = 6.
        WHEN 'Invoice->InvoiceLine->ReceiptLineReference'.
          <nodemap>-r3_seqnum = 7.
        WHEN 'Invoice->InvoiceLine->Delivery'.
          <nodemap>-r3_seqnum = 8.
        WHEN 'Invoice->InvoiceLine->AllowanceCharge'.
          <nodemap>-r3_seqnum = 9.
        WHEN 'Invoice->InvoiceLine->TaxTotal'.
          <nodemap>-r3_seqnum = 10.
        WHEN 'Invoice->InvoiceLine->WithholdingTaxTotal'.
          <nodemap>-r3_seqnum = 11.
        WHEN 'Invoice->InvoiceLine->Item'.
          <nodemap>-r3_seqnum = 12.
        WHEN 'Invoice->InvoiceLine->Price'.
          <nodemap>-r3_seqnum = 13.
      ENDCASE.
    ENDLOOP.

    SORT me->mt_nodemap BY nestcnt ASCENDING
                           r3_seqnum ASCENDING.

  ENDMETHOD.


  METHOD get_prefix.
    DATA: lv_ifrname TYPE string.

    FIELD-SYMBOLS: <tadir_v>  LIKE LINE OF me->mt_tadir_v,
                   <flattab>  TYPE zotct_s0001,
                   <sproxdat> LIKE LINE OF me->mt_sproxdat.

    READ TABLE me->mt_tadir_v WITH KEY ifr_name = xmlkey ASSIGNING <tadir_v>.
    IF sy-subrc EQ 0.
      prefix = <tadir_v>-ifr_nspce.
    ELSE.
      CLEAR: lv_ifrname.
      CONCATENATE xmlkey 'Type' INTO lv_ifrname.
      READ TABLE me->mt_tadir_v WITH KEY ifr_name = lv_ifrname ASSIGNING <tadir_v>.
      IF sy-subrc EQ 0.
        prefix = <tadir_v>-ifr_nspce.
      ELSE.
        READ TABLE me->mt_flattab WITH KEY xmlkey = xmlkey ASSIGNING <flattab>.
        IF sy-subrc EQ 0.
          lv_ifrname = <flattab>-abap_name.
        ENDIF.

        READ TABLE me->mt_tadir_v WITH KEY obj_name = lv_ifrname ASSIGNING <tadir_v>.
        IF sy-subrc EQ 0.
          prefix = <tadir_v>-ifr_nspce.
        ENDIF.
      ENDIF.
    ENDIF.

    IF xmlkey EQ 'ActualPackage'.
      READ TABLE me->mt_sproxdat WITH KEY ifr_name = xmlkey ASSIGNING <sproxdat>.
      IF sy-subrc EQ 0.
        lv_ifrname = <sproxdat>-obj_name.
      ENDIF.
      READ TABLE me->mt_tadir_v WITH KEY obj_name = lv_ifrname ASSIGNING <tadir_v>.
      IF sy-subrc EQ 0.
        prefix = <tadir_v>-ifr_nspce.
      ENDIF.
    ENDIF.

    CASE prefix.
      WHEN mc_xmlns.
        prefix = 'xmlns'.
      WHEN mc_cac.
        prefix = 'cac'.
      WHEN mc_xades.
        prefix = 'xades'.
      WHEN mc_udt.
        prefix = 'udt'.
      WHEN mc_cbc.
        prefix = 'cbc'.
      WHEN mc_ccts.
        prefix = 'ccts'.
      WHEN mc_ubltr.
        prefix = 'ubltr'.
      WHEN mc_qdt.
        prefix = 'qdt'.
      WHEN mc_ext.
        prefix = 'ext'.
      WHEN mc_ds.
        prefix = 'ds'.
      WHEN mc_xsi.
        prefix = 'xsi'.
      WHEN mc_schemalocation.
        prefix = 'schemaLocation'.
      WHEN mc_ns8.
        prefix = 'ns8'.
      WHEN mc_docname.
        prefix = 'docname'.
    ENDCASE.
  ENDMETHOD.


  METHOD get_xmlstr.
    DATA: lv_encoding TYPE string.

    me->set_namespaces( ).
    CLEAR: lv_encoding.
    CONCATENATE '<?xml version="1.0" encoding="utf-8"?>' cl_abap_char_utilities=>newline INTO lv_encoding.

    REPLACE ALL OCCURRENCES OF '<?xml version="1.0" encoding="utf-16"?>' IN me->mv_xmlstr WITH lv_encoding.

    xmlstr = me->mv_xmlstr.
  ENDMETHOD.


  METHOD set_namespaces.
*** set namespaces redefinition
    DATA: lcl_iterator TYPE REF TO if_ixml_node_iterator,
          lv_name      TYPE string,
          lcl_element  TYPE REF TO if_ixml_element,
          lcl_node     TYPE REF TO if_ixml_node.

    lcl_iterator = me->mo_document->create_iterator( ).
    lcl_node = lcl_iterator->get_next( ).

    WHILE lcl_node IS NOT INITIAL.

      CASE lcl_node->get_type( ).

        WHEN if_ixml_node=>co_node_element.
          lcl_element ?= lcl_node.

          lv_name = lcl_element->get_name( ).
          IF lv_name EQ me->mc_docname.
            lcl_element->set_attribute( name = 'xmlns' value = mc_xmlns ).
            lcl_element->set_attribute( name = 'xmlns:cac' value = mc_cac ).
            lcl_element->set_attribute( name = 'xmlns:cbc' value = mc_cbc ).
            lcl_element->set_attribute( name = 'xmlns:ds' value = mc_ds ).
            lcl_element->set_attribute( name = 'xmlns:ext' value = mc_ext ).
            lcl_element->set_attribute( name = 'xmlns:ns8' value = mc_ns8 ).
            lcl_element->set_attribute( name = 'xmlns:xades' value = mc_xades ).
            lcl_element->set_attribute( name = 'xmlns:xsi' value = mc_xsi ).
            lcl_element->set_attribute( name = 'xsi:schemaLocation' value = mc_schemalocation ).
            lcl_element->set_attribute( name = 'xmlns:ccts' value = mc_ccts ).
            lcl_element->set_attribute( name = 'xmlns:ubltr' value = mc_ubltr ).
            lcl_element->set_attribute( name = 'xmlns:qdt' value = mc_qdt ).
            lcl_element->set_attribute( name = 'xmlns:udt' value = mc_udt ).

            EXIT.
          ENDIF.
      ENDCASE.

      lcl_node = lcl_iterator->get_next( ).
    ENDWHILE.

    CLEAR me->mv_xmlstr.

    me->mo_ixml->create_renderer( document = me->mo_document
                                ostream  = me->mo_ixml->create_stream_factory(
                                )->create_ostream_cstring( string = mv_xmlstr
                                ) )->render( ).
  ENDMETHOD.
ENDCLASS.
