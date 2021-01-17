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

  methods SET_NAMESPACES .
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


  METHOD set_namespaces.


  ENDMETHOD.
ENDCLASS.
