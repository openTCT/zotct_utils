class ZOTCT_UBL_F definition
  public
  final
  create public .

public section.

  interfaces ZOTCT_IF_UBL .

  methods CONSTRUCTOR
    importing
      !LOCALE type ZOTCT_DE0001
      !PRODUCT type ZOTCT_DE0002 .
  methods GET_UBL
    returning
      value(UBL) type ref to OBJECT .
protected section.
private section.

  data MO_UBL type ref to OBJECT .
ENDCLASS.



CLASS ZOTCT_UBL_F IMPLEMENTATION.


  METHOD constructor.

    DATA: lo_ef TYPE REF TO zotcttr_cl_ef,
          lo_ea TYPE REF TO zotcttr_cl_ea,
          lo_ei TYPE REF TO zotcttr_cl_ei.

    IF locale EQ 'TR'.
      CASE product.
        WHEN 'EF'.
          CREATE OBJECT lo_ef.
          mo_ubl ?= lo_ef.
        WHEN 'EA'.
          CREATE OBJECT lo_ea.
          mo_ubl ?= lo_ea.
        WHEN 'EI'.
          CREATE OBJECT lo_ei.
          mo_ubl ?= lo_ei.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD get_ubl.
    ubl = me->mo_ubl.
  ENDMETHOD.
ENDCLASS.
