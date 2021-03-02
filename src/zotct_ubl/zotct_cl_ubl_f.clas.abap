CLASS zotct_cl_ubl_f DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zotct_if_ubl .

    METHODS constructor
      IMPORTING
        !locale  TYPE zotct_de0001
        !product TYPE zotct_de0002 .
    METHODS get_ubl
      RETURNING
        VALUE(ubl) TYPE REF TO object .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_ubl TYPE REF TO object .
ENDCLASS.



CLASS ZOTCT_CL_UBL_F IMPLEMENTATION.


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
