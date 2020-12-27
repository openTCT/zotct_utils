class ZOTCT_CL_ITAB_EXT definition
  public
  final
  create public .

public section.

  class-methods SHUFFLE
    changing
      !CT_TABLE type STANDARD TABLE .
  class-methods REVERSE
    changing
      !CT_TABLE type STANDARD TABLE .
protected section.
private section.
ENDCLASS.



CLASS ZOTCT_CL_ITAB_EXT IMPLEMENTATION.


  METHOD REVERSE.
*** INSERT field to the internal table

    data : l_datadescr   type ref to cl_abap_datadescr,
           l_comp_tab    type cl_abap_structdescr=>component_table,
           l_comp        type cl_abap_structdescr=>component,
           lo_new_tab    type ref to cl_abap_tabledescr,
           l_structure   type ref to cl_abap_structdescr,
           l_structure2  type ref to cl_abap_structdescr,
           l_new_table   type ref to data,
           l_new_struct  type ref to data,
           l_new_struct2 type ref to data,

           l_ref         type ref to data.

    FIELD-SYMBOLS : <f_target_tab>     TYPE ANY TABLE,
                    <f_target_struct>  TYPE any,
                    <f_target_struct2> TYPE any,
                    <f_field>          TYPE any.

    CREATE DATA : l_ref LIKE ct_table .

    ASSIGN l_ref->* TO <f_target_tab>.


    lo_new_tab   ?= cl_abap_tabledescr=>describe_by_data_ref( l_ref ).
    l_datadescr = lo_new_tab->get_table_line_type( ).

    l_structure ?= l_datadescr.
    l_comp_tab = l_structure->get_components( ).

    l_comp-name = 'SEQNR'.
*    l_comp-type = cl_abap_elemdescr=>get_c( p_length = '10' ).
    l_comp-type = cl_abap_elemdescr=>get_int8( ) .
    APPEND l_comp TO l_comp_tab. CLEAR l_comp.

    l_structure2 = cl_abap_structdescr=>create( l_comp_tab ).
    lo_new_tab  = cl_abap_tabledescr=>create( p_line_type = l_structure2 ).

    CREATE DATA l_new_table    TYPE HANDLE lo_new_tab.
    ASSIGN l_new_table->* TO <f_target_tab>.

    CREATE DATA l_new_struct2 TYPE HANDLE l_structure2 .
    ASSIGN l_new_struct2->* TO <f_target_struct2> .

    CREATE DATA l_new_struct TYPE HANDLE l_structure .
    ASSIGN l_new_struct->* TO <f_target_struct> .


** Fill in the SEQNR field with random integers
    DATA : lv_lines   TYPE qfranint,
           lv_ran_int TYPE qfranint.

    CLEAR lv_lines .

    DESCRIBE TABLE ct_table LINES lv_lines .

    LOOP AT ct_table ASSIGNING <f_target_struct>.
      ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <f_target_struct2> TO <f_field>.

      MOVE-CORRESPONDING <f_target_struct> TO <f_target_struct2> .

      <f_field> = sy-tabix .

      INSERT <f_target_struct2> INTO TABLE <f_target_tab> .

    ENDLOOP.

*** Sort the internal table and delete&modify the original one

    SORT <f_target_tab> BY ('SEQNR') DESCENDING .

    CLEAR ct_table[] .

    LOOP AT <f_target_tab> ASSIGNING <f_target_struct2>.
      APPEND INITIAL LINE TO ct_table ASSIGNING <f_target_struct> .
      MOVE-CORRESPONDING <f_target_struct2> TO <f_target_struct> .
    ENDLOOP.
  ENDMETHOD.


  METHOD SHUFFLE.

*** Insert field to the internal table

    DATA : l_datadescr   TYPE REF TO cl_abap_datadescr,
           l_comp_tab    TYPE cl_abap_structdescr=>component_table,
           l_comp        TYPE cl_abap_structdescr=>component,
           lo_new_tab    TYPE REF TO cl_abap_tabledescr,
           l_structure   TYPE REF TO cl_abap_structdescr,
           l_structure2  TYPE REF TO cl_abap_structdescr,
           l_new_table   TYPE REF TO data,
           l_new_struct  TYPE REF TO data,
           l_new_struct2 TYPE REF TO data,

           l_ref         TYPE REF TO data.

    FIELD-SYMBOLS : <f_target_tab>     TYPE ANY TABLE,
                    <f_target_struct>  TYPE any,
                    <f_target_struct2> TYPE any,
                    <f_field>          TYPE any.

    CREATE DATA : l_ref LIKE ct_table .

    ASSIGN l_ref->* TO <f_target_tab>.


    lo_new_tab   ?= cl_abap_tabledescr=>describe_by_data_ref( l_ref ).
    l_datadescr = lo_new_tab->get_table_line_type( ).

    l_structure ?= l_datadescr.
    l_comp_tab = l_structure->get_components( ).

    l_comp-name = 'SEQNR'.
*    l_comp-type = cl_abap_elemdescr=>get_c( p_length = '10' ).
    l_comp-type = cl_abap_elemdescr=>get_int8( ) .
    APPEND l_comp TO l_comp_tab. CLEAR l_comp.

    l_structure2 = cl_abap_structdescr=>create( l_comp_tab ).
    lo_new_tab  = cl_abap_tabledescr=>create( p_line_type = l_structure2 ).

    CREATE DATA l_new_table    TYPE HANDLE lo_new_tab.
    ASSIGN l_new_table->* TO <f_target_tab>.

    CREATE DATA l_new_struct2 TYPE HANDLE l_structure2 .
    ASSIGN l_new_struct2->* TO <f_target_struct2> .

    CREATE DATA l_new_struct TYPE HANDLE l_structure .
    ASSIGN l_new_struct->* TO <f_target_struct> .


** Fill in the SEQNR field with random integers
    DATA : lv_lines   TYPE qfranint,
           lv_ran_int TYPE qfranint.

    CLEAR lv_lines .

    DESCRIBE TABLE ct_table LINES lv_lines .

    LOOP AT ct_table ASSIGNING <f_target_struct>.
      ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <f_target_struct2> TO <f_field>.

      MOVE-CORRESPONDING <f_target_struct> TO <f_target_struct2> .

      CALL FUNCTION 'QF05_RANDOM_INTEGER'
        EXPORTING
          ran_int_max   = lv_lines
          ran_int_min   = 1
        IMPORTING
          ran_int       = lv_ran_int
        EXCEPTIONS
          invalid_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
        <f_field> = lv_ran_int .
      ENDIF.

      INSERT <f_target_struct2> INTO TABLE <f_target_tab> .

    ENDLOOP.

*** Sort the internal table and delete&modify the original one

    SORT <f_target_tab> BY ('SEQNR') ASCENDING .

    CLEAR ct_table[] .

    LOOP AT <f_target_tab> ASSIGNING <f_target_struct2>.
      APPEND INITIAL LINE TO ct_table ASSIGNING <f_target_struct> .
      MOVE-CORRESPONDING <f_target_struct2> TO <f_target_struct> .
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
