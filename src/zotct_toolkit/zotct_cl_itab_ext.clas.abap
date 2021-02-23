CLASS zotct_cl_itab_ext DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS shuffle
      CHANGING
        !table TYPE STANDARD TABLE .
    CLASS-METHODS reverse
      CHANGING
        !table TYPE STANDARD TABLE .
    CLASS-METHODS max
      IMPORTING
        !colname   TYPE string
        !table     TYPE STANDARD TABLE
      RETURNING
        VALUE(val) TYPE string .
    CLASS-METHODS min
      IMPORTING
        !colname   TYPE string
        !table     TYPE STANDARD TABLE
      RETURNING
        VALUE(val) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZOTCT_CL_ITAB_EXT IMPLEMENTATION.


  METHOD max.
    FIELD-SYMBOLS: <colname> TYPE string,
                   <table>   TYPE any,
                   <value>   TYPE any,
                   <restab>  TYPE ANY TABLE,
                   <res>     TYPE any.

    DATA: lcl_ref TYPE REF TO data.

    CREATE DATA: lcl_ref LIKE table.
    ASSIGN: lcl_ref->* TO <restab>,
            colname TO <colname>.

    <restab> = table.

    SORT <restab> BY (<colname>) DESCENDING.

    LOOP AT <restab> ASSIGNING <res>.
      ASSIGN COMPONENT <colname> OF STRUCTURE <res> TO <value>.
      IF <value> IS ASSIGNED.
        val = <value>.
      ENDIF.
      EXIT.
    ENDLOOP.
  ENDMETHOD.


  METHOD min.
    FIELD-SYMBOLS: <colname> TYPE string,
                   <table>   TYPE any,
                   <value>   TYPE any,
                   <restab>  TYPE ANY TABLE,
                   <res>     TYPE any.

    DATA: lcl_ref TYPE REF TO data.

    CREATE DATA: lcl_ref LIKE table.
    ASSIGN: lcl_ref->* TO <restab>,
            colname TO <colname>.

    <restab> = table.

    SORT <restab> BY (<colname>) ASCENDING.

    LOOP AT <restab> ASSIGNING <res>.
      ASSIGN COMPONENT <colname> OF STRUCTURE <res> TO <value>.
      IF <value> IS ASSIGNED.
        val = <value>.
      ENDIF.
      EXIT.
    ENDLOOP.
  ENDMETHOD.


  METHOD reverse.
    DATA: l_datadescr   TYPE REF TO cl_abap_datadescr,
          l_comp_tab    TYPE cl_abap_structdescr=>component_table,
          l_comp        TYPE cl_abap_structdescr=>component,
          lo_new_tab    TYPE REF TO cl_abap_tabledescr,
          l_structure   TYPE REF TO cl_abap_structdescr,
          l_structure2  TYPE REF TO cl_abap_structdescr,
          l_new_table   TYPE REF TO data,
          l_new_struct  TYPE REF TO data,
          l_new_struct2 TYPE REF TO data,

          l_ref         TYPE REF TO data.

    FIELD-SYMBOLS: <f_target_tab>     TYPE ANY TABLE,
                   <f_target_struct>  TYPE any,
                   <f_target_struct2> TYPE any,
                   <f_field>          TYPE any.

    CREATE DATA: l_ref LIKE table.

    ASSIGN l_ref->* TO <f_target_tab>.

    TRY .
        lo_new_tab   ?= cl_abap_tabledescr=>describe_by_data_ref( l_ref ).
        l_datadescr = lo_new_tab->get_table_line_type( ).
        l_structure ?= l_datadescr.
      CATCH cx_root.
        EXIT.
    ENDTRY.

    l_comp_tab = l_structure->get_components( ).

    l_comp-name = 'SEQNR'.
    l_comp-type = cl_abap_elemdescr=>get_int8( ).
    APPEND l_comp TO l_comp_tab. CLEAR l_comp.

    l_structure2 = cl_abap_structdescr=>create( l_comp_tab ).
    lo_new_tab  = cl_abap_tabledescr=>create( p_line_type = l_structure2 ).

    CREATE DATA l_new_table    TYPE HANDLE lo_new_tab.
    ASSIGN l_new_table->* TO <f_target_tab>.

    CREATE DATA l_new_struct2 TYPE HANDLE l_structure2.
    ASSIGN l_new_struct2->* TO <f_target_struct2>.

    CREATE DATA l_new_struct TYPE HANDLE l_structure.
    ASSIGN l_new_struct->* TO <f_target_struct>.


** Fill in the SEQNR field with random integers
    DATA: lv_lines   TYPE qfranint,
          lv_ran_int TYPE qfranint.

    CLEAR lv_lines.

    DESCRIBE TABLE table LINES lv_lines.

    LOOP AT table ASSIGNING <f_target_struct>.
      ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <f_target_struct2> TO <f_field>.

      MOVE-CORRESPONDING <f_target_struct> TO <f_target_struct2>.

      <f_field> = sy-tabix.

      INSERT <f_target_struct2> INTO TABLE <f_target_tab>.

    ENDLOOP.

*** Sort the internal table and delete&modify the original one

    SORT <f_target_tab> BY ('SEQNR') DESCENDING.

    CLEAR table[].

    LOOP AT <f_target_tab> ASSIGNING <f_target_struct2>.
      APPEND INITIAL LINE TO table ASSIGNING <f_target_struct>.
      MOVE-CORRESPONDING <f_target_struct2> TO <f_target_struct>.
    ENDLOOP.
  ENDMETHOD.


  METHOD shuffle.

*** Insert field to the internal table

    DATA: l_datadescr   TYPE REF TO cl_abap_datadescr,
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

    CREATE DATA: l_ref LIKE table.

    ASSIGN l_ref->* TO <f_target_tab>.


    TRY .
        lo_new_tab   ?= cl_abap_tabledescr=>describe_by_data_ref( l_ref ).
        l_datadescr = lo_new_tab->get_table_line_type( ).
        l_structure ?= l_datadescr.
      CATCH cx_root.
        EXIT.
    ENDTRY.
    l_comp_tab = l_structure->get_components( ).

    l_comp-name = 'SEQNR'.
    l_comp-type = cl_abap_elemdescr=>get_int8( ).
    APPEND l_comp TO l_comp_tab. CLEAR l_comp.

    l_structure2 = cl_abap_structdescr=>create( l_comp_tab ).
    lo_new_tab  = cl_abap_tabledescr=>create( p_line_type = l_structure2 ).

    CREATE DATA l_new_table    TYPE HANDLE lo_new_tab.
    ASSIGN l_new_table->* TO <f_target_tab>.

    CREATE DATA l_new_struct2 TYPE HANDLE l_structure2.
    ASSIGN l_new_struct2->* TO <f_target_struct2>.

    CREATE DATA l_new_struct TYPE HANDLE l_structure.
    ASSIGN l_new_struct->* TO <f_target_struct>.


** Fill in the SEQNR field with random integers
    DATA: lv_lines   TYPE qfranint,
          lv_ran_int TYPE qfranint.

    CLEAR lv_lines.

    DESCRIBE TABLE table LINES lv_lines.

    LOOP AT table ASSIGNING <f_target_struct>.
      ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <f_target_struct2> TO <f_field>.

      MOVE-CORRESPONDING <f_target_struct> TO <f_target_struct2>.

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
        <f_field> = lv_ran_int.
      ENDIF.

      INSERT <f_target_struct2> INTO TABLE <f_target_tab>.

    ENDLOOP.

*** Sort the internal table and delete&modify the original one

    SORT <f_target_tab> BY ('SEQNR') ASCENDING.

    CLEAR table[].

    LOOP AT <f_target_tab> ASSIGNING <f_target_struct2>.
      APPEND INITIAL LINE TO table ASSIGNING <f_target_struct>.
      MOVE-CORRESPONDING <f_target_struct2> TO <f_target_struct>.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
