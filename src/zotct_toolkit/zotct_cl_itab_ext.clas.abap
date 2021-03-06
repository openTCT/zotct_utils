class ZOTCT_CL_ITAB_EXT definition
  public
  final
  create public .

public section.

  class-methods SHUFFLE
    changing
      !TABLE type STANDARD TABLE .
  class-methods REVERSE
    changing
      !TABLE type STANDARD TABLE .
  class-methods MAX
    importing
      !COLNAME type STRING
      !TABLE type ANY TABLE
    returning
      value(VAL) type STRING
    raising
      ZCX_OTCT .
  class-methods MIN
    importing
      !COLNAME type STRING
      !TABLE type ANY TABLE
    returning
      value(VAL) type STRING .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZOTCT_CL_ITAB_EXT IMPLEMENTATION.


  METHOD max.
    FIELD-SYMBOLS: <colname>         TYPE string,
                   <value>           TYPE any,
                   <restab_standard> TYPE STANDARD TABLE,
                   <restab_index>    TYPE SORTED TABLE,
                   <restab_hashed>   TYPE HASHED TABLE,
                   <res>             TYPE any.

    DATA: lcl_ref TYPE REF TO data.
    DATA: knd TYPE c LENGTH 1.

    IF table IS INITIAL OR
       colname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_otct
        EXPORTING
          textid = zcx_otct=>input_initial.
    ENDIF.

    DESCRIBE TABLE table KIND knd.
    CREATE DATA: lcl_ref LIKE table.
    ASSIGN colname TO <colname>.

    CASE knd.
      WHEN sydes_kind-standard.

        ASSIGN: lcl_ref->* TO <restab_standard>.
        IF <restab_standard> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        <restab_standard> = table.

        SORT <restab_standard> BY (<colname>) DESCENDING.

        LOOP AT <restab_standard> ASSIGNING <res>.
          ASSIGN COMPONENT <colname> OF STRUCTURE <res> TO <value>.
          IF <value> IS ASSIGNED.
            val = <value>.
          ENDIF.
          EXIT.
        ENDLOOP.

      WHEN sydes_kind-sorted.

        ASSIGN: lcl_ref->* TO <restab_index>.
        IF <restab_index> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        <restab_index> = table.

        LOOP AT <restab_index> ASSIGNING <res>.
          UNASSIGN <value>.
          ASSIGN COMPONENT <colname> OF STRUCTURE <res> TO <value>.
          IF <value> IS ASSIGNED.
            IF <value> > val.
              val = <value>.
            ENDIF.
          ENDIF.
        ENDLOOP.

      WHEN sydes_kind-hashed.

        ASSIGN: lcl_ref->* TO <restab_hashed>.
        IF <restab_hashed> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        <restab_hashed> = table.

        LOOP AT <restab_hashed> ASSIGNING <res>.
          UNASSIGN <value>.
          ASSIGN COMPONENT <colname> OF STRUCTURE <res> TO <value>.
          IF <value> IS ASSIGNED.
            IF <value> > val.
              val = <value>.
            ENDIF.
          ENDIF.
        ENDLOOP.
    ENDCASE.

  ENDMETHOD.


  METHOD min.
    FIELD-SYMBOLS: <colname>         TYPE string,
                   <value>           TYPE any,
                   <restab_standard> TYPE STANDARD TABLE,
                   <restab_index>    TYPE SORTED TABLE,
                   <restab_hashed>   TYPE HASHED TABLE,
                   <res>             TYPE any.

    DATA: lcl_ref TYPE REF TO data.
    DATA: knd TYPE c LENGTH 1.

    DESCRIBE TABLE table KIND knd.
    CREATE DATA: lcl_ref LIKE table.
    ASSIGN colname TO <colname>.

    IF table IS INITIAL OR
       colname IS INITIAL.
      RAISE EXCEPTION TYPE zcx_otct
        EXPORTING
          textid = zcx_otct=>input_initial.
    ENDIF.

    CASE knd.
      WHEN sydes_kind-standard.

        ASSIGN: lcl_ref->* TO <restab_standard>.
        IF <restab_standard> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        <restab_standard> = table.

        SORT <restab_standard> BY (<colname>) ASCENDING.

        LOOP AT <restab_standard> ASSIGNING <res>.
          ASSIGN COMPONENT <colname> OF STRUCTURE <res> TO <value>.
          IF <value> IS ASSIGNED.
            val = <value>.
          ENDIF.
          EXIT.
        ENDLOOP.

      WHEN sydes_kind-sorted.

        ASSIGN: lcl_ref->* TO <restab_index>.
        IF <restab_index> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        <restab_index> = table.

        LOOP AT <restab_index> ASSIGNING <res>.
          UNASSIGN <value>.
          ASSIGN COMPONENT <colname> OF STRUCTURE <res> TO <value>.
          IF <value> IS ASSIGNED.
            IF <value> < val OR val IS INITIAL.
              val = <value>.
            ENDIF.
          ENDIF.
        ENDLOOP.

      WHEN sydes_kind-hashed.

        ASSIGN: lcl_ref->* TO <restab_hashed>.
        IF <restab_hashed> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        <restab_hashed> = table.

        LOOP AT <restab_hashed> ASSIGNING <res>.
          UNASSIGN <value>.
          ASSIGN COMPONENT <colname> OF STRUCTURE <res> TO <value>.
          IF <value> IS ASSIGNED.
            IF <value> < val OR val IS INITIAL.
              val = <value>.
            ENDIF.
          ENDIF.
        ENDLOOP.
    ENDCASE.

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
          l_ref         TYPE REF TO data,
          lv_lines      TYPE qfranint.

    FIELD-SYMBOLS: <f_target_tab>     TYPE ANY TABLE,
                   <f_target_struct>  TYPE any,
                   <f_target_struct2> TYPE any,
                   <f_field>          TYPE any.

    IF table IS INITIAL.
      RAISE EXCEPTION TYPE zcx_otct
        EXPORTING
          textid = zcx_otct=>input_initial.
    ENDIF.

    CREATE DATA: l_ref LIKE table.

    ASSIGN l_ref->* TO <f_target_tab>.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_new_tab   ?= cl_abap_tabledescr=>describe_by_data_ref( l_ref ).
        l_datadescr = lo_new_tab->get_table_line_type( ).
        l_structure ?= l_datadescr.
      CATCH cx_root.
        RETURN.
    ENDTRY.

    l_comp_tab = l_structure->get_components( ).

    l_comp-name = 'SEQNR'.
    l_comp-type = cl_abap_elemdescr=>get_int8( ).
    APPEND l_comp TO l_comp_tab.
    CLEAR l_comp.

    l_structure2 = cl_abap_structdescr=>create( l_comp_tab ).
    lo_new_tab  = cl_abap_tabledescr=>create( p_line_type = l_structure2 ).

    CREATE DATA l_new_table    TYPE HANDLE lo_new_tab.
    ASSIGN l_new_table->* TO <f_target_tab>.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CREATE DATA l_new_struct2 TYPE HANDLE l_structure2.
    ASSIGN l_new_struct2->* TO <f_target_struct2>.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CREATE DATA l_new_struct TYPE HANDLE l_structure.
    ASSIGN l_new_struct->* TO <f_target_struct>.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

** Fill in the SEQNR field with random integers

    CLEAR lv_lines.

    DESCRIBE TABLE table LINES lv_lines.

    LOOP AT table ASSIGNING <f_target_struct>.
      ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <f_target_struct2> TO <f_field>.
      CHECK sy-subrc IS INITIAL.

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
          l_ref         TYPE REF TO data,
          lv_lines      TYPE qfranint,
          lv_ran_int    TYPE qfranint.

    FIELD-SYMBOLS: <f_target_tab>     TYPE ANY TABLE,
                   <f_target_struct>  TYPE any,
                   <f_target_struct2> TYPE any,
                   <f_field>          TYPE any.

    IF table IS INITIAL.
      RAISE EXCEPTION TYPE zcx_otct
        EXPORTING
          textid = zcx_otct=>input_initial.
    ENDIF.

    CREATE DATA: l_ref LIKE table.

    ASSIGN l_ref->* TO <f_target_tab>.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.


    TRY.
        lo_new_tab   ?= cl_abap_tabledescr=>describe_by_data_ref( l_ref ).
        l_datadescr = lo_new_tab->get_table_line_type( ).
        l_structure ?= l_datadescr.
      CATCH cx_root.
        RETURN.
    ENDTRY.
    l_comp_tab = l_structure->get_components( ).

    l_comp-name = 'SEQNR'.
    l_comp-type = cl_abap_elemdescr=>get_int8( ).
    APPEND l_comp TO l_comp_tab.
    CLEAR l_comp.

    l_structure2 = cl_abap_structdescr=>create( l_comp_tab ).
    lo_new_tab  = cl_abap_tabledescr=>create( p_line_type = l_structure2 ).

    CREATE DATA l_new_table    TYPE HANDLE lo_new_tab.
    ASSIGN l_new_table->* TO <f_target_tab>.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CREATE DATA l_new_struct2 TYPE HANDLE l_structure2.
    ASSIGN l_new_struct2->* TO <f_target_struct2>.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CREATE DATA l_new_struct TYPE HANDLE l_structure.
    ASSIGN l_new_struct->* TO <f_target_struct>.
    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

** Fill in the SEQNR field with random integers

    CLEAR lv_lines.

    DESCRIBE TABLE table LINES lv_lines.

    LOOP AT table ASSIGNING <f_target_struct>.
      ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <f_target_struct2> TO <f_field>.
      CHECK sy-subrc IS INITIAL.
      MOVE-CORRESPONDING <f_target_struct> TO <f_target_struct2>.
      CLEAR lv_lines.

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
        RETURN.
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
