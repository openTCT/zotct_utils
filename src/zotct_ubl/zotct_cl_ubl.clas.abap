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

    DATA : gt_split     TYPE TABLE OF string,
           lc_itab_ext  TYPE REF TO zotct_cl_itab_ext,
           gt_flattab_r TYPE zotct_tt0001.

    FIELD-SYMBOLS : <ubl>       TYPE any,
                    <flattab_r> TYPE zotct_s0001,
                    <flattab_n> TYPE zotct_s0001,
                    <tabl>      LIKE LINE OF me->gt_tabl,
                    <ttyp>      LIKE LINE OF me->gt_ttyp,

                    <kunduz>    TYPE any,

                    <sincap>    TYPE ANY TABLE,

                    <fare>      TYPE any,

                    <yarasa>    TYPE any,

                    <maymun>    TYPE any,

                    <aslan>     TYPE any,

                    <kanguru>   TYPE ANY TABLE,

                    <kiwi>      TYPE any,

                    <beetle>    TYPE any
                    .

    ASSIGN gs_ubl->* TO <ubl> .

    gt_flattab_r[] = me->gt_flattab[] .

    CALL METHOD zotct_cl_itab_ext=>reverse
      CHANGING
        ct_table = gt_flattab_r.

*** Map ABAP Name

    LOOP AT gt_flattab_r ASSIGNING <flattab_r>.
      READ TABLE me->gt_tabl WITH KEY xml_name = <flattab_r>-xmlkey ASSIGNING <tabl>.
      IF sy-subrc EQ 0.
        <flattab_r>-abap_name = <tabl>-abap_name .
        <flattab_r>-r3_name   = <tabl>-r3_name .
        <flattab_r>-r3_objtyp = <tabl>-r3_objtyp .
      ELSE.
        READ TABLE me->gt_ttyp WITH KEY xml_name = <flattab_r>-xmlkey ASSIGNING <ttyp>.
        IF sy-subrc EQ 0.
          <flattab_r>-abap_name = <ttyp>-abap_name .
          <flattab_r>-r3_name   = <ttyp>-r3_name .
          <flattab_r>-r3_objtyp = <ttyp>-r3_objtyp .
        ENDIF.
      ENDIF.
    ENDLOOP.

    DATA : gt_flattab_n TYPE zotct_tt0001 .

    CLEAR gt_flattab_n[] .

    gt_flattab_n[] = gt_flattab_r[] .

    CALL METHOD zotct_cl_itab_ext=>reverse
      CHANGING
        ct_table = gt_flattab_n.

    DATA : gt_tavsan TYPE REF TO data .

    LOOP AT gt_flattab_n ASSIGNING <flattab_n>.

      IF <flattab_n>-r3_objtyp EQ 'TABL' .

        IF <fare> IS NOT ASSIGNED .
          IF <kunduz> IS NOT ASSIGNED .
            ASSIGN COMPONENT <flattab_n>-r3_name OF STRUCTURE <ubl> TO <kunduz> .
          ELSE.
            ASSIGN COMPONENT <flattab_n>-r3_name OF STRUCTURE <kunduz> TO <kunduz> .
          ENDIF.

          IF <flattab_n>-xmlval IS NOT INITIAL .
            ASSIGN COMPONENT 'BASE' OF STRUCTURE <kunduz> TO <kunduz>.
            ASSIGN COMPONENT 'BASE' OF STRUCTURE <kunduz> TO <kunduz>.
            ASSIGN COMPONENT 'CONTENT' OF STRUCTURE <kunduz> TO <kunduz>.

            <kunduz> = <flattab_n>-xmlval .
          ENDIF.
        ELSE .
          LOOP AT <sincap> ASSIGNING <maymun> .
            ASSIGN COMPONENT <flattab_n>-r3_name OF STRUCTURE <maymun> TO <aslan> .

            ASSIGN COMPONENT 'BASE' OF STRUCTURE <aslan> TO <fare>.
            ASSIGN COMPONENT 'BASE' OF STRUCTURE <fare> TO <fare>.
            ASSIGN COMPONENT 'CONTENT' OF STRUCTURE <fare> TO <fare>.
            <fare> = <flattab_n>-xmlval .
          ENDLOOP.

          IF <yarasa> IS INITIAL.
            <yarasa> = <sincap> .
          ELSE.
            ASSIGN <yarasa> TO <kanguru> .
            LOOP AT <kanguru> ASSIGNING <kiwi>.
              ASSIGN COMPONENT <flattab_n>-r3_name OF STRUCTURE <kiwi> TO <beetle> .

              <beetle> = <aslan> .
            ENDLOOP.
            <yarasa> = <kanguru> .
          ENDIF.
        ENDIF.
      ELSEIF <flattab_n>-r3_objtyp EQ 'TTYP' .
        IF gt_tavsan IS INITIAL.
          CREATE DATA gt_tavsan TYPE (<flattab_n>-abap_name) .
        ENDIF.

        ASSIGN gt_tavsan->* TO <sincap> .
        INSERT INITIAL LINE INTO TABLE <sincap> ASSIGNING <fare>.

        ASSIGN COMPONENT <flattab_n>-r3_name OF STRUCTURE <kunduz> TO <yarasa> .
      ENDIF.
    ENDLOOP.
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
            lv_cnt ,
            lv_reptxt .

      SPLIT <ubltab>-xmlkey AT '->' INTO TABLE gt_split1 .

      DESCRIBE TABLE gt_split1 LINES lv_splits .

      CLEAR me->gt_flattab[] .

      LOOP AT gt_split1 ASSIGNING <split>.
        lv_cnt = lv_cnt + 1 .
        APPEND INITIAL LINE TO me->gt_flattab ASSIGNING <flattab> .

        IF <split> CA '[]'.
          SPLIT <split> AT '[' INTO TABLE gt_split2 .

          READ TABLE gt_split2 ASSIGNING <split2> INDEX 2 .
          IF sy-subrc EQ 0.
            REPLACE ALL OCCURRENCES OF ']' IN <split2> WITH space .

            <flattab>-index = <split2> .

            CONCATENATE '[' <split2> ']' INTO lv_reptxt .

            REPLACE ALL OCCURRENCES OF lv_reptxt IN <split> WITH space .

          ENDIF.

        ELSE.
          <flattab>-index = 0 .
        ENDIF.

        <flattab>-xmlkey = <split> .

        <flattab>-attrib = <ubltab>-attrib .

        IF lv_cnt EQ lv_splits .
          <flattab>-xmlval = <ubltab>-xmlval .
        ENDIF.

      ENDLOOP.

      me->nest( ) .
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
