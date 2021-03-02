CLASS zotct_cl_tin DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_tin,
        tin TYPE string,
      END OF ty_tin .
    TYPES:
      BEGIN OF ty_result,
        tin   TYPE string,
        valid TYPE abap_bool,
      END OF ty_result .

    CLASS-METHODS create_dummy_tin
      IMPORTING
        !locale   TYPE land1
        !count    TYPE sytabix OPTIONAL
        !type     TYPE char4 DEFAULT 'TCKN'
      EXPORTING
        !tin_list TYPE zotct_tt0003
        !tin      TYPE string .
    CLASS-METHODS validate_tin
      IMPORTING
        !locale      TYPE land1
        !tin_list    TYPE zotct_tt0003 OPTIONAL
        !tin         TYPE string OPTIONAL
        !type        TYPE char4 DEFAULT 'TCKN'
      EXPORTING
        !result_list TYPE zotct_tt0004
        !valid       TYPE abap_bool .
  PROTECTED SECTION.

    DATA mt_tin TYPE zotct_tt0003 .
    DATA mt_result TYPE zotct_tt0004 .
  PRIVATE SECTION.

    CLASS-METHODS cre_dummy_tin_tr
      IMPORTING
        !count    TYPE sytabix
        !type     TYPE char4
      EXPORTING
        !tin_list TYPE zotct_tt0003
        !tin      TYPE string .
    CLASS-METHODS validate_tin_tr
      IMPORTING
        !locale      TYPE land1
        !tin_list    TYPE zotct_tt0003 OPTIONAL
        !tin         TYPE string OPTIONAL
        !type        TYPE char4
      EXPORTING
        !result_list TYPE zotct_tt0004
        !valid       TYPE abap_bool .
ENDCLASS.



CLASS ZOTCT_CL_TIN IMPLEMENTATION.


  METHOD create_dummy_tin.
    CASE locale.
      WHEN 'TR'.
        CALL METHOD cre_dummy_tin_tr
          EXPORTING
            count    = count
            type     = type
          IMPORTING
            tin_list = tin_list
            tin      = tin.
      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.


  METHOD cre_dummy_tin_tr.

    DATA: lv_ran_int   TYPE qfranint,
          ls_tin       TYPE ty_tin,
          lv_tckn_root TYPE c LENGTH 9,
          lv_even_sum  TYPE p,
          lv_odd_sum   TYPE p,
          lv_mod1      TYPE p,
          lv_mod2      TYPE p,
          lv_dec10     TYPE c LENGTH 1,
          lv_dec11     TYPE c LENGTH 1.

    DO count TIMES.

      IF type EQ 'VKN'.
*       Create a 10-digit number
        CALL FUNCTION 'QF05_RANDOM_INTEGER'
          EXPORTING
            ran_int_max   = 9999999999
            ran_int_min   = 1
          IMPORTING
            ran_int       = lv_ran_int
          EXCEPTIONS
            invalid_input = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
*         Implement suitable error handling here
        ELSE.
          CLEAR ls_tin.
          ls_tin-tin = lv_ran_int.
          APPEND ls_tin TO tin_list.
          tin = lv_ran_int.
        ENDIF.
      ELSEIF type EQ 'TCKN'.

*     Create a 9-digit number
        CALL FUNCTION 'QF05_RANDOM_INTEGER'
          EXPORTING
            ran_int_max   = 999999999
            ran_int_min   = 100000000 "first decimal must not be zero
          IMPORTING
            ran_int       = lv_ran_int
          EXCEPTIONS
            invalid_input = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
*       Implement suitable error handling here
        ELSE.
          CLEAR lv_tckn_root.

          lv_tckn_root = lv_ran_int.

          lv_even_sum = lv_tckn_root+1(1) +
                        lv_tckn_root+3(1) +
                        lv_tckn_root+5(1) +
                        lv_tckn_root+7(1).

          lv_odd_sum  = lv_tckn_root(1) +
                        lv_tckn_root+2(1) +
                        lv_tckn_root+4(1) +
                        lv_tckn_root+6(1) +
                        lv_tckn_root+8(1).

          lv_mod1 = ( lv_odd_sum * 7 - lv_even_sum ) MOD 10.
          lv_mod2 = ( lv_odd_sum + lv_even_sum + lv_mod1 ) MOD 10.

          lv_dec10 = lv_mod1.
          lv_dec11 = lv_mod2.

          CLEAR ls_tin.
          CONCATENATE lv_tckn_root lv_dec10 lv_dec11 INTO ls_tin-tin.
          APPEND ls_tin TO tin_list.
          tin = ls_tin-tin.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD validate_tin.
    CASE locale.
      WHEN 'TR'.
        CALL METHOD validate_tin_tr
          EXPORTING
            locale      = locale
            tin_list    = tin_list
            tin         = tin
            type        = type
          IMPORTING
            result_list = result_list
            valid       = valid.

      WHEN OTHERS.

    ENDCASE.
  ENDMETHOD.


  METHOD validate_tin_tr.

    DATA: ls_list        TYPE ty_tin,
          ls_result_list TYPE ty_result,
          lv_tckn_root   TYPE c LENGTH 9,
          lv_tckn        TYPE c LENGTH 11,
          lv_even_sum    TYPE p,
          lv_odd_sum     TYPE p,
          lv_mod1        TYPE p,
          lv_mod2        TYPE p,
          lv_dec10       TYPE c LENGTH 1,
          lv_dec11       TYPE c LENGTH 1,
          lv_strlen      TYPE p.

    LOOP AT tin_list INTO ls_list.
      CLEAR ls_result_list.

      CLEAR lv_strlen.

      lv_strlen = strlen( ls_list-tin ).

      IF ls_list-tin IS INITIAL OR
         lv_strlen LT 10 OR
         lv_strlen GT 11.
        ls_result_list-tin = ls_list-tin.
        ls_result_list-valid = abap_false.
        APPEND ls_result_list TO result_list.

        CONTINUE.
      ENDIF.

      IF type = 'VKN'.
        ls_result_list-tin = ls_list-tin.
        ls_result_list-valid = abap_true.

      ELSEIF type = 'TCKN'.
        IF ls_list-tin(1) EQ '0'.
          ls_result_list-tin = ls_list-tin.
          ls_result_list-valid = abap_false.
          APPEND ls_result_list TO result_list.

          CONTINUE.
        ENDIF.

        CLEAR: lv_tckn_root,
               lv_even_sum,
               lv_odd_sum,
               lv_mod1,
               lv_mod2,
               lv_dec10,
               lv_dec11.

        lv_tckn = ls_list-tin.

        lv_even_sum = lv_tckn+1(1) +
                      lv_tckn+3(1) +
                      lv_tckn+5(1) +
                      lv_tckn+7(1).

        lv_odd_sum  = lv_tckn(1) +
                      lv_tckn+2(1) +
                      lv_tckn+4(1) +
                      lv_tckn+6(1) +
                      lv_tckn+8(1).

        lv_mod1 = ( lv_odd_sum * 7 - lv_even_sum ) MOD 10.
        lv_mod2 = ( lv_odd_sum + lv_even_sum + lv_mod1 ) MOD 10.

        IF lv_tckn+9(1) EQ lv_mod1 AND
           lv_tckn+10(1) EQ lv_mod2.
          ls_result_list-tin = ls_list-tin.
          ls_result_list-valid = abap_true.
        ELSE.
          ls_result_list-tin = ls_list-tin.
          ls_result_list-valid = abap_false.
        ENDIF.
      ELSE.
        ls_result_list-tin = ls_list-tin.
        ls_result_list-valid = abap_false.
      ENDIF.

      APPEND ls_result_list TO result_list.
    ENDLOOP.

    valid = abap_true.

    LOOP AT result_list INTO ls_result_list WHERE valid IS INITIAL.
      valid = abap_false.
      EXIT.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
