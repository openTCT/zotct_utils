Please note that this package is under active development and some functionalities may not been tested and work stable.

# ZOTCT_UTILS
ZOTCT_UTILS is a main utilities package which contains helper classes for openTCT products. There are two subpackages (ZOTCT_TOOLKIT, ZOTCT_UBL) in this main package.

## ZOTCT_TOOLKIT
ZOTCT_TOOLKIT is designed as a general toolkit package. It includes various tools that needed for development, deployment and testing of openTCT products.

 - ZOTCT_CL_DEPLOY: Helper class for deployment. Used for transpor request generation, upload/download, package deletion etc.
 - ZOTCT_CL_ITAB_EXT: Extended functionality such as reverse(), shuffle(), for built-in ABAP internal tables. 
 - ZOTCT_CL_TIN: Helpers for Tax Identification Number. Designed for TIN validation, dummy TIN generation etc. based on given locale.

## ZOTCT_UBL
ZOTCT_UBL is designed as a UBL engine which transforms xpath-like strings to XML and vice-versa.

 - ZOTCT_UBL_F: Factory class for UBL engine.
 - ZOTCT_CL_UBL: General UBL class.
 - ZOTCTTR_CL_EF: Locale and product specific class. ZOTCTTR_CL_EF is e-Fatura (e-Invoice) for Turkey locale in this example.

# Reference

## ZOTCT_CL_ITAB_EXT
Extra functionality to internal tables.

**Methods:**

 **- MAX():** Returns maximum value of the given column of table.
 
| Parameter| Type| Typing| Description
|--|--|--|--|
|IV_COLNAME| Importing |TYPE String|The column name whose maximum value is to be returned|
|IT_TABLE|Importing|TYPE Standard Table|The table to be returned maximum value of one of its column|
|R_VAL|Returning|TYPE String|Maximum value of the given table's column|

Usage example

    DATA: lv_seqnr_max TYPE string.
    
    CALL METHOD zotct_cl_itab_ext=>max  
      EXPORTING  
        iv_colname = 'SEQNR'  
        it_table = gt_flattab  
      RECEIVING  
        r_val = lv_seqnr_max.

 **- MIN():** Returns minimum value of the given column of table.
 
| Parameter| Type| Typing| Description
|--|--|--|--|
|IV_COLNAME| Importing |TYPE String|The column name whose minimum value is to be returned|
|IT_TABLE|Importing|TYPE Standard Table|The table to be returned minimum value of one of its column|
|R_VAL|Returning|TYPE String|Minimum value of the given table's column|

Usage example

    DATA: lv_min_amount TYPE string.
    
    CALL METHOD zotct_cl_itab_ext=>min  
      EXPORTING  
        iv_colname = 'WRBTR'  
        it_table = lt_bseg  
      RECEIVING  
        r_val = lv_min_amount.

 **- REVERSE():** Reverses the order of items of the given internal table.
| Parameter| Type| Typing| Description
|--|--|--|--|
|CT_TABLE| Changing|TYPE Standard Table|Internal table to be reversed|

Usage example

    CALL METHOD zotct_cl_itab_ext=>reverse
      CHANGING
        ct_table = lt_table .

 **- SHUFFLE():** Change the order of items of the given internal table randomly.
| Parameter| Type| Typing| Description
|--|--|--|--|
|CT_TABLE| Changing|TYPE Standard Table|Internal table to be shuffled|

Usage example

    CALL METHOD zotct_cl_itab_ext=>reverse
      CHANGING
        ct_table = lt_table .
