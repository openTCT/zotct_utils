
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
|COLNAME| Importing |TYPE String|The column name whose maximum value is to be returned.|
|TABLE|Importing|TYPE Standard Table|The table to be returned maximum value of one of its column.|
|VAL|Returning|TYPE String|Maximum value of the given table's column.|

Usage example

    DATA: lv_seqnr_max TYPE string.
    
    CALL METHOD zotct_cl_itab_ext=>max  
      EXPORTING  
        colname = 'SEQNR'  
        table = gt_flattab  
      RECEIVING  
        val = lv_seqnr_max.

 **- MIN():** Returns minimum value of the given column of table.
 
| Parameter| Type| Typing| Description
|--|--|--|--|
|COLNAME| Importing |TYPE String|The column name whose minimum value is to be returned.|
|TABLE|Importing|TYPE Standard Table|The table to be returned minimum value of one of its column.|
|VAL|Returning|TYPE String|Minimum value of the given table's column.|

Usage example

    DATA: lv_min_amount TYPE string.
    
    CALL METHOD zotct_cl_itab_ext=>min  
      EXPORTING  
        colname = 'WRBTR'  
        table = lt_bseg  
      RECEIVING  
        val = lv_min_amount.

 **- REVERSE():** Reverses the order of items of the given internal table.
| Parameter| Type| Typing| Description
|--|--|--|--|
|TABLE| Changing|TYPE Standard Table|Internal table to be reversed.|

Usage example

    CALL METHOD zotct_cl_itab_ext=>reverse
      CHANGING
        table = lt_table .

 **- SHUFFLE():** Change the order of items of the given internal table randomly.
| Parameter| Type| Typing| Description
|--|--|--|--|
|TABLE| Changing|TYPE Standard Table|Internal table to be shuffled.|

Usage example

    CALL METHOD zotct_cl_itab_ext=>reverse
      CHANGING
        table = lt_table .
## ZOTCT_CL_TIN
Utilities for tax identification numbers.
**Methods:**
 **- CREATE_DUMMY_TIN():** Returns a valid dummy tax identification number based on given locale and count.
 | Parameter| Type| Typing| Description
|--|--|--|--|
|LOCALE| Importing|TYPE LAND1|Locale of Tax Identification Number.|
|COUNT| Importing|TYPE SYTABIX|Number of TINs to be generated.|
|TYPE| Importing|TYPE CHAR4 |TIN type (business, customer etc.)|
|TIN_LIST| Exporting|LIKE GT_TIN|Generated TIN list (if more than one TINs are requested.)|
|TIN| Exporting|TYPE STRING|Generated TIN (If only one TIN is requested.|
Usage example

    DATA : lt_tin_list LIKE zotct_cl_tin=>gt_tin.  
  
	CALL METHOD zotct_cl_tin=>create_dummy_tin  
		EXPORTING  
			locale = 'TR'  
			count = 12  
			type = 'TCKN'  
		IMPORTING  
			tin_list = lt_tin_list. 
 **- VALIDATE_TIN():** Validates given TIN(s) based on locale.
 | Parameter| Type| Typing| Description
|--|--|--|--|
|LOCALE| Importing|TYPE LAND1|Locale of Tax Identification Number.|
|TIN_LIST| Importing|LIKE GT_TIN|TIN List to be validated.|
|TIN| Importing|TYPE String |Single TIN to be validated.|
|TYPE| Importing|TYPE CHAR4|TIN type (business, customer etc.)|
|RESULT_LIST| Exporting|LIKE GT_RESULT|TIN list validation result table.|
|VALID| Exporting|TYPE ABAP_BOOL|Validity flag. True if all given TINs are valid.|

    DATA: lt_result_list LIKE zotct_cl_tin=>gt_result,  
		  lv_valid TYPE abap_bool.  
	  
	CALL METHOD zotct_cl_tin=>validate_tin  
		EXPORTING  
			locale = 'TR'  
			tin_list = lt_tin_list  
			type = 'TCKN'  
		IMPORTING  
			result_list = lt_result_list  
			valid = lv_valid.
