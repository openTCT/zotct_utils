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
