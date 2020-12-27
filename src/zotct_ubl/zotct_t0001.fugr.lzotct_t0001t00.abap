*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.12.2020 at 19:41:58
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZOTCT_T0001.....................................*
DATA:  BEGIN OF STATUS_ZOTCT_T0001                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZOTCT_T0001                   .
CONTROLS: TCTRL_ZOTCT_T0001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZOTCT_T0001                   .
TABLES: ZOTCT_T0001                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
