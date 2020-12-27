*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZOTCT_T0001
*   generation date: 18.12.2020 at 19:41:57
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZOTCT_T0001        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
