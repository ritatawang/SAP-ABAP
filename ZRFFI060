# Import：
I_KUNNR TYPE KUNNR.

# Table：
IT_LIST LIKE ZSFI041.

# Source code：

FUNCTION ZRFFI060.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_KUNNR) TYPE  KUNNR OPTIONAL
*"  TABLES
*"      IT_LIST STRUCTURE  ZSFI041 OPTIONAL
*"----------------------------------------------------------------------

  IF I_KUNNR <> ''.
    "Prefix Supplement 0
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = I_KUNNR
      IMPORTING
        OUTPUT = I_KUNNR.
    "get data
    CLEAR IT_LIST. REFRESH IT_LIST.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_LIST
      FROM ZTFI024
      WHERE KUNNR = I_KUNNR
      AND MSTYP = '01'.
      
  ENDIF.
ENDFUNCTION.
