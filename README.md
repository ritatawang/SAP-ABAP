# Import
I_DATUM TYPE DATUM.  "Date
I_XBLNR TYPE XBLNR.  "Reference Document Number
I_SGTXT TYPE SGTXT.  "Item Text
I_EBELN TYPE EBELN.  "Purchasing Document Number

# Export
O_TYPE TYPE CHAR1.  "Return Type S"Successful" E"Error"
O_MESG TYPE STRING.  "Return Message

# Table
IT_POITEM	LIKE BAPI_INCINV_CREATE_ITEM.  "BAPI Incoming Invoice Create for KPO

#Source code
FUNCTION ZRFFI044.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_DATUM) TYPE  DATUM OPTIONAL
*"     VALUE(I_XBLNR) TYPE  XBLNR OPTIONAL
*"     VALUE(I_SGTXT) TYPE  SGTXT OPTIONAL
*"     VALUE(I_EBELN) TYPE  EBELN OPTIONAL
*"  EXPORTING
*"     VALUE(O_TYPE) TYPE  CHAR1
*"     VALUE(O_MESG) TYPE  STRING
*"  TABLES
*"      IT_POITEM STRUCTURE  BAPI_INCINV_CREATE_ITEM
*"----------------------------------------------------------------------

*  WAIT UP TO 1 SECONDS.

  DATA: LV_DMBTR TYPE DMBTR.  "Amount
  DATA: LV_ITEM TYPE N LENGTH 6.  "INV Item
  DATA: CP_EIND TYPE STRING.  "Commit
  DATA:
    "採購訂單抬頭數據
    IT_EKKO             LIKE EKKO OCCURS 0 WITH HEADER LINE,
    "發票行項目數據
    IT_RSEG             LIKE RSEG OCCURS 0 WITH HEADER LINE,
    "抬頭信息
    IS_HEADERDATA       LIKE BAPI_INCINV_CREATE_HEADER,
    "行項目信息
    IT_ITEMDATA         LIKE TABLE OF BAPI_INCINV_CREATE_ITEM,
    IS_ITEMDATA         LIKE BAPI_INCINV_CREATE_ITEM,
    "返回信息
    IT_RETURN           LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE,
    "發票號碼
    LV_INVOICEDOCNUMBER LIKE BAPI_INCINV_FLD-INV_DOC_NO.

  "----判斷日期是否為空
  IF I_DATUM = '00000000'.
    O_TYPE = 'E'.
    O_MESG = 'Please enter the Posting Date.'.

  ELSE.
    "----判斷發票號碼是否為空
    IF I_XBLNR = ''.
      O_TYPE = 'E'.
      O_MESG = 'Please enter the Reference.'.

    ELSE.
      "----判斷PO是否存在
      SELECT EBELN INTO CORRESPONDING FIELDS OF TABLE IT_EKKO FROM EKKO
      WHERE EBELN = I_EBELN.
      IF SY-SUBRC <> 0.
        O_TYPE = 'E'.
        O_MESG = 'The PO dose not exist.'.

      ELSE.
        IF ( I_DATUM <> '00000000' AND I_XBLNR <> '' AND I_EBELN <> '' ).

          "----HEADERDATA
          CLEAR IS_HEADERDATA.
          IS_HEADERDATA-INVOICE_IND = 'X'.
          IS_HEADERDATA-DOC_TYPE = 'RE'.
          IS_HEADERDATA-DOC_DATE = I_DATUM.
          IS_HEADERDATA-PSTNG_DATE = I_DATUM.
          IS_HEADERDATA-BLINE_DATE = I_DATUM.
          IS_HEADERDATA-REF_DOC_NO = I_XBLNR. "Reference Document Number
          IS_HEADERDATA-COMP_CODE = '1000'.
          IS_HEADERDATA-CURRENCY = 'USD'.

          "----ITEMDATA
          SELECT
            EKPO~EBELN AS PO_NUMBER  "PO NUMBER
            EKPO~EBELP AS PO_ITEM    "PO ITEM
            EKBE~BELNR AS REF_DOC    "MIGO MATERIAL NUMBER
            EKBE~GJAHR AS REF_DOC_YEAR
            EKBE~BUZEI AS REF_DOC_IT
            EKPO~BRTWR AS ITEM_AMOUNT
            EKPO~MENGE AS QUANTITY
            EKPO~MEINS AS PO_UNIT
            EKPO~TXJCD AS TAXJURCODE
            INTO CORRESPONDING FIELDS OF TABLE IT_POITEM
            FROM EKPO
            LEFT JOIN EKBE ON EKPO~EBELN = EKBE~EBELN AND EKPO~EBELP = EKBE~EBELP AND BWART = '101'
            WHERE EKPO~EBELN = I_EBELN.

          "----排除已沖銷的mat doc.抓取最新的MIGO憑證
          SORT IT_POITEM BY PO_NUMBER PO_ITEM REF_DOC DESCENDING.
          SORT IT_POITEM BY PO_NUMBER PO_ITEM DESCENDING.
          DELETE ADJACENT DUPLICATES FROM IT_POITEM COMPARING PO_NUMBER PO_ITEM.
          SORT IT_POITEM BY PO_NUMBER PO_ITEM ASCENDING.
          DELETE IT_POITEM WHERE ITEM_AMOUNT = '0'.

          "----賦值ITEMDATA
          CLEAR LV_ITEM.
          LOOP AT IT_POITEM.
            LV_ITEM = LV_ITEM + 1. "行項目
            IS_ITEMDATA-INVOICE_DOC_ITEM = LV_ITEM.
            IS_ITEMDATA-PO_NUMBER = IT_POITEM-PO_NUMBER.
            IS_ITEMDATA-PO_ITEM = IT_POITEM-PO_ITEM.
            IS_ITEMDATA-REF_DOC = IT_POITEM-REF_DOC.
            IS_ITEMDATA-REF_DOC_YEAR = IT_POITEM-REF_DOC_YEAR.
            IS_ITEMDATA-REF_DOC_IT = IT_POITEM-REF_DOC_IT.
            IS_ITEMDATA-ITEM_AMOUNT = IT_POITEM-ITEM_AMOUNT.
            IS_ITEMDATA-QUANTITY = IT_POITEM-QUANTITY.
            IS_ITEMDATA-PO_UNIT = IT_POITEM-PO_UNIT.
            IS_ITEMDATA-TAXJURCODE = IT_POITEM-TAXJURCODE.
            IS_ITEMDATA-TAX_CODE = 'I0'."IT_POITEM-TAX_CODE.
            APPEND IS_ITEMDATA TO IT_ITEMDATA.
            CLEAR: IS_ITEMDATA.
            "計算PO Amount
            LV_DMBTR = LV_DMBTR + IT_POITEM-ITEM_AMOUNT.
            CLEAR IT_POITEM-ITEM_AMOUNT.
          ENDLOOP.

          "----賦值HEADER DATA - AMOUNT
          IS_HEADERDATA-GROSS_AMOUNT = LV_DMBTR.

          CLEAR: O_TYPE, O_MESG.
          CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
            EXPORTING
              HEADERDATA       = IS_HEADERDATA
            IMPORTING
              INVOICEDOCNUMBER = LV_INVOICEDOCNUMBER
            TABLES
              ITEMDATA         = IT_ITEMDATA
              RETURN           = IT_RETURN.

          "----根據處理結果，返回信息
          IF LINES( IT_RETURN ) > 0.
            READ TABLE IT_RETURN INDEX 1.
            IF SY-SUBRC = 0.
              O_TYPE = 'E'.
              O_MESG = IT_RETURN-MESSAGE.
            ENDIF.
          ELSE.
            O_TYPE = 'S'.
            O_MESG = 'Successfully！'.
          ENDIF.
          IF O_TYPE = 'S'.
            CONCATENATE O_MESG 'DOC NO.：' LV_INVOICEDOCNUMBER INTO O_MESG.
          ENDIF.

          "----將inv寫入數據庫（inv有效化）
          LOOP AT IT_RETURN WHERE TYPE = 'E' OR TYPE = 'A'.
            CP_EIND = 'X'.
          ENDLOOP.
          IF CP_EIND <> 'X'.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                WAIT = 'X'.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
