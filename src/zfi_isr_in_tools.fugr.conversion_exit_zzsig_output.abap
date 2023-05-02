FUNCTION CONVERSION_EXIT_ZZSIG_OUTPUT.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------
*   data: TEXTO(50) TYPE C.
*  " Get the Decimal Format from the user settings (SU01/SU3)
*  " Dezimaldarstellung aus den Benutzereinstellungen holen (SU01/SU3)
*  IF gv_decimal_sign IS INITIAL.
*    CALL FUNCTION 'CLSE_SELECT_USR01'
*      EXPORTING
*        username         = sy-uname
*        iv_delete_buffer = abap_true
*      IMPORTING
*        decimal_sign     = gv_decimal_sign
*        separator        = gv_separator.
*  ENDIF.
*  DATA(lv_input) = CONV string( input ).
*  DATA(lv_negativ_number) = boolc( lv_input CS '-' ).
*  REPLACE ALL OCCURRENCES OF '-' IN lv_input WITH ''.
*  SPLIT lv_input AT '.' INTO DATA(lv_number) DATA(lv_decimals).
*  REPLACE ALL OCCURRENCES OF '.' IN lv_input WITH ''.
*  DO strlen( lv_number ) TIMES.
*    IF sy-index EQ 1.
*      DATA(lv_mask) = |_|.
*      CONTINUE.
*    ENDIF.
*    lv_mask = |_{ COND #( WHEN sy-index MOD 3 EQ 1 THEN gv_separator ) }{ lv_mask }|.
*  ENDDO.
*  DO strlen( lv_decimals ) TIMES.
*    IF sy-index EQ 1.
*      lv_mask = |{ lv_mask }{ gv_decimal_sign }_|.
*      CONTINUE.
*    ENDIF.
*    lv_mask = |{ lv_mask }_|.
*  ENDDO.
*  lv_mask = |{ COND #( WHEN lv_negativ_number EQ abap_true THEN |-| ) }{ lv_mask }|.
**  WRITE lv_input TO output USING EDIT MASK lv_mask RIGHT-JUSTIFIED.
*  output = input.
*  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*    CHANGING
*      VALUE = output.
*
  DATA: TEXTI(100) TYPE C,
        TEXTO(100) TYPE C.

  WRITE INPUT TO TEXTI DECIMALS 2.

  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      VALUE = TEXTI.

  write TEXTI RIGHT-JUSTIFIED to texto.
  output = texto.

*  SEARCH TEXTO FOR '-'.
*  IF SY-SUBRC EQ 0 AND SY-FDPOS NE 0.
*
*    SPLIT TEXTO AT '-' INTO OUTPUT TEXT.
*    CONDENSE OUTPUT.
*    CONCATENATE '-' OUTPUT INTO OUTPUT.
*
*  ELSE.
*
*    OUTPUT = INPUT.
*    CONDENSE OUTPUT.
*
*  ENDIF.
*
*

ENDFUNCTION.
