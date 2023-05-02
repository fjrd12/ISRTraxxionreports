FUNCTION ZFI_REPARAR_SIGNO_NEGATIVO.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(VALUE) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(VALUE_FIXED) TYPE  STRING
*"----------------------------------------------------------------------

DATA:  TEXT(1) TYPE C.

SEARCH VALUE FOR '-'.
IF SY-SUBRC EQ 0 AND SY-FDPOS NE 0.

  SPLIT VALUE AT '-' INTO VALUE_FIXED TEXT.
  CONDENSE VALUE_FIXED.
  CONCATENATE '-' VALUE_FIXED INTO VALUE_FIXED.

ELSE.

  VALUE_FIXED = VALUE.
  CONDENSE VALUE_FIXED.

ENDIF.




ENDFUNCTION.
