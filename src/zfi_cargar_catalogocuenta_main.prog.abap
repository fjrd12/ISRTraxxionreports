*&---------------------------------------------------------------------*
*& Include          ZFI_CARGAR_CATALOGOCUENTA_MAIN
*&---------------------------------------------------------------------*
START-OF-SELECTION.

IF NOT P_PATH IS INITIAL.

  PERFORM READ_FILE.

ENDIF.

IF P_SHOW EQ 'X'.

  PERFORM DISPLAY_TABLE.

ENDIF.
