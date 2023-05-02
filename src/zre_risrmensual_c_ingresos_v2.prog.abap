*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Modificaciones                                                       *
*----------------------------------------------------------------------*
*   Fecha   |Consultor   |Transp.No.|Descripción                       *
* dd/mm/aaaa|            |          |                                  *
*----------------------------------------------------------------------*
*29/04/2022 |ASALGADO    |S4DK910473|llenar campo asiganción           *
*&---------------------------------------------------------------------*
INCLUDE ZRE_RISRMENSUAL_C_INGRESOSV2OP.
INCLUDE ZRE_RISRMENSUAL_C_INGRESOSV201.
INCLUDE zre_risrmensual_c_ingresos_f02.

INITIALIZATION.
  GX_VARIANT-REPORT = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = G_SAVE
    CHANGING
      CS_VARIANT = GX_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 0.
    VARIANT = GX_VARIANT-VARIANT.
  ENDIF.

START-OF-SELECTION.

  PERFORM: limpiar_variables.
  PERFORM get_data.
  PERFORM armar_reporte.

  if augbl is not INITIAL.
    delete it_reporte where zdocto_cobro not in augbl.
  endif.
  IF it_reporte IS NOT INITIAL.
*  PERFORM reporte_alv.
    CALL SCREEN 0100. "Llamada a la Dynpro del reporte
  ELSE.
    MESSAGE TEXT-000 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

END-OF-SELECTION.
