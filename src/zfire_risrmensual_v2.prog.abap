*&---------------------------------------------------------------------*
*& Report ZISRMENSUAL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
************************************************************************
************************************************************************
*                               AT FSW
*                   todos los derechos reservados
************************************************************************
************************************************************************
*----------------------------------------------------------------------
* programa          : reporte ZFIRE_RISRMENSUAL_V2
*----------------------------------------------------------------------
* id req fsw      : TRX-TR-008
* id req proyecto :
* descripcion     : Reporte determinación de ISR Pagos Provisionales
*                   Régimen General
*
*
* creado por      : Carla Jannette Pérez González
*                   Víctor Alejandro Rodríguez Ramírez
* solicitado por  : Leonardo Juárez Cantero
* fecha           : 22/10/2020
* transporte      :
*----------------------------------------------------------------------
* LOG de modificaciones
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* ID req fsw      : TRX-TR-008
* ID req proyecto :
* descripcion     : Reporte determinación de ISR Pagos Provisionales
*                   Régimen General
*
*
* modificado por  : Carla Jannette Pérez González
*                   Víctor Alejandro Rodríguez Ramírez
* solicitado por  : Leonardo Juárez Cantero
* fecha           : 20/11/2020
* transporte      : S4DK904358
*----------------------------------------------------------------------
*----------------------------------------------------------------------
* ID req fsw      : TRX-TR-008.1
* ID req proyecto :
* descripcion     : Reporte determinación de ISR Pagos Provisionales
*                   Régimen General, ajuste tvarvc
*
* modificado por  : Cristian Omar Jaramillo Méndez
*
* solicitado por  : Leonardo Juárez Cantero
* fecha           : 14/12/2020
* transporte      : S4DK905578
*----------------------------------------------------------------------
INCLUDE ZFIRE_RISRMENSUAL_TOP_V2.
*INCLUDE zfire_risrmensual_top.

* INCLUDE ZISRMENSUAL_O01                         .  " PBO-Modules
* INCLUDE ZISRMENSUAL_I01                         .  " PAI-Modules
INCLUDE ZFIRE_RISRMENSUAL_F01_V2.
*INCLUDE zfire_risrmensual_f01.

START-OF-SELECTION.
  PERFORM f_get_data.

  IF it_report_alv IS NOT INITIAL.
    PERFORM FIX_NEGATIVE_SYMBOL.
    PERFORM f_diplay_alv.
  ELSE.

    IF SV_FLAG_CATALOG EQ 'X'.
      MESSAGE s001(00) WITH TEXT-e02 DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE s001(00) WITH TEXT-e01 DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
