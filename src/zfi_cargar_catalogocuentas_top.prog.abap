*&---------------------------------------------------------------------*
*& Include          ZFI_CARGAR_CATALOGOCUENTAS_TOP
*&---------------------------------------------------------------------*

DATA: V_VALUE_CONVERTED   TYPE CHA_CLASS_VIEW-SOLLWERT,
      V_VALUE_TO_CONVERT  TYPE CHA_CLASS_DATA-SOLLWERT,
      G_ST_FIELDCAT       TYPE SLIS_FIELDCAT_ALV,
      G_IT_FIELDCAT       TYPE SLIS_T_FIELDCAT_ALV.

DATA IT_ZFI_CUENTAS_ISR TYPE STANDARD TABLE OF ZFI_CUENTAS_ISR.

DATA WA_LAYOUT TYPE SLIS_LAYOUT_ALV.
