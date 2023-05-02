
*&---------------------------------------------------------------------*
*& Report ZFIRE_PAGOS_PROVISIONALES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZFIRE_PAGOS_PROVI_COP_TOP_V2.
*INCLUDE zfire_pagos_provi_cop_top.
*INCLUDE zfire_pagos_provisionales_top           .    " Global Data

* INCLUDE ZFIRE_PAGOS_PROVISIONALES_O01           .  " PBO-Modules
* INCLUDE ZFIRE_PAGOS_PROVISIONALES_I01           .  " PAI-Modules
INCLUDE ZFIRE_PAGOS_PROVI_COP_F01_V2.
*INCLUDE zfire_pagos_provi_cop_f01.
*INCLUDE zfire_pagos_provisionales_f01           .  " FORM-Routines

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

*--> Insert AMP 06.01.2020
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM oculta_campos.
*<-- End insert AMP

START-OF-SELECTION.
  PERFORM limpia_variables.
  PERFORM consultas.
*APPEND tl_report_aux TO tl_report.
  IF tl_report IS NOT INITIAL.
*    PERFORM reporte_alv.
     CALL SCREEN 0100.
  ELSE.
    MESSAGE TEXT-000 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

END-OF-SELECTION.

INCLUDE zfire_pagos_provisionales_cf01.
