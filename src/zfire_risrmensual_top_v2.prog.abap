*&---------------------------------------------------------------------*
*& Include ZISRMENSUAL_TOP                          - Report ZISRMENSUAL
*&---------------------------------------------------------------------*
REPORT zisrmensual.

TABLES: fmglflext.

************************************************************************
****                 VARIABLES Y CONSTANTES                         ****
************************************************************************
DATA: gv_nombre_set   TYPE sethier-setid,
      vg_total        TYPE fmglflext-hsl10,
      vg_tot1         TYPE fmglflext-hsl01,
      vg_tot2         TYPE fmglflext-hsl02,
      vg_tot3         TYPE fmglflext-hsl03,
      vg_tot4         TYPE fmglflext-hsl04,
      vg_tot5         TYPE fmglflext-hsl05,
      vg_tot6         TYPE fmglflext-hsl06,
      vg_tot7         TYPE fmglflext-hsl07,
      vg_tot8         TYPE fmglflext-hsl08,
      vg_tot9         TYPE fmglflext-hsl09,
      vg_tot10        TYPE fmglflext-hsl10,
      vg_tot11        TYPE fmglflext-hsl11,
      vg_tot12        TYPE fmglflext-hsl12,
      vg_total_var    TYPE fmglflext-hsl01,
      vg_ret          TYPE fmglflext-racct,
      vg_tot_ret      TYPE fmglflext-hsl01,
      g_it_fieldcat   TYPE slis_t_fieldcat_alv,   "ALV
      g_st_fieldcat   TYPE slis_fieldcat_alv,     "ALV
      gv_type_h_hsl01 TYPE fmglflext-hsl01,
      gv_type_h_hsl02 TYPE fmglflext-hsl02,
      gv_type_h_hsl03 TYPE fmglflext-hsl03,
      gv_type_h_hsl04 TYPE fmglflext-hsl04,
      gv_type_h_hsl05 TYPE fmglflext-hsl05,
      gv_type_h_hsl06 TYPE fmglflext-hsl06,
      gv_type_h_hsl07 TYPE fmglflext-hsl07,
      gv_type_h_hsl08 TYPE fmglflext-hsl08,
      gv_type_h_hsl09 TYPE fmglflext-hsl09,
      gv_type_h_hsl10 TYPE fmglflext-hsl10,
      gv_type_h_hsl11 TYPE fmglflext-hsl11,
      gv_type_h_hsl12 TYPE fmglflext-hsl12,
      vl_hsl01(26)    TYPE c,
      vl_hsl02(26)    TYPE c,
      vl_hsl03(26)    TYPE c,
      vl_hsl04(26)    TYPE c,
      vl_hsl05(26)    TYPE c,
      vl_hsl06(26)    TYPE c,
      vl_hsl07(26)    TYPE c,
      vl_hsl08(26)    TYPE c,
      vl_hsl09(26)    TYPE c,
      vl_hsl10(26)    TYPE c,
      vl_hsl11(26)    TYPE c,
      vl_hsl12(26)    TYPE c,
      vl_total(26)    TYPE c,
      vg_isr_ret_01   TYPE fmglflext-hsl01,
      vg_isr_ret_02   TYPE fmglflext-hsl02,
      vg_isr_ret_03   TYPE fmglflext-hsl03,
      vg_isr_ret_04   TYPE fmglflext-hsl04,
      vg_isr_ret_05   TYPE fmglflext-hsl05,
      vg_isr_ret_06   TYPE fmglflext-hsl06,
      vg_isr_ret_07   TYPE fmglflext-hsl07,
      vg_isr_ret_08   TYPE fmglflext-hsl08,
      vg_isr_ret_09   TYPE fmglflext-hsl09,
      vg_isr_ret_10   TYPE fmglflext-hsl10,
      vg_isr_ret_11   TYPE fmglflext-hsl11,
      vg_isr_ret_12   TYPE fmglflext-hsl12,
      gv_iva_anticipo TYPE tvarvc-low.

CONSTANTS: c_01(2)                            TYPE c VALUE '01',
           c_02(2)                            TYPE c VALUE '02',
           c_03(2)                            TYPE c VALUE '03',
           c_04(2)                            TYPE c VALUE '04',
           c_05(2)                            TYPE c VALUE '05',
           c_06(2)                            TYPE c VALUE '06',
           c_07(2)                            TYPE c VALUE '07',
           c_08(2)                            TYPE c VALUE '08',
           c_09(2)                            TYPE c VALUE '09',
           c_10(2)                            TYPE c VALUE '10',
           c_11(2)                            TYPE c VALUE '11',
           c_12(2)                            TYPE c VALUE '12',
           c_zctas_isr(9)                     TYPE c VALUE 'ZCTAS_ISR',
           c_s(1)                             TYPE c VALUE 'S',
           c_gptx(4)                          TYPE c VALUE 'GPTX',
           c_ingresos_nominales_mensuales(28) TYPE c VALUE 'INGRESOS NOMINALES MENSUALES',
           c_ing_nominales_acumulados(24)     TYPE c VALUE 'ING NOMINALES ACUMULADOS',
           c_z_coeficiente_util(18)           TYPE c VALUE 'Z_COEFICIENTE_UTIL',
           c_coeficiente_de_utilidad(23)      TYPE c VALUE 'COEFICIENTE DE UTILIDAD',
           c_utilidad_fiscal(15)              TYPE c VALUE 'UTILIDAD FISCAL',
           c_perdidas_por_amortizar(22)       TYPE c VALUE 'PERDIDAS POR AMORTIZAR',
           c_perdidas_amortizada(19)          TYPE c VALUE 'PERDIDAS AMORTIZADA',
           c_z_perdidas_x_amort(18)           TYPE c VALUE 'Z_PERDIDAS_X_AMORT',
           c_estimulo_fiscal_ptu_del_mes(27)  TYPE c VALUE 'Estimulo Fiscal PTU del mes',
           c_z_estimulo_ptu(14)               TYPE c VALUE 'Z_ESTIMULO_PTU',
           c_estimulofiscalptuacumulado(30)   TYPE c VALUE 'Estimulo Fiscal PTU Acumulado',
           c_resultado_fiscal(16)             TYPE c VALUE 'RESULTADO FISCAL',
           c_tasa(4)                          TYPE c VALUE 'TASA',
           c_z_tasa_fiscal(13)                TYPE c VALUE 'Z_TASA_FISCAL',
           c_impuesto_determinado(20)         TYPE c VALUE 'IMPUESTO DETERMINADO',
           c_pprovisacumpagadosant(44)        TYPE c VALUE 'PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD',
           c_acreddeisrretmen(38)             TYPE c VALUE 'ACREDITAMIENTO DE ISR RETENIDO MENSUAL',
           c_acredisrretacum(40)              TYPE c VALUE 'ACREDITAMIENTO DE ISR RETENIDO ACUMULADO',
           c_isrpagperdesacredit(62)          TYPE c VALUE 'ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR',
           c_acumisrdespacdt(73)              TYPE c VALUE 'ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET DE ISR(DATO INFORMATIVO)',
           c_2105(4)                          TYPE c VALUE '2105',
           c_retenciones_de_isr(18)           TYPE c VALUE 'RETENCIONES DE ISR',
           c_tot_ret_isr(21)                  TYPE c VALUE 'TOTAL RETENCIONES ISR',
           c_z_isr_retenido(14)               TYPE c VALUE 'Z_ISR_RETENIDO',
           c_isr_retenido(12)                 TYPE c VALUE 'ISR RETENIDO',


           C_PTU_PAGADA(10)                   TYPE C VALUE 'PTU pagada'.



************************************************************************
****                   ESTRUCTURAS LOCALES                          ****
************************************************************************
TYPES:

  "FMGLFLEXT
  BEGIN OF ty_fmglflext,
    racct TYPE fmglflext-racct,
    ryear TYPE fmglflext-ryear,
    drcrk TYPE fmglflext-drcrk,
*    racct TYPE fmglflext-racct,
    hslvt TYPE fmglflext-hslvt,
    hsl01 TYPE fmglflext-hsl01,
    hsl02 TYPE fmglflext-hsl02,
    hsl03 TYPE fmglflext-hsl03,
    hsl04 TYPE fmglflext-hsl04,
    hsl05 TYPE fmglflext-hsl05,
    hsl06 TYPE fmglflext-hsl06,
    hsl07 TYPE fmglflext-hsl07,
    hsl08 TYPE fmglflext-hsl08,
    hsl09 TYPE fmglflext-hsl09,
    hsl10 TYPE fmglflext-hsl10,
    hsl11 TYPE fmglflext-hsl11,
    hsl12 TYPE fmglflext-hsl12,
  END OF ty_fmglflext,

  BEGIN OF ty_fmglflext_aux,
    racct TYPE fmglflext-racct,
    ryear TYPE fmglflext-ryear,
    hslvt TYPE fmglflext-hslvt,
    hsl01 TYPE fmglflext-hsl01,
    hsl02 TYPE fmglflext-hsl02,
    hsl03 TYPE fmglflext-hsl03,
    hsl04 TYPE fmglflext-hsl04,
    hsl05 TYPE fmglflext-hsl05,
    hsl06 TYPE fmglflext-hsl06,
    hsl07 TYPE fmglflext-hsl07,
    hsl08 TYPE fmglflext-hsl08,
    hsl09 TYPE fmglflext-hsl09,
    hsl10 TYPE fmglflext-hsl10,
    hsl11 TYPE fmglflext-hsl11,
    hsl12 TYPE fmglflext-hsl12,
  END OF ty_fmglflext_aux,

  "SKAT
  BEGIN OF ty_skat,
    spras TYPE skat-spras,
    ktopl TYPE skat-ktopl,
    saknr TYPE skat-saknr,
    txt50 TYPE skat-txt50,
  END OF ty_skat,

  BEGIN OF ty_report,
    racct TYPE fmglflext-racct,
    txt50 TYPE skat-txt50,
    hsl01 TYPE fmglflext-hsl01,
    hsl02 TYPE fmglflext-hsl02,
    hsl03 TYPE fmglflext-hsl03,
    hsl04 TYPE fmglflext-hsl04,
    hsl05 TYPE fmglflext-hsl05,
    hsl06 TYPE fmglflext-hsl06,
    hsl07 TYPE fmglflext-hsl07,
    hsl08 TYPE fmglflext-hsl08,
    hsl09 TYPE fmglflext-hsl09,
    hsl10 TYPE fmglflext-hsl10,
    hsl11 TYPE fmglflext-hsl11,
    hsl12 TYPE fmglflext-hsl12,
    total TYPE fmglflext-hsl12,
  END OF ty_report,

  BEGIN OF ty_report_calc,
    racct TYPE fmglflext-racct,
    txt50 TYPE string,
    hsl01 TYPE fmglflext-hsl01,
    hsl02 TYPE fmglflext-hsl02,
    hsl03 TYPE fmglflext-hsl03,
    hsl04 TYPE fmglflext-hsl04,
    hsl05 TYPE fmglflext-hsl05,
    hsl06 TYPE fmglflext-hsl06,
    hsl07 TYPE fmglflext-hsl07,
    hsl08 TYPE fmglflext-hsl08,
    hsl09 TYPE fmglflext-hsl09,
    hsl10 TYPE fmglflext-hsl10,
    hsl11 TYPE fmglflext-hsl11,
    hsl12 TYPE fmglflext-hsl12,
    total TYPE fmglflext-hsl12,
  END OF ty_report_calc,

  BEGIN OF ty_report_calc_dec,  "ADD:COJM}
    racct TYPE fmglflext-racct,
    txt50 TYPE string,
    hsl01 TYPE c LENGTH 26,  "fmglflext-hsl01, "{INI:MOD:COJM
    hsl02 TYPE c LENGTH 26,  "fmglflext-hsl02,
    hsl03 TYPE c LENGTH 26,  "fmglflext-hsl03,
    hsl04 TYPE c LENGTH 26,  "fmglflext-hsl04,
    hsl05 TYPE c LENGTH 26,  "fmglflext-hsl05,
    hsl06 TYPE c LENGTH 26,  "fmglflext-hsl06,
    hsl07 TYPE c LENGTH 26,  "fmglflext-hsl07,
    hsl08 TYPE c LENGTH 26,  "fmglflext-hsl08,
    hsl09 TYPE c LENGTH 26,  "fmglflext-hsl09,
    hsl10 TYPE c LENGTH 26,  "fmglflext-hsl10,
    hsl11 TYPE c LENGTH 26,  "fmglflext-hsl11,
    hsl12 TYPE c LENGTH 26,  "fmglflext-hsl12,
    total TYPE c LENGTH 26,  "fmglflext-hsl12, "FIN:MOD:COJM}
  END OF ty_report_calc_dec,

  "TVARVC
  BEGIN OF ty_tvarvc,
    low  TYPE tvarvc-low,
    high TYPE tvarvc-high,
  END OF ty_tvarvc,

  BEGIN OF ty_tvarvc_acreedoras,
    low TYPE tvarvc-low,
  END OF ty_tvarvc_acreedoras,

  BEGIN OF ty_tvar_cuenta_anticipos,
    low TYPE tvarvc-low,
  END OF ty_tvar_cuenta_anticipos,

  BEGIN OF ty_tvar_clasedoc_anticipos,
    low TYPE tvarvc-low,
  END OF ty_tvar_clasedoc_anticipos,

  BEGIN OF ty_iva_anticipo,
    low TYPE tvarvc-low,
  END OF ty_iva_anticipo,

  BEGIN OF ty_tvarvc_subsidio,
    low TYPE tvarvc-low,
  END OF ty_tvarvc_subsidio,

  BEGIN OF ty_report_ret,
    racct TYPE fmglflext-racct,
    txt50 TYPE string,
    hsl01 TYPE fmglflext-hsl01,
    hsl02 TYPE fmglflext-hsl02,
    hsl03 TYPE fmglflext-hsl03,
    hsl04 TYPE fmglflext-hsl04,
    hsl05 TYPE fmglflext-hsl05,
    hsl06 TYPE fmglflext-hsl06,
    hsl07 TYPE fmglflext-hsl07,
    hsl08 TYPE fmglflext-hsl08,
    hsl09 TYPE fmglflext-hsl09,
    hsl10 TYPE fmglflext-hsl10,
    hsl11 TYPE fmglflext-hsl11,
    hsl12 TYPE fmglflext-hsl12,
    total TYPE fmglflext-hsl12,
  END OF ty_report_ret,

  BEGIN OF ty_report_alv ,
    racct TYPE fmglflext-racct,
    txt50 TYPE string,
    hsl01 TYPE string,
    hsl02 TYPE string,
    hsl03 TYPE string,
    hsl04 TYPE string,
    hsl05 TYPE string,
    hsl06 TYPE string,
    hsl07 TYPE string,
    hsl08 TYPE string,
    hsl09 TYPE string,
    hsl10 TYPE string,
    hsl11 TYPE string,
    hsl12 TYPE string,
    total TYPE string,
  END OF ty_report_alv,

  BEGIN OF ty_acdoca,
    rbukrs TYPE acdoca-rbukrs,
    "gjahr  TYPE acdoca-gjahr,
    racct  TYPE acdoca-racct,
    wsl    TYPE acdoca-wsl,
    poper  TYPE acdoca-poper,
    "blart  TYPE acdoca-blart,
  END OF ty_acdoca,

  BEGIN OF ty_acdoca_aux,
    racct  TYPE acdoca-racct,
    wsl_01 TYPE acdoca-wsl,
    wsl_02 TYPE acdoca-wsl,
    wsl_03 TYPE acdoca-wsl,
    wsl_04 TYPE acdoca-wsl,
    wsl_05 TYPE acdoca-wsl,
    wsl_06 TYPE acdoca-wsl,
    wsl_07 TYPE acdoca-wsl,
    wsl_08 TYPE acdoca-wsl,
    wsl_09 TYPE acdoca-wsl,
    wsl_10 TYPE acdoca-wsl,
    wsl_11 TYPE acdoca-wsl,
    wsl_12 TYPE acdoca-wsl,
  END OF ty_acdoca_aux,

  "Agregando nuevas denificiones
  "Francisco Rodriguez 20/02/2023

  BEGIN OF TY_CUENTAS,
    BUKRS TYPE ZFI_CUENTAS_ISR-BUKRS,
    GJAHR TYPE ZFI_CUENTAS_ISR-GJAHR,
    RACCT TYPE ZFI_CUENTAS_ISR-RACCT,
  END OF  TY_CUENTAS,

  BEGIN OF TY_COFUT,
    GJAHR TYPE ZFI_CUENTAS_ISR-GJAHR,
    BUKRS TYPE ZFI_CUENTAS_ISR-BUKRS,
    MONAT TYPE ZFI_CUENTAS_ISR-MONAT,
    ZCOEFICIENTE_UTILIDAD TYPE ZFI_CUENTAS_ISR-ZCOEFICIENTE_UTILIDAD,
  END OF TY_COFUT.

************************************************************************
****              TABLAS INTERNAS Y AREAS DE TRABAJO                ****
************************************************************************
DATA:
  "TABLAS
  it_tvarvc                  TYPE TABLE OF ty_tvarvc,
  it_tvarvc_acreedoras       TYPE TABLE OF ty_tvarvc_acreedoras,
  it_tvar_cuenta_anticipos   TYPE TABLE OF ty_tvar_cuenta_anticipos,
  it_tvar_clasedoc_anticipos TYPE TABLE OF ty_tvar_clasedoc_anticipos,
  it_iva_anticipo            TYPE STANDARD TABLE OF ty_iva_anticipo,
  it_tvarvc_subsidio         TYPE TABLE OF ty_tvarvc_subsidio,
  it_fmglflext               TYPE STANDARD TABLE OF ty_fmglflext,
  it_fmglflext_aux           TYPE STANDARD TABLE OF ty_fmglflext_aux,
  it_acdoca                  TYPE STANDARD TABLE OF ty_acdoca,
  it_acdoca_aux              TYPE STANDARD TABLE OF ty_acdoca_aux,
  it_data_set                TYPE TABLE OF rgsbv,
  it_skat                    TYPE STANDARD TABLE OF ty_skat,
  it_report                  TYPE STANDARD TABLE OF ty_report,
  it_report_anticipo         TYPE STANDARD TABLE OF ty_report,
  it_report_aux              TYPE STANDARD TABLE OF ty_report,
  it_report_cal              TYPE STANDARD TABLE OF ty_report_calc,
  it_report_cal_dec          TYPE STANDARD TABLE OF ty_report_calc_dec,
  it_fmglflext_s             TYPE STANDARD TABLE OF ty_fmglflext,
  it_fmglflext_h             TYPE STANDARD TABLE OF ty_fmglflext,
  it_report_ret              TYPE STANDARD TABLE OF ty_report_ret,
  it_report_alv              TYPE STANDARD TABLE OF ty_report_alv,

  "Tipo Rango
  rg_data_set                TYPE RANGE OF fmglflext-racct,
  rwa_data_set               LIKE LINE OF rg_data_set,

  rg_racct                   TYPE RANGE OF fmglflext-racct,
  rwa_racct                  LIKE LINE OF  rg_racct,

  rg_racct_re_isr            TYPE RANGE OF fmglflext-racct,
  rwa_racct_re_isr           LIKE LINE OF  rg_racct,

  rg_isr_rete                TYPE RANGE OF fmglflext-racct,
  rwa_isr_rete               LIKE LINE OF  rg_racct,

  "Rango anticipos
  rg_cuenta_anticipo         TYPE RANGE OF acdoca-racct,
  rwa_cuenta_anticipo        LIKE LINE OF  rg_cuenta_anticipo,

  rg_clasedoc_anticipo       TYPE RANGE OF acdoca-blart,
  rwa_clasedoc_anticipo      LIKE LINE OF  rg_clasedoc_anticipo,

  "AREAS DE TABAJO
  wa_tvarvc                  TYPE ty_tvarvc,
  wa_tvarvc_acreedoras       TYPE ty_tvarvc_acreedoras,
  wa_tvar_cuenta_anticipos   TYPE ty_tvar_cuenta_anticipos,
  wa_tvar_clasedoc_anticipos TYPE ty_tvar_clasedoc_anticipos,
  wa_iva_anticipo            TYPE ty_iva_anticipo,
  wa_tvarvc_subsidio         TYPE ty_tvarvc_subsidio,
  wa_fmglflext               TYPE ty_fmglflext,
  wa_acdoca                  TYPE ty_acdoca,
  wa_acdoca_aux              TYPE ty_acdoca_aux,
  wa_data_set                TYPE rgsbv,
  wa_skat                    TYPE ty_skat,
  wa_report                  TYPE ty_report,
  wa_report_anticipo         TYPE ty_report,
  wa_report_cal              TYPE ty_report_calc,
  wa_report_cal_dec          TYPE ty_report_calc_dec,
  wa_report_calc             TYPE ty_report_calc,
  wa_report_calcu            TYPE ty_report_calc,
  wa_report_cal_aux          TYPE ty_report_calc,
  wa_report_cal_aux1         TYPE ty_report_calc,
  wa_fmglflext_s             TYPE ty_fmglflext,
  wa_fmglflext_h             TYPE ty_fmglflext,
  wa_report_ret              TYPE ty_report_ret,
  wa_report_alv              TYPE ty_report_alv,
  wa_fmglflext_aux           TYPE ty_fmglflext_aux,


  "Agregando nuevas variables por modificación de reporte
  " Francisco Rodriguez - 20/02/2023

  IT_CUENTAS                 TYPE STANDARD TABLE OF TY_CUENTAS,
  IT_COFUT                   TYPE STANDARD TABLE OF TY_COFUT,
  WA_CUENTAS                 TYPE TY_CUENTAS,
  WA_COFUT                   TYPE TY_COFUT,
  SV_FACTOR_RESULTADO        TYPE C LENGTH 26,
  SV_FACTOR_RESULTADO_AUX    TYPE C LENGTH 26,
  SV_FLAG_CATALOG            TYPE XFELD.

FIELD-SYMBOLS: <fs_report_cal_1>  TYPE ty_report_calc,
               <fs_report_cal_2>  TYPE ty_report_calc,
               <fs_report_cal_3>  TYPE ty_report_calc,
               <fs_report_cal_4>  TYPE ty_report_calc,
               <fs_report_cal_5>  TYPE ty_report_calc,
               <fs_report_cal_6>  TYPE ty_report_calc,
               <fs_report_cal_7>  TYPE ty_report_calc,
               <fs_report_cal_8>  TYPE ty_report_calc,
               <fs_report_cal_9>  TYPE ty_report_calc,
               <fs_report_cal_10> TYPE ty_report_calc,
               <fs_report_cal_11> TYPE ty_report_calc,
               <fs_report_cal_12> TYPE ty_report_calc.

************************************************************************
****                   PANTALLA DE SELECCION                        ****
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1.
PARAMETERS: s_rbukrs LIKE fmglflext-rbukrs, "Sociedad
            "s_RACCT  FOR fmglflext-RACCT, "Número de Cuenta
            s_rldnr  LIKE fmglflext-rldnr. "Ledger
SELECT-OPTIONS:
            s_ryear  FOR fmglflext-ryear. "Ejercicio
SELECTION-SCREEN END OF BLOCK b1.
