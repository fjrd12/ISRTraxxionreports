*&---------------------------------------------------------------------*
*& Include ZFIRE_PAGOS_PROVISIONALES_TOP            - Report ZFIRE_PAGOS_PROVISIONALES
*&---------------------------------------------------------------------*
REPORT zfire_pagos_provisionales.
TABLES: bkpf.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
*hotspot click control
    METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no sender. "Sender es para saber que ALV mandó el evento
" on_double_click
    METHODS: on_doble_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column es_row_no sender.

ENDCLASS. "lcl_event_handler DEFINITION
*--------------------Estructuras--------------------------*
TYPES: BEGIN OF ty_report,
         semaforo                   TYPE char4,
         bukrs                      TYPE bukrs,
         gjahr                      TYPE gjahr,
         periodo                    TYPE zperiodo,
         fechapago                  TYPE sy-datum,
         descripcion                TYPE char255,
         totalcargos                TYPE bseg-dmbtr,
         referencia                 TYPE char255,
         concepto                   TYPE char255,
         n_doc_pago                 TYPE belnr_d,
         n_doc_pago_sociedad        TYPE bukrs,
         n_doc_pago_ejercicio       TYPE gjahr,
         n_sol_anticipo             TYPE belnr_d,
         n_sol_anticipo_sociedad    TYPE bukrs,
         n_sol_anticipo_ejercicio   TYPE gjahr,
         n_documento_fac            TYPE belnr_d,
         n_documento_fac_sociedad   TYPE bukrs,
         n_documento_fac_ejercicio  TYPE gjahr,
         clasificacion              TYPE char255,
         foliofiscal                TYPE char255,
         impor_mon_l_fac            TYPE acdoca-tsl,
         rfc                        TYPE char20,
         razon_s                    TYPE char200,
         acredor                    TYPE acdoca-lifnr,
         ind_iva_16                 TYPE acdoca-mwskz,
         base_16                    TYPE acdoca-hsl,

         indi_iva_8                 TYPE acdoca-mwskz,
         base_8                     TYPE acdoca-hsl,

         ind_iva_0                  TYPE acdoca-mwskz,
         base_0                     TYPE acdoca-hsl,

         ind_exento                 TYPE acdoca-mwskz,
         base_exento                TYPE acdoca-hsl,

         ind_nodedu                TYPE acdoca-mwskz,
         base_nodedu                TYPE acdoca-hsl,

         ind_no_obj                 TYPE acdoca-mwskz,
         base_no_obj                TYPE acdoca-hsl,
         iva_16                     TYPE acdoca-hsl,
         iva_8                      TYPE acdoca-hsl,
         impu_iva_virtual           TYPE acdoca-hsl,
         iva_ret_arren              TYPE acdoca-hsl,
         iva_ret_serv_prof          TYPE acdoca-hsl,
         iva_ret_6                  TYPE acdoca-hsl,
         iva_retenido_4             TYPE acdoca-hsl,
         IVA_RET_FLETES             TYPE acdoca-hsl,
         imp_ret_cedular            TYPE acdoca-hsl,
         isr_honorarios             TYPE acdoca-hsl,
         isr_arrendamiento          TYPE acdoca-hsl,
         isr_pagos_extranjero       TYPE acdoca-hsl,
         isr_dividendos             TYPE acdoca-hsl,
         isr_intereses              TYPE acdoca-hsl,
         isr_venta_acciones_retener TYPE acdoca-hsl,
         isr_resico                 TYPE acdoca-hsl,
         2_sar                      TYPE acdoca-hsl,
         5_infonavit                TYPE acdoca-hsl,
         ayuda_sindic_x_pagar       TYPE acdoca-hsl,
         casetas                    TYPE acdoca-hsl,
         cesantia_vejez             TYPE acdoca-hsl,
         compens_extraordinar       TYPE acdoca-hsl,
         cuota_patronal_imss        TYPE acdoca-hsl,
         domingos_dias_fest         TYPE acdoca-hsl,
         fondo_ahorro               TYPE acdoca-hsl,
         otras_prestaciones         TYPE acdoca-hsl,
         otros_gastos_viaje         TYPE acdoca-hsl,
         premio_puntualidad         TYPE acdoca-hsl,
         prima_dominical            TYPE acdoca-hsl,
         rva_aguinaldo              TYPE acdoca-hsl,
         rva_prima_vacacional       TYPE acdoca-hsl,
         subsidio_empleo            TYPE acdoca-hsl,
         sueldos                    TYPE acdoca-hsl,
         viaticos                   TYPE acdoca-hsl,
         sueldos_por_pagar          TYPE acdoca-hsl,
         tiempo_extra_doble         TYPE acdoca-hsl,
         tiempo_extra_triple        TYPE acdoca-hsl,
         cesantia_vejez_empl        TYPE acdoca-hsl,
         cesantia_vejez_patro       TYPE acdoca-hsl,
         descuento_infonacot        TYPE acdoca-hsl,
         fac_admvas_maniobras       TYPE acdoca-hsl,
         fac_admvas_gts_camino      TYPE acdoca-hsl,
         fac_admvas_gts_talachas    TYPE acdoca-hsl,
         fac_admvas_gts_rep_men     TYPE acdoca-hsl,
         fac_admvas_gts_fianzas     TYPE acdoca-hsl,
         finiquito_por_pagar        TYPE acdoca-hsl,
         fondo_ahorro_emplead       TYPE acdoca-hsl,
         fondo_ahorro_empresa       TYPE acdoca-hsl,
         impulso_economico_crea     TYPE acdoca-hsl,
         imss_patronal              TYPE acdoca-hsl,
         imss_retenido              TYPE acdoca-hsl,
         infonavit_patronal         TYPE acdoca-hsl,
         infonavit_retenido         TYPE acdoca-hsl,
         ing_x_dev_empleados        TYPE acdoca-hsl,
         ing_x_descto_comedor       TYPE acdoca-hsl,
         isr_ret_x_sueldos          TYPE acdoca-hsl,
         prov_agui_x_pagar          TYPE acdoca-hsl,
         prov_prim_vacacional       TYPE acdoca-hsl,
         total                      TYPE acdoca-hsl,
         prima_vacacional           TYPE  zfied_prim_vacacional,
         cuota_sind_x_pagotot       TYPE  zfied_cuotasind_x_pagtot,
         pension_alim_total         TYPE   zfied_pens_alim_total,
         seguro_viv_infon_tot       TYPE zfied_seguro_viv_infon_tot,
         subsidio_empleo_tot        TYPE zfied_subsid_empleo_tot,

         otros_gastos               TYPE acdoca-hsl,
         otros_retiros              TYPE acdoca-hsl,
         isr_2                      TYPE acdoca-hsl,
         interes                    TYPE acdoca-hsl,
         activo_fijo1               type acdoca-anln1,
         activo_fijo2               type acdoca-anln2,
         zbanco_cobro               TYPE t012t-text1,

         indice                     TYPE sy-tabix,
         new_report                 TYPE XFELD,
       END OF ty_report,


****--> Agregando estructuras para modificacion
****--> Fredmar Pulido - 15/03/2023
       BEGIN OF TY_TRASPASOS,
         RLDNR  TYPE ACDOCA-RLDNR,
         RBUKRS TYPE ACDOCA-RBUKRS,
         GJAHR  TYPE ACDOCA-GJAHR,
         POPER  TYPE ACDOCA-POPER,
         BLART  TYPE ACDOCA-BLART,
         BUDAT  TYPE ACDOCA-BUDAT,
         BLDAT  TYPE ACDOCA-BLDAT,
         BELNR  TYPE ACDOCA-BELNR,
         DOCLN  TYPE ACDOCA-DOCLN,
         RACCT  TYPE ACDOCA-RACCT,
         TSL    TYPE ACDOCA-TSL,
         SGTXT   TYPE ACDOCA-SGTXT,
         WSL    TYPE ACDOCA-WSL,
         RWCUR  TYPE ACDOCA-RWCUR,
         HSL    TYPE ACDOCA-HSL,
         HBKID  TYPE ACDOCA-HBKID,
         HKTID  TYPE ACDOCA-HKTID,
         AWKEY  TYPE BKPF-AWKEY,
         XBLNR  TYPE BKPF-XBLNR,
         AUGBL  TYPE ACDOCA-AUGBL,
         AUGGJ  TYPE ACDOCA-AUGGJ,
         AUGDT  TYPE ACDOCA-AUGDT,
         KUNNR  TYPE ACDOCA-KUNNR,
         KURSF  TYPE BKPF-KURSF,
         KTOPL  TYPE KTOPL,
         LIFNR  TYPE LIFNR,
       END OF TY_TRASPASOS,

       BEGIN OF TY_PARAM_TRANSAC,
         NRO_DOC TYPE belnr_d,
         SOCIETY TYPE BUKRS,
         EXERCICE TYPE GJAHR,
       END OF TY_PARAM_TRANSAC,

       TTYREPORT           TYPE STANDARD TABLE OF TY_REPORT,
       TTRASPASO           TYPE STANDARD TABLE OF TY_TRASPASOS WITH NON-UNIQUE SORTED KEY CLAVE1 COMPONENTS GJAHR BELNR RBUKRS RACCT
                                                              WITH NON-UNIQUE SORTED KEY CLAVE2 COMPONENTS RLDNR RBUKRS GJAHR BELNR
                                                              WITH NON-UNIQUE SORTED KEY CLAVE3 COMPONENTS RBUKRS BELNR GJAHR,

       TTAMEX              TYPE STANDARD TABLE OF ZFI_EGRESOS_OUTPUT,

       TTDFKKBPTAXNUM     TYPE STANDARD TABLE OF DFKKBPTAXNUM.

*--------------------Work Area--------------------------*
DATA: sl_report TYPE ty_report.


*--------------------Tablas Internas--------------------------*
DATA: tl_report           TYPE TABLE OF ty_report,
      tl_report2          TYPE TABLE OF ty_report,
      tl_fieldcat         TYPE lvc_t_fcat,
      tl_ztax_detalle_egr TYPE TABLE OF ztax_detalle_egr,
      tl_lines            TYPE TABLE OF tline.


****--> Agregando tablas internas para modificacion
****--> Fredmar Pulido - 15/03/2023
      "Traspaso
DATA: IT_TR_ORIGEN          TYPE TTRASPASO,
      IT_TR_DESTINO         TYPE TTRASPASO,
      "Inversiones
      IT_INV_ORIGEN         TYPE TTRASPASO,
      IT_INV_DESTINO        TYPE TTRASPASO,
      "Amex
      IT_AMEX               TYPE TTAMEX,
      "Confirming
      IT_CONFIRMING         TYPE TTAMEX,
      "Anticipo
      IT_ANTICIPO           TYPE TTAMEX,
      IT_ANTICIPOAUX        TYPE TTAMEX,
      "Reembloso
      IT_REEMBOLSO          TYPE ZFI_EGRESOS_OUTPUTT,
      "Pagos a proveedor general
      IT_PGENERAL           TYPE ZFI_EGRESOS_OUTPUTT,
      "Liquidaciones
      IT_LIQUIDA            TYPE ZFI_EGRESOS_OUTPUTT,
      IT_INPUT              TYPE TTAMEX,
      IT_OUTPUT             TYPE TTAMEX,

      IT_DFKKBPTAXNUM       TYPE TTDFKKBPTAXNUM.


*--------------------Rangos--------------------------*
DATA: rg_augbl                       TYPE RANGE OF acdoca-augbl,
      rg_ran_liq                     TYPE RANGE OF acdoca-augbl,
      rg_blart                       TYPE RANGE OF acdoca-blart,
      rg_cta_cta                     TYPE RANGE OF acdoca-blart,
      rg_ti_doc_liq                  TYPE RANGE OF acdoca-blart,
      rg_ti_com_liq                  TYPE RANGE OF acdoca-blart,
      rg_blart_2                     TYPE RANGE OF acdoca-blart,
      rg_z_fondo_fijo                TYPE RANGE OF acdoca-blart,
      rg_ziva_0                      TYPE RANGE OF acdoca-mwskz,
      rg_lifnr                       TYPE RANGE OF bseg-lifnr,
      rg_z_referencia_interme        TYPE RANGE OF char2,
      rg_z_caso_ktosl_001            TYPE RANGE OF acdoca-ktosl,
      rg_z_caso_ktosl_004            TYPE RANGE OF acdoca-ktosl,
      rg_z_caso_ktosl_004_low        TYPE RANGE OF acdoca-ktosl,
      rg_ziva_16                     TYPE RANGE OF acdoca-racct,
*      rg_ziva_0                      TYPE RANGE OF acdoca-racct,
      rg_z_ejercicioval              TYPE RANGE OF acdoca-gjahr,
      rg_docval                      TYPE RANGE OF acdoca-belnr,
      rg_zret_iva_ret_sub_fletes     TYPE RANGE OF acdoca-racct,
      rg_resumen_nomina              TYPE RANGE OF tvarvc,
      rg_acreedores_movto            TYPE RANGE OF acdoca-linetype,
      rg_acreedores_movto_high       TYPE RANGE OF acdoca-blart,
      rg_compro_gasto                TYPE RANGE OF acdoca-linetype,
      rg_compro_gasto_high           TYPE RANGE OF acdoca-blart,
      rg_cargo_abono_compro_low      TYPE RANGE OF acdoca-bschl,
      rg_cargo_abono_low             TYPE RANGE OF acdoca-bschl,
      rg_ziva_8_low                  TYPE RANGE OF acdoca-racct,
      rg_ziva_8_high                 TYPE RANGE OF acdoca-mwskz,
      rg_ziva16                      TYPE RANGE OF acdoca-mwskz,
      rg_zret_iva_ret_sub_contra     TYPE RANGE OF acdoca-racct,
      rg_zret_ret_isr_honorarios     TYPE RANGE OF acdoca-racct,
      rg_zret_iva_ret_sub_honorarios TYPE RANGE OF acdoca-racct,
      rg_z_ctas_si                   TYPE RANGE OF acdoca-racct,
      rg_nomina_exclusion_s          TYPE RANGE OF acdoca-racct,
      rg_nomina_exclusion_h          TYPE RANGE OF acdoca-racct,
      rg_zret_ret_isr_arrendamiento  TYPE RANGE OF acdoca-racct,
      rg_zret_iva_ret_sub_arren      TYPE RANGE OF acdoca-racct,
      rg_imp_ret_cedular             TYPE RANGE OF acdoca-racct,
      rg_ret_isr_pagext              TYPE RANGE OF acdoca-racct,
      rg_nomina_ctas_a_detallar_low  TYPE RANGE OF acdoca-racct,
      rg_ctas_saldo_cero             TYPE RANGE OF acdoca-racct,
      rg_clave_egreso                TYPE RANGE OF acdoca-racct,
      rg_factoraje_egreso_cd         TYPE RANGE OF acdoca-racct,
      rg_facturas_fiscales_cd        TYPE RANGE OF acdoca-racct,
      rg_ctas_fac_fis                TYPE RANGE OF acdoca-racct,
      rg_ziva_16_high                TYPE RANGE OF acdoca-mwskz,
      rg_ziva_nd                     TYPE RANGE OF acdoca-mwskz,
      rg_belnr                       TYPE RANGE OF bseg-belnr,
      rg_hkont                       TYPE RANGE OF bseg-hkont,
      "Documentos de traspaso
      RG_DOCS_TRASPASO      TYPE RANGE OF ACDOCA-BLART,
      RG_CUENTAS_ORIGEN     TYPE RANGE OF ACDOCA-RACCT,
      RG_CUENTAS_DESTINO    TYPE RANGE OF ACDOCA-RACCT,
      RG_CUENTAS_CONCENT    TYPE RANGE OF ACDOCA-RACCT,
      RG_DOC_INVER          TYPE RANGE OF ACDOCA-BLART.
*--------------------Variables--------------------------*
DATA: vl_name            TYPE thead-tdname,
      vl_htype           TYPE dd01v-datatype,
      vl_total_registros TYPE i,
      vl_length          TYPE i,
      vl_buzei           TYPE bseg-buzei,
      vl_belnr_aux       TYPE bseg-belnr,
      vl_cont            TYPE i,
      vl_docu_ejercicio  TYPE char14,
      ref_ejecucion      TYPE REF TO cx_root,
      vl_total           TYPE acdoca-hsl,
      vl_valida_folio    TYPE C LENGTH 1,
      vl_answer          TYPE string,
      vl_entro           TYPE c,
      ob_grid            TYPE REF TO cl_gui_alv_grid,
      gd_layout          TYPE lvc_s_layo.

" Agregando Variables por actualizacion de reporte
" Fredmar Pulido

DATA: GX_VARIANT            TYPE DISVARIANT,
      G_SAVE                TYPE C VALUE 'X',
      G_VARIANT             TYPE DISVARIANT,
      lo_container          TYPE REF TO cl_gui_custom_container,
      lo_event_handler      TYPE REF TO lcl_event_handler. "Este es para ambos reportes

DATA: wa_variant            TYPE disvariant,
      wa_layout             TYPE lvc_s_layo.

CLASS lcl_layout_f4 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      for_salv
        CHANGING cv_layout TYPE disvariant-variant.
ENDCLASS.

CLASS lcl_layout_f4 IMPLEMENTATION.
  METHOD for_salv.

    DATA: ls_layout TYPE salv_s_layout_info,
          ls_key    TYPE salv_s_layout_key.

    ls_key-report = sy-repid.

    ls_layout = cl_salv_layout_service=>f4_layouts(
                  s_key    = ls_key
                  restrict = if_salv_c_layout=>restrict_none  ).

    cv_layout = ls_layout-layout.


  ENDMETHOD.                    "for_salv

ENDCLASS.
*--------------------Constantes--------------------------*
CONSTANTS: c_i     TYPE c VALUE 'I',
           c_h     TYPE c VALUE 'H',
           c_s     TYPE c VALUE 'S',
           c_eq    TYPE c VALUE 'EQ'  LENGTH 2,
           c_cp    TYPE c VALUE 'CP'  LENGTH 2,
           c_mx    TYPE c VALUE 'MX'  LENGTH 2,
           c_vst   TYPE c VALUE 'VST' LENGTH 3,
           c_wrx   TYPE c VALUE 'WRX' LENGTH 3,
           c_mx1   TYPE c VALUE 'MX1' LENGTH 3,
           c_15    TYPE c VALUE '15*' LENGTH 3,
           c_blart TYPE c VALUE 'RE' LENGTH 2,
           c_comf  TYPE c VALUE 'FF' LENGTH 2,
           c_w0    TYPE c  VALUE  'W0' LENGTH 2,
           c_w3    TYPE c  VALUE  'W3' LENGTH 2,
           c_w1    TYPE c  VALUE  'W1' LENGTH 2,
           c_0l    TYPE c LENGTH 2  VALUE '0L'.

FIELD-SYMBOLS: <fs_bseg_aux> TYPE any.

"parametros de entrada
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs FOR bkpf-bukrs OBLIGATORY.
  PARAMETERS : p_gjahr TYPE bkpf-gjahr OBLIGATORY.
  SELECT-OPTIONS:             p_monat FOR bkpf-monat.
  SELECT-OPTIONS: p_fecha FOR sy-datum.
  SELECT-OPTIONS s_belnr FOR bkpf-belnr.   "Insert AMP 22.12.2020
  PARAMETERS : p_pag_c RADIOBUTTON GROUP rb1.
  PARAMETERS : p_pag_s RADIOBUTTON GROUP rb1.
  PARAMETERS: p_guarda AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.


SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-110.
  PARAMETERS: P0 TYPE XFELD DEFAULT 'X' AS CHECKBOX,
              P1 TYPE XFELD DEFAULT 'X' AS CHECKBOX,
              P2 TYPE XFELD DEFAULT 'X' AS CHECKBOX,
              P3 TYPE XFELD DEFAULT 'X' AS CHECKBOX,
              P4 TYPE XFELD DEFAULT 'X' AS CHECKBOX,
              P5 TYPE XFELD DEFAULT 'X' AS CHECKBOX,
              P6 TYPE XFELD DEFAULT 'X' AS CHECKBOX,
              P7 TYPE XFELD DEFAULT 'X' AS CHECKBOX,
              P8 TYPE XFELD DEFAULT 'X' AS CHECKBOX,
              P9 TYPE XFELD DEFAULT 'X' AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-107 .
  PARAMETERS: VARIANT  LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK B3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR variant.
  lcl_layout_f4=>for_salv( CHANGING cv_layout = variant ).
  PERFORM oculta_campos.
*Sociedad
*Ejercicio
*Periodo
*Fecha del periodo este es un rango de fechas
