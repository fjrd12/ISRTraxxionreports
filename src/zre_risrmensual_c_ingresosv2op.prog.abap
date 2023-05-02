*&---------------------------------------------------------------------*
*& Include          ZTR_ISRMENSUAL_C_INGRESOS_TOP
*&---------------------------------------------------------------------*
REPORT ztr_isrmensual_c_ingresos.
TABLES: acdoca, bkpf, bseg, vbkd, vbfa.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
*hotspot click control
    METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no sender. "Sender es para saber que ALV mandó el evento
ENDCLASS. "lcl_event_handler DEFINITION



************************************************************************
*                       VARIABLES Y CONSTANTES                         *
************************************************************************
CONSTANTS: c_0l               TYPE c LENGTH 2  VALUE '0L',
           c_vacio            TYPE c LENGTH 1  VALUE ' ',
           c_zdoc_cob_c       TYPE c LENGTH 10 VALUE 'ZDOC_COB_C',
           c_dz               TYPE c LENGTH 2  VALUE 'DZ',
           c_es               TYPE c LENGTH 1  VALUE  'S',
           c_x                TYPE c LENGTH 1  VALUE 'X',
           c_i                TYPE c VALUE 'I',
           c_eq               TYPE c VALUE 'EQ'  LENGTH 2,
           c_zbancos_ingresos TYPE c LENGTH 16 VALUE 'ZBANCOS_INGRESOS',
           c_zcargo_abono     TYPE c LENGTH 12 VALUE 'ZCARGO_ABONO',
           c_zdoc_fac_cl      TYPE c LENGTH 11 VALUE 'ZDOC_FAC_CL',
           abap_on            TYPE abap_bool VALUE 'X'.

DATA: gd_layout   TYPE lvc_s_layo,
      vg_banderad TYPE i,
      v_name      TYPE  thead-tdname,
      v_object    TYPE  thead-tdobject,
      vg_total    TYPE acdoca-tsl,
      gv_global   TYPE c,
      gv_resumen  TYPE c,
      vg_bandera2 TYPE i,
      vg_document TYPE bkpf-belnr,
      vg_tsl      TYPE acdoca-tsl,
      vg_wsl      TYPE acdoca-wsl,
      vg_belnr    TYPE acdoca-belnr.
 DATA lv_test(1)."ARODEA
************************************************************************
*                      ESTRUCTURAS LOCALES                             *
************************************************************************
TYPES:

  BEGIN OF references,
    BUKRS TYPE BUKRS,
    BELNR  TYPE BELNR_D,
    GJAHR  TYPE GJAHR,
    XBLNR TYPE XBLNR1,
  END OF references,

  "ACDOCA
  BEGIN OF ty_cobros,
    rldnr  TYPE acdoca-rldnr,
    rbukrs TYPE acdoca-rbukrs,
    gjahr  TYPE acdoca-gjahr,
    augdt  TYPE acdoca-augdt,
    agjahr TYPE acdoca-gjahr,
    AUGGJ  TYPE acdoca-AUGGJ,
    augbl  TYPE acdoca-augbl,
    rwcur  TYPE acdoca-rwcur,
    poper  TYPE acdoca-poper,
    kunnr  TYPE acdoca-kunnr,
  END OF ty_cobros,

  BEGIN OF ty_extracto,
    bukrs TYPE bkpf-bukrs,
    belnr TYPE bkpf-belnr,
    gjahr TYPE bkpf-gjahr,
    sgtxt TYPE bseg-sgtxt,
    hbkid TYPE bseg-hbkid,
    hktid TYPE bseg-hktid,
    rwcur TYPE acdoca-rwcur,
    xblnr TYPE bkpf-xblnr,
    budat TYPE bkpf-budat,
    augbl TYPE bseg-augbl,
    augdt TYPE bseg-augdt,
    kursf TYPE bkpf-kursf,
  END OF ty_extracto,

  BEGIN OF ty_detcobro,
    rldnr      TYPE acdoca-rldnr,
    rbukrs     TYPE acdoca-rbukrs,
    gjahr      TYPE acdoca-gjahr,
    belnr      TYPE acdoca-belnr,
    docln      TYPE acdoca-docln,
    ryear      TYPE acdoca-ryear,
    aworg      TYPE acdoca-aworg,
    awref      TYPE acdoca-awref,
    rtcur      TYPE acdoca-rtcur,
    racct      TYPE acdoca-racct,
    prctr      TYPE acdoca-prctr,
    kokrs      TYPE acdoca-kokrs,
    tsl        TYPE acdoca-tsl,
    hsl        TYPE acdoca-hsl,
    wsl        TYPE acdoca-wsl,
    drcrk      TYPE acdoca-drcrk,
    budat      TYPE acdoca-budat,
    bldat      TYPE acdoca-bldat,
    blart      TYPE acdoca-blart,
    bschl      TYPE acdoca-bschl,
    linetype   TYPE acdoca-linetype,
    ktosl      TYPE acdoca-ktosl,
    ktopl      TYPE acdoca-ktopl,
    kunnr      TYPE acdoca-kunnr,
    mwskz      TYPE acdoca-mwskz,
    hbkid      TYPE acdoca-hbkid,
    hktid      TYPE acdoca-hktid,
    augdt      TYPE acdoca-augdt,
    augbl      TYPE acdoca-augbl,
    auggj      TYPE acdoca-auggj,
    gkont      TYPE acdoca-gkont,
    rbudget_pd TYPE acdoca-rbudget_pd,
  END OF ty_detcobro,

  BEGIN OF ty_fac_venta,
    rldnr  TYPE acdoca-rldnr,
    rbukrs TYPE acdoca-rbukrs,
    belnr  TYPE acdoca-belnr,
    gjahr  TYPE acdoca-gjahr,
    blart  TYPE acdoca-blart,
    budat  TYPE acdoca-budat,
    augdt  TYPE acdoca-augdt,
    augbl  TYPE acdoca-augbl,
    AUGGJ  TYPE acdoca-AUGGJ,
    kunnr  TYPE acdoca-kunnr,
  END OF ty_fac_venta,

  BEGIN OF ty_detfacturas,
    rldnr      TYPE acdoca-rldnr,
    rbukrs     TYPE acdoca-rbukrs,
    gjahr      TYPE acdoca-gjahr,
    belnr      TYPE acdoca-belnr,
    docln      TYPE acdoca-docln,
    ryear      TYPE acdoca-ryear,
    rwcur      TYPE acdoca-rwcur,
    aworg      TYPE acdoca-aworg,
    awref      TYPE acdoca-awref,
    rtcur      TYPE acdoca-rtcur,
    racct      TYPE acdoca-racct,
    prctr      TYPE acdoca-prctr,
    kokrs      TYPE acdoca-kokrs,
    tsl        TYPE acdoca-tsl,
    hsl        TYPE acdoca-hsl,
    wsl        TYPE acdoca-wsl,
    drcrk      TYPE acdoca-drcrk,
    budat      TYPE acdoca-budat,
    bldat      TYPE acdoca-bldat,
    blart      TYPE acdoca-blart,
    bschl      TYPE acdoca-bschl,
    linetype   TYPE acdoca-linetype,
    ktosl      TYPE acdoca-ktosl,
    ktopl      TYPE acdoca-ktopl,
    kunnr      TYPE acdoca-kunnr,
    mwskz      TYPE acdoca-mwskz,
    hbkid      TYPE acdoca-hbkid,
    hktid      TYPE acdoca-hktid,
    augdt      TYPE acdoca-augdt,
    augbl      TYPE acdoca-augbl,
    auggj      TYPE acdoca-auggj,
    gkont      TYPE acdoca-gkont,
    rbudget_pd TYPE acdoca-rbudget_pd,
  END OF ty_detfacturas,

  BEGIN OF ty_tvarvc,
    low TYPE tvarvc-low,
  END OF ty_tvarvc,

  BEGIN OF ty_fac_acree,
    rldnr      TYPE acdoca-rldnr,
    rbukrs     TYPE acdoca-rbukrs,
    gjahr      TYPE acdoca-gjahr,
    belnr      TYPE acdoca-belnr,
    docln      TYPE acdoca-docln,
    rwcur      TYPE acdoca-rwcur,
    ryear      TYPE acdoca-ryear,
    aworg      TYPE acdoca-aworg,
    awref      TYPE acdoca-awref,
    rtcur      TYPE acdoca-rtcur,
    racct      TYPE acdoca-racct,
    prctr      TYPE acdoca-prctr,
    kokrs      TYPE acdoca-kokrs,
    tsl        TYPE acdoca-tsl,
    wsl        TYPE acdoca-wsl,
    hsl        TYPE acdoca-hsl,
    budat      TYPE acdoca-budat,
    bldat      TYPE acdoca-bldat,
    blart      TYPE acdoca-blart,
    bschl      TYPE acdoca-bschl,
    drcrk      TYPE acdoca-drcrk,
    linetype   TYPE acdoca-linetype,
    ktosl      TYPE acdoca-ktosl,
    ktopl      TYPE acdoca-ktopl,
    kunnr      TYPE acdoca-kunnr,
    mwskz      TYPE acdoca-mwskz,
    hbkid      TYPE acdoca-hbkid,
    hktid      TYPE acdoca-hktid,
    augdt      TYPE acdoca-augdt,
    augbl      TYPE acdoca-augbl,
    auggj      TYPE acdoca-auggj,
    gkont      TYPE acdoca-gkont,
    rbudget_pd TYPE acdoca-rbudget_pd,
  END OF ty_fac_acree,

  BEGIN OF ty_cartaporte,
    bukrs TYPE bkpf-bukrs,
    belnr TYPE bkpf-belnr,
    gjahr TYPE bkpf-gjahr,
    awkey TYPE bkpf-awkey,
    bstkd TYPE vbkd-bstkd,
  END OF ty_cartaporte,


  BEGIN OF ty_iva,
    bukrs TYPE bseg-bukrs,
    belnr TYPE bseg-belnr,
    gjahr TYPE bseg-gjahr,
    sgtxt TYPE bseg-sgtxt,
    hkont TYPE bseg-hkont,
    BSCHL TYPE bseg-bschl,
    KOART TYPE bseg-koart,
    MWSKZ  TYPE bseg-MWSKZ,
    QSSKZ  TYPE QSSKZ,
    DMBTR  TYPE DMBTR,
    WRBTR  TYPE WRBTR,
    KZBTR  TYPE KZBTR_FI,
    PSWBT  TYPE PSWBT,
  END OF ty_iva,
  BEGIN OF ty_detalle,
    bukrs TYPE bseg-bukrs,
    belnr TYPE bseg-belnr,
    gjahr TYPE bseg-gjahr,
    sgtxt TYPE bseg-sgtxt,
    hkont TYPE bseg-hkont,
    BSCHL TYPE bseg-bschl,
    KOART TYPE bseg-koart,
    MWSKZ  TYPE bseg-MWSKZ,
    QSSKZ  TYPE QSSKZ,
    DMBTR  TYPE DMBTR,
    WRBTR  TYPE WRBTR,
    KZBTR  TYPE KZBTR_FI,
    PSWBT  TYPE PSWBT,
    AUGBL  TYPE AUGBL,
    AUGGJ  TYPE AUGGJ,
    budat  TYPE BUDAT,
    kunnr  TYPE KUNNR,
    kursf  type kursf,
    ktosl  type bseg-ktosl,
  END OF ty_detalle,

  BEGIN OF ty_report,
    zsema            TYPE char4,
    zfec_cobro       TYPE acdoca-budat,
    zref_cob         TYPE bkpf-xblnr,
    zsociedad        TYPE acdoca-rbukrs,
    zejercicio       TYPE acdoca-gjahr,
    zperiodo         TYPE acdoca-poper,
    zdescrip         TYPE bseg-sgtxt,
    zcobro_me        TYPE acdoca-wsl,
    zcobro_mn        TYPE acdoca-tsl,
    zper_real        TYPE acdoca-hsl,
    zutil_real       TYPE acdoca-hsl,
    zbanco_cobro     TYPE t012t-text1,
    zdocto_cobro     TYPE bkpf-belnr,
    zejer_cobro      TYPE acdoca-auggj,
    ztc_cobro        TYPE bkpf-kursf,
    zfec_factura     TYPE acdoca-budat,
    zdocto_factura   TYPE bkpf-belnr,
    zasig            TYPE bkpf-xblnr,
    ztc_factura      TYPE bkpf-kursf,
    zno_cliente      TYPE acdoca-kunnr,
    zdes_cliente     TYPE char255,
    zrfc_cliente     TYPE dfkkbptaxnum-taxnumxl,
    zuuid            TYPE tdline,
    zcarta_porte     TYPE vbkd-bstkd,
    zclase_docto     TYPE acdoca-blart,
    zimporte_fact_me TYPE acdoca-wsl,
    zimporte_fac_mn  TYPE acdoca-tsl,
    zdes_ingreso     TYPE skat-txt50,
    zimp_base_16     TYPE acdoca-wsl,
    zind_16          TYPE acdoca-mwskz,
    zimp_base_exc    TYPE acdoca-tsl,
    zind_exc         TYPE acdoca-mwskz,
    zimp_base_0      TYPE acdoca-tsl,
    zind_imp         TYPE acdoca-mwskz,
    ziva_al_16       TYPE acdoca-tsl,
    ziva_ret_4       TYPE acdoca-tsl,
    ziva_ret_6       TYPE acdoca-tsl,
    ZOTR_INGR        TYPE acdoca-tsl,
    ZOTR_DEPOS       TYPE acdoca-tsl,
    ztotal           TYPE acdoca-tsl,
  END OF ty_report,

  BEGIN OF ty_res_ctas,
    rbukrs  TYPE acdoca-rbukrs,
    belnr   TYPE acdoca-belnr,
    gjahr   TYPE acdoca-gjahr,
    mwskz   TYPE acdoca-mwskz,
    racct   TYPE acdoca-racct,
    wsl     TYPE acdoca-wsl,
    tsl     TYPE acdoca-tsl,
    hsl     TYPE acdoca-hsl,
    des_cta TYPE skat-txt50,
  END OF ty_res_ctas,

  begin of ty_traspasos,
    RLDNR  type acdoca-RLDNR,
    RBUKRS type acdoca-RBUKRS,
    GJAHR  type acdoca-GJAHR,
    POPER  TYPE acdoca-poper,
    BLART  type acdoca-BLART,
    BUDAT  type acdoca-BUDAT,
    BLDAT  type acdoca-BLDAT,
    BELNR  type acdoca-BELNR,
    DOCLN  type acdoca-DOCLN,
    RACCT  type acdoca-RACCT,
    TSL    type acdoca-TSL,
    SGTXT   type acdoca-SGTXT,
    WSL    type acdoca-wsl,
    RWCUR  type acdoca-rwcur,
    hsl    type acdoca-hsl,
    hbkid  type acdoca-hbkid,
    hktid  type acdoca-hktid,
    awkey  type bkpf-awkey,
    xblnr  type bkpf-xblnr,
    augbl  type acdoca-augbl,
    auggj  type acdoca-auggj,
    augdt  type acdoca-augdt,
    kunnr  type acdoca-kunnr,
    kursf  type bkpf-kursf,
  end of ty_traspasos,
  tdfkkbptaxnum type STANDARD TABLE OF dfkkbptaxnum,
  tbut000       type STANDARD TABLE OF but000,
  tty_report    type STANDARD TABLE OF ty_report,
  ttraspaso     type STANDARD TABLE OF TY_TRASPASOS WITH NON-UNIQUE SORTED KEY CLAVE1 COMPONENTS GJAHR belnr RBUKRS racct
                                                    WITH NON-UNIQUE SORTED KEY CLAVE2 COMPONENTS rldnr RBUKRS gjahr BELNR
                                                    WITH NON-UNIQUE SORTED KEY CLAVE3 COMPONENTS RBUKRS BELNR GJAHR,
  treferences   type STANDARD TABLE OF references,
  tt012t        type STANDARD TABLE OF t012t,
  tty_iva       type STANDARD TABLE OF ty_iva WITH NON-UNIQUE SORTED KEY CLAVE1 COMPONENTS BUKRS GJAHR BELNR,
  tty_detalle   type STANDARD TABLE OF ty_detalle.

************************************************************************
*                 TABLAS INTERNAS, AREAS DE TRABAJO Y RANGOS           *
************************************************************************
DATA: "Conceptos generales
      it_ing_conceptos      TYPE ttraspaso,
      "Traspaso
      it_tr_origen          TYPE ttraspaso,
      it_tr_destino         TYPE ttraspaso,
      "Ingresos por recuperación
      it_ing_tr_origen      TYPE ttraspaso,
      it_ing_tr_destino     TYPE ttraspaso,
      it_ing_IVA            TYPE tty_iva,
      "Intereses ganados
      it_int_origen         TYPE ttraspaso,
      it_int_origen2        TYPE ttraspaso,
      it_int_destino        TYPE ttraspaso,
      it_int_destino2       TYPE ttraspaso,
      it_int_compensa       TYPE ttraspaso,
      it_int_compensa2      TYPE ttraspaso,
      it_int_isr            TYPE ttraspaso,
      "Inversiones
      it_inv_origen         TYPE ttraspaso,
      it_inv_destino        TYPE ttraspaso,

      "Intercos
      it_interco_origen    TYPE ttraspaso,
      it_interco_destino   TYPE ttraspaso,

      "Otros ingresos
      it_otring_origen     TYPE ttraspaso,
      it_otring_destino    TYPE ttraspaso,

      "Otros Depositos
      it_otrdep_origen     TYPE ttraspaso,
      it_otrdep_destino    TYPE ttraspaso,

      "Factoraje Santander
      it_factsan_origen     TYPE ttraspaso,
      it_factsan_destino    TYPE ttraspaso,

      "Factoraje sencillo
      it_fact_pago          TYPE ttraspaso,
      it_fact_pago_detalle  TYPE ttraspaso,
      it_fact_intermedio    TYPE ttraspaso,
      it_fact_compensados   TYPE ttraspaso,
      it_fact_detalle       TYPE tty_detalle,

      it_cobros             TYPE STANDARD TABLE OF ty_cobros,
      it_extracto           TYPE STANDARD TABLE OF ty_extracto,
      it_sin_extracto       TYPE STANDARD TABLE OF ty_extracto,
      it_detcobro           TYPE STANDARD TABLE OF ty_detcobro,
      it_detcobro2          TYPE STANDARD TABLE OF ty_detcobro,
      it_fac_venta          TYPE STANDARD TABLE OF ty_fac_venta,
      it_detfacturas        TYPE STANDARD TABLE OF ty_detfacturas,
      it_carta_porte        TYPE STANDARD TABLE OF ty_cartaporte,
      it_tvar_zdoc          TYPE STANDARD TABLE OF ty_tvarvc,
      it_zbancos_ingresos   TYPE STANDARD TABLE OF ty_tvarvc,
      it_zcargo_abono       TYPE STANDARD TABLE OF ty_tvarvc,
      it_zdoc_fac_cl        TYPE STANDARD TABLE OF ty_tvarvc,
      it_bru_fac_acree      TYPE STANDARD TABLE OF ty_fac_acree,
      it_ing_fac_acree      TYPE STANDARD TABLE OF ty_fac_acree,
      it_iva_fac_acree      TYPE STANDARD TABLE OF ty_fac_acree,
      it_ret_fac_acree      TYPE STANDARD TABLE OF ty_fac_acree,
      it_reporte            TYPE tty_report,
      it_bseg               TYPE STANDARD TABLE OF bseg,
      it_bkpf               TYPE STANDARD TABLE OF bkpf,
      it_t012t              TYPE tt012t,
      it_dfkkbptaxnum       TYPE tdfkkbptaxnum,
      it_dfkkbptaxnum2      TYPE STANDARD TABLE OF dfkkbptaxnum,
      it_line               TYPE TABLE OF tline,
      it_but000             TYPE tbut000,
      it_skat               TYPE STANDARD TABLE OF skat,
      it_res_ctas           TYPE STANDARD TABLE OF ty_res_ctas,
      tl_ztax_detalle_ing   TYPE TABLE OF ztax_detalle_ing,
      it_ing_fac_acree_aux  TYPE STANDARD TABLE OF ty_fac_acree,
      it_bkpf_fac_ven       TYPE STANDARD TABLE OF bkpf,
      it_bkpf_fac           TYPE STANDARD TABLE OF bkpf,
      it_reporte_aux        TYPE STANDARD TABLE OF ty_report,

      wa_cobros             TYPE ty_cobros,
      wa_extracto           TYPE ty_extracto,
      wa_sin_extracto       TYPE ty_extracto,
      wa_detcobro           TYPE ty_detcobro,
      wa_fac_venta          TYPE ty_fac_venta,
      wa_detfacturas        TYPE ty_detfacturas,
      wa_carta_porte        TYPE ty_cartaporte,
      wa_tvar_zdoc          TYPE ty_tvarvc,
      wa_zbancos_ingresos   TYPE ty_tvarvc,
      wa_zcargo_abono       TYPE ty_tvarvc,
      wa_zdoc_fac_cl        TYPE ty_tvarvc,
      wa_bru_fac_acree      TYPE ty_fac_acree,
      wa_ing_fac_acree      TYPE ty_fac_acree,
      wa_iva_fac_acree      TYPE ty_fac_acree,
      wa_ret_fac_acree      TYPE ty_fac_acree,
      wa_reporte            TYPE ty_report,
      wa_res_ctas           TYPE ty_res_ctas,
      wa_column_id          TYPE lvc_s_col,
      wa_row_no             TYPE lvc_s_roid,
      lo_grid               TYPE REF TO cl_gui_alv_grid,
      lo_container          TYPE REF TO cl_gui_custom_container,
      lo_event_handler      TYPE REF TO lcl_event_handler, "Este es para ambos reportes
      wa_variant            TYPE disvariant,
      wa_layout             TYPE lvc_s_layo,
      rg_origen_concen      TYPE RANGE OF bseg-hkont,
      rg_tdoc_intereses     TYPE RANGE OF acdoca-blart,
      rg_bancos_ingresos    TYPE RANGE OF bseg-hkont,
      rg_doc_fac_cl         TYPE RANGE OF acdoca-blart,
      rg_tvar_doc           TYPE RANGE OF acdoca-blart,
      rg_cuentas_origen     TYPE RANGE OF acdoca-racct,
      rg_cuentas_destino    type RANGE OF acdoca-racct,
      rg_cargo_abono        TYPE RANGE OF bseg-bschl,
      rg_abono_cargo        TYPE RANGE OF acdoca-bschl,
      rg_abono_cargo_high   TYPE RANGE OF acdoca-bschl,
      rg_clave_deudor       TYPE RANGE OF acdoca-linetype,
      rg_clave_ing          TYPE RANGE OF acdoca-linetype,
      rg_ctas_ing_vta       TYPE RANGE OF acdoca-racct,
      rg_clave_iva          TYPE RANGE OF acdoca-linetype,
      rg_ctas_iva_ing       TYPE RANGE OF acdoca-racct,
      rg_clave_ret_iva      TYPE RANGE OF acdoca-linetype,
      rg_ctas_ret_iva_ing   TYPE RANGE OF acdoca-racct,
      rg_cuentas_ing_ban    TYPE RANGE OF acdoca-racct,
      rg_ctas_comision      TYPE RANGE OF acdoca-racct,
      rg_clave_comision     TYPE RANGE OF acdoca-linetype,
      rg_clave_balance      TYPE RANGE OF acdoca-linetype,
      rg_cuentas_per_rel    TYPE RANGE OF acdoca-racct,
      rg_clave_util_rea     TYPE RANGE OF acdoca-linetype,
      rg_cuentas_util_rel   TYPE RANGE OF acdoca-racct,
      rg_base_16            TYPE RANGE OF acdoca-mwskz,
      rg_base_exc           TYPE RANGE OF acdoca-mwskz,
      rg_ctas_iva_vta       TYPE RANGE OF acdoca-racct,
      rg_ctas_ret_iva_ing_4 TYPE RANGE OF acdoca-racct,
      rg_base_0             TYPE RANGE OF acdoca-mwskz,
      rg_stufe              TYPE RANGE OF vbfa-stufe,
      "Documentos de traspaso
      rg_docs_traspaso      TYPE RANGE OF acdoca-blart,
      "Recuperaciones
      rg_docs_ing_recup     TYPE RANGE OF acdoca-blart,
      rg_docs_cta_ingrec    TYPE RANGE OF acdoca-racct,
      "Intereses ganados
      rg_docs_intereses     TYPE RANGE OF acdoca-blart,
      rg_cta_pte            TYPE RANGE OF acdoca-racct,
      rg_cta_intereses      TYPE RANGE OF acdoca-racct,
      rg_cta_interesesisr   TYPE RANGE OF acdoca-racct,
      "Inversiones
      rg_doc_inver          TYPE RANGE OF acdoca-blart,
      rg_doc_inver_conc     TYPE RANGE OF acdoca-racct,
      "Prestámos intercos
      rg_doc_intercos       TYPE RANGE OF acdoca-blart,
      rg_ctas_intercos      TYPE RANGE OF acdoca-racct,
      "Otros ingresos
      rg_ctas_otring        TYPE RANGE OF acdoca-racct,
      "Otros Depositos
      rg_ctas_otdep         TYPE RANGE OF acdoca-racct,
      "Factoraje Santander España
      rg_fact_sanesp        TYPE RANGE OF acdoca-racct,
      rg_fact_santrans      TYPE RANGE OF acdoca-racct,
      "Factoraje simple
      rg_doc_pago           TYPE RANGE OF acdoca-blart,
      rg_compensa_factor    TYPE RANGE OF acdoca-blart,
      "Notas de crédito
      rg_nc_clase           TYPE RANGE OF acdoca-blart,
      rg_desc_reb           TYPE RANGE OF acdoca-racct,
      tl_fieldcat           TYPE lvc_t_fcat,
      GX_VARIANT            TYPE DISVARIANT,
      G_SAVE                TYPE C VALUE 'X',
      G_VARIANT             TYPE DISVARIANT.


FIELD-SYMBOLS: <fs_detfacturas>   TYPE ty_detfacturas,
               <fs_cobros>        TYPE ty_cobros,
               <fs_bkpf_extracto> TYPE bkpf,
               <fs_report_final>  TYPE ty_report.

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

SELECTION-SCREEN     BEGIN OF BLOCK inicio WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: sociedad  FOR acdoca-rbukrs OBLIGATORY.
  PARAMETERS:     ejerc     TYPE gjahr OBLIGATORY.
  SELECT-OPTIONS: periodo   FOR acdoca-poper.
  SELECT-OPTIONS: fechap    FOR acdoca-budat.
  SELECT-OPTIONS: augbl     FOR acdoca-augbl.
  PARAMETERS: r1 RADIOBUTTON GROUP rad1,
              r2 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK inicio.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-048 .
  PARAMETERS: VARIANT LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR variant.
  lcl_layout_f4=>for_salv( CHANGING cv_layout = variant ).

"ARODEA----->R-005385-Error en reporte de ingresos Egoba   01/01/01/2023
" Al término del año se reinician los rangos y en campo linetype se encuentra vacío
"por lo que este campo es esencial para las validaciones del reporte

"Se crea esta clase de utilidades y herramientas que ayuden al proceso

CLASS lcl_isr_tools DEFINITION.

  PUBLIC SECTION.
    CLASS-DATA bkpf type bkpf.
    CLASS-DATA treferences type treferences.

    METHODS fix_linetype
      IMPORTING
        !it_facturas LIKE it_detfacturas OPTIONAL
        !it_dcobro   LIKE it_detcobro OPTIONAL
      EXPORTING
        !et_facturas LIKE it_detfacturas
        !et_dcobro   LIKE it_detcobro.
    CLASS-METHODS map_client_data
      IMPORTING
        kunnr            type kunnr
        it_dfkkbptaxnum1 TYPE tdfkkbptaxnum
        it_but0001       TYPE tbut000
      EXPORTING
        wa_reporte1      TYPE ty_report.

    CLASS-METHODS map_reference
      IMPORTING
          GJAHR  TYPE GJAHR
          BELNR  TYPE BELNR_D
          RBUKRS TYPE  BUKRS
      EXPORTING
          XBLNR TYPE XBLNR1.

    CLASS-METHODS map_bank_name
      IMPORTING
          BUKRS TYPE  BUKRS
          HBKID TYPE  HBKID
          HKTID TYPE  HKTID
      EXPORTING
          TEXT1  TYPE FIBL_TXT50
      CHANGING
          it_t012t TYPE tt012t.
ENDCLASS.

CLASS lcl_isr_tools IMPLEMENTATION.
  method map_bank_name.
  DATA T012T type T012T.

    READ TABLE it_t012t
     ASSIGNING FIELD-SYMBOL(<fs_t012t>)
     WITH KEY spras = c_es
              bukrs = bukrs
              hbkid = hbkid
              hktid = hktid
      BINARY SEARCH.
   IF sy-subrc EQ 0.
      TEXT1 = <fs_t012t>-text1. "Banco del cobro
   else.
     SELECT single * from T012T
       into T012T
       where SPRAS = c_es and
             bukrs = bukrs and
             hbkid = hbkid and
             hktid = hktid.
    if sy-subrc = 0.
      TEXT1 = t012t-text1.
      append t012t to it_t012t.
      sort it_t012t.
    endif.

   ENDIF.
  endmethod.

  method map_reference.
    data: wreferences type references.
    read table treferences into wreferences with key BUKRS = RBUKRS
                                                     BELNR = BELNR
                                                     GJAHR = GJAHR.
    if sy-subrc = 0.

      xblnr = wreferences-xblnr.

    else.

      select single xblnr into wreferences-xblnr
      from bkpf
      where BUKRS = RBUKRS and
            BELNR = BELNR and
            GJAHR = GJAHR.

      if sy-subrc = 0.
           wreferences-BUKRS = wreferences-BUKRS.
           wreferences-BELNR = wreferences-BELNR.
           wreferences-GJAHR = wreferences-GJAHR.
           wreferences-XBLNR = wreferences-XBLNR.
           append wreferences to treferences.
           sort treferences.
      endif.

    endif.

  ENDMETHOD.
  METHOD map_client_data.
*    FIELD-SYMBOLS: <fs_dfkkbptaxnum2> type dfkkbptaxnum.
    READ TABLE it_dfkkbptaxnum1
         ASSIGNING FIELD-SYMBOL(<fs_dfkkbptaxnum2>)
         WITH KEY partner = kunnr
         BINARY SEARCH.
         IF sy-subrc EQ 0.
           IF NOT <fs_dfkkbptaxnum2>-taxnum IS INITIAL.
             wa_reporte1-zrfc_cliente = <fs_dfkkbptaxnum2>-taxnum. "RFC
           ELSEIF NOT <fs_dfkkbptaxnum2>-taxnumxl IS INITIAL. "RFC
             wa_reporte1-zrfc_cliente = <fs_dfkkbptaxnum2>-taxnumxl. "RFC
           ENDIF.
         ENDIF.

         READ TABLE it_but000
          ASSIGNING FIELD-SYMBOL(<fs_but000>)
             WITH KEY partner = kunnr
             BINARY SEARCH.
         IF sy-subrc EQ 0.
           IF NOT <fs_but000>-name_org1 IS INITIAL OR <fs_but000>-name_org2 IS NOT INITIAL
              OR <fs_but000>-name_org3 IS  NOT INITIAL OR <fs_but000>-name_org4 IS NOT INITIAL.
             CONCATENATE <fs_but000>-name_org1 <fs_but000>-name_org2 <fs_but000>-name_org3 <fs_but000>-name_org4
             INTO wa_reporte1-zdes_cliente. "Nombre del cliente
           ELSEIF NOT <fs_but000>-name_last IS INITIAL  OR <fs_but000>-name_first IS NOT INITIAL.
             CONCATENATE <fs_but000>-name_last <fs_but000>-name_first INTO wa_reporte1-zdes_cliente. "Nombre del cliente
           ENDIF.
         ENDIF.
  ENDMETHOD.

  METHOD fix_linetype.
    DATA:BEGIN OF ls_t8g,
      accountfro TYPE saknr,
      accountto  TYPE saknr,
      linetype   TYPE linetype,
    END OF ls_t8g.
    DATA it_facturas_a LIKE it_detfacturas.
    DATA it_dcobro_a   LIKE it_detcobro.
    DATA tr_saknr TYPE RANGE OF saknr.
    CONSTANTS lc_gptx(4) VALUE 'GPTX'.
    FIELD-SYMBOLS <fs_t8g17> LIKE ls_t8g.


CLEAR ls_t8g.
REFRESH:
*        et_facturas,
*        et_dcobro,
        tr_saknr,
        it_facturas_a ,
        it_dcobro_a  .

    it_facturas_a[] = it_facturas[].
    it_dcobro_a[]   = it_dcobro[].
    "Consulta los tipos de posición de la cuenta
    SELECT
      accountfro,
       accountto,
      linetype
      FROM t8g17
      INTO TABLE @DATA(it_t8g17)
      WHERE
        chartofacc EQ @lc_gptx.

"Determina que tabla es  la que se va a ajustar
    IF it_facturas_a IS NOT INITIAL.
      LOOP AT it_facturas_a ASSIGNING FIELD-SYMBOL(<fs_fact>)."recorre la tabla de las facturas
        IF <fs_fact>-linetype is initial."si es inicial lo busca
        LOOP AT it_t8g17 ASSIGNING <fs_t8g17>.
*          Por cada registro en la tabla de tipo de posición crea un rango y determina si la cuenta de la factura pertenece a ese rango
          tr_saknr = VALUE #(
          BASE  tr_saknr ( sign  = 'I'  option = 'BT'  low = <fs_t8g17>-accountfro   high = <fs_t8g17>-accountto ) ).

            IF <fs_fact>-racct IN tr_saknr. "Si el número de cuenta pertenece a este rango asigna el número de tipo de posición
            <fs_fact>-linetype = <fs_t8g17>-linetype.
            REFRESH: tr_saknr.
            EXIT."En cuantro lo encuentra termina la iteración y limpia la tabla
          ENDIF.
          REFRESH: tr_saknr.
        ENDLOOP.
         ENDIF.
      ENDLOOP.
      et_facturas[] = it_facturas_a[].
    ELSE.
      LOOP AT it_dcobro_a ASSIGNING FIELD-SYMBOL(<fs_dcobro>)."recorre la tabla de cobro
        IF <fs_dcobro>-linetype is initial."si es inicial lo busca
        LOOP AT it_t8g17 ASSIGNING <fs_t8g17>.
*          Por cada registro en la tabla de tipo de posición crea un rango y determina si la cuenta de la factura pertenece a ese rango
          tr_saknr = VALUE #(
          BASE  tr_saknr ( sign  = 'I'  option = 'BT'  low = <fs_t8g17>-accountfro   high = <fs_t8g17>-accountto ) ).
            IF <fs_dcobro>-racct IN tr_saknr. "Si pertenece a este rango asigna el número de tipo de posición
            <fs_dcobro>-linetype = <fs_t8g17>-linetype.
            REFRESH: tr_saknr.
            EXIT."En cuantro lo encuentra termina la iteración y limpia la tabla
          ENDIF.
          REFRESH: tr_saknr.
        ENDLOOP.
        endif.
      ENDLOOP.
      et_dcobro[] = it_dcobro_a[].
    ENDIF.


  ENDMETHOD.
ENDCLASS.
""ARODEA----->R-005385-Error en reporte de ingresos Egoba 01/01/01/2023
