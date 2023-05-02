FUNCTION ZFI_GET_REEMBOLSO_DATA.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(SOCIEDAD) TYPE  RSIS_T_RANGE
*"     REFERENCE(EJERC) TYPE  GJAHR
*"     REFERENCE(PERIODO) TYPE  RSIS_T_RANGE
*"     REFERENCE(AUGBL) TYPE  RSIS_T_RANGE OPTIONAL
*"  EXPORTING
*"     REFERENCE(REPORT_OUTPUT) TYPE  ZFI_EGRESOS_OUTPUTT
*"----------------------------------------------------------------------

***Definic
  data: it_paso1       TYPE ZFI_TRASPASOST, "Pago
        it_paso2       TYPE ZFI_TRASPASOST, "Pago partida de Banco
        it_paso3a      TYPE ZFI_TRASPASOST, "Documentos compensados unicos
        it_paso3all    TYPE ZFI_TRASPASOST, "Documentos compensados unicos
        it_paso3       TYPE ZFI_TRASPASOST, "Documento compensado completo
        it_paso4       TYPE ZFI_TRASPASOST, "Partidas compensadas gastos
        it_paso5       TYPE ZFI_TRASPASOST, "Partidas compensadas impuestos
        REPORT_OUTPUTL TYPE ZFI_EGRESOS_OUTPUT,
        REPORT_OUTPUTA TYPE  ZFI_EGRESOS_OUTPUTT,
        vl_NAME        TYPE THEAD-TDNAME,
        TL_LINES       TYPE STANDARD TABLE OF TLINE,
        it_egresos     TYPE ZFI_TRASPASOST,
        it_pagos       TYPE ZFI_TRASPASOST,
        it_impuestos   TYPE ZFI_TRASPASOST,
        it_acreedores  TYPE ZFI_TRASPASOST,
        tabix          TYPE SY-tabix.


  data: rg_ctas_egreso                 type RSIS_T_RANGE,
        rg_pos_egreso                  type RSIS_T_RANGE,
        rg_pos_impuesto                type RSIS_T_RANGE,
        rg_ziva_16                     type RSIS_T_RANGE,
        rg_ziva_cta16                  type RSIS_T_RANGE,
        rg_ziva_ind16                  type RSIS_T_RANGE,
        rg_ziva_8                      type RSIS_T_RANGE,
        rg_ziva_cta8                   type RSIS_T_RANGE,
        rg_ziva_ind8                   type RSIS_T_RANGE,
        rg_ziva_ind0                   type RSIS_T_RANGE,
        rg_ziva_0                      type RSIS_T_RANGE,
        rg_zret_iva_ret_sub_arren      TYPE RSIS_T_RANGE,
        rg_zret_iva_ret_sub_honorarios TYPE RSIS_T_RANGE,
        rg_zret_iva_ret_sub_contra     TYPE RSIS_T_RANGE,
        rg_zret_iva_ret_sub_fletes     TYPE RSIS_T_RANGE,
        rg_imp_ret_cedular             TYPE RSIS_T_RANGE,
        rg_zret_ret_isr_honorarios     TYPE RSIS_T_RANGE,
        rg_zret_ret_isr_arrendamiento  TYPE RSIS_T_RANGE,
        rg_ret_isr_pagext              TYPE RSIS_T_RANGE,
        range_wa                       type RSIS_S_RANGE,
        range_wa2                      type RSIS_S_RANGE,
        rg_nodeduc                     type RSIS_T_RANGE,
        rg_prov_amex                   type RSIS_T_RANGE,
        rg_docs_amex                   type RSIS_T_RANGE,
        rg_docs_comproba               type RSIS_T_RANGE,
        rg_docs_pagoacre               type RSIS_T_RANGE,
        valori                         type i,
        rg_ziva_nd                     type RSIS_T_RANGE.

  DATA: index      TYPE INT4.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZTR_CTAS_DESTINO'
    IMPORTING
      RANGE = rg_ctas_egreso.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZTR_DOC_COMPRO'
    IMPORTING
      RANGE = rg_docs_comproba.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZTR_FACT_BLART'
    IMPORTING
      RANGE = rg_docs_pagoacre.

  clear: REPORT_OUTPUT.
  PERFORM create_ranges using rg_prov_amex
                              rg_docs_amex
                              rg_ctas_egreso
                              rg_pos_egreso
                              rg_pos_impuesto
                              rg_ziva_16
                              rg_ziva_cta16
                              rg_ziva_ind16
                              rg_ziva_8
                              rg_ziva_cta8
                              rg_ziva_ind8
                              rg_ziva_ind0
                              rg_ziva_0
                              rg_zret_iva_ret_sub_arren
                              rg_zret_iva_ret_sub_honorarios
                              rg_zret_iva_ret_sub_contra
                              rg_zret_iva_ret_sub_fletes
                              rg_imp_ret_cedular
                              rg_zret_ret_isr_honorarios
                              rg_zret_ret_isr_arrendamiento
                              rg_ret_isr_pagext
                              range_wa
                              range_wa2
                              rg_nodeduc
                              rg_ziva_nd.


  perform get_data_reembolso using  it_paso1
                                    it_paso2
                                    it_paso3
                                    it_paso3all
                                    sociedad
                                    ejerc
                                    periodo
                                    augbl
                                    rg_ctas_egreso
                                    rg_docs_comproba
                                    rg_docs_pagoacre.

*  data: it_paso1       TYPE ZFI_TRASPASOST, "Pago
*        it_paso2       TYPE ZFI_TRASPASOST, "Pago partida de Banco
*        it_paso3       TYPE ZFI_TRASPASOST, "Documento compensado completo

  clear it_pagos.
  loop at it_paso1 into DATA(iw_data1).
    clear: iw_data1-LIFNR,
           iw_data1-kursf,
           iw_data1-awitem,
           iw_data1-racct,
           iw_data1-racct,
           iw_data1-hsl,
           iw_data1-wsl,
           iw_data1-hsl,
           iw_data1-hbkid,
           iw_data1-hktid.

    sort it_pagos.
    collect iw_data1 into it_pagos.
  ENDLOOP.

  if it_paso3 is NOT INITIAL.
    "posiciones de acreedor
    select * from @it_paso3 as documento
      where lifnr  NE ''
      into table @it_acreedores.
    clear IT_PASO3a.
    loop at it_paso3 into DATA(iw_paso3) where augbl ne ''.
      append iw_paso3 to IT_PASO3a.
    endloop.
    sort IT_PASO3A by rbukrs gjahr belnr augbl auggj.
    delete ADJACENT DUPLICATES FROM IT_PASO3A COMPARING rbukrs gjahr belnr augbl auggj.
  endif.

  if it_paso2 is NOT INITIAL.
    "posiciones de acreedor
    select * from @it_paso2 as documento
      where lifnr  NE ''
      into table @it_acreedores.


    SELECT
      spras,
      bukrs,
      hbkid,
      hktid,
      text1
 FROM t012t
 INTO TABLE @DATA(it_t012t)
  FOR ALL ENTRIES IN @it_paso2
     WHERE spras  EQ @sy-langu AND
           bukrs  EQ @it_paso2-rbukrs AND
           hbkid  EQ @it_paso2-hbkid AnD
           hktid  EQ @it_paso2-hktid.


    SELECT
          t~spras,
          t~bukrs,
          t~hbkid,
          t~hktid,
          t~text1,
          s~saknr
     FROM t012t as t
     INNER JOIN skb1 as s on s~bukrs = t~bukrs  and
                             s~hbkid = t~hbkid  and
                             s~hktid = t~hktid
     INTO TABLE @DATA(it_t012tk)
      FOR ALL ENTRIES IN @it_paso2
         WHERE t~spras  EQ @sy-langu AND
               t~bukrs  EQ @it_paso2-rbukrs AND
               s~saknr  EQ @it_paso2-racct.
  endif.

  if it_paso3all is NOT INITIAL.
*Datos de egresos
    SELECT ktopl, saknr,txt50
               INTO TABLE @DATA(tl_skat)
               FROM skat
               FOR ALL ENTRIES IN @it_paso3all
               WHERE spras = @sy-langu AND
                     ktopl = @it_paso3all-ktopl AND
                     saknr = @it_paso3all-racct.
*  endif.
**Datos de proveedores
*  if it_paso3all is NOT INITIAL.

* Busca el rfc de proveedor
    SELECT partner,taxtype,taxnum,taxnumxl
      INTO TABLE @DATA(tl_dfkkbptaxnum)
      FROM dfkkbptaxnum
      FOR ALL ENTRIES IN @it_paso3all
      WHERE partner  = @it_paso3all-lifnr AND
            taxtype  = @c_mx1.

    SELECT partner,name_org1,name_org2,name_org3,name_org4,name_last,name_first,natpers
      INTO TABLE @DATA(tl_but000)
      FROM but000
      FOR ALL ENTRIES IN @it_paso3all
      WHERE partner  = @it_paso3all-lifnr.


  loop at it_paso3all into DATA(iw_paso3all).
    tabix = sy-tabix.
    valori = iw_paso3all-awitem.
    iw_paso3all-buzei = valori.
    modify  it_paso3all from iw_paso3all INDEX tabix.
  ENDLOOP.

  endif.

**************************************** GET DATOS DE BANCOS***************************************
*Datos de Bancos
*  if it_paso2 is NOT INITIAL.



*  endif.
  DESCRIBE TABLE it_pagos LINES DATA(size_p).

****************************************GET DATOS DE BANCOS***************************************
  LOOP at it_pagos into DATA(iw_pagos).
****************************************DATOS DE BANCOS***************************************
    IF sy-batch EQ ''.
      index = sy-tabix.
*&---------------------------------------------------------------------*
*&    FUNCTION  PROGRESS DIALOG
*&---------------------------------------------------------------------*
     CALL FUNCTION 'PROGRESS_DIALOG'
        EXPORTING
          SIZE_LIST = size_p
          INDEX_PROGRESS = index
          TEXT_BEFORE = 'Procesando reembolso'.

    ENDIF.

    clear REPORT_OUTPUTL.
    read TABLE it_paso2 INTO DATA(iw_paso2) WITH key rldnr  = iw_pagos-rldnr
                                                     rbukrs = iw_pagos-rbukrs
                                                     gjahr  = iw_pagos-gjahr
                                                     belnr  = iw_pagos-belnr.


    if sy-subrc = 0.
      read TABLE it_t012t into DATA(iw_t012t) WITH key   spras  = sy-langu
                                 bukrs  = iw_paso2-rbukrs
                                 hbkid  = iw_paso2-hbkid
                                 hktid  = iw_paso2-hktid.
      if sy-subrc = 0.
        REPORT_OUTPUTL-ZBANCO_COBRO = iw_t012t-text1.
      else.
        read TABLE it_t012tk into DATA(iw_t012tk) WITH key  spras  = sy-langu
                                                            bukrs  = iw_paso2-rbukrs
                                                            saknr  = iw_paso2-racct.
        if sy-subrc = 0.
          REPORT_OUTPUTL-ZBANCO_COBRO = iw_t012tk-text1.
        endif.
      endif.
    endif.


    read TABLE it_paso1 INTO DATA(iw_paso1) WITH key rldnr  = iw_pagos-rldnr
                                                     rbukrs = iw_pagos-rbukrs
                                                     gjahr  = iw_pagos-gjahr
                                                     belnr  = iw_pagos-belnr.

    if sy-subrc = 0.
      REPORT_OUTPUTL-TOTALCARGOS = iw_paso1-hsl.
    endif.
****************************************DATOS DE BANCOS***************************************
****************************************DATOS DE DOCUMENTO DE PAGO***************************************
    perform datos_doc_pago USING REPORT_OUTPUTL
                                 iw_pagos.

    loop at it_paso3A into DATA(iw_factura) where rldnr  = iw_pagos-rldnr  and
                                                  rbukrs = iw_pagos-rbukrs and
                                                  auggj  = iw_pagos-gjahr  and
                                                  augbl  = iw_pagos-belnr.
*
****************************************DATOS DE DOCUMENTO DE FACTURA************************************
**FOLIOFISCAL
**        UUID
*      "Folio fiscal
*      CONCATENATE iw_factura-rbukrs iw_factura-belnr iw_factura-gjahr INTO vl_name.
*      CALL FUNCTION 'ZFI_ISR_READ_TEXT_BUFFER'
*        EXPORTING
*          client                  = sy-mandt
*          id                      = 'YUUD'
*          language                = 'S'
*          name                    = vl_name
*          object                  = 'BELEG'
*        TABLES
*          lines                   = tl_lines
*        EXCEPTIONS
*          id                      = 1
*          language                = 2
*          name                    = 3
*          not_found               = 4
*          object                  = 5
*          reference_check         = 6
*          wrong_access_to_archive = 7
*          OTHERS                  = 8.
*      IF sy-subrc = 0.
*        ASSIGN tl_lines[ 1 ] TO FIELD-SYMBOL(<fs_lines>).
*        IF sy-subrc = 0.
*          REPORT_OUTPUTL-FOLIOFISCAL = <fs_lines>-tdline.
*        ENDIF.
*      ENDIF.

      "Posiciones de egreso

      select
   documento~RLDNR,
   documento~RBUKRS,
   documento~GJAHR,
   documento~POPER,
   documento~BLART,
   documento~BUDAT,
   documento~BLDAT,
   documento~BELNR,
   documento~AWITEM,
   documento~RACCT,
   documento~TSL,
   documento~SGTXT,
   documento~WSL,
   documento~RWCUR,
   documento~HSL,
   documento~HBKID,
   documento~HKTID,
   documento~AWKEY,
   documento~XBLNR,
   documento~AUGBL,
   documento~AUGGJ,
   documento~AUGDT,
   documento~KUNNR,
   documento~KURSF,
   documento~LIFNR,
   documento~KTOPL,
   documento~ANLN1,
   documento~ANLN2,
   documento~LINETYPE,
   documento~MWSKZ,
   documento~KTOSL,
   posicion~xref3
   from @it_paso3all as documento
   inner join bseg as posicion
                   on  posicion~BUKRS = documento~RBUKRS  and
                       posicion~BELNR = documento~BELNR   and
                       posicion~GJAHR = documento~GJAHR   and
                       posicion~BUZEI = documento~BUZEI
   where documento~RLDNR  = @iw_factura-rldnr  and
         documento~RBUKRS = @iw_factura-RBUKRS and
         documento~GJAHR  = @iw_factura-GJAHR  and
         documento~BELNR  = @iw_factura-BELNR  and
         LINETYPE IN @rg_pos_egreso
   into CORRESPONDING FIELDS OF table @it_egresos.

      "posiciones de acreedor
      select * from @it_paso3all as documento
        where documento~RLDNR  = @iw_factura-rldnr  and
              documento~RBUKRS = @iw_factura-RBUKRS and
              documento~GJAHR  = @iw_factura-GJAHR  and
              documento~BELNR  = @iw_factura-BELNR  and
              documento~lifnr  NE ''
        into CORRESPONDING FIELDS OF table @it_acreedores.

      "posiciones de impuestos
      select * from @it_paso3all as documento
        where RLDNR  = @iw_factura-rldnr  and
              RBUKRS = @iw_factura-RBUKRS and
              GJAHR  = @iw_factura-GJAHR  and
              BELNR  = @iw_factura-BELNR  and
              LINETYPE IN @rg_pos_impuesto
        into CORRESPONDING FIELDS OF table @it_impuestos.


      loop at it_egresos  into DATA(iw_detalle) where  rldnr  = iw_factura-rldnr  and
                                                       rbukrs = iw_factura-rbukrs and
                                                       gjahr  = iw_factura-gjahr  and
                                                       belnr  = iw_factura-belnr.

        if sy-tabix = 1.
*Busca datos del proveedor
          loop at it_acreedores into DATA(iw_acreedor).
***************************************DATOS DE DOCUMENTO DE PROVEEDOR**********************************
*RAZON_S
*        Obtnere nombre del cliente
            ASSIGN tl_but000[ partner = iw_acreedor-lifnr ] TO FIELD-SYMBOL(<fs_but000>).
            IF sy-subrc = 0.
              IF <fs_but000>-natpers IS INITIAL.
                CONCATENATE <fs_but000>-name_org1 <fs_but000>-name_org2 <fs_but000>-name_org3 <fs_but000>-name_org4 INTO REPORT_OUTPUTL-razon_s SEPARATED BY space.
              ELSE.
                CONCATENATE <fs_but000>-name_last <fs_but000>-name_first INTO REPORT_OUTPUTL-razon_s SEPARATED BY space.
              ENDIF.
            ENDIF.
*ACREDOR
            REPORT_OUTPUTL-ACREDOR = iw_acreedor-lifnr.
            READ TABLE  tl_dfkkbptaxnum into DATA(wl_dfkkbptaxnum) WITH key partner  = iw_acreedor-lifnr
                                                                        taxtype      = c_mx1.
            if sy-subrc = 0.
              if wl_dfkkbptaxnum-TAXNUM is NOT INITIAL.
                REPORT_OUTPUTL-RFC = wl_dfkkbptaxnum-TAXNUM.
              endif.
              if wl_dfkkbptaxnum-TAXNUMXL is NOT INITIAL.
                REPORT_OUTPUTL-RFC = wl_dfkkbptaxnum-TAXNUMXL.
              endif.
            else.
              REPORT_OUTPUTL-RFC = iw_detalle-xref3.
            endif.

***************************************DATOS DE DOCUMENTO DE PROVEEDOR**********************************
          ENDLOOP.
********************************DATOS DE DOCUMENTO DE IVA E IND IMPUESTOS*******************************
          perform set_tax_data using it_impuestos
                                     it_paso3
                                     iw_detalle
                                     rg_ziva_ind16
                                     rg_ziva_cta16
                                     rg_ziva_ind8
                                     rg_ziva_cta8
                                     rg_zret_iva_ret_sub_arren
                                     rg_zret_iva_ret_sub_honorarios
                                     rg_zret_iva_ret_sub_contra
                                     rg_zret_iva_ret_sub_fletes
                                     rg_imp_ret_cedular
                                     rg_zret_ret_isr_honorarios
                                     rg_zret_ret_isr_arrendamiento
                                     rg_ret_isr_pagext
                                     REPORT_OUTPUTL.
********************************DATOS DE DOCUMENTO DE IVA E IND IMPUESTOS*******************************
*ISR_DIVIDENDOS no mapeado
*ISR_INTERESES no mapeado
*ISR_VENTA_ACCIONES_RETENER no mapeado
        else.
          clear: REPORT_OUTPUTL-iva_16,
                 REPORT_OUTPUTL-iva_8,
                 REPORT_OUTPUTL-IVA_RET_ARREN,
                 REPORT_OUTPUTL-IVA_RET_SERV_PROF,
                 REPORT_OUTPUTL-IVA_RET_6,
                 REPORT_OUTPUTL-IVA_RETENIDO_4,
                 REPORT_OUTPUTL-IMP_RET_CEDULAR,
                 REPORT_OUTPUTL-ISR_HONORARIOS,
                 REPORT_OUTPUTL-ISR_ARRENDAMIENTO,
                 REPORT_OUTPUTL-ISR_PAGOS_EXTRANJERO,
                 REPORT_OUTPUTL-TOTALCARGOS.
        endif.
*
*
********************************DATOS DE DOCUMENTO GENERALES DE FACTURA*******************************
*N_DOCUMENTO_FAC_SOCIEDAD
        REPORT_OUTPUTL-N_DOCUMENTO_FAC_SOCIEDAD = iw_factura-RBUKRS.
*N_DOCUMENTO_FAC_SOCIEDAD
        REPORT_OUTPUTL-N_DOCUMENTO_FAC_EJERCICIO = iw_factura-GJAHR.
*N_DOCUMENTO_FAC
        REPORT_OUTPUTL-N_DOCUMENTO_FAC = iw_factura-BELNR.
*CLASIFICACION
        READ TABLE tl_skat into DATA(wl_skat) WITH key ktopl = iw_detalle-ktopl
                                                       saknr = iw_detalle-racct.
        if sy-subrc = 0.
          REPORT_OUTPUTL-CLASIFICACION = wl_skat-txt50.
        endif.
********************************DATOS DE DOCUMENTO GENERALES DE FACTURA*******************************
*************************************IMPORTES Y BASES DE FACTURA**************************************
*IMPOR_MON_L_FAC
        REPORT_OUTPUTL-IMPOR_MON_L_FAC = iw_detalle-hsl.
        "BASE_16
        if iw_detalle-mwskz in rg_ziva_ind16.
          REPORT_OUTPUTL-BASE_16 = iw_detalle-hsl.
        endif.
        "BASE_8
        if iw_detalle-mwskz in rg_ziva_ind8.
          REPORT_OUTPUTL-BASE_8 = iw_detalle-hsl.
        endif.
        "IND_IVA_0
        "BASE_0
        if iw_detalle-mwskz in rg_ziva_ind0.
          REPORT_OUTPUTL-BASE_0 = iw_detalle-hsl.
          REPORT_OUTPUTL-ind_iva_0 = iw_detalle-mwskz.
        endif.
        "IND_EXENTO
        "BASE_EXENTO
*        if iw_detalle-mwskz in rg_ziva_0.
*          REPORT_OUTPUTL-BASE_EXENTO = iw_detalle-hsl.
*          REPORT_OUTPUTL-IND_EXENTO = iw_detalle-mwskz.
*        endif.
*
*        if iw_detalle-mwskz in rg_ziva_nd.
*          REPORT_OUTPUTL-BASE_EXENTO = iw_detalle-hsl.
*          REPORT_OUTPUTL-IND_EXENTO = iw_detalle-mwskz.
*        endif.
*        IND_NODEDU
*        BASE_NODEDU
        if iw_detalle-mwskz in rg_nodeduc.
          REPORT_OUTPUTL-BASE_NODEDU = iw_detalle-hsl.
          REPORT_OUTPUTL-IND_NODEDU = iw_detalle-mwskz.
        endif.
*       IND_NO_OBJ ??
*       BASE_NO_OBJ
        IF iw_detalle-mwskz IS INITIAL AND iw_detalle-ktosl NE 'WIT'.
          IF iw_detalle-hsl LT 0.
            iw_detalle-hsl = iw_detalle-hsl * -1.
          ENDIF.
          REPORT_OUTPUTL-base_no_obj    = iw_detalle-hsl.
        ENDIF.
*ACTIVO_FIJO1
        REPORT_OUTPUTL-ACTIVO_FIJO1 = iw_detalle-anln1.
*ACTIVO_FIJO2
        REPORT_OUTPUTL-ACTIVO_FIJO2 = iw_detalle-anln2.
*DESCRIPCION
        REPORT_OUTPUTL-FOLIOFISCAL = iw_detalle-SGTXT.
*************************************IMPORTES Y BASES DE FACTURA**************************************
********************************Conceptos desconocidos************************************************
**OTROS_GASTOS
**OTROS_RETIROS
*
**TOTAL
**Impuestos menos retenciones + bases imponibles
*        REPORT_OUTPUTL-TOTAL =  REPORT_OUTPUTL-BASE_16 +
*                                REPORT_OUTPUTL-BASE_8 +
*                                REPORT_OUTPUTL-BASE_0 +
*                                REPORT_OUTPUTL-BASE_EXENTO +
*                                REPORT_OUTPUTL-BASE_NODEDU +
**                                REPORT_OUTPUTL-BASE_NO_OBJ +
*                                REPORT_OUTPUTL-iva_16 +
*                                REPORT_OUTPUTL-iva_8 +
*                                REPORT_OUTPUTL-IVA_RET_ARREN +
*                                REPORT_OUTPUTL-IVA_RET_SERV_PROF +
*                                REPORT_OUTPUTL-IVA_RET_6 +
*                                REPORT_OUTPUTL-IVA_RETENIDO_4 +
*                                REPORT_OUTPUTL-ISR_HONORARIOS +
*                                REPORT_OUTPUTL-ISR_ARRENDAMIENTO +
*                                REPORT_OUTPUTL-ISR_PAGOS_EXTRANJERO +
*                                REPORT_OUTPUTL-IMP_RET_CEDULAR.
        REPORT_OUTPUTL-TOTAL = REPORT_OUTPUTL-IMPOR_MON_L_FAC.
*CONCEPTO
**??

**TOTALCARGOS
**??
        REPORT_OUTPUTL-NEW_REPORT = 'X'.
        append REPORT_OUTPUTL to REPORT_OUTPUT.
*        if flag_pagos is INITIAL.
*          clear REPORT_OUTPUTL-totalcargos.
*          flag_pagos = 'X'.
*        endif.

        clear: REPORT_OUTPUTL-iva_16,
               REPORT_OUTPUTL-iva_8,
               REPORT_OUTPUTL-IVA_RET_ARREN,
               REPORT_OUTPUTL-IVA_RET_SERV_PROF,
               REPORT_OUTPUTL-IVA_RET_6,
               REPORT_OUTPUTL-IVA_RETENIDO_4,
               REPORT_OUTPUTL-IMP_RET_CEDULAR,
               REPORT_OUTPUTL-ISR_HONORARIOS,
               REPORT_OUTPUTL-ISR_ARRENDAMIENTO,
               REPORT_OUTPUTL-ISR_PAGOS_EXTRANJERO,
               REPORT_OUTPUTL-TOTALCARGOS,
               REPORT_OUTPUTL-BASE_16,
               REPORT_OUTPUTL-BASE_8,
               REPORT_OUTPUTL-BASE_0,
               REPORT_OUTPUTL-BASE_EXENTO,
               REPORT_OUTPUTL-BASE_NODEDU.

      endloop.

      clear: REPORT_OUTPUTL-BASE_16,
             REPORT_OUTPUTL-BASE_8,
             REPORT_OUTPUTL-BASE_0,
             REPORT_OUTPUTL-BASE_EXENTO,
             REPORT_OUTPUTL-BASE_NODEDU,
             REPORT_OUTPUTL-BASE_NO_OBJ,
             REPORT_OUTPUTL-iva_16,
             REPORT_OUTPUTL-iva_8,
             REPORT_OUTPUTL-IVA_RET_ARREN,
             REPORT_OUTPUTL-IVA_RET_SERV_PROF,
             REPORT_OUTPUTL-IVA_RET_6,
             REPORT_OUTPUTL-IVA_RETENIDO_4,
             REPORT_OUTPUTL-ISR_HONORARIOS,
             REPORT_OUTPUTL-ISR_ARRENDAMIENTO,
             REPORT_OUTPUTL-ISR_PAGOS_EXTRANJERO,
             REPORT_OUTPUTL-IMP_RET_CEDULAR,
             REPORT_OUTPUTL-TOTALCARGOS.
    endloop.
  ENDLOOP.
*
*  REPORT_OUTPUTA[] = REPORT_OUTPUT[].
*  CLEAR REPORT_OUTPUT.
*  CALL FUNCTION 'ZFI_RECONFIG_SUMATORIA_2'
*    EXPORTING
*      REPORT_INPUT  = REPORT_OUTPUTA
*    IMPORTING
*      REPORT_OUTPUT = REPORT_OUTPUT.

ENDFUNCTION.
