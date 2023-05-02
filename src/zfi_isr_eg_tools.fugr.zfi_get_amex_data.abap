FUNCTION ZFI_GET_AMEX_DATA.
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
  data: it_paso0       TYPE ZFI_TRASPASOST, "Docs que involucren al proveedor amex
        it_paso0B      TYPE ZFI_TRASPASOST, "Docs de compensación a bancos
        it_paso1       TYPE ZFI_TRASPASOST, "Traspaso al documento de compensación
        it_paso2       TYPE ZFI_TRASPASOST, "Documentos que compensa
        it_paso3       TYPE ZFI_TRASPASOST, "Detalle compensados.
        it_paso4       TYPE ZFI_TRASPASOST, "Todas las posiciones de egreso
        it_paso5       TYPE ZFI_TRASPASOST, "Todas las posiciones de proveedor
        it_paso6       TYPE ZFI_TRASPASOST, "Todas las posiciones de impuestos
        REPORT_OUTPUTL TYPE ZFI_EGRESOS_OUTPUT,
        vl_NAME        TYPE THEAD-TDNAME,
        TL_LINES       TYPE STANDARD TABLE OF TLINE.

  data: rg_prov_amex                   type RSIS_T_RANGE,
        rg_docs_amex                   type RSIS_T_RANGE,
        rg_ctas_egreso                 type RSIS_T_RANGE,
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
        rg_ziva_nd                     type RSIS_T_RANGE.

  DATA index TYPE INT4.

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

  perform get_general_data using it_paso0
                                 it_paso0B
                                 it_paso1
                                 it_paso2
                                 it_paso3
                                 rg_docs_amex
                                 rg_prov_amex
                                 rg_ctas_egreso
*                                 augbl
                                 sociedad
                                 periodo
                                 ejerc.


  if it_paso3 is NOT INITIAL.

    SELECT ktopl, saknr,txt50
                 INTO TABLE @DATA(tl_skat)
                 FROM skat
                 FOR ALL ENTRIES IN @it_paso3
                 WHERE spras = @sy-langu AND
                       ktopl = @it_paso3-ktopl AND
                       saknr = @it_paso3-racct.

* Busca el rfc de proveedor
    SELECT partner,taxtype,taxnum,taxnumxl
      INTO TABLE @DATA(tl_dfkkbptaxnum)
      FROM dfkkbptaxnum
      FOR ALL ENTRIES IN @it_paso3
      WHERE partner  = @it_paso3-lifnr AND
            taxtype  = @c_mx1.

    SELECT partner,name_org1,name_org2,name_org3,name_org4,name_last,name_first,natpers
      INTO TABLE @DATA(tl_but000)
      FROM but000
      FOR ALL ENTRIES IN @it_paso3
      WHERE partner  = @it_paso3-lifnr.

  endif.

  if it_paso1 is NOT INITIAL.

    SELECT spras,
       bukrs,
       hbkid,
       hktid,
       text1
  FROM t012t
  INTO TABLE @DATA(it_t012t)
   FOR ALL ENTRIES IN @it_paso0b
      WHERE spras  EQ @sy-langu AND
            bukrs  EQ @it_paso0B-rbukrs AND
            hbkid  EQ @it_paso0B-hbkid AND
            hktid  EQ @it_paso0B-hktid.


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
      FOR ALL ENTRIES IN @it_paso0b
         WHERE t~spras  EQ @sy-langu AND
               t~bukrs  EQ @it_paso0b-rbukrs AND
               s~saknr  EQ @it_paso0B-racct.

  endif.

  DESCRIBE TABLE it_paso1 LINES DATA(size_p).

  loop at it_paso1 into DATA(iw_paso1). "Por Compensación.

    IF sy-batch EQ ''.
      index = sy-tabix.
*&---------------------------------------------------------------------*
*&      FUNCTION  PROGRESS DIALOG
*&---------------------------------------------------------------------*
      CALL FUNCTION 'PROGRESS_DIALOG'
        EXPORTING
          SIZE_LIST      = size_p
          INDEX_PROGRESS = index
          TEXT_BEFORE    = 'Procesando pago Amex'.

    ENDIF.
    clear REPORT_OUTPUTL.

    read table it_paso0 into DATA(iw_paso0) WITH key
            RLDNR  =      iw_paso1-rldnr
            RBUKRS =      iw_paso1-RBUKRS
            GJAHR  =      iw_paso1-GJAHR
            BELNR  =      iw_paso1-BELNR.

    if sy-subrc = 0.

      read table it_paso0b into DATA(iw_paso0B) WITH key
              RLDNR  =      iw_paso0-rldnr
              RBUKRS =      iw_paso0-RBUKRS
              GJAHR  =      iw_paso0-AUGGJ
              BELNR  =      iw_paso0-AUGBL.

      if sy-subrc = 0.
        if iw_paso0B-TSL < 0.
          REPORT_OUTPUTL-TOTALCARGOS = iw_paso0B-TSL * ( -1 ).
        else.
          REPORT_OUTPUTL-TOTALCARGOS = iw_paso0B-TSL.
        endif.
        sort it_t012t by spras bukrs hbkid hktid.
        READ TABLE it_t012t
                    ASSIGNING FIELD-SYMBOL(<fs_t012t>)
                    WITH KEY spras = sy-langu
                             bukrs = iw_paso0B-rbukrs
                             hbkid = iw_paso0B-hbkid
                             hktid = iw_paso0B-hktid
                     BINARY SEARCH.
        IF sy-subrc EQ 0.
          REPORT_OUTPUTL-zbanco_cobro = <fs_t012t>-text1. "Banco del cobro
        else.
          read TABLE it_t012tk into DATA(iw_t012tk) WITH key  spras  = sy-langu
                                                              bukrs  = iw_paso0B-rbukrs
                                                              saknr  = iw_paso0B-racct.
          if sy-subrc = 0.
            REPORT_OUTPUTL-ZBANCO_COBRO = iw_t012tk-text1.
          endif.
        ENDIF.
      endif.
    endif.
***************************************DATOS DE DOCUMENTO DE PAGO***************************************
    perform datos_doc_pago USING REPORT_OUTPUTL
                                 iw_paso1.
***************************************DATOS DE DOCUMENTO DE PAGO***************************************

    loop at it_paso2 into DATA(iw_paso2) where RLDNR  = iw_paso1-rldnr  and "Por documento compensado
                                               RBUKRS = iw_paso1-RBUKRS and
                                               AUGGJ  = iw_paso1-AUGGJ and
                                               AUGBL  = iw_paso1-AUGBL.
***************************************DATOS DE DOCUMENTO DE FACTURA************************************
*FOLIOFISCAL
*        UUID
      "Folio fiscal
      CONCATENATE iw_paso2-rbukrs iw_paso2-belnr iw_paso2-gjahr INTO vl_name.
      CALL FUNCTION 'ZFI_ISR_READ_TEXT_BUFFER'
        EXPORTING
          client                  = sy-mandt
          id                      = 'YUUD'
          language                = 'S'
          name                    = vl_name
          object                  = 'BELEG'
        TABLES
          lines                   = tl_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.
        ASSIGN tl_lines[ 1 ] TO FIELD-SYMBOL(<fs_lines>).
        IF sy-subrc = 0.
          REPORT_OUTPUTL-FOLIOFISCAL = <fs_lines>-tdline.
        ENDIF.
      ENDIF.

      "Posiciones de egreso
      select * from @it_paso3 as documento
        where RLDNR  = @iw_paso2-rldnr  and
              RBUKRS = @iw_paso2-RBUKRS and
              GJAHR  = @iw_paso2-GJAHR  and
              BELNR  = @iw_paso2-BELNR  and
              LINETYPE IN @rg_pos_egreso
        into table @it_paso4.

      "posiciones de acreedor
      select * from @it_paso3 as documento
        where RLDNR  = @iw_paso2-rldnr  and
              RBUKRS = @iw_paso2-RBUKRS and
              GJAHR  = @iw_paso2-GJAHR  and
              BELNR  = @iw_paso2-BELNR  and
              lifnr  NE ''
        into table @it_paso5.

      "posiciones de impuestos
      select * from @it_paso3 as documento
        where RLDNR  = @iw_paso2-rldnr  and
              RBUKRS = @iw_paso2-RBUKRS and
              GJAHR  = @iw_paso2-GJAHR  and
              BELNR  = @iw_paso2-BELNR  and
              LINETYPE IN @rg_pos_impuesto
        into table @it_paso6.
***************************************DATOS DE DOCUMENTO DE FACTURA************************************
      loop at it_paso4 into DATA(iw_paso4).

        if sy-tabix = 1.
*Busca datos del proveedor
          loop at it_paso5 into DATA(iw_paso5).
***************************************DATOS DE DOCUMENTO DE PROVEEDOR**********************************
*RAZON_S
*        Obtnere nombre del cliente
            ASSIGN tl_but000[ partner = iw_paso5-lifnr ] TO FIELD-SYMBOL(<fs_but000>).
            IF sy-subrc = 0.
              IF <fs_but000>-natpers IS INITIAL.
                CONCATENATE <fs_but000>-name_org1 <fs_but000>-name_org2 <fs_but000>-name_org3 <fs_but000>-name_org4 INTO REPORT_OUTPUTL-razon_s SEPARATED BY space.
              ELSE.
                CONCATENATE <fs_but000>-name_last <fs_but000>-name_first INTO REPORT_OUTPUTL-razon_s SEPARATED BY space.
              ENDIF.
            ENDIF.
*ACREDOR
            REPORT_OUTPUTL-ACREDOR = iw_paso5-lifnr.
            READ TABLE  tl_dfkkbptaxnum into DATA(wl_dfkkbptaxnum) WITH key partner  = iw_paso5-lifnr
                                                                        taxtype      = c_mx1.
            if sy-subrc = 0.
              if wl_dfkkbptaxnum-TAXNUM is NOT INITIAL.
                REPORT_OUTPUTL-RFC = wl_dfkkbptaxnum-TAXNUM.
              endif.
              if wl_dfkkbptaxnum-TAXNUMXL is NOT INITIAL.
                REPORT_OUTPUTL-RFC = wl_dfkkbptaxnum-TAXNUMXL.
              endif.
            endif.
            REPORT_OUTPUTL-IMPOR_MON_L_FAC = iw_paso5-hsl.

***************************************DATOS DE DOCUMENTO DE PROVEEDOR**********************************
          ENDLOOP.
********************************DATOS DE DOCUMENTO DE IVA E IND IMPUESTOS*******************************
          perform set_tax_data using it_paso6
                                     it_paso4
                                     iw_paso4
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
********************************DATOS DE DOCUMENTO GENERALES DE FACTURA*******************************
*N_DOCUMENTO_FAC_SOCIEDAD
        REPORT_OUTPUTL-N_DOCUMENTO_FAC_SOCIEDAD = iw_paso1-RBUKRS.
*N_DOCUMENTO_FAC_SOCIEDAD
        REPORT_OUTPUTL-N_DOCUMENTO_FAC_EJERCICIO = iw_paso1-GJAHR.
*N_DOCUMENTO_FAC
        REPORT_OUTPUTL-N_DOCUMENTO_FAC = iw_paso4-BELNR.
*CLASIFICACION
        READ TABLE tl_skat into DATA(wl_skat) WITH key ktopl = iw_paso4-ktopl
                                                       saknr = iw_paso4-racct.
        if sy-subrc = 0.
          REPORT_OUTPUTL-CLASIFICACION = wl_skat-txt50.
        endif.
********************************DATOS DE DOCUMENTO GENERALES DE FACTURA*******************************
*************************************IMPORTES Y BASES DE FACTURA**************************************
*IMPOR_MON_L_FAC

        "BASE_16
        if iw_paso4-mwskz in rg_ziva_ind16.
          REPORT_OUTPUTL-BASE_16 = iw_paso4-hsl.
        endif.

        "BASE_8
        if iw_paso4-mwskz in rg_ziva_ind8.
          REPORT_OUTPUTL-BASE_8 = iw_paso4-hsl.
        endif.

        "IND_IVA_0
        "BASE_0
        if iw_paso4-mwskz in rg_ziva_ind0.
          REPORT_OUTPUTL-BASE_0 = iw_paso4-hsl.
          REPORT_OUTPUTL-ind_iva_0 = iw_paso4-mwskz.
        endif.

        "IND_EXENTO
        "BASE_EXENTO
        if iw_paso4-mwskz in rg_ziva_0.
          REPORT_OUTPUTL-BASE_EXENTO = iw_paso4-hsl.
          REPORT_OUTPUTL-IND_EXENTO = iw_paso4-mwskz.
        endif.

        if iw_paso4-mwskz in rg_ziva_nd.
          REPORT_OUTPUTL-BASE_EXENTO = iw_paso4-hsl.
          REPORT_OUTPUTL-IND_EXENTO = iw_paso4-mwskz.
        endif.

*        IND_NODEDU
*        BASE_NODEDU
        if iw_paso4-mwskz in rg_nodeduc.
          REPORT_OUTPUTL-BASE_NODEDU = iw_paso4-hsl.
          REPORT_OUTPUTL-IND_NODEDU = iw_paso4-mwskz.
        endif.

*       IND_NO_OBJ ??
*       BASE_NO_OBJ
        IF iw_paso4-mwskz IS INITIAL AND iw_paso4-ktosl NE 'WIT'.
          IF iw_paso4-tsl LT 0.
            iw_paso4-tsl = iw_paso4-tsl * -1.
          ENDIF.
          REPORT_OUTPUTL-base_no_obj    = iw_paso4-tsl.
        ENDIF.
*ACTIVO_FIJO1
        REPORT_OUTPUTL-ACTIVO_FIJO1 = iw_paso4-anln1.
*ACTIVO_FIJO2
        REPORT_OUTPUTL-ACTIVO_FIJO2 = iw_paso4-anln2.
*DESCRIPCION
        REPORT_OUTPUTL-DESCRIPCION = iw_paso4-SGTXT.
*************************************IMPORTES Y BASES DE FACTURA**************************************
********************************Conceptos desconocidos************************************************
**OTROS_GASTOS
**OTROS_RETIROS
*
**ZBANCO_COBRO
**Obtener el banco propio del cobro
*
**TOTAL
**Impuestos menos retenciones + bases imponibles
        REPORT_OUTPUTL-TOTAL =  REPORT_OUTPUTL-BASE_16 +
                                REPORT_OUTPUTL-BASE_8 +
                                REPORT_OUTPUTL-BASE_0 +
                                REPORT_OUTPUTL-BASE_EXENTO +
                                REPORT_OUTPUTL-BASE_NODEDU +
                                REPORT_OUTPUTL-BASE_NO_OBJ +
                                REPORT_OUTPUTL-iva_16 +
                                REPORT_OUTPUTL-iva_8 +
                                REPORT_OUTPUTL-IVA_RET_ARREN +
                                REPORT_OUTPUTL-IVA_RET_SERV_PROF +
                                REPORT_OUTPUTL-IVA_RET_6 +
                                REPORT_OUTPUTL-IVA_RETENIDO_4 +
                                REPORT_OUTPUTL-ISR_HONORARIOS +
                                REPORT_OUTPUTL-ISR_ARRENDAMIENTO +
                                REPORT_OUTPUTL-ISR_PAGOS_EXTRANJERO +
                                REPORT_OUTPUTL-IMP_RET_CEDULAR.

**CONCEPTO
**??

**
**??
        REPORT_OUTPUTL-NEW_REPORT = 'X'.
        append REPORT_OUTPUTL to REPORT_OUTPUT.
        clear: REPORT_OUTPUTL-iva_16,
               REPORT_OUTPUTL-IMPOR_MON_L_FAC,
       REPORT_OUTPUTL-iva_8,
       REPORT_OUTPUTL-IVA_RET_ARREN,
       REPORT_OUTPUTL-IVA_RET_SERV_PROF,
       REPORT_OUTPUTL-IVA_RET_6,
       REPORT_OUTPUTL-IVA_RETENIDO_4,
       REPORT_OUTPUTL-IMP_RET_CEDULAR,
       REPORT_OUTPUTL-ISR_HONORARIOS,
       REPORT_OUTPUTL-ISR_ARRENDAMIENTO,
       REPORT_OUTPUTL-ISR_PAGOS_EXTRANJERO.
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
       REPORT_OUTPUTL-IMP_RET_CEDULAR.
    ENDLOOP.

  ENDLOOP.

  LOOP AT REPORT_OUTPUT ASSIGNING FIELD-SYMBOL(<FS_REPORT>).

    IF <FS_REPORT>-TOTAL < 0.
      <FS_REPORT>-TOTAL = <FS_REPORT>-TOTAL * -1.
    ENDIF.
    IF <FS_REPORT>-IMPOR_MON_L_FAC < 0.
      <FS_REPORT>-IMPOR_MON_L_FAC = <FS_REPORT>-IMPOR_MON_L_FAC * -1.
    ENDIF.
    IF <FS_REPORT>-BASE_16 < 0.
      <FS_REPORT>-BASE_16 = <FS_REPORT>-BASE_16 * -1.
    ENDIF.
    IF <FS_REPORT>-BASE_8 < 0.
      <FS_REPORT>-BASE_8 = <FS_REPORT>-BASE_8 * -1.
    ENDIF.
    IF <FS_REPORT>-BASE_0 < 0.
      <FS_REPORT>-BASE_0 = <FS_REPORT>-BASE_0 * -1.
      <FS_REPORT>-IND_IVA_0 = <FS_REPORT>-IND_IVA_0 * -1.
    ENDIF.
    IF <FS_REPORT>-BASE_EXENTO < 0.
      <FS_REPORT>-BASE_EXENTO = <FS_REPORT>-BASE_EXENTO * -1.
      <FS_REPORT>-IND_EXENTO = <FS_REPORT>-IND_EXENTO * -1.
    ENDIF.
    IF <FS_REPORT>-BASE_NODEDU < 0.
      <FS_REPORT>-BASE_NODEDU = <FS_REPORT>-BASE_NODEDU * -1.
      <FS_REPORT>-IND_NODEDU = <FS_REPORT>-IND_NODEDU * -1.
    ENDIF.
    IF <FS_REPORT>-IVA_16 < 0.
      <FS_REPORT>-IVA_16 = <FS_REPORT>-IVA_16 * -1.
    ENDIF.
    IF <FS_REPORT>-IVA_8 < 0.
      <FS_REPORT>-IVA_8 = <FS_REPORT>-IVA_8 * -1.
    ENDIF.
    IF <FS_REPORT>-IVA_RET_ARREN < 0.
      <FS_REPORT>-IVA_RET_ARREN = <FS_REPORT>-IVA_RET_ARREN * -1.
    ENDIF.
    IF <FS_REPORT>-IVA_RET_SERV_PROF < 0.
      <FS_REPORT>-IVA_RET_SERV_PROF = <FS_REPORT>-IVA_RET_SERV_PROF * -1.
    ENDIF.
    IF <FS_REPORT>-IVA_RET_6 < 0.
      <FS_REPORT>-IVA_RET_6 = <FS_REPORT>-IVA_RET_6 * -1.
    ENDIF.
    IF <FS_REPORT>-IVA_RETENIDO_4 < 0.
      <FS_REPORT>-IVA_RETENIDO_4 = <FS_REPORT>-IVA_RETENIDO_4 * -1.
    ENDIF.
    IF <FS_REPORT>-IMP_RET_CEDULAR < 0.
      <FS_REPORT>-IMP_RET_CEDULAR = <FS_REPORT>-IMP_RET_CEDULAR * -1.
    ENDIF.
    IF <FS_REPORT>-ISR_HONORARIOS < 0.
      <FS_REPORT>-ISR_HONORARIOS = <FS_REPORT>-ISR_HONORARIOS * -1.
    ENDIF.
    IF <FS_REPORT>-ISR_ARRENDAMIENTO < 0.
      <FS_REPORT>-ISR_ARRENDAMIENTO = <FS_REPORT>-ISR_ARRENDAMIENTO * -1.
    ENDIF.
    IF <FS_REPORT>-ISR_PAGOS_EXTRANJERO < 0.
      <FS_REPORT>-ISR_PAGOS_EXTRANJERO = <FS_REPORT>-ISR_PAGOS_EXTRANJERO * -1.
    ENDIF.
    IF <FS_REPORT>-TOTALCARGOS < 0.
      <FS_REPORT>-TOTALCARGOS = <FS_REPORT>-TOTALCARGOS * -1.
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
