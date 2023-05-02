FUNCTION ZFI_GET_ANTICIPO_DATA.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(SOCIEDAD) TYPE  RSIS_T_RANGE
*"     REFERENCE(EJERC) TYPE  GJAHR
*"     REFERENCE(PERIODO) TYPE  RSIS_T_RANGE
*"  EXPORTING
*"     REFERENCE(REPORT_OUTPUT) TYPE  ZFI_EGRESOS_OUTPUTT
*"----------------------------------------------------------------------
  CONSTANTS: c_0l  type rldnr VALUE '0L'.

  types: begin of compensa,
           rldnr  type acdoca-rldnr,
           rbukrs type acdoca-rbukrs,
           augbl  type acdoca-augbl,
           auggj  type acdoca-auggj,
           gjahr  type acdoca-gjahr,
           belnr  type acdoca-belnr,
         end of compensa.

  data: it_paso0        TYPE ZFI_TRASPASOST, "Pagos egresos
        it_paso0b       TYPE ZFI_TRASPASOST, "Pagos egresos bancos
        it_paso1        TYPE ZFI_TRASPASOST, "Compensados
        it_paso2        TYPE ZFI_TRASPASOST, "No compensados
        it_paso3        TYPE ZFI_TRASPASOST, "Compensación a factura
        it_paso4        TYPE ZFI_TRASPASOST, "Detalle de Factura
        it_paso5        TYPE ZFI_TRASPASOST, "Comprobación de Factura
        it_acreedores   TYPE ZFI_TRASPASOST,
        it_egresos      TYPE ZFI_TRASPASOST,
        it_impuestos    TYPE ZFI_TRASPASOST,
        it_comprobacion TYPE ZFI_TRASPASOST,
        REPORT_OUTPUTL  TYPE ZFI_EGRESOS_OUTPUT,
        vl_NAME         TYPE THEAD-TDNAME,
        TL_LINES        TYPE STANDARD TABLE OF TLINE,
        tcompensa       type STANDARD TABLE OF compensa,
        wcompensa       TYPE compensa.

  data: rg_ctas_anticipo               type RSIS_T_RANGE,
        rg_docs_anticipo               type RSIS_T_RANGE,
        rg_ctas_egreso                 type RSIS_T_RANGE,
        rg_doc_cob                     type RSIS_T_RANGE,
        rg_prov_amex                   type RSIS_T_RANGE,
        rg_docs_amex                   type RSIS_T_RANGE,
        rg_doc_miro                    type RSIS_T_RANGE,
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
        rg_ziva_nd                     type RSIS_T_RANGE,
        compensado                     type xfeld.
  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZTR_CTAS_ANTICIPOS'
    IMPORTING
      RANGE = rg_ctas_anticipo.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZTR_DOCS_ANTICIPOS'
    IMPORTING
      RANGE = rg_docs_anticipo.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZTR_CTAS_DESTINO'
    IMPORTING
      RANGE = rg_ctas_egreso.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZDOC_COB_ANTIC'
    IMPORTING
      RANGE = rg_doc_cob.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZDOC_MIRO'
    IMPORTING
      RANGE = rg_doc_miro.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZTR_CTAS_DESTINO'
    IMPORTING
      RANGE = rg_ctas_egreso.


  PERFORM get_data_anticipos using   it_paso0
                                     it_paso0B
                                     it_paso1
                                     it_paso2
                                     it_paso3
                                     it_paso4
                                     it_paso5
                                     sociedad
                                     ejerc
                                     periodo
                                     rg_docs_anticipo
                                     rg_ctas_anticipo
                                     rg_doc_cob
                                     rg_doc_miro
                                     RG_CTAS_EGRESO.

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

  if it_paso0B is NOT INITIAL.

    SELECT
          spras,
          bukrs,
          hbkid,
          hktid,
          text1
     FROM t012t
     INTO TABLE @DATA(it_t012t)
      FOR ALL ENTRIES IN @it_paso0B
         WHERE spras  EQ @sy-langu AND
               bukrs  EQ @it_paso0B-rbukrs AND
               hbkid  EQ @it_paso0B-hbkid AnD
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
      FOR ALL ENTRIES IN @it_paso0B
         WHERE t~spras  EQ @sy-langu AND
               t~bukrs  EQ @it_paso0B-rbukrs AND
               s~saknr  EQ @it_paso0B-racct.
  endif.

*Datos de proveedores
  if it_paso3 is NOT INITIAL.

    SELECT ktopl, saknr,txt50
                 INTO TABLE @DATA(tl_skat)
                 FROM skat
                 FOR ALL ENTRIES IN @it_paso3
                 WHERE spras = @sy-langu AND
                       ktopl = @it_paso3-ktopl AND
                       saknr = @it_paso3-racct.

    SELECT ktopl, saknr,txt50
              APPENDING TABLE @tl_skat
              FROM skat
              FOR ALL ENTRIES IN @it_paso5
              WHERE spras = @sy-langu AND
                    ktopl = @it_paso5-ktopl AND
                    saknr = @it_paso5-racct.

    SELECT ktopl, saknr,txt50
     APPENDING TABLE @tl_skat
     FROM skat
     FOR ALL ENTRIES IN @it_paso0
     WHERE spras = @sy-langu AND
           ktopl = @it_paso0-ktopl AND
           saknr = @it_paso0-racct.

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

    "posiciones de acreedor
    select rldnr, rbukrs, augbl, auggj, gjahr , belnr from @it_paso3 as documento
      into table @tcompensa.

    sort tcompensa.
    delete ADJACENT DUPLICATES FROM tcompensa COMPARING ALL FIELDS.

  endif.


  DESCRIBE TABLE it_paso0 LINES DATA(size_p).
  DATA index TYPE INT4.

  loop at it_paso0 into data(iw_data0).
   IF sy-batch EQ ''.
      index = sy-tabix.
*&---------------------------------------------------------------------*
*&    FUNCTION  PROGRESS DIALOG
*&---------------------------------------------------------------------*
     CALL FUNCTION 'PROGRESS_DIALOG'
        EXPORTING
          SIZE_LIST = size_p
          INDEX_PROGRESS = index
          TEXT_BEFORE = 'Procesando anticipo'.

    ENDIF.

    clear: compensado,
           REPORT_OUTPUTL.
***************************************DATOS DE DOCUMENTO DE PAGO***************************************
    perform datos_doc_pago USING REPORT_OUTPUTL
                                 iw_data0.

    REPORT_OUTPUTL-TOTALCARGOS = iw_data0-hsl.

    READ TABLE it_paso0B into data(iw_paso0B) WITH key rldnr  = iw_data0-rldnr
                                                       rbukrs = iw_data0-rbukrs
                                                       belnr  = iw_data0-belnr
                                                       gjahr  = iw_data0-gjahr.
    if sy-subrc = 0.
      read TABLE it_t012t into DATA(iw_t012t) WITH key  spras  = sy-langu
                                                  bukrs  = iw_paso0B-rbukrs
                                                  hbkid  = iw_paso0B-hbkid
                                                  hktid  = iw_paso0B-hktid.
      if sy-subrc = 0.
        REPORT_OUTPUTL-ZBANCO_COBRO = iw_t012t-text1.
      else.
        read TABLE it_t012tk into DATA(iw_t012tk) WITH key  spras  = sy-langu
                                                            bukrs  = iw_paso0B-rbukrs
                                                            saknr  = iw_paso0B-racct.
        if sy-subrc = 0.
          REPORT_OUTPUTL-ZBANCO_COBRO = iw_t012tk-text1.
        endif.
      endif.

    endif.




    loop at tcompensa into wcompensa where rldnr = iw_data0-rldnr and
                                           rbukrs = iw_data0-rbukrs and
                                           gjahr = iw_data0-auggj and
                                           belnr = iw_data0-augbl.
      compensado = 'X'.

      loop at it_paso4 into data(iw_data4) where rldnr = wcompensa-rldnr and
                                                 auggj = wcompensa-gjahr and
                                                 augbl = wcompensa-belnr.
        "Posiciones de egreso
        select * from @it_paso5 as documento
          where RLDNR  = @iw_data4-rldnr  and
                RBUKRS = @iw_data4-RBUKRS and
                GJAHR  = @iw_data4-GJAHR  and
                BELNR  = @iw_data4-BELNR  and
                LINETYPE IN @rg_pos_egreso
          into table @it_egresos.

        "posiciones de acreedor
        select * from @it_paso5 as documento
          where RLDNR  = @iw_data4-rldnr  and
                RBUKRS = @iw_data4-RBUKRS and
                GJAHR  = @iw_data4-GJAHR  and
                BELNR  = @iw_data4-BELNR  and
                lifnr  NE ''
          into table @it_acreedores.

        "posiciones de impuestos
        select * from @it_paso5 as documento
          where RLDNR  = @iw_data4-rldnr  and
                RBUKRS = @iw_data4-RBUKRS and
                GJAHR  = @iw_data4-GJAHR  and
                BELNR  = @iw_data4-BELNR  and
                LINETYPE IN @rg_pos_impuesto
          into table @it_impuestos.


        loop at it_egresos  into DATA(iw_detalle) where        rldnr  = iw_data4-rldnr  and
                                                               rbukrs = iw_data4-rbukrs and
                                                               gjahr  = iw_data4-gjahr  and
                                                               belnr  = iw_data4-belnr.

          if sy-tabix = 1.

***************************************DATOS DE DOCUMENTO DE FACTURA************************************
*FOLIOFISCAL
*        UUID
            "Folio fiscal
            CONCATENATE iw_data4-rbukrs iw_data4-belnr iw_data4-gjahr INTO vl_name.
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
*         Busca datos del proveedor
            loop at it_acreedores into DATA(iw_acreedor).
*         **************************************DATOS DE DOCUMENTO DE PROVEEDOR**********************************
*         RAZON_S
*                 Obtnere nombre del cliente
              ASSIGN tl_but000[ partner = iw_acreedor-lifnr ] TO FIELD-SYMBOL(<fs_but000>).
              IF sy-subrc = 0.
                IF <fs_but000>-natpers IS INITIAL.
                  CONCATENATE <fs_but000>-name_org1 <fs_but000>-name_org2 <fs_but000>-name_org3 <fs_but000>-name_org4 INTO REPORT_OUTPUTL-razon_s SEPARATED BY space.
                ELSE.
                  CONCATENATE <fs_but000>-name_last <fs_but000>-name_first INTO REPORT_OUTPUTL-razon_s SEPARATED BY space.
                ENDIF.
              ENDIF.
*         ACREDOR
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
              endif.
*         **************************************DATOS DE DOCUMENTO DE PROVEEDOR**********************************
            ENDLOOP.
*         *******************************DATOS DE DOCUMENTO DE IVA E IND IMPUESTOS*******************************
            perform set_tax_data using it_impuestos
                                       it_paso5
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
*         *******************************DATOS DE DOCUMENTO DE IVA E IND IMPUESTOS*******************************
*         ISR_DIVIDENDOS no mapeado
*         ISR_INTERESES no mapeado
*         ISR_VENTA_ACCIONES_RETENER no mapeado
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


*         *******************************DATOS DE DOCUMENTO GENERALES DE FACTURA*******************************
*         N_DOCUMENTO_FAC_SOCIEDAD
          REPORT_OUTPUTL-N_DOCUMENTO_FAC_SOCIEDAD = iw_data4-RBUKRS.
*         N_DOCUMENTO_FAC_SOCIEDAD
          REPORT_OUTPUTL-N_DOCUMENTO_FAC_EJERCICIO = iw_data4-GJAHR.
*         N_DOCUMENTO_FAC
          REPORT_OUTPUTL-N_DOCUMENTO_FAC = iw_data4-BELNR.
*         CLASIFICACION
          READ TABLE tl_skat into DATA(wl_skat) WITH key ktopl = iw_detalle-ktopl
                                                         saknr = iw_detalle-racct.
          if sy-subrc = 0.
            REPORT_OUTPUTL-CLASIFICACION = wl_skat-txt50.
          endif.
*         *******************************DATOS DE DOCUMENTO GENERALES DE FACTURA*******************************
*         ************************************IMPORTES Y BASES DE FACTURA**************************************
*         IMPOR_MON_L_FAC
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
          if iw_detalle-mwskz in rg_ziva_0.
            REPORT_OUTPUTL-BASE_EXENTO = iw_detalle-hsl.
            REPORT_OUTPUTL-IND_EXENTO = iw_detalle-mwskz.
          endif.

          if iw_detalle-mwskz in rg_ziva_nd.
            REPORT_OUTPUTL-BASE_EXENTO = iw_detalle-hsl.
            REPORT_OUTPUTL-IND_EXENTO = iw_detalle-mwskz.
          endif.
*                 IND_NODEDU
*                 BASE_NODEDU
          if iw_detalle-mwskz in rg_nodeduc.
            REPORT_OUTPUTL-BASE_NODEDU = iw_detalle-hsl.
            REPORT_OUTPUTL-IND_NODEDU = iw_detalle-mwskz.
          endif.
*                IND_NO_OBJ ??
*                BASE_NO_OBJ
          IF iw_detalle-mwskz IS INITIAL AND iw_detalle-ktosl NE 'WIT'.
            IF iw_detalle-hsl LT 0.
              iw_detalle-hsl = iw_detalle-hsl * -1.
            ENDIF.
            REPORT_OUTPUTL-base_no_obj    = iw_detalle-hsl.
          ENDIF.
*         ACTIVO_FIJO1
          REPORT_OUTPUTL-ACTIVO_FIJO1 = iw_detalle-anln1.
*         ACTIVO_FIJO2
          REPORT_OUTPUTL-ACTIVO_FIJO2 = iw_detalle-anln2.
*         DESCRIPCION
          REPORT_OUTPUTL-DESCRIPCION = iw_detalle-SGTXT.
*         ************************************IMPORTES Y BASES DE FACTURA**************************************
*         *******************************Conceptos desconocidos************************************************
*         *OTROS_GASTOS
*         *OTROS_RETIROS
*
*         *ZBANCO_COBRO
*         *Obtener el banco propio del cobro
*
*         *TOTAL
*         *Impuestos menos retenciones + bases imponibles
*          REPORT_OUTPUTL-TOTAL =  REPORT_OUTPUTL-BASE_16 +
*                                  REPORT_OUTPUTL-BASE_8 +
*                                  REPORT_OUTPUTL-BASE_0 +
*                                  REPORT_OUTPUTL-BASE_EXENTO +
*                                  REPORT_OUTPUTL-BASE_NODEDU +
**                                  REPORT_OUTPUTL-BASE_NO_OBJ +
*                                  REPORT_OUTPUTL-iva_16 +
*                                  REPORT_OUTPUTL-iva_8 +
*                                  REPORT_OUTPUTL-IVA_RET_ARREN +
*                                  REPORT_OUTPUTL-IVA_RET_SERV_PROF +
*                                  REPORT_OUTPUTL-IVA_RET_6 +
*                                  REPORT_OUTPUTL-IVA_RETENIDO_4 +
*                                  REPORT_OUTPUTL-ISR_HONORARIOS +
*                                  REPORT_OUTPUTL-ISR_ARRENDAMIENTO +
*                                  REPORT_OUTPUTL-ISR_PAGOS_EXTRANJERO +
*                                  REPORT_OUTPUTL-IMP_RET_CEDULAR.
          REPORT_OUTPUTL-TOTAL = REPORT_OUTPUTL-IMPOR_MON_L_FAC.
*         *CONCEPTO
          read TABLE tl_skat into DATA(wskat) with key
                                                       ktopl = iw_detalle-ktopl
                                                       saknr = iw_detalle-racct.

          if sy-subrc = 0.
            REPORT_OUTPUTL-concepto = wskat-txt50.
          endif.

*         *TOTALCARGOS
*         *??
          REPORT_OUTPUTL-NEW_REPORT = 'X'.
          append REPORT_OUTPUTL to REPORT_OUTPUT.

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
        endloop.
      endloop.
    endloop.
    if compensado is INITIAL.
      REPORT_OUTPUTL-CLASIFICACION = text-001.
      REPORT_OUTPUTL-CONCEPTO = text-001.
      REPORT_OUTPUTL-NEW_REPORT = 'X'.
      append REPORT_OUTPUTL to REPORT_OUTPUT.
    endif.

  ENDLOOP.




ENDFUNCTION.
