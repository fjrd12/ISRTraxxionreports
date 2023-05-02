FUNCTION ZFI_GET_LIQUIDACION.
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
        it_paso1B      TYPE ZFI_TRASPASOST, "Pago
        it_paso2       TYPE ZFI_TRASPASOST, "Pago partida de Banco
        REPORT_OUTPUTL TYPE ZFI_EGRESOS_OUTPUT,
        REPORT_OUTPUTA TYPE ZFI_EGRESOS_OUTPUTT,
        vl_NAME        TYPE THEAD-TDNAME,
        TL_LINES       TYPE STANDARD TABLE OF TLINE,
        it_egresos     TYPE ZFI_TRASPASOST,
        it_egresosa    TYPE ZFI_TRASPASOST,
        it_pagos       TYPE ZFI_TRASPASOST,
        it_impuestos   TYPE ZFI_TRASPASOST,
        it_acreedores  TYPE ZFI_TRASPASOST,
        tabix          TYPE SY-tabix,
        TBSEC          TYPE STANDARD TABLE OF ZFI_BSEC,
        WBSEC          TYPE ZFI_BSEC,
        index          TYPE INT4,
        SIZE_LIST      TYPE INT4,
        SIZE_P         TYPE INT4.


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
        rg_cuentas_exclude             type RSIS_T_RANGE,
        rg_ziva_nd                     type RSIS_T_RANGE,
        rg_doc_liq                     type RSIS_T_RANGE,
        rg_doc_comp_liq                type RSIS_T_RANGE,
        valori                         type i,
        to_report                      type xfeld,
        flag_valido                    type xfeld,
        flag_pago                      type xfeld.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZTR_CTAS_DESTINO'
    IMPORTING
      RANGE = rg_ctas_egreso.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZDOC_PAG_LIQ'
    IMPORTING
      RANGE = rg_doc_liq.


  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZDOC_COMP_LIQ'
    IMPORTING
      RANGE = rg_doc_comp_liq.

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


  perform get_data_liq       using  it_paso1
                                    it_paso1B
                                    it_paso2
                                    sociedad
                                    ejerc
                                    periodo
                                    augbl
                                    rg_ctas_egreso
                                    rg_doc_liq
                                    rg_doc_comp_liq.

  clear it_pagos.
  loop at it_paso2 into DATA(iw_data2) where augbl NE ''.
    clear: iw_data2-LIFNR,
           iw_data2-kursf,
           iw_data2-awitem,
           iw_data2-racct,
           iw_data2-racct,
           iw_data2-tsl,
           iw_data2-wsl,
           iw_data2-hsl.

    sort it_pagos.
    collect iw_data2 into it_pagos.
  ENDLOOP.

  sort it_pagos by belnr.


  if it_paso1B is NOT INITIAL.
    "posiciones de acreedor
    select * from @it_paso1B as documento
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
  FOR ALL ENTRIES IN @it_paso1
     WHERE spras  EQ @sy-langu AND
           bukrs  EQ @it_paso1-rbukrs AND
           hbkid  EQ @it_paso1-hbkid AnD
           hktid  EQ @it_paso1-hktid.


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
      FOR ALL ENTRIES IN @it_paso1
         WHERE t~spras  EQ @sy-langu AND
               t~bukrs  EQ @it_paso1-rbukrs AND
               s~saknr  EQ @it_paso1-racct.
*Datos de egresos
    SELECT ktopl, saknr,txt50
               INTO TABLE @DATA(tl_skat)
               FROM skat
               FOR ALL ENTRIES IN @it_paso1
               WHERE spras = @sy-langu AND
                     ktopl = @it_paso1-ktopl AND
                     saknr = @it_paso1-racct.
*  endif.
**Datos de proveedores
*  if it_paso3all is NOT INITIAL.

* Busca el rfc de proveedor
    SELECT partner,taxtype,taxnum,taxnumxl
      INTO TABLE @DATA(tl_dfkkbptaxnum)
      FROM dfkkbptaxnum
      FOR ALL ENTRIES IN @it_paso1B
      WHERE partner  = @it_paso1B-lifnr AND
            taxtype  = @c_mx1.

    SELECT partner,name_org1,name_org2,name_org3,name_org4,name_last,name_first,natpers
      INTO TABLE @DATA(tl_but000)
      FROM but000
      FOR ALL ENTRIES IN @it_paso1B
      WHERE partner  = @it_paso1B-lifnr.

    select  BUKRS,
            BELNR,
            GJAHR,
            BUZEI,
            NAME1,
            NAME2,
            NAME3,
            NAME4,
            STCD1,
            STCD2
    from bsec
      into TABLE @tbsec
      FOR ALL ENTRIES IN @it_paso1B
      where BUKRS = @it_paso1B-RBUKRS and
            BELNR = @it_paso1B-BELNR  AND
            GJAHR = @it_paso1B-GJAHR  and
            BUZEI = @it_paso1B-buzei.

  endif.

  DESCRIBE TABLE it_pagos LINES size_p.

  LOOP at it_pagos into DATA(iw_pagos).
****************************************DATOS DE BANCOS***************************************
    IF sy-batch EQ ''.
      index = sy-tabix.
*&---------------------------------------------------------------------*
*&    FUNCTION  PROGRESS DIALOG
*&---------------------------------------------------------------------*
      CALL FUNCTION 'PROGRESS_DIALOG'
        EXPORTING
          SIZE_LIST      = size_p
          INDEX_PROGRESS = index
          TEXT_BEFORE    = 'Procesando liquid'.

    endif.

    clear: REPORT_OUTPUTL,
           flag_valido,
           flag_pago.

    perform datos_doc_pago USING REPORT_OUTPUTL
                                 iw_pagos.


    LOOP AT it_paso2 into DATA(iw_pago2) where  rldnr  = iw_pagos-rldnr and
                                                          rbukrs = iw_pagos-rbukrs and
                                                          auggj  = iw_pagos-gjahr and
                                                          augbl  = iw_pagos-belnr and
                                                          racct  in rg_ctas_egreso.
      if iw_pago2-hsl < 0.
        REPORT_OUTPUTL-TOTALCARGOS = REPORT_OUTPUTL-TOTALCARGOS + iw_pago2-hsl * ( -1 ).
      endif.

      flag_pago = 'X'.
      if iw_pago2-xblnr is INITIAL.
        REPORT_OUTPUTL-REFERENCIA = iw_pago2-awkey.
      else.
        REPORT_OUTPUTL-REFERENCIA = iw_pago2-xblnr.
      endif.

    endloop.

*****************************************GET DATOS DE BANCOS***************************************
    loop at it_paso1 into data(iw_paso1) Where rldnr  = iw_pagos-rldnr and
                                               rbukrs = iw_pagos-rbukrs and
                                               auggj  = iw_pagos-gjahr and
                                               augbl  = iw_pagos-belnr.
      clear flag_valido.
*    Busca datos de proveedor
      loop at it_paso1B into data(iw_paso1B) Where rldnr  = iw_paso1-rldnr and
                                                   rbukrs = iw_paso1-rbukrs and
                                                   gjahr  = iw_paso1-gjahr and
                                                   belnr  = iw_paso1-belnr.

        REPORT_OUTPUTL-N_SOL_ANTICIPO = iw_paso1B-belnr.
        REPORT_OUTPUTL-N_SOL_ANTICIPO_SOCIEDAD = iw_paso1B-rbukrs.
        REPORT_OUTPUTL-N_SOL_ANTICIPO_EJERCICIO = iw_paso1B-gjahr.


****************************************DATOS DE DOCUMENTO DE PROVEEDOR**********************************
        READ TABLE tbsec into wbsec WITH key bukrs = iw_paso1B-rbukrs
                                             gjahr = iw_paso1B-gjahr
                                             belnr = iw_paso1B-belnr
                                             buzei = iw_paso1B-buzei.
        if sy-subrc ne 0.
*RAZON_S
*        Obtnere nombre del cliente
          ASSIGN tl_but000[ partner = iw_paso1B-lifnr ] TO FIELD-SYMBOL(<fs_but000>).
          IF sy-subrc = 0.
            IF <fs_but000>-natpers IS INITIAL.
              CONCATENATE <fs_but000>-name_org1 <fs_but000>-name_org2 <fs_but000>-name_org3 <fs_but000>-name_org4 INTO REPORT_OUTPUTL-razon_s SEPARATED BY space.
            ELSE.
              CONCATENATE <fs_but000>-name_last <fs_but000>-name_first INTO REPORT_OUTPUTL-razon_s SEPARATED BY space.
            ENDIF.
          ENDIF.
*ACREDOR
          REPORT_OUTPUTL-ACREDOR = iw_paso1B-lifnr.
          READ TABLE  tl_dfkkbptaxnum into DATA(wl_dfkkbptaxnum) WITH key partner  = iw_paso1B-lifnr
                                                                      taxtype      = c_mx1.
          if sy-subrc = 0.
            if wl_dfkkbptaxnum-TAXNUM is NOT INITIAL.
              REPORT_OUTPUTL-RFC = wl_dfkkbptaxnum-TAXNUM.
            endif.
            if wl_dfkkbptaxnum-TAXNUMXL is NOT INITIAL.
              REPORT_OUTPUTL-RFC = wl_dfkkbptaxnum-TAXNUMXL.
            endif.
*              else.
*                REPORT_OUTPUTL-RFC = iw_detalle-xref3.
          endif.

        else.

          if wbsec-stcd1 is NOT INITIAL.
            REPORT_OUTPUTL-RFC = wbsec-stcd1.
          ELSEIF wbsec-stcd2 is NOT INITIAL.
            REPORT_OUTPUTL-RFC = wbsec-stcd2.
          endif.

          CONCATENATE wbsec-name1 wbsec-name2 wbsec-name3 INTO REPORT_OUTPUTL-razon_s SEPARATED BY space.

        endif.

        flag_valido = 'X'.
      endloop.

      read TABLE it_t012t into DATA(iw_t012t) WITH key    spras  = sy-langu
                                                    bukrs  = iw_paso1-rbukrs
                                                    hbkid  = iw_paso1-hbkid
                                                    hktid  = iw_paso1-hktid.
      if sy-subrc = 0.
        REPORT_OUTPUTL-ZBANCO_COBRO = iw_t012t-text1.
      else.
        read TABLE it_t012tk into DATA(iw_t012tk) WITH key  spras  = sy-langu
                                                            bukrs  = iw_paso1-rbukrs
                                                            saknr  = iw_paso1-racct.
        if sy-subrc = 0.
          REPORT_OUTPUTL-ZBANCO_COBRO = iw_t012tk-text1.
        endif.

      endif.
      if flag_valido = 'X'.
        REPORT_OUTPUTL-CLASIFICACION = text-040.
        REPORT_OUTPUTL-IMPOR_MON_L_FAC = iw_paso1-hsl * ( -1 ).
        REPORT_OUTPUTL-TOTAL = iw_paso1-hsl * ( -1 ).
        REPORT_OUTPUTL-OTROS_RETIROS = iw_paso1-hsl * ( -1 ).
        append REPORT_OUTPUTL to REPORT_OUTPUT.

        if flag_pago = 'X'.
          clear REPORT_OUTPUTL-TOTALCARGOS.
        endif.

      endif.
    endloop.
  endloop.


ENDFUNCTION.
