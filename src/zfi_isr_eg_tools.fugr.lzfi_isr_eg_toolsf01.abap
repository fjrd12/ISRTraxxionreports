*----------------------------------------------------------------------*
***INCLUDE LZFI_ISR_EG_TOOLSF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form GET_VALUE_OF_RANGE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> RG_CTAS_EGRESO
*&      --> P_
*&---------------------------------------------------------------------*
FORM GET_VALUE_OF_RANGE  USING    P_RANGE TYPE  RSIS_T_RANGE
                                  NAME TYPE string.

  SELECT  SIGN,
          OPTI,
          LOW,
          HIGH
    INTO TABLE @DATA(tl_tvarvc)
    FROM tvarvc
    WHERE name = @name.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc ASSIGNING FIELD-SYMBOL(<fs_tvarvc>).
      p_range = VALUE  #(
                   BASE p_range ( low    = <fs_tvarvc>-low
                                  sign   = <fs_tvarvc>-sign
                                  high   = <fs_tvarvc>-high
                                  option = <fs_tvarvc>-opti ) ).
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_RANGES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> RG_PROV_AMEX
*&      --> RG_DOCS_AMEX
*&      --> RG_CTAS_EGRESO
*&      --> RG_POS_EGRESO
*&      --> RG_POS_IMPUESTO
*&      --> RG_ZIVA_16
*&      --> RG_ZIVA_CTA16
*&      --> RG_ZIVA_IND16
*&      --> RG_ZIVA_8
*&      --> RG_ZIVA_CTA8
*&      --> RG_ZIVA_IND8
*&      --> RG_ZIVA_IND0
*&      --> RG_ZIVA_0
*&      --> RG_ZRET_IVA_RET_SUB_ARREN
*&      --> RG_ZRET_IVA_RET_SUB_HONORARIOS
*&      --> RG_ZRET_IVA_RET_SUB_CONTRA
*&      --> RG_ZRET_IVA_RET_SUB_FLETES
*&      --> RG_IMP_RET_CEDULAR
*&      --> RG_ZRET_RET_ISR_HONORARIOS
*&      --> RG_ZRET_RET_ISR_ARRENDAMIENTO
*&      --> RG_RET_ISR_PAGEXT
*&      --> RANGE_WA
*&      --> RANGE_WA2
*&      --> RG_NODEDUC
*&---------------------------------------------------------------------*
FORM CREATE_RANGES  USING    RG_PROV_AMEX type RSIS_T_RANGE
                             RG_DOCS_AMEX type RSIS_T_RANGE
                             RG_CTAS_EGRESO type RSIS_T_RANGE
                             RG_POS_EGRESO type RSIS_T_RANGE
                             RG_POS_IMPUESTO type RSIS_T_RANGE
                             RG_ZIVA_16 type RSIS_T_RANGE
                             RG_ZIVA_CTA16 type RSIS_T_RANGE
                             RG_ZIVA_IND16 type RSIS_T_RANGE
                             RG_ZIVA_8 type RSIS_T_RANGE
                             RG_ZIVA_CTA8 type RSIS_T_RANGE
                             RG_ZIVA_IND8 type RSIS_T_RANGE
                             RG_ZIVA_IND0 type RSIS_T_RANGE
                             RG_ZIVA_0 type RSIS_T_RANGE
                             RG_ZRET_IVA_RET_SUB_ARREN type RSIS_T_RANGE
                             RG_ZRET_IVA_RET_SUB_HONORARIOS type RSIS_T_RANGE
                             RG_ZRET_IVA_RET_SUB_CONTRA type RSIS_T_RANGE
                             RG_ZRET_IVA_RET_SUB_FLETES type RSIS_T_RANGE
                             RG_IMP_RET_CEDULAR type RSIS_T_RANGE
                             RG_ZRET_RET_ISR_HONORARIOS type RSIS_T_RANGE
                             RG_ZRET_RET_ISR_ARRENDAMIENTO type RSIS_T_RANGE
                             RG_RET_ISR_PAGEXT type RSIS_T_RANGE
                             RANGE_WA type RSIS_S_RANGE
                             RANGE_WA2 type RSIS_S_RANGE
                             RG_NODEDUC type RSIS_T_RANGE
                             RG_ZIVA_ND type RSIS_T_RANGE.




  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZTR_DOCS_AMEX'
    IMPORTING
      RANGE = rg_docs_amex.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZTR_PROV_AMEX'
    IMPORTING
      RANGE = rg_prov_amex.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZTR_CTAS_DESTINO'
    IMPORTING
      RANGE = rg_ctas_egreso.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZCLAVE_EGRESO'
    IMPORTING
      RANGE = rg_pos_egreso.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZCLAVE_IMPUESTOS'
    IMPORTING
      RANGE = rg_pos_impuesto.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZIVA_16%'
    IMPORTING
      RANGE = rg_ziva_16.

  refresh: rg_ziva_cta16,
           rg_ziva_ind16.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZIVA_8%'
    IMPORTING
      RANGE = rg_ziva_8.

  refresh: rg_ziva_cta8,
           rg_ziva_ind8.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZIVA_0'
    IMPORTING
      RANGE = rg_ziva_ind0.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZIVA_0%'
    IMPORTING
      RANGE = rg_ziva_0.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZNODEDUC'
    IMPORTING
      RANGE = rg_nodeduc.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZRET_IVA_RET_SUB_ARREN'
    IMPORTING
      RANGE = rg_zret_iva_ret_sub_arren.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZRET_IVA_RET_SUB_HONORARIOS'
    IMPORTING
      RANGE = rg_zret_iva_ret_sub_honorarios.


  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZRET_IVA_RET_SUB_CONTRA'
    IMPORTING
      RANGE = rg_zret_iva_ret_sub_contra.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZRET_IVA_RET_SUB_FLETES'
    IMPORTING
      RANGE = rg_zret_iva_ret_sub_fletes.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZIMP_RET_CEDULAR'
    IMPORTING
      RANGE = rg_imp_ret_cedular.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZRET_RET_ISR_HONORARIOS'
    IMPORTING
      RANGE = rg_zret_ret_isr_honorarios.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZRET_RET_ISR_ARRENDAMIENTO'
    IMPORTING
      RANGE = rg_zret_ret_isr_arrendamiento.

  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZIMP_RET_ISR_PAGEXT'
    IMPORTING
      RANGE = rg_ret_isr_pagext.


  CALL FUNCTION 'ZFI_CREATE_RANGE_SELECTION'
    EXPORTING
      NAME  = 'ZIVA_ND'
    IMPORTING
      RANGE = rg_ziva_nd.


  loop at rg_ziva_8 into range_wa2.

    clear range_wa.
    range_wa-low = range_wa2-low.
    clear range_wa-high.
    range_wa-SIGN = 'I'.
    range_wa-OPTION = 'EQ'.
    append range_wa to rg_ziva_cta8.

    range_wa-low = range_wa2-high.
    clear range_wa-high.
    range_wa-SIGN = 'I'.
    range_wa-OPTION = 'EQ'.
    append range_wa to rg_ziva_ind8.

  endloop.


  loop at rg_ziva_16 into range_wa2.

    clear range_wa.
    range_wa-low = range_wa2-low.
    clear range_wa-high.
    range_wa-SIGN = 'I'.
    range_wa-OPTION = 'EQ'.
    append range_wa to rg_ziva_cta16.

    range_wa-low = range_wa2-high.
    clear range_wa-high.
    range_wa-SIGN = 'I'.
    range_wa-OPTION = 'EQ'.
    append range_wa to rg_ziva_ind16.

  endloop.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DATOS_DOC_PAGO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> REPORT_OUTPUTL
*&      --> IW_PASO1
*&---------------------------------------------------------------------*
FORM DATOS_DOC_PAGO  USING    REPORT_OUTPUTL TYPE ZFI_EGRESOS_OUTPUT
                              IW_PASO        TYPE ZFI_TRASPASOS.
*BUKRS
  REPORT_OUTPUTL-BUKRS = iw_paso-rbukrs.
*GJAHR
  REPORT_OUTPUTL-GJAHR = iw_paso-GJAHR.
*PERIODO
  REPORT_OUTPUTL-PERIODO = iw_paso-POPER.
*FECHAPAGO
  REPORT_OUTPUTL-FECHAPAGO = iw_paso-BUDAT.
*REFERENCIA
  REPORT_OUTPUTL-REFERENCIA = iw_paso-XBLNR.
*N_DOC_PAGO
  REPORT_OUTPUTL-N_DOC_PAGO = iw_paso-AUGBL.
*N_DOC_PAGO_SOCIEDAD
  REPORT_OUTPUTL-N_DOC_PAGO_SOCIEDAD = iw_paso-RBUKRS.
*N_DOC_PAGO_EJERCICIO
  REPORT_OUTPUTL-N_DOC_PAGO_EJERCICIO = iw_paso-GJAHR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TAX_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_PASO6
*&      --> IT_PASO4
*&      --> IW_PASO4
*&      --> RG_ZIVA_IND16
*&      --> RG_ZIVA_CTA16
*&      --> RG_ZIVA_IND8
*&      --> RG_ZIVA_CTA8
*&      --> RG_ZRET_IVA_RET_SUB_ARREN
*&      --> RG_ZRET_IVA_RET_SUB_HONORARIOS
*&      --> RG_ZRET_IVA_RET_SUB_CONTRA
*&      --> RG_ZRET_IVA_RET_SUB_FLETES
*&      --> RG_IMP_RET_CEDULAR
*&      --> RG_ZRET_RET_ISR_HONORARIOS
*&      --> RG_ZRET_RET_ISR_ARRENDAMIENTO
*&      --> RG_RET_ISR_PAGEXT
*&      --> ELSE
*&---------------------------------------------------------------------*
FORM SET_TAX_DATA  USING    IT_PASO6 type ZFI_TRASPASOST
                            IT_PASO4 type ZFI_TRASPASOST
                            IW_PASO4 type ZFI_TRASPASOS
                            RG_ZIVA_IND16 type RSIS_T_RANGE
                            RG_ZIVA_CTA16 type RSIS_T_RANGE
                            RG_ZIVA_IND8 type RSIS_T_RANGE
                            RG_ZIVA_CTA8 type RSIS_T_RANGE
                            RG_ZRET_IVA_RET_SUB_ARREN type RSIS_T_RANGE
                            RG_ZRET_IVA_RET_SUB_HONORARIOS type RSIS_T_RANGE
                            RG_ZRET_IVA_RET_SUB_CONTRA type RSIS_T_RANGE
                            RG_ZRET_IVA_RET_SUB_FLETES type RSIS_T_RANGE
                            RG_IMP_RET_CEDULAR type RSIS_T_RANGE
                            RG_ZRET_RET_ISR_HONORARIOS type RSIS_T_RANGE
                            RG_ZRET_RET_ISR_ARRENDAMIENTO type RSIS_T_RANGE
                            RG_RET_ISR_PAGEXT type RSIS_T_RANGE
                            REPORT_OUTPUTL type ZFI_EGRESOS_OUTPUT.

  "Deriva campos de impuestos.
  "*IVA_16
  "IND_IVA_16
  LOOP AT it_paso6 into DATA(iw_paso6)  WHERE rbukrs EQ iw_paso4-rbukrs AND
                                              gjahr  EQ iw_paso4-gjahr  AND
                                              belnr  EQ iw_paso4-belnr  AND
                                              mwskz  IN rg_ziva_ind16 and
                                              racct  IN rg_ziva_cta16.

    REPORT_OUTPUTL-iva_16     = REPORT_OUTPUTL-iva_16 + iw_paso6-hsl.
    REPORT_OUTPUTL-ind_iva_16 = iw_paso6-mwskz.

  ENDLOOP.

  " INDI_IVA_8
  " IVA_8
  LOOP AT it_paso6 into iw_paso6        WHERE rbukrs EQ iw_paso4-rbukrs AND
                                              gjahr  EQ iw_paso4-gjahr  AND
                                              belnr  EQ iw_paso4-belnr  AND
                                              mwskz  IN rg_ziva_ind8 and
                                              racct  IN rg_ziva_cta8.

    REPORT_OUTPUTL-iva_8     = REPORT_OUTPUTL-iva_8 + iw_paso6-hsl.
    REPORT_OUTPUTL-indi_iva_8 = iw_paso6-mwskz.
  ENDLOOP.

*IMPU_IVA_VIRTUAL No se está mapeando
*IVA_RET_ARREN
  LOOP AT it_paso6 into iw_paso6        WHERE rbukrs EQ iw_paso4-rbukrs AND
                                              gjahr  EQ iw_paso4-gjahr  AND
                                              belnr  EQ iw_paso4-belnr  AND
                                              racct  IN rg_zret_iva_ret_sub_arren.

    REPORT_OUTPUTL-IVA_RET_ARREN     = REPORT_OUTPUTL-IVA_RET_ARREN + iw_paso6-hsl.
  ENDLOOP.

*IVA_RET_SERV_PROF
  LOOP AT it_paso6 into iw_paso6        WHERE rbukrs EQ iw_paso4-rbukrs AND
                                              gjahr  EQ iw_paso4-gjahr  AND
                                              belnr  EQ iw_paso4-belnr  AND
                                              racct  IN rg_zret_iva_ret_sub_honorarios.

    REPORT_OUTPUTL-IVA_RET_SERV_PROF     = REPORT_OUTPUTL-IVA_RET_SERV_PROF + iw_paso6-hsl.
  ENDLOOP.

*IVA_RET_6
  LOOP AT it_paso6 into iw_paso6        WHERE rbukrs EQ iw_paso4-rbukrs AND
                                              gjahr  EQ iw_paso4-gjahr  AND
                                              belnr  EQ iw_paso4-belnr  AND
                                              racct  IN rg_zret_iva_ret_sub_contra.

    REPORT_OUTPUTL-IVA_RET_6     = REPORT_OUTPUTL-IVA_RET_6 + iw_paso6-hsl.
  ENDLOOP.

*IVA_RETENIDO_4
  LOOP AT it_paso6 into iw_paso6        WHERE rbukrs EQ iw_paso4-rbukrs AND
                                              gjahr  EQ iw_paso4-gjahr  AND
                                              belnr  EQ iw_paso4-belnr  AND
                                              racct  IN rg_zret_iva_ret_sub_fletes.
    REPORT_OUTPUTL-IVA_RETENIDO_4     = REPORT_OUTPUTL-IVA_RETENIDO_4 + iw_paso6-hsl.
  ENDLOOP.

*IMP_RET_CEDULAR
  LOOP AT it_paso6 into iw_paso6        WHERE rbukrs EQ iw_paso4-rbukrs AND
                                              gjahr  EQ iw_paso4-gjahr  AND
                                              belnr  EQ iw_paso4-belnr  AND
                                              racct  IN rg_imp_ret_cedular.
    REPORT_OUTPUTL-IMP_RET_CEDULAR     = REPORT_OUTPUTL-IMP_RET_CEDULAR + iw_paso6-hsl.
  ENDLOOP.

*ISR_HONORARIOS
  LOOP AT it_paso6 into iw_paso6        WHERE rbukrs EQ iw_paso4-rbukrs AND
                                              gjahr  EQ iw_paso4-gjahr  AND
                                              belnr  EQ iw_paso4-belnr  AND
                                              racct  IN rg_zret_ret_isr_honorarios.
    REPORT_OUTPUTL-ISR_HONORARIOS     = REPORT_OUTPUTL-ISR_HONORARIOS + iw_paso6-hsl.
  ENDLOOP.

*ISR_ARRENDAMIENTO
  LOOP AT it_paso6 into iw_paso6        WHERE rbukrs EQ iw_paso4-rbukrs AND
                                              gjahr  EQ iw_paso4-gjahr  AND
                                              belnr  EQ iw_paso4-belnr  AND
                                              racct  IN rg_zret_ret_isr_arrendamiento.
    REPORT_OUTPUTL-ISR_ARRENDAMIENTO     = REPORT_OUTPUTL-ISR_ARRENDAMIENTO + iw_paso6-hsl.
  ENDLOOP.

*ISR_PAGOS_EXTRANJERO
  LOOP AT it_paso6 into iw_paso6        WHERE rbukrs EQ iw_paso4-rbukrs AND
                                              gjahr  EQ iw_paso4-gjahr  AND
                                              belnr  EQ iw_paso4-belnr  AND
                                              racct  IN rg_ret_isr_pagext.
    REPORT_OUTPUTL-ISR_PAGOS_EXTRANJERO     = REPORT_OUTPUTL-ISR_PAGOS_EXTRANJERO + iw_paso6-hsl.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_GENERAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_PASO0
*&      --> IT_PASO1
*&      --> IT_PASO2
*&      --> IT_PASO3
*&      --> RG_DOCS_AMEX
*&      --> RG_PROV_AMEX
*&---------------------------------------------------------------------*
FORM GET_GENERAL_DATA  USING    IT_PASO0       type ZFI_TRASPASOST
                                IT_PASO0B      type ZFI_TRASPASOST
                                IT_PASO1       type ZFI_TRASPASOST
                                IT_PASO2       type ZFI_TRASPASOST
                                IT_PASO3       type ZFI_TRASPASOST
                                RG_DOCS_AMEX   type RSIS_T_RANGE
                                RG_PROV_AMEX   type RSIS_T_RANGE
                                rg_ctas_egreso type RSIS_T_RANGE
                                sociedad       type RSIS_T_RANGE
                                periodo        type RSIS_T_RANGE
                                ejerc          type gjahr.

  SELECT     acdoca~RLDNR,
               acdoca~RBUKRS,
               acdoca~GJAHR,
               acdoca~POPER,
               acdoca~BLART,
               acdoca~BUDAT,
               acdoca~BLDAT,
               acdoca~BELNR,
               acdoca~AWITEM,
               acdoca~RACCT,
               acdoca~TSL,
               acdoca~SGTXT,
               acdoca~wsl,
               acdoca~rwcur,
               acdoca~hsl,
               acdoca~hbkid,
               acdoca~hktid,
               bkpf~awkey,
               bkpf~xblnr,
               acdoca~augbl,
               acdoca~AUGGJ,
               acdoca~AUGDT,
               acdoca~kunnr,
               bkpf~kursf,
               acdoca~ktopl,
               acdoca~anln1,
               acdoca~anln2,
               acdoca~linetype,
               acdoca~mwskz,
               acdoca~ktosl
          FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
          INTO CORRESPONDING FIELDS OF TABLE @it_paso0
          WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
                acdoca~rbukrs IN @sociedad           AND    "Sociedad
                acdoca~ryear  EQ @ejerc              AND    "Ejercicio
                acdoca~poper  IN @periodo            AND    "Periodo
                acdoca~blart  IN @rg_docs_amex       AND
                acdoca~lifnr  IN @rg_prov_amex       and
                acdoca~AWREF_REV = '' and
                acdoca~augbl  NE ''.

  if sy-subrc = 0.
    SELECT acdoca~RLDNR,
          acdoca~RBUKRS,
          acdoca~GJAHR,
          acdoca~POPER,
          acdoca~BLART,
          acdoca~BUDAT,
          acdoca~BLDAT,
          acdoca~BELNR,
          acdoca~AWITEM,
          acdoca~RACCT,
          acdoca~TSL,
          acdoca~SGTXT,
          acdoca~wsl,
          acdoca~rwcur,
          acdoca~hsl,
          acdoca~hbkid,
          acdoca~hktid,
          bkpf~awkey,
          bkpf~xblnr,
          acdoca~augbl,
          acdoca~AUGGJ,
          acdoca~AUGDT,
          acdoca~kunnr,
          bkpf~kursf,
          acdoca~lifnr,
          acdoca~ktopl,
          acdoca~anln1,
          acdoca~anln2,
          acdoca~linetype,
          acdoca~mwskz,
          acdoca~ktosl
     FROM acdoca
         INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                            bkpf~belnr = acdoca~belnr and
                            bkpf~gjahr = acdoca~ryear
     INTO CORRESPONDING FIELDS OF TABLE @it_paso0B
     for ALL ENTRIES IN @it_paso0
     WHERE acdoca~RLDNR  =      @it_paso0-rldnr  and
           acdoca~RBUKRS =      @it_paso0-RBUKRS and
           acdoca~GJAHR  =      @it_paso0-AUGGJ  and
           acdoca~BELNR  =      @it_paso0-AUGBL  and
           acdoca~RACCT  in     @rg_ctas_egreso  and
           acdoca~AWREF_REV = ''.

    SELECT acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~AWITEM,
           acdoca~RACCT,
           acdoca~TSL,
           acdoca~SGTXT,
           acdoca~wsl,
           acdoca~rwcur,
           acdoca~hsl,
           acdoca~hbkid,
           acdoca~hktid,
           bkpf~awkey,
           bkpf~xblnr,
           acdoca~augbl,
           acdoca~AUGGJ,
           acdoca~AUGDT,
           acdoca~kunnr,
           bkpf~kursf,
           acdoca~lifnr,
           acdoca~ktopl,
           acdoca~anln1,
           acdoca~anln2,
           acdoca~linetype,
           acdoca~mwskz,
           acdoca~ktosl
      FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
      INTO CORRESPONDING FIELDS OF TABLE @it_paso1
      for ALL ENTRIES IN @it_paso0
      WHERE acdoca~RLDNR  =      @it_paso0-rldnr and
            acdoca~RBUKRS =      @it_paso0-RBUKRS and
            acdoca~GJAHR  =      @it_paso0-GJAHR and
            acdoca~AUGGJ  NE     '' AND
            acdoca~BELNR  =      @it_paso0-BELNR and
            acdoca~LIFNR  NOT IN @rg_prov_amex and
            acdoca~AWREF_REV = ''.

    if sy-subrc = 0.

      SELECT acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~AWITEM,
             acdoca~RACCT,
             acdoca~TSL,
             acdoca~SGTXT,
             acdoca~wsl,
             acdoca~rwcur,
             acdoca~hsl,
             acdoca~hbkid,
             acdoca~hktid,
             bkpf~awkey,
             bkpf~xblnr,
             acdoca~augbl,
             acdoca~AUGGJ,
             acdoca~AUGDT,
             acdoca~kunnr,
             bkpf~kursf,
             acdoca~lifnr,
             acdoca~ktopl,
             acdoca~anln1,
             acdoca~anln2,
             acdoca~linetype,
             acdoca~mwskz,
             acdoca~ktosl
        FROM acdoca
            INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                               bkpf~belnr = acdoca~belnr and
                               bkpf~gjahr = acdoca~ryear
        INTO CORRESPONDING FIELDS OF TABLE @it_paso2
        for ALL ENTRIES IN @it_paso1
        WHERE acdoca~RLDNR  =      @it_paso1-rldnr and
              acdoca~RBUKRS =      @it_paso1-RBUKRS and
              acdoca~AUGGJ  =      @it_paso1-AUGGJ and
              acdoca~AUGBL  =      @it_paso1-AUGBL and
              acdoca~BELNR  NE     @it_paso1-AUGBL and
              acdoca~blart  NOT IN @rg_docs_amex   AND
              acdoca~AWREF_REV = ''.

      if sy-subrc = 0.

        SELECT acdoca~RLDNR,
            acdoca~RBUKRS,
            acdoca~GJAHR,
            acdoca~POPER,
            acdoca~BLART,
            acdoca~BUDAT,
            acdoca~BLDAT,
            acdoca~BELNR,
            acdoca~AWITEM,
            acdoca~RACCT,
            acdoca~TSL,
            acdoca~SGTXT,
            acdoca~wsl,
            acdoca~rwcur,
            acdoca~hsl,
            acdoca~hbkid,
            acdoca~hktid,
            bkpf~awkey,
            bkpf~xblnr,
            acdoca~augbl,
            acdoca~AUGGJ,
            acdoca~AUGDT,
            acdoca~kunnr,
            bkpf~kursf,
            acdoca~lifnr,
            acdoca~ktopl,
            acdoca~anln1,
            acdoca~anln2,
            acdoca~linetype,
            acdoca~mwskz,
            acdoca~ktosl
       FROM acdoca
           INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                              bkpf~belnr = acdoca~belnr and
                              bkpf~gjahr = acdoca~ryear
       INTO CORRESPONDING FIELDS OF TABLE @it_paso3
       for ALL ENTRIES IN @it_paso2
       WHERE acdoca~RLDNR  = @it_paso2-rldnr and
             acdoca~RBUKRS = @it_paso2-RBUKRS and
             acdoca~GJAHR  = @it_paso2-GJAHR and
             acdoca~BELNR  = @it_paso2-BELNR and
             acdoca~AWREF_REV = ''.


      endif.

    endif.

  endif.

ENDFORM.


FORM GET_GENERAL_DATA_V2  USING     IT_PASO0       type ZFI_TRASPASOST
                                    IT_PASO0B      type ZFI_TRASPASOST
                                    IT_PASO0C      type ZFI_TRASPASOST
                                    IT_PASO1       type ZFI_TRASPASOST
                                    IT_PASO2       type ZFI_TRASPASOST
                                    IT_PASO3       type ZFI_TRASPASOST
                                    RG_DOCS_AMEX   type RSIS_T_RANGE
                                    RG_PROV_AMEX   type RSIS_T_RANGE
                                    rg_ctas_egreso type RSIS_T_RANGE
                                    rg_docs_pagoacre type RSIS_T_RANGE
                                    sociedad       type RSIS_T_RANGE
                                    periodo        type RSIS_T_RANGE
                                    ejerc          type gjahr.

  SELECT       acdoca~RLDNR,
               acdoca~RBUKRS,
               acdoca~GJAHR,
               acdoca~POPER,
               acdoca~BLART,
               acdoca~BUDAT,
               acdoca~BLDAT,
               acdoca~BELNR,
               acdoca~AWITEM,
               acdoca~RACCT,
               acdoca~TSL,
               acdoca~SGTXT,
               acdoca~wsl,
               acdoca~rwcur,
               acdoca~hsl,
               acdoca~hbkid,
               acdoca~hktid,
               bkpf~awkey,
               bkpf~xblnr,
               acdoca~augbl,
               acdoca~AUGGJ,
               acdoca~AUGDT,
               acdoca~kunnr,
               bkpf~kursf,
               acdoca~ktopl,
               acdoca~anln1,
               acdoca~anln2,
               acdoca~linetype,
               acdoca~mwskz,
               acdoca~ktosl
          FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
          INTO CORRESPONDING FIELDS OF TABLE @it_paso0
          WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
                acdoca~rbukrs IN @sociedad           AND    "Sociedad
                acdoca~ryear  EQ @ejerc              AND    "Ejercicio
                acdoca~poper  IN @periodo            AND    "Periodo
                acdoca~blart  IN @rg_docs_amex       AND
                acdoca~lifnr  IN @rg_prov_amex       and
                acdoca~AWREF_REV = ''.
*     and
*                acdoca~augbl  NE ''.

  if sy-subrc = 0.

    SELECT acdoca~RLDNR,
      acdoca~RBUKRS,
      acdoca~GJAHR,
      acdoca~POPER,
      acdoca~BLART,
      acdoca~BUDAT,
      acdoca~BLDAT,
      acdoca~BELNR,
      acdoca~AWITEM,
      acdoca~RACCT,
      acdoca~TSL,
      acdoca~SGTXT,
      acdoca~wsl,
      acdoca~rwcur,
      acdoca~hsl,
      acdoca~hbkid,
      acdoca~hktid,
      bkpf~awkey,
      bkpf~xblnr,
      acdoca~augbl,
      acdoca~AUGGJ,
      acdoca~AUGDT,
      acdoca~kunnr,
      bkpf~kursf,
      acdoca~lifnr,
      acdoca~ktopl,
      acdoca~anln1,
      acdoca~anln2,
      acdoca~linetype,
      acdoca~mwskz,
      acdoca~ktosl
 FROM acdoca
     INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                        bkpf~belnr = acdoca~belnr and
                        bkpf~gjahr = acdoca~ryear
 INTO CORRESPONDING FIELDS OF TABLE @it_paso0C
 WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
       acdoca~rbukrs IN @sociedad           AND    "Sociedad
       acdoca~ryear  EQ @ejerc              AND    "Ejercicio
       acdoca~poper  IN @periodo            AND    "Periodo
       acdoca~blart  IN @rg_docs_pagoacre   AND
       acdoca~lifnr  IN @rg_prov_amex       and
       acdoca~AWREF_REV = ''.

    if it_paso0C is NOT INITIAL.

      SELECT acdoca~RLDNR,
            acdoca~RBUKRS,
            acdoca~GJAHR,
            acdoca~POPER,
            acdoca~BLART,
            acdoca~BUDAT,
            acdoca~BLDAT,
            acdoca~BELNR,
            acdoca~AWITEM,
            acdoca~RACCT,
            acdoca~TSL,
            acdoca~SGTXT,
            acdoca~wsl,
            acdoca~rwcur,
            acdoca~hsl,
            acdoca~hbkid,
            acdoca~hktid,
            bkpf~awkey,
            bkpf~xblnr,
            acdoca~augbl,
            acdoca~AUGGJ,
            acdoca~AUGDT,
            acdoca~kunnr,
            bkpf~kursf,
            acdoca~lifnr,
            acdoca~ktopl,
            acdoca~anln1,
            acdoca~anln2,
            acdoca~linetype,
            acdoca~mwskz,
            acdoca~ktosl
       FROM acdoca
           INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                              bkpf~belnr = acdoca~belnr and
                              bkpf~gjahr = acdoca~ryear
       INTO CORRESPONDING FIELDS OF TABLE @it_paso0B
       for ALL ENTRIES IN @it_paso0C
       WHERE acdoca~RLDNR  =      @it_paso0C-rldnr  and
             acdoca~RBUKRS =      @it_paso0C-RBUKRS and
             acdoca~GJAHR  =      @it_paso0C-GJAHR  and
             acdoca~BELNR  =      @it_paso0C-BELNR  and
             acdoca~RACCT  in     @rg_ctas_egreso  and
             acdoca~AWREF_REV = ''.
    endif.

    SELECT acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~AWITEM,
           acdoca~RACCT,
           acdoca~TSL,
           acdoca~SGTXT,
           acdoca~wsl,
           acdoca~rwcur,
           acdoca~hsl,
           acdoca~hbkid,
           acdoca~hktid,
           bkpf~awkey,
           bkpf~xblnr,
           acdoca~augbl,
           acdoca~AUGGJ,
           acdoca~AUGDT,
           acdoca~kunnr,
           bkpf~kursf,
           acdoca~lifnr,
           acdoca~ktopl,
           acdoca~anln1,
           acdoca~anln2,
           acdoca~linetype,
           acdoca~mwskz,
           acdoca~ktosl
      FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
      INTO CORRESPONDING FIELDS OF TABLE @it_paso1
      for ALL ENTRIES IN @it_paso0
      WHERE acdoca~RLDNR  =      @it_paso0-rldnr and
            acdoca~RBUKRS =      @it_paso0-RBUKRS and
            acdoca~GJAHR  =      @it_paso0-GJAHR and
            acdoca~AUGGJ  NE     '' AND
            acdoca~BELNR  =      @it_paso0-BELNR and
            acdoca~LIFNR  NOT IN @rg_prov_amex and
            acdoca~AWREF_REV = ''.

    if sy-subrc = 0.

      SELECT acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~AWITEM,
             acdoca~RACCT,
             acdoca~TSL,
             acdoca~SGTXT,
             acdoca~wsl,
             acdoca~rwcur,
             acdoca~hsl,
             acdoca~hbkid,
             acdoca~hktid,
             bkpf~awkey,
             bkpf~xblnr,
             acdoca~augbl,
             acdoca~AUGGJ,
             acdoca~AUGDT,
             acdoca~kunnr,
             bkpf~kursf,
             acdoca~lifnr,
             acdoca~ktopl,
             acdoca~anln1,
             acdoca~anln2,
             acdoca~linetype,
             acdoca~mwskz,
             acdoca~ktosl
        FROM acdoca
            INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                               bkpf~belnr = acdoca~belnr and
                               bkpf~gjahr = acdoca~ryear
        INTO CORRESPONDING FIELDS OF TABLE @it_paso2
        for ALL ENTRIES IN @it_paso1
        WHERE acdoca~RLDNR  =      @it_paso1-rldnr and
              acdoca~RBUKRS =      @it_paso1-RBUKRS and
              acdoca~AUGGJ  =      @it_paso1-AUGGJ and
              acdoca~AUGBL  =      @it_paso1-AUGBL and
              acdoca~BELNR  NE     @it_paso1-AUGBL and
              acdoca~blart  NOT IN @rg_docs_amex   AND
              acdoca~AWREF_REV = ''.

      if sy-subrc = 0.

        SELECT acdoca~RLDNR,
            acdoca~RBUKRS,
            acdoca~GJAHR,
            acdoca~POPER,
            acdoca~BLART,
            acdoca~BUDAT,
            acdoca~BLDAT,
            acdoca~BELNR,
            acdoca~AWITEM,
            acdoca~RACCT,
            acdoca~TSL,
            acdoca~SGTXT,
            acdoca~wsl,
            acdoca~rwcur,
            acdoca~hsl,
            acdoca~hbkid,
            acdoca~hktid,
            bkpf~awkey,
            bkpf~xblnr,
            acdoca~augbl,
            acdoca~AUGGJ,
            acdoca~AUGDT,
            acdoca~kunnr,
            bkpf~kursf,
            acdoca~lifnr,
            acdoca~ktopl,
            acdoca~anln1,
            acdoca~anln2,
            acdoca~linetype,
            acdoca~mwskz,
            acdoca~ktosl
       FROM acdoca
           INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                              bkpf~belnr = acdoca~belnr and
                              bkpf~gjahr = acdoca~ryear
       INTO CORRESPONDING FIELDS OF TABLE @it_paso3
       for ALL ENTRIES IN @it_paso2
       WHERE acdoca~RLDNR  = @it_paso2-rldnr and
             acdoca~RBUKRS = @it_paso2-RBUKRS and
             acdoca~GJAHR  = @it_paso2-GJAHR and
             acdoca~BELNR  = @it_paso2-BELNR and
             acdoca~AWREF_REV = ''.


      endif.

    endif.

  endif.

ENDFORM.


FORM GET_DATA  USING    IT_PASO1            type ZFI_TRASPASOST
                        IT_PASO1B           type ZFI_TRASPASOST
                        IT_PASO2            type ZFI_TRASPASOST
                        IT_PASO2B           type ZFI_TRASPASOST
                        IT_PASO3            type ZFI_TRASPASOST
                        IT_PASO4            type ZFI_TRASPASOST
                        IT_DETALLE          type ZFI_TRASPASOST
                        SOCIEDAD            type RSIS_T_RANGE
                        EJERCICIO           type gjahr
                        PERIODO             type RSIS_T_RANGE
                        AUGBL               type RSIS_T_RANGE
                        RG_DOCS_CONFIRMING  type RSIS_T_RANGE
                        RG_CTAS_EGRESO      type RSIS_T_RANGE
                        RG_CTAS_CONF        type RSIS_T_RANGE
                        RG_ACREEDORES_BANCO type RSIS_T_RANGE
                        RG_DOCS_TRASLADOS   type RSIS_T_RANGE
                        RG_DOCS_FACTURAS    TYPE RSIS_T_RANGE.


  SELECT     acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~AWITEM,
             acdoca~RACCT,
             acdoca~TSL,
             acdoca~SGTXT,
             acdoca~wsl,
             acdoca~rwcur,
             acdoca~hsl,
             acdoca~hbkid,
             acdoca~hktid,
             bkpf~awkey,
             bkpf~xblnr,
             acdoca~augbl,
             acdoca~AUGGJ,
             acdoca~AUGDT,
             acdoca~kunnr,
             bkpf~kursf,
             acdoca~lifnr,
             acdoca~ktopl,
             acdoca~anln1,
             acdoca~anln2,
             acdoca~linetype,
             acdoca~mwskz,
             acdoca~ktosl
        FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
        INTO CORRESPONDING FIELDS OF TABLE @it_paso1
        WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
              acdoca~rbukrs IN @sociedad           AND    "Sociedad
              acdoca~ryear  EQ @ejercicio          AND    "Ejercicio
              acdoca~belnr  IN @augbl              AND    "Doc Pago
              acdoca~poper  IN @periodo            AND    "Periodo
              acdoca~blart  IN @rg_docs_confirming AND
              acdoca~racct  IN @rg_ctas_egreso     AND
              acdoca~AWREF_REV = '' and
              acdoca~augbl  NE ''.

  if sy-subrc = 0.


    SELECT acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~AWITEM,
           acdoca~RACCT,
           acdoca~TSL,
           acdoca~SGTXT,
           acdoca~wsl,
           acdoca~rwcur,
           acdoca~hsl,
           acdoca~hbkid,
           acdoca~hktid,
           bkpf~awkey,
           bkpf~xblnr,
           acdoca~augbl,
           acdoca~AUGGJ,
           acdoca~AUGDT,
           acdoca~kunnr,
           bkpf~kursf,
           acdoca~lifnr,
           acdoca~ktopl,
           acdoca~anln1,
           acdoca~anln2,
           acdoca~linetype,
           acdoca~mwskz,
           acdoca~ktosl
     FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
      INTO CORRESPONDING FIELDS OF TABLE @it_paso1b
      for ALL ENTRIES IN @it_paso1
      WHERE acdoca~RLDNR  = @it_paso1-rldnr and
            acdoca~RBUKRS = @it_paso1-RBUKRS and
            acdoca~GJAHR  = @it_paso1-GJAHR and
            acdoca~BELNR  = @it_paso1-BELNR and
            acdoca~racct  IN @rg_ctas_conf and
            acdoca~lifnr  IN @rg_acreedores_banco and
            acdoca~AWREF_REV = ''.

    if sy-subrc = 0.

      SELECT acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~AWITEM,
             acdoca~RACCT,
             acdoca~TSL,
             acdoca~SGTXT,
             acdoca~wsl,
             acdoca~rwcur,
             acdoca~hsl,
             acdoca~hbkid,
             acdoca~hktid,
             bkpf~awkey,
             bkpf~xblnr,
             acdoca~augbl,
             acdoca~AUGGJ,
             acdoca~AUGDT,
             acdoca~kunnr,
             bkpf~kursf,
             acdoca~lifnr,
             acdoca~ktopl,
             acdoca~anln1,
             acdoca~anln2,
             acdoca~linetype,
             acdoca~mwskz,
             acdoca~ktosl
        FROM acdoca
            INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                               bkpf~belnr = acdoca~belnr and
                               bkpf~gjahr = acdoca~ryear
        INTO CORRESPONDING FIELDS OF TABLE @it_paso2
        for ALL ENTRIES IN @it_paso1
        WHERE acdoca~RLDNR  = @it_paso1-rldnr and
              acdoca~RBUKRS = @it_paso1-RBUKRS and
              acdoca~AUGGJ  = @it_paso1-GJAHR and
              acdoca~AUGBL  = @it_paso1-BELNR and
              acdoca~blart  IN @rg_docs_traslados and
*                          acdoca~lifnr  IN @rg_acreedores_banco and
              acdoca~AWREF_REV = ''.

      if sy-subrc = 0.
        SELECT acdoca~RLDNR,
               acdoca~RBUKRS,
               acdoca~GJAHR,
               acdoca~POPER,
               acdoca~BLART,
               acdoca~BUDAT,
               acdoca~BLDAT,
               acdoca~BELNR,
               acdoca~AWITEM,
               acdoca~RACCT,
               acdoca~TSL,
               acdoca~SGTXT,
               acdoca~wsl,
               acdoca~rwcur,
               acdoca~hsl,
               acdoca~hbkid,
               acdoca~hktid,
               bkpf~awkey,
               bkpf~xblnr,
               acdoca~augbl,
               acdoca~AUGGJ,
               acdoca~AUGDT,
               acdoca~kunnr,
               bkpf~kursf,
               acdoca~lifnr,
               acdoca~ktopl,
               acdoca~anln1,
               acdoca~anln2,
               acdoca~linetype,
               acdoca~mwskz,
               acdoca~ktosl
          FROM acdoca
              INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                                 bkpf~belnr = acdoca~belnr and
                                 bkpf~gjahr = acdoca~ryear
          INTO CORRESPONDING FIELDS OF TABLE @it_paso2b
          for ALL ENTRIES IN @it_paso2
          WHERE acdoca~RLDNR  = @it_paso2-rldnr and
                acdoca~RBUKRS = @it_paso2-RBUKRS and
                acdoca~GJAHR  = @it_paso2-GJAHR and
                acdoca~BELNR  = @it_paso2-BELNR and
*                                     acdoca~racct  IN @rg_ctas_conf and
                acdoca~lifnr  NOT IN @rg_acreedores_banco and
                acdoca~AWREF_REV = ''.

        if sy-subrc = 0.

          SELECT acdoca~RLDNR,
                 acdoca~RBUKRS,
                 acdoca~GJAHR,
                 acdoca~POPER,
                 acdoca~BLART,
                 acdoca~BUDAT,
                 acdoca~BLDAT,
                 acdoca~BELNR,
                 acdoca~AWITEM,
                 acdoca~RACCT,
                 acdoca~TSL,
                 acdoca~SGTXT,
                 acdoca~wsl,
                 acdoca~rwcur,
                 acdoca~hsl,
                 acdoca~hbkid,
                 acdoca~hktid,
                 bkpf~awkey,
                 bkpf~xblnr,
                 acdoca~augbl,
                 acdoca~AUGGJ,
                 acdoca~AUGDT,
                 acdoca~kunnr,
                 bkpf~kursf,
                 acdoca~lifnr,
                 acdoca~ktopl,
                 acdoca~anln1,
                 acdoca~anln2,
                 acdoca~linetype,
                 acdoca~mwskz,
                 acdoca~ktosl
            FROM acdoca
                INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                                   bkpf~belnr = acdoca~belnr and
                                   bkpf~gjahr = acdoca~ryear
            INTO CORRESPONDING FIELDS OF TABLE @it_paso3
            for ALL ENTRIES IN @it_paso2b
            WHERE acdoca~RLDNR  = @it_paso2b-rldnr and
                  acdoca~RBUKRS = @it_paso2b-RBUKRS and
                  acdoca~GJAHR  = @it_paso2b-AUGGJ and
                  acdoca~BELNR = @it_paso2b-AUGBL and
                  acdoca~AWITEM <> '' and
*                                           acdoca~blart  IN @rg_docs_traslados and
*                                          acdoca~lifnr  IN @rg_acreedores_banco and
                  acdoca~AWREF_REV = ''.

          if sy-subrc = 0.
            SELECT acdoca~RLDNR,
                acdoca~RBUKRS,
                acdoca~GJAHR,
                acdoca~POPER,
                acdoca~BLART,
                acdoca~BUDAT,
                acdoca~BLDAT,
                acdoca~BELNR,
                acdoca~AWITEM,
                acdoca~RACCT,
                acdoca~TSL,
                acdoca~SGTXT,
                acdoca~wsl,
                acdoca~rwcur,
                acdoca~hsl,
                acdoca~hbkid,
                acdoca~hktid,
                bkpf~awkey,
                bkpf~xblnr,
                acdoca~augbl,
                acdoca~AUGGJ,
                acdoca~AUGDT,
                acdoca~kunnr,
                bkpf~kursf,
                acdoca~lifnr,
                acdoca~ktopl,
                acdoca~anln1,
                acdoca~anln2,
                acdoca~linetype,
                acdoca~mwskz,
                acdoca~ktosl
           FROM acdoca
               INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                                  bkpf~belnr = acdoca~belnr and
                                  bkpf~gjahr = acdoca~ryear
           INTO CORRESPONDING FIELDS OF TABLE @it_paso4
           for ALL ENTRIES IN @it_paso3
           WHERE acdoca~RLDNR  = @it_paso3-rldnr and
                 acdoca~RBUKRS = @it_paso3-RBUKRS and
                 acdoca~AUGGJ  = @it_paso3-GJAHR and
                 acdoca~AUGBL  = @it_paso3-BELNR and
                 acdoca~blart  IN @rg_docs_facturas and
                 acdoca~AWREF_REV = ''.

            if sy-subrc = 0.
              SELECT acdoca~RLDNR,
                   acdoca~RBUKRS,
                   acdoca~GJAHR,
                   acdoca~POPER,
                   acdoca~BLART,
                   acdoca~BUDAT,
                   acdoca~BLDAT,
                   acdoca~BELNR,
                   acdoca~AWITEM,
                   acdoca~RACCT,
                   acdoca~TSL,
                   acdoca~SGTXT,
                   acdoca~wsl,
                   acdoca~rwcur,
                   acdoca~hsl,
                   acdoca~hbkid,
                   acdoca~hktid,
                   bkpf~awkey,
                   bkpf~xblnr,
                   acdoca~augbl,
                   acdoca~AUGGJ,
                   acdoca~AUGDT,
                   acdoca~kunnr,
                   bkpf~kursf,
                   acdoca~lifnr,
                   acdoca~ktopl,
                   acdoca~anln1,
                   acdoca~anln2,
                   acdoca~linetype,
                   acdoca~mwskz,
                   acdoca~ktosl
              FROM acdoca
                  INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                                     bkpf~belnr = acdoca~belnr and
                                     bkpf~gjahr = acdoca~ryear
              INTO CORRESPONDING FIELDS OF TABLE @it_detalle
              for ALL ENTRIES IN @it_paso4
              WHERE acdoca~RLDNR  = @it_paso4-rldnr and
                    acdoca~RBUKRS = @it_paso4-RBUKRS and
                    acdoca~GJAHR  = @it_paso4-GJAHR and
                    acdoca~BELNR  = @it_paso4-BELNR and
                    acdoca~AWREF_REV = ''.
            endif.
          endif.

        endif.

      endif.
    endif.

  endif.

ENDFORM.

FORM GET_DATA2  USING   IT_PASO1            type ZFI_TRASPASOST
                        IT_PASO1B           type ZFI_TRASPASOST
                        IT_PASO2            type ZFI_TRASPASOST
                        IT_PASO2B           type ZFI_TRASPASOST
                        IT_PASO3            type ZFI_TRASPASOST
                        IT_PASO4            type ZFI_TRASPASOST
                        IT_PASOX            type ZFI_TRASPASOST
                        IT_PASOXB           type ZFI_TRASPASOST
                        IT_DETALLE          type ZFI_TRASPASOST
                        SOCIEDAD            type RSIS_T_RANGE
                        EJERCICIO           type gjahr
                        PERIODO             type RSIS_T_RANGE
                        AUGBL               type RSIS_T_RANGE
                        RG_DOCS_CONFIRMING  type RSIS_T_RANGE
                        RG_CTAS_EGRESO      type RSIS_T_RANGE
                        RG_CTAS_CONF        type RSIS_T_RANGE
                        RG_ACREEDORES_BANCO type RSIS_T_RANGE
                        RG_DOCS_TRASLADOS   type RSIS_T_RANGE
                        RG_DOCS_FACTURAS    TYPE RSIS_T_RANGE.

*Se buscan las salidas de banco

  SELECT     acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~AWITEM,
             acdoca~RACCT,
             acdoca~TSL,
             acdoca~SGTXT,
             acdoca~wsl,
             acdoca~rwcur,
             acdoca~hsl,
             acdoca~hbkid,
             acdoca~hktid,
             bkpf~awkey,
             bkpf~xblnr,
             acdoca~augbl,
             acdoca~AUGGJ,
             acdoca~AUGDT,
             acdoca~kunnr,
             bkpf~kursf,
             acdoca~lifnr,
             acdoca~ktopl,
             acdoca~anln1,
             acdoca~anln2,
             acdoca~linetype,
             acdoca~mwskz,
             acdoca~ktosl
        FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
        INTO CORRESPONDING FIELDS OF TABLE @it_paso1
        WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
              acdoca~rbukrs IN @sociedad           AND    "Sociedad
              acdoca~ryear  EQ @ejercicio          AND    "Ejercicio
              acdoca~belnr  IN @augbl              AND    "Doc Pago
              acdoca~poper  IN @periodo            AND    "Periodo
              acdoca~blart  IN @rg_docs_confirming AND
              acdoca~racct  IN @rg_ctas_egreso     AND
              acdoca~AWREF_REV = '' and
              acdoca~augbl  NE ''.

  if sy-subrc = 0.

*Luego se buscan que tengan acreedor Santander
    SELECT acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~AWITEM,
           acdoca~RACCT,
           acdoca~TSL,
           acdoca~SGTXT,
           acdoca~wsl,
           acdoca~rwcur,
           acdoca~hsl,
           acdoca~hbkid,
           acdoca~hktid,
           bkpf~awkey,
           bkpf~xblnr,
           acdoca~augbl,
           acdoca~AUGGJ,
           acdoca~AUGDT,
           acdoca~kunnr,
           bkpf~kursf,
           acdoca~lifnr,
           acdoca~ktopl,
           acdoca~anln1,
           acdoca~anln2,
           acdoca~linetype,
           acdoca~mwskz,
           acdoca~ktosl
     FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
      INTO CORRESPONDING FIELDS OF TABLE @it_paso1b
      for ALL ENTRIES IN @it_paso1
      WHERE acdoca~RLDNR  = @it_paso1-rldnr and
            acdoca~RBUKRS = @it_paso1-RBUKRS and
            acdoca~GJAHR  = @it_paso1-GJAHR and
            acdoca~BELNR  = @it_paso1-BELNR and
            acdoca~racct  IN @rg_ctas_conf and
            acdoca~lifnr  IN @rg_acreedores_banco and
            acdoca~AWREF_REV = ''.

    if sy-subrc = 0.
*Busca la primera compensación

      SELECT acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~AWITEM,
             acdoca~RACCT,
             acdoca~TSL,
             acdoca~SGTXT,
             acdoca~wsl,
             acdoca~rwcur,
             acdoca~hsl,
             acdoca~hbkid,
             acdoca~hktid,
             bkpf~awkey,
             bkpf~xblnr,
             acdoca~augbl,
             acdoca~AUGGJ,
             acdoca~AUGDT,
             acdoca~kunnr,
             bkpf~kursf,
             acdoca~lifnr,
             acdoca~ktopl,
             acdoca~anln1,
             acdoca~anln2,
             acdoca~linetype,
             acdoca~mwskz,
             acdoca~ktosl
        FROM acdoca
            INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                               bkpf~belnr = acdoca~belnr and
                               bkpf~gjahr = acdoca~ryear
        INTO CORRESPONDING FIELDS OF TABLE @it_paso2
        for ALL ENTRIES IN @it_paso1
        WHERE acdoca~RLDNR  = @it_paso1-rldnr and
              acdoca~RBUKRS = @it_paso1-RBUKRS and
              acdoca~AUGGJ  = @it_paso1-GJAHR and
              acdoca~AUGBL  = @it_paso1-BELNR and
              acdoca~blart  IN @rg_docs_traslados and
              acdoca~AWREF_REV = ''.

      if sy-subrc = 0.
*Busca la contrapartida de esa compensación
        SELECT acdoca~RLDNR,
               acdoca~RBUKRS,
               acdoca~GJAHR,
               acdoca~POPER,
               acdoca~BLART,
               acdoca~BUDAT,
               acdoca~BLDAT,
               acdoca~BELNR,
               acdoca~AWITEM,
               acdoca~RACCT,
               acdoca~TSL,
               acdoca~SGTXT,
               acdoca~wsl,
               acdoca~rwcur,
               acdoca~hsl,
               acdoca~hbkid,
               acdoca~hktid,
               bkpf~awkey,
               bkpf~xblnr,
               acdoca~augbl,
               acdoca~AUGGJ,
               acdoca~AUGDT,
               acdoca~kunnr,
               bkpf~kursf,
               acdoca~lifnr,
               acdoca~ktopl,
               acdoca~anln1,
               acdoca~anln2,
               acdoca~linetype,
               acdoca~mwskz,
               acdoca~ktosl
          FROM acdoca
              INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                                 bkpf~belnr = acdoca~belnr and
                                 bkpf~gjahr = acdoca~ryear
          INTO CORRESPONDING FIELDS OF TABLE @it_paso2b
          for ALL ENTRIES IN @it_paso2
          WHERE acdoca~RLDNR  = @it_paso2-rldnr and
                acdoca~RBUKRS = @it_paso2-RBUKRS and
                acdoca~GJAHR  = @it_paso2-GJAHR and
                acdoca~BELNR  = @it_paso2-BELNR and
                acdoca~lifnr  NOT IN @rg_acreedores_banco and
                acdoca~AWREF_REV = ''.

        if sy-subrc = 0.
*Buscamos la segunda compensación
          SELECT acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~AWITEM,
             acdoca~RACCT,
             acdoca~TSL,
             acdoca~SGTXT,
             acdoca~wsl,
             acdoca~rwcur,
             acdoca~hsl,
             acdoca~hbkid,
             acdoca~hktid,
             bkpf~awkey,
             bkpf~xblnr,
             acdoca~augbl,
             acdoca~AUGGJ,
             acdoca~AUGDT,
             acdoca~kunnr,
             bkpf~kursf,
             acdoca~lifnr,
             acdoca~ktopl,
             acdoca~anln1,
             acdoca~anln2,
             acdoca~linetype,
             acdoca~mwskz,
             acdoca~ktosl
        FROM acdoca
            INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                               bkpf~belnr = acdoca~belnr and
                               bkpf~gjahr = acdoca~ryear
        INTO CORRESPONDING FIELDS OF TABLE @it_pasox
        for ALL ENTRIES IN @it_paso2B
        WHERE acdoca~RLDNR  = @it_paso2B-rldnr and
              acdoca~RBUKRS = @it_paso2B-RBUKRS and
              acdoca~GJAHR  = @it_paso2B-AUGGJ and
              acdoca~BELNR  = @it_paso2B-AUGBL and
*              acdoca~blart  IN @rg_docs_traslados and
              acdoca~AWREF_REV = ''.
          if sy-subrc = 0.
*Busca la contrapartida de esa compensación
            SELECT acdoca~RLDNR,
                   acdoca~RBUKRS,
                   acdoca~GJAHR,
                   acdoca~POPER,
                   acdoca~BLART,
                   acdoca~BUDAT,
                   acdoca~BLDAT,
                   acdoca~BELNR,
                   acdoca~AWITEM,
                   acdoca~RACCT,
                   acdoca~TSL,
                   acdoca~SGTXT,
                   acdoca~wsl,
                   acdoca~rwcur,
                   acdoca~hsl,
                   acdoca~hbkid,
                   acdoca~hktid,
                   bkpf~awkey,
                   bkpf~xblnr,
                   acdoca~augbl,
                   acdoca~AUGGJ,
                   acdoca~AUGDT,
                   acdoca~kunnr,
                   bkpf~kursf,
                   acdoca~lifnr,
                   acdoca~ktopl,
                   acdoca~anln1,
                   acdoca~anln2,
                   acdoca~linetype,
                   acdoca~mwskz,
                   acdoca~ktosl
              FROM acdoca
                  INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                                     bkpf~belnr = acdoca~belnr and
                                     bkpf~gjahr = acdoca~ryear
              INTO CORRESPONDING FIELDS OF TABLE @it_pasoxb
              for ALL ENTRIES IN @it_pasox
              WHERE acdoca~RLDNR  = @it_pasox-rldnr and
                    acdoca~RBUKRS = @it_pasox-RBUKRS and
                    acdoca~GJAHR  = @it_pasox-GJAHR and
                    acdoca~BELNR  = @it_pasox-BELNR and
                    acdoca~lifnr  NOT IN @rg_acreedores_banco and
                    acdoca~AWREF_REV = ''.

            if sy-subrc = 0.
*Se buscan las facturas que compensa la segunda compensación
*              SELECT acdoca~RLDNR,
*                     acdoca~RBUKRS,
*                     acdoca~GJAHR,
*                     acdoca~POPER,
*                     acdoca~BLART,
*                     acdoca~BUDAT,
*                     acdoca~BLDAT,
*                     acdoca~BELNR,
*                     acdoca~AWITEM,
*                     acdoca~RACCT,
*                     acdoca~TSL,
*                     acdoca~SGTXT,
*                     acdoca~wsl,
*                     acdoca~rwcur,
*                     acdoca~hsl,
*                     acdoca~hbkid,
*                     acdoca~hktid,
*                     bkpf~awkey,
*                     bkpf~xblnr,
*                     acdoca~augbl,
*                     acdoca~AUGGJ,
*                     acdoca~AUGDT,
*                     acdoca~kunnr,
*                     bkpf~kursf,
*                     acdoca~lifnr,
*                     acdoca~ktopl,
*                     acdoca~anln1,
*                     acdoca~anln2,
*                     acdoca~linetype,
*                     acdoca~mwskz,
*                     acdoca~ktosl
*                FROM acdoca
*                    INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
*                                       bkpf~belnr = acdoca~belnr and
*                                       bkpf~gjahr = acdoca~ryear
*                INTO CORRESPONDING FIELDS OF TABLE @it_paso3
*                for ALL ENTRIES IN @it_pasoxb
*                WHERE acdoca~RLDNR  = @it_pasoxb-rldnr and
*                      acdoca~RBUKRS = @it_pasoxb-RBUKRS and
*                      acdoca~AUGGJ  = @it_pasoxb-GJAHR and
*                      acdoca~AUGBL  = @it_pasoxb-BELNR and
*
*                      acdoca~AWREF_REV = ''.
*
*              if sy-subrc = 0.
*Aca detalles de cabecera
              SELECT acdoca~RLDNR,
                  acdoca~RBUKRS,
                  acdoca~GJAHR,
                  acdoca~POPER,
                  acdoca~BLART,
                  acdoca~BUDAT,
                  acdoca~BLDAT,
                  acdoca~BELNR,
                  acdoca~AWITEM,
                  acdoca~RACCT,
                  acdoca~TSL,
                  acdoca~SGTXT,
                  acdoca~wsl,
                  acdoca~rwcur,
                  acdoca~hsl,
                  acdoca~hbkid,
                  acdoca~hktid,
                  bkpf~awkey,
                  bkpf~xblnr,
                  acdoca~augbl,
                  acdoca~AUGGJ,
                  acdoca~AUGDT,
                  acdoca~kunnr,
                  bkpf~kursf,
                  acdoca~lifnr,
                  acdoca~ktopl,
                  acdoca~anln1,
                  acdoca~anln2,
                  acdoca~linetype,
                  acdoca~mwskz,
                  acdoca~ktosl
             FROM acdoca
                 INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                                    bkpf~belnr = acdoca~belnr and
                                    bkpf~gjahr = acdoca~ryear
             INTO CORRESPONDING FIELDS OF TABLE @it_paso4
             for ALL ENTRIES IN @it_pasoxb
             WHERE acdoca~RLDNR  = @it_pasoxb-rldnr and
                   acdoca~RBUKRS = @it_pasoxb-RBUKRS and
                   acdoca~AUGGJ  = @it_pasoxb-GJAHR and
                   acdoca~AUGBL  = @it_pasoxb-BELNR and
                   acdoca~blart  IN @rg_docs_facturas and
                   acdoca~AWREF_REV = ''.

              if sy-subrc = 0.
*Por último los detalles
                SELECT acdoca~RLDNR,
                     acdoca~RBUKRS,
                     acdoca~GJAHR,
                     acdoca~POPER,
                     acdoca~BLART,
                     acdoca~BUDAT,
                     acdoca~BLDAT,
                     acdoca~BELNR,
                     acdoca~AWITEM,
                     acdoca~RACCT,
                     acdoca~TSL,
                     acdoca~SGTXT,
                     acdoca~wsl,
                     acdoca~rwcur,
                     acdoca~hsl,
                     acdoca~hbkid,
                     acdoca~hktid,
                     bkpf~awkey,
                     bkpf~xblnr,
                     acdoca~augbl,
                     acdoca~AUGGJ,
                     acdoca~AUGDT,
                     acdoca~kunnr,
                     bkpf~kursf,
                     acdoca~lifnr,
                     acdoca~ktopl,
                     acdoca~anln1,
                     acdoca~anln2,
                     acdoca~linetype,
                     acdoca~mwskz,
                     acdoca~ktosl
                FROM acdoca
                    INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                                       bkpf~belnr = acdoca~belnr and
                                       bkpf~gjahr = acdoca~ryear
                INTO CORRESPONDING FIELDS OF TABLE @it_detalle
                for ALL ENTRIES IN @it_paso4
                WHERE acdoca~RLDNR  = @it_paso4-rldnr and
                      acdoca~RBUKRS = @it_paso4-RBUKRS and
                      acdoca~GJAHR  = @it_paso4-GJAHR and
                      acdoca~BELNR  = @it_paso4-BELNR and
                      acdoca~AWREF_REV = ''.
              endif.

            endif.

          endif.
        endif.

      endif.
    endif.

  endif.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_DATA_ANTICIPOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_PASO0
*&      --> IT_PASO1
*&      --> IT_PASO2
*&      --> IT_PASO3
*&      --> RG_DOCS_ANTICIPO
*&      --> RG_CTAS_ANTICIPO
*&      --> RG_DOC_COB
*&---------------------------------------------------------------------*
FORM GET_DATA_ANTICIPOS  USING    IT_PASO0         type ZFI_TRASPASOST
                                  IT_PASO0B        type ZFI_TRASPASOST
                                  IT_PASO1         type ZFI_TRASPASOST
                                  IT_PASO2         type ZFI_TRASPASOST
                                  IT_PASO3         type ZFI_TRASPASOST
                                  IT_PASO4         type ZFI_TRASPASOST
                                  IT_PASO5         type ZFI_TRASPASOST
                                  SOCIEDAD         type RSIS_T_RANGE
                                  EJERCICIO        type gjahr
                                  PERIODO          type RSIS_T_RANGE
                                  RG_DOCS_ANTICIPO type RSIS_T_RANGE
                                  RG_CTAS_ANTICIPO type RSIS_T_RANGE
                                  RG_DOC_COB       type RSIS_T_RANGE
                                  RG_DOC_MIRO      type RSIS_T_RANGE
                                  RG_CTAS_EGRESOS  type RSIS_T_RANGE.

  SELECT   acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~AWITEM,
             acdoca~RACCT,
             acdoca~TSL,
             acdoca~SGTXT,
             acdoca~wsl,
             acdoca~rwcur,
             acdoca~hsl,
             acdoca~hbkid,
             acdoca~hktid,
             acdoca~ktopl,
             bkpf~awkey,
             bkpf~xblnr,
             acdoca~augbl,
             acdoca~AUGGJ,
             acdoca~AUGDT,
             acdoca~kunnr,
             bkpf~kursf,
             acdoca~lifnr
        FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
        INTO CORRESPONDING FIELDS OF TABLE @it_paso0
        WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
              acdoca~rbukrs IN @sociedad           AND    "Sociedad
              acdoca~ryear  EQ @ejercicio          AND    "Ejercicio
              acdoca~poper  IN @periodo            AND    "Periodo
              acdoca~blart  IN @rg_docs_anticipo   AND
              acdoca~racct  IN @rg_ctas_anticipo   AND
              acdoca~AWREF_REV = ''.

  if sy-subrc = 0.
    SELECT   acdoca~RLDNR,
               acdoca~RBUKRS,
               acdoca~GJAHR,
               acdoca~POPER,
               acdoca~BLART,
               acdoca~BUDAT,
               acdoca~BLDAT,
               acdoca~BELNR,
               acdoca~AWITEM,
               acdoca~RACCT,
               acdoca~TSL,
               acdoca~SGTXT,
               acdoca~wsl,
               acdoca~rwcur,
               acdoca~hsl,
               acdoca~hbkid,
               acdoca~hktid,
               acdoca~ktopl,
               bkpf~awkey,
               bkpf~xblnr,
               acdoca~augbl,
               acdoca~AUGGJ,
               acdoca~AUGDT,
               acdoca~kunnr,
               bkpf~kursf,
               acdoca~lifnr
          FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
          INTO CORRESPONDING FIELDS OF TABLE @it_paso0B
          FOR ALL ENTRIES IN @it_paso0
          WHERE acdoca~rldnr  EQ @it_paso0-rldnr     AND    "Ledger
                acdoca~rbukrs EQ @it_paso0-rbukrs    AND    "Sociedad
                acdoca~belnr  EQ @it_paso0-belnr     AND    "Nro documento
                acdoca~gjahr  EQ @it_paso0-gjahr     AND    "Ejercicio
                acdoca~racct  in @RG_CTAS_EGRESOS.
  endif.

  loop at it_paso0 into DATA(iw_paso0) where augbl <> ''.
    append iw_paso0 to it_paso1.
  endloop.

  loop at it_paso0 into iw_paso0 where augbl = ''.
    append iw_paso0 to it_paso2.
  endloop.

  if it_paso1 is NOT INITIAL.

    SELECT acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~AWITEM,
           acdoca~RACCT,
           acdoca~TSL,
           acdoca~SGTXT,
           acdoca~wsl,
           acdoca~rwcur,
           acdoca~hsl,
           acdoca~hbkid,
           acdoca~hktid,
           acdoca~ktopl,
           bkpf~awkey,
           bkpf~xblnr,
           acdoca~augbl,
           acdoca~AUGGJ,
           acdoca~AUGDT,
           acdoca~kunnr,
           bkpf~kursf,
           acdoca~lifnr
      FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
      INTO CORRESPONDING FIELDS OF TABLE @it_paso3
      for ALL ENTRIES IN @it_paso1
      WHERE acdoca~RLDNR  = @it_paso1-rldnr and
            acdoca~RBUKRS = @it_paso1-RBUKRS and
            acdoca~GJAHR  = @it_paso1-AUGGJ and
            acdoca~BELNR  = @it_paso1-AUGBL and
            acdoca~AUGBL  ne '' and
            acdoca~blart  NOT IN @rg_docs_anticipo.
*         AND
*            acdoca~AWREF_REV = ''.

    if sy-subrc = 0.
      SELECT acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~AWITEM,
         acdoca~RACCT,
         acdoca~TSL,
         acdoca~SGTXT,
         acdoca~wsl,
         acdoca~rwcur,
         acdoca~hsl,
         acdoca~hbkid,
         acdoca~hktid,
         acdoca~ktopl,
         bkpf~awkey,
         bkpf~xblnr,
         acdoca~augbl,
         acdoca~AUGGJ,
         acdoca~AUGDT,
         acdoca~kunnr,
         bkpf~kursf,
         acdoca~lifnr
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO CORRESPONDING FIELDS OF TABLE @it_paso4
    for ALL ENTRIES IN @it_paso3
    WHERE acdoca~RLDNR  = @it_paso3-rldnr and
          acdoca~RBUKRS = @it_paso3-RBUKRS and
          acdoca~AUGGJ  = @it_paso3-GJAHR and
          acdoca~AUGBL  = @it_paso3-BELNR and
          acdoca~blart  IN @rg_doc_miro.

      if sy-subrc = 0.
        SELECT acdoca~RLDNR,
     acdoca~RBUKRS,
     acdoca~GJAHR,
     acdoca~POPER,
     acdoca~BLART,
     acdoca~BUDAT,
     acdoca~BLDAT,
     acdoca~BELNR,
     acdoca~AWITEM,
     acdoca~RACCT,
     acdoca~TSL,
     acdoca~SGTXT,
     acdoca~wsl,
     acdoca~rwcur,
     acdoca~hsl,
     acdoca~hbkid,
     acdoca~hktid,
     bkpf~awkey,
     bkpf~xblnr,
     acdoca~augbl,
     acdoca~AUGGJ,
     acdoca~AUGDT,
     acdoca~kunnr,
     bkpf~kursf,
     acdoca~lifnr,
     acdoca~LINETYPE,
     acdoca~MWSKZ,
     acdoca~KTOSL,
     acdoca~ktopl
FROM acdoca
    INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                       bkpf~belnr = acdoca~belnr and
                       bkpf~gjahr = acdoca~ryear
INTO CORRESPONDING FIELDS OF TABLE @it_paso5
for ALL ENTRIES IN @it_paso4
WHERE acdoca~RLDNR  = @it_paso4-rldnr and
      acdoca~RBUKRS = @it_paso4-RBUKRS and
      acdoca~GJAHR  = @it_paso4-GJAHR and
      acdoca~BELNR  = @it_paso4-BELNR.
      endif.
*         and
*          acdoca~AWREF_REV = '' and
*          acdoca~BLART  not in @rg_doc_cob.

    endif.
  endif.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_REEMBOLSO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_PASO1
*&      --> IT_PASO2
*&      --> IT_PASO3
*&      --> IT_PASO4
*&      --> IT_PASO5
*&      --> SOCIEDAD
*&      --> EJERC
*&      --> PERIODO
*&      --> RG_CTAS_EGRESO
*&      --> RG_DOCS_COMPROBA
*&---------------------------------------------------------------------*
FORM GET_DATA_REEMBOLSO  USING    IT_PASO1 type ZFI_TRASPASOST "Pago
                                  IT_PASO2 type ZFI_TRASPASOST "Pago partidas banco
                                  IT_PASO3 type ZFI_TRASPASOST "Docs de comprobación de gastos
                                  IT_PASO3all type ZFI_TRASPASOST "Docs de comprobación de gastos
                                  SOCIEDAD type RSIS_T_RANGE
                                  EJERC type gjahr
                                  PERIODO type RSIS_T_RANGE
                                  AUGBL   type RSIS_T_RANGE
                                  RG_CTAS_EGRESO type RSIS_T_RANGE
                                  RG_DOCS_COMPROBA type RSIS_T_RANGE
                                  rg_docs_pagoacre type RSIS_T_RANGE.
  data tabix TYPE sy-tabix.

  SELECT acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~AWITEM,
         acdoca~RACCT,
         acdoca~TSL,
         acdoca~SGTXT,
         acdoca~wsl,
         acdoca~rwcur,
         acdoca~hsl,
         acdoca~hbkid,
         acdoca~hktid,
         acdoca~ktopl,
         bkpf~awkey,
         bkpf~xblnr,
         acdoca~augbl,
         acdoca~AUGGJ,
         acdoca~AUGDT,
         acdoca~kunnr,
         bkpf~kursf,
         acdoca~lifnr,
         acdoca~linetype,
         acdoca~mwskz
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO CORRESPONDING FIELDS OF TABLE @it_paso1
    WHERE acdoca~RLDNR  EQ @c_0l           and
          acdoca~RBUKRS IN @sociedad and
          acdoca~GJAHR  EQ @ejerc and
          acdoca~BELNR  in @augbl and
          acdoca~BLART  in @rg_docs_pagoacre and
          acdoca~poper  IN @periodo          AND
          acdoca~LIFNR = ''.
  sort it_paso1 by rldnr rbukrs gjahr.

  if sy-subrc = 0.

    SELECT   acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~AWITEM,
             acdoca~RACCT,
             acdoca~TSL,
             acdoca~SGTXT,
             acdoca~wsl,
             acdoca~rwcur,
             acdoca~hsl,
             acdoca~hbkid,
             acdoca~hktid,
             acdoca~ktopl,
             bkpf~awkey,
             bkpf~xblnr,
             acdoca~augbl,
             acdoca~AUGGJ,
             acdoca~AUGDT,
             acdoca~kunnr,
             bkpf~kursf,
             acdoca~lifnr,
             acdoca~linetype,
             acdoca~mwskz
        FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
        INTO CORRESPONDING FIELDS OF TABLE @it_paso3
        FOR ALL ENTRIES IN @it_paso1
        WHERE acdoca~rldnr  EQ @it_paso1-rldnr     AND    "Ledger
              acdoca~rbukrs EQ @it_paso1-rbukrs    AND    "Sociedad
              acdoca~auggj  EQ @it_paso1-gjahr     AND    "Ejercicio
              acdoca~augbl  EQ @it_paso1-belnr     AND
              acdoca~blart  not in @rg_docs_pagoacre AND
              acdoca~blart  in @rg_docs_comproba.    "Ejercicio.7

    sort it_paso3 by rldnr rbukrs gjahr.

    if sy-subrc = 0.

      SELECT   acdoca~RLDNR,
               acdoca~RBUKRS,
               acdoca~GJAHR,
               acdoca~POPER,
               acdoca~BLART,
               acdoca~BUDAT,
               acdoca~BLDAT,
               acdoca~BELNR,
               acdoca~AWITEM,
               acdoca~RACCT,
               acdoca~TSL,
               acdoca~SGTXT,
               acdoca~wsl,
               acdoca~rwcur,
               acdoca~hsl,
               acdoca~hbkid,
               acdoca~hktid,
               acdoca~ktopl,
               bkpf~awkey,
               bkpf~xblnr,
               acdoca~augbl,
               acdoca~AUGGJ,
               acdoca~AUGDT,
               acdoca~kunnr,
               bkpf~kursf,
               acdoca~lifnr,
               acdoca~linetype,
               acdoca~mwskz
          FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
          INTO CORRESPONDING FIELDS OF TABLE @it_paso3all
          FOR ALL ENTRIES IN @it_paso3
          WHERE acdoca~rldnr  EQ @it_paso3-rldnr     AND    "Ledger
                acdoca~rbukrs EQ @it_paso3-rbukrs    AND    "Sociedad
                acdoca~gjahr  EQ @it_paso3-gjahr     AND    "Ejercicio
                acdoca~belnr  EQ @it_paso3-belnr.

      sort it_paso3 by rldnr rbukrs gjahr.

      loop at it_paso1 into DATA(iw_paso1).
        tabix = sy-tabix.
        READ TABLE it_paso3 into DATA(iw_paso3) with key rldnr  = iw_paso1-rldnr
                                                         rbukrs = iw_paso1-rbukrs
                                                         auggj  = iw_paso1-gjahr
                                                         augbl  = iw_paso1-belnr.

        if sy-subrc ne 0.
          delete it_paso1 INDEX tabix.
        ENDIF.
      ENDLOOP.

      if it_paso1 is not INITIAL.

        SELECT acdoca~RLDNR,
               acdoca~RBUKRS,
               acdoca~GJAHR,
               acdoca~POPER,
               acdoca~BLART,
               acdoca~BUDAT,
               acdoca~BLDAT,
               acdoca~BELNR,
               acdoca~AWITEM,
               acdoca~RACCT,
               acdoca~TSL,
               acdoca~SGTXT,
               acdoca~wsl,
               acdoca~rwcur,
               acdoca~hsl,
               acdoca~hbkid,
               acdoca~hktid,
               acdoca~ktopl,
               bkpf~awkey,
               bkpf~xblnr,
               acdoca~augbl,
               acdoca~AUGGJ,
               acdoca~AUGDT,
               acdoca~kunnr,
               bkpf~kursf,
               acdoca~lifnr,
               acdoca~linetype,
               acdoca~mwskz
          FROM acdoca
              INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                                 bkpf~belnr = acdoca~belnr and
                                 bkpf~gjahr = acdoca~ryear
          INTO CORRESPONDING FIELDS OF TABLE @it_paso2
          for ALL ENTRIES IN @it_paso1
          WHERE acdoca~RLDNR  = @it_paso1-rldnr and
                acdoca~RBUKRS = @it_paso1-RBUKRS and
                acdoca~GJAHR  = @it_paso1-GJAHR and
                acdoca~BELNR  = @it_paso1-BELNR and
                acdoca~RACCT  in @RG_CTAS_EGRESO.
      endif.
    endif.

  endif.


ENDFORM.

FORM GET_DATA_GENERAL    USING    IT_PASO1 type ZFI_TRASPASOST "Pago
                                  IT_PASO2 type ZFI_TRASPASOST "Pago partidas banco
                                  IT_PASO3 type ZFI_TRASPASOST "Docs de comprobación de gastos
                                  IT_PASO3all type ZFI_TRASPASOST "Docs de comprobación de gastos
                                  SOCIEDAD type RSIS_T_RANGE
                                  EJERC type gjahr
                                  PERIODO type RSIS_T_RANGE
                                  AUGBL   type RSIS_T_RANGE
                                  RG_CTAS_EGRESO type RSIS_T_RANGE
                                  RG_DOCS_COMPROBA type RSIS_T_RANGE
                                  rg_docs_pagoacre type RSIS_T_RANGE
                                  rg_ctas_exclude  type RSIS_T_RANGE.
  data: tabix     TYPE sy-tabix,
        IT_egreso type ZFI_TRASPASOST,
        valori    type i.

  SELECT acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~AWITEM,
         acdoca~RACCT,
         acdoca~TSL,
         acdoca~SGTXT,
         acdoca~wsl,
         acdoca~rwcur,
         acdoca~hsl,
         acdoca~hbkid,
         acdoca~hktid,
         acdoca~ktopl,
         bkpf~awkey,
         bkpf~xblnr,
         acdoca~augbl,
         acdoca~AUGGJ,
         acdoca~AUGDT,
         acdoca~kunnr,
         bkpf~kursf,
         acdoca~lifnr,
         acdoca~linetype,
         acdoca~mwskz
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO CORRESPONDING FIELDS OF TABLE @it_paso1
    WHERE acdoca~RLDNR  EQ @c_0l           and
          acdoca~RBUKRS IN @sociedad and
          acdoca~GJAHR  EQ @ejerc and
          acdoca~BELNR  in @augbl and
          acdoca~BLART  in @rg_docs_pagoacre and
          acdoca~poper  IN @periodo AND
          acdoca~racct  in @RG_CTAS_EGRESO.
*          acdoca~racct not in @rg_ctas_exclude.

  sort it_paso1 by rldnr rbukrs gjahr.

  if sy-subrc = 0.

    SELECT   acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~AWITEM,
             acdoca~RACCT,
             acdoca~TSL,
             acdoca~SGTXT,
             acdoca~wsl,
             acdoca~rwcur,
             acdoca~hsl,
             acdoca~hbkid,
             acdoca~hktid,
             acdoca~ktopl,
             bkpf~awkey,
             bkpf~xblnr,
             acdoca~augbl,
             acdoca~AUGGJ,
             acdoca~AUGDT,
             acdoca~kunnr,
             bkpf~kursf,
             acdoca~lifnr,
             acdoca~linetype,
             acdoca~mwskz
        FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
        INTO CORRESPONDING FIELDS OF TABLE @it_paso3
        FOR ALL ENTRIES IN @it_paso1
        WHERE acdoca~rldnr  EQ @it_paso1-rldnr     AND    "Ledger
              acdoca~rbukrs EQ @it_paso1-rbukrs    AND    "Sociedad
              acdoca~auggj  EQ @it_paso1-gjahr     AND    "Ejercicio
              acdoca~augbl  EQ @it_paso1-belnr .
*          AND
*              acdoca~blart  not in @rg_docs_pagoacre AND
*              acdoca~blart  not in @rg_docs_comproba.    "Ejercicio.7

    sort it_paso3 by rldnr rbukrs gjahr.

    if sy-subrc = 0.

      SELECT   acdoca~RLDNR,
               acdoca~RBUKRS,
               acdoca~GJAHR,
               acdoca~POPER,
               acdoca~BLART,
               acdoca~BUDAT,
               acdoca~BLDAT,
               acdoca~BELNR,
               acdoca~AWITEM,
               acdoca~RACCT,
               acdoca~TSL,
               acdoca~SGTXT,
               acdoca~wsl,
               acdoca~rwcur,
               acdoca~hsl,
               acdoca~hbkid,
               acdoca~hktid,
               acdoca~ktopl,
               bkpf~awkey,
               bkpf~xblnr,
               acdoca~augbl,
               acdoca~AUGGJ,
               acdoca~AUGDT,
               acdoca~kunnr,
               bkpf~kursf,
               acdoca~lifnr,
               acdoca~linetype,
               acdoca~mwskz,
               acdoca~buzei
          FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
          INTO CORRESPONDING FIELDS OF TABLE @it_paso3all
          FOR ALL ENTRIES IN @it_paso3
          WHERE acdoca~rldnr  EQ @it_paso3-rldnr     AND    "Ledger
                acdoca~rbukrs EQ @it_paso3-rbukrs    AND    "Sociedad
                acdoca~gjahr  EQ @it_paso3-gjahr     AND    "Ejercicio
                acdoca~belnr  EQ @it_paso3-belnr     AND
                acdoca~racct not in @rg_ctas_exclude.


      if sy-subrc = 0.

*
*        loop at it_paso3all into DATA(iw_paso3all).
*          tabix = sy-tabix.
*          valori = iw_paso3all-awitem.
*          iw_paso3all-buzei = valori.
*          modify  it_paso3all from iw_paso3all INDEX tabix.
*        ENDLOOP.

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
     into CORRESPONDING FIELDS OF table @it_egreso.

        it_paso3all[] = it_egreso[].


      endif.

      sort it_paso3 by rldnr rbukrs gjahr.

      loop at it_paso1 into DATA(iw_paso1).
        tabix = sy-tabix.
        READ TABLE it_paso3 into DATA(iw_paso3) with key rldnr  = iw_paso1-rldnr
                                                         rbukrs = iw_paso1-rbukrs
                                                         auggj  = iw_paso1-gjahr
                                                         augbl  = iw_paso1-belnr.

        if sy-subrc ne 0.
          delete it_paso1 INDEX tabix.
        ENDIF.
      ENDLOOP.

      if it_paso1 is not INITIAL.

        SELECT acdoca~RLDNR,
               acdoca~RBUKRS,
               acdoca~GJAHR,
               acdoca~POPER,
               acdoca~BLART,
               acdoca~BUDAT,
               acdoca~BLDAT,
               acdoca~BELNR,
               acdoca~AWITEM,
               acdoca~RACCT,
               acdoca~TSL,
               acdoca~SGTXT,
               acdoca~wsl,
               acdoca~rwcur,
               acdoca~hsl,
               acdoca~hbkid,
               acdoca~hktid,
               acdoca~ktopl,
               bkpf~awkey,
               bkpf~xblnr,
               acdoca~augbl,
               acdoca~AUGGJ,
               acdoca~AUGDT,
               acdoca~kunnr,
               bkpf~kursf,
               acdoca~lifnr,
               acdoca~linetype,
               acdoca~mwskz
          FROM acdoca
              INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                                 bkpf~belnr = acdoca~belnr and
                                 bkpf~gjahr = acdoca~ryear
          INTO CORRESPONDING FIELDS OF TABLE @it_paso2
          for ALL ENTRIES IN @it_paso1
          WHERE acdoca~RLDNR  = @it_paso1-rldnr and
                acdoca~RBUKRS = @it_paso1-RBUKRS and
                acdoca~GJAHR  = @it_paso1-GJAHR and
                acdoca~BELNR  = @it_paso1-BELNR and
                acdoca~RACCT  in @RG_CTAS_EGRESO.
      endif.
    endif.

  endif.


ENDFORM.

FORM GET_DATA_LIQ        USING    IT_PASO1         type ZFI_TRASPASOST "Partida de bancos
                                  IT_PASO1B        type ZFI_TRASPASOST "Partida de Acreedores
                                  IT_PASO2         type ZFI_TRASPASOST "Partida de compensada
                                  SOCIEDAD         type RSIS_T_RANGE
                                  EJERC            type gjahr
                                  PERIODO          type RSIS_T_RANGE
                                  AUGBL            type RSIS_T_RANGE
                                  RG_CTAS_EGRESO   type RSIS_T_RANGE
                                  RG_DOCS_LIQ      type RSIS_T_RANGE
                                  rg_docs_comp_liq type RSIS_T_RANGE.
  data: tabix     TYPE sy-tabix,
        IT_egreso type ZFI_TRASPASOST,
        valori    type i.

  SELECT acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~AWITEM,
         acdoca~RACCT,
         acdoca~TSL,
         acdoca~SGTXT,
         acdoca~wsl,
         acdoca~rwcur,
         acdoca~hsl,
         acdoca~hbkid,
         acdoca~hktid,
         acdoca~ktopl,
         bkpf~awkey,
         bkpf~xblnr,
         acdoca~augbl,
         acdoca~AUGGJ,
         acdoca~AUGDT,
         acdoca~kunnr,
         bkpf~kursf,
         acdoca~lifnr,
         acdoca~linetype,
         acdoca~mwskz
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO CORRESPONDING FIELDS OF TABLE @it_paso1
    WHERE acdoca~RLDNR  EQ @c_0l           and
          acdoca~RBUKRS IN @sociedad and
          acdoca~BLART  in @RG_DOCS_LIQ and
          acdoca~racct  in @RG_CTAS_EGRESO.


  sort it_paso1 by rldnr rbukrs gjahr.

  if sy-subrc = 0.

    SELECT   acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~AWITEM,
             acdoca~RACCT,
             acdoca~TSL,
             acdoca~SGTXT,
             acdoca~wsl,
             acdoca~rwcur,
             acdoca~hsl,
             acdoca~hbkid,
             acdoca~hktid,
             acdoca~ktopl,
             bkpf~awkey,
             bkpf~xblnr,
             acdoca~augbl,
             acdoca~AUGGJ,
             acdoca~AUGDT,
             acdoca~kunnr,
             bkpf~kursf,
             acdoca~lifnr,
             acdoca~linetype,
             acdoca~mwskz
        FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
        INTO CORRESPONDING FIELDS OF TABLE @it_paso2
        FOR ALL ENTRIES IN @it_paso1
        WHERE acdoca~rldnr  EQ @it_paso1-rldnr     AND    "Ledger
              acdoca~rbukrs EQ @it_paso1-rbukrs    AND    "Sociedad
              acdoca~gjahr  EQ @it_paso1-auggj     AND    "Ejercicio
              acdoca~belnr  EQ @it_paso1-augbl and
              acdoca~blart  IN @rg_docs_comp_liq and
              acdoca~GJAHR  EQ @ejerc and
              acdoca~BELNR  in @augbl and
              acdoca~poper  IN @periodo.

    sort it_paso2 by gjahr rbukrs belnr.


    SELECT   acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~AWITEM,
         acdoca~RACCT,
         acdoca~TSL,
         acdoca~SGTXT,
         acdoca~wsl,
         acdoca~rwcur,
         acdoca~hsl,
         acdoca~hbkid,
         acdoca~hktid,
         acdoca~ktopl,
         bkpf~awkey,
         bkpf~xblnr,
         acdoca~augbl,
         acdoca~AUGGJ,
         acdoca~AUGDT,
         acdoca~kunnr,
         bkpf~kursf,
         acdoca~lifnr,
         acdoca~linetype,
         acdoca~mwskz
    FROM acdoca
    INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                       bkpf~belnr = acdoca~belnr and
                       bkpf~gjahr = acdoca~ryear
    INTO CORRESPONDING FIELDS OF TABLE @it_paso1B
    FOR ALL ENTRIES IN @it_paso1
    WHERE acdoca~rldnr  EQ @it_paso1-rldnr     AND    "Ledger
          acdoca~rbukrs EQ @it_paso1-rbukrs    AND    "Sociedad
          acdoca~gjahr  EQ @it_paso1-gjahr     AND    "Ejercicio
          acdoca~belnr  EQ @it_paso1-belnr     AND
          acdoca~lifnr  NE ''.


*          AND
*              acdoca~blart  not in @rg_docs_pagoacre AND
*              acdoca~blart  not in @rg_docs_comproba.    "Ejercicio.7
*
*    sort it_paso3 by rldnr rbukrs gjahr.
*
*    if sy-subrc = 0.
*
*      SELECT   acdoca~RLDNR,
*               acdoca~RBUKRS,
*               acdoca~GJAHR,
*               acdoca~POPER,
*               acdoca~BLART,
*               acdoca~BUDAT,
*               acdoca~BLDAT,
*               acdoca~BELNR,
*               acdoca~AWITEM,
*               acdoca~RACCT,
*               acdoca~TSL,
*               acdoca~SGTXT,
*               acdoca~wsl,
*               acdoca~rwcur,
*               acdoca~hsl,
*               acdoca~hbkid,
*               acdoca~hktid,
*               acdoca~ktopl,
*               bkpf~awkey,
*               bkpf~xblnr,
*               acdoca~augbl,
*               acdoca~AUGGJ,
*               acdoca~AUGDT,
*               acdoca~kunnr,
*               bkpf~kursf,
*               acdoca~lifnr,
*               acdoca~linetype,
*               acdoca~mwskz,
*               acdoca~buzei
*          FROM acdoca
*          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
*                             bkpf~belnr = acdoca~belnr and
*                             bkpf~gjahr = acdoca~ryear
*          INTO CORRESPONDING FIELDS OF TABLE @it_paso3all
*          FOR ALL ENTRIES IN @it_paso3
*          WHERE acdoca~rldnr  EQ @it_paso3-rldnr     AND    "Ledger
*                acdoca~rbukrs EQ @it_paso3-rbukrs    AND    "Sociedad
*                acdoca~gjahr  EQ @it_paso3-gjahr     AND    "Ejercicio
*                acdoca~belnr  EQ @it_paso3-belnr     AND
*                acdoca~racct not in @rg_ctas_exclude.
*
*
*      if sy-subrc = 0.
*
**
**        loop at it_paso3all into DATA(iw_paso3all).
**          tabix = sy-tabix.
**          valori = iw_paso3all-awitem.
**          iw_paso3all-buzei = valori.
**          modify  it_paso3all from iw_paso3all INDEX tabix.
**        ENDLOOP.
*
*        select
*             documento~RLDNR,
*             documento~RBUKRS,
*             documento~GJAHR,
*             documento~POPER,
*             documento~BLART,
*             documento~BUDAT,
*             documento~BLDAT,
*             documento~BELNR,
*             documento~AWITEM,
*             documento~RACCT,
*             documento~TSL,
*             documento~SGTXT,
*             documento~WSL,
*             documento~RWCUR,
*             documento~HSL,
*             documento~HBKID,
*             documento~HKTID,
*             documento~AWKEY,
*             documento~XBLNR,
*             documento~AUGBL,
*             documento~AUGGJ,
*             documento~AUGDT,
*             documento~KUNNR,
*             documento~KURSF,
*             documento~LIFNR,
*             documento~KTOPL,
*             documento~ANLN1,
*             documento~ANLN2,
*             documento~LINETYPE,
*             documento~MWSKZ,
*             documento~KTOSL,
*             posicion~xref3
*     from @it_paso3all as documento
*     inner join bseg as posicion
*                     on  posicion~BUKRS = documento~RBUKRS  and
*                         posicion~BELNR = documento~BELNR   and
*                         posicion~GJAHR = documento~GJAHR   and
*                         posicion~BUZEI = documento~BUZEI
*     into CORRESPONDING FIELDS OF table @it_egreso.
*
*        it_paso3all[] = it_egreso[].
*
*
*      endif.
*
*      sort it_paso3 by rldnr rbukrs gjahr.
*
*      loop at it_paso1 into DATA(iw_paso1).
*        tabix = sy-tabix.
*        READ TABLE it_paso3 into DATA(iw_paso3) with key rldnr  = iw_paso1-rldnr
*                                                         rbukrs = iw_paso1-rbukrs
*                                                         auggj  = iw_paso1-gjahr
*                                                         augbl  = iw_paso1-belnr.
*
*        if sy-subrc ne 0.
*          delete it_paso1 INDEX tabix.
*        ENDIF.
*      ENDLOOP.
*
*      if it_paso1 is not INITIAL.
*
*        SELECT acdoca~RLDNR,
*               acdoca~RBUKRS,
*               acdoca~GJAHR,
*               acdoca~POPER,
*               acdoca~BLART,
*               acdoca~BUDAT,
*               acdoca~BLDAT,
*               acdoca~BELNR,
*               acdoca~AWITEM,
*               acdoca~RACCT,
*               acdoca~TSL,
*               acdoca~SGTXT,
*               acdoca~wsl,
*               acdoca~rwcur,
*               acdoca~hsl,
*               acdoca~hbkid,
*               acdoca~hktid,
*               acdoca~ktopl,
*               bkpf~awkey,
*               bkpf~xblnr,
*               acdoca~augbl,
*               acdoca~AUGGJ,
*               acdoca~AUGDT,
*               acdoca~kunnr,
*               bkpf~kursf,
*               acdoca~lifnr,
*               acdoca~linetype,
*               acdoca~mwskz
*          FROM acdoca
*              INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
*                                 bkpf~belnr = acdoca~belnr and
*                                 bkpf~gjahr = acdoca~ryear
*          INTO CORRESPONDING FIELDS OF TABLE @it_paso2
*          for ALL ENTRIES IN @it_paso1
*          WHERE acdoca~RLDNR  = @it_paso1-rldnr and
*                acdoca~RBUKRS = @it_paso1-RBUKRS and
*                acdoca~GJAHR  = @it_paso1-GJAHR and
*                acdoca~BELNR  = @it_paso1-BELNR and
*                acdoca~RACCT  in @RG_CTAS_EGRESO.
*      endif.
*    endif.
*
  endif.


ENDFORM.
