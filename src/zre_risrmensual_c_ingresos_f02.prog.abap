*&---------------------------------------------------------------------*
*& Form ARMA_TRAPASOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_ACDOCA_TRASPASOS_OR
*&      --> IT_ACDOCA_TRASPASOS_DEST
*&      --> IT_REPORTE
*&---------------------------------------------------------------------*
FORM ARMA_TRAPASOS  USING    P_IT_ACDOCA_TRASPASOS_OR type ttraspaso
                             P_IT_ACDOCA_TRASPASOS_DEST type ttraspaso
                             P_IT_REPORTE type tty_report.
  data: w_line_rep  type ty_report,
        RACCT_MISMO type acdoca-racct.

  LOOP AT P_IT_ACDOCA_TRASPASOS_OR ASSIGNING FIELD-SYMBOL(<fs_origen>).
    CONCATENATE <fs_origen>-RACCT(9) '2' into RACCT_MISMO.

    read table P_IT_ACDOCA_TRASPASOS_DEST TRANSPORTING NO FIELDS
    with key GJAHR  = <fs_origen>-GJAHR
             belnr  = <fs_origen>-BELNR
             rbukrs = <fs_origen>-rbukrs
             RACCT = RACCT_MISMO.

    if sy-subrc ne 0.

      lcl_isr_tools=>map_reference( EXPORTING gjahr  = <fs_origen>-GJAHR
                                              belnr  = <fs_origen>-BELNR
                                              rbukrs = <fs_origen>-rbukrs
                                    IMPORTING xblnr  = wa_reporte-ZREF_COB ).
      clear w_line_rep.
      w_line_rep-zfec_cobro = <fs_origen>-budat.

      if <fs_origen>-xblnr is initial.
        w_line_rep-zref_cob = <fs_origen>-awkey.
      else.
        w_line_rep-zref_cob = <fs_origen>-xblnr.
      endif.
      w_line_rep-zsociedad = <fs_origen>-rbukrs.
      w_line_rep-zejercicio = <fs_origen>-GJAHR.
      w_line_rep-zperiodo = <fs_origen>-POPER.
      w_line_rep-zdescrip = <fs_origen>-sgtxt.
      w_line_rep-zdocto_cobro = <fs_origen>-BELNR.
      w_line_rep-zclase_docto = <fs_origen>-blart.

*     if <fs_origen>-rwcur NE 'MXN'.
*       w_line_rep-zimporte_fact_me = <fs_origen>-wsl. "Imp. Fact en ME
*     endif.
*     w_line_rep-zimporte_fac_mn =  <fs_origen>-hsl.
      w_line_rep-zdes_ingreso = text-040.
      lcl_isr_tools=>map_bank_name( EXPORTING bukrs  = <fs_origen>-rbukrs
                                              hbkid  = <fs_origen>-hbkid
                                              hktid  = <fs_origen>-hktid
                                    IMPORTING TEXT1  = w_line_rep-ZBANCO_COBRO
                                    CHANGING  it_t012t = it_t012t ).
      w_line_rep-ZOTR_DEPOS  = <fs_origen>-tsl.

      if <fs_origen>-tsl < 0.
        w_line_rep-ztotal  = <fs_origen>-tsl * ( -1 ).
      else.
        w_line_rep-ztotal  = <fs_origen>-tsl.
      endif.
      w_line_rep-ZCOBRO_MN = w_line_rep-ztotal.
      append w_line_rep to  P_IT_REPORTE.

    endif.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ARMA_ING_REC_SEG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_ACDOCA_TRASPASOS_OR
*&      --> IT_ACDOCA_TRASPASOS_DEST
*&      --> IT_REPORTE
*&---------------------------------------------------------------------*
FORM ARMA_ING_REC_SEG  USING    P_IT_ACDOCA_TRASPASOS_OR type ttraspaso
                                P_IT_ACDOCA_TRASPASOS_DEST type ttraspaso
                                P_it_ing_iva type tty_iva
                                P_IT_REPORTE type tty_report.
  data: w_line_rep type ty_report.
  LOOP AT P_IT_ACDOCA_TRASPASOS_OR ASSIGNING FIELD-SYMBOL(<fs_origen>).

*Determinar la referencia
    lcl_isr_tools=>map_reference( EXPORTING gjahr  = <fs_origen>-GJAHR
                                            belnr  = <fs_origen>-BELNR
                                            rbukrs = <fs_origen>-rbukrs
                                  IMPORTING xblnr  = wa_reporte-ZREF_COB ).
    clear w_line_rep.
    w_line_rep-zfec_cobro = <fs_origen>-budat.

    if <fs_origen>-xblnr is initial.
      w_line_rep-zref_cob = <fs_origen>-awkey.
    else.
      w_line_rep-zref_cob = <fs_origen>-xblnr.
    endif.
    w_line_rep-zsociedad = <fs_origen>-rbukrs.
    w_line_rep-zejercicio = <fs_origen>-GJAHR.
    w_line_rep-zperiodo = <fs_origen>-POPER.
    w_line_rep-zdescrip = <fs_origen>-sgtxt.
    w_line_rep-zdocto_cobro = <fs_origen>-BELNR.
    w_line_rep-zclase_docto = <fs_origen>-blart.
*Determinar montos
*    if <fs_origen>-rwcur NE 'MXN'.
*      w_line_rep-zimporte_fact_me = <fs_origen>-wsl. "Imp. Fact en ME
*    endif.
*    w_line_rep-zimporte_fac_mn =  <fs_origen>-hsl.
*Colocar nuevos conceptos
    w_line_rep-zdes_ingreso = text-041.

*Leer la contrapartida de bancos para determinar los datos de banco propio
    read table P_IT_ACDOCA_TRASPASOS_DEST ASSIGNING FIELD-SYMBOL(<fs_destino>)
    WITH key  RLDNR   = <fs_origen>-RLDNR
              RBUKRS  = <fs_origen>-RBUKRS
              GJAHR   = <fs_origen>-GJAHR
              BELNR   = <fs_origen>-BELNR.

    if sy-subrc = 0.
*Mapear los datos de banco propio
      lcl_isr_tools=>map_bank_name( EXPORTING bukrs  = <fs_destino>-rbukrs
                                              hbkid  = <fs_destino>-hbkid
                                              hktid  = <fs_destino>-hktid
                                    IMPORTING TEXT1  = w_line_rep-ZBANCO_COBRO
                                    CHANGING  it_t012t = it_t012t ).
    endif.

    if <fs_origen>-tsl < 0.
      w_line_rep-ZOTR_INGR  = <fs_origen>-tsl * ( -1 ).
    else.
      w_line_rep-ZOTR_INGR  = <fs_origen>-tsl.
    endif.
    w_line_rep-ZCOBRO_MN = w_line_rep-ZOTR_INGR.

    clear w_line_rep-ziva_al_16.

*Obtener iva de las partidas de iva
    loop at P_it_ing_iva ASSIGNING FIELD-SYMBOL(<fs_iva>) where
                                                                BUKRS  = <fs_origen>-RBUKRS and
                                                                GJAHR = <fs_origen>-GJAHR and
                                                                BELNR = <fs_origen>-BELNR.
      if <fs_iva>-wrbtr < 0.
        w_line_rep-ziva_al_16 = w_line_rep-ziva_al_16 + <fs_iva>-wrbtr * ( -1 ).
      endif.
    endloop.

    w_line_rep-ztotal  = w_line_rep-ZOTR_INGR + w_line_rep-ziva_al_16.

    append w_line_rep to  P_IT_REPORTE.

  ENDLOOP.

ENDFORM.


FORM ARMA_INTERESES  USING    P_IT_ACDOCA_INTERESES_OR    type ttraspaso
                              P_IT_ACDOCA_INTERESES_DEST  type ttraspaso
                              P_IT_ACDOCA_COMPENSA        type ttraspaso
                              P_IT_ACDOCA_INTERESES_OR2   type ttraspaso
                              P_IT_ACDOCA_INTERESES_DEST2 type ttraspaso
                              P_IT_ACDOCA_COMPENSA2       type ttraspaso
                              P_IT_REPORTE type tty_report.
  data: w_line_rep type ty_report.

*  Reportaje de cuentas de intereses
  LOOP AT P_IT_ACDOCA_INTERESES_DEST ASSIGNING FIELD-SYMBOL(<fs_destino>).

    lcl_isr_tools=>map_reference( EXPORTING gjahr  = <fs_destino>-GJAHR
                                            belnr  = <fs_destino>-BELNR
                                            rbukrs = <fs_destino>-rbukrs
                                  IMPORTING xblnr  = wa_reporte-ZREF_COB ).
    clear w_line_rep.
    w_line_rep-zfec_cobro = <fs_destino>-budat.

    if <fs_destino>-xblnr is initial.
      w_line_rep-zref_cob = <fs_destino>-awkey.
    else.
      w_line_rep-zref_cob = <fs_destino>-xblnr.
    endif.
    w_line_rep-zind_imp = 'C0'.
    w_line_rep-zsociedad = <fs_destino>-rbukrs.
    w_line_rep-zejercicio = <fs_destino>-GJAHR.
    w_line_rep-zperiodo = <fs_destino>-POPER.
    w_line_rep-zdescrip = <fs_destino>-sgtxt.
    w_line_rep-zdocto_cobro = <fs_destino>-BELNR.
    w_line_rep-zclase_docto = <fs_destino>-blart.
*    if <fs_destino>-rwcur NE 'MXN'.
*      w_line_rep-zimporte_fact_me = <fs_destino>-wsl. "Imp. Fact en ME
*    endif.
*    w_line_rep-zimporte_fac_mn =  <fs_destino>-hsl.
    w_line_rep-zdes_ingreso = text-042.
    READ TABLE P_IT_ACDOCA_INTERESES_OR ASSIGNING FIELD-SYMBOL(<fs_origen>)
    WITH key gjahr  = <fs_destino>-GJAHR
             belnr  = <fs_destino>-BELNR
             rbukrs = <fs_destino>-rbukrs.
    if sy-subrc = 0.

      READ TABLE P_IT_ACDOCA_COMPENSA ASSIGNING FIELD-SYMBOL(<fs_compensado>)
      WITH key gjahr  = <fs_origen>-AUGGJ
               belnr  = <fs_origen>-AUGBL
               rbukrs = <fs_origen>-rbukrs.

      if sy-subrc = 0.
        lcl_isr_tools=>map_bank_name( EXPORTING bukrs  = <fs_compensado>-rbukrs
                                                hbkid  = <fs_compensado>-hbkid
                                                hktid  = <fs_compensado>-hktid
                                    IMPORTING TEXT1  = w_line_rep-ZBANCO_COBRO
                                    CHANGING  it_t012t = it_t012t ).
      endif.
    endif.
    if <fs_destino>-tsl < 0.
      w_line_rep-ZOTR_INGR  = <fs_destino>-tsl * ( -1 ).
    else.
      w_line_rep-ZOTR_INGR  = <fs_destino>-tsl.
    endif.
    w_line_rep-ZCOBRO_MN = w_line_rep-ZOTR_INGR.
    w_line_rep-ztotal  = w_line_rep-ZOTR_INGR.
    append w_line_rep to  P_IT_REPORTE.

  ENDLOOP.


*  Reportaje de cuentas de intereses
  LOOP AT P_IT_ACDOCA_INTERESES_DEST2 ASSIGNING FIELD-SYMBOL(<fs_destino2>).

    lcl_isr_tools=>map_reference( EXPORTING gjahr  = <fs_destino2>-GJAHR
                                            belnr  = <fs_destino2>-BELNR
                                            rbukrs = <fs_destino2>-rbukrs
                                  IMPORTING xblnr  = wa_reporte-ZREF_COB ).
    clear w_line_rep.
    w_line_rep-zfec_cobro = <fs_destino2>-budat.

    if <fs_destino2>-xblnr is initial.
      w_line_rep-zref_cob = <fs_destino2>-awkey.
    else.
      w_line_rep-zref_cob = <fs_destino2>-xblnr.
    endif.
    w_line_rep-zind_imp = 'C0'.
    w_line_rep-zsociedad = <fs_destino2>-rbukrs.
    w_line_rep-zejercicio = <fs_destino2>-GJAHR.
    w_line_rep-zperiodo = <fs_destino2>-POPER.
    w_line_rep-zdescrip = <fs_destino2>-sgtxt.
    w_line_rep-zdocto_cobro = <fs_destino2>-BELNR.
    w_line_rep-zclase_docto = <fs_destino2>-blart.
    w_line_rep-zdes_ingreso = text-050.
    lcl_isr_tools=>map_bank_name( EXPORTING bukrs  = <fs_destino2>-rbukrs
                                            hbkid  = <fs_destino2>-hbkid
                                            hktid  = <fs_destino2>-hktid
                                IMPORTING TEXT1  = w_line_rep-ZBANCO_COBRO
                                CHANGING  it_t012t = it_t012t ).
    w_line_rep-ZOTR_INGR  = <fs_destino2>-tsl.
    w_line_rep-ztotal  = w_line_rep-ZOTR_INGR.
    append w_line_rep to  P_IT_REPORTE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ARMA_INVERSIONES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_INV_ORIGEN
*&      --> IT_INV_DESTINO
*&      --> IT_REPORTE
*&---------------------------------------------------------------------*
FORM ARMA_INVERSIONES  USING    P_IT_INV_ORIGEN  type ttraspaso
                                P_IT_INV_DESTINO type ttraspaso
                                P_IT_REPORTE type tty_report.
  data: w_line_rep type ty_report.
  LOOP AT P_IT_INV_ORIGEN ASSIGNING FIELD-SYMBOL(<fs_origen>).
    delete P_IT_REPORTE WHERE zsociedad  = <fs_origen>-rbukrs and
                              zejercicio = <fs_origen>-GJAHR  and
                              zdocto_cobro = <fs_origen>-BELNR.
  endloop.

  LOOP AT P_IT_INV_ORIGEN ASSIGNING <fs_origen>.

    lcl_isr_tools=>map_reference( EXPORTING gjahr  = <fs_origen>-GJAHR
                                            belnr  = <fs_origen>-BELNR
                                            rbukrs = <fs_origen>-rbukrs
                                  IMPORTING xblnr  = wa_reporte-ZREF_COB ).
    clear w_line_rep.
    w_line_rep-zfec_cobro = <fs_origen>-budat.

    if <fs_origen>-xblnr is initial.
      w_line_rep-zref_cob = <fs_origen>-awkey.
    else.
      w_line_rep-zref_cob = <fs_origen>-xblnr.
    endif.
    w_line_rep-zind_imp = 'C0'.
    w_line_rep-zsociedad = <fs_origen>-rbukrs.
    w_line_rep-zejercicio = <fs_origen>-GJAHR.
    w_line_rep-zperiodo = <fs_origen>-POPER.
    w_line_rep-zdescrip = <fs_origen>-sgtxt.
    w_line_rep-zdocto_cobro = <fs_origen>-BELNR.
    w_line_rep-zclase_docto = <fs_origen>-blart.
    w_line_rep-zdes_ingreso = text-043.
    lcl_isr_tools=>map_bank_name( EXPORTING bukrs  = <fs_origen>-rbukrs
                                          hbkid  = <fs_origen>-hbkid
                                          hktid  = <fs_origen>-hktid
                                IMPORTING TEXT1  = w_line_rep-ZBANCO_COBRO
                                CHANGING  it_t012t = it_t012t ).
    if <fs_origen>-tsl < 0.
      w_line_rep-ZOTR_DEPOS  = <fs_origen>-tsl * ( -1 ).
    else.
      w_line_rep-ZOTR_DEPOS  = <fs_origen>-tsl.
    endif.
    w_line_rep-ZCOBRO_MN = w_line_rep-ZOTR_DEPOS.
    w_line_rep-ztotal  = w_line_rep-ZOTR_DEPOS.
    append w_line_rep to  P_IT_REPORTE.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_TRASPASOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_TRASPASOS .
  SELECT acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~DOCLN,
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
           bkpf~kursf
      FROM acdoca
      INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                         bkpf~belnr = acdoca~belnr and
                         bkpf~gjahr = acdoca~ryear
      INTO TABLE @it_tr_destino
      WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
            acdoca~rbukrs IN @sociedad           AND    "Sociedad
            acdoca~ryear  EQ @ejerc              AND    "Ejercicio
            acdoca~poper  IN @periodo            AND    "Periodo
            acdoca~racct  IN @rg_cuentas_destino AND
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
           acdoca~DOCLN,
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
           bkpf~kursf
      FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
      INTO TABLE @it_tr_origen
      for ALL ENTRIES IN @it_tr_destino
      WHERE acdoca~RLDNR  = @it_tr_destino-rldnr and
            acdoca~RBUKRS = @it_tr_destino-RBUKRS and
            acdoca~GJAHR  = @it_tr_destino-GJAHR and
            acdoca~BELNR  = @it_tr_destino-BELNR and
            acdoca~racct  IN @rg_cuentas_origen and
            acdoca~BLART  IN @rg_docs_traspaso AND
            acdoca~AWREF_REV = ''.
  endif.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_RECUPERACION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_RECUPERACION .
  SELECT acdoca~RLDNR,
          acdoca~RBUKRS,
          acdoca~GJAHR,
          acdoca~POPER,
          acdoca~BLART,
          acdoca~BUDAT,
          acdoca~BLDAT,
          acdoca~BELNR,
          acdoca~DOCLN,
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
          bkpf~kursf
     FROM acdoca
     INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                        bkpf~belnr = acdoca~belnr and
                        bkpf~gjahr = acdoca~ryear
     INTO TABLE @it_ing_tr_destino
     WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
           acdoca~rbukrs IN @sociedad           AND    "Sociedad
           acdoca~ryear  EQ @ejerc              AND    "Ejercicio
           acdoca~poper  IN @periodo            AND    "Periodo
           acdoca~racct  IN @rg_docs_cta_ingrec AND
           acdoca~blart  in @rg_docs_ing_recup AND
           acdoca~AWREF_REV = ''.

  if sy-subrc = 0.

    SELECT  acdoca~RLDNR,
             acdoca~RBUKRS,
             acdoca~GJAHR,
             acdoca~POPER,
             acdoca~BLART,
             acdoca~BUDAT,
             acdoca~BLDAT,
             acdoca~BELNR,
             acdoca~DOCLN,
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
             bkpf~kursf
        FROM acdoca
            INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                               bkpf~belnr = acdoca~belnr and
                               bkpf~gjahr = acdoca~ryear
        INTO TABLE @it_ing_tr_origen
        for ALL ENTRIES IN @it_ing_tr_destino
        WHERE acdoca~RLDNR  = @it_ing_tr_destino-rldnr and
              acdoca~RBUKRS = @it_ing_tr_destino-RBUKRS and
              acdoca~GJAHR  = @it_ing_tr_destino-GJAHR and
              acdoca~BELNR  = @it_ing_tr_destino-BELNR and
              acdoca~racct  IN @rg_cuentas_origen AND
              acdoca~AWREF_REV = ''.

    if sy-subrc = 0.

      SELECT     bseg~bukrs,
                 bseg~belnr,
                 bseg~gjahr,
                 bseg~sgtxt,
                 bseg~hkont,
                 bseg~bschl,
                 bseg~koart,
                 bseg~MWSKZ,
                 bseg~QSSKZ,
                 bseg~DMBTR,
                 bseg~WRBTR,
                 bseg~KZBTR,
                 bseg~PSWBT
          FROM bseg
          INTO TABLE @it_ing_iva
          for ALL ENTRIES IN @it_ing_tr_origen
          WHERE
                bseg~BUKRS = @it_ing_tr_origen-RBUKRS and
                bseg~GJAHR  = @it_ing_tr_origen-GJAHR and
                bseg~BELNR  = @it_ing_tr_origen-BELNR and
                bseg~hkont  IN @rg_ctas_iva_ing.

    ENDIF.

  endif.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_INTERESES_GANADOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_INTERESES_GANADOS .
*      rg_docs_intereses     TYPE RANGE OF acdoca-blart,
*      rg_cta_pte            TYPE RANGE OF acdoca-racct,
*      rg_cta_intereses      TYPE RANGE OF acdoca-racct,
*      it_int_origen         TYPE ttraspaso,
*      it_int_destino        TYPE ttraspaso,

  SELECT acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~DOCLN,
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
         bkpf~kursf
    FROM acdoca
    INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                       bkpf~belnr = acdoca~belnr and
                       bkpf~gjahr = acdoca~ryear
    INTO TABLE @it_int_origen
    WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
          acdoca~rbukrs IN @sociedad           AND    "Sociedad
          acdoca~ryear  EQ @ejerc              AND    "Ejercicio
          acdoca~poper  IN @periodo            AND    "Periodo
          acdoca~racct  IN @rg_cta_pte         AND    "Cuenta puente
          acdoca~blart  in @rg_docs_intereses AND
          acdoca~AWREF_REV = '' and
          acdoca~DRCRK = 'S'.

  if sy-subrc = 0.

    SELECT
     acdoca~RLDNR,
     acdoca~RBUKRS,
     acdoca~GJAHR,
     acdoca~POPER,
     acdoca~BLART,
     acdoca~BUDAT,
     acdoca~BLDAT,
     acdoca~BELNR,
     acdoca~DOCLN,
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
     bkpf~kursf
FROM acdoca
    INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                       bkpf~belnr = acdoca~belnr and
                       bkpf~gjahr = acdoca~ryear
INTO TABLE @it_int_compensa
for ALL ENTRIES IN @it_int_origen
WHERE acdoca~RLDNR  = @it_int_origen-rldnr and
      acdoca~RBUKRS = @it_int_origen-RBUKRS and
      acdoca~GJAHR  = @it_int_origen-AUGGJ and
      acdoca~BELNR  = @it_int_origen-AUGBL and
      acdoca~racct  IN @rg_cuentas_origen AND
      acdoca~AWREF_REV = ''.

    SELECT
         acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~DOCLN,
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
         bkpf~kursf
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO TABLE @it_int_destino
    for ALL ENTRIES IN @it_int_origen
    WHERE acdoca~RLDNR  = @it_int_origen-rldnr and
          acdoca~RBUKRS = @it_int_origen-RBUKRS and
          acdoca~GJAHR  = @it_int_origen-GJAHR and
          acdoca~BELNR  = @it_int_origen-BELNR and
          acdoca~racct  IN @rg_cta_intereses AND
          acdoca~AWREF_REV = '' and
          acdoca~DRCRK = 'H'.

  endif.


*Escenario ISR retenido
  SELECT acdoca~RLDNR,
       acdoca~RBUKRS,
       acdoca~GJAHR,
       acdoca~POPER,
       acdoca~BLART,
       acdoca~BUDAT,
       acdoca~BLDAT,
       acdoca~BELNR,
       acdoca~DOCLN,
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
       bkpf~kursf
  FROM acdoca
  INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                     bkpf~belnr = acdoca~belnr and
                     bkpf~gjahr = acdoca~ryear
  INTO TABLE @it_int_origen2
  WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
        acdoca~rbukrs IN @sociedad           AND    "Sociedad
        acdoca~ryear  EQ @ejerc              AND    "Ejercicio
        acdoca~poper  IN @periodo            AND    "Periodo
        acdoca~racct  IN @rg_cta_interesesisr AND    "Cuenta destino
        acdoca~blart  in @rg_docs_intereses AND
        acdoca~AWREF_REV = ''.

  if sy-subrc = 0.

*    SELECT
*         acdoca~RLDNR,
*         acdoca~RBUKRS,
*         acdoca~GJAHR,
*         acdoca~POPER,
*         acdoca~BLART,
*         acdoca~BUDAT,
*         acdoca~BLDAT,
*         acdoca~BELNR,
*         acdoca~DOCLN,
*         acdoca~RACCT,
*         acdoca~TSL,
*         acdoca~SGTXT,
*         acdoca~wsl,
*         acdoca~rwcur,
*         acdoca~hsl,
*         acdoca~hbkid,
*         acdoca~hktid,
*         bkpf~awkey,
*         bkpf~xblnr,
*         acdoca~augbl,
*         acdoca~AUGGJ,
*         acdoca~AUGDT,
*         acdoca~kunnr,
*         bkpf~kursf
*FROM acdoca
*INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
*                   bkpf~belnr = acdoca~belnr and
*                   bkpf~gjahr = acdoca~ryear
*INTO TABLE @it_int_compensa2
*for ALL ENTRIES IN @it_int_origen2
*WHERE acdoca~RLDNR  = @it_int_origen2-rldnr and
*  acdoca~RBUKRS = @it_int_origen2-RBUKRS and
*  acdoca~GJAHR  = @it_int_origen2-AUGGJ and
*  acdoca~BELNR  = @it_int_origen2-AUGBL and
*  acdoca~racct  IN @rg_cuentas_destino AND
*  acdoca~AWREF_REV = ''.

    SELECT
         acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~DOCLN,
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
         bkpf~kursf
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO TABLE @it_int_destino2
    for ALL ENTRIES IN @it_int_origen2
    WHERE acdoca~RLDNR  = @it_int_origen2-rldnr and
          acdoca~RBUKRS = @it_int_origen2-RBUKRS and
          acdoca~GJAHR  = @it_int_origen2-GJAHR and
          acdoca~BELNR  = @it_int_origen2-BELNR and
          acdoca~racct  IN @rg_cuentas_destino AND
          acdoca~AWREF_REV = ''.

    if sy-subrc = 0.

      SELECT spras,
             bukrs,
             hbkid,
             hktid,
             text1
        FROM t012t
        APPENDING CORRESPONDING FIELDS OF TABLE @it_t012t
         FOR ALL ENTRIES IN @it_int_destino2
            WHERE spras  EQ @c_es AND
                  bukrs  EQ @it_int_destino2-rbukrs AND
                  hbkid  EQ @it_int_destino2-hbkid AND
                  hktid  EQ @it_int_destino2-hktid.
    endif.

  endif.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INVERSIONES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INVERSIONES .
  SELECT acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~DOCLN,
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
           bkpf~kursf
      FROM acdoca
      INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                         bkpf~belnr = acdoca~belnr and
                         bkpf~gjahr = acdoca~ryear
      INTO TABLE @it_inv_origen
      WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
            acdoca~rbukrs IN @sociedad           AND    "Sociedad
            acdoca~ryear  EQ @ejerc              AND    "Ejercicio
            acdoca~poper  IN @periodo            AND    "Periodo
            acdoca~racct  IN @rg_doc_inver_conc   AND    "Cuenta puente
            acdoca~blart  in @rg_doc_inver AND
            acdoca~AWREF_REV = '' AND
            acdoca~DRCRK = 'H'.

  if sy-subrc = 0.

    SELECT
         acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~DOCLN,
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
         bkpf~kursf
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO TABLE @it_inv_destino
    FOR ALL ENTRIES IN @it_inv_origen
    WHERE
          acdoca~RLDNR  = @it_inv_origen-rldnr and
          acdoca~RBUKRS = @it_inv_origen-RBUKRS and
          acdoca~AUGGJ  = @it_inv_origen-GJAHR  and
          acdoca~BELNR  = @it_inv_origen-BELNR  and
          acdoca~blart  in @rg_doc_inver and "Docs inversion
          acdoca~racct  IN @rg_cuentas_origen and
          acdoca~AWREF_REV = '' and
          acdoca~DRCRK = 'S'.
  endif.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_PRESTAMOS_INTERCO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_PRESTAMOS_INTERCO .
  SELECT acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~DOCLN,
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
           bkpf~kursf
      FROM acdoca
      INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                         bkpf~belnr = acdoca~belnr and
                         bkpf~gjahr = acdoca~ryear
      INTO TABLE @it_interco_origen
      WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
            acdoca~rbukrs IN @sociedad           AND    "Sociedad
            acdoca~ryear  EQ @ejerc              AND    "Ejercicio
            acdoca~poper  IN @periodo            AND    "Periodo
            acdoca~racct  IN @rg_cuentas_origen  AND    "Cuentas de abonos en bancos
            acdoca~blart  in @rg_doc_intercos    AND
            acdoca~AWREF_REV = ''.

  if sy-subrc = 0.

    SELECT
         acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~DOCLN,
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
         bkpf~kursf
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO TABLE @it_interco_destino
    FOR ALL ENTRIES IN @it_interco_origen
    WHERE
          acdoca~RLDNR  = @it_interco_origen-rldnr and
          acdoca~RBUKRS = @it_interco_origen-RBUKRS and
          acdoca~RYEAR  = @it_interco_origen-GJAHR  and
          acdoca~BELNR  = @it_interco_origen-BELNR  and
          acdoca~blart  in @rg_doc_intercos    and "Docs intercos
          acdoca~racct  IN @rg_ctas_intercos and
          acdoca~AWREF_REV = ''.
  endif.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ARMA_INTERCOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_INTERCO_ORIGEN
*&      --> IT_INTERCO_DESTINO
*&      --> IT_REPORTE
*&---------------------------------------------------------------------*

FORM ARMA_INTERCOS  USING    P_IT_INTERCO_ORIGEN type ttraspaso
                             P_IT_INTERCO_DESTINO type ttraspaso
                             P_IT_REPORTE type tty_report.
  data: w_line_rep type ty_report.
  LOOP AT P_IT_INTERCO_DESTINO ASSIGNING FIELD-SYMBOL(<fs_destino>).

    lcl_isr_tools=>map_reference( EXPORTING gjahr  = <fs_destino>-GJAHR
                                            belnr  = <fs_destino>-BELNR
                                            rbukrs = <fs_destino>-rbukrs
                                  IMPORTING xblnr  = wa_reporte-ZREF_COB ).
    clear w_line_rep.
    w_line_rep-zfec_cobro = <fs_destino>-budat.

    if <fs_destino>-xblnr is initial.
      w_line_rep-zref_cob = <fs_destino>-awkey.
    else.
      w_line_rep-zref_cob = <fs_destino>-xblnr.
    endif.
*    w_line_rep-zind_imp = 'C0'.
    w_line_rep-zsociedad = <fs_destino>-rbukrs.
    w_line_rep-zejercicio = <fs_destino>-GJAHR.
    w_line_rep-zperiodo = <fs_destino>-POPER.
    w_line_rep-zdescrip = <fs_destino>-sgtxt.
    w_line_rep-zdocto_cobro = <fs_destino>-BELNR.
    w_line_rep-zclase_docto = <fs_destino>-blart.
*    if <fs_destino>-rwcur NE 'MXN'.
*      w_line_rep-zimporte_fact_me = <fs_destino>-wsl. "Imp. Fact en ME
*    endif.
*    w_line_rep-zimporte_fac_mn =  <fs_destino>-hsl.
    w_line_rep-zdes_ingreso = text-044.

    READ TABLE P_IT_INTERCO_ORIGEN
    into DATA(IW_INTERCO_ORIGEN) WITH key  gjahr  = <fs_destino>-GJAHR
                                           belnr  = <fs_destino>-BELNR
                                           rbukrs = <fs_destino>-rbukrs.

    if sy-subrc = 0.
      lcl_isr_tools=>map_bank_name( EXPORTING bukrs  = IW_INTERCO_ORIGEN-rbukrs
                                              hbkid  = IW_INTERCO_ORIGEN-hbkid
                                              hktid  = IW_INTERCO_ORIGEN-hktid
                                  IMPORTING TEXT1  = w_line_rep-ZBANCO_COBRO
                                  CHANGING  it_t012t = it_t012t ).
    endif.

    if <fs_destino>-tsl < 0.
      w_line_rep-ZOTR_DEPOS  = <fs_destino>-tsl * ( -1 ).
    else.
      w_line_rep-ZOTR_DEPOS  = <fs_destino>-tsl.
    endif.
*    w_line_rep-ZOTR_DEPOS = w_line_rep-ZCOBRO_MN.
    w_line_rep-ztotal  = w_line_rep-ZOTR_DEPOS.
    w_line_rep-ZCOBRO_MN = w_line_rep-ztotal.
    append w_line_rep to  P_IT_REPORTE.
  endloop.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_PRESTAMOS_OT_ING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_PRESTAMOS_OT_ING .
  SELECT   acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~DOCLN,
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
           bkpf~kursf
      FROM acdoca
      INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                         bkpf~belnr = acdoca~belnr and
                         bkpf~gjahr = acdoca~ryear
      INTO TABLE @it_otring_origen
      WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
            acdoca~rbukrs IN @sociedad           AND    "Sociedad
            acdoca~ryear  EQ @ejerc              AND    "Ejercicio
            acdoca~poper  IN @periodo            AND    "Periodo
            acdoca~racct  IN @rg_cuentas_origen  AND    "Cuentas de abonos en bancos
            acdoca~AWREF_REV = ''.

  if sy-subrc = 0.

    SELECT
         acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~DOCLN,
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
         bkpf~kursf
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO TABLE @it_otring_destino
    FOR ALL ENTRIES IN @it_otring_origen
    WHERE
          acdoca~RLDNR  = @it_otring_origen-rldnr and
          acdoca~RBUKRS = @it_otring_origen-RBUKRS and
          acdoca~RYEAR  = @it_otring_origen-GJAHR  and
          acdoca~BELNR  = @it_otring_origen-BELNR  and
          acdoca~racct  IN @rg_ctas_otring and
          acdoca~AWREF_REV = ''.
  endif.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ARMA_OT_INGRE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_OTRING_ORIGEN
*&      --> IT_OTRING_DESTINO
*&      --> IT_REPORTE
*&---------------------------------------------------------------------*
FORM ARMA_OT_INGRE  USING    P_IT_OTRING_ORIGEN type ttraspaso
                             P_IT_OTRING_DESTINO type ttraspaso
                             P_IT_REPORTE type tty_report.

  data: w_line_rep type ty_report.

  LOOP AT P_IT_OTRING_DESTINO ASSIGNING FIELD-SYMBOL(<fs_destino>).

    lcl_isr_tools=>map_reference( EXPORTING gjahr  = <fs_destino>-GJAHR
                                            belnr  = <fs_destino>-BELNR
                                            rbukrs = <fs_destino>-rbukrs
                                  IMPORTING xblnr  = wa_reporte-ZREF_COB ).
    clear w_line_rep.
    w_line_rep-zfec_cobro = <fs_destino>-budat.

    if <fs_destino>-xblnr is initial.
      w_line_rep-zref_cob = <fs_destino>-awkey.
    else.
      w_line_rep-zref_cob = <fs_destino>-xblnr.
    endif.
*    w_line_rep-zind_imp = 'C0'.
    w_line_rep-zsociedad = <fs_destino>-rbukrs.
    w_line_rep-zejercicio = <fs_destino>-GJAHR.
    w_line_rep-zperiodo = <fs_destino>-POPER.
    w_line_rep-zdescrip = <fs_destino>-sgtxt.
    w_line_rep-zdocto_cobro = <fs_destino>-BELNR.
    w_line_rep-zclase_docto = <fs_destino>-blart.
*    if <fs_destino>-rwcur NE 'MXN'.
*      w_line_rep-zimporte_fact_me = <fs_destino>-wsl. "Imp. Fact en ME
*    endif.
*    w_line_rep-zimporte_fac_mn =  <fs_destino>-hsl.
    w_line_rep-zdes_ingreso = text-045.
    lcl_isr_tools=>map_bank_name( EXPORTING bukrs  = <fs_destino>-rbukrs
                                          hbkid  = <fs_destino>-hbkid
                                          hktid  = <fs_destino>-hktid
                                IMPORTING TEXT1  = w_line_rep-ZBANCO_COBRO
                                CHANGING  it_t012t = it_t012t ).
    if <fs_destino>-tsl < 0.
      w_line_rep-ZOTR_INGR  = <fs_destino>-tsl * ( -1 ).
    else.
      w_line_rep-ZOTR_INGR  = <fs_destino>-tsl.
    endif.
    w_line_rep-ZCOBRO_MN = w_line_rep-ZOTR_INGR.
    w_line_rep-ztotal  = w_line_rep-ZOTR_INGR.
    append w_line_rep to  P_IT_REPORTE.
  endloop.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_PRESTAMOS_OT_DEP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_PRESTAMOS_OT_DEP .
  SELECT   acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~DOCLN,
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
           bkpf~kursf
      FROM acdoca
      INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                         bkpf~belnr = acdoca~belnr and
                         bkpf~gjahr = acdoca~ryear
      INTO TABLE @it_otrdep_origen
      WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
            acdoca~rbukrs IN @sociedad           AND    "Sociedad
            acdoca~ryear  EQ @ejerc              AND    "Ejercicio
            acdoca~poper  IN @periodo            AND    "Periodo
            acdoca~racct  IN @rg_cuentas_origen  AND    "Cuentas de abonos en bancos
            acdoca~AWREF_REV = '' AND
            acdoca~DRCRK = 'S'.

  if sy-subrc = 0.

    SELECT
         acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~DOCLN,
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
         bkpf~kursf
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO TABLE @it_otrdep_destino
    FOR ALL ENTRIES IN @it_otrdep_origen
    WHERE
          acdoca~RLDNR  = @it_otrdep_origen-rldnr and
          acdoca~RBUKRS = @it_otrdep_origen-RBUKRS and
          acdoca~RYEAR  = @it_otrdep_origen-GJAHR  and
          acdoca~BELNR  = @it_otrdep_origen-BELNR  and
          acdoca~racct  IN @rg_ctas_otdep and
          acdoca~AWREF_REV = '' AND
          acdoca~DRCRK = 'H'.
  endif.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ARMA_OT_INDEP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_OTRDEP_ORIGEN
*&      --> IT_OTRDEP_DESTINO
*&      --> IT_REPORTE
*&---------------------------------------------------------------------*
FORM ARMA_OT_INDEP  USING    P_IT_OTRDEP_ORIGEN type ttraspaso
                             P_IT_OTRDEP_DESTINO type ttraspaso
                             P_IT_REPORTE type tty_report.

  data: w_line_rep type ty_report.

  LOOP AT P_IT_OTRDEP_DESTINO ASSIGNING FIELD-SYMBOL(<fs_destino>).

    lcl_isr_tools=>map_reference( EXPORTING gjahr  = <fs_destino>-GJAHR
                                            belnr  = <fs_destino>-BELNR
                                            rbukrs = <fs_destino>-rbukrs
                                  IMPORTING xblnr  = wa_reporte-ZREF_COB ).
    clear w_line_rep.
    w_line_rep-zfec_cobro = <fs_destino>-budat.

    if <fs_destino>-xblnr is initial.
      w_line_rep-zref_cob = <fs_destino>-awkey.
    else.
      w_line_rep-zref_cob = <fs_destino>-xblnr.
    endif.
*    w_line_rep-zind_imp = 'C0'.
    w_line_rep-zsociedad = <fs_destino>-rbukrs.
    w_line_rep-zejercicio = <fs_destino>-GJAHR.
    w_line_rep-zperiodo = <fs_destino>-POPER.
    w_line_rep-zdescrip = <fs_destino>-sgtxt.
    w_line_rep-zdocto_cobro = <fs_destino>-BELNR.
    w_line_rep-zclase_docto = <fs_destino>-blart.
*    if <fs_destino>-rwcur NE 'MXN'.
*      w_line_rep-zimporte_fact_me = <fs_destino>-wsl. "Imp. Fact en ME
*    endif.
*    w_line_rep-zimporte_fac_mn =  <fs_destino>-hsl.
    w_line_rep-zdes_ingreso = text-046.
    lcl_isr_tools=>map_bank_name( EXPORTING bukrs  = <fs_destino>-rbukrs
                                          hbkid  = <fs_destino>-hbkid
                                          hktid  = <fs_destino>-hktid
                                IMPORTING TEXT1  = w_line_rep-ZBANCO_COBRO
                                CHANGING  it_t012t = it_t012t ).
    if <fs_destino>-tsl < 0.
      w_line_rep-ZOTR_DEPOS  = <fs_destino>-tsl * ( -1 ).
    else.
      w_line_rep-ZOTR_DEPOS  = <fs_destino>-tsl.
    endif.
    w_line_rep-ZCOBRO_MN = w_line_rep-ZOTR_DEPOS.
    w_line_rep-ztotal  = w_line_rep-ZOTR_DEPOS.
    append w_line_rep to  P_IT_REPORTE.
  endloop.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FACTORAJE_SANTANDER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_FACTORAJE_SANTANDER .
  SELECT   acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~DOCLN,
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
           bkpf~kursf
      FROM acdoca
      INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                         bkpf~belnr = acdoca~belnr and
                         bkpf~gjahr = acdoca~ryear
      INTO TABLE @it_factsan_destino
      WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
            acdoca~rbukrs IN @sociedad           AND    "Sociedad
            acdoca~ryear  EQ @ejerc              AND    "Ejercicio
            acdoca~poper  IN @periodo            AND    "Periodo
            acdoca~racct  IN @rg_fact_sanesp     AND    "Cuentas Docts x cob descont
            acdoca~DRCRK = 'H' and
            acdoca~AWREF_REV = ''.

  if sy-subrc = 0.

    SELECT
         acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~DOCLN,
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
         bkpf~kursf
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO TABLE @it_factsan_origen
    FOR ALL ENTRIES IN @it_factsan_destino
    WHERE
          acdoca~RLDNR  = @it_factsan_destino-rldnr and
          acdoca~RBUKRS = @it_factsan_destino-RBUKRS and
          acdoca~RYEAR  = @it_factsan_destino-GJAHR  and
          acdoca~BELNR  = @it_factsan_destino-BELNR  and
          acdoca~racct  IN @rg_fact_santrans and
          acdoca~DRCRK = 'S' and
          acdoca~AWREF_REV = ''.
  endif.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ARMA_FACT_SAN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FACTSAN_ORIGEN
*&      --> IT_FACTSAN_DESTINO
*&      --> IT_REPORTE
*&---------------------------------------------------------------------*
FORM ARMA_FACT_SAN  USING    P_IT_FACTSAN_ORIGEN type ttraspaso
                             P_IT_FACTSAN_DESTINO type ttraspaso
                             P_IT_REPORTE type tty_report.

  data: w_line_rep type ty_report,
        factor16   type p DECIMALS 2 VALUE '0.16',
        factor4    type p DECIMALS 2 VALUE '0.04'.

  LOOP AT P_IT_FACTSAN_ORIGEN ASSIGNING FIELD-SYMBOL(<fs_origen>).

    lcl_isr_tools=>map_reference( EXPORTING gjahr  = <fs_origen>-GJAHR
                                            belnr  = <fs_origen>-BELNR
                                            rbukrs = <fs_origen>-rbukrs
                                  IMPORTING xblnr  = wa_reporte-ZREF_COB ).
    clear w_line_rep.
    w_line_rep-zfec_cobro = <fs_origen>-budat.

    if <fs_origen>-xblnr is initial.
      w_line_rep-zref_cob = <fs_origen>-awkey.
    else.
      w_line_rep-zref_cob = <fs_origen>-xblnr.
    endif.
*    w_line_rep-zind_imp = 'C0'.
    w_line_rep-zsociedad = <fs_origen>-rbukrs.
    w_line_rep-zejercicio = <fs_origen>-GJAHR.
    w_line_rep-zperiodo = <fs_origen>-POPER.
    w_line_rep-zdescrip = <fs_origen>-sgtxt.
    w_line_rep-zdocto_cobro = <fs_origen>-BELNR.
    w_line_rep-zclase_docto = <fs_origen>-blart.
*    if <fs_origen>-rwcur NE 'MXN'.
*      w_line_rep-zimporte_fact_me = <fs_origen>-wsl. "Imp. Fact en ME
*    endif.
*    w_line_rep-zimporte_fac_mn =  <fs_origen>-hsl.
    w_line_rep-zdes_ingreso = text-047.
    lcl_isr_tools=>map_bank_name( EXPORTING bukrs  = <fs_origen>-rbukrs
                                          hbkid  = <fs_origen>-hbkid
                                          hktid  = <fs_origen>-hktid
                                IMPORTING TEXT1  = w_line_rep-ZBANCO_COBRO
                                CHANGING  it_t012t = it_t012t ).
    if <fs_origen>-tsl < 0.
*      w_line_rep-zimp_base_16 = <fs_origen>-tsl * ( -1 ).
*      w_line_rep-ziva_al_16 = <fs_origen>-tsl * factor16   * ( -1 ).
*      w_line_rep-ziva_ret_4 = <fs_origen>-tsl * factor4.
      w_line_rep-ZOTR_DEPOS  = <fs_origen>-tsl * ( -1 ).
    else.
*      w_line_rep-zimp_base_16 = <fs_origen>-tsl.
*      w_line_rep-ziva_al_16 = <fs_origen>-tsl * factor16.
*      w_line_rep-ziva_ret_4 = <fs_origen>-tsl * factor4  * ( -1 ).
      w_line_rep-ZOTR_DEPOS  = <fs_origen>-tsl.
    endif.

    w_line_rep-ztotal  = <fs_origen>-tsl + w_line_rep-ziva_al_16 + w_line_rep-ziva_ret_4.
    w_line_rep-ZCOBRO_MN = w_line_rep-ztotal.
    append w_line_rep to  P_IT_REPORTE.
  endloop.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FACTORAJE_SENCILLO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_FACTORAJE_SENCILLO .
*
*      it_fact_pago          TYPE ttraspaso,
*      it_fact_intermedio    TYPE ttraspaso,
*      it_fact_compensados   TYPE ttraspaso,
  "Factoraje simple
*      rg_doc_pago           TYPE RANGE OF acdoca-blart,
*      rg_compensa_factor    TYPE RANGE OF acdoca-blart,
  SELECT   acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~DOCLN,
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
           bkpf~kursf
      FROM acdoca
      INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                         bkpf~belnr = acdoca~belnr and
                         bkpf~gjahr = acdoca~ryear
      INTO TABLE @it_fact_intermedio
      WHERE acdoca~rldnr  EQ @c_0l               AND    "Ledger
            acdoca~rbukrs IN @sociedad           AND    "Sociedad
            acdoca~blart  IN @rg_compensa_factor AND    "Cuentas de abonos en bancos
            acdoca~AWREF_REV = '' and
            acdoca~augbl  NE ''.

  if sy-subrc = 0.

    SELECT
         acdoca~RLDNR,
         acdoca~RBUKRS,
         acdoca~GJAHR,
         acdoca~POPER,
         acdoca~BLART,
         acdoca~BUDAT,
         acdoca~BLDAT,
         acdoca~BELNR,
         acdoca~DOCLN,
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
         bkpf~kursf
    FROM acdoca
        INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                           bkpf~belnr = acdoca~belnr and
                           bkpf~gjahr = acdoca~ryear
    INTO TABLE @it_fact_compensados
    FOR ALL ENTRIES IN @it_fact_intermedio
    WHERE
          acdoca~RLDNR  = @it_fact_intermedio-rldnr  and
          acdoca~RBUKRS = @it_fact_intermedio-RBUKRS and
          acdoca~AUGGJ  = @it_fact_intermedio-GJAHR  and
          acdoca~AUGBL  = @it_fact_intermedio-BELNR  and
          acdoca~blart not in @rg_doc_pago and
          acdoca~blart not IN @rg_compensa_factor AND
          acdoca~AWREF_REV = '' and
          acdoca~augbl  NE ''.

    if sy-subrc = 0.

      select
            bseg~bukrs,
            bseg~belnr,
            bseg~gjahr,
            bseg~sgtxt,
            bseg~hkont,
            bseg~BSCHL,
            bseg~KOART,
            bseg~MWSKZ,
            bseg~QSSKZ,
            bseg~DMBTR,
            bseg~WRBTR,
            bseg~KZBTR,
            bseg~PSWBT,
            bseg~AUGBL,
            bseg~AUGGJ,
            bkpf~budat,
            bseg~kunnr,
            bkpf~kursf,
            bseg~ktosl
        from bseg
        INNER JOIN bkpf on bkpf~bukrs = bseg~bukrs and
                           bkpf~belnr = bseg~belnr and
                           bkpf~gjahr = bseg~gjahr
        into table @it_fact_detalle
        for all entries in @it_fact_compensados
        where bseg~bukrs = @it_fact_compensados-RBUKRS and
              bseg~belnr = @it_fact_compensados-BELNR  and
              bseg~gjahr = @it_fact_compensados-GJAHR.

      if it_fact_detalle is NOT INITIAL.

        SELECT saknr,
               txt50
         FROM skat
         APPENDING CORRESPONDING FIELDS OF TABLE @it_skat
         FOR ALL ENTRIES IN @it_fact_detalle
         WHERE spras EQ @sy-langu AND
               ktopl EQ 'GPTX' AND
               saknr EQ @it_fact_detalle-hkont.

        sort it_skat by saknr.

        SELECT client,
            partner,
            taxtype,
            taxnum,
            taxnumxl
        FROM dfkkbptaxnum
        APPENDING TABLE @it_dfkkbptaxnum
        FOR ALL ENTRIES IN @it_fact_detalle
        WHERE partner  EQ @it_fact_detalle-kunnr.
        IF sy-subrc EQ 0.
          SORT it_dfkkbptaxnum BY partner.
        ENDIF.

        SELECT partner,
               name_org1,
               name_org2,
               name_org3,
               name_org4,
               name_last,
               name_first
        FROM but000
        APPENDING CORRESPONDING FIELDS OF TABLE @it_but000
        FOR ALL ENTRIES IN @it_fact_detalle
        WHERE partner EQ @it_fact_detalle-kunnr.
      endif.

      SELECT
           acdoca~RLDNR,
           acdoca~RBUKRS,
           acdoca~GJAHR,
           acdoca~POPER,
           acdoca~BLART,
           acdoca~BUDAT,
           acdoca~BLDAT,
           acdoca~BELNR,
           acdoca~DOCLN,
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
           bkpf~kursf
      FROM acdoca
          INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                             bkpf~belnr = acdoca~belnr and
                             bkpf~gjahr = acdoca~ryear
      INTO TABLE @it_fact_pago
      FOR ALL ENTRIES IN @it_fact_intermedio
      WHERE
            acdoca~RLDNR  = @it_fact_intermedio-rldnr  and
            acdoca~RBUKRS = @it_fact_intermedio-RBUKRS and
            acdoca~AUGGJ  = @it_fact_intermedio-GJAHR  and
            acdoca~augbl  = @it_fact_intermedio-BELNR  and
            acdoca~ryear  EQ @ejerc                    AND    "Ejercicio
            acdoca~poper  IN @periodo                  AND    "Periodo
            acdoca~AWREF_REV = '' and
            acdoca~blart in @rg_doc_pago.
      if sy-subrc = 0.
        SELECT
        acdoca~RLDNR,
        acdoca~RBUKRS,
        acdoca~GJAHR,
        acdoca~POPER,
        acdoca~BLART,
        acdoca~BUDAT,
        acdoca~BLDAT,
        acdoca~BELNR,
        acdoca~DOCLN,
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
        bkpf~kursf
   FROM acdoca
       INNER JOIN bkpf on bkpf~bukrs = acdoca~rbukrs and
                          bkpf~belnr = acdoca~belnr and
                          bkpf~gjahr = acdoca~ryear
   INTO TABLE @it_fact_pago_detalle
   FOR ALL ENTRIES IN @it_fact_pago
   WHERE
         acdoca~RLDNR  = @it_fact_pago-rldnr  and
         acdoca~RBUKRS = @it_fact_pago-RBUKRS and
         acdoca~GJAHR  = @it_fact_pago-GJAHR  and
         acdoca~BELNR  = @it_fact_pago-BELNR  and
         acdoca~AWREF_REV = ''.
      endif.

    endif.
  endif.

ENDFORM.

FORM ARMA_FACT_SIMPLE  USING    P_IT_FACT_PAGO       type ttraspaso
                                P_IT_FACT_INTERMEDIO type ttraspaso
                                P_IT_FACT_COMPENSADO type ttraspaso
                                P_IT_REPORTE         type tty_report
                                P_IT_DETALLE         type tty_detalle
                                P_IT_FACT_PAGO_DET   type ttraspaso.
  types: begin of docs,
           RLDNR  type acdoca-RLDNR,
           RBUKRS type acdoca-RBUKRS,
           GJAHR  type acdoca-GJAHR,
           BELNR  type acdoca-BELNR,
           AUGGJ  type acdoca-AUGGJ,
           AUGBL  type acdoca-AUGBL,
           AUGDT  type acdoca-AUGDT,
           XBLNR  type bkpf-XBLNR,
           AWKEY  type bkpf-AWKEY,
           POPER  type acdoca-POPER,
           kursf  type bkpf-kursf,
           SGTXT  type acdoca-sgtxt,
         end of docs,
         tdocs type standard table of docs
                 with NON-UNIQUE key rldnr
              RBUKRS
              GJAHR
              BELNR.

  data: w_line_rep       type ty_report,
        FACTORAJE        type tty_report,
        tdocs_pago       type tdocs,
        tdocs_intermedio type tdocs,
        flag             type xfeld,
        flag_delete      type xfeld,
        tabix            type sy-tabix.

  select distinct
    pago~RLDNR,
    pago~RBUKRS,
    pago~GJAHR,
    pago~BELNR,
    pago~AUGGJ,
    pago~AUGBL,
    pago~AUGDT,
    pago~XBLNR,
    pago~AWKEY,
    pago~POPER,
    pago~kursf,
    pago~sgtxt
  from @P_IT_FACT_PAGO as pago
  order by RLDNR,RBUKRS,GJAHR,BELNR
  into table @tdocs_pago.


  select distinct
     intermedio~RLDNR,
     intermedio~RBUKRS,
     intermedio~GJAHR,
     intermedio~BELNR,
     intermedio~AUGGJ,
     intermedio~AUGBL,
     intermedio~AUGDT,
     intermedio~XBLNR,
     intermedio~AWKEY,
     intermedio~POPER
   from @P_IT_FACT_INTERMEDIO as intermedio
   order by RLDNR,RBUKRS,GJAHR,BELNR
   into table @tdocs_intermedio.

*Filtrado de documentos interemedios
  loop at tdocs_intermedio into DATA(ls_intermediox).
    tabix = sy-tabix.
    READ TABLE tdocs_pago into DATA(ls_pagox)
          WITH key
               rldnr  = ls_intermediox-RLDNR
               RBUKRS = ls_intermediox-RBUKRS
               AUGGJ  = ls_intermediox-GJAHR
               AUGBL  = ls_intermediox-BELNR.
    if sy-subrc ne 0.
      delete TDOCS_INTERMEDIO index tabix.
    endif.
  ENDLOOP.

*Filtrado de la compensación
  loop at P_IT_FACT_COMPENSADO into DATA(ls_compensado).
    tabix = sy-tabix.
    read table tdocs_intermedio into ls_intermediox
    with key
                 rldnr  = ls_compensado-RLDNR
                 RBUKRS = ls_compensado-RBUKRS
                 AUGGJ  = ls_compensado-AUGGJ
                 AUGBL  = ls_compensado-AUGBL.
    if sy-subrc ne 0.
      delete P_IT_FACT_COMPENSADO index tabix.
    endif.
  endloop.

*15.  Determinación de carta porte de factura
  IF P_IT_FACT_COMPENSADO IS NOT INITIAL.
    SELECT bukrs,
      belnr,
      gjahr,
      awkey
    FROM bkpf
    INTO TABLE @DATA(it_bkpf_carta1)
    FOR ALL ENTRIES IN @P_IT_FACT_COMPENSADO
    WHERE bukrs EQ @P_IT_FACT_COMPENSADO-rbukrs AND
          belnr EQ @P_IT_FACT_COMPENSADO-belnr  AND
          gjahr EQ @P_IT_FACT_COMPENSADO-gjahr.

    IF sy-subrc EQ 0.

      SORT it_bkpf_carta1 BY bukrs belnr gjahr awkey.

      SELECT vbelv,
             posnv,
             vbeln,
             posnn
        FROM vbfa
        INTO TABLE @DATA(it_vbfa1)
        FOR ALL ENTRIES IN @it_bkpf_carta1
        WHERE vbeln EQ @it_bkpf_carta1-awkey(10) AND
              stufe IN @rg_stufe AND
              vbtyp_v EQ 'C'.
      IF sy-subrc EQ 0.
        SORT it_vbfa1 BY vbelv posnv.

        SELECT vbeln,
               posnr,
               bstkd
          FROM vbkd
          INTO TABLE @DATA(it_vbkd1)
          FOR ALL ENTRIES IN @it_vbfa1
          WHERE vbeln EQ @it_vbfa1-vbelv AND
                posnr EQ @it_vbfa1-posnv.
        IF sy-subrc EQ 0.
          SORT it_vbkd1 BY vbeln posnr.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDIF.

  sort it_t012t by spras bukrs hbkid hktid.
  IF it_bkpf_carta1 IS NOT INITIAL.

    LOOP AT it_bkpf_carta1 ASSIGNING FIELD-SYMBOL(<fs_carta1>).

      LOOP AT it_vbfa1 ASSIGNING FIELD-SYMBOL(<fs_vbfa1>) WHERE vbeln = <fs_carta1>-awkey.

        sort it_vbkd1 by vbeln posnr.
        READ TABLE it_vbkd1
        ASSIGNING FIELD-SYMBOL(<fs_vbkd1>)
        WITH KEY vbeln = <fs_vbfa1>-vbelv
                 posnr = <fs_vbfa1>-posnv
        BINARY SEARCH.
        IF sy-subrc EQ 0.

          wa_carta_porte-bukrs = <fs_carta1>-bukrs.
          wa_carta_porte-belnr = <fs_carta1>-belnr.
          wa_carta_porte-gjahr = <fs_carta1>-gjahr.
          wa_carta_porte-awkey = <fs_carta1>-awkey.
          wa_carta_porte-bstkd = <fs_vbkd1>-vbeln.

          APPEND  wa_carta_porte TO it_carta_porte.
          CLEAR: wa_carta_porte.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

  loop at tdocs_pago into DATA(ls_pago).

    loop at tdocs_intermedio into DATA(ls_intermedio)
            WHERE
                 rldnr  = ls_pago-RLDNR  and
                 RBUKRS = ls_pago-RBUKRS and
                 GJAHR  = ls_pago-AUGGJ  and
                 BELNR  = ls_pago-AUGBL.

      clear flag.

      loop at P_IT_FACT_COMPENSADO into ls_compensado
          WHERE
                  rldnr  = ls_intermedio-RLDNR  and
                  RBUKRS = ls_intermedio-RBUKRS and
                  AUGGJ  = ls_intermedio-AUGGJ  and
                  AUGBL = ls_intermedio-AUGBL.


        sort it_carta_porte by bukrs belnr gjahr.
        READ TABLE it_carta_porte
        ASSIGNING FIELD-SYMBOL(<fs_carta_porte>)
        WITH KEY bukrs = ls_compensado-rbukrs
                 belnr = ls_compensado-belnr
                 gjahr = ls_compensado-gjahr
         BINARY SEARCH.
        IF sy-subrc EQ 0.
          w_line_rep-zcarta_porte = <fs_carta_porte>-bstkd. "Carta Porte
        ENDIF.

        loop at P_IT_FACT_PAGO_DET into DATA(ls_pago_detalle)
          where rbukrs = ls_pago-rbukrs and
                belnr  = ls_pago-belnr and
                gjahr  = ls_pago-gjahr and
                hbkid is not INITIAL and
                hktid is NOT INITIAL.
          READ TABLE it_t012t
                      ASSIGNING FIELD-SYMBOL(<fs_t012t>)
                      WITH KEY spras = c_es
                               bukrs = ls_pago_detalle-rbukrs
                               hbkid = ls_pago_detalle-hbkid
                               hktid = ls_pago_detalle-hktid
                       BINARY SEARCH.
          IF sy-subrc EQ 0.
            w_line_rep-zbanco_cobro = <fs_t012t>-text1. "Banco del cobro
          ENDIF.
        endloop.

        w_line_rep-zdocto_cobro = ls_pago_detalle-BELNR.
        w_line_rep-zfec_cobro =  ls_intermedio-augdt.
        w_line_rep-zref_cob = ls_compensado-xblnr.
        w_line_rep-ztc_cobro = ls_intermedio-kursf.
        w_line_rep-zsociedad = ls_intermedio-rbukrs.
        w_line_rep-zperiodo = ls_intermedio-poper.
        w_line_rep-zdescrip = ls_intermedio-sgtxt.
        w_line_rep-zejer_cobro = ls_pago_detalle-gjahr.

        if ls_pago-xblnr is initial.
          w_line_rep-zref_cob = ls_intermedio-awkey.
        else.
          w_line_rep-zref_cob = ls_intermedio-xblnr.
        endif.

        if flag is INITIAL.
          w_line_rep-zcobro_mn = ls_pago_detalle-tsl.
          w_line_rep-zcobro_me = ls_pago_detalle-wsl.
          flag = 'X'.
        else.
          clear: w_line_rep-zcobro_mn,
                 w_line_rep-zcobro_me.
        ENDIF.
        w_line_rep-zper_real = ls_intermedio-poper.
*                        zutil_real = ''
*                        zbanco_cobro = ''
*                        ztc_cobro    = ''
        w_line_rep-zfec_factura  = ls_compensado-budat.
        w_line_rep-zdocto_factura = ls_compensado-belnr.
        w_line_rep-zasig  = ls_compensado-xblnr.
*

        CONCATENATE ls_compensado-RBUKRS ls_compensado-belnr ls_compensado-gjahr INTO v_name.
        CALL FUNCTION 'ZFI_ISR_READ_TEXT_BUFFER'
          EXPORTING
*           CLIENT                  = SY-MANDT
            ID                      = 'YUUD'
            LANGUAGE                = c_es
            NAME                    = v_name
            OBJECT                  = 'BELEG'
          TABLES
            LINES                   = it_line
          EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.
        IF sy-subrc EQ 0.

          LOOP AT it_line ASSIGNING FIELD-SYMBOL(<fs_line>).
            w_line_rep-zuuid = <fs_line>-tdline.
          ENDLOOP.

        ENDIF.
*                        zcarta_porte = ''
        w_line_rep-zclase_docto = ls_compensado-blart.

        if ls_compensado-rwcur NE 'MXN'.
          w_line_rep-zimporte_fact_me = ls_compensado-wsl. "Imp. Fact en ME
        endif.
        w_line_rep-zimporte_fac_mn =  ls_compensado-hsl.
*                         Datos de impuestos
        clear:  w_line_rep-zimp_base_16,
                w_line_rep-zind_16,
                w_line_rep-zimp_base_exc,
                w_line_rep-zind_exc,
                w_line_rep-zimp_base_0,
                w_line_rep-zind_imp,
                w_line_rep-ziva_al_16,
                w_line_rep-ziva_ret_4,
                w_line_rep-ziva_ret_6.


        loop at P_IT_DETALLE into DATA(LS_DETALLE) where bukrs = ls_compensado-rbukrs and
                                                         belnr = ls_compensado-belnr and
                                                         gjahr = ls_compensado-gjahr.

          w_line_rep-ztc_factura = ls_detalle-kursf.
          w_line_rep-zejercicio = LS_DETALLE-gjahr.

          IF ls_detalle-HKONT IN rg_ctas_ing_vta.
            sort it_skat by saknr.
            READ TABLE it_skat
            ASSIGNING FIELD-SYMBOL(<fs_skat>)
            WITH KEY saknr = ls_detalle-hkont
             BINARY SEARCH.

            if <fs_skat> is ASSIGNED.
              w_line_rep-zdes_ingreso = <fs_skat>-txt50.
            endif.
          endif.

          if ls_detalle-kunnr is NOT INITIAL.
            w_line_rep-zno_cliente = ls_detalle-kunnr. "No de cliente
            lcl_isr_tools=>map_client_data( exporting kunnr = w_line_rep-zno_cliente
                                                     it_dfkkbptaxnum1 = it_dfkkbptaxnum
                                                     it_but0001       = it_but000
                                           IMPORTING wa_reporte1 = w_line_rep ).
          endif.

*         Base e indicadores al 16%
          IF ls_detalle-HKONT IN rg_ctas_ing_vta and LS_DETALLE-mwskz IN rg_base_16.
            w_line_rep-zimp_base_16 =  LS_DETALLE-DMBTR + w_line_rep-zimp_base_16. "Importe grabado al 16%
            w_line_rep-zind_16 =  LS_DETALLE-mwskz. "Importe grabado al 16%
          ENDIF.

*         Iva al 16%
          IF ls_detalle-HKONT IN rg_ctas_iva_vta and LS_DETALLE-mwskz IN rg_base_16.
            w_line_rep-ziva_al_16 = w_line_rep-ziva_al_16 + LS_DETALLE-WRBTR. "IVA al 16%
          endif.

*         Base e indicadores excluidos
          IF  ls_detalle-HKONT IN rg_ctas_ing_vta and LS_DETALLE-mwskz IN rg_base_exc.
            w_line_rep-zimp_base_exc =   LS_DETALLE-DMBTR + w_line_rep-zimp_base_exc. "Importe Exento
            w_line_rep-zind_exc =   LS_DETALLE-mwskz. "Importe Exento
          ENDIF.

*         Base e indicadores base 0
          IF ls_detalle-HKONT IN rg_ctas_ing_vta and LS_DETALLE-mwskz IN rg_base_0.
            w_line_rep-zimp_base_0 = LS_DETALLE-DMBTR + w_line_rep-zimp_base_0 . "Importe Tasa 0
            w_line_rep-zind_imp =  LS_DETALLE-mwskz. "Importe Tasa 0
          ENDIF.
* Iva retenido al 4%
          if LS_DETALLE-HKONT IN rg_ctas_ret_iva_ing_4.
            w_line_rep-ziva_ret_4 = w_line_rep-ziva_ret_4 + LS_DETALLE-WRBTR * ( -1 ).
          endif.
* Iva retenido al 6%
          if LS_DETALLE-HKONT  IN rg_ctas_ret_iva_ing and LS_DETALLE-KTOSL = 'WIT'.
            w_line_rep-ziva_ret_6 = w_line_rep-ziva_ret_6 + LS_DETALLE-WRBTR * ( -1 ).
          endif.

        endloop.

        if w_line_rep-Zclase_docto in rg_nc_clase or ls_detalle-hkont in rg_desc_reb.
          w_line_rep-zimp_base_16  = w_line_rep-zimp_base_16 * ( -1 ).
          w_line_rep-zimp_base_exc = w_line_rep-zimp_base_exc * ( -1 ).
          w_line_rep-zimp_base_0 = w_line_rep-zimp_base_0 * ( -1 ).
          w_line_rep-ziva_al_16 =  w_line_rep-ziva_al_16 * ( -1 ).
          w_line_rep-ziva_ret_4 = w_line_rep-ziva_ret_4 * ( -1 ).
          w_line_rep-ziva_ret_6 = w_line_rep-ziva_ret_6 * ( -1 ).
        endif.
        w_line_rep-ztotal = w_line_rep-zimp_base_16 + w_line_rep-zimp_base_exc + w_line_rep-zimp_base_0 + w_line_rep-ziva_al_16 + w_line_rep-ziva_ret_4 + w_line_rep-ziva_ret_6.
        append w_line_rep to P_IT_REPORTE.
      endloop.
    endloop.
  endloop.
endform.
