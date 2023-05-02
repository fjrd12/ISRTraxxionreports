*&---------------------------------------------------------------------*
*& Include          ZFIRE_PAGOS_PROVISIONALES_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CONSULTAS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM consultas .


  if P0 = 'X'.
*UPDATE ACDOCA
*                  SET RCNTR = 'TE01010101'
*                      PRCTR = 'TE01010101'
*                  WHERE RLDNR  = '0L'AND
*                        RBUKRS = 'TE00'    AND
*                        GJAHR  = '2020'    AND
*                        BELNR  = '5100000157'    AND
*                        DOCLN  = '000003'.
*
*
*        COMMIT WORK.
    SELECT  COUNT( * ) INTO @vl_total_registros
           FROM ztax_detalle_egr.
    PERFORM rangos.
    DATA: lv_pago_procesado  TYPE bseg-belnr.

    "1.   Búsqueda de doctos KZ para sociedad, periodo, ejercicio busca pagos
    SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
           kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
           augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt,linetype "fecha_extracto,doc_extracto,
           , zuonr"refe_pago,importe_pago,texto_banco
      INTO TABLE @DATA(tl_acdoca)
      FROM acdoca
      WHERE rldnr  = '0L'         AND
            rbukrs IN @p_bukrs    AND
            gjahr  = @p_gjahr     AND
            blart  IN @rg_blart   AND
            poper  IN @p_monat    AND
            augbl  IN @rg_augbl   AND
            belnr IN @s_belnr     AND "Insert AMP 22.12.2020
             lifnr NOT IN @rg_lifnr.
    IF sy-subrc = 0.

* LQ liquidaciones


*    delete tl_acdoca where DOCLN NE  '000001'.
* Busca el rfc de proveedor
      SELECT partner,taxtype,taxnum,taxnumxl
        INTO TABLE @DATA(tl_dfkkbptaxnum)
        FROM dfkkbptaxnum
        FOR ALL ENTRIES IN @tl_acdoca
        WHERE partner  = @tl_acdoca-lifnr AND
              taxtype  = @c_mx1.
      SELECT partner,name_org1,name_org2,name_org3,name_org4,name_last,name_first,natpers
        INTO TABLE @DATA(tl_but000)
        FROM but000
        FOR ALL ENTRIES IN @tl_acdoca
        WHERE partner  = @tl_acdoca-lifnr.

*investiga el pago y quien lo compensa para el extracto de banco
      "Cada uno de estos pagos se deberá validar en la tabla pago de la tabla BKPF
      SELECT bukrs,belnr,gjahr,stblg,bktxt,awkey
        INTO TABLE @DATA(tl_bkpf)
        FROM bkpf
        FOR ALL ENTRIES IN @tl_acdoca
        WHERE bukrs = @tl_acdoca-rbukrs AND
              belnr = @tl_acdoca-belnr AND
              gjahr = @tl_acdoca-gjahr.

      IF p_pag_c IS NOT INITIAL."Validamos si es con extrantos bancarios
        "2.       Búsqueda de doctos KZ para sociedad, periodo y docto y validación en extracto bancario
*   Se recupera el campo BKPF- STBLG, el cual deberá estar vacío,
*   en caso contrario no deberá tomarse en cuenta puesto que el pago esta anulado, para el paso 2.
        DELETE tl_bkpf WHERE stblg = abap_on.
*Se determina la posición de bancos:
*Se busca el campo RACCT IN CUENTAS_BANCOS esta será la variable TVARVC para las salidas de bancos,
      ENDIF.
*      se busca el campo racct in cuentas_bancos esta será la variable tvarvc para las salidas de bancos,
*      prctr ne vacio y drcrk = h.
      "Busqueda a la tabla bseg por cada documento de pago sus datos descompone el pago en sus partidas
      SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
        INTO TABLE @DATA(tl_bseg)
        FROM bseg
        FOR ALL ENTRIES IN @tl_bkpf
        WHERE bukrs =  @tl_bkpf-bukrs AND
              belnr =  @tl_bkpf-belnr  AND
              gjahr =  @tl_bkpf-gjahr  AND
              ( hkont IN @rg_hkont OR
                lifnr NOT IN @rg_lifnr ). "  NO busca los  de factoraje
      IF sy-subrc = 0.
        IF p_pag_s IS NOT INITIAL.
* busca los datos del documento sin  extracto y su descripción
          SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
         INTO TABLE @DATA(tl_bseg_sin_e)
         FROM bseg
         FOR ALL ENTRIES IN @tl_bseg
         WHERE bukrs =  @tl_bseg-bukrs     AND
               belnr =  @tl_bseg-belnr     AND
               gjahr =  @tl_bseg-gjahr     AND
               shkzg = @c_s.
          IF sy-subrc = 0.
            SELECT land1,zlsch,text1
           INTO TABLE @DATA(tl_t042z)
           FROM t042z
           FOR ALL ENTRIES IN @tl_bseg_sin_e
           WHERE land1 = @c_mx             AND
                 zlsch = @tl_bseg_sin_e-zlsch.
          ENDIF.
        ENDIF.
* valida datos de extracto
* borra los que no tienen extracto
        IF p_pag_c IS NOT INITIAL.
          DELETE tl_bseg WHERE augbl IS INITIAL.
        ENDIF.
        SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
       INTO TABLE @DATA(tl_bseg_2)
       FROM bseg
       FOR ALL ENTRIES IN @tl_bseg
       WHERE bukrs =  @tl_bseg-bukrs     AND
             belnr =  @tl_bseg-augbl     AND
             gjahr =  @tl_bseg-gjahr
         AND hkont IN @rg_hkont.  "Insert AMP 15.01.2021
        "validamos para escenario 10 zounr
        LOOP AT tl_bseg ASSIGNING FIELD-SYMBOL(<fs_bseg>).
          IF <fs_bseg>-zuonr IS NOT INITIAL.
            vl_length = strlen( <fs_bseg>-zuonr ).
            IF vl_length <= 10 AND vl_length >= 2.
              IF <fs_bseg>-zuonr(2) IN rg_z_referencia_interme."Solo Permite Registros dentro de la variante
                vl_belnr_aux = <fs_bseg>-zuonr.
                vl_belnr_aux = |{ vl_belnr_aux ALPHA = IN }|.
                rg_belnr = VALUE  #(
                        BASE rg_belnr ( low    = vl_belnr_aux
                                        sign   = c_i
                                        option = c_eq ) ).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF rg_belnr IS NOT INITIAL.
          SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
         INTO TABLE @DATA(tl_bseg_escenario10)
         FROM bseg
         FOR ALL ENTRIES IN @tl_bseg
         WHERE bukrs =   @tl_bseg-bukrs     AND
               belnr IN  @rg_belnr          AND
               gjahr =   @tl_bseg-gjahr.
          IF sy-subrc = 0.
            DATA(tl_bseg_escenario10_iva) = tl_bseg_escenario10.
            DELETE tl_bseg_escenario10 WHERE hkont IN rg_ziva_16.
            DELETE tl_bseg_escenario10_iva WHERE hkont NOT IN rg_ziva_16.
          ENDIF.
        ENDIF.
      ENDIF.


*3.     Búsqueda de doctos compensados contra doctos KZ para la sociedad,
*   periodo y docto sin validación de extracto bancario.

      IF tl_bseg IS NOT INITIAL.
        "4.-Se procederá a leer de la tabla ACDOCA Universal Journal Entry Line Items por medio de los parámetros de entrada:
*Ledger     - RLDNR = 0L
*Sociedad -  Es el docto compensado de pago en esta sociedad
*Ejercicio   - Es el ejercicio compensado en este ejercicio
*Clase de docto = deberá estar las clases de factura y documento de pago, es la variable de la TVARVC ZDOC_FAC_C, dentro del parámetro individual se carga la clase de docto de pago, en este caso será KZ.
*AUGBL -  Este será toda la lista documentos obtenidos del punto 2 o 3

* FACTURAS
        SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
               kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
               augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt,linetype "fecha_extracto,doc_extracto,
               , zuonr
               "refe_pago,importe_pago,texto_banco
          INTO TABLE @DATA(tl_acdoca2)
          FROM acdoca
          FOR ALL ENTRIES IN @tl_bseg
          WHERE rldnr  = '0L'           AND
                rbukrs = @tl_bseg-bukrs AND
                gjahr  = @tl_bseg-gjahr AND
                blart  IN @rg_blart_2   AND
                augbl  = @tl_bseg-belnr
            AND bschl IN @rg_cargo_abono_compro_low.  "Insert AMP 14.01.2021
* Este es la factura en la posicion de acreedor con IVA o sin el incluido de debería leer para el reporte final
        IF sy-subrc = 0.
          "Validacion para escenario 3
          SORT tl_acdoca2 BY rbukrs augbl gjahr docln ASCENDING.

          DATA(tl_solo_nom) =  tl_acdoca2.
          DELETE  tl_solo_nom WHERE blart = 'CG'.

*** INVESTIGA SI DEBE ENTRAR O NO

          SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
       INTO TABLE @DATA(tl_bseg_nomina)
       FROM bseg
       FOR ALL ENTRIES IN @tl_solo_nom
       WHERE bukrs  =   @tl_solo_nom-rbukrs     AND
             belnr  =    @tl_solo_nom-belnr    AND
             bschl  = '31'                     AND
             gjahr  =     @tl_solo_nom-gjahr   AND
             h_blart =     'KR'.

          IF sy-subrc = 0.
            LOOP AT tl_bseg_nomina ASSIGNING FIELD-SYMBOL(<fs_solonom>).
              IF <fs_solonom>-zuonr+0(3) NE '600'.
                DELETE tl_solo_nom WHERE rbukrs =  <fs_solonom>-bukrs AND
                                         gjahr  =  <fs_solonom>-gjahr AND
                                         belnr  =   <fs_solonom>-belnr.
              ENDIF.
            ENDLOOP.
          ENDIF.


          DATA(tl_acdoca_aux) = tl_solo_nom.
          REFRESH: tl_acdoca2.
          LOOP AT tl_acdoca_aux ASSIGNING FIELD-SYMBOL(<fs_acdoca>).
            IF sy-tabix = 1.
              DATA(wa_acdoca) = <fs_acdoca>.
            ELSE.
              IF wa_acdoca-rbukrs = <fs_acdoca>-rbukrs  AND
                 wa_acdoca-belnr  = <fs_acdoca>-belnr   AND
                 wa_acdoca-gjahr  = <fs_acdoca>-gjahr.
                wa_acdoca-tsl = wa_acdoca-tsl + <fs_acdoca>-tsl.

              ELSE.
                tl_acdoca2 = VALUE  #(
                         BASE tl_acdoca2 ( wa_acdoca ) ).
                wa_acdoca = <fs_acdoca>.
              ENDIF.

            ENDIF.
          ENDLOOP.
          tl_acdoca2 = VALUE  #(
                          BASE tl_acdoca2 ( wa_acdoca ) ).
          tl_acdoca_aux = tl_acdoca2.
          DELETE ADJACENT DUPLICATES FROM tl_acdoca2 COMPARING rbukrs augbl gjahr mwskz zuonr .
          tl_acdoca_aux = tl_acdoca2.
* Se usa en el escenario 10
          DELETE tl_acdoca_aux WHERE zuonr IS INITIAL.
          LOOP AT tl_acdoca_aux ASSIGNING <fs_acdoca>.
            IF <fs_acdoca>-sgtxt IS NOT INITIAL.
              CALL FUNCTION 'NUMERIC_CHECK'
                EXPORTING
*                 string_in = <fs_acdoca>-sgtxt
                  string_in = <fs_acdoca>-zuonr   "Insert AMP 17.02.2021  Se cambia al campo ZUONR.
                IMPORTING
*                 string_out =
                  htype     = vl_htype
                EXCEPTIONS
                  OTHERS    = 1.
              IF vl_htype EQ 'NUMC'.
*              vl_docu_ejercicio = <fs_acdoca>-sgtxt.
                vl_docu_ejercicio = <fs_acdoca>-zuonr .   "Insert AMP 17.02.2021
                vl_docu_ejercicio = |{ vl_docu_ejercicio ALPHA = IN }|.
                rg_docval = VALUE  #(
                            BASE rg_docval ( low    = vl_docu_ejercicio(10)
                                               sign   = c_i
                                               option = c_eq ) ).
                rg_z_ejercicioval = VALUE  #(
                             BASE rg_z_ejercicioval ( low    = vl_docu_ejercicio+10(4)
                                                      sign   = c_i
                                                      option = c_eq ) ).
              ENDIF.
            ENDIF.
          ENDLOOP.
          IF rg_docval IS NOT INITIAL.
            SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
               INTO TABLE @DATA(tl_bseg_escenario3)
               FROM bseg
               WHERE bukrs IN   @p_bukrs       AND
                     belnr IN  @rg_docval     AND
                     gjahr IN  @rg_z_ejercicioval.
            IF sy-subrc = 0.
              DATA(tl_cargo_con_extracto) = tl_bseg_escenario3.
              REFRESH: tl_cargo_con_extracto.
              DATA(tl_bseg_aux) = tl_cargo_con_extracto.
              DATA(tl_bseg_aux_2) = tl_cargo_con_extracto.
              DATA(tl_abono_con_extracto) = tl_cargo_con_extracto.
              LOOP AT tl_bseg_escenario3 ASSIGNING <fs_bseg>.
                IF <fs_bseg>-shkzg = c_s.
                  tl_cargo_con_extracto  = VALUE  #(
                            BASE tl_cargo_con_extracto          (  <fs_bseg>  )   ).
                ELSEIF <fs_bseg>-shkzg = c_h.
                  tl_abono_con_extracto  = VALUE  #(
                            BASE tl_abono_con_extracto          (  <fs_bseg>  )   ).
                ENDIF.
              ENDLOOP.

**Detalle de nómina que vamos a validar que cuenta es la que esta procesando de la factura de nómina

              SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
             kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
             augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt,linetype "fecha_extracto,doc_extracto,
             , zuonr
             "refe_pago,importe_pago,texto_banco
             INTO TABLE @DATA(tl_cuentanom)
             FROM acdoca
             FOR ALL ENTRIES IN @tl_acdoca2
             WHERE rldnr  = '0L'           AND
              rbukrs = @tl_acdoca2-rbukrs AND
              gjahr  = @tl_acdoca2-gjahr AND
              belnr   = @tl_acdoca2-belnr AND
              racct  IN @rg_nomina_ctas_a_detallar_low AND
              bschl IN @rg_cargo_abono_low.  "Insert AMP 14.01.2021

              IF sy-subrc = 0.
                " es una factura de nomina solo toma 1 posición
                READ TABLE tl_cuentanom  INTO DATA(wa_cuenta_det)  INDEX 1.
                IF sy-subrc = 0.
                  READ TABLE  rg_nomina_exclusion_h INTO DATA(wa_nomina_excl) WITH   KEY low = wa_cuenta_det-racct.
                  IF sy-subrc = 0. " es la que ya existia en el rango y no se da de alta


                  ELSE.
                    DATA(rg_cta_det) =  rg_nomina_ctas_a_detallar_low. " respada el rango
                    "Agrega la cuenta a detallar en las excluidas
                    rg_nomina_exclusion_h = VALUE  #(
                    BASE rg_nomina_exclusion_h    ( low    =  wa_cuenta_det-racct
                                             sign   = c_i
                                             option = c_eq ) ).
                    "Ahora borra de las que deben borrarse para quitarlas de las excluidas
                    DELETE  rg_cta_det WHERE low EQ wa_cuenta_det-racct.
                    "solo quedan las que ahora se excluiran
                    DELETE  rg_nomina_exclusion_h WHERE low IN rg_cta_det.
                  ENDIF.
                  " busca la descripción de la cuenta de nomina a determinar su detalle
                  READ TABLE rg_resumen_nomina INTO  DATA(wa_ctaproc) WITH KEY low = wa_cuenta_det-racct.
                  IF sy-subrc = 0.
                  ENDIF.
                ENDIF.

              ENDIF.

              DELETE tl_cargo_con_extracto WHERE hkont IN rg_nomina_exclusion_s.
              DELETE tl_abono_con_extracto WHERE hkont IN rg_nomina_exclusion_h.
              SORT tl_abono_con_extracto BY bukrs belnr gjahr hkont.
              IF tl_cargo_con_extracto IS NOT INITIAL.
                SORT tl_cargo_con_extracto BY hkont.
*
                LOOP AT tl_cargo_con_extracto ASSIGNING <fs_bseg>.
                  IF sy-tabix = 1.
                    DATA(wa_bseg) = <fs_bseg>.
                  ELSE.
                    IF wa_bseg-bukrs = <fs_bseg>-bukrs AND  wa_bseg-belnr = <fs_bseg>-belnr AND
                       wa_bseg-gjahr = <fs_bseg>-gjahr AND wa_bseg-hkont =  <fs_bseg>-hkont.
                      wa_bseg-dmbtr = wa_bseg-dmbtr + <fs_bseg>-dmbtr.
                    ELSE.
                      tl_bseg_aux  = VALUE  #(
                                       BASE tl_bseg_aux          (   wa_bseg  )   ).
                      tl_bseg_aux_2  = VALUE  #(
                                       BASE tl_bseg_aux_2          (   wa_bseg  )   ).
                      CLEAR: wa_bseg.
                      wa_bseg = <fs_bseg>.
                    ENDIF.
                  ENDIF.

                ENDLOOP.
                IF wa_bseg IS NOT INITIAL.
                  tl_bseg_aux  = VALUE  #(
                              BASE tl_bseg_aux          (   wa_bseg  )   ).
                  tl_bseg_aux_2  = VALUE  #(
                                   BASE tl_bseg_aux_2          (   wa_bseg  )   ).
                ENDIF.
                tl_cargo_con_extracto = tl_bseg_aux.
              ENDIF.
              IF tl_abono_con_extracto IS NOT INITIAL.
                REFRESH: tl_bseg_aux.
                SORT tl_abono_con_extracto BY hkont.
                LOOP AT tl_abono_con_extracto ASSIGNING <fs_bseg>.
                  IF sy-tabix = 1.
                    wa_bseg = <fs_bseg>.
                  ELSE.
                    IF wa_bseg-bukrs = <fs_bseg>-bukrs AND  wa_bseg-belnr = <fs_bseg>-belnr AND
                       wa_bseg-gjahr = <fs_bseg>-gjahr AND wa_bseg-hkont =  <fs_bseg>-hkont.
                      wa_bseg-dmbtr = wa_bseg-dmbtr + <fs_bseg>-dmbtr.
                    ELSE.
                      tl_bseg_aux  = VALUE  #(
                                       BASE tl_bseg_aux          (   wa_bseg  )   ).
                      tl_bseg_aux_2  = VALUE  #(
                              BASE tl_bseg_aux_2          (   wa_bseg  )   ).
                      CLEAR: wa_bseg.
                      wa_bseg = <fs_bseg>.
                    ENDIF.
                  ENDIF.

                ENDLOOP.
                IF wa_bseg IS NOT INITIAL.
                  tl_bseg_aux  = VALUE  #(
                              BASE tl_bseg_aux          (   wa_bseg  )   ).
                  tl_bseg_aux_2  = VALUE  #(
                              BASE tl_bseg_aux_2          (   wa_bseg  )   ).
                ENDIF.
                tl_abono_con_extracto = tl_bseg_aux.
              ENDIF.
              SORT tl_bseg_aux_2 BY bukrs belnr gjahr hkont.
*           DELETE ADJACENT DUPLICATES FROM tl_bseg_aux_2 COMPARING bukrs belnr gjahr hkont.
            ENDIF.
          ENDIF.
          "Fin de la validacion escenario 3
          "Validacion escenario 4
          SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
                   kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
                   augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt,linetype "fecha_extracto,doc_extracto,
                   "refe_pago,importe_pago,texto_banco
              INTO TABLE @DATA(tl_acdoca4_v1)
              FROM acdoca
              FOR ALL ENTRIES IN @tl_acdoca2
              WHERE rldnr    = @tl_acdoca2-rldnr  AND
                    rbukrs   = @tl_acdoca2-rbukrs AND
                    gjahr    = @tl_acdoca2-gjahr  AND
                    ( blart  IN @rg_compro_gasto OR
                      blart  IN @rg_compro_gasto_high ) AND
                    belnr  = @tl_acdoca2-belnr
                AND bschl IN @rg_cargo_abono_compro_low    "Insert AMP 28.12.2020
                AND ktosl IN @rg_z_caso_ktosl_004_low.     "Insert AMP 28.12.2020
          "Fin de la validacion escenario 4
          IF  p_pag_c IS NOT INITIAL.
            DELETE tl_bseg WHERE augdt IS INITIAL.
          ENDIF.
* se busca todo el detalle de la factura del acreedor

          SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
                 kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
                 augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt,linetype "fecha_extracto,doc_extracto,
                 , zuonr"refe_pago,importe_pago,texto_banco
            INTO TABLE @DATA(tl_acdoca3) "detalle de facturas acreedor
            FROM acdoca
            FOR ALL ENTRIES IN @tl_acdoca2 "pago posición acreedor
            WHERE rldnr  = '0L'               AND
                  rbukrs = @tl_acdoca2-rbukrs AND
                  gjahr  = @tl_acdoca2-gjahr  AND
                  belnr  = @tl_acdoca2-belnr.
          IF sy-subrc = 0.
*          SORT tl_acdoca3 BY docln.
            SORT tl_acdoca3 BY rldnr rbukrs gjahr belnr.
*Detalle de facturas de acreedor  clase docto RE

            DATA(tl_acdoca1_base_noobj)  =  tl_acdoca3.
            DATA(tl_ctas_factura)        =  tl_acdoca3.
            DATA(tl_acdoca1_base_iva_16) =  tl_acdoca3.
            DATA(tl_acdoca1_base_iva_8)  =  tl_acdoca3.
            DATA(tl_acdoca1_base_nodedu) =  tl_acdoca3.

            DATA(tl_acdoca4) = tl_acdoca3.
            SORT tl_acdoca4 BY rldnr rbukrs gjahr belnr docln. " se ordenan
            DATA(tl_acdoca4_escenario4)    = tl_acdoca3.
            DATA(tl_acdoca4_imp_fac)        = tl_acdoca3.
            DATA(tl_acdoca4_egreso)        = tl_acdoca3.
            DATA(tl_acdoca5_ret_iva_6)     = tl_acdoca3. "subcontra
            DATA(tl_acdoca5_ret_va_4)      = tl_acdoca3. "fletes
            DATA(tl_acdoca_escenario_5)    = tl_acdoca3.
            DATA(tl_acdoca_iva_escenario6) = tl_acdoca3.
            DATA(tl_acdoca_ret_escenario6) = tl_acdoca3.
            DATA(tl_acdoca_ret_escenario7) = tl_acdoca3.
            DATA(tl_acdoca_ret_cedular)    = tl_acdoca3.
            DATA(tl_acdoca_ret_isr_pagext) = tl_acdoca3.
            DATA(tl_acdoca_iva_escenario7) = tl_acdoca3.
            DATA(tl_acdoca_iva_8)          = tl_acdoca3.


            DELETE tl_acdoca4_escenario4    WHERE  blart NOT IN rg_acreedores_movto_high.
            DELETE tl_acdoca4_escenario4    WHERE  racct IN rg_ctas_saldo_cero. "no se permiten cuentas de cuadre
            DATA(tl_acdoca4_no_ob)  = tl_acdoca4_escenario4.
            DELETE tl_acdoca4_no_ob WHERE linetype IN rg_acreedores_movto.
            DELETE tl_acdoca4_escenario4    WHERE  linetype NOT IN rg_acreedores_movto.

            SORT tl_acdoca4_escenario4 BY rldnr rbukrs gjahr belnr docln ASCENDING.
*--> Insert AMP 06.01.2021
*          DELETE tl_acdoca4_escenario4 WHERE NOT ktosl eq 'HRT'.
*Valida escenario comprobación de gastos.

            LOOP AT tl_acdoca4_escenario4 INTO DATA(wa_acdoca4_escenario4) WHERE bschl EQ '21'
                                                                             AND blart IN rg_acreedores_movto_high.
              DATA(lv_tsl4) = wa_acdoca4_escenario4-tsl * -1.
              LOOP AT tl_acdoca4_escenario4 INTO DATA(wa_acdoca4_escenario4_aux) WHERE bschl EQ '31'
                                                                                   AND tsl EQ lv_tsl4
                                                                                   AND belnr EQ wa_acdoca4_escenario4-belnr
                                                                                   AND ktosl EQ 'HRT'.
                DELETE tl_acdoca4_escenario4.
                EXIT.
              ENDLOOP.
              DELETE tl_acdoca4_escenario4.
            ENDLOOP.


*<-- End insert AMP
            "Recuperamos el RFC escenario 4
            IF tl_acdoca4_escenario4 IS NOT INITIAL.
              SELECT bukrs,belnr,gjahr,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
                 INTO TABLE @DATA(tl_bseg_escenario4)
                 FROM bseg
                 FOR ALL ENTRIES IN @tl_acdoca4_escenario4
                 WHERE bukrs =  @tl_acdoca4_escenario4-rbukrs AND
                       belnr =  @tl_acdoca4_escenario4-belnr  AND
                       gjahr =  @tl_acdoca4_escenario4-gjahr  AND
                       hkont = @tl_acdoca4_escenario4-gkont.
              SELECT ktopl, saknr,txt50
               INTO TABLE @DATA(tl_skat_4)
               FROM skat
               FOR ALL ENTRIES IN @tl_acdoca4_escenario4
               WHERE spras = @sy-langu                    AND
                     ktopl = @tl_acdoca4_escenario4-ktopl AND
                     saknr = @tl_acdoca4_escenario4-racct.
            ENDIF.

            DELETE tl_acdoca3  WHERE  rcntr IS INITIAL AND racct NOT IN rg_z_ctas_si AND blart EQ c_blart.
            "Sumamos los centros de coste
            DATA(tl_acdoca3_aux) = tl_acdoca3.

            DELETE  tl_acdoca3_aux       WHERE  racct IN rg_ctas_saldo_cero. "no se permiten cuentas de cuadre

            DELETE  tl_acdoca4_egreso    WHERE  racct IN rg_ctas_saldo_cero. "no se permiten cuentas de cuadre 12323
            DELETE  tl_acdoca4_egreso    WHERE linetype NOT IN  rg_clave_egreso.

            SORT    tl_acdoca4_egreso       BY belnr docln.

*          IF wa_acdoca-blart = 'RE'. CLEAR wa_acdoca. ENDIF.

            REFRESH: tl_acdoca3.

            LOOP AT  tl_acdoca3_aux ASSIGNING <fs_acdoca>.
              IF sy-tabix = 1.
                wa_acdoca = <fs_acdoca>.
              ELSE.
                IF <fs_acdoca>-belnr = wa_acdoca-belnr AND
                   <fs_acdoca>-rcntr = wa_acdoca-rcntr.
                  wa_acdoca-hsl = wa_acdoca-hsl + <fs_acdoca>-hsl.
                ELSE.
                  tl_acdoca3  = VALUE  #(
                          BASE tl_acdoca3          ( wa_acdoca )   ).
                  wa_acdoca = <fs_acdoca>.
                ENDIF.
              ENDIF.
            ENDLOOP.
            tl_acdoca3  = VALUE  #(
                         BASE tl_acdoca3          ( wa_acdoca )   ).

            DELETE tl_acdoca5_ret_iva_6      WHERE  racct NOT IN  rg_zret_iva_ret_sub_contra.

            DELETE tl_acdoca1_base_noobj    WHERE  bschl EQ 31.
            DELETE tl_acdoca1_base_noobj    WHERE  mwskz NE  ''.
            DELETE tl_acdoca1_base_noobj    WHERE  ktosl EQ  'WIT'.
            DELETE tl_acdoca1_base_noobj    WHERE  racct IN rg_ctas_saldo_cero. "no se permiten cuentas de cuadre


            DELETE tl_acdoca1_base_nodedu    WHERE  bschl EQ 31.
            DELETE tl_acdoca1_base_nodedu    WHERE  mwskz NE  'W8'.
            DELETE tl_acdoca1_base_nodedu    WHERE  ktosl EQ  'WIT'.
            DELETE tl_acdoca1_base_nodedu    WHERE  racct IN rg_ctas_saldo_cero. "no se permiten cuentas de cuadre

            DELETE  tl_ctas_factura         WHERE  bschl EQ 31.
            DELETE  tl_ctas_factura         WHERE  ktosl EQ 'WIT'.
            DELETE  tl_ctas_factura         WHERE  racct IN rg_ctas_saldo_cero.
            DELETE  tl_ctas_factura         WHERE  blart  NOT IN rg_z_fondo_fijo.. "no se permiten cuentas de cuadre
            DELETE  tl_ctas_factura         WHERE  ktosl  = 'VST'.

            DELETE tl_acdoca1_base_iva_8    WHERE  bschl EQ 31.
            DELETE tl_acdoca1_base_iva_8    WHERE  mwskz NE  c_w3.
            DELETE tl_acdoca1_base_iva_8    WHERE  racct IN rg_ctas_saldo_cero. "no se permiten cuentas de cuadre
            DELETE tl_acdoca1_base_iva_8    WHERE  ktosl EQ 'WIT'.


            SORT tl_acdoca1_base_iva_8      BY rbukrs gjahr belnr  ryear docln ASCENDING.

            DELETE tl_acdoca1_base_iva_16    WHERE  mwskz NE c_w1.
            DELETE tl_acdoca1_base_iva_16    WHERE  racct IN rg_ziva_16."solo queda la las cuentas de gasto
            DELETE tl_acdoca1_base_iva_16    WHERE  racct IN rg_ctas_saldo_cero. "no se permiten cuentas de cuadre
            DELETE tl_acdoca1_base_iva_16    WHERE  bschl EQ 31.
            DELETE tl_acdoca1_base_iva_16     WHERE  bschl EQ 21.
            tl_acdoca4_egreso = tl_acdoca1_base_iva_16.

            SORT tl_acdoca1_base_iva_16      BY rbukrs gjahr belnr  ryear docln ASCENDING.

            DELETE tl_acdoca5_ret_va_4       WHERE  racct NOT IN rg_zret_iva_ret_sub_fletes.

            DELETE tl_acdoca_iva_escenario6  WHERE  racct NOT IN rg_zret_iva_ret_sub_honorarios.
            DELETE tl_acdoca_ret_escenario6  WHERE  racct NOT IN rg_zret_ret_isr_honorarios.
            DELETE tl_acdoca_ret_escenario7  WHERE  racct NOT IN rg_zret_ret_isr_arrendamiento.
            DELETE tl_acdoca_iva_escenario7  WHERE  racct NOT IN rg_zret_iva_ret_sub_arren.
*          SORT tl_acdoca_iva_8 BY racct mwskz.
            DELETE tl_acdoca_iva_8           WHERE  racct NOT IN rg_ziva_8_low.
            DELETE tl_acdoca_iva_8           WHERE  mwskz NOT IN rg_ziva_8_high.

            "validamos si hay escenario 5
            DELETE tl_acdoca_escenario_5    WHERE  mwskz NOT IN rg_ziva_0.
            DELETE tl_acdoca_escenario_5    WHERE  bschl EQ 31.
            DELETE tl_acdoca_ret_cedular    WHERE racct NOT IN  rg_imp_ret_cedular.
            DELETE tl_acdoca_ret_isr_pagext  WHERE racct NOT IN  rg_ret_isr_pagext.


            IF tl_acdoca_escenario_5 IS NOT INITIAL.
              SELECT ktopl, saknr,txt50
                             INTO TABLE @DATA(tl_skat)
                             FROM skat
                             FOR ALL ENTRIES IN @tl_acdoca_escenario_5
                             WHERE spras = @sy-langu                    AND
                                   ktopl = @tl_acdoca_escenario_5-ktopl AND
                                   saknr = @tl_acdoca_escenario_5-racct.
              "Recuperamos el RFC escenario 5

              SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
                 INTO TABLE @DATA(tl_bseg_escenario5)
                 FROM bseg
                 FOR ALL ENTRIES IN @tl_acdoca_escenario_5
                 WHERE bukrs =  @tl_acdoca_escenario_5-rbukrs AND
                       belnr =  @tl_acdoca_escenario_5-belnr  AND
                       gjahr =  @tl_acdoca_escenario_5-gjahr.

            ENDIF.
            SORT tl_acdoca3 BY rldnr rbukrs gjahr belnr.
            DELETE tl_acdoca4 WHERE racct NOT IN rg_ziva_16.

          ENDIF.
          SELECT ktopl,saknr,txt50
            INTO TABLE @DATA(tl_skat2)
            FROM skat
            FOR ALL ENTRIES IN @tl_acdoca3
            WHERE spras = @sy-langu         AND
                  ktopl = @tl_acdoca3-kokrs AND
                  saknr = @tl_acdoca3-racct.


        ENDIF.
      ENDIF.

      SORT tl_acdoca4_v1 BY rbukrs gjahr belnr.
      SORT tl_acdoca_iva_escenario7 BY rbukrs gjahr belnr.
      SORT tl_bseg_escenario10 BY bukrs gjahr belnr.
      LOOP AT tl_bseg ASSIGNING <fs_bseg>.
        CLEAR: sl_report.
        "Semaforo
        IF p_pag_c IS NOT INITIAL.
          sl_report-semaforo = '@08@'.

          SPLIT <fs_bseg>-sgtxt AT '-' INTO DATA(vl_string) DATA(vl_string2).
          sl_report-referencia  = vl_string.
          sl_report-concepto    = vl_string2.
        ELSE.
          sl_report-semaforo = '@09@'.

        ENDIF.

        ASSIGN tl_bkpf[ bukrs = <fs_bseg>-bukrs belnr = <fs_bseg>-belnr gjahr = <fs_bseg>-gjahr ] TO FIELD-SYMBOL(<fs_bkpf>).
        IF sy-subrc = 0.
          ASSIGN tl_acdoca[ rbukrs = <fs_bkpf>-bukrs belnr = <fs_bkpf>-belnr gjahr = <fs_bkpf>-gjahr ] TO <fs_acdoca>.
          IF sy-subrc = 0.
            sl_report-fechapago            = <fs_acdoca>-augdt.
            sl_report-n_doc_pago           = <fs_acdoca>-belnr.
            sl_report-n_doc_pago_sociedad  = <fs_acdoca>-rbukrs.
            sl_report-n_doc_pago_ejercicio = <fs_acdoca>-gjahr.
            "Sociedad
            sl_report-bukrs = <fs_acdoca>-rbukrs.
            "periodo
            sl_report-periodo =  <fs_acdoca>-augdt+4(2).
            "ejercicio
            sl_report-gjahr   =   <fs_acdoca>-gjahr.
            IF p_pag_s IS NOT INITIAL.
              sl_report-referencia = <fs_bkpf>-awkey.
              sl_report-concepto   = <fs_bkpf>-bktxt.
            ENDIF.
            "RFC
            ASSIGN tl_dfkkbptaxnum[ partner = <fs_acdoca>-lifnr ] TO FIELD-SYMBOL(<fs_dfkkbptaxnum>).
            IF sy-subrc = 0.
              IF <fs_dfkkbptaxnum>-taxnumxl IS NOT INITIAL.
                sl_report-rfc = <fs_dfkkbptaxnum>-taxnumxl.
              ELSE.
                sl_report-rfc = <fs_dfkkbptaxnum>-taxnum.
              ENDIF.
              TRANSLATE sl_report-rfc TO UPPER CASE.
            ENDIF.
            "RAZON SOCIAL
            ASSIGN tl_but000[ partner = <fs_acdoca>-lifnr ] TO FIELD-SYMBOL(<fs_but000>).
            IF sy-subrc = 0.
              IF <fs_but000>-natpers IS INITIAL.
                CONCATENATE <fs_but000>-name_org1 <fs_but000>-name_org2 <fs_but000>-name_org3 <fs_but000>-name_org4 INTO sl_report-razon_s SEPARATED BY space.
              ELSE.
                CONCATENATE <fs_but000>-name_last <fs_but000>-name_first INTO sl_report-razon_s SEPARATED BY space.
              ENDIF.
            ENDIF.
            "Acredor
            sl_report-acredor = <fs_acdoca>-lifnr.
          ENDIF.
        ENDIF.
        IF p_pag_c IS NOT INITIAL.
          ASSIGN tl_bseg[ bukrs = <fs_bseg>-bukrs belnr = <fs_bseg>-augbl gjahr = <fs_bseg>-gjahr ] TO FIELD-SYMBOL(<fs_bseg_2>).
          IF sy-subrc = 0.

            sl_report-descripcion = <fs_bseg_2>-sgtxt.
            sl_report-totalcargos = <fs_bseg>-dmbtr.

          ENDIF.
        ELSE.
          ASSIGN tl_bseg_sin_e[ bukrs = <fs_bseg>-bukrs belnr = <fs_bseg>-belnr gjahr = <fs_bseg>-gjahr ] TO FIELD-SYMBOL(<fs_bseg_sin_e>).
          IF sy-subrc = 0.
            ASSIGN tl_t042z[ zlsch = <fs_bseg_sin_e>-zlsch ] TO FIELD-SYMBOL(<fs_t042z>).
            IF sy-subrc = 0.
              sl_report-descripcion = <fs_t042z>-text1.
            ENDIF.
          ENDIF.
        ENDIF.
        DATA(sl_report_aux) = sl_report.
        CLEAR: vl_cont.

        ASSIGN tl_acdoca2[ rbukrs = <fs_bseg>-bukrs augbl = <fs_bseg>-belnr gjahr = <fs_bseg>-gjahr ] TO FIELD-SYMBOL(<fs_acdoca2>).
        IF sy-subrc = 0.
          lv_pago_procesado = <fs_bseg>-belnr. " CARGA EL DOCTO DE FACTURA A PROCESAR DEBE SER UNICO
          LOOP AT tl_acdoca2 ASSIGNING <fs_acdoca2> FROM sy-tabix. " lee la tabla de pagos compensados vs acreedor
            IF <fs_acdoca2>-rbukrs NE <fs_bseg>-bukrs OR
               <fs_acdoca2>-augbl  NE <fs_bseg>-belnr OR
               <fs_acdoca2>-gjahr  NE <fs_bseg>-gjahr.
              EXIT.
            ENDIF.
            ADD 1 TO vl_cont.
            sl_report = sl_report_aux.

            sl_report-n_documento_fac           = <fs_acdoca2>-belnr.
            sl_report-n_documento_fac_sociedad  = <fs_acdoca2>-rbukrs.
            sl_report-n_documento_fac_ejercicio = <fs_acdoca2>-gjahr.
* no debe metese aqui por que no es su escenario
            ASSIGN tl_acdoca3[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr ] TO FIELD-SYMBOL(<fs_acdoca3>).
            IF sy-subrc = 0  AND <fs_acdoca3>-blart         NOT IN rg_z_fondo_fijo.
              DATA(vl_tabix) = sy-tabix.
              IF ( <fs_acdoca3>-ktosl IN rg_z_caso_ktosl_001 AND <fs_acdoca3>-rcntr IS NOT INITIAL ) OR ( <fs_acdoca3>-ktosl IN rg_z_caso_ktosl_004 AND <fs_acdoca3>-rcntr IS NOT INITIAL ) .
                ASSIGN tl_skat2[ ktopl = <fs_acdoca3>-kokrs saknr = <fs_acdoca3>-racct ] TO FIELD-SYMBOL(<fs_skat>).
                IF sy-subrc = 0.
                  "Clasificación
                  sl_report-clasificacion = <fs_skat>-txt50.
                ENDIF.
              ENDIF.

              ASSIGN tl_skat2[ ktopl = <fs_acdoca3>-kokrs saknr = <fs_acdoca3>-racct ] TO FIELD-SYMBOL(<fs_skat2>).
              IF sy-subrc = 0.
                "Clasificación
                sl_report-clasificacion = <fs_skat2>-txt50.
              ENDIF.
              IF <fs_acdoca3>-racct NOT IN rg_z_ctas_si."Valida para escenario 1
                IF <fs_acdoca3>-mwskz IN rg_ziva_16_high.
                  IF  <fs_acdoca3>-hsl LT 0.  <fs_acdoca3>-hsl =  <fs_acdoca3>-hsl * -1. ENDIF.
                  sl_report-base_16    = <fs_acdoca3>-hsl.
                ENDIF.

                IF <fs_acdoca3>-mwskz IS INITIAL AND <fs_acdoca3>-ktosl NE 'WIT'.
                  IF <fs_acdoca3>-tsl LT 0.
                    <fs_acdoca3>-tsl = <fs_acdoca3>-tsl * -1.
                  ENDIF.
                  sl_report-base_no_obj    = <fs_acdoca3>-tsl.
                ENDIF.
                IF  <fs_acdoca3>-mwskz   IN rg_ziva_nd.
                  IF <fs_acdoca3>-tsl LT 0.
                    <fs_acdoca3>-tsl = <fs_acdoca3>-tsl * -1.
                  ENDIF.
                  sl_report-base_exento   = <fs_acdoca3>-hsl.
                ENDIF.
                sl_report-total = <fs_acdoca3>-hsl.
                ASSIGN tl_acdoca4_escenario4[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr ] TO FIELD-SYMBOL(<fs_acdoca4_escenario4>).
                IF sy-subrc = 0.
                  sl_report-foliofiscal = <fs_acdoca3>-sgtxt.

                ELSE.
                  "Folio fiscal
                  CONCATENATE <fs_acdoca2>-rbukrs <fs_acdoca2>-belnr <fs_acdoca2>-gjahr INTO vl_name.
*                CALL FUNCTION 'READ_TEXT'
*                  EXPORTING
*                    client                  = sy-mandt
*                    id                      = 'YUUD'
*                    language                = 'S'
*                    name                    = vl_name
*                    object                  = 'BELEG'
*                  TABLES
*                    lines                   = tl_lines
*                  EXCEPTIONS
*                    id                      = 1
*                    language                = 2
*                    name                    = 3
*                    not_found               = 4
*                    object                  = 5
*                    reference_check         = 6
*                    wrong_access_to_archive = 7
*                    OTHERS                  = 8.

                  CALL FUNCTION 'ZFI_ISR_READ_TEXT_BUFFER'
                    EXPORTING
*                     CLIENT                  = SY-MANDT
                      ID                      = 'YUUD'
                      LANGUAGE                = 'S'
                      NAME                    = vl_name
                      OBJECT                  = 'BELEG'
*                     ARCHIVE_HANDLE          = 0
*                     LOCAL_CAT               = ' '
*             IMPORTING
*                     HEADER                  =
*                     OLD_LINE_COUNTER        =
                    TABLES
                      LINES                   = tl_lines
                    EXCEPTIONS
                      ID                      = 1
                      LANGUAGE                = 2
                      NAME                    = 3
                      NOT_FOUND               = 4
                      OBJECT                  = 5
                      REFERENCE_CHECK         = 6
                      WRONG_ACCESS_TO_ARCHIVE = 7
                      OTHERS                  = 8.

                  IF sy-subrc = 0.
                    ASSIGN tl_lines[ 1 ] TO FIELD-SYMBOL(<fs_lines>).
                    IF sy-subrc = 0.
                      sl_report-foliofiscal = <fs_lines>-tdline.
                    ENDIF.
                  ENDIF.
                ENDIF.
                "Escenario 2
              ELSE.
                LOOP AT tl_acdoca3 ASSIGNING <fs_acdoca3> FROM vl_tabix.
                  IF <fs_acdoca3>-rbukrs NE <fs_acdoca2>-rbukrs OR <fs_acdoca3>-gjahr NE <fs_acdoca2>-gjahr OR <fs_acdoca3>-belnr NE <fs_acdoca2>-belnr.
                    EXIT.
                  ENDIF.
                  sl_report-base_16 = sl_report-base_16 + <fs_acdoca3>-hsl.
                  sl_report-total   = sl_report-total   + <fs_acdoca3>-hsl.
                ENDLOOP.
                "Folio Fiscal
                sl_report-foliofiscal = <fs_acdoca2>-sgtxt.
              ENDIF.
            ELSE."escenario 3

*            sl_report-total          = sl_report-total   + ( <fs_acdoca2>-hsl * -1 ).
*            sl_report-base_no_obj    = sl_report-base_no_obj   + ( <fs_acdoca2>-hsl * -1 ).
            ENDIF.
            "Escenario 3
            IF tl_cargo_con_extracto IS NOT INITIAL OR tl_abono_con_extracto IS NOT INITIAL AND <fs_acdoca2>-sgtxt IS NOT INITIAL.
              CALL FUNCTION 'NUMERIC_CHECK'
                EXPORTING
*                 string_in = <fs_acdoca2>-sgtxt
                  string_in = <fs_acdoca2>-zuonr   "Insert AMP 17.02.2021  Se cambia al campo ZUONR.
                IMPORTING
*                 string_out =
                  htype     = vl_htype
                EXCEPTIONS
                  OTHERS    = 1.
              IF vl_htype EQ 'NUMC'.
**              vl_docu_ejercicio = <fs_acdoca2>-sgtxt.
                vl_docu_ejercicio = <fs_acdoca2>-zuonr.   "Insert AMP 17.02.2021  Se cambia al campo ZUONR.
                vl_docu_ejercicio = |{ vl_docu_ejercicio ALPHA = IN }|.
                SORT tl_bseg_aux_2 BY belnr bukrs gjahr.
                ASSIGN tl_bseg_aux_2[ belnr = vl_docu_ejercicio(10) bukrs =  <fs_bseg>-bukrs
                                             gjahr = vl_docu_ejercicio+10(4) ] TO FIELD-SYMBOL(<fs_bseg_aux_2>).
                IF sy-subrc = 0.
                  LOOP AT tl_bseg_aux_2 ASSIGNING <fs_bseg_aux_2> FROM sy-tabix.
                    CLEAR:vl_total.
                    IF vl_docu_ejercicio(10) NE <fs_bseg_aux_2>-belnr OR
                      <fs_bseg>-bukrs        NE <fs_bseg_aux_2>-bukrs OR
                     vl_docu_ejercicio+10(4) NE <fs_bseg_aux_2>-gjahr.
                      EXIT.
                    ENDIF.
                    SORT tl_cargo_con_extracto BY bukrs belnr gjahr hkont.
                    ASSIGN tl_cargo_con_extracto[ belnr = <fs_bseg_aux_2>-belnr bukrs =  <fs_bseg_aux_2>-bukrs
                                                  gjahr = <fs_bseg_aux_2>-gjahr hkont = <fs_bseg_aux_2>-hkont
                                                  buzei = <fs_bseg_aux_2>-buzei ] TO FIELD-SYMBOL(<fs_cargo_c_extra>).
                    IF sy-subrc = 0.
                      " se hace un read tabla index nada mas
                      LOOP AT tl_cargo_con_extracto ASSIGNING <fs_cargo_c_extra> FROM sy-tabix.
                        "      IF <fs_cargo_c_extra>-belnr NE <fs_bseg_aux_2>-belnr OR <fs_cargo_c_extra>-bukrs NE <fs_bseg_aux_2>-bukrs OR
                        "         <fs_cargo_c_extra>-gjahr NE <fs_bseg_aux_2>-gjahr OR <fs_cargo_c_extra>-hkont NE <fs_bseg_aux_2>-hkont.
                        IF sy-subrc = 0.
                          vl_total =  vl_total + <fs_cargo_c_extra>-dmbtr.
                          EXIT."sale por que busca otro registro
                        ENDIF.
                        "    ENDIF.
                      ENDLOOP.
                    ENDIF.
                    ASSIGN tl_abono_con_extracto[ belnr = <fs_bseg_aux_2>-belnr bukrs = <fs_bseg_aux_2>-bukrs
                                                  gjahr = <fs_bseg_aux_2>-gjahr hkont = <fs_bseg_aux_2>-hkont
                                                  buzei = <fs_bseg_aux_2>-buzei ] TO FIELD-SYMBOL(<fs_abono_c_extra>).
                    IF sy-subrc = 0.
                      LOOP AT tl_abono_con_extracto ASSIGNING <fs_abono_c_extra> FROM sy-tabix.
                        IF sy-subrc = 0.
*                      IF <fs_abono_c_extra>-belnr NE <fs_bseg_aux_2>-belnr OR <fs_abono_c_extra>-bukrs NE <fs_bseg_aux_2>-bukrs OR
*                        <fs_abono_c_extra>-gjahr NE <fs_bseg_aux_2>-gjahr OR <fs_abono_c_extra>-hkont NE <fs_bseg_aux_2>-hkont.
                          vl_total =  vl_total - <fs_abono_c_extra>-dmbtr.
                          EXIT."sale por que busca otro registro
                        ENDIF.
                      ENDLOOP.
                    ENDIF.
                    ASSIGN rg_resumen_nomina[ low = <fs_bseg_aux_2>-hkont ] TO FIELD-SYMBOL(<fs_resumen_n>).
                    IF sy-subrc = 0.
                      IF <fs_resumen_n>-high = 'SUBSIDIO AL EMPLEO'.
                        sl_report-subsidio_empleo = sl_report-subsidio_empleo + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'FONDO AHORRO EMPRESA'.
                        sl_report-fondo_ahorro_empresa = sl_report-fondo_ahorro_empresa + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'FONDO AHORRO EMPLEAD'.
                        sl_report-fondo_ahorro_emplead = sl_report-fondo_ahorro_emplead + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'DESCUENTO INFONACOT'.
                        sl_report-descuento_infonacot =  sl_report-descuento_infonacot + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'SUELDOS POR PAGAR'.
                        IF <fs_resumen_n>-high = wa_ctaproc-high.
                          sl_report-sueldos_por_pagar = 0.
                        ELSE.
                          sl_report-sueldos_por_pagar  =    sl_report-sueldos_por_pagar  + vl_total.
                        ENDIF.
                      ENDIF.


                      IF <fs_resumen_n>-high = 'IMSS PATRONAL'.
                        sl_report-imss_patronal = sl_report-imss_patronal + vl_total.
                      ENDIF.

                      IF <fs_resumen_n>-high = 'VIATICOS'.
                        sl_report-viaticos = sl_report-viaticos + vl_total.
                      ENDIF.

                      IF <fs_resumen_n>-high = '2% SAR'.
                        sl_report-2_sar = sl_report-2_sar + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'INFONAVIT PATRONAL'.
                        sl_report-infonavit_patronal = sl_report-infonavit_patronal + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'ING X DEV EMPLEADOS'.
                        sl_report-ing_x_dev_empleados =  sl_report-ing_x_dev_empleados + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'ING X DESCTO COMEDOR'.
                        sl_report-ing_x_descto_comedor =  sl_report-ing_x_descto_comedor + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'IMPULSO ECONOMICO CREA'.
                        sl_report-impulso_economico_crea = sl_report-impulso_economico_crea + vl_total.
                      ENDIF.

                      IF <fs_resumen_n>-high = 'CASETAS'.
                        sl_report-casetas = sl_report-casetas + vl_total.
                      ENDIF.

                      IF <fs_resumen_n>-high = 'CESANTÍA VEJEZ PATRO'.
                        sl_report-cesantia_vejez_patro = sl_report-cesantia_vejez_patro + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'ISR RET X SUELDOS'.
                        sl_report-isr_ret_x_sueldos = sl_report-isr_ret_x_sueldos + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'OTRAS PRESTACIONES'.
                        sl_report-otras_prestaciones = sl_report-otras_prestaciones + vl_total.
                      ENDIF.

                      IF <fs_resumen_n>-high = 'OTROS GASTOS VIAJE  '.
                        sl_report-otros_gastos_viaje = sl_report-otros_gastos_viaje + vl_total.
                      ENDIF.

                      IF <fs_resumen_n>-high = 'IMSS RETENIDO'.
                        sl_report-imss_retenido = sl_report-imss_retenido + vl_total.
                      ENDIF.

                      IF <fs_resumen_n>-high = 'CESANTÍA VEJEZ EMPL'.
                        sl_report-cesantia_vejez_empl = sl_report-cesantia_vejez_empl + vl_total.
                      ENDIF.

                      IF <fs_resumen_n>-high = 'FAC ADMVAS-MANIOBRAS'.
                        sl_report-fac_admvas_maniobras = sl_report-fac_admvas_maniobras  + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'FAC ADMVAS-GTS CAMINO'.
                        sl_report-fac_admvas_gts_camino  = sl_report-fac_admvas_gts_camino   + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'FAC ADMVAS-TALACHAS'.
                        sl_report-fac_admvas_gts_talachas  = sl_report-fac_admvas_gts_talachas   + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'FAC ADMVAS-REP MEN'.
                        sl_report-fac_admvas_gts_rep_men  = sl_report-fac_admvas_gts_rep_men   + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'FAC ADMVAS-FIANZAS'.
                        sl_report-fac_admvas_gts_fianzas   = sl_report-fac_admvas_gts_fianzas   + vl_total.
                      ENDIF.

                      IF <fs_resumen_n>-high = 'FINIQUITO POR PAGAR'.
                        IF <fs_resumen_n>-high = wa_ctaproc-high.
                          sl_report-finiquito_por_pagar = 0.
                        ELSE.
                          sl_report-finiquito_por_pagar = sl_report-finiquito_por_pagar + vl_total.
                        ENDIF.
                      ENDIF.

                      IF <fs_resumen_n>-high = 'INFONAVIT RETENIDO'.
                        sl_report-infonavit_retenido = sl_report-infonavit_retenido + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'PROV AGUI X PAGAR'.
                        sl_report-prov_agui_x_pagar = sl_report-prov_agui_x_pagar + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'SUELDOS'.
                        sl_report-sueldos = sl_report-sueldos  + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'TIEMPO EXTRA DOBLE'.
                        sl_report-tiempo_extra_doble = sl_report-tiempo_extra_doble + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'TIEMPO EXTRA TRIPLE'.
                        sl_report-tiempo_extra_triple = sl_report-tiempo_extra_triple + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'DOMINGOS Y DÍAS FEST'.
                        sl_report-domingos_dias_fest = sl_report-domingos_dias_fest + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'PRIMA DOMINICAL'.
                        sl_report-prima_dominical = sl_report-prima_dominical + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'COMPENS EXTRAORDINAR'.
                        sl_report-compens_extraordinar = sl_report-compens_extraordinar + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'PREMIO X PUNTU ASIST'.
                        sl_report-premio_puntualidad = sl_report-premio_puntualidad + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'CUOTA PATRONAL IMSS'.
                        sl_report-cuota_patronal_imss = sl_report-cuota_patronal_imss + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'FONDO DE AHORRO'.
                        sl_report-fondo_ahorro = sl_report-fondo_ahorro + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = '5% INFONAVIT'.
                        sl_report-5_infonavit = sl_report-5_infonavit + vl_total.
                      ENDIF.

*--> Insert AMP 18.02.2021
                      IF <fs_resumen_n>-high = 'PRIMA VACACIONAL'.
                        sl_report-prima_vacacional = sl_report-prima_vacacional + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'CUOTA SINDICAL X PAG TOTAL'.
                        sl_report-cuota_sind_x_pagotot = sl_report-cuota_sind_x_pagotot + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'PENSIÓN ALIMENTICIA'.
                        sl_report-pension_alim_total = sl_report-pension_alim_total + vl_total.
                      ENDIF.
                      IF <fs_resumen_n>-high = 'SEGURO VIV INFONAVIT'.
                        sl_report-seguro_viv_infon_tot = sl_report-seguro_viv_infon_tot + vl_total.
                      ENDIF.
*<-- End insert AMP 18.02.2021
                    ENDIF.
                  ENDLOOP.
                ENDIF.

                sl_report-n_sol_anticipo = vl_docu_ejercicio(10).
                sl_report-n_sol_anticipo_ejercicio = vl_docu_ejercicio+10(4).
                sl_report-n_sol_anticipo_sociedad = <fs_bseg>-bukrs.
              ENDIF.
            ENDIF.


            CLEAR sl_report-total.


* base no obj
            DATA wa_skat TYPE skat.
* procesamos el tipo RE la base no objeto
            ASSIGN tl_acdoca1_base_noobj[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr drcrk = c_s   mwskz = ''  ] TO FIELD-SYMBOL(<fs_acdocabnobj>).
            IF sy-subrc = 0.
              CLEAR: sl_report-base_no_obj.
              LOOP AT tl_acdoca1_base_noobj ASSIGNING <fs_acdocabnobj> FROM sy-tabix.
                IF <fs_acdocabnobj>-rbukrs NE <fs_acdoca2>-rbukrs OR
                   <fs_acdocabnobj>-gjahr  NE <fs_acdoca2>-gjahr  OR
                   <fs_acdocabnobj>-belnr  NE <fs_acdoca2>-belnr.
                  EXIT.
                ENDIF.

                IF <fs_acdocabnobj>-mwskz  = ''.
                  sl_report-base_no_obj   = sl_report-base_0 + <fs_acdocabnobj>-hsl.
                  SELECT SINGLE ktopl, saknr,txt50
                  FROM skat
                  WHERE spras = @sy-langu                    AND
                        ktopl = @<fs_acdocabnobj>-ktopl  AND
                        saknr = @<fs_acdocabnobj>-racct INTO CORRESPONDING FIELDS OF @wa_skat.
                  IF sy-subrc = 0.
                    "Clasificación
                    sl_report-clasificacion = wa_skat-txt50.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              IF  sl_report-base_no_obj  LT 0.
                sl_report-base_no_obj   =  sl_report-base_no_obj   * -1.
              ENDIF.

            ENDIF.


            IF  sl_report-base_no_obj  LT 0.
              sl_report-total =  sl_report-total +  sl_report-base_no_obj * -1.
            ELSE.
              sl_report-total =  sl_report-total +  sl_report-base_no_obj .
            ENDIF.

* procesamos el tipo RE la base gravable del 0%
            "          ASSIGN tl_acdoca1_base_0[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr drcrk = c_s     mwskz =  c_w0  ] TO FIELD-SYMBOL(<fs_acdocabiva0>).
            "          IF sy-subrc = 0.
            "            CLEAR: sl_report-base_0, sl_report-base_16.
            "            LOOP AT tl_acdoca1_base_iva_16 ASSIGNING <fs_acdocabiva0> FROM sy-tabix..
            "              IF <fs_acdocabiva0>-rbukrs NE <fs_acdoca2>-rbukrs OR
            "                 <fs_acdocabiva0>-gjahr  NE <fs_acdoca2>-gjahr  OR
            "                 <fs_acdocabiva0>-belnr  NE <fs_acdoca2>-belnr.
            "                EXIT.
            "              ENDIF.
            "              IF <fs_acdocabiva0>-mwskz  = c_w0.
            "                sl_report-base_0    = sl_report-base_0 + <fs_acdocabiva0>-hsl.
            "                SELECT SINGLE ktopl, saknr,txt50
            "            FROM skat
            "            WHERE spras = @sy-langu                    AND
            "                  ktopl = @<fs_acdocabiva0>-ktopl  AND
            "                  saknr = @<fs_acdocabiva0>-racct INTO CORRESPONDING FIELDS OF @wa_skat.
            "                IF sy-subrc = 0.
            "                  "Clasificación
            "                   sl_report-clasificacion = wa_skat-txt50.
            "                ENDIF.
            "              ENDIF.
            "            ENDLOOP.
            "            IF sl_report-base_0  LT 0.
            "              sl_report-base_0  = sl_report-base_0  * -1.
            "            ENDIF.
            "            sl_report-ind_iva_0 = c_w0.
            "          ENDIF.


            "          IF sl_report-base_16 LT 0.
            "            sl_report-total =  sl_report-total + sl_report-base_0  * -1.
            "          ELSE.
            "            sl_report-total =  sl_report-total + sl_report-base_0.
            "          ENDIF.

**** base 8

            ASSIGN tl_acdoca1_base_iva_8[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  mwskz =  c_w3 ] TO FIELD-SYMBOL(<fs_acdocabiva8>).
            IF sy-subrc = 0 AND  <fs_acdocabiva8>-blart NOT IN rg_z_fondo_fijo.
              CLEAR sl_report-base_8.

              SELECT ktopl, saknr,txt50
                          INTO TABLE @DATA(tl_skat4)
                          FROM skat
                          FOR ALL ENTRIES IN @tl_acdoca1_base_iva_8
                          WHERE spras = @sy-langu                    AND
                                ktopl = @tl_acdoca1_base_iva_8-ktopl AND
                                saknr = @tl_acdoca1_base_iva_8-racct.

              LOOP AT tl_acdoca1_base_iva_8 ASSIGNING <fs_acdocabiva8> WHERE  blart   NOT IN rg_z_fondo_fijo.
                IF <fs_acdocabiva8>-rbukrs NE <fs_acdoca2>-rbukrs OR
                   <fs_acdocabiva8>-gjahr  NE <fs_acdoca2>-gjahr  OR
                   <fs_acdocabiva8>-belnr  NE <fs_acdoca2>-belnr.
                  EXIT.
                ENDIF.
              ENDLOOP.
              IF <fs_acdocabiva8>-mwskz  IN rg_ziva_8_high.
                sl_report-base_8    = sl_report-base_8 + <fs_acdocabiva8>-hsl.
                ASSIGN tl_skat4[ ktopl = <fs_acdocabiva8>-ktopl saknr = <fs_acdocabiva8>-racct ] TO <fs_skat>.
                IF sy-subrc = 0.
                  "Clasificación
                  sl_report-clasificacion = <fs_skat>-txt50.
                ENDIF.
              ENDIF.
              IF sl_report-base_8 LT 0.
                sl_report-base_8 = sl_report-base_8 * -1.
              ENDIF.
              sl_report-indi_iva_8 = <fs_acdocabiva8>-mwskz.
            ENDIF.

            IF sl_report-base_8 LT 0.
              sl_report-total =  sl_report-total + sl_report-base_8  * -1.
            ELSE.
              sl_report-total =  sl_report-total + sl_report-base_8.
            ENDIF.

*** base 8

* procesamos el tipo RE la base gravable del 16%

            ASSIGN tl_acdoca1_base_iva_16[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr   mwskz   = c_w1 ] TO FIELD-SYMBOL(<fs_acdocabiva16>).
            IF sy-subrc = 0 AND  <fs_acdocabiva16>-blart             NOT IN rg_z_fondo_fijo.
              CLEAR sl_report-base_16.

              SELECT ktopl, saknr,txt50
                          INTO TABLE @DATA(tl_skat3)
                          FROM skat
                          FOR ALL ENTRIES IN @tl_acdoca1_base_iva_16
                          WHERE spras = @sy-langu                    AND
                                ktopl = @tl_acdoca1_base_iva_16-ktopl AND
                                saknr = @tl_acdoca1_base_iva_16-racct.

              LOOP AT tl_acdoca1_base_iva_16 ASSIGNING <fs_acdocabiva16> FROM sy-tabix.
                IF <fs_acdocabiva16>-blart   NOT IN rg_z_fondo_fijo.
                  IF <fs_acdocabiva16>-rbukrs NE <fs_acdoca2>-rbukrs OR
                     <fs_acdocabiva16>-gjahr  NE <fs_acdoca2>-gjahr  OR
                     <fs_acdocabiva16>-belnr  NE <fs_acdoca2>-belnr.
                    EXIT.
                  ELSE.
                    IF <fs_acdocabiva16>-mwskz  IN rg_ziva_16_high.
                      sl_report-base_16    = sl_report-base_16 + <fs_acdocabiva16>-hsl.
                      ASSIGN tl_skat3[ ktopl = <fs_acdocabiva16>-ktopl saknr = <fs_acdocabiva16>-racct ] TO <fs_skat>.
                      IF sy-subrc = 0.
                        "Clasificación
                        sl_report-clasificacion = <fs_skat>-txt50.
                      ENDIF.
                    ENDIF.
                    IF sl_report-base_16 LT 0.
                      sl_report-base_16 = sl_report-base_16 * -1.
                    ENDIF.
                    sl_report-ind_iva_16 = <fs_acdocabiva16>-mwskz.
                  ENDIF.

                ENDIF.

              ENDLOOP.

              IF sl_report-base_16 LT 0.
                sl_report-total =  sl_report-total + sl_report-base_16  * -1.
              ELSE.
                sl_report-total =  sl_report-total + sl_report-base_16.
              ENDIF.

            ENDIF.

            "Indicador de IVA 16%
            " no deberia importar
            ASSIGN tl_acdoca4[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr drcrk = c_s ktosl = c_vst koart = c_s  ] TO FIELD-SYMBOL(<fs_acdoca4>).
            IF sy-subrc = 0.
              LOOP AT tl_acdoca4 ASSIGNING <fs_acdoca4> FROM sy-tabix.
                IF <fs_acdoca4>-rbukrs NE <fs_acdoca2>-rbukrs OR
                   <fs_acdoca4>-gjahr  NE <fs_acdoca2>-gjahr  OR
                   <fs_acdoca4>-belnr  NE <fs_acdoca2>-belnr.
                  EXIT.
                ENDIF.
                IF <fs_acdoca4>-mwskz  IN rg_ziva_16_high.
                  sl_report-iva_16     = sl_report-iva_16 + <fs_acdoca4>-hsl.
                  sl_report-ind_iva_16 = <fs_acdoca4>-mwskz.

                ENDIF.
              ENDLOOP.
            ENDIF.

            IF sl_report-base_16 LT 0.
              sl_report-total =  sl_report-total + sl_report-iva_16   * -1.
            ELSE.
              sl_report-total =  sl_report-total + sl_report-iva_16 .
            ENDIF.

            "retenciones iva 6
            ASSIGN tl_acdoca5_ret_iva_6[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ] TO <fs_acdoca4>.
            IF sy-subrc = 0.
              sl_report-total = sl_report-total + <fs_acdoca4>-hsl.
              sl_report-iva_ret_6 = <fs_acdoca4>-hsl.
            ENDIF.
            "retenciones iva 4
            ASSIGN tl_acdoca5_ret_va_4[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ] TO <fs_acdoca4>.
            IF sy-subrc = 0.
              sl_report-total = sl_report-total + <fs_acdoca4>-hsl.
              sl_report-iva_retenido_4 = <fs_acdoca4>-hsl.
            ENDIF.



*        Validamos Escenario 6
            ASSIGN tl_acdoca_ret_escenario6[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ] TO FIELD-SYMBOL(<fs_acdoca_ret_escenario6>).
            IF sy-subrc = 0.
              sl_report-isr_honorarios = <fs_acdoca_ret_escenario6>-hsl.
              sl_report-total = sl_report-total + <fs_acdoca_ret_escenario6>-hsl.
            ENDIF.
            ASSIGN tl_acdoca_iva_escenario6[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ] TO FIELD-SYMBOL(<fs_acdoca_iva_escenario6>).
            IF sy-subrc = 0.
              sl_report-iva_ret_serv_prof = <fs_acdoca_iva_escenario6>-hsl.
              sl_report-total = sl_report-total + <fs_acdoca_iva_escenario6>-hsl.
            ENDIF.
*        Fin Validamos Escenario 6
*        Validamos Escenario 7
            ASSIGN tl_acdoca_ret_escenario7[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ] TO FIELD-SYMBOL(<fs_acdoca_ret_escenario7>).
            IF sy-subrc = 0.
              sl_report-isr_arrendamiento = <fs_acdoca_ret_escenario7>-hsl.
              sl_report-total = sl_report-total + <fs_acdoca_ret_escenario7>-hsl.
            ENDIF.
*          DATA(wa_acdoca_iva_escenario7) = tl_acdoca_iva_escenario7[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ].
*          TRY.
*              IF line_exists( tl_acdoca_iva_escenario7[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr ] ).
            ASSIGN tl_acdoca_iva_escenario7[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ] TO FIELD-SYMBOL(<fs_acdoca_iva_escenario7>).
            IF sy-subrc = 0." AND <fs_acdoca_iva_escenario7> IS ASSIGNED.
              sl_report-iva_ret_arren = <fs_acdoca_iva_escenario7>-hsl.
              sl_report-total = sl_report-total + <fs_acdoca_iva_escenario7>-hsl.
            ENDIF.
*              ENDIF.
*            CATCH cx_sy_zerodivide INTO ref_ejecucion.
*          ENDTRY.


            ASSIGN tl_acdoca_iva_8[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ] TO FIELD-SYMBOL(<fs_acdoca_iva_8>).
            IF sy-subrc = 0.
              sl_report-indi_iva_8 = <fs_acdoca_iva_8>-mwskz.
              sl_report-iva_8 = <fs_acdoca_iva_8>-hsl.
              sl_report-total = sl_report-total + <fs_acdoca_iva_8>-hsl.
*          sl_report-total = sl_report-total + <fs_acdoca_iva_escenario7>-hsl.
            ENDIF.

* retención cedular

            ASSIGN tl_acdoca_ret_cedular[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ] TO FIELD-SYMBOL(<fs_acdoca_ret_ced>).
            IF sy-subrc = 0.
              CLEAR: sl_report-imp_ret_cedular.
              sl_report-imp_ret_cedular  = <fs_acdoca_ret_ced>-hsl.
              sl_report-total = sl_report-total + sl_report-imp_ret_cedular.
*          sl_report-total = sl_report-total + <fs_acdoca_iva_escenario7>-hsl.
            ENDIF.


            ASSIGN tl_acdoca_ret_isr_pagext[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ] TO FIELD-SYMBOL(<fs_acdoca_pag_ext>).
            IF sy-subrc = 0.
              CLEAR: sl_report-isr_pagos_extranjero.
              sl_report-isr_pagos_extranjero  = <fs_acdoca_pag_ext>-hsl.
              sl_report-total = sl_report-total + sl_report-isr_pagos_extranjero.
*          sl_report-total = sl_report-total + <fs_acdoca_iva_escenario7>-hsl.
            ENDIF.
*        Fin Validamos Escenario 7
            "Validamos si es el importe escenario 4

*--> Insert AMP 06.01.2021
            SORT: tl_acdoca4_escenario4 BY belnr docln,
                  tl_acdoca3_aux  BY belnr docln.

            IF NOT tl_acdoca3_aux[] IS INITIAL.
              SELECT ktopl,saknr,txt50
         INTO TABLE @DATA(tl_skat_gral)
         FROM skat
         FOR ALL ENTRIES IN @tl_acdoca3_aux
         WHERE spras = @sy-langu         AND
               ktopl = @tl_acdoca3_aux-kokrs AND
               saknr = @tl_acdoca3_aux-racct.

              SELECT  bukrs,belnr, gjahr, buzei, hkont, xref3
          INTO TABLE @DATA(tl_bseg_gral)
                FROM bseg
             FOR ALL ENTRIES IN @tl_acdoca3_aux
               WHERE bukrs = @tl_acdoca3_aux-rbukrs  AND
                     belnr = @tl_acdoca3_aux-belnr AND
                     gjahr = @tl_acdoca3_aux-gjahr.
*       buzei = @tl_acdoca3_aux-docln.
            ENDIF.
*<-- End insert AMP
* se aplica a n escenarios
* empieza el verdadero procesamiento de datos aqui entra para varios casos

            ASSIGN tl_acdoca4_escenario4[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr
                                          augbl  = <fs_acdoca2>-augbl
                                          belnr  = <fs_acdoca2>-belnr ] TO <fs_acdoca4_escenario4>.
            IF sy-subrc = 0.
              DATA(vl_tabix2) = sy-tabix.
              DATA(vl_belnr) = <fs_acdoca4_escenario4>-belnr.
              ASSIGN tl_bseg_escenario4[ bukrs = <fs_acdoca4_escenario4>-rbukrs gjahr  = <fs_acdoca4_escenario4>-gjahr belnr = <fs_acdoca4_escenario4>-belnr ] TO FIELD-SYMBOL(<fs_bseg_escenario4>).
              IF sy-subrc = 0.
                "RFC
                sl_report-rfc = <fs_bseg_escenario4>-xref3." ktopl = <fs_bseg_escenario4>-ktopl
              ENDIF.

              ASSIGN tl_acdoca4_v1[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr drcrk = c_h ]  TO FIELD-SYMBOL(<fs_acdoca4_v1>).

* Sección de datos generales
              sl_report-n_doc_pago   =  <fs_acdoca4_v1>-augbl.
*              sl_report-n_sol_anticipo            = sl_report-n_documento_fac.   "Insert AMP 06.01.2021 Se comenta
              sl_report-n_sol_anticipo_ejercicio  = sl_report-n_documento_fac_ejercicio.
              sl_report-n_sol_anticipo_sociedad   = sl_report-n_documento_fac_sociedad.
              CLEAR: sl_report-n_documento_fac, sl_report-n_documento_fac_sociedad,sl_report-n_documento_fac_ejercicio,sl_report-base_no_obj,sl_report-total.
*              sl_report-impor_mon_l_fac  =  <fs_acdoca4_v1>-tsl * -1 .
*              sl_report-totalcargos =  <fs_acdoca4_v1>-tsl * -1 .
              sl_report-n_documento_fac_sociedad =  <fs_acdoca4_v1>-rbukrs.
              sl_report-totalcargos =  <fs_bseg>-dmbtr.
              sl_report-n_documento_fac_ejercicio =  <fs_acdoca4_v1>-gjahr.
              sl_report-n_documento_fac =  <fs_acdoca4_v1>-belnr.
*              sl_report-impor_mon_l_fac =  <fs_acdoca4_escenario4>-tsl * -1 .
              sl_report-impor_mon_l_fac =  <fs_acdoca4_v1>-tsl * -1 .
              sl_report-total = sl_report-impor_mon_l_fac.
              IF vl_cont NE 1.
*                sl_report-totalcargos  =  <fs_acdoca4_v1>-tsl * -1 .
*              ELSE.
**                CLEAR: sl_report-totalcargos.
              ENDIF.

*--> Insert AMP 06.01.2021

              DATA: cta_iva_16    TYPE i,
                    cta_iva_0     TYPE i,
                    cta_iva_8     TYPE i,
                    cta_iva_nd    TYPE i,
                    cta_iva_nobj  TYPE i,
                    lcta_iva_16   TYPE i,
                    lcta_iva_0    TYPE i,
                    lcta_iva_8    TYPE i,
                    lcta_iva_nd   TYPE i,
                    lcta_iva_nobj TYPE i.


              CLEAR: cta_iva_16,
                     cta_iva_0,
                     cta_iva_8,
                     cta_iva_nd,
                     cta_iva_nobj,
                     lcta_iva_16,
                     lcta_iva_0,
                     lcta_iva_8,
                     lcta_iva_nd,
                     lcta_iva_nobj.


              "se caga tota la información de cada base


              DATA(base_noobj)     =  tl_acdoca1_base_noobj.
              DATA(base_iva_16)    =  tl_acdoca1_base_iva_16 .
              DATA(base_iva_8)     =  tl_acdoca1_base_iva_8 .
              DATA(base_nodedu)    =  tl_acdoca1_base_nodedu.

              DATA(base_iva_0)     =  tl_acdoca_escenario_5.

              DELETE:   base_noobj    WHERE rbukrs EQ <fs_acdoca4_escenario4>-rbukrs AND  gjahr EQ <fs_acdoca4_escenario4>-gjahr AND  belnr NE <fs_acdoca4_escenario4>-belnr,
                        base_iva_16   WHERE rbukrs EQ <fs_acdoca4_escenario4>-rbukrs AND  gjahr EQ <fs_acdoca4_escenario4>-gjahr AND  belnr NE <fs_acdoca4_escenario4>-belnr,
                        base_nodedu   WHERE rbukrs EQ <fs_acdoca4_escenario4>-rbukrs AND  gjahr EQ <fs_acdoca4_escenario4>-gjahr AND  belnr NE <fs_acdoca4_escenario4>-belnr,
                        base_iva_8    WHERE rbukrs EQ <fs_acdoca4_escenario4>-rbukrs AND  gjahr EQ <fs_acdoca4_escenario4>-gjahr AND  belnr NE <fs_acdoca4_escenario4>-belnr,
                        base_iva_0    WHERE rbukrs EQ <fs_acdoca4_escenario4>-rbukrs AND  gjahr EQ <fs_acdoca4_escenario4>-gjahr AND  belnr NE <fs_acdoca4_escenario4>-belnr.


              SORT: base_noobj   BY rbukrs  gjahr    belnr  docln ASCENDING,
                    base_iva_16  BY rbukrs  gjahr    belnr  docln ASCENDING,
                    base_nodedu  BY rbukrs  gjahr    belnr  docln ASCENDING,
                    base_iva_8   BY rbukrs  gjahr    belnr  docln ASCENDING,
                    base_iva_0   BY rbukrs  gjahr    belnr  docln ASCENDING,
                    base_nodedu  BY rbukrs  gjahr    belnr  docln ASCENDING.

              DATA(tl_ctas_factura_c) =  tl_ctas_factura.
              DELETE ADJACENT DUPLICATES FROM tl_ctas_factura_c COMPARING rbukrs belnr gjahr racct.


              lcta_iva_nobj  = lines( base_noobj ).
              lcta_iva_16    = lines( base_iva_16   ).
              lcta_iva_8     = lines( base_iva_8    ).
              lcta_iva_nd    = lines( base_iva_8    ).
              lcta_iva_0     = lines( base_iva_0    ).

* ahora se determinara cuantas cuentas de gastos existen
* carga la posicion de acreedor
* ahora se pregunta si va a ser una factura solo con 1 posicion de acuerdo a la base

              LOOP AT tl_acdoca4_escenario4  INTO DATA(wa_acdoca3_aux) "FROM vl_tabix2 TO vl_tabix2
                                                       WHERE rbukrs EQ <fs_acdoca4_escenario4>-rbukrs
                                                         AND  gjahr EQ <fs_acdoca4_escenario4>-gjahr
                                                         AND  belnr EQ <fs_acdoca4_escenario4>-belnr
*                                                         AND  docln EQ <fs_acdoca4_escenario4>-docln
                                                         AND  ryear EQ <fs_acdoca4_escenario4>-ryear.
*                                                         AND  mwskz EQ  wa_acdoca2-mwskz.
                READ TABLE tl_skat_gral INTO DATA(wa_skat_gral) WITH KEY saknr = wa_acdoca3_aux-racct.
                IF sy-subrc EQ 0.
                  sl_report-clasificacion = wa_skat_gral-txt50.
                ENDIF.
                READ TABLE tl_bseg_gral INTO DATA(wa_bseg_gral) WITH KEY bukrs = wa_acdoca3_aux-rbukrs
                                                                         belnr = wa_acdoca3_aux-belnr
                                                                         gjahr = wa_acdoca3_aux-gjahr
                                                                         buzei = wa_acdoca3_aux-docln.
                IF sy-subrc EQ 0.
                  sl_report-rfc = wa_bseg_gral-xref3.
                ENDIF.
*                DELETE tl_acdoca4_escenario4.
                EXIT.
              ENDLOOP.

* esto funciona cuando hay una factura

              IF wa_acdoca3_aux IS NOT INITIAL AND wa_acdoca3_aux-blart   NOT IN rg_z_fondo_fijo.


*** codigo a basura
                CLEAR sl_report-n_sol_anticipo.

                IF wa_acdoca3_aux-mwskz IN rg_ziva_16_high.
                  READ TABLE  tl_acdoca1_base_iva_16 INTO  DATA(wa_base16_00)
                                                         WITH KEY rbukrs = wa_acdoca3_aux-rbukrs
                                                                   gjahr = wa_acdoca3_aux-gjahr
                                                                   belnr = wa_acdoca3_aux-belnr
                                                                   ryear = wa_acdoca3_aux-ryear.
                  IF sy-subrc = 0.
                    IF wa_base16_00-hsl LT 0. wa_base16_00-hsl = wa_base16_00-hsl * -1. ENDIF.
                    sl_report-base_16 = wa_base16_00-hsl.
                    sl_report-ind_iva_16 = wa_base16_00-mwskz.
                    sl_report-foliofiscal = wa_base16_00-sgtxt.
                    READ TABLE tl_skat_gral INTO DATA(wa_skat_gral2) WITH KEY saknr = wa_base16_00-racct.
                    IF sy-subrc EQ 0.
                      sl_report-clasificacion = wa_skat_gral2-txt50.
                    ENDIF.
                  ENDIF.

                ELSEIF wa_acdoca3_aux-mwskz IN rg_ziva_0.


                  sl_report-base_0 = wa_acdoca3_aux-hsl.
                  sl_report-ind_iva_0 = wa_acdoca3_aux-mwskz.


                ELSEIF wa_acdoca3_aux-mwskz IN rg_ziva_nd.
                  READ TABLE  tl_acdoca4_egreso INTO  DATA(wa_acdoca_dcta)
                                                          WITH KEY rbukrs = wa_acdoca3_aux-rbukrs
                                                                    gjahr = wa_acdoca3_aux-gjahr
                                                                    belnr = wa_acdoca3_aux-belnr
                                                                    ryear = wa_acdoca3_aux-ryear.
                  IF sy-subrc = 0."SOLO HAY UNA CUENTA DE GASTO
                    READ TABLE tl_skat_gral INTO DATA(wa_skat_gra00) WITH KEY saknr = wa_acdoca_dcta-racct.
                    IF sy-subrc EQ 0.
                      sl_report-clasificacion = wa_skat_gra00-txt50.
                    ENDIF.
                    IF wa_acdoca3_aux-hsl LT 0.
                      wa_acdoca3_aux-hsl = wa_acdoca3_aux-hsl * -1.
                    ENDIF.
                    sl_report-base_exento = wa_acdoca3_aux-hsl.
                    sl_report-ind_exento = wa_acdoca3_aux-mwskz.
                  ENDIF.

                ELSEIF  wa_acdoca3_aux-mwskz IS INITIAL.

* Este solo tiene una posición
                  READ TABLE  tl_acdoca4_no_ob INTO  DATA(wa_acdoca4_no_obj)
                                                                       WITH KEY rbukrs =  wa_acdoca3_aux-rbukrs
                                                                                 gjahr =  wa_acdoca3_aux-gjahr
                                                                                 belnr =  wa_acdoca3_aux-belnr
                                                                                 ryear =  wa_acdoca3_aux-ryear.
                  IF sy-subrc = 0.
                    READ TABLE tl_skat_gral INTO DATA(wa_skat_noobj) WITH KEY saknr = wa_acdoca4_no_obj-racct.
                    IF sy-subrc EQ 0.
                      sl_report-clasificacion = wa_skat_noobj-txt50.
                    ENDIF.
                  ENDIF.
                  IF wa_acdoca3_aux-tsl LT 0.
                    wa_acdoca3_aux-tsl = wa_acdoca3_aux-tsl *  -1.
                  ENDIF.
                  sl_report-base_no_obj  = wa_acdoca3_aux-tsl.
                ENDIF.
              ENDIF.
*<-- End insert AMP3232

              IF sl_report-n_doc_pago IS NOT INITIAL AND wa_acdoca3_aux-blart   NOT IN rg_z_fondo_fijo.
                CLEAR wa_acdoca3_aux.
                tl_report  = VALUE  #(
                            BASE tl_report          (  sl_report  )   ).
                CLEAR: sl_report-impor_mon_l_fac, sl_report-n_sol_anticipo,sl_report-n_sol_anticipo_ejercicio,sl_report-n_sol_anticipo_sociedad,sl_report-base_16,sl_report-iva_16, sl_report-total.
              ELSE.
                CLEAR  sl_report-impor_mon_l_fac.
              ENDIF.

* Carga la posición de acreedo
              DATA: lv_tot_fac TYPE acdoca-hsl.
              CLEAR lv_tot_fac.
* determina los totales de factura
              tl_acdoca4_imp_fac = tl_acdoca4_escenario4.

              DELETE tl_acdoca4_imp_fac WHERE rbukrs EQ <fs_acdoca2>-rbukrs  AND
                                    gjahr EQ <fs_acdoca2>-gjahr   AND
                                    belnr NE <fs_acdoca4_escenario4>-belnr  .

              LOOP AT tl_acdoca4_imp_fac ASSIGNING <fs_acdoca4_escenario4>
                             WHERE rbukrs EQ <fs_acdoca2>-rbukrs  AND
                                    gjahr EQ <fs_acdoca2>-gjahr   AND
                                    belnr EQ <fs_acdoca4_escenario4>-belnr  .
                lv_tot_fac  = lv_tot_fac + <fs_acdoca4_escenario4>-tsl * -1.
              ENDLOOP.
* hasta ahora se detemina el total de la factura aun falta determinar cuantas cuentas de gasto existen
              IF sy-subrc = 0.
                DELETE tl_acdoca4_escenario4 WHERE blart NE 'FF'.
                LOOP AT tl_acdoca4_escenario4 ASSIGNING <fs_acdoca4_escenario4> "FROM vl_tabix2 + 1
                  WHERE rbukrs EQ <fs_acdoca2>-rbukrs  AND
                         gjahr EQ <fs_acdoca2>-gjahr   AND
                         augbl EQ <fs_acdoca2>-augbl  .

                  IF sy-tabix LT ( vl_tabix2 + 1 ) AND  <fs_acdoca4_escenario4>-bschl = '31'.

                    CONTINUE.

                  ENDIF.


* Obtiene la base del impuesto
                  LOOP AT tl_acdoca4_egreso INTO wa_acdoca3_aux "from vl_tabix2 to vl_tabix2
                                                           WHERE rbukrs EQ <fs_acdoca4_escenario4>-rbukrs
                                                             AND  gjahr EQ <fs_acdoca4_escenario4>-gjahr
                                                             AND  belnr EQ <fs_acdoca4_escenario4>-belnr
*                                                         AND  docln EQ <fs_acdoca4_escenario4>-docln
                                                             AND  ryear EQ <fs_acdoca4_escenario4>-ryear.
*                                                         AND  mwskz EQ  wa_acdoca2-mwskz.



                    IF wa_acdoca3_aux IS NOT INITIAL.
                      CLEAR sl_report-n_sol_anticipo.
                      sl_report-n_documento_fac           = wa_acdoca3_aux-belnr.
                      sl_report-n_documento_fac_sociedad  = wa_acdoca3_aux-rbukrs.
                      sl_report-n_documento_fac_ejercicio = wa_acdoca3_aux-gjahr.
                      IF lv_tot_fac  LT 0. lv_tot_fac = lv_tot_fac  * -1. ENDIF.
                      sl_report-impor_mon_l_fac           =  lv_tot_fac .
                      CLEAR lv_tot_fac .
                    ENDIF.

                    DELETE tl_acdoca4_egreso.
                    EXIT.
                  ENDLOOP. "egreso

**¨solo carga datos generales de facturas

* ahora procedemos a meter la base de iva de la primera cuenta
                  vl_total = 0.
*               if lcta_iva_16 = 1. " solo hay una cuenta
                  DATA(tl_ctas_factura_c_i)  = tl_ctas_factura.
                  DATA(lv_index) = 0.
                  CLEAR   sl_report-total.
                  DELETE  tl_ctas_factura_c_i WHERE belnr NE wa_acdoca3_aux-belnr.
                  SORT tl_ctas_factura_c_i BY rbukrs gjahr belnr docln ASCENDING.
                  LOOP AT tl_ctas_factura_c_i INTO DATA(wa_ctas_factura).

                    CLEAR:
                             sl_report-iva_ret_6,
                             sl_report-iva_retenido_4,
                             sl_report-isr_honorarios,
                             sl_report-iva_ret_arren,
                             sl_report-imp_ret_cedular,
                             sl_report-iva_ret_serv_prof,
                             sl_report-isr_pagos_extranjero,
                             sl_report-isr_arrendamiento,
                             sl_report-n_sol_anticipo,
                             sl_report-n_sol_anticipo_ejercicio,
                             sl_report-n_sol_anticipo_sociedad,
                             sl_report-base_16,
                             sl_report-base_8,
                             sl_report-base_0,
                             sl_report-base_no_obj,
                             sl_report-base_exento,
                             sl_report-foliofiscal,
                             sl_report-iva_8,
                             sl_report-iva_16,
                             sl_report-ind_iva_16,
                             sl_report-ind_iva_0,
                             sl_report-indi_iva_8,
                             sl_report-ind_exento,
                             sl_report-clasificacion,
                             sl_report-total,
                             lv_tot_fac,
                             vl_total,
                             sl_report-total,
                             sl_report-base_exento,
                             sl_report-base_no_obj,
                             sl_report-ind_iva_16.
                    ADD 1 TO  lv_index.
                    IF lv_index = 1.
* empieza el procesamiento de cuentas
                      CLEAR: sl_report-base_16, vl_total.
                      LOOP AT base_iva_16 ASSIGNING  <fs_acdocabiva16> WHERE racct = wa_ctas_factura-racct.
                        vl_total = vl_total +  <fs_acdocabiva16>-hsl.
                        IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
** primer base
                        sl_report-base_16    = sl_report-base_16 + vl_total.
                        sl_report-ind_iva_16 = <fs_acdocabiva16>-mwskz.
                        READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_ctas_factura-racct.
                        IF sy-subrc EQ 0.
                          sl_report-clasificacion = wa_skat_gra00-txt50.
                        ENDIF.
                        sl_report-foliofiscal               = <fs_acdocabiva16>-sgtxt.
                        DELETE base_iva_16.
                        EXIT.
                      ENDLOOP.

                      IF sy-subrc EQ 0.
                        CLEAR:  vl_total.
                        sl_report-total = sl_report-total + sl_report-base_16 .
** obtiene el iva de esa base
                        CLEAR: sl_report-iva_16, sl_report-ind_iva_16.
                        DATA(lt_iva_16) =  tl_acdoca4.
                        DELETE lt_iva_16  WHERE rbukrs EQ <fs_acdoca4_escenario4>-rbukrs AND  gjahr EQ <fs_acdoca4_escenario4>-gjahr AND  belnr NE <fs_acdoca4_escenario4>-belnr.
                        SORT lt_iva_16 BY rbukrs  gjahr    belnr  docln ASCENDING.
                        LOOP AT lt_iva_16 ASSIGNING <fs_acdoca4>  WHERE rbukrs EQ <fs_acdoca2>-rbukrs AND
                                                                         gjahr  EQ <fs_acdoca2>-gjahr  AND
                                                                         belnr  EQ <fs_acdoca2>-belnr  AND
                                                                         racct IN rg_ziva_16.
                          IF <fs_acdoca4>-mwskz  IN rg_ziva_16_high.
                            sl_report-iva_16     = sl_report-iva_16 + <fs_acdoca4>-hsl.
                            sl_report-ind_iva_16 = <fs_acdoca4>-mwskz.
                          ENDIF.

                          DELETE lt_iva_16 .
                          EXIT.
                        ENDLOOP.
                      ENDIF.
                      sl_report-total = sl_report-total + sl_report-iva_16 .
*** retenciones


                      "Procesamiento de base al 8 %
                      CLEAR:  sl_report-base_8, vl_total.

                      LOOP AT  base_iva_8  INTO DATA(wa_basend)  WHERE racct = wa_ctas_factura-racct..
                        vl_total = vl_total +  wa_basend-hsl.
                        IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                        IF  wa_basend-mwskz = 'W3'.
                          sl_report-indi_iva_8 = 'W3'.
                          sl_report-base_8 = vl_total + sl_report-base_8.
                        ENDIF.
                        READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_ctas_factura-racct.
                        IF sy-subrc EQ 0.
                          sl_report-clasificacion = wa_skat_gra00-txt50.
                        ENDIF.
                        sl_report-foliofiscal               = wa_basend-sgtxt.
                        DELETE  base_iva_8." base_noobj.
                        EXIT.
                      ENDLOOP.
                      .

                      IF sy-subrc EQ 0.
                        CLEAR:  vl_total.
                        sl_report-total = sl_report-total + sl_report-base_8.
** obtiene el iva de esa base

                        CLEAR: sl_report-iva_8, sl_report-indi_iva_8.
                        DATA(lt_iva_8) =  tl_acdoca4.
                        DELETE lt_iva_8  WHERE rbukrs EQ <fs_acdoca4_escenario4>-rbukrs AND  gjahr EQ <fs_acdoca4_escenario4>-gjahr AND  belnr NE <fs_acdoca4_escenario4>-belnr.
                        SORT lt_iva_8 BY rbukrs  gjahr    belnr  docln ASCENDING.

                        LOOP AT  lt_iva_8 ASSIGNING <fs_acdoca4>  WHERE rbukrs EQ <fs_acdoca2>-rbukrs AND
                                                                         gjahr  EQ <fs_acdoca2>-gjahr  AND
                                                                         belnr  EQ <fs_acdoca2>-belnr  AND
                                                                         racct IN rg_ziva_8_low .
                          IF <fs_acdoca4>-mwskz  IN rg_ziva_8_high  .
                            sl_report-iva_8    = sl_report-iva_8 + <fs_acdoca4>-hsl.
                            sl_report-indi_iva_8 = <fs_acdoca4>-mwskz.
                          ENDIF.

                          DELETE lt_iva_8.
                          EXIT.
                        ENDLOOP.
                      ENDIF.
                      sl_report-total = sl_report-total + sl_report-iva_8 .



*base cero
                      CLEAR:  sl_report-base_0, vl_total.

                      LOOP AT base_iva_0  INTO DATA(wa_base0)  WHERE racct = wa_ctas_factura-racct..
                        vl_total = vl_total +  wa_base0-hsl.
                        IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                        IF  wa_base0-mwskz = 'W0'.
                          sl_report-ind_iva_0  = 'W0'.
                          sl_report-base_0 = vl_total + sl_report-base_0.
                        ENDIF.
                        READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_ctas_factura-racct.
                        IF sy-subrc EQ 0.
                          sl_report-clasificacion = wa_skat_gra00-txt50.
                        ENDIF.
                        sl_report-foliofiscal               = wa_base0-sgtxt.
                        DELETE base_iva_0." base_noobj.
                        EXIT.
                      ENDLOOP.
                      sl_report-total = sl_report-total + sl_report-base_0.

** el no obj

                      CLEAR:  sl_report-base_no_obj, vl_total.

                      LOOP AT base_noobj  INTO DATA(wa_nobj)  WHERE racct = wa_ctas_factura-racct..
                        vl_total = vl_total +  wa_nobj-hsl.
                        IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                        IF  wa_nobj-mwskz = ''.
                          sl_report-base_no_obj = vl_total + sl_report-base_no_obj.
                        ENDIF.
                        READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_ctas_factura-racct.
                        IF sy-subrc EQ 0.
                          sl_report-clasificacion = wa_skat_gra00-txt50.
                        ENDIF.
                        sl_report-foliofiscal               = wa_nobj-sgtxt.
                        DELETE base_noobj. " base_noobj.
                        EXIT.
                      ENDLOOP.
                      sl_report-total = sl_report-total + sl_report-base_no_obj.




                      CLEAR:  sl_report-base_nodedu, vl_total,sl_report-ind_nodedu.

                      LOOP AT  base_nodedu INTO wa_basend  WHERE racct = wa_ctas_factura-racct..
                        vl_total = vl_total +  wa_basend-hsl.
                        IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                        IF  wa_basend-mwskz = 'W8'.
                          sl_report-ind_nodedu = 'W8'.
                          sl_report-base_nodedu = vl_total + sl_report-base_nodedu .
                        ENDIF.
                        READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_ctas_factura-racct.
                        IF sy-subrc EQ 0.
                          sl_report-clasificacion = wa_skat_gra00-txt50.
                        ENDIF.
                        sl_report-foliofiscal               = wa_basend-sgtxt.
                        DELETE  base_nodedu." base_noobj.
                        EXIT.
                      ENDLOOP.

                      sl_report-total = sl_report-total + sl_report-base_nodedu.

                      "retenciones iva 6
                      ASSIGN tl_acdoca5_ret_iva_6[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ] TO <fs_acdoca4>.
                      IF sy-subrc = 0.
                        sl_report-total = sl_report-total + <fs_acdoca4>-hsl.
                        sl_report-iva_ret_6 = <fs_acdoca4>-hsl.
                      ENDIF.
                      "retenciones iva 4
                      ASSIGN tl_acdoca5_ret_va_4[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ] TO <fs_acdoca4>.
                      IF sy-subrc = 0.
                        sl_report-total = sl_report-total + <fs_acdoca4>-hsl.
                        sl_report-iva_retenido_4 = <fs_acdoca4>-hsl.
                      ENDIF.

                      READ TABLE tl_acdoca_ret_escenario6 WITH KEY  rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ASSIGNING <fs_acdoca_ret_escenario6>.
                      IF sy-subrc = 0.
                        sl_report-isr_honorarios = <fs_acdoca_ret_escenario6>-hsl.
                        sl_report-total = sl_report-total + <fs_acdoca_ret_escenario6>-hsl.
                      ENDIF.

                      READ TABLE  tl_acdoca_iva_escenario6 WITH KEY  rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr ASSIGNING <fs_acdoca_iva_escenario6>.
                      IF sy-subrc = 0.
                        sl_report-iva_ret_serv_prof = <fs_acdoca_iva_escenario6>-hsl.
                        sl_report-total = sl_report-total + <fs_acdoca_iva_escenario6>-hsl.
                      ENDIF.

                      READ TABLE tl_acdoca_ret_escenario7 WITH KEY  rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ASSIGNING <fs_acdoca_ret_escenario7>.
                      IF sy-subrc = 0.
                        sl_report-isr_arrendamiento = <fs_acdoca_ret_escenario7>-hsl.
                        sl_report-total = sl_report-total + <fs_acdoca_ret_escenario7>-hsl.
                      ENDIF.


                      READ TABLE tl_acdoca_iva_escenario7 WITH  KEY rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr ASSIGNING <fs_acdoca_iva_escenario7>.
                      IF sy-subrc = 0.
                        sl_report-iva_ret_arren = <fs_acdoca_iva_escenario7>-hsl.
                        sl_report-total = sl_report-total + <fs_acdoca_iva_escenario7>-hsl.
                      ENDIF.


                      READ TABLE tl_acdoca_ret_cedular WITH KEY rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ASSIGNING <fs_acdoca_ret_ced>.
                      IF sy-subrc = 0.
                        CLEAR: sl_report-imp_ret_cedular.
                        sl_report-imp_ret_cedular  = <fs_acdoca_ret_ced>-hsl.
                        sl_report-total = sl_report-total + sl_report-imp_ret_cedular.
                      ENDIF.


                      READ TABLE  tl_acdoca_ret_isr_pagext WITH KEY rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ASSIGNING   <fs_acdoca_pag_ext>.
                      IF sy-subrc = 0.
                        CLEAR: sl_report-isr_pagos_extranjero.
                        sl_report-isr_pagos_extranjero  = <fs_acdoca_pag_ext>-hsl.
                        sl_report-total = sl_report-total + sl_report-isr_pagos_extranjero.
                      ENDIF.
*** fin de retenciones
                      sl_report-n_doc_pago   =  <fs_acdoca4_escenario4>-augbl.

                      sl_report-n_sol_anticipo_ejercicio = sl_report-n_documento_fac_ejercicio.
                      sl_report-n_sol_anticipo_sociedad  = sl_report-n_documento_fac_sociedad.

                      LOOP AT tl_report INTO DATA(wa_report) WHERE n_doc_pago = <fs_acdoca2>-augbl
                                                                           AND razon_s IS NOT INITIAL.
                        sl_report-razon_s = wa_report-razon_s.
                        sl_report-acredor = wa_report-acredor.

                      ENDLOOP.
                      IF sl_report-n_doc_pago IS NOT INITIAL.
                        CLEAR wa_acdoca3_aux.
                        tl_report  = VALUE  #(
                                    BASE tl_report          (  sl_report  )   ).
                      ENDIF.
                    ELSE.
* empieza el procesamiento de cuentas
                      CLEAR: sl_report-impor_mon_l_fac,
                             sl_report-iva_ret_6,
                             sl_report-iva_retenido_4,
                             sl_report-iva_ret_arren,
                             sl_report-isr_honorarios,
                             sl_report-imp_ret_cedular,
                             sl_report-iva_ret_serv_prof,
                             sl_report-isr_pagos_extranjero,
                             sl_report-isr_arrendamiento,
                             sl_report-n_sol_anticipo,
                             sl_report-n_sol_anticipo_ejercicio,
                             sl_report-n_sol_anticipo_sociedad,
                             sl_report-base_16,
                             sl_report-base_8,
                             sl_report-base_0,
                             sl_report-base_no_obj,
                             sl_report-base_exento,
                             sl_report-foliofiscal,
                             sl_report-iva_8,
                             sl_report-iva_16,
                             sl_report-ind_iva_16,
                             sl_report-ind_iva_0,
                             sl_report-indi_iva_8,
                             sl_report-ind_exento,
                             sl_report-clasificacion,
                             sl_report-total,
                             lv_tot_fac,
                             vl_total,
                             sl_report-total,
                             sl_report-base_exento,
                             sl_report-base_no_obj,
                             sl_report-ind_iva_16.



                      "Procesamiento de base al 8 %
                      CLEAR:  sl_report-base_8, vl_total.

                      LOOP AT  base_iva_8  INTO DATA(wa_basend1)  WHERE racct = wa_ctas_factura-racct..
                        vl_total = vl_total +  wa_basend1-hsl.
                        IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                        IF  wa_basend1-mwskz = 'W3'.
                          sl_report-indi_iva_8 = 'W3'.
                          sl_report-base_8 = vl_total + sl_report-base_8.
                        ENDIF.
                        READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_ctas_factura-racct.
                        IF sy-subrc EQ 0.
                          sl_report-clasificacion = wa_skat_gra00-txt50.
                        ENDIF.
                        sl_report-foliofiscal               = wa_basend-sgtxt.
                        DELETE  base_iva_8." base_noobj.
                        EXIT.
                      ENDLOOP.
                      .

                      IF sy-subrc EQ 0.
                        CLEAR:  vl_total.
                        sl_report-total = sl_report-total + sl_report-base_8.
** obtiene el iva de esa base

                        CLEAR: sl_report-iva_8, sl_report-indi_iva_8.
                        DATA(lt_iva_81) =  tl_acdoca4.
                        DELETE lt_iva_81  WHERE rbukrs EQ <fs_acdoca4_escenario4>-rbukrs AND  gjahr EQ <fs_acdoca4_escenario4>-gjahr AND  belnr NE <fs_acdoca4_escenario4>-belnr.
                        SORT lt_iva_81 BY rbukrs  gjahr    belnr  docln ASCENDING.

                        LOOP AT  lt_iva_81 ASSIGNING <fs_acdoca4>  WHERE rbukrs EQ <fs_acdoca2>-rbukrs AND
                                                                         gjahr  EQ <fs_acdoca2>-gjahr  AND
                                                                         belnr  EQ <fs_acdoca2>-belnr  AND
                                                                         racct IN rg_ziva_8_low .
                          IF <fs_acdoca4>-mwskz  IN rg_ziva_8_high  .
                            sl_report-iva_8    = sl_report-iva_8 + <fs_acdoca4>-hsl.
                            sl_report-indi_iva_8 = <fs_acdoca4>-mwskz.
                          ENDIF.

                          DELETE lt_iva_81.
                          EXIT.
                        ENDLOOP.
                      ENDIF.
                      sl_report-total = sl_report-total + sl_report-iva_8 .

                      LOOP AT base_iva_16 ASSIGNING  <fs_acdocabiva16> WHERE racct = wa_ctas_factura-racct.
                        vl_total = vl_total + <fs_acdocabiva16>-hsl.
                        IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
** primer base
                        sl_report-base_16    = sl_report-base_16 + vl_total.
                        sl_report-ind_iva_16 = <fs_acdocabiva16>-mwskz.

                        READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_ctas_factura-racct.
                        IF sy-subrc EQ 0.
                          sl_report-clasificacion = wa_skat_gra00-txt50.
                        ENDIF.
                        sl_report-foliofiscal               = <fs_acdocabiva16>-sgtxt.
                        DELETE base_iva_16.
                        EXIT.
                      ENDLOOP.
                      IF sy-subrc = 0.
                        CLEAR vl_total.
                        sl_report-total = sl_report-total + sl_report-base_16 .
** obtiene el iva de esa base
                        LOOP AT tl_acdoca4 ASSIGNING <fs_acdoca4>  WHERE rbukrs EQ <fs_acdoca2>-rbukrs AND
                                                                         gjahr  EQ <fs_acdoca2>-gjahr  AND
                                                                         belnr  EQ <fs_acdoca2>-belnr  AND
                                                                         racct IN rg_ziva_16.
                          IF <fs_acdoca4>-mwskz  IN rg_ziva_16_high.
                            sl_report-iva_16     = sl_report-iva_16 + <fs_acdoca4>-hsl.
                            sl_report-ind_iva_16 = <fs_acdoca4>-mwskz.
                          ENDIF.

                          DELETE tl_acdoca4.
                          EXIT.
                        ENDLOOP.
                        sl_report-total = sl_report-total + sl_report-iva_16 .
                      ENDIF.
*** retenciones


*base cero3232
                      CLEAR:  sl_report-base_0, vl_total.

                      LOOP AT base_iva_0 INTO DATA(wa_base1)  WHERE racct = wa_ctas_factura-racct..
                        vl_total = vl_total +  wa_base1-hsl.
                        IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                        IF  wa_base1-mwskz    = 'W0'.
                          sl_report-ind_iva_0  = 'W0'.
                          sl_report-base_0    = sl_report-base_0 + vl_total.
                          sl_report-ind_iva_0 =  'W0'.
                        ENDIF.

                        READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_ctas_factura-racct.
                        IF sy-subrc EQ 0.
                          sl_report-clasificacion = wa_skat_gra00-txt50.
                        ENDIF.
                        sl_report-foliofiscal               = wa_base1-sgtxt.
                        DELETE  base_iva_0." base_noobj.
                        EXIT.
                      ENDLOOP.
                      sl_report-total = sl_report-total + sl_report-base_0 .

** el no obj

                      CLEAR:  sl_report-base_no_obj, vl_total.

                      LOOP AT base_noobj  INTO DATA(wa_nobj2)  WHERE racct = wa_ctas_factura-racct..
                        vl_total = vl_total +  wa_nobj2-hsl.
                        IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                        IF  wa_nobj2-mwskz = ''.
                          sl_report-base_no_obj = vl_total + sl_report-base_no_obj.
                        ENDIF.

                        READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_ctas_factura-racct.
                        IF sy-subrc EQ 0.
                          sl_report-clasificacion = wa_skat_gra00-txt50.
                        ENDIF.
                        sl_report-foliofiscal               = wa_nobj2-sgtxt.
                        DELETE base_noobj. " base_noobj.
                        EXIT.
                      ENDLOOP.

                      sl_report-total = sl_report-total + sl_report-base_no_obj.

                      CLEAR:  sl_report-base_nodedu, vl_total,sl_report-ind_nodedu.

                      LOOP AT  base_nodedu INTO wa_basend  WHERE racct = wa_ctas_factura-racct..
                        vl_total = vl_total +  wa_basend-hsl.
                        IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                        IF  wa_basend-mwskz = 'W8'.
                          sl_report-ind_nodedu = 'W8'.
                          sl_report-base_nodedu = vl_total + sl_report-base_nodedu .
                        ENDIF.
                        READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_ctas_factura-racct.
                        IF sy-subrc EQ 0.
                          sl_report-clasificacion = wa_skat_gra00-txt50.
                        ENDIF.
                        sl_report-foliofiscal               = wa_basend-sgtxt.
                        DELETE  base_nodedu." base_noobj.
                        EXIT.
                      ENDLOOP.

                      sl_report-total = sl_report-total + sl_report-base_nodedu.

                      IF sl_report-n_doc_pago IS NOT INITIAL.
                        CLEAR wa_acdoca3_aux.


                        tl_report  = VALUE  #(
                                    BASE tl_report          (  sl_report  )   ).
                      ENDIF.


                    ENDIF.
                    DELETE  tl_ctas_factura_c_i WHERE  rbukrs EQ <fs_acdoca2>-rbukrs AND
                                                                         gjahr  EQ <fs_acdoca2>-gjahr  AND
                                                                         belnr  EQ <fs_acdoca2>-belnr  AND
                                                                         racct = wa_ctas_factura-racct AND
                                                                         docln =  wa_ctas_factura-docln.
                  ENDLOOP.
                  DELETE tl_acdoca4_escenario4  WHERE rbukrs EQ <fs_acdoca2>-rbukrs AND
                                                                         gjahr  EQ <fs_acdoca2>-gjahr  AND
                                                                         belnr  EQ <fs_acdoca2>-belnr.
                ENDLOOP.
              ENDIF. "fin escenario multiples cuentas

***************************************** multiples facturas

            ELSE."si no entra al escenario 4 valida el 5
              IF  <fs_acdoca2>-blart NE c_blart." bo es RE
                ASSIGN tl_acdoca_escenario_5[ rbukrs = <fs_acdoca2>-rbukrs gjahr  = <fs_acdoca2>-gjahr belnr = <fs_acdoca2>-belnr  ]  TO FIELD-SYMBOL(<fs_acdoca_escenario_5>).
                IF sy-subrc = 0.
                  vl_tabix2 = sy-tabix.
                  IF <fs_acdoca3> IS ASSIGNED.
                    sl_report-foliofiscal = <fs_acdoca3>-sgtxt.
                    vl_buzei = |{ <fs_acdoca3>-docln ALPHA = OUT }|.
                    vl_buzei = |{ vl_buzei ALPHA = IN }|.
                    TRY.
                        ASSIGN tl_bseg_escenario5[ bukrs = <fs_acdoca3>-rbukrs gjahr  = <fs_acdoca3>-gjahr belnr = <fs_acdoca3>-belnr buzei = vl_buzei ]  TO FIELD-SYMBOL(<fs_bseg_escenario5>).
                      CATCH cx_root.
*                 Assignment failed, do what is required
                    ENDTRY.
                    IF sy-subrc = 0.
                      " si ya trae el rfc cargado no lo vuelve a poner
                      IF sl_report-rfc IS NOT INITIAL.
                        sl_report-rfc = <fs_bseg_escenario5>-xref3.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                  sl_report-impor_mon_l_fac = <fs_acdoca2>-hsl * -1.
*                tl_report  = VALUE  #(
*                             BASE tl_report          (  sl_report  )   ).

                  CLEAR: sl_report-base_16,sl_report-iva_16,sl_report-foliofiscal.
                  IF  <fs_acdoca2>-blart IN rg_z_fondo_fijo.
                    CLEAR: sl_report-totalcargos.
                  ENDIF.
                  LOOP AT tl_acdoca_escenario_5 ASSIGNING <fs_acdoca_escenario_5> FROM vl_tabix2.
                    IF <fs_acdoca_escenario_5>-rbukrs NE <fs_acdoca2>-rbukrs OR
                       <fs_acdoca_escenario_5>-gjahr  NE <fs_acdoca2>-gjahr  OR
                       <fs_acdoca_escenario_5>-belnr  NE <fs_acdoca2>-belnr.
                      EXIT.
                    ENDIF.

                    ASSIGN tl_skat[ ktopl = <fs_acdoca_escenario_5>-ktopl saknr = <fs_acdoca_escenario_5>-racct ] TO <fs_skat>.
                    IF sy-subrc = 0.
                      "Clasificación
                      sl_report-clasificacion = <fs_skat>-txt50.
                    ENDIF.
                    vl_buzei = |{ <fs_acdoca_escenario_5>-docln ALPHA = OUT }|.
                    vl_buzei = |{ vl_buzei ALPHA = IN }|.
                    IF <fs_acdoca3> IS ASSIGNED.
                      TRY.
                          ASSIGN tl_bseg_escenario5[ bukrs = <fs_acdoca_escenario_5>-rbukrs gjahr  = <fs_acdoca_escenario_5>-gjahr belnr = <fs_acdoca3>-belnr buzei = vl_buzei ]  TO <fs_bseg_escenario5>.
                        CATCH cx_root.
*                 Assignment failed, do what is required
                      ENDTRY.
                    ENDIF.
                    IF sy-subrc = 0.
*                    sl_report-rfc = <fs_bseg_escenario5>-xref3.
                    ENDIF.
                    IF <fs_bseg>-dmbtr LT 0. <fs_bseg>-dmbtr = <fs_bseg>-dmbtr * -1. ENDIF.
                    sl_report-totalcargos          = <fs_bseg>-dmbtr.
                    sl_report-total           = <fs_acdoca_escenario_5>-tsl.
                    sl_report-ind_iva_0      = <fs_acdoca_escenario_5>-mwskz.
                    sl_report-base_0          = <fs_acdoca_escenario_5>-tsl.
                    sl_report-foliofiscal     = <fs_acdoca_escenario_5>-sgtxt.

                    tl_report  = VALUE  #(
                                 BASE tl_report          (  sl_report  )   ).

                  ENDLOOP.

                ELSE.

                  IF <fs_acdoca2>-blart NOT IN  rg_acreedores_movto_high. " solo aplica si no es
                    sl_report-impor_mon_l_fac = <fs_acdoca2>-hsl * -1.
                    sl_report-totalcargos          = <fs_bseg>-dmbtr.
                    tl_report  = VALUE  #(
                                 BASE tl_report          (  sl_report  )   ).
                  ENDIF.
                  CLEAR: sl_report-totalcargos,
                         sl_report-total.  "Insert AMP 23.02.2021
                ENDIF.
              ELSE.
                IF <fs_bseg>-dmbtr LT 0. <fs_bseg>-dmbtr = <fs_bseg>-dmbtr * -1. ENDIF.
                sl_report-impor_mon_l_fac = <fs_acdoca2>-tsl * -1.
                tl_report  = VALUE  #(
                              BASE tl_report          (  sl_report  )   ).
                CLEAR sl_report-totalcargos.  "Insert AMP 23.02.2021
              ENDIF. " fin de RE
            ENDIF.

            DELETE tl_acdoca2 WHERE augbl = lv_pago_procesado.
            CLEAR lv_pago_procesado.
          ENDLOOP.

        ENDIF.
        ASSIGN tl_bseg_escenario10[ bukrs = <fs_bseg>-bukrs belnr = <fs_bseg>-zuonr gjahr = <fs_bseg>-gjahr shkzg = c_h ] TO FIELD-SYMBOL(<fs_bseg_escenario10>).
        IF sy-subrc = 0.
          sl_report = sl_report_aux.
          CLEAR:  sl_report-totalcargos .
          sl_report-descripcion = <fs_bseg_escenario10>-sgtxt.
          sl_report-n_documento_fac           = <fs_bseg>-zuonr.
          sl_report-n_documento_fac_ejercicio = <fs_bseg>-gjahr.
          sl_report-n_documento_fac_sociedad  = <fs_bseg>-bukrs.
          sl_report-impor_mon_l_fac           = <fs_bseg>-dmbtr.
          LOOP AT tl_bseg_escenario10 ASSIGNING <fs_bseg_escenario10> FROM sy-tabix.
            IF <fs_bseg_escenario10>-bukrs NE <fs_bseg>-bukrs OR
               <fs_bseg_escenario10>-belnr NE <fs_bseg>-zuonr OR
               <fs_bseg_escenario10>-gjahr NE <fs_bseg>-gjahr OR
               <fs_bseg_escenario10>-shkzg NE c_h.
              EXIT.
            ENDIF.
            sl_report-totalcargos = sl_report-totalcargos  + <fs_bseg_escenario10>-dmbtr.
          ENDLOOP.
          sl_report-total       = sl_report-totalcargos.
          sl_report-fechapago   = <fs_acdoca>-augdt.
          ASSIGN tl_bseg_escenario10[ bukrs = <fs_bseg>-bukrs belnr = <fs_bseg>-zuonr gjahr = <fs_bseg>-gjahr shkzg = c_s ] TO <fs_bseg_escenario10>.
          IF sy-subrc = 0.
            LOOP AT tl_bseg_escenario10 ASSIGNING <fs_bseg_escenario10> FROM sy-tabix.
              IF <fs_bseg_escenario10>-bukrs NE <fs_bseg>-bukrs OR
                 <fs_bseg_escenario10>-belnr NE <fs_bseg>-zuonr OR
                 <fs_bseg_escenario10>-gjahr NE <fs_bseg>-gjahr OR
                 <fs_bseg_escenario10>-shkzg NE c_s.
                EXIT.
              ENDIF.
              sl_report-base_16 = sl_report-base_16  + <fs_bseg_escenario10>-dmbtr.
            ENDLOOP.
          ENDIF.
          ASSIGN tl_bseg_escenario10_iva[ bukrs = <fs_bseg>-bukrs belnr = <fs_bseg>-zuonr gjahr = <fs_bseg>-gjahr ] TO <fs_bseg_escenario10>.
          IF sy-subrc = 0.
            LOOP AT tl_bseg_escenario10_iva ASSIGNING <fs_bseg_escenario10> FROM sy-tabix.
              IF <fs_bseg_escenario10>-bukrs NE <fs_bseg>-bukrs OR
                 <fs_bseg_escenario10>-belnr NE <fs_bseg>-zuonr OR
                 <fs_bseg_escenario10>-gjahr NE <fs_bseg>-gjahr OR
                 <fs_bseg_escenario10>-hkont NOT IN rg_ziva_16.
                EXIT.
              ENDIF.
              sl_report-iva_16 = sl_report-iva_16  + <fs_bseg_escenario10>-dmbtr.
            ENDLOOP.
          ENDIF.
          tl_report  = VALUE  #(
                          BASE tl_report          (  sl_report  )   ).
        ENDIF.

      ENDLOOP.


    ENDIF.
    DATA: lv_per TYPE n LENGTH 3.

* esta sección es solamente para la busqueda de LQ y casos especiales
    "1.1. Búsqueda de doctos KZ para sociedad, periodo, ejercicio busca pagos
    SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
           kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
           augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt,linetype "fecha_extracto,doc_extracto,
           , zuonr"refe_pago,importe_pago,texto_banco
      INTO TABLE @DATA(tl_liquida)
      FROM acdoca
      WHERE rldnr  = '0L'               AND
            rbukrs IN @p_bukrs          AND
            gjahr  = @p_gjahr           AND
            blart  IN @rg_ti_doc_liq    AND
            augbl  NE ''     AND
            belnr IN @s_belnr.   "

    IF sy-subrc = 0.
      SORT tl_liquida BY rldnr rbukrs gjahr belnr.

* valida el documento de compensacion que este en el mes
* ahora buscamos en el documento de compensación que no este anulado
      SELECT bukrs,belnr,gjahr,stblg,bktxt,awkey, budat, monat
        INTO TABLE @DATA(tl_bkpfbr)
        FROM bkpf
        FOR ALL ENTRIES IN @tl_liquida
        WHERE bukrs = @tl_liquida-rbukrs AND
              belnr = @tl_liquida-augbl AND
              monat  NOT IN @p_monat AND
              gjahr = @tl_liquida-gjahr.

      LOOP AT tl_bkpfbr ASSIGNING FIELD-SYMBOL(<fs_bkpfbr>).
        DELETE tl_liquida WHERE rbukrs  = <fs_bkpfbr>-bukrs AND
                                gjahr  = <fs_bkpfbr>-gjahr AND
                                augbl  = <fs_bkpfbr>-belnr.
      ENDLOOP.


* con esto ahora borramos las entradas
* determina las posiciones de valores de pago real
      "Busqueda a la tabla bseg por cada documento de pago sus datos descompone el pago en sus partidas
      SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,dmbtr, sgtxt,augdt,bschl,zuonr,shkzg,xref3,zlsch
        INTO TABLE @DATA(tl_det_pag)
        FROM bseg
        FOR ALL ENTRIES IN @tl_liquida
        WHERE bukrs =  @tl_liquida-rbukrs AND
              belnr =  @tl_liquida-augbl  AND
              gjahr =  @tl_liquida-gjahr  AND
              bschl =  @tl_liquida-bschl  AND
              hkont =  @tl_liquida-racct.



* Investiga si la liquidación que esta compensada esta anulada en ese caso la descarga
      "Cada uno de estos pagos se deberá validar en la tabla pago de la tabla BKPF
      SELECT bukrs,belnr,gjahr,stblg,bktxt,awkey, monat
        INTO TABLE @DATA(tl_bkpf2)
        FROM bkpf
        FOR ALL ENTRIES IN @tl_liquida
        WHERE bukrs = @tl_liquida-rbukrs AND
              belnr = @tl_liquida-belnr AND
              gjahr = @tl_liquida-gjahr.
* Se procede a eliminar el documento de liquidación si esta anulado

      LOOP AT  tl_bkpf2  ASSIGNING FIELD-SYMBOL(<fs_bkpf2>) WHERE stblg = abap_on.
        DELETE tl_liquida WHERE rbukrs  = <fs_bkpf2>-bukrs AND
                                gjahr  = <fs_bkpf2>-gjahr AND
                                belnr  = <fs_bkpf2>-belnr.
      ENDLOOP.


* ahora buscamos en el documento de compensación que no este anulado
      SELECT bukrs,belnr,gjahr,stblg,bktxt,awkey, budat, monat
        INTO TABLE @DATA(tl_bkpf3)
        FROM bkpf
        FOR ALL ENTRIES IN @tl_liquida
        WHERE bukrs = @tl_liquida-rbukrs AND
              belnr = @tl_liquida-augbl AND
              gjahr = @tl_liquida-gjahr.
      IF sy-subrc = 0.
        LOOP AT  tl_bkpf3  ASSIGNING FIELD-SYMBOL(<fs_bkpf3>) WHERE stblg = abap_on.
          DELETE tl_liquida WHERE rbukrs  = <fs_bkpf3>-bukrs AND
                                  gjahr   = <fs_bkpf3>-gjahr AND
                                  augbl   = <fs_bkpf3>-belnr.
        ENDLOOP.

      ENDIF.

* ahora se procede a realizar la extracción de las partes del documento

*detalle de docto de liquidación

* solo dejara los documentos que pertenecen al mes de pago
* Se procede a eliminar el documento de liquidación si esta anulado


      IF tl_liquida IS NOT  INITIAL.

        "1.2.     Búsqueda de doctos KZ para sociedad, periodo, ejercicio busca pagos
        SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
               kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
               augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt,linetype "fecha_extracto,doc_extracto,
               , zuonr"refe_pago,importe_pago,texto_banco
          FROM acdoca
          INTO TABLE @DATA(tl_liquida_det)
          FOR ALL ENTRIES IN @tl_liquida
          WHERE rldnr  = '0L'                AND
                rbukrs = @tl_liquida-rbukrs  AND
                gjahr  = @tl_liquida-gjahr   AND
                belnr  = @tl_liquida-belnr.
        IF sy-subrc = 0.
          SORT tl_liquida_det BY rldnr rbukrs gjahr belnr.
          DATA(tl_liquida_det_acree_pag)   = tl_liquida_det.
          DATA(tl_liquida_det_acree_pag_sc) = tl_liquida_det.
          DATA(tl_liquida_det_banco)       = tl_liquida_det.
          DATA(tl_liquida_det_nomina)      = tl_liquida_det.
          DELETE tl_liquida_det_acree_pag  WHERE bschl NE '21'.
          DELETE tl_liquida_det_acree_pag_sc  WHERE bschl NE '31'.
          DELETE tl_liquida_det_banco      WHERE racct NOT  IN rg_hkont .
          DELETE tl_liquida_det_nomina     WHERE racct NOT IN rg_resumen_nomina.
          DATA(lv_lines)  = lines( tl_liquida_det_banco ).
          IF lv_lines GT 0.
            DATA(lv_lines2)  = lines( tl_liquida_det_nomina  ).
            IF lv_lines2 GT 0.
* Existen datos de pago
* Se procesa el detalle de la nomina
              DATA(tl_liquida_det_nomina_cargo) =   tl_liquida_det_nomina.
              DATA(tl_liquida_det_nomina_abono) =   tl_liquida_det_nomina.
              DELETE tl_liquida_det_nomina_cargo WHERE  drcrk NE c_s.
              DELETE tl_liquida_det_nomina_abono WHERE  drcrk NE c_h.
              DELETE tl_liquida_det_nomina_cargo WHERE racct IN rg_nomina_exclusion_s.
              DELETE tl_liquida_det_nomina_abono WHERE racct IN rg_nomina_exclusion_h.
* Busca el rfc de proveedor
              SELECT partner,taxtype,taxnum,taxnumxl
                INTO TABLE @DATA(tl_dfkkbptaxnum2)
                FROM dfkkbptaxnum
                FOR ALL ENTRIES IN @tl_liquida_det_acree_pag
                WHERE partner  = @tl_liquida_det_acree_pag-lifnr AND
                      taxtype  = @c_mx1.


              SELECT partner,name_org1,name_org2,name_org3,name_org4,name_last,name_first,natpers
                INTO TABLE @DATA(tl_but0002)
                FROM but000
                FOR ALL ENTRIES IN @tl_liquida_det_acree_pag
                WHERE partner  = @tl_liquida_det_acree_pag-lifnr.
            ENDIF.
          ENDIF.
* se inicia el procesamiento de cada acreedor
          SORT tl_liquida_det_nomina_cargo BY rbukrs belnr gjahr racct.
          SORT tl_liquida_det_nomina_abono BY rbukrs belnr gjahr racct.
          DATA(lv_borrar_indice_cargo) = 0.
          DATA(lv_borrar_indice_abono) = 0.
          DATA(lv_borro_cargos) = 0.
          DATA(lv_borro_abonos) = 0.
** cargos
          LOOP AT tl_liquida_det_banco  ASSIGNING FIELD-SYMBOL(<fs_liquida_acree>).
            CLEAR sl_report." se borra por que es una un documento de pago nuevo

            DATA(tl_liquida_det_nomina_cargo2) =   tl_liquida_det_nomina.
            DATA(tl_liquida_det_nomina_abono2) =   tl_liquida_det_nomina.
            DELETE tl_liquida_det_nomina_cargo2 WHERE  belnr NE <fs_liquida_acree>-belnr AND
                                                       rbukrs =  <fs_liquida_acree>-rbukrs AND
                                                       gjahr = <fs_liquida_acree>-gjahr.

            DELETE tl_liquida_det_nomina_cargo2 WHERE  drcrk  EQ 'H'.
            DELETE tl_liquida_det_nomina_cargo2 WHERE  drcrk  EQ 'K'.

            DELETE tl_liquida_det_nomina_abono2 WHERE  belnr NE <fs_liquida_acree>-belnr AND
                                                       rbukrs =  <fs_liquida_acree>-rbukrs AND
                                                       gjahr = <fs_liquida_acree>-gjahr.

            DELETE tl_liquida_det_nomina_abono2 WHERE  drcrk  EQ 'S'.
            DELETE tl_liquida_det_nomina_abono2 WHERE  drcrk  EQ 'K'.

            DATA: lv_cta_enc TYPE acdoca-racct.
            DO.
              ASSIGN tl_liquida_det_nomina_cargo2[    belnr = <fs_liquida_acree>-belnr  rbukrs =  <fs_liquida_acree>-rbukrs
                                                     gjahr = <fs_liquida_acree>-gjahr ] TO FIELD-SYMBOL(<fs_liqui_det_cargo1>).
              IF sy-subrc = 0.
                lv_borrar_indice_cargo = sy-tabix.
                LOOP AT  tl_liquida_det_nomina_cargo2 ASSIGNING FIELD-SYMBOL(<fs_liqui_det_cargo>) FROM sy-tabix.
                  IF sy-subrc = 0.
                    CLEAR: vl_total.
                    vl_total =  vl_total + <fs_liqui_det_cargo>-tsl.
                    EXIT."sale por que busca otro registro
                  ENDIF.
                ENDLOOP.
              ELSE.
                ADD 1 TO lv_borro_cargos.
                EXIT.
              ENDIF.


              ASSIGN rg_resumen_nomina[ low = <fs_liqui_det_cargo>-racct ] TO FIELD-SYMBOL(<fs_resumen_n2>).
              IF sy-subrc = 0.
                IF <fs_resumen_n2>-high = 'SUBSIDIO AL EMPLEO'.
                  sl_report-subsidio_empleo = sl_report-subsidio_empleo + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'FONDO AHORRO EMPRESA'.
                  sl_report-fondo_ahorro_empresa = sl_report-fondo_ahorro_empresa + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'FONDO AHORRO EMPLEAD'.
                  sl_report-fondo_ahorro_emplead = sl_report-fondo_ahorro_emplead + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'DESCUENTO INFONACOT'.
                  sl_report-descuento_infonacot =  sl_report-descuento_infonacot + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'SUELDOS POR PAGAR'.
                  sl_report-sueldos_por_pagar  =    sl_report-sueldos_por_pagar  + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'IMSS PATRONAL'.
                  sl_report-imss_patronal = sl_report-imss_patronal + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = '2% SAR'.
                  sl_report-2_sar = sl_report-2_sar + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'INFONAVIT PATRONAL'.
                  sl_report-infonavit_patronal = sl_report-infonavit_patronal + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'ING X DEV EMPLEADOS'.
                  sl_report-ing_x_dev_empleados =  sl_report-ing_x_dev_empleados + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'ING X DESCTO COMEDOR'.
                  sl_report-ing_x_descto_comedor =  sl_report-ing_x_descto_comedor + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'IMPULSO ECONOMICO CREA'.
*              sl_report-impulso_economico_crea = sl_report-impulso_economico_crea + vl_total.
                ENDIF.

                IF <fs_resumen_n2>-high = 'FAC ADMVAS-MANIOBRAS'.
                  sl_report-fac_admvas_maniobras = sl_report-fac_admvas_maniobras  + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'FAC ADMVAS-GTS CAMINO'.
                  sl_report-fac_admvas_gts_camino  = sl_report-fac_admvas_gts_camino   + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'FAC ADMVAS-TALACHAS'.
                  sl_report-fac_admvas_gts_talachas  = sl_report-fac_admvas_gts_talachas   + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'FAC ADMVAS-REP MEN'.
                  sl_report-fac_admvas_gts_rep_men  = sl_report-fac_admvas_gts_rep_men   + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'FAC ADMVAS-FIANZAS'.
                  sl_report-fac_admvas_gts_fianzas   = sl_report-fac_admvas_gts_fianzas   + vl_total.
                ENDIF.

                IF <fs_resumen_n2>-high = 'CASETAS'.
                  sl_report-casetas = sl_report-casetas + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'CESANTÍA VEJEZ PATRO'.
                  sl_report-cesantia_vejez_patro = sl_report-cesantia_vejez_patro + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'ISR RET X SUELDOS'.
                  sl_report-isr_ret_x_sueldos = sl_report-isr_ret_x_sueldos + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'OTRAS PRESTACIONES'.
                  sl_report-otras_prestaciones = sl_report-otras_prestaciones + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'OTROS GASTOS VIAJE'.
                  sl_report-otros_gastos_viaje = sl_report-otros_gastos_viaje + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'IMSS RETENIDO'.
                  sl_report-imss_retenido = sl_report-imss_retenido + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'CESANTÍA VEJEZ EMPL'.
                  sl_report-cesantia_vejez_empl = sl_report-cesantia_vejez_empl + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'FINIQUITO POR PAGAR'.
                  sl_report-finiquito_por_pagar = sl_report-finiquito_por_pagar + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'INFONAVIT RETENIDO'.
                  sl_report-infonavit_retenido = sl_report-infonavit_retenido + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'PROV AGUI X PAGAR'.
                  sl_report-prov_agui_x_pagar = sl_report-prov_agui_x_pagar + vl_total.
                ENDIF.

                IF <fs_resumen_n2>-high = 'SUELDOS'.
                  sl_report-sueldos = sl_report-sueldos  + vl_total.
                ENDIF.

                IF <fs_resumen_n2>-high = 'VIATICOS'.
                  sl_report-viaticos = sl_report-viaticos  + vl_total.
                ENDIF.

                IF <fs_resumen_n2>-high = 'TIEMPO EXTRA DOBLE'.
                  sl_report-tiempo_extra_doble = sl_report-tiempo_extra_doble + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'TIEMPO EXTRA TRIPLE'.
                  sl_report-tiempo_extra_triple = sl_report-tiempo_extra_triple + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'DOMINGOS Y DÍAS FEST'.
                  sl_report-domingos_dias_fest = sl_report-domingos_dias_fest + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'PRIMA DOMINICAL'.
                  sl_report-prima_dominical = sl_report-prima_dominical + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'COMPENS EXTRAORDINAR'.
                  sl_report-compens_extraordinar = sl_report-compens_extraordinar + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'PREMIO X PUNTU ASIST'.
                  sl_report-premio_puntualidad = sl_report-premio_puntualidad + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'CUOTA PATRONAL IMSS'.
                  sl_report-cuota_patronal_imss = sl_report-cuota_patronal_imss + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'FONDO DE AHORRO'.
                  sl_report-fondo_ahorro = sl_report-fondo_ahorro + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = '5% INFONAVIT'.
                  sl_report-5_infonavit = sl_report-5_infonavit + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'PRIMA VACACIONAL'.
                  sl_report-prima_vacacional = sl_report-prima_vacacional + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'CUOTA SINDICAL X PAG TOTAL'.
                  sl_report-cuota_sind_x_pagotot = sl_report-cuota_sind_x_pagotot + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'PENSIÓN ALIMENTICIA'.
                  sl_report-pension_alim_total = sl_report-pension_alim_total + vl_total.
                ENDIF.
                IF <fs_resumen_n2>-high = 'SEGURO VIV INFONAVIT'.
                  sl_report-seguro_viv_infon_tot = sl_report-seguro_viv_infon_tot + vl_total.
                ENDIF.
              ENDIF.
              DELETE  tl_liquida_det_nomina_cargo2  INDEX   lv_borrar_indice_cargo.
            ENDDO.
* ahora se agregan los datos del reporte
            DO.


              ASSIGN tl_liquida_det_nomina_abono2[    belnr = <fs_liquida_acree>-belnr  rbukrs =  <fs_liquida_acree>-rbukrs
                                                     gjahr = <fs_liquida_acree>-gjahr ] TO FIELD-SYMBOL(<fs_liqui_det_abono1>).
              IF sy-subrc = 0.
                lv_borrar_indice_abono = sy-tabix.
                LOOP AT  tl_liquida_det_nomina_abono2 ASSIGNING FIELD-SYMBOL(<fs_liqui_det_abono>) FROM sy-tabix.
                  IF sy-subrc = 0.
                    CLEAR: vl_total.
                    vl_total =  vl_total + <fs_liqui_det_abono>-tsl.
                    EXIT."sale por que busca otro registro
                  ENDIF.
                ENDLOOP.
              ELSE.
                ADD 1 TO lv_borro_abonos.
                EXIT.
              ENDIF.

              ASSIGN rg_resumen_nomina[ low = <fs_liqui_det_abono>-racct ] TO FIELD-SYMBOL(<fs_resumen_n3>).
              IF sy-subrc = 0.
                IF <fs_resumen_n3>-high = 'SUBSIDIO AL EMPLEO'.
                  sl_report-subsidio_empleo = sl_report-subsidio_empleo + vl_total.
                ENDIF.

                IF <fs_resumen_n3>-high = 'FONDO AHORRO EMPRESA'.
                  sl_report-fondo_ahorro_empresa = sl_report-fondo_ahorro_empresa + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'FONDO AHORRO EMPLEAD'.
                  sl_report-fondo_ahorro_emplead = sl_report-fondo_ahorro_emplead + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'DESCUENTO INFONACOT'.
                  sl_report-descuento_infonacot =  sl_report-descuento_infonacot + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'SUELDOS POR PAGAR'.
                  sl_report-sueldos_por_pagar  =    sl_report-sueldos_por_pagar  + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'IMSS PATRONAL'.
                  sl_report-imss_patronal = sl_report-imss_patronal + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = '2% SAR'.
                  sl_report-2_sar = sl_report-2_sar + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'INFONAVIT PATRONAL'.
                  sl_report-infonavit_patronal = sl_report-infonavit_patronal + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'ING X DEV EMPLEADOS'.
                  sl_report-ing_x_dev_empleados =  sl_report-ing_x_dev_empleados + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'ING X DESCTO COMEDOR'.
                  sl_report-ing_x_descto_comedor =  sl_report-ing_x_descto_comedor + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'IMPULSO ECONOMICO CREA'.
*              sl_report-impulso_economico_crea = sl_report-impulso_economico_crea + vl_total.
                ENDIF.

                IF <fs_resumen_n3>-high = 'CESANTÍA VEJEZ PATRO'.
                  sl_report-cesantia_vejez_patro = sl_report-cesantia_vejez_patro + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'ISR RET X SUELDOS'.
                  sl_report-isr_ret_x_sueldos = sl_report-isr_ret_x_sueldos + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'OTRAS PRESTACIONES'.
                  sl_report-otras_prestaciones = sl_report-otras_prestaciones + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'OTROS GASTOS VIAJE'.
                  sl_report-otros_gastos_viaje = sl_report-otros_gastos_viaje + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'IMSS RETENIDO'.
                  sl_report-imss_retenido = sl_report-imss_retenido + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'CESANTÍA VEJEZ EMPL'.
                  sl_report-cesantia_vejez_empl = sl_report-cesantia_vejez_empl + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'FINIQUITO POR PAGAR'.
                  sl_report-finiquito_por_pagar = sl_report-finiquito_por_pagar + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'INFONAVIT RETENIDO'.
                  sl_report-infonavit_retenido = sl_report-infonavit_retenido + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'PROV AGUI X PAGAR'.
                  sl_report-prov_agui_x_pagar = sl_report-prov_agui_x_pagar + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'SUELDOS'.
                  sl_report-sueldos = sl_report-sueldos  + vl_total.
                ENDIF.

                IF <fs_resumen_n3>-high = 'VIATICOS'.
                  sl_report-viaticos = sl_report-viaticos  + vl_total.
                ENDIF.

                IF <fs_resumen_n3>-high = 'TIEMPO EXTRA DOBLE'.
                  sl_report-tiempo_extra_doble = sl_report-tiempo_extra_doble + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'TIEMPO EXTRA TRIPLE'.
                  sl_report-tiempo_extra_triple = sl_report-tiempo_extra_triple + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'DOMINGOS Y DÍAS FEST'.
                  sl_report-domingos_dias_fest = sl_report-domingos_dias_fest + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'PRIMA DOMINICAL'.
                  sl_report-prima_dominical = sl_report-prima_dominical + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'COMPENS EXTRAORDINAR'.
                  sl_report-compens_extraordinar = sl_report-compens_extraordinar + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'PREMIO X PUNTU ASIST'.
                  sl_report-premio_puntualidad = sl_report-premio_puntualidad + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'CUOTA PATRONAL IMSS'.
                  sl_report-cuota_patronal_imss = sl_report-cuota_patronal_imss + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'FONDO DE AHORRO'.
                  sl_report-fondo_ahorro = sl_report-fondo_ahorro + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = '5% INFONAVIT'.
                  sl_report-5_infonavit = sl_report-5_infonavit + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'PRIMA VACACIONAL'.
                  sl_report-prima_vacacional = sl_report-prima_vacacional + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'CUOTA SINDICAL X PAG TOTAL'.
                  sl_report-cuota_sind_x_pagotot = sl_report-cuota_sind_x_pagotot + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'PENSIÓN ALIMENTICIA'.
                  sl_report-pension_alim_total = sl_report-pension_alim_total + vl_total.
                ENDIF.
                IF <fs_resumen_n3>-high = 'SEGURO VIV INFONAVIT'.
                  sl_report-seguro_viv_infon_tot = sl_report-seguro_viv_infon_tot + vl_total.
                ENDIF.
              ELSE.
                ADD 1 TO lv_borro_abonos.
              ENDIF.
              DELETE   tl_liquida_det_nomina_abono2 INDEX  lv_borrar_indice_abono.
            ENDDO.
* se comienza a llenar la tabla de reporte
            IF p_pag_c IS NOT INITIAL.
              sl_report-semaforo = '@08@'.
            ELSE.
              sl_report-semaforo = '@09@'.
            ENDIF.

            READ TABLE tl_bkpf3  INTO  DATA(wa_cobro) WITH KEY
                                            bukrs  =  <fs_liquida_acree>-rbukrs
                                            gjahr   = <fs_liquida_acree>-gjahr
                                            belnr   = <fs_liquida_acree>-augbl.
            IF sy-subrc = 0.
* Datos generales de pago
              sl_report-fechapago            = wa_cobro-budat.
              sl_report-n_doc_pago           = wa_cobro-belnr.
              sl_report-n_doc_pago_sociedad  = wa_cobro-bukrs.
              sl_report-n_doc_pago_ejercicio = wa_cobro-gjahr.
              "Sociedad
              sl_report-bukrs = wa_cobro-bukrs.
              "periodo
              sl_report-periodo =  wa_cobro-budat+4(2).
              "ejercicio
              sl_report-gjahr   =  wa_cobro-gjahr.


              ASSIGN tl_det_pag[ belnr  = <fs_liquida_acree>-augbl gjahr  = <fs_liquida_acree>-gjahr bukrs  = <fs_liquida_acree>-rbukrs ] TO FIELD-SYMBOL(<fs_det_pago>).
              IF sy-subrc = 0.
                IF <fs_det_pago>-dmbtr LT 0.
                  sl_report-totalcargos  = <fs_det_pago>-dmbtr * -1.
                ELSE.
                  sl_report-totalcargos  = <fs_det_pago>-dmbtr.
                ENDIF.
              ENDIF.
              sl_report-referencia =  wa_cobro-awkey.
              sl_report-concepto   =  wa_cobro-bktxt.
              sl_report-referencia =  wa_cobro-awkey.
* datos de liquidación
              sl_report-n_sol_anticipo           =  <fs_liquida_acree>-belnr.
              sl_report-n_sol_anticipo_sociedad  =  <fs_liquida_acree>-rbukrs.
              sl_report-n_sol_anticipo_ejercicio =   <fs_liquida_acree>-gjahr.
* datos de rfc
              "RFC
              ASSIGN tl_liquida_det_acree_pag[  rbukrs  =  <fs_liquida_acree>-rbukrs gjahr   = <fs_liquida_acree>-gjahr
              belnr   = <fs_liquida_acree>-belnr ]  TO FIELD-SYMBOL(<fs_acree_liq>).
              IF sy-subrc = 0.
                "RFC

                ASSIGN tl_dfkkbptaxnum2[ partner = <fs_acree_liq>-lifnr ] TO FIELD-SYMBOL(<fs_dfkkbptaxnum1>).
                IF sy-subrc = 0.
                  IF <fs_dfkkbptaxnum1>-taxnumxl IS NOT INITIAL.
                    sl_report-rfc = <fs_dfkkbptaxnum1>-taxnumxl.
                  ELSE.
                    sl_report-rfc = <fs_dfkkbptaxnum1>-taxnum.
                  ENDIF.
                  TRANSLATE sl_report-rfc TO UPPER CASE.
                ENDIF.

                "RAZON SOCIAL
                ASSIGN tl_but0002[ partner = <fs_acree_liq>-lifnr ] TO FIELD-SYMBOL(<fs_but0001>).
                IF sy-subrc = 0.
                  IF <fs_but0001>-natpers IS INITIAL.
                    IF <fs_but0001>-name_org1 IS NOT INITIAL.
                      CONCATENATE <fs_but0001>-name_org1 <fs_but0001>-name_org2 <fs_but0001>-name_org3 <fs_but0001>-name_org4 INTO sl_report-razon_s SEPARATED BY space.
                    ELSE.
                      CONCATENATE <fs_but0001>-name_last <fs_but0001>-name_first INTO sl_report-razon_s SEPARATED BY space.
                    ENDIF.
                  ELSE.
                    IF <fs_but0001>-name_last IS NOT INITIAL.
                      CONCATENATE <fs_but0001>-name_last <fs_but0001>-name_first INTO sl_report-razon_s SEPARATED BY space.
                    ELSE.
                      CONCATENATE <fs_but0001>-name_org1 <fs_but0001>-name_org2 <fs_but0001>-name_org3 <fs_but0001>-name_org4 INTO sl_report-razon_s SEPARATED BY space.
                    ENDIF.
                  ENDIF.
                ENDIF.
                "Acredor
                sl_report-acredor = <fs_acree_liq>-lifnr.

                CASE <fs_liquida_acree>-blart.
                  WHEN 'LQ'.
                    sl_report-clasificacion   = 'Liquidación de viaje'.

                ENDCASE.
                "base no objeto

                IF <fs_liquida_acree>-tsl LT 0.
                  <fs_liquida_acree>-tsl = <fs_liquida_acree>-tsl * -1.
                ENDIF.

                sl_report-base_no_obj    =  <fs_liquida_acree>-tsl.
                " total
                sl_report-total          = <fs_liquida_acree>-tsl.
                " se agrega a la tabla

                tl_report  = VALUE  #(
                                          BASE tl_report          (  sl_report  )   ).
                CLEAR:sl_report.
*            sl_report-totalcargos, sl_report-base_no_obj, sl_report-total, sl_report-impor_mon_l_fac,
*            sl_report-n_sol_anticipo,sl_report-n_sol_anticipo_ejercicio,sl_report-n_sol_anticipo_sociedad,sl_report-base_16,sl_report-iva_16.
              ELSE.
                ASSIGN tl_liquida_det_acree_pag_sc[  rbukrs  =  <fs_liquida_acree>-rbukrs gjahr   = <fs_liquida_acree>-gjahr
                belnr   = <fs_liquida_acree>-belnr ]  TO FIELD-SYMBOL(<fs_acree_scliq>).
                IF sy-subrc  = 0.
                  "RFC

                  ASSIGN tl_dfkkbptaxnum2[ partner = <fs_acree_scliq>-lifnr ] TO FIELD-SYMBOL(<fs_dfkkbptaxnum2>).
                  IF sy-subrc = 0.
                    IF <fs_dfkkbptaxnum2>-taxnumxl IS NOT INITIAL.
                      sl_report-rfc = <fs_dfkkbptaxnum2>-taxnumxl.
                    ELSE.
                      sl_report-rfc = <fs_dfkkbptaxnum2>-taxnum.
                    ENDIF.
                    TRANSLATE sl_report-rfc TO UPPER CASE.
                  ENDIF.


                  "RAZON SOCIAL
                  ASSIGN tl_but0002[ partner = <fs_acree_scliq>-lifnr ] TO FIELD-SYMBOL(<fs_but0002>).
                  IF sy-subrc = 0.
                    IF <fs_but0002>-natpers IS INITIAL.
                      IF <fs_but0002>-name_org1 IS NOT INITIAL.
                        CONCATENATE <fs_but0002>-name_org1 <fs_but0002>-name_org2 <fs_but0002>-name_org3 <fs_but0002>-name_org4 INTO sl_report-razon_s SEPARATED BY space.
                      ELSE.
                        CONCATENATE <fs_but0002>-name_last <fs_but0002>-name_first INTO sl_report-razon_s SEPARATED BY space.
                      ENDIF.
                    ELSE.
                      IF <fs_but0002>-name_last IS NOT INITIAL.
                        CONCATENATE <fs_but0002>-name_last <fs_but0002>-name_first INTO sl_report-razon_s SEPARATED BY space.
                      ELSE.
                        CONCATENATE <fs_but0002>-name_org1 <fs_but0002>-name_org2 <fs_but0002>-name_org3 <fs_but0002>-name_org4 INTO sl_report-razon_s SEPARATED BY space.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                  "Acredor
                  sl_report-acredor = <fs_acree_scliq>-lifnr.

                  CASE <fs_liquida_acree>-blart.
                    WHEN 'LQ'.
                      sl_report-clasificacion   = 'Liquidación de viaje'.

                  ENDCASE.
                  "base no objeto

                  IF <fs_liquida_acree>-tsl LT 0.
                    <fs_liquida_acree>-tsl = <fs_liquida_acree>-tsl * -1.
                  ENDIF.

                  sl_report-base_no_obj    =  <fs_liquida_acree>-tsl.
                  " total
                  sl_report-total          = <fs_liquida_acree>-tsl.
                  " se agrega a la tabla

                  SELECT bukrs,belnr,gjahr,stblg,bktxt,awkey, monat
                                  FROM bkpf     INTO TABLE @DATA(tl_bkpf5)
                                  WHERE bukrs = @sl_report-bukrs AND
                                        belnr = @sl_report-n_doc_pago AND
                                        monat NOT IN @p_monat AND
                                        gjahr = @sl_report-gjahr.

                  IF sy-subrc = 0.
                    LOOP AT tl_bkpf5  ASSIGNING FIELD-SYMBOL(<fs_bkpf5>).
                      CLEAR sl_report.
                    ENDLOOP.
                  ENDIF.
                  IF sl_report  IS NOT INITIAL.
                    tl_report  = VALUE  #(
                                              BASE tl_report          (  sl_report  )   ).
                  ENDIF.
                  CLEAR:sl_report.
* Investiga si la liquidación que esta compensada esta anulada en ese caso la descarga
                  "Cada uno de estos pagos se deberá validar en la tabla pago de la tabla BKPF
                ENDIF.
              ENDIF.
            ELSE.
              CLEAR:sl_report.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
*-->  Insert AMP 06.01.2021
*    SORT tl_report BY n_doc_pago n_documento_fac.
    "1.2.1 Ahora buscara  el factoraje


    "1.   Búsqueda de doctos KZ para sociedad, periodo, ejercicio busca pagos
    SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
           kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
           augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt, linetype "fecha_extracto,doc_extracto,
           , zuonr"refe_pago,importe_pago,texto_banco
      INTO TABLE @DATA(tl_pagos_factoraje)
      FROM acdoca
      WHERE rldnr  = '0L'         AND
            rbukrs IN @p_bukrs    AND
            gjahr  = @p_gjahr     AND
            blart   IN @rg_blart  AND
            poper  IN @p_monat    AND
            augbl  IN @rg_augbl   AND
            belnr IN @s_belnr     AND  "Insert AMP 22.12.2020
            lifnr IN @rg_lifnr.
    IF sy-subrc = 0.
* Determina primero determina que no este anulado el pago en BKPF
      SELECT bukrs,belnr,gjahr,stblg,bktxt,awkey
      INTO TABLE @DATA(tl_cab_fact)
      FROM bkpf
      FOR ALL ENTRIES IN @tl_pagos_factoraje
      WHERE bukrs = @tl_pagos_factoraje-rbukrs AND
            belnr = @tl_pagos_factoraje-belnr AND
            gjahr = @tl_pagos_factoraje-gjahr.
* Se borran aquellos casos en que el pago esta anulado
      IF p_pag_c IS NOT INITIAL."
        LOOP AT  tl_cab_fact ASSIGNING FIELD-SYMBOL(<fs_cab_facto>) WHERE stblg = abap_on.
          DELETE tl_pagos_factoraje  WHERE rbukrs = <fs_cab_facto>-bukrs AND
                                        gjahr  = <fs_cab_facto>-gjahr AND
                                        belnr  = <fs_cab_facto>-belnr.

          DELETE tl_cab_fact WHERE    bukrs = <fs_cab_facto>-bukrs AND
                                        gjahr  = <fs_cab_facto>-gjahr AND
                                        belnr  = <fs_cab_facto>-belnr.
        ENDLOOP.
        " Ahora validamos que tenga un extracto de cuenta y que no este anulado
      ELSE.
      ENDIF.

      SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
    INTO TABLE @DATA(tl_det_pag_fac)
    FROM bseg
    FOR ALL ENTRIES IN @tl_cab_fact
    WHERE bukrs =  @tl_cab_fact-bukrs AND
          belnr =  @tl_cab_fact-belnr  AND
          gjahr =  @tl_cab_fact-gjahr  AND
           hkont  IN @rg_hkont  . "  Busca solo factoraje y para su extracto
      IF sy-subrc = 0.
        IF p_pag_c IS NOT INITIAL. " si es con extracto
* Si es con validación extracto se borra los que esten sin compensar en la posición del banco
          SELECT bukrs,belnr,gjahr,stblg,bktxt,awkey
    INTO TABLE @DATA(tl_extra_anu)
    FROM bkpf
    FOR ALL ENTRIES IN @tl_det_pag_fac
    WHERE bukrs = @tl_det_pag_fac-bukrs AND
          belnr = @tl_det_pag_fac-augbl AND
          gjahr = @tl_det_pag_fac-gjahr.
          IF sy-subrc = 0.
            LOOP AT  tl_extra_anu ASSIGNING FIELD-SYMBOL(<fs_extra_anu>) WHERE stblg = abap_on.


* busca el pago3232
              ASSIGN tl_det_pag_fac[ bukrs  = <fs_extra_anu>-bukrs
                                     gjahr  = <fs_extra_anu>-gjahr
                                     augbl  = <fs_extra_anu>-belnr  ] TO FIELD-SYMBOL(<fs_det_pag_anu>).
              IF sy-subrc = 0.
                DELETE tl_extra_anu  WHERE belnr = <fs_extra_anu>-belnr.
              ENDIF.

              ASSIGN tl_pagos_factoraje[ rbukrs  = <fs_extra_anu>-bukrs
                                             gjahr  = <fs_extra_anu>-gjahr
                                             augbl  = <fs_extra_anu>-belnr  ] TO FIELD-SYMBOL(<fs__pag_anu>).
              IF sy-subrc = 0.
                DELETE tl_pagos_factoraje INDEX sy-tabix.
                DELETE tl_extra_anu  WHERE belnr = <fs_extra_anu>-belnr.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ELSE.

        ENDIF." tipo de validacion de extracto

      ELSE. "si no encuentra detalle de extracto
      ENDIF." busca detalle de extracto

      SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
              kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
              augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt,linetype "fecha_extracto,doc_extracto,
              , zuonr
              "refe_pago,importe_pago,texto_banco
         INTO TABLE @DATA(tl_facturas_factoraje)
         FROM acdoca
         FOR ALL ENTRIES IN @tl_pagos_factoraje
         WHERE rldnr  = '0L'           AND
               rbukrs = @tl_pagos_factoraje-rbukrs AND
               gjahr  = @tl_pagos_factoraje-gjahr  AND
               blart  IN @rg_factoraje_egreso_cd   AND
               augbl  = @tl_pagos_factoraje-belnr  AND
               bschl  IN @rg_cargo_abono_compro_low.  "Insert AMP 14.01.2021
* Este es la factura en la posicion de acreedor con IVA o sin el incluido de debería leer para el reporte final
      IF sy-subrc = 0.
* valida que la factura no este anulada
        SELECT bukrs,belnr,gjahr,stblg,bktxt,awkey
  INTO TABLE @DATA(fact_anuladas)
  FROM bkpf
  FOR ALL ENTRIES IN @tl_facturas_factoraje
  WHERE bukrs = @tl_facturas_factoraje-rbukrs AND
  belnr = @tl_facturas_factoraje-augbl AND
  gjahr = @tl_facturas_factoraje-gjahr.

        IF sy-subrc = 0.
          LOOP AT  fact_anuladas ASSIGNING FIELD-SYMBOL(<fs_fac_anu>) WHERE stblg = abap_on.

            ASSIGN tl_facturas_factoraje[  rbukrs  = <fs_extra_anu>-bukrs
                                    gjahr  = <fs_extra_anu>-gjahr
                                    belnr  = <fs_extra_anu>-belnr  ] TO FIELD-SYMBOL(<fs_factuas_fact>).
            IF sy-subrc = 0.

              ASSIGN tl_det_pag_fac[ bukrs  = <fs_factuas_fact>-rbukrs
                                 gjahr  = <fs_factuas_fact>-gjahr
                                 augbl  = <fs_factuas_fact>-augbl  ] TO FIELD-SYMBOL(<fs_det_pag_anuf>).
              IF sy-subrc = 0.
                DELETE tl_det_pag_fac  WHERE bukrs  = <fs_factuas_fact>-rbukrs AND
                                     gjahr  = <fs_factuas_fact>-gjahr AND
                                     belnr  = <fs_factuas_fact>-augbl.
              ENDIF.

              ASSIGN tl_pagos_factoraje[ rbukrs     = <fs_factuas_fact>-rbukrs
                                             gjahr  = <fs_factuas_fact>-gjahr
                                             augbl  = <fs_factuas_fact>-augbl  ] TO FIELD-SYMBOL(<fs__pag_anu_fa>).
              IF sy-subrc = 0.
                DELETE tl_det_pag_fac  WHERE  bukrs  = <fs_factuas_fact>-rbukrs          AND
                                              gjahr  = <fs_factuas_fact>-gjahr    AND
                                              augbl  = <fs_factuas_fact>-augbl.
              ENDIF.
              DELETE tl_facturas_factoraje WHERE rbukrs  = <fs_extra_anu>-bukrs AND
                                   gjahr                 = <fs_extra_anu>-gjahr AND
                                   belnr                 = <fs_extra_anu>-belnr.


            ENDIF.

            DELETE   fact_anuladas WHERE belnr =   <fs_extra_anu>-belnr.
          ENDLOOP.
        ENDIF. " fin de busqueda de factura anuladas
      ENDIF. " fin de facturas

* Llena tablas auxiliares


* Busca el rfc de proveedor
      SELECT partner,taxtype,taxnum,taxnumxl
        INTO TABLE @DATA(tl_dfkkbptaxnum3)
        FROM dfkkbptaxnum
        FOR ALL ENTRIES IN @tl_pagos_factoraje
        WHERE partner  = @tl_pagos_factoraje-lifnr AND
              taxtype  = @c_mx1.

* datos de acreedor
      SELECT partner,name_org1,name_org2,name_org3,name_org4,name_last,name_first,natpers
        INTO TABLE @DATA(tl_but0003)
        FROM but000
        FOR ALL ENTRIES IN @tl_pagos_factoraje
        WHERE partner  = @tl_pagos_factoraje-lifnr.

* detalle de facturas


      SELECT land1,zlsch,text1
  INTO TABLE @DATA(tl_t042z2)
  FROM t042z
  FOR ALL ENTRIES IN @tl_det_pag_fac
  WHERE land1 = @c_mx             AND
     zlsch = @tl_det_pag_fac-zlsch.

      SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
              kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
              augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt,linetype "fecha_extracto,doc_extracto,
              , zuonr
              "refe_pago,importe_pago,texto_banco
         INTO TABLE @DATA(tl_deta_fact_facto)
         FROM acdoca
         FOR ALL ENTRIES IN @tl_facturas_factoraje
         WHERE rldnr  = '0L'           AND
               rbukrs = @tl_facturas_factoraje-rbukrs AND
               gjahr  = @tl_facturas_factoraje-gjahr AND
               belnr  = @tl_facturas_factoraje-belnr.


* primero recupera la posición de pago
      DATA(tl_fac_pag_total) =  tl_deta_fact_facto.
      DELETE tl_fac_pag_total WHERE bschl NE 31.


*** Inicia ciclo de pagos
      SORT tl_pagos_factoraje BY rbukrs gjahr belnr ASCENDING.

      LOOP AT  tl_pagos_factoraje ASSIGNING   FIELD-SYMBOL(<fs_fac_pagos>).
        LOOP AT tl_facturas_factoraje  ASSIGNING   FIELD-SYMBOL(<fs_facturas_pr>) WHERE augbl = <fs_fac_pagos>-belnr.

* fin de procesamiento
** lee el detalle
          DATA(tl_fac_detalle) =  tl_deta_fact_facto.
          DELETE tl_fac_detalle WHERE         belnr  NE <fs_facturas_pr>-belnr   AND
                                              rbukrs = <fs_fac_pagos>-rbukrs  AND
                                              gjahr  = <fs_fac_pagos>-gjahr.
          DATA(tl_det_fac_pag) =   tl_fac_detalle.
          DELETE tl_det_fac_pag WHERE bschl NE 21 .


          DATA(tl_det_fac_0%)      =   tl_fac_detalle.
          DELETE tl_det_fac_0% WHERE mwskz NOT IN rg_ziva_0..

          DATA(tl_det_fac_8%)      =   tl_fac_detalle.
          DELETE tl_det_fac_8% WHERE mwskz  NOT IN rg_ziva_8_high..

          DATA(tl_det_fac_16iva)    =   tl_fac_detalle.
          DELETE tl_det_fac_16iva WHERE mwskz  NE c_w1.

          DATA(tl_det_fac_rtiva_6)     =   tl_fac_detalle.
          DELETE tl_det_fac_rtiva_6 WHERE  racct NOT IN rg_zret_iva_ret_sub_fletes.


          DATA(tl_det_fac_rtiva_4)     =   tl_fac_detalle.
          DELETE tl_det_fac_rtiva_4 WHERE  racct NOT IN rg_zret_iva_ret_sub_contra.

          DATA(tl_det_fac_rtiva_subct) =   tl_fac_detalle.
          DELETE tl_det_fac_rtiva_subct WHERE  racct NOT IN rg_zret_iva_ret_sub_contra.

          DATA(tl_det_fac_rtiva_hono)  =   tl_fac_detalle.
          DELETE tl_det_fac_rtiva_hono WHERE   racct NOT IN rg_zret_iva_ret_sub_honorarios.

          DATA(tl_det_fac_rtiva_arre)  =   tl_fac_detalle.
          DELETE tl_det_fac_rtiva_arre WHERE racct NOT IN rg_zret_iva_ret_sub_arren.

          DATA(tl_det_fac_rtisr_hono)  =   tl_fac_detalle.
          DELETE tl_det_fac_rtisr_hono WHERE racct NOT IN  rg_zret_ret_isr_honorarios.

          DATA(tl_det_fac_rtsir_arre)  =   tl_fac_detalle.
          DELETE tl_det_fac_rtsir_arre WHERE racct NOT IN  rg_zret_ret_isr_arrendamiento.

          DATA(tl_det_fac_ret_cedular)  =   tl_fac_detalle.
          DELETE tl_det_fac_ret_cedular WHERE racct NOT IN   rg_imp_ret_cedular.

          DATA(tl_det_fac_no_obj)  =   tl_fac_detalle.
          DELETE tl_det_fac_no_obj WHERE mwskz IS NOT  INITIAL.

* inicia el procesamiento de datos
          CLEAR sl_report.

* se comienza a llenar la tabla de reporte
          IF p_pag_c IS NOT INITIAL.
            sl_report-semaforo = '@08@'.
          ELSE.
            sl_report-semaforo = '@09@'.
          ENDIF.
* Datos generales de pago
          sl_report-fechapago            = <fs_fac_pagos>-budat.
          sl_report-n_doc_pago           = <fs_fac_pagos>-belnr.
          sl_report-n_doc_pago_sociedad  = <fs_fac_pagos>-rbukrs.
          sl_report-n_doc_pago_ejercicio = <fs_fac_pagos>-gjahr.
          "Sociedad
          sl_report-bukrs = <fs_fac_pagos>-rbukrs.
          "periodo
          sl_report-periodo = <fs_fac_pagos>-budat+4(2).
          "ejercicio
          sl_report-gjahr   =  <fs_fac_pagos>-gjahr.


          IF <fs_fac_pagos>-hsl LT 0.
            sl_report-totalcargos  = <fs_fac_pagos>-hsl * -1.
          ELSE.
            sl_report-totalcargos  = <fs_fac_pagos>-hsl.
          ENDIF.


          sl_report-referencia =   <fs_fac_pagos>-awref.
          sl_report-concepto   =   <fs_fac_pagos>-sgtxt.
          sl_report-referencia =  <fs_fac_pagos>-zuonr.

          ASSIGN tl_dfkkbptaxnum3[ partner = <fs_fac_pagos>-lifnr ] TO FIELD-SYMBOL(<fs_dfkkbptaxnum3>).
          IF sy-subrc = 0.
            IF <fs_dfkkbptaxnum3>-taxnumxl IS NOT INITIAL.
              sl_report-rfc = <fs_dfkkbptaxnum3>-taxnumxl.
            ELSE.
              sl_report-rfc = <fs_dfkkbptaxnum3>-taxnum.
            ENDIF.
            TRANSLATE sl_report-rfc TO UPPER CASE.
          ENDIF.


          "RAZON SOCIAL
          ASSIGN tl_but0003[ partner = <fs_fac_pagos>-lifnr ] TO FIELD-SYMBOL(<fs_but0003>).
          IF sy-subrc = 0.
            IF <fs_but0003>-natpers IS INITIAL.
              IF <fs_but0003>-name_org1 IS NOT INITIAL.
                CONCATENATE <fs_but0003>-name_org1 <fs_but0003>-name_org2 <fs_but0003>-name_org3 <fs_but0003>-name_org4 INTO sl_report-razon_s SEPARATED BY space.
              ELSE.
                CONCATENATE <fs_but0003>-name_last <fs_but0003>-name_first INTO sl_report-razon_s SEPARATED BY space.
              ENDIF.
            ELSE.
              IF <fs_but0003>-name_last IS NOT INITIAL.
                CONCATENATE <fs_but0003>-name_last <fs_but0003>-name_first INTO sl_report-razon_s SEPARATED BY space.
              ELSE.
                CONCATENATE <fs_but0003>-name_org1 <fs_but0003>-name_org2 <fs_but0003>-name_org3 <fs_but0003>-name_org4 INTO sl_report-razon_s SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDIF.

          sl_report-clasificacion   = <fs_facturas_pr>-sgtxt.

          "Acredor
          sl_report-acredor =  <fs_fac_pagos>-lifnr.

          sl_report-n_documento_fac           = <fs_facturas_pr>-belnr.
          sl_report-n_documento_fac_ejercicio = <fs_facturas_pr>-gjahr.
          sl_report-n_documento_fac_sociedad  = <fs_facturas_pr>-rbukrs.

          "base no objeto
          ASSIGN tl_det_fac_no_obj[ rbukrs            = <fs_facturas_pr>-rbukrs
                                               gjahr  = <fs_facturas_pr>-gjahr
                                               belnr  = <fs_facturas_pr>-belnr  ] TO FIELD-SYMBOL(<fs__det_pag_re>).
          IF sy-subrc = 0.

            IF <fs_facturas_pr>-tsl LT 0.
              sl_report-impor_mon_l_fac           =  <fs_facturas_pr>-tsl * -1.
            ELSE.
              sl_report-impor_mon_l_fac           =  <fs_facturas_pr>-tsl.
            ENDIF.

            sl_report-base_no_obj    =  sl_report-impor_mon_l_fac.
          ENDIF.

          " total
          sl_report-total          = sl_report-base_no_obj.
          " se agrega a la tabla

          IF sl_report  IS NOT INITIAL.
            tl_report  = VALUE  #(
                                      BASE tl_report          (  sl_report  )   ).
          ENDIF.
          CLEAR:sl_report.

********************************
          DELETE tl_facturas_factoraje WHERE  belnr  = <fs_facturas_pr>-belnr  AND
                                              rbukrs = <fs_fac_pagos>-rbukrs  AND
                                              gjahr  = <fs_fac_pagos>-gjahr.
        ENDLOOP.

* fin de procesamiento
        DELETE tl_pagos_factoraje WHERE  belnr  = <fs_fac_pagos>-belnr   AND
                                         rbukrs = <fs_fac_pagos>-rbukrs  AND
                                         gjahr  = <fs_fac_pagos>-gjahr.
      ENDLOOP.

** fin ciclos de pagos
    ELSE. " no encuentra doctos de pago para factoraje
    ENDIF." fin de factoraje

    "1.1.3 facturas con iva y retenciones de verdad

*** fin de iva y retenciones  RE no incluye facturas de nóminas
**

    "1.   Búsqueda de doctos KZ para sociedad, periodo, ejercicio busca pagos
    SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
           kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
           augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt, linetype "fecha_extracto,doc_extracto,
           , zuonr"refe_pago,importe_pago,texto_banco
      INTO TABLE @DATA(tl_pagos_fac_re)
      FROM acdoca
      WHERE rldnr  = '0L'         AND
            rbukrs IN @p_bukrs    AND
            gjahr  = @p_gjahr     AND
            blart   IN @rg_blart  AND
            poper  IN @p_monat    AND
            augbl  IN @rg_augbl   AND
            belnr IN @s_belnr     AND  "Insert AMP 22.12.2020
            lifnr NOT IN @rg_lifnr.

    IF sy-subrc = 0.
* Determina primero determina que no este anulado el pago en BKPF
      SELECT bukrs,belnr,gjahr,stblg,bktxt,awkey
      INTO TABLE @DATA(tl_cab_fac_re)
      FROM bkpf
      FOR ALL ENTRIES IN @tl_pagos_fac_re
      WHERE bukrs = @tl_pagos_fac_re-rbukrs AND
            belnr = @tl_pagos_fac_re-belnr AND
            gjahr = @tl_pagos_fac_re-gjahr.
* Se borran aquellos casos en que el pago esta anulado
      IF p_pag_c IS NOT INITIAL."
        LOOP AT  tl_cab_fac_re ASSIGNING FIELD-SYMBOL(<fs_cab_fact_re>) WHERE stblg = abap_on.
          DELETE tl_pagos_fac_re  WHERE rbukrs = <fs_cab_fact_re>-bukrs AND
                                        gjahr  = <fs_cab_fact_re>-gjahr AND
                                        belnr  = <fs_cab_fact_re>-belnr.

          DELETE tl_cab_fac_re    WHERE bukrs = <fs_cab_fact_re>-bukrs AND
                                        gjahr  = <fs_cab_fact_re>-gjahr AND
                                        belnr  = <fs_cab_fact_re>-belnr.
        ENDLOOP.
        " Ahora validamos que tenga un extracto de cuenta y que no este anulado
      ELSE.
      ENDIF.

      SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
    INTO TABLE @DATA(tl_det_pag_fac_re)
    FROM bseg
    FOR ALL ENTRIES IN @tl_cab_fac_re
    WHERE bukrs =  @tl_cab_fac_re-bukrs AND
          belnr =  @tl_cab_fac_re-belnr  AND
          gjahr =  @tl_cab_fac_re-gjahr  AND
          hkont NOT  IN @rg_hkont  . "  Busca solo factoraje y para su extracto
      IF sy-subrc = 0.
        IF p_pag_c IS NOT INITIAL. " si es con extracto
* Si es con validación extracto se borra los que esten sin compensar en la posición del banco
          SELECT bukrs,belnr,gjahr,stblg,bktxt,awkey
    INTO TABLE @DATA(tl_extra_anu_re)
    FROM bkpf
    FOR ALL ENTRIES IN @tl_det_pag_fac_re
    WHERE bukrs = @tl_det_pag_fac_re-bukrs AND
          belnr = @tl_det_pag_fac_re-augbl AND
          gjahr = @tl_det_pag_fac_re-gjahr.
          IF sy-subrc = 0.
            LOOP AT  tl_extra_anu_re ASSIGNING FIELD-SYMBOL(<fs_extra_anu_re>) WHERE stblg = abap_on.


* busca el pago3232
              ASSIGN tl_det_pag_fac_re[ bukrs  = <fs_extra_anu_re>-bukrs
                                     gjahr  = <fs_extra_anu_re>-gjahr
                                     augbl  = <fs_extra_anu_re>-belnr  ] TO FIELD-SYMBOL(<fs_det_pag_anu_re>).
              IF sy-subrc = 0.
                DELETE tl_extra_anu_re  WHERE belnr = <fs_extra_anu_re>-belnr.
              ENDIF.

              ASSIGN tl_pagos_fac_re[ rbukrs  = <fs_extra_anu_re>-bukrs
                                             gjahr  = <fs_extra_anu_re>-gjahr
                                             augbl  = <fs_extra_anu_re>-belnr  ] TO FIELD-SYMBOL(<fs__pag_anu_re>).
              IF sy-subrc = 0.
                DELETE tl_pagos_fac_re INDEX sy-tabix.
                DELETE tl_extra_anu_re  WHERE belnr = <fs_extra_anu_re>-belnr.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ELSE.

        ENDIF." tipo de validacion de extracto

      ELSE. "si no encuentra detalle de extracto
      ENDIF." busca detalle de extracto

      SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
              kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
              augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt,linetype "fecha_extracto,doc_extracto,
              , zuonr
              "refe_pago,importe_pago,texto_banco
         INTO TABLE @DATA(tl_facturas_re)
         FROM acdoca
         FOR ALL ENTRIES IN @tl_pagos_fac_re
         WHERE rldnr  = '0L'           AND
               rbukrs = @tl_pagos_fac_re-rbukrs AND
*             gjahr  = @tl_pagos_fac_re-gjahr  AND    "LCABRERA 27 feb 2022 TICKET R-00021
               AUGGJ  = @tl_pagos_fac_re-gjahr  AND    "LCABRERA 27 feb 2022 TICKET R-00021
               blart  IN @rg_facturas_fiscales_cd   AND
               augbl  = @tl_pagos_fac_re-belnr  AND
               bschl  IN @rg_cargo_abono_compro_low.  "Insert AMP 14.01.2021
* Este es la factura en la posicion de acreedor con IVA o sin el incluido de debería leer para el reporte final
      IF sy-subrc = 0.
* valida que la factura no este anulada
        SELECT bukrs,belnr,gjahr,stblg,bktxt,awkey
  INTO TABLE @DATA(fact_anuladas_re)
  FROM bkpf
  FOR ALL ENTRIES IN @tl_facturas_re
  WHERE bukrs = @tl_facturas_re-rbukrs AND
  belnr = @tl_facturas_re-augbl AND
  gjahr = @tl_facturas_re-gjahr.

        IF sy-subrc = 0.
          LOOP AT  fact_anuladas_re ASSIGNING FIELD-SYMBOL(<fs_fac_anu_re>) WHERE stblg = abap_on.

            ASSIGN tl_facturas_re[  rbukrs  = <fs_extra_anu_re>-bukrs
                                    gjahr  = <fs_extra_anu_re>-gjahr
                                    belnr  = <fs_extra_anu_re>-belnr  ] TO FIELD-SYMBOL(<fs_factuas_fact_re>).
            IF sy-subrc = 0.

              ASSIGN tl_det_pag_fac_re[ bukrs  = <fs_factuas_fact_re>-rbukrs
                                 gjahr  = <fs_factuas_fact_re>-gjahr
                                 augbl  = <fs_factuas_fact_re>-augbl  ] TO FIELD-SYMBOL(<fs_det_pag_anu_re_fs>).
              IF sy-subrc = 0.
                DELETE tl_det_pag_fac  WHERE bukrs  = <fs_factuas_fact_re>-rbukrs AND
                                     gjahr  = <fs_factuas_fact_re>-gjahr AND
                                     belnr  = <fs_factuas_fact_re>-augbl.
              ENDIF.

              ASSIGN tl_pagos_fac_re[ rbukrs     = <fs_factuas_fact_re>-rbukrs
                                             gjahr  = <fs_factuas_fact_re>-gjahr
                                             augbl  = <fs_factuas_fact_re>-augbl  ] TO FIELD-SYMBOL(<fs__pag_anu_fa_fs_re>).
              IF sy-subrc = 0.
                DELETE tl_det_pag_fac_re  WHERE  bukrs  = <fs_factuas_fact_re>-rbukrs          AND
                                              gjahr  = <fs_factuas_fact_re>-gjahr    AND
                                              augbl  = <fs_factuas_fact_re>-augbl.
              ENDIF.
              DELETE tl_facturas_re WHERE rbukrs  = <fs_extra_anu_re>-bukrs AND
                                   gjahr                 = <fs_extra_anu_re>-gjahr AND
                                   belnr                 = <fs_extra_anu_re>-belnr.


            ENDIF.

            DELETE   fact_anuladas_re WHERE belnr =   <fs_extra_anu>-belnr.
          ENDLOOP.
        ENDIF. " fin de busqueda de factura anuladas
      ENDIF. " fin de facturas

* Llena tablas auxiliares


* Busca el rfc de proveedor
      SELECT partner,taxtype,taxnum,taxnumxl
        INTO TABLE @DATA(tl_dfkkbptaxnum4)
        FROM dfkkbptaxnum
        FOR ALL ENTRIES IN @tl_pagos_fac_re
        WHERE partner  = @tl_pagos_fac_re-lifnr AND
              taxtype  = @c_mx1.

* datos de acreedor
      SELECT partner,name_org1,name_org2,name_org3,name_org4,name_last,name_first,natpers
        INTO TABLE @DATA(tl_but0004)
        FROM but000
        FOR ALL ENTRIES IN @tl_pagos_fac_re
        WHERE partner  = @tl_pagos_fac_re-lifnr.

* detalle de facturas




      SELECT rldnr,rbukrs,gjahr,belnr,docln,ryear,aworg,awref,rtcur,racct,
              kokrs,tsl,wsl,budat,bldat,blart,bschl,ktopl,lifnr,hbkid,hktid,
              augdt,augbl,auggj,gkont,rbudget_pd,drcrk,prctr,ktosl,rcntr,koart,mwskz,hsl,sgtxt,linetype "fecha_extracto,doc_extracto,
              , zuonr
              "refe_pago,importe_pago,texto_banco
         INTO TABLE @DATA(tl_deta_fact_facto_re)
         FROM acdoca
         FOR ALL ENTRIES IN @tl_facturas_re
         WHERE rldnr  = '0L'           AND
               rbukrs = @tl_facturas_re-rbukrs AND
               gjahr  = @tl_facturas_re-gjahr AND
               belnr  = @tl_facturas_re-belnr.





*** Inicia ciclo de pagos
      SORT tl_pagos_fac_re BY rbukrs gjahr belnr  docln  ASCENDING.
      SORT tl_facturas_re  BY rbukrs gjahr belnr  docln ASCENDING.
      DELETE ADJACENT DUPLICATES FROM tl_pagos_fac_re COMPARING rbukrs gjahr belnr racct.
      IF sy-subrc = 0.

        SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
    INTO TABLE @DATA(tl_pago_unico)
    FROM bseg
    FOR ALL ENTRIES IN @tl_pagos_fac_re
    WHERE bukrs =  @tl_pagos_fac_re-rbukrs AND
          belnr =  @tl_pagos_fac_re-belnr  AND
          gjahr =  @tl_pagos_fac_re-gjahr  AND
          hkont    IN @rg_hkont  . "  Busca solo factoraje y para su extracto
      ENDIF.

* ARMA TOTALS
      DATA: vl_total2 TYPE bseg-dmbtr,
            vl_total3 TYPE bseg-dmbtr.

      DATA(lt_fac_loc) = tl_facturas_re.
      DELETE ADJACENT DUPLICATES FROM  lt_fac_loc COMPARING rbukrs gjahr belnr racct.
      LOOP AT tl_pagos_fac_re  ASSIGNING   FIELD-SYMBOL(<fs_fac_pagos_re>).

        LOOP AT lt_fac_loc  ASSIGNING   FIELD-SYMBOL(<fs_facturas_pr_re>) WHERE augbl = <fs_fac_pagos_re>-belnr.



          CLEAR: vl_total, vl_total2, vl_total3.
          LOOP AT tl_facturas_re ASSIGNING   FIELD-SYMBOL(<fs_tot_fac>)
           WHERE augbl = <fs_fac_pagos_re>-belnr  AND
                 belnr = <fs_facturas_pr_re>-belnr.
            .
            vl_total   =  vl_total + <fs_tot_fac>-tsl.
            vl_total2  =  vl_total2 + <fs_tot_fac>-wsl.
            vl_total3  =  vl_total3 + <fs_tot_fac>-hsl.


          ENDLOOP.


          LOOP AT tl_facturas_re ASSIGNING  <fs_tot_fac>
           WHERE augbl = <fs_fac_pagos_re>-belnr  AND
                 belnr = <fs_facturas_pr_re>-belnr.

            <fs_tot_fac>-tsl    =   vl_total .
            <fs_tot_fac>-wsl    =   vl_total2 .
            <fs_tot_fac>-hsl    =   vl_total3.
          ENDLOOP.
          CLEAR: vl_total, vl_total2, vl_total3.

        ENDLOOP.
      ENDLOOP.

      DELETE ADJACENT DUPLICATES FROM  tl_facturas_re COMPARING rbukrs gjahr belnr racct.



*** INVESTIGA SI DEBE ENTRAR O NO

      SELECT bukrs,belnr,gjahr,buzei,hkont,augbl,sgtxt,augdt,dmbtr,bschl,zuonr,shkzg,xref3,zlsch
   INTO TABLE @DATA(tl_bseg_nomina2)
   FROM bseg
   FOR ALL ENTRIES IN @tl_facturas_re
   WHERE bukrs  =   @tl_facturas_re-rbukrs     AND
         belnr  =    @tl_facturas_re-belnr    AND
         bschl  = '31'                     AND
         gjahr  =     @tl_facturas_re-gjahr   AND
         h_blart =     'KR'.

      IF sy-subrc = 0.
        LOOP AT tl_bseg_nomina2 ASSIGNING FIELD-SYMBOL(<fs_solonom2>).
          IF <fs_solonom2>-zuonr+0(3) EQ '600'.
            DELETE tl_facturas_re WHERE rbukrs =  <fs_solonom2>-bukrs AND
                                        gjahr  =  <fs_solonom2>-gjahr AND
                                        belnr  =   <fs_solonom2>-belnr.
          ENDIF.
        ENDLOOP.
      ENDIF.


      LOOP AT tl_pagos_fac_re  ASSIGNING  <fs_fac_pagos_re>.
        ASSIGN tl_pago_unico[ bukrs         = <fs_fac_pagos_re>-rbukrs
                                          gjahr  = <fs_fac_pagos_re>-gjahr
                                          belnr  = <fs_fac_pagos_re>-belnr  ] TO FIELD-SYMBOL(<fs_pago_unico>).
        IF sy-subrc = 0.
          <fs_fac_pagos_re>-tsl = <fs_pago_unico>-dmbtr.
        ENDIF.
        LOOP AT tl_facturas_re ASSIGNING   <fs_facturas_pr_re> WHERE augbl = <fs_fac_pagos_re>-belnr.

* fin de procesamiento
** lee el detalle
          DATA(tl_fac_detalle_re) =  tl_deta_fact_facto_re.
          DELETE tl_fac_detalle_re WHERE            belnr  NE <fs_facturas_pr_re>-belnr   AND
                                                    rbukrs = <fs_facturas_pr_re>-rbukrs  AND
                                                    gjahr  = <fs_facturas_pr_re>-gjahr.

* primero recupera la posición de pago con iva del acreedor
          DELETE tl_fac_detalle_re WHERE bschl = '31'.
          DELETE tl_fac_detalle_re WHERE bschl = '21'.
          SORT tl_fac_detalle_re BY rbukrs gjahr belnr  docln  ASCENDING.
          DATA(tl_fac_ctas_fac) =  tl_fac_detalle_re.
          DELETE tl_fac_ctas_fac      WHERE  bschl EQ 31.
          DELETE tl_fac_ctas_fac     WHERE  ktosl EQ  'KBS'.
          DELETE tl_fac_ctas_fac        WHERE  ktosl EQ 'WIT'.
          DELETE tl_fac_ctas_fac       WHERE  racct IN rg_ctas_saldo_cero.
          DELETE tl_fac_ctas_fac       WHERE  ktosl  = 'VST'.

          DATA(tl_fac_ctas) =  tl_fac_ctas_fac.
          SORT tl_fac_ctas BY  rbukrs belnr gjahr racct docln ASCENDING.

          DATA(tl_fac_pag_total_re) =  tl_fac_detalle_re.
          DELETE tl_fac_pag_total_re WHERE bschl NE 31.

          DATA(tl_det_fac_0%_re)      =   tl_fac_detalle_re.
          DELETE tl_det_fac_0%_re WHERE mwskz NOT IN rg_ziva_0..
          DELETE tl_det_fac_0%_re  WHERE bschl  EQ '31'.
          DELETE tl_det_fac_0%_re  WHERE ktosl EQ 'WIT'.

          SORT  tl_det_fac_0%_re  BY  rbukrs belnr gjahr racct docln ASCENDING.


          DATA(tl_det_fac_8%_re)      =   tl_fac_detalle_re.
          DELETE tl_det_fac_8%_re  WHERE mwskz NE 'W3'.
          DELETE tl_det_fac_8%_re  WHERE bschl  EQ '31'.
          DELETE tl_det_fac_8%_re  WHERE ktosl EQ 'WIT'.
          DELETE tl_det_fac_8%_re  WHERE ktosl EQ 'VST'.
          SORT tl_det_fac_8%_re    BY  rbukrs belnr gjahr racct docln ASCENDING.

          DATA(tl_det_fac_bas16_re)    =   tl_fac_detalle_re.
          DELETE tl_det_fac_bas16_re WHERE bschl  EQ '31'.
          DELETE tl_det_fac_bas16_re WHERE  mwskz   NE c_w1.
          DELETE tl_det_fac_bas16_re WHERE  ktosl EQ  'KBS'.
          DELETE tl_det_fac_bas16_re  WHERE ktosl EQ 'VST'.
          DELETE tl_det_fac_bas16_re WHERE ktosl  EQ 'WIT'.

          SORT tl_det_fac_bas16_re BY rbukrs gjahr belnr docln ASCENDING.

          DATA(tl_det_fac_no_obj_re)  =   tl_fac_detalle_re.
          DELETE tl_det_fac_no_obj_re WHERE mwskz NE ''.
          DELETE tl_det_fac_no_obj_re WHERE bschl  EQ '31'.
          DELETE tl_det_fac_no_obj_re WHERE ktosl EQ 'WIT'.
          DELETE tl_det_fac_no_obj_re WHERE ktosl EQ 'VST'.

          SORT tl_det_fac_no_obj_re BY  rbukrs belnr gjahr racct docln ASCENDING.

          DATA(tl_det_fac_no_nd)  =   tl_fac_detalle_re.
          DELETE tl_det_fac_no_nd WHERE mwskz NE 'W8'.
          DELETE tl_det_fac_no_nd  WHERE ktosl EQ 'WIT'.
          DELETE tl_det_fac_no_nd WHERE mwskz NOT IN rg_ziva_nd.
          DELETE tl_det_fac_no_nd WHERE bschl  EQ '31'.

          SORT  tl_det_fac_no_nd BY  rbukrs belnr gjahr racct docln ASCENDING.

          DATA(tl_det_fac_iva_16)     =   tl_fac_detalle_re.
          DELETE tl_det_fac_iva_16 WHERE  racct NOT  IN rg_ziva_16.



          DATA(tl_det_fac_iva_8)         =   tl_fac_detalle_re.
          DELETE tl_det_fac_iva_8 WHERE  mwskz NOT  IN rg_ziva_8_high.
          DELETE tl_det_fac_iva_8  WHERE ktosl NE'VST'.
          DELETE tl_det_fac_iva_8  WHERE ktosl EQ 'WIT'.
          DELETE tl_det_fac_iva_8 WHERE bschl  EQ '31'.

          SORT   tl_det_fac_iva_8  BY  rbukrs belnr gjahr racct docln ASCENDING.


          DATA(tl_det_fac_rtiva_fle)     =   tl_fac_detalle_re.
          DELETE tl_det_fac_rtiva_fle WHERE  racct NOT IN rg_zret_iva_ret_sub_fletes. "fletes 4%

          DATA(tl_det_fac_rtiva_subct_re) =   tl_fac_detalle_re.
          DELETE tl_det_fac_rtiva_subct_re WHERE  racct NOT IN rg_zret_iva_ret_sub_contra.

          DATA(tl_det_fac_rtiva_hono_re)  =   tl_fac_detalle_re.
          DELETE tl_det_fac_rtiva_hono_re WHERE   racct NOT IN rg_zret_iva_ret_sub_honorarios.

          DATA(tl_det_fac_rtiva_arre_re)  =   tl_fac_detalle_re.
          DELETE tl_det_fac_rtiva_arre_re WHERE racct NOT IN rg_zret_iva_ret_sub_arren.

          DATA(tl_det_fac_rtisr_hono_re)  =   tl_fac_detalle_re.
          DELETE tl_det_fac_rtisr_hono_re WHERE racct NOT IN  rg_zret_ret_isr_honorarios.

          DATA(tl_det_fac_rtsir_arre_re)  =   tl_fac_detalle_re.
          DELETE tl_det_fac_rtsir_arre_re  WHERE racct NOT IN  rg_zret_ret_isr_arrendamiento.

          DATA(tl_acdoca_ret_cedular2)  =   tl_fac_detalle_re.
          DELETE tl_acdoca_ret_cedular2    WHERE racct NOT IN  rg_imp_ret_cedular.

          DATA(tl_acdoca_ret_isr_pagext2)  =   tl_fac_detalle_re.
          DELETE tl_acdoca_ret_isr_pagext2  WHERE racct NOT IN  rg_ret_isr_pagext.
          "parte general

* inicia el procesamiento de datos
          CLEAR sl_report.

* se comienza a llenar la tabla de reporte
          IF p_pag_c IS NOT INITIAL.
            sl_report-semaforo = '@08@'.
          ELSE.
            sl_report-semaforo = '@09@'.
          ENDIF.
* Datos generales de pago
          sl_report-fechapago            = <fs_fac_pagos_re>-augdt.
          sl_report-n_doc_pago           = <fs_fac_pagos_re>-belnr.
          sl_report-n_doc_pago_sociedad  = <fs_fac_pagos_re>-rbukrs.
          sl_report-n_doc_pago_ejercicio = <fs_fac_pagos_re>-gjahr.
          "Sociedad
          sl_report-bukrs = <fs_fac_pagos_re>-rbukrs.
          "periodo
          sl_report-periodo = <fs_fac_pagos_re>-augdt+4(2).
          "ejercicio
          sl_report-gjahr   =  <fs_fac_pagos_re>-gjahr.


          IF <fs_fac_pagos_re>-hsl LT 0.
            sl_report-totalcargos  = <fs_fac_pagos_re>-hsl  * -1.
          ELSE.
            sl_report-totalcargos  = <fs_fac_pagos_re>-hsl .
          ENDIF.


          sl_report-referencia =   <fs_fac_pagos_re>-zuonr.
          sl_report-concepto   =   <fs_fac_pagos_re>-sgtxt.
          sl_report-referencia =  <fs_fac_pagos_re>-zuonr.






          ASSIGN tl_dfkkbptaxnum4[ partner = <fs_facturas_pr_re>-lifnr ] TO FIELD-SYMBOL(<fs_dfkkbptaxnum4>).
          IF sy-subrc = 0.
            IF <fs_dfkkbptaxnum4>-taxnumxl IS NOT INITIAL.
              sl_report-rfc = <fs_dfkkbptaxnum4>-taxnumxl.
            ELSE.
              sl_report-rfc = <fs_dfkkbptaxnum4>-taxnum.
            ENDIF.
            TRANSLATE sl_report-rfc TO UPPER CASE.
          ENDIF.


          "RAZON SOCIAL
          ASSIGN tl_but0004[ partner = <fs_facturas_pr_re>-lifnr ] TO FIELD-SYMBOL(<fs_but0004>).
          IF sy-subrc = 0.
            IF <fs_but0004>-natpers IS INITIAL.
              IF <fs_but0004>-name_org1 IS NOT INITIAL.
                CONCATENATE <fs_but0004>-name_org1 <fs_but0004>-name_org2 <fs_but0004>-name_org3 <fs_but0004>-name_org4 INTO sl_report-razon_s SEPARATED BY space.
              ELSE.
                CONCATENATE <fs_but0004>-name_last <fs_but0004>-name_first INTO sl_report-razon_s SEPARATED BY space.
              ENDIF.
            ELSE.
              IF <fs_but0004>-name_last IS NOT INITIAL.
                CONCATENATE <fs_but0004>-name_last <fs_but0004>-name_first INTO sl_report-razon_s SEPARATED BY space.
              ELSE.
                CONCATENATE <fs_but0004>-name_org1 <fs_but0004>-name_org2 <fs_but0004>-name_org3 <fs_but0004>-name_org4 INTO sl_report-razon_s SEPARATED BY space.
              ENDIF.
            ENDIF.
          ENDIF.

*        sl_report-clasificacion   = <fs_facturas_pr>-sgtxt. va  en funcion de la cuenta de iva

          "Acredor
          sl_report-acredor =  <fs_facturas_pr_re>-lifnr.

          sl_report-n_documento_fac           = <fs_facturas_pr_re>-belnr.
          sl_report-n_documento_fac_ejercicio = <fs_facturas_pr_re>-gjahr.
          sl_report-n_documento_fac_sociedad  = <fs_facturas_pr_re>-rbukrs.




          IF  <fs_facturas_pr_re>-hsl LT 0.
            sl_report-impor_mon_l_fac           =   <fs_facturas_pr_re>-hsl * -1.
          ELSE.
            sl_report-impor_mon_l_fac           =  <fs_facturas_pr_re>-hsl  .
          ENDIF.


* Ahora pregunta si es base 16
          CLEAR lv_index.

          SELECT ktopl,saknr,txt50
  INTO TABLE @tl_skat_gral
  FROM skat
  FOR ALL ENTRIES IN @tl_fac_ctas
  WHERE spras = @sy-langu         AND
   ktopl = @tl_fac_ctas-kokrs AND
   saknr = @tl_fac_ctas-racct.
          SORT tl_fac_ctas   BY  rbukrs belnr gjahr racct docln ASCENDING.
          LOOP AT tl_fac_ctas INTO DATA(wa_cta_fa).
            ADD 1 TO  lv_index.
            IF lv_index = 1.
** Procesa primero base 16
              CLEAR:

                             sl_report-iva_ret_6,
                             sl_report-iva_retenido_4,
                             sl_report-isr_honorarios,
                             sl_report-iva_ret_arren,
                             sl_report-imp_ret_cedular,
                             sl_report-iva_ret_serv_prof,
                             sl_report-isr_pagos_extranjero,
                             sl_report-isr_arrendamiento,
                             sl_report-n_sol_anticipo,
                             sl_report-n_sol_anticipo_ejercicio,
                             sl_report-n_sol_anticipo_sociedad,
                             sl_report-base_16,
                             sl_report-base_8,
                             sl_report-base_0,
                             sl_report-base_no_obj,
                             sl_report-base_exento,
                             sl_report-foliofiscal,
                             sl_report-iva_8,
                             sl_report-iva_16,
                             sl_report-ind_iva_16,
                             sl_report-ind_iva_0,
                             sl_report-indi_iva_8,
                             sl_report-ind_exento,
                             sl_report-ind_nodedu,
                             sl_report-clasificacion,
                             sl_report-total,
                             lv_tot_fac,
                             vl_total,
                             vl_valida_folio,
                             sl_report-total,
                             sl_report-base_exento,
                             sl_report-base_no_obj,
                             sl_report-ind_iva_16.


* base al 8


              CLEAR vl_name.
              CONCATENATE <fs_facturas_pr_re>-rbukrs <fs_facturas_pr_re>-belnr <fs_facturas_pr_re>-gjahr INTO vl_name.
*            CALL FUNCTION 'READ_TEXT'
*              EXPORTING
*                client                  = sy-mandt
*                id                      = 'YUUD'
*                language                = 'S'
*                name                    = vl_name
*                object                  = 'BELEG'
*              TABLES
*                lines                   = tl_lines
*              EXCEPTIONS
*                id                      = 1
*                language                = 2
*                name                    = 3
*                not_found               = 4
*                object                  = 5
*                reference_check         = 6
*                wrong_access_to_archive = 7
*                OTHERS                  = 8.

              CALL FUNCTION 'ZFI_ISR_READ_TEXT_BUFFER'
                EXPORTING
*                 CLIENT                  = SY-MANDT
                  ID                      = 'YUUD'
                  LANGUAGE                = 'S'
                  NAME                    = vl_name
                  OBJECT                  = 'BELEG'
*                 ARCHIVE_HANDLE          = 0
*                 LOCAL_CAT               = ' '
*             IMPORTING
*                 HEADER                  =
*                 OLD_LINE_COUNTER        =
                TABLES
                  LINES                   = tl_lines
                EXCEPTIONS
                  ID                      = 1
                  LANGUAGE                = 2
                  NAME                    = 3
                  NOT_FOUND               = 4
                  OBJECT                  = 5
                  REFERENCE_CHECK         = 6
                  WRONG_ACCESS_TO_ARCHIVE = 7
                  OTHERS                  = 8.

              IF sy-subrc = 0.
                ASSIGN tl_lines[ 1 ] TO FIELD-SYMBOL(<fs_lines2>).
                IF sy-subrc = 0.
                  sl_report-foliofiscal = <fs_lines2>-tdline.
                ENDIF.
              ELSE.
                vl_valida_folio  = 'N'.
              ENDIF.

* base 16
              CLEAR: sl_report-base_16, vl_total.

              LOOP AT tl_det_fac_bas16_re ASSIGNING  <fs_acdocabiva16> WHERE racct = wa_cta_fa-racct.
                vl_total = vl_total + <fs_acdocabiva16>-hsl.
                IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                sl_report-base_16    = sl_report-base_16 + vl_total.
                sl_report-ind_iva_16 = <fs_acdocabiva16>-mwskz.
                READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_cta_fa-racct.
                IF sy-subrc EQ 0.
                  sl_report-clasificacion = wa_skat_gra00-txt50.
                ENDIF.
** pas el iva
                IF vl_valida_folio  = 'N'.
                  sl_report-foliofiscal               = <fs_acdocabiva16>-sgtxt.
                ENDIF.
                DELETE tl_det_fac_bas16_re.
                EXIT.
              ENDLOOP.

              IF sy-subrc  = 0.
                CLEAR: sl_report-iva_16 , vl_total.
                sl_report-total = sl_report-total + sl_report-base_16.
                SORT tl_det_fac_iva_16 BY rbukrs augbl gjahr docln ASCENDING.
                LOOP AT tl_det_fac_iva_16 INTO DATA(wa_iva).
                  IF wa_iva-mwskz  IN rg_ziva_16_high.
                    sl_report-iva_16     = sl_report-iva_16 + wa_iva-hsl.
                  ENDIF.
                  DELETE tl_det_fac_iva_16.
                  EXIT.
                ENDLOOP.

                CLEAR:  vl_total.
                sl_report-total = sl_report-total + sl_report-iva_16 .
              ENDIF.


              "            procesamiento de base al 8 %
              CLEAR:  sl_report-base_8, vl_total.

              LOOP AT tl_det_fac_8%_re   INTO DATA(wa_base8)   WHERE racct = wa_cta_fa-racct.
                vl_total = vl_total + wa_base8-hsl.
                IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                IF  wa_base8-mwskz = 'W3'.
                  sl_report-indi_iva_8 = 'W3'.
                  sl_report-base_8 = vl_total + sl_report-base_8.
                ENDIF.
                READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr =  wa_cta_fa-racct.
                IF sy-subrc EQ 0.
                  sl_report-clasificacion = wa_skat_gra00-txt50.
                ENDIF.
                IF vl_valida_folio  = 'N'.
                  sl_report-foliofiscal               =  wa_base8-sgtxt.
                ENDIF.
                DELETE  tl_det_fac_8%_re." base_noobj.
                EXIT.
              ENDLOOP.
              .
              IF sy-subrc EQ 0.
                CLEAR:  vl_total.
                sl_report-total = sl_report-total + sl_report-base_8.
** obtiene el iva de esa base

                CLEAR: sl_report-iva_8.
                lt_iva_8 =   tl_det_fac_iva_8.
                SORT  lt_iva_8 BY rbukrs gjahr belnr docln ASCENDING.
                LOOP AT  lt_iva_8  INTO wa_iva.
                  IF wa_iva-mwskz  IN rg_ziva_8_high  .
                    sl_report-iva_8    = sl_report-iva_8 + wa_iva-hsl.
                    sl_report-indi_iva_8 = 'W3'.
                  ENDIF.
                  IF vl_valida_folio  = 'N'.
                    sl_report-foliofiscal               = wa_iva-sgtxt.
                  ENDIF.
                  DELETE lt_iva_8.
                  EXIT.
                ENDLOOP.
              ENDIF.
              sl_report-total = sl_report-total + sl_report-iva_8 .

* Base cero

              CLEAR: sl_report-base_0, vl_total.
              LOOP AT  tl_det_fac_0%_re  INTO wa_base0  WHERE racct = wa_cta_fa-racct..
                vl_total = vl_total + wa_base0-hsl.
                IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                IF  wa_base0-mwskz    = 'W0'.
                  sl_report-base_0    = sl_report-base_0 + vl_total.
                  sl_report-ind_iva_0 =  'W0'.
                ENDIF.

                READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_cta_fa-racct.
                IF sy-subrc EQ 0.
                  sl_report-clasificacion = wa_skat_gra00-txt50.
                ENDIF.
                IF vl_valida_folio  = 'N'.
                  sl_report-foliofiscal               = wa_base0-sgtxt.
                ENDIF.
                DELETE tl_det_fac_0%_re." base_cero
                EXIT.
              ENDLOOP.
              sl_report-total = sl_report-total + sl_report-base_0 .

*No deducible

              CLEAR: sl_report-base_nodedu, vl_total.

              LOOP AT   tl_det_fac_no_nd  INTO wa_basend  WHERE racct = wa_cta_fa-racct..
                vl_total = vl_total +  wa_basend-hsl.
                IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                IF  wa_basend-mwskz    = 'W8'.
                  sl_report-base_nodedu   = sl_report-base_nodedu  + vl_total.
                  sl_report-ind_nodedu  =  'W8'.
                ENDIF.

                READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_cta_fa-racct.
                IF sy-subrc EQ 0.
                  sl_report-clasificacion = wa_skat_gra00-txt50.
                ENDIF.
                IF vl_valida_folio  = 'N'.
                  sl_report-foliofiscal               = wa_basend-sgtxt.
                ENDIF.
                DELETE tl_det_fac_no_nd." base_cero
                EXIT.
              ENDLOOP.
              sl_report-total = sl_report-total + sl_report-base_nodedu.

              "NO objeto
              CLEAR: sl_report-base_no_obj, vl_total.

              LOOP AT    tl_det_fac_no_obj_re INTO DATA(wa_noobj)  WHERE racct = wa_cta_fa-racct.
                vl_total = vl_total +  wa_noobj-hsl.
                IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                IF  wa_noobj-mwskz    = ''.
                  sl_report-base_no_obj  = sl_report-base_no_obj + vl_total.
                ENDIF.

                READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_cta_fa-racct.
                IF sy-subrc EQ 0.
                  sl_report-clasificacion = wa_skat_gra00-txt50.
                ENDIF.
                IF vl_valida_folio  = 'N'.
                  sl_report-foliofiscal               = wa_noobj-sgtxt.
                ENDIF.
                DELETE tl_det_fac_no_obj_re ." base_cero
                EXIT.
              ENDLOOP.

              sl_report-total = sl_report-total +  sl_report-base_no_obj .


************************
              "retenciones

              "retenciones fletes

              LOOP AT  tl_det_fac_rtiva_fle  INTO DATA(re_fle) WHERE  rbukrs = <fs_facturas_pr_re>-rbukrs  AND
                                                  gjahr  = <fs_facturas_pr_re>-gjahr   AND
                                                   belnr = <fs_facturas_pr_re>-belnr.
                sl_report-total = sl_report-total + re_fle-hsl.
                sl_report-iva_retenido_4  =  sl_report-iva_retenido_4 + re_fle-hsl.

              ENDLOOP.

              " 4%

              LOOP AT  tl_det_fac_rtiva_subct_re INTO DATA(re_subc) WHERE  rbukrs = <fs_facturas_pr_re>-rbukrs  AND
                                                   gjahr  = <fs_facturas_pr_re>-gjahr   AND
                                                    belnr = <fs_facturas_pr_re>-belnr.


                sl_report-total = sl_report-total + re_subc-hsl.
                sl_report-iva_ret_6 =  sl_report-iva_ret_6   + re_subc-hsl.
              ENDLOOP.




              LOOP AT    tl_det_fac_rtiva_hono_re INTO DATA(re_hono)  WHERE  rbukrs = <fs_facturas_pr_re>-rbukrs  AND
                                                 gjahr  = <fs_facturas_pr_re>-gjahr   AND
                                                  belnr = <fs_facturas_pr_re>-belnr.
                sl_report-total = sl_report-total + re_fle-hsl.

                sl_report-total = sl_report-total + re_hono-hsl.
                sl_report-iva_ret_serv_prof  = re_hono-hsl.

              ENDLOOP.



              LOOP AT    tl_det_fac_rtiva_arre_re INTO DATA(re_arre) WHERE  rbukrs = <fs_facturas_pr_re>-rbukrs  AND
                                                   gjahr  = <fs_facturas_pr_re>-gjahr   AND
                                                    belnr = <fs_facturas_pr_re>-belnr.
                sl_report-total = sl_report-total + re_fle-hsl.

                sl_report-total = sl_report-total + re_arre-hsl.
                sl_report-iva_ret_arren   = re_arre-hsl.

              ENDLOOP.



              LOOP AT  tl_det_fac_rtsir_arre_re  INTO DATA(re_isr_arre) WHERE rbukrs = <fs_facturas_pr_re>-rbukrs  AND
                                                   gjahr  = <fs_facturas_pr_re>-gjahr   AND
                                                    belnr = <fs_facturas_pr_re>-belnr.
                sl_report-total = sl_report-total + re_fle-hsl.

                sl_report-total = sl_report-total + re_isr_arre-hsl.
                sl_report-isr_arrendamiento = re_isr_arre-hsl.

              ENDLOOP.


              LOOP AT tl_acdoca_ret_cedular2  ASSIGNING <fs_acdoca_ret_ced>  WHERE rbukrs = <fs_facturas_pr_re>-rbukrs  AND
                                                    gjahr  = <fs_facturas_pr_re>-gjahr   AND
                                                     belnr = <fs_facturas_pr_re>-belnr.
                sl_report-total = sl_report-total + re_fle-hsl.

                sl_report-imp_ret_cedular  = <fs_acdoca_ret_ced>-hsl.
                sl_report-total = sl_report-total + sl_report-imp_ret_cedular.

              ENDLOOP.



              LOOP AT  tl_acdoca_ret_isr_pagext2  ASSIGNING   <fs_acdoca_pag_ext> WHERE rbukrs = <fs_facturas_pr_re>-rbukrs  AND
                                                   gjahr  = <fs_facturas_pr_re>-gjahr   AND
                                                    belnr = <fs_facturas_pr_re>-belnr.
                sl_report-total = sl_report-total + re_fle-hsl.
                sl_report-isr_pagos_extranjero  = <fs_acdoca_pag_ext>-hsl.
                sl_report-total = sl_report-total + sl_report-isr_pagos_extranjero.

              ENDLOOP.



              LOOP AT  tl_det_fac_rtisr_hono_re  ASSIGNING   FIELD-SYMBOL(<fs_acdoca_pag_hono>) WHERE rbukrs = <fs_facturas_pr_re>-rbukrs  AND
                                                   gjahr  = <fs_facturas_pr_re>-gjahr   AND
                                                    belnr = <fs_facturas_pr_re>-belnr.
                sl_report-total = sl_report-total + re_fle-hsl.
                sl_report-isr_honorarios  = <fs_acdoca_pag_hono>-hsl.
                sl_report-total = sl_report-total + sl_report-isr_honorarios.

              ENDLOOP.

              IF sl_report-n_doc_pago IS NOT INITIAL.
                tl_report  = VALUE  #(
                            BASE tl_report          (  sl_report  )   ).
              ENDIF.
            ELSE.
* empieza el procesamiento de cuentas
              CLEAR:

                             sl_report-impor_mon_l_fac,
                             sl_report-iva_ret_6,
                             sl_report-iva_retenido_4,
                             sl_report-iva_ret_arren,
                             sl_report-isr_honorarios,
                             sl_report-imp_ret_cedular,
                             sl_report-iva_ret_serv_prof,
                             sl_report-isr_pagos_extranjero,
                             sl_report-isr_arrendamiento,
                             sl_report-n_sol_anticipo,
                             sl_report-n_sol_anticipo_ejercicio,
                             sl_report-n_sol_anticipo_sociedad,
                             sl_report-base_16,
                             sl_report-base_8,
                             sl_report-base_0,
                             sl_report-base_no_obj,
                             sl_report-base_exento,
                             sl_report-foliofiscal,
                             sl_report-iva_8,
                             sl_report-iva_16,
                             sl_report-ind_iva_16,
                             sl_report-ind_iva_0,
                             sl_report-indi_iva_8,
                             sl_report-ind_nodedu,
                             sl_report-ind_exento,
                             sl_report-clasificacion,
                             sl_report-total,
                             lv_tot_fac,
                             vl_total,
                             sl_report-total,
  vl_valida_folio,
                             sl_report-base_exento,
                             sl_report-base_no_obj,
                             sl_report-ind_iva_16.



              CLEAR vl_name.
              CONCATENATE <fs_facturas_pr_re>-rbukrs <fs_facturas_pr_re>-belnr <fs_facturas_pr_re>-gjahr INTO vl_name.
*            CALL FUNCTION 'READ_TEXT'
*              EXPORTING
*                client                  = sy-mandt
*                id                      = 'YUUD'
*                language                = 'S'
*                name                    = vl_name
*                object                  = 'BELEG'
*              TABLES
*                lines                   = tl_lines
*              EXCEPTIONS
*                id                      = 1
*                language                = 2
*                name                    = 3
*                not_found               = 4
*                object                  = 5
*                reference_check         = 6
*                wrong_access_to_archive = 7
*                OTHERS                  = 8.

              CALL FUNCTION 'ZFI_ISR_READ_TEXT_BUFFER'
                EXPORTING
*                 CLIENT                  = SY-MANDT
                  ID                      = 'YUUD'
                  LANGUAGE                = 'S'
                  NAME                    = vl_name
                  OBJECT                  = 'BELEG'
*                 ARCHIVE_HANDLE          = 0
*                 LOCAL_CAT               = ' '
*             IMPORTING
*                 HEADER                  =
*                 OLD_LINE_COUNTER        =
                TABLES
                  LINES                   = tl_lines
                EXCEPTIONS
                  ID                      = 1
                  LANGUAGE                = 2
                  NAME                    = 3
                  NOT_FOUND               = 4
                  OBJECT                  = 5
                  REFERENCE_CHECK         = 6
                  WRONG_ACCESS_TO_ARCHIVE = 7
                  OTHERS                  = 8.

              IF sy-subrc = 0.
                ASSIGN tl_lines[ 1 ] TO FIELD-SYMBOL(<fs_lines3>).
                IF sy-subrc = 0.
                  sl_report-foliofiscal = <fs_lines3>-tdline.
                ENDIF.
              ELSE.
                vl_valida_folio  = 'N'.
              ENDIF.

              CLEAR: sl_report-base_16, vl_total.
              LOOP AT tl_det_fac_bas16_re ASSIGNING  <fs_acdocabiva16> WHERE racct =   wa_cta_fa-racct.
                vl_total = vl_total + <fs_acdocabiva16>-hsl.
                IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                sl_report-base_16    = sl_report-base_16 + vl_total.
                sl_report-ind_iva_16 = <fs_acdocabiva16>-mwskz.
                READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_cta_fa-racct.
                IF sy-subrc EQ 0.
                  sl_report-clasificacion = wa_skat_gra00-txt50.
                ENDIF.
** pas el iva
                IF vl_valida_folio  = 'N'.
                  sl_report-foliofiscal               = <fs_acdocabiva16>-sgtxt.
                ENDIF.
                DELETE tl_det_fac_bas16_re.
                EXIT.
              ENDLOOP.
              IF sy-subrc = 0.
                sl_report-total = sl_report-total + sl_report-base_16 .
                CLEAR: sl_report-iva_16 ,  vl_total.

                LOOP AT tl_det_fac_iva_16 INTO wa_iva.
                  IF  wa_iva-mwskz  IN rg_ziva_16_high.
                    sl_report-iva_16     = sl_report-iva_16 +  wa_iva-hsl.
                    sl_report-ind_iva_16 =  wa_iva-mwskz.
                  ENDIF.
                  DELETE tl_det_fac_iva_16.
                  EXIT.
                ENDLOOP.
              ENDIF.
              CLEAR:  vl_total.
              sl_report-total = sl_report-total + sl_report-iva_16 .



              LOOP AT tl_det_fac_8%_re   INTO wa_base8   WHERE racct = wa_cta_fa-racct.
                vl_total = vl_total + wa_base8-hsl.
                IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                IF  wa_base8-mwskz = 'W3'.
                  sl_report-indi_iva_8 = 'W3'.
                  sl_report-base_8 = vl_total + sl_report-base_8.
                ENDIF.
                READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr =  wa_cta_fa-racct.
                IF sy-subrc EQ 0.
                  sl_report-clasificacion = wa_skat_gra00-txt50.
                ENDIF.
                IF vl_valida_folio  = 'N'.
                  sl_report-foliofiscal               =  wa_base8-sgtxt.
                ENDIF.
                DELETE  tl_det_fac_8%_re." base_noobj.
                EXIT.
              ENDLOOP.
              .
              IF sy-subrc EQ 0.
                CLEAR:  vl_total.
                sl_report-total = sl_report-total + sl_report-base_8.
** obtiene el iva de esa base

                CLEAR: sl_report-iva_8.
                lt_iva_8 =   tl_det_fac_iva_8.
                LOOP AT  lt_iva_8 ASSIGNING <fs_acdoca4>.
                  IF <fs_acdoca4>-mwskz  IN rg_ziva_8_high  .
                    sl_report-iva_8    = sl_report-iva_8 + <fs_acdoca4>-hsl.
                    sl_report-indi_iva_8 = 'W3'.
                  ENDIF.

                  DELETE lt_iva_8.
                  EXIT.
                ENDLOOP.
              ENDIF.
              sl_report-total = sl_report-total + sl_report-iva_8 .


*** base 0

              CLEAR: sl_report-base_0, vl_total.
              LOOP AT  tl_det_fac_0%_re  INTO wa_base0  WHERE racct = wa_cta_fa-racct.
                vl_total = vl_total + wa_base0-hsl.
                IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                IF   wa_base0-mwskz    = 'W0'.
                  sl_report-base_0    = sl_report-base_0 + vl_total.
                  sl_report-ind_iva_0 =  'W0'.
                ENDIF.

                READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr = wa_cta_fa-racct.
                IF sy-subrc EQ 0.
                  sl_report-clasificacion = wa_skat_gra00-txt50.
                ENDIF.
                IF vl_valida_folio  = 'N'.
                  sl_report-foliofiscal               = wa_base0-sgtxt.
                ENDIF.
                DELETE tl_det_fac_0%_re." base_cero
                EXIT.
              ENDLOOP.
              sl_report-total = sl_report-total + sl_report-base_0 .

** base nd

              CLEAR: sl_report-base_nodedu, vl_total.

              LOOP AT   tl_det_fac_no_nd  INTO wa_basend  WHERE racct =  wa_cta_fa-racct.
                vl_total = vl_total +  wa_basend-hsl.
                IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                IF  wa_basend-mwskz    = 'W8'.
                  sl_report-base_nodedu   = sl_report-base_nodedu + vl_total.
                  sl_report-ind_nodedu =  'W8'.
                ENDIF.

                READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr =  wa_cta_fa-racct.
                IF sy-subrc EQ 0.
                  sl_report-clasificacion = wa_skat_gra00-txt50.
                ENDIF.
                IF vl_valida_folio  = 'N'.
                  sl_report-foliofiscal               = wa_basend-sgtxt.
                ENDIF.
                DELETE tl_det_fac_no_nd." base_cero
                EXIT.
              ENDLOOP.
              sl_report-total = sl_report-total + sl_report-base_nodedu .

***32
              CLEAR: sl_report-base_no_obj, vl_total.

              LOOP AT    tl_det_fac_no_obj_re INTO wa_noobj   WHERE racct =  wa_cta_fa-racct.
                vl_total = vl_total +  wa_noobj-hsl.
                IF vl_total LT 0. vl_total = vl_total * -1. ENDIF.
                IF  wa_noobj-mwskz    = ''.
                  sl_report-base_no_obj  = sl_report-base_no_obj + vl_total.
                ENDIF.

                READ TABLE tl_skat_gral INTO  wa_skat_gra00  WITH KEY saknr =  wa_cta_fa-racct.
                IF sy-subrc EQ 0.
                  sl_report-clasificacion = wa_skat_gra00-txt50.
                ENDIF.
                IF vl_valida_folio  = 'N'.
                  sl_report-foliofiscal               = wa_noobj-sgtxt.
                ENDIF.
                DELETE tl_det_fac_no_obj_re ." base_cero
                EXIT.
              ENDLOOP.

              sl_report-total = sl_report-total +  sl_report-base_no_obj .

              IF sl_report  IS NOT INITIAL AND  sl_report-total GT 0.
                tl_report  = VALUE  #(
                                          BASE tl_report          (  sl_report  )   ).
              ENDIF.

            ENDIF.
            DELETE tl_fac_ctas WHERE  belnr  = wa_cta_fa-belnr  AND
                                              rbukrs = wa_cta_fa-rbukrs  AND
                                              gjahr  = wa_cta_fa-gjahr   AND
                                              docln =  wa_cta_fa-docln.
          ENDLOOP.
********************************
          DELETE tl_facturas_re        WHERE  belnr  = <fs_facturas_pr_re>-belnr  AND
                                              rbukrs = <fs_facturas_pr_re>-rbukrs  AND
                                              gjahr  = <fs_facturas_pr_re>-gjahr.
        ENDLOOP.

* fin de procesamiento
        DELETE tl_pagos_fac_re    WHERE belnr   = <fs_fac_pagos_re>-augbl   AND
                                         rbukrs = <fs_fac_pagos_re>-rbukrs  AND
                                         gjahr  = <fs_fac_pagos_re>-gjahr.
      ENDLOOP.


* ahora se tendra que recorrer la tabla de reporte para insertar los datos de la factura ya que de otra forma es mas complicado
** fin ciclos de pagos
    ELSE. " no encuentra doctos de pago para factoraje
    ENDIF." fin de factoraje

    SORT tl_report BY  n_doc_pago semaforo DESCENDING n_documento_fac  impor_mon_l_fac DESCENDING.
*<-- end insert AMP
    PERFORM REPAIR_OUTPUT.

  endif.
*****************************************************************

  PERFORM GET_RANGE.
  if P1 = 'X'.
    "Pagos a proveedores

    PERFORM PAGOS_GENERALES.
    PERFORM ARMAR_PAGOSG     USING IT_PGENERAL
                                   TL_REPORT2.

  endif.

  if P2 = 'X'.
    PERFORM BUSCA_TRASPASOS.
    PERFORM ARMA_TRAPASOS USING IT_TR_ORIGEN
                                IT_TR_DESTINO
                                TL_REPORT2
                                IT_DFKKBPTAXNUM.
  endif.
  if P3 = 'X'.

    PERFORM BUSCA_INVERSIONES.
    PERFORM ARMAR_INVERSIONES USING IT_INV_ORIGEN
                                    IT_INV_DESTINO
                                    TL_REPORT2.
  ENDIF.

  if P4 = 'X'.
    PERFORM AMEX.
    PERFORM ARMAR_AMEX USING IT_AMEX
                             TL_REPORT2.
  endif.

  if P5 = 'X'.
    PERFORM CONFIRMING.
    PERFORM ARMAR_CONFIRMING USING IT_CONFIRMING
                                   TL_REPORT2.
  endif.

  if P6 = 'X'.

    PERFORM ANTICIPO.
    PERFORM ARMAR_ANTICIPO USING IT_ANTICIPO
                                 TL_REPORT2.

  endif.

  if P7 = 'X'.
    PERFORM REEMBOLSOS.
    PERFORM ARMAR_REEMBOLSOS USING IT_LIQUIDA
                                   TL_REPORT2.
  endif.

  if P8 = 'X'.
    PERFORM LIQUIDA.
    PERFORM ARMAR_LIQUIDA    USING IT_LIQUIDA
                                   TL_REPORT2.
  endif.


  append LINES OF TL_REPORT2 to TL_REPORT.
*****************************************************************

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LIMPIA_VARIABLES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM limpia_variables .
  REFRESH: rg_hkont,rg_augbl,rg_blart,tl_report,tl_fieldcat,rg_docval,rg_z_ejercicioval,rg_ziva_0,rg_belnr,
           rg_nomina_exclusion_h,rg_nomina_exclusion_s,rg_z_caso_ktosl_001,rg_z_caso_ktosl_004,
           rg_z_caso_ktosl_004_low, rg_cargo_abono_compro_low.
  CLEAR:vl_entro.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REPORTE_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM reporte_alv .
  PERFORM build_fieldcat.
  PERFORM build_layout.
  PERFORM display_alv_report.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_fieldcat .
  tl_fieldcat = VALUE #(
     BASE tl_fieldcat
                      ( fieldname   = 'SEMAFORO'                   scrtext_s   = TEXT-001 scrtext_m   = TEXT-001 scrtext_l   = TEXT-001 col_opt = abap_on )
                      ( fieldname   = 'BUKRS'                      scrtext_s   = TEXT-079 scrtext_m   = TEXT-079 scrtext_l   = TEXT-079 col_opt = abap_on )
                      ( fieldname   = 'GJAHR'                      scrtext_s   = TEXT-081 scrtext_m   = TEXT-081 scrtext_l   = TEXT-081 col_opt = abap_on )
                      ( fieldname   = 'PERIODO'                    scrtext_s   = TEXT-080 scrtext_m   = TEXT-080 scrtext_l   = TEXT-080 col_opt = abap_on )
                      ( fieldname   = 'FECHAPAGO'                  scrtext_s   = TEXT-002 scrtext_m   = TEXT-002 scrtext_l   = TEXT-002 col_opt = abap_on )
                      ( fieldname   = 'DESCRIPCION'                scrtext_s   = TEXT-003 scrtext_m   = TEXT-003 scrtext_l   = TEXT-003 col_opt = abap_on )
                      ( fieldname   = 'TOTALCARGOS'                scrtext_s   = TEXT-004 scrtext_m   = TEXT-004 scrtext_l   = TEXT-004 col_opt = abap_on )
                      ( fieldname   = 'REFERENCIA'                 scrtext_s   = TEXT-005 scrtext_m   = TEXT-005 scrtext_l   = TEXT-005 col_opt = abap_on )
                      ( fieldname   = 'CONCEPTO'                   scrtext_s   = TEXT-006 scrtext_m   = TEXT-006 scrtext_l   = TEXT-006 col_opt = abap_on )
                      ( fieldname   = 'N_DOC_PAGO'                 scrtext_s   = TEXT-007 scrtext_m   = TEXT-007 scrtext_l   = TEXT-007 col_opt = abap_on )
                      ( fieldname   = 'N_SOL_ANTICIPO'             scrtext_s   = TEXT-008 scrtext_m   = TEXT-008 scrtext_l   = TEXT-008 col_opt = abap_on )
                      ( fieldname   = 'N_DOCUMENTO_FAC'            scrtext_s   = TEXT-009 scrtext_m   = TEXT-009 scrtext_l   = TEXT-009 col_opt = abap_on )
                      ( fieldname   = 'CLASIFICACION'              scrtext_s   = TEXT-010 scrtext_m   = TEXT-010 scrtext_l   = TEXT-010 col_opt = abap_on )
                      ( fieldname   = 'FOLIOFISCAL'                scrtext_s   = TEXT-011 scrtext_m   = TEXT-011 scrtext_l   = TEXT-011 col_opt = abap_on )
                      ( fieldname   = 'IMPOR_MON_L_FAC'            scrtext_s   = TEXT-012 scrtext_m   = TEXT-012 scrtext_l   = TEXT-012 col_opt = abap_on )
                      ( fieldname   = 'RFC'                        scrtext_s   = TEXT-013 scrtext_m   = TEXT-013 scrtext_l   = TEXT-013 col_opt = abap_on )
                      ( fieldname   = 'RAZON_S'                    scrtext_s   = TEXT-014 scrtext_m   = TEXT-014 scrtext_l   = TEXT-014 col_opt = abap_on )
                      ( fieldname   = 'ACREDOR'                    scrtext_s   = TEXT-015 scrtext_m   = TEXT-015 scrtext_l   = TEXT-015 col_opt = abap_on )
                      ( fieldname   = 'IND_IVA_16'                 scrtext_s   = TEXT-016 scrtext_m   = TEXT-016 scrtext_l   = TEXT-016 col_opt = abap_on )
                      ( fieldname   = 'BASE_16'                    scrtext_s   = TEXT-017 scrtext_m   = TEXT-017 scrtext_l   = TEXT-017 col_opt = abap_on )
                      ( fieldname   = 'INDI_IVA_8'                 scrtext_s   = TEXT-018 scrtext_m   = TEXT-018 scrtext_l   = TEXT-018 col_opt = abap_on )
                      ( fieldname   = 'BASE_8'                     scrtext_s   = TEXT-019 scrtext_m   = TEXT-019 scrtext_l   = TEXT-019 col_opt = abap_on )
                      ( fieldname   = 'IND_IVA_0'                  scrtext_s   = TEXT-020 scrtext_m   = TEXT-021 scrtext_l   = TEXT-021 col_opt = abap_on )
                      ( fieldname   = 'BASE_0'                     scrtext_s   = TEXT-021 scrtext_m   = TEXT-022 scrtext_l   = TEXT-022 col_opt = abap_on )
                      ( fieldname   = 'IND_NODEDU'                 scrtext_s   = TEXT-095 scrtext_m   = TEXT-095 scrtext_l   = TEXT-095 col_opt = abap_on )
                      ( fieldname   = 'BASE_NODEDU'                scrtext_s   = TEXT-096 scrtext_m   = TEXT-096 scrtext_l   = TEXT-096 col_opt = abap_on )
                      ( fieldname   = 'IND_EXENTO'                 scrtext_s   = TEXT-116 scrtext_m   = TEXT-116 scrtext_l   = TEXT-116 col_opt = abap_on )
                      ( fieldname   = 'BASE_EXENTO'                scrtext_s   = TEXT-115 scrtext_m   = TEXT-115 scrtext_l   = TEXT-115 col_opt = abap_on )
*                      ( fieldname   = 'IND_NO_OBJ'                 scrtext_s   = TEXT-024 scrtext_m   = TEXT-025 scrtext_l   = TEXT-025 col_opt = abap_on )
*                      ( fieldname   = 'BASE_NO_OBJ'                scrtext_s   = TEXT-025 scrtext_m   = TEXT-066 scrtext_l   = TEXT-066 col_opt = abap_on )
                      ( fieldname   = 'IVA_16'                     scrtext_s   = TEXT-026 scrtext_m   = TEXT-026 scrtext_l   = TEXT-026 col_opt = abap_on )
                      ( fieldname   = 'IVA_8'                      scrtext_s   = TEXT-069 scrtext_m   = TEXT-069 scrtext_l   = TEXT-069 col_opt = abap_on )
                      ( fieldname   = 'IMPU_IVA_VIRTUAL'           scrtext_s   = TEXT-027 scrtext_m   = TEXT-027 scrtext_l   = TEXT-027 col_opt = abap_on )
                      ( fieldname   = 'IVA_RET_ARREN'              scrtext_s   = TEXT-028 scrtext_m   = TEXT-028 scrtext_l   = TEXT-028 col_opt = abap_on )
                      ( fieldname   = 'IVA_RET_SERV_PROF'          scrtext_s   = TEXT-029 scrtext_m   = TEXT-029 scrtext_l   = TEXT-029 col_opt = abap_on )
                      ( fieldname   = 'IVA_RET_6'                  scrtext_s   = TEXT-030 scrtext_m   = TEXT-030 scrtext_l   = TEXT-030 col_opt = abap_on )
                      ( fieldname   = 'IVA_RET_FLETES'             scrtext_s   = TEXT-111 scrtext_m   = TEXT-111 scrtext_l   = TEXT-111 col_opt = abap_on )
                      ( fieldname   = 'IVA_RETENIDO_4'             scrtext_s   = TEXT-031 scrtext_m   = TEXT-031 scrtext_l   = TEXT-031 col_opt = abap_on ) "no_out = 'X'
                      ( fieldname   = 'IMP_RET_CEDULAR'            scrtext_s   = TEXT-094 scrtext_m   = TEXT-094 scrtext_l   = TEXT-094 col_opt = abap_on )
                      ( fieldname   = 'ISR_HONORARIOS'             scrtext_s   = TEXT-032 scrtext_m   = TEXT-032 scrtext_l   = TEXT-032 col_opt = abap_on )
                      ( fieldname   = 'ISR_ARRENDAMIENTO'          scrtext_s   = TEXT-033 scrtext_m   = TEXT-033 scrtext_l   = TEXT-033 col_opt = abap_on )
                      ( fieldname   = 'ISR_PAGOS_EXTRANJERO'       scrtext_s   = TEXT-034 scrtext_m   = TEXT-034 scrtext_l   = TEXT-034 col_opt = abap_on )
                      ( fieldname   = 'ISR_DIVIDENDOS'             scrtext_s   = TEXT-035 scrtext_m   = TEXT-035 scrtext_l   = TEXT-035 col_opt = abap_on )
                      ( fieldname   = 'ISR_INTERESES'              scrtext_s   = TEXT-036 scrtext_m   = TEXT-036 scrtext_l   = TEXT-036 col_opt = abap_on )
                      ( fieldname   = 'ISR_VENTA_ACCIONES_RETENER' scrtext_s   = TEXT-114 scrtext_m   = TEXT-114 scrtext_l   = TEXT-114 col_opt = abap_on )
                      ( fieldname   = 'ISR_RESICO'                 scrtext_s   = TEXT-037 scrtext_m   = TEXT-037 scrtext_l   = TEXT-037 col_opt = abap_on )
                      ( fieldname   = '2_SAR'                      scrtext_s   = TEXT-038 scrtext_m   = TEXT-038 scrtext_l   = TEXT-038 col_opt = abap_on )
                      ( fieldname   = '5_INFONAVIT'                scrtext_s   = TEXT-039 scrtext_m   = TEXT-039 scrtext_l   = TEXT-039 col_opt = abap_on )
                      ( fieldname   = 'AYUDA_SINDIC_X_PAGAR'       scrtext_s   = TEXT-088 scrtext_m   = TEXT-088 scrtext_l   = TEXT-088 col_opt = abap_on )
                      ( fieldname   = 'CASETAS'                    scrtext_s   = TEXT-087 scrtext_m   = TEXT-087 scrtext_l   = TEXT-087 col_opt = abap_on )
                      ( fieldname   = 'CESANTIA_VEJEZ'             scrtext_s   = TEXT-040 scrtext_m   = TEXT-040 scrtext_l   = TEXT-040 col_opt = abap_on )
                      ( fieldname   = 'COMPENS_EXTRAORDINAR'       scrtext_s   = TEXT-041 scrtext_m   = TEXT-041 scrtext_l   = TEXT-041 col_opt = abap_on )
                      ( fieldname   = 'CUOTA_PATRONAL_IMSS'        scrtext_s   = TEXT-042 scrtext_m   = TEXT-042 scrtext_l   = TEXT-042 col_opt = abap_on )
                      ( fieldname   = 'DOMINGOS_DIAS_FEST'         scrtext_s   = TEXT-043 scrtext_m   = TEXT-043 scrtext_l   = TEXT-043 col_opt = abap_on )
                      ( fieldname   = 'FONDO_AHORRO'               scrtext_s   = TEXT-044 scrtext_m   = TEXT-044 scrtext_l   = TEXT-044 col_opt = abap_on )
                      ( fieldname   = 'FAC_ADMVAS_MANIOBRAS'       scrtext_s   = TEXT-089 scrtext_m   = TEXT-089 scrtext_l   = TEXT-089 col_opt = abap_on )
                      ( fieldname   = 'FAC_ADMVAS_GTS_CAMINO'      scrtext_s   = TEXT-090 scrtext_m   = TEXT-090 scrtext_l   = TEXT-090 col_opt = abap_on )
                      ( fieldname   = 'FAC_ADMVAS_GTS_TALACHAS'   scrtext_s   = TEXT-091 scrtext_m   = TEXT-091 scrtext_l   = TEXT-091 col_opt = abap_on )
                      ( fieldname   = 'FAC_ADMVAS_GTS_REP_MEN'     scrtext_s   = TEXT-092 scrtext_m   = TEXT-092 scrtext_l   = TEXT-092 col_opt = abap_on )
                      ( fieldname   = 'FAC_ADMVAS_GTS_FIANZAS'     scrtext_s   = TEXT-093 scrtext_m   = TEXT-093 scrtext_l   = TEXT-093 col_opt = abap_on )
                      ( fieldname   = 'OTRAS_PRESTACIONES'         scrtext_s   = TEXT-075 scrtext_m   = TEXT-075 scrtext_l   = TEXT-073 col_opt = abap_on )
                      ( fieldname   = 'OTROS_GASTOS_VIAJE'         scrtext_s   = TEXT-086 scrtext_m   = TEXT-086 scrtext_l   = TEXT-086 col_opt = abap_on )
                      ( fieldname   = 'PREMIO_PUNTUALIDAD'         scrtext_s   = TEXT-045 scrtext_m   = TEXT-045 scrtext_l   = TEXT-045 col_opt = abap_on )
                      ( fieldname   = 'PRIMA_DOMINICAL'            scrtext_s   = TEXT-046 scrtext_m   = TEXT-046 scrtext_l   = TEXT-046 col_opt = abap_on )
                      ( fieldname   = 'RVA_AGUINALDO'              scrtext_s   = TEXT-047 scrtext_m   = TEXT-047 scrtext_l   = TEXT-047 col_opt = abap_on )
                      ( fieldname   = 'PRIMA_VACACIONAL'           scrtext_s   = TEXT-071 scrtext_m   = TEXT-071 scrtext_l   = TEXT-071 col_opt = abap_on )
                      ( fieldname   = 'RVA_PRIMA_VACACIONAL'       scrtext_s   = TEXT-048 scrtext_m   = TEXT-048 scrtext_l   = TEXT-048 col_opt = abap_on )
                      ( fieldname   = 'SUBSIDIO_EMPLEO'            scrtext_s   = TEXT-049 scrtext_m   = TEXT-049 scrtext_l   = TEXT-049 col_opt = abap_on )
                      ( fieldname   = 'SUELDOS'                    scrtext_s   = TEXT-050 scrtext_m   = TEXT-050 scrtext_l   = TEXT-050 col_opt = abap_on )
                      ( fieldname   = 'TIEMPO_EXTRA_DOBLE'         scrtext_s   = TEXT-051 scrtext_m   = TEXT-051 scrtext_l   = TEXT-051 col_opt = abap_on )
                      ( fieldname   = 'TIEMPO_EXTRA_TRIPLE'        scrtext_s   = TEXT-052 scrtext_m   = TEXT-052 scrtext_l   = TEXT-052 col_opt = abap_on )
                      ( fieldname   = 'VIATICOS'                   scrtext_s   = TEXT-085 scrtext_m   = TEXT-085 scrtext_l   = TEXT-085 col_opt = abap_on )
                      ( fieldname   = 'CESANTIA_VEJEZ_EMPL'        scrtext_s   = TEXT-053 scrtext_m   = TEXT-053 scrtext_l   = TEXT-053 col_opt = abap_on )
                      ( fieldname   = 'CESANTIA_VEJEZ_PATRO'       scrtext_s   = TEXT-054 scrtext_m   = TEXT-054 scrtext_l   = TEXT-054 col_opt = abap_on )
                      ( fieldname   = 'DESCUENTO_INFONACOT'        scrtext_s   = TEXT-055 scrtext_m   = TEXT-055 scrtext_l   = TEXT-055 col_opt = abap_on )
                      ( fieldname   = 'FINIQUITO_POR_PAGAR'        scrtext_s   = TEXT-084 scrtext_m   = TEXT-084 scrtext_l   = TEXT-084 col_opt = abap_on )
                      ( fieldname   = 'FONDO_AHORRO_EMPLEAD'       scrtext_s   = TEXT-056 scrtext_m   = TEXT-056 scrtext_l   = TEXT-056 col_opt = abap_on )
                      ( fieldname   = 'FONDO_AHORRO_EMPRESA'       scrtext_s   = TEXT-057 scrtext_m   = TEXT-057 scrtext_l   = TEXT-057 col_opt = abap_on )
                      ( fieldname   = 'IMPULSO_ECONOMICO_CREA'     scrtext_s   = TEXT-076 scrtext_m   = TEXT-076 scrtext_l   = TEXT-076 col_opt = abap_on )
                      ( fieldname   = 'IMSS_PATRONAL'              scrtext_s   = TEXT-058 scrtext_m   = TEXT-058 scrtext_l   = TEXT-058 col_opt = abap_on )
                      ( fieldname   = 'IMSS_RETENIDO'              scrtext_s   = TEXT-059 scrtext_m   = TEXT-059 scrtext_l   = TEXT-059 col_opt = abap_on )
                      ( fieldname   = 'INFONAVIT_PATRONAL'         scrtext_s   = TEXT-060 scrtext_m   = TEXT-060 scrtext_l   = TEXT-060 col_opt = abap_on )
                      ( fieldname   = 'INFONAVIT_RETENIDO'         scrtext_s   = TEXT-061 scrtext_m   = TEXT-061 scrtext_l   = TEXT-061 col_opt = abap_on )
                      ( fieldname   = 'ING_X_DEV_EMPLEADOS'        scrtext_s   = TEXT-077 scrtext_m   = TEXT-077 scrtext_l   = TEXT-077 col_opt = abap_on )
                      ( fieldname   = 'ING_X_DESCTO_COMEDOR'       scrtext_s   = TEXT-078 scrtext_m   = TEXT-078 scrtext_l   = TEXT-078 col_opt = abap_on )
                      ( fieldname   = 'ISR_RET_X_SUELDOS'          scrtext_s   = TEXT-062 scrtext_m   = TEXT-062 scrtext_l   = TEXT-062 col_opt = abap_on )
                      ( fieldname   = 'PENSION_ALIM_TOTAL'         scrtext_s   = TEXT-072 scrtext_m   = TEXT-072 scrtext_l   = TEXT-072 col_opt = abap_on )
                      ( fieldname   = 'PROV_AGUI_X_PAGAR'          scrtext_s   = TEXT-063 scrtext_m   = TEXT-063 scrtext_l   = TEXT-063 col_opt = abap_on )
                      ( fieldname   = 'PROV_PRIM_VACACIONAL'       scrtext_s   = TEXT-064 scrtext_m   = TEXT-064 scrtext_l   = TEXT-064 col_opt = abap_on )
                      ( fieldname   = 'CUOTA_SIND_X_PAGOTOT'       scrtext_s   = TEXT-073 scrtext_m   = TEXT-073 scrtext_l   = TEXT-073 col_opt = abap_on )
                      ( fieldname   = 'SEGURO_VIV_INFON_TOT'       scrtext_s   = TEXT-074 scrtext_m   = TEXT-074 scrtext_l   = TEXT-074 col_opt = abap_on )
*                      ( fieldname   = 'SUBSIDIO_EMPLEO_TOT'      scrtext_s   = TEXT-064 scrtext_m   = TEXT-064 scrtext_l   = TEXT-064 col_opt = abap_on )

                      ( fieldname   = 'OTROS_GASTOS'               scrtext_s   = TEXT-100 scrtext_m   = TEXT-100 scrtext_l   = TEXT-100 col_opt = abap_on )
                      ( fieldname   = 'OTROS_RETIROS'              scrtext_s   = TEXT-101 scrtext_m   = TEXT-101 scrtext_l   = TEXT-101 col_opt = abap_on )
                      ( fieldname   = 'ISR_2'                      scrtext_s   = TEXT-102 scrtext_m   = TEXT-102 scrtext_l   = TEXT-102 col_opt = abap_on )
                      ( fieldname   = 'ZBANCO_COBRO'               scrtext_s   = TEXT-103 scrtext_m   = TEXT-103 scrtext_l   = TEXT-103 col_opt = abap_on )
                      ( fieldname   = 'ACTIVO_FIJO1'               scrtext_s   = TEXT-104 scrtext_m   = TEXT-104 scrtext_l   = TEXT-104 col_opt = abap_on )
                      ( fieldname   = 'ACTIVO_FIJO2'               scrtext_s   = TEXT-105 scrtext_m   = TEXT-105 scrtext_l   = TEXT-105 col_opt = abap_on )
                      ( fieldname   = 'SUELDOS_POR_PAGAR'          scrtext_s   = TEXT-083 scrtext_m   = TEXT-083 scrtext_l   = TEXT-083 col_opt = abap_on )
                      ( fieldname   = 'TOTAL'                      scrtext_s   = TEXT-065 scrtext_m   = TEXT-065 scrtext_l   = TEXT-065 col_opt = abap_on )
                      ( fieldname   = 'NEW_REPORT'                 scrtext_s   = TEXT-180 scrtext_m   = TEXT-180 scrtext_l   = TEXT-180 col_opt = abap_on ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_LAYOUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_layout .
  gd_layout-stylefname = 'FIELD_STYLE'.
*  gd_layout-colwidth_optimize = abap_on.
  gd_layout-zebra             = abap_on.
  gd_layout-col_opt           = abap_on.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_alv_report .
  if P0 = 'X'.
    PERFORM revisa_totales.
  endif.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND' "handle_user_command
      is_layout_lvc            = gd_layout
      it_fieldcat_lvc          = tl_fieldcat
      i_save                   = abap_on
    TABLES
      t_outtab                 = tl_report.

*   IF ob_grid IS INITIAL.
*    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
*      IMPORTING
*        e_grid = ob_grid.
*  ENDIF.
*
*  IF ob_grid IS NOT INITIAL. "Sanity Test
**    PERFORM select_data .
*    CALL METHOD ob_grid->refresh_table_display.
*  ENDIF.

ENDFORM.
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZSTANDARD'.
ENDFORM. "Set_pf_status
FORM user_command
        USING p_ucomm LIKE sy-ucomm
              p_selfield TYPE slis_selfield.
  CASE p_ucomm .
    WHEN '&IC1'.

      PERFORM check USING p_selfield.
      PERFORM refresh USING p_selfield.
    WHEN '&SAV'.
      PERFORM guardar.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_SELFIELD
*&---------------------------------------------------------------------*
FORM check USING p_selfield TYPE slis_selfield.
  DATA: vl_string  TYPE string, vl_string2 TYPE string.
  SPLIT p_selfield-sel_tab_field  AT '-' INTO  vl_string vl_string2.
  DATA: vl_ebeln TYPE ebeln.
  IF vl_string IS NOT INITIAL.
    ASSIGN tl_report[ p_selfield-tabindex ] TO FIELD-SYMBOL(<fs_report>).
    IF sy-subrc = 0.
      IF vl_string2 = 'N_DOC_PAGO' AND <fs_report>-n_doc_pago IS NOT INITIAL.
        CALL FUNCTION 'FI_DOCUMENT_DISPLAY_RFC'
          EXPORTING
            i_belnr = <fs_report>-n_doc_pago
            i_bukrs = <fs_report>-n_doc_pago_sociedad
            i_gjahr = <fs_report>-n_doc_pago_ejercicio.
      ELSEIF vl_string2 = 'N_DOCUMENTO_FAC' AND <fs_report>-n_documento_fac IS NOT INITIAL.
        CALL FUNCTION 'FI_DOCUMENT_DISPLAY_RFC'
          EXPORTING
            i_belnr = <fs_report>-n_documento_fac
            i_bukrs = <fs_report>-n_documento_fac_sociedad
            i_gjahr = <fs_report>-n_documento_fac_ejercicio.
      ELSEIF vl_string2 = 'N_SOL_ANTICIPO' AND <fs_report>-n_sol_anticipo IS NOT INITIAL.
        CALL FUNCTION 'FI_DOCUMENT_DISPLAY_RFC'
          EXPORTING
            i_belnr = <fs_report>-n_sol_anticipo
            i_bukrs = <fs_report>-n_sol_anticipo_sociedad
            i_gjahr = <fs_report>-n_sol_anticipo_ejercicio.
      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh USING p_selfield TYPE slis_selfield.
  IF ob_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ob_grid.
  ENDIF.

  IF ob_grid IS NOT INITIAL. "Sanity Test
*    PERFORM select_data .
    CALL METHOD ob_grid->refresh_table_display.
  ENDIF.
  p_selfield-refresh    = abap_on.
  p_selfield-col_stable = abap_on.
  p_selfield-row_stable = abap_on.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form RANGOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM rangos .
  "Recuperamos la clase de documento de la variante
  SELECT low
     INTO TABLE @DATA(tl_tvarvc)
     FROM tvarvc
     WHERE name = 'ZDOC_PAG_C'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc ASSIGNING FIELD-SYMBOL(<fs_tvarvc>).
      rg_blart = VALUE  #(
                   BASE rg_blart ( low    = <fs_tvarvc>-low
                                   sign   = c_i
                                   option = c_eq ) ).
    ENDLOOP.
  ENDIF.
* recuperamos casos de liquidaciones o casos especiales
  "Recuperamos la clase de documento de la variante

* variables que controlan las liquidaciones
  SELECT low
     INTO TABLE @DATA(tl_tvarvc_liq)
     FROM tvarvc
     WHERE name = 'ZDOC_PAG_LIQ'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_liq ASSIGNING FIELD-SYMBOL(<fs_tvarvc_liq>).
      rg_ti_doc_liq = VALUE  #(
                   BASE rg_ti_doc_liq ( low  = <fs_tvarvc_liq>-low
                                   sign   = c_i
                                   option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
     INTO TABLE @DATA(tl_comp_liq)
     FROM tvarvc
     WHERE name = 'ZDOC_COMP_LIQ'.
  IF sy-subrc = 0.
    LOOP AT tl_comp_liq ASSIGNING FIELD-SYMBOL(<fs_comp_liq>).
      rg_ti_com_liq = VALUE  #(
                   BASE rg_ti_com_liq ( low  = <fs_comp_liq>-low
                                   sign   = c_i
                                   option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  SELECT low
     INTO TABLE @DATA(tl_ran_liq)
     FROM tvarvc
     WHERE name = 'ZDOC_RAN_LIQ'.
  IF sy-subrc = 0.
    LOOP AT tl_ran_liq ASSIGNING FIELD-SYMBOL(<fs_ran_liq>).
      rg_ran_liq = VALUE  #(
                   BASE rg_ran_liq ( low  = <fs_ran_liq>-low
                                   sign   = c_i
                                   option = c_eq ) ).
    ENDLOOP.
  ENDIF.

* fin de variables que controlan las liquidaciones

  SELECT low
     INTO TABLE tl_tvarvc
     FROM tvarvc
     WHERE name = 'ZDOC_FAC_C'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc ASSIGNING <fs_tvarvc>.
      rg_blart_2 = VALUE  #(
                   BASE rg_blart_2 ( low    = <fs_tvarvc>-low
                                   sign   = c_i
                                   option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  "augbl -  se validará que no este vacío y que contenga un docto serie 15*
  rg_augbl = VALUE  #(
             BASE rg_augbl ( low    = c_15
                             sign   = c_i
                             option = c_cp ) ).
  SELECT low
    INTO TABLE @DATA(tl_tvarvc_2)
    FROM tvarvc
    WHERE name = 'ZCUENTAS_BANCOS'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_2 ASSIGNING <fs_tvarvc>.
      rg_hkont = VALUE  #(
                   BASE rg_hkont ( low    = <fs_tvarvc>-low
                                   sign   = c_i
                                   option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low, high
    INTO TABLE @DATA(tl_tvarvc_4)
    FROM tvarvc
    WHERE name = 'ZIVA_16%'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING FIELD-SYMBOL(<fs_tvarvc4>).
      rg_ziva_16 = VALUE  #(
                   BASE rg_ziva_16 ( low    = <fs_tvarvc4>-low
                                     sign   = c_i
                                     option = c_eq ) ).
      rg_ziva_16_high = VALUE  #(
                        BASE rg_ziva_16_high ( low    = <fs_tvarvc4>-high
                       sign   = c_i
                       option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  SELECT low, high
    INTO TABLE @DATA(tl_tvarvc_5)
    FROM tvarvc
    WHERE name = 'ZIVA_ND'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_5 ASSIGNING FIELD-SYMBOL(<fs_tvarvc5>).
      rg_ziva_nd = VALUE  #(
                   BASE rg_ziva_nd ( low    = <fs_tvarvc5>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  SELECT low
    INTO TABLE @DATA(tl_tvarvc_3)
    FROM tvarvc
    WHERE name = 'Z_CASO_KTOSL_001'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_z_caso_ktosl_001 = VALUE  #(
                   BASE rg_hkont ( low    = <fs_tvarvc>-low
                                   sign   = c_i
                                   option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  REFRESH tl_tvarvc_3[].
  SELECT low
    INTO TABLE @tl_tvarvc_3
    FROM tvarvc
    WHERE name = 'Z_CASO_KTOSL_004'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_z_caso_ktosl_004 = VALUE  #(
                   BASE rg_hkont ( low    = <fs_tvarvc>-low
                                   sign   = c_i
                                   option = c_eq ) ).
      rg_z_caso_ktosl_004_low = VALUE  #(
                   BASE rg_z_caso_ktosl_004_low ( low    = <fs_tvarvc>-low
                                   sign   = c_i
                                   option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low
    INTO TABLE @tl_tvarvc_3
    FROM tvarvc
    WHERE name = 'ZRET_IVA_RET_SUB_FLETES'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_zret_iva_ret_sub_fletes = VALUE  #(
                   BASE rg_zret_iva_ret_sub_fletes ( low    = <fs_tvarvc>-low
                                                     sign   = c_i
                                                    option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low
    INTO TABLE @tl_tvarvc_3
    FROM tvarvc
    WHERE name = 'ZRET_IVA_RET_SUB_CONTRA'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_zret_iva_ret_sub_contra = VALUE  #(
                   BASE rg_zret_iva_ret_sub_contra ( low    = <fs_tvarvc>-low
                                                     sign   = c_i
                                                    option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low
    INTO TABLE @tl_tvarvc_3
    FROM tvarvc
    WHERE name = 'ZRET_IVA_RET_SUB_HONORARIOS'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_zret_iva_ret_sub_honorarios = VALUE  #(
                   BASE rg_zret_iva_ret_sub_honorarios ( low    = <fs_tvarvc>-low
                                                     sign   = c_i
                                                    option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low
    INTO TABLE @tl_tvarvc_3
    FROM tvarvc
    WHERE name = 'ZRET_RET_ISR_HONORARIOS'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_zret_ret_isr_honorarios = VALUE  #(
                   BASE rg_zret_ret_isr_honorarios ( low    = <fs_tvarvc>-low
                                                     sign   = c_i
                                                    option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
    INTO TABLE @tl_tvarvc_3
    FROM tvarvc
    WHERE name = 'Z_CTAS_SI'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_z_ctas_si = VALUE  #(
                   BASE rg_z_ctas_si ( low    = <fs_tvarvc>-low
                                                     sign   = c_i
                                                    option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low
    INTO TABLE @tl_tvarvc_3
    FROM tvarvc
    WHERE name = 'ZCONCEPTOS_NOMINA_EXCLUSION_S'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_nomina_exclusion_s = VALUE  #(
                   BASE rg_nomina_exclusion_s  ( low    = <fs_tvarvc>-low
                                                     sign   = c_i
                                                    option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low
  INTO TABLE @tl_tvarvc_3
  FROM tvarvc
  WHERE name = 'ZCONCEPTOS_NOMINA_EXCLUSION_H'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_nomina_exclusion_h = VALUE  #(
                   BASE rg_nomina_exclusion_h ( low    = <fs_tvarvc>-low
                                                     sign   = c_i
                                                    option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low
  INTO TABLE @tl_tvarvc_3
  FROM tvarvc
  WHERE name = 'ZRET_RET_ISR_ARRENDAMIENTO'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_zret_ret_isr_arrendamiento = VALUE  #(
                   BASE rg_zret_ret_isr_arrendamiento ( low    = <fs_tvarvc>-low
                                                        sign   = c_i
                                                        option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low
  INTO TABLE @tl_tvarvc_3
  FROM tvarvc
  WHERE name = 'ZRET_IVA_RET_SUB_ARREN'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_zret_iva_ret_sub_arren = VALUE  #(
                   BASE rg_zret_iva_ret_sub_arren ( low    = <fs_tvarvc>-low
                                                        sign   = c_i
                                                        option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
 INTO TABLE @tl_tvarvc_3
 FROM tvarvc
 WHERE name = 'ZIVA_0%'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_ziva_0 = VALUE  #(
                   BASE rg_ziva_0 ( low    = <fs_tvarvc>-low
                                    sign   = c_i
                                    option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low
INTO TABLE @tl_tvarvc_3
FROM tvarvc
WHERE name = 'Z_INTERMEDIARIO_FINA'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_lifnr = VALUE  #(
                   BASE rg_lifnr ( low    = <fs_tvarvc>-low
                                    sign   = c_i
                                    option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low
INTO TABLE @tl_tvarvc_3
FROM tvarvc
WHERE name = 'Z_REFERENCIA_INTERME'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_z_referencia_interme = VALUE  #(
                   BASE rg_z_referencia_interme ( low    = <fs_tvarvc>-low
                                    sign   = c_i
                                    option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low
INTO TABLE @tl_tvarvc_3
FROM tvarvc
WHERE name = 'Z_FONDO_FIJO'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_3 ASSIGNING <fs_tvarvc>.
      rg_z_fondo_fijo = VALUE  #(
                   BASE rg_z_fondo_fijo ( low    = <fs_tvarvc>-low
                                    sign   = c_i
                                    option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low,high
  INTO TABLE @tl_tvarvc_4
  FROM tvarvc
  WHERE name = 'ZCONCEPTOS_RESUMEN_NOMINA'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_resumen_nomina = VALUE  #(
                   BASE rg_resumen_nomina ( low    = <fs_tvarvc4>-low
                                            high   = <fs_tvarvc4>-high
                                            sign   = c_i
                                            option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  SELECT low,high
  INTO TABLE @tl_tvarvc_4
  FROM tvarvc
  WHERE name = 'Z_ACREEDORES_MOVTO'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_acreedores_movto = VALUE  #(
                   BASE rg_acreedores_movto ( low    = <fs_tvarvc4>-low
                                            sign   = c_i
                                            option = c_eq ) ).
      rg_acreedores_movto_high = VALUE  #(
                   BASE rg_acreedores_movto_high ( low    = <fs_tvarvc4>-high
                                                sign   = c_i
                                                option = c_eq ) ).
    ENDLOOP.
  ENDIF.
  SELECT low,high
 INTO TABLE @tl_tvarvc_4
 FROM tvarvc
 WHERE name = 'Z_COMPRO_GASTO'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_compro_gasto = VALUE  #(
                   BASE rg_compro_gasto   ( low    = <fs_tvarvc4>-low
                                            sign   = c_i
                                            option = c_eq ) ).
      rg_compro_gasto_high = VALUE  #(
                   BASE rg_compro_gasto_high ( low    = <fs_tvarvc4>-high
                                                sign   = c_i
                                                option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low,high
  INTO TABLE @tl_tvarvc_4
  FROM tvarvc
  WHERE name = 'ZIVA_8%'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_ziva_8_low = VALUE  #(
                   BASE rg_ziva_8_low ( low    = <fs_tvarvc4>-low
                                            sign   = c_i
                                            option = c_eq ) ).
      rg_ziva_8_high = VALUE  #(
                   BASE rg_ziva_8_high ( low    = <fs_tvarvc4>-high
                                                sign   = c_i
                                                option = c_eq ) ).
    ENDLOOP.
  ENDIF.

*--> Insert AMP 28.12.2020
  REFRESH tl_tvarvc_4[].

  SELECT low,high
 INTO TABLE @tl_tvarvc_4
 FROM tvarvc
 WHERE name = 'ZCARGO_ABONO_COMPRO'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_cargo_abono_compro_low = VALUE  #(
                   BASE rg_cargo_abono_compro_low   ( low    = <fs_tvarvc4>-low
                                            sign   = c_i
                                            option = c_eq ) ).
    ENDLOOP.
  ENDIF.
*<-- End insert AMP 28.12.2020


  REFRESH tl_tvarvc_4[].

  SELECT low,high
 INTO TABLE @tl_tvarvc_4
 FROM tvarvc
 WHERE name = 'ZCARGO_ABONO'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_cargo_abono_low = VALUE  #(
                   BASE rg_cargo_abono_low  ( low    = <fs_tvarvc4>-low
                                            sign   = c_i
                                            option = c_eq ) ).
    ENDLOOP.
  ENDIF.



  REFRESH tl_tvarvc_4[].

  SELECT low,high
 INTO TABLE @tl_tvarvc_4
 FROM tvarvc
 WHERE name = 'ZNOMINA_CTAS_A_DETALLAR'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_nomina_ctas_a_detallar_low  = VALUE  #(
                   BASE rg_nomina_ctas_a_detallar_low   ( low    = <fs_tvarvc4>-low
                                            sign   = c_i
                                            option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  REFRESH tl_tvarvc_4[].
  SELECT low,high
INTO TABLE @tl_tvarvc_4
FROM tvarvc
WHERE name = 'ZCTAS_SAL_CERO'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_ctas_saldo_cero = VALUE  #(
                  BASE  rg_ctas_saldo_cero ( low    = <fs_tvarvc4>-low
                                           sign   = c_i
                                           option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  REFRESH tl_tvarvc_4[].
  SELECT low,high
INTO TABLE @tl_tvarvc_4
FROM tvarvc
WHERE name = 'ZCLAVE_EGRESO'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_clave_egreso  = VALUE  #(
                 BASE   rg_clave_egreso  ( low    = <fs_tvarvc4>-low
                                          sign   = c_i
                                          option = c_eq ) ).
    ENDLOOP.
  ENDIF.
*factoraje
  REFRESH tl_tvarvc_4[].
  SELECT low,high
  INTO TABLE @tl_tvarvc_4
  FROM tvarvc
  WHERE name = 'ZREGRE_FAC_CD'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_factoraje_egreso_cd  = VALUE  #(
                 BASE  rg_factoraje_egreso_cd ( low    = <fs_tvarvc4>-low
                                          sign   = c_i
                                          option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  REFRESH tl_tvarvc_4[].
  SELECT low,high
  INTO TABLE @tl_tvarvc_4
  FROM tvarvc
  WHERE name = 'ZREGRE_FAC_FISCA'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_facturas_fiscales_cd  = VALUE  #(
                BASE   rg_facturas_fiscales_cd ( low    = <fs_tvarvc4>-low
                                         sign   = c_i
                                         option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  REFRESH tl_tvarvc_4[].
  SELECT low,high
  INTO TABLE @tl_tvarvc_4
  FROM tvarvc
  WHERE name = 'ZREGRE_FAF_CTAS_BAS'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_ctas_fac_fis  = VALUE  #(
                BASE   rg_ctas_fac_fis ( low    = <fs_tvarvc4>-low
                                         sign   = c_i
                                         option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  REFRESH tl_tvarvc_4[].
  SELECT low,high
INTO TABLE @tl_tvarvc_4
FROM tvarvc
WHERE name = 'ZIMP_RET_CEDULAR'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_imp_ret_cedular  = VALUE  #(
                 BASE    rg_imp_ret_cedular ( low    = <fs_tvarvc4>-low
                                          sign   = c_i
                                          option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  REFRESH tl_tvarvc_4[].

  SELECT low,high
INTO TABLE @tl_tvarvc_4
FROM tvarvc
WHERE name = 'ZIMP_RET_ISR_PAGEXT'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc_4 ASSIGNING <fs_tvarvc4>.
      rg_ret_isr_pagext  = VALUE  #(
                  BASE    rg_ret_isr_pagext ( low    = <fs_tvarvc4>-low
                                           sign   = c_i
                                           option = c_eq ) ).
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GUARDAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM guardar .

  CLEAR:vl_answer.
  IF vl_entro IS NOT INITIAL.
    CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
      EXPORTING
*       DEFAULTOPTION       = 'Y'
        textline1 = '¿Desea sobreescribir la información?'
*       TEXTLINE2 = ' '
        titel     = 'Advertencia'
*       START_COLUMN        = 25
*       START_ROW = 6
      IMPORTING
        answer    = vl_answer.

  ENDIF.

  IF tl_report IS NOT INITIAL AND ( vl_answer = 'J' OR vl_answer IS INITIAL ).
    vl_entro = abap_on.
    REFRESH tl_ztax_detalle_egr.
    DATA(vl_total_registros_aux) = vl_total_registros.
    LOOP AT tl_report ASSIGNING FIELD-SYMBOL(<fs_report>).
      ADD 1 TO vl_total_registros_aux.
      tl_ztax_detalle_egr = VALUE  #(
                         BASE tl_ztax_detalle_egr ( id                         = vl_total_registros_aux
                                                    bukrs                      = <fs_report>-n_doc_pago_sociedad
                                                    gjahr                      = <fs_report>-n_doc_pago_ejercicio
                                                    periodo                    = <fs_report>-fechapago+4(2)
                                                    datum                      = sy-datum
                                                    usuario                    = sy-uname
                                                    uzeit                      = sy-uzeit
                                                    extracto                   = p_pag_c
                                                    fechapago                  = <fs_report>-fechapago
                                                    descripcion                = <fs_report>-descripcion
                                                    totalcargos                = <fs_report>-totalcargos
                                                    referencia                 = <fs_report>-referencia
                                                    concepto                   = <fs_report>-concepto
                                                    n_doc_pago                 = <fs_report>-n_doc_pago
                                                    n_sol_anticipo             = <fs_report>-n_sol_anticipo
                                                    n_documento_fac            = <fs_report>-n_documento_fac
                                                    clasificacion              = <fs_report>-clasificacion
                                                    foliofiscal                = <fs_report>-foliofiscal
                                                    impor_mon_l_fac            = <fs_report>-impor_mon_l_fac
                                                    rfc                        = <fs_report>-rfc
                                                    razon_s                    = <fs_report>-razon_s
                                                    acredor                    = <fs_report>-acredor
                                                    ind_iva_16                 = <fs_report>-ind_iva_16
                                                    base_16                    = <fs_report>-base_16
                                                    indi_iva_8                 = <fs_report>-indi_iva_8
                                                    base_8                     = <fs_report>-base_8
                                                    ind_iva_0                  = <fs_report>-ind_iva_0
                                                    base_0                     = <fs_report>-base_0
                                                    ind_exento                 = <fs_report>-ind_exento
                                                    base_exento                = <fs_report>-base_exento
                                                    ind_no_obj                 = <fs_report>-ind_no_obj
                                                    base_no_obj                = <fs_report>-base_no_obj
                                                    ind_nodedu                 = <fs_report>-ind_nodedu
                                                    base_nodedu                = <fs_report>-base_nodedu
                                                    iva_16                     = <fs_report>-iva_16
                                                    iva_8                      = <fs_report>-iva_8
                                                    impu_iva_virtual           = <fs_report>-impu_iva_virtual
                                                    iva_ret_arren              = <fs_report>-iva_ret_arren
                                                    iva_ret_serv_prof          = <fs_report>-iva_ret_serv_prof
                                                    iva_ret_6                  = <fs_report>-iva_ret_6
                                                    iva_retenido_4             = <fs_report>-iva_retenido_4 "fletes
                                                    imp_ret_cedular            = <fs_report>-imp_ret_cedular
                                                    isr_honorarios             = <fs_report>-isr_honorarios
                                                    isr_arrendamiento          = <fs_report>-isr_arrendamiento
                                                    isr_pagos_extranjero       = <fs_report>-isr_pagos_extranjero
                                                    isr_dividendos             = <fs_report>-isr_dividendos
                                                    isr_intereses              = <fs_report>-isr_intereses
                                                    isr_venta_acciones_retener = <fs_report>-isr_venta_acciones_retener
                                                    z_2_sar                    = <fs_report>-2_sar
                                                    z_5_infonavit              = <fs_report>-5_infonavit
                                                    ayuda_sindic_x_pagar       = <fs_report>-ayuda_sindic_x_pagar
                                                    casetas                    = <fs_report>-casetas
                                                    cesantia_vejez             = <fs_report>-cesantia_vejez
                                                    compens_extraordinar       = <fs_report>-compens_extraordinar
                                                    cuota_patronal_imss        = <fs_report>-cuota_patronal_imss
                                                    domingos_dias_fest         = <fs_report>-domingos_dias_fest
                                                    fondo_ahorro               = <fs_report>-fondo_ahorro
                                                    otras_prestaciones         = <fs_report>-otras_prestaciones
                                                    fac_admvas_maniobras       = <fs_report>-fac_admvas_maniobras
                                                    fac_admvas_gts_camino      = <fs_report>-fac_admvas_gts_camino
                                                    fac_admvas_gts_talachas    = <fs_report>-fac_admvas_gts_talachas
                                                    fac_admvas_gts_rep_men     = <fs_report>-fac_admvas_gts_rep_men
                                                    fac_admvas_gts_fianzas     = <fs_report>-fac_admvas_gts_fianzas
                                                    otros_gastos_viaje         = <fs_report>-otros_gastos_viaje
                                                    premio_puntualidad         = <fs_report>-premio_puntualidad
                                                    prima_dominical            = <fs_report>-prima_dominical
                                                    rva_aguinaldo              = <fs_report>-rva_aguinaldo
                                                    rva_prima_vacacional       = <fs_report>-rva_prima_vacacional
                                                    subsidio_empleo            = <fs_report>-subsidio_empleo
                                                    sueldos                    = <fs_report>-sueldos
                                                    viaticos                   = <fs_report>-viaticos
                                                    sueldos_por_pagar          = <fs_report>-sueldos_por_pagar
                                                    tiempo_extra_doble         = <fs_report>-tiempo_extra_doble
                                                    tiempo_extra_triple        = <fs_report>-tiempo_extra_triple
                                                    cesantia_vejez_empl        = <fs_report>-cesantia_vejez_empl
                                                    cesantia_vejez_patro       = <fs_report>-cesantia_vejez_patro
                                                    descuento_infonacot        = <fs_report>-descuento_infonacot
                                                    finiquito_por_pagar        = <fs_report>-finiquito_por_pagar
                                                    fondo_ahorro_emplead       = <fs_report>-fondo_ahorro_emplead
                                                    fondo_ahorro_empresa       = <fs_report>-fondo_ahorro_empresa
                                                    imss_patronal              = <fs_report>-imss_patronal
                                                    imss_retenido              = <fs_report>-imss_retenido
                                                    infonavit_patronal         = <fs_report>-infonavit_patronal
                                                    infonavit_retenido         = <fs_report>-infonavit_retenido
                                                    ing_x_descto_comedor       = <fs_report>-ing_x_descto_comedor
                                                    ing_x_dev_empleados        = <fs_report>-ing_x_dev_empleados
                                                    isr_ret_x_sueldos          = <fs_report>-isr_ret_x_sueldos
                                                    prov_agui_x_pagar          = <fs_report>-prov_agui_x_pagar
                                                    prov_prim_vacacional       = <fs_report>-prov_prim_vacacional
                                                    prima_vacacional           = <fs_report>-prima_vacacional
                                                    pension_alim_total         = <fs_report>-pension_alim_total
                                                    seguro_viv_infon_tot       = <fs_report>-seguro_viv_infon_tot
                                                    subsidio_empleo_tot        = <fs_report>-subsidio_empleo_tot
                                                    total                      = <fs_report>-total                    ) ).
    ENDLOOP.
    MODIFY ztax_detalle_egr FROM TABLE tl_ztax_detalle_egr.
    MESSAGE TEXT-070 TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form OCULTA_CAMPOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM oculta_campos .

  IF sy-uname NE 'VRODRIGUEZ'.
    LOOP AT SCREEN.
      IF screen-name CS 'S_BELNR'.
        screen-invisible = 1.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REVISA_TOTALES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM revisa_totales .
  TYPES: BEGIN OF ty_pagos,
           bukrs      TYPE bukrs,
           gjahr      TYPE gjahr,
           periodo    TYPE zperiodo,
           n_doc_pago TYPE belnr_d,
         END OF ty_pagos.

  TYPES: BEGIN OF ty_facturas,
           bukrs           TYPE bukrs,
           gjahr           TYPE gjahr,
           periodo         TYPE zperiodo,
           n_doc_pago      TYPE belnr_d,
           n_documento_fac TYPE belnr_d,
           indice          TYPE sy-tabix,
         END OF ty_facturas.

  DATA: tl_pago_factura TYPE TABLE OF ty_pagos,
        wa_pago_factura LIKE LINE OF  tl_pago_factura,
        tl_facturas     TYPE TABLE OF  ty_facturas,
        wa_facturas     LIKE LINE OF   tl_facturas,
        lv_indice       TYPE sy-tabix,
        no_factura_ant  TYPE belnr_d.

  FIELD-SYMBOLS: <fs_tl_report> TYPE  ty_report.
* Determina pagos
  LOOP AT tl_report INTO sl_report.
    MOVE: sl_report-bukrs           TO wa_pago_factura-bukrs,
          sl_report-gjahr           TO wa_pago_factura-gjahr,
          sl_report-periodo         TO wa_pago_factura-periodo,
          sl_report-n_doc_pago      TO wa_pago_factura-n_doc_pago.
    APPEND wa_pago_factura    TO tl_pago_factura.
    CLEAR wa_pago_factura.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM tl_pago_factura COMPARING bukrs gjahr gjahr periodo n_doc_pago .

*Procesa pagos y numera pagos de 1 al n

  LOOP AT tl_pago_factura INTO wa_pago_factura.
    CLEAR lv_indice.
    LOOP AT  tl_report ASSIGNING <fs_tl_report> WHERE n_doc_pago = wa_pago_factura-n_doc_pago.
      ADD 1 TO lv_indice.
      MOVE:<fs_tl_report>-bukrs      TO wa_facturas-bukrs,
      <fs_tl_report>-gjahr           TO wa_facturas-gjahr,
      <fs_tl_report>-periodo         TO wa_facturas-periodo,
      <fs_tl_report>-n_doc_pago      TO wa_facturas-n_doc_pago,
      <fs_tl_report>-n_documento_fac TO wa_facturas-n_documento_fac,
      lv_indice                      TO wa_facturas-indice,
      lv_indice                      TO <fs_tl_report>-indice.
      APPEND wa_facturas        TO tl_facturas.
      CLEAR wa_facturas.
    ENDLOOP.
  ENDLOOP.
*Borra los pagos 1 para que se conserve solo los dos
  DELETE tl_facturas WHERE indice EQ 1.
* se recorre la tabla  y elimina cantidades
  LOOP AT tl_report ASSIGNING <fs_tl_report>.
    CLEAR no_factura_ant.
    READ TABLE tl_facturas INTO wa_facturas
    WITH KEY
    bukrs        = <fs_tl_report>-bukrs
 gjahr           = <fs_tl_report>-gjahr
 periodo         = <fs_tl_report>-periodo
 n_doc_pago      = <fs_tl_report>-n_doc_pago
 n_documento_fac = <fs_tl_report>-n_documento_fac
 indice          = <fs_tl_report>-indice.
    IF sy-subrc = 0.
      MOVE 0 TO <fs_tl_report>-totalcargos.
      IF no_factura_ant EQ  <fs_tl_report>-n_documento_fac.
        MOVE 0 TO <fs_tl_report>-impor_mon_l_fac.
      ENDIF.

    ELSE.
      no_factura_ant  = <fs_tl_report>-n_documento_fac.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& GOTO_FB03
*& CALL TRANSACTION FB03
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM GOTO_FB03 USING
       doc TYPE ty_param_transac-nro_doc
       soc TYPE ty_param_transac-society
       exe TYPE ty_param_transac-exercice.

  SET PARAMETER ID 'BLN' FIELD doc.
  SET PARAMETER ID 'BUK' FIELD soc.
  SET PARAMETER ID 'GJR' FIELD exe.

  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ZSTANDARD'.
* SET TITLEBAR 'xxx'.

  "***** REPORTE GLOBAL *****"
  CLEAR: wa_layout,
         wa_variant.

  PERFORM build_fieldcat. "Propiedades de columnas

  IF ob_grid IS INITIAL.
    CREATE OBJECT lo_container
      EXPORTING
        container_name = 'ALV_CONTAINER'.

    CREATE OBJECT ob_grid
      EXPORTING
        i_parent = lo_container.
  ENDIF.

  IF lo_event_handler IS NOT BOUND.
    CREATE OBJECT lo_event_handler.
  ENDIF.

  SET HANDLER: lo_event_handler->handle_hotspot_click FOR ob_grid,
               lo_event_handler->on_doble_click FOR ob_grid.


  wa_variant = GX_VARIANT.
  wa_layout-zebra = 'X'.
  wa_layout-CWIDTH_OPT = 'X'.
  CALL METHOD ob_grid->set_table_for_first_display
    EXPORTING
      is_variant                    = wa_variant "Estos dos parámetros se pasan para poder guardar variantes del layout
      i_save                        = 'A'
      is_layout                     = wa_layout
    CHANGING
      it_outtab                     = tl_report[]
      it_fieldcatalog               = tl_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

ENDMODULE.
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_hotspot_click.
*    PERFORM fm_alv_resumen USING e_row_id e_column_id es_row_no sender. "Construimos el ALV de resumen de posiciones
  ENDMETHOD."on_link_click

  METHOD on_doble_click.

    IF e_column EQ 'N_DOC_PAGO' OR
       e_column EQ 'N_DOCUMENTO_FAC' OR
       e_column EQ 'N_SOL_ANTICIPO'.
      DATA: wa_record TYPE ty_report.
      READ TABLE tl_report INTO wa_record INDEX es_row_no-row_id.
      CASE e_column.
        WHEN 'N_DOC_PAGO'.
          PERFORM GOTO_FB03
            USING wa_record-n_doc_pago " nro Document
                  wa_record-bukrs      " Society
                  wa_record-gjahr.     " exercise
        WHEN 'N_DOCUMENTO_FAC'.
          PERFORM GOTO_FB03
            USING wa_record-n_documento_fac            " nro Document
                  wa_record-n_documento_fac_sociedad   " Society
                  wa_record-n_documento_fac_ejercicio. " exercise
        WHEN 'N_SOL_ANTICIPO'.
          IF wa_record-n_sol_anticipo IS NOT INITIAL.
          PERFORM GOTO_FB03
            USING wa_record-n_sol_anticipo            " nro Document
                  wa_record-n_sol_anticipo_sociedad   " Society
                  wa_record-n_sol_anticipo_ejercicio. " exercise
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDMETHOD. "on_double_click method

ENDCLASS. "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE sy-ucomm .
    WHEN '&F03' OR "Cuando presionen cualquier botón para salir
          '&F15' OR
          '&F12'.
      SET SCREEN 0.
      LEAVE TO SCREEN 0.
    WHEN '&SAV'.
      PERFORM guardar.
  ENDCASE.
ENDMODULE.
