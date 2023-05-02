"------------------------------------------------------------------------
"  CODIGO ACTUAL DEV
"------------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Include          ZTR_ISRMENSUAL_C_INGRESOS_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form RANGOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM rangos .

  SELECT low, high
    INTO TABLE @DATA(tl_tvarvc)
    FROM tvarvc
    WHERE name = 'ZABONO_CARGO_CLIENTE'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc ASSIGNING FIELD-SYMBOL(<fs_tvarvc>).
      rg_abono_cargo = VALUE  #(
                   BASE rg_abono_cargo ( low    = <fs_tvarvc>-low
                                     sign   = c_i
                                     option = c_eq ) ).
      rg_abono_cargo_high = VALUE  #(
                        BASE rg_abono_cargo_high ( low    = <fs_tvarvc>-high
                       sign   = c_i
                       option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
   INTO TABLE @DATA(tl_tvarvc1)
   FROM tvarvc
   WHERE name = 'ZCLAVE_DEUDOR'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc1 ASSIGNING FIELD-SYMBOL(<fs_tvarvc1>).
      rg_clave_deudor = VALUE  #(
                   BASE rg_clave_deudor ( low    = <fs_tvarvc1>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
   INTO TABLE @DATA(tl_tvarvc2)
   FROM tvarvc
   WHERE name = 'ZCLAVE_ING'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc2 ASSIGNING FIELD-SYMBOL(<fs_tvarvc2>).
      rg_clave_ing = VALUE  #(
                   BASE rg_clave_ing ( low    = <fs_tvarvc2>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc3)
  FROM tvarvc
  WHERE name = 'ZCTAS_ING_VTA'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc3 ASSIGNING FIELD-SYMBOL(<fs_tvarvc3>).
      rg_ctas_ing_vta = VALUE  #(
                   BASE rg_ctas_ing_vta ( low    = <fs_tvarvc3>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc4)
  FROM tvarvc
  WHERE name = 'ZCLAVE_IVA'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc4 ASSIGNING FIELD-SYMBOL(<fs_tvarvc4>).
      rg_clave_iva = VALUE  #(
                   BASE rg_clave_iva ( low    = <fs_tvarvc4>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc5)
  FROM tvarvc
  WHERE name = 'ZCTAS_IVA_ING'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc5 ASSIGNING FIELD-SYMBOL(<fs_tvarvc5>).
      rg_ctas_iva_ing = VALUE  #(
                   BASE rg_ctas_iva_ing ( low    = <fs_tvarvc5>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
   INTO TABLE @DATA(tl_tvarvc6)
   FROM tvarvc
   WHERE name = 'ZCLAVE_RET_IVA'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc6 ASSIGNING FIELD-SYMBOL(<fs_tvarvc6>).
      rg_clave_ret_iva = VALUE  #(
                   BASE rg_clave_ret_iva ( low    = <fs_tvarvc6>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc7)
  FROM tvarvc
  WHERE name = 'ZCTAS_RET_IVA_ING'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc7 ASSIGNING FIELD-SYMBOL(<fs_tvarvc7>).
      rg_ctas_ret_iva_ing = VALUE  #(
                   BASE rg_ctas_ret_iva_ing ( low    = <fs_tvarvc7>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
     INTO TABLE @DATA(tl_tvarvc9)
     FROM tvarvc
     WHERE name = 'ZDOC_COB_C'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc9 ASSIGNING FIELD-SYMBOL(<fs_tvarvc9>).
      rg_tvar_doc = VALUE  #(
                   BASE rg_tvar_doc ( low    = <fs_tvarvc9>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT  SIGN,
          OPTI,
          LOW,
          HIGH
INTO TABLE @DATA(tl_tvarvc10)
FROM tvarvc
WHERE name = @c_zbancos_ingresos.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc10 ASSIGNING FIELD-SYMBOL(<fs_tvarvc10>).
      rg_bancos_ingresos = VALUE  #(
                   BASE rg_bancos_ingresos ( low    = <fs_tvarvc10>-low
                                     sign   = <fs_tvarvc10>-sign
                                     option = <fs_tvarvc10>-opti ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc11)
  FROM tvarvc
  WHERE name = @c_zcargo_abono.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc11 ASSIGNING FIELD-SYMBOL(<fs_tvarvc11>).
      rg_cargo_abono = VALUE  #(
                   BASE rg_cargo_abono ( low    = <fs_tvarvc11>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
   INTO TABLE @DATA(tl_tvarvc12)
   FROM tvarvc
   WHERE name = @c_zdoc_fac_cl.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc12 ASSIGNING FIELD-SYMBOL(<fs_tvarvc12>).
      rg_doc_fac_cl = VALUE  #(
                   BASE rg_doc_fac_cl ( low    = <fs_tvarvc12>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc13)
  FROM tvarvc
  WHERE name = 'ZCUENTAS_ING_BAN'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc13 ASSIGNING FIELD-SYMBOL(<fs_tvarvc13>).
      rg_cuentas_ing_ban = VALUE  #(
                   BASE rg_cuentas_ing_ban ( low    = <fs_tvarvc13>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc14)
  FROM tvarvc
  WHERE name = 'ZCTAS_COMISION'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc14 ASSIGNING FIELD-SYMBOL(<fs_tvarvc14>).
      rg_ctas_comision = VALUE  #(
                   BASE rg_ctas_comision ( low    = <fs_tvarvc14>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
    INTO TABLE @DATA(tl_tvarvc15)
    FROM tvarvc
    WHERE name = 'ZCLAVE_COMISION'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc15 ASSIGNING FIELD-SYMBOL(<fs_tvarvc15>).
      rg_clave_comision = VALUE  #(
                   BASE rg_clave_comision ( low    = <fs_tvarvc15>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc16)
  FROM tvarvc
  WHERE name = 'ZCLAVE_BALANCE'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc16 ASSIGNING FIELD-SYMBOL(<fs_tvarvc16>).
      rg_clave_balance = VALUE  #(
                   BASE rg_clave_balance ( low    = <fs_tvarvc16>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc17)
  FROM tvarvc
  WHERE name = 'ZCUENTAS_PER_REL'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc17 ASSIGNING FIELD-SYMBOL(<fs_tvarvc17>).
      rg_cuentas_per_rel = VALUE  #(
                   BASE rg_cuentas_per_rel ( low    = <fs_tvarvc17>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc18)
  FROM tvarvc
  WHERE name = 'ZCLAVE_UTIL_REA'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc18 ASSIGNING FIELD-SYMBOL(<fs_tvarvc18>).
      rg_clave_util_rea = VALUE  #(
                   BASE rg_clave_util_rea ( low    = <fs_tvarvc18>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc19)
  FROM tvarvc
  WHERE name = 'ZCUENTAS_UTIL_REL'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc19 ASSIGNING FIELD-SYMBOL(<fs_tvarvc19>).
      rg_cuentas_util_rel = VALUE  #(
                   BASE rg_cuentas_util_rel ( low    = <fs_tvarvc19>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc20)
  FROM tvarvc
  WHERE name = 'ZBASE_16'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc20 ASSIGNING FIELD-SYMBOL(<fs_tvarvc20>).
      rg_base_16 = VALUE  #(
                   BASE rg_base_16 ( low    = <fs_tvarvc20>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc21)
  FROM tvarvc
  WHERE name = 'ZBASE_EXC'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc21 ASSIGNING FIELD-SYMBOL(<fs_tvarvc21>).
      rg_base_exc = VALUE  #(
                   BASE rg_base_exc ( low    = <fs_tvarvc21>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc22)
  FROM tvarvc
  WHERE name = 'ZCTAS_IVA_VTA'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc22 ASSIGNING FIELD-SYMBOL(<fs_tvarvc22>).
      rg_ctas_iva_vta = VALUE  #(
                   BASE rg_ctas_iva_vta ( low    = <fs_tvarvc22>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
 INTO TABLE @DATA(tl_tvarvc23)
 FROM tvarvc
 WHERE name = 'ZCTAS_RET_IVA_ING_4'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc23 ASSIGNING FIELD-SYMBOL(<fs_tvarvc23>).
      rg_ctas_ret_iva_ing_4 = VALUE  #(
                   BASE rg_ctas_ret_iva_ing_4 ( low    = <fs_tvarvc23>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low
  INTO TABLE @DATA(tl_tvarvc24)
  FROM tvarvc
  WHERE name = 'ZBASE_0'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc24 ASSIGNING FIELD-SYMBOL(<fs_tvarvc24>).
      rg_base_0 = VALUE  #(
                   BASE rg_base_0 ( low    = <fs_tvarvc24>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  SELECT low
  INTO TABLE @DATA(tl_tvarvc25)
  FROM tvarvc
  WHERE name = 'ZSTUFE'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc25 ASSIGNING FIELD-SYMBOL(<fs_tvarvc25>).
      rg_stufe = VALUE  #(
                   BASE rg_stufe ( low    = <fs_tvarvc25>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
    INTO TABLE @DATA(tl_tvarvc26)
    FROM tvarvc
    WHERE name = 'ZTR_CTAS_ORIGEN'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc26 ASSIGNING FIELD-SYMBOL(<fs_tvarvc26>).
      rg_cuentas_origen = VALUE  #(
                   BASE rg_cuentas_origen ( low    = <fs_tvarvc26>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
  INTO TABLE @DATA(tl_tvarvc27)
  FROM tvarvc
  WHERE name = 'ZTR_CTAS_DESTINO'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc27 ASSIGNING FIELD-SYMBOL(<fs_tvarvc27>).
      rg_cuentas_destino = VALUE  #(
                   BASE rg_cuentas_destino ( low    = <fs_tvarvc27>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  SELECT low, high
INTO TABLE @DATA(tl_tvarvc28)
FROM tvarvc
WHERE name = 'ZTR_TRASPASO'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc28 ASSIGNING FIELD-SYMBOL(<fs_tvarvc28>).
      rg_docs_traspaso = VALUE  #(
                   BASE rg_docs_traspaso
                                   ( low    = <fs_tvarvc28>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
    INTO TABLE @DATA(tl_tvarvc29)
    FROM tvarvc
    WHERE name = 'Z_ING_RECUPERA_TD'.

  IF sy-subrc = 0.
    LOOP AT tl_tvarvc29 ASSIGNING FIELD-SYMBOL(<fs_tvarvc29>).
      rg_docs_ing_recup = VALUE  #(
                   BASE rg_docs_ing_recup ( low    = <fs_tvarvc29>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
    INTO TABLE @DATA(tl_tvarvc30)
    FROM tvarvc
    WHERE name = 'Z_ING_RECUPERA'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc30 ASSIGNING FIELD-SYMBOL(<fs_tvarvc30>).
      rg_docs_cta_ingrec = VALUE  #(
                   BASE rg_docs_cta_ingrec ( low    = <fs_tvarvc30>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
    INTO TABLE @DATA(tl_tvarvc31)
    FROM tvarvc
    WHERE name = 'Z_DOC_INTERESES'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc31 ASSIGNING FIELD-SYMBOL(<fs_tvarvc31>).
      rg_docs_intereses = VALUE  #(
                   BASE rg_docs_intereses ( low    = <fs_tvarvc31>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
  INTO TABLE @DATA(tl_tvarvc32)
  FROM tvarvc
  WHERE name = 'Z_INT_CTAPTE'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc32 ASSIGNING FIELD-SYMBOL(<fs_tvarvc32>).
      rg_cta_pte = VALUE  #(
                   BASE rg_cta_pte ( low    = <fs_tvarvc32>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
INTO TABLE @DATA(tl_tvarvc33)
FROM tvarvc
WHERE name = 'Z_CTA_INT'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc33 ASSIGNING FIELD-SYMBOL(<fs_tvarvc33>).
      rg_cta_intereses = VALUE  #(
                   BASE rg_cta_intereses ( low    = <fs_tvarvc33>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.



  SELECT low, high
INTO TABLE @DATA(tl_tvarvc34)
FROM tvarvc
WHERE name = 'ZTR_TDOC_INVER'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc34 ASSIGNING FIELD-SYMBOL(<fs_tvarvc34>).
      rg_doc_inver = VALUE  #(
                   BASE rg_doc_inver ( low    = <fs_tvarvc34>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  SELECT low, high
      INTO TABLE @DATA(tl_tvarvc35)
      FROM tvarvc
      WHERE name = 'ZTR_INV_CONCEN'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc35 ASSIGNING FIELD-SYMBOL(<fs_tvarvc35>).
      rg_doc_inver_conc = VALUE  #(
                   BASE rg_doc_inver_conc ( low    = <fs_tvarvc35>-low
                                            sign   = c_i
                                            option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
    INTO TABLE @DATA(tl_tvarvc36)
    FROM tvarvc
    WHERE name = 'Z_CTA_INTERCOS'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc36 ASSIGNING FIELD-SYMBOL(<fs_tvarvc36>).
      rg_ctas_intercos = VALUE  #(
                   BASE rg_ctas_intercos ( low    = <fs_tvarvc36>-low
                                          sign   = c_i
                                          option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
  INTO TABLE @DATA(tl_tvarvc37)
  FROM tvarvc
  WHERE name = 'Z_DOC_INTERCOS'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc37 ASSIGNING FIELD-SYMBOL(<fs_tvarvc37>).
      rg_doc_intercos = VALUE  #(
                   BASE rg_doc_intercos ( low    = <fs_tvarvc37>-low
                                          sign   = c_i
                                          option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  SELECT low, high
  INTO TABLE @DATA(tl_tvarvc38)
  FROM tvarvc
  WHERE name = 'Z_CTA_OT_ING'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc38 ASSIGNING FIELD-SYMBOL(<fs_tvarvc38>).
      rg_ctas_otring = VALUE  #(
                   BASE rg_ctas_otring ( low    = <fs_tvarvc38>-low
                                         sign   = c_i
                                         option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
  INTO TABLE @DATA(tl_tvarvc39)
  FROM tvarvc
  WHERE name = 'Z_CTA_OT_DEP'.

  IF sy-subrc = 0.
    LOOP AT tl_tvarvc39 ASSIGNING FIELD-SYMBOL(<fs_tvarvc39>).
      rg_ctas_otdep = VALUE  #(
                   BASE rg_ctas_otdep ( low    = <fs_tvarvc39>-low
                                        sign   = c_i
                                        option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
  INTO TABLE @DATA(tl_tvarvc40)
  FROM tvarvc
  WHERE name = 'Z_CTA_DESCONT'.

  IF sy-subrc = 0.
    LOOP AT tl_tvarvc40 ASSIGNING FIELD-SYMBOL(<fs_tvarvc40>).
      rg_fact_sanesp = VALUE  #(
                   BASE rg_fact_sanesp ( low    = <fs_tvarvc40>-low
                                        sign   = c_i
                                        option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  SELECT low, high
  INTO TABLE @DATA(tl_tvarvc41)
  FROM tvarvc
  WHERE name = 'Z_DOC_PAGO'.

  IF sy-subrc = 0.
    LOOP AT tl_tvarvc41 ASSIGNING FIELD-SYMBOL(<fs_tvarvc41>).
      rg_doc_pago = VALUE  #(
                   BASE rg_doc_pago ( low    = <fs_tvarvc41>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  SELECT low, high
  INTO TABLE @DATA(tl_tvarvc42)
  FROM tvarvc
  WHERE name = 'Z_DOC_COMPENSA_FACTOR'.

  IF sy-subrc = 0.
    LOOP AT tl_tvarvc42 ASSIGNING FIELD-SYMBOL(<fs_tvarvc42>).
      rg_compensa_factor = VALUE  #(
                   BASE rg_compensa_factor ( low    = <fs_tvarvc42>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
  INTO TABLE @DATA(tl_tvarvc43)
  FROM tvarvc
  WHERE name = 'Z_NC_CLASE'.

  IF sy-subrc = 0.
    LOOP AT tl_tvarvc43 ASSIGNING FIELD-SYMBOL(<fs_tvarvc43>).
      rg_nc_clase = VALUE  #(
                   BASE rg_nc_clase ( low    = <fs_tvarvc43>-low
                                     sign   = c_i
                                     option = c_eq ) ).
    ENDLOOP.
  ENDIF.


  SELECT low, high
  INTO TABLE @DATA(tl_tvarvc44)
  FROM tvarvc
  WHERE name = 'Z_CTA_TRANS'.

  IF sy-subrc = 0.
    LOOP AT tl_tvarvc44 ASSIGNING FIELD-SYMBOL(<fs_tvarvc44>).
      rg_fact_santrans = VALUE  #(
                   BASE rg_fact_santrans ( low    = <fs_tvarvc44>-low
                                        sign   = c_i
                                        option = c_eq ) ).
    ENDLOOP.
  ENDIF.

  SELECT low, high
  INTO TABLE @DATA(tl_tvarvc45)
  FROM tvarvc
  WHERE name = 'Z_TR_DESCUENTOS'.

  IF sy-subrc = 0.
    LOOP AT tl_tvarvc45 ASSIGNING FIELD-SYMBOL(<fs_tvarvc45>).
      rg_desc_reb = VALUE  #(
                   BASE rg_desc_reb   ( low    = <fs_tvarvc45>-low
                                        sign   = c_i
                                        option = c_eq ) ).
    ENDLOOP.
  ENDIF.

    SELECT low, high
    INTO TABLE @DATA(tl_tvarvc46)
    FROM tvarvc
    WHERE name = 'Z_CTA_ISRETEN'.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc46 ASSIGNING FIELD-SYMBOL(<fs_tvarvc46>).
      rg_cta_interesesISR = VALUE  #(
                   BASE rg_cta_interesesisr (  low    = <fs_tvarvc46>-low
                                                sign   = c_i
                                                option = c_eq ) ).
    ENDLOOP.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .
  "Inicio ARODEA----->R-005385-Error en reporte de ingresos Egoba   01/01/01/2023
  DATA lo_tools TYPE REF TO lcl_isr_tools.

  IF lo_tools IS NOT BOUND.
    CREATE OBJECT lo_tools.
  ENDIF.
  "FIN ARODEA----->R-005385-Error en reporte de ingresos Egoba   01/01/01/2023
  PERFORM rangos.
*****************************New Records FJRD*************************************
*1.- Traspasos
  perform get_data_traspasos.

*2.- Ingresos por recuperación de seguros
  perform get_data_recuperacion.

*3.-Intereses ganados
  perform get_intereses_ganados.

*4.-Inversiones
  perform inversiones.

*5.-Pres Interco MN LP  (Prestamos interco)
  perform get_prestamos_interco.

*6.-Otros ingresos
  perform get_prestamos_ot_ing.

*7.-Otros depositos
  perform get_prestamos_ot_dep.

*8.-Factoraje Santander España
  perform get_factoraje_santander.

*9.-Factoraje sencillo
  perform get_factoraje_sencillo.
*****************************New Records FJRD*************************************
*2. Cobros compensados con facturas

  SELECT rldnr,
         rbukrs,
         gjahr,
         augdt,
         augbl,
         AUGGJ,
         poper,
         rwcur,
         kunnr
    FROM acdoca
    INTO TABLE @DATA(it_acdoca)
    WHERE rldnr  EQ @c_0l               AND    "Ledger
          rbukrs IN @sociedad           AND    "Sociedad
          ryear  EQ @ejerc              AND    "Ejercicio
          poper  IN @periodo            AND    "Periodo
          blart  IN @rg_tvar_doc        AND    "Clase de documento
          augbl  NE @c_vacio            AND    "Núm. Doc.Comp.
          augbl  LIKE '14%'             AND
          augbl  IN @augbl.
  IF sy-subrc EQ 0.

    SORT it_acdoca BY rbukrs augbl.
    DELETE ADJACENT DUPLICATES FROM it_acdoca COMPARING augbl.

  ENDIF.

*3. Tabla de resultado de cobros compensados
  IF it_acdoca IS NOT INITIAL.
    LOOP AT it_acdoca ASSIGNING FIELD-SYMBOL(<fs_acdoca>).
      MOVE <fs_acdoca>-rldnr  TO wa_cobros-rldnr.
      MOVE <fs_acdoca>-rbukrs TO wa_cobros-rbukrs.
      MOVE <fs_acdoca>-gjahr  TO wa_cobros-gjahr.
      MOVE <fs_acdoca>-AUGGJ  TO wa_cobros-agjahr.
      MOVE <fs_acdoca>-augdt  TO wa_cobros-augdt.
      MOVE <fs_acdoca>-augbl  TO wa_cobros-augbl.
      MOVE <fs_acdoca>-AUGGJ  TO wa_cobros-AUGGJ.
      MOVE <fs_acdoca>-rwcur  TO wa_cobros-rwcur.
      MOVE <fs_acdoca>-poper  TO wa_cobros-poper.
      MOVE <fs_acdoca>-kunnr  TO wa_cobros-kunnr.
      APPEND  wa_cobros TO it_cobros.
    ENDLOOP.

    SORT it_cobros BY rbukrs augbl gjahr.

  ENDIF.

*4.	Validación de cobros anulados
  IF it_cobros IS NOT INITIAL.
    SELECT bukrs,
           belnr,
           gjahr,
           stblg
      FROM bkpf
      INTO CORRESPONDING FIELDS OF TABLE @it_bkpf
      FOR ALL ENTRIES IN @it_cobros
      WHERE bukrs EQ @it_cobros-rbukrs AND
            belnr EQ @it_cobros-augbl  AND
            gjahr EQ @it_cobros-AUGGJ.
*5.	Borrado de tabla I_COBROS
    IF sy-subrc EQ 0.
      LOOP AT it_bkpf ASSIGNING FIELD-SYMBOL(<fs_bkpf>).
        IF  NOT <fs_bkpf>-stblg IS INITIAL.
          DELETE it_cobros WHERE rbukrs EQ <fs_bkpf>-bukrs AND
                                 augbl  EQ <fs_bkpf>-belnr AND
                                 gjahr  EQ <fs_bkpf>-gjahr.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.


*6.	Validación de ejecución con compensación de cobro con o sin extracto
  IF r1 EQ c_x AND it_cobros IS NOT INITIAL.
*7.	Tabla de cobros con documentos de extractos

    SELECT bukrs,
           belnr,
           gjahr,
           augdt,
           augbl,
           bschl,
           sgtxt,
           hkont,
           hbkid,
           hktid
      FROM bseg
      INTO CORRESPONDING FIELDS OF TABLE @it_bseg
      FOR ALL ENTRIES IN @it_cobros
      WHERE bukrs EQ @it_cobros-rbukrs AND
            belnr EQ @it_cobros-augbl AND
            gjahr EQ @it_cobros-AUGGJ AND
            augbl NE @c_vacio AND
            hkont IN @rg_bancos_ingresos AND
            bschl IN @rg_cargo_abono. " AND
*            hbkid NE @c_vacio.

    IF sy-subrc EQ 0.

      SORT it_bseg BY bukrs belnr gjahr.

      SELECT bukrs,
             belnr,
             gjahr,
             budat,
             xblnr,
             kursf
        INTO TABLE @DATA(it_bkpf_extracto)
        FROM bkpf
        FOR ALL ENTRIES IN @it_cobros
        WHERE bukrs EQ @it_cobros-rbukrs AND
              belnr EQ @it_cobros-augbl  AND
              gjahr EQ @it_cobros-AUGGJ.

      IF sy-subrc EQ 0.

        SORT it_bkpf_extracto BY bukrs belnr gjahr.

        LOOP AT it_bseg ASSIGNING FIELD-SYMBOL(<fs_bseg>).

          MOVE <fs_bseg>-bukrs TO wa_extracto-bukrs.
          MOVE <fs_bseg>-belnr TO wa_extracto-belnr.
          MOVE <fs_bseg>-gjahr TO wa_extracto-gjahr.
          MOVE <fs_bseg>-sgtxt TO wa_extracto-sgtxt.
          MOVE <fs_bseg>-hbkid TO wa_extracto-hbkid.
          MOVE <fs_bseg>-hktid TO wa_extracto-hktid.
          MOVE <fs_bseg>-augbl TO wa_extracto-augbl.
          MOVE <fs_bseg>-augdt TO wa_extracto-augdt.

          READ TABLE it_bkpf_extracto ASSIGNING FIELD-SYMBOL(<fs_bkpf_extracto>)
                                            WITH KEY bukrs = <fs_bseg>-bukrs
                                                     belnr = <fs_bseg>-belnr
                                                     gjahr = <fs_bseg>-gjahr.
          IF sy-subrc EQ 0.

            MOVE <fs_bkpf_extracto>-xblnr TO wa_extracto-xblnr.
            MOVE <fs_bkpf_extracto>-budat TO wa_extracto-budat.
            MOVE <fs_bkpf_extracto>-kursf TO wa_extracto-kursf.

          ENDIF.

          READ TABLE it_cobros ASSIGNING FIELD-SYMBOL(<fs_cobros_ext>)
                                           WITH KEY rbukrs = <fs_bseg>-bukrs
                                                    augbl = <fs_bseg>-belnr
                                                    gjahr = <fs_bseg>-gjahr.
          IF sy-subrc EQ 0.

            MOVE <fs_cobros_ext>-rwcur TO wa_extracto-rwcur.

          ENDIF.

          APPEND wa_extracto TO it_extracto.
          CLEAR: wa_extracto.
        ENDLOOP.
      ENDIF.
    ENDIF.

*8.	Tabla de cobros con documentos sin documentos de extractos
  ELSEIF r2 EQ c_x AND it_cobros IS NOT INITIAL.

    SELECT bukrs,
           belnr,
           gjahr,
           augdt,
           augbl,
           bschl,
           sgtxt,
           hkont,
           hbkid,
           hktid
      FROM bseg
      INTO CORRESPONDING FIELDS OF TABLE @it_bseg
      FOR ALL ENTRIES IN @it_cobros
      WHERE bukrs EQ @it_cobros-rbukrs AND
            belnr EQ @it_cobros-augbl AND
            gjahr EQ @it_cobros-AUGGJ AND
            augbl EQ @c_vacio AND
            hkont IN @rg_bancos_ingresos AND
            bschl IN @rg_cargo_abono." AND
*            hbkid EQ @c_vacio.

    IF sy-subrc EQ 0.

      SORT it_bseg BY bukrs belnr gjahr.

      SELECT bukrs,
             belnr,
             gjahr,
             budat,
             xblnr,
             kursf
        FROM bkpf
        INTO TABLE @DATA(it_bkpf_sinextrac)
        FOR ALL ENTRIES IN @it_cobros
        WHERE bukrs EQ @it_cobros-rbukrs AND
              belnr EQ @it_cobros-augbl  AND
              gjahr EQ @it_cobros-AUGGJ.

      IF sy-subrc EQ 0.

        SORT it_bkpf_sinextrac BY bukrs belnr gjahr.

        LOOP AT it_bseg ASSIGNING FIELD-SYMBOL(<fs_bseg_sin_extra>).

          MOVE <fs_bseg_sin_extra>-bukrs TO wa_sin_extracto-bukrs.
          MOVE <fs_bseg_sin_extra>-belnr TO wa_sin_extracto-belnr.
          MOVE <fs_bseg_sin_extra>-gjahr TO wa_sin_extracto-gjahr.
          MOVE <fs_bseg_sin_extra>-sgtxt TO wa_sin_extracto-sgtxt.
          MOVE <fs_bseg_sin_extra>-hbkid TO wa_sin_extracto-hbkid.
          MOVE <fs_bseg_sin_extra>-hktid TO wa_sin_extracto-hktid.
          MOVE <fs_bseg_sin_extra>-augbl TO wa_sin_extracto-augbl.
          MOVE <fs_bseg_sin_extra>-augdt TO wa_sin_extracto-augdt.

          READ TABLE it_bkpf_sinextrac ASSIGNING FIELD-SYMBOL(<fs_bkpf_sinextrac>)
                                            WITH KEY bukrs = <fs_bseg_sin_extra>-bukrs
                                                     belnr = <fs_bseg_sin_extra>-belnr
                                                     gjahr = <fs_bseg_sin_extra>-gjahr.
          IF sy-subrc EQ 0.
            MOVE <fs_bkpf_sinextrac>-xblnr TO wa_sin_extracto-xblnr.
            MOVE <fs_bkpf_sinextrac>-budat TO wa_sin_extracto-budat.
            MOVE <fs_bkpf_sinextrac>-kursf TO wa_sin_extracto-kursf.
          ENDIF.
*corrección read, dump PRD ASALGADO 03.05.2022
*          READ TABLE it_cobros ASSIGNING FIELD-SYMBOL(<fs_cobros_sext>)     "ORIGINAL Se comenta por que en este tramo el field symbol no esta asignado y genera DUMP
*                                            WITH KEY rbukrs = <fs_bseg>-bukrs
*                                                     augbl = <fs_bseg>-belnr
*                                                     gjahr = <fs_bseg>-gjahr.

*          IF lv_test IS NOT INITIAL.
          READ TABLE it_cobros ASSIGNING FIELD-SYMBOL(<fs_cobros_sext>)   "ASALGAD0 03.05.2022
          WITH KEY rbukrs = <fs_bseg_sin_extra>-bukrs
          augbl = <fs_bseg_sin_extra>-augbl
          gjahr = <fs_bseg_sin_extra>-AUGGJ.
*          ENDIF.

*fin ASALGAD0 03.05.2022

          IF sy-subrc EQ 0.

            MOVE <fs_cobros_sext>-rwcur TO wa_sin_extracto-rwcur.

          ENDIF.

          APPEND wa_sin_extracto TO it_sin_extracto.
          CLEAR: wa_sin_extracto.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDIF.

*9.	Extraer detalle de cobro
  IF it_extracto IS NOT INITIAL.

    SORT it_extracto BY bukrs gjahr belnr.

    SELECT rldnr,
           rbukrs,
           gjahr,
           belnr,
           docln,
           ryear,
           aworg,
           awref,
           rtcur,
           racct,
           prctr,
           kokrs,
           tsl,
           hsl,
           wsl,
           drcrk,
           budat,
           bldat,
           blart,
           bschl,
           linetype,
           ktosl,
           ktopl,
           kunnr,
           mwskz,
           hbkid,
           hktid,
           augdt,
           augbl,
           auggj,
           gkont,
           rbudget_pd
      FROM acdoca
      INTO TABLE @it_detcobro
       FOR ALL ENTRIES IN @it_extracto
          WHERE rldnr  EQ @c_0l AND
                rbukrs EQ @it_extracto-bukrs AND
                gjahr  EQ @it_extracto-gjahr AND
                belnr  EQ @it_extracto-belnr.
    IF sy-subrc EQ 0.
      "ARODEA R-005385 Error en reporte de ingresos Egoba
      lo_tools->fix_linetype(
        EXPORTING
*          it_facturas =
          it_dcobro   = it_detcobro
        IMPORTING
*          et_facturas =
          et_dcobro   =  it_detcobro       ).
      "FIN ARODEA

      SORT it_detcobro BY rbukrs gjahr belnr.
    ENDIF.

    SELECT spras,
           bukrs,
           hbkid,
           hktid,
           text1
      FROM t012t
      INTO CORRESPONDING FIELDS OF TABLE @it_t012t
       FOR ALL ENTRIES IN @it_extracto
          WHERE spras  EQ @c_es AND
                bukrs  EQ @it_extracto-bukrs AND
                hbkid  EQ @it_extracto-hbkid AND
                hktid  EQ @it_extracto-hktid.

    if it_fact_pago_detalle is NOT INITIAL.
      SELECT spras,
             bukrs,
             hbkid,
             hktid,
             text1
        FROM t012t
        APPENDING CORRESPONDING FIELDS OF TABLE @it_t012t
         FOR ALL ENTRIES IN @it_fact_pago_detalle
            WHERE spras  EQ @c_es AND
                  bukrs  EQ @it_fact_pago_detalle-rbukrs AND
                  hbkid  EQ @it_fact_pago_detalle-hbkid AND
                  hktid  EQ @it_fact_pago_detalle-hktid.
    endif.

    IF sy-subrc EQ 0.
        SORT it_t012t BY spras bukrs hbkid hktid.
    ENDIF.

  ELSEIF it_sin_extracto IS NOT INITIAL.

    SORT it_sin_extracto BY bukrs gjahr belnr.

    SELECT rldnr,
           rbukrs,
           gjahr,
           belnr,
           docln,
           ryear,
           aworg,
           awref,
           rtcur,
           racct,
           prctr,
           kokrs,
           tsl,
           hsl,
           wsl,
           drcrk,
           budat,
           bldat,
           blart,
           bschl,
           linetype,
           ktosl,
           ktopl,
           kunnr,
           mwskz,
           hbkid,
           hktid,
           augdt,
           augbl,
           auggj,
           gkont,
           rbudget_pd
        FROM acdoca
        INTO TABLE @it_detcobro
        FOR ALL ENTRIES IN @it_sin_extracto
        WHERE rldnr  EQ @c_0l                   AND
              rbukrs EQ @it_sin_extracto-bukrs  AND
              gjahr  EQ @it_sin_extracto-gjahr  AND
              belnr  EQ @it_sin_extracto-belnr.
    IF sy-subrc EQ 0.
      "ARODEA R-005385 Error en reporte de ingresos Egoba
      lo_tools->fix_linetype(
      EXPORTING
*          it_facturas =
        it_dcobro   = it_detcobro
      IMPORTING
*          et_facturas =
        et_dcobro   =  it_detcobro       ).
      "FIN ARODEA
      SORT it_detcobro BY rbukrs gjahr belnr.
    ENDIF.

  ENDIF.

*10.  Extraer detalle de facturas
  IF it_cobros IS NOT INITIAL.
    SELECT rldnr,
           rbukrs,
           belnr,
           gjahr,
           blart,
           budat,
           augdt,
           augbl,
           AUGGJ,
           kunnr
      FROM acdoca
      INTO TABLE @it_fac_venta
      FOR ALL ENTRIES IN @it_cobros
      WHERE     rldnr  EQ @c_0l           AND    "Ledger
                rbukrs EQ @it_cobros-rbukrs AND  "Sociedad
                blart  IN @rg_doc_fac_cl  AND
                auggj  EQ @it_cobros-AUGGJ   AND      "LCABRERA 28 feb 2022
                augbl  EQ @it_cobros-augbl  AND
                augbl  LIKE '14%'.

    IF sy-subrc EQ 0.
      SORT it_fac_venta BY rbukrs augbl gjahr DESCENDING.

      SELECT bukrs,
             belnr,
             gjahr,
             stblg
     FROM bkpf
     INTO CORRESPONDING FIELDS OF TABLE @it_bkpf_fac_ven
     FOR ALL ENTRIES IN @it_fac_venta
     WHERE bukrs EQ @it_fac_venta-rbukrs AND
           belnr EQ @it_fac_venta-augbl  AND
           gjahr EQ @it_fac_venta-AUGGJ.

      IF sy-subrc EQ 0.

        SORT  it_bkpf_fac_ven BY bukrs belnr gjahr.

        LOOP AT it_bkpf_fac_ven ASSIGNING FIELD-SYMBOL(<fs_bkpf_fac_ven>).
          IF NOT <fs_bkpf_fac_ven>-stblg  IS INITIAL.
            DELETE it_cobros WHERE rbukrs EQ <fs_bkpf_fac_ven>-bukrs AND
                                   augbl  EQ <fs_bkpf_fac_ven>-belnr AND
                                   gjahr  EQ <fs_bkpf_fac_ven>-gjahr.
          ENDIF.
        ENDLOOP.
      ENDIF.

      SELECT rldnr
             rbukrs
             gjahr
             belnr
             docln
             ryear
             rwcur
             aworg
             awref
             rtcur
             racct
             prctr
             kokrs
             tsl
             hsl
             wsl
             drcrk
             budat
             bldat
             blart
             bschl
             linetype
             ktosl
             ktopl
             kunnr
             mwskz
             hbkid
             hktid
             augdt
             augbl
             auggj
             gkont
             rbudget_pd
        FROM acdoca
        INTO TABLE it_detfacturas
        FOR ALL ENTRIES IN it_fac_venta
        WHERE rldnr  EQ c_0l                 AND
              rbukrs EQ it_fac_venta-rbukrs  AND
              gjahr  EQ it_fac_venta-gjahr   AND
              belnr  EQ it_fac_venta-belnr.

      IF sy-subrc EQ 0.
        "INICIO ARODEA R-005385 Error en reporte de ingresos Egoba
        lo_tools->fix_linetype(
           EXPORTING
             it_facturas = it_detfacturas
*             it_dcobro   =
           IMPORTING
             et_facturas = it_detfacturas ).
        "FIN ARODEA
        SORT it_detfacturas BY rbukrs gjahr belnr docln ASCENDING.
      ENDIF.
    ENDIF.

    SELECT client,
           partner,
           taxtype,
           taxnum,
           taxnumxl
       FROM dfkkbptaxnum
       APPENDING TABLE @it_dfkkbptaxnum
       FOR ALL ENTRIES IN @it_detfacturas
       WHERE partner  EQ @it_detfacturas-kunnr. " AND
*           taxtype  EQ 'MX1'.
    IF sy-subrc EQ 0.
      SORT it_dfkkbptaxnum BY partner.
    ENDIF.

*  SELECT client,
*        partner,
*        taxtype,
*        taxnum,
*        taxnumxl
*    FROM dfkkbptaxnum
*    INTO TABLE @it_dfkkbptaxnum2
*    FOR ALL ENTRIES IN @it_detfacturas
*    WHERE partner  EQ @it_detfacturas-kunnr AND
*          taxtype  EQ 'US1'.
*  IF sy-subrc EQ 0.
*    SORT it_dfkkbptaxnum2 BY partner.
*  ENDIF.

*11.  Separación de registros de monto base + IVA.
*12.  Separación de registros de ingreso u otros movtos de la facturac
*13.  Separación de registros de IVA en factura
  ENDIF.
  LOOP AT it_detfacturas ASSIGNING <fs_detfacturas>.

*11.  Separación de registros de monto base + IVA.
    IF  <fs_detfacturas>-bschl EQ '01' OR <fs_detfacturas>-bschl EQ '11'.
      IF ( <fs_detfacturas>-bschl IN rg_abono_cargo ) OR ( <fs_detfacturas>-bschl IN rg_abono_cargo_high ) OR <fs_detfacturas>-bschl EQ '11'.

        IF <fs_detfacturas>-linetype IN rg_clave_deudor
        AND <fs_detfacturas>-kunnr NE c_vacio
        AND <fs_detfacturas>-augbl NE c_vacio.

          wa_bru_fac_acree-rldnr = <fs_detfacturas>-rldnr.
          wa_bru_fac_acree-rbukrs = <fs_detfacturas>-rbukrs.
          wa_bru_fac_acree-gjahr = <fs_detfacturas>-gjahr.
          wa_bru_fac_acree-belnr = <fs_detfacturas>-belnr.
          wa_bru_fac_acree-docln = <fs_detfacturas>-docln.
          wa_bru_fac_acree-rwcur = <fs_detfacturas>-rwcur.
          wa_bru_fac_acree-ryear = <fs_detfacturas>-ryear.
          wa_bru_fac_acree-aworg = <fs_detfacturas>-aworg.
          wa_bru_fac_acree-awref = <fs_detfacturas>-awref.
          wa_bru_fac_acree-rtcur = <fs_detfacturas>-rtcur.
          wa_bru_fac_acree-racct = <fs_detfacturas>-racct.
          wa_bru_fac_acree-prctr = <fs_detfacturas>-prctr.
          wa_bru_fac_acree-kokrs = <fs_detfacturas>-kokrs.
          wa_bru_fac_acree-tsl = <fs_detfacturas>-tsl.
          wa_bru_fac_acree-wsl = <fs_detfacturas>-wsl.
          wa_bru_fac_acree-hsl = <fs_detfacturas>-hsl.
          wa_bru_fac_acree-budat = <fs_detfacturas>-budat.
          wa_bru_fac_acree-bldat = <fs_detfacturas>-bldat.
          wa_bru_fac_acree-blart = <fs_detfacturas>-blart.
          wa_bru_fac_acree-bschl = <fs_detfacturas>-bschl.
          wa_bru_fac_acree-drcrk = <fs_detfacturas>-drcrk.
          wa_bru_fac_acree-linetype = <fs_detfacturas>-linetype.
          wa_bru_fac_acree-ktosl = <fs_detfacturas>-ktosl.
          wa_bru_fac_acree-ktopl = <fs_detfacturas>-ktopl.
          wa_bru_fac_acree-kunnr = <fs_detfacturas>-kunnr.
          wa_bru_fac_acree-mwskz = <fs_detfacturas>-mwskz.
          wa_bru_fac_acree-hbkid = <fs_detfacturas>-hbkid.
          wa_bru_fac_acree-hktid = <fs_detfacturas>-hktid.
          wa_bru_fac_acree-augdt = <fs_detfacturas>-augdt.
          wa_bru_fac_acree-augbl = <fs_detfacturas>-augbl.
          wa_bru_fac_acree-auggj = <fs_detfacturas>-auggj.
          wa_bru_fac_acree-gkont = <fs_detfacturas>-gkont.
          wa_bru_fac_acree-rbudget_pd = <fs_detfacturas>-rbudget_pd.

          APPEND wa_bru_fac_acree TO it_bru_fac_acree.
          CLEAR: wa_bru_fac_acree.

        ENDIF.
      ENDIF.

*12.  Separación de registros de ingreso u otros movtos de la facturac
*13.  Separación de registros de IVA en factura
    ELSEIF <fs_detfacturas>-bschl EQ '50'.
*12.  Separación de registros de ingreso u otros movtos de la facturac
      IF ( <fs_detfacturas>-bschl IN rg_abono_cargo ) OR ( <fs_detfacturas>-bschl IN rg_abono_cargo_high ).

        IF <fs_detfacturas>-linetype IN rg_clave_ing
        AND <fs_detfacturas>-ktosl EQ c_vacio
        AND <fs_detfacturas>-racct IN rg_ctas_ing_vta
        AND <fs_detfacturas>-prctr NE c_vacio.

          wa_ing_fac_acree-rldnr = <fs_detfacturas>-rldnr.
          wa_ing_fac_acree-rbukrs = <fs_detfacturas>-rbukrs.
          wa_ing_fac_acree-gjahr = <fs_detfacturas>-gjahr.
          wa_ing_fac_acree-belnr = <fs_detfacturas>-belnr.
          wa_ing_fac_acree-docln = <fs_detfacturas>-docln.
          wa_ing_fac_acree-rwcur = <fs_detfacturas>-rwcur.
          wa_ing_fac_acree-ryear = <fs_detfacturas>-ryear.
          wa_ing_fac_acree-aworg = <fs_detfacturas>-aworg.
          wa_ing_fac_acree-awref = <fs_detfacturas>-awref.
          wa_ing_fac_acree-rtcur = <fs_detfacturas>-rtcur.
          wa_ing_fac_acree-racct = <fs_detfacturas>-racct.
          wa_ing_fac_acree-prctr = <fs_detfacturas>-prctr.
          wa_ing_fac_acree-kokrs = <fs_detfacturas>-kokrs.
          wa_ing_fac_acree-tsl = <fs_detfacturas>-tsl.
          wa_ing_fac_acree-wsl = <fs_detfacturas>-wsl.
          wa_ing_fac_acree-hsl = <fs_detfacturas>-hsl.
          wa_ing_fac_acree-budat = <fs_detfacturas>-budat.
          wa_ing_fac_acree-bldat = <fs_detfacturas>-bldat.
          wa_ing_fac_acree-blart = <fs_detfacturas>-blart.
          wa_ing_fac_acree-bschl = <fs_detfacturas>-bschl.
          wa_ing_fac_acree-drcrk = <fs_detfacturas>-drcrk.
          wa_ing_fac_acree-linetype = <fs_detfacturas>-linetype.
          wa_ing_fac_acree-ktosl = <fs_detfacturas>-ktosl.
          wa_ing_fac_acree-ktopl = <fs_detfacturas>-ktopl.
          wa_ing_fac_acree-kunnr = <fs_detfacturas>-kunnr.
          wa_ing_fac_acree-mwskz = <fs_detfacturas>-mwskz.
          wa_ing_fac_acree-hbkid = <fs_detfacturas>-hbkid.
          wa_ing_fac_acree-hktid = <fs_detfacturas>-hktid.
          wa_ing_fac_acree-augdt = <fs_detfacturas>-augdt.
          wa_ing_fac_acree-augbl = <fs_detfacturas>-augbl.
          wa_ing_fac_acree-auggj = <fs_detfacturas>-auggj.
          wa_ing_fac_acree-gkont = <fs_detfacturas>-gkont.
          wa_ing_fac_acree-rbudget_pd = <fs_detfacturas>-rbudget_pd.

          APPEND wa_ing_fac_acree TO it_ing_fac_acree.
          CLEAR: wa_ing_fac_acree.

        ENDIF.

      ENDIF.

      IF  <fs_detfacturas>-bschl IN rg_abono_cargo  OR  <fs_detfacturas>-bschl IN rg_abono_cargo_high.

        IF <fs_detfacturas>-linetype IN rg_clave_ret_iva
         AND  <fs_detfacturas>-ktosl EQ 'WIT'
         AND  <fs_detfacturas>-racct IN rg_ctas_ret_iva_ing
         OR   ( <fs_detfacturas>-racct IN rg_ctas_ret_iva_ing_4 ).

          wa_ret_fac_acree-rldnr = <fs_detfacturas>-rldnr.
          wa_ret_fac_acree-rbukrs = <fs_detfacturas>-rbukrs.
          wa_ret_fac_acree-gjahr = <fs_detfacturas>-gjahr.
          wa_ret_fac_acree-belnr = <fs_detfacturas>-belnr.
          wa_ret_fac_acree-docln = <fs_detfacturas>-docln.
          wa_ret_fac_acree-rwcur = <fs_detfacturas>-rwcur.
          wa_ret_fac_acree-ryear = <fs_detfacturas>-ryear.
          wa_ret_fac_acree-aworg = <fs_detfacturas>-aworg.
          wa_ret_fac_acree-awref = <fs_detfacturas>-awref.
          wa_ret_fac_acree-rtcur = <fs_detfacturas>-rtcur.
          wa_ret_fac_acree-racct = <fs_detfacturas>-racct.
          wa_ret_fac_acree-prctr = <fs_detfacturas>-prctr.
          wa_ret_fac_acree-kokrs = <fs_detfacturas>-kokrs.
          wa_ret_fac_acree-tsl = <fs_detfacturas>-tsl.
          wa_ret_fac_acree-wsl = <fs_detfacturas>-wsl.
          wa_ret_fac_acree-hsl = <fs_detfacturas>-hsl.
          wa_ret_fac_acree-budat = <fs_detfacturas>-budat.
          wa_ret_fac_acree-bldat = <fs_detfacturas>-bldat.
          wa_ret_fac_acree-blart = <fs_detfacturas>-blart.
          wa_ret_fac_acree-bschl = <fs_detfacturas>-bschl.
          wa_ret_fac_acree-drcrk = <fs_detfacturas>-drcrk.
          wa_ret_fac_acree-linetype = <fs_detfacturas>-linetype.
          wa_ret_fac_acree-ktosl = <fs_detfacturas>-ktosl.
          wa_ret_fac_acree-ktopl = <fs_detfacturas>-ktopl.
          wa_ret_fac_acree-kunnr = <fs_detfacturas>-kunnr.
          wa_ret_fac_acree-mwskz = <fs_detfacturas>-mwskz.
          wa_ret_fac_acree-hbkid = <fs_detfacturas>-hbkid.
          wa_ret_fac_acree-hktid = <fs_detfacturas>-hktid.
          wa_ret_fac_acree-augdt = <fs_detfacturas>-augdt.
          wa_ret_fac_acree-augbl = <fs_detfacturas>-augbl.
          wa_ret_fac_acree-auggj = <fs_detfacturas>-auggj.
          wa_ret_fac_acree-gkont = <fs_detfacturas>-gkont.
          wa_ret_fac_acree-rbudget_pd = <fs_detfacturas>-rbudget_pd.

          APPEND wa_ret_fac_acree TO it_ret_fac_acree.
          CLEAR: wa_ret_fac_acree.

        ELSEIF <fs_detfacturas>-linetype IN rg_clave_ret_iva
        AND  <fs_detfacturas>-ktosl EQ c_vacio
        AND  <fs_detfacturas>-racct IN rg_ctas_ret_iva_ing
        OR   ( <fs_detfacturas>-racct IN rg_ctas_ret_iva_ing_4 ).

          wa_ret_fac_acree-rldnr = <fs_detfacturas>-rldnr.
          wa_ret_fac_acree-rbukrs = <fs_detfacturas>-rbukrs.
          wa_ret_fac_acree-gjahr = <fs_detfacturas>-gjahr.
          wa_ret_fac_acree-belnr = <fs_detfacturas>-belnr.
          wa_ret_fac_acree-docln = <fs_detfacturas>-docln.
          wa_ret_fac_acree-rwcur = <fs_detfacturas>-rwcur.
          wa_ret_fac_acree-ryear = <fs_detfacturas>-ryear.
          wa_ret_fac_acree-aworg = <fs_detfacturas>-aworg.
          wa_ret_fac_acree-awref = <fs_detfacturas>-awref.
          wa_ret_fac_acree-rtcur = <fs_detfacturas>-rtcur.
          wa_ret_fac_acree-racct = <fs_detfacturas>-racct.
          wa_ret_fac_acree-prctr = <fs_detfacturas>-prctr.
          wa_ret_fac_acree-kokrs = <fs_detfacturas>-kokrs.
          wa_ret_fac_acree-tsl = <fs_detfacturas>-tsl.
          wa_ret_fac_acree-wsl = <fs_detfacturas>-wsl.
          wa_ret_fac_acree-hsl = <fs_detfacturas>-hsl.
          wa_ret_fac_acree-budat = <fs_detfacturas>-budat.
          wa_ret_fac_acree-bldat = <fs_detfacturas>-bldat.
          wa_ret_fac_acree-blart = <fs_detfacturas>-blart.
          wa_ret_fac_acree-bschl = <fs_detfacturas>-bschl.
          wa_ret_fac_acree-drcrk = <fs_detfacturas>-drcrk.
          wa_ret_fac_acree-linetype = <fs_detfacturas>-linetype.
          wa_ret_fac_acree-ktosl = <fs_detfacturas>-ktosl.
          wa_ret_fac_acree-ktopl = <fs_detfacturas>-ktopl.
          wa_ret_fac_acree-kunnr = <fs_detfacturas>-kunnr.
          wa_ret_fac_acree-mwskz = <fs_detfacturas>-mwskz.
          wa_ret_fac_acree-hbkid = <fs_detfacturas>-hbkid.
          wa_ret_fac_acree-hktid = <fs_detfacturas>-hktid.
          wa_ret_fac_acree-augdt = <fs_detfacturas>-augdt.
          wa_ret_fac_acree-augbl = <fs_detfacturas>-augbl.
          wa_ret_fac_acree-auggj = <fs_detfacturas>-auggj.
          wa_ret_fac_acree-gkont = <fs_detfacturas>-gkont.
          wa_ret_fac_acree-rbudget_pd = <fs_detfacturas>-rbudget_pd.

          APPEND wa_ret_fac_acree TO it_ret_fac_acree.
          CLEAR: wa_ret_fac_acree.


        ENDIF.
      ENDIF.

*13.  Separación de registros de IVA en factura
      IF ( <fs_detfacturas>-bschl IN rg_abono_cargo ) OR ( <fs_detfacturas>-bschl IN rg_abono_cargo_high ).

        IF <fs_detfacturas>-linetype IN rg_clave_iva
         AND <fs_detfacturas>-ktosl EQ 'MWS'
         AND <fs_detfacturas>-racct IN rg_ctas_iva_ing.

          wa_iva_fac_acree-rldnr = <fs_detfacturas>-rldnr.
          wa_iva_fac_acree-rbukrs = <fs_detfacturas>-rbukrs.
          wa_iva_fac_acree-gjahr = <fs_detfacturas>-gjahr.
          wa_iva_fac_acree-belnr = <fs_detfacturas>-belnr.
          wa_iva_fac_acree-docln = <fs_detfacturas>-docln.
          wa_iva_fac_acree-rwcur = <fs_detfacturas>-rwcur.
          wa_iva_fac_acree-ryear = <fs_detfacturas>-ryear.
          wa_iva_fac_acree-aworg = <fs_detfacturas>-aworg.
          wa_iva_fac_acree-awref = <fs_detfacturas>-awref.
          wa_iva_fac_acree-rtcur = <fs_detfacturas>-rtcur.
          wa_iva_fac_acree-racct = <fs_detfacturas>-racct.
          wa_iva_fac_acree-prctr = <fs_detfacturas>-prctr.
          wa_iva_fac_acree-kokrs = <fs_detfacturas>-kokrs.
          wa_iva_fac_acree-tsl = <fs_detfacturas>-tsl.
          wa_iva_fac_acree-wsl = <fs_detfacturas>-wsl.
          wa_iva_fac_acree-hsl = <fs_detfacturas>-hsl.
          wa_iva_fac_acree-budat = <fs_detfacturas>-budat.
          wa_iva_fac_acree-bldat = <fs_detfacturas>-bldat.
          wa_iva_fac_acree-blart = <fs_detfacturas>-blart.
          wa_iva_fac_acree-bschl = <fs_detfacturas>-bschl.
          wa_iva_fac_acree-drcrk = <fs_detfacturas>-drcrk.
          wa_iva_fac_acree-linetype = <fs_detfacturas>-linetype.
          wa_iva_fac_acree-ktosl = <fs_detfacturas>-ktosl.
          wa_iva_fac_acree-ktopl = <fs_detfacturas>-ktopl.
          wa_iva_fac_acree-kunnr = <fs_detfacturas>-kunnr.
          wa_iva_fac_acree-mwskz = <fs_detfacturas>-mwskz.
          wa_iva_fac_acree-hbkid = <fs_detfacturas>-hbkid.
          wa_iva_fac_acree-hktid = <fs_detfacturas>-hktid.
          wa_iva_fac_acree-augdt = <fs_detfacturas>-augdt.
          wa_iva_fac_acree-augbl = <fs_detfacturas>-augbl.
          wa_iva_fac_acree-auggj = <fs_detfacturas>-auggj.
          wa_iva_fac_acree-gkont = <fs_detfacturas>-gkont.
          wa_iva_fac_acree-rbudget_pd = <fs_detfacturas>-rbudget_pd.

          APPEND wa_iva_fac_acree TO it_iva_fac_acree.
          CLEAR: wa_iva_fac_acree.

        ENDIF.
      ENDIF.

*14.  Separación de registros de retención en factura
    ELSEIF <fs_detfacturas>-bschl EQ '40'.

      IF ( <fs_detfacturas>-bschl IN rg_abono_cargo ) OR ( <fs_detfacturas>-bschl IN rg_abono_cargo_high ).

        IF <fs_detfacturas>-linetype IN rg_clave_iva
         AND <fs_detfacturas>-ktosl EQ 'MWS'
         AND <fs_detfacturas>-racct IN rg_ctas_iva_ing.

          wa_iva_fac_acree-rldnr = <fs_detfacturas>-rldnr.
          wa_iva_fac_acree-rbukrs = <fs_detfacturas>-rbukrs.
          wa_iva_fac_acree-gjahr = <fs_detfacturas>-gjahr.
          wa_iva_fac_acree-belnr = <fs_detfacturas>-belnr.
          wa_iva_fac_acree-docln = <fs_detfacturas>-docln.
          wa_iva_fac_acree-rwcur = <fs_detfacturas>-rwcur.
          wa_iva_fac_acree-ryear = <fs_detfacturas>-ryear.
          wa_iva_fac_acree-aworg = <fs_detfacturas>-aworg.
          wa_iva_fac_acree-awref = <fs_detfacturas>-awref.
          wa_iva_fac_acree-rtcur = <fs_detfacturas>-rtcur.
          wa_iva_fac_acree-racct = <fs_detfacturas>-racct.
          wa_iva_fac_acree-prctr = <fs_detfacturas>-prctr.
          wa_iva_fac_acree-kokrs = <fs_detfacturas>-kokrs.
          wa_iva_fac_acree-tsl = <fs_detfacturas>-tsl.
          wa_iva_fac_acree-wsl = <fs_detfacturas>-wsl.
          wa_iva_fac_acree-hsl = <fs_detfacturas>-hsl.
          wa_iva_fac_acree-budat = <fs_detfacturas>-budat.
          wa_iva_fac_acree-bldat = <fs_detfacturas>-bldat.
          wa_iva_fac_acree-blart = <fs_detfacturas>-blart.
          wa_iva_fac_acree-bschl = <fs_detfacturas>-bschl.
          wa_iva_fac_acree-drcrk = <fs_detfacturas>-drcrk.
          wa_iva_fac_acree-linetype = <fs_detfacturas>-linetype.
          wa_iva_fac_acree-ktosl = <fs_detfacturas>-ktosl.
          wa_iva_fac_acree-ktopl = <fs_detfacturas>-ktopl.
          wa_iva_fac_acree-kunnr = <fs_detfacturas>-kunnr.
          wa_iva_fac_acree-mwskz = <fs_detfacturas>-mwskz.
          wa_iva_fac_acree-hbkid = <fs_detfacturas>-hbkid.
          wa_iva_fac_acree-hktid = <fs_detfacturas>-hktid.
          wa_iva_fac_acree-augdt = <fs_detfacturas>-augdt.
          wa_iva_fac_acree-augbl = <fs_detfacturas>-augbl.
          wa_iva_fac_acree-auggj = <fs_detfacturas>-auggj.
          wa_iva_fac_acree-gkont = <fs_detfacturas>-gkont.
          wa_iva_fac_acree-rbudget_pd = <fs_detfacturas>-rbudget_pd.

          APPEND wa_iva_fac_acree TO it_iva_fac_acree.
          CLEAR: wa_iva_fac_acree.

        ENDIF.
      ENDIF.

      IF ( <fs_detfacturas>-bschl IN rg_abono_cargo ) OR ( <fs_detfacturas>-bschl IN rg_abono_cargo_high ).

        IF <fs_detfacturas>-linetype IN rg_clave_ing
        AND <fs_detfacturas>-ktosl EQ c_vacio
        AND <fs_detfacturas>-racct IN rg_ctas_ing_vta
        AND <fs_detfacturas>-prctr NE c_vacio.

          wa_ing_fac_acree-rldnr = <fs_detfacturas>-rldnr.
          wa_ing_fac_acree-rbukrs = <fs_detfacturas>-rbukrs.
          wa_ing_fac_acree-gjahr = <fs_detfacturas>-gjahr.
          wa_ing_fac_acree-belnr = <fs_detfacturas>-belnr.
          wa_ing_fac_acree-docln = <fs_detfacturas>-docln.
          wa_ing_fac_acree-rwcur = <fs_detfacturas>-rwcur.
          wa_ing_fac_acree-ryear = <fs_detfacturas>-ryear.
          wa_ing_fac_acree-aworg = <fs_detfacturas>-aworg.
          wa_ing_fac_acree-awref = <fs_detfacturas>-awref.
          wa_ing_fac_acree-rtcur = <fs_detfacturas>-rtcur.
          wa_ing_fac_acree-racct = <fs_detfacturas>-racct.
          wa_ing_fac_acree-prctr = <fs_detfacturas>-prctr.
          wa_ing_fac_acree-kokrs = <fs_detfacturas>-kokrs.
          wa_ing_fac_acree-tsl = <fs_detfacturas>-tsl.
          wa_ing_fac_acree-wsl = <fs_detfacturas>-wsl.
          wa_ing_fac_acree-hsl = <fs_detfacturas>-hsl.
          wa_ing_fac_acree-budat = <fs_detfacturas>-budat.
          wa_ing_fac_acree-bldat = <fs_detfacturas>-bldat.
          wa_ing_fac_acree-blart = <fs_detfacturas>-blart.
          wa_ing_fac_acree-bschl = <fs_detfacturas>-bschl.
          wa_ing_fac_acree-drcrk = <fs_detfacturas>-drcrk.
          wa_ing_fac_acree-linetype = <fs_detfacturas>-linetype.
          wa_ing_fac_acree-ktosl = <fs_detfacturas>-ktosl.
          wa_ing_fac_acree-ktopl = <fs_detfacturas>-ktopl.
          wa_ing_fac_acree-kunnr = <fs_detfacturas>-kunnr.
          wa_ing_fac_acree-mwskz = <fs_detfacturas>-mwskz.
          wa_ing_fac_acree-hbkid = <fs_detfacturas>-hbkid.
          wa_ing_fac_acree-hktid = <fs_detfacturas>-hktid.
          wa_ing_fac_acree-augdt = <fs_detfacturas>-augdt.
          wa_ing_fac_acree-augbl = <fs_detfacturas>-augbl.
          wa_ing_fac_acree-auggj = <fs_detfacturas>-auggj.
          wa_ing_fac_acree-gkont = <fs_detfacturas>-gkont.
          wa_ing_fac_acree-rbudget_pd = <fs_detfacturas>-rbudget_pd.

          APPEND wa_ing_fac_acree TO it_ing_fac_acree.
          CLEAR: wa_ing_fac_acree.

        ENDIF.

      ENDIF.


      IF  <fs_detfacturas>-bschl IN rg_abono_cargo  OR  <fs_detfacturas>-bschl IN rg_abono_cargo_high.

        IF <fs_detfacturas>-linetype IN rg_clave_ret_iva
         AND  <fs_detfacturas>-ktosl EQ 'WIT'
         AND  <fs_detfacturas>-racct IN rg_ctas_ret_iva_ing
         OR ( <fs_detfacturas>-racct IN rg_ctas_ret_iva_ing_4 ).

          wa_ret_fac_acree-rldnr = <fs_detfacturas>-rldnr.
          wa_ret_fac_acree-rbukrs = <fs_detfacturas>-rbukrs.
          wa_ret_fac_acree-gjahr = <fs_detfacturas>-gjahr.
          wa_ret_fac_acree-belnr = <fs_detfacturas>-belnr.
          wa_ret_fac_acree-docln = <fs_detfacturas>-docln.
          wa_ret_fac_acree-rwcur = <fs_detfacturas>-rwcur.
          wa_ret_fac_acree-ryear = <fs_detfacturas>-ryear.
          wa_ret_fac_acree-aworg = <fs_detfacturas>-aworg.
          wa_ret_fac_acree-awref = <fs_detfacturas>-awref.
          wa_ret_fac_acree-rtcur = <fs_detfacturas>-rtcur.
          wa_ret_fac_acree-racct = <fs_detfacturas>-racct.
          wa_ret_fac_acree-prctr = <fs_detfacturas>-prctr.
          wa_ret_fac_acree-kokrs = <fs_detfacturas>-kokrs.
          wa_ret_fac_acree-tsl = <fs_detfacturas>-tsl.
          wa_ret_fac_acree-wsl = <fs_detfacturas>-wsl.
          wa_ret_fac_acree-hsl = <fs_detfacturas>-hsl.
          wa_ret_fac_acree-budat = <fs_detfacturas>-budat.
          wa_ret_fac_acree-bldat = <fs_detfacturas>-bldat.
          wa_ret_fac_acree-blart = <fs_detfacturas>-blart.
          wa_ret_fac_acree-bschl = <fs_detfacturas>-bschl.
          wa_ret_fac_acree-drcrk = <fs_detfacturas>-drcrk.
          wa_ret_fac_acree-linetype = <fs_detfacturas>-linetype.
          wa_ret_fac_acree-ktosl = <fs_detfacturas>-ktosl.
          wa_ret_fac_acree-ktopl = <fs_detfacturas>-ktopl.
          wa_ret_fac_acree-kunnr = <fs_detfacturas>-kunnr.
          wa_ret_fac_acree-mwskz = <fs_detfacturas>-mwskz.
          wa_ret_fac_acree-hbkid = <fs_detfacturas>-hbkid.
          wa_ret_fac_acree-hktid = <fs_detfacturas>-hktid.
          wa_ret_fac_acree-augdt = <fs_detfacturas>-augdt.
          wa_ret_fac_acree-augbl = <fs_detfacturas>-augbl.
          wa_ret_fac_acree-auggj = <fs_detfacturas>-auggj.
          wa_ret_fac_acree-gkont = <fs_detfacturas>-gkont.
          wa_ret_fac_acree-rbudget_pd = <fs_detfacturas>-rbudget_pd.

          APPEND wa_ret_fac_acree TO it_ret_fac_acree.
          CLEAR: wa_ret_fac_acree.

        ELSEIF <fs_detfacturas>-linetype IN rg_clave_ret_iva
        AND  <fs_detfacturas>-ktosl EQ c_vacio
        AND  <fs_detfacturas>-racct IN rg_ctas_ret_iva_ing
        OR ( <fs_detfacturas>-racct IN rg_ctas_ret_iva_ing_4 ).

          wa_ret_fac_acree-rldnr = <fs_detfacturas>-rldnr.
          wa_ret_fac_acree-rbukrs = <fs_detfacturas>-rbukrs.
          wa_ret_fac_acree-gjahr = <fs_detfacturas>-gjahr.
          wa_ret_fac_acree-belnr = <fs_detfacturas>-belnr.
          wa_ret_fac_acree-docln = <fs_detfacturas>-docln.
          wa_ret_fac_acree-rwcur = <fs_detfacturas>-rwcur.
          wa_ret_fac_acree-ryear = <fs_detfacturas>-ryear.
          wa_ret_fac_acree-aworg = <fs_detfacturas>-aworg.
          wa_ret_fac_acree-awref = <fs_detfacturas>-awref.
          wa_ret_fac_acree-rtcur = <fs_detfacturas>-rtcur.
          wa_ret_fac_acree-racct = <fs_detfacturas>-racct.
          wa_ret_fac_acree-prctr = <fs_detfacturas>-prctr.
          wa_ret_fac_acree-kokrs = <fs_detfacturas>-kokrs.
          wa_ret_fac_acree-tsl = <fs_detfacturas>-tsl.
          wa_ret_fac_acree-wsl = <fs_detfacturas>-wsl.
          wa_ret_fac_acree-hsl = <fs_detfacturas>-hsl.
          wa_ret_fac_acree-budat = <fs_detfacturas>-budat.
          wa_ret_fac_acree-bldat = <fs_detfacturas>-bldat.
          wa_ret_fac_acree-blart = <fs_detfacturas>-blart.
          wa_ret_fac_acree-bschl = <fs_detfacturas>-bschl.
          wa_ret_fac_acree-drcrk = <fs_detfacturas>-drcrk.
          wa_ret_fac_acree-linetype = <fs_detfacturas>-linetype.
          wa_ret_fac_acree-ktosl = <fs_detfacturas>-ktosl.
          wa_ret_fac_acree-ktopl = <fs_detfacturas>-ktopl.
          wa_ret_fac_acree-kunnr = <fs_detfacturas>-kunnr.
          wa_ret_fac_acree-mwskz = <fs_detfacturas>-mwskz.
          wa_ret_fac_acree-hbkid = <fs_detfacturas>-hbkid.
          wa_ret_fac_acree-hktid = <fs_detfacturas>-hktid.
          wa_ret_fac_acree-augdt = <fs_detfacturas>-augdt.
          wa_ret_fac_acree-augbl = <fs_detfacturas>-augbl.
          wa_ret_fac_acree-auggj = <fs_detfacturas>-auggj.
          wa_ret_fac_acree-gkont = <fs_detfacturas>-gkont.
          wa_ret_fac_acree-rbudget_pd = <fs_detfacturas>-rbudget_pd.

          APPEND wa_ret_fac_acree TO it_ret_fac_acree.
          CLEAR: wa_ret_fac_acree.


        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT it_bru_fac_acree BY rbukrs gjahr belnr docln ASCENDING.
  SORT it_ing_fac_acree BY rbukrs gjahr belnr docln ASCENDING.
  SORT it_ret_fac_acree BY rbukrs gjahr belnr docln ASCENDING.
  SORT it_iva_fac_acree BY rbukrs gjahr belnr docln ASCENDING.

*15.  Determinación de carta porte de factura
  IF it_fac_venta IS NOT INITIAL.
    SELECT bukrs,
    belnr,
    gjahr,
    awkey
  FROM bkpf
  INTO TABLE @DATA(it_bkpf_carta)
  FOR ALL ENTRIES IN @it_fac_venta
  WHERE bukrs EQ @it_fac_venta-rbukrs AND
     belnr EQ @it_fac_venta-belnr  AND
     gjahr EQ @it_fac_venta-gjahr.
    IF sy-subrc EQ 0.

      SORT it_bkpf_carta BY bukrs belnr gjahr awkey.

      SELECT vbelv,
             posnv,
             vbeln,
             posnn
        FROM vbfa
        INTO TABLE @DATA(it_vbfa)
        FOR ALL ENTRIES IN @it_bkpf_carta
        WHERE vbeln EQ @it_bkpf_carta-awkey(10) AND
              stufe IN @rg_stufe AND
              vbtyp_v EQ 'C'.
      IF sy-subrc EQ 0.
        SORT it_vbfa BY vbelv posnv.

        SELECT vbeln,
               posnr,
               bstkd
          FROM vbkd
          INTO TABLE @DATA(it_vbkd)
          FOR ALL ENTRIES IN @it_vbfa
          WHERE vbeln EQ @it_vbfa-vbelv AND
                posnr EQ @it_vbfa-posnv.
        IF sy-subrc EQ 0.
          SORT it_vbkd BY vbeln posnr.
        ENDIF.

      ENDIF.

    ENDIF.
  ENDIF.
  IF it_bkpf_carta IS NOT INITIAL.

    LOOP AT it_bkpf_carta ASSIGNING FIELD-SYMBOL(<fs_carta>).

      LOOP AT it_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>) WHERE vbeln = <fs_carta>-awkey.

        sort it_vbkd by vbeln posnr.
        READ TABLE it_vbkd
        ASSIGNING FIELD-SYMBOL(<fs_vbkd>)
        WITH KEY vbeln = <fs_vbfa>-vbelv
                 posnr = <fs_vbfa>-posnv
        BINARY SEARCH.
        IF sy-subrc EQ 0.

          wa_carta_porte-bukrs = <fs_carta>-bukrs.
          wa_carta_porte-belnr = <fs_carta>-belnr.
          wa_carta_porte-gjahr = <fs_carta>-gjahr.
          wa_carta_porte-awkey = <fs_carta>-awkey.
          wa_carta_porte-bstkd = <fs_vbkd>-vbeln.

          APPEND  wa_carta_porte TO it_carta_porte.
          CLEAR: wa_carta_porte.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

* REVISION DE CONSULTAS DE LA LOGICA EN EL ARMADO DEL REPORTE.
  SELECT partner,
         name_org1,
         name_org2,
         name_org3,
         name_org4,
         name_last,
         name_first
  FROM but000
  APPENDING CORRESPONDING FIELDS OF TABLE @it_but000
  FOR ALL ENTRIES IN @it_bru_fac_acree
  WHERE partner EQ @it_bru_fac_acree-kunnr.
  IF sy-subrc EQ 0.
    SORT it_but000 BY partner.
  ENDIF.

  SELECT saknr,
         txt50
 FROM skat
 INTO CORRESPONDING FIELDS OF TABLE @it_skat
 FOR ALL ENTRIES IN @it_ing_fac_acree
 WHERE spras EQ 'S' AND
       ktopl EQ 'GPTX' AND
       saknr EQ @it_ing_fac_acree-racct.
  IF sy-subrc EQ 0.
    SORT it_skat BY saknr.
  ENDIF.

*  IF it_bkpf_fac IS NOT INITIAL. "ASALGADO 29.04.2022
  IF it_fac_venta IS NOT INITIAL. "ASALGADO 29.04.2022
    SELECT bukrs,
         belnr,
         gjahr,
         kursf,
         waers,
         xblnr
    FROM bkpf
    INTO CORRESPONDING FIELDS OF TABLE @it_bkpf_fac
    FOR ALL ENTRIES IN @it_fac_venta
    WHERE bukrs EQ @it_fac_venta-rbukrs AND
          belnr EQ @it_fac_venta-belnr  AND
          gjahr EQ @it_fac_venta-gjahr.
    IF sy-subrc EQ 0.
      SORT it_bkpf_fac BY bukrs belnr gjahr.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form ARMAR_REPORTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM armar_reporte .

  REFRESH: it_ing_fac_acree_aux,
           it_reporte_aux.
  "16.  Armado de tabla de reporte para sección de cobro

  LOOP AT it_bseg ASSIGNING FIELD-SYMBOL(<fs_bseg_rep>).

    IF r1 EQ c_x.

*      wa_reporte-zsema = '@08@'. "Semáforo

      LOOP AT it_extracto ASSIGNING FIELD-SYMBOL(<fs_extracto>)
        WHERE bukrs EQ <fs_bseg_rep>-bukrs and
              belnr EQ <fs_bseg_rep>-belnr and
              gjahr EQ <fs_bseg_rep>-gjahr.

        IF <fs_extracto>-belnr NE <fs_extracto>-augbl.

          wa_reporte-zfec_cobro = <fs_extracto>-budat. "Fecha cobro
          wa_reporte-zref_cob = <fs_extracto>-xblnr. "Referencia
          wa_reporte-zdescrip = <fs_extracto>-sgtxt. "Descripción cobro
          wa_reporte-zdocto_cobro = <fs_extracto>-belnr. "Documento de cobro
          wa_reporte-zejer_cobro = <fs_extracto>-gjahr. "Año de cobro

          sort it_t012t by spras bukrs hbkid hktid.
          READ TABLE it_t012t
                      ASSIGNING FIELD-SYMBOL(<fs_t012t>)
                      WITH KEY spras = c_es
                               bukrs = <fs_extracto>-bukrs
                               hbkid = <fs_extracto>-hbkid
                               hktid = <fs_extracto>-hktid
                       BINARY SEARCH.
          IF sy-subrc EQ 0.
            wa_reporte-zbanco_cobro = <fs_t012t>-text1. "Banco del cobro
          ENDIF.

          IF <fs_extracto>-rwcur NE 'MXN'.

            wa_reporte-ztc_cobro = <fs_extracto>-kursf. "TC Cobro



            LOOP AT it_detcobro ASSIGNING FIELD-SYMBOL(<fs_detcobro>)
              WHERE rbukrs = <fs_extracto>-bukrs and
                    belnr = <fs_extracto>-belnr and
                    gjahr = <fs_extracto>-gjahr.

              if <fs_detcobro>-kunnr is NOT INITIAL.
                wa_reporte-zno_cliente = <fs_detcobro>-kunnr.
                lcl_isr_tools=>map_client_data( exporting kunnr = wa_reporte-zno_cliente
                                            it_dfkkbptaxnum1 = it_dfkkbptaxnum
                                            it_but0001 = it_but000
                                 IMPORTING wa_reporte1 = wa_reporte ).
              endif.

              IF <fs_detcobro>-belnr EQ <fs_extracto>-belnr AND <fs_detcobro>-racct IN rg_cuentas_ing_ban
                 AND <fs_detcobro>-bschl EQ '40' AND <fs_detcobro>-linetype IN rg_clave_deudor .
                wa_reporte-zcobro_me = <fs_detcobro>-wsl. "Cobro en ME
                IF <fs_detcobro>-hsl < 0.
                  <fs_detcobro>-hsl = <fs_detcobro>-hsl * -1.
                  wa_reporte-zcobro_mn = wa_reporte-zcobro_mn + <fs_detcobro>-hsl.
                ELSE.
                  wa_reporte-zcobro_mn = wa_reporte-zcobro_mn + <fs_detcobro>-hsl.
                ENDIF.

              ENDIF.

              IF <fs_detcobro>-belnr EQ <fs_extracto>-belnr AND <fs_detcobro>-racct IN rg_cuentas_per_rel
                 AND <fs_detcobro>-bschl EQ '40' AND <fs_detcobro>-linetype IN rg_clave_util_rea.
                wa_reporte-zper_real = <fs_detcobro>-hsl. "Perdida realizada
              ENDIF.

              IF <fs_detcobro>-belnr EQ <fs_extracto>-belnr AND <fs_detcobro>-racct IN rg_cuentas_util_rel
                AND <fs_detcobro>-bschl EQ '40' AND <fs_detcobro>-linetype IN rg_clave_util_rea.
                wa_reporte-zutil_real = <fs_detcobro>-hsl. "Utilidad Realizada
              ENDIF.

            ENDLOOP.

          ELSE.

            LOOP AT it_detcobro ASSIGNING FIELD-SYMBOL(<fs_detcobromx>) WHERE   rbukrs EQ <fs_extracto>-bukrs and
                                                                                belnr EQ <fs_extracto>-belnr and
                                                                                gjahr EQ <fs_extracto>-gjahr.
              if <fs_detcobromx>-kunnr is NOT INITIAL.
                wa_reporte-zno_cliente = <fs_detcobromx>-kunnr.
                lcl_isr_tools=>map_client_data( exporting kunnr = wa_reporte-zno_cliente
                                            it_dfkkbptaxnum1 = it_dfkkbptaxnum
                                            it_but0001 = it_but000
                                 IMPORTING wa_reporte1 = wa_reporte ).
              endif.

              IF <fs_detcobromx>-belnr EQ <fs_extracto>-belnr AND <fs_detcobromx>-bschl EQ '40' AND <fs_detcobromx>-linetype IN rg_clave_balance.
                IF <fs_detcobromx>-hsl < 0.
                  <fs_detcobromx>-hsl = <fs_detcobromx>-hsl * -1.
                  wa_reporte-zcobro_mn = wa_reporte-zcobro_mn + <fs_detcobromx>-hsl.
                ELSE.
                  wa_reporte-zcobro_mn = wa_reporte-zcobro_mn + <fs_detcobromx>-hsl.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.

          READ TABLE it_cobros ASSIGNING FIELD-SYMBOL(<fs_cobros_ext2>)
                            WITH KEY rbukrs = <fs_extracto>-bukrs
                                     augbl  = <fs_extracto>-belnr
                                     gjahr  = <fs_extracto>-gjahr.
          IF sy-subrc EQ 0.

            wa_reporte-zsociedad =  <fs_cobros_ext2>-rbukrs.
            wa_reporte-zejercicio =  <fs_cobros_ext2>-gjahr.
            wa_reporte-zperiodo =  <fs_cobros_ext2>-poper.
            wa_reporte-zno_cliente = <fs_cobros_ext2>-kunnr.

            lcl_isr_tools=>map_client_data( exporting kunnr = wa_reporte-zno_cliente
                                                      it_dfkkbptaxnum1 = it_dfkkbptaxnum
                                                      it_but0001 = it_but000
                                           IMPORTING wa_reporte1 = wa_reporte ).
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSEIF  r2 EQ c_x.
      LOOP AT it_sin_extracto ASSIGNING FIELD-SYMBOL(<fs_sinextracto>) WHERE belnr EQ <fs_bseg_rep>-belnr.

        wa_reporte-zfec_cobro = <fs_sinextracto>-budat. "Fecha cobro
        wa_reporte-zref_cob = <fs_sinextracto>-xblnr. "Referencia
        wa_reporte-zdescrip = <fs_sinextracto>-sgtxt. "Descripción cobro
        wa_reporte-zdocto_cobro = <fs_sinextracto>-belnr. "Documento de cobro
        wa_reporte-zejer_cobro  = <fs_sinextracto>-gjahr. "Documento de cobro

        sort it_t012t by spras bukrs hbkid hktid.

        READ TABLE it_t012t
                ASSIGNING FIELD-SYMBOL(<fs_t012ts>)
                WITH KEY spras = c_es
                         bukrs = <fs_sinextracto>-bukrs
                         hbkid = <fs_sinextracto>-hbkid
                         hktid = <fs_sinextracto>-hktid
                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_reporte-zbanco_cobro = <fs_t012ts>-text1. "Banco del cobro
        ENDIF.

        IF <fs_sinextracto>-rwcur NE 'MXN'.

          wa_reporte-ztc_cobro = <fs_sinextracto>-kursf. "TC Cobro

          LOOP AT it_detcobro ASSIGNING FIELD-SYMBOL(<fs_detcobros>) WHERE belnr EQ <fs_sinextracto>-belnr.

            IF <fs_detcobros>-belnr EQ <fs_sinextracto>-belnr AND <fs_detcobros>-racct IN rg_cuentas_ing_ban
               AND <fs_detcobros>-bschl EQ '40' AND <fs_detcobros>-linetype IN rg_clave_deudor .
*cambiar wa ASALGADO 03.05.2022
              wa_reporte-zcobro_me = <fs_detcobros>-wsl. "Cobro en ME --> EN LA EF ESTA WSL en la tabla no
            ENDIF.

            IF <fs_detcobros>-belnr EQ <fs_sinextracto>-belnr AND <fs_detcobros>-racct IN rg_cuentas_per_rel
               AND <fs_detcobros>-bschl EQ '40' AND <fs_detcobros>-linetype IN rg_clave_util_rea.
              wa_reporte-zper_real = wa_reporte-zper_real + <fs_detcobros>-hsl. "Perdida realizada
            ENDIF.

            IF <fs_detcobros>-belnr EQ <fs_sinextracto>-belnr AND <fs_detcobros>-racct IN rg_cuentas_util_rel
              AND <fs_detcobros>-bschl EQ '40' AND <fs_detcobros>-linetype IN rg_clave_util_rea.
              wa_reporte-zutil_real = wa_reporte-zutil_real + <fs_detcobros>-hsl. "Utilidad Realizada
            ENDIF.

            wa_reporte-zsociedad =  <fs_detcobros>-rbukrs.
            wa_reporte-zejercicio =  <fs_detcobros>-gjahr.
          ENDLOOP.

        ELSE.

          LOOP AT it_detcobro ASSIGNING FIELD-SYMBOL(<fs_detcobromxs>) WHERE belnr EQ <fs_sinextracto>-belnr."<fs_extracto>-belnr. ARODEA

            IF <fs_detcobromxs>-belnr EQ <fs_extracto>-belnr AND <fs_detcobromxs>-bschl EQ '40' AND <fs_detcobromxs>-linetype IN rg_clave_balance.
              IF <fs_detcobromxs>-tsl < 0.
                <fs_detcobromxs>-tsl = <fs_detcobromxs>-tsl * - 1.
                wa_reporte-zcobro_mn = wa_reporte-zcobro_mn + <fs_detcobromxs>-tsl.
              ELSE.
                wa_reporte-zcobro_mn = wa_reporte-zcobro_mn + <fs_detcobromxs>-tsl.
              ENDIF.
            ENDIF.

            wa_reporte-zsociedad =  <fs_detcobromxs>-rbukrs.
            wa_reporte-zejercicio =  <fs_detcobromxs>-gjahr.

          ENDLOOP.

        ENDIF.

*  IF lv_test is not initial."arodea
        READ TABLE it_cobros ASSIGNING FIELD-SYMBOL(<fs_cobros_ext3>)  "ASALGADO
        WITH KEY rbukrs = <fs_sinextracto>-bukrs
        augbl  = <fs_sinextracto>-belnr
        gjahr  = <fs_sinextracto>-gjahr.
*FIN ASALGADO 03.05.2022
*        ENDIF."arodea
        IF sy-subrc EQ 0.

          wa_reporte-zsociedad =  <fs_cobros_ext3>-rbukrs.
          wa_reporte-zejercicio =  <fs_cobros_ext3>-gjahr.
          wa_reporte-zperiodo =  <fs_cobros_ext3>-poper.

        ENDIF.
      ENDLOOP.

    ENDIF.

    APPEND wa_reporte TO it_reporte.
    CLEAR: wa_reporte.

  ENDLOOP.

  it_ing_fac_acree_aux[] = it_ing_fac_acree[].
  it_reporte_aux[] = it_reporte[].

*  DELETE ADJACENT DUPLICATES FROM it_ing_fac_acree COMPARING racct.

  "17.  Armado de tabla de reporte para sección de factura de cliente
  REFRESH: it_reporte.
  CLEAR: vg_bandera2, wa_reporte, vg_document.

  LOOP AT it_reporte_aux ASSIGNING FIELD-SYMBOL(<fs_reporte>).
    CLEAR: vg_bandera2.

    <fs_reporte>-zsema = '@09@'. "Semáforo

    IF vg_document  NE <fs_reporte>-zdocto_cobro.

      vg_bandera2 = 1.

      LOOP AT it_fac_venta  ASSIGNING FIELD-SYMBOL(<fs_fac_venta>) WHERE  rbukrs EQ <fs_reporte>-zsociedad and
                                                                          augbl EQ <fs_reporte>-zdocto_cobro and
                                                                          auggj EQ <fs_reporte>-zejer_cobro.

        LOOP AT it_ing_fac_acree  ASSIGNING FIELD-SYMBOL(<fs_ing_fac_acree>) WHERE rbukrs EQ <fs_fac_venta>-rbukrs
            AND belnr EQ <fs_fac_venta>-belnr AND gjahr EQ <fs_fac_venta>-gjahr .

          CLEAR: vg_wsl, vg_tsl.


          IF vg_bandera2 EQ 1.

            CLEAR: vg_belnr.

            <fs_reporte>-zsema = '@08@'. "Semáforo
            wa_reporte-zsema = <fs_reporte>-zsema.
            wa_reporte-zfec_cobro = <fs_reporte>-zfec_cobro. "Fecha cobro
            wa_reporte-zref_cob = <fs_reporte>-zref_cob. "Referencia
            wa_reporte-zdescrip = <fs_reporte>-zdescrip. "Descripción cobro
            wa_reporte-zcobro_me = <fs_reporte>-zcobro_me. "Cobro en ME
            wa_reporte-zcobro_mn = <fs_reporte>-zcobro_mn.
            wa_reporte-zper_real = <fs_reporte>-zper_real. "Perdida realizada
            wa_reporte-zutil_real = <fs_reporte>-zutil_real. "Utilidad Realizada
            wa_reporte-zbanco_cobro = <fs_reporte>-zbanco_cobro. "Banco del cobro
            wa_reporte-zdocto_cobro = <fs_reporte>-zdocto_cobro. "Documento de cobro
            wa_reporte-zejer_cobro  = <fs_reporte>-zejer_cobro.
            wa_reporte-ztc_cobro = <fs_reporte>-ztc_cobro. "TC Cobro
            wa_reporte-zsociedad =  <fs_reporte>-zsociedad.
            wa_reporte-zejercicio =  <fs_reporte>-zejercicio.
            wa_reporte-zperiodo =  <fs_reporte>-zperiodo.
            wa_reporte-zfec_factura = <fs_fac_venta>-budat. "Fecha de factura
            wa_reporte-zclase_docto = <fs_fac_venta>-blart. "Clase de documento
            wa_reporte-zdocto_factura = <fs_fac_venta>-belnr. " Docto factura
            vg_belnr = <fs_fac_venta>-belnr.

            sort it_bkpf_fac by bukrs belnr gjahr.
            READ TABLE it_bkpf_fac
                 ASSIGNING FIELD-SYMBOL(<fs_bkpf1>)
                 WITH KEY bukrs = <fs_fac_venta>-rbukrs
                          belnr = <fs_fac_venta>-belnr
                          gjahr = <fs_fac_venta>-gjahr
                  BINARY SEARCH.
            IF sy-subrc EQ 0.
              wa_reporte-zasig = <fs_bkpf1>-xblnr. "Asignación

              IF <fs_bkpf1>-waers NE 'MXN'.
                wa_reporte-ztc_factura = <fs_bkpf1>-kursf. "TC Factura
              ENDIF.
            ENDIF.

            sort it_bru_fac_acree by rbukrs belnr gjahr.

            READ TABLE it_bru_fac_acree
                ASSIGNING FIELD-SYMBOL(<fs_bru_fac_acree>)
                WITH KEY rbukrs = <fs_fac_venta>-rbukrs
                         belnr = <fs_fac_venta>-belnr
                         gjahr = <fs_fac_venta>-gjahr
                 BINARY SEARCH.
            IF sy-subrc EQ 0.

              wa_reporte-zno_cliente = <fs_bru_fac_acree>-kunnr. "No de cliente
              lcl_isr_tools=>map_client_data( exporting kunnr = wa_reporte-zno_cliente
                                                         it_dfkkbptaxnum1 = it_dfkkbptaxnum
                                                         it_but0001 = it_but000
                                               IMPORTING wa_reporte1 = wa_reporte ).

              IF <fs_bru_fac_acree>-rwcur NE 'MXN'. " 26/04/2021 JRGN

                LOOP AT it_bru_fac_acree ASSIGNING FIELD-SYMBOL(<fs_bru_fac_acree2>) WHERE  rldnr  EQ '0L' and
                                                                                            rbukrs EQ <fs_bru_fac_acree>-rbukrs and
                                                                                            gjahr  EQ <fs_bru_fac_acree>-gjahr and
                                                                                            belnr  EQ <fs_bru_fac_acree>-belnr.

                  IF <fs_bru_fac_acree2>-drcrk EQ 'S' OR <fs_bru_fac_acree2>-bschl EQ '40'.
                    wa_reporte-zimporte_fact_me  = wa_reporte-zimporte_fact_me + <fs_bru_fac_acree2>-wsl. "Imp. Fact en ME
                    IF <fs_bru_fac_acree2>-hsl < 0.
                      <fs_bru_fac_acree2>-hsl = <fs_bru_fac_acree2>-hsl * -1. "Imp. Fact en MN
                    ENDIF.
                    wa_reporte-zimporte_fac_mn = wa_reporte-zimporte_fac_mn + <fs_bru_fac_acree2>-hsl. "Imp. Fact en MN

                  ELSEIF <fs_bru_fac_acree2>-drcrk EQ 'H' OR <fs_bru_fac_acree2>-bschl EQ '50'.

                    IF <fs_bru_fac_acree2>-wsl < 0.
                      <fs_bru_fac_acree2>-wsl = <fs_bru_fac_acree2>-wsl * -1. "Imp. Fact en ME
                    ENDIF.

                    IF <fs_bru_fac_acree2>-hsl < 0.
                      <fs_bru_fac_acree2>-hsl = <fs_bru_fac_acree2>-hsl * -1. "Imp. Fact en MN
                    ENDIF.

                    wa_reporte-zimporte_fac_mn = wa_reporte-zimporte_fac_mn + <fs_bru_fac_acree2>-hsl. "Imp. Fact en MN
                    wa_reporte-zimporte_fact_me  = wa_reporte-zimporte_fact_me + <fs_bru_fac_acree2>-wsl. "Imp. Fact en ME

                  ENDIF.
                ENDLOOP.

              ELSEIF <fs_bru_fac_acree>-rwcur EQ 'MXN'. " 26/04/2021 JRGN

                LOOP AT it_bru_fac_acree ASSIGNING FIELD-SYMBOL(<fs_bru_fac_acreem>) WHERE  rldnr  EQ '0L' and
                                                                                            rbukrs EQ <fs_bru_fac_acree>-rbukrs and
                                                                                            gjahr EQ <fs_bru_fac_acree>-gjahr and
                                                                                            belnr EQ <fs_bru_fac_acree>-belnr.

                  IF <fs_bru_fac_acreem>-drcrk EQ 'S' OR <fs_bru_fac_acreem>-bschl EQ '40'.
*                    vg_banderad = 1.
                    wa_reporte-zimporte_fac_mn = wa_reporte-zimporte_fac_mn + <fs_bru_fac_acreem>-hsl. "Imp. Fact en MN
                  ELSEIF <fs_bru_fac_acreem>-drcrk EQ 'H' OR <fs_bru_fac_acreem>-bschl EQ '50'.
*                    vg_banderad = 2.
                    IF <fs_bru_fac_acreem>-hsl < 0.
                      <fs_bru_fac_acreem>-hsl = <fs_bru_fac_acreem>-hsl * -1. "Imp. Fact en MN
                    ENDIF.

                    wa_reporte-zimporte_fac_mn = wa_reporte-zimporte_fac_mn + <fs_bru_fac_acreem>-hsl. "Imp. Fact en MN

                  ENDIF.
                ENDLOOP.
              ENDIF.  " 26/04/2021 JRGN

            ELSE.
              wa_reporte-zno_cliente = <fs_fac_venta>-kunnr.
              lcl_isr_tools=>map_client_data( exporting kunnr = wa_reporte-zno_cliente
                                                        it_dfkkbptaxnum1 = it_dfkkbptaxnum
                                                        it_but0001 = it_but000
                                               IMPORTING wa_reporte1 = wa_reporte ).
            ENDIF.

            CLEAR:vg_banderad.

            CONCATENATE <fs_fac_venta>-rbukrs <fs_fac_venta>-belnr <fs_fac_venta>-gjahr INTO v_name.
            CALL FUNCTION 'ZFI_ISR_READ_TEXT_BUFFER'
              EXPORTING
*               CLIENT                  = SY-MANDT
                ID                      = 'YUUD'
                LANGUAGE                = c_es
                NAME                    = v_name
                OBJECT                  = 'BELEG'
*               ARCHIVE_HANDLE          = 0
*               LOCAL_CAT               = ' '
*             IMPORTING
*               HEADER                  =
*               OLD_LINE_COUNTER        =
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
                wa_reporte-zuuid = <fs_line>-tdline.
              ENDLOOP.

            ENDIF.

            CLEAR: v_name.
            REFRESH: it_line.
            sort it_carta_porte by bukrs belnr gjahr.
            READ TABLE it_carta_porte
            ASSIGNING FIELD-SYMBOL(<fs_carta_porte>)
            WITH KEY bukrs = <fs_fac_venta>-rbukrs
                     belnr = <fs_fac_venta>-belnr
                     gjahr = <fs_fac_venta>-gjahr
             BINARY SEARCH.
            IF sy-subrc EQ 0.
              wa_reporte-zcarta_porte = <fs_carta_porte>-bstkd. "Carta Porte
            ENDIF.

            IF <fs_ing_fac_acree>-racct IN rg_ctas_ing_vta.

              wa_res_ctas-rbukrs = <fs_ing_fac_acree>-rbukrs.
              wa_res_ctas-belnr  = <fs_ing_fac_acree>-belnr.
              wa_res_ctas-gjahr  = <fs_ing_fac_acree>-gjahr.
              wa_res_ctas-mwskz  = <fs_ing_fac_acree>-mwskz.
              wa_res_ctas-racct  = <fs_ing_fac_acree>-racct.


              IF <fs_ing_fac_acree>-drcrk EQ 'S' OR <fs_ing_fac_acree>-bschl EQ '40'.

                IF <fs_ing_fac_acree>-hsl < 0.
                  <fs_ing_fac_acree>-hsl = <fs_ing_fac_acree>-hsl * -1.
                ENDIF.

                wa_res_ctas-hsl  =  <fs_ing_fac_acree>-hsl.

              ELSEIF <fs_ing_fac_acree>-drcrk EQ 'H' OR <fs_ing_fac_acree>-bschl EQ '50'.

                IF <fs_ing_fac_acree>-hsl < 0.
                  <fs_ing_fac_acree>-hsl = <fs_ing_fac_acree>-hsl * -1.
                ENDIF.

                wa_res_ctas-hsl  = <fs_ing_fac_acree>-hsl.

              ENDIF.

              IF wa_res_ctas-mwskz IN rg_base_16.

                wa_reporte-zimp_base_16 =  wa_res_ctas-hsl. "Importe grabado al 16%
                wa_reporte-zind_16 =  wa_res_ctas-mwskz. "Importe grabado al 16%
              ENDIF.

              IF wa_res_ctas-mwskz IN rg_base_exc.

                wa_reporte-zimp_base_exc =  wa_res_ctas-hsl. "Importe Exento
                wa_reporte-zind_exc =  wa_res_ctas-mwskz. "Importe Exento
              ENDIF.

              IF wa_res_ctas-mwskz IN rg_base_0.

                wa_reporte-zimp_base_0 =  wa_res_ctas-hsl. "Importe Tasa 0
                wa_reporte-zind_imp =  wa_res_ctas-mwskz. "Importe Tasa 0

              ENDIF.

              sort it_skat by saknr.
              READ TABLE it_skat
              ASSIGNING FIELD-SYMBOL(<fs_skat>)
              WITH KEY saknr = <fs_ing_fac_acree>-racct
               BINARY SEARCH.
              IF sy-subrc EQ 0.

                wa_res_ctas-des_cta = <fs_skat>-txt50.
                wa_reporte-zdes_ingreso = <fs_skat>-txt50.

              ENDIF.

              APPEND wa_res_ctas TO it_res_ctas.

              CLEAR: wa_res_ctas.

            ENDIF.


            LOOP AT it_iva_fac_acree ASSIGNING FIELD-SYMBOL(<fs_iva_fac_acree>) WHERE rbukrs EQ <fs_fac_venta>-rbukrs
                 AND belnr EQ <fs_fac_venta>-belnr AND gjahr EQ <fs_fac_venta>-gjahr AND racct IN rg_ctas_iva_vta
                 AND mwskz IN rg_base_16.

              IF <fs_iva_fac_acree>-tsl < 0.
                <fs_iva_fac_acree>-tsl = <fs_iva_fac_acree>-tsl * -1.
              ENDIF.
              wa_reporte-ziva_al_16 = wa_reporte-ziva_al_16 + <fs_iva_fac_acree>-tsl. "IVA al 16%

            ENDLOOP.

            LOOP AT  it_ret_fac_acree  ASSIGNING FIELD-SYMBOL(<fs_ret_fac_acree>) WHERE rbukrs EQ <fs_fac_venta>-rbukrs
                  AND belnr EQ <fs_fac_venta>-belnr AND gjahr EQ <fs_fac_venta>-gjahr .

              IF <fs_ret_fac_acree>-racct IN rg_ctas_ret_iva_ing_4 AND <fs_ret_fac_acree>-ktosl EQ 'WIT'.

                IF <fs_ret_fac_acree>-bschl EQ '40' OR <fs_ret_fac_acree>-drcrk EQ 'S'.

                  IF <fs_ret_fac_acree>-tsl > 0.

                    <fs_ret_fac_acree>-tsl = <fs_ret_fac_acree>-tsl * -1.

                  ENDIF.

                ENDIF.

                wa_reporte-ziva_ret_4 = wa_reporte-ziva_ret_4 + <fs_ret_fac_acree>-tsl. "IVA retenido  4% ""REVISAR CUAL ES EL CAMPO CORRECTO

              ENDIF.

              IF <fs_ret_fac_acree>-racct IN rg_ctas_ret_iva_ing AND <fs_ret_fac_acree>-ktosl EQ 'WIT'.

                IF <fs_ret_fac_acree>-bschl EQ '40' OR <fs_ret_fac_acree>-drcrk EQ 'S'.

                  IF <fs_ret_fac_acree>-tsl > 0.

                    <fs_ret_fac_acree>-tsl = <fs_ret_fac_acree>-tsl * -1.

                  ENDIF.

                ENDIF.

                wa_reporte-ziva_ret_6 = wa_reporte-ziva_ret_6 + <fs_ret_fac_acree>-tsl. "IVA retenido 6%

              ENDIF.

            ENDLOOP.

            if wa_reporte-Zclase_docto in rg_nc_clase or <fs_ing_fac_acree>-racct in rg_desc_reb.
              wa_reporte-zimp_base_16  = wa_reporte-zimp_base_16 * ( -1 ).
              wa_reporte-zimp_base_exc = wa_reporte-zimp_base_exc * ( -1 ).
              wa_reporte-zimp_base_0 = wa_reporte-zimp_base_0 * ( -1 ).
              wa_reporte-ziva_al_16 =  wa_reporte-ziva_al_16 * ( -1 ).
              wa_reporte-ziva_ret_4 = wa_reporte-ziva_ret_4 * ( -1 ).
              wa_reporte-ziva_ret_6 = wa_reporte-ziva_ret_6 * ( -1 ).
              wa_reporte-ZIMPORTE_FACT_ME = wa_reporte-ZIMPORTE_FACT_ME * ( -1 ).
              wa_reporte-ZIMPORTE_FAC_MN = wa_reporte-ZIMPORTE_FAC_MN * ( -1 ).
            endif.

            wa_reporte-ztotal = wa_reporte-zimp_base_16 + wa_reporte-zimp_base_exc + wa_reporte-zimp_base_0
                    + wa_reporte-ziva_al_16 + wa_reporte-ziva_ret_4 + wa_reporte-ziva_ret_6.
            APPEND wa_reporte TO it_reporte.

            CLEAR: vg_bandera2, wa_reporte.

          ELSE.

            CLEAR: wa_reporte , vg_wsl, vg_tsl.

            wa_reporte-zno_cliente = <fs_fac_venta>-kunnr. "No de cliente
            lcl_isr_tools=>map_client_data( exporting kunnr = wa_reporte-zno_cliente
                                                      it_dfkkbptaxnum1 = it_dfkkbptaxnum
                                                      it_but0001 = it_but000
                                            IMPORTING wa_reporte1 = wa_reporte ).

            <fs_reporte>-zsema = '@08@'. "Semáforo
            wa_reporte-zsema = <fs_reporte>-zsema.
            wa_reporte-zfec_factura = <fs_reporte>-zfec_factura.
            wa_reporte-zclase_docto = <fs_reporte>-zclase_docto.
            wa_reporte-zdocto_factura = <fs_fac_venta>-belnr.
            wa_reporte-zfec_cobro = <fs_reporte>-zfec_cobro. "Fecha cobro
            wa_reporte-zref_cob = <fs_reporte>-zref_cob. "Referencia
            wa_reporte-zdescrip = <fs_reporte>-zdescrip. "Descripción cobro
            wa_reporte-zdocto_cobro = <fs_reporte>-zdocto_cobro. "Documento de cobro
            wa_reporte-zejer_cobro = <fs_reporte>-zejer_cobro. "Documento de cobro
            wa_reporte-zbanco_cobro = <fs_reporte>-zbanco_cobro. "Banco del cobro
            wa_reporte-ztc_cobro = <fs_reporte>-ztc_cobro. "TC Cobro
            wa_reporte-zper_real = <fs_reporte>-zper_real. "Perdida realizada
            wa_reporte-zutil_real = <fs_reporte>-zutil_real. "Utilidad Realizada
            wa_reporte-zsociedad =  <fs_reporte>-zsociedad.
            wa_reporte-zejercicio =  <fs_reporte>-zejercicio.
            wa_reporte-zperiodo =  <fs_reporte>-zperiodo.

            IF <fs_ing_fac_acree>-racct IN rg_ctas_ing_vta.
              wa_reporte-zclase_docto = <fs_fac_venta>-blart.
              wa_reporte-zfec_factura = <fs_fac_venta>-budat.
            endif.
            sort it_bkpf_fac by bukrs belnr gjahr.

            READ TABLE it_bkpf_fac
               ASSIGNING FIELD-SYMBOL(<fs_bkpf3>)
               WITH KEY bukrs = <fs_fac_venta>-rbukrs
                        belnr = <fs_fac_venta>-belnr
                        gjahr = <fs_fac_venta>-gjahr
                BINARY SEARCH.
            IF sy-subrc EQ 0.
              wa_reporte-zasig = <fs_bkpf3>-xblnr. "Asignación

              IF <fs_bkpf3>-waers NE 'MXN'.
                wa_reporte-ztc_factura = <fs_bkpf3>-kursf. "TC Factura
              ENDIF.
            ENDIF.


            CLEAR: vg_banderad, v_name.
            REFRESH: it_line.

            CONCATENATE <fs_fac_venta>-rbukrs <fs_fac_venta>-belnr <fs_fac_venta>-gjahr INTO v_name.

            CALL FUNCTION 'ZFI_ISR_READ_TEXT_BUFFER'
              EXPORTING
*               CLIENT                  = SY-MANDT
                ID                      = 'YUUD'
                LANGUAGE                = c_es
                NAME                    = v_name
                OBJECT                  = 'BELEG'
*               ARCHIVE_HANDLE          = 0
*               LOCAL_CAT               = ' '
*             IMPORTING
*               HEADER                  =
*               OLD_LINE_COUNTER        =
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

              LOOP AT it_line ASSIGNING FIELD-SYMBOL(<fs_line1>).
                wa_reporte-zuuid = <fs_line1>-tdline.
              ENDLOOP.

            ENDIF.

            sort it_skat by saknr.
            READ TABLE it_skat
            ASSIGNING FIELD-SYMBOL(<fs_skat2>)
            WITH KEY saknr = <fs_ing_fac_acree>-racct
             BINARY SEARCH.
            IF sy-subrc EQ 0.

              wa_reporte-zdes_ingreso = <fs_skat2>-txt50.

            ENDIF.

            wa_reporte-zcarta_porte = <fs_reporte>-zcarta_porte.

            IF <fs_ing_fac_acree>-hsl < 0.

              <fs_ing_fac_acree>-hsl = <fs_ing_fac_acree>-hsl * -1.

            ENDIF.


            IF <fs_ing_fac_acree>-mwskz IN rg_base_16.

              wa_reporte-zimp_base_16 =  <fs_ing_fac_acree>-hsl. "Importe grabado al 16%
              wa_reporte-zind_16 =  <fs_ing_fac_acree>-mwskz. "Importe grabado al 16%
            ENDIF.

            IF <fs_ing_fac_acree>-mwskz IN rg_base_exc.

              wa_reporte-zimp_base_exc =  <fs_ing_fac_acree>-hsl. "Importe Exento
              wa_reporte-zind_exc =  <fs_ing_fac_acree>-mwskz. "Importe Exento
            ENDIF.

            IF <fs_ing_fac_acree>-mwskz IN rg_base_0.

              wa_reporte-zimp_base_0 =  <fs_ing_fac_acree>-hsl. "Importe Tasa 0
              wa_reporte-zind_imp =  <fs_ing_fac_acree>-mwskz. "Importe Tasa 0

            ENDIF.

            sort it_bru_fac_acree by rbukrs belnr gjahr.
            READ TABLE it_bru_fac_acree
                ASSIGNING FIELD-SYMBOL(<fs_bru_fac_acree1>)
                WITH KEY rbukrs = <fs_fac_venta>-rbukrs
                         belnr = <fs_fac_venta>-belnr
                         gjahr = <fs_fac_venta>-gjahr
                 BINARY SEARCH.
            IF sy-subrc EQ 0.

              wa_reporte-zno_cliente = <fs_bru_fac_acree1>-kunnr. "No de cliente

              lcl_isr_tools=>map_client_data( exporting kunnr = wa_reporte-zno_cliente
                                                         it_dfkkbptaxnum1 = it_dfkkbptaxnum
                                                         it_but0001 = it_but000
                                               IMPORTING wa_reporte1 = wa_reporte ).

              IF vg_belnr NE <fs_fac_venta>-belnr.


*              ############################# IMPORTES    ###########################

                IF <fs_bru_fac_acree1>-rwcur NE 'MXN'. " 26/04/2021 JRGN

                  LOOP AT it_bru_fac_acree ASSIGNING FIELD-SYMBOL(<fs_bru_fac_acree5>)
                    WHERE rldnr  EQ '0L' and
                          rbukrs EQ <fs_bru_fac_acree1>-rbukrs and
                          gjahr EQ <fs_bru_fac_acree1>-gjahr and
                          belnr EQ <fs_bru_fac_acree1>-belnr.
*                    belnr EQ <fs_fac_venta>-belnr.

                    IF <fs_bru_fac_acree5>-drcrk EQ 'S' OR <fs_bru_fac_acree5>-bschl EQ '40'.

                      wa_reporte-zimporte_fact_me  = wa_reporte-zimporte_fact_me + <fs_bru_fac_acree5>-wsl. "Imp. Fact en ME

                      IF <fs_bru_fac_acree5>-hsl < 0.

                        <fs_bru_fac_acree5>-hsl = <fs_bru_fac_acree5>-hsl * -1. "Imp. Fact en MN

                      ENDIF.

                      wa_reporte-zimporte_fac_mn = wa_reporte-zimporte_fac_mn + <fs_bru_fac_acree5>-hsl. "Imp. Fact en MN

                    ELSEIF <fs_bru_fac_acree5>-drcrk EQ 'H' OR <fs_bru_fac_acree5>-bschl EQ '50'.

                      IF <fs_bru_fac_acree5>-wsl < 0.

                        <fs_bru_fac_acree5>-wsl = <fs_bru_fac_acree5>-wsl * -1. "Imp. Fact en ME

                      ENDIF.

                      IF <fs_bru_fac_acree5>-hsl < 0.

                        <fs_bru_fac_acree5>-hsl = <fs_bru_fac_acree5>-hsl * -1. "Imp. Fact en MN

                      ENDIF.

                      wa_reporte-zimporte_fac_mn = wa_reporte-zimporte_fac_mn + <fs_bru_fac_acree5>-hsl. "Imp. Fact en MN

                      wa_reporte-zimporte_fact_me  = wa_reporte-zimporte_fact_me + <fs_bru_fac_acree5>-wsl. "Imp. Fact en ME

                    ENDIF.

                  ENDLOOP.

                ELSEIF <fs_bru_fac_acree1>-rwcur EQ 'MXN'. " 26/04/2021 JRGN

                  LOOP AT it_bru_fac_acree ASSIGNING FIELD-SYMBOL(<fs_bru_fac_acreem5>)
                    WHERE
                          rldnr  EQ '0L' and
                          rbukrs EQ <fs_bru_fac_acree1>-rbukrs and
                          gjahr  EQ <fs_bru_fac_acree1>-gjahr and
                          belnr  EQ <fs_bru_fac_acree1>-belnr.

*                     belnr EQ <fs_fac_venta>-belnr.

                    IF <fs_bru_fac_acreem5>-drcrk EQ 'S' OR <fs_bru_fac_acreem5>-bschl EQ '40'.
*                    vg_banderad = 1.

                      wa_reporte-zimporte_fac_mn = wa_reporte-zimporte_fac_mn + <fs_bru_fac_acreem5>-hsl. "Imp. Fact en MN

                    ELSEIF <fs_bru_fac_acreem5>-drcrk EQ 'H' OR <fs_bru_fac_acreem5>-bschl EQ '50'.
*                    vg_banderad = 2.


                      IF <fs_bru_fac_acreem5>-hsl < 0.

                        <fs_bru_fac_acreem5>-hsl = <fs_bru_fac_acreem5>-hsl * -1. "Imp. Fact en MN

                      ENDIF.

                      wa_reporte-zimporte_fac_mn = wa_reporte-zimporte_fac_mn + <fs_bru_fac_acreem5>-hsl. "Imp. Fact en MN

                    ENDIF.

                  ENDLOOP.

                ENDIF.  " 26/04/2021 JRGN


*              ############################# RETENCIONES ###########################

                LOOP AT it_iva_fac_acree ASSIGNING FIELD-SYMBOL(<fs_iva_fac_acree1>) WHERE rbukrs EQ <fs_fac_venta>-rbukrs
                  AND belnr EQ <fs_fac_venta>-belnr AND gjahr EQ <fs_fac_venta>-gjahr AND racct IN rg_ctas_iva_vta
                   AND mwskz IN rg_base_16.

                  IF <fs_iva_fac_acree1>-tsl < 0.

                    <fs_iva_fac_acree1>-tsl = <fs_iva_fac_acree1>-tsl * -1.

                  ENDIF.

                  wa_reporte-ziva_al_16 = wa_reporte-ziva_al_16 + <fs_iva_fac_acree1>-tsl. "IVA al 16%

                ENDLOOP.


                LOOP AT  it_ret_fac_acree  ASSIGNING FIELD-SYMBOL(<fs_ret_fac_acree1>)
                  WHERE rbukrs EQ <fs_fac_venta>-rbukrs
                    AND belnr EQ <fs_fac_venta>-belnr
                    AND gjahr EQ <fs_fac_venta>-gjahr.
*                    and racct  EQ <fs_ing_fac_acree>-racct.

                  IF <fs_ret_fac_acree1>-racct IN rg_ctas_ret_iva_ing_4 AND <fs_ret_fac_acree1>-ktosl EQ 'WIT'.

                    IF <fs_ret_fac_acree1>-bschl EQ '40' OR <fs_ret_fac_acree1>-drcrk EQ 'S'.

                      IF <fs_ret_fac_acree1>-tsl > 0.

                        <fs_ret_fac_acree1>-tsl = <fs_ret_fac_acree1>-tsl * -1.

                      ENDIF.

                    ENDIF.

                    wa_reporte-ziva_ret_4 = wa_reporte-ziva_ret_4 + <fs_ret_fac_acree1>-tsl. "IVA retenido  4% ""REVISAR CUAL ES EL CAMPO CORRECTO

                  ENDIF.

                  IF <fs_ret_fac_acree1>-racct IN rg_ctas_ret_iva_ing AND <fs_ret_fac_acree1>-ktosl EQ 'WIT'.

                    IF <fs_ret_fac_acree1>-bschl EQ '40' OR <fs_ret_fac_acree1>-drcrk EQ 'S'.

                      IF <fs_ret_fac_acree1>-tsl > 0.

                        <fs_ret_fac_acree1>-tsl =  <fs_ret_fac_acree1>-tsl * -1.

                      ENDIF.

                    ENDIF.

                    wa_reporte-ziva_ret_6 = wa_reporte-ziva_ret_6 + <fs_ret_fac_acree1>-tsl. "IVA retenido 6%

                  ENDIF.

                ENDLOOP.

*              ENDIF.

                CLEAR: vg_belnr.

                vg_belnr = <fs_fac_venta>-belnr.

              ENDIF.

            ENDIF.

            if wa_reporte-Zclase_docto in rg_nc_clase or <fs_ing_fac_acree>-racct in rg_desc_reb.
              wa_reporte-zimp_base_16  = wa_reporte-zimp_base_16 * ( -1 ).
              wa_reporte-zimp_base_exc = wa_reporte-zimp_base_exc * ( -1 ).
              wa_reporte-zimp_base_0 = wa_reporte-zimp_base_0 * ( -1 ).
              wa_reporte-ziva_al_16 =  wa_reporte-ziva_al_16 * ( -1 ).
              wa_reporte-ziva_ret_4 = wa_reporte-ziva_ret_4 * ( -1 ).
              wa_reporte-ziva_ret_6 = wa_reporte-ziva_ret_6 * ( -1 ).
              wa_reporte-ZIMPORTE_FACT_ME = wa_reporte-ZIMPORTE_FACT_ME * ( -1 ).
              wa_reporte-ZIMPORTE_FAC_MN = wa_reporte-ZIMPORTE_FAC_MN * ( -1 ).
            endif.

            wa_reporte-ztotal = wa_reporte-zimp_base_16 + wa_reporte-zimp_base_exc + wa_reporte-zimp_base_0
                    + wa_reporte-ziva_al_16 + wa_reporte-ziva_ret_4 + wa_reporte-ziva_ret_6.

            APPEND wa_reporte TO it_reporte.

            CLEAR: wa_reporte , vg_wsl, vg_tsl.

          ENDIF.


        ENDLOOP.

*    ENDIF.

      ENDLOOP.

    ELSEIF vg_document EQ <fs_reporte>-zdocto_cobro.
      <fs_reporte>-zsema = '@08@'. "Semáforo
    ENDIF.

    CLEAR: vg_document.

    vg_document = <fs_reporte>-zdocto_cobro.

  ENDLOOP.

  PERFORM arma_trapasos using it_tr_origen
                              it_tr_destino
                              it_reporte.

  PERFORM arma_ing_rec_seg using it_ing_tr_origen
                                 it_ing_tr_destino
                                 it_ing_iva
                                 it_reporte.
  PERFORM arma_intereses  using  it_int_origen
                                 it_int_destino
                                 it_int_compensa
                                 it_int_origen2
                                 it_int_destino2
                                 it_int_compensa2
                                 it_reporte.
  PERFORM arma_inversiones using it_inv_origen
                               it_inv_destino
                               it_reporte.

  PERFORM arma_intercos  using it_interco_origen
                             it_interco_destino
                             it_reporte.

  PERFORM arma_ot_ingre  using it_otring_origen
                             it_otring_destino
                             it_reporte.

  PERFORM arma_ot_indep  using it_otrdep_origen
                             it_otrdep_destino
                             it_reporte.

  PERFORM arma_fact_san using it_factsan_origen
                            it_factsan_destino
                            it_reporte.

  PERFORM arma_fact_simple using
                               it_fact_pago
                               it_fact_intermedio
                               it_fact_compensados
                               it_reporte
                               it_fact_detalle
                               it_fact_pago_detalle.

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
     ( fieldname   = 'ZSEMA'           scrtext_s   = TEXT-001 scrtext_m   = TEXT-001 scrtext_l   = TEXT-001 col_opt = abap_on )
     ( fieldname   = 'ZFEC_COBRO'      scrtext_s   = TEXT-002 scrtext_m   = TEXT-002 scrtext_l   = TEXT-002 col_opt = abap_on )
     ( fieldname   = 'ZREF_COB'        scrtext_s   = TEXT-003 scrtext_m   = TEXT-003 scrtext_l   = TEXT-003 col_opt = abap_on )
     ( fieldname   = 'ZSOCIEDAD'       scrtext_s   = TEXT-033 scrtext_m   = TEXT-033 scrtext_l   = TEXT-033 col_opt = abap_on )
     ( fieldname   = 'ZEJERCICIO'      scrtext_s   = TEXT-034 scrtext_m   = TEXT-034 scrtext_l   = TEXT-034 col_opt = abap_on )
     ( fieldname   = 'ZPERIODO'        scrtext_s   = TEXT-035 scrtext_m   = TEXT-035 scrtext_l   = TEXT-035 col_opt = abap_on )
     ( fieldname   = 'ZDESCRIP'        scrtext_s   = TEXT-004 scrtext_m   = TEXT-004 scrtext_l   = TEXT-004 col_opt = abap_on )
     ( fieldname   = 'ZCOBRO_ME'       scrtext_s   = TEXT-005 scrtext_m   = TEXT-005 scrtext_l   = TEXT-005 col_opt = abap_on )
     ( fieldname   = 'ZCOBRO_MN'       scrtext_s   = TEXT-006 scrtext_m   = TEXT-006 scrtext_l   = TEXT-006 col_opt = abap_on )
     ( fieldname   = 'ZPER_REAL'       scrtext_s   = TEXT-007 scrtext_m   = TEXT-007 scrtext_l   = TEXT-007 col_opt = abap_on )
     ( fieldname   = 'ZUTIL_REAL'      scrtext_s   = TEXT-008 scrtext_m   = TEXT-008 scrtext_l   = TEXT-008 col_opt = abap_on )
     ( fieldname   = 'ZBANCO_COBRO'    scrtext_s   = TEXT-009 scrtext_m   = TEXT-009 scrtext_l   = TEXT-009 col_opt = abap_on )
     ( fieldname   = 'ZDOCTO_COBRO'    scrtext_s   = TEXT-010 scrtext_m   = TEXT-010 scrtext_l   = TEXT-010 col_opt = abap_on key = abap_true hotspot = abap_true )
     ( fieldname   = 'ZTC_COBRO'       scrtext_s   = TEXT-011 scrtext_m   = TEXT-011 scrtext_l   = TEXT-011 col_opt = abap_on )
     ( fieldname   = 'ZFEC_FACTURA'    scrtext_s   = TEXT-012 scrtext_m   = TEXT-012 scrtext_l   = TEXT-012 col_opt = abap_on )
     ( fieldname   = 'ZDOCTO_FACTURA'  scrtext_s   = TEXT-013 scrtext_m   = TEXT-013 scrtext_l   = TEXT-013 col_opt = abap_on key = abap_true hotspot = abap_true )
     ( fieldname   = 'ZASIG'           scrtext_s   = TEXT-014 scrtext_m   = TEXT-014 scrtext_l   = TEXT-014 col_opt = abap_on )
     ( fieldname   = 'ZTC_FACTURA'     scrtext_s   = TEXT-015 scrtext_m   = TEXT-015 scrtext_l   = TEXT-015 col_opt = abap_on )
     ( fieldname   = 'ZNO_CLIENTE'     scrtext_s   = TEXT-016 scrtext_m   = TEXT-016 scrtext_l   = TEXT-016 col_opt = abap_on )
     ( fieldname   = 'ZDES_CLIENTE'    scrtext_s   = TEXT-017 scrtext_m   = TEXT-017 scrtext_l   = TEXT-017 col_opt = abap_on )
     ( fieldname   = 'ZRFC_CLIENTE'    scrtext_s   = TEXT-018 scrtext_m   = TEXT-018 scrtext_l   = TEXT-018 col_opt = abap_on )
     ( fieldname   = 'ZUUID'           scrtext_s   = TEXT-019 scrtext_m   = TEXT-019 scrtext_l   = TEXT-019 col_opt = abap_on )
     ( fieldname   = 'ZCARTA_PORTE'    scrtext_s   = TEXT-020 scrtext_m   = TEXT-020 scrtext_l   = TEXT-020 col_opt = abap_on )
     ( fieldname   = 'ZCLASE_DOCTO'    scrtext_s   = TEXT-021 scrtext_m   = TEXT-021 scrtext_l   = TEXT-021 col_opt = abap_on )
     ( fieldname   = 'ZIMPORTE_FACT_ME' scrtext_s   = TEXT-022 scrtext_m   = TEXT-022 scrtext_l   = TEXT-022 col_opt = abap_on EDIT_MASK = '==ZZSIGN')
     ( fieldname   = 'ZIMPORTE_FAC_MN' scrtext_s   = TEXT-023 scrtext_m   = TEXT-023 scrtext_l   = TEXT-023 col_opt = abap_on EDIT_MASK = '==ZZSIGN')
     ( fieldname   = 'ZDES_INGRESO'    scrtext_s   = TEXT-024 scrtext_m   = TEXT-024 scrtext_l   = TEXT-024 col_opt = abap_on )
     ( fieldname   = 'ZIMP_BASE_16'    scrtext_s   = TEXT-025 scrtext_m   = TEXT-025 scrtext_l   = TEXT-025 col_opt = abap_on EDIT_MASK = '==ZZSIGN')
     ( fieldname   = 'ZIND_16'         scrtext_s   = TEXT-036 scrtext_m   = TEXT-036 scrtext_l   = TEXT-036 col_opt = abap_on )
     ( fieldname   = 'ZIMP_BASE_EXC'   scrtext_s   = TEXT-026 scrtext_m   = TEXT-026 scrtext_l   = TEXT-026 col_opt = abap_on EDIT_MASK = '==ZZSIGN')
     ( fieldname   = 'ZIND_EXC'        scrtext_s   = TEXT-037 scrtext_m   = TEXT-037 scrtext_l   = TEXT-037 col_opt = abap_on EDIT_MASK = '==ZZSIGN')
     ( fieldname   = 'ZIMP_BASE_0'     scrtext_s   = TEXT-027 scrtext_m   = TEXT-027 scrtext_l   = TEXT-027 col_opt = abap_on EDIT_MASK = '==ZZSIGN')
     ( fieldname   = 'ZIND_IMP'        scrtext_s   = TEXT-028 scrtext_m   = TEXT-028 scrtext_l   = TEXT-028 col_opt = abap_on )
     ( fieldname   = 'ZIVA_AL_16'      scrtext_s   = TEXT-029 scrtext_m   = TEXT-029 scrtext_l   = TEXT-029 col_opt = abap_on EDIT_MASK = '==ZZSIGN')
     ( fieldname   = 'ZIVA_RET_4'      scrtext_s   = TEXT-030 scrtext_m   = TEXT-030 scrtext_l   = TEXT-030 col_opt = abap_on EDIT_MASK = '==ZZSIGN')
     ( fieldname   = 'ZIVA_RET_6'      scrtext_s   = TEXT-031 scrtext_m   = TEXT-031 scrtext_l   = TEXT-031 col_opt = abap_on no_out = 'X')
     ( fieldname   = 'ZOTR_INGR'       scrtext_s   = TEXT-038 scrtext_m   = TEXT-038 scrtext_l   = TEXT-038 col_opt = abap_on EDIT_MASK = '==ZZSIGN')
     ( fieldname   = 'ZOTR_DEPOS'      scrtext_s   = TEXT-039 scrtext_m   = TEXT-039 scrtext_l   = TEXT-039 col_opt = abap_on EDIT_MASK = '==ZZSIGN')
     ( fieldname   = 'ZTOTAL'          scrtext_s   = TEXT-032 scrtext_m   = TEXT-032 scrtext_l   = TEXT-032 col_opt = abap_on EDIT_MASK = '==ZZSIGN' )
              ).

ENDFORM.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM fm_alv_resumen USING e_row_id e_column_id es_row_no sender. "Construimos el ALV de resumen de posiciones
  ENDMETHOD."on_link_click

ENDCLASS. "lcl_event_handler IMPLEMENTATION
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

  IF lo_grid IS INITIAL.
    CREATE OBJECT lo_container
      EXPORTING
        container_name = 'ALV_CONTAINER'.

    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = lo_container.
  ENDIF.

  "SET EVENT HANDLER
*  lo_events = lo_alv->get_event( ).
  IF lo_event_handler IS NOT BOUND.
    CREATE OBJECT lo_event_handler.
  ENDIF.

  SET HANDLER: lo_event_handler->handle_hotspot_click FOR lo_grid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND' "handle_user_command
      is_layout_lvc            = gd_layout
      it_fieldcat_lvc          = tl_fieldcat
      i_save                   = abap_on
    TABLES
      t_outtab                 = it_reporte.



ENDFORM.
FORM limpiar_variables .


  CLEAR: wa_cobros,
         wa_extracto,
         wa_sin_extracto,
         wa_detcobro,
         wa_fac_venta,
         wa_detfacturas,
         wa_carta_porte,
         wa_tvar_zdoc,
         wa_zbancos_ingresos,
         wa_zcargo_abono,
         wa_zdoc_fac_cl,
         wa_bru_fac_acree,
         wa_ing_fac_acree,
         wa_iva_fac_acree,
         wa_ret_fac_acree,
         wa_reporte,
         wa_res_ctas,
         wa_column_id,
         wa_row_no,
         lo_grid,
         lo_container,
         lo_event_handler,
         wa_variant,
         wa_layout,
         rg_bancos_ingresos,
         rg_doc_fac_cl,
         rg_tvar_doc,
         rg_cargo_abono,
         rg_abono_cargo,
         rg_abono_cargo_high,
         rg_clave_deudor,
         rg_clave_ing,
         rg_ctas_ing_vta,
         rg_clave_iva,
         rg_ctas_iva_ing,
         rg_clave_ret_iva,
         rg_ctas_ret_iva_ing,
         rg_cuentas_ing_ban,
         rg_ctas_comision,
         rg_clave_comision,
         rg_clave_balance,
         rg_cuentas_per_rel,
         rg_clave_util_rea,
         rg_cuentas_util_rel,
         rg_base_16,
         rg_base_exc,
         rg_ctas_iva_vta,
         rg_ctas_ret_iva_ing_4,
         rg_base_0,
         rg_stufe,
         gd_layout,
         vg_banderad,
         v_name,
         v_object,
         vg_total,
         gv_global,
         gv_resumen,
         vg_bandera2,
         vg_document,
         vg_tsl,
         vg_wsl,
         vg_belnr,
         it_ing_conceptos,
         it_tr_origen,
         it_tr_destino,
         it_ing_tr_origen,
         it_ing_tr_destino,
         it_ing_IVA,
         it_int_origen,
         it_int_destino,
         it_inv_origen,
         it_inv_destino,
         it_interco_origen,
         it_interco_destino,
         it_otring_origen,
         it_otring_destino,
         it_otrdep_origen,
         it_otrdep_destino,
         it_factsan_origen,
         it_factsan_destino,
         it_fact_pago,
         it_fact_pago_detalle,
         it_fact_intermedio,
         it_fact_compensados,
         it_fact_detalle.

  REFRESH: it_cobros,
      it_extracto,
      it_sin_extracto,
      it_detcobro,
      it_detcobro2,
      it_fac_venta,
      it_detfacturas,
      it_carta_porte,
      it_tvar_zdoc,
      it_zbancos_ingresos,
      it_zcargo_abono,
      it_zdoc_fac_cl,
      it_bru_fac_acree,
      it_ing_fac_acree,
      it_iva_fac_acree,
      it_ret_fac_acree,
      it_reporte,
      it_bseg,
      it_bkpf,
      it_t012t,
      it_dfkkbptaxnum,
      it_line,
      it_but000,
      it_skat,
      it_res_ctas,
      tl_ztax_detalle_ing,
      it_ing_fac_acree_aux,
      it_bkpf_fac_ven,
      it_bkpf_fac,
      it_reporte_aux,
      it_dfkkbptaxnum2.

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

  IF it_reporte IS NOT INITIAL.
    LOOP AT it_reporte ASSIGNING FIELD-SYMBOL(<fs_report>).
      tl_ztax_detalle_ing = VALUE  #(
                         BASE tl_ztax_detalle_ing ( zsema           =  <fs_report>-zsema
                                                    zfec_cobro      =  <fs_report>-zfec_cobro
                                                    zref_cob        =  <fs_report>-zref_cob
                                                    zsociedad       =  <fs_report>-zsociedad
                                                    zejercicio      =  <fs_report>-zejercicio
                                                    zperiodo        =  <fs_report>-zperiodo
                                                    zdescrip        =  <fs_report>-zdescrip
                                                    zcobro_me       =  <fs_report>-zcobro_me
                                                    zcobro_mn       =  <fs_report>-zcobro_mn
                                                    zper_real       =  <fs_report>-zper_real
                                                    zutil_real      =  <fs_report>-zutil_real
                                                    zbanco_cobro    =  <fs_report>-zbanco_cobro
                                                    zdocto_cobro    =  <fs_report>-zdocto_cobro
                                                    ztc_cobro       =  <fs_report>-ztc_cobro
                                                    zfec_factura    =  <fs_report>-zfec_factura
                                                    zdocto_factura  =  <fs_report>-zdocto_factura
                                                    zasign          =  <fs_report>-zasig
                                                    ztc_factura     =  <fs_report>-ztc_factura
                                                    zno_cliente     =  <fs_report>-zno_cliente
                                                    zdes_cliente    =  <fs_report>-zdes_cliente
                                                    zrfc_cliente    =  <fs_report>-zrfc_cliente
                                                    zuiid           =  <fs_report>-zuuid
                                                    zcarta_porte    =  <fs_report>-zcarta_porte
                                                    zclase_docto    =  <fs_report>-zclase_docto
                                                    zimporte_fact_me =  <fs_report>-zimporte_fact_me
                                                    zimporte_fact_mn  =  <fs_report>-zimporte_fac_mn
                                                    zdes_ingreso      =  <fs_report>-zdes_ingreso
                                                    zimp_base_16      =  <fs_report>-zimp_base_16
                                                    zind_16           =  <fs_report>-zind_16
                                                    zimp_base_exc     =  <fs_report>-zimp_base_exc
                                                    zind_exc          =  <fs_report>-zind_exc
                                                    zimp_base_0       =  <fs_report>-zimp_base_0
                                                    zind_imp          =  <fs_report>-zind_imp
                                                    ziva_al_16        =  <fs_report>-ziva_al_16
                                                    ziva_ret_4        =  <fs_report>-ziva_ret_4
                                                    ziva_ret_6        =  <fs_report>-ziva_ret_6
                                                    ztotal            =  <fs_report>-ztotal
                                                    ) ).
    ENDLOOP.
    MODIFY ztax_detalle_ing FROM TABLE tl_ztax_detalle_ing.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FM_ALV_RESUMEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID
*&      --> E_COLUMN_ID
*&      --> ES_ROW_NO
*&      --> SENDER
*&---------------------------------------------------------------------*
FORM fm_alv_resumen  USING i_row_id        TYPE lvc_s_row
                           i_column_id     TYPE lvc_s_col
                           is_row_no       TYPE lvc_s_roid
                           sender.

  CLEAR:  wa_column_id,
            wa_row_no,
            gv_global,
            gv_resumen.

  IF i_column_id EQ 'ZDOCTO_COBRO'.

    lo_grid->get_scroll_info_via_id(
  IMPORTING
    es_row_no   = wa_row_no
    es_col_info = wa_column_id
     ).

    CASE sender.    "ALV que fue presionado
      WHEN lo_grid. "Reporte Global
        gv_global = abap_true.
    ENDCASE.

    UNASSIGN <fs_report_final>.
    ASSIGN it_reporte[ is_row_no-row_id ] TO <fs_report_final>. "Leemos el CHECK seleccionado

    IF sy-subrc EQ 0.

      SET PARAMETER ID 'BLN' FIELD <fs_report_final>-zdocto_cobro. "Pasamos el pedido seleccionado
      SET PARAMETER ID 'BUK' FIELD <fs_report_final>-zsociedad. "Pasamos el pedido seleccionado
      SET PARAMETER ID 'GJR' FIELD <fs_report_final>-zejercicio. "Pasamos el pedido seleccionado
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN. "Y llamamos a la transacción ME22N
*      WAIT UP TO 2 SECONDS.
*      COMMIT WORK.
    ENDIF.

  ELSEIF i_column_id EQ 'ZDOCTO_FACTURA'.
    lo_grid->get_scroll_info_via_id(
              IMPORTING
                es_row_no   = wa_row_no
                es_col_info = wa_column_id
                 ).

    CASE sender.    "ALV que fue presionado
      WHEN lo_grid. "Reporte Global
        gv_global = abap_true.
    ENDCASE.

    UNASSIGN <fs_report_final>.
    ASSIGN it_reporte[ is_row_no-row_id ] TO <fs_report_final>. "Leemos el CHECK seleccionado

    IF sy-subrc EQ 0.

      SET PARAMETER ID 'BLN' FIELD <fs_report_final>-zdocto_factura. "Pasamos el pedido seleccionado
      SET PARAMETER ID 'BUK' FIELD <fs_report_final>-zsociedad. "Pasamos el pedido seleccionado
      SET PARAMETER ID 'GJR' FIELD <fs_report_final>-zfec_factura+0(4). "Pasamos el pedido seleccionado
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN. "Y llamamos a la transacción ME22N
      WAIT UP TO 2 SECONDS.
      COMMIT WORK.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZSTANDARD'.

*  PERFORM build_fieldcat.
*  PERFORM build_layout.
*  PERFORM display_alv_report.

  "***** REPORTE GLOBAL *****"
  CLEAR: wa_layout,
         wa_variant.

  PERFORM build_fieldcat. "Propiedades de columnas

  IF lo_grid IS INITIAL.
    CREATE OBJECT lo_container
      EXPORTING
        container_name = 'ALV_CONTAINER'.

    CREATE OBJECT lo_grid
      EXPORTING
        i_parent = lo_container.
  ENDIF.

  "SET EVENT HANDLER
*  lo_events = lo_alv->get_event( ).
  IF lo_event_handler IS NOT BOUND.
    CREATE OBJECT lo_event_handler.
  ENDIF.

  SET HANDLER: lo_event_handler->handle_hotspot_click FOR lo_grid.
  wa_variant = GX_VARIANT.
  wa_layout-zebra = 'X'.
  wa_layout-CWIDTH_OPT = 'X'.
  CALL METHOD lo_grid->set_table_for_first_display
    EXPORTING
      is_variant                    = wa_variant "Estos dos parámetros se pasan para poder guardar variantes del layout
      i_save                        = 'A'
      is_layout                     = wa_layout
    CHANGING
      it_outtab                     = it_reporte[]
      it_fieldcatalog               = tl_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
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
