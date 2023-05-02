*&---------------------------------------------------------------------*
*& Include          ZISRMENSUAL_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_data.

************************************************************************
****                       CONSULTAS                                ****
************************************************************************
  CLEAR gv_nombre_set.

********* -> MODIFICANDO Y AGREGANDO BUSQUEDA DE CUENTAS DESDE TABLA REFERENCIA
*********  -> Francisco Rodriguez 20/02/2023

  "Modulo devuelve el nombre del SET de Datos
*  CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
*    EXPORTING
*      shortname                = c_zctas_isr
*    IMPORTING
*      new_setid                = gv_nombre_set
*    EXCEPTIONS
*      no_set_found             = 1
*      no_set_picked_from_popup = 2
*      wrong_class              = 3
*      wrong_subclass           = 4
*      table_field_not_found    = 5
*      fields_dont_match        = 6
*      set_is_empty             = 7
*      formula_in_set           = 8
*      set_is_dynamic           = 9
*      OTHERS                   = 10.

    "Consulta Set de Datos para las Cuentas
*    REFRESH it_data_set.
*    CALL FUNCTION 'G_SET_FETCH'
*      EXPORTING
*        setnr           = gv_nombre_set
*      TABLES
*        set_lines_basic = it_data_set
*      EXCEPTIONS
*        no_authority    = 1
*        set_is_broken   = 2
*        set_not_found   = 3
*        OTHERS          = 4.

    CLEAR: SV_FLAG_CATALOG.

    "Buscando Cuentas por sociedad y años fiscal para ser consultadas
    SELECT BUKRS,
           GJAHR,
           RACCT
    FROM ZFI_CUENTAS_ISR
    INTO TABLE @IT_CUENTAS
    WHERE ZCODIGO = 'CUENT'
      AND BUKRS EQ @S_RBUKRS
      AND GJAHR IN @S_RYEAR.

    IF sy-subrc EQ 0.

      REFRESH rg_data_set.

      "Rango para recuperar Cuentas
      LOOP AT IT_CUENTAS INTO WA_CUENTAS.
        rwa_data_set-sign = 'I'.
        rwa_data_set-option = 'EQ'.
        rwa_data_set-low = WA_CUENTAS-RACCT.
        APPEND rwa_data_set TO rg_data_set.
        CLEAR rwa_data_set.
      ENDLOOP.

      "Rango para Cuentas 4000000000 a 8999999999
      "se incluye un valor entre 4000000000 y 8999999999
      rwa_racct-sign    = 'I'.          " I : Incluir
      rwa_racct-option  = 'BT'.         " BT: Entre
      rwa_racct-low     = '4000000000'. " Cuenta
      rwa_racct-high    = '8999999999'. " A Cuenta
      APPEND rwa_racct TO rg_racct.

      "FMGLFLEXT Consulta para sumar los totales por mes de cada cuenta
      SELECT racct,
             ryear,
             SUM( hslvt ),
             SUM( hsl01 ),
             SUM( hsl02 ),
             SUM( hsl03 ),
             SUM( hsl04 ),
             SUM( hsl05 ),
             SUM( hsl06 ),
             SUM( hsl07 ),
             SUM( hsl08 ),
             SUM( hsl09 ),
             SUM( hsl10 ),
             SUM( hsl11 ),
             SUM( hsl12 )
        FROM fmglflext
        INTO TABLE @it_fmglflext_aux
        WHERE ryear  IN @s_ryear     AND "Ejercicio
              rldnr  EQ @s_rldnr     AND "Ledger
              racct  IN @rg_data_set AND "Número de Cuenta
              rbukrs EQ @s_rbukrs       "Sociedad
        GROUP BY racct, ryear.

      "FMGLFLEXT Dependiendo de las Cuentas y Parámetros de selección
      SELECT racct,
             ryear,
             drcrk,
*             racct,
             hslvt,
             hsl01,
             hsl02,
             hsl03,
             hsl04,
             hsl05,
             hsl06,
             hsl07,
             hsl08,
             hsl09,
             hsl10,
             hsl11,
             hsl12
        FROM fmglflext
        INTO TABLE @it_fmglflext
        WHERE ryear  IN @s_ryear     AND "Ejercicio
              rldnr  EQ @s_rldnr     AND "Ledger
              racct  IN @rg_data_set AND "Número de Cuenta
              rbukrs EQ @s_rbukrs.       "Sociedad

      IF sy-subrc EQ 0.
        SORT it_fmglflext BY racct.
        DELETE ADJACENT DUPLICATES FROM it_fmglflext COMPARING racct.
        "Consulta a SKAT para obtener Texto Explicativo
        SELECT spras,
               ktopl,
               saknr,
               txt50
          FROM skat
          INTO TABLE @it_skat
          FOR ALL ENTRIES IN @it_fmglflext
          WHERE spras EQ @c_s            AND
                ktopl EQ @c_gptx         AND
                saknr EQ @it_fmglflext-racct.

        "FMGLFLEXT Dependiendo de las Cuentas y Parámetros de selección para Debe/Haber S
        SELECT racct,
               ryear,
               drcrk,
               hslvt,
               SUM( hsl01 ),
               SUM( hsl02 ),
               SUM( hsl03 ),
               SUM( hsl04 ),
               SUM( hsl05 ),
               SUM( hsl06 ),
               SUM( hsl07 ),
               SUM( hsl08 ),
               SUM( hsl09 ),
               SUM( hsl10 ),
               SUM( hsl11 ),
               SUM( hsl12 )
          FROM fmglflext
          INTO TABLE @it_fmglflext_s
          WHERE ryear  IN @s_ryear     AND "Ejercicio
                drcrk  EQ 'S'          AND "Debe/Haber
                rldnr  EQ @s_rldnr     AND "Ledger
                racct  IN @rg_data_set AND "Número de Cuenta
                rbukrs EQ @s_rbukrs        "Sociedad
          GROUP BY racct, ryear, drcrk, hslvt.
        IF sy-subrc EQ 0.
          SORT it_fmglflext_s BY racct.
        ENDIF.

        "FMGLFLEXT Dependiendo de las Cuentas y Parámetros de selección para Debe/Haber H
        SELECT racct,
               ryear,
               drcrk,
               hslvt,
               SUM( hsl01 ),
               SUM( hsl02 ),
               SUM( hsl03 ),
               SUM( hsl04 ),
               SUM( hsl05 ),
               SUM( hsl06 ),
               SUM( hsl07 ),
               SUM( hsl08 ),
               SUM( hsl09 ),
               SUM( hsl10 ),
               SUM( hsl11 ),
               SUM( hsl12 )
          FROM fmglflext
          INTO TABLE @it_fmglflext_h
          WHERE ryear  IN @s_ryear     AND "Ejercicio
                drcrk  EQ 'H'          AND "Debe/Haber
                rldnr  EQ @s_rldnr     AND "Ledger
                racct  IN @rg_data_set AND "Número de Cuenta
                rbukrs EQ @s_rbukrs        "Sociedad
          GROUP BY racct, ryear, drcrk, hslvt.
        IF sy-subrc EQ 0.
          SORT it_fmglflext_h BY racct.
        ENDIF.

      ENDIF.
    ELSE.

      SV_FLAG_CATALOG = 'X'.

    ENDIF.

    "Obtención cuentas Anticipos V6 VARR 04/03/2021
    "Consultas a tvarvc para Anticipos
    "Cuentas anticipos
    SELECT low
      FROM tvarvc
      INTO TABLE it_tvar_cuenta_anticipos
      WHERE name EQ 'Z_CUENTAS_ANTICIPO'.
    IF sy-subrc EQ 0.
      "Pasar cuentas a rango
      LOOP AT it_tvar_cuenta_anticipos INTO wa_tvar_cuenta_anticipos.
        rwa_cuenta_anticipo-sign = 'I'.
        rwa_cuenta_anticipo-option = 'EQ'.
        rwa_cuenta_anticipo-low = wa_tvar_cuenta_anticipos-low.
        APPEND rwa_cuenta_anticipo TO rg_cuenta_anticipo.
        CLEAR rwa_cuenta_anticipo.
      ENDLOOP.
    ENDIF.

    "Clase de documento anticipos
    SELECT low
     FROM tvarvc
     INTO TABLE it_tvar_clasedoc_anticipos
     WHERE name EQ 'Z_CLASEDOC_ANTICIPO'.
    IF sy-subrc EQ 0.
      "Pasar clase Doc a rango
      LOOP AT it_tvar_clasedoc_anticipos INTO wa_tvar_clasedoc_anticipos.
        rwa_clasedoc_anticipo-sign = 'I'.
        rwa_clasedoc_anticipo-option = 'EQ'.
        rwa_clasedoc_anticipo-low = wa_tvar_clasedoc_anticipos-low.
        APPEND rwa_clasedoc_anticipo TO rg_clasedoc_anticipo.
        CLEAR rwa_clasedoc_anticipo.
      ENDLOOP.
    ENDIF.

    "Iva Anticipo
    SELECT low
      FROM tvarvc
      INTO TABLE it_iva_anticipo
      WHERE name EQ 'Z_ANTICIPO_IVA'.
      IF sy-subrc EQ 0.
        CLEAR gv_iva_anticipo.
        LOOP AT it_iva_anticipo INTO wa_iva_anticipo.
          gv_iva_anticipo = wa_iva_anticipo.
          CONDENSE gv_iva_anticipo NO-GAPS.
        ENDLOOP.
      ENDIF.

    SELECT rbukrs,
          "gjahr,
           racct,
      SUM( wsl ),
           poper
          "blart
      FROM acdoca
      INTO TABLE @it_acdoca
      WHERE rbukrs EQ @s_rbukrs           AND
            racct  IN @rg_cuenta_anticipo AND
            gjahr  IN @s_ryear            AND
            blart  IN @rg_clasedoc_anticipo
      GROUP BY rbukrs, racct, poper.
    IF sy-subrc EQ 0.
      SORT it_acdoca BY racct.
      "Asignamos montos a tabla interna global por mes y convertimos a positivo
      CLEAR  wa_acdoca_aux.
      REFRESH it_acdoca_aux.
      LOOP AT it_acdoca INTO wa_acdoca.
        "Cuenta RACCT
        wa_acdoca_aux-racct = wa_acdoca-racct.

        "Para Enero 1
        IF wa_acdoca-poper EQ '001'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_01 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_01 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        "Para Febrero 2
        IF wa_acdoca-poper EQ '002'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_02 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_02 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        "Para Marzo 3
        IF wa_acdoca-poper EQ '003'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_03 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_03 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        "Para Abril 4
        IF wa_acdoca-poper EQ '004'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_04 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_04 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        "Para Mayo 5
        IF wa_acdoca-poper EQ '005'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_05 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_05 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        "Para Junio 6
        IF wa_acdoca-poper EQ '006'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_06 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_06 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        "Para Julio 7
        IF wa_acdoca-poper EQ '007'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_07 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_07 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        "Para Agosto 8
        IF wa_acdoca-poper EQ '008'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_08 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_08 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        "Para Septiembre 9
        IF wa_acdoca-poper EQ '009'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_09 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_09 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        "Para Octubre 10
        IF wa_acdoca-poper EQ '010'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_10 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_10 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        "Para Noviembre 11
        IF wa_acdoca-poper EQ '011'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_11 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_11 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        "Para Diciembre 12
        IF wa_acdoca-poper EQ '012'.
          IF wa_acdoca-wsl LT 0.
            wa_acdoca_aux-wsl_12 = wa_acdoca-wsl * -1.
          ELSE.
            wa_acdoca_aux-wsl_12 = wa_acdoca-wsl.
          ENDIF.
        ENDIF.

        COLLECT wa_acdoca_aux INTO it_acdoca_aux.
        CLEAR  wa_acdoca_aux.
      ENDLOOP.
    ENDIF.

    "Obtención Cuentas Acreedoras TVARVC
    SELECT low
      FROM tvarvc
      INTO TABLE it_tvarvc_acreedoras
      WHERE name EQ 'Z_CTAS_ACREEDORAS'.
    IF sy-subrc EQ 0.
      SORT it_tvarvc_acreedoras BY low.
    ENDIF.

    "Obtención Cuentas Subsidio TVARVC
    SELECT low
      FROM tvarvc
      INTO TABLE it_tvarvc_subsidio
      WHERE name EQ 'Z_CTAS_SUBSIDIO'.
    IF sy-subrc EQ 0.
      SORT it_tvarvc_subsidio BY low.
    ENDIF.


  IF it_fmglflext IS NOT INITIAL.
    CLEAR wa_fmglflext.
    LOOP AT it_fmglflext INTO wa_fmglflext.

      wa_report-racct = wa_fmglflext-racct.        "Cuenta

      CLEAR wa_skat.
      READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_fmglflext-racct.

      IF sy-subrc EQ 0.
        wa_report-txt50 = wa_skat-txt50.           "Texto explicativo de la cuenta
      ENDIF.

*-----------------------Proceso para obtener Montos S y H de cada Cuenta----------------------*
      CLEAR wa_tvarvc_subsidio.
      "Si la cuenta se encuentra en la TVARVC, se muestra la su matoria de las cuentas H
      READ TABLE it_tvarvc_subsidio INTO wa_tvarvc_subsidio WITH KEY low = wa_fmglflext-racct.
      IF sy-subrc EQ 0.
        READ TABLE it_fmglflext_h INTO wa_fmglflext_h WITH KEY racct = wa_tvarvc_subsidio-low.
        IF sy-subrc EQ 0.
          CLEAR: vg_total.
          wa_report-hsl01 = wa_fmglflext_h-hsl01.
          wa_report-hsl02 = wa_fmglflext_h-hsl02.
          wa_report-hsl03 = wa_fmglflext_h-hsl03.
          wa_report-hsl04 = wa_fmglflext_h-hsl04.
          wa_report-hsl05 = wa_fmglflext_h-hsl05.
          wa_report-hsl06 = wa_fmglflext_h-hsl06.
          wa_report-hsl07 = wa_fmglflext_h-hsl07.
          wa_report-hsl08 = wa_fmglflext_h-hsl08.
          wa_report-hsl09 = wa_fmglflext_h-hsl09.
          wa_report-hsl10 = wa_fmglflext_h-hsl10.
          wa_report-hsl11 = wa_fmglflext_h-hsl11.
          wa_report-hsl12 = wa_fmglflext_h-hsl12.

          vg_total = wa_report-hsl01 + wa_report-hsl02 + wa_report-hsl03 + wa_report-hsl04 + wa_report-hsl05
                 + wa_report-hsl06   + wa_report-hsl07 + wa_report-hsl08 + wa_report-hsl09 + wa_report-hsl10 + wa_report-hsl11
                 + wa_report-hsl12.

          wa_report-total = vg_total.

          APPEND wa_report TO it_report.

          CLEAR: wa_fmglflext, vg_total, wa_report, wa_fmglflext_h, wa_tvarvc_subsidio.
        ENDIF.
      ELSE.
        CLEAR wa_tvarvc_acreedoras.

        "Si la cuenta se encuentra en la TVARVC, se respetará el signo de la sumatoria
        READ TABLE it_tvarvc_acreedoras INTO wa_tvarvc_acreedoras WITH KEY low = wa_fmglflext-racct.
        IF sy-subrc EQ 0.
          READ TABLE it_fmglflext_h INTO wa_fmglflext_h WITH KEY racct = wa_tvarvc_acreedoras-low.
          IF sy-subrc EQ 0.
            CLEAR: vg_total.
            wa_report-hsl01 = wa_fmglflext_h-hsl01.
            wa_report-hsl02 = wa_fmglflext_h-hsl02.
            wa_report-hsl03 = wa_fmglflext_h-hsl03.
            wa_report-hsl04 = wa_fmglflext_h-hsl04.
            wa_report-hsl05 = wa_fmglflext_h-hsl05.
            wa_report-hsl06 = wa_fmglflext_h-hsl06.
            wa_report-hsl07 = wa_fmglflext_h-hsl07.
            wa_report-hsl08 = wa_fmglflext_h-hsl08.
            wa_report-hsl09 = wa_fmglflext_h-hsl09.
            wa_report-hsl10 = wa_fmglflext_h-hsl10.
            wa_report-hsl11 = wa_fmglflext_h-hsl11.
            wa_report-hsl12 = wa_fmglflext_h-hsl12.

            vg_total = wa_report-hsl01 + wa_report-hsl02 + wa_report-hsl03 + wa_report-hsl04 + wa_report-hsl05
                   + wa_report-hsl06   + wa_report-hsl07 + wa_report-hsl08 + wa_report-hsl09 + wa_report-hsl10 + wa_report-hsl11
                   + wa_report-hsl12.

            wa_report-total = vg_total.

            APPEND wa_report TO it_report.

            CLEAR: wa_fmglflext, wa_report, wa_fmglflext_h, vg_total, wa_tvarvc_acreedoras.

          ELSE.
            "PROCESO NORMAL DE S Y H CUANDO NO SE ENCUENTRA EN H PREVIAMENTE

            CLEAR wa_fmglflext_s.
            READ TABLE it_fmglflext_s INTO wa_fmglflext_s WITH KEY racct = wa_fmglflext-racct.

            CLEAR wa_fmglflext_h.
            READ TABLE it_fmglflext_h INTO wa_fmglflext_h WITH KEY racct = wa_fmglflext-racct.
            IF sy-subrc EQ 0.

              " INICIO Se contemplan cuentas de Anticipo VARR 04/03/2021
              CLEAR wa_acdoca_aux.
              READ TABLE it_acdoca_aux INTO wa_acdoca_aux WITH KEY racct = wa_fmglflext-racct.
              IF sy-subrc EQ 0.
                CLEAR: vg_total.

                wa_report-hsl01 = wa_acdoca_aux-wsl_01 / gv_iva_anticipo.
                wa_report-hsl02 = wa_acdoca_aux-wsl_02 / gv_iva_anticipo.
                wa_report-hsl03 = wa_acdoca_aux-wsl_03 / gv_iva_anticipo.
                wa_report-hsl04 = wa_acdoca_aux-wsl_04 / gv_iva_anticipo.
                wa_report-hsl05 = wa_acdoca_aux-wsl_05 / gv_iva_anticipo.
                wa_report-hsl06 = wa_acdoca_aux-wsl_06 / gv_iva_anticipo.
                wa_report-hsl07 = wa_acdoca_aux-wsl_07 / gv_iva_anticipo.
                wa_report-hsl08 = wa_acdoca_aux-wsl_08 / gv_iva_anticipo.
                wa_report-hsl09 = wa_acdoca_aux-wsl_09 / gv_iva_anticipo.
                wa_report-hsl10 = wa_acdoca_aux-wsl_10 / gv_iva_anticipo.
                wa_report-hsl11 = wa_acdoca_aux-wsl_11 / gv_iva_anticipo.
                wa_report-hsl12 = wa_acdoca_aux-wsl_12 / gv_iva_anticipo.

                vg_total = wa_report-hsl01 + wa_report-hsl02 + wa_report-hsl03 + wa_report-hsl04 + wa_report-hsl05
                       + wa_report-hsl06   + wa_report-hsl07 + wa_report-hsl08 + wa_report-hsl09 + wa_report-hsl10 + wa_report-hsl11
                       + wa_report-hsl12.

                wa_report-total = vg_total.

                APPEND wa_report TO it_report.

                CLEAR: wa_fmglflext, vg_total, wa_report, wa_fmglflext_h, wa_tvarvc_subsidio.
              ELSE.

                wa_fmglflext_h-hsl01  =  wa_fmglflext_h-hsl01 * -1.
                wa_fmglflext_h-hsl02  =  wa_fmglflext_h-hsl02 * -1.
                wa_fmglflext_h-hsl03  =  wa_fmglflext_h-hsl03 * -1.
                wa_fmglflext_h-hsl04  =  wa_fmglflext_h-hsl04 * -1.
                wa_fmglflext_h-hsl05  =  wa_fmglflext_h-hsl05 * -1.
                wa_fmglflext_h-hsl06  =  wa_fmglflext_h-hsl06 * -1.
                wa_fmglflext_h-hsl07  =  wa_fmglflext_h-hsl07 * -1.
                wa_fmglflext_h-hsl08  =  wa_fmglflext_h-hsl08 * -1.
                wa_fmglflext_h-hsl09  =  wa_fmglflext_h-hsl09 * -1.
                wa_fmglflext_h-hsl10  =  wa_fmglflext_h-hsl10 * -1.
                wa_fmglflext_h-hsl11  =  wa_fmglflext_h-hsl11 * -1.
                wa_fmglflext_h-hsl12  =  wa_fmglflext_h-hsl12 * -1.

                "Cuando encuentra cuenta S y H se realiza la resta
                wa_report-hsl01 = wa_fmglflext_h-hsl01 - wa_fmglflext_s-hsl01.
                wa_report-hsl02 = wa_fmglflext_h-hsl02 - wa_fmglflext_s-hsl02.
                wa_report-hsl03 = wa_fmglflext_h-hsl03 - wa_fmglflext_s-hsl03.
                wa_report-hsl04 = wa_fmglflext_h-hsl04 - wa_fmglflext_s-hsl04.
                wa_report-hsl05 = wa_fmglflext_h-hsl05 - wa_fmglflext_s-hsl05.
                wa_report-hsl06 = wa_fmglflext_h-hsl06 - wa_fmglflext_s-hsl06.
                wa_report-hsl07 = wa_fmglflext_h-hsl07 - wa_fmglflext_s-hsl07.
                wa_report-hsl08 = wa_fmglflext_h-hsl08 - wa_fmglflext_s-hsl08.
                wa_report-hsl09 = wa_fmglflext_h-hsl09 - wa_fmglflext_s-hsl09.
                wa_report-hsl10 = wa_fmglflext_h-hsl10 - wa_fmglflext_s-hsl10.
                wa_report-hsl11 = wa_fmglflext_h-hsl11 - wa_fmglflext_s-hsl11.
                wa_report-hsl12 = wa_fmglflext_h-hsl12 - wa_fmglflext_s-hsl12.

                vg_total = wa_report-hsl01 + wa_report-hsl02 + wa_report-hsl03 + wa_report-hsl04 + wa_report-hsl05
                         + wa_report-hsl06 + wa_report-hsl07 + wa_report-hsl08 + wa_report-hsl09 + wa_report-hsl10 + wa_report-hsl11
                         + wa_report-hsl12.

                wa_report-total = vg_total.

                APPEND wa_report TO it_report.
                CLEAR: wa_fmglflext, wa_report, wa_fmglflext_h, wa_fmglflext_s, vg_total.
              ENDIF.

            ELSE. "Cuando no encuentra H se pasa sólo el total de la cuenta S

              "INICIO Se contemplan cuentas de Anticipo VARR 04/03/2021
              CLEAR wa_acdoca_aux.
              READ TABLE it_acdoca_aux INTO wa_acdoca_aux WITH KEY racct = wa_fmglflext-racct.
              IF sy-subrc EQ 0.
                CLEAR: vg_total.
                wa_report-hsl01 = wa_acdoca_aux-wsl_01 / gv_iva_anticipo.
                wa_report-hsl02 = wa_acdoca_aux-wsl_02 / gv_iva_anticipo.
                wa_report-hsl03 = wa_acdoca_aux-wsl_03 / gv_iva_anticipo.
                wa_report-hsl04 = wa_acdoca_aux-wsl_04 / gv_iva_anticipo.
                wa_report-hsl05 = wa_acdoca_aux-wsl_05 / gv_iva_anticipo.
                wa_report-hsl06 = wa_acdoca_aux-wsl_06 / gv_iva_anticipo.
                wa_report-hsl07 = wa_acdoca_aux-wsl_07 / gv_iva_anticipo.
                wa_report-hsl08 = wa_acdoca_aux-wsl_08 / gv_iva_anticipo.
                wa_report-hsl09 = wa_acdoca_aux-wsl_09 / gv_iva_anticipo.
                wa_report-hsl10 = wa_acdoca_aux-wsl_10 / gv_iva_anticipo.
                wa_report-hsl11 = wa_acdoca_aux-wsl_11 / gv_iva_anticipo.
                wa_report-hsl12 = wa_acdoca_aux-wsl_12 / gv_iva_anticipo.

                vg_total = wa_report-hsl01 + wa_report-hsl02 + wa_report-hsl03 + wa_report-hsl04 + wa_report-hsl05
                       + wa_report-hsl06   + wa_report-hsl07 + wa_report-hsl08 + wa_report-hsl09 + wa_report-hsl10 + wa_report-hsl11
                       + wa_report-hsl12.

                wa_report-total = vg_total.

                APPEND wa_report TO it_report.

                CLEAR: wa_fmglflext, vg_total, wa_report, wa_fmglflext_h, wa_tvarvc_subsidio.

              ELSE.

                wa_report-hsl01 = wa_fmglflext_s-hsl01.
                wa_report-hsl02 = wa_fmglflext_s-hsl02.
                wa_report-hsl03 = wa_fmglflext_s-hsl03.
                wa_report-hsl04 = wa_fmglflext_s-hsl04.
                wa_report-hsl05 = wa_fmglflext_s-hsl05.
                wa_report-hsl06 = wa_fmglflext_s-hsl06.
                wa_report-hsl07 = wa_fmglflext_s-hsl07.
                wa_report-hsl08 = wa_fmglflext_s-hsl08.
                wa_report-hsl09 = wa_fmglflext_s-hsl09.
                wa_report-hsl10 = wa_fmglflext_s-hsl10.
                wa_report-hsl11 = wa_fmglflext_s-hsl11.
                wa_report-hsl12 = wa_fmglflext_s-hsl12.

                vg_total = wa_report-hsl01 + wa_report-hsl02 + wa_report-hsl03 + wa_report-hsl04 + wa_report-hsl05
                           + wa_report-hsl06 + wa_report-hsl07 + wa_report-hsl08 + wa_report-hsl09 + wa_report-hsl10 + wa_report-hsl11
                           + wa_report-hsl12.

                wa_report-total = vg_total.

                APPEND wa_report TO it_report.
                CLEAR: wa_fmglflext, wa_report, wa_fmglflext_s, vg_total.
              ENDIF.

            ENDIF.
          ENDIF.

        ELSE. "PROCESO NORMAL DE S Y H CUANDO NO SE ENCUENTRA EN ACREEDORAS

          CLEAR wa_fmglflext_s.
          READ TABLE it_fmglflext_s INTO wa_fmglflext_s WITH KEY racct = wa_fmglflext-racct.

          CLEAR wa_fmglflext_h.
          READ TABLE it_fmglflext_h INTO wa_fmglflext_h WITH KEY racct = wa_fmglflext-racct.
          IF sy-subrc EQ 0.

            " INICIO Se contemplan cuentas de Anticipo VARR 04/03/2021
            CLEAR wa_acdoca_aux.
            READ TABLE it_acdoca_aux INTO wa_acdoca_aux WITH KEY racct = wa_fmglflext-racct.
            IF sy-subrc EQ 0.
              CLEAR: vg_total.
              wa_report-hsl01 = wa_acdoca_aux-wsl_01 / gv_iva_anticipo.
              wa_report-hsl02 = wa_acdoca_aux-wsl_02 / gv_iva_anticipo.
              wa_report-hsl03 = wa_acdoca_aux-wsl_03 / gv_iva_anticipo.
              wa_report-hsl04 = wa_acdoca_aux-wsl_04 / gv_iva_anticipo.
              wa_report-hsl05 = wa_acdoca_aux-wsl_05 / gv_iva_anticipo.
              wa_report-hsl06 = wa_acdoca_aux-wsl_06 / gv_iva_anticipo.
              wa_report-hsl07 = wa_acdoca_aux-wsl_07 / gv_iva_anticipo.
              wa_report-hsl08 = wa_acdoca_aux-wsl_08 / gv_iva_anticipo.
              wa_report-hsl09 = wa_acdoca_aux-wsl_09 / gv_iva_anticipo.
              wa_report-hsl10 = wa_acdoca_aux-wsl_10 / gv_iva_anticipo.
              wa_report-hsl11 = wa_acdoca_aux-wsl_11 / gv_iva_anticipo.
              wa_report-hsl12 = wa_acdoca_aux-wsl_12 / gv_iva_anticipo.

              vg_total = wa_report-hsl01 + wa_report-hsl02 + wa_report-hsl03 + wa_report-hsl04 + wa_report-hsl05
                     + wa_report-hsl06   + wa_report-hsl07 + wa_report-hsl08 + wa_report-hsl09 + wa_report-hsl10 + wa_report-hsl11
                     + wa_report-hsl12.

              wa_report-total = vg_total.

              APPEND wa_report TO it_report.

              CLEAR: wa_fmglflext, vg_total, wa_report, wa_fmglflext_h, wa_tvarvc_subsidio.
            ELSE.

              wa_fmglflext_h-hsl01  =  wa_fmglflext_h-hsl01 * -1.
              wa_fmglflext_h-hsl02  =  wa_fmglflext_h-hsl02 * -1.
              wa_fmglflext_h-hsl03  =  wa_fmglflext_h-hsl03 * -1.
              wa_fmglflext_h-hsl04  =  wa_fmglflext_h-hsl04 * -1.
              wa_fmglflext_h-hsl05  =  wa_fmglflext_h-hsl05 * -1.
              wa_fmglflext_h-hsl06  =  wa_fmglflext_h-hsl06 * -1.
              wa_fmglflext_h-hsl07  =  wa_fmglflext_h-hsl07 * -1.
              wa_fmglflext_h-hsl08  =  wa_fmglflext_h-hsl08 * -1.
              wa_fmglflext_h-hsl09  =  wa_fmglflext_h-hsl09 * -1.
              wa_fmglflext_h-hsl10  =  wa_fmglflext_h-hsl10 * -1.
              wa_fmglflext_h-hsl11  =  wa_fmglflext_h-hsl11 * -1.
              wa_fmglflext_h-hsl12  =  wa_fmglflext_h-hsl12 * -1.

              "Cuando encuentra cuenta S y H se realiza la resta
              wa_report-hsl01 = wa_fmglflext_h-hsl01 - wa_fmglflext_s-hsl01.
              wa_report-hsl02 = wa_fmglflext_h-hsl02 - wa_fmglflext_s-hsl02.
              wa_report-hsl03 = wa_fmglflext_h-hsl03 - wa_fmglflext_s-hsl03.
              wa_report-hsl04 = wa_fmglflext_h-hsl04 - wa_fmglflext_s-hsl04.
              wa_report-hsl05 = wa_fmglflext_h-hsl05 - wa_fmglflext_s-hsl05.
              wa_report-hsl06 = wa_fmglflext_h-hsl06 - wa_fmglflext_s-hsl06.
              wa_report-hsl07 = wa_fmglflext_h-hsl07 - wa_fmglflext_s-hsl07.
              wa_report-hsl08 = wa_fmglflext_h-hsl08 - wa_fmglflext_s-hsl08.
              wa_report-hsl09 = wa_fmglflext_h-hsl09 - wa_fmglflext_s-hsl09.
              wa_report-hsl10 = wa_fmglflext_h-hsl10 - wa_fmglflext_s-hsl10.
              wa_report-hsl11 = wa_fmglflext_h-hsl11 - wa_fmglflext_s-hsl11.
              wa_report-hsl12 = wa_fmglflext_h-hsl12 - wa_fmglflext_s-hsl12.

              vg_total = wa_report-hsl01 + wa_report-hsl02 + wa_report-hsl03 + wa_report-hsl04 + wa_report-hsl05
                       + wa_report-hsl06 + wa_report-hsl07 + wa_report-hsl08 + wa_report-hsl09 + wa_report-hsl10 + wa_report-hsl11
                       + wa_report-hsl12.

              wa_report-total = vg_total.

              APPEND wa_report TO it_report.
              CLEAR: wa_fmglflext, wa_report, wa_fmglflext_h, wa_fmglflext_s, vg_total.
            ENDIF.

          ELSE. "Cuando no encuentra H se pasa sólo el total de la cuenta S Pero primero se valida en las cuentas de Anticipos

            " INICIO Se contemplan cuentas de Anticipo VARR 04/03/2021
            CLEAR wa_acdoca_aux.
            READ TABLE it_acdoca_aux INTO wa_acdoca_aux WITH KEY racct = wa_fmglflext-racct.
            IF sy-subrc EQ 0.
              CLEAR: vg_total.
              wa_report-hsl01 = wa_acdoca_aux-wsl_01 / gv_iva_anticipo.
              wa_report-hsl02 = wa_acdoca_aux-wsl_02 / gv_iva_anticipo.
              wa_report-hsl03 = wa_acdoca_aux-wsl_03 / gv_iva_anticipo.
              wa_report-hsl04 = wa_acdoca_aux-wsl_04 / gv_iva_anticipo.
              wa_report-hsl05 = wa_acdoca_aux-wsl_05 / gv_iva_anticipo.
              wa_report-hsl06 = wa_acdoca_aux-wsl_06 / gv_iva_anticipo.
              wa_report-hsl07 = wa_acdoca_aux-wsl_07 / gv_iva_anticipo.
              wa_report-hsl08 = wa_acdoca_aux-wsl_08 / gv_iva_anticipo.
              wa_report-hsl09 = wa_acdoca_aux-wsl_09 / gv_iva_anticipo.
              wa_report-hsl10 = wa_acdoca_aux-wsl_10 / gv_iva_anticipo.
              wa_report-hsl11 = wa_acdoca_aux-wsl_11 / gv_iva_anticipo.
              wa_report-hsl12 = wa_acdoca_aux-wsl_12 / gv_iva_anticipo.

              vg_total = wa_report-hsl01 + wa_report-hsl02 + wa_report-hsl03 + wa_report-hsl04 + wa_report-hsl05
                     + wa_report-hsl06   + wa_report-hsl07 + wa_report-hsl08 + wa_report-hsl09 + wa_report-hsl10 + wa_report-hsl11
                     + wa_report-hsl12.

              wa_report-total = vg_total.

              APPEND wa_report TO it_report.

              CLEAR: wa_fmglflext, vg_total, wa_report, wa_fmglflext_h, wa_tvarvc_subsidio.
            ELSE.
              wa_report-hsl01 = wa_fmglflext_s-hsl01.
              wa_report-hsl02 = wa_fmglflext_s-hsl02.
              wa_report-hsl03 = wa_fmglflext_s-hsl03.
              wa_report-hsl04 = wa_fmglflext_s-hsl04.
              wa_report-hsl05 = wa_fmglflext_s-hsl05.
              wa_report-hsl06 = wa_fmglflext_s-hsl06.
              wa_report-hsl07 = wa_fmglflext_s-hsl07.
              wa_report-hsl08 = wa_fmglflext_s-hsl08.
              wa_report-hsl09 = wa_fmglflext_s-hsl09.
              wa_report-hsl10 = wa_fmglflext_s-hsl10.
              wa_report-hsl11 = wa_fmglflext_s-hsl11.
              wa_report-hsl12 = wa_fmglflext_s-hsl12.

              vg_total = wa_report-hsl01 + wa_report-hsl02 + wa_report-hsl03 + wa_report-hsl04 + wa_report-hsl05
                         + wa_report-hsl06 + wa_report-hsl07 + wa_report-hsl08 + wa_report-hsl09 + wa_report-hsl10 + wa_report-hsl11
                         + wa_report-hsl12.

              wa_report-total = vg_total.

              APPEND wa_report TO it_report.
              CLEAR: wa_fmglflext, wa_report, wa_fmglflext_s, vg_total.
            ENDIF.
            "FIN Se contemplan cuentas de Anticipo VARR 04/03/2021
            "Anteriormente se tenía el proceso de ELSE que se acaba de hacer aquí:
*            wa_report-hsl01 = wa_fmglflext_s-hsl01.
*            wa_report-hsl02 = wa_fmglflext_s-hsl02.
*            wa_report-hsl03 = wa_fmglflext_s-hsl03.
*            wa_report-hsl04 = wa_fmglflext_s-hsl04.
*            wa_report-hsl05 = wa_fmglflext_s-hsl05.
*            wa_report-hsl06 = wa_fmglflext_s-hsl06.
*            wa_report-hsl07 = wa_fmglflext_s-hsl07.
*            wa_report-hsl08 = wa_fmglflext_s-hsl08.
*            wa_report-hsl09 = wa_fmglflext_s-hsl09.
*            wa_report-hsl10 = wa_fmglflext_s-hsl10.
*            wa_report-hsl11 = wa_fmglflext_s-hsl11.
*            wa_report-hsl12 = wa_fmglflext_s-hsl12.
*
*            vg_total = wa_report-hsl01 + wa_report-hsl02 + wa_report-hsl03 + wa_report-hsl04 + wa_report-hsl05
*                       + wa_report-hsl06 + wa_report-hsl07 + wa_report-hsl08 + wa_report-hsl09 + wa_report-hsl10 + wa_report-hsl11
*                       + wa_report-hsl12.
*
*            wa_report-total = vg_total.
*
*            APPEND wa_report TO it_report.
*            CLEAR: wa_fmglflext, wa_report, wa_fmglflext_s, vg_total.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    it_report_aux[] = it_report[].
    it_report_anticipo[] = it_report[].

    "DELETE it_report WHERE racct NOT IN rg_racct. "VARR Se comentó y se pudo abajo del loop

    LOOP AT it_report INTO wa_report WHERE racct IN rg_racct
                                        OR racct IN rg_cuenta_anticipo. " VARR 04/03/2021

      "Se realiza la suma de cada posicion de INGRESOS NOMINALES MENSUALES
      vg_tot1      = vg_tot1      + wa_report-hsl01.
      vg_tot2      = vg_tot2      + wa_report-hsl02.
      vg_tot3      = vg_tot3      + wa_report-hsl03.
      vg_tot4      = vg_tot4      + wa_report-hsl04.
      vg_tot5      = vg_tot5      + wa_report-hsl05.
      vg_tot6      = vg_tot6      + wa_report-hsl06.
      vg_tot7      = vg_tot7      + wa_report-hsl07.
      vg_tot8      = vg_tot8      + wa_report-hsl08.
      vg_tot9      = vg_tot9      + wa_report-hsl09.
      vg_tot10     = vg_tot10     + wa_report-hsl10.
      vg_tot11     = vg_tot11     + wa_report-hsl11.
      vg_tot12     = vg_tot12     + wa_report-hsl12.
    ENDLOOP.

    vg_total_var = vg_tot1 + vg_tot2 + vg_tot3 + vg_tot4 + vg_tot5 + vg_tot6 + vg_tot7 + vg_tot8 + vg_tot9 +
               vg_tot10 + vg_tot11 + vg_tot12.

    DELETE it_report WHERE racct NOT IN rg_racct.

    CLEAR: wa_report_cal.
    "INGRESOS NOMINALES MENSUALES
    wa_report_cal-txt50 = c_ingresos_nominales_mensuales.
    wa_report_cal-hsl01 = vg_tot1.
    wa_report_cal-hsl02 = vg_tot2.
    wa_report_cal-hsl03 = vg_tot3.
    wa_report_cal-hsl04 = vg_tot4.
    wa_report_cal-hsl05 = vg_tot5.
    wa_report_cal-hsl06 = vg_tot6.
    wa_report_cal-hsl07 = vg_tot7.
    wa_report_cal-hsl08 = vg_tot8.
    wa_report_cal-hsl09 = vg_tot9.
    wa_report_cal-hsl10 = vg_tot10.
    wa_report_cal-hsl11 = vg_tot11.
    wa_report_cal-hsl12 = vg_tot12.
    wa_report_cal-total = vg_total_var.

    APPEND wa_report_cal TO it_report_cal.

    "ING NOMINALES ACUMULADOS
    CLEAR: wa_report_cal.
    wa_report_cal-txt50 = c_ing_nominales_acumulados.
    wa_report_cal-hsl01 = vg_tot1.
    wa_report_cal-hsl02 = vg_tot1 + vg_tot2.
    wa_report_cal-hsl03 = wa_report_cal-hsl02 + vg_tot3.
    wa_report_cal-hsl04 = wa_report_cal-hsl03 + vg_tot4.
    wa_report_cal-hsl05 = wa_report_cal-hsl04 + vg_tot5.
    wa_report_cal-hsl06 = wa_report_cal-hsl05 + vg_tot6.
    wa_report_cal-hsl07 = wa_report_cal-hsl06 + vg_tot7.
    wa_report_cal-hsl08 = wa_report_cal-hsl07 + vg_tot8.
    wa_report_cal-hsl09 = wa_report_cal-hsl08 + vg_tot9.
    wa_report_cal-hsl10 = wa_report_cal-hsl09 + vg_tot10.
    wa_report_cal-hsl11 = wa_report_cal-hsl10 + vg_tot11.
    wa_report_cal-hsl12 = wa_report_cal-hsl11 + vg_tot12.

    APPEND wa_report_cal TO it_report_cal.

    "COEFICIENTE DE UTILIDAD
    DATA V_CURRENT_COEFICIENTE TYPE C LENGTH 26.

    SELECT GJAHR,
           BUKRS,
           MONAT,
           ZCOEFICIENTE_UTILIDAD
      FROM ZFI_CUENTAS_ISR
      INTO TABLE @IT_COFUT
      WHERE ZCODIGO EQ 'COFUT'   AND
            GJAHR   IN @S_RYEAR  AND "EJERCICIO
            BUKRS   EQ @S_RBUKRS.    "SOCIEDAD

      IF SY-SUBRC EQ 0.

        CLEAR: WA_REPORT_CAL_DEC,
               V_CURRENT_COEFICIENTE.

        WA_REPORT_CAL_DEC-TXT50 = C_COEFICIENTE_DE_UTILIDAD.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_01
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.

        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL01 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL01 = V_CURRENT_COEFICIENTE.

        ENDIF.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_02
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.

        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL02 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL02 = V_CURRENT_COEFICIENTE.

        ENDIF.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_03
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.

        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL03 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL03 = V_CURRENT_COEFICIENTE.

        ENDIF.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_04
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.

        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL04 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL04 = V_CURRENT_COEFICIENTE.

        ENDIF.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_05
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.

        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL05 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL05 = V_CURRENT_COEFICIENTE.

        ENDIF.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_06
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.

        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL06 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL06 = V_CURRENT_COEFICIENTE.

        ENDIF.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_07
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.

        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL07 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL07 = V_CURRENT_COEFICIENTE.

        ENDIF.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_08
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.

        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL08 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL08 = V_CURRENT_COEFICIENTE.

        ENDIF.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_09
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.

        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL09 = V_CURRENT_COEFICIENTE.

        ENDIF.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_10
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.
        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL10 = V_CURRENT_COEFICIENTE.

        ENDIF.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_11
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.

        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL11 = V_CURRENT_COEFICIENTE.

        ENDIF.

        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_12
                                                   BUKRS = S_RBUKRS
                                                   GJAHR = S_RYEAR-LOW.

        IF SY-SUBRC EQ 0.

          CLEAR V_CURRENT_COEFICIENTE.
          WA_REPORT_CAL_DEC-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
          V_CURRENT_COEFICIENTE = WA_COFUT-ZCOEFICIENTE_UTILIDAD.

        ELSE.

          WA_REPORT_CAL_DEC-HSL12 = V_CURRENT_COEFICIENTE.

        ENDIF.

*        WA_REPORT_CAL_DEC-TOTAL = WA_REPORT_CAL_DEC-HSL01 +
*                                  WA_REPORT_CAL_DEC-HSL02 +
*                                  WA_REPORT_CAL_DEC-HSL03 +
*                                  WA_REPORT_CAL_DEC-HSL04 +
*                                  WA_REPORT_CAL_DEC-HSL05 +
*                                  WA_REPORT_CAL_DEC-HSL06 +
*                                  WA_REPORT_CAL_DEC-HSL07 +
*                                  WA_REPORT_CAL_DEC-HSL08 +
*                                  WA_REPORT_CAL_DEC-HSL09 +
*                                  WA_REPORT_CAL_DEC-HSL10 +
*                                  WA_REPORT_CAL_DEC-HSL11 +
*                                  WA_REPORT_CAL_DEC-HSL12.

      ENDIF.

      APPEND WA_REPORT_CAL_DEC TO IT_REPORT_CAL_DEC.

*    SELECT low,
*           high
*      FROM tvarvc
*      INTO TABLE @it_tvarvc
*      WHERE name EQ @c_z_coeficiente_util.
*
*    IF sy-subrc EQ 0.
*      DELETE it_tvarvc WHERE low+0(4) NE s_rbukrs.
*      CLEAR: wa_report_cal_dec.
*      wa_report_cal_dec-txt50 = c_coeficiente_de_utilidad.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_01
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl01 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_02
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl02 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_03
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl03 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_04
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl04 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_05
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl05 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_06
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl06 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_07
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl07 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_08
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl08 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_09
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl09 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_10
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl10 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_11
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl11 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_12
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal_dec-hsl12 = wa_tvarvc-high.
*      ENDIF.
*
*      APPEND wa_report_cal_dec TO it_report_cal_dec.
*
*    ENDIF.

    "UTILIDAD FISCAL
    READ TABLE it_report_cal INTO wa_report_calc WITH KEY txt50 = c_ing_nominales_acumulados.
    READ TABLE it_report_cal_dec INTO wa_report_cal_dec WITH KEY txt50 = c_coeficiente_de_utilidad.

    wa_report_cal-txt50 = c_utilidad_fiscal.
    wa_report_cal-hsl01 = wa_report_calc-hsl01 * wa_report_cal_dec-hsl01.
    wa_report_cal-hsl02 = wa_report_calc-hsl02 * wa_report_cal_dec-hsl02.
    wa_report_cal-hsl03 = wa_report_calc-hsl03 * wa_report_cal_dec-hsl03.
    wa_report_cal-hsl04 = wa_report_calc-hsl04 * wa_report_cal_dec-hsl04.
    wa_report_cal-hsl05 = wa_report_calc-hsl05 * wa_report_cal_dec-hsl05.
    wa_report_cal-hsl06 = wa_report_calc-hsl06 * wa_report_cal_dec-hsl06.
    wa_report_cal-hsl07 = wa_report_calc-hsl07 * wa_report_cal_dec-hsl07.
    wa_report_cal-hsl08 = wa_report_calc-hsl08 * wa_report_cal_dec-hsl08.
    wa_report_cal-hsl09 = wa_report_calc-hsl09 * wa_report_cal_dec-hsl09.
    wa_report_cal-hsl10 = wa_report_calc-hsl10 * wa_report_cal_dec-hsl10.
    wa_report_cal-hsl11 = wa_report_calc-hsl11 * wa_report_cal_dec-hsl11.
    wa_report_cal-hsl12 = wa_report_calc-hsl12 * wa_report_cal_dec-hsl12.

    APPEND wa_report_cal TO it_report_cal.

    "PTU PAGADA

    DATA INDEX TYPE I.

    REFRESH IT_COFUT.

    SELECT GJAHR,
           BUKRS,
           MONAT,
           ZCOEFICIENTE_UTILIDAD
      FROM ZFI_CUENTAS_ISR
      INTO TABLE @IT_COFUT
      WHERE ZCODIGO EQ 'PTUPA'   AND
            GJAHR   IN @S_RYEAR  AND "EJERCICIO
            BUKRS   EQ @S_RBUKRS.    "SOCIEDAD

      INDEX = LINES( IT_COFUT ).

      LOOP AT IT_COFUT INTO WA_COFUT WHERE BUKRS = S_RBUKRS AND
                                           GJAHR = S_RYEAR-LOW.

        CASE WA_COFUT-MONAT.
          WHEN C_01.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - ENERO'.
            WA_REPORT_CAL-HSL01 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
            WA_REPORT_CAL-HSL02 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
            WA_REPORT_CAL-HSL03 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
            WA_REPORT_CAL-HSL04 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
            WA_REPORT_CAL-HSL05 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
            WA_REPORT_CAL-HSL06 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
            WA_REPORT_CAL-HSL07 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
            WA_REPORT_CAL-HSL08 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
            WA_REPORT_CAL-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
            WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
            WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 12.
          WHEN C_02.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - FEBRERO'.
            WA_REPORT_CAL-HSL01 = 0.
            WA_REPORT_CAL-HSL02 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 11.
            WA_REPORT_CAL-HSL03 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 11.
            WA_REPORT_CAL-HSL04 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 11.
            WA_REPORT_CAL-HSL05 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 11.
            WA_REPORT_CAL-HSL06 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 11.
            WA_REPORT_CAL-HSL07 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 11.
            WA_REPORT_CAL-HSL08 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 11.
            WA_REPORT_CAL-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 11.
            WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 11.
            WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 11.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 11.
          WHEN C_03.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - MARZO'.
            WA_REPORT_CAL-HSL01 = 0.
            WA_REPORT_CAL-HSL02 = 0.
            WA_REPORT_CAL-HSL03 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 10.
            WA_REPORT_CAL-HSL04 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 10.
            WA_REPORT_CAL-HSL05 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 10.
            WA_REPORT_CAL-HSL06 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 10.
            WA_REPORT_CAL-HSL07 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 10.
            WA_REPORT_CAL-HSL08 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 10.
            WA_REPORT_CAL-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 10.
            WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 10.
            WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 10.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 10.
          WHEN C_04.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - ABRIL'.
            WA_REPORT_CAL-HSL01 = 0.
            WA_REPORT_CAL-HSL02 = 0.
            WA_REPORT_CAL-HSL03 = 0.
            WA_REPORT_CAL-HSL04 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 9.
            WA_REPORT_CAL-HSL05 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 9.
            WA_REPORT_CAL-HSL06 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 9.
            WA_REPORT_CAL-HSL07 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 9.
            WA_REPORT_CAL-HSL08 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 9.
            WA_REPORT_CAL-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 9.
            WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 9.
            WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 9.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 9.
          WHEN C_05.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - MAYO'.
            WA_REPORT_CAL-HSL01 = 0.
            WA_REPORT_CAL-HSL02 = 0.
            WA_REPORT_CAL-HSL03 = 0.
            WA_REPORT_CAL-HSL04 = 0.
            WA_REPORT_CAL-HSL05 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 8.
            WA_REPORT_CAL-HSL06 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 8.
            WA_REPORT_CAL-HSL07 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 8.
            WA_REPORT_CAL-HSL08 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 8.
            WA_REPORT_CAL-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 8.
            WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 8.
            WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 8.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 8.
          WHEN C_06.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - JUNIO'.
            WA_REPORT_CAL-HSL01 = 0.
            WA_REPORT_CAL-HSL02 = 0.
            WA_REPORT_CAL-HSL03 = 0.
            WA_REPORT_CAL-HSL04 = 0.
            WA_REPORT_CAL-HSL05 = 0.
            WA_REPORT_CAL-HSL06 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 7.
            WA_REPORT_CAL-HSL07 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 7.
            WA_REPORT_CAL-HSL08 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 7.
            WA_REPORT_CAL-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 7.
            WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 7.
            WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 7.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 7.
          WHEN C_07.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - JULIO'.
            WA_REPORT_CAL-HSL01 = 0.
            WA_REPORT_CAL-HSL02 = 0.
            WA_REPORT_CAL-HSL03 = 0.
            WA_REPORT_CAL-HSL04 = 0.
            WA_REPORT_CAL-HSL05 = 0.
            WA_REPORT_CAL-HSL06 = 0.
            WA_REPORT_CAL-HSL07 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 6.
            WA_REPORT_CAL-HSL08 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 6.
            WA_REPORT_CAL-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 6.
            WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 6.
            WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 6.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 6.
          WHEN C_08.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - AGOSTO'.
            WA_REPORT_CAL-HSL01 = 0.
            WA_REPORT_CAL-HSL02 = 0.
            WA_REPORT_CAL-HSL03 = 0.
            WA_REPORT_CAL-HSL04 = 0.
            WA_REPORT_CAL-HSL05 = 0.
            WA_REPORT_CAL-HSL06 = 0.
            WA_REPORT_CAL-HSL07 = 0.
            WA_REPORT_CAL-HSL08 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 5.
            WA_REPORT_CAL-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 5.
            WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 5.
            WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 5.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 5.
          WHEN C_09.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - SEPTIEMBRE'.
            WA_REPORT_CAL-HSL01 = 0.
            WA_REPORT_CAL-HSL02 = 0.
            WA_REPORT_CAL-HSL03 = 0.
            WA_REPORT_CAL-HSL04 = 0.
            WA_REPORT_CAL-HSL05 = 0.
            WA_REPORT_CAL-HSL06 = 0.
            WA_REPORT_CAL-HSL07 = 0.
            WA_REPORT_CAL-HSL08 = 0.
            WA_REPORT_CAL-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 4.
            WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 4.
            WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 4.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 4.
          WHEN C_10.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - OCTUBRE'.
            WA_REPORT_CAL-HSL01 = 0.
            WA_REPORT_CAL-HSL02 = 0.
            WA_REPORT_CAL-HSL03 = 0.
            WA_REPORT_CAL-HSL04 = 0.
            WA_REPORT_CAL-HSL05 = 0.
            WA_REPORT_CAL-HSL06 = 0.
            WA_REPORT_CAL-HSL07 = 0.
            WA_REPORT_CAL-HSL08 = 0.
            WA_REPORT_CAL-HSL09 = 0.
            WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 3.
            WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 3.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 3.
          WHEN C_11.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - NOVIEMBRE'.
            WA_REPORT_CAL-HSL01 = 0.
            WA_REPORT_CAL-HSL02 = 0.
            WA_REPORT_CAL-HSL03 = 0.
            WA_REPORT_CAL-HSL04 = 0.
            WA_REPORT_CAL-HSL05 = 0.
            WA_REPORT_CAL-HSL06 = 0.
            WA_REPORT_CAL-HSL07 = 0.
            WA_REPORT_CAL-HSL08 = 0.
            WA_REPORT_CAL-HSL09 = 0.
            WA_REPORT_CAL-HSL10 = 0.
            WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 2.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 2.
          WHEN C_12.
             WA_REPORT_CAL-TXT50 = 'PTU PAGADA - DICIEMBRE'.
            WA_REPORT_CAL-HSL01 = 0.
            WA_REPORT_CAL-HSL02 = 0.
            WA_REPORT_CAL-HSL03 = 0.
            WA_REPORT_CAL-HSL04 = 0.
            WA_REPORT_CAL-HSL05 = 0.
            WA_REPORT_CAL-HSL06 = 0.
            WA_REPORT_CAL-HSL07 = 0.
            WA_REPORT_CAL-HSL08 = 0.
            WA_REPORT_CAL-HSL09 = 0.
            WA_REPORT_CAL-HSL10 = 0.
            WA_REPORT_CAL-HSL11 = 0.
            WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD / 1.
        ENDCASE.

        WA_REPORT_CAL-TOTAL = WA_REPORT_CAL-HSL01 +
                              WA_REPORT_CAL-HSL02 +
                              WA_REPORT_CAL-HSL03 +
                              WA_REPORT_CAL-HSL04 +
                              WA_REPORT_CAL-HSL05 +
                              WA_REPORT_CAL-HSL06 +
                              WA_REPORT_CAL-HSL07 +
                              WA_REPORT_CAL-HSL08 +
                              WA_REPORT_CAL-HSL09 +
                              WA_REPORT_CAL-HSL10 +
                              WA_REPORT_CAL-HSL11 +
                              WA_REPORT_CAL-HSL12.

        APPEND WA_REPORT_CAL TO IT_REPORT_CAL.

      ENDLOOP.

      DATA: R_TXT50 TYPE RANGE OF string,
            W_TXT50 LIKE LINE OF R_TXT50.

      W_TXT50-low = 'PTU PAGADA -*'.
      W_TXT50-sign = 'I'.
      W_TXT50-option = 'CP'.
      append W_TXT50 to R_TXT50.

      LOOP AT IT_REPORT_CAL INTO WA_REPORT_CAL WHERE TXT50 IN R_TXT50.

        wa_report_cal_aux-HSL01 = wa_report_cal_aux-HSL01 + WA_REPORT_CAL-HSL01.
        wa_report_cal_aux-HSL02 = wa_report_cal_aux-HSL02 + WA_REPORT_CAL-HSL02.
        wa_report_cal_aux-HSL03 = wa_report_cal_aux-HSL03 + WA_REPORT_CAL-HSL03.
        wa_report_cal_aux-HSL04 = wa_report_cal_aux-HSL04 + WA_REPORT_CAL-HSL04.
        wa_report_cal_aux-HSL05 = wa_report_cal_aux-HSL05 + WA_REPORT_CAL-HSL05.
        wa_report_cal_aux-HSL06 = wa_report_cal_aux-HSL06 + WA_REPORT_CAL-HSL06.
        wa_report_cal_aux-HSL07 = wa_report_cal_aux-HSL07 + WA_REPORT_CAL-HSL07.
        wa_report_cal_aux-HSL08 = wa_report_cal_aux-HSL08 + WA_REPORT_CAL-HSL08.
        wa_report_cal_aux-HSL09 = wa_report_cal_aux-HSL09 + WA_REPORT_CAL-HSL09.
        wa_report_cal_aux-HSL10 = wa_report_cal_aux-HSL10 + WA_REPORT_CAL-HSL10.
        wa_report_cal_aux-HSL11 = wa_report_cal_aux-HSL11 + WA_REPORT_CAL-HSL11.
        wa_report_cal_aux-HSL12 = wa_report_cal_aux-HSL12 + WA_REPORT_CAL-HSL12.

      ENDLOOP.

      wa_report_cal_aux-TXT50 = 'PTU PAGADA - IMPORTE PAGADO'.

      wa_report_cal_aux-TOTAL = wa_report_cal_aux-HSL01 +
                                wa_report_cal_aux-HSL02 +
                                wa_report_cal_aux-HSL03 +
                                wa_report_cal_aux-HSL04 +
                                wa_report_cal_aux-HSL05 +
                                wa_report_cal_aux-HSL06 +
                                wa_report_cal_aux-HSL07 +
                                wa_report_cal_aux-HSL08 +
                                wa_report_cal_aux-HSL09 +
                                wa_report_cal_aux-HSL10 +
                                wa_report_cal_aux-HSL11 +
                                wa_report_cal_aux-HSL12.

      APPEND wa_report_cal_aux TO IT_REPORT_CAL.


*    IF SY-SUBRC EQ 0.
*
*        CLEAR: WA_REPORT_CAL.
*        WA_REPORT_CAL-TXT50 = C_PTU_PAGADA.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_01
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL01 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_02
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL02 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_03
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL03 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_04
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL04 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_05
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL05 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_06
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL06 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_07
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL07 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_08
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL08 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_09
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_10
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_11
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_12
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        WA_REPORT_CAL-TOTAL = WA_REPORT_CAL-HSL01 +
*                              WA_REPORT_CAL-HSL02 +
*                              WA_REPORT_CAL-HSL03 +
*                              WA_REPORT_CAL-HSL04 +
*                              WA_REPORT_CAL-HSL05 +
*                              WA_REPORT_CAL-HSL06 +
*                              WA_REPORT_CAL-HSL07 +
*                              WA_REPORT_CAL-HSL08 +
*                              WA_REPORT_CAL-HSL09 +
*                              WA_REPORT_CAL-HSL10 +
*                              WA_REPORT_CAL-HSL11 +
*                              WA_REPORT_CAL-HSL12.
*
*      ENDIF.
*
*     APPEND WA_REPORT_CAL TO IT_REPORT_CAL.


    "PERDIDAS POR AMORTIZAR Y PERDIDA AMORTIZADA.
*    REFRESH IT_COFUT.

    SELECT SINGLE ZCOEFICIENTE_UTILIDAD
      FROM ZFI_CUENTAS_ISR
      INTO @SV_FACTOR_RESULTADO
     WHERE ZCODIGO EQ 'FACTR' AND
             GJAHR IN @S_RYEAR  AND "EJERCICIO
             BUKRS EQ @S_RBUKRS.    "SOCIEDAD

     SV_FACTOR_RESULTADO_AUX = SV_FACTOR_RESULTADO.

     CLEAR: WA_REPORT_CAL,
            wa_report_cal_aux.

     WA_REPORT_CAL-TXT50 = C_PERDIDAS_POR_AMORTIZAR.
     wa_report_cal_aux-txt50 =  c_perdidas_amortizada.

     IF SY-SUBRC EQ 0.

       READ TABLE it_report_cal INTO wa_report_calc WITH KEY txt50 = c_utilidad_fiscal.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL01 = 0.

       ELSE.

         WA_REPORT_CAL-HSL01 = SV_FACTOR_RESULTADO - wa_report_calc-hsl01.
         SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl01.

         IF SV_FACTOR_RESULTADO LE 0.

           SV_FACTOR_RESULTADO = 0.
           WA_REPORT_CAL-HSL01 = 0.

         ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL01 =  SV_FACTOR_RESULTADO_AUX - WA_REPORT_CAL-HSL01.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL02 = 0.

       ELSE.

         WA_REPORT_CAL-HSL02 = SV_FACTOR_RESULTADO - wa_report_calc-hsl02.
         SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl02.

         IF SV_FACTOR_RESULTADO LE 0.

           SV_FACTOR_RESULTADO = 0.
           WA_REPORT_CAL-HSL02 = 0.

         ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL02 =  WA_REPORT_CAL-HSL01 - WA_REPORT_CAL-HSL02.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL03 = 0.

       ELSE.

         WA_REPORT_CAL-HSL03 = SV_FACTOR_RESULTADO - wa_report_calc-hsl03.
         SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl03.

         IF SV_FACTOR_RESULTADO LE 0.

           SV_FACTOR_RESULTADO = 0.
           WA_REPORT_CAL-HSL03 = 0.

         ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL03 =  WA_REPORT_CAL-HSL02 - WA_REPORT_CAL-HSL03.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL04 = 0.

       ELSE.

        WA_REPORT_CAL-HSL04 = SV_FACTOR_RESULTADO - wa_report_calc-hsl04.
        SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl04.

        IF SV_FACTOR_RESULTADO LE 0.

          SV_FACTOR_RESULTADO = 0.
          WA_REPORT_CAL-HSL04 = 0.

        ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL04 =  WA_REPORT_CAL-HSL03 - WA_REPORT_CAL-HSL04.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL05 = 0.

       ELSE.

         WA_REPORT_CAL-HSL05 = SV_FACTOR_RESULTADO - wa_report_calc-hsl05.
         SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl05.

         IF SV_FACTOR_RESULTADO LE 0.

           SV_FACTOR_RESULTADO = 0.
           WA_REPORT_CAL-HSL05 = 0.

         ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL05 =  WA_REPORT_CAL-HSL04 - WA_REPORT_CAL-HSL05.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL06 = 0.

       ELSE.

         WA_REPORT_CAL-HSL06 = SV_FACTOR_RESULTADO - wa_report_calc-hsl06.
         SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl06.

         IF SV_FACTOR_RESULTADO LE 0.

           SV_FACTOR_RESULTADO = 0.
           WA_REPORT_CAL-HSL06 = 0.

         ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL06 =  WA_REPORT_CAL-HSL05 - WA_REPORT_CAL-HSL06.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL07 = 0.

       ELSE.

         WA_REPORT_CAL-HSL07 = SV_FACTOR_RESULTADO - wa_report_calc-hsl07.
         SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl07.

         IF SV_FACTOR_RESULTADO LE 0.

           SV_FACTOR_RESULTADO = 0.
           WA_REPORT_CAL-HSL07 = 0.

         ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL07 =  WA_REPORT_CAL-HSL06 - WA_REPORT_CAL-HSL07.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL08 = 0.

       ELSE.

         WA_REPORT_CAL-HSL08 = SV_FACTOR_RESULTADO - wa_report_calc-hsl08.
         SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl08.

         IF SV_FACTOR_RESULTADO LE 0.

           SV_FACTOR_RESULTADO = 0.
           WA_REPORT_CAL-HSL08 = 0.

         ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL08 =  WA_REPORT_CAL-HSL07 - WA_REPORT_CAL-HSL08.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL09 = 0.

       ELSE.

         WA_REPORT_CAL-HSL09 = SV_FACTOR_RESULTADO - wa_report_calc-hsl09.
         SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl09.

         IF SV_FACTOR_RESULTADO LE 0.

           SV_FACTOR_RESULTADO = 0.
           WA_REPORT_CAL-HSL09 = 0.

         ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL09 =  WA_REPORT_CAL-HSL08 - WA_REPORT_CAL-HSL09.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL10 = 0.

       ELSE.

         WA_REPORT_CAL-HSL10 = SV_FACTOR_RESULTADO - wa_report_calc-hsl10.
         SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl10.

         IF SV_FACTOR_RESULTADO LE 0.

           SV_FACTOR_RESULTADO = 0.
           WA_REPORT_CAL-HSL10 = 0.

         ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL10 =  WA_REPORT_CAL-HSL09 - WA_REPORT_CAL-HSL10.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL11 = 0.

       ELSE.

         WA_REPORT_CAL-HSL11 = SV_FACTOR_RESULTADO - wa_report_calc-hsl11.
         SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl11.

         IF SV_FACTOR_RESULTADO LE 0.

           SV_FACTOR_RESULTADO = 0.
           WA_REPORT_CAL-HSL11 = 0.

         ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL11 =  WA_REPORT_CAL-HSL10 - WA_REPORT_CAL-HSL11.

       IF SV_FACTOR_RESULTADO EQ 0.

         WA_REPORT_CAL-HSL12 = 0.

       ELSE.

         WA_REPORT_CAL-HSL12 = SV_FACTOR_RESULTADO - wa_report_calc-hsl12.
         SV_FACTOR_RESULTADO = SV_FACTOR_RESULTADO - wa_report_calc-hsl12.

         IF SV_FACTOR_RESULTADO LE 0.

           SV_FACTOR_RESULTADO = 0.
           WA_REPORT_CAL-HSL12 = 0.

         ENDIF.

       ENDIF.

       wa_report_cal_aux-HSL12 =  WA_REPORT_CAL-HSL11 - WA_REPORT_CAL-HSL12.

     ENDIF.

     APPEND WA_REPORT_CAL TO IT_REPORT_CAL.

     APPEND wa_report_cal_aux  TO IT_REPORT_CAL.


*    SELECT GJAHR,
*           BUKRS,
*           MONAT,
*           ZCOEFICIENTE_UTILIDAD
*      FROM ZFI_CUENTAS_ISR
*      INTO TABLE @IT_COFUT
*      WHERE ZCODIGO EQ 'PERAM'   AND
*            GJAHR   IN @S_RYEAR  AND "EJERCICIO
*            BUKRS   EQ @S_RBUKRS.    "SOCIEDAD
*    IF SY-SUBRC EQ 0.
*
*        CLEAR: WA_REPORT_CAL.
*        WA_REPORT_CAL-TXT50 = C_PERDIDAS_POR_AMORTIZAR.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_01
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL01 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_02
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL02 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_03
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL03 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_04
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL04 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_05
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL05 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_06
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL06 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_07
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL07 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_08
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL08 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_09
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL09 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_10
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL10 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_11
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL11 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        READ TABLE IT_COFUT INTO WA_COFUT WITH KEY MONAT = C_12
*                                                   BUKRS = S_RBUKRS
*                                                   GJAHR = S_RYEAR-LOW.
*
*        WA_REPORT_CAL-HSL12 = WA_COFUT-ZCOEFICIENTE_UTILIDAD.
*
*        WA_REPORT_CAL-TOTAL = WA_REPORT_CAL-HSL01 +
*                              WA_REPORT_CAL-HSL02 +
*                              WA_REPORT_CAL-HSL03 +
*                              WA_REPORT_CAL-HSL04 +
*                              WA_REPORT_CAL-HSL05 +
*                              WA_REPORT_CAL-HSL06 +
*                              WA_REPORT_CAL-HSL07 +
*                              WA_REPORT_CAL-HSL08 +
*                              WA_REPORT_CAL-HSL09 +
*                              WA_REPORT_CAL-HSL10 +
*                              WA_REPORT_CAL-HSL11 +
*                              WA_REPORT_CAL-HSL12.

*      ENDIF.
*
*     APPEND WA_REPORT_CAL TO IT_REPORT_CAL.

*    REFRESH: it_tvarvc.
*    CLEAR: wa_tvarvc.
*
*    SELECT low,
*           high
*      FROM tvarvc
*      INTO TABLE @it_tvarvc
*      WHERE name EQ @c_z_perdidas_x_amort.

*    IF sy-subrc EQ 0.
*      DELETE it_tvarvc WHERE low+0(4) NE s_rbukrs.
*      CLEAR: wa_report_cal.
*      wa_report_cal-txt50 = c_perdidas_por_amortizar.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_01
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl01 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_02
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl02 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_03
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl03 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_04
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl04 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_05
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl05 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_06
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl06 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_07
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl07 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_08
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl08 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_09
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl09 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_10
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl10 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_11
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl11 = wa_tvarvc-high.
*      ENDIF.
*      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_12
*                                                   low+8(4) = s_ryear-low.
*      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
*        wa_report_cal-hsl12 = wa_tvarvc-high.
*      ENDIF.


    "Estimulo Fiscal PTU del mes
    REFRESH: it_tvarvc.
    CLEAR: wa_tvarvc.

    SELECT low,
           high
      FROM tvarvc
      INTO TABLE @it_tvarvc
      WHERE name EQ @c_z_estimulo_ptu.

    IF sy-subrc EQ 0.
      DELETE it_tvarvc WHERE low+0(4) NE s_rbukrs.
      CLEAR: wa_report_cal.
      wa_report_cal-txt50 = c_estimulo_fiscal_ptu_del_mes.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_01
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl01 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_02
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl02 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_03
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl03 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_04
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl04 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_05
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl05 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_06
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl06 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_07
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl07 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_08
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl08 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_09
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl09 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_10
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl10 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_11
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl11 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low+5(2) = c_12
                                                   low+8(4) = s_ryear-low.
      IF wa_tvarvc-low+0(4) EQ s_rbukrs.
        wa_report_cal-hsl12 = wa_tvarvc-high.
      ENDIF.

      APPEND wa_report_cal TO it_report_cal.

    ENDIF.

    "Estimulo Fiscal PTU Acumulado
    CLEAR: wa_report_calc,
           wa_report_cal.

    READ TABLE it_report_cal INTO wa_report_calc WITH KEY txt50 = c_estimulo_fiscal_ptu_del_mes.

    wa_report_cal-txt50 = c_estimulofiscalptuacumulado.
    wa_report_cal-hsl01 = wa_report_calc-hsl01.
    wa_report_cal-hsl02 = wa_report_cal-hsl01 + wa_report_calc-hsl02.
    wa_report_cal-hsl03 = wa_report_cal-hsl02 + wa_report_calc-hsl03.
    wa_report_cal-hsl04 = wa_report_cal-hsl03 + wa_report_calc-hsl04.
    wa_report_cal-hsl05 = wa_report_cal-hsl04 + wa_report_calc-hsl05.
    wa_report_cal-hsl06 = wa_report_cal-hsl05 + wa_report_calc-hsl06.
    wa_report_cal-hsl07 = wa_report_cal-hsl06 + wa_report_calc-hsl07.
    wa_report_cal-hsl08 = wa_report_cal-hsl07 + wa_report_calc-hsl08.
    wa_report_cal-hsl09 = wa_report_cal-hsl08 + wa_report_calc-hsl09.
    wa_report_cal-hsl10 = wa_report_cal-hsl09 + wa_report_calc-hsl10.
    wa_report_cal-hsl11 = wa_report_cal-hsl10 + wa_report_calc-hsl11.
    wa_report_cal-hsl12 = wa_report_cal-hsl11 + wa_report_calc-hsl12.

    APPEND wa_report_cal TO it_report_cal.

    "RESULTADO FISCAL
    CLEAR: wa_report_cal_aux,
           wa_report_calcu,
           wa_report_calc.

*    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_utilidad_fiscal.
*    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_perdidas_por_amortizar.
*    READ TABLE it_report_cal INTO wa_report_calc WITH KEY txt50 = c_estimulofiscalptuacumulado.
*
*    wa_report_cal-txt50 = c_resultado_fiscal.
*    wa_report_cal-hsl01 = wa_report_cal_aux-hsl01 - SV_FACTOR_RESULTADO_AUX - wa_report_calc-hsl01.
*    wa_report_cal-hsl02 = wa_report_cal_aux-hsl02 - wa_report_calcu-hsl01 - wa_report_calc-hsl02.
*    wa_report_cal-hsl03 = wa_report_cal_aux-hsl03 - wa_report_calcu-hsl02 - wa_report_calc-hsl03.
*    wa_report_cal-hsl04 = wa_report_cal_aux-hsl04 - wa_report_calcu-hsl03 - wa_report_calc-hsl04.
*    wa_report_cal-hsl05 = wa_report_cal_aux-hsl05 - wa_report_calcu-hsl04 - wa_report_calc-hsl05.
*    wa_report_cal-hsl06 = wa_report_cal_aux-hsl06 - wa_report_calcu-hsl05 - wa_report_calc-hsl06.
*    wa_report_cal-hsl07 = wa_report_cal_aux-hsl07 - wa_report_calcu-hsl06 - wa_report_calc-hsl07.
*    wa_report_cal-hsl08 = wa_report_cal_aux-hsl08 - wa_report_calcu-hsl07 - wa_report_calc-hsl08.
*    wa_report_cal-hsl09 = wa_report_cal_aux-hsl09 - wa_report_calcu-hsl08 - wa_report_calc-hsl09.
*    wa_report_cal-hsl10 = wa_report_cal_aux-hsl10 - wa_report_calcu-hsl09 - wa_report_calc-hsl10.
*    wa_report_cal-hsl11 = wa_report_cal_aux-hsl11 - wa_report_calcu-hsl10 - wa_report_calc-hsl11.
*    wa_report_cal-hsl12 = wa_report_cal_aux-hsl12 - wa_report_calcu-hsl11 - wa_report_calc-hsl12.

    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_utilidad_fiscal.
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_perdidas_amortizada.
    READ TABLE it_report_cal INTO wa_report_calc WITH KEY txt50 = c_estimulofiscalptuacumulado.

    wa_report_cal-txt50 = c_resultado_fiscal.
    wa_report_cal-hsl01 = wa_report_cal_aux-hsl01 - wa_report_calcu-hsl01 - wa_report_calc-hsl01.
    wa_report_cal-hsl02 = wa_report_cal_aux-hsl02 - wa_report_calcu-hsl02 - wa_report_calc-hsl02.
    wa_report_cal-hsl03 = wa_report_cal_aux-hsl03 - wa_report_calcu-hsl03 - wa_report_calc-hsl03.
    wa_report_cal-hsl04 = wa_report_cal_aux-hsl04 - wa_report_calcu-hsl04 - wa_report_calc-hsl04.
    wa_report_cal-hsl05 = wa_report_cal_aux-hsl05 - wa_report_calcu-hsl05 - wa_report_calc-hsl05.
    wa_report_cal-hsl06 = wa_report_cal_aux-hsl06 - wa_report_calcu-hsl06 - wa_report_calc-hsl06.
    wa_report_cal-hsl07 = wa_report_cal_aux-hsl07 - wa_report_calcu-hsl07 - wa_report_calc-hsl07.
    wa_report_cal-hsl08 = wa_report_cal_aux-hsl08 - wa_report_calcu-hsl08 - wa_report_calc-hsl08.
    wa_report_cal-hsl09 = wa_report_cal_aux-hsl09 - wa_report_calcu-hsl09 - wa_report_calc-hsl09.
    wa_report_cal-hsl10 = wa_report_cal_aux-hsl10 - wa_report_calcu-hsl10 - wa_report_calc-hsl10.
    wa_report_cal-hsl11 = wa_report_cal_aux-hsl11 - wa_report_calcu-hsl11 - wa_report_calc-hsl11.
    wa_report_cal-hsl12 = wa_report_cal_aux-hsl12 - wa_report_calcu-hsl12 - wa_report_calc-hsl12.

    APPEND wa_report_cal TO it_report_cal.

    "TASA
    REFRESH: it_tvarvc.
    CLEAR: wa_tvarvc.

    SELECT low,
           high
      FROM tvarvc
      INTO TABLE @it_tvarvc
      WHERE name EQ @c_z_tasa_fiscal.

    IF sy-subrc EQ 0.
      wa_report_cal-txt50 = c_tasa.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_01.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl01 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_02.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl02 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_03.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl03 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_04.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl04 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_05.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl05 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_06.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl06 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_07.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl07 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_08.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl08 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_09.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl09 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_10.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl10 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_11.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl11 = wa_tvarvc-high.
      ENDIF.
      READ TABLE it_tvarvc INTO wa_tvarvc WITH KEY low = c_12.
      IF sy-subrc EQ 0.
        wa_report_cal-hsl12 = wa_tvarvc-high.
      ENDIF.

      APPEND wa_report_cal TO it_report_cal.

    ENDIF.

    "IMPUESTO DETERMINADO
    CLEAR: wa_report_cal_aux,
           wa_report_calcu,
           wa_report_cal.

    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_resultado_fiscal.
    READ TABLE it_report_cal INTO wa_report_calcu   WITH KEY txt50 = c_tasa.

    wa_report_cal-txt50 = c_impuesto_determinado.
    wa_report_cal-hsl01 = wa_report_cal_aux-hsl01 * wa_report_calcu-hsl01.
    wa_report_cal-hsl02 = wa_report_cal_aux-hsl02 * wa_report_calcu-hsl02.
    wa_report_cal-hsl03 = wa_report_cal_aux-hsl03 * wa_report_calcu-hsl03.
    wa_report_cal-hsl04 = wa_report_cal_aux-hsl04 * wa_report_calcu-hsl04.
    wa_report_cal-hsl05 = wa_report_cal_aux-hsl05 * wa_report_calcu-hsl05.
    wa_report_cal-hsl06 = wa_report_cal_aux-hsl06 * wa_report_calcu-hsl06.
    wa_report_cal-hsl07 = wa_report_cal_aux-hsl07 * wa_report_calcu-hsl07.
    wa_report_cal-hsl08 = wa_report_cal_aux-hsl08 * wa_report_calcu-hsl08.
    wa_report_cal-hsl09 = wa_report_cal_aux-hsl09 * wa_report_calcu-hsl09.
    wa_report_cal-hsl10 = wa_report_cal_aux-hsl10 * wa_report_calcu-hsl10.
    wa_report_cal-hsl11 = wa_report_cal_aux-hsl11 * wa_report_calcu-hsl11.
    wa_report_cal-hsl12 = wa_report_cal_aux-hsl12 * wa_report_calcu-hsl12.

    APPEND wa_report_cal TO it_report_cal.

    "INI ADD 25/02/2021 CJPG y VARR <3
    "ISR RETENIDO
    REFRESH it_tvarvc.
    CLEAR   wa_tvarvc.

    SELECT low,
           high
      FROM tvarvc
      INTO TABLE @it_tvarvc
      WHERE name EQ @c_z_isr_retenido.

    IF sy-subrc EQ 0.
      SORT it_tvarvc BY low.

      LOOP AT it_tvarvc INTO wa_tvarvc.
        CLEAR rwa_isr_rete.
        rwa_isr_rete-sign    = 'I'.            " I : Incluir
        rwa_isr_rete-option  = 'EQ'.           " EQ: Igual
        rwa_isr_rete-low     = wa_tvarvc-low.  " Cuenta
        APPEND  rwa_isr_rete TO rg_isr_rete.
      ENDLOOP.

      CLEAR: wa_report_cal,
*      wa_report_cal-txt50 = c_isr_retenido.
             vg_isr_ret_01,
             vg_isr_ret_02,
             vg_isr_ret_03,
             vg_isr_ret_04,
             vg_isr_ret_05,
             vg_isr_ret_06,
             vg_isr_ret_07,
             vg_isr_ret_08,
             vg_isr_ret_09,
             vg_isr_ret_10,
             vg_isr_ret_11,
             vg_isr_ret_12.

      LOOP AT it_report_aux INTO wa_report.

        IF wa_report-racct IN rg_isr_rete.

          CLEAR wa_fmglflext.
          READ TABLE it_fmglflext_aux INTO wa_fmglflext_aux WITH KEY racct = wa_report-racct.

          IF sy-subrc EQ 0.
            wa_report_cal-racct = wa_report-racct.
            wa_report_cal-txt50 = wa_report-txt50.
            wa_report_cal-hsl01 = wa_fmglflext_aux-hslvt  + wa_fmglflext_aux-hsl01.
            wa_report_cal-hsl02 = wa_report_cal-hsl01     + wa_fmglflext_aux-hsl02.
            wa_report_cal-hsl03 = wa_report_cal-hsl02     + wa_fmglflext_aux-hsl03.
            wa_report_cal-hsl04 = wa_report_cal-hsl03     + wa_fmglflext_aux-hsl04.
            wa_report_cal-hsl05 = wa_report_cal-hsl04     + wa_fmglflext_aux-hsl05.
            wa_report_cal-hsl06 = wa_report_cal-hsl05     + wa_fmglflext_aux-hsl06.
            wa_report_cal-hsl07 = wa_report_cal-hsl06     + wa_fmglflext_aux-hsl07.
            wa_report_cal-hsl08 = wa_report_cal-hsl07     + wa_fmglflext_aux-hsl08.
            wa_report_cal-hsl09 = wa_report_cal-hsl08     + wa_fmglflext_aux-hsl09.
            wa_report_cal-hsl10 = wa_report_cal-hsl09     + wa_fmglflext_aux-hsl10.
            wa_report_cal-hsl11 = wa_report_cal-hsl10     + wa_fmglflext_aux-hsl11.
            wa_report_cal-hsl12 = wa_report_cal-hsl11     + wa_fmglflext_aux-hsl12.

            "Convertir a positivos las cantidades anteriores
            IF wa_report_cal-hsl01 LT 0.
              wa_report_cal-hsl01 = wa_report_cal-hsl01 * -1.
            ENDIF.

            IF wa_report_cal-hsl02 LT 0.
              wa_report_cal-hsl02 = wa_report_cal-hsl02 * -1.
            ENDIF.

            IF wa_report_cal-hsl03 LT 0.
              wa_report_cal-hsl03 = wa_report_cal-hsl03 * -1.
            ENDIF.

            IF wa_report_cal-hsl04 LT 0.
              wa_report_cal-hsl04 = wa_report_cal-hsl04 * -1.
            ENDIF.

            IF wa_report_cal-hsl05 LT 0.
              wa_report_cal-hsl05 = wa_report_cal-hsl05 * -1.
            ENDIF.

            IF wa_report_cal-hsl06 LT 0.
              wa_report_cal-hsl06 = wa_report_cal-hsl06 * -1.
            ENDIF.

            IF wa_report_cal-hsl07 LT 0.
              wa_report_cal-hsl07 = wa_report_cal-hsl07 * -1.
            ENDIF.

            IF wa_report_cal-hsl08 LT 0.
              wa_report_cal-hsl08 = wa_report_cal-hsl08 * -1.
            ENDIF.

            IF wa_report_cal-hsl09 LT 0.
              wa_report_cal-hsl09 = wa_report_cal-hsl09 * -1.
            ENDIF.

            IF wa_report_cal-hsl10 LT 0.
              wa_report_cal-hsl10 = wa_report_cal-hsl10 * -1.
            ENDIF.

            IF wa_report_cal-hsl11 LT 0.
              wa_report_cal-hsl11 = wa_report_cal-hsl11 * -1.
            ENDIF.

            IF wa_report_cal-hsl12 LT 0.
              wa_report_cal-hsl12 = wa_report_cal-hsl12 * -1.
            ENDIF.

            vg_isr_ret_01 = vg_isr_ret_01 + wa_report_cal-hsl01.
            vg_isr_ret_02 = vg_isr_ret_02 + wa_report_cal-hsl02.
            vg_isr_ret_03 = vg_isr_ret_03 + wa_report_cal-hsl03.
            vg_isr_ret_04 = vg_isr_ret_04 + wa_report_cal-hsl04.
            vg_isr_ret_05 = vg_isr_ret_05 + wa_report_cal-hsl05.
            vg_isr_ret_06 = vg_isr_ret_06 + wa_report_cal-hsl06.
            vg_isr_ret_07 = vg_isr_ret_07 + wa_report_cal-hsl07.
            vg_isr_ret_08 = vg_isr_ret_08 + wa_report_cal-hsl08.
            vg_isr_ret_09 = vg_isr_ret_09 + wa_report_cal-hsl09.
            vg_isr_ret_10 = vg_isr_ret_10 + wa_report_cal-hsl10.
            vg_isr_ret_11 = vg_isr_ret_11 + wa_report_cal-hsl11.
            vg_isr_ret_12 = vg_isr_ret_12 + wa_report_cal-hsl12.

            APPEND  wa_report_cal TO it_report_cal.
            CLEAR: wa_report, wa_report_cal.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    "FIN ADD 25/02/2021 CJPG y VARR <3


    "ACREDITAMIENTO DE ISR RETENIDO MENSUAL
*    CLEAR: wa_report,
*           wa_report_cal.
*    LOOP AT it_report INTO wa_report WHERE racct EQ '1113029998'. "NOTA: Cuando se tengan las cuentas correctas,
*      "cambiar el num de cuenta por la cuenta proporcionada
*      wa_report_cal-txt50 = c_acreddeisrretmen.
*      wa_report_cal-hsl01 = wa_report_cal-hsl01 + wa_report-hsl01.
*      wa_report_cal-hsl02 = wa_report_cal-hsl02 + wa_report-hsl02.
*      wa_report_cal-hsl03 = wa_report_cal-hsl03 + wa_report-hsl03.
*      wa_report_cal-hsl04 = wa_report_cal-hsl04 + wa_report-hsl04.
*      wa_report_cal-hsl05 = wa_report_cal-hsl05 + wa_report-hsl05.
*      wa_report_cal-hsl06 = wa_report_cal-hsl06 + wa_report-hsl06.
*      wa_report_cal-hsl07 = wa_report_cal-hsl07 + wa_report-hsl07.
*      wa_report_cal-hsl08 = wa_report_cal-hsl08 + wa_report-hsl08.
*      wa_report_cal-hsl09 = wa_report_cal-hsl09 + wa_report-hsl09.
*      wa_report_cal-hsl10 = wa_report_cal-hsl10 + wa_report-hsl10.
*      wa_report_cal-hsl11 = wa_report_cal-hsl11 + wa_report-hsl11.
*      wa_report_cal-hsl12 = wa_report_cal-hsl12 + wa_report-hsl12.
*    ENDLOOP.
*
*    APPEND wa_report_cal TO it_report_cal.

    "ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    CLEAR: wa_report_calc,
*           wa_report_cal.
*    READ TABLE it_report_cal INTO wa_report_calc WITH KEY txt50 = c_acreddeisrretmen.
*    wa_report_cal-txt50 = c_acredisrretacum.
*    wa_report_cal-hsl01 = wa_report_calc-hsl01.
*    wa_report_cal-hsl02 = wa_report_cal-hsl01 + wa_report_calc-hsl02.
*    wa_report_cal-hsl03 = wa_report_cal-hsl02 + wa_report_calc-hsl03.
*    wa_report_cal-hsl04 = wa_report_cal-hsl03 + wa_report_calc-hsl04.
*    wa_report_cal-hsl05 = wa_report_cal-hsl04 + wa_report_calc-hsl05.
*    wa_report_cal-hsl06 = wa_report_cal-hsl05 + wa_report_calc-hsl06.
*    wa_report_cal-hsl07 = wa_report_cal-hsl06 + wa_report_calc-hsl07.
*    wa_report_cal-hsl08 = wa_report_cal-hsl07 + wa_report_calc-hsl08.
*    wa_report_cal-hsl09 = wa_report_cal-hsl08 + wa_report_calc-hsl09.
*    wa_report_cal-hsl10 = wa_report_cal-hsl09 + wa_report_calc-hsl10.
*    wa_report_cal-hsl11 = wa_report_cal-hsl10 + wa_report_calc-hsl11.
*    wa_report_cal-hsl12 = wa_report_cal-hsl11 + wa_report_calc-hsl12.
*
*    APPEND wa_report_cal TO it_report_cal.


**************************************PARA ENERO (HSL01)***************************************************************
    "ADD VARR 01/03/2021
    DATA lv_monto_aux TYPE fmglflext-hsl01.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_cal.
    "Traemos Datos de Impuesto Detemrinado
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos "ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc    WITH KEY txt50 = c_acredisrretacum.
    wa_report_cal-txt50 = c_isrpagperdesacredit.
    wa_report_cal-hsl01 = wa_report_cal_aux-hsl01 - vg_isr_ret_01. "- wa_report_calc-hsl01.
    APPEND wa_report_cal TO it_report_cal.


    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR: wa_report_calcu, wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    wa_report_calcu-txt50 = c_acumisrdespacdt.
    wa_report_calcu-hsl01 = wa_report_cal-hsl01.
    APPEND wa_report_calcu TO it_report_cal.

**************************************PARA FEBRERO (HSL02)***************************************************************

    "---------------------------PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD----------------------------------
    CLEAR: wa_report_cal, wa_report_calcu.
    "Traemos dato de ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    wa_report_calcu-txt50 = c_pprovisacumpagadosant.
    wa_report_calcu-hsl02 = wa_report_cal-hsl01.
    APPEND wa_report_calcu TO it_report_cal.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    "Traemos Datos de Impuesto Detemrinado
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_calcu, lv_monto_aux.
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_pprovisacumpagadosant.
    "Traemos ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc    WITH KEY txt50 = c_acredisrretacum.
    "Traemos ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR En Fiels Symbol Para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_2> WITH KEY txt50 = c_isrpagperdesacredit.
    lv_monto_aux = wa_report_calcu-hsl02.
    IF lv_monto_aux LT 0.
      lv_monto_aux = lv_monto_aux * -1.
    ENDIF.
    <fs_report_cal_2>-hsl02 = wa_report_cal_aux-hsl02 - lv_monto_aux - vg_isr_ret_02. "- wa_report_calc-hsl02.

    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    "Traemos ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO ) Asignando a FS
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_2> WITH KEY txt50 = c_acumisrdespacdt.
    <fs_report_cal_2>-hsl02 = <fs_report_cal_2>-hsl01 + wa_report_cal-hsl02.


**************************************PARA MARZO (HSL03)***************************************************************

    "---------------------------PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD----------------------------------
    CLEAR wa_report_cal.
    "Traemos dato de ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD en FS para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_3> WITH KEY txt50 = c_pprovisacumpagadosant.
    <fs_report_cal_3>-hsl03 = wa_report_cal-hsl02.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    "Traemos Datos de Impuesto Detemrinado
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_calcu, lv_monto_aux.
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_pprovisacumpagadosant.
    "Traemos ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc  WITH KEY txt50 = c_acredisrretacum.
    "Traemos ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR En Fiels Symbol Para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_3> WITH KEY txt50 = c_isrpagperdesacredit.
    lv_monto_aux = wa_report_calcu-hsl03.
    IF lv_monto_aux LT 0.
      lv_monto_aux = lv_monto_aux * -1.
    ENDIF.
    <fs_report_cal_3>-hsl03 = wa_report_cal_aux-hsl03 - lv_monto_aux - vg_isr_ret_03. "- wa_report_calc-hsl03.

    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    "Traemos ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO ) Asignando a FS
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_3> WITH KEY txt50 = c_acumisrdespacdt.
    <fs_report_cal_3>-hsl03 = <fs_report_cal_3>-hsl02 + wa_report_cal-hsl03.


**************************************PARA ABRIL (HSL04)***************************************************************

    "---------------------------PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD----------------------------------
    CLEAR wa_report_cal.
    "Traemos dato de ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD en FS para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_4> WITH KEY txt50 = c_pprovisacumpagadosant.
    <fs_report_cal_4>-hsl04 = wa_report_cal-hsl03.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    "Traemos Datos de Impuesto Detemrinado
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_calcu, lv_monto_aux.
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_pprovisacumpagadosant.
    "Traemos ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc  WITH KEY txt50 = c_acredisrretacum.
    "Traemos ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR En Fiels Symbol Para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_4> WITH KEY txt50 = c_isrpagperdesacredit.
    lv_monto_aux = wa_report_calcu-hsl04.
    IF lv_monto_aux LT 0.
      lv_monto_aux = lv_monto_aux * -1.
    ENDIF.
    <fs_report_cal_4>-hsl04 = wa_report_cal_aux-hsl04 - lv_monto_aux - vg_isr_ret_04. "- wa_report_calc-hsl04.

    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    "Traemos ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO ) Asignando a FS
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_4> WITH KEY txt50 = c_acumisrdespacdt.
    <fs_report_cal_4>-hsl04 = <fs_report_cal_4>-hsl03 + wa_report_cal-hsl04.


**************************************PARA MAYO (HSL05)***************************************************************

    "---------------------------PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD----------------------------------
    CLEAR wa_report_cal.
    "Traemos dato de ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD en FS para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_5> WITH KEY txt50 = c_pprovisacumpagadosant.
    <fs_report_cal_5>-hsl05 = wa_report_cal-hsl04.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    "Traemos Datos de Impuesto Detemrinado
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_calcu, lv_monto_aux.
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_pprovisacumpagadosant.
    "Traemos ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc  WITH KEY txt50 = c_acredisrretacum.
    "Traemos ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR En Fiels Symbol Para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_5> WITH KEY txt50 = c_isrpagperdesacredit.
    lv_monto_aux = wa_report_calcu-hsl05.
    IF lv_monto_aux LT 0.
      lv_monto_aux = lv_monto_aux * -1.
    ENDIF.
    <fs_report_cal_5>-hsl05 = wa_report_cal_aux-hsl05 - lv_monto_aux - vg_isr_ret_05. "- wa_report_calc-hsl05.

    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    "Traemos ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO ) Asignando a FS
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_5> WITH KEY txt50 = c_acumisrdespacdt.
    <fs_report_cal_5>-hsl05 = <fs_report_cal_5>-hsl04 + wa_report_cal-hsl05.


**************************************PARA JUNIO (HSL06)***************************************************************

    "---------------------------PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD----------------------------------
    CLEAR wa_report_cal.
    "Traemos dato de ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD en FS para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_6> WITH KEY txt50 = c_pprovisacumpagadosant.
    <fs_report_cal_6>-hsl06 = wa_report_cal-hsl05.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    "Traemos Datos de Impuesto Detemrinado
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_calcu, lv_monto_aux.
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_pprovisacumpagadosant.
    "Traemos ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc  WITH KEY txt50 = c_acredisrretacum.
    "Traemos ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR En Fiels Symbol Para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_6> WITH KEY txt50 = c_isrpagperdesacredit.
    lv_monto_aux = wa_report_calcu-hsl06.
    IF lv_monto_aux LT 0.
      lv_monto_aux = lv_monto_aux * -1.
    ENDIF.
    <fs_report_cal_6>-hsl06 = wa_report_cal_aux-hsl06 - lv_monto_aux - vg_isr_ret_06. "- wa_report_calc-hsl06.

    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    "Traemos ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO ) Asignando a FS
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_6> WITH KEY txt50 = c_acumisrdespacdt.
    <fs_report_cal_6>-hsl06 = <fs_report_cal_6>-hsl05 + wa_report_cal-hsl06.


**************************************PARA JULIO (HSL07)***************************************************************

    "---------------------------PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD----------------------------------
    CLEAR wa_report_cal.
    "Traemos dato de ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD en FS para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_7> WITH KEY txt50 = c_pprovisacumpagadosant.
    <fs_report_cal_7>-hsl07 = wa_report_cal-hsl06.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    "Traemos Datos de Impuesto Detemrinado
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_calcu, lv_monto_aux.
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_pprovisacumpagadosant.
    "Traemos ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc  WITH KEY txt50 = c_acredisrretacum.
    "Traemos ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR En Fiels Symbol Para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_7> WITH KEY txt50 = c_isrpagperdesacredit.
    lv_monto_aux = wa_report_calcu-hsl07.
    IF lv_monto_aux LT 0.
      lv_monto_aux = lv_monto_aux * -1.
    ENDIF.
    <fs_report_cal_7>-hsl07 = wa_report_cal_aux-hsl07 - lv_monto_aux - vg_isr_ret_07. "- wa_report_calc-hsl07.

    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    "Traemos ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO ) Asignando a FS
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_7> WITH KEY txt50 = c_acumisrdespacdt.
    <fs_report_cal_7>-hsl07 = <fs_report_cal_7>-hsl06 + wa_report_cal-hsl07.


**************************************PARA AGOSTO (HSL08)***************************************************************

    "---------------------------PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD----------------------------------
    CLEAR wa_report_cal.
    "Traemos dato de ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD en FS para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_8> WITH KEY txt50 = c_pprovisacumpagadosant.
    <fs_report_cal_8>-hsl08 = wa_report_cal-hsl07.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    "Traemos Datos de Impuesto Detemrinado
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_calcu, lv_monto_aux.
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_pprovisacumpagadosant.
    "Traemos ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc  WITH KEY txt50 = c_acredisrretacum.
    "Traemos ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR En Fiels Symbol Para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_8> WITH KEY txt50 = c_isrpagperdesacredit.
    lv_monto_aux = wa_report_calcu-hsl08.
    IF lv_monto_aux LT 0.
      lv_monto_aux = lv_monto_aux * -1.
    ENDIF.
    <fs_report_cal_8>-hsl08 = wa_report_cal_aux-hsl08 - lv_monto_aux - vg_isr_ret_08. "- wa_report_calc-hsl08.

    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    "Traemos ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO ) Asignando a FS
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_8> WITH KEY txt50 = c_acumisrdespacdt.
    <fs_report_cal_8>-hsl08 = <fs_report_cal_8>-hsl07 + wa_report_cal-hsl08.


**************************************PARA SEPTIEMBRE (HSL09)***************************************************************

    "---------------------------PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD----------------------------------
    CLEAR wa_report_cal.
    "Traemos dato de ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD en FS para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_9> WITH KEY txt50 = c_pprovisacumpagadosant.
    <fs_report_cal_9>-hsl09 = wa_report_cal-hsl08.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    "Traemos Datos de Impuesto Detemrinado
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_calcu, lv_monto_aux.
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_pprovisacumpagadosant.
    "Traemos ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc  WITH KEY txt50 = c_acredisrretacum.
    "Traemos ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR En Fiels Symbol Para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_9> WITH KEY txt50 = c_isrpagperdesacredit.
    lv_monto_aux = wa_report_calcu-hsl09.
    IF lv_monto_aux LT 0.
      lv_monto_aux = lv_monto_aux * -1.
    ENDIF.
    <fs_report_cal_9>-hsl09 = wa_report_cal_aux-hsl09 - lv_monto_aux - vg_isr_ret_09 . "- wa_report_calc-hsl09.

    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    "Traemos ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO ) Asignando a FS
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_9> WITH KEY txt50 = c_acumisrdespacdt.
    <fs_report_cal_9>-hsl09 = <fs_report_cal_9>-hsl08 + wa_report_cal-hsl09.


**************************************PARA OCTUBRE (HSL10)***************************************************************

    "---------------------------PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD----------------------------------
    CLEAR wa_report_cal.
    "Traemos dato de ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD en FS para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_10> WITH KEY txt50 = c_pprovisacumpagadosant.
    <fs_report_cal_10>-hsl10 = wa_report_cal-hsl09.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    "Traemos Datos de Impuesto Detemrinado
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_calcu, lv_monto_aux.
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_pprovisacumpagadosant.
    "Traemos ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc  WITH KEY txt50 = c_acredisrretacum.
    "Traemos ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR En Fiels Symbol Para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_10> WITH KEY txt50 = c_isrpagperdesacredit.
    lv_monto_aux = wa_report_calcu-hsl10.
    IF lv_monto_aux LT 0.
      lv_monto_aux = lv_monto_aux * -1.
    ENDIF.
    <fs_report_cal_10>-hsl10 = wa_report_cal_aux-hsl10 - lv_monto_aux - vg_isr_ret_10. "- wa_report_calc-hsl10.

    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    "Traemos ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO ) Asignando a FS
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_10> WITH KEY txt50 = c_acumisrdespacdt.
    <fs_report_cal_10>-hsl10 = <fs_report_cal_10>-hsl09 + wa_report_cal-hsl10.


**************************************PARA NOVIEMBRE (HSL11)***************************************************************

    "---------------------------PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD----------------------------------
    CLEAR wa_report_cal.
    "Traemos dato de ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD en FS para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_11> WITH KEY txt50 = c_pprovisacumpagadosant.
    <fs_report_cal_11>-hsl11 = wa_report_cal-hsl10.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    "Traemos Datos de Impuesto Detemrinado
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_calcu, lv_monto_aux.
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_pprovisacumpagadosant.
    "Traemos ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc  WITH KEY txt50 = c_acredisrretacum.
    "Traemos ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR En Fiels Symbol Para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_11> WITH KEY txt50 = c_isrpagperdesacredit.
    lv_monto_aux = wa_report_calcu-hsl11.
    IF lv_monto_aux LT 0.
      lv_monto_aux = lv_monto_aux * -1.
    ENDIF.
    <fs_report_cal_11>-hsl11 = wa_report_cal_aux-hsl11 - lv_monto_aux - vg_isr_ret_11. "- wa_report_calc-hsl11.

    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    "Traemos ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO ) Asignando a FS
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_11> WITH KEY txt50 = c_acumisrdespacdt.
    <fs_report_cal_11>-hsl11 = <fs_report_cal_11>-hsl10 + wa_report_cal-hsl11.


**************************************PARA DICIEMBRE (HSL12)***************************************************************

    "---------------------------PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD----------------------------------
    CLEAR wa_report_cal.
    "Traemos dato de ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD en FS para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_12> WITH KEY txt50 = c_pprovisacumpagadosant.
    <fs_report_cal_12>-hsl12 = wa_report_cal-hsl11.

    "-------------------ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR---------------------
    "Traemos Datos de Impuesto Detemrinado
    CLEAR: wa_report_cal_aux, wa_report_calc, wa_report_calcu, lv_monto_aux.
    READ TABLE it_report_cal INTO wa_report_cal_aux WITH KEY txt50 = c_impuesto_determinado.
    "Traemos PAGOS PROVIS. ACUM PAGADOS CON ANTERIORIDAD
    READ TABLE it_report_cal INTO wa_report_calcu WITH KEY txt50 = c_pprovisacumpagadosant.
    "Traemos ACREDITAMIENTO DE ISR RETENIDO ACUMULADO
*    READ TABLE it_report_cal INTO wa_report_calc  WITH KEY txt50 = c_acredisrretacum.
    "Traemos ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR En Fiels Symbol Para actualizar
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_12> WITH KEY txt50 = c_isrpagperdesacredit.
    lv_monto_aux = wa_report_calcu-hsl12.
    IF lv_monto_aux LT 0.
      lv_monto_aux = lv_monto_aux * -1.
    ENDIF.
    <fs_report_cal_12>-hsl12 = wa_report_cal_aux-hsl12 - lv_monto_aux - vg_isr_ret_12 . "- wa_report_calc-hsl12.

    "---------------ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO )--------------
    "Traemos dato de ISR A PAGAR  PERIODO  DESPUES DE ACREDITAMIENTOS PP Y  RET ISR
    CLEAR wa_report_cal.
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    "Traemos ACUM ISR DESPUES DE ACREDITAMIENTO DE PP Y RET  DE ISR ( DATO  INFORMATIVO ) Asignando a FS
    READ TABLE it_report_cal ASSIGNING <fs_report_cal_12> WITH KEY txt50 = c_acumisrdespacdt.
    <fs_report_cal_12>-hsl12 = <fs_report_cal_12>-hsl11 + wa_report_cal-hsl12.

    "RETENCIONES DE ISR
    "Rango para cuentas de RETENCIONES ISR
    CLEAR rwa_racct_re_isr.
    REFRESH rg_racct_re_isr.

    rwa_racct_re_isr-sign    = 'I'.          " I : Incluir
    rwa_racct_re_isr-option  = 'BT'.         " BT: Entre
    rwa_racct_re_isr-low     = '1000000000'. " Cuenta
    rwa_racct_re_isr-high    = '3999999999'. " A Cuenta
    APPEND rwa_racct_re_isr TO rg_racct_re_isr.

    CLEAR: wa_report,
           wa_report_cal.

    wa_report_ret-txt50 = c_retenciones_de_isr.
    APPEND wa_report_ret TO it_report_ret.

    DELETE it_report_aux WHERE racct IN rg_isr_rete.    "ADD 25/02/2021 CJPG VARR
    DELETE it_report_aux WHERE racct IN rg_cuenta_anticipo.

    LOOP AT it_report_aux INTO wa_report.
      "vg_ret = wa_report-racct(4).
      "IF vg_ret EQ c_2105.
      IF wa_report-racct IN rg_racct_re_isr.

        CLEAR wa_fmglflext.
        READ TABLE it_fmglflext_aux INTO wa_fmglflext_aux WITH KEY racct = wa_report-racct.

        IF  sy-subrc EQ 0.

          wa_report_ret-txt50 = wa_report-txt50.
          wa_report_ret-racct = wa_report-racct.
          wa_report_ret-hsl01 = wa_fmglflext_aux-hslvt  + wa_fmglflext_aux-hsl01.
          wa_report_ret-hsl02 = wa_report_ret-hsl01     + wa_fmglflext_aux-hsl02.
          wa_report_ret-hsl03 = wa_report_ret-hsl02     + wa_fmglflext_aux-hsl03.
          wa_report_ret-hsl04 = wa_report_ret-hsl03     + wa_fmglflext_aux-hsl04.
          wa_report_ret-hsl05 = wa_report_ret-hsl04     + wa_fmglflext_aux-hsl05.
          wa_report_ret-hsl06 = wa_report_ret-hsl05     + wa_fmglflext_aux-hsl06.
          wa_report_ret-hsl07 = wa_report_ret-hsl06     + wa_fmglflext_aux-hsl07.
          wa_report_ret-hsl08 = wa_report_ret-hsl07     + wa_fmglflext_aux-hsl08.
          wa_report_ret-hsl09 = wa_report_ret-hsl08     + wa_fmglflext_aux-hsl09.
          wa_report_ret-hsl10 = wa_report_ret-hsl09     + wa_fmglflext_aux-hsl10.
          wa_report_ret-hsl11 = wa_report_ret-hsl10     + wa_fmglflext_aux-hsl11.
          wa_report_ret-hsl12 = wa_report_ret-hsl11     + wa_fmglflext_aux-hsl12.

          IF wa_report_ret-racct(2) EQ '21'. "ADD VARR si la cuenta inicia con 21, se vuelven sus valores positivos

            IF wa_report_ret-hsl01 LT 0.
              wa_report_ret-hsl01 = wa_report_ret-hsl01 * -1.
            ENDIF.
            IF wa_report_ret-hsl02 LT 0.
              wa_report_ret-hsl02 = wa_report_ret-hsl02 * -1.
            ENDIF.
            IF wa_report_ret-hsl03 LT 0.
              wa_report_ret-hsl03 = wa_report_ret-hsl03 * -1.
            ENDIF.
            IF wa_report_ret-hsl04 LT 0.
              wa_report_ret-hsl04 = wa_report_ret-hsl04 * -1.
            ENDIF.
            IF wa_report_ret-hsl05 LT 0.
              wa_report_ret-hsl05 = wa_report_ret-hsl05 * -1.
            ENDIF.
            IF wa_report_ret-hsl06 LT 0.
              wa_report_ret-hsl06 = wa_report_ret-hsl06 * -1.
            ENDIF.
            IF wa_report_ret-hsl07 LT 0.
              wa_report_ret-hsl07 = wa_report_ret-hsl07 * -1.
            ENDIF.
            IF wa_report_ret-hsl08 LT 0.
              wa_report_ret-hsl08 = wa_report_ret-hsl08 * -1.
            ENDIF.
            IF wa_report_ret-hsl09 LT 0.
              wa_report_ret-hsl09 = wa_report_ret-hsl09 * -1.
            ENDIF.
            IF wa_report_ret-hsl10 LT 0.
              wa_report_ret-hsl10 = wa_report_ret-hsl10 * -1.
            ENDIF.
            IF wa_report_ret-hsl11 LT 0.
              wa_report_ret-hsl11 = wa_report_ret-hsl11 * -1.
            ENDIF.
            IF wa_report_ret-hsl12 LT 0.
              wa_report_ret-hsl12 = wa_report_ret-hsl12 * -1.
            ENDIF.
          ENDIF.

*          vg_tot_ret = wa_report_ret-hsl01 + wa_report_ret-hsl02 + wa_report_ret-hsl03 + wa_report_ret-hsl04 +
*                       wa_report_ret-hsl05 + wa_report_ret-hsl06 + wa_report_ret-hsl07 + wa_report_ret-hsl08 +
*                       wa_report_ret-hsl09 + wa_report_ret-hsl10 + wa_report_ret-hsl11 +  wa_report_ret-hsl12.

          "INI ADD 26/02/2021 CJPG VARR
          IF wa_report_ret-hsl01 IS NOT INITIAL.
            vg_tot_ret = wa_report_ret-hsl01.
          ENDIF.

          IF  wa_report_ret-hsl02 IS NOT INITIAL.
            CLEAR vg_tot_ret.
            vg_tot_ret = wa_report_ret-hsl02.
          ENDIF.

          IF wa_report_ret-hsl03 IS NOT INITIAL.
            CLEAR vg_tot_ret.
            vg_tot_ret = wa_report_ret-hsl03.
          ENDIF.

          IF wa_report_ret-hsl04 IS NOT INITIAL.
            CLEAR vg_tot_ret.
            vg_tot_ret = wa_report_ret-hsl04.
          ENDIF.

          IF wa_report_ret-hsl05 IS NOT INITIAL.
            CLEAR vg_tot_ret.
            vg_tot_ret = wa_report_ret-hsl05.
          ENDIF.

          IF wa_report_ret-hsl06 IS NOT INITIAL.
            CLEAR vg_tot_ret.
            vg_tot_ret = wa_report_ret-hsl06.
          ENDIF.

          IF wa_report_ret-hsl07 IS NOT INITIAL.
            CLEAR vg_tot_ret.
            vg_tot_ret = wa_report_ret-hsl07.
          ENDIF.

          IF wa_report_ret-hsl08 IS NOT INITIAL.
            CLEAR vg_tot_ret.
            vg_tot_ret = wa_report_ret-hsl08.
          ENDIF.

          IF wa_report_ret-hsl09 IS NOT INITIAL.
            CLEAR vg_tot_ret.
            vg_tot_ret = wa_report_ret-hsl09.
          ENDIF.

          IF wa_report_ret-hsl10 IS NOT INITIAL.
            CLEAR vg_tot_ret.
            vg_tot_ret = wa_report_ret-hsl10.
          ENDIF.

          IF wa_report_ret-hsl11 IS NOT INITIAL.
            CLEAR vg_tot_ret.
            vg_tot_ret = wa_report_ret-hsl11.
          ENDIF.

          IF wa_report_ret-hsl12 IS NOT INITIAL.
            CLEAR vg_tot_ret.
            vg_tot_ret = wa_report_ret-hsl12.
          ENDIF.

          wa_report_ret-total =  vg_tot_ret.
          "FIN ADD 26/02/2021 CJPG VARR

          APPEND  wa_report_ret TO it_report_ret.
          CLEAR: wa_report, wa_report_ret.
        ENDIF.

*      ELSEIF wa_report-racct EQ '1163020000'.
*
*        wa_report_ret-txt50 = wa_report-txt50.
*        wa_report_ret-racct = wa_report-racct.
*        wa_report_ret-hsl01 = wa_report-hsl01.
*        wa_report_ret-hsl02 = wa_report-hsl02.
*        wa_report_ret-hsl03 = wa_report-hsl03.
*        wa_report_ret-hsl04 = wa_report-hsl04.
*        wa_report_ret-hsl05 = wa_report-hsl05.
*        wa_report_ret-hsl06 = wa_report-hsl06.
*        wa_report_ret-hsl07 = wa_report-hsl07.
*        wa_report_ret-hsl08 = wa_report-hsl08.
*        wa_report_ret-hsl09 = wa_report-hsl09.
*        wa_report_ret-hsl10 = wa_report-hsl10.
*        wa_report_ret-hsl11 = wa_report-hsl11.
*        wa_report_ret-hsl12 = wa_report-hsl12.
*
*        vg_tot_ret = wa_report_ret-hsl01 + wa_report_ret-hsl02 + wa_report_ret-hsl03 + wa_report_ret-hsl04 +
*                     wa_report_ret-hsl05 + wa_report_ret-hsl06 + wa_report_ret-hsl07 + wa_report_ret-hsl08 +
*                     wa_report_ret-hsl09 + wa_report_ret-hsl10 + wa_report_ret-hsl11 +  wa_report_ret-hsl12.
*
*        wa_report_ret-total =  vg_tot_ret.
*
*        APPEND  wa_report_ret TO it_report_ret.
*        CLEAR: wa_report, wa_report_ret.

      ENDIF.
    ENDLOOP.
    "TOTAL RETENCIONES ISR

    CLEAR: vg_tot1,
           vg_tot2,
           vg_tot3,
           vg_tot4,
           vg_tot5,
           vg_tot6,
           vg_tot7,
           vg_tot8,
           vg_tot9,
           vg_tot10,
           vg_tot11,
           vg_tot12,
           vg_total_var.


    LOOP AT it_report_ret INTO wa_report_ret.

      vg_tot1  = vg_tot1  + wa_report_ret-hsl01.
      vg_tot2  = vg_tot2  + wa_report_ret-hsl02.
      vg_tot3  = vg_tot3  + wa_report_ret-hsl03.
      vg_tot4  = vg_tot4  + wa_report_ret-hsl04.
      vg_tot5  = vg_tot5  + wa_report_ret-hsl05.
      vg_tot6  = vg_tot6  + wa_report_ret-hsl06.
      vg_tot7  = vg_tot7  + wa_report_ret-hsl07.
      vg_tot8  = vg_tot8  + wa_report_ret-hsl08.
      vg_tot9  = vg_tot9  + wa_report_ret-hsl09.
      vg_tot10 = vg_tot10 + wa_report_ret-hsl10.
      vg_tot11 = vg_tot11 + wa_report_ret-hsl11.
      vg_tot12 = vg_tot12 + wa_report_ret-hsl12.

    ENDLOOP.
    CLEAR: wa_report_ret.
    wa_report_ret-txt50 = c_tot_ret_isr.
    wa_report_ret-hsl01 = vg_tot1.
    wa_report_ret-hsl02 = vg_tot2.
    wa_report_ret-hsl03 = vg_tot3.
    wa_report_ret-hsl04 = vg_tot4.
    wa_report_ret-hsl05 = vg_tot5.
    wa_report_ret-hsl06 = vg_tot6.
    wa_report_ret-hsl07 = vg_tot7.
    wa_report_ret-hsl08 = vg_tot8.
    wa_report_ret-hsl09 = vg_tot9.
    wa_report_ret-hsl10 = vg_tot10.
    wa_report_ret-hsl11 = vg_tot11.
    wa_report_ret-hsl12 = vg_tot12.

    vg_total_var = vg_tot1 + vg_tot2 + vg_tot3 + vg_tot4 + vg_tot5 + vg_tot6 + vg_tot7 + vg_tot8 + vg_tot9 +
                   vg_tot10 + vg_tot11 + vg_tot12.

    wa_report_ret-total = vg_total_var.

    APPEND wa_report_ret TO it_report_ret.

    "Se Pasan las 3 tablas internas a una sola al ALV (IT_REPORT, IT_REPORT_CAL, IT_REPORT_RET).
    CLEAR: wa_report,
           wa_report_alv.

    "Proceso para Anicipos V6 VARR 04/03/2021
    IF it_acdoca_aux IS NOT INITIAL.
      CLEAR: wa_report_anticipo,
             wa_report_alv,
             wa_acdoca_aux,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      LOOP AT it_acdoca_aux INTO wa_acdoca_aux.
        READ TABLE it_report_anticipo INTO wa_report_anticipo WITH KEY racct = wa_acdoca_aux-racct.
        IF sy-subrc EQ 0.

          wa_report_alv-racct = wa_report_anticipo-racct.
          wa_report_alv-txt50 = wa_report_anticipo-txt50.


          WRITE: wa_report_anticipo-hsl01 TO vl_hsl01,
                 wa_report_anticipo-hsl02 TO vl_hsl02,
                 wa_report_anticipo-hsl03 TO vl_hsl03,
                 wa_report_anticipo-hsl04 TO vl_hsl04,
                 wa_report_anticipo-hsl05 TO vl_hsl05,
                 wa_report_anticipo-hsl06 TO vl_hsl06,
                 wa_report_anticipo-hsl07 TO vl_hsl07,
                 wa_report_anticipo-hsl08 TO vl_hsl08,
                 wa_report_anticipo-hsl09 TO vl_hsl09,
                 wa_report_anticipo-hsl10 TO vl_hsl10,
                 wa_report_anticipo-hsl11 TO vl_hsl11,
                 wa_report_anticipo-hsl12 TO vl_hsl12,
                 wa_report_anticipo-total TO vl_total.

          wa_report_alv-hsl01 = vl_hsl01.
          wa_report_alv-hsl02 = vl_hsl02.
          wa_report_alv-hsl03 = vl_hsl03.
          wa_report_alv-hsl04 = vl_hsl04.
          wa_report_alv-hsl05 = vl_hsl05.
          wa_report_alv-hsl06 = vl_hsl06.
          wa_report_alv-hsl07 = vl_hsl07.
          wa_report_alv-hsl08 = vl_hsl08.
          wa_report_alv-hsl09 = vl_hsl09.
          wa_report_alv-hsl10 = vl_hsl10.
          wa_report_alv-hsl11 = vl_hsl11.
          wa_report_alv-hsl12 = vl_hsl12.
          wa_report_alv-total = vl_total.

          CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                    wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                    wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.


          APPEND wa_report_alv TO it_report_alv.

        ENDIF.

      ENDLOOP.
    ENDIF.

    IF it_report IS NOT INITIAL.
      LOOP AT it_report INTO wa_report.

        CLEAR: vl_hsl01,
               vl_hsl01,
               vl_hsl02,
               vl_hsl03,
               vl_hsl04,
               vl_hsl05,
               vl_hsl06,
               vl_hsl07,
               vl_hsl08,
               vl_hsl09,
               vl_hsl10,
               vl_hsl11,
               vl_hsl12,
               vl_total.

        wa_report_alv-racct = wa_report-racct.
        wa_report_alv-txt50 = wa_report-txt50.


        WRITE: wa_report-hsl01 TO vl_hsl01,
               wa_report-hsl02 TO vl_hsl02,
               wa_report-hsl03 TO vl_hsl03,
               wa_report-hsl04 TO vl_hsl04,
               wa_report-hsl05 TO vl_hsl05,
               wa_report-hsl06 TO vl_hsl06,
               wa_report-hsl07 TO vl_hsl07,
               wa_report-hsl08 TO vl_hsl08,
               wa_report-hsl09 TO vl_hsl09,
               wa_report-hsl10 TO vl_hsl10,
               wa_report-hsl11 TO vl_hsl11,
               wa_report-hsl12 TO vl_hsl12,
               wa_report-total TO vl_total.

        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.
        wa_report_alv-total = vl_total.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.


        APPEND wa_report_alv TO it_report_alv.

      ENDLOOP.
    ENDIF.

    IF it_report_cal IS NOT INITIAL.
      CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_ingresos_nominales_mensuales.
      IF sy-subrc EQ 0.

        wa_report_alv-txt50 = wa_report_cal-txt50.

        WRITE: wa_report_cal-hsl01 TO vl_hsl01,
               wa_report_cal-hsl02 TO vl_hsl02,
               wa_report_cal-hsl03 TO vl_hsl03,
               wa_report_cal-hsl04 TO vl_hsl04,
               wa_report_cal-hsl05 TO vl_hsl05,
               wa_report_cal-hsl06 TO vl_hsl06,
               wa_report_cal-hsl07 TO vl_hsl07,
               wa_report_cal-hsl08 TO vl_hsl08,
               wa_report_cal-hsl09 TO vl_hsl09,
               wa_report_cal-hsl10 TO vl_hsl10,
               wa_report_cal-hsl11 TO vl_hsl11,
               wa_report_cal-hsl12 TO vl_hsl12,
               wa_report_cal-total TO vl_total.


        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.
        wa_report_alv-total = vl_total.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.
      ENDIF.

      CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_ing_nominales_acumulados.
      IF sy-subrc EQ 0.

        wa_report_alv-txt50 = wa_report_cal-txt50.

        WRITE: wa_report_cal-hsl01 TO vl_hsl01,
               wa_report_cal-hsl02 TO vl_hsl02,
               wa_report_cal-hsl03 TO vl_hsl03,
               wa_report_cal-hsl04 TO vl_hsl04,
               wa_report_cal-hsl05 TO vl_hsl05,
               wa_report_cal-hsl06 TO vl_hsl06,
               wa_report_cal-hsl07 TO vl_hsl07,
               wa_report_cal-hsl08 TO vl_hsl08,
               wa_report_cal-hsl09 TO vl_hsl09,
               wa_report_cal-hsl10 TO vl_hsl10,
               wa_report_cal-hsl11 TO vl_hsl11,
               wa_report_cal-hsl12 TO vl_hsl12.


        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.
      ENDIF.

      CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco
      "Coeficiente de Utilidad
      READ TABLE it_report_cal_dec INTO wa_report_cal_dec WITH KEY txt50 = c_coeficiente_de_utilidad.
      IF sy-subrc EQ 0.

        wa_report_alv-txt50 = wa_report_cal_dec-txt50.

        WRITE: wa_report_cal_dec-hsl01 TO vl_hsl01,
               wa_report_cal_dec-hsl02 TO vl_hsl02,
               wa_report_cal_dec-hsl03 TO vl_hsl03,
               wa_report_cal_dec-hsl04 TO vl_hsl04,
               wa_report_cal_dec-hsl05 TO vl_hsl05,
               wa_report_cal_dec-hsl06 TO vl_hsl06,
               wa_report_cal_dec-hsl07 TO vl_hsl07,
               wa_report_cal_dec-hsl08 TO vl_hsl08,
               wa_report_cal_dec-hsl09 TO vl_hsl09,
               wa_report_cal_dec-hsl10 TO vl_hsl10,
               wa_report_cal_dec-hsl11 TO vl_hsl11,
               wa_report_cal_dec-hsl12 TO vl_hsl12.
*               wa_report_cal_dec-total TO vl_total.

        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.
*        wa_report_alv-total = vl_total.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12 NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.
      ENDIF.

      CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco
      "Utilidad Fiscal
      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_utilidad_fiscal.
      IF sy-subrc EQ 0.

        wa_report_alv-txt50 = wa_report_cal-txt50.

        WRITE: wa_report_cal-hsl01 TO vl_hsl01,
               wa_report_cal-hsl02 TO vl_hsl02,
               wa_report_cal-hsl03 TO vl_hsl03,
               wa_report_cal-hsl04 TO vl_hsl04,
               wa_report_cal-hsl05 TO vl_hsl05,
               wa_report_cal-hsl06 TO vl_hsl06,
               wa_report_cal-hsl07 TO vl_hsl07,
               wa_report_cal-hsl08 TO vl_hsl08,
               wa_report_cal-hsl09 TO vl_hsl09,
               wa_report_cal-hsl10 TO vl_hsl10,
               wa_report_cal-hsl11 TO vl_hsl11,
               wa_report_cal-hsl12 TO vl_hsl12.


        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.
      ENDIF.

      CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco

      "PTU pagada

      LOOP AT it_report_cal INTO wa_report_cal WHERE txt50 IN R_TXT50.

        wa_report_alv-txt50 = wa_report_cal-txt50.

        WRITE: wa_report_cal-hsl01 TO vl_hsl01,
               wa_report_cal-hsl02 TO vl_hsl02,
               wa_report_cal-hsl03 TO vl_hsl03,
               wa_report_cal-hsl04 TO vl_hsl04,
               wa_report_cal-hsl05 TO vl_hsl05,
               wa_report_cal-hsl06 TO vl_hsl06,
               wa_report_cal-hsl07 TO vl_hsl07,
               wa_report_cal-hsl08 TO vl_hsl08,
               wa_report_cal-hsl09 TO vl_hsl09,
               wa_report_cal-hsl10 TO vl_hsl10,
               wa_report_cal-hsl11 TO vl_hsl11,
               wa_report_cal-hsl12 TO vl_hsl12,
               wa_report_cal-total TO vl_total.


        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.
        wa_report_alv-total = vl_total.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.

      ENDLOOP.

*      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = C_PTU_PAGADA.
*      IF sy-subrc EQ 0.
*
*        wa_report_alv-txt50 = wa_report_cal-txt50.
*
*        WRITE: wa_report_cal-hsl01 TO vl_hsl01,
*               wa_report_cal-hsl02 TO vl_hsl02,
*               wa_report_cal-hsl03 TO vl_hsl03,
*               wa_report_cal-hsl04 TO vl_hsl04,
*               wa_report_cal-hsl05 TO vl_hsl05,
*               wa_report_cal-hsl06 TO vl_hsl06,
*               wa_report_cal-hsl07 TO vl_hsl07,
*               wa_report_cal-hsl08 TO vl_hsl08,
*               wa_report_cal-hsl09 TO vl_hsl09,
*               wa_report_cal-hsl10 TO vl_hsl10,
*               wa_report_cal-hsl11 TO vl_hsl11,
*               wa_report_cal-hsl12 TO vl_hsl12,
*               wa_report_cal-total TO vl_total.
*
*
*        wa_report_alv-hsl01 = vl_hsl01.
*        wa_report_alv-hsl02 = vl_hsl02.
*        wa_report_alv-hsl03 = vl_hsl03.
*        wa_report_alv-hsl04 = vl_hsl04.
*        wa_report_alv-hsl05 = vl_hsl05.
*        wa_report_alv-hsl06 = vl_hsl06.
*        wa_report_alv-hsl07 = vl_hsl07.
*        wa_report_alv-hsl08 = vl_hsl08.
*        wa_report_alv-hsl09 = vl_hsl09.
*        wa_report_alv-hsl10 = vl_hsl10.
*        wa_report_alv-hsl11 = vl_hsl11.
*        wa_report_alv-hsl12 = vl_hsl12.
*        wa_report_alv-total = vl_total.
*
*        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
*                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
*                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.
*
*        APPEND wa_report_alv TO it_report_alv.
*      ENDIF.

      CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco

      "Factor Resultado

      CONCATENATE 'FACTOR RESULTADO:'
                  SV_FACTOR_RESULTADO_AUX
             INTO wa_report_alv-txt50
             SEPARATED BY SPACE.

      APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco

      CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      "Perdidas amortizada

      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_perdidas_amortizada.
      IF sy-subrc EQ 0.

        wa_report_alv-txt50 = wa_report_cal-txt50.

        WRITE: wa_report_cal-hsl01 TO vl_hsl01,
               wa_report_cal-hsl02 TO vl_hsl02,
               wa_report_cal-hsl03 TO vl_hsl03,
               wa_report_cal-hsl04 TO vl_hsl04,
               wa_report_cal-hsl05 TO vl_hsl05,
               wa_report_cal-hsl06 TO vl_hsl06,
               wa_report_cal-hsl07 TO vl_hsl07,
               wa_report_cal-hsl08 TO vl_hsl08,
               wa_report_cal-hsl09 TO vl_hsl09,
               wa_report_cal-hsl10 TO vl_hsl10,
               wa_report_cal-hsl11 TO vl_hsl11,
               wa_report_cal-hsl12 TO vl_hsl12,
               wa_report_cal-total TO vl_total.


        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.
        wa_report_alv-total = vl_total.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.
      ENDIF.

CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.
      "Perdidas por Amortizar

      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_perdidas_por_amortizar.
      IF sy-subrc EQ 0.

        wa_report_alv-txt50 = wa_report_cal-txt50.

        WRITE: wa_report_cal-hsl01 TO vl_hsl01,
               wa_report_cal-hsl02 TO vl_hsl02,
               wa_report_cal-hsl03 TO vl_hsl03,
               wa_report_cal-hsl04 TO vl_hsl04,
               wa_report_cal-hsl05 TO vl_hsl05,
               wa_report_cal-hsl06 TO vl_hsl06,
               wa_report_cal-hsl07 TO vl_hsl07,
               wa_report_cal-hsl08 TO vl_hsl08,
               wa_report_cal-hsl09 TO vl_hsl09,
               wa_report_cal-hsl10 TO vl_hsl10,
               wa_report_cal-hsl11 TO vl_hsl11,
               wa_report_cal-hsl12 TO vl_hsl12,
               wa_report_cal-total TO vl_total.


        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.
        wa_report_alv-total = vl_total.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.
      ENDIF.


      CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco
      "Estimulo Fiscal PTU del mes
      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_estimulo_fiscal_ptu_del_mes.
      IF sy-subrc EQ 0.

        wa_report_alv-txt50 = wa_report_cal-txt50.

        WRITE: wa_report_cal-hsl01 TO vl_hsl01,
              wa_report_cal-hsl02 TO vl_hsl02,
              wa_report_cal-hsl03 TO vl_hsl03,
              wa_report_cal-hsl04 TO vl_hsl04,
              wa_report_cal-hsl05 TO vl_hsl05,
              wa_report_cal-hsl06 TO vl_hsl06,
              wa_report_cal-hsl07 TO vl_hsl07,
              wa_report_cal-hsl08 TO vl_hsl08,
              wa_report_cal-hsl09 TO vl_hsl09,
              wa_report_cal-hsl10 TO vl_hsl10,
              wa_report_cal-hsl11 TO vl_hsl11,
              wa_report_cal-hsl12 TO vl_hsl12.


        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.
      ENDIF.

      CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      " Estimulo Fiscal PTU Acumulado
      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_estimulofiscalptuacumulado.
      IF sy-subrc EQ 0.

        wa_report_alv-txt50 = wa_report_cal-txt50.

        WRITE: wa_report_cal-hsl01 TO vl_hsl01,
               wa_report_cal-hsl02 TO vl_hsl02,
               wa_report_cal-hsl03 TO vl_hsl03,
               wa_report_cal-hsl04 TO vl_hsl04,
               wa_report_cal-hsl05 TO vl_hsl05,
               wa_report_cal-hsl06 TO vl_hsl06,
               wa_report_cal-hsl07 TO vl_hsl07,
               wa_report_cal-hsl08 TO vl_hsl08,
               wa_report_cal-hsl09 TO vl_hsl09,
               wa_report_cal-hsl10 TO vl_hsl10,
               wa_report_cal-hsl11 TO vl_hsl11,
               wa_report_cal-hsl12 TO vl_hsl12.


        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.
      ENDIF.

      CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco

      "Resultado Fiscal
      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_resultado_fiscal.
      IF sy-subrc EQ 0.

        wa_report_alv-txt50 = wa_report_cal-txt50.

        WRITE: wa_report_cal-hsl01 TO vl_hsl01,
              wa_report_cal-hsl02 TO vl_hsl02,
              wa_report_cal-hsl03 TO vl_hsl03,
              wa_report_cal-hsl04 TO vl_hsl04,
              wa_report_cal-hsl05 TO vl_hsl05,
              wa_report_cal-hsl06 TO vl_hsl06,
              wa_report_cal-hsl07 TO vl_hsl07,
              wa_report_cal-hsl08 TO vl_hsl08,
              wa_report_cal-hsl09 TO vl_hsl09,
              wa_report_cal-hsl10 TO vl_hsl10,
              wa_report_cal-hsl11 TO vl_hsl11,
              wa_report_cal-hsl12 TO vl_hsl12.


        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.
      ENDIF.

      CLEAR:  wa_report_cal,
              wa_report_alv,
              vl_hsl01,
              vl_hsl01,
              vl_hsl02,
              vl_hsl03,
              vl_hsl04,
              vl_hsl05,
              vl_hsl06,
              vl_hsl07,
              vl_hsl08,
              vl_hsl09,
              vl_hsl10,
              vl_hsl11,
              vl_hsl12,
              vl_total.

      APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco
      "Tasa
      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_tasa.

      IF sy-subrc EQ 0.

        wa_report_alv-txt50 = wa_report_cal-txt50.

        WRITE: wa_report_cal-hsl01 TO vl_hsl01,
               wa_report_cal-hsl02 TO vl_hsl02,
               wa_report_cal-hsl03 TO vl_hsl03,
               wa_report_cal-hsl04 TO vl_hsl04,
               wa_report_cal-hsl05 TO vl_hsl05,
               wa_report_cal-hsl06 TO vl_hsl06,
               wa_report_cal-hsl07 TO vl_hsl07,
               wa_report_cal-hsl08 TO vl_hsl08,
               wa_report_cal-hsl09 TO vl_hsl09,
               wa_report_cal-hsl10 TO vl_hsl10,
               wa_report_cal-hsl11 TO vl_hsl11,
               wa_report_cal-hsl12 TO vl_hsl12.


        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.
      ENDIF.

      CLEAR: wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

      APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco
      "Impuesto determinado
      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_impuesto_determinado.
      IF sy-subrc EQ 0.

        wa_report_alv-txt50 = wa_report_cal-txt50.

        WRITE:  wa_report_cal-hsl01 TO vl_hsl01,
                wa_report_cal-hsl02 TO vl_hsl02,
                wa_report_cal-hsl03 TO vl_hsl03,
                wa_report_cal-hsl04 TO vl_hsl04,
                wa_report_cal-hsl05 TO vl_hsl05,
                wa_report_cal-hsl06 TO vl_hsl06,
                wa_report_cal-hsl07 TO vl_hsl07,
                wa_report_cal-hsl08 TO vl_hsl08,
                wa_report_cal-hsl09 TO vl_hsl09,
                wa_report_cal-hsl10 TO vl_hsl10,
                wa_report_cal-hsl11 TO vl_hsl11,
                wa_report_cal-hsl12 TO vl_hsl12.


        wa_report_alv-hsl01 = vl_hsl01.
        wa_report_alv-hsl02 = vl_hsl02.
        wa_report_alv-hsl03 = vl_hsl03.
        wa_report_alv-hsl04 = vl_hsl04.
        wa_report_alv-hsl05 = vl_hsl05.
        wa_report_alv-hsl06 = vl_hsl06.
        wa_report_alv-hsl07 = vl_hsl07.
        wa_report_alv-hsl08 = vl_hsl08.
        wa_report_alv-hsl09 = vl_hsl09.
        wa_report_alv-hsl10 = vl_hsl10.
        wa_report_alv-hsl11 = vl_hsl11.
        wa_report_alv-hsl12 = vl_hsl12.

        CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                  wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                  wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

        APPEND wa_report_alv TO it_report_alv.
      ENDIF.

      CLEAR: wa_report_cal,
              wa_report_alv,
              vl_hsl01,
              vl_hsl01,
              vl_hsl02,
              vl_hsl03,
              vl_hsl04,
              vl_hsl05,
              vl_hsl06,
              vl_hsl07,
              vl_hsl08,
              vl_hsl09,
              vl_hsl10,
              vl_hsl11,
              vl_hsl12,
              vl_total.

      APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco
      "Pagos Provis. Acum Pagados con Anterioridad
      READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_pprovisacumpagadosant.

      wa_report_alv-txt50 = wa_report_cal-txt50.

      WRITE: wa_report_cal-hsl01 TO vl_hsl01,
              wa_report_cal-hsl02 TO vl_hsl02,
              wa_report_cal-hsl03 TO vl_hsl03,
              wa_report_cal-hsl04 TO vl_hsl04,
              wa_report_cal-hsl05 TO vl_hsl05,
              wa_report_cal-hsl06 TO vl_hsl06,
              wa_report_cal-hsl07 TO vl_hsl07,
              wa_report_cal-hsl08 TO vl_hsl08,
              wa_report_cal-hsl09 TO vl_hsl09,
              wa_report_cal-hsl10 TO vl_hsl10,
              wa_report_cal-hsl11 TO vl_hsl11,
              wa_report_cal-hsl12 TO vl_hsl12.


      wa_report_alv-hsl01 = vl_hsl01.
      wa_report_alv-hsl02 = vl_hsl02.
      wa_report_alv-hsl03 = vl_hsl03.
      wa_report_alv-hsl04 = vl_hsl04.
      wa_report_alv-hsl05 = vl_hsl05.
      wa_report_alv-hsl06 = vl_hsl06.
      wa_report_alv-hsl07 = vl_hsl07.
      wa_report_alv-hsl08 = vl_hsl08.
      wa_report_alv-hsl09 = vl_hsl09.
      wa_report_alv-hsl10 = vl_hsl10.
      wa_report_alv-hsl11 = vl_hsl11.
      wa_report_alv-hsl12 = vl_hsl12.

      CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

      APPEND wa_report_alv TO it_report_alv.
    ENDIF.

*    CLEAR: wa_report_cal,
*               wa_report_alv,
*               vl_hsl01,
*               vl_hsl01,
*               vl_hsl02,
*               vl_hsl03,
*               vl_hsl04,
*               vl_hsl05,
*               vl_hsl06,
*               vl_hsl07,
*               vl_hsl08,
*               vl_hsl09,
*               vl_hsl10,
*               vl_hsl11,
*               vl_hsl12,
*               vl_total.
*
*    APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco
    "Acreditamiento de ISR Mensual
*    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acreddeisrretmen.
*    IF sy-subrc EQ 0.
*
*      wa_report_alv-txt50 = wa_report_cal-txt50.
*
*      WRITE: wa_report_cal-hsl01 TO vl_hsl01,
*            wa_report_cal-hsl02 TO vl_hsl02,
*            wa_report_cal-hsl03 TO vl_hsl03,
*            wa_report_cal-hsl04 TO vl_hsl04,
*            wa_report_cal-hsl05 TO vl_hsl05,
*            wa_report_cal-hsl06 TO vl_hsl06,
*            wa_report_cal-hsl07 TO vl_hsl07,
*            wa_report_cal-hsl08 TO vl_hsl08,
*            wa_report_cal-hsl09 TO vl_hsl09,
*            wa_report_cal-hsl10 TO vl_hsl10,
*            wa_report_cal-hsl11 TO vl_hsl11,
*            wa_report_cal-hsl12 TO vl_hsl12.
*
*
*      wa_report_alv-hsl01 = vl_hsl01.
*      wa_report_alv-hsl02 = vl_hsl02.
*      wa_report_alv-hsl03 = vl_hsl03.
*      wa_report_alv-hsl04 = vl_hsl04.
*      wa_report_alv-hsl05 = vl_hsl05.
*      wa_report_alv-hsl06 = vl_hsl06.
*      wa_report_alv-hsl07 = vl_hsl07.
*      wa_report_alv-hsl08 = vl_hsl08.
*      wa_report_alv-hsl09 = vl_hsl09.
*      wa_report_alv-hsl10 = vl_hsl10.
*      wa_report_alv-hsl11 = vl_hsl11.
*      wa_report_alv-hsl12 = vl_hsl12.
*
*      CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
*                wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
*                wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.
*
*      APPEND wa_report_alv TO it_report_alv.
*    ENDIF.

*    CLEAR: wa_report_cal,
*             wa_report_alv,
*             vl_hsl01,
*             vl_hsl01,
*             vl_hsl02,
*             vl_hsl03,
*             vl_hsl04,
*             vl_hsl05,
*             vl_hsl06,
*             vl_hsl07,
*             vl_hsl08,
*             vl_hsl09,
*             vl_hsl10,
*             vl_hsl11,
*             vl_hsl12,
*             vl_total.

    "Acreditamiento de ISR Acumulado
*    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acredisrretacum.
*    IF sy-subrc EQ 0.
*
*      wa_report_alv-txt50 = wa_report_cal-txt50.
*
*      WRITE: wa_report_cal-hsl01 TO vl_hsl01,
*            wa_report_cal-hsl02 TO vl_hsl02,
*            wa_report_cal-hsl03 TO vl_hsl03,
*            wa_report_cal-hsl04 TO vl_hsl04,
*            wa_report_cal-hsl05 TO vl_hsl05,
*            wa_report_cal-hsl06 TO vl_hsl06,
*            wa_report_cal-hsl07 TO vl_hsl07,
*            wa_report_cal-hsl08 TO vl_hsl08,
*            wa_report_cal-hsl09 TO vl_hsl09,
*            wa_report_cal-hsl10 TO vl_hsl10,
*            wa_report_cal-hsl11 TO vl_hsl11,
*            wa_report_cal-hsl12 TO vl_hsl12.
*
*
*      wa_report_alv-hsl01 = vl_hsl01.
*      wa_report_alv-hsl02 = vl_hsl02.
*      wa_report_alv-hsl03 = vl_hsl03.
*      wa_report_alv-hsl04 = vl_hsl04.
*      wa_report_alv-hsl05 = vl_hsl05.
*      wa_report_alv-hsl06 = vl_hsl06.
*      wa_report_alv-hsl07 = vl_hsl07.
*      wa_report_alv-hsl08 = vl_hsl08.
*      wa_report_alv-hsl09 = vl_hsl09.
*      wa_report_alv-hsl10 = vl_hsl10.
*      wa_report_alv-hsl11 = vl_hsl11.
*      wa_report_alv-hsl12 = vl_hsl12.
*
*      CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
*                wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
*                wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.
*
*      APPEND wa_report_alv TO it_report_alv.
*    ENDIF.
    " INI ADD 25/02/2021 CJPG VARR
    CLEAR: wa_report_cal,
           wa_report_alv,
           vl_hsl01,
           vl_hsl01,
           vl_hsl02,
           vl_hsl03,
           vl_hsl04,
           vl_hsl05,
           vl_hsl06,
           vl_hsl07,
           vl_hsl08,
           vl_hsl09,
           vl_hsl10,
           vl_hsl11,
           vl_hsl12,
           vl_total.

    APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco

    wa_report_alv-txt50 = c_isr_retenido.
    APPEND wa_report_alv TO it_report_alv.

    LOOP AT it_report_cal INTO wa_report_cal WHERE racct IS NOT INITIAL.

      wa_report_alv-racct = wa_report_cal-racct.
      wa_report_alv-txt50 = wa_report_cal-txt50.

      WRITE: wa_report_cal-hsl01 TO vl_hsl01,
             wa_report_cal-hsl02 TO vl_hsl02,
             wa_report_cal-hsl03 TO vl_hsl03,
             wa_report_cal-hsl04 TO vl_hsl04,
             wa_report_cal-hsl05 TO vl_hsl05,
             wa_report_cal-hsl06 TO vl_hsl06,
             wa_report_cal-hsl07 TO vl_hsl07,
             wa_report_cal-hsl08 TO vl_hsl08,
             wa_report_cal-hsl09 TO vl_hsl09,
             wa_report_cal-hsl10 TO vl_hsl10,
             wa_report_cal-hsl11 TO vl_hsl11,
             wa_report_cal-hsl12 TO vl_hsl12.

      wa_report_alv-hsl01 = vl_hsl01.
      wa_report_alv-hsl02 = vl_hsl02.
      wa_report_alv-hsl03 = vl_hsl03.
      wa_report_alv-hsl04 = vl_hsl04.
      wa_report_alv-hsl05 = vl_hsl05.
      wa_report_alv-hsl06 = vl_hsl06.
      wa_report_alv-hsl07 = vl_hsl07.
      wa_report_alv-hsl08 = vl_hsl08.
      wa_report_alv-hsl09 = vl_hsl09.
      wa_report_alv-hsl10 = vl_hsl10.
      wa_report_alv-hsl11 = vl_hsl11.
      wa_report_alv-hsl12 = vl_hsl12.

      CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                wa_report_alv-hsl12 NO-GAPS.

      APPEND wa_report_alv TO it_report_alv.
      CLEAR: wa_report_alv, wa_report_cal, vl_hsl01, vl_hsl01, vl_hsl02, vl_hsl03, vl_hsl04,
              vl_hsl05, vl_hsl06, vl_hsl07, vl_hsl08, vl_hsl09, vl_hsl10, vl_hsl11, vl_hsl12.

    ENDLOOP.
    "FIN ADD 25/02/2021 CJPG VARR

    CLEAR:    wa_report_cal,
              wa_report_alv,
              vl_hsl01,
              vl_hsl01,
              vl_hsl02,
              vl_hsl03,
              vl_hsl04,
              vl_hsl05,
              vl_hsl06,
              vl_hsl07,
              vl_hsl08,
              vl_hsl09,
              vl_hsl10,
              vl_hsl11,
              vl_hsl12,
              vl_total.

    APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco
    APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco

    "ISR a Pagar Periodo Después de Acreditamientos
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_isrpagperdesacredit.
    IF sy-subrc EQ 0.

      wa_report_alv-txt50 = wa_report_cal-txt50.

      WRITE: wa_report_cal-hsl01 TO vl_hsl01,
             wa_report_cal-hsl02 TO vl_hsl02,
             wa_report_cal-hsl03 TO vl_hsl03,
             wa_report_cal-hsl04 TO vl_hsl04,
             wa_report_cal-hsl05 TO vl_hsl05,
             wa_report_cal-hsl06 TO vl_hsl06,
             wa_report_cal-hsl07 TO vl_hsl07,
             wa_report_cal-hsl08 TO vl_hsl08,
             wa_report_cal-hsl09 TO vl_hsl09,
             wa_report_cal-hsl10 TO vl_hsl10,
             wa_report_cal-hsl11 TO vl_hsl11,
             wa_report_cal-hsl12 TO vl_hsl12.


      wa_report_alv-hsl01 = vl_hsl01.
      wa_report_alv-hsl02 = vl_hsl02.
      wa_report_alv-hsl03 = vl_hsl03.
      wa_report_alv-hsl04 = vl_hsl04.
      wa_report_alv-hsl05 = vl_hsl05.
      wa_report_alv-hsl06 = vl_hsl06.
      wa_report_alv-hsl07 = vl_hsl07.
      wa_report_alv-hsl08 = vl_hsl08.
      wa_report_alv-hsl09 = vl_hsl09.
      wa_report_alv-hsl10 = vl_hsl10.
      wa_report_alv-hsl11 = vl_hsl11.
      wa_report_alv-hsl12 = vl_hsl12.

      CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

      APPEND wa_report_alv TO it_report_alv.
    ENDIF.

    CLEAR:   wa_report_cal,
             wa_report_alv,
             vl_hsl01,
             vl_hsl01,
             vl_hsl02,
             vl_hsl03,
             vl_hsl04,
             vl_hsl05,
             vl_hsl06,
             vl_hsl07,
             vl_hsl08,
             vl_hsl09,
             vl_hsl10,
             vl_hsl11,
             vl_hsl12,
             vl_total.

    APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco
    APPEND wa_report_alv TO it_report_alv.  "Se inserta una linea en blanco
    " Acum ISR Después de Acreditamientos
    READ TABLE it_report_cal INTO wa_report_cal WITH KEY txt50 = c_acumisrdespacdt.
    IF sy-subrc EQ 0.

      wa_report_alv-txt50 = wa_report_cal-txt50.

      WRITE: wa_report_cal-hsl01 TO vl_hsl01,
            wa_report_cal-hsl02 TO vl_hsl02,
            wa_report_cal-hsl03 TO vl_hsl03,
            wa_report_cal-hsl04 TO vl_hsl04,
            wa_report_cal-hsl05 TO vl_hsl05,
            wa_report_cal-hsl06 TO vl_hsl06,
            wa_report_cal-hsl07 TO vl_hsl07,
            wa_report_cal-hsl08 TO vl_hsl08,
            wa_report_cal-hsl09 TO vl_hsl09,
            wa_report_cal-hsl10 TO vl_hsl10,
            wa_report_cal-hsl11 TO vl_hsl11,
            wa_report_cal-hsl12 TO vl_hsl12.


      wa_report_alv-hsl01 = vl_hsl01.
      wa_report_alv-hsl02 = vl_hsl02.
      wa_report_alv-hsl03 = vl_hsl03.
      wa_report_alv-hsl04 = vl_hsl04.
      wa_report_alv-hsl05 = vl_hsl05.
      wa_report_alv-hsl06 = vl_hsl06.
      wa_report_alv-hsl07 = vl_hsl07.
      wa_report_alv-hsl08 = vl_hsl08.
      wa_report_alv-hsl09 = vl_hsl09.
      wa_report_alv-hsl10 = vl_hsl10.
      wa_report_alv-hsl11 = vl_hsl11.
      wa_report_alv-hsl12 = vl_hsl12.

      CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.

      APPEND wa_report_alv TO it_report_alv.
    ENDIF.


    CLEAR: wa_report_alv,
           wa_report_ret.

    APPEND wa_report_alv TO it_report_alv. "Linea en blanco

    IF it_report_ret IS NOT INITIAL.
      LOOP AT it_report_ret INTO wa_report_ret.
        CLEAR:   vl_hsl01,
                 vl_hsl01,
                 vl_hsl02,
                 vl_hsl03,
                 vl_hsl04,
                 vl_hsl05,
                 vl_hsl06,
                 vl_hsl07,
                 vl_hsl08,
                 vl_hsl09,
                 vl_hsl10,
                 vl_hsl11,
                 vl_hsl12,
                 vl_total.

        IF sy-tabix EQ 1.

          wa_report_alv-txt50 = wa_report_ret-txt50.

        ELSE.

          wa_report_alv-racct = wa_report_ret-racct.
          wa_report_alv-txt50 = wa_report_ret-txt50.

          WRITE: wa_report_ret-hsl01 TO vl_hsl01,
                 wa_report_ret-hsl02 TO vl_hsl02,
                 wa_report_ret-hsl03 TO vl_hsl03,
                 wa_report_ret-hsl04 TO vl_hsl04,
                 wa_report_ret-hsl05 TO vl_hsl05,
                 wa_report_ret-hsl06 TO vl_hsl06,
                 wa_report_ret-hsl07 TO vl_hsl07,
                 wa_report_ret-hsl08 TO vl_hsl08,
                 wa_report_ret-hsl09 TO vl_hsl09,
                 wa_report_ret-hsl10 TO vl_hsl10,
                 wa_report_ret-hsl11 TO vl_hsl11,
                 wa_report_ret-hsl12 TO vl_hsl12,
                 wa_report_ret-total TO vl_total.

          wa_report_alv-hsl01 = vl_hsl01.
          wa_report_alv-hsl02 = vl_hsl02.
          wa_report_alv-hsl03 = vl_hsl03.
          wa_report_alv-hsl04 = vl_hsl04.
          wa_report_alv-hsl05 = vl_hsl05.
          wa_report_alv-hsl06 = vl_hsl06.
          wa_report_alv-hsl07 = vl_hsl07.
          wa_report_alv-hsl08 = vl_hsl08.
          wa_report_alv-hsl09 = vl_hsl09.
          wa_report_alv-hsl10 = vl_hsl10.
          wa_report_alv-hsl11 = vl_hsl11.
          wa_report_alv-hsl12 = vl_hsl12.
          wa_report_alv-total = vl_total.

          CONDENSE: wa_report_alv-hsl02, wa_report_alv-hsl03, wa_report_alv-hsl04, wa_report_alv-hsl05, wa_report_alv-hsl06,
                    wa_report_alv-hsl07, wa_report_alv-hsl08, wa_report_alv-hsl09, wa_report_alv-hsl10, wa_report_alv-hsl11,
                    wa_report_alv-hsl12, wa_report_alv-total NO-GAPS.
        ENDIF.

        APPEND wa_report_alv TO it_report_alv.
        CLEAR  wa_report_alv.

      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DIPLAY_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_diplay_alv .

  DATA: vl_year(3) TYPE c,
        WA_LAYOUT TYPE SLIS_LAYOUT_ALV.

  WA_LAYOUT-ZEBRA = 'X'.

  vl_year = s_ryear-low+2(2).
  CONCATENATE '-' vl_year INTO vl_year.

  g_st_fieldcat-fieldname = 'RACCT'.    "Nombre del campo de la tabla
  g_st_fieldcat-seltext_l = 'Cuenta'.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'TXT50'.
  g_st_fieldcat-seltext_l = 'Concepto'.
  g_st_fieldcat-outputlen = 40.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'HSL01'.
  CONCATENATE 'Enero' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  g_st_fieldcat-just      = 'R'. "Alinear a la derecha.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'HSL02'.
  CONCATENATE 'Febrero' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'HSL03'.
  CONCATENATE 'Marzo' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'HSL04'.
  CONCATENATE 'Abril' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.


  g_st_fieldcat-fieldname = 'HSL05'.
  CONCATENATE 'Mayo' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'HSL06'.
  CONCATENATE 'Junio' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'HSL07'.
  CONCATENATE 'Julio' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'HSL08'.
  CONCATENATE 'Agosto' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'HSL09'.
  CONCATENATE 'Septiembre' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'HSL10'.
  CONCATENATE 'Octubre' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'HSL11'.
  CONCATENATE 'Noviembre' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'HSL12'.
  CONCATENATE 'Diciembre' vl_year INTO g_st_fieldcat-seltext_l.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.

  g_st_fieldcat-fieldname = 'TOTAL'.
  g_st_fieldcat-seltext_l = 'Total'.
  g_st_fieldcat-outputlen = 17.
  APPEND g_st_fieldcat TO g_it_fieldcat.

* Función ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      IS_LAYOUT     = WA_LAYOUT
      it_fieldcat   = g_it_fieldcat[]
    TABLES
      t_outtab      = it_report_alv[]
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FIX_NEGATIVE_SYMBOL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM FIX_NEGATIVE_SYMBOL .

  FIELD-SYMBOLS <FS_REPORT_ALV> TYPE ty_report_alv.

  LOOP AT it_report_alv ASSIGNING <FS_REPORT_ALV>.

    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl01
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl01.

    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl02
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl02.

    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl03
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl03.

    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl04
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl04.

    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl05
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl05.

    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl06
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl06.

    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl07
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl07.

    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl08
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl08.


    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl09
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl09.


    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl10
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl10.


    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl11
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl11.


    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-hsl12
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-hsl12.


    CALL FUNCTION 'ZFI_REPARAR_SIGNO_NEGATIVO'
      EXPORTING
        VALUE             = <FS_REPORT_ALV>-total
      IMPORTING
        VALUE_FIXED       = <FS_REPORT_ALV>-total.

  ENDLOOP.

ENDFORM.
