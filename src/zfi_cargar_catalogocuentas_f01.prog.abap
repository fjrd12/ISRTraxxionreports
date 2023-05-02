*&---------------------------------------------------------------------*
*& Include          ZFI_CARGAR_CATALOGOCUENTAS_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form READ_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM READ_FILE .

  TRY.

  DATA(V_CL_LOAD_EXCEL) = NEW LCL_LOAD_EXCEL( V_FULLPATH = CONV #( P_PATH )
                                            ).

  IF P_DELETE EQ 'X'.

    V_CL_LOAD_EXCEL->DELETE_TABLE( ).

    V_CL_LOAD_EXCEL->IMPORT( ).

    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
*       TITEL              = ' '
        TEXTLINE1          = 'Datos cargados en la tabla catalogo ZFI_CUENTAS_ISR'
*       TEXTLINE2          = ' '
*       START_COLUMN       = 25
*       START_ROW          = 6
              .

  ELSE.

    V_CL_LOAD_EXCEL->IMPORT( ).

    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
*       TITEL              = ' '
        TEXTLINE1          = 'Datos cargados en la tabla catalogo ZFI_CUENTAS_ISR'
*       TEXTLINE2          = ' '
*       START_COLUMN       = 25
*       START_ROW          = 6
              .


  ENDIF.

  CATCH LCX_CONFIGURATION INTO DATA(CONFIGURATIONEXCEPTION).
    WRITE: / CONFIGURATIONEXCEPTION->LOCAL_TEXT.

  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DISPLAY_TABLE .

  "Leer tabla
  SELECT *
    FROM ZFI_CUENTAS_ISR
    INTO TABLE @IT_ZFI_CUENTAS_ISR
    WHERE BUKRS EQ @P_RBUKRS "SOCIEDAD
      AND GJAHR IN @S_RYEAR. "EJERCICIO

  "Contruyendo catalogo de ALV
  G_ST_FIELDCAT-FIELDNAME = 'ZCODIGO'.    "NOMBRE DEL CAMPO DE LA TABLA
  G_ST_FIELDCAT-SELTEXT_L = 'CODIGO DE CATALOGO'.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'GJAHR'.
  G_ST_FIELDCAT-SELTEXT_L = 'EJERCICIO'.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'BUKRS'.
  G_ST_FIELDCAT-SELTEXT_L = 'SOCIEDAD'.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'MONAT'.
  G_ST_FIELDCAT-SELTEXT_L = 'MES CONTABLE'.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'RACCT'.
  G_ST_FIELDCAT-SELTEXT_L = 'NUMERO DE CUENTA'.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'RACCT'.
  G_ST_FIELDCAT-SELTEXT_L = 'NUMERO DE CUENTA'.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'ZCOEFICIENTE_UTILIDAD'.
  G_ST_FIELDCAT-SELTEXT_L = 'VALOR'.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'DESCRIPCION'.
  G_ST_FIELDCAT-SELTEXT_L = 'DESCIRPCION'.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'REPETIR'.
  G_ST_FIELDCAT-SELTEXT_L = 'BANDERA DE REPETICION'.
  G_ST_FIELDCAT-CHECKBOX  = 'X'.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'USUARIO'.
  G_ST_FIELDCAT-SELTEXT_L = 'USUARIO'.
  G_ST_FIELDCAT-CHECKBOX  = ' '.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'FECHA'.
  G_ST_FIELDCAT-SELTEXT_L = 'FECHA'.
  G_ST_FIELDCAT-CHECKBOX  = ' '.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'HORA'.
  G_ST_FIELDCAT-SELTEXT_L = 'HORA'.
  G_ST_FIELDCAT-CHECKBOX  = ' '.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  G_ST_FIELDCAT-FIELDNAME = 'MANDT'.
  G_ST_FIELDCAT-SELTEXT_L = 'MANDANTE'.
  G_ST_FIELDCAT-NO_OUT = 'X'.
  APPEND G_ST_FIELDCAT TO G_IT_FIELDCAT.

  "Modificanto Layout
  WA_LAYOUT-ZEBRA = 'X'.

* FUNCIÓN ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      IS_LAYOUT     = WA_LAYOUT
      IT_FIELDCAT   = G_IT_FIELDCAT[]
    TABLES
      T_OUTTAB      = IT_ZFI_CUENTAS_ISR[]
    EXCEPTIONS
      PROGRAM_ERROR = 1
      OTHERS        = 2.


ENDFORM.
