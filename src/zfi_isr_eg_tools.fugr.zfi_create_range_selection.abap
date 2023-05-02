FUNCTION ZFI_CREATE_RANGE_SELECTION.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(NAME) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(RANGE) TYPE  RSIS_T_RANGE
*"----------------------------------------------------------------------

  SELECT  SIGN,
          OPTI,
          LOW,
          HIGH
    INTO TABLE @DATA(tl_tvarvc)
    FROM tvarvc
    WHERE name = @name.
  IF sy-subrc = 0.
    LOOP AT tl_tvarvc ASSIGNING FIELD-SYMBOL(<fs_tvarvc>).
      range = VALUE  #(
                   BASE   range ( low    = <fs_tvarvc>-low
                                  sign   = <fs_tvarvc>-sign
                                  high   = <fs_tvarvc>-high
                                  option = <fs_tvarvc>-opti ) ).
    ENDLOOP.
  ENDIF.



ENDFUNCTION.
