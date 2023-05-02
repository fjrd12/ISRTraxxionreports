FUNCTION ZFI_ISR_READ_TEXT_BUFFER.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(CLIENT) LIKE  SY-MANDT DEFAULT SY-MANDT
*"     VALUE(ID) LIKE  THEAD-TDID
*"     VALUE(LANGUAGE) LIKE  THEAD-TDSPRAS
*"     VALUE(NAME) LIKE  THEAD-TDNAME
*"     VALUE(OBJECT) LIKE  THEAD-TDOBJECT
*"     VALUE(ARCHIVE_HANDLE) LIKE  SY-TABIX DEFAULT 0
*"     VALUE(LOCAL_CAT) DEFAULT SPACE
*"  EXPORTING
*"     VALUE(HEADER) LIKE  THEAD STRUCTURE  THEAD
*"     VALUE(OLD_LINE_COUNTER) TYPE  THEAD-TDTXTLINES
*"  TABLES
*"      LINES STRUCTURE  TLINE
*"  EXCEPTIONS
*"      ID
*"      LANGUAGE
*"      NAME
*"      NOT_FOUND
*"      OBJECT
*"      REFERENCE_CHECK
*"      WRONG_ACCESS_TO_ARCHIVE
*"----------------------------------------------------------------------
data: ZFI_ISR_TLINEW type ZFI_ISR_TLINEW,
      tline type tline.

clear: LINES.
refresh LINES.

read table ZFI_ISR_TLINET into ZFI_ISR_TLINEW
WITH TABLE KEY  TDOBJECT = OBJECT
                TDNAME = NAME
                TDID = ID
                TDSPRAS = LANGUAGE.

if sy-subrc = 0.
  append LINES OF ZFI_ISR_TLINEW-TLINE to lines.
else.

  ZFI_ISR_TLINEW-TDOBJECT = OBJECT.
  ZFI_ISR_TLINEW-TDNAME = NAME.
  ZFI_ISR_TLINEW-TDID = ID.
  ZFI_ISR_TLINEW-TDSPRAS = LANGUAGE.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                        = SY-MANDT
      ID                            = ID
      LANGUAGE                      = LANGUAGE
      NAME                          = NAME
      OBJECT                        = OBJECT
*     ARCHIVE_HANDLE                = 0
*     LOCAL_CAT                     = ' '
*   IMPORTING
*     HEADER                        =
*     OLD_LINE_COUNTER              =
    TABLES
      LINES                         = LINES
   EXCEPTIONS
     ID                            = 1
     LANGUAGE                      = 2
     NAME                          = 3
     NOT_FOUND                     = 4
     OBJECT                        = 5
     REFERENCE_CHECK               = 6
     WRONG_ACCESS_TO_ARCHIVE       = 7
     OTHERS                        = 8
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  else.
    append LINES OF lines to ZFI_ISR_TLINEW-TLINE.
    APPEND ZFI_ISR_TLINEW TO ZFI_ISR_TLINET.
  ENDIF.

ENDIF.

ENDFUNCTION.
