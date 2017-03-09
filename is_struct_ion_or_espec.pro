PRO IS_STRUCT_ION_OR_ESPEC,dbStruct, $
                           is_ion, $
                           QUIET=quiet, $
                           LUN=lun

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(lun) EQ 0 THEN lun = -1   ;stdout

  IF TAG_EXIST(dbStruct,'ji') THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINTF,lun,"This is a FAST ion database..."
     is_ion = 1
  ENDIF ELSE BEGIN
     IF ~KEYWORD_SET(quiet) THEN PRINTF,lun,"This is a FAST eSpec database..."
     is_ion = 0
  ENDELSE

END