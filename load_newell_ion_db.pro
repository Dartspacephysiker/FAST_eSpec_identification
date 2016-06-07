;2016/06/07
PRO LOAD_NEWELL_ION_DB,ion, $
                         ;; FAILCODES=failCodes, $
                         NEWELLDBDIR=NewellDBDir, $
                         NEWELLDBFILE=NewellDBFile, $
                         FORCE_LOAD_DB=force_load_db, $
                         DONT_LOAD_IN_MEMORY=nonMem, $
                         LUN=lun

  COMPILE_OPT idl2

  ;;This common block is defined ONLY here and in GET_H2D_NEWELLS__EACH_TYPE
  IF ~KEYWORD_SET(nonMem) THEN BEGIN
     COMMON NEWELL,NEWELL__ion, $
        ;; NEWELL__failCodes, $
        NEWELL__dbFile,NEWELL__dbDir
  ENDIF
  
  defNewellDBDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  defNewellDBFile        = 'iSpec_20160607_db--PARSED--Orbs_500-16361.sav'

  IF N_ELEMENTS(lun) EQ 0 THEN BEGIN
     lun                 = -1
  ENDIF

  IF ~KEYWORD_SET(nonMem) THEN BEGIN
     IF N_ELEMENTS(NEWELL__ion) NE 0 AND ~KEYWORD_SET(force_load_db) THEN BEGIN
        PRINT,'Restoring ion DB already in memory...'
        ion                 = NEWELL__ion
        ;; failCodes           = NEWELL__failCodes
        NewellDBDir         = NEWELL__dbDir
        NewellDBFile        = NEWELL__dbFile
        RETURN
     ENDIF
  ENDIF

  IF N_ELEMENTS(NewellDBDir) EQ 0 THEN BEGIN
     ;; IF KEYWORD_SET(load_culled_ion_db) THEN BEGIN
     ;;    NewellDBDir   = defCulledNewellDBDir
     ;; ENDIF ELSE BEGIN
        NewellDBDir      = defNewellDBDir
     ;; ENDELSE
  ENDIF
  IF ~KEYWORD_SET(nonMem) THEN BEGIN
     NEWELL__dbDir          = NewellDBDir
  ENDIF

  IF N_ELEMENTS(NewellDBFile) EQ 0 THEN BEGIN
     ;; IF KEYWORD_SET(load_culled_ion_db) THEN BEGIN
     ;;    NewellDBFile  = defCulledNewellDBFile
     ;; ENDIF ELSE BEGIN
     ;; IF KEYWORD_SET(despun_alf_db) THEN BEGIN
     ;;    PRINT,"ions for despun Alfvén wave database..."

     ;;    NewellDBFile     = defNewellDespunDBFile
     ;;    NEWELL__despun   = 1
     ;; ENDIF ELSE BEGIN
        NewellDBFile     = defNewellDBFile
        ;; NEWELL__despun   = 0
     ;; ENDELSE
        ;; ENDELSE
  ENDIF
  IF ~KEYWORD_SET(nonMem) THEN BEGIN
     NEWELL__dbFile         = NewellDBFile
  ENDIF

  IF N_ELEMENTS(ion) EQ 0 OR KEYWORD_SET(force_load_db) THEN BEGIN
     IF KEYWORD_SET(force_load_db) THEN BEGIN
        PRINTF,lun,"Forced loading of ion database ..."
     ENDIF
     ;; IF KEYWORD_SET(load_culled_ion_db) THEN BEGIN
     ;;    PRINTF,lun,'Loading culled ION DB: ' + NewellDBDir+NewellDBFile + '...'
     ;;    restore,NewellDBDir+NewellDBFile
     ;;    ion         = ion_culled
     ;; ENDIF ELSE BEGIN
        PRINTF,lun,'Loading ion DB: ' + NewellDBFile + '...'
        restore,NewellDBDir+NewellDBFile
     ;; ENDELSE
  ENDIF ELSE BEGIN
     PRINTF,lun,'ion DB already loaded! Not restoring ' + NewellDBFile + '...'
  ENDELSE
  IF ~KEYWORD_SET(nonMem) THEN BEGIN
     NEWELL__ion          = ion
  ENDIF

  ;; IF N_ELEMENTS(failCodes) NE 0 THEN BEGIN
  ;;    NEWELL__failCodes   = failCodes
  ;; ENDIF ELSE BEGIN
  ;;    NEWELL__failCodes   = !NULL
  ;;    PRINT,'This Newell DB file doesn''t have fail codes!'
  ;; ENDELSE

  RETURN

END