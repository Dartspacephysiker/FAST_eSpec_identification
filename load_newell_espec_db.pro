;2016/06/04
PRO LOAD_NEWELL_ESPEC_DB,eSpec, $
                         ;; FAILCODES=failCodes, $
                         NEWELLDBDIR=NewellDBDir, $
                         NEWELLDBFILE=NewellDBFile, $
                         FORCE_LOAD_DB=force_load_db, $
                         LUN=lun

  COMPILE_OPT idl2

  ;;This common block is defined ONLY here and in GET_H2D_NEWELLS__EACH_TYPE
  COMMON NEWELL,NEWELL__eSpec, $
     ;; NEWELL__failCodes, $
     NEWELL__dbFile,NEWELL__dbDir

  defNewellDBDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  defNewellDBFile        = 'eSpec_20160603_db--PARSED--Orbs_500-16361.sav'
  ;; defNewellDespunDBFile  = 'alf_eSpec_20160508_db--despun--TOTAL_ESPECS_FAILCODES__Orbs_502-16361--20160603.sav'

  IF N_ELEMENTS(lun) EQ 0 THEN BEGIN
     lun                 = -1
  ENDIF

  IF N_ELEMENTS(NEWELL__eSpec) NE 0 AND ~KEYWORD_SET(force_load_db) THEN BEGIN
     PRINT,'Restoring eSpec DB already in memory...'
     eSpec               = NEWELL__eSpec
     ;; failCodes           = NEWELL__failCodes
     NewellDBDir         = NEWELL__dbDir
     NewellDBFile        = NEWELL__dbFile
     RETURN
  ENDIF

  IF N_ELEMENTS(NewellDBDir) EQ 0 THEN BEGIN
     ;; IF KEYWORD_SET(load_culled_eSpec_db) THEN BEGIN
     ;;    NewellDBDir   = defCulledNewellDBDir
     ;; ENDIF ELSE BEGIN
        NewellDBDir      = defNewellDBDir
     ;; ENDELSE
  ENDIF
  NEWELL__dbDir          = NewellDBDir

  IF N_ELEMENTS(NewellDBFile) EQ 0 THEN BEGIN
     ;; IF KEYWORD_SET(load_culled_eSpec_db) THEN BEGIN
     ;;    NewellDBFile  = defCulledNewellDBFile
     ;; ENDIF ELSE BEGIN
     ;; IF KEYWORD_SET(despun_alf_db) THEN BEGIN
     ;;    PRINT,"eSpecs for despun Alfvén wave database..."

     ;;    NewellDBFile     = defNewellDespunDBFile
     ;;    NEWELL__despun   = 1
     ;; ENDIF ELSE BEGIN
        NewellDBFile     = defNewellDBFile
        ;; NEWELL__despun   = 0
     ;; ENDELSE
        ;; ENDELSE
  ENDIF
  NEWELL__dbFile         = NewellDBFile

  IF N_ELEMENTS(eSpec) EQ 0 OR KEYWORD_SET(force_load_db) THEN BEGIN
     IF KEYWORD_SET(force_load_db) THEN BEGIN
        PRINTF,lun,"Forced loading of eSpec database ..."
     ENDIF
     ;; IF KEYWORD_SET(load_culled_eSpec_db) THEN BEGIN
     ;;    PRINTF,lun,'Loading culled ESPEC DB: ' + NewellDBDir+NewellDBFile + '...'
     ;;    restore,NewellDBDir+NewellDBFile
     ;;    eSpec         = eSpec_culled
     ;; ENDIF ELSE BEGIN
        PRINTF,lun,'Loading eSpec DB: ' + NewellDBDir+NewellDBFile + '...'
        restore,NewellDBDir+NewellDBFile
     ;; ENDELSE
  ENDIF ELSE BEGIN
     PRINTF,lun,'eSpec DB already loaded! Not restoring ' + NewellDBFile + '...'
  ENDELSE
  NEWELL__eSpec          = eSpec

  ;; IF N_ELEMENTS(failCodes) NE 0 THEN BEGIN
  ;;    NEWELL__failCodes   = failCodes
  ;; ENDIF ELSE BEGIN
  ;;    NEWELL__failCodes   = !NULL
  ;;    PRINT,'This Newell DB file doesn''t have fail codes!'
  ;; ENDELSE

  RETURN

END