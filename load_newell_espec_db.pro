;2016/06/04
;;NOTE: We do not clean the current database. It's clean as a whistle.
PRO LOAD_NEWELL_ESPEC_DB,eSpec, $
                         FAILCODES=failCode, $
                         USE_UNSORTED_FILE=use_unsorted_file, $
                         NEWELLDBDIR=NewellDBDir, $
                         NEWELLDBFILE=NewellDBFile, $
                         FORCE_LOAD_DB=force_load_db, $
                         DONT_LOAD_IN_MEMORY=nonMem, $
                         DONT_PERFORM_CORRECTION=dont_perform_correction, $
                         DONT_CONVERT_TO_STRICT_NEWELL=dont_convert_to_strict_newell, $
                         JUST_TIMES=just_times, $
                         OUT_TIMES=out_times, $
                         ;; OUT_GOOD_I=good_i, $
                         LUN=lun, $
                         QUIET=quiet

  COMPILE_OPT idl2

  ;;This common block is defined ONLY here and in GET_ESPEC_ION_DB_IND
  ;; IF ~KEYWORD_SET(nonMem) THEN BEGIN
  @common__newell_espec.pro
  ;; ENDIF
  
  defNewellDBDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'

  ;;The file with failcodes
  defNewellDBFile        = 'eSpec_failCodes_20160609_db--PARSED--Orbs_500-16361.sav' ;;This file does not need to be cleaned

  ;;The file without failcodes
  defNewellDBFile        = 'eSpec_20160607_db--PARSED--with_mapping_factors--Orbs_500-16361.sav' ;;This file does not need to be cleaned

  ;; defNewellDBCleanInds   = 'iSpec_20160607_db--PARSED--Orbs_500-16361--indices_w_no_NaNs_INFs.sav'

  defSortNewellDBFile    =  "sorted--" + defNewellDBFile


  IF N_ELEMENTS(quiet) EQ 0 THEN quiet = 0

  IF N_ELEMENTS(lun) EQ 0 THEN BEGIN
     lun                 = -1
  ENDIF

  ;; IF ~KEYWORD_SET(nonMem) THEN BEGIN
  IF N_ELEMENTS(NEWELL__eSpec) NE 0 AND ~KEYWORD_SET(force_load_db) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(just_times): BEGIN
           IF ~quiet THEN PRINT,"Just giving eSpec times ..."
           out_times     = NEWELL__eSpec.x

           IF KEYWORD_SET(nonMem) THEN BEGIN
              CLEAR_ESPEC_DB_VARS
           ENDIF

           RETURN
        END
        ELSE: BEGIN
           IF ~quiet THEN PRINT,'Restoring eSpec DB already in memory...'
           eSpec         = NEWELL__eSpec
           IF N_ELEMENTS(NEWELL__failCodes) GT 0 THEN BEGIN
              failCodes  = NEWELL__failCodes
           ENDIF
           NewellDBDir   = NEWELL__dbDir
           NewellDBFile  = NEWELL__dbFile
        END
     ENDCASE
     RETURN
  ENDIF
  ;; ENDIF

  IF N_ELEMENTS(NewellDBDir) EQ 0 THEN BEGIN
     NewellDBDir      = defNewellDBDir
  ENDIF
  ;; IF ~KEYWORD_SET(nonMem) THEN BEGIN
  NEWELL__dbDir          = NewellDBDir
  ;; ENDIF

  IF N_ELEMENTS(NewellDBFile) EQ 0 THEN BEGIN
     CASE KEYWORD_SET(use_unsorted_file) OF
        1: BEGIN
           ;; IF ~quiet THEN PRINT,'Using UNsorted eSpec DB ...'
           specType      = 'UNSORTED'
           NewellDBFile  = defNewellDBFile
        END
        ELSE: BEGIN
           ;; IF ~quiet THEN PRINT,'Using sorted eSpec DB ...'
           specType      = 'SORTED'
           NewellDBFile  = defSortNewellDBFile
        END
     ENDCASE
  ENDIF
  ;; IF ~KEYWORD_SET(nonMem) THEN BEGIN
  NEWELL__dbFile         = NewellDBFile
  ;; ENDIF
  IF N_ELEMENTS(specType) EQ 0 THEN specType = ''
  IF N_ELEMENTS(eSpec) EQ 0 OR KEYWORD_SET(force_load_db) THEN BEGIN
     IF KEYWORD_SET(force_load_db) THEN BEGIN
        IF ~quiet THEN PRINTF,lun,"Forced loading of " + specType + " eSpec database ..."
     ENDIF
     IF ~quiet THEN PRINTF,lun,'Loading ' + specType + ' eSpec DB: ' + NewellDBFile + '...'
     RESTORE,NewellDBDir+NewellDBFile

     ;;Correct fluxes
     IF ~KEYWORD_SET(dont_perform_correction) THEN BEGIN
        IF ~quiet THEN PRINT,"Correcting eSpec fluxes..."
        eSpec.je[WHERE(eSpec.ilat LT 0)]  = (-1.)*(eSpec.je[WHERE(eSpec.ilat LT 0)])
        eSpec.jee[WHERE(eSpec.ilat LT 0)] = (-1.)*(eSpec.jee[WHERE(eSpec.ilat LT 0)])
        
        ;;Convert to strict Newell interpretation
        
        IF ~KEYWORD_SET(dont_convert_to_strict_newell) THEN BEGIN
           IF ~quiet THEN PRINT,"Converting eSpec DB to strict Newell interpretation ..."
           CONVERT_ESPEC_TO_STRICT_NEWELL_INTERPRETATION,eSpec,eSpec,/HUGE_STRUCTURE,/VERBOSE
        ENDIF ELSE BEGIN
           IF ~quiet THEN PRINT,'Not converting eSpec to strict Newell interp ...'
        ENDELSE
     ENDIF ELSE BEGIN
        IF ~quiet THEN PRINT,"Not correcting sign in each hemisphere, and not converting to strict Newell interp ..."
     ENDELSE

     ;;The following lines aren't necessary for this little beaut

     ;; IF FILE_TEST(NewellDBDir+defNewellDBCleanInds) THEN BEGIN
     ;;    RESTORE,NewellDBDir+defNewellDBCleanInds
     ;; ENDIF ELSE BEGIN        
     ;;    good_i = BASIC_ESPEC_ION_DB_CLEANER(eSpec,/CLEAN_NANS_AND_INFINITIES)
     ;;    IF ~quiet THEN PRINT,'Saving NaN- and INF-less eSpec DB inds to ' + defNewellDBCleanInds + '...'
     ;;    SAVE,good_i,FILENAME=NewellDBDir+defNewellDBCleanInds
     ;; ENDELSE
     
  ENDIF ELSE BEGIN
     IF ~quiet THEN PRINTF,lun,'eSpec DB already loaded! Not restoring ' + NewellDBFile + '...'
  ENDELSE

  ;; IF ~KEYWORD_SET(nonMem) THEN BEGIN
  NEWELL__eSpec          = eSpec

  IF N_ELEMENTS(failCode) NE 0 THEN BEGIN
     NEWELL__failCodes   = failCode
  ENDIF ELSE BEGIN
     NEWELL__failCodes   = !NULL
     IF ~quiet THEN PRINT,'This Newell DB file doesn''t have fail codes!'
  ENDELSE

  ;; ENDIF

  IF KEYWORD_SET(just_times) THEN BEGIN
     out_times              = TEMPORARY(eSpec.x)
  ENDIF

  IF KEYWORD_SET(nonMem) THEN BEGIN
     CLEAR_ESPEC_DB_VARS,QUIET=quiet
  ENDIF

  RETURN

END