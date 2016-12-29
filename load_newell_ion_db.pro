;2016/06/07
PRO LOAD_NEWELL_ION_DB,ion,ion__times,ion__delta_t, $
                       DOWNGOING=downgoing, $
                       ;; FAILCODES=failCodes, $
                       NEWELLDBDIR=NewellDBDir, $
                       NEWELLDBFILE=NewellDBFile, $
                       FORCE_LOAD_DB=force_load_db, $
                       DONT_LOAD_IN_MEMORY=nonMem, $
                       DONT_PERFORM_CORRECTION=dont_perform_SH_correction, $
                       DONT_MAP_TO_100KM=no_mapping, $
                       DO_NOT_MAP_FLUXES=do_not_map_fluxes, $
                       DO_NOT_MAP_DELTA_T=do_not_map_delta_t, $
                       LOAD_DELTA_T=load_delta_t, $
                       COORDINATE_SYSTEM=coordinate_system, $
                       USE_AACGM_COORDS=use_aacgm, $
                       USE_GEO_COORDS=use_geo, $
                       USE_MAG_COORDS=use_mag, $
                       JUST_TIMES=just_times, $
                       OUT_TIMES=out_times, $
                       LOAD_DELTA_ILAT_FOR_WIDTH_TIME=load_dILAT, $
                       LOAD_DELTA_ANGLE_FOR_WIDTH_TIME=load_dAngle, $
                       LOAD_DELTA_X_FOR_WIDTH_TIME=load_dx, $
                       ;; OUT_CLEANED_I=cleaned_i, $
                       CLEAR_MEMORY=clear_memory, $
                       LUN=lun

  COMPILE_OPT idl2

  IF KEYWORD_SET(clear_memory) THEN BEGIN
     CLEAR_ION_DB_VARS,QUIET=quiet
     RETURN
  ENDIF


  ;;This common block is defined ONLY here and in GET_ESPEC_ION_DB_IND, I believe
  ;; IF ~KEYWORD_SET(nonMem) THEN BEGIN
  @common__newell_ion_db.pro
  ;; ENDIF
  
  defNewellDBDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'

  CASE 1 OF
     KEYWORD_SET(downgoing): BEGIN
        defNewellDBFile        = 'iSpec_down_20161228_db--PARSED--Orbs_500-16361.sav'

        DB_date                = '20161228'
        DB_version             = 'v0.0'
        DB_extras              = ''
     END
     ELSE: BEGIN
        defNewellDBFile        = 'iSpec_20160607_db--PARSED--Orbs_500-16361.sav'
        defNewellDBCleanInds   = 'iSpec_20160607_db--PARSED--Orbs_500-16361--indices_w_no_NaNs_INFs.sav'

        DB_date                = '20160607'
        DB_version             = 'v0.0'
        DB_extras              = ''
     END
  ENDCASE
  IF N_ELEMENTS(lun) EQ 0 THEN BEGIN
     lun                 = -1
  ENDIF

  IF N_ELEMENTS(NEWELL_I__eSpec) NE 0 AND ~KEYWORD_SET(force_load_db) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(just_times): BEGIN
           IF ~quiet THEN PRINT,"Just giving eSpec times ..."
           out_times     = NEWELL_I__ion.x

           RETURN
        END
        KEYWORD_SET(nonMem): BEGIN
           PRINT,"Moving ion structure/data in mem to outputted variables ..."
           eSpec            = TEMPORARY(NEWELL_I__ion     )
           ;; fastLoc_times    = TEMPORARY(FASTLOC__times)
           NewellDBFile     = TEMPORARY(NEWELL_I__dbFile    )
           NewellDBDir      = TEMPORARY(NEWELL_I__dbDir     )
           ion__delta_t     = N_ELEMENTS(NEWELL_I__delta_t  ) GT 0 ? TEMPORARY(NEWELL_I__delta_t  ) : !NULL 
        END
        ELSE: BEGIN
           ;; IF ~quiet THEN PRINT,'Restoring eSpec DB already in memory...'
           ;; eSpec         = NEWELL_I__ion
           ;; NewellDBDir   = NEWELL__dbDir
           ;; NewellDBFile  = NEWELL__dbFile
           PRINT,"There is already an eSpec DB in memory! If you want it to come out, set /NO_MEMORY_LOAD"
        END
     ENDCASE
     RETURN
  ENDIF

  ;; IF ~KEYWORD_SET(nonMem) THEN BEGIN
     ;; IF N_ELEMENTS(NEWELL_I__ion) NE 0 AND ~KEYWORD_SET(force_load_db) THEN BEGIN
     ;;    CASE 1 OF
     ;;       KEYWORD_SET(just_times): BEGIN
     ;;          PRINT,"Just giving eSpec times ..."
     ;;          out_times     = ion.x
     ;;       END
     ;;       ELSE: BEGIN
     ;;          PRINT,'Restoring ion DB already in memory...'
     ;;          ion           = NEWELL_I__ion
     ;;          NewellDBDir   = NEWELL_I__dbDir
     ;;          NewellDBFile  = NEWELL_I__dbFile
     ;;       END
     ;;    ENDCASE
     ;;    RETURN
     ;; ENDIF
  ;; ENDIF

  IF N_ELEMENTS(NewellDBDir) EQ 0 THEN BEGIN
     NewellDBDir            = defNewellDBDir
  ENDIF
  ;; IF ~KEYWORD_SET(nonMem) THEN BEGIN
     NEWELL_I__dbDir        = NewellDBDir
  ;; ENDIF

  IF N_ELEMENTS(NewellDBFile) EQ 0 THEN BEGIN
     NewellDBFile           = defNewellDBFile
  ENDIF
  ;; IF ~KEYWORD_SET(nonMem) THEN BEGIN
     NEWELL_I__dbFile       = NewellDBFile
  ;; ENDIF

  IF N_ELEMENTS(ion) EQ 0 OR KEYWORD_SET(force_load_db) THEN BEGIN
     IF KEYWORD_SET(force_load_db) THEN BEGIN
        PRINTF,lun,"Forced loading of ion database ..."
     ENDIF
     PRINTF,lun,'Loading ion DB: ' + NewellDBFile + '...'
     RESTORE,NewellDBDir+NewellDBFile

     NEWELL_ESPEC__ADD_INFO_STRUCT,ion, $
                                   /IONS, $
                                   DB_DATE=DB_date, $
                                   DB_VERSION=DB_version, $
                                   DB_EXTRAS=DB_extras, $
                                   REDUCE_DBSIZE=reduce_dbSize, $
                                   IS_ALFNEWELL=is_AlfNewell

     IF KEYWORD_SET(downgoing) THEN BEGIN
        ion.info.is_downgoing = 1B
     ENDIF

     IF ~KEYWORD_SET(dont_perform_SH_correction) THEN BEGIN
        ;;Correct fluxes
        PRINT,"Correcting ionDB fluxes..."
        ion.ji[WHERE(ion.ilat GT 0)]  = (-1.)*(ion.ji[WHERE(ion.ilat GT 0)])
        ion.jei[WHERE(ion.ilat GT 0)] = (-1.)*(ion.jei[WHERE(ion.ilat GT 0)])

        ion.info.correctedFluxes = 1B
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;What type of delta do you want?
     delta_stuff = KEYWORD_SET(load_delta_t) + KEYWORD_SET(load_dILAT) + KEYWORD_SET(load_dx) + KEYWORD_SET(load_dAngle)
     CASE delta_stuff OF
        0:
        1: BEGIN
           IF ~KEYWORD_SET(load_delta_t) THEN BEGIN
              dILAT_file         = GET_FAST_DB_STRING(ion,/FOR_ION_DB) + '-delta_ilats.sav'
              RESTORE,NewellDBDir+dILAT_file
           ENDIF
        END
        ELSE: BEGIN
           PRINT,"Can't have it all."
           STOP
        END
     ENDCASE

     IF KEYWORD_SET(load_delta_t) THEN BEGIN
        PRINT,"Loading ion delta_ts ..."
        ion__delta_t = GET_ESPEC_ION_DELTA_T(ion, $
                                               DBNAME='ion')
     ENDIF


     ;; IF FILE_TEST(NewellDBDir+defNewellDBCleanInds) THEN BEGIN
     ;;    RESTORE,NewellDBDir+defNewellDBCleanInds
     ;; ENDIF ELSE BEGIN        
     ;;    cleaned_i = BASIC_ESPEC_ION_DB_CLEANER(ion,/CLEAN_NANS_AND_INFINITIES)
     ;;    PRINT,'Saving NaN- and INF-less ion DB inds to ' + defNewellDBCleanInds + '...'
     ;;    SAVE,cleaned_i,FILENAME=NewellDBDir+defNewellDBCleanInds
     ;; ENDELSE

     ;; IF ~KEYWORD_SET(nonMem) THEN BEGIN
     ;;    NEWELL_I__cleaned_i = cleaned_i
     ;; ENDIF

  ENDIF ELSE BEGIN
     PRINTF,lun,'ion DB already loaded! Not restoring ' + NewellDBFile + '...'
  ENDELSE


  IF KEYWORD_SET(coordinate_system) THEN BEGIN
     CASE STRUPCASE(coordinate_system) OF
        'AACGM': BEGIN
           use_aacgm = 1
           use_geo   = 0
           use_mag   = 0
        END
        'GEO'  : BEGIN
           use_aacgm = 0
           use_geo   = 1
           use_mag   = 0
        END
        'MAG'  : BEGIN
           use_aacgm = 0
           use_geo   = 0
           use_mag   = 1
        END
     ENDCASE

     PRINT,"Can't do anything with ion coords right now!"
  ENDIF

  IF ~KEYWORD_SET(nonMem) THEN BEGIN
     NEWELL_I__ion          = TEMPORARY(ion)

     IF KEYWORD_SET(delta_stuff) THEN BEGIN
        NEWELL_I__delta_t   = TEMPORARY(ion__delta_t)
     ENDIF

  ENDIF

  RETURN

END