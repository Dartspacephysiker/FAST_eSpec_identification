;2016/06/07
PRO LOAD_NEWELL_ION_DB,ion,ion__times,ion__delta_t, $
                       DOWNGOING=downgoing, $
                       ;; FAILCODES=failCodes, $
                       NEWELLDBDIR=NewellDBDir, $
                       NEWELLDBFILE=NewellDBFile, $
                       FORCE_LOAD_DB=force_load_db, $
                       ;; DONT_LOAD_IN_MEMORY=nonMem, $
                       DONT_PERFORM_CORRECTION=dont_perform_SH_correction, $
                       DONT_MAP_TO_100KM=no_mapping, $
                       DO_NOT_MAP_FLUXES=do_not_map_fluxes, $
                       DO_NOT_MAP_DELTA_T=do_not_map_delta_t, $
                       LOAD_DELTA_T=load_delta_t, $
                       LOAD_CHARE=load_charE, $
                       COORDINATE_SYSTEM=coordinate_system, $
                       USE_LNG=use_lng, $
                       USE_AACGM_COORDS=use_AACGM, $
                       USE_GEI_COORDS=use_GEI, $
                       USE_GEO_COORDS=use_GEO, $
                       USE_MAG_COORDS=use_MAG, $
                       USE_SDT_COORDS=use_SDT, $
                       JUST_TIMES=just_times, $
                       OUT_TIMES=out_times, $
                       LOAD_DELTA_ILAT_FOR_WIDTH_TIME=load_dILAT, $
                       LOAD_DELTA_ANGLE_FOR_WIDTH_TIME=load_dAngle, $
                       LOAD_DELTA_X_FOR_WIDTH_TIME=load_dx, $
                       ;; OUT_CLEANED_I=cleaned_i, $
                       CLEAR_MEMORY=clear_memory, $
                       NO_MEMORY_LOAD=noMem, $
                       QUIET=quiet, $
                       LUN=lun

  COMPILE_OPT idl2

  @common__newell_ion_db.pro
  
  defNewellDBDir         = '/SPENCEdata/Research/database/FAST/dartdb/ion_db/'
  defCoordDir            = '/SPENCEdata/Research/database/FAST/dartdb/ion_db/alternate_coords/'

  needsEphem             = 0

  IF N_ELEMENTS(quiet) EQ 0 THEN quiet = 0

  IF N_ELEMENTS(lun) EQ 0 THEN BEGIN
     lun                 = -1
  ENDIF

  ;;DONT_LOAD_IN_MEMORY is kept so that other routines don't make a mistake,
  ;;but I've included the keyword NO_MEMORY_LOAD from LOAD_MAXIMUS and LOAD_FASTLOC for the sake of my poor memory
  ;; IF N_ELEMENTS(noMem) NE 0 THEN BEGIN
  ;;    IF N_ELEMENTS(nonMem) EQ 0 THEN BEGIN
  ;;       nonMem = noMem
  ;;    ENDIF ELSE BEGIN
  ;;       IF nonMem NE noMem THEN BEGIN
  ;;          PRINT,"Ludicrosity. Tell me to do one thing and then the other."
  ;;          STOP
  ;;       ENDIF
  ;;    ENDELSE
  ;; ENDIF

  IF KEYWORD_SET(clear_memory) THEN BEGIN
     CLEAR_ION_DB_VARS,QUIET=quiet
     RETURN
  ENDIF

  CASE 1 OF
     KEYWORD_SET(downgoing): BEGIN
        ;; defNewellDBFile        = 'iSpec_down_20161228_db--PARSED--Orbs_500-16361.sav'

        ;; DB_date                = '20161228'
        ;; DB_version             = 'v0.0'
        ;; DB_extras              = ''

        defNewellDBFile        = 'downgoing_ions__combE__v0_1.sav'
        defNewellDBEphem       = 'downgoing_ions__ephem__v0_1.sav'

        needsEphem             = 1

        DB_date                = '20170214'
        DB_version             = 'v0.1'
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

  IF N_ELEMENTS(NEWELL_I__ion) NE 0 AND ~KEYWORD_SET(force_load_db) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(just_times): BEGIN
           IF ~quiet THEN PRINT,"Just giving ion times ..."
           out_times     = NEWELL_I__ion.x

           RETURN
        END
        KEYWORD_SET(noMem): BEGIN
           PRINT,"Moving ion structure/data in mem to outputted variables ..."
           ion              = TEMPORARY(NEWELL_I__ion     )
           ;; fastLoc_times    = TEMPORARY(FASTLOC__times)
           NewellDBFile     = TEMPORARY(NEWELL_I__dbFile    )
           NewellDBDir      = TEMPORARY(NEWELL_I__dbDir     )
           ion__delta_t     = N_ELEMENTS(NEWELL_I__delta_t  ) GT 0 ? TEMPORARY(NEWELL_I__delta_t  ) : !NULL 
        END
        ELSE: BEGIN
           PRINT,"There is already an ion DB in memory! If you want it to come out, set /NO_MEMORY_LOAD"
        END
     ENDCASE
     RETURN
  ENDIF

  ;;Pick up directory and file if not specified
  IF N_ELEMENTS(NewellDBDir) EQ 0 THEN BEGIN
     NewellDBDir            = defNewellDBDir
  ENDIF

  IF N_ELEMENTS(NewellDBFile) EQ 0 THEN BEGIN
     NewellDBFile           = defNewellDBFile
  ENDIF

  IF N_ELEMENTS(ion) NE 0 AND ~KEYWORD_SET(force_load_db) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(just_times): BEGIN
           IF ~quiet THEN PRINT,"Just giving ion times ..."
           out_times     = NEWELL_I__ion.x

           RETURN
        END
        KEYWORD_SET(noMem): BEGIN
           PRINT,"Moving ion structure/data in mem to outputted variables ..."
           ion              = TEMPORARY(NEWELL_I__ion     )
           ;; fastLoc_times    = TEMPORARY(FASTLOC__times)
           NewellDBFile     = TEMPORARY(NEWELL_I__dbFile    )
           NewellDBDir      = TEMPORARY(NEWELL_I__dbDir     )
           ion__delta_t     = N_ELEMENTS(NEWELL_I__delta_t  ) GT 0 ? TEMPORARY(NEWELL_I__delta_t  ) : !NULL 
        END
        ELSE: BEGIN
           PRINT,"There is already an ion DB in memory! If you want it to come out, set /NO_MEMORY_LOAD"
        END
     ENDCASE
  ENDIF ELSE BEGIN
     PRINTF,lun,'ion DB already loaded! Not restoring ' + NewellDBFile + '...'
  ENDELSE

  IF KEYWORD_SET(force_load_db) THEN BEGIN
     PRINTF,lun,"Forced loading of ion database ..."
  ENDIF
  PRINTF,lun,'Loading ion DB: ' + NewellDBFile + '...'
  RESTORE,NewellDBDir+NewellDBFile

  IF KEYWORD_SET(needsEphem) THEN BEGIN
     ion_info = ion.info
     jei      = ion.jei_lc
     ji       = (TEMPORARY(ion)).jei_lc

     RESTORE,NewellDBDir+defNewellDBEphem
     
     STR_ELEMENT,ephem,'info',/DELETE
     ion      = CREATE_STRUCT(TEMPORARY(ephem), $
                              "jei",jei, $
                              "ji",ji, $
                              ;; "chare",CHAR_ENERGY(ji,jei), $
                              'ion_info',TEMPORARY(ion_info))

  ENDIF

  IF KEYWORD_SET(load_charE) THEN BEGIN
     STR_ELEMENT,ion,'chare',CHAR_ENERGY(ion.ji,ion.jei),/ADD_REPLACE
     ion.info.has_charE = 1B
  ENDIF

  NEWELL_ESPEC__ADD_INFO_STRUCT,ion, $
                                /IONS, $
                                DB_DIR=NewellDBDir, $
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
     CASE 1 OF
        KEYWORD_SET(downgoing): BEGIN
           ;;Make Earthward positive
           inds                = WHERE(ion.ilat LT 0)
        END
        ELSE: BEGIN
           ;;Make outward positive
           inds                = WHERE(ion.ilat GT 0)
        END
     ENDCASE

     ion.ji[inds]              = (-1.)*(ion.ji[inds])
     ion.jei[inds]             = (-1.)*(ion.jei[inds])
     inds                      = !NULL
     ion.info.correctedFluxes  = 1B
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;What type of delta do you want?
  FASTDBS__DELTA_SWITCHER, $
     ion, $
     OUT_WIDTH_MEASURE=width_measure, $
     DBDIR=NewellDBDir, $
     LOAD_DELTA_T=load_delta_t, $
     LOAD_DELTA_ILAT_NOT_DELTA_T=load_dILAT, $
     LOAD_DELTA_ANGLE_FOR_WIDTH_TIME=load_dAngle, $
     LOAD_DELTA_X_FOR_WIDTH_TIME=load_dx, $
     DO_NOT_MAP_DELTA_T=do_not_map_delta_t, $
     DILAT_FILE=dILAT_file, $
     ;; FOR_ALFDB=alfDB, $
     ;; FOR_FASTLOC_DB=fastLocDB, $
     ;; /FOR_ESPEC_DB ;; , $
     /FOR_ION_DB

  IF N_ELEMENTS(width_measure) GT 0 THEN BEGIN
     ion__delta_t = TEMPORARY(width_measure)
  ENDIF

  ;; IF KEYWORD_SET(load_delta_t) THEN BEGIN
  ;;    PRINT,"Loading ion delta_ts ..."
  ;;    ion__delta_t = GET_ESPEC_ION_DELTA_T(ion, $
  ;;                                         DBNAME='ion')
  ;; ENDIF

  ;; delta_stuff = KEYWORD_SET(load_delta_t) + KEYWORD_SET(load_dILAT) + KEYWORD_SET(load_dx) + KEYWORD_SET(load_dAngle)
  ;; CASE delta_stuff OF
  ;;    0:
  ;;    1: BEGIN
  ;;       IF ~KEYWORD_SET(load_delta_t) THEN BEGIN
  ;;          dILAT_file         = GET_FAST_DB_STRING(ion,/FOR_ION_DB) + '-delta_ilats.sav'
  ;;          RESTORE,NewellDBDir+dILAT_file
  ;;       ENDIF
  ;;    END
  ;;    ELSE: BEGIN
  ;;       PRINT,"Can't have it all."
  ;;       STOP
  ;;    END
  ;; ENDCASE

  ;;Handle coordinates
  IF ~KEYWORD_SET(just_times) THEN BEGIN
     FASTDBS__COORDINATE_SWITCHER, $
        ion, $
        COORDINATE_SYSTEM=coordinate_system, $
        USE_LNG=use_lng, $
        USE_AACGM_COORDS=use_AACGM, $
        USE_GEI_COORDS=use_GEI, $
        USE_GEO_COORDS=use_GEO, $
        USE_MAG_COORDS=use_MAG, $
        USE_SDT_COORDS=use_SDT, $
        DEFCOORDDIR=defCoordDir, $
        AACGM_FILE=AACGM_file, $
        GEI_FILE=GEI_file, $
        GEO_FILE=GEO_file, $
        MAG_FILE=MAG_file, $
        SDT_FILE=SDT_file, $
        NO_MEMORY_LOAD=noMem

  ENDIF

  IF ~KEYWORD_SET(noMem) THEN BEGIN
     NEWELL_I__ion          = TEMPORARY(ion)

     IF KEYWORD_SET(delta_stuff) THEN BEGIN
        NEWELL_I__delta_t   = TEMPORARY(ion__delta_t)
     ENDIF

     NEWELL_I__dbFile       = TEMPORARY(NewellDBFile)
     NEWELL_I__dbDir        = TEMPORARY(NewellDBDir )

  ENDIF

  RETURN

END