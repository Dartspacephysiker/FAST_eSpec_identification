;2016/06/04
;;NOTE: We do not clean the current database. It's clean as a whistle.
PRO LOAD_NEWELL_ESPEC_DB,eSpec,eSpec__times,eSpec__delta_t, $
                         UPGOING=upgoing, $
                         GIGANTE=gigante, $
                         FINALDB=finalDB, $
                         FAILCODES=failCode, $
                         USE_UNSORTED_FILE=use_unsorted_file, $
                         NEWELLDBDIR=NewellDBDir, $
                         NEWELLDBFILE=NewellDBFile, $
                         FORCE_LOAD_DB=force_load_db, $
                         ;; DONT_LOAD_IN_MEMORY=nonMem, $
                         DONT_PERFORM_CORRECTION=dont_perform_SH_correction, $
                         DONT_CONVERT_TO_STRICT_NEWELL=dont_convert_to_strict_newell, $
                         DONT_MAP_TO_100KM=no_mapping, $
                         DO_NOT_MAP_FLUXES=do_not_map_fluxes, $
                         DO_NOT_MAP_DELTA_T=do_not_map_delta_t, $
                         LOAD_CHARE=load_charE, $
                         LOAD_DELTA_T=load_delta_t, $
                         COORDINATE_SYSTEM=coordinate_system, $
                         USE_LNG=use_lng, $
                         USE_AACGM_COORDS=use_AACGM, $
                         USE_GEI_COORDS=use_GEI, $
                         USE_GEO_COORDS=use_GEO, $
                         USE_MAG_COORDS=use_MAG, $
                         USE_SDT_COORDS=use_SDT, $
                         JUST_TIMES=just_times, $
                         ;; OUT_TIMES=out_times, $
                         ;; OUT_GOOD_I=good_i, $
                         LOAD_DELTA_ILAT_FOR_WIDTH_TIME=load_dILAT, $
                         LOAD_DELTA_ANGLE_FOR_WIDTH_TIME=load_dAngle, $
                         LOAD_DELTA_X_FOR_WIDTH_TIME=load_dx, $
                         USE_2000KM_FILE=use_2000km_file, $
                         CLEAR_MEMORY=clear_memory, $
                         NO_MEMORY_LOAD=noMem, $
                         REDUCED_DB=reduce_dbSize, $
                         LUN=lun, $
                         QUIET=quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__newell_espec.pro
  
  defNewellDBDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  defCoordDir            = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'


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
  ;;       ;; IF nonMem NE noMem THEN BEGIN
  ;;       ;;    PRINT,"Ludicrosity. Tell me to do one thing and then the other."
  ;;       ;;    STOP
  ;;       ;; ENDIF
  ;;    ENDELSE
  ;; ENDIF

  IF KEYWORD_SET(clear_memory) THEN BEGIN
     CLEAR_ESPEC_DB_VARS,QUIET=quiet
     RETURN
  ENDIF

  CASE 1 OF
     KEYWORD_SET(upgoing): BEGIN

        IF KEYWORD_SET(use_unsorted_file) THEN BEGIN
           PRINT,"Can't use unsorted with upgoing eSpec DB!"
           use_unsorted_file = 0
        ENDIF

        ;;The file without failcodes
        defSortNewellDBFile    = 'eSpec_up_20161229_db--PARSED--Orbs_500-16361.sav'
        DB_date                = '20161229'
        DB_version             = 'v0.0'
        DB_extras              = 'upgoing'

     END
     KEYWORD_SET(gigante): BEGIN

        IF KEYWORD_SET(use_unsorted_file) THEN BEGIN
           PRINT,"Can't use unsorted with gigante eSpec DB!"
           use_unsorted_file = 0
        ENDIF

        ;;The file without failcodes
        ;; defSortNewellDBFile    = 'eSpec_20170102_db--PARSED--Orbs_500-23999.sav'
        ;; DB_extras              = 'gigante'

        defNewellDBFile        = 'eSpec_20170203_db--PARSED--Orbs_500-24634.sav' 
        defNewellDBFile        = 'eSpecDB_20170203_v0_0--with_mapping_factors.sav' 

        DB_date                = '20170203'
        DB_version             = 'v0.0'
        DB_extras              = 'gigante/with_alternate_coords/with_mapping_factors'

        defSortNewellDBFile    =  defNewellDBFile

        GEI_file             = 'eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors-GEI.sav'
        GEO_file             = 'eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors-GEO.sav'
        MAG_file             = 'eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors-MAG.sav'
        SDT_file             = 'eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors-SDT.sav'

        AACGM_file           = 'Dartdb_20151222--500-16361_inc_lower_lats--maximus--AACGM_coords.sav'

     END
     KEYWORD_SET(finalDB): BEGIN

        IF KEYWORD_SET(use_unsorted_file) THEN BEGIN
           PRINT,"Can't use unsorted with gigante eSpec DB!"
           use_unsorted_file = 0
        ENDIF

        defNewellDBDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/v2018/'
        defCoordDir            = defNewellDBDir

        defNewellDBFile        = 'eMomDB_20181015-1000-11776-ephem.sav' 
        defNewellDBMomsFile    = 'eMomDB_20181015-1000-11776-LCangle_moms.sav'
        defNewellDBExtraFile   = 'eMomDB_20181015-1000-11776-extra.sav'

        DB_date                = '20181015'
        DB_version             = 'v0.0'
        DB_extras              = 'finalDB/with_alternate_coords/with_mapping_factors'

        defSortNewellDBFile    =  defNewellDBFile

        ;; GEI_file             = 'eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors-GEI.sav'
        ;; GEO_file             = 'eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors-GEO.sav'
        ;; MAG_file             = 'eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors-MAG.sav'
        ;; SDT_file             = 'eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors-SDT.sav'

        ;; AACGM_file           = 'Dartdb_20151222--500-16361_inc_lower_lats--maximus--AACGM_coords.sav'

     END
     ELSE: BEGIN
        ;;The file with failcodes
        ;; defNewellDBFile        = 'eSpec_failCodes_20160609_db--PARSED--Orbs_500-16361.sav' ;;This file does not need to be cleaned
        ;; DB_date                = '20160609'
        ;; DB_version             = 'v0.0'
        ;; DB_extras              = 'failcodes'

        ;;The file without failcodes
        defNewellDBFile        = 'eSpec_20160607_db--PARSED--with_mapping_factors--Orbs_500-16361.sav' ;;This file does not need to be cleaned
        DB_date                = '20160607'
        DB_version             = 'v0.0'
        DB_extras              = 'with_mapping_factors'

        defSortNewellDBFile    =  "sorted--" + defNewellDBFile

        GEI_file             = 'eSpec_DB_20160607-GEI.sav'
        GEO_file             = 'eSpec_DB_20160607-GEO.sav'
        MAG_file             = 'eSpec_DB_20160607-MAG.sav'
        MAG_file             = 'eSpecDB_20160607_v0_0--with_mapping_factors-MAG.sav'
        SDT_file             = 'eSpec_DB_20160607-SDT.sav'
        AACGM_file           = 'eSpec_DB_20160607-AACGM.sav'

        ;;The latest
        ;; defNewellDBFile        = 'eSpec_20170203_db--PARSED--Orbs_500-24634.sav' 
        ;; defNewellDBFile        = 'eSpecDB_20170203_v0_0--with_mapping_factors.sav' 

        ;; DB_date                = '20170203'
        ;; DB_version             = 'v0.0'
        ;; DB_extras              = 'with_mapping_factors'

        ;; defSortNewellDBFile    =  defNewellDBFile

        ;; GEI_file             = 'eSpec_DB_20170203_v0_0--with_mapping_factors-GEI.sav'
        ;; GEO_file             = 'eSpec_DB_20170203_v0_0--with_mapping_factors-GEO.sav'
        ;; MAG_file             = 'eSpec_DB_20170203_v0_0--with_mapping_factors-MAG.sav'
        ;; SDT_file             = 'eSpec_DB_20170203_v0_0--with_mapping_factors-SDT.sav'

        ;; AACGM_file           = 'Dartdb_20151222--500-16361_inc_lower_lats--maximus--AACGM_coords.sav'

        ;;the 2000 km file
        IF KEYWORD_SET(use_2000km_file) THEN BEGIN
           PRINT,'Using 2000 km eSpec DB ...'
           defNewellDBFile     = 'eSpec_20160607_db--orbs_500-16361--BELOW_2000km--with_alternate_coords__mapping_factors__strict_Newell_interp.sav'
           defSortNewellDBFile =  defNewellDBFile
           DB_date             = '20160607'
           DB_version          = 'v0.0'
           DB_extras           = 'Below_2000km/with_alternate_coords/with_mapping_factors/strict_Newell_interp'
           AACGM_file          = !NULL
           GEO_file            = !NULL
           MAG_file            = !NULL
        ENDIF

        ;; defNewellDBCleanInds   = 'iSpec_20160607_db--PARSED--Orbs_500-16361--indices_w_no_NaNs_INFs.sav'

     END
  ENDCASE

  IF N_ELEMENTS(NEWELL__eSpec) NE 0 AND ~KEYWORD_SET(force_load_db) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(just_times): BEGIN
           IF ~quiet THEN PRINT,"Just giving eSpec times ..."
           out_times     = NEWELL__eSpec.x

           RETURN
        END
        KEYWORD_SET(noMem): BEGIN
           PRINT,"Moving eSpec structure/data in mem to outputted variables ..."
           eSpec            = KEYWORD_SET(just_times) ? !NULL : TEMPORARY(NEWELL__eSpec     )
           eSpec__times     = KEYWORD_SET(just_times) ? (TEMPORARY(NEWELL__eSpec)).x : !NULL
           NewellDBFile     = TEMPORARY(NEWELL__dbFile    )
           NewellDBDir      = TEMPORARY(NEWELL__dbDir     )
           failCodes        = N_ELEMENTS(NEWELL__failCodes) GT 0 ? TEMPORARY(NEWELL__failCodes) : !NULL
           eSpec__delta_t   = N_ELEMENTS(NEWELL__delta_t  ) GT 0 ? TEMPORARY(NEWELL__delta_t  ) : !NULL 
        END
        ELSE: BEGIN
           PRINT,"There is already an eSpec DB in memory! If you want it to come out, set /NO_MEMORY_LOAD"
        END
     ENDCASE
     RETURN
  ENDIF


  ;;Pick up directory and file if not specified
  IF N_ELEMENTS(NewellDBDir) EQ 0 THEN BEGIN
     NewellDBDir      = defNewellDBDir
  ENDIF

  IF N_ELEMENTS(NewellDBFile) EQ 0 THEN BEGIN
     CASE KEYWORD_SET(use_unsorted_file) OF
        1: BEGIN
           specType      = 'UNSORTED'
           NewellDBFile  = defNewellDBFile
        END
        ELSE: BEGIN
           specType      = 'SORTED'
           NewellDBFile  = defSortNewellDBFile
        END
     ENDCASE
  ENDIF

  ;;If just getting times, at this point we've populated other variables
  ;;that might be of interest to user, so JET
  IF KEYWORD_SET(just_times) AND N_ELEMENTS(eSpec__times) GT 0 THEN RETURN

  IF N_ELEMENTS(specType) EQ 0 THEN specType = ''
  IF N_ELEMENTS(eSpec) EQ 0 OR KEYWORD_SET(force_load_db) THEN BEGIN
     IF KEYWORD_SET(force_load_db) THEN BEGIN
        IF ~quiet THEN PRINTF,lun,"Forced loading of " + specType + " eSpec database ..."
     ENDIF
     IF ~quiet THEN PRINTF,lun,'Loading ' + specType + ' eSpec DB: ' + NewellDBFile + '...'
     RESTORE,NewellDBDir+NewellDBFile

     IF KEYWORD_SET(use_2000km_file) THEN BEGIN
        eSpec    = {x          : eSpec.x                , $
                    orbit      : eSpec.orbit            , $
                    ;; mlt     : eSpec.coords.aacgm.mlt , $
                    ;; ilat    : eSpec.coords.aacgm.lat , $
                    ;; alt     : eSpec.coords.aacgm.alt , $
                    mlt        : eSpec.coords.SDT.mlt   , $
                    ilat       : eSpec.coords.SDT.ilat  , $
                    alt        : eSpec.coords.SDT.alt   , $
                    je         : eSpec.je               , $
                    jee        : eSpec.jee              , $
                    mapFactor  : eSpec.mapFactor        , $
                    mono       : eSpec.mono             , $
                    broad      : eSpec.broad            , $
                    diffuse    : eSpec.diffuse          , $
                    info       : eSpec.info}
     ENDIF

     IF KEYWORD_SET(just_times) THEN BEGIN
        PRINT,"Here you go, sport!"
        eSpec__times = (TEMPORARY(eSpec)).x

        RETURN

     ENDIF
     
     IF ~KEYWORD_SET(finalDB) THEN BEGIN
        ;;want characteristic energy? You sure?
        charE = ABS(eSpec.jee/eSpec.je)*6.242*1.0e11
        these = WHERE((eSpec.broad EQ 1) OR (eSpec.broad EQ 2) AND $
                      ( ( (charE LT 80 ) AND  (eSpec.mlt GE 9.5) AND (eSpec.mlt LE 14.5) ) OR $
                        ( (charE LT 80 ) AND ((eSpec.mlt LT 9.5) OR  (eSpec.mlt GT 14.5) )    )),nNonsense)
        IF nNonsense GT 0 THEN BEGIN
           PRINT,"There are " + STRCOMPRESS(nNonsense) + ' inds that cannot be broad, and yet are, Spence. Figure it out.'
           eSpec.broad[TEMPORARY(these)] = 255-10-2
        ENDIF
        IF KEYWORD_SET(load_charE) THEN BEGIN
           STR_ELEMENT,eSpec,'charE',TEMPORARY(charE),/ADD_REPLACE
        ENDIF

     ENDIF ELSE BEGIN

        eSpec = TEMPORARY(ephem)

     ENDELSE

     NEWELL_ESPEC__ADD_INFO_STRUCT,eSpec, $
                                  DB_DIR=NewellDBDir, $
                                  DB_DATE=DB_date, $
                                  DB_VERSION=DB_version, $
                                  DB_EXTRAS=DB_extras, $
                                  REDUCE_DBSIZE=reduce_dbSize

     eSpec.info.has_charE     = BYTE(KEYWORD_SET(load_charE))

     eSpec.info.is_gigante    = KEYWORD_SET(gigante)
     eSpec.info.is_final2018  = KEYWORD_SET(finalDB)
     IF KEYWORD_SET(upgoing) THEN BEGIN
        eSpec.info.is_upgoing = 1B
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;What type of delta do you want?
     FASTDBS__DELTA_SWITCHER, $
        eSpec, $
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
        /FOR_ESPEC_DB, $
        DELTA_STUFF=delta_stuff ;; , $
     ;; FOR_ION_DB=ionDB

     IF N_ELEMENTS(width_measure) GT 0 THEN BEGIN
        eSpec__delta_t = TEMPORARY(width_measure)
     ENDIF

     ;; delta_stuff = KEYWORD_SET(load_delta_t) + KEYWORD_SET(load_dILAT) + KEYWORD_SET(load_dx) + KEYWORD_SET(load_dAngle)
     ;; CASE delta_stuff OF
     ;;    0:
     ;;    1: BEGIN
     ;;       IF ~KEYWORD_SET(load_delta_t) THEN BEGIN
     ;;          dILAT_file         = GET_FAST_DB_STRING(eSpec,/FOR_ESPEC_DB) + '-delta_ilats.sav'
     ;;          RESTORE,NewellDBDir+dILAT_file
     ;;       ENDIF
     ;;    END
     ;;    ELSE: BEGIN
     ;;       PRINT,"Can't have it all."
     ;;       STOP
     ;;    END
     ;; ENDCASE

     ;; IF KEYWORD_SET(load_delta_t) THEN BEGIN
     ;;    PRINT,"Loading eSpec delta_ts ..."
     ;;    eSpec__delta_t = GET_ESPEC_ION_DELTA_T(eSpec, $
     ;;                                           DBNAME='eSpec')
     ;; ENDIF

     ;; IF KEYWORD_SET(load_dILAT) THEN BEGIN
     ;;    PRINT,"Loading dILAT in place of eSpec delta_t, and not mapping ..."
        
     ;;    ;; no_mapping              = 1
     ;;    do_not_map_delta_t      = 1

     ;;    eSpec__delta_t          = TEMPORARY(ABS(FLOAT(width_ILAT)))
     ;;    eSpec.info.dILAT_not_dt = 1B
     ;; ENDIF

     ;; IF KEYWORD_SET(load_dAngle) THEN BEGIN
     ;;    PRINT,"Loading dAngle in place of eSpec delta_t, and not mapping ..."
        
     ;;    ;; no_mapping               = 1
     ;;    do_not_map_delta_t       = 1

     ;;    eSpec__delta_t           = TEMPORARY(ABS(FLOAT(width_angle)))
     ;;    eSpec.info.dAngle_not_dt = 1B
     ;; ENDIF

     ;; IF KEYWORD_SET(load_dx) THEN BEGIN
     ;;    PRINT,"Loading dx in place of eSpec delta_t, and not mapping ..."
        
     ;;    ;; no_mapping              = 1
     ;;    do_not_map_delta_t      = 1

     ;;    eSpec__delta_t          = TEMPORARY(ABS(FLOAT(width_x)))
     ;;    eSpec.info.dx_not_dt    = 1B
     ;; ENDIF

     IF KEYWORD_SET(no_mapping) OR KEYWORD_SET(do_not_map_fluxes) THEN BEGIN
        PRINT,"Not mapping to 100 km ..."
     ENDIF ELSE BEGIN
        PRINT,"Mapping eSpec observations to 100 km ..."
        eSpec.je  *= eSpec.mapFactor
        eSpec.jee *= eSpec.mapFactor

        eSpec.info.is_mapped = 1B
     ENDELSE

     IF ~(KEYWORD_SET(do_not_map_delta_t) OR KEYWORD_SET(no_mapping)) $
        AND (N_ELEMENTS(eSpec__delta_t) GT 0) $
     THEN BEGIN
        eSpec__delta_t         /= SQRT(eSpec.mapFactor)
        eSpec.info.dt_is_mapped = 1B
     ENDIF

     ;; STR_ELEMENT,eSpec,'mapFactor',/DELETE

     IF KEYWORD_SET(reduce_dbSize) THEN BEGIN
        PRINT,"Reducing eSpec DB size, tossing out possibly extraneous members ..."

        IF MAX(eSpec.orbit) GT 65534 THEN BEGIN
           PRINT,"You're about to descend into confusion if you shrink tag member ORBIT for this database."
           STOP                 ;Because the tag ORBIT is type UINT
        ENDIF

        eSpec   = {x           : eSpec.x           , $
                   orbit       : UINT(eSpec.orbit) , $
                   mlt         : FLOAT(eSpec.mlt)  , $
                   ilat        : FLOAT(eSpec.ilat) , $
                   alt         : FLOAT(eSpec.alt)  , $
                   mono        : eSpec.mono        , $
                   broad       : eSpec.broad       , $
                   diffuse     : eSpec.diffuse     , $
                   je          : eSpec.je          , $
                   jee         : eSpec.jee         , $
                   info        : eSpec.info        }
     ENDIF

     ;;Correct fluxes
     IF ~(KEYWORD_SET(dont_perform_SH_correction) OR (KEYWORD_SET(just_times) AND KEYWORD_SET(dont_perform_SH_correction))) THEN BEGIN
        IF ~quiet THEN PRINT,"Correcting eSpec fluxes so that earthward is positive in SH..."
        
        ;;The following line says that if there are a lot of jee values in the Southern Hemi that are negative, we need to perform a conversion
        ;;to make earthward positive in the SH
        IF FLOAT(N_ELEMENTS(WHERE(eSpec.jee LT 0 AND eSpec.ilat LT 0)))/N_ELEMENTS(WHERE(eSpec.ilat LT 0)) GT 0.1 THEN BEGIN
           eSpec.je [WHERE(eSpec.ilat LT 0)] = (-1.)*(eSpec.je [WHERE(eSpec.ilat LT 0)])
           eSpec.jee[WHERE(eSpec.ilat LT 0)] = (-1.)*(eSpec.jee[WHERE(eSpec.ilat LT 0)])
        ENDIF
        
        ;;Book keeping
        eSpec.info.correctedFluxes = 1B

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

  ENDIF ELSE BEGIN
     IF ~quiet THEN PRINTF,lun,'eSpec DB already loaded! Not restoring ' + NewellDBFile + '...'
  ENDELSE

  ;;Handle coordinates
  IF ~KEYWORD_SET(just_times) THEN BEGIN

     FASTDBS__COORDINATE_SWITCHER, $
        eSpec, $
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
     NEWELL__eSpec          = TEMPORARY(eSpec)

     IF KEYWORD_SET(delta_stuff) THEN BEGIN
        NEWELL__delta_t     = TEMPORARY(eSpec__delta_t)
     ENDIF

     NEWELL__dbFile         = TEMPORARY(NewellDBFile)
     NEWELL__dbDir          = TEMPORARY(NewellDBDir )

     IF N_ELEMENTS(failCode) NE 0 THEN BEGIN
        NEWELL__failCodes   = TEMPORARY(failCode)
     ENDIF ELSE BEGIN
        NEWELL__failCodes   = !NULL
        IF ~quiet THEN PRINT,'This Newell DB file doesn''t have fail codes!'
     ENDELSE

  ENDIF

  RETURN
END