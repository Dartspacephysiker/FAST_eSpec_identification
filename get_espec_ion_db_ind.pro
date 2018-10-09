;***********************************************
;2016/06/07
;It's like GET_CHASTON_IND for the electron and ion DBs
FUNCTION GET_ESPEC_ION_DB_IND,dbStruct,lun, $
                              FOR_ALFVEN_DB=for_alfven_db, $
                              DESPUN_ALF_DB=despun_alf_db, $
                              DBFILE=dbfile, $
                              DBDIR=dbDir, $
                              CHARIERANGE=charIERange, $
                              ALFDB_PLOT_STRUCT=alfDB_plot_struct, $
                              IMF_STRUCT=IMF_struct, $
                              MIMC_STRUCT=MIMC_struct, $
                              USE_AACGM=use_aacgm, $
                              GET_ESPEC_I_NOT_ION_I=get_eSpec_i, $
                              GET_ION_I=get_ion_i, $
                              RESET_GOOD_INDS=reset_good_inds, $
                              DO_NOT_SET_DEFAULTS=do_not_set_defaults, $
                              DONT_LOAD_IN_MEMORY=noMem, $
                              PRINT_PARAM_SUMMARY=print_param_summary
  
  COMPILE_OPT IDL2,STRICTARRSUBS
  
  @common__mlt_ilat_magc_etc.pro

  IF ~KEYWORD_SET(noMem) THEN BEGIN
     
     IF KEYWORD_SET(for_alfven_db) THEN BEGIN
        ;;This common block is defined ONLY here, in GET_H2D_NEWELLS__EACH_TYPE, and in LOAD_ALF_NEWELL_ESPEC_DB
  @common__newell_alf.pro

  ;;This common block is defined ONLY here, in GET_H2D_NEWELLS__EACH_TYPE, and in LOAD_ALF_NEWELL_ION_DB
  @common__newell_alf_i.pro

  ENDIF ELSE BEGIN

     ;;This common block is defined ONLY here and in LOAD_NEWELL_ION_DB, I believe
     @common__newell_ion_db.pro
     ;; COMMON NEWELL_I,NEWELL_I__ion,NEWELL_I__HAVE_GOOD_I, $
     ;;    NEWELL_I__good_i,NEWELL_I__cleaned_i, $
     ;;    NEWELL_I__dbFile,NEWELL_I__dbDir, $
     ;;    NEWELL_I__charIERange, $
     ;;    NEWELL_I__RECALCULATE
     
     ;;This common block is defined ONLY here, in GET_H2D_NEWELLS__EACH_TYPE, and in LOAD_NEWELL_ESPEC_DB
    @common__newell_espec.pro
     

  ENDELSE

  ENDIF
                                ;For statistical auroral oval
  ;; defHwMAurOval=0
  ;; defHwMKpInd=7

  defLun                                          = -1

  ;; defPrintSummary                              = 0

  IF ~KEYWORD_SET(lun) THEN lun                   = defLun ;stdout

  IF KEYWORD_SET(use_AACGM) THEN BEGIN
     PRINT,"Fool! This hasn't been set up yet. You have to convert that unimaginably large database to other coordinates."
     STOP
  ENDIF

  IF N_ELEMENTS(MIMC_struct) EQ 0 THEN BEGIN
     STOP
     SET_DEFAULT_MLT_ILAT_AND_MAGC,MINMLT=minM,MAXMLT=maxM,BINM=binM, $
                                   MINILAT=minI,MAXILAT=maxI,BINI=binI, $
                                   MINLSHELL=minL,MAXLSHELL=maxL,BINL=binL, $
                                   MIN_MAGCURRENT=minMC,MAX_NEGMAGCURRENT=maxNegMC, $
                                   HEMI=hemi, $
                                   BOTH_HEMIS=both_hemis, $
                                   NORTH=north, $
                                   SOUTH=south, $
                                   MIMC_STRUCT=MIMC_struct, $
                                   LUN=lun
  ENDIF

  ;;;;;;;;;;;;;;;
  ;;Check whether this is a maximus or fastloc struct
  IF KEYWORD_SET(dbStruct) THEN BEGIN
     IF KEYWORD_SET(get_eSpec_i) THEN BEGIN
        is_ion     = 0
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(get_ion_i) THEN BEGIN
           is_ion  = 1
        ENDIF ELSE BEGIN
           IS_STRUCT_ION_OR_ESPEC,dbStruct,is_ion
        ENDELSE
     ENDELSE
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(get_eSpec_i) THEN BEGIN
        is_ion     = 0
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(get_ion_i) THEN BEGIN
           is_ion  = 1
        ENDIF
     ENDELSE
  ENDELSE

  IF ~KEYWORD_SET(get_ion_i) AND ~KEYWORD_SET(get_eSpec_i) AND ~KEYWORD_SET(dbStruct) THEN BEGIN
     PRINTF,lun,"Assuming this is an ion DB ..."
     is_ion  = 1 ;We assume this is maximus
  ENDIF

  ;;Get the databases if they're already in mem
  IF is_ion THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(for_alfven_db): BEGIN
           IF N_ELEMENTS(NWLL_ALF_I__ion) NE 0 THEN BEGIN
              dbStruct                                  = NWLL_ALF_I__ion
              dbFile                                    = NWLL_ALF_I__dbFile
              dbDir                                     = NWLL_ALF_I__dbDir
           ENDIF ELSE BEGIN
              LOAD_ALF_NEWELL_ION_DB, $ ;ion, $
                 alf_i__good_iSpec, $
                 good_iSpec_i, $
                 ;; FAILCODES=failCodes, $
                 NEWELLDBDIR=dbDir, $
                 NEWELLDBFILE=dbFile, $
                 FORCE_LOAD_DB=force_load_db, $
                 DONT_LOAD_IN_MEMORY=noMem, $
                 ;; OUT_CLEANED_I=cleaned_i, $
                 LUN=lun
              
              ;; IF ~KEYWORD_SET(nonMem) THEN BEGIN
              ;;    ;; NWLL_ALF_I__iSpec                      = iSpec
              ;;    NWLL_ALF_I__good_iSpec_i               = good_iSpec_i
              ;;    NWLL_ALF_I__good_alf_i                 = alf_i__good_iSpec
              ;;    NWLL_ALF_I__despun                     = KEYWORD_SET(despun_alf_db)
              ;;    NWLL_ALF_I__dbFile                     = dbFile
              ;;    NWLL_ALF_I__dbDir                      = dbDir

              ;;    ;; NWLL_ALF_I__cleaned_i    = cleaned_i
              ;; ENDIF
           ENDELSE
        END
        ELSE: BEGIN
           IF N_ELEMENTS(NEWELL_I__ion) NE 0 THEN BEGIN
              dbStruct  = NEWELL_I__ion
              dbFile    = NEWELL_I__dbFile
              dbDir     = NEWELL_I__dbDir
           ENDIF ELSE BEGIN
              LOAD_NEWELL_ION_DB,ion, $
                                 NEWELLDBDIR=dbDir, $
                                 NEWELLDBFILE=dbFile, $
                                 FORCE_LOAD_DB=force_load_db, $
                                 NO_MEMORY_LOAD=noMem, $
                                 LUN=lun

              ;; IF ~KEYWORD_SET(noMem) THEN BEGIN
              ;;    NEWELL_I__ion          = ion
              ;; ENDIF
           ENDELSE
        END
     ENDCASE

  ENDIF ELSE BEGIN ;;'Cause if ~is_ion, then it must is_electron!
     CASE 1 OF
        KEYWORD_SET(for_alfven_db): BEGIN
           IF N_ELEMENTS(NWLL_ALF__eSpec) NE 0 THEN BEGIN
              dbStruct  = NWLL_ALF__eSpec
              dbFile    = NWLL_ALF__dbFile
              dbDir     = NWLL_ALF__dbDir
           ENDIF ELSE BEGIN
              LOAD_ALF_NEWELL_ESPEC_DB,dbStruct,alf_i__good_eSpec,good_eSpec_i, $
                                   FAILCODES=failCodes, $
                                   NEWELLDBDIR=dbDir, $
                                   NEWELLDBFILE=dbFile, $
                                   FORCE_LOAD_DB=force_load_db, $
                                   DESPUN_ALF_DB=despun_alf_db, $
                                   DONT_LOAD_IN_MEMORY=noMem, $
                                   ;; OUT_GOOD_I=good_i, $
                                   LUN=lun
              IF ~KEYWORD_SET(noMem) THEN BEGIN
                 NWLL_ALF__eSpec         = dbStruct
                 NWLL_ALF__failCodes     = failCodes
                 NWLL_ALF__good_eSpec_i  = good_eSpec_i
                 NWLL_ALF__good_alf_i    = alf_i__good_eSpec
                 NWLL_ALF__despun        = KEYWORD_SET(despun_alf_db)
                 NWLL_ALF__dbFile        = dbFile
                 NWLL_ALF__dbDir         = dbDir
              ENDIF
           ENDELSE
        END
        ELSE: BEGIN
           IF N_ELEMENTS(NEWELL__eSpec) NE 0 THEN BEGIN
              dbStruct                   = NEWELL__eSpec
              dbFile                     = NEWELL__dbFile
              dbDir                      = NEWELL__dbDir
           ENDIF ELSE BEGIN
              LOAD_NEWELL_ESPEC_DB,dbStruct, $
                                   FAILCODES=failCodes, $
                                   NEWELLDBDIR=dbDir, $
                                   NEWELLDBFILE=dbFile, $
                                   FORCE_LOAD_DB=force_load_db, $
                                   ;; DONT_LOAD_IN_MEMORY=noMem, $
                                   NO_MEMORY_LOAD=noMem, $
                                   DONT_CONVERT_TO_STRICT_NEWELL=~KEYWORD_SET(eSpec__Newell_2009_interp), $
                                   USE_2000KM_FILE=eSpec__use_2000km_file, $
                                   ;; OUT_GOOD_I=good_i, $
                                   LUN=lun

              IF ~KEYWORD_SET(noMem) THEN BEGIN
                 NEWELL__eSpec         = TEMPORARY(dbStruct)
                 IF N_ELEMENTS(failCodes) GT 0 THEN BEGIN
                    NEWELL__failCodes  = TEMPORARY(failCodes)
                 ENDIF
                 NEWELL__dbFile        = TEMPORARY(dbFile)
                 NEWELL__dbDir         = TEMPORARY(dbDir)
              ENDIF

           ENDELSE
        END
     ENDCASE

  ENDELSE

  ;;Now check to see whether we have the appropriate vars for each guy
  IF ~is_ion THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(for_alfven_db): BEGIN
           have_em                        = KEYWORD_SET(NWLL_ALF__HAVE_GOOD_I)
           goodIStr                       = 'NWLL_ALF__good_i'
        END
        ELSE: BEGIN
           have_em                        = KEYWORD_SET(NEWELL__HAVE_GOOD_I)
           goodIStr                       = 'NEWELL__good_i'
        END
     ENDCASE

     IF ~KEYWORD_SET(have_em) OR KEYWORD_SET(reset_good_inds) THEN BEGIN
        IF KEYWORD_SET(reset_good_inds) THEN BEGIN
           PRINT,'Resetting ' + goodIStr + ' ...'
        ENDIF
        calculate                         = 1
     ENDIF ELSE BEGIN
        CASE 1 OF
           KEYWORD_SET(for_alfven_db): BEGIN
              nonzero_good_i              = N_ELEMENTS(NWLL_ALF__good_i)
           END
           ELSE: BEGIN
              nonzero_good_i              = N_ELEMENTS(NEWELL__good_i)
           END
        ENDCASE
        IF nonzero_good_i NE 0 THEN BEGIN
           CHECK_FOR_NEW_ESPEC_ION_IND_CONDS,is_ion, $
                                             KEYWORD_SET(for_alfven_db) ? NWLL_ALF__RECALCULATE : NEWELL__RECALCULATE, $
                                             IMF_STRUCT=IMF_struct, $
                                             MIMC_STRUCT=MIMC_struct, $
                                             ALFDB_PLOT_STRUCT=alfDB_plot_struct, $
                                             HAVE_GOOD_I=have_good_i, $
                                             LUN=lun
           calculate                      = KEYWORD_SET(for_alfven_db) ? NWLL_ALF__RECALCULATE : NEWELL__RECALCULATE
           CASE 1 OF
              KEYWORD_SET(for_alfven_db): BEGIN
                 NWLL_ALF__HAVE_GOOD_I    = have_good_i
                 NWLL_ALF_I__RECALCULATE  = calculate
              END
              ELSE: BEGIN
                 NEWELL__HAVE_GOOD_I      = have_good_i
                 NEWELL_I__RECALCULATE    = calculate ;;reset other DB too
              END
           ENDCASE
        ENDIF ELSE BEGIN
           PRINT,'But you should already have ' + goodIStr + '!!'
           STOP
        ENDELSE
     ENDELSE
  ENDIF ELSE BEGIN
     CASE 1 OF
        KEYWORD_SET(for_alfven_db): BEGIN
           have_em                        = KEYWORD_SET(NWLL_ALF_I_HAVE_GOOD_I)
           goodIStr                       = 'NWLL_ALF_I__good_i'
        END
        ELSE: BEGIN
           have_em                        = KEYWORD_SET(NEWELL_I_HAVE_GOOD_I)
           goodIStr                       = 'NEWELL_I__good_i'
        END
     ENDCASE

     IF ~KEYWORD_SET(have_em) OR KEYWORD_SET(reset_good_inds) THEN BEGIN
        IF KEYWORD_SET(reset_good_inds) THEN BEGIN
           PRINT,'Resetting good ' + goodIStr + '...'
        ENDIF
        calculate                         = 1
     ENDIF ELSE BEGIN
        CASE 1 OF
           KEYWORD_SET(for_alfven_db): BEGIN
              nonzero_good_i              = N_ELEMENTS(NWLL_ALF_I__good_i)
           END
           ELSE: BEGIN
              nonzero_good_i              = N_ELEMENTS(NEWELL_I__good_i)
              
           END
        ENDCASE

        IF nonzero_good_i NE 0 THEN BEGIN
           CHECK_FOR_NEW_ESPEC_ION_IND_CONDS,is_ion, $
                                             KEYWORD_SET(for_alfven_db) ? NWLL_ALF_I__RECALCULATE : NEWELL_I__RECALCULATE, $
                                             IMF_STRUCT=IMF_struct, $
                                             MIMC_STRUCT=MIMC_struct, $
                                             ALFDB_PLOT_STRUCT=alfDB_plot_struct, $
                                             HAVE_GOOD_I=have_good_i, $
                                             LUN=lun
           calculate                      = KEYWORD_SET(for_alfven_db) ? NWLL_ALF_I__RECALCULATE : NEWELL_I__RECALCULATE
           CASE 1 OF
              KEYWORD_SET(for_alfven_db): BEGIN
                 NWLL_ALF_I__HAVE_GOOD_I  = have_good_i
                 NWLL_ALF__RECALCULATE    = calculate
              END
              ELSE: BEGIN
                 NEWELL_I__HAVE_GOOD_I    = have_good_i
                 NEWELL__RECALCULATE      = calculate ;;make sure to recalculate other DB too
              END
           ENDCASE
        ENDIF ELSE BEGIN
           PRINT,'But you should already have ' + goodIStr + '!!'
           STOP
        ENDELSE
     ENDELSE
  ENDELSE

  IF KEYWORD_SET(calculate) THEN BEGIN

     ;; IF ~KEYWORD_SET(HwMAurOval) THEN HwMAurOval  = defHwMAurOval
     ;; IF ~KEYWORD_SET(HwMKpInd) THEN HwMKpInd      = defHwMKpInd


     ;;Welcome message
     PRINTF,lun,""
     PRINTF,lun,"****From GET_ESPEC_ION_DB_IND****"
     PRINTF,lun,FORMAT='("DBFile                        :",T35,A0)',dbFile
     PRINTF,lun,""

     ;;;;;;;;;;;;
     ;;Handle longitudes
     MIMC__minMLT         = MIMC_struct.minM * (KEYWORD_SET(MIMC_struct.use_Lng) ? 15. : 1.)
     MIMC__maxMLT         = MIMC_struct.maxM * (KEYWORD_SET(MIMC_struct.use_Lng) ? 15. : 1.)
     MIMC__binMLT         = MIMC_struct.binM * (KEYWORD_SET(MIMC_struct.use_Lng) ? 15. : 1.)
     MIMC__dayside        = KEYWORD_SET(dayside)
     MIMC__nightside      = KEYWORD_SET(nightside)
     mlt_i                = GET_MLT_INDS(dbStruct,MIMC__minMLT, $
                                         MIMC__maxMLT, $
                                         DAYSIDE=MIMC__dayside, $
                                         NIGHTSIDE=MIMC__nightside, $
                                         N_MLT=n_mlt, $
                                         N_OUTSIDE_MLT=n_outside_MLT, $
                                         USE_LNG=MIMC_struct.use_Lng, $
                                         LUN=lun)
     
     ;;;;;;;;;;;;
     ;;Handle latitudes, combine with mlt
     MIMC__hemi           = MIMC_struct.hemi
     MIMC__north          = KEYWORD_SET(MIMC_struct.north)
     MIMC__south          = KEYWORD_SET(MIMC_struct.south)
     MIMC__both_hemis     = KEYWORD_SET(MIMC_struct.both_hemis)
     MIMC__globe          = KEYWORD_SET(MIMC_struct.globe)
     IF KEYWORD_SET(do_lShell) THEN BEGIN

        MIMC__minLshell   = MIMC_struct.minL
        MIMC__maxLshell   = MIMC_struct.maxL
        MIMC__binLshell   = MIMC_struct.binL

        lshell_i          = GET_LSHELL_INDS(dbStruct,MIMC__minLshell,MIMC__maxLshell, $
                                            MIMC__hemi, $
                                            N_LSHELL=n_lshell, $
                                            N_NOT_LSHELL=n_not_lshell, $
                                            LUN=lun)
        region_i          = CGSETINTERSECTION(lshell_i,mlt_i)
     ENDIF ELSE BEGIN

        MIMC__minILAT     = MIMC_struct.minI
        MIMC__maxILAT     = MIMC_struct.maxI
        MIMC__binILAT     = MIMC_struct.binI

        MIMC__EA_binning  = KEYWORD_SET(alfDB_plot_struct.EA_binning)

        ilat_i            = GET_ILAT_INDS(dbStruct,MIMC__minILAT,MIMC__maxILAT, $
                                          MIMC__hemi, $
                                          N_ILAT=n_ilat, $
                                          N_NOT_ILAT=n_not_ilat, $
                                          LUN=lun)
        region_i          = CGSETINTERSECTION(ilat_i,mlt_i)
     ENDELSE

     ;;And if we kill stuff right now?
     killGap_file = GET_NEWELL_ESPEC_KILLGAP_FILE(dbStruct, $
                                                  NEWELLDBDIR=dbDir, $
                                                  FOR_ESPEC_DB=~is_ion, $
                                                  FOR_ION_DB=is_ion, $
                                                  /STOP_IF_NOEXIST)
     RESTORE,killGap_file

     nGood       = N_ELEMENTS(region_i)
     region_i    = CGSETINTERSECTION(region_i,TEMPORARY(keep_i),COUNT=nKept)
     PRINT,"Lost " + STRCOMPRESS(nGood - nKept,/REMOVE_ALL) + $
           ' inds to the killedGap ind thing ...'

     ;;Kill everything below 35 deg MAG lat (trust me, see the output from JOURNAL__20170201__ESPEC_PLOTS__DURING_STORMPHASES__ALTERNATE_COORDS
     ;; PRINT,"WARNING: YOU'VE GOT STUFF FROM >35deg MAG LAT IN FASTLOC"
     ;; PRINT,"WARNING: YOU'VE GOT STUFF FROM >35deg MAG LAT IN FASTLOC"
     ;; PRINT,"WARNING: YOU'VE GOT STUFF FROM >35deg MAG LAT IN FASTLOC"
     ;; PRINT,"WARNING: YOU'VE GOT STUFF FROM >35deg MAG LAT IN FASTLOC"
     ;; PRINT,"WARNING: YOU'VE GOT STUFF FROM >35deg MAG LAT IN FASTLOC"
     IF ~KEYWORD_SET(is_ion) THEN BEGIN
        GET_INDS_ABOVE_35_MAGLAT,dBStruct,region_i, $
                                 FOR_ESPEC_DB=~is_ion, $
                                 FOR_ION_DB=is_ion
     ENDIF
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Want just Holzworth/Meng statistical auroral oval?
     IF KEYWORD_SET(alfDB_plot_struct.HwMAurOval) THEN BEGIN
        region_i = CGSETINTERSECTION(region_i, $
                                     WHERE(ABS(dbStruct.ilat) GT AURORAL_ZONE(dbStruct.mlt,HwMKpInd,/lat)/(!DPI)*180.))
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;
     ;;Now combine them all
     IF KEYWORD_SET(do_lShell) THEN BEGIN
     ENDIF ELSE BEGIN
     ENDELSE

     ;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Limits on orbits to use?
     test = !NULL
     STR_ELEMENT,alfDB_plot_struct,'orbRange',test
     IF SIZE(test,/TYPE) NE 0 THEN BEGIN
        MIMC__orbRange        = alfDB_plot_struct.orbRange
        CASE N_ELEMENTS(alfDB_plot_struct.orbRange) OF
           1: BEGIN
              MIMC__orbRange  = [alfDB_plot_struct.orbRange,alfDB_plot_struct.orbRange]
           END
           2: BEGIN
              MIMC__orbRange  = alfDB_plot_struct.orbRange
           END
           ELSE: BEGIN
              PRINTF,lun,"Assuming you want me to believe you about this orbit array ..."
              is_orbArr       = 1
              MIMC__orbRange  = alfDB_plot_struct.orbRange
           END
        ENDCASE

        IF KEYWORD_SET(is_orbArr) THEN BEGIN
           tmp_i             = CGSETINTERSECTION(dbStruct.orbit, $
                                                 MIMC__orbRange, $
                                                 POSITIONS=orb_i)
        ENDIF ELSE BEGIN
           orb_i             = GET_ORBRANGE_INDS( $
                               dbStruct, $
                               MIMC__orbRange[0], $
                               MIMC__orbRange[1], $
                               LUN=lun)
        ENDELSE

        IF orb_i[0] NE -1 THEN BEGIN
           region_i          = CGSETINTERSECTION(region_i,orb_i)
        ENDIF ELSE BEGIN
           PRINTF,lun,'No orbs matching provided range!'
           STOP
        ENDELSE
     ENDIF     

     ;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Limits on season?
     test = ''
     STR_ELEMENT,alfDB_plot_struct,'restrict_by_season',test
     IF STRLEN(test) NE 0 THEN $
        IF (WHERE(STRMATCH(['spring','summer','fall','winter'], $
                           test, $
                           /FOLD_CASE) EQ 1))[0] NE -1 $
        THEN BEGIN

        season_i = GET_SEASON_INDS( $
                   dbStruct.x, $
                   SPRING=STRMATCH('spring',test,/FOLD_CASE), $
                   SUMMER=STRMATCH('summer',test,/FOLD_CASE), $
                   FALL=STRMATCH('fall',test,/FOLD_CASE), $
                   WINTER=STRMATCH('winter',test,/FOLD_CASE), $
                   ;; ALL_SEASONS=all_seasons, $
                   HEMI=MIMC__hemi, $
                   USE_JULDAY=use_julDay, $
                   QUIET=quiet)

        IF season_i[0] NE -1 THEN BEGIN
           nRegion           = N_ELEMENTS(region_i)
           region_i          = CGSETINTERSECTION(region_i, $
                                                 TEMPORARY(season_i), $
                                                COUNT=nKept)
           PRINT,"Lost " + STRCOMPRESS(nRegion - nKept,/REMOVE_ALL) + $
                 ' inds to ' + test + 'time ...'
        ENDIF ELSE BEGIN
           PRINTF,lun,'No orbs matching provided season (' + $
                  test + ')!'
           STOP
        ENDELSE

     ENDIF ELSE BEGIN
        PRINT,"Bogus: " + STRCOMPRESS(test)
        STOP
     ENDELSE

     ;;limits on altitudes to use?
     test = !NULL
     STR_ELEMENT,alfDB_plot_struct,'altitudeRange',test
     IF SIZE(test,/TYPE) NE 0 THEN BEGIN
        MIMC__altitudeRange  = alfDB_plot_struct.altitudeRange
        IF N_ELEMENTS(alfDB_plot_struct.altitudeRange) EQ 2 THEN BEGIN
           alt_i             = GET_ALTITUDE_INDS( $
                               dbStruct, $
                               MIMC__altitudeRange[0], $
                               MIMC__altitudeRange[1],LUN=lun)
           region_i          = CGSETINTERSECTION(region_i,alt_i)
        ENDIF ELSE BEGIN
           PRINTF,lun,"Incorrect input for keyword 'altitudeRange'!!"
           PRINTF,lun,"Please use altitudeRange = [minAlt maxAlt]"
           RETURN, -1
        ENDELSE
     ENDIF

 ;;      ;;limits on altitudes to use?
     ;; IF KEYWORD_SET(altitudeRange) THEN BEGIN
     ;;    MIMC__altitudeRange  = alfDB_plot_struct.altitudeRange
     ;;    IF N_ELEMENTS(altitudeRange) EQ 2 THEN BEGIN
     ;;       alt_i             = GET_ALTITUDE_INDS(dbStruct,MIMC__altitudeRange[0],MIMC__altitudeRange[1],LUN=lun)
     ;;       region_i          = CGSETINTERSECTION(region_i,alt_i)
     ;;    ENDIF ELSE BEGIN
     ;;       PRINTF,lun,"Incorrect input for keyword 'altitudeRange'!!"
     ;;       PRINTF,lun,"Please use altitudeRange=[minAlt maxAlt]"
     ;;       RETURN, -1
     ;;    ENDELSE
     ;; ENDIF
     
     ;; was using this to compare our Poynting flux estimates against Keiling et al. 2003 Fig. 3
     ;;limits on characteristic electron energies to use?
     test = !NULL
     STR_ELEMENT,alfDB_plot_struct,'charERange',test
     IF (SIZE(test,/TYPE) NE 0) THEN BEGIN
        IF N_ELEMENTS(alfDB_plot_struct.charERange) EQ 2 THEN BEGIN
           MIMC__charERange  = alfDB_plot_struct.charERange
           
           chare_i           = GET_CHARE_INDS( $
                               dbStruct, $
                               alfDB_plot_struct.charERange[0], $
                               alfDB_plot_struct.charERange[1], $
                               NEWELL_THE_CUSP=alfDB_plot_struct.fluxPlots__Newell_the_cusp, $
                               FOR_ESPEC_DB=~is_ion, $
                               FOR_ION_DB=is_ion, $
                               LUN=lun)

           region_i          = CGSETINTERSECTION(region_i,chare_i)
        ENDIF ELSE BEGIN
           PRINTF,lun,"Incorrect input for keyword 'charERange'!!"
           PRINTF,lun,"Please use charERange = [minCharE maxCharE]"
           RETURN, -1
        ENDELSE
     ENDIF

     IF KEYWORD_SET (charIERange) AND is_ion THEN BEGIN
        IF N_ELEMENTS(charIERange) EQ 2 THEN BEGIN
           CASE 1 OF
              KEYWORD_SET(for_alfven_db): BEGIN
                 NWLL_ALF_I__charIERange  = charIERange
              END
              ELSE: BEGIN
                 NEWELL_I__charIERange    = charIERange
              END
           ENDCASE
           
           charie_i                       = GET_CHARE_INDS(dbStruct, $
                                                           charIERange[0], $
                                                           charIERange[1], $
                                                           /FOR_ION_DB, $
                                                           LUN=lun)

           ;; charie                         = ABS(dbStruct.jei/dbStruct.ji)*6.242*1.0e11
           ;; charie_i                       = WHERE(charie GE charIERange[0] AND $
           ;;                                             charie LE charIERange[1])

           region_i                       = CGSETINTERSECTION(region_i,charie_i)
        ENDIF ELSE BEGIN
           PRINTF,lun,"Incorrect input for keyword 'charIERange'!!"
           PRINTF,lun,"Please use charIERange=[minCharIE maxCharIE]"
           RETURN, -1
        ENDELSE
     ENDIF

        good_i          = TEMPORARY(region_i)

     ;;Now, clear out all the garbage (NaNs & Co.)
     IF is_ion THEN BEGIN
        CASE 1 OF
           KEYWORD_SET(for_alfven_db): BEGIN
              nClean_i  = N_ELEMENTS(NWLL_ALF_I__cleaned_i)
           END
           ELSE: BEGIN
              nClean_i  = N_ELEMENTS(NEWELL_I__cleaned_i)
           END
        ENDCASE

        IF nClean_i EQ 0 THEN BEGIN
           
           ;; IF  THEN BEGIN
           tempDir                      = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
        CASE 1 OF
           KEYWORD_SET(for_alfven_db): BEGIN
              defNewellDBCleanInds      = 'alf_iSpec_20160607_db--PARSED--Orbs_500-16361--indices_w_no_NaNs_INFs.sav'
           END
           ELSE: BEGIN
              defNewellDBCleanInds      = 'iSpec_20160607_db--PARSED--Orbs_500-16361--indices_w_no_NaNs_INFs.sav'
           END
        ENDCASE
           
              IF FILE_TEST(tempDir+defNewellDBCleanInds) THEN BEGIN
                 RESTORE,tempDir+defNewellDBCleanInds
              ENDIF ELSE BEGIN        
                 PRINT,'Making NaN- and INF-less ion DB inds in ' + defNewellDBCleanInds + '...'
                 cleaned_i              = BASIC_ESPEC_ION_DB_CLEANER(dbStruct,/CLEAN_NANS_AND_INFINITIES)
                 PRINT,'Saving NaN- and INF-less ion DB inds to ' + defNewellDBCleanInds + '...'
                 SAVE,cleaned_i,FILENAME=tempDir+defNewellDBCleanInds
              ENDELSE

           CASE 1 OF
              KEYWORD_SET(for_alfven_db): BEGIN
                 NWLL_ALF_I__cleaned_i  = cleaned_i
                 IF NWLL_ALF_I__cleaned_i EQ !NULL THEN BEGIN
                    PRINTF,lun,"Couldn't clean Alfvén DB! Sup with that?"
                    STOP
                 ENDIF ELSE BEGIN
                 ENDELSE
              END
              ELSE: BEGIN
                 NEWELL_I__cleaned_i    = cleaned_i
                 IF NEWELL_I__cleaned_i EQ !NULL THEN BEGIN
                    PRINTF,lun,"Couldn't clean Alfvén DB! Sup with that?"
                    STOP
                 ENDIF ELSE BEGIN
                 ENDELSE
              END
           ENDCASE
        ENDIF
        good_i  = CGSETINTERSECTION(good_i, $
                                    KEYWORD_SET(for_alfven_db) ? $
                                    NWLL_ALF_I__cleaned_i : $
                                    NEWELL_I__cleaned_i) 

        ESPEC__SLAP_OFF_THOSE_HUGE_ONES,DBStruct, $
                                        ;; FOR_ESPEC_DBS=~is_ion, $
                                        /FOR_ION_DBS, $
                                        GOOD_I=good_i

     ENDIF ELSE BEGIN

        ESPEC__SLAP_OFF_THOSE_HUGE_ONES,DBStruct, $
                                        /FOR_ESPEC_DBS, $
                                        ;; FOR_ION_DBS=for_ion_DBs, $
                                        GOOD_I=good_i


        ;; IF KEYWORD_SET(alfDB_plot_struct.eSpec__remove_outliers) AND ~is_ion THEN BEGIN

        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;    ;;NEW WAY
        ;;    GET_ESPEC_INDS_BY_TYPE,NEWELL__eSpec, $ ;m_i,b_i,d_i, $
        ;;                           /LISTPLEASE, $
        ;;                           ;; N_ARR=NArr, $
        ;;                           OUT_I_LIST=i_list, $
        ;;                           ;; ONLY_STRICT=only_strict, $
        ;;                           ;; ONLY_NONSTRICT=only_nonStrict, $
        ;;                           USER_INDS=good_i, $
        ;;                           NO_INDS_AVAILABLE=skip_me, $
        ;;                           QUIET=quiet

        ;;    ;; final_i_list = LIST()
        ;;    ;; FOR k=0,N_ELEMENTS(i_list)-1 DO BEGIN

        ;;    ;;    IF skip_me[k] THEN CONTINUE

        ;;    ;;    tmp_i = i_list[k]

        ;;    ;;    tmpInlier_i = GET_FASTDB_OUTLIER_INDICES(dbStruct, $
        ;;    ;;                                          /FOR_ESPEC, $
        ;;    ;;                                          /REMOVE_OUTLIERS, $
        ;;    ;;                                          USER_INDS=tmp_i, $
        ;;    ;;                                          ;; ONLY_UPPER=only_upper, $
        ;;    ;;                                          /ONLY_UPPER, $
        ;;    ;;                                          ONLY_LOWER=only_lower, $
        ;;    ;;                                          ;; LOG_OUTLIERS=log_outliers, $
        ;;    ;;                                          /LOG_OUTLIERS, $
        ;;    ;;                                          LOG__ABS=log__abs, $
        ;;    ;;                                          LOG__NEG=log__neg, $
        ;;    ;;                                          /ADD_SUSPECTED)

        ;;    ;;    IF (tmpInlier_i[0] NE -1) THEN BEGIN
        ;;    ;;       final_i_list.Add,tmpInlier_i
        ;;    ;;    ENDIF ELSE BEGIN
        ;;    ;;       STOP
        ;;    ;;    ENDELSE

        ;;    ;;    ;; ADD_SUSPECTED=add_suspected)
        ;;    ;; ENDFOR

        ;;    ;; final_i = LIST_TO_1DARRAY(TEMPORARY(final_i_list))

        ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;    ;;OLD WAY
        ;;    ;; inlier_i = GET_FASTDB_OUTLIER_INDICES(dbStruct, $
        ;;    ;;                                       /FOR_ESPEC, $
        ;;    ;;                                       /REMOVE_OUTLIERS, $
        ;;    ;;                                       USER_INDS=region_i, $
        ;;    ;;                                       ;; ONLY_UPPER=only_upper, $
        ;;    ;;                                       /ONLY_UPPER, $
        ;;    ;;                                       ONLY_LOWER=only_lower, $
        ;;    ;;                                       ;; LOG_OUTLIERS=log_outliers, $
        ;;    ;;                                       /LOG_OUTLIERS, $
        ;;    ;;                                       LOG__ABS=log__abs, $
        ;;    ;;                                       LOG__NEG=log__neg, $
        ;;    ;;                                       /ADD_SUSPECTED)

        ;;    good_i = CGSETINTERSECTION(good_i,TEMPORARY(final_i))
        ;; ENDIF



        ;;Now screen based on spectral type
        ;; eSpec__screen_based_on_spectral_type = 1
        ;; IF KEYWORD_SET(eSpec__screen_based_on_spectral_type) THEN BEGIN
        ;;    jee_d_lims = [1e-10,10^(0.5)]

        ;; ENDIF

        ;; junk_blackballed_orbits = 1
        ;; IF KEYWORD_SET(junk_blackballed_orbits) THEN BEGIN
        ;;    good_i = TRASH_BAD_FAST_ORBITS(dbStruct,good_i)
        ;; ENDIF

        ;;Now get the file for which I feel better about the removal of the first 10 points
        safed_eSpecIndsFile = GET_NEWELL_ESPEC_SAFED_INDS_FILE(dbStruct, $
                                                          NEWELLDBDIR=dbDir, $
                                                          /STOP_IF_NOEXIST)

        IF STRUPCASE(safed_eSpecIndsFile) NE STRUPCASE('DontNeedItBro') THEN BEGIN
           RESTORE,safed_eSpecIndsFile

           IF N_ELEMENTS(dbStruct.orbit) NE eSpec_clean_info.totChecked THEN BEGIN
              PRINT,"Is there a database mix-up here? The answer is apparently yes."
              STOP
           ENDIF
           nGood       = N_ELEMENTS(good_i)
           good_i      = CGSETINTERSECTION(good_i,TEMPORARY(cleaned_eSpec_i),COUNT=nKept)
           PRINT,"Lost " + STRCOMPRESS(nGood - nKept,/REMOVE_ALL) + ' inds to the safety ind thing ...'

        ENDIF

        ;;Now kill dat
        ;; killGap_file = GET_NEWELL_ESPEC_KILLGAP_FILE(dbStruct, $
        ;;                                              NEWELLDBDIR=dbDir, $
        ;;                                              /STOP_IF_NOEXIST)

        ;; RESTORE,killGap_file

        ;; nGood       = N_ELEMENTS(good_i)
        ;; good_i      = CGSETINTERSECTION(good_i,TEMPORARY(keep_i),COUNT=nKept)
        ;; PRINT,"Lost " + STRCOMPRESS(nGood - nKept,/REMOVE_ALL) + ' inds to the killedGap ind thing ...'

        ;; IF N_ELEMENTS(NWLL_ALF__cleaned_i) EQ 0 THEN BEGIN
           ;; NWLL_ALF__cleaned_i                     = fastloc_cleaner(dbStruct,LUN=lun)
        ;;    NWLL_ALF__cleaned_i                     = fastloc_cleaner(dbStruct,LUN=lun)
        ;;    IF NWLL_ALF__cleaned_i EQ !NULL THEN BEGIN
        ;;       PRINTF,lun,"Couldn't clean fastloc DB! Sup with that?"
        ;;       STOP
        ;;    ENDIF ELSE BEGIN
        ;;    ENDELSE
        ;; ENDIF
        ;; good_i                                    = CGSETINTERSECTION(good_i,NWLL_ALF__cleaned_i) 
     ENDELSE

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Now some other user-specified exclusions set by keyword

     IF KEYWORD_SET(print_param_summary) THEN BEGIN
        PRINT_ALFVENDB_PLOTSUMMARY,dbStruct,good_i, $
                                   IMF_STRUCT=IMF_struct, $
                                   MIMC_STRUCT=MIMC_struct, $
                                   ALFDB_PLOT_STRUCT=alfDB_plot_struct, $
                                   LUN=lun
     ENDIF
     
     PRINTF,lun,"There are " + STRTRIM(N_ELEMENTS(good_i),2) + " total indices making the cut." 
     PRINTF,lun,''
     PRINTF,lun,"****END GET_ESPEC_ION_DB_IND****"
     PRINTF,lun,""

     IF is_ion THEN BEGIN
        NEWELL_I__good_i       = good_i
        NEWELL_I__HAVE_GOOD_I  = 1
     ENDIF ELSE BEGIN
        NWLL_ALF__good_i       = good_i
        NWLL_ALF__HAVE_GOOD_I  = 1
     ENDELSE

  ENDIF ELSE BEGIN
     IF is_ion THEN BEGIN
        good_i                 = NEWELL_I__good_i 
        NEWELL_I__HAVE_GOOD_I  = 1
     ENDIF ELSE BEGIN
        good_i                 = NWLL_ALF__good_i
        NWLL_ALF__HAVE_GOOD_I  = 1
     ENDELSE
  ENDELSE

  ;; good_i_file = '~/Desktop/eSpec_good_i__BOTH-' + $
  ;;      GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '.sav'
  ;; PRINT,"Saving good_i to " + good_i_file
  ;; SAVE,good_i,FILENAME=good_i_file
  ;; STOP

  RETURN,good_i

END
