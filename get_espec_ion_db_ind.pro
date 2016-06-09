;***********************************************
;2016/06/07
;This is like GET_CHASTON_IND for the electron and ion DBs
FUNCTION GET_ESPEC_ION_DB_IND,dbStruct,lun, $
                              DBFILE=dbfile, $
                              DBDIR=dbDir, $
                              ORBRANGE=orbRange, $
                              ALTITUDERANGE=altitudeRange, $
                              ;; CHARERANGE=charERange, $
                              BOTH_HEMIS=both_hemis, $
                              NORTH=north, $
                              SOUTH=south, $
                              HEMI=hemi, $
                              HWMAUROVAL=HwMAurOval, $
                              HWMKPIND=HwMKpInd, $
                              MINMLT=minM, $
                              MAXMLT=maxM, $
                              BINM=binM, $
                              MINILAT=minI, $
                              MAXILAT=maxI, $
                              BINILAT=binI, $
                              ;; DO_LSHELL=do_lshell, $
                              ;; MINLSHELL=minL, $
                              ;; MAXLSHELL=maxL, $
                              ;; BINLSHELL=binL, $
                              DAYSIDE=dayside, $
                              NIGHTSIDE=nightside, $
                              GET_ESPEC_I_NOT_ION_I=get_eSpec_i, $
                              GET_ION_I=get_ion_i, $
                              RESET_GOOD_INDS=reset_good_inds, $
                              DO_NOT_SET_DEFAULTS=do_not_set_defaults, $
                              DONT_LOAD_IN_MEMORY=nonMem, $
                              PRINT_PARAM_SUMMARY=print_param_summary
  
  COMPILE_OPT idl2
  
  COMMON MLT_ILAT_MAGC_ETC,MIMC__RECALCULATE, $
     MIMC__minMLT,MIMC__maxMLT, $
     MIMC__minILAT,MIMC__maxILAT,MIMC__binILAT, $
     MIMC__DO_lShell,MIMC__minLSHELL,MIMC__maxLSHELL,MIMC__binLSHELL, $
     MIMC__minMC,MIMC__maxNegMC, $
     MIMC__sample_t_restriction, $
     MIMC__hemi, $
     MIMC__orbRange, $
     MIMC__altitudeRange, $
     MIMC__charERange, $
     MIMC__poyntRange, $
     MIMC__despunDB,MIMC__chastDB, $
     MIMC__north,MIMC__south,MIMC__both_hemis, $
     MIMC__HwMAurOval, $
     MIMC__HwMKpInd

  IF ~KEYWORD_SET(nonMem) THEN BEGIN
     COMMON NEWELL_I,NEWELL_I__ion,NEWELL_I__HAVE_GOOD_I, $
        NEWELL_I__good_i,NEWELL_I__cleaned_i, $
        NEWELL_I__dbFile,NEWELL_I__dbDir, $
        NEWELL_I__RECALCULATE
     
     ;;This common block is defined ONLY here, in GET_H2D_NEWELLS__EACH_TYPE, and in LOAD_ALF_NEWELL_ESPEC_DB
     COMMON NWLL_ALF,NWLL_ALF__eSpec,NWLL_ALF__HAVE_GOOD_I, $
        NWLL_ALF__good_eSpec_i, $
        NWLL_ALF__good_alf_i, $
        NWLL_ALF__failCodes, $
        NWLL_ALF__despun, $
        NWLL_ALF__dbFile,NWLL_ALF__dbDir, $
        NWLL_ALF__RECALCULATE

  ENDIF
                                ;For statistical auroral oval
  defHwMAurOval=0
  defHwMKpInd=7

  defLun                                          = -1

  ;; defPrintSummary                              = 0

  IF ~KEYWORD_SET(lun) THEN lun                   = defLun ;stdout

  IF ~KEYWORD_SET(do_not_set_defaults) THEN BEGIN
     SET_DEFAULT_MLT_ILAT_AND_MAGC,MINMLT=minM,MAXMLT=maxM,BINM=binM, $
                                   MINILAT=minI,MAXILAT=maxI,BINI=binI, $
                                   MINLSHELL=minL,MAXLSHELL=maxL,BINL=binL, $
                                   MIN_MAGCURRENT=minMC,MAX_NEGMAGCURRENT=maxNegMC, $
                                   HEMI=hemi, $
                                   BOTH_HEMIS=both_hemis, $
                                   NORTH=north, $
                                   SOUTH=south, $
                                   LUN=lun
  ENDIF

  ;;;;;;;;;;;;;;;
  ;;Check whether this is a maximus or fastloc struct
  IF KEYWORD_SET(dbStruct) THEN BEGIN
     IF KEYWORD_SET(get_eSpec_i) THEN BEGIN
        is_ion                                = 0
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(get_ion_i) THEN BEGIN
           is_ion                             = 1
        ENDIF ELSE BEGIN
           IS_STRUCT_ION_OR_ESPEC,dbStruct,is_ion
        ENDELSE
     ENDELSE
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(get_eSpec_i) THEN BEGIN
        is_ion                                = 0
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(get_ion_i) THEN BEGIN
           is_ion                             = 1
        ENDIF
     ENDELSE
  ENDELSE

  IF ~KEYWORD_SET(get_ion_i) AND ~KEYWORD_SET(get_eSpec_i) AND ~KEYWORD_SET(dbStruct) THEN BEGIN
     PRINTF,lun,"Assuming this is an ion DB ..."
     is_ion                                   = 1 ;We assume this is maximus
  ENDIF

  ;;Get the databases if they're already in mem
  IF is_ion THEN BEGIN
     IF N_ELEMENTS(NEWELL_I__ion) NE 0 THEN BEGIN
        dbStruct                                  = NEWELL_I__ion
        dbFile                                    = NEWELL_I__dbFile
        dbDir                                     = NEWELL_I__dbDir
     ENDIF ELSE BEGIN
        LOAD_NEWELL_ION_DB,ion, $
                           ;; FAILCODES=failCodes, $
                           NEWELLDBDIR=dbDir, $
                           NEWELLDBFILE=dbFile, $
                           FORCE_LOAD_DB=force_load_db, $
                           DONT_LOAD_IN_MEMORY=nonMem, $
                           ;; OUT_CLEANED_I=cleaned_i, $
                           LUN=lun

        IF ~KEYWORD_SET(nonMem) THEN BEGIN
           NEWELL_I__ion          = ion
           ;; NEWELL_I__cleaned_i    = cleaned_i
        ENDIF
     ENDELSE
  ENDIF ELSE BEGIN
     IF N_ELEMENTS(NWLL_ALF__eSpec) NE 0 THEN BEGIN
        dbStruct                                  = NWLL_ALF__eSpec
        dbFile                                    = NWLL_ALF__dbFile
        dbDir                                     = NWLL_ALF__dbDir
     ENDIF ELSE BEGIN
        LOAD_NEWELL_ESPEC_DB,dbStruct, $
                             ;; FAILCODES=failCodes, $
                             NEWELLDBDIR=dbDir, $
                             NEWELLDBFILE=dbFile, $
                             FORCE_LOAD_DB=force_load_db, $
                             DONT_LOAD_IN_MEMORY=nonMem, $
                             ;; OUT_GOOD_I=good_i, $
                             LUN=lun
        NWLL_ALF__eSpec                            = dbStruct
        NWLL_ALF__dbFile                           = dbFile
        NWLL_ALF__dbDir                            = dbDir
     ENDELSE
  ENDELSE

  ;;Now check to see whether we have the appropriate vars for each guy
  IF ~is_ion THEN BEGIN
     IF ~KEYWORD_SET(NWLL_ALF__HAVE_GOOD_I) OR KEYWORD_SET(reset_good_inds) THEN BEGIN
        IF KEYWORD_SET(reset_good_inds) THEN BEGIN
           PRINT,'Resetting good eSpec inds...'
        ENDIF
        calculate                                 = 1
     ENDIF ELSE BEGIN
        IF N_ELEMENTS(NWLL_ALF__good_i) NE 0 THEN BEGIN
           CHECK_FOR_NEW_ESPEC_ION_IND_CONDS,is_ion, $
                                             ORBRANGE=orbRange, $
                                             ALTITUDERANGE=altitudeRange, $
                                             CHARERANGE=charERange, $
                                             BOTH_HEMIS=both_hemis, $
                                             NORTH=north, $
                                             SOUTH=south, $
                                             HEMI=hemi, $
                                             HWMAUROVAL=HwMAurOval, $
                                             HWMKPIND=HwMKpInd, $
                                             MINMLT=minMLT, $
                                             MAXMLT=maxMLT, $
                                             BINM=binMLT, $
                                             MINILAT=minILAT, $
                                             MAXILAT=maxILAT, $
                                             BINILAT=binILAT, $
                                             ;; DO_LSHELL=do_lshell, $
                                             ;; MINLSHELL=minLshell, $
                                             ;; MAXLSHELL=maxLshell, $
                                             ;; BINLSHELL=binLshell, $
                                             DAYSIDE=dayside, $
                                             NIGHTSIDE=nightside, $
                                             HAVE_GOOD_I=have_good_i, $
                                             LUN=lun
           calculate                             = MIMC__RECALCULATE
           ;; NEWELL_I__HAVE_GOOD_I                 = have_good_i
           NWLL_ALF__HAVE_GOOD_I                   = have_good_i
        ENDIF ELSE BEGIN
           PRINT,'But you should already have NWLL_ALF__good_i!!'
           STOP
        ENDELSE
     ENDELSE
  ENDIF ELSE BEGIN
     IF ~KEYWORD_SET(NEWELL_I__HAVE_GOOD_I) OR KEYWORD_SET(reset_good_inds) THEN BEGIN
        IF KEYWORD_SET(reset_good_inds) THEN BEGIN
           PRINT,'Resetting good ion inds...'
        ENDIF
        calculate                                 = 1
     ENDIF ELSE BEGIN
        IF N_ELEMENTS(NEWELL_I__good_i) NE 0 THEN BEGIN
           CHECK_FOR_NEW_ESPEC_ION_IND_CONDS,is_ion, $
                                             ORBRANGE=orbRange, $
                                             ALTITUDERANGE=altitudeRange, $
                                             CHARERANGE=charERange, $
                                             BOTH_HEMIS=both_hemis, $
                                             NORTH=north, $
                                             SOUTH=south, $
                                             HEMI=hemi, $
                                             HWMAUROVAL=HwMAurOval, $
                                             HWMKPIND=HwMKpInd, $
                                             MINMLT=minMLT, $
                                             MAXMLT=maxMLT, $
                                             BINM=binMLT, $
                                             MINILAT=minILAT, $
                                             MAXILAT=maxILAT, $
                                             BINILAT=binILAT, $
                                             ;; DO_LSHELL=do_lshell, $
                                             ;; MINLSHELL=minLshell, $
                                             ;; MAXLSHELL=maxLshell, $
                                             ;; BINLSHELL=binLshell, $
                                             DAYSIDE=dayside, $
                                             NIGHTSIDE=nightside, $
                                             HAVE_GOOD_I=have_good_i, $
                                             LUN=lun
           calculate                              = MIMC__RECALCULATE
           NEWELL_I__HAVE_GOOD_I                  = have_good_i
           ;; NWLL_ALF__HAVE_GOOD_I                    = have_good_i
        ENDIF ELSE BEGIN
           PRINT,'But you should already have NEWELL_I__good_i!!'
           STOP
        ENDELSE
     ENDELSE
  ENDELSE

  IF KEYWORD_SET(calculate) THEN BEGIN

     IF ~KEYWORD_SET(HwMAurOval) THEN HwMAurOval  = defHwMAurOval
     IF ~KEYWORD_SET(HwMKpInd) THEN HwMKpInd      = defHwMKpInd


     ;;Welcome message
     printf,lun,""
     printf,lun,"****From GET_ESPEC_ION_IND****"
     printf,lun,FORMAT='("DBFile                        :",T35,A0)',dbFile
     printf,lun,""

     ;;;;;;;;;;;;
     ;;Handle longitudes
     MIMC__minMLT                                 = minM
     MIMC__maxMLT                                 = maxM
     MIMC__binMLT                                 = binM
     MIMC__dayside                                = KEYWORD_SET(dayside)
     MIMC__nightside                              = KEYWORD_SET(nightside)
     mlt_i                                        = GET_MLT_INDS(dbStruct,MIMC__minMLT,MIMC__maxMLT, $
                                                                 DAYSIDE=MIMC__dayside,NIGHTSIDE=MIMC__nightside, $
                                                                 N_MLT=n_mlt,N_OUTSIDE_MLT=n_outside_MLT,LUN=lun)
     
     ;;;;;;;;;;;;
     ;;Handle latitudes, combine with mlt
     MIMC__hemi                                   = hemi
     MIMC__north                                  = KEYWORD_SET(north)
     MIMC__south                                  = KEYWORD_SET(south)
     MIMC__both_hemis                             = KEYWORD_SET(both_hemis)
     IF KEYWORD_SET(do_lShell) THEN BEGIN
        MIMC__minLshell                           = minL
        MIMC__maxLshell                           = maxL
        MIMC__binLshell                           = binL
        lshell_i                                  = GET_LSHELL_INDS(dbStruct,MIMC__minLshell,MIMC__maxLshell,MIMC__hemi, $
                                                                    N_LSHELL=n_lshell,N_NOT_LSHELL=n_not_lshell,LUN=lun)
        region_i                                  = CGSETINTERSECTION(lshell_i,mlt_i)
     ENDIF ELSE BEGIN
        MIMC__minILAT                             = minI
        MIMC__maxILAT                             = maxI
        MIMC__binILAT                             = binI
        ilat_i                                    = GET_ILAT_INDS(dbStruct,MIMC__minILAT,MIMC__maxILAT,MIMC__hemi, $
                                                                  N_ILAT=n_ilat,N_NOT_ILAT=n_not_ilat,LUN=lun)
        region_i                                  = CGSETINTERSECTION(ilat_i,mlt_i)
     ENDELSE

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Want just Holzworth/Meng statistical auroral oval?
     IF HwMAurOval THEN region_i = CGSETINTERSECTION(region_i, $
                                                     WHERE(ABS(dbStruct.ilat) GT auroral_zone(dbStruct.mlt,HwMKpInd,/lat)/(!DPI)*180.))

  ;;;;;;;;;;;;;;;;;;;;;;
     ;;Now combine them all
     IF KEYWORD_SET(do_lShell) THEN BEGIN
     ENDIF ELSE BEGIN
     ENDELSE

  ;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Limits on orbits to use?
     IF KEYWORD_SET(orbRange) THEN BEGIN
        MIMC__orbRange                            = orbRange
        CASE N_ELEMENTS(orbRange) OF
           1: BEGIN
              MIMC__orbRange                      = [orbRange,orbRange]
           END
           2: BEGIN
              MIMC__orbRange                      = orbRange
           END
           ELSE: BEGIN
              printf,lun,"Incorrect input for keyword 'orbRange'!!"
              printf,lun,"Please use orbRange=[minOrb maxOrb] or a single element"
              RETURN, -1
           END
        ENDCASE
        ;; IF N_ELEMENTS(orbRange) EQ 2 THEN BEGIN
        orb_i                                     = GET_ORBRANGE_INDS(dbStruct,MIMC__orbRange[0],MIMC__orbRange[1],LUN=lun)
        region_i                                  = CGSETINTERSECTION(region_i,orb_i)
        ;; ENDIF ELSE BEGIN
        ;; ENDELSE
     ENDIF
     

     ;;limits on altitudes to use?
     IF KEYWORD_SET(altitudeRange) THEN BEGIN
        MIMC__altitudeRange                       = altitudeRange
        IF N_ELEMENTS(altitudeRange) EQ 2 THEN BEGIN
           alt_i                                  = GET_ALTITUDE_INDS(dbStruct,MIMC__altitudeRange[0],MIMC__altitudeRange[1],LUN=lun)
           region_i                               = CGSETINTERSECTION(region_i,alt_i)
        ENDIF ELSE BEGIN
           printf,lun,"Incorrect input for keyword 'altitudeRange'!!"
           printf,lun,"Please use altitudeRange=[minAlt maxAlt]"
           RETURN, -1
        ENDELSE
     ENDIF
     
     ;; was using this to compare our Poynting flux estimates against Keiling et al. 2003 Fig. 3
     ;;limits on characteristic electron energies to use?
     ;; IF KEYWORD_SET (charERange) AND is_ion THEN BEGIN
     ;;    IF N_ELEMENTS(charERange) EQ 2 THEN BEGIN
     ;;       MIMC__charERange                       = charERange
           
     ;;       IF KEYWORD_SET(chastDB) THEN BEGIN
     ;;          chare_i                             = WHERE(dbStruct.char_elec_energy GE MIMC__charERange[0] AND $
     ;;                                                      dbStruct.char_elec_energy LE MIMC__charERange[1])
     ;;       ENDIF ELSE BEGIN
     ;;          chare_i                             = WHERE(dbStruct.max_chare_losscone GE MIMC__charERange[0] AND $
     ;;                                                      dbStruct.max_chare_losscone LE MIMC__charERange[1])
     ;;       ENDELSE
     ;;       region_i                               = CGSETINTERSECTION(region_i,chare_i)
     ;;    ENDIF ELSE BEGIN
     ;;       printf,lun,"Incorrect input for keyword 'charERange'!!"
     ;;       printf,lun,"Please use charERange=[minCharE maxCharE]"
     ;;       RETURN, -1
     ;;    ENDELSE
     ;; ENDIF

     ;; IF KEYWORD_SET(poyntRange) AND is_ion THEN BEGIN
     ;;    MIMC__poyntRange                       = poyntRange
     ;;    IF N_ELEMENTS(poyntRange) NE 2 OR (MIMC__poyntRange[1] LE MIMC__poyntRange[0]) THEN BEGIN
     ;;       PRINT,"Invalid Poynting range specified! poyntRange should be a two-element vector, [minPoynt maxPoynt]"
     ;;       PRINT,"No Poynting range set..."
     ;;       RETURN, -1
     ;;    ENDIF ELSE BEGIN
     ;;       region_i=CGSETINTERSECTION(region_i,where(dbStruct.pFluxEst GE MIMC__poyntRange[0] AND $
     ;;                                             dbStruct.pFluxEst LE MIMC__poyntRange[1]))
     ;;       printf,lun,FORMAT='("Poynting flux limits (eV)     :",T35,G8.2,T45,G8.2)',MIMC__poyntRange[0],MIMC__poyntRange[1]
     ;;    ENDELSE
     ;; ENDIF


     ;;gotta screen to make sure it's in ACE db too:
     ;;Only so many are useable, since ACE data start in 1998
     
     IF KEYWORD_SET(satellite) THEN BEGIN
        sat_i                                     = GET_SATELLITE_INDS(dbStruct,satellite,LUN=lun)
        good_i                                    = region_i[where(region_i GE sat_i,nGood,complement=lost,ncomplement=nlost)]
        lost                                      = region_i[lost]
     ENDIF ELSE BEGIN
        good_i                                    = region_i
     ENDELSE

     ;;Now, clear out all the garbage (NaNs & Co.)
     IF is_ion THEN BEGIN
        IF N_ELEMENTS(NEWELL_I__cleaned_i) EQ 0 THEN BEGIN
           
           tempDir                = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
           defNewellDBCleanInds   = 'iSpec_20160607_db--PARSED--Orbs_500-16361--indices_w_no_NaNs_INFs.sav'
  
           IF FILE_TEST(tempDir+defNewellDBCleanInds) THEN BEGIN
              RESTORE,tempDir+defNewellDBCleanInds
           ENDIF ELSE BEGIN        
              PRINT,'Making NaN- and INF-less ion DB inds in ' + defNewellDBCleanInds + '...'
              cleaned_i = BASIC_ESPEC_ION_DB_CLEANER(dbStruct,/CLEAN_NANS_AND_INFINITIES)
              PRINT,'Saving NaN- and INF-less ion DB inds to ' + defNewellDBCleanInds + '...'
              SAVE,cleaned_i,FILENAME=tempDir+defNewellDBCleanInds
           ENDELSE

           NEWELL_I__cleaned_i                     = cleaned_i
           IF NEWELL_I__cleaned_i EQ !NULL THEN BEGIN
              PRINTF,lun,"Couldn't clean Alfvén DB! Sup with that?"
              STOP
           ENDIF ELSE BEGIN
           ENDELSE
        ENDIF
        good_i                                    = CGSETINTERSECTION(good_i,NEWELL_I__cleaned_i) 
     ENDIF ELSE BEGIN
        PRINT,"eSpec DB needs no cleaning. She's clean as a whistle, you know."
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
                                   ;; CLOCKSTR=clockStr, $
                                   ;; ANGLELIM1=angleLim1, $
                                   ;; ANGLELIM2=angleLim2, $
                                   ORBRANGE=orbRange, $
                                   ALTITUDERANGE=altitudeRange, $
                                   CHARERANGE=charERange, $
                                   minMLT=minM, $
                                   maxMLT=maxM, $
                                   BINMLT=binM, $
                                   SHIFTMLT=shiftM, $
                                   MINILAT=minI, $
                                   MAXILAT=maxI, $
                                   BINILAT=binI, $
                                   ;; DO_LSHELL=do_lShell,MINLSHELL=minL,MAXLSHELL=maxL,BINLSHELL=binL, $
                                   ;; MIN_MAGCURRENT=minMC,MAX_NEGMAGCURRENT=maxNegMC, $
                                   HWMAUROVAL=HwMAurOval,HWMKPIND=HwMKpInd, $
                                   ;; BYMIN=byMin, BZMIN=bzMin, BYMAX=byMax, BZMAX=bzMax, BX_OVER_BYBZ_LIM=Bx_over_ByBz_Lim, $
                                   PARAMSTRING=paramString, PARAMSTRPREFIX=plotPrefix,PARAMSTRSUFFIX=plotSuffix,$
                                   SATELLITE=satellite, $
                                   ;; OMNI_COORDS=omni_Coords, $
                                   HEMI=hemi, $
                                   ;; DELAY=delay, $
                                   ;; STABLEIMF=stableIMF, $
                                   ;; SMOOTHWINDOW=smoothWindow, $
                                   ;; INCLUDENOCONSECDATA=includeNoConsecData, $
                                   HOYDIA=hoyDia, $
                                   MASKMIN=maskMin, $
                                   LUN=lun
     ENDIF
     
     printf,lun,"There are " + strtrim(n_elements(good_i),2) + " total indices making the cut." 
     PRINTF,lun,''
     printf,lun,"****END GET_ESPEC_ION_DB_IND****"
     printf,lun,""

     IF is_ion THEN BEGIN
        NEWELL_I__good_i                          = good_i
        NEWELL_I__HAVE_GOOD_I                     = 1
     ENDIF ELSE BEGIN
        NWLL_ALF__good_i                           = good_i
        NWLL_ALF__HAVE_GOOD_I                      = 1
     ENDELSE

  ENDIF ELSE BEGIN
     IF is_ion THEN BEGIN
        good_i                                    = NEWELL_I__good_i 
        NEWELL_I__HAVE_GOOD_I                     = 1
        ;; IF ARG_PRESENT(out_maximus) THEN BEGIN
        ;;    PRINT,'Giving you maximus...'
        ;;    out_maximus = NEWELL_I__maximus
        ;; ENDIF
        ;; IF ARG_PRESENT(out_cdbTime) THEN BEGIN
        ;;    PRINT,'Giving you maximus...'
        ;;    out_cdbTime = NEWELL_I__times
        ;; ENDIF
     ENDIF ELSE BEGIN
        good_i                                    = NWLL_ALF__good_i
        NWLL_ALF__HAVE_GOOD_I                       = 1
        ;; IF ARG_PRESENT(out_fastLoc) AND N_ELEMENTS(out_fastLoc) EQ 0 THEN BEGIN
        ;;    PRINT,'Giving you fastLoc...'
        ;;    out_fastLoc = NWLL_ALF__eSpec
        ;; ENDIF
        ;; ENDIF
     ENDELSE
  ENDELSE

  RETURN, good_i

END