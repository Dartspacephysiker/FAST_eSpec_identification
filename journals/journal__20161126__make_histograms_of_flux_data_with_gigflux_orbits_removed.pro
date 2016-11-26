;;11/26/16
PRO JOURNAL__20161126__MAKE_HISTOGRAMS_OF_FLUX_DATA_WITH_GIGFLUX_ORBITS_REMOVED

  COMPILE_OPT IDL2

  @common__newell_espec.pro

  minI = 60
  maxI = 90
  minM = 0
  maxM = 24
  hemi = 'NORTH'
  orbRange = [1000,108000]
  altitudeRange = [300,4300]
  

  no_screen = 1

  ;;eSpec DB options
  use_2000km_file = 0
  force_load_DB   = 0

  ;;Plot options
  output_fa_crossing  = 0
  skip_histos         = 1

  ;;txt output
  ;; output_txtFile      = 1

  ;;Histo plot options (if ~skip_histos)x
  eFlux               = 1
  eNumFlux            = 0
  charE               = 0

  log_plots           = 1
  log_stats           = 1
  zoomed_histoXRange  = 1
  zoomed_histoYRange  = 1

  save_plots          = 1

  ;; suff__plotDir       = '/orbs_with_extremeness'

  show_plots          = 0 

  custom_title        = 'Cleaned dists, ' + GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  bonus_plotsuff      = 'cleaned_dists'

  ;;gjør det en del 
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 OR KEYWORD_SET(force_load_DB) THEN BEGIN
     LOAD_NEWELL_ESPEC_DB, $
        !NULL, $
        NEWELLDBDIR=NewellDBDir, $
        USE_2000KM_FILE=use_2000km_file, $
        FORCE_LOAD_DB=force_load_DB
  ENDIF

  ;;First, get all the honker indices
  ;; CASE 1 OF
  ;;    KEYWORD_SET(doJee): BEGIN
  ;;       ;; gigantic  = gigantic_Jee
  ;;       tipo      = 'Jee'
  ;;    END
  ;;    KEYWORD_SET(doJe): BEGIN
  ;;       ;; gigantic  = gigantic_Je
  ;;       tipo      = 'Je'
  ;;    END
  ;;    KEYWORD_SET(doCharE): BEGIN        
  ;;       ;; gigantic  = gigantic_charE
  ;;       tipo      = 'charE'        
  ;;    END
  ;; ENDCASE
  ;; STR_ELEMENT,NEWELL__eSpec,tipo,INDEX=dbInd
  ;; wild_inds       = WHERE(NEWELL__eSpec.(dbInd) GE gigantic,nWild)
  
  ;; IF nWild EQ 0 OR (dbInd LT 0) THEN BEGIN
  ;;    PRINT,"Sorry, couldn't find any extreme indices."
  ;;    RETURN
  ;; ENDIF

  plotSuff        = STRING(FORMAT='(A0,"--",A0)', $
                           GET_NEWELL_DB_STRING(NEWELL__eSpec))

  SET_PLOT_DIR,plotDir, $
               /FOR_ESPEC_DB, $
               /ADD_TODAY, $
               ADD_SUFF=suff__plotDir

  CASE 1 OF
     KEYWORD_SET(no_screen): BEGIN
        
        custom_title        = ''
        bonus_plotsuff      = '_dists'


        lun = -1
        ;;Handle longitudes
        MIMC__minMLT         = minM
        MIMC__maxMLT         = maxM
        ;; MIMC__binMLT         = binM
        MIMC__dayside        = KEYWORD_SET(dayside)
        MIMC__nightside      = KEYWORD_SET(nightside)
        mlt_i                = GET_MLT_INDS(NEWELL__eSpec,MIMC__minMLT,MIMC__maxMLT, $
                                            DAYSIDE=MIMC__dayside,NIGHTSIDE=MIMC__nightside, $
                                            N_MLT=n_mlt,N_OUTSIDE_MLT=n_outside_MLT,LUN=lun)
        
     ;;;;;;;;;;;;
        ;;Handle latitudes, combine with mlt
        MIMC__hemi           = hemi
        MIMC__north          = KEYWORD_SET(north)
        MIMC__south          = KEYWORD_SET(south)
        MIMC__both_hemis     = KEYWORD_SET(both_hemis)
        IF KEYWORD_SET(do_lShell) THEN BEGIN
           MIMC__minLshell   = minL
           MIMC__maxLshell   = maxL
           MIMC__binLshell   = binL
           lshell_i          = GET_LSHELL_INDS(NEWELL__eSpec,MIMC__minLshell,MIMC__maxLshell,MIMC__hemi, $
                                               N_LSHELL=n_lshell,N_NOT_LSHELL=n_not_lshell,LUN=lun)
           region_i          = CGSETINTERSECTION(lshell_i,mlt_i)
        ENDIF ELSE BEGIN
           MIMC__minILAT     = minI
           MIMC__maxILAT     = maxI
           ;; MIMC__binILAT     = binI
           MIMC__EA_binning  = KEYWORD_SET(EA_binning)
           ilat_i            = GET_ILAT_INDS(NEWELL__eSpec,MIMC__minILAT,MIMC__maxILAT,MIMC__hemi, $
                                             N_ILAT=n_ilat,N_NOT_ILAT=n_not_ilat,LUN=lun)
           region_i          = CGSETINTERSECTION(ilat_i,mlt_i)
        ENDELSE

        IF KEYWORD_SET(orbRange) THEN BEGIN
           MIMC__orbRange        = orbRange
           CASE N_ELEMENTS(orbRange) OF
              1: BEGIN
                 MIMC__orbRange  = [orbRange,orbRange]
              END
              2: BEGIN
                 MIMC__orbRange  = orbRange
              END
              ELSE: BEGIN
                 PRINTF,lun,"Incorrect input for keyword 'orbRange'!!"
                 PRINTF,lun,"Please use orbRange=[minOrb maxOrb] or a single element"
              END
           ENDCASE
           ;; IF N_ELEMENTS(orbRange) EQ 2 THEN BEGIN
           orb_i                 = GET_ORBRANGE_INDS(NEWELL__eSpec,MIMC__orbRange[0], $
                                                     MIMC__orbRange[1],LUN=lun, $
                                                    /DONT_TRASH_BAD_ORBITS)
           region_i              = CGSETINTERSECTION(region_i,orb_i)
           ;; ENDIF ELSE BEGIN
           ;; ENDELSE
        ENDIF

        IF KEYWORD_SET(altitudeRange) THEN BEGIN
           MIMC__altitudeRange  = altitudeRange
           IF N_ELEMENTS(altitudeRange) EQ 2 THEN BEGIN
              alt_i             = GET_ALTITUDE_INDS(NEWELL__eSpec,MIMC__altitudeRange[0],MIMC__altitudeRange[1],LUN=lun)
              region_i          = CGSETINTERSECTION(region_i,alt_i)
           ENDIF ELSE BEGIN
              PRINTF,lun,"Incorrect input for keyword 'altitudeRange'!!"
              PRINTF,lun,"Please use altitudeRange=[minAlt maxAlt]"
           ENDELSE
        ENDIF

        good_i          = TEMPORARY(region_i)

        je_lims  = [0,2.855e10] ;Drop 0.25% of the broadbands
        jee_lims = [0,1.043e2]  ;Drop 0.25% of the broadbands

        ;; percentToDrop = N_ELEMENTS(WHERE( ( (espec.broad EQ 1) OR (eSpec.broad EQ 2) ) AND $
        ;;                                   ( eSpec.je GT 4e10 ) ) ) / $
        ;;                 FLOAT(N_ELEMENTS(WHERE( ( (espec.broad EQ 1) OR $
        ;;                                           (eSpec.broad EQ 2) )  ) )) $
        ;;                 * 100.

        ;;Now Je
        good_je_i  = WHERE(NEWELL__eSpec.je GE je_lims[0] AND $
                           NEWELL__eSpec.je LE je_lims[1],nGoodJe, $
                           NCOMPLEMENT=nBadJe)

        nGood      = N_ELEMENTS(good_i)
        good_i     = CGSETINTERSECTION(good_i,TEMPORARY(good_je_i),COUNT=nKept)
        PRINT,"Lost " + STRCOMPRESS(nGood - nKept,/REMOVE_ALL) + ' inds to Je screening ...'

        ;;Now Jee
        good_jee_i = WHERE(NEWELL__eSpec.jee GE jee_lims[0] AND $
                           NEWELL__eSpec.jee LE jee_lims[1],nGoodJee, $
                           NCOMPLEMENT=nBadJee)

        nGood      = N_ELEMENTS(good_i)
        good_i     = CGSETINTERSECTION(good_i,TEMPORARY(good_jee_i),COUNT=nKept)
        PRINT,"Lost " + STRCOMPRESS(nGood - nKept,/REMOVE_ALL) + ' inds to Jee screening ...'


     END
     ELSE: BEGIN
        user_inds  = GET_ESPEC_ION_DB_IND(NEWELL__eSpec,lun, $
                                          FOR_ALFVEN_DB=for_alfven_db, $
                                          ;; COORDINATE_SYSTEM=coordinate_system, $
                                          ;; USE_AACGM_COORDS=use_aacgm, $
                                          ;; USE_MAG_COORDS=use_mag, $
                                          ;; DBFILE=dbfile, $
                                          ;; DBDIR=dbDir, $
                                          ORBRANGE=orbRange, $
                                          ALTITUDERANGE=altitudeRange, $
                                          CHARERANGE=charERange, $
                                          CHARIERANGE=charIERange, $
                                          BOTH_HEMIS=both_hemis, $
                                          HEMI=hemi, $
                                          HWMAUROVAL=HwMAurOval, $
                                          HWMKPIND=HwMKpInd, $
                                          MINMLT=minM, $
                                          MAXMLT=maxM, $
                                          BINM=binM, $
                                          MINILAT=minI, $
                                          MAXILAT=maxI, $
                                          BINILAT=binI, $
                                          EQUAL_AREA_BINNING=equal_area_binning, $
                                          ;; DO_LSHELL=do_lshell, $
                                          ;; MINLSHELL=minL, $
                                          ;; MAXLSHELL=maxL, $
                                          ;; BINLSHELL=binL, $
                                          ;; DAYSIDE=dayside, $
                                          ;; NIGHTSIDE=nightside, $
                                          /GET_ESPEC_I_NOT_ION_I, $
                                          ;; GET_ION_I=get_ion_i, $
                                          RESET_GOOD_INDS=reset_good_inds, $
                                          DO_NOT_SET_DEFAULTS=do_not_set_defaults, $
                                          ;; /DONT_LOAD_IN_MEMORY, $
                                          DONT_LOAD_IN_MEMORY=nonMem, $
                                          ESPEC__NEWELL_2009_INTERP=eSpec__Newell_2009_interp, $
                                          ESPEC__USE_2000KM_FILE=eSpec__use_2000km_file, $
                                          /PRINT_PARAM_SUMMARY)

     END
  ENDCASE

  ;; IF KEYWORD_SET(output_txtFile) THEN BEGIN
  ;;    PRINT,"Making txtFile: " + plotSuff + '--orb_list.txt'
  ;;    SET_TXTOUTPUT_DIR,txtOutputDir, $
  ;;                 /FOR_ESPEC_DB, $
  ;;                 /ADD_TODAY, $
  ;;                 ADD_SUFF=suff__plotDir
  
  ;; ENDIF

  ;;NOW PLOTSSSSSSS
  
  user_plotSuff = plotSuff + (KEYWORD_SET(bonus_plotSuff) ? '--' + bonus_plotSuff : '')

  FOR k=0,2 DO BEGIN

     enumflux = k EQ 0
     eflux    = k EQ 1
     chare    = k EQ 2

     JOURNAL__20161122__SO_HOW_DO_WE_CLEAN_YOU_ESPEC_DB, $
        ENUMFLUX=eNumFlux, $
        EFLUX=eFlux, $
        CHARE=charE, $
        POS_ONLY=pos_only, $
        NEG_ONLY=neg_only, $
        LOG_PLOTS=log_plots, $
        LOG_STATS=log_stats, $
        SHOW_PLOTS=show_plots, $
        WINDOW_INDEX=safeWinInd, $
        SAVE_PLOTS=save_plots, $
        ZOOMED_HISTOXRANGE=zoomed_histoXRange, $
        ZOOMED_HISTOYRANGE=zoomed_histoYRange, $
        ;; NORMALIZE_YRANGE=normalize_yRange, $
        OUT_ESTATS=eStats, $
        USER_INDS=user_inds, $
        USER_PLOTSUFF=user_plotSuff, $
        PLOTDIR=plotDir, $
        SUFF__PLOTDIR=suff__plotDir, $
        CUSTOM_TITLE=custom_title
  ENDFOR
END
