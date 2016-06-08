;2016/06/07
PRO GET_NONALFVEN_FLUX_DATA,plot_i, $
                            FOR_STORMS=for_storms, $
                            T1_ARR=t1_arr,T2_ARR=t2_arr, $
                            EPLOTS=ePlots, $
                            EFLUXPLOTTYPE=eFluxPlotType, $
                            ENUMFLPLOTS=eNumFlPlots, $
                            ENUMFLPLOTTYPE=eNumFlPlotType, $
                            IONPLOTS=ionPlots, $
                            IFLUXPLOTTYPE=iFluxPlotType, $
                            OUT_EFLUX_DATA=eFlux_data, $
                            OUT_ENUMFLUX_DATA=eNumFlux_data, $
                            OUT_IFLUX_DATA=iFlux_data, $
                            OUT_INUMFLUX_DATA=iNumFlux_data, $
                            ESPEC__MLTS=eSpec__mlts, $
                            ESPEC__ILATS=eSpec__ilats, $
                            ION__MLTS=ion__mlts, $
                            ION__ILATS=ion__ilats, $
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
                            ;; GET_ESPEC_I_NOT_ION_I=get_eSpec_i, $
                            RESET_GOOD_INDS=reset_good_inds, $
                            DO_NOT_SET_DEFAULTS=do_not_set_defaults

  COMPILE_OPT idl2

  IF KEYWORD_SET(eNumFlPlots) OR KEYWORD_SET(ePlots) THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,eSpec;,/DONT_LOAD_IN_MEMORY

     good_eSpec_i = GET_ESPEC_ION_DB_IND(eSpec,lun, $
                                         ;; DBFILE=dbfile, $
                                         ;; DBDIR=dbDir, $
                                         ORBRANGE=orbRange, $
                                         ;; ALTITUDERANGE=altitudeRange, $
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
                                         /GET_ESPEC_I_NOT_ION_I, $
                                         ;; GET_ION_I=get_ion_i, $
                                         RESET_GOOD_INDS=reset_good_inds, $
                                         DO_NOT_SET_DEFAULTS=do_not_set_defaults, $
                                         ;; /DONT_LOAD_IN_MEMORY, $
                                         ;; DONT_LOAD_IN_MEMORY=nonMem, $
                                         /PRINT_PARAM_SUMMARY)

     nBef_eSpec             = N_ELEMENTS(good_eSpec_i)

  ENDIF

  IF KEYWORD_SET(ionPlots) THEN BEGIN
     LOAD_NEWELL_ION_DB,ion;,OUT_GOOD_I=basicClean_ion_i

     good_ion_i = GET_ESPEC_ION_DB_IND(ion,lun, $
                                       ;; DBFILE=dbfile, $
                                       ;; DBDIR=dbDir, $
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
                                       ;; GET_ESPEC_I_NOT_ION_I=get_eSpec_i, $
                                       /GET_ION_I, $
                                       RESET_GOOD_INDS=reset_good_inds, $
                                       DO_NOT_SET_DEFAULTS=do_not_set_defaults, $
                                       ;; /DONT_LOAD_IN_MEMORY, $
                                       ;; DONT_LOAD_IN_MEMORY=nonMem, $
                                       /PRINT_PARAM_SUMMARY)

     nBef_ion             = N_ELEMENTS(good_ion_i)

  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Doing storm stuff??
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  IF KEYWORD_SET(for_storms) THEN BEGIN
     
     IF KEYWORD_SET(eNumFlPlots) OR KEYWORD_SET(ePlots) THEN BEGIN
        todaysEspecFile   = TODAYS_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_ESPEC_INDICES(SUFFIX=indFileSuff, $
                                                                                      DSTCUTOFF=dstCutoff, $
                                                                                      /MOST_RECENT)
        PRINT,'Getting nonAlfven electron data ...'
        RESTORE,todaysEspecFile
        CASE 1 OF
           STRLOWCASE(for_storms) EQ 'nonstorm': BEGIN
              eSpec_i     = CGSETINTERSECTION(good_eSpec_i,ns_i,COUNT=nAft)
           END
           STRLOWCASE(for_storms) EQ 'mainphase': BEGIN
              eSpec_i     = CGSETINTERSECTION(good_eSpec_i,mp_i,COUNT=nAft)
           END
           STRLOWCASE(for_storms) EQ 'recoveryphase': BEGIN
              eSpec_i     = CGSETINTERSECTION(good_eSpec_i,rp_i,COUNT=nAft)
           END
        ENDCASE

        PRINT,"Dropped " + STRCOMPRESS(nBef_eSpec-nAft,/REMOVE_ALL) + " NaN- and INF-type Alfvén ion events..."
     ENDIF
     IF KEYWORD_SET(ionPlots) THEN BEGIN
        todaysIonFile     = TODAYS_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_ION_INDICES(SUFFIX=indFileSuff, $
                                                                                    DSTCUTOFF=dstCutoff, $
                                                                                    /MOST_RECENT)
        PRINT,'Getting nonAlfven ion data ...'
        RESTORE,todaysIonFile
        CASE 1 OF
           STRLOWCASE(for_storms) EQ 'nonstorm': BEGIN
              ion_i       = CGSETINTERSECTION(good_ion_i,ns_i,COUNT=nAft)
           END
           STRLOWCASE(for_storms) EQ 'mainphase': BEGIN
              ion_i       = CGSETINTERSECTION(good_ion_i,mp_i,COUNT=nAft)
           END
           STRLOWCASE(for_storms) EQ 'recoveryphase': BEGIN
              ion_i       = CGSETINTERSECTION(good_ion_i,rp_i,COUNT=nAft)
           END
        ENDCASE

        ;;Clean 'em up
        nBef_i            = N_ELEMENTS(ion_i)
        ;; ion_i             = CGSETINTERSECTION(ion_i,basicClean_ion_i,COUNT=nAft)
        PRINT,"Dropped " + STRCOMPRESS(nBef_i-nAft,/REMOVE_ALL) + " NaN- and INF-type Alfvén ion events..."

     ENDIF
  ENDIF ELSE BEGIN
     
     IF KEYWORD_SET(eNumFlPlots) OR KEYWORD_SET(ePlots) THEN BEGIN
        GET_DATA_AVAILABILITY_FOR_ARRAY_OF_UTC_RANGES, $
           T1_ARR=t1_Arr, $
           T2_ARR=t2_Arr, $
           DBSTRUCT=eSpec, $
           DBTIMES=dbTimes, $
           /FOR_ESPEC_DB, $
           /DO_NOT_MAKE_ORB_INFO, $
           RESTRICT_W_THESEINDS=restrict, $
           OUT_INDS_LIST=eSpec_i, $
           UNIQ_ORBS_LIST=uniq_orbs_list,UNIQ_ORB_INDS_LIST=uniq_orb_inds_list, $
           INDS_ORBS_LIST=inds_orbs_list,TRANGES_ORBS_LIST=tranges_orbs_list,TSPANS_ORBS_LIST=tspans_orbs_list, $
           PRINT_DATA_AVAILABILITY=0, $
           GIVE_TIMESPLIT_INFO=give_timeSplit_info, $
           VERBOSE=verbose, $
           /LIST_TO_ARR, $
           LUN=logLun
     ENDIF

     IF KEYWORD_SET(ionPlots) THEN BEGIN
        GET_DATA_AVAILABILITY_FOR_ARRAY_OF_UTC_RANGES, $
           T1_ARR=t1_Arr, $
           T2_ARR=t2_Arr, $
           DBSTRUCT=ion, $
           DBTIMES=dbTimes, $
           /FOR_ESPEC_DB, $
           /DO_NOT_MAKE_ORB_INFO, $
           RESTRICT_W_THESEINDS=restrict, $
           OUT_INDS_LIST=ion_i, $
           UNIQ_ORBS_LIST=uniq_orbs_list,UNIQ_ORB_INDS_LIST=uniq_orb_inds_list, $
           INDS_ORBS_LIST=inds_orbs_list,TRANGES_ORBS_LIST=tranges_orbs_list,TSPANS_ORBS_LIST=tspans_orbs_list, $
           PRINT_DATA_AVAILABILITY=0, $
           GIVE_TIMESPLIT_INFO=give_timeSplit_info, $
           VERBOSE=verbose, $
           /LIST_TO_ARR, $
           LUN=logLun

     ENDIF

  ENDELSE

;;Electrons
  IF KEYWORD_SET(eNumFlPlots) OR KEYWORD_SET(ePlots) THEN BEGIN
     LOAD_ALF_NEWELL_ESPEC_DB,!NULL,good_alf_eSpec_i,good_eSpec_assoc_w_alf_i,/DONT_LOAD_IN_MEMORY

     nBef                 = N_ELEMENTS(ion_i)

     ;; tmp_alf_eSpec_i   = CGSETINTERSECTION(plot_i,good_alf_eSpec_i,INDICES_B=eSpec_deleteable_ii)
     ;; eSpec_i           = CGSETDIFFERENCE(eSpec_i,good_eSpec_assoc_w_alf_i[eSpec_deleteable_ii],COUNT=nAft)

     eSpec_i              = CGSETDIFFERENCE(eSpec_i,good_eSpec_assoc_w_alf_i,COUNT=nAft)
     PRINT,"Dropped " + STRCOMPRESS(nBef-nAft,/REMOVE_ALL) + " Alfvén electron events..."

     eSpec__mlts          = eSpec.mlt[eSpec_i]
     eSpec__ilats         = eSpec.ilat[eSpec_i]
  ENDIF

;;Ions
  IF KEYWORD_SET(ionPlots) THEN BEGIN
     LOAD_ALF_NEWELL_ION_DB,good_alf_ion_i,good_iSpec_assoc_w_alf_i,/DONT_LOAD_IN_MEMORY

     nBef                 = N_ELEMENTS(ion_i)

     ;; tmp_alf_ion_i     = CGSETINTERSECTION(plot_i,good_alf_ion_i,INDICES_B=ion_deleteable_ii)
     ;; ion_i             = CGSETDIFFERENCE(ion_i,iSpec_assoc_w_alf_i[ion_deleteable_ii],COUNT=nAft)

     ion_i                = CGSETDIFFERENCE(ion_i,good_iSpec_assoc_w_alf_i,COUNT=nAft)
     PRINT,"Dropped " + STRCOMPRESS(nBef-nAft,/REMOVE_ALL) + " Alfvén ion events..."

     ion__mlts            = ion.mlt[ion_i]
     ion__ilats           = ion.ilat[ion_i]
  ENDIF

;;Now get the data
  IF KEYWORD_SET(ePlots) THEN BEGIN
     eFluxPlotType        = 'eFlux_nonAlfven'
     eFlux_data           = eSpec.jee[eSpec_i]
  ENDIF

  IF KEYWORD_SET(eNumFlPlots) THEN BEGIN
     eNumFlPlotType       = 'eNumFlux_nonAlfven'
     eNumFlux_data        = eSpec.je[eSpec_i]
  ENDIF

  IF KEYWORD_SET(ionPlots) THEN BEGIN
     CASE 1 OF
        STRUPCASE(iFluxPlotType) EQ 'ENERGY': BEGIN
           iFluxPlotType  = 'JEi_nonAlfven'
           iFlux_data     = ion.jei[ion_i]
        END
        ELSE: BEGIN
           iFluxPlotType  = 'Ji_nonAlfven'
           iNumFlux_data  = ion.ji[ion_i]
        END
     ENDCASE
  ENDIF

END