;2016/06/07
PRO GET_NONALFVEN_FLUX_DATA,plot_i, $
                            FOR_STORMS=for_storms, $
                            FOR_IMF_SCREENING=for_IMF_screening, $
                            NONALFVEN__JUNK_ALFVEN_CANDIDATES=nonAlfven__junk_alfven_candidates, $
                            NONALFVEN__ALL_FLUXES=nonalfven__all_fluxes, $
                            DESPUN_ALF_DB=despun_alf_db, $
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
                            INDICES__NONALFVEN_ESPEC=eSpec_i, $
                            INDICES__NONALFVEN_ION=ion_i, $
                            ESPEC__MLTS=eSpec__mlts, $
                            ESPEC__ILATS=eSpec__ilats, $
                            ION__MLTS=ion__mlts, $
                            ION__ILATS=ion__ilats, $
                            ORBRANGE=orbRange, $
                            ALTITUDERANGE=altitudeRange, $
                            CHARERANGE=charERange, $
                            CHARIERANGE=charIERange, $
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
                            SATELLITE=satellite, $
                            SMOOTH_IMF=smoothWindow, $
                            BYMIN=byMin, $
                            BYMAX=byMax, $
                            BZMIN=bzMin, $
                            BZMAX=bzMax, $
                            BTMIN=btMin, $
                            BTMAX=btMax, $
                            BXMIN=bxMin, $
                            BXMAX=bxMax, $
                            DO_ABS_BYMIN=abs_byMin, $
                            DO_ABS_BYMAX=abs_byMax, $
                            DO_ABS_BZMIN=abs_bzMin, $
                            DO_ABS_BZMAX=abs_bzMax, $
                            DO_ABS_BTMIN=abs_btMin, $
                            DO_ABS_BTMAX=abs_btMax, $
                            DO_ABS_BXMIN=abs_bxMin, $
                            DO_ABS_BXMAX=abs_bxMax, $
                            RESET_OMNI_INDS=reset_omni_inds, $
                            CLOCKSTR=clockStr, $
                            DONT_CONSIDER_CLOCKANGLES=dont_consider_clockAngles, $
                            RESTRICT_WITH_THESE_I=restrict_with_these_i, $
                            BX_OVER_BYBZ=Bx_over_ByBz_Lim, $
                            MULTIPLE_DELAYS=multiple_delays, $
                            RESOLUTION_DELAY=delay_res, $
                            BINOFFSET_DELAY=binOffset_delay, $
                            MULTIPLE_IMF_CLOCKANGLES=multiple_IMF_clockAngles, $
                            STABLEIMF=stableIMF, $
                            DO_NOT_CONSIDER_IMF=do_not_consider_IMF, $
                            OMNI_COORDS=omni_Coords, $
                            ANGLELIM1=angleLim1, $
                            ANGLELIM2=angleLim2, $
                            ;; GET_ESPEC_I_NOT_ION_I=get_eSpec_i, $
                            RESET_GOOD_INDS=reset_good_inds, $
                            DO_NOT_SET_DEFAULTS=do_not_set_defaults

  COMPILE_OPT idl2

  IF KEYWORD_SET(eNumFlPlots) OR KEYWORD_SET(ePlots) THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,eSpec;,/DONT_LOAD_IN_MEMORY

     IF ~KEYWORD_SET(for_IMF_screening) THEN BEGIN ;If doing IMF stuff, GET_RESTRICTED_AND_INTERPED_DB_INDICES will handle this
        good_eSpec_i         = GET_ESPEC_ION_DB_IND(eSpec,satellite,lun, $
                                            ;; DBFILE=dbfile, $
                                            ;; DBDIR=dbDir, $
                                            ORBRANGE=orbRange, $
                                            ALTITUDERANGE=altitudeRange, $
                                            CHARERANGE=charERange, $
                                            ;; CHARIERANGE=charIERange, $
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

        nBef_eSpec           = N_ELEMENTS(good_eSpec_i)
     ENDIF
  ENDIF

  IF KEYWORD_SET(ionPlots) THEN BEGIN
     LOAD_NEWELL_ION_DB,ion;,OUT_GOOD_I=basicClean_ion_i

     IF ~KEYWORD_SET(for_IMF_screening) THEN BEGIN ;If doing IMF stuff, GET_RESTRICTED_AND_INTERPED_DB_INDICES will handle this
        good_ion_i           = GET_ESPEC_ION_DB_IND(ion,satellite,lun, $
                                          DBFILE=dbfile, $
                                          DBDIR=dbDir, $
                                          ORBRANGE=orbRange, $
                                          ALTITUDERANGE=altitudeRange, $
                                          ;; CHARERANGE=charERange, $
                                          CHARIERANGE=charERange, $
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
  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Doing storm stuff?? Doing IMF stuff??
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  CASE 1 OF
     KEYWORD_SET(for_storms): BEGIN

        ;;Electrons?
        IF KEYWORD_SET(eNumFlPlots) OR KEYWORD_SET(ePlots) THEN BEGIN
           todaysEspecFile   = TODAYS_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_ESPEC_INDICES(SUFFIX=indFileSuff, $
                                                                                         DSTCUTOFF=dstCutoff, $
                                                                                         /MOST_RECENT)
           PRINT,'Getting ' + STRUPCASE(for_storms) + ' nonAlfven electron data ...'
           RESTORE,todaysEspecFile
           CASE 1 OF
              STRLOWCASE(for_storms) EQ 'nonstorm': BEGIN
                 eSpec_i     = CGSETINTERSECTION(good_eSpec_i,ns_i,COUNT=nAft_eSpec)
              END
              STRLOWCASE(for_storms) EQ 'mainphase': BEGIN
                 eSpec_i     = CGSETINTERSECTION(good_eSpec_i,mp_i,COUNT=nAft_eSpec)
              END
              STRLOWCASE(for_storms) EQ 'recoveryphase': BEGIN
                 eSpec_i     = CGSETINTERSECTION(good_eSpec_i,rp_i,COUNT=nAft_eSpec)
              END
           ENDCASE

           ;; PRINT,"Dropped " + STRCOMPRESS(nBef_eSpec-nAft_eSpec,/REMOVE_ALL) + " NaN- and INF-type Alfvén ion events..."
           PRINT,"Dropped " + STRCOMPRESS(nBef_eSpec-nAft_eSpec,/REMOVE_ALL) + " Alfvén eSpec events not associated with " + for_storms + " times ..."
           PRINT,FORMAT='(I0," remaining ...")',nAft_eSpec
        ENDIF

        ;;Now ions
        IF KEYWORD_SET(ionPlots) THEN BEGIN
           todaysIonFile     = TODAYS_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_ION_INDICES(SUFFIX=indFileSuff, $
                                                                                       DSTCUTOFF=dstCutoff, $
                                                                                       /MOST_RECENT)
           PRINT,'Getting ' + STRUPCASE(for_storms) + ' nonAlfven ion data ...'
           RESTORE,todaysIonFile
           CASE 1 OF
              STRLOWCASE(for_storms) EQ 'nonstorm': BEGIN
                 ion_i       = CGSETINTERSECTION(good_ion_i,ns_i,COUNT=nAft_ion)
              END
              STRLOWCASE(for_storms) EQ 'mainphase': BEGIN
                 ion_i       = CGSETINTERSECTION(good_ion_i,mp_i,COUNT=nAft_ion)
              END
              STRLOWCASE(for_storms) EQ 'recoveryphase': BEGIN
                 ion_i       = CGSETINTERSECTION(good_ion_i,rp_i,COUNT=nAft_ion)
              END
           ENDCASE

           ;; PRINT,"Dropped " + STRCOMPRESS(nBef_ion-nAft_ion,/REMOVE_ALL) + " NaN- and INF-type Alfvén ion events..."
           PRINT,"Dropped " + STRCOMPRESS(nBef_ion-nAft_ion,/REMOVE_ALL) + " Alfvén ion events not associated with " + for_storms + " times ..."
           PRINT,FORMAT='(I0," remaining ...")',nAft_ion

        ENDIF
     END
     KEYWORD_SET(for_IMF_screening): BEGIN
        ;;In this case, we're going to grab all the IMF-type inds a bit later
        IF KEYWORD_SET(eNumFlPlots) OR KEYWORD_SET(ePlots) THEN BEGIN
           eSpec_i_list      = GET_RESTRICTED_AND_INTERPED_DB_INDICES(eSpec,satellite,delay,LUN=lun, $
                                                                                   ;; DBTIMES=cdbTime, $
                                                                                   DBFILE=dbfile, $
                                                                                   HEMI=hemi, $
                                                                                   ORBRANGE=orbRange, $
                                                                                   ALTITUDERANGE=altitudeRange, $
                                                                                   CHARERANGE=charERange, $
                                                                                   ;; CHARIERANGE=charIERange, $ ;Only for non-Alfvén ions
                                                                                   SAMPLE_T_RESTRICTION=sample_t_restriction, $
                                                                                   MINMLT=minM, $
                                                                                   MAXMLT=maxM, $
                                                                                   BINM=binM, $
                                                                                   SHIFTM=shiftM, $
                                                                                   MINILAT=minI, $
                                                                                   MAXILAT=maxI, $
                                                                                   BINI=binI, $
                                                                                   DO_LSHELL=do_lshell, $
                                                                                   MINLSHELL=minL, $
                                                                                   MAXLSHELL=maxL, $
                                                                                   BINL=binL, $
                                                                                   ;; MIN_MAGCURRENT=minMC, $
                                                                                   ;; MAX_NEGMAGCURRENT=maxNegMC, $
                                                                                   SMOOTH_IMF=smoothWindow, $
                                                                                   BYMIN=byMin, $
                                                                                   BYMAX=byMax, $
                                                                                   BZMIN=bzMin, $
                                                                                   BZMAX=bzMax, $
                                                                                   BTMIN=btMin, $
                                                                                   BTMAX=btMax, $
                                                                                   BXMIN=bxMin, $
                                                                                   BXMAX=bxMax, $
                                                                                   DO_ABS_BYMIN=abs_byMin, $
                                                                                   DO_ABS_BYMAX=abs_byMax, $
                                                                                   DO_ABS_BZMIN=abs_bzMin, $
                                                                                   DO_ABS_BZMAX=abs_bzMax, $
                                                                                   DO_ABS_BTMIN=abs_btMin, $
                                                                                   DO_ABS_BTMAX=abs_btMax, $
                                                                                   DO_ABS_BXMIN=abs_bxMin, $
                                                                                   DO_ABS_BXMAX=abs_bxMax, $
                                                                                   RESET_OMNI_INDS=reset_omni_inds, $
                                                                                   CLOCKSTR=clockStr, $
                                                                                   DONT_CONSIDER_CLOCKANGLES=dont_consider_clockAngles, $
                                                                                   RESTRICT_WITH_THESE_I=restrict_with_these_i, $
                                                                                   DO_NOT_SET_DEFAULTS=do_not_set_defaults, $
                                                                                   BX_OVER_BYBZ=Bx_over_ByBz_Lim, $
                                                                                   MULTIPLE_DELAYS=multiple_delays, $
                                                                                   RESOLUTION_DELAY=delay_res, $
                                                                                   BINOFFSET_DELAY=binOffset_delay, $
                                                                                   MULTIPLE_IMF_CLOCKANGLES=multiple_IMF_clockAngles, $
                                                                                   STABLEIMF=stableIMF, $
                                                                                   DO_NOT_CONSIDER_IMF=do_not_consider_IMF, $
                                                                                   OMNI_COORDS=omni_Coords, $
                                                                                   ANGLELIM1=angleLim1, $
                                                                                   ANGLELIM2=angleLim2, $
                                                                                   HWMAUROVAL=HwMAurOval, $
                                                                                   HWMKPIND=HwMKpInd, $
                                                                                   RESET_GOOD_INDS=reset_good_inds);; , $
                                                                                   ;; NO_BURSTDATA=no_burstData)


        ENDIF
        IF KEYWORD_SET(ionPlots) THEN BEGIN
           ion_i_list        = GET_RESTRICTED_AND_INTERPED_DB_INDICES(ion,satellite,delay,LUN=lun, $
                                                                                  ;; DBTIMES=cdbTime, $
                                                                                  DBFILE=dbfile, $
                                                                                  HEMI=hemi, $
                                                                                  ORBRANGE=orbRange, $
                                                                                  ALTITUDERANGE=altitudeRange, $
                                                                                  ;; CHARERANGE=charERange, $
                                                                                  CHARIERANGE=charIERange, $ ;Only for non-Alfvén ions
                                                                                  SAMPLE_T_RESTRICTION=sample_t_restriction, $
                                                                                  MINMLT=minM, $
                                                                                  MAXMLT=maxM, $
                                                                                   BINM=binM, $
                                                                                   SHIFTM=shiftM, $
                                                                                   MINILAT=minI, $
                                                                                   MAXILAT=maxI, $
                                                                                   BINI=binI, $
                                                                                   DO_LSHELL=do_lshell, $
                                                                                   MINLSHELL=minL, $
                                                                                   MAXLSHELL=maxL, $
                                                                                   BINL=binL, $
                                                                                   ;; MIN_MAGCURRENT=minMC, $
                                                                                   ;; MAX_NEGMAGCURRENT=maxNegMC, $
                                                                                   SMOOTH_IMF=smoothWindow, $
                                                                                   BYMIN=byMin, $
                                                                                   BYMAX=byMax, $
                                                                                   BZMIN=bzMin, $
                                                                                   BZMAX=bzMax, $
                                                                                   BTMIN=btMin, $
                                                                                   BTMAX=btMax, $
                                                                                   BXMIN=bxMin, $
                                                                                   BXMAX=bxMax, $
                                                                                   DO_ABS_BYMIN=abs_byMin, $
                                                                                   DO_ABS_BYMAX=abs_byMax, $
                                                                                   DO_ABS_BZMIN=abs_bzMin, $
                                                                                   DO_ABS_BZMAX=abs_bzMax, $
                                                                                   DO_ABS_BTMIN=abs_btMin, $
                                                                                   DO_ABS_BTMAX=abs_btMax, $
                                                                                   DO_ABS_BXMIN=abs_bxMin, $
                                                                                   DO_ABS_BXMAX=abs_bxMax, $
                                                                                   RESET_OMNI_INDS=reset_omni_inds, $
                                                                                   CLOCKSTR=clockStr, $
                                                                                   DONT_CONSIDER_CLOCKANGLES=dont_consider_clockAngles, $
                                                                                   RESTRICT_WITH_THESE_I=restrict_with_these_i, $
                                                                                   DO_NOT_SET_DEFAULTS=do_not_set_defaults, $
                                                                                   BX_OVER_BYBZ=Bx_over_ByBz_Lim, $
                                                                                   MULTIPLE_DELAYS=multiple_delays, $
                                                                                   RESOLUTION_DELAY=delay_res, $
                                                                                   BINOFFSET_DELAY=binOffset_delay, $
                                                                                   MULTIPLE_IMF_CLOCKANGLES=multiple_IMF_clockAngles, $
                                                                                   STABLEIMF=stableIMF, $
                                                                                   DO_NOT_CONSIDER_IMF=do_not_consider_IMF, $
                                                                                   OMNI_COORDS=omni_Coords, $
                                                                                   ANGLELIM1=angleLim1, $
                                                                                   ANGLELIM2=angleLim2, $
                                                                                   HWMAUROVAL=HwMAurOval, $
                                                                                   HWMKPIND=HwMKpInd, $
                                                                                   RESET_GOOD_INDS=reset_good_inds);; , $
                                                                                   ;; NO_BURSTDATA=no_burstData)


        ENDIF
     END
     ELSE: BEGIN

        ;;Electrons?
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

        ;;Now ions
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
     END
  ENDCASE

  ;;Electrons
  IF ~KEYWORD_SET(nonalfven__all_fluxes) THEN BEGIN
     IF KEYWORD_SET(eNumFlPlots) OR KEYWORD_SET(ePlots) THEN BEGIN
        LOAD_ALF_NEWELL_ESPEC_DB,!NULL,good_alf_eSpec_i,good_eSpec_assoc_w_alf_i, $
                                 DESPUN_ALF_DB=despun_alf_db, $
                                 /DONT_LOAD_IN_MEMORY

        nBef_eSpec           = N_ELEMENTS(eSpec_i)

        IF KEYWORD_SET(nonAlfven__junk_alfven_candidates) THEN BEGIN
           PRINT,"Even junking electron measurements associated with Alfvén wave CANDIDATES!"
           eSpec_i           = CGSETDIFFERENCE(eSpec_i,good_eSpec_assoc_w_alf_i,COUNT=nAft_eSpec)
        ENDIF ELSE BEGIN
           tmp_alf_eSpec_i   = CGSETINTERSECTION(plot_i,good_alf_eSpec_i,INDICES_B=eSpec_deleteable_ii)
           eSpec_i           = CGSETDIFFERENCE(eSpec_i,good_eSpec_assoc_w_alf_i[eSpec_deleteable_ii],COUNT=nAft_eSpec)
        ENDELSE

        PRINT,"Dropped " + STRCOMPRESS(nBef_eSpec-nAft_eSpec,/REMOVE_ALL) + " Alfvén electron events..."
        PRINT,FORMAT='(I0," remaining ...")',nAft_eSpec

     ENDIF

     IF KEYWORD_SET(ionPlots) THEN BEGIN
        LOAD_ALF_NEWELL_ION_DB,good_alf_ion_i,good_iSpec_assoc_w_alf_i, $
                               DESPUN_ALF_DB=despun_alf_db, $
                               /DONT_LOAD_IN_MEMORY

        nBef_ion             = N_ELEMENTS(ion_i)

        IF KEYWORD_SET(nonAlfven__junk_alfven_candidates) THEN BEGIN
           PRINT,"Even junking ion measurements associated with Alfvén wave CANDIDATES!"
           ion_i             = CGSETDIFFERENCE(ion_i,good_iSpec_assoc_w_alf_i,COUNT=nAft_ion)
        ENDIF ELSE BEGIN
           tmp_alf_ion_i     = CGSETINTERSECTION(plot_i,good_alf_ion_i,INDICES_B=ion_deleteable_ii)
           ion_i             = CGSETDIFFERENCE(ion_i,good_iSpec_assoc_w_alf_i[ion_deleteable_ii],COUNT=nAft_ion)
        ENDELSE

        PRINT,"Dropped " + STRCOMPRESS(nBef_ion-nAft_ion,/REMOVE_ALL) + " Alfvén ion events..."
        PRINT,FORMAT='(I0," remaining ...")',nAft_ion
     ENDIF
  ENDIF

  ;;Now get the data
  IF KEYWORD_SET(ePlots) THEN BEGIN
     eFluxPlotType           = 'eFlux_nonAlfven'
     ;; eFlux_data           = eSpec.jee[eSpec_i]
     eFlux_data              = eSpec.jee
  ENDIF

  IF KEYWORD_SET(eNumFlPlots) THEN BEGIN
     eNumFlPlotType          = 'eNumFlux_nonAlfven'
     ;; eNumFlux_data        = eSpec.je[eSpec_i]
     eNumFlux_data           = eSpec.je
  ENDIF

  IF KEYWORD_SET(ionPlots) THEN BEGIN
     CASE 1 OF
        STRUPCASE(iFluxPlotType) EQ 'ENERGY': BEGIN
           iFluxPlotType     = 'JEi_nonAlfven'
           ;; iFlux_data     = ion.jei[ion_i]
           iFlux_data        = ion.jei
        END
        ELSE: BEGIN
           iFluxPlotType     = 'Ji_nonAlfven'
           ;; iNumFlux_data  = ion.ji[ion_i]
           iNumFlux_data     = ion.ji
        END
     ENDCASE
  ENDIF

  ;;Last, get the MLTs and ILATs
  ;;Electrons
  IF KEYWORD_SET(eNumFlPlots) OR KEYWORD_SET(ePlots) THEN BEGIN
     ;; eSpec__mlts          = eSpec.mlt[eSpec_i]
     ;; eSpec__ilats         = eSpec.ilat[eSpec_i]
     eSpec__mlts             = eSpec.mlt
     eSpec__ilats            = eSpec.ilat
  ENDIF

  ;;Ions
  IF KEYWORD_SET(ionPlots) THEN BEGIN
     ;; ion__mlts            = ion.mlt[ion_i]
     ;; ion__ilats           = ion.ilat[ion_i]
     ion__mlts               = ion.mlt
     ion__ilats              = ion.ilat
  ENDIF



END