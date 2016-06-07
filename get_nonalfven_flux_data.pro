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
                            OUT_EPLOT_DATA=ePlot_data, $
                            OUT_ENUMFLPLOT_DATA=eNumFlPlot_data, $
                            OUT_IONPLOT_DATA=ionPlot_data
  

  COMPILE_OPT idl2

  IF KEYWORD_SET(for_storms) THEN BEGIN
     todaysIonFile   = TODAYS_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_ION_INDICES(SUFFIX=indFileSuff, $
                                                                               DSTCUTOFF=dstCutoff, $
                                                                               /MOST_RECENT)
     
     todaysEspecFile = TODAYS_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_ESPEC_INDICES(SUFFIX=indFileSuff, $
                                                                                 DSTCUTOFF=dstCutoff, $
                                                                                 /MOST_RECENT)
     IF KEYWORD_SET(eNumFlPlots) OR KEYWORD_SET(ePlots) THEN BEGIN
        PRINT,'Getting nonAlfven electron data ...'
        RESTORE,todaysEspecFile
        LOAD_ALF_NEWELL_ESPEC_DB
        LOAD_NEWELL_ESPEC_DB,eSpec
        CASE 1 OF
           STRLOWCASE(for_storms) EQ 'nonstorm': BEGIN
              eSpec_i      = ns_i
           END
           STRLOWCASE(for_storms) EQ 'mainphase': BEGIN
              eSpec_i      = mp_i
           END
           STRLOWCASE(for_storms) EQ 'recoveryphase': BEGIN
              eSpec_i      = rp_i
           END
        ENDCASE

     ENDIF
     IF KEYWORD_SET(ionPlots) THEN BEGIN
        PRINT,'Getting nonAlfven ion data ...'

        RESTORE,todaysIonFile
        LOAD_ALF_NEWELL_ION_DB
        LOAD_NEWELL_ION_DB,ion
        CASE 1 OF
           STRLOWCASE(for_storms) EQ 'nonstorm': BEGIN
              ion_i       = ns_i
           END
           STRLOWCASE(for_storms) EQ 'mainphase': BEGIN
              ion_i       = mp_i
           END
           STRLOWCASE(for_storms) EQ 'recoveryphase': BEGIN
              ion_i       = rp_i
           END
        ENDCASE
     ENDIF
  ENDIF ELSE BEGIN
  
     IF KEYWORD_SET(eNumFlPlots) OR KEYWORD_SET(ePlots) THEN BEGIN
        LOAD_NEWELL_ESPEC_DB,eSpec
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
        LOAD_NEWELL_ION_DB,ion
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
     LOAD_ALF_NEWELL_ESPEC_DB,!NULL,good_alf_eSpec_i,good_eSpec_i,/DONT_LOAD_IN_MEMORY
     tmp_alf_eSpec_i = CGSETINTERSECTION(plot_i,good_alf_eSpec_i,INDICES_B=eSpec_deleteable_ii)
     eSpec_i         = CGSETDIFFERENCE(eSpec_i,good_eSpec_i[eSpec_deleteable_ii])
  ENDIF

  ;;Ions
  IF KEYWORD_SET(ionPlots) THEN BEGIN
     LOAD_ALF_NEWELL_ION_DB,good_alf_ion_i,good_iSpec_i,/DONT_LOAD_IN_MEMORY
     tmp_alf_ion_i = CGSETINTERSECTION(plot_i,good_alf_ion_i,INDICES_B=ion_deleteable_ii)
     ion_i         = CGSETDIFFERENCE(ion_i,good_iSpec_i[ion_deleteable_ii])
  ENDIF

  ;;Now get the data
  IF KEYWORD_SET(ePlots) THEN BEGIN
     eFluxPlotType = 'eFlux_nonAlfven'
     ePlot_data    = eSpec.jee[eSpec_i]
  ENDIF

  IF KEYWORD_SET(eNumFlPlots) THEN BEGIN
     eNumFlPlotType  = 'eNumFlux_nonAlfven'
     eNumFlPlot_data = eSpec.je[eSpec_i]
  ENDIF

  IF KEYWORD_SET(ionPlots) THEN BEGIN
     CASE 1 OF
        STRUPCASE(iFluxPlotType) EQ 'ENERGY': BEGIN
           iFluxPlotType = 'JEi_nonAlfven'
           ionPlot_data  = ion.jei[eSpec_i]
        END
        ELSE: BEGIN
           iFluxPlotType = 'Ji_nonAlfven'
           ionPlot_data  = ion.ji[eSpec_i]
        END
     ENDCASE
  ENDIF
  
END