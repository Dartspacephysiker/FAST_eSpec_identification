;2016/06/02
PRO GET_H2D_NEWELLS__EACH_TYPE__NONALFVEN,eSpec,eSpec_i, $
                                          ALFDB_PLOT_STRUCT=alfDB_plot_struct, $
                                          IMF_STRUCT=IMF_struct, $
                                          MIMC_STRUCT=MIMC_struct, $
                                          NEWELL_PLOTRANGE=newell_plotRange, $
                                          LOG_NEWELLPLOT=log_newellPlot, $
                                          NEWELLPLOT_AUTOSCALE=newellPlot_autoscale, $
                                          NEWELLPLOT_NORMALIZE=newellPlot_normalize, $
                                          NEWELLPLOT_PROBOCCURRENCE=newellPlot_probOccurrence, $
                                          T_PROBOCCURRENCE=t_ProbOccurrence, $
                                          T_PROBOCC_PLOTRANGE=t_probOcc_plotRange, $
                                          THISTDENOMINATOR=tHistDenominator, $
                                          NEWELL_2009_INTERP=newell_2009_interp, $
                                          COMB_ACCELERATED=comb_accelerated, $
                                          TMPLT_H2DSTR=tmplt_h2dStr, $
                                          H2DSTRS=h2dStrs, $
                                          ;; H2DMASKSTR=h2dMaskStr, $
                                          H2DFLUXN=h2dFluxN, $
                                          NEWELL_NONZERO_NEV_I=newell_nonzero_nEv_i, $
                                          ;; MASKMIN=maskMin, $
                                          DATANAMES=dataNames, $
                                          DATARAWPTRS=dataRawPtrs, $
                                          CB_FORCE_OOBHIGH=cb_force_oobHigh, $
                                          CB_FORCE_OOBLOW=cb_force_oobLow, $
                                          PRINT_MANDM=print_mAndM, $
                                          LUN=lun


  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;This common block is defined ONLY here, in GET_ESPEC_ION_DB_IND, and in LOAD_NEWELL_ESPEC_DB
  @common__newell_espec.pro
  
  ;; IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
  ;;    LOAD_NEWELL_ESPEC_DB,espec ;,/DONT_LOAD_IN_MEMORY
  ;; ENDIF ELSE BEGIN
  ;;    ;; eSpec  = TEMPORARY(NEWELL__eSpec)
  ;;    eSpec     = NEWELL__eSpec
  ;; ENDELSE

  IF KEYWORD_SET(newell_2009_interp) THEN BEGIN
     IF ~NEWELL__eSpec.info.Newell2009interp THEN BEGIN
        PRINT,"Well you've got yourself a mismatch. Figure it out."
        STOP
     ENDIF
  ENDIF

  tmp_eSpec    = { $
                 ;;x       : NEWELL__eSpec.x[eSpec_i]         , $
                 ;;orbit   : NEWELL__eSpec.orbit[eSpec_i]     , $
                 MLT     : NEWELL__eSpec.mlt[eSpec_i]       , $
                 ILAT    : NEWELL__eSpec.ilat[eSpec_i]      , $
                 ;;ALT     : NEWELL__eSpec.alt[eSpec_i]       , $
                 mono    : NEWELL__eSpec.mono[eSpec_i]      , $
                 broad   : NEWELL__eSpec.broad[eSpec_i]     , $
                 diffuse : NEWELL__eSpec.diffuse[eSpec_i]   , $
                 info    : NEWELL__eSpec.info               }              ; , $

  IF KEYWORD_SET(MIMC_struct.use_Lng) THEN BEGIN
     STR_ELEMENT,tmp_eSpec,'lng',NEWELL__eSpec.lng[eSpec_i],/ADD_REPLACE
  ENDIF

  IF KEYWORD_SET(t_probOccurrence) THEN BEGIN
     STR_ELEMENT,tmp_eSpec,'delta_t',NEWELL__delta_t[eSpec_i],/ADD_REPLACE
  ENDIF
  ;; Je:NEWELL__eSpec.Je[eSpec_i]            , $
  ;; Jee:NEWELL__eSpec.Jee[eSpec_i]};;       , $
  ;; nBad_eSpec:NEWELL__eSpec.nBad_eSpec[eSpec_i]}

  ;;The main body
  GET_H2D_NEWELLS__BODY,tmp_eSpec, $
                        /NONALFVEN, $
                        ALFDB_PLOT_STRUCT=alfDB_plot_struct, $
                        IMF_STRUCT=IMF_struct, $
                        MIMC_STRUCT=MIMC_struct, $
                        NEWELL_PLOTRANGE=newell_plotRange, $
                        LOG_NEWELLPLOT=log_newellPlot, $
                        NEWELLPLOT_AUTOSCALE=newellPlot_autoscale, $
                        NEWELLPLOT_NORMALIZE=newellPlot_normalize, $
                        NEWELLPLOT_PROBOCCURRENCE=newellPlot_probOccurrence, $
                        T_PROBOCCURRENCE=t_probOccurrence, $
                        T_PROBOCC_PLOTRANGE=t_probOcc_plotRange, $
                        THISTDENOMINATOR=tHistDenominator, $
                        COMB_ACCELERATED=comb_accelerated, $
                        TMPLT_H2DSTR=tmplt_h2dStr, $
                        H2DSTRS=h2dStrs, $
                        ;; H2DMASKSTR=h2dMaskStr, $
                        H2DFLUXN=h2dFluxN, $
                        NEWELL_NONZERO_NEV_I=newell_nonzero_nEv_i, $
                        ;; MASKMIN=maskMin, $
                        DATANAMES=dataNames, $
                        DATARAWPTRS=dataRawPtrs, $
                        CB_FORCE_OOBHIGH=cb_force_oobHigh, $
                        CB_FORCE_OOBLOW=cb_force_oobLow, $
                        PRINT_MANDM=print_mAndM, $
                        LUN=lun

END