;2016/06/02
PRO GET_H2D_NEWELLS__EACH_TYPE__NONALFVEN,eSpec,eSpec_i, $
                                          MINM=minM, $
                                          MAXM=maxM, $
                                          BINM=binM, $
                                          SHIFTM=shiftM, $
                                          MINI=minI, $
                                          MAXI=maxI, $
                                          BINI=binI, $
                                          EQUAL_AREA_BINNING=EA_binning, $
                                          NEWELL_PLOTRANGE=newell_plotRange, $
                                          LOG_NEWELLPLOT=log_newellPlot, $
                                          NEWELLPLOT_AUTOSCALE=newellPlot_autoscale, $
                                          NEWELLPLOT_NORMALIZE=newellPlot_normalize, $
                                          NEWELLPLOT_PROBOCCURRENCE=newellPlot_probOccurrence, $
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


  COMPILE_OPT idl2

  ;;This common block is defined ONLY here, in GET_ESPEC_ION_DB_IND, and in LOAD_NEWELL_ESPEC_DB
  @common__newell_espec.pro
  
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,espec ;,/DONT_LOAD_IN_MEMORY
  ENDIF ELSE BEGIN
     ;; eSpec  = TEMPORARY(NEWELL__eSpec)
     eSpec     = NEWELL__eSpec
  ENDELSE

  tmp_eSpec    = { $
                 ;;x       : eSpec.x[eSpec_i]         , $
                 ;;orbit   : eSpec.orbit[eSpec_i]     , $
                 MLT     : eSpec.mlt[eSpec_i]       , $
                 ILAT    : eSpec.ilat[eSpec_i]      , $
                 ;;ALT     : eSpec.alt[eSpec_i]       , $
                 mono    : eSpec.mono[eSpec_i]      , $
                 broad   : eSpec.broad[eSpec_i]     , $
                 diffuse : eSpec.diffuse[eSpec_i]} ; , $
  ;; Je:eSpec.Je[eSpec_i]            , $
  ;; Jee:eSpec.Jee[eSpec_i]};;       , $
  ;; nBad_eSpec:eSpec.nBad_eSpec[eSpec_i]}

  ;;The main body
  GET_H2D_NEWELLS__BODY,tmp_eSpec, $
                        MINM=minM, $
                        MAXM=maxM, $
                        BINM=binM, $
                        SHIFTM=shiftM, $
                        MINI=minI, $
                        MAXI=maxI, $
                        BINI=binI, $
                        EQUAL_AREA_BINNING=EA_binning, $
                        NEWELL_PLOTRANGE=newell_plotRange, $
                        LOG_NEWELLPLOT=log_newellPlot, $
                        NEWELLPLOT_AUTOSCALE=newellPlot_autoscale, $
                        NEWELLPLOT_NORMALIZE=newellPlot_normalize, $
                        NEWELLPLOT_PROBOCCURRENCE=newellPlot_probOccurrence, $
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