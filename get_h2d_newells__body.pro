;;11/24/16
PRO GET_H2D_NEWELLS__BODY,eSpec, $
                          NONALFVEN=nonAlf, $
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
                          COMBINE_ACCELERATED=comb_accelerated, $
                          TMPLT_H2DSTR=tmplt_h2dStr, $
                          H2DSTRS=h2dStrs, $
                          H2DFLUXN=h2dFluxN, $
                          NEWELL_NONZERO_NEV_I=newell_nonzero_nEv_i, $
                          DATANAMES=dataNames, $
                          DATARAWPTRS=dataRawPtrs, $
                          CB_FORCE_OOBHIGH=cb_force_oobHigh, $
                          CB_FORCE_OOBLOW=cb_force_oobLow, $
                          PRINT_MANDM=print_mAndM, $
                          LUN=lun

  COMPILE_OPT IDL2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Indices
  broad_i      = WHERE((eSpec.broad  EQ 1) OR (eSpec.broad EQ 2),nBroad)
  diffuse_i    = WHERE(eSpec.diffuse EQ 1,nDiffuse)
  mono_i       = WHERE((eSpec.mono   EQ 1) OR (eSpec.mono  EQ 2),nMono)

  IF KEYWORD_SET(t_probOccurrence) THEN BEGIN
     i_list    = LIST(broad_i,diffuse_i,mono_i)
  ENDIF
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;MLTs
  mlt_broad    = eSpec.mlt[broad_i]-MIMC_struct.shiftM
  mlt_diffuse  = eSpec.mlt[diffuse_i]-MIMC_struct.shiftM
  mlt_mono     = eSpec.mlt[mono_i]-MIMC_struct.shiftM

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Screen 'em for negs
  bNegMLT = WHERE(mlt_broad LT 0)
  dNegMLT = WHERE(mlt_diffuse LT 0)
  mNegMLT = WHERE(mlt_mono LT 0)
  IF bNegMLT[0] NE -1 THEN BEGIN
     mlt_broad[bNegMLT]   = mlt_broad[bNegMLT] + 24
  ENDIF
  IF dNegMLT[0] NE -1 THEN BEGIN
     mlt_diffuse[dNegMLT] = mlt_diffuse[dNegMLT] + 24
  ENDIF
  IF mNegMLT[0] NE -1 THEN BEGIN
     mlt_mono[mNegMLT]    = mlt_mono[mNegMLT] + 24
  ENDIF
  mlt_list      = LIST(TEMPORARY(mlt_broad),TEMPORARY(mlt_diffuse),TEMPORARY(mlt_mono))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;ILATs
  ilat_broad    = eSpec.ilat[broad_i]
  ilat_diffuse  = eSpec.ilat[diffuse_i]
  ilat_mono     = eSpec.ilat[mono_i]
  ilat_list     = LIST(TEMPORARY(ilat_broad),TEMPORARY(ilat_diffuse),TEMPORARY(ilat_mono))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Bonus
  titles        = ['Broadband','Diffuse','Monoenergetic']
  dataNames     = ['broad'    ,'diffuse','mono'         ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Combined?
  IF KEYWORD_SET(comb_accelerated) THEN BEGIN
     comb_i    = CGSETUNION(mono_i,broad_i)

     mlt_comb  = eSpec.mlt[comb_i]-MIMC_struct.shiftM
     cNegMLT   = WHERE(mlt_comb LT 0)
     IF cNegMLT[0] NE -1 THEN BEGIN
        mlt_comb[cNegMLT] = mlt_comb[cNegMLT] + 24
     ENDIF
     
     ilat_comb  = eSpec.ilat[comb_i]

     mlt_list.Add,mlt_comb
     ilat_list.Add,ilat_comb

     titles     = [titles   ,'Accelerated']
     dataNames  = [dataNames,'accel'      ]
     Newell_plotRange = [[Newell_plotRange],[Newell_plotRange[*,0]+Newell_plotRange[*,1]]]

     IF KEYWORD_SET(t_ProbOccurrence) THEN BEGIN
        i_list.Add,comb_i
     ENDIF

  ENDIF

  IF KEYWORD_SET(nonAlf) THEN BEGIN
     dataNames += '_nonAlf'
  ENDIF

  IF KEYWORD_SET(eSpec.info.Newell2009interp) THEN BEGIN
     dataNames             += '__2009_interp'
  ENDIF

  h2dStrs                   = !NULL
  dataRawPtrs               = !NULL
  newell_nonzero_nev_i_list = LIST()  
  nPlots                    = 3 + KEYWORD_SET(comb_accelerated)
  FOR i=0,nPlots-1 DO BEGIN
     tmpDataName             = dataNames[i]

     dims                    = SIZE(newell_plotRange,/DIMENSIONS)
     CASE N_ELEMENTS(dims) OF 
        0:   plotRange       = !NULL
        1: BEGIN
           CASE dims OF
              0: plotRange   = !NULL
              2: plotRange   = Newell_plotRange
              ELSE: BEGIN
              END
           ENDCASE
        END
        2:   plotRange       = Newell_plotRange[*,i]
     ENDCASE

     GET_H2D_NEWELL_AND_MASK,eSpec, $ ;eSpec_i, $
                             TITLE=titles[i], $
                             IN_MLTS=mlt_list[i], $
                             IN_ILATS=ilat_list[i], $
                             ALFDB_PLOT_STRUCT=alfDB_plot_struct, $
                             IMF_STRUCT=IMF_struct, $
                             MIMC_STRUCT=MIMC_struct, $
                             NEWELL_PLOTRANGE=plotRange, $
                             LOG_NEWELLPLOT=log_newellPlot, $
                             NEWELLPLOT_AUTOSCALE=newellPlot_autoscale, $
                             NEWELLPLOT_NORMALIZE=newellPlot_normalize, $
                             NEWELLPLOT_PROBOCCURRENCE=newellPlot_probOccurrence, $
                             TMPLT_H2DSTR=tmplt_h2dStr, $
                             H2DSTR=h2dStr, $
                             H2DFLUXN=h2dFluxN, $
                             NEWELL_NONZERO_NEV_I=newell_nonzero_nEv_i, $
                             DATANAME=tmpDataName, $
                             DATARAWPTR=dataRawPtr, $
                             CB_FORCE_OOBHIGH=cb_force_oobHigh, $
                             CB_FORCE_OOBLOW=cb_force_oobLow, $
                             PRINT_MANDM=print_mAndM, $
                             LUN=lun
     
     newell_nonzero_nev_i_list.add,newell_nonzero_nEv_i
     h2dStrs                            = [h2dStrs,h2dStr]
     dataNames[i]                       = tmpDataName
     dataRawPtrs                        = [dataRawPtrs,dataRawPtr]

  ENDFOR
  
  ;;Get denom, if probOccurrence
  IF KEYWORD_SET(newellPlot_probOccurrence) THEN BEGIN

     ;;Get the data
     GET_NEWELL_PROBOCCURRENCE,h2dStrs, $
                               ;; COMBINE_ACCELERATED=comb_accelerated, $
                               OUT_H2D_LIST=tmp_H2D_list
     FOR i=0,N_ELEMENTS(h2dStrs)-1 DO BEGIN
        h2dStrs[i].data                 = tmp_H2D_list[i]
     ENDFOR

     ;;Fix range if it's bogus
     IF KEYWORD_SET(newell_plotRange) AND N_ELEMENTS(newell_plotRange) EQ 2 THEN BEGIN
        IF newell_plotRange[0] GE 1 THEN BEGIN
           newell_plotRange[0]          = 0.
        ENDIF
        IF newell_plotRange[1] GE 1 THEN BEGIN
           newell_plotRange[1]          = 1.
        ENDIF
     ENDIF
  ENDIF

  IF KEYWORD_SET(t_probOccurrence) THEN BEGIN

     probOccH2DStrs   = !NULL
     probOccDataNames = !NULL
     FOR i=0,N_ELEMENTS(h2dStrs)-1 DO BEGIN
        
        tmpMask    = BYTE(h2dStrs[i].data)
        tmpMask[*] = 255
        tmpMask[newell_nonzero_nev_i_list[i]] = 0

        dims                  = SIZE(t_probOcc_plotRange,/DIMENSIONS)
        CASE N_ELEMENTS(dims) OF 
           0:   plotRange     = [0,1]
           1: BEGIN
              CASE dims OF
                 0: plotRange = !NULL
                 2: plotRange = t_probOcc_plotRange
                 ELSE: BEGIN
                 END
              ENDCASE
           END
           2:   plotRange     = t_probOcc_plotRange[*,i]
        ENDCASE

        GET_PROB_OCCURRENCE_PLOTDATA,eSpec,i_list[i],tHistDenominator, $
                                     /FOR_ESPEC_DBS, $
                                     ;; LOGPROBOCCURRENCE=(KEYWORD_SET(all_logPlots) OR KEYWORD_SET(logProbOccurrence)), $
                                     PROBOCCURRENCERANGE=plotRange, $
                                     ;; PROBOCCURRENCEAUTOSCALE=probOccurrenceAutoscale, $
                                     ;; DO_WIDTH_X=do_width_x, $
                                     MINM=MIMC_struct.minM, $
                                     MAXM=MIMC_struct.maxM, $
                                     BINM=MIMC_struct.binM, $
                                     SHIFTM=MIMC_struct.shiftM, $
                                     MINI=MIMC_struct.minI, $
                                     MAXI=MIMC_struct.maxI, $
                                     BINI=MIMC_struct.binI, $
                                     EQUAL_AREA_BINNING=alfDB_plot_struct.EA_binning, $
                                     DO_LSHELL=MIMC_struct.do_lshell, $
                                     MINL=MIMC_struct.minL, $
                                     MAXL=MIMC_struct.maxL, $
                                     BINL=MIMC_struct.binL, $
                                     OUTH2DBINSMLT=outH2DBinsMLT, $
                                     OUTH2DBINSILAT=outH2DBinsILAT, $
                                     OUTH2DBINSLSHELL=outH2DBinsLShell, $
                                     ;; H2D_NONZERO_NEV_I=hEv_nz_i, $
                                     H2DFLUXN=h2dFluxN, $
                                     H2DMASK=tmpMask, $
                                     OUT_H2DMASK=out_h2dMask, $
                                     OUT_H2DPROBOCC=H2DProbOcc, $
                                     H2DSTR=h2dStr, $
                                     TMPLT_H2DSTR=tmplt_h2dStr, $
                                     DATANAME=dataName, $
                                     DATARAWPTR=dataRawPtr
        
        probOccDataNames = [probOccDataNames,dataNames[i] + "_tProbOcc"]

        h2dStr.name      = probOccDataNames[-1]
        h2dStr.title     = h2dStrs[i].title + " (tProb. Occ.)"
        probOccH2DStrs   = [probOccH2DStrs,h2dStr] 

        ;;HEY, if newellPlot_probOccurrence isn't set, this will be printed below
        IF KEYWORD_SET(print_mandm) AND KEYWORD_SET(newellPlot_probOccurrence) THEN BEGIN
           ;; IF KEYWORD_SET(medianPlot) OR ~KEYWORD_SET(logAvgPlot) THEN BEGIN
           fmt                      = 'G10.4' 
           maxh2d                   = MAX(probOccH2DStrs[i].data[newell_nonzero_nEv_i_list[i]])
           minh2d                   = MIN(probOccH2DStrs[i].data[newell_nonzero_nEv_i_list[i]])
           ;; ENDIF ELSE BEGIN
           ;;    fmt                = 'F10.2'
           ;;    maxh2d             = ALOG10(MAX(probOccH2DStrs[i].data[newell_nonzero_nEv_i_list[i]]))
           ;;    minh2d             = ALOG10(MIN(probOccH2DStrs[i].data[newell_nonzero_nEv_i_list[i]]))
           ;; ENDELSE
           PRINTF,lun,probOccH2DStrs[i].title
           PRINTF,lun,FORMAT='("Max, min:",T20,' + fmt + ',T35,' + fmt + ')', $
                  maxh2d, $
                  minh2d
        ENDIF
      ENDFOR

     CASE 1 OF
        KEYWORD_SET(newellPlot_probOccurrence): BEGIN
           ;;Better combine       ; user wants BOTH

           dataNames = [dataNames,probOccDataNames]
           H2DStrs   = [H2DStrs,probOCCH2DStrs]
        END
        ELSE: BEGIN
           dataNames = probOccDataNames
           H2DStrs   = probOCCH2DStrs

        END
     ENDCASE

     ;;Fix range if it's bogus
     IF KEYWORD_SET(t_probOcc_plotRange) AND N_ELEMENTS(t_probOcc_plotRange) EQ 2 THEN BEGIN
        IF t_probOcc_plotRange[0] GE 1 THEN BEGIN
           t_probOcc_plotRange[0]          = 0.
        ENDIF
        IF t_probOcc_plotRange[1] GE 1 THEN BEGIN
           t_probOcc_plotRange[1]          = 1.
        ENDIF
     ENDIF
  ENDIF

  ;;Calculate stuff, set up lims, names, etc
  FOR i=0,nPlots-1 DO BEGIN
     IF KEYWORD_SET(newellPlot_probOccurrence) THEN BEGIN
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Now handle label stuff
        dataNames[i]                    = dataNames[i]     + "_probOcc"
        h2dStrs[i].title                = h2dStrs[i].title + " (Prob. Occ.)"
        h2dStrs[i].name                 = dataNames[i]

        pR_dimSize                      = SIZE(newell_plotRange,/DIMENSIONS)
        CASE NDIMEN(newell_plotRange) OF
           -1: BEGIN
              h2dStrs[i].lim            = FLOAT([0,1])
           END
           0: BEGIN
              h2dStrs[i].lim            = FLOAT([0,1])
           END
           1: BEGIN
              IF pR_dimSize[0] EQ 2 THEN BEGIN
                 h2dStrs[i].lim         = newell_plotRange
              ENDIF ELSE BEGIN
                 h2dStrs[i].lim         = FLOAT([0,1])
              ENDELSE
           END
           2: BEGIN
              IF (pR_dimSize[0] EQ 2) AND (pR_dimSize[1] GE 3) THEN BEGIN
                 h2dStrs[i].lim         = newell_plotRange[*,i]
              ENDIF ELSE BEGIN
                 h2dStrs[i].lim         = FLOAT([0,1])
              ENDELSE
           END
        ENDCASE

        IF KEYWORD_SET(log_newellPlot) THEN BEGIN
           dataNames[i]          = 'log_' + dataNames[i]
           h2dStrs[i].data[newell_nonzero_nev_i_list[i]]  = ALOG10(h2dStrs[i].data[newell_nonzero_nev_i_list[i]])
           h2dStrs[i].lim        = [(h2dStrs[i].lim[0] LT 1e-5) ? -5 : ALOG10(h2dStrs[i].lim[0]),ALOG10(h2dStrs[i].lim[1])] ;lower bound must be one
           h2dStrs[i].title      = 'Log ' + h2dStrs[i].title
           h2dStrs[i].is_logged  = 1
        ENDIF

        IF KEYWORD_SET(newellPlot_normalize) THEN BEGIN
           dataNames[i]         += '_normed'
           maxNEv                = MAX(h2dStrs[i].data[newell_nonzero_nEv_i_list[i]])
           h2dStrs[i].data       = h2dStrs[i].data/maxNEv
           h2dStrs[i].lim        = [0.0,1.0]
           h2dStrs[i].title     += STRING(FORMAT='(" (norm: ",G0.3,")")',maxNEv)
        ENDIF

        h2dStrs[i].name          = dataNames[i]

     ENDIF ELSE BEGIN

        IF KEYWORD_SET(log_newellPlot) THEN BEGIN
           dataNames[i]          = 'log_' + dataNames[i]
           h2dStrs[i].data[newell_nonzero_nEv_i_list[i]]  = ALOG10(h2dStrs[i].data[newell_nonzero_nEv_i_list[i]])
           h2dStrs[i].lim        = [(h2dStrs[i].lim[0] LT 1) ? 0 : ALOG10(h2dStrs[i].lim[0]),ALOG10(h2dStrs[i].lim[1])] ;lower bound must be one
           h2dStrs[i].title      = 'Log ' + h2dStrs[i].title
           h2dStrs[i].name       = dataName
           h2dStrs[i].is_logged  = 1
        ENDIF

        CASE 1 OF
           KEYWORD_SET(newellPlot_normalize): BEGIN ;Normalize?
              dataName          += '_normed'
              maxNEv             = MAX(h2dStrs[i].data[newell_nonzero_nEv_i_list[i]])
              h2dStrs[i].data    = h2dStrs[i].data/maxNEv
              h2dStrs[i].lim     = [0.0,1.0]
              h2dStrs[i].title  += STRING(FORMAT='(" (norm: ",G0.3,")")',maxNEv)
              h2dStrs[i].name    = dataName
           END
           KEYWORD_SET(newellPlot_autoscale): BEGIN ;Autoscale?
              PRINT,"Autoscaling newellPlot: " + dataNames[i] + "..."
              h2dStrs[i].lim     = [MIN(h2dStrs[i].data[newell_nonzero_nEv_i_list[i]]), $
                                    MAX(h2dStrs[i].data[newell_nonzero_nEv_i_list[i]])]
           END
           ELSE:
        ENDCASE
     ENDELSE

     IF KEYWORD_SET(print_mandm) THEN BEGIN
        ;; IF KEYWORD_SET(medianPlot) OR ~KEYWORD_SET(logAvgPlot) THEN BEGIN
        fmt                      = 'G10.4' 
        maxh2d                   = MAX(h2dStrs[i].data[newell_nonzero_nEv_i_list[i]])
        minh2d                   = MIN(h2dStrs[i].data[newell_nonzero_nEv_i_list[i]])
        ;; ENDIF ELSE BEGIN
        ;;    fmt                = 'F10.2'
        ;;    maxh2d             = ALOG10(MAX(h2dStrs[i].data[newell_nonzero_nEv_i_list[i]]))
        ;;    minh2d             = ALOG10(MIN(h2dStrs[i].data[newell_nonzero_nEv_i_list[i]]))
        ;; ENDELSE
        PRINTF,lun,h2dStrs[i].title
        PRINTF,lun,FORMAT='("Max, min:",T20,' + fmt + ',T35,' + fmt + ')', $
               maxh2d, $
               minh2d
     ENDIF


  ENDFOR

END
