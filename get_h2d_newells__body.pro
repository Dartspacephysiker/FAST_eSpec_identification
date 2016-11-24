;;11/24/16
PRO GET_H2D_NEWELLS__BODY,eSpec, $
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

  COMPILE_OPT IDL2

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Indices
  mono_i       = WHERE((eSpec.mono   EQ 1) OR (eSpec.mono  EQ 2),nMono)
  broad_i      = WHERE((eSpec.broad  EQ 1) OR (eSpec.broad EQ 2),nBroad)
  diffuse_i    = WHERE(eSpec.diffuse EQ 1,nDiffuse)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;MLTs
  mlt_mono     = eSpec.mlt[mono_i]-shiftM
  mlt_broad    = eSpec.mlt[broad_i]-shiftM
  mlt_diffuse  = eSpec.mlt[diffuse_i]-shiftM

  ;;Screen 'em for negs
  mNegMLT = WHERE(mlt_mono LT 0)
  bNegMLT = WHERE(mlt_broad LT 0)
  dNegMLT = WHERE(mlt_diffuse LT 0)
  IF mNegMLT[0] NE -1 THEN BEGIN
     mlt_mono[mNegMLT]    = mlt_mono[mNegMLT] + 24
  ENDIF
  IF bNegMLT[0] NE -1 THEN BEGIN
     mlt_broad[bNegMLT]   = mlt_broad[bNegMLT] + 24
  ENDIF
  IF dNegMLT[0] NE -1 THEN BEGIN
     mlt_diffuse[dNegMLT] = mlt_diffuse[dNegMLT] + 24
  ENDIF
  mlt_list      = LIST(TEMPORARY(mlt_mono),TEMPORARY(mlt_broad),TEMPORARY(mlt_diffuse))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;ILATs
  ilat_mono     = eSpec.ilat[mono_i]
  ilat_broad    = eSpec.ilat[broad_i]
  ilat_diffuse  = eSpec.ilat[diffuse_i]
  ilat_list     = LIST(TEMPORARY(ilat_mono),TEMPORARY(ilat_broad),TEMPORARY(ilat_diffuse))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Bonus
  titles                    = ['Monoenergetic','Broadband','Diffuse']
  dataNames                 = ['mono'         ,'broad'    ,'diffuse']

  IF KEYWORD_SET(nonAlf) THEN BEGIN
     dataNames += '_nonAlf'
  ENDIF

  IF KEYWORD_SET(eSpec.info.Newell2009interp) THEN BEGIN
     dataNames             += '__2009_interp'
  ENDIF

  h2dStrs                   = !NULL
  dataRawPtrs               = !NULL
  newell_nonzero_nev_i_list = LIST()  
  nPlots                    = 3
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
                             MINM=minM,MAXM=maxM, $
                             BINM=binM, $
                             SHIFTM=shiftM, $
                             MINI=minI,MAXI=maxI,BINI=binI, $
                             EQUAL_AREA_BINNING=EA_binning, $
                             DO_LSHELL=do_lShell, MINL=minL,MAXL=maxL,BINL=binL, $
                             NEWELL_PLOTRANGE=plotRange, $
                             LOG_NEWELLPLOT=log_newellPlot, $
                             NEWELLPLOT_AUTOSCALE=newellPlot_autoscale, $
                             NEWELLPLOT_NORMALIZE=newellPlot_normalize, $
                             NEWELLPLOT_PROBOCCURRENCE=newellPlot_probOccurrence, $
                             TMPLT_H2DSTR=tmplt_h2dStr, $
                             H2DSTR=h2dStr, $
                             ;; H2DMASKSTR=h2dMaskStr, $
                             H2DFLUXN=h2dFluxN, $
                             NEWELL_NONZERO_NEV_I=newell_nonzero_nEv_i, $
                             ;; MASKMIN=maskMin, $
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
              IF (pR_dimSize[0] EQ 2) AND (pR_dimSize[1] EQ 3) THEN BEGIN
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
