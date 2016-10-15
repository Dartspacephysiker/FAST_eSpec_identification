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
  COMMON NEWELL,NEWELL__eSpec, $
     NEWELL__HAVE_GOOD_I, $
     NEWELL__failCode, $
     NEWELL__good_i, $
     NEWELL__charERange, $
     ;; NEWELL__cleaned_i, $
     NEWELL__dbFile, $
     NEWELL__dbDir, $
     NEWELL__RECALCULATE
  
  ;; LOAD_MAXIMUS_AND_CDBTIME,!NULL,!NULL,DO_DESPUNDB=despun,/CHECK_DB,/QUIET

  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,espec ;,/DONT_LOAD_IN_MEMORY
  ENDIF ELSE BEGIN
     ;; eSpec                              = TEMPORARY(NEWELL__eSpec)
     eSpec                              = NEWELL__eSpec
  ENDELSE

  ;; plot_i__good_espec                 = CGSETINTERSECTION(plot_i,good_alf_i,INDICES_B=temp_eSpec_indices)


  tmp_eSpec                             = { x:eSpec.x[eSpec_i], $
                                            orbit:eSpec.orbit[eSpec_i], $
                                            MLT:eSpec.mlt[eSpec_i], $
                                            ILAT:eSpec.ilat[eSpec_i], $
                                            ALT:eSpec.alt[eSpec_i], $
                                            mono:eSpec.mono[eSpec_i], $
                                            broad:eSpec.broad[eSpec_i], $
                                            diffuse:eSpec.diffuse[eSpec_i], $
                                            Je:eSpec.Je[eSpec_i], $
                                            Jee:eSpec.Jee[eSpec_i], $
                                            nBad_eSpec:eSpec.nBad_eSpec[eSpec_i]}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Indices
  mono_i                                = WHERE((tmp_eSpec.mono   EQ 1) OR (tmp_eSpec.mono  EQ 2),nMono)
  broad_i                               = WHERE((tmp_eSpec.broad  EQ 1) OR (tmp_eSpec.broad EQ 2),nBroad)
  diffuse_i                             = WHERE(tmp_eSpec.diffuse EQ 1,nDiffuse)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;MLTs
  mlt_mono                              = tmp_eSpec.mlt[mono_i]-shiftM
  mlt_broad                             = tmp_eSpec.mlt[broad_i]-shiftM
  mlt_diffuse                           = tmp_eSpec.mlt[diffuse_i]-shiftM
  mlt_mono[WHERE(mlt_mono LT 0)]        = mlt_mono[WHERE(mlt_mono LT 0)] + 24
  mlt_broad[WHERE(mlt_broad LT 0)]      = mlt_broad[WHERE(mlt_broad LT 0)] + 24
  mlt_diffuse[WHERE(mlt_diffuse LT 0)]  = mlt_diffuse[WHERE(mlt_diffuse LT 0)] + 24

  mlt_list                              = LIST(TEMPORARY(mlt_mono),TEMPORARY(mlt_broad),TEMPORARY(mlt_diffuse))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;ILATs
  ilat_mono                             = tmp_eSpec.ilat[mono_i]
  ilat_broad                            = tmp_eSpec.ilat[broad_i]
  ilat_diffuse                          = tmp_eSpec.ilat[diffuse_i]
  ilat_list                             = LIST(TEMPORARY(ilat_mono),TEMPORARY(ilat_broad),TEMPORARY(ilat_diffuse))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Bonus
  titles                                = ['Monoenergetic','Broadband'   ,'Diffuse'       ]
  dataNames                             = ['mono_nonAlf'  ,'broad_nonAlf','diffuse_nonAlf']

  h2dStrs                               = !NULL
  dataRawPtrs                           = !NULL
  newell_nonzero_nev_i_list             = LIST()  
  nPlots                                = 3
  FOR i=0,nPlots-1 DO BEGIN
     tmpDataName                        = dataNames[i]
     GET_H2D_NEWELL_AND_MASK,tmp_eSpec, $ ;eSpec_i, $
                             TITLE=titles[i], $
                             IN_MLTS=mlt_list[i], $
                             IN_ILATS=ilat_list[i], $
                             MINM=minM,MAXM=maxM, $
                             BINM=binM, $
                             SHIFTM=shiftM, $
                             MINI=minI,MAXI=maxI,BINI=binI, $
                             EQUAL_AREA_BINNING=EA_binning, $
                             DO_LSHELL=do_lShell, MINL=minL,MAXL=maxL,BINL=binL, $
                             NEWELL_PLOTRANGE=newell_plotRange, $
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
        ;; h2dStrs[i].lim                  = KEYWORD_SET(newell_plotRange) AND N_ELEMENTS(newell_plotRange) EQ 2 ? $
        ;;                                   DOUBLE(newell_plotRange) : $
        ;;                                   DOUBLE([0,1]) 

        IF KEYWORD_SET(log_newellPlot) THEN BEGIN
           dataNames[i]          = 'log_' + dataNames[i]
           h2dStrs[i].data[newell_nonzero_nev_i_list[i]]  = ALOG10(h2dStrs[i].data[newell_nonzero_nev_i_list[i]])
           h2dStrs[i].lim        = [(h2dStrs[i].lim[0] LT 1e-5) ? -5 : ALOG10(h2dStrs[i].lim[0]),ALOG10(h2dStrs[i].lim[1])] ;lower bound must be one
           h2dStrs[i].title      = 'Log ' + h2dStrs[i].title
           h2dStrs[i].name       = dataNames[i]
           h2dStrs[i].is_logged  = 1
        ENDIF

        IF KEYWORD_SET(newellPlot_normalize) THEN BEGIN
           dataNames[i]         += '_normed'
           maxNEv                = MAX(h2dStrs[i].data[newell_nonzero_nEv_i_list[i]])
           h2dStrs[i].data       = h2dStrs[i].data/maxNEv
           h2dStrs[i].lim        = [0.0,1.0]
           h2dStrs[i].title     += STRING(FORMAT='(" (norm: ",G0.3,")")',maxNEv)
           h2dStrs[i].name       = dataNames[i]
        ENDIF

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


  eSpec = !NULL

END