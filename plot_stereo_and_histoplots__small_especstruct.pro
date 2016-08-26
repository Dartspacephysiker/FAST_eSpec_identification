;2016/08/26 Just plot them, k?
PRO PLOT_STEREO_AND_HISTOPLOTS__SMALL_ESPECSTRUCT, $
   IN_ESPEC=eSOrb, $
   ORBIT=orbit, $
   T1=t1, $
   T2=t2, $
   HEMI=hemi, $
   STEREO_PLOTS=stereo_plots, $
   HISTO_PLOTS=histo_plots, $
   HISTO_ONEORB=histo_oneOrb, $
   HISTO__SEPARATE_HEMIS=histo__separate_hemis, $
   STEREO__EXCLUDE_SDT=stereo__exclude_SDT, $
   STEREO__EXCLUDE_GEI=stereo__exclude_GEI, $
   STEREO__EXCLUDE_GEO=stereo__exclude_GEO, $
   STEREO__EXCLUDE_MAG=stereo__exclude_MAG, $
   STEREO__EXCLUDE_AACGM=stereo__exclude_AACGM, $
   ADD_PLOTTITLE=add_plotTitle, $
   RESTRICT_HISTO_ILAT_RANGE=restrict_histo_ILAT_range, $
   SAVEPLOT=savePlot, $
   PLOTDIR=plotDir, $
   BATCH=batch

  COMPILE_OPT idl2

  ;;Plot options
  savePlot      = N_ELEMENTS(savePlot) GT 0 ? savePlot          : 0

  stereo_plots  = N_ELEMENTS(stereo_plots) GT 0 ? stereo_plots  : 0 ;do stereographics?

  histo_plots   = N_ELEMENTS(histo_plots) GT 0 ? histo_plots    : 0 ;do histograms of differences?
  histo_oneOrb  = N_ELEMENTS(histo_oneOrb) GT 0 ? histo_oneOrb  : 0 ;Only for specified orbit, or all together?

  binMLT        = 0.25
  binLAT        = 0.2

  ;;DB to check out if no other available
  ;; dbDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  ;; dbFile        = 'eSpec_20160607_db--orbs_500-16361--BELOW_2000km--with_alternate_coords.sav'

  IF KEYWORD_SET(batch) THEN savePlot = 1

  IF N_ELEMENTS(histo__separate_hemis) EQ 0 THEN BEGIN
     histo__separate_hemis = 0
  ENDIF

  ;;Doing orb stuff?
  IF N_ELEMENTS(hemi) EQ 0 THEN BEGIN
     hemiArr       = KEYWORD_SET(hemi) ? hemi : 'NORTH'
  ENDIF ELSE BEGIN
     CASE STRUPCASE(hemi) OF
        'BOTH': BEGIN
           hemiArr = ['NORTH','SOUTH']
        END
        'NORTH': hemiArr = hemi
        'SOUTH': hemiArr = hemi
        ELSE: BEGIN
           PRINT,'Bogus hemisphere provided: ' + hemi
           PRINT,'Out ...'
           RETURN
        END
     ENDCASE
  ENDELSE

  IF KEYWORD_SET(savePlot) AND ~KEYWORD_SET(plotDir) THEN BEGIN
     SET_PLOT_DIR,plotDir,/FOR_ESPEC_DB,/ADD_TODAY
  ENDIF

  fileExt       = '.png'

  
  nStereoPlots  = (~KEYWORD_SET(stereo__exclude_SDT)) + (~KEYWORD_SET(stereo__exclude_GEI)) + $
                    (~KEYWORD_SET(stereo__exclude_GEO)) + (~KEYWORD_SET(stereo__exclude_MAG)) + $
                    (~KEYWORD_SET(stereo__exclude_AACGM))

  ;;Set up coordinate string for stereo plots
  allCoordString = ''
  allCNiceString = ''
  tmpN           = nStereoPlots
  IF (~KEYWORD_SET(stereo__exclude_SDT))   THEN BEGIN
     tmpN--
     allCoordString += 'MLTILAT_'
     allCNiceString += ((allCNiceString EQ '') ? '!C(' : ', ' ) + $
                       'MLTILAT' + $
                       ((tmpN EQ 0) ? ')' : '')

  ENDIF
  IF (~KEYWORD_SET(stereo__exclude_GEI))   THEN BEGIN
     tmpN--
     allCoordString += 'GEI_'
     allCNiceString += ((allCNiceString EQ '') ? '!C(' : ', ' ) + $
                       'GEI' + $
                       ((tmpN EQ 0) ? ')' : '')
  ENDIF
  IF (~KEYWORD_SET(stereo__exclude_GEO))   THEN BEGIN
     tmpN--
     allCoordString += 'GEO_'
     allCNiceString += ((allCNiceString EQ '') ? '!C(' : ', ' ) + $
                       'GEO' + $
                       ((tmpN EQ 0) ? ')' : '')
  ENDIF
  IF (~KEYWORD_SET(stereo__exclude_MAG))   THEN BEGIN
     tmpN--
     allCoordString += 'MAG_'
     allCNiceString += ((allCNiceString EQ '') ? '!C(' : ', ' ) + $
                       'GEO' + $
                       ((tmpN EQ 0) ? ')' : '')
  ENDIF
  IF (~KEYWORD_SET(stereo__exclude_AACGM)) THEN BEGIN
     tmpN--
     allCoordString += 'AACGM_'
     allCNiceString += ((allCNiceString EQ '') ? '!C(' : ', ' ) + $
                       'AACGM' + $
                       ((tmpN EQ 0) ? ')' : '')
  ENDIF

  ;;Is it sane?
  IF N_ELEMENTS(eSOrb) EQ 0 THEN BEGIN
     PRINT,"Can't do nothin' with nothin'. I need eSOrb!"
     RETURN
  ENDIF ELSE BEGIN
     CHECK_SORTED,eSOrb.x,isSort
     IF ~isSort THEN BEGIN
        PRINT,"eSOrb times are unsorted! Do you know what you've got on your hands?"
        STOP
     ENDIF
  ENDELSE

  ;;Get stuff for this orbit

  ;;First, orb indices
  timeString = ''
  CASE 1 OF
     (KEYWORD_SET(t1) AND KEYWORD_SET(t2)): BEGIN
        IF t2 LE t1 THEN BEGIN
           PRINT,'Um, t2 ≤ t1. Did you know that?'
           RETURN
        ENDIF

        orbInds       = WHERE((eSOrb.x GE t1) AND (eSOrb.x LE t2),nInds)
        timeString    = TIME_TO_STR(t1) + '–' + TIME_TO_STR(t2)
     END
     KEYWORD_SET(orbit): BEGIN
        orbInds       = WHERE(eSOrb.orbit EQ orbit,nInds)
     END
     ELSE: BEGIN
        nInds         = N_ELEMENTS(eSOrb.x)
        orbInds       = LINDGEN(nInds)
     END
  ENDCASE

  IF orbInds[0] EQ -1 THEN BEGIN
     PRINT,"No indices for this orbit ... Sorry. I guess I'd better go."
     RETURN
  ENDIF

  orbs          = eSOrb.orbit[orbInds[UNIQ(eSOrb.orbit[orbInds])]]
  nOrbs         = N_ELEMENTS(orbs)

  CASE nOrbs OF
     1: BEGIN
        orbPStr = 'Orbit ' + STRCOMPRESS(orbs,/REMOVE_ALL)
        orbStr  = "orbit_" + STRCOMPRESS(orbs,/REMOVE_ALL)
     END
     ELSE: BEGIN
        CASE 1 OF
           (nOrbs LE 5): BEGIN
              orbPStr = 'Orbits '
              orbStr  = "orbit_"
              FOR k=0,nOrbs-1 DO BEGIN
                 orbPStr += STRING(FORMAT='(I0)',orbs[k]) + $
                            (k EQ (nOrbs-1) ? '' : ', ')
                 orbStr  += STRING(FORMAT='(I0)',orbs[k]) + $
                            (k EQ (nOrbs-1) ? '' : '_')
              ENDFOR              
           END
           ELSE: BEGIN
              orbPStr = STRCOMPRESS(nOrbs,/REMOVE_ALL) + ' Orbits'
              orbStr  = STRCOMPRESS(nOrbs,/REMOVE_ALL) + '_orbs'
           END
        ENDCASE
     END  
  ENDCASE

  orbPStr             = timeString NE '' ? timeString + ' (' + orbPStr + ')' : orbPstr
  PRINT,STRCOMPRESS(nInds,/REMOVE_ALL) + ' available for ' + orbPStr

  FOR iHemi=0,N_ELEMENTS(hemiArr)-1 DO BEGIN

     hemis  = hemiArr[iHemi]
     CASE STRUPCASE(hemis) OF
        'NORTH': BEGIN
           hemi_i         = WHERE(eSOrb.coords.SDT.ILAT GT 0,nHere)
           niceHemiString = 'Northern Hemisphere'
        END
        'SOUTH': BEGIN
           hemi_i         = WHERE(eSOrb.coords.SDT.ILAT LT 0,nHere)
           niceHemiString = 'Southern Hemisphere'
        END
     ENDCASE
     
     IF nHere EQ 0 THEN BEGIN
        PRINT,'No ' + hemis + 'inds available! Skipping ...' 
        CONTINUE
     ENDIF

     stereoPlot_i = CGSETINTERSECTION(hemi_i,orbInds,COUNT=nHere)

     IF nHere EQ 0 THEN BEGIN
        PRINT,'Eliminated all plot_i! Skipping ...'
        CONTINUE
     ENDIF

     ;;Histoplot stuff
     ;; dMLTPlotPref  = 'dMLT__AACGM_vs_SDT--'+hemi+'--'
     ;; dAltPlotPref  = 'dAlt__AACGM_vs_SDT--'+hemi+'--'
     ;; dLatPlotPref  = 'dLat__AACGM_vs_SDT--'+hemi+'--'
     dMLTPlotPref  = 'dMLT__AACGM_vs_SDT--'
     dAltPlotPref  = 'dAlt__AACGM_vs_SDT--'
     dLatPlotPref  = 'dLat__AACGM_vs_SDT--'

     IF KEYWORD_SET(histo_plots) THEN BEGIN

        ;; deltaChar     = CGGREEK('Delta')
        deltaChar     = "104B ;"
        deltaChar     = "!4" + STRING(deltaChar)+"!X"

        color         = 'blue'
        polyColor     = 'royal blue'

        CASE 1 OF
           KEYWORD_SET(histo_oneOrb): BEGIN
              IF histo__separate_hemis THEN BEGIN
                 hInds   = stereoPlot_i
                 hPSuff  = orbStr + hemis + fileExt
                 hPTSuff = '!C' + orbPStr + ', ' + niceHemiString
              ENDIF ELSE BEGIN
                 hInds   = orbInds
                 hPSuff  = orbStr + fileExt
                 hPTSuff = '!C' + orbPStr + niceHemiString
              ENDELSE
           END
           ELSE: BEGIN
              IF histo__separate_hemis THEN BEGIN
                 hInds   = stereoPlot_i
                 hPSuff  = orbStr + hemis + fileExt
                 hPTSuff = '!C' + orbPStr + ', ' + niceHemiString
              ENDIF ELSE BEGIN
                 hInds   = orbInds
                 hPSuff  = orbStr + fileExt
                 hPTSuff = '!C' + orbPStr
              ENDELSE
           ENDELSE
        ENDCASE

        diffMLT  = eSOrb.coords.AACGM.MLT[hInds] - eSOrb.coords.SDT.MLT[hInds]
        diffAlt  = eSOrb.coords.AACGM.alt[hInds] - eSOrb.coords.SDT.alt[hInds]
        diffLat  = eSOrb.coords.AACGM.lat[hInds] - eSOrb.coords.SDT.ILAT[hInds]

        CGHISTOPLOT,diffMLT, $
                    TITLE='Difference between MLT!DAACGM_v2!N and MLT!DSDT!N'+hpTSuff, $
                    XTITLE=deltaChar+'MLT', $
                    /FILLPOLYGON, $
                    BINSIZE=binMLT, $
                    DATACOLORNAME=color, $
                    POLYCOLOR=polyColor, $
                    OUTPUT=KEYWORD_SET(savePlot) ? plotDir+ $
                    dMLTPlotPref + hPSuff : !NULL

        
        IF ~KEYWORD_SET(savePlot) THEN BEGIN
           PRINT,"Stopping so you can take it in ..."
           STOP
        ENDIF

        CGHISTOPLOT,diffLat, $
                    TITLE='Difference between LAT!DAACGM_v2!N and ILAT!DSDT!N'+hpTSuff, $
                    XTITLE=deltaChar+'Latitude', $
                    MAXINPUT=KEYWORD_SET(restrict_histo_ILAT_range) ? 15  : !NULL, $
                    MININPUT=KEYWORD_SET(restrict_histo_ILAT_range) ? -15 : !NULL, $
                    /FILLPOLYGON, $
                    BINSIZE=binLAT, $
                    DATACOLORNAME=color, $
                    POLYCOLOR=polyColor, $
                    OUTPUT=KEYWORD_SET(savePlot) ? plotDir + $
                    dLatPlotPref + hPSuff : !NULL

        IF ~KEYWORD_SET(savePlot) THEN BEGIN
           PRINT,"... Now raise your hands above your head—come on, way up there!—good, you've got it! ..."
           STOP
        ENDIF

        CGHISTOPLOT,diffAlt, $
                    TITLE='Difference between ALT!DAACGM_v2!N and ALT!DSDT!N'+hpTSuff, $
                    XTITLE=deltaChar+'Altitude', $
                    /FILLPOLYGON, $
                    DATACOLORNAME=color, $
                    POLYCOLOR=polyColor, $
                    OUTPUT=KEYWORD_SET(savePlot) ? plotDir + $
                    dAltPlotPref + hPSuff : !NULL

        IF ~KEYWORD_SET(savePlot) THEN BEGIN
           PRINT,"... And breathe ..."
           STOP
        ENDIF
     ENDIF

     IF KEYWORD_SET(stereo_plots) THEN BEGIN
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Now plot 'em

        ;; eSOrb.coords.SDT   has ALT,MLT,ILAT
        ;; eSOrb.coords.GEO   has ALT,LON,LAT
        ;; eSOrb.coords.MAG   has ALT,LON,LAT
        ;; eSOrb.coords.AACGM has ALT,MLT,LAT

        plotTitle         = 'Orbit '+ orbStr + ', ' + niceHemiString + allCNiceString
        sPName            = 'FAST_ephem--' + hemis + '--orb_' + orbStr + $
                            '--' + allCoordString + fileExt

        saveThisPlot      = 0
        IF ISA(in_window) THEN BEGIN
           havePlotWindow = 1
           window         = havePlotWindow
        ENDIF

        plotsRemaining    = nStereoPlots
        
        IF ~KEYWORD_SET(stereo__exclude_SDT) THEN BEGIN

           plotsRemaining--
           IF KEYWORD_SET(savePlot)       AND (plotsRemaining EQ 0) THEN saveThisPlot  = 1
           IF KEYWORD_SET(add_plotTitle) AND (plotsRemaining EQ 0) THEN titleThisPlot = 1

           coordName            = 'MLT-ILAT (SDT-provided)'
           plotName             = coordName
           color_list           = 'black'
           SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSOrb.coords.SDT.MLT[stereoPlot_i]*15., $
                                            eSOrb.coords.SDT.ILAT[stereoPlot_i], $
                                            HEMI=hemis, $
                                            PLOTNAME=plotName, $
                                            ;; OVERLAYAURZONE=overlayAurZone, $
                                            COLOR_LIST=color_list, $
                                            CENTERLON=centerLon, $
                                            OVERPLOT=overplot, $
                                            LAYOUT=layout, $
                                            PLOTPOSITION=plotPosition, $
                                            OUTPLOTARR=outPlotArr, $
                                            CURRENT_WINDOW=window, $
                                            IN_MAP=map, $
                                            SAVEPLOT=saveThisPlot, $
                                            SPNAME=sPName, $
                                            PLOTDIR=plotDir, $
                                            CLOSE_AFTER_SAVE=close_after_save, $
                                            HUGEPLOTMODE=hugePlotMode, $
                                            STRANS=sTrans, $
                                            PLOTTITLE=KEYWORD_SET(titleThisPlot) ? plotTitle : !NULL, $
                                            ADD_LINE=add_line, $
                                            LINESTYLE=lineStyle, $
                                            NO_SYMBOL=no_symbol, $
                                            OUT_ORBSTRARR_LIST=out_orbStrArr_list, $
                                            OUT_WINDOW=out_window, $
                                            OUT_MAP=out_map, $
                                            _EXTRA=e

           IF ~havePlotWindow THEN havePlotWindow = 1

        ENDIF

        ;;GEI, Regulier SDT-provided
        IF ~KEYWORD_SET(stereo__exclude_GEI) THEN BEGIN

           plotsRemaining--
           IF KEYWORD_SET(savePlot      ) AND (plotsRemaining EQ 0) THEN saveThisPlot  = 1
           IF KEYWORD_SET(add_plotTitle) AND (plotsRemaining EQ 0) THEN titleThisPlot = 1

           coordName            = 'GEI (SDT-provided)'
           plotName             = coordName
           color_list           = 'green'
           SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSOrb_GEI.lon,eSOrb_GEI.lat, $
                                            HEMI=hemis, $
                                            PLOTNAME=plotName, $
                                            ;; OVERLAYAURZONE=overlayAurZone, $
                                            COLOR_LIST=color_list, $
                                            CENTERLON=centerLon, $
                                            OVERPLOT=havePlotWindow, $
                                            LAYOUT=layout, $
                                            PLOTPOSITION=plotPosition, $
                                            OUTPLOTARR=outPlotArr, $
                                            CURRENT_WINDOW=window, $
                                            IN_MAP=map, $
                                            SAVEPLOT=saveThisPlot, $
                                            SPNAME=sPName, $
                                            PLOTDIR=plotDir, $
                                            CLOSE_AFTER_SAVE=close_after_save, $
                                            HUGEPLOTMODE=hugePlotMode, $
                                            STRANS=sTrans, $
                                            PLOTTITLE=KEYWORD_SET(titleThisPlot) ? plotTitle : !NULL, $
                                            ADD_LINE=add_line, $
                                            LINESTYLE=lineStyle, $
                                            NO_SYMBOL=no_symbol, $
                                            OUT_ORBSTRARR_LIST=out_orbStrArr_list, $
                                            OUT_WINDOW=out_window, $
                                            OUT_MAP=out_map, $
                                            _EXTRA=e

           IF ~havePlotWindow THEN havePlotWindow = 1

        ENDIF

        ;;GEO
        IF ~KEYWORD_SET(stereo__exclude_GEO) THEN BEGIN

           plotsRemaining--
           IF KEYWORD_SET(savePlot      ) AND (plotsRemaining EQ 0) THEN saveThisPlot  = 1
           IF KEYWORD_SET(add_plotTitle) AND (plotsRemaining EQ 0) THEN titleThisPlot = 1

           coordName            = 'GEO'
           plotName             = coordName
           color_list           = 'red'
           SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSOrb.coords.GEO.lon[stereoPlot_i], $
                                            eSOrb.coords.GEO.lat[stereoPlot_i], $
                                            HEMI=hemis, $
                                            PLOTNAME=plotName, $
                                            COLOR_LIST=color_list, $
                                            CENTERLON=centerLon, $
                                            /OVERPLOT, $
                                            LAYOUT=layout, $
                                            PLOTPOSITION=plotPosition, $
                                            OUTPLOTARR=outPlotArr, $
                                            CURRENT_WINDOW=window, $
                                            IN_MAP=map, $
                                            SAVEPLOT=saveThisPlot, $
                                            SPNAME=sPName, $
                                            PLOTDIR=plotDir, $
                                            CLOSE_AFTER_SAVE=close_after_save, $
                                            HUGEPLOTMODE=hugePlotMode, $
                                            STRANS=sTrans, $
                                            PLOTTITLE=KEYWORD_SET(titleThisPlot) ? plotTitle : !NULL, $
                                            ADD_LINE=add_line, $
                                            LINESTYLE=lineStyle, $
                                            NO_SYMBOL=no_symbol, $
                                            OUT_ORBSTRARR_LIST=out_orbStrArr_list, $
                                            OUT_WINDOW=out_window, $
                                            ;; /ADD_LEGEND, $
                                            OUT_MAP=out_map, $
                                            _EXTRA=e

           IF ~havePlotWindow THEN havePlotWindow = 1

        ENDIF

        ;;MAG
        IF ~KEYWORD_SET(stereo__exclude_MAG) THEN BEGIN

           plotsRemaining--
           IF KEYWORD_SET(savePlot      ) AND (plotsRemaining EQ 0) THEN saveThisPlot  = 1
           IF KEYWORD_SET(add_plotTitle) AND (plotsRemaining EQ 0) THEN titleThisPlot = 1

           coordName            = 'MAG'
           plotName             = coordName
           color_list           = 'blue'
           SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSOrb.coords.MAG.lon[stereoPlot_i], $
                                            eSOrb.coords.MAG.lat[stereoPlot_i], $
                                            HEMI=hemis, $
                                            PLOTNAME=plotName, $
                                            COLOR_LIST=color_list, $
                                            ;; OVERLAYAURZONE=overlayAurZone, $
                                            CENTERLON=centerLon, $
                                            ;; OVERPLOT=overplot, $
                                            /OVERPLOT, $
                                            LAYOUT=layout, $
                                            PLOTPOSITION=plotPosition, $
                                            OUTPLOTARR=outPlotArr, $
                                            CURRENT_WINDOW=window, $
                                            IN_MAP=map, $
                                            SAVEPLOT=saveThisPlot, $
                                            SPNAME=sPName, $
                                            PLOTDIR=plotDir, $
                                            CLOSE_AFTER_SAVE=close_after_save, $
                                            HUGEPLOTMODE=hugePlotMode, $
                                            STRANS=sTrans, $
                                            PLOTTITLE=KEYWORD_SET(titleThisPlot) ? plotTitle : !NULL, $
                                            ADD_LINE=add_line, $
                                            LINESTYLE=lineStyle, $
                                            NO_SYMBOL=no_symbol, $
                                            OUT_ORBSTRARR_LIST=out_orbStrArr_list, $
                                            OUT_WINDOW=out_window, $
                                            ADD_LEGEND=add_legend, $
                                            OUT_MAP=out_map, $
                                            _EXTRA=e

           IF ~havePlotWindow THEN havePlotWindow = 1

        ENDIF

        ;;AACGM
        IF ~KEYWORD_SET(stereo__exclude_AACGM) THEN BEGIN

           plotsRemaining--
           IF KEYWORD_SET(savePlot      ) AND (plotsRemaining EQ 0) THEN saveThisPlot  = 1
           IF KEYWORD_SET(add_plotTitle) AND (plotsRemaining EQ 0) THEN titleThisPlot = 1

           coordName            = 'AACGM_v2'
           plotName             = coordName
           color_list           = 'orange'
           SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSOrb.coords.AACGM.mlt[stereoPlot_i]*15., $
                                            eSOrb.coords.AACGM.lat[stereoPlot_i], $
                                            HEMI=hemis, $
                                            PLOTNAME=plotName, $
                                            COLOR_LIST=color_list, $
                                            CENTERLON=centerLon, $
                                            /OVERPLOT, $
                                            LAYOUT=layout, $
                                            PLOTPOSITION=plotPosition, $
                                            OUTPLOTARR=outPlotArr, $
                                            CURRENT_WINDOW=window, $
                                            PLOTSUFF=plotSuff, $
                                            IN_MAP=map, $
                                            SAVEPLOT=saveThisPlot, $
                                            SPNAME=sPName, $
                                            PLOTDIR=plotDir, $
                                            CLOSE_AFTER_SAVE=close_after_save, $
                                            HUGEPLOTMODE=hugePlotMode, $
                                            STRANS=sTrans, $
                                            PLOTTITLE=KEYWORD_SET(titleThisPlot) ? plotTitle : !NULL, $
                                            ADD_LINE=add_line, $
                                            LINESTYLE=lineStyle, $
                                            NO_SYMBOL=no_symbol, $
                                            OUT_ORBSTRARR_LIST=out_orbStrArr_list, $
                                            OUT_WINDOW=out_window, $
                                            /ADD_LEGEND, $
                                            OUT_MAP=out_map, $
                                            _EXTRA=e


           IF ~havePlotWindow THEN havePlotWindow = 1

        ENDIF

        IF ~KEYWORD_SET(batch) THEN STOP

        outPlotArr          = !NULL

        IF ISA(out_map)    AND N_ELEMENTS(out_map)     EQ 1 THEN out_map.Close
        out_map             = !NULL

        IF ISA(out_window) AND N_ELEMENTS(out_window)  EQ 1 THEN out_window.Close
        out_window          = !NULL

        IF ISA(map)        AND N_ELEMENTS(map)         EQ 1 THEN map.Close
        map                 = !NULL

        IF ISA(window)     AND N_ELEMENTS(window)      EQ 1 THEN window.Close
        window              = !NULL

        out_orbStrArr_list  = !NULL

     ENDIF
  ENDFOR

 END