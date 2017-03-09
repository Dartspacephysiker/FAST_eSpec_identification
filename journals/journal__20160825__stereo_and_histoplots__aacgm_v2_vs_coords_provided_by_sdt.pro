;2016/08/25 Now I've wised up, and the 20160823 journal has been modded
PRO JOURNAL__20160825__STEREO_AND_HISTOPLOTS__AACGM_V2_VS_COORDS_PROVIDED_BY_SDT, $
   IN_ESPEC=eSpec, $
   ORBIT=orbit, $
   T1=t1, $
   T2=t2, $
   HEMI=hemi, $
   SAVEPLOT=savePlot, $
   STEREO_PLOTS=stereo_plots, $
   HISTO_PLOTS=histo_plots, $
   HISTO_ONEORB=histo_oneOrb, $
   HISTO__SEPARATE_HEMIS=histo__separate_hemis, $
   RESTRICT_HISTO_ILAT_RANGE=restrict_histo_ILAT_range, $
   BATCH=batch

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Plot options
  savePlot      = N_ELEMENTS(savePlot) GT 0 ? savePlot          : 0

  stereo_plots  = N_ELEMENTS(stereo_plots) GT 0 ? stereo_plots  : 0 ;do stereographics?

  histo_plots   = N_ELEMENTS(histo_plots) GT 0 ? histo_plots    : 0 ;do histograms of differences?
  histo_oneOrb  = N_ELEMENTS(histo_oneOrb) GT 0 ? histo_oneOrb  : 0 ;Only for specified orbit, or all together?

  binMLT        = 0.25
  binLAT        = 0.2

  ;;DB to check out if no other available
  dbDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  dbFile        = 'eSpec_20160607_db--orbs_500-16361--BELOW_2000km--with_alternate_coords.sav'

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
        'NORTH':
        'SOUTH':
        ELSE: BEGIN
           PRINT,'Bogus hemisphere provided: ' + hemi
           PRINT,'Out ...'
           RETURN
        END
     ENDCASE
  ENDELSE

  IF KEYWORD_SET(savePlot) THEN SET_PLOT_DIR,plotDir,/FOR_ESPEC_DB,/ADD_TODAY
  fileExt       = '.png'

  ;;Load 'er up
  IF N_ELEMENTS(eSpec) EQ 0 THEN BEGIN
     PRINT,"Master file ..."
     RESTORE,dbDir+dbFile
  ENDIF

  ;;Get stuff for this orbit
  CASE 1 OF
     (KEYWORD_SET(t1) AND KEYWORD_SET(t2)): BEGIN
        IF t2 LE t1 THEN BEGIN
           PRINT,'Um, t2 ≤ t1. Did you know that?'
           RETURN
        ENDIF

        orbInds       = WHERE((eSpec.x GE t1) AND (eSpec.x LE t2),nInds)
        
        ;;Are we orderly?
        CHECK_SORTED,eSpec.x[orbInds],is_sorted,SORTED_I=sort_ii
        IF ~is_sorted THEN BEGIN
           PRINT,"Look out! eSpec.x[orbInds] isn't sorted!"
           STOP
        ENDIF

        orbs          = eSpec.orbit[orbInds[UNIQ(eSpec.orbit[orbInds])]]
        nOrbs         = N_ELEMENTS(orbs)

        CASE nOrbs OF
           1: BEGIN
              orbPStr = 'orb ' + STRCOMPRESS(orbs,/REMOVE_ALL)
              orbNum  = TEMPORARY(orbs)
           END
           ELSE: BEGIN
              orbPStr = 'orbs '
              FOR k=0,nOrbs-1 DO BEGIN
                 orbPStr += STRING(FORMAT='(I0)',orbs[k]) + $
                            (k EQ (nOrbs-1) ? '' : ', ')
              ENDFOR
              orbNum  = orbs[0]
           END  
        ENDCASE

        PRINT,STRCOMPRESS(nInds,/REMOVE_ALL) + ' available for '
     END
     ELSE: BEGIN
        orbNum        = KEYWORD_SET(orbit) ? orbit : 1000
        orbInds       = WHERE(eSpec.orbit EQ orbNum)
     END
  ENDCASE

  IF orbInds[0] EQ -1 THEN BEGIN
     PRINT,'No indices below 2000 km—well, 1990 km—available for this orbit ...'
     PRINT,"Sorry. I guess I'd better go."
     RETURN
  ENDIF ELSE BEGIN
     orbString  = STRCOMPRESS(orbNum,/REMOVE_ALL)
  ENDELSE

  FOR iHemi=0,N_ELEMENTS(hemiArr)-1 DO BEGIN

     hemis  = hemiArr[iHemi]
     CASE STRUPCASE(hemis) OF
        'NORTH': BEGIN
           hemi_ii = WHERE(eSpec.coords.SDT.ILAT[orbInds] GT 0,nHere)
           niceHemiString = 'Northern Hemisphere'
        END
        'SOUTH': BEGIN
           hemi_ii = WHERE(eSpec.coords.SDT.ILAT[orbInds] LT 0,nHere)
           niceHemiString = 'Southern Hemisphere'
        END
     ENDCASE
        
     IF nHere EQ 0 THEN BEGIN
        PRINT,'No ' + hemis + 'inds available! Skipping ...' 
        CONTINUE
     ENDIF

     hemi_i        = orbInds[TEMPORARY(hemi_ii)]
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
                 hInds   = hemi_i
                 hPSuff  = orbString + hemis+ fileExt
                 hPTSuff = '!C(Orbit ' + orbString + ', ' + niceHemiString + ')'
              ENDIF ELSE BEGIN
                 hInds   = orbInds
                 hPSuff  = orbString + fileExt
                 hPTSuff = '!C(Orbit ' + orbString + ')'
              ENDELSE
           END
           ELSE: BEGIN
              IF histo__separate_hemis THEN BEGIN
                 CASE hemis OF
                    'NORTH': BEGIN
                       hInds = WHERE(eSpec.coords.SDT.ILAT GT 0,nHere)
                    END
                    'SOUTH': BEGIN
                       hInds = WHERE(eSpec.coords.SDT.ILAT LT 0,nHere)
                    END
                 ENDCASE
                 hPSuff  = orbString + hemis+ fileExt
                 hPTSuff = '!C(Orbit ' + orbString + ', ' + niceHemiString + ')'
              ENDIF ELSE BEGIN
                 hInds   = LINDGEN(N_ELEMENTS(eSpec.x))
                 hPSuff  = 'all_orbs' + fileExt
                 hPTSuff = '!C(All orbits)'
              ENDELSE
           ENDELSE
        ENDCASE

        diffMLT  = eSpec.coords.AACGM.MLT[hInds] - eSpec.coords.SDT.MLT[hInds]
        diffAlt  = eSpec.coords.AACGM.alt[hInds] - eSpec.coords.SDT.alt[hInds]
        diffLat  = eSpec.coords.AACGM.lat[hInds] - eSpec.coords.SDT.ILAT[hInds]

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

        ;; eSpec.coords.SDT   has ALT,MLT,ILAT
        ;; eSpec.coords.GEO   has ALT,LON,LAT
        ;; eSpec.coords.MAG   has ALT,LON,LAT
        ;; eSpec.coords.AACGM has ALT,MLT,LAT

        coordName            = 'MLT-ILAT (SDT-provided)'
        ;; plotTitle            = KEYWORD_SET(add_plotTitles) ? $
        ;;                        'Orbit '+orbString+' ('+ coordName+' coordinates)' : !NULL
        plotName             = coordName
        color_list           = 'black'
        SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec.coords.SDT.MLT[orbInds]*15., $
                                         eSpec.coords.SDT.ILAT[orbInds], $
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
                                         HUGEPLOTMODE=hugePlotMode, $
                                         STRANS=sTrans, $
                                         ADD_LINE=add_line, $
                                         LINESTYLE=lineStyle, $
                                         NO_SYMBOL=no_symbol, $
                                         OUT_ORBSTRARR_LIST=out_orbStrArr_list, $
                                         OUT_WINDOW=out_window, $
                                         OUT_MAP=out_map, $
                                         _EXTRA=e
        ;;Regulier
        ;; coordName            = 'GEI (SDT-provided)'
        ;; plotTitle            = KEYWORD_SET(add_plotTitles) ? $
        ;;                        'Orbit '+orbString+' ('+ coordName+' coordinates)' : !NULL
        ;; plotName             = coordName
        ;; color_list           = 'green'
        ;; SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec_GEI.lon,eSpec_GEI.lat, $
        ;;                                  HEMI=hemis, $
        ;;                                  PLOTNAME=plotName, $
        ;;                                  ;; OVERLAYAURZONE=overlayAurZone, $
        ;;                                  COLOR_LIST=color_list, $
        ;;                                  CENTERLON=centerLon, $
        ;;                                  /OVERPLOT, $
        ;;                                  LAYOUT=layout, $
        ;;                                  PLOTPOSITION=plotPosition, $
        ;;                                  OUTPLOTARR=outPlotArr, $
        ;;                                  CURRENT_WINDOW=window, $
        ;;                                  IN_MAP=map, $
        ;;                                  HUGEPLOTMODE=hugePlotMode, $
        ;;                                  STRANS=sTrans, $
        ;;                                  ;; PLOTTITLE=plotTitle, $
        ;;                                  ADD_LINE=add_line, $
        ;;                                  LINESTYLE=lineStyle, $
        ;;                                  NO_SYMBOL=no_symbol, $
        ;;                                  OUT_ORBSTRARR_LIST=out_orbStrArr_list, $
        ;;                                  OUT_WINDOW=out_window, $
        ;;                                  OUT_MAP=out_map, $
        ;;                                  _EXTRA=e
        ;; STOP

        ;;GEO
        coordName            = 'GEO'
        plotTitle            = KEYWORD_SET(add_plotTitles) ? $
                               'Orbit '+orbString+' ('+ coordName+' coordinates)' : !NULL
        plotName             = coordName
        color_list           = 'red'
        SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec.coords.GEO.lon[orbInds], $
                                         eSpec.coords.GEO.lat[orbInds], $
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
                                         HUGEPLOTMODE=hugePlotMode, $
                                         STRANS=sTrans, $
                                         ADD_LINE=add_line, $
                                         LINESTYLE=lineStyle, $
                                         NO_SYMBOL=no_symbol, $
                                         OUT_ORBSTRARR_LIST=out_orbStrArr_list, $
                                         OUT_WINDOW=out_window, $
                                         ;; /ADD_LEGEND, $
                                         OUT_MAP=out_map, $
                                         _EXTRA=e

        ;;MAG
        coordName            = 'MAG'
        plotTitle            = KEYWORD_SET(add_plotTitles) ? $
                               'Orbit '+orbString+' ('+ coordName+' coordinates)' : !NULL
        plotName             = coordName
        color_list           = 'blue'
        SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec.coords.MAG.lon[orbInds], $
                                         eSpec.coords.MAG.lat[orbInds], $
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
                                         HUGEPLOTMODE=hugePlotMode, $
                                         STRANS=sTrans, $
                                         ADD_LINE=add_line, $
                                         LINESTYLE=lineStyle, $
                                         NO_SYMBOL=no_symbol, $
                                         OUT_ORBSTRARR_LIST=out_orbStrArr_list, $
                                         OUT_WINDOW=out_window, $
                                         ADD_LEGEND=add_legend, $
                                         OUT_MAP=out_map, $
                                         _EXTRA=e

        ;;MAG
        coordName            = 'AACGM_v2'
        ;; plotTitle            = KEYWORD_SET(add_plotTitles) ? $
        ;;                        'Orbit '+orbString+' ('+ coordName+' coordinates)' : !NULL
        plotTitle            = 'Orbit '+ orbString + ', ' + niceHemiString
        sPName               = 'FAST_ephem--below_2000km--' + hemis + '--orb_' + orbString + $
                               '--' + 'GEO-MAG-AACGM_v2' + fileExt
        plotName             = coordName
        color_list           = 'orange'
        SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec.coords.AACGM.mlt[orbInds]*15., $
                                         eSpec.coords.AACGM.lat[orbInds], $
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
                                         SAVEPLOT=savePlot, $
                                         SPNAME=sPName, $
                                         PLOTDIR=plotDir, $
                                         CLOSE_AFTER_SAVE=close_after_save, $
                                         HUGEPLOTMODE=hugePlotMode, $
                                         STRANS=sTrans, $
                                         PLOTTITLE=plotTitle, $
                                         ADD_LINE=add_line, $
                                         LINESTYLE=lineStyle, $
                                         NO_SYMBOL=no_symbol, $
                                         OUT_ORBSTRARR_LIST=out_orbStrArr_list, $
                                         OUT_WINDOW=out_window, $
                                         /ADD_LEGEND, $
                                         OUT_MAP=out_map, $
                                         _EXTRA=e

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

  ENDFOR

 END