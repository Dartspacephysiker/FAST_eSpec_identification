;2016/08/25 Now I've wised up, and the 20160823 journal has been modded
PRO JOURNAL__20160825__HISTOPLOTS__AACGM_V2_VS_COORDS_PROVIDED_BY_SDT

  COMPILE_OPT idl2

  ;;Plot options
  savePlot      = 1

  stereo_plots  = 1 ;do stereographics?

  histo_plots   = 1 ;do histograms of differences?
  histo_oneOrb  = 1 ;Only for specified orbit, or all together?

  ;;Doing orb stuff?
  hemi          = 'NORTH'
  orbNum        = 1000

  ;;DB to check out
  dbDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  dbFile        = 'eSpec_20160607_db--orbs_500-16361--BELOW_2000km--with_alternate_coords.sav'

  ;;Histoplot stuff
  dMLTPlotPref  = 'dMLT__AACGM_vs_SDT--'+hemi+'--'
  dAltPlotPref  = 'dAlt__AACGM_vs_SDT--'+hemi+'--'
  dLatPlotPref  = 'dLat__AACGM_vs_SDT--'+hemi+'--'

  IF KEYWORD_SET(savePlot) THEN SET_PLOT_DIR,plotDir,/FOR_ESPEC_DB,/ADD_TODAY
  fileExt       = '.png'

  ;;Load 'er up
  PRINT,"Master file ..."
  RESTORE,dbDir+dbFile

  ;;Get stuff for this orbit
  orbInds       = WHERE(eSpec.orbit EQ orbNum)

  IF orbInds[0] EQ -1 THEN BEGIN
     PRINT,'No indices below 2000 km—well, 1990 km—available for this orbit ...'
     PRINT,"Sorry. I guess I'd better go."
     RETURN
  ENDIF ELSE BEGIN
     orbString  = STRCOMPRESS(orbNum,/REMOVE_ALL)
  ENDELSE

  IF KEYWORD_SET(histo_plots) THEN BEGIN

     ;; deltaChar     = CGGREEK('Delta')
     deltaChar     = "104B ;"
     deltaChar     = "!4" + STRING(deltaChar)+"!X"

     color         = 'blue'
     polyColor     = 'royal blue'

     CASE 1 OF
        KEYWORD_SET(histo_oneOrb): BEGIN
           hInds   = orbInds
           hPSuff  = orbString + fileExt
           hPTSuff = '!C(orbit ' + orbString + ')'
        END
        ELSE: BEGIN
           hInds  = LINDGEN(eSpec.x)
           hPSuff = 'all_orbs' + fileExt
           hPTSuff = '!C(all orbits )'
        ENDELSE
     ENDCASE
     diffMLT  = eSpec.coords.AACGM.MLT[hInds] - eSpec.coords.SDT.MLT[hInds]
     diffAlt  = eSpec.coords.AACGM.alt[hInds] - eSpec.coords.SDT.alt[hInds]
     diffLat  = eSpec.coords.AACGM.lat[hInds] - eSpec.coords.SDT.ILAT[hInds]

     CGHISTOPLOT,diffMLT, $
                 TITLE='Difference between MLT!DAACGM_v2!N and MLT!DSDT!N'+hpTSuff, $
                 XTITLE=deltaChar+'MLT', $
                 /FILLPOLYGON, $
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
                 /FILLPOLYGON, $
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
     sPName               = 'FAST_coords--below_2000km--'+hemi+'orb_' + orbString + fileExt
     plotName             = coordName
     color_list           = 'black'
     SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec.coords.SDT.MLT[orbInds]*15., $
                                      eSpec.coords.SDT.ILAT[orbInds], $
                                      HEMI=hemi, $
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
     ;; sPName               = 'FAST_ephem--orbit_' + orbString + '--' + coordName + '.png'
     ;; plotName             = coordName
     ;; color_list           = 'green'
     ;; SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec_GEI.lon,eSpec_GEI.lat, $
     ;;                                  HEMI=hemi, $
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
     sPName               = 'FAST_ephem--orbit_' + orbString + '--' + coordName + '.png'
     plotName             = coordName
     color_list           = 'red'
     SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec.coords.GEO.lon[orbInds], $
                                      eSpec.coords.GEO.lat[orbInds], $
                                      HEMI=hemi, $
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
     sPName               = 'FAST_ephem--orbit_' + orbString + '--' + 'MLT-ILAT_GEI_GEO_MAG' + '.png'
     plotName             = coordName
     color_list           = 'blue'
     SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec.coords.MAG.lon[orbInds], $
                                      eSpec.coords.MAG.lat[orbInds], $
                                      HEMI=hemi, $
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
     plotTitle            = 'Orbit '+ orbString + ', Northern Hemisphere'
     sPName               = 'FAST_ephem--orbit_' + orbString + '--' + 'GEI_GEO_MAG_AACGM_v2' + '.png'
     plotName             = coordName
     color_list           = 'orange'
     SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec.coords.AACGM.mlt[orbInds]*15., $
                                      eSpec.coords.AACGM.lat[orbInds], $
                                      HEMI=hemi, $
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
  STOP

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


 END