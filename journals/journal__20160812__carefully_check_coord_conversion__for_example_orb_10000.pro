;2016/08/12 Trying to get a sense of what we've got here.
;;Generating info commented out at bottom

PRO JOURNAL__20160812__CAREFULLY_CHECK_COORD_CONVERSION__FOR_EXAMPLE_ORB_10000

  COMPILE_OPT idl2

  SET_PLOT_DIR,plotDir,/FOR_ESPEC_DB,/ADD_TODAY
  savePlot             = 1
  add_plotTitles       = 0

  R_E                  = 6371.2D ;Earth radius in km, from IGRFLIB_V2.pro

  orbNum               = 10000
  orbString            = STRCOMPRESS(orbNum,/REMOVE_ALL)

  ;;File with eSTest,eSpecEphem, eSTest,eSpecEphem,eSpecCoords,eSpec_GEO,eSpec_MAG,orbInds (indices into eSpec DB--the SORTED version with 28604345 entries
  littleBabyFile       = '/SPENCEdata/Research/Satellites/FAST/espec_identification/saves_output_etc/eSpecTest_orb' + orbString + '.sav'
  RESTORE,littleBabyFile

  ;;Now plot 'em
  hemi                 = 'NORTH'

  ;; eSpec_GEO     = {ALT:eEphem_GEOSph_arr[*,2], $
  ;;                  LON:eEphem_GEOSph_arr[*,1], $
  ;;                  LAT:eEphem_GEOSph_arr[*,0]}
  
  ;; eSpec_MAG     = {ALT:eEphem_MAGSph_arr[*,2], $
  ;;                  LON:eEphem_MAGSph_arr[*,1], $
  ;;                  LAT:eEphem_MAGSph_arr[*,0]}
  
  ;; eSpec_GEI     = {ALT:eEphem_GEISph_arr[*,2], $
  ;;                  LON:eEphem_GEISph_arr[*,1], $
  ;;                  LAT:eEphem_GEISph_arr[*,0]}
  
  ;; eSpec_AACGM   = {ALT:REFORM(eEphem_AACGMSph_arr[2,*]), $
  ;;                  MLT:REFORM(eEphem_AACGMSph_arr[3,*]), $
  ;;                  LAT:REFORM(eEphem_AACGMSph_arr[0,*])}

  ;;Regulier
  coordName            = 'GEI (SDT-provided)'
  plotTitle            = KEYWORD_SET(add_plotTitles) ? $
                         'Orbit '+orbString+' ('+ coordName+' coordinates)' : !NULL
  sPName               = 'FAST_ephem--orbit_' + orbString + '--' + coordName + '.png'
  plotName             = coordName
  color_list           = 'black'
  SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec_GEI.lon,eSpec_GEI.lat, $
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
                                   PLOTSUFF=plotSuff, $
                                   IN_MAP=map, $
                                   ;; SAVEPLOT=savePlot, $
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
                                   OUT_MAP=out_map, $
                                   _EXTRA=e
  ;; STOP

  ;;GEO
  coordName            = 'GEO'
  plotTitle            = KEYWORD_SET(add_plotTitles) ? $
                         'Orbit '+orbString+' ('+ coordName+' coordinates)' : !NULL
  sPName               = 'FAST_ephem--orbit_' + orbString + '--' + coordName + '.png'
  plotName             = coordName
  color_list           = 'red'
  SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec_GEO.lon,eSpec_GEO.lat, $
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
                                   PLOTSUFF=plotSuff, $
                                   IN_MAP=map, $
                                   ;; SAVEPLOT=savePlot, $
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
                                   ;; /ADD_LEGEND, $
                                   OUT_MAP=out_map, $
                                   _EXTRA=e

  ;;MAG
  coordName            = 'MAG'
  plotTitle            = KEYWORD_SET(add_plotTitles) ? $
                         'Orbit '+orbString+' ('+ coordName+' coordinates)' : !NULL
  sPName               = 'FAST_ephem--orbit_' + orbString + '--' + 'GEI_GEO_MAG' + '.png'
  plotName             = coordName
  color_list           = 'blue'
  SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec_MAG.lon,eSpec_MAG.lat, $
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
                                   PLOTSUFF=plotSuff, $
                                   IN_MAP=map, $
                                   ;; SAVEPLOT=savePlot, $
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
                                   ADD_LEGEND=add_legend, $
                                   OUT_MAP=out_map, $
                                   _EXTRA=e

  ;;MAG
  coordName            = 'AACGM'
  ;; plotTitle            = KEYWORD_SET(add_plotTitles) ? $
  ;;                        'Orbit '+orbString+' ('+ coordName+' coordinates)' : !NULL
  plotTitle            = 'Orbit '+ orbString + ', Northern Hemisphere'
  sPName               = 'FAST_ephem--orbit_' + orbString + '--' + 'GEI_GEO_MAG_AACGM' + '.png'
  plotName             = coordName
  color_list           = 'orange'
  SIMPLE_STEREOGRAPHIC_SCATTERPLOT,eSpec_AACGM.mlt*15.,eSpec_AACGM.lat, $
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

  STOP

END

  ;; orig_routineName     = 'JOURNAL__20160812__CAREFULLY_CHECK_COORD_CONVERSION__FOR_EXAMPLE_ORB_10000'
  ;; eSpecDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  
  ;; ;; outFile          = 'eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info--10to20mil.sav'
  ;; outFile          = 'eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info--restavdebeste.sav'
  
  ;; defNewellDBFile  = 'eSpec_20160607_db--PARSED--Orbs_500-16361.sav' 
  ;; realFile         = 'sorted--' + defNewellDBFile ;;This file does not need to be cleaned
  
  ;; ;;Load the stuff we need 
  ;; PRINT,'Restoring file with sorted/uniq times'
  ;; RESTORE,eSpecDir + realFile
  ;; eSpecTimes       = eSpec.x
  ;; orbInds = WHERE(eSpec.orbit EQ orbNum)
  ;; eSTest           = {x:eSpec.x[orbInds], $
  ;;                     orbit:eSpec.orbit[orbInds], $
  ;;                     mlt:eSpec.mlt[orbInds], $
  ;;                     ilat:eSpec.ilat[orbInds], $
  ;;                     alt:eSpec.alt[orbInds], $
  ;;                     mono:eSpec.mono[orbInds], $
  ;;                     broad:eSpec.broad[orbInds], $
  ;;                     diffuse:eSpec.diffuse[orbInds], $
  ;;                     je:eSpec.je[orbInds], $
  ;;                     jee:eSpec.jee[orbInds], $
  ;;                     nbad_eSpec:eSpec.nbad_eSpec[orbInds]}
  
  ;; GET_FA_ORBIT,eSTest.x,/TIME_ARRAY,/ALL,/DEFINITIVE
  
  ;; GET_DATA,'ORBIT',DATA=orbit
  ;; GET_DATA,'fa_pos',DATA=fa_pos
  ;; GET_DATA,'ALT',DATA=alt
  ;; GET_DATA,'ILAT',DATA=ilat
  ;; GET_DATA,'MLT',DATA=mlt
  ;; GET_DATA,'ILNG',DATA=ilng
  ;; GET_DATA,'LAT',DATA=lat
  ;; GET_DATA,'LNG',DATA=lng
  ;; GET_DATA,'fa_vel',DATA=fa_vel
  
  ;; eSpecEphem   = {orbit:orbit.y, $
  ;;                 fa_pos:fa_pos.y, $
  ;;                 alt:alt.y, $
  ;;                 lat:lat.y, $
  ;;                 lng:lng.y, $
  ;;                 ilat:ilat.y, $
  ;;                 ilng:ilng.y, $
  ;;                 mlt:mlt.y, $
  ;;                 fa_vel:fa_vel.y, $
  ;;                 POS_AND_VEL_COORDS:'GEI (per GET_FA_ORBIT)'}
  
  ;; SAVE,eSTest,eSpecEphem,orbInds,FILENAME=littleBabyFile
  
  ;; esTTemp              = eSTest.x
  
  ;; time_epoch           = UTC_TO_CDF_EPOCH(esTTemp)
  ;; nTot                 = N_ELEMENTS(esTTemp)
  ;; eEphem_MAG_arr       = MAKE_ARRAY(3,nTot,/FLOAT)
  ;; eEphem_GEO_arr       = MAKE_ARRAY(3,nTot,/FLOAT)
  ;; eEphem_GEI_arr       = MAKE_ARRAY(3,nTot,/FLOAT)
  ;; eEphem_MAGSph_arr    = MAKE_ARRAY(3,nTot,/FLOAT)
  ;; eEphem_GEOSph_arr    = MAKE_ARRAY(3,nTot,/FLOAT)
  ;; eEphem_GEISph_arr    = MAKE_ARRAY(3,nTot,/FLOAT)
  
  ;; PRINT,"Feeding it to GEOPACK ..."
  ;; FOR i=0,nTot-1 DO BEGIN
  
  ;;    tmpTime           = TIME_TO_STR(esTTemp[i])
  ;;    YearArr           = FIX(STRMID(tmpTime,0,4))
  ;;    MonthArr          = FIX(STRMID(tmpTime,5,2))
  ;;    DayArr            = FIX(STRMID(tmpTime,8,2))
  ;;    HourArr           = FIX(STRMID(tmpTime,11,2))
  ;;    MinArr            = FIX(STRMID(tmpTime,14,2))
  ;;    SecArr            = FLOAT(STRMID(tmpTime,17,6))
  
  ;;    ;; GEOPACK_RECALC,YearArr[i],MonthArr[i],DayArr[i],HourArr[i],MinArr[i],SecArr[i],/DATE
  ;;    GEOPACK_RECALC,YearArr,MonthArr,DayArr,HourArr,MinArr,SecArr,/DATE
  
  ;;    ;;do that dance
  ;;    ;;To MAG
  ;;    GEOPACK_CONV_COORD,eSpecEphem.fa_pos[i,0],eSpecEphem.fa_pos[i,1],eSpecEphem.fa_pos[i,2], $
  ;;                       faPosmag_x,faPosmag_y,faPosmag_z, $
  ;;                       /FROM_GEI,/TO_MAG,EPOCH=time_epoch[i]
  ;;    ;;To GEO
  ;;    GEOPACK_CONV_COORD,eSpecEphem.fa_pos[i,0],eSpecEphem.fa_pos[i,1],eSpecEphem.fa_pos[i,2], $
  ;;                       faPosgeo_x,faPosgeo_y,faPosgeo_z, $
  ;;                       /FROM_GEI,/TO_GEO,EPOCH=time_epoch[i]
  
  ;;    ;;update
  ;;    eEphem_MAG_arr[*,i] = [faPosmag_x,faPosmag_y,faPosmag_z]
  ;;    eEphem_GEO_arr[*,i] = [faPosgeo_x,faPosgeo_y,faPosgeo_z]
  ;;    eEphem_GEI_arr[*,i] = [eSpecEphem.fa_pos[i,0],eSpecEphem.fa_Pos[i,1],eSpecEphem.fa_Pos[i,2]]
  
  ;;    GEOPACK_SPHCAR,eSpecEphem.faPos[i,0],eSpecEphem.faPos[i,1],eSpecEphem.faPos[i,2],gei_r,gei_theta,gei_phi,/TO_SPHERE,/DEGREE
  ;;    GEOPACK_SPHCAR,faPosgeo_x,faPosgeo_y,faPosgeo_z,geo_r,geo_theta,geo_phi,/TO_SPHERE,/DEGREE
  ;;    GEOPACK_SPHCAR,faPosmag_x,faPosmag_y,faPosmag_z,mag_r,mag_theta,mag_phi,/TO_SPHERE,/DEGREE
  
  ;;    ;;Lat, long, height
  ;;    eEphem_MAGSph_arr[*,i]    = [mag_theta,mag_phi,mag_r] 
  ;;    eEphem_GEOSph_arr[*,i]    = [geo_theta,geo_phi,geo_r] 
  ;;    eEphem_GEISph_arr[*,i]    = [gei_theta,gei_phi,gei_r] 
  
  ;; ENDFOR
  
  ;; eEphem_MAGSph_arr    = [ $
  ;;                        [90.-REFORM(eEphem_MAGSph_arr[0,*])], $
  ;;                        [REFORM(eEphem_MAGSph_arr[1,*])], $
  ;;                        [REFORM(eEphem_MAGSph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
  ;;                        ]   
  
  ;; eEphem_GEOSph_arr    = [ $
  ;;                        [90.-REFORM(eEphem_GEOSph_arr[0,*])], $
  ;;                        [REFORM(eEphem_GEOSph_arr[1,*])], $
  ;;                        [REFORM(eEphem_GEOSph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
  ;;                        ]
  
  ;; eEphem_GEISph_arr    = [ $
  ;;                        [90.-REFORM(eEphem_GEISph_arr[0,*])], $
  ;;                        [REFORM(eEphem_GEISph_arr[1,*])], $
  ;;                        [REFORM(eEphem_GEISph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
  ;;                        ]
  
  ;; eSpec_GEO     = {ALT:eEphem_GEOSph_arr[*,2], $
  ;;                  LON:eEphem_GEOSph_arr[*,1], $
  ;;                  LAT:eEphem_GEOSph_arr[*,0]}
  
  ;; eSpec_MAG     = {ALT:eEphem_MAGSph_arr[*,2], $
  ;;                  LON:eEphem_MAGSph_arr[*,1], $
  ;;                  LAT:eEphem_MAGSph_arr[*,0]}
  
  ;; eSpec_GEI     = {ALT:eEphem_GEISph_arr[*,2], $
  ;;                  LON:eEphem_GEISph_arr[*,1], $
  ;;                  LAT:eEphem_GEISph_arr[*,0]}
    

  ;; eSTestUTC            = TIME_TO_STR(eSTest.x,/MSEC)

  ;; YearArr       = FIX(STRMID(eSTestUTC,0,4))
  ;; MonthArr      = FIX(STRMID(eSTestUTC,5,2))
  ;; DayArr        = FIX(STRMID(eSTestUTC,8,2))
  ;; HourArr       = FIX(STRMID(eSTestUTC,11,2))
  ;; MinArr        = FIX(STRMID(eSTestUTC,14,2))
  ;; SecArr        = FLOAT(STRMID(eSTestUTC,17,6))

  ;; nTot                 = N_ELEMENTS(esTestUTC)
  ;; eEphem_GEOSph_arr    = TRANSPOSE([[eSpec_GEO.LAT],[eSpec_GEO.LON],[eSpec_GEO.ALT]])
  ;; eEphem_AACGMSph_arr  = MAKE_ARRAY(4,nTot,/FLOAT) ;;One more for MLT at end

  ;; PRINT,"Feeding it to AACGM ..."
  ;; FOR i=0,nTot-1 DO BEGIN
  ;; ;; FOR i=0,100-1 DO BEGIN

  ;;    e = AACGM_V2_SETDATETIME(YearArr[i],MonthArr[i],DayArr[i],HourArr[i],MinArr[i],SecArr[i])

  ;;    ;;Get AACGM coords
  ;;    tmpAACGM                  = CNVCOORD_V2(eEphem_GEOSph_arr[*,i], /ALLOW_TRACE)
  ;;    AACGM_MLT                 = MLT_V2(tmpAACGM[1])
  ;;    eEphem_AACGMSph_arr[*,i]  = [tmpAACGM,AACGM_MLT]

  ;;    IF (i MOD 100) EQ 0 THEN PRINT,i

  ;; ENDFOR

  ;; eEphem_AACGMSph_arr[2,*]     = (eEphem_AACGMSph_arr[2,*]*R_E-R_E) ;convert back to altitude above sea level

  ;; eSpec_AACGM   = {ALT:REFORM(eEphem_AACGMSph_arr[2,*]), $
  ;;                  MLT:REFORM(eEphem_AACGMSph_arr[3,*]), $
  ;;                  LAT:REFORM(eEphem_AACGMSph_arr[0,*])}

  ;; SAVE,eSTest,eSpecEphem,orbInds,eSpecCoords,eSpec_GEO,eSpec_GEI,eSpec_MAG,eSpec_AACGM,FILENAME=littleBabyFile

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;;make struct
  ;; eSpecCoords = {TIME: esTTemp, $
  ;;                MAG: eEphem_MAG_arr, $
  ;;                GEO: eEphem_GEO_arr, $
  ;;                GEI: eEphem_GEI_arr, $
  ;;                CREATED: GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
  ;;                ORIGINATING_ROUTINE:orig_routineName}
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ;;Save it
  ;; PRINT,'Saving ' + littleBabyFile + '...'
  ;; SAVE,eSTest,eSpecEphem,orbInds,eSpecCoords,eSpec_GEO,eSpec_GEI,eSpec_MAG,FILENAME=littleBabyFile
  
  ;; PRINT,"Did it!"
