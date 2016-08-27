;2016/08/26 Instead of 
;;Generating info commented out at bottom
PRO JOURNAL__20160826__CAREFULLY_CHECK_COORD_CONVERSION_AND_JE_VALS__ANY_OLD_ORBIT

  COMPILE_OPT idl2

  startPeriod         = 0

  ;;Want to convert and save coordinates?
  convert_nonExisting = 0
  force_conversions   = 1

  ;;Want to do plots?
  make_plots          = 0
  stereo_plots        = 1
  histo_plots         = 1
  savePlot            = 1
  add_plotTitles      = 0
  plotHemi            = 'NORTH'
  stop_and_admire     = 0

  gen_orbList = 0 ;Generate the list afresh, for no good reason?

  startString = ['1999-03-24/23:49:00.000', $  ;(Period 1/6) Orbits: 10241, 10242
                 '1999-03-25/13:04:00.000', $  ;(Period 2/6) Orbits: 10247, 10248
                 '1999-02-17/18:37:00.000', $  ;(Period 3/6) Orbits: 9859
                 '1999-09-15/04:29:00.000', $  ;(Period 4/6) Orbits: 12137
                 '1999-09-28/03:15:00.000', $  ;(Period 5/6) Orbits: 12278
                 '1999-10-22/11:23:00.000']    ;(Period 6/6) Orbits: 12543

  stopString = ['1999-03-25/00:09:00.000', $
                '1999-03-25/13:24:00.000', $
                '1999-02-17/18:57:00.000', $
                '1999-09-15/04:49:00.000', $
                '1999-09-28/03:35:00.000', $
                '1999-10-22/11:43:00.000']

  nPeriods      = N_ELEMENTS(stopString)

  startStopStr  = [TRANSPOSE(startString), $
                   TRANSPOSE(stopString)]

  startStop_UTC = [TRANSPOSE(STR_TO_TIME(TEMPORARY(startString))), $
                   TRANSPOSE(STR_TO_TIME(TEMPORARY(stopString)))]

  diff_UTC      = [startStop_UTC[1,*]-startStop_UTC[0,*]]


  conjOrbList   = LIST([10241,10242],[10247,10248],9859,12137,12278,12543)
  nTot          = 0
  FOR j=startPeriod,nPeriods-1 DO BEGIN

     IF KEYWORD_SET(gen_orbList) THEN BEGIN

        GET_FA_ORBIT,startStop_UTC[0,j],startStop_UTC[1,j]
        GET_DATA,'ORBIT',DATA=orbit

        CHECK_SORTED,orbit.x,isSort,SORTED_I=sort_i,/QUIET
        IF ~isSort THEN orbit = {x:orbit.x[sort_i],y:orbit.y[sort_i]}

        orbits  = orbit.y[UNIQ(orbit.y)]
        conjOrbList.Add,orbits

        nOrbs   = N_ELEMENTS(UNIQ(orbit.y))

     ENDIF ELSE BEGIN
     
        orbits  = conjOrbList[j]
        nOrbs   = N_ELEMENTS(orbits)
        
     ENDELSE

     nTot       += nOrbs

     PRINT,FORMAT='("(Period ",I0,"/",I0,") Orbits: ",10(I0,:,", "))',j+1,nPeriods,orbits

  ENDFOR

  PRINT,'N Total Orbits: ' + STRCOMPRESS(nTot,/REMOVE_ALL)


  FOR j=startPeriod,nPeriods-1 DO BEGIN

     eSOrb = GET_ESPEC_COORD_CONVERSION(eSpec,T1=startStop_UTC[0,j], $
                                        T2=startStop_UTC[1,j], $
                                        TIME_ARRAY=time_array, $
                                        ORBIT_ARRAY=orbitArr, $
                                        /COMBINE_ALL_ESORB_STRUCTS, $
                                        CONVERT_IF_NOTEXIST=convert_nonExisting, $
                                        FORCE_CONVERSIONS=force_conversions)

     IF (eSOrb EQ !NULL) THEN CONTINUE
        
     IF KEYWORD_SET(make_plots) THEN BEGIN
        SET_PLOT_DIR,plotDir,/FOR_ESPEC_DB,/ADD_TODAY

        PLOT_STEREO_AND_HISTOPLOTS__SMALL_ESPECSTRUCT, $
           IN_ESPEC=eSOrb, $
           ORBIT=orbNum, $
           ;; T1=t1, $
           ;; T2=t2, $
           HEMI=plotHemi, $
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
           BATCH=~KEYWORD_SET(stop_and_admire)
     ENDIF

  ENDFOR

END

  ;; ;;File with eSTest,eSpecEphem, eSTest,eSpecEphem,eSpecCoords,eSpec_GEO,eSpec_MAG,orbInds (indices into eSpec DB--the SORTED version with 28604345 entries
  ;; littleBabyFilePref  = '/SPENCEdata/Research/Satellites/FAST/espec_identification/saves_output_etc/eSOrb' 

  ;; orig_routineName    = 'JOURNAL__20160826__CAREFULLY_CHECK_COORD_CONVERSION_AND_JE_VALS__ANY_OLD_ORBIT'
  ;; eSpecDir            = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  
  ;; defNewellDBFile     = 'eSpec_20160607_db--PARSED--Orbs_500-16361.sav' 
  ;; realFile            = 'sorted--' + defNewellDBFile ;;This file does not need to be cleaned
  
  ;; ;;Load the stuff we need 
  ;; PRINT,'Restoring file with sorted/uniq times'
  ;; RESTORE,eSpecDir + realFile
  ;; eSpecTimes          = eSpec.x

  ;; FOR k=0,N_ELEMENTS(orbNumArr)-1 DO BEGIN
  ;;    orbNum                = orbNumArr[k]
  ;;    orbString             = orbStringArr[k]
  ;;    littleBabyFile        = littleBabyFilePref + orbString + '.sav'

  ;;    PRINT,'Doing orbit ' + orbString + ' ...'

  ;;    CASE 1 OF
  ;;       KEYWORD_SET(convert_if_no_exist): BEGIN
  ;;          convertMePlease = ~FILE_TEST(littleBabyFile)
  ;;          PRINT,"File doesn't exist; performing conversion for orbit " + orbString + ' ...'
  ;;          ;; IF convertMePlease THEN BEGIN
  ;;          ;;    do_conversions  = 1
  ;;          ;; ENDIF ELSE BEGIN
  ;;          ;;    do_conversions  = 0
  ;;          ;; ENDELSE
  ;;       END
  ;;       KEYWORD_SET(do_conversions): BEGIN
  ;;          convertMePlease = 1
  ;;       END
  ;;       ELSE: 
  ;;    ENDCASE

  ;;    IF KEYWORD_SET(convertMePlease) THEN BEGIN
  ;;       orbInds            = WHERE(eSpec.orbit EQ orbNum)
  ;;       eSTest             = {x:eSpec.x[orbInds], $
  ;;                             orbit:eSpec.orbit[orbInds], $
  ;;                             mlt:eSpec.mlt[orbInds], $
  ;;                             ilat:eSpec.ilat[orbInds], $
  ;;                             alt:eSpec.alt[orbInds], $
  ;;                             mono:eSpec.mono[orbInds], $
  ;;                             broad:eSpec.broad[orbInds], $
  ;;                             diffuse:eSpec.diffuse[orbInds], $
  ;;                             je:eSpec.je[orbInds], $
  ;;                             jee:eSpec.jee[orbInds], $
  ;;                             nbad_eSpec:eSpec.nbad_eSpec[orbInds]}
        
  ;;       GET_FA_ORBIT,eSTest.x,/TIME_ARRAY,/ALL,/DEFINITIVE
        
  ;;       GET_DATA,'ORBIT',DATA=orbit
  ;;       GET_DATA,'fa_pos',DATA=fa_pos
  ;;       GET_DATA,'ALT',DATA=alt
  ;;       GET_DATA,'ILAT',DATA=ilat
  ;;       GET_DATA,'MLT',DATA=mlt
  ;;       GET_DATA,'ILNG',DATA=ilng
  ;;       GET_DATA,'LAT',DATA=lat
  ;;       GET_DATA,'LNG',DATA=lng
  ;;       GET_DATA,'fa_vel',DATA=fa_vel
        
  ;;       eSpecEphem         = {orbit:orbit.y, $
  ;;                             fa_pos:fa_pos.y, $
  ;;                             alt:alt.y, $
  ;;                             lat:lat.y, $
  ;;                             lng:lng.y, $
  ;;                             ilat:ilat.y, $
  ;;                             ilng:ilng.y, $
  ;;                             mlt:mlt.y, $
  ;;                             fa_vel:fa_vel.y, $
  ;;                             POS_AND_VEL_COORDS:'GEI (per GET_FA_ORBIT)'}
        
  ;;       ;; PRINT,'Saving eSpecEphem for orbit ' + orbString + ' ...'
  ;;       ;; SAVE,eSTest,eSpecEphem,orbInds,FILENAME=littleBabyFile
        
  ;;       esTTemp            = eSTest.x
        
  ;;       time_epoch         = UTC_TO_CDF_EPOCH(esTTemp)
  ;;       nTot               = N_ELEMENTS(esTTemp)
  ;;       eEphem_MAG_arr     = MAKE_ARRAY(3,nTot,/FLOAT)
  ;;       eEphem_GEO_arr     = MAKE_ARRAY(3,nTot,/FLOAT)
  ;;       eEphem_GEI_arr     = MAKE_ARRAY(3,nTot,/FLOAT)
  ;;       eEphem_MAGSph_arr  = MAKE_ARRAY(3,nTot,/FLOAT)
  ;;       eEphem_GEOSph_arr  = MAKE_ARRAY(3,nTot,/FLOAT)
  ;;       eEphem_GEISph_arr  = MAKE_ARRAY(3,nTot,/FLOAT)
        
  ;;       PRINT,"Feeding it to GEOPACK ..."
  ;;       FOR i=0,nTot-1 DO BEGIN
           
  ;;          tmpTime   = TIME_TO_STR(esTTemp[i])
  ;;          YearArr   = FIX(STRMID(tmpTime,0,4))
  ;;          MonthArr  = FIX(STRMID(tmpTime,5,2))
  ;;          DayArr    = FIX(STRMID(tmpTime,8,2))
  ;;          HourArr   = FIX(STRMID(tmpTime,11,2))
  ;;          MinArr    = FIX(STRMID(tmpTime,14,2))
  ;;          SecArr    = FLOAT(STRMID(tmpTime,17,6))
           
  ;;          ;; GEOPACK_RECALC,YearArr[i],MonthArr[i],DayArr[i],HourArr[i],MinArr[i],SecArr[i],/DATE
  ;;          GEOPACK_RECALC,YearArr,MonthArr,DayArr,HourArr,MinArr,SecArr,/DATE
           
  ;;          ;;do that dance
  ;;          ;;To MAG
  ;;          GEOPACK_CONV_COORD,eSpecEphem.fa_pos[i,0],eSpecEphem.fa_pos[i,1],eSpecEphem.fa_pos[i,2], $
  ;;                             faPosmag_x,faPosmag_y,faPosmag_z, $
  ;;                             /FROM_GEI,/TO_MAG,EPOCH=time_epoch[i]
  ;;          ;;To GEO
  ;;          GEOPACK_CONV_COORD,eSpecEphem.fa_pos[i,0],eSpecEphem.fa_pos[i,1],eSpecEphem.fa_pos[i,2], $
  ;;                             faPosgeo_x,faPosgeo_y,faPosgeo_z, $
  ;;                             /FROM_GEI,/TO_GEO,EPOCH=time_epoch[i]
           
  ;;          ;;update
  ;;          eEphem_MAG_arr[*,i]     = [faPosmag_x,faPosmag_y,faPosmag_z]
  ;;          eEphem_GEO_arr[*,i]     = [faPosgeo_x,faPosgeo_y,faPosgeo_z]
  ;;          eEphem_GEI_arr[*,i]     = [eSpecEphem.fa_pos[i,0],eSpecEphem.fa_Pos[i,1],eSpecEphem.fa_Pos[i,2]]
           
  ;;          GEOPACK_SPHCAR,eSpecEphem.fa_pos[i,0],eSpecEphem.fa_pos[i,1],eSpecEphem.fa_pos[i,2],gei_r,gei_theta,gei_phi,/TO_SPHERE,/DEGREE
  ;;          GEOPACK_SPHCAR,faPosgeo_x,faPosgeo_y,faPosgeo_z,geo_r,geo_theta,geo_phi,/TO_SPHERE,/DEGREE
  ;;          GEOPACK_SPHCAR,faPosmag_x,faPosmag_y,faPosmag_z,mag_r,mag_theta,mag_phi,/TO_SPHERE,/DEGREE
           
  ;;          ;;Lat, long, height
  ;;          eEphem_MAGSph_arr[*,i]  = [mag_theta,mag_phi,mag_r] 
  ;;          eEphem_GEOSph_arr[*,i]  = [geo_theta,geo_phi,geo_r] 
  ;;          eEphem_GEISph_arr[*,i]  = [gei_theta,gei_phi,gei_r] 
           
  ;;       ENDFOR
        
  ;;       eEphem_MAGSph_arr    = [ $
  ;;                              [90.-REFORM(eEphem_MAGSph_arr[0,*])], $
  ;;                              [REFORM(eEphem_MAGSph_arr[1,*])], $
  ;;                              [REFORM(eEphem_MAGSph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
  ;;                              ]   
        
  ;;       eEphem_GEOSph_arr    = [ $
  ;;                              [90.-REFORM(eEphem_GEOSph_arr[0,*])], $
  ;;                              [REFORM(eEphem_GEOSph_arr[1,*])], $
  ;;                              [REFORM(eEphem_GEOSph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
  ;;                              ]
        
  ;;       eEphem_GEISph_arr    = [ $
  ;;                              [90.-REFORM(eEphem_GEISph_arr[0,*])], $
  ;;                              [REFORM(eEphem_GEISph_arr[1,*])], $
  ;;                              [REFORM(eEphem_GEISph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
  ;;                              ]
        
  ;;       eSpec_GEO            = {ALT:eEphem_GEOSph_arr[*,2], $
  ;;                               LON:eEphem_GEOSph_arr[*,1], $
  ;;                               LAT:eEphem_GEOSph_arr[*,0]}
        
  ;;       eSpec_MAG            = {ALT:eEphem_MAGSph_arr[*,2], $
  ;;                               LON:eEphem_MAGSph_arr[*,1], $
  ;;                               LAT:eEphem_MAGSph_arr[*,0]}
        
  ;;       eSpec_GEI            = {ALT:eEphem_GEISph_arr[*,2], $
  ;;                               LON:eEphem_GEISph_arr[*,1], $
  ;;                               LAT:eEphem_GEISph_arr[*,0]}
        

  ;;       eSTestUTC            = TIME_TO_STR(eSTest.x,/MSEC)

  ;;       YearArr              = FIX(STRMID(eSTestUTC,0,4))
  ;;       MonthArr             = FIX(STRMID(eSTestUTC,5,2))
  ;;       DayArr               = FIX(STRMID(eSTestUTC,8,2))
  ;;       HourArr              = FIX(STRMID(eSTestUTC,11,2))
  ;;       MinArr               = FIX(STRMID(eSTestUTC,14,2))
  ;;       SecArr               = FLOAT(STRMID(eSTestUTC,17,6))

  ;;       nTot                 = N_ELEMENTS(esTestUTC)
  ;;       eEphem_GEOSph_arr    = TRANSPOSE([[eSpec_GEO.LAT],[eSpec_GEO.LON],[eSpec_GEO.ALT]])
  ;;       eEphem_AACGMSph_arr  = MAKE_ARRAY(4,nTot,/FLOAT) ;;One more for MLT at end

  ;;       PRINT,"Feeding it to AACGM ..."
  ;;       FOR i=0,nTot-1 DO BEGIN
  ;;          ;; FOR i=0,100-1 DO BEGIN

  ;;          err = AACGM_V2_SETDATETIME(YearArr[i],MonthArr[i],DayArr[i],HourArr[i],MinArr[i],SecArr[i])

  ;;          ;;Get AACGM coords
  ;;          tmpAACGM                  = CNVCOORD_V2(eEphem_GEOSph_arr[*,i],/ALLOW_TRACE)
  ;;          AACGM_MLT                 = MLT_V2(tmpAACGM[1])
  ;;          eEphem_AACGMSph_arr[*,i]  = [tmpAACGM,AACGM_MLT]

  ;;          IF (i MOD 100) EQ 0 THEN PRINT,i

  ;;       ENDFOR

  ;;       eEphem_AACGMSph_arr[2,*]     = (eEphem_AACGMSph_arr[2,*]*R_E-R_E) ;convert back to altitude above sea level

  ;;       eSpec_AACGM                  = {ALT:REFORM(eEphem_AACGMSph_arr[2,*]), $
  ;;                                       MLT:REFORM(eEphem_AACGMSph_arr[3,*]), $
  ;;                                       LAT:REFORM(eEphem_AACGMSph_arr[0,*])}

  ;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;       ;;make struct
  ;;       eSOrb  = {x          : esTTemp          , $
  ;;                 orbit      : eSpecEphem.orbit , $
  ;;                 coords     : {SDT   : {MLT  : eSpecEphem.MLT     , $
  ;;                                        ILAT : eSpecEphem.ILAT    , $
  ;;                                        alt  : eSpecEphem.alt}    , $
  ;;                               AACGM : TEMPORARY(eSpec_AACGM)     , $
  ;;                               GEI   : TEMPORARY(eSpec_GEI)       , $
  ;;                               GEO   : TEMPORARY(eSpec_GEO)       , $
  ;;                               MAG   : TEMPORARY(eSpec_MAG)}      , $
  ;;                 Je         : eSTest.Je                                           , $
  ;;                 JEe        : eSTest.JEe                                          , $
  ;;                 file_info  : {creation_date : GET_TODAY_STRING(/DO_YYYYMMDD_FMT) , $
  ;;                               originating_routine : orig_routineName} $
  ;;                }
        
  ;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;       ;;Save it
  ;;       PRINT,'Saving eSOrb, with several coordinates, for orbit ' + orbString + ' ...'
  ;;       SAVE,eSOrb,orbInds,FILENAME=littleBabyFile

  ;;       PRINT,"Did it!"

  ;;    ENDIF ELSE BEGIN
  ;;       PRINT,'Restoring ' + littleBabyFile + ' ...'
  ;;       RESTORE,littleBabyFile
  ;;    ENDELSE


  ;;    IF KEYWORD_SET(make_plots) THEN BEGIN
  ;;       SET_PLOT_DIR,plotDir,/FOR_ESPEC_DB,/ADD_TODAY

  ;;       PLOT_STEREO_AND_HISTOPLOTS__SMALL_ESPECSTRUCT, $
  ;;          IN_ESPEC=eSOrb, $
  ;;          ORBIT=orbNum, $
  ;;          ;; T1=t1, $
  ;;          ;; T2=t2, $
  ;;          HEMI=plotHemi, $
  ;;          STEREO_PLOTS=stereo_plots, $
  ;;          HISTO_PLOTS=histo_plots, $
  ;;          HISTO_ONEORB=histo_oneOrb, $
  ;;          HISTO__SEPARATE_HEMIS=histo__separate_hemis, $
  ;;          STEREO__EXCLUDE_SDT=stereo__exclude_SDT, $
  ;;          STEREO__EXCLUDE_GEI=stereo__exclude_GEI, $
  ;;          STEREO__EXCLUDE_GEO=stereo__exclude_GEO, $
  ;;          STEREO__EXCLUDE_MAG=stereo__exclude_MAG, $
  ;;          STEREO__EXCLUDE_AACGM=stereo__exclude_AACGM, $
  ;;          ADD_PLOTTITLE=add_plotTitle, $
  ;;          RESTRICT_HISTO_ILAT_RANGE=restrict_histo_ILAT_range, $
  ;;          SAVEPLOT=savePlot, $
  ;;          PLOTDIR=plotDir, $
  ;;          BATCH=batch
  ;;    ENDIF

  ;; ENDFOR



