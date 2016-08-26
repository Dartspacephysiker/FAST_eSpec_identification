;2016/08/26 Instead of 
;;Generating info commented out at bottom
FUNCTION GET_ESPEC_COORD_CONVERSION,eSpec, $
                                    T1=t1, $
                                    T2=t2, $
                                    TIME_ARRAY=time_array, $
                                    ORBIT_ARRAY=orbitArr, $
                                    COMBINE_ALL_ESORB_STRUCTS=combine_eSOrbStructs, $
                                    CONVERT_IF_NOTEXIST=convert_nonExisting, $
                                    FORCE_CONVERSIONS=force_conversions

  COMPILE_OPT idl2

  @common__newell_espec.pro

  __VERSIONSTR        = '1.0'

  R_E                 = 6371.2D ;Earth radius in km, from IGRFLIB_V2.pro

  outDir              = '/SPENCEdata/Research/Satellites/FAST/espec_identification/saves_output_etc/'
  littleBabyFilePref  = 'eSOrb--' 

  orig_routineName    = 'GET_ESPEC_COORD_CONVERSION v'+__VERSIONSTR
  

  ;;Load the stuff we need 
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,/DONT_PERFORM_CORRECTION
  ENDIF

  CASE 1 OF
     KEYWORD_SET(t1) AND KEYWORD_SET(time_array): BEGIN
        nTimes = N_ELEMENTS(t1)
        PRINT,"Getting coordinate conversions for " + $
              STRCOMPRESS(nTimes,/REMOVE_ALL) + " times ..."

        nLoops        = 1
        haveOrbArr    = 0
        timeTypeStr   = 'times' 
     END
     KEYWORD_SET(t1) AND KEYWORD_SET(t2)        : BEGIN
        nTimeSets     = N_ELEMENTS(t1)
        PRINT,"Getting coordinate conversions for " + $
              STRCOMPRESS(nTimeSets,/REMOVE_ALL) + " start/stop times ..."

        IF nTimeSets NE N_ELEMENTS(t2) THEN BEGIN
           PRINT,'Unequal # of input start times and stop times!'
           STOP
        ENDIF
        nLoops        = nTimeSets
        haveOrbArr    = 0
        timeTypeStr   = 'times' 
     END
     KEYWORD_SET(orbitArr): BEGIN
        nOrbs         = N_ELEMENTS(orbitArr)

        PRINT,"Getting coordinate conversions for " + $
              STRCOMPRESS(nOrbs,/REMOVE_ALL) + ' orbits'
        orbStringArr  = STRCOMPRESS(orbitArr,/REMOVE_ALL)

        nLoops        = nOrbs
        haveOrbArr    = 1
        timeTypeStr   = 'orbit' 

     END
  ENDCASE

  eSOrbList            = LIST()

  FOR k=0,nLoops-1 DO BEGIN

     CASE 1 OF
        KEYWORD_SET(time_array): BEGIN

           orbDateStr       = [STRMID(TIME_TO_STR(t1[0]),0,10), $
                               STRMID(TIME_TO_STR(t1[-1]),0,10)]
           CASE 1 OF
              (orbDateStr[0] EQ orbDateStr[1]): BEGIN
                 tempFNStr  = orbDateStr[0] + '_-_' + STRMID(TIME_TO_STR(t1[0],/MSEC),11,11) + '-' + $
                              STRMID(TIME_TO_STR(t1[0],/MSEC),11,11)
              END
              ELSE: BEGIN
                 tempFNStr  = orbDateStr[0] + '_-_' + STRMID(TIME_TO_STR(t1[0],/MSEC),11,11) + '-' + $
                              orbDateStr[1] + '_-_' + STRMID(TIME_TO_STR(t1[-1],/MSEC),11,11)
              END
           ENDCASE
           
           ;;Make the strings nice for filenames down the line
           tempFNStr   = tempFNStr.REPLACE('-', '_')
           tempFNStr   = tempFNStr.REPLACE(':', '_')
           tempFNStr   = tempFNStrs.REPLACE('.', '__')
        END
        KEYWORD_SET(t1[k]) AND KEYWORD_SET(t2[k])        : BEGIN
           PRINT,"Getting coordinate conversions for " + $
                 TIME_TO_STR(t1[k]) + '–' + TIME_TO_STR(t2[k])


           orbDateStr       = [STRMID(TIME_TO_STR(t1[k]),0,10), $
                               STRMID(TIME_TO_STR(t2[k]),0,10)]
           CASE 1 OF
              (orbDateStr[0] EQ orbDateStr[1]): BEGIN
                 tempFNStr  = orbDateStr[0] + '_-_' + STRMID(TIME_TO_STR(t1[k],/MSEC),11,11) + '-' + $
                              STRMID(TIME_TO_STR(t2[k],/MSEC),11,11)
              END
              ELSE: BEGIN
                 tempFNStr  = orbDateStr[0] + '_-_' + STRMID(TIME_TO_STR(t1[k],/MSEC),11,11) + '-' + $
                              orbDateStr[1] + '_-_' + STRMID(TIME_TO_STR(t2[k],/MSEC),11,11)
              END
           ENDCASE
           
           ;;Make the strings nice for filenames down the line
           tempFNStr   = tempFNStr.REPLACE('-', '_')
           tempFNStr   = tempFNStr.REPLACE(':', '_')
           tempFNStr   = tempFNStr.REPLACE('.', '__')

        END
        KEYWORD_SET(orbitArr): BEGIN
           orbNum           = orbitArr[k]
           tempFNString     = orbStringArr[k]

           PRINT,'Getting coordinate conversions for ' + timeTypeStr + ' ' + tempFNString + ' ...'
        END
     ENDCASE
     littleBabyFile   = outDir + littleBabyFilePref + tempFNStr + '.sav'

     CASE 1 OF
        KEYWORD_SET(convert_nonExisting): BEGIN
           convertMePlease = ~FILE_TEST(littleBabyFile)

           IF convertMePlease THEN BEGIN
              PRINT,"File doesn't exist; performing conversion for " + timeTypeStr + ' ' + tempFNStr + ' ...'
           ENDIF
        END
        KEYWORD_SET(force_conversions): BEGIN
           convertMePlease = 1
        END
        ELSE: 
     ENDCASE

     IF KEYWORD_SET(convertMePlease) THEN BEGIN
        CASE 1 OF
           KEYWORD_SET(time_array): BEGIN
              tmp_i        = VALUE_LOCATE(NEWELL__eSpec.x,t1,/L64)
              CHECK_SORTED,tmp_i,isSort,SORTED_I=sort_ii,/QUIET
              IF ~isSort THEN BEGIN
                 PRINT,'Unsorted temporary indices!! Attempting to sort/clean ...'
                 tmp_i     = tmp_i[TEMPORARY(sort_ii)]
              ENDIf
              IF N_ELEMENTS(tmp_i) NE N_ELEMENTS(UNIQ(tmp_i)) THEN BEGIN
                 PRINT,"There are duplicate tmpIndices present."
                 STOP
              ENDIF
              nTmp         = N_ELEMENTS(tmp_i)

              IF nTmp EQ 1 AND nTmp[0] EQ -1 THEN BEGIN
                 PRINT,"Pretty sure it's bogus."
                 STOP
              ENDIF
           END
           KEYWORD_SET(t1[k]) AND KEYWORD_SET(t2[k]): BEGIN
              tmp_i        = WHERE((NEWELL__eSpec.x GE t1) AND (NEWELL__eSpec.x LE t2),nTmp)
           END
           KEYWORD_SET(orbitArr): BEGIN
              tmp_i        = WHERE(NEWELL__eSpec.orbit EQ orbNum,nTmp)
           END
        ENDCASE

        IF nTmp EQ 0 THEN BEGIN
           PRINT,"No indices available for " + timeTypeStr + ' ' + tempFNStr + '! Skipping, I guess'
           eSOrbList       = !NULL
           CONTINUE
        ENDIF

        eSTest             = {x:          NEWELL__eSpec.x[tmp_i]       , $
                              orbit:      NEWELL__eSpec.orbit[tmp_i]   , $
                              mlt:        NEWELL__eSpec.mlt[tmp_i]     , $
                              ilat:       NEWELL__eSpec.ilat[tmp_i]    , $
                              alt:        NEWELL__eSpec.alt[tmp_i]     , $
                              mono:       NEWELL__eSpec.mono[tmp_i]    , $
                              broad:      NEWELL__eSpec.broad[tmp_i]   , $
                              diffuse:    NEWELL__eSpec.diffuse[tmp_i] , $
                              je:         NEWELL__eSpec.je[tmp_i]      , $
                              jee:        NEWELL__eSpec.jee[tmp_i]     , $
                              nbad_eSpec: NEWELL__eSpec.nbad_eSpec[tmp_i]}
        
        GET_FA_ORBIT,eSTest.x,/TIME_ARRAY,/ALL,/DEFINITIVE
        
        GET_DATA,'ORBIT',DATA=orbit
        GET_DATA,'fa_pos',DATA=fa_pos
        GET_DATA,'ALT',DATA=alt
        GET_DATA,'ILAT',DATA=ilat
        GET_DATA,'MLT',DATA=mlt
        GET_DATA,'ILNG',DATA=ilng
        GET_DATA,'LAT',DATA=lat
        GET_DATA,'LNG',DATA=lng
        GET_DATA,'fa_vel',DATA=fa_vel
        
        eSpecEphem         = {orbit:orbit.y, $
                              fa_pos:fa_pos.y, $
                              alt:alt.y, $
                              lat:lat.y, $
                              lng:lng.y, $
                              ilat:ilat.y, $
                              ilng:ilng.y, $
                              mlt:mlt.y, $
                              fa_vel:fa_vel.y, $
                              POS_AND_VEL_COORDS:'GEI (per GET_FA_ORBIT)'}
        
        
        esTTemp            = eSTest.x
        
        time_epoch         = UTC_TO_CDF_EPOCH(esTTemp)
        nTot               = N_ELEMENTS(esTTemp)
        eEphem_MAG_arr     = MAKE_ARRAY(3,nTot,/FLOAT)
        eEphem_GEO_arr     = MAKE_ARRAY(3,nTot,/FLOAT)
        eEphem_GEI_arr     = MAKE_ARRAY(3,nTot,/FLOAT)
        eEphem_MAGSph_arr  = MAKE_ARRAY(3,nTot,/FLOAT)
        eEphem_GEOSph_arr  = MAKE_ARRAY(3,nTot,/FLOAT)
        eEphem_GEISph_arr  = MAKE_ARRAY(3,nTot,/FLOAT)
        
        PRINT,"Feeding it to GEOPACK ..."
        FOR i=0,nTot-1 DO BEGIN
           
           tmpTime   = TIME_TO_STR(esTTemp[i])
           YearArr   = FIX(STRMID(tmpTime,0,4))
           MonthArr  = FIX(STRMID(tmpTime,5,2))
           DayArr    = FIX(STRMID(tmpTime,8,2))
           HourArr   = FIX(STRMID(tmpTime,11,2))
           MinArr    = FIX(STRMID(tmpTime,14,2))
           SecArr    = FLOAT(STRMID(tmpTime,17,6))
           
           ;; GEOPACK_RECALC,YearArr[i],MonthArr[i],DayArr[i],HourArr[i],MinArr[i],SecArr[i],/DATE
           GEOPACK_RECALC,YearArr,MonthArr,DayArr,HourArr,MinArr,SecArr,/DATE
           
           ;;do that dance
           ;;To MAG
           GEOPACK_CONV_COORD,eSpecEphem.fa_pos[i,0],eSpecEphem.fa_pos[i,1],eSpecEphem.fa_pos[i,2], $
                              faPosmag_x,faPosmag_y,faPosmag_z, $
                              /FROM_GEI,/TO_MAG,EPOCH=time_epoch[i]
           ;;To GEO
           GEOPACK_CONV_COORD,eSpecEphem.fa_pos[i,0],eSpecEphem.fa_pos[i,1],eSpecEphem.fa_pos[i,2], $
                              faPosgeo_x,faPosgeo_y,faPosgeo_z, $
                              /FROM_GEI,/TO_GEO,EPOCH=time_epoch[i]
           
           ;;update
           eEphem_MAG_arr[*,i]     = [faPosmag_x,faPosmag_y,faPosmag_z]
           eEphem_GEO_arr[*,i]     = [faPosgeo_x,faPosgeo_y,faPosgeo_z]
           eEphem_GEI_arr[*,i]     = [eSpecEphem.fa_pos[i,0],eSpecEphem.fa_Pos[i,1],eSpecEphem.fa_Pos[i,2]]
           
           GEOPACK_SPHCAR,eSpecEphem.fa_pos[i,0],eSpecEphem.fa_pos[i,1],eSpecEphem.fa_pos[i,2],gei_r,gei_theta,gei_phi,/TO_SPHERE,/DEGREE
           GEOPACK_SPHCAR,faPosgeo_x,faPosgeo_y,faPosgeo_z,geo_r,geo_theta,geo_phi,/TO_SPHERE,/DEGREE
           GEOPACK_SPHCAR,faPosmag_x,faPosmag_y,faPosmag_z,mag_r,mag_theta,mag_phi,/TO_SPHERE,/DEGREE
           
           ;;Lat, long, height
           eEphem_MAGSph_arr[*,i]  = [mag_theta,mag_phi,mag_r] 
           eEphem_GEOSph_arr[*,i]  = [geo_theta,geo_phi,geo_r] 
           eEphem_GEISph_arr[*,i]  = [gei_theta,gei_phi,gei_r] 
           
        ENDFOR
        
        eEphem_MAGSph_arr    = [ $
                               [90.-REFORM(eEphem_MAGSph_arr[0,*])], $
                               [REFORM(eEphem_MAGSph_arr[1,*])], $
                               [REFORM(eEphem_MAGSph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
                               ]   
        
        eEphem_GEOSph_arr    = [ $
                               [90.-REFORM(eEphem_GEOSph_arr[0,*])], $
                               [REFORM(eEphem_GEOSph_arr[1,*])], $
                               [REFORM(eEphem_GEOSph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
                               ]
        
        eEphem_GEISph_arr    = [ $
                               [90.-REFORM(eEphem_GEISph_arr[0,*])], $
                               [REFORM(eEphem_GEISph_arr[1,*])], $
                               [REFORM(eEphem_GEISph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
                               ]
        
        eSpec_GEO            = {ALT:eEphem_GEOSph_arr[*,2], $
                                LON:eEphem_GEOSph_arr[*,1], $
                                LAT:eEphem_GEOSph_arr[*,0]}
        
        eSpec_MAG            = {ALT:eEphem_MAGSph_arr[*,2], $
                                LON:eEphem_MAGSph_arr[*,1], $
                                LAT:eEphem_MAGSph_arr[*,0]}
        
        eSpec_GEI            = {ALT:eEphem_GEISph_arr[*,2], $
                                LON:eEphem_GEISph_arr[*,1], $
                                LAT:eEphem_GEISph_arr[*,0]}
        
        eSTestUTC            = TIME_TO_STR(eSTest.x,/MSEC)

        YearArr              = FIX(STRMID(eSTestUTC,0,4))
        MonthArr             = FIX(STRMID(eSTestUTC,5,2))
        DayArr               = FIX(STRMID(eSTestUTC,8,2))
        HourArr              = FIX(STRMID(eSTestUTC,11,2))
        MinArr               = FIX(STRMID(eSTestUTC,14,2))
        SecArr               = FLOAT(STRMID(eSTestUTC,17,6))

        nTot                 = N_ELEMENTS(esTestUTC)
        eEphem_GEOSph_arr    = TRANSPOSE([[eSpec_GEO.LAT],[eSpec_GEO.LON],[eSpec_GEO.ALT]])
        eEphem_AACGMSph_arr  = MAKE_ARRAY(4,nTot,/FLOAT) ;;One more for MLT at end

        PRINT,"Feeding it to AACGM ..."
        FOR i=0,nTot-1 DO BEGIN
           ;; FOR i=0,100-1 DO BEGIN

           err = AACGM_V2_SETDATETIME(YearArr[i],MonthArr[i],DayArr[i],HourArr[i],MinArr[i],SecArr[i])

           ;;Get AACGM coords
           tmpAACGM                  = CNVCOORD_V2(eEphem_GEOSph_arr[*,i],/ALLOW_TRACE)
           AACGM_MLT                 = MLT_V2(tmpAACGM[1])
           eEphem_AACGMSph_arr[*,i]  = [tmpAACGM,AACGM_MLT]

           IF (i MOD 100) EQ 0 THEN PRINT,i

        ENDFOR

        eEphem_AACGMSph_arr[2,*]     = (eEphem_AACGMSph_arr[2,*]*R_E-R_E) ;convert back to altitude above sea level

        eSpec_AACGM                  = {ALT:REFORM(eEphem_AACGMSph_arr[2,*]), $
                                        MLT:REFORM(eEphem_AACGMSph_arr[3,*]), $
                                        LAT:REFORM(eEphem_AACGMSph_arr[0,*])}

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;make struct
        eSOrb  = {x          : esTTemp          , $
                  orbit      : eSpecEphem.orbit , $
                  coords     : {SDT   : {MLT  : eSpecEphem.MLT     , $
                                         ILAT : eSpecEphem.ILAT    , $
                                         alt  : eSpecEphem.alt}    , $
                                AACGM : TEMPORARY(eSpec_AACGM)     , $
                                GEI   : TEMPORARY(eSpec_GEI)       , $
                                GEO   : TEMPORARY(eSpec_GEO)       , $
                                MAG   : TEMPORARY(eSpec_MAG)}      , $
                  Je         : eSTest.Je                                           , $
                  JEe        : eSTest.JEe                                          , $
                  file_info  : {creation_date       : GET_TODAY_STRING(/DO_YYYYMMDD_FMT) , $
                                originating_routine : orig_routineName, $
                                originating_db      : NEWELL__dbFile} $
                 }
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;Save it
        PRINT,'Saving eSOrb, with several coordinates, for ' + $
              timeTypeStr + ' ' + tempFNStr + ' ...'
        SAVE,eSOrb,tmp_i,FILENAME=littleBabyFile

        PRINT,"Did it!"

     ENDIF ELSE BEGIN
        PRINT,'Restoring ' + littleBabyFile + ' ...'
        RESTORE,littleBabyFile
     ENDELSE

     eSOrbList.Add,eSOrb


  ENDFOR

  ;;If eSOrbList doesn't exist, just get outta here!
  IF ~KEYWORD_SET(combine_eSOrbStructs) OR (eSOrbList EQ !NULL ) THEN RETURN,eSOrbList

  eSOrbFinal  = {x          : eSOrbList[0].x    , $
                 orbit      : eSOrbList[0].orbit, $
                 coords     : {SDT   : eSOrbList[0].coords.SDT, $
                               AACGM : eSOrbList[0].coords.AACGM    , $
                               GEI   : eSOrbList[0].coords.GEI, $
                               GEO   : eSOrbList[0].coords.GEO, $
                               MAG   : eSOrbList[0].coords.MAG}, $
                 Je         : eSOrbList[0].Je   , $
                 JEe        : eSOrbList[0].JEe  , $
                 file_info  : {creation_date       : GET_TODAY_STRING(/DO_YYYYMMDD_FMT) , $
                               originating_routine : orig_routineName, $
                               originating_db      : NEWELL__dbFile} $
                }

  FOR k=1,nLoops-1 DO BEGIN

     eSOrbFinal  = {x          : [eSOrbFinal.x,eSOrbList[k].x]    , $
                    orbit      : [eSOrbFinal.orbit,eSOrbList[k].orbit], $
                    coords     : {SDT   : {MLT:[eSOrbFinal.coords.SDT.MLT, $
                                                eSOrbList[k].coords.SDT.MLT], $
                                           ILAT:[eSOrbFinal.coords.SDT.ILAT, $
                                                 eSOrbList[k].coords.SDT.ILAT], $
                                           ALT:[eSOrbFinal.coords.SDT.alt, $
                                                eSOrbList[k].coords.SDT.alt]}, $
                                  AACGM   : {MLT:[eSOrbFinal.coords.AACGM.MLT, $
                                                  eSOrbList[k].coords.AACGM.MLT], $
                                             LAT:[eSOrbFinal.coords.AACGM.lat, $
                                                  eSOrbList[k].coords.AACGM.lat], $
                                             ALT:[eSOrbFinal.coords.AACGM.alt, $
                                                  eSOrbList[k].coords.AACGM.alt]}, $
                                  GEI   : {LON:[eSOrbFinal.coords.GEI.lon, $
                                                eSOrbList[k].coords.GEI.lon], $
                                           LAT:[eSOrbFinal.coords.GEI.lat, $
                                                eSOrbList[k].coords.GEI.lat], $
                                           ALT:[eSOrbFinal.coords.GEI.alt, $
                                                eSOrbList[k].coords.GEI.alt]}, $
                                  GEO   : {LON:[eSOrbFinal.coords.GEO.lon, $
                                                eSOrbList[k].coords.GEO.lon], $
                                           LAT:[eSOrbFinal.coords.GEO.lat, $
                                                eSOrbList[k].coords.GEO.lat], $
                                           ALT:[eSOrbFinal.coords.GEO.alt, $
                                                eSOrbList[k].coords.GEO.alt]}, $
                                  MAG   : {LON:[eSOrbFinal.coords.MAG.lon, $
                                                eSOrbList[k].coords.MAG.lon], $
                                           LAT:[eSOrbFinal.coords.MAG.lat, $
                                                eSOrbList[k].coords.MAG.lat], $
                                           ALT:[eSOrbFinal.coords.MAG.alt, $
                                                eSOrbList[k].coords.MAG.alt]} $
                                 }, $
                    Je         : [eSOrbFinal.Je,eSOrbList[k].Je]   , $
                    JEe        : [eSOrbFinal.JEe,eSOrbList[k].JEe]  , $
                    file_info  : eSOrbFinal.file_info $
                   }

  ENDFOR

  RETURN,eSOrbFinal

END


