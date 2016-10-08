;;2016/10/08
PRO JOURNAL__20161008__GET_ESPECDB_FOOTPOINTS_AND_MAGFIELD_RATIOS__GEOPACK_STYLE

  COMPILE_OPT IDL2

  PRINT,"STOP! Work in progress! Haven't figured out the details of the field-line mapping and as of right now (2016/10/08) I'm just resorting to SDT's little BFOOT thing."
  PRINT,'(See JOURNAL__20161008__GET_ESPECDB_FOOTPOINTS_AND_MAGFIELD_RATIOS__SDT_STYLE)'
  STOP

  orig_routineName  = 'JOURNAL__20161008__GET_ESPECDB_FOOTPOINTS_AND_MAGFIELD_RATIOS'

  R_E               = 6371.2D    ;Earth radius in km, from IGRFLIB_V2.pro

  altitude_max      = 4180       ;in km
  allow_fl_trace    = 1          ;Allow fieldline tracing for AACGM_v2?
  check_if_exists   = 1
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Input

  ;;This is the eSpec DB being referenced, o'course
  eSpecDir          = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  
  ;; defNewellDBFile   = 'eSpec_20160607_db--PARSED--Orbs_500-16361.sav' 
  ;; realFile          = 'sorted--' + defNewellDBFile ;;This file does not need to be cleaned
  
  ;;Just for reference--we don't actually use these here
  ephemFileIndArr   = [[      0,10000000,20000000], $
                      [9999999,19999999,28604344]]

  coordFiles        = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords' + $
                      ['_1','_2','_3']+'.sav'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;output
  outDir            = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  outFiles          = 'sorted--eSpec_20160607_db--Orbs_500-16361--footPoints_and_Bfield_ratios' + $
                      ['_1','_2','_3']+'--recalc_for_every_point.sav'

  ;;In case we get cut off--think SNES emulator savestate
  tmpFiles          = outDir+'TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361' + $
                      ['_1','_2','_3']+'--footpoints_and_Bfield_ratios.sav'
  timeThing         = outDir+'TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361' + $
                      ['_1','_2','_3']+'--recalc_for_every_point--timeStrings.sav'

  TIC
  clock = TIC('warnMe')
  FOR i=1,N_ELEMENTS(coordFiles)-1 DO BEGIN

     ;;Load the stuff we need (has GEO coords)
     PRINT,"Restoring " + coordFiles[i] + ' ...'
     RESTORE,eSpecDir+coordFiles[i]

     restrict_ii     = WHERE(eSpec_GEO.alt LE altitude_max,nAltitude)

     ;;Get indices into eSpec
     inds            = ephemFileIndArr[i,*]
     eSpec_i         = [inds[0]:inds[1]]

     IF nAltitude EQ 0 THEN BEGIN
        PRINT,'What?? No indices meeting these qualifications????'
        STOP
     ENDIF

     eSTTemp         = eSpecCoords.time[restrict_ii]

     ;;Check if we've already got it
     IF FILE_TEST(outDir+outFiles[i]) AND KEYWORD_SET(check_if_exists) THEN BEGIN

        PRINT,"File exists:" + outFiles[i]
        PRINT,"Checking for completeness ..."

        magRatio  = !NULL

        RESTORE,outDir+outFiles[i]

        IF N_ELEMENTS(magRatio) GT 0 THEN BEGIN
           IF N_ELEMENTS(magRatio.alt) EQ N_ELEMENTS(eSTTemp) THEN BEGIN
              PRINT,"File already complete:" + outFiles[i]
              PRINT,"Skipping ..."
              CONTINUE
           ENDIF
        ENDIF
     ENDIF

     ;; eSTTemp         = eSpec_GEO[restrict_ii]

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Some need-to-knowables
     nTot                 = N_ELEMENTS(eSTTemp)
     eEphem_GEOSph_arr    = TRANSPOSE([[eSpec_GEO.lat[restrict_ii]],[eSpec_GEO.lon[restrict_ii]],[eSpec_GEO.alt[restrict_ii]]])
     eEphem_Bfield_arr    = MAKE_ARRAY(4,nTot,/FLOAT) ;;One more for MLT at end

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Times in CDF epoch time
     nTot          = N_ELEMENTS(esTTemp)

     ;;Do we already have them?
     IF FILE_TEST(timething[i]) THEN BEGIN
        RESTORE,timeThing[i]
        IF N_ELEMENTS(esTTempStr) NE nTot THEN BEGIN
           PRINT,'Wrong number of elements!! Re-converting time stamps ...'
           recalcTime       = 1
        ENDIF ELSE BEGIN
           hateIt           = 0
           ;; IF savedAltitude_max NE altitude_max THEN BEGIN
           ;;    PRINT,"Saved altmax (" + STRCOMPRESS(savedAltitude_max,/REMOVE_ALL) + " doesn't match! Recalculating timestamps..."
           ;;    recalcTime    = 1
           ;;    hateIt        = 1
           ;; ENDIF
           ;; IF savedR_E NE R_E THEN BEGIN
           ;;    PRINT,"Saved R_E (" + STRCOMPRESS(savedR_E,/REMOVE_ALL) + " doesn't match! Recalculating timestamps..."
           ;;    recalcTime    = 1
           ;;    hateIt        = 1
           ;; ENDIF
           ;; IF savedAllow_fl_trace NE allow_fl_trace THEN BEGIN
           ;;    PRINT,"Saved allow_fl_trace (" + STRCOMPRESS(savedAllow_fl_trace,/REMOVE_ALL) + " doesn't match! Recalculating timestamps..."
           ;;    recalcTime    = 1
           ;;    hateIt        = 1
           ;; ENDIF

           PRINT,'Using already-converted timestamps from ' + timeThing[i] + ' ...'
           ;; PRINT,"Kidding! Continuing ..."
           ;; CONTINUE
           IF ~KEYWORD_SET(hateIt) THEN BEGIN
              recalcTime    = 0 
           ENDIF

        ENDELSE
     ENDIF ELSE BEGIN
        recalcTime          = 1
     ENDELSE

     ;;Convert
     IF recalcTime THEN BEGIN
        divFactor           = 10000
        esTTempStr    = MAKE_ARRAY(nTot,/STRING)
        FOR kk=0,(nTot/divFactor) DO BEGIN
           ind1       = kk*divFactor
           ind2       = ( ((kk+1)*divFactor) < (nTot - 1) )
           PRINT,'Inds: ' + STRCOMPRESS(ind1,/REMOVE_ALL) + ', ' + STRCOMPRESS(ind2,/REMOVE_ALL)
           tempI      = [ind1:ind2]
           esTTempStr[tempI] = TIME_TO_STR(esTTemp[tempI],/MSEC)
        ENDFOR
        PRINT,'Saving time strings to ' + timeThing[i] + ' ...'
        savedAltitude_max   = altitude_max
        savedR_E            = R_E
        savedAllow_fl_trace = allow_fl_trace
        SAVE,esttempstr,savedAltitude_max,savedR_E,savedAllow_fl_trace,FILENAME=timeThing[i]
     ENDIF

     finalYearArr       = FIX(STRMID(esTTempStr,0,4))
     finalMonthArr      = FIX(STRMID(esTTempStr,5,2))
     finalDayArr        = FIX(STRMID(esTTempStr,8,2))
     finalHourArr       = FIX(STRMID(esTTempStr,11,2))
     finalMinArr        = FIX(STRMID(esTTempStr,14,2))
     finalSecArr        = FLOAT(STRMID(esTTempStr,17,6))

     ;;Free up dat mem
     ;; eSpec              = !NULL
     eSpecCoords        = !NULL
     eSpec_GEO          = !NULL
     eSpec_GSM          = !NULL

     PRINT,"Feeding it to GEOPACK ..."
     IF FILE_TEST(tmpFiles[i]) THEN BEGIN
        PRINT,"Restoring " + tmpFiles[i] + ' ...'
        RESTORE,tmpFiles[i]
        PRINT,'Restored params'
        PRINT,'==============='
        PRINT,"nGotEm        : " + STRCOMPRESS(nGotEm,/REMOVE_ALL)
        PRINT,"lastCheck     : " + STRCOMPRESS(lastCheck,/REMOVE_ALL)
        PRINT,"checkInterval : " + STRCOMPRESS(checkInterval,/REMOVE_ALL)
        
        startK        = k

        PRINT,""
        PRINT,"Starting from k = " + STRCOMPRESS(startK,/REMOVE_ALL) + " ..."
     ENDIF ELSE BEGIN
        nGotEm        = 0
        lastCheck     = 0
        checkInterval = 100000
        startK        = 0
     ENDELSE


     TIC
     runCName = "GEOPACK B-field Clock"
     runC     = TIC(runCName)
     ;; GEOPACK_CONV_COORD
     ;; Description: Convert between a variety of commonly used coordinate systems.
     ;; Calling Sequence: geopack_conv_coord(_08), s1, s2, s3, d1, d2, d3.
     ;; Inputs: s1, s2, s3: Coordinates in system of origin.
     ;; Outputs: d1, d2, d3: Coordinates in target system.
     ;; Keywords: FROM_GEO: Specify source in geopgraphic coordinates. 
     ;;  FROM_MAG: Specify source in geomagnetic coordinates.
     ;;  FROM_GEI: Specify source in geocentric equatorial inertial coordinates.
     ;;  FROM_SM: Specify source in solar magnetic coordinates.
     ;;  FROM_GSM: Specify source in geocentric solar magnetospheric
     ;;  coordinates.
     ;;  FROM_GSE: Specify source in geocentric solar ecliptic coordinates.
     ;;  TO_GEO: Specify destination in geopgraphic coordinates.
     ;;  TO_MAG: Specify destination in geomagnetic coordinates.
     ;;  TO_GEI: Specify destination in geocentric equatorial inertial coordinates.
     ;;  TO_SM: Specify destination in solar magnetic coordinates.
     ;;  TO_GSM: Specify destination in geocentric solar magnetospheric
     ;;  coordinates. 

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Times in CDF epoch time
     time_epoch           = UTC_TO_CDF_EPOCH(esTTemp)

     ;; YearArr              = FIX(STRMID(fastLoc.time,0,4))
     ;; MonthArr             = FIX(STRMID(fastLoc.time,5,2))
     ;; DayArr               = FIX(STRMID(fastLoc.time,8,2))
     ;; HourArr              = FIX(STRMID(fastLoc.time,11,2))
     ;; MinArr               = FIX(STRMID(fastLoc.time,14,2))
     ;; SecArr               = FLOAT(STRMID(fastLoc.time,17,6))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;feed it to GEOPACK
     nTot                  = N_ELEMENTS(esTTemp)
     eEphem_GSM_arr        = MAKE_ARRAY(3,nTot,/FLOAT)
     eEphem_Bfield_arr     = MAKE_ARRAY(3,nTot,/FLOAT)
     eEphem_GSMSph_arr     = MAKE_ARRAY(3,nTot,/FLOAT)
     eEphem_BfieldSph_arr  = MAKE_ARRAY(3,nTot,/FLOAT)

     PRINT,"Feeding it to GEOPACK ..."
     FOR k=startK,nTot-1 DO BEGIN

        tmpTime           = TIME_TO_STR(esTTemp[k])
        YearArr           = FIX(STRMID(tmpTime,0,4))
        MonthArr          = FIX(STRMID(tmpTime,5,2))
        DayArr            = FIX(STRMID(tmpTime,8,2))
        HourArr           = FIX(STRMID(tmpTime,11,2))
        MinArr            = FIX(STRMID(tmpTime,14,2))
        SecArr            = FLOAT(STRMID(tmpTime,17,6))

        ;; GEOPACK_RECALC,YearArr[k],MonthArr[k],DayArr[k],HourArr[k],MinArr[k],SecArr[k],/DATE
        GEOPACK_RECALC,YearArr,MonthArr,DayArr,HourArr,MinArr,SecArr,/DATE

        ;;do that dance
        ;;To GSM
        GEOPACK_CONV_COORD,eEphem_GEO_arr[k,0],eEphem_GEO_arr[k,1],eEphem_GEO_arr[k,2], $
                           faPosgsm_x,faPosgsm_y,faPosgsm_z, $
                           /FROM_GEI,/TO_GSM,EPOCH=time_epoch[k]
        ;;To Bfield
        GEOPACK_IGRF_GEO_08,eEphem_GEOSph_arr[k,0],eEphem_GEOSph_arr[k,1],eEphem_GEOSph_arr[k,2], $
                            br,btheta,bphi, $
                            /FROM_GEI,/TO_GEO,EPOCH=time_epoch[k]

        ;;Trace field line
        ;; GEOPACK_TRACE,faposgsm_x,faposgsm_y,faposgsm_z

        ;;update
        eEphem_GSM_arr[*,i] = [faPosgsm_x,faPosgsm_y,faPosgsm_z]
        eEphem_Bfield_arr[*,i] = [br,btheta,bphi]


        GEOPACK_SPHCAR,faPosgsm_x,faPosgsm_y,faPosgsm_z,gsm_r,gsm_theta,gsm_phi,/TO_SPHERE,/DEGREE

        ;;Lat, long, height
        ;; eEphem_GSMSph_arr[*,i]    = [gsm_theta,gsm_phi,gsm_r] 
        eEphem_BfieldSph_arr[*,i]    = [geo_theta,geo_phi,geo_r] 
        ;;update
        ;; TiltArr    = [TiltArr,tempTilt]
        ;; eEphem_GSM_arr[*,i] = [faPosgsm_x,faPosgsm_y,faPosgsm_z]
        eEphem_Bfield_arr[*,i] = [br,btheta,bphi]


        nGotEm++

        
        IF nGotEm GE (lastCheck+checkInterval) THEN BEGIN
           PRINT,"N completed : " + STRCOMPRESS(nGotEm,/REMOVE_ALL)
           TOC,runC
           lastCheck += checkInterval

           SAVE,eEphem_Bfield_arr,lastCheck,checkInterval,nGotEm,k,FILENAME=tmpFiles[k]
        ENDIF
     ENDFOR
     TOC

     eEphem_Bfield_arr[2,*]     = (eEphem_Bfield_arr[2,*]*R_E-R_E) ;convert back to altitude above sea level

     magRatio   = {ALT:REFORM(eEphem_Bfield_arr[2,*]), $
                      MLT:REFORM(eEphem_Bfield_arr[3,*]), $
                      LAT:REFORM(eEphem_Bfield_arr[0,*])}

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Save it
     PRINT,'Saving ' + outDir + outFiles[i] + '...'
     SAVE,magRatio,restrict_ii,eSpec_i,FILENAME=outDir+outFiles[i]

     PRINT,"Did it! Finished with loop " + STRCOMPRESS(i+1,/REMOVE_ALL) + '/' + $
           STRCOMPRESS(N_ELEMENTS(coordFiles),/REMOVE_ALL)

     TOC

  ENDFOR

END


