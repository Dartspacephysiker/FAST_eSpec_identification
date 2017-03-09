;;08/17/16
PRO JOURNAL__20160817__CONVERT_ESPECDB_ILATS_TO_AACGM__BELOW_2000_KM__EVERY_6_HOURS

  COMPILE_OPT IDL2,STRICTARRSUBS

  orig_routineName  = 'JOURNAL__20160817__CONVERT_ESPECDB_ILATS_TO_AACGM__BELOW_2000_KM'

  R_E               = 6371.2D    ;Earth radius in km, from IGRFLIB_V2.pro

  altitude_max      = 1990       ;in km
  allow_fl_trace    = 0          ;Allow fieldline tracing for AACGM_v2?
  check_if_exists   = 1
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Input

  ;;This is the eSpec DB being referenced, o'course
  eSpecDir          = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  
  ;; defNewellDBFile   = 'eSpec_20160607_db--PARSED--Orbs_500-16361.sav' 
  ;; realFile          = 'sorted--' + defNewellDBFile ;;This file does not need to be cleaned
  
  ephemFiles        = ['eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info.sav', $
                      'eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info--10to20mil.sav', $
                      'eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info--restavdebeste.sav']

  ;;Just for reference--we don't actually use these here
  ephemFileIndArr   = [[      0,10000000,20000000], $
                      [9999999,19999999,28604344]]

  coordFiles        = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords' + $
                      ['_1','_2','_3']+'.sav'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;output
  outDir            = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/AACGM_v2/'
  outFiles          = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords' + $
                      ['_1','_2','_3']+'.sav'

  ;;In case we get cut off--think SNES emulator savestate
  tmpFiles          = outDir+'TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km' + $
                      ['_1','_2','_3']+'.sav'

  TIC
  clock = TIC('warnMe')
  FOR i=2,N_ELEMENTS(coordFiles)-1 DO BEGIN

     ;;Load the stuff we need (has GEO coords)
     PRINT,"Restoring " + coordFiles[i] + ' ...'
     RESTORE,eSpecDir+coordFiles[i]

     ;; LOAD_NEWELL_ESPEC_DB,/JUST_TIMES, $
     ;;                      OUT_TIMES=eSpecTimes, $
     ;;                      /DONT_LOAD_IN_MEMORY, $
     ;;                      /DONT_PERFORM_CORRECTION

     ;; inds        = ephemFileIndArr[i,*]
     ;; temp_i      = [inds[0]:inds[1]]
     
     ;; restrict_ii  = CGSETINTERSECTION(WHERE(eSpec_GEO.alt LE altitude_max,nAltitude),temp_i,COUNT=nGood)
     ;; IF nAltitude EQ 0 OR nGood EQ 0 THEN BEGIN
     ;;    PRINT,'What?? No indices meeting these qualifications????'
     ;;    STOP
     ;; ENDIF

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

        eSpec_AACGM  = !NULL

        RESTORE,outDir+outFiles[i]

        IF N_ELEMENTS(eSpec_AACGM) GT 0 THEN BEGIN
           IF N_ELEMENTS(eSpec_AACGM.alt) EQ N_ELEMENTS(eSTTemp) THEN BEGIN
              PRINT,"File already complete:" + outFiles[i]
              PRINT,"Skipping ..."
              CONTINUE
           ENDIF
        ENDIF
     ENDIF

     ;; eSTTemp         = eSpec_GEO[restrict_ii]

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Some need-to-knowables
     nTot                   = N_ELEMENTS(eSTTemp)
     eEphem_GEOSph_arr      = TRANSPOSE([[eSpec_GEO.lat[restrict_ii]],[eSpec_GEO.lon[restrict_ii]],[eSpec_GEO.alt[restrict_ii]]])
     eEphem_AACGMSph_arr    = MAKE_ARRAY(4,nTot,/FLOAT) ;;One more for MLT at end

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;generate the dates
     startYear       = 1996
     stopYear        = 2001

     hourArr         = [6,12,18]

     PRINT,'Making time arrays ...'
     MAKE_TIME_ARRAYS, $
        IN_STARTYEAR=startYear, $
        IN_STOPYEAR=stopYear, $
        IN_HOURARR=hourArr, $
        OUT_YEARARR=finalYearArr, $
        OUT_DOYARR=finalDOYArr, $
        OUT_MONTHARR=finalMonthArr, $
        OUT_DAYARR=finalDayArr, $
        OUT_HOURARR=finalHourArr, $
        OUT_JULDAY=julDay

     time_utc           = JULDAY_TO_UTC(julDay)
     nTimeIntervals     = N_ELEMENTS(time_utc)

     time_epoch         = UTC_TO_CDF_EPOCH(time_utc)


     ;;Free up dat mem
     ;; eSpec              = !NULL
     eSpecCoords        = !NULL
     eSpec_GEO          = !NULL
     eSpec_MAG          = !NULL

     PRINT,"Feeding it to AACGM ..."
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
        minK    = MIN(ABS(time_utc-esTTemp[0]),startK)
        startK  = (startK EQ 0 ? 0 : startK-1)
     ENDELSE


     TIC
     runCName = "AACGM Clock"
     runC     = TIC(runCName)
     FOR k=startK,nTimeIntervals-2 DO BEGIN ;nTimeIntervals-2 because we use k and k+1 for each iteration

        checkEmOut  = WHERE(( esTTemp GE  time_utc[k]) AND ( esTTemp LE time_utc[k+1]),nCheckEm)

        IF nCheckEm GT 0 THEN BEGIN

           e = AACGM_V2_SETDATETIME(finalYearArr[k],finalMonthArr[k],finalDayArr[k], $
                                    finalHourArr[k],0,0)

           ;; tmpAACGM                  = CNVCOORD_V2(eEphem_GEOSph_arr[*,checkEmOut], $
           ;;                                         /ALLOW_TRACE)
           tmpAACGM                  = CNVCOORD_V2(eEphem_GEOSph_arr[*,checkEmOut], $
                                                   ALLOW_TRACE=allow_fl_trace)
           AACGM_MLT                 = MLT_V2(tmpAACGM[1,*])
           eEphem_AACGMSph_arr[*,checkEmOut]  = [tmpAACGM,AACGM_MLT]

           nGotEm   += nCheckEm
        ENDIF
        
        IF nGotEm GE (lastCheck+checkInterval) THEN BEGIN
           PRINT,"N completed : " + STRCOMPRESS(nGotEm,/REMOVE_ALL)
           TOC,runC
           lastCheck += checkInterval

           SAVE,eEphem_AACGMSph_arr,lastCheck,checkInterval,nGotEm,k,FILENAME=tmpFiles[i]
        ENDIF

     ENDFOR
     TOC

     eEphem_AACGMSph_arr[2,*]     = (eEphem_AACGMSph_arr[2,*]*R_E-R_E) ;convert back to altitude above sea level

     eSpec_AACGM   = {ALT:REFORM(eEphem_AACGMSph_arr[2,*]), $
                      MLT:REFORM(eEphem_AACGMSph_arr[3,*]), $
                      LAT:REFORM(eEphem_AACGMSph_arr[0,*])}

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Save it
     PRINT,'Saving ' + outDir + outFiles[i] + '...'
     SAVE,eSpec_AACGM,eEphem_AACGMSph_arr,restrict_ii,eSpec_i,FILENAME=outDir+outFiles[i]

     PRINT,"Did it! Finished with loop " + STRCOMPRESS(i+1,/REMOVE_ALL) + '/' + $
           STRCOMPRESS(N_ELEMENTS(coordFiles),/REMOVE_ALL)

     TOC

  ENDFOR

END
