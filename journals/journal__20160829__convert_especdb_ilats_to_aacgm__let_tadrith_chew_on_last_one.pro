;;2016/08/29
;;Please compile AACGM lib before running: IDL> @compile_aacgm.pro
;;
;;This one is just for Tadrith. Thelonious will handle one and two, and Tadrith can
;;handle #3.
PRO JOURNAL__20160829__CONVERT_ESPECDB_ILATS_TO_AACGM__LET_TADRITH_CHEW_ON_LAST_ONE

  COMPILE_OPT IDL2,STRICTARRSUBS

  orig_routineName  = 'JOURNAL__20160827__CONVERT_ESPECDB_ILATS_TO_AACGM__ALL_OF_EM'

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


  ;;NICE AND CONSPICUOUS
  tadrithStart_i    = N_ELEMENTS(coordFiles)-1
  REALcheckInterval = 10000

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;output
  outDir            = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  outFiles          = 'sorted--eSpec_20160607_db--Orbs_500-16361--AACGM_v2_coords' + $
                      ['_1','_2','_3']+'--recalc_for_every_point.sav'

  ;;In case we get cut off--think SNES emulator savestate
  tmpFiles          = outDir+'TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361' + $
                      ['_1','_2','_3']+'--recalc_for_every_point.sav'
  timeThing         = outDir+'TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361' + $
                      ['_1','_2','_3']+'--recalc_for_every_point--timeStrings.sav'

  TIC
  clock = TIC('warnMe')
  FOR i=tadrithStart_i,N_ELEMENTS(coordFiles)-1 DO BEGIN

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
        checkInterval = REALcheckInterval
        startK        = 0
     ENDELSE


     TIC
     runCName = "AACGM Clock"
     runC     = TIC(runCName)
     FOR k=startK,nTot-1 DO BEGIN 

        ;; checkEmOut  = WHERE(( esTTemp GE  time_utc[k]) AND ( esTTemp LE time_utc[k+1]),nCheckEm)

        ;; IF nCheckEm GT 0 THEN BEGIN

        e = AACGM_V2_SETDATETIME(finalYearArr[k],finalMonthArr[k],finalDayArr[k], $
                                 finalHourArr[k],finalMinArr[k],finalSecArr[k])

           ;; tmpAACGM                  = CNVCOORD_V2(eEphem_GEOSph_arr[*,k], $
           ;;                                         /ALLOW_TRACE)
           tmpAACGM                  = CNVCOORD_V2(eEphem_GEOSph_arr[*,k], $
                                                   ALLOW_TRACE=allow_fl_trace)
           AACGM_MLT                 = MLT_V2(tmpAACGM[1,*])
           eEphem_AACGMSph_arr[*,k]  = [tmpAACGM,AACGM_MLT]

           nGotEm++
        ;; ENDIF
        
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


