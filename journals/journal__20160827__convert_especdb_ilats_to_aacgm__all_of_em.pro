;;2016/08/27
;;Please compile AACGM lib before running: IDL> @compile_aacgm.pro
PRO JOURNAL__20160827__CONVERT_ESPECDB_ILATS_TO_AACGM__ALL_OF_EM

  COMPILE_OPT IDL2

  orig_routineName  = 'JOURNAL__20160827__CONVERT_ESPECDB_ILATS_TO_AACGM__ALL_OF_EM'

  R_E               = 6371.2D   ;Earth radius in km, from IGRFLIB_V2.pro

  altitude_max      = 4180      ;in km
  allow_fl_trace    = 1         ;Allow fieldline tracing for AACGM_v2?
  check_if_exists   = 1

  create_notAltitude_file = 1
  notAltitude_suff        = '--missed_indices--' + GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  convert_varnames_and_resave_outFiles = 1

  force_newCheckItvl = 1000
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Input

  ;;This is the eSpec DB being referenced, o'course
  eSpecDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  
  ;;Just for reference--we don't actually use these here
  ephemFileIndArr  = [[      0,10000000,20000000], $
                      [9999999,19999999,28604344]]

  coordFiles       = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords' + $
                     ['_1','_2','_3']+'.sav'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;output
  outDir           = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  outFiles         = 'sorted--eSpec_20160607_db--Orbs_500-16361--AACGM_v2_coords' + $
                     ['_1','_2','_3']+'--recalc_for_every_point.sav'

  ;;In case we get cut off--think SNES emulator savestate
  tmpFiles         = outDir+'TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361' + $
                     ['_1','_2','_3']+'--recalc_for_every_point.sav'
  timeFiles        = outDir+'TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361' + $
                     ['_1','_2','_3']+'--recalc_for_every_point--timeStrings.sav'

  ;;Convert these var names to standard names
  GEOSph_inName        = 'eEphem_GEOSph_arr'
  AACGMSph_inName      = 'eEphem_AACGMSph_arr'
  GEOStruct_inName     = 'eSpec_GEO'
  AACGMStruct_inName   = 'eSpec_AACGM'
  coordStruct_inName   = 'eSpecCoords'
  timeStr_inName       = 'eSTTempStr'

  defAACGMSphName      = 'AACGMSph'
  defAACGMStructName   = 'AACGMStruct'
  defVar3              = 'restrict_ii'
  defVar4              = 'eSpec_i'
  ;; defVar5              = 'notRestrict_ii'


  TIC
  clock = TIC('warnMe')
  FOR i=0,N_ELEMENTS(coordFiles)-1 DO BEGIN

     ;;Convert these var names to standard names
     GEOSphName        =   GEOSph_inName     
     AACGMSphName      =   AACGMSph_inName   
     GEOStructName     =   GEOStruct_inName  
     AACGMStructName   =   AACGMStruct_inName
     coordStructName   =   coordStruct_inName
     timeStrName       =   timeStr_inName    

     timeTmp = GET_TIMES_AND_DECIDE_ALTITUDE_OR_NOT_ALTITUDE(ephemFileIndArr,eSpec_i,i, $
                                                             ALTITUDE_MAX=altitude_max, $
                                                             R_E=R_E, $
                                                             ALLOW_FL_TRACE=allow_fl_trace, $
                                                             COORDFILES=coordFiles, $
                                                             ESPECDIR=eSpecDir, $
                                                             CREATE_NOTALTITUDE_FILE=create_notAltitude_file, $
                                                             NOTALTITUDE_SUFF=notAltitude_suff, $
                                                             OUTFILES=outFiles, $
                                                             TMPFILES=tmpFiles, $
                                                             TIMEFILES=timeFiles, $
                                                             RESTRICT_II=restrict_ii, $
                                                             NOTRESTRICT_II=notRestrict_ii, $
                                                             DOEM_II=doEm_ii, $
                                                             NAME__GEOSTRUCT=GEOStructName, $
                                                             NAME__COORDSTRUCT=coordStructName, $
                                                             GEOSTRUCT=geoStruct)

     ;;Check if we've already got it
     IF KEYWORD_SET(check_if_exists) THEN BEGIN
        IF KEYWORD_SET(convert_varnames_and_resave_outFiles) THEN BEGIN
           CONVERT_VARNAMES_AND_RESAVE_OUTFILES,outDir+outFiles[i], $
                                                AACGMSphName,defAACGMSphName, $
                                                AACGMStructName,defAACGMStructName, $
                                                defVar3,defVar3, $
                                                defVar4,defVar4
                                                ;; GEOSPHNAME=GEOSphName, $
                                                ;; AACGMSPHNAME=AACGMSphName, $
                                                ;; ;; NAME__GEOSTRUCT=GEOStructName, $
                                                ;; NAME__AACGMSTRUCT=AACGMStructName
        ENDIF
        IF CHECK_EXISTS_AND_COMPLETENESS(outDir,outFiles,timeTmp,i,NAME__AACGMSTRUCT=AACGMStructName) THEN BEGIN
           CONTINUE
        ENDIF
     ENDIF

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Some need-to-knowables
     nTot = N_ELEMENTS(timeTmp)
     MAKE_GEO_AND_AACGM_SPHCOORD_ARRAYS,timeTmp,nTot,GEOSph,AACGMSph,doEm_ii, $
                                        GEOSTRUCT=geoStruct, $
                                        /DESTROY_GEOSTRUCT


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Times in CDF epoch time
     IF CHECK_NEED_TO_RECALC_TSTAMPS(timeFiles[i],timeTmp,altitude_max, $
                                     ALTITUDE_MAX=altitude_max, $
                                     R_E=R_E, $
                                     ALLOW_FL_TRACE=allow_fl_trace, $
                                     OUT_TIMETMPSTR=timeTmpStr, $
                                     TIMESTRNAME=timeStrName, $
                                     CONVERT_VARNAMES_AND_RESAVE_OUTFILES=convert_varnames_and_resave_outFiles) $
     THEN BEGIN
        timeTmpStr = RECALC_TSTAMPS(timeTmp,nTot,timeFiles[i], $
                                    ALTITUDE_MAX=altitude_max, $
                                    R_E=R_E, $
                                    ALLOW_FL_TRACE=allow_fl_trace)
     ENDIF

     CHOP_UTC_STRINGS_INTO_YMD_HMS,timeTmpStr,year,month,day,hour,min,sec

     PRINT,"Feeding it to AACGM ..."
     IF FILE_TEST(tmpFiles[i]) THEN BEGIN
        PRINT,"Restoring " + tmpFiles[i] + ' ...'
        RESTORE,tmpFiles[i]
        PRINT,'Restored params'
        PRINT,'==============='
        PRINT,"nGotEm        : " + STRCOMPRESS(nGotEm,/REMOVE_ALL)
        PRINT,"lastCheck     : " + STRCOMPRESS(lastCheck,/REMOVE_ALL)
        PRINT,"checkInterval : " + STRCOMPRESS(checkInterval,/REMOVE_ALL)
        
        IF KEYWORD_SET(force_newCheckItvl) THEN BEGIN
           PRINT,"Forcing new check interval: " + STRCOMPRESS(force_newCheckItvl,/REMOVE_ALL)
           checkInterval = force_newCheckItvl
        ENDIF

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
     runCName = "AACGM Clock"
     runC     = TIC(runCName)
     FOR k=startK,nTot-1 DO BEGIN 

        e = AACGM_V2_SETDATETIME(year[k],month[k],day[k], $
                                 hour[k],min[k],sec[k])

        tmpAACGM                  = CNVCOORD_V2(geoSph[*,k], $
                                                ALLOW_TRACE=allow_fl_trace)
        AACGM_MLT                 = MLT_V2(tmpAACGM[1,*])
        AACGMSph[*,k]  = [tmpAACGM,AACGM_MLT]

        nGotEm++
        
        IF nGotEm GE (lastCheck+checkInterval) THEN BEGIN
           PRINT,"N completed : " + STRCOMPRESS(nGotEm,/REMOVE_ALL)
           TOC,runC
           lastCheck += checkInterval

           SAVE,AACGMSph,lastCheck,checkInterval,nGotEm,k,FILENAME=tmpFiles[i]
        ENDIF

     ENDFOR
     TOC

     AACGMSph[2,*]     = (AACGMSph[2,*]*R_E-R_E) ;convert back to altitude above sea level

     AACGMStruct   = {ALT:REFORM(AACGMSph[2,*]), $
                      MLT:REFORM(AACGMSph[3,*]), $
                      LAT:REFORM(AACGMSph[0,*])}

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Save it
     PRINT,'Saving ' + outDir + outFiles[i] + '...'
     SAVE,AACGMStruct,AACGMSph,restrict_ii,notRestrict_ii,eSpec_i,FILENAME=outDir+outFiles[i]

     PRINT,"Did it! Finished with loop " + STRCOMPRESS(i+1,/REMOVE_ALL) + '/' + $
           STRCOMPRESS(N_ELEMENTS(coordFiles),/REMOVE_ALL)

     TOC

  ENDFOR

END

FUNCTION GET_TIMES_AND_DECIDE_ALTITUDE_OR_NOT_ALTITUDE,ephemFileIndArr,eSpec_i,i, $
   ALTITUDE_MAX=altitude_max, $
   R_E=R_E, $
   ALLOW_FL_TRACE=allow_fl_trace, $
   COORDFILES=coordFiles, $
   ESPECDIR=eSpecDir, $
   CREATE_NOTALTITUDE_FILE=create_notAltitude_file, $
   NOTALTITUDE_SUFF=notAltitude_suff, $
   OUTFILES=outFiles, $
   TMPFILES=tmpFiles, $
   TIMEFILES=timeFiles, $
   RESTRICT_II=restrict_ii, $
   NOTRESTRICT_II=notRestrict_ii, $
   DOEM_II=doEm_ii, $
   NAME__GEOSTRUCT=GEOStructName, $
   NAME__COORDSTRUCT=coordStructName, $
   GEOSTRUCT=geoStruct

  ;;Load the stuff we need (has GEO coords)
  PRINT,"Restoring " + coordFiles[i] + ' ...'
  RESTORE,eSpecDir+coordFiles[i]

  ;;Convert varname, if necessary
  geoString   = 'GEOstruct   = TEMPORARY(' + GEOStructName   + ')'
  coordString = 'coordStruct = TEMPORARY(' + coordStructName + ')'
  IF ~(EXECUTE(geoString) AND EXECUTE(coordString)) THEN STOP

  restrict_ii  = WHERE(GEOstruct.alt LE altitude_max,nAltitude, $
                       COMPLEMENT=notRestrict_ii,NCOMPLEMENT=nNotAltitude)

  IF KEYWORD_SET(create_notAltitude_file) THEN BEGIN
     PRINT,"Doing 'notAltitude' files ..."
     doEm_ii    = notRestrict_ii
     outFiles  += notAltitude_suff
     tmpFiles  += notAltitude_suff
     timeFiles += notAltitude_suff
     nCheck     = nNotAltitude
  ENDIF ELSE BEGIN
     doEm_ii    = restrict_ii
     nCheck     = nAltitude
  ENDELSE

  ;;Get indices into eSpec
  inds         = ephemFileIndArr[i,*]
  eSpec_i      = [inds[0]:inds[1]]

  IF nCheck EQ 0 THEN BEGIN
     PRINT,'What?? No indices meeting these qualifications????'
     STOP
  ENDIF

  RETURN,coordStruct.time[doEm_ii]

END

FUNCTION CHECK_EXISTS_AND_COMPLETENESS,outDir,outFiles,timeTmp,i, $
                                       NAME__AACGMSTRUCT=AACGMStructName

  IF FILE_TEST(outDir+outFiles[i]) THEN BEGIN

     PRINT,"File exists:" + outFiles[i]
     PRINT,"Checking for completeness ..."

     AACGMStruct  = !NULL

     RESTORE,outDir+outFiles[i]

     IF ~EXECUTE('AACGMStruct = ' + AACGMStructName) THEN STOP

     IF N_ELEMENTS(AACGMStruct) GT 0 THEN BEGIN
        IF N_ELEMENTS(AACGMStruct.alt) EQ N_ELEMENTS(timeTmp) THEN BEGIN
           PRINT,"File already complete:" + outFiles[i]
           PRINT,"Skipping ..."
           RETURN,1
        ENDIF ELSE BEGIN
           PRINT,FORMAT='(A0,I0,"/",I0,A0)', $
                 "So file ISN'T complete yet! Vi har ", $
                 N_ELEMENTS(AACGMStruct.alt), $
                 N_ELEMENTS(timeTmp), $
                 " so far."
        ENDELSE
     ENDIF ELSE BEGIN
        PRINT,"Haven't even started this file, children. Here we go."
     ENDELSE
  ENDIF

  RETURN,0

END

PRO MAKE_GEO_AND_AACGM_SPHCOORD_ARRAYS,timeTmp,nTot,GEOSph,AACGMSph,doEm_ii, $
                                       GEOSTRUCT=GEOstruct, $
                                       DESTROY_GEOSTRUCT=destroy_GEOstruct
                                       

  GEOSph   = TRANSPOSE([[GEOstruct.lat[doEm_ii]],[GEOstruct.lon[doEm_ii]],[GEOstruct.alt[doEm_ii]]])
  AACGMSph = MAKE_ARRAY(4,nTot,/FLOAT) ;;One more for MLT at end

  IF KEYWORD_SET(destroy_GEOstruct) THEN GEOstruct = !NULL
END

FUNCTION CHECK_NEED_TO_RECALC_TSTAMPS,timeFile,timeTmp,nTot, $
                                      ALTITUDE_MAX=altitude_max, $
                                      R_E=R_E, $
                                      ALLOW_FL_TRACE=allow_fl_trace, $
                                      OUT_TIMETMPSTR=timeTmpStr, $
                                      TIMESTRNAME=timeStrName, $
                                      CONVERT_VARNAMES_AND_RESAVE_OUTFILES=convert_varnames_and_resave_outFiles

  nTot          = N_ELEMENTS(timeTmp)

  defTimeStrName = 'timeTmpStr'

  ;;Do we already have them?
  IF FILE_TEST(timeFile) THEN BEGIN
     RESTORE,timeFile

     IF KEYWORD_SET(convert_varnames_and_resave_outFiles) THEN BEGIN
        IF ~EXECUTE(defTimeStrName + ' = TEMPORARY(' + timeStrName + ')') THEN STOP
        PRINT,"Re-saving "  + timeFile + " ..."
        SAVE,timeTmpStr,savedAltitude_max,savedR_E,savedAllow_fl_trace,FILENAME=timeFile
     ENDIF

     IF N_ELEMENTS(timeTmpStr) NE nTot THEN BEGIN
        PRINT,'Wrong number of elements!! Re-converting time stamps ...'
        recalcTime       = 1
     ENDIF ELSE BEGIN
        hateIt           = 0
        IF savedAltitude_max NE altitude_max THEN BEGIN
           PRINT,"Saved altmax (" + STRCOMPRESS(savedAltitude_max,/REMOVE_ALL) + " doesn't match! Recalculating timestamps..."
           recalcTime    = 1
           hateIt        = 1
        ENDIF
        IF savedR_E NE R_E THEN BEGIN
           PRINT,"Saved R_E (" + STRCOMPRESS(savedR_E,/REMOVE_ALL) + " doesn't match! Recalculating timestamps..."
           recalcTime    = 1
           hateIt        = 1
        ENDIF
        IF savedAllow_fl_trace NE allow_fl_trace THEN BEGIN
           PRINT,"Saved allow_fl_trace (" + STRCOMPRESS(savedAllow_fl_trace,/REMOVE_ALL) + " doesn't match! Recalculating timestamps..."
           recalcTime    = 1
           hateIt        = 1
        ENDIF

        PRINT,'Using already-converted timestamps from ' + timeFile + ' ...'
        ;; PRINT,"Kidding! Continuing ..."
        ;; CONTINUE
        IF ~KEYWORD_SET(hateIt) THEN BEGIN
           recalcTime    = 0 
        ENDIF

     ENDELSE
  ENDIF ELSE BEGIN
     recalcTime          = 1
  ENDELSE

  RETURN,recalcTime
END

FUNCTION RECALC_TSTAMPS,timeTmp,nTot,timeFile, $                
                        ALTITUDE_MAX=altitude_max, $
                        R_E=R_E, $
                        ALLOW_FL_TRACE=allow_fl_trace

  ;;Convert
  divFactor           = 10000
  timeTmpStr    = MAKE_ARRAY(nTot,/STRING)
  FOR kk=0,(nTot/divFactor) DO BEGIN
     ind1       = kk*divFactor
     ind2       = ( ((kk+1)*divFactor) < (nTot - 1) )
     PRINT,'Inds: ' + STRCOMPRESS(ind1,/REMOVE_ALL) + ', ' + STRCOMPRESS(ind2,/REMOVE_ALL)
     tempI      = [ind1:ind2]
     timeTmpStr[tempI] = TIME_TO_STR(timeTmp[tempI],/MSEC)
  ENDFOR
  PRINT,'Saving time strings to ' + timeFile + ' ...'
  savedAltitude_max   = altitude_max
  savedR_E            = R_E
  savedAllow_fl_trace = allow_fl_trace
  SAVE,timeTmpstr,savedAltitude_max,savedR_E,savedAllow_fl_trace,FILENAME=timeFile

  RETURN,timeTmpStr

END

PRO CHOP_UTC_STRINGS_INTO_YMD_HMS,timeTmpStr,year,month,day,hour,min,sec

  year   = FIX(STRMID(timeTmpStr,0,4))
  month  = FIX(STRMID(timeTmpStr,5,2))
  day    = FIX(STRMID(timeTmpStr,8,2))
  hour   = FIX(STRMID(timeTmpStr,11,2))
  min    = FIX(STRMID(timeTmpStr,14,2))
  sec    = FLOAT(STRMID(timeTmpStr,17,6))

END
