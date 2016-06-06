;;06/06/16
PRO JOURNAL__20160606__NOW_STITCH_TOGETHER_THE_RESULTS_AND_MAKE_SURE_THEYRE_NOT_BOGUS__IONS

  COMPILE_OPT idl2

  despun                    = 0

  IF KEYWORD_SET(despun) THEN BEGIN
     despunStr              = '--despun'
     dbDate                 = '20160508'
     firstDBOrb             = 502
     orbFile                = 'Dartdb_20160508_despun--502-16361_despun--orbits.sav'
  ENDIF ELSE BEGIN
     despunStr              = ''
     dbDate                 = '20151222'
     firstDBOrb             = 500
     orbFile                = 'Dartdb_20151222--500-16361_inc_lower_lats--burst_1000-16361--orbits.sav'
  ENDELSE
  firstOrb                  = 500
  lastOrb                   = 16361

  todayStr                  = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  ;;THE MASTER OUT FILE
  theMasterOutFile          = STRING(FORMAT='("alf_iSpec_",A0,"_db",A0,"--TIME_SERIES_AND_ORBITS_ALIGNED_WITH_DB--Orbs_",I0,"-",I0,"--",A0,".sav")', $
                                     dbDate, $
                                     despunStr, $
                                     firstDBOrb, $
                                     lastOrb, $
                                     todayStr)

  ;;load maximus and cdbTime
  LOAD_MAXIMUS_AND_CDBTIME,!NULL,cdbTime,DBDir=dbDir,/JUST_CDBTIME,DO_DESPUNDB=despun

  ;;load alfven_orbList
  RESTORE,dbDir + orbFile

  inDir                     = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'
  inTimeSeriesFile        = STRING(FORMAT='("iSpec_",A0,"_db--TIME_SERIES_AND_ORBITS--Orbs_",I0,"-",I0,".sav")', $
                                   GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                                   firstOrb, $
                                   lastOrb)
  RESTORE,inDir+inTimeSeriesFile

  orbChunkSize              = 100
  nChunks                   = (lastOrb-firstOrb)/orbChunkSize
  ;; iSpec_orbPadding          = 1

  chunk_fName_pref        = STRING(FORMAT='("alf_iSpec_",A0,"_db",A0,"--ISPEC_TIMES_COINCIDING_WITH_ALFDB--")',dbDate,despunStr)
  TIC

  closest_iSpec_i_final     = !NULL
  closest_iSpec_t_final     = !NULL
  closest_diffs_final       = !NULL
  alf_i_final               = !NULL

  PRINT,FORMAT='("nBad",T15,"nGood",T30,"nTot")'
  FOR iChunk=0,nChunks DO BEGIN

     startOrb               = firstOrb+iChunk*orbChunkSize
     stopOrb                = startOrb+orbChunkSize-1
     PRINT,FORMAT='("Orbs: ",I0,T20,I0)',startOrb,stopOrb

     temp_out_fname         = STRING(FORMAT='(A0,"Orbs_",I0,"-",I0,".sav")',chunk_fName_pref,startOrb,stopOrb)

     closest_iSpec_i        = !NULL
     temp_alf_i             = !NULL
     closest_diffs          = !NULL
     RESTORE,inDir+temp_out_fname
     IF N_ELEMENTS(closest_iSpec_i) EQ 0 OR N_ELEMENTS(temp_alf_i) EQ 0 OR N_ELEMENTS(closest_diffs) EQ 0 THEN STOP
     tempDiff               = cdbtime[temp_alf_i]-iSpec_times_final[closest_iSpec_i]
     bad                    = WHERE(ABS(tempDiff) GT 5,nBad,NCOMPLEMENT=nGood)

     ;; IF nBad GT 100 THEN STOP

     PRINT,FORMAT='(I0,T15,I0,T30,I0)',nBad,nGood,nBad+nGood

     ;;**Clock**
     IF KEYWORD_SET(clockme) THEN BEGIN
        clock               = TIC('Stitchem--' + STRCOMPRESS(iChunk,/REMOVE_ALL))
     ENDIF
     ;;**Clock**

     closest_iSpec_i_final  = [closest_iSpec_i_final,closest_iSpec_i]
     closest_iSpec_t_final  = [closest_iSpec_t_final,iSpec_times_final[closest_iSpec_i]]
     closest_diffs_final    = [closest_diffs_final,closest_diffs]
     alf_i_final            = [alf_i_final,temp_alf_i]

     ;;**Clock**
     IF KEYWORD_SET(clockme) THEN TOC,clock ;want a clock?
     ;;**Clock**

  ENDFOR

  PRINT,"Save all dat!!"
  SAVE,closest_diffs_final,alf_i_final, $
       closest_iSpec_i_final,closest_iSpec_t_final, $
       FILENAME=inDir+theMasterOutFile

END
