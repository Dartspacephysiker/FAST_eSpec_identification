;;06/06/16
PRO JOURNAL__20160606__SINCE_SOME_RESULTS_ARE_BOGUS__CAST_THEM_INTO_THE_FIRE_AND_KEEP_THE_GOOD__IONS

  COMPILE_OPT IDL2

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

  inDir                     = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;The output
  winnowedFile              = STRING(FORMAT='("alf_iSpec_",A0,"_db",A0,"--TIME_SERIES_AND_ORBITS_ALIGNED_WITH_DB--WINNOWED--Orbs_",I0,"-",I0,"--",A0,".sav")', $
                                     dbDate, $
                                     despunStr, $
                                     firstDBOrb, $
                                     lastOrb, $
                                     todayStr)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;The inputs
  ;;THE MASTER IN FILE
  theMasterInFile           = STRING(FORMAT='("alf_iSpec_",A0,"_db",A0,"--TIME_SERIES_AND_ORBITS_ALIGNED_WITH_DB--Orbs_",I0,"-",I0,"--",A0,".sav")', $
                                     dbDate, $
                                     despunStr, $
                                     firstDBOrb, $
                                     lastOrb, $
                                     todayStr)

  ;;Use this for orbs
  inTimeSeriesFDate         = '20160606'
  inTimeSeriesFile        = STRING(FORMAT='("iSpec_",A0,"_db--TIME_SERIES_AND_ORBITS--Orbs_",I0,"-",I0,".sav")', $
                                   inTimeSeriesFDate, $
                                   firstOrb, $
                                   lastOrb)

  ;;load maximus and cdbTime
  LOAD_MAXIMUS_AND_CDBTIME,!NULL,cdbTime,DBDir=dbDir,/JUST_CDBTIME,DO_DESPUNDB=despun

  RESTORE,inDir+inTimeSeriesFile
  RESTORE,inDir+theMasterInFile

  good_ii                   = WHERE(ABS(closest_diffs_final) LE 5,nGood, $
                                    COMPLEMENT=bad_ii, $
                                    NCOMPLEMENT=nBad)
  
  alf_i__good_iSpec         = alf_i_final[good_ii]
  good_iSpec_i              = closest_iSpec_i_final[good_ii]
  good_iSpec_t              = closest_iSpec_t_final[good_ii]
  good_diffs                = closest_diffs_final[good_ii]
  good_orbs                 = orbArr_final[good_iSpec_i]
  good_orbs_unique          = good_orbs[UNIQ(good_orbs)]

  alf_i__bad_iSpec          = alf_i_final[bad_ii]
  bad_iSpec_i               = closest_iSpec_i_final[bad_ii]
  bad_iSpec_t               = closest_iSpec_t_final[bad_ii]
  bad_diffs                 = closest_diffs_final[bad_ii]
  bad_orbs                  = orbArr_final[bad_iSpec_i]
  bad_orbs_unique           = bad_orbs[UNIQ(bad_orbs)]


  bad                       = WHERE(ABS(cdbtime[alf_i__good_iSpec]-good_iSpec_t) GT 5,nBad,NCOMPLEMENT=nGood)

  PRINT,'nBad  in the goods : ' + STRCOMPRESS(nBad,/REMOVE_ALL)
  PRINT,'nGood in the goods : ' + STRCOMPRESS(nGood,/REMOVE_ALL)

  PRINT,'Saving winnowed stuff to ' + winnowedFile + '...'
  SAVE,alf_i__good_iSpec,good_iSpec_i,good_iSpec_t,good_diffs,good_orbs,good_orbs_unique, $
       alf_i__bad_iSpec,bad_iSpec_i,bad_iSpec_t,bad_diffs,bad_orbs,bad_orbs_unique, $
       FILENAME=inDir+winnowedFile
  
END
