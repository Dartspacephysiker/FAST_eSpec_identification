;;06/06/16
;;I originally failed to check for false positives by failing to check the results of (WHERE())[0] EQ -1. You see some lines below
;;indicating that life is good.
PRO JOURNAL__20160606__ALIGN_ALFVENDB_I_WITH_HUGE_ISPEC_TIME_SERIES

  COMPILE_OPT IDL2

  ;;Stuff for iSpec database
  todayStr                  = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  despun                    = 0

  IF KEYWORD_SET(despun) THEN BEGIN
     despunStr              = '--despun'
     dbDate                 = '20160508'
     firstOrb               = 500
     orbFile                = 'Dartdb_20160508_despun--502-16361_despun--orbits.sav'
  ENDIF ELSE BEGIN
     despunStr              = ''
     dbDate                 = '20151222'
     firstOrb               = 500
     orbFile                = 'Dartdb_20151222--500-16361_inc_lower_lats--burst_1000-16361--orbits.sav'
  ENDELSE
  lastOrb                   = 16361

  ;;load maximus and cdbTime
  LOAD_MAXIMUS_AND_CDBTIME,!NULL,cdbTime,DBDir=dbDir,/JUST_CDBTIME,DO_DESPUNDB=despun

  ;;load alfven_orbList
  RESTORE,dbDir + orbFile

  inDir                   = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'
  inTimeSeriesFDate         = '20160606'
  inTimeSeriesFile        = STRING(FORMAT='("iSpec_",A0,"_db--TIME_SERIES_AND_ORBITS--Orbs_",I0,"-",I0,".sav")', $
                                   inTimeSeriesFDate, $
                                   firstOrb, $
                                   lastOrb)

  ;; inTimeSeriesFile          = STRING(FORMAT='("alf_iSpec_",A0,"_db",A0,"--TIME_SERIES_AND_ORBITS--Orbs_",I0,"-",I0,"--",A0,".sav")', $
  ;;                                    dbDate, $
  ;;                                    despunStr, $
  ;;                                    firstOrb, $
  ;;                                    lastOrb, $
  ;;                                    todayStr)

  RESTORE,inDir+inTimeSeriesFile

  orbChunkSize            = 100
  nChunks                 = (lastOrb-firstOrb)/orbChunkSize
  iSpec_orbPadding        = 1
  
  chunk_fName_pref        = STRING(FORMAT='("alf_iSpec_",A0,"_db",A0,"--ISPEC_TIMES_COINCIDING_WITH_ALFDB--")',dbDate,despunStr)
  TIC
  FOR iChunk=0,nChunks DO BEGIN

     startOrb             = firstOrb+iChunk*orbChunkSize
     stopOrb              = startOrb+orbChunkSize-1
     PRINT,FORMAT='("Orbs: ",I0,T20,I0)',startOrb,stopOrb
     temp_out_fname       = STRING(FORMAT='(A0,"Orbs_",I0,"-",I0,".sav")',chunk_fName_pref,startOrb,stopOrb)

     temp_alf_i           = WHERE(alfven_orbList GE startOrb AND alfven_orbList LE stopOrb,nTmpAlf)
     temp_alf_times       = cdbTime[temp_alf_i]

     ;;To check whether any of these are garbage (because I didn't do it up front!!!)
     ;;(AFTER CHECKING): Turns out they're all safe
     IF temp_alf_i[0] EQ -1 THEN BEGIN
        PRINT,"BEWARE: This orb range doesn't actually have squattum!"
        STOP
        ;; CONTINUE
     ENDIF

     temp_iSpec_i         = WHERE(orbArr_final GE (startOrb-iSpec_orbPadding) AND $
                                  orbArr_final LT (stopOrb+iSpec_orbPadding),nTmpISpec)
     temp_iSpec_times     = iSpec_times_final[temp_iSpec_i]
     
     
     clock                = TIC('Orbjunk' + STRCOMPRESS(iChunk,/REMOVE_ALL))
     temp_closest_ii      = VALUE_CLOSEST(temp_iSpec_times,temp_alf_times,closest_diffs,/BATCH_MODE)
     TOC,clock

     closest_iSpec_i      = temp_iSpec_i[TEMPORARY(temp_closest_ii)]

     PRINT,'Saving temp stuff...'
     SAVE,temp_alf_i,closest_iSpec_i,closest_diffs,FILENAME=inDir+temp_out_fname

     closest_iSpec_i      = !NULL
     temp_alf_i           = !NULL
     closest_diffs        = !NULL

  ENDFOR


END
