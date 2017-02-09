;;02/09/17
PRO JOURNAL__20170209__LOOK_FOR_GAPS_IN_IESA_DB_AND_REMOVE

  COMPILE_OPT IDL2

  startOrb    = 500             ;Otherwise it just picks the first orbit in eSpec
  showPlots   = 0
  savePS      = 0
  PSDir       = '/SPENCEdata/Research/Satellites/FAST/espec_identification/plots/201702--trim_transitions/'
  PSPref      = 'junk_transitions--'
  

  ;; filePref    = "esa_transit_times--"
  saveDir     = '/SPENCEdata/Research/database/FAST/dartdb/ion_db/'
  suffDir     = 'instrument_oddity_times/'
  saveDir    += suffDir

  ;; @common__newell_espec.pro

  ;; ;; LOAD_NEWELL_ESPEC_DB,eSpec,!NULL,NEWELL__delta_t, $
  ;; ;; /LOAD_DELTA_T, $
  ;; ;; /NO_MEMORY_LOAD
  ;; IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
  ;;    LOAD_NEWELL_ION_DB,!NULL,!NULL,!NULL, $
  ;;                         /DONT_MAP_TO_100KM, $
  ;;                         /DO_NOT_MAP_DELTA_T, $
  ;;                         /DONT_CONVERT_TO_STRICT_NEWELL
  ;;                         ;; /LOAD_DELTA_T, $
                          
  ;; ENDIF
  ;; CHECK_SORTED,NEWELL__eSpec.orbit,is_sorted,/QUIET

  prefSuff       = '__oddity_times--'
  saveFilePref   = 'downgoing_ions__ephem__v0_1'
  saveFilePref  += prefSuff

  RESTORE,'/SPENCEdata/Research/database/FAST/dartdb/ion_db/downgoing_ions__ephem__v0_1.sav'

  CHECK_SORTED,ephem.x,is_sorted,/QUIET
  IF ~is_sorted THEN STOP
  
  info           = ephem.info
  ilat           = ephem.ilat
  times          = ephem.x
  orbit           = (TEMPORARY(ephem)).orbit

  IDENTIFY_GAPS_IN_EESA_OR_IESA_TSERIES, $
     TIMES_UTC=times, $
     ORBIT_ARRAY=orbit, $
     ILAT_ARRAY=ilat, $
     INFO_FOR_STRUCT=info, $
     STARTORB=startOrb, $
     SAVEFILEPREF=saveFilePref, $
     SAVEDIR=saveDir, $
     SHOWPLOTS=showPlots, $
     SAVEPS=savePS, $
     PSDIR=PSDir, $
     PSPREF=PSPref

END
