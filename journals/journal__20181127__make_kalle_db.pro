;2018/11/27
PRO JOURNAL__20181127__MAKE_KALLE_DB

  COMPILE_OPT IDL2,STRICTARRSUBS


  outDir       = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/v2018/'

  startOrb     = 1000
  stopOrb      = 51315
  ;; Last file is /media/spencerh/data/FAST_electrons/electron_moments_and_spectral_identification__Orbit_51315.sav, tror jeg

  ;; todayStr = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  todayStr = '20181127'

  outPref = STRING(FORMAT='("eMomDB_",A0,"-",I0,"-",I0)', $
                   todayStr, $
                   startOrb, $
                   stopOrb)

  outFile = "FAST_energy_flux__orbits_1000-51315.sav"

  LCdbPref  = 'LCangle_moms'

  ALLdbPref  = 'ALLangle_moms'

  extradbPref = 'extra'

  ephemdbPref = 'ephem'


  LCDBStr = outPref + '-' + LCdbPref + '.sav'
  ALLDBStr = outPref + '-' + ALLdbPref + '.sav'
  extraDBStr = outPref + '-' + extradbPref + '.sav'
  ephemDBStr = outPref + '-' + ephemdbPref + '.sav'

  RESTORE,outDir+LCDBStr

  ;; keep j and je
  ;; j = lcangle_moms.j
  jeLC = lcangle_moms.je

  lcangle_moms = !NULL

  RESTORE,outDir+ALLDBStr

  ;; keep j and je
  ;; j = lcangle_moms.j
  jeALL = allAngle_moms.je

  allAngle_moms = !NULL

  RESTORE,outDir+extraDBStr

  fapos = extra.fa_pos
  favel = extra.fa_vel
  mapratio = extra.mapratio
  losscone = extra.losscone
  tdiffs = extra.tdiffs

  espec_bad_time = extra.espec_bad_time

  extra = !NULL

  RESTORE,outDir+ephemDBStr

  valid = ephem.valid
  time = ephem.time
  orbit = ephem.orbit
  
  ephem = !NULL

  fast = {time     : time     , $
          valid    : valid  AND ~espec_bad_time, $
          je_lc    : jelc     , $
          je_all   : jeAll    , $
          pos      : fapos    , $
          vel      : favel    , $
          mapratio : mapratio , $
          losscone : losscone , $
          tdiffs   : tdiffs   , $
          orbit    : orbit}

  
  PRINT,"Saving " + outFile + ' ...'
  SAVE,fast,FILENAME=outDir+outFile

  STOP

END
