;;2016/08/03 Note the addition of realFile, since that's the one that is sorted. The other had a few dupes of unknown origin.
PRO JOURNAL__20160803__GET_ESPEC_STUFF_IN_GEO_COORDS

  COMPILE_OPT idl2

  eSpecDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'

  ;; outFile          = 'eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info--10to20mil.sav'
  outFile          = 'eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info--restavdebeste.sav'

  defNewellDBFile  = 'eSpec_20160607_db--PARSED--Orbs_500-16361.sav' 
  realFile         = 'sorted--' + defNewellDBFile ;;This file does not need to be cleaned

  ;;Load the stuff we need 
  PRINT,'Restoring file with sorted/uniq times'
  RESTORE,eSpecDir + realFile
  eSpecTimes       = eSpec.x
  eSpec            = !NULL

  ;; LOAD_NEWELL_ESPEC_DB,/JUST_TIMES,OUT_TIMES=eSpecTimes,/DONT_LOAD_IN_MEMORY

  inFileIndArr     = [[     0,1000000,20000000], $
                      [999999,1999999,28604344]]
  k                = 2

  PRINT,'Sending it to GET_FA_ORBIT ...'
  ;; GET_FA_ORBIT,eSpecTimes[10000000:19999999],/TIME_ARRAY,/ALL,/DEFINITIVE
  ;; GET_FA_ORBIT,eSpecTimes[20000000:-1],/TIME_ARRAY,/ALL,/DEFINITIVE
  inds        = inFileIndArr[k,*]
  GET_FA_ORBIT,eSpecTimes[inds[0]:inds[-1]],/TIME_ARRAY,/ALL,/DEFINITIVE
  
  GET_DATA,'ORBIT',DATA=orbit
  GET_DATA,'fa_pos',DATA=fa_pos
  GET_DATA,'ALT',DATA=alt
  GET_DATA,'ILAT',DATA=ilat
  GET_DATA,'ILNG',DATA=ilng
  GET_DATA,'LAT',DATA=lat
  GET_DATA,'LNG',DATA=lng
  GET_DATA,'fa_vel',DATA=fa_vel

  eSpecEphem   = {orbit:orbit.y, $
                  fa_pos:fa_pos.y, $
                  alt:alt.y, $
                  lat:lat.y, $
                  lng:lng.y, $
                  fa_vel:fa_vel.y, $
                  POS_AND_VEL_COORDS:'GEI (per GET_FA_ORBIT)'}

  PRINT,"Saving bonus ephem to " + outFile + " ... " 
  SAVE,eSpecEphem,FILENAME=eSpecDir+outFile

END