;2018/10/08
PRO JOURNAL__20181008__COMPARE_NEW_STUFF_WITH_KNOWN_BAD_ORBITS

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; Look at GET_ORBRANGE_INDS or TRASH_BAD_FAST_ORBITS, and you're gonna find that there are lots of stinker orbits
  ;; 1. Let's try anything in the 1031-1035 range
  ;;    RESULT: Sheesh, yes, it's still very bad
  ;; 
  ;; 2. What about one with lots of transition garbage? Do 1197
  ;;    RESULT: Quoi
  

  ;; Load old DB
  @common__newell_espec.pro
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     ;; LOAD_NEWELL_ESPEC_DB,/DONT_CONVERT_TO_STRICT_NEWELL, $
     ;;                      ;; /NO_MEMORY_LOAD, $
     ;;                      /GIGANTE

     LOAD_NEWELL_ESPEC_DB,/DONT_CONVERT_TO_STRICT_NEWELL
  ENDIF
  
  newDir = '/thelonious_data1/FAST_electrons_2018/'
  newFile = 'electron_moments_and_spectral_identification__Orbit_1031.sav'

  PRINT,"Restoring " + newFile + ' ...'
  RESTORE,newDir+newFile

  ;; 1. Let's try anything in the 1031-1035 range
  these = WHERE(NEWELL__eSpec.orbit EQ 1031,nHere)

  PRINT,MEAN(ABS(etron.moments.all.je),/NAN)
  PRINT,ALOG10(MEAN(ABS(etron.moments.all.j),/NAN))
  ;; Garbage

  ;; 2. What about one with lots of transition garbage? Do 1197
  orbit = 1197
  newFile = 'electron_moments_and_spectral_identification__Orbit_' $
            + STRING(FORMAT='(I0)',orbit) $
            + '.sav'

  PRINT,"Restoring " + newFile + ' ...'
  RESTORE,newDir+newFile

  these = WHEREfOKetron

  (NEWELL__eSpec.orbit EQ orbit,nHere) = WHERE(eTron.valid AND ~extra.espec_bad_time)

  STOP

END
