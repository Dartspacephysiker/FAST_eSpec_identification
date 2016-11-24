;;11/24/16
;;This file, per JOURNAL__20161124__TRIM_FIRST_10_POINTS_FROM_EVERY_ORB_INTERVAL__RESAVE_ESPEC, is going to contain these guys:
;;cleaned_eSpec_i (LONG array)
;;eSpec_clean_info (way awesome struct)
FUNCTION GET_NEWELL_ESPEC_SAFED_INDS_FILE,eSpec, $
                                          NEWELLDBDIR=NewellDBDir, $
                                          STOP_IF_NOEXIST=stop_if_noExist

  COMPILE_OPT IDL2

  cleanedUpFile = NewellDBDir + $
                  'eSpecDB_' + $
                  eSpec.info.DB_DATE + '_' + $
                  (eSpec.info.DB_version).Replace('.','_') + '--' + $
                  eSpec.info.DB_extras.Replace('/','--') + $
                  '--cleaned_inds.sav'

  IF KEYWORD_SET(stop_if_noExist) THEN BEGIN
     IF ~FILE_TEST(cleanedUpFile) THEN BEGIN
        PRINT,"File doeesn't exist: " + cleanedUpFile
        PRINT,"What to do?"
        PRINT,"You know, if you visited JOURNAL__20161124__TRIM_FIRST_10_POINTS_FROM_EVERY_ORB_INTERVAL__RESAVE_ESPEC, you could probably get it handled."
        STOP
     ENDIF
  ENDIF

  RETURN,cleanedUpFile

END
