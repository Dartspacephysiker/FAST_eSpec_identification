;;11/24/16
;;This file, per JOURNAL__20161124__TRIM_FIRST_10_POINTS_FROM_EVERY_ORB_INTERVAL__RESAVE_ESPEC, is going to contain these guys:
;;cleaned_eSpec_i (LONG array)
;;eSpec_clean_info (way awesome struct)
FUNCTION GET_NEWELL_ESPEC_SAFED_INDS_FILE,eSpec, $
                                          NEWELLDBDIR=NewellDBDir, $
                                          STOP_IF_NOEXIST=stop_if_noExist, $
                                          JUSTNAMEPLEASE=justNamePlease, $
                                          QUIET=quiet

  COMPILE_OPT IDL2,STRICTARRSUBS

  cleanedUpFile = NewellDBDir + $
                  GET_NEWELL_DB_STRING(eSpec) + $
                  '--cleaned_inds.sav'

  IF KEYWORD_SET(justNamePlease) THEN BEGIN
     RETURN,cleanedUpFile
  ENDIF

  file20160607 = STRMATCH(GET_NEWELL_DB_STRING(eSpec),'*20160607*',/FOLD_CASE)
  file20170203 = STRMATCH(GET_NEWELL_DB_STRING(eSpec),'*20170203*',/FOLD_CASE)

  CASE 1 OF
     KEYWORD_SET(file20160607): BEGIN
        
        ;; IF KEYWORD_SET(stop_if_noExist) THEN BEGIN
        ;;    IF ~FILE_TEST(cleanedUpFile) THEN BEGIN
        ;;       IF ~KEYWORD_SET(quiet) THEN PRINT,"File doeesn't exist: " + cleanedUpFile
        ;;       IF ~KEYWORD_SET(quiet) THEN PRINT,"What to do?"
        ;;       IF ~KEYWORD_SET(quiet) THEN PRINT,"You know, if you visited JOURNAL__20161124__TRIM_FIRST_10_POINTS_FROM_EVERY_ORB_INTERVAL__RESAVE_ESPEC, you could probably get it handled."
        ;;       STOP
        ;;    ENDIF
        ;; ENDIF
        
     END
     KEYWORD_SET(file20170203): BEGIN
        ;;Why doesn't he need it? Because he was formed based on dupeless Je times, which trimmed the first 10 points from every
        ;;orbit in any case
        cleanedUpFile = 'DontNeedItBro'
     END
     KEYWORD_SET(eSpec.info.is_final2018): BEGIN
        ;;Why doesn't he need it? Because he was formed based on dupeless Je times, which trimmed the first 10 points from every
        ;;orbit in any case
        cleanedUpFile = 'DontNeedItBro-useDB!'
     END
  ENDCASE

  IF KEYWORD_SET(stop_if_noExist) THEN BEGIN
     IF ~(FILE_TEST(cleanedUpFile) OR KEYWORD_SET(file20170203) OR KEYWORD_SET(eSpec.info.is_final2018)) THEN BEGIN
        IF ~KEYWORD_SET(quiet) THEN PRINT,"File doesn't exist: " + cleanedUpFile
        IF ~KEYWORD_SET(quiet) THEN PRINT,"What to do?"
        IF ~KEYWORD_SET(quiet) THEN PRINT,"You know, if you visited JOURNAL__20161124__TRIM_FIRST_10_POINTS_FROM_EVERY_ORB_INTERVAL__RESAVE_ESPEC, you could probably get it handled."
        STOP
     ENDIF
  ENDIF

  IF ~KEYWORD_SET(quiet) THEN PRINT,"Remember, each cleanedUpFile should come with eSpec_clean_info AND cleaned_eSpec_i"
  RETURN,cleanedUpFile

END
