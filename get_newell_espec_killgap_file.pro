;;01/19/17
FUNCTION GET_NEWELL_ESPEC_KILLGAP_FILE,eSpec, $
                                       WHOWANTSTOKNOW=maximus, $
                                       NEWELLDBDIR=NewellDBDir, $
                                       STOP_IF_NOEXIST=stop_if_noExist

  COMPILE_OPT IDL2

  IF KEYWORD_SET(maximus) THEN BEGIN
     killGapFile = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/' + $
                   'eSpecDB_20160607_v0_0--with_mapping_factors--killedGap_inds.sav'
  ENDIF ELSE BEGIN
     killGapFile = NewellDBDir + $
                   GET_NEWELL_DB_STRING(eSpec) + $
                   '--killedGap_inds.sav'
  ENDELSE

  IF KEYWORD_SET(stop_if_noExist) THEN BEGIN
     IF ~FILE_TEST(killGapFile) THEN BEGIN
        PRINT,"File doeesn't exist: " + killGapFile
        PRINT,"What to do?"
        PRINT,"You know, if you visited JOURNAL__20170118__LOOK_FOR_GAPS_AND_REMOVE.PRO, you could probably get it handled."
        STOP
     ENDIF
  ENDIF

  RETURN,killGapFile

END
