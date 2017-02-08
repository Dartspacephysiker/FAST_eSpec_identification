;;01/19/17
FUNCTION GET_NEWELL_ESPEC_KILLGAP_FILE,eSpec, $
                                       WHOWANTSTOKNOW=maximus, $
                                       NEWELLDBDIR=NewellDBDir, $
                                       STOP_IF_NOEXIST=stop_if_noExist

  COMPILE_OPT IDL2

  defNewellDir   = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'
  IF KEYWORD_SET(NewellDBDir) THEN BEGIN
     killGapDir     = '../instrument_oddity_times/'
  ENDIF ELSE BEGIN
     NewellDBDir    = defNewellDir
     killGapDir     = 'instrument_oddity_times/'
  ENDELSE
  
  IF KEYWORD_SET(maximus) THEN BEGIN
     ;; killGapFile = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/' + $
     ;;               'eSpecDB_20160607_v0_0--with_mapping_factors--killedGap_inds.sav'
     killGapFile = NewellDBDir + killGapDir + $
                   'eSpecDB_20160607_v0_0--with_mapping_factors--killedGap_inds.sav'
     ;; killGapDate = '20170206'
     ;; isOK        = 
  ENDIF ELSE BEGIN
     file20160607 = STRMATCH(GET_NEWELL_DB_STRING(eSpec),'*20160607*',/FOLD_CASE)
     file20170203 = STRMATCH(GET_NEWELL_DB_STRING(eSpec),'*20170203*',/FOLD_CASE)

     CASE 1 OF
        KEYWORD_SET(file20160607): BEGIN
           killGapFileSuff = '--killedGap_inds.sav'
        END
        KEYWORD_SET(file20170203): BEGIN
           killGapFileSuff = '--killedGap_inds--500-24634--20170208.sav'
        END
     ENDCASE

     killGapFile = NewellDBDir + killGapDir + $
                   GET_NEWELL_DB_STRING(eSpec) + killGapFileSuff

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
