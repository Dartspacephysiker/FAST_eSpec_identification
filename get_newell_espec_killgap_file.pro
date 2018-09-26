;;01/19/17
FUNCTION GET_NEWELL_ESPEC_KILLGAP_FILE,DBStruct, $
                                       WHOWANTSTOKNOW=maximus, $
                                       NEWELLDBDIR=NewellDBDir, $
                                       FOR_ESPEC_DB=for_eSpec_DB, $
                                       FOR_ION_DB=for_ion_DB, $
                                       STOP_IF_NOEXIST=stop_if_noExist

  COMPILE_OPT IDL2,STRICTARRSUBS

  defNewellDir   = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'
  defNewellIDir  = '/SPENCEdata/Research/database/FAST/dartdb/ion_db/'
  
  IF KEYWORD_SET(maximus) THEN BEGIN
     ;; killGapFile = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/' + $
     ;;               'eSpecDB_20160607_v0_0--with_mapping_factors--killedGap_inds.sav'
     ;; killGapFile = NewellDBDir + killGapDir + $
     ;;               'eSpecDB_20160607_v0_0--with_mapping_factors--killedGap_inds.sav'
     killGapFile = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/instrument_oddity_times/eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors--killedGap_inds--500-24634--20170208.sav'
     ;; killGapDate = '20170206'
     ;; isOK        = 
  ENDIF ELSE BEGIN

     CASE 1 OF
        KEYWORD_SET(for_eSpec_DB): BEGIN
           IF KEYWORD_SET(NewellDBDir) THEN BEGIN
              killGapDir     = '../instrument_oddity_times/'
           ENDIF ELSE BEGIN
              NewellDBDir    = defNewellDir
              killGapDir     = 'instrument_oddity_times/'
           ENDELSE

           file20160607 = STRMATCH(GET_NEWELL_DB_STRING(DBStruct),'*20160607*',/FOLD_CASE)
           file20170203 = STRMATCH(GET_NEWELL_DB_STRING(DBStruct),'*20170203*',/FOLD_CASE)

           CASE 1 OF
              KEYWORD_SET(file20160607): BEGIN
                 killGapFileSuff = '--killedGap_inds.sav'
              END
              KEYWORD_SET(file20170203): BEGIN
                 killGapFileSuff = '--killedGap_inds--500-24634--20170208.sav'
              END
           ENDCASE
           
        END
        KEYWORD_SET(for_ion_DB): BEGIN
           ;; IF KEYWORD_SET(NewellDBDir) THEN BEGIN
           ;;    killGapDir     = '../instrument_oddity_times/'
           ;; ENDIF ELSE BEGIN
           ;;    NewellDBDir    = defNewellDir
           ;;    killGapDir     = 'instrument_oddity_times/'
           ;; ENDELSE
           killGapDir        = 'instrument_oddity_times/'
           IF ~KEYWORD_SET(NewellDBDir) THEN BEGIN
              NewellDBDir    = defNewellIDir
           ENDIF

           killGapFileSuff   = '__oddity_times--500-14361--20170215.sav'
        END
     ENDCASE

     killGapFile = NewellDBDir + killGapDir + $
                   GET_NEWELL_DB_STRING(DBStruct) + killGapFileSuff

  ENDELSE

  IF KEYWORD_SET(stop_if_noExist) THEN BEGIN
     IF ~FILE_TEST(killGapFile) THEN BEGIN
        PRINT,"File doesn't exist: " + killGapFile
        PRINT,"What to do?"
        PRINT,"You know, if you visited JOURNAL__20170118__LOOK_FOR_GAPS_AND_REMOVE.PRO, you could probably get it handled."
        STOP
     ENDIF
  ENDIF

  RETURN,killGapFile

END
