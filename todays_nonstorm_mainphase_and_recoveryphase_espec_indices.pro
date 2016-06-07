;2016/06/04
FUNCTION TODAYS_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_ESPEC_INDICES, $
   SUFFIX=suffix, $
   DSTCUTOFF=dstCutoff, $
   MOST_RECENT=most_recent

  COMPILE_OPT idl2

  IF N_ELEMENTS(dstCutoff) NE 0 THEN dst = dstCutoff ELSE dst = -20

  indDir               = '/SPENCEdata/Research/database/temps/'

  IF KEYWORD_SET(most_recent) THEN BEGIN
     mostRecent_bash   = indDir + 'mostRecent_eSpec_storms_inds.txt'
     SPAWN,'cat ' + mostRecent_bash,todaysFile
     ;; todaysFile        = indDir + todaysFile
  ENDIF ELSE BEGIN
     filePref          = 'todays_nonstorm_mainphase_and_recoveryphase_ESPEC_inds--dstCutoff_' + STRCOMPRESS(dst,/REMOVE_ALL) + "nT"
     hoyDia            = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
     todaysFile        = indDir + filePref + (KEYWORD_SET(suffix) ? suffix : '' ) + '--' + hoyDia + '.sav'
  ENDELSE

  RETURN,todaysFile

END