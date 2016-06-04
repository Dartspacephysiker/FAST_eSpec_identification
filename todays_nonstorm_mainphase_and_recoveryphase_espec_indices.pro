;2016/06/04
FUNCTION TODAYS_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_ESPEC_INDICES, $
   SUFFIX=suffix, $
   DSTCUTOFF=dstCutoff

  indDir               = '/SPENCEdata/Research/database/temps/'
  filePref             = 'todays_nonstorm_mainphase_and_recoveryphase_ESPEC_inds--dstCutoff_' + STRCOMPRESS(dstCutoff,/REMOVE_ALL) + "nT"
  hoyDia               = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  todaysFile           = indDir + filePref + (KEYWORD_SET(suffix) ? suffix : '' ) + '--' + hoyDia + '.sav'

  RETURN,todaysFile

END