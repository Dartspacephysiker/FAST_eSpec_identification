;2016/05/26 The idea is to convert to an interpretation of each event based strictly on Newell et al. [2009]
;;2017/02/04 You were wrong, Spence. You haven't been doing the strict Newell thing at all! your FAVOR_BROADSTRICT_OVER_MONO is
;the real Newell et al. [2009] interpretation, you see. (Newell et al. [2009], ¶21)
;;
PRO CONVERT_ESPEC_TO_STRICT_NEWELL_INTERPRETATION,eSpec,eSpec_interpreted, $
   NB_CONVTOM_=nB_ConvToM_, $
   NB_CONVTOMS=nB_ConvToMS, $
   NBSCONVTOMS=nBSConvToMS, $
   NBTCONVTOMT=nBTConvToMT, $
   NM_CONVTOBS=nM_ConvToBS, $
   FAVOR_BROADSTRICT_OVER_MONO=favor_broadStrict_over_mono, $
   HUGE_STRUCTURE=huge_structure, $
   HAS_NO_INFO_STRUCT=look__im_homeless, $
   VERBOSE=verbose
   
   
   COMPILE_OPT IDL2,STRICTARRSUBS

   ;;Check to see whether conversion has already happened
   STR_ELEMENT,eSpec,"info",INDEX=infoIndex
   IF infoIndex GE 0 THEN BEGIN
      IF eSpec.info.converted THEN BEGIN
         PRINT,"This DB appears to already have been interpreted!"
         PRINT,FORMAT='("eSpec.info.converted",T30,": ",I0)', $
               eSpec.info.converted
         PRINT,FORMAT='("eSpec.info.Newell2009interp",T30,": ",I0)', $
               eSpec.info.Newell2009interp
         PRINT,'Not converting eSpec DB ...'
         RETURN
      ENDIF

   ENDIF ELSE BEGIN
      
      IF ~KEYWORD_SET(look__im_homeless) THEN BEGIN
         PRINT,"How did you come to CONVERT_ESPEC_TO_STRICT[...] without having an info struct?"
         STOP
      ENDIF
      info  = {converted        : 0B, $
               Newell2009interp : 0B, $
               correctedFluxes  : 0B}
      eSpec = CREATE_STRUCT(eSpec,"info",info)

   ENDELSE

  ;;The goods
  monoGood              = WHERE(eSpec.mono     EQ 1  ,nMG  )
  monoGoodStrict        = WHERE(eSpec.mono     EQ 2  ,nMG_S)
  nMG_T                 = nMG+nMG_S

  broadGood             = WHERE(eSpec.broad    EQ 1  ,nBG  )
  broadGoodStrict       = WHERE(eSpec.broad    EQ 2  ,nBG_S)
  nBG_T                 = nBG+nBG_S

  diffGood              = WHERE(( (eSpec.broad EQ 0) OR (eSpec.broad GT 2) ) AND $
                                ( (eSpec.mono  EQ 0) OR (eSpec.mono  GT 2) ),nDG_T)

  n_G_T                 = N_ELEMENTS(CGSETUNION(diffGood,CGSETUNION(broadGood,monoGood)))
  n_G_T                 = nMG_T+nBG_T+nDG_T

  IF KEYWORD_SET(huge_structure) THEN BEGIN
     eSpec_interpreted  = TEMPORARY(eSpec)
  ENDIF ELSE BEGIN
     eSpec_interpreted  = eSpec
  ENDELSE


  ;Drop the broads if there's any competition between mono EQ 1 and broad EQ 1
  ;2017/02/04 The statement above is false! You only call 'em mono if there's competition between STRICTS!
  ;; i_B_ConvToM_          = CGSETINTERSECTION(monoGood      ,broadGood      ,COUNT=nB_ConvToM_)
  ;; i_B_ConvToMS          = CGSETINTERSECTION(monoGoodStrict,broadGood      ,COUNT=nB_ConvToMS)
  ;; i_BSConvToMS          = CGSETINTERSECTION(monoGoodStrict,broadGoodStrict,COUNT=nBSConvToMS)
  ;; i_M_ConvToBS          = CGSETINTERSECTION(monoGood      ,broadGoodStrict,COUNT=nM_ConvToBS)
  i_B_ConvToM_          = WHERE(eSpec.broad EQ 1 AND eSpec.mono EQ 1,nB_ConvToM_)
  i_B_ConvToMS          = WHERE(eSpec.broad EQ 1 AND eSpec.mono EQ 2,nB_ConvToMS)
  i_BSConvToMS          = WHERE(eSpec.broad EQ 2 AND eSpec.mono EQ 2,nBSConvToMS)
  i_M_ConvToBS          = WHERE(eSpec.broad EQ 2 AND eSpec.mono EQ 1,nM_ConvToBS)

  nBTConvToM_           = nB_ConvToM_ + nB_ConvToMS + nBSConvToMS

  IF KEYWORD_SET(verbose) THEN PRINT,"       monos  before   " + STRCOMPRESS(nMG ,/REMOVE_ALL)
  IF KEYWORD_SET(verbose) THEN PRINT,"Strict monos  before   " + STRCOMPRESS(nMG_S,/REMOVE_ALL)
  IF KEYWORD_SET(verbose) THEN PRINT,"Total  monos  before   " + STRCOMPRESS(nMG_T,/REMOVE_ALL)
  IF KEYWORD_SET(verbose) THEN PRINT,''
  IF KEYWORD_SET(verbose) THEN PRINT,"       broads  before  " + STRCOMPRESS(nBG ,/REMOVE_ALL)
  IF KEYWORD_SET(verbose) THEN PRINT,"Strict broads  before  " + STRCOMPRESS(nBG_S,/REMOVE_ALL)
  IF KEYWORD_SET(verbose) THEN PRINT,"Total  broads  before  " + STRCOMPRESS(nBG_T,/REMOVE_ALL)
  IF KEYWORD_SET(verbose) THEN PRINT,""
  IF KEYWORD_SET(verbose) THEN PRINT,"Total  diffs   before  " + STRCOMPRESS(nDG_T,/REMOVE_ALL)
  IF KEYWORD_SET(verbose) THEN PRINT,""
  IF KEYWORD_SET(verbose) THEN PRINT,"Total          before  " + STRCOMPRESS(n_G_T,/REMOVE_ALL) + $
                                     ' (but only ' + STRCOMPRESS(N_ELEMENTS(eSpec.x),/REMOVE_ALL) + ' events in total!)'

  IF nB_ConvToM_ GT 0 THEN BEGIN
     eSpec_interpreted.broad[i_B_ConvToM_] = 255-10-1
     eSpec_interpreted.mono[i_B_ConvToM_]  = 1
     IF KEYWORD_SET(verbose) THEN PRINT,"nB_Conv_to_M_  " + STRCOMPRESS(nB_ConvToM_,/REMOVE_ALL)
  ENDIF
  IF nB_ConvToMS GT 0 THEN BEGIN
     eSpec_interpreted.broad[i_B_ConvToMS] = 255-10-1
     eSpec_interpreted.mono[i_B_ConvToMS]  = 2
     IF KEYWORD_SET(verbose) THEN PRINT,"nB_Conv_to_MS  " + STRCOMPRESS(nB_ConvToMS,/REMOVE_ALL)
  ENDIF
  IF nBSConvToMS GT 0 THEN BEGIN
     eSpec_interpreted.broad[i_BSConvToMS] = 255-10-2
     eSpec_interpreted.mono[i_BSConvToMS]  = 2
     IF KEYWORD_SET(verbose) THEN PRINT,"nBSConv_to_MS  " + STRCOMPRESS(nBSConvToMS,/REMOVE_ALL)
  ENDIF

  ;;2017/02/04 Here's where you're wrong. There's no call for favoring mono over broadStrict (Newell et al. [2009], ¶21)
  IF nM_ConvToBS GT 0 THEN BEGIN
     ;; IF KEYWORD_SET(favor_broadStrict_over_mono) THEN BEGIN
        eSpec_interpreted.mono[i_M_ConvToBS]  = 255-10-1
        eSpec_interpreted.broad[i_M_ConvToBS] = 2
        ;; IF KEYWORD_SET(verbose) THEN PRINT,"nM_Conv_to_BS  " + STRCOMPRESS(nM_ConvToBS,/REMOVE_ALL)
     ;; ENDIF ELSE BEGIN
        ;; eSpec_interpreted.mono[i_M_ConvToBS]  = 1
        ;; eSpec_interpreted.broad[i_M_ConvToBS] = 255-10-2
        IF KEYWORD_SET(verbose) THEN PRINT,"nBS_Conv_to_M  " + STRCOMPRESS(nM_ConvToBS,/REMOVE_ALL)
     ;; ENDELSE
  ENDIF

  ;;Let everyone know what you did
  eSpec_interpreted.info.converted        = 1B
  ;; eSpec_interpreted.info.Newell2009interp = BYTE(~KEYWORD_SET(favor_broadStrict_over_mono)) ;2017/02/04 Wrong, you dreamed this up
  eSpec_interpreted.info.Newell2009interp = 1B

  ;;Details
  IF KEYWORD_SET(verbose) THEN BEGIN
     monoGood              = WHERE(eSpec_interpreted.mono     EQ 1  ,nMG  )
     monoGoodStrict        = WHERE(eSpec_interpreted.mono     EQ 2  ,nMG_S)
     nMG_T                 = nMG+nMG_S

     broadGood             = WHERE(eSpec_interpreted.broad    EQ 1  ,nBG  )
     broadGoodStrict       = WHERE(eSpec_interpreted.broad    EQ 2  ,nBG_S)
     nBG_T                 = nBG+nBG_S

     diffGood              = WHERE(( eSpec_interpreted.broad EQ 0 OR eSpec_interpreted.broad GT 2 ) AND $
                                   ( eSpec_interpreted.mono  EQ 0 OR eSpec_interpreted.mono  GT 2 ),nDG_T)

     ;; n_G_T                 = N_ELEMENTS(CGSETUNION(diffGood,CGSETUNION(broadGood,monoGood)))
     n_G_T                 = nMG_T+nBG_T+nDG_T

     PRINT,"**Monos **"
     PRINT,"       monos  after    " + STRCOMPRESS(nMG ,/REMOVE_ALL)
     PRINT,"Strict monos  after    " + STRCOMPRESS(nMG_S,/REMOVE_ALL)
     PRINT,"Total  monos  after    " + STRCOMPRESS(nMG_T,/REMOVE_ALL)
     PRINT,""
     PRINT,"**Broads**"
     PRINT,"       broads  after   " + STRCOMPRESS(nBG ,/REMOVE_ALL)
     PRINT,"Strict broads  after   " + STRCOMPRESS(nBG_S,/REMOVE_ALL)
     PRINT,"Total  broads  after   " + STRCOMPRESS(nBG_T,/REMOVE_ALL)
     PRINT,""
     PRINT,"**Diffs **"
     PRINT,"Total  diffs   after   " + STRCOMPRESS(nDG_T,/REMOVE_ALL)
     PRINT,""
     PRINT,"Total  events  after  " + STRCOMPRESS(n_G_T,/REMOVE_ALL)
  ENDIF

END