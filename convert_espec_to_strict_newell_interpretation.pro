;2016/05/26 The idea is to convert to an interpretation of each event based strictly on Newell et al. [2009]
PRO CONVERT_ESPEC_TO_STRICT_NEWELL_INTERPRETATION,eSpec,eSpec_interpreted, $
   NB_CONVTOM_=nB_ConvToM_, $
   NB_CONVTOMS=nB_ConvToMS, $
   NBSCONVTOMS=nBSConvToMS, $
   NBTCONVTOMT=nBTConvToMT, $
   NM_CONVTOBS=nM_ConvToBS
   
   
   COMPILE_OPT idl2

  eSpec_interpreted = eSpec

  ;;The goods
  monoGood         = WHERE(eSpec.mono     EQ 1  ,nMG  )
  monoGoodStrict   = WHERE(eSpec.mono     EQ 2  ,nMG_S)
  nMG_T            = nMG+nMG_S

  broadGood        = WHERE(eSpec.broad    EQ 1  ,nBG  )
  broadGoodStrict  = WHERE(eSpec.broad    EQ 2  ,nBG_S)
  nBG_T            = nBG+nBG_S


  ;Drop the broads if there's any competition between mono = 1 and broad = 1
  i_B_ConvToM_     = CGSETINTERSECTION(monoGood      ,broadGood      ,COUNT=nB_ConvToM_)
  i_B_ConvToMS     = CGSETINTERSECTION(monoGoodStrict,broadGood      ,COUNT=nB_ConvToMS)
  i_BSConvToMS     = CGSETINTERSECTION(monoGoodStrict,broadGoodStrict,COUNT=nBSConvToMS)
  i_M_ConvToBS     = CGSETINTERSECTION(monoGood      ,broadGoodStrict,COUNT=nM_ConvToBS)

  nBTConvToM_      = nB_ConvToM_ + nB_ConvToMS + nBSConvToMS

  IF nB_ConvToM_ GT 0 THEN BEGIN
     eSpec_interpreted.broad[i_B_ConvToM_] = 255-10-1
  ENDIF
  IF nB_ConvToMS GT 0 THEN BEGIN
     eSpec_interpreted.broad[i_B_ConvToMS] = 255-10-1
  ENDIF
  IF nBSConvToMS GT 0 THEN BEGIN
     eSpec_interpreted.broad[i_BSConvToMS] = 255-10-2
  ENDIF
  IF nM_ConvToBS GT 0 THEN BEGIN
     eSpec_interpreted.mono[i_M_ConvToBS]  = 255-10-1
  ENDIF

END