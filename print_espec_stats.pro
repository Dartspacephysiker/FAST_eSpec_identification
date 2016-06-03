;2016/05/26
PRO PRINT_ESPEC_STATS,eSpec,SKIP_FAILURE_REPORT=skip_failure_report, $
                      INTERPRETED_STATISTICS=interpreted_statistics, $
                      LUN=lun

  COMPILE_OPT idl2 

  IF N_ELEMENTS(lun) NE 0 THEN l = lun ELSE l = -1



  barSym           = '*'

  ;;The titles
  IF KEYWORD_SET(interpreted_statistics) THEN BEGIN
     titleTitle    = "ELECTRON SPECTRA IDENTIFICATION"
  ENDIF ELSE BEGIN
     titleTitle    = "NEWELL   SPECTRA IDENTIFICATION"
  ENDELSE
  monoTitle        = '     MONOENERGETIC EVENTS      '
  broadTitle       = '     BROADBAND     EVENTS      '
  diffuseTitle     = '     DIFFUSE       EVENTS      '
  failTitle        = '     WHY    WE     FAILED      '
  MAKE_BAR_STRING,titleBar,titleTitle,barSym
  MAKE_BAR_STRING,titleBar2,titleTitle,'='


  ;;The goods
  monoGood         = WHERE(eSpec.mono     EQ 1  ,nMG  )
  monoGoodStrict   = WHERE(eSpec.mono     EQ 2  ,nMG_S)
  nMG_T            = nMG+nMG_S

  broadGood        = WHERE(eSpec.broad    EQ 1  ,nBG  )
  broadGoodStrict  = WHERE(eSpec.broad    EQ 2  ,nBG_S)
  nBG_T            = nBG+nBG_S

  diffuse          = WHERE(eSpec.diffuse  EQ 1,nDiffuse)

  monoGoodtxt      = STRING(FORMAT='("Monoenergetic         ",T48,": ",I0)',nMG)
  monoGoodS_txt    = STRING(FORMAT='("Monoenergetic (strict)",T48,": ",I0)',nMG_S)
  monoGoodT_txt    = STRING(FORMAT='("Monoenergetic (total) ",T48,": ",I0)',nMG_T)

  broadGoodtxt     = STRING(FORMAT='("Broadband             ",T48,": ",I0)',nBG)
  broadGoodS_txt   = STRING(FORMAT='("Broadband     (strict)",T48,": ",I0)',nBG_S)
  broadGoodT_txt   = STRING(FORMAT='("Broadband     (total) ",T48,": ",I0)',nBG_T)

  diffusetxt       = STRING(FORMAT='("Diffuse                ",T48,": ",I0)',nDiffuse)


  PRINTF,l,titleBar2
  PRINTF,l,titleBar
  PRINTF,l,titleTitle
  PRINTF,l,titleBar
  PRINTF,l,titleBar2
  PRINTF,l,''
  PRINTF,l,monoTitle
  PRINTF,l,titleBar
  ;; PRINTF,l,''
  PRINTF,l,monoGoodtxt
  PRINTF,l,monoGoodS_txt
  PRINTF,l,monoGoodT_txt
  PRINTF,l,''
  PRINTF,l,''
  ;; PRINTF,l,titleBar
  PRINTF,l,broadTitle
  PRINTF,l,titleBar
  ;; PRINTF,l,''
  PRINTF,l,broadGoodtxt
  PRINTF,l,broadGoodS_txt
  PRINTF,l,broadGoodT_txt
  PRINTF,l,''
  PRINTF,l,''
  ;; PRINTF,l,titleBar
  PRINTF,l,diffuseTitle
  PRINTF,l,titleBar
  ;; PRINTF,l,''
  PRINTF,l,diffusetxt
  PRINTF,l,''
  IF KEYWORD_SET(skip_failure_report) THEN RETURN

  ;;The bads
  monoFail_step1   = WHERE(eSpec.mono     EQ 254,nMF_1)
  monoFail_step2   = WHERE(eSpec.mono     EQ 253,nMF_2)
  monoFail_step3   = WHERE(eSpec.mono     EQ 252,nMF_3)
  monoFail_step4   = WHERE(eSpec.mono     EQ 251,nMF_4)
  nMF_T            = nMF_1 + nMF_2 + nMF_3 + nMF_4

  broadFail_step1  = WHERE(eSpec.broad    EQ 254,nBF_1)
  broadFail_step2  = WHERE(eSpec.broad    EQ 253,nBF_2)
  broadFail_step3  = WHERE(eSpec.broad    EQ 252,nBF_3)
  nBF_T            = nBF_1 + nBF_2 + nBF_3

  monoFail_txt1    = 'Mono #1--Insufficient energy drop around peak         '
  monoFail_txt2    = 'Mono #2--Peak threshold (1.0e8 eV/cm^2-sr-eV)  not met'
  monoFail_txt3    = 'Mono #3--Characteristic energy              LE   80 eV'
  monoFail_txt4    = 'Mono #4--Peak energy                        LT  100 eV'
  monoFail_txtT    = 'TOTAL # MONO FAILURES'

  broadFail_txt1   = 'Broad #1--dJ_E/dE > 2.0e8 ev/cm^2-sr-ev FOR LT 6 bins '
  broadFail_txt2   = 'Broad #2--Characteristic energy             LE   80 eV'
  broadFail_txt3   = 'Broad #3--Too few bins above dJ_E/dE thresh AND min_eV'
  broadFail_txtT   = 'TOTAL # BROAD FAILURES'

  PRINTF,l,titleBar
  PRINTF,l,failTitle
  PRINTF,l,titleBar

  PRINTF,l,monoTitle
  PRINTF,l,titleBar
  ;; PRINTF,l,''
  PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1,nMF_1
  PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt2,nMF_2
  PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt3,nMF_3
  PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt4,nMF_4
  ;; PRINTF,l,''
  PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txtT,nMF_T
  PRINTF,l,''
  PRINTF,l,''
  PRINTF,l,broadTitle
  PRINTF,l,titleBar
  ;; PRINTF,l,''
  PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txt1,nBF_1
  PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txt2,nBF_2
  PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txt3,nBF_3
  ;; PRINTF,l,''
  PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txtT,nBF_T
  PRINTF,l,''

END

