;2016/05/26
PRO PRINT_ESPEC_STATS,eSpec, $
                      FAILCODES=failCodes, $
                      CHARETHRESH=charEThresh, $
                      SKIP_FAILURE_REPORT=skip_failure_report, $
                      INTERPRETED_STATISTICS=interpreted_statistics, $
                      LUN=lun

  COMPILE_OPT idl2

  IF N_ELEMENTS(lun) NE 0 THEN l = lun ELSE l = -1

  barSym                         = '*'

  ;;The titles
  IF KEYWORD_SET(interpreted_statistics) THEN BEGIN
     titleTitle                  = "ELECTRON SPECTRA IDENTIFICATION"
  ENDIF ELSE BEGIN
     titleTitle                  = "NEWELL   SPECTRA IDENTIFICATION"
  ENDELSE
  monoTitle                      = '     MONOENERGETIC EVENTS      '
  broadTitle                     = '     BROADBAND     EVENTS      '
  diffuseTitle                   = '     DIFFUSE       EVENTS      '
  failCodeTitle                  = '     FAILCODE       BONUS      '
  failTitle                      = '     WHY    WE     FAILED      '
  MAKE_BAR_STRING,titleBar,titleTitle,barSym
  MAKE_BAR_STRING,titleBar2,titleTitle,'='


  ;;The goods
  monoGood                       = WHERE(eSpec.mono     EQ 1  ,nMG  )
  monoGoodStrict                 = WHERE(eSpec.mono     EQ 2  ,nMG_S)
  nMG_T                          = nMG+nMG_S

  broadGood                      = WHERE(eSpec.broad    EQ 1  ,nBG  )
  broadGoodStrict                = WHERE(eSpec.broad    EQ 2  ,nBG_S)
  nBG_T                          = nBG+nBG_S

  diffuse                        = WHERE(eSpec.diffuse  EQ 1,nDiffuse)

  monoGoodtxt                    = STRING(FORMAT='("Monoenergetic         ",T48,": ",I0)',nMG)
  monoGoodS_txt                  = STRING(FORMAT='("Monoenergetic (strict)",T48,": ",I0)',nMG_S)
  monoGoodT_txt                  = STRING(FORMAT='("Monoenergetic (total) ",T48,": ",I0)',nMG_T)

  broadGoodtxt                   = STRING(FORMAT='("Broadband             ",T48,": ",I0)',nBG)
  broadGoodS_txt                 = STRING(FORMAT='("Broadband     (strict)",T48,": ",I0)',nBG_S)
  broadGoodT_txt                 = STRING(FORMAT='("Broadband     (total) ",T48,": ",I0)',nBG_T)

  diffusetxt                     = STRING(FORMAT='("Diffuse                ",T48,": ",I0)',nDiffuse)


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

  IF KEYWORD_SET(failCodes) THEN BEGIN
     ;;The bads
     monoFail_step1              = WHERE((failCodes.mono AND 1) GT 0,nMF_1)
     monoFail_step2              = WHERE((failCodes.mono AND 2) GT 0,nMF_2)
     monoFail_step3              = WHERE((failCodes.mono AND 4) GT 0,nMF_3)
     monoFail_step4              = WHERE((failCodes.mono AND 8) GT 0,nMF_4)

     monoFail_step1AND2          = CGSETINTERSECTION(monoFail_step1,monoFail_step2,         COUNT=nMF_1AND2)
     monoFail_step1AND3          = CGSETINTERSECTION(monoFail_step1,monoFail_step3,         COUNT=nMF_1AND3)
     monoFail_step1AND4          = CGSETINTERSECTION(monoFail_step1,monoFail_step4,         COUNT=nMF_1AND4)
     monoFail_step2AND3          = CGSETINTERSECTION(monoFail_step2,monoFail_step3,         COUNT=nMF_2AND3)
     monoFail_step2AND4          = CGSETINTERSECTION(monoFail_step2,monoFail_step4,         COUNT=nMF_2AND4)
     monoFail_step3AND4          = CGSETINTERSECTION(monoFail_step3,monoFail_step4,         COUNT=nMF_3AND4)
     monoFail_step1AND2AND3      = CGSETINTERSECTION(monoFail_step1,monoFail_step2AND3,     COUNT=nMF_1AND2AND3)
     monoFail_step1AND3AND4      = CGSETINTERSECTION(monoFail_step1,monoFail_step3AND4,     COUNT=nMF_1AND3AND4)
     monoFail_step2AND3AND4      = CGSETINTERSECTION(monoFail_step2,monoFail_step3AND4,     COUNT=nMF_2AND3AND4)
     monoFail_step1AND2AND3AND4  = CGSETINTERSECTION(monoFail_step1AND2,monoFail_step3AND4, COUNT=nMF_1AND2AND3AND4)

     monoFail_step1OR2           = CGSETUNION(monoFail_step1,monoFail_step2,                COUNT=nMF_1OR2)
     monoFail_step1OR3           = CGSETUNION(monoFail_step1,monoFail_step3,                COUNT=nMF_1OR3)
     monoFail_step1OR4           = CGSETINTERSECTION(monoFail_step1,monoFail_step4,         COUNT=nMF_1OR4)
     monoFail_step2OR3           = CGSETUNION(monoFail_step2,monoFail_step3,                COUNT=nMF_2OR3)
     monoFail_step2OR4           = CGSETINTERSECTION(monoFail_step2,monoFail_step4,         COUNT=nMF_2OR4)
     monoFail_step3OR4           = CGSETINTERSECTION(monoFail_step3,monoFail_step4,         COUNT=nMF_3OR4)
     monoFail_step1OR2OR3        = CGSETUNION(monoFail_step1,monoFail_step2OR3,             COUNT=nMF_1OR2OR3)
     monoFail_step1OR3OR4        = CGSETINTERSECTION(monoFail_step1,monoFail_step3OR4,      COUNT=nMF_1OR3OR4)
     monoFail_step2OR3OR4        = CGSETINTERSECTION(monoFail_step2,monoFail_step3OR4,      COUNT=nMF_2OR3OR4)
     monoFail_step1OR2OR3OR4     = CGSETINTERSECTION(monoFail_step1OR2,monoFail_step3OR4,   COUNT=nMF_1OR2OR3OR4)
     monoFail_T                  = CGSETUNION(monoFail_step1,monoFail_step2,                COUNT=nMF_T)

     broadFail_step1             = WHERE((failCodes.broad AND 1) GT 0,nBF_1)
     broadFail_step2             = WHERE((failCodes.broad AND 2) GT 0,nBF_2)
     broadFail_step3             = WHERE((failCodes.broad AND 4) GT 0,nBF_3)

     broadFail_step1AND2         = CGSETINTERSECTION(broadFail_step1,broadFail_step2,       COUNT=nBF_1AND2)
     broadFail_step1AND3         = CGSETINTERSECTION(broadFail_step1,broadFail_step3,       COUNT=nBF_1AND3)
     broadFail_step2AND3         = CGSETINTERSECTION(broadFail_step2,broadFail_step3,       COUNT=nBF_2AND3)
     broadFail_step1AND2AND3     = CGSETINTERSECTION(broadFail_step1,broadFail_step2AND3,   COUNT=nBF_1AND2AND3)

     broadFail_step1OR2          = CGSETUNION(broadFail_step1,broadFail_step2,              COUNT=nBF_1OR2)
     broadFail_step1OR3          = CGSETUNION(broadFail_step1,broadFail_step3,              COUNT=nBF_1OR3)
     broadFail_step2OR3          = CGSETUNION(broadFail_step2,broadFail_step3,              COUNT=nBF_2OR3)
     broadFail_step1OR2OR3       = CGSETUNION(broadFail_step1,broadFail_step2OR3,           COUNT=nBF_1OR2OR3)
     broadFail_T                 = CGSETUNION(broadFail_step1,broadFail_step2,              COUNT=nBF_T)

     broadFail_woulda            = BROADS_THAT_WOULD_HAVE_SUCCEEDED(eSpec,failCodes,charEThresh, $
                                                                    COUNT=nBF_woulda, $
                                                                    CUSPCOUNT=cuspCount, $
                                                                    NOTCUSPCOUNT=notCuspCount)
  ENDIF ELSE BEGIN
     ;;The bads
     monoFail_step1              = WHERE(eSpec.mono     EQ 254,nMF_1)
     monoFail_step2              = WHERE(eSpec.mono     EQ 253,nMF_2)
     monoFail_step3              = WHERE(eSpec.mono     EQ 252,nMF_3)
     monoFail_step4              = WHERE(eSpec.mono     EQ 251,nMF_4)
     nMF_T                       = nMF_1 + nMF_2 + nMF_3 + nMF_4

     broadFail_step1             = WHERE(eSpec.broad    EQ 254,nBF_1)
     broadFail_step2             = WHERE(eSpec.broad    EQ 253,nBF_2)
     broadFail_step3             = WHERE(eSpec.broad    EQ 252,nBF_3)
     nBF_T                       = nBF_1 + nBF_2 + nBF_3

  ENDELSE

  monoFail_txt1                  = 'Mono #1--Insufficient energy drop around peak         '
  monoFail_txt2                  = 'Mono #2--Peak threshold (1.0e8 eV/cm^2-sr-eV)  not met'
  monoFail_txt3                  = 'Mono #3--Characteristic energy              LE   80 eV'
  monoFail_txt4                  = 'Mono #4--Peak energy                        LT  100 eV'
  monoFail_txtT                  = 'TOTAL # MONO FAILURES'

  broadFail_txt1                 = 'Broad #1--dJ_E/dE > 2.0e8 ev/cm^2-sr-ev FOR LT 6 bins '
  broadFail_txt2                 = 'Broad #2--Characteristic energy             LE   80 eV'
  broadFail_txt3                 = 'Broad #3--Too few bins above dJ_E/dE thresh AND min_eV'
  broadFail_txtT                 = 'TOTAL # BROAD FAILURES'

  IF KEYWORD_SET(failCodes) THEN BEGIN
     monoFail_txt1AND2           = 'Mono 1AND2                                            '
     monoFail_txt1AND3           = 'Mono 1AND3                                            '
     monoFail_txt1AND4           = 'Mono 1AND4                                            '
     monoFail_txt2AND3           = 'Mono 2AND3                                            '
     monoFail_txt2AND4           = 'Mono 2AND4                                            '
     monoFail_txt3AND4           = 'Mono 3AND4                                            '
     monoFail_txt1AND2AND3       = 'Mono 1AND2AND3                                        '
     monoFail_txt1AND3AND4       = 'Mono 1AND3AND4                                        '
     monoFail_txt1AND2AND3AND4   = 'Mono 1AND2AND3AND4                                    '

     monoFail_txt1OR2            = 'Mono 1OR2                                             '
     monoFail_txt1OR3            = 'Mono 1OR3                                             '
     monoFail_txt1OR4            = 'Mono 1OR4                                             '
     monoFail_txt2OR3            = 'Mono 2OR3                                             '
     monoFail_txt2OR4            = 'Mono 2OR4                                             '
     monoFail_txt3OR4            = 'Mono 3OR4                                             '
     monoFail_txt1OR2OR3         = 'Mono 1OR2OR3                                          '
     monoFail_txt1OR3OR4         = 'Mono 1OR3OR4                                          '
     monoFail_txt1OR2OR3OR4      = 'Mono 1OR2OR3OR4                                       '

     broadFail_txt1AND2          = 'Broad 1AND2                                           '
     broadFail_txt1AND3          = 'Broad 1AND3                                           '
     broadFail_txt2AND3          = 'Broad 2AND3                                           '
     broadFail_txt1AND2AND3      = 'Broad 1AND2AND3                                       '

     broadFail_txt1OR2           = 'Broad 1OR2                                            '
     broadFail_txt1OR3           = 'Broad 1OR3                                            '
     broadFail_txt2OR3           = 'Broad 2OR3                                            '
     broadFail_txt1OR2OR3        = 'Broad 1OR2OR3                                         '

     broadFail_txtwoulda         = 'Broad would-be qualifier w/ lower chare               '
     broadFail_txtwouldacusp     = 'Broad would-be qualifier w/ lower chare (cusp)        '
     broadFail_txtwouldanotcusp  = 'Broad would-be qualifier w/ lower chare (not cusp)    '
  ENDIF

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


  IF KEYWORD_SET(failCodes) THEN BEGIN
     PRINTF,l,titleBar
     PRINTF,l,failCodeTitle
     PRINTF,l,titleBar
     PRINTF,l,''
     PRINTF,l,monoTitle
     PRINTF,l,titleBar
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1OR2,nMF_1OR2
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1OR3,nMF_1OR3
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1OR4,nMF_1OR4
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt2OR3,nMF_2OR3
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt2OR4,nMF_2OR4
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt3OR4,nMF_3OR4
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1OR2OR3,nMF_1OR2OR3
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1OR3OR4,nMF_1OR3OR4
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1OR2OR3OR4,nMF_1OR2OR3OR4
     PRINTF,l,''
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1AND2,nMF_1AND2
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1AND3,nMF_1AND3
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1AND4,nMF_1AND4
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt2AND3,nMF_2AND3
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt2AND4,nMF_2AND4
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt3AND4,nMF_3AND4
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1AND2AND3,nMF_1AND2AND3
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1AND3AND4,nMF_1AND3AND4
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',monoFail_txt1AND2AND3AND4,nMF_1AND2AND3AND4
     PRINTF,l,''
     PRINTF,l,''
     PRINTF,l,broadTitle
     PRINTF,l,titleBar
     PRINTF,l,''
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txt1OR2,nBF_1OR2
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txt1OR3,nBF_1OR3
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txt2OR3,nBF_2OR3
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txt1OR2OR3,nBF_1OR2OR3
     PRINTF,l,''
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txt1AND2,nBF_1AND2
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txt1AND3,nBF_1AND3
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txt2AND3,nBF_2AND3
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txt1AND2AND3,nBF_1AND2AND3
     PRINTF,l,''
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txtwoulda,nBF_woulda       
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txtwouldacusp,cuspCount
     PRINTF,l,FORMAT='(A0,T59,": ",I0)',broadFail_txtwouldanotcusp,notCuspCount
     PRINTF,l,''
     PRINTF,l,''
  ENDIF

END
