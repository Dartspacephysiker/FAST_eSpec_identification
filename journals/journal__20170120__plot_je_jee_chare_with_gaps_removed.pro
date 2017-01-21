;;2017/01/20
PRO JOURNAL__20170120__PLOT_JE_JEE_CHARE_WITH_GAPS_REMOVED

  COMPILE_OPT IDL2

  startOrb    = 9843            ;Otherwise it just picks the first orbit in eSpec
  showPlots   = 1
  savePS      = 1
  PSDir       = '/SPENCEdata/Research/Satellites/FAST/espec_identification/plots/201701--trim_transitions/je_jee_chare/'
  PSPref      = 'je_jee_chare_sans_transitions--'
  !P.MULTI    = [0,1,3,0,0]
  
  saveDir     = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/instrument_oddity_times/'

  @common__newell_espec.pro

  ;; LOAD_NEWELL_ESPEC_DB,eSpec,!NULL,NEWELL__delta_t, $
  ;; /LOAD_DELTA_T, $
  ;; /NO_MEMORY_LOAD
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,!NULL,!NULL,!NULL, $
                          /LOAD_CHARE
  ENDIF
  CHECK_SORTED,NEWELL__eSpec.orbit,is_sorted,/QUIET
  IF ~is_sorted THEN STOP
  
  ;;Get the goodies (baddies?)
  NewellDBDir   = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  DBString      = GET_NEWELL_DB_STRING(NEWELL__eSpec)+'--killedGap_inds--'

  killGap_file  = GET_NEWELL_ESPEC_KILLGAP_FILE(NEWELL__eSpec, $
                                                NEWELLDBDIR=NewellDBDir, $
                                                /STOP_IF_NOEXIST)

  RESTORE,killGap_file

  uniqOrb_i      = UNIQ(NEWELL__eSpec.orbit)
  
  orbs           = NEWELL__eSpec.orbit[uniqOrb_i]
  orbInd         = KEYWORD_SET(startOrb) ? WHERE(orbs EQ startOrb) : 0
  WHILE orbInd[0] EQ -1 DO BEGIN
     startOrb++
     orbInd      = WHERE(orbs EQ startOrb)
  ENDWHILE
  curOrb         = KEYWORD_SET(startOrb) ? startOrb : orbs[0]
  endOrb         = KEYWORD_SET(endOrb  ) ? endOrb   : orbs[-1]

  ;; lastSavOrb     = curOrb
  ;; deltaSavOrb    = 500
  ;; orbInd         = -1

  ;; junk_i         = !NULL
  ;; befStart_i     = !NULL
  ;; junkTimes      = !NULL
  ;; befStartTimes  = !NULL
  ;; missingIntervals = !NULL
  
  ;;plot setup
  LOADCT2,39

  ;; junkTransCol = 100
  ;; junkStartCol = 160

  ;; junkTransPSym = 1
  ;; junkStartPSym = 2
  ;; TSPSym        = 3

  ;; TSSymSize     = 50.0

  IF KEYWORD_SET(showPlots) THEN BEGIN
     IF KEYWORD_SET(savePS) THEN BEGIN
     ENDIF ELSE BEGIN
        WINDOW,0,XSIZE=800,YSIZE=600
     ENDELSE
  ENDIF

  final = 0
  ;; firstOrbThisLoop = lastSavOrb
  ;; lastOrbThisLoop  = lastSavOrb+deltaSavOrb-1
  loaded           = 0B
  WHILE curOrb LE endOrb DO BEGIN

     ;; IF (curOrb EQ lastSavOrb) OR ~loaded THEN BEGIN
     ;;    firstOrbThisLoop = lastSavOrb
     ;;    lastOrbThisLoop  = lastSavOrb+deltaSavOrb-1

     ;;    orbRangeString = STRING(FORMAT='(I0,"-",I0)',lastSavOrb,lastSavOrb+deltaSavOrb-1)
     ;;    orbSavFileName = STRING(FORMAT='(A0,A0,".sav")',DBString,orbRangeString)

     ;;    RESTORE,saveDir+orbSavFileName

     ;;    PRINT,STRING(FORMAT='("Restored ",I0," junk inds and ",I0," befStart inds for orbs ",A0," to file : ",A0)', $
     ;;                 N_ELEMENTS(junk_i),N_ELEMENTS(befStart_i),orbRangeString,orbSavFileName)

     ;;    lastSavOrb += deltaSavOrb
     ;;    loaded = 1B

     ;;    IF final THEN BREAK
     ;; ENDIF

     ;;Ind things
     orbString  = STRCOMPRESS(curOrb,/REMOVE_ALL)
     tmp_i      = WHERE(NEWELL__eSpec.orbit EQ curOrb[0],NBef)
     firstLastT = NEWELL__eSpec.x[[tmp_i[0],tmp_i[-1]]]

     IF NBef EQ 0 THEN BEGIN
        PRINT,"no inds for orbit " + STRCOMPRESS(curOrb,/REMOVE_ALL) + ". Move it on down."
        curOrb  = (orbs[++orbInd])[0]
        CONTINUE
     ENDIF
     
     tmp_i      = CGSETDIFFERENCE(tmp_i,junk_i,COUNT=nAftJunk)
     IF nAftJunk EQ 0 THEN STOP

     tmp_i      = CGSETDIFFERENCE(tmp_i,befStart_i,COUNT=nAftBefTimes)
     tmpTime    = NEWELL__eSpec.x[tmp_i]
     IF nAftBefTimes EQ 0 THEN STOP

     IF ~KEYWORD_SET(quiet) THEN PRINT,curOrb,N_ELEMENTS(tmp_i)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Plots
     IF KEYWORD_SET(showPlots) THEN BEGIN

        IF KEYWORD_SET(savePS) THEN BEGIN
           POPEN,PSDir+PSPref+orbString, $
                 XSIZE=6, $
                 YSIZE=6, $
                 /QUIET,/LAND
        ENDIF

        PLOT_ESPEC_JE_JEE_CHARE_ORBSUMMARY,curOrb, $
                                           NEWELL2009_PANEL=Newell2009_panel, $
                                           /USE_MY_JUNK_AND_BEFSTART, $
                                           JUNK_I=junk_i, $
                                           BEFSTART_I=befStart_i

        ;;;;For the first two paneaux
        ;; date_label = LABEL_DATE(DATE_FORMAT='')

        ;; tJul       = UTC_TO_JULDAY(tmpTime)
        ;; tJunk      = UTC_TO_JULDAY(NEWELL__eSpec.x[junk_i])
        ;; tBefStart  = UTC_TO_JULDAY(NEWELL__eSpec.x[befStart_i])
        ;; tRange     = UTC_TO_JULDAY(firstLastT)

        ;; PLOT,tJul,NEWELL__eSpec.je[tmp_i], $
        ;;      /NODATA, $
        ;;      TITLE='Orbit ' + orbString + ' (' + TIME_TO_STR(tmpTime[0],/MS) + ')', $
        ;;      XTICKFORMAT='(A1)', $
        ;;      XTICKUNITS='Time', $
        ;;      XRANGE=tRange, $
        ;;      /YLOG, $
        ;;      YRANGE=[1e6,5e11], $
        ;;      YTITLE='Je (#/cm!U2!N-s)', $
        ;;      YTICKV=[1e6,1e7,1e8,1e9,1e10,1e11], $
        ;;      YTICKNAME=['1e6','1e7','1e8','1e9','1e10','1e11'], $
        ;;      YSTYLE=1, $
        ;;      YMARGIN=[0,2]

        ;; OPLOT,tJul,NEWELL__eSpec.je[tmp_i], $
        ;;      PSYM=TSPsym, $
        ;;      COLOR=TSCol, $
        ;;      SYMSIZE=TSSymSize

        ;; OPLOT,tJunk,NEWELL__eSpec.je[junk_i], $
        ;;      PSYM=junkTransPSym, $
        ;;      COLOR=junkTransCol

        ;; OPLOT,tBefStart,NEWELL__eSpec.je[befStart_i], $
        ;;      PSYM=JunkStartPSym, $
        ;;      COLOR=junkStartCol

        ;; PLOT,tJul,NEWELL__eSpec.jee[tmp_i], $
        ;;      /NODATA, $
        ;;      XTICKFORMAT='(A1)', $
        ;;      XTICKUNITS='Time', $
        ;;      XRANGE=tRange, $
        ;;      /YLOG, $
        ;;      YRANGE=[1e-3,3e3], $
        ;;      YTITLE='Jee (mW/m!U2!N)', $
        ;;      YTICKV=[1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3], $
        ;;      YTICKNAME=['1e-3','1e-2','1e-1','1e0','1e1','1e2','1e3'], $
        ;;      YSTYLE=1, $
        ;;      YMARGIN=[0,1]

        ;; OPLOT,tJul,NEWELL__eSpec.jee[tmp_i], $
        ;;      PSYM=TSPsym, $
        ;;      COLOR=TSCol, $
        ;;      SYMSIZE=TSSymSize

        ;; OPLOT,tJunk,NEWELL__eSpec.jee[junk_i], $
        ;;      PSYM=junkTransPSym, $
        ;;      COLOR=junkTransCol

        ;; OPLOT,tBefStart,NEWELL__eSpec.jee[befStart_i], $
        ;;      PSYM=JunkStartPSym, $
        ;;      COLOR=junkStartCol

        ;; ;;Now chare, with dates
        ;; date_label = LABEL_DATE(DATE_FORMAT=['%I:%S','%D-%H'])

        ;; PLOT,tJul,NEWELL__eSpec.charE[tmp_i], $
        ;;      /NODATA, $
        ;;      XTICKFORMAT='LABEL_DATE', $
        ;;      XTICKUNITS=['Time','Time'], $
        ;;      XRANGE=tRange, $
        ;;      /YLOG, $
        ;;      YRANGE=[4,3e4], $
        ;;      YTITLE='Char E (eV)', $
        ;;      YTICKV=[1e1,1e2,1e3,1e4], $
        ;;      YTICKNAME=['1e1','1e2','1e3','1e4'], $
        ;;      YMARGIN=[6,1], $
        ;;      XMARGIN=[10,3], $
        ;;      YSTYLE=1

        ;; OPLOT,tJul,NEWELL__eSpec.charE[tmp_i], $
        ;;      PSYM=TSPsym, $
        ;;      COLOR=TSCol, $
        ;;      SYMSIZE=TSSymSize

        ;; OPLOT,tJunk,NEWELL__eSpec.charE[junk_i], $
        ;;      PSYM=junkTransPSym, $
        ;;      COLOR=junkTransCol

        ;; OPLOT,tBefStart,NEWELL__eSpec.charE[befStart_i], $
        ;;      PSYM=JunkStartPSym, $
        ;;      COLOR=junkStartCol


     ENDIF

     IF KEYWORD_SET(savePS) THEN BEGIN
        PCLOSE,/QUIET
        EPS2PDF,PSDir+PSPref+orbString, $
                /PS, $
                /TO_PNG, $
                /REMOVE_EPS, $
                /QUIET
     ENDIF

     curOrb           = (orbs[++orbInd])[0]

     ;; IF curOrb GE lastOrbThisLoop THEN BEGIN
     ;;    ;;clear vars 
     ;;    junk_i           = !NULL
     ;;    befStart_i       = !NULL
     ;;    junkTimes        = !NULL
     ;;    befStartTimes    = !NULL
     ;;    missingIntervals = !NULL 
     ;; ENDIF

     IF curOrb EQ endOrb THEN final = 1

  ENDWHILE

  PRINT,"FINISHED!"
  STOP

END


