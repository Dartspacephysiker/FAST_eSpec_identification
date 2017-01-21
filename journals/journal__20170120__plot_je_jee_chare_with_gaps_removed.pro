;;2017/01/20
PRO JOURNAL__20170120__PLOT_JE_JEE_CHARE_WITH_GAPS_REMOVED

  COMPILE_OPT IDL2

  startOrb    = 500            ;Otherwise it just picks the first orbit in eSpec
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
                          /LOAD_DELTA_T
  ENDIF
  DBString       = GET_NEWELL_DB_STRING(NEWELL__eSpec)+'--killedGap_inds--'
  CHECK_SORTED,NEWELL__eSpec.orbit,is_sorted,/QUIET

  IF ~is_sorted THEN STOP
  
  uniqOrb_i      = UNIQ(NEWELL__eSpec.orbit)
  
  orbs           = NEWELL__eSpec.orbit[uniqOrb_i]
  curOrb         = KEYWORD_SET(startOrb) ? startOrb : orbs[0]
  endOrb         = orbs[-1]
  lastSavOrb     = curOrb
  deltaSavOrb    = 500
  orbInd         = KEYWORD_SET(startOrb) ? WHERE(orbs EQ startOrb,/NULL) : 0
  junk_i         = !NULL
  befStart_i     = !NULL
  junkTimes      = !NULL
  befStartTimes  = !NULL
  missingIntervals = !NULL
  
  ;;vals for plot
  LOADCT2,39
  junkTransVal = 0
  junkStartVal = 1
  TSVal        = 2
  Val0312      = 3
  Val0628      = 4
  Val251       = 5
  ValUnsafe    = 6

  junkTransCol = 25
  junkStartCol = 50
  TSCol        = 75
  Col0312      = 100
  Col0628      = 125
  Col251       = 150
  ColUnsafe    = 175

  junkTransPSym = 1
  junkStartPSym = 2
  TSPSym        = 4
  PSym0312      = 5
  PSym0628      = 6
  PSym251       = 1
  PSymUnsafe    = 2

  IF KEYWORD_SET(showPlots) THEN BEGIN
     IF KEYWORD_SET(savePS) THEN BEGIN
     ENDIF ELSE BEGIN
        WINDOW,0,XSIZE=800,YSIZE=600
     ENDELSE
  ENDIF

  final = 0
  WHILE curOrb LE endOrb DO BEGIN

     IF (curOrb GT (lastSavOrb + deltaSavOrb - 1)) OR final THEN BEGIN
        ;;clear vars 
        junk_i         = !NULL
        befStart_i     = !NULL
        junkTimes      = !NULL
        befStartTimes  = !NULL
        missingIntervals = !NULL 

        orbRangeString = STRING(FORMAT='(I0,"-",I0)',lastSavOrb,lastSavOrb+deltaSavOrb-1)
        orbSavFileName = STRING(FORMAT='(A0,A0,".sav")',DBString,orbRangeString)

        RESTORE,saveDir+orbSavFileName

        PRINT,STRING(FORMAT='("Restored ",I0," junk inds and ",I0," befStart inds for orbs ",A0," to file : ",A0)', $
                     N_ELEMENTS(junk_i),N_ELEMENTS(befStart_i),orbRangeString,orbSavFileName)

        lastSavOrb += deltaSavOrb

        IF final THEN BREAK
     ENDIF

     orbString = STRCOMPRESS(curOrb,/REMOVE_ALL)
     tmp_i     = CGSETDIFFERENCE(WHERE(NEWELL__eSpec.orbit EQ curOrb[0],NBef),junk_i,COUNT=nAftJunk)
     IF nAftJunk EQ 0 THEN STOP
     tmp_i     = CGSETDIFFERENCE(tmp_i,befStart_i,COUNT=nAftBefTimes)
     tmpTime   = NEWELL__eSpec.x[tmp_i]
     IF nAftBefTimes EQ 0 THEN STOP
     IF NTmp EQ 0 THEN BEGIN
        PRINT,"no inds for orbit " + STRCOMPRESS(curOrb,/REMOVE_ALL) + ". Move it on down."
        curOrb = (orbs[++orbInd])[0]
        CONTINUE
     ENDIF
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Plots

     IF KEYWORD_SET(showPlots) THEN BEGIN

        IF KEYWORD_SET(savePS) THEN BEGIN
           POPEN,PSDir+PSPref+orbString, $
                 XSIZE=6, $
                 YSIZE=6, $
                 /QUIET,/LAND
        ENDIF

        date_label = LABEL_DATE(DATE_FORMAT = ['%I:%S','%D-%H'])
        tJul       = UTC_TO_JULDAY(tmpTime)

        PLOT,tJul,MAKE_ARRAY(N_ELEMENTS(tJul),VALUE=TSVal), $
             TITLE='Orbit ' + orbString + ' (' + TIME_TO_STR(tmpTime[0],/MS) + ')', $
             /NODATA, $
             ;; PSYM=TSPSym, $
             ;; COLOR=TSCol, $
             ;; XTICKFORMAT='LABEL_DATE', $
             XTICKFORMAT='(A1)', $
             XTICKUNITS=['Time','Time'], $
             XMARGIN=[12,2], $
             ;; YRANGE=[-0.1,6.1], $
             YSTYLE=1, $
             YMARGIN=[8,2]

        OPLOT,tJul,NEWELL__eSpec.je[tmp_i], $
              PSYM=TSPSym, $
              COLOR=TSCol       ;, $
        ;; XTICKFORMAT='LABEL_DATE', $
        ;; XTICKUNITS=['Time','Time'], $
        ;; XMARGIN=[12,2], $
        ;; YRANGE=[-0.1,4.1], $
        ;; YSTYLE=1, $
        ;; YTICKV=[0,1,2,3,4], $
        ;; YTICKNAME=["SRate Trans","Interv Start","T Series",'dt=0.628','dt=2.51'], $
        ;; YMARGIN=[8,2]





     ENDIF

     IF KEYWORD_SET(savePS) THEN BEGIN
        PCLOSE,/QUIET
        EPS2PDF,PSDir+PSPref+orbString, $
                /PS, $
                /TO_PNG, $
                /REMOVE_EPS, $
                /QUIET
     ENDIF

     STOP

     curOrb           = (orbs[++orbInd])[0]

     IF curOrb EQ endOrb THEN final = 1

  ENDWHILE

  PRINT,"FINISHED!"
  STOP

END


