;;2017/01/25
PRO JOURNAL__20170125__PLOT_JE_JEE_CHARE_WITH_GAPS_REMOVED__TPLOT_VERSION

  COMPILE_OPT IDL2

  startOrb    = 1381            ;Otherwise it just picks the first orbit in eSpec
  endOrb      = 16361
  savePS      = 1
  PSDir       = '/SPENCEdata/Research/Satellites/FAST/espec_identification/plots/20170125--trim_transitions__tplot/'
  PSPref      = 'je_jee_chare_sans_transitions--'

  check_existing = 1
  
  @common__newell_espec.pro

  ;; LOAD_NEWELL_ESPEC_DB,eSpec,!NULL,NEWELL__delta_t, $
  ;; /LOAD_DELTA_T, $
  ;; /NO_MEMORY_LOAD
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,!NULL,!NULL,!NULL, $
                          /LOAD_CHARE, $
                          /DONT_CONVERT_TO_STRICT_NEWELL
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

  ;;plot setup
  LOADCT2,39


  IF KEYWORD_SET(savePS) THEN BEGIN
  ENDIF ELSE BEGIN
     WINDOW,0,XSIZE=800,YSIZE=600
  ENDELSE

  final = 0
  loaded           = 0B
  WHILE curOrb LE endOrb DO BEGIN

     ;;Ind things
     orbString  = STRCOMPRESS(curOrb,/REMOVE_ALL)
     tmp_i      = WHERE(NEWELL__eSpec.orbit EQ curOrb[0],NBef)
     firstLastT = NEWELL__eSpec.x[[tmp_i[0],tmp_i[-1]]]

     outPlot    = PSDir+PSPref+orbString

     IF KEYWORD_SET(check_existing) THEN BEGIN
        fT1     = FILE_TEST(outPlot+'.ps')
        fT2     = FILE_TEST(outPlot+'00.png')
        IF fT1 THEN gotEr = outPlot+'.ps' $
        ELSE IF fT2 THEN gotEr = outPlot+'.ps'

        IF fT1 OR fT2 THEN BEGIN
           PRINT,"File exists: " + gotEr + '! Skipping ...'

           curOrb = (orbs[++orbInd])[0]
           IF curOrb EQ endOrb THEN final = 1

           CONTINUE

        ENDIF
     ENDIF

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

     IF KEYWORD_SET(savePS) THEN BEGIN
        POPEN,outPlot, $
              XSIZE=12, $
              YSIZE=8, $
              /QUIET

        LOADCT2,39
     ENDIF

     PLOT_ESPEC_JE_JEE_CHARE_ORBSUMMARY__TPLOT, $
        curOrb, $
        /NEWELL2009_PANEL, $
        /USE_MY_JUNK_AND_BEFSTART, $
        JUNK_I=junk_i, $
        BEFSTART_I=befStart_i

     IF KEYWORD_SET(savePS) THEN BEGIN
        PCLOSE,/QUIET
        EPS2PDF,outPlot, $
                /PS, $
                /TO_PNG, $
                /REMOVE_EPS, $
                /QUIET
     ENDIF ELSE BEGIN
        PRINT,"Stopped because otherwise I'm just going to show a million plots, probably for no good reason."
        STOP
     ENDELSE

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


