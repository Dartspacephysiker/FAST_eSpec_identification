;;01/18/17
PRO JOURNAL__20170118__LOOK_FOR_GAPS_AND_REMOVE

  COMPILE_OPT IDL2

  startOrb    = 16000             ;Otherwise it just picks the first orbit in eSpec
  showPlots   = 1
  savePS      = 1
  PSDir       = '/SPENCEdata/Research/Satellites/FAST/espec_identification/plots/201701--trim_transitions/'
  PSPref      = 'junk_transitions--'
  
  saveDir     = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'

  @common__newell_espec.pro

  ;; LOAD_NEWELL_ESPEC_DB,eSpec,!NULL,NEWELL__delta_t, $
  ;; /LOAD_DELTA_T, $
  ;; /NO_MEMORY_LOAD
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,!NULL,!NULL,!NULL, $
                          /LOAD_DELTA_T
  ENDIF
  DBString       = GET_NEWELL_DB_STRING(NEWELL__eSpec)+'--killedGap_inds'
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

  PRINT,FORMAT='(A0,T10,A0,T20,A0,T30,A0,T40,A0,T50,' $
        + 'A0,T60,A0,T70,A0)', $
        "Orbits","N Streak","N 2.51","N 0.628","N 0.312", $
        "N Unsafe","N Dupe Elems","MissedItvls"
  final = 0
  WHILE curOrb LE endOrb DO BEGIN

     ;;Reset inds for this orbit
     befStart_ii       = !NULL
     junk_ii           = !NULL
     nDupes            = 0
     nMissing          = 0

     IF (curOrb GT (lastSavOrb + deltaSavOrb - 1)) OR final THEN BEGIN
        orbRangeString = STRING(FORMAT='(I0,"-",I0)',lastSavOrb,lastSavOrb+deltaSavOrb-1)
        orbSavFileName = STRING(FORMAT='(A0,A0,".sav")',DBString,orbRangeString)

        eSpec_info     = NEWELL__eSpec.info
        PRINT,STRING(FORMAT='("Saving ",I0," junk inds and ",I0," befStart inds for orbs ",A0," to file : ",A0)', $
                     N_ELEMENTS(junk_i),N_ELEMENTS(befStart_i),orbRangeString,orbSavFileName)

        SAVE,junk_i,befStart_i,junkTimes,befStartTimes, $
             missingIntervals, $
             eSpec_info, $
             FILENAME=saveDir+orbSavFileName

        junk_i         = !NULL
        befStart_i     = !NULL
        junkTimes      = !NULL
        befStartTimes  = !NULL
        missingIntervals = !NULL 
        lastSavOrb += deltaSavOrb

        IF final THEN BREAK
     ENDIF

     orbString = STRCOMPRESS(curOrb,/REMOVE_ALL)
     tmp_i     = WHERE(NEWELL__eSpec.orbit EQ curOrb[0],NTmp)
     IF NTmp EQ 0 THEN BEGIN
        PRINT,"no inds for orbit " + STRCOMPRESS(curOrb,/REMOVE_ALL) + ". Move it on down."
        curOrb = (orbs[++orbInd])[0]
        CONTINUE
     ENDIF
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;First we worry about junking the first ten seconds in each hemisphere
     this         = !NULL
     this         = LOAD_JE_AND_JE_TIMES_FOR_ORB(curOrb,TIME_RANGES_OUT=tRanges,/USE_DUPELESS_FILES)
     ;;Make sure we junk the first ten seconds, if we haven't already, when entering auroral zone in either hemisphere
     nLoops       = this NE -1
     IF nLoops GT 0 THEN BEGIN

        intStartTs   = tRanges[*,0]
        CHECK_SORTED,intStartTs,sorted_intervals,/QUIET
        IF ~sorted_intervals THEN STOP
        
        extreme_ii   = !NULL
        interval_ii  = VALUE_CLOSEST2(NEWELL__eSpec.x[tmp_i],intStartTs, $
                                      EXTREME_I=extreme_ii, $
                                      ONLY_HAVE_EXTREME=onlyExtreme)

        IF N_ELEMENTS(extreme_ii) GT 0 THEN BEGIN
           lt0_iii = WHERE(extreme_ii EQ -1,nLT0)
           gtN_iii = WHERE(extreme_ii EQ NTmp,nGTN)
           
           IF nLT0 GT 0 THEN BEGIN
              interval_ii[lt0_iii] = 0
           ENDIF

           IF nGTN GT 0 THEN BEGIN
              interval_ii[gtN_iii] = NTmp-1
           ENDIF

        ENDIF

        goodIntMatch = WHERE(ABS(NEWELL__eSpec.x[tmp_i[interval_ii]]-intStartTs) LT 5, $
                             nGoodIntMatch, $
                             COMPLEMENT=badIntMatch, $
                             NCOMPLEMENT=nBadIntMatch)

        IF nGoodIntMatch NE N_ELEMENTS(intStartTs) THEN BEGIN

           ;;Let's see if we just don't have times for this interval
           intStopTs = tRanges[*,1] 
           intStop_ii = VALUE_CLOSEST2(NEWELL__eSpec.x[tmp_i],intStopTs) 
           goodStopMatch = WHERE(ABS(NEWELL__eSpec.x[tmp_i[intStop_ii]]-intStopTs) LT 5, $
                                 nGoodStopMatch, $
                                 COMPLEMENT=badStopMatch, $
                                 NCOMPLEMENT=nBadStopMatch)

           missingInterval = WHERE(badIntMatch EQ badStopMatch,nMissing)
           IF nMissing GT 0 THEN BEGIN

              IF (nMissing + nGoodIntMatch) EQ N_ELEMENTS(intStartTs) THEN BEGIN
                 ;; PRINT,FORMAT='("K, so you''re missing these intervals: ",10(I0,:,", "))',badIntMatch[missingInterval]
                 FOR m=0,nMissing-1 DO BEGIN
                    missingIntervals = [[missingIntervals],[curOrb,badIntMatch[missingInterval[m]]]]
                 ENDFOR

              ENDIF ELSE BEGIN

                 PRINT,"Un lío! The number of times that NEWELL__eSpec matches the ESA interval start times is " + $
                       "fewer than the number of ESA interval start times for this orbit!" 
                 STOP

              ENDELSE

           ENDIF

        ENDIF

        north_iii   = WHERE(NEWELL__eSpec.ilat[tmp_i[interval_ii]] GT 0.0,nNorth3)
        south_iii   = WHERE(NEWELL__eSpec.ilat[tmp_i[interval_ii]] LT 0.0,nSouth3)

        IF nNorth3 GT 0 THEN BEGIN
           tmpBefStart_ii  = WHERE(ABS(NEWELL__eSpec.x[tmp_i] - (intStartTs[north_iii[0]])) LE 10,nBefStart)
           IF nBefStart GT 0 THEN BEGIN
              ;; befStart_i   = [befStart_i,tmp_i[tmpBefStart_ii]]
              befStart_ii  = [befStart_ii,tmpBefStart_ii]
           ENDIF
        ENDIF

        IF nSouth3 GT 0 THEN BEGIN
           tmpBefStart_ii  = WHERE(ABS(NEWELL__eSpec.x[tmp_i] - (intStartTs[south_iii[0]])) LE 10,nBefStart)
           IF nBefStart GT 0 THEN BEGIN
              ;; befStart_i   = [befStart_i,tmp_i[tmpBefStart_ii]]
              befStart_ii  = [befStart_ii,tmpBefStart_ii]
           ENDIF
        ENDIF

     ENDIF ELSE BEGIN
        ;; PRINT,"No junkstart inds here!"
     ENDELSE

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Now we worry about transitions between sampling rates
     tmpTime = NEWELL__eSpec.x[tmp_i]
     delta   = 0.08
     dPlace  = -2
     nMin    = 4
     GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE,tmpTime-tmpTime[0],-2, $
                                        N=nMin, $
                                        DELTA=delta, $
                                        START_I=start_ii, $
                                        STOP_I=stop_ii, $
                                        STREAKLENS=streakLens, $
                                        OUT_RATES=rates, $
                                        FLOOR=floor, $
                                        CEILING=ceiling, $
                                        BADSTART_I=badStart_ii, $
                                        BADSTOP_I=badStop_ii, $
                                        BADSTREAKLENS=badStreakLens, $
                                        OUT_BADRATES=badRates


     ;;Junk any streaks smaller than nMin
     nBadStreaks   = N_ELEMENTS(badStart_ii)
     FOR k=0,nBadStreaks-1 DO BEGIN
        ;; IF badStart_ii[k] EQ badStop_ii[k] THEN BEGIN
        ;;    tmpJunk_ii = badStart_ii
        tmpJunk_ii = [(badStart_ii[k]):badStop_ii[k]]
        junk_ii    = [junk_ii,tmpJunk_ii]
     ENDFOR

     sampDT    = !NULL
     nStreaks  = N_ELEMENTS(start_ii)
     IF nStreaks GT 0 THEN BEGIN

        FOR k=0,nStreaks-1 DO BEGIN
           ;; PRINT,tmpTime[(start_ii[k]+1):stop_ii[k]]-tmpTime[(start_ii[k]):(stop_ii[k]-1)]
           sampDT = [sampDT,MEAN(tmpTime[(start_ii[k]+1):stop_ii[k]]-tmpTime[(start_ii[k]):(stop_ii[k]-1)])]
        ENDFOR

        IF (WHERE(sampDT LE 0))[0] NE -1 THEN BEGIN
           PRINT,"A negative sample dt, huh? Tell me about it."
           STOP
        END

        samp251    = ABS(sampDT-2.51) LT 0.2
        samp0628   = ABS(sampDT-0.624) LT 0.1
        samp0312   = ABS(sampDT-0.312) LT 0.1
        w251       = WHERE(samp251,n251)
        w0628      = WHERE(samp0628,n0628)
        w0312      = WHERE(samp0312,n0312)
        safe       = WHERE(TEMPORARY(samp251) OR TEMPORARY(samp0628) OR TEMPORARY(samp0312), $
                           nSafeStreaks,COMPLEMENT=wUnsafe,NCOMPLEMENT=nUnsafe)

        IF nSafeStreaks NE nStreaks THEN BEGIN
           PRINT,FORMAT='(A0,": ","Unsafe sampDTs!! (",' + STRCOMPRESS(nUnsafe) + '(F0.2,:,", "),A0)',orbSTring,sampDT[wUnsafe],")"
           ;; STOP

           ;;Junk all the places where we transition from 2.51-s sample period to something else
           RM_INTERVAL,tmpTime,wUnsafe,wUnsafe_ii,nUnsafe,start_ii,stop_ii,sampDT,junk_ii
           ;;    IF nTmpJunk GT 0 THEN BEGIN
           ;;       ;; junk_i  = [junk_i,tmp_i[tmpJunk_ii]]
           ;;       junk_ii = [junk_ii,tmpJunk_ii]
           ;;    ENDIF
           ;; ENDFOR
        ENDIF

        ;;Junk all the places where we transition from 2.51-s sample period to something else
        RM_INTERVAL,tmpTime,w251,w251_ii,n251,start_ii,stop_ii,sampDT,junk_ii
        ;; w251_ii = !NULL
        ;; FOR k=0,n251-1 DO BEGIN
        ;;    tmpK       = w251[k]
        ;;    w251_ii    = [w251_ii,[start_ii[tmpK]:stop_ii[tmpK]]]
        ;;    tmpJunk_ii = WHERE((tmpTime GE (tmpTime[stop_ii[tmpK]]-sampDT[tmpK]*3.1)) AND $
        ;;                       (tmpTime LE (tmpTime[stop_ii[tmpK]]+sampDT[tmpK]*3.1)), $
        ;;                       nTmpJunk)

        ;;    IF nTmpJunk GT 0 THEN BEGIN
        ;;       ;; junk_i  = [junk_i,tmp_i[tmpJunk_ii]]
        ;;       junk_ii = [junk_ii,tmpJunk_ii]
        ;;    ENDIF
        ;; ENDFOR

        ;;Junk all the places where we transition from 0.628-s sample period to something else
        w0628_ii = !NULL
        RM_INTERVAL,tmpTime,w0628,w0628_ii,n0628,start_ii,stop_ii,sampDT,junk_ii
        ;; FOR k=0,n0628-1 DO BEGIN
        ;;    tmpK       = w0628[k]
        ;;    w0628_ii   = [w0628_ii,[start_ii[tmpK]:stop_ii[tmpK]]]
        ;;    tmpJunk_ii = WHERE((tmpTime GE (tmpTime[stop_ii[tmpK]]-sampDT[tmpK]*3.1)) AND $
        ;;                       (tmpTime LE (tmpTime[stop_ii[tmpK]]+sampDT[tmpK]*3.1)), $
        ;;                       nTmpJunk)

        ;;    IF nTmpJunk GT 0 THEN BEGIN
        ;;       ;; junk_i  = [junk_i,tmp_i[tmpJunk_ii]]
        ;;       junk_ii = [junk_ii,tmpJunk_ii]
        ;;    ENDIF
        ;; ENDFOR

        ;;Junk all the places where we transition from 0.628-s sample period to something else
        RM_INTERVAL,tmpTime,w0312,w0312_ii,n0312,start_ii,stop_ii,sampDT,junk_ii
        ;; w0312_ii = !NULL
        ;; FOR k=0,n0312-1 DO BEGIN
        ;;    tmpK       = w0312[k]
        ;;    w0312_ii   = [w0312_ii,[start_ii[tmpK]:stop_ii[tmpK]]]
        ;;    tmpJunk_ii = WHERE((tmpTime GE (tmpTime[stop_ii[tmpK]]-sampDT[tmpK]*3.1)) AND $
        ;;                       (tmpTime LE (tmpTime[stop_ii[tmpK]]+sampDT[tmpK]*3.1)), $
        ;;                       nTmpJunk)

        ;;    IF nTmpJunk GT 0 THEN BEGIN
        ;;       ;; junk_i  = [junk_i,tmp_i[tmpJunk_ii]]
        ;;       junk_ii = [junk_ii,tmpJunk_ii]
        ;;    ENDIF
        ;; ENDFOR

     ENDIF

     IF KEYWORD_SET(showPlots) THEN BEGIN

        IF KEYWORD_SET(savePS) THEN BEGIN
           POPEN,PSDir+PSPref+orbString, $
                 XSIZE=6, $
                 YSIZE=4.5, $
                 /QUIET
        ENDIF

        date_label = LABEL_DATE(DATE_FORMAT = ['%I:%S','%D-%H'])
        tJul       = UTC_TO_JULDAY(tmpTime)

        PLOT,tJul,MAKE_ARRAY(N_ELEMENTS(tJul),VALUE=TSVal), $
             TITLE='Orbit ' + orbString + ' (' + TIME_TO_STR(tmpTime[0],/MS) + ')', $
             /NODATA, $
             ;; PSYM=TSPSym, $
             ;; COLOR=TSCol, $
             XTICKFORMAT='LABEL_DATE', $
             XTICKUNITS=['Time','Time'], $
             XMARGIN=[12,2], $
             YRANGE=[-0.1,6.1], $
             YSTYLE=1, $
             YTICKV=[0,1,2,3,4,5,6], $
             YTICKNAME=["SRate Trans","Interv Start","T Series",'dt=0.312','dt=0.628','dt=2.51','dt=unsafe'], $
             YMARGIN=[8,2]

        OPLOT,tJul,MAKE_ARRAY(N_ELEMENTS(tJul),VALUE=TSVal), $
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

        IF N_ELEMENTS(junk_ii) GT 0 THEN BEGIN
           OPLOT,tJul[junk_ii],MAKE_ARRAY(N_ELEMENTS(junk_ii),VALUE=junkTransVal), $
                 PSYM=junkTransPSym, $
                 COLOR=junkTransCol ;, $
           ;; XTICKFORMAT='LABEL_DATE', $
           ;; XTICKUNITS=['Time','Time']
        ENDIF

        IF N_ELEMENTS(befStart_ii) GT 0 THEN BEGIN
           OPLOT,tJul[befStart_ii],MAKE_ARRAY(N_ELEMENTS(befStart_ii),VALUE=junkStartVal), $
                 PSYM=junkStartPSym, $
                 COLOR=junkStartCol ;, $
           ;; XTICKFORMAT='LABEL_DATE', $
           ;; XTICKUNITS=['Time','Time']                 
        ENDIF

        IF N_ELEMENTS(wUnsafe_ii) GT 0 THEN BEGIN
           OPLOT,tJul[wUnsafe_ii],MAKE_ARRAY(N_ELEMENTS(wUnsafe_ii),VALUE=ValUnsafe), $
                 PSYM=PSymUnsafe, $
                 COLOR=ColUnsafe ;, $
           ;; XTICKFORMAT='LABEL_DATE', $
           ;; XTICKUNITS=['Time','Time']                 
        ENDIF

        IF N_ELEMENTS(w251_ii) GT 0 THEN BEGIN
           OPLOT,tJul[w251_ii],MAKE_ARRAY(N_ELEMENTS(w251_ii),VALUE=Val251), $
                 PSYM=PSym251, $
                 COLOR=Col251   ;, $
           ;; XTICKFORMAT='LABEL_DATE', $
           ;; XTICKUNITS=['Time','Time']                 
        ENDIF

        IF N_ELEMENTS(w0628_ii) GT 0 THEN BEGIN
           OPLOT,tJul[w0628_ii],MAKE_ARRAY(N_ELEMENTS(w0628_ii),VALUE=Val0628), $
                 PSYM=PSym0628, $
                 COLOR=Col0628  ;, $
           ;; XTICKFORMAT='LABEL_DATE', $
           ;; XTICKUNITS=['Time','Time']           
        ENDIF

        IF N_ELEMENTS(w0312_ii) GT 0 THEN BEGIN
           OPLOT,tJul[w0312_ii],MAKE_ARRAY(N_ELEMENTS(w0312_ii),VALUE=Val0312), $
                 PSYM=PSym0312, $
                 COLOR=Col0312  ;, $
           ;; XTICKFORMAT='LABEL_DATE', $
           ;; XTICKUNITS=['Time','Time']           
        ENDIF     

     ENDIF

     IF KEYWORD_SET(savePS) THEN BEGIN
        PCLOSE,/QUIET
        EPS2PDF,PSDir+PSPref+orbString, $
                /PS, $
                /TO_PNG, $
                /REMOVE_EPS, $
                /QUIET
     ENDIF

     ;;Now store 'em up!
     IF N_ELEMENTS(befStart_ii) GT 0 THEN BEGIN
        befStart_i    = [befStart_i,tmp_i[befStart_ii]]
        befStartTimes = [befStartTimes,tmpTime[befStart_ii]]
     ENDIF

     IF N_ELEMENTS(junk_ii) GT 0 THEN BEGIN

        CHECK_SORTED,junk_ii,junk_sorted,/QUIET

        ;;If they're not sorted, they may not be unique either. We simply must know
        IF ~junk_sorted THEN BEGIN

           nUniqJunk = N_ELEMENTS(UNIQ(junk_ii,SORT(junk_ii)))
           nJunk     = N_ELEMENTS(junk_ii)
           IF nUniqJunk NE nJunk THEN BEGIN
              ;; PRINT,"Say what??? Ridding us of " + STRCOMPRESS(nJunk-nUniqJunk) + " duplicate junkers"
              nDupes = nJunk-nUniqJunk
              junk_ii = junk_ii[UNIQ(junk_ii,SORT(junk_ii))]
           ENDIF ELSE BEGIN
              junk_ii = junk_ii[SORT(junk_ii)]
           ENDELSE

        ENDIF
        
        junk_i        = [junk_i,tmp_i[junk_ii]]
        junkTimes     = [junkTimes,tmpTime[junk_ii]]

     ENDIF

     PRINT,FORMAT='(A0,T10,I0,T20,I0,T30,I0,T40,I0,T50,I0,' $
           + 'T60,I0,T70,I0)', $
           orbString,nStreaks,n251,n0628,n0312, $
           nUnsafe,nDupes,nMissing

     curOrb           = (orbs[++orbInd])[0]

     IF curOrb EQ endOrb THEN final = 1

  ENDWHILE

  ;;Finish up
  IF curOrb GT lastSavOrb THEN BEGIN
     orbRangeString = STRING(FORMAT='(I0,"-",I0)',lastSavOrb,curOrb)
     orbSavFileName = STRING(FORMAT='("esa_transit_times--",A0,".sav")',orbRangeString)

     eSpec_info     = NEWELL__eSpec.info
     PRINT,STRING(FORMAT='("Saving ",I0," junk inds and ",I0," befStart inds for orbs ",A0," to file : ",A0)', $
                  N_ELEMENTS(junk_i),N_ELEMENTS(befStart_i),orbRangeString,orbSavFileName)

     SAVE,junk_i,befStart_i,junkTimes,befStartTimes, $
          missingIntervals, $
          eSpec_info, $
          FILENAME=saveDir+orbSavFileName

  ENDIF

  PRINT,"FINISHED!"
  STOP

END

PRO RM_INTERVAL,tmpTime,wCheck,wCheck_ii,nCheck,start_ii,stop_ii,sampDT,junk_ii
  wCheck_ii = !NULL

  befs = (sampDT*3.1) < 5
  afts = (sampDT*3.1) < 5
  FOR k=0,nCheck-1 DO BEGIN
     tmpK       = wCheck[k]
     wCheck_ii = [wCheck_ii,[start_ii[tmpK]:stop_ii[tmpK]]]
     tmpJunk_ii = WHERE((tmpTime GE (tmpTime[stop_ii[tmpK]]-befs[tmpK])) AND $
                        (tmpTime LE (tmpTime[stop_ii[tmpK]]+afts[tmpK])), $
                        nTmpJunk)

     IF nTmpJunk GT 0 THEN BEGIN
        ;; junk_i  = [junk_i,tmp_i[tmpJunk_ii]]
        junk_ii = [junk_ii,tmpJunk_ii]
     ENDIF ELSE BEGIN
        IF sampDT[tmpK] LT 0. THEN BEGIN

        ENDIF
     ENDELSE
  ENDFOR
END