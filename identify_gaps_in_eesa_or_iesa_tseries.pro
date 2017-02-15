;;02/09/17
PRO IDENTIFY_GAPS_IN_EESA_OR_IESA_TSERIES, $
   TIMES_UTC=times, $
   DIFFTTHRESH=diffTThresh, $
   ORBIT_ARRAY=orbit, $
   ILAT_ARRAY=ilat, $
   INFO_FOR_STRUCT=info, $
   STARTORB=startOrb, $
   SAVEFILEPREF=saveFilePref, $
   SAVEDIR=saveDir, $
   SHOWPLOTS=showPlots, $
   SAVEPS=savePS, $
   PSDIR=PSDir, $
   PSPREF=PSPref

  COMPILE_OPT IDL2

  uniqOrb_i      = UNIQ(orbit)
  
  diffTThresh    = KEYWORD_SET(diffTThresh) ? diffTThresh : 5

  uniqOrbs       = orbit[uniqOrb_i]
  curOrb         = KEYWORD_SET(startOrb) ? startOrb : uniqOrbs[0]
  endOrb         = uniqOrbs[-1]
  lastSavOrb     = curOrb
  deltaSavOrb    = 500
  orbInd         = KEYWORD_SET(startOrb) ? WHERE(uniqOrbs EQ startOrb,/NULL) : 0
  junk_i         = !NULL
  befStart_i     = !NULL
  junkTimes      = !NULL
  befStartTimes  = !NULL
  missingIntervals = !NULL
  
  ;;vals for plot
  LOADCT2,39

  IF KEYWORD_SET(showPlots) THEN BEGIN
     IF KEYWORD_SET(savePS) THEN BEGIN
     ENDIF ELSE BEGIN
        WINDOW,0,XSIZE=800,YSIZE=600
     ENDELSE
  ENDIF

  PRINT,FORMAT='(A0,T10,A0,T20,A0,T30,A0,T40,A0,T50,' $
        + 'A0,T60,A0,T70,A0,T80,A0)', $
        "Orbits","N Streak","N 2.51","N 0.628","N 0.312", $
        "N Unsafe","N Dupe Elems","MissedItvls","N 1.27"
  WHILE curOrb LE endOrb DO BEGIN

     ;;Reset inds for this orbit
     befStart_ii       = !NULL
     junk_ii           = !NULL
     nDupes            = 0
     nMissing          = 0

     IF curOrb GT (lastSavOrb + deltaSavOrb - 1) THEN BEGIN
        orbRangeString = STRING(FORMAT='(I0,"-",I0)',lastSavOrb,lastSavOrb+deltaSavOrb-1)
        orbSavFileName = STRING(FORMAT='(A0,A0,".sav")',saveFilePref,orbRangeString)

        PRINT,STRING(FORMAT='("Saving ",I0," junk inds and ",I0," befStart inds for orbs ",A0," to file : ",A0)', $
                     N_ELEMENTS(junk_i),N_ELEMENTS(befStart_i),orbRangeString,orbSavFileName)

        SAVE,junk_i,befStart_i,junkTimes,befStartTimes, $
             missingIntervals, $
             info, $
             FILENAME=saveDir+orbSavFileName

        junk_i         = !NULL
        befStart_i     = !NULL
        junkTimes      = !NULL
        befStartTimes  = !NULL
        missingIntervals = !NULL 
        lastSavOrb += deltaSavOrb
     ENDIF

     orbString = STRCOMPRESS(curOrb,/REMOVE_ALL)
     tmp_i     = WHERE(orbit EQ curOrb[0],NTmp)

     contPlease = 0
     CASE 1 OF
        (NTmp EQ 0): BEGIN
           PRINT,"no inds for orbit " + STRCOMPRESS(curOrb,/REMOVE_ALL) + ". Move it on down."
           curOrb = (uniqOrbs[++orbInd])[0]
           contPlease = 1
        END
        (NTmp LE 5): BEGIN
           PRINT,"LT 5 inds for orbit " + STRCOMPRESS(curOrb,/REMOVE_ALL) + ". Move it on down."
           curOrb = (uniqOrbs[++orbInd])[0]
           contPlease = 1
        END
        ELSE:
     ENDCASE
     
     IF contPlease THEN CONTINUE

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;First we worry about junking the first ten seconds in each hemisphere
     tmpTime      = times[tmp_i]

     this         = !NULL
     this         = LOAD_JE_AND_JE_TIMES_FOR_ORB(curOrb,TIME_RANGES_OUT=tRanges,/USE_DUPELESS_FILES)
     ;;Make sure we junk the first ten seconds, if we haven't already, when entering auroral zone in either hemisphere
     nLoops       = this NE -1
     IF nLoops GT 0 AND (N_ELEMENTS(tmpTime) GT 5) THEN BEGIN

        intStartTs   = tRanges[*,0]
        CHECK_SORTED,intStartTs,sorted_intervals,/QUIET
        IF ~sorted_intervals THEN STOP
        
        extreme_ii   = !NULL        
        interval_ii  = VALUE_CLOSEST2(tmpTime,intStartTs, $
                                      EXTREME_I=extreme_i, $
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
        
        goodIntMatch = WHERE(ABS(tmpTime[interval_ii]-intStartTs) LT diffTThresh, $
                             nGoodIntMatch, $
                             COMPLEMENT=badIntMatch, $
                             NCOMPLEMENT=nBadIntMatch)

        IF nGoodIntMatch NE N_ELEMENTS(intStartTs) THEN BEGIN

           ;;Let's see if we just don't have times for this interval
           intStopTs = tRanges[*,1] 
           intStop_ii = VALUE_CLOSEST2(tmpTime,intStopTs) 
           goodStopMatch = WHERE(ABS(tmpTime[intStop_ii]-intStopTs) LT diffTThresh, $
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

        north_iii   = WHERE(ilat[tmp_i[interval_ii]] GT 0.0,nNorth3)
        south_iii   = WHERE(ilat[tmp_i[interval_ii]] LT 0.0,nSouth3)

        IF nNorth3 GT 0 THEN BEGIN
           tmpBefStart_ii  = WHERE(ABS(tmpTime - (intStartTs[north_iii[0]])) LE 10,nBefStart)
           IF nBefStart GT 0 THEN BEGIN
              ;; befStart_i   = [befStart_i,tmp_i[tmpBefStart_ii]]
              befStart_ii  = [befStart_ii,tmpBefStart_ii]
           ENDIF
        ENDIF

        IF nSouth3 GT 0 THEN BEGIN
           tmpBefStart_ii  = WHERE(ABS(tmpTime - (intStartTs[south_iii[0]])) LE 10,nBefStart)
           IF nBefStart GT 0 THEN BEGIN
              ;; befStart_i   = [befStart_i,tmp_i[tmpBefStart_ii]]
              befStart_ii  = [befStart_ii,tmpBefStart_ii]
           ENDIF
        ENDIF

     ENDIF ELSE BEGIN
        PRINT,"Nothing here for orbit " + STRCOMPRESS(curOrb,/REMOVE_ALL) + "!"
     ENDELSE

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Now we worry about transitions between sampling rates
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
        samp127    = ABS(sampDT-1.27) LT 0.2
        samp0628   = ABS(sampDT-0.624) LT 0.1
        samp0312   = ABS(sampDT-0.312) LT 0.1
        w251       = WHERE(samp251,n251)
        w127       = WHERE(samp127,n127)
        w0628      = WHERE(samp0628,n0628)
        w0312      = WHERE(samp0312,n0312)
        ;; safe       = WHERE(TEMPORARY(samp251) OR TEMPORARY(samp0628) OR TEMPORARY(samp0312), $
        safe       = WHERE(TEMPORARY(samp251) OR TEMPORARY(samp127) OR TEMPORARY(samp0628) OR TEMPORARY(samp0312), $
                           nSafeStreaks,COMPLEMENT=wUnsafe,NCOMPLEMENT=nUnsafe)

        IF nSafeStreaks NE nStreaks THEN BEGIN
           PRINT,FORMAT='(A0,": ","Unsafe sampDTs!! (",' + STRCOMPRESS(nUnsafe) + '(F0.2,:,", "),A0)',orbSTring,sampDT[wUnsafe],")"

           ;;Junk all the places where we transition from 2.51-s sample period to something else
           IDENTIFY_GAPS__RM_INTERVAL,tmpTime,wUnsafe,wUnsafe_ii,nUnsafe,start_ii,stop_ii,sampDT,junk_ii
        ENDIF

        ;;Junk all the places where we transition from 2.51-s sample period to something else
        IDENTIFY_GAPS__RM_INTERVAL,tmpTime,w251,w251_ii,n251,start_ii,stop_ii,sampDT,junk_ii

        ;;Junk all the places where we transition from 1.27-s sample period to something else
        IDENTIFY_GAPS__RM_INTERVAL,tmpTime,w127,w127_ii,n127,start_ii,stop_ii,sampDT,junk_ii

        ;;Junk all the places where we transition from 0.628-s sample period to something else
        IDENTIFY_GAPS__RM_INTERVAL,tmpTime,w0628,w0628_ii,n0628,start_ii,stop_ii,sampDT,junk_ii

        ;;Junk all the places where we transition from 0.628-s sample period to something else
        IDENTIFY_GAPS__RM_INTERVAL,tmpTime,w0312,w0312_ii,n0312,start_ii,stop_ii,sampDT,junk_ii

     ENDIF
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Plot everythang
     IF KEYWORD_SET(showPlots) THEN BEGIN

        IF KEYWORD_SET(savePS) THEN BEGIN
           POPEN,PSDir+PSPref+orbString, $
                 XSIZE=6, $
                 YSIZE=4.5, $
                 /QUIET
        ENDIF

        PLOT_ESPEC_SAMPLE_RATES_AND_TRANSITION_TIMES,curOrb, $
           tmpTime, $
           JUNK_II=junk_ii, $
           BEFSTART_II=befStart_ii, $
           WUNSAFE_II=wUnsafe_ii, $
           W251_II=w251_ii, $
           W0628_II=w0628_ii, $
           W0312_II=w0312_ii

        ;; date_label = LABEL_DATE(DATE_FORMAT = ['%I:%S','%D-%H'])
        ;; tJul       = UTC_TO_JULDAY(tmpTime)

        ;; PLOT,tJul,MAKE_ARRAY(N_ELEMENTS(tJul),VALUE=TSVal), $
        ;;      TITLE='Orbit ' + orbString + ' (' + TIME_TO_STR(tmpTime[0],/MS) + ')', $
        ;;      /NODATA, $
        ;;      ;; PSYM=TSPSym, $
        ;;      ;; COLOR=TSCol, $
        ;;      XTICKFORMAT='LABEL_DATE', $
        ;;      XTICKUNITS=['Time','Time'], $
        ;;      XMARGIN=[12,2], $
        ;;      YRANGE=[-0.1,6.1], $
        ;;      YSTYLE=1, $
        ;;      YTICKV=[0,1,2,3,4,5,6], $
        ;;      YTICKNAME=["SRate Trans","Interv Start","T Series",'dt=0.312','dt=0.628','dt=2.51','dt=unsafe'], $
        ;;      YMARGIN=[8,2]

        ;; OPLOT,tJul,MAKE_ARRAY(N_ELEMENTS(tJul),VALUE=TSVal), $
        ;;       PSYM=TSPSym, $
        ;;       COLOR=TSCol       ;, $
        ;; ;; XTICKFORMAT='LABEL_DATE', $
        ;; ;; XTICKUNITS=['Time','Time'], $
        ;; ;; XMARGIN=[12,2], $
        ;; ;; YRANGE=[-0.1,4.1], $
        ;; ;; YSTYLE=1, $
        ;; ;; YTICKV=[0,1,2,3,4], $
        ;; ;; YTICKNAME=["SRate Trans","Interv Start","T Series",'dt=0.628','dt=2.51'], $
        ;; ;; YMARGIN=[8,2]

        ;; IF N_ELEMENTS(junk_ii) GT 0 THEN BEGIN
        ;;    OPLOT,tJul[junk_ii],MAKE_ARRAY(N_ELEMENTS(junk_ii),VALUE=junkTransVal), $
        ;;          PSYM=junkTransPSym, $
        ;;          COLOR=junkTransCol ;, $
        ;;    ;; XTICKFORMAT='LABEL_DATE', $
        ;;    ;; XTICKUNITS=['Time','Time']
        ;; ENDIF

        ;; IF N_ELEMENTS(befStart_ii) GT 0 THEN BEGIN
        ;;    OPLOT,tJul[befStart_ii],MAKE_ARRAY(N_ELEMENTS(befStart_ii),VALUE=junkStartVal), $
        ;;          PSYM=junkStartPSym, $
        ;;          COLOR=junkStartCol ;, $
        ;;    ;; XTICKFORMAT='LABEL_DATE', $
        ;;    ;; XTICKUNITS=['Time','Time']                 
        ;; ENDIF

        ;; IF N_ELEMENTS(wUnsafe_ii) GT 0 THEN BEGIN
        ;;    OPLOT,tJul[wUnsafe_ii],MAKE_ARRAY(N_ELEMENTS(wUnsafe_ii),VALUE=ValUnsafe), $
        ;;          PSYM=PSymUnsafe, $
        ;;          COLOR=ColUnsafe ;, $
        ;;    ;; XTICKFORMAT='LABEL_DATE', $
        ;;    ;; XTICKUNITS=['Time','Time']                 
        ;; ENDIF

        ;; IF N_ELEMENTS(w251_ii) GT 0 THEN BEGIN
        ;;    OPLOT,tJul[w251_ii],MAKE_ARRAY(N_ELEMENTS(w251_ii),VALUE=Val251), $
        ;;          PSYM=PSym251, $
        ;;          COLOR=Col251   ;, $
        ;;    ;; XTICKFORMAT='LABEL_DATE', $
        ;;    ;; XTICKUNITS=['Time','Time']                 
        ;; ENDIF

        ;; IF N_ELEMENTS(w0628_ii) GT 0 THEN BEGIN
        ;;    OPLOT,tJul[w0628_ii],MAKE_ARRAY(N_ELEMENTS(w0628_ii),VALUE=Val0628), $
        ;;          PSYM=PSym0628, $
        ;;          COLOR=Col0628  ;, $
        ;;    ;; XTICKFORMAT='LABEL_DATE', $
        ;;    ;; XTICKUNITS=['Time','Time']           
        ;; ENDIF

        ;; IF N_ELEMENTS(w0312_ii) GT 0 THEN BEGIN
        ;;    OPLOT,tJul[w0312_ii],MAKE_ARRAY(N_ELEMENTS(w0312_ii),VALUE=Val0312), $
        ;;          PSYM=PSym0312, $
        ;;          COLOR=Col0312  ;, $
        ;;    ;; XTICKFORMAT='LABEL_DATE', $
        ;;    ;; XTICKUNITS=['Time','Time']           
        ;; ENDIF     

        IF KEYWORD_SET(savePS) THEN BEGIN
           PCLOSE,/QUIET
           EPS2PDF,PSDir+PSPref+orbString, $
                   /PS, $
                   /TO_PNG, $
                   /REMOVE_EPS, $
                   /QUIET
        ENDIF

     ENDIF

     PRINT,FORMAT='(A0,T10,I0,T20,I0,T30,I0,T40,I0,T50,I0,' $
           + 'T60,I0,T70,I0,T80,I0)', $
           orbString,nStreaks,n251,n0628,n0312, $
           nUnsafe,nDupes,nMissing,n127

     curOrb           = (uniqOrbs[++orbInd])[0]

     IF curOrb EQ endOrb THEN BREAK

  ENDWHILE

  ;;Finish up
  IF curOrb GT lastSavOrb THEN BEGIN
     orbRangeString = STRING(FORMAT='(I0,"-",I0)',lastSavOrb,curOrb)
     orbSavFileName = STRING(FORMAT='(A0,A0,".sav")',saveFilePref,orbRangeString)

     ;; info     = NEWELL__eSpec.info
     PRINT,STRING(FORMAT='("Saving ",I0," junk inds and ",I0," befStart inds for orbs ",A0," to file : ",A0)', $
                  N_ELEMENTS(junk_i),N_ELEMENTS(befStart_i),orbRangeString,orbSavFileName)

     SAVE,junk_i,befStart_i,junkTimes,befStartTimes, $
          missingIntervals, $
          info, $
          FILENAME=saveDir+orbSavFileName

     junk_i         = !NULL
     befStart_i     = !NULL
     junkTimes      = !NULL
     befStartTimes  = !NULL
     missingIntervals = !NULL 
  ENDIF

  PRINT,"Finished!"

END
