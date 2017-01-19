;;01/18/17
PRO JOURNAL__20170118__LOOK_FOR_GAPS_AND_REMOVE

  COMPILE_OPT IDL2

  startOrb    = 500             ;Otherwise it just picks the first orbit in eSpec
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

  ;;vals for plot
  LOADCT2,39
  junkTransVal = 0
  junkStartVal = 1
  TSVal        = 2
  Val032       = 3
  Val0628      = 4
  Val251       = 5
  ValUnsafe    = 6

  junkTransCol = 25
  junkStartCol = 50
  TSCol        = 75
  Col032       = 100
  Col0628      = 125
  Col251       = 150
  ColUnsafe    = 175

  junkTransPSym = 1
  junkStartPSym = 2
  TSPSym        = 4
  PSym032       = 5
  PSym0628      = 6
  PSym251       = 1
  PSymUnsafe    = 2

  IF KEYWORD_SET(showPlots) THEN BEGIN
     IF KEYWORD_SET(savePS) THEN BEGIN
     ENDIF ELSE BEGIN
        WINDOW,0,XSIZE=800,YSIZE=600
     ENDELSE
  ENDIF

  PRINT,FORMAT='(A0,T10,A0,T20,A0,T30,A0,T40,A0,T50,A0)',"Orbits","N Streak","N 2.51","N 0.628","N 0.32","N Unsafe"
  WHILE curOrb LE endOrb DO BEGIN

     ;;Reset inds for this orbit
     befStart_ii       = !NULL
     junk_ii           = !NULL
     
     IF curOrb GT (lastSavOrb + deltaSavOrb - 1) THEN BEGIN
        orbRangeString = STRING(FORMAT='(I0,"-",I0)',lastSavOrb,lastSavOrb+deltaSavOrb-1)
        orbSavFileName = STRING(FORMAT='("esa_transit_times--",A0,".sav")',orbRangeString)

        eSpec_info     = NEWELL__eSpec.info
        PRINT,STRING(FORMAT='("Saving ",I0," junk inds and ",I0," befStart inds for orbs ",A0," to file : ",A0)', $
                     N_ELEMENTS(junk_i),N_ELEMENTS(befStart_i),orbRangeString,orbSavFileName)

        SAVE,junk_i,befStart_i,junkTimes,befStartTimes, $
             eSpec_info, $
             FILENAME=saveDir+orbSavFileName

        junk_i         = !NULL
        befStart_i     = !NULL
        junkTimes      = !NULL
        befStartTimes  = !NULL
        lastSavOrb += deltaSavOrb
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

        intStartTs   = tRanges[0,*]
        CHECK_SORTED,intStartTs,sorted_intervals
        IF ~sorted_intervals THEN STOP
        
        interval_ii  = VALUE_CLOSEST2(NEWELL__eSpec.x[tmp_i],intStartTs)

        goodIntMatch = WHERE(ABS(NEWELL__eSpec.x[tmp_i[interval_ii]]-intStartTs) LT 5,nGoodIntMatch)
        IF nGoodIntMatch NE N_ELEMENTS(intStartTs) THEN BEGIN
           PRINT,"Un lío! The number of times that NEWELL__eSpec matches the ESA interval start times is " + $
                 "fewer than the number of ESA interval start times for this orbit!"
           STOP
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
     delta   = 0.1
     GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE,tmpTime,-1, $
                                        N=n, $
                                        DELTA=delta, $
                                        START_I=start_ii, $
                                        STOP_I=stop_ii, $
                                        STREAKLENS=streakLens, $
                                        OUT_RATES=rates, $
                                        FLOOR=floor, $
                                        CEILING=ceiling

     sampDT    = !NULL
     nStreaks  = N_ELEMENTS(start_ii)
     FOR k=0,nStreaks-1 DO BEGIN
        ;; PRINT,tmpTime[(start_ii[k]+1):stop_ii[k]]-tmpTime[(start_ii[k]):(stop_ii[k]-1)]
        sampDT = [sampDT,MEAN(tmpTime[(start_ii[k]+1):stop_ii[k]]-tmpTime[(start_ii[k]):(stop_ii[k]-1)])]
     ENDFOR

     samp251    = ABS(sampDT-2.51) LT delta
     samp0628   = ABS(sampDT-0.628) LT delta
     samp032    = ABS(sampDT-0.32) LT delta
     w251       = WHERE(samp251,n251)
     w0628      = WHERE(samp0628,n0628)
     w032       = WHERE(samp032,n032)
     safe       = WHERE(TEMPORARY(samp251) OR TEMPORARY(samp0628) OR TEMPORARY(samp032), $
                        nSafeStreaks,COMPLEMENT=wUnsafe,NCOMPLEMENT=nUnsafe)

     PRINT,FORMAT='(A0,T10,I0,T20,I0,T30,I0,T40,I0,T50,I0)',orbString,nStreaks,n251,n0628,n032,nUnsafe

     IF nSafeStreaks NE nStreaks THEN BEGIN
        PRINT,FORMAT='(A0,": ","Unsafe sampDTs!! (",10(F0.2,:,", "),")")',orbSTring,sampDT[wUnsafe]
        ;; STOP

        ;;Junk all the places where we transition from 2.51-s sample period to something else
        wUnsafe_ii = !NULL
        FOR k=0,nUnsafe-1 DO BEGIN
           tmpK       = wUnsafe[k]
           wUnsafe_ii = [wUnsafe_ii,[start_ii[tmpK]:stop_ii[tmpK]]]
           tmpJunk_ii = WHERE((tmpTime GE (tmpTime[stop_ii[tmpK]]-sampDT[tmpK]*3.1)) AND $
                              (tmpTime LE (tmpTime[stop_ii[tmpK]]+sampDT[tmpK]*3.1)), $
                              nTmpJunk)

           IF nTmpJunk GT 0 THEN BEGIN
              ;; junk_i  = [junk_i,tmp_i[tmpJunk_ii]]
              junk_ii = [junk_ii,tmpJunk_ii]
           ENDIF
        ENDFOR
     ENDIF

     ;;Junk all the places where we transition from 2.51-s sample period to something else
     w251_ii = !NULL
     FOR k=0,n251-1 DO BEGIN
        tmpK       = w251[k]
        w251_ii    = [w251_ii,[start_ii[tmpK]:stop_ii[tmpK]]]
        tmpJunk_ii = WHERE((tmpTime GE (tmpTime[stop_ii[tmpK]]-sampDT[tmpK]*3.1)) AND $
                           (tmpTime LE (tmpTime[stop_ii[tmpK]]+sampDT[tmpK]*3.1)), $
                           nTmpJunk)

        IF nTmpJunk GT 0 THEN BEGIN
           ;; junk_i  = [junk_i,tmp_i[tmpJunk_ii]]
           junk_ii = [junk_ii,tmpJunk_ii]
        ENDIF
     ENDFOR

     ;;Junk all the places where we transition from 0.628-s sample period to something else
     w0628_ii = !NULL
     FOR k=0,n0628-1 DO BEGIN
        tmpK       = w0628[k]
        w0628_ii   = [w0628_ii,[start_ii[tmpK]:stop_ii[tmpK]]]
        tmpJunk_ii = WHERE((tmpTime GE (tmpTime[stop_ii[tmpK]]-sampDT[tmpK]*3.1)) AND $
                           (tmpTime LE (tmpTime[stop_ii[tmpK]]+sampDT[tmpK]*3.1)), $
                           nTmpJunk)

        IF nTmpJunk GT 0 THEN BEGIN
           ;; junk_i  = [junk_i,tmp_i[tmpJunk_ii]]
           junk_ii = [junk_ii,tmpJunk_ii]
        ENDIF
     ENDFOR

     ;;Junk all the places where we transition from 0.628-s sample period to something else
     w032_ii = !NULL
     FOR k=0,n032-1 DO BEGIN
        tmpK       = w032[k]
        w032_ii   = [w032_ii,[start_ii[tmpK]:stop_ii[tmpK]]]
        tmpJunk_ii = WHERE((tmpTime GE (tmpTime[stop_ii[tmpK]]-sampDT[tmpK]*3.1)) AND $
                           (tmpTime LE (tmpTime[stop_ii[tmpK]]+sampDT[tmpK]*3.1)), $
                           nTmpJunk)

        IF nTmpJunk GT 0 THEN BEGIN
           ;; junk_i  = [junk_i,tmp_i[tmpJunk_ii]]
           junk_ii = [junk_ii,tmpJunk_ii]
        ENDIF
     ENDFOR


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
             YTICKNAME=["SRate Trans","Interv Start","T Series",'dt=0.32','dt=0.628','dt=2.51','dt=unsafe'], $
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

        IF N_ELEMENTS(w032_ii) GT 0 THEN BEGIN
           OPLOT,tJul[w032_ii],MAKE_ARRAY(N_ELEMENTS(w032_ii),VALUE=Val032), $
                 PSYM=PSym032, $
                 COLOR=Col032   ;, $
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
        junk_i        = [junk_i,tmp_i[junk_ii]]
        junkTimes     = [junkTimes,tmpTime[junk_ii]]
     ENDIF

     curOrb           = (orbs[++orbInd])[0]

  ENDWHILE


END
