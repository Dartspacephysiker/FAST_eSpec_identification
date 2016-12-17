;;12/15/16
PRO PLOT_KH_PRECIPITATION_STRUCT,struct, $
                                 XVARS=xVars, $
                                 YVARS=yVars, $
                                 MLTRANGE=mltRange, $
                                 MINMLT=minM, $
                                 MAXMLT=maxM, $
                                 DAWNSECTOR=dawnSector, $
                                 DUSKSECTOR=duskSector, $
                                 DAYSIDE=dayside, $
                                 NIGHTSIDE=nightside, $
                                 AURORAL_OVAL=auroral_oval, $
                                 ILATRANGE=ilatRange, $
                                 MINILAT=minI, $
                                 MAXILAT=maxI, $
                                 ;; TRANGE=tRange, $
                                 OVERPLOT_SMOOTHED=overplot_smoothed, $
                                 DERIVATIVES=derivatives, $
                                 LOG_Y=log_y, $
                                 LOG_THEN_DERIV=log_then_deriv, $
                                 DERIV_THEN_LOG=deriv_then_log, $
                                 ONLY_POS=only_pos, $
                                 ONLY_NEG=only_neg, $
                                 ABS_X=abs_x, $
                                 ABS_Y=abs_y, $
                                 DERIV__ABS_Y=abs_y_deriv, $
                                 FFT=fft, $
                                 UNEVEN_FFT=FFT__uneven, $
                                 SIGNIFICANCE_FFT=FFT_overplot_significance, $
                                 FILENAME=fileName, $
                                 PLOTNAMEPREF=plotNamePref, $
                                 LOADDIR=loadDir, $
                                 PLOTDIR=plotDir, $
                                 PUBLICATION_SETTINGS=pubSettings, $
                                 POSTSCRIPT=postScript, $
                                 PDF=pdf, $
                                 REMOVE_EPS=remove_eps, $
                                 DO_NOT_SAVE=noSave

  COMPILE_OPT IDL2

  ;;Things that work
  __VALID_XVARS = STRUPCASE(['time','x','mlt','ilat','lShell', $
                             'pos','angle','charE','je','jee'])
  __VALID_YVARS = __VALID_XVARS
  ;; __VALID_YVARS = STRUPCASE(['charE','je','jee'])

  plotPanelXSize = 600
  plotPanelYSize = 400

  IF KEYWORD_SET(pdf) THEN BEGIN
     postScript = 1
  ENDIF

  IF KEYWORD_SET(postScript) THEN BEGIN

     IF KEYWORD_SET(plotDir) THEN BEGIN
        pDir   = plotDir
     ENDIF ELSE BEGIN
        SET_PLOT_DIR,pDir, $
                     /FOR_ESPEC_DB, $
                     ADD_SUFF='/KH_plots/'+ $
                     GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
     ENDELSE

     !P.CHARTHICK = 3
     !P.THICK     = 3

     plotPanelXSize = 10
     plotPanelYSize = 10

  ENDIF

  ;;Some declarations
  xVarList   = LIST()
  xTitleList = LIST()
  xNameList  = LIST()
  xUnitList  = LIST()

  yVarList   = LIST()
  yTitleList = LIST()
  yNameList  = LIST()
  yUnitList  = LIST()

  title      = ''

  IF KEYWORD_SET(pubSettings) THEN BEGIN
     cs = 1.8
  ENDIF ELSE BEGIN
     cs = 2.0
  ENDELSE


  IF KEYWORD_SET(loadDir) THEN BEGIN
     lDir   = loadDir
  ENDIF ELSE BEGIN
     lDir   = '/SPENCEdata/Research/Satellites/FAST/espec_identification/saves_output_etc/KH_stuff/'
  ENDELSE

  IF (N_ELEMENTS(struct) EQ 0) THEN BEGIN
     
     IF (N_ELEMENTS(fileName) EQ 0) THEN BEGIN
        PRINT,"No data provided! Let's choose a file."

        OK = 0

        WHILE ~OK DO BEGIN
           fileName = DIALOG_PICKFILE(PATH=lDir,DEFAULT_EXTENSION='sav')

           IF FILE_TEST(fileName) THEN BEGIN
              PRINT,'Restoring ' + fileName + ' ...'
              RESTORE,fileName
              IF N_ELEMENTS(struct) EQ 0 THEN BEGIN
                 PRINT,"Where's the struct? Returning ..."
                 RETURN
              ENDIF
           ENDIF ELSE BEGIN
              PRINT,"Couldn't restore squattum! Out."
              RETURN
           ENDELSE

           info = !NULL
           STR_ELEMENT,struct,'info',info

           IF SIZE(info,/TYPE) EQ 0 THEN BEGIN
              PRINT,"struct has no INFO member"
           ENDIF ELSE BEGIN
              print_tags = ['energy_electrons','e_angle','calibrated','mlts','ilats']

              PRINT_STRUCT,info,print_tags, $
                           MAX_ELEMENTS=100

              info = !NULL
              STR_ELEMENT,struct.info,'mlts',info
              IF SIZE(info,/TYPE) NE 0 THEN BEGIN
                 PRINT,"MLT info"
                 PRINT,"======="
                 PRINT_STRUCT,info
              ENDIF

              info = !NULL
              STR_ELEMENT,struct.info,'ilats',info
              IF SIZE(info,/TYPE) NE 0 THEN BEGIN
                 PRINT,"ILAT info"
                 PRINT,"========"
                 PRINT_STRUCT,info
              ENDIF

           ENDELSE

           OK2 = 0
           WHILE ~OK2 DO BEGIN
              PRINT,"Look OK?"
              reponse = ''
              READ,reponse

              CASE 1 OF
                 STRMATCH(STRUPCASE(reponse),'Y*'): BEGIN
                    OK  = 1
                    OK2 = 1
                 END
                 STRMATCH(STRUPCASE(reponse),'N*'): BEGIN
                    OK  = 0
                    OK2 = 1
                 END
                 ELSE: BEGIN
                    PRINT,'Try again ... Yes or No'
                 END
              ENDCASE
           ENDWHILE

        ENDWHILE

     ENDIF ELSE BEGIN

        IF FILE_TEST(fileName) THEN BEGIN
           PRINT,'Restoring ' + fileName + ' ...'
           RESTORE,fileName
           IF N_ELEMENTS(struct) EQ 0 THEN BEGIN
              PRINT,"Where's the struct? Returning ..."
              RETURN
           ENDIF
        ENDIF ELSE BEGIN
           PRINT,"Couldn't restore squattum! Out."
           RETURN
        ENDELSE

     ENDELSE

  ENDIF


  ;;Get plot inds, if requested
  haveMLTInfo      = KEYWORD_SET(dawnSector) + $
                     KEYWORD_SET(duskSector) + $
                     KEYWORD_SET(dayside) + $
                     KEYWORD_SET(nightside) + $
                     (KEYWORD_SET(minM) AND KEYWORD_SET(maxM)) + $
                     KEYWORD_SET(MLTRange)

  haveILATInfo     = (KEYWORD_SET(minI) AND KEYWORD_SET(maxI)) + KEYWORD_SET(hemi)

  CASE haveMLTInfo OF
     0: BEGIN
        minM = 2
        maxM = 10
     END
     1: 
     ELSE: BEGIN
        PRINT,"dawnSector : ",KEYWORD_SET(dawnSector ) ? dawnSector  : 0B
        PRINT,"duskSector : ",KEYWORD_SET(duskSector ) ? duskSector  : 0B
        PRINT,"dayside    : ",KEYWORD_SET(dayside    ) ? dayside     : 0B
        PRINT,"nightside  : ",KEYWORD_SET(nightside  ) ? nightside   : 0B
        PRINT,"minM       : ",KEYWORD_SET(minM       ) ? minM        : 0B
        PRINT,"maxM       : ",KEYWORD_SET(maxM       ) ? maxM        : 0B
        PRINT,"MLTRange   : ",KEYWORD_SET(MLTRange   ) ? MLTRange    : 0B
        PRINT,""
        PRINT,"Please don't set all of these at once. K? Chill out."
        RETURN
     END
  ENDCASE

  CASE haveILATInfo OF
     0: BEGIN
        PRINT,'Grabbing all ILATs ...'
     END
     1:
     ELSE: BEGIN
        PRINT,"ILAT opts"
        PRINT,"hemi         : ",KEYWORD_SET(hemi        ) ? hemi           : ''
        PRINT,"auroral_oval : ",KEYWORD_SET(auroral_oval) ? auroral_oval   : 0B
        PRINT,"minI         : ",KEYWORD_SET(minI        ) ? minI           : 0B
        PRINT,"maxI         : ",KEYWORD_SET(maxI        ) ? maxI           : 0B
        PRINT,"Please don't set all of these at once. K? Chill out."
        RETURN
     END
  ENDCASE

  IF KEYWORD_SET(MLTRange) THEN BEGIN
     IF N_ELEMENTS(MLTRange) NE 2 THEN BEGIN
        PRINT,'Bogus'
        STOP
     ENDIF
     minM = MIN(MLTRange)
     maxM = MAX(MLTRange)
  ENDIF

  nPts    = N_ELEMENTS(struct.mlt)
  IF haveMLTInfo THEN BEGIN
     mlt_i            = GET_MLT_INDS(!NULL,minM,maxM, $
                                     DAWNSECTOR=dawnSector, $
                                     DUSKSECTOR=duskSector, $
                                     DAYSIDE=dayside, $
                                     NIGHTSIDE=nightside, $
                                     ;; /DAWNSECTOR, $
                                     DIRECT_MLTS=struct.mlt)
  ENDIF ELSE BEGIN
     mlt_i            = INDGEN(nPts)
  ENDELSE

  IF KEYWORD_SET(ILATRange) THEN BEGIN
     IF N_ELEMENTS(ILATRange) NE 2 THEN BEGIN
        PRINT,'Bogus'
        STOP
     ENDIF
     minI = MIN(ILATRange)
     maxI = MAX(ILATRange)
  ENDIF

  IF haveILATInfo THEN BEGIN
     ilat_i           = GET_ILAT_INDS(!NULL,minI,maxI,hemi, $
                                      AURORAL_OVAL=auroral_oval, $
                                      DIRECT_LATITUDES=struct.ilat)

     mlt_i            = CGSETINTERSECTION(mlt_i,ilat_i, $
                                          COUNT=nInds, $
                                          NORESULT=-1)
     IF mlt_i[0] EQ -1 THEN BEGIN
        PRINT,"No indices to grab!"
        RETURN
     ENDIF
  ENDIF

  ;;Some declarations
  title      = 'Orbit ' + struct.info.orbit


  nxVars = N_ELEMENTS(xVars)

  ;;Now gather plot stuff
  CASE nxVars OF
     0: BEGIN
        PRINT,"Default Var: time"
        xVars  = 'time'
        nxVars = 1
     END
     1: 
     ELSE: BEGIN

        this = UNIQ(xVars,SORT(xVars))
        nUxVars = N_ELEMENTS(this)
        IF nUxVars LT nxVars THEN BEGIN
           PRINT,"Duplicados? ",xVars[CGSETDIFFERENCE(INDGEN(nxVars),this)]
        ENDIF
        
        xVars   = xVars[this]
        nxVars  = nUxVars
     END
  ENDCASE


  PLOT_KH__COMPILE_LISTS,xVars,struct, $
                         NVARS=nXVars, $
                         VALID_VARS=__VALID_XVARS, $
                         VARLIST=xVarList, $
                         TITLELIST=xTitleList, $
                         NAMELIST=xNameList, $
                         UNITLIST=xUnitList

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;YVars

  nYVars = N_ELEMENTS(yVars)

  CASE nYVars OF
     0: BEGIN
        PRINT,"Default yVar: Je"
        yVars  = 'Je'
        nYVars = 1
     END
     1: 
     ELSE: BEGIN

        this = UNIQ(yVars,SORT(yVars))
        nUYVars = N_ELEMENTS(this)
        IF nUYVars LT nYVars THEN BEGIN
           PRINT,"Duplicados? ",yVars[CGSETDIFFERENCE(INDGEN(nYVars),this)]
        ENDIF
        
        yVars   = yVars[this]
        nYVars  = nUYVars
     END
  ENDCASE

  PLOT_KH__COMPILE_LISTS,yVars,struct, $
                         NVARS=nYVars, $
                         VALID_VARS=__VALID_YVARS, $
                         VARLIST=yVarList, $
                         TITLELIST=yTitleList, $
                         NAMELIST=yNameList, $
                         UNITLIST=yUnitList

  ;; FOR k=0,nYVars-1 DO BEGIN
  ;;    tmpI  = WHERE(__VALID_YVARS     EQ STRUPCASE(yVars[k]))
  ;;    tmpI2 = WHERE(TAG_NAMES(struct) EQ STRUPCASE(yVars[k]))

  ;;    IF (tmpI[0] EQ -1) THEN BEGIN
  ;;       PRINT,"yVar not found!"
  ;;       STOP
  ;;    ENDIF
  ;;    IF N_ELEMENTS(tmpI) GT 1 THEN BEGIN
  ;;       PRINT,"yVars: more than one match!"
  ;;       STOP
  ;;    ENDIF

  ;;    IF (tmpI2[0] EQ -1) THEN BEGIN
  ;;       PRINT,"yVar not found!"
  ;;       STOP
  ;;    ENDIF
  ;;    IF N_ELEMENTS(tmpI2) GT 1 THEN BEGIN
  ;;       PRINT,"yVars: more than one match!"
  ;;       STOP
  ;;    ENDIF

  ;;    IF yVars[k] EQ 'X' THEN yVars[k] = 'TIME'


  ;;    CASE STRUPCASE(yVars[k]) OF
  ;;       'TIME': BEGIN
  ;;          yTitleList.Add,"Time since " + TIME_TO_STR(struct.y[0]) + ' (s)'
  ;;          ;; yVarList.Add,struct.(tmpI2)
  ;;          value = 0
  ;;          STR_ELEMENT,struct,'y',value
  ;;          IF value[0] NE 0 THEN BEGIN
  ;;             yVarList.Add,struct.y
  ;;          ENDIF ELSE BEGIN
  ;;             STR_ELEMENT,struct,'time',value
  ;;             IF value[0] NE 0 THEN BEGIN
  ;;                yVarList.Add,struct.time
  ;;             ENDIF ELSE BEGIN
  ;;                PRINT,"Couldn't get time var!!"
  ;;                STOP
  ;;             ENDELSE
  ;;          ENDELSE

  ;;          yNameList.Add,'time'
  ;;          yUnitList.Add,'s'
  ;;       END
  ;;       'MLT': BEGIN
  ;;          yTitleList.Add,"MLT"
  ;;          yVarList.Add,struct.mlt
  ;;          yNameList.Add,'MLT'
  ;;          yUnitList.Add,'MLT'
  ;;       END
  ;;       'ILAT': BEGIN
  ;;          yTitleList.Add,"ILAT (deg)"
  ;;          yVarList.Add,struct.ilat
  ;;          yNameList.Add,'ILAT'
  ;;          yUnitList.Add,'deg'
  ;;       END
  ;;       'POS': BEGIN
  ;;          yTitleList.Add,"Along-track Distance (km)"
  ;;          yVarList.Add,struct.pos
  ;;          yNameList.Add,'Pos'
  ;;          yUnitList.Add,'km'
  ;;       END
  ;;       'ANGLE': BEGIN
  ;;          yTitleList.Add,"Along-track Angular Distance (deg)"
  ;;          yVarList.Add,struct.angle
  ;;          yNameList.Add,'angle'
  ;;          yUnitList.Add,'deg'
  ;;       END
  ;;       'JEE': BEGIN
  ;;          yTitleList.Add,'Energy flux (mW/m^2)'
  ;;          yVarList.Add,struct.eFlux
  ;;          yNameList.Add,'eFlux'
  ;;          yUnitList.Add,'mW/m!U2!N'
  ;;       END
  ;;       'JE': BEGIN
  ;;          yTitleList.Add,'Number flux ($\mu$A/m$^2$)'
  ;;          yVarList.Add,struct.eNumFlux
  ;;          yNameList.Add,'eNumFlux'
  ;;          yUnitList.Add,'#/cm!U2!N-s'
  ;;       END
  ;;       'CHARE': BEGIN
  ;;          yTitleList.Add,'Average Energy (eV)'
  ;;          yVarList.Add,struct.eNumFlux
  ;;          yNameList.Add,'charE'
  ;;          yUnitList.Add,'eV'
  ;;       END
  ;;    ENDCASE
  ;;    ;;    END
  ;;    ;; ENDCASE

  ;; ENDFOR


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now plots
  
  ;;Depending on number of y vars, let's pick a layout
  ;; CASE 1 OF
     ;; KEYWORD_SET(separate_windows): BEGIN
  CASE 1 OF
     (nYVars LE 3): BEGIN
        nCol  = 1
        nRow  = nYVars
     END
     (nYVars LE 6): BEGIN
        nCol  = 2
        nRow  = 3
     END
     (nYVars LE 9): BEGIN
        nCol  = 3
        nRow  = 3
     END
     ELSE: BEGIN
        PRINT,"Jo for mange bilder. Stoppe."
        STOP
     END
  ENDCASE
     ;; ELSE: BEGIN

     ;; END
  ;; ENDCASE
  
  ;;Need time series if FFTing
  IF KEYWORD_SET(fft) THEN BEGIN
     value = 0
     STR_ELEMENT,struct,'x',value
     IF value[0] NE 0 THEN BEGIN
        tSerie = struct.x
     ENDIF ELSE BEGIN
        STR_ELEMENT,struct,'time',value
        IF value[0] NE 0 THEN BEGIN
           tSerie = struct.time
        ENDIF ELSE BEGIN
           PRINT,"Couldn't get time var!!"
           STOP
        ENDELSE
     ENDELSE

     ;;Fourier-transform variables
     ftV = {t      : TEMPORARY(tSerie), $
            uneven : BYTE(KEYWORD_SET(FFT__uneven)) $
           }
            
  ENDIF

  FOR k=0,nXVars-1 DO BEGIN
     IF ~KEYWORD_SET(postscript) THEN BEGIN
        WINDOW,k, $
               XSIZE=nCol*plotPanelXSize, $
               YSIZE=nRow*plotPanelYSize
     ENDIF

     !P.MULTI = [0,nCol,nRow,0,0]

     tmpX        = xVarList[k]
     tmpXNavn    = xNameList[k]
     tmpXTitle   = xTitleList[k]
     tmpXUnit    = xUnitList[k]

     tmpX_tConv  = 0

     IF KEYWORD_SET(abs_x) THEN BEGIN
        tmpX = ABS(tmpX)
        tmpXNavn = 'abs' + tmpXNavn
        tmpXUnit = 'abs' + tmpXUnit
     ENDIF

     IF KEYWORD_SET(fft) THEN BEGIN

        tmpTList      = LIST()
        tmpFFT_i_List = LIST()
        FFTs          = LIST()
        CASE 1 OF
           KEYWORD_SET(ftV.uneven): BEGIN

              nStreaks      = 1

              tmpTList.Add,ftV.t
              tmpFFT_i_list.Add,mlt_i

           END
           ELSE: BEGIN

              allow_delta_t = 5D-3

              GET_DOUBLE_BUFS__NTH_DECIMAL_PLACE,ftV.t,-3, $
                                                 N=128, $
                                                 DELTA=allow_delta_t, $
                                                 START_I=start_i, $
                                                 STOP_I=stop_i, $
                                                 OUT_RATES=rates, $
                                                 STREAKLENS=streakLens, $
                                                 FLOOR=floor, $
                                                 CEILING=ceiling

              nStreaks      = N_ELEMENTS(streakLens)

              FOR j=0,nStreaks-1 DO BEGIN
                 tmpFFT_i = [start_i[j]:stop_i[j]]
                 tmp_TS   = MAKE_EVENLY_SPACED_TIME_SERIES(START_T=ftV.t[start_i[j]], $
                                                           STOP_T=ftV.t[stop_i[j]], $
                                                           DELTA_T=1.D/rates[j])

                 
                 tmpTList.Add,tmp_TS
                 tmpFFT_i_list.Add,tmpFFT_i
              ENDFOR

           END
        ENDCASE

     ENDIF

     FOR l=0,nYVars-1 DO BEGIN

        tmpTmpX      = tmpX
        tmpTmpXNavn  = tmpXNavn
        tmpTmpXTitle = tmpXTitle
        tmpTmpXUnit  = tmpXUnit

        tmpY      = yVarList[l]
        tmpYNavn  = yNameList[l]
        tmpYTitle = yTitleList[l]
        tmpYUnit  = yUnitList[l]

        ;;Skip sammen
        IF tmpTmpXNavn EQ tmpYNavn THEN CONTINUE

        nX        = N_ELEMENTS(tmpTmpX)
        nY        = N_ELEMENTS(tmpY)
        IF nX NE nY THEN STOP

        CASE 1 OF
           KEYWORD_SET(only_pos): BEGIN
              tmp_i = WHERE(tmpY GT 0)
              tmpYNavn = 'pos' + tmpYNavn
              tmpYTitle += ' (pos)'
              ;; tmpYUnit  
           END
           KEYWORD_SET(only_neg): BEGIN
              tmp_i = WHERE(tmpY GT 0)
              tmpYNavn = 'neg' + tmpYNavn
              tmpYTitle += ' (pos)'
           END
           ELSE: BEGIN
              tmp_i = LINDGEN(N_ELEMENTS(tmpY))
           END
        ENDCASE

        tmp_i = CGSETINTERSECTION(tmp_i,mlt_i,NORESULT=-1)

        IF tmp_i[0] EQ -1 THEN BEGIN
           PRINT,"No inds!!"
        ENDIF

        IF KEYWORD_SET(abs_y) THEN BEGIN
           tmpY = ABS(tmpY)
           tmpYNavn = 'abs'+tmpYNavn
           tmpYTitle += ' (abs)'
           tmpYUnit   = 'abs'+tmpYUnit
        ENDIF

        IF KEYWORD_SET(derivatives) AND KEYWORD_SET(log_y) THEN BEGIN
           IF ~(KEYWORD_SET(deriv_then_log) OR $
              KEYWORD_SET(log_then_deriv))     $
           THEN BEGIN
              PRINT,"Defaulting to deriv then log ..."
              deriv_then_log = 1
           ENDIF

           CASE 1 OF
              KEYWORD_SET(deriv_then_log): BEGIN


                 ;;deriv
                 PLOT_KH__DERIV,tmpTmpX,tmpY, $
                                TMPXNAVN=tmpTmpXNavn, $
                                TMPYNAVN=tmpYNavn, $
                                TMPYUNIT=tmpYUnit, $
                                TMPXUNIT=tmpTmpXUnit, $
                                ABS_DERIV=abs_y_deriv

                 ;; tmpY = DERIV(tmpTmpX,tmpY)

                 ;; tmpTmpXNavn = 'd'+tmpTmpXNavn
                 ;; tmpYNavn = 'd'+tmpYNavn
                 ;; tmpYUnit = 'd'+tmpYUnit+'/'+'d'+tmpTmpXUnit

                 ;;then log
                 IF N_ELEMENTS(WHERE(tmpY LT 0,/NULL)) GT 0 THEN BEGIN
                    PRINT,"Neg vals in " + tmpYNavn + '!'
                    PRINT,'Converting to ABS ...'
                    tmpY = ABS(tmpY)
                    tmpYNavn = 'abs' + tmpYNavn
                 ENDIF
                    
                 tmpY     = ALOG10(tmpY)
                 tmpYNavn = 'log' + tmpYNavn

              END
              KEYWORD_SET(log_then_deriv): BEGIN

                 ;;log
                 IF N_ELEMENTS(WHERE(tmpY LT 0,/NULL)) GT 0 THEN BEGIN
                    PRINT,"Neg vals in " + tmpYNavn + '!'
                    PRINT,'Converting to ABS ...'
                    tmpY = ABS(tmpY)
                    tmpYNavn = 'abs' + tmpYNavn
                 ENDIF
                    
                 tmpY     = ALOG10(tmpY)
                 tmpYNavn = 'log' + tmpYNavn
                 tmpYUnit = 'log' + tmpYUnit

                 ;;then deriv
                 PLOT_KH__DERIV,tmpTmpX,tmpY, $
                                TMPXNAVN=tmpTmpXNavn, $
                                TMPYNAVN=tmpYNavn, $
                                TMPYUNIT=tmpYUnit, $
                                TMPXUNIT=tmpTmpXUnit, $
                                ABS_DERIV=abs_y_deriv

                 ;; tmpY = DERIV(tmpTmpX,tmpY)

                 ;; tmpTmpXNavn = 'd'+tmpTmpXNavn
                 ;; tmpYNavn = 'd'+tmpYNavn
                 ;; tmpYUnit = 'd'+tmpYUnit+'/'+'d'+tmpTmpXUnit

              END
           ENDCASE
        ENDIF ELSE BEGIN

           IF KEYWORD_SET(derivatives) THEN BEGIN

              PLOT_KH__DERIV,tmpTmpX,tmpY, $
                             TMPXNAVN=tmpTmpXNavn, $
                             TMPYNAVN=tmpYNavn, $
                             TMPYUNIT=tmpYUnit, $
                             TMPXUNIT=tmpTmpXUnit, $
                             ABS_DERIV=abs_y_deriv

              ;;deriv
              ;; tmpY = DERIV(tmpTmpX,tmpY)

              ;; tmpTmpXNavn = 'd'+tmpTmpXNavn
              ;; tmpYNavn = 'd'+tmpYNavn
              ;; tmpYUnit = 'd'+tmpYUnit+'/'+'d'+tmpTmpXUnit
           ENDIF

           IF KEYWORD_SET(log_y) THEN BEGIN
              ;;log
              IF N_ELEMENTS(WHERE(tmpY LT 0,/NULL)) GT 0 THEN BEGIN
                 PRINT,"Neg vals in " + tmpYNavn + '!'
                 PRINT,'Converting to ABS ...'
                 tmpY = ABS(tmpY)
                 tmpYNavn = 'abs' + tmpYNavn
              ENDIF
              
              tmpY     = ALOG10(tmpY)
              tmpYNavn = 'log' + tmpYNavn

           ENDIF

        ENDELSE 

        CASE 1 OF
           KEYWORD_SET(fft): BEGIN

              FFTs = LIST()
              CASE 1 OF
                 KEYWORD_SET(ftV.uneven): BEGIN

                    IF N_ELEMENTS(tmpY) NE N_ELEMENTS(ftV.t) THEN STOP

                    tmpYNavn = 'unevenFFT_' + tmpYNavn

                    f        = FFT_POWERSPECTRUM(DOUBLE(tmpY), $
                                                 (ftV.t-ftV.t[0])/(ftV.t[-1]-ftV.t[0]), $
                                                 FREQ=freq, $
                                                 ;; /TUKEY, $
                                                 ;; WIDTH=MEAN(ftV.t[1:-1]-ftV.t[0:-2]), $
                                                 ;; WIDTH=MEAN(ftV.t[1:-1]-ftV.t[0:-2]), $
                                                 SIGNIFICANCE=signif)


                    tmp_i = INDGEN(N_ELEMENTS(freq))
                    tmpTmpX  = TEMPORARY(freq)
                    tmpY  = TEMPORARY(f)
                    
                 END
                 ELSE: BEGIN

                    FFTPowList = LIST()
                    freqList   = LIST()
                    signifList = LIST()
                    nFreqs     = !NULL
                    freqRes    = !NULL
                    maxFreqs   = !NULL
                    minFreqs   = !NULL
                    FOR j=0,nStreaks-1 DO BEGIN

                       tmpT    = tmpTList[j]
                       tmpInds = tmpFFT_i_list[j]

                       nTmpT   = N_ELEMENTS(tmpT)

                       tmpFFTY = tmpY[tmpInds]-MEAN(tmpY[tmpInds])
                       tmpFFTX = tmpTmpX[tmpInds]

                       tmpFFTY = DATA_CUT({x:ftV.t[tmpInds],y:tmpFFTY}, $
                                          tmpT, $
                                          /INTERP_GAP)
                       tmpFFTX = DATA_CUT({x:ftV.t[tmpInds],y:tmpFFTX}, $
                                          tmpT, $
                                          /INTERP_GAP)

                       sPeriod = 1.D/rates[j]

                       ;;Frequencies
                       ;; T is an integer giving the number of elements in a particular dimension
                       ;; sPeriod is a floating-point number giving the sampling interval
                       X = FINDGEN((nTmpT - 1)/2) + 1
                       is_T_even = (nTmpT MOD 2) EQ 0
                       IF (is_T_even) THEN BEGIN
                          freq = [0.0, X, nTmpT/2, -nTmpT/2 + X]/(nTmpT*sPeriod)
                       ENDIF ELSE BEGIN
                          freq = [0.0, X, -(nTmpT/2 + 1) + X]/(nTmpT*sPeriod)
                       ENDELSE

                       
                       dVal     = MEAN(tmpFFTX[1:-1]-tmpFFTX[0:-2])

                       print,'dVal: ',dVal
                       print,tmpFFTX
                       f        = FFT_POWERSPECTRUM(tmpFFTY, $
                                                    ;; rates[j], $
                                                    dVal, $
                                                    ;; sPeriod, $
                                                    FREQ=freq, $
                                                    /TUKEY, $
                                                    ;; WIDTH=MEAN(ftV.t[1:-1]-ftV.t[0:-2]), $
                                                    ;; WIDTH=MEAN(ftV.t[1:-1]-ftV.t[0:-2]), $
                                                    SIGNIFICANCE=signif)
                       
                       ;; tmpFFT  = FFT(tmpFFTY)
                       
                       FFTPowList.Add,f
                       freqList.Add,freq
                       signifList.Add,signif
                       nFreqs     = [nFreqs,N_ELEMENTS(f)]
                       freqRes    = [freqRes,freq[1]-freq[0]]
                       maxFreqs   = [maxFreqs,MAX(freq)]
                       minFreqs   = [minFreqs,MIN(freq)]

                    ENDFOR

                    ;;Use power spectrum with worst resolution to do the deed
                    junk      = MAX(freqres,ind)
                    refFreq   = freqList[ind]

                    itrpPwArr = FLTARR(nFreqs[ind])
                    sigPwArr  = FLTARR(nFreqs[ind])
                    FOR j=0,nStreaks-1 DO BEGIN
                       
                       ;; itrpPwList.Add,INTERPOL(FFTPowList[j], $
                       ;;                         freqList[j], $
                       ;;                         refFreq, $
                       ;;                        /LSQUADRATIC)
                       itrpPwArr += INTERPOL(FFTPowList[j], $
                                             freqList[j], $
                                             refFreq, $
                                             /LSQUADRATIC)

                       sigPwArr  += INTERPOL(signifList[j], $
                                             freqList[j], $
                                             refFreq, $
                                             /LSQUADRATIC)

                    ENDFOR

                    itrpPwArr /= nStreaks

                    tmpTmpXNavn  += 'Freq'
                    tmpTmpXTitle += '(FFTFreq)'

                    tmpTmpX       = TEMPORARY(refFreq)
                    tmpY       = TEMPORARY(itrpPwArr)
                    tmp_i      = INDGEN(nFreqs[ind])
                    
                 END
              ENDCASE

              IF STRMATCH(STRUPCASE(tmpTmpXNavn),'*MLT*') THEN BEGIN
                 xRange = [0,40]
              ENDIF ELSE BEGIN
                 xRange = !NULL
              ENDELSE

           END
           ELSE: BEGIN

              IF STRMATCH(STRUPCASE(tmpTmpXNavn),'*TIME*') THEN BEGIN

                 ;; tmpTmpX = tmpTmpX - tmpTmpX[0]

                 IF ~tmpTmpX_tConv THEN BEGIN
                    tmpTmpX         = UTC_TO_JULDAY(tmpTmpX)
                    tmpTmpX_tConv   = 1
                 ENDIF

                 xTickUnits   = 'TIME'
                 xTickFormat  = 'LABEL_DATE'
                 dummy        = LABEL_DATE(DATE_FORMAT='%H:%I:%S')
              ENDIF ELSE BEGIN
                 xTickUnits   = !NULL
                 xTickFormat  = !NULL
              ENDELSE

              IF STRMATCH(STRUPCASE(tmpYNavn),'*TIME*') THEN BEGIN
                 tmpY = tmpY - tmpY[0]
              ENDIF

           END
        ENDCASE

        IF KEYWORD_SET(postScript) THEN BEGIN

           filNavn = 'orb_' + struct.info.orbit + $
                     '-' + tmpYNavn+'_vs_'+tmpTmpXNavn

           SET_PLOT,'PS'
           DEVICE,FILE=pDir+filNavn+'.eps', $
                  /ENCAPSUL, $
                  XSIZE=10, $
                  YSIZE=10, $
                  /INCHES, $
                  YOFFSET=2, $
                  /COLOR
        ENDIF

        PLOT,tmpTmpX[tmp_i],tmpY[tmp_i], $
             XRANGE=xRange, $
             XTITLE=(l EQ (nRow - 1) ? tmpTmpXTitle : !NULL), $
             YTITLE=tmpYTitle, $
             XTICKUNITS=xTickUnits, $
             XTICKFORMAT=xTickFormat, $
             XMARGIN=[12,3], $
             CHARSIZE=cs        ;, $
        ;; YRANGE=[-ysize,ysize]

        IF KEYWORD_SET(overplot_smoothed) THEN BEGIN
           OPLOT,tmpTmpX[tmp_i],SMOOTH(tmpY[tmp_i],5), $
                 COLOR=250
        ENDIF

        FFT_overplot_significance = 1
        IF KEYWORD_SET(FFT) AND KEYWORD_SET(FFT_overplot_significance) THEN BEGIN
           OPLOT,tmpTmpX[tmp_i],sigPwArr,COLOR=250
        ENDIF
           
        IF KEYWORD_SET(postscript) THEN BEGIN
           DEVICE,/CLOSE
           ;; PCLOSE
           SET_PLOT,'X'

        ENDIF

        IF KEYWORD_SET(pdf) THEN BEGIN
           EPS2PDF,pDir+filNavn, $
                   REMOVE_EPS=N_ELEMENTS(remove_eps) GT 0 ? remove_eps : 1
                   
        ENDIF

     ENDFOR

     IF ~KEYWORD_SET(noSave) THEN BEGIN

     ENDIF

  ENDFOR


END

PRO PLOT_KH__COMPILE_LISTS,vars,struct, $
                           NVARS=nVars, $
                           VALID_VARS=VALID_VARS, $
                           VARLIST=VarList, $
                           TITLELIST=TitleList, $
                           NAMELIST=NameList, $
                           UNITLIST=UnitList

  nVars = N_ELEMENTS(Vars)

  FOR k=0,nVars-1 DO BEGIN

     IF (WHERE(STRUPCASE(TAG_NAMES(struct)) EQ 'X'))[0] NE -1 THEN BEGIN
        IF STRUPCASE(Vars[k]) EQ 'TIME' THEN Vars[k] = 'X'
     ENDIF ELSE BEGIN
        IF STRUPCASE(Vars[k]) EQ 'X' THEN Vars[k] = 'TIME'
     ENDELSE

     tmpI  = WHERE(VALID_VARS        EQ STRUPCASE(Vars[k]))
     tmpI2 = WHERE(TAG_NAMES(struct) EQ STRUPCASE(Vars[k]))

     IF (tmpI[0] EQ -1) THEN BEGIN
        PRINT,"Var not found!"
        STOP
     ENDIF
     IF N_ELEMENTS(tmpI) GT 1 THEN BEGIN
        PRINT,"Vars: more than one match!"
        STOP
     ENDIF

     IF (tmpI2[0] EQ -1) THEN BEGIN
        PRINT,"Var not found!"
        STOP
     ENDIF
     IF N_ELEMENTS(tmpI2) GT 1 THEN BEGIN
        PRINT,"Vars: more than one match!"
        STOP
     ENDIF

     PLOT_KH__UPDATE_VARLIST,Vars[k],struct, $
                             VARLIST=VarList, $
                             TITLELIST=TitleList, $
                             NAMELIST=NameList, $
                             UNITLIST=UnitList

     ;; CASE STRUPCASE(xVars[k]) OF
     ;;    'TIME': BEGIN
     ;;       xTitleList.Add,"Time since " + TIME_TO_STR(struct.x[0]) + ' (s)'
     ;;       ;; xVarList.Add,struct.(tmpI2)
     ;;       value = 0
     ;;       STR_ELEMENT,struct,'x',value
     ;;       IF value[0] NE 0 THEN BEGIN
     ;;          xVarList.Add,struct.x
     ;;       ENDIF ELSE BEGIN
     ;;          STR_ELEMENT,struct,'time',value
     ;;          IF value[0] NE 0 THEN BEGIN
     ;;             xVarList.Add,struct.time
     ;;          ENDIF ELSE BEGIN
     ;;             PRINT,"Couldn't get time var!!"
     ;;             STOP
     ;;          ENDELSE
     ;;       ENDELSE

     ;;       xNameList.Add,'time'
     ;;       xUnitList.Add,'s'
     ;;    END
     ;;    'MLT': BEGIN
     ;;       xTitleList.Add,"MLT"
     ;;       xVarList.Add,struct.mlt
     ;;       xNameList.Add,'MLT'
     ;;       xUnitList.Add,'MLT'
     ;;    END
     ;;    'ILAT': BEGIN
     ;;       xTitleList.Add,"ILAT"
     ;;       xVarList.Add,struct.ilat
     ;;       xNameList.Add,'ILAT'
     ;;       xUnitList.Add,'deg'
     ;;    END
     ;;    'POS': BEGIN
     ;;       xTitleList.Add,"Along-track Distance (km)"
     ;;       xVarList.Add,struct.pos
     ;;       xNameList.Add,'Pos'
     ;;       xUnitList.Add,'km'
     ;;    END
     ;;    'ANGLE': BEGIN
     ;;       xTitleList.Add,"Along-track Angular Distance (deg)"
     ;;       xVarList.Add,struct.angle
     ;;       xNameList.Add,'angle'
     ;;       xUnitList.Add,'deg'
     ;;    END
     ;;    'JEE': BEGIN
     ;;       xTitleList.Add,'Energy flux (mW/m^2)'
     ;;       xVarList.Add,struct.eFlux
     ;;       xNameList.Add,'eFlux'
     ;;       xUnitList.Add,'mW/m!U2!N'
     ;;    END
     ;;    'JE': BEGIN
     ;;       xTitleList.Add,'Number flux ($\mu$A/m$^2$)'
     ;;       xVarList.Add,struct.eNumFlux
     ;;       xNameList.Add,'eNumFlux'
     ;;       xUnitList.Add,'#/cm!U2!N-s'
     ;;    END
     ;;    'CHARE': BEGIN
     ;;       xTitleList.Add,'Average Energy (eV)'
     ;;       xVarList.Add,struct.eNumFlux
     ;;       xNameList.Add,'charE'
     ;;       xUnitList.Add,'eV'
     ;;    END
     ;; ENDCASE

  ENDFOR

END

PRO PLOT_KH__UPDATE_VARLIST,varName,struct, $
                            VARLIST=varList, $
                            TITLELIST=titleList, $
                            NAMELIST=nameList, $
                            UNITLIST=unitList

  muLetter = '!4' + String('154'O) + '!X'

  CASE STRUPCASE(varName) OF
     'TIME': BEGIN
        TitleList.Add,"Time since " + TIME_TO_STR(struct.x[0]) + ' (s)'
        ;; VarList.Add,struct.(tmpI2)
        value = 0
        STR_ELEMENT,struct,'x',value
        IF value[0] NE 0 THEN BEGIN
           VarList.Add,struct.x
        ENDIF ELSE BEGIN
           STR_ELEMENT,struct,'time',value
           IF value[0] NE 0 THEN BEGIN
              VarList.Add,struct.time
           ENDIF ELSE BEGIN
              PRINT,"Couldn't get time var!!"
              STOP
           ENDELSE
        ENDELSE

        NameList.Add,'time'
        UnitList.Add,'s'
     END
     'X': BEGIN
        TitleList.Add,"Time since " + TIME_TO_STR(struct.x[0]) + ' (s)'
        ;; VarList.Add,struct.(tmpI2)
        value = 0
        STR_ELEMENT,struct,'x',value
        IF value[0] NE 0 THEN BEGIN
           VarList.Add,struct.x
        ENDIF ELSE BEGIN
           STR_ELEMENT,struct,'time',value
           IF value[0] NE 0 THEN BEGIN
              VarList.Add,struct.time
           ENDIF ELSE BEGIN
              PRINT,"Couldn't get time var!!"
              STOP
           ENDELSE
        ENDELSE

        NameList.Add,'time'
        UnitList.Add,'s'
     END
     'MLT': BEGIN
        TitleList.Add,"MLT"
        VarList.Add,struct.mlt
        NameList.Add,'MLT'
        UnitList.Add,'MLT'
     END
     'ILAT': BEGIN
        TitleList.Add,"ILAT"
        VarList.Add,struct.ilat
        NameList.Add,'ILAT'
        UnitList.Add,'deg'
     END
     'POS': BEGIN
        TitleList.Add,"Along-track Distance (km)"
        VarList.Add,struct.pos
        NameList.Add,'Pos'
        UnitList.Add,'km'
     END
     'ANGLE': BEGIN
        TitleList.Add,"Along-track Angular Distance (deg)"
        VarList.Add,struct.angle
        NameList.Add,'angle'
        UnitList.Add,'deg'
     END
     'JEE': BEGIN
        TitleList.Add,'Energy flux (mW/m^2)'
        VarList.Add,struct.jee
        NameList.Add,'eFlux'
        UnitList.Add,'mW/m!U2!N'
     END
     'JE': BEGIN
        ;; TitleList.Add,'Number flux ($\mu$A/m$^2$)'
        ;; TitleList.Add,'Number flux (' + muLetter + 'A/m!U2!N)'
        TitleList.Add,'Number flux (!4l!3A/m!U2!N)'

        VarList.Add,struct.je
        NameList.Add,'eNumFlux'
        UnitList.Add,'#/cm!U2!N-s'
     END
     'CHARE': BEGIN
        TitleList.Add,'Average Energy (eV)'
        VarList.Add,struct.charE
        NameList.Add,'charE'
        UnitList.Add,'eV'
     END
  ENDCASE


END

PRO PLOT_KH__DERIV,tmpX,tmpY, $
                   TMPXNAVN=tmpXNavn, $
                   TMPYNAVN=tmpYNavn, $
                   TMPYUNIT=tmpYUnit, $
                   TMPXUNIT=tmpXUnit, $
                   ABS_DERIV=abs_deriv
  
  ;;deriv
  tmpY = DERIV(tmpX,tmpY)


  tmpXNavn = 'd'+tmpXNavn
  tmpYNavn = 'd'+tmpYNavn
  tmpYUnit = 'd'+tmpYUnit+'/'+'d'+tmpXUnit

  IF KEYWORD_SET(abs_deriv) THEN BEGIN
     tmpY  = ABS(tmpY)

     tmpYNavn = 'abs' + tmpYNavn

  ENDIF   

END