;;06/11/16
PRO PLOT_NEWELL_ORB_ESPEC,orbit,interval,espec_num, $
                          ZERO_SPURIOUS_ENERGIES=zero_spurious_energies, $
                          OUT_PLOTARR=out_plotArr, $
                          WINDOW=window
                          

  COMPILE_OPT IDL2,STRICTARRSUBS

  LOAD_NEWELL_ORB_ESPEC_FILE,orbit,requested_interval, $
                             INTERVAL=interval, $
                             ESPECS_PARSED=eSpecs_parsed, $
                             ESPEC_LC=eSpec_lc, $
                             JEE_LC=jee_lc, $
                             JE_LC=je_lc, $
                             ESPEC_SC_MIN_ENERGY_IND=eSpec_sc_min_energy_ind, $ 
                             ESPEC_SC_POT=eSpec_sc_pot, $
                             ESPEC_SC_TIME=eSpec_sc_time, $
                             ISPEC_UP=iSpec_up, $
                             JEI_UP=jei_up, $
                             JI_UP=ji_up, $
                             ION_SC_MIN_ENERGY_IND=ion_sc_min_energy_ind, $
                             ION_SC_POT=ion_sc_pot, $
                             ION_SC_TIME=ion_sc_time
  
  
  nSpecs         = N_ELEMENTS(eSpec_lc.v[*,0])
  IF KEYWORD_SET(eSpec_num) THEN BEGIN
     eSpec_num   = eSpec_num < (nSpecs-1)
     eSpec_num   = eSpec_num[UNIQ(eSpec_num,SORT(eSpec_num))]
  ENDIF ELSE BEGIN
     eSpec_num   = 0
  ENDELSE


  IF N_ELEMENTS(eSpec_num) GT 1 THEN BEGIN
     eSpecString  = "eSpecs (" + STRING(FORMAT='(10(I0,:,", "))',eSpec_num+1) + STRING(FORMAT='(")/",I0)',nSpecs)
     ;; FOR i=0,N_ELEMENTS(eSpec_num)-1 DO BEGIN
     ;;    eSpecString += STRING(FORMAT='(I0)eSpec_num[i]
     ;; ENDFOR
  ENDIF ELSE BEGIN
     eSpecString = STRCOMPRESS(eSpec_num+1,/REMOVE_ALL)
  ENDELSE

  nPlots         = N_ELEMENTS(eSpec_num)
  plotArr        = MAKE_ARRAY(nPlots,/OBJ)

  IF ~ISA(window) THEN BEGIN
     window      = WINDOW(DIMENSION=[800,600], $
                          TITLE="E Spectra, Orbit " + $
                          STRCOMPRESS(orbit,/REMOVE_ALL) + $
                          ", Interval " + STRCOMPRESS(interval,/REMOVE_ALL) + $
                          ", " + eSpecString)
  ENDIF

  FOR i=0,nPlots-1 DO BEGIN

     tmpNum      = eSpec_num[i]
     X           = REVERSE(REFORM(eSpec_lc.v[tmpNum,*]))
     Y           = REVERSE(REFORM(eSpec_lc.y[tmpNum,*]))
     
     IF KEYWORD_SET(zero_spurious_energies) THEN BEGIN
        checkme  = WHERE(X LE 40 AND Y GT 1.0e8)
        IF checkme[0] NE -1 THEN BEGIN
           Y[checkme] = 0
        ENDIF
     ENDIF

     plotArr[i]  = PLOT(X, $
                       Y, $
                       TITLE=title, $
                       NAME=STRMID(TIME_TO_STR(eSpec_lc.x[tmpNum],/MSEC),11,12), $
                       XTITLE=xTitle, $
                       YTITLE=yTitle, $
                       XRANGE=xRange, $
                       YRANGE=yRange, $
                       YLOG=1, $
                       XLOG=1, $
                       THICK=2.2, $
                       SYMBOL='*', $
                       LINESTYLE=(GENERATE_LIST_OF_RANDOM_LINESTYLES(1))[0], $
                       COLOR=(GENERATE_LIST_OF_RANDOM_COLORS(1))[0], $
                       OVERPLOT=i GT 0, $
                       CURRENT=window) 

  ENDFOR

  legend         = LEGEND(TARGET=plotArr[*],POSITION=[0.85,0.85],/NORMAL)

  out_plotArr    = plotArr

END
