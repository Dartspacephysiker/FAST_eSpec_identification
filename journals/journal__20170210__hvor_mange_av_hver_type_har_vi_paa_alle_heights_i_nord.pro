;;2017/02/10
PRO JOURNAL__20170210__HVOR_MANGE_AV_HVER_TYPE_HAR_VI_PAA_ALLE_HEIGHTS_I_NORD

  COMPILE_OPT IDL2

  @common__newell_espec.pro
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,/LOAD_CHARE
  ENDIF

  ;; dir         = '/SPENCEdata/Research/Satellites/FAST/OMNI_FAST/saves_output_etc/20170210/'
  dir         = '/SPENCEdata/Research/Satellites/FAST/OMNI_FAST/saves_output_etc/20170217/'
  ;; good_i_file = 'eSpec_good_i-20170210.sav'
  ;; good_i_file = 'eSpec_good_i__BOTH-20170210.sav'
  good_i_file = 'eSpec_good_i__BOTH-20170217.sav'
  restore,dir+good_i_file

  maxAlt = 4300
  minAlt = 300
  dAlt   = 100
  nAlt   = (maxAlt-minAlt)/dAlt+1

  altArr = INDGEN(nAlt)*dAlt + minAlt
  
  broadArr = !NULL
  monoArr  = !NULL
  diffArr  = !NULL
  nHjArr   = !NULL
  FOR k=0,nAlt-1 DO BEGIN
     print,k
     bood_i   = CGSETINTERSECTION(good_i,WHERE(newell__espec.alt LE altarr[k]),COUNT=nBood)
     nHjArr   = [nHjArr,nBood]
     ;; broadArr = [broadArr,N_ELEMENTS(WHERE(NEWELL__eSpec.broad[bood_i] EQ 1 OR NEWELL__eSpec.broad[bood_i] EQ 2))/FLOAT(nBood)*100.]
     ;; monoArr  = [ monoArr,N_ELEMENTS(WHERE(NEWELL__eSpec.mono[bood_i]  EQ 1 OR NEWELL__eSpec.mono[bood_i]  EQ 2))/FLOAT(nBood)*100.]
     broadArr = [broadArr,N_ELEMENTS(WHERE(NEWELL__eSpec.broad[bood_i] EQ 1 OR NEWELL__eSpec.broad[bood_i] EQ 2))]
     monoArr  = [ monoArr,N_ELEMENTS(WHERE(NEWELL__eSpec.mono[bood_i]  EQ 1 OR NEWELL__eSpec.mono[bood_i]  EQ 2))]
     diffArr  = [ diffArr,nHjArr[-1]-(broadArr[-1]+monoArr[-1])]
     ;; diffArr  = [ diffArr,100.-(broadArr[-1]+monoArr[-1])]
  ENDFOR

  broadPArr   = broadArr/FLOAT(nHjArr)*100.
  monoPArr    = monoArr/FLOAT(nHjArr)*100.
  diffPArr    = diffArr/FLOAT(nHjArr)*100.

  altDArr     = (altArr[1:-1]+altArr[0:-2])/2.
  nHjDArr     = nHjArr[1:-1]-nHjArr[0:-2]
  broadDArr   = broadArr[1:-1]-broadArr[0:-2]
  monoDArr    = monoArr[1:-1]-monoArr[0:-2]
  diffDArr    = diffArr[1:-1]-diffArr[0:-2]

  broadDPArr  = broadDArr/FLOAT(nHjDArr)*100.
  monoDPArr   = monoDArr/FLOAT(nHjDArr)*100.
  diffDPArr   = diffDArr/FLOAT(nHjDArr)*100.

  overAll     = N_ELEMENTS(good_i)
  overBroad   = N_ELEMENTS(WHERE(NEWELL__eSpec.broad[good_i] EQ 1 OR NEWELL__eSpec.broad[good_i] EQ 2))
  overMono    = N_ELEMENTS(WHERE(NEWELL__eSpec.mono[good_i] EQ 1 OR NEWELL__eSpec.mono[good_i] EQ 2))
  overDiff    = overAll-overBroad-overMono
  PRINT,FORMAT='(A0,T10,A0,T20,A0,T30,A0,T40,A0,T50,A0)', $
        "Ind","maxAlt","N inds","% Broad","% Mono","% Diffuse"
  PRINT,FORMAT='(I0,T10,A0,T20,I0,T30,I0,T40,I0,T50,I0)', $
        -1,"ALL",overAll,overBroad,overMono,overDiff

  FOR k=0,nAlt-1 DO BEGIN
     PRINT,FORMAT='(I0,T10,I0,T20,I0,T30,F0.2,T40,F0.2,T50,F0.2)', $
           k,altArr[k],nHjArr[k],broadPArr[k],monoPArr[k],diffPArr[k]
  ENDFOR

  ;; good    = WHERE(FINITE(broadArr) AND FINITE(monoArr) AND FINITE(diffArr))
  good    = WHERE(FINITE(broadPArr) AND FINITE(monoPArr) AND FINITE(diffPArr))

  plotArr = MAKE_ARRAY(3,/OBJ)

  window     = WINDOW(DIMENSIONS=[900,600])
  yPRange    = [0,(MAX(broadPArr[good]) > MAX(monoPArr[good]) > MAX(diffPArr[good]-80))]
  plotArr[0] = PLOT(altArr[good],broadPArr[good], $
                    NAME='Broad', $
                    COLOR='red', $
                    LINESTYLE='', $
                    SYMBOL='*', $
                    ;; YRANGE=[0,(MAX(broadPArr[good]) > MAX(monoPArr[good]) > MAX(100-diffPArr[good]))], $
                    YRANGE=yPRange, $
                    XRANGE=[300,4300], $
                    XTITLE='Altitude (km)', $
                    YTITLE='% Precip type', $
                    CURRENT=window)

  plotArr[1] = PLOT(altArr[good],monoPArr[good], $
                    NAME='Mono', $
                    COLOR='Blue', $
                    LINESTYLE='', $
                    SYMBOL='+', $
                    /OVERPLOT, $
                    CURRENT=window)

  ;; plotArr[2] = PLOT(altArr[good],100.-diffArr[good], $
                    ;; NAME='Diffuse-100', $
  plotArr[2] = PLOT(altArr[good],diffPArr[good]-80, $
                    COLOR='Black', $
                    LINESTYLE='', $
                    SYMBOL='x', $
                    /OVERPLOT, $
                    CURRENT=window)

  legend = LEGEND(TARGET=plotArr, $
                  POSITION=[0.8,0.7], $
                  /NORMAL)


  ;;Now NOT cumulative
  plotArr2 = MAKE_ARRAY(3,/OBJ)

  good        = WHERE(FINITE(broadDPArr) AND FINITE(monoDPArr) AND FINITE(diffDPArr))
  window2     = WINDOW(DIMENSIONS=[900,600],TITLE=TIME_TO_STR(NEWELL__eSpec.x[0])+' - ' + TIME_TO_STR(NEWELL__eSpec.x[-1]))
  yDPRange    = [0,(MAX(broadDPArr[good]) > MAX(monoDPArr[good]) > MAX(diffDPArr[good]-80.))]
  plotArr2[0] = PLOT(altDArr[good],broadDPArr[good], $
                    NAME='Broad', $
                    COLOR='red', $
                    LINESTYLE='', $
                    SYMBOL='*', $
                    YRANGE=yDPRange, $
                    XRANGE=[300,4300], $
                    XTITLE='Altitude (km)', $
                    YTITLE='% Precip type', $
                    CURRENT=window2)

  plotArr2[1] = PLOT(altDArr[good],monoDPArr[good], $
                    NAME='Mono', $
                    COLOR='Blue', $
                    LINESTYLE='', $
                    SYMBOL='+', $
                    /OVERPLOT, $
                    CURRENT=window2)

  plotArr2[2] = PLOT(altDArr[good],diffDPArr[good]-80, $
                    NAME='Diffuse (shifted -80%)', $
                    COLOR='Black', $
                    LINESTYLE='', $
                    SYMBOL='x', $
                    /OVERPLOT, $
                    CURRENT=window2)

  legend = LEGEND(TARGET=plotArr2, $
                  POSITION=[0.8,0.7], $
                  /NORMAL)


  STOP

END
