;;2017/02/10
PRO JOURNAL__20170210__HVOR_MANGE_AV_HVER_TYPE_HAR_VI_PAA_ALLE_HEIGHTS_I_NORD

  COMPILE_OPT IDL2

  @common__newell_espec.pro
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,/LOAD_CHARE
  ENDIF

  dir         = '/SPENCEdata/Research/Satellites/FAST/OMNI_FAST/saves_output_etc/20170210/'
  ;; good_i_file = 'eSpec_good_i-20170210.sav'
  good_i_file = 'eSpec_good_i__BOTH-20170210.sav'
  restore,dir+good_i_file

  maxAlt = 4300
  minAlt = 400
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
     broadArr = [broadArr,N_ELEMENTS(WHERE(NEWELL__eSpec.broad[bood_i] EQ 1 OR NEWELL__eSpec.broad[bood_i] EQ 2))/FLOAT(nBood)*100.]
     monoArr  = [ monoArr,N_ELEMENTS(WHERE(NEWELL__eSpec.mono[bood_i]  EQ 1 OR NEWELL__eSpec.mono[bood_i]  EQ 2))/FLOAT(nBood)*100.]
     diffArr  = [ diffArr,100.-(broadArr[-1]+monoArr[-1])]
  ENDFOR

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
           k,altArr[k],nHjArr[k],broadArr[k],monoArr[k],diffArr[k]
  ENDFOR

  good    = WHERE(FINITE(broadArr) AND FINITE(monoArr) AND FINITE(diffArr))

  plotArr = MAKE_ARRAY(3,/OBJ)

  plotArr[0] = PLOT(altArr[good],broadArr[good], $
                    NAME='Broad', $
                    COLOR='red', $
                    LINESTYLE='', $
                    SYMBOL='*', $
                    YRANGE=[0,(MAX(broadArr[good]) > MAX(monoArr[good]) > MAX(100-diffArr[good]))], $
                    XRANGE=[300,4300], $
                    XTITLE='Altitude (km)', $
                    YTITLE='% Precip type')

  plotArr[1] = PLOT(altArr[good],monoArr[good], $
                    NAME='Mono', $
                    COLOR='Blue', $
                    LINESTYLE='', $
                    SYMBOL='+', $
                    /OVERPLOT)

  plotArr[2] = PLOT(altArr[good],100.-diffArr[good], $
                    NAME='100-Diffuse', $
                    COLOR='Black', $
                    LINESTYLE='', $
                    SYMBOL='x', $
                    /OVERPLOT)

  legend = LEGEND(TARGET=plotArr, $
                  POSITION=[0.8,0.7], $
                  /NORMAL)


  STOP

END
