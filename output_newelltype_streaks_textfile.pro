;;2016/11/11
;2018/02/03 A thing
;; FOR k=0,6 DO BEGIN & PRINT,1000L+k*3000,", ",3999L+k*3000 & OUTPUT_NEWELLTYPE_STREAKS_TEXTFILE,/MONO,ORBRANGE=[1000L+k*3000,3999L+k*3000] & ENDFOR
PRO OUTPUT_NEWELLTYPE_STREAKS_TEXTFILE, $
   ONLY_STRICT=only_strict, $
   ONLY_NONSTRICT=only_nonstrict, $
   BROAD=broad, $
   MONO=mono, $
   DIFFUSE=diffuse, $
   MIN_T_STREAKLEN=min_T_streakLen, $
   MLTRANGE=mltRange, $
   ORBRANGE=orbRange, $
   ALTRANGE=altRange, $
   ALLOWED_GAP_TIME=allowed_gap_time

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__newell_espec.pro

  outDir   = '/SPENCEdata/Research/Satellites/FAST/espec_identification/txtOutput/'
  
  mltR = KEYWORD_SET(mltRange) ? mltRange : [-5,5]
  orbR = KEYWORD_SET(orbRange) ? orbRange : [1000,16000]
  altR = KEYWORD_SET(altRange) ? altRange : [300,4300]

  IF N_ELEMENTS(decimal_place) EQ 0 THEN BEGIN
     decimal_place = ALOG10(5.0)
  ENDIF

  IF N_ELEMENTS(min_T_streakLen) EQ 0 THEN BEGIN
     min_T_streakLen = 60       ;in seconds
  ENDIF

  allowable_gap_time = KEYWORD_SET(allowed_gap_time) ? allowed_gap_time : 5
  
  CASE 1 OF
     KEYWORD_SET(mono)   : typeStr = 'mono'
     KEYWORD_SET(broad)  : typeStr = 'broad'
     KEYWORD_SET(diffuse): typeStr = 'diffuse'
     ELSE                : BEGIN
        mono    = 1
        typeStr = 'mono'
     END
  ENDCASE

  strictStr = ''
  doStrict  = 0
  IF KEYWORD_SET(only_strict) THEN BEGIN
     IF KEYWORD_SET(diffuse) THEN BEGIN
        PRINT,"Can't do strict diffuse ..."
     ENDIF ELSE BEGIN
        strictStr = '--only_strict'
        doStrict  = 1
     ENDELSE
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(only_nonStrict) THEN BEGIN
        strictStr = '--only_nonStrict'
     ENDIF
  ENDELSE

  mltType = SIZE(mltR[0],/TYPE)
  IF mltType EQ 2 OR mltType EQ 3 THEN BEGIN
     mltFmtStr = 'I0'
  ENDIF ELSE IF mltType EQ 4 OR mltType EQ 5 THEN BEGIN
     mltFmtStr = 'F0.2'
  ENDIF
  CASE (N_ELEMENTS(SIZE(mltR,/DIM))) OF
     1: BEGIN
        minM     = mltR[0]
        maxM     = mltR[1]
        mltStr   = STRING(FORMAT='("__",' + mltFmtStr + ',"-",' $
                          + mltFmtStr + ',"MLT")',mltR[0],mltR[1])
     END
     2: BEGIN
        minM     = mltR[0,*]
        maxM     = mltR[1,*]

        nRange   = N_ELEMENTS(mltR[0,*])
        mltStr   = STRING(FORMAT='("__",' + mltFmtStr + ',"-",' $
                          + mltFmtStr + ')',mltR[0,0],mltR[1,0])
        FOR k=1,nRange-1 DO BEGIN
           mltStr += STRING(FORMAT='("n",' + mltFmtStr + ',"-",' $
                            + mltFmtStr + ')',mltR[0,k],mltR[1,k])
        ENDFOR
        mltStr += 'MLT'
     END
  ENDCASE
  mltStr = mltStr.Replace('.','_')

  orbStr   = STRING(FORMAT='("__",I0,"-",I0,"ORB")',orbR[0],orbR[1])
  altStr   = STRING(FORMAT='("__",I0,"-",I0,"ALT")',altR[0],altR[1])
  min_TStr = STRING(FORMAT='("__minTStreak_sec_",I0)',min_T_streakLen)

  outFile  = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--' + typeStr + '_aurora_streaks' + $
             mltStr + orbStr + altStr + min_TStr + $
             strictStr + '.txt'

  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,/DONT_CONVERT_TO_STRICT_NEWELL, $
                          ;; /NO_MEMORY_LOAD, $
                          /GIGANTE
  ENDIF
  
  thisType  = WHERE(TAG_NAMES(NEWELL__eSpec) EQ STRUPCASE(typeStr))
  IF thisType[0] EQ -1 THEN STOP

  mltI     = GET_MLT_INDS(NEWELL__eSpec,minM,maxM)
  orbI     = GET_ORBRANGE_INDS(NEWELL__eSpec,orbR[0],orbR[1],/KEEP_TRASHINDS_ON_TAP)
  altI     = GET_ALTITUDE_INDS(NEWELL__eSpec,altR[0],altR[1])

  CASE 1 OF
     KEYWORD_SET(doStrict): BEGIN
        typeI    = WHERE(NEWELL__eSpec.(thisType) EQ 2,nType)
     END
     (KEYWORD_SET(diffuse) OR KEYWORD_SET(only_nonstrict)): BEGIN
        typeI    = WHERE(NEWELL__eSpec.(thisType) EQ 1,nType)
     END
     ELSE: BEGIN
        typeI    = WHERE((NEWELL__eSpec.(thisType) EQ 1) OR (NEWELL__eSpec.(thisType)) EQ 2,nType)
     END
  ENDCASE

  mltI     = CGSETINTERSECTION(mltI,TEMPORARY(orbI),COUNT=count)
  mltI     = CGSETINTERSECTION(mltI,TEMPORARY(altI),COUNT=count)
  mltI     = CGSETINTERSECTION(mltI,TEMPORARY(typeI),COUNT=count)

  PRINT,count,' inds to work with'

  pOrb  = NEWELL__eSpec.orbit[mltI]
  pAlt  = NEWELL__eSpec.alt[mltI]
  pMLT  = NEWELL__eSpec.MLT[mltI]
  pILAT = NEWELL__eSpec.ILAT[mltI]
  pTime = (TEMPORARY(NEWELL__eSpec.x))[mltI]

  PRINT,'Opening ' + outFile + ' ...'
  OPENW,outLun,outDir+outFile,/GET_LUN


  GET_DOUBLE_STREAKS__NTH_DECIMAL_PLACE,pTime,decimal_place, $
                                        CURRENT_FOR_PRINTING=NEWELL__eSpec.je[mltI]*1.6e-9, $ ;microA/m2
                                        ORBIT_FOR_PRINTING=pOrb, $
                                        ALT_FOR_PRINTING=pAlt, $
                                        MLT_FOR_PRINTING=pMLT, $
                                        ILAT_FOR_PRINTING=pILAT, $
                                        START_I=strt_i, $
                                        STOP_I=stop_i, $
                                        STREAKLENS=streakLens, $
                                        T_STREAKLENS=tStreak, $
                                        MIN_T_STREAKLEN=min_T_streakLen, $
                                        GAP_TIME=allowable_gap_time, $
                                        /PRINT_START_STOP_TIMES, $
                                        /SORT_BY_T_STREAKLEN, $
                                        /SORT_REVERSE, $
                                        /PRINT__INCLUDE_CURRENT, $
                                        OUTLUN=outLun


  PRINT,'Closing ' + outFile + ' ...'
  CLOSE,outLun
  FREE_LUN,outLun

END
