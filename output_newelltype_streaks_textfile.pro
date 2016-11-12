;;11/11/16
PRO OUTPUT_NEWELLTYPE_STREAKS_TEXTFILE, $
   ONLY_STRICT=only_strict, $
   ONLY_NONSTRICT=only_nonstrict, $
   BROAD=broad, $
   MONO=mono, $
   DIFFUSE=diffuse, $
   MIN_T_STREAKLEN=min_T_streakLen, $
   


  COMPILE_OPT IDL2

  outDir   = '/SPENCEdata/Research/Satellites/FAST/espec_identification/txtOutput/'
  
  mltRange = [21,24]
  orbRange = [1000,7000]
  altRange = [2000,4300]

  IF N_ELEMENTS(decimal_place) EQ 0 THEN BEGIN
     decimal_place = ALOG10(5.0)
  ENDIF

  IF N_ELEMENTS(min_T_streakLen) EQ 0 THEN BEGIN
     min_T_streakLen = 20 ;in seconds
  ENDIF

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

  outFile  = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--' + typeStr + '_aurora_streaks' + $
             strictStr + '.txt'

  LOAD_NEWELL_ESPEC_DB,eSpec,/DONT_CONVERT_TO_STRICT_NEWELL, $
                       /DONT_LOAD_IN_MEMORY

  thisType  = WHERE(TAG_NAMES(eSpec) EQ STRUPCASE(typeStr))
  IF thisType[0] EQ -1 THEN STOP

  mltI     = GET_MLT_INDS(eSpec,mltRange[0],mltRange[1])
  orbI     = GET_ORBRANGE_INDS(eSpec,orbRange[0],orbRange[1])
  altI     = GET_ALTITUDE_INDS(eSpec,altRange[0],altRange[1])

  CASE 1 OF
     KEYWORD_SET(doStrict): BEGIN
        typeI    = WHERE(eSpec.(thisType) EQ 2,nType)
     END
     (KEYWORD_SET(diffuse) OR KEYWORD_SET(only_nonstrict)): BEGIN
        typeI    = WHERE(eSpec.(thisType) EQ 1,nType)
     END
     ELSE: BEGIN
        typeI    = WHERE((eSpec.(thisType) EQ 1) OR (eSpec.(thisType)) EQ 2,nType)
     END
  ENDCASE

  mltI     = CGSETINTERSECTION(mltI,TEMPORARY(orbI),COUNT=count)
  mltI     = CGSETINTERSECTION(mltI,TEMPORARY(altI),COUNT=count)
  mltI     = CGSETINTERSECTION(mltI,TEMPORARY(typeI),COUNT=count)

  PRINT,count,' inds to work with'

  masterOrb  = eSpec.orbit[mltI]
  masterTime = (TEMPORARY(eSpec.x))[mltI]

  PRINT,'Opening ' + outFile + ' ...'
  OPENW,outLun,outDir+outFile,/GET_LUN


  GET_DOUBLE_STREAKS__NTH_DECIMAL_PLACE,masterTime,decimal_place, $
                                        CURRENT_FOR_PRINTING=eSpec.je[mltI]*1.6e-9, $ ;microA/m2
                                        START_I=strt_i, $
                                        STOP_I=stop_i, $
                                        STREAKLENS=streakLens, $
                                        T_STREAKLENS=tStreak, $
                                        MIN_T_STREAKLEN=min_T_streakLen, $
                                        /PRINT_START_STOP_TIMES, $
                                        /PRINT__INCLUDE_CURRENT, $
                                        OUTLUN=outLun


  PRINT,'Closing ' + outFile + ' ...'
  CLOSE,outLun
  FREE_LUN,outLun

  STOP

END
