;;01/19/17
PRO JOURNAL__20170119__STITCH_GAPS_FILES

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; startOrb    = 500             ;Otherwise it just picks the first orbit in eSpec
  ;; endOrb      = 9500
  ;; startOrb    = 500             ;Otherwise it just picks the first orbit in eSpec
  ;; endOrb      = 16361 
  startOrb    = 500             ;Otherwise it just picks the first orbit in eSpec
  endOrb      = 24634
  deltaSavOrb = 500

  ;; saveDir     = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'
  suffDir     = 'instrument_oddity_times/'  
  ;; suffDir     = ''

  orbRangeF   = STRING(FORMAT='(I0,"-",I0)',startOrb,endOrb)

  @common__newell_espec.pro

  ;; LOAD_NEWELL_ESPEC_DB,eSpec,!NULL,NEWELL__delta_t, $
  ;; /LOAD_DELTA_T, $
  ;; /NO_MEMORY_LOAD
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,!NULL,!NULL,!NULL, $
                          /DONT_MAP_TO_100KM, $
                          /DO_NOT_MAP_DELTA_T, $
                          /DONT_CONVERT_TO_STRICT_NEWELL
                          ;; /LOAD_DELTA_T

  ENDIF
  ;; saveFilePref = "esa_transit_times--"
  prefSuff       = '__oddity_times--'
  saveFilePref   = GET_NEWELL_DB_STRING(NEWELL__eSpec) + prefSuff

  saveFile    = saveFilePref+orbRangeF+'--'+GET_TODAY_STRING(/DO_YYYYMMDD_FMT)+'.sav'

  je              = NEWELL__eSpec.je
  jee             = NEWELL__eSpec.jee
  times           = NEWELL__eSpec.x
  orbit           = (TEMPORARY(NEWELL__eSpec)).orbit

  hjar = WHERE(orbit GE startOrb AND $
               orbit LE endOrb,nHjar )
  ;; CHECK_SORTED,orbit,is_sorted,/QUIET
  ;; IF ~is_sorted THEN STOP
  
  lastSavOrb           = startOrb

  nElem                = 2000000
  junk_i_final         = MAKE_ARRAY(nElem,VALUE=-1,/LONG  )
  befStart_i_final     = MAKE_ARRAY(nElem,VALUE=-1,/LONG  )
  junkTimes_final      = MAKE_ARRAY(nElem,VALUE=0.D,/DOUBLE)
  befStartTimes_final  = MAKE_ARRAY(nElem,VALUE=0.D,/DOUBLE)
  missingItvls_final   = MAKE_ARRAY(nElem,2,VALUE=0,/LONG)
  nJunk_final          = 0
  nBefStart_final      = 0
  nMissingItvls_final  = 0

  WHILE lastSavOrb LE endOrb DO BEGIN

     junk_i            = !NULL
     befStart_i        = !NULL
     junkTimes         = !NULL
     befStartTimes     = !NULL
     missingIntervals  = !NULL

     ultraOrb          = (lastSavOrb+deltaSavOrb-1) < endOrb
     orbRangeString    = STRING(FORMAT='(I0,"-",I0)',lastSavOrb,ultraOrb)
     orbSavFileName    = STRING(FORMAT='(A0,A0,".sav")',saveFilePref,orbRangeString)

     IF FILE_TEST(saveDir+suffDir+orbSavFileName) THEN BEGIN
        
        PRINT,"Restoring " + orbSavFileName + ' ...'
        RESTORE,saveDir+suffDir+orbSavFileName

        nJunk         = N_ELEMENTS(junk_i)
        nBefStart     = N_ELEMENTS(befStart_i)
        nMissingItvls = N_ELEMENTS(missingIntervals)
        PRINT,STRING(FORMAT='("Loading ",I0," junk inds and ",I0," befStart inds for orbs ",A0," to file : ",A0)', $
                     nJunk,nBefStart,orbRangeString,orbSavFileName)

        IF (nJunk     NE N_ELEMENTS(junkTimes    )) OR $
           (nBefStart NE N_ELEMENTS(befStartTimes))    $
        THEN BEGIN
           PRINT,"Unequal things!"
           STOP
        ENDIF

        IF nJunk GT 0 THEN BEGIN
           tmpJunk_i                  = [nJunk_final:(nJunk_final+nJunk-1)]
           junk_i_final[tmpJunk_i]    = junk_i
           junkTimes_final[tmpJunk_i] = junkTimes
        ENDIF

        IF nBefStart GT 0 THEN BEGIN
           tmpBefStart_i                      = [nBefStart_final:(nBefStart_final+nBefStart-1)]
           befStart_i_final[tmpBefStart_i]    = befStart_i
           befStartTimes_final[tmpBefStart_i] = befStartTimes
        ENDIF
        
        IF nMissingItvls GT 0 THEN BEGIN
           IF (nMissingItvls MOD 2) NE 0 THEN STOP
           nMissingItvls     /= 2
           tmpMissingItvls_i  = [nMissingItvls_final:(nMissingItvls_final+nMissingItvls-1)]
           missingItvls_final[tmpMissingItvls_i,*] = missingIntervals
        ENDIF

        nJunk_final         += nJunk
        nBefStart_final     += nBefStart
        nMissingItvls_final += nMissingItvls

     ENDIF ELSE BEGIN
        PRINT,"Couldn't load " + orbSavFileName + "!!"
        STOP
     ENDELSE

     lastSavOrb += deltaSavOrb

  ENDWHILE

  junk_i_final        = junk_i_final[0:(nJunk_final-1)]
  junkTimes_final     = junkTimes_final[0:(nJunk_final-1)]
  befStart_i_final    = befStart_i_final[0:(nBefStart_final-1)]
  befStartTimes_final = befStartTimes_final[0:(nBefStart_final-1)]
  missingItvls_final  = missingItvls_final[[0:(nMissingItvls_final-1)],*]

  junk_i        = TEMPORARY(junk_i_final)
  junkTimes     = TEMPORARY(junkTimes_final)
  befStart_i    = TEMPORARY(befStart_i_final)
  befStartTimes = TEMPORARY(befStartTimes_final)
  missingItvls  = TEMPORARY(missingItvls_final)

  percJunk     = (nJunk_final+nBefStart_final)/FLOAT(nHjar)*100
  PRINT,FORMAT='(A0,T10,A0,T20,A0,T30,A0,T40,A0)', $
        "N Junk","N BfStrt","NMssItvl","N tot","% Junked"
  PRINT,FORMAT='(I0,T10,I0,T20,I0,T30,I0,T40,F0.2)', $
        nJunk_final,nBefStart_final,nMissingItvls_final,nHjar,percJunk


  badTimes = [junkTimes,befStartTimes]
  badTimes = badTimes[SORT(badTimes)]
  this     = VALUE_CLOSEST2(times,badTimes)

  allJunk_i = CGSETUNION(junk_i,befstart_i)
  keep_i = CGSETDIFFERENCE(hjar,allJunk_i)


  ;;Je histos
  WINDOW,0
  maxin    = 11 
  minin    = 9 
  binSize  = 0.1
  maxVal   = 0.01
  !P.MULTI = [0,1,2,0,0] 
  CGHISTOPLOT,ALOG10(je[allJunk_i]), $
              MAXINPUT=maxin, $
              MININPUT=minin, $
              TITLE='Junk', $
              /FREQUENCY, $
              BINSIZE=binSize, $
              MAX_VALUE=maxVal
  CGHISTOPLOT,ALOG10(je[keep_i]), $
              MAXINPUT=maxin, $
              MININPUT=minin, $
              TITLE='Not junk', $
              /FREQUENCY, $
              BINSIZE=binSize, $
              MAX_VALUE=maxVal

  ;;Jee histos
  WINDOW,1
  maxin    = 3
  minin    = 0 
  binSize  = 0.1
  maxVal   = 0.01
  !P.MULTI = [0,1,2,0,0] 
  CGHISTOPLOT,ALOG10(jee[allJunk_i]), $
              MAXINPUT=maxin, $
              MININPUT=minin, $
              TITLE='Junk Jee', $
              /FREQUENCY, $
              BINSIZE=binSize, $
              MAX_VALUE=maxVal
  CGHISTOPLOT,ALOG10(jee[keep_i]), $
              MAXINPUT=maxin, $
              MININPUT=minin, $
              TITLE='Not junk Jee', $
              /FREQUENCY, $
              BINSIZE=binSize, $
              MAX_VALUE=maxVal

  hugeJe             = 1e11
  hugeJee            = 1e2
  
  ;;Huge Je?
  junkHugeJeFrac     = N_ELEMENTS(WHERE(je[allJunk_i] GE hugeJe))/ $
                       FLOAT(N_ELEMENTS(allJunk_i))*100
  notJunkHugeJeFrac  = N_ELEMENTS(WHERE(je[keep_i] GE hugeJe))/ $
                       FLOAT(N_ELEMENTS(keep_i))*100
  ;;Huge Jee?
  junkHugeJeeFrac    = N_ELEMENTS(WHERE(jee[allJunk_i] GE hugeJee))/ $
                       FLOAT(N_ELEMENTS(allJunk_i))*100
  notJunkHugeJeeFrac = N_ELEMENTS(WHERE(jee[keep_i] GE hugeJee))/ $
                       FLOAT(N_ELEMENTS(keep_i))*100

  PRINT,"Junk    Huge Je  % : ",junkHugeJeFrac
  PRINT,"notJunk Huge Je  % : ",notJunkHugeJeFrac
  PRINT,''
  PRINT,"Junk    Huge Jee % : ",junkHugeJeeFrac
  PRINT,"notJunk Huge Jee % : ",notJunkHugeJeeFrac

  IF N_ELEMENTS(times) NE $
     (N_ELEMENTS(keep_i)+N_ELEMENTS(allJunk_i)) $
  THEN BEGIN
     PRINT,"Ooooohhh yeah. Got a problem."
     STOP
  ENDIF

  PRINT,"Saving to " + saveFile + ' ...'
  STOP

  SAVE,keep_i, $
       junk_i,junkTimes, $
       befStart_i,befStartTimes, $
       missingItvls, $
       FILENAME=saveDir+suffDir+saveFile
  
END
