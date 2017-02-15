;;02/14/17
PRO JOURNAL__20170214__STITCH_ION_GAPS_FILES

  COMPILE_OPT IDL2

  startOrb    = 500             ;Otherwise it just picks the first orbit in eSpec
  endOrb      = 14361
  deltaSavOrb = 500

  saveDir     = '/SPENCEdata/Research/database/FAST/dartdb/ion_db/'
  suffDir     = 'instrument_oddity_times/'  
  ;; suffDir     = ''

  orbRangeF   = STRING(FORMAT='(I0,"-",I0)',startOrb,endOrb)

  @common__newell_ion_db.pro

  ;; LOAD_NEWELL_ESPEC_DB,eSpec,!NULL,NEWELL__delta_t, $
  ;; /LOAD_DELTA_T, $
  ;; /NO_MEMORY_LOAD
  IF N_ELEMENTS(NEWELL_I_ion) EQ 0 THEN BEGIN
     LOAD_NEWELL_ION_DB,/DOWNGOING
  ENDIF
  ;; saveFilePref = "esa_transit_times--"
  prefSuff       = '__oddity_times--'
  saveFilePref   = GET_NEWELL_DB_STRING(NEWELL_I__ion) + prefSuff

  saveFile        = saveFilePref+orbRangeF+'--'+GET_TODAY_STRING(/DO_YYYYMMDD_FMT)+'.sav'

  ji              = ABS(NEWELL_I__ion.ji)
  jei             = ABS(NEWELL_I__ion.jei)
  times           = NEWELL_I__ion.x
  orbit           = (TEMPORARY(NEWELL_I__ion)).orbit

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


  ;;Ji histos
  WINDOW,0
  maxin    = 11
  minin    = 4
  binSize  = 0.1
  maxVal   = 0.01
  !P.MULTI = [0,1,2,0,0] 
  CGHISTOPLOT,ALOG10(ji[allJunk_i]), $
              MAXINPUT=maxin, $
              MININPUT=minin, $
              TITLE='Junk', $
              /FREQUENCY, $
              BINSIZE=binSize, $
              MAX_VALUE=maxVal
  CGHISTOPLOT,ALOG10(ji[keep_i]), $
              MAXINPUT=maxin, $
              MININPUT=minin, $
              TITLE='Not junk', $
              /FREQUENCY, $
              BINSIZE=binSize, $
              MAX_VALUE=maxVal

  ;;Jei histos
  WINDOW,1
  maxin    = 4
  minin    = -4 
  binSize  = 0.1
  maxVal   = 0.01
  !P.MULTI = [0,1,2,0,0] 
  CGHISTOPLOT,ALOG10(jei[allJunk_i]), $
              MAXINPUT=maxin, $
              MININPUT=minin, $
              TITLE='Junk Jei', $
              /FREQUENCY, $
              BINSIZE=binSize, $
              MAX_VALUE=maxVal
  CGHISTOPLOT,ALOG10(jei[keep_i]), $
              MAXINPUT=maxin, $
              MININPUT=minin, $
              TITLE='Not junk Jei', $
              /FREQUENCY, $
              BINSIZE=binSize, $
              MAX_VALUE=maxVal

  hugeJi             = 1e11
  hugeJei            = 1e2
  
  ;;Huge Ji?
  junkHugeJiFrac     = N_ELEMENTS(WHERE(ji[allJunk_i] GE hugeJi))/ $
                       FLOAT(N_ELEMENTS(allJunk_i))*100
  notJunkHugeJiFrac  = N_ELEMENTS(WHERE(ji[keep_i] GE hugeJi))/ $
                       FLOAT(N_ELEMENTS(keep_i))*100
  ;;Huge Jei?
  junkHugeJeiFrac    = N_ELEMENTS(WHERE(jei[allJunk_i] GE hugeJei))/ $
                       FLOAT(N_ELEMENTS(allJunk_i))*100
  notJunkHugeJeiFrac = N_ELEMENTS(WHERE(jei[keep_i] GE hugeJei))/ $
                       FLOAT(N_ELEMENTS(keep_i))*100

  PRINT,"Junk    Huge Ji  % : ",junkHugeJiFrac
  PRINT,"notJunk Huge Ji  % : ",notJunkHugeJiFrac
  PRINT,''
  PRINT,"Junk    Huge Jei % : ",junkHugeJeiFrac
  PRINT,"notJunk Huge Jei % : ",notJunkHugeJeiFrac

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
