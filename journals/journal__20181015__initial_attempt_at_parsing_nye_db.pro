;2018/10/15
PRO JOURNAL__20181015__INITIAL_ATTEMPT_AT_PARSING_NYE_DB, $
   MAKE_LC_MOMS=make_LC_moms, $
   MAKE_ALL_MOMS=make_all_moms, $
   MAKE_LC_ERRS=make_LC_errs, $
   MAKE_ALL_ERRS=make_all_errs, $
   MAKE_EPHEMERIS=make_ephemeris, $
   MAKE_EXTRA=make_extra, $
   FOR_KARLSON=karlson

  COMPILE_OPT IDL2,STRICTARRSUBS

  inDir        = '/thelonious_data1/FAST_electrons_2018/'
  IF KEYWORD_SET(karlson) THEN BEGIN
     inDir     = '/media/spencerh/data/FAST_electrons/'
  ENDIF
  outDir       = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/v2018/'

  inFile_pref  = 'electron_moments_and_spectral_identification__Orbit_'


  tmpFile      = inFile_pref+'10000'+'.sav'

  eTron        = !NULL
  extra        = !NULL

  RESTORE,inDir+tmpFile

  startOrb     = 1000
  stopOrb      = 25445

  todayStr = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  outPref = STRING(FORMAT='("eMomDB_",A0,"-",I0,"-",I0)', $
                   todayStr, $
                   startOrb, $
                   stopOrb)

  ;; Assume ~ 1500 elements per orbit
  nPerOrb = 1500L
  n_elem = nPerOrb * (stopOrb-startOrb)

  ;; Have three copies of mapratio--eTron.moments.lc.mapRatio, eTron.moments.all.mapRatio, and extra.mapRatio.
  ;; See: 
  ;; FOR k=0,874 DO PRINT,k,", ",etron.moments.lc.mapratio[k],", ",extra.mapratio[k],", ",etron.moments.lc.mapratio[k]-extra.mapratio[k]

  ;; Have three copies of time--eTron.moments.lc.time, eTron.moments.all.time, and eTron.time.
  ;; See: 
  ;; FOR k=0,874 DO PRINT,k,", ",etron.time[k],", ",eTron.moments.lc.time[k],", ",etron.time[k]-eTron.moments.lc.time[k]


  ;; Six options: make LC moments, make all moments, Make LC error, make all error, make ephem, and make extra


  saveStr = "SAVE,"

  byteType   = 1
  floatType  = 4
  doubleType = 5
  CASE 1 OF
     KEYWORD_SET(make_LC_moms) OR KEYWORD_SET(make_all_moms): BEGIN

        ;;moments contains
        ;; n        
        ;; j        
        ;; je       
        ;; T        
        ;; ;; charE    
        ;; ;; cur      
        ;; ;; perp     
        ;; ;; all      
        ;; ;; info     

        CASE 1 OF
           KEYWORD_SET(make_LC_moms ): BEGIN
              momType = 'LC'
              dbType  = 'LCangle_moms'
           END
           KEYWORD_SET(make_all_moms): BEGIN
              momType = 'ALL'
              dbType  = 'ALLangle_moms'
           END
        ENDCASE
        momInd = WHERE(STRUPCASE(TAG_NAMES(eTron.moments)) EQ momType)

        IF momInd[0] EQ -1 THEN STOP

        ;; eTron = { $
        ;;         valid : elec_dEF.valid, $
        ;;         time : elec_dEF.time, $
        ;;         orbit  : TEMPORARY(orbit), $
        ;;         mlt  : TEMPORARY(mlt), $
        ;;         ilat : TEMPORARY(ilat), $
        ;;         alt  : TEMPORARY(alt), $
        ;;         moments : {lc : eMomStruct_lcAngle, $
        ;;                    all : eMomStruct_allAngle}, $
        ;;         mono : eSpecs_parsed.mono, $
        ;;         broad : eSpecs_parsed.broad, $
        ;;         diffuse : eSpecs_parsed.diffuse}
        

        ;; all  = {j          : TEMPORARY(jAll), $
        ;;         je         : TEMPORARY(jeAll       ) , $
        ;;         charE      : TEMPORARY(charEAll    ), $
        ;;         speed      : TEMPORARY(speedAll)}
        ;; ;; jjePerp_coVar  : TEMPORARY(jjePerp_coVar)}

        ;; perp = {j          : TEMPORARY(jPerp), $
        ;;         je         : TEMPORARY(jePerp       ) , $
        ;;         charE      : TEMPORARY(charEPerp    ) , $
        ;;         cur        : TEMPORARY(curPerp      ) , $
        ;;         jje_coVar  : TEMPORARY(jjePerp_coVar) , $
        ;;         jErr       : TEMPORARY(jPerpErr     ) , $
        ;;         jeErr      : TEMPORARY(jePerpErr    ) , $
        ;;         curErr     : TEMPORARY(curPerpErr   ) , $
        ;;         charEErr   : TEMPORARY(charEPerpErr )}

        moms = { $
               n        : MAKE_ARRAY(   n_elem,TYPE=floatType), $
               j        : MAKE_ARRAY(   n_elem,TYPE=floatType), $
               je       : MAKE_ARRAY(   n_elem,TYPE=floatType), $
               T        : MAKE_ARRAY(4, n_elem,TYPE=floatType), $
               charE    : MAKE_ARRAY(   n_elem,TYPE=floatType), $
               nErr     : MAKE_ARRAY(   n_elem,TYPE=floatType), $
               jErr     : MAKE_ARRAY(   n_elem,TYPE=floatType), $
               TErr     : MAKE_ARRAY(4, n_elem,TYPE=floatType), $
               charEErr : MAKE_ARRAY(   n_elem,TYPE=floatType)}

        ;; struct = {time           : TEMPORARY(time     ), $
        ;;           n              : TEMPORARY(n        ), $
        ;;           j              : TEMPORARY(j        ), $
        ;;           je             : TEMPORARY(je       ), $
        ;;           mapRatio       : mapRatio            , $
        ;;           T              : TEMPORARY(T        ), $
        ;;           charE          : TEMPORARY(charE    ), $
        ;;           cur            : TEMPORARY(cur      ), $
        ;;           jje_coVar      : TEMPORARY(jje_coVar), $
        ;;           errors         : TEMPORARY(errors   ), $
        ;;           nErr           : TEMPORARY(nErr     ), $
        ;;           jErr           : TEMPORARY(jErr     ), $
        ;;           jeErr          : TEMPORARY(jeErr    ), $
        ;;           TErr           : TEMPORARY(TErr     ), $
        ;;           curErr         : TEMPORARY(curErr   ), $
        ;;           charEErr       : TEMPORARY(charEErr ), $
        ;;           perp           : TEMPORARY(perp), $
        ;;           all            : TEMPORARY(all), $
        ;;           info           :{ is_mapped : KEYWORD_SET(map_to_100km)}}

     END
     KEYWORD_SET(make_LC_errs) OR KEYWORD_SET(make_all_errs): BEGIN

        ;;errs contains
        ;; jje_coVar
        ;; errors   
        ;; nErr     
        ;; jErr     
        ;; jeErr    
        ;; TErr     
        ;; curErr   
        ;; charEErr 
        ;; ;; perp     
        ;; ;; all      
        ;; ;; info     

        CASE 1 OF
           KEYWORD_SET(make_LC_moms ): BEGIN
              momType = 'LC'
              dbType  = 'LCangle_errs'
           END
           KEYWORD_SET(make_all_moms): BEGIN
              momType = 'ALL'
              dbType  = 'ALLangle_errs'
           END
        ENDCASE

        momInd = WHERE(STRUPCASE(TAG_NAMES(eTron.moments)) EQ momType)

        IF momInd[0] EQ -1 THEN STOP

        STOP

     END
     KEYWORD_SET(make_ephemeris): BEGIN

        dbType = 'ephem'

        ;; Ephem contains these
        ephem = { $
                valid    : MAKE_ARRAY(n_elem,TYPE=SIZE(eTron.valid   ,/TYPE)), $
                time     : MAKE_ARRAY(n_elem,TYPE=SIZE(eTron.time    ,/TYPE)), $
                orbit    : MAKE_ARRAY(n_elem,TYPE=SIZE(eTron.orbit   ,/TYPE)), $
                mlt      : MAKE_ARRAY(n_elem,TYPE=floatType), $
                ilat     : MAKE_ARRAY(n_elem,TYPE=floatType), $
                alt      : MAKE_ARRAY(n_elem,TYPE=floatType), $
                mono     : MAKE_ARRAY(n_elem,TYPE=SIZE(eTron.mono    ,/TYPE)), $
                broad    : MAKE_ARRAY(n_elem,TYPE=SIZE(eTron.broad   ,/TYPE)), $
                diffuse  : MAKE_ARRAY(n_elem,TYPE=SIZE(eTron.diffuse ,/TYPE))}

        ;; eTron = { $
        ;;         valid : elec_dEF.valid, $
        ;;         time : elec_dEF.time, $
        ;;         orbit  : TEMPORARY(orbit), $
        ;;         mlt  : TEMPORARY(mlt), $
        ;;         ilat : TEMPORARY(ilat), $
        ;;         alt  : TEMPORARY(alt), $
        ;;         moments : {lc : eMomStruct_lcAngle, $
        ;;                    all : eMomStruct_allAngle}, $
        ;;         mono : eSpecs_parsed.mono, $
        ;;         broad : eSpecs_parsed.broad, $
        ;;         diffuse : eSpecs_parsed.diffuse}

     END
     KEYWORD_SET(make_extra): BEGIN

        dbType = 'extra'

        eExtra = { $
                 tDiffs         : MAKE_ARRAY(   n_elem,TYPE=floatType), $        
                 eSpec_bad_time : MAKE_ARRAY(   n_elem,TYPE=byteType ), $
                 fa_pos         : MAKE_ARRAY(3, n_elem,TYPE=floatType), $        
                 fa_vel         : MAKE_ARRAY(3, n_elem,TYPE=floatType), $        
                 b_model        : MAKE_ARRAY(3, n_elem,TYPE=floatType), $       
                 b_foot         : MAKE_ARRAY(3, n_elem,TYPE=floatType), $        
                 mapRatio       : MAKE_ARRAY(   n_elem,TYPE=floatType), $      
                 foot_lat       : MAKE_ARRAY(   n_elem,TYPE=floatType), $      
                 foot_lng       : MAKE_ARRAY(   n_elem,TYPE=floatType), $      
                 losscone       : MAKE_ARRAY(2, n_elem,TYPE=floatType)}

     END
  ENDCASE

  outDB = outPref + '-' + dbType + '.sav'
  saveStr = saveStr + dbType + ",FILENAME='" + outDir + outDB + "'"

  PRINT,"Creating " + outDB + ' ...'

  curInd = 0L
  FOR iOrb=startOrb,stopOrb DO BEGIN

     ;; Sikre den eksisterer
     orbFile = inFile_pref+STRING(FORMAT='(I0)',iOrb)+'.sav'

     IF ~FILE_TEST(inDir + orbFile) THEN BEGIN
        PRINT,FORMAT='("No file for orbit ",I0,"!!!")',iOrb
        CONTINUE
     ENDIF

     ;; Clear out old ones
     eTron = !NULL
     extra = !NULL

     ;; Få strukturene
     RESTORE,inDir+orbFile

     ;; Hvor mange her?
     nHer = N_ELEMENTS(eTron.time)

     stopInd = curInd+nHer-1

     tmpInds = [curInd:stopInd]

     PRINT,FORMAT='(I5," : ",I4,TR5,"(",I8," - ",I8,")",TR10,"AVG: ",I0)', $
           iOrb, $
           nHer, $
           curInd, $
           stopInd, $
           stopInd/(iOrb-startOrb+1)

     ;; Hvilken type DB?
     CASE 1 OF
        KEYWORD_SET(make_LC_moms) OR KEYWORD_SET(make_all_moms): BEGIN

               moms.n        [   tmpInds] = eTron.moments.(momInd).n       
               moms.j        [   tmpInds] = eTron.moments.(momInd).j       
               moms.je       [   tmpInds] = eTron.moments.(momInd).je      
               moms.T        [*, tmpInds] = eTron.moments.(momInd).T       
               moms.charE    [   tmpInds] = eTron.moments.(momInd).charE   
               moms.nErr     [   tmpInds] = eTron.moments.(momInd).nErr    
               moms.jErr     [   tmpInds] = eTron.moments.(momInd).jErr    
               moms.TErr     [*, tmpInds] = eTron.moments.(momInd).TErr    
               moms.charEErr [   tmpInds] = eTron.moments.(momInd).charEErr

        END
        KEYWORD_SET(make_LC_errs) OR KEYWORD_SET(make_all_errs): BEGIN

        END
        KEYWORD_SET(make_ephemeris): BEGIN

           ephem.valid   [tmpInds] = eTron.valid  
           ephem.time    [tmpInds] = eTron.time   
           ephem.orbit   [tmpInds] = eTron.orbit  
           ephem.mlt     [tmpInds] = eTron.mlt    
           ephem.ilat    [tmpInds] = eTron.ilat   
           ephem.alt     [tmpInds] = eTron.alt    
           ephem.mono    [tmpInds] = eTron.mono   
           ephem.broad   [tmpInds] = eTron.broad  
           ephem.diffuse [tmpInds] = eTron.diffuse

        END
        KEYWORD_SET(make_extra): BEGIN

           eExtra.tDiffs         [   tmpInds] = extra.tDiffs        
           eExtra.eSpec_bad_time [   tmpInds] = extra.eSpec_bad_time
           eExtra.fa_pos         [*, tmpInds] = extra.fa_pos        
           eExtra.fa_vel         [*, tmpInds] = extra.fa_vel        
           eExtra.b_model        [*, tmpInds] = extra.b_model       
           eExtra.b_foot         [*, tmpInds] = extra.b_foot        
           eExtra.mapRatio       [   tmpInds] = extra.mapRatio      
           eExtra.foot_lat       [   tmpInds] = extra.foot_lat      
           eExtra.foot_lng       [   tmpInds] = extra.foot_lng      
           eExtra.losscone       [*, tmpInds] = extra.losscone      

        END
     ENDCASE

     curInd = curInd + nHer

     IF curInd GE n_elem THEN BEGIN
        PRINT,"This won't work!"
        STOP
     ENDIF

  ENDFOR

  PRINT,FORMAT='("Got ",I0," total! Trimming DB ...")',curInd
  finalInds = [0L:curInd-1]
  CASE 1 OF
     KEYWORD_SET(make_LC_moms) OR KEYWORD_SET(make_all_moms): BEGIN

        moms = { $
               n        : moms.n       [   finalInds], $
               j        : moms.j       [   finalInds], $
               je       : moms.je      [   finalInds], $
               T        : moms.T       [*, finalInds], $
               charE    : moms.charE   [   finalInds], $
               nErr     : moms.nErr    [   finalInds], $
               jErr     : moms.jErr    [   finalInds], $
               TErr     : moms.TErr    [*, finalInds], $
               charEErr : moms.charEErr[   finalInds]}

        CASE 1 OF
           KEYWORD_SET(make_LC_moms ): BEGIN
              LCangle_moms = TEMPORARY(moms)
           END
           KEYWORD_SET(make_all_moms): BEGIN
              ALLangle_moms = TEMPORARY(moms)
           END
        ENDCASE

     END
     KEYWORD_SET(make_LC_errs) OR KEYWORD_SET(make_all_errs): BEGIN

     END
     KEYWORD_SET(make_ephemeris): BEGIN

        ephem = { $
                valid    : ephem.valid  [finalInds], $
                time     : ephem.time   [finalInds], $
                orbit    : ephem.orbit  [finalInds], $
                mlt      : ephem.mlt    [finalInds], $
                ilat     : ephem.ilat   [finalInds], $
                alt      : ephem.alt    [finalInds], $
                mono     : ephem.mono   [finalInds], $
                broad    : ephem.broad  [finalInds], $
                diffuse  : ephem.diffuse[finalInds]}

     END
     KEYWORD_SET(make_extra): BEGIN

        extra = TEMPORARY(eExtra)
        extra = { $
                 tDiffs         : extra.tDiffs        [   finalInds], $        
                 eSpec_bad_time : extra.eSpec_bad_time[   finalInds], $
                 fa_pos         : extra.fa_pos        [*, finalInds], $        
                 fa_vel         : extra.fa_vel        [*, finalInds], $        
                 b_model        : extra.b_model       [*, finalInds], $       
                 b_foot         : extra.b_foot        [*, finalInds], $        
                 mapRatio       : extra.mapRatio      [   finalInds], $      
                 foot_lat       : extra.foot_lat      [   finalInds], $      
                 foot_lng       : extra.foot_lng      [   finalInds], $      
                 losscone       : extra.losscone      [*, finalInds]}

     END
  ENDCASE

  PRINT,"Saving " + outDB + " ..."
  PRINT,saveStr

  STOP

  good = EXECUTE(saveStr)

  IF ~good THEN STOP

END
  ;; CASE 1 OF
  ;;    KEYWORD_SET(make_LC_moms) OR KEYWORD_SET(make_all_moms): BEGIN

  ;;    END
  ;;    KEYWORD_SET(make_LC_errs) OR KEYWORD_SET(make_all_errs): BEGIN

  ;;    END
  ;;    KEYWORD_SET(make_ephemeris): BEGIN

  ;;    END
  ;;    KEYWORD_SET(make_extra): BEGIN

  ;;    END
  ;; ENDCASE
