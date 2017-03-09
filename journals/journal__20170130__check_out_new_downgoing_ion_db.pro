;;2017/01/30 
;;2017/01/30 Man, I forgot to include timestamps in the first 8644 files!!
;;2017/02/01 For the first 8644 orbits, we also open up the time file and tack that on
;;2017/02/14 Of course, only orbits without time stamps (i.e., before 8644) have the risk of being screwed up. Everything after is buono
;;           List at bottom.
PRO JOURNAL__20170130__CHECK_OUT_NEW_DOWNGOING_ION_DB

  COMPILE_OPT IDL2,STRICTARRSUBS

  dry_run             = 0

  make_ephem          = 0
  make_highE          = 0
  make_lowE           = 0
  make_combE          = 1
  
  dir                 = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/downgoing_ions__v1_output/'
  fPref               = 'downgoing_ions__v1--orbit_'
  tidFPref            = 'downgoing_ions__v1__time--orbit_'
  fSuff               = '.sav'

  outNavn             = 'downgoing_ions__'
  outDir              = '/SPENCEdata/Research/database/FAST/dartdb/ion_db/'

  startOrb            = 500
  ;; startOrb         = 13661
  stopOrb             = 14361 ;;as of 2017/02/08 this is all you had

  check_for_existing  = 1

  options             = ['ORBIT','ALT','MLT','ILAT','RATIO',                                   $
                         'ISPEC_DOWN_LC_RAM',                                                   $
                         'TMPJEI_DOWN_HIGHE','TMPJEI_DOWN_HIGHE_LC','TMPJEI_DOWN_HIGHE_LC_RAM', $
                         'TMPJI_DOWN_HIGHE' ,'TMPJI_DOWN_HIGHE_LC' ,'TMPJI_DOWN_HIGHE_LC_RAM' , $
                         'TMPJEI_DOWN_LOWE' ,'TMPJEI_DOWN_LOWE_LC' ,'TMPJEI_DOWN_LOWE_LC_RAM' , $
                         'TMPJI_DOWN_LOWE'  ,'TMPJI_DOWN_LOWE_LC'  ,'TMPJI_DOWN_LOWE_LC_RAM']

  maxNElem            = 30000000

  DB_version = '0.1'
  DB_extras  = 'mapRatio'
  orig_rtine = 'JOURNAL__20170130__CHECK_OUT_NEW_DOWNGOING_ION_DB'
  ;;Info struct
  info       = {DB_date              : GET_TODAY_STRING(/DO_YYYYMMDD_FMT) , $
                DB_version           : DB_version                         , $
                DB_extras            : DB_extras                          , $
                ;; eRange               : eRange                             , $
                originating_routine  : orig_rtine                         }
  time       = MAKE_ARRAY(maxNElem,/DOUBLE)
  CASE 1 OF
     KEYWORD_SET(make_ephem) : BEGIN
        ephem                  = { $;; x           : MAKE_ARRAY(maxNElem,/DOUBLE), $
                                  orbit       : MAKE_ARRAY(maxNElem,/UINT  ), $
                                  alt         : MAKE_ARRAY(maxNElem,/FLOAT ), $
                                  mlt         : MAKE_ARRAY(maxNElem,/FLOAT ), $
                                  ilat        : MAKE_ARRAY(maxNElem,/FLOAT ), $
                                  ratio       : MAKE_ARRAY(maxNElem,/FLOAT )}
        dbNavn                 = 'ephem'
        outNavnSuff            = 'ephem'
        
        membres                = ['ORBIT','ALT','MLT','ILAT','RATIO']
     END
     KEYWORD_SET(make_highE) OR KEYWORD_SET(make_lowE) OR KEYWORD_SET(make_combE): BEGIN
        ion                    = {jei         : MAKE_ARRAY(maxNElem,/FLOAT), $
                                  jei_lc      : MAKE_ARRAY(maxNElem,/FLOAT), $
                                  jei_lc_ram  : MAKE_ARRAY(maxNElem,/FLOAT), $
                                  ji          : MAKE_ARRAY(maxNElem,/FLOAT), $
                                  ji_lc       : MAKE_ARRAY(maxNElem,/FLOAT), $
                                  ji_lc_ram   : MAKE_ARRAY(maxNElem,/FLOAT)}
        
        dbNavn                 = 'ion'
        
        CASE 1 OF
           KEYWORD_SET(make_highE): BEGIN
              outNavnSuff      = 'highE'

              membres          = ['TMPJEI_DOWN_HIGHE','TMPJEI_DOWN_HIGHE_LC','TMPJEI_DOWN_HIGHE_LC_RAM', $
                                  'TMPJI_DOWN_HIGHE' ,'TMPJI_DOWN_HIGHE_LC' ,'TMPJI_DOWN_HIGHE_LC_RAM'   ]
              info.DB_version += '_highE'
              STR_ELEMENT,info,'eRange',[300,2.4e4],/ADD_REPLACE
           END
           KEYWORD_SET(make_lowE): BEGIN
              outNavnSuff      = 'lowE'
              membres          = ['TMPJEI_DOWN_LOWE','TMPJEI_DOWN_LOWE_LC','TMPJEI_DOWN_LOWE_LC_RAM', $
                                  'TMPJI_DOWN_LOWE' ,'TMPJI_DOWN_LOWE_LC' ,'TMPJI_DOWN_LOWE_LC_RAM'   ]
              info.DB_version += '_lowE'
              STR_ELEMENT,info,'eRange',[0,300],/ADD_REPLACE
           END
           KEYWORD_SET(make_combE): BEGIN
              outNavnSuff      = 'combE'
              membres          = ['TMPJEI_DOWN_COMBE','TMPJEI_DOWN_COMBE_LC','TMPJEI_DOWN_COMBE_LC_RAM', $
                                  'TMPJI_DOWN_COMBE' ,'TMPJI_DOWN_COMBE_LC' ,'TMPJI_DOWN_COMBE_LC_RAM'   ]
              info.DB_version += '_combE'
              STR_ELEMENT,info,'eRange',[0,2.4e4],/ADD_REPLACE
           END
        ENDCASE
     END
  ENDCASE

  outNavn += outNavnSuff + '__v'+DB_version.REPLACE('.','_') + '.sav'
  PRINT,"Making " + outNavn + ', avec ' + STRING(FORMAT='(6(A0,:,", "))',membres) + ' ...'

  execStr = STRING(FORMAT='("SAVE,",A0,",","FILENAME=",A0,A0)',dbNavn,'"'+outDir+outNavn+'"')
  PRINT,execStr

  totCnt = 0L
  hadToFixOrbs  = !NULL
  hadToRedoOrbs = !NULL
  FOR curOrb=startOrb,stopOrb DO BEGIN

     ;; nOffset    = 0
     itvl       = 0
     orbStr     = STRING(FORMAT='(I0)',curOrb)

     filNavn    = STRING(FORMAT='(A0,A0,"_",I0,A0)',fPref,orbStr,itvl,fSuff)
     alreadyHadToFix = 0
     alreadyHadToRedo = 0
     WHILE FILE_TEST(dir+filNavn) DO BEGIN

        JEi_down_highE_i = !NULL
        RESTORE,dir+filNavn

        CASE 1 OF
           KEYWORD_SET(make_ephem): BEGIN
              nHere = N_ELEMENTS(mlt)
              IF (nHere NE (N_ELEMENTS(ilat) > N_ELEMENTS(ratio) > N_ELEMENTS(orbit) > N_ELEMENTS(alt))) OR $
                 (nHere NE (N_ELEMENTS(ilat) < N_ELEMENTS(ratio) < N_ELEMENTS(orbit) < N_ELEMENTS(alt)))    $
              THEN STOP
           END
           KEYWORD_SET(make_highE): BEGIN
              nHere = N_ELEMENTS(tmpJi_down_highE_lc_ram)
              IF nHere NE (N_ELEMENTS(tmpJi_down_highE ) AND N_ELEMENTS(tmpJi_down_highE_lc ) AND $
                           N_ELEMENTS(tmpJei_down_highE) AND N_ELEMENTS(tmpJei_down_highE_lc) AND N_ELEMENTS(tmpJei_down_highE_lc_ram)) $
              THEN STOP
           END
           KEYWORD_SET(make_lowE): BEGIN
              nHere = N_ELEMENTS(tmpJi_down_lowE_lc_ram)
              IF nHere NE (N_ELEMENTS(tmpJi_down_lowE ) AND N_ELEMENTS(tmpJi_down_lowE_lc ) AND $
                           N_ELEMENTS(tmpJei_down_lowE) AND N_ELEMENTS(tmpJei_down_lowE_lc) AND N_ELEMENTS(tmpJei_down_lowE_lc_ram)) $
              THEN STOP
           END
           KEYWORD_SET(make_combE): BEGIN
              nHere = N_ELEMENTS(tmpJi_down_highE_lc_ram)
              IF nHere NE (N_ELEMENTS(tmpJi_down_highE ) AND N_ELEMENTS(tmpJi_down_highE_lc ) AND $
                           N_ELEMENTS(tmpJei_down_highE) AND N_ELEMENTS(tmpJei_down_highE_lc) AND N_ELEMENTS(tmpJei_down_highE_lc_ram)) $
              THEN STOP

              nHere = N_ELEMENTS(tmpJi_down_lowE_lc_ram)
              IF nHere NE (N_ELEMENTS(tmpJi_down_lowE ) AND N_ELEMENTS(tmpJi_down_lowE_lc ) AND $
                           N_ELEMENTS(tmpJei_down_lowE) AND N_ELEMENTS(tmpJei_down_lowE_lc) AND N_ELEMENTS(tmpJei_down_lowE_lc_ram)) $
              THEN STOP
           END
        ENDCASE

        nTid    = N_ELEMENTS(x)

        sepTime = (curOrb LE 8644) AND (nTid EQ 0)

        ;;Also restore time file if orbit is le 8644
        IF sepTime THEN BEGIN
           tidFilNavn = STRING(FORMAT='(A0,A0,"_",I0,A0)',tidFPref,orbStr,itvl,fSuff)
           IF ~FILE_TEST(dir+tidFilNavn) THEN STOP

           RESTORE,dir+tidFilNavn
           nTid = N_ELEMENTS(x)
           IF nTid NE nHere THEN BEGIN
              ;; PRINT,"Had to fix 'em: ",curOrb,itvl
              ;; hadToFixOrbs    = [[hadToFixOrbs],[curOrb,itvl]]
              ;; alreadyHadToFix = 1
              ;; IF ~KEYWORD_SET(dry_run) THEN STOP ;;No, no case nothing. Just stop and fix it. It seems manually running DOWNGOING_IONS__V1__GET_ION_TIME_SERIES manages to pick up the stuff du ønsker
              
              ;; CASE nTid-nHere OF
              ;;    1: BEGIN
              ;;       x       = x[0:-2]
              ;;    END
              ;;    -1: BEGIN
              ;;       nOffset = 1
              ;;    END
              ;;    ELSE: BEGIN
              ;;       PRINT,"Unequal number of time stamps and data points!"
              ;;       STOP
              ;;    END
              ;; ENDCASE
           ENDIF

           nFixer = N_ELEMENTS(JEi_down_highE_i)
           CASE 1 OF
              N_ELEMENTS(x) GT nFixer: BEGIN
                 IF nFixer EQ 0 THEN BEGIN
                    STOP
                 ENDIF ELSE BEGIN
                    nHere = N_ELEMENTS(tmpJi_down_highE)
                    IF nHere EQ nFixer THEN BEGIN 
                       x     = x[JEi_down_highE_i]
                       mlt   = mlt[JEi_down_highE_i]
                       ilat  = ilat[JEi_down_highE_i]
                       ratio = ratio[JEi_down_highE_i]
                       orbit = orbit[JEi_down_highE_i]
                       alt   = alt[JEi_down_highE_i]

                       nTid  = nFixer
                    ENDIF
                 ENDELSE
              END
              N_ELEMENTS(x) EQ nFixer: BEGIN
              END
              ELSE: BEGIN
                 STOP
              END
           ENDCASE

        ENDIF

        ;;If the data are misaligned
        IF nTid NE nHere THEN BEGIN
           PRINT,"Orbit " + STRCOMPRESS(curOrb) + ": Curse you!"

           CASE 1 OF
              KEYWORD_SET(make_ephem): BEGIN

                 IF nTid LT nHere THEN BEGIN
                    ;; x     = x[JEi_down_highE_i]
                    mlt   = mlt[JEi_down_highE_i]
                    ilat  = ilat[JEi_down_highE_i]
                    ratio = ratio[JEi_down_highE_i]
                    orbit = orbit[JEi_down_highE_i]
                    alt   = alt[JEi_down_highE_i]
                    nHere = N_ELEMENTS(JEi_down_highE_i)
                 ENDIF ELSE BEGIN
                 GET_ALT_MLT_ILAT_FROM_FAST_EPHEM, $
                    curOrb, $
                    x, $
                    OUT_TSORTED_I=tSort_i, $
                    OUT_ALT=alt, $
                    OUT_MLT=mlt, $
                    OUT_ILAT=ilat, $
                    OUT_MAPRATIO=ratio, $
                    OUT_NEVENTS=nHere, $
                    LOGLUN=logLun
                 ENDELSE
                 ;;For some problematic orbits, I ran DOWNGOING_IONS__V1__GET_ION_TIME_SERIES yet again to figure out which inds were missing
                 ;; IF sepTime THEN BEGIN
                 ;;    IF N_ELEMENTS(JEi_down_highE_i) GT 0 THEN BEGIN
                 ;;       IF N_ELEMENTS(x) GT N_ELEMENTS(JEi_down_highE_i) THEN BEGIN
                 ;;          x = x[TEMPORARY(JEi_down_highE_i)]
                 ;;       ENDIF ELSE BEGIN
                 ;;          STOP
                 ;;       ENDELSE
                 ;;    ENDIF
                 ;; ENDIF


                 IF ~KEYWORD_SET(dry_run) THEN BEGIN
                    IF nTid NE nHere THEN STOP
                 ENDIF
                 
                 IF ~alreadyHadToFix THEN hadToFixOrbs = [[hadToFixOrbs],[curOrb,itvl]]

              END
              ELSE: BEGIN
                 ;; PRINT,"More egregious ..."
                 ;;Just see

                 ;; IF ~alreadyHadToFix THEN hadToFixOrbs = [[hadToFixOrbs],[curOrb,itvl]]

                 uniq_i = UNIQ(x,SORT(x))
                 nUniq  = N_ELEMENTS(uniq_i)
                 IF nHere GT nUniq THEN BEGIN ;;There's a chance!
                    CASE 1 OF
                       KEYWORD_SET(make_highE): BEGIN
                          tmpJi_down_highE          = tmpJi_down_highE[uniq_i]         
                          tmpJi_down_highE_lc       = tmpJi_down_highE_lc[uniq_i]
                          tmpJi_down_highE_lc_ram   = tmpJi_down_highE_lc_ram[uniq_i]
                          tmpJei_down_highE         = tmpJei_down_highE[uniq_i]
                          tmpJei_down_highE_lc      = tmpJei_down_highE_lc[uniq_i]
                          tmpJei_down_highE_lc_ram  = tmpJei_down_highE_lc_ram[uniq_i] 
                       END
                       KEYWORD_SET(make_lowE): BEGIN
                          tmpJi_down_lowE           = tmpJi_down_lowE[uniq_i]         
                          tmpJi_down_lowE_lc        = tmpJi_down_lowE_lc[uniq_i]
                          tmpJi_down_lowE_lc_ram    = tmpJi_down_lowE_lc_ram[uniq_i]
                          tmpJei_down_lowE          = tmpJei_down_lowE[uniq_i]
                          tmpJei_down_lowE_lc       = tmpJei_down_lowE_lc[uniq_i]
                          tmpJei_down_lowE_lc_ram   = tmpJei_down_lowE_lc_ram[uniq_i] 
                       END
                       KEYWORD_SET(make_combE): BEGIN
                          tmpJi_down_highE          = tmpJi_down_highE[uniq_i]         
                          tmpJi_down_highE_lc       = tmpJi_down_highE_lc[uniq_i]
                          tmpJi_down_highE_lc_ram   = tmpJi_down_highE_lc_ram[uniq_i]
                          tmpJei_down_highE         = tmpJei_down_highE[uniq_i]
                          tmpJei_down_highE_lc      = tmpJei_down_highE_lc[uniq_i]
                          tmpJei_down_highE_lc_ram  = tmpJei_down_highE_lc_ram[uniq_i] 

                          tmpJi_down_lowE           = tmpJi_down_lowE[uniq_i]         
                          tmpJi_down_lowE_lc        = tmpJi_down_lowE_lc[uniq_i]
                          tmpJi_down_lowE_lc_ram    = tmpJi_down_lowE_lc_ram[uniq_i]
                          tmpJei_down_lowE          = tmpJei_down_lowE[uniq_i]
                          tmpJei_down_lowE_lc       = tmpJei_down_lowE_lc[uniq_i]
                          tmpJei_down_lowE_lc_ram   = tmpJei_down_lowE_lc_ram[uniq_i] 
                       END
                    ENDCASE
                    nHere = nUniq

                 ENDIF ELSE BEGIN

                    PRINT,"Help this brother"
                    IF sepTime THEN BEGIN
                       IF N_ELEMENTS(JEi_down_highE_i) GT 0 THEN BEGIN
                          IF N_ELEMENTS(x) GT N_ELEMENTS(JEi_down_highE_i) THEN BEGIN
                             x = x[TEMPORARY(JEi_down_highE_i)]

                          ENDIF ELSE BEGIN
                             STOP
                          ENDELSE
                       ENDIF ELSE BEGIN
                          STOP
                       END
                    ENDIF

                 ENDELSE

              END
           ENDCASE

           nTid   = N_ELEMENTS(x)
           uniq_i = UNIQ(x,SORT(x))
           nUniq  = N_ELEMENTS(uniq_i)
           IF nTid NE ((nTid AND nHere) AND (nTid AND nUniq)) THEN IF ~KEYWORD_SET(dry_run) THEN BEGIN
              STOP
           ENDIF
        ENDIF

        ;;If we just need to junk some dupes ...
        nUniq = N_ELEMENTS(UNIQ(x,SORT(x)))
        IF nTid NE nUniq THEN BEGIN

           PRINT,FORMAT='("Junking ",I0," inds that lack the unique qualities our team is looking for ...")', $
                 nTid-nUniq

           uniq_i                          = UNIQ(x,SORT(x))
           nHere                           = nUniq
           nTid                            = nUniq
           CASE 1 OF
              KEYWORD_SET(make_ephem): BEGIN
                 mlt                       = mlt[uniq_i]
                 ratio                     = ratio[uniq_i]
                 ilat                      = ilat[uniq_i]
                 alt                       = alt[uniq_i]
                 orbit                     = orbit[uniq_i]
              END
              KEYWORD_SET(make_highE): BEGIN
                 tmpJi_down_highE          = tmpJi_down_highE[uniq_i]         
                 tmpJi_down_highE_lc       = tmpJi_down_highE_lc[uniq_i]
                 tmpJi_down_highE_lc_ram   = tmpJi_down_highE_lc_ram[uniq_i]
                 tmpJei_down_highE         = tmpJei_down_highE[uniq_i]
                 tmpJei_down_highE_lc      = tmpJei_down_highE_lc[uniq_i]
                 tmpJei_down_highE_lc_ram  = tmpJei_down_highE_lc_ram[uniq_i] 
              END
              KEYWORD_SET(make_lowE): BEGIN
                 tmpJi_down_lowE           = tmpJi_down_lowE[uniq_i]         
                 tmpJi_down_lowE_lc        = tmpJi_down_lowE_lc[uniq_i]
                 tmpJi_down_lowE_lc_ram    = tmpJi_down_lowE_lc_ram[uniq_i]
                 tmpJei_down_lowE          = tmpJei_down_lowE[uniq_i]
                 tmpJei_down_lowE_lc       = tmpJei_down_lowE_lc[uniq_i]
                 tmpJei_down_lowE_lc_ram   = tmpJei_down_lowE_lc_ram[uniq_i] 
              END
              KEYWORD_SET(make_combE): BEGIN
                 tmpJi_down_highE          = tmpJi_down_highE[uniq_i]         
                 tmpJi_down_highE_lc       = tmpJi_down_highE_lc[uniq_i]
                 tmpJi_down_highE_lc_ram   = tmpJi_down_highE_lc_ram[uniq_i]
                 tmpJei_down_highE         = tmpJei_down_highE[uniq_i]
                 tmpJei_down_highE_lc      = tmpJei_down_highE_lc[uniq_i]
                 tmpJei_down_highE_lc_ram  = tmpJei_down_highE_lc_ram[uniq_i] 

                 tmpJi_down_lowE           = tmpJi_down_lowE[uniq_i]         
                 tmpJi_down_lowE_lc        = tmpJi_down_lowE_lc[uniq_i]
                 tmpJi_down_lowE_lc_ram    = tmpJi_down_lowE_lc_ram[uniq_i]
                 tmpJei_down_lowE          = tmpJei_down_lowE[uniq_i]
                 tmpJei_down_lowE_lc       = tmpJei_down_lowE_lc[uniq_i]
                 tmpJei_down_lowE_lc_ram   = tmpJei_down_lowE_lc_ram[uniq_i] 
              END
           ENDCASE

        ENDIF
        ;; tmpInds = [(0):(nHere-1-nOffset)]
        tmpInds = [(0):(nHere-1)]
        strInds = tmpInds + totCnt
        
        PRINT,FORMAT='(I0,T10,I0,T20,I0,T30,I0)',orbStr,itvl,nHere,sepTime

        time[strInds]                = (TEMPORARY(x    ))[tmpInds]
        IF ~KEYWORD_SET(dry_run) THEN BEGIN
           CASE 1 OF
              KEYWORD_SET(make_ephem) : BEGIN
                 ;; ephem.x     [strInds]  = (TEMPORARY(x    ))[tmpInds]
                 ephem.orbit [strInds]  = (TEMPORARY(orbit))[tmpInds]
                 ephem.alt   [strInds]  = (TEMPORARY(alt  ))[tmpInds]
                 ephem.mlt   [strInds]  = (TEMPORARY(mlt  ))[tmpInds]
                 ephem.ilat  [strInds]  = (TEMPORARY(ilat ))[tmpInds]
                 ephem.ratio [strInds]  = (TEMPORARY(ratio))[tmpInds]
              END
              KEYWORD_SET(make_highE): BEGIN
                 ion.jei        [strInds] = (TEMPORARY(tmpjei_down_highE       ))[tmpInds]
                 ion.jei_lc     [strInds] = (TEMPORARY(tmpjei_down_highE_lc    ))[tmpInds]
                 ion.jei_lc_ram [strInds] = (TEMPORARY(tmpjei_down_highE_lc_ram))[tmpInds]
                 ion.ji         [strInds] = (TEMPORARY(tmpji_down_highE        ))[tmpInds]
                 ion.ji_lc      [strInds] = (TEMPORARY(tmpji_down_highE_lc     ))[tmpInds]
                 ion.ji_lc_ram  [strInds] = (TEMPORARY(tmpji_down_highE_lc_ram ))[tmpInds]
              END                           
              (make_lowE): BEGIN            
                 ion.jei        [strInds] = (TEMPORARY(tmpjei_down_lowE       ))[tmpInds]
                 ion.jei_lc     [strInds] = (TEMPORARY(tmpjei_down_lowE_lc    ))[tmpInds]
                 ion.jei_lc_ram [strInds] = (TEMPORARY(tmpjei_down_lowE_lc_ram))[tmpInds]
                 ion.ji         [strInds] = (TEMPORARY(tmpji_down_lowE        ))[tmpInds]
                 ion.ji_lc      [strInds] = (TEMPORARY(tmpji_down_lowE_lc     ))[tmpInds]
                 ion.ji_lc_ram  [strInds] = (TEMPORARY(tmpji_down_lowE_lc_ram ))[tmpInds]
              END
              (make_combE): BEGIN            
                 ion.jei        [strInds] = (TEMPORARY(tmpjei_down_highE       ) + TEMPORARY(tmpjei_down_lowE       ))[tmpInds]
                 ion.jei_lc     [strInds] = (TEMPORARY(tmpjei_down_highE_lc    ) + TEMPORARY(tmpjei_down_lowE_lc    ))[tmpInds]
                 ion.jei_lc_ram [strInds] = (TEMPORARY(tmpjei_down_highE_lc_ram) + TEMPORARY(tmpjei_down_lowE_lc_ram))[tmpInds]
                 ion.ji         [strInds] = (TEMPORARY(tmpji_down_highE        ) + TEMPORARY(tmpji_down_lowE        ))[tmpInds]
                 ion.ji_lc      [strInds] = (TEMPORARY(tmpji_down_highE_lc     ) + TEMPORARY(tmpji_down_lowE_lc     ))[tmpInds]
                 ion.ji_lc_ram  [strInds] = (TEMPORARY(tmpji_down_highE_lc_ram ) + TEMPORARY(tmpji_down_lowE_lc_ram ))[tmpInds]
              END
           ENDCASE
        ENDIF

        totCnt += nHere

        ;;Check every 1000
        IF ~(curOrb MOD 1000) THEN IF N_ELEMENTS(UNIQ(time[0:(totCnt-1)])) NE totCnt THEN STOP

        filNavn = STRING(FORMAT='(A0,A0,"_",I0,A0)',fPref,orbStr,++itvl,fSuff)

     ENDWHILE

     IF itvl EQ 0 THEN BEGIN
        PRINT,FORMAT='(A0,": Aucune fil!")',orbStr
     ENDIF

  ENDFOR
  
  ;;Now trim
  finalInds = [0:totCnt-1]

  IF ~KEYWORD_SET(dry_run) THEN BEGIN
     CASE 1 OF
        KEYWORD_SET(make_ephem): BEGIN
           ;; ephem = {x           : ephem.x    [finalInds], $
           ephem = {x           : time       [finalInds], $
                    orbit       : ephem.orbit[finalInds], $
                    alt         : ephem.alt  [finalInds], $
                    mlt         : ephem.mlt  [finalInds], $
                    ilat        : ephem.ilat [finalInds], $
                    ratio       : ephem.ratio[finalInds], $
                    info        : info                    }
        END
        KEYWORD_SET(make_highE) OR KEYWORD_SET(make_lowE) OR KEYWORD_SET(make_combE): BEGIN
           ion   = {jei         : ion.jei        [finalInds], $
                    jei_lc      : ion.jei_lc     [finalInds], $
                    jei_lc_ram  : ion.jei_lc_ram [finalInds], $
                    ji          : ion.ji         [finalInds], $
                    ji_lc       : ion.ji_lc      [finalInds], $
                    ji_lc_ram   : ion.ji_lc_ram  [finalInds], $
                    info        : info                        $
                   }
        END
     ENDCASE
  ENDIF

  time = time[finalInds]

  IF N_ELEMENTS(UNIQ(time,SORT(time))) NE totCnt THEN BEGIN
     PRINT,"So there are dupes. Where?"
     STOP
     this = CGSETDIFFERENCE(LINDGEN(N_ELEMENTS(time)),UNIQ(time,SORT(time)))

     IF KEYWORD_SET(make_ephem) THEN BEGIN
        t
        PRINT,ephem.orbit[this[(uniq(ephem.orbit[this],SORT(ephem.orbit[this])))]]
        ;;Here's the list from 2017/02/09
        ;;It hasn't budged (270 orbits), even after running SDT over all of these orbs yet again. What can it mean?
        ;;  1158    4009    4070    7533    8667    8686    8706    8730    8953    9025
        ;;  9026    9058    9062    9065    9134    9160    9208    9450    9564    9648
        ;;  9668    9673    9688    9698    9718    9725    9738    9739    9740    9750
        ;;  9771    9784    9785    9791    9797    9807    9813    9818    9825    9858
        ;;  9861    9868    9871    9876    9883    9884    9894    9901    9915    9932
        ;;  9933    9943    9973   10010   10021   10044   10066   10085   10114   10142
        ;; 10150   10152   10163   10194   10252   10259   10266   10275   10302   10340
        ;; 10354   10407   10408   10418   10419   10429   10451   10462   10470   10485
        ;; 10488   10513   10533   10547   10549   10550   10574   10600   10707   10721
        ;; 10755   10763   10777   10786   10787   10799   11065   11151   11152   11178
        ;; 11180   11189   11196   11199   11200   11210   11211   11212   11222   11233
        ;; 11234   11244   11247   11255   11266   11267   11272   11276   11277   11285
        ;; 11288   11339   11394   11412   11413   11423   11447   11460   11482   11483
        ;; 11611   11614   11624   11712   11721   11723   11729   11730   11731   11833
        ;; 11865   11868   11931   11932   11954   11975   12008   12011   12040   12047
        ;; 12055   12065   12067   12088   12095   12096   12098   12104   12107   12111
        ;; 12136   12137   12140   12156   12174   12193   12206   12221   12225   12230
        ;; 12274   12281   12295   12306   12323   12333   12337   12348   12393   12434
        ;; 12481   12510   12511   12638   12665   12691   12694   12717   12722   12728
        ;; 12739   12744   12762   12817   12820   12830   12864   12931   12950   12964
        ;; 12966   13007   13010   13020   13027   13062   13064   13101   13107   13131
        ;; 13136   13147   13152   13153   13154   13161   13162   13165   13168   13179
        ;; 13181   13183   13184   13185   13188   13193   13194   13199   13202   13223
        ;; 13228   13247   13248   13257   13267   13268   13269   13271   13323   13326
        ;; 13328   13333   13345   13375   13488   13528   13529   13572   13643   13661
        ;; 13671   13697   13717   13719   13740   13822   13869   13966   14043   14064
        ;; 14086   14119   14184   14270   14286   14287   14298   14331   14341   14342

     ENDIF
  ENDIF


  PRINT,'Saving ' + dbNavn + ' to ' + outNavn + ' ...'
  IF ~KEYWORD_SET(dry_run) THEN BEGIN
     IF ~EXECUTE(execStr) THEN STOP
  ENDIF ELSE BEGIN
     PRINT,"PSYCHE!"
  ENDELSE

  IF N_ELEMENTS(hadToFixOrbs) GT 0 THEN BEGIN
     PRINT,"Had to fix orbs: "
     FOR k=0,N_ELEMENTS(hadToFixOrbs[0,*])-1 DO BEGIN
        PRINT,hadToFixOrbs[0,k],hadToFixOrbs[1,k]
     ENDFOR

       ;;  4070           0
       ;;  7533           1
       ;;  7533           2
       ;;  8667           0
       ;;  8686           0
       ;;  8706           0
       ;;  8730           0
       ;;  8953           0
       ;;  9025           1
       ;;  9026           0
       ;;  9058           0
       ;;  9062           0
       ;;  9065           0
       ;;  9134           0
       ;;  9160           0
       ;;  9208           1
       ;;  9450           0
       ;;  9564           0
       ;;  9564           1
       ;;  9648           0
       ;;  9648           3
       ;;  9648           5
       ;;  9668           1
       ;;  9668           3
       ;;  9673           0
       ;;  9673           3
       ;;  9688           0
       ;;  9698           1
       ;;  9718           0
       ;;  9725           1
       ;;  9738           0
       ;;  9739           0
       ;;  9740           0
       ;;  9750           0
       ;;  9771           0
       ;;  9784           0
       ;;  9785           0
       ;;  9791           0
       ;;  9797           0
       ;;  9807           2
       ;;  9813           0
       ;;  9818           0
       ;;  9825           1
       ;;  9858           0
       ;;  9861           0
       ;;  9868           0
       ;;  9871           0
       ;;  9876           1
       ;;  9883           0
       ;;  9884           0
       ;;  9894           3
       ;;  9901           0
       ;;  9915           2
       ;;  9932           0
       ;;  9933           0
       ;;  9943           0
       ;;  9973           0
       ;; 10010           0
       ;; 10021           0
       ;; 10044           0
       ;; 10066           0
       ;; 10085           0
       ;; 10114           0
       ;; 10114           1
       ;; 10142           0
       ;; 10150           0
       ;; 10152           0
       ;; 10163           0
       ;; 10194           1
       ;; 10252           0
       ;; 10259           0
       ;; 10266           1
       ;; 10275           0
       ;; 10302           0
       ;; 10340           2
       ;; 10354           1
       ;; 10407           0
       ;; 10408           0
       ;; 10418           0
       ;; 10419           0
       ;; 10429           0
       ;; 10451           0
       ;; 10462           0
       ;; 10470           0
       ;; 10485           0
       ;; 10488           0
       ;; 10513           0
       ;; 10533           0
       ;; 10547           0
       ;; 10549           0
       ;; 10550           0
       ;; 10550           1
       ;; 10550           2
       ;; 10574           0
       ;; 10600           0
       ;; 10600           1
       ;; 10600           2
       ;; 10707           0
       ;; 10721           2
       ;; 10755           0
       ;; 10763           0
       ;; 10777           1
       ;; 10777           3
       ;; 10777           4
       ;; 10786           0
       ;; 10787           0
       ;; 10787           1
       ;; 10787           2
       ;; 10787           4
       ;; 10787           6
       ;; 10787           8
       ;; 10787           9
       ;; 10799           0
       ;; 11065           0
       ;; 11151           0
       ;; 11152           0
       ;; 11178           3
       ;; 11178           5
       ;; 11180           0
       ;; 11180           1
       ;; 11189           0
       ;; 11189           1
       ;; 11196           2
       ;; 11199           1
       ;; 11200           1
       ;; 11200           2
       ;; 11210           0
       ;; 11210           2
       ;; 11211           1
       ;; 11211           3
       ;; 11212           0
       ;; 11222           1
       ;; 11233           0
       ;; 11234           3
       ;; 11234           5
       ;; 11244           0
       ;; 11247           0
       ;; 11255           0
       ;; 11266           0
       ;; 11267           0
       ;; 11272           1
       ;; 11276           0
       ;; 11276           1
       ;; 11277           1
       ;; 11285           0
       ;; 11288           1
       ;; 11339           0
       ;; 11394           0
       ;; 11412           0
       ;; 11413           0
       ;; 11423           1
       ;; 11447           0
       ;; 11460           0
       ;; 11482           0
       ;; 11483           0
       ;; 11611           0
       ;; 11614           0
       ;; 11624           0
       ;; 11712           0
       ;; 11721           0
       ;; 11723           0
       ;; 11729           0
       ;; 11730           0
       ;; 11731           0
       ;; 11731           1
       ;; 11731           7
       ;; 11731           8
       ;; 11731           9
       ;; 11833           0
       ;; 11865           0
       ;; 11868           0
       ;; 11931           1
       ;; 11932           0
       ;; 11954           0
       ;; 11975           2
       ;; 12008           0
       ;; 12011           0
       ;; 12040           0
       ;; 12047           0
       ;; 12055           0
       ;; 12065           0
       ;; 12067           0
       ;; 12067           1
       ;; 12067           2
       ;; 12067           3
       ;; 12067           4
       ;; 12088           0
       ;; 12095           0
       ;; 12095           1
       ;; 12096           3
       ;; 12098           0
       ;; 12104           0
       ;; 12107           0
       ;; 12111           0
       ;; 12136           0
       ;; 12137           0
       ;; 12140           0
       ;; 12156           0
       ;; 12174           0
       ;; 12193           0
       ;; 12206           0
       ;; 12221           5
       ;; 12221           6
       ;; 12221           7
       ;; 12221           8
       ;; 12225           0
       ;; 12230           0
       ;; 12274           0
       ;; 12281           0
       ;; 12281           1
       ;; 12295           0
       ;; 12306           0
       ;; 12323           0
       ;; 12333           0
       ;; 12337           0
       ;; 12348           1
       ;; 12393           2
       ;; 12434           0
       ;; 12481           0
       ;; 12510           0
       ;; 12511           1
       ;; 12638           0
       ;; 12665           0
       ;; 12691           0
       ;; 12694           0
       ;; 12717           0
       ;; 12722           0
       ;; 12728           0
       ;; 12739           0
       ;; 12744           0
       ;; 12762           0
       ;; 12817           1
       ;; 12820           0
       ;; 12830           0
       ;; 12864           0
       ;; 12931           0
       ;; 12950           0
       ;; 12964           1
       ;; 12966           0
       ;; 13007           0
       ;; 13010           0
       ;; 13020           0
       ;; 13027           1
       ;; 13062           0
       ;; 13064           0
       ;; 13101           2
       ;; 13107           1
       ;; 13131           0
       ;; 13136           1
       ;; 13147           1
       ;; 13152           0
       ;; 13152           1
       ;; 13152           2
       ;; 13152           4
       ;; 13153           0
       ;; 13154           0
       ;; 13161           0
       ;; 13162           0
       ;; 13162           3
       ;; 13165           0
       ;; 13165           1
       ;; 13168           1
       ;; 13179           2
       ;; 13179           4
       ;; 13181           0
       ;; 13183           1
       ;; 13184           0
       ;; 13185           0
       ;; 13188           1
       ;; 13188           2
       ;; 13188           3
       ;; 13188           4
       ;; 13188           5
       ;; 13193           1
       ;; 13194           0
       ;; 13199           0
       ;; 13202           0
       ;; 13223           0
       ;; 13228           0
       ;; 13247           0
       ;; 13248           0
       ;; 13257           0
       ;; 13267           1
       ;; 13268           0
       ;; 13269           0
       ;; 13271           0
       ;; 13323           0
       ;; 13326           1
       ;; 13328           3
       ;; 13333           0
       ;; 13345           0
       ;; 13375           0
       ;; 13488           2
       ;; 13528           0
       ;; 13528           1
       ;; 13529           1
       ;; 13572          11
       ;; 13643           0
       ;; 13661           3
       ;; 13671           1
       ;; 13697           1
       ;; 13717           0
       ;; 13719           1
       ;; 13740           0
       ;; 13822           1
       ;; 13869           0
       ;; 13966           0
       ;; 13966           1
       ;; 14043           0
       ;; 14064           1
       ;; 14086           1
       ;; 14119           0
       ;; 14184           0
       ;; 14270           0
       ;; 14286           0
       ;; 14287           0
       ;; 14298           0
       ;; 14331           0
       ;; 14341           1
       ;; 14342           1
                                    
  ENDIF

  IF N_ELEMENTS(hadToRedoOrbs) GT 0 THEN BEGIN
     PRINT,"Had to redo orbs: "
     FOR k=0,N_ELEMENTS(hadToRedoOrbs[0,*])-1 DO BEGIN
        PRINT,hadToRedoOrbs[0,k],hadToRedoOrbs[1,k]
     ENDFOR

  ENDIF
  
  STOP
  
END

;; redoOrbs = [638 ,    660 ,    750 ,    935  ,   1212,    1236,    1247,    1267,    1278,    1280, $
;;             1341,    1386,    1421,    1487,    1490,    1503,    1504,    1506,    1507,    1515, $
;;             1522,    1543,    1557,    1558,    1578,    1603,    1622,    1624,    1706,    1711, $
;;             1730,    1736,    1899,    1940,    2190,    2193,    2220,    2236,    2265,    2280, $
;;             2621,    2622,    2664,    2665,    2680,    2689,    2694,    2732,    2775,    2857, $
;;             2874,    2971,    3105,    3150,    3213,    3276,    3377,    3401,    3403,    3458, $
;;             3487,    3611,    3758,    3827,    3854,    3917,    3977,    3987,    4039,    4123, $
;;             4173,    4226,    4236,    4292,    4319,    4339,    4356,    4364,    4366,    4395, $
;;             4473,    4485,    4609,    4704,    5089,    5091,    5154,    5175,    5204,    5245, $
;;             5249,    5317,    5333,    5337,    5338,    5375,    5378,    5430,    5474,    5476, $
;;             5495,    5504,    5546,    5557,    5569,    5594,    5604,    5605,    5660,    5692, $
;;             5707,    5757,    5815,    5829,    5851,    5885,    5917,    5929,    5932,    5947, $
;;             5948,    6067,    6080,    6105,    6114,    6142,    6185,    6188,    6281,    6297, $
;;             6384,    6453,    6512,    6560,    6585,    6829,    6836,    6837,    6839,    6840, $
;;             6847,    6858,    6864,    6898,    7012,    7053,    7180,    7336,    7370,    7432, $
;;             7434,    7439,    7443,    7445,    7455,    7464,    7467,    7534,    7535,    7585, $
;;             7618,    7619,    7622,    7662,    7664,    7714,    7715,    7717,    7718,    7748, $
;;             7750,    7755,    7756,    7757,    7758,    7764,    7810,    7811,    7832,    7833, $
;;             7856,    7857,    7869,    7888,    7904,    7963,    7998,    8016,    8041,    8052, $
;;             8079,    8093,    8104,    8113,    8116,    8124,    8152,    8158,    8168,    8179, $
;;             8201,    8222,    8236,    8256,    8274,    8307,    8315,    8339,    8343,    8386, $
;;             8421,    8488,    8549,    8579,    8582,    8583,    8589,    8629,    8630,    8635]