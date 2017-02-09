;;2017/01/30 
;;2017/01/30 Man, I forgot to include timestamps in the first 8644 files!!
;;2017/02/01 For the first 8644 orbits, we also open up the time file and tack that on
PRO JOURNAL__20170130__CHECK_OUT_NEW_DOWNGOING_ION_DB

  COMPILE_OPT IDL2

  make_ephem  = 1
  make_highE  = 0
  make_lowE   = 0
  make_combE  = 0
  
  dir         = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/downgoing_ions__v1_output/'
  fPref       = 'downgoing_ions__v1--orbit_'
  tidFPref    = 'downgoing_ions__v1__time--orbit_'
  fSuff       = '.sav'

  outNavn     = 'downgoing_ions__'
  outDir      = '/SPENCEdata/Research/database/FAST/dartdb/ion_db/'

  ;; startOrb    = 500
  startOrb    = 4009
  stopOrb     = 14361 ;;as of 2017/02/08 this is all you had

  options  = ['ORBIT','ALT','MLT','ILAT','RATIO',                                   $
             'ISPEC_DOWN_LC_RAM',                                                   $
             'TMPJEI_DOWN_HIGHE','TMPJEI_DOWN_HIGHE_LC','TMPJEI_DOWN_HIGHE_LC_RAM', $
             'TMPJI_DOWN_HIGHE' ,'TMPJI_DOWN_HIGHE_LC' ,'TMPJI_DOWN_HIGHE_LC_RAM' , $
             'TMPJEI_DOWN_LOWE' ,'TMPJEI_DOWN_LOWE_LC' ,'TMPJEI_DOWN_LOWE_LC_RAM' , $
             'TMPJI_DOWN_LOWE'  ,'TMPJI_DOWN_LOWE_LC'  ,'TMPJI_DOWN_LOWE_LC_RAM']

  maxNElem = 30000000

  DB_version = '0.1'
  DB_extras  = 'mapRatio'
  orig_rtine = 'JOURNAL__20170130__CHECK_OUT_NEW_DOWNGOING_ION_DB'
  ;;Info struct
  info = {DB_date              : GET_TODAY_STRING(/DO_YYYYMMDD_FMT) , $
          DB_version           : DB_version                         , $
          DB_extras            : DB_extras                          , $
          ;; eRange               : eRange                             , $
          originating_routine  : orig_rtine                         }
  CASE 1 OF
     KEYWORD_SET(make_ephem) : BEGIN
        time                   = MAKE_ARRAY(maxNElem,/DOUBLE)
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
  FOR curOrb=startOrb,stopOrb DO BEGIN

     ;; nOffset    = 0
     itvl       = 0
     orbStr     = STRING(FORMAT='(I0)',curOrb)

     filNavn    = STRING(FORMAT='(A0,A0,"_",I0,A0)',fPref,orbStr,itvl,fSuff)
     WHILE FILE_TEST(dir+filNavn) DO BEGIN

        RESTORE,dir+filNavn

        nHere   = N_ELEMENTS(mlt)

        nTid    = N_ELEMENTS(x)

        sepTime = (curOrb LE 8644) AND (nTid EQ 0)

        ;;Also restore time file if orbit is le 8644
        IF sepTime THEN BEGIN
           tidFilNavn = STRING(FORMAT='(A0,A0,"_",I0,A0)',tidFPref,orbStr,itvl,fSuff)
           IF ~FILE_TEST(dir+tidFilNavn) THEN STOP

           RESTORE,dir+tidFilNavn
           nTid = N_ELEMENTS(x)
           IF nTid NE nHere THEN BEGIN
              STOP ;;No, no case nothing. Just stop and fix it. It seems manually running DOWNGOING_IONS__V1__GET_ION_TIME_SERIES maanages to pick up the stuff du ønsker
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
        ENDIF

        IF nTid NE N_ELEMENTS(UNIQ(x,SORT(x))) THEN STOP

        ;; tmpInds = [(0):(nHere-1-nOffset)]
        tmpInds = [(0):(nHere-1)]
        strInds = tmpInds + totCnt
        
        PRINT,FORMAT='(I0,T10,I0,T20,I0,T30,I0)',orbStr,itvl,nHere,sepTime

        time[strInds]                = (TEMPORARY(x    ))[tmpInds]
        CASE 1 OF
           KEYWORD_SET(make_ephem) : BEGIN
              ;; IF nHere NE N_ELEMENTS(
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

        totCnt += nHere
        filNavn = STRING(FORMAT='(A0,A0,"_",I0,A0)',fPref,orbStr,++itvl,fSuff)

     ENDWHILE

     IF itvl EQ 0 THEN BEGIN
        PRINT,FORMAT='(A0,": Aucune fil!")',orbStr
     ENDIF

  ENDFOR
  
  ;;Now trim
  finalInds = [0:totCnt-1]

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

  time = time[finalInds]
  IF N_ELEMENTS(UNIQ(time,SORT(time))) NE totCnt THEN BEGIN
     PRINT,"So there are dupes. Where?"
     STOP
     this = CGSETDIFFERENCE(LINDGEN(N_ELEMENTS(time)),UNIQ(time,SORT(time)))

     IF KEYWORD_SET(make_ephem) THEN BEGIN
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
  IF ~EXECUTE(execStr) THEN STOP

END
