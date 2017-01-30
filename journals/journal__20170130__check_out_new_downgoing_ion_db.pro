;;2017/01/30 
;;2017/01/30 Man, I forgot to include timestamps in the first 8644 files!!
PRO JOURNAL__20170130__CHECK_OUT_NEW_DOWNGOING_ION_DB

  COMPILE_OPT IDL2

  make_ephem  = 1
  make_highE  = 0
  make_lowE   = 0
  
  dir         = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/downgoing_ions__v1_output/'
  fPref       = 'downgoing_ions__v1--orbit_'
  fSuff       = '.sav'

  outNavn     = 'downgoing_ions__'
  outDir      = '/SPENCEdata/Research/database/FAST/dartdb/ion_db/'

  ;; startOrb    = 500
  startOrb    = 8648
  stopOrb     = 8658

  options  = ['ORBIT','ALT','MLT','ILAT','RATIO',                                   $
             'ISPEC_DOWN_LC_RAM',                                                   $
             'TMPJEI_DOWN_HIGHE','TMPJEI_DOWN_HIGHE_LC','TMPJEI_DOWN_HIGHE_LC_RAM', $
             'TMPJI_DOWN_HIGHE' ,'TMPJI_DOWN_HIGHE_LC' ,'TMPJI_DOWN_HIGHE_LC_RAM' , $
             'TMPJEI_DOWN_LOWE' ,'TMPJEI_DOWN_LOWE_LC' ,'TMPJEI_DOWN_LOWE_LC_RAM' , $
             'TMPJI_DOWN_LOWE'  ,'TMPJI_DOWN_LOWE_LC'  ,'TMPJI_DOWN_LOWE_LC_RAM']

  maxNElem = 30000000

  CASE 1 OF
     KEYWORD_SET(make_ephem) : BEGIN
        ephem = {x           : MAKE_ARRAY(maxNElem,/DOUBLE), $
                 orbit       : MAKE_ARRAY(maxNElem,/UINT  ), $
                 alt         : MAKE_ARRAY(maxNElem,/FLOAT ), $
                 mlt         : MAKE_ARRAY(maxNElem,/FLOAT ), $
                 ilat        : MAKE_ARRAY(maxNElem,/FLOAT ), $
                 ratio       : MAKE_ARRAY(maxNElem,/FLOAT )}
        dbNavn = 'ephem'
        outNavnSuff = 'ephem'

        membres  = ['ORBIT','ALT','MLT','ILAT','RATIO']
        DB_version = '0.1'
        DB_extras  = 'mapRatio'
     END
     KEYWORD_SET(make_highE) OR KEYWORD_SET(make_lowE): BEGIN
        ion   = {jei         : MAKE_ARRAY(maxNElem,/FLOAT), $
                 jei_lc      : MAKE_ARRAY(maxNElem,/FLOAT), $
                 jei_lc_ram  : MAKE_ARRAY(maxNElem,/FLOAT), $
                 ji          : MAKE_ARRAY(maxNElem,/FLOAT), $
                 ji_lc       : MAKE_ARRAY(maxNElem,/FLOAT), $
                 ji_lc_ram   : MAKE_ARRAY(maxNElem,/FLOAT)}

        dbNavn     = 'ion'
        DB_version = '0.1'
        DB_extras  = 'mapRatio'

        CASE 1 OF
           KEYWORD_SET(make_highE): BEGIN
              outNavnSuff = 'highE'

              membres     = ['TMPJEI_DOWN_HIGHE','TMPJEI_DOWN_HIGHE_LC','TMPJEI_DOWN_HIGHE_LC_RAM', $
                          'TMPJI_DOWN_HIGHE' ,'TMPJI_DOWN_HIGHE_LC' ,'TMPJI_DOWN_HIGHE_LC_RAM'   ]
              DB_version += '_highE'
              eRange      = [300,2.4e4]
           END
           KEYWORD_SET(make_lowE): BEGIN
              outNavnSuff = 'lowE'
              membres  = ['TMPJEI_DOWN_LOWE','TMPJEI_DOWN_LOWE_LC','TMPJEI_DOWN_LOWE_LC_RAM', $
                          'TMPJI_DOWN_LOWE' ,'TMPJI_DOWN_LOWE_LC' ,'TMPJI_DOWN_LOWE_LC_RAM'   ]
              DB_version += '_lowE'
              eRange      = [0,300]
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

     itvl = 0
     orbStr = STRING(FORMAT='(I0)',curOrb)

     filNavn = STRING(FORMAT='(A0,A0,"_",I0,A0)',fPref,orbStr,itvl,fSuff)
     WHILE FILE_TEST(dir+filNavn) DO BEGIN

        RESTORE,dir+filNavn

        nHere   = N_ELEMENTS(mlt)
        tmpInds = [(totCnt):(totCnt+nHere-1)]

        PRINT,FORMAT='(I0,T10,I0,T20,I0)',orbStr,itvl,nHere

        CASE 1 OF
           KEYWORD_SET(make_ephem) : BEGIN
              ;; IF nHere NE N_ELEMENTS(
              ephem.x     [tmpInds]  = TEMPORARY(x    )
              ephem.orbit [tmpInds]  = TEMPORARY(orbit)
              ephem.alt   [tmpInds]  = TEMPORARY(alt  )
              ephem.mlt   [tmpInds]  = TEMPORARY(mlt  )
              ephem.ilat  [tmpInds]  = TEMPORARY(ilat )
              ephem.ratio [tmpInds]  = TEMPORARY(ratio)
           END
           KEYWORD_SET(make_highE): BEGIN
              ion.jei        [tmpInds] = TEMPORARY(tmpjei_down_highE        )
              ion.jei_lc     [tmpInds] = TEMPORARY(tmpjei_down_highE_lc     )
              ion.jei_lc_ram [tmpInds] = TEMPORARY(tmpjei_down_highE_lc_ram )
              ion.ji         [tmpInds] = TEMPORARY(tmpji_down_highE         )
              ion.ji_lc      [tmpInds] = TEMPORARY(tmpji_down_highE_lc      )
              ion.ji_lc_ram  [tmpInds] = TEMPORARY(tmpji_down_highE_lc_ram  )
           END
           (make_lowE): BEGIN
              ion.jei        [tmpInds] = TEMPORARY(tmpjei_down_lowE        )
              ion.jei_lc     [tmpInds] = TEMPORARY(tmpjei_down_lowE_lc     )
              ion.jei_lc_ram [tmpInds] = TEMPORARY(tmpjei_down_lowE_lc_ram )
              ion.ji         [tmpInds] = TEMPORARY(tmpji_down_lowE         )
              ion.ji_lc      [tmpInds] = TEMPORARY(tmpji_down_lowE_lc      )
              ion.ji_lc_ram  [tmpInds] = TEMPORARY(tmpji_down_lowE_lc_ram  )
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
  finalInds = [0:totCnt]
  CASE 1 OF
     KEYWORD_SET(make_ephem): BEGIN
        ephem = {x           : ephem.x    [finalInds], $
                 orbit       : ephem.orbit[finalInds], $
                 alt         : ephem.alt  [finalInds], $
                 mlt         : ephem.mlt  [finalInds], $
                 ilat        : ephem.ilat [finalInds], $
                 ratio       : ephem.ratio[finalInds]  }
     END
     KEYWORD_SET(make_highE) OR KEYWORD_SET(make_lowE): BEGIN
        ion   = {jei         : ion.jei        [finalInds], $
                 jei_lc      : ion.jei_lc     [finalInds], $
                 jei_lc_ram  : ion.jei_lc_ram [finalInds], $
                 ji          : ion.ji         [finalInds], $
                 ji_lc       : ion.ji_lc      [finalInds], $
                 ji_lc_ram   : ion.ji_lc_ram  [finalInds], $
                 info:{DB_date    : GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                       DB_version : DB_version, $
                       DB_extras  : DB_extras , $
                       eRange     : eRange    }            $
                }
     END
  ENDCASE

  PRINT,'Saving ' + dbNavn + ' to ' + outNavn + ' ...'
  IF ~EXECUTE(execStr) THEN STOP

END
