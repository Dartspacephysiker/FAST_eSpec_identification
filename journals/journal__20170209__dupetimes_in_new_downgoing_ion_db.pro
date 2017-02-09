;;02/09/17
;;Output from the end of JOURNAL__20170130__CHECK_OUT_NEW_DOWNGOING_ION_DB suggests that we've got dupes
PRO JOURNAL__20170209__DUPETIMES_IN_NEW_DOWNGOING_ION_DB

  COMPILE_OPT IDL2

  
  dir          = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/downgoing_ions__v1_output/'
  fPref        = 'downgoing_ions__v1--orbit_'
  tidFPref     = 'downgoing_ions__v1__time--orbit_'
  fSuff        = '.sav'

  outNavn      = 'downgoing_ions__'
  outDir       = '/SPENCEdata/Research/database/FAST/dartdb/ion_db/'

  startOrb  = 500
  ;; startOrb     = 4009
  stopOrb      = 14361 ;;as of 2017/02/08 this is all you had

  maxNElem     = 30000000

  time         = MAKE_ARRAY(maxNElem,/DOUBLE)

  totCnt       = 0L
  nLastHere    = -1L
  bogeyCnt     = 0L
  bogeyFiles   = MAKE_ARRAY(10000,/STRING)
  BogeyOrbItvl = MAKE_ARRAY(10000,2,VALUE=-1,/LONG)
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
              STOP              ;No, no case nothing. Just stop and fix it.
                                ;It seems manually running DOWNGOING_IONS__V1__GET_ION_TIME_SERIES maanages to pick up the stuff du ønsker
           ENDIF
        ENDIF

        IF nTid NE N_ELEMENTS(UNIQ(x,SORT(x))) THEN STOP

        tmpInds = [(0):(nHere-1)]
        strInds = tmpInds + totCnt
        
        ;; PRINT,FORMAT='(I0,T10,I0,T20,I0,T30,I0)',orbStr,itvl,nHere,sepTime

        time[strInds]                = (TEMPORARY(x    ))[tmpInds]

        IF ARRAY_EQUAL(time[(strInds-nLastHere)],time[strInds]) THEN BEGIN
           PRINT,"Bogey: Orb-itvl ",orbStr,itvl
           bogeyFiles[bogeyCnt]       = dir+filNavn
           BogeyOrbItvl[bogeyCnt++,*] = [orbStr,itvl]
        ENDIF

        tmpjei_down_highE         = !NULL 
        tmpjei_down_highE_lc      = !NULL 
        tmpjei_down_highE_lc_ram  = !NULL
        tmpji_down_highE          = !NULL 
        tmpji_down_highE_lc       = !NULL 
        tmpji_down_highE_lc_ram   = !NULL 
        tmpjei_down_lowE          = !NULL
        tmpjei_down_lowE_lc       = !NULL
        tmpjei_down_lowE_lc_ram   = !NULL
        tmpji_down_lowE           = !NULL
        tmpji_down_lowE_lc        = !NULL
        tmpji_down_lowE_lc_ram    = !NULL
        orbit                     = !NULL
        alt                       = !NULL
        mlt                       = !NULL
        ilat                      = !NULL
        ratio                     = !NULL

        nLastHere = nHere
        totCnt   += nHere
        filNavn   = STRING(FORMAT='(A0,A0,"_",I0,A0)',fPref,orbStr,++itvl,fSuff)

     ENDWHILE

     IF itvl EQ 0 THEN BEGIN
        PRINT,FORMAT='(A0,": Aucune fil!")',orbStr
     ENDIF

  ENDFOR

  IF bogeyCnt GT 0 THEN BEGIN
     PRINT,"I recommend deleting these:"
     FOR k=0,bogeyCnt-1 DO PRINT,bogeyFiles[k]
     STOP
  ENDIF


END
