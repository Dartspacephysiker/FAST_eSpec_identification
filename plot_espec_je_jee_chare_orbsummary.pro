;2017/01/21
PRO PLOT_ESPEC_JE_JEE_CHARE_ORBSUMMARY,curOrb, $
                                       NEWELL2009_PANEL=Newell2009_panel, $
                                       USE_MY_JUNK_AND_BEFSTART=use_my_junk_and_befStart, $
                                       JUNK_I=junk_i, $
                                       BEFSTART_I=befStart_i

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__newell_espec.pro
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,/LOAD_CHARE
  ENDIF

  ;;Some plot things
  junkTransCol  = 100
  junkStartCol  = 160

  junkTransPSym = 1
  junkStartPSym = 2
  TSPSym        = 3

  TSSymSize     = 50.0

  !P.MULTI      = [0,1,3+KEYWORD_SET(Newell2009_panel),0,0]
  
  IF !D.NAME EQ 'X' THEN BEGIN
     oldCharSize = !P.CHARSIZE

     charSize    = 2.5
     !P.CHARSIZE = charSize
  ENDIF

  ;;For getting the bad boys
  IF KEYWORD_SET(use_my_junk_and_befStart) THEN BEGIN
     IF N_ELEMENTS(junk_i) EQ 0 OR N_ELEMENTS(befStart_i) EQ 0 THEN BEGIN
        PRINT,"You didn't give a {junk,befStart}_i to work with!"
        RETURN
     ENDIF
  ENDIF

  IF ~KEYWORD_SET(use_my_junk_and_befStart) THEN BEGIN
     NewellDBDir   = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
     DBString      = GET_NEWELL_DB_STRING(NEWELL__eSpec)+'--killedGap_inds--'

     killGap_file  = GET_NEWELL_ESPEC_KILLGAP_FILE(NEWELL__eSpec, $
                                                   NEWELLDBDIR=NewellDBDir, $
                                                   /STOP_IF_NOEXIST)

     RESTORE,killGap_file
  ENDIF
  
  ;;Ind things
  orbString = STRCOMPRESS(curOrb,/REMOVE_ALL)
  tmp_i     = WHERE(NEWELL__eSpec.orbit EQ curOrb[0],NBef)
  firstLastT = NEWELL__eSpec.x[[tmp_i[0],tmp_i[-1]]]

  IF NBef EQ 0 THEN BEGIN
     PRINT,"no inds for orbit " + STRCOMPRESS(curOrb,/REMOVE_ALL) + ". Move it on down."

     RETURN
  ENDIF
  
  tmp_i     = CGSETDIFFERENCE(tmp_i,junk_i,COUNT=nAftJunk)
  IF nAftJunk EQ 0 THEN STOP

  tmp_i     = CGSETDIFFERENCE(tmp_i,befStart_i,COUNT=nAftBefTimes)
  tmpTime   = NEWELL__eSpec.x[tmp_i]
  IF nAftBefTimes EQ 0 THEN STOP

  ;;For the first two paneaux
  date_label = LABEL_DATE(DATE_FORMAT='')

  tJul       = UTC_TO_JULDAY(tmpTime)
  tJunk      = UTC_TO_JULDAY(NEWELL__eSpec.x[junk_i])
  tBefStart  = UTC_TO_JULDAY(NEWELL__eSpec.x[befStart_i])
  tRange     = UTC_TO_JULDAY(firstLastT)

  PLOT,tJul,NEWELL__eSpec.je[tmp_i], $
       /NODATA, $
       CHARSIZE=charSize, $
       TITLE='Orbit ' + orbString + ' (' + TIME_TO_STR(tmpTime[0],/MS) + ')', $
       XTICKFORMAT='(A1)', $
       XTICKUNITS='Time', $
       XRANGE=tRange, $
       /YLOG, $
       YRANGE=[1e6,5e11], $
       YTITLE='Je (#/cm!U2!N-s)', $
       YTICKV=[1e6,1e7,1e8,1e9,1e10,1e11], $
       YTICKNAME=['1e6','1e7','1e8','1e9','1e10','1e11'], $
       YSTYLE=1, $
       YMARGIN=[0,2]

  OPLOT,tJul,NEWELL__eSpec.je[tmp_i], $
        PSYM=TSPsym, $
        COLOR=TSCol, $
        SYMSIZE=TSSymSize

  OPLOT,tJunk,NEWELL__eSpec.je[junk_i], $
        PSYM=junkTransPSym, $
        COLOR=junkTransCol

  OPLOT,tBefStart,NEWELL__eSpec.je[befStart_i], $
        PSYM=JunkStartPSym, $
        COLOR=junkStartCol

  PLOT,tJul,NEWELL__eSpec.jee[tmp_i], $
       /NODATA, $
       CHARSIZE=charSize, $
       XTICKFORMAT='(A1)', $
       XTICKUNITS='Time', $
       XRANGE=tRange, $
       /YLOG, $
       YRANGE=[1e-3,3e3], $
       YTITLE='Jee (mW/m!U2!N)', $
       YTICKV=[1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3], $
       YTICKNAME=['1e-3','1e-2','1e-1','1e0','1e1','1e2','1e3'], $
       YSTYLE=1, $
       YMARGIN=[0,1]

  OPLOT,tJul,NEWELL__eSpec.jee[tmp_i], $
        PSYM=TSPsym, $
        COLOR=TSCol, $
        SYMSIZE=TSSymSize

  OPLOT,tJunk,NEWELL__eSpec.jee[junk_i], $
        PSYM=junkTransPSym, $
        COLOR=junkTransCol

  OPLOT,tBefStart,NEWELL__eSpec.jee[befStart_i], $
        PSYM=JunkStartPSym, $
        COLOR=junkStartCol

  ;;Now chare, with dates
  date_label = LABEL_DATE(DATE_FORMAT=['%I:%S','%D-%H'])

  PLOT,tJul,NEWELL__eSpec.charE[tmp_i], $
       /NODATA, $
       CHARSIZE=charSize, $
       XTICKFORMAT='LABEL_DATE', $
       XTICKUNITS=['Time','Time'], $
       XRANGE=tRange, $
       /YLOG, $
       YRANGE=[4,3e4], $
       YTITLE='Char E (eV)', $
       YTICKV=[1e1,1e2,1e3,1e4], $
       YTICKNAME=['1e1','1e2','1e3','1e4'], $
       YMARGIN=[6,1], $
       XMARGIN=[10,3], $
       YSTYLE=1

  OPLOT,tJul,NEWELL__eSpec.charE[tmp_i], $
        PSYM=TSPsym, $
        COLOR=TSCol, $
        SYMSIZE=TSSymSize

  OPLOT,tJunk,NEWELL__eSpec.charE[junk_i], $
        PSYM=junkTransPSym, $
        COLOR=junkTransCol

  OPLOT,tBefStart,NEWELL__eSpec.charE[befStart_i], $
        PSYM=JunkStartPSym, $
        COLOR=junkStartCol

  IF KEYWORD_SET(Newell2009_panel) THEN BEGIN

     ;; IF ~KEYWORD_SET(no_stri

     ;; IF KEYWORD_SET(no_strict_types) THEN BEGIN
     ;;    GET_ESPEC_INDS_BY_TYPE,NEWELL__eSpec,m_i,b_i,d_i, $
     ;;                           USER_INDS=tmp_i

     ;;    names = ['Mono','Broad','Diffuse' ]
     ;;    add_str_element,plotstuff ,'ytickname',names
     ;;    add_str_element,oplotstuff,'ytickname',names
     ;;    add_str_element,plotstuff,'yticks',2,/REPLACE
     ;;    add_str_element,oplotstuff,'yticks',2,/REPLACE
     ;;    add_str_element,plotstuff,'ytickv',[1,3,5],/REPLACE
     ;;    add_str_element,oplotstuff,'ytickv',[1,3,5],/REPLACE
     ;;    col  =   [ 100      ,250   ,!p.color]
     ;; ENDIF ELSE BEGIN
     ;;    GET_ESPEC_INDS_BY_TYPE,NEWELL__eSpec,m_i,b_i,d_i, $
     ;;                           USER_INDS=tmp_i, $
     ;;                           ,/ONLY_NONSTRICT

     ;;    GET_ESPEC_INDS_BY_TYPE,NEWELL__eSpec,mS_i,bS_i,d_i, $
     ;;                           USER_INDS=tmp_i, $
     ;;                           ,/ONLY_STRICT

     ;;    names = ['Mono','Strict Mono','Broad','Strict Broad','Diffuse' ]
     ;;    add_str_element,plotstuff ,'ytickname',names
     ;;    add_str_element,oplotstuff,'ytickname',names
     ;;    add_str_element,plotstuff,'yticks',4,/REPLACE
     ;;    add_str_element,oplotstuff,'yticks',4,/REPLACE
     ;;    add_str_element,plotstuff,'ytickv',[1,2,3,4,5],/REPLACE
     ;;    add_str_element,oplotstuff,'ytickv',[1,2,3,4,5],/REPLACE
     ;;    col  =   [ 100      ,120   ,250   ,30    ,!p.color]
     ;; ENDELSE

     PLOT_SPECTRAL_TYPE__NEWELL_ET_AL_2009__TPLOT,DATA={x       : NEWELL__eSpec.x[tmp_i], $
                                                        mono    : NEWELL__eSpec.mono[tmp_i], $
                                                        broad   : NEWELL__eSpec.broad[tmp_i], $
                                                        diffuse : NEWELL__eSpec.diffuse[tmp_i]}

  ;; PLOT,tJul,NEWELL__eSpec.charE[tmp_i], $
  ;;      /NODATA, $
  ;;      CHARSIZE=charSize, $
  ;;      XTICKFORMAT='LABEL_DATE', $
  ;;      XTICKUNITS=['Time','Time'], $
  ;;      XRANGE=tRange, $
  ;;      /YLOG, $
  ;;      YRANGE=[4,3e4], $
  ;;      YTITLE='Char E (eV)', $
  ;;      YTICKV=[1e1,1e2,1e3,1e4], $
  ;;      YTICKNAME=['1e1','1e2','1e3','1e4'], $
  ;;      YMARGIN=[6,1], $
  ;;      XMARGIN=[10,3], $
  ;;      YSTYLE=1

  ;; OPLOT,tJul,NEWELL__eSpec.charE[tmp_i], $
  ;;       PSYM=TSPsym, $
  ;;       COLOR=TSCol, $
  ;;       SYMSIZE=TSSymSize

  ENDIF

  IF !D.NAME EQ 'X' THEN BEGIN
     !P.CHARSIZE = oldCharSize
  ENDIF

END
