;2017/01/21
PRO PLOT_ESPEC_JE_JEE_CHARE_ORBSUMMARY__TPLOT,curOrb, $
   NEWELL2009_PANEL=Newell2009_panel, $
   USE_MY_JUNK_AND_BEFSTART=use_my_junk_and_befStart, $
   JUNK_I=junk_i, $
   BEFSTART_I=befStart_i

  COMPILE_OPT IDL2

  @common__newell_espec.pro
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,/LOAD_CHARE, $
                          /DONT_CONVERT_TO_STRICT_NEWELL
  ENDIF

  ;;Some plot things
  junkTransCol  = 100
  junkStartCol  = 160

  junkTransPSym = 1
  junkStartPSym = 2
  TSPSym        = 3

  TSSymSize     = (!D.NAME EQ 'X' ) ? 50.0 : 50.0
  junkSymSize   = 0.9
  NewellSymSize = 0.3
  ;; !P.MULTI      = [0,1,3+KEYWORD_SET(Newell2009_panel),0,0]
  
  IF !D.NAME EQ 'X' THEN BEGIN
     
     WINDOW,0,XSIZE=800,YSIZE=1000

     LOADCT2,39
     oldCharSize = !P.CHARSIZE

     charSize    = 1.0
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
  ;; date_label = LABEL_DATE(DATE_FORMAT='')

  tJul       = UTC_TO_JULDAY(tmpTime)
  tJunk      = UTC_TO_JULDAY(NEWELL__eSpec.x[junk_i])
  tBefStart  = UTC_TO_JULDAY(NEWELL__eSpec.x[befStart_i])
  tRange     = UTC_TO_JULDAY(firstLastT)

  ;;First, Je
  var_name   = 'Je'
  STORE_DATA,var_name,DATA={x:tmpTime,y:NEWELL__eSpec.je[tmp_i]}
  OPTIONS,var_name,'color',TSCol
  OPTIONS,var_name,'psym',TSPSym
  OPTIONS,var_name,'symsize',TSSymSize
  OPTIONS,var_name,'ytitle','Je (#/cm!U2!N-s)'
  OPTIONS,var_name,'ytickv',[1e6,1e7,1e8,1e9,1e10,1e11]
  OPTIONS,var_name,'ytickname',['1e6','1e7','1e8','1e9','1e10','1e11']
  OPTIONS,var_name,'ystyle',1
  YLIM,var_name,1e6,5e11,1
  
  STORE_DATA,var_name+'_junk',DATA={x:NEWELL__eSpec.x[junk_i],y:NEWELL__eSpec.je[junk_i]}
  OPTIONS,var_name+'_junk','symsize',junkSymSize
  OPTIONS,var_name+'_junk','color',junkTransCol

  STORE_DATA,var_name+'_befStart',DATA={x:NEWELL__eSpec.x[befStart_i],y:NEWELL__eSpec.je[befStart_i]}
  OPTIONS,var_name+'_befStart','symsize',junkSymSize
  OPTIONS,var_name+'_befStart','color',junkStartCol

  ;; tmpVarNames = [var_name,var_name+'_junk',var_name+'_befStart']
  tmpVarNames = [var_name]
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars = tmpVarNames ELSE tPlt_vars = [tPlt_vars,tmpVarNames]

  ;;Now Jee
  var_name   = 'Jee'
  STORE_DATA,var_name,DATA={x:tmpTime,y:NEWELL__eSpec.jee[tmp_i]}
  OPTIONS,var_name,'color',TSCol
  OPTIONS,var_name,'psym',TSPSym
  OPTIONS,var_name,'symsize',TSSymSize
  OPTIONS,var_name,'ytitle','Jee (mW/m!U2!N)'
  OPTIONS,var_name,'ytickv',[1e-3,1e-2,1e-1,1e0,1e1,1e2,1e3]
  OPTIONS,var_name,'ytickname',['1e-3','1e-2','1e-1','1e0','1e1','1e2','1e3']
  OPTIONS,var_name,'ystyle',1
  YLIM,var_name,1e-3,3e3,1
  
  STORE_DATA,var_name+'_junk',DATA={x:NEWELL__eSpec.x[junk_i],y:NEWELL__eSpec.jee[junk_i]}
  OPTIONS,var_name+'_junk','symsize',junkSymSize
  OPTIONS,var_name+'_junk','color',junkTransCol

  STORE_DATA,var_name+'_befStart',DATA={x:NEWELL__eSpec.x[befStart_i],y:NEWELL__eSpec.jee[befStart_i]}
  OPTIONS,var_name+'_befStart','symsize',junkSymSize
  OPTIONS,var_name+'_befStart','color',junkStartCol

  ;; tmpVarNames = [var_name,var_name+'_junk',var_name+'_befStart']
  tmpVarNames = [var_name]
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars = tmpVarNames ELSE tPlt_vars = [tPlt_vars,tmpVarNames]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Now chare
  var_name   = 'charE'
  STORE_DATA,var_name,DATA={x:tmpTime,y:NEWELL__eSpec.charE[tmp_i]}
  OPTIONS,var_name,'color',TSCol
  OPTIONS,var_name,'psym',TSPSym
  OPTIONS,var_name,'symsize',TSSymSize
  OPTIONS,var_name,'ytitle','Char E (eV)'
  OPTIONS,var_name,'ytickv',[1e1,1e2,1e3,1e4]
  OPTIONS,var_name,'ytickname',['1e1','1e2','1e3','1e4']
  OPTIONS,var_name,'ystyle',1
  YLIM,var_name,4,3e4,1
  
  STORE_DATA,var_name+'_junk',DATA={x:NEWELL__eSpec.x[junk_i],y:NEWELL__eSpec.charE[junk_i]}
  OPTIONS,var_name+'_junk','symsize',junkSymSize
  OPTIONS,var_name+'_junk','color',junkTransCol

  STORE_DATA,var_name+'_befStart',DATA={x:NEWELL__eSpec.x[befStart_i],y:NEWELL__eSpec.charE[befStart_i]}
  OPTIONS,var_name+'_befStart','symsize',junkSymSize
  OPTIONS,var_name+'_befStart','color',junkStartCol

  ;; tmpVarNames = [var_name,var_name+'_junk',var_name+'_befStart']
  tmpVarNames = [var_name]
  IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars = tmpVarNames ELSE tPlt_vars = [tPlt_vars,tmpVarNames]

  IF KEYWORD_SET(Newell2009_panel) THEN BEGIN


     NewellStruct = {x       : NEWELL__eSpec.x[tmp_i], $
                     mono    : NEWELL__eSpec.mono[tmp_i], $
                     broad   : NEWELL__eSpec.broad[tmp_i], $
                     diffuse : NEWELL__eSpec.diffuse[tmp_i]}

     var_name = 'newellPanel_2009'
     PREPARE_IDENTIFIED_DIFF_EFLUXES_FOR_TPLOT,NewellStruct, $
                                               TPLOT_NAME=var_name, $
                                               /NO_STRICT_TYPES, $
                                               /CONVERT_TO_NEWELL_INTERP, $
                                               SYMSIZE=NewellSymSize, $
                                               YTITLE='2009 Interp'

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars = [var_name] ELSE tPlt_vars = [tPlt_vars,var_name]

     var_name = 'newellPanel'
     PREPARE_IDENTIFIED_DIFF_EFLUXES_FOR_TPLOT,NewellStruct, $
                                               TPLOT_NAME=var_name, $
                                               /FAVOR_BROADSTRICT_OVER_MONO, $
                                               /NO_STRICT_TYPES, $
                                               SYMSIZE=NewellSymSize, $
                                               YTITLE='Favor broadStrict'

     IF (N_ELEMENTS(tPlt_vars) EQ 0) THEN tPlt_vars = [var_name] ELSE tPlt_vars = [tPlt_vars,var_name]

  ENDIF

  GET_FA_ORBIT,firstLastT[0],firstLastT[1]

  TPLOT,tPlt_vars,VAR=['ALT','ILAT','MLT'], $
        TRANGE=firstLastT, $
        TITLE='Orbit ' + orbString + ' (' + TIME_TO_STR(tmpTime[0],/MS) + ')'


  TPLOT_PANEL,VARIABLE='Je',OPLOTVAR='Je_junk',PSYM=junkTransPSym
  TPLOT_PANEL,VARIABLE='Je',OPLOTVAR='Je_befStart',PSYM=junkStartPSym

  TPLOT_PANEL,VARIABLE='Jee',OPLOTVAR='Jee_junk',PSYM=junkTransPSym
  TPLOT_PANEL,VARIABLE='Jee',OPLOTVAR='Jee_befStart',PSYM=junkStartPSym

  TPLOT_PANEL,VARIABLE='charE',OPLOTVAR='charE_junk',PSYM=junkTransPSym
  TPLOT_PANEL,VARIABLE='charE',OPLOTVAR='charE_befStart',PSYM=junkStartPSym

  IF !D.NAME EQ 'X' THEN BEGIN
     !P.CHARSIZE = oldCharSize
  ENDIF

END
