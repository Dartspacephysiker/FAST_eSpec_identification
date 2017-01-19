;;01/19/17
PRO PLOT_ESPEC_SAMPLE_RATES_AND_TRANSITION_TIMES,curOrb, $
   tmpTime, $
   JUNK_II=junk_ii, $
   BEFSTART_II=befStart_ii, $
   WUNSAFE_II=wUnsafe_ii, $
   W251_II=w251_ii, $
   W0628_II=w0628_ii, $
   W0312_II=w0312_ii ;, $
   ;; WIN_INDEX=win_index

  COMPILE_OPT IDL2

  junkTransVal = 0
  junkStartVal = 1
  TSVal        = 2
  Val0312      = 3
  Val0628      = 4
  Val251       = 5
  ValUnsafe    = 6

  junkTransCol = 25
  junkStartCol = 50
  TSCol        = 75
  Col0312      = 100
  Col0628      = 125
  Col251       = 150
  ColUnsafe    = 175

  junkTransPSym = 1
  junkStartPSym = 2
  TSPSym        = 4
  PSym0312      = 5
  PSym0628      = 6
  PSym251       = 1
  PSymUnsafe    = 2

  orbString     = STRCOMPRESS(curOrb,/REMOVE_ALL)

  date_label    = LABEL_DATE(DATE_FORMAT = ['%I:%S','%D-%H'])
  tJul          = UTC_TO_JULDAY(tmpTime)

  PLOT,tJul,MAKE_ARRAY(N_ELEMENTS(tJul),VALUE=TSVal), $
       TITLE='Orbit ' + orbString + ' (' + TIME_TO_STR(tmpTime[0],/MS) + ')', $
       /NODATA, $
       XTICKFORMAT='LABEL_DATE', $
       XTICKUNITS=['Time','Time'], $
       XMARGIN=[12,2], $
       YRANGE=[-0.1,6.1], $
       YSTYLE=1, $
       YTICKV=[0,1,2,3,4,5,6], $
       YTICKNAME=["SRate Trans","Interv Start","T Series",'dt=0.312','dt=0.628','dt=2.51','dt=unsafe'], $
       YMARGIN=[8,2]

  OPLOT,tJul,MAKE_ARRAY(N_ELEMENTS(tJul),VALUE=TSVal), $
        PSYM=TSPSym, $
        COLOR=TSCol             ;, $

  IF N_ELEMENTS(junk_ii) GT 0 THEN BEGIN
     OPLOT,tJul[junk_ii],MAKE_ARRAY(N_ELEMENTS(junk_ii),VALUE=junkTransVal), $
           PSYM=junkTransPSym, $
           COLOR=junkTransCol   ;, $
  ENDIF

  IF N_ELEMENTS(befStart_ii) GT 0 THEN BEGIN
     OPLOT,tJul[befStart_ii],MAKE_ARRAY(N_ELEMENTS(befStart_ii),VALUE=junkStartVal), $
           PSYM=junkStartPSym, $
           COLOR=junkStartCol   ;, $
  ENDIF

  IF N_ELEMENTS(wUnsafe_ii) GT 0 THEN BEGIN
     OPLOT,tJul[wUnsafe_ii],MAKE_ARRAY(N_ELEMENTS(wUnsafe_ii),VALUE=ValUnsafe), $
           PSYM=PSymUnsafe, $
           COLOR=ColUnsafe      ;, $
  ENDIF

  IF N_ELEMENTS(w251_ii) GT 0 THEN BEGIN
     OPLOT,tJul[w251_ii],MAKE_ARRAY(N_ELEMENTS(w251_ii),VALUE=Val251), $
           PSYM=PSym251, $
           COLOR=Col251         ;, $
  ENDIF

  IF N_ELEMENTS(w0628_ii) GT 0 THEN BEGIN
     OPLOT,tJul[w0628_ii],MAKE_ARRAY(N_ELEMENTS(w0628_ii),VALUE=Val0628), $
           PSYM=PSym0628, $
           COLOR=Col0628        ;, $
  ENDIF

  IF N_ELEMENTS(w0312_ii) GT 0 THEN BEGIN
     OPLOT,tJul[w0312_ii],MAKE_ARRAY(N_ELEMENTS(w0312_ii),VALUE=Val0312), $
           PSYM=PSym0312, $
           COLOR=Col0312        ;, $
  ENDIF     

END
