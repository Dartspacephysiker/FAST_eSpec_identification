;;02/15/17
PRO JOURNAL__20170215__LOOK_AT_VARIOUSNESSES_ASSOCIATED_WITH_DOWNGOING_ION_DB

  COMPILE_OPT IDL2

  @common__newell_ion_db.pro

  reload = 0B
  CASE 1 OF
     N_ELEMENTS(NEWELL_I__ion) GT 0: BEGIN
        reload = ~(NEWELL_I__ion.info.is_downgoing AND NEWELL_i_ion.info.is_mapped)
     END
     ELSE: BEGIN
        reload = 0B
     END
  ENDCASE

  IF reload THEN BEGIN
     LOAD_NEWELL_ION_DB, $
        /FORCE_LOAD_DB, $
        /LOAD_DELTA_T, $
        /LOAD_CHARE, $
        /DOWNGOING
  ENDIF
  
  nz_i = WHERE(ABS(NEWELL_I__ion.ji) GT 0. AND ABS(NEWELL_I__ion.jei) GT 0.,nNZ)

  IF nNZ EQ 0 THEN STOP

  this = CREATEBOXPLOTDATA(NEWELL_I__ion.ji[nz_i])  
  this = CREATEBOXPLOTDATA(ALOG10(ABS(NEWELL_I__ion.ji[nz_i])))

  CGHISTOPLOT,ALOG10(ABS(newell_i__ion.ji[nz_i]))

  nTot        = N_ELEMENTS(NEWELL_I__ion.x)

  checkEmsJi = [9,10,11,12,13]
  PRINT,FORMAT='(A-8,T10,A0,T25,A0)','10^(POW)',"N Ji GT","% DB"
  FOR k=0,N_ELEMENTS(checkEmsJi)-1 DO BEGIN & $
     tmpN = N_ELEMENTS(WHERE(ALOG10(ABS(newell_i__ion.ji)) GT checkEmsJi[k],/NULL)) & $
     PRINT,FORMAT='(A-8,T10,I08,T25,G0.3)', $
           checkEmsJi[k],tmpN,tmpN/FLOAT(nTot)*100. & $
  ENDFOR
  
  checkEmsJei = [-1,0,1,2,3]
  PRINT,FORMAT='(A-8,T10,A0,T25,A0)','10^(POW)',"N Jei GT","% DB"
  FOR k=0,N_ELEMENTS(checkEmsJei)-1 DO BEGIN & $
     tmpN = N_ELEMENTS(WHERE(ALOG10(ABS(newell_i__ion.jei)) GT checkEmsJei[k],/NULL)) & $
     PRINT,FORMAT='(A-8,T10,I08,T25,G0.3)', $
           checkEmsJei[k],tmpN,tmpN/FLOAT(nTot)*100. & $
  ENDFOR

  ;; this = N_ELEMENTS(WHERE(ALOG10(ABS(newell_i__ion.ji)) GT 12))
  ;; PRINT,N_ELEMENTS(WHERE(ALOG10(ABS(newell_i__ion.ji)) GT 10))
END
