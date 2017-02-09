;;02/09/17
PRO IDENTIFY_GAPS__RM_INTERVAL,tmpTime,wCheck,wCheck_ii,nCheck,start_ii,stop_ii,sampDT,junk_ii

  COMPILE_OPT IDL2

  wCheck_ii = !NULL

  befs = (sampDT*3.1) < 5
  afts = (sampDT*3.1) < 5
  FOR k=0,nCheck-1 DO BEGIN
     tmpK       = wCheck[k]
     wCheck_ii = [wCheck_ii,[start_ii[tmpK]:stop_ii[tmpK]]]
     tmpJunk_ii = WHERE((tmpTime GE (tmpTime[stop_ii[tmpK]]-befs[tmpK])) AND $
                        (tmpTime LE (tmpTime[stop_ii[tmpK]]+afts[tmpK])), $
                        nTmpJunk)

     IF nTmpJunk GT 0 THEN BEGIN
        ;; junk_i  = [junk_i,tmp_i[tmpJunk_ii]]
        junk_ii = [junk_ii,tmpJunk_ii]
     ENDIF ELSE BEGIN
        IF sampDT[tmpK] LT 0. THEN BEGIN

        ENDIF
     ENDELSE
  ENDFOR

END

