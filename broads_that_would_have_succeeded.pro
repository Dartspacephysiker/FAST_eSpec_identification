FUNCTION BROADS_THAT_WOULD_HAVE_SUCCEEDED,eSpec,failCodes,charEThresh, $
                                          COUNT=NthatIHadBeenThere, $
                                          CUSPCOUNT=cuspCount, $
                                          NOTCUSPCOUNT=notCuspCount
  
  COMPILE_OPT IDL2,STRICTARRSUBS

  IF N_ELEMENTS(charEThresh) EQ 0 THEN charEThresh = 140

  ohthatwewere = WHERE( ( ( (failCodes.mono  AND 1) GT 0 ) OR  ( (failCodes.mono  AND 2) GT 0 ) ) $ ;;This says mono would have failed before charE
                        AND $
                        ( ( (failCodes.broad AND 1) EQ 0 ) AND ( (failCodes.broad AND 4) EQ 0 ) ) $ ;; This says broad would have passed all other reqs
                        AND $
                        ( (ABS(eSpec.jee/eSpec.je)*6.242*1.0e11) GE chareThresh ) , $            ;; This says the chare is above the threshold
                        NthatIHadBeenThere)


  ohCusp       = CGSETINTERSECTION(WHERE(eSpec.mlt GT 9.5 AND eSpec.mlt LT 14.5),ohthatwewere,COUNT=cuspCount)
  ohNotCusp    = CGSETINTERSECTION(WHERE(eSpec.mlt LE 9.5 OR  eSpec.mlt GE 14.5),ohthatwewere,COUNT=notCuspCount)

  RETURN,ohthatwewere

END