;;11/25/16
FUNCTION GET_ESPEC_ION_DELTA_T,elOrIon, $
                               DBNAME=dbName
                               

  COMPILE_OPT IDL2

  IF N_ELEMENTS(dbName) EQ 0 THEN dbname = 'ElOrIon'

  elOrIon_delta_t          = [FLOAT(elOrIon.x[1:-1]-elOrIon.x[0:-2]),-1.0]

  worst_i                = WHERE(( elOrIon_delta_t LT 0   ) OR $
                                 ( elOrIon_delta_t GT 2.6 ),nWorst, $
                                 COMPLEMENT=best_i, $
                                 NCOMPLEMENT=nBest)
  ;; IF nWorst GT 0 THEN BEGIN
  ;;    PRINT,'The worst!'
  ;;    elOrIon_delta_t[worst_i] = 2.5
  ;;    ;; STOP
  ;; ENDIF

  best_i__i  = VALUE_CLOSEST2(elOrIon.x[best_i],elOrIon.x[worst_i])
  
  best_i__ii = WHERE( ( ABS(elOrIon.x[best_i[best_i__i]] - $
                            elOrIon.x[worst_i]) LT 2.6 ) AND $
                      ( elOrIon_delta_t[best_i[best_i__i]] LT 2.6 ) AND $
                      ( elOrIon_delta_t[best_i[best_i__i]] GT 0 ), $
                      nBest_i__i, $
                      COMPLEMENT=best_i__badii, $
                      NCOMPLEMENT=nBest_i__badii)

  PRINT,"Salvaged " + STRCOMPRESS(nBest_i__i,/REMOVE_ALL) + $
        dbName + " delta-Ts from a cruel fate"
  PRINT,STRCOMPRESS(nBest_i__badii,/REMOVE_ALL) + $
        " are going to the pit (=0.0)"

  elOrIon_delta_t[worst_i[best_i__ii]]    = elOrIon_delta_t[best_i[best_i__i[best_i__ii]]]
  elOrIon_delta_t[worst_i[best_i__badii]] = 0.0 ;;Just have to discard them, because what are they?

  RETURN,elOrIon_delta_t
END
