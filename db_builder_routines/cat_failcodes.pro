PRO CAT_FAILCODES,failCodes,failCodes_in,cat_i



  IF N_ELEMENTS(cat_i) NE 0 THEN BEGIN
     tempFailCodes = { mono:failCodes_in.mono[cat_i], $
                       mono_nAbove:failCodes_in.mono_nAbove[cat_i], $
                       mono_nBelow:failCodes_in.mono_nBelow[cat_i], $
                       broad:failCodes_in.broad[cat_i], $
                       nBroad:failCodes_in.nBroad[cat_i], $
                       N_broad_GE_min_eV:failCodes_in.N_broad_GE_min_eV[cat_i], $          
                       peakFlux:failCodes_in.peakFlux[cat_i], $        
                       peakEnergy:failCodes_in.peakEnergy[cat_i]}
  ENDIF ELSE BEGIN
     tempFailCodes = failCodes_in
  ENDELSE

  IF N_ELEMENTS(failCodes) EQ 0 THEN BEGIN
     failCodes = tempFailCodes
  ENDIF ELSE BEGIN
     
     failCodes = { mono:[failCodes.mono,tempFailCodes.mono], $ 
                   mono_nAbove:[failCodes.mono_nAbove,tempFailCodes.mono_nAbove], $ 
                   mono_nBelow:[failCodes.mono_nBelow,tempFailCodes.mono_nBelow], $ 
                   broad:[failCodes.broad,tempFailCodes.broad], $
                   nBroad:[failCodes.nBroad,tempFailCodes.nBroad], $
                   N_broad_GE_min_eV:[failCodes.N_broad_GE_min_eV,tempFailCodes.N_broad_GE_min_eV], $
                   peakFlux:[failCodes.peakFlux,tempFailCodes.peakFlux], $
                   peakEnergy:[failCodes.peakEnergy,tempFailCodes.peakEnergy]}
     
  ENDELSE
  

END