;2016/05/21 Dad's birthday!!!
;Called by MAKE_ELECTRON_SPECTRA_STRUCT_FOR_ALFVEN_EVENTS as part of effort to figure out what these alfven events are

;Explanation of members
;;x: When are you?
;;MLT:
;;ILAT:
;;mono:       0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, (NEGATIVE) = step where algorithm failed
;;broad:      0 = not broadband    , 1 = broadband    , 2 = strict_broadband
;;diffuse:    0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
;;Je:         Electron number flux (#/cm^2-s)
;;Jee:        Electron energy flux (mW/m^2)
;;nBad_eSpec: 0 = no problems      , N = N bad bins
PRO MAKE_BLANK_ESPEC_JE_AND_JEE_STRUCTS,blank_eSpec,blank_je,blank_jee,nSpectra,nEnergies

  blank_eSpec = { x:MAKE_ARRAY(nSpectra,/DOUBLE), $
                  y:MAKE_ARRAY(nSpectra,nEnergies,/FLOAT), $
                  v:MAKE_ARRAY(nSpectra,nEnergies,/FLOAT)}
  
  blank_je    = MAKE_ARRAY(nSpectra,/FLOAT)
  blank_jee   = MAKE_ARRAY(nSpectra,/FLOAT)

END