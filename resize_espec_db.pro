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

FUNCTION RESIZE_ESPEC_DB,eSpecs_parsed,cat_i

  nBef        = N_ELEMENTS(eSpecs_parsed.x)
  nAft        = N_ELEMENTS(cat_i)

  temp_parsed = { x:eSpecs_parsed.x[cat_i], $
                  MLT:eSpecs_parsed.mlt[cat_i], $
                  ILAT:eSpecs_parsed.ilat[cat_i], $
                  mono:eSpecs_parsed.mono[cat_i], $
                  broad:eSpecs_parsed.broad[cat_i], $
                  diffuse:eSpecs_parsed.diffuse[cat_i], $
                  Je:eSpecs_parsed.Je[cat_i], $          
                  Jee:eSpecs_parsed.Jee[cat_i], $        
                  nBad_eSpec:eSpecs_parsed.nBad_eSpec[cat_i]}
  

  RETURN,temp_parsed

END