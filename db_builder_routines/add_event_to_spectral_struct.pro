PRO ADD_EVENT_TO_SPECTRAL_STRUCT,events,tempEvent,tSort_i, $
                                 HAS_ALT_AND_ORBIT=has_alt_and_orbit

  ;; event = { time_e:[events.time_e,tempEvent.time_e], $ ; When are you?

  COMPILE_OPT idl2

  IF N_ELEMENTS(tSort_i) GT 0 THEN BEGIN
     inds = tSort_i
  ENDIF ELSE BEGIN
     inds = LINDGEN(N_ELEMENTS(tempEvent.x))
  ENDELSE

  IF N_ELEMENTS(events) EQ 0 THEN BEGIN
     events = tempEvent
  ENDIF ELSE BEGIN
     IF KEYWORD_SET(has_alt_and_orbit) THEN BEGIN
        events = { x:[events.x,tempEvent.x[inds]], $ ; When are you?
                   orbit:[events.orbit,tempEvent.orbit[inds]], $
                   MLT:[events.mlt,tempEvent.mlt[inds]], $
                   ILAT:[events.ilat,tempEvent.ilat[inds]], $
                   ALT:[events.alt,tempEvent.alt[inds]], $
                   mono:[events.mono,tempEvent.mono[inds]], $ ;0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, (NEGATIVE) = step where algorithm failed
                   broad:[events.broad,tempEvent.broad[inds]], $              ;0 = not broadband    , 1 = broadband    , 2 = strict_broadband
                   diffuse:[events.diffuse,tempEvent.diffuse[inds]], $        ;0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
                   Je:[events.Je,tempEvent.Je[inds]], $                       ;Electron number flux (#/cm^2-s)
                   Jee:[events.Jee,tempEvent.Jee[inds]], $                    ;Electron energy flux (mW/m^2)
                   nBad_eSpec:[events.nBad_eSpec,tempEvent.nBad_eSpec[inds]]} ;0 = no problems      , N = N bad bins
     ENDIF ELSE BEGIN
        events = { x:[events.x,tempEvent.x[inds]], $ ; When are you?
                   MLT:[events.mlt,tempEvent.mlt[inds]], $
                   ILAT:[events.ilat,tempEvent.ilat[inds]], $
                   mono:[events.mono,tempEvent.mono[inds]], $ ;0 = not monoenergetic, 1 = monoenergetic, 2 = strict_monoenergetic, (NEGATIVE) = step where algorithm failed
                   broad:[events.broad,tempEvent.broad[inds]], $          ;0 = not broadband    , 1 = broadband    , 2 = strict_broadband
                   diffuse:[events.diffuse,tempEvent.diffuse[inds]], $    ;0 = not diffuse      , 1 = diffuse      , 2 = diffuse, flux extrapolated to 50 keV
                   Je:[events.Je,tempEvent.Je[inds]], $                   ;Electron number flux (#/cm^2-s)
                   Jee:[events.Jee,tempEvent.Jee[inds]], $                ;Electron energy flux (mW/m^2)
                   nBad_eSpec:[events.nBad_eSpec,tempEvent.nBad_eSpec[inds]]} ;0 = no problems      , N = N bad bins
        ;; time_i:[events.time_i,tempEvent.time_i[inds]], $    ;Ion time
        ;; Ji:[events.Ji,tempEvent.Ji[inds]], $                ;Ion number flux      (#/cm^2-s)
        ;; Jei:[events.Jei,tempEvent.Jei[inds]], $             ;Ion energy flux      (mW/m^2)
        ;; nBad_iSpec:[events.nBad_iSpec,tempEvent.nBad_iSpec[inds]], $ ;0 = no problems      , N = N bad bins
     ENDELSE        
  ENDELSE
  

END