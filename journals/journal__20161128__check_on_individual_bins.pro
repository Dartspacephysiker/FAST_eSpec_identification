;;11/28/16
PRO JOURNAL__20161128__CHECK_ON_INDIVIDUAL_BINS

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__newell_espec.pro

  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 OR KEYWORD_SET(force_load_DB) THEN BEGIN
     LOAD_NEWELL_ESPEC_DB, $
        !NULL, $
        NEWELLDBDIR=NewellDBDir, $
        USE_2000KM_FILE=use_2000km_file, $
        FORCE_LOAD_DB=force_load_DB
  ENDIF

  minm = 8
  maxm = 9
  mini = 62.5
  maxi = 65.0
  orbrange = [1000,10800]
  
  this = get_espec_ion_db_ind(newell__espec,minmlt=minm,maxmlt=maxm,minilat=mini,maxilat=maxi,$
  			    orbrange=orbrange)
  
  orbs = newell__espec.orbit[this[uniq(newell__espec.orbit[this])]]
  
  !P.MULTI = [0,1,2,0,0]
  cghistoplot,ALOG10(NEWELL__eSpec.je[this])
  cghistoplot,ALOG10(NEWELL__eSpec.jee[this])

  STOP

  maskem = WHERE((Newell__espec.je GT 0) AND (Newell__espec.jee GT 0))
  maskem = WHERE((Newell__espec.je GT 0) AND (Newell__espec.jee GT 0) AND ( (NEWELL__eSpec.broad EQ 1) OR (NEWELL__eSpec.broad EQ 2)  ))
  je = newell__espec.je[maskEm]
  jee = newell__espec.jee[maskEm]
  orbs = newell__espec.orbit[maskEm]
  
  ilats = newell__espec.ilat[maskEm]
  
  !P.MULTI = [0,1,3,0,0]
  
  prosjent = 99.0
  je_i = GET_INDICES_ABOVE_PERCENT_THRESHOLD(je,prosjent,OUT_FINAL_VAL=botBarJe)
  jee_i = GET_INDICES_ABOVE_PERCENT_THRESHOLD(jee,prosjent,OUT_FINAL_VAL=botBarJee)
  jeJee_i = CGSETINTERSECTION(je_i,jee_i,COUNT=NjeJee)
  
  cghistoplot,ALOG10(je[jejee_i])
  cghistoplot,ALOG10(jee[jejee_i])
  cghistoplot,ilats[jejee_i]


END
