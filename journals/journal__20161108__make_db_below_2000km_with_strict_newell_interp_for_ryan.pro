;;11/08/16
PRO JOURNAL__20161108__MAKE_DB_BELOW_2000KM_WITH_STRICT_NEWELL_INTERP_FOR_RYAN

  COMPILE_OPT IDL2

  dir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'

  km2000file = 'eSpec_20160607_db--orbs_500-16361--BELOW_2000km--with_alternate_coords__mapping_factors.sav'
  outkm2000file = 'eSpec_20160607_db--orbs_500-16361--BELOW_2000km--with_alternate_coords__mapping_factors__strict_Newell_interp.sav'

  LOAD_NEWELL_ESPEC_DB,eSpec,/DONT_LOAD_IN_MEMORY

  m = eSpec.mono
  b = eSpec.broad
  d = eSpec.diffuse
  time = eSpec.x

  magicNumber = 5893916L

  magicAlt = 1990
  stopAlt  = 1989
  inds = WHERE(eSpec.alt LE magicAlt,nInds)

  WHILE (nInds NE magicNumber) DO BEGIN
     magicAlt--

     inds = WHERE(eSpec.alt LE magicAlt,nInds)

     PRINT,'MagicAlt, nInds: ',magicAlt,nInds

     IF magicAlt EQ stopAlt THEN BEGIN
        PRINT,'Fine, trying another way ...'
        try_another_way = 1
        BREAK
     ENDIF
  ENDWHILE
  eSpec = !NULL

  RESTORE,dir+km2000file

  IF KEYWORD_SET(try_another_way) THEN BEGIN
     these = VALUE_CLOSEST2(time,eSpec.x)

     diffs = ABS(eSpec.x-time[these])
     this  = HISTOGRAM(diffs[WHERE(diffs LT 30)],LOCATIONS=locs)
     FOR i=0,n_elements(locs)-1 DO PRINT,locs[i],this[i]

  ENDIF ELSE BEGIN
     IF N_ELEMENTS(eSpec.alt) NE N_ELEMENTS(inds) THEN STOP
  ENDELSE

  eSpec = CREATE_STRUCT(eSpec,'MONO',m[these],"BROAD",b[these],'DIFFUSE',d[these])

  ;;Exclusivity
  PRINT,espec.mono[uniq(espec.mono,SORT(espec.mono))]
  PRINT,espec.broad[uniq(espec.broad,SORT(espec.broad))]
  PRINT,espec.diffuse[uniq(espec.diffuse,SORT(espec.diffuse))]

  check = CGSETINTERSECTION(WHERE(espec.mono EQ 1 OR espec.mono EQ 2), $
                            WHERE(espec.broad EQ 1 OR espec.broad EQ 2), $
                            COUNT=mboverlap,NORESULT=-1)

  check = CGSETINTERSECTION(WHERE(espec.mono EQ 1 OR espec.mono EQ 2), $
                            WHERE(espec.diffuse NE 0), $
                            COUNT=mdoverlap,NORESULT=-1)

  check = CGSETINTERSECTION(WHERE(espec.broad EQ 1 OR espec.broad EQ 2), $
                            WHERE(espec.diffuse NE 0), $
                            COUNT=bdoverlap,NORESULT=-1)

  PRINT,"Saving struct with hard 'n' fast Newell interp and only obs below 2000 km ..."
  STOP
  SAVE,eSpec,FILENAME=dir+outkm2000file

END
