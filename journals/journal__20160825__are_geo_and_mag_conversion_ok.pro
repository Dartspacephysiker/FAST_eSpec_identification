PRO JOURNAL__20160825__ARE_GEO_AND_MAG_CONVERSION_OK

  COMPILE_OPT idl2

  make_altDiff_histos = 1

  eSpecDir  = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  realFile  = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361.sav' 

  dbDir     = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/' 
  dbFiles   = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords_' + ['1','2','3'] + '.sav'


  ;;Start 'er up
  startStopInds = MAKE_ARRAY(2,N_ELEMENTS(dbFiles),/LONG)
  nToteSC   = 0
  nToteSM   = 0
  nToteSG   = 0

  PRINT,"Doing some counting ..."
  FOR i=0,N_ELEMENTS(dbFiles)-1 DO BEGIN

     RESTORE,dbDir+dbFiles[i]
     
     startStopInds[*,i] = (nToteSC + [0,N_ELEMENTS(eSpecCoords.time)-1])

     nToteSC += N_ELEMENTS(eSpecCoords.time)
     nToteSM += N_ELEMENTS(eSpec_MAG.alt)
     nToteSG += N_ELEMENTS(eSpec_GEO.alt)


     eSpecCoords = !NULL
     eSpec_MAG   = !NULL
     eSpec_GEO   = !NULL
  ENDFOR
  
  IF (nToteSC NE nToteSM) OR (nToteSC NE nToteSG) OR (nToteSM NE nToteSG) THEN STOP
  PRINT,'OK, everything looks good--or at least everyone has the same number of elements'

  PRINT,"Now stitching times ..."
  tFinal    = MAKE_ARRAY(nToteSC,/DOUBLE)
  altGEO    = MAKE_ARRAY(nToteSC,/DOUBLE)
  altMAG    = MAKE_ARRAY(nToteSC,/DOUBLE)
  FOR i=0,N_ELEMENTS(dbFiles)-1 DO BEGIN

     RESTORE,dbDir+dbFiles[i]
     
     tFinal[startStopInds[0,i]:startStopInds[1,i]] = eSpecCoords.time
     altGEO[startStopInds[0,i]:startStopInds[1,i]] = eSpec_GEO.alt
     altMAG[startStopInds[0,i]:startStopInds[1,i]] = eSpec_MAG.alt

     eSpecCoords = !NULL
     eSpec_MAG   = !NULL
     eSpec_GEO   = !NULL
  ENDFOR

  ;;Load the boss
  PRINT,'Restoring file with sorted/uniq times'
  RESTORE,eSpecDir + realFile
  eSpecTimes       = eSpec.x
  eSpecAlt         = eSpec.alt
  eSpec            = !NULL

  CHECK_SORTED,tFinal
  CHECK_SORTED,eSpecTimes

  IF N_ELEMENTS(tFinal) NE N_ELEMENTS(eSpecTimes) THEN STOP

  IF ~ARRAY_EQUAL(eSpecTimes,tFinal) THEN STOP

  ;;Now check altitudes
  diffAltGEO = eSpecAlt - TEMPORARY(altGEO)
  diffAltMAG = eSpecAlt - TEMPORARY(altMAG)

  IF KEYWORD_SET(make_altDiff_histos) THEN BEGIN
     SET_PLOT_DIR,plotDir,/FOR_ESPEC_DB,/ADD_TODAY
     CGHISTOPLOT,diffAltGEO,OUTPUT=plotDir+'sorted_eSpec_20160607_DB--diff_AltGEO_and_AltDST.png'
     CGHISTOPLOT,diffAltMAG,OUTPUT=plotDir+'sorted_eSpec_20160607_DB--diff_AltMAG_and_AltDST.png'
  ENDIF

  badGEO_i   = WHERE(ABS(diffAltGEO) GT 50,nBadGEO)
  badMAG_i   = WHERE(ABS(diffAltGEO) GT 50,nBadMAG)

  IF (nBadMag GT 0) OR (nBadGEO GT 0) THEN BEGIN
     PRINT,"So your altitude is screwed up."
  ENDIF

  PRINT,"If you made it this far, it's only because you've been living right."
END