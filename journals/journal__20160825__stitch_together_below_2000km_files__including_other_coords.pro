;;08/23/16
PRO JOURNAL__20160825__STITCH_TOGETHER_BELOW_2000KM_FILES__INCLUDING_OTHER_COORDS

  COMPILE_OPT IDL2


  inDir        = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'


  inFiles      = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords' + $
                 ['_1','_2','_3']+'--recalc_for_every_point.sav'
  GEOMAGFiles  = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords' + $
                 ['_1','_2','_3']+'.sav'

  ;;DB to check out
  dbDir        = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  dbFile       = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361.sav'


  outFile      = 'eSpec_20160607_db--orbs_500-16361--BELOW_2000km--with_alternate_coords.sav'

  ;;Get number of elements first
  PRINT,'Getting total number of elements ...'
  nTotal                    = 0
  nPerFile                  = !NULL
  startInds                 = 0
  FOR i=0,N_ELEMENTS(inFiles)-1 DO BEGIN
     ;;reset vars
     restrict_ii            = !NULL 
     eSpec_i                = !NULL
     nHere                  = 0

     ;;restore
     RESTORE,inDir+inFiles[i]

     ;;Get numbers
     nHere                  = N_ELEMENTS(restrict_ii)
     PRINT,STRCOMPRESS(nHere,/REMOVE_ALL) + ' in file ' + STRCOMPRESS(i+1,/REMOVE_ALL) + '...'

     nPerFile               = [nPerFile,nHere]
     nTotal                += nHere
     startInds              = [startInds,nTotal]

  ENDFOR
  PRINT,'There are ' + STRCOMPRESS(nTotal,/REMOVE_ALL) + ' total elements'

  ;;Set up arrays
  eSpec_AACGM_final         = {ALT:MAKE_ARRAY(nTotal,/FLOAT), $
                               MLT:MAKE_ARRAY(nTotal,/FLOAT), $
                               LAT:MAKE_ARRAY(nTotal,/FLOAT)}
  eSpec_GEO_final           = {ALT:MAKE_ARRAY(nTotal,/FLOAT), $
                               LON:MAKE_ARRAY(nTotal,/FLOAT), $
                               LAT:MAKE_ARRAY(nTotal,/FLOAT)}
  eSpec_MAG_final           = {ALT:MAKE_ARRAY(nTotal,/FLOAT), $
                               LON:MAKE_ARRAY(nTotal,/FLOAT), $
                               LAT:MAKE_ARRAY(nTotal,/FLOAT)}
  eSpec_i_final             = MAKE_ARRAY(nTotal,/LONG)
  tFinal                    = MAKE_ARRAY(nTotal,/DOUBLE)
  eEphem_AACGMSph_arr_final = MAKE_ARRAY(4,nTotal,/FLOAT)
  espec_GEOMAGTot           = 0

  ;;Now combine
  FOR i=0,N_ELEMENTS(inFiles)-1 DO BEGIN

     ;;start/stop inds
     start_i                = startInds[i]
     stop_i                 = startInds[i+1]-1

     ;;Clear vars
     eSpec_AACGM            = !NULL
     eEphem_AACGMSph_arr    = !NULL
     restrict_ii            = !NULL
     eSpec_i                = !NULL

     RESTORE,inDir+inFiles[i]

     PRINT,'Adding stuff from file ' + STRCOMPRESS(i+1,/REMOVE_ALL) + ' ...'
     eSpec_i_final[start_i:stop_i]              = eSpec_i[restrict_ii]
     eSpec_AACGM_final.alt[start_i:stop_i]      = eSpec_AACGM.alt
     eSpec_AACGM_final.mlt[start_i:stop_i]      = eSpec_AACGM.mlt
     eSpec_AACGM_final.lat[start_i:stop_i]      = eSpec_AACGM.lat

     eEphem_AACGMSph_arr_final[*,start_i:stop_i] = eEphem_AACGMSph_arr

     ;;Clear for memory's sake
     eSpec_AACGM            = !NULL
     eEphem_AACGMSph_arr    = !NULL
     nESpec_i               = N_ELEMENTS(eSpec_i)
     eSpec_i                = eSpec_i[TEMPORARY(restrict_ii)]

     ;;Now GEO and MAG
     PRINT,"Now the other coords ..."
     RESTORE,inDir+GEOMAGFiles[i]

     ;;Little check
     IF nESpec_i NE N_ELEMENTS(eSpec_GEO.alt) THEN STOP
     IF N_ELEMENTS(eSpecCoords.time) NE N_ELEMENTS(eSpec_GEO.alt) THEN BEGIN
        PRINT,'Unequal numbers of eSpecCoords and GEO/MAG coords'
        STOP
     ENDIF ELSE BEGIN
        PRINT,'Equal numbers of eSpecCoords and GEO/MAG coords!'
     ENDELSE

     espec_GEOMAGTot                         += N_ELEMENTS(eSpec_GEO.alt)
     eSpec_GEO_final.alt[start_i:stop_i]      = eSpec_GEO.alt[eSpec_i]
     eSpec_GEO_final.lon[start_i:stop_i]      = eSpec_GEO.lon[eSpec_i]
     eSpec_GEO_final.lat[start_i:stop_i]      = eSpec_GEO.lat[eSpec_i]

     eSpec_MAG_final.alt[start_i:stop_i]      = eSpec_MAG.alt[eSpec_i]
     eSpec_MAG_final.lon[start_i:stop_i]      = eSpec_MAG.lon[eSpec_i]
     eSpec_MAG_final.lat[start_i:stop_i]      = eSpec_MAG.lat[eSpec_i]
     
     tFinal[start_i:stop_i]                   = eSpecCoords.time[eSpec_i]

     eSpecCoords         = !NULL
     eSpec_GEO           = !NULL
     eSpec_MAG           = !NULL

  ENDFOR

  ;;Clear again
  eSpec_AACGM            = !NULL
  eEphem_AACGMSph_arr    = !NULL
  eSpec_i                = !NULL

  PRINT,'renaming vars...'
  eSpec_AACGM            = TEMPORARY(eSpec_AACGM_final)
  eEphem_AACGMSph_arr    = TEMPORARY(eEphem_AACGMSph_arr_final)
  eSpec_i                = TEMPORARY(eSpec_i_final)

  eSpec_GEO              = TEMPORARY(eSpec_GEO_final)
  eSpec_MAG              = TEMPORARY(eSpec_MAG_final)

  ;;Load 'er up
  PRINT,"Master file ..."
  RESTORE,dbDir+dbFile

  ;;Check everything, make sure we're safe

  IF espec_GEOMAGTot NE N_ELEMENTS(eSpec.x) THEN BEGIN
     PRINT,'Somehow you have unequal numbers of GEO/MAG coordinates and eSpec DB entries.'
     STOP
  ENDIF ELSE BEGIN
     PRINT,'Equal numbers of GEO/MAG coords and eSpec DB entries!'
  ENDELSE

  CHECK_SORTED,espec_i,is_sorted_i,/QUIET
  PRINT,"eSpec_i is " + (~KEYWORD_SET(is_sorted_i) ? "NOT " : "") + "sorted"

  CHECK_SORTED,eSpec.x[eSpec_i],is_tSorted,/QUIET
  PRINT,"eSpec(< 2000km) tStamps are " + (~KEYWORD_SET(is_tSorted) ? "NOT " : "") + "sorted"

  IF ~is_tSorted                           THEN STOP
  ;; IF ~ARRAY_EQUAL(tFinal,eSpec.x[eSpec_i]) THEN BEGIN
  ;;    diff = tFinal-eSpec.x[eSpec_i]
  ;;    bad_i = WHERE(ABS(diff) GT 0.0001,nBad,COMPLEMENT=good_i,NCOMPLEMENT=nGood)
  ;;    PRINT,STRCOMPRESS(nBad,/REMOVE_ALL) + ' instances where times do not match'
  ;;    PRINT,'... but ' + STRCOMPRESS(nGood,/REMOVE_ALL) + ' instances where they do ...'
  ;;    STOP
  ;; ENDIF

  PRINT,"Blowing those events above 2000 km to Bermuda!"
  eSpec = {x          : eSpec.x[eSpec_i]     , $
           orbit      : eSpec.orbit[eSpec_i] , $
           coords     : {SDT   : {MLT  : eSpec.MLT[eSpec_i]  , $
                                  ILAT : eSpec.ILAT[eSpec_i] , $
                                  alt  : eSpec.alt[eSpec_i]} , $
                         AACGM : TEMPORARY(eSpec_AACGM) , $
                         GEO   : TEMPORARY(eSpec_GEO)   , $
                         MAG   : TEMPORARY(eSpec_MAG)}  , $
           Je         : eSpec.Je[eSpec_i]    , $
           JEe        : eSpec.JEe[eSpec_i]}



  PRINT,'Saving to ' + outFile + ' ...'
  SAVE,eSpec, $
       ;; eEphem_AACGMSph_arr, $
       ;; eSpec_i, $
       FILENAME=dbDir+outFile

END
