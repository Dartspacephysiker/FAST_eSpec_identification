;;08/23/16
PRO JOURNAL__20160823__STITCH_TOGETHER_BELOW_2000KM_AACGM_FILES

  COMPILE_OPT IDL2,STRICTARRSUBS

  do_everyPoint_files       = 0

  inDir                     = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'

  IF KEYWORD_SET(do_everyPoint_files) THEN BEGIN
     inFiles                = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords' + $
                              ['_1','_2','_3']+'--recalc_for_every_point.sav'
     outFile                = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords--recalc_for_every_point--combined.sav'
  ENDIF ELSE BEGIN
     inFiles                = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords' + $
                              ['_1','_2','_3']+'.sav'
     outFile                = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords--combined.sav'
  ENDELSE

  ;;Final:

  ;;Each file contains the following
  ;;eSpec_AACGM,eEphem_AACGMSph_arr,restrict_ii,eSpec_i


  ;;Get number of elements first
  PRINT,'Getting total number of elements ...'
  nTotal                    = 0
  ;; nEspec_i_total            = 0
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

  eSpec_i_final             = MAKE_ARRAY(nTotal,/LONG)
  ;; restrict_ii_final         = MAKE_ARRAY(nTotal,/LONG)
  eEphem_AACGMSph_arr_final = MAKE_ARRAY(4,nTotal,/FLOAT)

  ;;Now combine
  FOR i=0,N_ELEMENTS(inFiles)-1 DO BEGIN

     ;;start/stop inds
     start_i                = startInds[i]
     stop_i                 = startInds[i+1]-1

     ;; startE_i               = startEspecInds[i]
     ;; stopE_i                = startEspecInds[i+1]-1

     ;;Clear vars
     eSpec_AACGM            = !NULL
     eEphem_AACGMSph_arr    = !NULL
     restrict_ii            = !NULL
     eSpec_i                = !NULL

     RESTORE,inDir+inFiles[i]

     PRINT,'Adding stuff from file ' + STRCOMPRESS(i+1,/REMOVE_ALL) + ' ...'
     eSpec_i_final[start_i:stop_i]              = eSpec_i[restrict_ii]
     ;; restrict_ii_final[start_i:stop_i]          = restrict_ii
     eSpec_AACGM_final.alt[start_i:stop_i]      = eSpec_AACGM.alt
     eSpec_AACGM_final.mlt[start_i:stop_i]      = eSpec_AACGM.mlt
     eSpec_AACGM_final.lat[start_i:stop_i]      = eSpec_AACGM.lat

     eEphem_AACGMSph_arr_final[*,start_i:stop_i] = eEphem_AACGMSph_arr

  ENDFOR

  ;;Clear again
  eSpec_AACGM            = !NULL
  eEphem_AACGMSph_arr    = !NULL
  ;; restrict_ii            = !NULL
  eSpec_i                = !NULL

  PRINT,'renaming vars...'
  eSpec_AACGM            = TEMPORARY(eSpec_AACGM_final)
  eEphem_AACGMSph_arr    = TEMPORARY(eEphem_AACGMSph_arr_final)
  ;; restrict_ii            = TEMPORARY(restrict_ii_final)
  eSpec_i                = TEMPORARY(eSpec_i_final)


  PRINT,'Saving to ' + outFile + ' ...'
  SAVE,eSpec_AACGM, $
       eEphem_AACGMSph_arr, $
       ;; restrict_ii, $
       eSpec_i, $
       FILENAME=inDir+outFile

END
