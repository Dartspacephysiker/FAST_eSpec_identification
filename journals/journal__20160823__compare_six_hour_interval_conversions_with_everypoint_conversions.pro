;;08/23/16
PRO JOURNAL__20160823__COMPARE_SIX_HOUR_INTERVAL_CONVERSIONS_WITH_EVERYPOINT_CONVERSIONS

  COMPILE_OPT IDL2,STRICTARRSUBS

  inDir                     = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/AACGM_v2/'

  outFile1               = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords--recalc_for_every_point--combined.sav'
  outFile2               = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords--combined.sav'

  SET_PLOT_DIR,plotDir,/FOR_ESPEC_DB,/ADD_TODAY

  RESTORE,inDir+outFile1
  AACGM_everyPoint       = TEMPORARY(eSpec_AACGM)
  
  ;;Clear
  eSpec_AACGM            = !NULL
  eEphem_AACGMSph_arr    = !NULL
  restrict_ii            = !NULL
  eSpec_i                = !NULL

  ;;Next file
  RESTORE,inDir+outFile2
  AACGM_notEvery         = TEMPORARY(eSpec_AACGM)

  ;;Clear
  eSpec_AACGM            = !NULL
  eEphem_AACGMSph_arr    = !NULL
  restrict_ii            = !NULL
  eSpec_i                = !NULL


  ;;Check, say, MLT
  diffMLT                = AACGM_everyPoint.MLT-AACGM_notEvery.MLT
  diffALT                = AACGM_everyPoint.ALT-AACGM_notEvery.ALT
  diffLAT                = AACGM_everyPoint.LAT-AACGM_notEvery.LAT

  fileExt                = '.png'

  CGHISTOPLOT,diffMLT, $
              TITLE=CGGREEK('Delta')+'MLT for Coords!DAACGM,6-hour!N and Coords!DAACGM,every tStamp!N', $
              XTITLE=CGGREEK('Delta')+'MLT', $
              OUTPUT=plotDir+'deltaMLT__AACGMsix-hour_coords_vs_AACGM_everyTstamp' + fileExt ;, $
              ;; PS_FONT=1
  

  ;; CGHISTOPLOT,diffALT, $
  ;;             TITLE=CGGREEK('Delta')+'ALT for Coords!DAACGM,6-hour!N and Coords!DAACGM,every tStamp!N', $
  ;;             XTITLE=CGGREEK('Delta')+'ALT', $
  ;;             OUTPUT=plotDir+'deltaALT__AACGMsix-hour_coords_vs_AACGM_everyTstamp' + fileExt

  CGHISTOPLOT,diffLAT, $
              TITLE=CGGREEK('Delta')+'LAT for Coords!DAACGM,6-hour!N and Coords!DAACGM,every tStamp!N', $
              XTITLE=CGGREEK('Delta')+'LAT', $
              OUTPUT=plotDir+'deltaLAT__AACGMsix-hour_coords_vs_AACGM_everyTstamp' + fileExt;; , $
              ;; PS_FONT=1

  STOP

END
