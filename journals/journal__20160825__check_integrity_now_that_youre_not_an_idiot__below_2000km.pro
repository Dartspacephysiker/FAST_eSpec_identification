;2016/08/25 Now I've wised up, and the 20160823 journal has been modded
;;Use JOURNAL__20160825__STITCH_TOGETHER_BELOW_2000KM_FILES__INCLUDING_OTHER_COORDS to get all coord systems together
PRO JOURNAL__20160825__CHECK_INTEGRITY_NOW_THAT_YOURE_NOT_AN_IDIOT__BELOW_2000KM

  COMPILE_OPT idl2

  ;;
  indDir       = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  indFile      = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords--recalc_for_every_point--combined.sav'
  indFiles     = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords' + $
                 ['_1','_2','_3']+'--recalc_for_every_point.sav'

  GEOMAGFiles  = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords' + $
                 ['_1','_2','_3']+'.sav'



  ;;DB to check out
  dbDir        = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  dbFile       = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361.sav'

  ;;outfile stuff
  outFile      = 'eSpec_20160607_db--orbs_500-16361--BELOW_2000km--with_AACGM_v2.sav'

  ;;Check integrity of individual AACGM_v2 files first
  FOR k=0,N_ELEMENTS(indFiles)-1 DO BEGIN
     PRINT,"File no. " + STRCOMPRESS(k,/REMOVE_ALL)
     RESTORE,indDir+indFiles[k]

     ;; especGood_i = espec_i[restrict_ii]

     CHECK_SORTED,espec_i,is_sorted_i,/QUIET
     CHECK_SORTED,restrict_ii,is_sorted_restrict_i,/QUIET
     CHECK_SORTED,especGood_i,is_sorted_good_i,/QUIET

     PRINT,"eSpec_i is " + (~KEYWORD_SET(is_sorted_i) ? "NOT " : "") + "sorted"
     PRINT,"restrict_ii is " + (~KEYWORD_SET(is_sorted_restrict_i) ? "NOT " : "") + "sorted"
     PRINT,"especGood_i is " + (~KEYWORD_SET(is_sorted_good_i) ? "NOT " : "") + "sorted"

  ENDFOR

  ;;Clear 'em
  eSpec_i = !NULL
  eSpecGood_i = !NULL
  restrict_ii = !NULL

  ;;Load 'er up
  PRINT,"Master file ..."

  RESTORE,indDir+indFile
  RESTORE,dbDir+dbFile

  CHECK_SORTED,espec_i,is_sorted_i,/QUIET
  PRINT,"eSpec_i is " + (~KEYWORD_SET(is_sorted_i) ? "NOT " : "") + "sorted"

  CHECK_SORTED,eSpec.x[eSpec_i],is_tSorted,/QUIET
  PRINT,"eSpec(< 2000km) tStamps are " + (~KEYWORD_SET(is_tSorted) ? "NOT " : "") + "sorted"

  ;; PRINT,"Blowing those events above 2000 km to Bermuda!"
  ;; eSpec = {x          : eSpec.x[eSpec_i]     , $
  ;;          orbit      : eSpec.orbit[eSpec_i] , $
  ;;          MLT        : eSpec.MLT[eSpec_i]   , $
  ;;          ILAT       : eSpec.ILAT[eSpec_i]  , $
  ;;          alt        : eSpec.alt[eSpec_i]   , $
  ;;          AACGM      : eSpec_AACGM          , $
  ;;          AACGM_sph  : eEphem_AACGMSph_Arr  , $
  ;;          Je         : eSpec.Je[eSpec_i]    , $
  ;;          JEe        : eSpec.JEe[eSpec_i]}

  ;; ;; eSpec_AACGMSphArr = TEMPORARY(eSpec_AACGMSph_Arr)

  ;; PRINT,"Saving repackaged eSpec stuff to " + outFile + ' ...'
  ;; SAVE,eSpec,FILENAME=dbDir+outFile

 END