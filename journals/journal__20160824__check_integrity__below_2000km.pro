;2016/08/24 Note, this check is built on a faulty premise
;When combining the results from each of the indFiles (1, 2, and 3), I naïvely assume
;that I can just slap restrict_ii into eSpec_i. Of course that doesn't work! The maximum
;value for restrict_ii is only ever going to be the number of eSpec_i in the partial indFile!
;When the eSpec_i all get combined, there are some 28 million eSpec_i. However, restrict_ii
;will never run to any value greater than, say, 10 million. Of course there are duplicates!
PRO JOURNAL__20160824__CHECK_INTEGRITY__BELOW_2000KM

  COMPILE_OPT idl2

  ;;
  indDir   = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/AACGM_v2/'
  indFile  = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords--recalc_for_every_point--combined.sav'
  indFiles = 'sorted--eSpec_20160607_db--Orbs_500-16361--BELOW_2000_km--AACGM_v2_coords' + $
             ['_1','_2','_3']+'--recalc_for_every_point.sav'

  ;;DB to check out
  dbDir    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  dbFile   = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361.sav'

  ;;Check individual files first
  FOR k=0,N_ELEMENTS(indFiles)-1 DO BEGIN
     PRINT,"File no. " + STRCOMPRESS(k,/REMOVE_ALL)
     RESTORE,indDir+indFiles[k]

     especGood_i = espec_i[restrict_ii]

     CHECK_SORTED,espec_i,is_sorted_i,/QUIET
     CHECK_SORTED,restrict_ii,is_sorted_restrict_i,/QUIET
     CHECK_SORTED,especGood_i,is_sorted_good_i,/QUIET

     PRINT,"eSpec_i is " + (~KEYWORD_SET(is_sorted_i) ? "NOT " : "") + "sorted"
     PRINT,"restrict_ii is " + (~KEYWORD_SET(is_sorted_restrict_i) ? "NOT " : "") + "sorted"
     PRINT,"especGood_i is " + (~KEYWORD_SET(is_sorted_good_i) ? "NOT " : "") + "sorted"

  ENDFOR


  ;;Load 'er up
  RESTORE,indDir+indFile
  RESTORE,dbDir+dbFile
    
  especGood_i = espec_i[restrict_ii]

  CHECK_SORTED,especGood_i  
  sort_ii = SORT(especGood_i)
  especSort_i = especGood_i[sort_ii]



 END