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


     CHECK_SORTED,espec_i,is_sorted_i
     CHECK_SORTED,especGood_i,is_sorted_good_i
     CHECK_SORTED,restrict_ii,is_sorted_restrict_i

     especGood_i = espec_i[restrict_ii]
  ENDFOR


  ;;Load 'er up
  RESTORE,indDir+indFile
  RESTORE,dbDir+dbFile
    
  especGood_i = espec_i[restrict_ii]

  CHECK_SORTED,especGood_i  
  sort_ii = SORT(especGood_i)
  especSort_i = especGood_i[sort_ii]



 END