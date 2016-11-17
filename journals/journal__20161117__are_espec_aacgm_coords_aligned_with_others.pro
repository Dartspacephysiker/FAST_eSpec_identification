;;11/17/16
PRO JOURNAL__20161117__ARE_ESPEC_AACGM_COORDS_ALIGNED_WITH_OTHERS

  COMPILE_OPT IDL2

  ;;First part opts
  ;;RESULTS OF FIRST PART: Files 2 and 3 (or 1 and 2, depending on how you want to count) are just fine
  skip_first_part      = 1
  first_part__checkNum = 2
  orbRArr              = ['500-5847','5847-11524','11524-16361']


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;FIRST PART

  IF ~skip_first_part THEN BEGIN

     dir       = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
     checkFile = 'eSpec_20160607_db--Orbs_' + orbRArr[first_part__checkNum] + $
                 '--with_alternate_coords__mapFactors--' + $
                 STRCOMPRESS(first_part__checkNum+1,/REMOVE_ALL) + '.sav'

     RESTORE,dir+checkFile

     time = espec.x 
     info = espec.info 
     coords = (TEMPORARY(espec)).coords
     
     dAAC_SDT = ABS(coords.sdt.alt-coords.aacgm.alt)
     dGEO_SDT = ABS(coords.geo.alt-coords.sdt.alt)
     dMAG_SDT = ABS(coords.mag.alt-coords.sdt.alt)

     PRINT,'AACGM vs. SDT'
     PRINT_HIST,dAAC_SDT

     PRINT,'GEO vs. SDT'
     PRINT_HIST,dGEO_SDT

     PRINT,'MAG vs. SDT'
     PRINT_HIST,dMAG_SDT

  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;SECOND PART

  ;;OK, the conclusion here is that the first file's coords are not aligned. How to realign them?
  checkFile   = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361_1--recalc_for_every_point.sav--before_20161117'
  eSpecDir    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  
  coordFiles  = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords' + $
                ['_1','_2','_3']+'.sav'
  restore,especdir+coordfiles[0]
  R_E  = 6371.2000D
  diff = ABS( (REFORM(eephem_aacgmsph_arr[2,*])-1.)*R_E - $
              espec_geo.alt[WHERE(espec_geo.alt LE 4180)])

  niceBoy = WHERE(diff LT 20,nNice,NCOMPLEMENT=nNotNice)
  PRINT,'There are ',nNotNice,' that have apparently not yet been converted' 
  PRINT,'But ',nNice/FLOAT(nNice+nNotNice),'% that have!' 

  STOP

END
