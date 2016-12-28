;;11/17/16
PRO JOURNAL__20161117__ARE_ESPEC_AACGM_COORDS_ALIGNED_WITH_OTHERS

  COMPILE_OPT IDL2

  skip_first_part      = 1
  skip_second_part     = 1
  skip_third_part      = 1

  ;;First part opts
  ;;RESULTS OF FIRST PART: Files 2 and 3 (or 1 and 2, depending on how you want to count) are just fine
  first_part__checkNum = 0
  first_part__indName  = 'LAT'

  orbRArr              = ['500-5847','5847-11524','11524-16361']



  dir       = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;FIRST PART

  IF ~skip_first_part THEN BEGIN

     IF N_ELEMENTS(indName) EQ 0 THEN BEGIN
        indName = 'alt'
     ENDIF

     multSDT    = 1.
     ;;Get proper indName
     CASE STRUPCASE(indName) OF
        'ALT': BEGIN
           AACGMIName  = 'ALT'
           SDTIName    = 'ALT'
           MAGIName    = 'ALT'
           GEOIName    = 'ALT'
        END
        'MLT': BEGIN
           AACGMIName  = 'MLT'
           SDTIName    = 'MLT'
           MAGIName    = 'LON'
           GEOIName    = 'LON'
           multSDT     = 15.
        END
        'LON': BEGIN
           AACGMIName  = 'MLT'
           SDTIName    = 'MLT'
           MAGIName    = 'LON'
           GEOIName    = 'LON'
           multSDT     = 15.
        END
        'LAT': BEGIN
           AACGMIName  = 'LAT'
           SDTIName    = 'ILAT'
           MAGIName    = 'LAT'
           GEOIName    = 'LAT'
        END
        'ILAT': BEGIN
           AACGMIName  = 'LAT'
           SDTIName    = 'ILAT'
           MAGIName    = 'LAT'
           GEOIName    = 'LAT'
        END
        ELSE: STOP
     ENDCASE

     checkFile = 'fully_parsed/eSpec_20160607_db--Orbs_' + orbRArr[first_part__checkNum] + $
                 '--with_alternate_coords__mapFactors--' + $
                 STRCOMPRESS(first_part__checkNum+1,/REMOVE_ALL) + '.sav'

     RESTORE,dir+checkFile

     time = espec.x 
     info = espec.info 
     coords = (TEMPORARY(espec)).coords
     
     AACGMInd  = WHERE(STRUPCASE(TAG_NAMES(coords.AACGM )) EQ STRUPCASE(AACGMIName))
     SDTInd    = WHERE(STRUPCASE(TAG_NAMES(coords.sdt   )) EQ STRUPCASE(SDTIName  ))
     MAGInd    = WHERE(STRUPCASE(TAG_NAMES(coords.MAG   )) EQ STRUPCASE(MAGIName  ))
     GEOInd    = WHERE(STRUPCASE(TAG_NAMES(coords.GEO   )) EQ STRUPCASE(GEOIName  ))

     IF (SDTInd[0] EQ -1) OR (MAGInd[0] EQ -1) OR $
        (GEOInd[0] EQ -1) OR (AACGMInd[0] EQ -1)  $
     THEN STOP

     dAAC_SDT = ABS(coords.sdt.(SDTInd)-coords.aacgm.(AACGMInd)      )
     dGEO_SDT = ABS(coords.geo.(GEOInd)-(coords.sdt.(SDTInd)*multSDT))
     dMAG_SDT = ABS(coords.mag.(MAGInd)-(coords.sdt.(SDTInd)*multSDT))

     PRINT,'AACGM vs. SDT'
     PRINT_HIST,dAAC_SDT

     PRINT,'GEO vs. SDT'
     PRINT_HIST,dGEO_SDT

     PRINT,'MAG vs. SDT'
     PRINT_HIST,dMAG_SDT

     RETURN

  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;SECOND PART

  ;;OK, the conclusion here is that the first file's coords are not aligned. How to realign them?

  IF ~KEYWORD_SET(skip_second_part) THEN BEGIN
     checkFile   = 'alternate_coords/TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361_1--recalc_for_every_point.sav--before_20161117'
     
     coordFiles  = 'alternate_coords/sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords' + $
                   ['_1','_2','_3']+'.sav'
     RESTORE,dir+checkFile
     RESTORE,dir+coordfiles[0]
     R_E  = 6371.2000D
     ;;0: latitude
     ;;1: longitude
     ;;2: altitude
     diff = ABS( (REFORM(eephem_aacgmsph_arr[2,*])-1.)*R_E - $
                 espec_geo.alt[WHERE(espec_geo.alt LE 4180)])

     niceBoy = WHERE(diff LT 20,nNice,NCOMPLEMENT=nNotNice)
     PRINT,'There are ',nNotNice,' that have apparently not yet been converted' 
     PRINT,'But ',nNice/FLOAT(nNice+nNotNice),'% that have!' 

     STOP

     RETURN

  ENDIF


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;THIRD PART: The temp file

  IF ~KEYWORD_SET(skip_third_part) THEN BEGIN
     ;;Now here's some telling evidence:
     R_E  = 6371.2000D

     dir       = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'
     checkFile1 = 'fully_parsed/eSpec_20160607_db--Orbs_500-5847--with_alternate_coords__mapFactors--1.sav'

     RESTORE,dir+checkFile1
     info = espec.info 
     coords = (TEMPORARY(espec)).coords

     checkFile2   = '/alternate_coords/TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361_1--recalc_for_every_point.sav--before_20161117'
     RESTORE,dir+checkFile2
     

     coordInds    = WHERE(coords.aacgm.alt LE 4180 AND coords.aacgm.alt GT 0.1)
     AACGMSphInds = WHERE(REFORM(eephem_aacgmsph_arr[2,*]-1.)*R_E GT 0.1,COMPLEMENT=idiot_i)
     PRINT_HIST, $
        ABS(coords.aacgm.alt[coordInds]-(REFORM(eephem_aacgmsph_arr[2,*]-1.)*R_E)[AACGMSphInds]), $
        /TOTAL
 
     STOP

     RETURN

  ENDIF

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;FOURTH PART: The not-temp file

  dir        = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'
  checkFile1 = 'fully_parsed/eSpec_20160607_db--Orbs_500-5847--with_alternate_coords__mapFactors--1.sav'

  RESTORE,dir+checkFile1
  info       = espec.info 
  coords     = (TEMPORARY(espec)).coords

  checkFile2 = 'alternate_coords/sorted--eSpec_20160607_db--Orbs_500-16361--AACGM_v2_coords_1--recalc_for_every_point.sav--before_20161117'

  RESTORE,dir+checkFile2
     

  coordInds    = WHERE(coords.aacgm.alt LE 4180 AND coords.aacgm.alt GT 0.1)
  AACGMSphInds = WHERE(REFORM(AACGMSph[2,*]-1.) GT 0.1,COMPLEMENT=idiot_i)

  diff         = ABS(coords.aacgm.alt[coordInds] - (REFORM(AACGMSph[2,*]))[AACGMSphInds])
  CASE 1 OF
     MEAN(diff) EQ 0.: BEGIN
        noPrint = 1
     END
     MEAN(diff) LT 1e-2: BEGIN
        binSize = MEAN(diff)/10.
     END
     ELSE:
  ENDCASE

  IF ~KEYWORD_SET(noPrint) THEN $
  PRINT_HIST, $
     diff, $
     /TOTAL, $
     BINSIZE=binSize

  FOR k=0,N_ELEMENTS(tag_names(coords.aacgm))-1 DO BEGIN
     PRINT,k,'  ' + (TAG_NAMES(coords.aacgm))[k],max(coords.aacgm.(k)),min(coords.aacgm.(k))
  ENDFOR

  ;; STOP

  @common__newell_espec.pro

  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB
  ENDIF

  coordInds = [0:9999999]
  coordInds = WHERE(FINITE(AACGMSph[2,coordInds]))
  diff = NEWELL__eSpec.alt[coordInds]-AACGMSph[2,coordInds]

  PRINT_HIST,ABS(diff),/TOTAL
  this = WHERE(ABS(diff) GT 100)

  nDiff  = N_ELEMENTS([0:9999999])
  divFac = 20
  factor = nDiff/divFac
  FOR k=0,divFac-1 DO BEGIN
     ind1    = k*factor
     ind2    = ( ((k+1)*Factor) < nDiff) - 1
     tmpInds = CGSETINTERSECTION([ind1:ind2],coordInds)

     PRINT,"INDS: ",ind1,ind2

     PRINT_HIST,ABS(diff[tmpInds])
  ENDFOR

  STOP

END
