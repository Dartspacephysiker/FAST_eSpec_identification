;;10/10/16
PRO JOURNAL__20161010__STITCH_ESPECDB_ALT_NOTALT_FILES

  COMPILE_OPT IDL2

  orig_routineName  = 'JOURNAL__20160827__STITCH_ESPECDB_ALT_NOTALT_FILES'

  check_for_sanity  = 1

  R_E               = 6371.2D   ;Earth radius in km, from IGRFLIB_V2.pro

  altitude_max      = 4180      ;in km

  ;; notAltitude_suff        = '--missed_indices--' + GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  notAltitude_suff  = '--missed_indices--' + '20161010'

  coordFiles        = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords' $
                      + ['_1','_2','_3']+'.sav'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  coordDir          = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'

  ;;Input

  ;;This is the eSpec DB being referenced, o'course
  eSpecDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  
  ;;Just for reference--we don't actually use these here
  ephemFileIndArr  = [[      0,10000000,20000000], $
                      [9999999,19999999,28604344]]

  coordFiles       = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords' + $
                     ['_1','_2','_3']+'.sav'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;output
  outFiles         = 'sorted--eSpec_20160607_db--Orbs_500-16361--AACGM_v2_coords' + $
                     ['_1','_2','_3']+'--recalc_for_every_point.sav'
  newOutSuff       = '--oppdatert'

  endSuff          = '--combined_with_notAlt'
  ;;Convert these var names to standard names
  smallFile        = coordDir+outFiles[0]+notAltitude_suff
  RESTORE,smallFile

  aacgmSph_small     = TEMPORARY(AACGMSph)
  AACGMStruct_small  = TEMPORARY(AACGMStruct)
  ;; notRestrict_ii  = TEMPORARY(notRestrict_ii)
  ;; nSmall             = N_ELEMENTS(notRestrict_ii)
  nSmall             = N_ELEMENTS(AACGMStruct_small.alt)

  RESTORE,coordDir+outFiles[0]
  ;; restrict_ii     = TEMPORARY(restrict_ii)
  ;; n                  = N_ELEMENTS(restrict_ii)
  n                  = N_ELEMENTS(AACGMStruct.alt)
  nTot               = nSmall+n

  IF nTot EQ N_ELEMENTS(eSpec_i) THEN BEGIN
     PRINT,"K, whatever you say. I'm just going to do it, if that's what you're telling me to do."
     STOP
     combInds           = [restrict_ii,notRestrict_ii]
     combInds           = combInds[SORT(combInds)]
     combStruct         = {alt:([AACGMStruct.alt,AACGMStruct_small.alt])[combInds], $
                           mlt:([AACGMStruct.mlt,AACGMStruct_small.mlt])[combInds], $
                           lat:([AACGMStruct.lat,AACGMStruct_small.lat])[combInds]}
     AACGMStruct        = TEMPORARY(combStruct)

     combSph            = MAKE_ARRAY(4,nTot)
     combSph[0,*]       = ([REFORM(AACGMSph[0,*]),REFORM(AACGMSph_small[0,*])])[combInds]
     combSph[1,*]       = ([REFORM(AACGMSph[1,*]),REFORM(AACGMSph_small[1,*])])[combInds]
     combSph[2,*]       = ([REFORM(AACGMSph[2,*]),REFORM(AACGMSph_small[2,*])])[combInds]
     combSph[3,*]       = ([REFORM(AACGMSph[3,*]),REFORM(AACGMSph_small[3,*])])[combInds]

     AACGMSph           = TEMPORARY(combSph)


     PRINT,"SAVE,AACGMStruct,AACGMSph,eSpec_i,restrict_ii,notRestrict_ii,n,nSmall,smallFile,coordDir+outFiles[0]"
     SAVE,AACGMStruct,AACGMSph,eSpec_i,restrict_ii,notRestrict_ii,n,nSmall,smallFile, $
          FILENAME=coordDir+outFiles[0]+newOutSuff

  ENDIF ELSE BEGIN
     PRINT,"So these don't match. You trying to pull something?"
     STOP
  ENDELSE



  ;; AACGMStruct.alt[restrict_ii]    = AACGMStruct.alt[0:N_ELEMENTS(restrict_ii)-1]
  ;; AACGMStruct.mlt[restrict_ii]    = AACGMStruct.mlt[0:N_ELEMENTS(restrict_ii)-1]
  ;; AACGMStruct.lat[restrict_ii]    = AACGMStruct.lat[0:N_ELEMENTS(restrict_ii)-1]

  ;; AACGMStruct.alt[notRestrict_ii] = AACGMStruct_small.alt
  ;; AACGMStruct.mlt[notRestrict_ii] = AACGMStruct_small.mlt
  ;; AACGMStruct.lat[notRestrict_ii] = AACGMStruct_small.lat

  IF KEYWORD_SET(check_for_sanity) THEN BEGIN
     PRINT,'Checking for sanity ...'
     RESTORE,coordDir+coordFiles[0]

     diff            = ABS(espec_GEO.alt - AACGMStruct.alt)
     ;; diff            = ABS(espec_GEO.alt[0:1000] - AACGMStruct.alt[0:1000])

     PRINT,'AACGM vs. GEO'
     PRINT_HIST,diff

     IF N_ELEMENTS(WHERE(diff GT 30)) GT 50 THEN PRINT,'Trouble!'
     STOP

     tryme           = WHERE(espec_geo.alt LE 4180,COMPLEMENT=dontTryMe)
     diff            = ABS(espec_GEO.alt[tryme] - AACGMStruct.alt[tryme])

     PRINT,'AACGM vs. GEO'
     PRINT_HIST,diff
     STOP

     diff            = ABS(espec_GEO.alt[dontTryMe] - AACGMStruct.alt[dontTryMe])

     PRINT,'AACGM vs. GEO'
     PRINT_HIST,diff
     STOP
  ENDIF

  PRINT,"SAVE,AACGMStruct,AACGMSph,eSpec_i,restrict_ii,notRestrict_ii,n,nSmall,smallFile,coordDir+outFiles[0]"
  SAVE,AACGMStruct,AACGMSph,eSpec_i,restrict_ii,notRestrict_ii,n,nSmall,smallFile,FILENAME=coordDir+outFiles[0]+endSuff
  
  STOP
END
