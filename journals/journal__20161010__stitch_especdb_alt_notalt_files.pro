;;10/10/16
PRO JOURNAL__20161010__STITCH_ESPECDB_ALT_NOTALT_FILES

  COMPILE_OPT IDL2

  orig_routineName  = 'JOURNAL__20160827__CONVERT_ESPECDB_ILATS_TO_AACGM__ALL_OF_EM'

  R_E               = 6371.2D   ;Earth radius in km, from IGRFLIB_V2.pro

  altitude_max      = 4180      ;in km
  allow_fl_trace    = 1         ;Allow fieldline tracing for AACGM_v2?
  check_if_exists   = 1

  create_notAltitude_file = 1
  notAltitude_suff        = '--missed_indices--' + GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  convert_varnames_and_resave_outFiles = 1

  force_newCheckItvl = 1000
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  outDir           = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  outFiles         = 'sorted--eSpec_20160607_db--Orbs_500-16361--AACGM_v2_coords' + $
                     ['_1','_2','_3']+'--recalc_for_every_point.sav'

  endSuff          = '--combined_with_notAlt'
  ;;Convert these var names to standard names
  in_names = {GEOSph       : 'eEphem_GEOSph_arr', $
              AACGMSph     : 'eEphem_AACGMSph_arr', $
              GEOStruct    : 'eSpec_GEO', $
              AACGMStruct  : 'eSpec_AACGM', $
              coordStruct  : 'eSpecCoords', $
              timeStr      : 'eSTTempStr', $
              DBInd        : 'eSpec_i'}

  defNames = {AACGMSph    : 'AACGMSph', $
              AACGMStruct : 'AACGMStruct', $
              restrictVar : 'restrict_ii', $
              DBInd       : 'DBInds'}

  smallFile = outDir+outFiles[0]+notAltitude_suff
  RESTORE,smallFile

  aacgmSph_small = AACGMSph
  AACGMStruct_small = AACGMStruct
  ;; notRestrict_ii = TEMPORARY(notRestrict_ii)
  nSmall    = N_ELEMENTS(notRestrict_ii)

  RESTORE,outDir+outFiles[0]
  ;; restrict_ii = TEMPORARY(restrict_ii)
  n   = N_ELEMENTS(restrict_ii)
  nTot = nSmall+n

  IF nTot NE N_ELEMENTS(eSpec_i) THEN STOP

  combInds = [restrict_ii,notRestrict_ii]
  
  combInds = combInds[SORT(combInds)]

  combStruct = {alt:([AACGMStruct.alt,AACGMStruct_small.alt])[combInds], $
                mlt:([AACGMStruct.mlt,AACGMStruct_small.mlt])[combInds], $
                lat:([AACGMStruct.lat,AACGMStruct_small.lat])[combInds]}

  AACGMStruct = TEMPORARY(combStruct)
  combSph    = MAKE_ARRAY(4,nTot)
  combSph[0,*] = ([REFORM(AACGMSph[0,*]),REFORM(AACGMSph_small[0,*])])[combInds]
  combSph[1,*] = ([REFORM(AACGMSph[1,*]),REFORM(AACGMSph_small[1,*])])[combInds]
  combSph[2,*] = ([REFORM(AACGMSph[2,*]),REFORM(AACGMSph_small[2,*])])[combInds]
  combSph[3,*] = ([REFORM(AACGMSph[3,*]),REFORM(AACGMSph_small[3,*])])[combInds]

  AACGMSph     = TEMPORARY(combSph)

  PRINT,"SAVE,AACGMStruct,AACGMSph,eSpec_i,restrict_ii,notRestrict_ii,n,nSmall,smallFile,outDir+outFiles[0]"
  SAVE,AACGMStruct,AACGMSph,eSpec_i,restrict_ii,notRestrict_ii,n,nSmall,smallFile,FILENAME=outDir+outFiles[0]+endSuff
  
  STOP
END
