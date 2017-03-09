;;2016/08/27
;;Please compile AACGM lib before running: IDL> @compile_aacgm.pro
PRO JOURNAL__20160827__CONVERT_ESPECDB_ILATS_TO_AACGM__ALL_OF_EM

  COMPILE_OPT IDL2,STRICTARRSUBS

  do_conversions    = 1

  orig_routineName  = 'JOURNAL__20160827__CONVERT_ESPECDB_ILATS_TO_AACGM__ALL_OF_EM'

  R_E               = 6371.2D   ;Earth radius in km, from IGRFLIB_V2.pro

  altitude_max      = 4180      ;in km
  allow_fl_trace    = 1         ;Allow fieldline tracing for AACGM_v2?
  check_if_exists   = 1

  create_notAltitude_file = 0
  notAltitude_suff        = '--missed_indices--' + GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  convert_varnames_and_resave_outFiles = 0

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

  ;; outFiles         = 'sorted--eSpec_20160607_db--Orbs_500-16361--AACGM_v2_coords' + $
  ;;                    ['_1','_2','_3']+'--recalc_for_every_point.sav--mydude'

  ;;In case we get cut off--think SNES emulator savestate
  tmpFiles         = outDir+'TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361' + $
                     ['_1','_2','_3']+'--recalc_for_every_point.sav'
  timeFiles        = outDir+'TEMP_AACGM--sorted--eSpec_20160607_db--Orbs_500-16361' + $
                     ['_1','_2','_3']+'--recalc_for_every_point--timeStrings.sav'

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
              DBInd       : 'DBInds', $
              DBIndName   : 'db_i'}

  IF KEYWORD_SET(do_conversions) THEN BEGIN
     CONVERT_GEO_TO_AACGM, $
        COORDFILES=coordFiles, $
        COORDDIR=eSpecDir, $
        TMPFILES=tmpFiles, $
        TIMEFILES=timeFiles, $
        EPHEMFILEINDARR=ephemFileIndArr, $
        OUTDIR=outDir, $
        OUTFILES=outFiles, $
        ORIG_ROUTINENAME=orig_routineName, $
        R_E=R_E, $
        ALTITUDE_MAX=altitude_max, $
        ALLOW_FL_TRACE=allow_FL_trace, $
        CHECK_IF_EXISTS=check_IF_exists, $
        CREATE_NOTALTITUDE_FILE=create_notAltitude_file, $
        NOTALTITUDE_SUFF=notAltitude_suff, $
        CONVERT_VARNAMES_AND_RESAVE_OUTFILES=convert_varNames_and_resave_outFiles, $
        FORCE_NEWCHECKITVL=force_newCheckItvl, $
        IN_NAMES=in_names, $
        DEFNAMES=defNames

  ENDIF

END

