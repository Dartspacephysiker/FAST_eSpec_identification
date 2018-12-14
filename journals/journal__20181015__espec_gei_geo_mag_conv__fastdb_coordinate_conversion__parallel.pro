;;02/01/17
PRO JOURNAL__20181015__ESPEC_GEI_GEO_MAG_CONV__FASTDB_COORDINATE_CONVERSION__PARALLEL

  COMPILE_OPT IDL2,STRICTARRSUBS
  
  ;; outFile_pref            = 'eMomDB_20181015-1000-11776'
  outFile_pref            = 'eMomDB_20181127-1000-51315'
  dry_run                 = 0

  ;; nCPUsToRun              = 4
  startCPU                = 4
  stopCPU                 = 6

  diag                    = 0 ;diagnostic mode

  check_if_exists         = 1

  ;;Alle
  ;; create_timeStampsArr      = [1,0,0,0,0]
  ;; get_GEI_coordsArr         = [0,0,1,0,0]
  ;; do_GEO_MAG_conversionsArr = [0,0,0,1,0]
  ;; do_AACGM_conversionsArr   = [0,0,0,0,0]
  ;; get_dipoleTilt_dataArr    = [0,1,0,0,0]
  ;; stitch_filesArr           = [0,0,0,0,1]

  create_timeStampsArr      = [0]
  get_GEI_coordsArr         = [0]
  do_GEO_MAG_conversionsArr = [0]
  do_AACGM_conversionsArr   = [0]
  get_dipoleTilt_dataArr    = [0]
  stitch_filesArr           = [1]

  ;; test_single             = 0

  ;;Less-important options
  DBDir                   = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/v2018/'
  coordDir                = DBDir 
  orig_routineName        = 'JOURNAL__20181015__ESPEC_GEI_GEO_MAG_CONV__FASTDB_COORDINATE_CONVERSION__PARALLEL'

  ;; LOAD_NEWELL_ESPEC_DB,eSpec, $
  ;;                      /NO_MEMORY_LOAD, $
  ;;                      /FINALDB, $
  ;;                      /DONT_CONVERT_TO_STRICT_NEWELL, $
  ;;                      /DONT_MAP_TO_100KM, $
  ;;                      /DONT_PERFORM_CORRECTION

  ;; times = (TEMPORARY(eSpec)).x

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Just get times, silly
  defNewellDBDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/v2018/'
  ephemFile        = 'eMomDB_20181127-1000-51315-ephem.sav' 

  RESTORE,defNewellDBDir+ephemFile
  times = (TEMPORARY(ephem)).time

  ;; nThang = N_ELEMENTS(create_timeStampsArr)
  nThang = N_ELEMENTS(create_timeStampsArr)
  FOR k=0,nThang-1 DO BEGIN

     ;;set 'er up
     create_timeStamps       = create_timeStampsArr[k]
     get_GEI_coords          = get_GEI_coordsArr[k]
     do_GEO_MAG_conversions  = do_GEO_MAG_conversionsArr[k]
     do_AACGM_conversions    = do_AACGM_conversionsArr[k]
     get_dipoleTilt_data     = get_dipoleTilt_dataArr[k]
     stitch_files            = stitch_filesArr[k]

     PRINT,""
     PRINT,"ITERATION ",k
     PRINT,'create_timeStamps     ',create_timeStamps
     PRINT,'get_GEI_coords        ',get_GEI_coords
     PRINT,'do_GEO_MAG_conversions',do_GEO_MAG_conversions
     PRINT,'do_AACGM_conversions  ',do_AACGM_conversions
     PRINT,'get_dipoleTilt_data   ',get_dipoleTilt_data
     PRINT,'stitch_files          ',stitch_files

     FASTDB_COORDINATE_CONVERSION__PARALLEL, $
        times, $
        NCPUSTORUN=nCPUsToRun, $
        STARTCPU=startCPU, $
        STOPCPU=stopCPU, $
        CREATE_TIMESTAMPS=create_timeStamps, $
        GET_GEI_COORDS=get_GEI_coords, $
        DO_GEO_MAG_CONVERSIONS=do_GEO_MAG_conversions, $
        DO_AACGM_CONVERSIONS=do_AACGM_conversions, $
        GET_DIPOLETILT_DATA=get_dipoleTilt_data, $
        STITCH_FILES=stitch_files, $
        ORIG_ROUTINENAME=orig_routineName, $
        COORDDIR=coordDir, $
        OUTFILE_PREF=outFile_pref, $
        DRY_RUN=dry_run, $
        CHECK_IF_EXISTS=check_if_exists, $
        DIAGNOSTIC=diag, $
        OK__CONTINUE_WITH_ONLY_FEW_CPUS=OK__low_CPU_number

  ENDFOR

END

