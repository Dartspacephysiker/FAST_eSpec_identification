;;11/24/16
PRO JOURNAL__20161124__TRIM_FIRST_10_POINTS_FROM_EVERY_ORB_INTERVAL__RESAVE_ESPEC

  COMPILE_OPT IDL2

  routineName = 'JOURNAL__20161124__TRIM_FIRST_10_POINTS_FROM_EVERY_ORB_INTERVAL__RESAVE_ESPEC'

  use_2000km_file = 0

  LOAD_NEWELL_ESPEC_DB,eSpec, $
                       /NO_MEMORY_LOAD, $
                       NEWELLDBDIR=NewellDBDir, $
                       /DONT_PERFORM_CORRECTION, $
                       /DONT_CONVERT_TO_STRICT_NEWELL, $
                       /DONT_MAP_TO_100KM, $
                       USE_2000KM_FILE=use_2000km_file

  safedFile = GET_NEWELL_ESPEC_SAFED_INDS_FILE(eSpec, $
                                               NEWELLDBDIR=NewellDBDir, $
                                               /JUSTNAMEPLEASE, $
                                               /QUIET)

  IF FILE_TEST(safedFile) THEN BEGIN
     PRINT,"This safedFile already exists!"
     STOP
  ENDIF

  startOrb = MIN(eSpec.orbit)
  stopOrb  = MAX(eSpec.orbit)

  ;;Assume they are all good
  keepInds = MAKE_ARRAY(N_ELEMENTS(eSpec.orbit),/LONG)
  curKeep  = 0L
  nLostTot = 0L
  clock    = TIC("clocker")
  fileDelta = 1000
  FOR orbit_num=startOrb,stopOrb DO BEGIN

     tmpString = "Orbit " + STRCOMPRESS(orbit_num,/REMOVE_ALL) + ": "

     es_thisOrb_i = WHERE(eSpec.orbit EQ orbit_num,n_es_thisOrb)

     ;;Skip if we have nothing
     IF n_es_thisOrb EQ 0 THEN BEGIN
        PRINT,tmpString + "eSpec DB has no inds! Skipping ..."
        CONTINUE
     ENDIF

     bro       = LOAD_JE_AND_JE_TIMES_FOR_ORB(orbit_num, $
                                              RETURN_STRUCT=return_struct, $
                                              /USE_DUPELESS_FILES, $
                                              JE_OUT=je, $
                                              TIME_RANGES_OUT=time_ranges, $
                                              TIME_RANGE_INDICES_OUT=time_range_indices, $
                                              NINTERVALS_OUT=nInterval, $
                                              OUT_JEFILENAME=jeFileName, $
                                              CLEAN_DUPES=clean_dupes, $
                                              ;; OUT_JEFILEDIR=jeFileDir, $
                                              /QUIET)

     IF bro NE 0 THEN BEGIN
        bro       = LOAD_JE_AND_JE_TIMES_FOR_ORB(orbit_num, $
                                                 RETURN_STRUCT=return_struct, $
                                                 /USE_DUPELESS_FILES, $
                                                 JE_OUT=je, $
                                                 TIME_RANGES_OUT=time_ranges, $
                                                 TIME_RANGE_INDICES_OUT=time_range_indices, $
                                                 NINTERVALS_OUT=nInterval, $
                                                 OUT_JEFILENAME=jeFileName, $
                                                 CLEAN_DUPES=clean_dupes)
     ENDIF
     

     tmpTime    = eSpec.x[es_thisOrb_i]
     claimed_ii = !NULL
     FOR k=0,nInterval-1 DO BEGIN
        tmp_ii = WHERE((tmpTime GE time_ranges[k,0]) AND (tmpTime LE time_ranges[k,1]),nClaimed)
        IF nClaimed GT 0 THEN BEGIN
           claimed_ii = [claimed_ii,tmp_ii]
        ENDIF
     ENDFOR

     nHere  = N_ELEMENTS(claimed_ii)

     IF nHere EQ 0 THEN BEGIN
        PRINT,tmpString,"No good eSpeckers"
        CONTINUE
     ENDIF

     ;;Now update everythang
     keepInds[curKeep:(curKeep+nHere-1)] = es_thisOrb_i[claimed_ii]

     nLost     = n_es_thisOrb - nHere
     curKeep  += nHere
     nLostTot += nLost

     PRINT,FORMAT='(A0,"Got ",I10,", lost ",I10," out of ",I10," (kept ",I10," total, lost ",I10," total)")', $
           tmpString, $
           nHere, $
           nLost, $
           n_es_thisOrb, $
           curKeep, $
           nLostTot

     IF (orbit_num MOD 1000) EQ 0 THEN BEGIN
        TOC,clock
     ENDIF

  ENDFOR

  cleaned_eSpec_i = (TEMPORARY(keepInds))[0:curKeep-1]

  eSpec_clean_info = {date                : GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                      originating_routine : routineName, $
                      db_info             : eSpec.info, $
                      totChecked          : N_ELEMENTS(eSpec.orbit), $
                      totKept             : curKeep, $
                      totLost             : nLostTot, $
                      startOrb            : startOrb, $
                      stopOrb             : stopOrb}          ;, $
                
  PRINT,"Saving cleaned_eSpec_i and info to " + safedFile + ' ...'
  SAVE,cleaned_eSpec_i,eSpec_clean_info,FILENAME=safedFile

END
