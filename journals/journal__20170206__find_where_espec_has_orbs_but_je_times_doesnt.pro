;;02/07/17
PRO JOURNAL__20170206__FIND_WHERE_ESPEC_HAS_ORBS_BUT_JE_TIMES_DOESNT

  COMPILE_OPT IDL2,STRICTARRSUBS

  startOrb = 24500

  LOAD_NEWELL_ESPEC_DB,eSpec, $
                       /NO_MEMORY_LOAD, $
                       NEWELLDBDIR=NewellDBDir, $
                       /DONT_PERFORM_CORRECTION, $
                       /DONT_CONVERT_TO_STRICT_NEWELL, $
                       /DONT_MAP_TO_100KM, $
                       USE_2000KM_FILE=use_2000km_file

  orbits   = (TEMPORARY(eSpec)).orbit

  startOrb = KEYWORD_SET(startOrb) ? startOrb : MIN(orbits)
  stopOrb  = KEYWORD_SET(stopOrb ) ? stopOrb  : MAX(orbits)

  clock    = TIC("clocker")
  badOrbs  = !NULL
  FOR orbit_num=startOrb,stopOrb DO BEGIN

     tmpString = "Orbit " + STRCOMPRESS(orbit_num,/REMOVE_ALL) + ": "

     es_thisOrb_i = WHERE(orbits EQ orbit_num,n_es_thisOrb)

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
        badOrbs = [badOrbs,orbit_num]
     ENDIF
     
     PRINT,orbit_num,(bro NE 0)

     IF (orbit_num MOD 1000) EQ 0 THEN BEGIN
        TOC,clock
     ENDIF

  ENDFOR

  PRINT,badOrbs

  STOP

END
