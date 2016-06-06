PRO EXTRACT_TIME_SERIES_FROM__NEWELL_FILES,firstOrb,lastOrb,missingOrbArr, $
   Newell_DB_dir,Newell_filePref, $
   OUT_TS=out_ts, $
   OUT_ORBARR=out_orbArr, $
   DO_IONS=do_ions, $
   LUN=lun

  IF KEYWORD_SET(do_ions) THEN PRINT,'Doing ions!'

  IF N_ELEMENTS(lun) NE 0 THEN outLun = lun ELSE outLun = -1

  outOrbArr          = !NULL
  spec_times_innie   = !NULL
  orbArr_innie       = !NULL
  spec_times_final   = !NULL
  orbArr_final       = !NULL
  maxChain           = 200
  orbCount           = 0
  FOR curOrb=firstOrb,lastOrb DO BEGIN

     PRINT,curOrb

     ;;First, cat all specs from each interval for this orbit
     doneski         = 0
     curInterval     = 0
     cur_spec_times  = !NULL
     cur_orbArr      = !NULL
     tempFile        = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)
     IF ~FILE_TEST(tempFile) THEN BEGIN

        doneski      = 1

        CONTINUE
     ENDIF
     WHILE ~doneski DO BEGIN
        RESTORE,tempFile
        
        specTimes       = (KEYWORD_SET(do_ions) ? ispec_up.x : tmpespec_lc.x)

        cur_spec_times  = [cur_spec_times,specTimes]
        cur_orbarr      = [cur_orbarr,MAKE_ARRAY(N_ELEMENTS(specTimes),VALUE=curOrb)]
        
        ;;Check for next interval
        curInterval++
        tempFile     = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)
        IF ~FILE_TEST(tempFile) THEN doneski  = 1
     ENDWHILE

     spec_times_innie  = [spec_times_innie,cur_spec_times]  
     orbArr_innie      = [orbArr_innie,cur_orbArr]
     orbCount++

     IF orbCount GE maxChain THEN BEGIN
        spec_times_final  = [spec_times_final,spec_times_innie]
        orbArr_final      = [orbArr_final,orbArr_innie]

        spec_times_innie  = !NULL
        orbArr_innie      = !NULL
        orbCount          = 0
     ENDIF

  ENDFOR

  IF orbCount GE 0 THEN BEGIN
        spec_times_final  = [spec_times_final,spec_times_innie]
        orbArr_final      = [orbArr_final,orbArr_innie]

        spec_times_innie  = !NULL
        orbArr_innie      = !NULL
        orbCount          = 0
     ENDIF

  out_orbArr              = TEMPORARY(orbArr_final)
  out_ts                  = TEMPORARY(spec_times_final)


END