PRO EXTRACT_TIME_SERIES_FROM__NEWELL_FILES,firstOrb,lastOrb,missingOrbArr, $
                                 Newell_DB_dir,Newell_filePref, $
                                 OUT_TS=out_ts, $
                                 OUT_ORBARR=out_orbArr, $
                                 LUN=lun

  IF N_ELEMENTS(lun) NE 0 THEN outLun = lun ELSE outLun = -1

  outOrbArr          = !NULL
  espec_times_innie  = !NULL
  orbArr_innie       = !NULL
  espec_times_final  = !NULL
  orbArr_final       = !NULL
  maxChain           = 200
  orbCount           = 0
  FOR curOrb=firstOrb,lastOrb DO BEGIN

     PRINT,curOrb

     ;;First, cat all eSpecs from each interval for this orbit
     doneski         = 0
     curInterval     = 0
     cur_eSpec_times = !NULL
     cur_orbArr      = !NULL
     tempFile        = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)
     IF ~FILE_TEST(tempFile) THEN BEGIN

        doneski      = 1

        CONTINUE
     ENDIF
     WHILE ~doneski DO BEGIN
        RESTORE,tempFile
        
        cur_espec_times = [cur_espec_times,tmpespec_lc.x]
        cur_orbarr      = [cur_orbarr,MAKE_ARRAY(N_ELEMENTS(tmpespec_lc.x),VALUE=curOrb)]
        
        ;;Check for next interval
        curInterval++
        tempFile     = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)
        IF ~FILE_TEST(tempFile) THEN doneski  = 1
     ENDWHILE

     espec_times_innie = [espec_times_innie,cur_espec_times]  
     orbArr_innie      = [orbArr_innie,cur_orbArr]
     orbCount++

     IF orbCount GE maxChain THEN BEGIN
        espec_times_final = [espec_times_final,espec_times_innie]
        orbArr_final      = [orbArr_final,orbArr_innie]

        espec_times_innie = !NULL
        orbArr_innie      = !NULL
        orbCount          = 0
     ENDIF

  ENDFOR

  IF orbCount GE 0 THEN BEGIN
        espec_times_final = [espec_times_final,espec_times_innie]
        orbArr_final      = [orbArr_final,orbArr_innie]

        espec_times_innie = !NULL
        orbArr_innie      = !NULL
        orbCount          = 0
     ENDIF

  out_orbArr              = TEMPORARY(orbArr_final)
  out_ts                  = TEMPORARY(espec_times_final)


END