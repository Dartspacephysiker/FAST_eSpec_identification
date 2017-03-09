;;06/03/16
PRO CHECK_RAW_ESPEC_FILES_FOR_COMPLETENESS

  COMPILE_OPT IDL2,STRICTARRSUBS

  firstOrb                             = 500
  lastOrb                              = 16361

  todayStr                             = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  logDir                               = '/SPENCEdata/Research/Satellites/FAST/espec_identification/txt_log_etc/'
  logFile                              = STRING(FORMAT='("eSpecs--checks_for_completeness--Orbs_",I0,"-",I0,"--",A0,".log")', $
                                                firstOrb,lastOrb,todayStr)
  missingFile                          = STRING(FORMAT='("eSpecs--checks_for_completeness--MISSING_ORBITS--Orbs_",I0,"-",I0,"--",A0,".txt")', $
                                                firstOrb,lastOrb,todayStr)
  saveFile                             = STRING(FORMAT='("eSpecs--checks_for_completeness--ARRAYS_OF_MISSING_DATA--Orbs_",I0,"-",I0,"--",A0,".sav")', $
                                                firstOrb,lastOrb,todayStr)

  Newell_DB_dir                        = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/Newell_batch_output/'
  Newell_filePref                      = 'Newell_et_al_identification_of_electron_spectra--ions_included--Orbit_'

  ;;Open the heavens
  PRINT,"Opening " + logFile + '...'
  PRINT,"Opening " + missingFile + '...'
  OPENW,logLun,logDir+logFile,/GET_LUN
  OPENW,missingLun,logDir+missingFile,/GET_LUN

  ;;"Declare" arrays and counters
  missing_especs_parsed_arr            = !NULL
  missing_ispec_up_arr                 = !NULL
  missing_jei_up_arr                   = !NULL
  missing_ji_up_arr                    = !NULL
  missing_out_sc_min_energy_ind_arr    = !NULL
  missing_out_sc_min_energy_ind_i_arr  = !NULL
  missing_out_sc_pot_arr               = !NULL
  missing_out_sc_pot_i_arr             = !NULL
  missing_out_sc_time_arr              = !NULL
  missing_out_sc_time_i_arr            = !NULL
  missing_tmpespec_lc_arr              = !NULL
  missing_tmpjee_lc_arr                = !NULL
  missing_tmpje_lc_arr                 = !NULL
  missingOrbArr                        = !NULL

  nMissing_especs_parsed               = 0
  nMissing_ispec_up                    = 0
  nMissing_jei_up                      = 0
  nMissing_ji_up                       = 0
  nMissing_out_sc_min_energy_ind       = 0
  nMissing_out_sc_min_energy_ind_i     = 0
  nMissing_out_sc_pot                  = 0
  nMissing_out_sc_pot_i                = 0
  nMissing_out_sc_time                 = 0
  nMissing_out_sc_time_i               = 0
  nMissing_tmpespec_lc                 = 0
  nMissing_tmpjee_lc                   = 0
  nMissing_tmpje_lc                    = 0

  FOR curOrb=firstOrb,lastOrb DO BEGIN

     PRINT,curOrb

     ;;Get events in this orb
     doneski                           = 0
     curInterval                       = 0
     tempFile                          = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)

     IF ~FILE_TEST(tempFile) THEN BEGIN

        doneski                        = 1

        missingOrbArr                  = [missingOrbArr,curOrb]

        missingString                  = STRING(FORMAT='("MISSING ORBIT: ",I0)',curOrb)
        PRINT,missingString
        PRINTF,missingLun,missingString
        CONTINUE
     ENDIF
     WHILE ~doneski DO BEGIN


        ;;Reset so that we don't get spurious information when the file is restored
        especs_parsed                  = !NULL
        ispec_up                       = !NULL
        jei_up                         = !NULL
        ji_up                          = !NULL
        out_sc_min_energy_ind          = !NULL
        out_sc_min_energy_ind_i        = !NULL
        out_sc_pot                     = !NULL
        out_sc_pot_i                   = !NULL
        out_sc_time                    = !NULL
        out_sc_time_i                  = !NULL
        tmpespec_lc                    = !NULL
        tmpjee_lc                      = !NULL
        tmpje_lc                       = !NULL

        RESTORE,tempFile


        nEvents                        = N_ELEMENTS(tmpespec_lc)

        IF N_ELEMENTS(especs_parsed) EQ 0 THEN BEGIN
           nMissing_especs_parsed++
           missing_especs_parsed_arr          = [missing_especs_parsed_arr,curOrb]
           got_especs_parsed                  = 0
        ENDIF ELSE BEGIN
           got_especs_parsed                  = 1
        ENDELSE

        IF N_ELEMENTS(ispec_up) EQ 0 THEN BEGIN
           nMissing_ispec_up++
           missing_ispec_up_arr               = [missing_ispec_up_arr,curOrb]
           got_ispec_up                       = 0
        ENDIF ELSE BEGIN
           got_ispec_up                       = 1
        ENDELSE

        IF N_ELEMENTS(jei_up) EQ 0 THEN BEGIN
           nMissing_jei_up++
           missing_jei_up_arr                 = [missing_jei_up_arr,curOrb]
           got_jei_up                         = 0
        ENDIF ELSE BEGIN
           got_jei_up                         = 1
        ENDELSE

        IF N_ELEMENTS(ji_up) EQ 0 THEN BEGIN
           nMissing_ji_up++
           missing_ji_up_arr                  = [missing_ji_up_arr,curOrb]
           got_ji_up                          = 0
        ENDIF ELSE BEGIN
           got_ji_up                          = 1
        ENDELSE

        IF N_ELEMENTS(out_sc_min_energy_ind) EQ 0 THEN BEGIN
           nMissing_out_sc_min_energy_ind++
           missing_out_sc_min_energy_ind_arr  = [missing_out_sc_min_energy_ind_arr,curOrb]
           got_out_sc_min_energy_ind          = 0
        ENDIF ELSE BEGIN
           got_out_sc_min_energy_ind          = 1
        ENDELSE

        IF N_ELEMENTS(out_sc_min_energy_ind_i) EQ 0 THEN BEGIN
           nMissing_out_sc_min_energy_ind_i++
           missing_out_sc_min_energy_ind_i_arr  = [missing_out_sc_min_energy_ind_i_arr,curOrb]
           got_out_sc_min_energy_ind_i          = 0
        ENDIF ELSE BEGIN
           got_out_sc_min_energy_ind_i          = 1
        ENDELSE

        IF N_ELEMENTS(out_sc_pot) EQ 0 THEN BEGIN
           nMissing_out_sc_pot++
           missing_out_sc_pot_arr     = [missing_out_sc_pot_arr,curOrb]
           got_out_sc_pot             = 0
        ENDIF ELSE BEGIN
           got_out_sc_pot             = 1
        ENDELSE

        IF N_ELEMENTS(out_sc_pot_i) EQ 0 THEN BEGIN
           nMissing_out_sc_pot_i++
           missing_out_sc_pot_i_arr   = [missing_out_sc_pot_i_arr,curOrb]
           got_out_sc_pot_i           = 0
        ENDIF ELSE BEGIN
           got_out_sc_pot_i           = 1
        ENDELSE

        IF N_ELEMENTS(out_sc_time) EQ 0 THEN BEGIN
           nMissing_out_sc_time++
           missing_out_sc_time_arr    = [missing_out_sc_time_arr,curOrb]
           got_out_sc_time            = 0
        ENDIF ELSE BEGIN
           got_out_sc_time            = 1
        ENDELSE

        IF N_ELEMENTS(out_sc_time_i) EQ 0 THEN BEGIN
           nMissing_out_sc_time_i++
           missing_out_sc_time_i_arr  = [missing_out_sc_time_i_arr,curOrb]
           got_out_sc_time_i          = 0
        ENDIF ELSE BEGIN
           got_out_sc_time_i          = 1
        ENDELSE

        IF N_ELEMENTS(tmpespec_lc) EQ 0 THEN BEGIN
           nMissing_tmpespec_lc++
           missing_tmpespec_lc_arr    = [missing_tmpespec_lc_arr,curOrb]
           got_tmpespec_lc            = 0
        ENDIF ELSE BEGIN
           got_tmpespec_lc            = 1
        ENDELSE

        IF N_ELEMENTS(tmpjee_lc) EQ 0 THEN BEGIN
           nMissing_tmpjee_lc++
           missing_tmpjee_lc_arr      = [missing_tmpjee_lc_arr,curOrb]
           got_tmpjee_lc              = 0
        ENDIF ELSE BEGIN
           got_tmpjee_lc              = 1
        ENDELSE

        IF N_ELEMENTS(tmpje_lc) EQ 0 THEN BEGIN
           nMissing_tmpje_lc++
           missing_tmpje_lc_arr       = [missing_tmpje_lc_arr,curOrb]
           got_tmpje_lc               = 0
        ENDIF ELSE BEGIN
           got_tmpje_lc               = 1
        ENDELSE


        diag_string = STRING(FORMAT='(I7,T9,I3,T14,I7,T23,I2,TR2,I2,TR2,I2,TR2,I2,TR2,I2,TR2,I2,TR2,I2,TR2,I2,TR2,I2,TR2,I2,TR2,I2,TR2,I2,TR2,I2)', $
                             curOrb, $
                             curInterval, $
                             nEvents, $
                             got_especs_parsed, $
                             got_ispec_up, $
                             got_jei_up, $
                             got_ji_up, $
                             got_out_sc_min_energy_ind, $
                             got_out_sc_min_energy_ind_i, $
                             got_out_sc_pot, $
                             got_out_sc_pot_i, $
                             got_out_sc_time, $
                             got_out_sc_time_i, $
                             got_tmpespec_lc, $
                             got_tmpjee_lc, $
                             got_tmpje_lc)


        PRINTF,logLun,diag_string

        ;;Check for next interval
        curInterval++
        tempFile = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)
        IF ~FILE_TEST(tempFile) THEN doneski  = 1
     ENDWHILE

  ENDFOR

  ;;Text summaries
  PRINT,'----------------------------------------'
  PRINT,'               SUMMARY                  '
  PRINT,'----------------------------------------'
  PRINT,''
  PRINT,FORMAT='("N Missing especs_parsed",T30,": ",I0)',nMissing_especs_parsed
  PRINT,FORMAT='("N Missing ispec_up",T30,": ",I0)',nMissing_ispec_up
  PRINT,FORMAT='("N Missing jei_up",T30,": ",I0)',nMissing_jei_up
  PRINT,FORMAT='("N Missing ji_up",T30,": ",I0)',nMissing_ji_up
  PRINT,FORMAT='("N Missing out_sc_min_energy_ind",T30,": ",I0)',nMissing_out_sc_min_energy_ind
  PRINT,FORMAT='("N Missing out_sc_min_energy_ind_i",T30,": ",I0)',nMissing_out_sc_min_energy_ind_i
  PRINT,FORMAT='("N Missing out_sc_pot",T30,": ",I0)',nMissing_out_sc_pot
  PRINT,FORMAT='("N Missing out_sc_pot_i",T30,": ",I0)',nMissing_out_sc_pot_i
  PRINT,FORMAT='("N Missing out_sc_time",T30,": ",I0)',nMissing_out_sc_time
  PRINT,FORMAT='("N Missing out_sc_time_i",T30,": ",I0)',nMissing_out_sc_time_i
  PRINT,FORMAT='("N Missing tmpespec_lc",T30,": ",I0)',nMissing_tmpespec_lc
  PRINT,FORMAT='("N Missing tmpjee_lc",T30,": ",I0)',nMissing_tmpjee_lc
  PRINT,FORMAT='("N Missing tmpje_lc",T30,": ",I0)',nMissing_tmpje_lc

  PRINTF,logLun,'----------------------------------------'
  PRINTF,logLun,'               SUMMARY                  '
  PRINTF,logLun,'----------------------------------------'
  PRINTF,logLun,''
  PRINTF,logLun,FORMAT='("N Missing especs_parsed",T30,": ",I0)',nMissing_especs_parsed
  PRINTF,logLun,FORMAT='("N Missing ispec_up",T30,": ",I0)',nMissing_ispec_up
  PRINTF,logLun,FORMAT='("N Missing jei_up",T30,": ",I0)',nMissing_jei_up
  PRINTF,logLun,FORMAT='("N Missing ji_up",T30,": ",I0)',nMissing_ji_up
  PRINTF,logLun,FORMAT='("N Missing out_sc_min_energy_ind",T30,": ",I0)',nMissing_out_sc_min_energy_ind
  PRINTF,logLun,FORMAT='("N Missing out_sc_min_energy_ind_i",T30,": ",I0)',nMissing_out_sc_min_energy_ind_i
  PRINTF,logLun,FORMAT='("N Missing out_sc_pot",T30,": ",I0)',nMissing_out_sc_pot
  PRINTF,logLun,FORMAT='("N Missing out_sc_pot_i",T30,": ",I0)',nMissing_out_sc_pot_i
  PRINTF,logLun,FORMAT='("N Missing out_sc_time",T30,": ",I0)',nMissing_out_sc_time
  PRINTF,logLun,FORMAT='("N Missing out_sc_time_i",T30,": ",I0)',nMissing_out_sc_time_i
  PRINTF,logLun,FORMAT='("N Missing tmpespec_lc",T30,": ",I0)',nMissing_tmpespec_lc
  PRINTF,logLun,FORMAT='("N Missing tmpjee_lc",T30,": ",I0)',nMissing_tmpjee_lc
  PRINTF,logLun,FORMAT='("N Missing tmpje_lc",T30,": ",I0)',nMissing_tmpje_lc
  

  ;;Save all output
  PRINTF,logLun,"Saving " + saveFile + '...'
  SAVE, $
     nMissing_especs_parsed, $             
     nMissing_ispec_up, $                  
     nMissing_jei_up, $                    
     nMissing_ji_up, $                     
     nMissing_out_sc_min_energy_ind, $     
     nMissing_out_sc_min_energy_ind_i, $   
     nMissing_out_sc_pot, $                
     nMissing_out_sc_pot_i, $              
     nMissing_out_sc_time, $               
     nMissing_out_sc_time_i, $             
     nMissing_tmpespec_lc, $               
     nMissing_tmpjee_lc, $                 
     nMissing_tmpje_lc, $                  
     missing_especs_parsed_arr, $
     missing_ispec_up_arr, $
     missing_jei_up_arr, $
     missing_ji_up_arr, $
     missing_out_sc_min_energy_ind_arr, $
     missing_out_sc_min_energy_ind_i_arr, $
     missing_out_sc_pot_arr, $
     missing_out_sc_pot_i_arr, $
     missing_out_sc_time_arr, $
     missing_out_sc_time_i_arr, $
     missing_tmpespec_lc_arr, $
     missing_tmpjee_lc_arr, $
     missing_tmpje_lc_arr, $
     FILENAME=logDir+saveFile

  ;;Close the heavens
  PRINT,"Closing " + logFile + '...'
  PRINT,"Closing " + missingFile + '...'
  CLOSE,logLun
  CLOSE,missingLun

  FREE_LUN,logLun
  FREE_LUN,missingLun


END
