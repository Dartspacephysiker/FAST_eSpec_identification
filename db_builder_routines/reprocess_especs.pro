;;06/03/16
PRO REPROCESS_ESPECS

  COMPILE_OPT IDL2

  quiet                     = 1

  firstOrb                  = 500
  lastOrb                   = 16361

  newFileDateStr            = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  Newell_DB_dir                        = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/Newell_batch_output/'
  Newell_filePref                      = 'Newell_et_al_identification_of_electron_spectra--ions_included--Orbit_'

  outDir                     = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'

  orbChunk_save_interval    = 500
  chunkDir                  = outDir+'fully_parsed/'
  chunk_saveFile_pref       = STRING(FORMAT='("eSpec_",A0,"_db--PARSED--Orbs_",I0,"-",I0)', $
                                     newFileDateStr, $
                                     firstOrb, $
                                     lastOrb)

  ;;String together a chunk of orbits, reanalyze, and save
  FOR curOrb=firstOrb,lastOrb DO BEGIN


     chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I0,"--eSpecs_",A0,I0,"-",I0,".sav")',chunk_saveFile_pref,chunkNum,failCodes_string, $
                              totNChunksSaved+1,nTotPredicted)
     chunkStartOrb   = curOrb
     orbCount        = 0
     WHILE orbCount LT orbChunk_save_interval DO BEGIN
        ;;Get events in this orb
        doneski                           = 0
        curInterval                       = 0
        tempFile                          = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)

        IF ~FILE_TEST(tempFile) THEN BEGIN

           doneski                        = 1
           orbCount++
           CONTINUE
        ENDIF
        WHILE ~doneski DO BEGIN

           RESET_ESPEC_RESTOREFILE_VARS,especs_parsed, $
                                        ispec_up, $
                                        jei_up, $
                                        ji_up, $
                                        out_sc_min_energy_ind, $
                                        out_sc_min_energy_ind_i, $
                                        out_sc_pot, $
                                        out_sc_pot_i, $
                                        out_sc_time, $
                                        out_sc_time_i, $
                                        tmpespec_lc, $
                                        tmpjee_lc, $
                                        tmpje_lc
           RESTORE,tempFile

           ;;Check for next interval
           curInterval++
           tempFile = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)
           IF ~FILE_TEST(tempFile) THEN doneski  = 1
        ENDWHILE

        orbCount++
     ENDWHILE
     curOrb       += orbCount
     chunkEndOrb   = curOrb

     ;;Handle this batch
     CHECK_DIFF_EFLUX_INPUTS_BEFORE_BEGINNING,eSpecs,jee,je,mlt,ilat

     IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,eSpec,Jee,Je, $
                                             mlt,ilat, $
                                             events_final, $
                                             SC_POT=sc_pot, $
                                             IND_SC_POT=ind_sc_pot, $
                                             ORBSTR=orbStr, $
                                             PRODUCE_FAILCODE_OUTPUT=produce_failCodes, $
                                             OUT_FAILCODES=failCodes, $
                                             /GIVE_TIMESPLIT_INFO, $
                                             QUIET=quiet, $
                                             /BATCH_MODE, $
                                             ERRORLOGFILE=errorLogFile
  ENDFOR

  ;;Give 'er a check

  ;;Now give everything a second run-through
  IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,eSpecs,jee_out,je_out, $
                                          alf_mlt,alf_ilat, $
                                          eSpecs_parsed, $
                                          SC_POT=alf_sc_pot, $
                                          ;; IND_SC_POT=ind_sc_pot, $
                                          ;; ORBSTR=orbStr, $
                                          /PRODUCE_FAILCODE_OUTPUT, $
                                          OUT_FAILCODES=failCodes, $
                                          /GIVE_TIMESPLIT_INFO, $
                                          QUIET=quiet, $
                                          /BATCH_MODE, $
                                          ERRORLOGFILE=errorLogFile


  IF ~KEYWORD_SET(save_chunks_for_speed) THEN BEGIN
     PRINT,'Saving parsed output to ' + eSpecParsedFile + '...'
     SAVE,eSpecs_parsed,failCodes,FILENAME=inDir+eSpecParsedFile
  ENDIF

END

PRO RESET_ESPEC_RESTOREFILE_VARS,especs_parsed, $
                                 ispec_up, $
                                 jei_up, $
                                 ji_up, $
                                 out_sc_min_energy_ind, $
                                 out_sc_min_energy_ind_i, $
                                 out_sc_pot, $
                                 out_sc_pot_i, $
                                 out_sc_time, $
                                 out_sc_time_i, $
                                 tmpespec_lc, $
                                 tmpjee_lc, $
                                 tmpje_lc

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

END