;;06/06/16
PRO COMBINE_ALL_ISPECS_JI_JEI

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Running options
  loud                      = 1
  add_fastloc_interped_alt  = 1

  firstOrb                  = 1000
  lastOrb                   = 16361


  newFileDateStr            = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  Newell_DB_dir             = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/Newell_batch_output/'
  Newell_filePref           = 'Newell_et_al_identification_of_electron_spectra--ions_included--Orbit_'

  outDir                    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'

  orbChunk_save_interval    = 500
  chunkNum                  = 0
  chunkDir                  = outDir+'fully_parsed/'
  chunk_saveFile_pref       = STRING(FORMAT='("iSpec_",A0,"_db--PARSED--Orbs_",I0,"-",I0)', $
                                     newFileDateStr, $
                                     firstOrb, $
                                     lastOrb)

  ;;String together a chunk of orbits, reanalyze, and save
  PRINT,FORMAT='("Start orb",T12,"Stop orb",T24,"N Predicted",T36,"N Actual",T48,"NT Predicted",T60,"NT Actual",T72,"N Orbs this chunk")'
  cur_orbArr                = !NULL
  orbCount                  = 0
  nPredicted                = 0
  nActual                   = 0
  nTotPredicted             = 0
  nTotActual                = 0
  TIC
  FOR curOrb=firstOrb,lastOrb DO BEGIN


     chunkStartOrb   = curOrb
     chunkEndOrb     = (curOrb + orbChunk_save_interval-1) < lastOrb
     ;; tmp_interval    = chunkEndOrb-chunkStartOrb
     clock           = TIC(STRING(FORMAT='("combine_all_iSpecs_ji_jei--Orbs_",I0,"-",I0)',chunkStartOrb,chunkEndOrb))
     WHILE curOrb LE chunkEndOrb DO BEGIN
     ;; WHILE orbCount LE tmp_interval DO BEGIN
        ;;Get events in this orb
        doneski                           = 0
        curInterval                       = 0
        tempFile                          = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)

        IF ~FILE_TEST(tempFile) THEN BEGIN
           doneski                        = 1
           IF KEYWORD_SET(loud) THEN PRINT,"No data for orbit " + STRCOMPRESS(curOrb,/REMOVE_ALL)

           curOrb++
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
           alt             = !NULL
           mlt             = !NULL
           ilat            = !NULL

           RESTORE,tempFile


           GET_ALT_MLT_ILAT_FROM_FAST_EPHEM,curOrb,ji_up.x, $
                                     OUT_TSORTED_I=tSort_i, $
                                     OUT_ALT=alt, $
                                     OUT_MLT=mlt, $
                                     OUT_ILAT=ilat, $
                                     LOGLUN=logLun

           nEvents         = N_ELEMENTS(ji_up.x)
           IF (nEvents NE N_ELEMENTS(jei_up.x)) THEN STOP

           CAT_JI_AND_JEI_FROM_NEWELL_FILES_INTO_STRUCT,ions,ji_up,jei_up,MAKE_ARRAY(nEvents,VALUE=curOrb), $
              alt,mlt,ilat

           ;; cur_orbArr      = [cur_orbArr,MAKE_ARRAY(nEvents,VALUE=curOrb)]

           nPredicted     += nEvents

           ;;Check for next interval
           curInterval++
           tempFile = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)
           IF ~FILE_TEST(tempFile) THEN doneski  = 1
        ENDWHILE

        orbCount++
        curOrb++
     ENDWHILE
     curOrb-- ;Fix the damage--trust me
     TOC,clock
     
     ;; iSpecs           = {x:eSpecs.x, $
     ;;                     orbit:cur_orbArr, $
     ;;                     mlt:eSpecs.mlt, $
     ;;                     ilat:eSpecs.ilat, $
     ;;                     mono:eSpecs.mono, $
     ;;                     broad:eSpecs.broad, $
     ;;                     diffuse:eSpecs.diffuse, $
     ;;                     je:eSpecs.je, $
     ;;                     jee:eSpecs.jee, $
     ;;                     nbad_espec:eSpecs.nbad_espec}

     ;; ji_orbits       = cur_orbArr


     ;; CREATE_STRUCT(eSpecs,"orbit",cur_orbArr)

     chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I02,"--iSpecs_for_orbs_",I0,"-",I0,".sav")', $
                              chunk_saveFile_pref, $
                              chunkNum++, $
                              chunkStartOrb, $
                              chunkEndOrb)
     PRINT,"Saving " + chunkTempFName + '...'
     ;; SAVE,ji_up_final,jei_up_final,FILENAME=chunkDir+chunkTempFName
     SAVE,ions,FILENAME=chunkDir+chunkTempFName

     ;;Check: did we hose it?
     nActual         = N_ELEMENTS(ions.x)
     nTotActual     += nActual
     nTotPredicted  += nPredicted

     ;;Some output
     PRINT,FORMAT='(I0,T12,I0,T24,I0,T36,I0,T48,I0,T60,I0,T72,I0)',chunkStartOrb,chunkEndOrb,nPredicted,nActual,nTotPredicted,nTotActual,orbCount

     ;;Now reset loop vars
     ji_up_final     = !NULL
     jei_up_final    = !NULL
     ions            = !NULL
     ;; cur_orbArr      = !NULL

     nPredicted      = 0
     nActual         = 0

     orbCount        = 0

  ENDFOR

  PRINT,'N total predicted: ' + STRCOMPRESS(nTotPredicted,/REMOVE_ALL)
  PRINT,'N total actual   : ' + STRCOMPRESS(nTotActual,/REMOVE_ALL)

  TOC

END
