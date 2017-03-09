;;06/04/16
PRO COMBINE_ALL_UPGOING_PARSED_ESPECS

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Running options
  loud                      = 0

  firstOrb                  = 500
  lastOrb                   = 16361


  newFileDateStr            = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  Newell_DB_dir             = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/Newell_batch_output/downgoing_ions_upgoing_electrons/'
  Newell_filePref           = 'Newell_et_al_identification_of_electron_spectra--downgoing_ions_upgoing_electrons--Orbit_'

  outDir                    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'

  orbChunk_save_interval    = 500
  chunkNum                  = 0
  chunkDir                  = outDir+'fully_parsed/'
  chunk_saveFile_pref       = STRING(FORMAT='("eSpec_up_",A0,"_db--PARSED--Orbs_",I0,"-",I0)', $
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
     clock           = TIC(STRING(FORMAT='("combine_all_parsed_especs--Orbs_",I0,"-",I0)',chunkStartOrb,chunkEndOrb))
     WHILE curOrb LE chunkEndOrb DO BEGIN
     ;; WHILE orbCount LE tmp_interval DO BEGIN
        ;;Get events in this orb
        doneski                           = 0
        curInterval                       = 0
        tempFile                          = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)

        IF ~FILE_TEST(tempFile) THEN BEGIN

           doneski                        = 1
           curOrb++
           IF KEYWORD_SET(loud) THEN PRINT,"No data for orbit " + STRCOMPRESS(curOrb,/REMOVE_ALL)
           CONTINUE
        ENDIF
        WHILE ~doneski DO BEGIN

           RESET_ESPEC_RESTOREFILE_VARS,especs_parsed, $
                                        ispec_down, $
                                        jei_down, $
                                        ji_down, $
                                        out_sc_min_energy_ind, $
                                        out_sc_min_energy_ind_i, $
                                        out_sc_pot, $
                                        out_sc_pot_i, $
                                        out_sc_time, $
                                        out_sc_time_i, $
                                        tmpespec_up, $
                                        tmpjee_up, $
                                        tmpje_up

           alt             = !NULL
           mlt             = !NULL
           ilat            = !NULL
           tSort_i         = !NULL
           nEvents         = !NULL

           RESTORE,tempFile

           GET_ALT_MLT_ILAT_FROM_FAST_EPHEM, $
              curOrb, $
              especs_parsed.x, $
              OUT_TSORTED_I=tSort_i, $
              OUT_ALT=alt, $
              OUT_MLT=mlt, $
              OUT_ILAT=ilat, $
              OUT_NEVENTS=nEvents, $
              LOGLUN=logLun

           IF KEYWORD_SET(tSort_i) THEN BEGIN
              eSpecs_parsed = {x:eSpecs_parsed.x[tSort_i], $
                               orbit:MAKE_ARRAY(nEvents,VALUE=curOrb), $
                               mlt:mlt, $
                               ilat:ilat, $
                               alt:alt, $
                               mono:eSpecs_parsed.mono[tSort_i], $
                               broad:eSpecs_parsed.broad[tSort_i], $
                               diffuse:eSpecs_parsed.diffuse[tSort_i], $
                               je:eSpecs_parsed.je[tSort_i], $
                               jee:eSpecs_parsed.jee[tSort_i], $
                               nbad_espec:eSpecs_parsed.nbad_espec[tSort_i]}
           ENDIF ELSE BEGIN
              eSpecs_parsed = {x:eSpecs_parsed.x, $
                               orbit:MAKE_ARRAY(nEvents,VALUE=curOrb), $
                               mlt:mlt, $
                               ilat:ilat, $
                               alt:alt, $
                               mono:eSpecs_parsed.mono, $
                               broad:eSpecs_parsed.broad, $
                               diffuse:eSpecs_parsed.diffuse, $
                               je:eSpecs_parsed.je, $
                               jee:eSpecs_parsed.jee, $
                               nbad_espec:eSpecs_parsed.nbad_espec}

           ENDELSE

           ADD_EVENT_TO_SPECTRAL_STRUCT, $
              eSpecs, $
              eSpecs_parsed, $
              ;; tSort_i, $
              /HAS_ALT_AND_ORBIT

           ;; cur_orbArr      = [cur_orbArr,MAKE_ARRAY(nEvents,VALUE=curOrb)]

           nPredicted                    += nEvents

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
     
     ;; eSpecs           = {x:eSpecs.x, $
     ;;                     orbit:cur_orbArr, $
     ;;                     mlt:eSpecs.mlt, $
     ;;                     ilat:eSpecs.ilat, $

     ;;                     mono:eSpecs.mono, $
     ;;                     broad:eSpecs.broad, $
     ;;                     diffuse:eSpecs.diffuse, $
     ;;                     je:eSpecs.je, $
     ;;                     jee:eSpecs.jee, $
     ;;                     nbad_espec:eSpecs.nbad_espec}

     ;; CREATE_STRUCT(eSpecs,"orbit",cur_orbArr)

     chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I02,"--eSpecs_for_orbs_",I0,"-",I0,".sav")', $
                              chunk_saveFile_pref, $
                              chunkNum++, $
                              chunkStartOrb, $
                              chunkEndOrb)
     PRINT,"Saving " + chunkTempFName + '...'
     SAVE,eSpecs,FILENAME=chunkDir+chunkTempFName

     ;;Check: did we hose it?
     nActual         = N_ELEMENTS(eSpecs.x)
     nTotActual     += nActual
     nTotPredicted  += nPredicted

     ;;Some output
     PRINT,FORMAT='(I0,T12,I0,T24,I0,T36,I0,T48,I0,T60,I0,T72,I0)',chunkStartOrb,chunkEndOrb,nPredicted,nActual,nTotPredicted,nTotActual,orbCount

     IF (nActual NE nPredicted) OR (nTotActual NE nTotPredicted) THEN STOP 

     ;;Now reset loop vars
     eSpecs          = !NULL
     cur_orbArr      = !NULL

     nPredicted      = 0
     nActual         = 0

     orbCount        = 0

  ENDFOR

  PRINT,'N total predicted: ' + STRCOMPRESS(nTotPredicted,/REMOVE_ALL)
  PRINT,'N total actual   : ' + STRCOMPRESS(nTotActual,/REMOVE_ALL)

  TOC

END

;; PRO RESET_ESPEC_RESTOREFILE_VARS,especs_parsed, $
;;                                  ispec_down, $
;;                                  jei_down, $
;;                                  ji_down, $
;;                                  out_sc_min_energy_ind, $
;;                                  out_sc_min_energy_ind_i, $
;;                                  out_sc_pot, $
;;                                  out_sc_pot_i, $
;;                                  out_sc_time, $
;;                                  out_sc_time_i, $
;;                                  tmpespec_up, $
;;                                  tmpjee_up, $
;;                                  tmpje_up

;;   especs_parsed                  = !NULL
;;   ispec_down                     = !NULL
;;   jei_down                       = !NULL
;;   ji_down                        = !NULL
;;   out_sc_min_energy_ind          = !NULL
;;   out_sc_min_energy_ind_i        = !NULL
;;   out_sc_pot                     = !NULL
;;   out_sc_pot_i                   = !NULL
;;   out_sc_time                    = !NULL
;;   out_sc_time_i                  = !NULL
;;   tmpespec_up                    = !NULL
;;   tmpjee_up                      = !NULL
;;   tmpje_up                       = !NULL

;; END