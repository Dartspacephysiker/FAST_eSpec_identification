;2017/12/05
PRO PROCESS_MCFADDEN_STYLE_ESA_STRUCTS

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Running options
  loud                      = 0

  firstOrb                  = 1532
  lastOrb                   = 50000


  newFileDateStr            = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  CASE 1 OF
     ;; KEYWORD_SET(upgoing): BEGIN
     ;;    Newell_DB_dir       = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/Newell_batch_output/downgoing_ions_upgoing_electrons/'
     ;;    Newell_filePref     = 'Newell_et_al_identification_of_electron_spectra--downgoing_ions_upgoing_electrons--Orbit_'
     ;;    pref                = "eSpec_up_"
     ;; END
     ELSE: BEGIN
        Newell_DB_dir       = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/batch_output__just_electrons_v2/'
        Newell_filePref     = 'Dartdb--e-_spectra__all_angles_energies_v4__justElec_v2--ees--Orbit_'
        pref                = "eSpec_"
     END
  ENDCASE

  suff = '.sav'

  outDir                    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/katusOrbs/'


  orbChunk_save_interval    = 5000
  chunkNum                  = 0
  chunkDir                  = outDir
  chunk_saveFile_pref       = STRING(FORMAT='(A0,A0,"_db--PARSED--Orbs_",I0,"-",I0)', $
                                     pref, $
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
     chunkMapRatio   = !NULL
     clock           = TIC(STRING(FORMAT='("combine_all_McFadden_especs--Orbs_",I0,"-",I0)',chunkStartOrb,chunkEndOrb))
     WHILE curOrb LE chunkEndOrb DO BEGIN
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

           alt             = !NULL
           mlt             = !NULL
           ilat            = !NULL
           mapRatio        = !NULL
           tSort_i         = !NULL
           nEvents         = !NULL

           RESTORE,tempFile
           check_sorted,diff_eflux.time,is_sorted,/QUIET
           IF ~is_sorted THEN BEGIN
              PRINT,"Not sorted!!!!"
              STOP
           ENDIF

           STOP

           IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,tmpeSpec_lc,tmpjee_lc,tmpje_lc, $
                                                   mlt,ilat,alt,orbit, $
                                                   eSpecs_parsed, $
                                                   SC_POT=out_sc_pot, $ ;The reason for no negative is that the sign gets flipped get_2d_ts_pot
                                                   /QUIET, $
                                                   BATCH_MODE=batch_mode, $
                                                   ORBSTR=orbStr, $
                                                   ERRORLOGFILE=badFile

           mag1      = (diff_eFlux[*].B_model[0]*diff_eFlux[*].B_model[0]+ $
                        diff_eFlux[*].B_model[1]*diff_eFlux[*].B_model[1]+ $
                        diff_eFlux[*].B_model[2]*diff_eFlux[*].B_model[2])^0.5
           mag2      = (diff_eFlux[*].B_foot[0]*diff_eFlux[*].B_foot[0]+ $
                        diff_eFlux[*].B_foot[1]*diff_eFlux[*].B_foot[1]+ $
                        diff_eFlux[*].B_foot[2]*diff_eFlux[*].B_foot[2])^0.5
           mapRatio  = TEMPORARY(mag2)/TEMPORARY(mag1)
           lcw       = ATAN(SQRT(1.D/(mapRatio-1.D)))*180./!PI

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
           chunkMapRatio = [chunkMapRatio,mapRatio]

           ADD_EVENT_TO_SPECTRAL_STRUCT,eSpecs,eSpecs_parsed,/HAS_ALT_AND_ORBIT
           PRINT,"N_ELEMENTS(eSpecs.x):",N_ELEMENTS(eSpecs.x)
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
     
     chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I02,"--eSpecs_for_orbs_",I0,"-",I0,".sav")', $
                              chunk_saveFile_pref, $
                              chunkNum++, $
                              chunkStartOrb, $
                              chunkEndOrb)
     PRINT,"Saving " + chunkTempFName + '...'
     SAVE,eSpecs,chunkMapRatio,FILENAME=chunkDir+chunkTempFName

     ;;Check: did we hose it?
     nActual         = N_ELEMENTS(eSpecs.x)
     nTotActual     += nActual
     nTotPredicted  += nPredicted

     ;;Some output
     PRINT,FORMAT='(I0,T12,I0,T24,I0,T36,I0,T48,I0,T60,I0,T72,I0)',chunkStartOrb,chunkEndOrb, $
           nPredicted,nActual, $
           nTotPredicted,nTotActual, $
           orbCount

     IF (nActual NE nPredicted) OR (nTotActual NE nTotPredicted) THEN STOP 

     IF N_ELEMENTS(chunkMapRatio) NE nActual THEN STOP

     ;;Now reset loop vars
     eSpecs          = !NULL
     chunkMapRatio   = !NULL
     cur_orbArr      = !NULL

     nPredicted      = 0
     nActual         = 0

     orbCount        = 0

  ENDFOR

END
