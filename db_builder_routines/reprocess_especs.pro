;;06/04/16
PRO REPROCESS_ESPECS

  COMPILE_OPT IDL2

  ;;Running options
  loud                      = 1

  firstOrb                  = 500
  lastOrb                   = 24634

  check_for_existing        = 1

  ;; newFileDateStr            = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  newFileDateStr            = '20170209'

  Newell_DB_dir             = '/SPENCEdata/software/sdt/batch_jobs/saves_output_etc/do_the_Newell_2009/Newell_batch_output/ions_included/'
  Newell_filePref           = 'Newell_et_al_identification_of_electron_spectra--ions_included--Orbit_'

  outDir                    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'

  ;; orbChunk_save_interval    = 250
  ;; chunkNum                  = 4
  orbChunk_save_interval    = 250
  chunkNum                  = 0

  chunkDir                  = outDir+'fully_parsed/'
  chunk_saveFile_pref       = STRING(FORMAT='("eSpec_",A0,"_db--PARSED--Orbs_",I0,"-",I0)', $
                                     newFileDateStr, $
                                     firstOrb, $
                                     lastOrb)

  ;;String together a chunk of orbits, reanalyze, and save
  PRINT,FORMAT='("Start orb",T12,"Stop orb",T24,"N Predicted",T36,"N Actual",T48,"NT Predicted",T60,"NT Actual",T72,"N Orbs this chunk")'
  orbCount                  = 0
  nPredicted                = 0
  nActual                   = 0
  nTotPredicted             = 0
  nTotActual                = 0
  mapRatio                  = !NULL
  TIC
  FOR curOrb=firstOrb,lastOrb DO BEGIN


     chunkStartOrb   = curOrb
     chunkEndOrb     = (curOrb + orbChunk_save_interval-1) < lastOrb
     clock           = TIC(STRING(FORMAT='("reprocess_especs--Orbs_",I0,"-",I0)',chunkStartOrb,chunkEndOrb))

     IF KEYWORD_SET(check_for_existing) THEN BEGIN
        chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I02,"--eSpecs_failCodes_for_orbs_",I0,"-",I0,".sav")', $
                                 chunk_saveFile_pref, $
                                 chunkNum, $
                                 chunkStartOrb, $
                                 chunkEndOrb)
        IF FILE_TEST(chunkDir+chunkTempFName) THEN BEGIN
           PRINT,"FILE EXISTS: " + chunkDir+chunkTempFName
           RESTORE,chunkDir+chunkTempFName

           nActual         = N_ELEMENTS(eSpecs.x)

           IF N_ELEMENTS(UNIQ(eSpecs.x,SORT(eSpecs.x))) NE nActual THEN STOP
           
           nTotActual     += nActual
           nTotPredicted  += nActual

           ;;Some output
           PRINT,FORMAT='(I0,T12,I0,T24,I0,T36,I0,T48,I0,T60,I0,T72,I0)',chunkStartOrb,chunkEndOrb,nPredicted,nActual,nTotPredicted,nTotActual,orbCount

           IF nTotActual NE nTotPredicted THEN STOP

           ;;Now reset loop vars
           eSpecs          = !NULL
           failCodes       = !NULL
           mapRatio        = !NULL

           nPredicted      = 0
           nActual         = 0

           orbCount        = 0

           curOrb          = chunkEndOrb
           chunkNum++ 

           CONTINUE
        ENDIF
     ENDIF

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

           RESTORE,tempFile

           GET_ALT_MLT_ILAT_FROM_FAST_EPHEM,curOrb,eSpecs_parsed.x, $
                                            OUT_TSORTED_I=tSort_i, $
                                            OUT_ALT=alt, $
                                            OUT_MLT=mlt, $
                                            OUT_ILAT=ilat, $
                                            OUT_MAPRATIO=mapRatioTemp, $
                                            OUT_NEVENTS=nUniqTime, $
                                            LOGLUN=logLun

           ;;Replace 'em!
           eSpecs_parsed   = !NULL
           eSpecs_temp     = !NULL
           failCodes_temp  = !NULL

           nEvents         = N_ELEMENTS(tmpeSpec_lc.x)
           IF nEvents NE nUniqTime OR (nEvents NE N_ELEMENTS(tmpJee_lc.x)) THEN BEGIN
              uniqtmpeSpec_lc = UNIQ(tmpeSpec_lc.x,SORT(tmpeSpec_lc.x))
              uniqtmpJee_lc   = UNIQ(tmpJee_lc.x,SORT(tmpJee_lc.x))
              uniqtmpJe_lc    = UNIQ(tmpJe_lc.x,SORT(tmpJe_lc.x))

              tmpeSpec_lc     = {x: tmpeSpec_lc.x[uniqtmpeSpec_lc], $
                                 y: tmpeSpec_lc.y[uniqtmpeSpec_lc,*], $
                                 v: tmpeSpec_lc.y[uniqtmpeSpec_lc,*]}
              tmpJee_lc       = {x: tmpJee_lc.x[uniqtmpJee_lc], $
                                 y: tmpJee_lc.y[uniqtmpJee_lc]}
              tmpJe_lc        = {x: tmpJe_lc.x[uniqtmpJe_lc], $
                                 y: tmpJe_lc.y[uniqtmpJe_lc]}

              nIter           = 0
              letMyPeopleGo   = ARRAY_EQUAL(tmpeSpec_lc.x,tmpJe_lc.x) AND $
                                ARRAY_EQUAL(tmpeSpec_lc.x,tmpJee_lc.x)
              WHILE ~letMyPeopleGo   $
              DO BEGIN

                 CASE 1 OF
                    N_ELEMENTS(tmpeSpec_lc.x) GT N_ELEMENTS(tmpJee_lc.x): BEGIN
                       picker = VALUE_CLOSEST2(tmpeSpec_lc.x,tmpJee_lc.x)
                       tmpeSpec_lc   = {x: tmpeSpec_lc.x[picker], $
                                        y: tmpeSpec_lc.y[picker,*], $
                                        v: tmpeSpec_lc.y[picker,*]}
                    END
                    N_ELEMENTS(tmpeSpec_lc.x) LT N_ELEMENTS(tmpJee_lc.x): BEGIN
                       picker = VALUE_CLOSEST2(tmpJee_lc.x,tmpeSpec_lc.x)
                       tmpJee_lc     = {x: tmpJee_lc.x[picker], $
                                        y: tmpJee_lc.y[picker]}
                       tmpJe_lc      = {x: tmpJe_lc.x[picker], $
                                        y: tmpJe_lc.y[picker]}
                    END
                    N_ELEMENTS(tmpeSpec_lc.x) EQ N_ELEMENTS(tmpJee_lc.x): BEGIN
                       ;; picker = VALUE_CLOSEST2(tmpeSpec_lc.x,tmpJee_lc.x)
                       decideMe = WHERE(ABS(tmpeSpec_lc.x-tmpJee_lc.x) GE 0.5,youDecide)
                    END
                    ELSE: BEGIN
                       PRINT,"I'm nonplussed"
                       STOP
                    END
                 ENDCASE


                 IF (ARRAY_EQUAL(tmpeSpec_lc.x,tmpJe_lc.x) AND $
                     ARRAY_EQUAL(tmpeSpec_lc.x,tmpJee_lc.x))    $
                 THEN letMyPeopleGo = 1

                 ;;youDecide EQ 0 corresponds to no timestamps being more than 0.5 s apart
                 IF ~KEYWORD_SET(youDecide) THEN letMyPeopleGo = 1

                 IF nIter GT 3 THEN BEGIN
                    PRINT,"There's obviously a genuine problem here."
                    STOP
                 ENDIF

                 nIter++
              ENDWHILE
              
              nEvents = N_ELEMENTS(tmpeSpec_lc.x)
              IF nEvents NE nUniqTime THEN STOP
           ENDIF

           mapRatio        = [mapRatio,mapRatioTemp]
           IDENTIFY_DIFF_EFLUXES_AND_CREATE_STRUCT,tmpeSpec_lc,tmpjee_lc,tmpJe_lc,mlt,ilat,alt,MAKE_ARRAY(nEvents,VALUE=curOrb), $
                                                   eSpecs_temp, $
                                                   /QUIET, $
                                                   /HAS_ALT_AND_ORBIT, $
                                                   SC_POT=out_sc_pot, $
                                                   ;; /PRODUCE_FAILCODE_OUTPUT, $
                                                   ;; OUT_FAILCODES=failCodes_temp, $
                                                   /GIVE_TIMESPLIT_INFO, $
                                                   /BATCH_MODE
           ;; ADD_EVENT_TO_SPECTRAL_STRUCT__WITH_ALT,eSpecs,eSpecs_parsed,alt,MAKE_ARRAY(nEvents,VALUE=curOrb)
           ADD_EVENT_TO_SPECTRAL_STRUCT,eSpecs,eSpecs_temp,/HAS_ALT_AND_ORBIT
           ;; ADD_ESPEC_FAILCODES_TO_FAILCODE_STRUCT,failCodes,failCodes_temp

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

     chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I02,"--eSpecs_failCodes_for_orbs_",I0,"-",I0,".sav")', $
                              chunk_saveFile_pref, $
                              chunkNum++, $
                              chunkStartOrb, $
                              chunkEndOrb)

     PRINT,"Saving " + chunkTempFName + '...'
     ;; SAVE,eSpecs,failCodes,FILENAME=chunkDir+chunkTempFName
     SAVE,eSpecs,mapRatio,FILENAME=chunkDir+chunkTempFName

     ;;Check: did we hose it?
     nActual         = N_ELEMENTS(eSpecs.x)
     nTotActual     += nActual
     nTotPredicted  += nPredicted

     ;;Some output
     PRINT,FORMAT='(I0,T12,I0,T24,I0,T36,I0,T48,I0,T60,I0,T72,I0)',chunkStartOrb,chunkEndOrb,nPredicted,nActual,nTotPredicted,nTotActual,orbCount

     IF nTotActual NE nTotPredicted THEN STOP

     ;;Now reset loop vars
     eSpecs          = !NULL
     failCodes       = !NULL
     mapRatio        = !NULL

     nPredicted      = 0
     nActual         = 0

     orbCount        = 0

  ENDFOR

  PRINT,'N total predicted: ' + STRCOMPRESS(nTotPredicted,/REMOVE_ALL)
  PRINT,'N total actual   : ' + STRCOMPRESS(nTotActual,/REMOVE_ALL)

  TOC

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