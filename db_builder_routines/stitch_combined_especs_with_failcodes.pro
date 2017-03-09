;;06/09/16
PRO STITCH_COMBINED_ESPECS_WITH_FAILCODES

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Running options
  loud                      = 0

  firstOrb                  = 500
  lastOrb                   = 16361


  oldFileDateStr            = '20160609'
  newFileDateStr            = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  outDir                    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'

  orbChunk_save_interval    = 250
  chunkNum                  = 0
  chunkDir                  = outDir+'fully_parsed/'
  chunk_saveFile_pref       = STRING(FORMAT='("eSpec_",A0,"_db--PARSED--Orbs_",I0,"-",I0)', $
                                     oldFileDateStr, $
                                     firstOrb, $
                                     lastOrb)
  masterFile                = STRING(FORMAT='("eSpec_failCodes_",A0,"_db--PARSED--Orbs_",I0,"-",I0,".sav")', $
                                     newFileDateStr, $
                                     firstOrb, $
                                     lastOrb)

  TIC
  ;;Combine em all
  FOR curOrb=firstOrb,lastOrb DO BEGIN


     chunkStartOrb   = curOrb
     chunkEndOrb     = (curOrb + orbChunk_save_interval-1) < lastOrb
     tmp_interval    = chunkEndOrb-chunkStartOrb
     clock           = TIC(STRING(FORMAT='("stitch_combined_especs_failCodes--Orbs_",I0,"-",I0)',chunkStartOrb,chunkEndOrb))
     
     chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I02,"--eSpecs_failCodes_for_orbs_",I0,"-",I0,".sav")', $
                              chunk_saveFile_pref, $
                              chunkNum++, $
                              chunkStartOrb, $
                              chunkEndOrb)

     RESTORE,chunkDir+chunkTempFName

     ADD_EVENT_TO_SPECTRAL_STRUCT,eSpec,eSpecs,/HAS_ALT_AND_ORBIT
     ADD_ESPEC_FAILCODES_TO_FAILCODE_STRUCT,failCode,failCodes

     TOC,clock

     curOrb        += tmp_interval

  ENDFOR

  PRINT,"Saving to " + masterFile + '...'
  ;; eSpec = !NULL
  ;; eSpec = eSpecs
  SAVE,eSpec,failCode,FILENAME=chunkDir+masterFile
  TOC

END
