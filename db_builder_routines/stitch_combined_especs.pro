;;06/04/16
PRO STITCH_COMBINED_ESPECS

  COMPILE_OPT IDL2

  ;;Running options
  loud                      = 0

  firstOrb                  = 500
  lastOrb                   = 16361


  ;; newFileDateStr            = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  newFileDateStr            = '20160607'

  outDir                    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'

  orbChunk_save_interval    = 500
  chunkNum                  = 0
  chunkDir                  = outDir+'fully_parsed/'
  chunk_saveFile_pref       = STRING(FORMAT='("eSpec_",A0,"_db--PARSED--Orbs_",I0,"-",I0)', $
                                     newFileDateStr, $
                                     firstOrb, $
                                     lastOrb)
  masterFile                = STRING(FORMAT='("eSpec_",A0,"_db--PARSED--Orbs_",I0,"-",I0,".sav")', $
                                     newFileDateStr, $
                                     firstOrb, $
                                     lastOrb)

  TIC
  ;;Combine em all
  FOR curOrb=firstOrb,lastOrb DO BEGIN


     chunkStartOrb   = curOrb
     chunkEndOrb     = (curOrb + orbChunk_save_interval-1) < lastOrb
     tmp_interval    = chunkEndOrb-chunkStartOrb
     clock           = TIC(STRING(FORMAT='("combine_all_parsed_especs--Orbs_",I0,"-",I0)',chunkStartOrb,chunkEndOrb))
     
     chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I02,"--eSpecs_for_orbs_",I0,"-",I0,".sav")', $
                              chunk_saveFile_pref, $
                              chunkNum++, $
                              chunkStartOrb, $
                              chunkEndOrb)

     RESTORE,chunkDir+chunkTempFName

     ADD_EVENT_TO_SPECTRAL_STRUCT,eSpec,eSpecs,/HAS_ALT_AND_ORBIT

     TOC,clock

     curOrb        += tmp_interval

  ENDFOR

  PRINT,"Saving to " + masterFile + '...'
  ;; eSpec = !NULL
  ;; eSpec = eSpecs
  SAVE,eSpec,FILENAME=chunkDir+masterFile
  TOC

END
