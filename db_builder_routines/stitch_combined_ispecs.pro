;;06/07/16
PRO STITCH_COMBINED_ISPECS

  COMPILE_OPT IDL2

  ;;Running options
  loud                      = 0

  firstOrb                  = 500
  lastOrb                   = 16361

  newFileDateStr            = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  outDir                    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'

  orbChunk_save_interval    = 500
  chunkNum                  = 0
  chunkDir                  = outDir+'fully_parsed/'
  chunk_saveFile_pref       = STRING(FORMAT='("iSpec_",A0,"_db--PARSED--Orbs_",I0,"-",I0)', $
                                     newFileDateStr, $
                                     firstOrb, $
                                     lastOrb)
  masterFile                = STRING(FORMAT='("iSpec_",A0,"_db--PARSED--Orbs_",I0,"-",I0,".sav")', $
                                     newFileDateStr, $
                                     firstOrb, $
                                     lastOrb)

  TIC
  ;;Combine em all
  FOR curOrb=firstOrb,lastOrb DO BEGIN


     chunkStartOrb   = curOrb
     chunkEndOrb     = (curOrb + orbChunk_save_interval-1) < lastOrb
     tmp_interval    = chunkEndOrb-chunkStartOrb
     clock           = TIC(STRING(FORMAT='("combine_all_iSpecs--Orbs_",I0,"-",I0)',chunkStartOrb,chunkEndOrb))
     
     chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I02,"--iSpecs_for_orbs_",I0,"-",I0,".sav")', $
                              chunk_saveFile_pref, $
                              (chunkNum > 0), $
                              chunkStartOrb, $
                              chunkEndOrb)
     chunkNum++

     RESTORE,chunkDir+chunkTempFName

           CAT_ION_STRUCTS,ion,ions

     TOC,clock

     curOrb        += tmp_interval

  ENDFOR

  PRINT,"Saving to " + masterFile + '...'
  ;; eSpec = !NULL
  ;; eSpec = eSpecs
  SAVE,ion,FILENAME=chunkDir+masterFile
  TOC

END
