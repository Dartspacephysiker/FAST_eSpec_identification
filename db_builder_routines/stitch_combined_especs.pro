;;06/04/16
PRO STITCH_COMBINED_ESPECS,UPGOING=upgoing, $
                           DBDATE=DBDate

  COMPILE_OPT IDL2

  ;;Running options
  loud                      = 0

  firstOrb                  = 500
  lastOrb                   = 16361

  ;; newFileDateStr            = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) ;'20160607'
  newFileDateStr            = KEYWORD_SET(DBDate) ? DBDate : GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  outDir                    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'

  skipSortTags              = ['MLT','ALT','ILAT','JI','JEI']
  CASE 1 OF
     KEYWORD_SET(upgoing): BEGIN
        fPref    = "eSpec_up_"
        clockStr = "eSpecs"
     END
     ELSE: BEGIN
        fPref = "eSpec_"
        clockStr = "eSpecs"
     END
  ENDCASE

  orbChunk_save_interval    = 500
  chunkNum                  = 0
  chunkDir                  = outDir+'fully_parsed/'
  chunk_saveFile_pref       = STRING(FORMAT='(A0,A0,"_db--PARSED--Orbs_",I0,"-",I0)', $
                                     fPref, $
                                     newFileDateStr, $
                                     firstOrb, $
                                     lastOrb)
  masterFile                = STRING(FORMAT='(A0,A0,"_db--PARSED--Orbs_",I0,"-",I0,".sav")', $
                                     fPref, $
                                     newFileDateStr, $
                                     firstOrb, $
                                     lastOrb)

  TIC
  ;;Combine em all
  FOR curOrb=firstOrb,lastOrb DO BEGIN

     chunkStartOrb   = curOrb
     chunkEndOrb     = (curOrb + orbChunk_save_interval-1) < lastOrb
     tmp_interval    = chunkEndOrb-chunkStartOrb
     clock           = TIC(STRING(FORMAT='("combine_all_parsed_",A0,"--Orbs_",I0,"-",I0)', $
                                  clockStr, $
                                  chunkStartOrb, $
                                  chunkEndOrb))
     
     chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I02,"--",A0,"_for_orbs_",I0,"-",I0,".sav")', $
                              chunk_saveFile_pref, $
                              (chunkNum > 0), $
                              clockStr, $
                              chunkStartOrb, $
                              chunkEndOrb)

     chunkNum++

     RESTORE,chunkDir+chunkTempFName

     ADD_EVENT_TO_SPECTRAL_STRUCT,eSpec,eSpecs,/HAS_ALT_AND_ORBIT

     TOC,clock

     curOrb        += tmp_interval

  ENDFOR

  PRINT,"Making sure it's super sorted ..."
  CHECK_SORTED,eSpec.x,is_sorted,/QUIET
  IF (N_ELEMENTS(UNIQ(eSpec.x,SORT(eSpec.x))) NE N_ELEMENTS(eSpec.x)) OR $
  ~is_sorted $
  THEN BEGIN
     PRINT,"TROUBLE!"

     nInds  = N_ELEMENTS(eSpec.x)
     good_i = UNIQ(eSpec.x,SORT(eSpec.x))

     tags   = TAG_NAMES(eSpec)

     PRINT,"N previous: ",nInds
     PRINT,"N uniq    : ",N_ELEMENTS(good_i)
     FOR k=0,N_ELEMENTS(tags)-1 DO BEGIN
        IF N_ELEMENTS(eSpec.(k)) EQ nInds THEN BEGIN

           PRINT,"Replacing elements in " + tags[k]

           STR_ELEMENT,eSpec,tags[k],(eSpec.(k))[good_i],/ADD_REPLACE

           IF (WHERE(STRUPCASE(tags[k]) EQ STRUPCASE(skipSortTags)))[0] EQ -1 THEN BEGIN
              CHECK_SORTED,eSpec.(k),is_sorted,/QUIET
              IF ~is_sorted THEN BEGIN
                 PRINT,"Triple death!"
                 STOP
              ENDIF
           ENDIF

        ENDIF

     ENDFOR
  ENDIF

  PRINT,"Saving to " + masterFile + '...'
  ;; eSpec = !NULL
  ;; eSpec = eSpecs
  SAVE,eSpec,FILENAME=chunkDir+masterFile
  TOC

END
