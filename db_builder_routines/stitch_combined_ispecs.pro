;;06/07/16
PRO STITCH_COMBINED_ISPECS,DOWNGOING=downgoing, $
                           DBDATE=DBDate

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;;Running options
  loud                      = 0

  firstOrb                  = 500
  lastOrb                   = 16361

  newFileDateStr            = KEYWORD_SET(DBDate) ? DBDate : GET_TODAY_STRING(/DO_YYYYMMDD_FMT)

  outDir                    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'

  skipSortTags              = ['MLT','ALT','ILAT','JI','JEI']
  CASE 1 OF
     KEYWORD_SET(downgoing): BEGIN
        fPref    = "iSpec_down_"
        clockStr = "iSpecs"
     END
     ELSE: BEGIN
        fPref = "iSpec_"
        clockStr = "iSpecs"
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
     clock           = TIC(STRING(FORMAT='("combine_all_",A0,"--Orbs_",I0,"-",I0)', $
                                  clockStr, $
                                  chunkStartOrb,chunkEndOrb))
     
     chunkTempFName  = STRING(FORMAT='(A0,"--CHUNK_",I02,"--",A0,"_for_orbs_",I0,"-",I0,".sav")', $
                              chunk_saveFile_pref, $
                              (chunkNum > 0), $
                              clockStr, $
                              chunkStartOrb, $
                              chunkEndOrb)
     chunkNum++

     RESTORE,chunkDir+chunkTempFName

           CAT_ION_STRUCTS,ion,ions

     TOC,clock

     curOrb        += tmp_interval

  ENDFOR

  PRINT,"Making sure it's super sorted ..."
  CHECK_SORTED,ion.x,is_sorted,/QUIET
  IF (N_ELEMENTS(UNIQ(ion.x,SORT(ion.x))) NE N_ELEMENTS(ion.x)) OR $
  ~is_sorted $
  THEN BEGIN
     PRINT,"TROUBLE!"

     nInds  = N_ELEMENTS(ion.x)
     good_i = UNIQ(ion.x,SORT(ion.x))

     tags   = TAG_NAMES(ion)

     PRINT,"N previous: ",nInds
     PRINT,"N uniq    : ",N_ELEMENTS(good_i)
     FOR k=0,N_ELEMENTS(tags)-1 DO BEGIN
        IF N_ELEMENTS(ion.(k)) EQ nInds THEN BEGIN

           PRINT,"Replacing elements in " + tags[k]

           ;; STR_ELEMENT,ion,tags[k],(ion.(k))[good_i],/ADD_REPLACE

           IF (WHERE(STRUPCASE(tags[k]) EQ STRUPCASE(skipSortTags)))[0] EQ -1 THEN BEGIN
              CHECK_SORTED,ion.(k),is_sorted,/QUIET
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
  SAVE,ion,FILENAME=chunkDir+masterFile
  TOC

END
