;2016/10/08
PRO JOURNAL__20161008__GET_ESPECDB_FOOTPOINTS_AND_MAGFIELD_RATIOS__SDT_STYLE

  COMPILE_OPT idl2

  outDir        = '/SPENCEdata/Research/database/FAST/dartdb/saves/mapratio_dbs/'
  outFilePref   = 'mapratio_for_20160607_eSpecDB--20161008--'

  tmpDir        = outDir + 'TEMP_eSpec/'
  finalFile     = 'mapratio_for_20160607_eSpecDB--20161008.sav'

  finalMappedDB = 'eSpec_20160607_db--PARSED--with_mapping_factors--Orbs_500-16361.sav'

  delta         = 1000000

  ;; start_k       = 28
  ;; stop_k        = 29

  create_footpoint_files = 0
  stitch_footpoint_files = 0
  apply_ratios_to_db     = 1

  LOAD_NEWELL_ESPEC_DB,!NULL, $
                       OUT_TIMES=t, $
                       /JUST_TIMES, $
                       /DONT_LOAD_IN_MEMORY, $
                       /DONT_CONVERT_TO_STRICT_NEWELL, $
                       /QUIET


  need_div = KEYWORD_SET(create_footpoint_files) OR $
             KEYWORD_SET(stitch_footpoint_files)

  IF need_div THEN BEGIN
     div = DIVVY_INDICES(N_ELEMENTS(t),delta,kDecPlace,lens)
  ENDIF

  IF KEYWORD_SET(create_footpoint_files) THEN BEGIN
     CREATE_FOOTPOINT_FILES,div,lens,kDecPlace,outFilePref,tmpDir, $
                            START_K=start_k, $
                            END_K=end_k
  ENDIF

  IF KEYWORD_SET(stitch_footpoint_files) THEN BEGIN
     STITCH_TOGETHER_ESPEC_FOOTPOINT_FILES,div,lens,kDecPlace,outFilePref,tmpDir, $
                                           finalFile,outDir, $
                                           START_K=start_k, $
                                           END_K=end_k
  ENDIF

  IF KEYWORD_SET(apply_ratios_to_db) THEN BEGIN
     APPLY_RATIOS_TO_DB,finalMappedDB,finalFile,outDir
  ENDIF

END

FUNCTION DIVVY_INDICES,N,delta,OUT_kDecPlace,OUT_lens

  COMPILE_OPT idl2

  ;;divvy up indices
  div       = !NULL
  ind       = 0
  WHILE ind LT (N-1) DO BEGIN
     tmpInd = (ind + delta-1) < (N - 1)
     div    = [[div],[ind,tmpInd]]
     ind    = tmpInd + 1

  END

  PRINT,"Index pairs:"
  FOR k=0, N_ELEMENTS(div[0,*])-1 DO BEGIN
     PRINT,div[0,k],div[1,k]
  ENDFOR
  PRINT,'Look OK?'

  OUT_kDecPlace = FLOOR(MAX(ALOG10(N_ELEMENTS(div[0,*])))) + 1
  OUT_lens      = REFORM(div[1,*]-div[0,*]) + 1

  RETURN,div
END

PRO CREATE_FOOTPOINT_FILES,div,lens,kDecPlace,outFilePref,tmpDir, $
                           START_K=start_k, $
                           END_K=end_k
  
  COMPILE_OPT idl2

  IF ~MAKE_SURE_FILES_EXIST(div,kDecPlace,outFilePref,tmpDir) THEN RETURN

  strtK = N_ELEMENTS(start_k) GT 0 ? start_k : 0

  stopK = N_ELEMENTS(stop_k) GT 0 ? stop_k   : N_ELEMENTS(div[0,*])

  PRINT,"Kicking off the show ..."
  clock = TIC("eSpec_mapClock")
  FOR k=strtk,stopK-1 DO BEGIN

     inds = [div[0,k]:div[1,k]]

     LOAD_NEWELL_ESPEC_DB,!NULL, $
                          OUT_TIMES=t, $
                          /JUST_TIMES, $
                          /DONT_LOAD_IN_MEMORY, $
                          /DONT_CONVERT_TO_STRICT_NEWELL, $
                          /QUIET

     PRINT,FORMAT='("Getting the next ",I0," inds (",I0," through ",I0,") ...")', $
           lens[k],inds[0],inds[-1]
     outFileSuff = STRING(FORMAT='(I0'+STRCOMPRESS(kDecPlace,/REMOVE_ALL)+')',k)

     tmpT = t[inds]
     t    = !NULL

     GET_FA_ORBIT,tmpT,/TIME_ARRAY,/ALL
     get_data,'ILAT',DATA=ilat
     ;; GET_DATA,'ORBIT',data=ilat

     PRINT,'Got FA_ORBIT stuff!'
     ;;Scale electron energy flux to 100km, pos flux earthward
     GET_DATA,'B_model',DATA=bMod
     GET_DATA,'BFOOT',DATA=bFoot

     mag1      = (bMod.y[*,0]*bMod.y[*,0]+ $
                  bMod.y[*,1]*bMod.y[*,1]+ $
                  bMod.y[*,2]*bMod.y[*,2])^0.5
     mag2      = (bFoot.y[*,0]*bFoot.y[*,0]+ $
                  bFoot.y[*,1]*bFoot.y[*,1]+ $
                  bFoot.y[*,2]*bFoot.y[*,2])^0.5
     ratio     = mag2/mag1

     GET_DATA,'ORBIT',DATA=orbit
     orbit     = orbit.y

     mapRatio  = { mag1: mag1, $
                   mag2: mag2, $
                   ratio: ratio, $
                   times: TEMPORARY(tmpT), $
                   orbit: TEMPORARY(orbit)}

     PRINT,'Saving ' + outFilePref + outFileSuff + '.dat ...'
     SAVE,mapRatio,FILENAME=tmpDir+outFilePref+outFileSuff+'.dat'

     mapRatio  = !NULL

     TOC,clock
  ENDFOR

  PRINT,"Finished getting map ratios!"

END

PRO STITCH_TOGETHER_ESPEC_FOOTPOINT_FILES,div,lens,kDecPlace,outFilePref,tmpDir, $
                                          finalFile,outDir, $
                                          START_K=start_k, $
                                          END_K=end_k

  COMPILE_OPT idl2

  IF ~MAKE_SURE_FILES_EXIST(div,kDecPlace,outFilePref,tmpDir) THEN RETURN

  strtK = N_ELEMENTS(start_k) GT 0 ? start_k : 0

  stopK = N_ELEMENTS(stop_k) GT 0 ? stop_k   : N_ELEMENTS(div[0,*])


  totPoints = 0
  FOR k=strtK,stopK-1 DO totPoints += lens[k]

  PRINT,STRCOMPRESS(totPoints,/REMOVE_ALL) + ' points to stitch in total'

  mapFinal  = {mag1:MAKE_ARRAY(totPoints,/FLOAT), $
               mag2:MAKE_ARRAY(totPoints,/FLOAT), $
               ratio:MAKE_ARRAY(totPoints,/FLOAT)} ;, $
  ;; times:MAKE_ARRAY(totPoints,/DOUBLE), $
               ;; orbit:MAKE_ARRAY(totPoints,/LONG)}

  PRINT,"Stitch it ..."
  clock = TIC("eSpec stitch")
  FOR k=strtK,stopK-1 DO BEGIN

     inds = [div[0,k]:div[1,k]]

     outFileSuff = STRING(FORMAT='(I0'+STRCOMPRESS(kDecPlace,/REMOVE_ALL)+')',k)

     fName = tmpDir+outFilePref+outFileSuff+'.dat'

     RESTORE,fName

     PRINT,FORMAT='("Stitching the next ",I0," inds (",I0," through ",I0,") ...")', $
           lens[k],inds[0],inds[-1]

     IF N_ELEMENTS(mapRatio.mag1) NE lens[k] THEN BEGIN
        PRINT,"WHATTLLKSJFDLJF?!?SDF"
        STOP
     ENDIF

     mapFinal.mag1[inds]  = mapRatio.mag1
     mapFinal.mag2[inds]  = mapRatio.mag2
     mapFinal.ratio[inds] = mapRatio.ratio
;; mapFinal.times[inds] = mapRatio.times
;; mapFinal.orbit[inds] = mapRatio.orbit

     mapRatio  = !NULL

     TOC,clock
  ENDFOR

  PRINT,'Finished stitching!'

  mapRatio = TEMPORARY(mapFinal)

  PRINT,'Saving ' + finalFile + ' ...'
  SAVE,mapRatio,FILENAME=outDir+finalFile

  STOP
END

PRO APPLY_RATIOS_TO_DB,finalMappedDB,finalFile,outDir

  PRINT,'Restoring mapRatio DB ...'
  RESTORE,outDir+finalFile

  LOAD_NEWELL_ESPEC_DB,eSpec, $
                       NEWELLDBDIR=NewellDBDir, $
                       /DONT_LOAD_IN_MEMORY, $
                       /DONT_CONVERT_TO_STRICT_NEWELL, $
                       /QUIET
  
  STR_ELEMENT,eSpec,'mapFactor',INDEX=index

  IF index EQ -2 THEN BEGIN
     PRINT,"Something's up, children. There's no eSpec DB to be had ..."
     RETURN
  ENDIF

  IF index GE 0 THEN BEGIN
     PRINT,"Something's up, children. I think we've already updated this DB."
     RETURN
  ENDIF



  IF N_ELEMENTS(mapRatio.mag1) NE N_ELEMENTS(eSpec.je) THEN STOP ELSE BEGIN
     PRINT,'K, same number of elements at any rate ...'
     PRINT,"Application!"
  ENDELSE

  ;; eSpec.Je  * mapRatio.ratio
  ;; eSpec.Jee * mapRatio.ratio

  eSpec = CREATE_STRUCT(eSpec,'mapFactor',mapRatio.ratio)

  PRINT,'Saving updated eSpec to ' + finalMappedDB + ' ...'
  SAVE,eSpec,FILENAME=NewellDBDir+finalMappedDB

END

FUNCTION MAKE_SURE_FILES_EXIST,div,kDecPlace,outFilePref,outDir

  COMPILE_OPT idl2

  PRINT,'Making sure all ' + STRCOMPRESS(N_ELEMENTS(div[0,*]),/REMOVE_ALL) + $
        ' files exist ...'
  FOR k=0,N_ELEMENTS(div[0,*])-1 DO BEGIN

     outFileSuff = STRING(FORMAT='(I0'+STRCOMPRESS(kDecPlace,/REMOVE_ALL)+')',k)

     fName = outDir+outFilePref+outFileSuff+'.dat'

     IF ~FILE_TEST(fName) THEN BEGIN
        PRINT,"File doesn't exist: " + fName
        PRINT,'Returning ...'
        RETURN,0
     ENDIF

  ENDFOR

  RETURN,1
END

