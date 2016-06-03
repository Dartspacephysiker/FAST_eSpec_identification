PRO CHECK_NEWELL_FILES_FOR_DUPES,cdbTime,orbArr,firstOrb,lastOrb,missingOrbArr, $
                                 Newell_DB_dir,Newell_filePref, $
                                 OUT_ORBS_WITH_DUPES_ARR=outOrbArr, $
                                 EPSILON=eps, $
                                 LUN=lun

  IF N_ELEMENTS(lun) NE 0 THEN outLun = lun ELSE outLun = -1

  PRINTF,outLun,FORMAT='("Orbit",T10,"N dupes","N unique")'

  outOrbArr          = !NULL
  FOR curOrb=firstOrb,lastOrb DO BEGIN

     PRINT,curOrb

     ;;First, cat all eSpecs from each interval for this orbit
     doneski         = 0
     curInterval     = 0
     cur_eSpec       = !NULL
     tempFile        = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)
     IF ~FILE_TEST(tempFile) THEN BEGIN

        doneski      = 1

        CONTINUE
     ENDIF
     WHILE ~doneski DO BEGIN
        RESTORE,tempFile
        
        CAT_ESPECS_FROM_NEWELL_FILES,cur_eSpec,tmpeSpec_lc

        ;;Check for next interval
        curInterval++
        tempFile     = STRING(FORMAT='(A0,A0,I0,"_",I0,".sav")',Newell_DB_dir,Newell_filePref,curOrb,curInterval)
        IF ~FILE_TEST(tempFile) THEN doneski  = 1
     ENDWHILE

     ;;Now check for dupes
     has_dupes       = !NULL
     CHECK_DUPES,cur_eSpec.x,HAS_DUPES=has_dupes,N_DUPES=nDupes,OUT_UNIQ_I=uniq_i,/QUIET

     ;; tmpTime         = cur_eSpec.x
     ;; nDupes          = 0
     ;; has_dupes       = 0
     ;; check_general   = 0
     ;; CASE N_ELEMENTS(tmpTime) OF
     ;;    1: 
     ;;    2: BEGIN
     ;;       IF ABS(tmptTime[0] - tmpTime[1]) LT delta THEN BEGIN
     ;;          has_dupes = 1
     ;;          nDupes++
     ;;       ENDIF
     ;;    END
     ;;    ELSE: BEGIN
     ;;       check_general = 1
     ;;    END
     ;; ENDCASE


     ;; IF check_general THEN BEGIN
     ;;    ;;Check beginning and end really quick-like
     ;;    tmpDiff = MIN(ABS(tmpTime[0] - tmpTime[1:-1]),tmpInd)
     ;;    tmpInd++
     ;;    IF tmpDiff LE eps THEN BEGIN
     ;;       nDupes++
     ;;       ;; dupe_inds = [dupe_inds,tmpInd]
     ;;    ENDIF

     ;;    tmpDiff = MIN(ABS(tmpTime[-1] - tmpTime[0:-2]),tmpInd)
     ;;    IF tmpDiff LE eps THEN BEGIN
     ;;       nDupes++
     ;;       ;; dupe_inds = [dupe_inds,tmpInd]
     ;;    ENDIF

     ;;    FOR k=1,N_ELEMENTS(tmpTime)-2 DO BEGIN
     ;;       tmpDiff = MIN(ABS(tmpTime[k] - [tmpTime[0:k],tmpTime[k+1:-1]]),tmpInd)
     ;;       IF tmpDiff LE eps THEN BEGIN
     ;;          IF tmpInd GE k THEN tmpInd++
     ;;          ;; dupe_inds = [dupe_inds,tmpInd]
     ;;          nDupes++
     ;;       ENDIF
     ;;    ENDFOR

     ;;    nDupes /= 2 ;Because we double-count all dupes
     ;;    has_dupes = nDupes GT 0
     ;; ENDIF

     IF has_dupes THEN BEGIN
        outOrbArr    = [[outOrbArr],[curOrb,nDupes]]
        PRINTF,outLun,FORMAT='(I0,T10,I0)'
     ENDIF
  ENDFOR

  
END