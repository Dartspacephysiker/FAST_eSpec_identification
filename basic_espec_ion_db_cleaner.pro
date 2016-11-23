;2016/06/07
FUNCTION BASIC_ESPEC_ION_DB_CLEANER,dbStruct,LUN=lun, $
                                    CLEAN_NANS_AND_INFINITIES=clean_nans_and_infinities, $
                                    CLEAN_ON_THE_FLY=clean_on_the_fly
  
  COMPILE_OPT idl2
  
  IF N_ELEMENTS(lun) EQ 0 THEN lun = -1
  n_events = N_ELEMENTS(dbStruct.orbit)
  n_good = n_events
  tot_nLost = 0
  
  IS_STRUCT_ION_OR_ESPEC,dbStruct,is_ion
  
  @alfven_db_cleaner_defaults.pro
  
  IF KEYWORD_SET(clean_nans_and_infinities) THEN BEGIN
     dbTags = tag_names(dbStruct)

     IF is_ion THEN BEGIN
        ;; clean_these_inds = INDGEN(N_ELEMENTS(dbTags))
        clean_these_inds = [1,2,3,4,5,6]
     ENDIF ELSE BEGIN
        ;; clean_these_inds = [2,3,7,8]
        ;; clean_these_inds=INDGEN(N_ELEMENTS(TAG_NAMES(dbStruct)))
        cleanNames = ['je','jee','x']
        clean_these_inds = !NULL
        FOR k=0,N_ELEMENTS(cleanNames)-1 DO BEGIN
           STR_ELEMENT,dbStruct,cleannames[k],INDEX=tmpInd
           IF tmpInd GE 0 THEN BEGIN
              clean_these_inds = [clean_these_inds,tmpInd]
           ENDIF
        ENDFOR
     ENDELSE


     FOR i = 0,N_ELEMENTS(clean_these_inds)-1 DO BEGIN
        IF N_ELEMENTS(good_i) EQ 0 THEN BEGIN
           good_i     = WHERE(FINITE(dbStruct.(clean_these_inds[i])),/NULL)
           nLost      = n_good-N_ELEMENTS(good_i)
           tot_nLost += nLost
           n_good     = n_good - nLost

           IF nLost GT 0 THEN BEGIN
              PRINTF,lun,FORMAT='("NaNs/infs in ",A20,T40,": ",I0)',dbTags[clean_these_inds[i]],nLost
           ENDIF
        ENDIF ELSE BEGIN
           test_i = WHERE(FINITE(dbStruct.(clean_these_inds[i])),/NULL)
           IF N_ELEMENTS(test_i) GT 0 THEN BEGIN
              good_i     = CGSETINTERSECTION(test_i,good_i)
              nLost      = n_good-N_ELEMENTS(good_i)
              tot_nLost += nLost
              n_good     = n_good - nLost

              IF nLost GT 0 THEN BEGIN
                 PRINTF,lun,FORMAT='("NaNs/infs in ",A20,T40,": ",I0)',dbTags[clean_these_inds[i]],nLost
              ENDIF
           ENDIF ELSE BEGIN
              PRINTF,lun,"Lost all indices to " + mTags[clean_these_inds[i]] + "!"
           ENDELSE
        ENDELSE
     ENDFOR
  ENDIF
  
  printf,lun,FORMAT='("NaNs, infinities",T40,": ",I0)',tot_nLost
  
  ;;Now handle the rest
  IF KEYWORD_SET(clean_nans_and_infinities) THEN BEGIN
     good_i = CGSETINTERSECTION(good_i,WHERE(dbStruct.ILAT LE ilat_hcutoff AND dbStruct.ILAT GE ilat_lcutoff)) 
  ENDIF ELSE BEGIN
     good_i = WHERE(dbStruct.ILAT LE ilat_hcutoff AND dbStruct.ILAT GE ilat_lcutoff)
  ENDELSE
  nlost      = n_good-N_ELEMENTS(good_i)
  tot_nLost += nLost
  n_good -= nLost
  IF nLost GT 0 THEN BEGIN
     PRINTF,lun,FORMAT='("N lost to basic ILAT restr",T40,": ",I0)',nlost
  ENDIF

  good_i = CGSETINTERSECTION(good_i,WHERE(dbStruct.MLT LE mlt_hcutoff AND dbStruct.MLT GE 0.0))
  nlost      = n_good-N_ELEMENTS(good_i)
  tot_nLost += nLost
  n_good -= nLost
  IF nLost GT 0 THEN BEGIN
     PRINTF,lun,FORMAT='("N lost to basic MLT restr",T40,": ",I0)',nlost
  ENDIF

  good_i = CGSETINTERSECTION(good_i,WHERE(dbStruct.ORBIT LE orbit_hcutoff AND dbStruct.ORBIT GE orbit_lcutoff))
  nlost      = n_good-N_ELEMENTS(good_i)
  tot_nLost += nLost
  n_good -= nLost
  IF nLost GT 0 THEN BEGIN
     PRINTF,lun,FORMAT='("N lost to basic ORBIT restr",T40,": ",I0)',nlost
  ENDIF

  IF KEYWORD_SET(clean_on_the_fly) THEN BEGIN
     PRINT,"Resizing on the fly ..."
     IF is_ion THEN BEGIN
        dbStruct = RESIZE_ION_DB(dbStruct,good_i)
     ENDIF ELSE BEGIN
        dbStruct = RESIZE_ESPEC_DB(dbStruct,good_i)
     ENDELSE
  ENDIF

  PRINTF,lun,FORMAT='("N lost to basic cutoffs",T40,": ",I0)',tot_nlost
  PRINTF,lun,FORMAT='("N surviving basic screening",T40,": ",I0)',n_good
  PRINTF,lun,"****END basic_db_cleaner.pro****"
  PRINTF,lun,""

  RETURN,good_i
  
END
