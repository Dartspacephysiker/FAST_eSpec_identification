PRO CHECK_FOR_NEW_ESPEC_ION_IND_CONDS,is_ion,who_recalculate, $
                            ORBRANGE=orbRange, $
                            ALTITUDERANGE=altitudeRange, $
                            CHARERANGE=charERange, $
                            BOTH_HEMIS=both_hemis, $
                            NORTH=north, $
                            SOUTH=south, $
                            HEMI=hemi, $
                            HWMAUROVAL=HwMAurOval, $
                            HWMKPIND=HwMKpInd, $
                            MINMLT=minMLT, $
                            MAXMLT=maxMLT, $
                            BINM=binMLT, $
                            MINILAT=minILAT, $
                            MAXILAT=maxILAT, $
                            BINILAT=binILAT, $
                            ;; DO_LSHELL=do_lshell, $
                            ;; MINLSHELL=minLshell, $
                            ;; MAXLSHELL=maxLshell, $
                            ;; BINLSHELL=binLshell, $
                            DAYSIDE=dayside, $
                            NIGHTSIDE=nightside, $
                            HAVE_GOOD_I=have_good_i, $
                            LUN=lun

  COMPILE_OPT idl2

  COMMON MLT_ILAT_MAGC_ETC

  IF KEYWORD_SET(~is_ion) THEN BEGIN
     
     IF N_ELEMENTS(charERange) GT 0 THEN BEGIN
        IF N_ELEMENTS(MIMC__charERange) GT 0 THEN BEGIN
           IF ~ARRAY_EQUAL(MIMC__charERange,charERange) THEN BEGIN
              who_recalculate   = 1
              have_good_i       = 0
              RETURN
           ENDIF
        ENDIF
     ENDIF

  ENDIF

  IF N_ELEMENTS(orbRange) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__orbRange) GT 0 THEN BEGIN
        IF ~ARRAY_EQUAL(MIMC__orbRange, orbRange) THEN BEGIN
           who_recalculate   = 1
              have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(altitudeRange) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__altitudeRange) GT 0 THEN BEGIN
        IF ~ARRAY_EQUAL(MIMC__altitudeRange,altitudeRange) THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF


  IF N_ELEMENTS(both_hemis) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__both_hemis) GT 0 THEN BEGIN
        IF MIMC__both_hemis NE both_hemis THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(north) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__north) GT 0 THEN BEGIN
        IF MIMC__north NE north THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(south) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__south) GT 0 THEN BEGIN
        IF MIMC__south NE south THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(hemi) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__hemi) GT 0 THEN BEGIN
        IF MIMC__hemi NE hemi THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(minMLT) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__minMLT) GT 0 THEN BEGIN
        IF MIMC__minMLT NE minMLT THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(maxMLT) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__maxMLT) GT 0 THEN BEGIN
        IF MIMC__maxMLT NE maxMLT THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(binMLT) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__binMLT) GT 0 THEN BEGIN
        IF MIMC__binMLT NE binMLT THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(minILAT) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__minILAT) GT 0 THEN BEGIN
        IF MIMC__minILAT NE minILAT THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(maxILAT) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__maxILAT) GT 0 THEN BEGIN
        IF MIMC__maxILAT NE maxILAT THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(binILAT) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__binILAT) GT 0 THEN BEGIN
        IF MIMC__binILAT NE binILAT THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(do_lshell) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__do_lshell) GT 0 THEN BEGIN
        IF MIMC__do_lshell NE do_lshell THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  ;; IF N_ELEMENTS(minLshell) GT 0 THEN BEGIN
  ;;    IF N_ELEMENTS(MIMC__minLshell) GT 0 THEN BEGIN
  ;;       IF MIMC__minLshell NE minLshell THEN BEGIN
  ;;          who_recalculate   = 1
  ;;          have_good_i       = 0
  ;;          RETURN
  ;;       ENDIF
  ;;    ENDIF
  ;; ENDIF

  ;; IF N_ELEMENTS(maxLshell) GT 0 THEN BEGIN
  ;;    IF N_ELEMENTS(MIMC__maxLshell) GT 0 THEN BEGIN
  ;;       IF MIMC__maxLshell NE maxLshell THEN BEGIN
  ;;          who_recalculate   = 1
  ;;          have_good_i       = 0
  ;;          RETURN
  ;;       ENDIF
  ;;    ENDIF
  ;; ENDIF

  ;; IF N_ELEMENTS(binLshell) GT 0 THEN BEGIN
  ;;    IF N_ELEMENTS(MIMC__binLshell) GT 0 THEN BEGIN
  ;;       IF MIMC__binLshell NE binLshell THEN BEGIN
  ;;          who_recalculate   = 1
  ;;          have_good_i       = 0
  ;;          RETURN
  ;;       ENDIF
  ;;    ENDIF
  ;; ENDIF

  IF N_ELEMENTS(dayside) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__dayside) GT 0 THEN BEGIN
        IF MIMC__dayside NE dayside THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(nightside) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__nightside) GT 0 THEN BEGIN
        IF MIMC__nightside NE nightside THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(HwMAurOval) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__HwMAurOval) GT 0 THEN BEGIN
        IF MIMC__HwMAurOval NE HwMAurOval THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF N_ELEMENTS(HwMKpInd) GT 0 THEN BEGIN
     IF N_ELEMENTS(MIMC__HwMKpInd) GT 0 THEN BEGIN
        IF MIMC__HwMKpInd NE HwMKpInd THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  who_recalculate            = 0
  have_good_i                = 1

END