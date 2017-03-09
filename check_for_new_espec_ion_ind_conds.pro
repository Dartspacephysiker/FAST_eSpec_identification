PRO CHECK_FOR_NEW_ESPEC_ION_IND_CONDS, $
   is_ion,who_recalculate, $
   IMF_STRUCT=IMF_struct, $
   MIMC_STRUCT=MIMC_struct, $
   ALFDB_PLOT_STRUCT=alfDB_plot_struct, $
   ;; ORBRANGE=orbRange, $
   ;; ALTITUDERANGE=altitudeRange, $
   ;; CHARERANGE=charERange, $
   ;; BOTH_HEMIS=both_hemis, $
   ;; NORTH=north, $
   ;; SOUTH=south, $
   ;; HEMI=hemi, $
   ;; HWMAUROVAL=HwMAurOval, $
   ;; HWMKPIND=HwMKpInd, $
   ;; MINMLT=minM, $
   ;; MAXMLT=maxM, $
   ;; BINM=binM, $
   ;; MINILAT=minI, $
   ;; MAXILAT=maxI, $
   ;; BINILAT=binI, $
   ;; EQUAL_AREA_BINNING=EA_binning, $
   ;; DO_LSHELL=do_lshell, $
   ;; MINLSHELL=minLshell, $
   ;; MAXLSHELL=maxLshell, $
   ;; BINLSHELL=binLshell, $
   ;; DAYSIDE=dayside, $
   ;; NIGHTSIDE=nightside, $
   HAVE_GOOD_I=have_good_i, $
   LUN=lun

  COMPILE_OPT IDL2,STRICTARRSUBS

  COMMON MLT_ILAT_MAGC_ETC

  IF KEYWORD_SET(~is_ion) THEN BEGIN
     
     IF TAG_EXIST(alfDB_plot_struct,'charERange') THEN BEGIN
        IF N_ELEMENTS(MIMC__charERange) GT 0 THEN BEGIN
           IF ~ARRAY_EQUAL(MIMC__charERange,charERange) THEN BEGIN
              who_recalculate   = 1
              have_good_i       = 0
              RETURN
           ENDIF
        ENDIF
     ENDIF

  ENDIF

  IF TAG_EXIST(alfDB_plot_struct,'orbRange') THEN BEGIN
     IF N_ELEMENTS(MIMC__orbRange) GT 0 THEN BEGIN
        IF ~ARRAY_EQUAL(MIMC__orbRange, orbRange) THEN BEGIN
           who_recalculate   = 1
              have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(alfDB_plot_struct,'altitudeRange') THEN BEGIN
     IF N_ELEMENTS(MIMC__altitudeRange) GT 0 THEN BEGIN
        IF ~ARRAY_EQUAL(MIMC__altitudeRange,altitudeRange) THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF


  IF TAG_EXIST(MIMC_struct,'both_hemis') THEN BEGIN
     IF N_ELEMENTS(MIMC__both_hemis) GT 0 THEN BEGIN
        IF MIMC__both_hemis NE both_hemis THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(MIMC_struct,'north') THEN BEGIN
     IF N_ELEMENTS(MIMC__north) GT 0 THEN BEGIN
        IF MIMC__north NE MIMC_struct.north THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(MIMC_struct,'south') THEN BEGIN
     IF N_ELEMENTS(MIMC__south) GT 0 THEN BEGIN
        IF MIMC__south NE MIMC_struct.south THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(MIMC_struct,'hemi') THEN BEGIN
     IF N_ELEMENTS(MIMC__hemi) GT 0 THEN BEGIN
        IF MIMC__hemi NE MIMC_struct.hemi THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(MIMC_struct,'minM') THEN BEGIN
     IF N_ELEMENTS(MIMC__minM) GT 0 THEN BEGIN
        IF MIMC__minM NE MIMC_struct.minM THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(MIMC_struct,'maxM') THEN BEGIN
     IF N_ELEMENTS(MIMC__maxM) GT 0 THEN BEGIN
        IF MIMC__maxM NE MIMC_struct.maxM THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(MIMC_struct,'binM') THEN BEGIN
     IF N_ELEMENTS(MIMC__binM) GT 0 THEN BEGIN
        IF MIMC__binM NE MIMC_struct.binM THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(MIMC_struct,'minI') THEN BEGIN
     IF N_ELEMENTS(MIMC__minILAT) GT 0 THEN BEGIN
        IF MIMC__minILAT NE MIMC_struct.minI THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(MIMC_struct,'maxI') THEN BEGIN
     IF N_ELEMENTS(MIMC__maxILAT) GT 0 THEN BEGIN
        IF MIMC__maxILAT NE MIMC_struct.maxI THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(MIMC_struct,'binI') THEN BEGIN
     IF N_ELEMENTS(MIMC__binILAT) GT 0 THEN BEGIN
        IF MIMC__binILAT NE MIMC_struct.binI THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(MIMC_struct,'do_lshell') THEN BEGIN
     IF N_ELEMENTS(MIMC__do_lshell) GT 0 THEN BEGIN
        IF MIMC__do_lshell NE MIMC_struct.do_lshell THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(alfDB_plot_struct,'EA_binning') THEN BEGIN
     IF N_ELEMENTS(MIMC__EA_binning) GT 0 THEN BEGIN
        IF MIMC__EA_binning NE alfDB_plot_struct.EA_binning THEN BEGIN
           MIMC__RECALCULATE = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  ;; IF TAG_EXIST(MIMC_struct,'minL') THEN BEGIN
  ;;    IF N_ELEMENTS(MIMC__minLshell) GT 0 THEN BEGIN
  ;;       IF MIMC__minLshell NE minL THEN BEGIN
  ;;          who_recalculate   = 1
  ;;          have_good_i       = 0
  ;;          RETURN
  ;;       ENDIF
  ;;    ENDIF
  ;; ENDIF

  ;; IF TAG_EXIST(MIMC_struct,'maxL') THEN BEGIN
  ;;    IF N_ELEMENTS(MIMC__maxLshell) GT 0 THEN BEGIN
  ;;       IF MIMC__maxLshell NE maxL THEN BEGIN
  ;;          who_recalculate   = 1
  ;;          have_good_i       = 0
  ;;          RETURN
  ;;       ENDIF
  ;;    ENDIF
  ;; ENDIF

  ;; IF TAG_EXIST(MIMC_struct,'binL') THEN BEGIN
  ;;    IF N_ELEMENTS(MIMC__binLshell) GT 0 THEN BEGIN
  ;;       IF MIMC__binLshell NE binL THEN BEGIN
  ;;          who_recalculate   = 1
  ;;          have_good_i       = 0
  ;;          RETURN
  ;;       ENDIF
  ;;    ENDIF
  ;; ENDIF

  IF TAG_EXIST(MIMC_struct,'dayside') THEN BEGIN
     IF N_ELEMENTS(MIMC__dayside) GT 0 THEN BEGIN
        IF MIMC__dayside NE dayside THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(MIMC_struct,'nightside') THEN BEGIN
     IF N_ELEMENTS(MIMC__nightside) GT 0 THEN BEGIN
        IF MIMC__nightside NE nightside THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(alfDB_plot_struct,'HwMAurOval') THEN BEGIN
     IF N_ELEMENTS(MIMC__HwMAurOval) GT 0 THEN BEGIN
        IF MIMC__HwMAurOval NE alfDB_plot_struct.HwMAurOval THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  IF TAG_EXIST(alfDB_plot_struct,'HwMKpInd') THEN BEGIN
     IF N_ELEMENTS(MIMC__HwMKpInd) GT 0 THEN BEGIN
        IF MIMC__HwMKpInd NE alfDB_plot_struct.HwMKpInd THEN BEGIN
           who_recalculate   = 1
           have_good_i       = 0
           RETURN
        ENDIF
     ENDIF
  ENDIF

  who_recalculate            = 0
  have_good_i                = 1

END