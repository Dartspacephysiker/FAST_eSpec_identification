;;11/25/16
PRO JOURNAL__20161125__LOOK_AT_NEWELLTYPE_HISTOS_OF_SUPPOSEDLY_REJECT_INDICES

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__newell_espec.pro

  ;;eSpec DB options
  use_2000km_file = 0
  force_load_DB   = 1

  ;;Plot options
  eFlux               = 1
  eNumFlux            = 0
  charE               = 0

  log_plots           = 1
  log_stats           = 1
  zoomed_histoXRange  = 1
  zoomed_histoYRange  = 1

  save_plots          = 1

  show_plots          = 0

  ;;gjør det en del 
  LOAD_NEWELL_ESPEC_DB, $
     !NULL, $
     NEWELLDBDIR=NewellDBDir, $
     USE_2000KM_FILE=use_2000km_file, $
     FORCE_LOAD_DB=force_load_DB

  safedFile = GET_NEWELL_ESPEC_SAFED_INDS_FILE(NEWELL__eSpec, $
                                               NEWELLDBDIR=NewellDBDir)

  PRINT,'Restore those "safe" indices'
  RESTORE,safedFile

  reject_eSpec_i = LINDGEN(N_ELEMENTS(NEWELL__eSpec.x))
  reject_eSpec_i = CGSETDIFFERENCE(reject_eSpec_i,cleaned_eSpec_i,COUNT=nReject)

  PRINT,"N good inds: ",eSpec_clean_info.totKept
  PRINT,"N bad  inds: ",eSpec_clean_info.totLost

  ;;NOW PLOTSSSSSSS
  safeWinInd          = KEYWORD_SET(show_plots) ? 5 : !NULL
  rejectWinInd        = KEYWORD_SET(show_plots) ? 6 : !NULL

  JOURNAL__20161122__SO_HOW_DO_WE_CLEAN_YOU_ESPEC_DB, $
     ENUMFLUX=je, $
     EFLUX=eFlux, $
     CHARE=charE, $
     POS_ONLY=pos_only, $
     NEG_ONLY=neg_only, $
     LOG_PLOTS=log_plots, $
     LOG_STATS=log_stats, $
     SHOW_PLOTS=show_plots, $
     WINDOW_INDEX=safeWinInd, $
     SAVE_PLOTS=save_plots, $
     ZOOMED_HISTOXRANGE=zoomed_histoXRange, $
     ZOOMED_HISTOYRANGE=zoomed_histoYRange, $
     NORMALIZE_YRANGE=normalize_yRange, $
     OUT_ESTATS=eStats, $
     USER_INDS=cleaned_eSpec_i, $
     USER_PLOTSUFF='--safed_indices--' + eSpec_clean_info.db_info.db_extras.Replace('/','--')

  IF nReject GT 0 THEN BEGIN
     JOURNAL__20161122__SO_HOW_DO_WE_CLEAN_YOU_ESPEC_DB, $
        ENUMFLUX=je, $
        EFLUX=eFlux, $
        CHARE=charE, $
        POS_ONLY=pos_only, $
        NEG_ONLY=neg_only, $
        LOG_PLOTS=log_plots, $
        LOG_STATS=log_stats, $
        SHOW_PLOTS=show_plots, $
        SAVE_PLOTS=save_plots, $
        WINDOW_INDEX=rejectWinInd, $
        ZOOMED_HISTOXRANGE=zoomed_histoXRange, $
        ZOOMED_HISTOYRANGE=zoomed_histoYRange, $
        NORMALIZE_YRANGE=normalize_yRange, $
        OUT_ESTATS=eStats, $
        USER_INDS=reject_eSpec_i, $
        USER_PLOTSUFF='--UNsafe_indices--' + eSpec_clean_info.db_info.db_extras.Replace('/','--')
  ENDIF
  

END
