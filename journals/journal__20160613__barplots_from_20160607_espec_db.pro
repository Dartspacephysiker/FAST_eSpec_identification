PRO JOURNAL__20160613__BARPLOTS_FROM_20160607_ESPEC_DB

  LOAD_NEWELL_ESPEC_DB,eSpec,/DONT_LOAD_IN_MEMORY


  SET_PLOT_DIR,/FOR_ESPECDB,/ADD_TODAY

  ;;By altitude
  bpSaveName      = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)+'--barplot_eSpec_types_vs_altitude--20160607_eSpec_DB.png'
  plots           = BARPLOT_ESPEC_TYPES_VS_ALT(eSpec, $
                                               PLOTDIR=plotDir, $
                                               /SAVEPLOT, $
                                               SPNAME=bpSaveName)

  bpSaveName      = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)+'--barplot_eSpec_types_vs_MLT--20160607_eSpec_DB.png'
  plots           = BARPLOT_ESPEC_TYPES_VS_MLT(eSpec, $
                                               PLOTDIR=plotDir, $
                                               /SAVEPLOT, $
                                               SPNAME=bpSaveName)

END