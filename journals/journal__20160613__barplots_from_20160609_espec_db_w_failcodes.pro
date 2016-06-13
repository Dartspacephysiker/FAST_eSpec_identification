PRO JOURNAL__20160613__BARPLOTS_FROM_20160609_ESPEC_DB_W_FAILCODES

  inDir    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  ;;The file with failcodes
  inFile   = 'eSpec_failCodes_20160609_db--PARSED--Orbs_500-16361.sav' ;;This file does not need to be cleaned

  RESTORE,inDir+inFile
  CONVERT_ESPEC_TO_STRICT_NEWELL_INTERPRETATION,eSpec,eSpec,/HUGE_STRUCTURE

  SET_PLOT_DIR,/FOR_ESPECDB,/ADD_TODAY

  yRange          = [0.0,1.03]

  ;;By altitude
  bpSaveName      = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)+'--barplot_eSpec_types_vs_altitude--20160609_eSpec_DB_w_failCodes.png'
  plots           = BARPLOT_ESPEC_TYPES_VS_ALT(eSpec, $
                                               PLOTDIR=plotDir, $
                                               YRANGE=yRange, $
                                               /SAVEPLOT, $
                                               SPNAME=bpSaveName)

  bpSaveName      = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)+'--barplot_eSpec_types_vs_MLT--20160609_eSpec_DB_w_failCodes.png'
  plots           = BARPLOT_ESPEC_TYPES_VS_MLT(eSpec, $
                                               PLOTDIR=plotDir, $
                                               YRANGE=yRange, $
                                               /SAVEPLOT, $
                                               SPNAME=bpSaveName)

END