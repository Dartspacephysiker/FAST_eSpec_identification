;;06/14/16
PRO JOURNAL__20160614__JUST_NEWELL_PROBOCCURRENCE_TO_SEE_WHATS_GOING_ON

  COMPILE_OPT IDL2

  do_despunDB  = 1

  logAvgPlot   = 0

  do_not_consider_IMF = 1

  ;;Non-Alfvén options
  nonAlfven__newellPlot_probOccurrence = 1  
  nonAlfven__newell_plotRange          = [[0.0,0.2], $
                                          [0.0,0.2], $
                                          [0.6,1.0]]
  ;; nonAlfven_flux_plots  = 1
  nonAlfven__all_fluxes = 1
  ;; newell_analyze_multiply_by_type_probability = 1
  ;; newell_analyze_eFlux  = 1

  ;;Gross rates?
  do_grossRate_fluxQuantities = 0
  do_timeAvg_fluxQuantities   = 0

  ;;Plots to do
  ePlots                   = 0
  eNumFlPlots              = 0
  pPlots                   = 0
  ionPlots                 = 0
  probOccurrencePlot       = 0


  ;;;;;;;;;;;;;;;;;;;;;;
  ;;49--PFLUXEST
  pPlotRange                     = [5.e5,5.e8] ;for pFlux divided by width_x and multiplied by area
  logPFPlot                      = 1

  eFluxPlotType            = 'eflux_nonalfven'
  ;; logEfPlot                = 1
  ;; ePlotRange               = [5e7,10^(9.0)]
  logEfPlot                = 0
  ePlotRange               = [0,1]
  noNegEflux               = 1

  eNumFlPlotType           = 'enumf_nonalfven'
  noNegENumFl              = 1
  ;; logENumFlPlot            = [1,1]
  ;; ENumFlPlotRange          = [[1e-1,1e1], $
  ;;                             [1e7,1e9]]
  ;; logENumFlPlot            = 1
  ;; ENumFlPlotRange          = [1e23,3e24]
  logENumFlPlot            = 0
  ENumFlPlotRange          = [1e7,3e8]
  

  ;; logPfPlot                = 1
  ;; PPlotRange               = [1e-1,1e1]
  logPfPlot                = 0
  PPlotRange               = [1e7,2e8]

  ;;;;;;;;;;;;;;;;;;;;;;
  ;;18-INTEG_ION_FLUX_UP
  ifluxPlotType            = 'if_nonalfven_Up'
  noNegIflux               = 1
  logIfPlot                = 1
  iPlotRange               = [2.e22,5e23]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;ProbOccurrence
  logProbOccurrence        = 0
  probOccurrenceRange      = [0,0.1]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Tiled plot options

  reset_good_inds          = 1

  ;; altRange                 = [[340,1180], $
  ;;                             [1180,2180], $
  ;;                             [2180,3180], $
  ;;                             [3180,4180]]

  ;; altRange                 = [[3675,4180]]
  ;; altitudeRange            = [300,1000]
  ;; altitudeRange            = [1000,2000]
  ;; altitudeRange            = [2000,3000]
  ;; altitudeRange            = [3000,4180]
  altitudeRange            = [4000,4180]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;IMF condition stuff--run the ring!
  ;; btMin                          = 5
  ;; btMax                          = 5

  ;;Delay stuff
  ;; nDelays                        = 1
  ;; delayDeltaSec                  = 1800
  ;; delayArr                       = (INDGEN(nDelays,/LONG)-nDelays/2)*delayDeltaSec

  reset_omni_inds                = 1

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;ILAT stuff
  ;; hemi                           = 'NORTH'
  ;; minI                           = 61
  ;; maxI                           = 87

  hemi                           = 'SOUTH'
  minI                           = -87
  maxI                           = -61

  binI                           = 2.0

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;MLT stuff
  binM                           = 0.5
  shiftM                         = 0.0

  ;; minMLT                         = 6
  ;; maxMLT                         = 18

  ;;Bonus
  maskMin                        = 5

  PLOT_ALFVEN_STATS_IMF_SCREENING,maximus, $
                                  RESTRICT_WITH_THESE_I=restrict_with_these_i, $
                                  ORBRANGE=orbRange, $
                                  ALTITUDERANGE=altitudeRange, $
                                  CHARERANGE=charERange, $
                                  CHARIERANGE=charIERange, $ ;;Only applicable for non-Alfvén stuff
                                  POYNTRANGE=poyntRange, $
                                  SAMPLE_T_RESTRICTION=sample_t_restriction, $
                                  NUMORBLIM=numOrbLim, $
                                  MINMLT=minM,MAXMLT=maxM, $
                                  BINMLT=binM, $
                                  SHIFTMLT=shiftM, $
                                  MINILAT=minI,MAXILAT=maxI, $
                                  BINILAT=binI, $
                                  DO_LSHELL=do_lShell,REVERSE_LSHELL=reverse_lShell, $
                                  MINLSHELL=minL,MAXLSHELL=maxL,BINLSHELL=binL, $
                                  MIN_MAGCURRENT=minMC, $
                                  MAX_NEGMAGCURRENT=maxNegMC, $
                                  HWMAUROVAL=HwMAurOval, $
                                  HWMKPIND=HwMKpInd, $
                                  ;; MIN_NEVENTS=min_nEvents, $
                                  MASKMIN=maskMin, $
                                  CLOCKSTR=clockStr, $
                                  DONT_CONSIDER_CLOCKANGLES=dont_consider_clockAngles, $
                                  ANGLELIM1=angleLim1, $
                                  ANGLELIM2=angleLim2, $
                                  BYMIN=byMin, $
                                  BYMAX=byMax, $
                                  BZMIN=bzMin, $
                                  BZMAX=bzMax, $
                                  BTMIN=btMin, $
                                  BTMAX=btMax, $
                                  BXMIN=bxMin, $
                                  BXMAX=bxMax, $
                                  DO_ABS_BYMIN=abs_byMin, $
                                  DO_ABS_BYMAX=abs_byMax, $
                                  DO_ABS_BZMIN=abs_bzMin, $
                                  DO_ABS_BZMAX=abs_bzMax, $
                                  DO_ABS_BTMIN=abs_btMin, $
                                  DO_ABS_BTMAX=abs_btMax, $
                                  DO_ABS_BXMIN=abs_bxMin, $
                                  DO_ABS_BXMAX=abs_bxMax, $
                                  RESET_OMNI_INDS=reset_omni_inds, $
                                  SATELLITE=satellite, $
                                  OMNI_COORDS=omni_Coords, $
                                  HEMI=hemi, $
                                  NORTH=north, $
                                  SOUTH=south, $
                                  BOTH_HEMIS=both_hemis, $
                                  DELAY=delay, $
                                  MULTIPLE_DELAYS=multiple_delays, $
                                  MULTIPLE_IMF_CLOCKANGLES=multiple_IMF_clockAngles, $
                                  RESOLUTION_DELAY=delay_res, $
                                  BINOFFSET_DELAY=binOffset_delay, $
                                  STABLEIMF=stableIMF, $
                                  SMOOTHWINDOW=smoothWindow, $
                                  INCLUDENOCONSECDATA=includeNoConsecData, $
                                  DO_NOT_CONSIDER_IMF=do_not_consider_IMF, $
                                  NONSTORM=nonStorm, $
                                  RECOVERYPHASE=recoveryPhase, $
                                  MAINPHASE=mainPhase, $
                                  DSTCUTOFF=dstCutoff, $
                                  NPLOTS=nPlots, $
                                  EPLOTS=ePlots, $
                                  EPLOTRANGE=ePlotRange, $
                                  EFLUXPLOTTYPE=eFluxPlotType, $
                                  LOGEFPLOT=logEfPlot, $
                                  ABSEFLUX=abseflux, $
                                  NOPOSEFLUX=noPosEFlux, $
                                  NONEGEFLUX=noNegEflux, $
                                  ENUMFLPLOTS=eNumFlPlots, $
                                  ENUMFLPLOTTYPE=eNumFlPlotType, $
                                  LOGENUMFLPLOT=logENumFlPlot, $
                                  ABSENUMFL=absENumFl, $
                                  NONEGENUMFL=noNegENumFl, $
                                  NOPOSENUMFL=noPosENumFl, $
                                  ENUMFLPLOTRANGE=ENumFlPlotRange, $
                                  AUTOSCALE_ENUMFLPLOTS=autoscale_eNumFlplots, $
                                  NEWELL_ANALYZE_EFLUX=newell_analyze_eFlux, $
                                  NEWELL_ANALYSIS__OUTPUT_SUMMARY=newell_analysis__output_summary, $
                                  NEWELL_ANALYZE_MULTIPLY_BY_TYPE_PROBABILITY=newell_analyze_multiply_by_type_probability, $
                                  NONALFVEN_FLUX_PLOTS=nonAlfven_flux_plots, $
                                  NONALFVEN__JUNK_ALFVEN_CANDIDATES=nonAlfven__junk_alfven_candidates, $
                                  NONALFVEN__ALL_FLUXES=nonalfven__all_fluxes, $
                                  PPLOTS=pPlots, $
                                  LOGPFPLOT=logPfPlot, $
                                  ABSPFLUX=absPflux, $
                                  NONEGPFLUX=noNegPflux, $
                                  NOPOSPFLUX=noPosPflux, $
                                  PPLOTRANGE=PPlotRange, $
                                  IONPLOTS=ionPlots, IFLUXPLOTTYPE=ifluxPlotType, LOGIFPLOT=logIfPlot, ABSIFLUX=absIflux, $
                                  NONEGIFLUX=noNegIflux, NOPOSIFLUX=noPosIflux, IPLOTRANGE=IPlotRange, $
                                  OXYPLOTS=oxyPlots, $
                                  OXYFLUXPLOTTYPE=oxyFluxPlotType, $
                                  LOGOXYFPLOT=logOxyfPlot, $
                                  ABSOXYFLUX=absOxyFlux, $
                                  NONEGOXYFLUX=noNegOxyFlux, $
                                  NOPOSOXYFLUX=noPosOxyFlux, $
                                  OXYPLOTRANGE=oxyPlotRange, $
                                  CHAREPLOTS=charEPlots, CHARETYPE=charEType, LOGCHAREPLOT=logCharEPlot, ABSCHARE=absCharE, $
                                  NONEGCHARE=noNegCharE, NOPOSCHARE=noPosCharE, CHAREPLOTRANGE=CharEPlotRange, $
                                  CHARIEPLOTS=chariePlots, LOGCHARIEPLOT=logChariePlot, ABSCHARIE=absCharie, $
                                  NONEGCHARIE=noNegCharie, NOPOSCHARIE=noPosCharie, CHARIEPLOTRANGE=ChariePlotRange, $
                                  AUTOSCALE_FLUXPLOTS=autoscale_fluxPlots, $
                                  DIV_FLUXPLOTS_BY_ORBTOT=div_fluxPlots_by_orbTot, $
                                  DIV_FLUXPLOTS_BY_APPLICABLE_ORBS=div_fluxPlots_by_applicable_orbs, $
                                  ORBCONTRIBPLOT=orbContribPlot, $
                                  LOGORBCONTRIBPLOT=logOrbContribPlot, $
                                  ORBTOTPLOT=orbTotPlot, $
                                  ORBFREQPLOT=orbFreqPlot, $
                                  ORBCONTRIBRANGE=orbContribRange, $
                                  ORBCONTRIBAUTOSCALE=orbContribAutoscale, $
                                  ORBTOTRANGE=orbTotRange, $
                                  ORBFREQRANGE=orbFreqRange, $
                                  ORBCONTRIB_NOMASK=orbContrib_noMask, $
                                  NEVENTPERORBPLOT=nEventPerOrbPlot, LOGNEVENTPERORB=logNEventPerOrb, $
                                  NEVENTPERORBRANGE=nEventPerOrbRange, $
                                  NEVENTPERORBAUTOSCALE=nEventPerOrbAutoscale, $
                                  DIVNEVBYTOTAL=divNEvByTotal, $
                                  NEVENTPERMINPLOT=nEventPerMinPlot, $
                                  NEVENTPERMINRANGE=nEventPerMinRange, $
                                  LOGNEVENTPERMIN=logNEventPerMin, $
                                  NEVENTPERMINAUTOSCALE=nEventPerMinAutoscale, $
                                  NORBSWITHEVENTSPERCONTRIBORBSPLOT=nOrbsWithEventsPerContribOrbsPlot, $
                                  NOWEPCO_RANGE=nowepco_range, $
                                  NOWEPCO_AUTOSCALE=nowepco_autoscale, $
                                  LOG_NOWEPCOPLOT=log_nowepcoPlot, $
                                  PROBOCCURRENCEPLOT=probOccurrencePlot, $
                                  PROBOCCURRENCERANGE=probOccurrenceRange, $
                                  PROBOCCURRENCEAUTOSCALE=probOccurrenceAutoscale, $
                                  LOGPROBOCCURRENCE=logProbOccurrence, $
                                  THISTDENOMINATORPLOT=tHistDenominatorPlot, $
                                  THISTDENOMPLOTRANGE=tHistDenomPlotRange, $
                                  THISTDENOMPLOTNORMALIZE=tHistDenomPlotNormalize, $
                                  THISTDENOMPLOTAUTOSCALE=tHistDenomPlotAutoscale, $
                                  THISTDENOMPLOT_NOMASK=tHistDenomPlot_noMask, $
                                  NEWELLPLOTS=newellPlots, $
                                  NEWELL_PLOTRANGE=newell_plotRange, $
                                  LOG_NEWELLPLOT=log_newellPlot, $
                                  NEWELLPLOT_AUTOSCALE=newellPlot_autoscale, $
                                  NEWELLPLOT_NORMALIZE=newellPlot_normalize, $
                                  NEWELLPLOT_PROBOCCURRENCE=newellPlot_probOccurrence, $
                                  NONALFVEN__NEWELLPLOT_PROBOCCURRENCE=nonAlfven__newellPlot_probOccurrence, $
                                  NONALFVEN__NEWELL_PLOTRANGE=nonalfven__newell_plotRange, $
                                  TIMEAVGD_PFLUXPLOT=timeAvgd_pFluxPlot, $
                                  TIMEAVGD_PFLUXRANGE=timeAvgd_pFluxRange, $
                                  LOGTIMEAVGD_PFLUX=logTimeAvgd_PFlux, $
                                  TIMEAVGD_EFLUXMAXPLOT=timeAvgd_eFluxMaxPlot, $
                                  TIMEAVGD_EFLUXMAXRANGE=timeAvgd_eFluxMaxRange, $
                                  LOGTIMEAVGD_EFLUXMAX=logTimeAvgd_EFluxMax, $
                                  DO_TIMEAVG_FLUXQUANTITIES=do_timeAvg_fluxQuantities, $
                                  DO_GROSSRATE_FLUXQUANTITIES=do_grossRate_fluxQuantities, $
                                  DO_GROSSRATE_WITH_LONG_WIDTH=do_grossRate_with_long_width, $
                                  WRITE_GROSSRATE_INFO_TO_THIS_FILE=grossRate_info_file, $
                                  WRITE_ORB_AND_OBS_INFO=write_obsArr_textFile, $
                                  DIVIDE_BY_WIDTH_X=divide_by_width_x, $
                                  MULTIPLY_BY_WIDTH_X=multiply_by_width_x, $
                                  ADD_VARIANCE_PLOTS=add_variance_plots, $
                                  ONLY_VARIANCE_PLOTS=only_variance_plots, $
                                  VAR__PLOTRANGE=var__plotRange, $
                                  VAR__REL_TO_MEAN_VARIANCE=var__rel_to_mean_variance, $
                                  VAR__DO_STDDEV_INSTEAD=var__do_stddev_instead, $
                                  VAR__AUTOSCALE=var__autoscale, $
                                  PLOT_CUSTOM_MAXIND=plot_custom_maxInd, $
                                  CUSTOM_MAXINDS=custom_maxInds, $
                                  CUSTOM_MAXIND_RANGE=custom_maxInd_range, $
                                  CUSTOM_MAXIND_AUTOSCALE=custom_maxInd_autoscale, $
                                  CUSTOM_MAXIND_DATANAME=custom_maxInd_dataname, $
                                  CUSTOM_MAXIND_TITLE=custom_maxInd_title, $
                                  CUSTOM_GROSSRATE_CONVFACTOR=custom_grossRate_convFactor, $
                                  LOG_CUSTOM_MAXIND=log_custom_maxInd, $
                                  SUM_ELECTRON_AND_POYNTINGFLUX=sum_electron_and_poyntingflux, $
                                  MEDIANPLOT=medianPlot, $
                                  LOGAVGPLOT=logAvgPlot, $
                                  ALL_LOGPLOTS=all_logPlots, $
                                  SQUAREPLOT=squarePlot, POLARCONTOUR=polarContour, $ ;WHOLECAP=wholeCap, $
                                  DBFILE=dbfile, $
                                  RESET_GOOD_INDS=reset_good_inds, $
                                  NO_BURSTDATA=no_burstData, $
                                  DATADIR=dataDir, $
                                  CHASTDB=chastDB, $
                                  DO_DESPUNDB=do_despunDB, $
                                  NEVENTSPLOTRANGE=nEventsPlotRange, $
                                  LOGNEVENTSPLOT=logNEventsPlot, $
                                  NEVENTSPLOTAUTOSCALE=nEventsPlotAutoscale, $
                                  NEVENTSPLOTNORMALIZE=nEventsPlotNormalize, $
                                  WRITEASCII=writeASCII, WRITEHDF5=writeHDF5, WRITEPROCESSEDH2D=writeProcessedH2d, $
                                  SAVERAW=saveRaw, RAWDIR=rawDir, $
                                  JUSTDATA=justData, SHOWPLOTSNOSAVE=showPlotsNoSave, $
                                  PLOTDIR=plotDir, $
                                  SUFFIX_PLOTDIR=suffix_plotDir, $
                                  PLOTPREFIX=plotPrefix, $
                                  PLOTSUFFIX=plotSuffix, $
                                  SAVE_ALF_INDICES=save_alf_indices, $
                                  TXTOUTPUTDIR=txtOutputDir, $
                                  SUFFIX_TXTDIR=suffix_txtDir, $
                                  MEDHISTOUTDATA=medHistOutData, $
                                  MEDHISTOUTTXT=medHistOutTxt, $
                                  OUTPUTPLOTSUMMARY=outputPlotSummary, $
                                  DEL_PS=del_PS, $
                                  EPS_OUTPUT=eps_output, $
                                  TILE_IMAGES=tile_images, $
                                  N_TILE_ROWS=n_tile_rows, $
                                  N_TILE_COLUMNS=n_tile_columns, $
                                  TILING_ORDER=tiling_order, $
                                  TILE__FAVOR_ROWS=tile__favor_rows, $
                                  GROUP_LIKE_PLOTS_FOR_TILING=group_like_plots_for_tiling, $
                                  SCALE_LIKE_PLOTS_FOR_TILING=scale_like_plots_for_tiling, $
                                  ;; BLANK_TILE_POSITIONS=blank_tile_positions, $
                                  TILEPLOTSUFF=tilePlotSuff, $
                                  TILEPLOTTITLE=tilePlotTitle, $
                                  NO_COLORBAR=no_colorbar, $
                                  CB_FORCE_OOBHIGH=cb_force_oobHigh, $
                                  CB_FORCE_OOBLOW=cb_force_oobLow, $
                                  FANCY_PLOTNAMES=fancy_plotNames, $
                                  OUT_TEMPFILE_LIST=out_tempFile_list, $
                                  OUT_DATANAMEARR_list=out_dataNameArr_list, $
                                  OUT_PARAMSTRING_LIST=out_paramString_list, $
                                  OUT_PLOT_I_LIST=out_plot_i_list, $
                                  _EXTRA = e



END
