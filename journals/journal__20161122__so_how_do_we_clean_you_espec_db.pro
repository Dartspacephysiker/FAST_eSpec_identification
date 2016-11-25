;;11/22/16
;;This is legitimately the question of the day. How do we clean this little chocolatier?
;;
;;I ran it like this:
;;JOURNAL__20161122__SO_HOW_DO_WE_CLEAN_YOU_ESPEC_DB,/SHOW_PLOTS,/LOG_PLOTS,WINDOW_INDEX=1,OUT_ESTATS=eStats
;;JOURNAL__20161122__SO_HOW_DO_WE_CLEAN_YOU_ESPEC_DB,/SHOW_PLOTS,/LOG_PLOTS,WINDOW_INDEX=2,OUT_ESTATS=eStats,/LOG_DATA
;;FOR k=0,N_ELEMENTS(eStats)-1 DO PRINT,eStats[k].name,N_ELEMENTS(eStats[k].bpd.extras.outlier_values)
;;
;;It's nice
PRO JOURNAL__20161122__SO_HOW_DO_WE_CLEAN_YOU_ESPEC_DB, $
   ENUMFLUX=je, $
   EFLUX=jee, $
   CHARE=charE, $
   POS_ONLY=pos_only, $
   NEG_ONLY=neg_only, $
   LOG_PLOTS=log_plots, $
   LOG_STATS=log_stats, $
   WINDOW_INDEX=winInd, $
   OUT_ESTATS=out_eStats, $
   SHOW_PLOTS=show_plots, $
   SAVE_PLOTS=save_plots, $
   ZOOMED_HISTOXRANGE=zoomed_histoXRange, $
   ZOOMED_HISTOYRANGE=zoomed_histoYRange, $
   NORMALIZE_YRANGE=normalize_yRange, $
   USER_INDS=user_inds, $
   USER_PLOTSUFF=user_plotSuff, $
   PLOTDIRSUFF=plotDirSuff, $
   CUSTOM_TITLE=custom_title

  COMPILE_OPT IDL2

  @common__newell_espec.pro

  IF ~(KEYWORD_SET(je) OR KEYWORD_SET(jee) OR KEYWORD_SET(charE)) THEN BEGIN
     je = 1
  ENDIF

  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB,/REDUCED_DB
  ENDIF

  m_name = 'Mono'
  b_name = 'Broad'
  d_name = 'Diffuse'

  CASE 1 OF
     KEYWORD_SET(je): BEGIN
        stats_name    = 'Je'
        structMemName = 'Je'
        IF KEYWORD_SET(zoomed_histoXRange) THEN BEGIN
           xRange = [[1e5,1e12],[1e6,1e12],[1e4,1e12]]
        ENDIF
        IF KEYWORD_SET(zoomed_histoYRange) THEN BEGIN
           yRange = [[0,0.01],[0,0.01],[0,0.005]]
        ENDIF
     END
     KEYWORD_SET(jee): BEGIN
        stats_name    = 'Jee'
        structMemName = 'Jee'
        IF KEYWORD_SET(zoomed_histoXRange) THEN BEGIN
           xRange = [[1e-5,1e3],[1e-5,1e4],[5e-6,1e3]]
        ENDIF
        IF KEYWORD_SET(zoomed_histoYRange) THEN BEGIN
           yRange = [[0,0.01],[0,0.01],[0,0.005]]
        ENDIF
     END
     KEYWORD_SET(charE): BEGIN
        stats_name    = 'charE'
        structMemName = ['Je','Jee']
        IF KEYWORD_SET(zoomed_histoXRange) THEN BEGIN
           xRange = [[1e2,1e5],[1e2,1e5],[1e0,1e5]]
        ENDIF
        IF KEYWORD_SET(zoomed_histoYRange) THEN BEGIN
           yRange = [[0,0.01],[0,0.01],[0,0.001]]
        ENDIF
     END
  ENDCASE

  ESPEC_STATS_STRINGS,stats_name,stats_title, $
                      ENUMFLUX=Je, $
                      EFLUX=Jee, $
                      CHARE=charE, $
                      POS_ONLY=pos_only, $
                      NEG_ONLY=neg_only, $
                      LOG_STATS=log_stats

  anc_suff = ''
  IF KEYWORD_SET(zoomed_histoXRange) THEN BEGIN
     anc_suff += '--zoomed_X'
  ENDIF

  IF KEYWORD_SET(zoomed_histoYRange) THEN BEGIN
     anc_suff += '--zoomed_Y'
  ENDIF

  structInd = !NULL
  FOR k=0,N_ELEMENTS(structMemName)-1 DO BEGIN
     STR_ELEMENT,NEWELL__eSpec,structMemName[k],INDEX=tmpStructInd

     IF tmpStructInd LT 0 THEN STOP

     structInd = [structInd,tmpStructInd]
  ENDFOR

  GET_ESPEC_INDS_BY_TYPE,NEWELL__eSpec,m_i,b_i,d_i, $
                         /LISTPLEASE, $
                         OUT_I_LIST=i_list, $
                         N_ARR=NArr, $
                         ONLY_STRICT=only_strict, $
                         ONLY_NONSTRICT=only_nonStrict, $
                         USER_INDS=user_inds, $
                         NO_INDS_AVAILABLE=skip_me, $
                         /QUIET

  IF KEYWORD_SET(user_inds) THEN BEGIN
     IF N_ELEMENTS(user_plotSuff) EQ 0 THEN user_plotSuff = ''
  ENDIF

  ;; IF KEYWORD_SET(zoomed_histoYRange) AND ~KEYWORD_SET(normalize_yRange) THEN BEGIN

  ;; ENDIF

  columns   = 3
  rows      = 1
  !P.MULTI  = [0, columns, rows, 0, 0]

  names       = [m_name,b_name,d_name]

  ;;work with titles
  stats_title = REPLICATE(stats_title,3)
  title       = names + ' ' + stats_title + ' Dist.'
  IF KEYWORD_SET(custom_title) THEN BEGIN
     title[0] = custom_title + '(' + names[0] + ')'
  ENDIF

  eStatsList = LIST()
  FOR k=0,2 DO BEGIN

     tmpStats = GET_ESPEC_STATISTICS( $
                NEWELL__eSpec, $
                i_list[k], $
                ENUMFLUX=Je, $
                EFLUX=Jee, $
                CHARE=charE, $
                POS_ONLY=pos_only, $
                NEG_ONLY=neg_only, $
                LOG_STATS=log_stats, $
                BPD__OUTLIERS=BPDOutliers, $
                BPD__SUSPECTED_OUTLIERS=BPDSusOutliers, $
                STATS_NAME_SUFF=(KEYWORD_SET(stats_name_suff) ? stats_name_suff : '' ) + $
                names[k], $
                OUT_STATS_NAME=out_stats_name, $
                OUT_I=out_i)

     eStatsList.Add,tmpStats

     IF KEYWORD_SET(show_plots) OR KEYWORD_SET(save_plots) THEN BEGIN
        IF k EQ 0 THEN BEGIN
           CASE 1 OF
              KEYWORD_SET(save_plots): BEGIN
                 SET_PLOT_DIR,plotDir, $
                              /FOR_ESPEC_DB, $
                              /ADD_TODAY, $
                              ADD_SUFF=plotDirSuff

                 !P.CHARTHICK = 3
                 !P.THICK = 3

                 ;; @startup
                 ;; POPEN,filename,XSIZE=10,YSIZE=10

                 filNavn = stats_name+anc_suff + ( KEYWORD_SET(user_inds) ? user_plotSuff : '' )
                 PRINT,'Opening ' + filNavn + ' ...'

                 SET_PLOT,'PS'
                 DEVICE,FILE=plotDir+filNavn+'.eps', $
                        /ENCAPSUL, $
                        XSIZE=10, $
                        YSIZE=6, $
                        /INCHES, $
                        YOFFSET=2, $
                        /COLOR
              END
              KEYWORD_SET(show_plots): BEGIN
                 WINDOW,(N_ELEMENTS(winInd) GT 0 ? winInd : 0),XSIZE=1000,YSIZE=700
              END
              ELSE:
           ENDCASE
        ENDIF

        IF skip_me[k] OR (N_ELEMENTS(out_i) LE 1) THEN CONTINUE

        CASE 1 OF
           KEYWORD_SET(log_plots): BEGIN
              eDat = KEYWORD_SET(charE) ? $
                     NEWELL__eSpec.(structInd[1]) / $
                     NEWELL__eSpec.(structInd[0]) * 6.242 * 1.0e11 : $
                     NEWELL__eSpec.(structInd[0])
              IF KEYWORD_SET(neg_only) THEN eDat = ABS(eDat)
              CGHISTOPLOT,(ALOG10( eDat ))[out_i], $
                          ;; TITLE='Log(Je) ' + names[k] + ' dist'
                          ;; TITLE=out_stats_name + ' dist', $
                          TITLE=title[k], $
                          BINSIZE=0.1, $
                          XRANGE=KEYWORD_SET(zoomed_histoXRange) ? $
                          ALOG10(REFORM(xRange[*,k])) : !NULL, $
                          ;; FREQUENCY=KEYWORD_SET(normalize_yRange), $
                          FREQUENCY=KEYWORD_SET(zoomed_histoYRange), $
                          YRANGE=KEYWORD_SET(zoomed_histoYRange) ? $
                          ;; [0,0.1] : !NULL
                          REFORM(yRange[*,k]) : !NULL
              
           END
           ELSE: BEGIN
              CGHISTOPLOT,( KEYWORD_SET(charE) ? $
                            NEWELL__eSpec.(structInd[1]) / $
                            NEWELL__eSpec.(structInd[0]) * 6.242 * 1.0e11 : $
                            NEWELL__eSpec.(structInd[0]) )[out_i], $
                          ;; TITLE='Je ' + names[k] + ' dist', $
                          TITLE=title[k], $
                          XRANGE=KEYWORD_SET(zoomed_histoXRange) ? $
                          REFORM(xRange[*,k]) : !NULL, $
                          ;; FREQUENCY=KEYWORD_SET(normalize_yRange), $
                          FREQUENCY=KEYWORD_SET(zoomed_histoYRange), $
                          YRANGE=KEYWORD_SET(zoomed_histoYRange) ? $
                          [0,0.1] : !NULL
              
           END
        ENDCASE
     ENDIF

  ENDFOR

  IF ARG_PRESENT(out_eStats) THEN BEGIN
     IF N_ELEMENTS(out_eStats) GT 0 THEN BEGIN
        FOR k=0,N_ELEMENTS(eStatsList)-1 DO BEGIN
           out_eStats.Add,eStatsList[k]
        ENDFOR
     ENDIF ELSE BEGIN
        out_eStats = eStatsList
     ENDELSE
  ENDIF

  CASE 1 OF
     KEYWORD_SET(save_plots): BEGIN

        DEVICE,/CLOSE

        EPS2PDF,plotDir+filNavn,/REMOVE_EPS

        SET_PLOT,'X'
     END
     ELSE:
  ENDCASE

END

PRO ESPEC_STATS_STRINGS,stats_name,stats_title, $
                        ENUMFLUX=Je, $
                        EFLUX=Jee, $
                        CHARE=charE, $
                        POS_ONLY=pos_only, $
                        NEG_ONLY=neg_only, $
                        LOG_STATS=log_stats

  CASE 1 OF
     KEYWORD_SET(je): BEGIN
        stats_name    = 'Je'
        stats_title   = 'Number Flux'
     END
     KEYWORD_SET(jee): BEGIN
        stats_name    = 'Jee'
        stats_title   = 'Energy Flux'
     END
     KEYWORD_SET(charE): BEGIN
        stats_name    = 'charE'
        stats_title   = 'Characteristic Energy'
     END
  ENDCASE

  ;;Get filename stuff
  IF KEYWORD_SET(pos_only) OR KEYWORD_SET(neg_only) OR KEYWORD_SET(log_stats) THEN BEGIN
     stats_title += '('
  ENDIF

  CASE 1 OF
     KEYWORD_SET(pos_only): BEGIN
        stats_name  += '--pos'
        stats_title += 'Positive' + ( KEYWORD_SET(log_stats) ? ', ' : '')
     END
     KEYWORD_SET(neg_only): BEGIN
        stats_name  += '--neg'
        stats_title += 'Negative' + ( KEYWORD_SET(log_stats) ? ', ' : '')
     END
     ELSE:
  ENDCASE

  IF KEYWORD_SET(log_stats) THEN BEGIN
     stats_name   = 'Log_' + stats_name
     stats_title += 'Log'
  ENDIF

  ;;Get filename stuff
  IF KEYWORD_SET(pos_only) OR KEYWORD_SET(neg_only) OR KEYWORD_SET(log_stats) THEN BEGIN
     stats_title += ')'
  ENDIF

END