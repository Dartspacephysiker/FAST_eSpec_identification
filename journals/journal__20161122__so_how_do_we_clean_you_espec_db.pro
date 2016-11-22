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
   LOG_PLOTS=log_plots, $
   LOG_DATA=log_data, $
   WINDOW_INDEX=winInd, $
   OUT_ESTATS=out_eStats, $
   SHOW_PLOTS=show_plots

  COMPILE_OPT IDL2

  je     = 1
  jee    = 0

  LOAD_NEWELL_ESPEC_DB,eSpec,/NO_MEMORY_LOAD
  m_name = 'Mono'
  b_name = 'Broad'
  d_name = 'Diffuse'

  CASE 1 OF
     KEYWORD_SET(je): BEGIN
        structMemName = 'Je'
     END
     KEYWORD_SET(jee): BEGIN
        structMemName = 'Jee'
     END
  ENDCASE

  STR_ELEMENT,eSpec,structMemName,INDEX=structInd

  IF structInd LT 0 THEN STOP

  m_i = WHERE(eSpec.mono    EQ 1 OR eSpec.mono  EQ 2,N_m)
  b_i = WHERE(eSpec.broad   EQ 1 OR eSpec.broad EQ 2,N_b)
  d_i = WHERE(eSpec.diffuse EQ 1,N_d)

  columns   = 3
  rows      = 1
  !P.MULTI  = [0, columns, rows, 0, 0]

  IF KEYWORD_SET(show_plots) THEN BEGIN
     WINDOW,winInd,XSIZE=900,YSIZE=700
  ENDIF

  i_list = LIST(TEMPORARY(m_i),TEMPORARY(b_i),TEMPORARY(d_i))
  NArr   = [N_m,N_b,N_d]
  names  = [m_name,b_name,d_name]

  eStatsList = LIST()
  FOR k=0,2 DO BEGIN
     tmp_i_pos = CGSETINTERSECTION(i_list[k],WHERE(eSpec.(structInd) GT 0),COUNT=nTmp)

     ;; PRINT,STRCOMPRESS(nTmp,/REMOVE_ALL) + ' positive ' + names[k] + ' inds here'
     ;; PRINT,"(i.e., " + STRCOMPRESS(NArr[k]-nTmp,/REMOVE_ALL) + " are negative)"

     tmpStats = GET_ESPEC_STATISTICS( $
                eSpec, $
                i_list[k], $
                ENUMFLUX=Je, $
                EFLUX=Jee, $
                POS_ONLY=pos_only, $
                NEG_ONLY=neg_only, $
                LOG_DATA=log_data, $
                BPD__OUTLIERS=BPDOutliers, $
                BPD__SUSPECTED_OUTLIERS=BPDSusOutliers, $
                STATS_NAME_SUFF=(KEYWORD_SET(stats_name_suff) ? stats_name_suff : '' ) + names[k], $
                OUT_STATS_NAME=out_stats_name, $
                OUT_I=out_i)

     eStatsList.Add,tmpStats

     IF KEYWORD_SET(show_plots) THEN BEGIN
        CASE 1 OF
           KEYWORD_SET(log_plots): BEGIN
              CGHISTOPLOT,(ALOG10(eSpec.(structInd)))[out_i], $
                          ;; TITLE='Log(Je) ' + names[k] + ' dist'
                          TITLE=out_stats_name + ' dist'

           END
           ELSE: BEGIN
              CGHISTOPLOT,eSpec.(structInd)[out_i], $
                          TITLE='Je ' + names[k] + ' dist'
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

  ;; ;;mono
  ;; CGHISTOPLOT,eSpec.je[m_i], $
  ;;             TITLE=m_name + ' dist'
  ;; ;;broad
  ;; CGHISTOPLOT,eSpec.je[b_i], $
  ;;             TITLE=b_name + ' dist'

  ;; ;;diffuse
  ;; CGHISTOPLOT,eSpec.je[d_i], $
  ;;             TITLE=d_name + ' dist'

END
