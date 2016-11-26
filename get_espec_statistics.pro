;;11/22/16
;;If Je and Jee are simultaneously requested, a list is returned

FUNCTION GET_ESPEC_STATISTICS, $
   eSpec, $
   eSpec_i, $
   ENUMFLUX=Je, $
   EFLUX=Jee, $
   CHARE=charE, $
   POS_ONLY=pos_only, $
   NEG_ONLY=neg_only, $
   LOG_STATS=log_stats, $
   BPD__OUTLIERS=BPDOutliers, $
   BPD__SUSPECTED_OUTLIERS=BPDSusOutliers, $
   STATS_NAME_SUFF=stats_name_suff, $
   OUT_STATS_NAME=out_stats_name, $
   OUT_I=out_i

  COMPILE_OPT IDL2

  IF KEYWORD_SET(log_stats) THEN BEGIN
     IF ~(KEYWORD_SET(pos_only) OR KEYWORD_SET(neg_only)) THEN BEGIN
        PRINT,'GET_ESPEC_STATISTICS: Neither POS_ONLY nor NEG_ONLY has been set, and yet you want log stats. I set POS_ONLY for you.'
        pos_only = 1
     ENDIF
  ENDIF

  IF KEYWORD_SET(Je) AND KEYWORD_SET(Jee) THEN eSpec_stats = LIST()

  IF ~(KEYWORD_SET(Je) OR KEYWORD_SET(Jee)) THEN BEGIN
  ENDIF

  CASE 1 OF
     KEYWORD_SET(Je): BEGIN
        eDat = eSpec.je
        stats_name = 'Je'
     END
     KEYWORD_SET(Jee): BEGIN
        eDat = eSpec.jee
        stats_name = 'Jee'
     END
     KEYWORD_SET(charE): BEGIN
        eDat = eSpec.je/eSpec.jee
        stats_name = 'charE'
     END
     ELSE: BEGIN
        PRINT,"No request for Je, Jee, or charE. I'll do Je."
        eDat = eSpec.je
        stats_name = 'Je'
     END
  ENDCASE


  discret_inds = GET_POS_NEG_INDICES(data, $
                                     USER_INDS=eSpec_i, $
                                     POS_ONLY=pos_only, $
                                     NEG_ONLY=neg_only, $
                                     INCLUDE_WHERE_EQ_0=inc_0, $
                                     FINITE=finite, $
                                     ;; RETURN_STRUCT=struct, $
                                     OUT_STATS_NAME=stats_name, $
                                     /VERBOSE)

  ;; CASE 1 OF
  ;;    KEYWORD_SET(pos_only): BEGIN
  ;;       discret_inds = CGSETINTERSECTION(eSpec_i,WHERE(eDat GT 0),NORESULT=-1,COUNT=nDiscret)
  ;;       stats_name += '--pos'

  ;;       PRINT,STRCOMPRESS(nDiscret,/REMOVE_ALL) + ' positive inds here'
  ;;       PRINT,"(i.e., " + STRCOMPRESS(N_ELEMENTS(eSpec_i)-nDiscret,/REMOVE_ALL) + " are negative)"
  ;;    END
  ;;    KEYWORD_SET(neg_only): BEGIN
  ;;       discret_inds = CGSETINTERSECTION(eSpec_i,WHERE(eDat LT 0),NORESULT=-1,COUNT=nDiscret)
  ;;       stats_name += '--neg'

  ;;       PRINT,STRCOMPRESS(nDiscret,/REMOVE_ALL) + ' negative inds here'
  ;;       PRINT,"(i.e., " + STRCOMPRESS(N_ELEMENTS(eSpec_i)-nDiscret,/REMOVE_ALL) + " are positive)"
  ;;    END
  ;;    ELSE: BEGIN
  ;;       discret_inds = eSpec_i
  ;;       nDiscret     = N_ELEMENTS(eSpec_i)
  ;;    END
  ;; ENDCASE

  IF KEYWORD_SET(log_stats) THEN BEGIN
     eDat[discret_inds] = ALOG10( ( KEYWORD_SET(neg_only) ? ABS(eDat) : eDat )[discret_inds])
     stats_name = 'Log_' + stats_name
  ENDIF

  IF KEYWORD_SET(stats_name_suff) THEN BEGIN
     stats_name += stats_name_suff
  ENDIF

  eSpecStats = GET_BOXPLOT_AND_MOMENT_STATISTICS( $
               eDat, $
               discret_inds, $
               STATS_NAME=stats_name, $
               BPD__OUTLIERS=BPDOutliers, $
               BPD__SUSPECTED_OUTLIERS=BPDSusOutliers)

  IF ARG_PRESENT(out_i) THEN BEGIN
     out_i = TEMPORARY(discret_inds)
  ENDIF

  IF ARG_PRESENT(out_stats_name) THEN BEGIN
     out_stats_name = TEMPORARY(stats_name)
  ENDIF

  RETURN,eSpecStats

END
