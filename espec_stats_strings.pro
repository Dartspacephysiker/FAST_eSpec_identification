;;11/26/16
PRO ESPEC_STATS_STRINGS,stats_name,stats_title, $
                        ENUMFLUX=Je, $
                        EFLUX=Jee, $
                        CHARE=charE, $
                        POS_ONLY=pos_only, $
                        NEG_ONLY=neg_only, $
                        LOG_STATS=log_stats

  COMPILE_OPT idl2

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
