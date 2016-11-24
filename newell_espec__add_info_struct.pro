;;11/24/16
;;Update the eSpec DBs with an info struct
PRO NEWELL_ESPEC__ADD_INFO_STRUCT,eSpec, $
                                  DB_DATE=DB_date, $
                                  DB_VERSION=DB_version, $
                                  DB_EXTRAS=DB_extras, $
                                  REDUCE_DBSIZE=reduce_dbSize, $
                                  IS_ALFNEWELL=is_AlfNewell

  COMPILE_OPT IDL2

  info  = {converted        : 0B, $
           Newell2009interp : 0B, $
           correctedFluxes  : 0B, $
           is_reduced       : BYTE(KEYWORD_SET(reduce_dbSize)), $
           DB_date          : '', $
           DB_version       : '', $
           DB_extras        : '', $
           is_AlfNewell     : BYTE(KEYWORD_SET(is_AlfNewell))}


  IF KEYWORD_SET(DB_date) THEN BEGIN
     info.DB_date    = DB_date
  ENDIF

  IF KEYWORD_SET(DB_version) THEN BEGIN
     info.DB_version = DB_version
  ENDIF

  IF KEYWORD_SET(DB_extras) THEN BEGIN
     info.DB_extras  = DB_extras
  ENDIF

  ;;Now see whether to replace or just append the thing
  STR_ELEMENT,eSpec,"info",INDEX=infoIndex
  IF infoIndex LT 0 THEN BEGIN
     eSpec = CREATE_STRUCT(eSpec,"info",info)
  ENDIF ELSE BEGIN
     STR_ELEMENT,eSpec.info,'converted',INDEX=convIndex
     IF convIndex LT 0 THEN BEGIN
        tmpInfo = eSpec.info
        tmpInfo = CREATE_STRUCT(TEMPORARY(info),tmpInfo)
        STR_ELEMENT,eSpec,'info',TEMPORARY(tmpInfo),/ADD_REPLACE
     ENDIF ELSE BEGIN
        PRINT,"This database appears to have 'info' already! You may care to inspect."
        STOP
     ENDELSE
  ENDELSE


END
