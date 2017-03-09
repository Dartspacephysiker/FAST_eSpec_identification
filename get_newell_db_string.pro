;;11/25/16
FUNCTION GET_NEWELL_DB_STRING,elOrI

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; RETURN,'eSpecDB_' + $
  ;;        eSpec.info.DB_DATE + '_' + $
  ;;        (eSpec.info.DB_version).Replace('.','_') + '--' + $
  ;;        eSpec.info.DB_extras.Replace('/','--')

  RETURN,GET_FAST_DB_STRING(elOrI,/FOR_ESPEC_DB,/FOR_ION_DB)

END

