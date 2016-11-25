;;11/25/16
FUNCTION GET_NEWELL_DB_STRING,eSpec

  COMPILE_OPT IDL2

  RETURN,'eSpecDB_' + $
         eSpec.info.DB_DATE + '_' + $
         (eSpec.info.DB_version).Replace('.','_') + '--' + $
         eSpec.info.DB_extras.Replace('/','--')

END

