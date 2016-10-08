PRO CLEAR_ESPEC_DB_VARS,QUIET=quiet

  COMMON NEWELL

  IF ~KEYWORD_SET(quiet) THEN PRINT,'Clearing eSpec COMMON vars ...'

  NEWELL__eSpec        = !NULL
  NEWELL__HAVE_GOOD_I  = !NULL
  NEWELL__failCode     = !NULL
  NEWELL__good_i       = !NULL
  NEWELL__charERange   = !NULL

  NEWELL__dbFile       = !NULL
  NEWELL__dbDir        = !NULL
  NEWELL__RECALCULATE  = !NULL

  
END