;;02/06/17
PRO JOURNAL__20170206__SHRINK_MEMBERS_OF_HUGE_ESPEC_TO_FLOAT_NOT_DOUBLE

  COMPILE_OPT IDL2,STRICTARRSUBS

  LOAD_NEWELL_ESPEC_DB,eSpec, $
                       NEWELLDBDIR=NewellDBDir, $
                       NEWELLDBFILE=NewellDBFile, $
                       /DONT_CONVERT_TO_STRICT_NEWELL, $
                       /DONT_PERFORM_CORRECTION, $
                       /DONT_MAP_TO_100KM, $
                       /NO_MEMORY_LOAD

  tmpMLT = FLOAT(eSpec.mlt)
  STR_ELEMENT,eSpec,'mlt',TEMPORARY(tmpMLT),/ADD_REPLACE
  
  tmpILAT = FLOAT(eSpec.ilat)
  STR_ELEMENT,eSpec,'ilat',TEMPORARY(tmpILAT),/ADD_REPLACE
  
  tmpALT = FLOAT(eSpec.alt)
  STR_ELEMENT,eSpec,'alt',TEMPORARY(tmpALT),/ADD_REPLACE

  tmpOrb = UINT(eSpec.orbit)
  STR_ELEMENT,eSpec,'orbit',TEMPORARY(tmpOrb),/ADD_REPLACE

  PRINT,"Check it, make sure it looks OK"
  PRINT,"If you wanna save, use SAVE,eSpec,FILENAME=NewellDBDir+NewellDBFile"
  HELP,eSpec
  STOP

END
