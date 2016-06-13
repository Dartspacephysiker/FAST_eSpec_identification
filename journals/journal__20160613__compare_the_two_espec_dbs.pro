;;06/13/16
PRO JOURNAL__20160613__COMPARE_THE_TWO_ESPEC_DBS

  COMPILE_OPT IDL2

  inDir    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  ;;The file with failcodes
  inFile1  = 'eSpec_failCodes_20160609_db--PARSED--Orbs_500-16361.sav' ;;This file does not need to be cleaned
  inFile2  = 'eSpec_20160607_db--PARSED--Orbs_500-16361.sav'           ;;This file does not need to be cleaned

  RESTORE,inDir+inFile1

  monoF    = eSpec.mono
  broadF   = eSpec.broad
  diffuseF = eSpec.diffuse

  eSpec    = !NULL
  failCode = !NULL

  RESTORE,inDir+inFile2
  mono     = eSpec.mono
  broad    = eSpec.broad
  diffuse  = eSpec.diffuse

  eSpec    = !NULL

  monoU    = CGSETINTERSECTION(WHERE(monoF EQ 1 OR monoF EQ 2,nMF),WHERE(mono EQ 1 OR mono EQ 2,nM))
  broadU   = CGSETINTERSECTION(WHERE(broadF EQ 1 OR broadF EQ 2,nBF),WHERE(broad EQ 1 OR broad EQ 2,nB),COUNT=nBU)
  diffuseU = CGSETINTERSECTION(WHERE(diffuseF EQ 1,nDF),WHERE(diffuse EQ 1,nD),COUNT=nDU)


  STOP
END
