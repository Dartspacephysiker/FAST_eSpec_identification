;;11/11/16
PRO JOURNAL__20161111__ARMISTICE_SEARCH_FOR_PREMIDNIGHT_MONOENERGETIC_AURORA

  COMPILE_OPT IDL2

  LOAD_NEWELL_ESPEC_DB,eSpec,/DONT_CONVERT_TO_STRICT_NEWELL, $
                       /DONT_LOAD_IN_MEMORY

  mltRange = [21,24]
  orbRange = [1000,7000]
  altRange = [2000,4300]

  mltI     = GET_MLT_INDS(eSpec,mltRange[0],mltRange[1])
  orbI     = GET_ORBRANGE_INDS(eSpec,orbRange[0],orbRange[1])
  altI     = GET_ALTITUDE_INDS(eSpec,altRange[0],altRange[1])
  monoI    = WHERE((eSpec.mono EQ 1) OR (eSpec.mono) EQ 2,nMono)

  mltI     = CGSETINTERSECTION(mltI,TEMPORARY(orbI),COUNT=count)
  mltI     = CGSETINTERSECTION(mltI,TEMPORARY(altI),COUNT=count)
  mltI     = CGSETINTERSECTION(mltI,TEMPORARY(monoI),COUNT=count)

  PRINT,count,' inds to work with'

  STOP

  masterOrb  = eSpec.orbit[mltI]
  masterTime = (TEMPORARY(eSpec.x))[mltI]

  GET_DOUBLE_STREAKS__NTH_DECIMAL_PLACE,masterTime,ALOG10(5.0), $
                                        START_I=strt_i, $
                                        STOP_I=stop_i, $
                                        STREAKLENS=streakLens, $
                                        T_STREAKLENS=tStreak, $
                                        MIN_T_STREAKLEN=20, $
                                        /PRINT_START_STOP_TIMES

END
