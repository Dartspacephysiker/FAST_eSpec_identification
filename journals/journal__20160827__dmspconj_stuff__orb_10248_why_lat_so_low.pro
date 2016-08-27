;;08/27/16
PRO JOURNAL__20160827__DMSPCONJ_STUFF__ORB_10248_WHY_LAT_SO_LOW

  COMPILE_OPT IDL2

  quiet = 0

  orbArr   = [10247,10248]

  dmspConj_tRange = [922367040.0000,922368240.0000]

  FOREACH orb,orbArr DO BEGIN
     PRINT,"Orbit " + STRCOMPRESS(orb,/REMOVE_ALL)

     e = LOAD_JE_AND_JE_TIMES_FOR_ORB(orb, $
                                      JE_OUT=je, $
                                      TIME_RANGES_OUT=time_ranges, $
                                      TIME_RANGE_INDICES_OUT=time_range_indices, $
                                      NINTERVALS_OUT=number_of_intervals, $
                                      OUT_JEFILENAME=jeFileName, $
                                      /CLEAN_DUPES, $
                                      QUIET=quiet)



     GET_FA_ORBIT,je.x,/TIME_ARRAY

     time_i  = WHERE((je.x GE dmspConj_tRange[0]) AND (je.x LE dmspConj_tRange[1]),nTime)

     IF nTime EQ 0 THEN BEGIN
        PRINT,'No data here ...'
        CONTINUE
     ENDIF

     GET_DATA,'ALT',DATA=alt
     GET_DATA,'ILAT',DATA=ilat
     GET_DATA,'MLT',DATA=mlt

     PRINT,FORMAT='(A0,T30,A0)',"Time","ILAT"
     FOREACH tInd,time_i DO PRINT,FORMAT='(A0,T30,F0.2)', $
                                  TIME_TO_STR(ilat.x[tInd],/MS),ilat.y[tInd]

  ENDFOREACH

END
