;2016/08/27
;The harsh reality is that I just made a batch job this
;morning—alfven_stats_5__just_get_interval_times_and_save__dmspconjjob__20160827.batch—and it appears that there really are almost
;always duplicate Je members in the data. Here are the contents of 20160827--DUPE_REPORT--as5__just_get_interval_times_and_save.txt:
;Orbit    N sorted Je    N duplicates (using UNIQ)
;10242    1201           6
;10247    3184           1
;10248    2218           2
;9859     2222           2
;12137    2736           8
;12278    2922           7
;12543    2277           2
PRO JOURNAL__20160827__CHECK_THE_JE_ON_ORBS_FOR_DMSP_CONJSTUDY__DO_DUPES_MATCH_SDT_BATCH_JOB, $
   ORBLIST=userOrbList

  COMPILE_OPT IDL2

  @common__newell_espec.pro

  orbList = N_ELEMENTS(userOrbList) GT 0 ? userOrbList : $
            [10242, $
             10247, $
             10248, $
             9859, $
             12137, $
             12278, $
             12543]

  PRINT,"***********************************"
  PRINT,"FROM CLEANED_JE_AND_TIMERANGE FILES"
  PRINT,"***********************************"
  PRINT,FORMAT='(A0,T10,A0,T20,A0,T30,A0)',"Orbit","N Orig","N dupes","Sorted"
  FOREACH orbit_num,orbList DO BEGIN
     e = LOAD_JE_AND_JE_TIMES_FOR_ORB(orbit_num, $
                                      JE_OUT=je, $
                                      TIME_RANGES_OUT=time_ranges, $
                                      TIME_RANGE_INDICES_OUT=time_range_indices, $
                                      NINTERVALS_OUT=number_of_intervals, $
                                      OUT_JEFILENAME=jeFileName, $
                                      /QUIET)

     IF ~e THEN BEGIN
        nOrig  = N_ELEMENTS(je.x)
        CHECK_SORTED,je.x,isSort,/QUIET,SORTED_I=sort_i
        nUniq  = N_ELEMENTS(UNIQ(je.x[sort_i]))
        nDupes = nOrig-nUniq
        PRINT,FORMAT='(I0,T10,I0,T20,I0,T30,I0)',orbit_num,nOrig,nDupes,isSort
     ENDIF ELSE PRINT,"Error"
  ENDFOREACH

  ;;The UNsorted electron DB
  LOAD_NEWELL_ESPEC_DB,/DONT_CONVERT_TO_STRICT_NEWELL, $
                       /USE_UNSORTED_FILE
  PRINT,""
  PRINT,"**********************"
  PRINT,"FROM UNSORTED ESPEC DB"
  PRINT,"**********************"
  PRINT,FORMAT='(A0,T10,A0,T20,A0,T30,A0)',"Orbit","N Orig","N dupes","Sorted"
  FOREACH orbit_num,orbList DO BEGIN
     orb_i = WHERE(NEWELL__eSpec.orbit EQ orbit_num,nOrig)

     ;; nOrig  = N_ELEMENTS(je.x)
     CHECK_SORTED,NEWELL__eSpec.x[orb_i],isSort,/QUIET,SORTED_I=sort_ii
     nUniq  = N_ELEMENTS(UNIQ(NEWELL__eSpec.x[orb_i[sort_ii]]))
     nDupes = nOrig-nUniq
     PRINT,FORMAT='(I0,T10,I0,T20,I0,T30,I0)',orbit_num,nOrig,nDupes,isSort

  ENDFOREACH

  ;;The UNUNsorted electron DB
  LOAD_NEWELL_ESPEC_DB,/DONT_CONVERT_TO_STRICT_NEWELL,/FORCE_LOAD_DB
  PRINT,""
  PRINT,"********************"
  PRINT,"FROM SORTED ESPEC DB"
  PRINT,"********************"
  PRINT,FORMAT='(A0,T10,A0,T20,A0,T30,A0)',"Orbit","N Orig","N dupes","Sorted"
  FOREACH orbit_num,orbList DO BEGIN
     orb_i = WHERE(NEWELL__eSpec.orbit EQ orbit_num,nOrig)

     ;; nOrig  = N_ELEMENTS(je.x)
     CHECK_SORTED,NEWELL__eSpec.x[orb_i],isSort,/QUIET,SORTED_I=sort_ii
     nUniq  = N_ELEMENTS(UNIQ(NEWELL__eSpec.x[orb_i[sort_ii]]))
     nDupes = nOrig-nUniq
     PRINT,FORMAT='(I0,T10,I0,T20,I0,T30,I0)',orbit_num,nOrig,nDupes,isSort

  ENDFOREACH

END
