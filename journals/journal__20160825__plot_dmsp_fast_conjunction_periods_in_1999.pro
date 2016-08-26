;2016/08/25
;List of DMSP/FAST conjunction times in 1999, provided by Ryan McGranaghan
;; Dawnward
;;    March 24-25       23:49:00.000-00:09:00.000
;;    March 25          13:04:00.000-13:24:00.000    

;; Duskward
;;    February 17       18:37:00.000-18:57:00.000
;;    September 15      04:29:00.000-04:49:00.000
;;    September 28      03:15:00.000-03:35:00.000
;;    October 22        11:23:00.000-11:43:00.000
PRO JOURNAL__20160825__PLOT_DMSP_FAST_CONJUNCTION_PERIODS_IN_1999

  COMPILE_OPT idl2

  ;;DB to check out
  dbDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  dbFile        = 'eSpec_20160607_db--orbs_500-16361--BELOW_2000km--with_alternate_coords.sav'

  print_orbits  = 1

  RESTORE,dbDir+dbFile

  startString = ['1999-03-24/23:49:00.000', $
                 '1999-03-25/13:04:00.000', $
                 '1999-02-17/18:37:00.000', $
                 '1999-09-15/04:29:00.000', $
                 '1999-09-28/03:15:00.000', $
                 '1999-10-22/11:23:00.000']


  stopString = ['1999-03-25/00:09:00.000', $
                '1999-03-25/13:24:00.000', $
                '1999-02-17/18:57:00.000', $
                '1999-09-15/04:49:00.000', $
                '1999-09-28/03:35:00.000', $
                '1999-10-22/11:43:00.000']

  nPeriods      = N_ELEMENTS(stopString)

  startStopStr  = [TRANSPOSE(startString), $
                   TRANSPOSE(stopString)]

  startStop_UTC = [TRANSPOSE(STR_TO_TIME(TEMPORARY(startString))), $
                   TRANSPOSE(STR_TO_TIME(TEMPORARY(stopString)))]

  diff_UTC      = [startStop_UTC[1,*]-startStop_UTC[0,*]]


  FOR j=0,nPeriods-1 DO BEGIN
     PRINT,'Period ' + STRCOMPRESS(j+1,/REMOVE_ALL) + '/' + $
           STRCOMPRESS(nPeriods,/REMOVE_ALL) + ' ...'

     IF KEYWORD_SET(print_orbits) THEN BEGIN
        GET_FA_ORBIT,startStop_UTC[0,j],startStop_UTC[1,j]
        GET_DATA,'ORBIT',DATA=orbit
        CHECK_SORTED,orbit.x,isSort,SORTED_I=sort_i
        IF ~isSort THEN orbit = {x:orbit.x[sort_i],y:orbit.y[sort_i]}
        orbits  = orbit.y[UNIQ(orbit.y)]
        nOrbs   = N_ELEMENTS(UNIQ(orbit.y))
        PRINT,FORMAT='("Orbits: ",10(I0,:,", "))',orbits
     ENDIF

     JOURNAL__20160825__STEREO_AND_HISTOPLOTS__AACGM_V2_VS_COORDS_PROVIDED_BY_SDT, $
        IN_ESPEC=eSpec, $
        T1=startStop_UTC[0,j], $
        T2=startStop_UTC[1,j], $
        HEMI='BOTH', $
        /SAVEPLOT, $
        /STEREO_PLOTS, $
        /HISTO_PLOTS, $
        /HISTO_ONEORB, $
        /BATCH
     
  ENDFOR

  STOP

END