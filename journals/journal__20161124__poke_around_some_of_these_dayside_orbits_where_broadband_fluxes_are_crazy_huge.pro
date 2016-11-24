;;11/24/16
;;These inds were produced while running JOURNAL__20161103__ZHANG_2014__NEWELLDB__EQUAL_AREA_BINNING
;;Look for Blacklisted_orbits.txt. It'll tell you things.
PRO JOURNAL__20161124__POKE_AROUND_SOME_OF_THESE_DAYSIDE_ORBITS_WHERE_BROADBAND_FLUXES_ARE_CRAZY_HUGE

  COMPILE_OPT IDL2

  boxingRound = 2

  indDir  = '/SPENCEdata/Research/Satellites/FAST/espec_identification/saves_output_etc/'
  indFile = '20161123--nonalfven_espec_i_and_paramstrings--journal_20161103_zhang_2014--each_clockangle.sav'

  LOAD_NEWELL_ESPEC_DB,!NULL, $
                       /REDUCED_DB

  @common__newell_espec.pro

  orbRange = [1000,10800]
  altitudeRange = [1000,4300]
  hemi     = 'NORTH'
  minMLT   = 14
  maxMLT   = 17.5
  minILAT  = 60
  maxILAT  = 70

  check_i = GET_ESPEC_ION_DB_IND(Newell__eSpec, $
                                 /GET_ESPEC_I_NOT_ION_I, $
                                 ALTITUDERANGE=altitudeRange, $
                                 ORBRANGE=orbRange, $
                                 HEMI=hemi, $
                                 MINMLT=minMLT, $
                                 MAXMLT=maxMLT, $
                                 MINILAT=minILAT, $
                                 MAXILAT=maxILAT)

  RESTORE,indDir+indFile

  i_list    = TEMPORARY(indices__nonalfven_espec_list) 
  pStr_list = TEMPORARY(paramstring_list)

  CASE boxingRound OF
     1: BEGIN
        listthis  = [0,6,7] ;the one with dawn-north
     END
     2: BEGIN
        listthis  = [1,2,3,5]   ;the one with dusk-south
     END
  ENDCASE

  orb_jerks = LIST()
  final_i_list = LIST()
  FOR k=0,N_ELEMENTS(listthis)-1 DO BEGIN

     ;; inds      = (TEMPORARY(i_list))[listthis[k]]
     ;; pStr      = (TEMPORARY(pStr_list))[listthis[k]]

     inds      = i_list[listthis[k]]
     pStr      = pStr_list[listthis[k]]
     nGood     = N_ELEMENTS(inds)

     PRINT,"Doing " + pStr + ' ...'

     ;; PRINT,"Start with " + STRCOMPRESS(nGood,/REMOVE_ALL) + " inds ..."
     ;; inds      = CGSETINTERSECTION(inds,WHERE( ( Newell__eSpec.broad EQ 1 ) OR ( Newell__eSpec.broad EQ 2 ) ),COUNT=count,NORESULT=-1)
     ;; PRINT,"... But drop " + STRCOMPRESS(nGood-count,/REMOVE_ALL) + " because they're not broad ..."

     ;; nGood     = count
     inds      = CGSETINTERSECTION(inds,check_i,COUNT=count,NORESULT=-1)
     PRINT,"... And another " + STRCOMPRESS(nGood-count,/REMOVE_ALL) + " because they're not in the right place."

     CHECK_SORTED,inds,is_sorted

     IF ~is_sorted THEN STOP

     uniq_ii   = UNIQ(Newell__eSpec.orbit[inds])
     uniqOrbs  = Newell__eSpec.orbit[inds[uniq_ii]]

     orb_jerks.Add,TEMPORARY(uniqOrbs)

     final_i_list.Add,inds

  ENDFOR

  final_i = LIST_TO_1DARRAY(final_i_list,/SKIP_NANS)
  final_i_list = !NULL

  final_ii = UNIQ(final_i,SORT(final_i))
  final_i  = final_i[final_ii]

  ;; masterJerk = orb_jerks.ToArray()

  ;; CHECK_DUPES,masterJerk,/PRINTDUPES

  ;; FOR k=1,N_ELEMENTS(orb_jerks)-1 DO BEGIN
  ;;    masterJerk = CGSETINTERSECTION(masterJerk,orb_jerks[k],NORESULT=-1)
  ;;    IF masterjerk[0] EQ -1 THEN STOP
  ;; ENDFOR

  ;; PRINT,FORMAT='("Master jerks: ",6(I0,:,","))',masterJerk

  JOURNAL__20161122__SO_HOW_DO_WE_CLEAN_YOU_ESPEC_DB, $
     /EFLUX, $
     /LOG_PLOTS, $
     /LOG_STATS, $
     /SAVE_PLOTS, $
     OUT_ESTATS=eStats, $
     USER_INDS=final_i, $
     USER_PLOTSUFF='dawnward_IMF--Pynesfyll'

  huge_jee_i = CGSETINTERSECTION(final_i,WHERE( ( ALOG10(ABS(Newell__eSpec.jee)) GT 1.0 ) AND ( Newell__eSpec.jee GT 0 )),NORESULT=-1)
  IF huge_jee_i[0] EQ -1 THEN PRINT,"You're lying to me"

  uniq_ii       = UNIQ(Newell__eSpec.orbit[huge_jee_i])  
  huge_jee_orbs = Newell__eSpec.orbit[huge_jee_i[uniq_ii]]

  FOR k=0,N_ELEMENTS(huge_jee_orbs)-1 DO BEGIN
     JOURNAL__20161122__SO_HOW_DO_WE_CLEAN_YOU_ESPEC_DB, $
        /EFLUX, $
        /LOG_PLOTS, $
        /LOG_STATS, $
        /SAVE_PLOTS, $
        OUT_ESTATS=eStats, $
        USER_INDS=WHERE(Newell__eSpec.orbit EQ huge_jee_orbs[k]), $
        USER_PLOTSUFF='--round_' + $
        STRCOMPRESS(boxingRound,/REMOVE_ALL) + '__orb_' $
        + STRCOMPRESS(huge_jee_orbs[k],/REMOVE_ALL)
  ENDFOR

  STOP

END
