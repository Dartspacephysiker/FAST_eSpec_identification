;;01/02/17
;;Well, according to the FAST website, things started getting troublesome with the ESA data in June 2000 because of some mag field
;;phase error. So let's just use stuff before orbit 14973
PRO JOURNAL__20170102__WHICH_ORBITS_AT_REALLY_LOW_LATITUDES

  COMPILE_OPT IDL2,STRICTARRSUBS

  @common__newell_espec.pro 

  LOAD_NEWELL_ESPEC_DB,/LOAD_DELTA_T,/REDUCED_DB

  orbit = LONG(newell__espec.orbit)
  ilat  = FLOAT(newell__espec.ilat)
  mlt   = FLOAT(newell__espec.mlt)
  alt   = (TEMPORARY(newell__espec)).alt


  minI  = -55
  maxI  = -48

  minM  = 0
  maxM  = 3
  this  = WHERE(mlt GT minM AND mlt LT maxM AND ilat LT maxI AND ilat GT minI)
  lowis = WHERE(ABS(ilat) LT 60 AND ilat LT 0)
  lowin = WHERE(ABS(ilat) LT 60 AND ilat GT 0)

  uniq_i   = UNIQ(orbit[this])
  uniqOrbs = orbit[this[uniq_i]]

  FOR k=0,N_ELEMENTS(uniq_i)-1 DO BEGIN

     PRINT, $
        uniqOrbs[k], $
        N_ELEMENTS(WHERE(orbit[this] EQ uniqOrbs[k]))

  ENDFOR

  !P.MULTI = [0,1,2,0,0]

  CGHISTOPLOT,orbit[lowin]
  CGHISTOPLOT,orbit[lowis]

  
END
