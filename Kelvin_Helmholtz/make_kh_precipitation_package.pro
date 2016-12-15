;;12/15/16
FUNCTION MAKE_KH_PRECIPITATION_PACKAGE, $
   ENERGY_ELECTRONS=energy_electrons, $
   ELECTRON_ANGLERANGE=electron_angleRange, $
   EEB_OR_EES=eeb_or_ees, $
   MINMLT=minM, $
   MAXMLT=maxM, $
   DAWNSECTOR=dawnSector, $
   DUSKSECTOR=duskSector, $
   DAYSIDE=dayside, $
   NIGHTSIDE=nightside, $
   AURORAL_OVAL=auroral_oval, $
   MINILAT=minI, $
   MAXILAT=maxI, $
   PLOTNAMEPREF=plotNamePref, $
   LOADDIR=loadDir, $
   PLOTDIR=plotDir, $
   OUTFNAME=outFName, $
   DO_NOT_SAVE=noSave

  COMPILE_OPT IDL2

  IF N_ELEMENTS(energy_electrons) EQ 0 THEN BEGIN
     energy_electrons = [30,3e4]
  ENDIF

  IF ~KEYWORD_SET(eeb_or_ees) THEN eeb_OR_ees = 'ees'

  IF KEYWORD_SET(loadDir) THEN BEGIN
     lDir   = loadDir
  ENDIF ELSE BEGIN
     lDir   = '~/software/sdt/batch_jobs/saves_output_etc/'
  ENDELSE

  IF KEYWORD_SET(plotDir) THEN BEGIN
     pDir   = plotDir
  ENDIF ELSE BEGIN
     SET_PLOT_DIR,pDir,/FOR_SDT,/ADD_TODAY
  ENDELSE

  pNamePref        = KEYWORD_SET(plotNamePref) ? plotNamePref : ''

  haveMLTInfo      = KEYWORD_SET(dawnSector) + $
                     KEYWORD_SET(duskSector) + $
                     KEYWORD_SET(dayside) + $
                     KEYWORD_SET(nightside) + $
                     (KEYWORD_SET(minM) AND KEYWORD_SET(maxM))

  haveILATInfo     = (KEYWORD_SET(minI) AND KEYWORD_SET(maxI)) + KEYWORD_SET(hemi)

  CASE haveMLTInfo OF
     0: BEGIN
        minM = 2
        maxM = 10
     END
     1: 
     ELSE: BEGIN
        PRINT,"dawnSector : ",KEYWORD_SET(dawnSector ) ? dawnSector  : 0B
        PRINT,"duskSector : ",KEYWORD_SET(duskSector ) ? duskSector  : 0B
        PRINT,"dayside    : ",KEYWORD_SET(dayside    ) ? dayside     : 0B
        PRINT,"nightside  : ",KEYWORD_SET(nightside  ) ? nightside   : 0B
        PRINT,"minM       : ",KEYWORD_SET(minM       ) ? minM        : 0B
        PRINT,"maxM       : ",KEYWORD_SET(maxM       ) ? maxM        : 0B
        PRINT,""
        PRINT,"Please don't set all of these at once. K? Chill out."
        RETURN,-1
     END
  ENDCASE

  CASE haveILATInfo OF
     0: BEGIN
        PRINT,'Grabbing all ILATs ...'
     END
     1:
     ELSE: BEGIN
        PRINT,"ILAT opts"
        PRINT,"hemi         : ",KEYWORD_SET(hemi        ) ? hemi           : ''
        PRINT,"auroral_oval : ",KEYWORD_SET(auroral_oval) ? auroral_oval   : 0B
        PRINT,"minI         : ",KEYWORD_SET(minI        ) ? minI           : 0B
        PRINT,"maxI         : ",KEYWORD_SET(maxI        ) ? maxI           : 0B
        PRINT,"Please don't set all of these at once. K? Chill out."
        RETURN,-1
     END
  ENDCASE

  ;;Now get some dater
  this             = GET_ESA_TIMERANGES(BURST=(STRUPCASE(eeb_or_ees) EQ 'EEB'), $
                                        AURORAL_OVAL=auroral_oval)

  IF this[0] EQ -1 THEN BEGIN
     PRINT,"Couldn't get ESA timeRanges!"
     RETURN,this
  ENDIF

  GET_DATA,'MLT' ,DATA=mlt
  GET_DATA,'ILAT',DATA=ilat

  mlt_i            = GET_MLT_INDS(!NULL,minM,maxM, $
                                  DAWNSECTOR=dawnSector, $
                                  DUSKSECTOR=duskSector, $
                                  DAYSIDE=dayside, $
                                  NIGHTSIDE=nightside, $
                                  ;; /DAWNSECTOR, $
                                  DIRECT_MLTS=mlt.y)

  IF haveILATInfo THEN BEGIN
     ilat_i           = GET_ILAT_INDS(!NULL,minI,maxI,hemi, $
                                      DIRECT_LATITUDES=ilat.y)
     mlt_i            = CGSETINTERSECTION(mlt_i,ilat_i, $
                                          COUNT=nInds, $
                                          NORESULT=-1)
     IF mlt_i[0] EQ -1 THEN BEGIN
        PRINT,"No indices to grab!"
        RETURN,-1
     ENDIF
  ENDIF

  ;;Now times …
  t1               = mlt.x[mlt_i[0]]
  t2               = mlt.x[mlt_i[-1]]

  PRINT,TIME_TO_STR([t1,t2])

  t2old = t2
  GET_LOSSCONE_AND_EFLUX_DATA,T1=t1,T2=t2Old, $
                              LOAD_DAT_FROM_FILE=diff_eFlux_file, $
                              LOAD_DIR=loadDir, $
                              EEB_OR_EES=eeb_or_ees, $
                              DIFF_EFLUX=diff_eFlux, $
                              SPECTRA_AVERAGE_INTERVAL=spec_avg_intvl, $
                              OUT_ORB=orb, $
                              OUT_ANGLERANGE=e_angle, $
                              /FIT_EACH_ANGLE, $ ;Perma-set because we need all angles here
                              CUSTOM_E_ANGLERANGE=electron_angleRange, $
                              ANGLESTR=angleStr, $
                              ESPECUNITS=eSpecUnits, $
                              ELECTRON_ENERGY_LIMS=energy_electrons, $
                              SAVE_DIFF_EFLUX_TO_FILE=save_diff_eFlux_to_file

  ;;Now filename, if not provided
  GET_DATA,'ORBIT',DATA=orb
  orb              = orb.y[0]
  orbStr           = STRCOMPRESS(orb,/REMOVE_ALL)
  IF KEYWORD_SET(outFName) THEN BEGIN
     outName       = outFName
     saveSuff      = ''
  ENDIF ELSE BEGIN
     saveSuff      = '.sav'
     outName       = 'orb_' + orbStr + '-KH_checkitout-' + $
                     eeb_or_ees + '-'                    + $ 
                     STRING(FORMAT='("eAngle_",I0,"-",I0,"_")', $
                            ROUND(e_angle[0]), $
                            ROUND(e_angle[1]))           + $
                     STRING(FORMAT='("eEnergy_",I0,"-",I0,"_")', $
                            ROUND(energy_electrons[0]), $
                            ROUND(energy_electrons[1]))           + $
                     GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  ENDELSE



  GET_2DT,'je_2d_fs','fa_' + eeb_or_ees + '_c', $
          NAME='Jee', $
          T1=t1, $
          T2=t2, $
          ENERGY=energy_electrons, $
          ANGLE=e_angle, $
          CALIB=(N_ELEMENTS(calib) GT 0 ? calib : 1)

  GET_2DT,'j_2d_fs','fa_' + eeb_or_ees + '_c', $
          NAME='Je', $
          T1=t1, $
          T2=t2, $
          ENERGY=energy_electrons, $
          ANGLE=e_angle, $
          CALIB=(N_ELEMENTS(calib) GT 0 ? calib : 1)

  GET_DATA,'Jee',DATA=Jee
  GET_DATA,'Je',DATA=Je


  IF N_ELEMENTS(Jee.y) NE N_ELEMENTS(Je.y) THEN STOP

  keep = WHERE(FINITE(je.y) AND FINITE(jee.y))
  je.x = je.x[keep]
  je.y = je.y[keep]
  je.x = je.x[keep]
  je.y = je.y[keep]
  
  time_order = SORT(je.x)
  je.x  = je.x[time_order]
  je.y  = je.y[time_order]
  jee.x = jee.x[time_order]
  jee.y = jee.y[time_order]

  GET_FA_ORBIT,Je.x,/TIME_ARRAY
  GET_DATA,'MLT',DATA=mlt
  GET_DATA,'ILAT',DATA=ilat
  GET_DATA,'ALT',DATA=alt

  chare = (jee.y / je.y ) * 6.242 * 1.0e11

  ;;Now some bonus stuff--angles, position, you know
  GET_DATA,'fa_vel',DATA=vel
  speed = SQRT(vel.y[*,0]^2+vel.y[*,1]^2+vel.y[*,2]^2)*1000.0
  
  ;;get position of each mag point
  ;;samplingperiod=magz.x(300)-magz.x(299)
  ;;position=make_array(n_elements(magz.x),/double)
  ;;position=speed(300)*samplingperiod*findgen(n_elements(magz.x))
  ;;speed_mag_point=speed(300)
  
  old_pos        = 0.
  pos            = MAKE_ARRAY(N_ELEMENTS(je.x),/DOUBLE)
  speed_je_point = MAKE_ARRAY(N_ELEMENTS(je.x),/DOUBLE)
  FOR j=0L,N_ELEMENTS(je.x)-2 DO BEGIN

     speed_point_ind   = MIN(ABS(vel.x-je.x[j]),ind)

     speed_je_point[j] = speed[ind]
     samplingperiod    = je.x[j+1]-je.x[j]

     pos[j]            = old_pos+speed_je_point[j]*samplingperiod
     old_pos           = pos[j]
  ENDFOR

  width_angle = SPHDIST(DOUBLE(mlt.y[0:-2])*15.D,DOUBLE(ilat.y[0:-2]), $
                        DOUBLE(mlt.y[1:-1])*15.D,DOUBLE(ilat.y[1:-1]), $
                        /DEGREES)

  angle       = [0,TOTAL(width_angle,/CUMULATIVE)]

  ;;derivatives
  derivs = {charE : {mlt  : DERIV(mlt.y ,charE), $
                     ilat : DERIV(ilat.y,charE), $
                     pos  : DERIV(pos   ,charE), $
                     angle: DERIV(angle ,charE)},$
            je    : {mlt  : DERIV(mlt.y ,je.y ), $
                     ilat : DERIV(ilat.y,je.y ), $
                     pos  : DERIV(pos   ,je.y ), $
                     angle: DERIV(angle ,je.y )},$
            jee   : {mlt  : DERIV(mlt.y ,jee.y), $
                     ilat : DERIV(ilat.y,jee.y), $
                     pos  : DERIV(pos   ,jee.y ),$
                     angle: DERIV(angle ,jee.y)}}

  ;;Some info
  info   = {energy_electrons : energy_electrons                    , $
            e_angle          : e_angle                             , $
            calibrated       : (N_ELEMENTS(calib) GT 0 ? calib : 1), $
            MLTs             : {haveMLTInfo  : haveMLTInfo                                 , $
                                dawnSector   : KEYWORD_SET(dawnSector ) ? dawnSector  : 0B , $
                                duskSector   : KEYWORD_SET(duskSector ) ? duskSector  : 0B , $
                                dayside      : KEYWORD_SET(dayside    ) ? dayside     : 0B , $
                                nightside    : KEYWORD_SET(nightside  ) ? nightside   : 0B , $
                                minM         : KEYWORD_SET(minM       ) ? minM        : 0B , $
                                maxM         : KEYWORD_SET(maxM       ) ? maxM        : 0B}, $
            ILATs            : {haveILATinfo : haveILATInfo                                , $
                                hemi         : KEYWORD_SET(hemi       ) ? hemi          : '' , $
                                auroral_oval : KEYWORD_SET(auroral_oval) ? auroral_oval : 0B , $
                                minI         : KEYWORD_SET(minI       ) ? minI          : 0B , $
                                maxI         : KEYWORD_SET(maxI       ) ? maxI          : 0B}, $
            orbit            : orbStr                              , $
            date             : GET_TODAY_STRING(/DO_YYYYMMDD_FMT)}

  struct = {x      : mlt.x , $
            mlt    : mlt.y , $
            ilat   : ilat.y, $
            je     : je.y  , $
            jee    : jee.y , $
            charE  : chare , $
            derivs : derivs, $
            info   : info    }
  
  IF ~KEYWORD_SET(noSave) THEN BEGIN
     testFName = outName + saveSuff
     counter   = 1
     WHILE FILE_TEST(lDir+testFName) DO BEGIN
        testFName = outName + $
                    STRING(FORMAT='("_",I02)',counter) + $
                    saveSuff
     ENDWHILE
     finalName = testFName
     PRINT,"Saving KH package to " + finalName
     SAVE,struct,FILENAME=lDir+finalName
  ENDIF

  CASE 1 OF
     KEYWORD_SET(return_struct): BEGIN
        RETURN,struct
     END
     ELSE: BEGIN
        RETURN,1
     END
  ENDCASE

END
