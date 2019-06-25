;2019/06/24
PRO JOURNAL__20190624__CHECK_AVAIL_OF_QUANTS_FOR_OUTFLOW_PAPER

  COMPILE_OPT IDL2,STRICTARRSUBS


  outDir       = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/v2018/'

  startOrb     = 1000
  stopOrb      = 51315
  ;; Last file is /media/spencerh/data/FAST_electrons/electron_moments_and_spectral_identification__Orbit_51315.sav, tror jeg

  ;; todayStr = GET_TODAY_STRING(/DO_YYYYMMDD_FMT)
  todayStr = '20181127'

  outPref = STRING(FORMAT='("eMomDB_",A0,"-",I0,"-",I0)', $
                   todayStr, $
                   startOrb, $
                   stopOrb)

  ephemdbPref = 'ephem'
  ephemDBStr = outPref + '-' + ephemdbPref + '.sav'

  RESTORE,outDir+ephemDBStr

  minMLT = 6
  maxMLT = 18
  minAlt = 3800
  maxAlt = 5000
  minAbsILAT = 60
  maxAbsILAT = 87

  maxOrb = 9936

  ;; goodInds = WHERE((ephem.mlt GE minMLT) AND $
  ;;                  (ephem.mlt LE maxMLT) AND $
  ;;                  (ephem.alt GE minAlt) AND $
  ;;                  (ephem.alt LE maxAlt) AND $
  ;;                  (abs(ephem.ilat) GE minAbsILAT) AND $
  ;;                  (abs(ephem.ilat) LE maxAbsILAT) AND $
  ;;                  (ephem.orbit LE maxOrb))
  
  ;; tot = N_ELEMENTS(WHERE(ephem.orbit LE maxOrb))

  ;; goodInds = WHERE(ephem.orbit EQ 8276)

  ;; angler = ANGLE_BETWEEN_NOON_MID_MERIDIAN_AND_TRAJ(ephem.mlt[goodInds],ephem.ilat[goodInds])

  ;; FOR i=0,N_ELEMENTS(goodInds)-1 DO BEGIN
  ;;    ind = goodInds[i] & PRINT,i,ind,ephem.mlt[ind],ephem.ilat[ind],angler[i]
  ;; ENDFOR

  ;; MASSA


  ;; FOR orb=8260,8292 DO BEGIN
  FOR orb=1421,1503 DO BEGIN

     goodInds = WHERE((ephem.mlt GE minMLT) AND $
                      (ephem.mlt LE maxMLT) AND $
                      (ephem.alt GE minAlt) AND $
                      (ephem.alt LE maxAlt) AND $
                      (abs(ephem.ilat) GE minAbsILAT) AND $
                      (abs(ephem.ilat) LE maxAbsILAT) AND $
                      (ephem.orbit EQ orb))


;     angler = ANGLE_BETWEEN_NOON_MID_MERIDIAN_AND_TRAJ(ephem.mlt[goodInds],ephem.ilat[goodInds])

     ;; FOR i=0,N_ELEMENTS(goodInds)-1 DO BEGIN
     ;;    ind = goodInds[i]
     ;;    PRINT,orb,i,ind,ephem.mlt[ind],ephem.ilat[ind],angler[i]
     ;; ENDFOR

     dotProd = ANGLE_BETWEEN_NOON_MID_MERIDIAN_AND_TRAJ(ephem.mlt[goodInds],ephem.ilat[goodInds])

     PRINT,orb,N_ELEMENTS(goodInds), $
           median(ephem.mlt[goodInds]), $
           median(ephem.ilat[goodInds]), $
           acos(abs(median(dotProd)))*!RADEG

  ENDFOR

  STOP

END


FUNCTION ANGLE_BETWEEN_NOON_MID_MERIDIAN_AND_TRAJ,mlt,ilat


  COMPILE_OPT IDL2,STRICTARRSUBS

  unwrappedMLT = UNWRAP(mlt*15.*!DTOR)
  dmlt = (unwrappedMLT[1:-1]-unwrappedMLT[0:-2])
  dilat = ilat[1:-1]-ilat[0:-2]

  dmlt = [dmlt[0],dmlt]
  dilat = [dilat[0],dilat]


  mltilatVecMag = SQRT(dmlt^(2.D)+dilat^(2.D))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Now tangent vector

  theta = ((90.D)-ilat)*!DTOR
  ;; theta = !DPI/2.-ilat/180.*!DPI

  phi = (unwrappedMLT)*!DTOR

  tVecMag = SQRT((COS(theta))^2.D + (SIN(theta)*COS(phi))^2.D)


  TcompTheta = -COS(phi)        ;Theta component of tangent vector
  TcompPhi = COS(theta)*SIN(phi) ;Phi component of tangent vector
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Now dot prod

  ;; angle = ACOS(ABS((dmlt*TcompPhi+dilat*TcompTheta)/(tVecMag*mltilatVecMag)))*!RADEG
  ;; RETURN, angle

  dotProd = (dmlt*TcompPhi+dilat*TcompTheta)/(tVecMag*mltilatVecMag)

  RETURN, dotProd

END