;;2016/08/05 Do it for Ryan McGranaghan
PRO JOURNAL__20160805__CONVERT_ALL_ESPECDB_ILATS_TO_MLATS

  COMPILE_OPT idl2

  orig_routineName = 'JOURNAL__20160805__CONVERT_ALL_FASTLOCDB_ILATS_TO_MLATS'

  outDir           = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'

  inFiles          = ['eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info.sav', $
                      'eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info--10to20mil.sav', $
                      'eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info--restavdebeste.sav']

  inFileIndArr     = [[     0,1000000,2000000], $
                      [999999,1999999,2999999]]

  ;;This guy's too big to use ...
  ;; finalInFile       = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info.sav'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;output
  outFile         = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords' + $
                    ['_1','_2','_3']+'.sav'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Input

  TIC
  clock = TIC('warnMe')
  FOR k=0,N_ELEMENTS(inFiles)-1 DO BEGIN
     LOAD_NEWELL_ESPEC_DB,/JUST_TIMES,OUT_TIMES=eSpecTimes
     inds        = inFileIndArr[k,*]
     eSTTemp     = eSpecTimes[inds[0]:inds[1]]

     eSpecTimes  = !NULL

     ;;Load the stuff we need 
     PRINT,"Restoring " + inFiles[k] + ' ...'
     RESTORE,outDir+inFiles[k]
     
     ;; GEOPACK_CONV_COORD
     ;; Description: Convert between a variety of commonly used coordinate systems.
     ;; Calling Sequence: geopack_conv_coord(_08), s1, s2, s3, d1, d2, d3.
     ;; Inputs: s1, s2, s3: Coordinates in system of origin.
     ;; Outputs: d1, d2, d3: Coordinates in target system.
     ;; Keywords: FROM_GEO: Specify source in geopgraphic coordinates. 
     ;;  FROM_MAG: Specify source in geomagnetic coordinates.
     ;;  FROM_GEI: Specify source in geocentric equatorial inertial coordinates.
     ;;  FROM_SM: Specify source in solar magnetic coordinates.
     ;;  FROM_GSM: Specify source in geocentric solar magnetospheric
     ;;  coordinates.
     ;;  FROM_GSE: Specify source in geocentric solar ecliptic coordinates.
     ;;  TO_GEO: Specify destination in geopgraphic coordinates.
     ;;  TO_MAG: Specify destination in geomagnetic coordinates.
     ;;  TO_GEI: Specify destination in geocentric equatorial inertial coordinates.
     ;;  TO_SM: Specify destination in solar magnetic coordinates.
     ;;  TO_GSM: Specify destination in geocentric solar magnetospheric
     ;;  coordinates. 

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Times in CDF epoch time
     time_epoch           = UTC_TO_CDF_EPOCH(esTTemp)

     YearArr              = FIX(STRMID(fastLoc.time,0,4))
     MonthArr             = FIX(STRMID(fastLoc.time,5,2))
     DayArr               = FIX(STRMID(fastLoc.time,8,2))
     HourArr              = FIX(STRMID(fastLoc.time,11,2))
     MinArr               = FIX(STRMID(fastLoc.time,14,2))
     SecArr               = FLOAT(STRMID(fastLoc.time,17,6))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;feed it to GEOPACK
     nTot                 = N_ELEMENTS(esTTemp)
     eEphem_MAG_arr       = MAKE_ARRAY(3,nTot,/FLOAT)
     eEphem_GEO_arr       = MAKE_ARRAY(3,nTot,/FLOAT)
     eEphem_MAGSph_arr    = MAKE_ARRAY(3,nTot,/FLOAT)
     eEphem_GEOSph_arr    = MAKE_ARRAY(3,nTot,/FLOAT)

     PRINT,"Feeding it to GEOPACK ..."
     FOR i=0,nTot-1 DO BEGIN

        GEOPACK_RECALC,YearArr[i],MonthArr[i],DayArr[i],HourArr[i],MinArr[i],SecArr[i],/DATE

        ;;do that dance
        ;;To MAG
        GEOPACK_CONV_COORD,eSpecEphem.fa_pos[i,0],eSpecEphem.fa_pos[i,1],eSpecEphem.fa_pos[i,2], $
                           faPosmag_x,faPosmag_y,faPosmag_z, $
                           /FROM_GEI,/TO_MAG,EPOCH=time_epoch[i]
        ;;To GEO
        GEOPACK_CONV_COORD,eSpecEphem.fa_pos[i,0],eSpecEphem.fa_pos[i,1],eSpecEphem.fa_pos[i,2], $
                           faPosgeo_x,faPosgeo_y,faPosgeo_z, $
                           /FROM_GEI,/TO_GEO,EPOCH=time_epoch[i]

        ;;update
        eEphem_MAG_arr[*,i] = [faPosmag_x,faPosmag_y,faPosmag_z]
        eEphem_GEO_arr[*,i] = [faPosgeo_x,faPosgeo_y,faPosgeo_z]


        GEOPACK_SPHCAR,faPosgeo_x,faPosgeo_y,faPosgeo_z,geo_r,geo_theta,geo_phi,/TO_SPHERE,/DEGREE
        GEOPACK_SPHCAR,faPosmag_x,faPosmag_y,faPosmag_z,mag_r,mag_theta,mag_phi,/TO_SPHERE,/DEGREE

        ;;Lat, long, height
        eEphem_MAGSph_arr[*,i]    = [mag_theta,mag_phi,mag_r] 
        eEphem_GEOSph_arr[*,i]    = [geo_theta,geo_phi,geo_r] 
        ;;update
        ;; TiltArr    = [TiltArr,tempTilt]
        eEphem_MAG_arr[*,i] = [faPosmag_x,faPosmag_y,faPosmag_z]
        eEphem_GEO_arr[*,i] = [faPosgeo_x,faPosgeo_y,faPosgeo_z]


        eEphem_MAGSph_arr    = [ $
                               [90.-REFORM(eEphem_MAGSph_arr[0,*])], $
                               [REFORM(eEphem_MAGSph_arr[1,*])], $
                               [REFORM(eEphem_MAGSph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
                               ]   

        eEphem_GEOSph_arr    = [ $
                               [90.-REFORM(eEphem_GEOSph_arr[0,*])], $
                               [REFORM(eEphem_GEOSph_arr[1,*])], $
                               [REFORM(eEphem_GEOSph_arr[2,*])-R_E] $ ;Convert to latitude from colatitude here
                               ]

        IF (i MOD 1e4) EQ 0 THEN BEGIN
           PRINT,'i = ' + STRCOMPRESS(i,/REMOVE_ALL)
           TOC,clock
        ENDIF

     ENDFOR


     eSpec_GEO     = {ALT:eEphem_GEOSph_arr[*,2], $
                      LON:eEphem_GEOSph_arr[*,1], $
                      LAT:eEphem_GEOSph_arr[*,0]}

     eSpec_MAG     = {ALT:eEphem_MAGSph_arr[*,2], $
                      LON:eEphem_MAGSph_arr[*,1], $
                      LAT:eEphem_MAGSph_arr[*,0]}


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;make struct
     eSpecCoords = {TIME: esTTemp, $
                    MAG: eEphem_MAG_arr, $
                    GEO: eEphem_GEO_arr, $
                    CREATED: GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                    ORIGINATING_ROUTINE:orig_routineName}

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;Save it
     PRINT,'Saving ' + outDir + outFile[k] + '...'
     save,eSpecCoords,eSpec_GEO,eSpec_MAG,FILENAME=outDir+outFile[k]

     PRINT,"Did it!"

     TOC

  ENDFOR
END

