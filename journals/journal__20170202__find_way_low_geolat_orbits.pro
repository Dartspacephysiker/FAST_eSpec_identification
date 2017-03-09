;;02/02/17
PRO JOURNAL__20170202__FIND_WAY_LOW_GEOLAT_ORBITS

  COMPILE_OPT IDL2,STRICTARRSUBS

  dir    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  geofil = 'eSpec_DB_20160607-GEO.sav'
  magfil = 'eSpec_DB_20160607-MAG.sav'

  saveMagFil = 'eSpec_DB_20160607-MAG_lat_GT_35_inds.sav'

  RESTORE,dir+geofil
  RESTORE,dir+magfil

  !P.MULTI = [0,2,2,0,0]

  WINDOW,0,XSIZE=1000,YSIZE=800
  CGHISTOPLOT,GEO.lon,TITLE='GEO longitude'
  CGHISTOPLOT,GEO.lat,TITLE='GEO latitude'

  CGHISTOPLOT,MAG.lon,TITLE='MAG longitude'
  CGHISTOPLOT,MAG.lat,TITLE='MAG latitude'

  MAG_lat_GT_35_i = WHERE(ABS(MAG.lat) GT 35)

  SAVE,MAG_lat_GT_35_i,FILENAME=dir+saveMagFil

  STOP

END
