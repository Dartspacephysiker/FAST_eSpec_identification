;2018/10/09
PRO JOURNAL__20181012__THE_KALLE_ALLMLT_DB__GET_GEO_COORDS

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; file ='/SPENCEdata/Research/Satellites/FAST/espec_identification/saves_output_etc/20181010-Kalle_inds_ALLMLT__BOTH__gigante_DB.sav'

  allFile='/SPENCEdata/Research/Satellites/FAST/espec_identification/saves_output_etc/20181010-Kalle_inds_ALLMLT__BOTH__gigante_DB__delta_t_and_mag_coords.sav'

  coordDir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  GEOfile  = 'eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors-GEO.sav'

  outDir = '/SPENCEdata/Research/Satellites/FAST/espec_identification/saves_output_etc/'
  redGEOfile = '20181010-Kalle_inds_ALLMLT__BOTH__gigante_DB__GEO_coords.sav'


  ;; redMagFile = file.Replace('.sav','__MAG_coords.sav')

  RESTORE,allFile

  RESTORE,coordDir+GEOfile

  geo = {alt: geo.alt[good_i], $
         lon: geo.lon[good_i], $
         lat: geo.lat[good_i]}

  SAVE,geo,FILENAME=outDir+redGEOfile

  STOP

END
