;2018/10/09
PRO JOURNAL__20181009__THE_KALLE_DB__NOW_GET_MAG_COORDINATES

  COMPILE_OPT IDL2,STRICTARRSUBS

  file = '/SPENCEdata/Research/Satellites/FAST/espec_identification/20181009-Kalle_inds_21-24MLT__BOTH__gigante_DB.sav'

  MAGdir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  MAGfile = 'eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors-MAG.sav'

  redMagFile = file.Replace('.sav','__MAG_coords.sav')

  RESTORE,file

  RESTORE,MAGdir+MAGfile

  mag = {alt: mag.alt[good_i], $
         lon: mag.lon[good_i], $
         lat: mag.lat[good_i]}

  SAVE,mag,FILENAME=redMagFile

  STOP

END
