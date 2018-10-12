;2018/10/09
PRO JOURNAL__20181010__THE_KALLE_ALLMLT_DB__GET_MAG_COORDS

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; file = '/SPENCEdata/Research/Satellites/FAST/espec_identification/saves_output_etc/20181009-Kalle_inds_21-24MLT__BOTH__gigante_DB.sav'

  file ='/SPENCEdata/Research/Satellites/FAST/espec_identification/saves_output_etc/20181010-Kalle_inds_ALLMLT__BOTH__gigante_DB.sav'

  allFile='/SPENCEdata/Research/Satellites/FAST/espec_identification/saves_output_etc/20181010-Kalle_inds_ALLMLT__BOTH__gigante_DB__delta_t_and_mag_coords.sav'

  MAGdir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  MAGfile = 'eSpecDB_20170203_v0_0--gigante--with_alternate_coords--with_mapping_factors-MAG.sav'

  ;; redMagFile = file.Replace('.sav','__MAG_coords.sav')

  RESTORE,file

  RESTORE,MAGdir+MAGfile

  mag = {alt: mag.alt[good_i], $
         lon: mag.lon[good_i], $
         lat: mag.lat[good_i]}

  ;; SAVE,mag,FILENAME=redMagFile
  SAVE,espec,good_i,espec_delta_t,mag,FILENAME=allFile

  STOP

END
