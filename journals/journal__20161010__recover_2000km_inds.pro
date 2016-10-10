;;10/10/16
PRO JOURNAL__20161010__RECOVER_2000KM_INDS

  COMPILE_OPT IDL2

  originating_routine = 'JOURNAL__20161010__RECOVER_2000KM_INDS'

  dir        = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  km2000File = 'eSpec_20160607_db--orbs_500-16361--BELOW_2000km--with_alternate_coords.sav'
  fullFile   = 'sorted--eSpec_20160607_db--PARSED--with_mapping_factors--Orbs_500-16361.sav'

  new2000file = 'eSpec_20160607_db--orbs_500-16361--BELOW_2000km--with_alternate_coords__mapping_factors.sav'

  RESTORE,dir+km2000File
  tmpTime = espec.x
  eSpec = !NULL

  RESTORE,dir+fullFile

  inds_2000km = VALUE_CLOSEST2(espec.x,tmpTime)

  mapFactor = eSpec.mapFactor[inds_2000km]

  eSpec = !NULL

  RESTORE,dir+km2000File

  PRINT,'Reform dat'
  eSpec = {x:espec.x, $
           orbit:eSpec.orbit, $
           coords:{sdt:{alt:eSpec.coords.SDT.alt, $
                        mlt:eSpec.coords.SDT.mlt, $
                        ilat:eSpec.coords.SDT.ilat}, $
                   aacgm:{alt:ESpec.Coords.AACGM.alt, $
                          mlt:ESpec.Coords.AACGM.mlt, $
                          lat:ESpec.Coords.AACGM.lat}, $
                   geo:{alt:eSpec.coords.GEO.alt, $
                        lon:eSpec.coords.GEO.lon, $
                        lat:eSpec.coords.GEO.lat}, $
                   mag:{alt:eSpec.coords.MAG.alt, $
                        lon:eSpec.coords.MAG.lon, $
                        lat:eSpec.coords.MAG.lat}}, $
           je:eSpec.je, $
           jee:eSpec.jee, $
           mapFactor:mapFactor, $
           info:{creation_date:GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                 sign_convention:"Positive fluxes are earthward in both hemispheres", $
                 originating_routine:originating_routine}}


  PRINT,'Saving updated 2000km DB'
  SAVE,eSpec,FILENAME=dir+new2000File


END
