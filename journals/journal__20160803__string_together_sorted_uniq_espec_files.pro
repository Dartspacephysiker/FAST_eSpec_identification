PRO JOURNAL__20160803__STRING_TOGETHER_SORTED_UNIQ_ESPEC_FILES

  COMPILE_OPT idl2

  eSpecDir          = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'

  inFiles           = ['eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info.sav', $
                       'eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info--10to20mil.sav', $
                       'eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info--restavdebeste.sav']

  finalFile         = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--bonus_ephemeris_info.sav'
  nFiles            = N_ELEMENTS(inFiles)

  FOR k=0,nFiles-1 DO BEGIN

     RESTORE,eSpecDir+inFiles[k]

     CASE k OF
        0: BEGIN
           eSFinal  = eSpecEphem
        END
        ELSE: BEGIN
           eSFinal  = { $
                      orbit: [eSFinal.orbit,eSpecEphem.orbit], $            
                      fa_pos: [eSFinal.fa_pos,eSpecEphem.fa_pos], $
                      alt: [eSFinal.alt,eSpecEphem.alt], $   
                      lat: [eSFinal.lat,eSpecEphem.lat], $   
                      lng: [eSFinal.lng,eSpecEphem.lng], $   
                      fa_vel: [eSFinal.fa_vel,eSpecEphem.fa_vel], $
                      pos_and_vel_coords: eSpecEphem.pos_and_vel_coords}
           

        END
     ENDCASE

     PRINT,"... And again!"
  ENDFOR

  eSpecEphem        = eSFinal

  PRINT,"Wrapping it up, saving to " + finalFile + ' ...'
  SAVE,eSpecEphem,FILENAME=eSpecDir+finalFile

END