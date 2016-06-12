;;06/11/16
PRO LOAD_NEWELL_ORB_ESPEC_FILE,orbit,requested_interval, $
                               INTERVAL=interval, $
                               ESPECS_PARSED=eSpecs_parsed, $
                               ESPEC_LC=eSpec_lc, $
                               JEE_LC=jee_lc, $
                               JE_LC=je_lc, $
                               ESPEC_SC_MIN_ENERGY_IND=eSpec_sc_min_energy_ind, $ 
                               ESPEC_SC_POT=eSpec_sc_pot, $
                               ESPEC_SC_TIME=eSpec_sc_time, $
                               ISPEC_UP=iSpec_up, $
                               JEI_UP=jei_up, $
                               JI_UP=ji_up, $
                               ION_SC_MIN_ENERGY_IND=ion_sc_min_energy_ind, $
                               ION_SC_POT=ion_sc_pot, $
                               ION_SC_TIME=ion_sc_time
                               
  COMPILE_OPT IDL2


  IF ~KEYWORD_SET(orbit) THEN BEGIN
     PRINT,"Orbit number not provided; setting to default (500) ..."
     orbit                 = 500
  ENDIF
  orbStr                   = STRCOMPRESS(orbit,/REMOVE_ALL)

  n_intvls                 = GET_ORBIT_N_INTERVALS(orbit) 
  intervalStr              = STRCOMPRESS((KEYWORD_SET(requested_interval) ? requested_interval : 0 ) < (n_intvls-1),/REMOVE_ALL)

  inDir                    = '/SPENCEdata/software/sdt/batch_jobs/Alfven_study/20160520--get_Newell_identification_for_Alfven_events--NOT_despun/Newell_batch_output/'
  inFile_pref              = 'Newell_et_al_identification_of_electron_spectra--ions_included--Orbit_'
  inFile                   = inFile_pref + orbStr + '_' + intervalStr + '.sav'

  PRINT,'Restoring ' + inFile + ' ...'
  RESTORE,inDir+inFile


  espec_lc                 = tmpespec_lc
  jee_lc                   = tmpjee_lc
  je_lc                    = tmpje_lc
  espec_sc_min_energy_ind  = out_sc_min_energy_ind 
  espec_sc_pot             = out_sc_pot
  espec_sc_time            = out_sc_time
  ispec_up                 = ispec_up
  jei_up                   = jei_up
  ji_up                    = ji_up
  ion_sc_min_energy_ind    = out_sc_min_energy_ind_i
  ion_sc_pot               = out_sc_pot_i
  ion_sc_time              = out_sc_time_i
  

END
