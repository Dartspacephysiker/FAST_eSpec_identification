;;06/07/16
PRO REMOVE_ALF_EVENTS_FROM_ESPEC_STORM_INDS,HEMI=hemi

  COMPILE_OPT IDL2

  IF ~KEYWORD_SET(hemi) THEN BEGIN
     PRINT,'Hemi not set! Default: North'
     hemi               = 'NORTH'
  ENDIF

  eSpecIndDir           = '/SPENCEdata/Research/database/temps/'
  eSpecIndFile          = 'todays_nonstorm_mainphase_and_recoveryphase_ESPEC_inds--dstCutoff_-20nT--DartDB_startstop_times--20160604.sav'

  alfStormDir           = '/SPENCEdata/Research/Satellites/FAST/storms_Alfvens/saves_output_etc/'

  ;; alfNSFile             = 'May_31_16--nonstorm--Dstcutoff_-20--NORTH--logAvg--maskMin10--good_alf_eSpec_i_and_inds_into_eSpec_quantities.sav'
  ;; alfMPFile             = 'May_31_16--mainphase--Dstcutoff_-20--NORTH--logAvg--maskMin10--good_alf_eSpec_i_and_inds_into_eSpec_quantities.sav'
  ;; alfRPFile             = 'May_31_16--recoveryphase--Dstcutoff_-20--NORTH--logAvg--maskMin10--good_alf_eSpec_i_and_inds_into_eSpec_quantities.sav'

  alfFiles              = MAKE_ARRAY(3,/STRING)
  newAlfFiles           = MAKE_ARRAY(3,/STRING)
  alfFileDate           = 'May_31_16'
  stormphase            = ['nonstorm','mainphase','recoveryphase']
  Dstcutoff             = -20
  maskMin               = 10
  fileSuff              = '--good_alf_eSpec_i_and_inds_into_eSpec_quantities.sav'
  newFileSuff           = '--eSpec_inds--alf_events_removed.sav'


  ;;For det første, vi skaffer indekser fra eSpec stormfil
  PRINT,'Restoring ' + eSpecIndFile + '...'
  RESTORE,eSpecIndDir+eSpecIndFile

  STOP

  ;;Nesten vi må gjøre er å skaffe alle eSpec indekser tilknyttet med Alfvénisk aktivitet

  indList               = LIST(TEMPORARY(ns_i),TEMPORARY(mp_i),TEMPORARY(rp_i))
  LOAD_ALF_NEWELL_ESPEC_DB,espec,good_alf_i,good_eSpec_i,DESPUN_ALF_DB=despun

  FOR i=0,2 DO BEGIN
     alfFiles[i]        = STRING(FORMAT='(A0,"--",A0,"--Dstcutoff_",I0,"--",A0,"--logAvg--maskMin",I0,A0)', $
                                 alfFileDate, $
                                 stormphase[i], $
                                 Dstcutoff, $
                                 hemi, $
                                 maskMin, $
                                 fileSuff)

     newAlfFiles[i]     = STRING(FORMAT='(A0,"--",A0,"--Dstcutoff_",I0,"--",A0,"--logAvg--maskMin",I0,A0)', $
                                 alfFileDate, $
                                 stormphase[i], $
                                 Dstcutoff, $
                                 hemi, $
                                 maskMin, $
                                 newFileSuff)

  ENDFOR

  ;;Nå at vi har filenavnene kan vi åpne dem og drepe alle hendelser some er tilknytet med Alfvénisk aktivitet
  FOR i=0,2 DO BEGIN
     RESTORE,alfStormDir+alfFiles[i]


     PRINT,"Saving non-Alfvén stuff to " + newAlfFiles[i]
     SAVE,FILENAME=alfStormDir+newAlfFiles[i]
  ENDFOR
  STOP


END
