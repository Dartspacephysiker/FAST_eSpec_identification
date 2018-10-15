;2018/10/15
PRO JOURNAL__20181015__NYE_DB__HOW_ABOUT_CLEANING

  COMPILE_OPT IDL2,STRICTARRSUBS

  outDir       = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/v2018/'

  file        = '../v2018/eMomDB_20181015-1000-11776-ephem.sav' 
  momsFile    = '../v2018/eMomDB_20181015-1000-11776-LCangle_moms.sav'
  extraFile   = '../v2018/eMomDB_20181015-1000-11776-extra.sav'

  minOrb      = 1000
  maxOrb      = 11776

  RESTORE,outdir+file
  RESTORE,outDir+extraFile

  good_i = WHERE(ephem.valid,nGood)
  PRINT,FORMAT='(I0,A0)',nGood,' inds are "valid"'

  good_i2 = WHERE(~extra.eSpec_bad_time,nGood2)
  PRINT,FORMAT='(I0,A0)',nGood2,' inds are not "espec_bad_time"'

  good_i = CGSETINTERSECTION(good_i,good_i2,COUNT=nGoodAll)
  
  PRINT,FORMAT='(I0,A0)',nGoodAll,' inds are both kinds of good'

  theseInds = GET_ORBRANGE_INDS(ephem,minOrb,maxOrb,LUN=lun, $
                                DBTIMES=ephem.time, $ 
                                DONT_TRASH_BAD_ORBITS=keepJunk, $
                                KEEP_TRASHINDS_ON_TAP=keep_trashInds_on_tap)

  STOP

  good_i = CGSETINTERSECTION(good_i,theseInds,COUNT=nGoodFinal)

  SAVE,good_i,FILENAME=outDir+file.Replace(".sav","-goodInds.sav")

  ;; Prove it
  dog = N_ELEMENTS(WHERE(extra.espec_bad_time[good_i] EQ 1,nBalogga))
  dog = N_ELEMENTS(WHERE(ephem.valid         [good_i] EQ 0,nBaligga))
END
