;;06/08/16
PRO SPLIT_ESPECDB_I_BY_ESPEC_TYPE,good_i, $ ;is_despun, $
                                PURE_B_I=pure_b_i, $
                                PURE_D_I=pure_d_i, $
                                PURE_M_I=pure_m_i, $
                                ;; MIX_BD_I=mix_bd_i, $
                                ;; MIX_BM_I=mix_bm_i, $
                                ;; MIX_DM_I=mix_dm_i, $
                                ;; MIX_BDM_I=mix_bdm_i, $
                                ANOMAL_I=anomal_i, $
                                SUMMARY=summary, $
                                OUT_TITLES=out_titles, $
                                OUT_DATANAMESUFFS=out_datanamesuffs, $
                                OUT_I_LIST=out_i_list, $
                                ;; DESPUN_ALF_DB=despun_alf_db, $
                                SUM_LUN=sum_lun

  COMPILE_OPT IDL2

  LOAD_NEWELL_ESPEC_DB,eSpec

  ;; LOAD_ALF_NEWELL_ESPEC_DB,eSpec,alf_i__good_eSpec, $
  ;;                          NEWELLDBFILE=NewellDBFile, $
  ;;                          NEWELLDBDIR=NewellDBDir, $
  ;;                          DESPUN_ALF_DB=is_despun

  ;; outFile       = STRMID(NewellDBFile,0,STRPOS(newelldbfile,'TOTAL')) + $
  ;;                    ;; 'all_good_alf_eSpec_i--CATEGORIZED--' + $
  ;;                     'all_good_alf_eSpec_i--killed_befs_afts--CATEGORIZED--' + $
  ;;                    STRMID(NewellDBFile,STRPOS(NewellDBFile,'Orbs_'),STRPOS(NewellDBFile,'2016')-STRPOS(NewellDBFile,'Orbs_')) + $
  ;;                    ;; GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '.sav'
  ;;                 '20160606' + '.sav'


  ;; RESTORE,NewellDBDir+outFile

  pure_b_i      = WHERE(eSpec.broad EQ 1 OR eSpec.broad EQ 2)
  pure_d_i      = WHERE(eSpec.diffuse EQ 1 OR eSpec.diffuse EQ 2)
  pure_m_i      = WHERE(eSpec.mono EQ 1 OR eSpec.mono EQ 2)
  ;; mix_bd_i      = WHERE(mix_bd)
  ;; mix_bm_i      = WHERE(mix_bm)
  ;; mix_dm_i      = WHERE(mix_dm)
  ;; mix_bdm_i     = WHERE(mix_bdm)
  anomal_i      = CGSETDIFFERENCE(good_i,[pure_b_i,pure_d_i,pure_m_i],NORESULT=-1)

  IF pure_b_i[0] NE -1 THEN BEGIN
     pure_b_i   = CGSETINTERSECTION(good_i,pure_b_i,COUNT=nB)
  ENDIF

  IF pure_d_i[0] NE -1 THEN BEGIN
     pure_d_i   = CGSETINTERSECTION(good_i,pure_d_i,COUNT=nD)
  ENDIF

  IF pure_m_i[0] NE -1 THEN BEGIN
     pure_m_i   = CGSETINTERSECTION(good_i,pure_m_i,COUNT=nM)
  ENDIF

  ;; IF mix_bd_i[0] NE -1 THEN BEGIN
  ;;    mix_bd_i   = CGSETINTERSECTION(good_i,mix_bd_i,COUNT=nBD)
  ;; ENDIF

  ;; IF mix_bm_i[0] NE -1 THEN BEGIN
  ;;    mix_bm_i   = CGSETINTERSECTION(good_i,mix_bm_i,COUNT=nBM)
  ;; ENDIF

  ;; IF mix_dm_i[0] NE -1 THEN BEGIN
  ;;    mix_dm_i   = CGSETINTERSECTION(good_i,mix_dm_i,COUNT=nDM)
  ;; ENDIF

  ;; IF mix_bdm_i[0] NE -1 THEN BEGIN
  ;;    mix_bdm_i  = CGSETINTERSECTION(good_i,mix_bdm_i,COUNT=nBDM)
  ;; ENDIF

  IF anomal_i[0] NE -1 THEN BEGIN
     ;; anomal_i   = CGSETINTERSECTION(good_i,anomal_i,COUNT=nAnom)
     anomal_i   = CGSETINTERSECTION(good_i,anomal_i,COUNT=nAnom)
  ENDIF
  IF N_ELEMENTS(nAnom) EQ 0 THEN nAnom = 0

  ;; sum           = nB+nD+nM+nBD+nBM+nDM+nBDM+nAnom
  sum           = nB+nD+nM+nAnom

  IF KEYWORD_SET(summary) THEN BEGIN
     IF N_ELEMENTS(sum_lun) EQ 1 THEN tmpLun = sum_lun ELSE tmpLun = -1
     PRINTF,tmpLun,""
     PRINTF,tmpLun,FORMAT='(A0,T10,": ",I0)',"pure_b_i",nB
     PRINTF,tmpLun,FORMAT='(A0,T10,": ",I0)',"pure_d_i",nD
     PRINTF,tmpLun,FORMAT='(A0,T10,": ",I0)',"pure_m_i",nM
     ;; PRINTF,tmpLun,FORMAT='(A0,T10,": ",I0)',"mix_bd_i",nBD
     ;; PRINTF,tmpLun,FORMAT='(A0,T10,": ",I0)',"mix_bm_i",nBM
     ;; PRINTF,tmpLun,FORMAT='(A0,T10,": ",I0)',"mix_dm_i",nDM
     ;; PRINTF,tmpLun,FORMAT='(A0,T10,": ",I0)',"mix_bdm_i",nBDM
     PRINTF,tmpLun,FORMAT='(A0,T10,": ",I0)',"anomal_i",nAnom
     PRINTF,tmpLun,""
     PRINTF,tmpLun,FORMAT='(A0,T10,": ",I0)',"nIncoming",N_ELEMENTS(good_i)
     PRINTF,tmpLun,FORMAT='(A0,T10,": ",I0)',"SUM",sum
     PRINTF,tmpLun,""
  ENDIF

  out_titles        = " (" + ['Broadband','Diffuse','Monoenergetic'] + ")" ;,"Broad/Diff",'Broad/Mono','Diff/Mono','BDM','Anomalous'] + ")"
  out_datanamesuffs = "_" + ['broad','diff','mono'] ;,'BD','BM','DM','BDM','Anom']
  out_i_list        = LIST(pure_b_i,pure_d_i,pure_m_i) ;,mix_bd_i,mix_bm_i,mix_dm_i,mix_bdm_i,anomal_i)

END
