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
                                  COMBINE_ACCELERATED=comb_accelerated, $
                                  ;; DESPUN_ALF_DB=despun_alf_db, $
                                  SUM_LUN=sum_lun

  COMPILE_OPT IDL2

  @common__newell_espec.pro
  IF N_ELEMENTS(NEWELL__eSpec) EQ 0 THEN BEGIN
     LOAD_NEWELL_ESPEC_DB
     PRINT,"How??"
     STOP
  ENDIF

  pure_b_i      = WHERE(NEWELL__eSpec.broad EQ 1 OR NEWELL__eSpec.broad EQ 2)
  pure_d_i      = WHERE(NEWELL__eSpec.diffuse EQ 1 OR NEWELL__eSpec.diffuse EQ 2)
  pure_m_i      = WHERE(NEWELL__eSpec.mono EQ 1 OR NEWELL__eSpec.mono EQ 2)
  ;; mix_bd_i      = WHERE(mix_bd)
  ;; mix_bm_i      = WHERE(mix_bm)
  ;; mix_dm_i      = WHERE(mix_dm)
  ;; mix_bdm_i     = WHERE(mix_bdm)
  anomal_i      = CGSETDIFFERENCE(good_i,[pure_b_i,pure_d_i,pure_m_i],NORESULT=-1)

  IF pure_b_i[0] NE -1 THEN BEGIN
     pure_b_i   = CGSETINTERSECTION(good_i,pure_b_i,COUNT=nB,NORESULT=-1)
  ENDIF

  IF pure_d_i[0] NE -1 THEN BEGIN
     pure_d_i   = CGSETINTERSECTION(good_i,pure_d_i,COUNT=nD,NORESULT=-1)
  ENDIF

  IF pure_m_i[0] NE -1 THEN BEGIN
     pure_m_i   = CGSETINTERSECTION(good_i,pure_m_i,COUNT=nM,NORESULT=-1)
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
     anomal_i   = CGSETINTERSECTION(good_i,anomal_i,COUNT=nAnom,NORESULT=-1)
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
  out_i_list        = LIST(nB GT 0 ? pure_b_i : !NULL, $
                           nD GT 0 ? pure_d_i : !NULL, $
                           nM GT 0 ? pure_m_i : !NULL) ;,mix_bd_i,mix_bm_i,mix_dm_i,mix_bdm_i,anomal_i)

  IF KEYWORD_SET(comb_accelerated) THEN BEGIN
     out_titles = [out_titles,'(accel.)']
     out_datanamesuffs = [out_datanamesuffs,"_accel"]
     CASE 1 OF
        (nB GT 0) AND (nM GT 0): BEGIN
           out_i_list.ADD,CGSETUNION(pure_b_i,pure_m_i)
        END
        (nB GT 0): BEGIN
           out_i_list.ADD,pure_b_i
        END
        (nM GT 0): BEGIN
           out_i_list.ADD,pure_M_i
        END
        ELSE: BEGIN
           out_i_list.ADD,!NULL
        END
     ENDCASE
     
  ENDIF
  
END
