;2016/06/07
PRO GET_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_ION_INDICES, $
   NONSTORM_I=ns_i, $
   MAINPHASE_I=mp_i, $
   RECOVERYPHASE_I=rp_i, $
   DSTCUTOFF=dstCutoff, $
   SMOOTH_DST=smooth_dst, $
   STORM_DST_I=s_dst_i, $
   NONSTORM_DST_I=ns_dst_i, $
   MAINPHASE_DST_I=mp_dst_i, $
   RECOVERYPHASE_DST_I=rp_dst_i, $
   N_STORM=n_s, $
   N_NONSTORM=n_ns, $
   N_MAINPHASE=n_mp, $
   N_RECOVERYPHASE=n_rp, $
   NONSTORM_T1=ns_t1,MAINPHASE_T1=mp_t1,RECOVERYPHASE_T1=rp_t1, $
   NONSTORM_T2=ns_t2,MAINPHASE_T2=mp_t2,RECOVERYPHASE_T2=rp_t2, $
   PRODUCE_LOGFILE=produce_logFile, $
   GIVE_TIMESPLIT_INFO=give_timeSplit_info, $
   USE_ALFDB_STARTSTOP=use_alfDB_startstop, $
   LUN=lun

  COMPILE_OPT idl2

  IF N_ELEMENTS(lun) EQ 0 THEN lun = -1 ;stdout

  indFileSuff     = ''

  IF KEYWORD_SET(use_alfDB_startstop) THEN BEGIN
     PRINT,'Using AlfDB startstop...'
     earliest_UTC = STR_TO_TIME('1996-10-06/16:00:00')
     latest_UTC   = STR_TO_TIME('2000-10-07/00:00:00')
     indFileSuff  = '--DartDB_startstop_times'
  ENDIF

  LOAD_DST_AE_DBS,dst,ae,LUN=lun
  SET_TXTOUTPUT_DIR,txtOutputDir,/FOR_ESPEC_DB,/ADD_TODAY
  LOAD_NEWELL_ION_DB,ion

  IF KEYWORD_SET(produce_logFile) THEN BEGIN
     logFile   = GET_TODAY_STRING(/DO_YYYYMMDD_FMT) + '--' + 'ion_DB' + indFileSuff + '--nonstorm_mainphase_and_recoveryphase_inds.log'
     OPENW,logLun,txtOutputDir+logFile,/GET_LUN
  ENDIF

  GET_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_PERIODS,dst, $
     DSTCUTOFF=dstCutoff, $
     SMOOTH_DST=smooth_dst, $
     STORM_DST_I=s_dst_i, $
     NONSTORM_DST_I=ns_dst_i, $
     MAINPHASE_DST_I=mp_dst_i, $
     RECOVERYPHASE_DST_I=rp_dst_i, $
     N_STORM=n_s, $
     N_NONSTORM=n_ns, $
     N_MAINPHASE=n_mp, $
     N_RECOVERYPHASE=n_rp, $
     EARLIEST_UTC=earliest_UTC, $
     LATEST_UTC=latest_UTC, $
     LUN=lun

  dst_i_list=LIST(ns_dst_i,mp_dst_i,rp_dst_i)
  strings=["nonstorm","mainphase","recoveryphase"]

  todaysFile = TODAYS_NONSTORM_MAINPHASE_AND_RECOVERYPHASE_ION_INDICES(SUFFIX=indFileSuff, $
                                                                       DSTCUTOFF=dstCutoff, $
                                                                       SMOOTH_DST=smooth_dst)

  IF FILE_TEST(todaysFile) THEN BEGIN
     PRINTF,lun,"Already have nonstorm and storm ion inds! Restoring today's file..."
     RESTORE,todaysFile
  ENDIF ELSE BEGIN
     
     PRINT,'Making todaysFile: ' + todaysFile

     mostRecentFile        = '/SPENCEdata/Research/database/temps/mostRecent_eSpec_storms_inds.txt'

     IF KEYWORD_SET(give_timesplit_info) THEN BEGIN
        TIC
     ENDIF

     FOR i=0,2 DO BEGIN

        IF KEYWORD_SET(give_timesplit_info) THEN BEGIN
           clock     = TIC(strings[i]+'_clock')
        ENDIF

        inds=dst_i_list[i]
        help,inds
        GET_STREAKS,inds,START_I=start_dst_ii,STOP_I=stop_dst_ii,SINGLE_I=single_dst_ii
        
        ;; OPENW,this,'/SPENCEdata/Research/Satellites/FAST/storms_Alfvens/get_and_set_routines/startstop_'+strings[i],/GET_LUN
        ;; FOR j=0,N_ELEMENTS(start_dst_ii)-1 DO BEGIN
        ;;    printf,this,FORMAT='(I10,T15,I10)',inds[start_dst_ii[j]],inds[stop_dst_ii[j]]
        ;; ENDFOR
        ;; CLOSE,this
        
        GET_DATA_AVAILABILITY_FOR_ARRAY_OF_UTC_RANGES, $
           T1_ARR=dst.time[inds[start_dst_ii]], $
           T2_ARR=dst.time[inds[stop_dst_ii]], $
           DBSTRUCT=ion, $
           DBTIMES=dbTimes, $
           /FOR_ESPEC_DB, $
           /DO_NOT_MAKE_ORB_INFO, $
           RESTRICT_W_THESEINDS=restrict, $
           OUT_INDS_LIST=inds_list, $
           UNIQ_ORBS_LIST=uniq_orbs_list,UNIQ_ORB_INDS_LIST=uniq_orb_inds_list, $
           INDS_ORBS_LIST=inds_orbs_list,TRANGES_ORBS_LIST=tranges_orbs_list,TSPANS_ORBS_LIST=tspans_orbs_list, $
           PRINT_DATA_AVAILABILITY=0, $
           GIVE_TIMESPLIT_INFO=give_timeSplit_info, $
           VERBOSE=KEYWORD_SET(produce_logFile), $
           /LIST_TO_ARR, $
           LUN=logLun
        
        IF i EQ 0 THEN BEGIN
           ns_i                = inds_list
           ns_t1               = dst.time[inds[start_dst_ii]]
           ns_t2               = dst.time[inds[stop_dst_ii]]
        ENDIF ELSE BEGIN
           IF i EQ 1 THEN BEGIN
              mp_i             = inds_list 
              mp_t1            = dst.time[inds[start_dst_ii]]
              mp_t2            = dst.time[inds[stop_dst_ii]]
           ENDIF ELSE BEGIN
              IF i EQ 2 THEN BEGIN
                 rp_i          = inds_list
                 rp_t1         = dst.time[inds[start_dst_ii]]
                 rp_t2         = dst.time[inds[stop_dst_ii]]
              ENDIF
           ENDELSE
        ENDELSE
        
        IF KEYWORD_SET(give_timesplit_info) THEN BEGIN
           TOC,clock
        ENDIF

     ENDFOR

     PRINTF,lun,"Saving FAST ion nonstorm/storm indices for today..."
     save,ns_i,mp_i,rp_i,s_dst_i,ns_dst_i,mp_dst_i,rp_dst_i, $
          n_s,n_ns,n_mp,n_rp, $
          ns_t1,ns_t2,mp_t1,mp_t2,rp_t1,rp_t2, $
          FILENAME=todaysFile

     PRINTF,lun,"Updating mostRecent file..."
     SPAWN,'echo "' + todaysFile + '" > ' + mostRecentFile

  ENDELSE

  IF KEYWORD_SET(produce_logFile) THEN BEGIN
     CLOSE,logLun
     FREE_LUN,logLun
  ENDIF

END