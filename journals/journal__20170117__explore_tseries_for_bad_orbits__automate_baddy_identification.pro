;;01/17/17
PRO JOURNAL__20170117__EXPLORE_TSERIES_FOR_BAD_ORBITS__AUTOMATE_BADDY_IDENTIFICATION

  COMPILE_OPT IDL2


  PROGRAM_NOMMER = 2


  ;; LOAD_NEWELL_ESPEC_DB,eSpec,/NO_MEMORY_LOAD
  @common__newell_espec.pro
  LOAD_NEWELL_ESPEC_DB

  IF PROGRAM_NOMMER EQ 1 THEN BEGIN

     ;;Some baddies from GET_ORBRANGE_INDS
     worstOrbRange = [1028,1056]
     badOrbs       = [11991,11997] ;Could go back to 11985, if you really wanted

     ;; inds          = WHERE(NEWELL__eSpec.orbit GE badOrbs[0] AND NEWELL__eSpec.orbit LE badOrbs[1])


     NewellSpecTN  = 'spectral_types'
     tPlt_vars     = ['Je','Jee','charepanel',NewellSpecTN]

     symSize       = 2.0

     ;;Plot setup
     YLIM,'Je',1e8,1e12,1
     YLIM,'Jee',1e-1,4e4,1
     YLIM,'charepanel',1e0,1.5e4,1
     OPTIONS,'Je','PSYM',3
     OPTIONS,'Jee','PSYM',3
     OPTIONS,'charepanel','PSYM',3
     OPTIONS,'Je','SYMSIZE',symSize
     OPTIONS,'Jee','SYMSIZE',symSize
     OPTIONS,'charepanel','SYMSIZE',symSize
     OPTIONS,'charepanel','ytitle','E/q Volts'

     gapDist       = 1
     interp_gap    = 0

     WINDOW,0,XSIZE=1000,YSIZE=600

     curOrb        = badOrbs[0]
     WHILE curOrb LT badOrbs[1] DO BEGIN
        
        tmpI       = WHERE(NEWELL__eSpec.orbit EQ curOrb)

        IF tmpI[0] EQ -1 THEN BEGIN
           PRINT,"Whoa!"
           CONTINUE
        ENDIF

        tmpTime    = NEWELL__eSpec.x[tmpI]
        charetmp   = {x:tmpTime,y:NEWELL__eSpec.Jee[tmpI]/NEWELL__eSpec.Je[tmpI]*6.242*1.0e11}
        Jetmp      = {x:tmpTime,y:NEWELL__eSpec.Je[tmpI]}
        Jeetmp     = {x:tmpTime,y:NEWELL__eSpec.Jee[tmpI]}

        Je         = DATA_CUT(Jetmp,tmpTime, $
                              GAP_DIST=gapDist, $
                              INTERP_GAP=interp_gap)
        Jee        = DATA_CUT(Jeetmp,tmpTime, $
                              GAP_DIST=gapDist, $
                              INTERP_GAP=interp_gap)
        chare      = DATA_CUT(charetmp,tmpTime, $
                              GAP_DIST=gapDist, $
                              INTERP_GAP=interp_gap)

        Je         = {x:tmpTime,y:Je}
        Jee        = {x:tmpTime,y:Jee}
        chare      = {x:tmpTime,y:chare}

        STORE_DATA,'Je',DATA=Je
        STORE_DATA,'Jee',DATA=Jee
        STORE_DATA,'charepanel',DATA=chare

        GET_FA_ORBIT,tmpTime,/TIME_ARRAY

        PREPARE_IDENTIFIED_DIFF_EFLUXES_FOR_TPLOT,{x:tmpTime, $
                                                   orbit:NEWELL__eSpec.orbit[tmpI], $
                                                   mono:NEWELL__eSpec.mono[tmpI], $
                                                   broad:NEWELL__eSpec.broad[tmpI], $
                                                   diffuse:NEWELL__eSpec.diffuse[tmpI]}, $
                                                  TPLOT_NAME=NewellSpecTN, $
                                                  NO_STRICT_TYPES=no_strict_types, $
                                                  CONVERT_TO_NEWELL_INTERP=convert_to_Newell_interp

        TPLOT,tPlt_vars,VAR=['ALT','MLT','ILAT'],TITLE='Orbit ' + STRCOMPRESS(curOrb,/REMOVE_ALL)

        curOrb++

        STOP
     ENDWHILE

     STOP

  ENDIF

  IF PROGRAM_NOMMER EQ 2 THEN BEGIN

     wild_i   = WHERE(NEWELL__eSpec.Je GE 1e11 AND NEWELL__eSpec.Jee GE 100)

     uniq_ii  = UNIQ(NEWELL__eSpec.orbit[wild_i])
     uniq_i   = wild_i[uniq_ii]

     uniqOrbs = NEWELL__eSpec.orbit[uniq_i]

     FOR k=0,N_ELEMENTS(uniqOrbs)-1 DO BEGIN
        PRINT,k,uniqOrbs[k]
     ENDFOR
  ENDIF

END
