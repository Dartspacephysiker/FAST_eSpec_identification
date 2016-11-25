;;11/25/16
PRO GET_ESPEC_INDS_BY_TYPE,eSpec,m_i,b_i,d_i, $
                           LISTPLEASE=listPlease, $
                           N_ARR=NArr, $
                           OUT_I_LIST=i_list, $
                           ONLY_STRICT=only_strict, $
                           ONLY_NONSTRICT=only_nonStrict, $
                           USER_INDS=user_inds, $
                           NO_INDS_AVAILABLE=skip_me, $
                           QUIET=quiet

  COMPILE_OPT IDL2

  opener = 'GET_ESPEC_INDS_BY_TYPE: '
  CASE 1 OF
     KEYWORD_SET(only_strict): BEGIN
        IF ~KEYWORD_SET(quiet) THEN BEGIN
           PRINT,opener + "ONLY STRICT"
        ENDIF

        m_i = WHERE(eSpec.mono    EQ 2,N_m)
        b_i = WHERE(eSpec.broad   EQ 2,N_b)
        d_i = WHERE(eSpec.diffuse EQ 1,N_d)
        IF ~KEYWORD_SET(quiet) THEN BEGIN
           PRINT,"To be clear, diffuse can't be 'only strict' but I'm passing them over anyway"
        ENDIF
     END
     KEYWORD_SET(only_nonStrict): BEGIN
        IF ~KEYWORD_SET(quiet) THEN BEGIN
           PRINT,opener + "ONLY NONSTRICT"
        ENDIF
        m_i = WHERE(eSpec.mono    EQ 1,N_m)
        b_i = WHERE(eSpec.broad   EQ 1,N_b)
        d_i = WHERE(eSpec.diffuse EQ 1,N_d)
     END
     ELSE: BEGIN
        IF ~KEYWORD_SET(quiet) THEN BEGIN
           PRINT,opener + "DO IT"
        ENDIF
        m_i = WHERE(eSpec.mono    EQ 1 OR eSpec.mono  EQ 2,N_m)
        b_i = WHERE(eSpec.broad   EQ 1 OR eSpec.broad EQ 2,N_b)
        d_i = WHERE(eSpec.diffuse EQ 1,N_d)
     END
  ENDCASE
  skip_me = [0,0,0]

  IF KEYWORD_SET(user_inds) THEN BEGIN
     IF ~KEYWORD_SET(quiet) THEN BEGIN
        PRINT,opener + 'Further restricting with user-provided inds ...'
     ENDIF
     
     m_i = CGSETINTERSECTION(m_i,user_inds,COUNT=mCount,NORESULT=-1)
     b_i = CGSETINTERSECTION(b_i,user_inds,COUNT=bCount,NORESULT=-1)
     d_i = CGSETINTERSECTION(d_i,user_inds,COUNT=dCount,NORESULT=-1)

  ENDIF

  IF ~KEYWORD_SET(quiet) THEN BEGIN
     PRINT,opener+'N M: ' + STRCOMPRESS(mCount,/REMOVE_ALL)
     PRINT,opener+'N B: ' + STRCOMPRESS(bCount,/REMOVE_ALL)
     PRINT,opener+'N D: ' + STRCOMPRESS(dCount,/REMOVE_ALL)
  ENDIF
  
  IF m_i[0] EQ -1 THEN skip_me[0] = 1
  IF b_i[0] EQ -1 THEN skip_me[1] = 1
  IF d_i[0] EQ -1 THEN skip_me[2] = 1

  IF KEYWORD_SET(listPlease) THEN BEGIN
     i_list = LIST(TEMPORARY(m_i),TEMPORARY(b_i),TEMPORARY(d_i))
  ENDIF

  NArr   = [N_m,N_b,N_d]

END
