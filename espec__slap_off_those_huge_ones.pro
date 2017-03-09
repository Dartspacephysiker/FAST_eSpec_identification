;;2017/02/15
PRO ESPEC__SLAP_OFF_THOSE_HUGE_ONES,DBStruct, $
   FOR_ESPEC_DBS=for_eSpec_DBs, $
   FOR_ION_DBS=for_ion_DBs, $
   GOOD_I=good_i

  COMPILE_OPT IDL2,STRICTARRSUBS

  IF ~(KEYWORD_SET(for_eSpec_DBs) OR KEYWORD_SET(for_ion_DBs)) THEN BEGIN
     IS_STRUCT_ION_OR_ESPEC,DBStruct,for_ion_DBs
     for_eSpec_DBs = ~for_ion_DBs
  ENDIF

  IF N_ELEMENTS(good_i) EQ 0 THEN BEGIN
     good_i = LINDGEN(N_ELEMENTS(DBStruct.mlt))
  ENDIF

  CASE 1 OF
     KEYWORD_SET(for_eSpec_DBs): BEGIN

        ;; PRINT,"eSpec DB needs no cleaning. She's clean as a whistle, you know."
        PRINT,'" … See, in my day we never cleaned eSpec … "'

        ;; je_lims  = [0,3e10]
        ;; jee_lims = [0,3e2]

        ;;The command: GET_INDICES_ABOVE_PERCENT_THRESHOLD(NEWELL__eSpec.je[WHERE(broad)],0.25,OUT_FINAL_VAL=je_upper_lim)
        je_lims  = [0,2.855e10] ;Drop 0.25% of the broadbands 
        jee_lims = [0,1.043e2]  ;Drop 0.25% of the broadbands

        ;;The command: GET_INDICES_ABOVE_PERCENT_THRESHOLD(NEWELL__eSpec.je[WHERE(broad)],1.0,OUT_FINAL_VAL=je_upper_lim)
        je_lims  = [0,3.36e9]   ;Drop 1.0% of all
        jee_lims = [0,3.1172]   ;Drop 1.0% of all

        ;; je_lims  = [0,1e10] ;Drop ... less than 1.0% (by rounding up to nearest decade)
        ;; jee_lims = [0,10.0] ;Drop ... less than 1.0% (by rounding up to nearest decade)

        je_lims  = [0,1e11]     ;Drop ... less than 1.0% (by rounding up to nearest decade)
        jee_lims = [0,100.0]    ;Drop ... less than 1.0% (by rounding up to nearest decade)

        ;; percentToDrop = N_ELEMENTS(WHERE( ( (espec.broad EQ 1) OR (eSpec.broad EQ 2) ) AND $
        ;;                                   ( eSpec.je GT 4e10 ) ) ) / $
        ;;                 FLOAT(N_ELEMENTS(WHERE( ( (espec.broad EQ 1) OR $
        ;;                                           (eSpec.broad EQ 2) )  ) )) $
        ;;                 * 100.

        ;;;START COMMENT TO FORCE MYSELF TO LABORIOUSLY CHECK EVERY ORBIT
        ;;Now Je
        good_je_i  = WHERE(dbStruct.je GE je_lims[0] AND $
                           dbStruct.je LE je_lims[1],nGoodJe, $
                           NCOMPLEMENT=nBadJe)

        nGood      = N_ELEMENTS(good_i)
        good_i     = CGSETINTERSECTION(good_i,TEMPORARY(good_je_i),COUNT=nKept)
        PRINT,"Lost " + STRCOMPRESS(nGood - nKept,/REMOVE_ALL) + ' inds to Je screening ...'

        ;;Now Jee
        good_jee_i = WHERE(dbStruct.jee GE jee_lims[0] AND $
                           dbStruct.jee LE jee_lims[1],nGoodJee, $
                           NCOMPLEMENT=nBadJee)

        nGood      = N_ELEMENTS(good_i)
        good_i     = CGSETINTERSECTION(good_i,TEMPORARY(good_jee_i),COUNT=nKept)
        PRINT,"Lost " + STRCOMPRESS(nGood - nKept,/REMOVE_ALL) + ' inds to Jee screening ...'
        ;;;STOP COMMENT TO FORCE MYSELF TO LABORIOUSLY CHECK EVERY ORBIT


     END
     KEYWORD_SET(for_ion_DBs): BEGIN

        CASE 1 OF
           KEYWORD_SET(DBStruct.info.is_downgoing): BEGIN
              ;;2017/02/15
              ;; ji_lims  = [0,e10] ;Drop ... less than .01% 
              ;; jei_lims = [0,10.0] ;Drop ... .0301% 

              ;;2017/02/15
              ji_lims  = [0,3e9] ;Drop ... .143% 
              jei_lims = [0,1.0] ;Drop ... .166% 
           END
           ELSE: BEGIN
              PRINT,"You decide, partner. Check out JOURNAL__20170215__LOOK_AT_VARIOUSNESSES_ASSOCIATED_WITH_DOWNGOING_ION_DB " + $
                    "if you want to see how we did it with the downgoing ion DB"
              STOP

              ;; ji_lims  = [0,1e10] ;Drop ... less than .01% 
              ;; jei_lims = [0,10.0] ;Drop ... .0301% 
           END
        ENDCASE

        PRINT,'" … Ain''t never heard about cleaning no ion data! … "'

        good_ji_i  = WHERE(dbStruct.ji GE ji_lims[0] AND $
                           dbStruct.ji LE ji_lims[1],nGoodJi, $
                           NCOMPLEMENT=nBadJi)

        nGood      = N_ELEMENTS(good_i)
        good_i     = CGSETINTERSECTION(good_i,TEMPORARY(good_ji_i),COUNT=nKept)
        PRINT,"Lost " + STRCOMPRESS(nGood - nKept,/REMOVE_ALL) + ' inds to Ji screening ...'

        ;;Now Jei
        good_jei_i = WHERE(dbStruct.jei GE jei_lims[0] AND $
                           dbStruct.jei LE jei_lims[1],nGoodJei, $
                           NCOMPLEMENT=nBadJei)

        nGood      = N_ELEMENTS(good_i)
        good_i     = CGSETINTERSECTION(good_i,TEMPORARY(good_jei_i),COUNT=nKept)
        PRINT,"Lost " + STRCOMPRESS(nGood - nKept,/REMOVE_ALL) + ' inds to Jei screening ...'

     END
  ENDCASE


END
