;;10/10/16
PRO JOURNAL__20161010__PLOTS_SHOWING_DIFF_TWEEN_MAPPED_UNMAPPED_FLUXES

  COMPILE_OPT IDL2

  ;;Just for reference--we don't actually use these here
  ;; ephemFileIndArr  = [[      0,10000000,20000000], $
  ;;                     [9999999,19999999,28604344]]

  ;; LOAD_NEWELL_ESPEC_DB,eSpec, $
  ;;                      /DONT_LOAD_IN_MEMORY, $
  ;;                      /DONT_CONVERT_TO_STRICT_NEWELL

  ;; orbStrings     = REFORM(STRING(FORMAT='(I0)',espec.orbit[ephemfileindarr]),N_ELEMENTS(ephemFileIndArr[*,0]),N_ELEMENTS(ephemFileIndArr[0,*]))
  orbStrings     = ['500','5847','11524']+'-'+['5847','11524','16361']

  ;; eSpec          = !NULL

  ultimateDir    = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  ;; ultimateFiles  = 'eSpec_20160607_db--Orbs_' + orbStrings[*,0] + '-' + orbStrings[*,1] + $
  ultimateFiles  = 'eSpec_20160607_db--Orbs_' + orbStrings + $
                   '--with_alternate_coords__mapFactors--' + $
                   ['1','2','3']+'.sav'

  SET_PLOT_DIR,plotDir,/FOR_ESPEC_DB,/ADD_TODAY

  jeFiles      = plotDir + 'eSpec_20160607_db--Orbs_' + orbStrings + '--Je'
  jeMapFiles   = plotDir + 'eSpec_20160607_db--Orbs_' + orbStrings + '--Je_mapped'
  jeBothFiles  = plotDir + 'eSpec_20160607_db--Orbs_' + orbStrings + '--Je--unmapped_vs_mapped'

  jeeFiles     = plotDir + 'eSpec_20160607_db--Orbs_' + orbStrings + '--Jee'
  jeeMapFiles  = plotDir + 'eSpec_20160607_db--Orbs_' + orbStrings + '--Jee_mapped'
  jeeBothFiles = plotDir + 'eSpec_20160607_db--Orbs_' + orbStrings + '--Jee--unmapped_vs_mapped'

  suff = '.png'

  ;; jeTitle = ['','Number flux (#/cm$^2$-s)',''] + '!C'
  ;; jeeTitle = ['','Energy flux (erg/cm$^2$-s)',''] + '!C'

  jeTitle = ['','Number flux ','']
  jeeTitle = ['','Energy flux ','']

  jeMapTitle = ['','(#/cm!U2!N-s)!CNumber flux, 100 km','']
  jeeMapTitle = ['','(erg/cm!U2!N-s)!CEnergy flux, 100 km','']

  jeBounds = [5.8,11]
  jeeBounds = [-4.1,2]

  jeBinSize = 0.2
  jeeBinSize = 0.2

  absSuff = '--ABS'
  posSuff = '--POS'
  negSuff = '--NEG'

  absTSuff = '(abs)'
  posTSuff = '(pos)'
  negTSuff = '(neg)'

  absTMapSuff = ''
  posTMapSuff = ''
  negTMapSuff = ''

  charSize = 1.4

  jeFluxInd  = 3
  jeeFluxInd = 4
  jeFluxName = 'Je'
  jeeFluxName = 'Jee'

  save_ps = 1

  LOOPIT_MAP,jeFluxInd, $
             ultimateFiles,ultimateDir, $
             jeTitle,jeMapTitle, $
             jeFiles,jeMapFiles, $
             jeBounds,jeBounds, $
             jeBinSize,jeBinSize, $
             absSuff,posSuff,negSuff, $
             absTSuff,posTSuff,negTSuff, $
             absTMapSuff,posTMapSuff,negTMapSuff, $
             suff, $
             CHARSIZE=charSize, $
             CREATE_INDIVIDUAL_OUTPUT=create_individual_output, $
             SAVE_PS=save_ps, $
             PSFILENAMES=jeBothFiles
             

  LOOPIT_MAP,jeeFluxInd, $
             ultimateFiles,ultimateDir, $
             jeeTitle,jeeMapTitle, $
             jeeFiles,jeeMapFiles, $
             jeeBounds,jeeBounds, $
             jeeBinSize,jeeBinSize, $
             absSuff,posSuff,negSuff, $
             absTSuff,posTSuff,negTSuff, $
             absTMapSuff,posTMapSuff,negTMapSuff, $
             suff, $
             CHARSIZE=charSize, $
             CREATE_INDIVIDUAL_OUTPUT=create_individual_output, $
             SAVE_PS=save_ps, $
             PSFILENAMES=jeeBothFiles

  ;; LOOPIT,ultimateFiles,ultimateDir, $
  ;;        jeTitle,jeeTitle, $
  ;;        jeFiles,jeeFiles, $
  ;;        jeBounds,jeeBounds, $
  ;;        jeBinSize,jeeBinSize, $
  ;;        absSuff,posSuff,negSuff, $
  ;;        absTSuff,posTSuff,negTSuff, $
  ;;        suff, $
  ;;        CREATE_INDIVIDUAL_OUTPUT=create_individual_output

  ;; LOOPIT,ultimateFiles,ultimateDir, $
  ;;        jeMapTitle,jeeMapTitle, $
  ;;        jeMapFiles,jeeMapFiles, $
  ;;        jeBounds,jeeBounds, $
  ;;        jeBinSize,jeeBinSize, $
  ;;        absSuff,posSuff,negSuff, $
  ;;        absTSuff,posTSuff,negTSuff, $
  ;;        suff, $
  ;;        CREATE_INDIVIDUAL_OUTPUT=create_individual_output, $
  ;;        /MAPPEDQUANTS

END

PRO LOOPIT,ultimateFiles,ultimateDir, $
           jeTitle,jeeTitle, $
           jeFiles,jeeFiles, $
           jeBounds,jeeBounds, $
           jeBinSize,jeeBinSize, $
           absSuff,posSuff,negSuff, $
           absTSuff,posTSuff,negTSuff, $
           suff, $
           CHARSIZE=charSize, $
           CREATE_INDIVIDUAL_OUTPUT=create_individual_output, $
           MAPPEDQUANTS=mappedQuants

  FOR k=0,N_ELEMENTS(ultimateFiles)-1 DO BEGIN

     plot_i = 1

     RESTORE,ultimateDir+ultimateFiles[k]

     PRINT,k 
     PRINT_STATS,eSpec

     je  = eSpec.je
     jee = eSpec.jee

     IF KEYWORD_SET(mappedQuants) THEN BEGIN
        je  *= eSpec.mapFactor
        jee *= eSpec.mapFactor
     ENDIF

     good_i = WHERE(FINITE(je) OR FINITE(jee),nGood, $
                    COMPLEMENT=bad_i,NCOMPLEMENT=nBad)

     IF nBad GT 0 THEN BEGIN
        PRINT,'There are ' + STRCOMPRESS(nBad,/REMOVE_ALL) + ' ~finite inds here ...'
     ENDIF

     abs_i     = TEMPORARY(good_i)
     posJe_i   = CGSETINTERSECTION(abs_i,WHERE(je GT 0))
     negJe_i   = CGSETINTERSECTION(abs_i,WHERE(je LT 0))
     posJee_i  = CGSETINTERSECTION(abs_i,WHERE(jee GT 0))
     negJee_i  = CGSETINTERSECTION(abs_i,WHERE(jee LT 0))

     CGHISTOPLOT,ALOG10(ABS(je[abs_i])), $
                 TITLE=jeTitle[plot_i-1]+absTSuff, $
                 MININPUT=jeBounds[0]-1, $
                 MAXINPUT=jeBounds[1], $
                 BINSIZE=jeBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? jeFiles[k]+absSuff+suff : !NULL, $
                 LAYOUT=[3,2,plot_i],/WINDOW
     plot_i++

     CGHISTOPLOT,ALOG10(je[posJe_i]), $
                 TITLE=jeTitle[plot_i-1]+posTSuff, $
                 MININPUT=jeBounds[0], $
                 MAXINPUT=jeBounds[1], $
                 BINSIZE=jeBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? jeFiles[k]+posSuff+suff : !NULL, $
                 LAYOUT=[3,2,plot_i],ADDCMD=~KEYWORD_SET(save_PS)
     plot_i++

     CGHISTOPLOT,ALOG10(ABS(je[negJe_i])), $
                 TITLE=jeTitle[plot_i-1]+negTSuff, $
                 MININPUT=jeBounds[0], $
                 MAXINPUT=jeBounds[1], $
                 BINSIZE=jeBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? jeFiles[k]+negSuff+suff : !NULL, $
                 LAYOUT=[3,2,plot_i],ADDCMD=~KEYWORD_SET(save_PS)
     plot_i++

     CGHISTOPLOT,ALOG10(ABS(jee[abs_i])), $
                 TITLE=JeeTitle[(plot_i-1) MOD 3]+absTSuff, $
                 MININPUT=JeeBounds[0], $
                 MAXINPUT=JeeBounds[1], $
                 BINSIZE=JeeBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? JeeFiles[k]+absSuff+suff : !NULL, $
                 LAYOUT=[3,2,plot_i],ADDCMD=~KEYWORD_SET(save_PS)
     plot_i++

     CGHISTOPLOT,ALOG10(jee[posJee_i]), $
                 TITLE=JeeTitle[(plot_i-1) MOD 3]+posTSuff, $
                 MININPUT=JeeBounds[0], $
                 MAXINPUT=JeeBounds[1], $
                 BINSIZE=JeeBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? JeeFiles[k]+posSuff+suff : !NULL, $
                 LAYOUT=[3,2,plot_i],ADDCMD=~KEYWORD_SET(save_PS)
     plot_i++

     CGHISTOPLOT,ALOG10(ABS(jee[negJee_i])), $
                 TITLE=JeeTitle[(plot_i-1) MOD 3]+negTSuff, $
                 MININPUT=JeeBounds[0], $
                 MAXINPUT=JeeBounds[1], $
                 BINSIZE=JeeBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? JeeFiles[k]+negSuff+suff : !NULL, $
                 LAYOUT=[3,2,plot_i],ADDCMD=~KEYWORD_SET(save_PS)
     plot_i++

     eSpec        = !NULL

  ENDFOR

END

PRO LOOPIT_MAP,fluxInd, $
               ultimateFiles,ultimateDir, $
               fluxTitle,fluxMapTitle, $
               fluxFiles,fluxMapFiles, $
               fluxBounds,fluxMapBounds, $
               fluxBinSize,fluxMapBinSize, $
               absSuff,posSuff,negSuff, $
               absTSuff,posTSuff,negTSuff, $
               absTMapSuff,posTMapSuff,negTMapSuff, $
               suff, $
               CHARSIZE=charSize, $
               CREATE_INDIVIDUAL_OUTPUT=create_individual_output, $
               SAVE_PS=save_ps, $
               PSFILENAMES=psFileName



  FOR k=0,N_ELEMENTS(ultimateFiles)-1 DO BEGIN

     plot_i = 1

     RESTORE,ultimateDir+ultimateFiles[k]

     PRINT,k 
     PRINT_STATS,eSpec

     flux    = eSpec.(fluxInd)
     fluxMap = eSpec.(fluxInd) * eSpec.mapFactor

     eSpec  = !NULL

     good_i = WHERE(FINITE(flux),nGood, $
                    COMPLEMENT=bad_i,NCOMPLEMENT=nBad)

     IF nBad GT 0 THEN BEGIN
        PRINT,'There are ' + STRCOMPRESS(nBad,/REMOVE_ALL) + ' ~finite inds here ...'
     ENDIF

     abs_i     = TEMPORARY(good_i)
     posFlux_i   = CGSETINTERSECTION(abs_i,WHERE(flux GT 0))
     negFlux_i   = CGSETINTERSECTION(abs_i,WHERE(flux LT 0))

     
     IF KEYWORD_SET(save_PS) THEN BEGIN
        POPEN,PSFileName[k],/PORT,XSIZE=8,YSIZE=6
     ENDIF ELSE BEGIN
        CGWINDOW,WXSIZE=1000,WYSIZE=800
     ENDELSE

     CGHISTOPLOT,ALOG10(ABS(flux[abs_i])), $
                 TITLE=fluxTitle[plot_i-1]+absTSuff, $
                 YTITLE='', $
                 MININPUT=fluxBounds[0], $
                 MAXINPUT=fluxBounds[1], $
                 BINSIZE=fluxBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? fluxFiles[k]+absSuff+suff : !NULL, $
                 CHARSIZE=charSize, $
                 LAYOUT=[3,2,plot_i], $
                 WINDOW=~KEYWORD_SET(save_PS)
     plot_i++

     CGHISTOPLOT,ALOG10(flux[posFlux_i]), $
                 TITLE=fluxTitle[plot_i-1]+posTSuff, $
                 YTITLE='', $
                 MININPUT=fluxBounds[0], $
                 MAXINPUT=fluxBounds[1], $
                 BINSIZE=fluxBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? fluxFiles[k]+posSuff+suff : !NULL, $
                 CHARSIZE=charSize, $
                 LAYOUT=[3,2,plot_i],ADDCMD=~KEYWORD_SET(save_PS)
     plot_i++

     CGHISTOPLOT,ALOG10(ABS(flux[negFlux_i])), $
                 TITLE=fluxTitle[plot_i-1]+negTSuff, $
                 YTITLE='', $
                 MININPUT=fluxBounds[0], $
                 MAXINPUT=fluxBounds[1], $
                 BINSIZE=fluxBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? fluxFiles[k]+negSuff+suff : !NULL, $
                 CHARSIZE=charSize, $
                 LAYOUT=[3,2,plot_i],ADDCMD=~KEYWORD_SET(save_PS)
     plot_i++

     CGHISTOPLOT,ALOG10(ABS(fluxMap[abs_i])), $
                 TITLE=FluxMapTitle[(plot_i-1) MOD 3]+absTMapSuff, $
                 YTITLE='', $
                 MININPUT=FluxMapBounds[0], $
                 MAXINPUT=FluxMapBounds[1], $
                 BINSIZE=FluxMapBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? FluxMapFiles[k]+absSuff+suff : !NULL, $
                 CHARSIZE=charSize, $
                 LAYOUT=[3,2,plot_i],ADDCMD=~KEYWORD_SET(save_PS)
     plot_i++

     CGHISTOPLOT,ALOG10(fluxMap[posFlux_i]), $
                 TITLE=FluxMapTitle[(plot_i-1) MOD 3]+posTMapSuff, $
                 YTITLE='', $
                 MININPUT=FluxMapBounds[0], $
                 MAXINPUT=FluxMapBounds[1], $
                 BINSIZE=FluxMapBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? FluxMapFiles[k]+posSuff+suff : !NULL, $
                 CHARSIZE=charSize, $
                 LAYOUT=[3,2,plot_i],ADDCMD=~KEYWORD_SET(save_PS)
     plot_i++

     CGHISTOPLOT,ALOG10(ABS(fluxMap[negFlux_i])), $
                 TITLE=FluxMapTitle[(plot_i-1) MOD 3]+negTMapSuff, $
                 YTITLE='', $
                 MININPUT=FluxMapBounds[0], $
                 MAXINPUT=FluxMapBounds[1], $
                 BINSIZE=FluxMapBinSize, $
                 OUTPUT=KEYWORD_SET(create_individual_output) ? FluxMapFiles[k]+negSuff+suff : !NULL, $
                 CHARSIZE=charSize, $
                 LAYOUT=[3,2,plot_i],ADDCMD=~KEYWORD_SET(save_PS)
     plot_i++

     eSpec        = !NULL

     IF KEYWORD_SET(save_PS) THEN BEGIN
        PCLOSE
     ENDIF

  ENDFOR

END

PRO PRINT_STATS,eSpec

  PRINT,TIME_TO_STR([espec.x[0],espec.x[-1]]) 
  PRINT,espec.orbit[0],espec.orbit[-1] 
  PRINT,"N in this slice    : ",N_ELEMENTS(espec.x) 
  PRINT,"N finite Je        : ",N_ELEMENTS(WHERE(FINITE(eSpec.Je))) 
  PRINT,"N finite Jee       : ",N_ELEMENTS(WHERE(FINITE(eSpec.Jee))) 
  PRINT,"N finite AACGM.mlt : ",N_ELEMENTS(WHERE(FINITE(eSpec.coords.aacgm.mlt))) 
  PRINT,"N finite AACGM.alt : ",N_ELEMENTS(WHERE(FINITE(eSpec.coords.aacgm.alt))) 
  PRINT,"N finite AACGM.lat : ",N_ELEMENTS(WHERE(FINITE(eSpec.coords.aacgm.lat)))

END