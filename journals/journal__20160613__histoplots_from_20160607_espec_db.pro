PRO JOURNAL__20160613__HISTOPLOTS_FROM_20160607_ESPEC_DB

  broad    = WHERE(eSpec.broad EQ 1 OR eSpec.broad EQ 2,nB)
  mono     = WHERE(eSpec.mono EQ 1 OR eSpec.mono EQ 2,nM)
  diffuse  = WHERE(eSpec.diffuse EQ 1,nD)

  ;;Histo plots that are not normalized. Of course, everything is peaked at the highest altitudes
  CGHISTOPLOT,eSpec.alt[broad], $
              TITLE="Broadband e!U-!N Spectra, 20160607 e!U-!N DB", $
              OUTPUT='altitude_histo--broadband_e-_spectra--20160613.png', $
              XTITLE='Altitude (km)'
  
  CGHISTOPLOT,eSpec.alt[WHERE(eSpec.mono EQ 1 OR eSpec.mono EQ 2)], $
              TITLE="Monoenergetic e!U-!N Spectra, 20160607 e!U-!N DB", $
              OUTPUT='altitude_histo--monoenergetic_e-_spectra--20160613.png', $
              XTITLE='Altitude (km)'
  
  CGHISTOPLOT,eSpec.alt[WHERE(eSpec.diffuse EQ 1 OR eSpec.diffuse EQ 2)], $
              TITLE="Diffuse e!U-!N Spectra, 20160607 e!U-!N DB", $
              OUTPUT='altitude_histo--diffuse_e-_spectra--20160613.png', $
              XTITLE='Altitude (km)'

  
  binsize      = 80             ;km
  minLoc       = 340            ;km
  maxLoc       = 4180           ;km

  broadAltHist    = HISTOGRAM(eSpec.alt[broad]   ,BINSIZE=binsize,MIN=minLoc,MAX=maxLoc,/L64,LOCATIONS=bLoc)
  monoAltHist     = HISTOGRAM(eSpec.alt[mono]    ,BINSIZE=binsize,MIN=minLoc,MAX=maxLoc,/L64,LOCATIONS=mLoc)
  diffuseAltHist  = HISTOGRAM(eSpec.alt[diffuse] ,BINSIZE=binsize,MIN=minLoc,MAX=maxLoc,/L64,LOCATIONS=dLoc)
  denomHist       = broadAltHist + monoAltHist + diffuseAltHist
  goodLoc         = WHERE(denomHist GT 0,nGood)

  IF ~ARRAY_EQUAL(bLoc,mLoc) OR ~ARRAY_EQUAL(bLoc,dLoc) OR ~ARRAY_EQUAL(mLoc,dLoc) THEN BEGIN
     PRINT,"Histos are unequal; you're about to have bogusness."
     STOP
  ENDIF

  ;; histList        = LIST(broadAltHist,monoAltHist,diffuseAltHist,denomHist)
  histList        = LIST(broadAltHist,monoAltHist,diffuseAltHist)

  ;;Now plots
  window          = WINDOW(DIMENSIONS=[800,600])
  plotArr         = MAKE_ARRAY(4,/OBJ)
  plotMargin      = [0.15, 0.25, 0.15, 0.15]

  plotNames       = ['Broadband','Monoenergetic','Diffuse','Sum']
  colorArr        = ['Red','Blue','Purple','Black']
  lsArr           = ['-','-','-','-']

  xRange          = [340,4180]
  yRange          = [0,0.2]
  diffRange       = [0.85,1.01]
  FOR i=0,1 DO BEGIN
     plotArr[i]   = PLOT(bLoc, $
                         DOUBLE((histList[i])[goodLoc])/denomHist[goodLoc], $
                         AXIS_STYLE=1, $
                         XTITLE='Altitude (km)', $
                         XRANGE=xRange, $
                         YRANGE=yRange, $
                         NAME=plotNames[i], $
                         COLOR=colorArr[i], $
                         LINESTYLE=lsArr[i], $
                         THICK=2.2, $
                         /HISTOGRAM, $
                         OVERPLOT=i GT 0, $
                         MARGIN=plotMargin, $
                         CURRENT=window)
  ENDFOR

  plotArr[2]   = PLOT(bLoc, $
                      DOUBLE((histList[2])[goodLoc])/denomHist[goodLoc], $
                      XTITLE='Altitude (km)', $
                      XRANGE=xRange, $
                      YRANGE=diffRange, $
                      AXIS_STYLE=0, $
                      NAME=plotNames[2], $
                      COLOR=colorArr[2], $
                      LINESTYLE=lsArr[2], $
                      THICK=2.2, $
                      /HISTOGRAM, $
                      MARGIN=plotMargin, $
                      CURRENT=window)
  
  plotArr[3]   = PLOT(bLoc, $
                      DOUBLE((histList[0]+histList[1]+histList[2])[goodLoc])/denomHist[goodLoc], $
                      XTITLE='Altitude (km)', $
                      XRANGE=xRange, $
                      YRANGE=diffRange, $
                      AXIS_STYLE=0, $
                      NAME=plotNames[3], $
                      COLOR=colorArr[3], $
                      LINESTYLE=lsArr[3], $
                      THICK=2.2, $
                      /HISTOGRAM, $
                      MARGIN=plotMargin, $
                      CURRENT=window)

  ;; diffAxis = (plotArr[0].AXES)[3]
  ;; diffAxis.TITLE = 'Diffuse Probability'
  ;; diffAxis.AXIS_RANGE = diffRange
  ;; diffAxis.HIDE = 0

  diffAxis = axis('y', $
                  TARGET=plotArr[2], $
                  COLOR=colorArr[2], $
                  MAJOR=5, $    ; [0, 90, 180, 270, 360]
                  MINOR=2, $
                  LOCATION=[MAX(plotArr[0].xrange),0,0], $ ; right axis, data coordinates
                  TEXTPOS=1, $                         ; text faces outward
                  TICKDIR=1, $                         ; ticks face inward
                  TITLE='Diffuse Probability')

  legend          = LEGEND(TARGET=plotArr[*], $
                           /NORMAL, $
                           POSITION=[0.8,0.8])

END