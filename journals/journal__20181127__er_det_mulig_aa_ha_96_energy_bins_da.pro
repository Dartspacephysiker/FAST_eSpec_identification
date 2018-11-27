;2018/11/27 I am using orbit 48908 here, which (according to /thelonious_data1/FAST_electrons_2018/weird_energy_log.txt) has 96 energy bins straight from SDT, not my imposition.
;So, run ELECTRON_MOM[...] with a breakpoint set inside GET_DIFF_EFLUX around line 769, where CALL_FUNCTION happens.
;THEN do the plots
;; Check out Orb48908-contour_96_energy_bins.ps
PRO JOURNAL__20181127__ER_DET_MULIG_AA_HA_96_ENERGY_BINS_DA

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; ELECTRON_MOMENTS_AND_SPECTRAL_IDENTIFICATION_V0

  eeb_or_ees = 'ees'
  routine = 'get_fa_'+eeb_or_ees+'_c'

  time = '2008-09-28/12:40:24.029'
  time = '2008-09-28/12:40:39.056'
  t1 = S2T(time)
  dat = call_function(routine,t1,/calib,/ad)

  ;; See?
  ;; WINDOW,0,XSIZE=800,YSIZE=800
  ;; CONTOUR2D,dat,/POLAR

  xSize       = 9.5
  ySize       = 9.5
  land        = 1

  xWinSize    = 700
  yWinSize    = 700

  outDir = '/SPENCEdata/Research/Satellites/FAST/espec_identification/plots/'
  outPS = 'Orb48908-contour_96_energy_bins'
  POPEN,outDir+outPS, $
        XSIZE=xSize, $
        YSIZE=ySize, $
        LAND=land, $
        CTABLE=43, $
        ENCAPSULATED=eps, $
        OPTIONS={charsize:1.5}

  CONTOUR2D,dat, $
            ;; ANGLE=angle, $
            /POLAR, $
            /FILL, $
            ;; /OVERPLOT, $
            /MSEC, $
            LIMITS=limits, $
            /LABEL, $
            THICK=thick

  PCLOSE


END
