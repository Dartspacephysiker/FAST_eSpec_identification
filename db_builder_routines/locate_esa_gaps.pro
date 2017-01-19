;+
;PROCEDURE:	LOCATE_ESA_GAPS
;PURPOSE:	
;	Make a cdf file with esa data
;INPUT:		
;	data_str, 	a string (either 'fa_ees_c','fa_ess','fa_ies','fa_ieb' ...)
;			where get_'string' returns a 2D data structure
;KEYWORDS:
;	T1:		start time, seconds since 1970
;	T2:		end time, seconds since 1970		
;	NENERGY		number of energy bins
;	NBINS		number of angle bins
;	UNITS:		convert to these units if included
;	NAME:  		New name of the Data Quantity
;
;
;CREATED BY:	J.McFadden	99/01/19
;VERSION:	1
;LAST MODIFICATION:  		00-8-15	
;MOD HISTORY:
;	00-8-15	Added modifications involving "last_delta_time" to account for fast/slow survey transitions
;
;	04-4-19 Changed to handle 96 energy mode for EESA, put the retrace back in.
;
;NOTES:	  
;	Current version only works for FAST eesa and iesa data
;-
;;01/18/17
PRO LOCATE_ESA_GAPS,data_str,  $
                    T1=t1, $
                    T2=t2, $
                    NENERGY=nenergy, $ 
                    NBINS=nbins, $
                    units = units, $
                    gap_time = gap_time

  COMPILE_OPT IDL2

  ;;	Time how long the routine takes
  ex_start = SYSTIME(1)

  ;;	Set defaults for keywords, etc.
  n       = 0
  max     = 5000                ; this could be improved (increased from 2000 - KRB)

  routine = 'get_'+data_str

  IF KEYWORD_SET(t1) THEN BEGIN
     t   = t1
     dat = CALL_FUNCTION(routine,t,/CALIB)
  ENDIF ELSE BEGIN
     t   = 1000                 ; get first sample
     dat = CALL_FUNCTION(routine,t,/CALIB,/START)
  ENDELSE

  last_time       = (dat.time+dat.end_time)/2.
  last_delta_time = dat.end_time-dat.time
  nenergy         = dat.nenergy


  ;;	Collect the data - Main Loop
  
  IF KEYWORD_SET(t2) THEN tmax = t2 ELSE tmax = 1.E30

  WHILE (dat.valid NE 0) AND (n LT max) DO BEGIN
     IF (dat.valid EQ 1) THEN BEGIN

        ;; Test to see if a transition between fast and slow survey occurs, 
        ;; ie delta_time changes, and skip some data if it does.
        IF (ABS((dat.end_time-dat.time) - last_delta_time) GT 1. ) THEN BEGIN
           IF routine EQ 'fa_ees_c' THEN nskip=2 ELSE nskip=3
           ;; If fast to slow, skip two or three arrays
           IF (dat.end_time-dat.time) GT last_delta_time THEN BEGIN
              FOR i=1,nskip DO BEGIN
                 dat = CALL_FUNCTION(routine,t,/CALIB,/AD)
              ENDFOR
           ENDIF ELSE BEGIN
              WHILE dat.time LT last_time+7.5 DO BEGIN
                 dat = CALL_FUNCTION(routine,t,/CALIB,/AD)
              ENDWHILE
           ENDELSE
        ENDIF
        
        ;; Test for data gaps and add NAN if gaps are present.
        IF ABS((dat.time+dat.end_time)/2.-last_time) GE gap_time THEN BEGIN

           IF n GE 2 THEN dbadtime = cdfdat[n-1].time - cdfdat[n-2].time ELSE dbadtime = gap_time/2.

           cdfDat[n].time               = (last_time) + dbadtime
           cdfDat[n].delta_time         = !values.f_nan
           cdfDat[n].data[*,*]          = !values.f_nan
;		cdfDat[n].energy[*,*]   = !values.f_nan
           cdfDat[n].energy[*]          = !values.f_nan
           cdfDat[n].angle[*,*]         = !values.f_nan
           cdfDat[n].n_energy           = !values.f_nan
           cdfDat[n].n_angle            = !values.f_nan
           n=n+1

           IF ((dat.time+dat.end_time)/2. GT cdfdat[n-1].time + gap_time) AND (n LT max) THEN BEGIN
              cdfDat[n].time            = (dat.time+dat.end_time)/2. - dbadtime
              cdfDat[n].delta_time      = !values.f_nan
              cdfDat[n].data[*,*]       = !values.f_nan
              ;; cdfDat[n].energy[*,*]  = !values.f_nan
              cdfDat[n].energy[*]       = !values.f_nan
              cdfDat[n].angle[*,*]      = !values.f_nan
              cdfDat[n].n_energy        = !values.f_nan
              cdfDat[n].n_angle         = !values.f_nan
              n=n+1
           ENDIF

        ENDIF

        dat                                   = CONV_UNITS(dat,units)
        data[*,*]                             = 0.
        ;; data(0:nenergy-1,0:dat.nbins-1)=dat.data(retrace:nenergy-1+retrace,0:dat.nbins-1)
        data[0:dat.nenergy-1,0:dat.nbins-1]   = dat.data
        energy[*,*]                           = 0.
        ;; energy(0:nenergy-1,0:dat.nbins-1)=dat.energy(retrace:nenergy-1+retrace,0:dat.nbins-1)
        energy[0:dat.nenergy-1]               = REFORM(dat.energy[*,0])
        angle[*,*]                            = 0.
        ;; angle(0:nenergy-1,0:dat.nbins-1)=dat.theta(retrace:nenergy-1+retrace,0:dat.nbins-1)
        angle[0:dat.nenergy-1,0:dat.nbins-1]  = dat.theta

        IF n LT max THEN BEGIN
           
           cdfDat[n].time                     = (dat.time+dat.end_time)/2.
           cdfDat[n].delta_time               = dat.end_time-dat.time
           cdfDat[n].data[*,*]                = data
           ;; cdfDat[n].energy[*,*]           = energy
           cdfDat[n].energy[*]                = energy
           cdfDat[n].angle[*,*]               = angle
           cdfDat[n].n_energy                 = dat.nenergy
           cdfDat[n].n_angle                  = dat.nbins

           last_time                          = cdfDat[n].time
           last_delta_time                    = cdfDat[n].delta_time 
           n                                  = n+1
        ENDIF

     ENDIF ELSE BEGIN
        PRINT,'Invalid packet, dat.valid ne 1, at: ',time_to_str(dat.time)
     ENDELSE

     dat = CALL_FUNCTION(routine,t,/CALIB,/AD)
     IF dat.valid NE 0 THEN IF dat.time GT tmax THEN dat.valid=0

  ENDWHILE

  RETURN

END
