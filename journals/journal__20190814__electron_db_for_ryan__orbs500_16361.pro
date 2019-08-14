;2019/08/14
;; TOC
;; PART 1 - get electron db timestamps as doubles
;; PART 2 - align newly created timestamps with original DB
;; PART 3 - Make final db for passing to Ryan

PRO JOURNAL__20190814__ELECTRON_DB_FOR_RYAN__ORBS500_16361

  COMPILE_OPT IDL2,STRICTARRSUBS

  outdir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'
  outfile = 'FAST_electrons__20160607_db__orbs500-16361__incl_AACGM.sav'

  timedir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  outtimefile = 'eSpec_DB_20160607-TIME_DOUBLES.sav'
  timestrfile = 'eSpec_DB_20160607-TIME.sav'

  dbdir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  dbfile = 'eSpec_20160607_db--PARSED--Orbs_500-16361.sav'

  aacgmdir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  aacgmfile = 'eSpec_DB_20160607-AACGM.sav'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 1 - get electron db timestamps as doubles

  IF ~FILE_TEST(timedir+outtimefile) THEN BEGIN
     print,"Converting electron db timestamps from strings to doubles ..."
     restore,timedir+intimestrfile
     times = make_array(n_elements(timeStr),/DOUBLE)
     npaagaa=10000L
     nloops=n_elements(timestr)/npaagaa

     for i=0L,nloops do begin
        starti = i*npaagaa
        stopi = min([(i+1)*npaagaa-1,n_elements(timeStr)-1])
        print,i,starti,stopi
        tmpinds = [starti:stopi]
        times[tmpinds] = s2t(timestr[tmpinds])
     endfor

     save,times,filename=timedir+outtimefile

  ENDIF ELSE BEGIN
     print,"Already made electron db timestamps from strings to doubles!"
     restore,timedir+outtimefile
  ENDELSE

  restore,dbdir+dbfile

  ;; First sort espec DB
  keepespec_i = uniq(espec.x,sort(espec.x))
  espec = {x           : espec.x[keepespec_i], $         
           orbit       : espec.orbit[keepespec_i], $     
           mlt         : espec.mlt[keepespec_i], $       
           ilat        : espec.ilat[keepespec_i], $      
           alt         : espec.alt[keepespec_i], $       
           mono        : espec.mono[keepespec_i], $      
           broad       : espec.broad[keepespec_i], $     
           diffuse     : espec.diffuse[keepespec_i], $   
           je          : espec.je[keepespec_i], $        
           jee         : espec.jee[keepespec_i], $       
           nbad_espec  : espec.nbad_espec[keepespec_i]}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2 - align newly created timestamps with original DB
;; There are more rows in dbfile than in TIME_DOUBLES (and thus in AACGM) file

  check_sorted,espec.x
  check_sorted,times
 
  IF n_elements(espec.x) NE n_elements(times) THEN BEGIN
     STOP
  ENDIF

  IF max(abs(espec.x-times)) GT 0.5 THEN BEGIN
     print,"Consider inspection/cleaning!"
     STOP
  ENDIF

  ;; ;; timeUniqInds = uniq(times,sort(times))
  ;; ;; times = times[timeUniqInds]

  ;; timesInds = lindgen(n_elements(times))

  ;; aligner = value_closest2(espec.x,times,/CONSTRAINED)

  ;; ;; aligner = aligner[uniq(aligner)]
  ;; uniqInds = uniq(aligner,sort(aligner))
  ;; aligner = aligner[uniqInds]

  ;; ;; times = 

  ;; ;; cghistoplot,abs(espec.x[aligner]-times)
  ;; maxTDiff = 1                  ;seconds
  ;; keeps = where(abs(espec.x[aligner]-times) LT maxTDiff)

  ;; ;; Check for dupes
  ;; check_dupes,keeps
  ;; ;; > No duplicates in this array!

  ;; check_sorted,keeps
  ;; ;; > This array is sorted!

  ;; check_dupes,aligner
  ;; ;; We have dupes here...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 3 - Make final db for passing to Ryan

  ;; restore AACGM file
  restore,aacgmdir+aacgmfile

  elec = {x           : espec.x, $
             orbit       : espec.orbit, $     
             mlt         : espec.mlt, $       
             ilat        : espec.ilat, $      
             alt         : espec.alt, $       
             aacgm_mlt   : aacgm.mlt, $
             aacgm_mlat  : aacgm.lat, $
             aacgm_alt   : aacgm.alt, $
             mono        : espec.mono, $      
             broad       : espec.broad, $     
             diffuse     : espec.diffuse, $   
             je          : espec.je, $        
             jee         : espec.jee}

  ;; elec = {x           : espec.x[aligner[keeps]], $
  ;;            orbit       : espec.orbit[aligner[keeps]], $     
  ;;            mlt         : espec.mlt[aligner[keeps]], $       
  ;;            ilat        : espec.ilat[aligner[keeps]], $      
  ;;            alt         : espec.alt[aligner[keeps]], $       
  ;;            aacgm_mlt   : aacgm.mlt[keeps], $
  ;;            aacgm_ilat  : aacgm.ilat[keeps], $
  ;;            aacgm_alt   : aacgm.alt[keeps], $
  ;;            mono        : espec.mono[aligner[keeps]], $      
  ;;            broad       : espec.broad[aligner[keeps]], $     
  ;;            diffuse     : espec.diffuse[aligner[keeps]], $   
  ;;            je          : espec.je[aligner[keeps]], $        
  ;;            jee         : espec.jee[aligner[keeps]]}

  print,"Saving to " + outfile + ' ...'
  save,elec,filename=outdir+outfile

END
