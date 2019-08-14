;2019/08/14
;; TOC
;; PART 1 - get electron db timestamps as doubles
;; PART 2 - align newly created timestamps with original DB
;; PART 3 - Make final db for passing to Ryan

PRO JOURNAL__20190814__ELECTRON_DB_FOR_RYAN__ORBS500_16361

  COMPILE_OPT IDL2,STRICTARRSUBS

  timedir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  outtimefile = 'eSpec_DB_20160607-TIME_DOUBLES.sav'
  timestrfile = 'eSpec_DB_20160607-TIME.sav'

  outdir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/'
  outfile = 'FAST_electrons__20160607_db__orbs500-16361__incl_AACGM.sav'

  dbdir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'
  dbfile = 'eSpec_20160607_db--PARSED--Orbs_500-16361.sav'

  aacgmdir = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  aacgmfile = 'eSpec_DB_20160607-AACGM.sav'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 1 - get electron db timestamps as doubles

  IF ~FILE_TEST(timedir+outtimefile) THEN BEGIN
     print("Converting electron db timestamps from strings to doubles ...")
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
     print("Already made electron db timestamps from strings to doubles!")
  ENDELSE

  restore,dbdir+dbfile

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 2 - align newly created timestamps with original DB
;; There are more rows in dbfile than in TIME_DOUBLES (and thus in AACGM) file

  aligner = value_closest2(espec.x,times,/CONSTRAINED)
  cghistoplot,abs(espec.x[aligner]-times)
  maxTDiff = 1                  ;seconds
  keeps = where(abs(espec.x[aligner]-times) LT maxTDiff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART 3 - Make final db for passing to Ryan

  ;; restore AACGM file
  restore,aacgmdir+aacgmfile

  finalDB = {x           : espec.x[aligner[keeps]], $
             orbit       : espec.orbit[aligner[keeps]], $     
             mlt         : espec.mlt[aligner[keeps]], $       
             ilat        : espec.ilat[aligner[keeps]], $      
             alt         : espec.alt[aligner[keeps]], $       
             aacgm_mlt   : aacgm.mlt[keeps], $
             aacgm_ilat  : aacgm.ilat[keeps], $
             aacgm_alt   : aacgm.alt[keeps], $
             mono        : espec.mono[aligner[keeps]], $      
             broad       : espec.broad[aligner[keeps]], $     
             diffuse     : espec.diffuse[aligner[keeps]], $   
             je          : espec.je[aligner[keeps]], $        
             jee         : espec.jee[aligner[keeps]]}

  print("Saving to " + outfile + ' ...')
  save,finalDB,filename=outdir+outfile

END
