PRO JOURNAL__20160803__SORT_ESPEC_DB_SO_THAT_ITS_RIGHT

  COMPILE_OPT idl2

  ;; LOAD_NEWELL_ESPEC_DB,/JUST_TIMES,OUT_TIMES=eSpecTimes,/DONT_LOAD_IN_MEMORY
  LOAD_NEWELL_ESPEC_DB,eSpec, $
                       /DONT_LOAD_IN_MEMORY, $
                       /DONT_PERFORM_CORRECTION, $
                       NEWELLDBDIR=NewellDBDir, $
                       NEWELLDBFILE=NewellDBFile

  newFile  = "sorted--" + NewellDBFile
  nBef     = N_ELEMENTS(eSpec.x)

  ;;First sort 'em
  sorted   = SORT(eSpec.x)

  eSpec    = { $
             x          : eSpec.x[sorted]       , $         
             orbit      : eSpec.orbit[sorted]   , $     
             mlt        : eSpec.mlt[sorted]     , $       
             ilat       : eSpec.ilat[sorted]    , $      
             alt        : eSpec.alt[sorted]     , $       
             mono       : eSpec.mono[sorted]    , $      
             broad      : eSpec.broad[sorted]   , $     
             diffuse    : eSpec.diffuse[sorted] , $   
             je         : eSpec.je[sorted]      , $        
             jee        : eSpec.jee[sorted]     , $       
             nbad_espec : eSpec.nbad_espec[sorted] $
             }

  ;;Now unique 'em
  sorted   = UNIQ(eSpec.x)
  nAft     = N_ELEMENTS(sorted)

  eSpec    = { $
             x          : eSpec.x[sorted]       , $         
             orbit      : eSpec.orbit[sorted]   , $     
             mlt        : eSpec.mlt[sorted]     , $       
             ilat       : eSpec.ilat[sorted]    , $      
             alt        : eSpec.alt[sorted]     , $       
             mono       : eSpec.mono[sorted]    , $      
             broad      : eSpec.broad[sorted]   , $     
             diffuse    : eSpec.diffuse[sorted] , $   
             je         : eSpec.je[sorted]      , $        
             jee        : eSpec.jee[sorted]     , $       
             nbad_espec : eSpec.nbad_espec[sorted] $
             }

  PRINT,"Lost " + STRCOMPRESS(nBef-nAft,/REMOVE_ALL) + ' entries for being too blasé ...'


  PRINT,"Saving sorted file: " + newFile
  SAVE,eSpec,FILENAME=NewellDBDir+newFile


END