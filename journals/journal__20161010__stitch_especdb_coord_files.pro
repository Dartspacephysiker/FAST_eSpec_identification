;;10/10/16
PRO JOURNAL__20161010__STITCH_ESPECDB_COORD_FILES

  COMPILE_OPT IDL2

  ;;The one that made the files we're loading
  orig_routineName  = 'JOURNAL__20160827__CONVERT_ESPECDB_ILATS_TO_AACGM__ALL_OF_EM'

  originating_routine = 'JOURNAL__20161010__STITCH_ESPECDB_COORD_FILES'

  just_print_stats    = 1
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Input

  ;;This is the eSpec DB being referenced, o'course
  coordDir         = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  
  ;;Just for reference--we don't actually use these here
  ephemFileIndArr  = [[      0,10000000,20000000], $
                      [9999999,19999999,28604344]]

  coordFiles       = 'sorted--eSpec_20160607_db--PARSED--Orbs_500-16361--GEO_and_MAG_coords' + $
                     ['_1','_2','_3']+'.sav'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;output
  outDir           = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/alternate_coords/'
  savFiles         = 'sorted--eSpec_20160607_db--Orbs_500-16361--AACGM_v2_coords' + $
                     ['_1','_2','_3']+'--recalc_for_every_point.sav'

  LOAD_NEWELL_ESPEC_DB,eSpec, $
                       /DONT_LOAD_IN_MEMORY, $
                       /DONT_CONVERT_TO_STRICT_NEWELL

  orbStrings       = REFORM(STRING(FORMAT='(I0)',espec.orbit[ephemfileindarr]),N_ELEMENTS(ephemFileIndArr[*,0]),N_ELEMENTS(ephemFileIndArr[0,*]))
  ;; ultimateFiles    = 'eSpec_20160607_db--Orbs_500-16361--with_alternate_coords__mapFactors--' + $
  ultimateFiles    = 'eSpec_20160607_db--Orbs_' + orbStrings[*,0] + '-' + orbStrings[*,1] + $
                     '--with_alternate_coords__mapFactors--' + $
                     ['1','2','3']+'.sav'
  ultimateDir      = '/SPENCEdata/Research/database/FAST/dartdb/electron_Newell_db/fully_parsed/'

  ordinal = ['First','Second','Third']

  IF ~KEYWORD_SET(just_print_stats) THEN BEGIN

     FOR k=0,N_ELEMENTS(coordFiles)-1 DO BEGIN

        LOAD_NEWELL_ESPEC_DB,eSpec,/DONT_LOAD_IN_MEMORY, $
                             /DONT_CONVERT_TO_STRICT_NEWELL

        RESTORE,coordDir+coordFiles[k]

        RESTORE,outDir+savFiles[k]

        inds  = [ephemFileIndArr[k,0]:ephemFileIndArr[k,1]]

        IF N_ELEMENTS(inds) NE N_ELEMENTS(AACGMStruct.alt) THEN STOP
        IF N_ELEMENTS(inds) NE N_ELEMENTS(eSpec_GEO.alt)   THEN STOP
        IF N_ELEMENTS(inds) NE N_ELEMENTS(eSpec_MAG.alt)   THEN STOP

        PRINT,TIME_TO_STR([espec.x[inds[0]],espec.x[inds[-1]]])

        eSpec = {x:espec.x[inds], $
                 orbit:eSpec.orbit[inds], $
                 coords:{sdt:{alt:eSpec.alt[inds], $
                              mlt:eSpec.mlt[inds], $
                              ilat:eSpec.ilat[inds]}, $
                         aacgm:{alt:AACGMStruct.alt, $
                                mlt:AACGMStruct.mlt, $
                                lat:AACGMStruct.lat}, $
                         geo:{alt:eSpec_GEO.alt, $
                              lon:eSpec_GEO.lon, $
                              lat:eSpec_GEO.lat}, $
                         mag:{alt:eSpec_MAG.alt, $
                              lon:eSpec_MAG.lon, $
                              lat:eSpec_MAG.lat}}, $
                 je:eSpec.je[inds], $
                 jee:eSpec.jee[inds], $
                 mapFactor:eSpec.mapFactor[inds], $
                 info:{creation_date:GET_TODAY_STRING(/DO_YYYYMMDD_FMT), $
                       sign_convention:"Positive fluxes are earthward in both hemispheres", $
                       originating_routine:originating_routine}, $
                 misc:ordinal[k] + ' of three files'}

        PRINT,k 
        PRINT_STATS,eSpec

        PRINT,"Saving stitched file to " + ultimateFiles[k] + ' ...'
        SAVE,eSpec,FILENAME=ultimateDir+ultimateFiles[k]


        eSpec        = !NULL
        AACGMStruct  = !NULL
        eSpec_GEO    = !NULL
        eSpec_MAG    = !NULL
        ;; eSpec_i   = !NULL

     ENDFOR

  ENDIF ELSE BEGIN

     FOR k=0,N_ELEMENTS(coordFiles)-1 DO BEGIN

        RESTORE,ultimateDir+ultimateFiles[k]
        PRINT,k 
        PRINT_STATS,eSpec

        eSpec = !NULL

     ENDFOR

  ENDELSE

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