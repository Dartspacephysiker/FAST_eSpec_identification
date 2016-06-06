PRO CAT_JI_AND_JEI_FROM_NEWELL_FILES_INTO_STRUCT,ionStruct,ji_up,jei_up,orbits,alt,mlt,ilat,cat_i

  IF N_ELEMENTS(cat_i) NE 0 THEN BEGIN
     x         = ji_up.x[cat_i]
     ji_tmp    = ji_up.y[cat_i]
     jei_tmp   = jei_up.y[cat_i]
     orb_tmp   = orbits[cat_i]
     alt_tmp   = alt[cat_i]
     mlt_tmp   = mlt[cat_i]
     ilat_tmp  = ilat[cat_i]
  ENDIF ELSE BEGIN
     x         = ji_up.x
     ji_tmp    = ji_up.y
     jei_tmp   = jei_up.y
     orb_tmp   = orbits
     alt_tmp   = alt
     mlt_tmp   = mlt
     ilat_tmp  = ilat
  ENDELSE

  IF N_ELEMENTS(ionStruct) EQ 0 THEN BEGIN
     ionStruct = {x:x, $
                  orbit:orb_tmp, $
                  alt:alt_tmp, $
                  mlt:mlt_tmp, $
                  ilat:ilat_tmp, $
                  ji:ji_tmp, $
                  jei:jei_tmp}
  ENDIF ELSE BEGIN
     ionStruct = {x:[ionStruct.x,x], $
                  orbit:[ionStruct.orbit,orb_tmp], $
                  alt:[ionStruct.mlt,alt_tmp], $
                  mlt:[ionStruct.mlt,mlt_tmp], $
                  ilat:[ionStruct.ilat,ilat_tmp], $
                  ji:[ionStruct.ji,ji_tmp], $
                  jei:[ionStruct.jei,jei_tmp]}
  ENDELSE

END