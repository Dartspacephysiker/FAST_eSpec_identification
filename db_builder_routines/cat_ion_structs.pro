PRO CAT_ION_STRUCTS,ionStruct,ionzz,cat_i

  IF N_ELEMENTS(cat_i) NE 0 THEN BEGIN
     ionTemp = {x:ionzz.x[cat_i], $
                  orbit:ionzz.orbit[cat_i], $
                  alt:ionzz.alt[cat_i], $
                  mlt:ionzz.mlt[cat_i], $
                  ilat:ionzz.ilat[cat_i], $
                  ji:ionzz.ji[cat_i], $
                  jei:ionzz.jei[cat_i]}

  ENDIF ELSE BEGIN
     ionTemp = ionzz
  ENDELSE

  IF N_ELEMENTS(ionStruct) EQ 0 THEN BEGIN
     ionStruct = ionTemp
  ENDIF ELSE BEGIN
     ionStruct = {x:[ionStruct.x,ionTemp.x], $
                  orbit:[ionStruct.orbit,ionTemp.orbit], $
                  alt:[ionStruct.mlt,ionTemp.alt], $
                  mlt:[ionStruct.mlt,ionTemp.mlt], $
                  ilat:[ionStruct.ilat,ionTemp.ilat], $
                  ji:[ionStruct.ji,ionTemp.ji], $
                  jei:[ionStruct.jei,ionTemp.jei]}
  ENDELSE

END