;2016/06/07
FUNCTION RESIZE_ION_DB,ionStruct,cat_i

  ions = {x:ionStruct.x[cat_i], $
          orbit:ionStruct.orbit[cat_i], $
          alt:ionStruct.alt[cat_i], $
          mlt:ionStruct.mlt[cat_i], $
          ilat:ionStruct.ilat[cat_i], $
          ji:ionStruct.ji[cat_i], $
          jei:ionStruct.jei[cat_i]}

  RETURN,ions
END