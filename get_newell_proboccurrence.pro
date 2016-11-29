;2016/06/11
PRO GET_NEWELL_PROBOCCURRENCE,h2dStrs, $
                              ;; COMBINE_ACCELERATED=comb_accelerated, $                              
                              OUT_H2D_LIST=out_H2D_list

  COMPILE_OPT idl2

  ;;Denominator doesn't change, even if we have accelerated
  denom                             = h2dStrs[0].data +h2dStrs[1].data+h2dStrs[2].data
  tmp_div_i                         = WHERE(denom GT 0)
  IF tmp_div_i[0] EQ -1 THEN STOP

  out_H2D_list                      = LIST()
  FOR i=0,N_ELEMENTS(h2dStrs)-1 DO BEGIN
     tmpH2D                         = h2dStrs[i].data
     tmpH2d[tmp_div_i]              = h2dStrs[i].data[tmp_div_i]/FLOAT(denom[tmp_div_i])
     out_H2D_list.ADD,tmpH2D
  ENDFOR

END