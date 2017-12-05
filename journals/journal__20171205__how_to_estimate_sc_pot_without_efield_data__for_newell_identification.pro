;2017/12/05
PRO JOURNAL__20171205__HOW_TO_ESTIMATE_SC_POT_WITHOUT_EFIELD_DATA__FOR_NEWELL_IDENTIFICATION

  COMPILE_OPT IDL2,STRICTARRSUBS

  load_maximus_and_cdbtime,maximus,/no_memory_load

  pos10scpot = WHERE(maximus.sc_pot GT 20)
  neg10scpot = WHERE(maximus.sc_pot LT -20)

  posOrbs = maximus.orbit[pos10scpot]
  negOrbs = maximus.orbit[neg10scpot]

  posOrbs = posOrbs[UNIQ(posOrbs,SORT(posOrbs))]
  negOrbs = negOrbs[UNIQ(negOrbs,SORT(negOrbs))]

  !P.MULTI = [0,2,2,0,0]

  cghistoplot,maximus.sc_pot
  cghistoplot,maximus.sc_pot[pos10scpot]
  cghistoplot,maximus.sc_pot[neg10scpot]

  ;; Maybe something like this:
  ;; 0. Start at field-aligned angle, zero energy
  ;; 1. While (energy below 45 eV) loop
  ;; 1a. If flux is above 5.0e8, set to zero

END
