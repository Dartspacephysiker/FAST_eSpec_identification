;2018/04/25
PRO JOURNAL__20180425__MAKE_NEWELLTYPE_FILES_FOR_MIDNIGHT__SHORTER_STREAK_REQUIREMENT

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; mltRange = [3,9]
  ;; mltRange = [15,21]
  mltRange=[-3.5,1.5]

  min_T_streakLen = 30

  FOR k=0,6 DO BEGIN
     PRINT,1000L+k*3000,", ",3999L+k*3000
     OUTPUT_NEWELLTYPE_STREAKS_TEXTFILE,/MONO, $
                                        MLTRANGE=mltRange, $
                                        ORBRANGE=[1000L+k*3000,3999L+k*3000], $
                                        MIN_T_STREAKLEN=min_T_streakLen
  ENDFOR
END

