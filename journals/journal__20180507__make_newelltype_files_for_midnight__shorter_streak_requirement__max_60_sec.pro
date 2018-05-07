;2018/05/07
PRO JOURNAL__20180507__MAKE_NEWELLTYPE_FILES_FOR_MIDNIGHT__SHORTER_STREAK_REQUIREMENT__MAX_60_SEC

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; mltRange = [3,9]
  ;; mltRange = [15,21]
  mltRange=[-3.5,1.5]

  min_T_streakLen = 30
  max_T_streakLen = 60

  FOR k=0,0 DO BEGIN
     PRINT,1000L+k*3000,", ",3999L+k*3000
     OUTPUT_NEWELLTYPE_STREAKS_TEXTFILE,/MONO, $
                                        MLTRANGE=mltRange, $
                                        ORBRANGE=[1000L+k*3000,3999L+k*3000], $
                                        MIN_T_STREAKLEN=min_T_streakLen, $
                                        MAX_T_STREAKLEN=max_T_streakLen
  ENDFOR
END
