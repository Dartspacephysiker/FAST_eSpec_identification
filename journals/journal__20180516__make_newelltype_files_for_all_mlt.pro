;2018/05/16
PRO JOURNAL__20180516__MAKE_NEWELLTYPE_FILES_FOR_ALL_MLT

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; mltRange = [3,9]
  ;; mltRange = [15,21]
  mltRange=[0,24]

  min_T_streakLen = 30
  ;; max_T_streakLen = 60

  FOR k=0,6 DO BEGIN
     PRINT,1000L+k*3000,", ",3999L+k*3000
     OUTPUT_NEWELLTYPE_STREAKS_TEXTFILE,/MONO, $
                                        MLTRANGE=mltRange, $
                                        ORBRANGE=[1000L+k*3000,3999L+k*3000], $
                                        MIN_T_STREAKLEN=min_T_streakLen, $
                                        MAX_T_STREAKLEN=max_T_streakLen
  ENDFOR
END

