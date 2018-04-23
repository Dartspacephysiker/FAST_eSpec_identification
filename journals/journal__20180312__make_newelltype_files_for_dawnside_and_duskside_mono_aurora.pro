;2018/03/12
PRO JOURNAL__20180312__MAKE_NEWELLTYPE_FILES_FOR_DAWNSIDE_AND_DUSKSIDE_MONO_AURORA

  COMPILE_OPT IDL2,STRICTARRSUBS

  ;; mltRange = [3,9]
  ;; mltRange = [15,21]
  mltRange=[[ 3, 9], $
            [15,21]]

  FOR k=0,6 DO BEGIN
     PRINT,1000L+k*3000,", ",3999L+k*3000
     OUTPUT_NEWELLTYPE_STREAKS_TEXTFILE,/MONO, $
                                        MLTRANGE=mltRange, $
                                        ORBRANGE=[1000L+k*3000,3999L+k*3000]
  ENDFOR
END
