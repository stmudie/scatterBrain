PRO scatterBrainControl, _REF_EXTRA = extra

;  excel = Dialog_Message('Do you require Excel Scan Mode?', /QUESTION, /DEFAULT_NO)
;  IF excel EQ 'Yes' THEN startExcel = 1 ELSE startExcel = 0
  scatter = scatterBrain(/EPICS, STARTEXCEL = startExcel, _EXTRA = extra, version = 1.41)

END 