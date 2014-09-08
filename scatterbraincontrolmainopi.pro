PRO scatterBrainControlMainOPI, _REF_EXTRA = extra

;  excel = Dialog_Message('Do you require Excel Scan Mode?', /QUESTION, /DEFAULT_NO)
;  IF excel EQ 'Yes' THEN startExcel = 1 ELSE startExcel = 0
  setenv, 'EZCA_IDL_SHARE=C:\Program Files (x86)\EPICS_WIN32_binaries\ezcaIDL.dll'
  scatter = scatterBrain(/EPICS, STARTEXCEL = startExcel, _EXTRA = extra, version = 2.501)

END 