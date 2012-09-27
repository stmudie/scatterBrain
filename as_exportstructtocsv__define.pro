FUNCTION as_exportstucttocsv::init

  @as_scatterheader.macro

  RETURN, 1

END

PRO as_exportstructtocsv::export, structArray, FILENAME = fileName

  @as_scatterheader.macro

  numberTags = N_Tags(structArray)
  tagNames = Tag_Names(structArray)
  array = !null
  FOREACH name, TagNames, key DO BEGIN
    column = [name, String(structArray.(key))]
    array = [array,Transpose(column)]
  ENDFOREACH

  reopen:

  IF ~KeyWord_Set(fileName) THEN fileName = Dialog_Pickfile(/OVERWRITE_PROMPT, /WRITE, FILTER = ['*.CSV'], DEFAULT_EXTENSION = 'CSV')

  IF fileName EQ 'Cancel' THEN RETURN

  OpenW, fileLun, fileName, ERROR = err, /GET_LUN
    IF err NE 0 THEN BEGIN
      result = Dialog_Message('Error opening file for writing. Choose another file?', /QUESTION)
      IF ISA(fileLun,/NUMBER) THEN Free_Lun, fileLun
      IF result EQ 'Yes' THEN BEGIN
        fileName = !null
        GOTO, reopen
      ENDIF ELSE RETURN
    ENDIF
  
    FOR i=0, N_Elements(column) - 1 DO BEGIN
      tempStr = StrJoin(array[*,i],',',/SINGLE)
      PrintF, fileLun, tempStr
    ENDFOR
  
  Free_Lun, fileLun

END

PRO as_exportstructtocsv__define

  void = {as_exportstructtocsv, $
          void : ''}

END