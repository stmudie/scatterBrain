FUNCTION as_exportstucttoexcel::init

  @as_scatterheader.macro

  RETURN, 1

END

PRO as_exportstructtoexcel::export, structArray

  @as_scatterheader.macro

  self.NewFile

  numberTags = N_Tags(structArray)
  tagNames = Tag_Names(structArray)
  array = !null
  FOREACH name, TagNames, key DO BEGIN
    column = [name, String(structArray.(key))]
    array = [array,Transpose(column)]
  ENDFOREACH

 ; self.excelObj.SetProperty, ScreenUpdating = 0

  FOR i=1, N_Elements(array[0,*]) DO BEGIN
    stringI = StrCompress(i, /REMOVE_ALL)
    self.SetRange, 'A' + stringI + ':AD' + stringI, array[*,i-1]
  ENDFOR

  ;self.excelObj.SetProperty, ScreenUpdating = 1

END

PRO as_exportstructtoexcel__define

  void = {as_exportstructtoexcel, $
          INHERITS as_talktoexcel, $
          void : ''}

END


