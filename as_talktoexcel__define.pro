FUNCTION AS_TalkToExcel::init, VISIBLE=visible

  @as_scatterheader.macro

  excelObj =Obj_New('IDLcomIDispatch$CLSID$00024500_0000_0000_C000_000000000046')
  IF Obj_Valid(excelObj) THEN BEGIN
    IF N_Elements(visible) EQ 0 THEN visible = 1
    excelObj.SetProperty, VISIBLE = visible
    self.excelObj = excelObj
    RETURN, 1
  ENDIF ELSE RETURN, 0
  
END

PRO AS_TalkToExcel::NewFile

  self.excelObj.GetProperty, WORKBOOKS = workBooks
  workbook = workBooks.add()

END

PRO AS_TalkToExcel::OpenFile, file

  @as_scatterheader.macro

  IF N_Elements(file) EQ 0 THEN file = self.excelObj.GetOpenFileName("Excel Workbook (*.xlsx), *.xlsx,Excel Macro-Enable Workbook (*.xlsm), *.xlsm,Excel 97-2003 Workbook (*.xls), *.xls")
  IF file NE '' THEN BEGIN 
    self.excelObj.GetProperty, WORKBOOKS = workBook
    book = workBook.Open(file)
        
    self.book = book
    Obj_Destroy, workBook
    
    self.ChangeSheet, 1
    
  ENDIF
  
END

PRO AS_TalkToExcel::SaveFile

  @as_scatterheader.macro

  self.book.save

END

PRO AS_TalkToExcel::SaveCopyAs, file

  @as_scatterheader.macro

  IF N_Elements(file) EQ 0 THEN file = self.excelObj.GetSaveAsFileName('',"Excel Workbook (*.xlsx), *.xlsx,Excel Macro-Enable Workbook (*.xlsm), *.xlsm,Excel 97-2003 Workbook (*.xls), *.xls")
  IF file NE '' THEN self.book.SaveCopyAs, file
  
END

PRO AS_TalkToExcel::SaveAsFile, file

  @as_scatterheader.macro

  IF N_Elements(file) EQ 0 THEN file = self.excelObj.GetSaveAsFileName('',"Excel Workbook (*.xlsx), *.xlsx,Excel Macro-Enable Workbook (*.xlsm), *.xlsm,Excel 97-2003 Workbook (*.xls), *.xls")
  IF file NE '' THEN BEGIN 
    self.book.SaveAs, file
  ENDIF

END

PRO AS_TalkToExcel::ChangeSheet, nameOrIndex

  @as_scatterheader.macro

  errorCount = 0
  CATCH, error_status
  
  IF error_status NE 0 THEN BEGIN
    errorCount++
    IF errorCount EQ 1 THEN BEGIN
      self.excelObj.GetProperty, WORKBOOKS = workBook
      workBook.GetProperty, ITEM = book, 1
      self.book = book
      Obj_Destroy,workbook
    ENDIF ELSE BEGIN
      CATCH, /CANCEL
      RETURN
    ENDELSE
    CATCH, /CANCEL
  ENDIF

  self.book.GetProperty, SHEETS=sheets
  sheets.GetProperty, ITEM=sheet, nameOrIndex
  sheet.Activate
  self.activeSheet = sheet
  Obj_Destroy, sheets

END

PRO AS_TalkToExcel::Copy, source, destination

  @as_scatterheader.macro

  SWITCH N_Elements(source) OF
    2 : self.ChangeSheet, source[1]
    1 : BEGIN 
          self.activeSheet.GetProperty, RANGE = sourceRange, source[0]
          BREAK
        END
    ELSE : RETURN 
  ENDSWITCH


  SWITCH N_Elements(source) OF
    2 : self.ChangeSheet, destination[1]
    1 : BEGIN 
          self.activeSheet.GetProperty, RANGE = destinationRange, destination[0]
          BREAK
        END
    ELSE : RETURN 
  ENDSWITCH
  
  result = sourceRange.Copy(destinationRange)

  Obj_Destroy, sourceRange, destinationRange
  
  RETURN

END

FUNCTION AS_TalkToExcel::GetRange, range, ADDRESS = address, OFFSET = offset

  @as_scatterheader.macro

  IF N_Elements(range) EQ 0 THEN RETURN, ''
  errorCount = 0
  CATCH, error_status
   
  IF error_status NE 0 THEN BEGIN
    errorCount++
    IF errorCount EQ 1 THEN self.ChangeSheet, 1 ELSE BEGIN
      CATCH, /CANCEL
      RETURN, 0
    ENDELSE
  ENDIF 
  
  self.activeSheet.GetProperty, RANGE=tempRange, range[0]
  IF KeyWord_Set(offset) THEN BEGIN
    tempRange.GetProperty, OFFSET = offsetRange, offset[0], offset[1]
    range = offsetRange
  ENDIF ELSE range = tempRange 
  
  IF KeyWord_Set(address) THEN BEGIN
    range.GetProperty, ADDRESS = add
    addArray = StrSplit(add, ',',/EXTRACT)
    Obj_Destroy, range
    RETURN, addArray
  ENDIF
  range.GetProperty, VALUE=value, AREAS=areas
  areas.GetProperty, COUNT=count
  IF count GT 1 THEN BEGIN
    value = StrArr(count)
    FOR i = 1, count DO BEGIN
      areas.GetProperty, ITEM=item, i
      item.GetProperty, VALUE=temp
      Obj_Destroy, item
      value[i-1] = temp
    ENDFOR
  ENDIF
  
  Obj_Destroy, tempRange
  IF Obj_Valid(offsetRange) THEN Obj_Destroy, offsetRange
  Obj_Destroy, areas
  
  dims = Size(value)
  
  IF dims[0] EQ 2 THEN BEGIN
    value = Reform(value, dims[2], dims[1], /OVERWRITE)
    value = Transpose(value)
  ENDIF
  IF Size(value, /TYPE) EQ 0 THEN value = ''
  RETURN, value

END

PRO AS_TalkToExcel::Highlight, inRange, REMOVE=remove, MOVE=move

  @as_scatterheader.macro

  IF N_Elements(inRange) EQ 0 OR inRange EQ '' THEN RETURN
  IF ~Obj_Valid(self.activeSheet) THEN self.ChangeSheet, 1
  self.activeSheet.GetProperty, RANGE=range, inRange
  range.Select
  range.GetProperty, FORMATCONDITIONS=form
  
  IF KeyWord_Set(remove) OR KeyWord_Set(move) THEN BEGIN
    form.GetProperty, COUNT=count
    IF count GE 1 THEN BEGIN
      condition = form.Item(1)
      condition.GetProperty, TYPE=type
                  
      CATCH, error_status
      IF error_status NE 0 THEN BEGIN
        type = 0
        Print, "Can't call delete for conditional formatting iconset. Consider deleting formating on each cell separately."
        CATCH, /CANCEL
      ENDIF
      IF type EQ 6 THEN BEGIN
        Obj_Destroy, range
        IF ~KeyWord_Set(move) THEN moveTo = 'A1' ELSE moveTo = move
        self.activeSheet.GetProperty, RANGE=range, moveTo
        condition.ModifyAppliesToRange, range
        Obj_Destroy, condition
        IF ~KeyWord_Set(move) THEN BEGIN
          range.GetProperty, FORMATCONDITIONS=form
          condition = form.Item(1)
          condition.delete
        ENDIF
      ENDIF
    ENDIF
  ENDIF ELSE BEGIN

    self.book.GetProperty, ICONSETS=iconsets
    iconsets.GetProperty, ITEM=iconset, 1
    Obj_Destroy, iconsets
    
    condition = form.AddIconSetCondition(iconset)
    condition.SetFirstPriority
    condition.GetProperty, ICONCRITERIA=criteria, 1
    criteria.GetProperty, ITEM=criterion, 2
    criterion.SetProperty, VALUE = -2000
    Obj_Destroy, criterion
    criteria.GetProperty, ITEM=criterion, 3
    criterion.SetProperty, VALUE = -1000
    Obj_Destroy, criterion
    Obj_Destroy, iconset

  ENDELSE
  
  IF Obj_Valid(form) THEN Obj_Destroy, form
  IF Obj_Valid(condition) THEN Obj_Destroy, condition
  Obj_Destroy, range

END

PRO AS_TalkToExcel::RunMacro, macro

  @as_scatterheader.macro

  self.excelObj.run, macro

END

PRO AS_TalkToExcel::SetRange, range, inValue, BACKCOLOUR=backColour, FONTCOLOUR=fontColour, FONTSTYLE=fontStyle

  @as_scatterheader.macro

  IF ~Obj_Valid(self.activeSheet) THEN self.ChangeSheet, 1
  self.activeSheet.GetProperty, RANGE=range, range[0]
  range.GetProperty, AREAS=areas
  areas.GetProperty, COUNT=areaCount
   
  valueCount = N_Elements(inValue)
  dims = Size(inValue, /STRUCTURE)
  CASE dims.N_DIMENSIONS OF 
    0 : value = inValue 
    1 : value = Transpose(inValue)
    2 : value = Reform(Transpose(inValue), (dims.DIMENSIONS)[0],(dims.DIMENSIONS)[1])
    ELSE : RETURN
  ENDCASE
   
    cellNo = 0
  
  IF areaCount EQ 1 AND dims.TYPE GE 1 AND dims.TYPE LE 5 THEN range.SetProperty, VALUE2=value ELSE BEGIN
  
    FOR i = 1, areaCount DO BEGIN
      areas.GetProperty, ITEM=item, i
      item.GetProperty, COUNT = itemCount
      FOR j = 1, itemCount DO BEGIN
        IF cellNo GE valueCount THEN BREAK
        item.GetProperty, ITEM = cell, j
        IF value[cellNo] NE !Null THEN cell.SetProperty, VALUE2=value[cellNo]
        Obj_Destroy, cell
        cellNo++
      ENDFOR
      Obj_Destroy, item
    ENDFOR
  
    Obj_Destroy, areas

  ENDELSE

;****************
;      Formatting
;****************

  IF KeyWord_Set(backColour) THEN BEGIN
    range.GetProperty, INTERIOR = interior
    binArray = StrArr(24)
    binArray[0:7] = (Binary(Byte(backColour[0])))[0:7]
    binArray[8:15] = (Binary(Byte(backColour[1])))[0:7]
    binArray[16:23] = (Binary(Byte(backColour[2])))[0:7]
    colour = Total( (Byte(Reverse(binArray)) EQ 49) * 2L^Indgen(24) )
    interior.SetProperty, Color = colour 
    Obj_Destroy, interior
  ENDIF
  IF KeyWord_Set(fontColour) OR KeyWord_Set(fontStyle) THEN BEGIN
    range.GetProperty, FONT = font
    IF KeyWord_Set(fontColour) THEN BEGIN
      binArray = StrArr(24)
      binArray[0:7] = (Binary(Byte(fontColour[0])))[0:7]
      binArray[8:15] = (Binary(Byte(fontColour[1])))[0:7]
      binArray[16:23] = (Binary(Byte(fontColour[2])))[0:7]
      colour = Total( (Byte(Reverse(binArray)) EQ 49) * 2L^Indgen(24) )
      font.SetProperty, COLOR = colour 
    ENDIF
    IF KeyWord_Set(fontStyle) THEN font.SetProperty, FONTSTYLE = fontStyle  ; String "Bold" for bold, "Italic" for italic, "Bold Italic" for bold italic, "Regular" for regular
    Obj_Destroy, font
  ENDIF

  Obj_Destroy, range
  
END

PRO AS_TalkToExcel::CloseFile

  @as_scatterheader.macro

  self.book->Close

END

PRO AS_TalkToExcel::ShowExcel
 
  @as_scatterheader.macro
 
  self.excelObj.SetProperty, VISIBLE = 1
  
END

PRO AS_TalkToExcel::Cleanup

  @as_scatterheader.macro

  Obj_Destroy, self.book
  Obj_Destroy, self.activeSheet
  
  self.excelObj->Quit

END

PRO AS_TalkToExcel__Define

  void = {as_talktoexcel, $
             excelObj : Obj_New(), $
                 book : Obj_New(), $
          activeSheet : Obj_New()  $
         }
END