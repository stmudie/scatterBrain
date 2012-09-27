PRO as_exportstruct_event, event
  
  @as_scatterheader.macro
  
  Widget_Control, event.top, GET_UVALUE = as_exportstruct
  as_exportstruct.event, event

END

PRO as_exportstruct::event, event

  @as_scatterheader.macro

  widgetName = Widget_Info(event.id, /UNAME)
  CASE widgetName OF
  
    'EXPORT' : BEGIN
                 typeID = Widget_Info(self.wBase, FIND_BY_UNAME = 'EXPORT TYPE')
                 type = Widget_Info(typeID, /COMBOBOX_GETTEXT)
  
                 CASE type OF 
                   'Direct to Excel (Windows Only)' : type = 'Excel'
                   'CSV File' : type = 'CSV'
                 ENDCASE
                 
                 self.export, type
    
               END
    'NAMES' : BEGIN
               (*self.outTags)[event.value] = event.select 
              END
    'SELECT ALL' : BEGIN
                     nameButtons = Widget_Info(self.wBase, FIND_BY_UNAME = 'NAMES')
                     Widget_Control, nameButtons, SET_VALUE = Replicate(1, N_Tags(*self.struct))
                     (*self.outTags)[*] = 1
                   END
    'DESELECT ALL' : BEGIN
                       nameButtons = Widget_Info(self.wBase, FIND_BY_UNAME = 'NAMES')
                       Widget_Control, nameButtons, SET_VALUE = IntArr(N_Tags(*self.struct))
                       (*self.outTags)[*] = 0
                     END
    ELSE:
  ENDCASE

END

PRO as_exportstruct::export, type
  
  @as_scatterheader.macro
  
  tagPositions = Where(*self.outTags EQ 1)
  tagNames = (Tag_Names(*self.struct))
  
  outStruct = !Null
  FOREACH tagPos, tagPositions DO outStruct=Create_struct(outStruct, tagNames[tagPos],(*self.struct).(tagPos))
  
  type = StrUpCase(type)
  
  CASE type OF
    'EXCEL' :  outputObj = as_exportstructtoexcel()
    'CSV'   :  outputObj = as_exportstructtocsv()
    ELSE : BEGIN
             result = Dialog_Message('Invalid Export Type. Returning...')
             RETURN
           END
  ENDCASE

    outputObj.export, outStruct

END

FUNCTION as_exportstruct::init, structArray
  
  @as_scatterheader.macro

  numberTags = N_Tags(structArray)
  tagNames = Tag_Names(structArray)
  self.outTags = Ptr_New(Replicate(1, numberTags))
  
  
  self.struct = Ptr_New(structArray)
  
  wBase = Widget_Base(/COLUMN)
  
  wSelectAll = Widget_Button(wBase, VALUE = 'Select All', UNAME = 'SELECT ALL')
  wDeSelectAll = Widget_Button(wBase, VALUE = 'Deselect All', UNAME = 'DESELECT ALL')
  
  wTags = CW_BGROUP(wBase, tagNames, SET_VALUE = *self.outTags, COLUMN = (numberTags/10) + ((numberTags MOD 10) GT 0), /NONEXCLUSIVE, /RETURN_INDEX, UNAME = 'NAMES')
  
  wExportType = Widget_Combobox(wBase, VALUE = ['CSV File', 'Direct to Excel (Windows Only)'], UNAME = 'EXPORT TYPE')
  wExportButton = Widget_Button(wBase, VALUE = 'Export', UNAME = 'EXPORT')
  
  Widget_Control, wBase, /REALIZE
  Widget_Control, wBase, SET_UVALUE = self
  
  XManager, 'as_exportstruct', wBase, /NO_BLOCK
  
  self.wBase = wBase

  RETURN, 1
  
END

PRO as_exportstruct__define

  void = {as_exportstruct, $
          wBase : 0L, $,
          outTags : Ptr_New(), $
          struct : Ptr_New() }

END