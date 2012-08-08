PRO AS_editOPalette_Event, event

Widget_Control, event.top, GET_UVALUE = info, /NO_COPY

Widget_Name = Widget_Info(event.id, /UNAME) ; Get UNAME of Widget that generated the event.

Widget_Type = Tag_Names(event,/STRUCTURE_NAME)

IF Widget_Type EQ 'WIDGET_SLIDER' THEN BEGIN
  
  info.customPalette->GetProperty, N_COLORS = numColours
  info.index = info.index < (numColours - 1)
  rgb = info.customPalette->GetRGB(info.index)
  
  CASE Widget_Name OF
    'Red Slider'  :  BEGIN
                   
                     info.customPalette->SetRGB, info.index, event.value, rgb[1], rgb[2]
                     imageBarRed = Transpose([[[Indgen(256)#Replicate(1,10)]],[[Replicate(rgb[1],256,10)]],[[Replicate(rgb[2],256,10)]]],[2,0,1])
                     imageBarGreen = Transpose([[[Replicate(event.value,256,10)]],[[Indgen(256)#Replicate(1,10)]],[[Replicate(rgb[2],256,10)]]],[2,0,1])
                     imageBarBlue = Transpose([[[Replicate(event.value,256,10)]],[[Replicate(rgb[1],256,10)]],[[Indgen(256)#Replicate(1,10)]]],[2,0,1])
               END
    'Green Slider'  :  BEGIN
                   
                     info.customPalette->SetRGB, info.index, rgb[0], event.value, rgb[2]
                     imageBarRed = Transpose([[[Indgen(256)#Replicate(1,10)]],[[Replicate(event.value,256,10)]],[[Replicate(rgb[2],256,10)]]],[2,0,1])
                     imageBarGreen = Transpose([[[Replicate(rgb[0],256,10)]],[[Indgen(256)#Replicate(1,10)]],[[Replicate(rgb[2],256,10)]]],[2,0,1])
                     imageBarBlue = Transpose([[[Replicate(rgb[0],256,10)]],[[Replicate(event.value,256,10)]],[[Indgen(256)#Replicate(1,10)]]],[2,0,1])
               END
    'Blue Slider'  :  BEGIN
                   
                     info.customPalette->SetRGB, info.index, rgb[0], rgb[1], event.value
                     imageBarRed = Transpose([[[Indgen(256)#Replicate(1,10)]],[[Replicate(rgb[1],256,10)]],[[Replicate(event.value,256,10)]]],[2,0,1])
                     imageBarGreen = Transpose([[[Replicate(rgb[0],256,10)]],[[Indgen(256)#Replicate(1,10)]],[[Replicate(event.value,256,10)]]],[2,0,1])
                     imageBarBlue = Transpose([[[Replicate(rgb[0],256,10)]],[[Replicate(rgb[1],256,10)]],[[Indgen(256)#Replicate(1,10)]]],[2,0,1])
               END
    
    ENDCASE
    
    info.oRedBar->SetProperty, DATA = imageBarRed
    info.oGreenBar->SetProperty, DATA = imageBarGreen
    info.oBlueBar->SetProperty, DATA = imageBarBlue
                     
    info.oDRWin->Draw, info.oRView
    info.oDGWin->Draw, info.oGView
    info.oDBWin->Draw, info.oBView
    info.oCWin->Draw, info.oCView
    IF Obj_Valid(info.oCPWin) THEN info.oCPWin->Draw, info.oCPView
    info.oWin->Draw, info.oView
   
ENDIF

IF Widget_Type EQ 'WIDGET_COMBOBOX' THEN BEGIN 
  CASE Widget_Name OF
    'Select Table' : BEGIN
              
       IF event.index NE 0 THEN BEGIN
          IF info.customTables NE '' THEN info.oPalette->LoadCT, event.index-1, FILE = info.customTables ELSE info.oPalette->LoadCT, event.index-1
       ENDIF
       info.oWin->Draw, info.oView
    
    END
    'Palette Size' : BEGIN
       info.customPalette->GetProperty, RED_VALUES = red, GREEN_VALUES = green, BLUE_VALUES = blue, N_COLORS = oldSize
       newSize = (event.index + 1)^2
       
       IF oldSize GE newSize THEN BEGIN
          newRed = red[0:newSize-1]
          newGreen = green[0:newSize-1]
          newBlue = blue[0:newSize-1]
          IF info.index GT newSize THEN info.index = newSize - 1
       ENDIF   
       IF oldSize LT newSize THEN BEGIN
          newRed = IntArr(newSize)
          newRed[0:oldSize-1] = red 
          newGreen = IntArr(newSize)
          newGreen[0:oldSize-1] = green
          newBlue = IntArr(newSize)
          newBlue[0:oldSize-1] = blue
       ENDIF
       info.customPalette->SetProperty, RED_VALUES = newRed, GREEN_VALUES = newGreen, BLUE_VALUES = newBlue
       
       custImage = IndGen(newSize)
       custImage = Reverse(Reform(custImage,SqRt(newSize),SqRt(newSize)),2)
       info.oCustImage->SetProperty, Data = custImage
       info.oCPView->SetProperty, VIEWPLANE_RECT = [0,0,SqRt(newSize),Sqrt(newSize)]
       info.oCPWin->Draw, info.oCPView
    
    END
  ENDCASE
ENDIF

IF Widget_Type EQ 'WIDGET_BUTTON' THEN BEGIN
  CASE Widget_Name OF 
    'Load Table'      : BEGIN
          info.oPalette->GetProperty, RED_VALUES = red, GREEN_VALUES = green, BLUE_VALUES = blue
          info.customPalette->GetProperty, N_COLORS = custColors
          IF N_Elements(red) EQ custColors THEN BEGIN
            info.customPalette->SetProperty, RED_VALUES = red, GREEN_VALUES = green, BLUE_VALUES = blue
          ENDIF
          IF N_Elements(red) GT custColors THEN BEGIN
            indices = info.preindex + IndGen((info.preindex + custColors < 256) - info.preindex) 
            IF N_Elements(indices) LT custColors THEN BEGIN 
              indices2 = IndGen(custColors - N_Elements(indices))
              indices = [indices,indices2] 
            ENDIF
            info.customPalette->SetProperty, RED_VALUES = red[indices], GREEN_VALUES = green[indices], BLUE_VALUES = blue[indices]
          ENDIF
          AS_editOPalette_UpdateGUI, info
          
      
      END
    'Save Palette'    : BEGIN
          IF info.customTables EQ '' THEN BEGIN
              info.customTables = Dialog_Pickfile(TITLE = 'No custom colour table file specified, please select...', /MUST_EXIST)
              IF info.customTables EQ '' THEN BREAK
          ENDIF
          
          flag = 0
          
          WHILE flag EQ 0 DO BEGIN
            ctName = textbox(TITLE='Table Name', LABEL='Table Name...', GROUP_LEADER = event.top)
            LoadCT, GET_NAMES = tableNames, FILE = info.customTables
            matchingTable = Where(STRUPCASE(ctName) EQ STRUPCASE(tableNames))
            IF N_Elements(matchingTable) GT 1 THEN matchingTable = matchingTable[N_Elements(matchingTable)-1]
            writeTable = 'YES'
            IF matchingTable GE 0 THEN writeTable = Dialog_Message('Overwrite existing colour table?',/QUESTION) ELSE flag = 1
            IF writeTable EQ 'YES' THEN flag = 1
          ENDWHILE
          
          IF ctName EQ '' THEN BREAK 
           
          info.customPalette->GetProperty, RED_VALUES = red, GREEN_VALUES=green, BLUE_VALUES = blue
          IF N_Elements(red) LT 256 THEN BEGIN
              temp = red
              red = IntArr(256)
              red[0:N_Elements(temp)-1] = temp
              temp = green
              green = IntArr(256)
              green[0:N_Elements(temp)-1] = temp
              temp = blue
              blue = IntArr(256)
              blue[0:N_Elements(temp)-1] = temp
          ENDIF
          IF matchingTable LT 0 THEN matchingTable = 255
          IF matchingTable EQ 255 THEN Widget_Control, info.wSelTable, COMBOBOX_ADDITEM = ctName
          ModifyCT, matchingTable, ctName, red, green,blue, FILE = info.customTables
          
    END
    
  ENDCASE
ENDIF 
Widget_Control, event.top, SET_UVALUE = info, /NO_COPY

END

PRO AS_editOPalette_UpdateGUI, info

   info.customPalette->GetProperty, N_COLORS = numColours
   rgb = info.customPalette->GetRGB(info.index < (numColours - 1))
   
   Widget_Control, info.wRedSlider, SET_VALUE = rgb[0]
   Widget_Control, info.wGreenSlider, SET_VALUE = rgb[1]
   Widget_Control, info.wBlueSlider, SET_VALUE = rgb[2]
    
   imageBarRed = Transpose([[[Indgen(256)#Replicate(1,10)]],[[Replicate(rgb[1],256,10)]],[[Replicate(rgb[2],256,10)]]],[2,0,1])
   imageBarGreen = Transpose([[[Replicate(rgb[0],256,10)]],[[Indgen(256)#Replicate(1,10)]],[[Replicate(rgb[2],256,10)]]],[2,0,1])
   imageBarBlue = Transpose([[[Replicate(rgb[0],256,10)]],[[Replicate(rgb[1],256,10)]],[[Indgen(256)#Replicate(1,10)]]],[2,0,1])
   info.oRedBar->SetProperty, DATA = imageBarRed
   info.oGreenBar->SetProperty, DATA = imageBarGreen
   info.oBlueBar->SetProperty, DATA = imageBarBlue
                     
   info.oDRWin->Draw, info.oRView
   info.oDGWin->Draw, info.oGView
   info.oDBWin->Draw, info.oBView
   info.oCWin->Draw, info.oCView
   IF Obj_Valid(info.oCPWin) THEN info.oCPWin->Draw, info.oCPView
   info.oWin->Draw, info.oView

END

PRO AS_editOPalette_CustomPal_Events, event

Widget_Control, event.top, GET_UVALUE = info, /NO_COPY

IF event.Press EQ 1 THEN BEGIN
   geom = Widget_Info(event.ID, /GEOMETRY)
   info.customPalette->GetProperty, N_COLORS = colours
   xpos = Fix(SqRt(colours)*event.x/geom.xsize)
   ypos = (SqRt(colours)-1) - Fix(SqRt(colours)*event.y/geom.ysize)
   info.index = xpos+ypos*SqRt(colours)
   imageColour = intarr(2,2)
   imageColour[*,*] = info.index
   info.oIColour->SetProperty,data = imageColour
   info.oCWin->Draw, info.oCView
   info.oCPWin->Draw, info.oCPView 
   AS_editOPalette_UpdateGUI, info
ENDIF


Widget_Control, event.top, SET_UVALUE = info, /NO_COPY

END

PRO AS_editOPalette_Draw_Events, event

Widget_Control, event.top, GET_UVALUE = info, /NO_COPY

IF event.Press EQ 1 THEN BEGIN
   geom = Widget_Info(event.ID, /GEOMETRY)
   info.oPalette->GetProperty, N_COLORS = colours
   xpos = Fix(SqRt(colours)*event.x/geom.xsize)
   ypos = (SqRt(colours)-1) - Fix(SqRt(colours)*event.y/geom.ysize)
   info.preindex = xpos+ypos*SqRt(colours)
   imageColour = intarr(2,2)
   imageColour[*,*] = info.preindex
   rgb = info.oPalette->GetRGB(info.preindex)
   info.customPalette->SetRGB, info.index, rgb[0], rgb[1], rgb[2] 
   info.oCWin->Draw, info.oCView
   IF Obj_Valid(info.oCPWin) THEN info.oCPWin->Draw, info.oCPView
   AS_editOPalette_UpdateGUI, info
ENDIF

Widget_Control, event.top, SET_UVALUE = info, /NO_COPY


END

PRO AS_editOPalette_Done, event
  WIDGET_CONTROL, Event.top, /DESTROY
END

PRO AS_editOPalette_Cleanup, baseID

Widget_Control, baseID, GET_UVALUE = info, /NO_COPY
  
  IF Obj_Valid(info.oPalette) THEN Obj_Destroy, info.oPalette
  IF info.hideCustom EQ 0 AND ~Obj_Valid(info.oCPView) AND Obj_Valid(info.customPalette) THEN Obj_Destroy, info.customPalette 
       
  IF Obj_Valid(info.oRView) THEN Obj_Destroy, info.oRView
  IF Obj_Valid(info.oGView) THEN Obj_Destroy, info.oGView
  IF Obj_Valid(info.oBView) THEN Obj_Destroy, info.oBView
   
  IF Obj_Valid(info.oCView) THEN Obj_Destroy, info.oCView
  IF Obj_Valid(info.oCPView) THEN Obj_Destroy, info.oCPView
  IF Obj_Valid(info.oView) THEN Obj_Destroy, info.oView
  
END  
  

PRO AS_editOPalette, CUSTOMPALETTE=customPalette, HIDECUSTOM = hideCustom, INDEX = index, NEWRGB = newRGB, CUSTOMTABLES = customTables, TABLEINDEX = tableIndex, MODAL = modal, GROUPLEADER = groupleader

IF !d.name EQ 'X' THEN colorModel = 0 ELSE colorModel = 1

IF KeyWord_Set(modal) AND KeyWord_Set(groupleader) THEN wBase = Widget_Base(/ROW, /MODAL, GROUP_LEADER = groupleader) $
                                                   ELSE wBase = Widget_Base(/ROW, GROUP_LEADER = groupleader)

IF ~KeyWord_Set(index) THEN index = 0
IF ~KeyWord_Set(hideCustom) THEN hideCustom = 0
IF Size(tableIndex,/TNAME) NE 'INT' THEN tableIndex = 39

wBaseLeft = Widget_Base(wBase, /COLUMN, /BASE_ALIGN_CENTER)
wBaseCenter= Widget_Base(wBase, /COLUMN)
wBaseCenterRight = Widget_Base(wBase,/ROW, /BASE_ALIGN_CENTER, /ALIGN_CENTER)
wBaseRight = Widget_Base(wBase, /COLUMN)
wColour = Widget_Draw(wBaseLeft, XSIZE = 100, YSIZE = 80, GRAPHICS_LEVEL = 2, COLOR_MODEL = colorModel)

oPalette = Obj_New('IDLgrPalette')

LOADCT, GET_NAMES = paletteNames
custTable = 0
IF KeyWord_Set(customTables) THEN BEGIN
  IF Size(customTables, /TNAME) EQ 'STRING' THEN BEGIN 
    IF File_Test(customTables) THEN BEGIN 
      LOADCT, FILE = customTables, GET_NAMES = customPaletteNames
      wSelTable = Widget_ComboBox(wBaseRight, VALUE = ['Load Predefined...', customPaletteNames], UNAME = 'Select Table')
      oPalette->LoadCT, tableIndex < N_Elements(customPaletteNames), FILE = customTables
      custTable = 1
    ENDIF      
  ENDIF
ENDIF
 
IF ~custTable THEN BEGIN
    wSelTable = Widget_ComboBox(wBaseRight, VALUE = ['Load Predefined...', paletteNames], UNAME = 'Select Table')
    customTables = ''
    oPalette->LoadCT, tableIndex < 40
ENDIF 
wDraw = Widget_Draw(wBaseRight, XSIZE = 200, YSIZE = 200, GRAPHICS_LEVEL = 2, COLOR_MODEL = colorModel, /BUTTON_EVENTS, EVENT_PRO = 'AS_editOPalette_Draw_Events')

wRedSlider = Widget_Slider(wBaseLeft, MAXIMUM = 255, XSIZE = 200, UNAME = 'Red Slider', /DRAG)
wDrawRed = Widget_Draw(wBaseLeft, XSIZE = 167, YSIZE = 10, GRAPHICS_LEVEL = 2)

wGreenSlider = Widget_Slider(wBaseLeft, MAXIMUM = 255, XSIZE = 200, UNAME = 'Green Slider', /DRAG)
wDrawGreen = Widget_Draw(wBaseLeft, XSIZE = 167, YSIZE = 10, GRAPHICS_LEVEL = 2)

wBlueSlider = Widget_Slider(wBaseLeft, MAXIMUM = 255, XSIZE = 200, UNAME = 'Blue Slider', /DRAG)
wDrawBlue = Widget_Draw(wBaseLeft, XSIZE = 167, YSIZE = 10, GRAPHICS_LEVEL = 2)

wDoneBut = Widget_Button(wBaseLeft, VALUE = 'DONE', EVENT_PRO = 'AS_editOPalette_Done')

cust = 0
custColours = 1
oCustImage = 0

IF Obj_Valid(customPalette) THEN BEGIN
  IF Obj_Class(customPalette) EQ 'IDLGRPALETTE' THEN BEGIN
    customPalette->GetProperty, N_COLORS = custColours
    IF custColours LT 1 THEN BEGIN
      customPalette->SetProperty, RED_VALUES = [0], GREEN_VALUES = [0], BLUE_VALUES = [0]
      custColours = 1
    ENDIF
    
    IF hideCustom EQ 0 THEN BEGIN
      cust = 1
      wComboBase = Widget_Base(wBaseCenter, /ROW)
      wSizeLable = Widget_Label(wComboBase, VALUE = 'Choose Palette Size...   ')
      wPalSize = Widget_ComboBox(wComboBase, VALUE = ['1','4','9','16','25','36','49','64','81','100','121','144','169','196','225','256'], UNAME = 'Palette Size') 
      Widget_Control, wPalSize, SET_COMBOBOX_SELECT = Sqrt(custColours)-1
      wCustomPal = Widget_Draw(wBaseCenter, XSIZE = 200, YSIZE = 200, GRAPHICS_LEVEL = 2, COLOR_MODEL = colorModel, /BUTTON_EVENTS, EVENT_PRO = 'AS_editOPalette_CustomPal_Events')
      wSavePal = Widget_Button(wBaseCenter, VALUE = 'Save Palette', UNAME = 'Save Palette')
      wLoadTable = Widget_Button(wBaseCenterRight, VALUE = 'arrow.bmp', /BITMAP, UNAME = 'Load Table')
    ENDIF
  ENDIF
ENDIF ELSE BEGIN
  
  oPalette.GetProperty, RED_VALUES = R, GREEN_VALUES = G, BLUE_VALUES = B
  customPalette = Obj_New('IDLgrPalette', R, G, B)
  
ENDELSE

Widget_Control, wBase, /REALIZE
Widget_Control, wDraw, GET_VALUE = oWin
Widget_Control, wColour, GET_VALUE = oCWin
Widget_Control, wDrawRed, GET_VALUE = oDRWin
Widget_Control, wDrawGreen, GET_VALUE = oDGWin
Widget_Control, wDrawBlue, GET_VALUE = oDBWin


IF cust THEN BEGIN
  Widget_Control, wCustomPal, GET_VALUE = oCPWin 
  oCPWin->SetProperty,  PALETTE = customPalette
  custImage = IndGen(custColours)
  custImage = Reverse(Reform(custImage,SqRt(custColours),SqRt(custColours)),2)
  oCustImage = Obj_New('IDLgrImage', custImage)

ENDIF ELSE oCPWin = 0

oWin->SetProperty,  PALETTE = oPalette
oCWin->SetProperty,  PALETTE = customPalette

oPalette->GetProperty, N_COLORS = preColours

preImage = IndGen(preColours)
preImage = Reverse(reform(preImage,SqRt(preColours),SqRt(preColours)),2)
oPreImage = Obj_New('IDLgrImage', preImage)

imageColour = intarr(2,2)
imageColour[*,*] = index

oIColour = Obj_New('IDLgrImage', imageColour)

imageBars = intarr(256,10)

oRedBar = Obj_New('IDLgrImage', imageBars)
oGreenBar = Obj_New('IDLgrImage', imageBars)
oBlueBar = Obj_New('IDLgrImage', imageBars)

;oGraph = Obj_New('IDLgrPlot', Findgen(100), Findgen(100)^2)

oModel = Obj_New('IDLgrModel')
oView = Obj_New('IDLgrView', VIEWPLANE_RECT = [0,0,SqRt(preColours),SqRt(preColours)])

oCModel = Obj_New('IDLgrModel')
oCView = Obj_New('IDLgrView', VIEWPLANE_RECT = [0,0,2,2])

oRModel = Obj_New('IDLgrModel')
oRView = Obj_New('IDLgrView', VIEWPLANE_RECT = [0,0,256,10])

oBModel = Obj_New('IDLgrModel')
oBView = Obj_New('IDLgrView', VIEWPLANE_RECT = [0,0,256,10])

oGModel = Obj_New('IDLgrModel')
oGView = Obj_New('IDLgrView', VIEWPLANE_RECT = [0,0,256,10]) 
 
oModel->Add, oPreImage;oPolyLine
oView->Add, oModel
oWin->Draw, oView

oCModel->Add, oIColour
oCView->Add, oCModel
oCWin->Draw, oCView

oRModel->Add, oRedBar
oRView->Add, oRModel
oDRWin->Draw, oRView

oGModel->Add, oGreenBar
oGView->Add, oGModel
oDGWin->Draw, oGView

oBModel->Add, oBlueBar
oBView->Add, oBModel
oDBWin->Draw, oBView

oCPView = 0
IF cust THEN BEGIN
  oCPModel = Obj_New('IDLgrModel')
  oCPView = Obj_New('IDLgrView', VIEWPLANE_RECT = [0,0,SqRt(custColours),SqRt(custColours)])
  
  oCPView->Add, oCPModel
  oCPModel->Add, oCustImage
  oCPWin->Draw, oCPView
ENDIF

preindex = 0
IF ~KeyWord_Set(newRGB) THEN newRGB = 0 

info = {  oCWin : oCWin, $
        oPalette: oPalette, $ 
          index : index, $
         oCPWin : oCPWin, $
        oCPView : oCPView, $
  customPalette : customPalette, $
     oCustImage : oCustImage, $
       preindex : preindex, $
        oIColour: oIColour, $
         oCView : oCView, $
         oWin   : oWin, $
         oView  : oView, $
     wRedSlider : wRedSlider, $
         oRedBar: oRedBar, $
         oRView : oRView, $
         oDRWin : oDRWin, $
   wGreenSlider : wGreenSlider, $
       oGreenBar: oGreenBar, $
         oGView : oGView, $
         oDGWin : oDGWin, $
    wBlueSlider : wBlueSlider, $
        oBlueBar: oBlueBar, $
         oBView : oBView, $
         oDBWin : oDBWin, $
         newRGB : newRGB, $
   customTables : customTables, $
      wSelTable : wSelTable, $ 
     hideCustom : hideCustom}

AS_editOPalette_UpdateGUI, info

Widget_Control, wBase, SET_UVALUE = info, /NO_COPY

XManager, 'AS_editOPalette', wBase, /NO_BLOCK, EVENT_HANDLER = 'AS_editOPalette_Event', CLEANUP = 'AS_editOPalette_Cleanup'

END