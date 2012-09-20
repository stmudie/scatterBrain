FUNCTION drawButtonObj_Get_Obj, id

  @as_scatterheader.macro

  Widget_Control, id, GET_UVALUE = drawButtonObj
  drawWidget = Widget_Info(id, /CHILD)
  WIDGET_CONTROL, drawWidget, GET_UVALUE=drawButtonObj
  RETURN, drawButtonObj
  
END

PRO drawButtonObj_Set_Text, id, text

  @as_scatterheader.macro

  Widget_Control, id, GET_UVALUE = drawButtonObj
  drawWidget = Widget_Info(id, /CHILD)
  WIDGET_CONTROL, drawWidget, GET_UVALUE=drawButtonObj
  drawButtonObj.ChangeText, text

END

PRO drawButtonObj_kill, drawID

  @as_scatterheader.macro

  Widget_Control, drawID, GET_UVALUE=drawButtonObj
  Obj_Destroy, drawButtonObj

END

FUNCTION drawButtonObj_event, event
  
  @as_scatterheader.macro
    
  Widget_Control, event.id, GET_UVALUE = drawButtonObj
  event = drawButtonObj.event(event)
  RETURN, event

END

FUNCTION drawButtonObj::Event, event

  @as_scatterheader.macro

  IF (~self.toggle OR event.press OR event.type EQ -1) THEN BEGIN
    self.pressed = ~self.pressed
    self.imageOutObj->SetProperty, HIDE = self.pressed
    IF Obj_Valid(self.oText) THEN self.oText->SetProperty, VERTICAL_ALIGN=0.45 + self.pressed*.1
  ENDIF
  IF event.type EQ 1 AND event.press THEN BEGIN
    self.imageOutObj->SetProperty, HIDE = 0
    self.pressed = 0
    IF Obj_Valid(self.oText) THEN self.oText->SetProperty, VERTICAL_ALIGN=0.45
  ENDIF
  
  self.drawButtonWindow->Draw, self.oView
  
  IF event.type EQ 0 THEN RETURN, { DRAWBUTTON_EVENT, ID:self.drawButtonTLB, TOP:event.top, HANDLER:0L, SELECT : 1} ELSE RETURN, 0

END

PRO drawButtonObj::SetToggle, toggle

  @as_scatterheader.macro

  self.toggle = toggle
  IF toggle EQ 0 THEN BEGIN
    self.pressed = 0
    self.imageOutObj->SetProperty, HIDE = self.pressed
    IF Obj_Valid(self.oText) THEN self.oText->SetProperty, VERTICAL_ALIGN=0.45 + self.pressed*.1
    self.drawButtonWindow->Draw, self.oView
  ENDIF

END

FUNCTION drawButtonObj::GetTLB

  @as_scatterheader.macro

  RETURN, self.drawButtonTLB

END

PRO drawButtonObj::ChangeText, text, TEXTLOCATION = textLocation, TEXTCOLOUR=textColour

  @as_scatterheader.macro

  IF Obj_Valid(self.oText) THEN self.oText->SetProperty, STRINGS=text ELSE BEGIN
    oFont = Obj_New('IDLgrFont', 'Helvetica*Bold')
    self.oText = Obj_New('IDLgrText', text, ALIGNMENT=0.5, VERTICAL_ALIGNMENT=0.45, CHAR_DIMENSIONS = [35,35], FONT=oFont)
    self.oModel->Add, self.oText
  ENDELSE
  IF KeyWord_Set(textLocation) THEN BEGIN
    self.drawButtonWindow.GetProperty, DIMENSIONS = dims
    textLocation = textLocation*[dims]
    self.oText.SetProperty, LOCATIONS = [textLocation, 0]
  ENDIF
  IF KeyWord_Set(textColour) THEN self.oText.SetProperty, COLOR=textColour
  self.drawButtonWindow->Draw, self.oView

END

PRO drawButtonObj::CleanUp

  @as_scatterheader.macro

  IF Obj_Valid(self.imageOutObj) THEN Obj_Destroy, self.imageOutObj
  IF Obj_Valid(self.oText)       THEN Obj_Destroy, self.oText
  IF Obj_Valid(self.oView)       THEN Obj_Destroy, self.oView

END

FUNCTION drawButtonObj::init, $
  Parent, $
  imageIn, $
  imageOut, $
  VALUE = value, $
  SCALE = scale, $
  UNAME = uname, $
  UVALUE = uvalue, $
  SENSITIVE = sensitive, $
  TOGGLE = toggle, $
  TEXTLOCATION = textLocation, $
  TEXTSIZE = textSize, $
  TEXTCOLOUR=textColour
  
  @as_scatterheader.macro

  IF NOT (KEYWORD_SET(uvalue)) THEN uvalue=0
  IF NOT (KEYWORD_SET(uname)) THEN uname='drawButtonObj'
  IF NOT (KEYWORD_SET(scale)) THEN scale=1
  IF NOT (KEYWORD_SET(toggle)) THEN toggle=0
    
  wButtonBase = Widget_Base(Parent, UNAME = uname, SENSITIVE = sensitive, FUNC_GET_VALUE='DRAWBUTTONOBJ_GET_OBJ', PRO_SET_VALUE='drawButtonObj_Set_Text', UVALUE = uvalue)
  wButtonDraw = Widget_Draw(wButtonBase, GRAPHICS_LEVEL = 2, /BUTTON_EVENTS, RETAIN = 2, EVENT_FUNC = 'drawButtonObj_event', KILL_NOTIFY='drawButtonObj_kill', UVALUE=self)

  Widget_Control, wButtonBase, /REALIZE

  imageInObj = Obj_New('IDLgrImage',imageIn, blend_function=[3,4])
  imageOutObj = Obj_New('IDLgrImage',imageOut, blend_function=[3,4])

  Widget_Control, wButtonDraw, GET_VALUE = drawButtonWindow, XSIZE = scale * N_Elements(imageIn[0,*,0]), YSIZE = scale * N_Elements(imageIn[0,0,*])

  oModel = Obj_New('IDLgrModel')
  oView  = Obj_New('IDLgrView', VIEWPLANE_RECT = [0,0,N_Elements(imageIn[0,*,0]),N_Elements(imageIn[0,0,*])])

  oModel->Add, imageInObj
  oModel->Add, imageOutObj

  IF N_Elements(textLocation) NE 2 THEN textLocation = [0,0]
  IF N_Elements(textColour) EQ 0 THEN textColour = 0

  textLocation = textLocation*[N_Elements(imageIn[0,*,0]),N_Elements(imageIn[0,0,*])]

  oFont = Obj_New('IDLgrFont', 'Helvetica*Bold')

  IF KEYWORD_SET(value) THEN BEGIN
    oText = Obj_New('IDLgrText', value, LOCATIONS = [textLocation, 0],ALIGNMENT=0.5, VERTICAL_ALIGNMENT=0.45, COLOR=textColour, CHAR_DIMENSIONS = [35,35], FONT=oFont)
    oModel->Add, oText
  ENDIF ELSE oText = Obj_New()

  
  oView->Add, oModel

  drawButtonWindow->Draw, oView

  self.drawButtonTLB = wButtonBase
  self.drawButtonWindow = drawButtonWindow 
  self.oView = oView
  self.oText = oText
  self.oModel = oModel
  self.imageOutObj = imageOutObj
  self.toggle = toggle
    
  RETURN, 1
  
END

PRO drawButtonObj__Define

  void = {drawButtonObj, $
           drawButtonTLB    : 0L, $
           drawButtonWindow : Obj_New(), $
           oView            : Obj_New(), $
           oText            : Obj_New(), $
           oModel           : Obj_New(), $
           imageOutObj      : Obj_New(), $
           toggle           : 0, $
           pressed          : 0 }

END

FUNCTION drawButton, $

  Parent, $ 
  imageIn, $
  imageOut, $
  VALUE = value, $
  SCALE = scale, $
  UNAME = uname, $
  UVALUE = uvalue, $
  SENSITIVE = sensitive, $
  TOGGLE = toggle, $ 
  TEXTLOCATION = textLocation, $
  TEXTSIZE = textSize, $
  TEXTCOLOUR=textColour, $
  OBJECT = object
  
  @as_scatterheader.macro

  object = obj_new("drawButtonObj", $
  Parent, $ 
  imageIn, $
  imageOut, $
  VALUE = value, $
  SCALE = scale, $
  UNAME = uname, $
  UVALUE = uvalue, $
  SENSITIVE = sensitive, $
  TOGGLE = toggle, $ 
  TEXTLOCATION = textLocation, $
  TEXTSIZE = textSize, $
  TEXTCOLOUR=textColour)
  
  IF Obj_Valid(object) THEN RETURN, object.GetTLB() ELSE RETURN, -1L
  
END