PRO as_newexperimentfromtemplate_event, event

  @as_scatterheader.macro

  Widget_Control, event.top, GET_UVALUE = fromTemplateObj
  fromTemplateObj.event, event

END

FUNCTION as_newexperimentfromtemplate::init, templateList, NOTIFYOBJ = notifyObj, PATH = path

  @as_scatterheader.macro

  IF Keyword_Set(notifyObj) THEN $
    IF TypeName(notifyObj[0]) EQ 'NOTIFY' $
      THEN self.notifyObj = List(notifyObj,/EXTRACT)
  
  IF ~ISA(templateList, 'STRING') THEN RETURN, -1
  IF ISA(path, 'STRING') THEN BEGIN
    FOREACH p, path, key DO BEGIN
      tempPath = ''
      WHILE tempPath EQ '' DO BEGIN
        IF File_Test(p, /DIRECTORY) THEN tempPath = p ELSE IF File_Dirname(p) EQ p THEN CD, CURRENT = tempPath ELSE p = File_Dirname(p)
      ENDWHILE
      self.path[key]=tempPath
    ENDFOREACH
  ENDIF
  
  self.templateList = List(templateList, /EXTRACT)
    
  self.wBase = Widget_Base(/COLUMN, /TLB_KILL_REQUEST_EVENTS)
   
  result = self.IDLitComponent::init()

  self.RegisterProperty, 'saxswaxs', 1, NAME = 'Two Detectors'
  self.RegisterProperty, 'path', 0, NAME = 'User Path', USERDEF = self.path[0]
  self.RegisterProperty, 'path2', 0, NAME = 'User Path 2', USERDEF = self.path[1], /HIDE
  self.RegisterProperty, 'ExperimentName', 4, NAME = 'Experiment Name'
  self.RegisterProperty, 'templateNo', 9, NAME = 'Template', ENUMLIST = templateList
  
  self.SetPropertyAttribute, 'NAME', /HIDE
  self.SetPropertyAttribute, 'DESCRIPTION', /HIDE
  self.SetProperty, NAME = 'New Experiment'

  wPropertyWidget = Widget_PropertySheet(self.wBase, VALUE = self, XSIZE = 100, UNAME = 'newExpPropSheet')

  createButton = Widget_Button(self.wBase, VALUE = 'Create', UNAME = 'CREATE')
  cancelButton = Widget_Button(self.wBase, VALUE = 'Cancel', UNAME = 'CANCEL')

  Widget_Control, self.wBase, /REALIZE
  
  Widget_Control, self.wBase, SET_UVALUE = self
  
  XManager, 'as_newexperimentfromtemplate', self.wbase, /NO_BLOCK
  
  RETURN, 1

END


PRO as_newexperimentfromtemplate::event, event

  @as_scatterheader.macro

  widget_type = Tag_Names(event, /STRUCTURE)

  IF widget_type EQ 'TLB_KILL_REQUEST_EVENTS' OR widget_type EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
    Obj_Destroy, self
    RETURN
  ENDIF 
  
  Widget_Name = Widget_Info(event.id, /UNAME)
  
  CASE Widget_Name OF
    'newExpPropSheet' : BEGIN
                          IF event.type EQ 0 THEN BEGIN
                            IF event.proptype NE 0 THEN BEGIN
                              ; Get the value of the property identified by event.identifier.
                              value = WIDGET_INFO(event.ID, COMPONENT = event.component, PROPERTY_VALUE = event.identifier)
                              
                            ENDIF ELSE BEGIN
                              
                              CASE event.identifier OF
                                'PATH' : BEGIN
                                          value = Dialog_Pickfile(PATH = self.path[0], /DIRECTORY)
                                          IF value EQ '' THEN value = self.path[0] ELSE self.path[0] = value
                                         END
                                'PATH2' : BEGIN
                                           value = Dialog_Pickfile(PATH = self.path[1], /DIRECTORY)
                                           IF value EQ '' THEN value = self.path[1] ELSE self.path[1] = value
                                          END
                              ENDCASE
                              
                              
                              ; Set the component’s attribute value.
                              event.component->SetPropertyAttribute, event.identifier, USERDEF = value
                                                                                      
                            ENDELSE
                            
                            ; Set the component’s property value.
                            event.component->SetPropertyByIdentifier, event.identifier, value
                            
                          ENDIF
                          
                          Widget_Control, event.id, /REFRESH_PROPERTY
                        
                        END
    'CREATE'  :  BEGIN
                   propertySheetID = Widget_Info(event.top, FIND_BY_UNAME='newExpPropSheet')
                   experimentName = WIDGET_INFO(propertySheetID, PROPERTY_VALUE = 'ExperimentName')
                   self.GetProperty, templateStr = templateStr
                   self.notify, {TEMPLATE, template : templateStr, experimentName : experimentName, path : self.path, saxswaxs : self.saxswaxs}
                   Obj_Destroy, self
                 END
    'CANCEL':  Obj_Destroy, self
  ENDCASE
  
END

PRO as_newexperimentfromtemplate::SetProperty, $
  EXPERIMENTNAME = experimentName, $
  TEMPLATENO = templateNo, $
  SAXSWAXS = saxswaxs, $
  _REF_EXTRA = extra

  @as_scatterheader.macro

  IF N_Elements(saxswaxs) EQ 1 THEN BEGIN
    self.saxswaxs = saxswaxs 
    self.SetPropertyAttribute, 'path2', HIDE = ~saxswaxs
    Widget_Control, Widget_Info(self.wBase, FIND_BY_UNAME = 'newExpPropSheet'), YSIZE = 5
  ENDIF

  IF ISA(experimentName, 'STRING') THEN self.experimentName = experimentName
  IF ISA(templateNo, 'LONG') THEN self.templateNo = templateNo

  self.IDLitComponent::SetProperty, _EXTRA = extra

END

PRO as_newexperimentfromtemplate::GetProperty, $
  EXPERIMENTNAME = experimentName, $
  SAXSWAXS = saxswaxs, $
  TEMPLATENO = templateNo, $
  TEMPLATESTR = templateStr, $
  _REF_EXTRA = extra

  @as_scatterheader.macro
  
  IF Arg_Present(experimentName) THEN experimentName = self.experimentName
  IF Arg_Present(saxswaxs) THEN saxswaxs = self.saxswaxs
  IF Arg_Present(templateNo) THEN templateNo = self.templateNo
  IF Arg_Present(templateStr) THEN BEGIN
    templateStr = (self.templateList.toarray())[self.templateNo]
  ENDIF 
  self.IDLitComponent::GetProperty, _EXTRA = extra

END

PRO as_newexperimentfromtemplate::notify, event

  @as_scatterheader.macro

  FOREACH notify, self.notifyobj DO IF Obj_Valid(notify) THEN notify.notify, event

END

PRO as_newexperimentfromtemplate::cleanup
  
  @as_scatterheader.macro

  self.IDLitComponent::Cleanup
  IF Widget_Info(self.wbase, /VALID) THEN Widget_Control, self.wbase, /DESTROY
  
END

PRO as_newexperimentfromtemplate__define

void = {as_newexperimentfromtemplate, $
        INHERITS IDL_Object, $
        INHERITS IDLitcomponent, $
        wBase : 0L, $
        templateList : List(), $
        templateNo: 0, $
        path : ['',''], $
        saxswaxs : 0, $
        experimentName : '', $
        notifyObj : List() }

END