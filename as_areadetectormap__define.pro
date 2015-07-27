PRO as_ADMap_PollEvent, event

  @as_scatterheader.macro

  Widget_Control, event.id, GET_UVALUE = as_ADMap
  IF Obj_Valid(as_ADMAP) THEN as_ADMap->event, event 

END

PRO as_ADMap_PropertyEvent, event

  @as_scatterheader.macro

  prop = Widget_Info(event.handler, FIND_BY_UNAME = 'PropSheet')
  Widget_Control, prop, GET_UVALUE = as_ADMap
  as_ADMap->event, event 

END

FUNCTION as_areadetectormap::init, detectorStruct, NOTIFYOBJ = notifyObj

  @as_scatterheader.macro

  IF Keyword_Set(notifyObj) THEN $
    IF TypeName(notifyObj[0]) EQ 'NOTIFY' $
      THEN self.notifyObj = List(notifyObj,/EXTRACT)

  detValid = 'YES'
  IF Size(detectorStruct,/TYPE) NE 8 THEN detValid = 'NO' ELSE BEGIN
    tags = Tag_Names(detectorStruct)
    testTags = ['BASE', 'CAM','IMAGE']
    FOR i = 0, N_Elements(testTags) - 1 DO BEGIN
      IF Where(Tags EQ testTags[i]) EQ -1 THEN detValid = 'NO'
    ENDFOR
  ENDELSE

  IF detValid EQ 'YES' THEN BEGIN
    self.areaDetectors = Ptr_New(Obj_New('as_areadetector', (detectorStruct.base)[0], (detectorStruct.cam)[0], (detectorStruct.image)[0]))

    FOR i = 1, N_Elements(detectorStruct) - 1 DO $  
      *self.areaDetectors = [*self.areaDetectors, Obj_New('as_areadetector', (detectorStruct.base)[i], (detectorStruct.cam)[i], (detectorStruct.image)[i])]
  ENDIF ELSE self.areaDetectors = Ptr_New(Obj_New('as_areadetector'))
  
  self.timerBase = Widget_Base(MAP=0, UNAME = 'TimerBase')
  Widget_Control, self.timerBase, /REALIZE
  Widget_Control, self.timerBase, SET_UVALUE = self
  XManager, 'as_areadetectormap', self.timerBase, EVENT_HANDLER = 'as_ADMap_PollEvent', /NO_BLOCK
  
  RETURN, 1

END

PRO as_areadetectormap::Event, event

  @as_scatterheader.macro

  widgetName = Widget_Info(event.id, /UNAME)

  CASE widgetName OF
   'TimerBase'     : BEGIN
                      IF N_Elements(*self.areaDetectors) EQ 0 THEN BREAK
                      ID = IntArr(N_Elements(*self.areaDetectors))
                      FOR i = 0, N_Elements(*self.areaDetectors) - 1 DO BEGIN
                        (*self.areaDetectors)[i]->GetProperty, AUTOLOAD = autoLoad
                        IF autoLoad THEN IF (*self.areaDetectors)[i]->NewArray() THEN ID[i] = 1  
                      ENDFOR
                      newDet = Where(ID EQ 1)
                      IF newDet[0] NE -1 THEN BEGIN
                        self.notify, ID
                      ENDIF
                      IF self.poll EQ 1 THEN Widget_Control, self.timerBase, TIMER = 0.1
                     END
   'PropSheet'     : BEGIN
                       IF (event.type EQ 0) THEN BEGIN ; Value changed. 
                       
                        ; Get the value of the property identified by 
                        ; event.identifier. 
                        value = Widget_Info(event.ID, COMPONENT = event.component, PROPERTY_VALUE = event.identifier) 
                       
                        ; Set the component's property value. 
                        event.component->SetPropertyByIdentifier, event.identifier, value 
                       
                       ENDIF ELSE BEGIN ; Selection changed. 
                       
                       ENDELSE
                       
                       self.selComponent = event.component
                       Widget_Control, self.prop, PROPERTYSHEET_SETSELECTED=event.identifier
                       IF Obj_Valid(event.component) THEN Widget_Control, self.prop, /REFRESH_PROPERTY
                     END

    'PropBase'     : BEGIN
                       IF Tag_Names(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN Widget_Control, event.id, MAP = 0 $
                                                                                        ELSE WIDGET_CONTROL, self.prop, SCR_XSIZE = event.x 
                     END
    'Add Detector' : BEGIN
                       IF N_Elements(*self.areaDetectors) GT 0 THEN *self.areaDetectors = [*self.areaDetectors, Obj_New('as_areadetector')] $
                                                               ELSE *self.areaDetectors = Obj_New('as_areadetector')
                       Widget_Control, self.prop, SET_VALUE=*self.areaDetectors
                       size = Widget_Info(self.prop,/GEOMETRY)
                       WIDGET_CONTROL, self.prop, SCR_XSIZE = size.scr_xsize + 100 
                     END
    'Start Polling' : BEGIN
                        self->poll, 1
                      END
    'Delete Detector' : BEGIN
                       IF N_Elements(*self.areaDetectors) EQ 0 THEN BREAK
                       component = Where(*self.areaDetectors EQ self.selComponent, COMPLEMENT = keep, /NULL)
                       *self.areaDetectors = (*self.areaDetectors)[keep]
                       Obj_Destroy, self.selComponent
                       IF N_Elements(*self.areaDetectors) GT 0 THEN Widget_Control, self.prop, SET_VALUE=*self.areaDetectors ELSE Widget_Control, self.prop, SET_VALUE=Obj_New()
                     END                     
  ENDCASE

END

PRO as_areadetectormap::ShowGUI, base, GROUP_LEADER = groupLeader
  
  @as_scatterheader.macro
  
  IF Widget_Info(self.prop,/VALID) THEN BEGIN
    Widget_Control, Widget_Info(self.prop,/PARENT), /MAP
    RETURN
  ENDIF
  
  IF Arg_Present(base) THEN propBase = Widget_Base(base, GROUP_LEADER = groupLeader, EVENT_PRO = 'as_ADMap_PropertyEvent', TITLE = 'Detector Dialog', /COLUMN, /TLB_SIZE_EVENTS, /TLB_KILL_REQUEST_EVENTS, UNAME='PropBase') $
                        ELSE BEGIN
                          propBase = Widget_Base(GROUP_LEADER = groupLeader, EVENT_PRO = 'as_ADMap_PropertyEvent', TITLE = 'Detector Dialog', /COLUMN, /TLB_SIZE_EVENTS, /TLB_KILL_REQUEST_EVENTS, UNAME = 'PropBase')
                          Widget_Control, propBase, /REALIZE
                          XManager, 'as_areadetectormap', propBase, EVENT_HANDLER = 'as_ADMap_PropertyEvent', /NO_BLOCK
                        ENDELSE
                        
  self.prop = Widget_PropertySheet(propBase, GROUP_LEADER = groupLeader, VALUE = *self.areaDetectors, $
                              EVENT_PRO = 'as_ADMap_PropertyEvent', UNAME = 'PropSheet', SCR_XSIZE = (150*N_Elements(*self.areaDetectors)) + 120 > 250, YSIZE=25, /MULTIPLE_PROPERTIES)
  
  add    = Widget_Button(propBase, GROUP_LEADER = groupLeader, SCR_XSIZE = 50, VALUE = 'Add Detector', UNAME = 'Add Detector')
  poll   = Widget_Button(propBase, GROUP_LEADER = groupLeader, SCR_XSIZE = 50, VALUE = '(Re) Start Polling', UNAME = 'Start Polling')
  delete = Widget_Button(propBase, GROUP_LEADER = groupLeader, SCR_XSIZE = 50, VALUE = 'Delete Detector', UNAME = 'Delete Detector')
  
  Widget_Control, self.prop, SET_UVALUE = self
  
END

PRO as_areadetectormap::poll, start
  
  @as_scatterheader.macro
  
  IF N_Elements(self.areaDetectors) EQ 0 THEN BEGIN
    result = Dialog_Message('No detectors defined, polling not started.')
    RETURN
  ENDIF
  
  IF N_Elements(start) EQ 0 THEN start = 1
  self.poll = start
  CASE start OF 
    1    :  Widget_Control, self.timerBase, TIMER = 0.1
    ELSE :
  ENDCASE      
        
END

PRO as_areadetectormap::Stop
 
  @as_scatterheader.macro
 
  FOREACH det, *self.areaDetectors DO BEGIN
    det->SetProperty, ACQUIRE = 0
    ;det->SetProperty, 
  END
    
END

PRO as_areadetectormap::Acquire
  
  @as_scatterheader.macro
  
  result = caput('SR13ID01IOC69:Acquire_CMD', 1)
  
;  castartgroup
;  FOR i = 0, N_Elements(*self.areaDetectors) - 1 DO BEGIN
;    
;      ((*self.areaDetectors)[i])->GetProperty, SOFTWARETRIGGER=softwareTrigger
;      IF softwareTrigger THEN ((*self.areaDetectors)[i])->SetProperty, /ACQUIRE
;    
;  ENDFOR
;  result = caendgroup()
  

END

FUNCTION as_areadetectormap::GetFrame, detId

  @as_scatterheader.macro

  frame = Reverse((*self.areaDetectors)[detId]->GetArray(),2)
  RETURN, frame

END

PRO as_areadetectormap::StoreParams, paramObj

  @as_scatterheader.macro

  ADMAP = !Null

  FOREACH det, *self.areaDetectors DO ADMAP = [ADMAP,det.GetParamMap()]
  
  paramObj->SetParameters, ADMAP=ADMAP

END

PRO as_areadetectormap::RefreshPropertySheet

  @as_scatterheader.macro

  IF self.prop GT 0 THEN Widget_Control, self.prop, SET_VALUE = *self.areaDetectors

END

PRO as_areadetectormap::NewParams, paramObj

  @as_scatterheader.macro

  paramObj->GetParameters, ADMAP=ADMap

  numADs = N_Elements(*self.areaDetectors)
  numNewADs = N_Elements(ADMap)
  
  IF numNewADs GT numADs THEN FOR i = 0, numNewADs-numADs-1 DO *self.areaDetectors = [*self.areaDetectors, Obj_New('as_areadetector')]
  IF numNewADs LT numADs THEN BEGIN
    Obj_Destroy, (*self.areaDetectors)[numNewADs:*]
    *self.areaDetectors = (*self.areaDetectors)[0:numNewADs-1]
  ENDIF
  
  FOR i = 0, numNewADs - 1 DO BEGIN
    (*self.areaDetectors)[i]->NewParams, ADMap[i]
  ENDFOR

  self.RefreshPropertySheet

END

PRO as_areadetectormap::GetProperty, detID, FILENAME = fileName, NUMDETECTORS = numDet, EXPOSURETIME = exposureTime, EXPOSUREPERIOD = exposurePeriod, _REF_EXTRA = extra

  @as_scatterheader.macro

  IF Arg_Present(fileName) THEN BEGIN
    result = caget('SR13ID01IOC69:FileNameCommon', fileName)
    fileName = String(fileName)
  ENDIF

  IF Arg_Present(numDet) THEN numDet = N_Elements(*self.areaDetectors)

  IF Arg_Present(exposureTime) THEN result = caget('SR13ID01IOC69:AcquireTimeCommon', exposureTime)
  IF Arg_Present(exposureTime) THEN result = caget('SR13ID01IOC69:AcquirePeriodCommon', exposurePeriod)

  IF detID GT N_Elements(*self.areaDetectors) - 1 THEN RETURN
  IF ~Obj_Valid((*self.areaDetectors)[0]) THEN RETURN
  
  CATCH, errorStatus
  
  IF errorStatus NE 0 THEN BEGIN
    CATCH, /CANCEL
    PRINT, !err_string
    RETURN  
  ENDIF
    
  IF N_Elements(detID) GT 0 THEN (*self.areaDetectors)[detID]->GetProperty, _EXTRA=extra

END

PRO as_areadetectormap::SetProperty, detID, FILENAME = fileName, EXPOSURETIME = exposureTime, EXPOSUREPERIOD = exposurePeriod, _REF_EXTRA = extra

  @as_scatterheader.macro

  IF KeyWord_Set(fileName) THEN result = caput('SR13ID01IOC69:FileNameCommon', ezcaStringToByte(fileName))
  IF KeyWord_Set(exposureTime) THEN result = caput('SR13ID01IOC69:AcquireTimeCommon', exposureTime)
  IF KeyWord_Set(exposurePeriod) THEN result = caput('SR13ID01IOC69:AcquirePeriodCommon', exposurePeriod)
  
  IF N_Elements(detID) GT 0 THEN (*self.areaDetectors)[detID]->SetProperty, _EXTRA=extra ELSE BEGIN
  
    FOR i = 0, N_Elements((*self.areaDetectors)) - 1 DO BEGIN
      (*self.areaDetectors)[i]->GetProperty, CONTROL=control
      IF control EQ 1 THEN (*self.areaDetectors)[i]->SetProperty, _EXTRA=extra
    ENDFOR
  
  ENDELSE
  
END

PRO as_areadetectormap::notify, event

  @as_scatterheader.macro

  FOREACH notify, self.notifyObj DO IF Obj_Valid(notify) THEN notify.notify, event

END

PRO as_areadetectormap__define

  void = { as_areadetectormap,          $
           timerBase      : 0L,         $
           areaDetectors  : Ptr_New(),  $
           prop           : 0L,         $
           notifyObj      : List(),  $
           selComponent   : Obj_New(),  $
           poll           : 0           $
         }

END