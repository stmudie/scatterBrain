PRO AS_DefineMask_event, event
  
  @as_scatterheader.macro
  
  Widget_Control, event.top, GET_UVALUE = maskObj
  maskObj->AS_Maskobj::event,event
END
  
PRO AS_DefineBeamStopPropertyEvent, event

  @as_scatterheader.macro 
  
  IF (event.type EQ 0) THEN BEGIN
    value = WIDGET_INFO(event.ID, COMPONENT = event.component, PROPERTY_VALUE = event.identifier)
    ; Set the component’s property value.
    event.component->SetPropertyByIdentifier, event.identifier, value
  ENDIF
  Widget_Control, event.id, /REFRESH_PROPERTY
  Widget_Control, event.top, GET_UVALUE = maskObj
  maskObj.DrawImage, /PRESERVEVIEWPLANE
  
END

PRO AS_DefineMaskPropertyEvent, event

  @as_scatterheader.macro

  IF Tag_Names(event, /STRUCTURE_NAME) EQ 'WIDGET_TIMER' THEN BEGIN
    ; TODO Wanted to have mask property sheets to update regularly so when moving masks with mouse in frame the info would update.
    ; However, then you cant change anything in property sheet unless you are quicker than 1 second. Need different solution.
    ;Widget_Control, event.id, /REFRESH_PROPERTY
    ;IF Widget_Info(event.id, /MAP) THEN Widget_Control, event.id, TIMER = 1
    RETURN
  ENDIF

  Widget_Control, event.top, GET_UVALUE = maskObj
  IF (event.type EQ 0) THEN BEGIN ; Value changed.
  
     ; Get the value of the property identified by event.identifier.
     value = Widget_Info(event.ID, COMPONENT = event.component, PROPERTY_VALUE = event.identifier)
  
     ; Set the component’s property value.
     IF event.identifier EQ 'NAME' THEN BEGIN
     
       event.component.GetProperty, NAME = oldName, LOCK = lock
       IF lock EQ 1 THEN RETURN
     
       Widget_Control, event.id, GET_VALUE = maskObjects
       
       FOREACH m, maskObjects DO BEGIN
         IF m EQ event.component THEN CONTINUE
         m.GetProperty, NAME = maskName
         IF StrUpCase(value) EQ StrUpCase(maskName) THEN BEGIN
           value = oldName
           result = Dialog_Message("Two or more masks cannot have the same name.", /ERROR)
           BREAK
         ENDIF
       ENDFOREACH 
     
       IF StrUpCase(value) NE StrUpCase(oldName) THEN BEGIN
         maskObj->MaskNameUpdated, value, oldName
         Widget_Control, Widget_Info(event.id,/PARENT), BASE_SET_TITLE = value
       ENDIF
     
     ENDIF
     event.component->SetPropertyByIdentifier, event.identifier, value
     
     IF event.identifier EQ 'COLOR' AND Obj_Class(event.component) EQ 'IDLGRPOLYLINE' THEN BEGIN
       Widget_Control, event.id, GET_VALUE=objects
       obj = Where(event.component EQ objects)
       row = (Widget_Info(Widget_Info(event.top, FIND_BY_UNAME='Mask Table ' + StrCompress(String(obj),/REMOVE_ALL)),/TABLE_SELECT))[1]
       maskObj.SetVertexColours, event.component, value, SELECT = row
     ENDIF
     maskObj.DrawImage, /PRESERVEVIEWPLANE
     maskObj.UpdateTable, event.component
     maskObj.PolyChanged     
  ENDIF ELSE BEGIN ; Selection changed.Return information about single or multiple property selections. 
       
    maskObj.SetSelected, event.component

  ENDELSE
  Widget_Control, event.id, /REFRESH_PROPERTY
END

FUNCTION AS_MaskObj::Init, MASKNOTIFY = notifyObj, _REF_Extra=extra

  @as_scatterheader.macro

  IF Keyword_Set(notifyObj) THEN $
    IF TypeName(notifyObj[0]) EQ 'NOTIFY' $
      THEN self.mask.notify = List(notifyObj,/EXTRACT)

  self.mask.current =    2
  self.mask.zoom =       3
  self.mask.numPoints =  25
  self.mask.mask =       Ptr_New(1)
  self.mask.maskdef =    Ptr_New(/ALLOCATE_HEAP)
  self.mask.maskObjects = IDLgrModel()
  self.mask.beamStop = as_beamstopmaskobject(500,500,20,30,10,500)
  self.mask.maskObjects.add, self.mask.beamStop 
  self.mask.lastVertex = list()
  
  self.mask.oldMaskPoly = Ptr_New(/ALLOCATE_HEAP)
  
  self.mask.nameChanges = Ptr_New(ALLOCATE_HEAP)
                    
  result = self->AS_FrameObj::Init( _Extra=extra)

  self.parent.add, self.mask.maskObjects
  
  RETURN, result

END
  

;*************************************************************************
; Copyright (c) 2008 Australian Synchrotron
;*************************************************************************
;+
; NAME:
; AS__SaxsMakeMaskPolygon
;
; PURPOSE:
; This procedure constructs masking polygons - these are used by saxs_cake_setup to make make
; the mask array used for all profile integrations
;
; CATEGORY:
; SAXS Detector Masks
;
; CALLING SEQUENCE:
;
; AS__SaxsMakeMaskPolygon, frmdata, mask_polygon
;
; INPUTS:
; fmrdata:  Structure containing information about the frame.
; mask_polygon: Structure containing information about the masks.
;
; KEYWORD PARAMETERS:
; FRAMEBORDER: Make frame mask. 
; BEAMSTOP: Make beamstop mask 
;
; OUTPUTS:
; Passed structures updated with new mask parameters.
;
; SIDE EFFECTS:
; Passed structures updated with new mask parameters.
;
; MODIFICATION HISTORY:
;   Written by: David Cookson December 2002
;   01-Dec-02  created David J. Cookson
;   01-Jan-03  DJC - added the masking for image frames from Wire Detector
;   27-Nov-03  DJC - bug fix for SMART frames with 512x512 pixels
;   15-May-08  STM - Updated to AS convention. Removed common blocks - structures passed as variables.
;-


PRO AS__MaskObj::MakeMaskPolygon, FRAMEBORDER=frameborder, BEAMSTOP=beamstop

  @as_scatterheader.macro

    xc = self.frame.xc
    yc = self.frame.yc
    xbsr = self.frame.xbsr
    ybsr = self.frame.ybsr
    nx = self.frame.nxpix
    ny = self.frame.nypix

    IF KEYWORD_SET(frameborder) THEN BEGIN
        ; Polygon 0 - Outer frame border including - this is applied first
        ; Polygon 1 - Excluding mask for beam stop and contents - this is applied last

        CASE frmdata.type OF

            'SMART': BEGIN
                self.mask.npts[0] = 10
                self.mask.x[0,0:9] = [30,500,950,991,993,1010,500,44,39,30] / 1024. * nx
                self.mask.y[0,0:9] = [984,986,995,950,500,34,34,27,500,984] / 1024. * ny
            END

            'MAR165' : BEGIN
                self.mask.npts[0] = N_ELEMENTS(self.mask.x[0,*])
                radius = MIN([nx,ny])/2 - 1
                cx = nx/2 -1
                cy = ny/2 -1
                th = FINDGEN(self.mask.npts[0])/(self.mask.npts[0]-1) * !PI * 2
                cirpol = TRANSPOSE([[th],[REPLICATE(radius, self.mask.npts[0])]])
                cirxy = CV_COORD(FROM_POLAR=cirpol,/to_rect)
                self.mask.x[0,*] = cirxy[0,*] + cx
                self.mask.y[0,*] = cirxy[1,*] + cy
            END

            'MAR345' : BEGIN
                self.mask.npts[0] = N_ELEMENTS(self.mask.x[0,*])
                radius = MIN([nx,ny])/2 - 1
                cx = nx/2 -1
                cy = ny/2 -1
                th = FINDGEN(self.mask.npts[0])/(self.mask.npts[0]-1) * !PI * 2
                cirpol = TRANSPOSE([[th],[REPLICATE(radius, self.mask.npts[0])]])
                cirxy = CV_COORD(from_polar=cirpol,/TO_RECT)
                self.mask.x[0,*] = cirxy[0,*] + cx
                self.mask.y[0,*] = cirxy[1,*] + cy
            END

            'ADC' : BEGIN
                self.mask.npts[0] = 5
                self.mask.x[0,0:4] = [0.04,0.96,0.96,0.04,0.04] * nx
                self.mask.y[0,0:4] = [0.98,0.98,0.12,0.12,0.98] * ny
            END

            ELSE : BEGIN
                self.mask.npts[0] = 5
                self.mask.x[0,0:4] = [0.02,0.98,0.98,0.02,0.02] * nx
                self.mask.y[0,0:4] = [0.98,0.98,0.02,0.02,0.98] * ny
            ENDELSE
        ENDCASE
    ENDIF

    IF KEYWORD_SET(beamstop) THEN BEGIN
        ; Now make beam stop mask
        ; bdr1 is the border width around the center of the beamstop arm shadow
        ; bdr2 is the border width around the center of the beamstop  shadow
        bdr1 = 10. / 1024. * nx
        bdr2 = 20. / 1024. * nx
        PI = 3.14159
        p = CV_COORD(FROM_RECT=[xbsr-xc,ybsr-yc], /to_polar)
        spoon_th = [p[0], p[0]+bdr1/p[1], p[0]+bdr1/bdr2,p[0]+PI/4, p[0]+PI/2, p[0]+PI*0.75, p[0]+PI]
        spoon_r = [p[1], p[1], bdr2, bdr2, bdr2, bdr2, bdr2]
        mask_th = [spoon_th, REVERSE(2*p[0] - spoon_th)]
        mask_r = [spoon_r, REVERSE(spoon_r)]
        mask_pol = [TRANSPOSE(mask_th),TRANSPOSE(mask_r)]
        maskxy = CV_COORD(FROM_POLAR = mask_pol, /TO_RECT)
        self.mask.npts[1] = 14
        self.mask.x[1,0:self.mask.npts[1]-1] = maskxy[0,*] + xc
        self.mask.y[1,0:self.mask.npts[1]-1] = maskxy[1,*] + yc
    ENDIF
END

PRO AS_MaskObj::ApplyMasks

  self.PolyChanged, /SET
  self.UpdateMaskArray

END

PRO AS_MaskObj::ShowMasks, CLEAR=clear, UNFILLED=unfilled, OPACITY = opacity

  @as_scatterheader.macro

  IF ~Obj_Valid(self.mask.maskObjects) THEN RETURN

  IF KeyWord_Set(clear) THEN BEGIN
    self.mask.maskObjects.SetProperty, /HIDE
    RETURN
  ENDIF

  self.mask.maskObjects.SetProperty, HIDE = 0

  IF ~KeyWord_Set(unfilled) THEN unfilled = 0
    
  maskObjs = self.mask.maskObjects.get(/all)
  FOREACH maskObj, maskObjs DO BEGIN
    IF KeyWord_Set(opacity) THEN BEGIN maskObj.GetProperty, LINEOPACITY = lineOpacity, FILLOPACITY = fillOpacity
      IF fillOpacity GT 0 THEN ratio = lineOpacity/fillOpacity ELSE ratio = 2
      maskObj.SetProperty, LINEOPACITY = opacity/100., FILLOPACITY = opacity/(ratio*100.), FILLHIDE = unfilled
    ENDIF ELSE maskObj.SetProperty, FILLHIDE = unfilled
    
  ENDFOREACH
      
END

PRO AS_MaskObj::ShowMaskTable, show

  @as_scatterheader.macro

  IF Widget_info(self.mask.defineMaskBase,/VALID) THEN BEGIN
    Widget_Control, self.mask.defineMaskBase, MAP = show
    Widget_Control, Widget_Info(self.mask.defineMaskBase, FIND_BY_UNAME = 'defineMaskPropSheet'), TIMER = 1
  ENDIF ELSE BEGIN
    
    monitorObj = IDLsysMonitorInfo()
    primaryMonitor = monitorObj.GetPrimaryMonitorIndex()
    monitorSize = monitorObj.GetRectangles(/EXCLUDE_TASKBAR)
    
    defineMaskBase = Widget_Base(GROUP_LEADER=self.frame.group_leader, TITLE = 'scatterBrain Mask Definitions', /FLOATING, /TLB_SIZE_EVENT, /TLB_KILL_REQUEST_EVENTS, UVALUE=self, /COLUMN, MBAR = menuBar, X_SCROLL_SIZE = 500, UNAME = 'Mask Base')
    addButtonMenu = Widget_Button(menuBar, /MENU, VALUE='Add Mask')
    addButton = Widget_Button(addButtonMenu, VALUE='Add Mask', UNAME = 'Add Mask')
    deleteButtonMenu = Widget_Button(menuBar, /MENU, VALUE='Delete Mask')
    deleteButton = Widget_Button(deleteButtonMenu, VALUE='Delete Mask', UNAME = 'Delete Mask Base')
    applyButtonMenu = Widget_Button(menuBar, /MENU, VALUE='Apply Masks')
    applyButton = Widget_Button(applyButtonMenu, VALUE='Apply Masks', UNAME = 'Apply Masks')
    maskPropertiesBase = Widget_Base(defineMaskBase, /ROW)
    self.mask.wMaskTab = Widget_Tab(maskPropertiesBase, /MULTILINE)
    beamStopBase = Widget_Base(self.mask.wMaskTab, TITLE = 'Beamstop', /ROW, UNAME='Mask Tab Beamstop') 
    defineBeamStopPropertySheet = Widget_PropertySheet(beamStopBase, VALUE = self.mask.beamStop, ysize = 15, EVENT_PRO = 'AS_DefineBeamStopPropertyEvent', UNAME = 'Beamstop Property')
    
    maskObjects = (self.mask.maskObjects.GET(/ALL, ISA = 'as_maskobject',COUNT = nMasks))
    
    IF nMasks EQ 0 THEN maskObjects = ObjArr(1) 
    
    i = 0
    FOREACH mask, maskObjects DO BEGIN
      IF ~Obj_Valid(mask) THEN CONTINUE
      tabBase = Widget_Base(self.mask.wMaskTab, TITLE = mask.name, /ROW, UNAME='Mask Tab ' + StrCompress(String(i),/REMOVE_ALL)) 
      ;defineMaskPropertySheet = Widget_PropertySheet(defineUserMasksBase,  VALUE = maskObjects, SCR_XSIZE = 70+150*N_Elements(maskObjects[0:*:2]), ysize = 17, EVENT_PRO = 'AS_DefineMaskPropertyEvent', UNAME = 'defineMaskPropSheet') 
      defineMaskPropertySheet = Widget_PropertySheet(tabBase,  VALUE = mask, ysize = 17, EVENT_PRO = 'AS_DefineMaskPropertyEvent', UNAME = 'defineMaskPropSheet') 
      defineMaskTableBase = Widget_Base(tabBase, /ROW, UNAME = 'Mask Table Base')
      buffer = Widget_Label(tabBase, VALUE = '', XSIZE = 36)
      mask.getproperty, DATA=data, /RELATIVE
      defineMaskTable = Widget_Table(tabBase, XSIZE = 2, YSIZE = self.mask.numPoints*2, SCR_XSIZE = 196, COLUMN_WIDTHS = 60, FORMAT = '(F7.2)', /ALL_EVENTS, /EDITABLE, /CONTEXT_EVENTS, ALIGNMENT = 2, UVALUE = {CellChangedFlag: 0, CellBuffer: [0,0], CellPosition: [0,0]}, UNAME='Mask Table ' + StrCompress(String(i),/REMOVE_ALL))
      Widget_Control, defineMaskTable, SCR_YSIZE = (Widget_Info(defineMaskTable,/ROW_HEIGHTS))[0] * (self.mask.numPoints+2)
      IF data NE !null THEN Widget_Control, defineMaskTable, SET_VALUE = data
      i += 1
    ENDFOREACH

    Widget_Control, defineMaskBase, MAP = show
    Widget_Control, defineMaskBase, /REALIZE
    ;Widget_Control, defineMaskPropertySheet, TIMER = 1
    self.mask.defineMaskBase=defineMaskBase
    XManager, 'AS_DefineMask', defineMaskBase, /NO_BLOCK
      
  ENDELSE

END

PRO AS_MaskObj::NewMask, MASKOBJECT = maskObject

  @as_scatterheader.macro

  maskObject = as_maskObject()
  self.mask.maskObjects.add, maskObject

END

PRO AS_MaskObj::AddDefinedMasks

  @as_scatterheader.macro

  self.mask.maskObjects.remove, /ALL
  self.mask.maskObjects.add, self.mask.beamStop
  IF N_Elements(*self.mask.maskdef) LE 0 THEN RETURN
  FOREACH mask, *self.mask.maskdef DO BEGIN
  
    maskObject = as_maskObject()
    SWITCH StrCompress(StrUpCase(mask.shape)) OF
            'POLYGON' :
            '0'       : BEGIN
                          maskObject.SetProperty, MASKTYPE = mask.type, MASKSHAPE = mask.shape, DATA = *mask.params, FILLOPACITY = 0.5, NAME = mask.name, BEAMRELATIVE=mask.auto, COLOUR = mask.colour, LOCK=mask.lock
                          BREAK
                        END
            'CIRCLE'  :  
            '1':        BEGIN
                          params = FltArr(6)
                          params[4] = 360  ; Default is a circle
                          params[0:N_Elements(*mask.params)-1] = *mask.params
                          maskObject.SetProperty, MASKTYPE=mask.type, MASKSHAPE=mask.shape, CENTREX=params[0], CENTREY=params[1], RADIUSMAX=params[2], RADIUSMIN=params[3], ANGLEMAX=params[4], ANGLEMIN=params[5], FILLOPACITY = 0.5, NAME = mask.name, COLOUR = mask.colour, LOCK = mask.lock, BEAMRELATIVE=mask.auto
                          BREAK
                        END
    ENDSWITCH
    
    self.mask.maskObjects.add, maskObject
    IF Widget_Info(self.mask.defineMaskBase, /VALID) THEN Widget_Control, self.mask.defineMaskBase, /DESTROY
    
    ;self.parent->Add, maskObject.getModel()   
    
  
  ENDFOREACH

  self.ShowMaskTable, 0
 
  self.SetSelected, maskObject
  self.PolyChanged, /SET
  

END

PRO AS_MaskObj::UpdateTable, updatedMask

  @as_scatterheader.macro

  IF ~Obj_Valid(updatedMask) THEN updatedMask = self.mask.selectedMask
  updatedMask.GetProperty, DATA = data, SELECTEDVERTEX = selectedVertex, /RELATIVE
  table = Where(self.mask.maskObjects.Get(/ALL, ISA = 'as_maskobject') EQ updatedMask)
  table = Widget_Info(self.mask.defineMaskBase, FIND_BY_UNAME='Mask Table ' + StrCompress(table, /REMOVE_ALL))
  Widget_Control, table, SET_VALUE = data
  Widget_Control, table, SET_TABLE_SELECT = [0,selectedVertex,1,selectedVertex]
  Widget_Control, table, SET_TABLE_VIEW=[0,0]

END

PRO AS_MaskObj::UndoAddClick

  IF self.mask.lastVertex.count() EQ 0 THEN RETURN
  
  lastVertex = (self.mask.lastVertex).remove(-1)

  IF Obj_Valid(lastVertex.mask) EQ 1 THEN BEGIN
    lastVertex.mask.selectvertex, lastVertex.vertex
    lastVertex.mask.deletevertex
    lastvertex.mask = Obj_New()
    lastVertex.vertex = 0
  ENDIF

END

PRO AS_MaskObj::SetSelected, selectedObj

  @as_scatterheader.macro

  IF Size(selectedObj, /TYPE) GE 1 AND Size(selectedObj, /TYPE) LE 5 THEN BEGIN
    selectedObj = (self.mask.maskObjects.Get(/ALL, ISA = 'as_maskobject'))[Fix(selectedObj)]
  ENDIF
  IF Obj_Valid(selectedObj) THEN maskNum = Where(selectedObj EQ self.mask.maskObjects.Get(/ALL, ISA = 'as_maskobject')) ELSE maskNum = -1
   IF maskNum GE 0 THEN BEGIN
    IF Obj_Valid(self.mask.selectedMask) THEN self.mask.selectedMask.ShowLabels, 0
    self.mask.selectedMask = selectedObj
    self.mask.selectedMask.ShowLabels, 1
    self.DrawImage, /PRESERVEVIEWPLANE
    Widget_Control, self.mask.wMaskTab, SET_TAB_CURRENT = maskNum + 1
  ENDIF

END

PRO AS_MaskObj::PolyChanged, SET=set

  @as_scatterheader.macro

  IF self.mask.maskObjects.count() EQ 0 THEN RETURN
  temp = FltArr(self.mask.maskObjects.count(),2,50)
  i = 0
  FOREACH m, self.mask.maskObjects.get(/ALL) DO BEGIN
    m.GetProperty, DATA=data
    num = m.numVertices
    IF num EQ 0 THEN CONTINUE
    temp[i,*,0:num-1] = data
    i++
  ENDFOREACH
  IF KeyWord_Set(set) THEN BEGIN
    *self.mask.oldMaskPoly = temp 
    self.frame.frameViewobj->GetProperty, VIEWPLANE_RECT=view
    self.message_obj.DeleteMessage, 'UnMasks'
    self.DrawImage, /PRESERVEVIEWPLANE
  ENDIF ELSE BEGIN
    oldPoly  = *self.mask.oldMaskPoly
    IF oldPoly EQ !Null THEN oldPoly = 0
    IF Total(Abs(oldPoly - temp)) GT 0 THEN BEGIN
      self.frame.frameViewobj->GetProperty, VIEWPLANE_RECT=view
      self.message_obj.SetProperty, LOCATIONS = [0,view[3]-20,0]
      self.message_obj.AddMessage, 'UnMasks','Unapplied Masks'
      self.DrawImage, /PRESERVEVIEWPLANE
    ENDIF
  ENDELSE

END

PRO AS_MaskObj::ClearMaskArray

  @as_scatterheader.macro

  *self.mask.mask = 1
  
END

PRO AS_MaskObj::DefineMasks, define

  @as_scatterheader.macro

  self.mask.DefiningMask = define
  self.ShowMaskTable, define

END

PRO AS_MaskObj::MaskNameUpdated, newName, oldName
  
  @as_scatterheader.macro

  IF Ptr_Valid(self.mask.nameChanges) THEN *self.mask.nameChanges = [[*self.mask.nameChanges],[newName,oldName]] ELSE self.mask.nameChanges = Ptr_New([newName,oldName])

END

PRO AS_MaskObj::GetProperty, _REF_Extra = extra

  @as_scatterheader.macro

  self.AS_FrameObj::GetProperty, _EXTRA = extra

END

PRO AS_MaskObj::StoreParams, paramObj, CONFIG = config

  @as_scatterheader.macro

  maskdef = !NULL
  beamstop = !NULL
  IF Obj_Valid((self.mask.maskObjects.Get(/ALL))[0]) GT 0 THEN BEGIN
    FOREACH m, self.mask.maskObjects.Get(/ALL) DO BEGIN
      params = m.GetSaveParams()
      IF Size(params,/TNAME) EQ 'STRUCT' THEN BEGIN
        CASE params.maskShape OF
          'Beamstop' : beamstop = params 
          ELSE       : maskdef = [maskdef,{name : params.name, shape : params.maskShape, type : params.maskType, auto : params.beamRelative, colour : params.colour, lock : params.lock, params: Ptr_New(params.data)} ]
        ENDCASE
      ENDIF
    ENDFOREACH
  
  paramObj->SetParameters, MASK=maskdef, CHANGEDMASKNAMES = *self.mask.nameChanges, BEAMSTOP = beamstop, CONFIGNO = config
  *self.mask.nameChanges = !Null
  
  ENDIF
  
  self.AS_FrameObj::StoreParams, paramObj, CONFIG=config
  
END

PRO AS_MaskObj::MaskNotify, event
  
  @as_scatterheader.macro
  
  FOREACH notify, self.mask.notify DO IF Obj_Valid(notify) THEN notify.notify, event

END

PRO AS_MaskObj::Event, event, scatterEvent
  
  @as_scatterheader.macro
  
  widgetName = Widget_Info(event.id, /UNAME)
  
  split = StrSplit(widgetName, ' ',/EXTRACT)
  IF N_Elements(split) EQ 3 THEN BEGIN
    IF split[0]+split[1] EQ ['MaskTable'] THEN BEGIN
      widgetName = 'Mask Table'
      tableNo = Fix(split[2])
    ENDIF
  ENDIF 
  CASE widgetName OF
    'Mask Base'  : BEGIN
                     IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
                       self.showMaskTable, 0
                       self.mask.definingMask = 0
                       self.masknotify, {MASKWINDOW, event : 'Close' } 
                     ENDIF ELSE Widget_Control, event.id, SCR_XSIZE = event.x
                   END
    'Mask Table' : BEGIN
                    
                    self.SetSelected, tableNo
                    IF Tag_Names(event,/STRUCTURE) EQ 'WIDGET_CONTEXT' THEN BEGIN
                      result = Dialog_Message('Delete This Vertex?', /QUESTION)
                      IF result EQ 'Yes' THEN BEGIN
                        self.mask.selectedMask.SelectVertex, event.row
                        self.mask.selectedMask.DeleteVertex
                        self.mask.selectedMask.GetProperty, DATA=data, /RELATIVE
                        Widget_Control, event.id, SET_VALUE=data
                      ENDIF
                    ENDIF ELSE BEGIN
                      IF event.type EQ 4 THEN BEGIN
                        IF event.sel_top GE 0 THEN BEGIN
                          self.mask.selectedMask.SelectVertex, event.sel_top
                          view = Widget_Info(event.id, /TABLE_VIEW)
                          FOR i = 0, N_Elements(self.mask.maskObjects.Get(/ALL, ISA = 'as_maskobject')) - 1 DO BEGIN
                            Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='Mask Table ' + StrCompress(i,/REMOVE_ALL)), SET_TABLE_VIEW=view
                          ENDFOR
                        ENDIF ELSE BEGIN
                          Widget_Control, event.id, GET_UVALUE = cellTracking
                          IF cellTracking.CellChangedFlag EQ 1 THEN BEGIN
                            IF cellTracking.CellBuffer EQ -1 THEN Widget_Control, event.id, /DELETE_ROWS, USE_TABLE_SELECT=[cellTracking.CellPosition,cellTracking.CellPosition] $
                                                             ELSE Widget_Control, event.id, SET_VALUE = cellTracking.CellBuffer, USE_TABLE_SELECT=[cellTracking.CellPosition,cellTracking.CellPosition]
                            Widget_Control, event.id, SET_UVALUE = {CellChangedFlag: 0, CellBuffer: [0,0], CellPosition: [0,0]}
                          ENDIF
                        ENDELSE
                      ENDIF
                      IF event.type EQ 0 THEN BEGIN
                        Widget_Control, event.id, GET_UVALUE = cellTracking
                        IF cellTracking.CellChangedFlag LE 0 THEN BEGIN
                          IF event.y LT self.mask.selectedMask.NumVertices() THEN BEGIN
                            Widget_Control, event.id, GET_VALUE = data, /USE_TABLE_SELECT
                            Widget_Control, event.id, SET_UVALUE = {CellChangedFlag: 1, CellBuffer: data, CellPosition: [event.x,event.y]}
                          ENDIF ELSE BEGIN
                            IF (event.ch LT 48B OR event.ch GT 57B) AND event.ch NE 13B AND event.ch NE 45B THEN BEGIN
                              Widget_Control, event.id, GET_VALUE = data
                              Widget_Control, event.id, SET_VALUE = data
                              Widget_Control, event.id, SET_UVALUE = {CellChangedFlag: 0, CellBuffer: [0,0], CellPosition: [0,0]}
                              RETURN
                            ENDIF
                            IF cellTracking.CellChangedFlag EQ -1 THEN buffer = [cellTracking.cellbuffer, event.ch] ELSE buffer = event.ch
                            Widget_Control, event.id, SET_UVALUE = {CellChangedFlag: -1, CellBuffer: buffer, CellPosition: [event.x,event.y]}
                          ENDELSE
                        ENDIF
                        IF (event.ch LT 48B OR event.ch GT 57B) AND event.ch NE 13B AND event.ch NE 45B THEN BEGIN
                          Widget_Control, event.id, GET_VALUE = data
                          IF cellTracking.CellBuffer[0] EQ -1 THEN Widget_Control, event.id, SET_VALUE = data[*,0:-2] $
                                                              ELSE Widget_Control, event.id, SET_VALUE = data[event.x,event.y], /USE_TABLE_SELECT, ALIGNMENT = 2
                          Widget_Control, event.id, SET_UVALUE = {CellChangedFlag: 0, CellBuffer: [0,0], CellPosition: [0,0]}
                        ENDIF
                        IF event.ch EQ 13B THEN BEGIN
                          IF event.y GT self.mask.selectedMask.numVertices() THEN RETURN
                          IF event.y LT self.mask.selectedMask.numVertices() THEN BEGIN 
                            Widget_Control, event.id, GET_VALUE = data
                            self.mask.selectedMask.MoveVertex, data[*,event.y], /RELATIVE
                          ENDIF ELSE BEGIN
                            Widget_Control, event.id, GET_VALUE = data
                            data = [[data],[0,0]]
                            data[event.x,event.y] = Fix(String(Byte(cellTracking.cellbuffer)))
                            Widget_Control, event.id, SET_VALUE = data
                            self.mask.selectedMask.MoveVertex, data[*,event.y], /ADD, /RELATIVE
                          ENDELSE
                          self.mask.selectedMask.GetProperty, DATA=data, /RELATIVE
                          Widget_Control, event.id, SET_VALUE=data, SET_UVALUE = {CellChangedFlag: 0, CellBuffer: [0,0], CellPosition: [0,0]}
                        ENDIF
                      ENDIF
                    ENDELSE
                    self.DrawImage, /PRESERVEVIEWPLANE
    END
    'Define Base' : BEGIN
                      prop = Widget_Info(event.top, FIND_BY_UNAME = 'defineMaskPropSheet')
                      Widget_Control, prop, SCR_XSIZE = event.x, SCR_YSIZE = event.y
    END
    'Add Mask'    : BEGIN
                      masks = self.mask.maskObjects.get(/ALL, ISA = 'as_maskobject', COUNT = maskNo)
                      maskNames = list()
                      FOREACH mask, masks DO maskNames.add, mask.name
                      maskObject = as_maskobject()
                      newName = ''
                      i=1
                      WHILE newName EQ '' DO BEGIN
                        newName = 'Mask ' + StrCompress(maskNo+i)
                        IF maskNames.where(newName) NE !NULL THEN newName = ''
                        i++
                      ENDWHILE
                      maskObject.SetProperty, fillOpacity = 0.5, NAME = newName, maskType = 1
                      self.mask.maskObjects.add, maskObject
                      tabBase = Widget_Base(self.mask.wMaskTab, TITLE = maskObject.name, /ROW, UNAME='Mask Tab ' + StrCompress(maskNo,/REMOVE_ALL) )
                      defineMaskPropertySheet = Widget_PropertySheet(tabBase,  VALUE = maskObject, ysize = 17, EVENT_PRO = 'AS_DefineMaskPropertyEvent', UNAME = 'defineMaskPropSheet') 
                      defineMaskTable = Widget_Table(tabBase, XSIZE = 2, YSIZE = self.mask.numPoints*2, SCR_XSIZE = 196, COLUMN_WIDTHS = 60, /ALL_EVENTS, /EDITABLE, /CONTEXT_EVENTS, ALIGNMENT = 2, UVALUE = {CellChangedFlag: 0, CellBuffer: [0,0], CellPosition: [0,0]}, UNAME='Mask Table ' + StrCompress(maskNo,/REMOVE_ALL))
                      Widget_Control, defineMaskTable, SCR_YSIZE = (Widget_Info(defineMaskTable,/ROW_HEIGHTS))[0] * (self.mask.numPoints+2)
                      Widget_Control, self.mask.wMaskTab, SET_TAB_CURRENT = maskNo + 1
                      self.SetSelected, maskNo
                      
    END
    'Delete Mask Base' : BEGIN
                      masks = self.mask.maskobjects.get(/all, isa='as_maskobject')
                      maskNames = List()
                      FOREACH mask, masks DO maskNames.add, mask.name
                      wMaskDeleteBase = Widget_Base(GROUP_LEADER = event.top, /COLUMN, /MODAL)
                      wMaskNameList = Widget_List(wMaskDeleteBase, YSIZE = maskNames.count(), /MULTIPLE, VALUE = maskNames.toArray(), UNAME = 'MASK NAME LIST')
                      wDeleteMask = Widget_Button(wMaskDeleteBase, VALUE = 'Delete Mask(s)', EVENT_PRO = 'AS_DefineMask_event', UNAME = 'DELETE MASKS')
                      Widget_Control, wMaskDeleteBase, /REALIZE
                      Widget_Control, wMaskDeleteBase, SET_UVALUE = self
    END
    'DELETE MASKS' : BEGIN
                       wListID = Widget_Info(event.top, FIND_BY_UNAME = 'MASK NAME LIST')
                       selected = Widget_Info(wListID, /LIST_SELECT)
                       IF N_Elements(selected) EQ 1 THEN result = Dialog_Message('Are you sure you want to delete this mask? This mask will also become unavailable in other configs for this experiment file.', /QUESTION) $
                                        ELSE result = Dialog_Message('Are you sure you want to delete these masks? These masks will also become unavailable in other configs for this experiment file.', /QUESTION)
                       IF result EQ 'No' THEN RETURN
                       masks = self.mask.maskobjects.get(/all, isa='as_maskobject')
                       Obj_Destroy, masks[selected]
                       numDeleted = 0
                       FOR maskNo=0, N_Elements(masks)-1 DO BEGIN
                         tabID = Widget_Info(self.mask.defineMaskBase, FIND_BY_UNAME = 'Mask Tab ' + StrCompress(maskNo,/REMOVE_ALL))
                         tableID = Widget_Info(self.mask.defineMaskBase, FIND_BY_UNAME = 'Mask Table ' + StrCompress(maskNo,/REMOVE_ALL))
                         IF Where(maskNo EQ selected, /NULL) NE !NULL THEN BEGIN
                           numDeleted ++
                           Widget_Control, tabID, /DESTROY
                         ENDIF ELSE BEGIN
                           Widget_Control, tabID, SET_UNAME = 'Mask Tab ' + StrCompress(maskNo-numDeleted,/REMOVE_ALL)
                           Widget_Control, tableID, SET_UNAME = 'Mask Table ' + StrCompress(maskNo-numDeleted,/REMOVE_ALL)
                         ENDELSE
                       ENDFOR
                       self.SetSelected, -1
                       Widget_Control, event.top, /DESTROY
                       RETURN
                       
    END
    'Apply Masks' : self.ApplyMasks
    ELSE          :
  ENDCASE
  
  FOREACH m, self.mask.maskObjects.Get(/ALL) DO BEGIN
    IF Obj_Valid(m) THEN m.SetProperty, BEAMPOSITION=[self.frame.xc, self.frame.yc]
  ENDFOREACH
  
  IF self.mask.DefiningMask EQ 1 THEN BEGIN
    IF Widget_Info(event.id, /UNAME) EQ 'FRAME_DRAW' THEN BEGIN
      geom = Widget_Info(event.id, /GEOMETRY)
      self.frame.frameviewobj->GetProperty,VIEWPLANE_RECT=view
      x = Round(view[2]*(event.x/geom.xsize) + view[0])
      y = Round(view[3]*(event.y/geom.ysize) + view[1])
    
      IF ISA(scatterEvent,'STRUCT') THEN BEGIN
        IF N_Elements(scatterEvent.x) NE 0 THEN x = Round(scatterEvent.x)
        IF N_Elements(scatterEvent.y) NE 0 THEN y = Round(scatterEvent.y)
      ENDIF

      IF self.mask.movingMask.flag GE 1 THEN BEGIN
        IF event.type EQ 1 THEN BEGIN
            Widget_Control, Widget_Info(self.mask.defineMaskBase, FIND_BY_UNAME='defineMaskPropSheet'), /REFRESH_PROPERTY
            self.UpdateTable, self.mask.selectedMask
            Widget_Control, event.id, DRAW_MOTION_EVENTS =0
            self.mask.movingMask.flag = 0
            self.refreshpropertysheet
        ENDIF
        IF event.type EQ 2 THEN BEGIN
          self.mask.selectedMask.GetProperty, MASKSHAPE=maskShape
          IF maskShape EQ 0 THEN BEGIN
            self.mask.selectedMask.GetProperty, DATA=data
            data = ([x,y]-self.mask.movingMask.start)#Replicate(1,N_Elements(data[0,*])) + data
            self.mask.selectedMask.SetProperty, DATA=data  
          ENDIF ELSE BEGIN
            CASE self.mask.movingMask.flag OF
                1 : BEGIN
                      self.mask.selectedMask.GetProperty, CENTREX=centreX, CENTREY=centreY
                      centre = [centreX, centreY]+([x,y]-self.mask.movingMask.start)
                      self.mask.selectedMask.SetProperty, CENTREX=centre[0], CENTREY=centre[1]
                    END
                2 : BEGIN
                      self.mask.selectedMask.GetProperty, RADIUSMIN=radiusMin, RADIUSMax=radiusMax
                      radiusChange = SqRt(x^2+y^2)-SqRt(self.mask.movingMask.start[0]^2+self.mask.movingMask.start[1]^2)
                      IF radiusMin GT 0 THEN radiusMin = radiusMin + radiusChange > 0
                      IF radiusMax GT 0 THEN radiusMax = radiusMax + radiusChange > 0
                      self.mask.selectedMask.SetProperty, RADIUSMIN=radiusMin, RADIUSMax=radiusMax
                    END
                4 : BEGIN
                      self.mask.selectedMask.GetProperty, RADIUSMIN=radiusMin, RADIUSMax=radiusMax
                      radiusDelta = SqRt(x^2+y^2)-SqRt(self.mask.movingMask.start[0]^2+self.mask.movingMask.start[1]^2)
                      radiusMin = radiusMin - radiusDelta > 0
                      radiusMax = radiusMax + radiusDelta > 0
                      self.mask.selectedMask.SetProperty, RADIUSMIN=radiusMin, RADIUSMax=radiusMax
                    END
                ELSE :
            ENDCASE
          ENDELSE
          self.mask.movingMask.start = [x,y]
        
        ENDIF
      ENDIF
      IF event.press GT 0 AND (event.modifiers EQ 0 OR event.modifiers EQ 8) THEN BEGIN
        IF event.modifiers EQ 0 THEN BEGIN
          maskObjects = self.mask.maskObjects.Get(/ALL, ISA = 'as_maskobject', COUNT = numMasks)
          IF numMasks EQ 0 THEN RETURN ELSE table = Widget_Info(self.mask.defineMaskBase, FIND_BY_UNAME='Mask Table ' + StrCompress(Where(self.mask.selectedMask EQ maskObjects),/REMOVE_ALL))
          IF event.press EQ 2 THEN BEGIN
            self.mask.selectedMask.GetProperty, Data=data
            rSquared = [1,1]#([data[0,*] - x, data[1,*] - y])^2
            result = min(rSquared, minPos)
            (self.mask.selectedMask).SelectVertex, minPos
            Widget_Control, table, SET_TABLE_SELECT = [0,minPos,1,minPos]
            Widget_Control, table, SET_TABLE_VIEW = [0,0]
          ENDIF ELSE BEGIN
            IF event.press EQ 1 THEN BEGIN
              self.mask.selectedMask.MoveVertex, [x,y], /ADD
              self.mask.selectedMask.GetProperty, Selectedvertex = Vertex
              self.mask.lastVertex.add, {mask : self.mask.selectedMask, vertex : vertex}
            ENDIF ELSE self.mask.selectedMask.MoveVertex, [x,y]
            self.UpdateTable
          ENDELSE
        ENDIF
        IF event.press GE 1 AND event.modifiers EQ 8 AND event.type EQ 0 THEN BEGIN
          maskNum = 0
          pixel = x + self.frame.nxpix*(y)
          
          FOREACH maskObject, self.mask.maskObjects.Get(/ALL, ISA = 'as_maskobject') DO BEGIN
            maskObject.GetProperty, DATA=data
            polyvec = PolyFillV(data[0,*], data[1,*],self.frame.nxpix,self.frame.nypix)
            IF Where(polyvec EQ pixel) GE 0 THEN BEGIN
              self.SetSelected, maskNum
              BREAK
            ENDIF
            maskNum++
          ENDFOREACH

          Widget_Control, event.id, /DRAW_MOTION_EVENTS
          self.mask.movingMask.flag  = event.press
          self.mask.movingMask.start = [x,y]
        ENDIF
      ENDIF
    ENDIF
    self.PolyChanged
  ENDIF    

END

PRO AS_MaskObj::RefreshPropertySheet

  @as_scatterheader.macro

  IF self.mask.defineMaskBase EQ 0 THEN RETURN
  Widget_Control, Widget_Info(self.mask.defineMaskBase, FIND_BY_UNAME='Beamstop Property'), /REFRESH_PROPERTY
  tabs = Widget_Info(self.mask.wMaskTab,/ALL_CHILDREN)
  maskObjects = self.mask.maskObjects.get(/ALL, ISA = 'as_maskobject')
  FOREACH tab, tabs DO BEGIN
    sheet = Widget_Info(tab, FIND_BY_UNAME='defineMaskPropSheet')
    IF sheet GT 0 THEN Widget_Control, sheet, /REFRESH_PROPERTY
    ;Widget_Control, sheet, SET_VALUE = maskObjects[key]
  ENDFOREACH
  print, 'refresh'

END

PRO AS_MaskObj::NewParams, paramObj, CONFIG = config, MASKONLY=maskOnly 

  @as_scatterheader.macro

  IF ~Keyword_Set(maskOnly) THEN self->AS_FrameObj::NewParams, paramObj, CONFIGNO = config
  
  IF Keyword_Set(config) THEN beamstop = (mask = config)
  
  paramObj->GetParameters, MASK=mask, BEAMSTOP = beamstop

  IF Size(mask, /TYPE) EQ 8 THEN self.mask.maskdef = Ptr_New(mask) ELSE self.mask.maskdef = Ptr_New(/ALLOCATE_HEAP)
  
  self.mask.beamStop.SetProperty, masktype = beamstop.masktype, radius = beamstop.radius, angle = beamstop.angle, width = beamstop.width, offsetX = beamstop.offsetx, offsetY = beamstop.offsety
  
  self->AddDefinedMasks
  
  self.mask.beamStop.SetProperty, BEAMPOSITION=[self.frame.xc, self.frame.yc]

  self->RefreshPropertySheet

  self->UpdateMaskArray
  
END

PRO AS_MaskObj::UpdateMaskArray

  @as_scatterheader.macro

  mask = FltArr(self.frame.nxpix,self.frame.nypix)                                    ; initial mask array is zeroed
  mask[*]=0
  nmsk = 0

  IF self.mask.maskObjects.count() LE 0 THEN BEGIN
    *self.mask.mask = (mask[*] = 1)
    RETURN
  ENDIF
  
  ; Apply user including masks (masks from #2 onwards)
  ;
  FOREACH maskObject, self.mask.maskObjects.Get(/ALL) DO BEGIN
      maskObject.GetProperty, MASKTYPE=type
      IF type EQ 2 THEN BEGIN
          maskObject.GetProperty, DATA=data
          polyvec = PolyFillV(data[0,*], data[1,*],self.frame.nxpix,self.frame.nypix)
          nmsk = nmsk+1

;          IF N_Elements(polyvec) LT 3 THEN BEGIN
;              maskstr = ['Border','BeamStop','#1','#2','#3','#4','#5','#6','#7','#8']
;              retmes = Dialog_Message('Problem with '+maskstr(i)+' masking polygon')
;              RETURN,0
;          ENDIF

          mask[polyvec]=1
      ENDIF
  ENDFOREACH

  ; If no user including masks were applied - include all pixels in frame
  IF nmsk EQ 0 THEN mask[*]=1

  ; Create and apply outer frame exclusion area from polygon mask #0 (if #0 is active)
;  IF self.mask.type[0] EQ 1 THEN BEGIN
;      tempmask = Replicate(0,self.frame.nxpix,self.frame.nypix)
;      npt = self.mask.npts[0]-1
;      tempmask[PolyFillV(self.mask.x[0,0:npt], self.mask.y[0,0:npt],self.frame.nxpix,self.frame.nypix)] = 1
;      mask = mask * tempmask
;  ENDIF

  ; Now apply user excluding masks from #1 (the beam stop mask) onwards
  ;
  FOREACH maskObject, self.mask.maskObjects.Get(/ALL) DO BEGIN
      maskObject.GetProperty, MASKTYPE=type
      IF type EQ 1 THEN BEGIN
          maskObject.GetProperty, DATA=data
          IF N_Elements(data) LT 6 THEN CONTINUE
          polyvec = PolyFillV(data[0,*], data[1,*],self.frame.nxpix,self.frame.nypix)
          nmsk = nmsk+1

;          IF N_Elements(polyvec) LT 3 THEN BEGIN
;              maskstr = ['Border','BeamStop','#1','#2','#3','#4','#5','#6','#7','#8']
;              retmes = Dialog_Message('Problem with '+maskstr(i)+' masking polygon')
;              RETURN,0
;          ENDIF

          mask[polyvec]=0
      ENDIF
  ENDFOREACH
  
  
;  FOR i = 1,N_Elements(self.mask.type)-1 DO BEGIN      ; apply user excluding masks
;      IF (self.mask.type[i] EQ 2) AND (self.mask.npts[i] GT 2) THEN BEGIN
;          npt = self.mask.npts[i]-1
;          polyvec = PolyFillV(self.mask.x[i,0:npt], self.mask.y[i,0:npt],self.frame.nxpix,self.frame.nypix)

;          IF N_Elements(polyvec) LT 3 THEN BEGIN
;              maskstr = ['Border','BeamStop','#1','#2','#3','#4','#5','#6','#7','#8']
;              retmes = Dialog_Message('Problem with '+maskstr(i)+' masking polygon')
;              RETURN,0
;          ENDIF

;          mask[polyvec]=0
;      ENDIF
;  ENDFOR

  *self.mask.mask = mask

END

PRO AS_MaskObj__Define

;maskdef = { maskdef, name: '', type: 0, shape: 0, auto: 0, params: Ptr_New() }
movingMask = {movingMask, flag: 0, start: [0.,0.]}

mask = { AS_MaskObj_Struc,            $ ; CURRENT MASK POLYGON DEFINITIONS
          mask:             Ptr_New(),      $
          nmouse:           0,              $ ; Current vertex point from mouse
          current:          2,              $ ; Current mask being defined
          zoom:             3,              $ ; Current zoom used in mask define window
          xlow:             0,              $ ; Lowest x value shown in draw frame
          ylow:             0,              $ ; Lowest y value shown in draw frame
          numPoints:        25,             $ ; Default number of points for a mask
          maskdef:          Ptr_New(),      $
          maskObjects:      Obj_New(),      $
          wMaskTab    :      0l,      $
          beamStop:         Obj_New(),      $ ; Beamstop mask.
          definingMask:     0,              $ ;Flag indicating that a mask is being defined using the cursor.
          defineMaskBase  : 0L,             $
          movingMask      : movingMask,     $
          selectedMask    : Obj_New(),      $
          oldMaskPoly     : Ptr_New(),      $
          nameChanges     : Ptr_New(),      $
          lastVertex      : List(),         $
          notify          : List()          $
;          type:       intarr(10),     $ ; Type code: 0=unused, 1=inclusive, 2=exclusive
;          shape:      intarr(10),     $ ; Shape code: 0=general, 1=circle, 2=sector, 3=qz, 4=qy
;          auto:       intarr(10),     $ ;=1 to automatically update when beam center changes
;          npts:       intarr(10),     $ ; Current selected mask
;          x:          fltarr(10,25),  $ ; (i,j) i vectors of up to j x-coordinates
;          y:          fltarr(10,25),  $ ; (i,j) i vectors of up to j y-coordinates
;          cirx:       fltarr(10),     $ ; x-center of circle mask
;          ciry:       fltarr(10),     $ ; y-center of circle mask
;          cirr:       fltarr(10),     $ ; radius of circle mask
;          sangle:     fltarr(10),     $ ; start angle if polygon is a beam-centered sector
;          fangle:     fltarr(10),     $ ; finish angle if polygon is a beam-centered sector
 ;         qz:         fltarr(10),     $ ; Qz for constant qz integration
 ;         qzw:        fltarr(10),     $ ; Qz integration width
 ;         qy:         fltarr(10),     $ ; Qy for constant qy integration
 ;         qyw:        fltarr(10)      $ ; Qy integration width
       }

void = { AS_MaskObj, $
         INHERITS AS_FrameObj,       $
         mask : mask $
         }

END
