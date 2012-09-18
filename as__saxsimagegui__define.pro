PRO  as__saxsimagegui_event, event

  Widget_Control, event.handler, GET_UVALUE = as__saxsimagegui
  as__saxsimagegui->as__saxsimagegui::event, event

END

PRO as__saxsimagegui::event, event

widgetName = Widget_Info(event.id, /UNAME)

CASE widgetName OF
      'FRAME_DRAW' :  BEGIN
              
                              IF self.profileLineActive EQ 1 THEN BEGIN
                                self->Overlay_Line, DATA = data
                                data[1,*] = [event.x,event.y]
                                self->Overlay_Line, DATA = data
                              ENDIF
                              IF event.type EQ 4 THEN self->DrawImage, /PRESERVE
                      
                              IF event.press EQ 1 AND event.modifiers EQ 1 THEN BEGIN
                                ;self.frame.frameWinObj->GetProperty, DIMENSIONS = winDims
                                ;self.frame_obj->GetProperty, DIMENSIONS = imgDims
                                geom = Widget_Info(event.id, /GEOMETRY)
                                self.frame.frameviewobj->GetProperty,VIEWPLANE_RECT=view
                                ;self.frame_obj->qClick, event.x*imgDims[0]/winDims[0], event.y*imgDims[1]/winDims[1], self.frameModel_Obj, self.frameView_Obj, self.frame.frameWinObj
                                self->qClick, view[2]*(event.x/geom.xsize) + view[0], view[3]*(event.y/geom.ysize) + view[1]
                                self.frame.frameWinObj->Draw
                              ENDIF
                      
                              IF self.findingCentre GT 0 THEN BEGIN
                                IF event.press EQ 1 THEN BEGIN
                                  geom = Widget_Info(event.id, /GEOMETRY)
                                  self.frame.frameviewobj->GetProperty,VIEWPLANE_RECT=view
                                  x = view[2]*(event.x/geom.xsize) + view[0] 
                                  y = view[3]*(event.y/geom.ysize) + view[1]
                                  self.centreing[self.findingCentre - 1:self.findingCentre] = [x, y]
                                  self.findingCentre = self.findingCentre+2
                                  IF self.findingCentre Gt 5 THEN self.findingCentre = 1
                                  IF Obj_Valid(self.centreingPoints) THEN self.centreingPoints->SetProperty, DATA = Transpose([[self.centreing[0], self.centreing[2], self.centreing[4]], [self.centreing[1],self.centreing[3], self.centreing[5]]]) ELSE BEGIN
                                    self.centreingPoints = Obj_New('IDLgrPolyline', [self.centreing[0], self.centreing[2], self.centreing[4]], [self.centreing[1],self.centreing[3], self.centreing[5]], LINESTYLE = 6, SYMBOL = self.centreingSymbol)
                                    self.frameModel_obj->Add, self.centreingPoints
                                    self.frame.framewinobj->draw
                                  ENDELSE
                                ENDIF
                                IF event.press EQ 4 THEN BEGIN
                                  testZero = [self.centreing[0]+self.centreing[1],self.centreing[2]+self.centreing[3],self.centreing[4]+self.centreing[5]]
                                  IF min(testZero) EQ 0 THEN BEGIN
                                    self.message_obj.AddMessage, 'RingArc', "You haven't selected 3 points, continue clicking!"
                                  ENDIF ELSE BEGIN
                                    self->fitCircle, self.centreing                
                                    self.centreing = [0,0,0,0,0,0]
                                    self.findingCentre = 0
                                    self.message_obj.DeleteMessage, 'RingArc'
                                    Obj_Destroy, self.centreingPoints
                                  ENDELSE
                                ENDIF
                              ENDIF
                      
                              IF self.boxActive EQ 1 THEN BEGIN
                                geom = Widget_Info(event.id, /GEOMETRY)
                                self.frame.frameviewobj->GetProperty,VIEWPLANE_RECT=view
                                x = view[2]*(event.x/geom.xsize) + view[0] 
                                y = view[3]*(event.y/geom.ysize) + view[1]
                                self.box_obj->GetProperty, DATA = DATA
                                DATA[0,2:3] = x
                                DATA[1,0] = y
                                DATA[1,3:4] = y
                                self.box_obj->SetProperty, DATA = DATA
                                self.frame.frameWinObj->Draw
                              ENDIF
                              IF event.press EQ 1 AND event.modifiers EQ 2 THEN BEGIN
                                geom = Widget_Info(event.id, /GEOMETRY)
                                self.frame.frameviewobj->GetProperty,VIEWPLANE_RECT=view
                                x = view[2]*(event.x/geom.xsize) + view[0]
                                y = view[3]*(event.y/geom.ysize) + view[1]
                                Widget_Control, event.id, /DRAW_MOTION_EVENTS
                                DATA = Transpose([[x,x,x,x,x],[y,y,y,y,y]])
                                self.box_obj->SetProperty, DATA = DATA
                                self.boxActive = 1
                              ENDIF
                              IF event.release EQ 1 AND self.boxActive EQ 1 THEN BEGIN
                                geom = Widget_Info(event.id, /GEOMETRY)
                                self.box_obj->GetProperty,DATA=data
                                self->GetProperty,DIMENSIONS=dims
                                
                                xSize = Max(data[0,*])-Min(data[0,*])
                                ySize = Max(data[1,*])-Min(data[1,*])
                                
                                selXSize = xSize > ySize*(dims[0]/dims[1])
                                selYSize = ySize > xSize*(dims[1]/dims[0])
                                
                                self.frame.frameviewobj->SetProperty,VIEWPLANE_RECT=[Min(data[0,*]),Min(data[1,*]),selXSize, selYSize]
                                Widget_Control, event.id, DRAW_MOTION_EVENTS=0
                                DATA = Transpose([[0,0,0,0,0],[0,0,0,0,0]])
                                self.box_obj->SetProperty, DATA = DATA
                                self.boxActive = 0
                                self.frame.frameWinObj->Draw
                              ENDIF
                              IF event.press EQ 4 AND event.modifiers EQ 2 THEN BEGIN
                                self->GetProperty,DIMENSIONS=dims
                                self.frame.frameviewobj->SetProperty,VIEWPLANE_RECT=[0,0,dims[0],dims[1]]
                                self.frame.frameWinObj->Draw
                              ENDIF
      
                
      ;                      IF event.press EQ 1 THEN BEGIN
      ;                        self.frame_obj->Overlay_Line, DATA = Transpose([[event.x,event.x],[event.y,event.y]])
      ;                        Widget_Control, event.id, /DRAW_MOTION_EVENTS
      ;                        self.profileLineActive = 1
      ;                      ENDIF
      ;                      IF event.release EQ 1 THEN BEGIN
      ;                        Widget_Control, event.id, DRAW_MOTION_EVENTS = 0
      ;                        self.profileLineActive = 0
      ;                      ENDIF
                             self.frame.framewinobj->draw
              
            END
              
            ; *********** TAB BAR EVENTS ***************************************
              
            'CONTROL_TAB': 
              
            'Transmission SAXS' : BEGIN
                                    self.cake.type = 0
                                    self.cake.ok = 0
                                  END  
            'In Plane SAXS'  : BEGIN
                                self.cake.type = 2
                                self.cake.ok = 0
                              END
            'Out Of Plane SAXS' : BEGIN 
                                    self.cake.type = 1
                                    self.cake.ok = 0
                                  END
            
            'FINDRINGARC': BEGIN
                                                                
                             self.frame.frameViewobj->GetProperty, VIEWPLANE_RECT=view
                             self.message_obj.SetProperty, LOCATIONS = [0,view[3]-20,0]
                             self.message_obj.AddMessage, 'RingArc', 'LEFT click three points on ring, then RIGHT click'
                             self.frame.frameWinObj->Draw
                             self.findingCentre = 1
                                                                
                           END
            'FIND 4 SECTORS': BEGIN
                                self.Find4Sectors
                              END
            'AUTO FIND 4 SECTORS' :
            'X BEAM CENTRE':BEGIN
                              self.frame.xc = event.value
                              self.UpdateBeamCursor
                              self.cake.ok = 0
                              self.cake.newmask = 1
                              temp = self->CakeSetup()
                              IF Widget_Info(Widget_Info(event.top, FIND_BY_UNAME = 'AUTO FIND 4 SECTORS'), /BUTTON_SET) THEN self.Find4Sectors
                            END

            'Y BEAM CENTRE':BEGIN
                              self.frame.yc = event.value
                              self.UpdateBeamCursor
                              self.cake.ok = 0
                              self.cake.newmask = 1
                              temp = self->CakeSetup()
                              IF Widget_Info(Widget_Info(event.top, FIND_BY_UNAME = 'AUTO FIND 4 SECTORS'), /BUTTON_SET) THEN self.Find4Sectors
                            END

            'X BEAM LEFT' : BEGIN
                      
                              self.frame.xc--
                              Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='X BEAM CENTRE'), SET_VALUE = self.frame.xc
                              self.UpdateBeamCursor
                              self.cake.ok = 0
                              self.cake.newmask = 1
                              temp = self->CakeSetup()
                              IF Widget_Info(Widget_Info(event.top, FIND_BY_UNAME = 'AUTO FIND 4 SECTORS'), /BUTTON_SET) THEN self.Find4Sectors
            END
            'X BEAM RIGHT' : BEGIN
                      
                              self.frame.xc++
                              Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='X BEAM CENTRE'), SET_VALUE = self.frame.xc
                              self.UpdateBeamCursor
                              self.cake.ok = 0
                              self.cake.newmask = 1
                              temp = self->CakeSetup()
                              IF Widget_Info(Widget_Info(event.top, FIND_BY_UNAME = 'AUTO FIND 4 SECTORS'), /BUTTON_SET) THEN self.Find4Sectors
                      
            END
            'Y BEAM DOWN' : BEGIN
                      
                              self.frame.yc--
                              Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='Y BEAM CENTRE'), SET_VALUE = self.frame.yc
                              self.UpdateBeamCursor
                              self.cake.ok = 0
                              self.cake.newmask = 1
                              temp = self->CakeSetup()
                              IF Widget_Info(Widget_Info(event.top, FIND_BY_UNAME = 'AUTO FIND 4 SECTORS'), /BUTTON_SET) THEN self.Find4Sectors
                      
            END
            'Y BEAM UP' : BEGIN
                      
                              self.frame.yc++ 
                              Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='Y BEAM CENTRE'), SET_VALUE = self.frame.yc
                              self.UpdateBeamCursor
                              self.cake.ok = 0
                              self.cake.newmask = 1
                              temp = self->CakeSetup()
                              IF Widget_Info(Widget_Info(event.top, FIND_BY_UNAME = 'AUTO FIND 4 SECTORS'), /BUTTON_SET) THEN self.Find4Sectors
                      
            END
            'SAVE CONFIG' : BEGIN
                              Widget_Control, Widget_Info(self.imageGUIBase, FIND_BY_UNAME='CONFIG COMBO'), GET_VALUE = value
                              configName = Widget_Info(Widget_Info(self.imageGUIBase,  FIND_BY_UNAME='CONFIG COMBO'), /COMBOBOX_GETTEXT)
                              IF configName EQ 'New...' OR configName EQ '' THEN BEGIN
                                configName = TextBox(Title = 'Enter Config Name', GROUP_LEADER = self.imageGUIBase, LABEL = 'Config Name:')
                                IF configName EQ '' THEN RETURN
                              ENDIF
                              index = Where((self.configNames).toArray() EQ configName, nElements)
                              IF nElements GT 1 THEN BEGIN
                                numConfigsMessage = Dialog_Message('Multiple configs appear to have the same name. Probably a bug, using first.')
                                index = index[0]
                              ENDIF
                              IF index[0] EQ -1 THEN BEGIN
                                index = N_Elements(self.configNames)
                                (self.configNames).Add, configName
                                Widget_Control, Widget_Info(self.imageGUIBase,  FIND_BY_UNAME='CONFIG COMBO'), COMBOBOX_ADDITEM = configName
                              ENDIF
                              self.currentConfig = index[0]
                              self.profiles_obj.StoreParams, self.frame.logObj, CONFIGNO = index
                              self.frame.logObj.SetParameters, CONFIGNO = index, CONFIGNAME = configName
                              self.StoreParams, self.frame.logObj, CONFIG = index
                              self.SaveCakeLUT, index
                              Widget_Control, Widget_Info(self.imageGUIBase, FIND_BY_UNAME='PARAM LABEL'), SET_VALUE = configName
                              self.notify, {CONFIG, event: 'save'}
                            END
            'LOAD CONFIG' : BEGIN
                              configName = Widget_Info(Widget_Info(self.imageGUIBase,  FIND_BY_UNAME='CONFIG COMBO'), /COMBOBOX_GETTEXT)
                              IF configName EQ 'New...' OR configName EQ '' THEN RETURN
                              index = Where((self.configNames).toArray() EQ configName)
                              IF index[0] EQ -1 THEN RETURN
                              self.currentConfig = index[0]
                              self.profiles_obj.NewParams, self.frame.logObj, index
                              self.AS_Maskobj::NewParams, self.frame.logObj, CONFIG = index
                              Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='X BEAM CENTRE'), SET_VALUE = self.frame.xc
                              Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='Y BEAM CENTRE'), SET_VALUE = self.frame.yc
                              self.LoadCakeLUT, index
                              Widget_Control, Widget_Info(self.imageGUIBase, FIND_BY_UNAME='PARAM LABEL'), SET_VALUE = configName
                            END
            'SAVE SLOT': BEGIN
                             
                            ; Widget_Control, event.id, GET
                              
;                            Widget_Control, event.id, GET_UVALUE=slotNum
;                            loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(event.top,FIND_BY_UNAME='NormTab'), FIND_BY_UNAME='SAVE SLOT'),/PARENT),/ALL_CHILDREN)
;                            Widget_Control, loadSlots[slotNum], GET_VALUE=oldName
;                            slotName = textbox(GROUP_LEADER=event.top, LABEL='Config Name:', TITLE='Enter name for configuration.', VALUE = oldName)
;                            Widget_Control, loadSlots[slotNum], SET_VALUE = slotName
;                            loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(event.top,FIND_BY_UNAME='QRangeTab'), FIND_BY_UNAME='SAVE SLOT'),/PARENT),/ALL_CHILDREN)
;                            Widget_Control, loadSlots[slotNum], SET_VALUE = slotName
;                            loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(event.top,FIND_BY_UNAME='NormTab'), FIND_BY_UNAME='LOAD SLOT'),/PARENT),/ALL_CHILDREN)
;                            Widget_Control, loadSlots[slotNum], SET_VALUE = slotName
;                            loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(event.top,FIND_BY_UNAME='QRangeTab'), FIND_BY_UNAME='LOAD SLOT'),/PARENT),/ALL_CHILDREN)
;                            Widget_Control, loadSlots[slotNum], SET_VALUE = slotName
                            
                            self.StoreParams, self.frame.logObj, CONFIG = slotNum
                            self->SaveCakeLUT, slotNum
                              
            END
            'LOAD SLOT': BEGIN
                            Widget_Control, event.id, GET_UVALUE=slotNum
                            
;                            loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(event.top,FIND_BY_UNAME='NormTab'), FIND_BY_UNAME='SAVE SLOT'),/PARENT),/ALL_CHILDREN)
;                            FOR i = 0, N_Elements(loadSlots) - 1 DO BEGIN
;                              Widget_Control, loadSlots[i], SET_BUTTON = i EQ slotNum
;                            ENDFOR
;                            loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(event.top,FIND_BY_UNAME='QRangeTab'), FIND_BY_UNAME='SAVE SLOT'),/PARENT),/ALL_CHILDREN)
;                            FOR i = 0, N_Elements(loadSlots) - 1 DO BEGIN
;                              Widget_Control, loadSlots[i], SET_BUTTON = i EQ slotNum
;                            ENDFOR
;                            loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(event.top,FIND_BY_UNAME='NormTab'), FIND_BY_UNAME='LOAD SLOT'),/PARENT),/ALL_CHILDREN)
;                            FOR i = 0, N_Elements(loadSlots) - 1 DO BEGIN
;                              Widget_Control, loadSlots[i], SET_BUTTON = i EQ slotNum
;                            ENDFOR
;                            loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(event.top,FIND_BY_UNAME='QRangeTab'), FIND_BY_UNAME='LOAD SLOT'),/PARENT),/ALL_CHILDREN)
;                            FOR i = 0, N_Elements(loadSlots) - 1 DO BEGIN
;                              Widget_Control, loadSlots[i], SET_BUTTON = i EQ slotNum
;                            ENDFOR
                            
                            self->LoadCakeLUT, slotNum
                            
                            IF Obj_Valid(self.frame.beamCursor) THEN self.frame.beamCursor->SetProperty, DATA = Transpose([[0,self.frame.nxpix,self.frame.xc,self.frame.xc,self.frame.xc],[self.frame.yc,self.frame.yc,self.frame.yc,0,self.frame.nypix]]) ELSE BEGIN
                              self.frame.beamCursor = Obj_New('IDLgrPolyline', [0,self.frame.nxpix,self.frame.xc,self.frame.xc,self.frame.xc],[self.frame.yc,self.frame.yc,self.frame.yc,0,self.frame.nypix], LINESTYLE = 2, COLOR = 255)  
                              self.PARENT->Add, self.frame.beamCursor
                            ENDELSE
                            
                            IF Obj_Valid(self.mask.maskObjects) THEN BEGIN
                              IF Widget_Info(Widget_Info(event.top, FIND_BY_UNAME='SHOW UNFILLED MASKS'), /BUTTON_SET) THEN unFill = 1 ELSE unFill = 0
                              self->ShowMasks, UNFILLED = unFill
                            ENDIF

                            self.frame.frameWinObj->Draw
                            
            END
            'SHOW MASKS': BEGIN
                            IF event.select THEN BEGIN
                              self->ShowMasks
                              self.frame.frameWinObj->Draw
                            ENDIF
            END
            'SHOW UNFILLED MASKS': BEGIN
                            IF event.select THEN BEGIN
                              self->ShowMasks, /UNFILLED
                              self.frame.frameWinObj->Draw
                            ENDIF
            END
            'MASK OPACITY' : BEGIN
                                IF Widget_Info(Widget_Info(event.top, FIND_BY_UNAME='SHOW UNFILLED MASKS'),/BUTTON_SET) THEN unfilled = 1 ELSE unfilled = 0
                                self->ShowMasks, OPACITY = event.value, UNFILLED = unfilled
                                self.frame.frameWinObj->Draw
            END
            'HIDE MASKS': BEGIN
                            self->ShowMasks, /CLEAR
                            self.frame.frameWinObj->Draw
            END
            'DEFINE MASKS' : BEGIN
                            self->DefineMasks, event.select 
            END
                  ; ***********  FRAME DISPLAY EVENTS
            
            'HIST_BUT' : BEGIN
                         IF event.select THEN BEGIN
                            self->HistDisplay, self.wScatterBase, self.frame.frameWinObj, self.frame.frameViewobj, hist_button = self.wHistBut
    
                         ENDIF ELSE self->HistHide
                         
            END
            
            'AUTO_BUT' : BEGIN
                         IF event.select THEN BEGIN
                            self->SetProperty, AUTOSCALE = 1
                            self.frame.frameWinObj->Draw
                         ENDIF ELSE self->SetProperty, AUTOSCALE = 0
                         
            END
            'ADD_RING' : BEGIN
                        Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='RING_TEXT'), GET_VALUE = qRadius
                        self->OverLay_QCirc, qRadius
                        self.profiles_obj->AddQMarker, qRadius
            END
            'DELETE_RING' : BEGIN
                        Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='RING_TEXT'), GET_VALUE = qRadius
                        self->Delete_QCirc, qRadius
                        self.frame.frameWinObj->Draw
                        self.profiles_obj->DeleteQMarker, qRadius
            END
            'DELETE_ALL_RINGS' : BEGIN
                        self->Delete_QCirc, -1
                        self.frame.frameWinObj->Draw
                        self.profiles_obj->DeleteQMarker, -1
            END
            
            'POWER_LAW' : BEGIN
                        self->PowerLawScale, event.value 
                        self.frame.frameWinObj->Draw
            END
            'Min Intensity' : BEGIN
                                Widget_Control, Widget_Info(self.imageGuiBase, FIND_BY_UNAME='AUTO_BUT'), SET_VALUE = 0
                                IF event.value GT self.frame.histXzoom[1] OR event.value LT self.frame.histXZoom[0] THEN BEGIN
                                  minMax = Widget_Info(event.id, /SLIDER_MIN_MAX)
                                  self.frame.histObj.zoomexternal, /OUT
                                  self.frame.histObj.setthresholdexternal, min = minMax[0], max = minMax[1]
                                  self.frame.histObj.zoomexternal
                                ENDIF
                                self.AS_FrameObj::SetProperty, autoscale = 0, histMin = event.value
            END
            'Max Intensity' : BEGIN
                                Widget_Control, Widget_Info(self.imageGuiBase, FIND_BY_UNAME='AUTO_BUT'), SET_VALUE = 0
                                IF event.value GT self.frame.histXzoom[1] OR event.value LT self.frame.histXzoom[0] THEN BEGIN
                                  minMax = Widget_Info(event.id, /SLIDER_MIN_MAX)
                                  self.frame.histObj.zoomexternal, /OUT
                                  self.frame.histObj.setthresholdexternal, min = minMax[0], max = minMax[1]
                                  self.frame.histObj.zoomexternal
                                ENDIF
                                self.AS_FrameObj::SetProperty, autoscale = 0, histMax = event.value
            END
            ELSE        : Print, 'Nothing happening here ...'
      
      ENDCASE
      self->as_saxsimagetools::event, event
END

PRO as__saxsimagegui::hist, xstruct

  IF xstruct.hist_win GT 0 THEN BEGIN
    Widget_Control, Widget_Info(self.imageGuiBase, FIND_BY_UNAME='Min Intensity'), SET_VALUE = xstruct.minThresh
    Widget_Control, Widget_Info(self.imageGuiBase, FIND_BY_UNAME='Max Intensity'), SET_VALUE = xstruct.maxThresh
    Widget_Control, self.wHistBut, SET_VALUE = xstruct.visible
  ENDIF
  
  self.AS_FrameObj::hist, xstruct

END

FUNCTION as__saxsimagegui::GetImage, fname, _REF_EXTRA = extra

  result = self.AS_FrameObj::GetImage(fname, _EXTRA = extra)
  IF result LT 0 THEN return, -1
  max = Max(*self.frame.rawData * *self.mask.mask,MIN=min)
  Widget_Control, Widget_Info(self.imageGuiBase, FIND_BY_UNAME='Min Intensity'), SET_SLIDER_MIN = min, SET_SLIDER_MAX = max
  Widget_Control, Widget_Info(self.imageGuiBase, FIND_BY_UNAME='Max Intensity'), SET_SLIDER_MIN = min, SET_SLIDER_MAX = max
  saturated = self.CheckSaturation()
  IF saturated THEN self.message_obj.AddMessage, 'Saturated','Detector likely saturated.' ELSE self.message_obj.DeleteMessage, 'Saturated'
  self.DrawImage, /PRESERVEVIEWPLANE
  
  RETURN, result

END

PRO as__saxsimagegui::Find4Sectors

  fname = self.frame.fname
  profileData = self->Sectors(fname, 4, /CURRENTIMAGE, /NODISPLAYMASK)
  FOREACH profile, profileData, key DO BEGIN
    self.frame.fname = 'Sector ' + StrCompress(key,/REMOVE_ALL)
    oldProfileRefIndex = self.profiles_obj.findByFName(self.frame.fname)
    IF N_Elements(profile.profile) GT 1 THEN self.notify, {FRAMEPLOTREQ, name: self.frame.fname, q_arr: List(profile.q_arr, /EXTRACT), profile: List(profile.profile, /EXTRACT), error: List(profile.error, /EXTRACT)}
    IF oldProfileRefIndex GE 0 THEN self.Profiles_obj->ReplaceWithLast, oldProfileRefIndex
  ENDFOREACH
  self.profiles_obj.ReOrgColours
  self.frame.fname = fname

END

;PRO as__saxsimagegui::AddProfile, q_arr, data, error, fname, _REF_EXTRA = extra
;
;  configName = (self.configNames)[self.currentConfig]
;  self.cake.profileObj->AddProfile, q_arr, data, error, fname, CONFIGNAME = configName, _EXTRA = extra
;
;END

PRO as__saxsimagegui::NewParams, paramObj
  
  self->as_saxsimagetools::NewParams, paramObj
  
  ;Todo At the moment this is only called when an XML is opened. What would happen if called at other time? I think combobox would get screwed up.
  paramObj->GetParameters, FRAME=frame
  nullName = Where(frame.confname EQ '')
  IF nullName[0] GE 0 THEN (frame[nullname].confname) = ' ' 
  Widget_Control, Widget_Info(self.imageGUIBase, FIND_BY_UNAME = 'CONFIG COMBO'), SET_VALUE= ['New...', frame.confName] 
  Widget_Control, Widget_Info(self.imageGUIBase, FIND_BY_UNAME = 'CONFIG COMBO'), SET_COMBOBOX_SELECT = 1
  Widget_Control, Widget_Info(self.imageGUIBase, FIND_BY_UNAME = 'PARAM LABEL'), SET_VALUE = (frame.confName)[0]
  
  self.configNames.add, frame.confName, /EXTRACT

  FOR i = 0, N_Elements(frame) - 1 DO BEGIN
;    loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(self.imageGUIBase,FIND_BY_UNAME='NormTab'), FIND_BY_UNAME='SAVE SLOT'),/PARENT),/ALL_CHILDREN)
;    Widget_Control, loadSlots[i], SET_VALUE = (frame.confName)[i]
;    loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(self.imageGUIBase,FIND_BY_UNAME='QRangeTab'), FIND_BY_UNAME='SAVE SLOT'),/PARENT),/ALL_CHILDREN)
;    Widget_Control, loadSlots[i], SET_VALUE = (frame.confName)[i]
;    loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(self.imageGUIBase,FIND_BY_UNAME='NormTab'), FIND_BY_UNAME='LOAD SLOT'),/PARENT),/ALL_CHILDREN)
;    Widget_Control, loadSlots[i], SET_VALUE = (frame.confName)[i]
;    loadSlots = Widget_Info(Widget_Info(Widget_Info(Widget_Info(self.imageGUIBase,FIND_BY_UNAME='QRangeTab'), FIND_BY_UNAME='LOAD SLOT'),/PARENT),/ALL_CHILDREN)
;    Widget_Control, loadSlots[i], SET_VALUE = (frame.confName)[i]
    
    self.cake.cakeMemory[i].camlength  = (frame.len)[i]
    self.cake.cakeMemory[i].wavelength = (frame.wlen)[i]
    self.cake.cakeMemory[i].xc         = (frame.xc)[i]
    self.cake.cakeMemory[i].yc         = (frame.yc)[i]

  ENDFOR

  Widget_Control, Widget_Info(self.imageGUIBase, FIND_BY_UNAME='X BEAM CENTRE'), SET_VALUE = self.frame.xc
  Widget_Control, Widget_Info(self.imageGUIBase, FIND_BY_UNAME='Y BEAM CENTRE'), SET_VALUE = self.frame.yc
  
  self.frame.frameViewObj->SetProperty, VIEWPLANE_RECT = [0,0,self.frame.nxpix,self.frame.nypix]

END

FUNCTION as__saxsimagegui::Read

  RETURN, self.frame.frameWinObj.read()

END

PRO as__saxsimagegui::SetProperty, AUTOSCALE = autoScale, DEFINEMASKS = defineMasks, _REF_Extra = extra
  
  IF N_Elements(autoScale) THEN BEGIN
    Widget_Control, Widget_Info(self.imageGUIBase, FIND_BY_UNAME = 'AUTO_BUT'), SET_VALUE = autoScale
    self.AS_CAKEOBJ::SetProperty, AUTOSCALE = autoScale, _EXTRA = extra
  ENDIF ELSE self.AS_CAKEOBJ::SetProperty, _EXTRA = extra
  IF N_Elements(defineMasks) EQ 1 THEN BEGIN
    Widget_Control, Widget_Info(self.imageGUIBase, FIND_BY_UNAME = 'DEFINE MASKS'), SET_VALUE = defineMasks
  ENDIF
  
  
END

PRO as__saxsimagegui::GetProperty, CONFIGNAME = configName, _REF_Extra = extra

  IF Arg_Present(configName) THEN BEGIN
    IF N_Elements(self.configNames) GE self.currentConfig + 1 THEN configName = (self.configNames)[self.currentConfig] $
                                                              ELSE configName = ''
  ENDIF

  self.as_saxsimagetools::GetProperty, _EXTRA = extra

END

FUNCTION as__saxsimagegui::init, base, qData, profileObj, RESOURCE=resource, _REF_Extra = extra

  self.profiles_obj = profileObj
  
  self.configNames = list()
  
  imageGUIBase = Widget_Base(base, /COLUMN)
  self.frame.group_leader = base
  Widget_Control, imageGUIBase, SET_UVALUE = self
  
  imageGUITop  = Widget_Base(imageGUIBase, /ROW)
  
    fdraw = Widget_Draw(imageGUITop, UNAME='FRAME_DRAW', /BUTTON_EVENTS, /EXPOSE_EVENTS, SCR_XSIZE=512, SCR_YSIZE=512,GRAPHICS_LEVEL=2)

    imageGUIRight = Widget_Base(imageGUITop,/COLUMN)
    fancyFrameBase = Widget_Base(imageGUIRight)
    configControlLabel = Widget_Label(fancyFrameBase, VALUE = ' Configuration Selector ', FONT = 'Arial*16*Bold', XOFFSET = 2)
    frameConfigBase = Widget_Base(fancyFrameBase,/COLUMN,/FRAME,YOFFSET=10,YPAD=8)
    paramBase = Widget_Base(frameConfigBase,/BASE_ALIGN_CENTER,/COLUMN)
    selectedLabel = Widget_Label(paramBase, VALUE = 'Current Config: ')
    paramLabel = Widget_Label(paramBase, VALUE = 'New...', /DYNAMIC_RESIZE, UNAME = 'PARAM LABEL', FONT = 'Arial*15*bold')
    paramBaseRow = Widget_Base(frameConfigBase,/BASE_ALIGN_CENTER,/ROW)
    paramComboBase = Widget_Base(paramBaseRow,/BASE_ALIGN_CENTER,/COLUMN)
    definedLabel = Widget_Label(paramComboBase, VALUE = 'Defined Configs: ')
    paramCombo = Widget_Combobox(paramComboBase,XSIZE =100,VALUE = 'New...',/EDITABLE, UNAME = 'CONFIG COMBO')
    paramButBase = Widget_Base(paramBaseRow, /COLUMN)
    paramSaveButton = Widget_Button(paramButBase, VALUE='Save', FONT='Arial*12',YSIZE=15,UNAME = 'SAVE CONFIG')
    paramLoadButton = Widget_Button(paramButBase, VALUE='Load', FONT='Arial*12',YSIZE=15,UNAME = 'LOAD CONFIG')
    
    fancyFrameBase = Widget_Base(imageGUIRight)
    controlTabLabel = Widget_Label(fancyFrameBase, VALUE = ' Control Tabs ', FONT='Arial*16*bold', XOFFSET = 2)
    tabBase = Widget_Base(fancyFrameBase,/COLUMN,/FRAME,YOFFSET=10,YPAD=8)
    wBotScatterColTab = Widget_Tab(tabBase, UNAME='CONTROL_TAB',/MULTILINE)
      
      wBotScatterCol1 = Widget_Base(wBotScatterColTab,TITLE='Details', /COLUMN)
      datadir = CW_FIELD(wBotScatterCol1, xsize=20, /NOEDIT, /COLUMN, TITLE = 'Data Directory' $
                ,VALUE = '',UNAME='DDIR')
      lfile = CW_FIELD(wBotScatterCol1,xsize=20,/NOEDIT, /COLUMN, TITLE = 'Normalization Log File' $
                ,VALUE = '',UNAME='LOGFILE')
      paramfile = CW_FIELD(wBotScatterCol1,xsize=20,/NOEDIT, /COLUMN, TITLE = 'Param File' $
               ,VALUE = '',UNAME='PARAMFILE')
      tmstmp = CW_FIELD(wBotScatterCol1,xsize=20,/NOEDIT, /COLUMN, TITLE = 'Time Stamp ' $
          ,VALUE = '',UNAME='TIMESTMP')
     ;      ,VALUE = profdata.timestamp[0],UNAME='TIMESTMP')
      cntdwn = CW_FIELD(wBotScatterCol1, xsize=15, /NOEDIT, /COLUMN $
               , TITLE = 'Exposure Secs', VALUE = 0,UNAME='PROFTIME')
      iocnts = CW_FIELD(wBotScatterCol1, xsize=15,/NOEDIT, /LONG, /COLUMN, TITLE = 'Io Counts' $
              ,VALUE = 0,UNAME='IOCOUNTS')
      bscnts = CW_FIELD(wBotScatterCol1,xsize=15,/NOEDIT, /LONG, /COLUMN, TITLE = 'B.Stop Counts' $
              ,VALUE = 0,UNAME='BSCOUNTS')
      transmis = CW_FIELD(wBotScatterCol1,xsize=15,/NOEDIT, /FLOAT, /COLUMN, TITLE = 'Transmission' $
              ,VALUE = 0,UNAME='TRANSMIS')
      sftotal = CW_FIELD(wBotScatterCol1,xsize=15,/NOEDIT, /FLOAT, /COLUMN, TITLE = 'Scaling Factor' $
              ,VALUE = 0,UNAME='SFTOTAL')


      wBotScatterCol2 = Widget_Base(wBotScatterColTab,TITLE='QRange', UNAME = 'QRangeTab', /COLUMN)
   
        wCakeTypeBase = Widget_Base(wBotScatterCol2, /EXCLUSIVE, /COLUMN)
        wTransmission = Widget_Button(wCakeTypeBase, VALUE = 'Transmission SAXS', UNAME = 'Transmission SAXS')
        wInPlane = Widget_Button(wCakeTypeBase, VALUE = 'In Plane GISAXS', UNAME = 'In Plane SAXS')
        wOutOfPlane = Widget_Button(wCakeTypeBase, VALUE = 'Out Of Plane GISAXS', UNAME = 'Out Of Plane SAXS')
   
        IF N_Elements(resource) GT 0 THEN BEGIN
          leftIn = resource.GetResource('leftIn')
          leftOut = resource.GetResource('leftOut')
          rightIn = resource.GetResource('rightIn')
          rightOut = resource.GetResource('rightOut')
        ENDIF
   
        wFindCentreBut = Widget_Label(wBotScatterCol2, VALUE = 'Find Centre')
        wFindRingArc = Widget_Button(wBotScatterCol2, VALUE = 'Find From Ring/Arc', UNAME = 'FINDRINGARC')
        wFind4Sectors = Widget_Button(wBotScatterCol2, VALUE = 'Find From 4 Sectors', UNAME = 'FIND 4 SECTORS')
        wFindNonExBase = Widget_Base(wBotScatterCol2, /COLUMN, /NONEXCLUSIVE)
        wFind4SectorsAuto = Widget_Button(wFindNonExBase, VALUE = 'Auto Update 4 Sectors', UNAME = 'AUTO FIND 4 SECTORS')
        wXCentreLabel = Widget_Label(wBotScatterCol2, VALUE = 'X Beam Centre')
        xCentreBase = Widget_Base(wBotScatterCol2, /ROW, XSIZE = 135, /BASE_ALIGN_CENTER)
        wLeftXButton = drawButton(xCentreBase, leftin, leftout, scale = 0.45, /SENSITIVE, UNAME='X BEAM LEFT')
        wXCentreField = CW_FIELD(xCentreBase, /FLOAT, TITLE='', XSIZE = 10, /COLUMN, /RETURN_EVENTS, UNAME = 'X BEAM CENTRE')
        wRightXButton = drawButton(xCentreBase, rightin, rightout, scale = 0.45, /SENSITIVE, UNAME='X BEAM RIGHT')
        wYCentreLabel = Widget_Label(wBotScatterCol2, VALUE = 'Y Beam Centre')
        yCentreBase = Widget_Base(wBotScatterCol2, /ROW,XSIZE = 135, /BASE_ALIGN_CENTER)
        wDownYButton = drawButton(yCentreBase, leftin, leftout, scale = 0.45, /SENSITIVE, UNAME='Y BEAM DOWN')
        wYCentreField = CW_FIELD(yCentreBase, /FLOAT, TITLE='', XSIZE = 10, /COLUMN, /RETURN_EVENTS, UNAME = 'Y BEAM CENTRE')
        wUpYButton = drawButton(yCentreBase, rightin, rightout, scale = 0.45, /SENSITIVE, UNAME='Y BEAM UP')
      wBotScatterCol3 = Widget_Base(wBotScatterColTab,TITLE='Norm', UNAME = 'NormTab',/COLUMN)
   
;  wSaveParameters = LonArr(2)
;  wLoadParameters = LonArr(2)
;   
;  wSaveParameters[0] = Widget_Button(wBotScatterCol2, value = 'Save Param',/MENU)
;  wSaveParameters[1] = Widget_Button(wBotScatterCol3, value = 'Save Param',/MENU)
;  
;  wLoadParameters[0] = Widget_Button(wBotScatterCol2, value = 'Load Param',/MENU)
;  wLoadParameters[1] = Widget_Button(wBotScatterCol3, value = 'Load Param',/MENU)
;   
;  FOR i = 0, 1 DO BEGIN
;    FOR j = 0, 9 DO BEGIN
;      wParamSlot = Widget_Button(wSaveParameters[i], VALUE = 'Slot: ' + StrCompress(String(j)),/CHECKED_MENU, UNAME = 'SAVE SLOT', UVALUE = j)
;      wLoadSlot = Widget_Button(wLoadParameters[i], VALUE = 'Slot: ' + StrCompress(String(j)),/CHECKED_MENU, UNAME = 'LOAD SLOT', UVALUE = j)
;    ENDFOR
;  ENDFOR

    wBotScatterCol4 = Widget_Base(wBotScatterColTab, TITLE='Masks', UNAME = 'MasksTab',/COLUMN)
      wShowHideMaskBase = Widget_Base(wBotScatterCol4, /EXCLUSIVE, /COLUMN)
        wShowMasks = Widget_Button(wShowHideMaskBase, VALUE='Show Masks', UNAME = 'SHOW MASKS')
        wShowFilledMasks = Widget_Button(wShowHideMaskBase, VALUE='Show Masks (Unfilled)', UNAME = 'SHOW UNFILLED MASKS')
        wHideMasks = Widget_Button(wShowHideMaskBase, VALUE='Hide Masks', UNAME = 'HIDE MASKS')
        Widget_Control, wShowMasks, SET_BUTTON = 1
      wMaskOpacity = Widget_Slider(wBotScatterCol4, TITLE='Mask Opacity', MINIMUM = 0, MAXIMUM = 100, UNAME = 'MASK OPACITY',/DRAG)
      wDefineMasks = CW_BGROUP(wBotScatterCol4, 'Define Masks', /NONEXCLUSIVE, UNAME = 'DEFINE MASKS')

    wBotScatterCol5 = Widget_Base(wBotScatterColTab, TITLE='Image', UNAME = 'ImageTab',/COLUMN)
      self.wHistBut = CW_BGroup(wBotScatterCol5, 'Show Histogram', SET_VALUE=0, UNAME = 'HIST_BUT', /NONEXCLUSIVE, /COLUMN)
      wAutoScaleBut = CW_BGroup(wBotScatterCol5, 'Auto Scale', UNAME = 'AUTO_BUT', SET_VALUE = 1, /NONEXCLUSIVE, /COLUMN)
      wPowerLaw = CW_Field(wBotScatterCol5, VALUE = '0.0', UNAME = 'POWER_LAW', TITLE = 'Power Law', /FLOATING, XSIZE = 5, /RETURN_EVENTS)
      wRingBase = Widget_Base(wBotScatterCol5,/ROW,/BASE_ALIGN_CENTER)
      wRingButBase = Widget_Base(wRingBase,/COLUMN)
      wAddRingBut = Widget_Button(wRingButBase, VALUE='Add Ring', UNAME = 'ADD_RING',YSIZE = 15)
      wDeleteRingBut = Widget_Button(wRingButBase, VALUE='Delete Ring', UNAME = 'DELETE_RING',YSIZE = 15)
      wRingText = CW_Field(wRingBase, VALUE = '0.0', UNAME = 'RING_TEXT', TITLE = '',/FLOATING, XSIZE = 5, /COLUMN)
      wDeleteAllRingBut = Widget_Button(wBotScatterCol5, VALUE='Delete All Rings', UNAME = 'DELETE_ALL_RINGS',YSIZE = 15)
      wMinSlider = Widget_Slider(wBotScatterCol5, TITLE = 'Min', VALUE = 0, MINIMUM = 0, MAXIMUM = 100, /DRAG, UNAME = 'Min Intensity')
      wMaxSlider = Widget_Slider(wBotScatterCol5, TITLE = 'Max', VALUE = 100, MINIMUM = 0, MAXIMUM = 100, /DRAG, UNAME = 'Max Intensity')

  Widget_Control, base, /REALIZE
  
  ; Create frame palette
  palette = Obj_New('IDLgrPalette')
  palette->LoadCT,0
  
  self->SetProperty, PALETTE=palette
  
  ; Create frame object from draw widget
  Widget_Control, fdraw, GET_VALUE = frameWin
  self.frame.frameWinObj = frameWin
    
  ; Create box object for setting zoom on frame.
  DATA = Transpose([[0,0,0,0,0],[0,0,0,0,0]])
  self.box_obj = Obj_New('IDLgrPolyline',DATA,COLOR=[255,255,0])
  
  ;Create symbol object used for displaying clicked points for centreing routine.
  self.centreingSymbol = Obj_New('IDLgrSymbol', SIZE = 5)
  
  ; Create text object for displaying messages on image
  self.message_obj = Obj_New('as_messageObj', COLOR = [255,255,0], VERTICAL_ALIGNMENT = 0)
  
  ; Create rest of graphics hierarchy and associate with each other.
  self.frameModel_obj = Obj_New('IDLgrModel')
  self.frame.frameViewobj = Obj_New('IDLgrView')
  self.frameModel_obj->Add, self
  self.frameModel_obj->Add, self.box_obj
  self.frameModel_obj->Add, self.message_obj
  self.frame.frameViewobj->Add, self.frameModel_obj
  self.frame.frameWinObj->SetProperty, GRAPHICS_TREE = self.frame.frameViewobj
    
  XManager, 'as__saxsimagegui', imageGUIBase, /NO_BLOCK
    
  self.imageGUIBase = imageGUIBase
    
  RETURN, self->AS_SaxsImageTools::Init(qData, profileObj,_Extra=extra)

END

PRO as__saxsimagegui__define

  void = { as__saxsimagegui,           $
           INHERITS as_saxsimagetools, $
           ; Objects
           frameModel_obj     : Obj_New(), $
           message_obj        : Obj_New(), $
           centreingSymbol    : Obj_New(), $
           centreingPoints    : Obj_New(), $
           box_obj            : Obj_New(), $
           profiles_obj       : Obj_New(), $
           ; Flags
           profileLineActive  : 0        , $
           findingCentre      : 0        , $
           boxActive          : 0        , $
           ; Widgets
           imageGUIBase       : 0L       , $
           wScatterBase       : 0L       , $
           wHistBut           : 0L       , $
           ; Parameters
           centreing          : FltArr(6), $
           configNames        : List(),  $
           currentConfig      : 0 $
         }

END
