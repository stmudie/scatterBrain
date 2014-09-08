PRO as_profilecontainerobj_baseevent, event

  @as_scatterheader.macro
  
  Widget_Control, event.top, GET_UVALUE=as_profilecontainerobj
  as_profilecontainerobj->baseEvent, event
END

PRO as_profilecontainerobj_drawevent, event
  
  @as_scatterheader.macro

  Widget_Control, Widget_Info(event.id, /PARENT), GET_UVALUE=as_profilecontainerobj
  as_profilecontainerobj->drawEvent, event
END

PRO AS_ProfileContainerObj::baseEvent, event

  @as_scatterheader.macro

  widgetName = Widget_Info(event.id, /UNAME)

  CASE widgetName OF 
    'Base'        : BEGIN
                      IF Tag_Names(event,/STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BREAK
                      children = Widget_Info(event.top, /ALL_CHILDREN)
                      whereNotMenu = Where(Widget_Info(children,/UNAME) NE 'menuBar')
                      children = children[whereNotMenu]
                      xAux = Total((Widget_Info(children,/GEOM)).xsize) - self.drawSize[0]
                      x = event.x - xAux
                      y = event.y
                      self->ReSize, x, y
                      self->NotifyObject, {type: 'Resize', x: x, y: y}
                    END
   'SAVE SINGLE'  : self.SaveProfiles
   'SAVE MULTIPLE': self.SaveProfiles, /MULTIPLE
   'SAVE PLOT IMAGE' : self.SavePlotImage
   'Norm'         : BEGIN
                      IF ~Obj_Valid(self.normGUI) THEN self.normGUI = as_normalisation(GROUPLEADER=self.wBase, NOTIFY_OBJ = [{OBJECT:self, METHOD:'SetNormParams'},{OBJECT:self, METHOD:'UpdatePlot'}])
                      self.normGUI->showGUI
                      self.normGUI.DetCounts, self.ionrm, self.ibsnrm, 1
                    END
   'Abs Cal'      : BEGIN
                      IF ~Obj_Valid(self.absGUI) THEN self.absGUI = as_abscal(GROUPLEADER=self.wBase, NOTIFY_OBJ = [notify('SetNormParams',self),notify('UpdatePlot',self)])
                      self.absGUI->showGUI
                      self.absGUI.DetCounts, self.ionrm, self.ibsnrm, 1, /APPLIED
                      self.absGUI.SetCurrentCalibration, self.cscalib, USE = self.useCalib
                    END
   'Q Calibration': BEGIN
;                      IF ~Obj_Valid(self.qCalibGUI) THEN BEGIN
;                        Widget_Control, self.groupleader, GET_UVALUE = scatterBrain
;                        self.qCalibGUI = scatterBrain.qCalibGUI(GROUPLEADER=self.wBase);, NOTIFY_OBJ = [{OBJECT:self, METHOD:'SetQCalibParams'},{OBJECT:self, METHOD:'UpdatePlot'}])
;                      ENDIF
;                      self.qCalibGUI->showGUI
                      
                      Widget_Control, event.id, GET_UVALUE = detectorNo
                      Widget_Control, self.groupleader, GET_UVALUE = scatterBrain
                      
                      CASE detectorNo OF
                        0: scatterBrain.qCalibGUI.showGui
                        1: scatterBrain.qCalibGUI2.showGui
                      ENDCASE 

                      
                      ;self.qCalibGUI.SetProperty, WAVELENGTH = ,CAMERALENGTH = 
                    END
   'Help'         : BEGIN
                      Widget_Control, self.groupleader, GET_UVALUE = scatterBrain
                      scatterBrain.help, 'Plot_Control' 
                    END
  ENDCASE               

END

PRO AS_ProfileContainerObj::drawEvent, event

  @as_scatterheader.macro
  
  IF event.type NE 4 THEN BEGIN
    self.profileXAxis->GetProperty, LOCATION=location, CRANGE=crange, XCOORD_CONV=xcoord_conv
    geom = Widget_Info(event.id, /GEOMETRY)
    dataX = ((event.x/geom.xsize)-xcoord_conv[0])/xcoord_conv[1]
    IF self.xLog THEN dataX = 10^dataX
    x = event.x/geom.xsize
    self.profileYAxis->GetProperty, LOCATION=location, CRANGE=crange, YCOORD_CONV=ycoord_conv
    dataY = ((event.y/geom.ysize)-ycoord_conv[0])/ycoord_conv[1]                           
    IF self.yLog THEN dataY = 10^dataY
    y = event.y/geom.ysize
  
    IF event.modifiers EQ 1 THEN BEGIN
      IF event.type EQ 0 THEN BEGIN 
        Widget_Control, self.groupLeader, GET_UVALUE=info, /NO_COPY
                    
          expo = 10.0d^(3 -1 - floor(alog10(abs(dataX))))
          q = long(dataX*expo)/expo
        
          self->AddQMarker, q
          info.frame_obj->Overlay_QCirc, q
        Widget_Control, self.groupLeader, SET_UVALUE=info, /NO_COPY
      ENDIF
    ENDIF ELSE BEGIN
   
      IF self.boxActive EQ 1 THEN BEGIN
        self.boxObj->GetProperty, DATA = DATA
        DATA[0,2:3] = x
        DATA[1,0] = y
        DATA[1,3:4] = y
        self.boxObj->SetProperty, DATA = DATA
        self.profileWindow->Draw, self.profileView
      ENDIF
      IF event.press EQ 1 THEN BEGIN
        Widget_Control, event.id, /DRAW_MOTION_EVENTS
        DATA = Transpose([[x,x,x,x,x],[y,y,y,y,y]])
        self.boxObj->SetProperty, DATA = DATA
        self.boxActive = 1
        self.xRangeZoom = [dataX,dataX] 
        self.yRangeZoom = [dataY,dataY]
      ENDIF
      IF event.release EQ 1 THEN BEGIN
        Widget_Control, event.id, DRAW_MOTION_EVENTS=0
        DATA = Transpose([[0,0,0,0,0],[0,0,0,0,0]])
        self.boxObj->SetProperty, DATA = DATA
        self.boxActive = 0
        self.xRangeZoom[1] = dataX
        self.yRangeZoom[1] = dataY
        self.xRangeZoom = [min(self.xRangeZoom),max(self.xRangeZoom)]
        self.yRangeZoom = [min(self.yRangeZoom),max(self.yRangeZoom)]
        IF self.xRangeZoom[0] EQ self.xRangeZoom[1] THEN RETURN
        IF self.yRangeZoom[0] EQ self.yRangeZoom[1] THEN RETURN
        
        self.xRange = self.xRangeZoom
        self.yRange = self.yRangeZoom
        
        self.profileXAxis->SetProperty, RANGE=self.xrangezoom
        self.profileYAxis->SetProperty, RANGE=self.yrangezoom
        self.profileXAxisGrid->SetProperty, RANGE=self.xrangezoom
        self.profileYAxisGrid->SetProperty, RANGE=self.yrangezoom
    
        self.profileXAxis->GetProperty, CRange=xrange
        self.profileYAxis->GetProperty, CRange=yrange
     
        ; Set up the scaling so that the axes for the plot and the
        ; plot data extends from 0->1 in the X and Y directions.

        xs = [((self.position[0]*xrange[1])-(self.position[2]*xrange[0])) / $
              (xrange[1]-xrange[0]), (self.position[2]-self.position[0])/(xrange[1]-xrange[0])]

        ys = [((self.position[1]*yrange[1])-(self.position[3]*yrange[0])) / $
              (yrange[1]-yrange[0]), (self.position[3]-self.position[1])/(yrange[1]-yrange[0])]

        ; Scale the plot data and axes into 0->1.
        IF Ptr_Valid(self.profileRefs) THEN IF N_Elements(*self.profileRefs) GT 0 THEN BEGIN
          FOR i = 0, N_Elements(*self.profileRefs) - 1 DO BEGIN 
            IF Obj_Valid((*self.profileRefs)[i].profilePlot) THEN (*self.profileRefs)[i].profilePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, XRANGE = xrange, YRANGE = yrange
          ENDFOR
        ENDIF
        self.livePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, XRANGE = xrange, YRANGE = yrange
        self.peakFit->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, XRANGE = xrange, YRANGE = yrange
        
        self.profileXAxis->SetProperty, XCoord_Conv=xs, MAJOR = -1, MINOR = 4 + self.xlog*4
        self.profileYAxis->SetProperty, YCoord_Conv=ys
        self.profileXAxisGrid->SetProperty, XCoord_Conv=xs, MAJOR = -1
        self.profileYAxisGrid->SetProperty, YCoord_Conv=ys
        self->LinLogQMarkers
      ENDIF
      IF event.press EQ 4 THEN BEGIN
    
          xranges = List()
          yranges = List()
    
          IF Ptr_Valid(self.profileRefs) THEN IF N_Elements(*self.profileRefs) GT 0 THEN BEGIN
            
            FOR i = 0, N_Elements(*self.profileRefs) - 1 DO BEGIN
               IF Obj_Valid((*self.profileRefs)[i].profilePlot) THEN BEGIN
                (*self.profileRefs)[i].profilePlot->GetProperty, HIDE = hide, THICK = thick
                data = (*self.profileRefs)[i].profiles->GetData(/BACK)
                IF N_Elements(data) EQ 1 and data[0] EQ -1 THEN RETURN
                xrange = [min(data[0,*]),max(data[0,*])]
                yrange = [min(data[1,*]),max(data[1,*])]
                IF self.xlog EQ 1 AND xrange[0] LE 0 THEN BEGIN
                  data[0,Where(data[0,*] LE 0)] = max(data[0,*])
                  xrange = [min(data[0,*]),max(data[0,*])]
                ENDIF  
                IF self.ylog EQ 1 AND yrange[0] LE 0 THEN BEGIN
                  data[1,Where(data[1,*] LE 0)] = max(data[1,*])
                  yrange = [min(data[1,*]),max(data[1,*])]
                ENDIF  
                xranges.add, xrange, /EXTRACT
                yranges.add, yrange, /EXTRACT
                Obj_Destroy, (*self.profileRefs)[i].profilePlot
                self->PlotProfile, i, /REPLOT,  XLOG = self.xLog, YLOG = self.yLog, _Extra=extra
                (*self.profileRefs)[i].profilePlot->SetProperty, HIDE = hide, THICK = thick
               ENDIF
            ENDFOR
          ENDIF
          IF Obj_Valid(self.livePlot) THEN BEGIN
            self.livePlot->GetProperty, HIDE = hide
            IF HIDE EQ 0 THEN BEGIN
              self.livePlot->GetProperty, DATA = data
              IF N_Elements(data) GT 0 THEN BEGIN
                xrange = [min(data[0,*]),max(data[0,*])]
                yrange = [min(data[1,*]),max(data[1,*])]
              ENDIF
              IF self.xlog EQ 1 AND xrange[0] LE 0 THEN BEGIN
                data[Where(data LE 0)] = max(data)
                xrange = [min(data[0,*]),max(data[0,*])]
              ENDIF  
              IF self.ylog EQ 1 AND yrange[0] LE 0 THEN BEGIN
                data[Where(data LE 0)] = max(data)
                yrange = [min(data[1,*]),max(data[1,*])]
              ENDIF  
              xranges.add, xrange, /EXTRACT
              yranges.add, yrange, /EXTRACT
            ENDIF
            self->PlotProfile, LIVE=1
            self.livePlot->SetProperty, HIDE = hide
          ENDIF
    
         IF Obj_Valid(self.peakFit) THEN BEGIN
            self.profileXAxis->GetProperty, CRange=xrange
            self.profileYAxis->GetProperty, CRange=yrange
    
            ; Set up the scaling so that the axes for the plot and the
            ; plot data extends from 0->1 in the X and Y directions.
  
            xs = [((self.position[0]*xrange[1])-(self.position[2]*xrange[0])) / $
                  (xrange[1]-xrange[0]), (self.position[2]-self.position[0])/(xrange[1]-xrange[0])]
          
            ys = [((self.position[1]*yrange[1])-(self.position[3]*yrange[0])) / $
                  (yrange[1]-yrange[0]), (self.position[3]-self.position[1])/(yrange[1]-yrange[0])]
          
              ; Scale the plot data and axes into 0->1.
            self.peakFit->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, XRANGE = xrange, YRANGE = yrange
        
         ENDIF
        IF xranges.count() EQ 0 THEN xrange = [0.01, 1] ELSE xrange = [min(xranges.toarray(),/NAN),max(xranges.toarray(),/NAN)]
        IF yranges.count() EQ 0 THEN yrange = [0.01, 1] ELSE yrange = [min(yranges.toarray(),/NAN),max(yranges.toarray(),/NAN)]
        
        IF Finite(yrange[0]) EQ 0 THEN yrange[0] = 0
        IF Finite(yrange[1]) EQ 0 THEN yrange[1] = yrange[0]
        
        IF Finite(xrange[0]) EQ 0 THEN xrange[0] = 0
        IF Finite(xrange[1]) EQ 0 THEN xrange[1] = xrange[0]
        
        IF xrange[0] EQ xrange[1] THEN xrange[1] = xrange[0]+1
        IF yrange[0] EQ yrange[1] THEN yrange[1] = yrange[0]+1
        
        self.xRangeZoom = xRange 
        self.yRangeZoom = yRange
        
        self.profileXAxis->SetProperty, RANGE=xrange
        self.profileYAxis->SetProperty, RANGE=yrange
        self.profileXAxisGrid->SetProperty, RANGE=xrange
        self.profileYAxisGrid->SetProperty, RANGE=yrange
    
        self.profileXAxis->GetProperty, CRange=xrange
        self.profileYAxis->GetProperty, CRange=yrange
     
       ; Set up the scaling so that the axes for the plot and the
       ; plot data extends from 0->1 in the X and Y directions.

        xs = [((self.position[0]*xrange[1])-(self.position[2]*xrange[0])) / $
              (xrange[1]-xrange[0]), (self.position[2]-self.position[0])/(xrange[1]-xrange[0])]

        ys = [((self.position[1]*yrange[1])-(self.position[3]*yrange[0])) / $
              (yrange[1]-yrange[0]), (self.position[3]-self.position[1])/(yrange[1]-yrange[0])]

        ; Scale the plot data and axes into 0->1.
        IF Ptr_Valid(self.profileRefs) THEN IF N_Elements(*self.profileRefs) GT 0 THEN BEGIN
          IF N_Elements(*self.profileRefs) GT 0 THEN BEGIN
            FOR i = 0, N_Elements(*self.profileRefs) - 1 DO BEGIN 
              IF Obj_Valid((*self.profileRefs)[i].profilePlot) THEN (*self.profileRefs)[i].profilePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, XRANGE = xrange, YRANGE = yrange
            ENDFOR
          ENDIF
        ENDIF
        self.livePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, XRANGE = xrange, YRANGE = yrange
        
        self.profileXAxis->SetProperty, XCoord_Conv=xs, MAJOR = -1
        self.profileYAxis->SetProperty, YCoord_Conv=ys
        self.profileXAxisGrid->SetProperty, XCoord_Conv=xs, MAJOR = -1
        self.profileYAxisGrid->SetProperty, YCoord_Conv=ys
        
        self->LinLogQMarkers
        
      ENDIF
    ENDELSE
  ENDIF
  self.profileWindow->Draw, self.profileView
    
END

FUNCTION AS_ProfileContainerObj::Init, GROUPLEADER=groupLeader, PLOTPALETTE = plotPalette, NOTIFY_OBJ = notifyObj, _REF_Extra=extra

  @as_scatterheader.macro

  IF KeyWord_Set(plotPalette) THEN self.plotPalette = plotPalette ELSE BEGIN
    profilePalette_obj = Obj_New('IDLgrPalette')
    profilePalette_obj->LoadCT, 34
  ENDELSE
  
  IF Keyword_Set(notifyObj) THEN $
    IF TypeName(notifyObj[0]) EQ 'NOTIFY' $
      THEN self.notifyObj = List(notifyObj,/EXTRACT)
      
  CD, CURRENT=current
  self.profilePath = current
  self.drawSize = [600,400]
  self.position = [0.15, 0.15, 0.925, 0.925]
  self.ionrm = 300000.0
  self.ibsnrm = 300000.0
  self.CSCalib = 1.0
  self.xLog = 0
  self.yLog = 0
  self.nrmType = 0
  self.useCalib = 0
  self.profileRefs  = Ptr_New()
  self.blankColour = [0,0,0]
  self.xRange = [0,0.001]
  self.yRange = [0,0.001]
  self.profileFont = Obj_New('IDLgrFont','Helvetica', SIZE = 10)
  self.xAxisTitle_obj = OBJ_NEW('IDLgrText','q (' + String("305B) + '!U-1!N)', FONT = self.profileFont, /ENABLE_FORMATTING)
  self.yAxisTitle_obj = OBJ_NEW('IDLgrText','Intensity Counts/s(Arb. Units)', FONT = self.profileFont, /ENABLE_FORMATTING)
  self.profileXAxis = Obj_New('IDLgrAxis', 0, RANGE = self.xrange, TICKLEN=0.03, NAME = 'X_AXIS', LOCATION=[1000,self.position[1],0], TITLE=self.xAxisTitle_obj, COLOR = [0,0,0], /EXACT)
  self.profileYAxis = Obj_New('IDLgrAxis', 1, RANGE = self.yrange, TICKLEN=0.03, NAME = 'Y_AXIS', LOCATION=[self.position[0],1000,0], TITLE=self.yAxisTitle_obj, COLOR = [0,0,0], /EXACT)
  self.profileXAxisGrid  = Obj_New('IDLgrAxis', 0, /NOTEXT, RANGE = self.xrange, TICKLEN=[self.position[2] - self.position[0]], SUBTICKLEN = 1, LOCATION=[1000,self.position[1],-1], COLOR = [200,200,200], /EXACT, GRIDSTYLE = 0)
  self.profileYAxisGrid  = Obj_New('IDLgrAxis', 1, /NOTEXT, RANGE = self.yrange, TICKLEN=[self.position[3] - self.position[1]], SUBTICKLEN = 1, LOCATION=[self.position[0],1000,-1], COLOR = [200,200,200], /EXACT, GRIDSTYLE = 0)
  
  messageFont = Obj_New('IDLgrFont','Helvetica')
  self.messageObj = IDLgrText('',FONT = messageFont, CHAR_DIMENSIONS = [0.01,0.01], COLOR = [0,0,0])
  
  self.liveProfile = AS__saxsprofileaddons(I0Norm=1)
  self.livePlot = IDLgrPlot()
  
  self.peakFit = IDLgrPlot(LINESTYLE = 1)
  self.peakFit.SetProperty, HIDE = 1
  
  self.powerPlot = IDLgrPlot()
  self.powerPlot.SetProperty, HIDE = 1
  self.powerPlot.Description = 50
  
    ; Because we may not be using exact axis ranging, the axes
    ; may extend further than the xrange and yrange. Get the
    ; actual axis range so that the plot, etc. can be scaled
    ; appropriately.

  self.profileXAxis->GetProperty, CRange=xrange
  self.profileYAxis->GetProperty, CRange=yrange
  self.xrange = xrange
  self.yrange = yrange
  
    ; Set up the scaling so that the axes for the plot and the
    ; plot data extends from 0->1 in the X and Y directions.

  xs = [((self.position[0]*self.xrange[1])-(self.position[2]*self.xrange[0])) / $
        (self.xrange[1]-self.xrange[0]), (self.position[2]-self.position[0])/(self.xrange[1]-self.xrange[0])]

  ys = [((self.position[1]*self.yrange[1])-(self.position[3]*self.yrange[0])) / $
        (self.yrange[1]-self.yrange[0]), (self.position[3]-self.position[1])/(self.yrange[1]-self.yrange[0])]

    ; Scale the plot data and axes into 0->1.

  self.profileXAxis->SetProperty, XCoord_Conv=xs
  self.profileXAxisGrid->SetProperty, XCoord_Conv=xs
  self.profileYAxis->SetProperty, YCoord_Conv=ys
  self.profileYAxisGrid->SetProperty, YCoord_Conv=ys
  
  self.profileXAxis->GetProperty, TICKTEXT = xAxisText_obj
  self.profileYAxis->GetProperty, TICKTEXT = yAxisText_obj
  
  self.xAxisText_obj = xAxisText_obj
  self.yAxisText_obj = yAxisText_obj
  self.xAxisText_obj->SetProperty, RECOMPUTE_DIMENSIONS=2
  self.yAxisText_obj->SetProperty, RECOMPUTE_DIMENSIONS=2
  self.xAxisTitle_obj->SetProperty, RECOMPUTE_DIMENSIONS=2
  self.yAxisTitle_obj->SetProperty, RECOMPUTE_DIMENSIONS=2
  
  self.legend = Obj_New('IDLgrLegend', FONT = self.profileFont, ITEM_COLOR = [0], COLUMNS = 3)
  modelInit = self->IDLgrModel::Init(_EXTRA=extra)
  self->Add, self.profileXAxis
  self->Add, self.profileYAxis
  self->Add, self.profileXAxisGrid
  self->Add, self.profileYAxisGrid
  self->Add, self.messageObj
  self->Add, self.legend
  self.legend->Translate, 0.5,0.8,0
  self->Add, self.livePlot
  self->Add, self.peakFit
  self->Add, self.powerPlot

  IF KeyWord_Set(groupLeader) THEN BEGIN
    self.groupLeader = groupLeader
    geom = Widget_Info(groupLeader,/GEOMETRY)
    self.wBase = Widget_Base(Group_Leader=groupLeader, TITLE = 'scatterBrain Plot', XOFFSET = geom.xoffset + geom.xsize + 2*geom.margin + 200, /ROW, /TLB_SIZE_EVENTS, /TLB_KILL_REQUEST_EVENTS, MBAR=wMenuBase, EVENT_PRO='as_profilecontainerobj_baseEvent', UNAME = 'Base')
  ENDIF ELSE self.wBase = Widget_Base(TITLE = 'scatterBrain Plot', /ROW, /TLB_SIZE_EVENTS, MBAR=wMenuBase, UNAME = 'Base')
  wDraw = Widget_Draw(self.wBase, XSIZE = self.drawSize[0], YSIZE = self.drawSize[1], GRAPHICS_LEVEL=2, /BUTTON_EVENTS, /EXPOSE_EVENTS, EVENT_PRO='as_profilecontainerobj_drawEvent')
  Widget_Control, wMenuBase, SET_UNAME = 'menuBar'
  wSaveMenu = Widget_Button(wMenuBase, /MENU, VALUE = 'Save Profiles')
  wSaveSingleFile = Widget_Button(wSaveMenu, VALUE = 'Save ALL profiles to ONE large file', UNAME = 'SAVE SINGLE')
  wSaveSingleFile = Widget_Button(wSaveMenu, VALUE = 'Save EACH profile to individual files', UNAME = 'SAVE MULTIPLE')
  wSaveImage = Widget_Button(wSaveMenu, VALUE = 'Save current plot as image', UNAME = 'SAVE PLOT IMAGE') 
  
  ;wNormMenu = Widget_Button(wMenuBase, /MENU, VALUE = 'Normalisation')
  ;wNormButton = Widget_Button(wNormMenu, VALUE = 'Normalisation', UNAME='Norm')
  wAbsMenu = Widget_Button(wMenuBase, /MENU, VALUE = 'Intensity Normalisation and Calibration')
  wAbsButton = Widget_Button(wAbsMenu, VALUE = 'Intensity Normalisation and Calibration', UNAME='Abs Cal')
  wQCalibMenu = Widget_Button(wMenuBase, /MENU, VALUE = 'Q Calibration')
  wQCalibButton = Widget_Button(wQCalibMenu, VALUE = 'Q Calibration Detector 1', UVALUE = 0, UNAME='Q Calibration')
  wQCalibButton = Widget_Button(wQCalibMenu, VALUE = 'Q Calibration Detector 2', UVALUE = 1, UNAME='Q Calibration')
  wHelpButton = Widget_Button(wMenuBase, /MENU, VALUE = 'Help');, UNAME = 'Help', EVENT_PRO='as_profilecontainerobj_baseEvent')
  wContextHelp= Widget_Button(wHelpButton, VALUE = 'Context Help', UNAME = 'Help')
    
  Widget_Control, self.wBase, /REALIZE
  Widget_Control, self.wBase, SET_UVALUE = self
  Widget_Control, wDraw, GET_VALUE = temp
    
  self.profileWindow = temp
  self.profileWindow->SetProperty, PALETTE=self.plotPalette
  self.profileView = Obj_New('IDLgrView', Viewplane_Rect=[0.0, 0.0, 1.0, 1.0], Location=[0,0], color = [255,255,255])
  
  DATA = Transpose([[50,50,100,100,50],[100,200,200,100,100]])
  self.boxObj = Obj_New('IDLgrPolyline',DATA)
  
  self->Add, self.boxObj
  self.profileView->Add, self
  self.profileWindow->Draw, self.profileView
  
  self.absGUI = as_abscal(GROUPLEADER=self.wBase, NOTIFY_OBJ = [notify('SetNormParams',self),notify('UpdatePlot',self)])
  
  RETURN, modelInit

END

PRO AS_ProfileContainerObj::Cleanup

  @as_scatterheader.macro

  Obj_Destroy, self.profileWindow
  Obj_Destroy, self.profileView
  Obj_Destroy, self.profileXAxis
  Obj_Destroy, self.profileYAxis
  Obj_Destroy, self.profileXAxisGrid
  Obj_Destroy, self.profileYAxisGrid
  Obj_Destroy, self.xAxisText_obj
  Obj_Destroy, self.yAxisText_obj
  Obj_Destroy, self.xAxisTitle_obj
  Obj_Destroy, self.yAxisTitle_obj
  Obj_Destroy, self.profileFont
  Obj_Destroy, self.legend
  Obj_Destroy, self.plotPalette
  Obj_Destroy, self.boxObj
  Obj_Destroy, self.liveProfile
  Obj_Destroy, self.livePlot
  Obj_Destroy, self.normGui

  Ptr_Free, self.profileRefs
  Ptr_Free, self.qMarkersObj
  Ptr_Free, self.qMarkersQ
  

END

PRO AS_ProfileContainerObj::NewParams, paramObj, configNo, ALL=all
  
  @as_scatterheader.macro

  IF N_Elements(configNo) GT 0 THEN normParams = configNo[0] ELSE normParams = 0
  paramObj.GetParameters, NORMPARAMS = normParams
  self.SetNormParams, normParams, NOUPDATEPLOTS=~KeyWord_Set(ALL)
  IF normParams.useAbsCal EQ 0 THEN self.setNormParams, {NORMTYPE : normParams.normType}, NOUPDATEPLOTS=~KeyWord_Set(ALL)
 

END

PRO AS_ProfileContainerObj::StoreParams, paramObj, CONFIGNO = config

  @as_scatterheader.macro

  paramObj->SetParameters, ABSCAL=self.CSCalib, USEABSCAL=self.usecalib, NORMTYPE = self.nrmType, I0Norm = self.IoNrm, IBSNORM = self.ibsnrm, CONFIGNO = config

END

FUNCTION AS_ProfileContainerObj::GetProfiles, plotIndex

  @as_scatterheader.macro

  data = list()
  FOREACH index, plotIndex DO BEGIN
    plotRef = Where((*self.profileRefs).refNum EQ index)
    data.add,(*self.profileRefs)[plotRef].profiles->GetData(/BACK, XLOG=0, YLOG=0)
  ENDFOREACH

  RETURN, data
  
END

PRO AS_ProfileContainerObj::SaveProfiles, fileName, MULTIPLE=multiple

  @as_scatterheader.macro
  
  IF N_Elements(fileName) EQ 0 THEN fileName = ''
  IF ~File_Test(fileName) THEN BEGIN
    IF KeyWord_Set(multiple) THEN fileName = Dialog_Pickfile(TITLE = 'Select directory to store profiles. Filenames will be built from image filenames.', /DIRECTORY, GET_PATH = tempPath, PATH = self.profilePath, /WRITE) $
                             ELSE fileName = Dialog_Pickfile(TITLE = 'Enter Filename...', /WRITE, /OVERWRITE_PROMPT, GET_PATH = tempPath, PATH = self.profilePath)
  ENDIF
  
  IF tempPath NE '' THEN self.profilePath = tempPath
  
  IF fileName EQ '' THEN RETURN
 
  profileList = list()
  nameList = list()
  maxElem = 0
    
  FOR profileIndex = 0, N_Elements(*self.profileRefs) - 1 DO BEGIN
    profileRef = (*self.profileRefs)[profileIndex]
    profileList.add, profileRef.profiles.GetData(/BACK, XLOG=0, YLOG=0)
    profileRef.profiles.GetProperty, fname=name
    nameList.add, name
    maxElem = maxElem > N_Elements(profileList[-1])/3
  ENDFOR
  numProfiles = N_Elements(profileList)
        
  titleList = list('q   ', 'I   ', 'Err   ')
    
  i = 0  
    
  IF KeyWord_Set(multiple) THEN BEGIN
  
    
    FOREACH name, nameList DO BEGIN
      
      OpenW, profileFile, fileName + StrMid(name,0,(strsplit(name, '.'))[-1]-1) + '.dat', /GET_LUN
        PrintF, profileFile, name
        PrintF, profileFile, titleList.toarray(), FORMAT = '(' + StrCompress(3,/REMOVE) + 'A' + StrCompress(1+StrLen((profileList[i])[0]),/REMOVE) + ')'
        PrintF, profileFile, profileList[i], FORMAT = '(' + StrCompress(3,/REMOVE) + 'A' + StrCompress(1+StrLen((profileList[i])[0]),/REMOVE) + ')'
      Free_Lun, profileFile
      i += 1
      
    ENDFOREACH
  
  ENDIF ELSE BEGIN

    profileArr = FltArr(3,maxElem,numProfiles)
    
    FOREACH item, profileList DO BEGIN
      profileArr[*,0:N_Elements(item[0,*])-1,i] = item
      i += 1
    ENDFOREACH
    IF numProfiles GT 1 THEN profileArr = Reform(Transpose(profileArr,[0,2,1]), 3*numProfiles, maxElem)
    
    FOR i = 0, numProfiles-2 DO titleList +=titleList[0:2]
    
    nameListArr = StrArr(3*numProfiles)
    nameListArr[1:*:3] = nameList.toArray()
    
    OpenW, profileFile, fileName, /GET_LUN
      PrintF, profileFile, nameListArr, FORMAT = '(' + StrCompress(3*numProfiles,/REMOVE) + 'A' + StrCompress(1+StrLen(profileArr[0]),/REMOVE) + ')'
      PrintF, profileFile, titleList.toArray(), FORMAT = '(' + StrCompress(3*numProfiles,/REMOVE) + 'A' + StrCompress(1+StrLen(profileArr[0]),/REMOVE) + ')'
      PrintF, profileFile, profileArr, FORMAT = '(' + StrCompress(3*numProfiles,/REMOVE) + 'A' + StrCompress(1+StrLen(profileArr[0]),/REMOVE) + ')'
    Free_Lun, profileFile
  
  ENDELSE
  
END

FUNCTION AS_ProfileContainerObj::GetPlotImage

  @as_scatterheader.macro

  imageObj = self.profileWindow.read()
  imageObj.GetProperty, DATA = image
  RETURN, image

END

PRO AS_ProfileContainerObj::SavePlotImage, FILENAME = fileName

  @as_scatterheader.macro

  filters = [['*.bmp;*.gif;*.jpg;*.jpeg;*.png;*.tif;*.tiff','*.bmp', '*.gif', '*.jpg;*.jpeg', '*.png', '*.tif;*.tiff','*.*'], $
             ['All Image Formats','Bitmap','GIF','JPEG','PNG','TIFF','All Files']]

  IF ~KeyWord_Set(fileName) THEN fileName = Dialog_Pickfile(DEFAULT_EXTENSION = '*.jpg', FILTER = filters, /FIX_FILTER, /OVERWRITE_PROMPT,/WRITE)
  IF fileName EQ '' THEN RETURN
  image = self.GetPlotImage()
  extension = StrUpCase((StrSplit(fileName, '.',/EXTRACT))[-1])
  IF extension EQ 'TIF' THEN extension = 'TIFF'
  IF extension EQ 'JPG' THEN extension = 'JPEG'
  Write_Image, fileName, extension, image

END

PRO AS_ProfileContainerObj::SetPowerPlot, power, SLIDE = slide

  @as_scatterheader.macro

  IF ~N_elements(slide) THEN BEGIN
    slide = self.PowerPlot.Description
  ENDIF

  IF N_Elements(power) EQ 0 THEN BEGIN
    power = self.PowerPlot.Name
  ENDIF

  IF power GE 0 THEN BEGIN
    self.PowerPlot.SetProperty, HIDE = 1
  ENDIF ELSE BEGIN
    self.PowerPlot.Name = power
    self.PowerPlot.Description = slide
    self.PowerPlot.SetProperty, HIDE = 0
    
    IF (self.profileRefs) NE !null THEN data = ((*self.profileRefs)[0].profiles).GetData() $
                                 ELSE data = findgen(100)*0.00001 + 0.00001

    max = max(data[1,*]*data[0,0]^power/(data[0,*]^power),whereMax)
    
    ;IF self.ylog THEN expnt = 10 ELSE expnt = 1

    expnt = 7    
    mult = (max - min(data[1,(where(data[1,*] Gt 0))]))*(slide^expnt/100.^expnt) + min(data[1,(where(data[1,*] Gt 0))])

    powerProfile = [[data[0,*]],mult*[data[0,*]^power]/data[0,0]^power]
   
    IF self.xlog THEN powerProfile[0,*] = Alog10(powerProfile[0,*])
    IF self.ylog THEN powerProfile[1,*] = Alog10(powerProfile[1,*])

    self.powerPlot.SetProperty, DATAX = powerProfile[0,*], DATAY = powerProfile[1,*], HIDE = 0
    self.profileXAxis->GetProperty, CRange=xrange
    self.profileYAxis->GetProperty, CRange=yrange


    ; Set up the scaling so that the axes for the plot and the
    ; plot data extends from 0->1 in the X and Y directions.

    xs = [((self.position[0]*xrange[1])-(self.position[2]*xrange[0])) / $
      (xrange[1]-xrange[0]), (self.position[2]-self.position[0])/(xrange[1]-xrange[0])]

    ys = [((self.position[1]*yrange[1])-(self.position[3]*yrange[0])) / $
      (yrange[1]-yrange[0]), (self.position[3]-self.position[1])/(yrange[1]-yrange[0])]

    ; Scale the plot data and axes into 0->1.
    self.powerPlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys

    ;self.UpdatePlot, /KEEPFIT

  ENDELSE

END

FUNCTION AS_ProfileContainerObj::FitPeak, index, FULLRANGE = fullRange, PLOT = plot
  
  @as_scatterheader.macro

  temp = IntArr(N_Elements(index))
  peak = Float(temp)
  
  FOR i = 0, N_Elements(index) - 1 DO temp[i] = Where((*self.profileRefs).refNum EQ index[i])
  index = temp

  FOREACH j, index DO peak = ((*self.profileRefs)[j].profiles).FitPeak(self.xRangeZoom, PEAKSIGMA = peakSigma, CHI = fitChi, PLOT = fitPlot)
 
  IF ~Obj_Valid(self.qCalibGUI) THEN BEGIN
                        Widget_Control, self.groupleader, GET_UVALUE = scatterBrain
                        self.qCalibGUI = scatterBrain.qCalibGUI(GROUPLEADER=self.wBase)
  ENDIF

  self.qCalibGUI.SetProperty, PEAK = peak[0]

  IF self.xlog THEN fitPlot[0,*] = Alog10(fitPlot[0,*])
  IF self.ylog THEN fitPlot[1,*] = Alog10(fitPlot[1,*])

  self.peakFit.SetProperty, DATAX = fitPlot[0,*], DATAY = fitPlot[1,*], HIDE = 0
  self.profileXAxis->GetProperty, CRange=xrange
  self.profileYAxis->GetProperty, CRange=yrange
  
  
    ; Set up the scaling so that the axes for the plot and the
    ; plot data extends from 0->1 in the X and Y directions.

  xs = [((self.position[0]*xrange[1])-(self.position[2]*xrange[0])) / $
        (xrange[1]-xrange[0]), (self.position[2]-self.position[0])/(xrange[1]-xrange[0])]

  ys = [((self.position[1]*yrange[1])-(self.position[3]*yrange[0])) / $
        (yrange[1]-yrange[0]), (self.position[3]-self.position[1])/(yrange[1]-yrange[0])]

    ; Scale the plot data and axes into 0->1.
  self.peakFit->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
  
  self.messageObj.SetProperty, CHAR_DIMENSIONS = [0.03,0.03], STRINGS = 'Fit: Peak = ' + StrCompress(String(peak, FORMAT = '(D6.4)'), /REMOVE_ALL) + ', Peak Sigma = ' + StrCompress(String(peakSigma, FORMAT = '(E10.2)'), /REMOVE_ALL) + ', Chi = ' + StrCompress(String(fitChi, FORMAT = '(D10.3)'), /REMOVE_ALL)
  
  self.UpdatePlot, /KEEPFIT
  
  RETURN, peak

END

PRO AS_ProfileContainerObj::LinLog, AXIS, _REF_Extra=extra
  
  @as_scatterheader.macro
  
  CASE AXIS OF
    'X'       : BEGIN
                  IF self.xLog EQ 0 THEN BEGIN
                      self.xLog = 1
                      self.profileXAxis->SetProperty, /LOG, EXACT=1, MAJOR=-1, MINOR = 8
                      self.profileXAxisGrid->SetProperty, /LOG, EXACT=1, MAJOR=-1, MINOR = 8
                  ENDIF ELSE BEGIN
                      self.xLog = 0
                      self.profileXAxis->SetProperty, LOG = 0, MAJOR = -1, MINOR = 4
                      self.profileXAxisGrid->SetProperty, LOG = 0, MAJOR = -1, MINOR = 4
                  ENDELSE
                END
    'Y'       : BEGIN
                  IF self.yLog EQ 0 THEN BEGIN
                      self.yLog = 1
                      self.profileYAxis->SetProperty, /LOG, EXACT=1, MAJOR=-1, MINOR=8
                      self.profileYAxisGrid->SetProperty, /LOG, EXACT=1, MAJOR=-1, MINOR=8
                  ENDIF ELSE BEGIN
                      self.yLog = 0
                      self.profileYAxis->SetProperty, LOG = 0, MAJOR = -1, MINOR = 4
                      self.profileYAxisGrid->SetProperty, LOG = 0, MAJOR = -1, MINOR = 4
                  ENDELSE
                END
  ENDCASE
  IF Ptr_Valid(self.profileRefs) THEN IF N_Elements(*self.profileRefs) GT 0 THEN BEGIN
    FOR i = 0, N_Elements(*self.profileRefs) - 1 DO BEGIN
       IF Obj_Valid((*self.profileRefs)[i].profilePlot) THEN BEGIN
        (*self.profileRefs)[i].profilePlot->GetProperty, HIDE = hide
        (*self.profileRefs)[i].profilePlot->GetProperty, THICK = thick
        (*self.profileRefs)[i].profilePlot->GetProperty, NAME = name
        Obj_Destroy, (*self.profileRefs)[i].profilePlot
        self->PlotProfile, i, name, /REPLOT,  XLOG = self.xLog, YLOG = self.yLog, /KEEPZOOM, _Extra=extra
        (*self.profileRefs)[i].profilePlot->SetProperty, HIDE = hide
        (*self.profileRefs)[i].profilePlot->SetProperty, THICK = thick
       ENDIF
    ENDFOR
  ENDIF
  IF Obj_Valid(self.livePlot) THEN BEGIN
    self.livePlot->GetProperty, HIDE = hide
    self->PlotProfile, LIVE=1, /KEEPZOOM
    self.livePlot->SetProperty, HIDE = hide
  ENDIF
  IF Obj_Valid(self.peakFit) THEN BEGIN
    self.peakFit.GetProperty, DATA = data
    IF N_Elements(data) GT 0 THEN BEGIN
      CASE Axis OF
        'X' : IF self.xlog THEN data[0,*] = Alog10(data[0,*]) ELSE data[0,*] = 10^(data[0,*])
        'Y' : IF self.ylog THEN data[1,*] = Alog10(data[1,*]) ELSE data[1,*] = 10^(data[1,*])
      ENDCASE
      self.peakFit.SetProperty, DATAX = data[0,*], DATAY = data[1,*]
    ENDIF
  ENDIF
  self.SetPowerPlot
  self->LinLogQMarkers
  
END

PRO AS_ProfileContainerObj::SetNormParams, normStruct, NOUPDATEPLOTS=noUpdatePlots

  @as_scatterheader.macro

  IF Where(Tag_Names(normStruct) EQ 'NORMTYPE') GE 0 THEN BEGIN
    IF normStruct.normType GE 0 THEN BEGIN
      self.useCalib = 0
      self.nrmType = normStruct.normType
      IF Obj_Valid(self.absGUI) THEN self.absGUI.SetProperty, NORMTYPE = self.nrmType
      IF Ptr_Valid(self.profileRefs) AND ~KeyWord_Set(noUpdatePlots) THEN FOREACH profile, ((*self.profileRefs).profiles) DO profile.SetNorm, normStruct.normType
      CASE normStruct.normType OF 
        0: self.yAxisTitle_obj.SetProperty, STRINGS = 'Intensity Counts/s (No Norm.)'
        1: self.yAxisTitle_obj.SetProperty, STRINGS = 'Intensity Counts/s (Norm. To I0)'
        2: self.yAxisTitle_obj.SetProperty, STRINGS = 'Intensity Counts/s (Norm. To BeamStop)'
        3: self.yAxisTitle_obj.SetProperty, STRINGS = 'Intensity Counts/s (Norm. To I0 & BeamStop)'
        ELSE:
      ENDCASE
    ENDIF
  ENDIF
  
  IF Where(Tag_Names(normStruct) EQ 'COUNTERRATIO') GE 0 THEN BEGIN
    IF normStruct.counterRatio GE 0 THEN BEGIN
      IF Ptr_Valid(self.profileRefs) AND ~KeyWord_Set(noUpdatePlots) THEN FOREACH profile, ((*self.profileRefs).profiles) DO profile.SetProperty, COUNTERNORM = normStruct.counterRatio
    ENDIF
  ENDIF

  IF Where(Tag_Names(normStruct) EQ 'I0NORM') GE 0 THEN BEGIN
    IF normStruct.I0Norm GE 0 THEN self.ionrm = normStruct.I0Norm
    IF Ptr_Valid(self.profileRefs) AND ~KeyWord_Set(noUpdatePlots) THEN FOREACH profile, ((*self.profileRefs).profiles) DO profile.SetProperty, I0Norm = self.ionrm
  ENDIF
  
  IF Where(Tag_Names(normStruct) EQ 'IBSNORM') GE 0 THEN BEGIN
    IF normStruct.IBSNorm GE 0 THEN self.ibsnrm = normStruct.IBSNorm
    IF Obj_Valid(self.normGUI) THEN self.normGUI.DetCounts, self.ionrm, self.ibsnrm, 1
    IF Obj_Valid(self.absGUI) THEN self.absGUI.DetCounts, self.ionrm, self.ibsnrm, 1, /APPLIED
    IF Ptr_Valid(self.profileRefs) AND ~KeyWord_Set(noUpdatePlots) THEN FOREACH profile, ((*self.profileRefs).profiles) DO profile.SetProperty, IBSNorm = self.ibsnrm
  ENDIF

  IF Where(Tag_Names(normStruct) EQ 'ABSCAL') GE 0 THEN BEGIN
    IF normStruct.absCal GE 0 THEN BEGIN
      self.usecalib = 1
      self.yAxisTitle_obj.SetProperty, STRINGS = 'Absolute Intensity cm!U-1'
      self.cscalib = normStruct.absCal
      ;IF Obj_Valid(self.absGUI) THEN self.absGUI.SetCurrentCalibration, self.cscalib, /USE
      IF Ptr_Valid(self.profileRefs) AND ~KeyWord_Set(noUpdatePlots) THEN FOREACH profile, ((*self.profileRefs).profiles) DO profile.SetCalib, normStruct.absCal
    ENDIF
  ENDIF
  
  IF Ptr_Valid(self.profileRefs) THEN BEGIN
    FOREACH profile, ((*self.profileRefs).profiles),key DO BEGIN
      data = profile.GetData(/BACK, XLOG=self.xlog, YLOG=self.ylog)
      (((*self.profileRefs)[key]).profilePlot).SetProperty, DATAX = data[0,*], DATAY=data[1,*]
    ENDFOREACH
  ENDIF
  
END

PRO AS_ProfileContainerObj::ShowHidePlot, index

  @as_scatterheader.macro

  temp = (hidden = IntArr(N_Elements(index)))
  FOR i = 0, N_Elements(index) - 1 DO temp[i] = Where((*self.profileRefs).refNum EQ index[i])
  index = temp
  FOR i =0, N_Elements(index) - 1 DO BEGIN
    (*self.profileRefs)[index[i]].profilePlot->GetProperty, HIDE = temp
    hidden[i] = temp
  END
  FOR i =0, N_Elements(index) - 1 DO (*self.profileRefs)[index[i]].profilePlot->SetProperty, HIDE = ~hidden[i]
END

PRO AS_ProfileContainerObj::HidePlot, index

  @as_scatterheader.macro

  temp = IntArr(N_Elements(index))
  FOR i = 0, N_Elements(index) - 1 DO temp[i] = Where((*self.profileRefs).refNum EQ index[i])
  index = temp
  
  FOR i =0, N_Elements(index) - 1 DO (*self.profileRefs)[index[i]].profilePlot->SetProperty, /HIDE
END

PRO AS_ProfileContainerObj::ShowPlot, index

  @as_scatterheader.macro

  temp = IntArr(N_Elements(index))
  FOR i = 0, N_Elements(index) - 1 DO temp[i] = Where((*self.profileRefs).refNum EQ index[i])
  index = temp

  FOR i =0, N_Elements(index) - 1 DO (*self.profileRefs)[index[i]].profilePlot->SetProperty, HIDE = 0
END

PRO AS_ProfileContainerObj::ShowErrorPlot, show

  @as_scatterheader.macro
  
  IF N_Elements(show) GT 0 THEN self.showErrorBars = show
  
  IF ~Ptr_Valid(self.profileRefs) THEN RETURN
  
  SWITCH self.showErrorBars OF
    0:  
    2:  BEGIN 
          FOREACH profile, ((*self.profileRefs).profilePlot) DO IF profile NE !NULL THEN profile.showErrorPlot, self.showErrorBars
          BREAK
        END
    1:  BEGIN
          FOREACH profile, ((*self.profileRefs).profilePlot) DO profile.showErrorPlot, 0
          IF self.current EQ -1 THEN RETURN
          ((*self.profileRefs)[self.current].profilePlot).showErrorPlot, 1
        END
  ENDSWITCH
  
  self.UpdatePlot
  
END

PRO AS_ProfileContainerObj::LineWidth, index, width, ADD = add

  @as_scatterheader.macro

  IF ~Ptr_Valid(self.profileRefs) THEN RETURN 

  temp = IntArr(N_Elements(index))
  FOR i = 0, N_Elements(index) - 1 DO temp[i] = Where((*self.profileRefs).refNum EQ index[i])
  index = temp

  FOR i =0, N_Elements(index) - 1 DO BEGIN
    currentWidth = 0
    IF KeyWord_Set(Add) THEN BEGIN 
      (*self.profileRefs)[index[i]].profilePlot->GetProperty, THICK = currentWidth
    ENDIF
    (*self.profileRefs)[index[i]].profilePlot->SetProperty, THICK = width + currentWidth
  ENDFOR

END

PRO AS_ProfileContainerObj::LineOpacity, index, opacity

  @as_scatterheader.macro

  temp = IntArr(N_Elements(index))
  FOR i = 0, N_Elements(index) - 1 DO temp[i] = Where((*self.profileRefs).refNum EQ index[i])
  index = temp

  FOR i =0, N_Elements(index) - 1 DO (*self.profileRefs)[index[i]].profilePlot->SetProperty, ALPHA_CHANNEL = opacity

END

PRO AS_ProfileContainerObj::MultOffset, index, MULT=mult, OFFSET=offset

  @as_scatterheader.macro
  
  temp = IntArr(N_Elements(index))
  FOR i = 0, N_Elements(index) - 1 DO temp[i] = Where((*self.profileRefs).refNum EQ index[i])
  index = temp

  FOR i =0, N_Elements(index) - 1 DO BEGIN
    (*self.profileRefs)[index[i]].profiles->SetProperty, MULT = mult, OFFSET = offset
    data = (*self.profileRefs)[index[i]].profiles->GetData(/BACK,XLOG=self.xlog, YLOG=self.ylog)
    (*self.profileRefs)[index[i]].profilePlot->SetProperty, DATAX= data[0,*], DATAY=data[1,*]
    (*self.profileRefs)[index[i]].profiles->GetProperty, NOTIFYPROFILE=notifyProfile
     FOR j = 0, N_Elements(notifyProfile) - 1 DO BEGIN
        data = notifyProfile[j]->GetData(/BACK,XLOG=self.xlog, YLOG=self.ylog)
        (*self.profileRefs)[Where((*self.profileRefs).profiles EQ notifyProfile[j])].profilePlot->SetProperty, DATAX= data[0,*], DATAY=data[1,*]
     ENDFOR 
  ENDFOR
  self->UpdatePlot
  
END

PRO AS_ProfileContainerObj::PlotProfile, profile, fname, LIVE = live, REPLOT = Replot, XLOG = xLog, YLOG = yLog, KEEPZOOM = keepZoom, _Extra=extra

  @as_scatterheader.macro

  IF ~KeyWord_Set(xLog) THEN xLog = self.xLog
  IF ~KeyWord_Set(yLog) THEN yLog = self.yLog
  IF ~KeyWord_Set(keepZoom) THEN keepZoom = 0

  IF KeyWord_Set(live) AND Obj_Valid(self.liveProfile) THEN BEGIN
    data = self.liveProfile->GetData(/BACK,XLOG=xlog, YLOG=ylog,_Extra=extra)
    IF N_Elements(data) GT 1 THEN self.livePlot->SetProperty, DATAX = data[0,*], DATAY = data[1,*], THICK = 2, COLOR = [0,0,0],_EXTRA=extra $
                             ELSE RETURN
  ENDIF ELSE BEGIN
    
    data = (*self.profileRefs)[profile].profiles->GetData(/BACK,XLOG=xlog, YLOG=ylog,_Extra=extra)
    
    IF ~KeyWord_Set(replot) THEN BEGIN 
      IF KeyWord_Set(extra) THEN BEGIN
        IF Where(Tag_Names(extra) EQ 'COLOR') GE 0 THEN plotColourTemp = extra.color ELSE plotColourTemp = profile
      ENDIF ELSE plotColourTemp = profile
      (*self.profileRefs)[profile].plotColour = Ptr_New(plotColourTemp)
    ENDIF
  
    (*self.profileRefs)[profile].profilePlot = Obj_New('as_plotobject', NAME= fname, COLOR = *(*self.profileRefs)[profile].plotColour)
    self->Add, (*self.profileRefs)[profile].profilePlot
    IF KeyWord_Set(ylog) AND N_Elements(DATA) GT 1 THEN errorBars = data[2:3, *] ELSE errorBars = Reform(data[2,*])
    IF self.showErrorBars EQ 2 THEN showErrorBars = 1 ELSE showErrorBars = 0
    (*self.profileRefs)[profile].profilePlot->SetProperty, DATAX = data[0,*], DATAY = data[1,*], ERRORBARS = errorBars, SHOWERRORBARS = showErrorBars, _EXTRA=extra
    (*self.profileRefs)[profile].showPlot = 1

  ENDELSE
  
  ;IF N_Elements((*self.profileRefs).profilePlot) EQ 1 THEN BEGIN
  IF Ptr_Valid(self.profileRefs) THEN BEGIN
    IF N_Elements(*self.profileRefs) EQ 1 THEN BEGIN
      IF XLOG EQ 0 THEN self.xRange[0] = Max(data[0,Where(data[0,*] GT 0)]) ELSE self.xRange[0] = Max(10^data[0,Where(Finite(data[0,*]))])
      IF YLOG EQ 0 THEN self.yRange[0] = Max(data[1,*]) ELSE self.yRange[0] = Max(10^data[1,Where(Finite(data[0,*]))])
    ENDIF
  ENDIF

  IF KeyWord_Set(xLog) THEN BEGIN
             IF self.xRange[0] LE 0 THEN self.xRange[0] = self.xRange[1]
             self.xRange = [MIN(10^data[0,Where(Finite(data[0,*])AND data[1,*] GT 0)]) < self.xRange[0],MAX(10^data[0,Where(Finite(data[0,*]))]) > self.xRange[1]] 
             IF ~KeyWord_Set(live) THEN (*self.profileRefs)[profile].profilePlot->SetProperty, xrange = ALog10(self.xrange) $
                                   ELSE self.livePlot.SetProperty, xrange = ALog10(self.xrange)
  ENDIF ELSE self.xRange = [MIN(data[0,*]) < self.xRange[0],MAX(data[0,*]) > self.xRange[1]] 
  IF KeyWord_Set(yLog) THEN BEGIN
    IF self.yRange[0] LE 0 THEN self.yRange[0] = self.yRange[1]
    self.yRange = [MIN(10^data[1,Where(Finite(data[1,*]))]) < (self.yRange[0]),MAX(10^data[1,Where(Finite(data[1,*]))]) > self.yRange[1]] 
  ENDIF ELSE self.yRange = [MIN(data[1,*]) < self.yRange[0],MAX(data[1,*]) > self.yRange[1]]

  IF self.yrange[0]-self.yrange[1] EQ 0 THEN self.yrange[1]=self.yrange[1]*10
  IF self.xrange[0]-self.xrange[1] EQ 0 THEN self.xrange[1]=self.xrange[1]*10
  IF Total(Finite(self.xRange)) NE 2 THEN self.xRange = [0,1]
  IF Total(Finite(self.yRange)) NE 2 THEN self.yRange = [0,1]

  IF ~keepZoom THEN BEGIN
  
    IF self.xrange[0] - self.xrange[1] EQ 0 THEN self.xrange = [0,1]
    IF self.yrange[0] - self.yrange[1] EQ 0 THEN self.yrange = [0,1]
  
    self.profileXAxis->SetProperty, RANGE=self.xrange
    self.profileYAxis->SetProperty, RANGE=self.yrange
    self.profileXAxisGrid->SetProperty, RANGE=self.xrange
    self.profileYAxisGrid->SetProperty, RANGE=self.yrange

  ENDIF
    
  ;ENDIF
    
 
   ; Because we may not be using exact axis ranging, the axes
  ; may extend further than the xrange and yrange. Get the
  ; actual axis range so that the plot, etc. can be scaled
  ; appropriately.

  self.profileXAxis->GetProperty, CRange=xrange
  self.profileYAxis->GetProperty, CRange=yrange
  
    ; Set up the scaling so that the axes for the plot and the
    ; plot data extends from 0->1 in the X and Y directions.

  xs = [((self.position[0]*xrange[1])-(self.position[2]*xrange[0])) / $
        (xrange[1]-xrange[0]), (self.position[2]-self.position[0])/(xrange[1]-xrange[0])]

  ys = [((self.position[1]*yrange[1])-(self.position[3]*yrange[0])) / $
        (yrange[1]-yrange[0]), (self.position[3]-self.position[1])/(yrange[1]-yrange[0])]

    ; Scale the plot data and axes into 0->1.
  IF ~KeyWord_Set(live) THEN (*self.profileRefs)[profile].profilePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys $
                        ELSE self.livePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
  self.profileXAxis->SetProperty, XCoord_Conv=xs
  self.profileYAxis->SetProperty, YCoord_Conv=ys
  self.profileXAxisGrid->SetProperty, XCoord_Conv=xs
  self.profileYAxisGrid->SetProperty, YCoord_Conv=ys

  ;thickness = FltArr(N_Elements(*self.profiles))
  ;thickness[*] = 2

  IF Ptr_Valid(self.profileRefs) THEN BEGIN
    FOR i = 0, N_Elements(*self.profileRefs) - 1 DO BEGIN 
      IF Obj_Valid((*self.profileRefs)[i].profilePlot) THEN (*self.profileRefs)[i].profilePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, XRANGE = xrange, YRANGE = yrange
    ENDFOR
  ENDIF
  self.livePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, XRANGE = xrange, YRANGE = yrange
  self.peakFit->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, XRANGE = xrange, YRANGE = yrange

  self.profileWindow->Draw, self.profileView
  
END

;PRO AS_ProfileContainerObj::SetBlank, profile, PLOT = plot
;
;IF KeyWord_Set(plot) THEN profile = 
;
;IF ~Ptr_Valid(self.blanks) THEN BEGIN
;    numBlank = 1
;    self.blanks = Ptr_New(self.)
;    self.dispBlank = Ptr_New(IntArr(1))
;    
;  ENDIF ELSE BEGIN
;    numBlank = N_Elements(*self.blanks) + 1
;    blanksTemp = ObjArr(numBlank)
;    blanksTemp[0:numBlank-2] = *self.Blank
;    dispBlankTemp = IntArr(numBlank)
;    dispBlankTemp[0:numBlank-2] = *self.dispBlank
;  ENDELSE
;
;END

PRO AS_ProfileContainerObj::UpdatePlot, void, KEEPFIT = keepFit

  @as_scatterheader.macro

  self.SetPowerPlot

  IF ~KeyWord_Set(keepFit) THEN BEGIN
    self.messageObj.SetProperty, STRINGS = ''
    self.peakFit.SetProperty, HIDE = 1
  ENDIF
  self.profileWindow->Draw, self.profileView

END

PRO AS_ProfileContainerObj::ReplaceWithLast, index

  @as_scatterheader.macro

  temp = Where((*self.profileRefs).refNum EQ index[0])
  index = temp
  
  last = N_Elements(*self.profileRefs)-1
  
  (*self.profileRefs)[index].profiles->GetProperty, NOTIFYPROFILE=notifyProfile, BACKOBJ = backObj, FNAME=fNameOld, CONFIGNAME=configNameOld
  
  tempObj = (*self.profileRefs)[index].profiles
  (*self.profileRefs)[index].profiles = (*self.profileRefs)[last].profiles
  (*self.profileRefs)[index].profiles.SetProperty, NOTIFYPROFILE=notifyProfile, BACK = backObj
  IF N_Elements(notifyProfile) GT 0 THEN BEGIN
    FOREACH noteProfile, notifyProfile DO BEGIN
      noteProfile.GetProperty, BACKOBJ = back
      IF back EQ tempObj THEN noteProfile.SetProperty, BACK = (*self.profileRefs)[index].profiles
    ENDFOREACH
  ENDIF
  (*self.profileRefs)[last].profiles = tempObj
  self->DeleteProfile, (*self.profileRefs)[last].refNum
  data = (*self.profileRefs)[index].profiles->GetData(XLOG=self.xlog, YLOG=self.ylog, /BACK)
  IF N_Elements(data) GT 1 OR data[0] GT -1 THEN (*self.profileRefs)[index].profilePlot->SetProperty, DATAX= data[0,*], DATAY=data[1,*], ERRORBARS = data[2,*]

  (*self.profileRefs)[index].profiles->GetProperty, FNAME=fName, CONFIGNAME=configName

  IF (configName NE configNameOld) OR (fName NE fNameOld) THEN BEGIN
    event = {type : 'PlotNameChange',  label : fname + ' (' + configName + ')', index : (*self.profileRefs)[index].refNum}
    self.NotifyObject, event
  ENDIF

  FOR j = 0, N_Elements(notifyProfile) - 1 DO BEGIN
    data = notifyProfile[j]->GetData(XLOG=self.xlog, YLOG=self.ylog, /BACK)
    (*self.profileRefs)[Where((*self.profileRefs).profiles EQ notifyProfile[j])].profilePlot->SetProperty, DATAX= data[0,*], DATAY=data[1,*]
  ENDFOR 


;  last = N_Elements(*self.profileRefs)-1
;  data = (*self.profileRefs)[last].profiles->GetData(
;  (*self.profileRefs)[index].profiles->SetProperty, PROFILE=Reform(data[1,*]), QVECTOR = Reform(data[0,*]), ERROR = Reform(data[2,*])
;  self->DeleteProfile, (*self.profileRefs)[last].refNum
;    
;  data = (*self.profileRefs)[index].profiles->GetData(XLOG=self.xlog, YLOG=self.ylog)
;  (*self.profileRefs)[index].profilePlot->SetProperty, DATAX= data[0,*], DATAY=data[1,*]
;  (*self.profileRefs)[index].profiles->GetProperty, NOTIFYPROFILE=notifyProfile
;  FOR j = 0, N_Elements(notifyProfile) - 1 DO BEGIN
;      data = notifyProfile[j]->GetData(XLOG=self.xlog, YLOG=self.ylog)
;      (*self.profileRefs)[Where((*self.profileRefs).profiles EQ notifyProfile[j])].profilePlot->SetProperty, DATAX= data[0,*], DATAY=data[1,*]
;  ENDFOR 
;  
  self->UpdatePlot
   
END

PRO AS_ProfileContainerObj::MoveProfile, index, newPosition

  @as_scatterheader.macro

  IF index EQ newPosition THEN RETURN

  tempProfileRef = (*self.profileRefs)[index]

  IF index EQ 0 THEN (*self.profileRefs)[0:-2] = (*self.profileRefs)[1:*]
  IF index LT N_Elements((*self.profileRefs)) - 1 THEN (*self.profileRefs)[0:-2] = [(*self.profileRefs)[0:index-1],(*self.profileRefs)[index+1:-1]] 
  
  IF newPosition EQ 0 THEN (*self.profileRefs) = [tempProfileRef,(*self.profileRefs)[0:-2]]
  IF newPosition EQ N_Elements((*self.profileRefs)) - 1 THEN (*self.profileRefs) = [(*self.profileRefs)[0:-2],tempProfileRef]
  IF newPosition LT N_Elements((*self.profileRefs)) - 1 THEN $
                    (*self.profileRefs) = [(*self.profileRefs)[0:newPosition-1],tempProfileRef,(*self.profileRefs)[newPosition:-2]]
        
END

PRO AS_ProfileContainerObj::DeleteProfile, index, ALL=all, NONOTIFY = noNotify

  @as_scatterheader.macro

  IF ~Ptr_Valid(self.profileRefs) THEN RETURN
  IF KeyWord_Set(ALL) THEN index = IndGen(N_Elements(*self.profileRefs)) ELSE BEGIN
    temp = IntArr(N_Elements(index))
    FOR i = 0, N_Elements(index) - 1 DO temp[i] = Where((*self.profileRefs).refNum EQ index[i])
    index = temp
  ENDELSE
  
  numProfiles = N_Elements(*self.profileRefs)
  
  IF numProfiles GT 1 THEN BEGIN
    
    valid = Obj_Valid((*self.profileRefs)[index].profilePlot) 
    refNumbers = (*self.profileRefs)[index[Where(valid NE 0)]].refNum
    IF ~KeyWord_Set(noNotify) THEN FOREACH refNum, refNumbers DO self->NotifyObject, {type: 'DeletePlot', index: refNum, leaveProfile: 1}
    Obj_Destroy, (*self.profileRefs)[index[Where(valid NE 0)]].profilePlot
    valid = Obj_Valid((*self.profileRefs)[index].profiles)
    Obj_Destroy, (*self.profileRefs)[index[Where(valid NE 0)]].profiles
    valid = Ptr_Valid((*self.profileRefs)[index].plotColour)
    Ptr_Free, (*self.profileRefs)[index[Where(valid NE 0)]].plotColour
    
    tempArr = IntArr(numProfiles)
    tempArr[index] = 1
    tempArr = Where(tempArr NE 1)
    *self.profileRefs = (*self.profileRefs)[tempArr]
    
;    FOR i = 0, numProfiles - 1 DO BEGIN
;      temp = Where(index LE i, move)
;      (*self.profileRefs[i].plotColour) = plotColoursTemp[i] - move
;    ENDFOR
    
;    FOR i =0, N_Elements(*self.plotColours) - 1 DO (*self.profilePlots)[i]->SetProperty, color=(*self.plotColours)[i]

    
  ENDIF 
  
  IF numProfiles LE 1 OR KeyWord_Set(ALL) OR N_Elements(*self.profileRefs) EQ 0 THEN BEGIN
    valid = Obj_Valid((*self.profileRefs)[index].profilePlot)
    refNumbers = (*self.profileRefs)[index[Where(valid NE 0, /NULL)]].refNum
    IF ~KeyWord_Set(noNotify) THEN FOREACH refNum, refNumbers DO self->NotifyObject, {type: 'DeletePlot', index: refNum, leaveProfile: 1}
    IF Obj_Valid((*self.profileRefs).profilePlot) THEN Obj_Destroy, (*self.profileRefs).profilePlot
    Obj_Destroy, (*self.profileRefs).profiles
    Ptr_Free, (*self.profileRefs).plotColour
    Ptr_Free, self.profileRefs
  ENDIF


  self->UpdatePlot
          
END

PRO AS_ProfileContainerObj::AddProfile, q_arr, data, error, fname, NOPLOT = noplot, SETBLANK = setBlank, LIVE = live, PROFILEINDEX = profileIndex, DETECTORNO = detectorNo, _REF_Extra=extra

  @as_scatterheader.macro

  IF KeyWord_Set(live) THEN BEGIN
    IF self.realTime THEN BEGIN
      self.liveProfile.SetProperty, PROFILE = data, QVECTOR=q_arr, ERROR = error, _EXTRA=extra
      self.PlotProfile, /LIVE, _EXTRA=extra
      self.UpdatePlot
      profileIndex = -1
      RETURN
    ENDIF
    IF self.ignoreLive THEN RETURN
  ENDIF
  
  IF ~Ptr_Valid(self.profileRefs) THEN BEGIN
    numProf = 1
    self.profileRefs = Ptr_New({profileRefs})
  ENDIF ELSE BEGIN
    numProf = N_Elements(*self.profileRefs) + 1
    *self.profileRefs = [*self.profileRefs,{profileRefs}]
  ENDELSE

  IF ~KeyWord_Set(error) THEN error = FltArr(N_Elements(data))
    
  profile = Obj_New('AS__saxsprofileaddons', data, error, fname, QVECTOR = q_arr, DETECTORNO = detectorNo, I0Norm = self.ionrm, IBSNorm = self.ibsnrm, _EXTRA=extra) 
  profile.SetNorm, self.nrmType
  IF self.usecalib EQ 1 THEN profile.SetCalib, self.cscalib
  
  (*self.profileRefs)[numProf-1] = { profileRefs, $
                                  refNum: Long(self.lastIndex + 1), $
                                  profiles: profile, $
                                  showPlot: 0, $
                                  profilePlot: Obj_New(), $
                                  plotColour: Ptr_New() $
                                  }
  self.lastIndex = self.lastIndex + 1                                  
                                                
  profileIndex= self.lastIndex
   
  IF ~KeyWord_Set(NOPLOT) THEN BEGIN
    profile.GetProperty, CONFIGNAME = configName
    self->PlotProfile, numProf-1, fname, /KEEPZOOM, _EXTRA=extra
    self->NotifyObject, {type: 'NewPlot', label: fname + ' (' + configName  + ')', index: Long(self.lastIndex)}
  ENDIF
  
  self.current = numProf-1
  
END



PRO AS_ProfileContainerObj::SelectPlot, plotNo, DOUBLE=double

  @as_scatterheader.macro

  IF plotNo[0] GE 0 THEN BEGIN
    plotRef = !Null
    FOREACH plot, plotNo DO BEGIN
      temp = Where((*self.profileRefs).refNum EQ plot)
      plotRef = [plotRef,temp]
    ENDFOREACH
    
    self.current = plotRef[0]
    ((*self.profileRefs)[plotRef[0]]).profiles->GetProperty, I0=I0, BS=BS, TIME = time
    IF Obj_Valid(self.normGui) THEN self.normGui.DetCounts, I0, BS, time
    IF Obj_Valid(self.absGui) THEN BEGIN
      self.absGui.DetCounts, I0, BS, time
      I0Arr = (BSArr = (AveDetCounts = !Null))
      I0BackArr = (BSBackArr = (AveBackDetCounts = !Null))
      FOREACH prof, ((*self.profileRefs)[plotRef]).profiles DO BEGIN
        prof->GetProperty, I0=I0, BS=BS, BACKOBJ=backObj
        data = prof.GetData(/NONORM)
        I0Arr = [I0Arr,I0]
        BSArr = [BSArr,BS]
        AveDetCounts = [AveDetCounts, mean(data[1,Where(data[0,*] GE self.xrangezoom[0] and data[0,*] LE self.xrangezoom[1])])]
        IF Obj_Valid(backObj) THEN BEGIN
          backObj->GetProperty, I0=I0Back, BS=BSBack
          background = backObj.GetData(/NONORM)
          I0BackArr = [I0BackArr,I0Back]
          BSBackArr = [BSBackArr,BSBack]
          AveBackDetCounts = [AveBackDetCounts, mean(background[1,Where(background[0,*] GE self.xrangezoom[0] and background[0,*] LE self.xrangezoom[1])])]
        ENDIF
      ENDFOREACH
      IF Obj_Valid(backObj) THEN self.absGui.CalibPoints, AveDetCounts, I0Arr/self.ionrm, BSArr/self.ibsnrm, BACKCOUNTS = AveBackDetCounts, I0Back = I0BackArr/self.ionrm, BSBack = BSBackArr/self.ibsnrm $
                            ELSE self.absGui.CalibPoints, AveDetCounts, I0Arr/self.ionrm, BSArr/self.ibsnrm
    ENDIF
  ENDIF ELSE BEGIN
    self.current = -1
    IF Obj_Valid(self.absGui) THEN BEGIN
      self.absGui.CalibPoints, /CLEAR
    ENDIF
  ENDELSE
  self.showErrorPlot
END

FUNCTION AS_ProfileContainerObj::SetBlank, index, blank

  @as_scatterheader.macro

  temp = IntArr(N_Elements(index))
  FOR i = 0, N_Elements(index) - 1 DO temp[i] = Where((*self.profileRefs).refNum EQ index[i])
  index = temp

  IF N_Elements(blank) EQ 0 THEN BEGIN
  
    FOR i = 0, N_Elements(index) - 1 DO BEGIN
      (*self.profileRefs)[index[i]].profiles->SetProperty, BACK=''
      data = (*self.profileRefs)[index[i]].profiles->GetData(/BACK,XLOG=self.xlog, YLOG=self.ylog)
      IF KeyWord_Set(self.ylog) THEN errorBars = data[2:3, *] ELSE errorBars = Reform(data[2,*])
      (*self.profileRefs)[index[i]].profilePlot->SetProperty, DATAX= data[0,*], DATAY=data[1,*], ERRORBARS = errorBars
    ENDFOR
  
  ENDIF ELSE BEGIN

    blank = Where((*self.profileRefs).refNum EQ blank)
  
    FOR i = 0, N_Elements(index) - 1 DO BEGIN
      (*self.profileRefs)[index[i]].profiles->SetProperty, BACK=(*self.profileRefs)[blank].profiles
      data = (*self.profileRefs)[index[i]].profiles->GetData(/BACK,XLOG=self.xlog, YLOG=self.ylog)
      IF N_Elements(data) EQ 1 AND data[0] EQ -1 THEN BEGIN
        (*self.profileRefs)[index[i]].profiles->SetProperty, BACK=''
        RETURN, -1
      ENDIF
      IF KeyWord_Set(self.ylog) THEN errorBars = data[2:3, *] ELSE errorBars = Reform(data[2,*])
      (*self.profileRefs)[index[i]].profilePlot->SetProperty, DATAX= data[0,*], DATAY=data[1,*], ERRORBARS = errorBars
    ENDFOR
  
  ENDELSE
  
  IF self.xlog THEN BEGIN
             self.xRange = [MIN(10^data[0,Where(Finite(data[0,*])AND data[1,*] GT 0)]) < self.xRange[0],MAX(10^data[0,Where(Finite(data[0,*]))]) > self.xRange[1]] 
  ENDIF ELSE self.xRange = [MIN(data[0,*]) < self.xRange[0],MAX(data[0,*]) > self.xRange[1]] 
  IF self.yLog THEN self.yRange = [MIN(10^data[1,Where(Finite(data[1,*]))]) < (self.yRange[0]),MAX(10^data[1,Where(Finite(data[1,*]))]) > self.yRange[1]] ELSE $
                            self.yRange = [MIN(data[1,*]) < self.yRange[0],MAX(data[1,*]) > self.yRange[1]]

  IF Finite(self.yrange[0]) EQ 0 THEN self.yrange[0] = 0
  IF Finite(self.yrange[1]) EQ 0 THEN self.yrange[1] = self.yrange[0]
  IF Finite(self.xrange[0]) EQ 0 THEN self.xrange[0] = 0
  IF Finite(self.xrange[1]) EQ 0 THEN self.xrange[1] = self.xrange[0]

  IF self.yrange[0]-self.yrange[1] EQ 0 THEN self.yrange[1]=self.yrange[1]*10
  IF self.xrange[0]-self.xrange[1] EQ 0 THEN self.xrange[1]=self.xrange[1]*10
  self.profileXAxis->SetProperty, RANGE=self.xrange
  self.profileYAxis->SetProperty, RANGE=self.yrange
  self.profileXAxisGrid->SetProperty, RANGE=self.xrange
  self.profileYAxisGrid->SetProperty, RANGE=self.yrange
  
  self.profileXAxis->GetProperty, CRange=xrange
  self.profileYAxis->GetProperty, CRange=yrange
     
  ; Set up the scaling so that the axes for the plot and the
  ; plot data extends from 0->1 in the X and Y directions.

  xs = [((self.position[0]*xrange[1])-(self.position[2]*xrange[0])) / $
         (xrange[1]-xrange[0]), (self.position[2]-self.position[0])/(xrange[1]-xrange[0])]

  ys = [((self.position[1]*yrange[1])-(self.position[3]*yrange[0])) / $
         (yrange[1]-yrange[0]), (self.position[3]-self.position[1])/(yrange[1]-yrange[0])]

  ; Scale the plot data and axes into 0->1.
  FOR i = 0, N_Elements(*self.profileRefs) - 1 DO BEGIN 
    IF Obj_Valid((*self.profileRefs)[i].profilePlot) THEN (*self.profileRefs)[i].profilePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, XRANGE = xrange, YRANGE = yrange
  ENDFOR
  self.livePlot->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, XRANGE = xrange, YRANGE = yrange
  
  self.profileXAxis->SetProperty, XCoord_Conv=xs, MAJOR = -1
  self.profileYAxis->SetProperty, YCoord_Conv=ys

  self.profileXAxisGrid->SetProperty, XCoord_Conv=xs, MAJOR = -1
  self.profileYAxisGrid->SetProperty, YCoord_Conv=ys
        
  self->LinLogQMarkers
  
  self->UpdatePlot

  RETURN, 0
  
END

PRO AS_ProfileContainerObj::NotifyObject, event

  @as_scatterheader.macro

  FOREACH notify, self.notifyObj DO IF Obj_Valid(notify) THEN notify.notify, event
    
END

PRO AS_ProfileContainerObj::LinLogQMarkers

  @as_scatterheader.macro

  self->UpdatePlot
  
  IF Ptr_Valid(self.qMarkersQ) THEN BEGIN
    tempQArr = *self.qMarkersQ
    self->DeleteQMarker, -1
    FOR i = 0, N_Elements(tempQArr) - 1 DO BEGIN
      self->AddQMarker, tempQArr[i]
    ENDFOR
  ENDIF

END

PRO AS_ProfileContainerObj::AddQMarker, q

  @as_scatterheader.macro

  self.profileXAxis->GetProperty, XCOORD_CONV=xcoord_conv
  self.profileYAxis->GetProperty, YCOORD_CONV=ycoord_conv
  
  
  IF self.xlog EQ 1 THEN x =  ALog10(q)*xcoord_conv[1] + xcoord_conv[0] $
                    ELSE x =  q*xcoord_conv[1] + xcoord_conv[0]
  
  IF self.ylog EQ 1 THEN yrange = ALog10(self.yrange)*ycoord_conv[1] + ycoord_conv[0] $
                    ELSE yrange = self.yrange*ycoord_conv[1] + ycoord_conv[0]
  defaultYRange = [0.15,0.925]
  IF Finite(yRange[0]) EQ 0 THEN yRange[0] = defaultYRange[0]
  IF Finite(yRange[1]) EQ 0 THEN yRange[0] = defaultYRange[1]
  IF Ptr_Valid(self.qMarkersQ) THEN BEGIN
  
    FOR i = 0, N_Elements(*self.qMarkersQ) - 1 DO BEGIN
      tempQ = (*self.qMarkersQ)[i]
      IF Abs(tempQ - x) LT 0.001*x THEN RETURN
    ENDFOR
  
  ENDIF
  
  qLabel = Obj_New('IDLgrText', String(q, format = '(F5.2)'), COLOR = [127,127,127], CHAR_DIMENSIONS = [0.025,0.025])
  qMarker = Obj_New('IDLgrPolyLine', [x,x], yrange, LINESTYLE = 2, COLOR = [127,127,127], LABEL_OBJECTS=qLabel)
  self->Add, qMarker
  
  IF ~Ptr_Valid(self.qMarkersObj) THEN self.qMarkersObj = Ptr_New(qMarker) ELSE *self.qMarkersObj = [*self.qMarkersObj,qMarker]
  IF ~Ptr_Valid(self.qMarkersQ)   THEN self.qMarkersQ   = Ptr_New(q)       ELSE *self.qMarkersQ   = [*self.qMarkersQ,q]
  
  self.profileWindow->Draw, self.profileView

END

PRO AS_ProfileContainerObj::DeleteQMarker, q

  @as_scatterheader.macro

  IF ~Ptr_Valid(self.qMarkersObj) THEN RETURN

  IF q EQ -1 THEN BEGIN
  
    Obj_Destroy, (*self.qMarkersObj)
    Ptr_Free, self.qMarkersObj
    Ptr_Free, self.qMarkersQ
    self.profileWindow->Draw, self.profileView
      
  ENDIF ELSE BEGIN

    ;self.profileXAxis->GetProperty, LOCATION=location, CRANGE=crange, XCOORD_CONV=xcoord_conv
    ;x = q*xcoord_conv[1] + xcoord_conv[0]
   
    FOR i = 0, N_Elements(*self.qMarkersQ) - 1 DO BEGIN
      tempQ = (*self.qMarkersQ)
      IF Abs(tempQ[i] - q) LT 0.001*q THEN BEGIN
        (*self.qMarkersObj)[i].GetProperty, LABEL_OBJECT = labelObject
        Obj_Destroy, (*self.qMarkersObj)[i], labelObject
        temp = IndGen(N_Elements(*self.qMarkersQ))
        temp = Where(temp NE i)
        IF temp[0] EQ -1 THEN BEGIN
          Ptr_Free, self.qMarkersObj
          Ptr_Free, self.qMarkersQ
        ENDIF ELSE BEGIN
          *self.qMarkersObj = (*self.qMarkersObj)[temp]
          *self.qMarkersQ   = (*self.qMarkersQ)[temp]
        ENDELSE
        BREAK
      ENDIF
    ENDFOR
  ENDELSE
  
  self.profileWindow->Draw, self.profileView

END

PRO AS_ProfileContainerObj::ReOrgColours

  @as_scatterheader.macro

 skip = 0

  IF Ptr_Valid(self.profileRefs) THEN $
  FOR i = 0, N_Elements((*self.profileRefs).plotColour) -1 DO BEGIN
    IF (*self.profileRefs)[i].profilePlot EQ !NULL THEN BEGIN
      skip++
      continue
    ENDIF
    (*self.profileRefs)[i].plotColour = Ptr_New(i-skip)
    (*self.profileRefs)[i].profilePlot->SetProperty, color=i-skip
  ENDFOR
  

END

FUNCTION AS_ProfileContainerObj::FindByFName, fName

  @as_scatterheader.macro

  IF ~Ptr_Valid(self.profileRefs) THEN RETURN, -1
  FOREACH profile, (*self.profileRefs).profiles, key DO BEGIN
    profile.GetProperty, FNAME=fileName
    IF StrMatch(fileName, fName + '*') THEN RETURN, (*self.profileRefs)[key].refNum
  ENDFOREACH
  
  RETURN, -1
  
END

PRO AS_ProfileContainerObj::GetProperty, $
  BASE=base, $
  MEDMEAN=medmean, $
  COLOUR=colour, $
  MULT=mult, $
  OFFSET=offset, $
  FNAME=fName, $
  XRANGEZOOM=xRangeZoom, $
  IBSNRM=ibsnrm, $
  CSCALIB=CSCalib, $
  SHOWERRORBARS = showErrorBars, $
  DETECTORNO = detectorNo, $
  NORMTYPE = normType, $
  _REF_Extra=extra

  @as_scatterheader.macro

  IF Arg_Present(base) THEN base = self.wBase
  IF Arg_Present(medmean) THEN medmean = self.medmean
  IF Arg_Present(colour) THEN BEGIN
    index = Where((*self.profileRefs).refNum EQ colour[0])
    colour = *(*self.profileRefs)[index].plotColour
  ENDIF 
  IF Arg_Present(MULT) THEN BEGIN
    index = Where((*self.profileRefs).refNum EQ mult[0])
    (*self.profileRefs)[index].profiles->GetProperty, MULT = mult
  ENDIF
  IF Arg_Present(OFFSET) THEN BEGIN
    index = Where((*self.profileRefs).refNum EQ offset[0])
    (*self.profileRefs)[index].profiles->GetProperty, OFFSET = offset 
  ENDIF
  IF Arg_Present(data) THEN BEGIN
    index = Where((*self.profileRefs).refNum EQ offset[0])
    (*self.profileRefs)[index].profiles->GetData, OFFSET = offset 
  ENDIF
  IF Arg_Present(DETECTORNO) THEN BEGIN
    index = Where((*self.profileRefs).refNum EQ fName[0])
    (*self.profileRefs)[index].profiles->GetProperty, DETECTORNO=detectorNo
  ENDIF
  IF Arg_Present(FNAME) THEN BEGIN
    index = Where((*self.profileRefs).refNum EQ fName[0])
    (*self.profileRefs)[index].profiles->GetProperty, FNAME=fName
  ENDIF
  IF Arg_Present(xRangeZoom) THEN BEGIN
    xRangeZoom = self.xRangeZoom
  ENDIF
  IF Arg_Present(IBSNRM) THEN BEGIN
    IBSNRM = self.ibsnrm
  ENDIF
  IF Arg_Present(CSCalib) THEN BEGIN
    CSCalib = self.CSCalib
  ENDIF
  IF Arg_Present(NormType) THEN BEGIN
    normType = self.nrmType
  ENDIF

  IF Arg_Present(showErrorBars) THEN showErrorBars = self.showErrorBars
  
  self->IDLgrModel::GetProperty, _Extra=extra

END

PRO AS_ProfileContainerObj::SetProperty, NORMTYPE = normType, REALTIME = realTime, IGNORELIVE = ignoreLive, SHOWERRORBARS = showErrorBars, NOTIFY_OBJ = notifyObj

@as_scatterheader.macro

IF KeyWord_Set(normType) THEN self.nrmtype = normType
IF N_Elements(realTime) GE 1 THEN BEGIN
  self.realTime = realTime
  self.livePlot.SetProperty, HIDE = ~realTime
ENDIF
IF N_Elements(ignoreLive) GE 1 THEN BEGIN
  self.ignoreLive = ignoreLive
ENDIF

IF KeyWord_Set(showErrorBars) THEN self.showErrorPlot, showErrorBars

IF Keyword_Set(notifyObj) THEN $
    IF TypeName(notifyObj[0]) EQ 'NOTIFY' $
      THEN self.notifyObj = List(notifyObj,/EXTRACT)

END

PRO AS_ProfileContainerObj::UpdateProfileWidgets, _extra

  @as_scatterheader.macro

  IF Ptr_Valid(self.profilerefs) THEN BEGIN
    IF N_Elements((*self.profileRefs)) LE self.current THEN self.current = -1
   (*self.profileRefs)[self.current].profiles->UpdateProfileWidgets, self.groupleader
  ENDIF

END

PRO AS_ProfileContainerObj::ReSize, x, y

  @as_scatterheader.macro

  self.drawSize = [x, y]
  
  self.profileWindow->SetProperty, DIMENSIONS = [x > 1,y > 1]
  self.profileWindow->Draw, self.profileView  

END

PRO AS_ProfileContainerObj__Define

  profileRefs = { profileRefs, refNum: 0L, profiles: Obj_New(), showPlot: 0, profilePlot: Obj_New(), plotColour: Ptr_New() }
 
   void = {AS_ProfileContainerObj , $
               INHERITS IDLgrModel,  $
               wBase              : 0L       , $
               profileWindow      : Obj_New(), $
               profileView        : Obj_New(), $
               groupLeader        : 0L       , $
               lastIndex          : 0L       , $
               profileRefs        : Ptr_New(), $
               profileXAxis       : Obj_New(), $ 
               profileYAxis       : Obj_New(), $
               profileXAxisGrid   : Obj_New(), $ 
               profileYAxisGrid   : Obj_New(), $
               xAxisText_obj      : Obj_New(), $
               yAxisText_obj      : Obj_New(), $
               xAxisTitle_obj     : Obj_New(), $ 
               yAxisTitle_obj     : Obj_New(), $
               profileFont        : Obj_New(), $
               xRange             : [0.0,0.0], $
               xRangeZoom         : [0.0,0.0], $ 
               yRange             : [0.0,0.0], $
               yRangeZoom         : [0.0,0.0], $
               legend             : Obj_New(), $
               blankColour        : [0,0,0]  , $
               plotPalette        : Obj_New(), $
               xLog               : 0        , $
               yLog               : 0        , $
               medmean            : 0        , $
               nrmType            : 0        , $
               current            : 0        , $
               ionrm              : 300000.0 , $
               ibsnrm             : 300000.0 , $
               useCalib           : 0        , $
               CSCalib            : 1.0      , $
               boxObj             : Obj_New(), $
               boxActive          : 0        , $
               position           : [0.0,0.0,0.0,0.0], $
               drawSize           : [0,0],     $
               qMarkersObj        : Ptr_New(), $
               qMarkersQ          : Ptr_New(), $
               realTime           : 0        , $
               ignoreLive         : 0        , $
               profilePath        : ''       , $
               liveProfile        : Obj_New(), $
               livePlot           : Obj_New(), $
               normGUI            : Obj_New(), $
               absGUI             : Obj_New(), $
               qCalibGUI          : Obj_New(), $
               peakFit            : Obj_New(), $
               powerPlot          : Obj_New(), $
               messageObj         : Obj_New(), $
               notifyObj          : List()   , $
               showErrorBars      : 0          $
               }
END

