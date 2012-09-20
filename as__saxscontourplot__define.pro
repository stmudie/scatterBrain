PRO as__saxscontourplot_event, event

  @as_scatterheader.macro

  widget_control, event.top, GET_UVALUE=AS_PlotControl
  AS_PlotControl->event, event
END


PRO as__saxscontourplot::event, event

  @as_scatterheader.macro

  IF Tag_Names(event, /STRUCTURE) EQ 'WIDGET_BASE' THEN BEGIN
    geom = Widget_Info(Widget_Info(event.id, FIND_BY_UNAME = 'Contour Type Base'),/GEOMETRY)
    Widget_Control, Widget_Info(event.id, FIND_BY_UNAME = 'Contour Draw'), XSIZE = event.x-geom.xsize, YSIZE = event.y
    self.oContourWindow.Draw
    RETURN
  ENDIF
  
  IF Tag_Names(event, /STRUCTURE) EQ 'FSC_FIELD_EVENT' THEN Widget_Control, event.id, GET_UVALUE = widgetName $
                                                       ELSE widgetName = Widget_Info(event.ID, /UNAME)
  
  CASE widgetName OF
    'Contour Plot Index' : IF event.select EQ 1 THEN self.plotIndex

    'Contour Filename' : IF event.select EQ 1 THEN self.plotIndex, /FILENAMES

    'Contour Image Index' : IF event.select EQ 1 THEN self.imageIndex
                              
    'Linear Scale'  :  BEGIN 
                         IF event.select EQ 0 THEN BREAK
                         self.scaleType = 0
                         result = self.UpdateContour()
                         self.oContourWindow.Draw
                       END
    'Log Scale' :      BEGIN 
                         IF event.select EQ 0 THEN BREAK
                         self.scaleType = 1
                         result = self.UpdateContour()
                         self.oContourWindow.Draw
                       END
    'Power Scale' :    BEGIN 
                         IF event.select EQ 0 THEN BREAK
                         self.scaleType = 2
                         result = self.UpdateContour()
                         self.oContourWindow.Draw
                       END
    'Power Value' :    BEGIN
                         Widget_Control, Widget_Info(event.top, FIND_BY_UNAME = 'Power Scale'), /SET_BUTTON
                         self.scaleType = 2
                         self.powerValue = event.value
                         result = self.UpdateContour()
                         self.oContourWindow.Draw 
                       END
    'Contour Draw' :   BEGIN
                         
                         IF event.type EQ 0 THEN BEGIN
                           IF event.press EQ 1 THEN BEGIN
                             Widget_Control, event.id, /DRAW_MOTION_EVENTS 
                             self.oContourWindow.GetProperty, GRAPHICS_TREE = viewObj
                             viewObj.GetProperty, VIEWPLANE_RECT = viewPlane
                             geom = Widget_Info(event.id, /GEOMETRY)
                             xPos = (event.x/geom.xsize)*viewPlane[2] + viewPlane[0]
                             yPos = (event.y/geom.ysize)*viewPlane[3] + viewPlane[1]

                             IF event.modifiers EQ 1 THEN BEGIN
                               self.lowerQBoundObj.GetProperty, data = dataLow
                               self.upperQBoundObj.GetProperty, data = dataUp
                               IF Abs(dataUp[0,0] - xPos) LT Abs(dataLow[0,0] - xPos) THEN self.settingQBounds = 2 ELSE self.settingQBounds = 1
                             ENDIF ELSE self.settingQBounds = 0
                             
                             CASE self.settingQBounds OF  
                               0 : BEGIN
                                     data = FltArr(2,5)
                                     data[0,*] = xPos
                                     data[1,*] = yPos
                                     self.rubberBand.SetProperty, DATA = data, HIDE = 0
                                   END
                               1 : BEGIN
                                     dataLow[0,*] = xPos < dataUp[0,0]
                                     self.lowerQBoundObj.SetProperty, DATA = dataLow
                                     Widget_Control, Widget_Info(self.wContourBase,FIND_BY_UNAME = 'Lower Q Bound'), SET_VALUE = String(xPos)
                                   END
                               2 : BEGIN
                                     dataUp[0,*] = xPos
                                     self.upperQBoundObj.SetProperty, DATA = dataUp
                                     Widget_Control, Widget_Info(self.wContourBase,FIND_BY_UNAME = 'Upper Q Bound'), SET_VALUE = String(xPos)
                                   END
                             ENDCASE
                             self.oContourWindow.Draw
                             
                           ENDIF
                           IF event.press EQ 4 THEN BEGIN
                             
                             CASE event.modifiers OF 
                               0 : BEGIN
                                     self.rubberBand.SetProperty, DATA = FltArr(2,5)
                                     ;fileNames = Widget_Info(Widget_Info(event.top, FIND_BY_UNAME = 'Contour Filename'), /BUTTON_SET)
                                     self.zoomOut
                                   END
                               1 : BEGIN
                                     self.settingQBounds = 3
                                     Widget_Control, event.id, /DRAW_MOTION_EVENTS 
                                     self.oContourWindow.GetProperty, GRAPHICS_TREE = viewObj
                                     viewObj.GetProperty, VIEWPLANE_RECT = viewPlane
                                     geom = Widget_Info(event.id, /GEOMETRY)
                                     xPos = (event.x/geom.xsize)*viewPlane[2] + viewPlane[0]
                                     
                                     self.lowerQBoundObj.GetProperty, DATA = dataLow
                                     self.upperQBoundObj.GetProperty, DATA = dataUp
                                     
                                     qShift = ((dataUp[0,0] + dataLow[0,0])/2) - xPos
                                     dataLow[0,*] -= qShift                                    
                                     dataUp[0,*] -= qShift
                                     
                                     self.lowerQBoundObj.SetProperty, DATA = dataLow
                                     self.upperQBoundObj.SetProperty, DATA = dataUp
                                     
                                     Widget_Control, Widget_Info(self.wContourBase,FIND_BY_UNAME = 'Lower Q Bound'), SET_VALUE = String(dataLow[0,0])
                                     Widget_Control, Widget_Info(self.wContourBase,FIND_BY_UNAME = 'Upper Q Bound'), SET_VALUE = String(dataUp[0,0])
                                     self.plotAtQ
                                   END
                                ELSE:
                             ENDCASE  
                             self.oContourWindow.Draw
                           ENDIF
                         ENDIF
                         IF event.type EQ 1 THEN BEGIN
                           IF event.release GE 1 THEN BEGIN
                             Widget_Control, event.id, DRAW_MOTION_EVENTS = 0
                             
                             CASE self.settingQBounds OF
                             0 : BEGIN                             
                                   self.rubberBand.GetProperty, DATA = data
                                   IF N_Elements(data) EQ 0 OR event.release NE 1 THEN RETURN
                                   self.rubberBand.SetProperty, DATA = FltArr(2,5), HIDE = 1
                                   self.oContour.GetProperty, GEOM=geom, DATA_VALUES=contourData
                                   zoomPosOld = self.zoomPos
                                   IF Size((self.initY)[self.yPlotType], /TNAME) EQ 'STRING' THEN plotY = IndGen(N_Elements((self.initY)[self.yPlotType])) ELSE plotY = (self.initY)[self.yPlotType]  
                                   IF self.irregular THEN self.zoomPos = List(Where(geom[0,*] GT Min(data[0,*]) AND geom[0,*] LT Max(data[0,*]) AND geom[1,*] GT Min(data[1,*]) AND geom[1,*] LT Max(data[1,*]))) $
                                                     ELSE self.zoomPos = List(Where(self.initX.toArray() GT Min(data[0,*]) AND self.initX.toArray() LT Max(data[0,*])), Where(plotY GT Min(data[1,*]) AND plotY LT Max(data[1,*])))
                                   result = self.UpdateContour()
                                   IF result EQ 0 THEN self->SetAxes $;, FILENAMES = Widget_Info(Widget_Info(event.top, FIND_BY_UNAME = 'Contour Filename'), /BUTTON_SET) 
                                                  ELSE self.zoomPos = zoomPosOld
                                 END
                             1 : BEGIN
                                   self.settingQBounds = 0
                                 END
                             2 : BEGIN
                                   self.settingQBounds = 0
                                 END
                             3:
                             ENDCASE
                             self.oContourWindow.Draw
                           ENDIF
                         ENDIF
                         IF event.type EQ 2 THEN BEGIN
                           
                           self.oContourWindow.GetProperty, GRAPHICS_TREE = viewObj
                           viewObj.GetProperty, VIEWPLANE_RECT = viewPlane
                           geom = Widget_Info(event.id, /GEOMETRY)
                           x = (event.x/geom.xsize)*viewPlane[2] + viewPlane[0]
                           y = (event.y/geom.ysize)*viewPlane[3] + viewPlane[1]
                           
                           CASE self.settingQBounds OF
                             0 : BEGIN
                                   self.rubberBand.GetProperty, DATA = data
                                   IF N_Elements(data) EQ 0 THEN RETURN
                                   data[1,1] = y
                                   data[*,2] = [x,y]
                                   data[0,3] = x
                                   self.rubberBand.SetProperty, DATA = data
                                 END
                             1 : BEGIN
                                   self.upperQBoundObj.GetProperty, DATA = dataUp
                                   self.lowerQBoundObj.GetProperty, DATA = data
                                   data[0,*] = x < dataUp[0,0] 
                                   self.lowerQBoundObj.SetProperty, DATA = data
                                   Widget_Control, Widget_Info(self.wContourBase,FIND_BY_UNAME = 'Lower Q Bound'), SET_VALUE = String(data[0,0])
                                   self.plotAtQ
                                 END
                             2 : BEGIN
                                   self.lowerQBoundObj.GetProperty, DATA = dataLow
                                   self.upperQBoundObj.GetProperty, DATA = data
                                   data[0,*] = x > dataLow[0,0]
                                   self.upperQBoundObj.SetProperty, DATA = data
                                   Widget_Control, Widget_Info(self.wContourBase,FIND_BY_UNAME = 'Upper Q Bound'), SET_VALUE = String(data[0,0])
                                   self.plotAtQ
                                 END
                             3 : BEGIN
                                   self.lowerQBoundObj.GetProperty, DATA = dataLow
                                   self.upperQBoundObj.GetProperty, DATA = dataUp
                                     
                                   qShift = ((dataUp[0,0] + dataLow[0,0])/2) - x
                                   dataLow[0,*] -= qShift                                    
                                   dataUp[0,*] -= qShift
                                     
                                   self.lowerQBoundObj.SetProperty, DATA = dataLow
                                   self.upperQBoundObj.SetProperty, DATA = dataUp
                                    
                                   Widget_Control, Widget_Info(self.wContourBase,FIND_BY_UNAME = 'Lower Q Bound'), SET_VALUE = String(dataLow[0,0])
                                   Widget_Control, Widget_Info(self.wContourBase,FIND_BY_UNAME = 'Upper Q Bound'), SET_VALUE = String(dataUp[0,0])
                                   self.plotAtQ
                                 END
                           ENDCASE                
                           self.oContourWindow.Draw
                         ENDIF
                       END
    'Blank Draw'  :  BEGIN
                      leafID = Widget_Info(event.drag_id, /TREE_SELECT)
                      IF N_Elements(leafID) GT 1 THEN result = Dialog_Message('You have dropped multiple blanks - using first in list')
                      Widget_Control, leafID[0], GET_VALUE = value
                      self.notify, {CONTOURBLANK, name : value }
                     END
    'NO BLANK'    : BEGIN
                      self.noBlank = event.select
                      result = self.UpdateContour()
                      IF Obj_Valid(self.plotObj) THEN self.plotAtQ
                      self.oContourWindow.Draw
                    END
    'Export ASCII' : BEGIN
                       IF self.irregular EQ 1 THEN BEGIN
                        result = Dialog_Message('Exporting irregular data not supported at this time.')
                        BREAK
                       ENDIF
                       
                       file = Dialog_Pickfile()
                       IF file EQ '' THEN BREAK
                       
                       result = self.updatecontour(q, y, z, YSTRINGS = yStrings)
                       IF yStrings NE !NULL THEN y = yStrings
                       data = StrArr(N_Elements(z[*,0])+1, N_Elements(z[0,*])+1)
                       data[0,0] = (self.yTypeNames)[self.yPlotType]+'/q'
                       data[1:*,1:*]=StrCompress(z)
                       data[0,1:*] = StrCompress(y)
                       data[1:*,0] = StrCompress(q)
                       maxStringLen = StrCompress(Max(StrLen(data)+1),/REMOVE_ALL)
                       OpenW, file_LUN, file, /GET_LUN
                         printf, file_LUN, data, FORMAT = '(' + StrCompress(N_Elements(data[*,0]),/REMOVE_ALL) + 'A' + maxStringLen + ')'
                       Free_LUN, file_LUN
                     END
    'Show Surface': self.plotSurface, /CREATE
    'Lower Q Bound' : BEGIN
                        self.lowerQBoundObj.GetProperty, DATA = data
                        event.object.GetProperty, VALUE = val
                        data[0,*] = val
                        self.lowerQBoundObj.SetProperty, DATA = data
                        self.oContourWindow.Draw
                        self.plotAtQ
                      END
    'Upper Q Bound' : BEGIN
                        self.upperQBoundObj.GetProperty, DATA = data
                        event.object.GetProperty, VALUE = val
                        data[0,*] = val
                        self.upperQBoundObj.SetProperty, DATA = data
                        self.oContourWindow.Draw
                        self.plotAtQ
                      END
    'Interp X'      : BEGIN
                        event.object.GetProperty, VALUE = val
                        IF self.interpX EQ val THEN RETURN
                        self.interpX = (val > 1) < 8
                        event.object.SetProperty, VALUE = self.interpX
                        IF Obj_Valid(self.surfaceObj) THEN self.plotSurface
                      END
    'Interp Y'      : BEGIN
                        event.object.GetProperty, VALUE = val
                        IF self.interpY EQ val THEN RETURN
                        self.interpY = (val > 1) < 8
                        event.object.SetProperty, VALUE = self.interpY
                        IF Obj_Valid(self.surfaceObj) THEN self.plotSurface
                      END 
    'Plot Q'        : self.plotAtQ, /CREATE
    'Export Plot Q' : self.plotAtQ, /SAVE
    'Contour Plot Y': BEGIN
                        self.yPlotType = event.index
                        result  = self.UpdateContour(YTITLE = (self.yTypeNames)[self.yPlotType])
                      END
    ELSE :
  ENDCASE
END

PRO as__saxscontourplot::plotSurface, CREATE = create

  @as_scatterheader.macro

  self.oContour.GetProperty, DATA_VALUES = z, GEOM = geom
  self.yAxis.GetProperty, TITLE = yTitleObj
  yTitleObj.GetProperty, STRINGS = yTitle
    
  ;IF Widget_Info(Widget_Info(event.top, FIND_BY_UNAME='Contour Plot Index'), /BUTTON_SET) THEN yTitle = 'Plot Index'
  ;IF Widget_Info(Widget_Info(event.top, FIND_BY_UNAME='Contour Image Index'), /BUTTON_SET) THEN yTitle = 'Image Index'
  
  IF self.interpX GT 1 OR self.interpY GT 1 THEN BEGIN
    z = interpolate(z,findgen(N_Elements(z[*,0])*self.interpX)/self.interpX,findgen(N_Elements(z[0,*])*self.interpY)/self.interpY, cubic=-.5, /grid)
  ENDIF
 
  textImageGreen = (textImageBlue = (textImageRed = bytarr(N_Elements(z[*,0]), N_Elements(z[0,*]))))
  
  bytZ = bytScl(z)
  palette = idlgrpalette()
  palette.loadct, 34
  FOR i = 0, 255 DO BEGIN
    colour = palette.GetRGB(i)
    textImageRed[Where(bytZ EQ i)] = colour[0]
    textImageGreen[Where(bytZ EQ i)] = colour[1]
    textImageBlue[Where(bytZ EQ i)] = colour[2] 
  
  ENDFOR
  textImage = Transpose([[[textImageRed]],[[textImageGreen]], [[textImageBlue]]],[2,0,1])
  
  IF Obj_Valid(self.surfaceObj) AND ~KeyWord_Set(create) THEN BEGIN
    self.surfaceObj.GetProperty, TEXTURE_IMAGE = textImageObj
    textImageObj.SetProperty, DATA = textImage
    IF self.interpX GT 1 OR self.interpY GT 1 THEN $
      self.surfaceObj.SetData, z, interpolate(Reform(geom[0,*,0]),findgen(N_Elements(z[*,0]))/self.interpX), interpolate(Reform(geom[1,0,*]),findgen(N_Elements(z[0,*]))/self.interpY) $
    ELSE BEGIN
      self.surfaceObj.SetData, z, Reform(geom[0,*,0]), Reform(geom[1,0,*])
    ENDELSE
  ENDIF ELSE BEGIN
    IF self.interpX GT 1 OR self.interpY GT 1 THEN $
        self.surfaceObj = Surface(z, interpolate(Reform(geom[0,*,0]),findgen(N_Elements(z[*,0]))/self.interpX), interpolate(Reform(geom[1,0,*]),findgen(N_Elements(z[0,*]))/self.interpY), TEXTURE_IMAGE = textImage, /TEXTURE_INTERP, XTITLE = 'q ($\AA^{-1}$)',YTITLE = yTitle, ZTITLE = 'Intensity') $
    ELSE $
        self.surfaceObj = Surface(z, Reform(geom[0,*,0]), Reform(geom[1,0,*]), TEXTURE_IMAGE = textImage, /TEXTURE_INTERP, XTITLE = 'q ($\AA^{-1}$)',YTITLE = yTitle, ZTITLE = 'Intensity')
  ENDELSE

END

PRO as__saxscontourplot::plotAtQ, CREATE = create, SAVE = save

  @as_scatterheader.macro

  IF ~(Obj_Valid(self.plotObj) OR KeyWord_Set(create) OR KeyWord_Set(save)) THEN RETURN
                                
  self.lowerQBoundObj.GetProperty, DATA = lowerBound
  self.upperQBoundObj.GetProperty, DATA = upperBound

  range = Where(self.initX.toArray() GT lowerBound[0,0] AND self.initX.toArray() LT upperBound[0,0])
  IF range[0] EQ -1 THEN BEGIN
    void = Min(Abs(self.initX.toArray() - lowerBound[0,0]),range)
    Widget_Control, Widget_Info(self.wContourBase,FIND_BY_UNAME = 'Lower Q Bound'), SET_VALUE = String((self.initX.toArray())[range])
    Widget_Control, Widget_Info(self.wContourBase,FIND_BY_UNAME = 'Upper Q Bound'), SET_VALUE = String((self.initX.toArray())[range])
  ENDIF

  z = Reform(self.initZ.toArray(),self.initX.count(), N_Elements((self.initY)[self.yPlotType]))
  
  IF self.blankZ.count() GT 0 THEN blankz =  Total((self.blankZ.toArray())[range]) ELSE blankz = 0                      
  intensity = Total(z[range,*],1) + (self.noblank-1)*blankZ
  IF Size((self.initY)[self.yPlotType],/TNAME) EQ 'STRING' THEN plotY = Indgen(N_Elements((self.initY)[self.yPlotType])) $
                                                           ELSE plotY = (self.initY)[self.yPlotType]

  IF KeyWord_Set(save) THEN BEGIN
    file = Dialog_Pickfile()
    data = StrArr(2, N_Elements(intensity)+1)
    data[*,0] = ['y', 'Intensity']
    data[0,1:*] = String(plotY)
    data[1,1:*] = String(intensity)
    width = Max(StrLen(plotY)) > Max(StrLen(intensity))
    IF file NE '' THEN BEGIN
      OpenW, fileLUN, file, /GET_LUN
        PrintF, fileLUN, data, FORMAT = '(2A '+ StrCompress(width, /REMOVE_ALL) + '  )' 
      Free_Lun, fileLUN
    ENDIF
    RETURN
  ENDIF

  IF Size((self.initY)[self.yPlotType],/TNAME) EQ 'STRING' THEN BEGIN
    self.yAxis.GetProperty, TICKVALUES = tickValues, TICKTEXT = tickText
    tickText.GetProperty, STRINGS = tickStrings
    IF ~Obj_Valid(self.plotObj) THEN self.plotObj = Plot(plotY, intensity, WINDOW_TITLE = 'scatterBrain Stratigraphic Slice', XTITLE = (self.yTypeNames)[self.yPlotType], XTICKVALUES = tickValues, XTICKNAME = tickStrings, YTITLE = 'Intensity') $
                                ELSE self.plotObj.SetData, plotY, intensity
  ENDIF ELSE BEGIN
    IF ~Obj_Valid(self.plotObj) THEN self.plotObj = Plot(plotY, intensity, WINDOW_TITLE = 'scatterBrain Stratigraphic Slice', XTITLE = (self.yTypeNames)[self.yPlotType], YTITLE = 'Intensity') $
                                ELSE self.plotObj.SetData, plotY, intensity
  ENDELSE

  

END

FUNCTION as__saxscontourplot::scale, z

  @as_scatterheader.macro

  CASE self.scaleType OF 
    0    : RETURN, z
    1    : RETURN, alog10(z)
    2    : BEGIN
             RETURN, z^self.powerValue
           END
    ELSE : 
  ENDCASE
  
END

PRO as__saxscontourplot::SetAxes, YTITLE = yTitle

  self.yAxis.GetProperty, TICKTEXT = tickText, TICKVALUES = tickValues
  tickText.GetProperty, STRINGS = tickStrings

  self.oContour.GetProperty, GEOM = geom, DATA_VALUES = data
  self.oContourWindow.GetProperty, GRAPHICS_TREE = viewObj
  
  x = geom[0,*,0]
  y = geom[1,0,*]
  
  tickType = 'Number'
  FOREACH tS, tickStrings DO BEGIN
    tS = StrCompress(tS,/REMOVE_ALL)
    IF StRegex(tS, '[^0-9\.-]') + StRegex(tS, '\.[0-9-]*\.') + StRegex(tS, '-[0-9\.]*-') NE -3 THEN BEGIN
      tickType = 'String'
      BREAK
    ENDIF 
  ENDFOREACH
  IF tickType EQ 'Number' THEN IF Abs(Total(Double(tickStrings) - tickValues)) GT N_Elements(tickValues)*0.02 THEN tickType = 'String'
  
  xRange = Max(x) - Min(x)
  yRange = Max(y) - Min(y)

  viewObj.SetProperty, VIEWPLANE_RECT = [-0.15 * xRange + Min(x), -0.15 * yRange + Min(y), xRange + .2*xRange, yRange + .2*yRange]
  IF xRange GT 0 THEN self.xAxis.SetProperty, RANGE = [Min(x),Max(x)], LOCATION = [0,Min(y),0], TICKLEN = 0.01*yRange, MINOR = 4
  IF yRange GT 0 THEN self.yAxis.SetProperty, RANGE = [Min(y),Max(y)], LOCATION = [Min(x),0,0], TICKLEN = 0.01*xRange
  
  viewObj.SetProperty, VIEWPLANE_RECT = [-0.15 * xRange + Min(x),-0.15 * yRange + Min(y),xRange+0.2*xRange,yRange+0.2*yRange]
  
  IF KeyWord_Set(yTitle) THEN BEGIN
    self.yAxis.GetProperty, TITLE = titleObj
    titleObj.SetProperty, STRINGS = yTitle
    IF Obj_Valid(self.surfaceObj) THEN self.surfaceObj.SetProperty, YTITLE = yTitle
  ENDIF
    
  ticks = N_Elements(y)
  IF ticks GT 10 THEN BEGIN
    rem = 1
    ticks = 7
    WHILE rem LT 5 AND rem NE 0 DO BEGIN
      ticks++
      rem = (N_Elements(y)-1) MOD (ticks-1)
    ENDWHILE
  ENDIF
  
  skip = Ceil(N_Elements(y)/(ticks-1))
  tickPos = y[0:*:skip]
  self.yAxis.SetProperty, TICKVALUES = tickPos, MAJOR = ticks, MINOR = 0
  IF Obj_Valid(self.surfaceObj) THEN self.surfaceObj.SetProperty, YTICKVALUES = tickPos, YMAJOR = ticks, YMINOR = 0

  IF tickType EQ 'String' THEN BEGIN
    tickText.SetProperty, STRINGS = String(((self.inity)[self.yPlotType])[tickPos])
    self.oContourWindow.Draw
    tickText.GetProperty, XRANGE = textRange
    textWidth = textRange[1] - textRange[0]
    viewObj.GetProperty, VIEWPLANE_RECT = viewPlane
    viewObj.SetProperty, VIEWPLANE_RECT = viewPlane + [-textWidth,0,textWidth,0]
    ;IF Obj_Valid(self.surfaceObj) THEN self.surfaceObj.SetProperty, YTITLE = 'Filenames (Plot Index)', YTICKNAME = (self.filenames.toArray())[tickPos] 
        
  ENDIF
;    title.SetProperty, STRINGS = 'Plot Index'
;    IF Obj_Valid(self.surfaceObj) THEN self.surfaceObj.SetProperty, YTITLE = 'Plot Index', YTICKNAME = ''
;  ENDELSE
  

  self.lowerQBoundObj.GetProperty, DATA = data
  data[1,* ] = [min(y),max(y)]
  self.lowerQBoundObj.SetProperty, DATA = data
  
  self.upperQBoundObj.GetProperty, DATA = data
  data[1,* ] = [min(y),max(y)]
  self.upperQBoundObj.SetProperty, DATA = data
     
  self.oContourWindow.Draw

END

PRO as__saxscontourplot::ImageIndex

  @as_scatterheader.macro

  self.zoomOut
  imageIndex = IntArr(self.plotIndex.count())
  FOREACH fname, self.fileNames, key DO BEGIN
    imageIndex[key] = Fix(StrMid(fname, 7,4 ,/REVERSE_OFFSET))  
  ENDFOREACH
 
;  self.oContour.GetProperty, GEOM = geom, DATA_VALUES = data
;  yContourTemp = (yContour = geom[1,*])
;  xContour = geom[0,*]
;     
;  FOR i = 0, key-1 DO BEGIN
;    yContourTemp[Where(yContour EQ i)] = imageIndex[i] 
;  ENDFOR
;       
;  yContour = yContourTemp
;      
;  self.initY = List(yContour, /EXTRACT)
;      
;  result = self.UpdateContour(xContour, yContour, data)

  self.initY = List(imageIndex, /EXTRACT) 
  result = self.UpdateContour()
                                  
  IF result EQ 0 THEN BEGIN
    self.SetAxes
    self.yAxis.GetProperty, TITLE = title 
    title.SetProperty, STRINGS = 'Image Index'
    IF Obj_Valid(self.plotObj) THEN self.plotAtQ
    self.oContourWindow.Draw
  ENDIF

END

PRO as__saxscontourplot::PlotIndex, FILENAMES = fileNames

  @as_scatterheader.macro

  self.zoomOut
  
  
  IF self.irregular THEN BEGIN
  
;  self.oContour.GetProperty, GEOM = geom, DATA_VALUES = data
;  yContourTemp = (yContour = geom[1,*])
;  xContour = geom[0,*]
;  
;  startPoint = 0
;  FOR i = 0, N_Elements(self.plotIndex)-1 DO BEGIN
;    yContourTemp[Where(yContour EQ yContour[startPoint], numPoints)] = i
;    startPoint += numPoints
;  ENDFOR
;   
;  yContour = yContourTemp
;  
;  self.initY = List(yContour, /EXTRACT) 
;   
;  result = self.updateContour(xContour, yContour, data)
   
  ENDIF ELSE BEGIN
    self.initY = List(self.plotIndex, /EXTRACT)
    result = self.updateContour()
  ENDELSE
  IF Obj_Valid(self.plotObj) THEN self.plotAtQ
  self.SetAxes, FILENAMES = KeyWord_Set(fileNames)
  
END

PRO as__saxscontourplot::Notify, event

  @as_scatterheader.macro

   FOREACH notify, self.notifyObj DO IF Obj_Valid(notify) THEN notify.notify, event

END

PRO as__saxscontourplot::ZoomOut

  @as_scatterheader.macro

  x = self.initX.toArray()
  y = (self.initY)[self.yPlotType];.toArray()
  z = self.initZ.toArray()
 
  IF self.irregular THEN self.zoomPos = List(Indgen(self.initX.count())) ELSE self.zoomPos = List(Indgen(self.initX.count()),Indgen(N_Elements((self.initY)[self.yPlotType])))
 
  result = self.UpdateContour()
 
  IF result EQ 0 THEN BEGIN
  
    self.SetAxes
    self.oContourWindow.Draw
   
  ENDIF  

END

PRO as__saxscontourplot::SetBlank, blank, name

  @as_scatterheader.macro

  self.oBlankText.SetProperty, STRINGS = name
  self.oBlankWindow.Draw
  self.oBlankText.GetProperty, XRANGE = xRange, FONT = font
  WHILE xRange[0] LT 0 OR xRange[1] GT 1 DO BEGIN
    font.GetProperty, SIZE = size
    font.SetProperty, SIZE = SIZE*.95
    self.oBlankWindow.Draw
    self.oBlankText.GetProperty, XRANGE = xRange
  ENDWHILE
  
  IF Total(blank[0,*] - self.initx.toArray()) LT 0.0001 THEN self.blankz = List(blank[1,*]) $
                                                        ELSE self.blankZ = List(Interpol(Reform(blank[1,*]), Reform(blank[0,*]), self.initX.toArray()),/EXTRACT)
  
  void = self.UpdateContour()
  self.oContourWindow.Draw
  
END

FUNCTION as__saxscontourplot::UpdateContour, x, y, z, YTITLE = yTitle, YSTRINGS = yStrings

  @as_scatterheader.macro

  IF self.irregular EQ 0 THEN BEGIN
  
    x = Reform((self.initX.toArray())[(self.zoomPos)[0]])
    y = Reform(((self.initY)[self.yPlotType])[(self.zoomPos)[1]])
    IF Size(y,/TNAME) EQ 'STRING' THEN BEGIN
      yStrings = y
      y = IndGen(N_Elements(y))
    ENDIF
    IF Max(x) - Min(x) EQ 0 OR Max(y) - Min(y) EQ 0 THEN BEGIN
      result = Dialog_Message('Q Range is zero, plot not updated.')
      RETURN, -1
    ENDIF
    zoomPos = Reform((self.zoomPos)[0]#Replicate(1,N_Elements(y)) + (self.zoomPos)[1]##Replicate(self.initx.count(), N_Elements(x)),N_Elements(x#y))
    z = Reform(Reform((self.initZ.toArray())[zoomPos]) + (self.noBlank-1)*Reform(Reform((self.blankZ.toArray())[(self.zoomPos)[0]])#Replicate(1,N_Elements(y)),N_Elements(x#y)), N_Elements(x), N_Elements(y))
    reducePoints = 1 + Fix(N_Elements(z) / 1E5)
    self.yAxis.GetProperty, TICKTEXT = tickText
    self.yAxis.SetProperty, RANGE = [Min(y[0:*:reducePoints]),Max(y[0:*:reducePoints])] 
    self.oContour.SetProperty, DATA_VALUES = self.scale(z[0:*:reducePoints,0:*:reducePoints]), GEOMX = x[0:*:reducePoints], GEOMY= y[0:*:reducePoints] 
    
    IF N_Elements(yStrings) GT 0 THEN tickText.SetProperty, STRINGS = yStrings[0:*:reducePoints] 
      
    CATCH, surfacePlotError
    IF surfacePlotError NE 0 THEN BEGIN
      CATCH, /CANCEL
      print, surfacePlotError
    ENDIF ELSE BEGIN
      IF Obj_Valid(self.surfaceObj) THEN BEGIN
        self.plotSurface
        ;yTitle = 'Plot Index'
        ;IF Widget_Info(Widget_Info(self.wContourBase, FIND_BY_UNAME='Contour Plot Index'), /BUTTON_SET) THEN yTitle = 'Plot Index'
        ;IF Widget_Info(Widget_Info(self.wContourBase, FIND_BY_UNAME='Contour Image Index'), /BUTTON_SET) THEN yTitle = 'Image Index'
;        self.surfaceObj.SetProperty, VERT_COLORS=bytscl(z[0:*:reducePoints,0:*:reducePoints]), YTITLE = yTitle
;        self.surfaceObj.SetData, self.scale(z[0:*:reducePoints,0:*:reducePoints]), x[0:*:reducePoints], y[0:*:reducePoints]
      ENDIF
   ENDELSE
  
  ENDIF ELSE BEGIN

    CATCH, triangulateError
  
    IF triangulateError NE 0 THEN BEGIN
      CATCH, /CANCEL
      RETURN, -1
    ENDIF
  
    IF N_Params() EQ 0 THEN BEGIN
      x = Reform((self.initX.toArray())[self.zoomPos.toArray()])
      y = Reform((self.initY.toArray())[self.zoomPos.toArray()])
      z = Reform((self.initZ.toArray())[self.zoomPos.toArray()]) + (self.noBlank-1)*Reform((self.blankZ.toArray())[self.zoomPos.toArray()])
    ENDIF
  
    Triangulate, x, y, tri
    dims = Size(tri, /DIMENSIONS)
    poly = LonArr(4, dims[1])
    poly[0,*] = 3
    poly[1:3, *] =tri
    poly = reform(poly, N_Elements(poly), /OVERWRITE)
    self.oContour.SetProperty, DATA_VALUES = self.scale(z), GEOMX = x, GEOMY= y, polygons = poly 
    
  ENDELSE
  
  self.setaxes, YTITLE = yTitle
  
  RETURN, 0

END

FUNCTION as__saxscontourplot::Init, x, y, z, FILENAMES=fileNames, NOTIFYOBJ = notifyObj, IRREGULAR = irregular
  
  @as_scatterheader.macro
  
  IF KeyWord_Set(irregular) THEN BEGIN
  
    IF N_Elements(x) NE N_Elements(x) OR N_Elements(x) NE N_Elements(z) THEN BEGIN
      result = Dialog_Message('For irregular grid, elements of x, y, and z must be equal.')
      RETURN, -1
    ENDIF
    
    self.irregular = 1
    
  ENDIF ELSE BEGIN
      
      IF Size(y, /TNAME) EQ 'STRUCT' THEN ysize = N_Elements(y.(0)) ELSE ysize = N_Elements(y)
      
      IF N_Elements(x) NE N_Elements(z[*,0]) OR ysize NE N_Elements(z[0,*]) THEN BEGIN
        result = Dialog_Message('Number of elements do not agree.')
        RETURN, -1
      ENDIF
    
    self.irregular = 0
          
  ENDELSE
  
  self.initX = List(x, /EXTRACT)
  IF Size(y,/TNAME) EQ 'STRUCT' THEN BEGIN
    self.yTypeNames = List(Tag_Names(y),/EXTRACT)
    self.initY = List()
    FOR i = 0, N_Elements(self.yTypeNames) - 1 DO self.initY.Add, y.(i)
    y = y.(0)
  ENDIF ELSE BEGIN
    self.initY = List(y, /EXTRACT)
    self.yTypeNames = List()
  ENDELSE
  self.initZ = List(z, /EXTRACT)
  self.blankZ = List(0)
  
  self.powerValue = 1
  self.interpX = 1
  self.interpY = 1
  
  IF KeyWord_Set(fileNames) THEN self.fileNames = List(fileNames, /EXTRACT)
  
  IF Keyword_Set(notifyObj) THEN $
    IF TypeName(notifyObj[0]) EQ 'NOTIFY' $
      THEN self.notifyObj = List(notifyObj,/EXTRACT)
  
  ;hist = Histogram(y, LOCATIONS = yValues)
  ;self.plotIndex = List(yValues[Where(hist NE 0)], /EXTRACT)
  self.plotIndex = List(IndGen(N_Elements(y)),/EXTRACT)
  
  wContourBase = Widget_Base(/ROW, TITLE = 'scatterBrain Contour Plot', /TLB_SIZE_EVENTS)
  self.wContourBase = wContourBase
  WIDGET_CONTROL, wContourBase, SET_UVALUE = self
  wControlBase = Widget_Base(wContourBase, /COLUMN, UNAME = 'Contour Type Base')
  wAxisTypeLabel = Widget_Label(wControlBase, VALUE = 'Y-Axis Type')
  IF self.yTypeNames.count() GT 0 THEN BEGIN
    yType = Widget_Combobox(wControlBase, VALUE = self.yTypeNames.toArray(), UNAME = 'Contour Plot Y')
  ENDIF
  wScaleTypeBase = Widget_Base(wControlBase, /COLUMN, UNAME = 'Scale Type Base',/FRAME)
  wScaleTypeLabel = Widget_Label(wScaleTypeBase, VALUE = 'Scale Type')
  wScaleTypeButtonBase = Widget_Base(wScaleTypeBase, /EXCLUSIVE)
  linearScale = Widget_Button(wScaleTypeButtonBase, VALUE = 'Linear Scale', UNAME = 'Linear Scale')
  logScale = Widget_Button(wScaleTypeButtonBase, VALUE = 'Log Scale', UNAME = 'Log Scale')
  powerScale = Widget_Button(wScaleTypeButtonBase, VALUE = 'Power Scale', UNAME = 'Power Scale')
  powerSlider = CW_FSlider(wScaleTypeBase, MINIMUM = 0.1, VALUE = 1, MAXIMUM = 1, SCROLL = 0.1, /EDIT, FORMAT = '(G9.2)', UNAME = 'Power Value')
  wBlankBase = Widget_Base(wControlBase, /COLUMN, /FRAME)
  wBlankDraw = Widget_Draw(wBlankBase, XSIZE = 140, YSIZE = 40, GRAPHICS_LEVEL = 2, /DROP_EVENTS, /FRAME, UNAME = 'Blank Draw')
  wBlankBaseBut = Widget_Base(wBlankBase, /NONEXCLUSIVE)
  wBlankDisable = Widget_Button(wBlankBaseBut, VALUE = 'Disable Blank', UNAME = 'NO BLANK')
  wExportContour = Widget_Button(wControlBase, VALUE = 'Export to ASCII', UNAME = 'Export ASCII')
  wSurfaceBase = Widget_Base(wControlBase, /FRAME, /COLUMN)
  wInterpBase = Widget_Base(wSurfaceBase, /ROW)
  wInterpX = FSC_Field(wInterpBase, TITLE = 'Interp X', DECIMAL = 0, VALUE = 1, XSIZE = 6, /CR_ONLY, EVENT_PRO = 'as__saxscontourplot_event', /COLUMN, UVALUE = 'Interp X')
  wInterpY = FSC_Field(wInterpBase, TITLE = 'Interp Y', DECIMAL = 0, VALUE = 1, XSIZE = 6, /CR_ONLY, EVENT_PRO = 'as__saxscontourplot_event', /COLUMN, UVALUE = 'Interp Y')
  wShowSurface = Widget_Button(wSurfaceBase, VALUE = 'Show Surface Plot', UNAME = 'Show Surface')
  wStratBase = Widget_Base(wControlBase, /FRAME, /COLUMN)
  wQBoundBase = Widget_Base(wStratBase, /ROW)
  wLowQBound = FSC_Field(wQBoundBase,VALUE = min(x), TITLE = 'Min q', DECIMAL = 4, XSIZE = 8, EVENT_PRO = 'as__saxscontourplot_event', /COLUMN, UVALUE = 'Lower Q Bound', UNAME = 'Lower Q Bound')
  wUpperQBound = FSC_Field(wQBoundBase, VALUE = max(x), TITLE = 'Max q', DECIMAL = 4, XSIZE = 8, EVENT_PRO = 'as__saxscontourplot_event', /COLUMN, UVALUE = 'Upper Q Bound', UNAME = 'Upper Q Bound')
  wPlotQ = Widget_Button(wStratBase, VALUE = 'Plot Stratigraphic Slice', UNAME = 'Plot Q')
  wExportPlotQ = Widget_Button(wStratBase, VALUE = 'Export Stratigraphic Slice', UNAME = 'Export Plot Q')
  wContourDraw = Widget_Draw(wContourBase, XSIZE = 640, YSIZE = 480, GRAPHICS_LEVEL = 2, /BUTTON_EVENTS, UNAME = 'Contour Draw')
  
  Widget_Control, linearScale, /SET_BUTTON
  WIDGET_CONTROL, wContourBase, /REALIZE  
  
  ; Setup Blank Drop Draw_Widget
  Widget_Control, wBlankDraw, GET_VALUE=oBlankWindow
  self.oBlankWindow = oBlankWindow
  oBlankView = IDLgrView(VIEWPLANE_RECT = [0,0,1,1], COLOR = [200,200,200])
  oBlankFont = IDLgrFont('helvetica*bold')  
  oBlankText = IDLgrText('Drop Blank Here', FONT = oBlankFont, COLOR = [255,0,0], LOCATIONS = [0.5,0.5], ALIGNMENT = 0.5, VERTICAL_ALIGNMENT = 0.5)
  self.oBlankText = oBlankText
  oBlankModel = IDLgrModel()
  oBlankModel.Add, oBlankText
  oBlankView.Add, oBlankModel
  oBlankWindow.SetProperty, GRAPHICS_TREE = oBlankView
  oBlankWindow.Draw
  
  
  ; Retrieve the newly-created Window object.
  WIDGET_CONTROL, wContourDraw, GET_VALUE=oContourWindow
  self.oContourWindow = oContourWindow
  
  IF ~KeyWord_Set(irregular) THEN self.oContour = IDLgrContour(z, GEOMX = x, GEOMY = y) ELSE BEGIN

    self.oContour = IDLgrContour()
    result = self.UpdateContour(x, y, z)
  
  ENDELSE
  
  contourPalette = IDLgrPalette()
  contourPalette.LoadCT, 34
  
  self.oContour.SetProperty, PLANAR = 1, GEOMZ = -.01, N_LEVELS = 24, /FILL, PALETTE = contourPalette, C_COLOR = IndGen(24)*255/23.
  
  xTitle = IDLgrText('q (' + String("305B) + '!U-1!N)', /ENABLE_FORMATTING, RECOMPUTE_DIMENSIONS = 2)
  yTitle = IDLgrText((self.yTypeNames)[0], RECOMPUTE_DIMENSIONS = 2)
  
  self.xAxis = IDLgrAxis(TITLE=xTitle, LOCATION = [0,Min(y),0],RANGE = [Min(x), Max(x)], /EXACT, TICKDIR=1)
  self.yAxis = IDLgrAxis(1,TITLE=yTitle, LOCATION = [Min(x),0,0],RANGE = [Min(y),Max(y)], /EXACT, TICKDIR=1);, MINOR = 0, TICKLEN = .02)
 
  self.xAxis.GetProperty, TICKTEXT = xAxisText
  self.yAxis.GetProperty, TICKTEXT = yAxisText
  
  xAxisText.SetProperty, RECOMPUTE_DIMENSIONS = 1
  yAxisText.SetProperty, RECOMPUTE_DIMENSIONS = 1
 
  self.rubberBand = IDLgrPolyline()
  self.lowerQBoundObj = IDLgrPolyline([min(x), min(x)],[min(y),max(y)],COLOR = [255,0,0])
  self.upperQBoundObj = IDLgrPolyline([max(x), max(x)],[min(y),max(y)],COLOR = [255,0,0])
 
  viewObj = IDLgrView()
  modelObj = IDLgrModel()
       
  modelObj.Add, self.xAxis
  modelObj.Add, self.yAxis
  modelObj.Add, self.rubberBand
  modelObj.Add, self.oContour 
  modelObj.Add, self.lowerQBoundObj
  modelObj.Add, self.upperQBoundObj
  
  viewObj.Add, modelObj
    
 
  self.oContourWindow.SetProperty, GRAPHICS_TREE = viewObj
  
  self.zoomout
  self.SetAxes
    
  self.oContourWindow.Draw, viewObj
  
  XManager, 'as__saxscontourplot', wContourBase, /NO_BLOCK
  
  RETURN, 1

END

PRO as__saxscontourplot__Define

  void = { as__saxscontourplot, $
           oContourWindow : Obj_New(), $
           oContour       : Obj_New(), $
           oBlankText     : Obj_New(), $
           oBlankWindow   : Obj_New(), $
           xAxis          : Obj_New(), $
           yAxis          : Obj_New(), $
           rubberBand     : Obj_New(), $
           surfaceObj     : Obj_New(), $
           plotObj        : Obj_New(), $
           lowerQBoundObj : Obj_New(), $
           upperQBoundObj : Obj_New(), $
           notifyObj      : List(), $
           initX          : List(),  $
           initY          : List(),  $
           initZ          : List(),  $
           blankZ         : List(),  $
           plotIndex      : List(),  $
           fileNames      : List(),  $
           zoomPos        : List(),  $
           yTypeNames     : List(),  $
           yPlotType      : 0,       $
           wContourBase   : 0l,      $
           irregular      : 0,       $
           scaleType      : 0,       $
           powerValue     : 0.1,     $
           noBlank        : 0,       $
           settingQBounds : 0,       $
           interpX        : 1,       $
           interpY        : 1        $
           }

END
