PRO AS_ImageMosaic_event, event

  @as_scatterheader.macro

  Widget_Control, event.handler, GET_UVALUE = AS_ImageMosaic
  AS_ImageMosaic.Event, event
END

PRO AS_ImageMosaic_PaletteUpdate, DATA = as_imageMosaic
COMMON COLORS, R_orig, G_orig, B_orig, R_curr, G_curr, B_curr

  @as_scatterheader.macro

  as_imagemosaic.paletteupdate, R_curr, G_curr, B_curr
END

PRO AS_ImageMosaic::Event, event

  @as_scatterheader.macro

  widgetName = Widget_Info(event.id, /UNAME)

  CASE widgetName OF
    'Mosaic Base' : BEGIN
                      IF Tag_Names(event, /STRUCTURE) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
                        Obj_Destroy, self
                        RETURN
                      ENDIF
;                      Widget_Control, Widget_Info(event.id, FIND_BY_UNAME = 'Mosaic Draw'), XSIZE = event.x, YSIZE = event.y
;                      self.drawWindow.Draw
                    END
    'Mosaic Draw' : BEGIN
                      IF event.press THEN BEGIN
                        self.drawWindow.GetProperty, GRAPHICS_TREE = viewObj
                        selectedObjects = self.drawWindow.Select(viewObj, [event.x, event.y])
                        FOREACH selected, selectedObjects DO BEGIN
                          IF ISA(selected, 'IDLGRIMAGE') THEN BEGIN
                            selected.GetProperty, PARENT = model
                          ENDIF
                        ENDFOREACH
                        IF Obj_Valid(model) THEN BEGIN 
                          models = viewObj.Get(/ALL)
                          modelNo = Where(models EQ model)
                          viewObj.move, modelNo, viewObj.count()-1
                          self.dragging.dragModel = model
                          self.dragging.dragStart = [event.x, event.y]
                          Widget_Control, event.id, DRAW_MOTION_EVENTS = 1
                        ENDIF
                      ENDIF
                      
                      IF Obj_Valid(self.dragging.dragModel) THEN BEGIN
                        self.drawWindow.GetProperty, GRAPHICS_TREE = viewObj
                        viewObj.GetProperty, VIEWPLANE_RECT = rect
                        oldZoom = self.viewplane[3]/rect[3]
                        self.dragging.dragModel.Translate, (event.x - self.dragging.dragStart[0]) / oldzoom, (event.y - self.dragging.dragStart[1]) / oldzoom, 0
                        self.dragging.dragStart = [event.x, event.y]
                        self.drawWindow.draw
                        
                      ENDIF
                      
                      IF event.release THEN BEGIN
                        
                        self.drawWindow.GetProperty, GRAPHICS_TREE = viewObj
                        selectedObjects = self.drawWindow.Select(viewObj, [event.x, event.y])
                        FOREACH selected, selectedObjects DO BEGIN
                          IF ISA(selected, 'IDLGRIMAGE') THEN BEGIN
                            selected.GetProperty, PARENT = model
                            IF Obj_Valid(model) THEN BEGIN 
                              IF model NE self.dragging.dragModel THEN dropModel = model
                            ENDIF
                          ENDIF
                        ENDFOREACH
                        IF Obj_Valid(dropModel) THEN BEGIN
                          dropModelNo = Where(self.imagesArrObj EQ dropModel)
                          orderDropModelNo = (self.order)[dropModelNo]
                          modelNo = Where(self.imagesArrObj EQ self.dragging.dragModel)
                          orderModelNo = (self.order)[modelNo]
                        
                          mode = 0
                          
                          IF orderDropModelNo LT orderModelNo THEN mode += 1
                          IF orderDropModelNo GT orderModelNo THEN mode += 2
                          
                          FOREACH order, self.order, key DO BEGIN
                            IF order EQ orderModelNo THEN (self.order)[key] = orderDropModelNo
                            IF order GT orderModelNo THEN BEGIN
                              IF order GT orderDropModelNo THEN CONTINUE
                              IF order LE orderDropModelNo THEN BEGIN
                                (self.order)[key] = order - 1
                                CONTINUE
                              ENDIF
                            ENDIF
                            IF order LT orderModelNo THEN BEGIN
                              IF order GE orderDropModelNo THEN BEGIN 
                                (self.order)[key] = order + 1
                              ENDIF
                              IF order LT orderDropModelNo THEN CONTINUE
                            ENDIF
                          ENDFOREACH
                        ENDIF
                        
                        self.dragging.dragModel = Obj_New()
                        Widget_Control, event.id, DRAW_MOTION_EVENTS = 0
                        
                        self.Display, /MOVEONLY
                      ENDIF
                      
                    END
    ELSE          : Print, 'Not Implemented'
  ENDCASE 

END

PRO AS_ImageMosaic::SetProperty, IMAGES = images, COLUMNS = columns, ROWS = rows, PALETTE = palette, ZOOM = zoom, SCALEINDIVIDUALLY = scaleIndividually

  @as_scatterheader.macro

  IF ISA(palette, 'IDLGRPALETTE') THEN BEGIN
    Obj_Destroy, self.palette
    self.palette = palette
    self.drawWindow.SetProperty, PALETTE=palette
    self.drawWindow.Draw
  ENDIF ELSE IF ISA(palette, /NUMBER) THEN BEGIN
    IF palette GE 0 AND palette LE 40 THEN self.palette.LoadCT, palette
    self.drawWindow.Draw
  ENDIF
  
  IF KeyWord_Set(zoom) THEN BEGIN
    self.drawWindow.GetProperty, GRAPHICS_TREE = viewObj
    viewObj.SetProperty, VIEWPLANE_RECT = self.viewplane/zoom
    oldxsize = (xsize = (self.viewplane*zoom)[2])
    oldysize = (ysize = (self.viewplane*zoom)[3])
    
    self.FixXYSize, xsize, ysize
    
    self.drawWindow.SetProperty, DIMENSIONS = [xsize,ysize]
    Widget_Control, Widget_Info(self.wBase, FIND_BY_UNAME = 'Mosaic Draw'), XSIZE =xsize, YSIZE = ysize, DRAW_XSIZE = self.viewplane[2], DRAW_YSIZE = self.viewplane[3], SET_DRAW_VIEW = [0,0]  
    
    self.drawWindow.draw
  ENDIF
  
  IF N_Elements(scaleIndividually) GT 0 THEN self.scaleIndividually = KeyWord_Set(scaleIndividually)
  
  IF ~KeyWord_Set(columns) AND ~KeyWord_Set(rows) AND ~KeyWord_Set(images) AND N_Elements(scaleIndividually) EQ 0 THEN RETURN 

  IF KeyWord_Set(columns) THEN self.columns = columns
  IF KeyWord_Set(rows) THEN self.rows = rows
  IF KeyWord_Set(images) THEN BEGIN
  
    IF ISA(images, 'LIST') THEN self.imagesArr = images ELSE BEGIN

      self.imagesArr.Remove, /ALL
      sizeImages = Size(images)
      CASE sizeImages[0] OF
        2 : self.imagesArr = List(images)
        3 : FOR i = 0, sizeImages[1] - 1 DO self.imagesArr.Add, Reform(images[i,*,*])
        4 : FOR i = 0, sizeImages[1] - 1 DO self.imagesArr.Add, Reform(images[i,*,*,*])    
        ELSE :
      ENDCASE
    ENDELSE
    
  ENDIF
  
  self.Display
  
END

PRO AS_ImageMosaic::Save, USEZOOM = useZoom

  @as_scatterheader.macro

  filters = ['*.jpg', '*.tif', '*.png']
  fileName = Dialog_Pickfile(TITLE = 'Choose filename.', /WRITE, /OVERWRITE_PROMPT, DEFAULT_EXTENSION = 'jpg', FILTER = filters)
  
  IF fileName EQ '' THEN RETURN

  self.drawWindow.GetProperty, GRAPHICS_TREE = viewObj
  viewObj.GetProperty, VIEWPLANE_RECT = rect
  oldZoom = self.viewplane[3]/rect[3]
  IF ~KeyWord_Set(useZoom) THEN BEGIN
    self.SetProperty, zoom = 1
    (self.drawWindow.Read()).GetProperty, DATA = image
    self.SetProperty, zoom = oldzoom
  ENDIF ELSE BEGIN
    (self.drawWindow.Read()).GetProperty, DATA = image
    image = image[*,0:oldZoom*self.viewplane[2],0:oldZoom*self.viewplane[3]]
  ENDELSE
  
    
  fileExtension = StrUpCase((StrSplit(fileName, '.', /EXTRACT))[-1])
  
  SWITCH fileExtension OF
    'JPG' :  
    'JPEG': BEGIN
              Write_JPeg, fileName, image, /TRUE
              BREAK
            END
    'TIF' : 
    'TIFF': BEGIN
              Write_Tiff, fileName, Reverse(image,3) 
              BREAK
            END
    'PNG' : BEGIN
              Write_PNG, fileName, image 
              BREAK
            END
  ENDSWITCH

END

PRO AS_ImageMosaic::PaletteUpdate, R, G, B

  @as_scatterheader.macro

  self.palette.SetProperty, RED_VALUES = R, GREEN_VALUES = G, BLUE_VALUES = B
  self.drawWindow.draw

END

PRO AS_ImageMosaic::ShowHideLabels, show

  @as_scatterheader.macro

  models = self.imagesArrObj.Get(/ALL)
  FOREACH model, models DO BEGIN
    IF ISA(model, 'IDLGRMODEL') THEN BEGIN
      objects = model.Get(/ALL)
      FOREACH object, objects DO IF ISA(object, 'IDLGRTEXT') THEN object.SetProperty, HIDE = ~show
    ENDIF
  ENDFOREACH
  self.drawWindow.Draw
END

PRO AS_ImageMosaic::Scale, LINEAR = linear, LOG=log, SQRT = sqrt, TOPCHOP = topChop, MINVALUE = minValue

  @as_scatterheader.macro
  
  IF KeyWord_Set(log) THEN self.scale = 'LOG'
  IF KeyWord_Set(sqrt) THEN self.scale = 'SQRT'
  IF KeyWord_Set(linear) THEN self.scale = 'LINEAR'
  IF N_Elements(topChop) THEN self.topChop = topChop
  IF N_Elements(minValue) THEN self.minValue = minValue
  self.Display

END

PRO AS_ImageMosaic::Display, MOVEONLY = moveOnly

  @as_scatterheader.macro

  images = self.imagesArr

  IF N_Elements(images) GT 0 THEN BEGIN
    imageX = (imageY = 0)
    FOREACH image, images DO BEGIN
      dims = (Size(image))[1:2]
      imageX = imageX > dims[0]
      imageY = imageY > dims[1]
    ENDFOREACH

    columns = self.columns
    rows = self.rows

    xSize = imageX*columns
    ySize = imageY*rows
    
    posXArray = Reform(Replicate(1,rows)##Indgen(columns)*imageX,rows*columns)
    posYArray = Reform(Replicate(1,columns)#Indgen(rows)*imageY,rows*columns)
    
    posArray = Transpose([[posXArray],[posYArray]])
    
    IF KeyWord_Set(moveOnly) THEN BEGIN
    
      FOREACH object, self.imagesArrObj, key DO BEGIN
        
        object.GetProperty, TRANSFORM = trans
        trans[3,0] = 0
        trans[3,1] = 0
        object.SetProperty, TRANSFORM = trans
        object.Translate,posArray[0,(self.order)[key]],posArray[1,(self.order)[key]],0
      
      ENDFOREACH
      
      self.drawWindow.Draw
      RETURN
    ENDIF
    
    IF ~self.imagesArrObj.isEmpty() THEN self.imagesArrObj.Remove, /ALL
    
    IF ~self.scaleIndividually THEN BEGIN
      image[where(image LT 0)] = 0
      imageMax = (imageMin = 0)
      FOREACH image, images, key DO BEGIN
        a = histogram(image,REVERSE_INDICES = r)
        b = r[0:r[0]-1]
        max = (where(b gt b[-1]-self.topChop))[0]
        imageMax = imageMax > max
        ;IF key EQ 0 THEN imageMin = min ELSE imageMin = imageMin < min
      ENDFOREACH
    ENDIF
    
    imageMin = self.minValue
    
    FOREACH image, images, key DO BEGIN
      image[where(image LT 0)] = 0
      IF key GE rows*columns THEN BREAK
      void = IDLgrModel()
      IF (self.order)[key] GT rows*columns - 1 THEN CONTINUE
      void.Translate,posArray[0,(self.order)[key]],posArray[1,(self.order)[key]],0
     
      IF self.scaleIndividually THEN BEGIN
        image[where(image gt 2^21.)] = 0
        a = histogram(image,REVERSE_INDICES = r)
        b = r[0:r[0]-1]
        imageMax = (where(b gt b[-1]-self.topChop))[0]
      ENDIF
    
      CASE self.scale OF
        'LOG'    : BEGIN 
                     image = ALog10(image)
                     image[Where(Finite(image) EQ 0)] = 0
                     image = scale_vector(image,0,255, MINVALUE = ALog10(imageMin), MAXVALUE = ALog10(imageMax))
                   END
        'SQRT'   : image = scale_vector(SqRt(image),0, 255, MINVALUE = SqRt(imageMin), MAXVALUE = SqRt(imageMax))
        ELSE     : image = scale_vector(image, 0, 255, MINVALUE = imageMin, MAXVALUE = imageMax)
      ENDCASE
     
      ;IF ~self.scaleIndividually THEN image = Fix(255*(image-imageMin)/Float(imageMax)) ELSE image = BytScl(image)
     
      void.Add, IDLgrImage(image, palette = self.palette)
      IF ~self.labels.isEmpty() THEN BEGIN
        IF self.labels.count() GT key THEN BEGIN
          void.Add, IDLgrText((self.labels)[key], COLOR = [255,255,0], CHAR_DIMENSIONS = [0.07*imageX,0.07*imageX], LOCATIONS = [0.05*imageX,imageY-0.1*imageY])
        ENDIF
      ENDIF
      self.imagesArrObj.Add, void
    ENDFOREACH
  ENDIF
  
  oldXsize = xsize
  oldYsize = ysize
  self.FixXYSize, xsize, ysize
  
  self.drawWindow.SetProperty, DIMENSIONS = [xsize,ysize]
  Widget_Control, Widget_Info(self.wBase, FIND_BY_UNAME = 'Mosaic Draw'), XSIZE =xsize, YSIZE = ysize, DRAW_XSIZE = oldxsize, DRAW_YSIZE = oldysize, SET_DRAW_VIEW = [0,0]

  self.drawWindow.GetProperty, GRAPHICS_TREE = viewObj
  
  viewObj.GetProperty, VIEWPLANE_RECT = rect
  oldZoom = self.viewplane[3]/rect[3]
  
  self.viewplane = [0,0,oldxsize ,oldysize]
  viewObj.SetProperty, VIEWPLANE_RECT = self.viewplane
  existingImageObj = viewObj.Get(/ALL)
  IF Size(existingImageObj[0],/TYPE) EQ 11 THEN FOREACH existingImage, existingImageObj DO Obj_Destroy, existingImage
  FOREACH imageObj, self.imagesArrObj DO viewObj.add, imageObj
  
  Widget_Control, Widget_Info(self.wBase, FIND_BY_UNAME = 'Mosaic Draw'), SET_DRAW_VIEW = [0,0]
  
  self.SetProperty, zoom = oldzoom
  
  self.drawWindow.Draw

END

PRO AS_ImageMosaic::FixXYSize, xsize, ysize, xbuffer, ybuffer

  @as_scatterheader.macro

  IF N_Elements(xbuffer) EQ 0 THEN xbuffer = 0
  IF N_Elements(ybuffer) EQ 0 THEN ybuffer = 0

  IF ~Obj_Valid(self.monitorObj) THEN self.monitorObj = IDLsysMonitorInfo()
  primaryMonitor = self.monitorObj.GetPrimaryMonitorIndex()
  monitorSize = self.monitorObj.GetRectangles(/EXCLUDE_TASKBAR)
  
  ID = self.wBase
  WHILE ID NE 0 DO BEGIN
    topID = ID
    ID = Widget_Info(ID,/PARENT)
  ENDWHILE
  
  geomTop = Widget_Info(topID, /GEOMETRY)
  geomDraw = Widget_Info(self.wBase, /GEOMETRY)
  
  useMonXSize = monitorSize[2,primaryMonitor] - xbuffer - (geomTop.xsize - geomDraw.xsize) - 20
  useMonYSize = monitorSize[3,primaryMonitor] - ybuffer - (geomTop.ysize - geomDraw.ysize) - 20
  
  xsize = xsize < useMonXSize
  ysize = ysize < useMonYSize 
  
END


PRO AS_ImageMosaic::Cleanup

  @as_scatterheader.macro

  IF Widget_Info(self.wBase, /VALID) THEN Widget_Control, self.wBase, /DESTROY

END

FUNCTION AS_ImageMosaic::Init, images, columns, rows, LABELS = labels, GROUP_LEADER = groupLeader, DOCK = dock

  @as_scatterheader.macro

  XSize = 640
  YSize = 480

  self.labels = List()
  self.scale = 'LOG'
  self.scaleIndividually = 1
  self.topChop = 100
  self.minValue = 3

  IF N_Elements(columns) GT 0 THEN self.columns = columns[0] > 1 ELSE self.columns = 1
  IF N_Elements(rows) GT 0 THEN self.rows = rows[0] > 1 ELSE self.rows = 1
  IF Size(labels, /TNAME) EQ 'STRING' THEN self.labels = List(labels, /EXTRACT)
  IF ISA(labels, 'LIST') THEN self.labels = labels

  IF ~KeyWord_Set(groupLeader) THEN groupLeader = 0
  IF Widget_Info(groupLeader, /VALID) AND KeyWord_Set(dock) EQ 1 THEN $  
             wBase = Widget_Base(groupLeader, UNAME = 'Mosaic Base') $
        ELSE wBase = Widget_Base(GROUP_LEADER = groupLeader, /TLB_SIZE_EVENTS, /TLB_KILL_REQUEST_EVENTS, UNAME = 'Mosaic Base')
  wDraw = Widget_Draw(wBase, XSIZE = XSize, YSIZE = YSize, X_SCROLL_SIZE = XSize, Y_SCROLL_SIZE = YSize, GRAPHICS_LEVEL = 2, /BUTTON_EVENTS, /SCROLL, UNAME = 'Mosaic Draw')

  Widget_Control, wBase, /REALIZE
  Widget_Control, wDraw, GET_VALUE = drawWindow
    
  self.drawWindow = drawWindow
  
  Widget_Control, wBase, SET_UVALUE = self
  self.wBase = wBase
  
  self.viewplane = [0,0,xsize ,ysize]
  viewObj = IDLgrView(COLOR = [0,0,0], VIEWPLANE_RECT = self.viewplane)
  newXsize = xsize
  newYsize = ysize
  self.FixXYSize, newXsize, newYsize
  Widget_Control, wDraw, XSIZE = xsize, YSIZE = ysize, DRAW_XSIZE = newXsize, DRAW_YSIZE = newYsize, SET_DRAW_VIEW = [0,0]

  Loadct, 0
  XLoadct, GROUP = wBase, UPDATECALLBACK='as_imagemosaic_paletteupdate', UPDATECBDATA = self
  self.palette = IDLgrPalette()
  self.palette.Loadct, 0
  drawWindow.SetProperty, GRAPHICS_TREE = viewObj;, PALETTE=self.palette

  self.imagesArrObj = List()
  self.order = List(IndGen(N_Elements(images)),/EXTRACT)
  IF Arg_Present(images) THEN self.SetProperty, IMAGES = images
  
  
  XManager, 'AS_ImageMosaic', wBase, /NO_BLOCK

  RETURN, 1

END

PRO AS_ImageMosaic__Define

  dragging = { dragging, dragModel : Obj_New(), modelStart : [0,0], dragStart : [0,0]}

  void = {AS_ImageMosaic, $
          wBase : 0L, $
          drawWindow : Obj_New(), $
          imagesArr : List(), $
          imagesArrObj : List(), $  
          columns : 0L, $
          rows : 0L, $
          viewplane : [0.0,0.0,0.0,0.0], $
          palette : Obj_New(), $
          monitorObj : Obj_New(), $
          labels : List(), $
          dragging : dragging, $
          order : List(), $
          scale : '', $
          scaleIndividually : 1, $
          topChop : 1L, $
          minValue : 1}

END