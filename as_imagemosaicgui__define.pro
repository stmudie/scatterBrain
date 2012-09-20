PRO as_imagemosaicGUI_event, event

  @as_scatterheader.macro
  
  Widget_Control, event.top, GET_UVALUE = as_imagemosaicGUI
  as_imagemosaicGUI.event, event
END

PRO as_imagemosaicGUI::Event, event

  @as_scatterheader.macro

  widgetName = Widget_Info(event.id, /UNAME)
  
  CASE widgetName OF
    'Mosaic GUI Base' : BEGIN
                          IF Tag_Names(event, /STRUCTURE) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
                            Obj_Destroy, self
                            RETURN
                          ENDIF
                        END
    'Linear'         : IF event.select THEN self.scale, /LINEAR
    'Log'            : IF event.select THEN self.scale, /LOG
    'Square Root'    : IF event.select THEN self.scale, /SQRT
    'Hide Labels'    : self.ShowHideLabels, ~event.select
    'Scale Individually': self.SetProperty, SCALEINDIVIDUALLY = event.select
    'Min Value Slider' : self.scale, MINVALUE = event.value
    'Top Chop Slider' : BEGIN
                          Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='10X Bright'), GET_VALUE = tenX
                          self.scale, TOPCHOP = event.value*(1L+9*tenX)
                        END
    '10X Bright' : BEGIN
                     Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='Top Chop Slider'), GET_VALUE = chop
                     self.scale, TOPCHOP = chop*(1L+9*event.select)
                   END
    'Save 100'       : self.save
    'Save Use Zoom'  : self.save, /USEZOOM
    'Column Slider' : self.SetProperty, columns = event.value
    'Row Slider' : self.SetProperty, rows = event.value
    'Zoom Slider' : self.SetProperty, zoom = event.value
    ELSE :
  ENDCASE
  
  self.as_imagemosaic::event, event

END

PRO AS_ImageMosaicGUI::FixXYSize, xsize, ysize, xbuffer, ybuffer

  @as_scatterheader.macro

  IF N_Elements(ybuffer) EQ 0 THEN ybuffer = 50 ELSE ybuffer += 50
  self->as_imagemosaic::FixXYSize, xsize, ysize, xbuffer, ybuffer 

END

PRO as_imagemosaicGUI::SetProperty, _REF_EXTRA = extra

  @as_scatterheader.macro

  self->as_imagemosaic::SetProperty, _EXTRA = extra
  
  IF Where(extra EQ 'IMAGES') GE 0 OR Where(extra EQ 'ROWS') GE 0 OR Where(extra EQ 'COLUMNS') GE 0 THEN BEGIN
    Widget_Control, Widget_Info(self.wGUIBase, FIND_BY_UNAME = 'Column Slider'), SET_SLIDER_MAX = self.imagesArr.count()
    Widget_Control, Widget_Info(self.wGUIBase, FIND_BY_UNAME = 'Row Slider'), SET_SLIDER_MAX = self.imagesArr.count()
    
    Widget_Control, Widget_Info(self.wGUIBase, FIND_BY_UNAME = 'Column Slider'), SET_VALUE = self.columns
    Widget_Control, Widget_Info(self.wGUIBase, FIND_BY_UNAME = 'Row Slider'), SET_VALUE = self.rows
  ENDIF
  
END

PRO as_imagemosaicGUI::Cleanup

  @as_scatterheader.macro

  self->as_imagemosaic::cleanup
  IF Widget_Info(self.wGUIBase, /VALID) THEN Widget_Control, self.wGUIBase, /DESTROY
  
END

FUNCTION as_imagemosaicGUI::Init, images, columns, rows, GROUP_LEADER = groupLeader, _REF_EXTRA = extra

  @as_scatterheader.macro

  wGUIBase = Widget_Base(GROUP_LEADER = groupLeader, TITLE = 'scatterBrain Mosaic', MBAR=wMenuBase, EVENT_PRO = 'as_imagemosaicGUI_event', /TLB_KILL_REQUEST_EVENTS, /COLUMN, UNAME = 'Mosaic GUI Base')
  wHeaderBase = Widget_Base(wGUIBase, /BASE_ALIGN_BOTTOM, /FRAME, /ROW)
  wScrollBase = Widget_Base(wHeaderBase, XSIZE = 100, FRAME=0, /COLUMN)
  wZoomBase = Widget_Base(wHeaderBase, FRAME=0, /COLUMN)
  wButtonColumnBase = Widget_Base(wHeaderBase, /COLUMN)
  wExclusiveBase = Widget_Base(wButtonColumnBase, /EXCLUSIVE, /COLUMN)
  wNonExclusiveBase = Widget_Base(wButtonColumnBase, /NONEXCLUSIVE, /COLUMN)
  
  wFileButton = Widget_Button(wMenuBase, /MENU, VALUE = 'File')
  
  wColumnsSlider = Widget_Slider(wScrollBase, TITLE = 'Columns', VALUE = 1, MINIMUM = 1, UNAME = 'Column Slider')
  wRowsSlider = Widget_Slider(wScrollBase, TITLE = 'Rows', VALUE = 1, MINIMUM = 1, UNAME = 'Row Slider')
  wZoomSlider = CW_FSlider(wZoomBase, TITLE = 'Zoom Out', VALUE = 1, MINIMUM = 0.01, MAXIMUM = 1, /EDIT, /DRAG, UNAME = 'Zoom Slider')
  
  wScaleLinearCheck = Widget_Button(wExclusiveBase, VALUE = 'Linear', UNAME = 'Linear')
  wScaleLogCheck = Widget_Button(wExclusiveBase, VALUE = 'Log', UNAME = 'Log')
  wScaleSqRtCheck = Widget_Button(wExclusiveBase, VALUE = 'Square Root', UNAME = 'Square Root')
  Widget_Control, wScaleLogCheck, /SET_BUTTON
  
  wMinValue = Widget_Slider(wButtonColumnBase, TITLE = 'Min Image Value', VALUE = 3, UNAME = 'Min Value Slider')
  wTopChop = Widget_Slider(wButtonColumnBase, TITLE = 'Remove Brightest Pixels', VALUE = 100, MINIMUM = 0, MAXIMUM = 1000, UNAME = 'Top Chop Slider')
  
  wScaleIndividually = Widget_Button(wNonExclusiveBase, VALUE = 'Scale Individually', UNAME = 'Scale Individually')
  Widget_Control, wScaleIndividually, /SET_BUTTON
  wHideLabelCheck = Widget_Button(wNonExclusiveBase, VALUE = 'Hide Labels', UNAME = 'Hide Labels')

  wTop10TimesMult = Widget_Button(wNonExclusiveBase, VALUE = '10X Remove Brightest Pixels', UNAME = '10X Bright')
  
  wSaveImageButton = Widget_Button(wFileButton, VALUE = 'Save Image', /MENU, UNAME = 'Save')
  wSaveImageNoZoom = Widget_Button(wSaveImageButton, VALUE = 'Save 100%', UNAME = 'Save 100')
  wSaveImageZoom   = Widget_Button(wSaveImageButton, VALUE = 'Save with current zoom', UNAME = 'Save Use Zoom')
  
  
  self.wGUIBase = wGUIBase
  
  Widget_Control, wGUIBase, SET_UVALUE = self

  RETURN, self->as_imagemosaic::init(images, columns, rows, GROUP_LEADER = wGUIBase, /DOCK, _EXTRA = extra)

END

PRO as_imagemosaicGUI__define

  void = {as_imagemosaicGUI, $
          INHERITS as_imagemosaic, $
          wGUIBase : 0L }

END