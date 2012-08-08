PRO as_abscal_event, event

  Widget_Control, event.top, GET_UVALUE = as_abscal
  as_abscal.event, event

END

PRO as_abscal::event, event

  widgetName = Widget_Info(event.ID, /UNAME)
  
  CASE widgetName OF 
    'ABS BASE'       : BEGIN
                         ;IF Tag_Names(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN OBj_Destroy, self 
                         Widget_Control, self.wAbsBase, MAP = 0
                       END
    'CONFIG COMBO'   : 
    'NORM COUNTERS'  :  BEGIN
                         self.notify, {ABSRETURN, absCal : -1, counterRatio : self.ratio, I0Norm : self.I0rate, IBSNorm : self.IBSRate }
                         Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME='Current I0 VAL'), SET_VALUE = String(self.I0Rate)
                         Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME='Current IBS VAL'), SET_VALUE = String(self.IBSRate)
                         Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME='Current RATIO VAL'), SET_VALUE = String(self.ratio)
                        END
    'NO NORM'        :  IF event.select THEN self.normSelected = 0
    'I0 NORM'        :  IF event.select THEN self.normSelected = 1
    'IBS NORM'       :  IF event.select THEN self.normSelected = 2
    'I0BS NORM'      :  IF event.select THEN result = Dialog_Message('I told you not to select this option!!') ;self.normSelected = 3
    'APPLY NORM TYPE':  BEGIN
                          IF self.useAbsCal EQ 0 THEN self.notify, {NORMRETURN, normType : self.normSelected, counterRatio : -1, I0Norm : -1, IBSNorm : -1}
                        END
    'FORCE ZERO'     :  BEGIN
                          self.forceZero = event.select
                          self.calibrate
                        END
    'CALIBRANT COMBO':  BEGIN
                          split = StrSplit(event.str,':',/EXTRACT)
                          IF N_Elements(split) EQ 2 THEN calibCross = Float(split[1]) ELSE calibCross = Float(split[0])
                          IF Size(calibCross, /TYPE) EQ 4 THEN self.calibCross = calibCross
                          IF event.index EQ -1 THEN BEGIN
                            Widget_Control, event.id, GET_VALUE = values
                            IF (Where(values EQ event.str))[0] EQ -1 THEN Widget_Control, event.id, SET_VALUE = [event.str, values]
                          ENDIF
                        END
    'APPLY'          :  BEGIN
                         IF self.absCal GT 0 THEN BEGIN
                           self.SetCurrentCalibration, self.absCal, 0, USE = Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'USE ABS')
                         ENDIF ELSE result = Dialog_Message('Calibration invalid, not applied.')
                        END
    'USE ABS'        :  BEGIN
                          self.useAbsCal = event.select
                          self.SetCurrentCalibration, USE = event.select
                        END
    ELSE             : Print, 'Widget Name ' + widgetName + ' not handled by event case.'
  ENDCASE

END

FUNCTION as_abscal::init, GROUPLEADER = groupLeader, NOTIFY_OBJ = notifyObj, showGUI = showGUI

  IF Keyword_Set(notifyObj) THEN $
    IF TypeName(notifyObj[0]) EQ 'NOTIFY' $
      THEN self.notifyObj = List(notifyObj,/EXTRACT)

  monitorObj = IDLsysMonitorInfo()
  primaryMonitor = monitorObj.GetPrimaryMonitorIndex()
  monitorSize = monitorObj.GetRectangles(/EXCLUDE_TASKBAR)
  useMonYSize = monitorSize[3,primaryMonitor]

  IF KeyWord_Set(groupLeader) THEN BEGIN
                                IF Widget_Info(groupLeader,/VALID) THEN BEGIN
                                  self.groupLeader = groupLeader
                                  self.wAbsBase = Widget_Base(GROUP_LEADER=groupLeader, TITLE = 'Intensity Normalisation and Calibration GUI', EVENT_PRO = 'as_abscal_event',  /COLUMN, /TLB_KILL_REQUEST_EVENTS,  /SCROLL, Y_SCROLL_SIZE = useMonYsize-75 < 900, X_SCROLL_SIZE = 610, MAP = KeyWord_Set(showGUI), UNAME = 'ABS BASE')
                                ENDIF
                              ENDIF ELSE self.wAbsBase = Widget_Base(TITLE = 'Intensity Normalisation and Calibration GUI', EVENT_PRO = 'as_abscal_event',SCR_YSIZE = useMonYSize, /COLUMN, /FLOATING, /TLB_KILL_REQUEST_EVENTS, /SCROLL, MAP = KeyWord_Set(showGUI), UNAME = 'ABS BASE')
  
  wFrameBase = Widget_Base(self.wAbsBase, /COLUMN, /FRAME)
  
  wConfigBase = Widget_Base(wFrameBase, /ROW)
  wConfigLabel = Widget_Label(wConfigBase, VALUE = 'Choose Configuration to Edit:')
  wConfigCombo = Widget_Combobox(wConfigBase, VALUE = 'All', UNAME = 'CONFIG COMBO')
  wBuffer = Widget_Label(wFrameBase, VALUE = '')
  
  wRowFrameBase = Widget_Base(wFrameBase, /ROW)
  
  wNormDetBase = Widget_Base(wRowFrameBase, /COLUMN, /FRAME)
  wNormDetLabel = Widget_Label(wNormDetBase, YSIZE = 30, XSIZE = 250, VALUE = 'Select airshot in scatterBrain plot screen to ' +String([13b]) + 'load counts for normalising counters.')
  wNormDetRowBase = Widget_Base(wNormDetBase, /ROW)
  wCurrentLabel = Widget_Label(wNormDetRowBase, VALUE = 'New:    ')
  wNormI0Text = FSC_Field(wNormDetRowBase, VALUE = '1', XSIZE = 10, TITLE = 'I0 (c/s)', /NONSENSITIVE, /COLUMN, UNAME = 'I0 VAL')
  wNormSlashLabel = Widget_Label(wNormDetRowBase, VALUE = '/')
  wNormIBSText = FSC_Field(wNormDetRowBase, VALUE = '1', XSIZE = 10, TITLE = 'IBS (c/s)', /NONSENSITIVE, /COLUMN,UNAME = 'IBS VAL')
  wNormEqualsLabel = Widget_Label(wNormDetRowBase, VALUE = '=')
  wNormNormText = FSC_Field(wNormDetRowBase, VALUE = '1', XSIZE = 10, TITLE = 'Ratio', /NONSENSITIVE, /COLUMN, UNAME = 'RATIO VAL')
  wNormDetRowBase2 = Widget_Base(wNormDetBase, /ROW)
  wCurrentLabel = Widget_Label(wNormDetRowBase2, VALUE = 'Current:')
  wCurrentNormI0Text = FSC_Field(wNormDetRowBase2, VALUE = '1', XSIZE = 10, TITLE = '', /NONSENSITIVE, /COLUMN, UNAME = 'Current I0 VAL')
  wNormSlashLabel = Widget_Label(wNormDetRowBase2, VALUE = '/')
  wCurrentNormIBSText = FSC_Field(wNormDetRowBase2, VALUE = '1', XSIZE = 10, TITLE = '', /NONSENSITIVE, /COLUMN,UNAME = 'Current IBS VAL')
  wNormEqualsLabel = Widget_Label(wNormDetRowBase2, VALUE = '=')
  wCurrentNormNormText = FSC_Field(wNormDetRowBase2, VALUE = '1', XSIZE = 10, TITLE = '', /NONSENSITIVE, /COLUMN, UNAME = 'Current RATIO VAL')
  wNormDetectors = Widget_Button(wNormDetBase, VALUE = 'Apply Normalisation to I0/Beam Stop Counters', UNAME = 'NORM COUNTERS')
  
  wNormTypeBase = Widget_Base(wRowFrameBase, /COLUMN, /FRAME)
  wNormTypeLabel = Widget_Label(wNormTypeBase, VALUE = 'Select to Change Type of Normalisation')
  wNormTypeButBase = Widget_Base(wNormTypeBase, /EXCLUSIVE, /COLUMN)
  wNoNormBut = Widget_Button(wNormTypeButBase, VALUE = 'No Normalisation', UNAME = 'NO NORM')
  wI0NormBut = Widget_Button(wNormTypeButBase, VALUE = 'Incident Intensity Normalisation', UNAME = 'I0 NORM')
  wIBSNormBut = Widget_Button(wNormTypeButBase, VALUE = 'Beamstop Intensity Normalisation', UNAME = 'IBS NORM')
  wI0BSNormBut = Widget_Button(wNormTypeButBase, VALUE = 'Do Not Select This Option', UNAME = 'I0BS NORM')
  wApplyBut = Widget_Button(wNormTypeBase, VALUE = 'Apply change of Normalisation', UNAME = 'APPLY NORM TYPE')
  
  
  wBuffer = Widget_Label(wFrameBase, VALUE = '')
  wAbsCalBase = Widget_Base(wFrameBase, /COLUMN, /FRAME)
  wAbsPlot = Widget_Draw(wAbsCalBase, XSize = 500, YSize = 500)

  wNonExclusiveButtonBase = Widget_Base(wAbsCalBase, /ROW, /NONEXCLUSIVE)
  wUseAbsCalButton = Widget_Button(wNonExclusiveButtonBase, FONT = 'BOLD*24', VALUE = 'Use Absolute Calibration', UNAME = 'USE ABS')
  wForceZero = Widget_Button(wNonExclusiveButtonBase, VALUE = 'Force Through Origin', UNAME = 'FORCE ZERO')
  
  calibrantName = ['Water','Glassy Carbon']
  calibrantValue = [0.0163,35]
  
  self.calibCross = calibrantValue[0]
  
  wAbsCalibrantBase = Widget_Base(wAbsCalBase, /ROW)
  wAbsCalibrant = Widget_Combobox(wAbsCalibrantBase, VALUE = calibrantName + ' : '+ StrCompress(calibrantValue,/REMOVE_ALL), /EDITABLE, UNAME='CALIBRANT COMBO')
  
  wAbsCalLabelBase1 = Widget_Base(wAbsCalBase, /ROW)
  wCalFactorLabel = Widget_Label(wAbsCalLabelBase1, FONT = '24', VALUE = 'Absolute Calibration Factor:')
  wCalFactor = Widget_Label(wAbsCalLabelBase1, VALUE = '--          ', FONT = 'BOLD*24', /DYNAMIC_RESIZE, UNAME = 'Abs Cal Factor')
  buffer = Widget_Label(wAbsCalLabelBase1, VALUE = '  ')
  wCurrentLabel = Widget_Label(wAbsCalLabelBase1, VALUE = 'Current:', FONT = '24', /DYNAMIC_RESIZE, UNAME = 'Current Label')
  wCurrentCalFactor = Widget_Label(wAbsCalLabelBase1, VALUE = '--', FONT = 'BOLD*24', /DYNAMIC_RESIZE, UNAME = 'Current Abs Cal Factor')
  wAbsCalLabelBase2 = Widget_Base(wAbsCalBase, /ROW)
  wCalFactorOffLabel = Widget_Label(wAbsCalLabelBase2, FONT = '24', VALUE = 'Absolute Calibration Factor Offset:')
  wCalFactorOff = Widget_Label(wAbsCalLabelBase2, VALUE = '--          ', /DYNAMIC_RESIZE, FONT = 'BOLD*24', UNAME = 'Abs Cal Factor Offset')
  buffer = Widget_Label(wAbsCalLabelBase2, VALUE = '  ')
  wCurrentLabel = Widget_Label(wAbsCalLabelBase2, VALUE = 'Current:', FONT = '24', /DYNAMIC_RESIZE, UNAME = 'Current Label')
  wCurrentCalFactorOff = Widget_Label(wAbsCalLabelBase2, VALUE = '--', FONT = 'BOLD*24', /DYNAMIC_RESIZE, UNAME = 'Current Abs Cal Factor Offset')
  wApplyBut = Widget_Button(wAbsCalBase, VALUE = 'Set Absolute Calibration', UNAME = 'APPLY')
  
  
  self.DetCounts = Ptr_New(/ALLOCATE_HEAP)
  self.I0 = Ptr_New(/ALLOCATE_HEAP)
  self.IBS = Ptr_New(/ALLOCATE_HEAP)
  
  self.backCounts = Ptr_New(/ALLOCATE_HEAP)
  self.I0Back = Ptr_New(/ALLOCATE_HEAP)
  self.BSBack = Ptr_New(/ALLOCATE_HEAP)
  
  Widget_Control, self.wAbsBase, /REALIZE
  Widget_Control, self.wAbsBase, SET_UVALUE = self
  
  geom = Widget_Info(wFrameBase, /GEOMETRY)
  ;IF geom.ysize GT useMonYSize THEN Widget_Control, self.wAbsBase, SCR_YSIZE = useMonYSize ELSE Widget_Control, self.wAbsBase, SCR_YSIZE = geom.ysize + 5 
  
  Widget_Control, wAbsPlot, GET_VALUE = plotWindow
  
  wSet, plotWindow
  self.plotWindow = plotWindow
  
  RETURN, 1

END

PRO as_abscal::ShowGUI

  Widget_Control, self.wAbsBase, /MAP

END

PRO as_abscal::notify, event

  FOREACH notify, self.notifyObj DO IF Obj_Valid(notify) THEN notify.notify, event

END

PRO as_abscal::SetProperty, NORMTYPE = normType

  IF N_Elements(normType) GT 0 THEN BEGIN
    self.normSelected = normType[0]
    CASE self.normSelected OF
      0 : Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'NO NORM'), /SET_BUTTON
      1 : Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'I0 NORM'), /SET_BUTTON
      2 : Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'IBS NORM'), /SET_BUTTON
      3 : Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'I0BS NORM'), /SET_BUTTON
    ENDCASE
  ENDIF

END

PRO as_abscal::DetCounts, I0, IBS, expTime, APPLIED=applied

  IF N_Elements(I0) GT 0 AND N_Elements(IBS) GT 0 THEN BEGIN

    self.ratio = I0/Float(IBS)
    self.I0Rate = I0/expTime
    self.IBSRate = IBS/expTime

    Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME='I0 VAL'), SET_VALUE = String(self.I0Rate)
    Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME='IBS VAL'), SET_VALUE = String(self.IBSRate)
    Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME='RATIO VAL'), SET_VALUE = String(self.ratio)
    
    IF KeyWord_Set(applied) THEN BEGIN
      Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME='Current I0 VAL'), SET_VALUE = String(self.I0Rate)
      Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME='Current IBS VAL'), SET_VALUE = String(self.IBSRate)
      Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME='Current RATIO VAL'), SET_VALUE = String(self.ratio)
    ENDIF
    
  ENDIF

END

PRO as_abscal::SetCurrentCalibration, ABSCal, Offset, USE = use

  IF N_Elements(ABSCal) THEN Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'Current Abs Cal Factor'), SET_VALUE = String(ABSCal)
  IF N_Elements(Offset) THEN Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'Current Abs Cal Factor Offset'), SET_VALUE = String(Offset)
  
  IF N_Elements(use) GT 0 THEN BEGIN
    IF KeyWord_Set(use) THEN BEGIN
      Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'Current Abs Cal Factor'), GET_VALUE = absCal
      
      self.notify, {ABSRETURN, absCal : float(absCal), counterRatio : -1, I0Norm : -1, IBSNorm : -1}
      self.useAbsCal = 1
      Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'USE ABS'), SET_BUTTON = use
    ENDIF ELSE BEGIN
      self.useAbsCal = 0
      self.notify, {NORMRETURN, normType : self.normSelected, counterRatio : -1, I0Norm : -1, IBSNorm : -1}
    ENDELSE
  ENDIF

END

PRO as_abscal::CalibPoints, DetCounts, I0, BS, BACKCOUNTS = backCounts, I0Back = I0Back, BSBack = BSBack, CLEAR = clear

  IF KeyWord_Set(clear) THEN BEGIN
    *self.DetCounts = !Null
    wSet, self.plotWindow
    cgPlot, [0,10], [0,100], XTitle = 'Normalised Beamstop Counts', YTitle = 'Average Detector Counts', /NODATA
    Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'Abs Cal Factor'), SET_VALUE = '--          '
    Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'Abs Cal Factor Offset'), SET_VALUE = '--          '
    self.absCal = 0
    RETURN
  ENDIF

  *self.DetCounts = detCounts
  *self.I0 = I0
  *self.IBS = BS

  IF N_Elements(backCounts) GT 0 AND N_Elements(BSBack) EQ N_Elements(backCounts) THEN BEGIN
    *self.backCounts = backCounts
    *self.I0Back = I0Back
    *self.BSBack = BSBack
  ENDIF

  self.Calibrate

END

PRO as_abscal::Calibrate
  
  IF N_Elements(*self.detCounts) EQ 0 THEN RETURN
  
  IF N_Elements(*self.backCounts) GT 0 AND N_Elements(*self.BSBack) EQ N_Elements(*self.backCounts) THEN BEGIN
    XRange = [0, Max([*self.IBS,*self.BSBack])]
    YRange = [0, Max([*self.DetCounts,*self.backCounts])]
  ENDIF ELSE BEGIN
    XRange = [0, Max(*self.IBS)]
    YRange = [0, Max(*self.DetCounts)]
  ENDELSE
  
  ; Use the function lingradient so that it automatically compiles for build, as it is only used as an input to the curvefit routine, and passed as a string.
  lingradient, 1, [1,2], void
  
  sampleFitParams = [(*self.DetCounts)[0]/(*self.IBS)[0], 0]
  sampleCalib = (*self.DetCounts)[0]
  IF N_Elements(*self.DetCounts) GT 1 THEN BEGIN
    IF N_Elements(*self.DetCounts) EQ 2 AND self.forceZero EQ 0 THEN BEGIN
      sampleFitParams[0] = ((*self.DetCounts)[1]-(*self.DetCounts)[0])/((*self.IBS)[1]-(*self.IBS)[0])
      sampleFitParams[1] = (*self.DetCounts)[0] - sampleFitParams[0]*(*self.IBS)[0]
      sampleCalib = (*self.DetCounts)
    ENDIF ELSE sampleCalib = CurveFit([*self.IBS],[*self.DetCounts],weights, sampleFitParams, FUNCTION_NAME='lingradient', FITA=[1,~self.forceZero], /NODERIVATIVE)
  ENDIF
  
  backFitParams = [0,0]
  
  IF N_Elements(*self.backCounts) GT 0 THEN BEGIN
    backFitParams = [(*self.backCounts)[0]/(*self.BSBack)[0], 0]
    backCalib = (*self.backCounts)[0]
    IF N_Elements(*self.backCounts) EQ 2 AND self.forceZero EQ 0 THEN BEGIN
      backFitParams[0] = ((*self.backCounts)[1]-(*self.backCounts)[0])/((*self.BSBack)[1]-(*self.BSBack)[0])
      backFitParams[1] = (*self.backCounts)[0] - backFitParams[0]*(*self.BSBack)[0]
      backCalib = (*self.backCounts)
    ENDIF ELSE IF N_Elements(*self.backCounts) GT 1 THEN backCalib = CurveFit([*self.BSBack],[*self.backCounts],weights, backFitParams, FUNCTION_NAME='lingradient', FITA=[1,~self.forceZero], /NODERIVATIVE)
  ENDIF
  
  wSet, self.plotWindow
  
  cgPlot, [*self.IBS], [*self.DetCounts], psym=42, symcolor = 'Red', XTitle = 'Normalised Beamstop Counts', YTitle = 'Average Detector Counts', XRange = XRange, YRange = YRange
  order = sort(*self.IBS)
  cgPlot, [0,(*self.IBS)[order]], [sampleFitParams[1],sampleCalib[order]], color = 'Red', /OVERPLOT
  IF N_Elements(*self.backCounts) GT 0 AND N_Elements(*self.BSBack) EQ N_Elements(*self.backCounts) THEN BEGIN
    cgPlot, [*self.BSBack], [*self.backCounts], psym=42, symcolor = 'Blue', /OVERPLOT
    cgPlot, [0,(*self.BSBack)[order]], [backFitParams[1],backCalib[order]], color = 'Blue', /OVERPLOT
  ENDIF

  self.absCal = self.calibCross/(sampleFitParams[0] - backFitParams[0])

  Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'Abs Cal Factor'), SET_VALUE = String(self.absCal)
  Widget_Control, Widget_Info(self.wAbsBase, FIND_BY_UNAME = 'Abs Cal Factor Offset'), SET_VALUE = String(Max([sampleFitParams[1], backFitParams[1]]))
  

END

PRO as_abscal::EditConfig, RENAME=rename, DELETE=delete

  

END

PRO as_abscal::Cleanup

  IF ISA(self.notifyObj, 'LIST') THEN IF self.notifyObj.count() GT 0 THEN Obj_Destroy, self.notifyObj.toArray()
  IF Widget_Info(self.wAbsBase, /VALID) THEN Widget_Control, self.wAbsBase, /DESTROY

END

PRO as_abscal__define

  void = {ABSRETURN, absCal : 0.0, counterRatio : 0, I0Norm : 0.0, IBSNorm : 0.0}

  void = {as_abscal, $
          groupLeader : 0L, $
          notifyObj : List(), $
          ratio : 0.0, $
          I0Rate : 0.0, $
          IBSRate : 0.0, $
          normSelected : 0, $
          absCal : 0.0, $
          useAbsCal : 0, $
          calibPoints : List(), $
          calibCross : 0.0, $
          plotWindow : 0L, $
          wAbsBase : 0L, $
          forceZero : 0, $
          DetCounts : Ptr_New(), $
          I0  : Ptr_New(), $
          IBS : Ptr_New(), $
          backcounts : Ptr_New(), $
          I0Back : Ptr_New(), $
          BSBack :Ptr_New() }

END