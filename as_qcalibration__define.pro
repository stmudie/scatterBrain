PRO as_QCalibration_event, event

  Widget_Control, event.top, GET_UVALUE = as_QCalibration
  as_QCalibration.event, event

END

PRO as_QCalibration::event, event

  widgetName = Widget_Info(event.ID, /UNAME)
  
  CASE widgetName OF 
    'QCALIB BASE'     : BEGIN
;                        IF Tag_Names(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN BEGIN
;                          Widget_Control, self.wQCalibBase, /DESTROY
;                          Obj_Destroy, self
;                        ENDIF
                        IF Tag_Names(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN Widget_Control, self.wQCalibBase, MAP = 0
                      END
    'CONFIG COMBO'  :
    'ENERGY'        :  BEGIN
                         Widget_Control, event.id, GET_VALUE = energy
                         wavelength = 12398.4172 / energy
                         Widget_Control, Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'WAVELENGTH'), SET_VALUE = String(wavelength)
                         self.calcLength
                       END
    'WAVELENGTH'    :  BEGIN
                         Widget_Control, event.id, GET_VALUE = wavelength
                         energy = 12398.4172 / wavelength
                         Widget_Control, Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'ENERGY'), SET_VALUE = String(energy)
                         self.calcLength 
                       END
    'DETECTOR ANGLE':  BEGIN
                         
                       END
    'STANDARD'      :  BEGIN
                         Widget_Control, event.id, GET_UVALUE = standards
                         Widget_Control, Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'STANDARD PEAKS'), $
                                         SET_VALUE = (standards[event.index].peaks)[0] + String((standards[event.index].peaks)[1],FORMAT = '(F7.4)')
                         self.calcLength
                       END
    'STANDARD PEAKS':  BEGIN
                         self.calcLength
                       END
    'APPLY'         :  BEGIN
                        wWavelength = Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'WAVELENGTH')
                        wDetectorAngle = Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'DETECTOR ANGLE')
                        wCameraLength = Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'CAMERA LENGTH')
                        wCurrentCameraLength = Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'CURRENT CAMERA LENGTH')
                        
                        Widget_Control, wWaveLength, GET_VALUE = wavelength
                        self.currWavelength = wavelength
                        Widget_Control, wCameraLength, GET_VALUE = cameraLength
                        Widget_Control, wDetectorAngle, GET_VALUE = detectorAngle
                        
                        IF cameraLength LT 1 THEN BEGIN
                          result = Dialog_Message('Error calculating camera length (zero). Not applying.', /ERROR)
                          BREAK
                        ENDIF
                        
                        Widget_Control, wCurrentCameraLength, SET_VALUE = cameraLength
                        
                        self.notify, {cameralength : cameraLength, wavelength : wavelength, detectorAngle : detectorAngle}
                        ;Widget_Control, self.wQCalibBase, /DESTROY
                        ;Obj_Destroy, self
                        Widget_Control, self.wQCalibBase, MAP = 0
                       END
    ELSE            : Print, 'Widget Name ' + widgetName + ' not handled by event case.'
  ENDCASE

END

FUNCTION as_QCalibration::init, GROUPLEADER = groupLeader, NOTIFY_OBJ = notifyObj, SHOWGUI = showGUI

  IF KeyWord_Set(notifyObj) THEN $
    IF TypeName(notifyObj[0]) EQ 'NOTIFY' $
      THEN self.notifyObj = List(notifyObj, /EXTRACT)

  void = {standards, standardName : '', peaks : list()}

  standards =  Replicate({standards},4)
  standards[0] = {standardName : 'Silver Behenate (AgBeh)', peaks : list(['001 : ','002 : ','003 : '],[0.1076,0.2153,0.3229])}
  standards[1] = {standardName : 'Rat Tail Tendon', peaks : list(['1st : ','2nd : ','3rd : '],[0.0096,0.0192,0.0288,0.0384])}
  standards[2] = {standardName : 'Lanthanum Hexaboride (LaB6)', peaks : list(['100 : ','110 : ','111 : ','200 : '],[1.5115, 2.1376, 2.6180, 3.0230])}
  standards[3] = {standardName : 'Custom...', peaks : list([''],[''])}

  IF KeyWord_Set(groupLeader) THEN BEGIN
                                IF Widget_Info(groupLeader,/VALID) THEN BEGIN
                                  self.groupLeader = groupLeader
                                  self.wQCalibBase = Widget_Base(GROUP_LEADER=groupLeader, TITLE = 'Q Calibration GUI', EVENT_PRO = 'as_QCalibration_event', /FLOATING, /COLUMN, /TLB_KILL_REQUEST_EVENTS, MAP = KeyWord_Set(showGUI), UNAME = 'QCALIB BASE')
                                ENDIF
                              ENDIF ELSE self.wQCalibBase = Widget_Base(TITLE = 'Q Calibration GUI',EVENT_PRO = 'as_QCalibration_event', /COLUMN, /TLB_KILL_REQUEST_EVENTS, MAP = KeyWord_Set(showGUI), UNAME = 'QCALIB BASE')
  
  wFrameBase = Widget_Base(self.wQCalibBase, /COLUMN, /FRAME)
  
  wWavelengthBase = Widget_Base(wFrameBase, /COLUMN, /FRAME)
  wWavelengthLabel = Widget_Label(wWavelengthBase, VALUE = 'Enter Current Beam Energy or Wavelength:')
  wWavelengthRow = Widget_Base(wWavelengthBase, /ROW)
  wEnergyText = Widget_Text(wWavelengthRow, /EDITABLE, UNAME = 'ENERGY')
  wEnergyLabel = Widget_Label(wWavelengthRow, VALUE = 'eV')
  wWavelengthText = Widget_Text(wWavelengthRow, /EDITABLE, UNAME = 'WAVELENGTH')
  wWavelengthLabel = Widget_Label(wWavelengthRow, VALUE = String([197b]))
  wDetAngLabel = Widget_Label(wWavelengthBase, VALUE = 'Enter Current Detector Angle (WAXS):')
  wDetAngRow = Widget_Base(wWavelengthBase, /ROW)
  wDetAngText = Widget_Text(wDetAngRow, /EDITABLE, UNAME = 'DETECTOR ANGLE')
  wDetAngLabel = Widget_Label(wDetAngRow, FONT = '18', VALUE = String([176b]))
  wBuffer2 = Widget_Label(wFrameBase, VALUE = '')
  
  wQCalibPeakBase = Widget_Base(wFrameBase, /COLUMN, /FRAME)
  wPeakPosition = FSC_Field(wQCalibPeakBase, TITLE = 'Fitted Peak Position:', VALUE = Float(0.0), DECIMAL = 5, OBJECT = peakPosObj, UNAME = 'FITTED POS')
  Widget_Control, wPeakPosition, SET_UVALUE = peakPosObj
  
  wFittedPeakLabel = Widget_Label(wQCalibPeakBase, VALUE = 'Choose Fitted Peak:')
  wStandardRow = Widget_Base(wQCalibPeakBase, /ROW)
  wStandardCombo = Widget_Combobox(wStandardRow, VALUE = standards.standardName, UNAME = 'STANDARD', UVALUE = standards)
  wStandardPeakCombo = Widget_Combobox(wStandardRow, /EDITABLE, /DYNAMIC_RESIZE,  VALUE = (standards[0].peaks)[0] + String((standards[0].peaks)[1],FORMAT = '(F7.4)'), UNAME = 'STANDARD PEAKS')
  wStandardPeakLabel = Widget_Label(wStandardRow, VALUE = '1/'+String([197b]))
  
  wBuffer = Widget_Label(wFrameBase, VALUE = '')

  wCameraLength = FSC_Field(wFrameBase, TITLE = 'Calibrated Camera Length:', UNAME = 'CAMERA LENGTH')
  wCurrentCameraLength = FSC_Field(wFrameBase, TITLE = 'Current Camera Length:', UNAME = 'CURRENT CAMERA LENGTH')
    
  wConfigBase = Widget_Base(self.wQCalibBase, /ROW)  
  wConfigLabel = Widget_Label(wConfigBase, VALUE = 'Choose Configuration to Edit:')
  
  wConfigCombo = Widget_Combobox(wConfigBase, VALUE = 'All', UNAME = 'CONFIG COMBO')
  
  wApplyCalibration = Widget_Button(self.wQCalibBase, VALUE = 'Apply Q Calibration', UNAME = 'APPLY')
  
  Widget_Control, self.wQCalibBase, /REALIZE
  Widget_Control, self.wQCalibBase, SET_UVALUE = self
  
  RETURN, 1

END

PRO as_QCalibration::notify, event 

  FOREACH notify, self.notifyObj DO IF Obj_Valid(notify) THEN notify.notify, event

END

PRO as_QCalibration::SetProperty, PEAK = peak, GROUPLEADER = groupLeader, CAMERALENGTH = cameraLength, WAVELENGTH = waveLength, DETECTORANGLE = detectorAngle

  IF KeyWord_Set(peak) THEN BEGIN
    Widget_Control, Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'FITTED POS'), GET_UVALUE = peakPosObj
    peakPosObj.object->Set_Value, peak
  ENDIF

  IF KeyWord_Set(cameraLength) THEN Widget_Control, Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'CURRENT CAMERA LENGTH'), SET_VALUE = String(cameraLength)
  IF KeyWord_Set(waveLength) THEN BEGIN
    self.currWavelength = wavelength
    Widget_Control, Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'WAVELENGTH'), SET_VALUE = String(waveLength)
    energy = 12398.4172 / wavelength
    Widget_Control, Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'ENERGY'), SET_VALUE = String(energy)
  ENDIF
  IF KeyWord_Set(detectorAngle) THEN Widget_Control, Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'DETECTOR ANGLE'), SET_VALUE = String(detectorAngle)
  IF KeyWord_Set(groupLeader) THEN BEGIN
    self.groupLeader = groupLeader
    Widget_Control, self.wQCalibBase, GROUP_LEADER = groupLeader
  ENDIF

  self.calcLength
  
END

PRO as_QCalibration::CalcLength

  standardPeaksCombo = Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'STANDARD PEAKS')
  chosenPeak = Widget_Info(standardPeaksCombo, /COMBOBOX_GETTEXT)
  chosenPeak = Float((StrSplit(chosenPeak, ' ', /EXTRACT))[-1])
  
  wPeakFit = Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'FITTED POS')
  wWavelength = Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'WAVELENGTH')
  wCurrentCameraLength = Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'CURRENT CAMERA LENGTH')
  wCameraLength = Widget_Info(self.wQCalibBase, FIND_BY_UNAME = 'CAMERA LENGTH')
  
  Widget_Control, wPeakFit, GET_VALUE = peak
  Widget_Control, wWavelength, GET_VALUE = wavelength
  Widget_Control, wCurrentCameraLength, GET_VALUE = currentCameraLength
  
  peak = Float(peak)
  wavelength = Float(wavelength)
  currentCameraLength = Float(currentCameraLength)
  
  cameraLength = currentCameraLength * Tan(2*Asin(peak*self.currWavelength/2/!dpi)) $
                 /Tan(2*Asin(chosenPeak*wavelength/2/!dpi))
  
  Widget_Control, wCameraLength , SET_VALUE = String(cameraLength)

END

PRO as_QCalibration::ShowGUI

  Widget_Control, self.wQCalibBase, /MAP

END

PRO as_QCalibration::EditConfig, RENAME=rename, DELETE=delete

  

END

PRO as_QCalibration__define

  void = {QCALIBRETURN, qCalib : 0}

  void = {as_QCalibration, $
          groupLeader : 0L, $
          notifyObj : List(), $
          currWavelength : 0.0, $
          qCalib : 0.0, $
          wQCalibBase : 0L  }

END