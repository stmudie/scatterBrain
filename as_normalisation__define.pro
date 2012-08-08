PRO as_normalisation_event, event

  Widget_Control, event.top, GET_UVALUE = as_normalisation
  as_normalisation.event, event

END

PRO as_normalisation::event, event

  widgetName = Widget_Info(event.ID, /UNAME)
  
  CASE widgetName OF 
    'NORM BASE'     : BEGIN
                        IF Tag_Names(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN Widget_Control, self.wNormBase, MAP = 0
                      END
    'CONFIG COMBO'  :
    'NORM COUNTERS' :  BEGIN
                        FOREACH obj, (*self.notifyObj) DO Call_Method, obj.method, obj.object, {NORMRETURN, normType : -1, counterRatio : self.ratio, I0Norm : self.I0rate, IBSNorm : self.IBSRate }
                       END
    'NO NORM'       :  IF event.select THEN self.normSelected = 0
    'I0 NORM'       :  IF event.select THEN self.normSelected = 1
    'IBS NORM'      :  IF event.select THEN self.normSelected = 2
    'I0BS NORM'     :  IF event.select THEN result = Dialog_Message('I told you not to select this option!!') ;self.normSelected = 3
    'APPLY'         :  BEGIN
                        FOREACH obj, (*self.notifyObj) DO Call_Method, obj.method, obj.object, {NORMRETURN, normType : self.normSelected, counterRatio : -1, I0Norm : -1, IBSNorm : -1}
                       END
    ELSE            : Print, 'Widget Name ' + widgetName + ' not handled by event case.'
  ENDCASE

END

FUNCTION as_normalisation::init, GROUPLEADER = groupLeader, NOTIFY_OBJ = notifyObj

  IF N_Elements(notifyObj) GT 0 THEN BEGIN
    FOR i = 0, N_Elements(notifyObj) - 1 DO BEGIN
      tags = Tag_Names(notifyObj)
      void  = Where(tags EQ 'METHOD', count1)
      void = Where(tags EQ 'OBJECT', count2)
      IF count1 + count2 NE 2 THEN BEGIN
        result = Dialog_Message('Incorrect tags. Normalisation GUI not started.')
        RETURN, 0
      ENDIF
    ENDFOR
    self.notifyObj = Ptr_New(notifyObj)
  ENDIF ELSE self.notifyObj = Ptr_New(/ALLOCATE_HEAP)


  IF KeyWord_Set(groupLeader) THEN BEGIN
                                IF Widget_Info(groupLeader,/VALID) THEN BEGIN
                                  self.groupLeader = groupLeader
                                  self.wNormBase = Widget_Base(GROUP_LEADER=groupLeader, TITLE = 'Normalisation GUI', EVENT_PRO = 'as_normalisation_event', /COLUMN, /FLOATING, /TLB_KILL_REQUEST_EVENTS, UNAME = 'NORM BASE')
                                ENDIF
                              ENDIF ELSE self.wNormBase = Widget_Base(TITLE = 'Normalisation GUI',EVENT_PRO = 'as_normalisation_event', /COLUMN, /FLOATING, /TLB_KILL_REQUEST_EVENTS, UNAME = 'NORM BASE')
  
  wFrameBase = Widget_Base(self.wNormBase, /COLUMN, /FRAME)
  
  wConfigBase = Widget_Base(wFrameBase, /ROW)
  wConfigLabel = Widget_Label(wConfigBase, VALUE = 'Choose Configuration to Edit:')
  wConfigCombo = Widget_Combobox(wConfigBase, VALUE = 'All', UNAME = 'CONFIG COMBO')
  wBuffer = Widget_Label(wFrameBase, VALUE = '')
  
  wNormDetBase = Widget_Base(wFrameBase, /COLUMN, /FRAME)
  wNormDetLabel = Widget_Label(wNormDetBase, YSIZE = 30, XSIZE = 250, VALUE = 'Select airshot in scatterBrain plot screen to ' +String([13b]) + 'load counts for normalising counters.')
  wNormDetRowBase = Widget_Base(wNormDetBase, /ROW)
  wNormI0Text = FSC_Field(wNormDetRowBase, VALUE = '1', XSIZE = 10, TITLE = 'I0 (c/s)', /NONSENSITIVE, /COLUMN, UNAME = 'I0 VAL')
  wNormSlashLabel = Widget_Label(wNormDetRowBase, VALUE = '/')
  wNormIBSText = FSC_Field(wNormDetRowBase, VALUE = '1', XSIZE = 10, TITLE = 'IBS (c/s)', /NONSENSITIVE, /COLUMN,UNAME = 'IBS VAL')
  wNormEqualsLabel = Widget_Label(wNormDetRowBase, VALUE = '=')
  wNormNormText = FSC_Field(wNormDetRowBase, VALUE = '1', XSIZE = 10, TITLE = 'Ratio', /NONSENSITIVE, /COLUMN, UNAME = 'RATIO VAL')
  wNormDetectors = Widget_Button(wNormDetBase, VALUE = 'Apply Normalisation to I0/Beam Stop Counters', UNAME = 'NORM COUNTERS')
  wBuffer = Widget_Label(wFrameBase, VALUE = '')
  wNormTypeBase = Widget_Base(wFrameBase, /COLUMN, /FRAME)
  wNormTypeLabel = Widget_Label(wNormTypeBase, VALUE = 'Select to Change Type of Normalisation')
  wNormTypeButBase = Widget_Base(wNormTypeBase, /EXCLUSIVE, /COLUMN)
  wNoNormBut = Widget_Button(wNormTypeButBase, VALUE = 'No Normalisation', UNAME = 'NO NORM')
  wI0NormBut = Widget_Button(wNormTypeButBase, VALUE = 'Incident Intensity Normalisation', UNAME = 'I0 NORM')
  wIBSNormBut = Widget_Button(wNormTypeButBase, VALUE = 'Beamstop Intensity Normalisation', UNAME = 'IBS NORM')
  wI0BSNormBut = Widget_Button(wNormTypeButBase, VALUE = 'Do Not Select This Option', UNAME = 'I0BS NORM')
  
  wApplyBut = Widget_Button(wNormTypeBase, VALUE = 'Apply change of Normalisation', UNAME = 'APPLY')
  
  Widget_Control, self.wNormBase, /REALIZE
  Widget_Control, self.wNormBase, SET_UVALUE = self
  
  RETURN, 1

END

PRO as_normalisation::ShowGUI

  Widget_Control, self.wNormBase, /MAP

END

PRO as_normalisation::DetCounts, I0, IBS, expTime

  IF N_Elements(I0) GT 0 AND N_Elements(IBS) GT 0 THEN BEGIN

    self.ratio = I0/Float(IBS)
    self.I0Rate = I0/expTime
    self.IBSRate = IBS/expTime

    Widget_Control, Widget_Info(self.wNormBase, FIND_BY_UNAME='I0 VAL'), SET_VALUE = String(self.I0Rate)
    Widget_Control, Widget_Info(self.wNormBase, FIND_BY_UNAME='IBS VAL'), SET_VALUE = String(self.IBSRate)
    Widget_Control, Widget_Info(self.wNormBase, FIND_BY_UNAME='RATIO VAL'), SET_VALUE = String(self.ratio)
  ENDIF

END

PRO as_normalisation::EditConfig, RENAME=rename, DELETE=delete

  

END

PRO as_normalisation__define

  void = {NORMRETURN, normType : 0, counterRatio : 0, I0Norm : 0.0, IBSNorm : 0.0}

  void = {as_normalisation, $
          groupLeader : 0L, $
          notifyObj : Ptr_New(), $
          normSelected : 0, $ 
          ratio : 0.0, $
          I0Rate : 0.0, $
          IBSRate : 0.0, $
          wNormBase : 0L  }

END
