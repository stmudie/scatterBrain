PRO AS_PollAreaDetector_event,event

  Widget_Control, event.id, GET_UVALUE = AS_PollAreaDetector
  AS_PollAReaDetector->Event,event 
  
END

PRO AS_PollAreaDetector::Event,event

  IF self.areaDetectorObj.areaDetectorImageObj->NewArray() THEN BEGIN
    fullFileName = self.areaDetectorObj.areaDetectorObj->GetProperty('FullFileName_RBV')
    filePath = self.areaDetectorObj.areaDetectorObj->GetProperty('FilePath_RBV')
    fileName = String(fullfileName[Where(fullfileName NE filePath)])
    self.notify, fileName
    wCount = Widget_Info(self.wBase, FIND_BY_UNAME = 'Count')
    Widget_Control, wCount, GET_VALUE = count
    count = StrCompress(String(1 + Fix(Count)))
    Widget_Control, wCount, SET_VALUE = count
  ENDIF
  IF self.poll EQ 1 THEN Widget_Control, event.id, TIMER=self.interval
END

PRO AS_PollAreaDetector::StartPoll, INTERVAL = interval
  Widget_Control, self.wBase, /MAP
  IF Keyword_Set(interval) THEN self.interval = interval ELSE self.interval = 0.1
  Widget_Control, self.wBase, TIMER=self.interval
  self.poll = 1
END

PRO AS_PollAreaDetector::StopPoll
  Widget_Control, self.wBase, MAP = 0
  self.poll = 0
END

PRO AS_PollAreaDetector::notify, event

  FOREACH notify, self.notifyObject DO BEGIN
    IF Obj_Valid(notify) THEN notify.notify, event
    wait, 0.2
  ENDFOREACH

END

FUNCTION AS_PollAreaDetector::INIT, areaDetectorObj, NOTIFYOBJECT = notifyObject, GROUP_LEADER=groupLeader

  CASE !D.NAME OF
      'WIN': COUNTFONT = "BOLD*24"
      'x'  : COUNTFONT = "lucidasans-bold-24"
    ELSE : COUNTFONT = ""
    ENDCASE

  IF Obj_Valid(areaDetectorObj.areaDetectorImageObj) THEN BEGIN
    IF Obj_Class(areaDetectorObj.areaDetectorImageObj) NE 'EPICS_ND_STD_ARRAYS' THEN RETURN, -1
  ENDIF ELSE RETURN, -1
  
  IF Obj_Valid(areaDetectorObj.areaDetectorObj) THEN BEGIN
    IF Obj_Class(areaDetectorObj.areaDetectorObj) NE 'EPICS_AD_FILE' THEN RETURN, -1
  ENDIF ELSE RETURN, -1
  
  self.areaDetectorObj = areaDetectorObj

IF Keyword_Set(notifyObject) THEN $
    IF TypeName(notifyObject[0]) EQ 'NOTIFY' $
      THEN self.notifyObject = List(notifyObject,/EXTRACT)
  
  IF Keyword_Set(GROUP_LEADER) THEN self.wBase = Widget_Base(GROUP_LEADER=groupLeader, /FLOATING, /COLUMN,TLB_FRAME_ATTR=11) $
                               ELSE self.wBase = Widget_Base(/COLUMN,TLB_FRAME_ATTR=11)
  wCountLabelLabel = Widget_Label(self.wBase, VALUE = 'No. of areaDetector Frames',FONT = COUNTFONT)
  wCountLabel = Widget_Label(self.wBase, VALUE = '          0', UNAME = 'Count',FONT = COUNTFONT)
  
  Widget_Control, self.wBase, /REALIZE
  Widget_Control, self.wBase, MAP = 0
  Widget_Control, self.wBase, SET_UVALUE = self
    
  XManager, 'AS_PollAreaDetector', self.wBase, /NO_BLOCK
  
  RETURN, 1

END

PRO AS_PollAreaDetector__define

void = {AS_PollAreaDetector, $
        wBase           : 0L, $
        poll            : 0, $
        areaDetectorObj : {areaObjs, areaDetectorObj : Obj_New(), areaDetectorImageObj : Obj_New()}, $
        interval        : 0.0, $
        notifyObject    : List()}

END