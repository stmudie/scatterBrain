PRO as_saxscontrol_event, event
  Widget_Control, event.top, GET_UVALUE = as_saxscontrol
  as_saxscontrol->event, event
END

PRO as_saxscontrol::event, event

  CASE self.state OF
    'STARTACQUISITION'       :  self->moveAcquireStart
    'ACQUIRECHECKCLEAR'      :  self->checkAcquireClear
    'ACQUIRE'                :  self->acquire
    'STARTTRANSMISSION'      :  self->moveTransmissionStart
    'TRANSMISSIONCHECKCLEAR' :  self->checkAcquireClear
    'TRANSMISSION'           :  self->transmission
    'ABORT'                  :
  ENDCASE
 
END

PRO as_saxscontrol::moveAcquireStart

  FOR i = 0, N_Elements(*self.PVMap) - 1 DO BEGIN
    (*self.PVMap)[i]->GetProperty, acquireEnable = ENABLE
    IF ENABLE EQ 1 THEN BEGIN
      (*self.PVMap)[i]->GetProperty, PVDone = DONE, PVRBV = RBV
      result = CASetMonitor(DONE)
      result = CASetMonitor(RBV)
    ENDIF
  ENDFOR

  self->moveAcquire  
      
  Widget_Control, self.wSaxsControlBase, TIMER = 0.1
  self.state = 'ACQUIRECHECKCLEAR'

END

PRO as_saxscontrol::moveTransmissionStart

    FOR i = 0, N_Elements(*self.PVMap) DO BEGIN
    IF (*self.PVMap)[i].transmissionEnable THEN BEGIN
      result = CASetMonitor((*self.PVMap)[i].PVDone)
      result = CASetMonitor((*self.PVMap)[i].PVRBV)
    ENDIF
  ENDFOR

  self->moveTransmission  
      
  Widget_Control, self.wSaxsControlBase, TIMER = 0.1
  self.state = 'TRANSMISSIONCHECKCLEAR'

END

PRO as_saxscontrol::checkAcquireClear
  enable = IntArr(N_Elements(*self.PVMap))
  FOR i = 0, N_Elements(enable) - 1 DO BEGIN
    (*self.PVMap)[i]->GetProperty, acquireEnable = temp
    enable[i] = temp
  ENDFOR
  
  enableIndex = Where(enable EQ 1)
  IF enableIndex[0] EQ -1 THEN BEGIN
    self.state = 'ACQUIRE'
    Widget_Control, self.wSaxsControlBase, TIMER = 0.1
    RETURN
  ENDIF
  self->DoneMoving, enableIndex
  IF Total(enableIndex) EQ N_Elements(enableIndex) THEN BEGIN
    result = caGetArray((*self.PVMap)[enableIndex].PVRBV, rbv)
    test = rbv GT (*self.PVMap)[enableIndex].acquireLow AND rbv LT (*self.PVMap)[enableIndex].enableHigh
  ENDIF ELSE BEGIN
    self.state = 'ACQUIRECHECKCLEAR'
    Widget_Control, self.wSaxsControlBase, TIMER = 0.1
    RETURN
  ENDELSE

  IF Where(test NE 1) EQ -1 THEN BEGIN
    self.state = 'ACQUIRE'
    result = CAClearMonitor((*self.PVMap)[i].PVDone)
    result = CAClearMonitor((*self.PVMap)[i].PVRBV)
  ENDIF ELSE BEGIN   
    result = Where(test EQ 0, COUNT=N_Error)
    PVlist = (*self.PVMap)[N_Error].Name
    result = Dialog_Message("PVs " + PVList + " have finished moving but aren't in position. Returning ...", /ERROR)
    self.state = ''
    RETURN
  ENDELSE
  
  Widget_Control, self.wSaxsControlBase, TIMER = 0.1

END

PRO as_saxscontrol::checkTransmissionClear

  enableIndex = Where((*self.PVMap)[i].transmissionEnable EQ 1)
  self->DoneMoving, enableIndex 
  IF Total(enableIndex) EQ N_Elements(enableIndex) THEN BEGIN
    result = caGetArray((*self.PVMap)[enableIndex].PVRBV, rbv)
    test = rbv GT (*self.PVMap)[enableIndex].transmissionLow AND rbv LT (*self.PVMap)[enableIndex].enableHigh
  ENDIF ELSE BEGIN
    self.state = 'TRANSMISSIONCHECKCLEAR'
  ENDELSE

  IF Where(test NE 1) EQ -1 THEN BEGIN
    self.state = 'TRANSMISSION'
    result = CAClearMonitor((*self.PVMap)[i].PVDone)
    result = CAClearMonitor((*self.PVMap)[i].PVRBV)
  ENDIF ELSE BEGIN   
    result = Where(test EQ 0, COUNT=N_Error)
    PVlist = (*self.PVMap)[N_Error].Name
    result = Dialog_Message("PVs " + PVList + " have finished moving but aren't in position. Returning ...", /ERROR)
    self.state = ''
    RETURN
  ENDELSE
  
  Widget_Control, self.wSaxsControlBase, TIMER = 0.1

END

PRO as_saxscontrol::acquire

  IF Obj_Valid((*self.notifyObj).object) THEN Call_Method, (*self.notifyObj).method, (*self.notifyObj).object 

END

PRO as_saxscontrol::transmission

  result = CAPut('SR13ID01HU02IOC02:TRANSMISSION', 1)  

END

PRO as_saxscontrol::startacquisition

  Widget_Control, self.wSaxsControlBase, TIMER = 0.1
  self.state = 'STARTACQUISITION'

END

PRO as_saxscontrol::starttransmission

  Widget_Control, self.wSaxsControlBase, TIMER = 0.1
  self.state = 'STARTTRANSMISSION'

END

PRO as_saxscontrol::abort

  self.state = 'ABORT'

END

PRO as_saxscontrol::GetProperty, STATE=state, _Ref_Extra = extra

  IF Arg_Present(state) THEN state = self.state
  self->IDLitComponent::GetProperty, _extra=extra
  
END

PRO as_saxscontrol::SetProperty, STATE=state, _Ref_Extra = extra

  IF KeyWord_Set(state) THEN self.state = state
  self->as_pvmap::SetProperty, _extra=extra
  
  
END

PRO as_saxscontrol::Cleanup

  Ptr_Free, self.notifyObj

END


FUNCTION as_saxscontrol::init, NOTIFYOBJ = notifyObj

  IF N_Elements(notifyObj) GT 0 THEN BEGIN
    FOR i = 0, N_Elements(notifyObj) - 1 DO BEGIN
      tags = Tag_Names(notifyObj)
      void  = Where(tags EQ 'METHOD', count1)
      void = Where(tags EQ 'OBJECT', count2)
      IF count1 + count2 NE 2 THEN BEGIN
        result = Dialog_Message('Incorrect tags. Detector Map Object not created.')
        RETURN, 0
      ENDIF
    ENDFOR
    self.notifyObj = Ptr_New(notifyObj)
  ENDIF ELSE self.notifyObj = Ptr_New(/ALLOCATE_HEAP)

  self.wSaxsControlBase = Widget_Base(/COLUMN, MAP=0)
 
  Widget_Control, self.wSaxsControlBase, /REALIZE
  Widget_Control, self.wSaxsControlBase, SET_UVALUE = self

  XManager, 'as_saxscontrol', self.wSaxsControlBase, /NO_BLOCK
  
  RETURN, self->as_pvmap::init()
  
END

PRO as_saxscontrol__define

  void = {as_saxscontrol, $
            INHERITS as_pvmap,            $
            notifyObj        : Ptr_New(), $
            wSaxsControlBase : 0L,        $
            state            : ''         $
         }

END