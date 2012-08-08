FUNCTION as_pv::init, _Ref_Extra = extra

  result = self->IDLitComponent::Init(_extra=extra)
  
  self->RegisterProperty, 'PVSet',              4    
  self->RegisterProperty, 'PVRBV',              4
  self->RegisterProperty, 'PVDone',             4
  self->RegisterProperty, 'PVCheckDrive',       1
  self->RegisterProperty, 'AcquireHigh',        3
  self->RegisterProperty, 'AcquireLow',         3
  self->RegisterProperty, 'AcquireEnable',      1
  self->RegisterProperty, 'TransmissionHigh',   3
  self->RegisterProperty, 'TransmissionLow',    3
  self->RegisterProperty, 'TransmissionEnable', 1
  self->RegisterProperty, 'ReadBack',           3
  self->RegisterProperty, 'SetPoint',           3
  self->RegisterProperty, 'LogEnable',          1
                        
  RETURN, result

END

PRO as_pv::NewParams, params

  newParamNames = Tag_Names(params)
  paramNames = Tag_Names({as_pv})
  
  FOR paramNum = 0, N_Elements(newParamNames) - 1 DO BEGIN
    paramPos = Where(paramNames EQ newParamNames[paramNum])
    IF paramPos GE 0 THEN self.(paramPos) = params.(paramNum) 
  ENDFOR

END

PRO as_pv::GetProperty, $
  PVSET              = PVSet,              $
  PVRBV              = PVRBV,              $
  PVDONE             = PVDone,             $
  PVCHECKDRIVE       = PVCheckDrive,       $
  ACQUIREHIGH        = acquireHigh,        $
  ACQUIRELOW         = acquireLow,         $
  ACQUIREENABLE      = acquireEnable,      $
  TRANSMISSIONHIGH   = transmissionHigh,   $
  TRANSMISSIONLOW    = transmissionLow,    $
  TRANSMISSIONENABLE = transmissionEnable, $
  LOGENABLE          = logEnable,          $
  LASTPOSITION       = lastPosition,       $
  LASTTIME           = lastTime,           $
  READBACK           = readBack,           $
  SETPOINT           = setPoint,           $
  _REF_EXTRA         = extra 
                   
  IF Arg_Present(PVSet)                THEN PVSet                = self.PVSet
  IF Arg_Present(PVRBV)                THEN PVRBV                = self.PVRBV
  IF Arg_Present(PVDone)               THEN PVDone               = self.PVDone            
  IF Arg_Present(PVCheckDrive)         THEN PVCheckDrive         = self.PVCheckDrive
  IF Arg_Present(acquireHigh)          THEN acquireHigh          = self.acquireHigh       
  IF Arg_Present(acquireLow)           THEN acquireLow           = self.acquireLow        
  IF Arg_Present(acquireEnable)        THEN acquireEnable        = self.acquireEnable     
  IF Arg_Present(transmissionHigh)     THEN transmissionHigh     = self.transmissionHigh  
  IF Arg_Present(transmissionLow)      THEN transmissionLow      = self.transmissionLow   
  IF Arg_Present(transmissionEnable)   THEN transmissionEnable   = self.transmissionEnable
  IF Arg_Present(logEnable)            THEN logEnable            = self.logEnable
  IF Arg_Present(lastPosition)         THEN lastPosition         = self.lastPosition
  IF Arg_Present(lastTime)             THEN lastTime             = self.lastTime

  IF Arg_Present(readBack) THEN BEGIN
    readBack = ''
    IF self.PVRBV NE '' THEN BEGIN
      result = CAGet(self.PVRBV, read)
      IF result EQ 0 THEN readBack = read
    ENDIF
  ENDIF

  IF Arg_Present(setPoint) THEN BEGIN
    setPoint = ''
    IF self.PVSet NE '' THEN BEGIN 
      result = CAGet(self.PVSet, set)
      IF result EQ 0 THEN setPoint = set
    ENDIF
  ENDIF

  self->IDLitComponent::GetProperty, _EXTRA = extra
                                             
END

PRO as_pv::SetProperty,  $
  PVSET              = PVSet,              $
  PVRBV              = PVRBV,              $
  PVDONE             = PVDone,             $
  PVCHECKDRIVE       = PVCheckDrive,       $
  ACQUIREHIGH        = acquireHigh,        $
  ACQUIRELOW         = acquireLow,         $
  ACQUIREENABLE      = acquireEnable,      $
  TRANSMISSIONHIGH   = transmissionHigh,   $
  TRANSMISSIONLOW    = transmissionLow,    $
  TRANSMISSIONENABLE = transmissionEnable, $
  LOGENABLE          = logEnable,          $
  LASTPOSITION       = lastPosition,       $
  STRUCT             = struct,             $
  _REF_EXTRA         = extra

  IF N_Elements(struct) GT 0 THEN BEGIN
    IF Tag_Names(struct, /STRUCTURE_NAME) EQ 'AS_PV' THEN BEGIN
      PVSet              = struct.PVSet
      PVRBV              = struct.PVRBV
      PVDone             = struct.PVDone
      PVCheckDrive       = struct.PVCheckDrive
      acquireHigh        = struct.acquireHigh
      acquireLow         = struct.acquireLow
      acquireEnable      = struct.acquireEnable
      transmissionHigh   = struct.transmissionHigh
      transmissionLow    = struct.transmissionLow
      transmissionEnable = struct.transmissionEnable
      logEnable          = struct.logEnable
      lastPosition       = struct.lastposition
    ENDIF
  ENDIF

  IF N_Elements(PVSet)                THEN BEGIN
    result = CASearch(PVSet)
    caStartGroup
    IF self.PVSet NE '' THEN clear = CAClearMonitor(self.PVset)
    IF result EQ 0 THEN set = CASetMonitor(PVset)
    result = caEndGroup()
    self.PVSet = PVSet
  ENDIF            
  IF N_Elements(PVRBV)                THEN BEGIN
    result = CASearch(PVRBV)
    caStartGroup
    IF self.PVRBV NE '' THEN clear = CAClearMonitor(self.PVRBV)
    IF result EQ 0 THEN set = CASetMonitor(PVRBV)
    result = caEndGroup()
    self.PVRBV = PVRBV
  ENDIF
  IF N_Elements(PVDone)               THEN BEGIN
    result = CASearch(PVDone)
    caStartGroup
    IF self.PVDone NE '' THEN clear = CAClearMonitor(self.PVDone)
    IF result EQ 0 THEN set = CASetMonitor(PVDone)
    result = caEndGroup()
    self.PVDone = PVDone        
  ENDIF  
  IF N_Elements(PVCheckDrive)         THEN self.PVCheckDrive        = PVCheckDrive
  IF N_Elements(acquireHigh)          THEN self.acquireHigh         = acquireHigh       
  IF N_Elements(acquireLow)           THEN self.acquireLow          = acquireLow        
  IF N_Elements(acquireEnable)        THEN self.acquireEnable       = acquireEnable     
  IF N_Elements(transmissionHigh)     THEN self.transmissionHigh    = transmissionHigh  
  IF N_Elements(transmissionLow)      THEN self.transmissionLow     = transmissionLow   
  IF N_Elements(transmissionEnable)   THEN self.transmissionEnable  = transmissionEnable
  IF N_Elements(logEnable)            THEN self.logEnable           = logEnable
  IF N_Elements(lastPosition)         THEN BEGIN
    self.lastPosition        = lastPosition
    self.lastTime            = SysTime(1)
  ENDIF

  IF N_Elements(extra) GE 1 THEN IF Where(extra EQ 'NAME') EQ -1 THEN self->IDLitComponent::SetProperty, _EXTRA = extra, NAME=PVSet $
                                                                 ELSE self->IDLitComponent::SetProperty, _EXTRA = extra
                                                                         
END

PRO as_pv__define

void = {as_pv, $
         INHERITS IDLitComponent,        $
         PVSet              : '',        $
         PVRBV              : '',        $
         PVDone             : '',        $
         PVCheckDrive       : 0,         $
         acquireHigh        : 0.0,       $ 
         acquireLow         : 0.0,       $
         acquireEnable      : 0  ,       $
         transmissionHigh   : 0.0,       $
         transmissionLow    : 0.0,       $
         transmissionEnable : 0,         $
         doneDeadBand       : 0.0,       $
         lastPosition       : 0.0,       $
         logEnable          : 0,         $
         lastTime           : SysTime(1) $
       }

END