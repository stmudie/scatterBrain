PRO as_pv_PropertyEvent, event

  prop = Widget_Info(event.handler, FIND_BY_UNAME = 'PropSheet')
  Widget_Control, prop, GET_UVALUE = as_pv
  as_pv->as_pvmap::event, event 

END

FUNCTION as_pvmap::init, PVMap

  self.pvmap = Ptr_New(/ALLOCATE_HEAP)
  
  void = Obj_New('as_pv')

  IF N_Elements(PVMap) NE 0 THEN BEGIN
    void->SetProperty, STRUCT = PVMap[0]
  ENDIF

  *self.pvmap = void
  
  FOR i = 1, N_Elements(PVMap)-1 DO BEGIN
  
    void = Obj_New('as_pv')
    void->SetProperty, STRUCT = PVMap[i]
    *self.pvmap = [*self.pvmap, void]
  
  ENDFOR
                          
  RETURN, 1

END

PRO as_pvmap::event, event

  widgetName = Widget_Info(event.id, /UNAME)

  CASE widgetName OF
  'PropSheet'   : BEGIN
                    IF (event.type EQ 0) THEN BEGIN ; Value changed. 
                   
                      ; Get the value of the property identified by 
                      ; event.identifier. 
                      value = Widget_Info(event.ID, COMPONENT = event.component, PROPERTY_VALUE = event.identifier) 
                     
                      ; Set the component's property value. 
                      event.component->SetPropertyByIdentifier, event.identifier, value 
                   
                    ENDIF ELSE BEGIN ; Selection changed. 
                   
                    ENDELSE 
                    
                    self.selComponent = event.component
                    Widget_Control, self.prop, /REFRESH_PROPERTY
                  
                  END

    'PropBase'  : BEGIN
                    Widget_Control, self.prop, SCR_XSIZE = event.x, SCR_YSIZE = event.y 
                  END
    'Add PV'    : BEGIN
                    IF N_Elements(*self.pvmap) GT 0 THEN *self.pvmap = [*self.pvmap, Obj_New('as_pv')] ELSE *self.pvmap = Obj_New('as_pv')
                    Widget_Control, self.prop, SET_VALUE=*self.pvmap
                  END
    'Delete PV' : BEGIN
                    component = Where(*self.pvmap EQ self.selComponent, COMPLEMENT = keep)
                    *self.pvmap = (*self.pvmap)[keep]
                    Obj_Destroy, self.selComponent
                  END
  ENDCASE
END               

PRO as_pvmap::ShowGUI, base, GROUP_LEADER = groupLeader
  
  IF Arg_Present(base) THEN propBase = Widget_Base(base, GROUP_LEADER = groupLeader, EVENT_PRO = 'as_pv_PropertyEvent', /COLUMN, /TLB_SIZE_EVENTS, UNAME='PropBase') $
                        ELSE BEGIN
                          propBase = Widget_Base(GROUP_LEADER = groupLeader, EVENT_PRO = 'as_pv_PropertyEvent', /COLUMN, /TLB_SIZE_EVENTS, UNAME = 'PropBase')
                          Widget_Control, propBase, /REALIZE
                          XManager, 'as_pvmap', propBase, EVENT_HANDLER = 'as_pv_PropertyEvent', /NO_BLOCK
                        ENDELSE
                        
  self.prop = Widget_PropertySheet(propBase, GROUP_LEADER = groupLeader, VALUE = *self.pvmap, $
                              EVENT_PRO = 'as_pv_PropertyEvent', UNAME = 'PropSheet', YSIZE=20, /MULTIPLE_PROPERTIES)
  
  add    = Widget_Button(propBase, GROUP_LEADER = groupLeader, SCR_XSIZE = 50, VALUE = 'Add PV', UNAME = 'Add PV')
  delete = Widget_Button(propBase, GROUP_LEADER = groupLeader, SCR_XSIZE = 50, VALUE = 'Delete PV', UNAME = 'Delete PV')
  
  Widget_Control, self.prop, SET_UVALUE = self
  
END

PRO as_pvmap::MoveAcquire

  FOR i = 0, N_Elements(*self.PVMap) - 1 DO BEGIN
    (*self.PVMap)[i]->GetProperty, PVCheckDrive = CheckDrive, acquireEnable = Enable
    IF CheckDrive AND Enable THEN BEGIN
      result = caGet((*self.PVMap)[i].PVRBV, readBack)
      IF readBack LT (*self.PVMap)[i].acquireLow THEN result = caPut((*self.PVMap)[i].PVSet,(*self.PVMap)[i].acquireLow) $ 
                                                 ELSE IF readBack GT (*self.PVMap)[i].acquireHigh $
                                                 THEN result = caPut((*self.PVMap)[i].PVSet,(*self.PVMap)[i].acquireHigh)
    ENDIF
  ENDFOR

END

PRO as_pvmap::MoveTransmission
  
  FOR i = 0, N_Elements(*self.PVMap) DO BEGIN
    IF (*self.PVMap)[i].PVCheckDrive AND (*self.PVMap)[i].acquireTransmission THEN BEGIN
      result = caGet((*self.PVMap)[i].PVRBV, readBack)
      IF readBack LT (*self.PVMap)[i].transmissionLow THEN result = caPut((*self.PVMap)[i].PVSet,(*self.PVMap)[i].transmissionLow) $ 
                                                 ELSE IF readBack GT (*self.PVMap)[i].transmissionHigh $
                                                 THEN result = caPut((*self.PVMap)[i].PVSet,(*self.PVMap)[i].transmissionHigh)
    ENDIF
  ENDFOR

END


PRO as_pvmap::DoneMoving, index

  IF N_Elements(index) GT 0 THEN BEGIN
  
    notNull = Where((*self.pvmap).PVDone NE '', NCOMPLEMENT = numNull, COMPLEMENT=null)
    donePVs = ((*self.pvmap).PVDone)[notNull]
    result = caGetArray(donePVs,done) 
    nullPVs = ((*self.pvmap).PVRBV)[null]
    result = caGetArray(nullPVs, pos)
    
    check = Where(SysTime(1) - ((*self.pvmap).lastTime)[null[i]] GE 0.5)
    nullDone = IntArr(numNull)
    nullDone[check] = Abs(((*self.pvmap).lastPosition)[null[check]] - pos[check[i]]) LT $
                         pos[check]*((*self.pvmap).doneDeadBand)[null[check]] 
    
    ((*self.pvmap).lastPosition)[null[check]] = pos[null[check]]
    index[notNull] = done
    index[null] = nullDone
  
  ENDIF

END

PRO as_pvmap::NewParams, paramObj

  paramObj->GetParameters, PVMAP=PVMap

  numPVs = N_Elements(*self.pvmap)
  numNewPVs = N_Elements(PVMap)
  
  IF numNewPVs GT numPVs THEN FOR i = 0, numNewPVs-numPVs - 1 DO *self.pvmap = [*self.pvmap, Obj_New('as_pv')]
  IF numNewPVs LT numPVs THEN BEGIN
    Obj_Destroy, (*self.pvmap)[numNewPVs:*]
    *self.pvmap = (*self.pvmap)[0:numNewPVs-1]
  ENDIF
  
  FOR i = 0, numNewPVs - 1 DO BEGIN
    (*self.pvmap)[i]->NewParams, PVMap[i]
  ENDFOR

END

PRO as_pvmap::GetProperty
  
                                             
END

FUNCTION as_pvmap::getLogData

  readBackStruct = Replicate({ Name: '', PVRBV: '', Value: 0.0 }, N_Elements(*self.pvmap))

  FOR i = 0, N_Elements(*self.pvmap) - 1 DO BEGIN
  
    (*self.pvmap)[i]->GetProperty, NAME=name, PVRBV = rbv, LOGENABLE = log
    IF log EQ 1 THEN BEGIN
      (*self.pvmap)[i]->GetProperty, READBACK = readBack
      readBackStruct[i].Name = name
      readBackStruct[i].PVRBV = rbv
      readBackStruct[i].Value = readBack
    ENDIF
  
  ENDFOR

  RETURN, readBackStruct

END

PRO as_pvmap::AddPV, newPVData

  type = Size(newPVData, /TYPE)
  
  CASE type OF
    7  :    BEGIN
              pv = Obj_New('AS_PV')
              IF N_Elements(newPVData) EQ 1 THEN BEGIN
                pv->SetProperty, PVName = newPVData
              ENDIF
              IF N_Elements(newPVData) GT 0 AND N_Elements(newPVData) MOD 2 THEN BEGIN
                void = Create_Struct(AS_PV)
                tags = Tag_Names(void)
                FOR i = 0, N_Elements(newPVData)/2 - 1 DO BEGIN
                  tagPos = Where(tags EQ StrUpCase(newPVData[i*2]))
                  void.(tagPos) = newPVDAta[i*2+1]
                ENDFOR
                pv->SetProperty, STRUCT = void
              ENDIF 
            END
    8  :    BEGIN
              IF Tag_Names(newPVData,/STRUCTURE_NAME) EQ'AS_PV' THEN BEGIN
                pv = Obj_New('AS_PV')
                pv->SetProperty, STRUCT = newPVData
              ENDIF
            END
    11 :    BEGIN
              IF Obj_Class(newPVData) EQ 'AS_PV' THEN IF N_Elements(*self.pvmap) GT 0 THEN *self.pvmap = [*self.pvmap,newPVData] $
                                                                                      ELSE *self.pvmap = newPVData
            END                                                                                      
    ELSE :  
  ENDCASE

END

PRO as_pvmap::cleanup

  IF N_Elements(*self.pvMap) GT 0 THEN Obj_Destroy, *self.pvmap
  Ptr_Free, self.pvmap

END

PRO as_pvmap__define

void = {as_pvmap, $
          pvmap         : Ptr_New(), $
          prop          : 0L       , $
          selComponent  : Obj_New()  $
       }

END