PRO AS_SAXSExcelScans_event, event

  @as_scatterheader.macro

  Widget_Control, event.top, GET_UVALUE = AS_SAXSExcelScans
  AS_SAXSExcelScans.event, event

END

FUNCTION AS_SAXSExcelScans::INIT, basePV, detector, GUI=GUI, _REF_Extra = extra

  @as_scatterheader.macro
  
  self.basePV = basePV
  self.detector = detector
  
  IF KeyWord_Set(GUI) THEN BEGIN
    wScanBase    = Widget_Base(/ROW)
    wGetParams   = Widget_Button(wScanBase, VALUE = 'Get Parameters From Excel', UNAME = 'GET PARAMS')
    wInitialise  = Widget_Button(wScanBase, VALUE = 'Initialise Scan', UNAME = 'INITIALISE')
    wStart       = Widget_Button(wScanBase, VALUE = 'Start Scan', UNAME = 'START')
    wPointNumber = Widget_Label( wScanBase, VALUE = '000000', UNAME = 'POINT')
  
    Widget_Control, wScanBase, /REALIZE
    Widget_Control, wScanBase, SET_UVALUE = self
  
    XManager, 'AS_SAXSExcelScans', wScanBase, /NO_BLOCK
    self.wScanBase = wScanBase
  ENDIF
  
  !Null = {scanStruct, MODE : '', LOOP : 0, POSITIONER : '', STARTVALUE : 0.0d, ENDVALUE : 0.0d, STEP : 0.0d, NUMBER : 0.0d, POINTS : Ptr_New()}
  self.scanArray = Ptr_New(/ALLOCATE_HEAP)
  self.namesArray = Ptr_New(/ALLOCATE_HEAP)
  CAInit
  RETURN, self->as_talktoexcel::init(_extra = extra)

END

PRO AS_SAXSExcelScans::event, event

  @as_scatterheader.macro

  widgetName = Widget_Info(event.id, /UNAME)
  
  CASE widgetName OF
    'GET PARAMS' : BEGIN 
                     Widget_Control, /HOURGLASS
                     self.GetScanParams
                     Widget_Control, HOURGLASS = 0
                   END
    'INITIALISE' : self.InitialiseScan
    'START'      : BEGIN
                     self.Start
                     FOR i = 1, self.levels DO result = CASetMonitor(self.basePV + 'scan' + StrCompress(i,/REMOVE_ALL) + '.CPT')
                     FOR i = 1, self.levels DO result = CASetMonitor(self.basePV + 'scan' + StrCompress(i,/REMOVE_ALL) + '.NPTS')
                     Widget_Control, Widget_Info(self.wScanBase, FIND_BY_UNAME='POINT'), TIMER = 0.5
                   END
    'POINT'      : BEGIN
                     finishedPoints = IntArr(self.levels)
                     numPoints = IntArr(self.levels)
                     FOR i = 1, self.levels DO BEGIN
                       result = CAGet(self.basePV + 'scan' + StrCompress(i,/REMOVE_ALL) + '.CPT', tempFinished)
                       result = CAGet(self.basePV + 'scan' + StrCompress(i,/REMOVE_ALL) + '.NPTS', tempNumber)
                       pos = Where((*self.basePV + 'scan').loop EQ i)
                       currentPos = (self.GetRange('points', OFFSET = [tempFinished,0], /ADDRESS))[pos]
                       IF self.currentPos[i-1] EQ '' THEN FOR j = 0, N_Elements(currentPos) - 1 DO self.highlight, currentPos[j] $
                                                     ELSE FOR j = 0, N_Elements(currentPos) - 1 DO self.highlight, (StrSplit(self.currentPos[i-1],',',/EXTRACT))[j], MOVE = currentPos[j]
                       self.currentPos[i-1] = StrJoin(currentPos,',')
                       finishedPoints[i-1] = tempFinished
                       numPoints[i-1] = tempNumber
                     ENDFOR
                     totalPoints = 1
                     FOR i = 1, self.levels DO BEGIN 
                       totalPoints = numPoints[i-1]*totalPoints
                       CASE i OF 
                          1  : IF finishedPoints[0] LT numPoints[0] OR self.levels EQ 1 THEN totalFinishedPts = finishedPoints[0] ELSE totalFinishedPts = 0 
                          2  : totalFinishedPts += finishedPoints[1]*numPoints[0]
                          3  : totalFinishedPts += finishedPoints[2]*numPoints[0]*numPoints[1]
                        ENDCASE
                     ENDFOR
                     Widget_Control, event.id, SET_VALUE = String(totalFinishedPts)
                     IF totalFinishedPts GE totalPoints THEN BEGIN 
                        FOR i = 1, self.levels DO result = CASetMonitor(self.basePV + 'scan' + StrCompress(i,/REMOVE_ALL) + '.CPT')
                        FOR i = 1, self.levels DO result = CASetMonitor(self.basePV + 'scan' + StrCompress(i,/REMOVE_ALL) + '.NPTS')
                        FOR i = 0, 3 DO FOREACH pos, StrSplit(self.currentPos[i],',',/EXTRACT) DO self.highlight, pos, /REMOVE
                        self.currentpos[*] = ''
                     ENDIF ELSE Widget_Control, event.id, TIMER=0.5
                   END
  ENDCASE

END

FUNCTION AS_SAXSExcelScans::GetScanParams

  @as_scatterheader.macro

  self.ChangeSheet, 'MoveExpose'
  numPositioners = self.GetRange('NumberPositioners')
  IF numPositioners EQ 0 THEN RETURN, -1
  IF Size(*self.scanArray, /type) EQ 8 THEN FOREACH ptr, (*self.scanArray).points DO Ptr_Free, ptr
  *self.scanArray = Replicate({scanStruct},numPositioners)
    
  (*self.scanArray).Number = (self.GetRange('Number'))[0:numPositioners-1]
  (*self.scanArray).Mode = (self.GetRange('Mode'))[0:numPositioners-1]
  (*self.scanArray).StartValue = (self.GetRange('Start'))[0:numPositioners-1]
  (*self.scanArray).EndValue = (self.GetRange('End'))[0:numPositioners-1]
  (*self.scanArray).Step = (self.GetRange('Step'))[0:numPositioners-1]
  pointsAddress = (self.GetRange('Points',/ADDRESS))[0:numPositioners-1]
  
  self.ChangeSheet, 'Enumerators'
  loopEnumeration = self.GetRange('LoopNumber')
  positionerEnumeration = self.GetRange('HumanPositioners')
  machinePosEnum = self.GetRange('MachinePositioners')
  self.ChangeSheet, 'MoveExpose'
  loop = self.GetRange('LOOP')
  positioner = self.GetRange('Positioner')
  
  FOR i = 0, numPositioners - 1 DO BEGIN
    (*self.scanArray)[i].positioner = machinePosEnum[Where(positionerEnumeration EQ positioner[i])]
    (*self.scanArray)[i].loop = Where(loopEnumeration EQ loop[i])
    IF (*self.scanArray)[i].Mode EQ 'Table' THEN BEGIN
      ((*self.scanArray)[i].points) = Ptr_New(self.GetRange(pointsAddress[i]+ ':' + self.GetRange(pointsAddress[i],OFFSET=[(*self.scanArray)[i].Number-1,0],/ADDRESS)))
    ENDIF
  ENDFOR
  
  self.ChangeSheet, 'SampleNames'
  self.runMacro, 'UpdateSampleNamesMacro'
  numLoop = IntArr(3)
  numLoop[*] = 1
  FOR i = 0, numPositioners - 1 DO BEGIN
    CASE (*self.scanArray)[i].loop OF
      1 : numLoop[0] = (*self.scanArray)[i].number
      2 : numLoop[1] = (*self.scanArray)[i].number
      3 : numLoop[2] = (*self.scanArray)[i].number
    ENDCASE
  ENDFOR
  numberPoints = numLoop[0]*numLoop[1]*numLoop[2]
  *self.namesArray = self.GetRange(self.GetRange('SampleNames', /ADDRESS) + ':' + self.GetRange('SampleNames', /ADDRESS, OFFSET=[numberPoints-1,0]))
  
  !Null = Where(*self.namesArray NE '', numNames)
  nameType = 0
  IF numNames LT numberPoints THEN BEGIN
    IF numNames GT 0 THEN BEGIN
      result = Dialog_Message("Number of names on SampleNames sheet less than number of points to be collected. Please fix.",/ERROR)
      ;IF result EQ 'No' THEN RETURN, -1
      RETURN, -1
    ENDIF 
    self.ChangeSheet, 'MoveExpose'
    numPositionNames = IntArr(3)
    FOR loop = 0, 2 DO BEGIN 
      loopPos = Where((*self.scanArray).loop EQ loop + 1)
      FOREACH l, loopPos DO BEGIN
        posNames = self.GetRange((self.GetRange('Points', /ADDRESS, OFFSET = [0,-1]))[l] + ':' + (self.GetRange('Points', /ADDRESS, OFFSET=[numLoop[loop]-1,-1]))[l])
        result = Where(posNames NE '', numberTemp)
        numPositionNames[loop] >= numberTemp
      ENDFOREACH
    ENDFOR
    
    IF numLoop[0] NE numPositionNames[0] THEN nameType += 1
    IF numLoop[1] NE numPositionNames[1] THEN nameType += 2
    IF numLoop[2] NE numPositionNames[2] THEN nameType += 4
    SWITCH nameType OF
      0 : 
      1 : 
      2 :
      3 :
      4 :
      5 :
      6 : BEGIN
            self.ChangeSheet, 'SampleNames'
            self.runMacro, 'UpdateSampleNamesMacro'
            *self.namesArray = self.GetRange(self.GetRange('SampleNames', /ADDRESS) + ':' + self.GetRange('SampleNames', /ADDRESS, OFFSET=[numberPoints-1,0]))
            BREAK
          END
      7 : BEGIN
            result = Dialog_Message("Incomplete filename or or filename fragment lists given. Scan will run using single file name (entered in main program), with incrementing suffix.")
            *self.namesArray = 0
          END
      ELSE :
    ENDSWITCH
  ENDIF
  
  self.ChangeSheet, 'MoveExpose'
  self.SetRange,'B2', 'Parameters Successfully Uploaded', fontStyle = 'Bold', fontColour = [0,97,0], backcolour= [198,239,206]
  RETURN, nameType
  
END

FUNCTION AS_SAXSExcelScans::ScanActive

  @as_scatterheader.macro

  scanning = 0
  result = CAGet(self.basePV + 'scan1.EXSC', scanning)
  RETURN, scanning

END

PRO AS_SAXSExcelScans::Start, scanName, scanDescription

  @as_scatterheader.macro

;  If N_Params() GE 1 THEN self.SetRange, '$B$1', scanName
;  If N_Params() GE 2 THEN self.SetRange, '$B$6', scanDescription
;  self.SaveCopyAs, scanName + 'scan.xlsm'
;  self.SetRange, '$B$1', 'SAXS Scan Worksheet'
;  self.SetRange, '$B$6', ''
  result = CAPut(self.basePV + 'scan' + StrCompress(self.levels,/REMOVE_ALL)+'.EXSC', 1)
  
END

PRO AS_SAXSExcelScans::Stop

  @as_scatterheader.macro

  result = CAPut(self.basePV + 'AbortScans.PROC', 0)
  Wait, 0.1
  result = CAPut(self.basePV + 'AbortScans.PROC', 0)
  Wait, 0.1
  result = CAPut(self.basePV + 'AbortScans.PROC', 0)

END

PRO AS_SAXSExcelScans::Pause, pause

  @as_scatterheader.macro

  IF ~Arg_Present(pause) THEN pause = 1
  IF pause NE 1 OR pause NE 0 THEN pause = 1
  result = CAPut(self.basePV + 'scan1.PAUS', pause)

END

PRO AS_SAXSExcelScans::InitialiseScan, NONAMES=noNames

  @as_scatterheader.macro

  IF ~KeyWord_Set(noNames) THEN noNames = 0 
  ;CAStartGroup
  FOR i = 1, 3 DO BEGIN
    j = 1
    number = 0
    loopPos = Where((*self.scanArray).Loop EQ i, count, /NULL)
    scanPV = self.basePV + 'scan' + StrCompress(i,/REMOVE_ALL)+'.'
    result = 0
    IF ~noNames THEN result += CAPut(self.basePV + 'fileIndex' + StrCompress(i,/REMOVE_ALL),1)
    result += CAPut(scanPV+'CMND',6)
    IF count GT 0 THEN BEGIN
      self.levels = i
      IF i EQ 1 THEN result += CAPut(scanPV+'T1PV', self.detector) $
                ELSE result += CAPut(scanPV+'T1PV', self.basePV + 'scan' + StrCompress(i-1,/REMOVE_ALL)+'.EXSC')
    ENDIF
    IF loopPos NE !Null THEN BEGIN
      FOREACH pos, loopPos DO BEGIN
        scanNum = StrCompress(j,/REMOVE_ALL)
        scanParams = (*self.scanArray)[pos]
        result += CAPut(scanPV+'R'+scanNum+'PV', scanParams.positioner)
        result += CAPut(scanPV+'P'+scanNum+'PV', scanParams.positioner)
        CASE (*self.scanArray)[pos].mode OF 
          'Linear' : BEGIN
                       result += CAPut(scanPV+'P'+scanNum+'SM', 0)
                       result += CAPut(scanPV+'P'+scanNum+'SP', scanParams.startvalue)
                       result += CAPut(scanPV+'P'+scanNum+'EP', scanParams.endvalue)
                     END
          'Table'  : BEGIN
                       result += CAPut(scanPV+'P'+scanNum+'SM', 1)
                       result += CAPut(scanPV+'P'+scanNum+'PA', *scanParams.points)
                     END
        ENDCASE
        number = number > scanParams.number
        j++
      ENDFOREACH
    ENDIF
    IF N_Elements(*self.namesArray) GT 1 THEN BEGIN
      result += CAPut(scanPV+'P'+StrCompress(4,/REMOVE_ALL)+'SM', 0)
      result += CAPut(scanPV+'P'+StrCompress(4,/REMOVE_ALL)+'SP', 1)
      result += CAPut(scanPV+'P'+StrCompress(4,/REMOVE_ALL)+'EP', number)
      result += CAPut(scanPV+'R'+StrCompress(4,/REMOVE_ALL)+'PV', self.basePV + 'fileIndex' + StrCompress(i,/REMOVE_ALL))
      result += CAPut(scanPV+'P'+StrCompress(4,/REMOVE_ALL)+'PV', self.basePV + 'fileIndex' + StrCompress(i,/REMOVE_ALL))
    ENDIF ELSE BEGIN
      result += CAPut(scanPV+'P'+StrCompress(4,/REMOVE_ALL)+'SM', 0)
      result += CAPut(scanPV+'P'+StrCompress(4,/REMOVE_ALL)+'SP', 1)
      result += CAPut(scanPV+'P'+StrCompress(4,/REMOVE_ALL)+'EP', 0)
      result += CAPut(scanPV+'R'+StrCompress(4,/REMOVE_ALL)+'PV', '')
      result += CAPut(scanPV+'P'+StrCompress(4,/REMOVE_ALL)+'PV', '')
    ENDELSE
    result += CAPut(scanPV+'NPTS', number)
  ENDFOR

  ;result = CAEndGroup()

  IF result NE 0 THEN !Null = Dialog_Message("Error setting one of the PVs")
  result = 0


  IF ~noNames THEN BEGIN
    nameWaveform = StrJoin(Reform(*self.namesArray))
    nameIndexArray = IntArr(N_Elements(*self.namesArray) + 1)
    i = 1
    FOREACH name, *self.namesArray DO BEGIN
      nameIndexArray[i] = nameIndexArray[i-1] + StrLen(name)
      i++
    ENDFOREACH
    nameIndexArray[-1] = StrLen(nameWaveform)
    
    result += CAPut(self.basePV + 'fileNames',Byte(nameWaveform))
    result += CAPut(self.basePV + 'fileIndices',nameIndexArray)
    
    IF result NE 0 THEN !Null = Dialog_Message("Error setting one of the PVs")
    result = 0
  ENDIF
  
END

PRO AS_SAXSExcelScans::GetProperty, NUMPOINTS=numPoints, CURRENTPOINT = currentPoint, PVS = PVs, SAMPLENAMES = sampleNames

  @as_scatterheader.macro

  IF Arg_Present(numPoints) THEN BEGIN
    numPoints = 1
    FOREACH p, (*self.scanArray).number DO numPoints *= Fix(p)
  ENDIF
  IF Arg_Present(currentPoint) THEN BEGIN
    result  = CAGet(self.basePV + 'scan1.CPT', level1)
    result += CAGet(self.basePV + 'scan2.CPT', level2)
    result += CAGet(self.basePV + 'scan3.CPT', level3)
    currentPoint = (level1+1)*(level2+1)*(level3+1)
  ENDIF
  IF Arg_Present(PVs) THEN BEGIN
    PVs = (*self.scanArray).positioner
  ENDIF
  IF Arg_Present(sampleNames) THEN BEGIN
    sampleNames = *self.namesArray
  ENDIF

END



PRO AS_SAXSExcelScans::Cleanup

  @as_scatterheader.macro

  IF Widget_Info(self.wScanBase, /VALID) THEN Widget_Control, self.wScanBase, /DESTROY
  IF Size(*self.scanArray, /type) EQ 8 THEN FOREACH ptr, (*self.scanArray).points DO Ptr_Free, ptr
  Ptr_Free, self.scanArray
  self->AS_TalkToExcel::Cleanup

END

PRO AS_SAXSExcelScans__Define

  void = {AS_SAXSExcelScans,  $
          INHERITS as_talktoexcel, $
          wScanBase : 0L, $
          scanArray : Ptr_New(), $
          namesArray : Ptr_New(), $
          basePV: '', $
          detector : '', $
          levels   : 0, $
          currentPos : StrArr(4)}

END










;    nameType = 0
;    
;    IF numLoop[0] NE numPositionNames[0] THEN nameType += 1
;    IF numLoop[1] NE numPositionNames[1] THEN nameType += 2
;    IF numLoop[2] NE numPositionNames[2] THEN nameType += 4
;        
;    CASE nameType OF
;      0:  BEGIN
;            self.ChangeSheet, 'SampleNames'
;            self.runMacro, 'UpdateSampleNamesMacro'
;            *self.namesArray = self.GetRange(self.GetRange('SampleNames', /ADDRESS) + ':' + self.GetRange('SampleNames', /ADDRESS, OFFSET=[numberPoints-1,0]))
;          END
;      1:  BEGIN
;            result = Dialog_Message("Only loop 1 filenames given. Scan will run with these filenames, and incremental suffix determined from position in outer loops.")
;            FOR loop = 1,2 DO BEGIN
;              loopPos = Where((*self.scanArray).loop EQ loop + 1)
;              FOREACH l, loopPos DO self.SetRange, (self.GetRange('Points', /ADDRESS, OFFSET = [0,-1]))[l] + ':' + (self.GetRange('Points', /ADDRESS, OFFSET=[numLoop[loop]-1,-1]))[l], Replicate('',numLoop[loop])
;            ENDFOR
;            self.ChangeSheet, 'SampleNames'
;            self.runMacro, 'UpdateSampleNamesMacro'
;            *self.namesArray = self.GetRange(self.GetRange('SampleNames', /ADDRESS) + ':' + self.GetRange('SampleNames', /ADDRESS, OFFSET=[numberPoints-1,0]))
;            FOR i = 0, numLoop[1]*numLoop[2] - 1 DO BEGIN
;              *self.namesArray[0+i*numLoop[0],(i+1)*numLoop[0] - 1] += '_' + String(i, '(I04)')
;            ENDFOR
;          END
;      2:  BEGIN
;            result = Dialog_Message("Only loop 1 and loop 3 filenames given. Scan will run with these filenames, and incremental suffix determined from position in loop 2.")
;            loopPos = Where((*self.scanArray).loop EQ 2)
;            FOREACH l, loopPos DO self.SetRange, (self.GetRange('Points', /ADDRESS, OFFSET = [0,-1]))[l] + ':' + (self.GetRange('Points', /ADDRESS, OFFSET=[numLoop[1]-1,-1]))[l], Replicate('',numLoop[1])
;            self.ChangeSheet, 'SampleNames'
;            self.runMacro, 'UpdateSampleNamesMacro'
;            *self.namesArray = self.GetRange(self.GetRange('SampleNames', /ADDRESS) + ':' + self.GetRange('SampleNames', /ADDRESS, OFFSET=[numberPoints-1,0]))
;            FOR i = 1, numLoop[1] DO BEGIN
;              (*self.namesArray)[0+(i-1)*numLoop[0]*numLoop[2],i*numLoop[0]*numLoop[2] - 1] += '_' + String(i, '(I04)')
;            ENDFOR
;          END
;      3:
;      4:  BEGIN
;            result = Dialog_Message("Only loop 1 and loop 2 filenames given. Scan will run with these filenames, and incremental suffix determined from position in loop 3.")
;            loopPos = Where((*self.scanArray).loop EQ 3)
;            FOREACH l, loopPos DO self.SetRange, (self.GetRange('Points', /ADDRESS, OFFSET = [0,-1]))[l] + ':' + (self.GetRange('Points', /ADDRESS, OFFSET=[numLoop[2]-1,-1]))[l], Replicate('',numLoop[2])
;            self.ChangeSheet, 'SampleNames'
;            self.runMacro, 'UpdateSampleNamesMacro'
;            *self.namesArray = self.GetRange(self.GetRange('SampleNames', /ADDRESS) + ':' + self.GetRange('SampleNames', /ADDRESS, OFFSET=[numberPoints-1,0]))
;            FOR i = 1, numLoop[2]  DO BEGIN
;              (*self.namesArray)[0+(i-1)*numLoop[0]*numLoop[1]:i*numLoop[0]*numLoop[1] - 1] += '_' + String(i, '(I04)')
;            ENDFOR
;            self.SetRange, self.GetRange('SampleNames', /ADDRESS) + ':' + self.GetRange('SampleNames', /ADDRESS, OFFSET=[numberPoints-1,0]), *self.namesArray
;          END
;      5:  
;      6:
;      7:  result = Dialog_Message("No filenames given. Scan will run with single filename, and incremental suffix.")
;    ENDCASE
;   
;  ENDIF   