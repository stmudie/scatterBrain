FUNCTION as_proteinexcelscans::GetScanParams, PROTEIN=protein

  IF self.detectorBackup EQ '' THEN self.detectorBackup = self.detector

  scanType = self.GetRange('ScanType')
 
  IF StrUpCase(scanType) EQ 'PROTEIN SCAN' THEN protein = 1
 
  IF KeyWord_Set(protein) THEN BEGIN

    self.detector = 'SR13ID01SYR01:FULL_SEL_SQ.VAL'

    errorCode = Fix(self.GetRange('ErrorCode'))
    
    binArr = binary(errorCode)
    
    cancelScan = 0
    
    FOREACH bin, (Reverse(binArr))[0:4], key DO BEGIN
    
      IF bin EQ 1 THEN BEGIN
    
        CASE key OF
          0: BEGIN
               result = Dialog_Message('Sample names entered without coordinate, scan not initialised.')
               cancelScan = 1
             END
          1: result = Dialog_Message('Coordinates entered without corresponding sample name - these coordinates will be ignored.')
          2: BEGIN
               result = Dialog_Message('Identical filenames entered. Scan not initialised.')
               cancelScan = 1
             END 
          3: BEGIN
               result = Dialog_Message('Identical coordinates entered. Scan not initialised.')
               cancelScan = 1
             END 
          4:
        ENDCASE
      ENDIF
    
    ENDFOREACH
    
    IF cancelScan EQ 1 THEN RETURN, -1
    

;*** Set some PV's up specific to protein scanning.
    scanPV = self.basePV + 'scan1.'
    result = 0
    result += CAPut(scanPV+'BSPV','SR13ID01SYR01:SCAN_RECORD_MESSAGE.VAL')
    result += CAPut(scanPV+'BSCD',0)
    result += CAPut(scanPV+'ASPV','SR13ID01SYR01:SCAN_RECORD_MESSAGE.VAL')
    result += CAPut(scanPV+'ASCD',1)
    result += CAPut(scanPV+'D01PV','SR13ID01SYR01:FULL_SEL_SQ.VAL')
    result += CAPut(scanPV+'PDLY',2)
    result += CAPut(scanPV+'DDLY',5)
    IF result NE 0 THEN result = Dialog_Message("Error setting some PV's. Continuing anyway.")
;***
 
    *self.scanArray = Replicate({scanStruct},3)
    
    ((*self.scanArray)[0:2].Number) = self.GetRange('Number')
    ((*self.scanArray)[0:2].mode) = 'Table'
    ((*self.scanArray)[0:2].loop) = 1
    ((*self.scanArray)[0].Positioner) = 'SR13ID01SYR01:SMPL_RAW_COORD'
    ((*self.scanArray)[1].Positioner) = 'SR13ID01SYR01:WASH_TYPE'
    ((*self.scanArray)[2].Positioner) = 'SR13ID01HU02IOC04:SMPL_TYPE'
    
    sampleIndices = Where(self.GetRange('SampleNames') NE '')
    
    *self.namesArray = (self.GetRange('SampleNames'))[sampleIndices];[0:(*self.scanArray)[0].Number-1]
    coordinates = (self.GetRange('Coordinates'))[sampleIndices];[0:(*self.scanArray)[0].Number-1]
    plateNumbers = (self.GetRange('PlateNumber'))[sampleIndices];[0:(*self.scanArray)[0].Number-1]
    
;*** Replace spaces with underscores in sample names
    FOREACH name, *self.namesArray, key DO BEGIN
      (*self.namesArray)[key] = StrJoin(StrSplit(name,' ',/EXTRACT),'_')
    ENDFOREACH
;***    
    
    
    self.ChangeSheet, 'Enumerators'
    plateEnum = (self.GetRange('PlateEnum'))

    plateVal = !Null

    FOREACH plateStr, plateNumbers, key DO BEGIN
      plateVal = [plateVal,Where(plateStr EQ plateEnum)]
    ENDFOREACH
    ((*self.scanArray)[0].points) = Ptr_New(IntArr((*self.scanArray)[0].Number))
  
    FOREACH coord, coordinates, key DO BEGIN
      letter = StrUpCase(StrMid(coord,0,1))
      number = StrUpCase(StrMid(coord,1,2))
      CASE letter OF
        'A' : letterIndex = 0
        'B' : letterIndex = 1
        'C' : letterIndex = 2
        'D' : letterIndex = 3
        'E' : letterIndex = 4
        'F' : letterIndex = 5
        'G' : letterIndex = 6
        'H' : letterIndex = 7
        ELSE:
      ENDCASE
      (*((*self.scanArray)[0].points))[key] = letterIndex*12 + number + 96*plateVal[key]
    ENDFOREACH

    washEnum = (self.GetRange('WashEnum'))
    self.ChangeSheet, 'ScanParams'
    wash = (self.GetRange('Wash'))[sampleIndices];[0:(*self.scanArray)[0].Number-1]
    washVal = !Null
    FOREACH washStr, wash, key DO BEGIN
      washVal = [washVal,Where(StrUpCase(washStr) EQ StrUpCase(washEnum))]
    ENDFOREACH
    ((*self.scanArray)[1].points) = Ptr_New(washVal)
    
    self.ChangeSheet, 'Enumerators'
    
    sampleTypeEnum = (self.GetRange('sampleTypeEnum'))
    self.ChangeSheet, 'ScanParams'
    sampleType = (self.GetRange('SampleType'))[sampleIndices];[0:(*self.scanArray)[0].Number-1]
    sampleTypeVal = !Null
    FOREACH sampleTypeStr, sampleType, key DO BEGIN
      sampleTypeVal = [sampleTypeVal,Where(StrUpCase(sampleTypeStr) EQ StrUpCase(sampleTypeEnum))]
    ENDFOREACH
    ((*self.scanArray)[2].points) = Ptr_New(sampleTypeVal)
    
    self.copy, ['B12:E203','ScanParams'], ['B12:E203','UploadedScanParams']  

    self.ChangeSheet, 'ScanParams'
         
    RETURN, 8
  ENDIF ELSE BEGIN
    self.detector = self.detectorBackup
    RETURN, self.as_saxsexcelscans::GetScanParams()
  ENDELSE

END

PRO as_proteinexcelscans__define

  void = {as_proteinexcelscans, $
          INHERITS as_saxsexcelscans, $
          detectorBackUp : ''}

END