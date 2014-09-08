FUNCTION as_areadetector::init, basePV, camPV, imagePV, _Ref_Extra=extra

  @as_scatterheader.macro
  
  test = N_Elements(basePV)+N_Elements(camPV)+N_Elements(imagePV)
  
  IF test NE 0 AND test NE 3 THEN BEGIN
  
    message, 'Define all parameters, or no parameters.'
    RETURN, 0
  
  ENDIF

  IF Size(basePV,  /TYPE) EQ 7 THEN self.basePV  = basePV   ELSE self.basePV  = ''
  IF Size(camPV,   /TYPE) EQ 7 THEN self.camPV   = camPV    ELSE self.camPV   = ''
  IF Size(imagePV, /TYPE) EQ 7 THEN self.imagePV = imagePV  ELSE self.imagePV = '' 
  
  result = self->IDLitComponent::Init(_extra=extra)
    
  self->RegisterProperty, 'basePV' , 4, NAME = 'Base PV'
  self->RegisterProperty, 'camPV'  , 4, NAME = 'Camera PV Fragment'
  self->RegisterProperty, 'imagePV', 4, NAME = 'Image PV Fragment'
  self->RegisterProperty, 'logfilePV', 4, NAME = 'Logfile PV Fragment'
  self->RegisterProperty, 'filePV',  4, NAME = 'File PV Fragment'
  self->RegisterProperty, 'FileName', 4, NAME = 'File Name'
  self->RegisterProperty, 'FilePath', 4, NAME = 'File Path'
  self->RegisterProperty, 'FileNumber', 2, NAME = 'File Number'
  self->RegisterProperty, 'FullFileName', 4, NAME = 'Fully Resolved File Name'
  self->RegisterProperty, 'FileTemplate', 4, NAME = 'File Name Template'
  self->RegisterProperty, 'ExposureTime', 3, NAME = 'Exposure Time'
  self->RegisterProperty, 'ExposurePeriod', 3, NAME = 'Exposure Period'
  self->RegisterProperty, 'Manufacturer', 4
  self->RegisterProperty, 'Model', 4
  self->RegisterProperty, 'ArraySizeX', 2, NAME = 'X Image Size'
  self->RegisterProperty, 'ArraySizeY', 2, NAME = 'Y Image Size'
  self->RegisterProperty, 'PixelSize', 3, NAME = 'Pixel Size'
  self->RegisterProperty, 'DetectorState', 4, NAME = 'Detector State'
  self->RegisterProperty, 'StatusMessage', 4, NAME = 'Status Message'
  self->RegisterProperty, 'ImageMode', 9, ENUMLIST = ['Single', 'Multiple', 'Continuous'], NAME = 'Image Mode' 
  self->RegisterProperty, 'TriggerMode', 9, ENUMLIST = ['Internal', 'Ext. Enable', 'Ext. Trigger', 'Mult. Trigger', 'Alignment', 'Gap Less'], NAME = 'Trigger Name'
  self->RegisterProperty, 'Shutter', 9, ENUMLIST = ['Closed','Open','Undefined']
  self->RegisterProperty, 'Control', 1, NAME = 'Control Detector From scatterBrain'
  self->RegisterProperty, 'SoftwareTrigger', 1, NAME = 'Trigger Acquisition From scatterBrain'
  self->RegisterProperty, 'AutoLoad', 1, NAME = 'Auto Load Images'
  self->RegisterProperty, 'logfilePath', 4, NAME = 'Logfile Path'
  self->RegisterProperty, 'logfilename', 4, NAME = 'Logfile name
   
  self->InitCam 
  self->InitImage
  self->InitLogfile
   
  RETURN, 1

END

PRO as_areadetector::InitCam

  @as_scatterheader.macro

  IF self.basePV NE '' THEN BEGIN
    IF Obj_Class(self.ADCamObj) EQ 'EPICS_AD_BASE' THEN Obj_Destroy, self.ADCamObj
    IF Obj_Class(self.ADFileObj) EQ 'EPICS_AD_FILE' THEN Obj_Destroy, self.ADFileObj
    IF Obj_Class(self.ADCamFileObj) EQ 'EPICS_AD_FILE' THEN Obj_Destroy, self.ADCamFileObj
    IF self.camPV NE '' THEN BEGIN
      self.ADCamObj = Obj_New('EPICS_AD_BASE', self.basePV + self.camPV)
      self.ADCamFileObj = Obj_New('EPICS_AD_FILE', self.basePV + self.camPV)
    ENDIF
    IF self.filePV NE '' THEN self.ADFileObj = Obj_New('EPICS_AD_FILE', self.basePV + self.filePV)
    
    IF Obj_Valid(self.ADCamObj) THEN self.ADCamObj->addProperty, 'PixelSize', 'PixelSize'
    IF Obj_Valid(self.ADCamObj) THEN self.ADCamObj->addProperty, 'ShutterControl', 'ShutterControl'
    IF Obj_Valid(self.ADCamObj) THEN self.ADCamObj->addProperty, 'ShutterStatus_RBV', 'ShutterStatus_RBV'
    IF Obj_Valid(self.ADFileObj) THEN self.ADFileObj->addProperty, 'FullFileName_RBV', 'FullFileName_RBV'
    IF Obj_Valid(self.ADCamFileObj) THEN self.ADCamFileObj->addProperty, 'FullFileName_RBV', 'FullFileName_RBV'
    
  ENDIF
  
END

PRO as_areadetector::InitImage

  @as_scatterheader.macro

  IF self.basePV NE '' AND self.imagePV NE '' THEN BEGIN
    IF Obj_Class(self.ADStdObj) EQ 'EPICS_ND_STD_ARRAYS' THEN Obj_Destroy, self.ADStdObj
    self.ADStdObj = Obj_New('EPICS_ND_STD_ARRAYS', self.basePV + self.imagePV)
  ENDIF

END

PRO as_areadetector::InitLogfile

  @as_scatterheader.macro

  IF self.basePV EQ '' OR self.logfilePV EQ '' THEN RETURN
  IF Obj_Class(self.ADLogFileObj) EQ 'EPICS_AD_FILE' THEN Obj_Destroy, self.ADLogFileObj
  self.ADLogFileObj = Obj_New('EPICS_AD_FILE', self.basePV + self.logfilePV)

END

FUNCTION as_areadetector::GetArray
  
  @as_scatterheader.macro
  
  CATCH, error
  IF error EQ 0 THEN BEGIN
    IF Obj_Valid(self.ADStdObj) THEN RETURN, self.ADStdObj->GetArray()
  ENDIF
  
  CATCH, /CANCEL
  
  RETURN, 0

END

FUNCTION as_areadetector::NewArray

  @as_scatterheader.macro

  CATCH, errorStatus
  
  IF errorStatus NE 0 THEN BEGIN
    CATCH, /CANCEL
    RETURN, !NULL
  ENDIF
  IF Obj_Valid(self.ADStdObj) THEN RETURN, self.ADStdObj->NewArray()

END

FUNCTION as_areadetector::GetParamMap

  @as_scatterheader.macro

  self.GetProperty, ARRAYSIZEX=XSize, ARRAYSIZEY = YSize, PIXELSIZE = PSize

  RETURN, {DETECTORDEF: self.name, XSIZE: Long(XSize), YSIZE: Long(YSize), PIXELSIZE: Double(PSize), BASEPV: self.basePV, CAMPV: self.camPV, IMAGEPV: self.imagePV, LOGFILEPV: self.logfilePV, FILEPV: self.filePV, CONTROL: self.control, SOFTWARETRIGGER: self.softwareTrigger, AUTOLOAD: self.autoLoad}

END

PRO as_areadetector::NewParams, params

  @as_scatterheader.macro

  newParamNames = Tag_Names(params)
  paramNames = Tag_Names({as_areadetector})
  
  FOR paramNum = 0, N_Elements(newParamNames) - 1 DO BEGIN
    paramPos = Where(paramNames EQ newParamNames[paramNum])
    IF paramPos GE 0 THEN self.(paramPos) = params.(paramNum) 
  ENDFOR

  self.name = params.detectordef

  self->InitCam
  self->InitImage
  self->InitLogfile

END

PRO as_areadetector::SetProperty, $
  BASEPV  = basePV,    $
  CAMPV   = camPV ,    $
  IMAGEPV = imagePV,   $
  LOGFILEPV = logfilePV,$
  FILEPV  = filePV,    $
  CONTROL = control,   $
  AUTOLOAD = autoLoad, $
  SOFTWARETRIGGER = softwareTrigger, $
  _Ref_extra = extra 

  @as_scatterheader.macro

  IF N_Elements(basePV)  EQ 1 THEN self.basePV  = basePV
  IF N_Elements(camPV)   EQ 1 THEN self.camPV   = camPV
  IF N_Elements(imagePV) EQ 1 THEN self.imagePV = imagePV
  IF N_Elements(logfilePV) EQ 1 THEN self.logfilePV = logfilePV
  IF N_Elements(filePV) EQ 1 THEN self.filePV = filePV
  IF N_Elements(control) EQ 1 THEN self.control = control
  IF N_Elements(autoLoad) EQ 1 THEN self.autoLoad = autoLoad
  IF N_Elements(softwareTrigger) EQ 1 THEN self.softwareTrigger = softwareTrigger
  IF N_Elements(basePV) + N_Elements(camPV) + N_Elements(imagePV) + N_Elements(filePV) + N_Elements(logfilePV) GT 0 THEN BEGIN
    self->InitCam 
    self->InitImage
    self->InitLogfile
  ENDIF

  self->IDLitComponent::SetProperty, _EXTRA = extra
  self->SetADProperty, _EXTRA = extra
  
END

PRO as_areadetector::SetADProperty, $
  FILENAME = fileName, $
  FILEPATH = filePath, $
  FILENUMBER = fileNumber, $
  FILETEMPLATE = fileTemplate, $
  IMAGEMODE = imageMode, $
  ACQUIRE = acquire, $
  EXPOSURETIME = acquireTime, $
  EXPOSUREPERIOD = acquirePeriod, $
  NUMIMAGES = numImages, $
  SHUTTER = shutter, $
  LOGFILEPATH = logfilepath, $
  LOGFILENAME = logfilename

  @as_scatterheader.macro

  camValid = Obj_Valid(self.ADCamObj)
  imageValid = Obj_Valid(self.ADStdObj)
  fileValid = Obj_Valid(self.ADFileObj)
  camFileValid = Obj_Valid(self.ADCamFileObj)
  logfilevalid = Obj_Valid(self.ADLogFileObj)

  IF self.control THEN BEGIN

    IF N_Elements(fileName) THEN BEGIN
      fileNameByte = BytArr(256)
      fileNameByte[0:N_Elements(Byte(fileName))-1] = Byte(fileName)
      IF camFileValid THEN self.ADCamFileObj->SetProperty, 'Filename', filenameByte ELSE $
      IF fileValid THEN self.ADFileObj->SetProperty, 'Filename', filenameByte
    ENDIF
    IF N_Elements(filePath) THEN BEGIN
      filePathByte = BytArr(256)
      filePathByte[0:N_Elements(Byte(filePath))-1] = Byte(filePath)
      IF camFileValid THEN self.ADCamFileObj->SetProperty, 'FilePath', filePathByte ELSE $
      IF fileValid THEN self.ADFileObj->SetProperty, 'FilePath', filePathByte
      
    ENDIF
    IF N_Elements(fileNumber) THEN BEGIN
      IF camFileValid THEN self.ADCamFileObj->SetProperty, 'FileNumber', long(fileNumber) ELSE $
      IF fileValid THEN self.ADFileObj->SetProperty, 'FileNumber', long(fileNumber)
    ENDIF
    IF N_Elements(fileTemplate) THEN BEGIN
      fileTemplateByte = BytArr(256)
      fileTemplateByte[0:N_Elements(Byte(fileTemplate))-1] = Byte(fileTemplate)
      IF camFileValid THEN self.ADCamFileObj->SetProperty, 'FileTemplate', fileTemplateByte ELSE $
      IF fileValid THEN self.ADFileObj->SetProperty, 'FileTemplate', fileTemplateByte
    ENDIF
    IF N_Elements(ImageMode) THEN BEGIN
      IF camValid THEN self.ADCamObj->SetProperty, 'ImageMode', ImageMode
    ENDIF
    IF N_Elements(TriggerMode) THEN BEGIN
      IF camValid THEN self.ADCamObj->SetProperty, 'TriggerMode', TriggerMode
    ENDIF
    IF N_Elements(acquireTime) THEN BEGIN
      IF camValid THEN self.ADCamObj->SetProperty, 'AcquireTime', acquireTime
    ENDIF
    IF N_Elements(acquirePeriod) THEN BEGIN
      IF camValid THEN self.ADCamObj->SetProperty, 'AcquirePeriod', acquirePeriod
    ENDIF
    IF N_Elements(numImages) THEN BEGIN
      IF camValid THEN self.ADCamObj->SetProperty, 'NumImages', numImages
    ENDIF
    IF N_Elements(shutter) THEN BEGIN
      IF shutter EQ 2 THEN RETURN
      IF camValid THEN self.ADCamObj->SetProperty, 'ShutterControl', shutter
    ENDIF
    IF N_Elements(logfilepath) THEN BEGIN
      logfilePathByte = BytArr(256)
      logfilePathByte[0:N_Elements(Byte(logfilepath))-1] = Byte(logfilepath)
      IF logfileValid THEN self.ADLogFileObj->SetProperty, 'FilePath',logfilepathByte
    ENDIF
    IF N_Elements(logfilename) THEN BEGIN
      logfileNameByte = BytArr(256)
      logfileNameByte[0:N_Elements(Byte(logfilename))-1] = Byte(logfilename)
      IF logfileValid THEN self.ADLogFileObj->SetProperty, 'FileName',logfilenameByte
    ENDIF
  ENDIF
  
  IF self.softwareTrigger THEN BEGIN
    IF N_Elements(Acquire) THEN BEGIN
      IF camValid THEN self.ADCamObj->SetProperty, 'Acquire', Acquire
    ENDIF
  ENDIF

END

PRO as_areadetector::GetProperty, $
  BASEPV  = basePV,    $
  CAMPV   = camPV ,    $
  IMAGEPV = imagePV,   $
  LOGFILEPV = logfilePV, $
  FILEPV  = filePV,    $
  CONTROL = control,   $
  SOFTWARETRIGGER = softwareTrigger,   $
  AUTOLOAD= autoLoad,  $
  _Ref_extra = extra 

  @as_scatterheader.macro

  IF Arg_Present(basePV)   THEN basePV  = self.basePV
  IF Arg_Present(camPV)    THEN camPV   = self.camPV
  IF Arg_Present(imagePV)  THEN imagePV = self.imagePV
  IF Arg_Present(logFilePV) THEN logFilePV = self.logFilePV
  IF Arg_Present(filePV)  THEN filePV = self.filePV
  IF Arg_Present(control) THEN control = self.control
  IF Arg_Present(autoLoad) THEN autoLoad = self.autoLoad
  IF Arg_Present(softwareTrigger) THEN softwareTrigger = self.softwareTrigger
  
  self->IDLitComponent::GetProperty, _EXTRA = extra
  self->GetADProperty, _EXTRA = extra  

END

PRO as_areadetector::GetADProperty, $
  FILENAME = fileName, $
  FILEPATH = filePath, $
  FULLFILENAME = fullFileName, $
  FILENUMBER = fileNumber, $
  FILETEMPLATE = fileTemplate, $
  MANUFACTURER = manufacturer, $
  MODEL = model, $
  ARRAYSIZEX = arraySizeX, $
  ARRAYSIZEY = arraySizeY, $
  PIXELSIZE = pixelSize, $
  DETECTORSTATE = detectorState, $
  STATUSMESSAGE = statusMessage, $
  IMAGEMODE = imageMode, $
  TRIGGERMODE = triggerMode, $
  EXPOSURETIME = acquireTime, $
  EXPOSUREPERIOD = acquirePeriod, $
  NUMIMAGES = numImages, $
  GETCAMERAFILE = getCameraFile, $
  TRIGGERSTRING = triggerString, $
  SHUTTER = shutter, $
  LOGFILEPATH = logfilepath, $
  LOGFILENAME = logfilename

  @as_scatterheader.macro

  camValid = Obj_Valid(self.ADCamObj)
  imageValid = Obj_Valid(self.ADStdObj)
  camFileValid = Obj_Valid(self.ADCamFileObj)
  fileValid = 0;Obj_Valid(self.ADFileObj) AND ~KeyWord_Set(GETCAMERAFILE)
  fileValidTemp=Obj_Valid(self.ADFileObj) AND ~KeyWord_Set(GETCAMERAFILE)
  logfilevalid = Obj_Valid(self.ADLogfileObj)

  IF Arg_Present(logfilepath) THEN BEGIN
    IF logfilevalid THEN logfilepath = String(self.ADLogFileObj->GetProperty('FilePath_RBV')) ELSE logfilepath = ''
  ENDIF
  IF Arg_Present(logfilename) THEN BEGIN
    IF logfilevalid THEN logfilename = String(self.ADLogFileObj->GetProperty('FileName_RBV')) ELSE logfilename = ''
  ENDIF
  IF Arg_Present(fileName) THEN BEGIN
    IF fileValid THEN fileName = String(self.ADFileObj->GetProperty('FileName_RBV')) ELSE $
    IF camFileValid THEN fileName = String(self.ADCamFileObj->GetProperty('FileName_RBV')) ELSE fileName = ''
  ENDIF
  IF Arg_Present(filePath) THEN BEGIN
    IF fileValid THEN filePath = String(self.ADFileObj->GetProperty('FilePath_RBV')) ELSE $
    IF camFileValid THEN filePath = String(self.ADCamFileObj->GetProperty('FilePath_RBV')) ELSE filePath = ''
  ENDIF
  IF Arg_Present(fullFileName) THEN BEGIN
    IF fileValidTemp THEN fullFileName = String(self.ADFileObj->GetProperty('FullFileName_RBV')) ELSE $
    IF camFileValid THEN fullFileName = String(self.ADCamFileObj->GetProperty('FullFileName_RBV')) ELSE fullFileName = ''
  ENDIF
  IF Arg_Present(fileNumber) THEN BEGIN
    IF fileValid THEN fileNumber = String(self.ADFileObj->GetProperty('FileNumber_RBV')) ELSE $
    IF camFileValid THEN fileNumber = String(self.ADCamFileObj->GetProperty('FileNumber_RBV')) ELSE fileNumber = 1
  ENDIF
  IF Arg_Present(fileTemplate) THEN BEGIN
    IF fileValid THEN fileTemplate = String(self.ADFileObj->GetProperty('FileTemplate_RBV')) ELSE $
    IF camFileValid THEN fileTemplate = String(self.ADCamFileObj->GetProperty('FileTemplate_RBV')) ELSE fileTemplate = ''
  ENDIF
  IF Arg_Present(manufacturer) THEN BEGIN
    IF camValid THEN manufacturer = String(self.ADCamObj->GetProperty('Manufacturer_RBV')) ELSE manufacturer = ''
  ENDIF
  IF Arg_Present(model) THEN BEGIN
    IF camValid THEN model = String(self.ADCamObj->GetProperty('Model_RBV')) ELSE model = ''
  ENDIF
  IF Arg_Present(ArraySizeX) THEN BEGIN
    IF camValid THEN ArraySizeX = self.ADCamObj->GetProperty('ArraySizeX_RBV') ELSE ArraySizeX = ''
  ENDIF
  IF Arg_Present(ArraySizeY) THEN BEGIN
    IF camValid THEN ArraySizeY = self.ADCamObj->GetProperty('ArraySizeY_RBV') ELSE ArraySizeY = ''
  ENDIF
  IF Arg_Present(pixelSize) THEN BEGIN
    IF camValid THEN pixelSize = self.ADCamObj->GetProperty('PixelSize') ELSE PixelSize = ''
  ENDIF
  IF Arg_Present(DetectorState) THEN BEGIN
    IF camValid THEN DetectorState = String(self.ADCamObj->GetProperty('DetectorState_RBV')) ELSE DetectorState = ''
  ENDIF
  IF Arg_Present(StatusMessage) THEN BEGIN
    IF camValid THEN StatusMessage = String(self.ADCamObj->GetProperty('StatusMessage_RBV')) ELSE StatusMessage = ''
  ENDIF
  IF Arg_Present(ImageMode) THEN BEGIN
    IF camValid THEN ImageMode = String(self.ADCamObj->GetProperty('ImageMode_RBV', STRING=0)) ELSE ImageMode = ''
  ENDIF
  IF Arg_Present(TriggerMode) THEN BEGIN
    IF camValid THEN BEGIN
      TriggerMode = String(self.ADCamObj->GetProperty('TriggerMode_RBV', STRING=0))
      IF KeyWord_Set(triggerString) THEN TriggerMode = (['Internal', 'Ext. Enable', 'Ext. Trigger', 'Mult. Trigger', 'Alignment', 'Gap Less'])[TriggerMode] 
    ENDIF ELSE TriggerMode = ''
  ENDIF
  IF Arg_Present(acquireTime) THEN BEGIN
    IF camValid THEN acquireTime = self.ADCamObj->GetProperty('AcquireTime') ELSE acquireTime = 0
  ENDIF
  IF Arg_Present(acquirePeriod) THEN BEGIN
    IF camValid THEN acquirePeriod = self.ADCamObj->GetProperty('AcquirePeriod') ELSE acquirePeriod = 0 
  ENDIF
  IF Arg_Present(numImages) THEN BEGIN
    IF camValid THEN numImages = self.ADCamObj->GetProperty('NumImages') ELSE numImages = 0 
  ENDIF
  IF Arg_Present(shutter) THEN BEGIN
    IF camValid THEN BEGIN
      shutterString = self.ADCamObj->GetProperty('ShutterStatus_RBV') 
      CASE shutterString OF
        'Closed': shutter = 0
        'Open'  : shutter = 1
        ELSE    : shutter = 2
      ENDCASE
    ENDIF ELSE shutter = 2
  ENDIF
END

PRO as_areadetector__define

   
  void = { as_areadetector,                  $
           INHERITS IDLitComponent        ,  $
           basePV     : ''                  ,  $
           camPV      : ''                  ,  $
           imagePV    : ''                  ,  $
           logfilePV : ''                 ,$
           filePV    : ''                  ,  $
           control   : 0                   ,  $
           softwareTrigger   : 0                   ,  $
           autoLoad  : 0                   ,  $
           ADCamObj : Obj_New()           ,  $
           ADCamFileObj : Obj_New()           ,  $
           ADLogFileObj : Obj_New(), $
           ADFileObj: Obj_New()           ,  $
           ADStdObj : Obj_New()              $
         }

END