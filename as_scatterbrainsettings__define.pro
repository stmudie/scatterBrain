FUNCTION as_scatterBrainSettings::init

  @as_scatterheader.macro

  self.settingsPath = self.getSettingsPath()
  
  void = {CAMERAPVS, cameraPVs : '', wavelengthPV : '', lengthPV : '', WAXSAnglePV : ''}
  
  self.cameraPVs = Ptr_New({CAMERAPVS})
  self.attCameraPVs = Ptr_New({ATTRIBUTESCAMERAPVS, cameraPVS: ['wavelengthPV','lengthPV','WAXSAnglePV']}) 
  
  
  void = {DETECTOR, Detector : '', basePV : '', camPV : '', imagePV : '', filePV : '', logPV : '', localFilePath : '', remoteFilePath : '', I0PV : '', IBSPV : '', ITPV : '', LUTPV : '', NORMPV : '', QVECTORPV : ''}

  self.detector = Ptr_New({DETECTOR})
  self.attDetector = Ptr_New({ATTRIBUTESDETECTOR, Detector: ['basePV', 'camPV', 'imagePV', 'filePV', 'logPV', 'localFilePath', 'remoteFilePath', 'I0PV', 'IBSPV', 'ITPV', 'LUTPV', 'NORMPV', 'QVECTORPV']})
  
  void = {RECENTFILE, recentFile : ''}
  
  self.recentFile = Ptr_New(replicate({RECENTFILE},8))
  self.attRecentFile = Ptr_New({ATTRIBUTESRECENTFILE, recentFile :['']})
  
  void = {GENERAL, generalSettings : '', zingerThresh : '', binSize : '', startingdirectory1 : '', startingDirectory2 : '', autoCheckUpdates : '', errorBars : ''}
  self.general = Ptr_New({GENERAL})
  self.attGeneral = Ptr_New({ATTRIBUTESGENERAL, generalSettings : ['zingerThresh', 'binSize', 'startingdirectory1', 'startingDirectory2', 'autoCheckUpdates', 'errorBars']})
    
  RETURN, self.as_xmlparamfile::init()

END

FUNCTION as_scatterBrainSettings::getSettingsPath

  @as_scatterheader.macro

  ; Increment if author_readme_text is changed
  author_readme_version = 1

  author_readme_text = $

     ['This is the user configuration directory for', $
      'IDL based products from the Australian Synchrotron:', $
      '', $
      ' Australian Synchrotron', $
      ' 800 Blackburn Road', $
      ' Clayton, VIC, 3168', $
      ' Australia.']

  ; Increment if app_readme_text is changed
  app_readme_version = 1 

  app_readme_text = $

     ['This is the configuration directory for scatterBrain.', $
      ' Data acquisition and reduction software for the ', $
      'SAXS/WAXS beamline at the Australian Synchrotron.', $
      '', $
      'It is safe to remove this directory, as it', $
      'will be recreated on demand. However, note that all', $
      'settings will revert to their default values.']

  settings_dir = APP_USER_DIR('australiansynchrotron', 'Australian Synchrotron', $
      'scatterBrain', 'scatterBrain SAXS/WAXS data acquisition and reduction.', $
      app_readme_text, app_readme_version, $
      AUTHOR_README_TEXT=author_readme_text, $
      AUTHOR_README_VERSION=author_readme_version, $
      RESTRICT_APPVERSION='1.0', /RESTRICT_FAMILY)

   RETURN, settings_dir + Path_Sep()

END

PRO as_scatterBrainSettings::ParseFile

  @as_scatterheader.macro

  IF ~File_Test(self.settingsPath + 'scatterBrainSettings.xml') THEN BEGIN
    self.savefile
  ENDIF

  self->AS_XMLPARAMFILE::ParseFile, self.settingsPath + 'scatterBrainSettings.xml', STRUCT = *self.detector, ATTSTRUCT=*self.attDetector
  *self.detector = *self.structArray
  self->AS_XMLPARAMFILE::ParseFile, self.settingsPath + 'scatterBrainSettings.xml', STRUCT = *self.recentFile, ATTSTRUCT=*self.attRecentFile
  *self.recentFile = *self.structArray
  self->AS_XMLPARAMFILE::ParseFile, self.settingsPath + 'scatterBrainSettings.xml', STRUCT = *self.cameraPVs, ATTSTRUCT=*self.attCameraPVs
  *self.cameraPVs = *self.structArray
  self->AS_XMLPARAMFILE::ParseFile, self.settingsPath + 'scatterBrainSettings.xml', STRUCT = *self.general, ATTSTRUCT=*self.attGeneral
  *self.general = *self.structArray
  
  ;Set some defaults if values weren't present in file
  
  ;General Settings
  general = *self.general
  IF general.zingerThresh EQ '' THEN self.SetProperty, zingerThresh = 3.0 
  IF general.binSize EQ '' THEN self.SetProperty, binSize = 2
  IF general.startingdirectory1 EQ '' THEN BEGIN
    CD, CURRENT = current
    self.SetProperty, startingdirectory1 = current
  ENDIF
  IF general.startingDirectory2 EQ '' THEN BEGIN
    CD, CURRENT = current
    self.SetProperty, startingDirectory2 = current
  ENDIF
  IF general.autoCheckUpdates EQ '' THEN self.SetProperty, autoCheckUpdates = 1
  
  ;Detector Settings
  detector = *self.detector
  IF (detector.basePV)[0] EQ '' THEN self.SetProperty, basePV = '13PIL1:'
  IF (detector.camPV)[0] EQ '' THEN camPV = 'cam1:'
  IF (detector.imagePV)[0] EQ '' THEN imagePV = 'image1:'
  IF (detector.I0PV)[0] EQ '' THEN I0PV = 'I0'
  IF (detector.IBSPV)[0] EQ '' THEN IBSPV = 'IBS'
  IF (detector.ITPV)[0] EQ '' THEN ITPV = 'IT'
    
END

PRO as_scatterBrainSettings::SaveFile

  @as_scatterheader.macro
  
  self->New, 'scatterBrainSettings', FILEVERSION = '1'
  self->NewFromStruct, 'scatterBrainSettings', STRUCT = *self.general, ATTSTRUCT = *self.attGeneral, APPENDTO = 'base'
  self->NewFromStruct, STRUCT = *self.cameraPVs, ATTSTRUCT = *self.attCameraPVs, APPENDTO = 'base'
  detectorList = self->AddElement('base', 'DETECTORLIST','')
  self->NewFromStruct, 'scatterBrainSettings', STRUCT = *self.detector, ATTSTRUCT = *self.attDetector, APPENDTO = detectorList
  recentFileList = self->AddElement('base', 'RECENTFILELIST','')
  self->NewFromStruct, 'scatterBrainSettings', STRUCT = *self.recentFile, ATTSTRUCT = *self.attRecentFile, APPENDTO = recentFileList
  self->Save, FILENAME = self.settingsPath + 'scatterBrainSettings.xml'
  
END

PRO as_scatterBrainSettings::GetProperty, $
  DETECTOR = detector, $
  BASEPV = basePV, $
  CAMPV = camPV, $
  IMAGEPV = imagePV, $
  FILEPV = filePV, $
  LOGPV = logPV, $
  LOCALFILEPATH = localFilePath, $
  REMOTEFILEPATH = remoteFilePath, $
  LUTPV = LUTPV, $
  NORMPV = normPV, $
  QVECTORPV = QVectorPV, $
  I0PV = I0PV, $
  ITPV = ITPV, $
  IBSPV = IBSPV, $
  WAVELENGTHPV = wavelengthPV, $
  LENGTHPV = lengthPV, $
  WAXSANGLEPV = waxsAnglePV, $
  ZINGERTHRESH = zingerThresh, $
  BINSIZE = binSize, $
  RECENTFILE = recentFile, $
  SETTINGSPATH = settingsPath, $
  startingdirectory1 = startingdirectory1, $
  STARTINGDIRECTORY2 = startingDirectory2, $
  AUTOCHECKUPDATES = autoCheckUpdates, $
  ERRORBARS = errorBars

  @as_scatterheader.macro

  IF Arg_Present(detector) THEN detector = (*self.detector).detector
  IF Arg_Present(basePV) THEN basePV = (*self.detector).basePV
  IF Arg_Present(camPV) THEN camPV = (*self.detector).camPV
  IF Arg_Present(imagePV) THEN imagePV = (*self.detector).imagePV
  IF Arg_Present(filePV) THEN filePV = (*self.detector).filePV
  IF Arg_Present(logPV) THEN logPV = (*self.detector).logPV
  IF Arg_Present(localFilePath) THEN localFilePath = (*self.detector).localFilePath
  IF Arg_Present(remoteFilePath) THEN remoteFilePath = (*self.detector).remoteFilePath
  IF Arg_Present(LUTPV) THEN LUTPV = (*self.detector).LUTPV
  IF Arg_Present(NORMPV) THEN NORMPV = (*self.detector).NORMPV
  IF Arg_Present(QVECTORPV) THEN QVECTORPV = (*self.detector).QVECTORPV
  IF Arg_Present(I0PV) THEN I0PV = (*self.detector).I0PV
  IF Arg_Present(ITPV) THEN ITPV = (*self.detector).ITPV
  IF Arg_Present(IBSPV) THEN IBSPV = (*self.detector).IBSPV
  IF Arg_Present(wavelengthPV) THEN wavelengthPV = (*self.cameraPVs).wavelengthPV
  IF Arg_Present(lengthPV) THEN lengthPV = (*self.cameraPVs).lengthPV
  IF Arg_Present(waxsAnglePV) THEN waxsAnglePV = (*self.cameraPVs).waxsAnglePV
  IF Arg_Present(zingerThresh) AND Ptr_Valid(self.general) THEN zingerThresh = (*self.general).zingerThresh
  IF Arg_Present(binSize) AND Ptr_Valid(self.general) THEN binSize = (*self.general).binSize
  IF Arg_Present(recentFile) THEN recentFile = (*self.recentFile).recentFile
  IF Arg_Present(settingsPath) THEN settingsPath = self.settingsPath
  IF Arg_Present(startingdirectory1) THEN startingdirectory1 = (*self.general).startingdirectory1
  IF Arg_Present(startingDirectory2) THEN startingDirectory2 = (*self.general).startingDirectory2
  IF Arg_Present(autoCheckUpdates) THEN autoCheckUpdates = Fix((*self.general).autoCheckUpdates)
  IF Arg_Present(errorBars) THEN errorBars = Fix((*self.general).errorBars)

END

PRO as_scatterBrainSettings::SetProperty, $
  DETNO = detNo, $
  DETECTOR = detector, $
  BASEPV = basePV, $
  CAMPV = camPV, $
  IMAGEPV = imagePV, $
  FILEPV = filePV, $
  LOGPV = logPV, $
  LOCALFILEPATH = localFilePath, $
  REMOTEFILEPATH = remoteFilePath, $
  LUTPV = LUTPV, $
  NORMPV = normPV, $
  QVECTORPV = QVectorPV, $
  I0PV = I0PV, $
  ITPV = ITPV, $
  IBSPV = IBSPV, $
  WAVELENGTHPV = wavelengthPV, $
  LENGTHPV = lengthPV, $
  WAXSANGLEPV = waxsAnglePV, $
  ZINGERTHRESH = zingerThresh, $
  BINSIZE = binSize, $
  RECENTFILE = recentFile, $
  startingdirectory1 = startingdirectory1, $
  STARTINGDIRECTORY2 = startingDirectory2, $
  AUTOCHECKUPDATES = autoCheckUpdates, $
  NOSAVE = noSave, $
  ERRORBARS = errorBars

  @as_scatterheader.macro

  IF N_Elements(detector) + $
     N_Elements(basePV) + $
     N_Elements(camPV) + $
     N_Elements(imagePV) + $
     N_Elements(filePV) + $
     N_Elements(logPV) + $
     N_Elements(localFilePath) + $
     N_Elements(remoteFilePath) + $
     N_Elements(normPV) + $
     N_Elements(LUTPV) + $
     N_Elements(QVectorPV) + $
     N_Elements(I0PV) + $
     N_Elements(ITPV) + $
     N_Elements(IBSPV) GT 0 THEN BEGIN
    
    IF N_Elements(detNo) EQ 0 THEN detNo = 0
    detNo = detNo < N_Elements(*self.detector)
    IF detNo LT 0 THEN detNo = N_Elements(*self.detector)
    
    IF detNo EQ N_Elements(*self.detector) THEN BEGIN
      tempDetector = *self.detector
      *self.detector = Replicate({DETECTOR},detNo + 1)
      (*self.detector)[0:N_Elements(*self.detector)-1] = tempDetector
    ENDIF
    
    IF N_Elements(detector) THEN ((*self.detector)[detNo].detector) = detector
    IF N_Elements(basePV) THEN ((*self.detector)[detNo].basePV) = basePV
    IF N_Elements(camPV) THEN ((*self.detector)[detNo].camPV) = camPV
    IF N_Elements(imagePV) THEN ((*self.detector)[detNo].imagePV) = imagePV
    IF N_Elements(filePV) THEN ((*self.detector)[detNo].filePV) = filePV
    IF N_Elements(logPV) THEN ((*self.detector)[detNo].logPV) = logPV
    IF N_Elements(localFilePath) THEN ((*self.detector)[detNo].localFilePath) = localFilePath 
    IF N_Elements(remoteFilePath) THEN ((*self.detector)[detNo].remoteFilePath) = localFilePath
    IF N_Elements(LUTPV) THEN ((*self.detector)[detNo].LUTPV) = LUTPV
    IF N_Elements(NORMPV) THEN ((*self.detector)[detNo].NORMPV) = NORMPV
    IF N_Elements(NORMPV) THEN ((*self.detector)[detNo].QVECTORPV) = QVECTORPV
    IF N_Elements(I0PV) THEN ((*self.detector)[detNo].I0PV) = I0PV
    IF N_Elements(ITPV) THEN ((*self.detector)[detNo].ITPV) = ITPV
    IF N_Elements(IBSPV) THEN ((*self.detector)[detNo].IBSPV) = IBSPV
      
  ENDIF
  
  IF N_Elements(wavelengthPV) THEN (*self.cameraPVs).wavelengthPV = wavelengthPV
  IF N_Elements(lengthPV) THEN (*self.cameraPVs).lengthPV = lengthPV
  IF N_Elements(waxsAnglePV) THEN (*self.cameraPVs).waxsAnglePV = waxsAnglePV
  IF N_Elements(zingerThresh) AND Ptr_Valid(self.general) THEN (*self.general).zingerThresh = zingerThresh
  IF N_Elements(binSize) AND Ptr_Valid(self.general) THEN (*self.general).binSize = binSize  
  IF N_Elements(recentFile) GT 0 THEN BEGIN
    recentFileList = (*self.recentFile)[*].recentFile
    alreadyPresent = Where(recentFileList EQ recentFile,/NULL)
    IF alreadyPresent NE !NULL THEN recentFileList[alreadyPresent] = ''
    recentFileList = recentFileList[Where(recentFileList NE '')]
    recentFileList = [recentFile,recentFileList]
    numRecentFiles = N_Elements(recentFileList) < 8
    newRecentFileList = StrArr(8)
    newRecentFileList[0:numRecentFiles-1] = recentFileList[0:numRecentFiles-1]
    (*self.recentFile)[*].recentFile = newRecentFileList
  ENDIF
  IF ISA(startingdirectory1, 'STRING') AND Ptr_Valid(self.general) THEN (*self.general).startingdirectory1 = startingdirectory1
  IF ISA(startingDirectory2, 'STRING') AND Ptr_Valid(self.general) THEN (*self.general).startingDirectory2 = startingDirectory2
  IF Ptr_Valid(self.general) THEN (*self.general).autoCheckUpdates = KeyWord_Set(autoCheckUpdates)
  IF N_Elements(errorBars) AND Ptr_Valid(self.general) THEN (*self.general).errorBars = String(errorBars)
  
  IF ~KeyWord_Set(NOSAVE) THEN self.SaveFile
  
END


PRO as_scatterBrainSettings__define

  void = {as_scatterBrainSettings, $
          INHERITS as_xmlparamfile, $
          settingsPath: '', $
          detector : Ptr_New(), $
          attDetector : Ptr_New(),$
          cameraPVs : Ptr_New(), $
          attCameraPVs : Ptr_New(), $
          recentFile : Ptr_New(), $
          attRecentFile :Ptr_New(), $
          general : Ptr_New(), $
          attGeneral : Ptr_New() }

END