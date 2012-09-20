FUNCTION as_upgradeversion_callback, StatusInfo, ProgressInfo, object

  @as_scatterheader.macro

  event = {DOWNLOADPROGRESS, downloaded : progressinfo[2], total : progressinfo[1]} 
  
  object.notify, event
  
  RETURN, 1

END

FUNCTION as_upgradeversion::init, programName, programURL, programInstallDir, NOTIFYOBJ = notifyObj

  @as_scatterheader.macro

  IF Keyword_Set(notifyObj) THEN $
    IF TypeName(notifyObj[0]) EQ 'NOTIFY' $
      THEN self.notify = List(notifyObj,/EXTRACT)
      
  self.name = programName
  self.url = programURL
  self.dir = programInstallDir

  self.urlObj = IDLnetURL(CALLBACK_FUNCTION = 'as_upgradeversion_callback', CALLBACK_DATA = self)
  
  RETURN, 1

END

PRO as_upgradeversion::notify, event

  @as_scatterheader.macro

  FOREACH notify, self.notify DO IF Obj_Valid(notify) THEN notify.notify, event

END

FUNCTION as_upgradeversion::newestVersion, MESSAGE = message, SILENT = silent

  @as_scatterheader.macro

  CATCH, errorStatus 
   IF (errorStatus NE 0) THEN BEGIN
      CATCH, /CANCEL
      IF Keyword_Set(silent) THEN RETURN, -1
      self.urlObj.GetProperty, RESPONSE_CODE=rspCode, RESPONSE_HEADER=rspHdr, RESPONSE_FILENAME=rspFn
      IF rspCode EQ 6 THEN result = Dialog_Message('Error connecting to ' + self.url + '. Check internet connectivity, or try again later.') $
                      ELSE result = Dialog_Message(!Error_State.msg)
      RETURN, -1
   ENDIF
  
  result = self.urlObj.Get(URL = self.url + self.name + 'Version.dat', /STRING_ARRAY)
  self.version = Float(result[0])
  
  IF Arg_Present(message) THEN BEGIN
    message = self.urlObj.Get(URL = self.url + self.name + 'Message.txt', /STRING_ARRAY)
  ENDIF
  
  RETURN, self.version
  
END

FUNCTION as_upgradeversion::getNewestVersion, BACKUP = backup

  @as_scatterheader.macro

  failed = 0
  
  fileArray = self.urlObj.Get(URL = self.url + self.name + 'Manifest.txt', /STRING_ARRAY)
  FOREACH file, fileArray DO BEGIN 
    result = self.urlObj.Get(URL = self.url + file, FILENAME = self.dir + file + '.new')
    IF result NE self.dir + file + '.new' THEN failed = 1
  ENDFOREACH
 
  IF failed EQ 0 THEN BEGIN
    FOREACH file, fileArray DO BEGIN 
      IF File_Test(self.dir + file) THEN File_Move, self.dir + file, self.dir + file + '.bak', /OVERWRITE
      File_Move, self.dir + file + '.new', self.dir + file
    ENDFOREACH
  ENDIF
  
  RETURN, ~failed

END

PRO as_upgradeversion__define

void = {as_upgradeversion, $
        urlObj : Obj_New(), $
        notify : List(), $
        name   : '', $
        url    : '', $
        dir    : '', $
        version : 0.0 }

END