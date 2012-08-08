FUNCTION as_upgradeversion_callback, StatusInfo, ProgressInfo, object

  event = {DOWNLOADPROGRESS, downloaded : progressinfo[2], total : progressinfo[1]} 
  
  object.notify, event
  
  RETURN, 1

END

FUNCTION as_upgradeversion::init, programName, programURL, programInstallDir, NOTIFYOBJ = notifyObj


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

  FOREACH notify, self.notify DO IF Obj_Valid(notify) THEN notify.notify, event

END

FUNCTION as_upgradeversion::newestVersion

  CATCH, errorStatus 
   IF (errorStatus NE 0) THEN BEGIN
      CATCH, /CANCEL
      self.urlObj.GetProperty, RESPONSE_CODE=rspCode, RESPONSE_HEADER=rspHdr, RESPONSE_FILENAME=rspFn
      IF rspCode EQ 6 THEN result = Dialog_Message('Error connecting to ' + self.url + '. Check internet connectivity, or try again later.') $
                      ELSE result = Dialog_Message(!Error_State.msg)
      RETURN, -1
   ENDIF
  
  result = self.urlObj.Get(URL = self.url + self.name + 'Version.dat', /STRING_ARRAY)
  self.version = Float(result[0])
  
  RETURN, self.version
  
END

FUNCTION as_upgradeversion::getNewestVersion

  result = self.urlObj.Get(URL = self.url + self.name + '.sav', FILENAME = self.dir + self.name + '.sav')
 
  RETURN, 1

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