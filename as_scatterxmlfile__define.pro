FUNCTION as_scatterXMLFile::init, NOTIFYOBJECT = notifyObject

  @as_scatterheader.macro

  self.fileVersionNumber = StrCompress(1.0)

  IF Keyword_Set(notifyObject) THEN $
    IF TypeName(notifyObject[0]) EQ 'NOTIFY' $
      THEN self.notifyObject = List(notifyObject,/EXTRACT)

  self.currentConfig = 0

  void = {CONFIGURATION, CONFIGURATION: '', NAME: '', LOADCONFIG: '', DATAPATH: '', CAMERADEFS: '', USERMASKS: '', COUNTERDEFS: '', NORMALISATION: '', $
                         DETECTOR: '', LENGTH: '', BEAMX: '', BEAMY: '', WAVELENGTH: '', STOPACTIVE: '', STOPRADIUS: '', STOPOFFSETX: '', STOPOFFSETY : '', STOPANGLE: '', STOPWIDTH: '', WAXSANGLE: '', $
                         INCIDENT: '', TRANSMISSION: '',BEAMSTOP: '', $
                         ABSCAL: '', USEABSCAL: '', I0NORM: '', IBSNORM: '', NORMTYPE: ''}
  
  self.configurations    = Ptr_New({CONFIGURATION})
  self.attConfigurations = Ptr_New({ATTCONFIGURATION, $
                                          CONFIGURATION : ['NAME', 'LOADCONFIG', 'DATAPATH'],$
                                          CAMERADEFS    : ['DETECTOR','LENGTH','BEAMX','BEAMY','WAVELENGTH','STOPACTIVE','STOPRADIUS','STOPOFFSETX','STOPOFFSETY','STOPANGLE','STOPWIDTH','WAXSANGLE'], $
                                          USERMASKS     : [''], $
                                          COUNTERDEFS   : ['INCIDENT','TRANSMISSION','BEAMSTOP'], $
                                          NORMALISATION : ['ABSCAL','USEABSCAL','I0NORM','IBSNORM','NORMTYPE']})
  
  void = {DETECTORDEF, DETECTORDEF: '', XSIZE: '', YSIZE: '', PIXELSIZE: '', BASEPV: '', CAMPV: '', IMAGEPV: '', LOGFILEPV: '', FILEPV: '', CONTROL: '',SOFTWARETRIGGER: '', AUTOLOAD: '', ROTATION: ''}
  self.detectorDefs = Ptr_New({DETECTORDEF})
  self.attDetectorDefs = Ptr_New({ATTDETECTORDEF, DETECTORDEF : ['XSIZE','YSIZE','PIXELSIZE','BASEPV', 'CAMPV', 'IMAGEPV', 'LOGFILEPV', 'FILEPV', 'CONTROL', 'SOFTWARETRIGGER', 'AUTOLOAD', 'ROTATION']})
  
  void = {USERMASK, USERMASK: '', MASKNAME: '', SHAPE: '', AUTO: '', COLOUR: '', LOCK: ''}
  self.userMasks    = Ptr_New({USERMASK})
  self.attUserMasks = Ptr_New({ATTUSERMASK, USERMASK : ['MASKNAME','SHAPE','AUTO', 'COLOUR', 'LOCK']})
  
  void = {PV, PV: '', SET: '', READ: '', LOG: '', ACQUIRE: '', ACQUIREHIGH: '', ACQUIRELOW: '', TRANS: '', TRANSHIGH: '', TRANSLOW: ''}
  self.PVs = Ptr_New({PV})
  self.attPVs = Ptr_New({ATTPV, PV: ['SET','READ','LOG','ACQUIRE','ACQUIREHIGH','ACQUIRELOW','TRANS','TRANSHIGH','TRANSLOW']})
   
  void = {LOGLINE,  LOGLINE: '',COMMENT:'', ATTACHMENT:'', TYPE:''}
  self.loglines = Ptr_New({LOGLINE})
  self.attLogline = Ptr_New({ATTLOGLINE, LOGLINE: [Ptr_New()], COMMENT : ['ATTACHMENT'], TYPE : ['']})
   
  
  self.logPVs = Ptr_New({TESTATT: '' })
  
  self.attrList = Ptr_New(/ALLOCATE_HEAP)

  self.DTD =  '<?xml version="1.0" encoding="UTF-8"?>' + $
              '<!DOCTYPE scatterBrain [' + String([10B]) + $
              '<!ELEMENT scatterBrain (Parameters+,Experiment)>' + String([10B]) + $ 
              '<!ELEMENT Parameters (PVMap,DetectorMap, MaskMap,CONFIGURATION+)>' + String([10B]) + $
              '<!ELEMENT Experiment (LOGLINE*)>' + String([10B]) + $
              '<!ELEMENT PVMap (PV*)>' + String([10B]) + $
              '<!ELEMENT DetectorMap (DETECTORDEF*)>' + String([10B]) + $
              '<!ELEMENT MaskMap (USERMASK*)>' + String([10B]) + $
              '<!ELEMENT CONFIGURATION (CAMERADEFS,USERMASKS,COUNTERDEFS,NORMALISATION)>' + String([10B]) + $
              '<!ELEMENT LOGLINE (#PCDATA|COMMENT|TYPE)*>' + String([10B]) + $
              '<!ELEMENT PV EMPTY>' + String([10B]) + $
              '<!ELEMENT DETECTORDEF (#PCDATA)>' + String([10B]) + $
              '<!ELEMENT USERMASK (#PCDATA)>' + String([10B]) + $
              '<!ELEMENT CAMERADEFS EMPTY>' + String([10B]) + $
              '<!ELEMENT USERMASKS (#PCDATA)>' + String([10B]) + $
              '<!ELEMENT COUNTERDEFS EMPTY>' + String([10B]) + $
              '<!ELEMENT NORMALISATION EMPTY>' + String([10B]) + $
              '<!ELEMENT COMMENT (#PCDATA)>' + String([10B]) + $
              '<!ELEMENT TYPE (#PCDATA)>' + String([10B]) + $
              '' + String([10B]) + $
              '<!ATTLIST scatterBrain FileVersion CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CONFIGURATION NAME CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CONFIGURATION LOADCONFIG CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CONFIGURATION DATAPATH CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST PV ACQUIRE (true|false) #IMPLIED>' + String([10B]) + $
              '<!ATTLIST PV ACQUIREHIGH CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST PV ACQUIRELOW CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST PV LOG (true|false) #IMPLIED>' + String([10B]) + $
              '<!ATTLIST PV READ CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST PV SET CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST PV TRANS (true|false) #IMPLIED>' + String([10B]) + $
              '<!ATTLIST PV TRANSHIGH CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST PV TRANSLOW CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST USERMASK SHAPE (Polygon|Circle|Arc) #IMPLIED>' + String([10B]) + $
              '<!ATTLIST USERMASK MASKNAME CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST USERMASK AUTO (true|false) #IMPLIED>' + String([10B]) + $
              '<!ATTLIST USERMASK COLOUR CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST USERMASK LOCK (true|false) #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF BASEPV CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF CAMPV CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF IMAGEPV CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF LOGFILEPV CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF FILEPV CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF XSIZE CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF YSIZE CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF PIXELSIZE CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF CONTROL CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF SOFTWARETRIGGER CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF AUTOLOAD CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF ROTATION CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST DETECTORDEF ACTIVE CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS DETECTOR CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS BEAMX CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS BEAMY CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS LENGTH CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS STOPANGLE CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS STOPWIDTH CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS STOPRADIUS CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS STOPACTIVE CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS STOPOFFSETX CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS STOPOFFSETY CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS WAVELENGTH CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST CAMERADEFS WAXSANGLE CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST COUNTERDEFS BEAMSTOP CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST COUNTERDEFS INCIDENT CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST COUNTERDEFS TRANSMISSION CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST NORMALISATION ABSCAL CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST NORMALISATION USEABSCAL CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST NORMALISATION I0NORM CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST NORMALISATION IBSNORM CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST NORMALISATION NORMTYPE CDATA #IMPLIED>' + String([10B]) + $
              '<!ATTLIST COMMENT ATTACHMENT CDATA #IMPLIED>' + String([10B]) + $
              ']>' + String([10B]) + $
              '<scatterBrain FileVersion="' + self.fileVersionNumber + ' ">' + $
              '<Parameters>' + $
              '<PVMap>' + $
              '<PV ACQUIRE="true" ACQUIREHIGH="" ACQUIRELOW="" LOG="true" READ="" SET="" TRANS="true" TRANSHIGH="" TRANSLOW=""></PV>' + $
              '</PVMap>' + $
              '<DetectorMap>' + $
              '<DETECTORDEF BASEPV="" CAMPV="" IMAGEPV="" FILEPV="" XSIZE="" YSIZE="" PIXELSIZE="" CONTROL="" SOFTWARETRIGGER="" AUTOLOAD="" ROTATION="" ACTIVE=""></DETECTORDEF>' + $
              '</DetectorMap>' + $
              '<MaskMap>' + $
              '<USERMASK SHAPE="Polygon"></USERMASK>' + $
              '</MaskMap>' + $
              '<CONFIGURATION>' + $
              '<CAMERADEFS BEAMX="" BEAMY="" LENGTH="" STOPANGLE="" STOPRADIUS="" STOPWIDTH="" WAVELENGTH="" WAXSANGLE=""></CAMERADEFS>' + $
              '<USERMASKS></USERMASKS>' + $
              '<COUNTERDEFS BEAMSTOP="" INCIDENT="" TRANSMISSION=""></COUNTERDEFS>' + $
              '<NORMALISATION ABSCAL="" USEABSCAL="" I0NORM="" IBSNORM="" NORMTYPE=""></NORMALISATION>' + $
              '</CONFIGURATION>' + $
              '</Parameters>' + $
              '<Experiment>' + $
              '<LOGLINE>' + $
              '<COMMENT ATTACHMENT=""></COMMENT>' + $
              '<TYPE></TYPE>' +$
              '</LOGLINE>' + $
              '</Experiment>' + $
              '</scatterBrain> '


  RETURN, self->as_xmlparamfile::init()

END

PRO as_scatterXMLFile::notify, event
  
  @as_scatterheader.macro
  
  FOREACH notify, self.notifyObject DO IF Obj_Valid(notify) THEN notify.notify, event

END

PRO as_scatterXMLFile::StartElement, URI, local, strName, attrName, attrValue

  @as_scatterheader.macro

  IF KeyWord_Set(attrName) AND strName EQ 'LOGLINE' THEN BEGIN
    FOREACH attr, attrName DO BEGIN
      attrUp = StrUpCase(attr)
      IF Where(attrUp EQ *self.attrList) EQ -1 THEN *self.attrList = [*self.attrList,attrUp] 
    ENDFOREACH
  ENDIF
  
  self.AS_XMLParamFile::startElement, URI, local, strName, attrName, attrValue
  
END

PRO as_scatterXMLFile::ParseFile, fileName, LOGONLY=logOnly, UPDATE=upDate

  @as_scatterheader.macro

  currentLength = 0

  ; Check filename arguement contains something. If not return.
  IF N_Elements(fileName) EQ 0 THEN BEGIN
    filename = 'error'
    RETURN
  ENDIF 
  
  ; Check that file exists.
  IF ~File_Test(fileName) THEN BEGIN
    result = Dialog_Message('Filename for XML parseing routine invalid. Returning.')
    filename = 'error'
    RETURN
  ENDIF

  IF ~KeyWord_Set(LogOnly) THEN progressBarObj = Obj_New('progressBar',/FAST,/NOCANCEL,/START,TEXT='Opening file.')

  ; Set object filename variable to new name
  self.xmlFileName = fileName
  IF KeyWord_Set(LogOnly) THEN self.logFile = fileName ELSE BEGIN
    self.experimentFile = fileName
    self.logFile = ''
  ENDELSE

  ; If keyword "logOnly" is not set (so we are loading a full file) then complete this section (reading DTD from file).
  IF ~KeyWord_Set(logOnly) THEN BEGIN                                                      
    OpenR, XMLFile, self.xmlfilename, /GET_LUN
    temp = ''
    self.readDTD = ''
    ReadF, XMLFile, temp
    scatterBrainTagFound = 0
    WHILE ~EOF(XMLFile) DO BEGIN                                ;Read file to end
      ReadF, XMLFile, temp
      IF StrMid(temp,0,13) EQ '<scatterBrain' THEN BEGIN                                                ;If encounter start of scatterBrain elements (base element of XML) then
        self.readDTD = self.readDTD + StrMid(self.DTD,StrPos(self.DTD,'<scatterBrain>'))    ;complete self.readDTD by appending template XML file from self.DTD defined in init
        scatterBrainTagFound = 1
        BREAK                                                                               ;then break out.
      ENDIF
      self.readDTD = self.readDTD + temp + String([10B])        ; Store each line into a "readDTD variable"
    ENDWHILE
    Free_Lun, XMLFile
    IF scatterBrainTagFound EQ 0 THEN BEGIN
      result = Dialog_Message('Not a valid scatterBrain exmperiment file. Returning.')
      filename = 'error'
      progressBarObj.destroy
      RETURN
    ENDIF
    IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, 10
  ENDIF ELSE BEGIN      ; Complete next section if we only want to read a log file.
    lines = File_Lines(self.xmlfilename)    ; Number of loglines to read
    IF KeyWord_Set(update) THEN BEGIN ; If updating (just want to read lines that have been added since last read).
      currentLength = N_elements(*self.loglines)
      lastShot = currentLength - self.addedLines + 2*self.addedGapless > 0 ; How many shots have we already read?                       ;  Old Code: matching name instead of using number of shots:  lastShot = Where(StrMatch(logString, '*' + ((*self.loglines).logline)[-1] + '*') EQ 1)
      IF lastShot EQ 1 THEN BEGIN
        IF ((*self.loglines).logline)[-1] EQ '' THEN lastShot = 0
      ENDIF
      lines = lines - lastShot
      IF lines EQ 0 THEN BEGIN
        IF Obj_Valid(progressBarObj) THEN progressBarObj->Destroy
        RETURN
      ENDIF
    ENDIF ELSE BEGIN
      lastShot = 0
    ENDELSE
    OpenR, XMLFile, self.xmlfilename, /GET_LUN
    Skip_Lun, XMLFile, lastShot, /LINES
    logString = !Null
    temp = ''
    logstring = StrArr(lines)
    index = 0
;    WHILE NOT EOF(XMLFile) DO BEGIN   ;Read in file
;     readf, XMLFile, temp
;     logstring[index] = temp
;     index +=1
;    ENDWHILE
    FOR i = 0, lines - 1 DO BEGIN
      readf, XMLFile, temp
      logstring[i+index] = temp
    ENDFOR
    Free_Lun, XMLFile
    IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, 20
    
    
    ; Parse the log file without a DTD, just to populate the attrList. I.e., what are the attributes used in this log.
    xml = StrJoin(['<?xml version="1.0" encoding="UTF-8"?>','<scatterLog>',logString,'</scatterLog>'],/SINGLE)
    self.AS_XMLPARAMFILE::ParseFile, xml, /XML_STRING
    
    IF xml EQ 'error' THEN BEGIN
      IF Obj_Valid(progressBarObj) THEN progressBarObj->Destroy
      RETURN
    ENDIF
      
    logDtdHeader = '<?xml version="1.0" encoding="UTF-8"?>' + String([10B]) + $
                  '<!DOCTYPE scatterLog [' + String([10B]) + $
                  '<!ELEMENT scatterLog (LOGLINE)*>' + String([10B]) + $
                  '<!ELEMENT LOGLINE (#PCDATA)>' + String([10B])

    IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, 40

    ; Create DTD using the list of attributes used in loglines.
    IF N_Elements(*self.attrList) GT 0 THEN BEGIN
      self.logDtd = ''
      FOREACH attr, *self.attrList DO BEGIN
        self.logDtd = self.logDtd + '<!ATTLIST LOGLINE ' + attr + ' CDATA #IMPLIED>' + String([10B])
      ENDFOREACH
      dtd = logDtdHeader + self.logDtd
    ENDIF
    
    ; If wanting to do an update check if there are any new attributes. If there are cancel update tag, and do full read.
    IF KeyWord_Set(update) THEN BEGIN
      IF Size(*self.loglines, /TYPE) EQ 8 THEN names = Tag_Names(*self.loglines) ELSE names = ''
      FOREACH attr, *self.attrList DO BEGIN
        IF Where(StrUpCase(attr) EQ names) EQ -1 THEN BEGIN
          update = 0
          BREAK
        ENDIF
      ENDFOREACH
      ; If "update" cancelled need to reform DTD, using whole logfile
      IF KeyWord_Set(update) EQ 0 THEN BEGIN
        progressBarObj = Obj_New('progressBar',/FAST,/NOCANCEL,/START,TEXT='Opening file.')
        xml = StrJoin(['<?xml version="1.0" encoding="UTF-8"?>','<scatterLog>',logString,'</scatterLog>'],/SINGLE)
        self.AS_XMLPARAMFILE::ParseFile, xml, /XML_STRING
        IF xml EQ 'error' THEN BEGIN
          IF Obj_Valid(progressBarObj) THEN progressBarObj->Destroy
          RETURN
        ENDIF
        logDtdHeader = '<?xml version="1.0" encoding="UTF-8"?>' + String([10B]) + $
                       '<!DOCTYPE scatterLog [' + String([10B]) + $
                       '<!ELEMENT scatterLog (LOGLINE)*>' + String([10B]) + $
                       '<!ELEMENT LOGLINE (#PCDATA)>' + String([10B])
        
        IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, 60

        IF N_Elements(*self.attrList) GT 0 THEN BEGIN
          self.logDtd = ''
          FOREACH attr, *self.attrList DO BEGIN
            self.logDtd = self.logDtd + '<!ATTLIST LOGLINE ' + attr + ' CDATA #IMPLIED>' + String([10B])
          ENDFOREACH
          dtd = logDtdHeader + self.logDtd
        ENDIF
      ENDIF ;ELSE logString = logString[lastShot:*]
    ENDIF

    IF N_Elements(dtd) GT 0 THEN dtd = dtd + ']>' ELSE BEGIN
      filename = 'error'
      IF Obj_Valid(progressBarObj) THEN progressBarObj->Destroy
      RETURN
    ENDELSE
    logString = ['<scatterLog>',logString,'</scatterLog>']
    self.xmlFileName = StrJoin(logString, /SINGLE)
    xml_string = 1
  ENDELSE
  IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, 20
  ; Parse the file each time for separate part (probably not the most efficient way of doing it. Only do following if not a simple logfile, as these sections don't exist.  
  IF ~KeyWord_Set(LOGONLY) THEN BEGIN      
    tempStruct = {CONFIGURATION}
    tempAttStruct = {ATTCONFIGURATION}
    self->AS_XMLPARAMFILE::ParseFile, self.xmlFileName, STRUCT = tempStruct, ATTSTRUCT=tempAttStruct, XML_STRING=xml_string
    IF Size(*self.structArray, /TYPE) EQ 2 THEN BEGIN
      IF *self.structArray EQ -1 THEN BEGIN
        filename = 'error'
        IF Obj_Valid(progressBarObj) THEN progressBarObj->Destroy
        RETURN
      ENDIF
    ENDIF
    *self.configurations = *self.structArray
    
    IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, 30
    
    tempStruct = {USERMASK}
    tempAttStruct = {ATTUSERMASK}
    self->AS_XMLPARAMFILE::ParseFile, self.xmlFileName, STRUCT = tempStruct, ATTSTRUCT=tempAttStruct, XML_STRING=xml_string
    *self.userMasks = *self.structArray
    
    IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, 40
    
    tempStruct = {DETECTORDEF}
    tempAttStruct = {ATTDETECTORDEF}
    self->AS_XMLPARAMFILE::ParseFile, self.xmlFileName, STRUCT = tempStruct, ATTSTRUCT=tempAttStruct, XML_STRING=xml_string
    *self.detectorDefs = *self.structArray
    
    IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, 50
    
    tempStruct = {PV}
    tempAttStruct = {ATTPV}
    self->AS_XMLPARAMFILE::ParseFile, self.xmlFileName, STRUCT = tempStruct, ATTSTRUCT=tempAttStruct, XML_STRING=xml_string
    *self.PVs = *self.structArray

    IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, 60

    temp = ''
    i = 0
    logLineAttr = !Null
    WHILE (temp NE '<scatterBrain>' AND i LT N_Elements(xml)) DO BEGIN
      temp = self.readDTD[i]
      split = StrSplit(temp, /EXTRACT)
      IF N_Elements(split) GE 3 THEN BEGIN
        IF split[0] EQ '<!ATTLIST' AND split[1] EQ 'LOGLINE' THEN BEGIN
          logLineAttr = [logLineAttr,split[2]]
          StrDTDArray = StrArr(3)
          dtd = self.DTD
          StrDTDArray[0] = StrMid(dtd,0,StrPos(dtd,'<!ATTLIST COMMENT'))
          StrDTDArray[1] = '<!ATTLIST LOGLINE ' + split[2] + ' CDATA #IMPLIED>' + String([10B])
          StrDTDArray[2] = StrMid(dtd,StrPos(dtd,'<!ATTLIST COMMENT'))
          IF StrMatch(dtd, '*'+StrJoin(StrDTDArray)+'*',/FOLD_CASE) EQ 1 THEN CONTINUE
          self.dtd = StrJoin(StrDTDArray)
        ENDIF 
      ENDIF
      i++
    ENDWHILE
  
    IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, 70
  
;    tempStruct = !Null
;    IF logLineAttr NE !Null THEN FOREACH l, logLineAttr DO tempStruct = Create_Struct(l,'',tempStruct)

  ENDIF
  
  tempStruct = !Null
  
  IF KeyWord_Set(update) THEN tempStruct = Create_Struct({LOGLINE},(*self.logPVs)[0]) ELSE BEGIN
    IF N_Elements(*self.attrList) GT 0 THEN BEGIN
      FOREACH l, *self.attrList DO tempStruct = Create_Struct(l,'',tempStruct)
      *self.logPVs = tempStruct
      tempStruct = Create_Struct({LOGLINE},(*self.logPVs)[0])
    ENDIF ELSE BEGIN
      tempStruct = {LOGLINE}
    ENDELSE
  ENDELSE
  
  tempAttStruct = {ATTLOGLINE}
  self->AS_XMLPARAMFILE::ParseFile, self.xmlFileName, STRUCT = tempStruct, ATTSTRUCT=tempAttStruct, XML_STRING = xml_string
  IF KeyWord_Set(update) THEN *self.loglines = [(*self.loglines)[0:*], * self.structArray] ELSE *self.loglines = *self.structArray

  IF (*self.loglines) NE !NULL THEN BEGIN
    ((*self.loglines)[Where((*self.loglines).type EQ '',/NULL)].type) = 'RAW'
    
    IF (Where(tag_names(*self.loglines) EQ 'GAPLESS_MODE'))[0] GE 0 THEN BEGIN
      gapless_pos = Where((*self.loglines)[currentLength-self.gaplessProgress:*].Gapless_Mode EQ 1,/NULL)
      If gapless_pos NE !NULL THEN BEGIN
        if N_Elements(gapless_pos) MOD 3 GT 0 THEN BEGIN
          *self.loglines = (*self.loglines)[0:currentLength-1]
          return
        endif
        gapless_pos += currentLength-self.gaplessProgress
        
        last_filename = ''
        FOREACH g, reverse(gapless_pos) DO BEGIN
          line = (*self.loglines)[g]
          fname = File_Basename(line.logline)
          fname = StrMid(fname, 0, StrLen(fname)-7)
          IF fname NE last_filename THEN BEGIN
            last_filename = fname
            g_pos_array = list(g)
            count = 1
            ibs = fix(line.ibs)
            it = fix(line.it)
            i0 = fix(line.i0)
            self.gaplessProgress = 2
          ENDIF ELSE BEGIN
            g_pos_array.add, g
            count += 1
            ibs += fix(line.ibs)
            it += fix(line.it)
            i0 += fix(line.i0)
            self.gaplessProgress = 3
          ENDELSE
          IF count EQ 3 THEN BEGIN
            (*self.loglines)[g_pos_array[0]].ibs = ibs
            (*self.loglines)[g_pos_array[0]].it = it
            (*self.loglines)[g_pos_array[0]].i0 = i0
            (*self.loglines)[g_pos_array[0]].logline = File_Dirname(line.logline, /MARK_DIRECTORY) + fname + '.tif'
            IF g_pos_array[2] NE 0 THEN *self.loglines = (*self.loglines)[[findgen(g_pos_array[2]), g_pos_array[0] + findgen(N_elements(*self.loglines)-g_pos_array[0])]] $
                                   ELSE *self.loglines = (*self.loglines)[g_pos_array[0] + findgen(N_elements(*self.loglines)-g_pos_array[0])]
            self.addedGapless += 1
            self.gaplessProgress = 0

          ENDIF
           
        ENDFOREACH
      endif
    ENDIF
    
    IF ~KeyWord_Set(logonly) THEN BEGIN
      void = Where((*self.loglines).type EQ 'RAW', NCOMPLEMENT = addedLines)
      self.addedlines = addedLines
    ENDIF
  ENDIF

;  numLogLines = N_Elements((*self.loglines).logline)
;  tempstr = StrArr(numLogLines)
;  FOR i = 0 , numLogLines - 1 DO BEGIN
;    tempStr[i] = (StrSplit(((*self.loglines).logline)[i],String([10B]),/EXTRACT))[0]
;  ENDFOR 
;  (*self.loglines).logline = tempStr

  IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, 100

  IF Obj_Valid(progressBarObj) THEN progressBarObj->Destroy

  IF ~KeyWord_Set(LogOnly) AND File_Test(self.logFile) EQ 1 THEN self.xmlFileName = self.logFile 

END

PRO as_scatterXMLFile::Clear

  @as_scatterheader.macro
  
  self.configurations = Ptr_New({CONFIGURATION})
  self.detectorDefs = Ptr_New({DETECTORDEF})
  self.userMasks = Ptr_New({USERMASK})
  self.PVs = Ptr_New({PV})
  self.loglines = Ptr_New({LOGLINE})
  self.attrList = Ptr_New(/ALLOCATE_HEAP)

END

PRO as_scatterXMLFile::SaveFile, fileName, EMPTY = empty, FILELIST = fileList, TYPEFILELIST = typeFileList, QUIET=quiet

  @as_scatterheader.macro
  
  quiet = KeyWord_Set(quiet)
  
  (*self.attLogline).logline = Ptr_New(Tag_Names(*self.logPVs))
  
  IF N_Elements(fileName) GE 1 THEN self.xmlFileName = (self.experimentFile = fileName) ELSE BEGIN
    IF File_Test(self.experimentFile) THEN self.xmlFileName = self.experimentFile ELSE BEGIN
      file = Dialog_Pickfile(TITLE = 'No experiment file loaded yet, choose file to write to.' ,/WRITE, /OVERWRITE_PROMPT)
      IF file EQ '' THEN RETURN
      self.experimentFile = (self.xmlFileName = file)
    ENDELSE
  ENDELSE

  IF File_Test(self.experimentFile) AND ~quiet THEN BEGIN
    result = Dialog_Message('This will overwite the file '+ self.experimentFile +'. Are you sure you want to continue?',/QUESTION)
    IF result EQ 'No' THEN RETURN
  ENDIF
  ;IF self.readDTD NE '' THEN BEGIN
  ;  self->New, 'scatterBrain', DTD = self.readDTD 
  ;ENDIF ELSE self->New, 'scatterBrain', DTD = self.DTD 
  savingMessage = progressbar(/FAST,/NOCANCEL,/START,TEXT='Saving experiment file...')
  logDtd = ''
  FOREACH logAtt, *((*self.attlogline).logline[0]) DO BEGIN
    logDtd = logDtd + '<!ATTLIST LOGLINE ' + logAtt + ' CDATA #IMPLIED>' + String([10B])
  ENDFOREACH
  
  self->New, 'scatterBrain', DTD = StrMid(self.dtd,0,StrPos(self.dtd, ']>')) + logDtd + StrMid(self.dtd, StrPos(self.dtd, ']>'))
    savingMessage->Update, 10
  parameters = self->AddElement('base', 'Parameters', '')
  PVMap = self->AddElement(parameters, 'PVMap','')
  self->NewFromStruct, STRUCT = *self.pvs, ATTSTRUCT = *self.attPVs, APPENDTO=PVMap
    savingMessage->Update, 20
  detectorMap = self->AddElement(parameters, 'DetectorMap','')
  self->NewFromStruct, STRUCT = *self.detectorDefs, ATTSTRUCT = *self.attDetectorDefs, APPENDTO=detectorMap
    savingMessage->Update, 30
  maskMap = self->AddElement(parameters, 'MaskMap','')
  self->NewFromStruct, STRUCT = *self.userMasks, ATTSTRUCT = *self.attUserMasks, APPENDTO=maskMap
    savingMessage->Update, 40
  self->NewFromStruct, STRUCT = *self.configurations, ATTSTRUCT = *self.attConfigurations, APPENDTO=parameters
    savingMessage->Update, 50
  experiment = self->AddElement('base', 'Experiment', '')
  IF ~Keyword_Set(empty) THEN IF *self.loglines NE !Null THEN BEGIN
    IF N_Elements(fileList) GT 0 THEN BEGIN
      logLinesTemp = Replicate((*self.loglines)[0],N_Elements(fileList))
      fileListTemp = List()
      FOREACH logline, (*self.loglines).logline DO fileListTemp.add, File_Basename(logline)
      FOREACH file, fileList, key DO loglinesTemp[key] = (*self.loglines)[(Where(fileListTemp.toarray() EQ file))[0]]
    ENDIF ELSE loglinesTemp = *self.loglines 
    ;self->NewFromStruct, STRUCT = loglinesTemp, ATTSTRUCT = *self.attLogline, APPENDTO=experiment
    numLines = N_Elements(loglinesTemp)
    FOREACH logline, loglinesTemp, lineNo DO BEGIN
      IF TypeName(typeFileList) EQ 'STRING' THEN IF StrUpCase(typeFileList[0]) EQ 'COPY' THEN BEGIN
        loglineFile = File_BaseName(logline.logline)
        loglinePath = File_DirName(logline.logline, /MARK_DIRECTORY)
        logline.logline = loglinePath + 'copy_' + loglineFile
      ENDIF
      line = self->AddElement(experiment, 'LOGLINE', logline.logline)
      FOREACH attribute, (Tag_Names(logline))[4:*], key DO self->AddAttribute, line, attribute, logline.(key+4)
      comment = self->AddElement(line, 'COMMENT', logline.comment)
      self->AddAttribute, comment, 'ATTACHMENT', logline.attachment
      IF TypeName(typeFileList) EQ 'STRING' THEN type = self->AddElement(line, 'TYPE', typeFileList[0]) $
                                            ELSE type = self->AddElement(line, 'TYPE', logline.type)
      savingMessage->Update, 50 + 40*(numLines-1)/lineNo
    ENDFOREACH 
  ENDIF
  
  self->Save, FILENAME=self.xmlFileName
  savingMessage->Update, 100
  savingMessage->Destroy
  
  IF File_Test(self.logFile) THEN self.xmlFileName = self.logFile

END

PRO as_scatterXMLFile::SetParameters, $
MASK=mask, $
CHANGEDMASKNAMES = changedMaskNames, $ 
BEAMSTOP = beamStop, $
CAKE=cake, FRAME=frame, $
ADMAP=ADMap, PVMAP=PVMap, $
ABSCAL=absCal, $
USEABSCAL = useAbsCal, $ 
NORMTYPE=normType, $
I0NORM=I0Norm, $
IBSNORM=IBSNorm, $
COUNTERDEFS = counterDefs, $ 
CONFIGNO=config, $
CONFIGNAME = configName, $
CONFIGDATAPATH = configDataPath

  @as_scatterheader.macro
  
  ; TODO Forcing detector 0 at this stage.
  ;detNum = 0 

  IF ~KeyWord_Set(config) THEN config = 0   

  IF config GE N_Elements(*self.configurations) THEN BEGIN
    *self.configurations = [*self.configurations,{CONFIGURATION}]
    (*self.configurations)[config].dataPath = (*self.configurations)[config-1].dataPath 
  ENDIF

  IF KeyWord_Set(configName) THEN (*self.configurations)[config].name = configName
  IF N_Elements(configDataPath) THEN (*self.configurations)[config].dataPath = configDataPath

  IF KeyWord_Set(mask) THEN BEGIN
  
    userMasks = list()
    
    FOREACH m, mask DO BEGIN
      ;IF m.delete EQ 1 THEN BEGIN
       ; IF 
      ;ENDIF
      IF m.auto EQ 0 THEN auto = 'false' ELSE auto = 'true'
      IF m.lock EQ 0 THEN lock = 'false' ELSE lock = 'true'
      IF Size((*m.params),/N_DIMENSIONS) EQ 2 THEN BEGIN
        params = '['+ StrJoin(Transpose(StrCompress((*m.params)[0,*], /REMOVE_ALL)),',') + ']'
        params = params + '['+ StrJoin(Transpose(StrCompress((*m.params)[1,*], /REMOVE_ALL)),',') + ']'
      ENDIF ELSE params = '['+ StrJoin(StrCompress((*m.params), /REMOVE_ALL),',') + ']'
      colour = '[' + strcompress(strjoin(string(m.colour, format = '(I)'),','),/REMOVE_ALL) + ']'
      usermasks.add, {USERMASK, usermask : params, MASKNAME : m.name, SHAPE :m.shape, AUTO : auto, COLOUR : colour, LOCK : lock}
    ENDFOREACH 

    *self.usermasks = usermasks.toArray()

    usedMasks = Where(mask.type GE 0)
    (*self.Configurations)[config].userMasks = '[' + StrJoin(mask[usedMasks].name, ',') + '][' + StrJoin(mask[usedMasks].type, ',') + ']'
      
  ENDIF
  

;  IF KeyWord_Set(changedMaskNames) THEN BEGIN
;    row = 0
;    FOREACH cMN, changedMaskNames[1,*] DO BEGIN
;      conf=0
;      FOREACH um, (*self.Configurations).userMasks DO BEGIN
;        ;userMaskFragments = StrSplit(um,'[,[]'+cMN+'[],]',/REGEX,/EXTRACT) 
;        userMaskFragments = StrSplit(um,',[]',/EXTRACT)
;        userMaskFragments[Where(userMaskFragments EQ cMN[0],/NULL)] = changedMaskNames[0,row]
;        ;IF N_Elements(userMaskFragments) EQ 2 THEN ((*self.Configurations)[conf].userMasks) = StrJoin([userMaskFragments[0],changedMaskNames[0,row],userMaskFragments[1]])
;        numFrags = N_Elements(userMaskFragments)
;        IF numFrags GT 1 THEN BEGIN
;          userMaskFragments[0] = '['+userMaskFragments[0]
;          userMaskFragments[(numFrags/2) - 1] = userMaskFragments[(numFrags/2) - 1]+']'
;          userMaskFragments[(numFrags/2)] = '['+userMaskFragments[(numFrags/2)]
;          userMaskFragments[-1] = userMaskFragments[-1]+']' 
;          ((*self.Configurations)[conf].userMasks) = StrJoin(userMaskFragments, ',')
;        ENDIF
;        conf++
;      ENDFOREACH
;      row++
;    ENDFOREACH
;  ENDIF

  IF KeyWord_Set(counterDefs) THEN BEGIN
    ((*self.configurations)[config].beamstop) = counterDefs.beamstop
    ((*self.configurations)[config].incident) = counterDefs.incident
    ((*self.configurations)[config].transmission) = counterDefs.transmission
  ENDIF

  IF KeyWord_Set(beamStop) THEN BEGIN
    ((*self.configurations)[config].stopactive) = beamstop.maskType
    ((*self.configurations)[config].stopradius) = beamstop.radius
    ((*self.configurations)[config].stopoffsetX) = beamstop.offsetX
    ((*self.configurations)[config].stopoffsetY) = beamstop.offsetY
    ((*self.configurations)[config].stopangle) = beamstop.angle
    ((*self.configurations)[config].stopwidth) = beamstop.width
  ENDIF
  
  IF KeyWord_Set(frame) THEN BEGIN
     
     ((*self.configurations)[config].detector) = frame.detector
     
     ((*self.configurations)[config].wavelength) = frame.wlen
     ((*self.configurations)[config].length)     = frame.len
     ((*self.configurations)[config].beamx)      = frame.xc
     ((*self.configurations)[config].beamy)      = frame.yc
     ((*self.configurations)[config].waxsangle)  = frame.detangle
     ;((*self.configurations)[config].waxsangle)  = frame.detoffsetv
     ;((*self.configurations)[config].waxsangle)  = frame.detoffseth
     
   ENDIF

  IF N_Elements(absCal) GT 0 THEN BEGIN
    (*self.configurations)[config].absCal = absCal
  ENDIF
  
  IF N_Elements(useAbsCal) GT 0 THEN BEGIN
    (*self.configurations)[config].useAbsCal = useAbsCal
  ENDIF

  IF N_Elements(normType) GT 0 THEN BEGIN
    (*self.configurations)[config].normType = normType
  ENDIF

  IF N_Elements(I0Norm) GT 0 THEN BEGIN
    (*self.configurations)[config].I0Norm = I0Norm
  ENDIF

  IF N_Elements(IBSNorm) GT 0 THEN BEGIN
    (*self.configurations)[config].IBSNorm = IBSNorm
  ENDIF

  IF KeyWord_Set(ADMap) THEN BEGIN
    tempDetectorDefs = *self.detectorDefs
    *self.detectorDefs = Replicate({DETECTORDEF}, N_Elements(ADMap))
      
    (*self.detectorDefs).DetectorDef = String(ADMap.DetectorDef) 
    FOREACH xsize, ADMap.XSize, key DO IF xsize GT 0 THEN ((*self.detectorDefs)[key].XSize) = String(xsize) ELSE ((*self.detectorDefs)[key].XSize) = String((tempDetectorDefs.XSize)[key])
    FOREACH ysize, ADMap.YSize, key DO IF ysize GT 0 THEN ((*self.detectorDefs)[key].YSize) = String(ysize) ELSE ((*self.detectorDefs)[key].YSize) = (tempDetectorDefs.YSize)[key]
    FOREACH psize, ADMap.PixelSize, key DO IF psize GT 0 THEN ((*self.detectorDefs)[key].PixelSize) = String(psize) ELSE ((*self.detectorDefs)[key].PixelSize) = (tempDetectorDefs.PixelSize)[key]
    (*self.detectorDefs).BasePV      = String(ADMap.BasePV)          
    (*self.detectorDefs).CamPV       = String(ADMap.CamPV)           
    (*self.detectorDefs).ImagePV     = String(ADMap.ImagePV)
    (*self.detectorDefs).LogFilePV   = String(ADMap.LogFilePV)         
    (*self.detectorDefs).FilePV      = String(ADMap.FilePV)          
    (*self.detectorDefs).Control     = String(ADMap.Control)         
    (*self.detectorDefs).SoftwareTrigger = String(ADMap.SoftwareTrigger) 
    (*self.detectorDefs).AutoLoad    = String(ADMap.AutoLoad)
    (*self.detectorDefs).Rotation    = tempDetectorDefs.rotation;String(ADMap.Rotation)        
        
  ENDIF

END

PRO as_scatterXMLFile::GetParameters, $
MASK=mask, $
BEAMSTOP = beamstop, $
CAKE=cake, FRAME=frame, $
ADMAP=ADMap, $
PVMAP=PVMap, $
NORMPARAMS = normParams, $
LOGPARAMS = logParams, $
RAWLOG = rawLog, $
CONFIGDATAPATH = configDataPath, $
CONFIGNO = config, $
NUMRAWIMAGES = numRawImages


  @as_scatterheader.macro

  IF ~KeyWord_Set(config) THEN config = 0  
  IF Arg_Present(configDataPath) THEN configDataPath = (*self.configurations)[config].dataPath

 IF Arg_Present(mask) THEN BEGIN
   
   IF Size(mask, /TYPE) EQ 0 THEN mask = 0
   
   void = { maskdef, name: '', type: 0, shape: '', auto: 0b, lock: 0b, colour : IntArr(3), params: Ptr_New() }
   
     IF N_Elements(*self.usermasks) GE 1 THEN IF (*self.usermasks)[0].usermask NE '' THEN BEGIN
     
       textSplit = StrSplit(((*self.configurations)[mask]).usermasks,'[]',/EXTRACT)
       knownMasks = StrSplit(textSplit[0],',',/EXTRACT)
       IF N_Elements(textSplit) GT 1 THEN maskType = StrSplit(textSplit[1],',',/EXTRACT)
       maskdef = Replicate({maskdef}, N_Elements(*self.usermasks)) 
       FOREACH umask, *self.usermasks, index DO BEGIN
        
         ;userMaskNum = Where(((*self.userMasks).maskname) EQ knownMask)
         knownMaskIndex = Where(umask.maskname EQ knownMasks)
         if knownMaskIndex GE 0 THEN type = Long(maskType[knownMaskIndex]) ELSE type = 0
         tempParams = StrSplit(umask.usermask,'[]',/EXTRACT)
         tempParams1 = StrSplit(tempParams[0],',',/EXTRACT)

         IF N_Elements(tempParams) EQ 2 THEN BEGIN
           tempParams2 = StrSplit(tempParams[1],',',/EXTRACT)
           params = Transpose([[tempParams1],[tempParams2]])
         ENDIF ELSE params = tempParams1
         
         IF umask.colour EQ '' THEN colour = '[0,0,0]' ELSE colour = umask.colour
         colour = Fix([strsplit(colour,'[,]',/EXTRACT)])
         maskdef[index] = { name: umask.maskname, type: type, shape: umask.shape, auto: umask.auto EQ 'true', lock: umask.lock EQ 'true', colour: colour, params : Ptr_New(Float(params)) }

       ENDFOREACH


;       FOR i = 0, N_Elements(*self.usermasks) - 1 DO BEGIN
;         usedMask = Where(knownMasks EQ ((*self.userMasks).maskname)[i])
;         IF usedMask GE 0 THEN type = Long(maskType[i]) ELSE type = 0
;         tempParams = StrSplit(((*self.userMasks).usermask)[i],'[]',/EXTRACT)
;         tempParams1 = StrSplit(tempParams[0],',',/EXTRACT)
;         IF N_Elements(tempParams) EQ 2 THEN BEGIN
;           tempParams2 = StrSplit(tempParams[1],',',/EXTRACT)
;           params = Transpose([[tempParams1],[tempParams2]])
;         ENDIF ELSE params = tempParams1
;         IF ((*self.userMasks).colour)[i] EQ '' THEN colour = '[0,0,0]' ELSE colour = ((*self.userMasks).colour)[i]
;         colour = Fix([strsplit(colour,'[,]',/EXTRACT)])
;         maskdef[i] = { name: ((*self.userMasks).maskname)[i], type: type, shape: ((*self.userMasks).shape)[i], auto: ((*self.userMasks).auto)[i] EQ 'true', lock: ((*self.userMasks).lock)[i] EQ 'true', colour: colour, params : Ptr_New(Float(params)) }
;       ENDFOR
       
       mask = maskdef
     ENDIF
   
  ENDIF
  
  IF Arg_Present(beamstop) THEN BEGIN
    IF Size(beamstop, /TYPE) EQ 0 THEN beamstop = 0
    configStruct = (*self.configurations)[beamstop]
    beamstop = {masktype : configStruct.stopactive, radius : configStruct.stopradius, offsetX : configStruct.stopOffsetX, offsetY : configStruct.stopOffsetY, width : configStruct.stopWidth, angle : configStruct.stopangle}
  ENDIF
    
  IF Arg_Present(cake) THEN cake = { a: 0 }
 
  IF Arg_Present(normParams) THEN normParams = { absCal : Double((*self.configurations)[normParams].absCal), useAbsCal : Fix((*self.configurations)[normParams].useAbsCal), I0Norm : Long((*self.configurations)[normParams].I0Norm), IBSNorm : Long((*self.configurations)[normParams].IBSNorm), normType : Fix((*self.configurations)[normParams].normType) }
 
  IF Arg_Present(frame) THEN BEGIN
  
   numConfigs = N_Elements(*self.configurations)

   detNum = (Where(((*self.configurations).detector)[0] EQ (*self.detectorDefs).DETECTORDEF))[0]
   
   frame = { confName: ((*self.configurations).name)[0], $
             detector: ((*self.detectorDefs).detectorDef)[detNum], $
             nxchip  : ((*self.detectorDefs).xsize)[detNum], $
             nychip  : ((*self.detectorDefs).ysize)[detNum], $
             nxpix   : ((*self.detectorDefs).xsize)[detNum], $
             nypix   : ((*self.detectorDefs).ysize)[detNum], $
             psize   : ((*self.detectorDefs).pixelSize)[detNum], $
             wlen    : ((*self.configurations).wavelength)[0],$
             len     : ((*self.configurations).length)[0],$
             xc      : ((*self.configurations).beamx)[0], $
             yc      : ((*self.configurations).beamy)[0], $
             detangle: ((*self.configurations).waxsangle)[0]}

   FOR i = 1, numConfigs - 1 DO BEGIN
     detNum = Where(((*self.configurations).detector)[i] EQ (*self.detectorDefs).DETECTORDEF)
     
     frame = [frame, $
             { confName: ((*self.configurations).name)[i], $
               detector: ((*self.detectorDefs).detectorDef)[detNum], $
               nxchip  : ((*self.detectorDefs).xsize)[detNum], $
               nychip  : ((*self.detectorDefs).ysize)[detNum], $
               nxpix   : ((*self.detectorDefs).xsize)[detNum], $
               nypix   : ((*self.detectorDefs).ysize)[detNum], $
               psize   : ((*self.detectorDefs).pixelSize)[detNum], $
               wlen    : ((*self.configurations).wavelength)[i],$
               len     : ((*self.configurations).length)[i],$
               xc      : ((*self.configurations).beamx)[i], $
               yc      : ((*self.configurations).beamy)[i], $
               detangle: ((*self.configurations).waxsangle)[i]}]
   
   ENDFOR          
             
 ENDIF
 
 IF Arg_Present(ADMap) THEN BEGIN
  numDetectors = N_Elements(*self.detectorDefs)
  ADMap = *self.detectorDefs
 ENDIF

  IF Arg_Present(PVMap) THEN BEGIN
    numPVs = N_Elements(*self.pvs)
  
    IF numPVs GT 0 THEN BEGIN
    
      PVMap = { acquireenable      : StrUpCase(((*self.pvs).acquire)[0]) EQ 'TRUE', $
                acquireLow         : ((*self.pvs).acquireLow)[0], $
                acquireHigh        : ((*self.pvs).acquireHigh)[0], $
                logenable          : StrUpCase(((*self.pvs).log)[0]) EQ 'TRUE', $
                pvRBV              : ((*self.pvs).read)[0], $
                pvset              : ((*self.pvs).set)[0], $
                transmissionenable : StrUpCase(((*self.pvs).trans)[0]) EQ 'TRUE', $
                transmissionhigh   : ((*self.pvs).transHigh)[0], $
                transmissionlow    : ((*self.pvs).transLow)[0] }
    
    ENDIF  
              
    FOR i = 1, numPVs - 1 DO BEGIN
      
      PVMap = [PVMap, $
              { acquireenable      : StrUpCase(((*self.pvs).acquire)[i]) EQ 'TRUE', $
                acquireLow         : ((*self.pvs).acquireLow)[i], $
                acquireHigh        : ((*self.pvs).acquireHigh)[i], $
                logenable          : StrUpCase(((*self.pvs).log)[i]) EQ 'TRUE', $
                pvRBV              : ((*self.pvs).read)[i], $
                pvset              : ((*self.pvs).set)[i], $
                transmissionenable : StrUpCase(((*self.pvs).trans)[i]) EQ 'TRUE', $
                transmissionhigh   : ((*self.pvs).transHigh)[i], $
                transmissionlow    : ((*self.pvs).transLow)[i] }]
      
    ENDFOR

  ENDIF

  IF Arg_Present(rawLog) THEN rawLog = *self.loglines

  IF Arg_Present(numRawImages) AND *self.loglines NE !NULL THEN BEGIN
    void = Where((*self.loglines).type EQ 'RAW', numRawImages)
  ENDIF ELSE numRawImages = 0
    
END

FUNCTION as_scatterXMLFile::GetLogAttributes

  @as_scatterheader.macro

  RETURN, Tag_Names(*self.logPVs)

END

FUNCTION as_scatterXMLFile::GetValue, param

  @as_scatterheader.macro
  
  paramNo = -1
  IF Size(*self.PVs,/TYPE) EQ 8 THEN paramNo = Where(STRUPCASE(param) EQ Tag_Names(*self.PVs))
  IF paramNo GE 0 THEN RETURN, (*self.PVs).(paramNo)
  IF Size(*self.userMasks,/TYPE) EQ 8 THEN paramNo = Where(STRUPCASE(param) EQ Tag_Names(*self.userMasks))
  IF paramNo GE 0 THEN RETURN, (*self.userMasks).(paramNo)
  IF Size(*self.configurations,/TYPE) EQ 8 THEN paramNo = Where(STRUPCASE(param) EQ Tag_Names(*self.configurations))
  IF paramNo GE 0 THEN RETURN, (*self.configurations).(paramNo)
  IF Size(*self.loglines,/TYPE) EQ 8 THEN paramNo = Where(STRUPCASE(param) EQ Tag_Names(*self.loglines))
  IF paramNo GE 0 THEN RETURN, (*self.loglines).(paramNo)
  IF Size(*self.logPVs,/TYPE) EQ 8 THEN paramNo = Where(STRUPCASE(param) EQ Tag_Names(*self.logPVs))
  IF paramNo GE 0 THEN RETURN, (*self.logPVs).(paramNo)
  
  RETURN, ''

END

PRO as_scatterXMLFile::SetValue, param, setValue, POSITION=position

  @as_scatterheader.macro

  FOR index = 0, N_Elements(param) - 1 DO BEGIN
    IF N_Elements(setValue) EQ N_Elements(param) THEN value = setValue[index] ELSE value = setValue
    
    paramNo = Where(STRUPCASE(param[index]) EQ Tag_Names(*self.PVs))
    IF paramNo GE 0 THEN BEGIN  
      IF N_Elements(position) EQ 0 THEN (*self.PVs)[*].(paramNo) = value
      IF N_Elements(position) GE 1 THEN BEGIN
        IF position NE -1 THEN (*self.PVs)[position].(paramNo) = value ELSE BEGIN
          *self.PVs = [*self.PVs,{PV}]
          (*self.PVs)[N_Elements(*self.PVs)-1].(paramNo) = value
          position = N_Elements(*self.PVs)-1
        ENDELSE
      ENDIF
    ENDIF
    
    paramNo = Where(STRUPCASE(param[index]) EQ Tag_Names(*self.userMasks))
    IF paramNo GE 0 THEN BEGIN  
      IF N_Elements(position) EQ 0 THEN (*self.userMasks)[*].(paramNo) = value
      IF N_Elements(position) GE 1 THEN BEGIN
        IF position NE -1 THEN (*self.userMasks)[position].(paramNo) = value ELSE BEGIN
          *self.userMasks = [*self.userMasks,{USERMASK}]
          (*self.userMasks)[N_Elements(*self.userMasks)-1].(paramNo) = value
          position = N_Elements(*self.userMasks)-1
        ENDELSE
      ENDIF
    ENDIF
  
    paramNo = Where(STRUPCASE(param[index]) EQ Tag_Names(*self.configurations))
    IF paramNo GE 0 THEN BEGIN  
      IF N_Elements(position) EQ 0 THEN (*self.configurations)[*].(paramNo) = value
      IF N_Elements(position) GE 1 THEN BEGIN
        IF position NE -1 THEN (*self.configurations)[position].(paramNo) = value ELSE BEGIN
          *self.configurations = [*self.configurations,{CONFIGURATION}]
          (*self.configurations)[N_Elements(*self.configurations)-1].(paramNo) = value
          position = N_Elements(*self.configurations)-1
        ENDELSE
      ENDIF
    ENDIF
    IF ISA(*self.loglines,'STRUCT') THEN paramNo = Where(STRUPCASE(param[index]) EQ Tag_Names(*self.loglines)) ELSE paramNo = -1
    IF paramNo GE 0 THEN BEGIN  
      IF N_Elements(position) EQ 0 THEN (*self.loglines)[*].(paramNo) = value
      IF N_Elements(position) GE 1 THEN BEGIN
        IF position NE -1 THEN (*self.loglines)[position].(paramNo) = value ELSE BEGIN
          *self.loglines = [*self.loglines,{LOGLINE}]
          (*self.loglines)[N_Elements(*self.loglines)-1].(paramNo) = value
          position = N_Elements(*self.loglines)-1
          names = Tag_Names(*self.logPVs)
          tempStruct = Create_Struct(names[0],'')
          FOR i = 1, N_Elements(names) - 1 DO tempStruct = Create_Struct(tempStruct, names[i],'')
          *self.logPVs = [*self.logPVs,tempStruct]
        ENDELSE
      ENDIF
    ENDIF
    paramNo = Where(STRUPCASE(param[index]) EQ Tag_Names(*self.logPVs))
    IF paramNo GE 0 THEN BEGIN  
      IF N_Elements(position) EQ 0 THEN (*self.logPVs)[*].(paramNo) = value
      IF N_Elements(position) GE 1 THEN (*self.logPVs)[position].(paramNo) = value
    ENDIF
  ENDFOR

END

PRO as_scatterXMLFile::RedefineLogLine, loglineStruct

  @as_scatterheader.macro

  self.loglines = Ptr_New(loglineStruct)
  newNames = Tag_Names(loglineStruct)
  oldNames = Tag_Names({LOGLINE})
  nameList = list()
  FOREACH name, newNames DO IF Where(name EQ oldNames) EQ -1 THEN nameList.add, name
   
  IF nameList.count() GT 0 THEN (*self.attLogline).logline = Ptr_New(nameList.toArray())
  
  tempStruct = !Null
  
  FOREACH l, nameList DO tempStruct = Create_Struct(l,'',tempStruct)
  *self.logPVs = tempStruct
  
END

FUNCTION as_scatterXMLFile::FileLoaded

  @as_scatterheader.macro

  IF self.xmlFileName NE '' OR self.xmlFileName NE 'error' THEN RETURN, 1 ELSE RETURN, 0

END

PRO as_scatterXMLFile::GetProperty, XMLFILENAME = xmlFileName, LOADCONFIG = loadConfig, NUMLOADCONFIG = numLoadConfig 

  @as_scatterheader.macro

  IF Arg_Present(loadConfig) THEN BEGIN
  
    name = self.GetValue('name')
    loadConfigDet = self.GetValue('loadconfig')
    
    loadConfig = { Detector1 : name[Where(loadConfigDet EQ 'DETECTOR1')], Detector2 : name[Where(loadConfigDet EQ 'DETECTOR2',/NULL)]}
    
  ENDIF

  IF Arg_Present(numLoadConfig) THEN BEGIN
  
    name = self.GetValue('name')
    loadConfigDet = self.GetValue('loadconfig')
    
    numLoadConfig = { Detector1 : (Where(loadConfigDet EQ 'DETECTOR1'))[0], Detector2 : (Where(loadConfigDet EQ 'DETECTOR2'))[0]}
    
  ENDIF


  IF Arg_Present(xmlFileName) THEN xmlFileName = self.xmlFileName

END

PRO as_scatterXMLFile::NewLogLine, fname, exptime, i0, it, ibs, TIMESTAMP = timeStamp, TYPE = type

  @as_scatterheader.macro

  IF ~KeyWord_Set(type) THEN type = 'Processed' 

  *self.loglines = [*self.loglines, (*self.loglines)[0]]
  
  FOR i=0, N_Elements(tag_names((*self.loglines)[-1]))-1 DO (*self.loglines)[-1].(i) = ''
  
  ((*self.loglines))[-1].type = type
  ((*self.loglines))[-1].exptime = exptime
  ((*self.loglines))[-1].logline = fname
  
  i0Attr = StrUpCase((*self.configurations)[self.currentConfig].Incident) 
  itAttr = StrUpCase((*self.configurations)[self.currentConfig].Transmission)
  ibsAttr = StrUpCase((*self.configurations)[self.currentConfig].Beamstop)
  
  names = Tag_Names(*self.loglines)
  i0Pos = Where(names EQ i0Attr)
  itPos = Where(names EQ itAttr)
  ibsPos = Where(names EQ ibsAttr)
  
  ((*self.loglines))[-1].(i0Pos) = i0
  ((*self.loglines))[-1].(itPos) = it
  ((*self.loglines))[-1].(ibsPos) = ibs

  IF N_Elements(timeStamp) GT 0 THEN ((*self.loglines))[-1].timestamp = timeStamp ELSE ((*self.loglines))[-1].timestamp = systime()
  self.addedLines = self.addedLines + 1

END

FUNCTION as_scatterXMLFile::GetIndex, fname

  @as_scatterheader.macro

;  IF Size(*self.loglines, /TYPE) EQ 8 THEN BEGIN
;    filenames = !Null
;    FOREACH filename, (*self.loglines).logline DO filenames = [filenames,AS_FNameStripDir(filename, SEPARATOR='/')]
;    index = Where(filenames EQ fname)
;  ENDIF ELSE index = 0
  
  IF Size(*self.loglines, /TYPE) EQ 8 THEN BEGIN
    index = Where(File_Basename((*self.loglines).logline) EQ fname)
  ENDIF ELSE index = 0

  IF N_Elements(index) GT 1 THEN BEGIN
    result = Dialog_Message('More than one log line with the same file name, using most recent.')
    RETURN, index[N_Elements(index) - 1]
  ENDIF

  RETURN, index

END

PRO as_scatterXMLFile::SwitchIBSIT, CONFIGNAME = configName

  @as_scatterheader.macro

  IF ~KeyWord_Set(CONFIGNAME) THEN config = 0 ELSE BEGIN
    config = Where((*self.configurations).name EQ configName)
  ENDELSE

  itAttr = StrUpCase((*self.configurations)[config].Transmission)
  ibsAttr = StrUpCase((*self.configurations)[config].Beamstop)
  
  names = Tag_Names(*self.loglines)
  itPos = Where(names EQ itAttr)
  ibsPos = Where(names EQ ibsAttr)
  
  it = Long(((*self.loglines)).(itPos))
  ibs = Long(((*self.loglines)).(ibsPos))
  
  (((*self.loglines)).(itPos)) = ibs
  (((*self.loglines)).(ibsPos)) = it

END

FUNCTION as_scatterXMLFile::GetType, index

  RETURN, ((*self.loglines))[index].type

END

FUNCTION as_scatterXMLFile::GetScale, index, I0 = I0, IBS = IBS, IT = IT, TIME = time, STAMP=timestamp, CONFIGNUM = config

  @as_scatterheader.macro
  
  IF ~KeyWord_Set(config) THEN config = 0
  
  IF N_Elements(*self.loglines) EQ 0 THEN BEGIN
    
    I0 = 1
    IBS = 1
    IT = 1
    time = 1
    timestamp = ''
    RETURN, 1
    
  ENDIF
  
  time = ((*self.loglines))[index].exptime
  
  i0Attr = StrUpCase((*self.configurations)[config].Incident) 
  itAttr = StrUpCase((*self.configurations)[config].Transmission)
  ibsAttr = StrUpCase((*self.configurations)[config].Beamstop)
  
  names = Tag_Names(*self.loglines)
  i0Pos = Where(names EQ i0Attr)
  itPos = Where(names EQ itAttr)
  ibsPos = Where(names EQ ibsAttr)
  
  i0 = Long(((*self.loglines))[index].(i0Pos)); - (*self.i0bgcounts)[index] * time
  it = Long(((*self.loglines))[index].(itPos)); - (*self.itbgcounts)[index] * time
  ibs = Long(((*self.loglines))[index].(ibsPos)); - (*self.ibsbgcounts)[index] * time

  timestamp = ((*self.loglines))[index].timestamp
  ;FOR i = 0, 4 DO timestamp = timestamp + (*self.timestamp)[i,index] + ' '
  
  RETURN, Float(it)/Float(i0)

END

PRO as_scatterXMLFile__define

  void = { as_scatterXMLFile, $
           INHERITS as_xmlparamfile, $
           fileVersionNumber  : '',        $
           xmlFileName        : '',        $
           experimentFile     : '',        $
           logFile            : '',        $
           dtd                : '',        $
           readDtd            : '',        $
           logDtd             : '',        $
           addedLines         : 0,         $
           addedGapless       : 0,         $
           gaplessProgress    : 0,         $
           configurations     : Ptr_New(), $
           attConfigurations  : Ptr_New(), $
           detectorDefs       : Ptr_New(), $
           attDetectorDefs    : Ptr_New(), $
           userMasks          : Ptr_New(), $
           attUserMasks       : Ptr_New(), $
           loglines           : Ptr_New(), $
           attLogline         : Ptr_New(), $
           pvs                : Ptr_New(), $
           attPVs             : Ptr_New(), $
           logPVs             : Ptr_New(), $
           notifyObject       : List(), $
           attrList           : Ptr_New(), $
           currentConfig      : 0}

END
