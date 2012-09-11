FUNCTION as__convertsaxs15toscatterbrain::init

  @as_scatterheader.macro

  RETURN, 1

END

FUNCTION as__convertsaxs15toscatterbrain::convert, SAXFile, logFile, xmlFile

  @as_scatterheader.macro
  
  CATCH, errorStatus
  
  IF errorStatus GT 0 THEN BEGIN
    CATCH, /CANCEL
    RETURN, -1
  ENDIF

  IF ~Arg_Present(SAXFile) THEN SAXFile = ''
  IF ~Arg_Present(logFile) THEN logFile = ''
  IF File_Test(SAXFile) NE 1 THEN SAXFile = Dialog_Pickfile(TITLE = 'Select valid SAX file.', GET_PATH = path, /MUST_EXIST, FILTER = '*.sax', /READ)
  IF File_Test(SAXFile) NE 1 THEN RETURN, -1
  IF File_Test(logFile) NE 1 THEN logFile = Dialog_Pickfile(TITLE = 'Select SAXS15ID log file.', GET_PATH = path, PATH = path, /MUST_EXIST, FILTER = '*.log', /READ)
  IF File_Test(logFile) NE 1 THEN RETURN, -1
  IF ~Arg_Present(xmlFile) THEN xmlFile = ''
  IF xmlFile EQ '' THEN xmlFile = Dialog_Pickfile(TITLE = 'File name for export.', FILTER = '*.xml', DEFAULT_EXTENSION = 'xml', PATH = path, /WRITE)
  IF xmlFile EQ '' THEN RETURN, -1

  SAXConvertObj = as__convertsaxfile()
  SAXConvertObj.convert, SAXFile, XMLOBJECT = XMLObject
  
  logConvertObj = as__convertlogfile()
  log = logConvertObj.readSAXS15LogFile(logFile)
  
  XMLObject.SetParameters, counterDefs = {beamstop : 'Ibs', transmission : 'It', incident : 'I0'}
  XMLObject.RedefineLogLine, {LOGLINE: '',COMMENT:'', ATTACHMENT:'', TYPE:'', TIMESTAMP : '', EXPTIME : '', I0 : '', Ibs: '', It : ''}
    
  transmission = !NULL  
    
  FOREACH fname, log.fname, key DO BEGIN
    transTemp = log.itcounts[key] - log.itbgcounts[key]
    IF StrUpCase(fname) EQ 'TRANSMISSION' THEN BEGIN
      transmission = transTemp
      CONTINUE
    ENDIF
    IF transmission NE !NULL THEN transTemp = transmission
    XMLObject.NewLogLine, fname, log.exptime[key], log.i0counts[key]-log.i0bgcounts[key], transTemp, log.ibscounts[key]-log.ibsbgcounts[key], TIMESTAMP = StrJoin(log.timestamp[*,key], ' '), TYPE = 'RAW'
  ENDFOREACH
  
  XMLObject.saveFile, xmlFile;, /EMPTY  

  RETURN, 1

END


PRO as__convertsaxs15toscatterbrain__define

  void = {as__convertsaxs15toscatterbrain, $
          test : 1 $
          }

END