FUNCTION AS_XMLParamFile::INIT

  @as_scatterheader.macro

  initXML = self->IDLffXMLDOMDocument::Init()
  initXML = initXML < self->IDLffXMLSAX::Init()
  self.currentAttStruct = Ptr_New(/ALLOCATE_HEAP)
  self.currentStruct = Ptr_New(/ALLOCATE_HEAP)
  self.structArray = Ptr_New(/ALLOCATE_HEAP)
    
  IF initXML NE 1 THEN RETURN, 0
  
  RETURN, 1

END

PRO AS_XMLParamFile::Load, FILENAME=filename

  @as_scatterheader.macro

  self->IDLffXMLDOMDocument::Load, FILENAME = filename
  self.base = self->GetDocumentElement()

END

PRO AS_XMLParamFile::New, baseElement, DTD = DTD, FILEVERSION = fileVersion

  @as_scatterheader.macro
  
  IF Keyword_Set(DTD) THEN BEGIN
    IF ~StrMatch(dtd, '*<?xml*') THEN DTD = '<?xml version="1.0" encoding="UTF-8"?>'+DTD
    self->IDLffXMLDOMDocument::Load, STRING = DTD
    self.base = self->GetDocumentElement()
  ENDIF

  IF Obj_Valid(self.base) THEN BEGIN
    nodeList = self.base->GetChildNodes()
    FOR i = 0, nodeList->GetLength() - 1 DO void = self.base->RemoveChild(nodeList->Item(0))
  ENDIF ELSE BEGIN
    self.base = self->CreateElement(baseElement) 
    void = self->AppendChild(self.base)
  ENDELSE
  
  IF N_Elements(fileVersion) THEN self->AddAttribute, self.base, 'FileVersion', fileVersion

END

FUNCTION AS_XMLParamFile::AddElement, parent, Child, Text

  @as_scatterheader.macro

  IF Size(parent, /TYPE) EQ 7 THEN BEGIN
    IF STRUPCASE(parent) EQ 'BASE' THEN parent = self.base
  ENDIF
  element = self->CreateElement(Child)
  textObj = self->CreateTextNode(text)
  void = element->AppendChild(textObj)
  void = parent->AppendChild(element)
  RETURN, element

END

PRO AS_XMLParamFile::AddAttribute, Parent, attribute, value

  @as_scatterheader.macro

  parent->SetAttribute, Attribute, value
  
END

PRO AS_XMLParamFile::Save, FILENAME = fileName, _REF_Extra=extra

  @as_scatterheader.macro

  CATCH, Error_Status
  
  IF Error_Status EQ 0 THEN BEGIN
    self->IDLffXMLDOMDocument::Save, FILENAME=fileName, _extra=extra, /PRETTY_PRINT
  ENDIF ELSE BEGIN
    print, !error_state.msg
    result = Dialog_Message('Error encountered saving experiment file.')
  ENDELSE

END

PRO AS_XMLParamFile::NewFromStruct, baseElement, STRUCT=struct, ATTSTRUCT=attStruct, APPENDTO=appendTo

  @as_scatterheader.macro

  arraySize = N_Elements(struct)
  IF N_Elements(struct) EQ 0 THEN RETURN
  names = StrUpCase(Tag_Names(struct))
  elemNames = StrUpCase(Tag_Names(attStruct))
  IF ~Keyword_Set(appendTo) THEN BEGIN
    self->New, baseElement
    appendToElement = 'base'
  ENDIF ELSE appendToElement = appendTo
  progress = 0
  time = systime(/SECONDS)
  IF arraySize GT 1000 THEN BEGIN
    progressBarObj = Obj_New('progressBar',/FAST,/NOCANCEL,/START,TEXT='Creating XML...')
  ENDIF
  FOR array = 0, arraySize - 1 DO BEGIN
    IF KeyWord_Set(struct) THEN BEGIN
      FOR i = 0, N_Elements(names) - 1 DO BEGIN
        elemNo = Where(elemNames EQ names[i])
        IF elemNo GE 0 THEN BEGIN
          curElemNo = elemNo
          IF i EQ 0 THEN element = self->AddElement(appendToElement, names[0], (struct.(0))[array]) $ 
                    ELSE BEGIN
                      IF ~Keyword_Set(appendTo) THEN nodeList = self.base->GetChildNodes() ELSE nodeList = appendToElement->GetChildNodes()
                      last = 0
                      index = nodeList->GetLength()
                      WHILE last NE 1 DO BEGIN
                        temp = nodeList->Item(index)
                        IF Obj_Class(temp) EQ 'IDLFFXMLDOMELEMENT' THEN BEGIN
                          offset = index
                          last = 1
                        ENDIF 
                        index -= 1
                      ENDWHILE
                      element = self->AddElement(nodeList->Item(offset), names[i], (struct.(i))[array])
                    ENDELSE
        ENDIF; ELSE BEGIN
        IF Size(attStruct.(curElemNo),/TYPE) EQ 10 THEN attributes = *(attStruct.(curElemNo))[0] ELSE attributes = attStruct.(curElemNo)
        FOR j = 0, N_Elements(attributes) -1 DO BEGIN
          k = Where(names EQ StrUpCase((attributes)[j]))
          IF k GE 0 THEN self->AddAttribute, element, names[k[0]], (struct.(k[0]))[array]
        ENDFOR
;      ENDELSE
      ENDFOR
    ENDIF
    IF Obj_Valid(progressBarObj) AND (progressTemp = 100*(array/Float(arraySize))) - progress GT 1 THEN BEGIN
      progressBarObj->Update, (progress = progressTemp), TEXT = 'Approx. Time Remaining: ' + StrCompress(String((-time + (time = Systime(/SECONDS)))*(100-progressTemp), FORMAT = '(I6)')) + ' s.' 
    ENDIF
  ENDFOR
  IF Obj_Valid(progressBarObj) THEN progressBarObj->Destroy
END

PRO AS_XMLParamFile::Cleanup

  @as_scatterheader.macro

  self->IDLffXMLSAX::Cleanup
  self->IDLffXMLDOMDocument::Cleanup

  IF Ptr_Valid(self.currentStruct) THEN Ptr_Free, self.currentStruct
  IF Ptr_Valid(self.structArray) THEN Ptr_Free, self.structArray
  IF Ptr_Valid(self.currentAttStruct) THEN Ptr_Free, self.currentAttStruct
  IF Obj_Valid(self.base) THEN Obj_Destroy, self.base



END

;***********************************************************
; Following methods for Open and parse XML using SAX
;***********************************************************

PRO AS_XMLParamFile::ParseFile, fileName, STRUCT=struct, ATTSTRUCT=attStruct, XML_STRING=xml_string

  @as_scatterheader.macro

  IF KeyWord_Set(struct) THEN BEGIN
    IF Ptr_Valid(self.currentStruct) THEN *self.currentStruct = struct[0] ELSE self.currentStruct = Ptr_New(struct[0])
  ENDIF
  
  IF KeyWord_Set(attStruct) THEN BEGIN
    IF Ptr_Valid(self.currentAttStruct) THEN *self.currentAttStruct = attStruct ELSE self.currentAttStruct = Ptr_New(attStruct[0])
  ENDIF
  
  Ptr_Free, self.structArray
  self.structArray = Ptr_New(/ALLOCATE_HEAP)
  
  CATCH, Error_Status
  
  IF Error_Status EQ 0 THEN BEGIN
    self->IDLffXMLSAX::ParseFile, fileName, XML_STRING = xml_string
    IF N_Elements(*self.structArray) GT 0 THEN *self.structArray = (*self.structArray)[0:self.row-1] 
  ENDIF ELSE BEGIN
    *self.structArray = -1
    print, !error_state.msg
    result = Dialog_Message('Error encountered parseing file.')
  ENDELSE
  
END

PRO AS_XMLParamFile::characters, data

  @as_scatterheader.macro

  self.charBuffer = self.charBuffer + data
  
END

PRO AS_XMLParamFile::StartDocument

  @as_scatterheader.macro

  IF (N_Elements(*self.currentStruct) GT 0) THEN BEGIN
    name = Tag_Names(*self.currentStruct,/STRUCTURE)
    IF name NE '' THEN *self.currentStruct = Create_Struct(NAME=Tag_Names(*self.currentStruct,/STRUCTURE)) ELSE BEGIN
      names = Tag_Names(*self.currentStruct)
      tempStruct = Create_Struct(names[0],'')
      FOR i = 1, N_Elements(names) - 1 DO tempStruct = Create_Struct(tempStruct, names[i],'')
      *self.currentStruct = tempStruct
    ENDELSE
  ENDIF 
  self.row = 0
END

PRO AS_XMLParamFile::startElement, URI, local, strName, attrName, attrValue

  @as_scatterheader.macro

  IF N_Elements(self.charBuffer) GT 0 THEN BEGIN
    self.charBufferTemp = [strName,self.charBuffer]
  ENDIF 
 
  self.charBuffer = ''

  IF Size(*self.currentStruct,/TYPE) NE 8 OR Size(*self.currentAttStruct,/TYPE) NE 8 THEN RETURN
  
  IF KeyWord_Set(attrName) THEN attrName = StrUpCase(attrName)
  strName = StrUpCase(strName)
  names = StrUpCase(Tag_Names(*self.currentStruct))
  strNames = StrUpCase(Tag_Names(*self.currentAttStruct))
  structName = StrUpCase(Tag_Names(*self.currentStruct,/STRUCTURE_NAME))

  IF strName EQ names[0] THEN BEGIN
    IF structName NE '' THEN *self.currentStruct = Create_Struct(NAME=structName) ELSE BEGIN
      tagNames = Tag_Names(*self.currentStruct)
      tempStruct = Create_Struct(tagNames[0],'')
      FOR i = 1, N_Elements(tagNames) - 1 DO tempStruct = Create_Struct(tempStruct, tagNames[i],'')
      *self.currentStruct = tempStruct
    ENDELSE
  ENDIF
  IF Where(strNames EQ strName) GE 0 THEN BEGIN
      FOR i = 0, N_Elements(attrName)-1 DO BEGIN
        att = Where(names EQ attrName[i])
        IF att GE 0 THEN (*self.currentStruct).(att) = attrValue[i]
      ENDFOR
  ENDIF

END

PRO AS_XMLParamFile::IgnorableWhitespace, chars

  @as_scatterheader.macro

  self.charBuffer = StrCompress(self.charBuffer)

END

PRO AS_XMLParamFile::endElement, URI, local, strName

  @as_scatterheader.macro

  IF Size(*self.currentStruct,/TYPE) NE 8 OR Size(*self.currentAttStruct,/TYPE) NE 8 THEN RETURN


  ;print, strName + self.charBuffer
  strName = StrUpCase(strName)
  strNames = StrUpCase(Tag_Names(*self.currentAttStruct))
  names = StrUpCase(Tag_Names(*self.currentStruct))

  elemNo = Where(names EQ strName)

  IF elemNo GE 0 THEN (*self.currentStruct).(elemNo) = StrTrim(StrJoin(StrSplit(self.charBuffer, String(10b),/EXTRACT))) $
                 ELSE IF Ptr_Valid(self.miscStruct) THEN BEGIN
                                                      miscNames = StrUpCase(Tag_Names(*self.miscStruct))
                                                      IF Where(strName EQ miscNames) EQ -1 THEN *self.miscStruct = Create_Struct(strName,StrTrim(StrJoin(StrSplit(self.charBuffer, String(10b),/EXTRACT))),*self.miscStruct)
                                                    ENDIF ELSE self.miscStruct = Ptr_New(Create_Struct(strName,StrTrim(StrJoin(StrSplit(self.charBuffer, String(10b),/EXTRACT)))))

;  IF elemNo GE 0 THEN (*self.currentStruct).(elemNo) = self.charBuffer $
;                 ELSE IF Ptr_Valid(self.miscStruct) THEN BEGIN
;                                                      miscNames = StrUpCase(Tag_Names(*self.miscStruct))
;                                                      IF Where(strName EQ miscNames) EQ -1 THEN *self.miscStruct = Create_Struct(strName,self.charBuffer,*self.miscStruct)
;                                                    ENDIF ELSE self.miscStruct = Ptr_New(Create_Struct(strName,self.charBuffer))
  
  IF strName EQ names[0] THEN BEGIN
    IF N_Elements(*self.structArray) EQ 0 THEN BEGIN
      *self.structArray = Replicate(*self.currentStruct,1000)
      (*self.structArray)[self.row] = *self.currentStruct
    ENDIF ELSE BEGIN
      IF self.row MOD 1000 EQ 0 THEN *self.structArray = [*self.structArray, Replicate(*self.currentStruct,1000)]
      (*self.structArray)[self.row] = *self.currentStruct
      ;*self.structArray = [*self.structArray, *self.currentStruct]
    ENDELSE
    self.row = self.row + 1
  ENDIF
  
  IF N_Elements(self.charBufferTemp) GT 0 AND strName EQ self.charBufferTemp[0] THEN BEGIN
    self.charBuffer = self.charBufferTemp[1]
    self.charBufferTemp = ['','']
  ENDIF ELSE self.charBuffer = ''
        
END

FUNCTION AS_XMLParamFile::GetParams

  @as_scatterheader.macro

  RETURN, *self.structArray

END

;***********************************************
; Class Definition
;***********************************************

PRO AS_XMLParamFile__define

void = { AS_XMLParamFile, $
        INHERITS IDL_Object, $
        INHERITS IDLffXMLDOMDocument, $
        INHERITS IDLffXMLSAX, $
        charBuffer : '', $
        charBufferTemp : ['',''], $
        miscStruct : Ptr_New(), $
        currentStruct : Ptr_New(), $
        currentAttStruct : Ptr_New(), $
        structArray : Ptr_New(),$
        ;XMLfileName : '',  $
        base : Obj_New(), $
        row  : 0 $
        }

END