
;---------------------------------------------------------------------------
; Init method.
; Called when the AS_LogFileObj object is created.

FUNCTION AS_LogFileObj::Init

  @as_scatterheader.macro
 
    void =         {void, $
                INHERITS IDLffXMLSAX,  $
                charBuffer :   '' ,    $ 
                numFields     :   ''      $
               }
               
    self.numFields[0]       = N_Tags(void)

    void =      {AS_LogFileObj}
    self.numFields[1]       = N_Tags(void)
               
    FOR i = self.numFields[0], self.numFields[1] - 1 DO BEGIN
       self.(i) = PTR_NEW(/ALLOCATE_HEAP)
    END    
           
    RETURN, self->IDLffXMLSAX::Init()
END
;---------------------------------------------------------------------------

; Cleanup method.
; Called when the AS_LogFileObj object is destroyed.

PRO AS_LogFileObj::Cleanup

  @as_scatterheader.macro
  
   ; Release pointer
   
   FOR i = self.numFields[0], self.numFields[1] - 1 DO BEGIN
    IF Ptr_Valid(self.(i))      THEN Ptr_Free, self.(i)
   END    
  
    ; Call superclass cleanup method
   self->IDLffXMLSAX::Cleanup
END

;---------------------------------------------------------------------------

; StartDocument method for reading XML
; Called when parsing of the document data begins.
; If the arrays pointed contain data, reinitialise them.

PRO AS_LogFileObj::StartDocument

  @as_scatterheader.macro

   FOR i = self.numFields[0], self.numFields[1] - 1 DO BEGIN
      IF (N_Elements(*self.(i)) GT 0)      THEN void = Temporary(*self.(i))
   END    

END
    
;---------------------------------------------------------------------------
; Characters method for XML
; Called when parsing character data within an element.
; Adds data to the charBuffer field.

PRO AS_LogFileObj::characters, data
  
  @as_scatterheader.macro

   self.charBuffer = self.charBuffer + data
END

;---------------------------------------------------------------------------
; StartElement for XML
; Called when the parser encounters the start of an element.

PRO AS_LogFileObj::startElement, URI, local, strName, attName, attValue

  @as_scatterheader.macro
   
   void = { AS_LogFileObj }
   tags = Tag_Names(void)
   
   SWITCH strName OF
      "transmission" :
      "saxsshot"     : BEGIN
        IF N_Elements(attName) NE 0 THEN attName = StrUpCase(attName)
        self.charBuffer = ''
        IF N_Elements(attName) GT 0 THEN BEGIN
          IF N_Elements(*self.(self.numFields[0])) EQ 0 THEN BEGIN
            FOR i = self.numFields[0] + 1, self.numFields[1] - 1 DO BEGIN
              IF Where(attName EQ Tags[i]) NE -1 THEN *self.(i)  = attValue[Where(attName EQ Tags[i])] ELSE *self.(i) = ''
            ENDFOR
          ENDIF ELSE BEGIN
            FOR i = self.numFields[0] + 1, self.numFields[1] - 1 DO BEGIN
              IF Where(attName EQ Tags[i]) NE -1 THEN *self.(i)  = [*self.(i),attValue[Where(attName EQ Tags[i])]] ELSE *self.(i) = [*self.(i),'']
            ENDFOR
          ENDELSE
        ENDIF
      END
      "Experiment"   :
      "Sample"       : 
   ENDSWITCH
   
END

;---------------------------------------------------------------------------
; EndElement method for XML
; Called when the parser encounters the end of an element.

PRO AS_LogFileObj::EndElement, URI, Local, strName

  @as_scatterheader.macro

   CASE strName OF
      "Experiment":
      "Sample" :
      "transmission":  BEGIN
          IF (N_ELEMENTS(*self.fname) EQ 0) THEN $
            *self.fname = "transmission" $
         ; If the array pointed at by pArray contains data
         ; already, extend the array.
         ELSE $
            *self.fname = [*self.fname,"transmission"]
            
      END
      
      "saxsshot": BEGIN
              
         ; If the array pointed at by pArray has no elements,
         ; set it equal to the current data.
         IF (N_ELEMENTS(*self.fname) EQ 0) THEN $
            *self.fname = "saxsshot" $
         ; If the array pointed at by pArray contains data
         ; already, extend the array.
         ELSE $
            *self.fname = [*self.fname,"saxsshot"]
      END
   ENDCASE 

END

;---------------------------------------------------------------------------
; GetArray method for XML.
; Returns the current array stored internally. If
; no data is available, returns -1.

FUNCTION AS_LogFileObj::GetArray, attribute

@as_scatterheader.macro

attribute = StrUpCase(attribute)
void = { AS_LogFileObj }
tags = Tag_Names(void)

FOR i = self.numFields[0], self.numFields[1] - 1 DO BEGIN 
  IF tags(i) EQ attribute THEN IF (N_Elements(*self.(i)) GT 0) THEN RETURN, *self.(i)
ENDFOR
  
END

;FUNCTION As_LogFileObj::NewLine, data


;---------------------------------------------------------------------------

PRO AS_LogFileObj__Define
   
   void = {AS_LogFileObj , $
               INHERITS IDLffXMLSAX,   $
               charBuffer :   ''   ,   $
               numFields  : [0,0],     $
               fname      : Ptr_New(), $
               exptime    : Ptr_New(), $
               i0counts   : Ptr_New(), $
               i0bgcounts : Ptr_New(), $
               itcounts   : Ptr_New(), $
               itbgcounts : Ptr_New(), $
               ibscounts  : Ptr_New(), $
               ibsbgcounts: Ptr_New(), $
               timestamp  : Ptr_New(), $
               opticstr   : Ptr_New(), $
               sdata      : Ptr_New()  $
                 }
END