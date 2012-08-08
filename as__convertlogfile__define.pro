FUNCTION AS__ConvertLogFile::INIT

  self.logTemplate = { LOGTEMPLATE, $                ; define log file ascii template for 'read_ascii'
        version:         1.0,       $
        dataStart:       long(0),   $
        delimiter:       byte(32),  $
        missingValue:    "",        $
        commentSymbol:   '/#',      $
        fieldCount:      long([22]),$
        fieldTypes:      [7,4,3,3,3,3,3,3,7,7,7,7,7,7,7,7,7,7,7,7,7,7], $
        fieldNames:      ['fname','exptime','i0counts','i0bgcounts','itcounts','itbgcounts' $
                         ,'ibscounts','ibsbgcounts','timestamp','t2','t3','t4','t5','opticstr' $
                         ,'sdata','s1','s2','s3','s4','s5','s6','s7'], $
        fieldLocations:  long([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]), $ ; dummy vector
        fieldGroups:     [0,1,2,3,4,5,6,7,8,8,8,8,8,9,10,10,10,10,10,10,10,10] $            ; grouping vector
        }

  RETURN, 1

END

FUNCTION AS__ConvertLogFile::readSAXS15LogFile, logFileName

  log = Read_ASCII(logFileName, TEMPLATE = self.logtemplate)
   
  RETURN, log

END

PRO AS__ConvertLogFile__Define

  logTemplate = { LOGTEMPLATE, $                ; define log file ascii template for 'read_ascii'
        version:         1.0,       $
        dataStart:       long(0),   $
        delimiter:       byte(32),  $
        missingValue:    "",        $
        commentSymbol:   '/#',      $
        fieldCount:      long([22]),$
        fieldTypes:      [7,4,3,3,3,3,3,3,7,7,7,7,7,7,7,7,7,7,7,7,7,7], $
        fieldNames:      ['fname','exptime','i0counts','i0bgcounts','itcounts','itbgcounts' $
                         ,'ibscounts','ibsbgcounts','timestamp','t2','t3','t4','t5','opticstr' $
                         ,'sdata','s1','s2','s3','s4','s5','s6','s7'], $
        fieldLocations:  long([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]), $ ; dummy vector
        fieldGroups:     [0,1,2,3,4,5,6,7,8,8,8,8,8,9,10,10,10,10,10,10,10,10] $            ; grouping vector
        }

  void = {AS__ConvertLogFile, $
           filename : '', $
           logtemplate : logtemplate $
         }

END