;*******************************************************************************
;   saxs15ID  -  Version 3.29 Display and control program for SAXS on 15-ID-D
;
;
;   This widget driven IDL program allows relatively painless acquistion, viewing
;   and manipulation of raw SAXS data
;
;   Written by David J.Cookson (DJC) Australian Synchrotron Research Program September 2002-2008
;                                    Australian Synchrotron 2008-
;   Development continued by Stephen T. Mudie (STM) Australian Synchrotron 2008-
;*******************************************************************************
;
;+
; NAME:
;   SAXS15ID_EVENT / SAXS15ID
;
; VERSION:
;   3.29
;
; PURPOSE:
;   Main GUI for Acquisition, manipulation and visualization of SAXS data on 15-ID-D
;
; CATEGORY:
;   GUI window - setup and event processing
;
; CALLING SEQUENCE:
;   Type:  "scatterbrain" at the IDL command line
;
; INPUTS:
;   None
;
; OPTIONAL INPUT PARAMETERS:
;   None
;
; KEYWORD PARAMETERS:
;   EPICS       If set, saxs15id will be in full EPICS control mode
;
;   AUTOPROC    If set, an 'Auto Process' button will appear on front of main
;               screen - but only in analysis mode (/EPICS, /DEBUG or /QDEBUG not set)
;
;   DEBUG       If set, this mimics full EPICS contol mode with all EPICS calls
;               replaced with a popup window
;
;   QDEBUG      If set, this gives similar behaviour as for /DEBUG but with more
;               paraphrasing of multiple EPICS calls
;
;   SAVEDATA    Switch. If set, saxs15id will NOT initialize and clear out all the data
;               that was current when the saxs15id GUI was terminated.  This way you can
;               exit the main GUI and keep the data when you come back in - if you exit IDL
;               completely, then of course all the data is lost
;
;   NODET       Switch. If set, saxs15id will not attempt to send commands to the CCD
;               detector this is useful especially in Debug mode
;
; OUTPUTS:
;   All output file generation (profiles, parameters and postscript files)
;   is driven from GUI screens selected from the main menu bar
;
; OPTIONAL OUTPUT PARAMETERS:
;   None
;
; COMMON BLOCKS:
;   None
;
; RESTRICTIONS:
;   Unless using this to control the SAXS camera the "self.pollEpics" variable
;   in the CONTROL_DATA common block should be set to zero.
;
; PROCEDURE:
;   Straightforward.  Use from the IDL control line - better still compile for VM 6.0
;
; MODIFICATION HISTORY:
;   15-SEP-2002 Created by David J. Cookson (DJC)
;   28-DEC-2002 (DJC)  Major GUI changes
;   20-APR-2003 (DJC)  Allows control of SAXS camera
;   10-JUN-2003 (DJC)  Added multi-sector processing option
;   20-OCT-2003 (DJC)  Added the ability to process Princeton format files
;    8-NOV-2003 (DJC)  Added DEBUG mode to allow simultated EPICS operation
;   03-MAR-2004 (DJC)  Added whole new acquire/move capability
;   12-APR-2004 (DJC)  Added new window for absolute calibration
;   16-APR-2004 (DJC)  Major addition to main GUI in control mode - SAXS logger
;   02-JUN-2004 (DJC)  Added the ability to make an exposure script for each sample position
;   12-SEP-2004 (DJC)  Large additions to accommodate GiSAXS, ASAXS and SAXS mapping
;   12-DEC-2004 (DJC)  Spatial correction capability added
;   10-FEB-2005 (DJC)  Lots of bugs fixed in run-time and analysis version
;   13-MAY-2005 (DJC)  Added new window for output of single and mosaic images
;   20-JAN-2006 (DJC)  Added new 'Auto Process' Button to main window
;   17-AUG-2007 (DJC)  Now two choices for main window GUI layout
;   12-May-2008 (STM)  Added ability to resize base, and have draw widgets resize.
;   19-May-2008 (STM)  Added DEFAULT_EXTENSION='txt' to dialog_pickfile for save profile summary. This insures
;                      file is save to correct position when there is a full stop in the path.
;-


PRO scatterBrain_Quit, event
 
  @as_scatterheader.macro
  
 ; ** FILE -> Exit
          WIDGET_CONTROL, Event.top, /DESTROY
END
 
PRO scatterBrain_Cleanup, baseID

  @as_scatterheader.macro
  
  Widget_Control, baseID, GET_UVALUE = scatterBrain
  Obj_Destroy, scatterBrain
END

PRO scatterbrain_realize, wBaseCntl
  
  @as_scatterheader.macro
  
  id = Widget_Info(wBaseCntl, FIND_BY_UNAME='SAXS_BASE')
  Widget_Control, id, GET_UVALUE=scatterBrain
  scatterBrain->realise, wBaseCntl
END

PRO scatterbrain::realise, wBaseCntl

  @as_scatterheader.macro

    IF self.pollEpics GT 0 THEN BEGIN

        WIDGET_CONTROL, Widget_Info(self.wBaseCntl, FIND_BY_UNAME='TEXP') $
                            , SET_COMBOBOX_SELECT = batchdata.itexp

        WIDGET_CONTROL, Widget_Info(self.wBaseCntl, FIND_BY_UNAME='LOGLINECOUNT') $
                                , set_value = string(n_elements(log_data.fname[*]), format='(I4)')
        IF N_Tags(log_data) EQ 0 THEN BEGIN
        
            WIDGET_CONTROL, Widget_Info(self.wBaseCntl, FIND_BY_UNAME='LOGTEXT') $
                          , set_value = 'No log data in memory', /append, set_text_top_line=2000
        ENDIF
    ENDIF

END

PRO scatterbrain_event, event

  @as_scatterheader.macro

  ;id = Widget_Info(event.top, FIND_BY_UNAME='SAXS_BASE')
  Widget_Control, event.top, GET_UVALUE=scatterBrain
  scatterBrain->event, event
END

PRO scatterbrain::event, event

    ;  Check to see that the widgets returning an event are not one of the few that do not
    ;  have a valid 'value' attribute - if they belong to this group, only recover the
    ;  'uvalue' parameter

  COMPILE_OPT idl2
  ;ON_ERROR, 2
  
  CATCH, theError
  IF theError NE 0 THEN BEGIN
    CATCH, Cancel=1
    void = Error_Message(/Traceback)
    RETURN
  ENDIF


    type = Tag_Names(event,/structure_name)
    
    if (type NE 'WIDGET_TIMER') AND (type NE 'WIDGET_COMBOBOX') AND (type NE 'WIDGET_TAB') AND (type NE 'WIDGET_BASE') AND (type NE 'WIDGET_KILL_REQUEST') $ 
                                AND (type NE 'WIDGET_TREE_SEL') AND (type NE 'DRAWBUTTON_EVENT') AND (type NE 'FSC_INPUTFIELD_EVENT') then $
                                    WIDGET_CONTROL, event.id, GET_VALUE=val

    IF type EQ 'FSC_INPUTFIELD_EVENT' THEN BEGIN
      event.ObjRef.GetProperty, NAME=widgetName
    ENDIF ELSE widgetName = Widget_Info(event.ID,/UNAME)
    
    IF StrMid(widgetName,0,12) EQ 'RECENT_FILE_' THEN widgetName = 'RECENT_FILE'
    
    CASE widgetName OF

        ; ***********  BASE EVENTS      ********************************************************
        
      'SAXS_BASE' :  BEGIN 
                        
                       ;Resize the frame draw window. Add the minimum change in size to keep the aspect ratio constant.

                       newXSize = event.x-self.aux_base_size[0]
                       newYSize = event.y-self.aux_base_size[1]
                       self.frame_obj->ReSize, DIM=[newXSize, newYSize]
                       self.frame_obj->GetProperty, HEIGHT=height
                       IF height GT 0 THEN self.scatterXMLGUI_obj->SetProperty, HEIGHT = height + 110
                   
                      ; Resize the message box.
                      Widget_Control, Widget_Info(self.wScatterBase, FIND_BY_UNAME='MESBOX'), GET_UVALUE=nonMesboxSize
                      Widget_Control, Widget_Info(self.wScatterBase, FIND_BY_UNAME='MESBOX'), XSIZE = event.x - nonMesboxSize
      END
        
        
        ; ***********  MENU BAR EVENTS  ********************************************************

        ; ** FILE MENU **

        ; ** FILE -> Process SAXS Image

        ; ** FILE -> Process SAXS Image -> Single Image

        ; read, integrate, normalize and display a 1-D SAXS profile from a 2D image file
        'OPEN IMAGES': begin
;            dir = self.dir
;            fext2 = self.fext2
;            f_name1 = as_saxs_get_filenames(self.fext, /single, FEXT2=fext2, DIR=dir)
;            IF fext2 THEN self.fext2 = fext2
;            IF dir THEN self.dir = dir
;            
            fName = Dialog_Pickfile(PATH = self.imagesDir, /READ, /MUST_EXIST, GET_PATH = filePath)
            
            IF fName EQ '' THEN BREAK
            ;retval = saxs_image_proc(f_name1, INFO = info)
            ;saxs_display_update, INFO=info
            fName = File_BaseName(fName)
            self.frame_obj.GetProperty, IMAGEPATH = imagePath
            self.frame_obj.SetProperty, PATH = filePath  
            return = self.ProcessImage(fName)
            self.frame_obj.SetProperty, PATH = imagePath
                        
            
        END

        'RECENT_FILE' : BEGIN
                          Widget_Control, event.id, GET_UVALUE = recentFile
                          CD, File_DirName(recentFile)
                          self.loadXML, recentFile
                        END

        ; ** FILE -> Create New Experiment - template. So no reference to previous experiment.
        'NEW EXPERIMENT TEMPLATE' : BEGIN
                                   xmlFiles = File_Search(self.settingsObj.settingsPath + Path_Sep() + '*.xml')
                                   IF N_Elements(xmlFiles) GT 0 THEN BEGIN
                                     templateList = List()
                                     DOM = IDLffXMLDOMDocument()
                                     FOREACH xmlFile, xmlFiles DO BEGIN
                                       DOM.load, FILENAME = xmlFile
                                       IF (DOM.GetDocumentElement()).GetTagName() EQ 'scatterBrain' THEN templateList.add, File_BaseName(xmlFile, '.xml')
                                     ENDFOREACH
                                   ENDIF
                                   
                                   IF Obj_Valid(self.frame_obj2) THEN path2 = self.frame_obj2.frame.path ELSE path2 = ''
                                   
                                   path = [self.frame_obj.frame.path,path2]
                                   
                                   
                                   IF path[0] EQ '' OR path[0] EQ 'No Name Yet' THEN path[0] = self.settingsobj.startingDirectory1
                                   IF path[1] EQ '' OR path[1] EQ 'No Name Yet' THEN path[1] = self.settingsobj.startingDirectory2
                                   
                                   path = self.GetUserDir(path)
                                   
                                   newTemplate = as_newexperimentfromtemplate(templateList.toArray(), NOTIFYOBJ = notify('NewExperimentCallback',self), PATH = path)
                                   ;self.settingsObj.GetProperty, DETECTOR=detectors
                                   ;newExpDialog = as_newexperiment(DETECTORLIST = detectors, NOTIFY = notify('NewExperimentCallback', self))
                                 END

        ; ** FILE -> Create Empty Experiment From Current
        
        'NEW EXPERIMENT EMPTY' : BEGIN
                                   
                                   newExpDialog = as_newexperimentfromtemplate(['Current'],NOTIFY = notify('newXML', self, Hash('files',0)), PATH = self.GetUserDir())
                                   geom = Widget_Info(event.top, /GEOM)
                                   self.scatterXMLGUI_obj->SetProperty, HEIGHT = geom.scr_ysize*.9 
                                END

        'NEW EXPERIMENT FILES' : BEGIN
                                   newExpDialog = as_newexperimentfromtemplate(NOTIFY = notify('newXML', self, Hash('files',1)), PATH = self.GetUserDir())
                                   geom = Widget_Info(event.top, /GEOM)
                                   self.scatterXMLGUI_obj->SetProperty, HEIGHT = geom.scr_ysize*.9 
                                 END
                
        ; ** FILE -> Get XML File
        
        ; load the XML file containing the log and parameters
         
        'LOAD XML' :  self.loadXML
                     
        'SAVE XML' : self.saveXML 
                     
        'SAVE AS XML' : BEGIN
                          xmlFile = Dialog_Pickfile(filter = '*.xml', PATH = self.experimentDir, TITLE = 'Select XML File', /WRITE, /OVERWRITE_PROMPT)
                          IF xmlFile NE '' THEN BEGIN
                            IF self.pollEpics GT 0 THEN self.areaDetectorObj.StoreParams, self.scatterXMLGUI_obj
                            self.scatterXMLGUI_obj->SaveFile, xmlFile
                          ENDIF
                          widgetIDS = [Widget_Info(event.top, FIND_BY_UNAME = 'NEW EXPERIMENT EMPTY'),Widget_Info(event.top, FIND_BY_UNAME = 'NEW EXPERIMENT FILES')]
                          FOREACH widgetID, widgetIDS DO IF widgetID GT 0 THEN Widget_Control, widgetID, SENSITIVE = 1
                     END
         
        ; ** File -> Export Experiment file with selected
        'EXPORT EXPERIMENT FILES' : print, 'yes'    

        ; ** ACQUIRE MENU - Disabled in non-control mode **

        ; ** ACQUIRE -> Open Live Log File
        
        ; Open the live log file for the experiment.
        
        'ACQ_LIVELOG'  : BEGIN
        
          file = Dialog_Pickfile(PATH = self.imagesDir, /MUST_EXIST,filter = '*.log')
          self.liveLog = file 
          path = File_DirName(self.liveLog, /MARK_DIRECTORY)
          self.frame_obj.SetProperty, PATH=path
          
          IF self.pollEpics EQ 0 THEN BEGIN
            self.scatterXMLGUI_obj->ParseFile, FILENAME = file, /LOGONLY, /UPDATE
            IF file EQ 'error' THEN self.liveLog = ''
          ENDIF
        
        END
        
        'ACQ_POLLLIVELOG' : BEGIN
          IF Tag_Names(event, /STRUCTURE_NAME) EQ 'WIDGET_BUTTON' THEN BEGIN
            Widget_Control, event.id, SET_BUTTON = ~Widget_Info(event.id, /BUTTON_SET) 
          ENDIF
          IF Widget_Info(event.id, /BUTTON_SET) EQ 1 THEN BEGIN
            file = self.liveLog
            self.scatterXMLGUI_obj->ParseFile, FILENAME = file, /LOGONLY, /UPDATE
            IF file EQ 'error' THEN BEGIN
              self.liveLog = ''
              result = Dialog_Message('Invalid live log file, turning off polling. Reselect live log and then restart polling.')
              Widget_Control, event.id, SET_BUTTON = 0
            ENDIF ELSE Widget_Control, event.id, TIMER = 1
          ENDIF
        
        END

        'ACQ_EXPORTLUT' : self.ExportLUT

        'ACQ_EXPORTTEMPLATE' : BEGIN
                                 xmlFile = Dialog_Pickfile(filter = '*.xml', PATH = self.settingsObj.settingsPath, TITLE = 'Save Template File', /WRITE, /OVERWRITE_PROMPT)
                                 IF xmlFile NE '' THEN BEGIN
                                   IF self.pollEpics GT 0 THEN self.areaDetectorObj.StoreParams, self.scatterXMLGUI_obj
                                   self.scatterXMLGUI_obj->SaveFile, xmlFile, /EMPTY
                                 ENDIF
                               END
        
        ; ** ACQUIRE -> Data Path
        
        'DATA PATH' : BEGIN
        
          file = Dialog_Pickfile(/MUST_EXIST, /DIRECTORY)
          IF file NE '' THEN self.imagesDir = file
          self.frame_obj.SetProperty, PATH=file
        
        END
        
        ; ** ACQUIRE -> Data Path 2
        
        'DATA PATH 2' : BEGIN
        
          file = Dialog_Pickfile(/MUST_EXIST, /DIRECTORY)
          IF file EQ '' THEN BREAK
          ;IF file NE '' THEN self.filenames.dir = file
          self.frame_obj2.SetProperty, PATH=file

        END

        ; ** ACQUIRE -> areaDetector
        
        ; set up the areaDetector interface
        
        'ACQ_AREADETECTOR' : BEGIN
            
            IF ~Obj_Valid(self.areaDetectorObj) THEN BEGIN 
              self.saxsFile_obj->GetParameters,AREADETECTOR = areaDetector
              self.areaDetectorObj = Obj_New('as_areadetectormap', areaDetector)
            ENDIF 
            self.areaDetectorObj->ShowGui
           
        END
        'ACQ_PVMAP' : BEGIN
        
            IF Obj_Valid(self.saxscontrol) THEN self.saxscontrol->ShowGui
        
        END
        
        ; ** ACQUIRE -> Setup

;        ; set up the CCD detector interface and exposure script
;        'ACQ_SETUP' : BEGIN
;            saxs_acquire_setup, self.wScatterBase
;        END

        ; ** ACQUIRE -> User PVs

        ; set up the CCD detector interface and exposure script
;        'ACQ_USERMOTORS' : BEGIN
;            saxs_acquire_usermotors, self.wScatterBase
;        END

        ; ** SETTINGS MENU **
        ; ** SETTINGS -> Second Detector
        '2ND DETECTOR' : BEGIN
                           set = Widget_Info(event.id, /BUTTON_SET)
                           IF ~set THEN BEGIN
                             self.Use2ndDetector
                           ENDIF ELSE BEGIN
                             Widget_Control, event.id, SET_BUTTON = 0
                             Widget_Control, Widget_Info(event.top, FIND_BY_UNAME = 'DATA PATH 2'), SENSITIVE = 0
                             self.frame_obj2.GetProperty, GROUP_LEADER = groupLeader
                             Widget_Control, groupLeader, /DESTROY
                             Obj_Destroy, self.frame_obj2
                             Obj_Destroy, self.qData_obj2
                           ENDELSE
                         END
        
        ; ** SETTINGS -> General Settings
        'GENERAL SETTINGS' : BEGIN
                               IF ~Widget_Info(self.settingsBase, /VALID) THEN BEGIN
                                 self.settingsBase = Widget_Base(GROUP_LEADER=self.wScatterBase,TITLE='scatterBrain General Settings', EVENT_PRO='scatterbrain_event', /COLUMN, /TLB_KILL_REQUEST_EVENTS, UNAME = 'SETTINGS BASE')
                                 IF self.pollEpics EQ 1 THEN BEGIN
                                   nonExclusiveSettings = Widget_Base(self.settingsBase, /COLUMN,/NONEXCLUSIVE)
                                   resetFileNumber = Widget_Button(nonExclusiveSettings, VALUE = 'Auto Reset Filenumber', UNAME = 'RESET FILENUMBER')
                                   checkAllFilenameExist = Widget_Button(nonExclusiveSettings, VALUE = 'Check all files in image series for conflicts with existing files. Slow for large numbers of files.', UNAME = 'CHECK ALL FILE EXISTENCE')
                                   checkScanFilenamesExist = Widget_Button(nonExclusiveSettings, VALUE = 'Check filenames in scan mode for conflicts with existing files.', UNAME = 'CHECK SCAN FILES')
                                   Widget_Control, resetFileNumber, SET_BUTTON = self.resetFileNumber
                                 ENDIF
                                 zingerBase = Widget_Base(self.settingsBase, /COLUMN, /FRAME)
                                 zingerThreshold = CW_FSlider(zingerBase, TITLE = '', VALUE = 10000, MINIMUM = 0, MAXIMUM = 2^20., XSIZE = 200, /EDIT, UNAME = 'ZINGER THRESHOLD')
                                 zingerLabel = Widget_Text(zingerBase, FRAME = 0, VALUE = 'Zinger Threshold. This sets the value used by the zinger mask routine. You must run that routine after changing this value.', YSIZE = 3, /WRAP)
                                 row1 = widget_base(self.settingsbase, /ROW, /FRAME)
                                 self.frame_obj.GetProperty, STEP = step
                                 row2 = widget_base(self.settingsbase, /ROW, /FRAME)
                                                                  
                                 CASE !version.os_family OF
                                  'Windows' : BEGIN
                                                sectors = Widget_Slider(row1, TITLE = 'Number of Sectors          ', VALUE = 180, MINIMUM = 1, MAXIMUM = 360, /VERTICAL, YSIZE = 30, UNAME = 'NO. SECTORS')
                                                binSize = Widget_Slider(row2, TITLE = 'q-vector bin size          ', VALUE = step > 1, /VERTICAL, MINIMUM = 1, MAXIMUM = 10, YSIZE = 15, UNAME = 'Q BIN SIZE')
                                              END
                                   ELSE:      BEGIN
                                                sectors = Widget_Slider(row1, TITLE = 'Number of Sectors          ', VALUE = 180, MINIMUM = 1, MAXIMUM = 360, XSIZE = 150, YSIZE = 50, UNAME = 'NO. SECTORS')
                                                binSize = Widget_Slider(row2, TITLE = 'q-vector bin size          ', VALUE = step > 1, MINIMUM = 1, MAXIMUM = 10, XSIZE = 150, YSIZE = 50, UNAME = 'Q BIN SIZE')
                                              END
                                 ENDCASE
                                 
                                 row3 = Widget_Base(row2, /NONEXCLUSIVE)
                                 pseudoLog = Widget_Button(row3, VALUE = 'Pseudo Log', UNAME = 'PSEUDO LOG')
                                 Widget_Control, pseudoLog, SET_BUTTON = ~step
                                 Widget_Control, binSize, SENSITIVE = step
                                 ;binSize = CW_Field(self.settingsBase, TITLE = 'q-vector bin size', VALUE = step, /FLOATING, /RETURN_EVENTS, UNAME = 'Q BIN SIZE')
                                 errorBarsButtons = CW_BGROUP(self.settingsBase, ['None','Last Selected Only', 'All'],LABEL_TOP = 'Show Error Bars',/ROW,/EXCLUSIVE, UNAME = 'SHOW ERROR BARS')
                                 Widget_Control, errorBarsButtons, SET_VALUE=self.settingsObj.ErrorBars
                                 startingDirectory = Widget_Button(self.settingsBase, VALUE = 'Set Starting Directory', UNAME = 'STARTING DIRECTORY')
                                 nonExclusiveSettings2 = Widget_Base(self.settingsBase, /COLUMN,/NONEXCLUSIVE)
                                 autoCheckForUpdates = Widget_Button(nonExclusiveSettings2, VALUE = 'Check For Updates on start up?', UNAME = 'AUTO CHECK UPDATE')
                                 
                                 Widget_Control, autoCheckForUpdates, SET_BUTTON = self.settingsObj.autoCheckUpdates
                                                                  
                                 Widget_Control, self.settingsBase, SET_UVALUE = self
                                 Widget_Control, self.settingsBase, /REALIZE
                                 
                                 XManager, 'scatterbrain', self.settingsBase, /NO_BLOCK
                                 
                               ENDIF ELSE Widget_Control, self.settingsBase, MAP = 1
        END
        'SETTINGS BASE'    : BEGIN
                               IF type EQ 'WIDGET_KILL_REQUEST' THEN Widget_Control, self.settingsBase, MAP = 0 
                               
        END
        'RESET FILENUMBER' : BEGIN
                               self.resetFileNumber = event.select
        END
        'CHECK ALL FILE EXISTENCE' : BEGIN
                                       self.checkAllFilesExistence = event.select
        END
        'CHECK SCAN FILES' : BEGIN
                               self.checkScanFiles = event.select
        END
        'SET ZINGER MASK'  :BEGIN
                              IF Widget_Info(self.settingsBase, /VALID) THEN Widget_Control, Widget_Info(self.settingsBase, FIND_BY_UNAME = 'ZINGER THRESHOLD'), GET_VALUE = threshold $
                                                                        ELSE threshold = 1000000l
                              self.frame_obj.SetZingerPixMask, threshold
        END
        'ZINGER THRESHOLD' : BEGIN
        END
        'Q BIN SIZE'       : BEGIN
                               self.frame_obj.SetProperty, step = event.value
                               self.settingsObj.SetProperty, binsize = event.value
        END
        'PSEUDO LOG' : BEGIN
                         Widget_Control, Widget_Info(self.settingsBase, FIND_BY_UNAME = 'Q BIN SIZE'), GET_VALUE = step
                         IF event.select EQ 1 THEN step = 0
                         Widget_Control, Widget_Info(self.settingsBase, FIND_BY_UNAME = 'Q BIN SIZE'), SENSITIVE = ~event.select
                         self.frame_obj.SetProperty, step = step
                         self.settingsObj.SetProperty, binsize = step
                       END
        'STARTING DIRECTORY' : BEGIN
                                 startingDirectory = Dialog_Pickfile(/DIRECTORY)
                                 IF File_Test(startingDirectory, /DIRECTORY) THEN self.settingsObj.startingDirectory1 = startingDirectory
                               END
        'AUTO CHECK UPDATE' : BEGIN
                                self.settingsObj.autoCheckUpdates = event.select
                              END
        'SHOW ERROR BARS'  : BEGIN
                               self.settingsObj.errorBars = event.value
                               self.profiles_obj.showErrorPlot, event.value
                             END
        'NO. SECTORS'      :
         ; ** TOOLS MENU **
         ; ** TOOLS -> Export Current Image
         'EXPORT IMAGE' : BEGIN
           self.ExportCurrentImage, /RAW
         END
         'EXPORT IMAGE ALL' : BEGIN
           self.ExportCurrentImage
         END
         'DAT FILE MODE'    : BEGIN
           self.datfileobject = as_datfileloader(['Raw Dat','Average','Subtracted', 'Manual'],[self.experimentDir + Path_Sep() + 'raw_dat', self.experimentDir + Path_Sep() + 'avg', self.experimentDir + Path_Sep() + 'sub', self.experimentDir + Path_Sep() + 'manual'], GROUPLEADER = self.wScatterBase, NOTIFYOBJECT = notify('DatCallback', self))
         END                             
                             
        'CONVERT SAXS15'   : BEGIN
                               convertObj = as__convertsaxs15toscatterbrain()
                               xmlFile = ''
                               convertStatus = convertObj.convert('', '', xmlFile)
                               CASE convertStatus OF 
                                   1 : result = Dialog_Message('Files converted successfully. Experiment file at ' + xmlFile + '.') 
                                  -1 : result = Dialog_Message('File not converted.')
                               ENDCASE
        END
        'SWITCH IBS IT' :   BEGIN
                              self.frame_obj.GetProperty, CONFIGNAME = configName
                              self.scatterXMLGUI_obj.SwitchIbsIT, CONFIGNAME = configNames 
        END
        
        'EXPORT NATURE PAPER' :BEGIN
                                 wNatureBase = Widget_Base(GROUP_LEADER = event.top, TITLE = 'Export Nature Paper', /COLUMN)
                                 wLabel  = Widget_Text(wNatureBase, VALUE = 'To use this feature you are required to make an "in app" purchase. This feature is AUD500. Please enter credit card details below.', YSIZE = 5, /WRAP)
                                 name = FSC_Field(wNatureBase, TITLE = 'Card Holder')
                                 wCreditCardNoBase = Widget_Base(wNatureBase, /ROW)
                                 wNumberLabel = Widget_Label(wCreditCardNoBase, VALUE = 'Number')
                                 FOR i = 0, 3 DO BEGIN
                                   wText = Widget_Text(wCreditCardNoBase, /EDITABLE, XSIZE = 4)
                                   wBuffer = Widget_Label(wCreditCardNoBase, VALUE = '', XSIZE = 4)
                                 ENDFOR
                                 wCreditCardExpBase = Widget_Base(wNatureBase, /ROW)
                                 wExpLabel = Widget_Label(wCreditCardExpBase, VALUE = 'Expiry')
                                 wText = Widget_Text(wCreditCardExpBase, /EDITABLE, XSIZE = 4)
                                 wBuffer = Widget_Label(wCreditCardExpBase, VALUE = '/', XSIZE = 4)
                                 wText = Widget_Text(wCreditCardExpBase, /EDITABLE, XSIZE = 4)
                                 wSubmitButton = Widget_Button(wNatureBase, VALUE = 'Submit')
                                 Widget_Control, wNatureBase, /REALIZE
        END
        
        'EXPORT LOGFILE' : BEGIN
                                   self.scatterXMLGUI_obj.GetParameters, RAWLOG = rawLog
                                   exportLogfile = as_exportstruct(rawLog)
        
        END
        'EXPORT LOGFILE FILES' : BEGIN
                                   self.scatterXMLGUI_obj.GetParameters, RAWLOG = rawLog
                                   indicies = List()
                                   FOREACH name, self.currentlySelected DO indicies.add, self.scatterXMLGUI_obj.GetIndex(name)
                                   exportLogFile = as_exportstruct(rawLog[indicies.toArray()])
        END
        'TWO DIMENSIONAL SUBTRACTION' : BEGIN
                                          twoDSubtract = as_twodsubtract(GROUP_LEADER = self.wScatterBase, NOTIFYOBJECT = notify('TwoDSubtract',self))
         END
        
        ; ** SCAN MENU **
        ; ** SCAN -> Mode
        'SCAN_MODE' : BEGIN
                        ;TODO In future I'll have to talk to all detectors to give them correct template.
                        detID = 0 
                        self.scanMode = ~self.scanMode
                        Widget_Control, event.id, SET_BUTTON = self.scanMode
                        Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='START'), SENSITIVE = 1
                        IF self.scanMode THEN BEGIN
                          IF ~Obj_Valid(self.excelObj) THEN self.excelObj = as_proteinexcelscans('SR13ID01HU02IOC02:','13PIL1:cam1:Acquire')
                          self.excelObj.ShowExcel
                          Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='START'), SET_VALUE = 'Start Scan'
                          Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='START'), GET_VALUE = buttonObj
                          buttonObj.SetToggle, 1
                          Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='INITIALISE_SCAN'), SENSITIVE = 1
                          ;self.areaDetectorObj.SetProperty, detID, FileTemplate = '%s%s.tif'
                          Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='PAUSE'), SENSITIVE = 1
                        ENDIF ELSE BEGIN 
                          Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='START'), SET_VALUE = 'Acquire'
                          Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='START'), GET_VALUE = buttonObj
                          buttonObj.SetToggle, 0
                          Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='INITIALISE_SCAN'), SENSITIVE = 0
                          self.areaDetectorObj.SetProperty, FileTemplate = '%s%s_%4.4d.tif'
                          Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='PAUSE'), SENSITIVE = 0
                        ENDELSE
                      END
                      
        'INITIALISE_SCAN' : result = self.InitialiseScan()
        
        'REVEAL_EXCEL' : BEGIN
                           IF ~Obj_Valid(self.excelObj) THEN self.excelObj = as_proteinexcelscans('SR13ID01HU02IOC02:','13PIL1:cam1:Acquire')
                           self.excelObj.ShowExcel
                         END 
        
        ; ** HELP MENU **
        ; ** HELP -> Wiki        
        'WIKI' : mg_open_url, 'http://aswebsaxs.synchrotron.org.au/saxswiki/index.php/SR13ID01_scatterBrain_Manual'
        
        ; ** HELP -> Help
        'HELP' : Call_Method, 'Overview', self.helpFile 
        
        ; ** HELP -> Context Help
        'FRAME_HELP'   : Call_Method, 'Frame_Window', self.helpFile
        
        ; ** HELP -> About
        'ABOUT'        : Call_Method, 'About', self.helpFile
        
        ; ** HELP -> Version
        'VERSION_NOTES': Call_Method, 'Version_notes', self.helpFile

        ; ** HELP -> Check for Update
        'CHECK UPDATE' : BEGIN
                           updateBase = Widget_Base(GROUP_LEADER = self.wScatterBase, EVENT_PRO = 'scatterbrain_event', /COLUMN)
                           updateLabel = Widget_Label(updateBase, VALUE = 'scatterBrain Update')
                           
                           CASE self.operatingSystem OF 
                             'Windows' : programName = 'scatterbrainanalysiswin'
                             'Mac'     : programName = 'scatterbrainanalysisosx'
                             'Linux'   : programName = 'scatterbrainanalysislinux'
                             ELSE : RETURN
                           ENDCASE
                           
                           updateObj = as_upgradeversion(programName, 'http://www.synchrotron.org.au/images/beamlines/saxswaxs/scatterbrain/',self.programDir)
                           newVersion = updateObj.newestVersion(MESSAGE = versionMessage)
                           Obj_Destroy, updateObj
                           
                           currentLabel = Widget_Label(updateBase, VALUE = 'Current version : ' + StrCompress(String(self.version,FORMAT = '(D7.3)'),/REMOVE_ALL))
                           IF newVersion GT 0 THEN newestLabel = Widget_Label(updateBase, VALUE = 'Newest version available : ' + StrCompress(String(newVersion, FORMAT = '(D7.3)'),/REMOVE_ALL), UNAME = 'NEWEST LABEL') $
                                              ELSE newestLabel = Widget_Label(updateBase, VALUE = 'Newest version available : Could not connect to server', UNAME = 'NEWEST LABEL')
                           
                           IF self.version LT newVersion THEN BEGIN
                             result = Dialog_Message(['Version ' + StrCompress(String(newVersion, FORMAT = '(D7.3)')) + ' is available', 'Notes:', AS_SplitMessage(versionMessage,500)],/INFORMATION)
                             updateLabel = Widget_Label(updateBase, VALUE = 'A new version of scatterBrain is available.', UNAME = 'UPDATE LABEL')
                             updateButton = Widget_Button(updateBase, VALUE = 'Update', UNAME = 'UPDATE')
                           ENDIF ELSE BEGIN
                             IF newVersion GT 0 THEN BEGIN
                               updateLabel = Widget_Label(updateBase, VALUE = 'Already using latest version.', UNAME = 'UPDATE LABEL')
                               updateButton = Widget_Button(updateBase, VALUE = 'Reinstall', UNAME = 'UPDATE')
                             ENDIF ELSE BEGIN
                               updateLabel = Widget_Label(updateBase, VALUE = 'No update available as cannot connect to server.', UNAME = 'UPDATE LABEL')
                               updateButton = Widget_Button(updateBase, VALUE = 'Recheck For Update', UNAME = 'RECHECK UPDATE')
                             ENDELSE
                           ENDELSE
                           
                           self.updateprogressLabel = Widget_Label(updateBase, VALUE = '', /DYNAMIC_RESIZE, UNAME = 'UPDATE PROGRESS LABEL')
                           
                           Widget_Control, updateBase, SET_UVALUE = self
                           Widget_Control, updateBase, /REALIZE
                           XManager, 'scatterbrain', updateBase, /NO_BLOCK
                         END
        'RECHECK UPDATE' : BEGIN
                             
                             CASE self.operatingSystem OF 
                               'Windows' : programName = 'scatterbrainanalysiswin'
                               'Mac'     : programName = 'scatterbrainanalysisosx'
                               'Linux'   : programName = 'scatterbrainanalysislinux'
                               ELSE : RETURN
                             ENDCASE
                             
                             updateObj = as_upgradeversion(programName, 'http://www.synchrotron.org.au/images/beamlines/saxswaxs/scatterbrain/',self.programDir)
                             newVersion = updateObj.newestVersion(MESSAGE = versionMessage)
                             Obj_Destroy, updateObj
                             
                             newestLabel = Widget_Info(event.top, FIND_BY_UNAME = 'NEWEST LABEL')
                             IF newVersion GT 0 THEN Widget_Control, newestLabel, SET_VALUE = 'Newest version available : ' + StrCompress(String(newVersion, FORMAT = '(D7.3)'),/REMOVE_ALL) $
                                                ELSE Widget_Control, newestLabel, SET_VALUE = 'Newest version available : Could not connect to server'
                           
                             updateLabel = Widget_Info(event.top, FIND_BY_UNAME = 'UPDATE LABEL')
                             updateButton = Widget_Info(event.top, FIND_BY_UNAME = 'RECHECK UPDATE')
                             
                             IF self.version LT newVersion THEN BEGIN
                               result = Dialog_Message(['Version ' + StrCompress(String(newVersion, FORMAT = '(D7.3)')) + ' is available', 'Notes:', AS_SplitMessage(versionMessage,500)],/INFORMATION)
                               Widget_Control, updateLabel, SET_VALUE = 'A new version of scatterBrain is available.'
                               Widget_Control, updateButton, SET_VALUE = 'Update'
                             ENDIF ELSE BEGIN
                               IF newVersion GT 0 THEN BEGIN
                                 Widget_Control, updateLabel, SET_VALUE = 'Already using latest version.'
                                 Widget_Control, updateButton, SET_VALUE = 'Reinstall', SET_UNAME = 'UPDATE'
                               ENDIF ELSE BEGIN
                                 Widget_Control, updateLabel, SET_VALUE = 'No update available as cannot connect to server.'
                                 Widget_Control, updateButton, SET_VALUE = 'Recheck For Update'
                               ENDELSE
                             ENDELSE
                           END
        'UPDATE'       : BEGIN
                           CASE self.operatingSystem OF 
                             'Windows' : programName = 'scatterbrainanalysiswin'
                             'Mac'     : programName = 'scatterbrainanalysisosx'
                             'Linux'   : programName = 'scatterbrainanalysislinux'
                             ELSE : RETURN
                           ENDCASE
                           updateObj = as_upgradeversion(programName, 'http://www.synchrotron.org.au/images/beamlines/saxswaxs/scatterbrain/',self.programDir, NOTIFYOBJ = notify('UpgradeCallback',self))
                           result = updateObj.getNewestVersion()
                           CASE result OF
                            0 : BEGIN
                                  void = Dialog_Message('Download failed - please try again.')
                                  Widget_Control, self.updateprogressLabel, SET_VALUE = 'Failed.'
                                END
                            1 : void = Dialog_Message(['Download succeeded, restart scatterBrain to use the new version.','If you have problems your previous version has been backed up.','These are the *.bak files in you scatterBrain directory'],/INFORMATION) 
                           ENDCASE
                         END
                         
        
        ; ***********  CAMERA CONTROL EVENTS  ********************************************************

        ; change name for next output SAXS images acquired by CCD detector
        'NIMAGE' : BEGIN
            if self.pollEpics EQ 0 then begin
                WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='NIMAGE'), set_value = 'Not applicable'
                WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='NIMAGE'), sensitive = 0 
                return
            endif

            strmes = 'Next filename = ' + val
            retmes = AS_AddMessage(strmes,self.wScatterBase)

            strFragments = strsplit(val,COUNT = nFragments, /EXTRACT)
            if nFragments GT 1 then begin
                retmes = dialog_message('No spaces in filename please')
                Widget_Control, event.id, SET_VALUE=StrJoin(strFragments),SET_TEXT_SELECT = event.offset-1,/INPUT_FOCUS
                RETURN
            endif

            test = n_elements(strsplit(self.imagesDir))
            if test GT 1 then begin
                retmes = dialog_message('No spaces in file path please')
                return  
            endif

            if n_elements(self.imagesDir) GT 40 then begin
                retmes = dialog_message('Total file path exceeds 40 characters - please change')
                return
            endif

            self.filenames.next = val
            print, type
            IF type EQ 'WIDGET_TEXT_CH' OR type EQ 'WIDGET_KBRD_FOCUS'THEN BEGIN
              changeName = 0
              IF type EQ 'WIDGET_TEXT_CH' THEN IF event.ch EQ 10B THEN changeName = 1
              IF type EQ 'WIDGET_KBRD_FOCUS' THEN IF event.enter EQ 0 THEN changeName = 1
              IF changeName EQ 1 AND val NE '' THEN BEGIN
                control = -1
                detID = 0
                fileName = ''
                WHILE control NE 1 DO BEGIN
                  control = -1
                  self.areaDetectorObj.GetProperty, detID, CONTROL = control
                  IF control EQ 1 THEN self.areaDetectorObj.GetProperty, detID, FILENAME = fileName
                  detID++
                  IF control EQ -1 THEN BREAK
                ENDWHILE
                IF fileName NE self.filenames.next THEN BEGIN 
                  IF self.resetFileNumber THEN self.areaDetectorObj.SetProperty, FILENAME = self.filenames.next, FILENUMBER = 1 $
                                          ELSE self.areaDetectorObj.SetProperty, FILENAME = self.filenames.next
                ENDIF
              ENDIF
            ENDIF
            IF type EQ 'WIDGET_KBRD_FOCUS' THEN BEGIN
              self.areaDetectorObj.GetProperty, 0, FILENAME = fileName
            ENDIF
            
        END
        'RIMAGE' : BEGIN
                    
                    Widget_Control, event.id, TIMER = 1
                   
                    detID = 0
                    control = -1
                    fileName = !Null
                    detectorState = !Null
                    WHILE control NE 1 DO BEGIN
                      control = -1
                      self.areaDetectorObj.GetProperty, detID, CONTROL = control
                      IF control EQ 1 THEN self.areaDetectorObj.GetProperty, detID, FILENAME = fileName, FILEPATH = filePath, FILENUMBER = fileNumber, FILETEMPLATE = fileTemplate, NUMIMAGES = numImages, EXPOSURETIME = exposureTime, DETECTORSTATE = detectorState, TRIGGERMODE = triggerMode, /GETCAMERAFILE
                      detID++
                      IF control EQ -1 THEN BREAK
                    ENDWHILE
                    
                    IF detectorState EQ !Null THEN RETURN
                    
                    IF triggerMode EQ 'Gap Less' THEN self.frame_obj.SetProperty, SATURATION = 3*2.^20 ELSE self.frame_obj.SetProperty, SATURATION = 2.^20
                    
                    IF ~self.scanMode AND detectorState NE 'Acquire' THEN BEGIN
                      Widget_Control, event.id, GET_UVALUE = currentFullFileName
                      IF N_Elements(currentFullFileName) EQ 0 THEN currentFullFileName = {fullFileName : '', numImages: 0}
                      IF N_Elements(fileName) GT 0 THEN BEGIN
                        numCodes = N_Elements(StrSplit(fileTemplate, '%'))
                        CASE numCodes OF
                          1 : fullFileName = String(format = '(%"' + fileTemplate + '")', filePath)
                          2 : fullFileName = String(format = '(%"' + fileTemplate + '")', filePath, fileName)
                          3 : fullFileName = String(format = '(%"' + fileTemplate + '")', filePath, fileName, fileNumber)
                          ELSE : fullFileName = filePath + fileName
                        ENDCASE
                        ;print, currentFullFileName.fullFileName, fullFileName
                        IF currentFullFileName.fullFileName NE fullFileName OR currentFullFileName.numImages NE numImages THEN BEGIN
                          Widget_Control, event.id, SET_UVALUE = {fullFileName : fullFileName, numImages : numImages}
                          Widget_Control, event.id, SET_VALUE = fileName
                          self.frame_obj.GetProperty, IMAGEPATH = imagePath
                          IF self.checkAllFilesExistence EQ 1 THEN fileList = StrArr(numImages) ELSE fileList = ''
                          FOR i = 0, self.checkAllFilesExistence * (numimages - 1) DO BEGIN
                            IF fileTemplate EQ '%s%s.tif' THEN fileList[i] = File_Basename(String(format = '(%"' + fileTemplate + '")', filePath, fileName)) $
                                                          ELSE fileList[i] = File_Basename(String(format = '(%"' + fileTemplate + '")', filePath, fileName, fileNumber + i)) 
                          ENDFOR
                          existingFileList = File_Test(imagePath + fileList)
                          existingFileCount = Total(existingFileList, /INTEGER)
                          IF existingFileCount NE 0 THEN BEGIN 
                            IF existingFileCount EQ 1 THEN result = Dialog_Message('Warning, IF you start an acquisition you will overwrite an existing file. Press "Ok" and check entered filename.') $
                                                              ELSE result = Dialog_Message('Warning, IF you start an acquisition you will overwrite ' + StrCompress(existingFileCount,/REMOVE_ALL) + ' existing files. Press "Ok" and check filenames.')
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                    ;TODO There are a couple of timer things happening here that aren't related to RIMAGE. Should just create a widget to put timer on and run all slow timing events off that.
                    IF N_Elements(fileNumber) THEN Widget_Control, Widget_Info(event.top,FIND_BY_UNAME='RINDEX'), SET_VALUE = StrCompress(fileNumber,/REMOVE_ALL)
                    IF N_Elements(numImages) THEN Widget_Control, Widget_Info(event.top,FIND_BY_UNAME='RNUMIMAGES'), SET_VALUE = StrCompress(numImages,/REMOVE_ALL)
                    IF N_Elements(exposureTime) THEN Widget_Control, Widget_Info(event.top,FIND_BY_UNAME='REXPTIME'), SET_VALUE = StrCompress(Number_Formatter(exposureTime,DECIMAL=2),/REMOVE_ALL)
                    IF self.liveLog NE '' THEN BEGIN
                      liveLog = self.liveLog
                      self.scatterXMLGUI_obj->ParseFile, FILENAME = liveLog, /LOGONLY, /UPDATE
                      IF liveLog EQ 'error' THEN self.liveLog = ''
                    ENDIF
                    IF self.scanMode GT 0 THEN BEGIN
                      IF self.excelObj.ScanActive() THEN BEGIN
                        self.scanMode = 2
                        Widget_Control, Widget_Info(self.wScatterBase, FIND_BY_UNAME='MESBOX'), GET_VALUE = messageText
;                        messageBoxSize = (Widget_Info(Widget_Info(self.wScatterBase, FIND_BY_UNAME='MESBOX'),/GEOM)).XSize
                        self.excelObj.GetProperty, CURRENTPOINT=currPoint, NUMPOINTS=numPoints
;                        messageText = StrJoin(StrSplit(messageText, String(Byte([10B])),/EXTRACT))
                        messageText = StrSplit(messageText, '#',/EXTRACT)
                        message = messageText[0] + '# Point' + StrCompress(currPoint) + ' of ' + StrCompress(numPoints) + '.'
                        messageBoxSize = (Widget_Info(Widget_Info(self.wScatterBase, FIND_BY_UNAME='MESBOX'),/GEOM)).XSize
                        result = AS_AddMessage(message, self.wScatterBase,SPLIT=messageBoxSize)
                      ENDIF ELSE BEGIN
                        IF self.scanMode EQ 2 THEN BEGIN
                          self.scanMode = 1
                          messageBoxSize = (Widget_Info(Widget_Info(self.wScatterBase, FIND_BY_UNAME='MESBOX'),/GEOM)).XSize
                          message = 'Scan completed.'
                          ;SPAWN, 'sndrec32 /embedding /play /close "c:\windows\media\tada.wav"', /NOSHELL
                          WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='START'), GET_VALUE = buttonObj
                          buttonObj.setToggle,0
                          buttonObj.setToggle,1
                          WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='START'), SET_VALUE='Start Scan', SENSITIVE = 1
                          result = AS_AddMessage(message, self.wScatterBase,SPLIT=messageBoxSize)
                        ENDIF
                      ENDELSE
                    ENDIF
                    
        END
        'INDEX'  : BEGIN
                    self.areaDetectorObj.SetProperty, FILENUMBER = *event.value
        END
        'NUMIMAGES': BEGIN
                    detID = 0
                    self.areaDetectorObj.SetProperty, NUMIMAGES = *event.value
                    IF *event.value GT 1 THEN BEGIN
                      self.areaDetectorObj.GetProperty, detID, EXPOSURETIME = time, EXPOSUREPERIOD = period
                      ;TODO This delta added to exposure period is for the 1M. Need to make it a configuration item.
                      IF period LE time THEN self.areaDetectorObj.SetProperty, EXPOSUREPERIOD = time + 0.05
                    ENDIF ELSE BEGIN
                      self.areaDetectorObj.SetProperty, EXPOSUREPERIOD = time
                    ENDELSE
        END

        ; change the exposure time
        'TEXP' : BEGIN

            Widget_Control, event.id, GET_VALUE = expList
            time = as_stringtonumber(event.str, /FLOAT)
            inList = Where(Abs(time - Float(expList)) LT 0.001)
            IF inList EQ -1 THEN BEGIN
              expList = [StrCompress(Time,/REMOVE_ALL),expList]
              expList = expList[Sort(Float(expList))]
              newPos = Where(expList EQ StrCompress(Time,/REMOVE_ALL))
              Widget_Control, event.id, SET_VALUE = expList, SET_COMBOBOX_SELECT = newPos
            ENDIF
            detID = 0
            self.areaDetectorObj->GetProperty, detID, NUMIMAGES = numImages
            ;TODO This delta added to exposure period is for the 1M. Need to make it a configuration item. Also detID needs to be set from active detector.
            IF numImages GT 1 THEN period = time + 0.05 ELSE period = time
            self.areaDetectorObj->SetProperty, EXPOSURETIME = time, EXPOSUREPERIOD = period
            
        END

        ; change from transmission measurement to SAXS exposure mode
        'POLLTYPE' : begin
            case val of
                0: poll.action = 'TRAN'
                1: poll.action = 'EXP'
                2: poll.action = 'CALIB'
            endcase
        END

   
          ; ***********  ACTION/SEQUENCE FLOW CONTROL EVENTS  *******************************************

        ; start measurement/exposure
        'START' : BEGIN
;            IF self.filenames.next EQ 'no_name_yet' THEN BEGIN
;                retmes = dialog_message('No frame file name specified!')
;                return
;            ENDIF
;            IF AS_fnamevalid(self.filenames.next,self.fext, self.nameformat) EQ 0 THEN BEGIN
;                retmes = dialog_message('Invalid Filename : ' + self.filenames.next)
;                return
;            ENDIF
            IF AS_PollFile(self.imagesDir + self.filenames.next) THEN BEGIN
                strmes = self.filenames.next + '  already exists on   ' + self.imagesDir + '  - Continue?'
                mesres = dialog_message(strmes,/DEFAULT_NO,/QUESTION)
                IF mesres NE 'Yes' THEN BEGIN
                    self.filenames.next = 'no_name_yet'
                    WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='NIMAGE'), SET_VALUE='no_name_yet'
                    return
                ENDIF
            ENDIF

;            WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='START'), SENSITIVE=0
;            WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='STOP'), SENSITIVE=1

            messageBoxSize = (Widget_Info(Widget_Info(self.wScatterBase, FIND_BY_UNAME='MESBOX'),/GEOM)).XSize
            IF self.scanMode EQ 0 THEN BEGIN
            
              ;TODO Acquisition should be run through saxsControl thus: self.saxsControl->startacquisition For now just calling Acquire PV, or SCAN PV, depending on mode.
              
              self.areaDetectorObj.GetProperty, 0, TRIGGERMODE=mode, FILENAME=fileName, EXPOSURETIME = time, /GETCAMERAFILE, /TRIGGERSTRING
              mess = 'Starting a ' + StrCompress(String(time, FORMAT='(F10.2)'),/REMOVE_ALL) + ' second acquisition using trigger mode ' + mode + '. Save with filename: ' + String(Byte([13B])) + fileName + '.tif'
              result = AS_AddMessage(mess, self.wScatterBase,SPLIT = messageBoxSize)
              self.areaDetectorObj.Acquire
            
            ENDIF ELSE BEGIN  
              Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='START'), SET_VALUE = 'Initialising...'
              result = self.InitialiseScan()
              IF result EQ -1 THEN BREAK
              Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='START'), SET_VALUE = 'Scanning...'
              Widget_Control, Widget_Info(event.top, FIND_BY_UNAME='START'), SENSITIVE = 0
              self.excelObj.GetProperty, NUMPOINTS=numPoints, PVS=PVs
              mess = 'Starting a scan with a total of ' + StrCompress(numPoints,/REMOVE_ALL) + ' points. Process variables used in scan are ' + String(Byte([13B])) + StrJoin(PVs,Byte([13B])) + '.' $
                     + String(Byte([13B])) + 'Scan Progress #'
              result = AS_AddMessage(mess, self.wScatterBase, SPLIT = messageBoxSize)
              self.excelObj.Start, 'test', 'test'
            ENDELSE
            
;            WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='POLLTYPE'), SENSITIVE=0

     
        END

 
        'STOP' : BEGIN
            WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='START'), SENSITIVE=1
            
            ;************New Stop Code************* 
             result = caput('SR13ID01HU02IOC02:ACQUIRE_STOP_ALL',1) 
             IF self.scanMode GT 1 THEN BEGIN
               WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='START'), GET_VALUE = buttonObj
               buttonObj.setToggle,0
               buttonObj.setToggle,1
               WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='START'), SET_VALUE='Start Scan'
             ENDIF ELSE BEGIN
               message = 'Exposure aborted by user.'
               messageBoxSize = (Widget_Info(Widget_Info(self.wScatterBase, FIND_BY_UNAME='MESBOX'),/GEOM)).XSize
               result = AS_AddMessage(message, self.wScatterBase,SPLIT=messageBoxSize)
             ENDELSE
             
;************Old Stop code*************
;
;            IF self.scanMode GT 1 THEN BEGIN
;              WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='START'), GET_VALUE = buttonObj
;              buttonObj.setToggle,0
;              buttonObj.setToggle,1
;              WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='START'), SET_VALUE='Start Scan'
;              self.excelObj.Stop
;              self.excelObj.GetProperty, CURRENTPOINT=currPoint, NUMPOINTS=numPoints
;              IF currPoint EQ 1 THEN point = 'point' ELSE point = 'points'
;              message = 'Scan aborted by user after completing ' + StrCompress(currPoint) + ' ' + point + ' of' + StrCompress(numPoints) + '.' 
;              messageBoxSize = (Widget_Info(Widget_Info(self.wScatterBase, FIND_BY_UNAME='MESBOX'),/GEOM)).XSize
;              result = AS_AddMessage(message, self.wScatterBase,SPLIT=messageBoxSize)
;              resumeID = Widget_Info(event.top, FIND_BY_UNAME = 'RESUME')
;              IF resumeID GT 0 THEN Widget_Control, resumeID, SET_VALUE = 'PAUSE', SET_UVALUE = 'PAUSE'
;              self.scanMode = 1
;            ENDIF ELSE BEGIN
;              self.areaDetectorObj.Stop
;              self.areaDetectorObj.SetProperty, SHUTTER='Closed'
;              message = 'Exposure aborted by user.' 
;              messageBoxSize = (Widget_Info(Widget_Info(self.wScatterBase, FIND_BY_UNAME='MESBOX'),/GEOM)).XSize
;              result = AS_AddMessage(message, self.wScatterBase,SPLIT=messageBoxSize)
;            ENDELSE
		END
       
        'PAUSE' : BEGIN
                crackedGlass = self.GetResource('crackedGlass') 
                self.frame_obj.GetProperty, dimensions = dims
                ;scale = dims[0]/N_Elements(crackedGlass[0,*,0]) < dims[1]/N_Elements(crackedGlass[0,0,*])
                crackedGlass = congrid(crackedGlass,3,dims[0],dims[1])
                crackedGlass2 = BytArr(3,dims[0],dims[1])
                crackedGlass2[*,0:N_Elements(crackedGlass[0,*,0])-1,0:N_Elements(crackedGlass[0,0,*])-1] = crackedGlass
                self.frame_obj.SetProperty, DATA = crackedGlass2
                self.frame_obj.drawimage
                
                result = Dialog_Message('Oh oh. Look what you have done!! This pause button has issues, if you want to pause a scan, then use the scan gui on the computer to the left. Check the wiki for details.')
;                    IF Obj_Valid(self.excelObj) THEN BEGIN
;                      self.excelObj.Pause, 1
;                      Widget_Control, event.id, SET_VALUE = 'RESUME', SET_UVALUE = 'RESUME'
;                    ENDIF
        END
        
        'RESUME': BEGIN
                    IF Obj_Valid(self.excelObj) THEN BEGIN
                      self.excelObj.Pause, 0
                      Widget_Control, event.id, SET_VALUE = 'PAUSE', SET_UVALUE = 'PAUSE'
                    ENDIF
        END

        ELSE: BEGIN
            
           
        ; ***********  END OF VALID EVENTS  *********************************************************    
              print,dialog_message('No action yet for this')
            
        ENDELSE

    ENDCASE    ; end of main screen GUI case statement

   ;self.frameWin->Draw, self.frameView_obj

END

PRO scatterbrain::Refresh

  @as_scatterheader.macro

  self.frameWin->Draw
  
END

  
PRO scatterbrain::Cleanup
  
  Widget_Control, self.wScatterBase, /DESTROY
  
  Obj_Destroy, self.frame_obj
  Obj_Destroy, self.frameModel_obj
  Obj_Destroy, self.framePalette_obj
  Obj_Destroy, self.frameView_obj
  Obj_Destroy, self.areaDetectorObj
  Obj_Destroy, self.pollAreaDetector
  Obj_Destroy, self.cleanup_obj
  Obj_Destroy, self.centreingSymbol
  Obj_Destroy, self.scatterXMLGUI_obj
  Obj_Destroy, self.profiles_obj
  Obj_Destroy, self.saxsControl
  Obj_Destroy, self.helpFile
  Obj_Destroy, self.excelObj
  Obj_Destroy, self.boxObj
  Obj_Destroy, self.message_obj
  Obj_Destroy, self.plotControl_obj    
  Obj_Destroy, self.profilePalette_obj 
  Obj_Destroy, self.frameWin           
  Obj_Destroy, self.centreingPoints    
       
END

PRO scatterbrain::saveXML

  @as_scatterheader.macro

  IF self.pollEpics GT 0 THEN self.areaDetectorObj.StoreParams, self.scatterXMLGUI_obj
  name = self.scatterXMLGUI_obj.GetValue('name')
  load = StrArr(N_Elements(name))
  self.frame_obj.GetProperty, CONFIGNAME = configName1
  IF Obj_Valid(self.frame_obj2) THEN BEGIN
    self.frame_obj2.GetProperty, CONFIGNAME = configName2
    load[[Where(name EQ configName1),Where(name EQ configName2)]] = ['DETECTOR1', 'DETECTOR2']  
  ENDIF ELSE load[Where(name EQ configName1)] = 'DETECTOR1'
  
  self.scatterXMLGUI_obj.SetValue, 'loadconfig', load
  self.scatterXMLGUI_obj->SaveFile
  widgetIDS = [Widget_Info(self.wScatterBase, FIND_BY_UNAME = 'NEW EXPERIMENT EMPTY'),Widget_Info(self.wScatterBase, FIND_BY_UNAME = 'NEW EXPERIMENT FILES')]
  FOREACH widgetID, widgetIDS DO IF widgetID GT 0 THEN Widget_Control, widgetID, SENSITIVE = 1

END

PRO scatterbrain::loadXML, xmlFile

  @as_scatterheader.macro

  CD, current=current
  IF self.experimentDir NE '' THEN path = self.experimentDir ELSE path = current
  IF ISA(xmlFile, 'STRING') THEN BEGIN
    IF ~File_Test(xmlFile) AND File_BaseName(xmlFile) EQ '.' THEN BEGIN
      xmlFileTestPaths = [self.experimentDir + Path_Sep() + xmlFile,current + Path_Sep() + xmlFile]
      FOREACH xmlFileTest, xmlFileTestPaths DO BEGIN
        IF File_Test(xmlFileTest) THEN BEGIN
          xmlFile = xmlFileTest
          BREAK
        ENDIF
      ENDFOREACH
    ENDIF
  ENDIF ELSE xmlFile = ''
  IF ~File_Test(xmlFile) THEN xmlFile = Dialog_Pickfile(filter = '*.xml', PATH = path, TITLE = 'Select Experiment File', /READ, /MUST_EXIST)
  IF xmlFile EQ '' THEN RETURN
  self.experimentDir = File_DirName(xmlFile, /MARK_DIRECTORY)
  
  imagesDir = self.experimentDir + 'images' + Path_Sep()
  self.imagesDir = File_Test(imagesDir, /DIRECTORY) ? imagesDir : self.experimentDir

  self.scatterXMLGUI_obj->ParseFile, FILENAME = xmlFile
  IF xmlFile EQ 'error' THEN RETURN
  self.settingsObj.SetProperty, RECENTFILE = xmlFile
  self.updateRecentFileList
                      
  widgetIDS = [Widget_Info(self.wScatterBase, FIND_BY_UNAME = 'NEW EXPERIMENT EMPTY'),Widget_Info(self.wScatterBase, FIND_BY_UNAME = 'NEW EXPERIMENT FILES')]
  FOREACH widgetID, widgetIDS DO IF widgetID GT 0 THEN Widget_Control, widgetID, SENSITIVE = 1
  self.liveLog = ''
  
  self.scatterXMLGUI_obj.GetProperty, NUMLOADCONFIG=numLoadConfig
  
  IF numLoadConfig.detector2 NE -1 THEN self.Use2ndDetector
  IF numLoadConfig.detector1 EQ -1 THEN numLoadConfig.detector1 = 0
  
  self.frame_obj->NewParams, self.scatterXMLGUI_obj, CONFIGNO = numLoadConfig.detector1
  IF Obj_Valid(self.frame_obj2) THEN BEGIN
    self.frame_obj2->LOADCONFIG, numLoadConfig.detector2
    result = self.qCalibGUI(frameNo = 1)
  ENDIF
  IF self.pollEpics GT 0 THEN self.areaDetectorObj->NewParams, self.scatterXMLGUI_obj
  self.profiles_obj.NewParams, self.scatterXMLGUI_obj
  result = self.qCalibGUI()
  ; TODO Need to start using saxsControl
  ;self.saxsControl->NewParams, self.scatterXMLGUI_obj
  ;self.filenames.log = xmlFile
  self.frame_obj->SetProperty, PATH = self.imagesDir
  self.frame_obj->ReSize, BUFFER = self.aux_base_size + [50,50]
  geom = Widget_Info(self.wScatterBase, /GEOM)
  self.scatterXMLGUI_obj->SetProperty, HEIGHT = geom.scr_ysize*.9 

  
  

END

PRO scatterBrain::newXML, event, DATA = data

  @as_scatterheader.macro

  files = 0
  IF TypeName(data) EQ 'HASH' THEN IF data.haskey('files') THEN files = data['files']
  xmlfile = event.path[0] + event.experimentName + Path_Sep() + event.experimentName + '.xml'

  IF event.saxswaxs EQ 1 THEN BEGIN
    self.settingsObj.startingdirectory2 = event.path[1]
    File_MkDir, event.path[1] + event.experimentName
    CD, event.path[1] + event.experimentName
    File_MkDir, ['avg','analysis','sub','raw_sub','raw_dat','manual','images']
  ENDIF

  
  self.settingsObj.startingdirectory1 = event.path[0]
  File_MkDir, event.path[0] + event.experimentName
  CD, event.path[0] + event.experimentName
  File_MkDir, ['avg','analysis','sub','raw_sub','raw_dat','manual','images']
  CD, 'images'
  OpenW, fileLUN, 'livelogfile.log', /GET_LUN
  Free_LUN, fileLUN
  OpenW, fileLUN, 'livelogfile-comments.log', /GET_LUN
    PrintF, fileLUN, '<?xml version="1.0"?>'
    PrintF, fileLUN, '<root></root>'
  Free_LUN, fileLUN
  CD, CURRENT = current
  
  IF Tag_Names(event, /STRUCTURE_NAME) EQ 'TEMPLATE' THEN BEGIN
    IF event.template NE 'Current' THEN BEGIN
      templateFile = self.settingsObj.settingsPath + Path_Sep() + event.template + '.xml'
      IF File_Test(templateFile) THEN BEGIN
        File_Copy, templateFile, xmlfile 
      ENDIF ELSE BEGIN
        result = Dialog_Message('Invalid template file.', /ERROR)
        RETURN
      ENDELSE
      self.loadXML, xmlfile
    ENDIF
  ENDIF

  self.liveLog = current + Path_Sep() + 'livelogfile.log'

  self.settingsObj.GetProperty, LOCALFILEPATH = localPath, REMOTEFILEPATH = remotePath, DETECTOR = detectors
  
  self.scatterXMLGUI_obj.GetParameters, ADMap = ADMap
  
  detID = Where(ADMap.control EQ 1)
  
  FOREACH detNo, detID DO BEGIN
    confDetID = Where((ADMap.Detectordef)[detno] EQ detectors)
    localPathPattern = StrJoin(StrSplit(localpath[confDetID],Path_Sep(),/EXTRACT),Path_Sep()+Path_Sep())
    newPathSuffix = StrSplit(event.path[detNo],localPathPattern,/REGEX,/EXTRACT,/FOLD_CASE)
    newPathSuffix = StrJoin([StrSplit(newPathSuffix,Path_Sep(),/EXTRACT),event.experimentName,'images'],'/')
    sep = StrMid(remotePath[confDetID],0,1,/REVERSE_OFFSET) EQ '/' ? '' : '/'
    newRemotePath = remotePath[confDetID] + sep + newPathSuffix
    self.areaDetectorObj.SetProperty, detNo, filePath=newRemotePath
    self.areaDetectorObj.SetProperty, detNo, logfilepath=newRemotePath
    self.areaDetectorObj.SetProperty, detNo, logfilename='livelogfile.log'
    ;IF CAResult NE 0 THEN result = Dialog_Message('Error setting logfile path and name.')
  ENDFOREACH
   
  IF Obj_Valid(self.areaDetectorObj) THEN self.areaDetectorObj.StoreParams, self.scatterXMLGUI_obj
  
  IF event.template EQ 'Current' THEN BEGIN
    IF files EQ 1 THEN BEGIN
      IF ~self.fileSelectList.IsEmpty() THEN BEGIN
        self.scatterXMLGUI_obj->SaveFile, xmlFile, FILELIST = self.fileSelectList.toarray(), TYPEFILELIST = 'COPY'
        self.frame_obj.GetProperty, IMAGEPATH = oldImagePath
        IF StrMid(oldImagePath, StrLen(oldImagePath)-1,1) NE Path_Sep() THEN oldImagePath = oldImagePath + Path_Sep()
        File_Copy, oldImagePath + self.fileSelectList.toarray(), current + Path_Sep() + 'copy_' + self.fileSelectList.toarray()
      ENDIF ELSE files = 0
    ENDIF
    IF files EQ 0 THEN self.scatterXMLGUI_obj->SaveFile, xmlfile, /EMPTY
    self.scatterXMLGUI_obj->ParseFile, FILENAME = xmlFile
    self.frame_obj->NewParams, self.scatterXMLGUI_obj
    IF Obj_Valid(self.frame_obj2) THEN BEGIN
      self.frame_obj2->NewParams, self.scatterXMLGUI_obj
      result = self.qCalibGUI(frameNo = 1)
    ENDIF
    
    self.profiles_obj.NewParams, self.scatterXMLGUI_obj
    result = self.qCalibGUI()
    self.frame_obj->ReSize, BUFFER = self.aux_base_size + [50,50]
    self.frame_obj.SetProperty, PATH=current + Path_Sep()
  ENDIF

END

PRO scatterBrain::Use2ndDetector

  Widget_Control, Widget_Info(self.wScatterBase, FIND_BY_UNAME = '2ND DETECTOR'), SET_BUTTON = 1
  Widget_Control, Widget_Info(self.wScatterBase, FIND_BY_UNAME = 'DATA PATH 2'), /SENSITIVE
  secondFrameBase = Widget_Base(GROUP_LEADER = self.wScatterBase, TITLE = '2nd Detector/Frame')
  self.qData_obj2 = Obj_New('AS__SaxsQData')
  self.frame_obj2 = Obj_New('as__saxsimagegui', secondFrameBase, self.qData_obj2, self.profiles_obj, RESOURCE=self, NOTIFY = notify('FrameCallback',self))
  self.frame_obj2->SetProperty, LOGOBJ=self.scatterXMLGUI_obj
  IF self.scatterXMLGUI_Obj.FileLoaded() THEN self.frame_obj2->NewParams, self.scatterXMLGUI_Obj
  self.qCalibGUI2.SetProperty, NOTIFYOBJ = notify('qCalibChange',self.frame_obj2)

END

PRO scatterbrain::TwoDSubtract, twoD

  index = self.scatterXMLGUI_obj.GetIndex(twoD.blank)
  typeFrame = self.scatterXMLGUI_obj.GetType(index)
  IF self.frame_obj.getimage(twoD.blank, TYPEFRAME = typeFrame) THEN BEGIN
    blank = *self.frame_obj.frame.rawdata
    void = self.scatterXMLGUI_obj.GetScale(index,I0=Blanki0counts, IBS = Blankibscounts,TIME=Blanktime1)
  ENDIF

  FOREACH sampleName, twoD.samples DO BEGIN
    index = self.scatterXMLGUI_obj.GetIndex(sampleName)
    typeFrame = self.scatterXMLGUI_obj.GetType(index)
    IF self.frame_obj.getimage(sampleName, TYPEFRAME = typeFrame) THEN BEGIN
      sample = *self.frame_obj.frame.rawdata
      void = self.scatterXMLGUI_obj.GetScale(index,I0=Samplei0counts, IBS = Sampleibscounts,TIME=Sampletime1)
    ENDIF
    IF twoD.prefix NE '' THEN prefix = twoD.prefix ELSE prefix = 'SUB'
    IF ~File_Test(File_Dirname(self.frame_obj.frame.path)+'/summedimages/') THEN File_MKDir, File_Dirname(self.frame_obj.frame.path)+'/summedimages/'
    FQfilename = File_Dirname(self.frame_obj.frame.path)+'/summedimages/' + prefix+ sampleName
    
    CASE self.profiles_obj.nrmType OF
      0 : subimage = sample - blank
      1 : subimage = sample - Float(Samplei0counts)*blank/Float(Blanki0counts)
      2 : subimage = sample - Float(Sampleibscounts)*blank/Float(Blankibscounts)
      ELSE : subimage = sample - Float(Sampleibscounts)*blank/Blankibscounts
    ENDCASE
    
    
    Write_Tiff, FQfilename, Reverse(subimage,2), /LONG, /SIGNED, DESCRIPTION='Subtracted frame created by scatterBrain using the following files: ' + sampleName + ' - ' + twoD.blank
    self.scatterXMLGUI_obj.NewLogLine, prefix + sampleName, Sampletime1, Samplei0counts, 0, SampleiBScounts, TYPE = 'SUBTRACTED'
  ENDFOREACH

END

PRO scatterbrain::LogFileSelected, Selected

  COMPILE_OPT idl2
  
  CATCH, theError
  IF theError NE 0 THEN BEGIN
    CATCH, Cancel=1
    void = Error_Message(/Traceback)
    RETURN
  ENDIF

  IF selected.type EQ 'DESELECT'THEN BEGIN
    self.currentlySelected = List()
    RETURN
  ENDIF

  self.currentlySelected = selected.name

  CASE Selected.type OF 
    'SELECT' : BEGIN
                 self.fileSelectList = selected.name
                 ;self.profiles_obj.
                 IF selected.clicks EQ 2 THEN BEGIN
                   numFiles = selected.name.count() 
                   IF numFiles GT 1 THEN BEGIN
                    self.frame_obj.SetProperty, UPDATEIMAGE = 0
                    IF Obj_Valid(self.frame_obj2) THEN self.frame_obj2.SetProperty, UPDATEIMAGE = 0
                   ENDIF
                   FOREACH name, selected.name, key DO BEGIN
                     IF key EQ numFiles - 1 AND numFiles GT 1 THEN BEGIN
                       self.frame_obj.SetProperty, UPDATEIMAGE = 1
                       IF Obj_Valid(self.frame_obj2) THEN self.frame_obj2.SetProperty, UPDATEIMAGE = 1
                     ENDIF
                     result = self.ProcessImage(name)
                   ENDFOREACH
                 ENDIF
               END
    'EXPORT PROFILES' : BEGIN
                 profileIndices = IntArr(selected.name.count())
                 self.frame_obj.SetProperty, UPDATEIMAGE = 0
                 selected.name.reverse
                 FOREACH name, selected.name, key DO profileIndices[key] = self.ProcessImage(name, /NOPLOT, NOSETUP = ~(key EQ 1) )
                 self.frame_obj.SetProperty, UPDATEIMAGE = 1
                 data = self.profiles_obj.GetProfiles(profileIndices)
                 self.profiles_obj.DeleteProfile, profileIndices
                 saveDirectory = Dialog_Pickfile(/DIRECTORY)
                 titleList = list('q   ', 'I   ', 'Err   ')
                 
                 FOREACH name, selected.Name, i DO BEGIN
      
                   OpenW, profileFile, saveDirectory + StrMid(name,0,(strsplit(name, '.'))[-1]-1) + '.dat', /GET_LUN
                     PrintF, profileFile, name
                     PrintF, profileFile, titleList.toarray(), FORMAT = '(' + StrCompress(3,/REMOVE) + 'A' + StrCompress(1+StrLen((data[i])[0]),/REMOVE) + ')'
                     PrintF, profileFile, data[i], FORMAT = '(' + StrCompress(3,/REMOVE) + 'A' + StrCompress(1+StrLen((data[i])[0]),/REMOVE) + ')'
                   Free_Lun, profileFile
                
                 ENDFOREACH
                   
               END
    'EXPORT PROFILES SINGLE FILE' : BEGIN
                 profileIndices = IntArr(selected.name.count())
                 self.frame_obj.SetProperty, UPDATEIMAGE = 0
                 selected.name.reverse
                 FOREACH name, selected.name, key DO profileIndices[key] = self.ProcessImage(name, /NOPLOT, NOSETUP = ~(key EQ 1) )
                 self.frame_obj.SetProperty, UPDATEIMAGE = 1
                 data = self.profiles_obj.GetProfiles(profileIndices)
                 self.profiles_obj.DeleteProfile, profileIndices
                 profilesArray = DblArr(N_Elements(data)+1,N_Elements((data[0])[0,*]))
                 profilesArray[0,*] = (data[0])[0,*]
                 FOREACH p, data, key DO profilesArray[key+1,*] = p[1,*]
                 titleArray =['q', selected.name.toArray()]
                 saveName = Dialog_Pickfile()
                 IF saveName EQ '' THEN BREAK
                 columnWidth = Max(StrLen(titleArray)) + 1 > StrLen(profilesArray[0]) + 1
                 OpenW, fileLUT, saveName, /GET_LUN
                   PrintF, fileLUT, titleArray, FORMAT = '(' + StrCompress(data.count() + 1,/REMOVE) + 'A' + StrCompress(columnWidth,/REMOVE) + ')'
                   PrintF, fileLUT, profilesArray, FORMAT = '(' + StrCompress(data.count() + 1,/REMOVE) + 'A' + StrCompress(columnWidth,/REMOVE) + ')'  
                 Free_Lun, fileLUT
                 
               END
    'SUM'    : BEGIN
                self.frame_obj.SetProperty, AUTOSCALE = 0
                result = self.ProcessImage(selected.name, /SAVESUMMED)
               END
    'CONTOUR': BEGIN
                 ;profiler, /RESET
                 ;profiler,/SYSTEM
                 profileIndices = IntArr(selected.name.count())
                 self.frame_obj.SetProperty, UPDATEIMAGE = 0
                 selected.name.reverse
                 result = ''
                 FOREACH name, selected.name, key DO BEGIN
                   index = self.ProcessImage(name, /NOPLOT, NOSETUP = ~(key EQ 1) )
                   IF index EQ -1 THEN result = Dialog_Message('File opening error encountered, continue anyway?',/QUESTION)
                   IF result EQ 'No' THEN BREAK
                   profileIndices[key] = index
                 ENDFOREACH
                 IF result EQ 'No' THEN BREAK
                 self.frame_obj.SetProperty, UPDATEIMAGE = 1
                 profileIndices = profileIndices[Where(profileIndices NE -1, /NULL)]
                 IF N_Elements(profileIndices) EQ 0 THEN BEGIN
                   result = Dialog_Message('No valid files selected to create contour - returning.')
                   BREAK
                 ENDIF
                 data = self.profiles_obj.GetProfiles(profileIndices)
                 self.profiles_obj.DeleteProfile, profileIndices
                 
                 self.profiles_obj.GetProperty, XRANGEZOOM = xRange
                 
                 yNames = self.scatterXMLGUI_obj.GetLogAttributes()
                 
                 indices = !Null
                 imageIndex = !Null
                 yStruct = {}
                 FOREACH name, selected.name DO BEGIN
                   indices = [indices, self.scatterXMLGui_Obj.GetIndex(name)]
                   imageIndex = [imageIndex,Fix(StrMid(name, 7,4 ,/REVERSE_OFFSET))]
                 ENDFOREACH
                 FOREACH yName, Reverse(yNames) DO BEGIN
                   valType = 'Number'
                   attValString = ((self.scatterXMLGui_obj.GetValue(yName))[indices])
                   FOREACH aVS, attValString DO BEGIN
                     IF StRegex(aVS, '[^0-9]') + StRegex(aVS, '\.[0-9]*\.') + StRegex(aVS, '-[0-9]*-') NE -3 THEN BEGIN
                       valType = 'String'
                       BREAK
                     ENDIF 
                   ENDFOREACH
                   IF valType EQ 'String' THEN attVal = attValString ELSE attVal = Double(attValString)
                   yStruct = Create_Struct(yName, attVal, yStruct)
                 ENDFOREACH
                 yStruct = Create_Struct('Index', Indgen(data.count()),'File Index', imageIndex, 'Filename',selected.name.toArray(), yStruct)
                 
                 q = (data[0])[0,*]
                                  
                 inRange = Where(q GT xRange[0] AND q LT xRange[1])
                 IF N_Elements(inRange) EQ 1 THEN inRange = Indgen(N_Elements(q))
                
                 q = q[inRange]
                
                 zContour = FltArr(N_Elements(q), data.count())
                 FOREACH prof, data, index DO BEGIN
                   zContour[*,index] =  Reform(prof[1,inRange])
                 ENDFOREACH
                 IF selected.add EQ 1 AND Obj_Valid(self.contourPlot) EQ 1 THEN BEGIN
                  self.contourPlot.addProfiles, zContour
                 ENDIF ELSE self.contourPlot = as__saxscontourplot(q, yStruct, zContour, GROUPLEADER = self.wScatterBase, NOTIFYOBJ = notify('ContourCallback',self))
                 ;profiler, /REPORT, FILENAME = 'profiler.dat'
               END
    'MOVIE' : BEGIN
                
                self.frame_obj.GetProperty, IMAGEPATH = path
                movieFileName = Dialog_Pickfile(/OVERWRITE_PROMPT,/WRITE)
                IF movieFileName EQ '' THEN RETURN
                videoObj = IDLffVideoWrite(movieFileName)
                frameNo = 0
                FOREACH name, selected.name, key DO BEGIN
                  IF key GT 0 THEN self.profiles_obj.deleteProfile, profileIndex
                  profileIndex = self.processImage(name, NOSETUP = key GT 0)
                  IF profileIndex EQ -1 THEN BEGIN
                    result = Dialog_Message('File opening error encountered, continue anyway?',/QUESTION)
                    IF result EQ 'No' THEN BREAK
                    CONTINUE
                  ENDIF
                  profileIndexTemp = profileIndex
                  self.profiles_obj.LineWidth, profileIndexTemp, 4
                  self.profiles_obj.updateplot
                  imageObj = self.frame_obj.read()
                  imageObj.GetProperty, DATA = frame
                  plotImage = self.profiles_obj.getplotimage()
                  geomFrame = Size(frame)
                  geomPlot = Size(plotImage)
                  width = geomFrame[2] + geomPlot[2] + 15
                  height = (geomFrame[3] > geomPlot[3]) + 10
                  image = BytArr(3, Width, Height)
                  image[0:2,5:geomFrame[2]+4,(height-geomFrame[3])/2:(height-geomFrame[3])/2+geomFrame[3]-1]=frame
                  image[0:2,geomFrame[2]+10:geomFrame[2]+10+geomPlot[2]-1,(height-geomPlot[3])/2:(height-geomPlot[3])/2+geomPlot[3]-1]=plotImage
                  IF frameNo EQ 0 THEN streamIndex = videoObj.AddVideoStream(N_Elements(image[0,*,0]),N_Elements(image[0,0,*]), 5, BIT_RATE = 4e7)
                  void = videoObj.Put(streamIndex, image)
                  frameNo++
                ENDFOREACH
                  
                Obj_Destroy, videoObj
                                
               END
    'MOSAIC' : BEGIN
                frameList = List()
                self.frame_obj.GetProperty, IMAGEPATH = path
                FOREACH name, selected.name DO BEGIN
                  result = self.frame_obj.GetRawImage(path + name, frame=frame)
                  IF result EQ -1 THEN BEGIN
                    yn = Dialog_Message('File opening error encountered, continue anyway?',/QUESTION)
                    IF yn EQ 'No' THEN BREAK
                  ENDIF ELSE frameList.Add, frame
                ENDFOREACH
                IF frameList.count() EQ 0 THEN BEGIN
                  dialog = Dialog_Message('No valid files, returning.')
                  BREAK
                ENDIF
                rows = Fix(Sqrt(framelist.count()))
                columns = Ceil(framelist.count()/Float(rows))
                mosaic = as_imagemosaicGUI(frameList, columns, rows, LABELS = selected.name)
               END
    'SECTOR' : BEGIN
                 numSectors = 40
                 IF self.settingsBase GT 0 THEN BEGIN
                   wNoSectors = Widget_Info(self.settingsBase, FIND_BY_UNAME = 'NO. SECTORS')
                   IF wNoSectors GT 0 THEN Widget_Control, wNoSectors, GET_VALUE = numSectors
                   numSectors = Round(numSectors)
                 ENDIF
                 profileData = self.frame_obj.Sectors(selected.name, numSectors)
                 
                 q = (profiledata[0]).q_arr
                 y = (360*(IndGen(numSectors)/Float(numSectors))-180)
                 
                 zContour = FltArr(N_Elements(q), N_Elements(y))
                 FOREACH prof, profiledata, index DO zContour[*,index] =  prof.profile ;Reform((prof.profile)[1,*])
                 
                 y = {Sector : y}
                 
                 self.contourPlot = as__saxscontourplot(q, y, zContour, FILENAMES = selected.name.toArray(), GROUPLEADER = self.wScatterBase, NOTIFYOBJ = notify('ContourCallback',self))
                 
               END
    'NIGEL'  : BEGIN
                nigelImage = self.GetResource('Nigel') 
                self.frame_obj.GetProperty, dimensions = dims
                scale = dims[0]/N_Elements(nigelImage[0,*,0]) < dims[1]/N_Elements(nigelimage[0,0,*])
                nigelImage = congrid(nigelImage,3,scale*N_Elements(nigelImage[0,*,0]),scale*N_Elements(nigelimage[0,0,*]))
                nigelImage2 = BytArr(3,dims[0],dims[1])
                nigelImage2[*,0:N_Elements(nigelImage[0,*,0])-1,0:N_Elements(nigelImage[0,0,*])-1] = nigelImage
                self.frame_obj.SetProperty, DATA = nigelImage2
                self.frame_obj.drawimage
               END
    ELSE     :
  ENDCASE
END

FUNCTION scatterbrain::ProcessImage, name, SAVESUMMED = saveSummed, LIVEFRAME = liveFrame, NOPLOT = noPlot, NOSETUP = noSetup, DETECTORNO = detectorNo

  @as_scatterheader.macro

  IF N_Elements(detectorNo) EQ 0 THEN detectorNo = -1

  live = KeyWord_Set(liveFrame)
  IF live THEN BEGIN
    CASE liveFrame.count() OF
      1 : liveFrame1 = liveFrame[0]
      2 : liveFrame2 = liveFrame[1]
      ELSE :
    ENDCASE
    typeFrame = 'Live'
  ENDIF ELSE BEGIN
    typeFrame = self.scatterXMLGUI_obj.GetType(self.scatterXMLGUI_obj.GetIndex(name))
  ENDELSE
  
  profileData=self.frame_obj->GetAndCake(name, SAVESUMMED = saveSummed, SUMMEDNAME = summedName, FRAME = liveFrame1, NOSETUP = noSetup, typeFrame = typeFrame)
  IF Obj_Valid(self.frame_obj2) THEN profileData2=self.frame_obj2->GetAndCake(name, SAVESUMMED = saveSummed, SUMMEDNAME = summedName, FRAME = liveFrame2, NOSETUP = noSetup, TYPEFRAME = typeFrame)
  IF Size(profileData, /TNAME) EQ 'INT' THEN BEGIN
    IF profileData[0] EQ -1 THEN RETURN, -1
  ENDIF
  
  IF KeyWord_Set(saveSummed) THEN name = summedName
  IF detectorNo LT 1 THEN BEGIN
    self.frame_obj.GetProperty, CONFIGNAME = configName, TIME = time, I0SF = i0sf, IBSSF = ibssf
    self.profiles_obj.AddProfile, profileData.q_arr, profileData.profile, profileData.error, name, CONFIGNAME = configName, DETECTORNO = 0, TIME = time, I0COUNT = i0sf, IBSCOUNT = ibssf, LIVE = live, NOPLOT = noPlot, PROFILEINDEX = profileIndex
  ENDIF
  IF Obj_Valid(self.frame_obj2) THEN BEGIN
    IF detectorNo NE 0 THEN BEGIN
      self.frame_obj2.GetProperty, CONFIGNAME = configName, TIME = time, I0SF = i0sf, IBSSF = ibssf
      IF ISA(profileData2, 'STRUCT') THEN self.profiles_obj.AddProfile, profileData2.q_arr, profileData2.profile, profileData2.error, name, CONFIGNAME = configName, DETECTORNO = 1, TIME = time, I0COUNT = i0sf, IBSCOUNT = ibssf, LIVE = live, NOPLOT = noPlot, PROFILEINDEX = profileIndex
    ENDIF
  ENDIF
  
  WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='SIMAGE'), SET_VALUE=name
  self.profiles_obj->UpdateProfileWidgets
  
  IF N_Elements(profileIndex) EQ 0 THEN profileIndex = -1
  
  RETURN, profileIndex

END

PRO scatterbrain::ExportCurrentImage, RAW=raw

  @as_scatterheader.macro

  filters = ['*.jpg', '*.tif', '*.png']
  fileName = Dialog_Pickfile(TITLE = 'Choose filename.', /WRITE, /OVERWRITE_PROMPT, DEFAULT_EXTENSION = 'jpg', FILTER = filters)
  
  IF fileName EQ '' THEN RETURN

  

  IF KeyWord_Set(raw) THEN BEGIN
                        tempImage = IDLgrImage(*(self.frame_obj).data, PALETTE = (self.frame_obj).palette)
                        tempWindow = IDLgrWindow(RETAIN=2, DIMENSIONS=[981,1043])
                        tempView = IDLgrView(VIEWPLANE_RECT = [0,0,981,1043])
                        tempModel = IDLgrModel()
                        tempModel->Add, tempImage
                        tempView->Add, tempModel
                        tempWindow->Draw, tempView
                        imageObj = tempWindow.read()
                        Obj_Destroy, [tempWindow,tempView, tempModel, tempImage]
                      ENDIF ELSE imageObj = self.frame_obj.Read()
  
  imageObj.GetProperty, DATA = image
  
  fileExtension = StrUpCase((StrSplit(fileName, '.', /EXTRACT))[-1])
  
  SWITCH fileExtension OF
    'JPG' :  
    'JPEG': BEGIN
              IF Size(image, /N_DIMENSIONS) EQ 3 THEN Write_JPeg, fileName, image, TRUE = 1 ELSE Write_JPeg, fileName, image
              BREAK
            END
    'TIF' : 
    'TIFF': BEGIN
              IF Size(image, /N_DIMENSIONS) EQ 3 THEN Write_Tiff, fileName, Reverse(image,3) ELSE Write_Tiff, fileName, Reverse(image,2) 
              BREAK
            END
    'PNG' : BEGIN
              Write_PNG, fileName, image 
              BREAK
            END
  ENDSWITCH
  
END

FUNCTION scatterbrain::InitialiseScan

  @as_scatterheader.macro

  ;TODO Need to control all detectors.
  detID = 0
  Widget_Control, /HOURGLASS
  scanParamsResult = self.excelObj.GetScanParams()
  IF scanParamsResult LT 0 THEN BEGIN
    result = Dialog_Message('Scan not initialised!', /ERROR)
    RETURN, -1
  ENDIF
  
  IF scanParamsResult LT 7 THEN BEGIN
    self.excelObj.GetProperty, SAMPLENAMES = sampleNames
    self.frame_obj.GetProperty, IMAGEPATH = imagePath
    IF self.checkScanFiles EQ 1 THEN BEGIN
      existingFileList = File_Test(imagePath + sampleNames + '.tif')
      existingFileCount = Total(existingFileList, /INTEGER)
      IF existingFileCount NE 0 THEN BEGIN 
        IF existingFileCount EQ 1 THEN result = Dialog_Message('Warning, IF you start the Scan you will overwrite an existing file. Press "Cancel" and check entered filenames, or "Ok" to ignore.', /ERROR, /CANCEL, /DEFAULT_CANCEL) $
                                  ELSE result = Dialog_Message('Warning, IF you start the Scan you will overwrite ' + StrCompress(existingFileCount,/REMOVE_ALL) + ' existing files. Press "Cancel" and check filenames, or "Ok" to ignore.', /ERROR, /CANCEL, /DEFAULT_CANCEL)
        IF result EQ 'Cancel' THEN RETURN, -1
      ENDIF
    ENDIF
  ENDIF

  CASE scanParamsResult OF
    7: BEGIN
         self.areaDetectorObj.SetProperty, FileTemplate = '%s%s_%4.4d.tif'
         self.excelObj.InitialiseScan, /NONAMES
         self.areaDetectorObj.SetProperty, detID, FileName = self.filenames.next
       END
    8: BEGIN 
         self.areaDetectorObj.SetProperty, FileTemplate = '%s%s_%4.4d.tif'
         self.excelObj.InitialiseScan
       END     
    ELSE: BEGIN
            self.areaDetectorObj.SetProperty, FileTemplate = '%s%s.tif'
            self.excelObj.InitialiseScan
          END
  ENDCASE
  Widget_Control, HOURGLASS = 0

  RETURN, 1

END

PRO scatterbrain::updateRecentFileList

  @as_scatterheader.macro

  self.settingsObj.GetProperty, RECENTFILE = recentFileList
  
  recentFileList[Where(recentFileList EQ !Null,/NULL)] = ''
  
  FOREACH recentFile, recentFileList, key DO $ 
    Widget_Control, Widget_Info(self.wScatterBase, FIND_BY_UNAME = 'RECENT_FILE_' + StrCompress(key, /REMOVE_ALL)), SET_VALUE = StrCompress(key, /REMOVE_ALL) + ': ' + recentFile, SET_UVALUE = recentFile
  
END

PRO scatterbrain::exportLUT
  self.settingsObj.GetProperty, LUTPV = LUTPV, NORMPV = NORMPV, QVECTORPV=QVECTORPV 
  LUT = self.frame_obj.GetLUT() 
  QVECTOR = self.qData_Obj.GetProperty(/Q_ARR)
  IF N_Elements(LUT) GT 1 THEN result = caput(LUTPV, [N_Elements(QVECTOR),LUT])
  IF N_Elements(QVECTOR) GT 1 THEN result = caput(QVECTORPV, QVECTOR)
  normParam = 1/((self.profiles_obj).ibsnrm*(self.profiles_obj).CSCalib)
  IF Finite(normParam) NE 1 THEN normParam = 1
  result = caput(NORMPV,normParam)
  ;result = caput(NORMPV,1/self.profiles_obj.CSCalib)
END

FUNCTION scatterbrain::qCalibGUI, GROUPLEADER = groupLeader, NOTIFY_OBJ = notifyObj, SHOWGUI = showGUI, FRAMENO = frameNo

  @as_scatterheader.macro

  IF N_Elements(frameNo) EQ 0 THEN frameNo = 0

  CASE frameNo OF
    0 : BEGIN
          qCalibGUI = self.qCalibGUI
          frame_obj = self.frame_obj
        END
    1 : BEGIN
          qCalibGUI = self.qCalibGUI2
          frame_obj = self.frame_obj2
        END
  ENDCASE 

  IF Obj_Valid(frame_obj) THEN configNo = frame_obj.frame.configNo ELSE configNo = 0
  
  IF Obj_Valid(qCalibGUI) THEN BEGIN
    IF KeyWord_Set(groupLeader) THEN qCalibGUI.SetProperty, GROUPLEADER = groupLeader
    self.scatterXMLGUI_obj.GetParameters, FRAME = frame
    IF N_Elements(frame) - 1 GE configNo THEN qCalibGUI.SetProperty, WAVELENGTH = frame[configNo].wlen, CAMERALENGTH = frame[configNo].len, DETECTORANGLE = frame[configNo].detAngle
    RETURN, qCalibGUI
  ENDIF
    
  IF KeyWord_Set(notifyObj) THEN notifyObj = [notify('qCalibChange',frame_obj), notifyObj] $
                              ELSE notifyObj = notify('qCalibChange',frame_obj)
  
  qCalibGUI = as_qcalibration(GROUPLEADER=groupLeader, NOTIFY_OBJ = notifyObj, SHOWGUI = KeyWord_Set(showGUI))
  self.scatterXMLGUI_obj.GetParameters, FRAME = frame
  IF N_Elements(frame)-1 GE configNo THEN qCalibGUI.SetProperty, WAVELENGTH = frame[configNo].wlen, CAMERALENGTH = frame[configNo].len
  RETURN, qCalibGUI

END

PRO scatterbrain::Help, topic 

  @as_scatterheader.macro

  Call_Method, topic, self.helpfile

END

PRO scatterbrain::Acquire

  @as_scatterheader.macro

  result = self.areaDetectorObj->Acquire(self.filenames.next)

END

FUNCTION scatterbrain::GetUserDir, dir

  @as_scatterheader.macro

  IF ~Arg_Present(dir) THEN dir = self.experimentDir

  pathList = list()
  FOREACH d, dir DO BEGIN
    IF d NE '' THEN path = d ELSE path = ''
    IF ~file_test(path) THEN CD, CURRENT = path
    IF StrUpCase(File_Basename(path)) EQ 'IMAGES' THEN path = File_Dirname(File_Dirname(path),/MARK_DIRECTORY)
    pathList.add, path
  ENDFOREACH
  RETURN, pathList.toArray()
END

PRO scatterBrain::NewExperimentCallback, event, data

  @as_scatterheader.macro

  CASE Tag_Names(event, /STRUCTURE_NAME) OF
  
    'DONE' : self.createFreshExperiment, PATH = event.path, EXPERIMENT = event.experimentName, LOGFILE = event.logfileName, DETECTOR = event.detector
    'TEMPLATE' : self.newXML, event
             
    ELSE :
  
  ENDCASE

END

PRO scatterBrain::MaskDefineCallback, event

  @as_scatterheader.macro

  CASE Tag_Names(event, /STRUCTURE_NAME) OF
    'MASKWINDOW'  :BEGIN
                     CASE event.event OF
                       'Close' : self.frame_obj.SetProperty, DEFINEMASKS = 0
                     ENDCASE
                   END
  ENDCASE
END

PRO scatterbrain::ContourCallback, event

  @as_scatterheader.macro

  CASE Tag_Names(Event, /STRUCTURE_NAME) OF
    'CONTOURBLANK'  :BEGIN
                       blankID = self.ProcessImage(event.name, /NOPLOT)
                       blank = self.profiles_obj.GetProfiles(blankID)
                       self.profiles_obj.DeleteProfile, blankID
                       self.contourPlot.SetBlank, Reform(blank.toArray()), event.name
                     END
    'DATCONTOURBLANK' : BEGIN
                          profile = self.ReadDat(event.name)
                          self.contourPlot.SetBlank, transpose([[profile.q.toarray()], [profile.intensity.toarray()], [profile.error.toarray()]]), File_Basename(event.name)
                        END
    'KILLCONTOUR' : BEGIN
                      IF Obj_Valid(self.datfileobject) EQ 1 THEN self.datfileobject.contourclosed
                    END
  ENDCASE
END

FUNCTION scatterBrain::ReadDat, filename

  OpenR, fileLun, filename, /GET_LUN
    tempStr = ''
    ReadF, fileLun, tempStr
    ReadF, fileLun, tempStr
    tempFlt = FltArr(3)
    q = List()
    i = List()
    e = List()
    WHILE ~EOF(fileLun) DO BEGIN
      ReadF, fileLun, tempFlt
      q.add, tempFlt[0]
      i.add, tempFlt[1]
      e.add, tempFlt[2]
    ENDWHILE
  Free_LUN, fileLun

  RETURN, {q : q, intensity : i, error : e}

END

PRO scatterBrain::PlotDat, filenames

  FOREACH filename, filenames DO BEGIN
    data = self.ReadDat(filename)
    q = data.q
    i = data.intensity
    e = data.error
    self.profiles_obj.AddProfile, q.ToArray(), i.ToArray(), e.ToArray(), File_Basename(filename), CONFIGNAME = 'dat', /PRENORM, PROFILEINDEX = profileIndex
  ENDFOREACH
  self.profiles_obj->UpdateProfileWidgets
  
END

PRO scatterBrain::ContourDat, filenames, UPDATE = update

  data = list()

  FOREACH file, filenames, key DO BEGIN
  
    temp = self.ReadDat(file)
    data.add, (temp.intensity).toArray()
    
    IF key EQ 0 THEN nPoints = (temp.q).count()
    
    IF (temp.q).count() NE nPoints THEN BEGIN
      result = Dialog_Message('Not all profiles have the same number of points. Returning...')
      RETURN
    ENDIF
  
  ENDFOREACH

  zContour = fltarr(nPoints,filenames.count())
  FOREACH profile, data, key DO zContour[*,key] = profile
  q = temp.q
  
  IF KeyWord_Set(update) EQ 1 AND Obj_Valid(self.contourPlot) EQ 1 THEN BEGIN
     self.contourPlot.addProfiles, zContour
  ENDIF ELSE BEGIN
    self.contourPlot = as__saxscontourplot(q.toArray(), indgen(filenames.count()), zContour, GROUPLEADER = self.wScatterBase, NOTIFYOBJ = notify('ContourCallback',self))
  ENDELSE
  

END

PRO scatterBrain::DatCallback, event

  @as_scatterheader.macro

  CASE Tag_Names(event, /STRUCTURE_NAME) OF
    'SELECT' : self.plotdat, event.filenames
    'CONTOUR': self.contourdat, event.filenames
    'CONTOURUPDATE' : self.contourdat, event.filenames, /UPDATE
  ENDCASE 
  
END

PRO scatterbrain::FrameCallback, event

  @as_scatterheader.macro

  CASE Tag_Names(event, /STRUCTURE_NAME) OF
    'FRAMEPLOTREQ' : BEGIN
                    self.frame_obj.GetProperty, CONFIGNAME = configName, TIME = time, I0SF = i0sf, IBSSF = ibssf
                    self.profiles_obj.AddProfile, event.q_arr.toArray(), event.profile.toArray(), event.error.toArray(), event.name, CONFIGNAME = configName, TIME = time, I0COUNT = i0sf, IBSCOUNT = ibssf, LIVE = live, NOPLOT = noPlot, PROFILEINDEX = profileIndex
                    WIDGET_CONTROL, Widget_Info(self.wScatterBase, FIND_BY_UNAME='SIMAGE'), SET_VALUE=name
                    self.profiles_obj->UpdateProfileWidgets
                  END
    'CONFIG'       : BEGIN
                       CASE StrUpCase(event.event) OF
                         'SAVE' : BEGIN
                                    result = Dialog_Message('Save Experiment?',/QUESTION)
                                    IF result EQ 'Yes' THEN BEGIN
                                      self.saveXML
                                    ENDIF
                                    IF self.pollEpics THEN BEGIN
                                      result = Dialog_Message('Export LUT?', /QUESTION)
                                      IF result EQ 'Yes' THEN self.ExportLUT
                                    ENDIF
                                  END
                         'LOAD' : BEGIN
                                    frameNo = Where([self.frame_obj,self.frame_obj2] EQ event.frameobj)
                                    result = self.qCalibGUI(ShowGUI = 0, frameNo = frameNo)
                                  END
                         ELSE :
                       ENDCASE
                     END
  ELSE :
  ENDCASE

END

PRO scatterbrain::UpgradeCallback, event

  @as_scatterheader.macro

  CASE Tag_Names(event, /STRUCTURE_NAME) OF
    'DOWNLOADPROGRESS' : BEGIN
                          IF event.downloaded EQ event.total THEN progress = 'Finished. Please restart scatterBrain.' $
                                                             ELSE progress = 'Downloaded ' + String(event.downloaded/2^20., FORMAT ='(F5.2)') + ' MB of ' + String(event.total/2^20., FORMAT ='(F5.2)') + ' MB'
                          print, progress
                          Widget_Control, self.updateprogressLabel, SET_VALUE = progress
                         END
    ELSE :
  ENDCASE
END

PRO scatterbrain::PlotControlCallback, event

  @as_scatterheader.macro

  CASE Tag_Names(event, /STRUCTURE_NAME) OF
    'PLOTSELECT' : BEGIN
                     fileName = event.filename
                     IF event.clicks EQ 2 THEN BEGIN
                       self.profiles_obj->SelectPlot, fileName, /DOUBLE
                       self.profiles_obj->UpdateProfileWidgets
                       self.profiles_obj->GetProperty, FNAME=fileName 
                       IF StrUpCase((StrSplit(filename, '.',/EXTRACT))[-1]) NE 'TIF' THEN BREAK
                       sf = self.frame_obj.GetImage(fileName)
                       Widget_Control, Widget_Info(self.wScatterBase, FIND_BY_UNAME = 'SIMAGE'), SET_VALUE = fileName
                     ENDIF
                   END
    'PLOTDROP'   : BEGIN
                     result = self.ProcessImage(event.filename)
                   END
    'PLOTDATDROP': BEGIN
                     self.PlotDat, event.filename
                   END
    'PLOTREPLOT' : BEGIN
                     result = self.ProcessImage(event.filename, DETECTORNO = event.detectorNo)
                   END
  ENDCASE

END

PRO scatterbrain::NewFrame, detID

  @as_scatterheader.macro

  ;logData = self.saxsControl->GetLogData()
  ;print, logData

  IF self.liveLog NE '' THEN self.scatterXMLGUI_obj->ParseFile, FILENAME = self.liveLog, /LOGONLY, /UPDATE
  liveFrameArray = list()
  FOR i = 0, N_Elements(detID) - 1 DO BEGIN
    IF detID[i] EQ 1 THEN BEGIN 
      liveFrame = self.areaDetectorObj->GetFrame(i)
      IF liveFrame EQ !NULL THEN RETURN
      liveFrameArray.add, liveFrame
      self.frame_obj.GetProperty, DIMENSIONS=oldDims
      IF Total(fix(oldDims)-(size(liveFrame))[1:2],/PRESERVE_TYPE) NE 0 THEN BEGIN
        result = Dialog_Message('Frame size changed, internal mask array cleared.', /INFORMATION)
        self.frame_obj->ClearMaskArray
      ENDIF
    ENDIF
  ENDFOR
  self.areaDetectorObj->GetProperty, 0, FULLFILENAME = fullFileName, FILEPATH = filePath
  IF (fullFileName EQ !NULL) + (filePath EQ !NULL) EQ 0 THEN BEGIN
    fileName = (StrSplit(fullfilename, filepath, /REGEX, /EXTRACT))[0]
    result = self.ProcessImage(fileName, LIVEFRAME = liveFrameArray)
    Widget_Control, Widget_Info(self.wScatterBase, FIND_BY_UNAME = 'SIMAGE'), SET_VALUE = fileName
  ENDIF

END

PRO scatterbrain::NewParams, paramObj

  @as_scatterheader.macro

  filenames = 1

  paramObj->GetParameters, FILENAMES=filenames
  
  fileNamesTags = Tag_Names(filenames)
  
  FOR i = 0, N_Elements(filenamesTags) - 1 DO BEGIN
  
    matchTag = Where(Tag_Names(self.filenames) EQ filenamesTags[i])
    IF matchTag GE 0 THEN BEGIN 
      
      IF Size(filenames.(i),/TYPE) EQ 10 AND Size(self.filenames.(matchTag),/TYPE) EQ 10 THEN BEGIN
          Ptr_Free, self.filenames.(matchTag)
          self.filenames.(matchTag) = filenames.(i)
      ENDIF 
      IF Size(filenames.(i),/TYPE) EQ 10 AND Size(self.filenames.(matchTag),/TYPE) NE 10 THEN BEGIN
         self.filenames.(matchTag) = *(filenames.(i))
      ENDIF 
      IF Size(filenames.(i),/TYPE) NE 10 AND Size(self.filenames.(matchTag),/TYPE) EQ 10 THEN BEGIN
         *(self.filenames.(matchTag)) = filenames.(i)
      ENDIF 
      IF Size(filenames.(i),/TYPE) NE 10 AND Size(self.filenames.(matchTag),/TYPE) NE 10 THEN self.filenames.(matchTag) = filenames.(i)
              
    ENDIF
   
  ENDFOR

END

PRO scatterbrain::LoadResources, UNLOAD=unload

  @as_scatterheader.macro

  IF KeyWord_Set(unload) THEN BEGIN
    Ptr_Free, self.resource
    RETURN
  ENDIF
  
  CD, CURRENT=current
  pathArr = [current, self.programDir, self.programDir + 'resources', StrSplit(!path,';',/EXTRACT)]
  count = 0
  path2Image = ''
  path2ColourTable = ''
  
  WHILE path2Image EQ '' AND count LT N_Elements(pathArr)  DO BEGIN
    path2Image = File_Search(pathArr[count] + path_sep() + 'scatterBrainResources.sav')
    count = count + 1
  ENDWHILE
  
  IF path2Image NE '' THEN BEGIN
    RESTORE, path2Image
    self.resource = Ptr_New(buttonImages)
  ENDIF
  
  count = 0
  WHILE path2ColourTable EQ '' AND count LT N_Elements(pathArr)  DO BEGIN
    path2ColourTable = File_Search(pathArr[count] + path_sep() + 'scatterColourTable.tbl')
    count = count + 1
  ENDWHILE
  
  IF path2ColourTable NE '' THEN BEGIN
    *self.resource = Create_Struct(*self.resource, 'COLOURTABLEFILE', path2ColourTable)
  ENDIF
      
END

FUNCTION scatterbrain::GetResource, resource

  @as_scatterheader.macro
  
  tagNum = Where(Tag_Names(*self.resource) EQ StrUpCase(resource))
  
  RETURN, (*self.resource).(tagNum)

END

FUNCTION scatterbrain::init     $
         , savedata=savedata $
         , epics=epics       $
         , autoproc=autoproc $
         , monitor=monitor   $
         , batch=batch       $
         , debug=debug       $
         , qdebug=qdebug     $
         , nodet=nodet       $
         , startExcel=startExcel $
         , nloglines=nloglines $
         , version=version

  @as_scatterheader.macro

  CD, current = current
  self.programDir = current + path_sep()
  
  ; Determine Operating System
  CASE StrUpCase(!Version.OS_Family) OF
      'WINDOWS': self.operatingSystem = 'Windows'
      'UNIX': BEGIN
                IF StrPos(!Version.OS_Name, 'Mac') GE 0 THEN BEGIN
                  self.operatingSystem = 'Mac' ; Macintosh
                ENDIF ELSE BEGIN
                  self.operatingSystem = 'Linux' ; other UNIX.
                ENDELSE
              END
       ELSE : self.operatingSystem = 'Other'   
  ENDCASE   

; INITIALISE PREDETERMINED CLASS VARIABLES
   self.dir    = 'no_name_yet'
   self.fext   = 'tif'
   self.fext2  = 'tif'               
   self.nameformat = '%s%3.3d'
   self.resetFileNumber = 0
   self.fileSelectList = List()

; Filenames
   self.filenames.timestamp   = ''               
   self.filenames.versionstr  = '   vXXXXX'     
   self.filenames.nseq        = 'no_name_yet'   
   self.filenames.dir         = 'no_name_yet'   
   self.filenames.mappeddir   = 'no_name_yet'   
   self.filenames.profdir     = 'no_name_yet'   
   ;self.filenames.log         = 'no_name_yet'   
   self.filenames.params      = 'no_name_yet'   
   self.filenames.batch       = 'no_name_yet'   
   self.filenames.next        = 'no_name_yet'   
   self.filenames.autoproc    = 'saxs_default'  
   self.filenames.autoplist   = ['saxs_default','saxs_archaeometry','saxs_fiber_analyze','saxs_herman_orient'] 
   self.filenames.autostr0    = ['Unused','Q datum','Q datum','Ref. Angle'] 
   self.filenames.autostr1    = ['Q-range','Qrng(Inten)','Q Range','Q Range']    
   self.filenames.autostr2    = ['Unused','Qrng(Total)','Int.Range','Ang. Range'] 
   self.filenames.autostr3    = ['Unused','Qrng(Power)','Med, Stdev','N angles']   
   self.filenames.autostr4    = ['Unused','Qrng(Peaks)','Box, Offset','Unused']     
   self.filenames.autostr5    = ['Unused','Unused','Window','Unused']     
   self.filenames.autopout    = 'no_name_yet'   
   self.filenames.autopbat    = 0               
   self.filenames.psplot      = 'saxs15id.ps'

   self.currentlySelected = List()

   self.settingsObj = as_scatterbrainsettings()
   self.settingsObj.ParseFile
   
   validPath = 0b
   startingDirectory = self.settingsObj.startingDirectory1
    WHILE validPath EQ 0 DO BEGIN
      validPath = File_Test(startingDirectory, /DIRECTORY)
      IF ~validPath THEN BEGIN
        startingDirectoryTemp = File_DirName(startingDirectory)
        IF startingDirectory EQ startingDirectoryTemp THEN startingDirectory = current ELSE startingDirectory = startingDirectoryTemp
      ENDIF
    ENDWHILE 
   
    CD, startingDirectory
    
    IF self.settingsObj.autoCheckUpdates AND ~KeyWord_Set(epics) THEN BEGIN
    
      CASE self.operatingSystem OF 
        'Windows' : programName = 'scatterbrainanalysiswin'
        'Mac'     : programName = 'scatterbrainanalysisosx'
        'Linux'   : programName = 'scatterbrainanalysislinux'
        ELSE : programName = ''
      ENDCASE   
      IF programName NE '' THEN BEGIN
        updateObj = as_upgradeversion(programName, 'http://www.synchrotron.org.au/images/beamlines/saxswaxs/scatterbrain/',self.programDir)
        newVersion = updateObj.newestVersion(MESSAGE = versionMessage, /SILENT)
        Obj_Destroy, updateObj
        
        IF version LT newVersion THEN BEGIN
          result = Dialog_Message(['Newer Version ' + StrCompress(String(newVersion, FORMAT = '(D7.3)')) + ' is available. Please go to Help->Check for Updates to upgrade.', 'Version notes:', as_splitmessage(versionMessage,500)],/INFORMATION)
        ENDIF
      ENDIF
    ENDIF
    
    IF KeyWord_Set(epics) THEN BEGIN
    
      pollEpics = epics 
      cainit
      casettimeout, 0.1
      casetretrycount, 10
    ENDIF ELSE pollEpics = 0
    
    IF ~KeyWord_Set(version) THEN version = 0    
;    filenames.versionstr = ' v0.2'
    versionstr = ' v' + StrCompress(String(version, FORMAT = '(d7.3)'),/REMOVE_ALL)
    IF ~KeyWord_Set(epics) THEN versionstr = versionstr + ' Analysis'
    wScatterBase = Widget_Base(/ROW, UNAME='SAXS_BASE', XOFFSET=0 ,YOFFSET=0,TITLE='scatterBrain' + versionStr $
             ,SPACE=3 ,XPAD=3 ,YPAD=3,MBAR=SAXS_BASE_MBAR, MAP = 0, /TLB_SIZE_EVENTS, NOTIFY_REALIZE='scatterbrain_realize')
    
    Widget_Control, wScatterBase, SET_UVALUE = self
    
;    wProfileBase = Widget_Base(GROUP_LEADER=wScatterBase, /COLUMN, /FLOATING, UNAME='PROFILE_BASE',XOFFSET=750 ,YOFFSET=0)

    MENU_FILE = Widget_Button(SAXS_BASE_MBAR, UNAME='MENU_FILE' ,/MENU,VALUE='File')

;    FILE_IMAGE = Widget_Button(MENU_FILE, UNAME='FILE_OPEN',/MENU,VALUE='Process SAXS Image')

    FILE_IMPRCS = Widget_Button(MENU_FILE, UNAME='OPEN IMAGES',VALUE='Open Image(s)')

;    FILE_IMPRCM = Widget_Button(FILE_IMAGE, UNAME='IMG_PROC_M',VALUE='Average Images')
;
;    FILE_IMGSER = Widget_Button(FILE_IMAGE, UNAME='IMG_PROC_SERIES',VALUE='Image Series')

;    FILE_GET_LOG = Widget_Button(MENU_FILE, UNAME = 'FILE_GET_LOG', VALUE= 'Get Scan Log')
;    
;    FILE_GET_PARMS = Widget_Button(MENU_FILE, UNAME = 'FILE_GET_SAX1' $
;                        , VALUE= 'Get SAXS Parameters')

     FILE_NEW_XML = Widget_Button(MENU_FILE, VALUE ='Create New Experiment...', SENSITIVE = 1, /MENU)
     
     IF KeyWord_Set(epics) THEN BEGIN
       FILE_NEW_EMPTY_XML = Widget_Button(FILE_NEW_XML, VALUE ='Create New Empty Experiment From Template', SENSITIVE = 1, UNAME = 'NEW EXPERIMENT TEMPLATE')
       FILE_NEW_EMPTY_XML = Widget_Button(FILE_NEW_XML, VALUE ='Create New Empty Experiment From Current', SENSITIVE = 0, UNAME = 'NEW EXPERIMENT EMPTY')
       FILE_NEW_FILES_XML = Widget_Button(FILE_NEW_XML, VALUE ='Create New Experiment with Currently Selected Files', SENSITIVE = 0, UNAME = 'NEW EXPERIMENT FILES')
     ENDIF
     
     FILE_GET_XML = Widget_Button(MENU_FILE, VALUE = 'Get Experiment', UNAME = 'LOAD XML')
     
     FILE_SAVE_XML = Widget_Button(MENU_FILE, VALUE = 'Save Experiment', UNAME = 'SAVE XML')
     
     FILE_SAVE_XML = Widget_Button(MENU_FILE, VALUE = 'Save As Experiment ...', UNAME = 'SAVE AS XML')
    
;    FILE_SAV_PROFS = Widget_Button(MENU_FILE,UNAME ='FILE_SAV_PROFS',VALUE='Save Profiles',/menu)
;
;    FILE_SAV_PROFS_SF = Widget_Button(FILE_SAV_PROFS, UNAME = 'FILE_SAV_PROFS_SF' $
;                        , VALUE = 'N profiles -> 1 File    (ASCII Text)')
;
;    FILE_SAV_PROFS_SF = Widget_Button(FILE_SAV_PROFS, UNAME = 'FILE_SAV_PROFS_MF' $
;                        , VALUE = 'N profiles -> N Files  (ASCII Text)')
;
;    FILE_SAV_PROFS_SF = Widget_Button(FILE_SAV_PROFS, UNAME = 'FILE_SAV_PROFS_OT' $
;                        , VALUE = 'N profiles -> OTOKO/ASCII Format)')
;
;    FILE_SAV_PROFS_PS = Widget_Button(FILE_SAV_PROFS $
;                        , VALUE = 'Print Profiles Image   (Postscript)',/menu)
;    FILE_SAV_PROFS_PSCOL = Widget_Button(FILE_SAV_PROFS_PS, UNAME = 'FILE_SAV_PROFS_PSCOL', VALUE = 'PostScript Color')
;    FILE_SAV_PROFS_PSCOL = Widget_Button(FILE_SAV_PROFS_PS, UNAME = 'FILE_SAV_PROFS_PSBW', VALUE = 'PostScript B/W')
;
;    FILE_SAV_PROFS_SUM = Widget_Button(FILE_SAV_PROFS, UNAME = 'FILE_SAV_PROFS_SUMMARY' $
;                        , VALUE = 'Save Profile Summary (text)')
;
;    FILE_SAV_PROFS_SF = Widget_Button(FILE_SAV_PROFS, UNAME = 'FILE_SAV_PROFS_SESSION' $
;                        , VALUE = 'Save complete session  (Large Binary File)')
;
;    FILE_IMPORT_PROFS = Widget_Button(MENU_FILE, UNAME = 'FILE_IMPORT_PROF', VALUE = 'Import Profile')
;
;    FILE_CLEAR = Widget_Button(MENU_FILE, UNAME = 'FILE_CLEAR', VALUE= 'Clear All Profiles')
;
;    FILE_RESTORE = Widget_Button(MENU_FILE, UNAME = 'FILE_RESTORE_SESSION', VALUE= 'Restore profiles/session')
;
    
    MENU_RECENT = Widget_Button(MENU_FILE, VALUE='Recent Files...',/MENU)
    
    recentFiles = self.settingsObj.recentFile
    
    FOREACH file, recentFiles, key DO BEGIN
      stringKey = StrCompress(key,/REMOVE_ALL)
      FILE_RECENT = Widget_Button(MENU_RECENT, VALUE=stringKey + ': ' + file, UNAME='RECENT_FILE_' + stringKey, UVALUE=file)
    ENDFOREACH
    
    FILE_EXIT = Widget_Button(MENU_FILE, UNAME='FILE_EXIT',VALUE='Exit', EVENT_PRO = 'scatterBrain_Quit')

    MENU_ACQUIRE = Widget_Button(SAXS_BASE_MBAR, UNAME='MENU_ACQUIRE',/MENU, VALUE='Acquire')

    ACQUIRE_LIVELOG = Widget_Button(MENU_ACQUIRE, VALUE = 'Select Live Log', UNAME='ACQ_LIVELOG')
    
    ACQUIRE_DATAPATH = Widget_Button(MENU_ACQUIRE, VALUE = 'Change 1st Frame Data Path', UNAME='DATA PATH')
    
    ACQUIRE_DATAPATH2 = Widget_Button(MENU_ACQUIRE, VALUE = 'Change 2nd Frame Data Path', SENSITIVE = 0, UNAME='DATA PATH 2')

    IF pollEpics GT 0 THEN BEGIN
                
        ACQUIRE_SETUP = Widget_Button(MENU_ACQUIRE, UNAME='ACQ_AREADETECTOR' $
                      , VALUE='Area Detector')
        ACQUIRE_SETUP = Widget_Button(MENU_ACQUIRE, UNAME='ACQ_PVMAP' $
                      , VALUE='PV Map')
        MENU_SCAN = Widget_Button(SAXS_BASE_MBAR, UNAME='MENU_SCAN', /MENU, VALUE='Scan')
        
        SCAN_MODE = Widget_Button(MENU_SCAN, /CHECKED_MENU, VALUE = 'Scan Mode', UNAME='SCAN_MODE')
        
        INITIALISE_SCAN = Widget_Button(MENU_SCAN, VALUE = 'Initialise Scan', SENSITIVE = 0, UNAME='INITIALISE_SCAN')
        
        REVEAL_EXCEL = Widget_Button(MENU_SCAN, VALUE = 'Reveal Excel', SENSITIVE = 1, UNAME='REVEAL_EXCEL')

        ACQUIRE_EXPORT_LUT = Widget_Button(MENU_ACQUIRE, VALUE = 'Export LUT', UNAME = 'ACQ_EXPORTLUT')
        
        ACQUIRE_EXPORT_TEMPLATE = Widget_Button(MENU_ACQUIRE, VALUE = 'Export Experiment Template', UNAME = 'ACQ_EXPORTTEMPLATE')
;        
;        ACQUIRE_SETUP = Widget_Button(MENU_ACQUIRE, UNAME='ACQ_SETUP' $
;                      , VALUE='Setup')
;
;        ACQUIRE_SETUP = Widget_Button(MENU_ACQUIRE, UNAME='ACQ_USERMOTORS' $
;                      , VALUE='User PVs')
;
;        ACQUIRE_MOVEXP = Widget_Button(MENU_ACQUIRE, UNAME='ACQ_MOVEXP' $
;                      , VALUE='Move & Expose')
;
;        ACQUIRE_MOVEXP = Widget_Button(MENU_ACQUIRE, UNAME='ACQ_MOVMON' $
;                      , VALUE='Move & Monitor')
    ENDIF ELSE BEGIN
      ACQUIRE_POLLLIVELOG = Widget_Button(MENU_ACQUIRE, VALUE = 'Poll Live Log', /CHECKED_MENU, UNAME='ACQ_POLLLIVELOG')
    END

    MENU_TOOLS = Widget_Button(SAXS_BASE_MBAR, UNAME='MENU_TOOLS',VALUE='Tools')
    
    DAT_MODE = Widget_Button(MENU_TOOLS, VALUE = 'Dat File Mode', UNAME = 'DAT FILE MODE')
    
    EXPORT_IMAGE = Widget_Button(MENU_TOOLS, VALUE = 'Export Image', /MENU)
    
    EXPORT_IMAGE_CURRENT = Widget_Button(EXPORT_IMAGE, VALUE = 'Export Current Image', UNAME = 'EXPORT IMAGE')
    
    EXPORT_IMAGE_MASKS = Widget_Button(EXPORT_IMAGE, VALUE = 'Export Current Image With Annotations', UNAME = 'EXPORT IMAGE ALL')
    
    EXPORT_SHORT_EXPERIMENT = Widget_Button(FILE_NEW_XML, VALUE ='Export Experiment File with Only Currently Selected Files', SENSITIVE = 0, UNAME = 'EXPORT EXPERIMENT FILES')
    
    SET_ZINGER_MASK = Widget_Button(MENU_TOOLS, VALUE = 'Set Zinger Mask', UNAME = 'SET ZINGER MASK')
    
    CONVERT_SAXS15 = Widget_Button(MENU_TOOLS, VALUE = 'Convert SAXS15ID log/sax files to scatterBrain experiment', UNAME = 'CONVERT SAXS15')
    
    SWITCH_IT_IBS = Widget_Button(MENU_TOOLS, VALUE = 'Switch beamstop and transmission data', UNAME = 'SWITCH IBS IT')
    
    EXPORT_NATURE_PAPER = Widget_Button(MENU_TOOLS, VALUE = 'Export Nature Paper', UNAME = 'EXPORT NATURE PAPER')
    
    EXPORT_LOGFILE = Widget_Button(MENU_TOOLS, VALUE = 'Export Logfile', /MENU)
    
    EXPORT_LOGFILE_COMPLETE = Widget_Button(EXPORT_LOGFILE, VALUE = 'Including All Files', UNAME = 'EXPORT LOGFILE')
    
    EXPORT_LOGFILE_COMPLETE = Widget_Button(EXPORT_LOGFILE, VALUE = 'Selected Files Only', UNAME = 'EXPORT LOGFILE FILES')
    
    TWO_D_SUBTRACT = Widget_Button(MENU_TOOLS, VALUE = 'Two Dimensional Subtraction', UNAME = 'TWO DIMENSIONAL SUBTRACTION')
    

;    MENU_TOOLS = Widget_Button(SAXS_BASE_MBAR, UNAME='MENU_TOOLS',VALUE='Tools')
;
;    TOOLS_LOGFILE = Widget_Button(MENU_TOOLS, VALUE='Log File',/menu)
;
;    TOOLS_LOGFILE_DISP = Widget_Button(TOOLS_LOGFILE, UNAME='TOOLS_DISPLOG', VALUE='Display Current')
;
;    TOOLS_LOGFILE_DISP = Widget_Button(TOOLS_LOGFILE, UNAME='TOOLS_SUMLOG', VALUE='Summarize Current')
;
;    TOOLS_LOGFILE_OVRD = Widget_Button(TOOLS_LOGFILE, UNAME='TOOLS_LOGOVRD', VALUE='Override Log Values')
;
;    TOOLS_HEADER = Widget_Button(MENU_TOOLS, VALUE='Frame Header/Info', /menu)
;
;    TOOLS_HEADER_DISP = Widget_Button(TOOLS_HEADER, UNAME='TOOLS_CURFRMHEAD', VALUE='Display Current Frame Header')
;
;    TOOLS_HEADER_SERIES = Widget_Button(TOOLS_HEADER, UNAME='TOOLS_FRMHEAD', VALUE='Summarize Frame Series')
;
;    TOOLS_IMGMOD = Widget_Button(MENU_TOOLS, VALUE='Modify Image', /menu)
;
;    TOOLS_IMGMOD_MEDSM = Widget_Button(TOOLS_IMGMOD, VALUE='Median Smooth', UNAME='TOOLS_IMGMOD_MEDSM')
;
;    TOOLS_IMGMOD_MEDSM = Widget_Button(TOOLS_IMGMOD, VALUE='Boxcar Smooth', UNAME='TOOLS_IMGMOD_BOXSM')
;
;    TOOLS_IMGMOD_MEDSM = Widget_Button(TOOLS_IMGMOD, VALUE='Boxcar Standard Dev', UNAME='TOOLS_IMGMOD_BSDEV')
;
;    TOOLS_IMGMOD_SSUB = Widget_Button(TOOLS_IMGMOD, VALUE='Subtract Boxcar Average', UNAME='TOOLS_IMGMOD_SBOX')
;
;    TOOLS_IMGMOD_NORM = Widget_Button(TOOLS_IMGMOD, VALUE='Normalize Image', UNAME='TOOLS_IMGMOD_NORM',/MENU)
;
;    TOOLS_IMGMOD_NORMIO = Widget_Button(TOOLS_IMGMOD_NORM, VALUE='to Io', UNAME='TOOLS_IMGMOD_NORMIO')
;
;    TOOLS_IMGMOD_NORMBS = Widget_Button(TOOLS_IMGMOD_NORM, VALUE='to Ibs', UNAME='TOOLS_IMGMOD_NORMBS')
;
;    TOOLS_IMGMOD_ICORR = Widget_Button(TOOLS_IMGMOD, VALUE='Image Correction', /menu)
;
;    TOOLS_IMGMOD_UNWARP = Widget_Button(TOOLS_IMGMOD_ICORR, VALUE='Unwarp Image', UNAME='TOOLS_IMGMOD_UNWARP')
;
;    TOOLS_IMGMOD_FIXOS = Widget_Button(TOOLS_IMGMOD_ICORR, VALUE='Fix Sector Offsets', UNAME='TOOLS_IMGMOD_FIXOS')
;
;    TOOLS_IMGMOD_IREG = Widget_Button(TOOLS_IMGMOD, VALUE='Insert ROI to Current Frame', /MENU)
;
;    TOOLS_IMGMOD_IREG1 = Widget_Button(TOOLS_IMGMOD_IREG, VALUE='Cut/Paste to ROI', UNAME='TOOLS_IMGMOD_IREG_1')
;
;    TOOLS_IMGMOD_IREG2 = Widget_Button(TOOLS_IMGMOD_IREG, VALUE='Average Outside ROI', UNAME='TOOLS_IMGMOD_IREG_2')
;
;    TOOLS_GENIMG = Widget_Button(MENU_TOOLS, VALUE='Generate Image', /menu)
;
;    TOOLS_GENIMG_SWEE = Widget_Button(TOOLS_GENIMG, VALUE='... from current profile', UNAME='TOOLS_GENIMG_SWEEP')
;
;    TOOLS_GENIMG_SSUB = Widget_Button(TOOLS_GENIMG, VALUE='Psuedo Flood Field', UNAME='TOOLS_GENIMG_FLOOD')
;
;    TOOLS_GENIMG_PLAW = Widget_Button(TOOLS_GENIMG, VALUE='Power law pattern', UNAME='TOOLS_GENIMG_PLAW')
;
;    TOOLS_PROF = Widget_Button(MENU_TOOLS, VALUE='Profile', /menu)
;
;    TOOLS_PROF_ALG = Widget_Button(TOOLS_PROF, VALUE='Algebraic Manipulations', UNAME='TOOLS_PROF_ALG')
;
;    TOOLS_PROF_AZIM = Widget_Button(TOOLS_PROF, VALUE='Azimuthal Arc', UNAME='TOOLS_PROF_AZIM')
;
;    TOOLS_PROF_CONS = Widget_Button(TOOLS_PROF, VALUE='Consolidate', UNAME='TOOLS_PROF_CONS')
;
;    TOOLS_PROF_MSEQ = Widget_Button(TOOLS_PROF, VALUE='Mask Sequence', UNAME='TOOLS_PROF_MSEQ')
;
;    TOOLS_MON = Widget_Button(MENU_TOOLS, UNAME='TOOLS_MON',VALUE='Counter Check')
;
;    TOOLS_IMG_OUT = Widget_Button(MENU_TOOLS, VALUE='Output Images',UNAME='TOOLS_IMAGE_OUT')
;
;    TOOLS_IMG_PREV = Widget_Button(MENU_TOOLS, VALUE='Preview Images',UNAME='TOOLS_IMAGE_PREVIEW')
;
;    MENU_DATA = Widget_Button(SAXS_BASE_MBAR, UNAME='MENU_PROCESS',VALUE='Data')
;
;    DATA_PRANGE = Widget_Button(MENU_DATA, UNAME='DATA_PRANGE',VALUE='Plot Range')
;
;    DATA_QRANGE = Widget_Button(MENU_DATA, UNAME='DATA_QRANGE',VALUE='Q-Range')
;
;    DATA_NORM = Widget_Button(MENU_DATA, UNAME='DATA_NORM',VALUE='Normalization')
;
;    DATA_SAVE_PARMS = Widget_Button(MENU_DATA, UNAME = 'DATA_SAVE_PARMS' $
;                        , VALUE= 'Save SAXS Parameters')
;
;    MENU_DETECTOR = Widget_Button(SAXS_BASE_MBAR, UNAME='MENU_DET', VALUE='Detector')
;
;    DET_MASK = Widget_Button(MENU_DETECTOR, UNAME='DET_MASK', VALUE='Area Masks')
;
;    DET_PARAM = Widget_Button(MENU_DETECTOR, UNAME='DET_PARAM',VALUE='Frame Parameters')
;
;    DET_SPAT = Widget_Button(MENU_DETECTOR, UNAME='DET_SPAT',VALUE='Spatial Correction')
;
;    MENU_OPTIONS = Widget_Button(SAXS_BASE_MBAR, UNAME='MENU_OPTIONS',/MENU, VALUE='Options')
    MENU_SETTINGS = Widget_Button(SAXS_BASE_MBAR, UNAME='MENU_SET',VALUE='Settings')
    
    wSecondFrame = Widget_Button(MENU_SETTINGS, /CHECKED_MENU, VALUE = 'Use 2nd Detector', UNAME = '2ND DETECTOR')
    
    SETTINGS_GENERAL = Widget_Button(MENU_SETTINGS, VALUE = 'General Settings', UNAME = 'GENERAL SETTINGS')

     wHelpMenu = Widget_Button(SAXS_BASE_MBAR, UNAME='HELP_MENU',/MENU, VALUE='Help')
     
;    DOPTIONS = Widget_Button(MENU_OPTIONS, UNAME='DOPTIONS',VALUE='Main Display')
;
     SCATTER_WIKI = Widget_Button(wHelpMenu, UNAME='WIKI',VALUE='WIKI Manual.')

     SCATTER_HELP = Widget_Button(wHelpMenu, UNAME='HELP',VALUE='Help')
     
     CONTEXT_HELP = Widget_Button(wHelpMenu, UNAME='FRAME_HELP',VALUE='Context Help') 
    
     ABOUT = Widget_Button(wHelpMenu, UNAME='ABOUT',VALUE='About scatterBrain')
     
     VERSION_NOTES = Widget_Button(wHelpMenu, UNAME='VERSION_NOTES',VALUE='Version Notes')
     
     UPDATE = Widget_Button(wHelpMenu, VALUE = 'Check for Updates', UNAME = 'CHECK UPDATE')

    wLeftScatterBase = Widget_Base(wScatterBase, /COLUMN, /FRAME)
    wRightScatterBase = Widget_Base(wScatterBase, /COLUMN, /FRAME)
    wTopScatterBase = Widget_Base(wRightScatterBase, /ROW)                           ; Top half of saxs screen
    wLeftTopScatterBase = Widget_Base(wTopScatterBase,/COLUMN)
    wAcquireTopScatterBase = Widget_Base(wLeftTopScatterBase, /ROW)
    wTopBuffer = Widget_Label(wTopScatterBase, VALUE = '', SCR_XSIZE = 40)
    wRightTopScatterBase = Widget_Base(wTopScatterBase, /COLUMN)
    ;wTopProfileBase = WIDGET_BASE(wProfileBase,/COLUMN)
    wBotScatterBase = Widget_Base(wRightScatterBase,/COLUMN)                           ; Bottom half of saxs screen
    ;wBotProfileBase = WIDGET_BASE(wProfileBase,/ROW)                           
    
    ; Create four rows in the top left hand block on the main screen
    
    nScatterRows = 2
    
    wScatterRows = LonArr(nScatterRows)
    
    FOR i = 0, nScatterRows - 1 DO wScatterRows[i] = WIDGET_BASE(wLeftTopScatterBase,/ROW,/BASE_ALIGN_CENTER)
    
      
    ; Set out widgets in top left block including directory and file name boxes

    if pollEpics GT 0 then begin
      labelFont = 'Arial*BOLD*14'
      nScatterColumns = 4
      wScatterColumns = LonArr(nScatterColumns)
      
      wScatterColumns[0] = WIDGET_BASE(wScatterRows[0],/COLUMN,/BASE_ALIGN_BOTTOM, /ALIGN_BOTTOM)
      
      readExposureLabel = Widget_Label(wScatterColumns[0], VALUE='-', /DYNAMIC_RESIZE, FONT = labelFont, /ALIGN_CENTER, UNAME = 'REXPTIME')
      exposeBase = Widget_Base(wScatterColumns[0], /ROW, /ALIGN_CENTER)
      texpLabel = Widget_Label(exposeBase, VALUE = 'Time: ')
      texp = Widget_Combobox(exposeBase, /DYNAMIC_RESIZE, /EDITABLE, UNAME = 'TEXP' $
        , value=['1','2','5','10','20','30','40','60','120'], sensitive=1)
    
            
      buffer = Widget_Label(wScatterRows[0], VALUE = '', SCR_XSIZE = 5)
      fileLabelBase = Widget_Base(wScatterRows[0], /COLUMN, /ALIGN_BOTTOM)
      nextFileLabel = Widget_Label(fileLabelBase, VALUE='Next File: ')
      fileLabelBuf = Widget_Label(fileLabelBase, VALUE = ' ', SCR_YSIZE = 5)
      
      wScatterColumns[1] = WIDGET_BASE(wScatterRows[0],/COLUMN,/BASE_ALIGN_BOTTOM,/ALIGN_BOTTOM)
      
      readFileLabel = Widget_Label(wScatterColumns[1], VALUE = 'No Filename Entered', /DYNAMIC_RESIZE, /ALIGN_LEFT, FONT = labelFont, UNAME = 'RIMAGE')
      fileBuf = Widget_Label(wScatterColumns[1], VALUE = ' ', SCR_YSIZE = 1)
      nimage = Widget_Text(wScatterColumns[1],xsize=20, ysize = 1, /ALL_EVENTS, /EDITABLE  $
            , VALUE = '', FONT = labelFont, UNAME='NIMAGE')
      ;      , VALUE = profdata.lastname,UNAME='NIMAGE')
      fileBuf = Widget_Label(wScatterColumns[1], VALUE = ' ', SCR_YSIZE = 1)
      
      buffer = Widget_Label(wScatterRows[0], VALUE = '', SCR_XSIZE = 5)
      
      wScatterColumns[2] = WIDGET_BASE(wScatterRows[0],/COLUMN,/BASE_ALIGN_BOTTOM,/ALIGN_BOTTOM)
      readIndex = Widget_Label(wScatterColumns[2], VALUE = '99999', FONT = labelFont, UNAME = 'RINDEX', /ALIGN_CENTER)
      index = FSC_InputField(wScatterColumns[2],VALUE='1',TITLE='Index: ', XSIZE = 5, /INTEGER, /CR_Only, Event_Pro='scatterbrain_event', NAME='INDEX')
      
      buffer = Widget_Label(wScatterRows[0], VALUE = '', SCR_XSIZE = 5)
      wScatterColumns[3] = WIDGET_BASE(wScatterRows[0],/COLUMN,/BASE_ALIGN_BOTTOM,/ALIGN_BOTTOM)
      numImages = Widget_Label(wScatterColumns[3], VALUE = '99999', FONT = labelFont, UNAME = 'RNUMIMAGES', /ALIGN_CENTER)
      numImages = FSC_InputField(wScatterColumns[3],VALUE='1',TITLE='Images: ', XSIZE = 5, /INTEGER, /CR_Only, Event_Pro='scatterbrain_event', NAME='NUMIMAGES')
      
    endif

;    maxfnum = CW_FIELD(wScatterRows[3],xsize=4,/NOEDIT, TITLE = ' Max Index ' $
;          ,VALUE = 0,UNAME='MAXFNUM')

    CASE !VERSION.OS_FAMILY OF 
      'Windows'     : currentImageFont = 'Arial*18*BOLD'
      'unix'        : currentImageFont = 'lucidasans-18'
    ENDCASE
    
    simage = Widget_Label(wScatterRows[1],xsize=200, FONT = currentImageFont, VALUE = 'Current Image Frame' $
          ,UNAME='SIMAGE')
    ;      ,VALUE = profdata.imgfname[1],UNAME='SIMAGE')

    IF pollEpics GT 0 THEN BEGIN
      userMessage = 'Current Action In Progress is going to be written here'
      yMessSize = 100 
    ENDIF ELSE BEGIN
      userMessage = ''
      yMessSize = 0
    ENDELSE
    
    userMessage = as_splitmessage(userMessage, 150)
    mesbox = Widget_Label(wRightTopScatterBase,VALUE = userMessage $    
           , xsize = 150, ysize = yMessSize, SENSITIVE=1, UNAME='MESBOX')


    ; Create bottom section of screen

    wBotScatterRow = WIDGET_BASE(wBotScatterBase, /ROW)
    wHistScatterRow = WIDGET_BASE(wBotScatterBase,/ROW)
    ;wBotProfileRow = WIDGET_BASE(wBotPRofileBase,/ROW)

    self.loadResources
    
    greendown = self.GetResource('greendown')
    greenup = self.GetResource('greenup')
    faundown = self.GetResource('faundown')
    faunup = self.GetResource('faunup')
    reddown = self.GetResource('reddown')
    redup = self.GetResource('redup')

    if pollEpics GT 0 then begin
;      actype = CW_BGROUP(wRightTopScatterBase,['Transmission','SAXS Exposure'] $
;              , SET_VALUE= 0, ROW=3,/EXCLUSIVE, UNAME='POLLTYPE')
      acstart = drawButton(wAcquireTopScatterBase, greendown, greenup, VALUE='Acquire', sensitive=1 $
              , scale = 0.4, textLocation = [0.5,0.5], textColour = [255,255,255], UNAME='START')
      actrans = drawButton(wAcquireTopScatterBase, faundown, faunup, VALUE='Pause Scan', sensitive=0 $
              , scale = 0.4, textLocation = [0.5,0.5], textColour = [255,255,255], UNAME='PAUSE')
      acstop = drawButton(wAcquireTopScatterBase, reddown, redup, VALUE='Abort',sensitive=1 $
              , scale = 0.4, textLocation = [0.5,0.5], textColour = [255,255,255], UNAME='STOP')
;      acAreaDetector = Widget_Button(wRightTopScatterBase, VALUE='Poll AreaDetector', sensitive=1 $
;              ,XSIZE = 8,YSIZE=30,UNAME='Poll AreaDetector')
      IF KeyWord_Set(startExcel) THEN self.excelObj = as_proteinexcelscans('SR13ID01HU02IOC02:','13PIL1:cam1:Acquire')
    endif ;else begin

    ; Create Profile Objects
    profilePalette_obj = Obj_New('IDLgrPalette')
    cTable = self.GetResource('COLOURTABLEFILE')
    profilePalette_obj->LoadCT, 42, FILE = cTable
    profilePalette_obj.GetProperty, RED_VALUES = red, GREEN_VALUES = green, BLUE_VALUES = blue
    red   = red[0:15]
    green = green[0:15]
    blue  = blue[0:15]
    profilePalette_obj.SetProperty, RED_VALUES = red, GREEN_VALUES = green, BLUE_VALUES = blue
    profiles_obj = Obj_New('AS_ProfileContainerObj', GROUPLEADER= wScatterBase, plotPalette = profilePalette_obj)
    profiles_obj.showErrorPlot, self.settingsObj.errorBars
    
;    ;Create box object for setting zoom on frame.
;    DATA = Transpose([[0,0,0,0,0],[0,0,0,0,0]])
;    boxObj = Obj_New('IDLgrPolyline',DATA,COLOR=[255,255,0])
;    
;    ;Create symbol object used for displaying clicked points for centreing routine.
;    self.centreingSymbol = Obj_New('IDLgrSymbol', SIZE = 5)
;    
;    ; Create text object for displaying messages on image
;    message_obj = Obj_New('IDLgrText',  STRING = '', VERTICAL_ALIGNMENT=1,COLOR = [0,0,0])
    
    ; Create qData Frame and Logfile Objects
    qData_obj = Obj_New('AS__SaxsQData')
    frame_obj = Obj_New('as__saxsimagegui', wBotScatterRow, qData_obj, profiles_obj, RESOURCE=self, MASKNOTIFY = notify('maskDefineCallback',self), NOTIFY = notify('FrameCallback',self))
    IF Size(self.settingsObj.zingerThresh, /TYPE) NE 0 THEN frame_obj.frame.zing = self.settingsObj.zingerThresh
     
    IF Size(self.settingsObj.binsize, /TYPE) NE 0 THEN BEGIN
      binsize = self.settingsObj.binsize
      IF binsize GE 0 THEN frame_obj.SetProperty, STEP = binsize
    ENDIF
    
    ;logFile_obj = Obj_New('AS_LogFileObjOld',FRAME=frame_obj,GROUP_LEADER=wLeftScatterBase, /DOCK)
    scatterXMLGUI_obj = Obj_New('as_scatterxmlgui', GROUP_LEADER=wLeftScatterBase, NOTIFYOBJECT = notify('LogFileSelected', self), /DOCK)
    ;saxsFile_obj = Obj_New('AS__SAXSFileObj', NOTIFYOBJS=[frame_obj,self])
    
        
    ; Realise widget hierarchy
    WIDGET_CONTROL, wScatterBase, /REALIZE
    WIDGET_CONTROL, wScatterBase, /MAP      ; Map base - this makes it appear all at once, looks much nicer.
    
    IF pollEpics GT 0 THEN BEGIN
      WIDGET_CONTROL, readFileLabel, TIMER = 1       
    ENDIF

    ; Put geometry into User Value of mesbox so if TLB resised we can resize the message box
    WIDGET_CONTROL, mesbox, SET_UVALUE = (Widget_Info(wLeftScatterBase,/GEOM)).XSIZE + (Widget_Info(wLeftTopScatterBase,/GEOM)).XSIZE +(Widget_Info(wTopBuffer,/GEOM)).XSIZE 
;    framePalette_obj = Obj_New('IDLgrPalette')
;    framePalette_obj->LoadCT,0

    ;Create areaDetector Object
    IF pollEpics GT 0 THEN BEGIN
      self.areaDetectorObj = Obj_New('as_areadetectormap', NOTIFYOBJ = notify('newFrame',self))
      self.areaDetectorObj->Poll
    ENDIF

    ;Create saxscontrol object, which includes the PV Map.
    ; TODO Need to start using saxscontrol - not using it at this stage.
;    notifyAcquire = { object: self, method: 'acquire' }
;    self.saxsControl = Obj_New('as_saxscontrol', NOTIFYOBJ = notifyAcquire)

    ; Create plot control object
    plotControl_obj = AS_PlotControl(GROUPLEADER = wScatterBase, FRAMEOBJ = frame_obj, PLOTPALETTE = profilePalette_obj, PROFILES_OBJ = profiles_obj, POLLEPICS=pollEpics, RESOURCE=self, NOTIFYOBJ = notify('PlotControlCallback',self), DOCK = 1)
    profiles_obj->SetProperty, NOTIFY_OBJ = notify('Callback',plotControl_obj)
    
    frame_obj->SetProperty, LOGOBJ=scatterXMLGUI_obj
            
    ; Rest of objects    
;    frameModel_obj = Obj_New('IDLgrModel')
;    frameView_obj = Obj_New('IDLgrView')
;    frame_obj->SetProperty, FRAMEVIEWOBJ= frameView_obj,PALETTE=framePalette_obj, LOGOBJ=logFile_obj
;    frameModel_obj->Add, frame_obj
;    frameModel_obj->Add, boxObj
;    frameModel_obj->Add, message_obj
;    frameView_obj->Add, frameModel_obj
;        
        
    ; Create container object to hold objects not accessible via other means, to enable easy cleanup
    cleanup_obj = Obj_New('IDL_Container')
    
    ; Create help object
    self.helpFile = scatterBrainHelp(TITLE='scatterBrain Help', GROUP_LEADER = wScatterBase)
     
   ; WIDGET_CONTROL, wProfileBase,/REALIZE
    WIDGET_CONTROL, wScatterBase, TLB_GET_SIZE = base_size
    
;    Widget_Control, fdraw, GET_VALUE = frameWin
    ;Widget_Control, pdraw, GET_VALUE = profileWin
   
    ;frame_obj->SetProperty, FRAMEWINOBJ = frameWin
        
    aux_base_size = base_size - [512,512]                       

    ;frameWin->SetProperty, GRAPHICS_TREE = frameView_Obj

    ;frameWin->Draw

    ;Populate Newly Updated Class Variables
    
    self.wScatterBase       = wScatterBase
    self.frame_obj          = frame_obj 
    self.qData_obj          = qData_obj         
    ;self.boxObj             = boxObj             
    ;self.message_obj        = message_obj        
    ;self.frameModel_obj     = frameModel_obj     
    ;self.frameView_obj      = frameView_obj      
    ;self.frameWin           = frameWin
    self.scatterXMLGUI_obj = scatterXMLGUI_obj        
    ;self.logFile_obj        = logFile_Obj
    ;self.saxsFile_obj       = saxsFile_obj       
    self.profiles_obj       = profiles_obj       
    self.plotControl_obj    = plotControl_obj    
    self.profilePalette_obj = profilePalette_obj 
    self.aux_base_size      = aux_base_size                      
    ;self.framePalette_obj   = framePalette_obj   
    ;self.wHistBut           = wHistBut           
    self.cleanup_obj        = cleanup_obj        
    self.pollEpics          = pollEpics
    self.version            = version

    ; Create Q Calibration Objects
    self.qCalibGUI = self.qCalibGUI(GROUPLEADER=self.wScatterBase, ShowGUI = 0, frameNo = 0)
    self.qCalibGUI2 = self.qCalibGUI(GROUPLEADER=self.wScatterBase, ShowGUI = 0, frameNo = 1)
        
    
       
    XMANAGER, 'scatterbrain', wScatterBase, /NO_BLOCK, CLEANUP = 'scatterBrain_CleanUp'
      
    RETURN, 1
     
END

PRO scatterbrain__define

filenames = {filenames,               $ ; NAMES OF FILES USED IN SAXS15ID
             timestamp:     '',       $ ; time stamp string for file creation
             versionstr:    '',       $ ; this shows up at the top of the main panel
             nseq:          '',       $ ; number of frames in summing sequence
             dir:           '',       $ ; name of directory holding frame data
             mappeddir:     '',       $ ; frame name directory as seen from CCD server computer
             profdir:       '',       $ ; name of directory for storing integrated profiles
             log:           '',       $ ; name of log file for frame data
             params:        '',       $ ; name of parameter file for camera data
             batch:         '',       $ ; name of batch file used for detector ops
             next:          '',       $ ; file name for next frame to be acquired
             autoproc:      '',       $ ; name of currently used autoproc procedure
             autoplist:     StrArr(4),$ ; list of available autoproc procedures
             autostr0:      StrArr(4),$
             autostr1:      StrArr(4),$
             autostr2:      StrArr(4),$
             autostr3:      StrArr(4),$
             autostr4:      StrArr(4),$
             autostr5:      StrArr(4),$
             autopout:      '',       $ ; name of autoproc output file
             autopbat:      0,        $ ; =1 if autoproc is be run in a batch mode
             psplot:        ''        $ ; name of postscript output
       }

void = {scatterbrain, $
       filenames          : filenames,       $
       experimentDir      : '',              $
       imagesDir          : '',              $
       areaDetectorObj    : Obj_New(),       $
       saxsControl        : Obj_New(),       $
       wScatterBase       : 0L,              $
       wHistBut           : 0L,              $
       histWin            : 0L,              $
       settingsBase       : 0L,              $
       frame_obj          : Obj_New(),       $
       frame_obj2         : Obj_New(),       $ ; second frame if doing saxs/waxs
       qData_Obj          : Obj_New(),       $
       qData_Obj2         : Obj_New(),       $ ; qdata for second frame (saxs/waxs)
       boxObj             : Obj_New(),       $
       message_obj        : Obj_New(),       $
       frameModel_obj     : Obj_New(),       $
       frameView_obj      : Obj_New(),       $
       scatterXMLGUI_obj  : Obj_New(),       $
       profiles_obj       : Obj_New(),       $
       plotControl_obj    : Obj_New(),       $
       profilePalette_obj : Obj_New(),       $
       framePalette_obj   : Obj_New(),       $
       frameWin           : Obj_New(),       $
       cleanup_obj        : Obj_New(),       $
       pollAreaDetector   : Obj_New(),       $
       centreingPoints    : Obj_New(),       $
       centreingSymbol    : Obj_New(),       $
       contourPlot        : Obj_New(),       $
       pollEpics          : 0,               $
       boxActive          : 0,               $
       profileLineActive  : 0,               $
       findingCentre      : 0,               $
       scanMode           : 0,               $  
       aux_base_size      : IntArr(2),       $
       centreing          : IntArr(6),       $
       liveLog            : '',              $
       dir                : '',              $
       fext               : '',              $
       fext2              : '',              $
       nameformat         : '',              $
       helpFile           : Obj_New(),       $
       resource           : Ptr_New(),       $
       resetFileNumber    : 0b,              $
       checkAllFilesExistence: 0b,           $
       checkScanFiles     : 0b,              $
       excelObj           : Obj_New(),       $
       qCalibGUI          : Obj_New(),       $
       qCalibGUI2         : Obj_New(),       $
       fileSelectList     : List(),          $
       version            : 0.0,             $
       programDir         : '',              $
       updateProgressLabel: 0L,              $
       settingsObj        : Obj_New(),       $
       currentlySelected  : List(),          $
       operatingSystem    : '',              $
       datfileobject      : Obj_New()        $ 
       }                                     

END
